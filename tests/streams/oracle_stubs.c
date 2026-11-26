/* Stand-ins only for framework calls; xml_stream_parser.c and ezxml.c are pristine. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void mpas_log_write_c(const char *message, const char *level)
{
    const char *prefix = "";
    size_t n = strlen(message);
    if (!strcmp(level, "MPAS_LOG_WARN")) prefix = "WARNING:";
    if (!strcmp(level, "MPAS_LOG_ERR")) prefix = "ERROR:";
    if (!strcmp(level, "MPAS_LOG_CRIT")) prefix = "CRITICAL ERROR:";
    while (n && message[n-1] == ' ') n--;
    printf("%s %.*s\n", prefix, (int)n, message);
}

static const char *registry[] = {"invariant", "input", "restart", "iau", "lbc_in",
    "ugwp_oro_data_in", "ugwp_ngw_in", "da_state"};
static const int registry_direction[] = {1, 1, 3, 1, 1, 1, 1, 3};
static const char *packages[] = {"mp_kessler_in", "mp_thompson_in", "mp_thompson_aers_in", "mp_wsm6_in",
    "cu_grell_freitas_in", "cu_kain_fritsch_in", "cu_ntiedtke_in", "bl_ysu_in", "bl_mynn_in", "sf_noahmp_in",
    "iau", "limited_area", "jedi_da", "no_invariant_stream", "ugwp_orog_stream", "ugwp_ngw_stream",
    "ugwp_diags_stream", "les", "sfclayer"};
static char *created[256];
static int directions[256], n_created;
/* The clock's alarm ids, and whether MPAS_stream_mgr_add_alarm put the alarm on its stream. */
static char *alarms[1024];
static int on_stream[1024], n_alarms;
/* mpas_c_to_f_string then trim(): the stream manager never sees trailing blanks. */
static size_t trimmed(const char *name)
{
    size_t n = strlen(name);
    while (n && name[n-1] == ' ') n--;
    return n;
}
static int immutable_index(const char *name)
{
    size_t n = trimmed(name);
    for (int i = 0; i < 8; i++) if (n == strlen(registry[i]) && !strncmp(name, registry[i], n)) return i;
    return -1;
}
static int created_index(const char *name)
{
    size_t n = trimmed(name);
    for (int i = 0; i < n_created; i++) if (n == strlen(created[i]) && !strncmp(name, created[i], n)) return i;
    return -1;
}

void stream_mgr_create_stream_c(void *m, const char *s, int *d, const char *t,
    const char *fi, const char *rt, const char *ri, int *im, int *p, int *cl, int *io, int *err)
{
    char msg[4096];
    printf("@ create %s direction=%d template=%s filename_interval=%s reference_time=%s record_interval=%s immutable=%d precision=%d clobber=%d io_type=%d\n",
           s, *d, t, fi, rt, ri, *im, *p, *cl, *io);
    *err = 0;
    if (*im && immutable_index(s) < 0) {
        snprintf(msg, sizeof(msg), "No stream matching %s exists in call to MPAS_stream_mgr_set_property().", s);
        mpas_log_write_c(msg, "MPAS_LOG_ERR");
        mpas_log_write_c("********************************************************************************", "MPAS_LOG_ERR");
        snprintf(msg, sizeof(msg), " Error: Stream '%s' was not defined in the Registry.xml file as ", s);
        mpas_log_write_c(msg, "MPAS_LOG_ERR");
        mpas_log_write_c("        an immutable stream. Immutable streams may only be defined in the Registry.xml", "MPAS_LOG_ERR");
        mpas_log_write_c("        file for a core.", "MPAS_LOG_ERR");
        mpas_log_write_c("********************************************************************************", "MPAS_LOG_ERR");
        *err = 1;
    } else if (!*im) {
        /* MPAS_stream_mgr_create_stream fails without a line when a stream of that (trimmed) name exists
           (mpas_stream_manager.F:353-357). stream_mgr_create_stream_c then overwrites ierr with the result of
           each later set_property call, which finds that stream and succeeds (:6038-6052); ierr_c is the last
           one (:6054-6058). So the create reports an error only when none of those calls is made. */
        if (immutable_index(s) >= 0 || created_index(s) >= 0) {
            *err = 1;
            if (strcmp(rt, "initial_time") || strcmp(ri, "none") || trimmed(fi) != 4 || strncmp(fi, "none", 4) || *d == 4)
                *err = 0;
            return;
        }
        if (n_created == 256) exit(2);
        created[n_created] = strndup(s, trimmed(s));
        directions[n_created++] = *d;
    }
}
void stream_mgr_add_field_c(void *m, const char *s, const char *f, const char *p, int *err)
{ printf("@ field %s %s %s\n", s, f, p); *err = 0; }
void stream_mgr_add_pool_c(void *m, const char *s, const char *f, const char *p, int *err)
{ printf("@ pool %s %s %s\n", s, f, p); *err = 0; }
void stream_mgr_add_immutable_stream_fields_c(void *m, const char *s, const char *f, const char *p, int *err)
{
    char msg[4096];
    printf("@ substream %s %s %s\n", s, f, p);
    *err = 0;
    if (immutable_index(f) >= 0) return;
    /* mpas_stream_manager.F:6170-6181: a mutable stream that exists adds nothing. One that does not exist (yet)
       logs the error (:2191) and leaves is_immutable unset: false in MPAS 8.4.2's run of such a streams file
       (docs/streams.md), so the C parser goes on to its own search of the file. */
    if (created_index(f) >= 0) return;
    snprintf(msg, sizeof(msg), "Stream %.*s does not exist in call to MPAS_stream_mgr_get_property().", (int)trimmed(f), f);
    mpas_log_write_c(msg, "MPAS_LOG_ERR");
    *err = 1;
}
/* stream_mgr_add_alarm_c (mpas_stream_manager.F:6198-6292): the clock alarm first (mpas_add_clock_alarm,
   mpas_timekeeping.F:523/530), then MPAS_stream_mgr_add_alarm (:1158-1230). */
void stream_mgr_add_alarm_c(void *m, const char *s, const char *d, const char *start, const char *interval, int *err)
{
    char msg[4096], id[4096];
    int index = immutable_index(s), direction = index >= 0 ? registry_direction[index] : 0, a;
    printf("@ alarm %s %s %s %s\n", s, d, start, interval);
    *err = 0;
    if (!strcmp(interval, "none")) return;
    snprintf(id, sizeof(id), "%.*s_%s", (int)trimmed(s), s, d);
    for (a = 0; a < n_alarms; a++) if (!strcmp(id, alarms[a])) break;
    if (a < n_alarms) {
        snprintf(msg, sizeof(msg), "OOPS -- we have a duplicate alarmID%s", id);
        mpas_log_write_c(msg, "MPAS_LOG_ERR");
        *err = 1;
    } else {
        if (n_alarms == 1024) exit(2);
        alarms[n_alarms] = strdup(id);
        on_stream[n_alarms++] = 0;
    }
    if (created_index(s) >= 0) direction = directions[created_index(s)];
    if ((direction == 1 && !strcmp(d, "output")) || (direction == 2 && !strcmp(d, "input")) || direction == 4) {
        snprintf(msg, sizeof(msg), "Attempting to add an alarm %s to invalid direction for stream %.*s", id, (int)trimmed(s), s);
        mpas_log_write_c(msg, "MPAS_LOG_ERR");
        *err = 1;
        return;
    }
    if (on_stream[a]) {
        snprintf(msg, sizeof(msg), "Requested %s alarm %s already on stream %.*s", d, id, (int)trimmed(s), s);
        mpas_log_write_c(msg, "MPAS_LOG_ERR");
        *err = 1;
        return;
    }
    on_stream[a] = 1;
}
void stream_mgr_add_pkg_c(void *m, const char *s, const char *p, int *err)
{
    char msg[4096], package[4096];
    size_t n = strlen(p);
    while (n && p[n-1] == ' ') n--;
    snprintf(package, sizeof(package), "%.*s", (int)n, p);
    /* mpas_stream_manager.F:6320: trim(package)//'Active' before lookup. */
    printf("@ package %s %s\n", s, p);
    *err = 0;
    for (int i = 0; i < 19; i++) if (!strcmp(package, packages[i])) return;
    snprintf(msg, sizeof(msg), "Package %sActive not found in call to MPAS_stream_mgr_add_pkg().", package);
    mpas_log_write_c(msg, "MPAS_LOG_ERR");
    *err = 1;
}
