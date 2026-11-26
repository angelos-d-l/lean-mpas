/* MPAS's side of the streams-file checks (tests/streams/run.py):  xml_oracle FILE MESH_STREAM
   MPAS 8.4.2's own xml_stream_parser.c with the stand-ins of oracle_stubs.c: the mesh stream's attributes, then the
   whole parse, with the log lines MPAS writes around them. */
#include <stdio.h>
#include <stdlib.h>
void mpas_log_write_c(const char *, const char *);
void xml_stream_get_attributes(char *, char *, int *, char *, char *, char *, char *, int *);
void xml_stream_parser(char *, void *, int *, int *);
int main(int argc, char **argv)
{
    int status, comm = 0;
    char filename[4096], reference[4096], interval[4096], io[4096], msg[4096];
    if (argc != 3) return 2;
    snprintf(msg, sizeof(msg), "Reading streams configuration from file %s", argv[1]);
    mpas_log_write_c(msg, "MPAS_LOG_OUT");
    xml_stream_get_attributes(argv[1], argv[2], &comm, filename, reference, interval, io, &status);
    printf("@ mesh status=%d", status);
    if (!status) printf(" template=%s reference_time=%s filename_interval=%s io_type=%s", filename, reference, interval, io);
    puts("");
    if (status) {
        snprintf(msg, sizeof(msg), "stream xml get attribute failed: %s", argv[1]);
        mpas_log_write_c(msg, "MPAS_LOG_CRIT");
        return 1;
    }
    snprintf(msg, sizeof(msg), " ** Attempting to bootstrap MPAS framework using stream: %s", argv[2]);
    mpas_log_write_c(msg, "MPAS_LOG_OUT");
    xml_stream_parser(argv[1], NULL, &comm, &status);
    printf("@ parser status=%d\n", status);
    if (status) {
        snprintf(msg, sizeof(msg), "xml stream parser failed: %s", argv[1]);
        mpas_log_write_c(msg, "MPAS_LOG_CRIT");
    } else mpas_log_write_c(" ** Validating streams", "MPAS_LOG_OUT");
    return status;
}
