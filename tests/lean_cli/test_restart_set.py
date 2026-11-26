"""The generated restart inventory (tools/gen_restart_set.py) against the state types and the MPAS name map."""
from collections import Counter
import json
from pathlib import Path
import re
import subprocess
import sys
import unittest

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / 'tools'))
from gen_phys_state import member_names


# The dynamics fields (f) and the Thompson fields (mp) a restart carries besides the physics state types.
DYN_MICRO = (
    'f%u1 f%u2 f%w1 f%w2 f%theta_m1 f%theta_m2 f%rho_zz1 f%rho_zz2 f%scalars1 f%scalars2 f%ru f%rw '
    'f%rho_p f%rtheta_p f%exner f%pressure_p f%ru_save f%rw_save f%rho_p_save f%rtheta_p_save '
    'f%rho_zz_old_split f%rho_edge f%v f%vorticity f%divergence f%ke f%pv_vertex f%pv_edge f%ke_edge '
    'f%ke_vertex f%ru_p f%rw_p f%rho_pp f%rtheta_pp f%rtheta_pp_old f%ru_avg f%ww_avg f%ru_avg_split '
    'f%ww_avg_split f%tend_u f%tend_w f%tend_theta f%tend_rho f%tend_u_euler f%tend_w_euler '
    'f%tend_theta_euler f%rthdynten f%eddy_visc_horz f%h_divergence f%dpdz f%delsq_u f%delsq_vorticity '
    'f%delsq_divergence f%delsq_w f%delsq_theta f%tend_w_pert f%rt_diabatic_tend f%cqu f%cqw f%qtot '
    'f%cofwr f%cofwz f%coftz f%cofwt f%a_tri f%alpha_tri f%gamma_tri f%cofrz f%tend_ru_physics '
    'f%tend_rtheta_physics f%tend_rho_physics f%tend_scalars f%rqvdynten f%horiz_flux f%rho_zz_int '
    'f%sc_old f%sc_new f%s_max f%s_min f%wdtn f%scale_arr f%flux f%flux_upwind f%flux_tmp f%u_zonal '
    'f%u_meridional mp%nt_c mp%surface_pressure mp%tend_sfc_pressure mp%rainnc mp%rainncv mp%snownc '
    'mp%snowncv mp%graupelnc mp%graupelncv mp%sr mp%dtheta_dt_mp mp%rainprod mp%evapprod mp%re_cloud '
    'mp%re_ice mp%re_snow '
).split()

class RestartSet(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        try:
            import gen_restart_set
        except ModuleNotFoundError as error:
            raise AssertionError('no restart generator') from error
        cls.gen = gen_restart_set
        cls.config = json.loads((ROOT / 'config/restart.json').read_text())
        cls.rows = cls.gen.inventory(cls.config)
        cls.by_name = {r['name']: r for r in cls.rows}
        cls.objects = {'state': 'state', 'lands': 'land', 'convection': 'gf', 'drag': 'gwdo'}

    def test_every_member_once(self):
        expected = {f'{obj}%{name}' for obj, short in self.objects.items()
                    for name in member_names(short)}
        expected.update(DYN_MICRO)
        actual = {r['member'] for r in self.rows if r.get('primary') and not (r.get('guard') or r.get('condition'))}
        # A user stream can write precipitable water at resume before Thompson updates it.
        expected.add('mp%precipw')
        self.assertEqual(len(expected), 697)
        self.assertEqual(actual, expected)
        self.assertEqual(len(self.rows), len(self.by_name))
        counts = Counter(r['member'] for r in self.rows if r.get('primary'))
        self.assertEqual(set(counts.values()), {1})
        walk = (ROOT / 'src/io/lean_restart_set.f90').read_text()
        self.assertRegex(walk, r'(?s)#ifdef LEAN_NOAHMP_SWSPLIT\s+call restart_var\(rf, .*?sw_frac_dir_vis.*?#endif')
        self.assertRegex(walk, r'(?s)#ifdef LEAN_BUDGET\s+call restart_var\(rf, .*?lean_rad_flux.*?#endif')
        table_calls = re.findall(r"call restart_(?:var|static)\(rf, '([^']+)'", walk)
        self.assertEqual(table_calls, [r['name'] for r in self.rows])

    def test_mpas_names(self):
        catalog = self.gen.catalog_names()
        self.assertEqual(len(catalog), 874)
        mpas = {r['name'] for r in self.rows if r['mpas_name']}
        self.assertLessEqual(mpas, catalog)
        lean = {r['member'] for r in self.rows if r['name'].startswith('lean_')}
        for member in 'maxwidth maxmf ztop_plume ktop_plume qstar exch_m'.split():
            self.assertIn('state%' + member, lean)
        self.assertIn('convection%shortwave', lean)

    def test_budget_flux_pack_is_allocatable(self):
        walk = (ROOT / 'src/io/lean_restart_set.f90').read_text()
        self.assertIsNotNone(re.search(r'real, allocatable :: flux\(:,\s*:\)', walk),
                             'budget flux pack is automatic')

    def test_outside_objects(self):
        outside = {r['name'] for r in self.rows if not r.get('primary')
                   and not r['name'].startswith('lean_ghost_') and r['member'] != 'f%scalars2'}
        self.assertEqual(outside, set((
            'xtime initial_time Time lean_cold_start lean_alarm_radtlw lean_alarm_radtsw lean_alarm_green lean_alarm_diag lean_alarm_sfcbdy xicem ivgtyp isltyp lean_smois_first_step '
            'gsw glw olrtoa lean_rad_cos_zenith lean_rad_sw_frac_dir_vis lean_rad_sw_frac_dir_nir lean_rad_sw_frac_dif_vis '
            'lean_rad_flux lean_rad_heating_lw lean_rad_heating_sw sfc_emiss snow sfc_albedo xice '
            'sst vegfra rho_base exner_base rtheta_base pressure_base rainc acsnow updraft_helicity_max w_velocity_max '
            'wind_speed_level1_max relhum refl10cm refl10cm_max refl10cm_1km refl10cm_1km_max '
            'lean_glacier_of lean_glacier_real lean_glacier_int lean_glacier_exchange lean_seaice_any '
            + ' '.join('lean_seaice_' + n for n in
                       'xice sst tslb snow snowc snoalb z0 albedo emiss grdflx acsnow acsnom sfcrunoff'.split())
        ).split()))

    def test_dims(self):
        dims = {
            'nCells', 'nEdges', 'nVertices', 'nVertLevels', 'nVertLevelsP1', 'nSoilLevels', 'maxEdges',
            'lean_nCellsP1', 'lean_nEdgesP1', 'lean_nVerticesP1', 'lean_nScalars',
            'lean_nGlacierReal', 'lean_nGlacierInt', 'lean_nGlacierCells', 'lean_nExchangeReal', 'StrLen'
        }
        for row in self.rows:
            for dim in row['dims']:
                self.assertTrue(dim in dims or re.fullmatch(r'lean_n[1-9][0-9]*', dim), (row['name'], dim))
            if row['mpas_name'] and row.get('ghost'):
                ghost = self.by_name['lean_ghost_' + row['name']]
                self.assertEqual(ghost['dims'], [d for j, d in enumerate(row['dims']) if j != row['ghost'] - 1])
        self.assertEqual(self.by_name['w']['dims'], ['nVertLevelsP1', 'nCells'])
        self.assertEqual(self.by_name['lean_dyn_scale_arr']['dims'], ['nVertLevels', 'lean_n2', 'lean_nCellsP1'])
        self.assertEqual(self.by_name['lean_seaice_tslb']['dims'], ['nSoilLevels', 'nCells'])

    def test_layout_hash(self):
        original = self.gen.layout_hash(self.rows)
        self.assertRegex(original, r'^[0-9a-f]{16}$')
        self.assertEqual(original, self.gen.layout_hash(self.gen.inventory(self.config)))
        changed = [dict(row) for row in self.rows]
        changed[0]['name'] += '_changed'
        self.assertNotEqual(original, self.gen.layout_hash(changed))
        self.assertIn(original, (ROOT / 'src/io/lean_restart_layout.f90').read_text())

    def test_glacier_members(self):
        members = self.gen.glacier_members()
        self.assertEqual(Counter(m['kind'] for m in members), {'real': 517, 'integer': 12})
        self.assertTrue(all(m['rank'] > 0 for m in members))

    def test_check(self):
        result = subprocess.run([sys.executable, 'tools/gen_restart_set.py', '--check'],
                                cwd=ROOT, capture_output=True, text=True)
        self.assertEqual((result.returncode, result.stdout, result.stderr), (0, '', ''))
