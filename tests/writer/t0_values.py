"""lean's history and diag files against MPAS's, bit for bit, except the measured differences listed here;
lean's own fields are reported separately."""
import argparse
import sys
from pathlib import Path
import numpy as np
from netCDF4 import Dataset
sys.path.insert(0,str(Path(__file__).resolve().parents[2]))
from tools.field_classes import load
CLASSES=load(Path(__file__).resolve().parents[2]/'src/lean_fields.f90')
# The MPAS reference files were written by MPAS built with nvfortran 25.3 and strict flags. With nvfortran 25.3 every
# compared value is bit-identical. With gfortran 15.2 (arm64 macOS) a few fields differ in the last bits; MPAS's own
# expressions compiled with that gfortran give lean's bits (docs/acceptance.md, 'What gfortran gives differently at the
# start'). The caller supplies the compiler of the lean build. Each entry: (largest difference, number of differing
# values).
GFORTRAN_BITS={'greece_60km': {'pressure': (0.015625, 835),
                 'uReconstructMeridional': (1.9073486328125e-06, 19),
                 'mslp': (0.0234375, 28),
                 'relhum_50hPa': (9.5367431640625e-07, 39),
                 'relhum_100hPa': (2.384185791015625e-06, 22),
                 'relhum_200hPa': (2.288818359375e-05, 23),
                 'relhum_250hPa': (1.52587890625e-05, 16),
                 'relhum_500hPa': (6.103515625e-05, 25),
                 'relhum_700hPa': (4.9591064453125e-05, 34),
                 'relhum_850hPa': (9.1552734375e-05, 32),
                 'relhum_925hPa': (6.29425048828125e-05, 26),
                 'dewpoint_50hPa': (3.0517578125e-05, 27),
                 'dewpoint_100hPa': (1.52587890625e-05, 11),
                 'dewpoint_200hPa': (1.52587890625e-05, 13),
                 'dewpoint_250hPa': (1.52587890625e-05, 9),
                 'dewpoint_500hPa': (3.0517578125e-05, 15),
                 'dewpoint_700hPa': (3.0517578125e-05, 19),
                 'dewpoint_850hPa': (3.0517578125e-05, 19),
                 'dewpoint_925hPa': (3.0517578125e-05, 17),
                 'temperature_50hPa': (3.0517578125e-05, 32),
                 'temperature_100hPa': (3.0517578125e-05, 15),
                 'temperature_200hPa': (3.0517578125e-05, 19),
                 'temperature_250hPa': (1.52587890625e-05, 16),
                 'temperature_500hPa': (6.103515625e-05, 17),
                 'temperature_700hPa': (3.0517578125e-05, 19),
                 'temperature_850hPa': (3.0517578125e-05, 13),
                 'temperature_925hPa': (6.103515625e-05, 16),
                 'height_50hPa': (0.00390625, 18),
                 'height_100hPa': (0.00390625, 18),
                 'height_200hPa': (0.00390625, 23),
                 'height_250hPa': (0.001953125, 22),
                 'height_500hPa': (0.00341796875, 30),
                 'height_700hPa': (0.006103515625, 23),
                 'height_850hPa': (0.0040283203125, 42),
                 'height_925hPa': (0.00445556640625, 37),
                 'uzonal_50hPa': (4.76837158203125e-06, 27),
                 'uzonal_100hPa': (3.814697265625e-06, 11),
                 'uzonal_200hPa': (4.76837158203125e-06, 19),
                 'uzonal_250hPa': (5.7220458984375e-06, 14),
                 'uzonal_500hPa': (4.291534423828125e-06, 23),
                 'uzonal_700hPa': (8.58306884765625e-06, 35),
                 'uzonal_850hPa': (6.9141387939453125e-06, 39),
                 'uzonal_925hPa': (4.0531158447265625e-06, 34),
                 'umeridional_50hPa': (2.5033950805664062e-06, 41),
                 'umeridional_100hPa': (1.430511474609375e-06, 21),
                 'umeridional_200hPa': (7.62939453125e-06, 24),
                 'umeridional_250hPa': (1.9073486328125e-06, 14),
                 'umeridional_500hPa': (3.814697265625e-06, 28),
                 'umeridional_700hPa': (3.814697265625e-06, 37),
                 'umeridional_850hPa': (7.033348083496094e-06, 43),
                 'umeridional_925hPa': (4.76837158203125e-06, 33),
                 'w_50hPa': (5.638867150992155e-11, 47),
                 'w_100hPa': (8.731149137020111e-10, 40),
                 'w_200hPa': (1.0710209608078003e-08, 33),
                 'w_250hPa': (1.6763806343078613e-08, 31),
                 'w_500hPa': (2.8870999813079834e-08, 28),
                 'w_700hPa': (1.4435499906539917e-08, 27),
                 'w_850hPa': (4.237517714500427e-08, 43),
                 'w_925hPa': (2.6542693376541138e-08, 33),
                 'vorticity_50hPa': (4.001776687800884e-11, 72),
                 'vorticity_100hPa': (1.1641532182693481e-10, 45),
                 'vorticity_200hPa': (2.9103830456733704e-11, 47),
                 'vorticity_250hPa': (2.0372681319713593e-10, 42),
                 'vorticity_500hPa': (7.275957614183426e-11, 49),
                 'vorticity_700hPa': (2.9103830456733704e-10, 63),
                 'vorticity_850hPa': (1.8917489796876907e-10, 88),
                 'vorticity_925hPa': (1.4551915228366852e-10, 65),
                 't_isobaric': (4.57763671875e-05, 83),
                 'z_isobaric': (0.006103515625, 365),
                 'meanT_500_300': (3.0517578125e-05, 9),
                 'cape': (0.05312347412109375, 212),
                 'cin': (0.0288543701171875, 157),
                 'srh_0_1km': (1.9073486328125e-05, 512),
                 'srh_0_3km': (4.57763671875e-05, 1285),
                 'umeridional_1km': (9.5367431640625e-07, 1),
                 'temperature_surface': (6.103515625e-05, 6)},
 'x1_2562/reference/strict_l55': {'pressure': (0.015625, 1203),
                                  'sh2o': (1.4901161193847656e-08, 8),
                                  'mslp': (0.0234375, 24),
                                  'relhum_50hPa': (2.384185791015625e-06, 43),
                                  'relhum_100hPa': (1.52587890625e-05, 8),
                                  'relhum_200hPa': (1.52587890625e-05, 26),
                                  'relhum_250hPa': (4.00543212890625e-05, 30),
                                  'relhum_500hPa': (1.52587890625e-05, 30),
                                  'relhum_700hPa': (2.6702880859375e-05, 38),
                                  'relhum_850hPa': (6.4849853515625e-05, 23),
                                  'relhum_925hPa': (0.000110626220703125, 24),
                                  'dewpoint_50hPa': (3.0517578125e-05, 22),
                                  'dewpoint_100hPa': (1.52587890625e-05, 10),
                                  'dewpoint_200hPa': (3.0517578125e-05, 26),
                                  'dewpoint_250hPa': (3.0517578125e-05, 18),
                                  'dewpoint_500hPa': (3.0517578125e-05, 17),
                                  'dewpoint_700hPa': (4.57763671875e-05, 14),
                                  'dewpoint_850hPa': (3.0517578125e-05, 8),
                                  'dewpoint_925hPa': (3.0517578125e-05, 16),
                                  'temperature_50hPa': (3.0517578125e-05, 25),
                                  'temperature_100hPa': (3.0517578125e-05, 13),
                                  'temperature_200hPa': (4.57763671875e-05, 23),
                                  'temperature_250hPa': (3.0517578125e-05, 24),
                                  'temperature_500hPa': (3.0517578125e-05, 18),
                                  'temperature_700hPa': (6.103515625e-05, 23),
                                  'temperature_850hPa': (3.0517578125e-05, 21),
                                  'temperature_925hPa': (3.0517578125e-05, 25),
                                  'height_50hPa': (0.00390625, 9),
                                  'height_100hPa': (0.001953125, 13),
                                  'height_200hPa': (0.0029296875, 21),
                                  'height_250hPa': (0.0029296875, 24),
                                  'height_500hPa': (0.00341796875, 32),
                                  'height_700hPa': (0.00390625, 38),
                                  'height_850hPa': (0.0040283203125, 46),
                                  'height_925hPa': (0.00433349609375, 45),
                                  'uzonal_50hPa': (3.814697265625e-06, 38),
                                  'uzonal_100hPa': (3.814697265625e-06, 15),
                                  'uzonal_200hPa': (3.814697265625e-06, 16),
                                  'uzonal_250hPa': (3.814697265625e-06, 23),
                                  'uzonal_500hPa': (3.814697265625e-06, 28),
                                  'uzonal_700hPa': (5.7220458984375e-06, 34),
                                  'uzonal_850hPa': (7.62939453125e-06, 35),
                                  'uzonal_925hPa': (5.245208740234375e-06, 31),
                                  'umeridional_50hPa': (1.1175870895385742e-06, 37),
                                  'umeridional_100hPa': (2.384185791015625e-06, 17),
                                  'umeridional_200hPa': (3.814697265625e-06, 25),
                                  'umeridional_250hPa': (3.814697265625e-06, 30),
                                  'umeridional_500hPa': (1.7285346984863281e-06, 32),
                                  'umeridional_700hPa': (5.7220458984375e-06, 34),
                                  'umeridional_850hPa': (4.470348358154297e-06, 32),
                                  'umeridional_925hPa': (8.106231689453125e-06, 39),
                                  'w_50hPa': (5.820766091346741e-11, 34),
                                  'w_100hPa': (2.0372681319713593e-10, 39),
                                  'w_200hPa': (9.313225746154785e-10, 39),
                                  'w_250hPa': (2.3283064365386963e-09, 31),
                                  'w_500hPa': (2.561137080192566e-09, 44),
                                  'w_700hPa': (1.718217390589416e-08, 38),
                                  'w_850hPa': (8.17817635834217e-09, 46),
                                  'w_925hPa': (5.122274160385132e-09, 40),
                                  'vorticity_50hPa': (1.2732925824820995e-11, 70),
                                  'vorticity_100hPa': (1.2732925824820995e-11, 34),
                                  'vorticity_200hPa': (3.637978807091713e-11, 53),
                                  'vorticity_250hPa': (2.9103830456733704e-11, 53),
                                  'vorticity_500hPa': (4.3655745685100555e-11, 49),
                                  'vorticity_700hPa': (6.548361852765083e-11, 71),
                                  'vorticity_850hPa': (6.912159733474255e-11, 74),
                                  'vorticity_925hPa': (7.275957614183426e-11, 53),
                                  't_isobaric': (4.57763671875e-05, 123),
                                  'z_isobaric': (0.00439453125, 399),
                                  'meanT_500_300': (6.103515625e-05, 48),
                                  'updraft_helicity_max': (1.1641532182693481e-10, 283),
                                  'cape': (0.3353271484375, 836),
                                  'cin': (0.0382080078125, 406),
                                  'srh_0_1km': (6.103515625e-05, 1296),
                                  'srh_0_3km': (9.1552734375e-05, 1554),
                                  'temperature_surface': (6.103515625e-05, 10)},
 'x1_2562': {'pressure': (0.015625, 985),
             'sh2o': (1.4901161193847656e-08, 8),
             'mslp': (0.0234375, 28),
             'relhum_50hPa': (1.9073486328125e-06, 38),
             'relhum_100hPa': (1.52587890625e-05, 19),
             'relhum_200hPa': (1.52587890625e-05, 17),
             'relhum_250hPa': (1.9073486328125e-05, 25),
             'relhum_500hPa': (1.1444091796875e-05, 20),
             'relhum_700hPa': (2.6702880859375e-05, 16),
             'relhum_850hPa': (3.814697265625e-05, 35),
             'relhum_925hPa': (3.814697265625e-05, 23),
             'dewpoint_50hPa': (1.52587890625e-05, 30),
             'dewpoint_100hPa': (1.52587890625e-05, 12),
             'dewpoint_200hPa': (1.52587890625e-05, 7),
             'dewpoint_250hPa': (1.52587890625e-05, 19),
             'dewpoint_500hPa': (3.0517578125e-05, 16),
             'dewpoint_700hPa': (3.0517578125e-05, 7),
             'dewpoint_850hPa': (3.0517578125e-05, 19),
             'dewpoint_925hPa': (3.0517578125e-05, 12),
             'temperature_50hPa': (3.0517578125e-05, 35),
             'temperature_100hPa': (4.57763671875e-05, 22),
             'temperature_200hPa': (4.57763671875e-05, 13),
             'temperature_250hPa': (4.57763671875e-05, 27),
             'temperature_500hPa': (3.0517578125e-05, 15),
             'temperature_700hPa': (6.103515625e-05, 13),
             'temperature_850hPa': (6.103515625e-05, 22),
             'temperature_925hPa': (3.0517578125e-05, 18),
             'height_50hPa': (0.00390625, 15),
             'height_100hPa': (0.0029296875, 16),
             'height_200hPa': (0.00390625, 15),
             'height_250hPa': (0.0029296875, 25),
             'height_500hPa': (0.00390625, 44),
             'height_700hPa': (0.004150390625, 28),
             'height_850hPa': (0.002685546875, 26),
             'height_925hPa': (0.00421142578125, 41),
             'uzonal_50hPa': (4.76837158203125e-06, 33),
             'uzonal_100hPa': (7.62939453125e-06, 22),
             'uzonal_200hPa': (3.814697265625e-06, 17),
             'uzonal_250hPa': (3.814697265625e-06, 15),
             'uzonal_500hPa': (7.62939453125e-06, 28),
             'uzonal_700hPa': (3.635883331298828e-06, 21),
             'uzonal_850hPa': (3.337860107421875e-06, 29),
             'uzonal_925hPa': (9.5367431640625e-06, 27),
             'umeridional_50hPa': (2.86102294921875e-06, 44),
             'umeridional_100hPa': (3.337860107421875e-06, 25),
             'umeridional_200hPa': (3.814697265625e-06, 15),
             'umeridional_250hPa': (2.86102294921875e-06, 23),
             'umeridional_500hPa': (2.86102294921875e-06, 22),
             'umeridional_700hPa': (2.6226043701171875e-06, 23),
             'umeridional_850hPa': (3.814697265625e-06, 33),
             'umeridional_925hPa': (1.1444091796875e-05, 28),
             'w_50hPa': (2.1827872842550278e-11, 44),
             'w_100hPa': (2.1827872842550278e-10, 32),
             'w_200hPa': (9.313225746154785e-10, 34),
             'w_250hPa': (2.561137080192566e-09, 43),
             'w_500hPa': (3.725290298461914e-09, 52),
             'w_700hPa': (1.3969838619232178e-09, 29),
             'w_850hPa': (4.6566128730773926e-09, 26),
             'w_925hPa': (1.0826624929904938e-08, 35),
             'vorticity_50hPa': (2.7284841053187847e-11, 78),
             'vorticity_100hPa': (1.4551915228366852e-11, 52),
             'vorticity_200hPa': (2.205524651799351e-11, 35),
             'vorticity_250hPa': (2.3646862246096134e-11, 42),
             'vorticity_500hPa': (4.9112713895738125e-11, 57),
             'vorticity_700hPa': (2.319211489520967e-11, 52),
             'vorticity_850hPa': (6.548361852765083e-11, 68),
             'vorticity_925hPa': (5.820766091346741e-11, 48),
             't_isobaric': (6.103515625e-05, 106),
             'z_isobaric': (0.00390625, 355),
             'meanT_500_300': (3.0517578125e-05, 16),
             'cape': (0.33770751953125, 792),
             'cin': (0.03155517578125, 351),
             'srh_0_1km': (6.103515625e-05, 690),
             'srh_0_3km': (9.1552734375e-05, 1358),
             'temperature_surface': (6.103515625e-05, 14)}}
# Any compiler, measured with nvfortran 25.3. The MPAS files carry nvfortran's vectorized sums over levels
# (integral_zstaggered for updraft helicity, compute_layer_mean for meanT_500_300); lean adds the levels in source order
# there. MPAS's routines compiled as MPAS was equal MPAS's files on every cell; compiled with -Mnovect they give these
# counts (docs/fixes.md). An entry is added here only once the cause of the difference is located.
# The 55-level key stands first: the first matching key is used.
ANY_BITS={
    # Largest difference and count over every file of the references, t=0 and later (strict_diag1h, strict_l55_diag1h,
    # strict_l55_3h, strict_24h, athens strict_diag1h). At t=0: x1 1 and 1, x1 55 levels 283 and 20, athens 2. The
    # maxima are running maxima, so later files carry other cells.
    'x1_2562/reference/strict_l55': {'updraft_helicity_max':(1.862645149230957e-09,346), 'meanT_500_300':(6.103515625e-05,27)},
    # meanT_500_300 on x1 includes the option runs (opt_apvm05_3h 4 cells, off_sfc_3h 4.57763671875e-05); same cause,
    # every other field of those files equals MPAS.
    'x1_2562': {'updraft_helicity_max':(4.656612873077393e-10,5), 'meanT_500_300':(4.57763671875e-05,4)},
    'athens_3km': {'updraft_helicity_max':(3.7252902984619141e-09,2)},
}
def compare(lean,mpas,fc):
    errors=[]; differences={}
    allowed=dict(next((values for mesh,values in ANY_BITS.items() if mesh in str(mpas)),{}))
    allowed.update(next((values for mesh,values in (GFORTRAN_BITS if fc=='gfortran' else {}).items() if mesh in str(mpas)),{}))
    with Dataset(lean) as a, Dataset(mpas) as b:
        a.set_auto_mask(False); b.set_auto_mask(False)
        for n,v in a.variables.items():
            cls=CLASSES.get(n)
            x=np.asarray(v[:]);y=np.asarray(b[n][:])
            same=x.shape==y.shape and x.tobytes()==y.tobytes()
            if same:
                if cls in ('SOURCE_LEAN','SOURCE_DIAG'): print(f'{n}: same')
            elif x.shape==y.shape and x.dtype.kind=='f' and n in allowed:
                d=np.abs(x.astype('f8')-y.astype('f8'))
                delta=float(d.max());at=np.unravel_index(d.argmax(),d.shape)
                print(f'{n}: differs max {delta:.17g} at {at} ({int((d>0).sum())} values)')
                differences[n]=delta
                maximum,count=allowed[n]
                if not delta<=maximum: errors.append(f'{n}: difference {delta} above the measured {maximum}')
                changed=int((x.view('u'+str(x.dtype.itemsize))!=y.view('u'+str(y.dtype.itemsize))).sum())
                if changed>count: errors.append(f'{n}: difference count {changed} above the measured {count}')
            else:
                if x.shape==y.shape and x.dtype.kind=='f':
                    d=np.abs(x.astype('f8')-y.astype('f8'))
                    print(f'{n}: unallowed max {float(d.max()):.17g}, count {int((d>0).sum())}')
                errors.append(f'{n} ({cls}): differs')
    assert not errors, '\n'.join(errors)
    return differences
if __name__=='__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--fc',required=True,choices=['gfortran','nvfortran'])
    parser.add_argument('files',nargs='+')
    args=parser.parse_args()
    parser.error('files must be lean/MPAS pairs') if len(args.files)%2 else None
    found={}
    for i in range(0,len(args.files),2):found.update(compare(*args.files[i:i+2],args.fc))
    print('T0 VALUES OK')
