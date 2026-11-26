"""Serial real-model writer checks: one plain build (flavor mpas), every run in atmosphere_model's form in an MPAS run
directory. The structure, clobber, kill and stream cases need only the release case inputs; the value comparisons need
the MPAS reference outputs of data/cases/*/reference, which are not released, and SKIP without them. Scratch stays
only after a failure."""
from pathlib import Path
import os, subprocess, sys, shutil, json, re, shlex
import numpy as np
from netCDF4 import Dataset
ROOT=Path(__file__).resolve().parents[2];os.chdir(ROOT)
sys.path.insert(0,str(ROOT))
from tests.writer.t0_values import compare
from tests.restart.compare_runs import compare_variable
from tools.cases.run_spec import edit_namelist
FC=sys.argv[1] if len(sys.argv)>1 else 'gfortran'
JOBS=sys.argv[2] if len(sys.argv)>2 else '2'
assert 1<=int(JOBS)<=2
WORK=ROOT/'build/writer/runs';WORK.mkdir(parents=True,exist_ok=True)
NCDUMP=os.environ.get('NCDUMP') or shutil.which('ncdump') or 'ncdump'   # it must read CDF-5
NCGEN=os.environ.get('NCGEN') or shutil.which('ncgen') or 'ncgen'
PY=os.environ.get('LEAN_PY',sys.executable)
B=None
LOGS=('log.atmosphere.0000.out','log.atmosphere.0000.err')

def command(args,log=None,cwd=ROOT,env=None,expected=0):
    with open(log,'w') if log else open(os.devnull,'w') as out:
        p=subprocess.run(list(map(str,args)),cwd=cwd,env=env,stdout=out,stderr=subprocess.STDOUT)
    if expected is not None and p.returncode!=expected:
        if log: print(Path(log).read_text()[-12000:])
        raise AssertionError(f'{args}: exit {p.returncode}')
    return p.returncode

def build():
    # the one build of every case
    global B
    if B is not None:return
    log=WORK/'build.log'
    command(['./lean','build','--flavor','mpas','--target','cpu','--fc',FC,'--jobs',JOBS,'--json'],log)
    B=Path(json.loads(log.read_text()[log.read_text().index('{'):])['build_json']).resolve().parent

def directory(name,mesh='greece_60km',variant='',spec=None):
    rd=WORK/name
    if rd.exists():shutil.rmtree(rd)
    inp=ROOT/f'data/cases/{mesh}/inputs'/variant
    specs=[f'cases/{mesh}/case.txt',f'cases/{mesh}/mpas_runs/'+('strict_12h.txt' if mesh=='greece_60km' else 'strict_24h.txt')]
    if spec:
        extra=WORK/(name+'.spec');extra.write_text(spec);specs.append(extra)
    cmd='root='+shlex.quote(str(ROOT))+'; . tests/inputs/run_dir.sh; make_run_dir '+ ' '.join(map(shlex.quote,map(str,[rd,inp,*specs])))
    command(['sh','-c',cmd],WORK/(name+'.setup.log'))
    return rd

def set_steps(rd,steps):
    # config_run_duration of the run directory's namelist: steps of 120 s
    s=steps*120;path=rd/'namelist.atmosphere'
    path.write_text(edit_namelist(path.read_text(),'nhyd_model','config_run_duration',f"'{s//86400}_{s//3600%24:02d}:{s//60%60:02d}:{s%60:02d}'"))

def run(rd,steps,name=None,env=None,expected=0):
    # atmosphere_model's form, steps long; WORK/<name>.log holds log.atmosphere.0000.out (stdout and .err if it stopped before opening it)
    name=name or rd.name
    set_steps(rd,steps)
    launcher=rd/'atmosphere_model'
    if not launcher.is_symlink():launcher.symlink_to(B/'lean_mpas')
    for n in LOGS:(rd/n).unlink(missing_ok=True)
    e=os.environ.copy();e.update(env or {})
    code=command([launcher],WORK/(name+'.stdout'),rd,e,None)
    log=rd/LOGS[0]
    text=log.read_text() if log.is_file() else (WORK/(name+'.stdout')).read_text()+((rd/LOGS[1]).read_text() if (rd/LOGS[1]).is_file() else '')
    (WORK/(name+'.log')).write_text(text)
    if expected is not None and code!=expected:
        print(text[-12000:])
        raise AssertionError(f'{name}: exit {code}')
    return code

def headers(rd,mesh,ref):
    files=sorted(rd.glob('history.*.nc'))+sorted(rd.glob('diag.*.nc'))
    assert files, 'FAIL structure: no file written'
    for f in files:
        kind='history' if f.name.startswith('history') else 'diag'
        cdl=ROOT/f'cases/{mesh}/reference/{ref}.{kind}.cdl'
        log=WORK/(f.name+'.header.log')
        command(['python3','tests/writer/compare_cdl.py',f,cdl],log)
        print(log.read_text().strip(),flush=True)
    return files

def same(a,b):
    assert Path(a).read_bytes()==Path(b).read_bytes(),f'{a} differs from {b}'

def cleanup():
    shutil.rmtree(WORK)
    shutil.rmtree(ROOT/'build/writer/cache',ignore_errors=True)
    try: WORK.parent.rmdir()
    except OSError: pass

passed,skipped=[],[]
def done(name):
    print('ok   '+name,flush=True);passed.append(name)

REFERENCE=ROOT/'data/cases'
NOT_RELEASED='the MPAS reference outputs are not released'
def skip(name,why=NOT_RELEASED):
    print(f'SKIP {name}: {why}',flush=True);skipped.append(name)

def have(*paths):
    return all(Path(p).exists() for p in paths)

def sea_ice_history(six_hour=None):
    mesh='x1_2562'
    cases=[
        ('strict_diag1h','',30,('00.00.00','00.30.00','01.00.00')),
        ('strict_l55_diag1h','l55',30,('00.00.00','00.30.00','01.00.00')),
    ]
    # The structure run already uses strict_24h's streams for the first 6 h.
    if six_hour is not None:
        cases.append(('strict_24h','',0,('00.00.00','06.00.00')))
    # gfortran against MPAS built with nvfortran: maximum relative error, absolute error (mm), and changed cells.
    # Rain inputs differ in the two forecasts. Test exact zero wherever MPAS has zero.
    snow_bounds={
        ('strict_diag1h','01.00.00'):(0.0021568576628785189,1.8211210317531368e-10,1),
        ('strict_l55_diag1h','00.30.00'):(0.00011508972708086517,1.944044925039634e-11,1),
        ('strict_l55_diag1h','01.00.00'):(71.962373677158027,1.8382229018953922e-06,5),
        ('strict_24h','06.00.00'):(0.071881616804275475,0.0011668205261230469,123),
    }
    errors=[]
    for case,variant,steps,stamps in cases:
        refdir=REFERENCE/mesh/'reference'/case
        if not refdir.is_dir():
            skip('sea_ice_history '+case);continue
        if case=='strict_24h':
            rd=six_hour
        else:
            spec=(ROOT/f'cases/{mesh}/mpas_runs/{case}.txt').read_text()
            rd=directory('sea_ice_'+case,mesh,variant,spec=spec)
            run(rd,steps)
        with Dataset(rd/'x1.2562.init.nc') as init:
            fraction=np.asarray(init['xice'][:]).ravel()
        ice=fraction>0
        assert np.count_nonzero(ice)==146 and np.count_nonzero(fraction==1)==4, case
        for stamp in stamps:
            name=f'history.2026-11-08_{stamp}.nc'
            label=f'{case} {stamp}'
            with Dataset(rd/name) as actual,Dataset(refdir/name) as expected:
                for field in ('sh2o','smois'):
                    a=np.asarray(actual[field][0])[ice]
                    b=np.asarray(expected[field][0])[ice]
                    count=np.count_nonzero(a!=b)
                    absolute=np.max(np.abs(a-b))
                    print(f'{label} {field}: {count} differing values, max absolute {absolute:.17g}',flush=True)
                    if a.tobytes()!=b.tobytes():
                        # gfortran only: the fractional power of NoahmpInitMainMod.F90:96-102 gives up to 5 cold-start sh2o values
                        # another last bit (docs/acceptance.md, 'What gfortran gives differently at the start').
                        cold_sh2o=FC=='gfortran' and stamp=='00.00.00' and field=='sh2o'
                        if not (cold_sh2o and count<=5 and absolute<=3.725290298461914e-09):
                            errors.append(f'{label} {field}: {count} differing values, max absolute {absolute:.17g}')
                a=np.asarray(actual['acsnow'][0],dtype=np.float64)[ice]
                b=np.asarray(expected['acsnow'][0],dtype=np.float64)[ice]
            nonzero=b!=0
            delta=np.abs(a-b)
            count=np.count_nonzero(delta)
            absolute=np.max(delta)
            relative=np.max(delta[nonzero]/np.abs(b[nonzero]),initial=0.)
            zero_delta=np.max(delta[~nonzero],initial=0.)
            negative=np.count_nonzero(a<0)
            print(f'{label} acsnow: {count} differing cells, max relative {relative:.17g}, '
                  f'max absolute {absolute:.17g}, max absolute at reference zero {zero_delta:.17g}, '
                  f'{negative} negative',flush=True)
            if not np.isfinite(a).all() or negative:
                errors.append(f'{label} acsnow: must be finite and nonnegative ({negative} negative)')
            limit_rel,limit_abs,limit_count=snow_bounds.get((case,stamp),(0.,0.,0)) if FC=='gfortran' else (0.,0.,0)
            if count>limit_count or relative>limit_rel or absolute>limit_abs or zero_delta:
                errors.append(f'{label} acsnow: differs from MPAS in {count} cells')
    assert not errors,'FAIL sea_ice_history:\n'+'\n'.join(errors)
    done('sea_ice_history')

def ldiags_greece():
    refdir=REFERENCE/'greece_60km/reference/strict_diag1h'
    if not refdir.is_dir():
        skip('ldiags_greece');return
    rd=directory('ldiags_greece',spec=(ROOT/'cases/greece_60km/mpas_runs/strict_diag1h.txt').read_text())
    run(rd,30)
    f=rd/'diag.2026-11-08_01.00.00.nc'
    fields=['refl10cm_max','refl10cm_1km','refl10cm_1km_max']
    checked=0
    activity=0
    with Dataset(f) as ds,Dataset(rd/'diag.2026-11-08_00.00.00.nc') as initial,Dataset(refdir/f.name) as ref:
        ds.set_auto_mask(False)
        for name in fields:
            values=np.asarray(ds[name][:]);assert values.size>0,name
            assert np.isfinite(values).all() and (values<1.e30).all(),name
            if name=='refl10cm_1km_max':assert (values>=0.).all()
            if name in ('refl10cm_max','refl10cm_1km'):
                assert np.any(np.asarray(ref[name][:])!=0.),'inactive reference fixture '+name
                assert np.any(values!=np.asarray(initial[name][:])),'l_diags did not update '+name
                activity+=1
            checked+=1
    with Dataset(rd/'history.2026-11-08_01.00.00.nc') as ds:
        values=np.asarray(ds['relhum'][:]);assert values.size>0
        assert np.isfinite(values).all() and (values<1.e30).all();checked+=1
    assert checked==4
    assert activity==2
    if FC=='nvfortran':
        files=sorted(rd.glob('history.*.nc'))+sorted(rd.glob('diag.*.nc'))
        assert files
        for f in files:compare(f,refdir/f.name,FC)
    done('ldiags_greece')

def refl_stream():
    # A user stream may write the 3-D reflectivity.
    rd=directory('refl_stream')
    text=(ROOT/'tests/writer/streams/none.atmosphere').read_text().replace('ertel_pv','refl10cm').replace(' <var name="dtaux3d_ls"/>\n','').replace(' <var name="cldfrac_low_UPP"/>\n','').replace(' <var name="cape"/>\n','')
    (rd/'streams.atmosphere').write_text(text)
    run(rd,1)
    with Dataset(rd/'unsupported.nc') as ds:
        assert 'refl10cm' in ds.variables
    done('refl_stream')

if not have(REFERENCE/'greece_60km/inputs/greece_60km.init.nc',REFERENCE/'x1_2562/inputs/x1.2562.init.nc'):
    print('SKIP writer runs: needs the release case inputs in data/cases (cases/README.md)')
    sys.exit(77)

t0_errors=[]
def check_t0(a,b):
    try: compare(a,b,FC)
    except AssertionError as e:
        print('T0 VALUES FAIL '+str(a)+': '+str(e),flush=True)
        t0_errors.append(str(e))

build()
g=directory('structure_greece');run(g,90)
headers(g,'greece_60km','strict_12h');done('structure_greece')
greece_ref=REFERENCE/'greece_60km/reference/strict_12h'
x1_ref=REFERENCE/'x1_2562/reference/strict_24h'
if have(greece_ref/'history.2026-11-08_03.00.00.nc'):
    # After the first step MPAS's water points hold tslb 273.16 and smois 1 (NoahmpDriverMainMod.F90:135-144).
    with Dataset(g/'history.2026-11-08_03.00.00.nc') as a,Dataset(greece_ref/'history.2026-11-08_03.00.00.nc') as b, \
         Dataset(REFERENCE/'greece_60km/inputs/greece_60km.init.nc') as init:
        water=np.asarray(init['landmask'][:])==0
        for name in ['tslb','smois']:
            x=np.asarray(a[name][0])[water];y=np.asarray(b[name][0])[water]
            assert x.tobytes()==y.tobytes(),f'{name} on water points at 03:00 differs from MPAS (max {np.abs(x-y).max()})'
    done('water_soil_3h')
else:
    skip('water_soil_3h')
pairs=[]
x=directory('structure_x1','x1_2562');run(x,180)
headers(x,'x1_2562','strict_24h');done('structure_x1')
if have(greece_ref,x1_ref):
    for refdir,rd in ((greece_ref,g),(x1_ref,x)):
        for kind in ['history','diag']:
            f=rd/f'{kind}.2026-11-08_00.00.00.nc';ref=refdir/f.name
            pairs += [f,ref];check_t0(f,ref)
    if not t0_errors:done('t0_values')
else:
    skip('t0_values')
l55=directory('structure_l55','x1_2562','l55');run(l55,18)
with Dataset(next(l55.glob('history.*.nc'))) as ds:
    assert len(ds.dimensions['nVertLevels'])==55 and len(ds.dimensions['nVertLevelsP1'])==56
ref=REFERENCE/'x1_2562/reference/strict_l55_3h/diag.2026-11-08_03.00.00.nc'
if have(ref):
    with Dataset(next(l55.glob('diag.*.nc'))) as a,Dataset(ref) as b:
        assert {n:len(d) for n,d in a.dimensions.items()}=={n:len(d) for n,d in b.dimensions.items()}
done('structure_l55')
l55_ref=REFERENCE/'x1_2562/reference/strict_l55_diag1h'
if have(l55_ref):
    for kind in ('history','diag'):
        f=l55/f'{kind}.2026-11-08_00.00.00.nc'
        check_t0(f,l55_ref/f.name)
    done('t0_l55')
else:
    skip('t0_l55')
sea_ice_history(x)

# Writing history and diag or not leaves the state alone: lean's own outputs and the variables of the 03:20 restart
# that both runs carry keep their bytes (the diagnostics' maxima and their alarm follow the streams).
restart='stream restart output_interval 3:20:00\n'
on=directory('identical_on',spec=restart);off=directory('identical_off',spec=restart+'stream output output_interval none\nstream diagnostics output_interval none\n')
for rd in [on,off]:
    run(rd,100,env={k:str(WORK/(rd.name+'.'+s)) for k,s in [('LEAN_PRODUCTS','products'),('LEAN_STORM','storm'),('LEAN_SURFACE','surface'),('LEAN_FILM','film')]})
for suffix in ['products','storm','surface','film']:same(WORK/('identical_on.'+suffix),WORK/('identical_off.'+suffix))
with Dataset(on/'restart.2026-11-08_03.20.00.nc') as ra,Dataset(off/'restart.2026-11-08_03.20.00.nc') as rb:
    shared=sorted((set(ra.variables)&set(rb.variables))-{'lean_alarm_diag'})
    assert len(shared)>800,len(shared)
    for n in shared:compare_variable(ra[n],rb[n])
done('identical');done('products_same')
if pairs:
    command([PY,'tests/writer/read_xarray.py',*pairs],WORK/'xarray-structure.log')
    print((WORK/'xarray-structure.log').read_text().strip(),flush=True)

def user_streams():
    refdir=REFERENCE/'greece_60km/reference/strict_user_3h'
    if not have(refdir/'output_files.txt',greece_ref):
        skip('user');return
    user=directory('user',spec="nml nhyd_model config_run_duration '0_03:00:00'\n")
    shutil.copy(ROOT/'tests/writer/streams/user.atmosphere',user/'streams.atmosphere')
    shutil.copy(ROOT/'tests/writer/streams/stream_list.u_nested',user/'stream_list.u_nested')
    command([NCGEN,'-k','cdf5','-o',user/'u_never.nc',ROOT/'tests/writer/streams/u_never.cdl'],WORK/'ncgen.log')
    untouched=(user/'u_never.nc').read_bytes()
    run(user,90)
    assert (user/'u_never.nc').read_bytes()==untouched
    log=(WORK/'user.log').read_text()
    assert log.count("but clobber_mode is set to 'never_modify'.")==4
    manifest=set()
    for line in (refdir/'output_files.txt').read_text().splitlines():
        words=line.split()
        if len(words)==2:manifest.add(words[1] if words[0].isdigit() else words[0])
    files={str(p.relative_to(user)) for p in user.rglob('*.nc') if not p.is_symlink()}
    assert files==manifest,(files^manifest)
    user_pairs=[]
    for name in sorted(manifest):
        f=user/name;ref=refdir/name
        # MPAS's reference run keeps one file per user stream and lists every file it wrote in output_files.txt; a
        # listed file that was not kept is compared with a kept file of the same stream (history/diag with the
        # greece_60km strict_12h t=0 file).
        if not ref.exists():
            if name.startswith(('history.','diag.')):
                kind=name.split('.')[0]
                ref=greece_ref/f'{kind}.2026-11-08_00.00.00.nc'
            elif name.startswith('grp/'):ref=refdir/'grp/u_group.2026-11-08_02.nc'
            else:ref=next(refdir.glob(name.split('.')[0]+'.*.nc'))
        expected=subprocess.check_output([NCDUMP,'-h',str(ref)],text=True)
        if name.startswith(('history.','diag.')):
            # Same immutable/default field structure; globals come from the actual user run's saved file.
            globals_header=subprocess.check_output([NCDUMP,'-h',str(refdir/'u_multi.nc')],text=True)
            expected=expected.split('// global attributes:')[0]+'// global attributes:'+globals_header.split('// global attributes:')[1]
        cdl=WORK/'reference.cdl';cdl.write_text(expected)
        if name=='u_never.nc':
            # This is the untouched ncgen marker fixture, not a file produced by lean.
            assert subprocess.check_output([NCDUMP,'-h',str(f)],text=True)==expected
        else:
            command(['python3','tests/writer/compare_cdl.py',f,cdl],WORK/'user-header.log')
        user_pairs += [f,ref]
        if name=='u_multi.nc':
            with Dataset(f) as ds:assert len(ds.dimensions['Time'])==4
        if name.startswith('grp/'):
            with Dataset(f) as ds:assert len(ds.dimensions['Time'])==2
        if name.startswith('u_double'):
            with Dataset(f) as ds:assert ds['t2m'].dtype==np.dtype('f8')
    assert len(list(user.glob('u_final*')))==1 and '_03.' in next(user.glob('u_final*')).name
    command([PY,'tests/writer/read_xarray.py',*user_pairs],WORK/'xarray-user.log')
    print((WORK/'xarray-user.log').read_text().strip(),flush=True)
    done('user')
user_streams()

def tiny(rd,mode='never_modify',template='c.nc',interval='00:20:00',extra=''):
    text=(rd/'streams.atmosphere').read_text()
    text=re.sub(r'(<stream name="(?:output|diagnostics)"[\s\S]*?output_interval=")[^"]+',r'\g<1>none',text)
    text=text.replace('</streams>',f'''<stream name="c" type="output" filename_template="{template}" filename_interval="none"
 output_interval="{interval}" clobber_mode="{mode}">
 <var name="xtime"/><var name="Time"/><var name="t2m"/>{extra}</stream></streams>''')
    (rd/'streams.atmosphere').write_text(text)

rd=directory('inactive_package')
tiny(rd,extra='<var name="dtaux3d_ls"/>')
run(rd,1)
log=(WORK/'inactive_package.log').read_text()
assert "asks for 'dtaux3d_ls'" not in log
assert ' * Requested field dtaux3d_ls is deactivated due to packages, or is a scratch variable.' in log
with Dataset(rd/'c.nc') as ds:
    assert set(ds.variables)=={'xtime','Time','t2m'}
done('inactive_package')

for mode in ['never_modify','overwrite','append','truncate']:
    rd=directory('clobber_'+mode);tiny(rd,mode,template='c.$Y-$M-$D_$h.$m.$s.nc' if mode=='never_modify' else 'c.nc')
    if mode=='never_modify':
        text=(rd/'streams.atmosphere').read_text().replace('filename_interval="none"\n output_interval="00:20:00"','filename_interval="output_interval"\n output_interval="00:20:00"')
        (rd/'streams.atmosphere').write_text(text)
    run(rd,30,name=rd.name+'.first')
    initial_files={p.name:p.read_bytes() for p in rd.glob('c*.nc')}
    run(rd,30,name=rd.name+'.second')
    log=(WORK/(rd.name+'.second.log')).read_text()
    if mode=='never_modify':
        assert len(initial_files)==4
        for n,data in initial_files.items():assert (rd/n).read_bytes()==data
        assert log.count("but clobber_mode is set to 'never_modify'.")==4
    else:
        with Dataset(rd/'c.nc') as ds:assert len(ds.dimensions['Time'])==4
        if mode=='append':assert log.count("but clobber_mode is set to 'append'.")==4
        else:assert 'ERROR:' not in log
rd=directory('write_failure');tiny(rd)
(rd/'c.nc.lean_partial').mkdir()
assert run(rd,0,expected=None)!=0
failure=(WORK/'write_failure.log').read_text()
assert failure.count('Error writing one or more output streams')==1
assert not (rd/'c.nc').exists()
done('clobber')

kill_spec="nml nhyd_model config_run_duration '0_00:40:00'\n"
baseline=directory('killed_baseline',spec=kill_spec)
def kill_streams(rd):
    text=(rd/'streams.atmosphere').read_text()
    text=re.sub(r'(<stream name="(?:output|diagnostics)"[\s\S]*?output_interval=")[^"]+',r'\g<1>00:02:00',text)
    text=re.sub(r'(<stream name="diagnostics"[\s\S]*?filename_template=")[^"]+',r'\g<1>diag.nc',text)
    (rd/'streams.atmosphere').write_text(text)
kill_streams(baseline)
command([B/'lean_mpas'],WORK/'killed_baseline.stdout',baseline,dict(os.environ,LEAN_WRITER_DIE='invalid'))
warning=(baseline/'log.atmosphere.0000.out').read_text()
assert warning.count('WARNING: LEAN_WRITER_DIE is a test switch')==1, 'missing LEAN_WRITER_DIE startup warning'
for kill_at in [2,211,315]:
    rd=directory('killed_'+str(kill_at),spec=kill_spec)
    kill_streams(rd)
    code=command([B/'lean_mpas'],WORK/(rd.name+'.stdout'),rd,dict(os.environ,LEAN_WRITER_DIE=str(kill_at)),expected=None)
    assert code==-9,code
    log=(rd/'log.atmosphere.0000.out').read_text()
    completed=log.count('Timing for stream output:')
    if kill_at==2:
        first='history.2026-11-08_00.00.00.nc'
        assert not (rd/first).exists(), 'first write published an incomplete file'
        assert (rd/(first+'.lean_partial')).is_file(), 'first write lost its partial file'
    else:
        assert completed>0, 'append kill did not follow a logged completed output'
    for p in rd.glob('*.nc'):
        if p.is_symlink():continue
        command([NCDUMP,'-h',p],WORK/'killed-header.log')
        with Dataset(p) as ds:
            assert list(ds.variables)[-1]==('hv_oml' if p.name=='diag.nc' else 'tslb')
            if p.name=='diag.nc':
                assert len(ds.dimensions['Time'])==completed, (kill_at,len(ds.dimensions['Time']),completed)
                with Dataset(baseline/p.name) as original:
                    ds.set_auto_mask(False);original.set_auto_mask(False)
                    for name in ('xtime','t2m'):
                        assert ds[name][0].tobytes()==original[name][0].tobytes(), (kill_at,name,'record 1 changed')
                assert ds['xtime'][0].tobytes().rstrip(b'\0 ')==b'2026-11-08_00:00:00'
done('killed')

rd=directory('none_field')
shutil.copy(ROOT/'tests/writer/streams/none.atmosphere',rd/'streams.atmosphere')
assert run(rd,1,expected=None)!=0
log=(WORK/'none_field.log').read_text()
assert "asks for 'ertel_pv', which lean_mpas does not compute" in log
assert "asks for 'cldfrac_low_UPP', which lean_mpas does not compute" in log
assert "asks for 'cape', which lean_mpas does not compute" not in log
assert not [p for p in rd.rglob('*.nc') if not p.is_symlink()]
done('none_field')
rd=directory('start0630',variant='v0630',spec="nml nhyd_model config_start_time '2026-11-08_06:30:00'\nstream output output_interval 3:00:00\nstream diagnostics output_interval 3:00:00\n")
run(rd,90)
for kind in ['history','diag']:
    for stamp,seconds in [('06.30.00',0),('09.30.00',10800)]:
        with Dataset(rd/f'{kind}.2026-11-08_{stamp}.nc') as ds:
            assert ds['Time'].units=='seconds since 2026-11-08 06:30:00'
            assert ds['Time'][0]==seconds
            assert ds['xtime'][0].tobytes().rstrip(b'\0 ')==('2026-11-08_'+stamp.replace('.',':')).encode()
done('start0630')
rd=directory('writer_sfc2',spec='nml physics_lsm_noahmp config_noahmp_iopt_sfc 2\n');run(rd,1)
with Dataset(next(rd.glob('history.*.nc'))) as ds:assert ds.config_noahmp_iopt_sfc==2
done('writer_sfc2')
refl_stream()
if have(greece_ref):
    # The 3-hour run gives the 00 and 03 pairs; extend it for the 06 pair.
    rd=directory('values_6h');run(rd,180)
    pairs_checked=0
    for stamp in ('00','03','06'):
        for kind in ('history','diag'):
            f=rd/f'{kind}.2026-11-08_{stamp}.00.00.nc'
            ref=greece_ref/f.name
            assert f.exists() and ref.exists(),(f,ref)
            # gfortran's state differs from the nvfortran-built MPAS reference after t=0: only t=0 is compared.
            if FC=='gfortran' and stamp!='00':continue
            compare(f,ref,FC);pairs_checked+=1
    assert pairs_checked==(2 if FC=='gfortran' else 6)
    done('values_6h')
else:
    skip('values_6h')
ldiags_greece()
assert not t0_errors,'; '.join(t0_errors)
print(f'WRITER RUNS OK {len(passed)}, {len(skipped)} skipped',flush=True)
cleanup()
