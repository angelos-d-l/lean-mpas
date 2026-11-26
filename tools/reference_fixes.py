"""Verify that the six files of reference_fixes/ are exactly the MPAS 8.4.2 originals of reference/mpas_src
(./lean fetch-upstream) plus their listed fixes, and print the sha256 of both forms:
  module_cu_gf.mpas.F          qco(i,k) initialised next to qrco(i,k)
  mpas_atmphys_o3climatology.F a latitude equal to the last ozone latitude takes the end branch (.ge., not .gt.)
  mpas_timekeeping.F           mpas_add_clock_alarm sets ierr = 0 whatever ESMF returned
  module_mp_thompson.F         the four sedimentation sub-step counts guarded against the integer range
  mpas_isobaric_diagnostics.F  the cell temperature is also computed when only t_isobaric or meanT_500_300 is requested
  summarize_timestep.inc       summarize_timestep of mpas_atm_time_integration.F, its five extremum-location lookups
                               run only when an extremum was found
docs/fixes.md says why each fix is needed.

    python3 tools/reference_fixes.py
"""
from pathlib import Path
import hashlib,json,re
ROOT=Path(__file__).resolve().parents[1]
PRISTINE=ROOT/'reference/mpas_src/src/core_atmosphere/physics/physics_wrf/module_cu_gf.mpas.F'
PATCHED=ROOT/'reference_fixes/module_cu_gf.mpas.F'
OLD=b'        qrco(i,k)=0.\n        cd(i,k)=1.*entr_rate\n'
NEW=b'        qrco(i,k)=0.\n        qco(i,k)=0.\n        cd(i,k)=1.*entr_rate\n'
OZONE_PRISTINE=ROOT/'reference/mpas_src/src/core_atmosphere/physics/mpas_atmphys_o3climatology.F'
OZONE_PATCHED=ROOT/'reference_fixes/mpas_atmphys_o3climatology.F'
CLOCK_PRISTINE=ROOT/'reference/mpas_src/src/framework/mpas_timekeeping.F'
CLOCK_PATCHED=ROOT/'reference_fixes/mpas_timekeeping.F'
THOMPSON_PRISTINE=ROOT/'reference/mpas_src/src/core_atmosphere/physics/physics_wrf/module_mp_thompson.F'
THOMPSON_PATCHED=ROOT/'reference_fixes/module_mp_thompson.F'
ISOBARIC_PRISTINE=ROOT/'reference/mpas_src/src/core_atmosphere/diagnostics/mpas_isobaric_diagnostics.F'
ISOBARIC_PATCHED=ROOT/'reference_fixes/mpas_isobaric_diagnostics.F'
SUMMARY_PRISTINE=ROOT/'reference/mpas_src/src/core_atmosphere/dynamics/mpas_atm_time_integration.F'
SUMMARY_PATCHED=ROOT/'reference_fixes/summarize_timestep.inc'
# the four lines that turn the offset of an extremum into its level, index, latitude and longitude
LOOKUP=re.compile(rb'^         kMax = mod\(offset_1d, size\((w|u|spd),1\)\) \+ 1\n'
                  rb'         indexMax = \(offset_1d / size\(\1,1\)\) \+ 1(?: ! Integer divide)?\n'
                  rb'         latMax = lat(Cell|Edge)\(indexMax\)\n'
                  rb'         lonMax = lon\2\(indexMax\)\n', re.MULTILINE)
def sha(data): return hashlib.sha256(data).hexdigest()
def guarded(m):
    return b'         if (offset_1d /= huge(1)) then\n'+b''.join(b'   '+line for line in m[0].splitlines(keepends=True))+b'         end if\n'
def verify():
    original=PRISTINE.read_bytes()
    assert hashlib.sha256(original).hexdigest()=='b63086afc7f7213d1ec04de13a2b4a77dde718167c22304a030741da05fb5baf', 'pristine GF hash changed'
    assert original.count(OLD)==1
    assert PATCHED.read_bytes()==original.replace(OLD,NEW), 'reference fixes differ from the single listed initialization'
    ozone=OZONE_PRISTINE.read_bytes()
    assert hashlib.sha256(ozone).hexdigest()=='adbd1e35946cdb99c9b4da7c4be608a10b8bd45cff68e4bc1373f3afcf16c80c', 'pristine ozone hash changed'
    old=b'if(lat .gt. lat_ozone(latsiz)) then'
    new=b'if(lat .ge. lat_ozone(latsiz)) then'
    assert ozone.count(old)==1
    assert OZONE_PATCHED.read_bytes()==ozone.replace(old,new), 'ozone fix differs from listed endpoint guard'
    clock=CLOCK_PRISTINE.read_bytes()
    assert hashlib.sha256(clock).hexdigest()=='9e8e8a81480a5a26cd5a81ca94c38f0e247e5f5ef51923349ea5d2bba343bee6', 'pristine clock hash changed'
    a=clock.index(b'   subroutine mpas_add_clock_alarm(')
    b=clock.index(b'   end subroutine mpas_add_clock_alarm',a)
    old=b'         if (present(ierr)) then\n            if (ierr == ESMF_SUCCESS) ierr = 0\n         end if'
    assert clock[a:b].count(old)==1
    assert CLOCK_PATCHED.read_bytes()==clock[:a]+clock[a:b].replace(old,b'         if (present(ierr)) ierr = 0')+clock[b:]
    thompson=THOMPSON_PRISTINE.read_bytes()
    assert hashlib.sha256(thompson).hexdigest()=='1ba1f30d486bc8e2a2e6295f8495877f963f7995a047558f8a68ff80012a70e5', 'pristine Thompson hash changed'
    old=b'nstep = MAX(nstep, INT(DT/delta_tp + 1.))'
    assert thompson.count(old)==4
    assert THOMPSON_PATCHED.read_bytes()==thompson.replace(old,b'if (DT/delta_tp + 1. .lt. 2147483648.) '+old), 'Thompson fix differs from the four listed guards'
    isobaric=ISOBARIC_PRISTINE.read_bytes()
    assert sha(isobaric)=='b7c5ffed6c43f1ee86612174fb1aac4d33051a29a945fb39d93e67170b855de0', 'pristine isobaric hash changed'
    old=b'if (NEED_TEMP .or. NEED_RELHUM .or. NEED_DEWPOINT .or. need_mslp) then'
    assert isobaric.count(old)==1
    assert ISOBARIC_PATCHED.read_bytes()==isobaric.replace(old,old[:-6]+b' .or. need_t_isobaric .or. need_meanT_500_300) then'), \
        'isobaric fix differs from the listed condition'
    timeint=SUMMARY_PRISTINE.read_bytes()
    assert sha(timeint)=='579acb2819b565760822194401d5af9e33c129a4114ac03e6882d7abdbabc757', 'pristine time integration hash changed'
    first,last=b'   subroutine summarize_timestep(domain)\n',b'   end subroutine summarize_timestep\n'
    assert timeint.count(first)==1 and timeint.count(last)==1
    a=timeint.index(first); b=timeint.index(last,a)+len(last)
    summary,count=LOOKUP.subn(guarded,timeint[a:b])
    assert count==5, f'location guard matched {count} times, expected five'
    assert SUMMARY_PATCHED.read_bytes()==summary, 'summarize_timestep fix differs from the five listed guards'
    files={'gf':(original,PATCHED),'ozone':(ozone,OZONE_PATCHED),'clock':(clock,CLOCK_PATCHED),'thompson':(thompson,THOMPSON_PATCHED),
           'isobaric':(isobaric,ISOBARIC_PATCHED),'summary':(timeint,SUMMARY_PATCHED)}
    out={'checked':[patched.name for _,patched in files.values()]}
    for key,(pristine,patched) in files.items():
        out[f'{key}_pristine_sha256']=sha(pristine)
        out[f'{key}_patched_sha256']=sha(patched.read_bytes())
    return out
if __name__=='__main__':
    print(json.dumps(verify(),indent=2))
