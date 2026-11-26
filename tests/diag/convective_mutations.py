"""Mutations of src/diag/diag_convective.f90 (each convective output, and the helper arithmetic kept from MPAS) that the
convective checks must see. Run serially after a strict CPU build; output_families needs the MPAS reference outputs of
data/cases, which are not released."""
from pathlib import Path
import json,shlex,subprocess
ROOT=Path(__file__).resolve().parents[2];B=ROOT/'build/cpu-gfortran-strict';W=ROOT/'build/diag/conv_mutations';W.mkdir(parents=True,exist_ok=True)
fc=json.loads((B/'build.json').read_text())['compiler']['path'];flags=shlex.split((B/'flags.txt').read_text());inc=['-I'+s for s in (B/'moddirs.txt').read_text().splitlines()];objects=[s for s in (B/'objects.txt').read_text().splitlines() if not s.endswith(('/main.f90.o','/diag_convective.f90.o'))];libs=shlex.split((B/'libs.txt').read_text())
def run(args,log,cwd=ROOT):
 with log.open('w') as out:p=subprocess.run(list(map(str,args)),cwd=cwd,stdout=out,stderr=subprocess.STDOUT)
 return p.returncode
assert run([fc,*flags,*inc,'-J'+str(W),'-c','tests/diag/mpas_oracle.f90','-o',W/'oracle.o'],W/'oracle-compile.log')==0
src=(ROOT/'src/diag/diag_convective.f90').read_text()
fields=['cape','cin','srh_0_1km','srh_0_3km','uzonal_surface','uzonal_1km','uzonal_6km','umeridional_surface','umeridional_1km','umeridional_6km','temperature_surface','dewpoint_surface']
mutations=[]
old=' end subroutine\n      recursive real (kind=RKIND) function column_height_value'
for i,name in enumerate(fields,1):
 mutations.append((name,old,f' out({i})=out({i})+1.\n'+old,'output_families','FAIL convective oracle bits '+name))
for name,old,new,label in [
 ('getcape','    return\n    end subroutine getcape','    cape=cape+1.\n    return\n    end subroutine getcape','getcape synthetic bits'),
 ('height','      column_height_value = wz*column_values(kz) + wzp1*column_values(kz+1)','      column_height_value = 0.','srh_and_levels height bits'),
 ('staggered','      end function integral_zstaggered','      integral_zstaggered=0.\n      end function integral_zstaggered','srh_and_levels staggered bits'),
 ('point','      end function integral_zpoint','      integral_zpoint=0.\n      end function integral_zpoint','srh_and_levels point bits')]:
 mutations.append((name,old,new,'convective','FAIL '+label))
for name,old,new,test,label in mutations:
 assert old in src,(name,'mutation not applied')
 d=W/name;d.mkdir(exist_ok=True);source=d/'diag_convective.f90';source.write_text(src.replace(old,new))
 assert run([fc,*flags,*inc,'-J'+str(d),'-c',source,'-o',d/'mutant.o'],d/'compile.log')==0,(name,'compile')
 assert run([fc,*flags,'-I'+str(d),'-I'+str(W),*inc,'-o',d/'test',ROOT/f'tests/diag/{test}.f90',W/'oracle.o',d/'mutant.o',*objects,*libs],d/'link.log')==0,(name,'link')
 args=[d/'test']+([ROOT,'greece_60km'] if test=='output_families' else [])
 code=run(args,d/'run.log',d)
 assert code!=0 and label in (d/'run.log').read_text(),(name,(d/'run.log').read_text()[-1500:])
 print('MUTATION SEEN '+name+' '+label,flush=True)
print('CONVECTIVE MUTATIONS OK')
