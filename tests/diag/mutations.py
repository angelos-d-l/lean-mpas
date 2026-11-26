"""Mutations of src/diag/diag_isobaric.f90 that the isobaric checks must see: each mutant is compiled into the check
program, which must then fail. Run serially after a strict CPU build (./lean build); output_families needs the MPAS
reference outputs of data/cases, which are not released."""
from pathlib import Path
import json,shlex,subprocess,sys
ROOT=Path(__file__).resolve().parents[2]
B=ROOT/'build/cpu-gfortran-strict'
WORK=ROOT/'build/diag/mutations';WORK.mkdir(parents=True,exist_ok=True)
fc=json.loads((B/'build.json').read_text())['compiler']['path']
flags=shlex.split((B/'flags.txt').read_text())
inc=['-I'+s for s in (B/'moddirs.txt').read_text().splitlines()]
objects=[s for s in (B/'objects.txt').read_text().splitlines() if not s.endswith('/main.f90.o')]
libs=shlex.split((B/'libs.txt').read_text())
def command(args,log,cwd=ROOT):
    with log.open('w') as out:p=subprocess.run(list(map(str,args)),cwd=cwd,stdout=out,stderr=subprocess.STDOUT)
    return p.returncode
oracle=WORK/'oracle.o'
assert command([fc,*flags,*inc,'-J'+str(WORK),'-c',ROOT/'tests/diag/mpas_oracle.f90','-o',oracle],WORK/'oracle-compile.log')==0
source=(ROOT/'src/diag/diag_isobaric.f90').read_text()
mutations=[
 ('interp','  if(p_out(k)<p_in(1))then','  if(.true.)then','isobaric','FAIL interp_column bits'),
 ('slp','         slp=0.','         slp=1.','isobaric','FAIL slp_failure all cells zero'),
 ('layer','           layerMean(:) = 0.0','           layerMean(:) = 1.0','isobaric','FAIL layer_mean_order all cells zero'),
 ('temperature_only',' end subroutine\n ! MPAS:1005',' if(.not.out%need(1))out%t_isobaric=0.\n end subroutine\n ! MPAS:1005','output_families','FAIL t_isobaric_alone'),
 ('kite_order','    do j=1,size(cells_on_vertex,1)','    do j=size(cells_on_vertex,1),1,-1','output_families','FAIL isobaric vertex oracle bits'),
 ('slp_message','Troubles finding level ','Wrong finding level ','messages',''),
 ('layer_message','Error in compute_layer_mean: pressure should increase with index','wrong layer order message','messages',''),
]
for name,old,new,test,label in mutations:
    assert old in source,(name,'mutation not applied')
    work=WORK/name;work.mkdir(exist_ok=True)
    path=work/'diag_isobaric.f90';path.write_text(source.replace(old,new))
    obj=work/'diag_isobaric.o'
    assert command([fc,*flags,*inc,'-J'+str(work),'-c',path,'-o',obj],work/'compile.log')==0,(name,'compile')
    test_source='isobaric' if test=='messages' else test
    exe=work/'test'
    rest=[o for o in objects if not o.endswith('/diag_isobaric.f90.o')]
    assert command([fc,*flags,'-I'+str(work),'-I'+str(WORK),*inc,'-o',exe,ROOT/f'tests/diag/{test_source}.f90',oracle,obj,*rest,*libs],work/'link.log')==0,(name,'link')
    args=[exe]
    if test=='output_families':args += [ROOT,'greece_60km']
    status=command(args,work/'run.log',work)
    if test=='messages':
        assert status==0,(name,'numeric tests failed before message check')
        status=command([sys.executable,ROOT/'tests/diag/slp_messages.py',work/'log.atmosphere.0000.out'],work/'messages.log')
        assert status!=0,(name,'message mutation survived')
    else:
        assert status!=0 and label in (work/'run.log').read_text(),(name,(work/'run.log').read_text()[-1000:])
    print('MUTATION SEEN '+name+(' '+label if label else ' message assertion'),flush=True)
print('DIAG MUTATIONS OK',flush=True)
