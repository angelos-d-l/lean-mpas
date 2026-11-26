"""The order of the diagnostic and output calls in the time loop of src/lean_model.f90 (start and step), including the
reset made only at a cold start, then the same fragments run with stub operations."""
from pathlib import Path
s=Path('src/lean_model.f90').read_text()
a=s.index('  call plan_start(set,clock,cfg,output_plan)')
b=s.index(' call cpu_time(loop_cpu)',a)
c=s.index('   call plan_step_outputs(set,clock,output_plan)')
d=s.index(' enddo',c)
for block,calls in [(s[a:b],['plan_start','diag_start','diag_written','diag_reset','diag_update','output_fields_to_host','diag_compute','write_streams','diag_reset']),
                      (s[c:d],['plan_step_outputs','diag_written','diag_update','output_fields_to_host','diag_compute','write_streams','diag_reset'])]:
    cursor=0
    for call in calls:
        at=block.find('call '+call+'(',cursor)
        assert at>=0, f'call order: missing {call}'
        cursor=at+len('call '+call+'(')
assert s.index('call diag_setup(')<s.index('call model_device_start()')
assert s.index('call diag_start_step(')>s.index('if(do_sw)call reset_alarm('), 'diag_start_step before radiation resets'
assert s.index('call diag_start_step(')>s.index("call log_write('--- time to run the SW")
print('DIAG ORDER OK: reset update fetch compute write reset; update fetch compute write reset')

# Run the integration fragments with stub operations. Keep the actual placement inside
# any_output: a three-step run writes only at step 2, in addition to its start record.
import re
import subprocess
import sys
mapping={'diag_reset':'reset','diag_update':'update','output_fields_to_host':'fetch',
         'diag_compute':'compute','write_streams':'write'}
def fragment(block):
    stack=[];result=[]
    for line in block.splitlines():
        text=line.strip().lower()
        if re.match(r'if\s*\(.*\)\s*then$',text):
            stack.append('output_plan%any_output' in text or (stack[-1] if stack else False))
        elif text in ('endif','end if'):
            # The fragment begins inside present(set); its final endif closes that outer guard.
            if stack:stack.pop()
        match=re.search(r'\bcall (\w+)\(',text)
        if match and match[1] in mapping:
            prefix='if(writing)' if stack and stack[-1] else ''
            result.append(prefix+"call stub('"+mapping[match[1]]+"')")
    assert not stack,'unclosed integration fragment'
    assert result,'empty integration fragment'
    return result
start=fragment(s[a:b]);step=fragment(s[c:d])
out=Path('build/diag');out.mkdir(parents=True,exist_ok=True)
source='''program order_driver
implicit none
integer::it
logical::writing
character(256)::line
 do it=0,3
 writing=it==0.or.it==2
 line=''
 if(it==0)then
'''+ '\n'.join(start)+'''
 else
'''+ '\n'.join(step)+'''
 endif
 print '(a)',trim(line)
 enddo
contains
subroutine stub(name)
character(*),intent(in)::name
if(len_trim(line)>0)line=trim(line)//' '
line=trim(line)//' '//name
line=adjustl(line)
end subroutine
end program
'''
path=out/'order_driver.f90';path.write_text(source)
compiler=sys.argv[1] if len(sys.argv)>1 else 'gfortran'
subprocess.run([compiler,'-o',str(out/'order_driver'),str(path)],check=True)
actual=subprocess.check_output([str(out/'order_driver')],text=True).splitlines()
expected=['reset update fetch compute write reset','update','update fetch compute write reset','update']
assert actual==expected,(actual,expected)
print('DIAG STUB WRITER OK: '+'; '.join(actual))
