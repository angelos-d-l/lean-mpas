! LEAN_GF_NOSHALLOW and LEAN_GF_DXMIN on synthetic columns (tests/gf_synthetic.f90, 4096 soundings, two calls each):
! for one build, one record per column and call with GF's outputs and the cell's dx, written to the build's directory;
! tests/suite_coupling/gf_switches.py compares the builds' records.
program gf_switches
 use config,only:nz
 use model_state
 use gf_state
 use gf_fixture,only:gf_sample
 use gf_synthetic
 use grell_freitas,only:advance_gf
 use phys_state, only: zero_gf, zero_state
 implicit none
 type(state_type),target,save::state
 type(gf_state_type),target,save::gf
 type(column_view)::a
 type(gf_column)::f
 type(gf_sample)::r
 integer::u,c,it
 call allocate_state(state); call zero_state(state)
 call allocate_gf(gf); call zero_gf(gf)
 a=view_column(state,1)
 f=gf_view(gf,1)
 call init_gf_synthetic()
 open(newunit=u,file='gf-switches.bin',form='unformatted',access='stream',status='replace')
 do c=1,4096
  call make_gf_synthetic(c,a,r)
  call synthetic_gf_state(r,f)
  do it=2,3
   call advance_gf(a,f,it)
   write(u) a%dx,f%hbot,f%htop,f%xmb_total,f%xmb_shallow,f%raincv,f%pratec,f%k22_shallow,f%kbcon_shallow,f%ktop_shallow, &
    f%rthcuten,f%rqvcuten,f%rqccuten,f%rqicuten,f%qc_cu,f%qi_cu
  enddo
 enddo
 close(u)
end program
