! The conversions around MYNN PBL: mixing ratios to the specific contents MYNN mixes, and its new specific contents back
! to mixing-ratio tendencies (physics/physics_wrf/bl_mynn_pre.F and bl_mynn_post.F of MPAS 8.4.2).
module mynn_conversions
 use config, only: nz
 implicit none
 private
 public :: pbl_specific_contents, pbl_mixing_tendencies
contains
 subroutine pbl_specific_contents(qv,qc,qi,qs,sqv,sqc,sqi,sqs)
 real,intent(in) :: qv(nz),qc(nz),qi(nz),qs(nz)
 real,intent(out) :: sqv(nz),sqc(nz),sqi(nz),sqs(nz)
 integer :: k
 do k=1,nz
  sqv(k)=qv(k)/(1.+qv(k))
 enddo
 do k=1,nz
  sqc(k)=qc(k)/(1.+qv(k))
 enddo
 do k=1,nz
  sqi(k)=qi(k)/(1.+qv(k))
 enddo
#ifdef LEAN_MYNN_MOIST
 ! Lean departure (build switch LEAN_MYNN_MOIST, on in flavor lean, off in mpas): snow per moist mass like cloud and ice. MYNN does not
 ! mix snow; sqs reaches only rh_hack in mym_condensation.
 do k=1,nz
  sqs(k)=qs(k)/(1.+qv(k))
 enddo
#else
 ! As MPAS (physics_wrf/bl_mynn_pre.F:135): snow is divided by (1+qs), not (1+qv) as cloud and ice are; kept so that the
 ! default build matches MPAS (LEAN_MYNN_MOIST changes it).
 do k=1,nz
  sqs(k)=qs(k)/(1.+qs(k))
 enddo
#endif
 end subroutine

 subroutine pbl_mixing_tendencies(delt,qv,qc,qi,qs,dqv,dqc,dqi,dqs)
 real,intent(in) :: delt,qv(nz),qc(nz),qi(nz),qs(nz)
 real,intent(inout) :: dqv(nz),dqc(nz),dqi(nz),dqs(nz)
 real :: sqv(nz),sqc(nz),sqi(nz),sqs(nz),sq,rq
 integer :: k
#ifdef LEAN_MYNN_MOIST
 ! Lean departure (build switch LEAN_MYNN_MOIST, on in flavor lean, off in mpas). MYNN mixes the specific contents s = q/(1+qv) with the
 ! moist density rho_d*(1+qv) held fixed, so a change ds is a change rho_d*(1+qv)*ds of the species' mass per volume. At
 ! the fixed dry density of the dynamics that is dq = (1+qv)*ds. MPAS instead converts the new s back with s/(1-s)
 ! (vapour) and s*(1+s_v) (cloud, ice, snow): every vapour change, the surface flux included, comes out (1+qv) too large,
 ! and a zero tendency takes q*qv**2 of the condensate away each call. Here a zero tendency stays exactly zero and the
 ! column keeps MYNN's water.
 do k=1,nz
  dqv(k)=dqv(k)*(1.+qv(k))
 enddo
 do k=1,nz
  dqc(k)=dqc(k)*(1.+qv(k))
 enddo
 do k=1,nz
  dqi(k)=dqi(k)*(1.+qv(k))
 enddo
 do k=1,nz
  dqs(k)=dqs(k)*(1.+qv(k))
 enddo
#else
 do k=1,nz
  sq=qv(k)/(1.+qv(k))
  sqv(k)=sq+dqv(k)*delt
  rq=sqv(k)/(1.-sqv(k))
  dqv(k)=(rq-qv(k))/delt
 enddo
 do k=1,nz
  sq=qc(k)/(1.+qv(k))
  sqc(k)=sq+dqc(k)*delt
  rq=sqc(k)*(1.+sqv(k))
  dqc(k)=(rq-qc(k))/delt
 enddo
 do k=1,nz
  sq=qi(k)/(1.+qv(k))
  sqi(k)=sq+dqi(k)*delt
  rq=sqi(k)*(1.+sqv(k))
  dqi(k)=(rq-qi(k))/delt
 enddo
 do k=1,nz
  sq=qs(k)/(1.+qv(k))
  sqs(k)=sq+dqs(k)*delt
  rq=sqs(k)*(1.+sqv(k))
  dqs(k)=(rq-qs(k))/delt
 enddo
#endif
 end subroutine
end module
