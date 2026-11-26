! Derived from MPAS 8.4.2 physics/physics_mmm/bl_mynn.F90 (NCAR MMM physics, BSD 3-clause licence). Changes are marked lean: or Lean departure.
module lean_bl_mynn
  use ccpp_kind_types,only: kind_phys
  use lean_bl_mynn_common,only: cp , cpv , cliq , cice , ep_1 , ep_2 , ep_3 , grav , karman , p1000mb , &
    & r_d , r_v , svp1 , svp2 , svp3 , svpt0 , xlf , xls , xlv , p608 , t0c , tref , tkmin , tv0 , gtr , &
    & xlvcp , xlscp , rvovrd , rcp , cphh_st , cphm_st , cphh_unst , cphm_unst , b1 , b2 , zero
  use lean_bl_mynn_subroutines
  implicit none
  private
  public:: bl_mynn_init, bl_mynn_finalize, bl_mynn_run
  contains
  subroutine bl_mynn_init(con_cp,con_cpv,con_cice,con_cliq,con_ep1,con_ep2,con_grav,con_karman,con_p0, &
    & con_rd,con_rv,con_svp1,con_svp2,con_svp3,con_svpt0,con_xlf,con_xls,con_xlv,errmsg,errflg)
    real(kind=kind_phys),intent(in)::con_cp,con_cpv,con_cice,con_cliq
    real(kind=kind_phys),intent(in)::con_ep1,con_ep2
    real(kind=kind_phys),intent(in)::con_grav
    real(kind=kind_phys),intent(in)::con_karman
    real(kind=kind_phys),intent(in)::con_p0
    real(kind=kind_phys),intent(in)::con_rd,con_rv
    real(kind=kind_phys),intent(in)::con_svp1,con_svp2,con_svp3,con_svpt0
    real(kind=kind_phys),intent(in)::con_xlf,con_xls,con_xlv
    character(len=*),intent(out)::errmsg
    integer,intent(out)::errflg
    cp = con_cp
    cpv = con_cpv
    cliq = con_cliq
    cice = con_cice
    ep_1 = con_ep1
    ep_2 = con_ep2
    grav = con_grav
    karman = con_karman
    p1000mb = con_p0
    r_d = con_rd
    r_v = con_rv
    rvovrd = r_v/r_d
    svp1 = con_svp1
    svp2 = con_svp2
    svp3 = con_svp3
    svpt0 = con_svpt0
    xlf = con_xlf
    xls = con_xls
    xlv = con_xlv
    ep_3 = 1.-ep_2
    gtr = grav/tref
    p608 = ep_1
    rcp = r_d/cp
    t0c = svpt0
    tv0 = p608*tref
    xlscp = (xlv+xlf)/cp
    xlvcp = xlv/cp
    errmsg = " "
    errflg = 0
  end subroutine bl_mynn_init
  subroutine bl_mynn_finalize(errmsg,errflg)
    character(len=*),intent(out)::errmsg
    integer,intent(out)::errflg
    errmsg = ' '
    errflg = 0
  end subroutine bl_mynn_finalize
  subroutine bl_mynn_run (initflag,delt,dz,dx,u,v,w,th,sqv,sqc,sqi,sqs,qni,p,exner,rho,tt,xland,ts,ps,ust, &
    & hfx,qfx,rmol,wspd,uoce,voce,qke,tsq,qsq,cov,rublten,rvblten,rthblten,rqvblten,rqcblten,rqiblten,rqsblten, &
    & rqniblten,exch_h,exch_m,pblh,kpbl,el_pbl,sh,sm,qc_bl,qi_bl,cldfra_bl,maxwidth,maxmf,ztop_plume, &
    & ktop_plume,errmsg,errflg)
    use config, only: kte => nz, kme => nz1
    integer,parameter ::kts=1
    integer,intent(in)::initflag
    real(kind=kind_phys),intent(in)::delt
    real(kind=kind_phys),intent(in)::dx,xland,ps,ts,ust,hfx,qfx,rmol,wspd,uoce,voce
    real(kind=kind_phys),intent(in),dimension(kts:kte)::dz,u,v,th,tt,p,exner,rho
    real(kind=kind_phys),intent(inout),dimension(kts:kte)::sqv,sqc,sqi,sqs,qni
    real(kind=kind_phys),intent(in),dimension(kts:kme)::w
    integer,intent(inout)::kpbl,ktop_plume
    real(kind=kind_phys),intent(inout)::pblh
    real(kind=kind_phys),intent(inout),dimension(kts:kte)::cldfra_bl,qc_bl,qi_bl
    real(kind=kind_phys),intent(inout),dimension(kts:kte)::el_pbl,qke,cov,qsq,tsq,sh,sm
    real(kind=kind_phys),intent(inout),dimension(kts:kte)::rublten,rvblten,rthblten,rqvblten,rqcblten, &
      & rqiblten,rqsblten,rqniblten
    character(len=*),intent(out)::errmsg
    integer,intent(out)::errflg
    real(kind=kind_phys),intent(out)::maxwidth,maxmf,ztop_plume
    real(kind=kind_phys),intent(out),dimension(kts:kte)::exch_h,exch_m
    logical::initialize_qke
    integer ::k
    real(kind=kind_phys)::cpm,flq,flqc,flqv,flt,fltv,phh,pmz,psig_bl,psig_shcu,phi_m,th_sfc,zet
    real(kind=kind_phys),dimension(kts:kte)::qv1
    real(kind=kind_phys),dimension(kts:kte)::det_sqc,det_u,det_v,sub_u,sub_v
    real(kind=kind_phys),dimension(kts:kte)::pdc,pdk,pdq,pdt,sgm,sqw,thetav,thl,vq,vt,kzero
    real(kind=kind_phys),dimension(kts:kte)::dfh,dfm,dfq,qcd,tcd,diss_heat
    real(kind=kind_phys),dimension(kts:kte)::rstoch_col
    real(kind=kind_phys),dimension(kts:kte+1)::zw
    real(kind=kind_phys),dimension(kts:kte+1)::s_aw1,s_awthl1,s_awqt1,s_awqv1,s_awqc1,s_awu1,s_awv1,s_awqke1, &
      & s_awqni1
    real(kind=kind_phys),dimension(kts:kte+1)::sd_aw1,sd_awthl1,sd_awqv1,sd_awqc1,sd_awu1,sd_awv1
    real(kind=kind_phys),dimension(kts:kte)::tkeprodtd
    real(kind=kind_phys)::rmol1
    real(kind=kind_phys),dimension(kts:kte)::edmf_a1,edmf_w1,edmf_qt1,edmf_thl1,edmf_ent1,edmf_qc1,sub_thl1, &
      & sub_sqv1,det_thl1,det_sqv1
    real(kind=kind_phys)::delt2
    call prepare_column()
    call initialize_turbulence()
    call surface_buoyancy()
    call clouds_and_plumes()
    call advance_turbulence_and_mix()
    contains
    subroutine prepare_column()
      errmsg = " "
      errflg = 0
      do k = kts,kte
        rublten(k) = 0._kind_phys
        rvblten(k) = 0._kind_phys
        rthblten(k) = 0._kind_phys
        rqvblten(k) = 0._kind_phys
        rqcblten(k) = 0._kind_phys
        rqiblten(k) = 0._kind_phys
        rqsblten(k) = 0._kind_phys
        rqniblten(k) = 0._kind_phys
      enddo
      ktop_plume = 0
      maxwidth = 0._kind_phys
      maxmf = 0._kind_phys
      ztop_plume = 0._kind_phys
      rmol1 = rmol
      do k = kts,kte
        kzero(k) = 0._kind_phys
      enddo
      do k = kts,kte
        rstoch_col(k) = 0._kind_phys
      enddo
      ktop_plume = 0
      maxwidth = 0._kind_phys
      maxmf = 0._kind_phys
      ztop_plume = 0._kind_phys
      ! lean: MPAS zeroes the tendencies a second time here; they are already zero (above), so that is left out
      ! lean: of the plume arrays only edmf_a1 and edmf_w1 are zeroed here (mym_initialize reads them at the first step); the others
      ! are DMP_mf outputs that DMP_mf zeroes itself before any read, and nothing reads them before it is called
      do k = kts,kte
        edmf_a1(k) = 0._kind_phys
        edmf_w1(k) = 0._kind_phys
      enddo
      do k = kts,kte
        exch_h(k) = 0._kind_phys
        exch_m(k) = 0._kind_phys
      enddo
      do k = kts,kte+1
        sd_aw1(k) = 0._kind_phys
        sd_awthl1(k) = 0._kind_phys
        sd_awqv1(k) = 0._kind_phys
        sd_awqc1(k) = 0._kind_phys
        sd_awu1(k) = 0._kind_phys
        sd_awv1(k) = 0._kind_phys
      enddo
      do k = kts,kte
        qv1(k) = sqv(k)/(1.-sqv(k))
      enddo
      k = kts
      zw(k) = 0._kind_phys
      do k = kts+1,kte+1
        zw(k) = zw(k-1) + dz(k-1)
      enddo
      do k = kts,kte
        sqw(k) = sqv(k)+sqc(k)+sqi(k)
        thl(k) = th(k) - xlvcp/exner(k)*sqc(k) - xlscp/exner(k)*(sqi(k))
        thetav(k) = th(k)*(1.+0.608*sqv(k))
      enddo
    end subroutine prepare_column
    subroutine initialize_turbulence()
      if(initflag > 0) then
        initialize_qke = .true.
        do k = kts,kte
          sh(k) = 0._kind_phys
          sm(k) = 0._kind_phys
          el_pbl(k) = 0._kind_phys
          tsq(k) = 0._kind_phys
          qsq(k) = 0._kind_phys
          cov(k) = 0._kind_phys
          cldfra_bl(k) = 0._kind_phys
          qc_bl(k) = 0._kind_phys
          qi_bl(k) = 0._kind_phys
          qke(k) = 0._kind_phys
        enddo
        if(initialize_qke) then
          do k = kts,kte
            qke(k)=5.*ust*max((ust*700.-zw(k))/(max(ust,0.01)*700.),0.01)
          enddo
        endif
        call get_pblh(pblh,thetav,qke,zw,dz,xland,kpbl)
        call scale_aware(dx,pblh,psig_bl,psig_shcu)
        call mym_initialize(xland,dz,dx,zw,u,v,thl,sqv,pblh,th,thetav,sh,sm,ust,rmol1,el_pbl,qke,tsq,qsq,cov, &
          & psig_bl,cldfra_bl,edmf_w1,edmf_a1,initialize_qke)
      endif
    end subroutine initialize_turbulence
    subroutine surface_buoyancy()
      call get_pblh(pblh,thetav,qke,zw,dz,xland,kpbl)
      call scale_aware(dx,pblh,psig_bl,psig_shcu)
      cpm = cp*(1.+0.84*qv1(kts))
      flqv = qfx/rho(kts)
      flqc = 0.0
      th_sfc = ts/exner(kts)
      flq = flqv + flqc
      flt = hfx/(rho(kts)*cpm ) - xlvcp*flqc/exner(kts)
      fltv = flt + flqv*p608*th_sfc
      rmol1 = -karman*gtr*fltv/max(ust**3,1.0e-6)
      zet = 0.5*dz(kts)*rmol1
      zet = max(zet, -20.)
      zet = min(zet, 20.)
      phi_m = phim(zet)
      pmz = phi_m - zet
      phh = phih(zet)
    end subroutine surface_buoyancy
    subroutine clouds_and_plumes()
      do k = kts,kte
        vt(k) = 0._kind_phys
        vq(k) = 0._kind_phys
        sgm(k) = 0._kind_phys
      enddo
      call mym_condensation(dz,xland,thl,sqw,sqc,sqi,sqs,p,exner,tsq,qsq,cov,sh,el_pbl,qc_bl,qi_bl,cldfra_bl, &
        & pblh,vt,vq,th,sgm,rstoch_col)
      do k = kts,kte
        tkeprodtd(k) = 0._kind_phys
      enddo
      call dmp_mf(zw,dz,p,rho,u,v,w,th,thl,thetav,tt,sqw,sqv,sqc,qke,qni,exner,vt,vq,flt,fltv,flq,pblh,dx, &
        & xland,th_sfc,edmf_a1,edmf_w1,edmf_qt1,edmf_thl1,edmf_ent1,edmf_qc1,s_aw1,s_awthl1,s_awqt1,s_awqv1, &
        & s_awqc1,s_awu1,s_awv1,s_awqke1,s_awqni1,sub_thl1,sub_sqv1,sub_u,sub_v,det_thl1,det_sqv1,det_sqc,det_u, &
        & det_v,qc_bl,cldfra_bl,psig_shcu,maxwidth,ktop_plume,maxmf,ztop_plume,rstoch_col)
    end subroutine clouds_and_plumes
    subroutine advance_turbulence_and_mix()
      delt2 = delt
      call mym_turbulence(xland,dz,dx,zw,u,v,thl,thetav,sqc,sqw,qke,vt,vq,rmol1,flt,fltv,flq,pblh,th,sh,sm, &
        & el_pbl,dfm,dfh,dfq,tcd,qcd,pdk,pdt,pdq,pdc,psig_bl,cldfra_bl,edmf_w1,edmf_a1,tkeprodtd)
      call mym_predict(delt2,dz,ust,flt,flq,pmz,phh,el_pbl,dfq,rho,pdk,pdt,pdq,pdc,qke,tsq,qsq,cov,s_aw1,s_awqke1)
      do k = kts,kte-1
        diss_heat(k) = min(max(1.0*(qke(k)**1.5)/(b1*max(0.5*(el_pbl(k)+el_pbl(k+1)),1.))/cp,0.0),0.002)
        diss_heat(k) = diss_heat(k) * exp(-10000./MAX(p(k),1.))
      enddo
      diss_heat(kte) = 0.
      call mynn_tendencies(delt,dz,rho,u,v,th,tt,qv1,qni,ps,p,exner,thl,sqv,sqc,sqi,kzero,sqw,ust,flt,flqv, &
        & flqc,wspd,uoce,voce,tcd,qcd,dfm,dfh,rublten,rvblten,rthblten,rqvblten,rqcblten,rqiblten,rqsblten, &
        & rqniblten,diss_heat,s_aw1,s_awthl1,s_awqv1,s_awqc1,s_awu1,s_awv1,s_awqni1,sd_aw1,sd_awthl1,sd_awqv1, &
        & sd_awqc1,sd_awu1,sd_awv1,sub_thl1,sub_sqv1,sub_u,sub_v,det_thl1,det_sqv1,det_sqc,det_u,det_v)
      call retrieve_exchange_coeffs(dfm,dfh,dz,exch_m,exch_h)
    end subroutine advance_turbulence_and_mix
  end subroutine bl_mynn_run
end module lean_bl_mynn
