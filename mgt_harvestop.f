      subroutine mgt_harvestop

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the harvest operation (no kill)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    auto_eff(:) |none           |fertilizer application efficiency calculated
!!                                |as the amount of N applied divided by the
!!                                |amount of N removed at harvest
!!    cnyld(:)    |kg N/kg yield  |fraction of nitrogen in yield
!!    cpyld(:)    |kg P/kg yield  |fraction of phosphorus in yield
!!    curyr       |none           |current year in simulation
!!    harveff       |none         |harvest efficiency: fraction of harvested 
!!                                |yield that is removed from HRU; the 
!!                                |remainder becomes residue on the soil
!!                                |surface
!!    hi_ovr      |(kg/ha)/(kg/ha)|harvest index target specified at
!!                                |harvest
!!    hru_dafr(:) |km2/km2        |fraction of watershed area in HRU
!!    hrupest(:)  |none           |pesticide use flag:
!!                                | 0: no pesticides used in HRU
!!                                | 1: pesticides used in HRU
!!    hvsti(:)    |(kg/ha)/(kg/ha)|harvest index: crop yield/aboveground
!!                                |biomass
!!    icr(:)      |none           |sequence number of crop grown within the
!!                                |current year
!!    idc(:)      |none           |crop/landcover category:
!!                                |1 warm season annual legume
!!                                |2 cold season annual legume
!!                                |3 perennial legume
!!                                |4 warm season annual
!!                                |5 cold season annual
!!                                |6 perennial
!!                                |7 trees
!!    ihru        |none           |HRU number
!!    npmx        |none           |number of different pesticides used in
!!                                |the simulation
!!    plt_pst(:,:)|kg/ha          |pesticide on plant foliage
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
!!    wsyf(:)     |(kg/ha)/(kg/ha)|Value of harvest index between 0 and HVSTI
!!                                |which represents the lowest value expected
!!                                |due to water stress
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    plt_pst(:,:)|kg/ha          |pesticide on plant foliage
!!    rsr1c(:)    |               |initial root to shoot ratio at beg of growing season
!!    rsr2c(:)    |               |root to shoot ratio at end of growing season
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
!!    tnyld(:)    |kg N/kg yield  |modifier for autofertilization target
!!                                |nitrogen content for plant
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    clip        |kg/ha          |yield lost during harvesting
!!    clipn       |kg N/ha        |nitrogen in clippings
!!    clipp       |kg P/ha        |phosphorus in clippings
!!    clippst     |kg pst/ha      |pesticide in clippings
!!    hiad1       |none           |actual harvest index (adj for water/growth)
!!    j           |none           |HRU number
!!    k           |none           |counter
!!    wur         |none           |water deficiency factor
!!    yield       |kg             |yield (dry weight)
!!    yieldn      |kg N/ha        |nitrogen removed in yield
!!    yieldp      |kg P/ha        |phosphorus removed in yield
!!    yldpst      |kg pst/ha      |pesticide removed in yield
!!    xx          |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      
      integer :: j, k

!!   change per JGA 8/31/2011 gsm PUT YIELD IN modparm.f
!!    real :: hiad1, wur, yield, clip, yieldn, yieldp, clipn, clipp
      real :: hiad1, wur, clip, yieldn, yieldp, clipn, clipp
      real :: yldpst, clippst, rtresnew
      !!add by zhang
      !!===================
      real :: BLG1, BLG2, BLG3,  CLG, sf
      real :: sol_min_n, resnew, resnew_n, resnew_ne
      real :: LMF, LSF, LSLF, LSNF,LMNF 
      real ::  RLN, RLR

      orgc_f = 0.
      BLG1 = 0.
      BLG2 = 0.
      BLG3 = 0.
      CLG = 0.
      sf = 0.
      sol_min_n = 0.
      resnew = 0.
      resnew_n = 0.
      resnew_ne = 0.
      LMF = 0.
      LSF = 0.
      LSLF = 0.
      LSNF = 0.
      LMNF = 0.
      
      RLN = 0.
      RLR = 0.
      !!add by zhang
      !!===================

      j = ihru
            
      idp = pcom(j)%plcur(ipl)%idplt

      !pl_tot => hru(j)%pl_tot(ipl)
      !veg_ag => hru(j)%veg_ag(ipl)
      !grain => hru(j)%grain(ipl)
      !root => hru(j)%root(ipl)
      !rsd_flt => hru(j)%rsd_flt(ipl)
      
      ssb = 0.
      ssabg = 0.
      ssr = 0.
      ssn = 0.
      ssp = 0.
      ssb = pcom(j)%plm(ipl)%mass                       ! Armen 16 Jan 2009 storing info
      ssabg = pcom(j)%plm(ipl)%mass * (1.- pcom(j)%plg(ipl)%rwt)  ! Armen 16 Jan 2009 storing info
      ssr = ssb * pcom(j)%plg(ipl)%rwt                           ! Armen 16 Jan 2009 storing info
      ssn = pcom(j)%plm(ipl)%nmass                             ! Armen 20 May 2006 storing info
      ssp = pcom(ihru)%plm(ipl)%pmass                              ! Armen 20 May 2006 storing info
	
	
!! calculate modifier for autofertilization target nitrogen content
      tnyld(j) = 0.
      tnyld(j) = (1. - pcom(j)%plg(ipl)%rwt) * pcom(j)%plm(ipl)%mass *  
     &   pcom(j)%plm(ipl)%n_fr * auto_eff(j)

      hiad1 = 0.
      if (hi_ovr > 0.) then
        hiad1 = hi_ovr
      else
        if (pcom(j)%plg(ipl)%plet < 10.) then
          wur = 100.
        else
          wur = 0.
          wur = 100. * pcom(j)%plg(ipl)%plet / pcom(j)%plg(ipl)%plet
        endif
        hiad1 = (pcom(j)%plg(ipl)%hvstiadj - pldb(idp)%wsyf) *    
     &      (wur / (wur + Exp(6.13 - .0883 * wur))) +                   
     &      pldb(idp)%wsyf
        if (hiad1 > pldb(idp)%hvsti) then
          hiad1 = pldb(idp)%hvsti
        end if
      end if


!! check if yield is from above or below ground
      yield = 0.
      if (pldb(idp)%hvsti > 1.001) then
        yield = pcom(j)%plm(ipl)%mass * (1. - 1. / (1. + hiad1))
      else
        yield = (1.-pcom(j)%plg(ipl)%rwt)*pcom(j)%plm(ipl)%mass * hiad1
      endif
      if (yield < 0.) yield = 0.

!! determine clippings (biomass left behind) and update yield
      clip = 0.
      clip = yield * (1. - harveff)
      yield = yield * harveff
      if (yield < 0.) yield = 0.
      if (clip < 0.) clip = 0.

      if (hi_ovr > 0.) then
        !! calculate nutrients removed with yield
        yieldn = 0.
        yieldp = 0.
        yieldn = yield * pcom(j)%plm(ipl)%n_fr
        yieldp = yield * pcom(j)%plm(ipl)%p_fr
        yieldn = Min(yieldn, 0.80 * pcom(j)%plm(ipl)%nmass)    ! note Armen changed .80 for 0.9
        yieldp = Min(yieldp, 0.80 * pcom(ihru)%plm(ipl)%pmass) ! note Armen changed .80 for 0.9
        !! calculate nutrients removed with clippings
        clipn = 0.
        clipp = 0.
        clipn = clip * pcom(j)%plm(ipl)%n_fr
        clipp = clip * pcom(j)%plm(ipl)%p_fr
        clipn = Min(clipn,pcom(j)%plm(ipl)%nmass-yieldn)
        clipp = Min(clipp,pcom(ihru)%plm(ipl)%pmass-yieldp)
      else
        !! calculate nutrients removed with yield
        yieldn = 0.
        yieldp = 0.
        yieldn = yield * pldb(idp)%cnyld
        yieldp = yield * pldb(idp)%cpyld
        yieldn = Min(yieldn, 0.80 * pcom(j)%plm(ipl)%nmass) ! note Armen changed .80 for 0.9
        yieldp = Min(yieldp, 0.80 * pcom(ihru)%plm(ipl)%pmass) ! note Armen changed .80 for 0.9
        !! calculate nutrients removed with clippings
        clipn = 0.
        clipp = 0.
        clipn = clip * pldb(idp)%cnyld
        clipp = clip * pldb(idp)%cpyld
        clipn = Min(clipn,pcom(j)%plm(ipl)%nmass - yieldn)
        clipp = Min(clipp,pcom(ihru)%plm(ipl)%pmass - yieldp)
      endif

      yieldn = Max(yieldn,0.)
      yieldp = Max(yieldp,0.)
      clipn = Max(clipn,0.)
      clipp = Max(clipp,0.)

      !!add by zhang
      !!=====================

      if (bsn_cc%cswat == 2) then
        grainc_d(j) = grainc_d(j)+ yield * 0.42
        rsdc_d(j) = rsdc_d(j)+(clip+yield) * 0.42
      end if
      !!add by zhang
      !!=====================
      
            
      !! add clippings to residue and organic n and p
      hru(j)%rsd_flt(ipl)%mass = hru(j)%rsd_flt(ipl)%mass + clip
      hru(j)%rsd_flt(ipl)%nmass = clipn + hru(j)%rsd_flt(ipl)%nmass
      hru(j)%rsd_flt(ipl)%pmass = clipp + hru(j)%rsd_flt(ipl)%pmass

            !!insert new biomss by zhang
            !!===============================
            if (bsn_cc%cswat == 2) then
	          !!all the lignin from STD is assigned to LSL, 
	            !!add STDL calculation
	          !!
	          !sol_LSL(k,ihru) = sol_STDL(k,ihru)
	          !CLG=BLG(3,JJK)*HUI(JJK)/(HUI(JJK)+EXP(BLG(1,JJK)-BLG(2,JJK)*&HUI(JJK))
	          ! 52  BLG1 = LIGNIN FRACTION IN PLANT AT .5 MATURITY
                ! 53  BLG2 = LIGNIN FRACTION IN PLANT AT MATURITY
                !CROPCOM.dat BLG1 = 0.01 BLG2 = 0.10
                !SUBROUTINE ASCRV(X1,X2,X3,X4)
                !EPIC0810
                !THIS SUBPROGRAM COMPUTES S CURVE PARMS GIVEN 2 (X,Y) POINTS.
                !XX=LOG(X3/X1-X3)
                !X2=(XX-LOG(X4/X2-X4))/(X4-X3)
                !X1=XX+X3*X2
                !RETURN
                !END 
                !HUI(JJK)=HU(JJK)/XPHU               
                
                BLG1 = 0.01/0.10 !BLG1/BLG2
                BLG2 = 0.99
                BLG3 = 0.10 !BLG2
                !CALL ASCRV(BLG(1,I),BLG(2,I),.5,1.)
                XX = log(0.5/BLG1-0.5)
                BLG2 = (XX -log(1./BLG2-1.))/(1.-0.5)
                BLG1 = XX + 0.5*BLG2
                CLG=BLG3*pcom(j)%plcur(ipl)%phuacc/
     &              (pcom(j)%plcur(ipl)%phuacc + 
     &              EXP(BLG1-BLG2*pcom(j)%plcur(ipl)%phuacc))

	          !if (k == 1) then
		        sf = 0.05
	          !else
		        !sf = 0.1
	          !end if	

               !kg/ha  
	          sol_min_n = 0.	
	          sol_min_n = (soil(j)%nut(1)%no3 + soil(j)%nut(1)%nh3)
	          	          
	          resnew = clip 
	          resnew_n = clipn   	    
        	    resnew_ne = resnew_n + sf * sol_min_n
        	        !Not sure 1000 should be here or not!
        	    !RLN = 1000*(resnew * CLG/(resnew_n+1.E-5))
        	    RLN = (resnew * CLG/(resnew_n+1.E-5))
        	    !RLR is the fraction of lignin in the added residue
        	    RLR = MIN(.8, resnew * CLG/1000/(resnew/1000+1.E-5))
        	    !In most cases, lignin content in residue should be less than 30%
        	    !Therefore, RLR is expected to be less than 0.3
        	    !In the future, we may want to add a check make sure LMF is less than 1.0 - RLR.
        	    !this would help to avoid sol_LS becoming less than sol_LSL 
        	    
        	    LMF = 0.85 - 0.018 * RLN
        	    if (LMF <0.01) then
        	        LMF = 0.01
        	    else
        	        if (LMF >0.7) then
        	            LMF = 0.7
        	        end if
        	    end if      	  
	          !if ((resnew * CLG/(resnew_n+1.E-5)) < 47.22) then
		        !    LMF = 0.85 - 0.018 * (resnew * CLG/(resnew_n+1.E-5))
	          !else
		        !    LMF = 0.
	          !end if 	

	          LSF =  1 - LMF  
        	  
	          soil(j)%cbn(1)%lm = soil(j)%cbn(1)%lm + LMF * resnew
	          soil(j)%cbn(1)%ls = soil(j)%cbn(1)%ls + LSF * resnew
        	  
	          !In Jimmy's code, lignin added to sol_LSL is calculated as RLR*LSF*resnew
	          !However, I think we should use RLR*resnew; Confirmed with Jimmy
	          !sol_LSL(1,j) = sol_LSL(1,j) + RLR* LSF * resnew	
	          soil(j)%cbn(1)%lsl = soil(j)%cbn(1)%lsl + RLR*resnew
	                    
	          soil(j)%cbn(1)%lsc = soil(j)%cbn(1)%lsc + 0.42*LSF * resnew  
	          !In allignment with the sol_LSL calculation, sol_LSLC is also changed
	          !sol_LSLC(1,j) = sol_LSLC(1,j) + RLR*0.42*LSF * resnew
	          soil(j)%cbn(1)%lslc = soil(j)%cbn(1)%lslc + RLR*0.42+resnew
	          soil(j)%cbn(1)%lslnc=soil(j)%cbn(1)%lsc-soil(j)%cbn(1)%lslc 
                
                !X3 = MIN(X6,0.42*LSF * resnew/150)                 
	          if (resnew_ne >= (0.42 * LSF * resnew /150)) then
		         soil(j)%cbn(1)%lsn=soil(j)%cbn(1)%lsn+0.42*LSF*resnew/150
		         soil(j)%cbn(1)%lmn = soil(j)%cbn(1)%lmn + resnew_ne - 
     &                         (0.42 * LSF * resnew / 150) + 1.E-25
	          else
		         soil(j)%cbn(1)%lsn = soil(j)%cbn(1)%lsn + resnew_ne
		         soil(j)%cbn(1)%lmn = soil(j)%cbn(1)%lmn + 1.E-25
	          end if
        	
	          !LSNF = sol_LSN(1,j)/(sol_LS(1,j)+1.E-5)	
        	  
	          soil(j)%cbn(1)%lmc = soil(j)%cbn(1)%lmc + 0.42 * LMF * resnew
	          !LMNF = sol_LMN(1,j)/(sol_LM(1,j) + 1.E-5)           
                
                !update no3 and nh3 in soil
                soil(j)%nut(1)%no3 = soil(j)%nut(1)%no3 * (1-sf)
                soil(j)%nut(1)%nh3 = soil(j)%nut(1)%nh3 * (1-sf)
            end if
            !!insert new biomss by zhang
            !!=============================



	!! Calculation for dead roots allocations, resetting phenology, updating other pools

      ff3 = 0.
      if (ssabg > 1.e-6) then
        ff3 = (yield + clip) / ssabg	! Armen 20 May 2008 and 16 Jan 2009
      else
        ff3 = 1.
      endif 
      if (ff3 > 1.0) ff3 = 1.0


	! nssr is the new mass of roots
      nssr = pcom(j)%plg(ipl)%rwt * ssabg * (1. - ff3) /  
     &  (1. - pcom(j)%plg(ipl)%rwt)  
      rtresnew = ssr - nssr
      if (ssr > 1.e-6) then
       ff4 = rtresnew / ssr
      else
	   ff4 = 0.
      end if
      rtresn = ff4 * ssn
      rtresp = ff4 * ssp


      !! reset leaf area index and fraction of growing season
      if (ssb > 0.001) then
        pcom(j)%plg(ipl)%lai = pcom(j)%plg(ipl)%lai * (1. - ff3)
        if (pcom(j)%plg(ipl)%lai < pldb(idp)%alai_min) then   !Sue
          pcom(j)%plg(ipl)%lai = pldb(idp)%alai_min
        end if
       pcom(j)%plcur(ipl)%phuacc = pcom(j)%plcur(ipl)%phuacc*(1. - ff3) 
        pcom(j)%plg(ipl)%rwt = .4 - .2 * pcom(j)%plcur(ipl)%phuacc
      else
        pcom(j)%plm(ipl)%mass = 0.
        pcom(j)%plg(ipl)%lai = 0.
        pcom(j)%plcur(ipl)%phuacc = 0.
      endif

	!! remove n and p in harvested yield, clipped biomass, and dead roots 
      pcom(j)%plm(ipl)%mass = pcom(j)%plm(ipl)%mass - yield - 
     &    clip - rtresnew ! Armen 20May2008
      pcom(j)%plm(ipl)%nmass=pcom(j)%plm(ipl)%nmass-yieldn-clipn-rtresn
      pcom(ihru)%plm(ipl)%pmass = pcom(ihru)%plm(ipl)%pmass - yieldp -
     &    clipp - rtresp 
      if (pcom(j)%plm(ipl)%mass < 0.) pcom(j)%plm(ipl)%mass = 0.
      if (pcom(j)%plm(ipl)%nmass < 0.) pcom(j)%plm(ipl)%nmass = 0.
      if (pcom(ihru)%plm(ipl)%pmass < 0.) pcom(ihru)%plm(ipl)%pmass = 0.

      !! compute fraction of roots in each layer	! Armen 20 May 2008
      call pl_rootfr

      !! allocate roots, N, and P to soil pools	! Armen 20 May 2008
      do l = 1, hru(j)%sol%nly
       hru(j)%ly(l)%rsd=hru(j)%ly(l)%rsd+soil(j)%ly(l)%rtfr * rtresnew
       soil(j)%nut(l)%fon=soil(j)%nut(l)%fon+soil(j)%ly(l)%rtfr * rtresn
       soil(j)%nut(l)%fop=soil(j)%nut(l)%fop+soil(j)%ly(l)%rtfr * rtresp

             !!insert new biomss by zhang
             !!=============================
             if (bsn_cc%cswat == 2) then
	          !!all the lignin from STD is assigned to LSL, 
	            !!add STDL calculation
	          !!
	          !sol_LSL(k,ihru) = sol_STDL(k,ihru)
	          !CLG=BLG(3,JJK)*HUI(JJK)/(HUI(JJK)+EXP(BLG(1,JJK)-BLG(2,JJK)*&HUI(JJK))
	          ! 52  BLG1 = LIGNIN FRACTION IN PLANT AT .5 MATURITY
                ! 53  BLG2 = LIGNIN FRACTION IN PLANT AT MATURITY
                !CROPCOM.dat BLG1 = 0.01 BLG2 = 0.10
                !SUBROUTINE ASCRV(X1,X2,X3,X4)
                !EPIC0810
                !THIS SUBPROGRAM COMPUTES S CURVE PARMS GIVEN 2 (X,Y) POINTS.
                !XX=LOG(X3/X1-X3)
                !X2=(XX-LOG(X4/X2-X4))/(X4-X3)
                !X1=XX+X3*X2
                !RETURN
                !END 
                !HUI(JJK)=HU(JJK)/XPHU   
                
                rsdc_d(j) = rsdc_d(j)+soil(j)%ly(l)%rtfr*rtresnew * 0.42
                
                BLG3 = 0.10
                BLG1 = 0.01/0.10
                BLG2 = 0.99
                
                XX = log(0.5/BLG1-0.5)
                BLG2 = (XX -log(1./BLG2-1.))/(1.-0.5)
                BLG1 = XX + 0.5*BLG2
                CLG=BLG3*pcom(j)%plcur(ipl)%phuacc/
     &            (pcom(j)%plcur(ipl)%phuacc + 
     &            EXP(BLG1-BLG2*pcom(j)%plcur(ipl)%phuacc))


                !kg/ha  
	          sol_min_n = 0.	
	          sol_min_n = (soil(j)%nut(l)%no3 + soil(j)%nut(l)%nh3)
	          	          
	          resnew = soil(j)%ly(l)%rtfr * rtresnew
	          !resnew_n = resnew * pcom(j)%plm(ipl)%n_fr     	    
        	    !resnew_ne = resnew_n + sf * sol_min_n
        	    resnew_n = soil(j)%ly(l)%rtfr * rtresn
        	    resnew_ne = resnew_n + sf * sol_min_n
       	        !Not sure 1000 should be here or not!
        	    !RLN = 1000*(resnew * CLG/(resnew_n+1.E-5))
        	    RLN = (resnew * CLG/(resnew_n+1.E-5))
        	    RLR = MIN(.8, resnew * CLG/1000/(resnew/1000+1.E-5))
        	    
        	    LMF = 0.85 - 0.018 * RLN
        	    if (LMF <0.01) then
        	        LMF = 0.01
        	    else
        	        if (LMF >0.7) then
        	            LMF = 0.7
        	        end if
        	    end if      	  
	          !if ((resnew * CLG/(resnew_n+1.E-5)) < 47.22) then
		        !    LMF = 0.85 - 0.018 * (resnew * CLG/(resnew_n+1.E-5))
	          !else
		        !    LMF = 0.
	          !end if 	

	          LSF =  1 - LMF  
        	  
	          soil(j)%cbn(l)%lm = soil(j)%cbn(l)%lm + LMF * resnew
	          soil(j)%cbn(l)%ls = soil(j)%cbn(l)%ls + LSF * resnew

                
	          !here a simplified assumption of 0.5 LSL
	          LSLF = 0.0
	          LSLF = CLG          
	          
	          soil(j)%cbn(l)%lsl = soil(j)%cbn(l)%lsl + RLR* LSF * resnew
	          soil(j)%cbn(l)%lsc = soil(j)%cbn(l)%lsc + 0.42*LSF * resnew  
	          
	          soil(j)%cbn(l)%lslc=soil(j)%cbn(l)%lslc+RLR*0.42*LSF * resnew
	          soil(j)%cbn(l)%lslnc = soil(j)%cbn(l)%lsc - 
     &                     soil(j)%cbn(1)%lslc
                
               
	          if (resnew_ne >= (0.42 * LSF * resnew /150)) then
		         soil(j)%cbn(l)%lsn = soil(j)%cbn(l)%lsn + 0.42 * 
     &                         LSF * resnew/150.
		         soil(j)%cbn(l)%lmn = soil(j)%cbn(l)%lmn + resnew_ne - 
     &                         (0.42 * LSF * resnew / 150) + 1.E-25
	          else
		         soil(j)%cbn(l)%lsn = soil(j)%cbn(l)%lsn + resnew_ne
		         soil(j)%cbn(l)%lmn = soil(j)%cbn(l)%lmn + 1.E-25
	          end if	
	          
	          
	          
        	
	          !LSNF = sol_LSN(1,j)/(sol_LS(1,j)+1.E-5)	
        	  
	          soil(j)%cbn(l)%lmc = soil(j)%cbn(l)%lmc + 0.42 * LMF * resnew
	          !LMNF = sol_LMN(1,j)/(sol_LM(1,j) + 1.E-5)           
                
                !update no3 and nh3 in soil
                soil(j)%nut(1)%no3 = soil(j)%nut(1)%no3 * (1-sf)
                soil(j)%nut(1)%nh3 = soil(j)%nut(1)%nh3 * (1-sf)  
             end if        
             !!insert new biomass by zhang
             !!=============================
      end do

      rtfr = 0.         

	!! adjust foliar pesticide for plant removal
      if (hrupest(j) == 1) then
        ipestdb = hru(j)%dbs%pest_init
        npmx = pesti_db(ipestdb)%num
        do k = 1, npmx
          !! calculate amount of pesticide removed with yield and clippings
          yldpst = 0.
          clippst = 0.
          if (pldb(idp)%hvsti > 1.001) then
            yldpst = hru(j)%pst(k)%plt
            hru(j)%pst(k)%plt = 0.
          else
            yldpst = hiad1 * hru(j)%pst(k)%plt
            hru(j)%pst(k)%plt = hru(j)%pst(k)%plt - yldpst
            if (hru(j)%pst(k)%plt < 0.)hru(j)%pst(k)%plt = 0.
          endif
          clippst = yldpst * (1. - harveff)
          if (clippst < 0.) clippst = 0.
          !! add pesticide in clippings to soil surface
          hru(j)%ly(1)%pst(k) = hru(j)%ly(1)%pst(k) + clippst
          
        end do   
      end if

      return
      end subroutine mgt_harvestop