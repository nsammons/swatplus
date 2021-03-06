       subroutine ch_rtout
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine summarizes data for reaches

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units      |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ammonian(:)   |mg N/L     |ammonia concentration in reach
!!    bury          |mg pst     |loss of pesticide from active sediment layer
!!                              |by burial
!!    chlora(:)     |mg chl-a/L |chlorophyll-a concentration in reach
!!    difus         |mg pst     |diffusion of pesticide from sediment to reach
!!    disolvp(:)    |mg P/L     |dissolved phosphorus concentration in reach
!!    hbactlp(:)    |# cfu/100mL|less persistent bacteria in reach/outflow
!!                              |during hour
!!    hbactp(:)     |# cfu/100mL|persistent bacteria in reach/outflow during
!!                              |hour
!!    hbod(:)       |mg O2/L    |carbonaceous biochemical oxygen demand in
!!                              |reach at end of hour
!!    hchla(:)      |mg chl-a/L |chlorophyll-a concentration in reach at end of
!!                              |hour
!!    hdisox(:)     |mg O2/L    |dissolved oxygen concentration in reach at
!!                              |end of hour
!!    hnh4(:)       |mg N/L     |ammonia concentration in reach at end of hour
!!    hno2(:)       |mg N/L     |nitrite concentration in reach at end of hour
!!    hno3(:)       |mg N/L     |nitrate concentration in reach at end of hour
!!    horgn(:)      |mg N/L     |organic nitrogen concentration in reach at
!!                              |end of hour
!!    horgp(:)      |mg P/L     |organic phosphorus concentration in reach at
!!                              |end of hour
!!    hsedyld(:)    |metric tons|sediment transported out of reach during hour
!!    hsolp(:)      |mg P/L     |dissolved phosphorus concentration in reach at
!!                              |end of hour
!!    hsolpst(:)    |mg pst/m^3 |soluble pesticide concentration in outflow
!!                              |on day
!!    hsorpst(:)    |mg pst/m^3 |sorbed pesticide concentration in outflow
!!                              |on day
!!    hrtwtr(:)     |m^3 H2O    |water leaving reach during hour
!!    ihout         |none       |outflow hydrograph location
!!    inum1         |none       |reach number
!!    inum2         |none       |inflow hydrograph location
!!    nitraten(:)   |mg N/L     |nitrate concentration in reach
!!    nitriten(:)   |mg N/L     |nitrite concentration in reach
!!    organicn(:)   |mg N/L     |organic nitrogen concentration in reach
!!    organicp(:)   |mg P/L     |organic phosphorus concentration in reach
!!    rch_bactlp(:) |# cfu/100ml|less persistent bacteria in reach/outflow
!!                              |at end of day
!!    rch_bactp(:)  |# cfu/100ml|persistent bacteria in reach/outflow at end
!!                              |of day
!!    rch_cbod(:)   |mg O2/L    |carbonaceous biochemical oxygen demand in
!!                              |reach
!!    rch_dox(:)    |mg O2/L    |dissolved oxygen concentration in reach
!!    reactb        |mg pst     |amount of pesticide in sediment that is lost
!!                              |through reactions
!!    reactw        |mg pst     |amount of pesticide in reach that is lost
!!                              |through reactions
!!    resuspst      |mg pst     |amount of pesticide moving from sediment to
!!                              |reach due to resuspension
!!    rnum1         |none       |fraction of inflow that is overland flow
!!    rtevp         |m^3 H2O    |evaporation from reach on day
!!    rttlc         |m^3 H2O    |transmission losses from reach on day
!!    rtwtr         |m^3 H2O    |water leaving reach on day
!!    sedpst_act(:) |m          |depth of active sediment layer in reach for
!!                              |pesticide
!!    sedpst_conc(:)|mg/(m**3)  |inital pesticide concentration in river bed
!!                              |sediment
!!    sedrch        |metric tons|sediment transported out of channel
!!                              |during time step
!!    setlpst       |mg pst     |amount of pesticide moving from water to
!!                              |sediment due to settling
!!    solpesto      |mg pst/m^3 |soluble pesticide concentration in outflow
!!                              |on day
!!    sorpesto      |mg pst/m^3 |sorbed pesticide concentration in outflow
!!                              |on day
!!    volatpst      |mg pst     |amount of pesticide in reach lost by
!!                              |volatilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name             |units      |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bedvol      |m^3           |volume of river bed sediment
!!    ii          |none          |counter
!!    jrch        |none          |reach number
!!    sedcon      |mg/L          |sediment concentration in outflow
!!    sedpest     |mg pst        |pesticide in river bed sediment
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use jrw_datalib_module

      real :: sedcon, bedvol, sedpest, wtmp

      wtmp = 5.0 + 0.75 * wst(iwst)%weat%tave
!! set values for routing variables
      ob(icmd)%hd(1)%temp = wtmp
      ob(icmd)%hd(1)%flo = rtwtr
      ob(icmd)%hd(1)%sed = sedrch
      ob(icmd)%hd(1)%bacp = ch(jrch)%rch_bactp
      ob(icmd)%hd(1)%baclp = ch(jrch)%rch_bactlp
      ob(icmd)%hd(1)%met1 = ob(icmd)%hin%met1 * (1. - rnum1)
      ob(icmd)%hd(1)%met2 = ob(icmd)%hin%met2 * (1. - rnum1)
      ob(icmd)%hd(1)%met3 = ob(icmd)%hin%met3 * (1. - rnum1)
!!    sediment routing
      ob(icmd)%hd(1)%san = rch_san
      ob(icmd)%hd(1)%sil = rch_sil
      ob(icmd)%hd(1)%cla = rch_cla
      ob(icmd)%hd(1)%sag = rch_sag
      ob(icmd)%hd(1)%lag = rch_lag
      ob(icmd)%hd(1)%grv = rch_gra
      if (bsn_cc%event < 3) then
       ob(icmd)%hd(1)%orgn = ch(jrch)%organicn * rtwtr / 1000. +        
     &                                                    ch(jrch)%orgn
       ob(icmd)%hd(1)%sedp = ch(jrch)%organicp * rtwtr / 1000. +        
     &                                                    ch(jrch)%orgp
       ob(icmd)%hd(1)%no3 = ch(jrch)%nitraten * rtwtr / 1000.
       ob(icmd)%hd(1)%solp = ch(jrch)%disolvp * rtwtr / 1000.
       ob(icmd)%hd(1)%psol = solpesto * rtwtr
       ob(icmd)%hd(1)%psor = sorpesto * rtwtr
       ob(icmd)%hd(1)%chla = ch(jrch)%chlora * rtwtr / 1000.
       ob(icmd)%hd(1)%nh3 = ch(jrch)%ammonian * rtwtr / 1000.
       ob(icmd)%hd(1)%no2 = ch(jrch)%nitriten * rtwtr / 1000.
       ob(icmd)%hd(1)%cbod = ch(jrch)%rch_cbod *  rtwtr/ 1000.
       ob(icmd)%hd(1)%dox = ch(jrch)%rch_dox *  rtwtr/ 1000.
      else
       do ii = 1, nstep 
          ts(ii)%hh(ihout)%flo = hrtwtr(ii)   ! urban modeling by J.Jeong
          ts(ii)%hh(ihout)%sed = hsedyld(ii)  ! urban modeling by J.Jeong

	! From this point, check each variables if it is simulated at subdaily interval before using the output - Jaehak 9/11/09
          ts(ii)%hh(ihout)%temp = 0.
          ts(ii)%hh(ihout)%orgn = horgn(ii) * hrtwtr(ii) / 1000.
          ts(ii)%hh(ihout)%sedp = horgp(ii) *  hrtwtr(ii) / 1000.
          ts(ii)%hh(ihout)%no3 = hno3(ii) * hrtwtr(ii) / 1000.
          ts(ii)%hh(ihout)%solp = hsolp(ii) * hrtwtr(ii) / 1000.
          ts(ii)%hh(ihout)%psol = hsolpst(ii) * hrtwtr(ii)
          ts(ii)%hh(ihout)%psor = hsorpst(ii) * hrtwtr(ii)
          ts(ii)%hh(ihout)%chla = hchla(ii) * hrtwtr(ii) / 1000.
          ts(ii)%hh(ihout)%nh3 = hnh4(ii) * hrtwtr(ii) / 1000.
          ts(ii)%hh(ihout)%no2 = hno2(ii) * hrtwtr(ii) / 1000.
          ts(ii)%hh(ihout)%cbod = hbod(ii) *  hrtwtr(ii)/ 1000.
          ts(ii)%hh(ihout)%dox = hdisox(ii) *  hrtwtr(ii)/ 1000.
          ts(ii)%hh(ihout)%bacp = hbactp(ii)
          ts(ii)%hh(ihout)%baclp = hbactlp(ii)
          ts(ii)%hh(ihout)%met1 = ts(ii)%hh(inum2)%met1 * (1. - rnum1)
          ts(ii)%hh(ihout)%met2 = ts(ii)%hh(ihout)%met2 * (1. - rnum1)
          ts(ii)%hh(ihout)%met3 = ts(ii)%hh(ihout)%met3 * (1. - rnum1)

          ob(icmd)%hd(1)%orgn = ob(icmd)%hd(1)%orgn +                   
     $                                        ts(ii)%hh(ihout)%orgn
          ob(icmd)%hd(1)%sedp = ob(icmd)%hd(1)%sedp +                   
     &                                        ts(ii)%hh(ihout)%sedp
          ob(icmd)%hd(1)%no3 = ob(icmd)%hd(1)%no3 +                     
     &                                        ts(ii)%hh(ihout)%no3
          ob(icmd)%hd(1)%solp = ob(icmd)%hd(1)%solp +                   
     &                                        ts(ii)%hh(ihout)%solp
          ob(icmd)%hd(1)%psol = ob(icmd)%hd(1)%psol +                   
     &                                        ts(ii)%hh(ihout)%psol
          ob(icmd)%hd(1)%psor = ob(icmd)%hd(1)%psor +                   
     &                                        ts(ii)%hh(ihout)%psor
          ob(icmd)%hd(1)%chla = ob(icmd)%hd(1)%chla +                   
     &                                        ts(ii)%hh(ihout)%chla
          ob(icmd)%hd(1)%nh3 = ob(icmd)%hd(1)%nh3 +                     
     &                                        ts(ii)%hh(ihout)%nh3
          ob(icmd)%hd(1)%no2 = ob(icmd)%hd(1)%no2 +                     
     &                                        ts(ii)%hh(ihout)%no2
          ob(icmd)%hd(1)%cbod = ob(icmd)%hd(1)%cbod +                   
     &                                        ts(ii)%hh(ihout)%cbod
          ob(icmd)%hd(1)%dox = ob(icmd)%hd(1)%dox +                     
     &                                        ts(ii)%hh(ihout)%dox
        end do
      end if


!! set subdaily reach output    - by jaehak jeong for urban project, subdaily output in output.rch file
!	if (bsn_cc%event==3.and.pco%iprint==3) then
!	  do ii=1,nstep
!! determine sediment concentration in outflow
!          sedcon = 0.
!          if (hrtwtr(ii) > 0.01) then
!            sedcon = hsedyld(ii) / hrtwtr(ii) * 1.e6
!          else
!            sedcon = 0.
!          end if
!          rchhr(1,jrch,ii) = ts(ii)%hh(inum2)%flo * (1. - rnum1)!!flow in (m^3/s)
!     &      / (bsn_prm%dts * 60.)		       
!          rchhr(2,jrch,ii) = hrtwtr(ii) / (bsn_prm%dts * 60.)            !!flow out (m^3/s)
!          rchhr(3,jrch,ii) = hrtevp(ii) / (bsn_prm%dts * 60.)            !!evap (m^3/s)
!          rchhr(4,jrch,ii) = hrttlc(ii) / (bsn_prm%dts * 60.)            !!tloss (m^3/s)
!          rchhr(5,jrch,ii) =ts(ii)%hh(inum2)%sed * (1. - rnum1)   !!sed in (tons)
!          rchhr(6,jrch,ii) = hsedyld(ii)                         !!sed out (tons)
!          rchhr(7,jrch,ii) = sedcon						       !!sed conc (mg/L)
!	  end do
!	endif

!! determine sediment concentration in outflow
      sedcon = 0.
      if (rtwtr > 0.01) then
        sedcon = sedrch / rtwtr * 1.e6
      else
        sedcon = 0.
      end if
      if (sedcon > 200000.) sedcon = 200000.

!! determine amount of pesticide in river bed sediments
      bedvol = 0.
      sedpest = 0.
      bedvol = ch_hyd(jhyd)%w *ch_hyd(jhyd)%l * 1000.*
     &        ch_pst(jpst)%sedpst_act
      sedpest = ch_pst(jpst)%sedpst_conc * bedvol
      
      return
      end subroutine ch_rtout