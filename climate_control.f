      subroutine climate_control
 
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls weather inputs to SWAT. Precipitation and
!!    temperature data is read in and the weather generator is called to 
!!    fill in radiation, wind speed and relative humidity as well as 
!!    missing precipitation and temperatures. Adjustments for climate
!!    changes studies are also made in this subroutine.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    elevp(:)    |m             |elevation of precipitation gage station
!!    elevt(:)    |m             |elevation of temperature gage station
!!    hru_sub(:)  |none          |subbasin in which HRU is located
!!    ifirstpet   |none          |potential ET data search code
!!                               |0 first day of potential ET data located in
!!                               |  file
!!                               |1 first day of potential ET data not located
!!                               |  in file
!!    i_mo        |none          |current month of simulation
!!    nhru        |none          |number of HRUs in watershed
!!    nstep       |none          |number of lines of rainfall data for each
!!                               |day
!!    welev(:)    |m             |elevation of weather station used to compile
!!                               |weather generator data
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    frad(:,:)   |none          |fraction of solar radiation occuring during
!!                               |hour in day in HRU
!!    hru_ra(:)   |MJ/m^2        |solar radiation for the day in HRU
!!    hru_rmx(:)  |MJ/m^2        |maximum solar radiation for the day in HRU
!!    ifirstpet   |none          |potential ET data search code
!!                               |0 first day of potential ET data located in
!!                               |  file
!!                               |1 first day of potential ET data not located
!!                               |  in file
!!    petmeas     |mm H2O        |potential ET value read in for day
!!    rainsub(:,:)|mm H2O        |precipitation for the time step during the
!!                               |day in HRU
!!    rhd(:)      |none          |relative humidity for the day in HRU
!!    u10(:)      |m/s           |wind speed for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    fradbsb(:)  |none          |hourly solar radiation fractions for subbasin
!!    ib          |none          |counter
!!    idap        |julain date   |day currently being simulated
!!    ii          |none          |counter
!!    inum3sprev  |none          |subbasin number of previous HRU
!!    iyp         |none          |year currently being simulated
!!    k           |none          |counter
!!    pdif        |mm H2O        |difference in precipitation for station and
!!                               |precipitation for elevation band
!!    rabsb       |MJ/m^2        |generated solar radiation for subbasin
!!    ratio       |none          |fraction change in precipitation due to 
!!                               |elevation changes
!!    rbsb        |mm H2O        |generated precipitation for subbasin
!!    rhdbsb      |none          |generated relative humidity for subbasin
!!    rmxbsb      |MJ/m^2        |generated maximum solar radiation for subbasin
!!    tdif        |deg C         |difference in temperature for station and
!!                               |temperature for elevation band
!!    tmnbsb      |deg C         |generated minimum temperature for subbasin
!!    tmxbsb      |deg C         |generated maximum temperature for subbasin
!!    u10bsb      |m/s           |generated wind speed for subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max, Min
!!    SWAT: pmeas, tmeas, smeas, hmeas, wmeas
!!    SWAT: pgen, tgen, weatgn, clgen, slrgen, rhgen, wndgen

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use climate_parms
      use basin_module

      integer :: k, inum3sprev, npcpbsb, ii, iyp, idap, ib
      real :: tmxbsb, tmnbsb, rbsb, rhdbsb, rabsb, u10bsb, rmxbsb
      real :: daylbsb,  fradbsb(nstep),tdif, pdif, ratio
      
      !! Precipitation:
      do ii = 1, mwst
        iwst = wst_pointer(ii)
        iwgn = wst(iwst)%wco%wgn
        if (wst(iwst)%wco%pcpsim == 2) then
          call cli_pgen(iwgn)
        else
          ig = wst(iwst)%wco%pgage
          wst(iwst)%weat%precip = pcp(ig)%ts(time%day,time%yrs)
          if (wst(iwst)%weat%precip <= -97.) then
           call cli_pgen(iwgn)
          end if
        end if
      end do
      
!! Temperature: 
      do ii = 1, mwst
        iwst = wst_pointer(ii)
        iwgn = wst(iwst)%wco%wgn
        call cli_weatgn(iwgn)
        if (wst(iwst)%wco%tmpsim == 2) then
          call cli_tgen(iwgn)
        else
          ig = wst(iwst)%wco%tgage
          wst(iwst)%weat%tmax = tmp(ig)%ts(time%day,time%yrs)
          wst(iwst)%weat%tmin = tmp(ig)%ts2(time%day,time%yrs)
          if (wst(iwst)%weat%tmax <= -97.) then
            call cli_weatgn(iwgn)
            call cli_tgen(iwgn)
          end if
        end if
        wst(iwst)%weat%tave = (wst(iwst)%weat%tmax + 
     &                                        wst(iwst)%weat%tmin) / 2.
      end do

!! Solar Radiation: 
      do ii = 1, mwst
        iwst = wst_pointer(ii)
        iwgn = wst(iwst)%wco%wgn
        call cli_clgen(iwgn)
        if (wst(iwst)%wco%slrsim == 2) then
          call cli_slrgen(iwgn)
        else
          ig = wst(iwst)%wco%sgage
          wst(iwst)%weat%solrad = slr(ig)%ts(time%day,time%yrs)
          if (wst(iwst)%weat%solrad <= -97.) then
            call cli_slrgen(iwgn)
          end if
        end if
      end do
        
!! Relative Humidity: 
      do ii = 1, mwst
        iwst = wst_pointer(ii)
        iwgn = wst(iwst)%wco%wgn
        if (wst(iwst)%wco%rhsim == 2) then
          call cli_rhgen(iwgn)
        else
          ig = wst(iwst)%wco%hgage
          wst(iwst)%weat%rhum = hmd(ig)%ts(time%day,time%yrs)
          if (wst(iwst)%weat%rhum <= -97.) then
            call cli_rhgen(iwgn)
          end if
        end if
      end do 

!! Wind Speed: 
      do ii = 1, mwst
        iwst = wst_pointer(ii)
        iwgn = wst(iwst)%wco%wgn
        if (wst(iwst)%wco%wndsim == 2) then
          call cli_wndgen(iwgn)
        else
          ig = wst(iwst)%wco%wgage
          wst(iwst)%weat%windsp = wnd(ig)%ts(time%day,time%yrs)
          if (wst(iwst)%weat%windsp <= -97.) then
            call cli_wndgen(iwgn)
          end if
        end if
      end do 

!! Potential ET: Read in data !!
      if (bsn_cc%pet == 3) then
        if (ifirstpet == 0) then
          read (140,5100) petmeas
        else
          ifirstpet = 0
          do 
            iyp = 0
            idap = 0
            read (140,5000) iyp, idap, petmeas
            if (iyp == time%yrc .and. idap == time%idaf) exit
          end do
        end if
        do ii = 1, mwst
          iwst = wst_pointer(ii)
          wst(iwst)%weat%pet = petmeas
        end do
      else
        !! HARGREAVES POTENTIAL EVAPOTRANSPIRATION METHOD
        !! extraterrestrial radiation
        !! 37.59 is coefficient in equation 2.2.6 !!extraterrestrial
        !! 30.00 is coefficient in equation 2.2.7 !!max at surface
        do ii = 1, mwst
          iwst = wst_pointer(ii)
          ramm = wst(iwst)%weat%solradmx * 37.59 / 30. 
          if (wst(iwst)%weat%tmax > wst(iwst)%weat%tmin) then
            xl = 2.501 - 2.361e-3 * wst(iwst)%weat%tave
            wst(iwst)%weat%pet = .0023*(ramm / xl)*(wst(iwst)%weat%tave 
     &        + 17.8) * (wst(iwst)%weat%tmax - wst(iwst)%weat%tmin)**0.5
            wst(iwst)%weat%pet = Max(0., wst(iwst)%weat%pet)
          else
            wst(iwst)%weat%pet = 0.
          endif
        end do
      end if

!! Climate Change Adjustments !!
      do iip = 1, mwst
        iwst = wst_pointer(iip)
        wst(iwst)%weat%precip = wst(iwst)%weat%precip * (1. +           
     &                                     wst(iwst)%rfinc(i_mo) / 100.)
        if (wst(iwst)%weat%precip < 0.) wst(iwst)%weat%precip = 0.
        if (nstep > 0) then
          do ii = 1, nstep
            rainsub(iwst,ii) = rainsub(iwst,ii) *                       
     &                              (1. + wst(iwst)%rfinc(i_mo) / 100.)
            if (rainsub(iwst,ii) < 0.) rainsub(iwst,ii) = 0.
          end do
        end if
        wst(iwst)%weat%tmax = wst(iwst)%weat%tmax +                     
     &                                            wst(iwst)%tmpinc(i_mo)
        wst(iwst)%weat%tmin = wst(iwst)%weat%tmin +                     
     &                                            wst(iwst)%tmpinc(i_mo)
        wst(iwst)%weat%solrad = wst(iwst)%weat%solrad +                 
     &                                            wst(iwst)%radinc(i_mo)
        wst(iwst)%weat%solrad = Max(0.,wst(iwst)%weat%solrad)
        wst(iwst)%weat%rhum = wst(iwst)%weat%rhum +                     
     &                                            wst(iwst)%huminc(i_mo)
        wst(iwst)%weat%rhum = Max(0.01,wst(iwst)%weat%rhum)
        wst(iwst)%weat%rhum = Min(0.99,wst(iwst)%weat%rhum)
      end do

      return
 5000 format (i4,i3,f5.1)
 5100 format (7x,f5.1)

      end subroutine climate_control