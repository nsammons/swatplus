      subroutine ch_rthr
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine routes flow at any required time step through the reach 
!!    using a constant storage coefficient  
!!	Routing method: Variable Storage routing   

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name            |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)         |m             |average depth of main channel
!!    ch_k(2,:)       |mm/hr         |effective hydraulic conductivity of
!!                                   |main channel alluvium
!!    ch_n(2,:)       |none          |Manning's "n" value for the main channel
!!    ch_s(2,:)       |m/m           |average slope of main channel
!!    chside(:)       |none          |change in horizontal distance per unit
!!                                   |change in vertical distance on channel
!!                                   |side slopes; always set to 2 (slope=1/2)
!!    inum1           |none          |reach number
!!    inum2           |none          |inflow hydrograph storage location number
!!    pet_day         |mm H2O        |potential evapotranspiration
!!    phi(1,:)        |m^2           |cross-sectional area of flow in channel at
!!                                   |bankfull depth
!!    phi(6,:)        |m             |bottom width of main channel
!!    rchstor(:)      |m^3 H2O       |water stored in reach
!!    rnum1           |none          |fraction of overland flow
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hdepth(:)   |m             |depth of flow during time step
!!    hharea(:)   |m^2           |cross-sectional area of flow
!!    hhstor(:)   |m^3 H2O       |water stored in reach at end of time step
!!    hhtime(:)   |hr            |flow travel time for time step
!!    hrchwtr(:)  |m^3 H2O       |water stored in reach at beginning of time step
!!    hrtevp(:)   |m^3 H2O       |evaporation losses for hour
!!    hrttlc(:)    |m^3 H2O       |transmission losses for hour
!!    hrtwtr(:)   |m^3 H2O       |water leaving reach during time step
!!    hsdti(:)    |m^3/s         |average flow rate during time step
!!    rchdep      |m             |depth of flow on day
!!    rchstor(:)  |m^3 H2O       |water stored in reach
!!    rhy(:)          |m H2O         |main channel hydraulic radius
!!    rtevp       |m^3 H2O       |evaporation from reach on day
!!    rttime      |hr            |reach travel time
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sdti        |m^3/s         |average flow on day in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    c           |none          |inverse of channel side slope
!!    ii          |none          |counter (hour)
!!    inhyd       |none          |inflow hydrograph storage location number
!!    jrch        |none          |reach number
!!    p           |m             |wetted perimeter
!!    scoef       |none          |storage coefficient
!!    nstep       |none          |No. of steps in a day (depends on model operational time step)
!!    topw        |m             |width of channel at water level
!!    vol         |m^3 H2O       |volume of water in reach
!!    wtrin       |m^3 H2O       |water entering reach during hour
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sum, Min, Sqrt
!!    SWAT: Qman

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!    subroutine developed by A. Van Griensven,
!!    Hydrology-Vrije Universiteit Brussel, Belgium
!!	Modified by N.Kannan, Blackland Research, Temple, USA

      use basin_module
      use climate_parms
      use jrw_datalib_module

      integer :: ii, inhyd
      real :: wtrin, c, p, scoef
      real :: vol, topw

      inhyd = 0
      inhyd = inum2

!	nstep = int(1440 / bsn_prm%dts) !! nstep is a global variable

!!     start of sub-daily loop

      do ii = 1, nstep

        !! water entering reach during time step

        wtrin = 0.
        wtrin = ts(k)%hh(inhyd)%flo * (1. - rnum1) 

        !! calculate volume of water in reach

        vol = 0.
        if (ii == 1) then
          hrchwtr(ii) = ch(jrch)%rchstor
          vol = wtrin + ch(jrch)%rchstor
        else
          hrchwtr(ii) = hhstor(ii-1)
          vol = wtrin + hhstor(ii-1)
        end if
        vol = Max(vol,1.e-14) ! changed from e-4 to e-14 for urban modeing by J.Jeong 4/21/2008

        !! calculate cross-sectional area of flow

        hharea(ii) = vol / (ch_hyd(jhyd)%l * 1000.)

        !! calculate depth of flow

        c = 0.
        c = ch_hyd(jhyd)%side
        if (hharea(ii) <= ch(jrch)%phi(1)) then
          hdepth(ii) = Sqrt(hharea(ii) / c + ch(jrch)%phi(6) *          
     &      ch(jrch)%phi(6) / (4. * c * c)) - ch(jrch)%phi(6) / (2. * c)
          if (hdepth(ii) < 0.) hdepth(ii) = 0.
        else
          hdepth(ii) = Sqrt((hharea(ii) - ch(jrch)%phi(1)) / 4. + 25. * 
     &        ch_hyd(jhyd)%w * ch_hyd(jhyd)%w / 64.) - 5. *
     &                                        ch_hyd(jhyd)%w / 8.
          if (hdepth(ii) < 0.) hdepth(ii) = 0.
          hdepth(ii) = hdepth(ii) + ch_hyd(jhyd)%d
        end if

        !! calculate wetted perimeter

        p = 0.
        if (hdepth(ii) <= ch_hyd(jhyd)%d) then
          p = ch(jrch)%phi(6) + 2. * hdepth(ii) * Sqrt(1. + c * c)
        else
          p = ch(jrch)%phi(6) + 2. * ch_hyd(jhyd)%d * Sqrt(1. + c * c) +
     &        4. * ch_hyd(jhyd)%w + 2. * (hdepth(ii) - ch_hyd(jhyd)%d) *
     &          Sqrt(17.)
        end if

        !! calculate hydraulic radius

        rhy(ii) = 0.
        if (p > 0.01) then
          rhy(ii) = hharea(ii) / p
        else
          rhy(ii) = 0.
        end if

        !! calculate rate of flow in reach

        hsdti(ii) = Qman(hharea(ii), rhy(ii), ch_hyd(jhyd)%n, 
     &                                                 ch_hyd(jhyd)%s)
       
        if (hsdti(ii) > 0.) then

          !! calculate travel time

          hhtime(ii) = ch_hyd(jhyd)%l * hharea(ii) / (3.6 * hsdti(ii))
          if (hhtime(ii) < 1.) then
            rttime = rttime + hhtime(ii)
          else
            rttime = rttime + 1.
          end if

          !! calculate volume of water leaving reach on day

          scoef = 0.
          scoef = 2. / (2. * hhtime(ii) + 1.)
          if (scoef > 1.) scoef = 1.
          hrtwtr(ii) = scoef * vol
          if (hrtwtr(ii) < 1.e-12) hrtwtr(ii) = 0.


          !! calculate transmission losses

          hrttlc = 0.
          if (hhtime(ii) < 1.) then
            hrttlc(ii)=ch_hyd(jhyd)%k * ch_hyd(jhyd)%l * p * hhtime(ii)
          else
            hrttlc(ii) = ch_hyd(jhyd)%k * ch_hyd(jhyd)%l * p
          end if
          hrttlc(ii) = Min(hrtwtr(ii),hrttlc(ii))
          hrtwtr(ii) = hrtwtr(ii) - hrttlc(ii)
          rttlc = rttlc + hrttlc(ii)

          hrtevp = 0.
          if (hrtwtr(ii) > 0.) then

            !! calculate width of channel at water level

            topw = 0.
            if (hdepth(ii) <= ch_hyd(jhyd)%d) then
              topw = ch(jrch)%phi(6) + 2. * hdepth(ii)*ch_hyd(jhyd)%side
            else
              topw = 5. * ch_hyd(jhyd)%w + 8. * (hdepth(ii) - 
     &                                               ch_hyd(jhyd)%d)
            end if

            !! calculate evaporation

            if (hhtime(ii) < 1.) then
              hrtevp(ii) = bsn_prm%evrch * pet_day/nstep * 
     &          ch_hyd(jhyd)%l * topw * hhtime(ii)
            else
              hrtevp(ii) = bsn_prm%evrch * pet_day/nstep * 
     &          ch_hyd(jhyd)%l * topw
            end if
            if (hrtevp(ii) < 0.) hrtevp(ii) = 0.
            hrtevp(ii) = Min(hrtwtr(ii),hrtevp(ii))
            hrtwtr(ii) = hrtwtr(ii) - hrtevp(ii)
            rtevp = rtevp + hrtevp(ii)
          end if

          !! set volume of water in channel at end of hour

          if (ii == 1) then
            hhstor(ii) = ch(jrch)%rchstor + wtrin - hrtwtr(ii) - 
     &       hrtevp(ii) - hrttlc(ii)
          else
            hhstor(ii) = hhstor(ii-1) + wtrin - hrtwtr(ii) - 
     &       hrtevp(ii) - hrttlc(ii)
          end if
          if (hhstor(ii) < 0.) then
            hrtwtr(ii) = hrtwtr(ii) + hhstor(ii)
            hhstor(ii) = 0.
            if (hrtwtr(ii) < 1.e-12) hrtwtr(ii) = 0.
          end if
        end if

      end do                     !! end of sub-daily loop

!! calculate amount of water in channel at end of day

!      if (hhstor(nstep) < 0.1.and.hrtwtr(ii-1)>0.) then
!        hrtwtr(nstep) = hrtwtr(nstep) + hhstor(nstep)
!        hhstor(nstep) = 0.
!      end if
      if (hrtwtr(nstep) < 0.) hrtwtr(nstep) = 0.
      
!! calculation of daily average values

      !! set volume of water in reach at end of day
      ch(jrch)%rchstor = hhstor(nstep)
      !! calculate total amount of water leaving reach
      rtwtr = Sum(hrtwtr)
      !! calculate average flow area
      rcharea = Sum (hharea) / nstep
      !! calculate average flow depth
      rchdep = Sum(hdepth) / nstep
      !! calculate average flow rate
      sdti = Sum(hsdti) / nstep

      return
      end subroutine ch_rthr