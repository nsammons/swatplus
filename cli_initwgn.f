      subroutine cli_initwgn(iwgn)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes the HRU weather generator parameters from the 
!!    .wgn file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i           |none          |HRU number
!!    ndays(:)    |julian date   |julian date for last day of preceding
!!                               |month (where the array location is the
!!                               |number of the month) The dates are for
!!                               |leap years
!!    rndseed(:,:)|none          |random number generator seeds
!!    rnmd1       |none          |random number between 0.0 and 1.0
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    daylmn(:)   |hr            |shortest daylength occurring during the year
!!    dewpt(:,:)  |deg C         |average dew point temperature for the month
!!    dormhr(:)   |hour          |time threshold used to define dormant
!!                               |period for plant (when daylength is within
!!                               |the time specified by dl from the minimum
!!                               |daylength for the area, the plant will go
!!                               |dormant)
!!    ireg(:)     |none          |precipitation category:
!!                               |  1 precipitation <= 508 mm/yr
!!                               |  2 precipitation > 508 and <= 1016 mm/yr
!!                               |  3 precipitation > 1016 mm/yr
!!    latcos(:)   |none          |Cos(Latitude)
!!    latsin(:)   |none          |Sin(Latitude)
!!    pcp_stat(:,1,:)|mm/day     |average amount of precipitation falling in
!!                               |one day for the month
!!    pcp_stat(:,2,:)|mm/day     |standard deviation for the average daily
!!                               |precipitation
!!    pcp_stat(:,3,:)|none       |skew coefficient for the average daily 
!!                               |precipitation
!!    phutot(:)   |heat unit     |total potential heat units for year (used
!!                               |when no crop is growing)
!!    pr_w(1,:,:) |none          |probability of wet day after dry day in month
!!    pr_w(2,:,:) |none          |probability of wet day after wet day in month
!!    pr_w(3,:,:) |none          |proportion of wet days in the month
!!    solarav(:,:)|MJ/m^2/day    |average daily solar radiation for the month
!!    tmp_an(:)   |deg C         |average annual air temperature
!!    tmpmn(:,:)  |deg C         |avg monthly minimum air temperature
!!    tmpmx(:,:)  |deg C         |avg monthly maximum air temperature
!!    tmpstdmn(:,:)|deg C        |standard deviation for avg monthly minimum air
!!                               |temperature
!!    tmpstdmx(:,:)|deg C        |standard deviation for avg monthly maximum air
!!                               |temperature
!!    welev(:)    |m             |elevation of weather station used to compile
!!                               |data
!!    wlat(:)     |degrees       |latitude of weather station used to compile
!!                               |data
!!    wndav(:,:) |m/s            |average wind speed for the month
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dl          |hour          |time threshold used to define dormant
!!                               |period for plant (when daylength is within
!!                               |the time specified by dl from the minimum
!!                               |daylength for the area, the plant will go
!!                               |dormant)
!!    j           |none          |counter
!!    irelh       |none          |irelh = 0 (dewpoint)
!!                               |      = 1 (relative humidity)
!!                               |note:  inputs > 1.0 (dewpoint)
!!                               |       inputs < 1.0 (relative hum)
!!    lattan      |none          |Tan(Latitude)
!!    m1          |none          |array location (see definition of ndays)
!!    mdays       |none          |number of days in the month
!!    mon         |none          |monthly counter
!!    nda         |julian date   |julian date of last day in the month
!!    pcp         |mm H2O        |generated precipitation
!!    pcpmm(:)    |mm            |amount of precipitation in month
!!    pcpd(:)     |days          |average number of days of precipitation
!!                               |in the month
!!    r6          |none          |variable to hold calculation result
!!    rainhhmx(:) |mm            |maximum 0.5 hour rainfall in month
!!                               |for entire period of record
!!    rain_hhsm(:)|mm            |smoothed values for maximum 0.5 hour rainfall
!!    rain_yrs    |none          |number of years of recorded maximum 0.5h 
!!                               |rainfall used to calculate values for 
!!                               |rainhhmx(:)
!!    rndm1       |none          |random number between 0.0 and 1.0
!!    rnm2        |none          |random number between 0.0 and 1.0
!!    sum         |none          |variable to hold summation results
!!    summm_p     |mm            |sum of precipitation over year
!!    summn_t     |deg C         |sum of mimimum temp values over year
!!    summx_t     |deg C         |sum of maximum temp values over year
!!    tav         |deg C         |average monthly temperature
!!    titldum     |NA            |title line of .wgn file (not used elsewhere)
!!    tmax        |deg C         |maximum average monthly temperature
!!    tmin        |deg C         |minimum average monthly temperature
!!    tmpsoil     |deg C         |initial temperature of soil layers
!!    x1          |none          |variable to hold calculation results
!!    x2          |none          |variable to hold calculation results
!!    x3          |none          |variable to hold calculation results
!!    xlv         |none          |variable to hold calculation results
!!    xx          |varies        |variable to hold calculation results
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sin, Cos, Tan, Abs, Acos, Log, Exp, MaxVal
!!    SWAT: Aunif, Dstn1

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module

      real :: xx, lattan, x1, x2, x3, tav, tmin, tmax
      real :: summx_t, summn_t, summm_p, sum, rnm2, r6, xlv, pcp
      real, dimension (12) :: rain_hhsm
      real :: tmpsoil, sffc, rndm1, dl
      integer :: mon, mdays, j, m1, nda, xrnd

      !! determine if input for dewpt is relative humidity
      i = iwgn
      do mon = 1,12
       if (wgn(i)%dewpt(mon) > 1.0  .or. wgn(i)%dewpt(mon) < 0.0) then
         irelh(i) = 0
       end if
      end do 

      !! variables needed for radiation calcs.
      xx = 0.0
      lattan = 0.0
      x1 = 0.0
      x2 = 0.0
      xx = wgn(i)%lat / 57.296 
                          !!convert degrees to radians (2pi/360=1/57.296)
      wgn_pms(i)%latsin = Sin(xx)
      wgn_pms(i)%latcos = Cos(xx)
      lattan = Tan(xx)
!! calculate minimum daylength 
!! daylength=2*acos(-tan(sd)*tan(lat))/omega
!! where solar declination, sd, = -23.5 degrees for minimum daylength in
!!                      northern hemisphere and -tan(sd) = .4348
!!       absolute value is taken of tan(lat) to convert southern hemisphere
!!                      values to northern hemisphere
!!       the angular velocity of the earth's rotation, omega, = 15 deg/hr or
!!                      0.2618 rad/hr and 2/0.2618 = 7.6394
      x1 = .4348 * Abs(lattan)      
      if (x1 < 1.) x2 = Acos(x1) 
      wgn_pms(i)%daylmn = 7.6394 * x2

!! calculate day length threshold for dormancy
      if (bsn_prm%dorm_hr < -1.e-6) then
        dl = 0.
         if (Abs(wgn(i)%lat) > 40.) then
          dl = 1.
         else if (Abs(wgn(i)%lat) < 20.) then
          dl = -1.
         else
         dl = (Abs(wgn(i)%lat) - 20.) / 20.
         end if
      else
         dl = bsn_prm%dorm_hr
      end if
      wgn_pms(i)%daylth = dl


!! calculate smoothed maximum 0.5hr rainfall amounts
      rain_hhsm = 0.
      rain_hhsm(1) = (wgn(i)%rainhmx(12) + wgn(i)%rainhmx(1) +          
     &                                  wgn(i)%rainhmx(2)) / 3.
      do mon = 2, 11
        rain_hhsm(mon) = (wgn(i)%rainhmx(mon-1) + wgn(i)%rainhmx(mon) + 
     &                     wgn(i)%rainhmx(mon+1)) / 3.
      end do
      rain_hhsm(12) = (wgn(i)%rainhmx(11) + wgn(i)%rainhmx(12) +        
     &                     wgn(i)%rainhmx(1)) / 3.


!! calculate missing values and additional parameters
      summx_t = 0.
      summn_t = 0.
      summm_p = 0.
      tmin = 100.
      tmax = 0.
      do mon = 1, 12
        mdays = 0
        tav = 0.
        mdays = ndays(mon+1) - ndays(mon)
        tav = (wgn(i)%tmpmx(mon) + wgn(i)%tmpmn(mon)) / 2.
        if (tav > tmax) tmax = tav
        if (tav < tmin) tmin = tav
        summx_t = summx_t + wgn(i)%tmpmx(mon)
        summn_t = summn_t + wgn(i)%tmpmn(mon)

        !! calculate total potential heat units
        if (tav > 0.)wgn_pms(i)%phutot = wgn_pms(i)%phutot + tav * mdays

        !! calculate values for pr_w if missing or bad
        if (wgn(i)%pr_ww(mon) <= wgn(i)%pr_wd(mon).or.
     &                                 wgn(i)%pr_wd(mon) <= 0.) then
          if (wgn(i)%pcpd(mon) < .1) wgn(i)%pcpd(mon) = 0.1
          wgn(i)%pr_wd(mon) = .75 * wgn(i)%pcpd(mon) / mdays
          wgn(i)%pr_ww(mon) = .25 + wgn(i)%pr_wd(mon)
        else
        !! if pr_w values good, use calculated pcpd based on these values
        !! using first order Markov chain
        wgn(i)%pcpd(mon) = mdays * wgn(i)%pr_wd(mon) /                  
     &                      (1. - wgn(i)%pr_ww(mon) + wgn(i)%pr_wd(mon))
    
        end if

        !! calculate precipitation-related values
        if (wgn(i)%pcpd(mon) <= 0.) wgn(i)%pcpd(mon) = .001
        wgn_pms(i)%pr_wdays(mon) = wgn(i)%pcpd(mon) / mdays
        wgn_pms(i)%pcpmean(mon) = wgn(i)%pcpmm(mon) / wgn(i)%pcpd(mon)
        if (wgn(i)%pcpskw(mon) < 0.2) wgn(i)%pcpskw(mon) = 0.2
        summm_p = summm_p + wgn(i)%pcpmm(mon)
        wgn_pms(i)%pcpdays = wgn_pms(i)%pcpdays + wgn(i)%pcpd(mon)
      end do

      wgn_pms(i)%pcp_an = summm_p
      wgn_pms(i)%tmp_an = (summx_t + summn_t) / 24.

      !! calculate initial temperature of soil layers
      if (time%idaf > ndays(2)) then
        do mon = 2, 12
          m1 = 0
          nda = 0
          m1 = mon + 1
          nda = ndays(m1) - 1
          if (time%idaf <= nda) exit
        end do
      else
        mon = 1
      end if

      xrnd = rndseed(idg(3),i)
      rndm1 = Aunif(xrnd)
      do mon = 1, 12
        !! calculate precipitation correction factor for pcp generator
        if (bsn_prm%rdist == 0) then
          r6 = 0.
          rnm2 = 0.
          xlv = 0.
          pcp = 0.
          sum = 0.
          r6 = wgn(i)%pcpskw(mon) / 6.
          do j = 1, 1000
            rnm2 = Aunif(xrnd)
            xlv = (cli_Dstn1(rndm1,rnm2) -r6) * r6 + 1
            rndm1 = rnm2
            xlv = (xlv**3 - 1.) * 2 / wgn(i)%pcpskw(mon)
            pcp = xlv * wgn(i)%pcpstd(mon) + wgn_pms(i)%pcpmean(mon)
            if (pcp < 0.01) pcp = 0.01
            sum = sum + pcp
          end do
          if (sum > 0.) then
            wgn_pms(i)%pcf(mon) = 1000. * wgn_pms(i)%pcpmean(mon) / sum
          else
            wgn_pms(i)%pcf(mon) = 1.
          end if
        end if

        !! calculate or estimate amp_r values
        x1 = 0.
        x2 = 0.
        x3 = 0.
        if (wgn(i)%rain_yrs < 1.0) wgn(i)%rain_yrs = 10.
        x1 = .5 / wgn(i)%rain_yrs 
        x2 = x1 / wgn(i)%pcpd(mon)
        x3 = rain_hhsm(mon) / Log(x2)
        if (wgn_pms(i)%pcpmean(mon) > 1.e-4) then
          wgn_pms(i)%amp_r(mon) = bsn_prm%adj_pkr * (1. - Exp(x3 /  
     &                                        wgn_pms(i)%pcpmean(mon)))
        else
          wgn_pms(i)%amp_r(mon) = 0.95
        end if
        if (wgn_pms(i)%amp_r(mon) < .1) wgn_pms(i)%amp_r(mon) = .1
        if (wgn_pms(i)%amp_r(mon) > .95) wgn_pms(i)%amp_r(mon) = .95
      end do

!!    determine precipitation category (ireg initialized to category 1)
      xx = 0
      xx = summm_p
      if (summm_p > 508.) wgn_pms(i)%ireg = 2
      if (summm_p > 1016.) wgn_pms(i)%ireg = 3

      return
      end subroutine cli_initwgn