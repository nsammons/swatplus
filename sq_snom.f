      subroutine sq_snom
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine predicts daily snom melt when the average air
!!    temperature exceeds 0 degrees Celcius

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    iida         |julian date   |day being simulated (current julian date)
!!    ihru         |none          |HRU number
!!    precipday    |mm H2O        |precipitation on the current day in the HRU
!!    sno_hru(:)   |mm H2O        |amount of water in snow in HRU on current day
!!    snocov1      |none          |1st shape parameter for snow cover equation
!!                                |This parameter is determined by solving the
!!                                |equation for 50% snow cover
!!    snocov2      |none          |2nd shape parameter for snow cover equation
!!                                |This parameter is determined by solving the
!!                                |equation for 95% snow cover
!!    snotmp(:)    |deg C         |temperature of snow pack in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    precipday    |mm H2O        |amount of water in effective precipitation
!!                                |in HRU
!!    precipdt(:)  |mm H2O        |precipitation for the time step during day
!!    sno_hru(:)   |mm H2O        |amount of water in snow in HRU on current day
!!    snofall      |mm H2O        |amount of precipitation falling as freezing 
!!                                |rain/snow on day in HRU
!!    snomlt       |mm H2O        |amount of water in snow melt for the day in 
!!                                |HRU
!!    snotmp(:)    |deg C         |temperature of snow pack in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ib          |none          |counter
!!    j           |none          |HRU number
!!    smfac       |
!!    smleb       |mm H2O        |amount of snow melt in elevation band on 
!!                               |current day
!!    smp         |mm H2O        |precipitation on current day for HRU
!!    snocov      |none          |fraction of HRU area covered with snow
!!    sum         |mm H2O        |snow water content in HRU on current day
!!    xx          |none          |ratio of amount of current day's snow water
!!                               |content to the minimum amount needed to
!!                               |cover ground completely
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Real, Sin, Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      integer :: j, ib
      real :: sum, smp, smfac, smleb
      real :: xx, snocov 

      j = 0
      j = ihru
      sum = 0.
      smp = 0.
      isub = hru_sub(j)
      isno = hru(j)%dbs%snow
      
	ib = 1

        !! estimate snow pack temperature
        snotmp(j)=snotmp(j) * (1. - snodb(isno)%timp) + tmpav(j) *      
     &            snodb(isno)%timp

        if (tmpav(j) <= snodb(isno)%falltmp) then
          !! calculate snow fall
          sno_hru(j) = sno_hru(j) + precipday
          snofall = precipday
          precipday = 0.
          precipdt = 0.
        endif
 
        if (tmx(j) > snodb(isno)%melttmp .and. sno_hru(j) > 0.) then
          !! adjust melt factor for time of year
          smfac = 0.
          snomlt = 0.
          smfac = (snodb(isno)%meltmx + snodb(isno)%meltmn) / 2. +      
     &       Sin((iida - 81) / 58.09) *                                 
     &       (snodb(isno)%meltmx - snodb(isno)%meltmn) / 2.    !! 365/2pi = 58.09
          snomlt = smfac * (((snotmp(j)+tmx(j))/2.)-snodb(isno)%melttmp)

          !! adjust for areal extent of snow cover
          if (sno_hru(j) < snodb(isno)%covmx) then
            xx = 0.
            xx = sno_hru(j) / snodb(isno)%covmx
            snocov = xx / (xx + Exp(snocov1 - snocov2 * xx))
          else
            snocov = 1.
          endif
          snomlt = snomlt * snocov
          if (snomlt < 0.) snomlt = 0.
          if (snomlt > sno_hru(j)) snomlt = sno_hru(j)
          sno_hru(j) = sno_hru(j) - snomlt
          precipday = precipday + snomlt
          if (nstep > 0) then
            do ii = 1, nstep
             precipdt(ii+1) = precipdt(ii+1) + snomlt / nstep
            end do
          end if
          if (precipday < 0.) precipday = 0.
        else
          snomlt = 0.
        end if
 
      return
      end subroutine sq_snom


