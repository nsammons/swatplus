      subroutine cli_rhgen(iwgn)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine generates weather relative humidity, solar
!!    radiation, and wind speed.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    dewpt(:,:)  |deg C         |average dew point temperature for the month
!!    idg(:)      |none          |array location of random number seed used
!!                               |for a given process
!!    j           |none          |HRU number
!!    irelh       |none          |irelh = 0 (dewpoint)
!!                               |      = 1 (relative humidity)
!!                               |note:  inputs > 1.0 (dewpoint)
!!                                       inputs < 1.0 (relative humidity)
!!    i_mo        |none          |month being simulated
!!    pr_w(3,:,:) |none          |proportion of wet days in a month
!!    rndseed(:,:)|none          |random number seeds
!!    tmpmn(:,:)  |deg C         |avg monthly minimum air temperature
!!    tmpmx(:,:)  |deg C         |avg monthly maximum air temperature
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rhd(:)      |none          |relative humidity for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    blm         |none          |lowest relative humidity value allowed for
!!                               |any day in month
!!    rhm         |none          |mean monthly relative humidity adjusted for
!!                               |wet or dry condiditions
!!    rhmo        |none          |mean monthly relative humidity
!!    tmpmean     |deg C         |average temperature for the month in HRU
!!    uplm        |none          |highest relative humidity value allowed for
!!                               |any day in month
!!    vv          |none          |variable to hold intermediate calculation
!!    yy          |none          |variable to hold intermediate calculation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp
!!    SWAT: Atri, Ee

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      real :: vv, rhm, yy, uplm, blm
      real :: rhmo, tmpmean

      !! Climate Paramenters required for Penman-Monteith !!

      !! Generate relative humidity !!
      rhmo = 0.
      yy = 0.
      rhm = 0.
      vv = 0.
      uplm = 0.
      blm = 0.
      tmpmean = 0.
      tmpmean = (wgn(iwgn)%tmpmx(i_mo) +                       
     &                 wgn(iwgn)%tmpmn(i_mo)) / 2.

      !! dewpoint or relative humidity --
      if (irelh(iwgn) == 1) then 
        rhmo = wgn(iwgn)%dewpt(i_mo)
      else
        rhmo = Ee(wgn(iwgn)%dewpt(i_mo)) / Ee(tmpmean)
      endif

      yy = 0.9 * wgn_pms(iwgn)%pr_wdays(i_mo)
      rhm = (rhmo - yy) / (1.0 - yy)
      if (rhm < 0.05) rhm = 0.5 * rhmo
      if (wst(iwst)%weat%precip > 0.0) rhm = rhm * 0.1 + 0.9
      vv = rhm - 1.
      uplm = rhm - vv * Exp(vv)
      blm = rhm * (1.0 - Exp(-rhm))
      wst(iwst)%weat%rhum = Atri(blm,rhm,uplm,rndseed(idg(7),iwgn))

      return
      end subroutine cli_rhgen