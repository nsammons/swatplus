      subroutine hru_urban
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes loadings from urban areas using the
!!    USGS regression equations or a build-up/wash-off algorithm

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    al5          |none           |fraction of daily rainfall that occurs
!!                                 |during 0.5h highest intensity
!!    curbden(:)   |km/ha          |curb length density in HRU
!!    dirtmx(:)    |kg/curb km     |maximum amount of solids allowed to
!!                                 |build up on impervious surfaces
!!    fimp(:)      |fraction       |fraction of HRU area that is
!!                                 |impervious (both directly and
!!                                 |indirectly connected)
!!    hru_km(:)    |km^2           |area of HRU in square kilometers
!!    iida         |julian date    |day being simulated (current julian date)
!!    ihru         |none           |HRU number
!!    nsweep(:)    |none           |sequence number of street sweeping operation
!!                                 |within the year
!!    peakr        |m^3/s          |peak runoff rate
!!    precipday    |mm H2O         |precipitation for the day in HRU
!!    sedyld(:)    |metric tons    |daily soil loss caused by water erosio
!!    surfq(:)     |mm H2O         |surface runoff for the day in HRU
!!    tconc(:)     |hr             |time of concentration
!!    thalf(:)     |days           |time for the amount of solids on
!!                                 |impervious areas to build up to 1/2
!!                                 |the maximum level
!!    tnconc(:)    |mg N/kg sed    |concentration of total nitrogen in
!!                                 |suspended solid load from impervious
!!                                 |areas
!!    tno3conc(:)  |mg NO3-N/kg sed|concentration of NO3-N in suspended
!!                                 |solid load from impervious areas
!!    tpconc(:)    |mg P/kg sed    |concentration of total phosphorus in
!!                                 |suspended solid load from impervious
!!                                 |areas
!!    twash(:)     |days           |time that solids have built-up on streets
!!    urbcoef(:)   |1/mm           |wash-off coefficient for removal of
!!                                 |constituents from an impervious surface
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    twash(:)    |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cod         |kg            |carbonaceous biological oxygen demand of 
!!                               |surface runoff from urban area
!!    dirt        |kg/curb km    |amount of solids built up on impervious
!!                               |surfaces
!!    dirto       |kg/ha         |amount of solids built up on impervious
!!                               |surfaces at beginning of day
!!    durf        |hr            |duration of rainfall
!!    j           |none          |HRU number
!!    rp1         |none          |variable to hold intermediate calculation
!!                               |value
!!    sus_sol     |kg            |suspended solid loading in surface runoff
!!                               |from urban area
!!    tn          |kg            |total nitrogen in surface runoff from
!!                               |urban area
!!    tp          |kg            |total phosphorus in surface runoff from 
!!                               |urban area
!!    turo        |
!!    urbk        |1/hr          |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max, Log
!!    SWAT: Regres, sweep

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      real :: cod, sus_sol, tn, tp, urbk, turo, dirto, durf, rp1, dirt
      integer :: j

      j = 0
      j = ihru
      ilu = hru(j)%dbs%landuse
      ulu = hru(j)%luse%urb_lu

      select case (hru(j)%luse%iurban)

        case (1)                         !! USGS regression equations
        if (precipday > .1 .and. surfq(j) > .1) then
          cod = 0.
          sus_sol = 0.
          tn = 0.
          tp = 0.
          cod = Regres(1)
          sus_sol = Regres(2)
          tn = Regres(3)
          tp = Regres(4)
  
          sedyld(j) = (.001 * sus_sol) * urbdb(ulu)%fimp + sedyld(j)    
     &                                        * (1. - urbdb(ulu)%fimp)

          !! The sediment loading from urban imprevious area is assumed 
	    !! to be all sitly particles
          silyld(j) = (.001 * sus_sol) * urbdb(ulu)%fimp
     &                             + silyld(j) * (1. - urbdb(ulu)%fimp)
          sanyld(j) = sanyld(j) * (1. - urbdb(ulu)%fimp)
          clayld(j) = clayld(j) * (1. - urbdb(ulu)%fimp)
          sagyld(j) = sagyld(j) * (1. - urbdb(ulu)%fimp)
          lagyld(j) = lagyld(j) * (1. - urbdb(ulu)%fimp)

          sedorgn(j)=(.7 * tn / (hru(j)%km *100.))*urbdb(ulu)%fimp
     &                          + sedorgn(j) * (1. - urbdb(ulu)%fimp)
          surqno3(j)=(.3 * tn / (hru(j)%km *100.))*urbdb(ulu)%fimp
     &                          + surqno3(j) * (1. - urbdb(ulu)%fimp)
          sedorgp(j)=(.75 * tp / (hru(j)%km*100.))*urbdb(ulu)%fimp
     &                         +  sedorgp(j) * (1. - urbdb(ulu)%fimp)
          surqsolp(j)=.25 * tp / (hru(j)%km *100.)*urbdb(ulu)%fimp
     &                         + surqsolp(j) * (1. - urbdb(ulu)%fimp)
        endif

        case (2)                         !! build-up/wash-off algorithm

        if (surfq(j) > 0.1) then
          !! rainy day: no build-up, street cleaning allowed

          !! calculate amount of dirt on streets prior to wash-off
          dirt = 0.
          dirto = 0.
          dirt = urbdb(ulu)%dirtmx * twash(j) /                         
     &                                (urbdb(ulu)%thalf + twash(j))
          dirto = dirt

          !! calculate wash-off of solids
          urbk = 0.
          urbk = urbdb(ulu)%urbcoef * (peakr * 3.6 / hru(j)%km)
                                     !! expression in () peakr in mm/hr
          rp1 = 0.
          durf = 0.
          turo = 0.
          if(al5==0) al5 = 1e-6    !J.Jeong urban modeling
          rp1 = -2. * Log(1.- al5)
          durf = 4.605 / rp1         
          turo = durf + tconc(j)
          if (turo > 24.) turo = 24.
          xx = urbk * turo
          if (xx > 24.) xx = 24.
          dirt = dirt * Exp (-xx)
          if (dirt < 1.e-6) dirt = 0.0

          !! set time to correspond to lower amount of dirt
          twash(j) = 0.
          twash(j) = urbdb(ulu)%thalf * dirt / (urbdb(ulu)%dirtmx -dirt)

          sus_sol = 0.
          tn = 0.
          tp = 0.
          tno3 = 0.
          !! amounts are kg/ha
          sus_sol = Max(0., (dirto - dirt) * urbdb(ulu)%curbden)
          tn = urbdb(ulu)%tnconc * sus_sol / 1.e6
          tp = urbdb(ulu)%tpconc * sus_sol / 1.e6
          tno3 = urbdb(ulu)%tno3conc * sus_sol / 1.e6

          sedyld(j) = (.001 * sus_sol*hru(j)%ha)*urbdb(ulu)%fimp+
     &                              sedyld(j) * (1. - urbdb(ulu)%fimp)

          !! The sediment loading from urban imprevious area is assumed 
	    !! to be all sitly particles
          silyld(j) = (.001 * sus_sol * hru(j)%ha) *
     &                urbdb(ulu)%fimp + silyld(j) * (1. -
     &                                                urbdb(ulu)%fimp)
          sanyld(j) = sanyld(j) * (1. - urbdb(ulu)%fimp)
          clayld(j) = clayld(j) * (1. - urbdb(ulu)%fimp)
          sagyld(j) = sagyld(j) * (1. - urbdb(ulu)%fimp)
          lagyld(j) = lagyld(j) * (1. - urbdb(ulu)%fimp)

          surqno3(j) = tno3 * urbdb(ulu)%fimp + surqno3(j) *
     &                                        (1. - urbdb(ulu)%fimp)
          sedorgn(j) = (tn - tno3) * urbdb(ulu)%fimp + sedorgn(j) *     
     &                                        (1. - urbdb(ulu)%fimp)
          sedorgp(j) = .75 * tp * urbdb(ulu)%fimp + sedorgp(j)          
     &                                      * (1. - urbdb(ulu)%fimp)
          surqsolp(j) = .25 * tp * urbdb(ulu)%fimp + surqsolp(j) *      
     &                                        (1. - urbdb(ulu)%fimp)
        else
          !! dry day
          twash(j) = twash(j) + 1.

        end if

      end select

      return
      end subroutine hru_urban
