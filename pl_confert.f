      subroutine pl_confert
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates a continuous fertilizer operation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactkddb(:)  |none          |bacteria partition coefficient:
!!                                |1: all bacteria in solution
!!                                |0: all bacteria sorbed to soil particles
!!    bactlpdb(:)  |# bact/kg man |concentration of less persistent
!!                                |bacteria in manure(fertilizer)
!!    bactlpq(:)   |# colonies/ha |less persistent bacteria in soil solution
!!    bactlps(:)   |# colonies/ha |less persistent bacteria attached to soil
!!                                |particles
!!    bactpdb(:)   |# bact/kg man |concentration of persistent bacteria
!!                                |in manure(fertilizer)
!!    bactpq(:)    |# colonies/ha |persistent bacteria in soil solution
!!    bactps(:)    |# colonies/ha |persistent bacteria attached to soil particles
!!    cfrt_id(:)   |none          |manure (fertilizer) identification
!!                                |number from fert.dat
!!    cfrt_kg(:)   |(kg/ha)/day   |dry weight of fertilizer/manure deposited
!!                                |on HRU daily
!!    curyr        |none          |current year of simulation
!!    fminn(:)     |kg minN/kg frt|fraction of mineral N (NO3 + NH3) in 
!!                                |fertilizer/manure
!!    fminp(:)     |kg minP/kg frt|fraction of mineral P in fertilizer/manure
!!    fnh3n(:)     |kg NH3-N/kg minN|fraction of NH3-N in mineral N in 
!!                                |fertilizer/manure
!!    forgn(:)     |kg orgN/kg frt|fraction of organic N in fertilizer/manure
!!    forgp(:)     |kg orgP/kg frt|fraction of organic P in fertilizer/manure
!!    cfertn       |kg N/ha       |total amount of nitrogen applied to soil
!!                                |during continuous fertilizer operation in 
!!                                |HRU on day
!!    cfertp       |kg P/ha       |total amount of phosphorus applied to soil
!!                                |during continuous fertilizer operation in 
!!                                |HRU on day
!!    hru_dafr(:)  |km**2/km**2   |fraction of watershed area in HRU
!!    icfrt(:)     |none          |continuous fert flag for HRU:
!!                                |0 HRU currently not continuously fertilized
!!                                |1 HRU currently continuously fertilized
!!    iida         |julian date   |day being simulated (current julian day
!!    ihru         |none          |HRU number
!!    ncf(:)       |none          |sequence number of continuous fertilizer
!!                                |operation within the year
!!    ndcfrt(:)    |days          |number of days HRU has been continuously
!!                                |fertilized
!!    fert_days(:) |none          |number of days continuous fertilization
!!                                |will be simulated
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlpq(:)  |# colonies/ha |less persistent bacteria in soil solution
!!    bactlps(:)  |# colonies/ha |less persistent bacteria attached to soil
!!                               |particles
!!    bactpq(:)   |# colonies/ha |persistent bacteria in soil solution
!!    bactps(:)   |# colonies/ha |persistent bacteria attached to soil particles
!!    cfertn      |kg N/ha       |total amount of nitrogen applied to soil
!!                               |during continuous fertilizer operation in 
!!                               |HRU on day
!!    cfertp      |kg P/ha       |total amount of phosphorus applied to soil
!!                               |during continuous fertilizer operation in 
!!                               |HRU on day
!!    icfrt(:)    |none          |continuous fertilizer flag for HRU:
!!                               |0 HRU currently not continuously fertilized
!!                               |1 HRU currently continuously fertilized
!!    ifrt_freq(:)|days          |number of days between applications in 
!!                               |continuous fertlizer operation
!!    ncf(:)      |none          |sequence number of continuous fertilizer
!!                               |operation within the year
!!    ndcfrt(:)   |days          |number of days HRU has been continuously
!!                               |fertilized
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    frt_t       |
!!    gc          |
!!    gc1         |
!!    it          |none          |manure/fertilizer id number from fert.dat
!!    j           |none          |HRU number
!!    l           |none          |number of soil layer that manure is applied
!!    swf         |
!!    xx          |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: Erfc

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module
      use basin_module

      integer :: j, l, it
      real :: gc, gc1, swf, frt_t, xx
      j = 0
      j = ihru

!! if continuous fertilization not currently on, check to see if it is time
!! to initialize continuous fertilization
      
      if (iday_fert(j) == ifrt_freq(j)) then
        !! apply manure
        it = 0
        it = cfrt_id(j)
        if (cfrt_kg(j) > 0.) then
          l = 1
          if (bsn_cc%cswat == 0 .or. cswat == 1) then
          soil(j)%nut(l)%no3 = soil(j)%nut(l)%no3 + cfrt_kg(j) *
     &                 (1. - fertdb(it)%fnh3n) * fertdb(it)%fminn
          soil(j)%nut(l)%fon = soil(j)%nut(l)%fon + cfrt_kg(j) *
     &                 fertdb(it)%forgn
          soil(j)%nut(l)%nh3 = soil(j)%nut(l)%nh3 + cfrt_kg(j) *
     &                 fertdb(it)%fnh3n * fertdb(it)%fminn
          soil(j)%nut(l)%solp = soil(j)%nut(l)%solp + cfrt_kg(j) *
     &                 fertdb(it)%fminp
          soil(j)%nut(l)%fop = soil(j)%nut(l)%fop + cfrt_kg(j) *
     &                 fertdb(it)%forgp
          end if

          !!Add by zhang
          !!========================
          if (bsn_cc%cswat == 2) then
            soil(j)%nut(l)%fop = soil(j)%nut(l)%fop + cfrt_kg(j) *    
     &                 fertdb(it)%forgp
            soil(j)%nut(l)%no3 = soil(j)%nut(l)%no3 + cfrt_kg(j) *   
     &                 (1. - fertdb(it)%fnh3n) * fertdb(it)%fminn   
            soil(j)%nut(l)%nh3 = soil(j)%nut(l)%nh3 + cfrt_kg(j) *     
     &                 fertdb(it)%fnh3n * fertdb(it)%fminn 
            soil(j)%nut(l)%solp = soil(j)%nut(l)%solp + cfrt_kg(j) *   
     &                 fertdb(it)%fminp   
     
              orgc_f = 0.35
              !X1 fertilizer attributed to fresh carbon & nitrogen pool 
              X1 = cfrt_kg(j) 
              X8 = X1 * orgc_f
              RLN = .175 *(orgc_f)/(fertdb(it)%fminn + fertdb(it)%forgn 
     &                                                      + 1.e-5)
              X10 = .85-.018*RLN
              if (X10<0.01) then
                X10 = 0.01
              else
                if (X10 > .7) then
                    X10 = .7
                end if
              end if
              XXX = X8 * X10
              soil(j)%cbn(l)%lmc = soil(j)%cbn(l)%lmc + XXX
              YY = X1 * X10
              soil(j)%cbn(l)%lm = soil(j)%cbn(l)%lm + YY
              ZZ = X1 *fertdb(it)%forgn * X10
              soil(j)%cbn(l)%lmn = soil(j)%cbn(l)%lmn + ZZ
              soil(j)%cbn(l)%lsn = soil(j)%cbn(l)%lsn + X1
     &                      *fertdb(it)%forgn -ZZ
              XZ = X1 *orgc_f-XXX
              soil(j)%cbn(l)%lsc = soil(j)%cbn(l)%lsc + XZ
              soil(j)%cbn(l)%lslc = soil(j)%cbn(l)%lslc + XZ * .175
              soil(j)%cbn(l)%lslnc=soil(j)%cbn(l)%lslnc+XZ*(1.-.175) 
              YZ = X1 - YY
              soil(j)%cbn(l)%ls = soil(j)%cbn(l)%ls + YZ
              soil(j)%cbn(l)%lsl = soil(j)%cbn(l)%lsl + YZ*.175
              
               soil(j)%nut(l)%fon=soil(j)%cbn(l)%lmn+soil(j)%cbn(l)%lsn
          
          end if
          !!Add by zhang
          !!========================
        end if
        !! reset frequency counter
        iday_fert(j) = 1

        !! summary calculations
        cfertn = cfertn + cfrt_kg(j) *                                  
     &               (fertdb(it)%fminn + fertdb(it)%forgn)
        cfertp = cfertp + cfrt_kg(j) *                                  
     &               (fertdb(it)%fminp + fertdb(it)%forgp)
        tcfrtn(j) = tcfrtn(j) + cfertn
        tcfrtp(j) = tcfrtp(j) + cfertp
          
        if (pco%mout == 1) then
         write (143, 1000) j, time%yrc, i_mo, iida,
     *      "         ",
     *    "CONT FERT", phubase(j), pcom(j)%plcur(ipl)%phuacc,
     *      hru(j)%sol%sw, pcom(j)%plm(ipl)%mass,
     *      soil(j)%ly(1)%rsd,sol_sumno3(j),sol_sumsolp(j), cfrt_kg(j)
        end if
     
      else
        iday_fert(j) = iday_fert(j) + 1
      end if

!! check to set if continuous fertilizer period is over
      if (ndcfrt(j) == fert_days(j)) then
        icfrt(j) = 0
        ndcfrt(j) = 0
        iday_fert(j) = 0
        ncf(j) = ncf(j) + 1
      end if

1000  format (4i6,2a15,7f10.2,20x,f10.2)
      return
      end subroutine pl_confert