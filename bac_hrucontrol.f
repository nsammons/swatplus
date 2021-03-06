      subroutine bac_hrucontrol
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates bacteria growth, transport with runoff and
!!    loss due to percolation into soil 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    bactlpq(:)  |# cfu/m^2     |less persistent bacteria in soil solution
!!    bactlps(:)  |# cfu/m^2     |less persistent bacteria attached to soil
!!                               |particles

!!    bactpq(:)   |# cfu/m^2     |persistent bacteria in soil solution
!!    bactps(:)   |# cfu/m^2     |persistent bacteria attached to soil particles
!!    curyr       |none          |current year of simulation
!!    enratio     |none          |enrichment ratio calculated for current day 
!!                               |in HRU
!!    filterw(:)  |m             |filter strip width for bacteria transport
!!    hru_dafr(:) |none          |fraction of watershed area in HRU
!!    ihru        |none          |HRU number
!!    precipday   |mm H2O        |precipitation for the day in HRU
!!    surfq(:)    |mm H2O        |surface runoff generated on day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlpq(:)  |# cfu/m^2     |less persistent bacteria in soil solution
!!    bactlps(:)  |# cfu/m^2     |less persistent bacteria attached to soil
!!                               |particles
!!    bactpq(:)   |# cfu/m^2     |persistent bacteria in soil solution
!!    bactps(:)   |# cfu/m^2     |persistent bacteria attached to soil particles
!!    bactrolp    |# cfu/m^2     |less persistent bacteria transported to main
!!                               |channel with surface runoff
!!    bactrop     |# cfu/m^2     |persistent bacteria transported to main
!!                               |channel with surface runoff
!!    bactsedlp   |# cfu/m^2     |less persistent bacteria transported with 
!!                               |sediment in surface runoff
!!    bactsedp    |# cfu/m^2     |persistent bacteria transported with 
!!                               |sediment in surface runoff
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    blpq        |# cfu/m^2     |less persistent bacteria in soil solution at
!!                               |beginning of day
!!    blps        |# cfu/m^2     |less persistent bacteria attached to soil
!!                               |particles at beginning of day
!!    bpq         |# cfu/m^2     |persistent bacteria in soil solution at
!!                               |beginning of day
!!    bps         |# cfu/m^2     |persistent bacteria attached to soil particles
!!                               |at beginning of day
!!    cbact       |
!!    j           |none          |HRU number
!!    wt1         |none          |conversion factor to convert kg/ha to g/t(ppm)
!!    xx          |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min, Max
!!    SWAT: Theta

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use parm

      integer :: j
      real :: bacsol_out, bacsor_out

      j = ihru

      ibacdb = hru(j)%dbs%bact_init
      if (ibacdb == 0) return          
      do ib = 1, bact(ibacdb)%num
        ibtyp = bact(ibacdb)%bac(ib)%num_db
        if (hru(j)%ly(1)%bacsol(ib) < 1.e-6) hru(j)%ly(1)%bacsol(ib) =.0
        if (hru(j)%ly(1)%bacsor(ib) < 1.e-6) hru(j)%ly(1)%bacsor(ib) =.0
        if (pcom(j)%plg(1)%bac(ib) < 1.e-6) pcom(j)%plg(1)%bac(ib) = 0.0

        !! bacteria washoff, regrowth and dieoff in the soil layer and on the plant
        call bac_ls_process (ibtyp, pcom(j)%plg(1)%bac(ib), 
     &        hru(j)%ly(1)%bacsol(ib), hru(j)%ly(1)%bacsor(ib),
     &        precipday, tmpav(j), bacdiegrosol_out, bacdiegrosor_out, 
     &        bacdiegroplt_out)
        
        !! bacteria in the surface runoff and sediment transported
        if (qday > 0.) then
        call bac_ls_runoff (ibtyp, pcom(j)%plg(1)%bac(ib), 
     &        hru(j)%ly(1)%bacsol(ib), hru(j)%ly(1)%bacsor(ib),
     &        1.0, qday, soil(j)%phys(1)%bd, soil(j)%phys(1)%d, 
     &        sedyld(j), hru(j)%ha, bacsol_out, bacsor_out)
        end if

        !! bacteria leached through a soil layer 
        call bac_ls_swrouting (ibtyp, pcom(j)%plg(1)%bac(ib),
     &            hru(j)%ly(1)%prk, soil(j)%phys(1)%conv_wt, baclch_out)

      end do
      
      return
      end subroutine bac_hrucontrol