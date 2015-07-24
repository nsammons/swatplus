      subroutine aqu_no3 (iaq, percno3, rchrgn_prev, revap, seep, stor,
     & stor_no3, flow, flow3, revapno3, seepno3)
      
!!    this subroutine computes nitrate loadings from the aquifer and computes
!!    no3 balance

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!         ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    iaq         |NA            |aquifer properties from aquifer.aqu
!!    iaqu        |NA            |aquifer number
!!    perc_no3    |kg            |nitrate percolating pest bottom of soil profile
!!    revap       |m^3           |evaporation from the aquifer
!!    seep        |m^3           |seepage through the aquifer
!!    stor        |m^3           |water storage in aquifer
!!    stor_no3    |kg            |
!!    flo         |m^3           |flow from aquifer in current time step 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name              |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    flono3          |m^3         |
!!    revapno3        |kg          |
!!    seepno3         |kg          |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    conc_no3        |kg/m^3      |concentration of no3 in the aquifer
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    ~ ~ ~ MODULES USED ~ ~ ~
!!    jrw_datalib module
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module
      
      integer :: iaq
      real :: percno3, rchrgn_prev, revap, seep, stor, stor_no3, flow,
     &  flow3, revapno3, seepno3
      
      !! compute nitrate recharge into the aquifer
      rchrg_n = (1. - aqudb(iaq)%delay) * percno3 + aqudb(iaq)%delay *
     & rchrn_prev
      
      if (stor > 1.e-6) then
        conc_no3 = stor_no3 / stor
      else
        conc_no3 = 0.
      endif
      
      flono3 = conc * flo
      revapno3 = conc * revap
      seepno3 = conc * seep
      
      return
      end subroutine aqu_no3