      subroutine aqu_gwht (iaq, rchrg, gwht_prev, gwht)
      
!!    this subroutine lags simulates groundwater height by lagging recharge
!!    and assuming a specific yield of the aquifer (Arnold and Allen, 1995)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!         ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    iaq        |NA            |aquifer properties from aquifer.aqu
!!    gwht_prev  |m             |groundwater height on previous time step
!!    rchrg      |m^3           |recharge 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name              |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    gwht       |       |groundwater height
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    ~ ~ ~ MODULES USED ~ ~ ~
!!    jrw_datalib module
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module
      
      integer :: iaq
      real :: gwht_prev, rchrg, gwht
      
      gwht = gwht * aqudb(iaq)%alpha + rchrg * (1.-aqudb(iaq)%alpha)
     &   / (800. * aqudb(iaq)%spyld * aqudb(iaq)%alpha + 1.e-6)
      gwht = Max(1.e-6,gwht)

      return
      end subroutine aqu_gwht