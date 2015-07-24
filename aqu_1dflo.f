      subroutine aqu_1dflo (iaq, rchrg, flo_prev, stor_mm, flo)
      
!!    this subroutine calculates aquifer return flow using the lag (alpha)
!!    method of Arnold and Allen (1995)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!         ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    iaq        |NA            |aquifer properties from aquifer.aqu
!!    rchrg      |m^3           |recharge at current time step
!!    flo_prev   |m^3           |previous time step aquifer flow
!!    stor_mm    |mm            |water storage in aquifer at current time step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name              |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    flo          |m^3         |aquifer flow at current time step
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
      real :: rchrg, flo_prev, stor_mm, flo
      
      if (stor_mm > aqudb(iaq)%flo_min) then
        flo = flo_prev * (1. - aqudb(iaq)%alpha) + rchrg * 
     &                            aqudb(iaq)%alpha
      else
        flo = 0.
      endif

      return
      end subroutine aqu_1dflo