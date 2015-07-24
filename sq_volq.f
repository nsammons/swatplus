      subroutine sq_volq

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Call subroutines to calculate the current day's CN for the HRU and
!!    to calculate surface runoff

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: surq_daycn, surq_breakcn, surq_greenampt, dir_rnff
!!    SWAT: surq_hourly

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use basin_module

!! Compute surface runoff for day
      select case (bsn_cc%event)
        case (0)
          call sq_daycn
        case default
          call sq_greenampt
        !  call dir_rnff
        !case (3)
        !  call surq_hourly
      end select


      return
      end subroutine sq_volq
