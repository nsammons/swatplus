      function chg_par (val_cur, chg)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function computes new paramter value based on 
!!    user defined change

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    val_cur     |variable      |current parameter value
!!                               |the standard temperature (20 degrees C)
!!    chg         |data type     |contains information on variable change
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chg_par     |variable      |new parameter value
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      real, intent (in) :: val_cur
      character(len=10) :: chg_typ  
      type (change_parms) :: chg
      real :: chg_par

      select case (chg%typ)

      case ("absval")
        chg_par = chg%val
     
      case ("abschg")
        chg_par = val_cur + chg%val

      case ("pctchg")
        chg_par = (1. + chg%val / 100.) * val_cur
      
      end select
      
      chg_par = amax1(chg_par,chg%absmin)
      chg_par = amin1(chg_par,chg%absmax)
      
      

      return
      end function chg_par