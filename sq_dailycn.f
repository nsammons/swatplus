      subroutine sq_dailycn

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Calculates curve number for the day in the HRU 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
!!    sci(:)      |none          |retention coefficient for cn method based on 
!!                               |plant ET
!!    smx(:)      |none          |retention coefficient for cn method based on
!!                               |soil moisture
!!    wrt(1,:)    |none          |1st shape parameter for calculation of
!!                               |water retention
!!    wrt(2,:)    |none          |2nd shape parameter for calculation of
!!                               |water retention
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnday(:)    |none          |curve number for current day, HRU and at 
!!                               |current soil moisture
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    r2          |none          |retention parameter in CN equation
!!    xx          |none          |variable used to store intermediate
!!                               |calculation result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module

      integer :: j   

      real :: xx, r2

      j = ihru

      xx = wrt(1,j) - wrt(2,j) * hru(j)%sol%sw
      if (xx < -20.) xx = -20.
      if (xx > 20.) xx = 20.

      if (icn <= 0) then
        !! traditional CN method (function of soil water)
        if ((hru(j)%sol%sw + Exp(xx)) > 0.001) then
          r2 = bsn_prm%r2adj * smx(j) * (1.-hru(j)%sol%sw /             
     &                  (hru(j)%sol%sw+Exp(xx)))
        end if
      else                        
        !! alternative CN method (function of plant ET) 
        r2 = amax1(3., sci(j))           
      end if

      if (soil(j)%phys(2)%tmp <= 0.) r2=smx(j) * (1. -
     &          Exp(- bsn_prm%cn_froz * r2))
      r2 = amax1(3.,r2)

      cnday(j) = 25400. / (r2 + 254.)
      
      return
      end subroutine sq_dailycn