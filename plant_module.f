      module plant_module

      use parm
      use time_module
      use climate_parms
     
      contains
!! routines for plant module
      include 'pl_waterup.f'
      include 'pl_nupd.f'
      include 'pl_nup.f'
      include 'pl_pupd.f'
      include 'pl_pup.f'
      include 'pl_grow.f'
      include 'pl_nfix.f'
      include 'pl_tstr.f'
      include 'pl_anfert.f'
      include 'pl_irrigate.f'
      include 'pl_fert.f'
      include 'pl_burnop.f'
      include 'pl_confert.f'
      include 'pl_conapply.f'
      include 'pl_graze.f'
      include 'pl_apply.f'
      include 'pl_rootfr.f'

      subroutine plantmod

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine predicts daily potential growth of total plant
!!    biomass and roots and calculates leaf area index. Incorporates
!!    residue for tillage functions and decays residue on ground
!!    surface. Adjusts daily dry matter based on water stress.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
!!    phubase(:)  |heat units    |base zero total heat units (used when no
!!                               |land cover is growing)
!!    phutot(:)   |heat units    |total potential heat units for year (used
!!                               |when no crop is growing)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    phubase(:)  |heat units    |base zero total heat units (used when no
!!                               |land cover is growing)
!!    sol_cov(:)  |kg/ha         |amount of residue on soil surface
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: operatn, swu, grow

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      use jrw_datalib_module

      integer :: j

      j = ihru  
      par = 0.

      !! calculate residue on soil surface for current day
      sol_cov(j) = 0.
      do ipl = 1, npl(j)
        sol_cov(j) = sol_cov(j) + .8 * pcom(j)%plm(ipl)%mass
      end do
      sol_cov(j) = sol_cov(j) + soil(j)%ly(1)%rsd
      sol_cov(j) = Max(sol_cov(j),0.)

      !! compute plant water use and water stress
      !! compute actual plant transpiration
      ep_day = 0.
      !! partition to each plant in the community
      do ipl = 1, npl(j)
        if (sumlai > 1.e-6) then
          epmax(ipl) = ep_max * pcom(j)%plg(ipl)%lai / sumlai
        else
          epmax(ipl) = 0.
        end if
      end do

      npl_gro = 0
      do ipl = 1, npl(j)
        if (pcom(j)%plcur(ipl)%gro == 1) then
          call pl_waterup
          npl_gro = npl_gro + 1
          ip = ipl  !used for only one plant growing
        end if
      end do
 
      !! calculate photosynthetically active radiation during growth period
      if (npl_gro == 1) then
        !! calculate photosynthetically active radiation for one plant
        if (pcom(j)%plcur(ip)%idorm == 0 .and. pcom(j)%plcur(ip)%gro 
     &                                                         == 1)then
          idp = pcom(j)%plcur(ip)%idplt
          pl_db => pldb(idp)
          par(1) = .5 * hru_ra(j) * (1. - Exp(-pldb(idp)%ext_coef *     
     &          (pcom(j)%plg(ip)%lai + .05)))
        end if
      else if (npl_gro > 1) then
        !! calculate photosynthetically active radiation for multiple plants
        if (sumlai > 1.e-6) then
          translt = 0.
          do ipl = 1, npl(j)
            do jpl = 1, npl(j)
              x1 = pcom(j)%plg(jpl)%cht - .5 * pcom(j)%plg(ipl)%cht
              if (x1 > 0.) then
                idp = pcom(j)%plcur(ipl)%idplt
                pl_db => pldb(idp)
                translt(ipl) = translt(ipl) + x1/pcom(j)%plg(ipl)%cht * 
     &                    pcom(j)%plg(ipl)%lai * (-pldb(idp)%ext_coef)
              end if
            end do
          end do
          sum = 0.
          do ipl = 1,npl(j)
            translt(ipl) = exp(translt(ipl))
            sum = sum + translt(ipl)
          end do
          sumf = 0.
          sumle = 0.
          do ipl = 1, npl(j)
            idp = pcom(j)%plcur(ipl)%idplt
            translt(ipl) = translt(ipl) / sum
            x1 = pcom(j)%plg(ipl)%lai * pldb(idp)%ext_coef
            sumle = sumle + x1
            sumf = sumf + (1. - exp(-x1)) * translt(ipl)
          end do
          fi = 1. - exp(-sumle)
          do ipl = 1, npl(j)
            idp = pcom(j)%plcur(ipl)%idplt
            if (sumf > 0.) then
              htfac(ipl) = (1. - exp(-pldb(idp)%ext_coef *              
     &                     pcom(j)%plg(ipl)%lai)) * translt(ipl) / sumf
            else
              htfac(ipl) = 1.
            end if
            htfac(ipl) = fi * htfac(ipl)
            htfac(ipl) = 1.
            par(ipl) = .5 * htfac(ipl) * hru_ra(j) * (1. -              
     &        Exp(-pldb(idp)%ext_coef * (pcom(j)%plg(ipl)%lai + .05)))
          end do  
        end if
      end if
      
      uno3d(ipl) = 0.
      uno3d_tot = 0.
      uapd(ipl) = 0.
      uapd_tot = 0.
      do ipl = 1, npl(j)
        idp = pcom(j)%plcur(ipl)%idplt
        if (pcom(j)%plcur(ipl)%idorm == 0.and.pcom(j)%plcur(ipl)%gro==1)
     &                                                              then
        !! update accumulated heat units for the plant
        delg = 0.
        if (pcom(j)%plg(ipl)%phumat > 0.1) then
          delg = (tmpav(j) - pldb(idp)%t_base) / pcom(j)%plg(ipl)%phumat
        end if
        if (delg < 0.) delg = 0.
        pcom(j)%plcur(ipl)%phuacc = pcom(j)%plcur(ipl)%phuacc + delg  
        call pl_nupd
        call pl_pupd
        uno3d_tot = uno3d_tot + uno3d(ipl)
        uapd_tot = uapd_tot + uapd(ipl)
        end if
      end do
      sum_no3 = 0.
      sum_solp = 0.
      do nly = 1, hru(j)%sol%nly
        sum_no3 = sum_no3 + soil(j)%nut(nly)%no3
        sum_solp = sum_solp + soil(j)%nut(nly)%solp
      end do
     
      call pl_grow
      
      return
      
      end subroutine plantmod
      end module plant_module