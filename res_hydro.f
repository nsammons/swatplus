      subroutine res_hydro (jres, ihyd, ised)

      use jrw_datalib_module
      use reservoir_module
      use conditional_module
      !use hydrograph_module
      use climate_parms
      use time_module
      
      real :: vol, vvr, targ, xx, flw, ndespill
      real :: v_lo(2)

      !! store initial values
      vol = res(jres)%flo

      !! calculate surface area for day
      ressa = res_hyd(ihyd)%br1 * res(jres)%flo ** res_hyd(ihyd)%br2

      !! calculate water balance for day
      resev = 10. * res_hyd(ihyd)%evrsv * pet_day * ressa
      ressep = res_hyd(ihyd)%k * ressa * 240.
      respcp = respcp * ressa * 10.

      !! new water volume for day
      res(jres)%flo = res(jres)%flo + respcp + resflwi - resev - ressep

      !! if reservoir volume is less thanzero
      if (res(jres)%flo < 0.001) then

        !! if volume deficit in reservoir exists, reduce seepage so
        !! that reservoir volume is zero
        ressep = ressep + res(jres)%flo
        res(jres)%flo = 0.

        !! if seepage is less than volume deficit, take remainder
        !! from evaporation
        if (ressep < 0.) then
          resev = resev + ressep
          ressep = 0.
        end if

      else  !res volume > 0
          
      !! determine reservoir outflow
      irel = res_dat(res_ob(jres)%props)%release
      ihyd = res_dat(res_ob(jres)%props)%hyd
      do isets = 1, cond_db(irel)%num_rulesets
        iflag = 0
        do irules = 1, cond_db(irel)%ruleset(isets)%num_cond
          do iif = 1, 2
            select case 
     &         (cond_db(irel)%ruleset(isets)%cond_if(irules)%depend_var)
            case ("vol")
              select case 
     &         (cond_db(irel)%ruleset(isets)%cond_if(irules)%v_bnd(iif))
              case ("pvol")
                v_lo(iif) = res_hyd(ihyd)%pvol
              case ("evol")
                v_lo(iif) = res_hyd(ihyd)%evol
              end select
            case ("flo")
            case ("tim")
                v_lo(iif) = 1.
            end select
          end do
          b_lo = cond_db(irel)%ruleset(isets)%cond_if(irules)%c_lo *
     &                                                          v_lo(1)
          b_up = cond_db(irel)%ruleset(isets)%cond_if(irules)%c_up *
     &                                                          v_lo(2)
          if (res(jres)%flo >= b_lo .and. res(jres)%flo <= b_up)  then
            iflag = iflag + 1
          end if
        end do
        if (iflag == irules - 1) then
          !condition is met - set the release rate
          select case (cond_db(irel)%ruleset(isets)%typ)
          case ("rate")
            resflwo = cond_db(irel)%ruleset(isets)%con * 86400.
          case ("days")
            resflwo = (res(jres)%flo - b_lo) / 
     &                                 cond_db(irel)%ruleset(isets)%con
          case ("weir")
            !resflwo = 
          case ("meas")
            irel = int(cond_db(irel)%ruleset(isets)%con)
            select case (recall(irel)%typ)
            case (1)    !daily
              resflwo = recall(irel)%hd(time%day,time%yrs)%flo
            case (2)    !monthly
              resflwo = recall(irel)%hd(time%mo,time%yrs)%flo
            case (3)    !annual
              resflwo = recall(irel)%hd(1,time%yrs)%flo
            end select
          end select
          exit
        else
          !default rules
          select case (cond_db(irel)%ruleset(isets)%typ)
          case ("rate")
            resflwo = cond_db(irel)%ruleset(isets)%con * 86400.
          case ("days")
            resflwo = (res(jres)%flo - b_lo) / 
     &                                 cond_db(irel)%ruleset(isets)%con
          case ("weir")
            !resflwo = 
          case ("meas")
            irel = int(cond_db(irel)%ruleset(isets)%con)
            select case (recall(irel)%typ)
            case (1)    !daily
              resflwo = recall(irel)%hd(time%day,time%yrs)%flo
            case (2)    !monthly
              resflwo = recall(irel)%hd(time%mo,time%yrs)%flo
            case (3)    !annual
              resflwo = recall(irel)%hd(1,time%yrs)%flo
            end select
          end select
          exit
        end if
      end do

        !! subtract outflow from reservoir storage
        res(jres)%flo = res(jres)%flo - resflwo
        if (res(jres)%flo < 0.) then
          resflwo = resflwo + res(jres)%flo
          res(jres)%flo = 0.
        end if

!!    update surface area for day
      ressa = res_hyd(ihyd)%br1 * res(jres)%flo ** res_hyd(ihyd)%br2

      end if   !res volume < > 0.
      
      return
      end subroutine res_hydro