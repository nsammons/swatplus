      subroutine subbasin_control
      
!!    ~ ~ ~ PURPOSE ~ ~ ~

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!

      use parm
      
      character (len=3) :: ihtyp
      
      ob(icmd)%hd(1) = hz
      ob(icmd)%hd(2) = hz
      ob(icmd)%hd(3) = hz
      ob(icmd)%hd(4) = hz
      ob(icmd)%hd(5) = hz
      swb_d(isub) = hwbz
      snb_d(isub) = hnbz
      sls_d(isub) = hlsz
      spw_d(isub) = hpwz
      
      do ielem = 1, sub_d(isub)%num_tot
        ise = sub_d(isub)%num(ielem)
        iob = sub_elem(ise)%obj
        ihru = sub_elem(ise)%obtypno
        ihtyp = sub_elem(ise)%htyp
        ht1 = hz
        ht2 = hz
        ht3 = hz
        ht4 = hz
        ht5 = hz
        delrto = hz
        
        !define delivery ratio - all variables are hyd_output type
        idr = sub_elem(ise)%idr
        ihtypno = sub_elem(ise)%htypno
        if (idr > 0) then
          !input dr from sub_dr.dat
          delrto = sub_dr(idr)
          else
          !calculated dr = f(tconc element/ tconc sub)
          delrto = sub_elem(ielem)%dr(0)
        end if

        if (sub_elem(ielem)%obtyp == "exc") then
        !! compute hyds for export coefficients-ht1==surface,ht2==groundwater
          ht1 = exco(ob(iob)%props) ** sub_dr(sub_elem(ise)%idr)
          ht2 = hz
          if (ob(iob)%ha > .01) then
            !per area units - mm, t/ha, kg/ha (ie hru, apex)
            ef = sub_elem(ise)%frac * sub(isub)%da_km2 /
     &                                               (ob(iob)%ha / 100.)
            ht1 = ef * ht1
            !ht2 = exco(ob(iob)%props2) ** dr(sub_elem(ise)%idr)
          end if
          
        else

        select case (sub_elem(ielem)%obtyp)
        case ("hru")
        !define expansion factor to surface/soil and recharge
        ef = sub_elem(ise)%frac * sub(isub)%da_km2 / 
     &                                           (hru(ihru)%ha / 100.)
        case ("hlt")
        ef = sub_elem(ise)%frac * sub(isub)%da_km2 / sd_db(ihru)%dakm2
        end select
        
        !compute all hyd's needed for routing
        select case (ihtypno)
        case (1)   !total hyd
          ht1 = ob(iob)%hd(1) ** delrto
          ht1 = ef * ht1
          if (ob(iob)%typ == 1) then
            ht3 = ob(iob)%hd(3) ** delrto
            ht3 = ef * ht3
            ht4 = ob(iob)%hd(4) ** delrto
            ht4 = ef * ht4
            ht5 = ef * ob(iob)%hd(5)  !no dr on tile
          end if
        case (3)   !surface runoff hyd
          ht1 = ob(iob)%hd(ihtypno) ** delrto
          ht1 = ef * ht1
          ht3 = ob(iob)%hd(ihtypno) ** delrto
          ht3 = ef * ht3
        case (4)   !soil lateral flow hyd
          ht1 = ob(iob)%hd(ihtypno) ** delrto
          ht1 = ef * ht1
          ht4 = ob(iob)%hd(ihtypno) ** delrto
          ht4 = ef * ht4
        case (5)   !tile flow hyd
          ht1 = ef * ob(iob)%hd(ihtypno)
          ht5 = ef * ob(iob)%hd(ihtypno)  !no dr on tile
        end select

        !recharge hyd - hru_lte computes gw flow and doesnt need recharge hyd
        if (sub_elem(ielem)%obtyp /= "hlt") ht2 = ef * ob(iob)%hd(2)

        ! summing HRU output for the subbasin
        if (sub_elem(ise)%frac > 1.e-9) then
          const = 1. / sub_elem(ise)%frac   !only have / operator set up (could * frac_dfn directly)
          if (sub_elem(ise)%obtyp == "hru") then
            swb_d(isub) = swb_d(isub) + hwb_d(ihru) / const
            snb_d(isub) = snb_d(isub) + hnb_d(ihru) / const
            sls_d(isub) = sls_d(isub) + hls_d(ihru) / const
            spw_d(isub) = spw_d(isub) + hpw_d(ihru) / const
          end if
          ! summing HRU_LTE output
          if (sub_elem(ise)%obtyp == "hlt") then
            swb_d(isub) = swb_d(isub) + sdwb_d(ihru) / const
            snb_d(isub) = snb_d(isub) + sdnb_d(ihru) / const
            sls_d(isub) = sls_d(isub) + sdls_d(ihru) / const
            spw_d(isub) = spw_d(isub) + sdpw_d(ihru) / const
          end if
        end if
      end if      
                
      ob(icmd)%hd(1) = ob(icmd)%hd(1) + ht1             !sum total hyd 
      ob(icmd)%hd(2) = ob(icmd)%hd(2) + ht2             !sum recharge hyd
      ob(icmd)%hd(3) = ob(icmd)%hd(3) + ht3             !sum surfacce hyd 
      ob(icmd)%hd(4) = ob(icmd)%hd(4) + ht4             !sum lateral soil hyd
      ob(icmd)%hd(5) = ob(icmd)%hd(5) + ht5             !sum tile hyd 
          
      end do  !element loop
        
      ! summing subbasin output for the basin
      if (ob(icmd)%ha > 1.e-12 .and. bsn%ha > 1.e-12) then
        const = bsn%ha / ob(icmd)%ha        !only have / operator set up
        bwb_d = bwb_d + swb_d(isub) / const
        bnb_d = bnb_d + snb_d(isub) / const
        bls_d = bls_d + sls_d(isub) / const
        bpw_d = bpw_d + spw_d(isub) / const
      end if

       if (time%yrs > pco%nyskip .and. time%step /= 0) 
     &                                  call subbasin_output
      
	return

      end subroutine subbasin_control