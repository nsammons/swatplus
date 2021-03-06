      subroutine basin_output
      
      use time_module
      
      integer :: iz=0
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs BASIN variables on daily, monthly and annual time steps

!!    PRINT CODES: 0 = average annual (always print)
!!                 1 = yearly
!!                 2 = monthly
!!                 3 = daily
     
        !! sum monthly variables
        bwb_m = bwb_m + bwb_d
        bnb_m = bnb_m + bnb_d
        bls_m = bls_m + bls_d
        bpw_m = bpw_m + bpw_d
        
!!!!! daily print - SUBBASIN
        if (pco%wb_bsn == 3) then
          write (4300,100) time%day, time%yrc, iz, bwb_d  !! waterbal
        end if 
        if (pco%nb_bsn == 3) then
          write (4301,100) time%day, time%yrc, iz, bnb_d  !! nutrient bal
        end if
        if (pco%ls_bsn == 3) then
          write (4302,100) time%day, time%yrc, iz, bls_d  !! losses
        end if
        if (pco%pw_bsn == 3) then
          write (4303,100) time%day, time%yrc, iz, bpw_d  !! plant weather
        end if 

!!!!! monthly print - BASIN
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          bpw_m = bpw_m // const
          !bwb_m = bwb_m // const
          bwb_m%cn = bwb_m%cn / const 
          bwb_m%sw = bwb_m%sw / const
          bwb_m%pet_day = bwb_m%pet_day / const
          bwb_y = bwb_y + bwb_m
          bnb_y = bnb_y + bnb_m
          bls_y = bls_y + bls_m
          bpw_y = bpw_y + bpw_m
          if (pco%wb_bsn == 2) then
            write (4300,100) time%mo, time%yrc, iz, bwb_m
          end if
          if (pco%nb_bsn == 2) then 
            write (4301,100) time%mo, time%yrc, iz, bnb_m
          end if
          if (pco%ls_bsn == 2) then
            write (4302,100) time%mo, time%yrc, iz, bls_m
          end if
          if (pco%pw_bsn == 2) then
            write (4303,100) time%mo, time%yrc, iz, bpw_m
          end if
  
          bwb_m = hwbz
          bnb_m = hnbz
          bls_m = hlsz
          bpw_m = hpwz
        end if

!!!!! yearly print - BASIN
        if (time%end_yr == 1) then
           bpw_y = bpw_y // 12.
           !bwb_y = bwb_y // 12.
           bwb_y%cn = bwb_y%cn / 12. 
           bwb_y%sw = bwb_y%sw / 12.
           bwb_y%pet_day = bwb_y%pet_day / 12.
           bwb_a = bwb_a + bwb_y
           bnb_a = bnb_a + bnb_y
           bls_a = bls_a + bls_y
           bpw_a = bpw_a + bpw_y
           if (pco%wb_bsn == 1) then
             write (4300,102) '     0', time%yrc, iz, bwb_y
           end if
           if (pco%nb_bsn == 1) then
             write (4301,102) '     0', time%yrc, iz, bnb_y
           end if
           if (pco%ls_bsn == 1) then
             write (4302,102) '     0', time%yrc, iz, bls_y
           end if
           if (pco%pw_bsn == 1) then
             write (4303,102) '     0', time%yrc, iz, bpw_y
           end if
 
!!!!! zero yearly variables        
          bwb_y = hwbz
          bnb_y = hnbz
          bls_y = hlsz
          bpw_y = hpwz
        end if
        
!!!!! average annual print - BASIN
      if (time%end_sim == 1) then
        bwb_a = bwb_a / time%yrs_prt
        write (4304,102) '     0', time%yrs, iz, bwb_a
      end if
      if (time%end_sim == 1) then
        bnb_a = bnb_a / time%yrs_prt
        write (4305,102) '     0', time%yrs, iz, bnb_a
      end if
      if (time%end_sim == 1) then     
        bls_a = bls_a / time%yrs_prt
        write (4306,102) '     0', time%yrs, iz, bls_a
      end if
      if (time%end_sim == 1) then     
        bpw_a = bpw_a / time%yrs_prt
        write (4307,102) '     0', time%yrs, iz, bpw_a
      end if
      
100   format (2i6,i8,18f12.3)
102   format (a,i6,i8,18f12.3)
       
      end subroutine basin_output