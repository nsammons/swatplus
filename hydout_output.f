      subroutine hydout_output (iout)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs hyd variables on daily, monthly and annual time steps
      
      !!  0 = average annual (always print)
      !!  1 = yearly
      !!  2 = monthly
      !!  3 = daily  

!!!!! daily print
        if (pco%hyd == 3) then
          write (5001,101) time%day, time%yrs, icmd, ob(icmd)%typ, 
     &      ob(icmd)%props, ob(icmd)%obtyp_out(iout), 
     &      ob(icmd)%obtypno_out(iout), ob(icmd)%htyp_out(iout), 
     &      ob(icmd)%obj_out(iout), ht1
        end if
        ob(icmd)%hout_m(iout) = ob(icmd)%hout_m(iout) + ht1

!!!!! monthly print
        if (time%end_mo == 1) then
          if (pco%hyd == 2) then
            write (5001,101) time%day, time%yrs, icmd, ob(icmd)%typ, 
     &      ob(icmd)%props, ob(icmd)%obtyp_out(iout), 
     &      ob(icmd)%obtypno_out(iout), ob(icmd)%htyp_out(iout), 
     &      ob(icmd)%obj_out(iout), ob(icmd)%hout_m(iout)
          end if
          ob(icmd)%hout_y(iout) = ob(icmd)%hout_y(iout) + 
     &                                        ob(icmd)%hout_m(iout)
          ob(icmd)%hout_m(iout) = hz
        end if
        
!!!!! yearly print
        if (time%end_yr == 1) then
          if (pco%hyd == 1) then
            write (5001,101) time%day, time%yrs, icmd, ob(icmd)%typ, 
     &      ob(icmd)%props, ob(icmd)%obtyp_out(iout), 
     &      ob(icmd)%obtypno_out(iout), ob(icmd)%htyp_out(iout), 
     &      ob(icmd)%obj_out(iout), ob(icmd)%hout_y(iout)
          end if
          ob(icmd)%hout_a(iout) = ob(icmd)%hout_a(iout) + 
     &                                           ob(icmd)%hout_y(iout)
          ob(icmd)%hout_y(iout) = hz
        end if
        
!!!!! average annual print
        if (time%end_sim == 1 .and. pco%hyd == 0) then
          ob(icmd)%hout_a(iout) = ob(icmd)%hout_a(iout) / time%yrs_prt
          write (5001,100) ob(icmd)%name, time%day, time%yrs, icmd, 
     &      ob(icmd)%typ, ob(icmd)%props, ob(icmd)%obtyp_out(iout), 
     &      ob(icmd)%obtypno_out(iout), ob(icmd)%htyp_out(iout), 
     &      ob(icmd)%obj_out(iout), ob(icmd)%hout_a(iout)
        end if
        
100   format (a16,5i8,'    out ',2(a8,i8),a13,30(1x,e10.4))
101   format (5i8,'    out ',2(a8,i8),a13,30(1x,e10.4))
       
      end subroutine hydout_output