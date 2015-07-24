      subroutine hyddep_output
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs hyd variables on daily, monthly and annual time steps
      
      !!  0 = average annual (always print)
      !!  1 = yearly
      !!  2 = monthly
      !!  3 = daily  

!!!!! daily print
      if (pco%hyd == 3) then
          write (5005,101) time%day, time%yrs, icmd, ob(icmd)%typ, 
     &      ob(icmd)%props, ht1
      endif
        ob(icmd)%hdep_m = ob(icmd)%hdep_m + ht1

!!!!! monthly print
      if (time%end_mo == 1) then
        if (pco%hyd == 2) then
            write (5005,101) time%day, time%yrs, icmd, ob(icmd)%typ, 
     &      ob(icmd)%props, ob(icmd)%hdep_m
        end if
          ob(icmd)%hdep_y = ob(icmd)%hdep_y + ob(icmd)%hdep_m
          ob(icmd)%hdep_m = hz
      endif
        
!!!!! yearly print
      if (time%end_yr == 1) then
        if (pco%hyd == 1) then
            write (5005,101) time%day, time%yrs, icmd, ob(icmd)%typ, 
     &      ob(icmd)%props, ob(icmd)%hin_y
        end if
          ob(icmd)%hdep_a = ob(icmd)%hdep_a + ob(icmd)%hdep_y
          ob(icmd)%hdep_y = hz
      endif
        
!!!!! average annual print
        if (time%end_sim == 1 .and. pco%hyd == 0) then
          ob(icmd)%hdep_a = ob(icmd)%hdep_a / time%yrs_prt
          write (5005,100) ob(icmd)%name, time%day, time%yrs, icmd, 
     &      ob(icmd)%typ, ob(icmd)%props, ob(icmd)%hdep_a
        end if

100   format (a16,5i8,a13,30(1x,e10.4))
101   format (5i8,a13,30(1x,e10.4))
       
      end subroutine hyddep_output