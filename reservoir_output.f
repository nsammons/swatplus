      subroutine reservoir_output(j)
      
      use time_module
      use basin_module
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs reservoir output variables    

!!!!! daily print
        if (pco%res == 3) then
          write (5002,100) time%day, time%yrs, j, res(j)%flo, resd(j)
        end if 
        resm(j) = resm(j) + resd(j)
        resd(j) = resmz
        
!!!!! monthly print
        if (time%end_mo == 1) then
          resy(j) = resy(j) + resm(j)
          if (pco%res == 2) then
            write (5002,100) time%day, time%yrs, j, res(j)%flo, resm(j)
          end if
          resm(j) = resmz
        end if

!!!!! yearly print
       if (time%end_yr == 1) then
          resa(j) = resa(j) + resy(j)
          if (pco%res == 1) then
            write (5002,100) time%day, time%yrs, j, res(j)%flo, resy(j)
          end if
          resy(j) = resmz
       end if

!!!!! average annual print
        if (time%end_sim == 1 .and. pco%res == 0) then
          resa(j) = resy(j) / time%yrs_prt
          write (5002,100) time%day, time%yrs, j, res(j)%flo, resa(j)
        end if
        
100   format (2i6,i8,46e10.3)
       
      end subroutine reservoir_output