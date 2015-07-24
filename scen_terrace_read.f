       subroutine scen_terrace_read
      
       use input_file_module
      
       character (len=80) :: titldum
       character (len=80) :: header
       integer :: eof, i, imax
       
       mterrops = 0
       eof = 0
       imax = 0
      
       !! read terrace operations
       inquire (file=in_str%terrace_str, exist=i_exist)
       if (i_exist == 0 .or. in_str%terrace_str == 'null') then
         write (4444,4445) in_str%terrace_str, mterrops, imax, 
     &                                             '         null'
         allocate (terrace_db(0:0))
       else
       do
         open (107,file=in_str%terrace_str)
         read (107,*,iostat=eof) titldum
         if (eof < 0) exit
         read (107,*,iostat=eof) header
         if (eof < 0) exit
        do while (eof >= 0)
          read (107,*,iostat=eof) i
          if (eof < 0) exit
          imax = amax1(imax,i)
          mterrops = mterrops + 1
        end do
         
         allocate (terrace_db(0:imax))
         rewind (107)
         read (107,*) titldum
         read (107,*) header
         
          write (4444,4446) in_str%terrace_str, mterrops, imax
         
         do iterrop = 1, mterrops
           read (107,*,iostat=eof) i
           backspace (107)
           read (107,*,iostat=eof) k, terrace_db(i)
           if (eof < 0) exit
         end do
         db_mx%terrop_db = mterrops
         exit
       enddo
       endif
       close(107)
       return
4445   format (1x,a25,1x,2i6,a)
4446   format (1x,a25,1x,2i6)          
      end subroutine scen_terrace_read