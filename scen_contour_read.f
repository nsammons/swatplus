       subroutine scen_contour_read
      
       use input_file_module
      
       character (len=80) :: titldum
       character (len=80) :: header
       integer :: eof, i, imax
       
       mcontops = 0
       eof = 0
       imax = 0
      
       !! read contour operations
       inquire (file=in_str%contour_str, exist=i_exist)
       if (i_exist == 0 .or. in_str%contour_str == 'null') then
         write (4444,4445) in_str%contour_str, mcontops, imax, 
     &                                           '         null'
         allocate (contour_db(0:0))
       else
       do
         open (107,file=in_str%contour_str)
         read (107,*,iostat=eof) titldum
         if (eof < 0) exit
         read (107,*,iostat=eof) header
         if (eof < 0) exit
         do while (eof >= 0)
           read (107,*,iostat=eof) i
           if (eof < 0) exit
          imax = amax1(imax,i)
          mcontops = mcontops + 1
         end do
         
         allocate (contour_db(0:imax))
         rewind (107)
         read (107,*) titldum
         read (107,*) header    
         
         write (4444,4446) in_str%contour_str, mcontops, imax
         
         do icontop = 1, mcontops
           read (107,*,iostat=eof) i
           backspace (107)
           read (107,*,iostat=eof) k, contour_db(i)
           if (eof < 0) exit
         end do
         db_mx%contop_db = mcontops
         exit
       enddo
       endif
       close(107)
4445   format (1x,a25,1x,2i6,a)
4446   format (1x,a25,1x,2i6)
       return      
      end subroutine scen_contour_read