       subroutine readtime_read 
      
       use time_module
       use input_file_module
       
       character (len=500) :: header
       character (len=80) :: titldum
       integer :: eof
       
       eof = 0

       !! read weather codes
       inquire (file=in_sim%time, exist=i_exist)
       if (i_exist /= 0) then
         write (4444,*) 'time.sim exists'   
       do
         open (107,file=in_sim%time)
         read (107,*,iostat=eof) titldum
         if (eof < 0) exit
         read (107,*,iostat=eof) header
         if (eof < 0) exit
         read (107,*,iostat=eof) time%nbyr, time%yrc, time%idaf, 
     &              time%idal_in, time%step
         if (time%idaf <= 0) time%idaf = 1
         if (eof < 0) exit
         exit
         close (107) 
       enddo
       else
        write (4444,*) 'time.sim does not exist'
       endif
      
       return
       end subroutine readtime_read            