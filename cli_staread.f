       subroutine cli_staread 
      
       use input_file_module
       
       character (len=500) :: header
       character (len=80) :: titldum
       integer :: eof, imax
       
       eof = 0
       imax = 0
       mwst = 0
       
       inquire (file=in_cli%weat_sta, exist=i_exist)
       if (i_exist == 0 .or. in_cli%weat_sta == 'null') then
         mwst = 1
         allocate (wst_pointer(0:1))
         allocate (wst(0:1))
         allocate (npcp(0:1))
         
         write (4444,4445) in_cli%weat_sta, mwst, imax, 
     &                                             '         null'
       else
       do
            !! read weather stations data from weather.wst - gages and meas/gen
            open (107,file=in_cli%weat_sta)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            !! determine max number for array (imax) and total number in file (mwst)
            do while (eof >= 0)
            read (107,*,iostat=eof) i
            if (eof < 0) exit
              imax = amax1(imax,i)
              mwst = mwst + 1
            end do
            
            allocate (wst_pointer(mwst))
            allocate (wst(imax))
            allocate (npcp(imax))
            
            rewind (107)
            read (107,*) titldum
            read (107,*) header
            do iwst = 1, mwst
              read (107,*) i
              backspace (107)
              read (107,*,iostat=eof) wst_pointer(iwst), wst(i)%name, 
     &                  wst(i)%wco, wst(i)%mseas_dayf, wst(i)%mseas_daye
              if (eof < 0) exit
            end do
            write (4444,4446) in_cli%weat_sta, mwst, imax
            exit
       enddo
       endif
       
      close (107) 
            
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
   
       return
       end subroutine cli_staread         