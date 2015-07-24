      subroutine sep_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof, i, imax
      
      msep = 0
      eof = 0
      imax = 0
      
      inquire (file=in_str%septic_str,exist=i_exist)                  
      if (i_exist == 0 .or. in_str%septic_str == 'null') then 
        write (4444,4445) in_str%septic_str, msep, imax, '         null'
        allocate (sep(0:0))
        db_mx%septic = msep 
      else
        do 
          open (172,file=in_str%septic_str)
          read (172,*,iostat=eof) titldum
          if (eof < 0) exit
          read (172,*,iostat=eof) header
          if (eof < 0) exit
          do while (eof >= 0)
            read (172,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            msep = msep + 1
          end do
          
          allocate (sep(0:imax))
          rewind (172)
          read (172,*) titldum
          read (172,*) header   
          
          write (4444,4446) in_str%septic_str, msep, imax
                
          do isep = 1, msep
            read (172,*,iostat=eof) i
            backspace (172)
            read(172,*,iostat=eof) k, sep(i)
            if (eof < 0) exit
          end do    
          db_mx%septic = msep
          exit
        enddo
        end if
 
      close(172)
      
      return  
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
      end subroutine sep_read