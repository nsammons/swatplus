      subroutine sdr_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof, i, imax
      
      msdr = 0
      eof = 0
      imax = 0
      
      !! read all subsurface drainage data from sdr.dat
      inquire (file=in_str%tiledrain_str, exist=i_exist)
      if (i_exist == 0 .or. in_str%tiledrain_str == 'null') then
        write (4444,4445) in_str%tiledrain_str, msdr, imax, 
     &                                              '         null'
        allocate (sdr(0:0))  
        db_mx%sdr = msdr 
      else
        do
          open (107,file=in_str%tiledrain_str)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*) header
          if (eof < 0) exit
          do while (eof >= 0)
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            msdr = msdr + 1
          end do
          
          allocate (sdr(0:imax))
          rewind (107)
          read (107,*) titldum
          read (107,*) header  
          
          write (4444,4446) in_str%tiledrain_str, msdr, imax

          do isdr = 1, msdr
            read (107,*,iostat=eof) i
             backspace (107) 
            read (107,*,iostat=eof) k, sdr(i)
            if (eof < 0) exit
          end do
          db_mx%sdr = msdr
          exit
        enddo
        endif
      close(107)
      
      return
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
      end subroutine sdr_read