      subroutine cli_tmeas
      
      use input_file_module
      
      character (len=80) :: header
      character (len=80) :: titldum
      integer :: eof, i, imax
       
       mtmp = 0
       eof = 0
       imax = 0

      !! read all measured daily temperature data
      inquire (file=in_cli%tmp_cli, exist=i_exist)
      if (i_exist == 0 .or. in_cli%tmp_cli == 'null') then
        write (4444,4445) in_cli%tmp_cli, mtmp, imax, 
     &                                        '         null'
         allocate (tmp(0:0))
      else
      do 
        open (107,file=in_cli%tmp_cli)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        imax = 0
          do while (eof >= 0)
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            mtmp = mtmp + 1 
          end do
          
      allocate (tmp(0:imax))
      rewind (107)
      read (107,*) titldum
      read (107,*) header
      
      write (4444,4446) in_cli%tmp_cli, mtmp, imax
      
      do ii = 1, mtmp
        read (107,*,iostat=eof) i
        backspace (107)
        read (107,*,iostat = eof) k, tmp(i)%filename
        if (eof < 0) exit
        open (108,file = tmp(i)%filename)
        read (108,*,iostat=eof) titldum
        if (eof < 0) exit
        read (108,*,iostat=eof) header
        if (eof < 0) exit
        read (108,*,iostat=eof) tmp(i)%nbyr, tmp(i)%lat, tmp(i)%long, 
     &                                tmp(i)%elev
        if (eof < 0) exit
       
        allocate (tmp(i)%ts(366,tmp(i)%nbyr))
        allocate (tmp(i)%ts2(366,tmp(i)%nbyr))

        ! read and store entire year
       do 
         read (108,*,iostat=eof) iyr, istep, tempx, tempn
         if (eof < 0) exit
         if (iyr == time%yrc) exit
       end do
       
       backspace (108)
       iyr_prev = iyr
       iyrs = 1
       
       do 
         read (108,*,iostat=eof) iyr, istep, tmp(i)%ts(istep,iyrs),
     &                        tmp(i)%ts2(istep,iyrs)
         if (eof < 0) exit
         read (108,*,iostat=eof) iyr, istep
         if (eof < 0) exit
         backspace (108)
         if (iyr /= iyr_prev) then
           iyr_prev = iyr
           iyrs = iyrs + 1
         endif
       end do
       close (108)
       
      end do
      close (107)
      exit
      end do
      endif
      
      return
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)

      end subroutine cli_tmeas