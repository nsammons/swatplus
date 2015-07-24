      subroutine str_init_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof
      
      mstr = 0
      eof = 0
      
      !! read all managment operations data from str_init.dat
      inquire (file=in_str%initial_str, exist=i_exist)
      if (i_exist == 0 .or. in_str%initial_str == 'null') then
        write (4444,4445) in_str%initial_str, mstr, imax, 
     &                                         '         null'
        allocate (str_init(0:0))
        db_mx%str_ops = 1
      else
      do
        open (107,file=in_str%initial_str)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            mstr = mstr + 1
          end do
                
        allocate (str_init(0:imax))
        rewind (107) 
        read (107,*) titldum
        read (107,*) header
        
        write (4444,4446) in_str%initial_str, mstr, imax
                
        do istr = 1, mstr
          read (107,*,iostat=eof) i
          backspace (107)
          read (107,*,iostat=eof) k, str_init(istr)%str_prac
          if (eof < 0) exit
        end do
        close (107)
        exit
      end do
      end if
        
      return
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
      end subroutine str_init_read