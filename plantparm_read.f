      subroutine plantparm_read
      
      use input_file_module

      integer :: ic
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, i, imax
      
      eof = 0
      imax = 0
      mpl = 0

      inquire (file=in_parmdb%plants_plt, exist=i_exist)
      if (i_exist == 0 .or. in_parmdb%plants_plt == ' null') then
         write (4444,4445) in_parmdb%plants_plt, mpl, imax, 
     &                                        '         null'
        allocate (pldb(0:0))
        allocate (plcp(0:0))
      else
      do
        open (104,file=in_parmdb%plants_plt)
        read (104,*,iostat=eof) titldum
        if (eof < 0) exit
        read (104,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (104,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            mpl = mpl + 1
          end do
        allocate (pldb(0:imax))
        allocate (plcp(0:imax))
        
        rewind (104)
        read (104,*) titldum
        read (104,*) header
        
        write (4444,4446) in_parmdb%plants_plt, mpl, imax
        
        do ic = 1, mpl
          read (104,*,iostat=eof) i
          backspace (104)
          read (104,*,iostat=eof) k, pldb(i)
          if (eof < 0) exit
        end do
        exit
      enddo
      endif
      
      close (104)
      return
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
      end subroutine plantparm_read