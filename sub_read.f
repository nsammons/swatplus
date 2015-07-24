      subroutine sub_read
      
      use basin_module
      use input_file_module
      
      ! read subbasin parameters (ie drainage area and topographic inputs)
      character (len=80) :: titldum, header
      integer :: eof, i, imax
      
      msub_db = 0
      eof = 0
      imax = 0
      
      inquire (file=in_sub%sub, exist=i_exist)
      if (i_exist == 0 .or. in_sub%sub == 'null') then
          write (4444,4445) in_sub%sub, msub_db, imax, '         null'
          allocate (sub(0:0))
      else
      do
        open (107,file=in_sub%sub)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (107,*,iostat=eof) i
            if (eof < 0 .or. i == 0) exit
            imax = amax1(imax,i)
            msub_db = msub_db + 1
          end do
          
        allocate (sub(0:imax))
        rewind (107)
        read (107,*) titldum
        read (107,*) header
        
        write (4444,4446) in_sub%sub, msub_db, imax

      !! read subbasin parameters
        do isub = 1, msub_db
          read (107,*,iostat=eof) i
          backspace (107)
          read (107,*,iostat=eof) k, sub(i)
          if (eof < 0) exit
        end do
     
      !! read spatial input to each subbasin (ie hru fractions)
      allocate (sub_tc(0:imax))
      allocate (sub_n(0:imax))
      allocate (uhs(0:imax,nstep*3+1))
      allocate (swb_d(0:imax))
      allocate (swb_m(0:imax))
      allocate (swb_y(0:imax))
      allocate (swb_a(0:imax))
      allocate (snb_d(0:imax))
      allocate (snb_m(0:imax))
      allocate (snb_y(0:imax))
      allocate (snb_a(0:imax))
      allocate (sls_d(0:imax))
      allocate (sls_m(0:imax))
      allocate (sls_y(0:imax))
      allocate (sls_a(0:imax))
      allocate (spw_d(0:imax))
      allocate (spw_m(0:imax))
      allocate (spw_y(0:imax))
      allocate (spw_a(0:imax))

      close(107)
        exit
      enddo
      endif      

      return
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
      end subroutine sub_read