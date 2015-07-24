      subroutine hru_read 
      
      use jrw_datalib_module
      use input_file_module
      
      character (len=500) :: header
      character (len=80) :: titldum
      integer :: eof, i, imax
      
      eof = 0
      imax = 0
      mhru_db = 0
      
      call allocate_parms

      inquire (file=in_hru%hru_data, exist=i_exist)
      if (i_exist == 0 .or. in_hru%hru_data == 'null') then
        allocate (hru_db(0:0))
        allocate (hru(0:0))
        allocate (soil(0:0))
        allocate (pcom(0:0))
        write (4444,4445) in_hru%hru_data, mhru_db, imax, 
     &                                          '         null'
      else 
      do
        open (113,file=in_hru%hru_data)
        read (113,*,iostat=eof) titldum
        if (eof < 0) exit
        read (113,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (113,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            mhru_db = mhru_db + 1
          end do
          
        allocate (hru_db(0:imax))
        allocate (hru(0:imax))
        allocate (soil(0:imax))
        allocate (pcom(0:imax))
        rewind (113)
        read (113,*) titldum
        read (113,*) header

      do ihru = 1, mhru_db
        read (113,*) i
        backspace (113)
        read (113,*,iostat=eof) k, hru(i)%ha, hru_db(i)%dbs
        hru(i)%km = hru(i)%ha / 100.
        if (eof < 0) exit
      end do
      write (4444,4446) in_hru%hru_data, mhru_db, imax
      exit
      enddo
      endif
      
      close (113)
       
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
      
       return
       end subroutine hru_read     