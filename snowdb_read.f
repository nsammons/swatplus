      subroutine snowdb_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof, i, imax
      
      msno = 0
      eof = 0
      imax = 0
      
      
      !! read snow database data from snow.dat
      inquire (file=in_parmdb%snow, exist=i_exist)
      if (i_exist == 0 .or. in_parmdb%snow == 'null') then
        write (4444,4445) in_parmdb%snow, msno, imax, '         null'
        allocate (snodb(0:0))
        db_mx%sno = msno
      else 
      do 
        open (107,file=in_parmdb%snow)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            msno = msno + 1
          end do
          
        rewind (107)
        read (107,*) titldum
        read (107,*) header
        allocate (snodb(0:imax))
        
        write (4444,4446) in_parmdb%snow, msno, imax 
      
        do isno = 1, msno
          read (107,*,iostat=eof) i
          backspace (107)
          read (107,*,iostat=eof) k, snodb(isno)
          if (eof < 0) exit
        end do
      db_mx%sno = msno
      exit
      enddo
      endif
      close (107)
      
      return
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
      end subroutine snowdb_read