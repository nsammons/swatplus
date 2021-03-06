      subroutine bac_lsparms_read
      
      use input_file_module

      character (len=80) :: titldum, header
      integer :: mbac, ibac
      character (len=13) :: file
      integer :: eof, i, imax
      
      mbac = 0
      eof = 0
      imax = 0
      
      
      !! read bacteria properties
      inquire (file=in_bac%bacteria,exist=i_exist)
      if (i_exist == 0 .or. in_bac%bacteria == 'null') then
         write(4444,4445) in_bac%bacteria, mbac, imax, '         null'
         allocate (bac_db(0:0))
      else
      do
        open (107,file=in_bac%bacteria)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            mbac = mbac + 1
          end do
          
        allocate (bac_db(0:imax))
        rewind (107)
        read (107,*) titldum
        read (107,*) header
        
        write (4444,4446) in_bac%bacteria, mbac, imax

        do ibac = 1, mbac
          read (107,*,iostat=eof) i
          backspace (107)
          read (107,*,iostat=eof) k, bac_db(i)
          if (eof < 0) exit
        end do
        exit
      enddo
      endif
      
      close (107)
      
      return
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
      end subroutine bac_lsparms_read