      subroutine pst_lsinit_read
      
      use input_file_module

      character (len=80) :: titldum, header
      integer :: mpesti_db, ipestdb, eof
      integer :: i, imax
      
      eof = 0
      mpesti_db = 0
      imax = 0
                             
      !! real all pesticide initialization data from pest_initial.dat
      inquire (file=in_init%initial_pst,exist=i_exist)
      if (i_exist == 0 .or. in_init%initial_pst == 'null') then
        write (4444,4445) in_init%initial_pst, mpesti_db, imax, 
     &                                        '         null'
         allocate (pesti_db(0:0))
      else
        do
          open (107,file=in_init%initial_pst)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
           do while (eof >= 0)
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            mpesti_db = mpesti_db + 1
           end do
           
!          allocate (pesti_db(mpesti_db+1))
           allocate (pesti_db(0:imax))
           rewind (107)
           read (107,*) titldum
           read (107,*) header
           
           write (4444,4446) in_init%initial_pst, mpesti_db, imax
           
          do ipestdb = 1, mpesti_db
            read (107,*,iostat=eof) i
            backspace (107)
            read (107,*,iostat=eof) k, pesti_db(i)%name, 
     &                                            pesti_db(i)%num
            if (eof < 0) exit
            allocate (pesti_db(ipestdb)%pesti(pesti_db(i)%num))
            do ipest = 1, pesti_db(i)%num
              read (107,*,iostat=eof) pesti_db(i)%pesti(ipest)
              if (eof < 0) exit
            end do
          end do
          db_mx%pestdb = mpesti_db + 1
          exit
        end do
      end if
      
      close (107) 
      
      return
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
      end subroutine pst_lsinit_read