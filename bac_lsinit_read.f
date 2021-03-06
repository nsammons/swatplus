      subroutine bac_lsinit_read
      
      use input_file_module

      character (len=80) :: titldum
      integer :: mbac_db, ibac, eof
      
      mbac_db = 0
      
      !! allocate and initialize bacteria in soil and plant
      inquire (file=in_bac%init_bac,exist=i_exist)
      if (i_exist == 0 .or. in_bac%init_bac == 'null') then
         allocate (bact(0:0))
         db_mx%bactdb = 0
         write (4444,4445) in_bac%init_bac, mbac_db, db_mx%bactdb,
     &                                                  '         null'
      else
        do
          open (107,file=in_bac%init_bac)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) mbac_db
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          if (mbac_db > 0) then
            allocate (bact(mbac_db))
            do ibac = 1, mbac_db
              read (107,*,iostat=eof) bact(ibac)%num
              if (eof < 0) exit
              mbac = bact(ibac)%num
              allocate (bact(ibac)%bac(mbac+1))
              do ibact = 1, bact(ibac)%num
                read (107,*,iostat=eof) bact(mbac)%bac(ibac)%num_db,
     &            bact(ibac)%bac(ibact)%plt, bact(ibac)%bac(ibact)%sol, 
     &            bact(ibac)%bac(ibact)%sor
                if (eof < 0) exit
              end do
            end do
            db_mx%bactdb = mbac_db + 1
            write (4444,4446) in_bac%init_bac, mbac_db, db_mx%bactdb
          end if
            exit
        end do
      end if
        close (107)

4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)

      return
      end subroutine bac_lsinit_read