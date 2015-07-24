      subroutine condition_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof, i, imax, k, ii
      
      mcond = 0
      eof = 0
      
      !! read all data from hydrol.dat
      inquire (file=in_cond%cond_ctl, exist=i_exist)
      if (i_exist == 0 .or. in_cond%cond_ctl == 'null') then
        allocate (cond_db(0:0))
        db_mx%cond = mcond 
        write (4444,4445) in_cond%cond_ctl, db_mx%cond, imax, 
     &                                          '         null'
      else
        do
          open (107,file=in_cond%cond_ctl)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) mcond
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          allocate (cond_db(0:mcond))

          do i = 1, mcond 
            read (107,*,iostat=eof) ii, cond_db(i)%name, 
     &        cond_db(i)%ruleset_typ, cond_db(i)%num_rulesets,
     &        cond_db(i)%default_typ, cond_db(i)%default_con
            mrs = cond_db(i)%num_rulesets
            allocate (cond_db(i)%ruleset(mrs))
            do irs = 1, mrs
              read (107,*,iostat=eof) cond_db(i)%ruleset(irs)%name, 
     &        cond_db(i)%ruleset(irs)%num_cond,
     &        cond_db(i)%ruleset(irs)%typ, cond_db(i)%ruleset(irs)%con
              if (eof < 0) exit
              mif = cond_db(i)%ruleset(irs)%num_cond
              allocate (cond_db(i)%ruleset(irs)%cond_if(mif))
              do iif = 1, mif
                read (107,*,iostat=eof) 
     &                             cond_db(i)%ruleset(irs)%cond_if(iif)
                if (eof < 0) exit
              end do
            end do
          end do
          db_mx%cond = mcond
          write (4444,4446) in_cond%cond_ctl, db_mx%cond, imax
          exit
        enddo
      endif
      close (107)

4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
      
      return  
      end subroutine condition_read