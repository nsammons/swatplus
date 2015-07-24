      subroutine mgt_mgtops_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof
      
      mscheds = 0
      eof = 0
      imax = 0
        
      !!   read mgtops.dat file
      !! calculate number of records in management 
      inquire (file=in_sch%management_sch, exist=i_exist)
      if (i_exist == 0 .or. in_sch%management_sch == 'null') then
        allocate (sched(0:0))
        db_mx%mgt_ops = 0
        write (4444,4445) in_sch%management_sch, mscheds, imax,
     &                                        '         null'
      else
      do
       open (107,file=in_sch%management_sch)
       read (107,*,iostat=eof) titldum
       if (eof < 0) exit
       read (107,*,iostat=eof) mscheds
       if (eof < 0) exit
       read (107,*,iostat=eof) header
       if (eof < 0) exit
       allocate (sched(mscheds+1))
       do isched = 1, mscheds
         read (107,*,iostat=eof) sched(isched)%numb, sched(isched)%name,
     &                 sched(isched)%num_ops
           if (eof < 0) exit
           allocate (sched(isched)%mgt_ops(sched(isched)%num_ops))
           call read_mgtops(isched)
       end do
       db_mx%mgt_ops = mscheds + 1 
       exit
      enddo
      write (4444,4446) in_sch%management_sch, mscheds, imax
      endif
      close(107)
      
      return
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
      
      end subroutine mgt_mgtops_read