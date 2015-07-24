      subroutine mgt_sweepops_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, i, imax
      
      msweepops = 0
      eof = 0
      imax = 0
      
      !! read street sweeping operations 
      inquire (file=in_ops%sweep_ops, exist=i_exist)
      if (i_exist == 0 .or. in_ops%sweep_ops == 'null') then 
         write (4444,4445) in_ops%sweep_ops, msweepops, imax, 
     &                                        '         null'
        allocate (sweepop_db(0:0))
      else
      do
        open (107,file=in_ops%sweep_ops)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        do while (eof >= 0)
          read (107,*,iostat=eof) i
          if (eof < 0) exit
          imax = amax1(imax,i)
          msweepops = msweepops + 1
        end do
        
         allocate (sweepop_db(0:imax))
         rewind (107)
         read (107,*) titldum
         read (107,*) header
         
         write (4444,4446) in_ops%sweep_ops, msweepops, imax
              
        do isweepop = 1, msweepops
         read (107,*,iostat=eof) i
         backspace (107) 
          read (107,*,iostat=eof) k, sweepop_db(i)
          if (eof < 0) exit
        end do
        db_mx%sweepop_db = msweepops + 1
        exit
      enddo
      endif
      close(107)
      
      return
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
      end subroutine mgt_sweepops_read