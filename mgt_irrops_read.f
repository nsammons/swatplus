       subroutine mgt_irrops_read
      
       use input_file_module
      
       character (len=80) :: titldum
       character (len=80) :: header
       integer :: eof, i, imax
       
       mirrops = 0
       eof = 0
       imax = 0
       
      !! read irrigation operations
      inquire (file=in_ops%irr_ops, exist=i_exist)
      if (i_exist == 0 .or. in_ops%irr_ops == 'null') then
         write (4444,4445) in_ops%irr_ops, mirrops, imax, 
     &                                        '         null'
        allocate (irrop_db(0:0))
      else
      do
        open (107,file=in_ops%irr_ops)
        read (107,*) titldum
        if (eof < 0) exit
        read (107,*) header
        if (eof < 0) exit
        do while (eof >= 0)
          read (107,*,iostat=eof) i
          if (eof < 0) exit
          imax = amax1(imax,i)
          mirrops = mirrops + 1
        end do

        allocate (irrop_db(0:imax))
        rewind (107)
        read (107,*) titldum
        read (107,*) header
        
        write (4444,4446) in_ops%irr_ops, mirrops, imax
        
        do irr_op = 1, mirrops
         read (107,*,iostat=eof) i
         backspace (107) 
          read (107,*) k, irrop_db(i)
          if (eof < 0) exit
        end do
        db_mx%irrop_db = mirrops
        exit
      enddo
      endif
      close(107)
      
      return
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
      end subroutine mgt_irrops_read