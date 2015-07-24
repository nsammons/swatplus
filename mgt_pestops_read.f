      subroutine mgt_pestops_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof, i, imax
      
      mpestops = 0
      eof = 0
      imax = 0
       
      !! read pesticide operations
      inquire (file=in_ops%pest_ops, exist=i_exist)
      if (i_exist == 0 .or. in_ops%pest_ops == 'null') then
        write (4444,4445) in_ops%pest_ops, mpestops, imax, 
     &                                        '         null'
        allocate (pestop_db(0:0))
      else
      do 
        open (107,file=in_ops%pest_ops)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            mpestops = mpestops + 1
          end do       
        
        allocate (pestop_db(0:imax))
        rewind (107)
        read (107,*) titldum
        read (107,*) header  
        
        write (4444,4446) in_ops%pest_ops, mpestops, imax
           
        do ipestop = 1, mpestops
          read (107,*,iostat=eof) i
          backspace (107)
          read (107,*,iostat=eof) k, pestop_db(i)
          if (eof < 0) exit
        end do
        db_mx%pestdb = mpestops
        exit
      enddo
      endif
      close(107)
      
      return
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)      
      end subroutine mgt_pestops_read