      subroutine mgt_contfertops_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, i, imax
      
      mcfertops = 0
      eof = 0
      imax = 0
                       
      !! read continuous fertilizer operations
      inquire (file=in_ops%contfert_ops, exist=i_exist)
      if (i_exist == 0 .or. in_ops%contfert_ops == 'null') then
        write (4444,4445) in_ops%contfert_ops, mcfertops, imax, 
     &                                        '         null'        
        allocate (contfertop_db(0:0))
      else
      do
        open (107,file=in_ops%contfert_ops)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
           read (107,*,iostat=eof) i
           if (eof < 0) exit
           imax = amax1(imax,i)
           mcfertops = mcfertops + 1
          end do
 
        allocate (contfertop_db(0:imax))
        rewind (107)
        read (107,*) titldum
        read (107,*) header
        
        write (4444,4446) in_ops%contfert_ops, mcfertops, imax

        do icfertop = 1, mcfertops
         read (107,*,iostat=eof) i
         backspace (107) 
          read (107,*,iostat=eof) k, contfertop_db(i)
          if (eof < 0) exit
        end do
        db_mx%contfertop_db = mcfertops
        exit
      enddo
      endif
      close(107)
      
      return
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
                              
      end subroutine mgt_contfertops_read