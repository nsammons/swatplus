      subroutine mgt_autofertops_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, i, imax
      
      mafertops = 0
      eof = 0
      imax = 0
      
          !! read autofertilizer operations
          inquire (file=in_ops%autofert_ops, exist=i_exist)
          if (i_exist == 0 .or. in_ops%autofert_ops == 'null') then
            write (4444,4445) in_ops%autofert_ops, mafertops, imax, 
     &                                        '         null'
            allocate (autofertop_db(0:0))
          else
          do
            open (107,file=in_ops%autofert_ops)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            read (107,*,iostat=eof) header
            if (eof < 0) exit
             do while (eof >= 0)
               read (107,*,iostat=eof) i
               if (eof < 0) exit
               imax = amax1(imax,i)
               mafertops = mafertops + 1
             end do
             
            allocate (autofertop_db(0:imax))
            rewind (107) 
            read (107,*) titldum
            read (107,*) header
            
            write (4444,4446) in_ops%autofert_ops, mafertops, imax
            
            do iafertop = 1, mafertops
              read (107,*,iostat=eof) i
              backspace (107)
              read (107,*,iostat=eof) k, autofertop_db(i)
              if (eof < 0) exit
            end do
            db_mx%autofertop_db = mafertops
            exit
          end do
          endif
          close(107)
          
      return
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)     
                              
      end subroutine mgt_autofertops_read