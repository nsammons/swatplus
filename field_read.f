      subroutine field_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof, i, imax
      
      mfield = 0
      eof = 0
      imax = 0
        
      !! read all data from topo.dat
      inquire (file=in_hyd%field_fld, exist=i_exist)
      if (i_exist == 0 .or. in_hyd%field_fld == 'null') then
        allocate (field_db(0:0))
        db_mx%topo = mfield
        write (4444,4445) in_hyd%field_fld, db_mx%topo, imax, 
     &                                            '         null'
      else
        do
          open (107,file=in_hyd%field_fld)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          do while (eof >= 0)
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            mfield = mfield + 1
          end do
          
          allocate (field_db(0:imax))
          rewind (107)
          read (107,*) titldum
          read (107,*) header
          
          
          do ith = 1, mfield
             read (107,*,iostat=eof) i
             backspace (107) 
             read (107,*,iostat=eof) k, field_db(i)
             if (eof < 0) exit
          end do
          db_mx%topo = mfield
          write (4444,4446) in_hyd%field_fld, db_mx%topo, imax
          exit
        enddo
      endif
      close (107)
      
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)      
      return  
      end subroutine field_read