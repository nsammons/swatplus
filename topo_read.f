      subroutine topo_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof, i, imax
      
      mtopo = 0
      eof = 0
      imax = 0
      
      !! read all data from topo.dat
      inquire (file=in_hyd%topogr_hyd, exist=i_exist)
      if (i_exist == 0 .or. in_hyd%topogr_hyd == 'null') then
        write (4444,4445) in_hyd%topogr_hyd, mtopo, imax, 
     &                                        '         null'
        allocate (topo_db(0:0))
        db_mx%topo = mtopo 
      else
        do
          open (107,file=in_hyd%topogr_hyd)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          do while (eof >= 0)
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            mtopo = mtopo + 1
          end do
          
          allocate (topo_db(0:imax))
          rewind (107)
          read (107,*) titldum
          read (107,*) header
          
          write (4444,4446) in_hyd%topogr_hyd, mtopo, imax
                   
          do ith = 1, mtopo
             read (107,*,iostat=eof) i
             backspace (107) 
             read (107,*,iostat=eof) k, topo_db(i)
             if (eof < 0) exit
          end do
          db_mx%topo = mtopo
          exit
        enddo
      endif
      close (107)
      
      return  
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
      end subroutine topo_read