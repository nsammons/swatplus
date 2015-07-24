      subroutine tillparm_read
      
      use input_file_module

      integer :: it     
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: tilldbase
      integer :: eof, i, imax
      
      eof = 0
      imax = 0
      mtl = 0
      
      inquire (file=in_parmdb%till_til, exist=i_exist)
      if (i_exist == 0 .or. in_parmdb%till_til == 'null') then
          write (4444,4445) in_parmdb%till_til, mtl, imax, 
     &                                        '         null'
          allocate (tilldb(0:0))
      else
      do
        open (105,file=in_parmdb%till_til)
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (105,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            mtl = mtl + 1
          end do
          
        allocate (tilldb(0:imax)) 
        rewind (105)
        read (105,*) titldum
        read (105,*) header
        
        write (4444,4446) in_parmdb%till_til, mtl, imax       
        
          do itl = 1, mtl
            read (105,*,iostat=eof) i
            backspace (105)
            read (105,*,iostat=eof) k, tilldb(i)
            if (eof < 0) exit
          end do    
        exit
      enddo
      endif

      close (105)
      return
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
5000  format (i4,4x,a8,8x,f8.3,8x,f8.3,8x,f8.3)
      end subroutine tillparm_read