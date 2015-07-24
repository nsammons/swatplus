      subroutine urbanparm_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: urbandb
      integer :: eof, i, imax
      
      eof = 0
      imax = 0
      murb = 0
      
      inquire (file=in_parmdb%urban_urb, exist=i_exist)
      if (i_exist == 0 .or. in_parmdb%urban_urb == 'null') then
          write (4444,4445) in_parmdb%urban_urb, murb, imax, 
     &                                       '         null'
          allocate (urbdb(0:0))
      else
      do
        open (108,file=in_parmdb%urban_urb)
        read (108,*,iostat=eof) titldum
        if (eof < 0) exit
        read (108,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (108,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            murb = murb + 1
          end do
        allocate (urbdb(0:imax)) 
        
        rewind (108)
        read (108,*) titldum
        read (108,*) header
        
        write (4444,4446) in_parmdb%urban_urb, murb, imax
            
         do iu = 1, murb
           read (108,*,iostat=eof) i
           backspace (108)
           read (108,*,iostat=eof) k, urbdb(i)
           if (eof < 0) exit
         end do
       exit
      enddo
      endif

      close (108)
      return
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
      end subroutine urbanparm_read