      subroutine fertparm_read
      
      use input_file_module
   
      integer :: it
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: fertdbase
      integer :: eof, i, imax
      
      eof = 0
      imax = 0
      mfrt = 0
      
      inquire (file=in_parmdb%fert_frt, exist=i_exist)
      if (i_exist == 0 .or. in_parmdb%fert_frt == 'null') then
         allocate (fertdb(0:0))
      else
      do  
        open (107,file=in_parmdb%fert_frt)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
           do while (eof >= 0) 
             read (107,*,iostat=eof) i
             if (eof < 0) exit
             imax = amax1(imax,i)
             mfrt = mfrt + 1
           end do
           
        allocate (fertdb(0:imax)) 
        rewind (107)
        read (107,*) titldum
        read (107,*) header
        
        do it = 1, mfrt
          read (107,*,iostat=eof) i
          backspace (107)
          read (107,*,iostat=eof) k, fertdb(i)
          if (eof < 0) exit
        end do
       exit
      enddo
      endif

      if (i_exist == 0) then
        write (4444,4445) in_parmdb%fert_frt, mfrt, imax, 
     &                                         '         null'
      else 
        write (4444,4446) in_parmdb%fert_frt, mfrt, imax
      end if
      
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
5000  format (i4,1x,a8,6f8.3,2f11.0)
      
      close (107)
      return
      end subroutine fertparm_read