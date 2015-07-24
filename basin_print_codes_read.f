      subroutine basin_print_codes_read
      
      use input_file_module
       
      character (len=500) :: header
      character (len=80) :: titldum
      integer :: eof
       
      eof = 0

      !! read weather codes
      inquire (file=in_sim%prt, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_sim%prt)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        read (107,*,iostat=eof) pco
        if (eof < 0) exit
        exit
      enddo
      endif
      close (107)
      
       if (i_exist == 0 .or. in_sim%prt == 'null') then
         imax = 0
         write (4444,4445) in_sim%prt, imax, imax, '         null'
       else 
         imax = 1
         write (4444,4446) in_sim%prt, imax, imax
       end if
            
      return
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6) 
      end subroutine basin_print_codes_read           