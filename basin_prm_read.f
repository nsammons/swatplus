      subroutine basin_prm_read
      
      use input_file_module
      
      integer :: eof
      
      eof = 0

      inquire (file=in_basin%parms_bas, exist=i_exist)
      if (i_exist /= 0) then
        !! read basin parameters
      do
        open (107,file=in_basin%parms_bas)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        read (107,*,iostat=eof) bsn_prm
        if (eof < 0) exit
        exit
      enddo
      end if
        close(107)
        
      if (i_exist == 0 .or. in_basin%parms_bas == 'null') then
        imax = 0
        write (4444,4445)in_basin%parms_bas, imax, imax, '         null'
      else 
        imax = 1
        write (4444,4446) in_basin%parms_bas, imax, imax
      end if
      
      return
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
          
      end subroutine basin_prm_read