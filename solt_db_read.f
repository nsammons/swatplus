      subroutine solt_db_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof, i, imax
      
      msolt_db = 0
      eof = 0
      imax = 0
      
      !! read all soil test operations data from soiltest.dat
      inquire (file=in_sol%nut_sol,exist=i_exist)
      if (i_exist == 0 .or. in_sol%nut_sol == 'null') then
        write (4444,4445) in_sol%nut_sol, msolt_db, imax, 
     &                                        '         null'
        allocate (solt_db(0:0))
        db_mx%soiltest = msolt_db
      else
        do
         open (107,file=in_sol%nut_sol)
         read (107,*,iostat=eof) titldum
         if (eof < 0) exit
         read (107,*,iostat=eof) header
         if (eof < 0) exit
          do while (eof >= 0)
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            msolt_db = msolt_db + 1
          end do
                
         allocate (solt_db(0:imax))
         rewind (107)
         read (107,*) titldum
         read (107,*) header       
         
         write (4444,4446) in_sol%nut_sol, msolt_db, imax
                
         do isolt = 1, msolt_db
          read (107,*,iostat=eof) i
          backspace (107)
          read (107,*,iostat=eof) k, solt_db(i)
          if (eof < 0) exit
         end do 
       db_mx%soiltest = msolt_db 
       exit
        enddo
      endif
      
      close(107)
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)      
      return  
      end subroutine solt_db_read