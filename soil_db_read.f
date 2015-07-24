      subroutine soil_db_read
      
      use input_file_module

      character (len=80) :: titldum
      character (len=80) :: header
      integer :: isol, j
      integer :: eof
      
      msoils = 0
      eof = 0

      inquire (file=in_sol%soils_sol,exist=i_exist)
      if (i_exist == 0 .or. in_sol%soils_sol == 'null') then
        allocate (soildb(0:0))
        allocate (soildb(0)%ly(0:0))
        db_mx%soil = msoils
      else
        do  
          open (107,file=in_sol%soils_sol)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) msoils
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          allocate (soildb(0:msoils)) 
      
        do isol = 1, msoils
            
         read (107,*,iostat=eof) k, soildb(isol)%s%snam, 
     &          soildb(isol)%s%nly
         mlyr = soildb(isol)%s%nly
         if (eof < 0) exit
       
         allocate (soildb(isol)%ly(mlyr)) 
       
         backspace 107
       read (107,*,iostat=eof) k,
     &  soildb(isol)%s%snam, soildb(isol)%s%nly, 
     &  soildb(isol)%s%hydgrp, soildb(isol)%s%zmx,                      
     &  soildb(isol)%s%anion_excl, soildb(isol)%s%crk,                  
     &  soildb(isol)%s%texture, (soildb(isol)%ly(j)%z,                  
     &  soildb(isol)%ly(j)%bd, soildb(isol)%ly(j)%awc,                  
     &  soildb(isol)%ly(j)%k, soildb(isol)%ly(j)%cbn,                   
     &  soildb(isol)%ly(j)%clay, soildb(isol)%ly(j)%silt,               
     &  soildb(isol)%ly(j)%sand, soildb(isol)%ly(j)%rock,               
     &  soildb(isol)%ly(j)%alb, soildb(isol)%ly(j)%usle_k,              
     &  soildb(isol)%ly(j)%ec, soildb(isol)%ly(j)%cal,                  
     &  soildb(isol)%ly(j)%ph, j = 1, mlyr) 
        if (eof < 0) exit
        end do
        db_mx%soil = msoils
        exit
        enddo
        endif
      
        close (107)
            
      return
      end subroutine soil_db_read