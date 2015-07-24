      subroutine landuse_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof, i, imax
      
      mlu = 0
      eof = 0
      imax = 0
      
      !! read all landuse data from landuse.dat
      inquire (file=in_lum%landuse_lum, exist=i_exist)
      if (i_exist == 0 .or. in_lum%landuse_lum == 'null') then
         write (4444,4445) in_lum%landuse_lum, mlu, imax, 
     &                                        '         null'
        allocate (luse_db(0:0))
        db_mx%landuse = mlu
      else
      do
       open (107,file=in_lum%landuse_lum)
       read (107,*,iostat=eof) titldum
       if (eof < 0) exit
       read (107,*,iostat=eof) header
       if (eof < 0) exit
       do while (eof >= 0)
         read (107,*,iostat=eof) i
         if (eof < 0) exit
         imax = amax1(imax,i)
         mlu = mlu + 1
       end do
       
       allocate (luse_db(0:imax))
       rewind (107)
       read (107,*) titldum
       read (107,*) header
       
       write (4444,4446) in_lum%landuse_lum, mlu, imax
      
       do ilu = 1, mlu
         read (107,*,iostat=eof) i
         backspace (107)   
         read (107,*,iostat=eof) k, luse_db(i)
         if (eof < 0) exit
       end do
       db_mx%landuse = mlu
       exit
      end do
      endif
      
      close(107)
      
      return  
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
      end subroutine landuse_read