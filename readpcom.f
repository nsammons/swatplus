      subroutine readpcom
      
      use input_file_module

      use parm
      use jrw_datalib_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof
      
      mcom = 0
      eof = 0

!! Open plant community data file
      inquire (file=in_init%initial_plt, exist=i_exist)
      if (i_exist == 0 .or. in_init%initial_plt == 'null') then
        allocate (pcomdb(0:0))
        allocate (pcomdb(0)%pl(0:0))
        db_mx%plantcom = mcom + 1
      else
      do     
       open (113,file=in_init%initial_plt)
       read (113,*,iostat=eof) titldum
       if (eof < 0) exit
       read (113,*,iostat=eof) mcom
       if (eof < 0) exit
       read (113,*,iostat=eof) header
       if (eof < 0) exit
       allocate (pcomdb(0:mcom))

       do icom = 1, mcom
       ! loop through all plant communities
         read (113,*,iostat=eof)  k
         backspace (113)
         read (113,*,iostat=eof)  kk, pcomdb(k)%name, 
     &                                    pcomdb(k)%plants_com
         if (eof < 0) exit
         mpcom = pcomdb(k)%plants_com
         allocate (pcomdb(k)%pl(mpcom))
         do iplt = 1, mpcom
           read (113,*,iostat=eof) pcomdb(k)%pl(iplt)
           if (eof < 0) exit
         end do
       end do
      end do
      end if

      return
      end