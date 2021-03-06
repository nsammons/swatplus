      subroutine hyd_read_objs

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     reads in the routing information from the watershed configuration
!!     input file (.fig) and calculates the number of subbasins, reaches, 
!!     and reservoirs
     
!!     ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     titldum      |NA            |description line
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!     Intrinsic: Max
!!     SWAT: caps

!!     ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hydrograph_module
      use input_file_module
      
      character (len=80) :: titldum, header
      integer :: npsu, isp, cmdno,hydno, cmd_prev, ob1, ob2
      integer :: eof
      
      eof = 0
      
      !! read number of spatial objects from obj_connect.dat
      inquire (file=in_sim%object_cnt, exist=i_exist)
      if (i_exist == 0 .or. in_sim%object_cnt == 'null') then
        write (4444,4445) in_sim%object_cnt, 'file not found'
          allocate (ob(0:0))
      else
      do
        open (107,file=in_sim%object_cnt)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        read (107,*,iostat=eof) sp_ob
        if (eof < 0) exit
      enddo
      endif

      close (107)
      allocate (ob(sp_ob%objs))
4445  format (1x,a25,13x,a)
      return    
      end subroutine hyd_read_objs