      subroutine atmoparm_read
      
      use parm
      use basin_module
      use input_file_module
      
      character (len=80) :: titldum
      character (len=13) :: atmodb
      character (len=80) :: file
      integer :: eof 
  
      matmodep = 0
      eof = 0


      inquire (file=in_parmdb%atmodb,exist=i_exist)
      if (i_exist == 0 .or. in_parmdb%atmodb == 'null') then
        !!no filename 
        allocate (atmodep(0:0))
        db_mx%atmodep = 0
        write (4444,4445) in_parmdb%atmodb, matmodep, db_mx%atmodep,
     &                                                  '         null'
      else
        do
          open (127,file = in_parmdb%atmodb)
            do iii = 1, 5
              read (127,*,iostat=eof) titldum
              if (eof < 0) exit
            end do
              read (127,*,iostat=eof) matmodep, momax
              if (eof < 0) exit
              allocate (atmodep(0:matmodep))
              db_mx%atmodep = matmodep
              write(4444,4446) in_parmdb%atmodb, matmodep, db_mx%atmodep
          
       if (bsn_cc%atmo == 1) then
          do iadep = 1, matmodep
            read (127,*,iostat=eof) atmodep(iadep)%nh4_rf,
     &                                 atmodep(iadep)%no3_rf,
     &                                 atmodep(iadep)%nh4_dry,
     &                                 atmodep(iadep)%no3_dry
            if (eof < 0) exit
          end do
       else if (bsn_cc%atmo == 2) then
            read (127,1001,iostat=eof) mo_atmo1, iyr_atmo1
1001        format (2i6)
            do iadep = 1, matmodep
              allocate (atmodep(iadep)%no3_rfmo(momax))
              allocate (atmodep(iadep)%nh4_rfmo(momax))
              allocate (atmodep(iadep)%no3_drymo(momax))
              allocate (atmodep(iadep)%nh4_drymo(momax))
              read (127,*) (atmodep(iadep)%nh4_rfmo(imo), imo = 1,momax)
              read (127,*) (atmodep(iadep)%no3_rfmo(imo), imo = 1,momax)
              read (127,*) (atmodep(iadep)%nh4_drymo(imo),imo = 1,momax)
              read (127,*) (atmodep(iadep)%no3_drymo(imo),imo = 1,momax)
            end do
       endif
       exit
       enddo
      endif

4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)

      return
      end subroutine atmoparm_read