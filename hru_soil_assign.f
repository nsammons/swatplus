      subroutine hru_soil_assign

      !! set hru soils to appropriate database soils
      do ihru = 1, mhru

        !! allocate bacteria
        ibact_db = hru(ihru)%dbs%bact_init
        mbac = pesti_db(ibact_db)%num
        if (mbac > 0) then
          do ly = 1, sol(isol)%s%nly
            allocate (hru(ihru)%ly(ly)%bacsol(mbac))
            allocate (hru(ihru)%ly(ly)%bacsor(mbac))
          end do
        end if
        
        mbac_db = hru(ihru)%dbs%bact_init
        mbac = bact(mbac_db)%num
        do ibac = 1, mbac
          if (ly == 1) then
            hru(ihru)%ly(1)%bacsol(ibac) = bact(mbac_db)%bac(ibac)%sol
            hru(ihru)%ly(1)%bacsor(ibac) = bact(mbac_db)%bac(ibac)%sor
          else
            hru(ihru)%ly(1)%bacsol(ibac) = 0.
            hru(ihru)%ly(1)%bacsor(ibac) = 0.
          end if
        end do
        do ipl = 1, mpl
          pcom(ihru)%plg(ipl)%bac(ibac) = bact(ibacdb)%bac(ibac)%plt
        end do
        
        !! allocate pesticides
        ipest_db = hru(ihru)%dbs%pest_init
        mpst = pesti_db(ipest_db)%num
        allocate (hru(ihru)%pst(mpst))
        ipestdb = hru(ihru)%dbs%pest_init
        npmx = pesti_db(ipestdb)%num
        if (npmx > 0) then
          do ly = 1, sol(isol)%s%nly
            allocate (hru(ihru)%ly(ly)%kp(npmx))
            allocate (hru(ihru)%ly(ly)%pst(npmx))
          end do
        end if

        ipest_db = hru(ihru)%dbs%pest_init
        do ipest = 1, pesti_db(ipest_db)%num
         hru(ihru)%pst(ipest)%num_db =                                  
     &                            pesti_db(ipest_db)%pesti(ipest)%num_db
         hru(ihru)%pst(ipest)%plt = pesti_db(ipest_db)%pesti(ipest)%plt
         hru(ihru)%ly(1)%pst(ipest) =                                   
     &                              pesti_db(ipest_db)%pesti(ipest)%soil
         hru(ihru)%pst(ipest)%enr = pesti_db(ipest_db)%pesti(ipest)%enr
        end do

      end do   !hru loop
      
      return
      end subroutine hru_soil_assign