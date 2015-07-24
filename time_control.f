      subroutine time_control

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine contains the loops governing the modeling of processes
!!    in the watershed 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    biomix(:)   |none          |biological mixing efficiency.
!!                               |Mixing of soil due to activity of earthworms
!!                               |and other soil biota. Mixing is performed at
!!                               |the end of every calendar year.
!!    hi_targ(:,:,:)|(kg/ha)/(kg/ha)|harvest index target of cover defined at
!!                               |planting
!!    icr(:)      |none          |sequence number of crop grown within the
!!                               |current year
!!    mcr         |none          |max number of crops grown per year
!!    nhru        |none          |number of HRUs in watershed
!!    tnyld(:)    |kg N/kg yield |modifier for autofertilization target
!!                               |nitrogen content for plant
!!    tnylda(:)   |kg N/kg yield |estimated/target nitrogen content of
!!                               |yield used in autofertilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr       |none          |current year in simulation (sequence)
!!    hi_targ(:,:,:)|(kg/ha)/(kg/ha)|harvest index target of cover defined at
!!                               |planting
!!    i           |julian date   |current day in simulation--loop counter
!!    icr(:)      |none          |sequence number of crop grown within the
!!                               |current year
!!    iida        |julian date   |day being simulated (current julian day)
!!    ndmo(:)     |days          |cumulative number of days accrued in the
!!                               |month since the simulation began where the
!!                               |array location number is the number of the
!!                               |month
!!    ntil(:)     |none          |sequence number of tillage operation within
!!                               |current year
!!    tnylda(:)   |kg N/kg yield |estimated/target nitrogen content of
!!                               |yield used in autofertilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ic          |none          |counter
!!    iix         |none          |sequence number of current year in rotation
!!    iiz         |none          |sequence number of current crop grown
!!                               |within the current year
!!    j           |none          |counter
!!    xx          |none          |current year in simulation sequence
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Mod, Real
!!    SWAT: sim_inityr, std3, xmon, sim_initday, clicon, command
!!    SWAT: writed, writem, tillmix

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module
      use parm
      use time_module
      use mgtops_module
      use climate_module
      use basin_module
      use change_module
      
      integer :: j, iix, iiz, ic, mon, ii
      integer :: isce = 0
      real :: xx

      do curyr = 1, time%nbyr
        time%yrs = curyr
        
        !! initialize annual variables
        call sim_inityr

        !!determine beginning and ending dates of simulation in current year
        if (Mod(time%yrc,4) == 0) then 
          ndays = ndays_leap
        else 
          ndays = ndays_noleap
        end if

        !! set beginning day of simulation for year
        if (time%yrs > 1 .or. time%idaf == 0) then
          time%idaf = 1
        end if

        !! set ending day of simulation for year
        time%idal = ndays(13)
        if (time%yrs == time%nbyr .and. time%idal_in > 0) then
          time%idal = time%idal_in
        end if
        
        !! sum years of printing for average annual writes
        if (time%yrs > pco%nyskip) then
          time%yrs_prt = time%yrs_prt + float(time%idal - time%idaf + 1)
        end if
        
        !! set current julian date to begin annual simulation
        iida = time%idaf

        call xmon
       if (ifirstatmo == 1) then
         ifirstatmo = 0
         if (bsn_cc%atmo == 2) then 
           iyr_at = iyr_atmo1
           mo_at = mo_atmo1
            do
              mo_atmo = mo_atmo + 1
              if (iyr_at == time%yrc .and. mo_at == i_mo) exit
              mo_at = mo_at + 1
              if (mo_at > 12) then
                mo_at = 1
                iyr_at = iyr_at + 1
              endif
              if (mo_atmo > 1000) exit
            end do  
         endif
       endif
       
        do i = time%idaf, time%idal       !! begin daily loop
            
          !! determine month
          iida = i
          call xmon
          time%day = i
          write (*,1234) time%yrs, time%day
          time%mo = i_mo
          !! check for end of month, year and simulation
          time%end_mo = 0
          time%end_yr = 0
          time%end_sim = 0
          if (i == ndays(i_mo+1)) then
            time%end_mo = 1
          end if
          if (i == time%idal) then
            time%end_yr = 1
            if (time%yrs == time%nbyr) then
              time%end_sim = 1
              time%yrs_prt = time%yrs_prt / 365.
            end if
          end if
          
          !! initialize variables at beginning of day
          call sim_initday
          !! added for Srini in output.mgt nitrogen and phosphorus nutrients per JGA by gsm 9/8/2011
                  
          sol_sumno3 = 0.
          sol_sumsolp = 0.
          do j = 1, mhru
            do ly = 1, hru(j)%sol%nly
              sol_sumno3(j) = sol_sumno3(j) + soil(j)%nut(ly)%no3 +
     *          soil(j)%nut(ly)%nh3
              sol_sumsolp(j) = sol_sumsolp(j) +  soil(j)%nut(ly)%solp
            enddo
          enddo

          dtot = dtot + 1.

          if (time%yrs > pco%nyskip) ndmo(i_mo) = ndmo(i_mo) + 1

          call climate_control      !! read in/generate weather
          
          !! check to determine if scenarios need to be set
          
          do while (time%day == chg_scen(isce)%day .and. time%yrc == 
     &                                              chg_scen(isce)%year)
 !!           ityp = chg_scen(isce)%typ           !!nbs
            do ielem = 1, chg_scen(isce)%num_tot
              ihru = chg_scen(isce)%num(ielem)
              call change_scenario(chg_scen(isce), ihru)
            end do
            isce = isce + 1
          end do

          call command              !! command loop
          
          call soil_write  

        do ihru = 1, nhru  
          isched = hru(j)%dbs%mgt_ops
          if (sched(isched)%num_ops > 0) then
          if (time%idaf > 180 .and. wst(iwst)%lat < 0) then
            if (i == 180) then
              isched = hru(j)%dbs%mgt_ops
              if (sched(isched)%mgt_ops(nop(ihru))%op /= 20) then
                dorm_flag = 1
                call mgt_operatn
                dorm_flag = 0
              endif
              nop(ihru) = nop(ihru) + 1
              if (nop(ihru) > sched(isched)%num_ops) then
                nop(ihru) = 1
              end if
      
              phubase(ihru) = 0.
	        yr_skip(ihru) = 0
	      endif
	    end if
	    endif
        end do

        end do                                        !! end daily loop

        !! perform end-of-year processes
        do j = 1, mhru

          !! compute biological mixing at the end of every year

!          if (biomix(j) > .001) call mgt_tillmix (j,biomix(j))
          if (hru(j)%hyd%biomix > .001)
     &             call mgt_newtillmix (j,hru(j)%hyd%biomix)

          !! update sequence number for year in rotation to that of
          !! the next year and reset sequence numbers for operations
          do ipl = 1, npl(j)
            idp = pcom(j)%plcur(ipl)%idplt
            if (idp > 0) then
              if (pldb(idp)%idc == 7) then
                pcom(j)%plcur(ipl)%curyr_mat = 
     &              pcom(j)%plcur(ipl)%curyr_mat + 1
                pcom(j)%plcur(ipl)%curyr_mat = 
     &              Min(pcom(j)%plcur(ipl)%curyr_mat,pldb(idp)%mat_yrs)
              end if
            end if
          end do

          !! update target nitrogen content of yield with data from
          !! year just simulated
          do ic = 1, mcr
            xx = Real(time%yrs)
            tnylda(j) = (tnylda(j) * xx + tnyld(j)) / (xx + 1.)
          end do

          if (time%idaf < 181) then
            isched = hru(j)%dbs%mgt_ops
            if (sched(isched)%num_ops > 0) then
            if (sched(isched)%mgt_ops(nop(j))%op /= 20) then
              dorm_flag = 1
              ihru = j
              call mgt_operatn
              dorm_flag = 0
            end if
            nop(j) = nop(j) + 1
            if (nop(j) > sched(isched)%num_ops) then
              nop(j) = 1
            end if
            
            phubase(j) = 0.
            yr_skip(j) = 0
            end if
          end if
        end do

      !! update simulation year
      time%yrc = time%yrc + 1
      end do            !!     end annual loop

      return
 1234 format (1x,' Executing year/day ', 2i4)
      end subroutine time_control