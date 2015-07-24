      program main
!!    this is the main program that reads input, calls the main simulation
!!    model, and writes output.
!!    comment changes to test merging with trunk and c:\branch_test code
!!    two lines added to c:\branch_test code

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!         ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    date        |NA            |date simulation is performed where leftmost
!!                               |eight characters are set to a value of
!!                               |yyyymmdd, where yyyy is the year, mm is the 
!!                               |month and dd is the day
!!    time        |NA            |time simulation is performed where leftmost
!!                               |ten characters are set to a value of
!!                               |hhmmss.sss, where hh is the hour, mm is the 
!!                               |minutes and ss.sss is the seconds and
!!                               |milliseconds
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    prog        |NA            |program name and version
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    i           |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
      
      use parm
      use hydrograph_module
      use subbasin_module
      use hru_module
!      use wateruse_module
      use climate_module
      use aquifer_module
      use channel_module
      use sd_hru_module
      use sd_channel_module
      use basin_module
      use change_module
      use jrw_datalib_module
      use conditional_module
      use reservoir_module
      use input_file_module
      !use output_landscape_module
      
      implicit none
      integer :: mres, ob1, ob2, ires, imp, mrch, irch, isdc, imax
      integer :: ibac, mbac, mbac_db, ii, iob, idfn, isb, ielem
      integer :: istr_db, mstr_prac, istr, iscenario, j, ichan, idat
      real :: rto, sumn, t_ch
      
      prog = "SWAT+ July 23 2015    MODULAR Rev 41"

      write (*,1000)
 1000 format(1x,"                   SWAT+              ",/,             &
     &          "           Revision 41 - July 23      ",/,             &
     &          "      Soil & Water Assessment Tool    ",/,             &
     &          "               PC Version             ",/,             &
     &          "    Program reading . . . executing",/)

!! process input
      
      open (4444,file="diagnostics.out")
      write (4444,4445) 
4445  format (1x,'FILENAME',21x,'REC',3x,'MAX',9x,'FILE STATUS')
		
      call hyd_read_objs
      call readtime_read
      call readcio_read
!!!  open file to print all output files that are written
      open (9000,file='files_out.out')
      write (9000,*) 'FILES_OUT - OUTPUT FILES WRITTEN'
      
      call basin_cc_read
      call basin_prm_read
      call basin_prm_default
      call basin_print_codes_read
      
      call cli_staread
      call cli_wgnread
      
      call cli_pmeas
      call cli_tmeas
      call cli_smeas
      call cli_hmeas
      call cli_wmeas

      call sep_read
 !!     call septic_default
      call solt_db_read
      call topo_read
      call field_read
      call hydrol_read
      call landuse_read
      call cntbl_read
      call sdr_read
      call snowdb_read
      call soil_db_read
      
      !! read management scheduling and data files
      call mgt_mgtops_read
      call mgt_irrops_read
      call mgt_autoirrops_read
      call mgt_fertops_read
      call mgt_autofertops_read
      call mgt_contfertops_read
      call mgt_pestops_read
      call mgt_contpestops_read
      call mgt_harvops_read
      call mgt_grazeops_read
      call mgt_sweepops_read
      
      !! read structural operations files
      call scen_terrace_read
      call scen_stripcrop_read
      call scen_rsdmgt_read
      call scen_plparmup_read
      call scen_grwway_read
      call scen_fire_read
      call scen_filtstrip_read
      call scen_contour_read
      call scen_bmpuser_read
      !call scen_septic_read
      
      !! databases used by all spatial modules
      call plantparm_read                             !! read the plant paramter database
      call plantparm_init                             !! initialize plant parameters
      call tillparm_read                              !! read the tillage database
      call pestparm_read                              !! read the pesticide database
      call fertparm_read                              !! read the fertilizer/nutrient database
      call urbanparm_read                             !! read the urban land types database
      call bac_lsparms_read                           !! read the bacteria data parameters
      call septicparm_read 
      call atmoparm_read
      
      !! call readseptwq         !! read the septic database (read from HRU_READ)
      call readpcom             !! read the plant community database
        
      call bac_lsinit_read
      call pst_lsinit_read
      
      call hyd_read_connect
      
      call object_output_read
      
      maqu_sp = sp_ob%aqu
      mhru = sp_ob%hru
      mres = sp_ob%res

      call hru_read
      !! set the object number for each hru-to point to weather station
      do ihru = 1, mhru
        hru(ihru)%obj_no = sp_ob1%hru + ihru - 1
        ob(hru(ihru)%obj_no)%ha = hru(ihru)%ha
      end do
      
      call hru_soil_init (mres)
      
      !read calibration data (if included)
      call change_par_read
      do ichg_par = 1, mchg_par
        do ispu = 1, chg_prm(ichg_par)%num_tot
          call current_par_value
        end do
      end do
 
      !! set initial curve number parameters
      do ihru = 1, mhru
        call curno(cn2(ihru),ihru)
      end do

      !! read initial structural operations data and set parms
      call str_init_read
      do ihru = 1, mhru
        istr_db = hru(ihru)%dbs%str_init
        do iscenario = 1, mstr_prac
          istr = str_init(istr_db)%str_prac(iscenario)
          if (istr > 0) then
            call change_scenario(chg_scen(istr),ihru)
          end if
        end do
      end do
      
      !! read structural operations scheduling data and set parms
      call change_scen_read
      
      call sd_hru_read
      call sd_channel_read
      
      !set parms for sd-channel-landscape linkage
      do isdc = 1, sp_ob%chandeg
        i = sp_ob1%chandeg + isdc - 1
        if (ob(i)%props2 > 0) then
          call sd_channel_surf_link (ob(i)%props, ob(i)%props2, mres)
        end if
      end do
        
      call rte_read_nut
      
      call ch_read_hyd (imax)
      call channel_allo (sp_ob%chan)
      do ich = 1, imax
        !! initialize flow routing variables
        call ch_ttcoef (ich)
      end do
      call ch_read_sed
      call ch_read_nut
      call ch_read_pst
      
      do irch = 1, sp_ob%chan
        i = sp_ob1%chan + irch - 1 
        idat = ob(i)%props
        call ch_initial (idat, irch)
      end do
      
      ! read reservoir data
      call res_hyd_read
      call res_sed_read
      call res_nut_read
      call res_pst_read
      call res_weir_read
      
      call sub_read
      
      ! compute weighted Mannings n for each subbasin
      do isub = 1, sp_ob%sub
        sub_n(isub) = 0.
        do ii = 1, sub_d(isub)%num_tot
          ielem = sub_d(isub)%num(ii)
          if (sub_elem(ielem)%obtyp == "hru") then
            ihru = sub_elem(ielem)%obtypno 
            sub_n(isub) = sub_n(isub) + hru(ihru)%luse%ovn* hru(ihru)%km
          end if
        end do
      end do
      call sub_allo
            
      !! allocate reservoir variables
      call res_allo (mres)
      call res_initial (mres)
      
      !! set reservoir object numbers for reservoir objects
      ob1 = sp_ob1%res
      ob2 = sp_ob1%res + sp_ob%res - 1
      ires = 0
      do i = ob1, ob2
        ires = ires + 1
        res_ob(ires)%ob = i
        res_ob(ires)%typ = "res"
        res_ob(ires)%props = ob(i)%props
      end do
        
      !! set reservoir object numbers for hru's with surface storage
      do ihru = 1, mhru
        if (hru(ihru)%dbs%surf_stor > 0) then
          ires = ires + 1
          hru(ihru)%res = ires
          res_ob(ires)%ob = ihru
          res_ob(ires)%typ = "hru"
          res_ob(ires)%props = hru(ihru)%dbs%surf_stor
        else
          iob = sp_ob1%hru + ihru - 1
          if (ob(iob)%flood_ch_lnk > 0) then
            ires = ires + 1
            hru(ihru)%res = ires
            res_ob(ires)%ob = ihru
            res_ob(ires)%typ = "fpl"
            res_ob(ires)%props = 0
          end if
        end if
      end do

      !! set reservoir object numbers for hru's in flood plain without surface storage
      
      do i = 1, sp_ob%objs
        !! compute basin drainage area by summing hru areas if hru's are routed somewhere
        !! or if they are not routed and not part of a subbasin
        if (ob(i)%typ == 1 .or. ob(i)%typ == 2) then
          if (ob(i)%src_tot > 0 .or. ob(i)%src_tot + ob(i)%subs_tot
     &                                                        == 0) then
            bsn%ha = bsn%ha + ob(i)%ha
          end if
        end if
      end do
          
      do isub = 1, sp_ob%sub
        iob = sp_ob1%sub + isub - 1
        ob(iob)%ha = 100. * sub(isub)%da_km2
        bsn%ha = bsn%ha + ob(iob)%ha
        sub_n(isub) = sub_n(isub) / sub(isub)%da_km2
      end do

      !! read modflow inputs  **Ryan**
      
      call aqu_read
      call aqu_initial
      call condition_read
      call readlup

      call output_landscape_init

!!   open soils.out file       
      if (pco%sout > 0) then
        open (121,file="soils.out")
        write (121,1002)
        write (9000,*) 'SOILS     soils.out'
      end if
      
!!   open mgt.out file 
      if (pco%mout > 0) then
        open (143,file="mgt.out",recl=800)
        write (143,*) mgt_hdr
        write (143,*) mgt_hdr_unt1
        write (143,*) mgt_hdr_unt2
        write (9000,*) 'MGT     mgt.out'
      end if
      
!!  yield biomass file
      if (pco%mout > 0) then
        open (4700,file="yield.out", recl=800)
      end if  
      
      if (pco%hydcon > 0) then
        open (7000,file="hydcon.out")
        write (9000,*) 'HYDCON     hydcon.out'
 !       write (7000, 1004)
      end if
      
      if (pco%hyd >= 0) then
        open (5001,file="hyd-out.out",recl=800)
        write (5001,*) hyd_hdr
        write (9000,*) 'HYDOUT     hyd-out.out'
        open (5004,file="hyd-in.out",recl=800)
        write (5004,*) hyd_hdr
        write (9000,*) 'HYDIN     hyd-in.out'
        open (5005,file="deposition.out",recl=800)
        write (5005,*) hyd_hdr
        write (9000,*) 'DEPO     deposition.out'
      end if
             
      !!compute time of concentration (sum of overland and channel times)
      do ihru = 1, mhru
        ichan = topo_db(ith)%channel_db
        t_ov(ihru) = .0556 * (hru(ihru)%topo%slope_len * 
     &         hru(ihru)%luse%ovn) ** .6 / (hru(ihru)%topo%slope + 
     &                                                      .0001) ** .3
        t_ch = .62 * ch_hyd(ichan)%l * ch_hyd(ichan)%n ** .75 /
     &         ((hru(ihru)%ha / 100.)**.125 * ch_hyd(ichan)%s**.375)
        tconc(ihru) = t_ov(ihru) + t_ch
        !! compute fraction of surface runoff that is reaching the main channel
        if (bsn_cc%event>1) then
          brt(ihru) = 1.-Exp(-bsn_prm%surlag / (tconc(ihru) /
     &                                     (bsn_prm%dts / 60.)))	!! urban modeling by J.Jeong
        else
          brt(ihru) = 1. - Exp(-bsn_prm%surlag / tconc(ihru))
        endif
        sumn = sumn + hru(ihru)%luse%ovn * hru(ihru)%km
      end do

      ! compute delivery ratio for each hru in the sub
      sumn = 0.
      do isub = 1, sp_ob%sub
        do ii = 1, sub_d(isub)%num_tot
          ielem = sub_d(isub)%num(ii)
          ihru = sub_elem(ielem)%obtypno
!          if (sub_elem(ielem)%obtyp == "hru" .or.
!     &        sub_elem(ielem)%obtyp == "hlt") then

          if (sub_elem(ielem)%idr == 0) then
            allocate (sub_elem(ielem)%dr(0:0))
            select case (sub_elem(ielem)%obtyp)
            case ("hru")
              rto = tconc(ihru) / sub_tc(isub)
            case ("hlt")
              rto = sd_db(ihru)%tc / sub_tc(isub)
            case ("sdc")
              rto = 1.
            case ("sub")
              rto = 1.
            end select
            
            rto = amin1(.95,rto ** .5)
            sub_elem(ielem)%dr(0) = rto .add. hz
!          end if
          end if
        end do
        sub_n(isub) = sumn / sub(isub)%da_km2
      end do
        
      call hyd_connect_out

!! save initial values
      scenario = 1

      do iscen = 1, scenario
        !! simulate watershed processes
        if (time%step == 0) then
          time%end_sim = 1
          call command
        end if
        if (time%step >= 1) call time_control
        !!reinitialize for new scenario
 !       if (scenario > iscen) call rewind_init
      end do

      do i = 101, 109       !Claire 12/2/09: change 1, 9  to 101, 109.
        close (i)
      end do
      close(124)
      write (*,1001)
 1001 format (/," Execution successfully completed ")
 1002 format (t15,'SURFACE',t29,'-------  SOIL PROFILE  -------',/, 
     &  t8,'DAY',t15,'SOL_RSD',t27,'SOL_P',t38,            
     &  'NO3',t47,'ORG_N',t57,'ORG_P',t70,'CN'/,t16,                    
     &  '(t/ha)',t25,'(kg/ha)',t35,                                     
     &  '(kg/ha)',t45,'(kg/ha)',t56,'(kg/ha)')
	stop
      end