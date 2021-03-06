      module channel_module
       
      use hydrograph_module
      use climate_parms
      use time_module

      integer :: jsed, jnut, jpst      
      real :: rttime, rchdep, rtevp, rttlc 
      real, dimension (:), allocatable :: hrtwtr     !m^3 H2O       |water leaving reach
      real, dimension (:), allocatable :: hharea     !m^2           |cross-sectional area of flow
      real, dimension (:), allocatable :: hdepth     !m             |depth of flow
      real, dimension (:), allocatable :: rhy        !m H2O         |main channel hydraulic radius
      real, dimension (:), allocatable :: hsdti      !m^3/s         |flow rate in reach for hour
      real, dimension (:), allocatable :: hhtime     !hr            |flow travel time for hour
      real, dimension (:), allocatable :: hrttlc     !m^3 H2O       |transmission losses from reach during time step
      real, dimension (:), allocatable :: hrtevp     !m^3 H2O       |evaporation from reach during time step
      real, dimension (:), allocatable :: hhstor     !m^3 H2O       |water stored in reach at end of hour
      real, dimension (:), allocatable :: hrchwtr    !m^3 H2O       |water stored at beginning of day
      real, dimension (:), allocatable :: halgae     !mg alg/L      |algal biomass concentration in reach
      real, dimension (:), allocatable :: hbactlp    !# cfu/100mL   |less persistent bacteria in reach/outflow during hour
      real, dimension (:), allocatable :: hbactp     !# cfu/100mL   |persistent bacteria in reach/outflow during hour
      real, dimension (:), allocatable :: hbod       !mg O2/L       |carbonaceous biochemical oxygen demand inreach at end of hour
      real, dimension (:), allocatable :: hchla      !mg chl-a/L    |chlorophyll-a concentration in reach at end of hour
      real, dimension (:), allocatable :: hdisox     !mg O2/L       |dissolved oxygen concentration in reach at end of hour
      real, dimension (:), allocatable :: hnh4       !mg N/L        |ammonia concentration in reach at end of hour
      real, dimension (:), allocatable :: hno2       !mg N/L        |nitrite concentration in reach at end of hour
      real, dimension (:), allocatable :: hno3       !mg N/L        |nitrate concentration in reach at end of hour 
      real, dimension (:), allocatable :: horgn      !mg N/L        |organic nitrogen concentration in reach at end of hour
      real, dimension (:), allocatable :: horgp      !mg P/L        |organic phosphorus concentration in reach at end of hour
      real, dimension (:), allocatable :: hsedst     !metric tons   |amount of sediment stored in reach at the end of hour    
      real, dimension (:), allocatable :: hsedyld    !metric tons   |sediment transported out of reach during hour
      real, dimension (:), allocatable :: hsolp      !mg P/L        |dissolved phosphorus concentration in reach at end of hour
      real, dimension (:), allocatable :: hsolpst    !mg pst/m^3    |soluble pesticide concentration in outflow on day
      real, dimension (:), allocatable :: hsorpst    !mg pst/m^3    |sorbed pesticide concentration in outflow on day
      real, dimension (:), allocatable :: rchsep     !
      
      
      real :: bnkrte              !
      real :: degrte              !
      real :: sedrch              !metric tons       |sediment transported out of reach on day
      real :: rch_sil             !
      real :: rch_cla             !
      real :: rtwtr               !m^3 H2O           |water leaving reach on day
      integer:: sed_ch 
      
      
      type channel
          real :: algae = 0.     ! mg alg/L      |algal biomass concentration in reach
          real :: ammonian = 0.  ! mg N/L        |ammonia concentration in reach
          real :: bankst = 0.    ! m^3 H2O       |bank storage 
          real :: li = 0.        ! km            |initial length of main channel
          real :: orgn = 0.      !               |organic nitrogen contribution from channel erosion 
          real :: orgp = 0.      !               |organic phosphorus contribution from channel erosion 
          real :: si = 0.        !(m/n)          |slope of main channel
          real :: wi = 0.        !(m)            |width of main channel at top of bank
          real :: di = 0.        !(m)            |depth of main channel from top of bank to bottom
          real :: chlora = 0.    ! mg chl-a/L    |chlorophyll-a concentration in reach
          real :: pst_conc =0.   ! mg/(m**3)     |initial pesticide concentration in reach
          real :: dep_chan =0.   ! m             |average daily water depth in channel
          real :: disolvp = 0.   ! mg P/L        |dissolved P concentration in reach
          real :: drift = 0.     ! kg            |amount of pesticide drifting onto main channel in subbasin
          real :: flwin = 0.     ! m^3 H2O       |flow into reach on previous day
          real :: flwout = 0.    ! m^3 H2O       |flow out of reach on previous day
          real :: nitraten = 0.  ! mg N/L        |nitrate concentration in reach
          real :: nitriten = 0.  ! mg N/L        |nitrite concentration in reach
          real :: organicn = 0.  ! mg N/L        |organic nitrogen concentration in reach
          real :: organicp = 0.  ! mg P/L        |organic phosphorus concentration in reach
          real :: rch_bactlp= 0. ! # cfu/100ml   |less persistent bacteria stored in reach
          real :: rch_bactp = 0. ! # cfu/100ml   |persistent bacteria stored in reach
          real :: rch_cbod = 0.  ! mg O2/L       |carbonaceous biochemical oxygen demand in reach 
          real :: rch_dox = 0.   ! mg O2/L       |dissolved oxygen concentration in reach
          real :: rchstor = 0.   ! m^3 H2O       |water stored in reach
          real :: sedst = 0.     ! metric tons   |amount of sediment stored in reach
          real :: vel_chan = 0.  ! m/s           |average flow velocity in channel
          real :: bed_san = 0.
          real :: bed_sil = 0.
          real :: bed_cla = 0.
          real :: bed_gra = 0.
          real :: bnk_san = 0.
          real :: bnk_sil = 0.
          real :: bnk_cla = 0.
          real :: bnk_gra = 0.
          real :: depfp = 0.
          real :: depprfp = 0.
          real :: depsilfp = 0.
          real :: depclafp = 0.
          real :: depch = 0.
          real :: depprch = 0.
          real :: depsanch = 0.
          real :: depsilch = 0.
          real :: depclach = 0.
          real :: depsagch= 0.
          real :: deplagch = 0.
          real :: depgrach = 0.
          real :: sanst = 0.
          real :: silst = 0.
          real :: clast = 0.
          real :: sagst = 0.
          real :: lagst = 0.
          real :: grast = 0.
          real, dimension (13) :: phi = 0.
          real :: wattemp = 0.         
          real :: bactp = 0.
         real :: bactlp = 0.
      end type channel
      type (channel), dimension(:), allocatable :: ch 
      
      type channel_wq_db
          character(len=13) :: name
  !        real :: rs1 = 1.          ! m/day or m/hr   |local algal settling rate in reach at 20 deg C
  !        real :: rs2 = .05         ! (mg disP-P)/    |benthos source rate for dissolved phos ((m**2)*day)|in reach at 20 deg C
          !                                              or (mg disP-P)/((m**2)*hr)|
  !        real :: rs3 = .5          ! (mg NH4-N)/     |benthos source rate for ammonia nit in ((m**2)*day)|reach at 20 deg C
          !                                              or (mg NH4-N)/((m**2)*hr)|
  !        real :: rs4 = .05         ! 1/day or 1/hr   |rate coeff for organic nitrogen settling in reach at 20 deg C
  !        real :: rs5 = .05         ! 1/day or 1/hr   |org phos settling rate in reach at 20 deg C
  !        real :: rs6 = 2.5         ! 1/day           |rate coeff for settling of arbitrary non-conservative constituent in reach
  !        real :: rs7 = 2.5         ! (mg ANC)/       |benthal source rate for arbitrary ((m**2)*day)|non-conservative constituent in reach
  !        real :: rk1 = 1.71        ! 1/day or 1/hr   |CBOD deoxygenation rate coeff in reach at 20 deg C
  !        real :: rk2 = 1.          ! 1/day or 1/hr   |reaeration rate in accordance with Fickian diffusion in reach at 20 deg C
  !        real :: rk3 = 2.          ! 1/day or 1/hr   |rate of loss of CBOD due to settling in reach at 20 deg C
  !        real :: rk4 = 0.          ! mg O2/          |sed oxygen demand rate in reach ((m**2)*day)|at 20 deg C or mg O2/((m**2)*hr)
  !        real :: rk5 = 1.71        ! 1/day           |coliform die-off rate in reach
  !        real :: rk6 = 1.71        ! 1/day           |decay rate for arbitrary non-conservative constituent in reach
  !        real :: bc1 = .55         ! 1/hr            |rate constant for biological oxidation of NH3 to NO2 in reach at 20 deg C
  !        real :: bc2 = 1.1         ! 1/hr            |rate constant for biological oxidation of NO2 to NO3 in reach at 20 deg C
  !        real :: bc3 = .21         ! 1/hr            |rate constant for hydrolysis of organic N to ammonia in reach at 20 deg C
  !        real :: bc4 = .35         ! 1/hr            |rate constant for the decay of organic P to dissolved P in reach at 20 deg C
  !        real :: pst_rea = .007    ! 1/day           |pesticide reaction coeff in reach
  !        real :: pst_vol = .01     ! m/day           |pesticide volatilization coeff in reach
  !        real :: pst_koc = 0.      ! m**3/g          |pesticide partition coeff between water and sediment in reach
  !        real :: pst_mix = .001    ! m/day           |mixing velocity (diffusion/dispersion) for pesticide in reach
  !        real :: pst_rsp = .002    ! m/day           |resuspension velocity in reach for pesticide sorbed to sediment 
  !        real :: pst_stl = 1.      ! m/day           |settling velocity in reach for pesticide sorbed to sediment
  !        real :: sedpst_act = .03  ! m               |depth of active sediment layer in reach for pesticide
  !        real :: sedpst_bry = .002 ! m/day           |pesticide burial velocity in river bed sediment
  !        real :: sedpst_conc = 0.  ! mg/(m**3)       |inital pesticide concentration in river bed sediment
  !        real :: sedpst_rea = .05  ! 1/day           |pesticide reaction coeff in river bed sediment  
      end type channel_wq_db
      type (channel_wq_db), dimension(:), allocatable :: chwq

      type ch_output
          real :: flo_in = 0.                  ! (m^3/s)
          real :: flo_out = 0.                 ! (m^3/s)
          real :: evap = 0.                    ! (m^3/s)
          real :: tloss = 0.                   ! (m^3/s)
          real :: sed_in = 0.                  ! (tons)
          real :: sed_out = 0.                 ! (tons)
          real :: sed_conc = 0.                ! (mg/L)
          real :: orgn_in = 0.                 ! (kg N)
          real :: orgn_out = 0.                ! (kg N)
          real :: orgp_in = 0.                 ! (kg P)
          real :: orgp_out = 0.                ! (kg P)
          real :: no3_in = 0.                  ! (kg N)
          real :: no3_out = 0.                 ! (kg N)
          real :: nh4_in = 0.                  ! (kg)
          real :: nh4_out = 0.                 ! (kg)
          real :: no2_in = 0.                  ! (kg)
          real :: no2_out = 0.                 ! (kg)
          real :: solp_in = 0.                 ! (kg P)
          real :: solp_out = 0.                ! (kg P)
          real :: chla_in = 0.                 ! (kg)
          real :: chla_out = 0.                ! (kg)
          real :: cbod_in = 0.                 ! (kg)
          real :: cbod_out = 0.                ! (kg)
          real :: dis_in = 0.                  ! (kg)
          real :: dis_out = 0.                 ! (kg)
          real :: solpst_in = 0.               ! (mg pst)
          real :: solpst_out = 0.              ! (mg pst)
          real :: sorbpst_in = 0.              ! (mg pst)
          real :: sorbpst_out = 0.             ! (mg pst)
          real :: react = 0.                   ! (mg pst)
          real :: volat = 0.                   ! (mg)
          real :: setlpst = 0.                 ! (mg pst)
          real :: resuspst = 0.                ! (mg)
          real :: difus = 0.                   ! mg pst                
          real :: reactb = 0.                  ! pst/sed (mg)
          real :: bury = 0.                    ! pst bury (mg)
          real :: sedpest = 0.                 ! pst in rivbed sed mg
          real :: bacp = 0.                    ! persistent bact out
          real :: baclp = 0.                   ! lpersistent bact out
          real :: met1 = 0.                    ! cmetal #1  
          real :: met2 = 0.                    ! cmetal #2
          real :: met3 = 0.                    ! cmetal #3
          real :: sand_in = 0.                 ! sand in 
          real :: sand_out = 0.                ! sand out
          real :: silt_in = 0.                 ! silt_in
          real :: silt_out = 0.                ! silt_out
          real :: clay_in = 0.                 ! clay_in
          real :: clay_out = 0.                ! clay_out
          real :: smag_in = 0.                 ! sm ag in  
          real :: smag_out = 0.                ! sm ag out
          real :: lag_in = 0.                  ! lg ag in
          real :: lag_out = 0.                 ! lg ag out
          real :: grvl_in = 0.                 ! gravel in
          real :: grvl_out = 0.                ! gravel out           
          real :: bnk_ero = 0.                 ! bank erosion
          real :: ch_deg = 0.                  ! channel degradation
          real :: ch_dep = 0.                  ! channel deposition
          real :: fp_dep = 0.                  ! flood deposition
          real :: tot_ssed = 0.                ! total suspended sediments
      end type ch_output
      
      type (ch_output), dimension(:), allocatable, save :: ch_d
      type (ch_output), dimension(:), allocatable, save :: ch_m
      type (ch_output), dimension(:), allocatable, save :: ch_y
      type (ch_output), dimension(:), allocatable, save :: ch_a
      type (ch_output) :: chz
      
      type ch_header
          character (len=6) :: yrs =          ' time '
          character (len=6) :: yrc =          ' year '
          character (len=8) :: isd =        '   unit '
          character(len=15) :: flo_in =    '    floin_m^3/s'        ! (m^3/s)
          character(len=15) :: flo_out =   '   floout_m^3/s'        ! (m^3/s)
          character(len=15) :: evap =      '     evap_m^3/s'        ! (m^3/s)
          character(len=15) :: tloss =     '    tloss_m^3/s'        ! (m^3/s)
          character(len=15) :: sed_in =    '     sedin_tons'        ! (tons)
          character(len=15) :: sed_out=    '    sedout_tons'        ! (tons)
          character(len=15) :: sed_conc =  '   sedconc_mg/L'        ! (mg/L)
          character(len=15) :: orgn_in =   '     orgnin_kgN'        ! (kg N)
          character(len=15) :: orgn_out =  '    orgnout_kgN'        ! (kg N)
          character(len=15) :: orgp_in =   '     orgpin_kgP'        ! (kg P)
          character(len=15) :: orgp_out =  '    orgpout_kgP'        ! (kg P)
          character(len=15) :: no3_in =    '      no3in_kgN'        ! (kg N)
          character(len=15) :: no3_out =   '     no3out_kgN'        ! (kg N)
          character(len=15) :: nh4_in =    '       nh4in_kg'        ! (kg)
          character(len=15) :: nh4_out=    '      nh4out_kg'        ! (kg)
          character(len=15) :: no2_in =    '       no2in_kg'        ! (kg)
          character(len=15) :: no2_out =   '      no2out_kg'        ! (kg)
          character(len=15) :: solp_in =   '     solpin_kgP'        ! (kg P)
          character(len=15) :: solp_out =  '    solpout_kgP'        ! (kg P)
          character(len=15) :: chla_in =   '      chlain_kg'        ! (kg)
          character(len=15) :: chla_out =  '     chlaout_kg'        ! (kg)
          character(len=15) :: cbod_in =   '      cbodin_kg'        ! (kg)
          character(len=15) :: cbod_out =  '     cbodout_kg'        ! (kg)
          character(len=15) :: dis_in =    '       disin_kg'        ! (kg)
          character(len=15) :: dis_out =   '      disout_kg'        ! (kg)
          character(len=15) :: solpst_in = '  solpsti_mgpst'        ! (mg pst)
          character(len=15) :: solpst_out ='  solpsto_mgpst'        ! (mg pst)
          character(len=15) :: sorbpst_in =' sorbpsti_mgpst'        ! (mg pst)
          character(len=15) :: sorbpst_out=' sorbpsto_mgpst'        ! (mg pst)
          character(len=15) :: react =     '    react_mgpst'        ! (mg pst)
          character(len=15) :: volat =     '       volat_mg'        ! (mg)
          character(len=15) :: setlpst =   '  setlpst_mgpst'        ! (mg pst)
          character(len=15) :: resuspst =  '    resuspst_mg'        ! (mg)
          character(len=15) :: difus =     '    difus_mgpst'        ! (mg pst)              
          character(len=15) :: reactb =    '   reactb_sedmg'        ! pst/sed (mg)
          character(len=15) :: bury =      '    bury_burymg'        ! pst bury (mg)
          character(len=15) :: sedpest =   '     sedpest_mg'        ! pst in rivbed sed mg
          character(len=15) :: bacp =      '           bacp'        ! persistent bact out
          character(len=15) :: baclp =     '          baclp'        ! lpersistent bact out
          character(len=15) :: met1 =      '           met1'        ! cmetal #1  
          character(len=15) :: met2 =      '           met2'        ! cmetal #2
          character(len=15) :: met3 =      '           met3'        ! cmetal #3
          character(len=15) :: sand_in =   '        sand_in'        ! sand in 
          character(len=15) :: sand_out =  '       sand_out'        ! sand out
          character(len=15) :: silt_in =   '        silt_in'        ! silt_in
          character(len=15) :: silt_out =  '       silt_out'        ! silt_out
          character(len=15) :: clay_in =   '        clay_in'        ! clay_in
          character(len=15) :: clay_out =  '       clay_out'        ! clay_out
          character(len=15) :: smag_in =   '        smag_in'        ! sm ag in  
          character(len=15) :: smag_out =  '       smag_out'        ! sm ag out
          character(len=15) :: lag_in =    '         lag_in'        ! lg ag in
          character(len=15) :: lag_out =   '        lag_out'        ! lg ag out
          character(len=15) :: grvl_in =   '        grvl_in'        ! gravel in
          character(len=15) :: grvl_out =  '       grvl_out'        ! gravel out
          character(len=15) :: bnk_ero =   '        bnk_ero'        ! bank erosion
          character(len=15) :: ch_deg =    '         ch_deg'        ! channel degradation
          character(len=15) :: ch_dep =    '         ch_dep'        ! channel deposition
          character(len=15) :: fp_dep =    '         fp_dep'        ! flood deposition
          character(len=15) :: tot_ssed =  '       tot_ssed'        ! total suspended sediments
      end type ch_header
      type (ch_header) :: ch_hdr
      interface operator (+)
        module procedure ch_add
      end interface
      
      interface operator (/)
        module procedure ch_div
      end interface
        
      interface operator (*)
        module procedure ch_mult
      end interface 
             
      contains
!! routines for channel module
      include 'channel_control.f'
      include 'channel_output.f'
      include 'channel_allo.f'
      include 'ch_rchinit.f'                  
      include 'ch_rtday.f'                    
      include 'ch_rtmusk.f'                   
      include 'ch_rthr.f'                     
      include 'ch_rthmusk.f'                  
      include 'ch_rtsed.f'                    
      include 'ch_rtsed_bagnold.f'            
      include 'ch_rtsed_kodatie.f'            
      include 'ch_rtsed_yangsand.f'          
      include 'ch_rtsed_Molinas_Wu.f'        
      include 'ch_hhnoqual.f'                  
      include 'ch_hhwatqual.f'                 
      include 'ch_rthsed.f'                    
      include 'ch_noqual.f'                    
      include 'ch_watqual2.f'                  
      include 'ch_watqual.f'                   
      include 'ch_rtpest.f'                    
      include 'ch_rthpest.f'                   
      include 'ch_rtbact.f'                    
      include 'ch_irr_rch.f'                   
      include 'ch_rchuse.f'                    
      include 'ch_biofilm.f'    !!!!! call is commented             
 !     include 'ch_ttcoef.f'
      include 'ch_rtout.f' 
      include 'ch_initial.f'

      function ch_add(cho1,cho2) result (cho3)
      type (ch_output),  intent (in) :: cho1
      type (ch_output),  intent (in) :: cho2
      type (ch_output) :: cho3
       cho3%flo_in = cho1%flo_in + cho2%flo_in
       cho3%flo_out = cho1%flo_out + cho2%flo_out
       cho3%evap = cho1%evap + cho2%evap   
       cho3%tloss = cho1%tloss + cho2%tloss  
       cho3%sed_in = cho1%sed_in + cho2%sed_in
       cho3%sed_out = cho1%sed_out + cho2%sed_out
       cho3%sed_conc = cho1%sed_conc + cho2%sed_conc     
       cho3%orgn_in =cho1%orgn_in + cho2%orgn_in
       cho3%orgn_out = cho1%orgn_out + cho2%orgn_out            
       cho3%orgp_in = cho1%orgp_in + cho2%orgp_in
       cho3%orgp_out = cho1%orgp_out + cho2%orgp_out
       cho3%no3_in = cho1%no3_in + cho2%no3_in
       cho3%no3_out = cho1%no3_out + cho2%no3_out
       cho3%nh4_in = cho1%nh4_in + cho2%nh4_in
       cho3%nh4_out = cho1%nh4_out + cho2%nh4_out
       cho3%no2_in = cho1%no2_in + cho2%no2_in
       cho3%no2_out = cho1%no2_out + cho2%no2_out
       cho3%solp_in = cho1%solp_in + cho2%solp_in
       cho3%solp_out = cho1%solp_out + cho2%solp_out
       cho3%chla_in = cho1%chla_in + cho2%chla_in
       cho3%chla_out = cho1%chla_out + cho2%chla_out
       cho3%cbod_in = cho1%cbod_in + cho2%cbod_in
       cho3%cbod_out = cho1%cbod_out + cho2%cbod_out
       cho3%dis_in = cho1%dis_in + cho2%dis_in
       cho3%dis_out = cho1%dis_out + cho2%dis_out
       cho3%solpst_in = cho1%solpst_in + cho2%solpst_in
       cho3%solpst_out = cho1%solpst_out + cho2%solpst_out
       cho3%sorbpst_in = cho1%sorbpst_in + cho2%sorbpst_in
       cho3%sorbpst_out = cho1%sorbpst_out + cho2%sorbpst_out         
       cho3%react = cho1%react + cho2%react
       cho3%volat = cho1%volat + cho2%volat
       cho3%setlpst = cho1%setlpst + cho2%setlpst
       cho3%resuspst = cho1%resuspst + cho2%resuspst
       cho3%difus = cho1%difus + cho2%difus
       cho3%reactb = cho1%reactb + cho2%reactb
       cho3%bury = cho1%bury + cho2%bury
       cho3%sedpest = cho1%sedpest + cho2%sedpest
       cho3%bacp = cho1%bacp + cho2%bacp
       cho3%baclp = cho1%baclp + cho2%baclp
       cho3%met1 = cho1%met1 + cho2%met1
       cho3%met2 = cho1%met2 + cho2%met2
       cho3%met3 = cho1%met3 + cho2%met3
       cho3%sand_in = cho1%sand_in + cho2%sand_in           
       cho3%sand_out = cho1%sand_out + cho2%sand_out
       cho3%silt_in = cho1%silt_in + cho2%silt_in
       cho3%silt_out = cho1%silt_out + cho2%silt_out
       cho3%clay_in = cho1%clay_in + cho2%clay_in
       cho3%clay_out = cho1%clay_out + cho2%clay_out
       cho3%smag_in = cho1%smag_in + cho2%smag_in
       cho3%smag_out = cho1%smag_out + cho2%smag_out
       cho3%lag_in = cho1%lag_in + cho2%lag_in
       cho3%lag_out = cho1%lag_out + cho2%lag_out
       cho3%grvl_in = cho1%grvl_in + cho2%grvl_in
       cho3%grvl_out = cho1%grvl_out + cho2%grvl_out
       cho3%bnk_ero = cho1%bnk_ero + cho2%bnk_ero
       cho3%ch_deg = cho1%ch_deg + cho2%ch_deg
       cho3%fp_dep = cho1%fp_dep + cho2%fp_dep
       cho3%tot_ssed = cho1%tot_ssed + cho2%tot_ssed
      end function
      
      function ch_div (ch1,const) result (ch2)
        type (ch_output), intent (in) :: ch1
        real, intent (in) :: const
        type (ch_output) :: ch2
        consta = time%nbyr
        ch2%flo_in = ch1%flo_in / consta
        ch2%flo_out = ch1%flo_out / consta
        ch2%evap = ch1%evap / consta  
        ch2%tloss = ch1%tloss / consta  
        ch2%sed_in = ch1%sed_in / consta 
        ch2%sed_out = ch1%sed_out / consta           
        ch2%sed_conc = ch1%sed_conc / consta            
        ch2%orgn_in = ch1%orgn_in / consta  
        ch2%orgn_out = ch1%orgn_out / consta              
        ch2%orgp_in = ch1%orgp_in / consta  
        ch2%orgp_out = ch1%orgp_out / consta            
        ch2%no3_in = ch1%no3_in / consta
        ch2%no3_out = ch1%no3_out / consta                    
        ch2%nh4_in = ch1%nh4_in / consta  
        ch2%nh4_out = ch1%nh4_out / consta               
        ch2%no2_in = ch1%no2_in / consta 
        ch2%no2_out = ch1%no2_out / consta                      
        ch2%solp_in = ch1%solp_in / consta        
        ch2%solp_out = ch1%solp_out / consta                   
        ch2%chla_in = ch1%chla_in / consta   
        ch2%chla_out = ch1%chla_out / consta                  
        ch2%cbod_in = ch1%cbod_in / consta    
        ch2%cbod_out = ch1%cbod_out / consta                   
        ch2%dis_in = ch1%dis_in / consta      
        ch2%dis_out = ch1%dis_out / consta                    
        ch2%solpst_in = ch1%solpst_in / consta    
        ch2%solpst_out = ch1%solpst_out / consta                
        ch2%sorbpst_in = ch1%sorbpst_in / consta   
        ch2%sorbpst_out = ch1%sorbpst_out / consta                  
        ch2%react = ch1%react / consta                                
        ch2%volat = ch1%volat / consta                         
        ch2%setlpst = ch1%setlpst / consta                            
        ch2%resuspst = ch1%resuspst / consta                          
        ch2%difus = ch1%difus / consta                               
        ch2%reactb = ch1%reactb / consta                               
        ch2%bury = ch1%bury / consta                                   
        ch2%sedpest = ch1%sedpest / consta
        ch2%bacp = ch1%bacp / consta                        
        ch2%baclp = ch1%baclp / consta                       
        ch2%met1 = ch1%met1 / consta                         
        ch2%met2 = ch1%met2 / consta                         
        ch2%met3 = ch1%met3 / consta                         
        ch2%sand_in = ch1%sand_in / consta          
        ch2%sand_out = ch1%sand_out / consta                         
        ch2%silt_in = ch1%silt_in / consta          
        ch2%silt_out = ch1%silt_out / consta                       
        ch2%clay_in = ch1%clay_in / consta             
        ch2%clay_out = ch1%clay_out / consta                        
        ch2%smag_in = ch1%smag_in / consta            
        ch2%smag_out = ch1%smag_out / consta                       
        ch2%lag_in = ch1%lag_in / consta          
        ch2%lag_out = ch1%lag_out / consta                      
        ch2%grvl_in = ch1%grvl_in / consta        
        ch2%grvl_out = ch1%grvl_out / consta                      
        ch2%bnk_ero = ch1%bnk_ero / consta
        ch2%ch_deg = ch1%ch_deg / consta
        ch2%fp_dep = ch1%fp_dep / consta
        ch2%tot_ssed = ch1%tot_ssed / consta
      end function ch_div
      
      function ch_mult (const, chn1) result (chn2)
        type (ch_output), intent (in) :: chn1
        real, intent (in) :: const
        type (ch_output) :: chn2
        chn2%flo_in = consta * chn1%flo_in      
        chn2%flo_out = consta * chn1%flo_out
        chn2%evap = consta * chn1%evap
        chn2%tloss = consta * chn1%tloss
        chn2%sed_in = consta * chn1%sed_in
        chn2%sed_out = consta * chn1%sed_out
        chn2%sed_conc = consta * chn1%sed_conc      
        chn2%orgn_in = consta * chn1%orgn_in
        chn2%orgn_out = consta * chn1%orgn_out
        chn2%orgp_in = consta * chn1%orgp_in
        chn2%orgp_out = consta * chn1%orgp_out
        chn2%no3_in = consta * chn1%no3_in
        chn2%no3_out = consta * chn1%no3_out    
        chn2%nh4_in = consta * chn1%nh4_in
        chn2%nh4_out = consta * chn1%nh4_out
        chn2%no2_in = consta * chn1%no2_in
        chn2%no2_out = consta * chn1%no2_out
        chn2%solp_in = consta * chn1%solp_in
        chn2%solp_out = consta * chn1%solp_out      
        chn2%chla_in = consta * chn1%chla_in
        chn2%chla_out = consta * chn1%chla_out
        chn2%cbod_in = consta * chn1%cbod_in
        chn2%cbod_out = consta * chn1%cbod_out
        chn2%dis_in = consta * chn1%dis_in
        chn2%dis_out = consta * chn1%dis_out     
        chn2%solpst_in = consta * chn1%solpst_in
        chn2%solpst_out = consta * chn1%solpst_out
        chn2%sorbpst_in = consta * chn1%sorbpst_in
        chn2%sorbpst_out = consta * chn1%sorbpst_out
        chn2%react = consta * chn1%react
        chn2%bury = consta * chn1%bury      
        chn2%sedpest = consta * chn1%sedpest
        chn2%bacp = consta * chn1%bacp
        chn2%baclp = consta * chn1%baclp
        chn2%met1 = consta * chn1%met1
        chn2%met2 = consta * chn1%met2
        chn2%met3 = consta * chn1%met3     
        chn2%sand_in = consta * chn1%sand_in
        chn2%sand_out = consta * chn1%sand_out
        chn2%silt_in = consta * chn1%silt_in
        chn2%silt_out = consta * chn1%silt_out
        chn2%clay_in = consta * chn1%clay_in
        chn2%clay_out = consta * chn1%clay_out      
        chn2%smag_in = consta * chn1%smag_in
        chn2%smag_out = consta * chn1%smag_out
        chn2%lag_in = consta * chn1%lag_in
        chn2%lag_out = consta * chn1%lag_out
        chn2%grvl_in = consta * chn1%grvl_in
        chn2%grvl_out = consta * chn1%grvl_out   
        chn2%bnk_ero = consta * chn1%bnk_ero
        chn2%ch_deg = consta * chn1%ch_deg
        chn2%fp_dep = consta * chn1%fp_dep
        chn2%tot_ssed = consta * chn1%tot_ssed     
      end function ch_mult
               
      end module channel_module