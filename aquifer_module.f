      module aquifer_module

      use hydrograph_module
      use jrw_datalib_module
      use jrw_process_module

      integer :: iaq, iaqdb, maqu_sp, msh_aqu, msh_aqp
      real :: rchrg1
      real, dimension (:), allocatable :: rchrg      !! mm H2O     |amt of water entergin shallow aquifer
      real, dimension (:), allocatable :: rchrg_n    !! mm H2O     |amt of nitrate getting to the shallow aquifer
      real, dimension (:), allocatable :: no3gw      !! kg N/ha    |nitrate loading to reach in groundwater
      real, dimension (:), allocatable :: gw_qdeep   !! mm H2O     |groundwater contribution to streamflow from deep aquifer
      real, dimension (:), allocatable :: gw_delaye  !! days       |groundwater delay (time required for water leaving the
                                                     !!              bottom of the root zone to reach shallow aquifer
      real, dimension (:), allocatable :: alpha_bfe  !!            |Exp(-alpha_bf(:)) 
      real, dimension (:), allocatable :: gw_nloss   !!
            
      type aquifer_dynamic
        real :: flo = 0.       !m^3       |flow from aquifer in current time step       
        real :: stor = 0.      !          |water storage in aquifer 
        real :: hgt = 0.       !m         |groundwater height
        real :: no3 = 0.       !ppm NO3-N |nitrate-N concentration in aquifer
        real :: minp = 0.      !kg        |mineral phosphorus from aquifer on current timestep    
        real :: orgn = 0.  
        real :: orgp = 0.   
        real :: rchrg = 0.     !m^3       |recharge 
        real :: rchrg_n = 0.   !          |amount of nitrate getting to the shallow aquifer  
        real :: perc = 0.  
        real :: nloss = 0. 
      end type aquifer_dynamic
      type (aquifer_dynamic), dimension(:), allocatable :: aqu
      type (aquifer_dynamic), dimension(:), allocatable, save :: aqu_m
      type (aquifer_dynamic), dimension(:), allocatable, save :: aqu_y
      type (aquifer_dynamic), dimension(:), allocatable, save :: aqu_a
      type (aquifer_dynamic) :: aquz
      

      type aqu_header
          character (len=6) :: yrs =       ' time '
          character (len=6) :: yrc =       ' year '
          character (len=8) :: isd =       '   unit '
          character(len=15) :: flo =       '         flo_mm'          ! (mm)
          character(len=15) :: stor =      '        stor_mm'          ! (mm)
          character(len=15) :: hgt =       '       height_m'          ! (m)
          character(len=15) :: no3 =       '     no3_kgN/ha'          ! (kg/ha N)
          character(len=15) :: sed_in =    '    minp_kgP/ha'          ! (kg/ha P)
          character(len=15) :: sed_out=    '    orgn_kgN/ha'          ! (kg/ha N)
          character(len=15) :: sed_conc =  '    orgp_kgP/ha'          ! (kg/ha P)
          character(len=15) :: orgn_in =   '       rchrg_mm'          ! (mm)
          character(len=15) :: orgn_out =  '  rchrgn_kgN/ha'          ! (kg/ha N)
          character(len=15) :: orgp_in =   '        perc_mm'          ! (mm)
          character(len=15) :: orgp_out =  '   nloss_kgN/ha'          ! (kg/ha N)
      end type aqu_header
      type (aqu_header) :: aqu_hdr
      interface operator (+)
        module procedure aqu_add
      end interface
      
      interface operator (/)
        module procedure aqu_div
      end interface
        
      contains
        
      !! routines for shallow aquifer module
      include 'aqu_initial.f'
      include 'aqu_1dlag.f'
      include 'aqu_hyds.f'
      include 'aquifer_output.f'

      
      function aqu_add(aqo1,aqo2) result (aqo3)
      type (aquifer_dynamic),  intent (in) :: aqo1
      type (aquifer_dynamic),  intent (in) :: aqo2
      type (aquifer_dynamic) :: aqo3
       aqo3%flo = aqo1%flo + aqo2%flo
       aqo3%hgt = aqo1%hgt + aqo2%hgt
       aqo3%no3 = aqo1%no3 + aqo2%no3   
       aqo3%minp = aqo1%minp + aqo2%minp  
       aqo3%orgn = aqo1%orgn + aqo2%orgn
       aqo3%orgp = aqo1%orgp + aqo2%orgp
       aqo3%rchrg = aqo1%rchrg + aqo2%rchrg     
       aqo3%rchrg_n = aqo1%rchrg_n + aqo2%rchrg_n
       aqo3%perc = aqo1%perc + aqo2%perc           
       aqo3%nloss = aqo1%nloss + aqo2%nloss
      end function aqu_add
      
      function aqu_div (aq1,const) result (aq2)
        type (aquifer_dynamic), intent (in) :: aq1
        real, intent (in) :: const
        type (aquifer_dynamic) :: aq2
        consta = time%nbyr
        aq2%flo = aq1%flo / consta
        aq2%hgt = aq1%hgt / consta
        aq2%no3 = aq1%no3 / consta  
        aq2%minp = aq1%minp / consta  
        aq2%orgn = aq1%orgn / consta 
        aq2%orgp = aq1%orgp / consta           
        aq2%rchrg = aq1%rchrg / consta            
        aq2%rchrg_n = aq1%rchrg_n / consta  
        aq2%perc = aq1%perc / consta              
        aq2%nloss = aq1%nloss / consta  
      end function aqu_div
        
      end module aquifer_module