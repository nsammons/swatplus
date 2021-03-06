      module sd_hru_module
      
      use hydrograph_module
      use time_module
      use climate_module
      use output_landscape_module
      use input_file_module
    
      real :: a1, a2
      real :: precip, snowfall, snowmelt, runoff, flowlat, chflow, perc
      real :: tmax, tmin, tave, raobs, tstress, ws, biomass
      real :: cn_sd, aet, pet, sedin
      real, dimension(:), allocatable :: sd_qday, sd_qfdc, pr, be
      real, dimension(12) :: awct = 0.
      real, dimension(12) :: port = 0.
      real, dimension(12) :: scon = 0.
        
      type swatdeg_hru_data
        character(len=13) :: name
        real :: dakm2 = 0.          !km^2          |drainage area
        real :: cn2 = 0.            !none          |condition II curve number
        real :: tc = 0.             !min           |time of concentration
        real :: soildep = 0.        !mm            |soil profile depth
        real :: slope = 0.          !m/m           |land surface slope
        real :: slopelen = 0.       !m             |land surface slope length
        real :: sy = 0.             !mm            |specific yld of the shallow aquifer
        real :: abf = 0.            !              |alpha factor groundwater
        real :: revapc = 0.         !              |revap coefficient amt of et from shallow aquifer
        real :: percc = 0.          !              |percolation coeff from shallow to deep
        real :: sw = 0.             !frac          |initial soil water (frac of awc)
        real :: gw = 0.             !mm            |initial shallow aquifer storage
        real :: gwflow = 0.         !mm            |initial shallow aquifer flow
        real :: gwdeep = 0.         !mm            |initital deep aquifer flow
        real :: snow = 0.           !mm            |initial snow water equivalent
        real :: xlat = 0.           !              |latitude
        integer :: itext = 0        !              |soil texture
                                    !              |1-sand 2-loamy sand 3-sandy loam 4-loam
                                    !              |5-silt loam 6-silt 7-silty clay 8-clay loam
                                    !              |9-sandy clay loam 10-sandy clay 
                                    !              |11-silty clay 12=clay  
        integer :: igrow1 = 0       !julian day    |start of growing season
        integer :: igrow2 = 0       !julian day    |end of growing season
        integer :: icrop = 0        !              |plant type (as listed in plants.plt)
        integer :: ipet = 0         !              |potential ET method
        integer :: irr = 0          !              |irrigation code 0=no irr 1=irrigation
        integer :: irrsrc = 0       !              |irrigation source 0=outside basin 1=shal aqu 2=deep
        real :: uslek = 0.          !              |usle soil erodibility factor
        real :: uslec = 0.          !              |usle cover factor
        real :: uslep = 0.          !none          |USLE equation support practice (P) factor
        real :: uslels = 0.         !none          |USLE equation length slope (LS) factor
      end type swatdeg_hru_data
      type (swatdeg_hru_data), dimension (:), allocatable :: sd_db
      
      type swatdeg_hru_dynamic
        character(len=13) :: name
        integer :: hydno
        real :: uslefac = 0.                  !             |USLE slope length factor
        real :: s1 = 0.
        real :: s3 = 0.
        real :: yls = 0.
        real :: ylc = 0.
        real :: awc = 0.                      !mm/mm        |available water capacity of soil 
        real :: g = 0.
        real :: hufh = 0.
        real :: phu = 0.     
        real :: por = 0.
        real :: sc = 0.
        real :: sw = 0.                      !mm/mm         |initial soil water storage
        real :: gw = 0.                      !mm            |initial shallow aquifer storage
        real :: snow = 0.                    !mm            |initial water content of snow
        real :: gwflow = 0.                  !mm            |initial groundwater flow
        integer :: igro = 0                  !              |0=plant growing; 1=not growing
        real :: dm = 0.                      !t/ha          |plant biomass
        real :: alai = 0.                    !              |initial leaf area index
        real :: gwdeep = 0.                  !mm            |deep aquifer storage
      end type swatdeg_hru_dynamic
      type (swatdeg_hru_dynamic), dimension (:), allocatable :: sd
               
      contains
!! routines for swatdeg_hru module
      include 'sd_hru_read.f'
      include 'sd_hru_control.f'
      include 'sd_hru_output.f'

      end module sd_hru_module