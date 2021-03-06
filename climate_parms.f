      module climate_parms
      
      integer :: mwst, ifirsts, ifirsth, ifirstw, ifirstpet, mwgn
 !     real, dimension (:), allocatable :: rmeas
 !    real, dimension (:), allocatable :: txmeas
 !     real, dimension (:), allocatable :: tnmeas
 !     real, dimension (:), allocatable :: slrmeas
 !     real, dimension (:), allocatable :: rhmeas
 !     real, dimension (:), allocatable :: wndmeas
      real, dimension (:,:), allocatable :: frad
      integer, dimension (:), allocatable :: elevp,elevt
      integer, dimension (:), allocatable :: idg, wst_pointer
      integer, dimension (:,:), allocatable :: rndseed
      real, dimension (:), allocatable :: rnd2,rnd3,rnd8,rnd9
      integer, dimension (:), allocatable :: ifirstt,ifirstpcp
      !! this should go in the weather module
           
      type weather_generator_db
        character(len=2) :: state                 !! state of weather station
        character(len=10) :: station              !! station number of weather station
        character(len=8) :: lstation              !! station number
        integer :: id                             !! station id number
        real :: lat =  0.0                        !! degrees       |latitude of weather station used to compile data
        real :: long = 0.0                        !! degrees       |longitude of weather station 
        real :: elev = 0.0                        !!               |elevation of weather station used to compile weather generator data
        real :: rain_yrs = 10.0                   !! none          |number of years of recorded maximum 0.5h rainfall used to calculate values for rainhhmx(:)
        real, dimension (12) :: tmpmx = (/-0.032,2.9,9.95,17.78,23.73,
     &        28.74,30.28,28.91,25.85,19.39,10.81,2.96/)      !! deg C  |avg monthly maximum air temperature
        real, dimension (12) :: tmpmn = (/-9.94,-7.31,-1.23,4.73,10.36,
     &        15.47,17.67,16.23,12.34,6.23,0.39,-6.06/)       !! deg C  |avg monthly minimum air temperature
        real, dimension (12) :: tmpstdmx = (/6.76,6.63,6.88,6.12,5.33,
     &        3.74,3.21,3.27,4.36,5.76,6.39,6.53/)            !! deg C  |standard deviation for avg monthly maximum air temperature 
        real, dimension (12) :: tmpstdmn = (/7.74,7.39,5.84,5.34,5.01,
     &        3.96,3.49,3.79,5.02,5.52,5.69,7.21/)            !! deg C  |standard deviation for avg monthly minimum air temperature
        real, dimension (12) :: pcpmm = (/49.6,38.8,74.0,94.3,72.9,90.7,
     &        104.6,91.1,79.6,57.9,57.2,71.5/)                !! mm     |amount of precipitation in month
        real, dimension (12) :: pcpstd = (/8.9,7.6,9.4,13.7,8.6,12.7,
     &        17.8,17.5,12.7,13.5,8.9,11.9/)                  !! mm/day |standard deviation for the average daily
        real, dimension (12) :: pcpskw = (/0.85,1.87,0.61,1.0,-1.26,
     &        0.01,0.71,1.68,0.09,2.28,0.88,2.51/)            !! none   |skew coefficient for the average daily precipitation
        real, dimension (12) :: pr_wd = (/0.17,0.18,0.21,0.22,0.18,0.20,
     &        0.18,0.19,0.18,0.14,0.19,0.22/)                 !! none   |probability of wet day after dry day in month 
        real, dimension (12) :: pr_ww = (/0.36,0.36,0.45,0.42,0.48,0.41,
     &        0.34,0.32,0.37,0.36,0.38,0.42/)                 !! none   |probability of wet day after wet day in month
        real, dimension (12) :: pcpd = (/6.51,6.37,8.57,8.25,7.97,7.59,
     &        6.64,6.77,6.67,5.56,7.04,8.52/)                 !! days   |average number of days of precipitation in the month
        real, dimension (12) :: rainhmx = (/11.4,11.2,13.2,29.2,19.3,
     &        51.3,45.0,41.7,27.2,12.7,8.4,8.1/)              !! mm     |maximum 0.5 hour rainfall in month
        real, dimension (12) :: solarav = (/5.61,8.34,12.15,16.05,19.94,
     &        21.79,22.0,19.57,16.13,11.27,7.12,4.99/)        !! MJ/m^2/day    |average daily solar radiation for the month
        real, dimension (12) :: dewpt = (/-5.89,-5.05,-1.29,4.51,9.05,
     &        15.45,17.81,17.38,12.52,6.71,0.04,-4.49/)       !! deg C  |average dew point temperature for the month
        real, dimension (12) :: windav = (/5.39,5.43,5.78,5.79,5.02,
     &        4.33,3.71,3.51,3.97,4.37,5.18,5.21/)            !! m/s    |average wind speed for the month
        character (len=15) :: station_name
      end type weather_generator_db
      type (weather_generator_db), dimension(:),allocatable :: wgn
      type (weather_generator_db), dimension(:),allocatable :: wgn_orig
      type (weather_generator_db), dimension(:),allocatable :: fwgn
      
      type wgn_parms
        real, dimension (12) :: pr_wdays = 0.   !! none   |proportion of wet days in a month
        real, dimension (12) :: pcpmean = 0.    !! mm/day |average amount of precipitation falling in one day for the month
        real :: daylmn = 0.                     !!               |minimum day length
        real :: daylth = 0.                     !!               |day length threshhold to trigger dormancy
        real :: latsin = 0.                     !!               |sine of latitude
        real :: latcos = 0.                     !!               |cosine of latitude
        real :: phutot = 0.                     !!               |total base zero heat units for year
        real :: pcpdays = 0.                    !!               |days of precip in year
        real :: tmp_an = 0.                     !!               |average annual air temperature
        real :: pcp_an = 0.                     !!               |average annual precipitation
        real, dimension (12) :: pcf = 0.        !!               |normalization factor for precipitation
        real, dimension (12) :: amp_r = 0.      !!               |alpha factor for rain(mo max 0.5h rain)
        integer :: ireg = 0                     !!               |annual precip category-1 <= 508 mm; 2 > 508 and <= 1016 mm; 3 > 1016 mm/yr
      end type wgn_parms
      type (wgn_parms), dimension(:),allocatable :: wgn_pms
          
      type wind_direction_db
        character(len=16) :: name = 'default-uniform'
        real, dimension (12,16) :: dir = 1.     !! 1-16         |avg monthly wind direstion
      end type wind_direction_db
      type (wind_direction_db), dimension(:),allocatable :: wnd_dir
      
      type weather_daily
        real :: precip
        real :: tmax
        real :: tmin
        real :: tave
        real :: solrad
        real :: solradmx
        real :: rhum
        real :: dewpt
        real :: windsp
        real :: pet
        real :: wndir
      end type weather_daily
            
      type weather_codes_station
        integer :: wgn = 1        !!  weather generator station number
        integer :: pcpsim = 2     !!  rainfall input code
        integer :: tmpsim = 2     !!  temperature input code
                                  !!   1 daily max/min read for each subbasin
                                  !!   2 daily max/min simulated for each subbasin  
        integer :: rhsim = 2      !!  relative humidity input code
                                  !!   1 measured data read for each subbasin
                                  !!   2 data simulated for each subbasin
        integer :: slrsim = 2     !!  solar radiation input code
                                  !!   1 measured data read for each subbasin
                                  !!   2 data simulated for each subbasin
        integer :: wndsim = 2     !!  windspeed input code
                                  !!   1 measured data read for each subbasin
                                  !!   2 data simulated for each subbasin
        integer :: pgage = 0      !!  gage number for rainfall 
        integer :: tgage = 0      !!  gage number fo temperature
        integer :: sgage = 0      !!  gage number for solar radiation
        integer :: hgage = 0      !!  gage number for relative humidity
        integer :: wgage = 0      !!  gage number for windspeed
        integer :: wndir = 0      !!  number of wind direction gage (.dir) files used in sim
        integer :: atmodep = 0    !!  atmospheric depostion data file locator
      end type weather_codes_station
      
      type weather_station
        character(len=16) :: name = "Farmer Branch IL"
        real :: lat                          ! degrees    |latitude
        integer :: mseas_dayf                ! julian day |first day of monsoon season to initiate growth 
                                             !            |of tropical plants
        integer :: mseas_daye                ! julian day |ending day of monsoon season to initiate growth 
                                             !            |of tropical plants
        type (weather_codes_station) :: wco 
        type (weather_daily) :: weat
        real, dimension(12) :: rfinc = 0     ! deg C      |monthly precipitation adjustment
        real, dimension(12) :: tmpinc = 0    ! deg C      |monthly temperature adjustment
        real, dimension(12) :: radinc = 0    ! MJ/m^2     |monthly solar radiation adjustment
        real, dimension(12) :: huminc = 0    ! none       |monthly humidity adjustment
      end type weather_station
      type (weather_station), dimension(:),allocatable :: wst
         
      type climate_measured_data
        character (len=16) :: name
        real :: lat                !! latitude of raingage         
        real :: long               !! longitude of raingage
        real :: elev               !! elevation of raingage
        integer :: nbyr            !! number of years of daily rainfall
        character (len=13) :: filename
        real :: tstep              !! timestep of precipitation
        real, dimension (:,:), allocatable :: ts
        real, dimension (:,:), allocatable :: ts2
        real, dimension (:,:,:), allocatable :: tss
      end type climate_measured_data
      type (climate_measured_data), dimension(:), allocatable :: pcp
      type (climate_measured_data), dimension(:), allocatable :: tmp
      type (climate_measured_data), dimension(:), allocatable :: slr
      type (climate_measured_data), dimension(:), allocatable :: hmd
      type (climate_measured_data), dimension(:), allocatable :: wnd
      
      end module climate_parms