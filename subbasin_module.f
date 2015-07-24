      module subbasin_module

      use parm
      use sd_hru_module
      use hydrograph_module
      use output_landscape_module
      use jrw_datalib_module
      use channel_module

      integer :: isub, msub_db
      real, dimension (:), allocatable :: sub_tc, sub_n
      real, dimension (:,:), allocatable :: uhs
    
      type subbasin_parameters
        character(len=16) :: name = ""
        real :: da_km2 = 0.      !! km2      drainage area
        real :: lat = 0.         !!          latitude
        real :: long = 0.        !!          long
        integer :: topo_db = 0.  !!          topo.dat file pointer
      end type subbasin_parameters
      type (subbasin_parameters), dimension(:), allocatable :: sub

      contains
!! routines for routing unit module
      include 'subbasin_control.f'
      include 'sub_read.f'
      include 'sub_allo.f'
      include 'subbasin_output.f'

      end module subbasin_module