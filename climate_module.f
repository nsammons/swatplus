      module climate_module

      use parm
      use climate_parms
      use time_module
      use hydrograph_module
       
      contains
!! routines for climate module
      include 'cli_pmeas.f'
      include 'cli_tmeas.f'
      include 'cli_smeas.f'
      include 'cli_hmeas.f'
      include 'cli_pgen.f'
      include 'cli_weatgn.f'
      include 'cli_tgen.f'
      include 'cli_clgen.f'
      include 'cli_slrgen.f'
      include 'cli_wndgen.f'
      include 'cli_pgenhr.f'
      include 'cli_dstn1.f'
      include 'cli_wmeas.f'
      include 'cli_tair.f'
      include 'cli_rhgen.f'
      include 'climate_control.f'
 !     include 'cli_readwgn.f'
      include 'cli_initwgn.f'
      include 'cli_staread.f'
      include 'cli_wgnread.f'
 !     include 'gcycl.f'
 !     include 'cli_forecast_read.f'
              
      end module climate_module