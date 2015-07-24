      module hru_module

        use parm
        use jrw_datalib_module
        use jrw_process_module
        use climate_parms
        use time_module
        use hydrograph_module
        use reservoir_module
        
        use plant_module
        use mgtops_module
        use surfrunoff_module
        use erosion_module
        use pesticide_module
        use nutrient_module
        use evapotrans_module
        use structural_mgt_practices_module
        use septic_module
        use soil_temperature_module
        use surfstore_module
 !!       use carbon_module
        use other_module
        use output_landscape_module

      contains
        include 'hru_control.f'
        include 'bac_apply_hrucon.f'
        include 'erfc.f'
        include 'hru_soiltest_update.f'
        include 'hru_soil_chem.f'
        include 'hru_soil_phys.f'
        include 'hru_sweep.f'
        include 'hru_urbanhr.f'    !!! put in urban module
        include 'hru_urban.f'      !!!  "   " 
        include 'hru_urb_bmp.f'    !!!  "   " 
        include 'hru_output.f'
        include 'hru_read.f'
        include 'bac_hrucontrol.f'
        include 'bac_lsinit_read.f'
        include 'pst_lsinit_read.f'
        include 'hru_soil_assign.f'
        include 'rls_routesurf.f'
        
        !! removing modules within hru module
        include 'rls_routesoil.f'
        include 'swr_depstor.f'
        include 'swr_drains.f'
        include 'swr_percmacro.f'
        include 'swr_percmain.f'
        include 'swr_percmicro.f'
        include 'swr_satexcess.f'
        include 'swr_substor.f'
        include 'swr_latsed.f'
        include 'swr_subwq.f'
        include 'swr_origtile.f'

      end module hru_module