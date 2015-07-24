      module evapotrans_module

      use parm
      use jrw_datalib_module
    
      contains
!! routines for evapotranspiration module
      include 'et_act.f'
      include 'et_pot.f'

      end module evapotrans_module