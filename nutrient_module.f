      module nutrient_module

      use parm
      use jrw_datalib_module
      use time_module
    
      contains
!! routines for nutrient cycling module
      include 'nut_psed.f'
      include 'nut_nrain.f'
      include 'nut_nlch.f'
      include 'nut_solp.f'
      include 'nut_nminrl.f'
      include 'nut_nitvol.f'
      include 'nut_pminrl.f'
      include 'nut_pminrl2.f'
      include 'nut_denit.f'
      include 'nut_orgn.f'
      include 'nut_orgnc.f'
   
      end module nutrient_module