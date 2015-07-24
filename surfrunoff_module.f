      module surfrunoff_module

      use parm
      use jrw_datalib_module
    
      contains
!! routines for surface runoff module
      include 'sq_canopyint.f'
      include 'sq_crackflow.f'
      include 'sq_crackvol.f'
      include 'sq_dailycn.f'
      include 'sq_daycn.f'
      include 'sq_greenampt.f'
      include 'sq_snom.f'
      include 'sq_surfst.f'
      include 'sq_volq.f' 


      end module surfrunoff_module