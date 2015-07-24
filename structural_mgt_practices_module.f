      module structural_mgt_practices_module

      use parm
      use time_module
    
      contains
!! routines for structural management practices module
      include 'smp_bmpfixed.f'
      include 'smp_buffer.f'
      include 'smp_filter.f'
      include 'smp_filtw.f'
      include 'smp_grass_wway.f'


      end module structural_mgt_practices_module