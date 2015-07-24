      module other_module

      use parm
      use time_module
    
      contains
!! routines for other (leftover) module
      include 'sub_subbasin.f'
      include 'varinit.f'
      include 'water_hru.f'
 !     include 'schedule_ops.f'
 !     include 'curno.f'
 !     include 'ttcoef.f'
      include 'albedo.f'
      include 'surface.f'
      include 'wattable.f'
          
      end module other_module