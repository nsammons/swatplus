      module mgtops_module

      use parm 
      use plant_module
      use time_module
      use jrw_datalib_module
      
      contains
!! routines for management operations module
      include 'mgt_plantop.f' 
      include 'mgt_dormant.f' 
      include 'mgt_harvkillop.f' 
      include 'mgt_harvestop.f' 
      include 'mgt_harvgrainop.f' 
      include 'mgt_killop.f' 
      include 'mgt_newtillmix.f'
      include 'mgt_sched.f'
      include 'mgt_operatn.f'
      include 'mgt_irrsub.f'
      include 'mgt_autoirr.f'
      include 'mgt_tillfactor.f'
      include 'mgt_tillmix.f'
      include 'mgt_trop_gro.f'

      end module mgtops_module