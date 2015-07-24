      module jrw_process_module
     
      contains
      include 'bac_apply_soil.f'
      include 'bac_apply_plant.f'
 !     include 'bac_ls_runoff.f'
      include 'aqu_1dflo.f'
      include 'aqu_gwht.f'
      include 'aqu_revap.f'
      include 'aqu_no3.f'
      include 'aqu_minp.f'
      include 'aqu_rechrglag.f'
      include 'aqu_seep.f'

      
      end module jrw_process_module 