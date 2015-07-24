      module pesticide_module

      use parm
 !!     use pest_parms
    
      contains
!! routines for pesticide module
      include 'pst_decay.f'
      include 'pst_enrsb.f'
      include 'pst_lch.f'
      include 'pst_pesty.f'
      include 'pst_washp.f'
 !!     include 'pst_pestw.f'

      end module pesticide_module