      module erosion_module

      use parm
    
      contains
!! routines for erosion module
      include 'ero_alph.f'
      include 'ero_cfactor.f'
      include 'ero_eiusle.f'
      include 'ero_ovrsed.f'
      include 'ero_pkq.f'
      include 'ero_ysed.f'

      end module erosion_module