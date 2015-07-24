      subroutine aqu_1dlag 
      
      use jrw_datalib_module

      aqu(iaq)%rchrg = rchrg1
      if (aqu(iaq)%stor > aqudb(iaq)%flo_min) then
        aqu(iaq)%flo = aqu(iaq)%flo * alpha_bfe(iaq) + rchrg1
     &                                    * (1. - alpha_bfe(iaq))
      else
        aqu(iaq)%flo = 0.
      endif

      !! set hydrograph flow from aquifer- convert mm to m3
      ob(icmd)%hd(1)%flo = 10. * aqu(iaq)%flo * ob(icmd)%ha
      
!!    update storage

      !call aqu_gwht (iaq, rchrg, gwht_prev, gwht)
      
      !call aqu_revap (iaq, pet, stor_prev, revap, stor)
      
!!    update storage

      !call aqu_no3 (iaq, percno3, rchrgn_prev, revap, seep, stor,
!     & stor_no3, flow, flow3, revapno3, seepno3)
      
      !call aqu_minp (iaq, flo, minp)
      
      !call aqu_hyds
      
      return
      end subroutine aqu_1dlag