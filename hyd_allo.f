      subroutine hyd_allo (mhyd)

!!    hydrograph arrays-replaces varoute and shyd
      allocate (hd(mhyd))
      allocate (hm(mhyd))
      allocate (ha(mhyd))
      allocate (haa(mhyd))

      if (nstep > 0) then
        allocate (ts(nstep))
        allocate (ts(nstep)%hh(mhyd))
      end if
      allocate (hyd_km2(mhyd))

      return
      end subroutine hyd_allo
