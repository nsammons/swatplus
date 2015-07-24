      subroutine channel_allo (mch)
      
      !use jrw_datalib_module
      
      integer :: mch
      allocate (ch(mch))
      !allocate (ch_dat(0:mch))
      allocate (ch_d(mch))
      allocate (ch_m(mch))
      allocate (ch_y(mch))
      allocate (ch_a(mch))
      allocate (rchsep(mch))

      if (nstep > 0) then
      allocate (hrtwtr(nstep))
      allocate (hharea(nstep))
      allocate (hdepth(nstep))
      allocate (rhy(nstep))
      allocate (hsdti(nstep))
      allocate (hhtime(nstep))
      allocate (hrttlc(nstep))
      allocate (hrtevp(nstep))
      allocate (hhstor(nstep))
      allocate (hrchwtr(nstep))
      allocate (halgae(nstep))
      allocate (hbactlp(nstep))
      allocate (hbactp(nstep))
      allocate (hbod(nstep))
      allocate (hchla(nstep))
      allocate (hdisox(nstep))
      allocate (hnh4(nstep))
      allocate (hno2(nstep))
      allocate (hno3(nstep))
      allocate (horgn(nstep))
      allocate (horgp(nstep))
      allocate (hsedst(nstep))
      allocate (hsedyld(nstep))
      allocate (hsolp(nstep))
      allocate (hsolpst(nstep))
      allocate (hsorpst(nstep))
      end if

      return
      end subroutine channel_allo