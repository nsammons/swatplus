      subroutine ch_rchuse
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine removes water from reach for consumptive water use

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    inum1       |none          |reach number
!!    i_mo        |none          |month of simulation
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sedrch      |metric tons   |sediment transported out of reach on day
!!    wurch(:,:)  |10^4 m^3/day  |average daily water removal from the reach
!!                               |for the month
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sedrch      |metric tons   |sediment transported out of reach on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    jrch        |none          |HRU number
!!    wtrin       |m^3 H2O       |water outflow from reach prior to
!!                               |subtracting irrigation diversions
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

         use basin_module

      real :: wtrin

      wtrin = 0.
      wtrin = rtwtr

      !need to integrate with new water use module -->  rtwtr = rtwtr - wurch(i_mo,jrch) * 10000.
      if (rtwtr < 0.) rtwtr = 0.

!      if (bsn_cc%event > 2) then
!        do ii = 1, nstep
!          !need to integrate with new water use module --> hrtwtr(ii) = hrtwtr(ii) - wurch(i_mo,jrch) * 10000. 
!     &      / real(nstep)
!          if (hrtwtr(ii) < 0.) hrtwtr(ii) = 0.
!        end do
!      end if

      if (wtrin /= rtwtr .and. wtrin > 0.01) then
        sedrch = sedrch * rtwtr / wtrin

        rch_san = rch_san * rtwtr / wtrin
        rch_sil = rch_sil * rtwtr / wtrin
        rch_cla = rch_cla * rtwtr / wtrin
        rch_sag = rch_sag * rtwtr / wtrin
        rch_lag = rch_lag * rtwtr / wtrin
        rch_gra = rch_gra * rtwtr / wtrin

        if (sedrch  < 1.e-6) then
	    sedrch = 0.
	    rch_san = 0.
          rch_sil = 0.
          rch_cla = 0.
          rch_sag = 0.
          rch_lag = 0.
          rch_gra = 0.
	  end if

        if (bsn_cc%event > 2) then
          do ii = 1, nstep
            hsedyld(ii) = hsedyld(ii) * rtwtr / wtrin
            if (hrtwtr(ii) == 0.) hsedyld(ii) = 0.
          end do
        end if
      end if

      return
      end subroutine ch_rchuse