      subroutine mgt_trop_gro

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine ends and initializes tropical plant growth

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    idc(:)         |none          |crop/landcover category:
!!                                  |1 warm season annual legume
!!                                  |2 cold season annual legume
!!                                  |3 perennial legume
!!                                  |4 warm season annual
!!                                  |5 cold season annual
!!                                  |6 perennial
!!                                  |7 trees
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use climate_parms
      use basin_module

      real :: resnew
      integer :: j

      j = ihru

      select case (pldb(idp)%idc)

      !! beginning of forest dormant period
      case (7)
        pcom(j)%mseas = 0
        pcom(j)%plcur(ipl)%gro = 1
        pcom(j)%plcur(ipl)%idorm = 0
        resnew = pcom(j)%plm(ipl)%mass * pcom(j)%plg(ipl)%bio_leaf
        hru(j)%rsd_flt(ipl)%mass = hru(j)%rsd_flt(ipl)%mass +
     &                                                            resnew
        hru(j)%rsd_flt(ipl)%nmass = resnew * pcom(j)%plm(ipl)%n_fr +
     &                                         hru(j)%rsd_flt(ipl)%nmass
        hru(j)%rsd_flt(ipl)%pmass = resnew * pcom(j)%plm(ipl)%p_fr +
     &                                         hru(j)%rsd_flt(ipl)%pmass
        pcom(j)%plm(ipl)%mass = pcom(j)%plm(ipl)%mass *   
     &          (1. - pcom(j)%plg(ipl)%bio_leaf)
        pcom(j)%plm(ipl)%nmass = pcom(j)%plm(ipl)%nmass - resnew *
     &         pcom(j)%plm(ipl)%n_fr
        pcom(ihru)%plm(ipl)%pmass = 
     &        pcom(ihru)%plm(ipl)%pmass - resnew * pcom(j)%plm(ipl)%p_fr
        pcom(j)%plg(ipl)%lai = pldb(idp)%alai_min
        pcom(j)%plcur(ipl)%phuacc = 0.
        !pcom(j)%plg(ipl)%laimxfr = 0.        !Sue White - dormancy

      !! beginning of perennial (pasture/alfalfa) dormant period
      case (3, 6)
        pcom(j)%mseas = 0
        pcom(j)%plcur(ipl)%gro = 1
        pcom(j)%plcur(ipl)%idorm = 0
        resnew = pldb(idp)%bm_dieoff * pcom(j)%plm(ipl)%mass
        hru(j)%rsd_flt(ipl)%mass = hru(j)%rsd_flt(ipl)%mass + resnew
        hru(j)%rsd_flt(ipl)%nmass = hru(j)%rsd_flt(ipl)%nmass +
     &         pldb(idp)%bm_dieoff * pcom(j)%plm(ipl)%nmass
        hru(j)%rsd_flt(ipl)%pmass = hru(j)%rsd_flt(ipl)%pmass +
     &         pldb(idp)%bm_dieoff * pcom(ihru)%plm(ipl)%pmass
        pcom(j)%plm(ipl)%mass = (1. - pldb(idp)%bm_dieoff) *
     &        pcom(j)%plm(ipl)%mass
        pcom(j)%plm(ipl)%nmass = (1. - pldb(idp)%bm_dieoff) *
     &         pcom(j)%plm(ipl)%nmass
        pcom(ihru)%plm(ipl)%pmass = (1. - pldb(idp)%bm_dieoff) *
     &         pcom(ihru)%plm(ipl)%pmass
        pcom(j)%plg(ipl)%lai = pldb(idp)%alai_min
        pcom(j)%plcur(ipl)%phuacc = 0.
          
      end select

      return
      end subroutine mgt_trop_gro