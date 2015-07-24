      subroutine hru_hyds
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine summarizes data for subbasins with multiple HRUs and
!!    prints the daily output.hru file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactrolp      |# cfu/m^2     |less persistent bacteria transported to main
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aird(:)     |mm H2O        |amount of water applied to HRU on current
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnv         |none          |conversion factor (mm/ha => m^3)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: hruday, impndday, subday
!!    SWAT: alph, pkq, ysed, enrsb, pesty, orgn, psed
!!    SWAT: Tair

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      use hydrograph_module
      use basin_module

      integer :: j, sb, kk, ii
      real :: cnv, sub_ha, wtmp, baseflw, bf_fr,hr
      real :: sub_hwyld(nstep), hqd(4*nstep), hsd(4*nstep),hqdtst(nstep)   ! hqd, hsd locally defined. J.Jeong 4/26/2009

      j = ihru
      cnv_m3 = hru(j)%ha * 10.
      cnv_kg = hru(j)%ha

      !! assign reach loadings for subbasin
      !! zero out hydrograph storage locations
      iob = icmd 
      ob(icmd)%hd(3) = hz

      !! surface runoff hydrograph (3)
      ob(icmd)%peakrate = peakr
      ob(icmd)%hd(3)%temp = 5. + .75 * tmpav(j)       !!wtmp
      ob(icmd)%hd(3)%flo = qday * cnv_m3              !!qdr m3/d
      ob(icmd)%hd(3)%sed = sedyld(j)                  !!sedyld
      ob(icmd)%hd(3)%orgn = sedorgn(j) * cnv_kg       !!sedorgn
      ob(icmd)%hd(3)%sedp = (sedorgp(j) + sedminpa(j) +
     &                  sedminps(j)) * cnv_kg         !!sedorgp & sedminps
      ob(icmd)%hd(3)%no3 = surqno3(j) * cnv_kg        !!surqno3 & latno3 & no3gw
      ob(icmd)%hd(3)%solp = surqsolp(j) * cnv_kg      !!surqsolp & sedminpa
      ob(icmd)%hd(3)%chla = chl_a(j) *cnv_kg          !!chl_a
      ob(icmd)%hd(3)%nh3 = 0.                         !! NH3
      ob(icmd)%hd(3)%no2 = 0.                         !! NO2
      ob(icmd)%hd(3)%cbod = cbodu(j) * cnv_kg         !!cbodu
      ob(icmd)%hd(3)%dox = doxq(j) *cnv_kg            !!doxq & soxy
      !if (ob(icmd)%hd(3)%flo > .1) then
      !  ob(icmd)%hd(3)%bacp = (bactrop + bactsedp) *sub_ha/hd(ihout)%flo
      !  ob(icmd)%hd(3)%baclp = (bactrolp+bactsedlp)*sub_ha/hd(ihout)%flo
      !end if
      ob(icmd)%hd(3)%met1 = 0.                        !! cmetal #1
      ob(icmd)%hd(3)%met2 = 0.                        !! cmetal #2
      ob(icmd)%hd(3)%met3 = 0.                        !! cmetal #3
      ob(icmd)%hd(3)%san = sanyld(j)                  !! detached sand
      ob(icmd)%hd(3)%sil = silyld(j)                  !! detached silt
      ob(icmd)%hd(3)%cla = clayld(j)                  !! detached clay
      ob(icmd)%hd(3)%sag = sagyld(j)                  !! detached sml ag
      ob(icmd)%hd(3)%lag = lagyld(j)                  !! detached lrg ag
      
      !recharge hydrograph (2)
      ob(icmd)%hd(2)%flo = sepbtm(j) * cnv_m3          !! recharge flow
      ob(icmd)%hd(2)%no3 = percn(j) * cnv_kg          !! recharge nitrate
      
      !lateral soil flow hydrograph (4)
      ob(icmd)%hd(4)%flo = latq(j) * cnv_m3          !! lateral flow
      ob(icmd)%hd(4)%no3 = latno3(j) * cnv_kg
      
      !tile flow hydrograph (5)
      ob(icmd)%hd(5)%flo = tileq(j) * cnv_m3          !! tile flow
      ob(icmd)%hd(5)%no3 = tileno3(j) * cnv_kg        !! tile flow nitrate 
      
      !sum to obtain the total outflow hydrograph (1)
      ob(icmd)%hd(1) = hz
      do ihyd = 3, 5
        ob(icmd)%hd(1) = ob(icmd)%hd(1) + ob(icmd)%hd(ihyd)
      end do
      
          !! assign reach loadings for subbasin
          !! zero out hydrograph storage locations
          if (bsn_cc%event > 1) then
          do ii = 1, mvaro
            do kk = 1, nstep
              ts(ii)%hh(ihout) = hz
            end do
          end do

          !! set values for different routing variables
          !! storage locations set to zero are not currently used
          do ii = 1, nstep
            ratio = 1. / float(nstep)
            !!need to put Jaehak's unit hyd and urban routines back in to get hwyld and hhsedy
!            if (sub_wyld(sb) > 1.e-3)                                   
!     &                              ratio = sub_hwyld(ii) / sub_wyld(sb)
            if (sub_hwyld(ii) > 0.) then
              ts(ii)%hh(ihout)%temp = wtmp                           !!wtmp
              ts(ii)%hh(ihout)%flo = hd(ihout)%flo * ratio           !!water
              ts(ii)%hh(ihout)%sed = hd(ihout)%sed * ratio           !!sedyld
              ts(ii)%hh(ihout)%orgn = hd(ihout)%orgn * ratio         !!sedorgn
              ts(ii)%hh(ihout)%sedp = hd(ihout)%sedp * ratio         !!sedorgp
              ts(ii)%hh(ihout)%no3 = hd(ihout)%no3 * ratio           !!no3
              ts(ii)%hh(ihout)%solp = hd(ihout)%solp * ratio         !!minp
              ts(ii)%hh(ihout)%psol = hd(ihout)%psol * ratio         !!sol pst
              ts(ii)%hh(ihout)%psor = hd(ihout)%psor * ratio         !!sorb pst
              ts(ii)%hh(ihout)%chla = hd(ihout)%chla * ratio         !!chl_a
              ts(ii)%hh(ihout)%nh3 = 0.                              !! NH3
              ts(ii)%hh(ihout)%no2 = 0.                              !! NO2
              ts(ii)%hh(ihout)%cbod = hd(ihout)%cbod * ratio         !!cbodu
              ts(ii)%hh(ihout)%dox = hd(ihout)%dox * ratio           !!doxq & soxy
              ts(ii)%hh(ihout)%bacp = hd(ihout)%bacp * ratio         !!bactp
              ts(ii)%hh(ihout)%baclp = hd(ihout)%baclp * ratio       !!bactlp
              ts(ii)%hh(ihout)%met1 = 0.                             !!cmetal#1
              ts(ii)%hh(ihout)%met2 = 0.                             !!cmetal#2
              ts(ii)%hh(ihout)%met3 = 0.                             !!cmetal#3  
            end if
		  end do
        end if

      return   
      end subroutine hru_hyds