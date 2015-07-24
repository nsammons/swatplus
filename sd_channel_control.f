      subroutine sd_channel_control
      
      ich = isdch
      isd_db = ob(icmd)%props
      
      !! set incoming flow and sediment
      chflow_m3 = ob(icmd)%hin%flo
      if (chflow_m3 > 1.e-6) then
      sedin = ob(icmd)%hin%sed
      
      !! compute headcut
      !! adjust peak rate for headcut advance -also adjusts CEAP gully from
      !! edge-of-field to trib (assuming rectangular shape and constant tc)
      pr_ratio = (sd_ch(ich)%chl - sd_ch(ich)%hc_len / 1000.) / 
     &                                                  sd_ch(ich)%chl
      pr_ratio = amax1(pr_ratio, 0.)
      !assume triangular hydrograph
      peakrate = 2. * chflow_m3 / (1.5 * sd_chd(ich)%tc)
      peakrate = peakrate / 60.   !convert min to sec
      attack = peakrate * sd_chd(ich)%hc_hgt
      hc_adv = sd_ch(ich)%hc_co * (attack - sd_ch(ich)%attack0)
      hc_adv = amax1(hc_adv, 0.)
      hc_adv = amin1(hc_adv, sd_ch(ich)%chl*1000.)
      sd_ch(ich)%hc_len = sd_ch(ich)%hc_len + hc_adv / 1000.
      !! compute sediment yield from headcut- assume bd = 1.2 t/m3
      !! assume channel dimensions are same as data file
      hc_sed = hc_adv * sd_chd(ich)%chw * sd_chd(ich)%chd * 1.2
      
!!        compute changes in channel dimensions
          chside = 2.
          b = sd_ch(ich)%chw - 2. * sd_ch(ich)%chd * chside
          b = amax1(0., b)
          sd_ch(ich)%phi(6) = b
          sd_ch(ich)%phi(7) = sd_ch(ich)%chd

!!        compute flow and travel time at bankfull depth
          chside = 2.
          p = b + 2. * sd_ch(ich)%chd * Sqrt(chside * chside + 1.)
          a = b * sd_ch(ich)%chd + chside * sd_ch(ich)%chd *            
     &        sd_ch(ich)%chd
          rh = a / p
          sd_ch(ich)%phi(1) = a
          sd_ch(ich)%phi(5) = Qman(a,rh,sd_chd(ich)%chn,sd_ch(ich)%chs)
  
          IF (peakrate > sd_ch(ich)%phi(5)) THEN
            rcharea = sd_ch(ich)%phi(1)
            rchdep = sd_ch(ich)%chd
            !! estimate overbank flow - assume a triangular hyd
            tbase = 1.5 * sd_chd(ich)%tc * 60.  !seconds
            vol_ovb = 0.5 * (peakrate - sd_ch(ich)%phi(5)) * 
     &                              sd_ch(ich)%phi(5) / peakrate * tbase
            vol_ovb = amin1(vol_ovb, chflow_m3)
            vol_ovb = peakrate - sd_ch(ich)%phi(5)
            const = vol_ovb / peakrate
            ob(icmd)%hd(3) = const * ob(icmd)%hin
            !find current total flood volume (ht1)
            ht1 = hz
            ics = ob(icmd)%props2
            do ii = 1, ch_sur(ics)%num
              ht1 = ht1 + ch_sur(ics)%hd(ii)
            end do
            !add current and new flood volumes
            ht1 = ht1 + ob(icmd)%hd(3)
           if (ht1%flo > ch_sur(ics)%flood_volmx(0)) then
            !calc flood depth above channel bottom (flodd_dep)
            sum_vol = 0.
            do ii = 1, ch_sur(ics)%num
              if (ht1%flo < ch_sur(ics)%flood_volmx(ii)) then
                !solve quadrative for depth above base of current element
                a = sd_ch(ich)%chl * 1000. / sd_ch(ich)%chs
                b = ch_sur(ics)%wid(ii-1) * sd_ch(ich)%chl * 1000.
                c =  ch_sur(ics)%flood_volmx(ii-1) - ht1%flo
                xx = b ** 2 - 4. * a * c
                dep = (-b + sqrt(xx)) / (2. * a)
                dep = amax1(0., dep)
                ic = ii
              end if
            end do
            !calc flood depth above channel bottom
            flood_dep = dep + ch_sur(ics)%dep(ic-1)
            !calc new flood volume for each element
            do ii = 1, ch_sur(ics)%num
              !calc depth in the element
              
              if (flood_dep < ch_sur(ics)%dep(ii-1)) then
                !no flooding on element
                ch_sur(ics)%hd(ii)%flo = 0.
              else if (flood_dep < ch_sur(ics)%dep(ii)) then
                !flood level within the element
                dep_e = flood_dep - ch_sur(ics)%dep(ii-1)
                ch_sur(ics)%hd(ii)%flo = dep_e ** 2 / sd_ch(ich)%chs 
     &                                                  * sd_ch(ich)%chl
              else
                !flood level over max element depth
                ch_sur(ics)%hd(ii)%flo = 2. * sd_ch(ich)%chw * 
     &            (flood_dep - ch_sur(ics)%dep(ii)) + sd_ch(ich)%chw * 
     &            (ch_sur(ics)%dep(ii) - ch_sur(ics)%dep(ii-1))
              end if
              ch_sur(ics)%hd(ii)%flo = amin1 (
     &           ch_sur(ics)%hd(ii)%flo, ch_sur(ics)%flood_volmx(ii))
              sum_vol = sum_vol + ch_sur(ics)%hd(ii)%flo
            end do
            !determine fraction of total flood volume and set volume for each element
            rto = sum_vol / ht1%flo  !ensure water balance is maintained
            do ii = 1, ch_sur(ics)%num
              const = rto * ch_sur(ics)%hd(ii)%flo / ht1%flo
              ch_sur(ics)%hd(ii) = const * ht1
            end do
           end if
          ELSE
!!          find the crossectional area and depth for volrt
!!          by iteration method at 1cm interval depth
!!          find the depth until the discharge rate is equal to volrt
            !zero overbank flow
            ob(icmd)%hd(3) = hz
            sdti = 0.
            rchdep = 0.
            DO WHILE (sdti < peakrate)
              rchdep = rchdep + 0.01
              rcharea = (sd_ch(ich)%phi(6) + chside * rchdep) * rchdep
              p=sd_ch(ich)%phi(6)+2. * rchdep*Sqrt(1.+chside *chside)
              rh = rcharea / p
              sdti = Qman(rcharea, rh, sd_chd(ich)%chn, sd_ch(ich)%chs)
            END DO
          END IF

!!        calculate flow velocity
          vc = 0.001
          IF (rcharea > 1.e-4) THEN
            vc = peakrate / rcharea
            IF (vc > sd_ch(ich)%phi(9)) vc = sd_ch(ich)%phi(9)
          END IF

      !select bedload equation
      SELECT CASE (sd_chd(ich)%bl_eqn)
        CASE (0) !! undefined type
        !!        compute deposition and degradation in the channel
          cyin = 0.
          cych = 0.
          depnet = 0.
          deg = 0.
          dep = 0.
          IF (chflow_m3 > 1.e-4) THEN
            cyin = sedin / chflow_m3
            IF (sd_chd(ich)%d50 > 1.) THEN
              coeff = .6
            ELSE
              coeff = -3. * sd_chd(ich)%d50 + 3.6
            END IF
            cych = coeff * sd_chd(ich)%spcon * vc ** sd_chd(ich)%spexp
            depnet = chflow_m3 * (cych - cyin)
            IF (depnet > 0.) THEN
              deg = depnet * sd_chd(ich)%cherod * sd_chd(ich)%chcov
              dep = 0.
            ELSE
              dep = -depnet
              deg = 0.
            END IF
          END IF
          sedout = sedin - dep + deg + hc_sed
          bedld = sd_chd(ich)%bedldcoef * sedout / (1. - bedld_coef)
	    washld = sedout - bedld
          
!!        compute changes in channel dimensions
          depdeg = 0.
          depdeg = sd_ch(ich)%chd - sd_chd(ich)%chd
          IF (sd_ch(ich)%chs > sd_chd(ich)%chseq) THEN
            dot = 35.86 * rchdep * sd_ch(ich)%chs * sd_chd(ich)%cherod 
            sd_ch(ich)%chd = sd_ch(ich)%chd + dot
            sd_ch(ich)%chw = sd_chd(ich)%chwdr * sd_ch(ich)%chd
            sd_ch(ich)%chs = sd_ch(ich)%chs-dot/(sd_ch(ich)%chl*1000.)
            sd_ch(ich)%chs = MAX(sd_chd(ich)%chseq, sd_ch(ich)%chs)
!           call ttcoef(jrch)
          END IF

        CASE (1)        
          xx = sd_chd(ich)%chn ** (2.39 - 0.8 * ALOG(sd_chd(ich)%d50))
          yy = (sd_chd(ich)%d50 - 0.07) ** - 0.14
          sd_chd(ich)%acoef = 0.025 * xx * yy
	    sd_chd(ich)%bcoef = 4.93 - 0.74 * ALOG(sd_chd(ich)%d50)
          sd_chd(ich)%ccoef = -0.46 + 0.65 * ALOG(sd_chd(ich)%d50)
          sd_chd(ich)%acoef = sd_chd(ich)%acoef * (0.3048 ** (2. -      
     &        sd_chd(ich)%bcoef - sd_chd(ich)%ccoef)) 
          qs = sd_chd(ich)%acoef * (vc ** sd_chd(ich)%bcoef) * (rchdep**
     &        sd_chd(ich)%ccoef)
          qs = qs * 228960.                  !! m2/s => t/day

        CASE (2)
          qc = 0.0000194 * sd_chd(ich)%d50 / (sd_ch(ich)%chs ** 1.333)
          qs = 7000.*(sd_ch(ich)%chs**1.5)*(vc-qc)/(sd_chd(ich)%d50**.5)
          qs = qs * 86.4                     !! kgls => t/day

        CASE (3)
         IF (sd_chd(ich)%d50 > 1.) THEN
          coeff = .6
         ELSE
          coeff = -3. * sd_chd(ich)%d50 + 3.6
         END IF
         qs=coeff*sd_chd(ich)%acoef*(vc**sd_chd(ich)%bcoef)*(rchdep **  
     &      sd_chd(ich)%ccoef) * (sd_ch(ich)%chs ** sd_chd(ich)%dcoef)
         qs = qs * sd_ch(ich)%chw     !! t/d = t/m/d * m
         !qs = qs * trac_tday   !! percent of day crit tractive force is exceeded

        CASE (4)
          IF (sd_chd(ich)%d50 > 1.) THEN
          coeff = .6
         ELSE
          coeff = -3. * sd_chd(ich)%d50 + 3.6
         END IF
          qs = coeff * sd_chd(ich)%acoef * peakrate ** sd_chd(ich)%bcoef
          
      END SELECT


      IF (i_chantype .ne.0) then
      IF (chflow_m3 > 1.e-4) then
	  bedld = sd_chd(ich)%bedldcoef * sedin / (1. - bedld_coef)
	  washld = sedin
	  depnet = qs - bedld
!!	  cyin = bedld / chflow_m3 
!!	  depnet = chflow_m3 * (qs - cyin)
        !! depnet = qs - sedin
        IF (depnet > 0.) THEN
          deg = depnet
          dep = 0.
        ELSE
           dep = -depnet
           deg = 0.
        END IF

	sedout = (bedld-dep+deg) + washld + hc_sed
      
      IF (deg > 0.) THEN

        depdeg = 0.
        depdeg = sd_ch(ich)%chd - sd_chd(ich)%chd
        IF (sd_ch(ich)%chs > sd_chd(ich)%chseq) THEN
          shear = 9800. * rchdep * sd_ch(ich)%chs   !! Pa = N/m^2 * m * m/m
	    IF (shear > sd_chd(ich)%shearcr) THEN
	      r = 1. - bedld / qs
            r = amin1(1.,r)
            downcut = r * shear * sd_chd(ich)%cherod     !! cm/hr = Pa * cm/hr/Pa
	    ELSE
	      downcut = 0.
          END IF
          trac_tday = .1  !this needs to be computed based on hydrograph shape
	    downcut = .24 * trac_tday * downcut     !! m/d = 24 hr/d *.01 m/cm * cm/hr
          sd_ch(ich)%chd = sd_ch(ich)%chd + downcut
          sd_ch(ich)%chw = sd_chd(ich)%chwdr * sd_ch(ich)%chd
          sd_ch(ich)%chs = sd_ch(ich)%chs-downcut/(sd_ch(ich)%chl*1000.)
          sd_ch(ich)%chs = MAX(sd_chd(ich)%chseq, sd_ch(ich)%chs)
 !         call ttcoef(jrch)
        END IF
      END IF
      END IF
      END IF
          
      
      !! output_channel
      chsd_d(ich)%flo = ob(icmd)%hin%flo  / 86400. 
      chsd_d(ich)%peakr = peakrate 
      chsd_d(ich)%sed_in = ob(icmd)%hin%sed  
      chsd_d(ich)%sed_out = sedout
      chsd_d(ich)%washld = washld
      chsd_d(ich)%bedld = bedld
      chsd_d(ich)%dep = dep
      chsd_d(ich)%dc_sed = deg
      chsd_d(ich)%hc_sed = hc_sed
      
      !! set values for outflow hydrograph
      !! storage locations set to zero are not currently used
      ob(icmd)%hd(1)%temp = 5. + .75 * tave        !!wtmp
      ob(icmd)%hd(1)%flo = chflow_m3               !!qdr m3/d
      ob(icmd)%hd(1)%sed = sedout                  !!sedyld
      ob(icmd)%hd(1)%orgn = 0.
      ob(icmd)%hd(1)%sedp = 0.
      ob(icmd)%hd(1)%no3 = 0.
      ob(icmd)%hd(1)%solp = 0.
      ob(icmd)%hd(1)%chla = 0.
      ob(icmd)%hd(1)%nh3 = 0.                         !! NH3
      ob(icmd)%hd(1)%no2 = 0.                         !! NO2
      ob(icmd)%hd(1)%cbod = 0.
      ob(icmd)%hd(1)%dox = 0.
      if (ob(icmd)%hd(1)%flo > .1) then
        ob(icmd)%hd(1)%bacp = 0.
        ob(icmd)%hd(1)%baclp = 0.
      end if
      ob(icmd)%hd(1)%met1 = 0.                            !! cmetal #1
      ob(icmd)%hd(1)%met2 = 0.                            !! cmetal #2
      ob(icmd)%hd(1)%met3 = 0.                            !! cmetal #3
      ob(icmd)%hd(1)%san = 0.                             !! det sand
      ob(icmd)%hd(1)%sil = 0.                             !! det silt
      ob(icmd)%hd(1)%cla = 0.                             !! det clay
      ob(icmd)%hd(1)%sag = 0.                             !! det sml ag
      ob(icmd)%hd(1)%lag = 0.                             !! det lrg ag
         
      !! set values for recharge hydrograph
      ob(icmd)%hd(2)%flo = perc  
      else
          
      ob(icmd)%hd(1) = hz
      end if
      
      if (time%yrs > pco%nyskip) then
        call sd_channel_output
      end if
      
      end subroutine sd_channel_control