      subroutine sub_allo
      
      use basin_module

      do isub = 1, sp_ob%sub
        ith = sub(isub)%topo_db
        ichan = topo_db(ith)%channel_db
        !if (ith > 0 .and. ichan > 0) then                  
        ! compute tc for the subbasin
          tov = .0556 * (topo_db(ith)%slope_len * sub_n(isub)) ** .6 /
     &                                        topo_db(ith)%slope ** .3
          tch = .62 * ch_hyd(ichan)%l * ch_hyd(ichan)%n ** .75 /
     &                  (sub(isub)%da_km2**.125 * ch_hyd(ichan)%s**.375)
          sub_tc(isub) = tov + tch
        !end if                                             
      end do

      if (bsn_cc%event > 1) then
!!    compute unit hydrograph for computing subbasin hydrograph from direct runoff
      do isb = 1, sp_ob%sub
        ql = 0.
        sumq = 0.
        tb = .5 + .6 * sub_tc(isb) + bsn_prm%tb_adj  !baseflow time, hr
        if (tb > 48.) tb = 48.			   !maximum 48hrs
        tp = .375 * tb                       ! time to peak flow
	  !! convert to time step (from hr), J.Jeong March 2009
	  tb = ceiling(tb * 60./ real(bsn_prm%dts))
	  tp = int(tp * 60./ real(bsn_prm%dts))         
	  
	  if(tp==0) tp = 1
	  if(tb==tp) tb = tb + 1
	  itb(isb) = int(tb) 
        
	  ! Triangular Unit Hydrograph
	  if (bsn_cc%uhyd == 1) then
	    do i = 1, itb(isb)
            xi = float(i)
 	      if (xi < tp) then           !! rising limb of hydrograph
              q = xi / tp
            else                        !! falling limb of hydrograph
              q = (tb - xi) / (tb - tp)
            end if
            q = Max(0.,q)
            uhs(isb,i) = (q + ql) / 2.
            ql = q
            sumq = sumq + uhs(isb,i)
          end do
          
		do i = 1, itb(isb)
            uhs(isb,i) = uhs(isb,i) / sumq
          end do
	  
	  ! Gamma Function Unit Hydrograph
	  elseif (bsn_cc%uhyd == 2) then
          i = 1; q=1.
		do while (q>0.0001)
            xi = float(i)
            q = (xi / tp) ** bsn_prm%uhalpha * exp((1.- xi / tp) *      
     &                       uhalpha)
            q = Max(0.,q)
            uhs(isb,i) = (q + ql) / 2.
            ql = q
            sumq = sumq + uhs(isb,i)
	      i = i + 1
	      if (i>3.*nstep) exit
	    end do
	    itb(isb) = i - 1
          do i = 1, itb(isb)
            uhs(isb,i) = uhs(isb,i) / sumq
          end do
	  endif 
      end do
      end if

      return
      end subroutine sub_allo