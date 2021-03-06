      subroutine hru_soil_phys (isol)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes soil physical properties

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cn2(:)        |none          |SCS runoff curve number for moisture
!!                                 |condition II
!!    ddrain(:)     |mm            |depth to the sub-surface drain
!!    hru_dafr(:)   |km2/km2       |fraction of total watershed area contained
!!                                 |in HRU
!!    i             |none          |HRU number
!!    rock(:)       |%             |percent of rock fragments in soil layer
!!    sno_hru(:)    |mm H2O        |amount of water stored as snow
!!    usle_ls(:)    |none          |USLE equation length slope (LS) factor
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ldrain(:)     |none          |soil layer where drainage tile is located
!!    rock(:)       |none          |exponential value that is a function of
!!                                 |percent rock
!!    sol_st(:,:)   |mm H2O        |amount of water stored in the soil layer
!!                                 |on any given day (less wp water)
!!    usle_mult(:)  |none          |product of USLE K,P,LS,exp(rock)
!!    wfsh(:)       |mm            |wetting front matric potential
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dg          |mm            |depth of layer
!!    j           |none          |counter
!!    nly         |none          |number of soil layers
!!    pormm       |mm            |porosity in mm depth
!!    sumpor      |mm            |porosity of profile
!!    xx          |none          |variable to hold value
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Sqrt
!!    SWAT: Curno

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module

      integer :: nly, j
      real :: xx, sumpor, dg, pormm, nota, a,b,c,d

      if (sol(isol)%s%alb < 0.1) sol(isol)%s%alb  = 0.1
      if (sol(isol)%s%anion_excl<=1.e-6) sol(isol)%s%anion_excl =       
     &                                  anion_excl_bsn
      if (sol(isol)%s%anion_excl >= 1.) sol(isol)%s%anion_excl = 0.99

      nly = sol(isol)%s%nly
      do j = 1, nly
        a = 50.0
        b = 20.0
        c = 5.0
        d = 2.0           
        nota = 10.
        if (sol(isol)%phys(j)%k <= 0.0) then 
          if (sol(isol)%s%hydgrp == "A") then
            sol(isol)%phys(j)%k = a
	    else
          if (sol(isol)%s%hydgrp == "B") then
            sol(isol)%phys(j)%k = b
	    else
          if (sol(isol)%s%hydgrp == "C") then
            sol(isol)%phys(j)%k = c
	    else
          if (sol(isol)%s%hydgrp == "D") then
            sol(isol)%phys(j)%k = d          !Claire 12/2/09
          else 
           sol(isol)%phys(j)%k = nota
          endif
          endif
          endif
          endif
        endif

        if (sol(isol)%phys(j)%bd <= 1.e-6) sol(isol)%phys(j)%bd = 1.3
        if (sol(isol)%phys(j)%bd > 2.) sol(isol)%phys(j)%bd = 2.0
        if (sol(isol)%phys(j)%awc <= 1.e-6) sol(isol)%phys(j)%awc = .005
        if (sol(isol)%phys(j)%awc >= .8) sol(isol)%phys(j)%awc = .8
        if (sol(isol)%phys(j)%rock > 98.0) sol(isol)%phys(j)%rock= 98.0
        sol(isol)%ly(j)%n = sol(isol)%cbn(j)%cbn / 11.0
        !! Defaults for ph and calcium mjw average of 20,000 SSURGO soils mjw rev 490
        if (sol(isol)%ly(j)%cal <= 1.e-6) sol(isol)%ly(j)%cal = 2.8
        if (sol(isol)%ly(j)%ph<= 1.e-6) sol(isol)%ly(j)%ph = 6.5
      end do
!-------------------------------------------------------------
  
      nly = sol(isol)%s%nly

!!    calculate composite usle value
      sol(isol)%phys(1)%rock = Exp(-.053 * sol(isol)%phys(1)%rock)

!!    calculate water content of soil at -1.5 MPa and -0.033 MPa
      do j = 1, nly
        sol(isol)%phys(j)%wp=0.4 * sol(isol)%phys(j)%clay * 
     &                                      sol(isol)%phys(j)%bd / 100.
        if (sol(isol)%phys(j)%wp <= 0.) sol(isol)%phys(j)%wp = .005
         sol(isol)%phys(j)%up=sol(isol)%phys(j)%wp+sol(isol)%phys(j)%awc
         sol(isol)%phys(j)%por = 1. - sol(isol)%phys(j)%bd / 2.65
        if (sol(isol)%phys(j)%up >= sol(isol)%phys(j)%por) then
         sol(isol)%phys(j)%up = sol(isol)%phys(j)%por - .05
         sol(isol)%phys(j)%wp=sol(isol)%phys(j)%up-sol(isol)%phys(j)%awc
        if (sol(isol)%phys(j)%wp <= 0.) then
          sol(isol)%phys(j)%up = sol(isol)%phys(j)%por * .75
          sol(isol)%phys(j)%wp = sol(isol)%phys(j)%por * .25
        end if
        end if
        !! compute drainable porosity and variable water table factor - Daniel
        drpor = sol(isol)%phys(j)%por - sol(isol)%phys(j)%up
        sol(isol)%ly(j)%vwt=(437.13*drpor**2)-(95.08 * drpor)+8.257
       end do

      sa = sol(isol)%phys(1)%sand / 100.
      cl = sol(isol)%phys(1)%clay  / 100.
      si = sol(isol)%phys(1)%silt / 100.
!!    determine detached sediment size distribution
!!    typical for mid-western soils in USA (Foster et al., 1980)
!!    Based on SWRRB
       sol(isol)%s%det_san = 2.49 * sa * (1. - cl)   !! Sand fraction
       sol(isol)%s%det_sil = 0.13 * si               !! Silt fraction
       sol(isol)%s%det_cla = 0.20 * cl               !! Clay fraction   
       if (cl < .25) then
         sol(isol)%s%det_sag = 2.0 * cl              !! Small aggregate fraction                    
       else if (cl > .5) then
         sol(isol)%s%det_sag = .57
       else
         sol(isol)%s%det_sag = .28 * (cl - .25) + .5
       end if

       sol(isol)%s%det_lag = 1. - sol(isol)%s%det_san -                 
     &  sol(isol)%s%det_sil - sol(isol)%s%det_cla - sol(isol)%s%det_sag  !! Large Aggregate fraction

!!	Error check. May happen for soils with more sand
!!    Soil not typical of mid-western USA
!!    The fraction wont add upto 1.0
	if (sol(isol)%s%det_lag < 0.) then
	  sol(isol)%s%det_san = sol(isol)%s%det_san/(1 - sol(isol)%s%det_lag) 
	  sol(isol)%s%det_sil = sol(isol)%s%det_sil/(1 - sol(isol)%s%det_lag) 
	  sol(isol)%s%det_cla = sol(isol)%s%det_cla/(1 - sol(isol)%s%det_lag) 
	  sol(isol)%s%det_sag = sol(isol)%s%det_sag/(1 - sol(isol)%s%det_lag) 
	  sol(isol)%s%det_lag = 0.
      end if


!!    initialize water/drainage coefs for each soil layer
      xx = 0.
      sumpor = 0.
      do j = 1, nly
        dg = 0.
        pormm = 0.
        dg = sol(isol)%phys(j)%d - xx
        pormm = sol(isol)%phys(j)%por * dg
        sumpor = sumpor + pormm
        sol(isol)%phys(j)%ul=(sol(isol)%phys(j)%por - 
     &     sol(isol)%phys(j)%wp)
     &                                                        * dg
        sol(isol)%s%sumul = sol(isol)%s%sumul + sol(isol)%phys(j)%ul
        sol(isol)%phys(j)%fc = dg * (sol(isol)%phys(j)%up - 
     &                                        sol(isol)%phys(j)%wp)
        sol(isol)%s%sumfc = sol(isol)%s%sumfc + sol(isol)%phys(j)%fc
        sol(isol)%phys(j)%st = sol(isol)%phys(j)%fc * sol(isol)%s%ffc
        sol(isol)%phys(j)%hk = (sol(isol)%phys(j)%ul - 
     &     sol(isol)%phys(j)%fc) / sol(isol)%phys(j)%k
        if (sol(isol)%phys(j)%hk < 1.) sol(isol)%phys(j)%hk = 1.
        sol(isol)%s%sw = sol(isol)%s%sw + sol(isol)%phys(j)%st
        sol(isol)%phys(j)%wpmm = sol(isol)%phys(j)%wp * dg
        sol(isol)%s%sumwp = sol(isol)%s%sumwp + sol(isol)%phys(j)%wpmm
        sol(isol)%phys(j)%crdep = sol(isol)%s%crk * 0.916 *
     &    Exp(-.0012 * sol(isol)%phys(j)%d) * dg
        sol(isol)%ly(j)%volcr = sol(isol)%phys(j)%crdep *
     &    (sol(isol)%phys(j)%fc - sol(isol)%phys(j)%st) / 
     &    (sol(isol)%phys(j)%fc)
        xx = sol(isol)%phys(j)%d
      end do
      !! initialize water table depth and soil water for Daniel
      sol(isol)%s%swpwt = sol(isol)%s%sw
      if (sol(isol)%s%ffc > 1.) then
        sol(isol)%s%wat_tbl = (sol(isol)%s%det_lag - sol(isol)%s%ffc *  
     &    sol(isol)%s%sumfc) / sol(isol)%phys(nly)%d
      else
        sol(isol)%s%wat_tbl = 0.                              !!! jga
      end if
      sol(isol)%s%avpor = sumpor / sol(isol)%phys(nly)%d
      sol(isol)%s%avbd = 2.65 * (1. - sol(isol)%s%avpor)

!!    calculate infiltration parameters for subdaily time step
      if (bsn_cc%event > 0) then
        sol(isol)%phys(1)%sand = 100. - sol(isol)%phys(1)%clay -
     &                                    sol(isol)%phys(1)%silt
        
      end if

      return
      end subroutine hru_soil_phys