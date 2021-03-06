      subroutine ero_ovrsed()
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes splash erosion by raindrop impact and flow erosion by overland flow

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cht(:)      |m             |canopy height
!!    fimp(:)     |fraction      |fraction of HRU area that is
!!                               |impervious (both directly and
!!                               |indirectly connected)
!!    hhqday(:)   |mm H2O        |surface runoff generated each timestep 
!!                               |of day in HRU
!!    hru_km(:)   |km2           |area of HRU in square kilometers
!!    inum1       |none          |subbasin number
!!    rainsub(:,:)|mm H2O        |precipitation for the time step during the
!!                               |day in HRU
!!    eros_spl	  |none          |coefficient of splash erosion varing 0.9-3.1
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hhsedy(:,:)|tons           |sediment yield from HRU drung a time step
!!                               |applied to HRU

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!	  bed_shear		|N/m2		   |shear stress b/w stream bed and flow	
!!	  erod_k		|g/J		   |soil detachability value	
!!    jj			|none          |HRU number
!!    kk			|none          |time step of the day
!!	  ke_direct		|J/m2/mm	   |rainfall kinetic energy of direct throughfall
!!	  ke_leaf		|J/m2/mm	   |rainfall kinetic energy of leaf drainage
!!	  ke_total		|J/m2   	   |total kinetic energy of rainfall
!!	  percent_clay	|percent	   |percent clay
!!	  percent_sand	|percent	   |percent sand
!!	  percent_silt	|percent	   |percent silt
!!	  pheff     	|m			   |effective plant height
!!	  rdepth_direct	|mm			   |rainfall depth of direct throughfall
!!	  rdepth_leaf	|mm			   |rainfall depth of leaf drainage
!!	  rdepth_tot	|mm			   |total rainfall depth 
!!    rintnsty	    |mm/hr         |rainfall intensity
!!	  sedspl		|tons		   |sediment yield by rainfall impact during time step
!!	  sedov 		|tons		   |sediment yield by overland flow during time step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: log10, Exp, Real
!!
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!  Splash erosion model is adopted from EUROSEM model developed by Morgan (2001).
!!	Rill/interill erosion model is adoped from Modified ANSWERS model by Park et al.(1982)
!!  Code developed by J. Jeong and N. Kannan, BRC.

      use jrw_datalib_module
      use basin_module
      use climate_parms
      
	integer :: k, j
	real :: percent_clay, percent_silt, percent_sand, erod_k
	real :: ke_direct, ke_leaf, ke_total,pheff, c
	real :: rdepth_direct, rdepth_leaf, rdepth_tot, canopy_cover
	real :: bed_shear, sedov, sedspl, rain_d50, rintnsty

	j = ihru
	ulu = hru(j)%luse%urb_lu
      ilu = hru(j)%dbs%landuse
      
!! Fraction of sand
      percent_clay = soil(j)%phys(1)%clay
	percent_silt = soil(j)%phys(1)%silt 
	percent_sand = 100. - percent_clay - percent_silt

!! Soil detachability values adopted from EUROSEM User Guide (Table 1)
	if ((percent_clay>=40.) .and. (percent_sand>=20.) .and. 
     &(percent_sand<=45.)) then
	  erod_k = 2.0 !clay
      elseif ((percent_clay>=27.) .and. (percent_sand>=20.) .and.
     &(percent_sand<=45.)) then
	  erod_k = 1.7 !Clay loam
      elseif ((percent_silt<=40.).and.(percent_sand<=20.)) then
	  erod_k = 2.0 !Clay
      elseif ((percent_silt>40.).and.(percent_clay>=40.)) then
	  erod_k = 1.6 !Silty clay
      elseif ((percent_clay>=35.).and.(percent_sand>=45.)) then
	  erod_k = 1.9 !Sandy clay
      elseif ((percent_clay>=27.).and.(percent_sand<20.)) then
	  erod_k = 1.6 !Silty clay loam
      elseif ((percent_clay<=10.).and.(percent_silt>=80.)) then
	  erod_k = 1.2 !Silt
      elseif (percent_silt>=50.) then
	  erod_k = 1.5 !Silt loam
      elseif ((percent_clay>=7.) .and. (percent_sand<=52.) .and. 
     &(percent_silt>=28.)) then
	  erod_k = 2.0 !Loam
      elseif (percent_clay>=20.) then
	  erod_k = 2.1 !Sandy clay loam
      elseif (percent_clay>=percent_sand-70.) then
	  erod_k = 2.6 !Sandy loam
      elseif (percent_clay>=(2. * percent_sand) - 170.) then
	  erod_k = 3.0 !Loamy sand
      else
	  erod_k = 1.9 !Sand 
      end if
	
 !!	canopy cover based on leaf area index
!!	canopy cover is assumed to be 100% if LAI>=1
	  if(sumlai >= 1.) then
	    canopy_cover = 1.
	  else
	    canopy_cover = sumlai
	  end if

      do k=1,nstep
	  rintnsty = 60. * rainsub(j,k) / Real(bsn_prm%dts) 
	  rain_d50 = 0.188 * rintnsty ** 0.182

	  if(rintnsty>0) then
	
      !! Rainfall kinetic energy generated by direct throughfall (J/m^2/mm)
	    ke_direct = 8.95 + 8.44 * log10(rintnsty)
        if(ke_direct<0.) ke_direct = 0.
	  !! Rainfall kinetic energy generated by leaf drainage (J/m^2)
	    pheff = 0.5 * cht_mx(j)
	    ke_leaf = 15.8 * pheff ** 0.5 - 5.87
	    if (ke_leaf<0) ke_leaf = 0.

	  !! Depth of rainfall
	    rdepth_tot = rainsub(j,k) / (bsn_prm%dts * 60.)
	    rdepth_leaf = rdepth_tot * canopy_cover 
	    rdepth_direct = rdepth_tot - rdepth_leaf 
	  else
	    ke_direct = 0.
	    ke_leaf = 0.
		rdepth_tot = 0.
	    rdepth_leaf = 0.
	    rdepth_direct = 0.
	  endif

	!! total kinetic energy by rainfall (J/m^2)
	  ke_total = 0.001 * (rdepth_direct * ke_direct + rdepth_leaf * 
     &	  ke_leaf)

	!! total soil detachment by raindrop impact
	  sedspl = erod_k * ke_total * exp(-bsn_prm%eros_spl * 
     &    hhqday(k) / 1000.) * hru(j)%km ! tons

	!! Impervious area of HRU
	  if(hru(j)%luse%urb_lu > 0) sedspl = sedspl * (1.- urbdb(ulu)%fimp)

	!! maximum water depth that allows splash erosion
	  if(hhqday(k)>=3.* rain_d50.or.hhqday(k)<=1.e-3) sedspl = 0.


	!! Overland flow erosion 
    !! cover and management factor used in usle equation (ysed.f)
	  c = Exp((-.2231 - cvm_com(j)) *                                      
     &	  Exp(-.00115 * sol_cov(j)) + cvm_com(j))
	!! specific weight of water at 5 centigrate =9807N/m3
	  bed_shear = 9807 * (hhqday(k) / 1000.) * hru(j)%topo%slope ! N/m2
	  sedov = 11.02 * bsn_prm%rill_mult * soil(j)%ly(1)%usle_k * 
     &    bsn_prm%c_factor * c * bed_shear ** bsn_prm%eros_expo ! kg/hour/m2
	  if(bsn_cc%event>=2) then
	    sedov = 16.667 * sedov * hru(j)%km * bsn_prm%dts ! tons per time step
	  else
	    sedov = 24000. * sedov * hru(j)%km	! tons per day
	  endif

	!! Impervious area of HRU
	  if(hru(j)%luse%urb_lu > 0) sedov = sedov * (1.- urbdb(ulu)%fimp)

	  hhsedy(j,k) = dratio(inum1) * (sedspl + sedov)
	  if (hhsedy(j,k) < 1.e-10) hhsedy(j,k) = 0.

	end do
	return
      end subroutine ero_ovrsed