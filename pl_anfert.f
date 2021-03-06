      subroutine pl_anfert
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine automatically applies Nitrogen and Phosphorus when
!!    Nitrogen stress exceeds a user input threshhold.  

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    fminn(:)    |kg minN/kg frt|fraction of fertilizer which is mineral
!!                               |nitrogen (NO3 + NH3)
!!    fminp(:)    |kg minP/kg frt|fraction of fertilizer which is mineral
!!                               |phosphorus
!!    fnh3n(:)    |kg NH3-N/kg N |fraction of mineral N content of
!!                               |fertilizer which is NH3
!!    forgn(:)    |kg orgN/kg frt|fraction of fertilizer which is organic
!!                               |nitrogen
!!    forgp(:)    |kg orgP/kg frt|fraction of fertilizer which is organic
!!                               |phosphorus
!!    afrt_surface(:) |none          |fraction of fertilizer which is applied
!!                               |to top 10 mm of soil (the remaining
!!                               |fraction is applied to first soil
!!                               |layer)
!!    auto_nyr(:) |kg NO3-N/ha   |maximum NO3-N content allowed to be
!!                               |applied in one year by auto-fertilization
!!    auto_napp(:)|kg NO3-N/ha   |maximum NO3-N content allowed in one
!!                               |fertilizer application
!!    auto_nstrs(:)|none          |nitrogen stress factor which triggers
!!                               |auto fertilization
!!    bactkddb(:) |none          |fraction of bacteria in solution (the
!!                               |remaining fraction is sorbed to soil
!!                               |particles)
!!    bactlpdb(:) |# bact/kg frt |concentration of less persistent
!!                               |bacteria in fertilizer
!!    bactlpq(:)  |# colonies/ha |less persistent bacteria in soil solution
!!    bactlps(:)  |# colonies/ha |less persistent bacteria attached to soil
!!                               |particles
!!    bactpdb(:)  |# bact/kg frt |concentration of persistent bacteria in
!!                               |fertilizer
!!    bactpq(:)   |# colonies/ha |persistent bacteria in soil solution
!!    bactps(:)   |# colonies/ha |persistent bacteria attached to soil particles
!!    curyr       |none          |current year of simulation
!!    hru_dafr(:) |km**2/km**2   |fraction of watershed area in HRU
!!    icr(:)      |none          |sequence number of crop grown within the
!!                               |current year
!!    ihru        |none          |HRU number
!!    tnylda(:)   |kg N/kg yield |estimated/target nitrogen content of
!!                               |yield used in autofertilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    anano3(:)   |kg N/ha       |total amount of nitrogen applied during the
!!                               |year in auto-fertilization
!!    bactlpq(:)  |# colonies/ha |less persistent bacteria in soil solution
!!    bactlps(:)  |# colonies/ha |less persistent bacteria attached to soil
!!                               |particles
!!    bactpq(:)   |# colonies/ha |persistent bacteria in soil solution
!!    bactps(:)   |# colonies/ha |persistent bacteria attached to soil particles
!!    tauton(:)   |kg N/ha       |amount of N applied in autofert operation in
!!                               |year
!!    tautop(:)   |kg P/ha       |amount of P applied in autofert operation in
!!                               |year
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dwfert      |kg fert/ha    |amount of fertilizer to be applied to meet
!!                               |nitrogen requirement
!!    j           |none          |HRU number
!!    ly          |none          |counter (soil layers)
!!    nstress     |none          |code for approach used to determine amount
!!                               |of nitrogen to HRU
!!                               |0 nitrogen target approach
!!                               |1 annual max approach
!!    rtoaf       |none          |weighting factor used to partition the
!!                               |organic N & P content of the fertilizer
!!                               |between the fresh organic and the active
!!                               |organic pools
!!    targn       |kg N/ha       |target mineral N application
!!    tfp         |kg minP/kg frt|fraction of mineral P to be applied
!!    tpno3       |
!!    tsno3       |
!!    xx          |none          |fraction of total amount of fertilizer to
!!                               |be applied to layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module
      use basin_module

      real, parameter :: rtoaf = 0.50
      integer :: j, ly, ifrt
      real :: tsno3, tpno3, dwfert, xx, targn, tfp

      j = ihru

      ifrt = iafrttyp(j)
      ipl = iplt_afert(j)

!! determine amount of mineral N to be applied
      if (pcom(j)%plstr(ipl)%strsn < auto_nstrs(j)) then
        targn = 0.
        if (nstress(j) == 0) then                !! n target approach
         tsno3 = 0.
         tpno3 = 0.
         do ly = 1, hru(j)%sol%nly
           tsno3 = tsno3 + soil(j)%nut(ly)%no3 + soil(j)%nut(ly)%nh3
         end do
         tpno3 = pcom(j)%plm(ipl)%nmass

         targn = tnylda(j) - tsno3 - tpno3
         if (targn > auto_napp(j)) targn = auto_napp(j)
         if (targn < 0.) targn = 0.

         anano3(j) = anano3(j) + targn
         if (anano3(j) >= auto_nyr(j)) then
           targn = auto_nyr(j) - (anano3(j) - targn)
           if (targn < 0.) targn = 0.
           anano3(j) = auto_nyr(j)
         endif

        else                                  !! annual max approach
          targn = auto_napp(j) * (1. - pcom(j)%plcur(ipl)%phuacc)
          if (targn > auto_napp(j)) targn = auto_napp(j)

          anano3(j) = anano3(j) + targn
          if (anano3(j) >= auto_nyr(j)) then
            targn = auto_nyr(j) - (anano3(j) - targn)
            anano3(j) = auto_nyr(j)
          endif
        endif
        if (targn <= 1.e-6) return


!! add nutrients to soil based on nitrogen need
        dwfert = 0.
        if (fertdb(ifrt)%fminn > 0.0001) then
          dwfert = targn / fertdb(ifrt)%fminn
        else
          dwfert = 0.
        endif
 
        !! add bacteria to surface layer
        bactpq(j) = bactpq(j) + fertdb(ifrt)%bactkddb *                 
     &                                  fertdb(ifrt)%bactpdb * dwfert
        bactlpq(j) = bactlpq(j) + fertdb(ifrt)%bactkddb *               
     &                                 fertdb(ifrt)%bactlpdb * dwfert
        bactps(j) = bactps(j) + (1. - fertdb(ifrt)%bactkddb) *          
     &                                  fertdb(ifrt)%bactpdb * dwfert
        bactlps(j) = bactlps(j) + (1. - fertdb(ifrt)%bactkddb) *        
     &                                 fertdb(ifrt)%bactlpdb * dwfert

        do ly = 1, 2
          xx = 0.
          if (ly == 1) then
            xx = afrt_surface(j)
          else
            xx = 1. - afrt_surface(j)
          endif
  
          soil(j)%nut(ly)%no3 = soil(j)%nut(ly)%no3 + xx * dwfert *
     &            fertdb(ifrt)%fminn *  (1. - fertdb(ifrt)%fnh3n)
          soil(j)%nut(ly)%nh3 = soil(j)%nut(ly)%nh3 + xx * dwfert *
     &              fertdb(ifrt)%fminn * fertdb(ifrt)%fnh3n

          if (bsn_cc%cswat == 0) then
		  soil(j)%nut(ly)%fon = soil(j)%nut(ly)%fon + rtoaf * xx * dwfert
     &                    * fertdb(ifrt)%forgn
            soil(j)%nut(ly)%aorgn = soil(j)%nut(ly)%aorgn + (1. - rtoaf)
     &                 * xx * dwfert * fertdb(ifrt)%forgn
            soil(j)%nut(ly)%fop = soil(j)%nut(ly)%fop + rtoaf*xx*dwfert 
     &                    * fertdb(ifrt)%forgp
            soil(j)%nut(ly)%orgp=soil(j)%nut(ly)%orgp + (1.-rtoaf)*xx * 
     &                    dwfert* fertdb(ifrt)%forgp
	    end if
	    if (bsn_cc%cswat == 1) then
            soil(j)%cbn(ly)%mc = soil(j)%cbn(ly)%mc + xx * dwfert *
     &                                       fertdb(ifrt)%forgn * 10.
            soil(j)%nut(ly)%mn = soil(j)%nut(ly)%mn + xx * dwfert *
     &                                       fertdb(ifrt)%forgn
            soil(j)%nut(ly)%mp = soil(j)%nut(ly)%mp + xx * dwfert *
     &                                       fertdb(ifrt)%forgp
	    end if

	    !! add by zhang
	    !!=================
	    if (bsn_cc%cswat == 2) then
            soil(j)%nut(ly)%fop = soil(j)%nut(ly)%fop + rtoaf*xx*dwfert 
     &                    * fertdb(ifrt)%forgp
            soil(j)%nut(ly)%orgp=soil(j)%nut(ly)%orgp+(1. - rtoaf)*xx * 
     &                    dwfert* fertdb(ifrt)%forgp	    
            !!Allocate organic fertilizer to Slow (SWAT_active) N pool;
            soil(j)%cbn(ly)%hsn = soil(j)%cbn(ly)%hsn + (1. - rtoaf)*xx
     &                    * dwfert * fertdb(ifrt)%forgn
            soil(j)%nut(ly)%aorgn = soil(j)%cbn(ly)%hsn

          !orgc_f is the fraction of organic carbon in fertilizer
          !for most fertilziers this value is set to 0.
              orgc_f = 0.0 
              
          !X1 is fertlizer applied to layer (kg/ha)
          !xx is fraction of fertilizer applied to layer
              X1 = xx * dwfert 
              X8 = X1 * orgc_f
              RLN = .175 *(orgc_f)/(fertdb(ifrt)%fminn +                
     &                                    fertdb(ifrt)%forgn + 1.e-5)
              X10 = .85-.018*RLN
              if (X10<0.01) then
                X10 = 0.01
              else
                if (X10 > .7) then
                    X10 = .7
                end if
              end if
              XXX = X8 * X10
              soil(j)%cbn(ly)%lmc = soil(j)%cbn(ly)%lmc + XXX
              YY = X1 * X10
              soil(j)%cbn(ly)%lm = soil(j)%cbn(ly)%lm + YY
              
              ZZ = X1 *rtoaf * fertdb(ifrt)%forgn * X10
              
              soil(j)%cbn(ly)%lmn = soil(j)%cbn(ly)%lmn + ZZ
              soil(j)%cbn(ly)%lsn = soil(j)%cbn(ly)%lsn + X1
     &                      *fertdb(ifrt)%forgn -ZZ
              XZ = X1 *orgc_f-XXX
              soil(j)%cbn(ly)%lsc = soil(j)%cbn(ly)%lsc + XZ
              soil(j)%cbn(ly)%lslc = soil(j)%cbn(ly)%lslc + XZ * .175 
              soil(j)%cbn(ly)%lslnc=soil(j)%cbn(ly)%lslnc + XZ*(1.175)
              YZ = X1 - YY
              soil(j)%cbn(ly)%ls = soil(j)%cbn(ly)%ls + YZ
              soil(j)%cbn(ly)%lsl = soil(j)%cbn(ly)%lsl + YZ*.175
              
              soil(j)%nut(ly)%fon=soil(j)%cbn(ly)%lmn+
     &                 soil(j)%cbn(ly)%lsn
	    
	    end if
	    !! add by zhang
	    !!=================

          !! check for P stress
          tfp = 0.
          if (pcom(j)%plstr(ipl)%strsp <= 0.75) then
            tfp = fertdb(ifrt)%fminn / 7.
          else
            tfp = fertdb(ifrt)%fminp
          end if
          soil(j)%nut(ly)%solp = soil(j)%nut(ly)%solp + xx * dwfert*tfp
        end do
        

!! summary calculations
          auton = auton + dwfert * (fertdb(ifrt)%fminn +                
     &                                             fertdb(ifrt)%forgn)
          autop = autop + dwfert * (tfp + fertdb(ifrt)%forgp)
          tauton(j) = tauton(j) + auton
          tautop(j) = tautop(j) + autop
        
        if (pco%mout ==  1) then
              write (143, 1000) j, time%yrc,i_mo,iida, 
     *        fertdb(autofertop%db_num)%fertnm,
     *      "AUTOFERT", phubase(j),pcom(j)%plcur(ipl)%phuacc,
     *        hru(j)%sol%sw, pcom(j)%plm(ipl)%mass,
     *        soil(j)%ly(1)%rsd,sol_sumno3(j),sol_sumsolp(j), dwfert,
     *        fertno3, fertnh3, fertorgn, fertsolp, fertorgp            
        end if
      
      endif
      
1000  format (4i6,2a15,7f10.2,20x,f10.2,10x,5f10.2) 
      
      return
      end subroutine pl_anfert