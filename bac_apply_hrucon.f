       subroutine bac_apply_hrucon
      
      
      !! calculate ground cover
      !! graze only if adequate biomass in HRU
            
!!    this subroutine applies bacteria leached to the plants and soil 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!         ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ibtyp        |NA            |bacteria type from 'bact_parms.dat'
!!    sol_bacsol   |# cfu/m^2     |soluble bacteria in soil layer
!!    sol_prk      |mm            |water percolating through the soil layer
!!    sol_wt       |mm            |soil weight
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name              |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    baclch_out        |# cfu/m^2     |bacteria leached to the next layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ MODULES USED ~ ~ ~
!!    bac_ls_parms   |type bacteria_db - contains  'bac_lsparms_read.f'

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      if (bioms_tot > bioms_min) then
        gc = (1.99532 - erfc(1.333 * sumlai - 2.)) / 2.1
        if (gc < 0.) gc = 0.
        gc1 = 1. - gc
        do ibac = 1, bact(ibacdb)%num
          ibtyp = bact(ibacdb)%bac(ibac)%num_db
          frt_t = bac_db(ibtyp)%swf * manure_kgh / 1000.
          do ipl = 1, npl(j)
            call bac_apply_plant (ibacdb, ibac, gc,frt_t, 
     &            bac_db(ibtyp)%kd, pcom(j)%plg(ipl)%bac(ibac), 
     &            hru(j)%ly(1)%bacsol(ibac), hru(j)%ly(1)%bacsor(ibac))
          end do
          call bac_apply_soil (ibacdb, ibac, gc, frt_t, 
     &             
     &            hru(j)%ly(1)%bacsol(ibac), hru(j)%ly(1)%bacsor(ibac))
        end do
      end if
      
      end subroutine bac_apply_hrucon