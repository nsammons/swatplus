      subroutine mgt_sched (isched)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name       |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name            |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    auto_eff(:) |none          |fertilizer application efficiency calculated
!!                               |as the amount of N applied divided by the
!!                               |amount of N removed at harvest
!!    icpst                      |icpst = 0 do not apply = 1 application period
!!    ipst_freq   |days          |number of days between applications
!!    iday_pest   |day           |current day between applications
!!    ndcpst      |day           |current day within the application period
!!    irramt(:)   |mm H20        |depth of irrigation water applied to HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module
      use basin_module
      
      j = ihru
      
      select case (mgt%op)

        case (1)    !! establish a plant community
            deallocate (pcom(ihru)%plg)
            deallocate (pcom(ihru)%plm) 
            deallocate (pcom(ihru)%plstr) 
            deallocate (pcom(ihru)%plcur) 
            !deallocate (hru(ihru)%pl_tot)
            deallocate (hru(ihru)%veg_ag)
            deallocate (hru(ihru)%grain)
            deallocate (hru(ihru)%root)
            deallocate (hru(ihru)%rsd_flt)
            deallocate (hru(ihru)%rsd_std)
            icom = mgt%op1
            ipl_com(j) = icom
            npl(j) = pcomdb(icom)%plants_com
            allocate (pcom(ihru)%plg(npl(j))) 
            allocate (pcom(ihru)%plm(npl(j))) 
            allocate (pcom(ihru)%plstr(npl(j))) 
            allocate (pcom(ihru)%plcur(npl(j))) 
            !allocate (hru(ihru)%pl_tot(npl(j)))
            allocate (hru(ihru)%veg_ag(npl(j)))
            allocate (hru(ihru)%grain(npl(j)))
            allocate (hru(ihru)%root(npl(j)))
            allocate (hru(ihru)%rsd_flt(npl(j)))
            allocate (hru(ihru)%rsd_std(npl(j)))
            !! allocate bacteria
            ibact_db = hru(ihru)%dbs%bact_init
            if (ibact_db > 0) then
              mbac = pesti_db(ibact_db)%num
              if (mbac > 0) then
                do ipl = 1, npl(j)
                  allocate (pcom(ihru)%plg(ipl)%bac(mbac))
                end do
              end if
            end if
        
            cvm_com(j) = 0.
            blai_com(j) = 0.
            tnylda(j) = 0.
            rsdco_plcom(j) = 0.
            do ipl = 1, npl(j)
              pcom(j)%plcur(ipl)%gro = pcomdb(icom)%pl(ipl)%igro
              pcom(j)%plcur(ipl)%idorm = 0
              idp = pcomdb(icom)%pl(ipl)%db_num
              pcom(j)%plg(ipl)%phumat = pcomdb(icom)%pl(ipl)%phu
              pcom(j)%plg(ipl)%lai = pcomdb(icom)%pl(ipl)%lai
              pcom(j)%plm(ipl)%mass = pcomdb(icom)%pl(ipl)%bioms
              pcom(j)%plcur(ipl)%phuacc = pcomdb(icom)%pl(ipl)%phuacc
              pcom(j)%plcur(ipl)%curyr_mat = pcomdb(icom)%pl(ipl)%yrmat
              cvm_com(j) = plcp(idp)%cvm + cvm_com(j)
              rsdco_plcom(j) = rsdco_plcom(j) + pldb(idp)%rsdco_pl
              pcom(j)%plcur(ipl)%idplt = pcomdb(icom)%pl(ipl)%db_num
              idp = pcom(j)%plcur(ipl)%idplt
              pcom(j)%plm(ipl)%p_fr = (pldb(idp)%pltpfr1 -    
     &        pldb(idp)%pltpfr3) *                                      
     &        (1.-pcom(j)%plcur(ipl)%phuacc/(pcom(j)%plcur(ipl)%phuacc+
     &        Exp(plcp(idp)%pup1 - plcp(idp)%pup2 *                     
     &        pcom(j)%plcur(ipl)%phuacc))) + pldb(idp)%pltpfr3
              pcom(j)%plm(ipl)%nmass=pcom(j)%plm(ipl)%n_fr *  
     &            pcom(j)%plm(ipl)%mass
              pcom(j)%plm(ipl)%n_fr = (pldb(idp)%pltnfr1- 
     &        pldb(idp)%pltnfr3)*(1.- pcom(j)%plcur(ipl)%phuacc / 
     &        (pcom(j)%plcur(ipl)%phuacc +
     &        Exp(plcp(idp)%nup1 - plcp(idp)%nup2 *                     
     &        pcom(j)%plcur(ipl)%phuacc))) + pldb(idp)%pltnfr3
              pcom(ihru)%plm(ipl)%pmass = pcom(j)%plm(ipl)%p_fr *
     &           pcom(j)%plm(ipl)%mass
              tnylda(j) = tnylda(j) + 350. * pldb(idp)%cnyld *          
     &                                   pldb(idp)%bio_e / npl(j)
              if (pcom(j)%plcur(ipl)%pop_com < 1.e-6) then
                pcom(j)%plg(ipl)%laimx_pop = pldb(idp)%blai
              else
                xx = pcom(j)%plcur(ipl)%pop_com / 1001.
                pcom(j)%plg(ipl)%laimx_pop = pldb(idp)%blai * xx / (xx +
     &               exp(pldb(idp)%pop1 - pldb(idp)%pop2 * xx))
              end if
              blai_com(j) = pcom(j)%plg(ipl)%laimx_pop + blai_com(j)
              
              if (pco%mout ==  1) then
                write (143, 1000) j, time%yrc,i_mo,iida, 
     &          pldb(idp)%plantnm, pcomdb(icom)%name, phubase(j),       
     &          pcom(j)%plcur(ipl)%phuacc,
     &          hru(j)%sol%sw, pcom(j)%plm(ipl)%mass, soil(j)%ly(1)%rsd,
     &          sol_sumno3(j),                                          
     &          sol_sumsolp(j),pcom(j)%plg(ipl)%lai,
     &          pcom(j)%plg(ipl)%laimx_pop
              end if
              cvm_com(j) = cvm_com(j) / npl(j)
            end do

          case (2)    !! plant one plant or entire community

            icom = mgt%op1
            do ipl = 1, npl(j)
              idp = pcomdb(icom)%pl(ipl)%db_num
              if (mgt%op2 == 0 .or. mgt%op2 == ipl) then
                pcom(j)%plcur(ipl)%gro = 1
                pcom(j)%plcur(ipl)%idorm = 0
              end if
              if (pco%mout ==  1) then
                write (143, 1000) j, time%yrc,i_mo,iida, 
     &          pldb(idp)%plantnm, pcomdb(icom)%name, phubase(j),       
     &          pcom(j)%plcur(ipl)%phuacc,
     &          hru(j)%sol%sw, pcom(j)%plm(ipl)%mass, soil(j)%ly(1)%rsd,
     &          sol_sumno3(j),                                          
     &          sol_sumsolp(j),pcom(j)%plg(ipl)%lai,
     &          pcom(j)%plg(ipl)%laimx_pop
              end if
            end do
            
          case (3)  !! harvest only operation
            ihk = mgt%op1
            ipl = amax1(1, mgt%op2)
            harvop = harvop_db(mgt%op1)
            hi_ovr = harvop%hi_ovr
            harveff = harvop%eff
            if (harveff <= 0.) then harveff = 1.0 

            do ipl = 1, npl(j)
            if (ihk == 0 .or. ihk == ipl) then
            
            !harvest specific type
            select case (harvop%typ)
            case ('biomass')    
              call mgt_harvestop
            case ('grain')
              call mgt_harvgrainop
            case ('residue')
            case ('tree')
            case ('tuber')
            end select
            
            !! sum yield and num. of harvest to calc ave yields
            pcom(j)%plg(ipl)%yield = pcom(j)%plg(ipl)%yield + yield
            pcom(j)%plcur(ipl)%harv_num = pcom(j)%plcur(ipl)%harv_num+1
            
            idp = pcom(j)%plcur(ipl)%idplt
            if (pco%mout ==  1) then
              write (143, 1001) j, time%yrc, i_mo, iida,
     *        pldb(idp)%plantnm,
     *    "HARVEST ONLY",phubase(j),pcom(j)%plcur(ipl)%phuacc,
     *        hru(j)%sol%sw, pcom(j)%plm(ipl)%mass,
     *        soil(j)%ly(1)%rsd, yield, strsn_sum(j), strsp_sum(j),
     *        strstmp_sum(j), strsw_sum(j), strsa_sum(j)
            end if
            end if
            end do
          
            case (4)   !! kill operation
              ihk = mgt%op1
              
              do ipl = 1, npl(j)
                if (ihk == 0 .or. ihk == ipl) then
                call mgt_killop
  
              if (pco%mout ==  1) then 
                write (143, 1000) j, time%yrc,i_mo, iida,
     *          "         ",
     *     "    KILL", phubase(j), pcom(j)%plcur(ipl)%phuacc,
     *          hru(j)%sol%sw, pcom(j)%plm(ipl)%mass, 
     *           soil(j)%ly(1)%rsd, sol_sumno3(j), sol_sumsolp(j)
              end if
            
              phubase(j) = 0.
              pcom(j)%plcur(ipl)%phuacc = 0.
              end if
              end do
       
          case (5)   !! harvest and kill operation
            if (mgt%op1 <= 0) mgt%op1 = 1
            harvop = harvop_db(mgt%op1)
            
            do ipl = 1, npl(j)
              biomass = pcom(j)%plm(ipl)%mass
              if (mgt%op2 == 0 .or. mgt%op2 == ipl) then
                          
              !harvest specific type
              select case (harvop%typ)
              case ('biomass')    
                call mgt_harvestop
              case ('grain')
                !hru(j)%pl_tot(ipl)%mass = 2000.
                call mgt_harvgrainop
              case ('residue')
              case ('tree')
              case ('tuber')
              end select
            
              call mgt_killop
              
              !call mgt_harvkillop
              !! sum yield and num. of harvest to calc ave yields
              pcom(j)%plg(ipl)%yield = pcom(j)%plg(ipl)%yield + yield
              pcom(j)%plcur(ipl)%harv_num=pcom(j)%plcur(ipl)%harv_num+1
            
              idp = pcom(j)%plcur(ipl)%idplt
              if (pco%mout == 1) then
                write (143, 1001) j, time%yrc, i_mo,iida,
     *    pldb(idp)%plantnm, "HARV/KILL", phubase(j), 
     *    pcom(j)%plcur(ipl)%phuacc,hru(j)%sol%sw,biomass,
     *    soil(j)%ly(1)%rsd, sol_sumno3(j),sol_sumsolp(j),yield, 
     *    strsn_sum(j), strsp_sum(j), strstmp_sum(j), strsw_sum(j), 
     *    strsa_sum(j)
1001  format (4i6,2a15,8f10.2,30x,5f10.2) 
              end if 
              end if
              pcom(j)%plcur(ipl)%phuacc = 0.
            end do
            phubase(j) = 0.
            
          case (6)   !! tillage operation
            idtill = mgt%op1
            ipl = amax1(1, mgt%op2)
            call mgt_newtillmix(j,0.)
            
            if (pco%mout == 1) then
              write (143, 1003) j, time%yrc, i_mo, iida,
     *        tilldb(idtill)%tillnm,
     *        "TILLAGE",phubase(j),pcom(j)%plcur(ipl)%phuacc,
     *        hru(j)%sol%sw, pcom(j)%plm(ipl)%mass, soil(j)%ly(1)%rsd,
     *        sol_sumno3(j), sol_sumsolp(j), tilldb(idtill)%effmix
1003  format (4i6,2a15,7f10.2,30x,f10.2)
            end if

          case (7)  !! irrigation operation
            irrop = irrop_db(mgt%op1)
            if (mgt%op3 > 1.e-6) irrop%amt_mm = mgt%op3
            if (irrop%eff < 1.e-6) irrop%eff = 1.0
            call mgt_irrsub
            
            if (pco%mout == 1) then
              write (143, 1002) j, time%yrc, i_mo, 
     *        iida, "        ", "IRRIGATE", phubase(j),                 
     *        pcom(j)%plcur(ipl)%phuacc,
     *        hru(j)%sol%sw,pcom(j)%plm(ipl)%mass, soil(j)%ly(1)%rsd,
     &        sol_sumno3(j),                                            
     &        sol_sumsolp(j),irramt(j), irr_sc(j), irr_no(j)
1002        format (4i6,2a15,7f10.2,10x,f10.2,70x,2i7)
            end if
          
          case (8)   !! auto irrigation operation 
            airrop = airrop_db(mgt%op1)
            iplt_airr(j) = amax1(1, mgt%op2)
            if (irrop%eff < 1.e-6) irrop%eff = 1.0
            wstrs_id(j) = airrop%wstr_id
            auto_wstr(j) = airrop%wstr_trig
            irr_eff(j) = airrop%eff
            irr_mx(j) = airrop%amt_mm
            if (irr_mx(j) <= 1.e-6) irr_mx(j) = mgt%op3
            irr_asq(j) = airrop%surq
            hru(j)%irrsrc%flag = 1
              
          case (9)    !! release/impound water in rice fields
            imp_trig(j) = mgt%op1
            ipl = amax1(1, mgt%op2)
          
            if (pco%mout ==  1) then
              write (143, 1000) j, time%yrc, i_mo,iida, 
     * "         ","RELEASE/IMPOUND", phubase(j),
     * pcom(j)%plcur(ipl)%phuacc, hru(j)%sol%sw,pcom(j)%plm(ipl)%mass,
     * soil(j)%ly(1)%rsd, sol_sumno3(j), sol_sumsolp(j)
            end if
          
          case (10)   !! fertilizer operation
            ipl = amax1(1, mgt%op2)
            fertop = fertop_db(mgt%op1)
            if (mgt%op3 > 1.e-6) fertop%amt_kgh = mgt%op3
            if (fertop%surface <= 1.e-6) fertop%surface = 0.2
            call pl_fert
            !call bac_apply_hrucon    THIS SHOULD BE CALLED FROM AN HRU CONTROL MODULE 
            !hru(j)%ly(1)%bacsol(ibac) = sol_bacsol
            !hru(j)%ly(1)%bacsor(ibac) = sol_bacsor
            
            if (pco%mout == 1) then
              write (143, 1004) j, time%yrc,i_mo,iida, 
     *        fertdb(fertop%db_num)%fertnm,
     *        "   FERT", phubase(j),pcom(j)%plcur(ipl)%phuacc,
     *        hru(j)%sol%sw, pcom(j)%plm(ipl)%mass, soil(j)%ly(1)%rsd, 
     *        sol_sumno3(j), sol_sumsolp(j), frt_kg, fertno3, fertnh3, 
     *        fertorgn, fertsolp, fertorgp
1004  format (4i6,2a15,7f10.2,20x,f10.2,10x,5f10.2)
            endif
   
          case (11)   !! auto fertilizer operation
            ipl = amax1(1, mgt%op2)
            autofertop = autofertop_db(mgt%op1)
            iafrttyp(j) = amax1 (autofertop%db_num, 1)
            nstress(j) = autofertop%option
            auto_nstrs(j) = autofertop%str_trig
            iplt_afert(j) = autofertop%plant_trig
            auto_napp(j) = autofertop%amt_kgh
            auto_nyr(j) = autofertop%ann_mx
            auto_eff(j) = autofertop%eff
            afrt_surface(j) = autofertop%surface

            !! calculate tnylda for autofertilization
            ncrp = idp
            if (pldb(ncrp)%hvsti < 1.) then
              tnylda(j) = 350. * pldb(ncrp)%cnyld * pldb(ncrp)%bio_e
            else
              tnylda(j) = 1000. * pldb(ncrp)%cnyld * pldb(ncrp)%bio_e
            endif
         
          case (12)    !! continuous fertilization operation
            ipl = amax1(1, mgt%op2)
            contfertop = contfertop_db(mgt%op1)
            fert_days(j) = contfertop%days
            cfrt_id(j) = contfertop%db_num
            ifrt_freq(j) = contfertop%freq
            cfrt_kg(j) = contfertop%amt_kgh
            icfrt(j) = 1
            ndcfrt(j) = 1
            iday_fert(j) = ifrt_freq(j)
 
          case (13)   !! pesticide operation
            pestop = pestop_db(mgt%op1)
            if (mgt%op3 > 1.e-6) pestop%amt_kgh = mgt%op3
            
            call pl_apply
            
            if (pco%mout == 1) then
              write (143, 1004) j, time%yrc,i_mo,iida, 
     *        pestdb(ipest)%pestnm, "   PEST", phubase(j),              
     *        pcom(j)%plcur(ipl)%phuacc,
     *        hru(j)%sol%sw,pcom(j)%plm(ipl)%mass, 
     *        soil(j)%ly(1)%rsd,sol_sumno3(j),sol_sumsolp(j),pst_kg
            endif

          case (14)    !! continuous pesticide operation
            ipl = amax1(1, mgt%op2)
            contpestop = contpestop_db(mgt%op1)
            pest_days(j) = contpestop%days
            cpst_id(j) = contpestop%db_num
            ipst_freq(j) = contpestop%freq
            cpst_kg(j) = contpestop%amt_kgh
            icpst(j) = 1
            ndcpst(j) = 0
            iday_pest(j) = ipst_freq(j)

          case (15)    !! grazing operation
            ndeat(j) = 0
            igrz(j) = 1
            ipl = amax1(1, mgt%op2)
            grazeop = grazeop_db(mgt%op1)
            manure_id(j) = grazeop%manure_dbnum
            grz_days(j) = grazeop%days
            bio_eat(j) = grazeop%eat
            bio_trmp(j) = grazeop%tramp
            if (grazeop%manure < = 0.) then 
              grazeop%manure = 0.95 * grazeop%eat
            end if
            manure_kg(j) = grazeop%manure
          
            if (pco%mout ==  1) then
              write (143, 1005) j, time%yrc,i_mo,iida,
     *        "         ",
     *     "   GRAZE", phubase(j), pcom(j)%plcur(ipl)%phuacc,
     *     hru(j)%sol%sw, pcom(j)%plm(ipl)%mass, 
     *     soil(j)%ly(1)%rsd,sol_sumno3(j),sol_sumsolp(j),manure_kg(j)
1005  format (4i6,2a15,7f10.2,20x,f10.2)
            end if

          case (16)   !! burning
            burn_frlb = mgt%op3
            ipl = amax1(1, mgt%op2)
            call pl_burnop
            if (pco%mout ==  1) then
              write (143, 1000) j, time%yrc, i_mo,iida, 
     *        "         ",
     *     "      BURN", phubase(j),pcom(j)%plcur(ipl)%phuacc,
     *        hru(j)%sol%sw, pcom(j)%plm(ipl)%mass, 
     *        soil(j)%ly(1)%rsd,sol_sumno3(j),sol_sumsolp(j)
            end if

          case (17)   !! street sweeping (only if iurban=2)
            ipl = amax1(1, mgt%op2)
            sweepop = sweepop_db(mgt%op1)
            sweepeff = sweepop%eff
            fr_curb = sweepop%fr_curb
            
            if (pco%mout ==  1) then
              write (143, 1000) j, time%yrc, i_mo,iida, 
     *        "         ",
     * "STREET SWEEP",phubase(j),pcom(j)%plcur(ipl)%phuacc,
     *        hru(j)%sol%sw, pcom(j)%plm(ipl)%mass, 
     *        soil(j)%ly(1)%rsd, sol_sumno3(j), sol_sumsolp(j)
            end if
               
          case (18)    !! print plant community status to output.mgt
            do ipl = 1, pcomdb(icom)%plants_com
              idp = pcom(j)%plcur(ipl)%idplt
              write (143, 1000) j, time%yrc, i_mo,iida,  
     &        pldb(idp)%plantnm, pcomdb(icom)%name, phubase(j),         
     &        pcom(j)%plcur(ipl)%phuacc,
     &        hru(j)%sol%sw, pcom(j)%plm(ipl)%mass, soil(j)%ly(1)%rsd, 
     &        sol_sumno3(j),                                            
     &        sol_sumsolp(j), pcom(j)%plg(ipl)%lai,                       
     &        pcom(j)%plg(ipl)%laimx_pop
            end do
          
          case (20)    !! skip a year
            yr_skip(j) = 1

      end select

      if (mgt%op /= 20) nop(j) = nop(j) + 1  !don't icrement if skip year
      if (nop(j) > sched(isched)%num_ops) then
        nop(j) = 1
      end if
      
      mgt = sched(isched)%mgt_ops(nop(j))
      
1000  format (4i6,2a15,9f10.2)    
      return

      end subroutine mgt_sched