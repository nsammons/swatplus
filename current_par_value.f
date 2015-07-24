      subroutine current_par_value
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine finds the current paramter value based on 
!!    user defined change

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    val_cur     |variable      |current parameter value
!!                               |the standard temperature (20 degrees C)
!!    chg         |data type     |contains information on variable change
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chg_par     |variable      |new parameter value
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      use jrw_datalib_module

      ielem = chg_prm(ichg_par)%num(ispu)
      
      select case (chg_prm(ichg_par)%name)
          
      case ("cn2")
        cn2(ielem) = chg_par (cn2(ielem), chg_prm(ichg_par))  

      !! HRU  
      case ("usle_p")
        hru(ielem)%luse%usle_p = chg_par (hru(ielem)%luse%usle_p, 
     &                    chg_prm(ichg_par))
        
      case ("ovn")
        hru(ielem)%luse%ovn = chg_par (hru(ielem)%luse%ovn, 
     &                    chg_prm(ichg_par))
        
      case ("elev")
        hru(ielem)%topo%elev = chg_par (hru(ielem)%topo%elev, 
     &                    chg_prm(ichg_par))
        
      case ("slope")
        hru(ielem)%topo%slope = chg_par (hru(ielem)%topo%slope, 
     &                    chg_prm(ichg_par))
        
      case ("slope_len")
        hru(ielem)%topo%slope_len = chg_par(hru(ielem)%topo%slope_len,
     &                    chg_prm(ichg_par))
        
      case ("lat_ttime")
        hru(ielem)%hyd%lat_ttime = chg_par(hru(ielem)%hyd%lat_ttime,
     &                    chg_prm(ichg_par))
            
      case ("lat_sed")
        hru(ielem)%hyd%lat_sed = chg_par (hru(ielem)%hyd%lat_sed, 
     &                    chg_prm(ichg_par))
        
      case ("lat_len")
        hru(ielem)%topo%lat_len = chg_par (hru(ielem)%topo%lat_len, 
     &                    chg_prm(ichg_par))
        
      case ("canmx")
        hru(ielem)%hyd%canmx = chg_par (hru(ielem)%hyd%canmx, 
     &                    chg_prm(ichg_par))
        
      case ("esco")
        hru(ielem)%hyd%esco = chg_par (hru(ielem)%hyd%esco, 
     &                    chg_prm(ichg_par))
         
      case ("epco")
        hru(ielem)%hyd%epco = chg_par (hru(ielem)%hyd%epco, 
     &                    chg_prm(ichg_par))
        
      case ("erorgn")
        hru(ielem)%hyd%erorgn = chg_par (hru(ielem)%hyd%erorgn, 
     &                    chg_prm(ichg_par))
        
      case ("erorgp")
        hru(ielem)%hyd%erorgp = chg_par (hru(ielem)%hyd%erorgp, 
     &                    chg_prm(ichg_par))
        
      case ("evpot")
        hru(ielem)%hyd%evpot = chg_par (hru(ielem)%hyd%evpot, 
     &                    chg_prm(ichg_par))
        
      case ("dis_stream")
        hru(ielem)%topo%dis_stream=chg_par(hru(ielem)%topo%dis_stream,
     &                    chg_prm(ichg_par))
        
      case ("biomix")
        hru(ielem)%hyd%biomix = chg_par (hru(ielem)%hyd%biomix, 
     &                    chg_prm(ichg_par))
        
      case ("dep_imp")
        hru(ielem)%hyd%dep_imp = chg_par (hru(ielem)%hyd%dep_imp, 
     &                    chg_prm(ichg_par))
        
      case ("lat_orgn")
        hru(ielem)%hyd%lat_orgn = chg_par (hru(ielem)%hyd%lat_orgn, 
     &                    chg_prm(ichg_par))
      
      case ("lat_orgp")
        hru(ielem)%hyd%lat_orgp = chg_par (hru(ielem)%hyd%lat_orgp, 
     &                    chg_prm(ichg_par))
        
      case ("field_len")
        hru(ielem)%field%len = chg_par(hru(ielem)%field%len,
     &                    chg_prm(ichg_par))
        
      case ("field_wid")
        hru(ielem)%field%wid = chg_par(hru(ielem)%field%wid,
     &                    chg_prm(ichg_par))
        
      case ("field_ang")
        hru(ielem)%field%ang = chg_par(hru(ielem)%field%ang,
     &                    chg_prm(ichg_par))
               
      !! SOL  
      case ("anion_excl")
        sol(isol)%s%anion_excl = chg_par(sol(isol)%s%anion_excl,
     &                    chg_prm(ichg_par))
         
      case ("crk")
         sol(isol)%s%crk = chg_par(sol(isol)%s%crk,
     &                    chg_prm(ichg_par))
         
      case ("z")
        do ly = 1, hru(ielem)%sol%nly
          soil(ielem)%phys(ly)%d = chg_par(soil(ielem)%phys(ly)%d,
     &                    chg_prm(ichg_par))
        end do
         
      case ("bd")
        do ly = 1, hru(ielem)%sol%nly
          soil(ielem)%phys(ly)%bd = chg_par(soil(ielem)%phys(ly)%bd,
     &                    chg_prm(ichg_par))
        end do
         
      case ("awc")
        do ly = 1, hru(ielem)%sol%nly
          soil(ielem)%phys(ly)%awc = chg_par(soil(ielem)%phys(ly)%awc,
     &                    chg_prm(ichg_par))
        end do
         
      case ("k")
        do ly = 1, hru(ielem)%sol%nly
          soil(ielem)%phys(ly)%k = chg_par(soil(ielem)%phys(ly)%k,
     &                    chg_prm(ichg_par))
        end do
         
      case ("cbn")
        do ly = 1, hru(ielem)%sol%nly
          soil(ielem)%cbn(ly)%cbn = chg_par(soil(ielem)%cbn(ly)%cbn,
     &                    chg_prm(ichg_par))
        end do
         
      case ("clay")
        do ly = 1, hru(ielem)%sol%nly
          soil(ielem)%phys(ly)%clay = chg_par(soil(ielem)%phys(ly)%clay,
     &                    chg_prm(ichg_par))
        end do
         
      case ("silt")
        do ly = 1, hru(ielem)%sol%nly
          soil(ielem)%phys(ly)%silt = chg_par(soil(ielem)%phys(ly)%silt,
     &                    chg_prm(ichg_par))
        end do
         
      case ("sand")
        do ly = 1, hru(ielem)%sol%nly
          soil(ielem)%phys(ly)%sand = chg_par(soil(ielem)%phys(ly)%sand,
     &                    chg_prm(ichg_par))
        end do
         
      case ("rock")
        do ly = 1, hru(ielem)%sol%nly
          soil(ielem)%phys(ly)%rock = chg_par(soil(ielem)%phys(ly)%rock,
     &                    chg_prm(ichg_par))
        end do
         
      case ("alb")
        do ly = 1, hru(ielem)%sol%nly
          soil(ielem)%ly(ly)%alb = chg_par(soil(ielem)%ly(ly)%alb,
     &                    chg_prm(ichg_par))
        end do
         
      case ("usle_k")
        do ly = 1, hru(ielem)%sol%nly
          soil(ielem)%ly(ly)%usle_k = chg_par(soil(ielem)%ly(ly)%usle_k,
     &                    chg_prm(ichg_par))
        end do
         
      case ("ec")
        do ly = 1, hru(ielem)%sol%nly
          soil(ielem)%ly(ly)%ec = chg_par(soil(ielem)%ly(ly)%ec,
     &                    chg_prm(ichg_par))
        end do
         
      case ("cal")
        do ly = 1, hru(ielem)%sol%nly
          soil(ielem)%ly(ly)%cal = chg_par(soil(ielem)%ly(ly)%cal,
     &                    chg_prm(ichg_par))
        end do
      
      case ("ph")
        do ly = 1, hru(ielem)%sol%nly
           soil(ielem)%ly(ly)%ph = chg_par(soil(ielem)%ly(ly)%ph,
     &                    chg_prm(ichg_par))
        end do
        
       
       !! BSN
      case ("surlag")
        bsn_prm%surlag = chg_par(bsn_prm%surlag,
     &                    chg_prm(ichg_par))
        
      case ("adj_pkr")
        bsn_prm%adj_pkr = chg_par(bsn_prm%adj_pkr,
     &                    chg_prm(ichg_par))
        
      case ("prf")
        bsn_prm%prf = chg_par(bsn_prm%prf,
     &                    chg_prm(ichg_par))
        
      case ("spcon")
        bsn_prm%spcon = chg_par(bsn_prm%spcon,
     &                    chg_prm(ichg_par))
        
      case ("spexp")
        bsn_prm%spexp = chg_par(bsn_prm%spexp,
     &                    chg_prm(ichg_par))
        
      case ("evrch")
        bsn_prm%evrch = chg_par(bsn_prm%evrch,
     &                    chg_prm(ichg_par))
        
      case ("evlai")
        bsn_prm%evlai = chg_par(bsn_prm%evlai,
     &                    chg_prm(ichg_par))
        
      case ("ffcb")
        bsn_prm%ffcb = chg_par(bsn_prm%ffcb,
     &                    chg_prm(ichg_par))
        
      case ("cmn")
        bsn_prm%cmn = chg_par(bsn_prm%cmn,
     &                    chg_prm(ichg_par))
        
      case ("nperco")
        bsn_prm%nperco = chg_par(bsn_prm%nperco,
     &                    chg_prm(ichg_par))
        
      case ("pperco")
        bsn_prm%pperco = chg_par(bsn_prm%pperco,
     &                    chg_prm(ichg_par))
        
      case ("phoskd")
        bsn_prm%phoskd = chg_par(bsn_prm%phoskd,
     &                    chg_prm(ichg_par))
        
      case ("psp")
        bsn_prm%psp = chg_par(bsn_prm%psp,
     &                    chg_prm(ichg_par))
        
      case ("rsdco")
        bsn_prm%rsdco = chg_par(bsn_prm%rsdco,
     &                    chg_prm(ichg_par))
        
      case ("percop")
        bsn_prm%percop = chg_par(bsn_prm%percop,
     &                    chg_prm(ichg_par))
        
      case ("msk_co1")
        bsn_prm%msk_co1= chg_par(bsn_prm%msk_co1,
     &                    chg_prm(ichg_par))
        
      case ("msk_co2")
        bsn_prm%msk_co2 = chg_par(bsn_prm%msk_co2,
     &                    chg_prm(ichg_par))
        
      case ("msk_x")
        bsn_prm%msk_x = chg_par(bsn_prm%msk_x,
     &                    chg_prm(ichg_par))
        
      case ("cncoef")
        bsn_prm%cncoef = chg_par(bsn_prm%cncoef,
     &                    chg_prm(ichg_par))
        
      case ("trnsrch")
        bsn_prm%trnsrch = chg_par(bsn_prm%trnsrch,
     &                    chg_prm(ichg_par))
        
      case ("cdn")
        bsn_prm%cdn = chg_par(bsn_prm%cdn,
     &                    chg_prm(ichg_par))
         
      case ("tb_adj")
        bsn_prm%tb_adj = chg_par(bsn_prm%tb_adj,
     &                    chg_prm(ichg_par))
        
      case ("sdnco")
        bsn_prm%sdnco = chg_par(bsn_prm%sdnco,
     &                    chg_prm(ichg_par))
        
      case ("n_updis")
        bsn_prm%n_updis = chg_par(bsn_prm%n_updis,
     &                    chg_prm(ichg_par))
        
      case ("p_updis")
        bsn_prm%p_updis = chg_par(bsn_prm%p_updis,
     &                    chg_prm(ichg_par))
        
      case ("dorm_hr")
        bsn_prm%dorm_hr = chg_par(bsn_prm%dorm_hr,
     &                    chg_prm(ichg_par))
        
      case ("smxco")
        bsn_prm%smxco = chg_par(bsn_prm%smxco,
     &                    chg_prm(ichg_par))
        
      case ("fixco")
        bsn_prm%fixco = chg_par(bsn_prm%fixco,
     &                    chg_prm(ichg_par))

!!     SWQ
      case ("rs1")
          ch_nut(ielem)%rs1 = chg_par(ch_nut(ielem)%rs1,
     &                    chg_prm(ichg_par))
         
       case ("rs2")
          ch_nut(ielem)%rs2 = chg_par(ch_nut(ielem)%rs2,
     &                    chg_prm(ichg_par))
        
       case ("rs3")
          ch_nut(ielem)%rs3 = chg_par(ch_nut(ielem)%rs3,
     &                    chg_prm(ichg_par)) 
        
       case ("rs4")
          ch_nut(ielem)%rs4 = chg_par(ch_nut(ielem)%rs4,
     &                    chg_prm(ichg_par))
        
       case ("rs5")
          ch_nut(ielem)%rs5 = chg_par(ch_nut(ielem)%rs5,
     &                    chg_prm(ichg_par))
        
       case ("rs6")
          ch_nut(ielem)%rs6 = chg_par(ch_nut(ielem)%rs6,
     &                    chg_prm(ichg_par)) 
        
       case ("rs7")
          ch_nut(ielem)%rs7 = chg_par(ch_nut(ielem)%rs7,
     &                    chg_prm(ichg_par))
        
       case ("rk1")
          ch_nut(ielem)%rk1 = chg_par(ch_nut(ielem)%rk1,
     &                    chg_prm(ichg_par))
        
       case ("rk2")
          ch_nut(ielem)%rk2 = chg_par(ch_nut(ielem)%rk2,
     &                    chg_prm(ichg_par))
        
       case ("rk3")
          ch_nut(ielem)%rk3 = chg_par(ch_nut(ielem)%rk3,
     &                    chg_prm(ichg_par)) 
        
       case ("rk4")
          ch_nut(ielem)%rk4 = chg_par(ch_nut(ielem)%rk4,
     &                    chg_prm(ichg_par)) 
        
       case ("rk5")
          ch_nut(ielem)%rs2 = chg_par(ch_nut(ielem)%rs2,
     &                    chg_prm(ichg_par)) 
        
       case ("rk6")
          ch_nut(ielem)%rk6 = chg_par(ch_nut(ielem)%rk6,
     &                    chg_prm(ichg_par))
        
       case ("bc1")
          ch_nut(ielem)%bc1 = chg_par(ch_nut(ielem)%bc1,
     &                    chg_prm(ichg_par))
        
       case ("bc2")
          ch_nut(ielem)%bc2 = chg_par(ch_nut(ielem)%bc2,
     &                    chg_prm(ichg_par))
        
       case ("bc3")
          ch_nut(ielem)%bc3 = chg_par(ch_nut(ielem)%bc3,
     &                    chg_prm(ichg_par))
        
        case ("bc4")
          ch_nut(ielem)%bc4 = chg_par(ch_nut(ielem)%bc4,
     &                    chg_prm(ichg_par))
        
        case ("sedpst_conc")
          ch_pst(ielem)%sedpst_conc = chg_par(ch_pst(ielem)%sedpst_conc,
     &                    chg_prm(ichg_par)) 
        
        case ("pst_rea_ch")
          ch_pst(ielem)%pst_rea = chg_par(ch_pst(ielem)%pst_rea,
     &                    chg_prm(ichg_par))
        
        case ("pst_vol_ch")
          ch_pst(ielem)%pst_vol = chg_par(ch_pst(ielem)%pst_vol,
     &                    chg_prm(ichg_par)) 
        
        case ("pst_koc_ch")
          ch_pst(ielem)%pst_koc = chg_par(ch_pst(ielem)%pst_koc,
     &                    chg_prm(ichg_par)) 
        
        case ("pst_stl_ch")
          ch_pst(ielem)%pst_stl = chg_par(ch_pst(ielem)%pst_stl,
     &                    chg_prm(ichg_par)) 
        
        case ("pst_rsp_ch")
          ch_pst(ielem)%pst_rsp = chg_par(ch_pst(ielem)%pst_rsp,
     &                    chg_prm(ichg_par))
        
        case ("pst_mix_ch")
          ch_pst(ielem)%pst_mix = chg_par(ch_pst(ielem)%pst_mix,
     &                    chg_prm(ichg_par)) 
        
        case ("sedpst_rea")
          ch_pst(ielem)%sedpst_rea = chg_par(ch_pst(ielem)%sedpst_rea,
     &                    chg_prm(ichg_par)) 
        
        case ("sedpst_bry")
          ch_pst(ielem)%sedpst_bry = chg_par(ch_pst(ielem)%sedpst_bry,
     &                    chg_prm(ichg_par))
        
        case ("sedpst_act")
          ch_pst(ielem)%sedpst_act = chg_par(ch_pst(ielem)%sedpst_act,
     &                    chg_prm(ichg_par))
        
        case ("rch_dox")
          ch(ielem)%rch_dox = chg_par(ch(ielem)%rch_dox,
     &                    chg_prm(ichg_par))
        
        case ("rch_cbod")
          ch(ielem)%rch_cbod = chg_par(ch(ielem)%rch_cbod,
     &                    chg_prm(ichg_par))
        
        case ("algae")
          ch(ielem)%algae = chg_par(ch(ielem)%algae,
     &                    chg_prm(ichg_par))
        
        case ("organicn")
          ch(ielem)%organicn = chg_par(ch(ielem)%organicn,
     &                    chg_prm(ichg_par))
        
        case ("ammonian")
          ch(ielem)%ammonian = chg_par(ch(ielem)%ammonian,
     &                    chg_prm(ichg_par)) 
        
        case ("nitriten")
          ch(ielem)%nitriten = chg_par(ch(ielem)%nitriten,
     &                    chg_prm(ichg_par))
        
        case ("organicp")
          ch(ielem)%organicp = chg_par(ch(ielem)%organicp,
     &                    chg_prm(ichg_par)) 
        
        case ("disolvp")
          ch(ielem)%disolvp = chg_par(ch(ielem)%disolvp,
     &                    chg_prm(ichg_par))
        
!!      RTE
         case ("w")
            ch_hyd(ielem)%w = chg_par(ch_hyd(ielem)%w,
     &                    chg_prm(ichg_par))
       
         case ("d")
            ch_hyd(ielem)%d = chg_par(ch_hyd(ielem)%d,
     &                    chg_prm(ichg_par))
        
         case ("s")
            ch_hyd(ielem)%s = chg_par(ch_hyd(ielem)%s,
     &                    chg_prm(ichg_par))
        
         case ("l")
            ch_hyd(ielem)%l = chg_par(ch_hyd(ielem)%l,
     &                    chg_prm(ichg_par))
        
         case ("n")
            ch_hyd(ielem)%n = chg_par(ch_hyd(ielem)%n,
     &                    chg_prm(ichg_par))
        
         case ("k_ch")
            ch_hyd(ielem)%k = chg_par(ch_hyd(ielem)%k,
     &                    chg_prm(ichg_par))
        
         case ("cov1")
            ch_sed(ielem)%cov1 = chg_par(ch_sed(ielem)%cov1,
     &                    chg_prm(ichg_par))
        
         case ("cov2")
            ch_sed(ielem)%cov2 = chg_par(ch_sed(ielem)%cov2,
     &                    chg_prm(ichg_par))
        
         case ("wdr")
            ch_hyd(ielem)%wdr = chg_par(ch_hyd(ielem)%wdr,
     &                    chg_prm(ichg_par))
        
         case ("alpha_bnk")
            ch_hyd(ielem)%alpha_bnk = chg_par(ch_hyd(ielem)%alpha_bnk,
     &                    chg_prm(ichg_par))
        
         case ("onco")
            ch_nut(ielem)%onco = chg_par(ch_nut(ielem)%onco,
     &                    chg_prm(ichg_par))
        
         case ("opco")
            ch_nut(ielem)%opco = chg_par(ch_nut(ielem)%opco,
     &                    chg_prm(ichg_par))
        
         case ("side")
            ch_hyd(ielem)%side = chg_par(ch_hyd(ielem)%side,
     &                    chg_prm(ichg_par))
        
         case ("bnk_bd")
            ch_sed(ielem)%bnk_bd = chg_par(ch_sed(ielem)%bnk_bd,
     &                    chg_prm(ichg_par))
        
         case ("bed_bd")
            ch_sed(ielem)%bed_bd = chg_par(ch_sed(ielem)%bed_bd,
     &                    chg_prm(ichg_par))
        
         case ("bnk_kd")
            ch_sed(ielem)%bnk_kd = chg_par(ch_sed(ielem)%bnk_kd,
     &                    chg_prm(ichg_par))
        
         case ("bed_kd")
            ch_sed(ielem)%bed_kd = chg_par(ch_sed(ielem)%bed_kd,
     &                    chg_prm(ichg_par))
        
         case ("bnk_d50")
            ch_sed(ielem)%bnk_d50 = chg_par(ch_sed(ielem)%bnk_d50,
     &                    chg_prm(ichg_par))
        
         case ("bed_d50")
            ch_sed(ielem)%bed_d50 = chg_par(ch_sed(ielem)%bed_d50,
     &                    chg_prm(ichg_par))
        
         case ("tc_bnk")
            ch_sed(ielem)%tc_bnk = chg_par(ch_sed(ielem)%tc_bnk,
     &                    chg_prm(ichg_par))
        
         case ("tc_bed")
            ch_sed(ielem)%tc_bed = chg_par(ch_sed(ielem)%tc_bed,
     &                    chg_prm(ichg_par))
        
         case ("erod(1)")    !! January only    
            ch_sed(ielem)%erod(1) = chg_par(ch_sed(ielem)%erod(1),
     &                    chg_prm(ichg_par))       
 
      !!RES
         case ("esa")
           res_hyd(ielem)%esa = chg_par(res_hyd(ielem)%esa,
     &                    chg_prm(ichg_par))
        
         case ("evol")
           res_hyd(ielem)%evol = chg_par(res_hyd(ielem)%evol,
     &                    chg_prm(ichg_par)) 
        
         case ("psa")
           res_hyd(ielem)%psa = chg_par(res_hyd(ielem)%psa,
     &                    chg_prm(ichg_par))
        
         case ("pvol")
           res_hyd(ielem)%pvol = chg_par(res_hyd(ielem)%pvol,
     &                    chg_prm(ichg_par))
        
         case ("nsed")
           res_sed(ielem)%nsed = chg_par(res_sed(ielem)%nsed,
     &                    chg_prm(ichg_par))
        
         case ("k_res")
           res_hyd(ielem)%k = chg_par(res_hyd(ielem)%k,
     &                    chg_prm(ichg_par))

         case ("evrsv")
            res_hyd(ielem)%evrsv = chg_par(res_hyd(ielem)%evrsv,
     &                    chg_prm(ichg_par))
        
         case ("vol")
            res(ielem)%flo = chg_par(res(ielem)%flo,
     &                    chg_prm(ichg_par))
        
         case ("sed")
            res(ielem)%sed = chg_par(res(ielem)%sed,
     &                    chg_prm(ichg_par))

         case ("orgp")
            res(ielem)%sedp = chg_par(res(ielem)%sedp,
     &                    chg_prm(ichg_par))
        
         case ("orgn")
            res(ielem)%orgn = chg_par(res(ielem)%orgn,
     &                    chg_prm(ichg_par))
        
         case ("solp")
            res(ielem)%solp = chg_par(res(ielem)%solp,
     &                    chg_prm(ichg_par))
        
         case ("no3")
            res(ielem)%no3 = chg_par(res(ielem)%no3,
     &                    chg_prm(ichg_par))
        
         case ("nh3")
            res(ielem)%nh3 = chg_par(res(ielem)%nh3,
     &                    chg_prm(ichg_par))
        
         case ("no2")
            res(ielem)%no2 = chg_par(res(ielem)%no2,
     &                    chg_prm(ichg_par))
        
         case ("psetlr1")
            res_nut(ielem)%psetlr1 = chg_par(res_nut(ielem)%psetlr1,
     &                    chg_prm(ichg_par))
        
         case ("psetlr2")
            res_nut(ielem)%psetlr2 = chg_par(res_nut(ielem)%psetlr2,
     &                    chg_prm(ichg_par))
        
         case ("nsetlr1")
            res_nut(ielem)%nsetlr1 = chg_par(res_nut(ielem)%nsetlr1,
     &                    chg_prm(ichg_par))
        
         case ("nsetlr2")
            res_nut(ielem)%nsetlr2 = chg_par(res_nut(ielem)%nsetlr2,
     &                    chg_prm(ichg_par))
        
         case ("chlar")
            res_nut(ielem)%chlar = chg_par(res_nut(ielem)%chlar,
     &                    chg_prm(ichg_par))
        
         case ("seccir")
            res_nut(ielem)%seccir = chg_par(res_nut(ielem)%seccir,
     &                    chg_prm(ichg_par))
        
         case ("pst_conc")
            res_pst(ielem)%pst_conc = chg_par(res_pst(ielem)%pst_conc,
     &                    chg_prm(ichg_par))
                       
         case ("pst_rea_res")
            res_pst(ielem)%pst_rea = chg_par(res_pst(ielem)%pst_rea,
     &                    chg_prm(ichg_par))
                       
         case ("pst_vol_res")
            res_pst(ielem)%pst_vol = chg_par(res_pst(ielem)%pst_vol,
     &                    chg_prm(ichg_par))
        
         case ("pst_koc_res")
            res_pst(ielem)%pst_koc = chg_par(res_pst(ielem)%pst_koc,
     &                    chg_prm(ichg_par))
        
         case ("pst_stl_res")
            res_pst(ielem)%pst_stl = chg_par(res_pst(ielem)%pst_stl,
     &                    chg_prm(ichg_par))
        
         case ("pst_rsp_res")
            res_pst(ielem)%pst_rsp = chg_par(res_pst(ielem)%pst_rsp,
     &                    chg_prm(ichg_par))
        
         case ("pst_mix_res")
            res_pst(ielem)%pst_mix = chg_par(res_pst(ielem)%pst_mix,
     &                    chg_prm(ichg_par))
                                
         case ("spst_conc")
            res_pst(ielem)%spst_conc = chg_par(res_pst(ielem)%spst_conc,
     &                    chg_prm(ichg_par))
                
         case ("spst_bry")
            res_pst(ielem)%spst_bry = chg_par(res_pst(ielem)%spst_bry,
     &                    chg_prm(ichg_par))
        
         case ("spst_act")
            res_pst(ielem)%spst_act = chg_par(res_pst(ielem)%spst_act,
     &                    chg_prm(ichg_par))

      end select

      return
      end subroutine current_par_value