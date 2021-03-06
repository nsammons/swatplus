      module input_file_module

!! file.cio input file   
      type input_sim
!! simulation
        character(len=25) :: time = "time.sim"
        character(len=25) :: prt = "print.prt"
        character(len=25) :: object_prt = "object.prt"
        character(len=25) :: object_cnt = "object.cnt"
      end type input_sim
      type (input_sim) :: in_sim
	
      type input_cli
!! climate
       character(len=25) :: weat_sta = "weather-sta.cli"
       character(len=25) :: weat_wgn = "weather-wgn.cli"
       character(len=25) :: wind_dir = "wind-dir.cli"
       character(len=25) :: pcp_cli = "pcp.cli"
       character(len=25) :: tmp_cli = "tmp.cli"
       character(len=25) :: slr_cli = "slr.cli"
       character(len=25) :: hmd_cli = "hmd.cli"
       character(len=25) :: wnd_cli = "wnd.cli"
      end type input_cli
      type (input_cli) :: in_cli

      type input_con
!! connect
       character(len=25) :: hru_con = "hru.con"
       character(len=25) :: hruez_con = "hru-ez.con"
       character(len=25) :: sub_con = "subbasin.con"
       character(len=25) :: modflow_con = "modflow.con"
       character(len=25) :: aqu_con = "aquifer2d.con"
       character(len=25) :: aqu2d_con = "aquifer.con"
       character(len=25) :: chan_con = "channel.con"
       character(len=25) :: res_con = "reservoir.con"
       character(len=25) :: rec_con = "recall.con"
       character(len=25) :: exco_con = "exco.con"
       character(len=25) :: delr_con = "delratio.con"
       character(len=25) :: out_con = "outlet.con"
       character(len=25) :: chandeg_con = "chandeg.con"
      end type input_con
      type (input_con) :: in_con

      type input_cha 
!! channel  
       character(len=25) :: init = "initial.cha"
       character(len=25) :: dat =  "channel.cha"
       character(len=25) :: hyd =  "hydrology.cha"
       character(len=25) :: sed =  "sediment.cha"
       character(len=25) :: nut =  "nutrients.cha"
       character(len=25) :: pest = "pesticide.cha"
       character(len=25) :: chan_ez = "channel-ez.cha"
      end type input_cha
      type (input_cha) :: in_cha

      type input_res
!! reservoir
       character(len=25) :: init_res = "initial.res"
       character(len=25) :: res =      "reservoir.res"
       character(len=25) :: hyd_res =  "hydrology.res"
       character(len=25) :: nut_res =  "nutrients.res"
       character(len=25) :: pest_res = "pesticide.res"
       character(len=25) :: sed_res =  "sediment.res"
       character(len=25) :: weir_res = "weir.res"
      end type input_res
      type (input_res) :: in_res

      type input_sub
!! subbasin
       character(len=25) :: def_sub = "define.sub"
       character(len=25) :: ele_sub = "element.sub"
       character(len=25) :: sub = "subbasin.sub"
       character(len=25) :: sub_del = "subbasin.del"
      end type input_sub
      type (input_sub) :: in_sub

      type input_hru
!! HRU
       character(len=25) :: hru_data = "hru-data.hru"
       character(len=25) :: hru_ez   = "hru-ez.hru"
      end type input_hru
      type (input_hru) :: in_hru

      type input_delr
!! delivery ratio
       character(len=25) :: del_ratio = "delratio.del"
      end type input_delr
      type (input_delr) :: in_delr

      type input_aqu
!! aquifer 
       character(len=25) :: aqu = "aquifer.aqu"
      end type input_aqu
      type (input_aqu) :: in_aqu

      type input_link
!! link
       character(len=25) :: chan_surf = "chan-surf.lin"
       character(len=25) :: chan_aqu = "chan-aqu.lin"
      end type input_link
      type (input_link) :: in_link

      type input_basin
       character(len=25) :: codes_bas = "codes.bsn"
       character(len=25) :: parms_bas = "parameters.bsn"
      end type input_basin
      type (input_basin) :: in_basin

      type input_hydrology
       character(len=25) :: hydrol_hyd = "hydrology.hyd"
       character(len=25) :: topogr_hyd = "topography.top"
       character(len=25) :: field_fld  = "field.fld"
      end type input_hydrology
      type (input_hydrology) :: in_hyd
  
      type input_exco
       character(len=25) :: exco = "exco.exc"
      end type input_exco
      type (input_exco) :: in_exco

      type input_bacteria
       character(len=25) :: init_bac = "initial.bac"
       character(len=25) :: bacteria = "bacteria.bac"
      end type input_bacteria
      type (input_bacteria) :: in_bac

      type input_structural
       character(len=25) :: septic_str = "septic.str"
       character(len=25) :: bmpuser_str = "bmpuser.str"
       character(len=25) :: contour_str = "contour.str"
       character(len=25) :: fstrip_str = "filterstrip.str"
       character(len=25) :: fire_str = "fire.str"
       character(len=25) :: grassww_str = "grassedww.str"
       character(len=25) :: plparms_str = "plantparms.str"
       character(len=25) :: residue_str = "residue.str"
       character(len=25) :: stcrop_str = "stripcrop.str"
       character(len=25) :: terrace_str = "terrace.str"
       character(len=25) :: tiledrain_str = "tiledrain.str"
       character(len=25) :: initial_str = "initial.str"
      end type input_structural
      type (input_structural) :: in_str

      type input_parameter_databases
       character(len=25) :: plants_plt = "plants.plt"
       character(len=25) :: fert_frt = "fertilizer.frt"
       character(len=25) :: till_til = "tillage.til"
       character(len=25) :: pest_pst = "pesticide.pst"
       character(len=25) :: urban_urb = "urban.urb"
       character(len=25) :: septic_sep = "septic.sep"
       character(len=25) :: snow = "snow.sno"
       character(len=25) :: atmodb = "atmo.atm"
      end type input_parameter_databases
      type (input_parameter_databases) :: in_parmdb

      type input_ops
       character(len=25) :: autofert_ops = "autofert.ops"
       character(len=25) :: autoirr_ops = "autoirr.ops"
       character(len=25) :: contfert_ops = "contfert.ops"
       character(len=25) :: contpest_ops = "contpest.ops"
       character(len=25) :: fert_ops = "fert.ops"
       character(len=25) :: graze_ops = "graze.ops"
       character(len=25) :: harv_ops = "harv.ops"
       character(len=25) :: irr_ops = "irr.ops"
       character(len=25) :: pest_ops = "pest.ops"
       character(len=25) :: sweep_ops = "sweep.ops"
      end type input_ops
      type (input_ops) :: in_ops

      type input_sch
       character(len=25) :: management_sch = "management.sch"
      end type input_sch
      type (input_sch) :: in_sch

      type input_lum
       character(len=25) :: cntable_lum = "cntable.lum"
       character(len=25) :: landuse_lum = "landuse.lum"
      end type input_lum
      type (input_lum) :: in_lum

      type input_chg
       character(len=25) :: parms_chg = "parameters.chg"
       character(len=25) :: scen_chg = "scenarios.chg"
      end type input_chg
      type (input_chg) :: in_chg

      type input_init
       character(len=25) :: initial_pst = "initial.pst"
       character(len=25) :: initial_plt = "initial.plt"
       end type input_init
      type (input_init) :: in_init

      type input_soils
       character(len=25) :: soils_sol = "soils.sol"
       character(len=25) :: nut_sol = "nutrients.sol"
      end type input_soils
      type (input_soils) :: in_sol

      type input_condition
       character(len=25) :: cond_ctl = "condition.ctl"
      end type input_condition
      type (input_condition) :: in_cond
      
      contains

      include 'readcio_read.f'

      end module input_file_module 