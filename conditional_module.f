      module conditional_module
      
      use jrw_datalib_module
       
      type condition_if
        character(len=3) :: depend_var            ! dependent variable (ie volume, flow, sw, time, etc)
        character(len=3) :: obj_var               ! object variable (ie res, hru, canal, etc)
        integer :: obvar_no                       ! object number
        real :: c_lo                              ! lower limit constant
        real :: c_up                              ! upper limit constant
        character(len=4), dimension(2) :: v_bnd   ! lower limit variable
      end type condition_if
       
      type conditional_rule_set
        character (len=16) :: name
        integer :: num_cond
        character(len=4) :: typ
        real :: con
        type (condition_if), dimension(:), allocatable :: cond_if
      end type conditional_rule_set
      type (conditional_rule_set), dimension(:), allocatable :: ruleset
      
      type conditionals
        character (len=16) :: name
        character(len=4) :: ruleset_typ
        integer :: num_rulesets
        character(len=4) :: default_typ
        real :: default_con
        type (conditional_rule_set), dimension(:),allocatable :: ruleset
      end type conditionals
      type (conditionals), dimension(:), allocatable :: cond_db
      
      contains
      include 'condition_read.f'
      
      end module conditional_module   