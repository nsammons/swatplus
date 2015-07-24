      subroutine read_mgtops(isched)
      
      use jrw_datalib_module
      use parm
      use time_module
      
      integer :: iyear, day, mon

      iyear = 1
      do iop = 1, sched(isched)%num_ops             !! operation loop
        read (107,*)   sched(isched)%mgt_ops(iop)%op,
     &                 sched(isched)%mgt_ops(iop)%mon,
     &                 sched(isched)%mgt_ops(iop)%day,
     &                 sched(isched)%mgt_ops(iop)%husc,
     &                 sched(isched)%mgt_ops(iop)%op1,
     &                 sched(isched)%mgt_ops(iop)%op2,
     &                 sched(isched)%mgt_ops(iop)%op3,
     &                 sched(isched)%mgt_ops(iop)%name

        ! if operation is zero, default to 1
        if (sched(isched)%mgt_ops(iop)%op1 <= 0)
     &                                sched(isched)%mgt_ops(iop)%op1 = 1
        
        if (sched(isched)%mgt_ops(iop)%op > 0) then
          day = sched(isched)%mgt_ops(iop)%day
          mon = sched(isched)%mgt_ops(iop)%mon
          sched(isched)%mgt_ops(iop)%jday = Jdt(ndays,day,mon)
          sched(isched)%mgt_ops(iop)%year = iyear
          if (sched(isched)%mgt_ops(iop)%op == 20) iyear = iyear + 1
        end if

      end do                                  !! operation loop

      return
      end