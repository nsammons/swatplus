      subroutine hyd_read_connect

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     reads in the routing information from the watershed configuration
!!     input file (.fig) and calculates the number of subbasins, reaches, 
!!     and reservoirs

      use hydrograph_module
      use input_file_module
      
      character (len=80) :: titldum, header
      character (len=16) :: namedum
      character (len=3) :: iob_out, iobtyp, ihtyp
      integer :: npsu, isp, cmdno, idone, hydno, cmd_prev, ob1, ob2
      integer :: iobj_tot
      integer :: eof, i, imax
      
      eof = 0
      imax = 0
      mexco_sp = 0

      allocate (rcv_sum(sp_ob%objs))
      allocate (dfn_sum(sp_ob%objs))
      rcv_sum = 0
      dfn_sum = 0
      
      !! set first object number of each type
      nspu = 1
      if (sp_ob%hru > 0) then         ! 1==hru
        sp_ob1%hru = nspu
        nspu = sp_ob%hru + nspu
      end if
      if (sp_ob%hru_lte > 0) then     ! 2==hru_lte
        sp_ob1%hru_lte = nspu
        nspu = sp_ob%hru_lte + nspu
      end if
      if (sp_ob%sub > 0) then         ! 3==subbasin
        sp_ob1%sub = nspu
        nspu = sp_ob%sub + nspu
      end if
      if (sp_ob%modflow > 0) then     ! 4==modparm
        sp_ob1%modflow = nspu
        nspu = sp_ob%modflow + nspu
      end if
      if (sp_ob%aqu > 0) then         ! 5==aquifer
        sp_ob1%aqu = nspu
        nspu = sp_ob%aqu + nspu
      end if
      if (sp_ob%chan > 0) then        ! 6==channel
        sp_ob1%chan = nspu
        nspu = sp_ob%chan + nspu
      end if
      if (sp_ob%res > 0) then         ! 7==reservoir
        sp_ob1%res = nspu
        nspu = sp_ob%res + nspu
      end if
      if (sp_ob%recday > 0) then      ! 8==recday
        sp_ob1%recday = nspu
        nspu = sp_ob%recday + nspu
      end if
      if (sp_ob%recmon > 0) then      ! 9==recmon
        sp_ob1%recmon = nspu
        nspu = sp_ob%recmon + nspu
      end if
      if (sp_ob%recyr > 0) then       ! 10==recyear
        sp_ob1%recyr = nspu
        nspu = sp_ob%recyr + nspu
      end if
      if (sp_ob%exco > 0) then        ! 11==exco
        sp_ob1%exco = nspu
        nspu = sp_ob%exco + nspu
      end if
      if (sp_ob%dr > 0) then          ! 12==dr
        sp_ob1%dr = nspu
        nspu = sp_ob%dr + nspu
      end if
      if (sp_ob%canal > 0) then       ! 13==canal
        sp_ob1%canal = nspu
        nspu = sp_ob%canal + nspu
      end if
      if (sp_ob%pump > 0) then        ! 14==pump
        sp_ob1%pump = nspu
        nspu = sp_ob%pump + nspu
      end if
      if (sp_ob%outlet > 0) then      ! 15==outlet
        sp_ob1%outlet = nspu
        nspu = sp_ob%outlet + nspu
      end if
      if (sp_ob%chandeg > 0) then     ! 16==swat-deg channel
        sp_ob1%chandeg = nspu
        nspu = sp_ob%chandeg + nspu
      end if
!      if (sp_ob%a2d > 0) then        ! 17==2D aquifer
!        sp_ob1%a2d = nspu
!        nspu = sp_ob%a2d + nspu
!      end if
      
      !! read hru spatial data
      inquire (file=in_con%hru_con, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_con%hru_con)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mhru_sp
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

      if (sp_ob%hru > 0) then
      ob1 = sp_ob1%hru
      ob2 = sp_ob1%hru + sp_ob%hru - 1
      do i = ob1, ob2
        ob(i)%typ = 1
        allocate (ob(i)%hd(5))
        read (107,*,iostat=eof) k, ob(i)%name,
     &    ob(i)%props, ob(i)%wst, ob(i)%src_tot
        if (eof < 0) exit
       ! if (ob(i)%src_tot > 0) then
          nspu = ob(i)%src_tot
          allocate (ob(i)%obj_out(nspu))
          allocate (ob(i)%obtyp_out(nspu))
          allocate (ob(i)%obtypno_out(nspu))
          allocate (ob(i)%htyp_out(nspu))
          allocate (ob(i)%ihtyp_out(nspu))
          allocate (ob(i)%frac_out(nspu))
          allocate (ob(i)%hout_m(nspu))
          allocate (ob(i)%hout_y(nspu))
          allocate (ob(i)%hout_a(nspu))
          backspace (107)
          read (107,*,iostat=eof) k, ob(i)%name, 
     &        ob(i)%props,ob(i)%wst,ob(i)%src_tot,(ob(i)%obtyp_out(isp),
     &        ob(i)%obtypno_out(isp), ob(i)%htyp_out(isp), 
     &        ob(i)%frac_out(isp), isp = 1, nspu)
          if (eof < 0) exit
       ! end if
      end do
      endif
      exit
      enddo
      endif
      
      close (107)
      
      if (sp_ob%hru_lte > 0) then
      !! read hru.lte spatial data
      inquire (file=in_con%hruez_con, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_con%hruez_con)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mhrulte_sp
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

      ob1 = sp_ob1%hru_lte
      ob2 = sp_ob1%hru_lte + sp_ob%hru_lte - 1
      do i = ob1, ob2
        ob(i)%typ = 2
        allocate (ob(i)%hd(1))
        read (107,*) k, ob(i)%name, ob(i)%props, ob(i)%wst,
     &                  ob(i)%src_tot
        ob(i)%num = k
        if (ob(i)%src_tot > 0) then
          nspu = ob(i)%src_tot
          allocate (ob(i)%obj_out(nspu))
          allocate (ob(i)%obtyp_out(nspu))
          allocate (ob(i)%obtypno_out(nspu))
          allocate (ob(i)%htyp_out(nspu))
          allocate (ob(i)%ihtyp_out(nspu))
          allocate (ob(i)%frac_out(nspu))
          allocate (ob(i)%hout_m(nspu))
          allocate (ob(i)%hout_y(nspu))
          allocate (ob(i)%hout_a(nspu))
          backspace (107)
          read (107,*,iostat=eof) k, ob(i)%name, ob(i)%props,
     &     ob(i)%wst, ob(i)%src_tot, (ob(i)%obtyp_out(isp),
     &     ob(i)%obtypno_out(isp),ob(i)%htyp_out(isp), 
     &     ob(i)%frac_out(isp), isp = 1, nspu)
          if (eof < 0) exit
        endif
      enddo
      exit
        enddo
      endif
      
      end if     
      close (107)

      if (sp_ob%sub > 0) then
       
      !!read subbasin connect data - where to route output
      inquire (file=in_con%sub_con, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_con%sub_con)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) msub_sp
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

      ob1 = sp_ob1%sub
      ob2 = sp_ob1%sub + sp_ob%sub - 1
      do i = ob1, ob2
        ob(i)%typ = 3
        allocate (ob(i)%hd(5))
        read (107,*,iostat=eof) k, ob(i)%name, ob(i)%props, 
     &        ob(i)%src_tot
        if (eof < 0) exit
        if (ob(i)%src_tot > 0) then
          nspu = ob(i)%src_tot
          allocate (ob(i)%obj_out(nspu))
          allocate (ob(i)%obtyp_out(nspu))
          allocate (ob(i)%obtypno_out(nspu))
          allocate (ob(i)%htyp_out(nspu))
          allocate (ob(i)%ihtyp_out(nspu))
          allocate (ob(i)%frac_out(nspu))
          allocate (ob(i)%hout_m(nspu))
          allocate (ob(i)%hout_y(nspu))
          allocate (ob(i)%hout_a(nspu))
          backspace (107)
          read (107,*,iostat=eof) k, ob(i)%name, ob(i)%props, 
     &         ob(i)%src_tot,
     &         (ob(i)%obtyp_out(isp), ob(i)%obtypno_out(isp),
     &         ob(i)%htyp_out(isp), ob(i)%frac_out(isp), isp = 1, nspu)
          if (eof < 0) exit
        end if
      end do
        exit
      close (107)
      enddo
      endif
         
      !!read data for each element in all subbasins
      inquire (file=in_sub%ele_sub, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_sub%ele_sub)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) msub_elems
        allocate (sub_elem(msub_elems))
        allocate (ielem_sub(msub_elems))
        ielem_sub = 0
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        
        do isp = 1, msub_elems
          read (107,*,iostat=eof) k, sub_elem(isp)%name, 
     &      sub_elem(isp)%obtyp, sub_elem(isp)%obtypno, 
     &      sub_elem(isp)%htyp, sub_elem(isp)%frac, sub_elem(isp)%idr
          if (eof < 0) exit
        end do
        exit
      end do
      close (107)
              
      !read all delivery ratio data for subbasin deliveries
      inquire (file=in_sub%sub_del, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_sub%sub_del)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) msdr
        allocate (sub_dr(msdr))
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        do i = 1, msdr
          read (107,*,iostat=eof) sub_dr(i)
          if (eof < 0) exit
        end do
        exit
      enddo
      endif
      end if
      
      !!read subbasin definition data -ie. hru's in the subbasin
      inquire (file=in_sub%def_sub, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_sub%def_sub)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) msub_sp
        allocate (sub_d(msub_sp))
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

      ob1 = sp_ob1%sub
      ob2 = sp_ob1%sub + sp_ob%sub - 1
      do i = ob1, ob2
        isub = ob(i)%props
        ob(i)%typ = 3
        ob(i)%subs_tot = 0
        read (107,*,iostat=eof) numb, namedum, nspu
        allocate (elem_cnt(nspu))
        
        if (eof < 0) exit
        if (nspu > 0) then
          backspace (107)
          read (107,*,iostat=eof) numb,namedum, nspu,
     &                                    (elem_cnt(isp), isp = 1, nspu)
          if (eof < 0) exit
          
          !!save the object number of each defining unit
          ielem = 0
          do ii = 1, nspu
            ie1 = elem_cnt(ii)
            if (ii == nspu) then
              ielem = ielem + 1
            else
              if (elem_cnt(ii+1) < 0) then
                ie2 = abs(elem_cnt(ii+1))
                do ie = ie1, ie2
                  ielem = ielem + 1
                end do
                if (ii+1 == nspu) exit
              else
                ielem = ielem + 1
              end if
            end if
            if (ii == nspu .and. elem_cnt(ii) < 0) exit
          end do
          allocate (sub_d(isub)%num(ielem))
          sub_d(isub)%num_tot = ielem
          ob(i)%dfn_tot = sub_d(isub)%num_tot
          
          ielem = 0
          do ii = 1, nspu
            ie1 = elem_cnt(ii)
            ie2 = ie2
            if (ii == nspu) then
              ielem = ielem + 1
              sub_d(isub)%num(ielem) = ie1
            else
              if (elem_cnt(ii+1) < 0) then
                ie2 = abs(elem_cnt(ii+1))
                do ie = ie1, ie2
                  ielem = ielem + 1
                  sub_d(isub)%num(ielem) = ie
                end do
                if (ii+1 == nspu) exit
              else
                ielem = ielem + 1
                sub_d(isub)%num(ielem) = ie1
              end if
            end if
            if (ii == nspu .and. elem_cnt(ii) < 0) exit
          end do
          if (ie2 < ie1) ie2 = ie1
          
          do ii = ie1, ie2
            iobtyp = sub_elem(ii)%obtyp       !object type in sub
            select case (iobtyp)
            case ("hru")   !hru
              sub_elem(ii)%obj = sp_ob1%hru + sub_elem(ii)%obtypno - 1
            case ("hlt")   !hru_lte
              sub_elem(ii)%obj = sp_ob1%hru_lte + sub_elem(ii)%obtypno-1
            case ("cha")   !channel
              sub_elem(ii)%obj = sp_ob1%chan + sub_elem(ii)%obtypno -1
            case ("exc")   !export coefficient
              sub_elem(ii)%obj = sp_ob1%exco + sub_elem(ii)%obtypno - 1
            case ("del")   !delivery ratio
              sub_elem(ii)%obj = sp_ob1%dr + sub_elem(ii)%obtypno - 1
            case ("out")   !outlet
              sub_elem(ii)%obj = sp_ob1%outlet + sub_elem(ii)%obtypno -1
            case ("sdc")   !swat-deg channel
              sub_elem(ii)%obj = sp_ob1%chandeg + sub_elem(ii)%obtypno-1
            end select
            k = sub_elem(ii)%obj
            ob(k)%subs_tot = ob(k)%subs_tot + 1
          end do
          
          do ii = ie1, ie2
          select case (sub_elem(ii)%htyp)
            case ("tot")   !total flow
               sub_elem(ii)%htypno = 1
            case ("rhg")   !recharge
               sub_elem(ii)%htypno = 2              
            case ("sur")   !surface
               sub_elem(ii)%htypno = 3 
            case ("lat")   !lateral
               sub_elem(ii)%htypno = 4
            case ("til")   !tile
               sub_elem(ii)%htypno = 5  
            end select
          end do
          
        end if
        deallocate (elem_cnt)
      end do
        exit
      enddo
      endif
      
        ! set all subbasins that each element is in
        do ielem = 1, msub_elems
          iob = sub_elem(ielem)%obj
          isub_tot = ob(iob)%subs_tot
          allocate (sub_elem(ielem)%sub(isub_tot))
        end do
        do isub = 1, msub_sp
          do ielem = 1, sub_d(isub)%num_tot
            ie = sub_d(isub)%num(ielem)
            ielem_sub(ie) = ielem_sub(ie) + 1
            sub_elem(ie)%sub(ielem_sub(ie)) = isub
            iob = sub_elem(ie)%obj
            ob(iob)%elem = ie
          end do
        end do
      
      endif
      close (107)
            
      if (sp_ob%modflow > 0) then
      !!read aquifer definition data - ie. objects recharging the aquifer
      inquire (file=in_con%modflow_con, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_con%modflow_con)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mf_sp
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

      ob1 = sp_ob1%modflow
      ob2 = sp_ob1%modflow + sp_ob%modflow - 1
      do i = ob1, ob2
        ob(i)%typ = 4
        
        read (107,*,iostat=eof) k, ob(i)%name, ob(i)%ha, 
     &        ob(i)%props, ob(i)%props2, ob(i)%src_tot
        if (eof < 0) exit
        if (ob(i)%src_tot > 0) then
          nspu = ob(i)%src_tot
          allocate (ob(i)%hd(nspu))  ! 1-D lag model - forcing to 2 aquifers for CEAP II
                                     ! need to adjust for 2-D model
          allocate (ob(i)%obj_out(nspu))
          allocate (ob(i)%obtyp_out(nspu))
          allocate (ob(i)%obtypno_out(nspu))
          allocate (ob(i)%htyp_out(nspu))
          allocate (ob(i)%ihtyp_out(nspu))
          allocate (ob(i)%frac_out(nspu))
          allocate (ob(i)%hout_m(nspu))
          allocate (ob(i)%hout_y(nspu))
          allocate (ob(i)%hout_a(nspu))
          backspace (107)
          read (107,*) k, ob(i)%name, ob(i)%ha, ob(i)%props, 
     &       ob(i)%props2,ob(i)%wst,ob(i)%src_tot,(ob(i)%obtyp_out(isp),
     &         ob(i)%obtypno_out(isp),ob(i)%htyp_out(isp), 
     &         ob(i)%frac_out(isp), isp = 1, nspu)
        end if
      end do
      exit
      enddo
      endif
      close (107)
      end if
                       
      if (sp_ob%aqu > 0) then
      !!read aquifer definition data - ie. objects recharging the aquifer
      inquire (file=in_con%aqu_con, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_con%aqu_con)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) maqu_sp
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

      ob1 = sp_ob1%aqu
      ob2 = sp_ob1%aqu + sp_ob%aqu - 1
      do i = ob1, ob2
        ob(i)%typ = 5
        
        read (107,*,iostat=eof) k, ob(i)%name, ob(i)%ha, 
     &        ob(i)%props, ob(i)%props2, ob(i)%src_tot
        if (eof < 0) exit
        if (ob(i)%src_tot > 0) then
          nspu = ob(i)%src_tot
          allocate (ob(i)%hd(nspu))  ! 1-D lag model - forcing to 2 aquifers for CEAP II
                                     ! need to adjust for 2-D model
          allocate (ob(i)%obj_out(nspu))
          allocate (ob(i)%obtyp_out(nspu))
          allocate (ob(i)%obtypno_out(nspu))
          allocate (ob(i)%htyp_out(nspu))
          allocate (ob(i)%ihtyp_out(nspu))
          allocate (ob(i)%frac_out(nspu))
          allocate (ob(i)%hout_m(nspu))
          allocate (ob(i)%hout_y(nspu))
          allocate (ob(i)%hout_a(nspu))
          backspace (107)
          read (107,*) k, ob(i)%name, ob(i)%ha, ob(i)%props, 
     &         ob(i)%props2, ob(i)%src_tot, (ob(i)%obtyp_out(isp), 
     &         ob(i)%obtypno_out(isp),ob(i)%htyp_out(isp), 
     &         ob(i)%frac_out(isp), isp = 1, nspu)
        end if
      end do
      exit
      enddo
      endif
      close (107)
      end if
           
      !!read data for aquifer elements for 2-D groundwater model
      inquire (file=in_link%chan_aqu, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_link%chan_aqu)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mcha_sp
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        imax = 0
        do while (eof >= 0)
          read (107,*,iostat=eof) i
          if (eof < 0) exit
          imax = amax1(imax,i)
        end do
          
        allocate (ch_aqu(0:imax))
        rewind (107)
        read (107,*) titldum
        read (107,*,iostat=eof) mcha_sp
        read (107,*) header

        do ise = 1, mcha_sp
          read (107,*,iostat=eof) i, namedum, nspu
          allocate (ch_aqu(i)%aqu_no(nspu))

          if (eof < 0) exit
          if (nspu > 0) then
            backspace (107)
            read (107,*,iostat=eof) numb, ch_aqu(i)%name, ch_aqu(i)%num,
     &                            (ch_aqu(i)%aqu_no(isp), isp = 1, nspu)
            if (eof < 0) exit
          end if
        end do
      end do
      close (107)
      end if
          
      if (sp_ob%chan > 0) then
      !! read channel spatial data
      inquire (file=in_con%chan_con, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_con%chan_con)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mchan_sp
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

      ob1 = sp_ob1%chan
      ob2 = sp_ob1%chan + sp_ob%chan - 1
      do i = ob1, ob2
        ob(i)%typ = 6
        allocate (ob(i)%hd(3))
        read (107,*,iostat=eof) k, ob(i)%name, ob(i)%ha, 
     &     ob(i)%props, ob(i)%props2, ob(i)%wst, ob(i)%src_tot
        if (eof < 0) exit
        if (ob(i)%src_tot > 0) then
          nspu = ob(i)%src_tot
          allocate (ob(i)%obj_out(nspu))
          allocate (ob(i)%obtyp_out(nspu))
          allocate (ob(i)%obtypno_out(nspu))
          allocate (ob(i)%htyp_out(nspu))
          allocate (ob(i)%ihtyp_out(nspu))
          allocate (ob(i)%frac_out(nspu))
          allocate (ob(i)%hout_m(nspu))
          allocate (ob(i)%hout_y(nspu))
          allocate (ob(i)%hout_a(nspu))
          backspace (107)
          read (107,*,iostat=eof) k, ob(i)%name, ob(i)%ha, 
     &       ob(i)%props, ob(i)%props2, ob(i)%wst, ob(i)%src_tot, 
     &       (ob(i)%obtyp_out(isp), ob(i)%obtypno_out(isp), 
     &       ob(i)%htyp_out(isp), ob(i)%frac_out(isp), isp = 1, nspu)
          if (eof < 0) exit
        end if
      end do
      exit
      enddo
      endif
      close (107)
      end if
                         
      !!read data for surface elements in the floodplain-for overbank flooding
      inquire (file=in_link%chan_surf, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_link%chan_surf)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mcha_sp
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        imax = 0
        do while (eof >= 0)
          read (107,*,iostat=eof) i
          if (eof < 0) exit
          imax = amax1(imax,i)
        end do
          
        allocate (ch_sur(0:imax))
        rewind (107)
        read (107,*) titldum
        read (107,*,iostat=eof) mcha_sp
        read (107,*) header

        do ise = 1, mcha_sp
          read (107,*,iostat=eof) i, namedum, nspu
          allocate (ch_sur(i)%obtyp(0:nspu))
          allocate (ch_sur(i)%obtypno(0:nspu))
          allocate (ch_sur(i)%wid(0:nspu))
          allocate (ch_sur(i)%dep(0:nspu))
          allocate (ch_sur(i)%flood_volmx(0:nspu))
          allocate (ch_sur(i)%hd(0:nspu))
        
          if (eof < 0) exit
          if (nspu > 0) then
            backspace (107)
            read (107,*,iostat=eof) numb, ch_sur(i)%name, ch_sur(i)%num,
     &     (ch_sur(i)%obtyp(isp), ch_sur(i)%obtypno(isp), isp = 1, nspu)
            if (eof < 0) exit
          end if

        end do
        exit
      end do
      close (107)
      end if
                    
      if (sp_ob%res > 0) then
      !! read channel spatial data
      inquire (file=in_con%res_con, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_con%res_con)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mres_sp
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

      ob1 = sp_ob1%res
      ob2 = sp_ob1%res + sp_ob%res - 1
      do i = ob1, ob2
        ob(i)%typ = 7
        allocate (ob(i)%hd(2))
        read (107,*,iostat=eof) k, ob(i)%name, ob(i)%ha, 
     &     ob(i)%props, ob(i)%props2, ob(i)%wst, ob(i)%src_tot
        if (eof < 0) exit
        if (ob(i)%src_tot > 0) then
          nspu = ob(i)%src_tot
          allocate (ob(i)%obj_out(nspu))
          allocate (ob(i)%obtyp_out(nspu))
          allocate (ob(i)%obtypno_out(nspu))
          allocate (ob(i)%htyp_out(nspu))
          allocate (ob(i)%ihtyp_out(nspu))
          allocate (ob(i)%frac_out(nspu))
          allocate (ob(i)%hout_m(nspu))
          allocate (ob(i)%hout_y(nspu))
          allocate (ob(i)%hout_a(nspu))
          backspace (107)
          read (107,*,iostat=eof) k, ob(i)%name, ob(i)%ha, 
     &       ob(i)%props, ob(i)%props2, ob(i)%wst, ob(i)%src_tot, 
     &       (ob(i)%obtyp_out(isp), ob(i)%obtypno_out(isp), 
     &       ob(i)%htyp_out(isp), ob(i)%frac_out(isp), isp = 1, nspu)
          if (eof < 0) exit
        end if
      end do
      exit
      enddo
      endif
      close (107)
      end if
                     
      if (sp_ob%recday > 0) then
      !! read export coefficient spatial data
      inquire (file=in_con%rec_con, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_con%rec_con)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mrec_sp
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

      ob1 = sp_ob1%recday
      ob2 = sp_ob1%recday + sp_ob%recday - 1
      do i = ob1, ob2
        ob(i)%typ = 8
        allocate (ob(i)%hd(1))
        read (107,*,iostat=eof) k, ob(i)%name, ob(i)%ha, ob(i)%props, 
     &          ob(i)%props2, ob(i)%src_tot
        if (eof < 0) exit
        if (ob(i)%src_tot > 0) then
          nspu = ob(i)%src_tot
          allocate (ob(i)%obj_out(nspu))
          allocate (ob(i)%obtyp_out(nspu))
          allocate (ob(i)%obtypno_out(nspu))
          allocate (ob(i)%htyp_out(nspu))
          allocate (ob(i)%ihtyp_out(nspu))
          allocate (ob(i)%frac_out(nspu))
          allocate (ob(i)%hout_m(nspu))
          allocate (ob(i)%hout_y(nspu))
          allocate (ob(i)%hout_a(nspu))
          backspace (107)
          read (107,*,iostat=eof) k, ob(i)%name, ob(i)%ha, ob(i)%props, 
     &         ob(i)%props2, ob(i)%src_tot,
     &         (ob(i)%obtyp_out(isp), ob(i)%obtypno_out(isp), 
     &         ob(i)%htyp_out(isp), ob(i)%frac_out(isp), isp = 1, nspu)
          if (eof < 0) exit
        end if
      end do
      exit
      enddo
      endif
      close (107)     
      
      !read all recall files
      inquire (file="recall.rec", exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file="recall.rec")
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        imax = 0
          do while (eof >= 0)
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            mrec_sp = mrec_sp + 1 
          end do
          
      allocate (recall(0:imax))
      rewind (107)
      read (107,*) titldum
      read (107,*) header
      
      do ii = 1, mrec_sp
        read (107,*,iostat=eof) i
        backspace (107)
        read (107,*,iostat = eof) k, recall(i)%name, recall(i)%typ,
     &          recall(i)%filename
        if (eof < 0) exit
        open (108,file = recall(i)%filename)
        read (108,*,iostat=eof) titldum
        if (eof < 0) exit
        read (108,*,iostat=eof) nbyr
        if (eof < 0) exit
        read (108,*,iostat=eof) header
        if (eof < 0) exit
        
          select case (recall(i)%typ)
           case (1) !! daily
            allocate (recall(i)%hd(366,nbyr))
            
           case (2) !! monthly
            allocate (recall(i)%hd(12,nbyr))
            
           case (3) !! annual
            allocate (recall(i)%hd(1,nbyr))
           end select

        ! read and store entire year
       do 
         read (108,*,iostat=eof) iyr, istep
         if (eof < 0) exit
         if (iyr == time%yrc) exit
       end do
       
       backspace (108)
       iyr_prev = iyr
       iyrs = 1
       
       do 
         if (recall(i)%typ == 3) then
           read (108,*,iostat=eof) iyr, recall(i)%hd(1,iyrs)
           !convert concentrations to mass
           call hyd_convert (recall(i)%hd(1,iyrs))
           if (eof < 0) exit
           read (108,*,iostat=eof) iyr
         else   
           read (108,*,iostat=eof) iyr, istep, recall(i)%hd(istep,iyrs)
           !convert concentrations to mass
           call hyd_convert (recall(i)%hd(istep,iyrs))
           if (eof < 0) exit
           read (108,*,iostat=eof) iyr, istep
         end if
         if (eof < 0) exit
         backspace (108)
         if (iyr /= iyr_prev) then
           iyr_prev = iyr
           iyrs = iyrs + 1
         endif
       end do
       close (108)
       
      end do
      close (107)
      exit
      enddo
      endif
      endif
        
           
      if (sp_ob%exco > 0) then
      !! read export coefficient spatial data
      inquire (file=in_con%exco_con, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_con%exco_con)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mexco_sp
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

      ob1 = sp_ob1%exco
      ob2 = sp_ob1%exco + sp_ob%exco - 1
      do i = ob1, ob2
        ob(i)%typ = 11
        allocate (ob(i)%hd(2))
        read (107,*,iostat=eof) k, ob(i)%name, ob(i)%ha, ob(i)%props, 
     &          ob(i)%props2, ob(i)%src_tot
        if (eof < 0) exit
        if (ob(i)%src_tot > 0) then
          nspu = ob(i)%src_tot
          allocate (ob(i)%obj_out(nspu))
          allocate (ob(i)%obtyp_out(nspu))
          allocate (ob(i)%obtypno_out(nspu))
          allocate (ob(i)%htyp_out(nspu))
          allocate (ob(i)%ihtyp_out(nspu))
          allocate (ob(i)%frac_out(nspu))
          allocate (ob(i)%hout_m(nspu))
          allocate (ob(i)%hout_y(nspu))
          allocate (ob(i)%hout_a(nspu))
          backspace (107)
          read (107,*,iostat=eof) k, ob(i)%name, ob(i)%ha, ob(i)%props, 
     &         ob(i)%props2, ob(i)%src_tot,
     &         (ob(i)%obtyp_out(isp), ob(i)%obtypno_out(isp), 
     &         ob(i)%htyp_out(isp), ob(i)%frac_out(isp), isp = 1, nspu)
          if (eof < 0) exit
        end if
      end do
      exit
      enddo
      endif
      close (107)     
      
      !read all export coefficient data here - don't need a module
      inquire (file=in_exco%exco, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_exco%exco)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        imax = 0
          do while (eof >= 0)
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            mexco_sp = mexco_sp + 1 
          end do
          
      allocate (exco(0:imax))
      rewind (107)
      read (107,*) titldum
      read (107,*) header
      
      do ii = 1, mexco_sp
        read (107,*,iostat=eof) i
        backspace (107)
        read (107,*,iostat=eof) k, exco(i)
        if (eof < 0) exit
      end do
      close (107)
      exit
      enddo
      endif
      endif
      
      if (sp_ob%dr > 0) then
      !! read delivery ratio spatial data
      inquire (file=in_con%delr_con, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_con%delr_con)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mdr_sp
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

      ob1 = sp_ob1%dr
      ob2 = sp_ob1%dr + sp_ob%dr - 1
      do i = ob1, ob2
        ob(i)%typ = 12
        allocate (ob(i)%hd(2))
        read (107,*,iostat=eof) k, ob(i)%name, ob(i)%ha, ob(i)%props, 
     &         ob(i)%src_tot
        if (eof < 0) exit
        if (ob(i)%src_tot > 0) then
          nspu = ob(i)%src_tot
          allocate (ob(i)%obj_out(nspu))
          allocate (ob(i)%obtyp_out(nspu))
          allocate (ob(i)%obtypno_out(nspu))
          allocate (ob(i)%htyp_out(nspu))
          allocate (ob(i)%ihtyp_out(nspu))
          allocate (ob(i)%frac_out(nspu))
          allocate (ob(i)%hout_m(nspu))
          allocate (ob(i)%hout_y(nspu))
          allocate (ob(i)%hout_a(nspu))
          backspace (107)
          read (107,*,iostat=eof) k, ob(i)%name, ob(i)%ha, ob(i)%props, 
     &         ob(i)%src_tot,(ob(i)%obtyp_out(isp), 
     &         ob(i)%obtypno_out(isp), ob(i)%htyp_out(isp), 
     &         ob(i)%frac_out(isp), isp = 1, nspu)
          if (eof < 0) exit
        end if
      end do
      close (107)
      exit
      enddo
      endif
      
      !read all delivery ratio data here - don't need a module
      inquire (file=in_delr%del_ratio, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_delr%del_ratio)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mdr_sp
        allocate (dr(mdr_sp))
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        do i = 1, mdr_sp
          read (107,*,iostat=eof) dr(i)
          if (eof < 0) exit
        end do
        exit
      enddo
      endif
      end if
      
        if (sp_ob%outlet > 0) then
        inquire (file=in_con%out_con, exist=i_exist)
        if (i_exist /= 0) then
        do 
        !! read outlet spatial data
          open (107,file=in_con%out_con)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) mout_sp
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit

        ob1 = sp_ob1%outlet
        ob2 = sp_ob1%outlet + sp_ob%outlet - 1
        do i = ob1, ob2
          ob(i)%typ = 15
          read (107,*,iostat=eof) k, ob(i)%name, ob(i)%ha, 
     &          ob(i)%props
          !! set outflow object type to 0 - needed in final subroutine loop 
          allocate (ob(i)%obtypno_out(1))
          ob(i)%obtypno_out(1) = 0
          if (eof < 0) exit
        end do
        exit
        enddo
        endif
        end if
            
      if (sp_ob%chandeg > 0) then
      !! read chan-deg spatial data
      inquire (file=in_con%chandeg_con, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_con%chandeg_con)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mchandeg_sp
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

      ob1 = sp_ob1%chandeg
      ob2 = sp_ob1%chandeg + sp_ob%chandeg - 1
      do i = ob1, ob2
        ob(i)%typ = 16
        allocate (ob(i)%hd(3))
        read (107,*,iostat=eof) k, ob(i)%name, ob(i)%ha, 
     &     ob(i)%props, ob(i)%props2, ob(i)%wst, ob(i)%src_tot
        if (eof < 0) exit
        if (ob(i)%src_tot > 0) then
          nspu = ob(i)%src_tot
          allocate (ob(i)%obj_out(nspu))
          allocate (ob(i)%obtyp_out(nspu))
          allocate (ob(i)%obtypno_out(nspu))
          allocate (ob(i)%htyp_out(nspu))
          allocate (ob(i)%ihtyp_out(nspu))
          allocate (ob(i)%frac_out(nspu))
          allocate (ob(i)%hout_m(nspu))
          allocate (ob(i)%hout_y(nspu))
          allocate (ob(i)%hout_a(nspu))
          backspace (107)
          read (107,*,iostat=eof) k, ob(i)%name, ob(i)%ha, 
     &       ob(i)%props, ob(i)%props2, ob(i)%wst, ob(i)%src_tot, 
     &       (ob(i)%obtyp_out(isp), ob(i)%obtypno_out(isp), 
     &       ob(i)%htyp_out(isp), ob(i)%frac_out(isp), isp = 1, nspu)
          if (eof < 0) exit
        end if

      end do
      exit
      enddo
      endif
      close (107)
      end if
           
      !! for each hru or defining unit, set all subbasins that contain it 
        do i = 1, sp_ob%objs
          nspu = ob(i)%subs_tot
          if (nspu > 0) then
            allocate (ob(i)%obj_subs(nspu))
          end if
        end do
      
        dfn_sum = 0
        do i = 1, sp_ob%objs
          !! determine subbasin the hrus are in and add subbasin area to basin area
          if (ob(i)%typ == 3) then          !only need for subs-not aquifers
            isub = ob(i)%props
            do ii = 1, sub_d(isub)%num_tot
              !!only have hrus set up - need to add other objects
              ielem = sub_d(isub)%num(ii)          !points to element in sub_element
              k = sub_elem(ielem)%obj              !object type number of defining unit (ie hru)
              dfn_sum(ielem) = dfn_sum(ielem) + 1  !sequential number for subbasin object
              kk = dfn_sum(ielem)
              ob(k)%obj_subs(kk) = i
            end do
          end if
        end do
      
      !! determine number of recieving units and set object numbers for outflow hyds
        do i = 1, sp_ob%objs
          iph = 0
          iphl = 0
          ipsub = 0
          ipmf = 0
          ipaqu = 0
          ipcha = 0
          ipres = 0
          ipec = 0
          ipdr = 0
          ipo = 0
          ipsdc = 0
          do ii = 1, ob(i)%src_tot
            iob_out = ob(i)%obtyp_out(ii)          !object type out
            select case (iob_out)
            case ("hru")   !hru
              ob(i)%obj_out(ii) = sp_ob1%hru + ob(i)%obtypno_out(ii) - 1
              j = ob(i)%obj_out(ii)
              ob(j)%rcv_tot = ob(j)%rcv_tot + 1
              if (j /= iph) ob(j)%rcvob_tot = ob(j)%rcvob_tot + 1
              iph = j
            case ("hlt")   !hru_lte
              ob(i)%obj_out(ii) = sp_ob1%hru_lte+ob(i)%obtypno_out(ii)-1
              j = ob(i)%obj_out(ii)
              ob(j)%rcv_tot = ob(j)%rcv_tot + 1
              if (j /= iphl) ob(j)%rcvob_tot = ob(j)%rcvob_tot + 1
              iphl = j
            case ("sub")   !subbasin
              ob(i)%obj_out(ii) = sp_ob1%sub + ob(i)%obtypno_out(ii) - 1
              isub = ob(i)%obtypno_out(ii)
              j = ob(i)%obj_out(ii)
              ob(j)%rcv_tot = ob(j)%rcv_tot + 1
              if (j /= ipsub) ob(j)%rcvob_tot = ob(j)%rcvob_tot + 1
              do kk = 1, sub_d(isub)%num_tot
                ielem = sub_d(isub)%num(kk)
                iob = sub_elem(ielem)%obj
                ob(iob)%rcv_tot = ob(iob)%rcv_tot + 1
                if (j /= ipsub) ob(iob)%rcvob_tot = ob(iob)%rcvob_tot +1
              end do
              ipsub = j
            case ("mfl")   !modflow
              ob(i)%obj_out(ii) = sp_ob1%modflow+ob(i)%obtypno_out(ii)-1
              j = ob(i)%obj_out(ii)
              ob(j)%rcv_tot = ob(j)%rcv_tot + 1
              if (j /= ipaqu) ob(j)%rcvob_tot = ob(j)%rcvob_tot + 1
              ipmf = j
            case ("aqu")   !aquifer
              ob(i)%obj_out(ii) = sp_ob1%aqu + ob(i)%obtypno_out(ii) - 1
              j = ob(i)%obj_out(ii)
              ob(j)%rcv_tot = ob(j)%rcv_tot + 1
              if (j /= ipaqu) ob(j)%rcvob_tot = ob(j)%rcvob_tot + 1
              ipaqu = j
            case ("cha")   !channel
              !! all channels receive flow from modflow
              if (ob(i)%typ == 4 .and. ob(i)%obtypno_out(ii) <= 0) then
                do ich = 1, sp_ob%chan
                  ob(i)%obj_out(ii) = sp_ob1%chan + ich - 1
                  j = ob(i)%obj_out(ii)
                  ob(j)%rcv_tot = ob(j)%rcv_tot + 1
                  ob(j)%rcvob_tot = ob(j)%rcvob_tot + 1
                end do
              else
                ob(i)%obj_out(ii) = sp_ob1%chan +ob(i)%obtypno_out(ii)-1
                j = ob(i)%obj_out(ii)
                ob(j)%rcv_tot = ob(j)%rcv_tot + 1
                if (j /= ipcha) ob(j)%rcvob_tot = ob(j)%rcvob_tot + 1
                ipcha = j
              end if
            case ("res")   !reservoir
              ob(i)%obj_out(ii) = sp_ob1%res + ob(i)%obtypno_out(ii) -1
              j = ob(i)%obj_out(ii)
              ob(j)%rcv_tot = ob(j)%rcv_tot + 1
              if (j /= ipres) ob(j)%rcvob_tot = ob(j)%rcvob_tot + 1
              ipres = j
            case ("exc")   !export coefficients
              ob(i)%obj_out(ii) = sp_ob1%exco + ob(i)%obtypno_out(ii) -1
              j = ob(i)%obj_out(ii)
              ob(j)%rcv_tot = ob(j)%rcv_tot + 1
              if (j /= ipec) ob(j)%rcvob_tot = ob(j)%rcvob_tot + 1
              ipec = j
            case ("del")   !delivery ratio
              ob(i)%obj_out(ii) = sp_ob1%dr + ob(i)%obtypno_out(ii) - 1
              j = ob(i)%obj_out(ii)
              ob(j)%rcv_tot = ob(j)%rcv_tot + 1
              if (j /= ipdr) ob(j)%rcvob_tot = ob(j)%rcvob_tot + 1
              ipdr = j
            case ("out")   !outlet
              ob(i)%obj_out(ii) = sp_ob1%outlet +ob(i)%obtypno_out(ii)-1
              j = ob(i)%obj_out(ii)
              ob(j)%rcv_tot = ob(j)%rcv_tot + 1
              if (j /= ipo) ob(j)%rcvob_tot = ob(j)%rcvob_tot + 1
              ipo = j
            case ("sdc")   !swat-deg channel
              ob(i)%obj_out(ii) = sp_ob1%chandeg+ob(i)%obtypno_out(ii)-1
              j = ob(i)%obj_out(ii)
              ob(j)%rcv_tot = ob(j)%rcv_tot + 1
              if (j /= ipsdc) ob(j)%rcvob_tot = ob(j)%rcvob_tot + 1
              ipsdc = j
            end select
            
            select case (ob(i)%htyp_out(ii))
            case ("tot")   !total flow
               ob(i)%ihtyp_out(ii) = 1
            case ("rhg")   !recharge
               ob(i)%ihtyp_out(ii) = 2              
            case ("sur")   !surface
               ob(i)%ihtyp_out(ii) = 3 
            case ("lat")   !lateral
               ob(i)%ihtyp_out(ii) = 4
            case ("til")   !tile
               ob(i)%ihtyp_out(ii) = 5  
            end select             
          end do
        end do

      !! allocate receiving arrays
      do i = 1, sp_ob%objs
        if (ob(i)%rcv_tot > 0) then
          nspu = ob(i)%rcv_tot
          allocate (ob(i)%obj_in(nspu))
          allocate (ob(i)%obtyp_in(nspu))
          allocate (ob(i)%obtypno_in(nspu))
          allocate (ob(i)%htyp_in(nspu))
          allocate (ob(i)%frac_in(nspu))
          !!inflow hyd: 1==surf 2==lateral
          allocate (ob(i)%hin_m(2))
          allocate (ob(i)%hin_y(2))
          allocate (ob(i)%hin_a(2))
        end if
      end do

      !! loop through again and set receiving dependencies 
      do i = 1, sp_ob%objs                                  ! i=object number of source object
        do ii = 1, ob(i)%src_tot                            ! ii=sequential source number
          !! if modflow object and set for all channels
          if (ob(i)%obtyp_out(ii) == "cha" .and. ob(i)%obtypno_out(ii)
     &                                                       <= 0) then
            !! set all incoming channel object data to modflow
            do ich = 1, sp_ob%chan
              kk = sp_ob1%chan + ich - 1
              rcv_sum(kk) = rcv_sum(kk) + 1                   ! setting sequential inflow number
              jj = rcv_sum(kk)                                ! jj=seqential receiving number
              ob(kk)%obj_in(jj) = i                           ! source object number (for receiving unit)
              ob(kk)%obtyp_in(jj) = ob(i)%typ
              ob(kk)%obtypno_in(jj) = ob(i)%props
              ob(kk)%htyp_in(jj) = ob(i)%ihtyp_out(ii)
              ob(kk)%frac_in(jj) = ob(i)%frac_out(ii)
            end do
          else
            kk = ob(i)%obj_out(ii)                          ! kk=object number of outflow object
            rcv_sum(kk) = rcv_sum(kk) + 1                   ! setting sequential inflow number
            jj = rcv_sum(kk)                                ! jj=seqential receiving number
            ob(kk)%obj_in(jj) = i                           ! source object number (for receiving unit)
            ob(kk)%obtyp_in(jj) = ob(i)%typ
            ob(kk)%obtypno_in(jj) = ob(i)%props
            ob(kk)%htyp_in(jj) = ob(i)%ihtyp_out(ii)
            ob(kk)%frac_in(jj) = ob(i)%frac_out(ii)
          end if
        end do
      end do
      
      !! loop through again to set command (object) sequence
      cmdno = 0
      idone = 0
      iobj_tot = 0
      rcv_sum = 0
      iord = 1
      dfn_sum = 0
      
      do while (idone == 0)
        do i = 1, sp_ob%objs
          !check if all incoming and defining objects have been met
          !if not sum incoming 
          if (rcv_sum(i) == ob(i)%rcvob_tot .and. 
     &                                 dfn_sum(i) == ob(i)%dfn_tot) then
            if (ob(i)%fired == 0) then
            ob(i)%fired = 1
            iobj_tot = iobj_tot + 1    ! check to see if all objects are done
            if (iobj_tot == sp_ob%objs) idone = 1
            !sum defining units for each subbasin
            do k = 1, ob(i)%subs_tot
              kk = ob(i)%obj_subs(k)       !ob number of subbasin
              dfn_sum(kk) = dfn_sum(kk) + 1
            end do
          
            isrc_tot = amax1 (ob(i)%src_tot, 1)  !force to go through once
            iprev = 0
            do ii = 1, isrc_tot
              !! add receiving object for single objects
              if (ob(i)%obtypno_out(ii) > 0) then
                k = ob(i)%obj_out(ii)
                if (iprev /= k) then
                  rcv_sum(k) = rcv_sum(k) + 1
                  !! add subbasin elements
                  if (ob(k)%typ == 3) then
                    isub = ob(k)%props
                    do jj = 1, sub_d(isub)%num_tot
                      ielem = sub_d(isub)%num(jj)
                      iob = sub_elem(ielem)%obj
                      rcv_sum(iob) = rcv_sum(iob) + 1
                    end do
                  end if
                end if
                iprev = k
              else
                !! modflow - for all inflow objects
                if (ob(i)%typ == 4) then
                  iobtyp = ob(i)%obtyp_out(ii)
                  !! add recieving for all channels from modflow
                  if (iobtyp == "cha") then
                    ob1 = sp_ob1%chan
                    ob2 = sp_ob1%chan + sp_ob%chan - 1
                    do ich = ob1, ob2
                      rcv_sum(ich) = rcv_sum(ich) + 1
                    end do
                  end if
                end if
              end if
              if (ob(i)%typ /= 11) then   !exco's are not commands
                ob(i)%cmd_prev = cmd_prev
                if (cmd_prev > 0) then
                  ob(cmd_prev)%cmd_next = i
                else
                  sp_ob1%objs = i  !first command number
                end if
                cmd_prev = i
                !rcv_sum(i) = rcv_sum(i) + 1
                ob(i)%cmd_order = iord
              end if  !exco's are not commands
            
            end do
            end if
          end if
        end do
        iord = iord + 1
      end do
      
      !allocate hydrograph arrays
      call hyd_allo (hydno)
      
      end subroutine hyd_read_connect