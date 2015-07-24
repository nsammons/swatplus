      subroutine ch_read_nut
      
      use input_file_module
      use basin_module

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the lake water quality input file (.lwq).
!!    This file contains data related to initial pesticide and nutrient levels
!!    in the lake/reservoir and transformation processes occuring within the 
!!    lake/reservoir. Data in the lake water quality input file is assumed to
!!    apply to all reservoirs in the watershed.          

      integer :: eof, i, imax
      character (len=80) :: titldum
      character (len=80) :: header

      eof = 0
      imax = 0
      mch = 0

      inquire (file=in_cha%nut,exist=i_exist)
      if (i_exist == 0 .or. in_cha%nut == 'null') then
        write (4444,4445) in_cha%nut, mch, imax, '         null'
        allocate (ch_nut(0:0))
      else
      do
        open (105,file=in_cha%nut)
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (105,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            mch = mch + 1
          end do        
        
        allocate (ch_nut(0:imax))
        rewind (105)
        read (105,*) titldum
        read (105,*) header

        write (4444,4446) in_cha%nut, mch, imax
         
        do ich = 1, mch
          read (105,*,iostat=eof) i
          backspace (105)
          read (105,*) k, ch_nut(i)
          if (eof < 0) exit
          
!!    set default values for undefined parameters

      if (ch_nut(jnut)%lao <= 0) ch_nut(jnut)%lao = 2
      if (ch_nut(jnut)%igropt <= 0) ch_nut(jnut)%igropt = 2
      if (ch_nut(jnut)%ai0 <= 0.) ch_nut(jnut)%ai0 = 50.
      if (ch_nut(jnut)%ai1 <= 0.) ch_nut(jnut)%ai1 = 0.08
      if (ch_nut(jnut)%ai2 <= 0.) ch_nut(jnut)%ai2 = 0.015
      if (ch_nut(jnut)%ai3 <= 0.) ch_nut(jnut)%ai3 = 1.60
      if (ch_nut(jnut)%ai4 <= 0.) ch_nut(jnut)%ai4 = 2.0
      if (ch_nut(jnut)%ai5 <= 0.) ch_nut(jnut)%ai5 = 3.5
      if (ch_nut(jnut)%ai6 <= 0.) ch_nut(jnut)%ai6 = 1.07
      if (ch_nut(jnut)%mumax <= 0.) ch_nut(jnut)%mumax = 2.0
      if (ch_nut(jnut)%rhoq <= 0.) ch_nut(jnut)%rhoq = 2.5      !! previous 0.3
      if (ch_nut(jnut)%tfact <= 0.) ch_nut(jnut)%tfact = 0.3
      if (ch_nut(jnut)%k_l <= 0.) ch_nut(jnut)%k_l = 0.75
      if (ch_nut(jnut)%k_n <= 0.) ch_nut(jnut)%k_n = 0.02
      if (ch_nut(jnut)%k_p <= 0.) ch_nut(jnut)%k_p = 0.025
      if (ch_nut(jnut)%lambda0 <= 0.) ch_nut(jnut)%lambda0 = 1.0
      if (ch_nut(jnut)%lambda1 <= 0.) ch_nut(jnut)%lambda1 = 0.03
      if (ch_nut(jnut)%lambda2 <= 0.) ch_nut(jnut)%lambda2 = 0.054
      if (ch_nut(jnut)%p_n <= 0.) ch_nut(jnut)%p_n = 0.5
      if (chla_subco <= 0.) chla_subco = 40.0 
      
!! convert units on k_l:read in as kJ/(m2*min), use as MJ/(m2*hr)
      ch_nut(jnut)%k_l = ch_nut(jnut)%k_l * 1.e-3 * 60.

!! change units from day to hour if hourly (subdaily) routing is performed
      if (bsn_cc%event == 3) then
        ch_nut(jnut)%mumax = ch_nut(jnut)%mumax / 24.
        ch_nut(jnut)%rhoq = ch_nut(jnut)%rhoq / 24.
      end if
     
!!    set default values for undefined parameters
      if (ch_nut(irch)%rs1 <= 0.) ch_nut(irch)%rs1 = 1.0
      if (ch_nut(irch)%rs2 <= 0.) ch_nut(irch)%rs2 = 0.05
      if (ch_nut(irch)%rs3 <= 0.) ch_nut(irch)%rs3 = 0.5
      if (ch_nut(irch)%rs4 <= 0.) ch_nut(irch)%rs4 = 0.05
      if (ch_nut(irch)%rs5 <= 0.) ch_nut(irch)%rs5 = 0.05
      if (ch_nut(irch)%rs6 <= 0.) ch_nut(irch)%rs6 = 2.5
      if (ch_nut(irch)%rs7 <= 0.) ch_nut(irch)%rs7 = 2.5
      if (ch_nut(irch)%rk1 <= 0.) ch_nut(irch)%rk1 = 1.71
      if (ch_nut(irch)%rk2 <= 0.) ch_nut(irch)%rk2 = 1.0    ! previous 50.0
      if (ch_nut(irch)%rk4 <= 0.) ch_nut(irch)%rk4 = 2.0
      if (ch_nut(irch)%rk5 <= 0.) ch_nut(irch)%rk5 = 2.0
      if (ch_nut(irch)%rk6 <= 0.) ch_nut(irch)%rk6 = 1.71
      if (ch_nut(irch)%bc1 <= 0.) ch_nut(irch)%bc1 = 0.55 
      if (ch_nut(irch)%bc2 <= 0.) ch_nut(irch)%bc2 = 1.1
      if (ch_nut(irch)%bc3 <= 0.) ch_nut(irch)%bc3 = 0.21
      if (ch_nut(irch)%bc4 <= 0.) ch_nut(irch)%bc4 = 0.35
      
!! change units from day to hour if hourly routing is performed
      if (bsn_cc%event > 2) then
        ch_nut(irch)%rs1 = ch_nut(irch)%rs1 / 24.
        ch_nut(irch)%rs2 = ch_nut(irch)%rs2 / 24.
        ch_nut(irch)%rs3 = ch_nut(irch)%rs3 / 24.
        ch_nut(irch)%rs4 = ch_nut(irch)%rs4 / 24.
        ch_nut(irch)%rs5 = ch_nut(irch)%rs5 / 24.
        ch_nut(irch)%rk1 = ch_nut(irch)%rk1 / 24.
        ch_nut(irch)%rk2 = ch_nut(irch)%rk2 / 24.
        ch_nut(irch)%rk3 = ch_nut(irch)%rk3 / 24.
        ch_nut(irch)%rk4 = ch_nut(irch)%rk4 / 24.
        ch_nut(irch)%bc1 = ch_nut(irch)%bc1 / 24.
        ch_nut(irch)%bc2 = ch_nut(irch)%bc2 / 24.
        ch_nut(irch)%bc3 = ch_nut(irch)%bc3 / 24.
        ch_nut(irch)%bc4 = ch_nut(irch)%bc4 / 24.
      end if
        end do
        exit
      enddo
      endif
      close(105)

      return
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
      end subroutine ch_read_nut