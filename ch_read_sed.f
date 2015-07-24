      subroutine ch_read_sed
      
      use input_file_module

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

      inquire (file=in_cha%sed,exist=i_exist)
      if (i_exist == 0 .or. in_cha%sed == 'null') then
        write (4444,4445) in_cha%sed, mch, imax, '         null'
        allocate (ch_sed(0:0))
      else
      do
        open (105,file=in_cha%sed)
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (105,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            mch = mrch + 1
          end do        
        
        allocate (ch_sed(0:imax))
        rewind (105)
        read (105,*) titldum
        read (105,*) header
 
        write (4444,4446) in_cha%sed, mch, imax
             
        do ich = 1, mch
          read (105,*,iostat=eof) i
          backspace (105)
          read (105,*) k, ch_sed(i)
          if (eof < 0) exit
          
       if (ch_sed(irch)%tc_bnk <= 0.) ch_sed(irch)%tc_bnk=0. !! Critical shear stress (N.m^2)
        if (ch_sed(irch)%tc_bed <= 0.) ch_sed(irch)%tc_bed=0. !! Critical shear stress (N.m^2)

      if (ch_sed(irch)%eqn <= 0) then
        ch_sed(irch)%eqn=0 !! SWAT Default sediment routing routine
        if (ch_sed(irch)%cov1 <= 0.0) ch_sed(irch)%cov1 = 0.0
        if (ch_sed(irch)%cov2 <= 0.0) ch_sed(irch)%cov2 = 0.0
        if (ch_sed(irch)%cov1 >= 1.0) ch_sed(irch)%cov1 = 1.0
        if (ch_sed(irch)%cov2 >= 1.0) ch_sed(irch)%cov2 = 1.0
	else 
        if (ch_sed(irch)%cov1 <= 0.0) ch_sed(irch)%cov1 = 1.0
        if (ch_sed(irch)%cov2 <= 0.0) ch_sed(irch)%cov2 = 1.0
        if (ch_sed(irch)%cov1 >= 25.) ch_sed(irch)%cov1 = 25.
        if (ch_sed(irch)%cov2 >= 25.) ch_sed(irch)%cov2 = 25.
	end if
	  

!!    Bank material is assumed to be silt type partcile if not given.
      if (ch_sed(irch)%bnk_d50 <= 1.e-6) ch_sed(irch)%bnk_d50 = 50. !! Units are in Micrometer
      if (ch_sed(irch)%bnk_d50 > 10000) ch_sed(irch)%bnk_d50 = 10000.

!!    Bed material is assumed to be sand type partcile if not given.
      if (ch_sed(irch)%bed_d50 <= 1.e-6) ch_sed(irch)%bed_d50 = 500 !! Units are in Micrometer
      if (ch_sed(irch)%bed_d50 > 10000) ch_sed(irch)%bed_d50 = 10000. 

!!    Bulk density of channel bank sediment 
	if (ch_sed(irch)%bnk_bd <= 1.e-6) ch_sed(irch)%bnk_bd = 1.40 !! Silty loam bank

!!    Bulk density of channel bed sediment
	if (ch_sed(irch)%bed_bd <= 1.e-6) ch_sed(irch)%bed_bd = 1.50  !! Sandy loam bed

!!  An estimate of channel bank erodibility coefficient from jet test if it is not available
!!  Units of kd is (cm^3/N/s)
!!  Base on Hanson and Simon, 2001
      if (ch_sed(irch)%bnk_kd <= 1.e-6) then
	  if (ch_sed(irch)%tc_bnk <= 1.e-6) then
	    ch_sed(irch)%bnk_kd = 0.2
	  else 
          ch_sed(irch)%bnk_kd = 0.2 / sqrt(ch_sed(irch)%tc_bnk)
	  end if
	end if

!!  An estimate of channel bed erodibility coefficient from jet test if it is not available
!!  Units of kd is (cm^3/N/s)
!!  Base on Hanson and Simon, 2001
      if (ch_sed(irch)%bed_kd <= 1.e-6) then
	  if (ch_sed(irch)%tc_bed <= 1.e-6) then
	    ch_sed(irch)%bed_kd = 0.2
	  else 
          ch_sed(irch)%bed_kd = 0.2 / sqrt(ch_sed(irch)%tc_bed)
	  end if
      end if

      sumerod = 0.
      do mo = 1, 12
        sumerod = sumerod + ch_sed(irch)%erod(mo)
      end do

      if (sumerod < 1.e-6) then
        do mo = 1, 12
          ch_sed(irch)%erod(mo) = ch_sed(irch)%cov1
        end do
      end if
      
        end do
        exit
      end do
      end if
      
      close(105)
      
!!!!!!!!!!!!JEFF CODE!!!!!!!!!!!!!
! code from ch_readrte.f subroutine
!    initialize variables for channel degradation
!      ch(irch)%di = chdb(irch)%d 
!      ch(irch)%li = chdb(irch)%l
!      ch(irch)%si = chdb(irch)%s
!      ch(irch)%wi = chdb(irch)%w
      
!!    initialize flow routing variables
!     call ch_ttcoef (irch)
      
!     do irch = 1, RCHMAX
!        irchdb = ch_dat(irch)%hyd
!        ch(irch)%di = chdb(irchdb)%db
!        ...
!     end do
      
      return
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
      end subroutine ch_read_sed