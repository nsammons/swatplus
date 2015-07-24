
      module recyear_module

       use hydrograph_module
       use time_module
     
      contains

      
      subroutine readyr

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     reads in the input data for the recyear command
     
!!     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     i            |none          |file number
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     eof          |none          |end of file flag (=-1 at end of file)
!!     ii           |none          |counter
!!     iya          |none          |counter
!!     titldum      |NA            |description line
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      character (len=80) :: titldum
      character (len=13) :: year_in
      integer :: ii, iya, eof, ia1

!!    initialize variables
      eof = 0
      allocate (yr(myr))
      
      year_in = ""
      read (102,5100) year_in
      call caps(year_in)
      open (108,file=year_in,recl=350)
  
      do ii = 1, 6 
        read (108,5000) titldum
      end do

!!    Read until the year is the beginning year of simulation
      iya = 1
        do
        read (108,*,iostat=eof) ia1, yr(iya)%hyr(ihout)%flo,            
     &              yr(iya)%hyr(ihout)%sed,                             
     &              yr(iya)%hyr(ihout)%orgn,                            
     &              yr(iya)%hyr(ihout)%sedp,                            
     &              yr(iya)%hyr(ihout)%no3,                             
     &              yr(iya)%hyr(ihout)%nh3,                             
     &              yr(iya)%hyr(ihout)%no2,                             
     &              yr(iya)%hyr(ihout)%solp,                            
     &              yr(iya)%hyr(ihout)%cbod,                            
     &              yr(iya)%hyr(ihout)%dox,                             
     &              yr(iya)%hyr(ihout)%chla,                            
     &              yr(iya)%hyr(ihout)%psol,                            
     &              yr(iya)%hyr(ihout)%psor,                            
     &              yr(iya)%hyr(ihout)%bacp,                            
     &              yr(iya)%hyr(ihout)%baclp,                           
     &              yr(iya)%hyr(ihout)%met1,                            
     &              yr(iya)%hyr(ihout)%met2,                            
     &              yr(iya)%hyr(ihout)%met3
            if (ia1 == time%yrc) exit
	      if (eof < 0) exit
        end do

      do iya = 2, time%nbyr + 2  !2 extra for forecast scenarios
        read (108,*,iostat=eof) ia1, yr(iya)%hyr(ihout)%flo,            
     &              yr(iya)%hyr(ihout)%sed,                             
     &              yr(iya)%hyr(ihout)%orgn,                            
     &              yr(iya)%hyr(ihout)%sedp,                            
     &              yr(iya)%hyr(ihout)%no3,                             
     &              yr(iya)%hyr(ihout)%nh3,                             
     &              yr(iya)%hyr(ihout)%no2,                             
     &              yr(iya)%hyr(ihout)%solp,                            
     &              yr(iya)%hyr(ihout)%cbod,                            
     &              yr(iya)%hyr(ihout)%dox,                             
     &              yr(iya)%hyr(ihout)%chla,                            
     &              yr(iya)%hyr(ihout)%psol,                            
     &              yr(iya)%hyr(ihout)%psor,                            
     &              yr(iya)%hyr(ihout)%bacp,                            
     &              yr(iya)%hyr(ihout)%baclp,                           
     &              yr(iya)%hyr(ihout)%met1,                            
     &              yr(iya)%hyr(ihout)%met2,                            
     &              yr(iya)%hyr(ihout)%met3
        if (eof < 0) exit
      end do

      close (108)

      return
 5000 format (a80)
 5100 format (10x,2a13)
      end subroutine readyr            
      
      subroutine recyear
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine inputs measured loadings to the stream network
!!    for routing through the watershed where the records are summarized
!!    on an annual basis

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlpyr(:,:)|# cfu/100ml  |average daily loading of less persistent
!!                               |bacteria for year
!!    bactpyr(:,:)|# cfu/100ml   |average daily loading of persistent bacteria
!!                               |for year
!!    cbodyr(:,:) |kg/day        |average daily loading of CBOD for year
!!    chlayr(:,:) |kg/day        |average daily loading of chlorophyll-a for year
!!    cmtl1yr(:,:)|kg/day        |average daily loading of conservative metal #1
!!                               |for year
!!    cmtl2yr(:,:)|kg/day        |average daily loading of conservative metal #2
!!                               |for year
!!    cmtl3yr(:,:)|kg/day        |average daily loading of conservative metal #3
!!                               |for year
!!    curyr       |none          |year of simulation
!!    disoxyr(:,:)|kg/day        |average daily loading of dissolved oxygen for 
!!                               |year
!!    floyr(:,:)  |m**3/d        |average daily water loading for year
!!    ihout       |none          |hydrograph storage location number
!!    inum1       |none          |file number
!!    minpyr(:,:) |kg P/day      |average daily mineral P loading for year
!!    mvaro       |none          |max number of variables routed through the
!!                               |reach
!!    nh3yr(:,:)  |kg N/day      |average daily NH3-N loading for year
!!    no2yr(:,:)  |kg N/day      |average daily NO2-N loading for year
!!    no3yr(:,:)  |kg N/day      |average daily NO3-N loading for year
!!    orgnyr(:,:) |kg N/day      |average daily organic N loading for year
!!    orgpyr(:,:) |kg P/day      |average daily organic P loading for year
!!    sedyr(:,:)  |metric tons/d |average daily sediment loading for year
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name             |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    j           |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module

      integer :: ii
      inum1 = ihout
      
!! zero flow out variables
      hd(ihout) = hz
      do ii = 1, nstep
        ts(ii)%hh(ihout) = hz
      end do

      hd(ihout) = yr(time%yrs)%hmon(inum1)

      if (bsn_cc%event > 2) then
        stp = real(nstep)
        do ii = 1, nstep
          ts(ii)%hh(ihout)%flo = yr(time%yrs)%hmon(inum1)%flo / stp
          ts(ii)%hh(ihout)%sed = yr(time%yrs)%hmon(inum1)%sed / stp
          ts(ii)%hh(ihout)%orgn = yr(time%yrs)%hmon(inum1)%orgn / stp
          ts(ii)%hh(ihout)%sedp = yr(time%yrs)%hmon(inum1)%sedp / stp
          ts(ii)%hh(ihout)%no3 = yr(time%yrs)%hmon(inum1)%no3 / stp
          ts(ii)%hh(ihout)%solp = yr(time%yrs)%hmon(inum1)%solp / stp
          ts(ii)%hh(ihout)%psol = yr(time%yrs)%hmon(inum1)%psol / stp
          ts(ii)%hh(ihout)%psor = yr(time%yrs)%hmon(inum1)%psor / stp
          ts(ii)%hh(ihout)%chla = yr(time%yrs)%hmon(inum1)%chla / stp
          ts(ii)%hh(ihout)%nh3 = yr(time%yrs)%hmon(inum1)%nh3 / stp
          ts(ii)%hh(ihout)%no2 = yr(time%yrs)%hmon(inum1)%no2 / stp
          ts(ii)%hh(ihout)%cbod = yr(time%yrs)%hmon(inum1)%cbod / stp
          ts(ii)%hh(ihout)%dox = yr(time%yrs)%hmon(inum1)%dox / stp
          ts(ii)%hh(ihout)%bacp = yr(time%yrs)%hmon(inum1)%bacp / stp
          ts(ii)%hh(ihout)%baclp = yr(time%yrs)%hmon(inum1)%baclp / stp
          ts(ii)%hh(ihout)%met1 = yr(time%yrs)%hmon(inum1)%met1 / stp
          ts(ii)%hh(ihout)%met2 = yr(time%yrs)%hmon(inum1)%met2 / stp
          ts(ii)%hh(ihout)%met3 = yr(time%yrs)%hmon(inum1)%met3 / stp
        end do
      end if

      return
      end subroutine recyear
 
      end module recyear_module