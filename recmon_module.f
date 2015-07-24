      module recmon_module

      use hydrograph_module 
      use time_module
    
      contains
      
      subroutine recmon_allo
        allocate (mn(12))
        allocate (mn(12)%yr(myr))
        allocate (mn(12)%yr(myr)%hmon(mrecm))
      end subroutine recmon_allo

      
      subroutine readmon

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     reads in the input data for the recmon command
     
!!     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     i            |none          |file number
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!     name            |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     eof          |none          |end of file flag (=-1 at end of file)
!!     ia1          |none          |dummy variable
!!     ia2          |none          |dummy variable
!!     ii           |none          |counter
!!     iya          |none          |counter
!!     mon          |none          |month counter
!!     titldum      |NA            |description line
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      character (len=80) :: titldum
      character (len=13) :: month_in
      integer :: ii, iya, mon, eof, ia1, ia2, begmon
      
!!    initialize variables
      eof = 0
      inum1 = ihout
             
      month_in = ""
      read (102,5100) month_in
      month_in = month_in(1:index(month_in,'.')-1)
      call caps(month_in)
      open (107,file=month_in,recl=350)

      do ii = 1, 6
        read (107,5000) titldum
      end do

!!    Read until the year is the beginning year of simulation
        iya = 1
        mon = 1
        do
          read (107,*,iostat=eof) ia1, ia2,                             
     &                  mn(mon)%yr(iya)%hmon(inum1)%flo,                
     &                  mn(mon)%yr(iya)%hmon(inum1)%sed,                
     &                  mn(mon)%yr(iya)%hmon(inum1)%orgn,               
     &                  mn(mon)%yr(iya)%hmon(inum1)%sedp,               
     &                  mn(mon)%yr(iya)%hmon(inum1)%no3,                
     &                  mn(mon)%yr(iya)%hmon(inum1)%nh3,                
     &                  mn(mon)%yr(iya)%hmon(inum1)%no2,                
     &                  mn(mon)%yr(iya)%hmon(inum1)%solp,               
     &                  mn(mon)%yr(iya)%hmon(inum1)%cbod,               
     &                  mn(mon)%yr(iya)%hmon(inum1)%dox,                
     &                  mn(mon)%yr(iya)%hmon(inum1)%chla,               
     &                  mn(mon)%yr(iya)%hmon(inum1)%psol,               
     &                  mn(mon)%yr(iya)%hmon(inum1)%psor,               
     &                  mn(mon)%yr(iya)%hmon(inum1)%bacp,               
     &                  mn(mon)%yr(iya)%hmon(inum1)%baclp,              
     &                  mn(mon)%yr(iya)%hmon(inum1)%met1,               
     &                  mn(mon)%yr(iya)%hmon(inum1)%met2,               
     &                  mn(mon)%yr(iya)%hmon(inum1)%met3
	   
		  if (ia2 == time%yrc) exit
	      if (eof < 0) exit
        end do



      do iya = 1, time%nbyr + 2  !2 extra for scenarios
          if (iya == 1) then
            begmon = 2
          else
            begmon = 1
          end if
        do mon = begmon, 12
          read (107,*,iostat=eof) ia1, ia2,                             
     &                  mn(mon)%yr(iya)%hmon(inum1)%flo,                
     &                  mn(mon)%yr(iya)%hmon(inum1)%sed,                
     &                  mn(mon)%yr(iya)%hmon(inum1)%orgn,               
     &                  mn(mon)%yr(iya)%hmon(inum1)%sedp,               
     &                  mn(mon)%yr(iya)%hmon(inum1)%no3,                
     &                  mn(mon)%yr(iya)%hmon(inum1)%nh3,                
     &                  mn(mon)%yr(iya)%hmon(inum1)%no2,                
     &                  mn(mon)%yr(iya)%hmon(inum1)%solp,               
     &                  mn(mon)%yr(iya)%hmon(inum1)%cbod,               
     &                  mn(mon)%yr(iya)%hmon(inum1)%dox,                
     &                  mn(mon)%yr(iya)%hmon(inum1)%chla,               
     &                  mn(mon)%yr(iya)%hmon(inum1)%psol,               
     &                  mn(mon)%yr(iya)%hmon(inum1)%psor,               
     &                  mn(mon)%yr(iya)%hmon(inum1)%bacp,               
     &                  mn(mon)%yr(iya)%hmon(inum1)%baclp,              
     &                  mn(mon)%yr(iya)%hmon(inum1)%met1,               
     &                  mn(mon)%yr(iya)%hmon(inum1)%met2,               
     &                  mn(mon)%yr(iya)%hmon(inum1)%met3
          if (eof < 0) exit
        end do
        if (eof < 0) exit
      end do

      close (107)
         
      return
 5000 format (a80)
 5100 format (10x,2a13)
      end subroutine readmon


      subroutine recmon

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine inputs measured loadings to the stream network
!!    for routing through the watershed where the records are summarized
!!    on a monthly basis

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    curyr         |none         |year of simulation
!!    ihout         |none         |hydrograph storage location number
!!    inum1         |none         |file number
!!    i_mo          |none         |month of simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name             |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name       |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii         |none          |counter
!!    j          |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module

      integer :: ii
      inum1 = ihout
      
      !! zero flow out variables
      hd(ihout) = hz
      do ii = 1, nstep
        ts(ii)%hh(ihout) = hz 
      end do

      hd(ihout) = mn(i_mo)%yr(time%yrs)%hmon(inum1)
      
      if (bsn_cc%event > 2) then
      do ii = 1, nstep
      stp = real(nstep)
      ts(ii)%hh(ihout)%flo = mn(i_mo)%yr(time%yrs)%hmon(inum1)%flo / stp
      ts(ii)%hh(ihout)%sed = mn(i_mo)%yr(time%yrs)%hmon(inum1)%sed / stp
      ts(ii)%hh(ihout)%orgn=mn(i_mo)%yr(time%yrs)%hmon(inum1)%orgn / stp
      ts(ii)%hh(ihout)%sedp=mn(i_mo)%yr(time%yrs)%hmon(inum1)%sedp / stp
      ts(ii)%hh(ihout)%no3 = mn(i_mo)%yr(time%yrs)%hmon(inum1)%no3 / stp
      ts(ii)%hh(ihout)%solp=mn(i_mo)%yr(time%yrs)%hmon(inum1)%solp / stp
      ts(ii)%hh(ihout)%psol=mn(i_mo)%yr(time%yrs)%hmon(inum1)%psol / stp
      ts(ii)%hh(ihout)%psor=mn(i_mo)%yr(time%yrs)%hmon(inum1)%psor / stp
      ts(ii)%hh(ihout)%chla=mn(i_mo)%yr(time%yrs)%hmon(inum1)%chla / stp
      ts(ii)%hh(ihout)%nh3=mn(i_mo)%yr(time%yrs)%hmon(inum1)%nh3 / stp
      ts(ii)%hh(ihout)%no2 = mn(i_mo)%yr(time%yrs)%hmon(inum1)%no2 / stp
      ts(ii)%hh(ihout)%cbod=mn(i_mo)%yr(time%yrs)%hmon(inum1)%cbod / stp
      ts(ii)%hh(ihout)%dox = mn(i_mo)%yr(time%yrs)%hmon(inum1)%dox / stp
      ts(ii)%hh(ihout)%bacp=mn(i_mo)%yr(time%yrs)%hmon(inum1)%bacp / stp
      ts(ii)%hh(ihout)%baclp=mn(i_mo)%yr(time%yrs)%hmon(inum1)%baclp/stp
      ts(ii)%hh(ihout)%met1=mn(i_mo)%yr(time%yrs)%hmon(inum1)%met1/ stp
      ts(ii)%hh(ihout)%met2=mn(i_mo)%yr(time%yrs)%hmon(inum1)%met2 / stp
      ts(ii)%hh(ihout)%met3=mn(i_mo)%yr(time%yrs)%hmon(inum1)%met3 / stp
      end do
      end if

      return
      end subroutine recmon
      
      end module recmon_module