      module recall_daily
       
       use hydrograph_module
       use time_module
      
      contains
      
!!
      subroutine  read_recday
      
      character (len=13) :: day_in      
      integer :: ifirsta, idapa, iypa

      inum1 = ihout
      ifirsta = 1
      day_in = ""
      read (102,5100) day_in
      call caps(day_in)
      open (555+inum1,file=day_in,recl=350)
      do ii = 1, 6
        read (555+inum1,5200) titldum
      end do
      !! different start date than the SWAT run
      if (ifirsta == 1) then
        do 
          read (555+inum1,*) idapa, iypa
	    if(idapa == time%idaf .and. iypa == time%yrc) exit
        end do
        ifirsta = 0
      endif
      
 5100 format (10x,2a13)
 5200 format (a80)
      return
      end subroutine read_recday
      

      subroutine recday
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine inputs measured loadings to the stream network for 
!!    routing through the watershed where the records are summarized on a
!!    daily basis

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    inum1       |none          |reach number
!!    ihout       |none          |hydrograph storage location number
!!    inum1       |none          |file number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name             |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    idap        |julian date   |julian date of record
!!    ii          |none          |counter
!!    iyp         |year          |year of record
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module

      integer :: ii

!! initialize variables

      hd(ihout) = hz
      do ii = 1, nstep
        ts(ii)%hh(ihout) = hz
      end do

      read (555+inum1,*) idap, iyp, hd(ihout)%flo, hd(ihout)%sed,       
     &   hd(ihout)%orgn, hd(ihout)%sedp, hd(ihout)%no3, hd(ihout)%nh3,  
     &   hd(ihout)%no2, hd(ihout)%solp, hd(ihout)%cbod, hd(ihout)%dox,  
     &   hd(ihout)%chla, hd(ihout)%psol, hd(ihout)%psor, hd(ihout)%bacp,
     &   hd(ihout)%baclp, hd(ihout)%met1, hd(ihout)%met2, hd(ihout)%met3

      if (bsn_cc%event > 2) then
        stp = real(nstep)
        do ii = 1, nstep
          ts(ii)%hh(ihout)%flo = hd(ihout)%flo / stp
          ts(ii)%hh(ihout)%sed = hd(ihout)%sed / stp
          ts(ii)%hh(ihout)%orgn = hd(ihout)%orgn / stp
          ts(ii)%hh(ihout)%sedp = hd(ihout)%sedp / stp
          ts(ii)%hh(ihout)%no3 = hd(ihout)%no3 / stp
          ts(ii)%hh(ihout)%solp = hd(ihout)%solp / stp
          ts(ii)%hh(ihout)%psol = hd(ihout)%psol / stp
          ts(ii)%hh(ihout)%psor = hd(ihout)%psor / stp
          ts(ii)%hh(ihout)%chla = hd(ihout)%chla / stp
          ts(ii)%hh(ihout)%nh3 = hd(ihout)%nh3 / stp
          ts(ii)%hh(ihout)%no2 = hd(ihout)%no2 / stp
          ts(ii)%hh(ihout)%cbod = hd(ihout)%cbod / stp
          ts(ii)%hh(ihout)%dox = hd(ihout)%dox / stp
          ts(ii)%hh(ihout)%bacp = hd(ihout)%bacp / stp
          ts(ii)%hh(ihout)%baclp = hd(ihout)%baclp / stp
          ts(ii)%hh(ihout)%met1 = hd(ihout)%met1 / stp
          ts(ii)%hh(ihout)%met2 = hd(ihout)%met2 / stp
          ts(ii)%hh(ihout)%met3 = hd(ihout)%met3 / stp
        end do
      end if

      return
      end subroutine recday

      
      end module recall_daily