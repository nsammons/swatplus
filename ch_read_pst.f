      subroutine ch_read_pst
      
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

      inquire (file=in_cha%pest,exist=i_exist)
      if (i_exist == 0 .or. in_cha%pest == 'null') then
        write (4444,4445) in_cha%pest, mch, imax, '         null'
        allocate (ch_pst(0:0))
      else
      do
        open (105,file=in_cha%pest)
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
        
        allocate (ch_pst(0:imax))
        rewind (105)
        read (105,*) titldum
        read (105,*) header
        
        write (4444,4446) in_cha%pest, mch, imax
          
        do ich = 1, mch
          read (105,*,iostat=eof) i
          backspace (105)
          read (105,*) k, ch_pst(i)
          if (eof < 0) exit
          
      if (ch_pst(irch)%pst_vol <= 1.e-6) ch_pst(irch)%pst_vol = 0.01
      if (ch_pst(irch)%pst_koc <= 1.e-6) ch_pst(irch)%pst_koc = 0.
      if (ch_pst(irch)%pst_stl <= 1.e-6) ch_pst(irch)%pst_stl = 1.
      if (ch_pst(irch)%pst_rsp <= 1.e-6) ch_pst(irch)%pst_rsp = 0.002
      if (ch_pst(irch)%pst_mix <= 1.e-6) ch_pst(irch)%pst_mix = 0.001
      if (ch_pst(irch)%sedpst_conc <= 1.e-6)ch_pst(irch)%sedpst_conc=0.
      if (ch_pst(irch)%sedpst_rea <= 1.e-6) ch_pst(irch)%sedpst_rea=0.05
      if (ch_pst(irch)%sedpst_bry <= 1.e-6)ch_pst(irch)%sedpst_bry=0.002
      if (ch_pst(irch)%sedpst_act <= 1.e-6)ch_pst(irch)%sedpst_act=0.030

        end do
        exit
      enddo
      endif
      close(105)

      return
4445  format (1x,a25,1x,2i6,a)
4446  format (1x,a25,1x,2i6)
      end subroutine ch_read_pst