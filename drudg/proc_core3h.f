*
* Copyright (c) 2021 NVI, Inc.
*
* This file is part of VLBI Field System
* (see http://github.com/nvi-inc/fs).
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/>.
*
      subroutine proc_core3h(cproc_core8h,icode)
      implicit none
! Generate the core3h procs...
! A large part of this is generating the bitmasks

      include 'hardware.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/statn.ftni'
      include 'drcom.ftni'

! functions
      integer itras
! passed
      character*(*) cproc_core8h
      integer icode
! function
!      integer iwhere_in_string_list

! DBBC3_DDCs 128 BBCs.  
! The BBCs are tied to 8 boards, with each board having 16 BBCs.  The order is
!           Mask2   Mask1
! Board01:  65-72,  1-8
! Board02:  73-80,  9-16
! Board03:  81-99,  17-24
!..
! Board08: 121-128, 57-67
! Each BBC is tied to upto 4 channel: LM,LS,UM,US.
! If we are recording a channel want to turn on the corresponding bit.
! Total number of bits in a mask is 4*8=32

! Our approach is simple.
!  We start with one mask for each BBC and set the appropriate bitrs.
!  Then we use this to set the bits in iboardmask.
      integer*4 imask(8,2) 
      integer   ibbc_mask(max_bbc)
      character*5 lsked_csb(max_chan)  !for debugging purposes. 
      integer*4 imask_temp
      integer iboard                   !
      integer ihalf
      integer ibbc_tmp
      integer num_shift  
      integer ipass               !which pass. hardcoded to 1
      integer ic                  !counter of channels.
      
      integer isb, isb_out        !sideband
      integer ihd                 !counter over headstacks. 
      integer ibit                !counter over bits
      integer ibbc 
      integer num_tracks
      logical kdebug 
      integer iset 
      character*1 lul(2)            !ASCII "U","L"
      character*1 lsm(2)            !ASCII "S","M"
     
      data lul/"U","L"/
      data lsm/"S","M"/
      kdebug=.false. 
!      kdebug=.true. 

        do iboard=1,8
        imask(iboard,1)=0
        imask(iboard,2)=0
      end do 
      do ibbc=1,max_bbc
       ibbc_mask(ibbc)=0
      end do 

      ipass=1
!      write(*,*)
      if(kdebug) then
        write(*,'(a)') "  sb   sbo   bit  bbc  chan pass stn  code  CSB"
      endif
    
! Go through all of the channels. 
!    if(kdebug) write(*,*) "ic ibbc isb ibit" 
      num_tracks=0
      do ic=1,max_chan
         do isb=1,2
            isb_out=isb
            if(abs(freqrf(ic,istn,icode)).lt.freqlo(ic,istn,icode)) then
              isb_out=3-isb    !swap the sidebands
            endif ! reverse sidebands
!            do ihd=1,max_headstack
            do ihd=1,1
            do ibit=1,2 
              if (itras(isb,ibit,ihd,ic,ipass,istn,icode).ne.-99) then         !number of tracks set.
                ibbc=ibbcx(ic,istn,icode)   !this is the BBC#
                num_tracks=num_tracks+1

!                 if(kdebug) write(*,'(4i4)') ic, ibbc,  isb, ibit 
! order of bits Most to least is LSBM, LSBS, USBM, USBS
! isb_out=1 is USB. 
!                if(isb_out .eq. 1 .and. ibit .eq. 1) iset=1 
!                if(isb_out .eq. 1 .and. ibit .eq. 2) iset=2
!                if(isb_out .eq. 2 .and. ibit .eq. 1) iset=4
!                if(isb_out .eq. 2 .and. ibit .eq. 2) iset=8 
                if(isb_out .eq. 1) then
                    iset=ibit
                else
                    iset=ibit*4
                endif 
                ibbc_mask(ibbc)=ibbc_mask(ibbc)+iset    
!                write(*,*) ibbc, iset, ibbc_mask(ibbc)              

                write(lsked_csb(num_tracks),'(i3.3,a1,a1)')
     >            ibbc, lul(isb_out), lsm(ibit)
                if(kdebug) then
                  write(*,'(8i5,1x,a)')  isb,isb_out,ibit,ibbc,ic,
     >                   ipass,istn,icode,lsked_csb(num_tracks)
                endif
              endif
            enddo
          enddo
        enddo
      enddo
! Now have all of the BBCs. Set the board_masks
      do ibbc=1,max_bbc
        if(ibbc_mask(ibbc) .ne. 0) then 
!          write(*,'("ibbc msk ",i4,1x,z8)') ibbc, ibbc_mask(ibbc)  
          ibbc_tmp=ibbc
          ihalf=1 
          if(ibbc .gt. 64) then
            ihalf=2
            ibbc_tmp=ibbc_tmp-64
          endif
          iboard=1
         do while(ibbc_tmp .gt. 8)
           ibbc_tmp=ibbc_tmp-8
           iboard=iboard+1
         end do 
    
! Example.  IB=35.   Ihalf=1, Iboard=4, ibbc_tmp=3 
          num_shift=(ibbc_tmp-1)*4 
!          write(*,*) "Num_shift ", num_shift 
          imask_temp=ishft(ibbc_mask(ibbc),num_shift)  
          imask(iboard,ihalf)=ior(imask(iboard,ihalf),imask_temp)
!          write(*,'("BBC board half ",3i4)') ibbc, iboard, ihalf
!          write(*,'("Mask ",2(1x,z32))')imask_temp, imask(iboard,ihalf) 

        endif 
      end do 
! Now output command the procedure
      call proc_write_define(lu_outfile, luscn,cproc_core8h)

      write(lu_outfile,'(a)') "core3h_mode0=begin,$"
      do iboard =1,8
!        write(*,*) "BRD ",iboard, imask(iboard,1), imask(iboard,2)
        if(imask(iboard,1) .ne. 0 .or. imask(iboard,2) .ne. 0) then 
         write(cbuf,
     &     '("core3h_mode",i1,"=",2("0x",z8.8,","),",",f6.2,",$")') 
     &     iboard, imask(iboard,2), imask(iboard,1), 
     &     samprate(istn,icode)
!         write(*,*) idec, samprate(istn,icode) 
         call drudg_write(lu_outfile,cbuf) 
        endif
      end do
      write(lu_outfile,'(a)') "core3h_mode0=end,$"
      write(lu_outfile,"(a)") 'enddef'

      return
      end 

        
        







