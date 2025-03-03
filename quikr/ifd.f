*
* Copyright (c) 2020 NVI, Inc.
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
      subroutine ifd(ip)
C  IF DISTRIBUTOR CONTROL    <910324.0011>
C 
      dimension ip(5) 
C     INPUT VARIABLES:
C        IP(1)  - class number of input parameter buffer. 
C 
C     OUTPUT VARIABLES: 
C        IP(1) - CLASS
C        IP(2) - # REC
C        IP(3) - ERROR
C        IP(4) - who we are 
C 
C   COMMON BLOCKS USED
      include '../include/fscom.i'
C
C     CALLED SUBROUTINES: GTPRM2
C
C   LOCAL VARIABLES
C        NCHAR  - number of characters in buffer
C        ICH    - character counter
      integer*2 IBUF(20)                      ! Class buffer
      integer get_buf,ichcm_ch
      logical kdef(2),kfirst
      character cjchar
      dimension inp(2)                        ! input indices for if1 & if2
      dimension iat(2)                        ! if attenuator settings
      dimension iold(2)
      dimension iparm(2)                      ! parameters from gtprm
      dimension ireg(2)                       ! registers from exec calls
      equivalence (reg,ireg(1)),(parm,iparm(1))
C
C  LOCAL CONSTANT
      parameter (ilen=40)
C
      include '../include/boz.i'
C
C  PROGRAMMER: NRV
C     LAST MODIFIED: 810207
C
C 
C     1. If we have a class buffer, then we are to set the IFD. 
C     If no class buffer, we have been requested to read the IFD. 
C 
      kfirst=.true.
      kdef(1)=.false.
      kdef(2)=.false.
      ichold = -99
      iclcm = ip(1) 
      if (iclcm.eq.0) then
        ierr = -1
        goto 990
      endif
      call ifill_ch(ibuf,1,ilen,' ')
      ireg(2) = get_buf(iclcm,ibuf,-ilen,idum,idum)
      nchar = ireg(2) 
      ieq = iscn_ch(ibuf,1,nchar,'=')
      if (ieq.eq.0) goto 500                ! if no parameters, read device
      call fs_get_inp1if(inp1if)
      call fs_get_inp2if(inp2if)
      if (cjchar(ibuf,ieq+1).eq.'?') then
        ip(1) = 0
        ip(4) = ocp77
        call ifdis(ip,iclcm,kfirst)
        return
      endif
C 
      if (ichcm(ibuf,ieq+1,ltsrs,1,ilents).eq.0) goto 600
      if (ichcm(ibuf,ieq+1,lalrm,1,ilenal).eq.0) goto 700 
C 
C 
C     2. Step through buffer getting each parameter and decoding it.
C     Command from user has these parameters: 
C                   IFD=<atten1>,<atten2>,<input1>,<input2> 
C     Choices are <input>: NOR, ALT.  Default NOR.
C                 <atten>: attenuator setting, 0 to 63db. Default current.
C                          If number is signed, interpret as a change to
C                          the present attenuator setting.
C                          IF atten is MAX, go to 63 and remember old value.
C                          IF atten is OLD, return to old value.
C
C     2.1 ATTEN1 AND ATTEN2, PARAMETERS 1 AND 2
C
      ich = 1+ieq
      do i=1,2
        ist = ich
        call gtprm2(ibuf,ich,nchar,0,parm,ierr)
        if(ichcm_ch(iparm,1,'max').eq.0) then
          call fs_get_iat1if(iat1if)
          if(i.eq.1) iold(1)=iat1if
          call fs_get_iat2if(iat2if)
          if(i.eq.2) iold(2)=iat2if
          iat(i)=63
        else if(ichcm_ch(iparm,1,'old').eq.0) then
          if(i.eq.1) iat(1)=iol1if_fs
          if(i.eq.2) iat(2)=iol2if_fs
        else
          ich=ist
          call gtprm2(ibuf,ich,nchar,1,parm,ierr)
          if (ierr.eq.2) then
            kdef(i)=.true.     ! default
            iat(i)=0
          else if (ierr.eq.1) then
            call fs_get_iat1if(iat1if)
            if (i.eq.1) iat(i) = iat1if
            call fs_get_iat2if(iat2if)
            if (i.eq.2) iat(i) = iat2if
          else if (iparm(1).lt.0.or.iscn_ch(ibuf,ist,ich-1,'+').ne.0)
     $           then
            call fs_get_iat1if(iat1if)
            if (i.eq.1) iat(i) = iat1if + iparm(1)
            call fs_get_iat2if(iat2if)
            if (i.eq.2) iat(i) = iat2if + iparm(1)
          else
            iat(i) = iparm(1)
          endif
        endif
        if (iat(i).lt.0.or.iat(i).gt.63) then
           ierr = -200-i
           goto 990
        endif
      enddo
      call fs_get_ifd_set(ifd_set)
      ifd_set_save=ifd_set
      if((kdef(1).or.kdef(2)).and.ifd_set.ne.1) then
         call get_att(iat1,iat2,ip)
         if(ip(3).lt.0) then
            ierr=-300
            goto 990
         endif
         if(kdef(1)) iat(1)=iat1
         if(kdef(2)) iat(2)=iat2
      else
         if(kdef(1)) then
            call fs_get_iat1if(iat1if)
            iat(1) = iat1if
         endif
         if(kdef(2)) then
            call fs_get_iat2if(iat2if)
            iat(2) = iat2if
         endif
      endif
C
C     2.2 INPUT1 AND INPUT2 - PARAMETERS 3 AND 4
C 
      do i=1,2
        ic1 = ich 
        call gtprm2(ibuf,ich,nchar,0,parm,ierr) 
        if (ierr.eq.2) then
          inp(i) = 0
        else if (ierr.eq.1) then
          if (i.eq.1) inp(i) = inp1if
          if (i.eq.2) inp(i) = inp2if
        else
          call iifed(1,inp(i),ibuf,ic1,ich-2)
          if (inp(i).lt.0) then
            ierr = -202-i
            goto 990
          endif
        endif
      enddo
C
C     3. Finally, format the buffer for the controller.
C     We have a valid IAT(1),IAT(2),INP(1),INP(2).
C
      ibuf(1) = 0
      call char2hol('if',ibuf(2),1,2)
      call if2ma(ibuf(3),iat(1),iat(2),inp(1),inp(2))
C
C     4. Now plant these values into COMMON.
C     Next send the buffer to SAM.
C     Finally schedule BOSS to request that MATCN gets the data.
C
      call fs_get_icheck(icheck(16),16)
      ichold = icheck(16)
      icheck(16)=0
      call fs_set_icheck(icheck(16),16)
      iat1if = iat(1)
      call fs_set_iat1if(iat1if)
      iat2if = iat(2)
      call fs_set_iat2if(iat2if)
      inp1if = inp(1)
      call fs_set_inp1if(inp1if)
      inp2if = inp(2)
      call fs_set_inp2if(inp2if)
      iol1if_fs = iold(1)
      iol2if_fs = iold(2)
      ifd_set=1
      call fs_set_ifd_set(ifd_set)
C
      iclass=0
      nch = 12
      call put_buf(iclass,ibuf,-nch,'fs','  ')
C
      nrec = 1
      goto 800
C 
C 
C     5.  This is the read device section.
C     Fill up two class buffers, one requesting % data (mode -2), 
C     the other ! (mode -1).
C 
500   call char2hol('if',ibuf(2),1,2)
      iclass = 0
      do i=1,2
        ibuf(1) = -i
        call put_buf(iclass,ibuf,-4,'fs','  ')
      enddo
C 
      nrec = 2
      goto 800
C 
C 
C 
C     6. This is the test/reset device section. 
C 
600   ibuf(1) = 6 
      call char2hol('if',ibuf(2),1,2)
      iclass=0
      call put_buf(iclass,ibuf,-4,'fs','  ')
      nrec = 1
      goto 800
C 
C 
C     7. This is the alarm query and reset request. 
C 
700   ibuf(1) = 7 
      call char2hol('if',ibuf(2),1,2)
      iclass=0
      call put_buf(iclass,ibuf,-4,'fs','  ')
      nrec = 1
      goto 800
C
C
C     8. All MATCN requests are scheduled here, and then IFDIS called.
C
800   call run_matcn(iclass,nrec)
      call rmpar(ip)
      if (ichold.ne.-99) then
        icheck(16) = ichold
        call fs_set_icheck(icheck(16),16)
      endif
      if (ichold.ge.0) then
        icheck(16)=mod(ichold,1000)+1
        call fs_set_icheck(icheck(16),16)
      endif
      call ifdis(ip,iclcm,kfirst)
      if(kfirst.and.ieq.ne.0.and.(kdef(1).or.kdef(2))) then
         kfirst=.false.
         if(ip(1).ne.0) call clrcl(ip(1))
         if(ifd_set_save.ne.1) then
            call logit7ci(0,0,0,0,-301,'qi',0)
         else
            call logit7ci(0,0,0,0,-302,'qi',0)
         endif
         goto 500
      endif
      return
C
990   ip(1) = 0
      ip(2) = 0
      ip(3) = ierr
      call char2hol('qi',ip(4),1,2)
      return
      end
