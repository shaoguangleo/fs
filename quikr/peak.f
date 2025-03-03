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
      subroutine peak(ip,itask)
C
C  Peak up on tape drive read resposne
C
      include '../include/fscom.i'
      include '../include/boz.i'
C
      integer ip(5),ireg(2),iparm(2)
      integer*2 ibuf(50)
      integer get_buf,ichcm_ch
      real*4 mper
      equivalence (reg,ireg(1)),(parm,iparm(1))
      character cjchar
      data ilen/100/
C
      if( itask.eq.4) then
         indxtp=1
      else
         indxtp=2
      endif
C
      ichold=-99
      iclass=0
      nrec=0
C
C  1.  Get the command
C
      iclcm=ip(1)
      if(iclcm.eq.0) then
        ip(3)=-1
        goto 990
      endif
      call ifill_ch(ibuf,1,ilen,' ')
      ireg(2) = get_buf(iclcm,ibuf,-ilen,idum,idum)
      nchar=min0(ilen,ireg(2))
      ieq=iscn_ch(ibuf,1,nchar,'=')
      if(ieq.eq.0) then
        goto 500
      else if(cjchar(ibuf,ieq+1).eq.'?') then
        ip(4)=ocp77
        goto 600
      endif
      ich=ieq+1
C
C  2. Get parameters.
C
C   Number of samples
C
      call gtprm(ibuf,ich,nchar,1,parm,ierr)
      if(cjchar(parm,1).eq.',') then
        nsamp=3
      else if(cjchar(parm,1).eq.'*') then
        nsamp=nsamppk_fs(indxtp)
      else if(ierr.eq.0) then
        nsamp=iparm(1)
      else
        ip(3)=-241
        goto 990
      endif
C
C   Number of iterations
C
      call gtprm(ibuf,ich,nchar,1,parm,ierr)
      if(cjchar(parm,1).eq.',') then
        iter=1
      else if(cjchar(parm,1).eq.'*') then
        iter=iterpk_fs(indxtp)
      else if(ierr.eq.0) then
        iter=iparm(1)
      else
        ip(3)=-242
        goto 990
      endif
C
C   Head to move
C
      call gtprm(ibuf,ich,nchar,0,parm,ierr)
      call fs_get_drive(drive)
      call fs_get_drive_type(drive_type)
      if((ichcm_ch(parm,1,'r').eq.0..or.ichcm_ch(parm,1,'2').eq.0)
     &   .and.(
     $     (VLBA.eq.drive(indxtp).and.VLBAB.eq.drive_type(indxtp))
     $     .or.VLBA4.eq.drive(indxtp).or.MK3.eq.drive(indxtp).or.
     &     (MK4.eq.drive(indxtp).and.MK4B.ne.drive_type(indxtp))
     $     )
     &     ) then
        ihd = 2
      else if(ichcm_ch(parm,1,'r').eq.0.or.
     &        ichcm_ch(parm,1,'2').eq.0) then
        ip(3)=-501
        goto 990
      else if(ichcm_ch(parm,1,'w').eq.0.or.
     &        ichcm_ch(parm,1,'1').eq.0) then
        ihd = 1
      else if(cjchar(parm,1).eq.'*') then
        ihd=ihdpk_fs(indxtp)
      else if (cjchar(parm,1).eq.','
     &       .and.(
     $       VLBA4.eq.drive(indxtp).or.MK3.eq.drive(indxtp).or.
     &       (MK4.eq.drive(indxtp).and.MK4B.ne.drive_type(indxtp))
     $       )
     $       ) then
        ihd = 2
      else if (cjchar(parm,1).eq.',') then
        ihd = 1
      else
        ip(3) = -243
        goto 990
      endif
C
C   Minimum peak voltage
C
      call gtprm(ibuf,ich,nchar,2,parm,ierr)
      if(cjchar(parm,1).eq.',') then
        vmin=.2
      else if(cjchar(parm,1).eq.'*') then
        vmin=vminpk_fs(indxtp)
      else if(ierr.eq.0) then
        vmin=parm
      else
        ip(3)=-244
        goto 990
      endif
C
C  3. Plant values in COMMON
C
300   continue
      ihdpk_fs(indxtp)=ihd
      iterpk_fs(indxtp)=iter
      nsamppk_fs(indxtp)=nsamp
      vminpk_fs(indxtp)=vmin
      goto 990
C
C  5.  Find peak
C
500   continue
      kpeakv_fs(indxtp)=.false.
      call fs_get_ispeed(ispeed,indxtp)
      call fs_get_cips(cips,indxtp)
      call fs_get_idirtp(idirtp,indxtp)
      call fs_get_ienatp(ienatp,indxtp)
      if(ispeed(indxtp).eq.0.or.(ispeed(indxtp).eq.-3.and.
     $     cips(indxtp).eq.0)) then
C       !tape must be moving
        ip(3)=-341
        goto 990
      else if(idirtp(indxtp).ne.1.and.ienatp(indxtp).ne.0) then ! not rec in rev
        ip(3)= -342
        goto 990
      else if(ihdpk_fs(indxtp).eq.0) then ! command must be set-up
        ip(3)=-343
        goto 990
      endif
C
      call fs_get_icheck(icheck(20+indxtp-1),20+indxtp-1)
      ichold=icheck(20+indxtp-1)
      icheck(20+indxtp-1) = 0
      call fs_set_icheck(icheck(20+indxtp-1),20+indxtp-1)
C
      call lvdonn('lock',ip,indxtp)
      if(ip(3).ne.0) goto 800
C
      call pkhd(ihdpk_fs(indxtp),iterpk_fs(indxtp),nsamppk_fs(indxtp),
     $     rpdt_fs(indxtp),vltpk_fs(indxtp),peakv,mper,ip,khecho_fs,
     $     lu,kpeakv_fs(indxtp),vminpk_fs(indxtp),indxtp)
      if(ip(3).ne.0) goto 800
C
      call lvdofn('unlock',ip,indxtp)
      if(ip(3).ne.0) goto 800
C
C  6.  Set up response
C
600   continue
      nch=ieq
      if(ieq.eq.0) nch=nchar+1
      nch=ichmv_ch(ibuf,nch,'/')
C
      nch=nch+ib2as(nsamppk_fs(indxtp),ibuf,nch,ocp100003)
      nch=mcoma(ibuf,nch)
C
      nch=nch+ib2as(iterpk_fs(indxtp),ibuf,nch,ocp100003)
      nch=mcoma(ibuf,nch)
C
      if(ihdpk_fs(indxtp).eq.1) then
        nch=ichmv_ch(ibuf,nch,'1')
      else if(ihdpk_fs(indxtp).eq.2) then
        nch=ichmv_ch(ibuf,nch,'2')
      endif
      nch=mcoma(ibuf,nch)
C
      nch=nch+ir2as(vminpk_fs(indxtp),ibuf,nch,8,3)
      nch=mcoma(ibuf,nch)
C
      if(ieq.eq.0) nch=nch+ir2as(peakv,ibuf,nch,8,3)
      nch=mcoma(ibuf,nch)
C
      if(ieq.eq.0) nch=nch+ir2as(mper,ibuf,nch,8,1)
      nch=mcoma(ibuf,nch)
C
      if(kpeakv_fs(indxtp)) then
        nch=ichmv_ch(ibuf,nch,'t')
      else
        nch=ichmv_ch(ibuf,nch,'f')
      endif
      nch=mcoma(ibuf,nch)
C
      call fs_get_drive(drive)
      call fs_get_drive_type(drive_type)
      if((drive(indxtp).eq.VLBA.and.drive_type(indxtp).eq.VLBA2).or.
     &     (drive(indxtp).eq.VLBA4.and.drive_type(indxtp).eq.VLBA42)
     &     )then
         nch=nch+ir2as(vltpk_fs(indxtp),ibuf,nch,8,1)
      else
         nch=nch+ir2as(vltpk_fs(indxtp),ibuf,nch,8,3)
      endif
C
      nch=nch-1
      call put_buf(iclass,ibuf,-nch,'fs','  ')
      nrec=1
      goto 990
C
800   continue
      if(ip(2).ne.0) call clrcl(ip(1))
      ip(2)=0
      call logit7(0,0,0,0,ip(3),ip(4),ip(5))
      call lvdofn('unlock',ip,indxtp)
C
C  That's all
C
990   continue
      call char2hol('q@',ip(4),1,2)
      ip(1)=iclass
      ip(2)=nrec
      if(ichold.ne.-99) then
        icheck(20+indxtp-1)=ichold
        call fs_set_icheck(icheck(20+indxtp-1),20+indxtp-1)
      endif
      return
      end
