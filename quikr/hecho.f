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
      subroutine hecho(ip)
C
C  SET  HEAD ECHO CONTROL
C
      include '../include/fscom.i'
      include '../include/boz.i'
C
      integer ip(5),ireg(2),iparm(2)
      integer*2 ibuf(50)
      integer get_buf,ichcm_ch
      logical kec
      character cjchar
      equivalence (reg,ireg(1)),(parm,iparm(1))
      data ilen/100/
C
C  1.  Get the command
C
      iclass=0
      nrec=0
      iclcm=ip(1)
      if(iclcm.eq.0) then
        ip(3)=-1
        goto 990
      endif
      ireg(2) = get_buf(iclcm,ibuf,-ilen,idum,idum)
      nchar=min0(ilen,ireg(2))
      ieq=iscn_ch(ibuf,1,nchar,'=')
      if(ieq.eq.0) then
        goto 600
      else if(cjchar(ibuf,ieq+1).eq.'?') then
        ip(4)=ocp77
        goto 600
      endif
C
C  2. Get parameters.
C
      ich=ieq+1
      call gtprm(ibuf,ich,nchar,0,parm,ierr)
      if(ichcm_ch(parm,1,'on').eq.0) then
        kec=.true.
      else if(ichcm_ch(parm,1,'off').eq.0) then
        kec=.false.
      else
        ip(3) = -291
        goto 990
      endif
C
C  3. Plant values in COMMON
C
300   continue
      khecho_fs=kec
      goto 990
C
C  5.  Display values
C
600   continue
      nch=ieq
      if(ieq.eq.0) nch=nchar+1
      nch=ichmv_ch(ibuf,nch,'/')
      if(khecho_fs) then
        nch=ichmv_ch(ibuf,nch,'on')
      else
        nch=ichmv_ch(ibuf,nch,'off')
      endif
C
      nch=nch-1
      call put_buf(iclass,ibuf,-nch,'fs','  ')
      nrec=1
C
C  That's all
C
990   continue
      call char2hol('q@',ip(4),1,2)
      ip(1)=iclass
      ip(2)=nrec
      return
      end
