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
      subroutine rdoff(ip)
C 
C     Set and display ra/dec offsets
C 
      include '../include/fscom.i'
      include '../include/boz.i'
C 
      dimension ip(1) 
      dimension ireg(2),iparm(2)
      integer get_buf
      integer*2 ibuf(20),lds,lhs
c     integer*4 is
      character cjchar
C 
      equivalence (ireg(1),reg),(iparm(1),parm) 
C 
      data ilen/40/ 
C 
      iclcm = ip(1) 
      do i=1,3
        ip(i) = 0
      enddo
      call char2hol('qo',ip(4),1,2)
      if (iclcm.eq.0) then
        ip(3) = -1
        return
      endif
      ireg(2) = get_buf(iclcm,ibuf,-ilen,idum,idum)
      nchar = ireg(2) 
      ieq = iscn_ch(ibuf,1,nchar,'=') 
      if (ieq.eq.0) goto 500
C 
C     2. Parse the command: RADECOFF=<raoffset>,<decoffset> 
C 
C     2.1 First get the RA offset and convert to radians
C 
      ich = ieq+1
      ic1 = ich 
      call gtprm(ibuf,ich,nchar,0,parm,ierr) 
      if (cjchar(parm,1).eq.'*') then
        call fs_get_raoff(raoff)
        ra = raoff                 !  pick up the ra from common
      else if (cjchar(parm,1).eq.',') then
        ip(3) = -101            !  there is no default for the ra offset
        return
      else
        call gtrad(ibuf,ic1,ich-2,3,ra,ierr)
        if (ierr.lt.0) then
          ip(3) = -201
          return
        endif
      endif
C 
C     2.2 Next the DEC offset.
C 
      ic1 = ich
      call gtprm(ibuf,ich,nchar,0,parm,ierr) 
      if (cjchar(parm,1).eq.'*') then
        call fs_get_decoff(decoff)
        dec = decoff               !  pick up the dec offset from common
      else if (cjchar(parm,1).eq.',') then
        ip(3) = -102             !  there is no default for the dec offset
        return
      else
        call gtrad(ibuf,ic1,ich-2,2,dec,ierr)
        if (ierr.lt.0) then
          ip(3) = -202
          return
        endif
      endif
C 
C     3. Plant the variables in COMMON. 
C     ***NOTE*** WE LEAVE AZ EL AND XLY OFFSETS AS THEY ARE
C 
      raoff = ra
      call fs_set_raoff(raoff)
      decoff = dec
      call fs_set_decoff(decoff)
      ierr = 0
C
C     4. Now schedule ANTCN.  Tell it to do source offsets.
C
      call fs_get_idevant(idevant)
      if (ichcm_ch(idevant,1,'/dev/null ').ne.0) then
        call run_prog('antcn','wait',2,idum,idum,idum,idum)
        call rmpar(ip)
      else
       ip(3)=-302
      endif
      return
C
C     5. Return the offsets for display
C
500   nch = ichmv_ch(ibuf,nchar+1,'/')
      call fs_get_decoff(decoff)
      call fs_get_raoff(raoff)
      call radec(dble(abs(raoff)),dble(decoff),0.0,irah,iram,ras,
     .     lds,idcd,idcm,dcs,lhs,i,i,d)
C     is=ras*1000
C     ras=is/1000.
C     nch = nch + ir2as(irah*10000.0+iram*100.0+ras,ibuf,nch,10,3)
      if(raoff.lt.0.0) nch=ichmv_ch(ibuf,nch,'-')
      nch = nch + ib2as(irah,ibuf,nch,ocp40000+ocp400*2+2)
      nch = nch + ib2as(iram,ibuf,nch,ocp40000+ocp400*2+2)
C     iras = ifix(ras)
C     nch = nch + ib2as(iras,ibuf,nch,ocp40000+ocp400*2+2)
C     ras = ras-iras
      if (ras.lt.10.0) nch=ichmv_ch(ibuf,nch,'0')
      nch = nch + ir2as(ras,ibuf,nch,4,1)
      nch = ichmv_ch(ibuf,nch,',')
      if(ichcm_ch(lds,1,'-').eq.0) nch = ichmv(ibuf,nch,lds,1,1)
C     is=dcs*100
C     dcs=is/100.
C     nch = nch + ir2as(idcd*10000.0+idcm*100.0+dcs,ibuf,nch,9,2)
      nch = nch + ib2as(idcd,ibuf,nch,ocp40000+ocp400*2+2)
      nch = nch + ib2as(idcm,ibuf,nch,ocp40000+ocp400*2+2)
C     idcs = ifix(dcs)
C     nch = nch + ib2as(idcs,ibuf,nch,ocp40000+ocp400*2+2)
C     dcs = dcs-idcs
      if (dcs.lt.10.0) nch=ichmv_ch(ibuf,nch,'0')
      nch = nch + ir2as(dcs,ibuf,nch,4,1)
C
      iclass = 0
      nch = nch - 1
      call put_buf(iclass,ibuf,-nch,'fs','  ')
      ip(1) = iclass
      ip(2) = 1 
      ip(3) = 0 
      call char2hol('qo',ip(4),1,2)
      return
      end 
