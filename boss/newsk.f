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
      subroutine newsk(ibuf,ic1,nchar,idcbsk,iblen,ierr,
     .icurln,ilstln)
C
C  NEWSK handles the starting of a new schedule for BOSS
C
C  INPUT:
C
C     IBUF - input buffer with command and parameters
C     NCHAR - number of characters in IBUF (i.e. last char)
C     IC1 - first character of the field we are to process
C     IDCBSK - schedule file DCB, already opened OK
C     IBLEN - available length of IBUF
      integer*2 ibuf(1)
      integer idcbsk(1)
C
C  COMMON:
C
      include '../include/fscom.i'
C
C
C  LOCAL:
C
      integer*4 irec,ioff,irec2,id,icurln,ilstln
      integer fmpposition,fmpreadstr,fmpsetpos,fmpsetline
      integer ichcm_ch
      logical kpast,ktime
      integer it(6)
      integer*2 ib(256)
      character*512 ibc
      character cjchar
      equivalence (ib,ibc)
      data  ibl/256/
C
C  First check 1st line of schedule for experiment name and year
      ilog=0
      irecln = 1
      ilen = fmpreadstr(idcbsk,ierr,ibc)
      irecln = irecln + 1
      if(ilen.le.0) then
        ierr = -1
        goto 100
      endif
      if(cjchar(ib,1).ne.'"') goto 100
      nch = 2
C  If 1st line is a comment, get experiment name here
      call gtfld(ib,nch,ibl*2,icf,ic2)
      if(icf.eq.0) goto 99
      nc = min0(8,ic2-icf+1)
      lexper2=' '
      call ifill_ch(ilexper2,1,MAX_SKD,' ')
      idummy = ichmv(ilexper2,1,ib,icf,nc)
      call hol2char(ilexper2,1,MAX_SKD,lexper2)
      call fs_set_lexper2(ilexper2)
      call char2hol(lexper2,ilexper,1,8)
      call fs_set_lexper(ilexper)
C
C count all the leading comments in the schdeule
C
 99   continue
      ilog=ilog+1
      ilen = fmpreadstr(idcbsk,ierr,ibc)
      if(cjchar(ib,1).ne.'"') goto 100
      goto 99
C
100   idum = fmpsetpos(idcbsk,ierr,0,id)
      irecln = 1 
      ilstln=100000
      nlines=0
      ic2=iscn_ch(ibuf,ic1,nchar,',')+1
      if(ic2.eq.1) ic2=nchar+2
      if(ic2.gt.nchar) goto 105
      nlines=ias2b(ibuf,ic2,nchar-ic2+1)
      if(nlines.ge.0) goto 105
        call logit6c(0,0,0,0,-138,'bo')
        ierr=-1
        goto 900
C
C 1. Determine the type of parameter which was input with the command.
C    null = start with SOURCE before now+5min.
C    time = start with SOURCE before time.
C    #nnn = start with line nnn in file.
C
105   irec = -1
      if (nchar.lt.ic1.or.ic2.le.ic1+1) then
        call fc_rte_time(it,it(6))      ! Get current time ...
        call iadt(it,5,3)           ! ... and add 5 minutes to it
        goto 200
      endif
C                            #
      if (cjchar(ibuf,ic1).ne.'#') then
        call gttim(ibuf,ic1,ic2-2,0,it1,it2,it3,ierr)
        if (ierr.lt.0) then
          call logit6c(0,0,0,0,ierr,'sp')
          goto 900
        endif
        it(6) = it1/1024+1970
        it(5) = mod(it1,1024)
        it(4) = it2/60
        it(3) = mod(it2,60)
        it(2) = it3/100
        it(1) = mod(it3,100)
        goto 200
      endif
C
C 1.2 The case of #nnn - just position to the line and we're done.
C
      iline = ias2b(ibuf,ic1+1,ic2-ic1-2)
      ierr = 0
      if (iline.le.0) then
        call logit7ci(0,0,0,1,-106,'bo',iline)
        ierr = -1
        goto 900
      endif
      if(iline.eq.1.and.(nlines.gt.ilog.or.nlines.eq.0)) then
         iline=ilog+1
         if(nlines.gt.ilog) nlines=nlines-ilog
      endif
      if (iline.le.1) goto 800
      irec2 = iline
      idum = fmpsetline(idcbsk,ierr,irec2-1)
      irecln = irec2
cxx      idum = fmpsetpos(idcbsk,ierr,irec2,ioff)
C                     Space down to the requested line
      if (ierr.ge.0) goto 800
      call logit7ci(0,0,0,1,-134,'bo',ierr)
      goto 900
C
C
C     2. Search for a time in schedule file.
C     Desired time is in IT (HP format).
C
200   continue
      ilen = fmpreadstr(idcbsk,ierr,ibc)
      irecln = irecln + 1
      if (ilen.le.0) then
        ierr = -1
        call logit6c(0,0,0,0,-124,'bo')
        goto 900
      endif
      if (ichcm_ch(ib,1,'scan_name=').eq.0) then
        idum = fmpposition(idcbsk,ierr,irec,ioff)
        irec2=irec-ilen-1
        irec2ln = irecln-1
        ktime=.false.
C                     Remember the location of the last SOURCE command
        goto 200
      endif
      if (ktime) goto 200
      if (cjchar(ib,1).ne.'!') goto 200
C                          ! -- a wait-for command
      if (cjchar(ib,2).eq.'+') goto 200
C                          + -- relative wait, which we ignore
      iec = iflch(ib,ilen*2)
      call gttim(ib,2,iec,0,it1,it2,it3,ierr)
      ktime=.true.
      if(ierr.lt.0) goto 200
      if (kpast(it1,it2,it3,it)) goto 200
C                   If this time is in the past, go back and read some more
      if (irec.le.0) then
        call logit6c(0,0,0,0,-132,'bo')
        ierr = -1
        goto 900
      endif
cxx      irec2=irec-1
      idum = fmpsetpos(idcbsk,ierr,irec2,ioff)
C                     Re-position to the source command
      idum = fmpposition(idcbsk,ierr,irec2,id)
      iline = irec2ln
      irecln = irec2ln
      ierr = 0
C
C
800   idum = fmpposition(idcbsk,ierr,irec,ioff)
      icurln=irecln-1
      if(nlines.gt.0) ilstln = iline+nlines-1

C ** CLOSE DCB IF AN ERROR OCCURRED -- NRV 840709
900   if (ierr.lt.0) call fmpclose(idcbsk,inerr)

      return
      end

