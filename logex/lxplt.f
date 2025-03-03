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
      subroutine lxplt
c     LOGEX PLOTTING ROUTINE !<870115:05:35>
C
C LXPLT - LOGEX plotting routine
C
C MODIFICATIONS:
C
C    DATE     WHO  DESCRIPTION
C    820525   CAK  SUBROUTINE CREATED
C    820526   CAK  SCRATCH FILE FOR PLOT ADDED
C    820607   CAK  LXPLT HAS BEEN CHANGED FROM A SUBROUTINE TO A SEGMENT
C                  PROGRAM OF LOGEX.
C    820923   KNM  THE STRIP-CHART PLOT AND THE SINGLE SCREEN PLOT ARE
C                  HANDLED IN THIS PROGRAM SEGMENT. LXTPL IS NOW
C                  OBSOLETE.
c    871130   LEF  Changed back to subroutine and added CDS.
C
C COMMON BLOCKS USED:
C
      include 'lxcom.i'
C
C SUBROUTINE INTERFACES:
C    CALLING SUBROUTINES:
C     LNFCH Utilities
C
C LOCAL VARIABLES:
C
      include '../include/boz.i'
C
      character*79 outbuf
      integer answer, trimlen, ichcm_ch
      integer idcb1(2), idcb2(2)
      integer idcb3(2),it(6)
      integer*4 secs
      integer*2 iqc, isize
      dimension scale(5)
C        - Scale values for each parameter
      integer*2 iplch(5)
C        - Plotting character of each parameter.
      logical kpass2
C        - A SECOND PASS IS NEEDED TO HANDLE DELTAS
      dimension y2min(5),y2max(5)
C        - TEMPORARY ARRAYS TO SUPPORT A SECOND PASS
      dimension iparm(2),ival(2)
C
      integer*2 line(65)
      character*130 cline
      integer*2 pline(65,100)
      equivalence (line(1),cline)
C        - Buffer for plot.
C
      integer fmpreadstr, fmpsetpos
      integer fmpwritexx, fmpreadxx, fmpappend
      integer fmpsetline, writestr, iflch
      integer irec
      character*64 FILENAME1, FILENAME2
      character*4 ihc
      integer*2 ihas(2)
      equivalence (ihc,ihas)
      character*11 cmax,cmin
      integer*2 imin(6),imax(6)
C        - Contains the min,max scale values in double precision.
C          This is done because there are no routines to convert
C          double precision to ASCII.
C
      save oldformat
      equivalence(ltitle(24),imin(1)),(ltitle(30),imax(1))
      equivalence (cmax,imax),(cmin,imin)
      equivalence (parm,iparm(1)),(value,ival(1))
C
      dimension ymin(5),ymax(5),iy(5)
C        - Y-Axis minimum, maximum, plotting locations for the data
C          points.
C
C
      integer*2 ltitle(40)
      dimension yy(5)
      integer*2 irec1(14)
C          Parm value to plot.
      double precision xx,xmin,xmax,xa
      equivalence (xx,irec1(1)),(yy(1),irec1(5))
C        - Log time in terms of days (& fractions of days).
C          X-Axis minimum, maximum. Log day. Log minutes.
C
      data FILENAME1/'/tmp/loge1'/
      data FILENAME2/'/tmp/loge2'/
C
C
c      data ltitle/78,'                      [            ( )] sc (',
c     .'           ,            ),  ch < >'/
      data ltitle/78,2H  ,2H  ,2H  ,2H  ,2H  ,2H  ,2H  ,2H  ,2H  ,
     .2H  ,2H  ,2H[ ,2H  ,2H  ,2H  ,2H  ,2H  ,2H (,2H ),2H] ,2Hsc,
     .2H (,2H  ,2H  ,2H  ,2H  ,2H  ,2H ,,2H  ,2H  ,2H  ,2H  ,2H  ,
     .2H  ,2H),,2H  ,2Hch,2H <,2H >/
C
C INITIALIZED VARIABLES:
C
      data iplch/2h1 ,2h2 ,2h3 ,2h4 ,2h5 /
C       - Plot characters
C
      data scale/1.149253,4*0.0/
      data oldformat/-1/
C
C
C **************************************************************
C
C 1. Create and open scratch files.
C
C **************************************************************
C
C
      nc = ib2as(ihgt,ihas,1,ocp100004)
      call fmpopen(idcb1,FILENAME1,ierr,'w+',5)
      if (ierr.lt.0) goto 1100
      call fmpopen(idcb2,FILENAME2,ierr,'w+',5)
      if (ierr.lt.0) goto 1100
C
C
C **************************************************************
C
C 2. Make sure a PARM and a COMMAND command has been specified.
C    If that's ok, initialize variables & arrays.
C
C **************************************************************
C
C
      if (ncmd.eq.0.or.nump.eq.0) goto 1200
      imagw=(iwidth+1)/2
      nplot=0
      nlout=0
      iout=0
      call ifill_ch(line,1,130,' ')
      xmin=1.d20
      xmax=-1.d20
      do i=1,nump
        ymin(i)=1.d20
        ymax(i)=-1.d20
      end do
C
C
C **************************************************************
C
C 3. Call LXGET to read log entries.
C
C **************************************************************
C
C
      lstend=0
      ilen=0
      call lxget
      do while ((lstend.ne.-1).and.(ilen.ge.0))  !!!READ LOG ENTRIES
        xx=0.d0
        if (icode.eq.-1) goto 1200 !!!IF BREAK, PURGE SCRATCH FILES & RETURN.
C
C Count the number of entries & lines outputted.
C
        nplot=nplot+1
        nlout=nlout+1
C
C Pick up the decoded time from common and check XX for min & max
C time scale.
C
        it(6)=itl1/1024+1970
        it(5)=mod(itl1,1024)
        it(4)=itl2/60
        it(3)=mod(itl2,60)
        it(2)=itl3/100
        it(1)=mod(itl3,100)
        call fc_rte2secs(it,secs)
        if(secs.lt.0) then
           call po_put_c(' time conversion error 1 - plot deleted.')
           goto 1200
        endif
        xx=secs*100.0d0+it(1)
        if (xx.lt.xmin) xmin=xx
        if (xx.gt.xmax) xmax=xx
C
C Get specified parm store the value
C
        if(logformat.ne.oldformat) then
           if(logformat.eq.1) then
              idyps=1
              ihrps=4
              imnps=6
              iscps=8
              ildch=10
              ifrst=11
           else if(logformat.eq.2) then
              idyps=3
              ihrps=6
              imnps=8
              iscps=10
              ildch=14
              ifrst=15
           else if(logformat.eq.3) then
C     not Y10K compliant
              idyps=6
C     not Y10K compliant
              ihrps=10
C     not Y10K compliant
              imnps=13
C     not Y10K compliant
              iscps=16
C     not Y10K compliant
              ildch=21
C     not Y10K compliant
              ifrst=22
           endif
           oldformat=logformat
        endif
        do 440 n=1,nump
C
C  Skip over the time field plus the number of characters in NCOMND
C  to begin the first character of the PARM at ICH.
C
          ich = ifrst+ncomnd(1)
          if (nparm(n).eq.1) goto 410
C
C  If more than one PARM is specified, the following DO loop will
C  move ICH to the first character of that particular PARM.
C
          do i=1,nparm(n)-1
            ich = 1 + iscn_ch(ibuf,ich,nchar,',')
          end do
410       call gtprm(ibuf,ich,nchar,2,parm,ierr)
          if (ierr.ne.0) goto 440
          value = parm
C
C  Determine if the logarithm scale is to be used
C
          if (ichcm_ch(llogx(n),2,'db').eq.0) then
            if (value.le.0) value=1.0
            value=10.0*log10(value)
          end if
C
C Store the value for later plotting & check Y scale min & max.
C
        yy(n)=value
        if (yy(n).lt.ymin(n)) ymin(n)=yy(n)
        if (yy(n).gt.ymax(n)) ymax(n)=yy(n)
440     continue
C
C Write scratch record.
C
        id = fmpwritexx(idcb1, ierr, xx)
        if (ierr.lt.0) goto 1100
        call lxget
      enddo  !!!READ LOG ENTRIES
C
C
C ************************************************************
C
C 6. Check for a sufficient number of points to plot.
C
C ************************************************************
C
C
600   continue
      ierr=0
      call fmprewind(idcb1,ierr)
      if (ierr.lt.0) goto 1100
      if (nplot.lt.1) then
        call po_put_c(' no plot points - plot deleted.')
        goto 1200
      end if
C
C
C **************************************************************
C
C 7. Check for auto-scaling
C
C **************************************************************
C
C
      it(6)=its1/1024+1970
      it(5)=mod(its1,1024)
      it(4)=its2/60
      it(3)=mod(its2,60)
      it(2)=its3/100
      it(1)=mod(its3,100)
      call fc_rte2secs(it,secs)
        if(secs.lt.0) then
           call po_put_c(' time conversion error 2 - plot deleted.')
           goto 1200
        endif
      xa=secs*100.0d0+it(1)
      if (its1.ne.1) xmin=xa
c
      it(6)=ite1/1024+1970
      it(5)=mod(ite1,1024)
      it(4)=ite2/60
      it(3)=mod(ite2,60)
      it(2)=ite3/100
      it(1)=mod(ite3,100)
      call fc_rte2secs(it,secs)
        if(secs.lt.0) then
           call po_put_c(' time conversion error 3 - plot deleted.')
           goto 1200
        endif
      xa=secs*100.0d0+it(1)
      if (ite1.ne.(2038-1970)*1024+1) xmax=xa
      kpass2=.false.
      do i=1,nump
        kpass2=kpass2.or.sdelta(i).ne.0.0
      enddo
      if(kpass2) then
        do i=1,nump
          y2min(i)=ymin(i)
          y2max(i)=ymax(i)
          ymin(i)=1e20
          ymax(i)=-1e20
        enddo
        ierr=0
        call fmprewind(idcb1,ierr)
        if (ierr.lt.0) goto 1100
        do i=1,nplot
          id = fmpreadxx(idcb1,ierr,xx)
          do n=1,nump
            if(sdelta(n).lt.0..and.yy(n).lt.y2max(n)+sdelta(n)) then
              yy(n)=y2max(n)
            else if(sdelta(n).gt.0..and.yy(n).gt.y2min(n)+sdelta(n))then
              yy(n)=y2min(n)
            endif
            if(yy(n).lt.ymin(n)) ymin(n)=yy(n)
            if(yy(n).gt.ymax(n)) ymax(n)=yy(n)
          enddo
        enddo
        ierr=0
        call fmprewind(idcb1,ierr)
        if (ierr.lt.0) goto 1100
      endif
      do 710 i=1,nump
        if(smax(i).ne.smin(i)) goto 710
        smax(i)=ymax(i)
        smin(i)=ymin(i)
710   continue
      if (ikey.eq.6) isize=iwidth-21
      if (ikey.eq.13) isize=ihgt-1
      scalex=(xmax-xmin)/float(iwidth-1)
      do i=1,nump
        scale(i)=(smax(i)-smin(i))/isize
      end do
C
C
C **************************************************************
C
C 8. Write out COMMAND and PARM specifications as a PLOT header
C
C **************************************************************
C
C
C Write up to 5 plot titles giving Y-information.
C Write one line giving X-information.
C
      do i=1,nump
        call ifill_ch(ltitle(2),1,22,' ')
        jlen=iflch(logna,20)
        call ichmv(ltitle(2),1,logna,1,jlen)
        idum= mcoma(ltitle,jlen+3)
        call ichmv(ltitle(2),jlen+4,lstatn,1,8)
        call ichmv(ltitle(2),24,lcomnd,1,12)
        id = ib2as(nparm(i),ltitle(2),37,1)
        rlg10=0.0
        if (smin(i).ne.0.0) rlg10=log10(abs(smin(i)))
C
        if (rlg10.ge.0.0) then
          ilg10=rlg10+1
          iprec=max(6-ilg10,0)
        else
          ilg10=rlg10-1
          im=9
          if(smin(i).lt.0) im=8
          iprec=min(5-ilg10,im)
        endif
C
        call ifill_ch(imin,1,11,' ' )
        call jr2as(smin(i),imin,1,11,iprec)
        rlg10=0.0
        if (smax(i).ne.0.0) rlg10=log10(abs(smax(i)))
C
        if (rlg10.ge.0.0) then
          ilg10=rlg10+1
          iprec=max(6-ilg10,0)
        else
          ilg10=rlg10-1
          im=9
          if(smin(i).lt.0) im=8
          iprec=min(5-ilg10,im)
        endif
C
        call ifill_ch(imax,1,11,' ')
        call jr2as(smax(i),imax,1,11,iprec)
        call ichmv(ltitle(2),77,iplch(i),1,1)
        nchar = iflch(ltitle(2),78)
        call lxwrt(ltitle(2),nchar)
        if(icode.eq.-1) goto 1200
      enddo
C
      call char2hol(' ',l6,1,1)
      if (ikey.eq.13) goto 900
C
C Write a blank line, then a line of dashes for the strip-chart.
C
      call lxwrt(line,iwidth)
      call ifill_ch(line,22,iwidth-22,'--')
      call lxwrt(line,iwidth)
      goto 1000
C
C
C **************************************************************
C
C 9. Reformat X-limits to DDD-HHMM.  Make the plot borders.
C
C **************************************************************
C
C
900   call ifill_ch(line,1,130,' ')
      nch=1
      call lxhms(xmin,line,nch)
      nch=iwidth-19
      call lxhms(xmax,line,nch)
      call lxwrt(line,nch)
      if(icode.eq.-1) goto 1200
C
C Clear page image
C Write <|> and <-> on plot borders
C
      call ifill_ch(line,1,imagw*2,' ')
      call char2hol('| ',line,1,2)
      call char2hol(' |',line(imagw),1,2)
C
      irec = 1
      id = fmpsetpos(idcb2,ierr,irec,-irec)
      id = writestr(idcb2, ierr, cline,imagw*2)
      call ichmv(pline(1,1),1,line(1),1,imagw*2)
      do i=2,ihgt-1
        call ichmv(pline(1,i),1,line(1),1,imagw*2)
        id = writestr(idcb2, ierr, cline,imagw*2)
        if (ierr.lt.0) goto 1100
      enddo
C
      call ifill_ch(line,1,imagw*2,'-')
      irec = 1
      id = fmpsetpos(idcb2,ierr,irec,-irec)
      id = writestr(idcb2, ierr, cline,imagw*2)
      call ichmv(pline(1,1),1,line(1),1,imagw*2)
      if (ierr.lt.0) goto 1100
      id = fmpappend(idcb2,ierr)
      id = writestr(idcb2, ierr, cline,imagw*2)
      call ichmv(pline(1,ihgt),1,line(1),1,imagw*2)
      if (ierr.lt.0) goto 1100
C
C
C **************************************************************
C
C 10. Plot the points for the plot depending on the type
C     specified.
C
C **************************************************************
C
C
1000  call ifill_ch(line,1,130,' ')
      do 1070 i=1,nplot
       id = fmpreadxx(idcb1,ierr,xx)
cxx        if(ifbrk(idum).lt.0) icode=-1
        if(icode.eq.-1) goto 1200
        if (ierr.lt.0) goto 1100
C
C Determine the X-position for the single screen plot
C
        ix=(xx-xmin)/scalex+1.
        if (ix.lt.1) ix=1
        if (ix.gt.iwidth) ix=iwidth
C
C Determine the Y-positions for both plots.
C
        do 1010 j=1,nump
          iy(j)=(yy(j)-smin(j))/scale(j)+1.
1010    continue
C
C Make and write the strip chart plot.
C
        if(ikey.ne.6) goto 1030
        nch=1
        call lxhms(xx,line,nch)
        call ichmv_ch(line,21,'|')
        call ifill_ch(line,22,iwidth-22,' ')
        call ichmv_ch(line,iwidth,'|')
C
        do 1020 k=1,nump
          if(iy(k).lt.1) iy(k)=1
          if(iy(k).gt.isize) iy(k)=isize
          iqc=jchar(line,iy(k)+21)
           if ((ichcm_ch(iqc,1,' ').ne.0).and.
     .       (ichcm_ch(iqc,1,'|').ne.0)) goto 1015
          call ichmv(line,iy(k)+21,iplch(k),1,1)
          goto 1020
1015      call ichmv_ch(line,iy(k)+21,'=')
1020    continue
C
        call lxwrt(line,iwidth)
        if(icode.eq.-1) goto 1200
        goto 1070
C
C The points for the single screen plot are written to the second
C scratch file here.
C
1030    do n=1,nump
          if (iy(n).lt.1) iy(n)=1
          if (iy(n).gt.ihgt) iy(n)=ihgt
          irec = ihgt + 1 - iy(n)
          id = fmpsetline(idcb2,ierr,irec-1)
          id = fmpreadstr(idcb2,ierr,cline)
          call ichmv(line(1),1,pline(1,irec),1,imagw*2)
          if (ierr.lt.0) goto 1100
          iqc=jchar(line(1),ix)
          if ((ichcm_ch(iqc,1,' ').eq.0).or.
     .       (ichcm_ch(iqc,1,'|').eq.0).or.
     .       (ichcm_ch(iqc,1,'-').eq.0)) then
            call ichmv(line(1),ix,iplch(n),1,1)
          else
            call ichmv_ch(line(1),ix,'=')
          endif
          irec = ihgt+1-iy(n)
          id = fmpsetline(idcb2,ierr,irec-1)
          call ichmv(pline(1,irec),1,line(1),1,imagw*2)
          if(ierr.lt.0) goto 1100
        enddo
1070  continue
C
      if (ikey.eq.13) goto 1080
      call ifill_ch(line,1,130,' ')
      call ifill_ch(line,22,iwidth-22,'-')
      call lxwrt(line,iwidth)
      goto 1200
C
C The Single-Screen plot scratch file is written out here
C
1080  do i=1,ihgt
        call ichmv(line(1),1,pline(1,i),1,imagw*2)
        len = iflch(line,imagw*2)
cxx        if(ifbrk(idum).lt.0) icode=-1
        if(icode.eq.-1) goto 1200
        if (ierr.lt.0) goto 1100
        call lxwrt(line,len)
        if(icode.eq.-1) goto 1200
      enddo
      goto 1200
C
C
C
C **************************************************************
C
C 11. Errors encountered while using the scratch files are
C     written here.
C
C **************************************************************
C
C
C
1100  continue
      outbuf=' error '
      nc = ib2as(ierr,answer,1,4)
      call hol2char(answer,1,4,outbuf(8:))
      nchar = trimlen(outbuf) + 1
      outbuf(nchar:)=' in scratch file - plot deleted.'
      call po_put_c(outbuf)
C
C
C **************************************************************
C
C 12. Purge the scratch files.
C
C **************************************************************
C
C
1200  continue
      call fmpclose(idcb1,ierr)
      call ftn_purge(FILENAME1,ierr)
      call fmpclose(idcb2,ierr)
      call fmpclose(idcb3,ierr)   !!!TEST!!!
      call ftn_purge(FILENAME2,ierr)
      ilxget=0
      if (nump.eq.0) then
        call po_put_c(' parm command must be issued in order to plot')
      end if
      if (ncmd.eq.0) then
        call po_put_c(' one command must be issued in order to p
     .lot')
      end if
      if (ncmd.eq.0.or.nump.eq.0) icode=-1
C
      return
      end
