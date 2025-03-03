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
      subroutine newpf(idcbp1,idcbp2,lproc1,maxpr1,nproc1,lproc2,
     .maxpr2,nproc2,ibuf,iblen,istkop,istksk)
C
C     NEWPF checks for a newly-edited procedure file from PFMED
C     and acts accordingly.
C
C     DATE   WHO CHANGES                   LAST EDIT <910328.1434>
C     810906 NRV CREATED
C     890509 MWH Modified to use CI files
C
C  COMMON BLOCKS:
C
      include '../include/fscom.i'
C
C  INPUT/OUTPUT:
C
C     IDCBP1,IDCBP2 - DCBs for the two procedure files
C     LPROC1,LPROC2 - directory arrays for the two files
C     MAXPR1,MAXPR2 - maximum number of entries in each file
C     NPROC1,NPROC2 - actual number of entries
C     IBUF - buffer to pass to OPNPF for use
C     IBLEN - length of IBUF in words
C     ISTKOP,ISTKSK - operator and schedule procedure stacks
      dimension idcbp1(1),idcbp2(1)
      integer*2 ibuf(1)
      integer*4 lproc1(4,1),lproc2(4,1)
      dimension istkop(1),istksk(1)
C
C
C  CALLING ROUTINES: BOSS
C  SUBROUTINES CALLED:  OPNPF, KSTAK, FMP
C
C  LOCAL:
C
C     KSTAK - function which is TRUE if there's some procedure in the stacks
      character*64 pathname,pathname2,link
      logical kstak
      integer trimlen, nch, rn_take
C
C  INITIALIZED:
C
C
C     1. Lock the resource number no matter what.
C
      ierr = 0
      irnprc = rn_take('pfmed',1)
      if (irnprc.eq.0) then
C
C     2. Get a new version of the schedule proc file.
C     First check that there are not any procedures from the schedule
C     library on the active stack list.
C     If not, then close the file, purge it, and rename the pending
C     edited file to the proper name.  Then call OPNPF to get the
C     directory.
C
        call fs_get_lnewsk2(ilnewsk2)
        if (ilnewsk2(1).eq.0) then
          lnewsk2=' '
        else
          call hol2char(ilnewsk2,1,MAX_SKD,lnewsk2)
        end if
        if ((lnewsk2.ne.' ').and.(.not.kstak(istkop,istksk,1))) then
          call fmpclose(idcbp1,ierr)
          call fs_get_lprc2(ilprc2)
          call hol2char(ilprc2,1,MAX_SKD,lprc2)
          nch = trimlen(lprc2)
          call follow_link(lprc2(:nch),link,ierr)
          if(ierr.lt.0) then
             call logit7ci(0,0,0,1,-500+ierr,'bo',ierr)
             goto 300
          endif
          if(link.eq.' ') then
             pathname = FS_ROOT//'/proc/' // lprc2(:nch) // '.prc'
          else
             pathname = FS_ROOT//'/proc/' // link(:trimlen(link))
          endif
          call ftn_purge(pathname,ierr)
C                     Purge the old version of the file
          call fs_get_lprc2(ilprc2)
          call hol2char(ilprc2,1,MAX_SKD,lprc2)
          nch = trimlen(lprc2)
          if(link.eq.' ') then
             pathname2= FS_ROOT//'/proc/' // lprc2(:nch) // '.prx'
          else
             iprc=index(link,".prc")
             link(iprc+3:iprc+3)='x'
             pathname2= FS_ROOT//'/proc/' // link(:trimlen(link))
          endif
          call ftn_rename(pathname2,ierr1,pathname,ierr2)
C                     Rename the edited version to the proper name
          if (ierr1.lt.0) then
            call logit7ci(0,0,0,1,-127,'bo',ierr1)
            ierr1 = 0
          else if (ierr2.lt.0) then
            call logit7ci(0,0,0,1,-127,'bo',ierr2)
            ierr2 = 0
          else
            call fs_get_lprc2(ilprc2)
            call hol2char(ilprc2,1,MAX_SKD,lprc2)
            call opnpf(lprc2,idcbp1,ibuf,iblen,lproc1,maxpr1,nproc1,
     &               ierr,'o')
            lnewsk2 = ' '
            call char2hol(lnewsk2,ilnewsk2,1,MAX_SKD)
            call fs_set_lnewsk2(ilnewsk2)
            call char2hol(lnewsk2,ilnewsk,1,8)
            call fs_set_lnewsk(ilnewsk)
          endif
        endif
C
C     3. Get a new version of the station proc file.
C
 300    continue
        call fs_get_lnewpr2(ilnewpr2)
        if (ilnewpr2(1).eq.0) then
          lnewpr2=' '
        else
          call hol2char(ilnewpr2,1,MAX_SKD,lnewpr2)
        endif
        if (lnewpr2.ne.' '.and..not.kstak(istkop,istksk,2)) then
          call fmpclose(idcbp2,ierr)
          call fs_get_lstp2(ilstp2)
          call hol2char(ilstp2,1,MAX_SKD,lstp2)
          nch = trimlen(lstp2)
          call follow_link(lstp2(:nch),link,ierr)
          if(ierr.lt.0) then
             call logit7ci(0,0,0,1,-505+ierr,'bo',ierr)
             goto 400
          endif
          if(link.eq.' ') then
             pathname = FS_ROOT//'/proc/' // lstp2(:nch) // '.prc'
          else
             pathname = FS_ROOT//'/proc/' // link(:trimlen(link))
          endif
          call ftn_purge(pathname,ierr)
          call fs_get_lstp2(ilstp2)
          call hol2char(ilstp2,1,MAX_SKD,lstp2)
          nch = trimlen(lstp2)
          if(link.eq.' ') then
             pathname2= FS_ROOT//'/proc/' // lstp2(:nch) // '.prx'
          else
             iprc=index(link,".prc")
             link(iprc+3:iprc+3)='x'
             pathname2= FS_ROOT//'/proc/' // link(:trimlen(link))
          endif
          call ftn_rename(pathname2,ierr1,pathname,ierr2)
          if (ierr1.lt.0) then
            call logit7ci(0,0,0,1,-127,'bo',ierr1)
            ierr1 = 0
          else if (ierr2.lt.0) then
            call logit7ci(0,0,0,1,-127,'bo',ierr2)
            ierr2 = 0
          else
            call fs_get_lstp2(ilstp2)
            call hol2char(ilstp2,1,MAX_SKD,lstp2)
            call opnpf(lstp2,idcbp2,ibuf,iblen,lproc2,maxpr2,nproc2,
     $                 ierr,'o')
            lnewpr2 = ' '
            call char2hol(lnewpr2,ilnewpr2,1,MAX_SKD)
            call fs_set_lnewpr2(ilnewpr2)
            call char2hol(lnewpr2,ilnewpr,1,8)
            call fs_set_lnewpr(ilnewpr)
          endif
        endif
C
 400    continue
        call rn_put('pfmed') 
        if (ierr.lt.0) call logit7ci(0,0,0,1,-131,'bo',ierr)
      endif
C
      return
      end
