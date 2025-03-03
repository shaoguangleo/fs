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
      function iat(itran,ntr,lumat,kecho,irecv,nrc,ierr,itn)
C 
C   IAT handles communications with the MAT 
C 
C  WHO  WHEN    WHAT
C  NRV  810624  MODIFY FOR DVF00 TRIGGER CHAR IMMEDIATE TURNAROUND
C  NRV  811012  REMOVE SOME WVR-SPECIFIC CODE (AFTER RBM) 
C  MWH  870911  Modify for use with A400 8-channel MUX
C  gag  920716  Added mode -54.
C
      include 'matcm.i'
      include '../include/time_arrays.i'
C 
C  INPUT VARIABLES:
C 
C  NTR    - number of characters in ITRAN 
C  LUMAT  - LU of MAT daisy chain 
C
      integer*2 itran(1)         ! buffer to be transmitted
      logical kecho              ! true if MAT communications echo desired
C 
C  OUTPUT VARIABLES: 
C 
C  NRC    - number of characters in received buffer 
C  IERR   - error code

      integer*2 irecv(1)         ! buffer received from AT 
C 
C  CALLING SUBROUTINES: MATCN,DATAT
C
C  CALLED SUBROUTINES: character utilities
C 
C LOCAL VARIABLES 
C 
      include '../include/boz.i'
C
C  ILEN   - max length available in ITRAN buffer, characters. 
C          ***NOTE*** THIS MUST CORRESPOND TO THE LENGTH
C                     OF IBUF2 IN MATCN, LESS 1 WORD. 
C  IFRECV - 1 if we expect response, 0 if none expected
C  IR     - which terminal character in IRSPN we find
C  MAXTRY - maximum number of times we will try to communicate
C  ITRY - count of attempts
C  NCTRAN - number of char to be transmitted
C  NRSPN  - number of responses possible
      integer ichmv, portflush, portwrite,portread,portoutdrain
      dimension ireg(2)
      integer nchrc(8)   ! number of characters received in responses
      integer*2 irspn(4) ! terminal characters which generate a response
      integer wrdech,maxech
      parameter (wrdech=256,maxech=wrdech*2)
      integer*2 iebuf(wrdech),iebuf2(wrdech)
      integer itn
      integer maxlog,maxmess,maxlogwd
      parameter (maxlog=512,maxmess=maxlog-18,maxlogwd=maxmess/2)
      integer*2 echo_log(maxlogwd)
      integer*2 dbg_buf(maxlogwd)
      integer*2 irecx(13)
      character*1 cjchar
      logical kprompt
      integer*4 icent
C
      equivalence (ireg(1),reg)
C
C   INITIALIZED VARIABLES
C
      data nchrc/3,3,10,10,3,3,16,3/
      data irspn/2h$*,2h?/,2h>',2h :/
C                   We put <enq> in first charcater of last word by hand below
      data ilen/158/
      data nrspn/8/
      data maxtry/2/
C
      call pchar(irspn(4),1,5)
C
C 1. Set up the buffer to be sent to the MAT.
C    First initialize some things.
C
      ierr = 0
      itry = 0
      nrc = 0
      nctran = ntr
      itimeout=itn
C
C  1.1 Find the last character in the buffer to determine the
C      type of response.
C
      if (iscn_ch(itran,1,ntr,':').ne.0) then
        ir = 8
        ifrecv = 1
      else if (imode.eq.-54) then
        ifrecv = 1
      else if (imode.eq.9) then
        ifrecv = 1
      else if (imode.eq.10) then
        ifrecv = 1
      else if (imode.eq.11) then
        ifrecv = 1
      else
C  A colon in the message means a response to the download 
        do ir=1,nrspn
          if (jchar(itran,ntr).eq.jchar(irspn,ir)) then
            ifrecv = 1
            goto 200
          endif
        enddo
        ifrecv = 0
      endif
C  Check each type of terminal character
C  IR holds the index for the type of response
C
C  2. Write the buffer to the MAT, and read response if expected.
C     Set the time-out on the MAT depending on the response.
C
200   continue
c
c special delay for mark4 formatter only
c
      call fc_rte_ticks(icent)
      if(imode.eq.10.and.(i4dcent+3.lt.icent.or.icent.lt.i4dcent)) then
         if(icent.lt.i4dcent) then
            icent=3
         else
            icent=i4dcent+3-icent
         endif
         if(icent.lt.4.and.icent.gt.0) call susp(1,icent)
      endif
      if (kecho) then
        call echoe(itran,iebuf,nctran,iecho,maxech)
      endif
C  Write message on the screen if echo is on
      ilen=nchrc(ir)
      ierr=portflush(lumat)
      if(imode.eq.-53) then
        ierr=portwrite(lumat,itran,nctran-1)
        idum=ichmv(irecx,1,itran,nctran,1)
        call fc_rte_cmpt(unixsec(1),unixhs(1))
        call fc_rte_ticks(centisec(1))
        ierr=portwrite(lumat,irecx,1)
      else if(imode.eq.-54) then
        call fc_rte_cmpt(unixsec(1),unixhs(1))
        call fc_rte_ticks(centisec(1))
        ierr=portwrite(lumat,itran,nctran)
      else
        ierr=portwrite(lumat,itran,nctran)
c        write(6,'(i10,1x,39a2)')
c     &       nctran,(itran(iweh),iweh=1,(nctran+1)/2)
      endif
C                   Write the buffer to the MAT bus
      if (ifrecv.eq.0) then
        iat = 0
        if (kecho) then
           idum=ichmv(echo_log,1,iebuf,1,min(maxmess-idum-1,iecho))
           call logit2(echo_log,idum-1)
        endif
        return    !  we're done now if there is to be no response.
      endif
c
c wait for the output to drain
c
C removing output drain maybe a 
C solution for an intermittent kernel 2.6 problem with SuperMicro C7SIM-Q?
c     ierr=portoutdrain(lumat)
C
C  For actual communications, use ocp2000 in the read request.
C  For terminal tests, use ocp400 instead.
C    ocp2000 = 0 0 0 0 1 0 0 0 0    followed by six bits of LU #
C            !       !   !   ! ASCII read
C            !       !   ! no echo
C            !       ! transmit special characters
C            ! buffered mode
C
      maxc=ilen
C  at this time, don't know how many characters are expected 7/16/92
      if(imode.eq.-53) then
        ireg(1)=portread(lumat,irecx,ilen,1,-1,itimeout)
        call fc_rte_ticks(centisec(2))
        call fc_rte_cmpt(unixsec(2),unixhs(2))
        ireg(1)=portread(lumat,irecx(2),ilen,maxc-1,-1,itimeout)
        idum=ichmv(irecv,1,irecx,1,1)
        idum=ichmv(irecv,2,irecx(2),1,maxc-1)
        ilen=ilen+1
      else if(imode.eq.-54) then
        maxc=26
        ireg(1)=portread(lumat,irecx,ilen,1,-1,itimeout)
        if(ireg(1).eq.0.and.ilen.eq.1) then
           call fc_rte_ticks(centisec(2))
           call fc_rte_cmpt(unixsec(2),unixhs(2))
           ireg(1)=portread(lumat,irecx(2),ilen,maxc-1,10,itimeout)
           idum=ichmv(irecv,1,irecx,1,1)
           idum=ichmv(irecv,2,irecx(2),1,maxc-1)
           ilen=ilen+1
        endif
      else if (imode.eq.9.or.imode.eq.11) then
        maxc=178
        if(imode.eq.11) itimeout=max(210,itimeout)
        ireg(1)=portread(lumat,irecv,ilen,maxc,10,itimeout)
c         read(5,'(39a2)') (irecv(iweh),iweh=1,39)
c         ilen=iscn_ch(irecv,1,78,'$')-1
c         write(6,'(''got:'',39a2)') (irecv(iweh),iweh=1,(ilen+1)/2)
c         ireg(1)=0
      else if(imode.eq.10) then
        maxc=178
        ireg(1)=portread(lumat,irecv,ilen,maxc,m4dt,itimeout)
        call fc_rte_ticks(i4dcent)        
      else
        ireg(1)=portread(lumat,irecv,ilen,maxc,-1,itimeout)
      endif
c     if (ichcm_ch(irecv,1,'t').eq.0) then
c       ierr = -4
c       return
c     endif
c
      nrc=ilen
      if (kecho) then
         call echoe(irecv,iebuf2,nrc,iecho2,maxech)
         idum=ichmv(echo_log,1,iebuf,1,min(maxmess-idum-1,iecho))
         idum=ichmv(echo_log,idum,iebuf2,1,min(maxmess-idum-1,iecho2))
         call logit2(echo_log,idum-1)
      endif
C  If echo requested, write response on screen
      itry = itry + 1
C
C
C  8. Now check for errors.  If time-out or wrong number of characters,
C     try communications all over again.
C     If we got a ocp6 (ack) or ocp25 (nak) substitute readable ACK or NAK.
C
      if(ireg(1).eq.-1) then !bad buffer length on read
         ierr=-10
      else if(ireg(1).eq.-3) then !read error
         ierr=-11
      else if (ireg(1).eq.-2) then ! timeout
         if (imode.eq.9.or.imode.eq.10.or.imode.eq.11.or.
     &        imode.eq.-54) then
            call echoe(itran,iebuf,nctran,iecho,maxech)
            idum=ichmv_ch(dbg_buf,1,"debug: ")
            idum=ichmv(dbg_buf,idum,iebuf,1,iecho)
            if(nrc.gt.0) then
               call echoe(irecv,iebuf2,nrc,iecho2,maxech)
               idum=ichmv(dbg_buf,idum,iebuf2,1,iecho2)
            endif
            call logit2(dbg_buf,idum-1)
         endif
         if ((imode.eq.9.or.imode.eq.10.or.imode.eq.11.or.imode.eq.-54)
     &        .and.itry.lt.3) goto 200
         if (itry.lt.maxtry) goto 200
         ierr = -4
      else if (nrc.ne.nchrc(ir).and.imode.ne.9.and.imode.ne.10
     &       .and.imode.ne.11.and.imode.ne.-54) then
c                                ! wrong # of characters in response
         if (itry.lt.maxtry) goto 200
         call ifill_ch(irecv,1,80,' ')
         nrc=0
         ierr = -5
      else if (imode.eq.9.or.imode.eq.11.or.imode.eq.-54) then
         if(nrc.ge.3) then
            kprompt=ichcm_ch(irecv,nrc-2,'>'//char(13)//char(10)).eq.0
         endif
         if(nrc.lt.3.or..not.kprompt) then
            call echoe(irecv,iebuf2,nrc,iecho2,maxech)
            idum=ichmv_ch(dbg_buf,1,'debug: ')
            idum=ichmv(dbg_buf,idum,iebuf2,1,iecho2)
            call logit2(dbg_buf,idum-1)
            if(itry.lt.3) goto 200
            ierr=-8
            goto 999
         endif
         nrc=nrc-3
         if(nrc.ge.10) then
            do i=nrc,max(1,nrc-24),-1
               if(cjchar(irecv,i).eq.'?') then
                  if(ichcm_ch(irecv,i,'? ERROR').eq.0) then
                     call echoe(irecv,iebuf2,nrc,iecho2,maxech)
                     idum=ichmv_ch(dbg_buf,1,'debug: ')
                     idum=ichmv(dbg_buf,idum,iebuf2,1,iecho2)
                     call logit2(dbg_buf,idum-1)
                     ierr=ias2b(irecv,i+8,1+(nrc-1)-(i+8))
                     if(ierr.eq.7.and.itry.lt.3) goto 200
                     if(ierr.eq.32768) then
                        ierr=-998
                     else if(ierr.lt.-50.or.ierr.gt.50) then
                        ierr=-999
                     else if(ierr.gt.0) then
                        ierr=-800-ierr
                     else if(ierr.lt.0) then
                        ierr=-850+ierr
                     endif
                     goto 999
                  endif
               endif
            enddo
         else if(nrc.eq.0) then
            ierr = +1
            nrc = ichmv_ch(irecv,1,'ack') - 1
         endif
      else if (imode.eq.10) then
         if(ichcm_ch(irecv,1,'error ').eq.0.and.nrc.ge.8) then
            ierr=ias2b(irecv,7,2)
            if(ierr.eq.32768) then
               ierr=-997
            else
               ierr=-700-ierr
            endif
            goto 999
         endif
         if(m4dt.eq.13.and.jchar(irecv,1).eq.10) then
           do i=2,nrc
             call pchar(irecv,i-1,jchar(irecv,i))
           enddo
           nrc=nrc-1
         endif
         nrc=nrc-1
      else if (jchar(irecv,1).eq.ocp6) then ! ack response
         ierr = +1
         nrc = ichmv_ch(irecv,1,'ack') - 1
      else if (jchar(irecv,1).eq.ocp25) then ! nak response
         if (ir.eq.5.and.itry.lt.maxtry) goto 200
         ierr = +2
         nrc = ichmv_ch(irecv,1,'nak') - 1
      else
         ierr = 0
      endif
c
 999  continue
      iat = ierr
c
      return
      end 
