      subroutine trkall(itras,lmode,itrk,lm,nm,ifan,itrax)

C  TRKALL returns the complete list of tracks to be
C  recorded, given the mode and list of tracks assigned
C  in the schedule file. It looks at the mode name to
C  determine whether there is fan-in or fan-out and adds
C  or subtracts tracks appropriately.
C  Also outputs a 3-character mode for the procedure names,
C        in the form "Vxy" where x and y are the fan factors,
C        If fan factor is unity, the mode is simply "V".
C        For Mark III modes, the name is the one-letter mode.
C  Restrictions: 
C  1. Fan in/out may be 1:2, 2:1, 4:1 or 1:4 only.
C  3. VLBA modes must begin with "V".
C  4. Track assignments in "itras" are converted to VLBA 
C     track numbers when they are returned in "itrk".
C                 
C History
C 951214 nrv New.
C 960124 nrv Bad fan-out logic replaced. 
C 960201 nrv Worse fan-out logic replaced.
C 960531 nrv Fanout factor is input, already determined.
C 961018 nrv Fan out the 'M' modes just like the 'V' ones.
C
C Called by: PROCS

      include '../skdrincl/skparm.ftni'
C
C  INPUT:
      integer itras(2,2,max_chan)
C             Mark III # track assignments from schedule
C             sub-array for only this code, this station
      integer*2 lmode ! first 2 characters of mode from schedule
      integer ifan ! fanout factor
C
C  OUTPUT:
      integer itrk(36) ! tracks to be recorded/enabled
C           VLBA track # assignments
      integer*2 lm(2) ! 3-character mode for procedure names
      integer nm ! number of characters in lm, 1 or 3
      integer itrax(2,2,max_chan) ! a fanned-out version of itras
C
C  LOCAL:
      integer idum,it,i,n,iy,ibit,ichan,isb
      integer ichcm_ch,ib2as,ichmv,ichmv_ch,iscn_ch,ias2b
C
C
      call ifill(lm,1,4,oblank)
      idum = ichmv(lm,1,lmode,1,1) ! first character is mode
      nm = 1
C
C 1. Initialize the itrax array to itras values.
C    Initialize itrk to 0.

      do i=1,36
        itrk(i)=0
      enddo
      do isb=1,2
        do ibit=1,2
          do ichan=1,max_chan
            itrax(isb,ibit,ichan)=itras(isb,ibit,ichan)
            it = itrax(isb,ibit,ichan)
            if (it.ne.-99) itrk(it+3)=1
          enddo
        enddo
      enddo

C 2. Now check for fan-out and add the appropriate tracks. 

C     If this is a VLBA mode or Mk4 mode, check for fan-out
      if (ichcm_ch(lm,1,'V').eq.0.or.ichcm_ch(lm,1,'M').eq.0) then 
       
      if (ifan.ne.0) then ! fan-out
          idum = ichmv_ch(lm,2,'1') ! fan-out 1:
          idum = ib2as(ifan,lm,3,1) ! fan-out   n
          nm=3 ! 3 characters in mode name
          if (ifan.gt.1) then ! add fanout tracks
            do isb=1,2 ! u/l
              do ibit=1,2 ! s/m
                do ichan=1,max_chan ! channels
                  it = itras(isb,ibit,ichan)
                  if (it.ne.-99) then ! fan it out
                    if (ifan.eq.2.or.ifan.eq.4) then ! 1:2
                      itrax(isb,ibit,ichan+2)=it+2
                      itrk(it+2+3)=1
                    endif
                    if (ifan.eq.4) then ! 1:4
                      itrax(isb,ibit,ichan+4)=it+4
                      itrk(it+4+3)=1
                      itrax(isb,ibit,ichan+6)=it+6
                      itrk(it+6+3)=1
                    endif
                  endif ! fan it out
                enddo ! channels
              enddo ! s/m
            enddo ! u/l
          endif ! add fanout tracks
      endif

C 3. Fan-in mode. Not implemented.

      iy = iscn_ch(lmode,1,8,':1') 
      if (iy.ne.0) then ! fan-in
        n=ias2b(lmode,iy-1,1)
        if (n.eq.1.or.n.eq.2.or.n.eq.4) then ! valid fan
          idum = ichmv_ch(lm,1,'V 1 ')
          idum = ib2as(n,lm,2,1)
          nm=3
        endif
C       No fan-in track handling at this time
      endif
C
      endif ! VLBA mode and check for fan

C 4. Fill in the itrk array. Already done above.

C     do isb=1,2 ! u/l
C       do ibit=1,2 ! s/m
C         do ichan=1,max_chan ! channels
C           it = itrax(isb,ibit,ichan)
C           if (it.ne.-99) itrk(it+3)=1
C         enddo ! channels
C       enddo ! s/m
C     enddo ! u/l

      RETURN
      END
