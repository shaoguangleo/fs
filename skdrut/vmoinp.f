      SUBROUTINE VMOINP(ivexnum,LU,IERR)

C     This routine gets all the mode information from the vex file.
C     Call once to get all values in freqs.ftni filled in, then call 
C     SETBA to figure out which frequency bands are there.
C
C History
C 960518 nrv New.
C 960522 nrv Revised.
C 960610 nrv Move initialization of freqs.ftni arrays here
C 960817 nrv Add S2 record mode
C 961003 nrv Keep getting modes even if there are too many.
C 961018 nrv Change the index on the BBC link to be the index found when
C            the BBC list was searched, instead of the channel index.
C            Check "lnetsb" and not "lsubvc" (subgroup!) for sideband.
C 961020 nrv Add call to VUNPROLL and store the roll def in LBARREL
C 961022 nrv Change MARK to Mark for checking rack/rec types.
C 961101 nrv Don't complain if things are missing from the modes for 
C            some stations. Just set nchan to 0 as a flag.
C 970110 nrv Save the pass order list by code number.
C 970114 nrv Call VUNPPRC to read $PROCEDURES. Store prefix. 
C 970114 nrv Add polarization to VUNPIF call. Save LPOL per channel.
C 970121 nrv Save npassl by code and station!
C 970123 nrv Add calls to ERRORMSG
C 970124 nrv Remove "lm" from call to vunpS2M
C 970206 nrv Remove itra2, ihdpo2,ihddi2 and add headstack index
C            Change call to VUNPHP to add number of headstacks found.
C 970206 nrv Change size of arrays to VUNPTRK for fandefs to max_track.
C 970213 nrv Remove the test for a rack before setting NCHAN. It should be
C            set even for rack=none
C 971208 nrv Add fpcal, fpcal_base to vunpif call. Add cpcalref to vunpfrq.
C 971208 nrv Add call to VUNPPCAL.
C 991110 nrv Save modedefname as catalog name.
C 011119 nrv Clean up logic so that information isn't saved if there
C            aren't any chan_defs.
C 020112 nrv Add roll parameters to VUNPROLL call. Add call to CKROLL.!
C 020327 nrv Add data modulation paramter to VUNPTRK.
C 021111 jfq Extend S2 mode to support LBA rack
! 2006Oct06. Made arguments to vunpif all ASCII.
! 2006Nov18. Converted remaining holleriths to ASCII.
! 2006Nov29  Capitalize before checking on recorder.
! 2007Jul13  Fixed bug if nchdefs=0.  Was trying to check roll, but this wouldn't work
! 2010.06.15 Fixed bug if recorder was K5. Wasn't initializing tracks. 
! 2012Sep14  Fixed bug with not initializing bbc_present for VEX schedules
! 2015Jun05  JMG Modified to use new version of itras. 
! 2016Jan19  JMG. Changed dim of variables: max_track-->2*max_track since sign& magnitude can be on same  track 
! 2016Jan19 Also re-arranged definition of parameters to group like to together
! 2017Feb27  Skip some stuff dealing with headstack for Disk recording.
! 2018Oct09  Preserve mode and band if VEX created from sked previously.  Previously was setting to numerical value, first mode=01, 2nd 02.
!            Also keep better track of number of  freq-channels. If everything except for side-band is the same, assume same freq-channels
!            which means use same BBC. 
! 2019Aug27 Above was good, but did not check if from sked file.  
   

      implicit none 
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/statn.ftni'
C
C  INPUT:
      integer ivexnum,lu
C
C  OUTPUT:
      integer ierr

C  CALLED BY: 
C  CALLS:  fget_mode_def
C          frinit
C          vunpfrq, vunpbbc,vunpif,vunpprc,vunptrk,vunphead,
C          vunroll,ckroll
! function
      integer iwhere_in_string_list   
      integer ptr_ch,fvex_len,fget_mode_def,fget_all_lowl

C  LOCAL:  
      logical kadd_track_map

      integer ix,ib,ic,i,ia,icode,istn
      integer il,im,in, iret,ierr1,iul,ism,ip,ipc,itone
      integer iinc,ireinit
      integer ifanfac


      integer irtrk(18,max_roll_def)
      integer ih 

      double precision bitden_das
      integer nsubpass,npcaldefs,nrdefs,nrsteps
      integer nchdefs,nbbcdefs,nifdefs,nfandefs,nhd,nhdpos,npl
 
      character*8 cpre
      character*16 cs2m
      character*16 cm
      character*8 cs2d

      double precision bitden
      character*4 cmodu, croll ! ON or OFF

! IF related parameters. 
      character*6 cifdref(max_ifd)
      character*2 cin2(max_ifd),cs2(max_ifd),cp2(max_ifd)      !LO, sideband

! BBC parameters
      character*6 cbbcref(max_bbc)
!Things that depend on number of channels.

      character*3 cs(max_chan)
      character*6 cfrbbref(max_chan)

      character*6 cbbifdref(max_chan)
      character*6 cfrpcalref(max_chan)
      character*6 cpcalref(max_chan)
      character*6 cchanidref(max_chan)
      character*2 csb(max_chan),csg(max_chan)
      integer ipct(max_chan,max_tone),ntones(max_chan)

      double precision frf(max_chan),flo(max_chan),vbw(max_chan)
      double precision fpcal(max_chan),fpcal_base(max_chan)

! Things that depend on number of tracks. 
      character*6 ctrchanref(2*max_track)
      character*1 cp(2*max_track),csm(2*max_track)
      integer ihdn(2*max_track)
      integer itrk(2*max_track),ivc(2*max_bbc)

!things that depend pass.
      double precision posh(max_index,max_headstack)
      integer indexp(max_index),indexl(max_pass)
      integer iii
      character*1 csubpassl(max_pass),csubpass(max_subpass)
      character*3 cpassl(max_pass)

      double precision srate
      character*128 cout  
     
      logical kDiskRec,km4rec,km3rec,kvrec,ks2rec
      logical kk4rec

      character*8 crecorder  !temporary holding
      integer ibbc   !index 

      logical kvunppcal_first    !first call to vunppcall
      character*1 lq 

      integer ifc            !number of frequency channels. 
      integer ib_old 

      integer ind
      integer itmp
      logical kvex_from_sked

!*********************************************************************************************
! Start of code
      lq="'"
      kvunppcal_first=.true. 
      do iii=1,max_index
        indexp(iii)=0             !initialize 
      enddo


! See if has $SCHEDULING_PARAMS
       iret=fget_all_lowl(ptr_ch(char(0)),ptr_ch(char(0)),
     .  ptr_ch('literals'//char(0)),
     .  ptr_ch('SCHEDULING_PARAMS'//char(0)),ivexnum)      
      kvex_from_sked=iret .eq. 0
      write(*,*) "Kvex_from_from_sked: ", kvex_from_sked 
 
! 1. First get all the mode def names. 
!    Station names have already been gotten and saved.
!
      ncodes=0
      iret = fget_mode_def(ptr_ch(cout),len(cout),ivexnum) ! get first one
      do while (iret.eq.0.and.fvex_len(cout).gt.0)
        il=fvex_len(cout)
        IF  (ncodes.eq.MAX_FRQ) THEN  !
          write(lu,'("VMOINP01 - Too many modes.  Max is ",
     .    i3,".  Ignored: ",a)') MAX_FRQ,cout(1:il)
        else
          ncodes=ncodes+1
          modedefnames(ncodes)=cout
          if (il.gt.16) then
            write(lu,'(a)')  "VMOINP02 - Mode name   "//cout(1:il)//
     >      " too long for matching in sked catalogs."
            write(lu,'(a)') "Only keeping 16 chars: "//cout(1:16)
            il=16
          endif
          cmode_cat(ncodes)=cout(1:il)
          write(*,*) "Found mode: ", cout(1:il)
! This is the default
          write(ccode(ncodes)(1:2),'(i2.2)') ncodes
          cnafrq(ncodes)=ccode(ncodes)
          ind=index(cout(1:il),".") 
          if(kvex_from_sked.and. ind .ne. 0) then 
            itmp=min(8,ind-1)
            if(itmp .gt. 0) then
              itmp=min(8,ind-1) 
              cnafrq(ncodes)=cout(1:itmp)
              ccode(ncodes)=cout(ind+1:ind+2)
              write(*,*) cnafrq(ncodes), " ", ccode(ncodes) 
            endif 
          endif 
        END IF
        iret = fget_mode_def(ptr_ch(cout),len(cout),0) ! get next one
      enddo
  
C 1.5 Now initialize arrays using nstatn and ncodes.
      call frinit(nstatn,ncodes)
 
C 2. Call routines to retrieve and store all the mode/station 
C    information, one mode/station at a time. Not all modes are
C    defined for all stations. 

      ierr1=0
      do icode=1,ncodes ! get all mode information 

! START pre-2018OCT09 way of assigning codes. 
C    Assign a code to the mode and the same to the name
!        write(ccode(icode)(1:2),'(i2.2)') icode
!        cnafrq(icode)=ccode(icode)
! End old way of assigning codes. 
        do istn=1,nstatn ! for one station at a time

! Initialize this array.
          call new_track_map()
          kadd_track_map=.false.

          il=fvex_len(modedefnames(icode))
          im=fvex_len(stndefnames(istn))
!          write(*,*) "stndefnames: ", stndefnames(istn)
!          write(*,*) "modeefnames: ", modedefnames(icode)

          crecorder=cstrec(istn,1)
          call capitalize(crecorder)
       
! Find what kind of recorder.
          kvrec=.false.
          km3rec=.false.
          km4rec=.false.
          kk4rec=.false.
          ks2rec=.false.
          kDiskRec=.true.

          if(crecorder .eq. "VLBA" .or. crecorder .eq. "VLBA4") then
             kvrec  =.true.
          else if(crecorder .eq. "MARK3") then
            km3rec =.true.
          else if(crecorder .eq. "MARK4") then 
            km4rec =.true.
          else if(crecorder .eq. "K4-1" .or.
     >             crecorder .eq. "K4-2") then
            kk4rec=.true.
          else if(crecorder .eq. "S2") then
            ks2rec = .true.
!            write(*,*) "VMOINP. Where did you get an S2 recorder?"
!            ks2rec = .false. 
          else if(crecorder.eq."MARK5A" .or. 
     >            crecorder .eq. "MARK5B" .or.
     >            crecorder.eq. "K5") then 
            kdiskRec=.true.
          else          
            kdiskRec=.true. 
          end if 
          if(.not.kdiskRec) then
            write(*,'("VMOINP. For station ", a, " recorder= ", a,
     >       "...assuming final recorder will be disk based.")') 
     >       cstnna(istn), crecorder
            kdiskRec=.true.
 !           ks2rec  =.false. 
          endif       
   
C         Initialize roll to blank
          cbarrel(istn,icode)=" "
         
C         Get $FREQ statements. If there are no chan_defs for this
C         station, then skip the other sections.
          CALL vunpfrq(modedefnames(icode),stndefnames(istn),
     >      ivexnum,iret,ierr,lu,bitden_das,srate,cSG,Frf,csb,
     >      cchanidref,VBw,cs,cfrbbref,cfrpcalref,nchdefs)
          if (ierr.ne.0) then 
            write(lu,'("VMOINP02 - Error getting $FREQ information",
     >        " for mode ",a," station ",a/" iret=",i5," ierr=",i5)') 
     >        modedefnames(icode)(1:il),stndefnames(istn)(1:im),
     >        iret,ierr
            call errormsg(iret,ierr,'FREQ',lu)
            ierr1=1
          endif

C         Get $PROCEDURES statements.
C         (Get other procedure timing info later.)
          call vunpprc(modedefnames(icode),stndefnames(istn),
     &      ivexnum,iret,ierr,lu,cpre)
          if (ierr.ne.0) then
            write(lu,'("VMOINP03 - Error getting $PROCEDURES for mode "
     &      ,a," station ",a/" iret=",i5," ierr=",i5)') 
     &      modedefnames(icode)(1:il),stndefnames(istn)(1:im),
     &      iret,ierr
            call errormsg(iret,ierr,'PROCEDURES',lu)
            ierr1=1
          endif

        nchan(istn,icode) = nchdefs
        if (nchdefs.gt.0) then ! chandefs > 0
C         Get $BBC statements.
          call vunpbbc(modedefnames(icode),stndefnames(istn),
     >     ivexnum,iret,ierr,lu,cbbcref,ivc,cbbifdref,nbbcdefs)
          do ibbc=1,max_bbc
            if(ivc(ibbc) .ne. 0) then
               ibbc_present(ivc(ibbc),istn,icode)=1
            endif
          end do 

          if (ierr.ne.0) then 
            write(lu,'("VMOINP04 - Error getting $BBC information",
     .      " for mode ",a," station ",a/" iret=",i5," ierr=",i5)') 
     .      modedefnames(icode)(1:il),stndefnames(istn)(1:im),iret,ierr
            call errormsg(iret,ierr,'BBC',lu)
            ierr1=2
          endif

C         Get $IF statements.
          call vunpif(modedefnames(icode),stndefnames(istn),
     .       ivexnum,iret,ierr,lu,
     .       cifdref,flo,cs2,cIN2,cp2,fpcal,fpcal_base,nifdefs)
          if (ierr.ne.0) then 
            write(lu,'("VMOINP05 - Error getting $IF information",
     .      " for mode ",a," station ",a/" iret=",i5," ierr=",i5)') 
     .      modedefnames(icode)(1:il),stndefnames(istn)(1:im),iret,ierr
            call errormsg(iret,ierr,'IF',lu)
            ierr1=3
          endif
  
C         Get $TRACKS statements (i.e. fanout).
          if (ks2rec) then
            call vunps2m(modedefnames(icode),stndefnames(istn),
     .      ivexnum,iret,ierr,lu,cs2m,
     .      cs2d,cp,ctrchanref,csm,itrk,nfandefs,ihdn,ifanfac)
          else
            call vunptrk(modedefnames(icode),stndefnames(istn),kDiskRec,
     .      ivexnum,iret,ierr,lu,
     .      cm,cp,ctrchanref,csm,itrk,nfandefs,ihdn,ifanfac,cmodu)
          endif
          if (ierr.ne.0) then 
            write(lu,'("VMOINP06 - Error getting $TRACKS information",
     .      " for mode ",a," station ",a/" iret=",i5," ierr=",i5)') 
     .      modedefnames(icode)(1:il),stndefnames(istn)(1:im),
     .      iret,ierr
            if (ks2rec) then
              call errormsg(iret,ierr,'S2_TRACKS',lu)
            else
              call errormsg(iret,ierr,'TRACKS',lu)
            endif
            ierr1=4
          endif
       
C         Get $HEAD_POS and $PASS_ORDER statements.
          if (ks2rec) then
            call vunps2g(modedefnames(icode),stndefnames(istn),ivexnum,
     >          iret,ierr,lu,cpassl,npl)
          else if(kDiskRec) then
            npl=1
            nhdpos=1
            cpassl(1)="1A"
            csubpassl(1)="A"
          else                   
            call vunphp(modedefnames(icode),stndefnames(istn),ivexnum,
     .        iret,ierr,lu,
     .        indexp,posh,nhdpos,nhd,cpassl,indexl,csubpassl,npl)
          endif     
          if (ierr.ne.0) then 
            write(lu,'("VMOINP07 - Error getting $HEAD_POS and",
     .        "$PASS_ORDER information for mode",a, " station ",a,
     .         /, " iret=",i5," ierr=",i5)') 
     .      modedefnames(icode)(1:il),stndefnames(istn)(1:im),iret,ierr
            if (ks2rec) then
              call errormsg(iret,ierr,'S2_HEAD_POS',lu)
            else
              call errormsg(iret,ierr,'HEAD_POS',lu)
            endif
            ierr1=5
          endif

C         Get $ROLL statements.
          call vunproll(modedefnames(icode),stndefnames(istn),ivexnum,
     .      iret,ierr,lu,croll,irtrk,iinc,ireinit,nrdefs,nrsteps)
          if (ierr.ne.0) then 
            write(lu,'("VMOINP08 - Error getting $ROLL information",
     .      " for mode ",a," station ",a/" iret=",i5," ierr=",i5)') 
     .      modedefnames(icode)(1:il),stndefnames(istn)(1:im),iret,ierr
            call errormsg(iret,ierr,'ROLL',lu)
            ierr1=6
          endif

C         Get $PHASE_CAL_DETECT statements.
          call vunppcal(modedefnames(icode),stndefnames(istn),ivexnum,
     >        iret,ierr,lu,cpcalref,ipct,ntones,npcaldefs,
     >         kvunppcal_first)
C
C 3. Now decide what to do with this information. If we got to this
C    point there were no reading or content errors for this station/mode
C    combination. Some consistency checks are done here.
C
C    Count subpasses and store subpass names found in the fanout defs.
C    Now IS necessary for S2 recorders.
C         if (ks2rec) then
C         else ! non-S2
            do i=1,max_subpass
              csubpass(i)=' '
            enddo
            nsubpass=0
            do i=1,nfandefs ! each fandef
              ix=iwhere_in_string_list(csubpass,nsubpass,cp(i))
              if(ix .eq. 0) then  !not found. A new pass.
                if (nsubpass.gt.max_subpass) then
                  write(lu,'("VMOINP15 - Too many subpasses for mode",
     .              a," station ",a,". Max is ",i5,".")') 
     .              modedefnames(icode)(1:il),stndefnames(istn)(1:im),
     .              max_subpass
                else
                  nsubpass=nsubpass+1
                  csubpass(nsubpass)=cp(i)
                endif
              endif
            enddo ! each fandef
! 2014Sep22
            nsubpass=1
! 2014Sep22 

C         endif ! S2/not

! prior to 2018Oct03, did not use ifc.
! 'ifc' keeps track of number of independnent channels.
!  if have same channel configuration except for sidebands,  corresponds to same ifc.
!  Because of this  final ifc can less than nchdefs. 
  
          
          ifc=1   !initialize
C    Save the chan_def info and its links.
          do i=1,nchdefs ! each chan_def line
            ib=iwhere_in_string_list(cbbcref,nbbcdefs,cfrbbref(i))
! Check if previous was the same as this. If so, same frequency channel. 
            if(i .gt.1) then
              if(frf(i) .ne. frf(i-1) .or.
     >           csg(i) .ne. csg(i-1) .or.
     >           vbw(i) .ne. vbw(i-1) .or.
     >           cs(i)  .ne. cs(i-1)  .or.
     >           ib     .ne. ib_old ) then 
                 ifc=ifc+1  
              endif 
            endif 
            ib_old=ib                   

            invcx(ifc,istn,icode)=ifc ! save channel index number 
            cSUBVC(ifc,istn,ICODE) = cSG(i) ! sub-group, i.e. S or X
            FREQRF(ifc,istn,ICODE) = Frf(i) ! RF frequency
!            cnetsb(i,istn,icode) = csb(i) ! net sideband
! Actual value of sideband determined by tracklayout (ITRAS below) 
            cnetsb(ifc,istn,icode) = "U"
!            write(*,*) "CSB: ", csb(i) 
            VCBAND(ifc,istn,ICODE) = VBw(i) ! video bandwidth
            ifan(istn,icode)=ifanfac ! fanout factor
            cset(ifc,istn,icode) = cs(i) ! switching 
C           BBC refs

            if(ib .eq. 0) then
              write(lu,'("VMOINP09 - BBC link missing for channel ",i3,
     &        " for mode ",a," station ",a)') i,
     &        modedefnames(icode)(1:il),stndefnames(istn)(1:im)
            else
              ibbcx(ifc,istn,icode) = ivc(ib) ! BBC number
            endif
            ic=iwhere_in_string_list(cifdref,nifdefs,cbbifdref(ib))
           
            if (ic.eq.0) then
              write(lu,'("VMOINP10 - IFD link missing for channel ",i3,
     &        " for mode ",a," station ",a)') i,
     &        modedefnames(icode)(1:il),stndefnames(istn)(1:im)
            else   
              cifinp(ifc,istn,icode) = cin2(ic) ! IF input channel          
              cosb(ifc,istn,icode)   = cs2(ic)  ! LO sideband
              cpol(ifc,istn,icode)   = cp2(ic)  ! polarization
              freqlo(ifc,istn,icode) = flo(ic)  ! LO frequency
              freqpcal(ifc,istn,icode) = fpcal(ic) ! pcal frequency
              freqpcal_base(ifc,istn,icode) = fpcal_base(ic) ! pcal_base frequency
            endif
C           Phase cal refs
            ipc=iwhere_in_string_list(cpcalref,npcaldefs,cfrpcalref(i))

            if (ipc.eq. 0) then
              in=fvex_len(cfrpcalref(i))
              write(lu,
     >       '("VMOINP15 - PCAL link ",a,a,a" missing for channel ",i3, 
     >            " for mode ",a," station ",a)') 
     >         lq, cfrpcalref(i)(1:in),lq, i,  
     >         modedefnames(icode)(1:il),stndefnames(istn)(1:im)
            else
              do itone=1,ntones(ipc)
                ipctone(itone,i,istn,icode)=ipct(ipc,itone)
                npctone(i,istn,icode)=ntones(ipc)
              enddo
            endif
C           Track assignments
            if (km3rec.or.km4rec.or.kk4rec.or.kDiskRec
     >          .or.kvrec.or.ks2rec) then
            do ix=1,nfandefs ! check each fandef
              if (ctrchanref(ix).eq.cchanidref(i)) then ! matched link
                ip=iwhere_in_String_list(csubpass,nsubpass,cp(ix))
                if (ip.eq.0) then
                  write(lu,'("VMOINP11 - Subpass not found for chandef",
     .             i3,", fandef ",i3," for mode ",a," station ",a)') i,
     .             ix, modedefnames(icode)(1:il),stndefnames(istn)(1:im)
                else
                  ism=1 ! sign
                  if (csm(ix).eq.'m') ism=2 ! magnitude
                  iul=1 ! usb
                  if(csb(i) .eq. "L") iul=2
!                  if(freqlo(i,istn,icode).gt.freqrf(i,istn,icode)) then
! This flips the sign of the sideband that is recorded. 
!                      iul=3-iul     
!                  endif                    
                  if (cstrack(istn) .eq. "LBA" ) then 
                    ia=1
                    do while (ia.le.nchdefs.and.
     &                         (cfrbbref(ia).ne.cfrbbref(i).or.ia.eq.i))
                      ia=ia+1
                    enddo
                    if (ia.le.nchdefs) then
                       if (Frf(i).lt.Frf(ia)) iul=2 ! IFP LSB channel
                       if (Frf(i).gt.Frf(ia)) iul=1 ! IFP USB channel
                    endif
                  endif 
                 call add_track(itrk(ix),iul,ism,ihdn(ix),ifc,ip)            
                 kadd_track_map=.true.
                endif
              endif ! matched link
            enddo ! check each fandef
            endif ! m3/4 or v rec
          enddo ! each chan_def line
          nchan(istn,icode)=ifc
         
C
C    3.2 Save the non-channel specific info for this mode.
C         Recording format, "Mark3", "Mark4", "VLBA".
C         Make these identical for now in VEX files. When there is a
C         mode name available, put that in LMODE. SPEED checks LMFMT
C         to determine DR/NDR. drudg modifies LMFMT from user input
C         for non-VEX.
          if (.not.ks2rec) then
            cmode(istn,icode)=cm
            cmfmt(istn,icode)=cm
          endif
C         Sample rate.
          samprate(istn,icode)=srate ! sample rate

          if (ks2rec) then
            cs2mode(istn,icode)=cs2m
            cs2data(istn,icode)=cs2d
            cmode(istn,icode)=cs2m
          else if(kdiskrec) then
! 2017Feb27. Skip some stuff dealing with headstack for Disk recording.
            continue 

          else ! m3/m4/vrec
C         Set bit density depending on the mode and rack type
            if(cmode(istn,icode)(1:1) .eq. "V") then
              bitden=34020 ! VLBA non-data replacement
            else 
              bitden=33333 ! Mark3/4 data replacement
            endif
C           If "56000" was specified, for this station, use higher bit density
            if (bitden_das.gt.55000.d0) then 
              if(cmode(istn,icode)(1:1) .eq. "V") then
                bitden=56700 ! VLBA non-data replacement
              else 
                bitden=56250 ! Mark3/4 data replacement
              endif
            endif
            if (bitden_das.ne.bitden .and. .not. kDiskRec) then
              write(lu,'("VMOINP12 - Bit density ",f6.0," for ",a," ",a,
     .        " changed to ",f6.0)') bitden_das,
     .        modedefnames(icode)(1:il),stndefnames(istn)(1:im),bitden
            endif
            bitdens(istn,icode)=bitden
C       Check number of passes and pass order indices
            if (npl.ne.nhdpos*nsubpass) then
              write(lu,'("VMOINP13 - Inconsistent pass order list")')
              write(*,*) "npl, nhdpos, nsubpass: ",npl, nhdpos, nsubpass 
            endif

            do ip=1,npl ! number of passes in list
              ix=1
              do while (ix.le.nhdpos.and.indexl(ip).ne.indexp(ix))
                ix=ix+1
              enddo
              if (ix.gt.nhdpos) then
                write(lu,'("VMOINP14 - Index ",i3," in $PASS_ORDER not ",
     .          "found in $HEAD_POS for ",a," ",a)') i,
     .          modedefnames(icode)(1:il),stndefnames(istn)(1:im)
              endif
            enddo  
          endif ! m3/4 or v rec
C    Store head positions and subpases
          do ip=1,npl ! number of passes in list
            cpassorderl(ip,istn,icode) = cpassl(ip)
          enddo
          if (km4rec.or.km3rec.or.kvrec) then
            do ip=1,npl ! number of passes in list
              ix=iwhere_in_string_list(csubpass,nsubpass,csubpassl(ip))
              do ih=1,nhd ! store the head offsets
                ihdpos(ih,ip,istn,icode)=posh(indexl(ip),ih)
                ihddir(ih,ip,istn,icode)=ix   
              enddo
            enddo  ! number of passes in list
          endif ! m3/4 or v rec
          npassl(istn,icode) = npl
        endif ! chandefs > 0

        if ((km4rec.or.km3rec.or.kvrec) .and. nchdefs .gt. 0) then
C         Check barrel roll. croll is either the standard name for
C         the canned mode or "M" to indicate non-standard, or off.
C         The name in the schedule file is not used unless it is "off".
            call ckroll(nrdefs,nrsteps,irtrk,iinc,ireinit,
     .                    istn,icode,croll)
            cbarrel(istn,icode)=croll
        endif

C       Store data modulation
        cmodulation(istn,icode) = cmodu

C       Store the procedure prefix by station and code.
        if(cpre.eq. " ") then
           cpre="01_"
        endif ! missing
        cprefix(istn,icode)=cpre
        if(kadd_track_map) then
           call add_track_map(istn,icode)
        endif
        enddo ! for one station at a time
      enddo ! get all mode information

      ierr=ierr1

      RETURN
      END
