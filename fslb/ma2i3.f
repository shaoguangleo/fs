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
      subroutine ma2i3(ibuf1,ibuf2,iat,imix,is1,is2,is3,is4,ipcalp,
     &     iswp,freq,irem,ipcal,lck,tpi)
c
C  convert mat buffer to i3 data
C 
C     This routine converts the buffers returned from the MAT 
C     into the IF3 distributor attenuator, switches, total power, etc.
C 
C  INPUT: 
C 
      integer*2 ibuf1(1),ibuf2(1) 
C      - buffers of length 5 words, as returned from MATCN
C      - buffer 1 contains the ! strobe data
C      - buffer 2 contains the % strobe data
C 
C  OUTPUT:
C 
C     iat - attenuator setting
C     imix - mixer state
C     is1 - switch 1
C     is2 - switch 2
C     is3 - switch 3
C     is4 - switch 4
c     ipcalp - pcal control present
C     iswp - ext switch present
      integer*4 freq
C          - long frequency
C     irem - remote/local setting 
C     lck -  lo lock indication
C     tpi - total power reading, binary
C 
C     Buffers from MATCN look like: 
C      ! data:      i3fffff0ps
C     where
C                   fffff - is the lo frequency in BBC freqeuncy style
C                   p     - upper bit is zeroif the ext switch is present
C                         - least bit is one if pcal control   is present
C                   s     - four bits representing switch states
C      % data:      IFttttf0at
C     where 
C                   f = lo lock, alarm, pcal on=0/off=1, lcl/remote bits
C                   0 = a fixed zero
C                   a = mixer sw. state (2 bits), upper 2 bits of atten.
C                   t = lower 4 bits of atten.
C
      include '../include/boz.i'
C
      integer*4 fc_bbc2freq,bits
C
C  Atten: upper 2 bits from char 9, lower 4 from char 10
C
      iat = 16*and(ia2hx(ibuf2,9),3) + ia2hx(ibuf2,10)
      imix = and(ia2hx(ibuf2,9),ocp14)/4
      iswh = ia2hx(ibuf1,10)
      is1  = 2-and(iswh,1)
      is2  = 2-(and(iswh,2)/2)
      is3  = 2-(and(iswh,4)/4)
      is4  = 2-(and(iswh,8)/8)
      ipcalp = and(ia2hx(ibuf1,9),1)
      iswp = 1-(and(ia2hx(ibuf1,9),8)/8)
c
c lo freq
c
      bits=ocp200000*ia2hx(ibuf1,3)+ocp10000*ia2hx(ibuf1,4)
     &     + ocp400*ia2hx(ibuf1,5)+ocp20*ia2hx(ibuf1,6)+ia2hx(ibuf1,7)
      freq=fc_bbc2freq(bits)
c
      irem = 1-and(ia2hx(ibuf2,7),1)
      ipcal = 1-and(ia2hx(ibuf2,7),2)/2
      lck = 1-(and(ia2hx(ibuf2,7),8)/8)
C
C  Pick up four bits from each character for TP
      tpi = ocp10000*ia2hx(ibuf2,3) + ocp400*ia2hx(ibuf2,4)
     .     + ocp20*ia2hx(ibuf2,5) + ia2hx(ibuf2,6)
C
      return
      end
