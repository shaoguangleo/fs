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
      subroutine ma2rp4(ibuf,irem,iby,ieq,ita,itb)

C  convert mat buffer to rp data for Mark IV drive
C 
C     This routine converts the buffers returned from the MAT 
C     into the reproduce reproduce information.
C 
C  INPUT: 
C 
      integer*2 ibuf(1) 
C      - buffers of length 5 words, as returned from MATCN
C 
C  OUTPUT:
C 
C     IBY - bypass or not 
C     IEQ - equalizer setting 
C     ITA - track A 
C     ITB - track B 
C 
C     1. The format of the tape drive control word which sets 
C     up the tracks to be reproduced is:
C
C        TPxrexbbaa
C     x = ignore info
C     r = bypass
C     e = equalizer
C    bb = stack head and track for b
C    aa = stack head and track for a
C 
C     bit 31    = local
C            30 =
C            29 = master reset
C         26-28 = 0
C            25 = Bypass mode
C         22-24 = 0
C         20-21 = Equalizer selection (0,1,2,3)
C         16-19 = 0
C         14-15 = Channel B stack select
C          8-13 = Channel B head select
C          6-7  = Channel A stack select
C          0-5  = Channel A head select
C 
      include '../include/boz.i'
C
      integer*2 itemp
C
      irem = and(ia2hx(ibuf,3),8)/8
C
      iby = and(ia2hx(ibuf,4),2)/2 
      ieq = ia2hx(ibuf,5)

      call ichmv(itemp,1,ibuf,7,1)
      ihunds = JISHFT(and(ia2hx(itemp,1),zcp0c),-2)
C
      itens = and(ia2hx(itemp,1),zcp03)
C
      call ichmv(itemp,1,ibuf,8,1)
      iones = ia2hx(itemp,1)
C
      itb=(ihunds*100)+(itens*16)+iones
C      
      call ichmv(itemp,1,ibuf,9,1)
      ihunds = JISHFT(and(ia2hx(itemp,1),zcp0c),-2)
C
      itens = and(ia2hx(itemp,1),zcp03)
C
      call ichmv(itemp,1,ibuf,10,1)
      iones = ia2hx(itemp,1)
C
      ita=(ihunds*100)+(itens*16)+iones
C 
      return
      end 
