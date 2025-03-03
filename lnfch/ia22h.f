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
      function ia22h(iword) 
C 
C    IA22H converts a 2-character ASCII word into a decimal number
C          between 0 and 255
C 
      integer*2 iword
C
      include '../include/boz.i'
C
      dimension ival(23)
C               - lookup table of hex values
      data ival/0,1,2,3,4,5,6,7,8,9,7*0,10,11,12,13,14,15/
C 
C     1. Get the index into lookup table by subtracting ocp57 from
C       each character.  If index is not within range set to 0. 
C 
      ind1 = jchar(iword,1)
      if (ind1.ge.ocp141.and.ind1.le.ocp146) ind1 = ind1 - ocp40
C          if lower case a ... f, change to upper
      ind2 = jchar(iword,2)
      if (ind2.ge.ocp141.and.ind2.le.ocp146) ind2 = ind2 - ocp40
C          if lower case a ... f, change to upper
      ind1 = ind1 - ocp57 
      ind2 = ind2 - ocp57 
      ia22h = -1
      if (ind1.lt.1.or.ind1.gt.23.or.ind2.lt.1.or.ind2.gt.23) return
      ia22h = ival(ind1)*16 + ival(ind2)

      return
      end 
