/*
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
 */
/* rte_sett.c - set FS and optionally system time */

#include <sys/types.h>
#include <sys/times.h>
#include <time.h>
#include <string.h>

#include "../include/params.h"
#include "../include/fs_types.h"
#include "../include/fscom.h"
#include "../include/shm_addr.h"

/*
 * oFmClock - formatter time
 * oFmHs  - centiseconds to go with oFmClock
 * lCentiSec - raw cpu time corresponding to (oFmClock,oFmHs)
 * mode - 'cpu' to set cpu time
 *        'offset' to reset offset
 *        'rate'   to measure rate
 *
 * ierr non-zero only if mode is 's' and stime fails
 */

int rte_secs();

int rte_sett( oFmClock, iFmHs, lCentiSec, sMode)
//time_t oFmClock;
int    oFmClock;
int iFmHs;
int lCentiSec;
char *sMode;
{
    int iIndex, ierr;
    char model;
    int iComputer;

    iIndex = 01 & shm_addr->time.index;
    model = shm_addr->time.model;
    iComputer= shm_addr->time.icomputer[iIndex];

    if (!strcmp(sMode,"cpu")) {
        struct timespec tp;
        rte_sleep(25);
        tp.tv_sec=oFmClock;
        tp.tv_nsec=0;
        ierr=0;
// unused, trapped by caller since it would require root permission
//
// untested, for Bullseye and later:
//        ierr=clock_settime(CLOCK_REALTIME,&tp);
// was before Bullseye:
//        ierr=stime(&oFmClock);
//
//        if(ierr)
//          perror("setting system time");
        shm_addr->time.secs_off = rte_secs();
    } else {
       time_t oCpuClock;
       int iCpuHs;
       int lEpoch, lOffset, lDiffHs, lSpan;
       float fRate;

       oCpuClock=lCentiSec/100+shm_addr->time.secs_off;
       iCpuHs = lCentiSec % 100;
       lDiffHs = (oFmClock-oCpuClock)*100+iFmHs-iCpuHs;

       lEpoch = shm_addr->time.epoch[iIndex];
       fRate = shm_addr->time.rate[iIndex];
       lOffset = shm_addr->time.offset[iIndex];
       lSpan = shm_addr->time.span[iIndex];

       if(!strcmp(sMode,"offset")) {
                  	   /* don't update rate, but save the other stuff */
         lEpoch=lCentiSec;
         lOffset = lDiffHs;
       } else if (!strcmp(sMode,"adapt") || !strcmp(sMode,"rate") ) {
					/* update the rate */
         lSpan = lCentiSec-lEpoch;
         fRate=((double)(lDiffHs-lOffset))/lSpan;
       }

       iIndex = 01 & ~iIndex;
       shm_addr->time.rate[iIndex] = fRate;
       shm_addr->time.span[iIndex] = lSpan;
       shm_addr->time.offset[iIndex] = lOffset;
       shm_addr->time.epoch[iIndex] = lEpoch;
       shm_addr->time.icomputer[iIndex] = iComputer;
       shm_addr->time.index = iIndex;

       ierr=0;
    }

    return ierr;
}
