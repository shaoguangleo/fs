/*
 * Copyright (c) 2020-2021, 2023 NVI, Inc.
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
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "../include/params.h"
#include "../include/fs_types.h"
#include "../include/fscom.h"

extern struct fscom *shm_addr;

#define PERMISSIONS 0664
#define BUFFSIZE 512*4096

static int  write_comment(lnamef,fd,buff)
char lnamef[];
int fd;
char buff[ ];
{
  off_t offset;
  ssize_t count;
  int it[6];

  offset=lseek(fd, -1L, SEEK_END);
  if(offset == (off_t) -1) {
    perror("\007!! help! ** seeking last byte");
    return -1;
  }

  count=read(fd,buff,1);
  if(count < 0) {
    perror("\007!! help! ** reading last byte");
    return -2;
  } else if (count == 0) {
    fprintf(stderr,"\007!! help! ** couldn't read last byte for unknown reason\n");
    return -3;
  }

  if(count == 1 && buff[0] != '\n') {
    buff[0]='\n';
    errno=0;
    count=write(fd,buff,1);
    if(count < 0 || count == 0 && errno !=0) {
      perror("\007!! help! ** writing pre-comment new-line");
      return -4;
    } else if(count ==0) {
      fprintf(stderr,"\007!! help! ** couldn't add pre-comment new-line for unknown reason\n");
      return -5;
    }
  }
  rte_time(it,&it[5]);
  buff[0]='\0';
  int2str(buff,it[5],-4,1);
  strcat(buff,".");
  int2str(buff,it[4],-3,1);
  strcat(buff,".");
  int2str(buff,it[3],-2,1);
  strcat(buff,":");
  int2str(buff,it[2],-2,1);
  strcat(buff,":");
  int2str(buff,it[1],-2,1);
  strcat(buff,".");
  int2str(buff,it[0],-2,1);
  sprintf(buff+strlen(buff),":\"ddout recovered log file '%s'\n",lnamef);

  errno=0;
  count=write(fd,buff,strlen(buff));
  if(count < 0 || count == 0 && errno !=0) {
    perror("\007!! help! ** writing comment");
    return -6;
  } else if(count ==0) {
    fprintf(stderr,"\007!! help! ** couldn't add comment for unknown reason\n");
    return -7;
  }
  return 0;
}

int recover_log(lnamef,fd)
char lnamef[];
int fd;
{
  int fail, fd2;
  int before, after, seconds;
  ssize_t count, countw, cum;
  off_t size, offset;
  static char buf_copy[BUFFSIZE];

  fail=FALSE;
  fd2=open(lnamef,O_RDONLY);  /* check to see if the file exists */
  if(fd2<0 && errno == ENOENT) {
    shm_addr->abend.other_error=1;
    fprintf(stderr,"\007!! help! ** log file '%s' doesn't exist, attempting to recover by copying.\n",
	    lnamef);

    fd2 = open(lnamef, O_RDWR|O_SYNC|O_CREAT,PERMISSIONS); /* try to create it */
    if (fd2 < 0) {
      fprintf(stderr,
	      "\007!! help! ** can't create file '%s', giving up\n",
	      lnamef);
      fail=TRUE;
    } else {

      /* now try to make a copy */
      size=lseek(fd,0L,SEEK_CUR);
      if(size < 0)
	perror("\007!! help! ** determining size of old file to copy: no progress meter");
      offset=lseek(fd, 0L, SEEK_SET);
      if(offset < 0) {
	perror("\007!! help! ** rewinding old file to copy");
	fprintf(stderr,"\007!! help! ** can't rewind original file, giving up\n");
	fail=TRUE;
      } else {
	count=0;
	countw=0;
	cum=0;
	rte_rawt(&before);
	seconds=2;
	fprintf(stderr,
		"\007!! help! ** copying to recover log file '%s', please wait ... starting.\n",
		lnamef);
	while(count==countw && 0 < (count= read(fd,buf_copy,BUFFSIZE))) {
	  countw= write(fd2,buf_copy,count);
	  if(size >0) {
	    cum+=count;
	    rte_rawt(&after);
	    if((after-before)>seconds*100) {
	      fprintf(stderr,
		      "\007!! help! ** copying to recover log file '%s', please wait ... %2d%%\n",
		      lnamef, (int) (cum*100./size));
	      seconds=seconds+2;
	    }
	  }
	}
	if(count < 0) {
	  fprintf(stderr,"\007!! help! ** failed, error reading original file, giving up\n",lnamef);
	  perror("\007!! help! ** reading original file");
	  fail=TRUE;
	} else if (count!=0 && count!=countw) {
	  fprintf(stderr,"\007!! help! ** failed, error writing to '%s', giving up\n",lnamef);
	  perror("\007!! help! ** writing recovered log");
	  fail=TRUE;
	} else
	  fprintf(stderr,
		"!! help! ** copying to recover log file '%s', done.\n",
		lnamef);
      }
    }

    if(fail) {
      fprintf(stderr,"\007!! help! ** You can attempt to recover by unmounting the file system and\n");
      fprintf(stderr,"\007!! help! ** grep-ing the file system for lines starting with the date\n");
      fprintf(stderr,"\007!! help! ** portion of time tag for the date(s) of the session. Try to\n");
      fprintf(stderr,"\007!! help! ** do as little as possible to the file system until you\n");
      fprintf(stderr,"\007!! help! ** dismount it. Please see /usr2/fs/misc/logrecovery.txt for details.\n");
    } else {
      int ierr;

      fprintf(stderr,"!! help! ** good news, log file '%s' seems to be recovered, please check it.\n",lnamef);
      ierr=write_comment(lnamef,fd2,buf_copy);
      if(ierr < -5) {
        fprintf(stderr,"\007!! help! ** problem adding comment at end of recovered file, see above, may be benign\n");
        shm_addr->abend.other_error=1;
      } else if (ierr < -3) {
        fprintf(stderr,"\007!! help! ** problem adding pre-comment new-line at end of recovered file, see above, may be benign\n");
        shm_addr->abend.other_error=1;
      } else if (ierr < 0) {
        fprintf(stderr,"\007!! help! ** problem checking for new-line at end of recovered file, see above, may be benign\n");
        shm_addr->abend.other_error=1;
      } else
        fprintf(stderr,"!! help! ** recovery comment successfully added to recovered log.\n");
    }
  } else if (fd2 < 0) {
    shm_addr->abend.other_error=1;
    perror("\007!! help! ** checking for existence of log file");
  }

  if(fd2 >= 0 && close(fd2) < 0) {
    shm_addr->abend.other_error=1;
    perror("\007!! help! ** closing fd2");
  }

  return fd;
}
