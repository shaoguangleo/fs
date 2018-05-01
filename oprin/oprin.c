/* fs/oprin/oprin.c -- UFS operator input.
 * $Id$

 * Copyright (C) 1992 NASA GSFC, Ed Himwich. (weh@vega.gsfc.nasa.gov)
 * Copyright (C) 1995 Ari Mujunen. (amn@nfra.nl, Ari.Mujunen@hut.fi)

 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License.
 * See the file 'COPYING' for details.

 * $Log$
 */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>

/* For tolower, etc. */
#include <ctype.h>

/* For assert. */
#include <assert.h>

/* For open, stat, read, close, isatty. */
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

/* For malloc, free. */
#include <stdlib.h>

#include <errno.h>

/* For GNU Readline. */
#include <readline/readline.h>
#include <readline/history.h>

#include "../include/params.h"
#include "../include/fs_types.h"
#include "../include/fscom.h"

#include "readl.h"

/* External FS variables. */
extern struct fscom *shm_addr;

/* External FS functions, perhaps these should eventually go into a '.h'? */
extern void setup_ids(void);
extern void sig_ignore(void);
extern void cls_snd(long *class,
		    char *buffer,
		    int length,
		    int parm3,
		    int parm4);
extern void skd_run(char name[5], char w, long ip[5]);

static long ipr[5] = { 0, 0, 0, 0, 0};

/* Our prompt at the beginning of a line. */
static char prompt[] = ">";

/* SNAP command that
    - gets recommended to a user who presses ctrl-D (EOF) at the beginning
      of input line to "logout" ie. quit */
static char termination_command[] = "end_oprin";


/* Set by command line flag. Determines if oprin is started with the field system or 
 * later.
 * 
 * Always == 0 when FS build with server/client
 */
int fs_internal = 0;


/* sent to boss */
void run_snap(char *cmd) {
	static int kfirst = 1;
	while (kfirst && shm_addr->iclopr == -1)
		rte_sleep(2);
	kfirst = 0;

	if (nsem_test("fs   ") != 1 && !fs_internal) {
		fprintf(stderr, "FS no longer running.\n");
		exit(0);
	}
	int len = strlen(cmd);
	cls_snd(&(shm_addr->iclopr), cmd, len, 0, 0);
	skd_run("boss ", 'n', ipr);
	return;
}

/* Oprin commands */

void client_command(const char *cmd) {
    static FILE* f = NULL;

    if (!f) {
        char* fdstr = getenv("FS_CLIENT_PIPE_FD");

        if (!fdstr || !*fdstr ) {
            fprintf(stderr, "oprin: connected to fsclient");
            return;
        }
        f = fdopen(atoi(fdstr), "w");
        if (!f) {
            perror("oprin: error opening pipe");
            return;
        }
    }

    fprintf(f, "%s\n", cmd);
    fflush(f);
}

void end_oprin(const char *arg) {
	if (fs_internal) {
		fprintf(stderr,
		        "The '%s' command is only available from a stand-alone "
		        "'oprin'.\nUse 'terminate' to stop the field system.\n",
		        termination_command);
		return;
	}
	exit(EXIT_SUCCESS);
}

void terminate(const char *arg) {
    /* without the display server, run this command as usual */
#ifdef FS_NO_DISPLAY_SERVER
        run_snap("terminate");
#endif

	if (!arg || !arg[0]) {
		fprintf(stderr,
		        "You are running FS client. Use \"terminate=fs\" or \"terminate=client\"\n");
		return;
	}

	if (strcmp(arg, "client") == 0) {
        /* TODO: actually kill the client here */
		exit(EXIT_SUCCESS);
	}

	if (strcmp(arg, "fs") == 0) {
            run_snap("terminate");
	}
}

/* contents of cmd after first '=' are handed to the function in arg */

static const struct cmd local_commands[] = {
    {"end_oprin", end_oprin},
    {"client", client_command},
/*    {"terminate", terminate}, */
    {NULL, NULL},
};

/* Tries to run a local command. Returns true on if the command exists */
int try_local_command(const char *s) {
	char *str = strdup(s);

	char *cmd = str;
	while (isspace(*cmd)) cmd++;

	char *arg = strchr(cmd, '=');
	if (arg) *arg++ = '\0';

	int ret = 0;
	unsigned int i;

	const struct cmd* lcmd = local_commands;

	for (lcmd = local_commands; lcmd->name; lcmd++) {
		if (strcmp(lcmd->name, cmd) == 0) {
			lcmd->cmd(arg);
			ret = 1;
			break;
		}
	}
	free(str);
	return ret;
}

/* 'oprin' main. */

int
main(int argc, char **argv)
{
  char *input;
  char *previous_input;
  int length;
  int kfirst=1;

  setup_ids();
  sig_ignore();

#ifdef FS_NO_DISPLAY_SERVER
  fs_internal = argc == 2 && 0 == strcmp("-fs_internal", argv[1]);
#endif

  if(nsem_test("fs   ") != 1 && !fs_internal) {
    fprintf(stderr, "FS not running.\n");
    exit(0);
  }

  initialize_readline(local_commands);
  /* TODO: add local commands to readline tab completetion */

  previous_input = NULL;
  while (1) {

    if(nsem_test("fs   ") != 1 && !fs_internal) {
      fprintf(stderr, "FS no longer running.\n");
      exit(0);
    }

    input = readline(prompt);

    if(nsem_test("fs   ") != 1 && !fs_internal) {
      if(input == NULL)
      fprintf(stderr, "\n");
      fprintf(stderr, "FS no longer running.\n");
      exit(0);
    }

    /* After a user has completed a new input line,
       get rid of the previous one (if it exists). */
    if (previous_input) {
      free(previous_input);
      previous_input = NULL;
    }

    if (input == NULL) {
      /* We have EOF ie. ctrl-D at the beginning of a line. */

      /* If file input, we give up and pretend we have got a
         termination command. */
      if (!isatty(STDIN_FILENO)){
	input = dupstr(termination_command);
      }
    }
    if (input == NULL) {
      /* EOF-warning at interactive terminals only. */
      if (isatty(STDERR_FILENO)) {
	if(!fs_internal) {
	  fprintf(stderr, "Use '%s' to stop this instance of oprin.\n",
		 termination_command);
	}
	fprintf(stderr, "Use 'terminate' to stop the field system.\n");
      }
      continue;  /* no further actions (ie. 'free()') required */
    }
    
    
    /* Now we have got something that's not EOF,
       perhaps an empty line or a real command. */

    if ((length = strlen(input))) {
         /* 'readline()' removes the terminating newline. */

      /* We have at least one character. Let's add the line to the history. */

      add_history(input);

      /* oprin commands override fs commands */
      if (try_local_command(input)) {
          previous_input = input;
          continue;
      }

      /* Execute this SNAP command via "boss". */
      /* wait for iclopr to be defined on the first time through */
      while (kfirst && shm_addr->iclopr==-1)
	rte_sleep(2);
      kfirst=0;
      if(nsem_test("fs   ") != 1 && !fs_internal) {
	fprintf(stderr, "FS no longer running.\n");
	exit(0);
      }
      cls_snd( &(shm_addr->iclopr), input, length, 0, 0);
      skd_run("boss ",'n',ipr);

    }  /* if have at least one character */

    /* Instead of 'free()'ing the input line buffer straight away,
       we store it to be freed _after_ the user has completed
       the next input line.  In this way "boss" (et al) can safely
       access the previous buffer.  (Might be unnecessary, since
       'cls_snd()' apparently makes a copy of the command line.) */
    previous_input = input;
  }  /* while forever */

  return (0);  /* ok termination, actually never reached */
  /* Command tables actually never get deallocated,
     but they'll vanish when the 'oprin' process vanishes. */
}  /* main of oprin */
