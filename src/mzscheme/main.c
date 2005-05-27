/*
  MzScheme
  Copyright (c) 2004-2005 PLT Scheme, Inc.
  Copyright (c) 1995-2000 Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

/* This file defines MzScheme's main(), which is a jumble of
   platform-specific initialization. The included file "cmdline.inc"
   implements command-line parsing. (MrEd also uses "cmdline.inc".)

   The rest of the source code resides in the `src' subdirectory
   (except for the garbage collector, which is in `gc', `sgc', or
   `gc2', depending on which one you're using). */

#include "scheme.h"

/*========================================================================*/
/*                       configuration and includes                       */
/*========================================================================*/

/* #define STANDALONE_WITH_EMBEDDED_EXTENSION */
/*    STANDALONE_WITH_EMBEDDED_EXTENSION builds an executable with
      built-in extensions. The extension is initialized by calling
      scheme_initialize(env), where `env' is the initial environment.
      By default, command-line parsing, the REPL, and initilization
      file loading are turned off. */

#ifdef STANDALONE_WITH_EMBEDDED_EXTENSION
# define DONT_PARSE_COMMAND_LINE
# define DONT_RUN_REP
# define DONT_LOAD_INIT_FILE
#endif

#ifdef FILES_HAVE_FDS
# include <sys/types.h>
# include <sys/time.h>
# ifdef SELECT_INCLUDE
#  include <sys/select.h>
# endif
#endif
#ifndef NO_USER_BREAK_HANDLER
# include <signal.h>
#endif
#ifdef UNISTD_INCLUDE
# include <unistd.h>
#endif
#ifdef MACINTOSH_EVENTS
# ifndef OS_X
#  include <Events.h>
# endif
#endif
#ifdef MACINTOSH_SIOUX
# include <console.h>
# include <SIOUX.h>
#endif
#ifdef MACINTOSH_SET_STACK
# include <Memory.h>
#endif
#ifdef MACINTOSH_EVENTS
# ifndef OS_X
#  include "simpledrop.h"
# endif
#endif

#ifdef WIN32_THREADS
/* Only set up for Boehm GC that thinks it's a DLL: */
# include <windows.h>
# define GC_THINKS_ITS_A_DLL_BUT_ISNT
#endif
#ifdef GC_THINKS_ITS_A_DLL_BUT_ISNT
extern BOOL WINAPI DllMain(HINSTANCE inst, ULONG reason, LPVOID reserved);
#endif

/*========================================================================*/
/*                configuration for command-line parsing                  */
/*========================================================================*/

#ifndef DONT_LOAD_INIT_FILE
static char *get_init_filename(Scheme_Env *env)
{
  Scheme_Object *f;

  if (!scheme_setjmp(scheme_error_buf)) {
    f = scheme_builtin_value("find-system-path");
    if (f) {
      Scheme_Object *a[1];

      a[0] = scheme_intern_symbol("init-file");

      f = _scheme_apply(f, 1, a);

      if (SCHEME_PATHP(f))
	return SCHEME_PATH_VAL(f);
    }
  }

  return NULL;
}
#endif

#ifdef STANDALONE_WITH_EMBEDDED_EXTENSION
extern Scheme_Object *scheme_initialize(Scheme_Env *env);
#endif

#ifdef EXPAND_FILENAME_TILDE
# define INIT_FILENAME "~/.mzschemerc"
#else
# ifdef DOS_FILE_SYSTEM
#  define INIT_FILENAME "%%HOMEDRIVE%%\\%%HOMEPATH%%\\mzschemerc.ss"
# else
#  define INIT_FILENAME "PREFERENCES:mzschemerc.ss"
# endif
#endif
#define GET_INIT_FILENAME get_init_filename
#define PRINTF printf
#define PROGRAM "MzScheme"
#define PROGRAM_LC "mzscheme"
#define BANNER scheme_banner()
#define MZSCHEME_CMD_LINE

/*========================================================================*/
/*                        command-line parsing                            */
/*========================================================================*/

#include "cmdline.inc"

/*========================================================================*/
/*                            MacOS glue                                  */
/*========================================================================*/

#include "macglue.inc"

/*========================================================================*/
/*                             OSKit glue                                 */
/*========================================================================*/

#include "oskglue.inc"

/*========================================================================*/
/*                           ctl-C handler                                */
/*========================================================================*/

#ifndef NO_USER_BREAK_HANDLER
# ifndef MAC_MZ_GUI_ENABLED

static void user_break_hit(int ignore)
{
  scheme_break_thread(NULL);
  scheme_signal_received();

#  ifdef SIGSET_NEEDS_REINSTALL
  MZ_SIGSET(SIGINT, user_break_hit);
#  endif
#  ifdef MZ_PRECISE_GC
#   ifndef GC_STACK_CALLEE_RESTORE
  /* Restore variable stack. */
  GC_variable_stack = (void **)__gc_var_stack__[0];
#   endif
#  endif
}

# endif
#endif

#if defined(_IBMR2)
static void dangerdanger(int ignored)
{
  char *s = "mzscheme: Danger - paging space low\n";
  write(2, s, strlen(s));
  scheme_collect_garbage();
}
#endif

/*========================================================================*/
/*                                 main                                   */
/*========================================================================*/

#ifdef USE_SENORA_GC
# include "sgc/sgc.h"
#endif

/* Forward declarations: */
static void do_scheme_rep(Scheme_Env *);
static int cont_run(FinishArgs *f);
int actual_main(int argc, char *argv[]);

#if defined(WINDOWS_UNICODE_SUPPORT) && !defined(__CYGWIN32__)
# define MAIN wmain
# define MAIN_char wchar_t
# define MAIN_argv wargv
# define WINDOWS_UNICODE_MAIN
#else
# define MAIN main
# define MAIN_char char
# define MAIN_argv argv
#endif

/*****************************     main    ********************************/
/*            Phase 1 setup, then call actual_main (indirectly)           */

int MAIN(int argc, MAIN_char **MAIN_argv)
{
  void *stack_start;
  int rval;
#ifdef WINDOWS_UNICODE_MAIN
  char **argv;
#endif

  stack_start = (void *)&stack_start;

#if defined(MZ_PRECISE_GC)
  stack_start = (void *)&__gc_var_stack__;
  GC_init_type_tags(_scheme_last_type_, scheme_weak_box_type);
#endif

  scheme_set_stack_base(stack_start, 1);

#if defined(OSKIT) && !defined(OSKIT_TEST) && !KNIT
  oskit_prepare(&argc, &argv);
#endif

  scheme_actual_main = actual_main;

#ifdef WINDOWS_UNICODE_MAIN
 {
   char *a;
   int i, j, l;
   argv = (char **)malloc(sizeof(char*)*argc);
   for (i = 0; i < argc; i++) {
     for (j = 0; wargv[i][j]; j++) {
     }
     l = scheme_utf8_encode((unsigned int*)wargv[i], 0, j, 
			    NULL, 0,
			    1 /* UTF-16 */);
     a = malloc(l + 1);
     scheme_utf8_encode((unsigned int *)wargv[i], 0, j, 
			(unsigned char *)a, 0,
			1 /* UTF-16 */);
	 a[l] = 0;
     argv[i] = a;
   }
 }
#endif

  rval = scheme_image_main(argc, argv); /* calls actual_main */

  /* This line ensures that __gc_var_stack__ is the
     val of GC_variable_stack in scheme_image_main. */
  argv = NULL;
  return rval;
}


/*************************     actual_main    *****************************/
/*      Phase 2 setup, then parse command-line and go to cont_run         */

int actual_main(int argc, char *argv[])
{
  int exit_val;

#ifdef MACINTOSH_SET_STACK
  long calcLimit;
  THz zone;
	
  /* 1 MB stack: */
  zone = GetZone();
  calcLimit = ((long)LMGetCurStackBase()-(*(long *)zone)-sizeof(Zone)) - 1048576;
  if (calcLimit % 2)
    calcLimit++;
  SetApplLimit((Ptr)((*(long *)zone)+sizeof(Zone)+calcLimit));
#endif

#ifdef MAC_MZ_GUI_ENABLED
  MaxApplZone();
	
  InitGraf(&qd.thePort);		/* initialize Mac stuff */
  InitFonts();
  InitWindows();
  InitMenus();
  TEInit();
  InitDialogs(NULL);
  
  MoreMasters();
  MoreMasters();
	
# ifdef MACINTOSH_SIOUX
  SIOUXSettings.initializeTB = 0;
# endif

  scheme_handle_aewait_event = handle_one;

  scheme_sleep = MacSleep;

  GC_out_of_memory = MacOutOfMemory;

  Drop_GetArgs(&argc, &argv);
#endif // MAC_MZ_GUI_ENABLED

#ifdef MACINTOSH_SIOUX
  { 
    KeyMap keys;
    GetKeys(keys);
    if (keys[1] & 32768L) { /* Cmd key down */
	    int argc2;
	    char **argv2;
	    argc2 = ccommand(&argv2);
	    if (argc2 > 1) {
	      int i, j;
	      char **both = (char **)malloc(sizeof(char *) * (argc + argc2 - 1));
	      for (i = 0; i < argc; i++) {
	        both[i] = argv[i];
	      }
	      for (j = 1; j < argc2; j++, i++) {
	        both[i] = argv2[j];
	      }
	        
	      argv = both;
	      argc += argc2 - 1;
	    }
	}
  }
  
  SIOUXSettings.autocloseonquit = 0;
  SIOUXSettings.asktosaveonclose = 0;
#endif

#ifndef NO_USER_BREAK_HANDLER
# ifndef MAC_MZ_GUI_ENABLED
  MZ_SIGSET(SIGINT, user_break_hit);
# else
  scheme_check_for_break = check_break_flag;
# endif
#endif

  exit_val = run_from_cmd_line(argc, argv, scheme_basic_env, cont_run);

  scheme_immediate_exit(exit_val);
  return exit_val; /* shouldn't happen! */
}

/*************************       cont_run     *****************************/
/*              Phase 3 setup (none), then go to do_scheme_rep            */

static int cont_run(FinishArgs *f)
{
  return finish_cmd_line_run(f, do_scheme_rep);
}

/*************************   do_scheme_rep   *****************************/
/*              Finally, do a read-eval-print-loop                       */

static void do_scheme_rep(Scheme_Env *env)
{
#ifndef NO_USER_BREAK_HANDLER
# ifdef MAC_MZ_GUI_ENABLED
  scheme_set_param(scheme_config, MZCONFIG_ENABLE_BREAK, scheme_true);
# endif
#endif

  /* enter read-eval-print loop */
  {
    Scheme_Object *rep;
    rep = scheme_builtin_value("read-eval-print-loop");
    if (rep) {
      scheme_apply(rep, 0, NULL);
      printf("\n");
    }
  }
}

/*========================================================================*/
/*                         junk for testing                               */
/*========================================================================*/

#if 0
/* For testing STANDALONE_WITH_EMBEDDED_EXTENSION */
Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  return scheme_eval_string("(lambda (v) (and (eq? v #t) "
			    "  (lambda () "
			    "    (printf \"These were the args: ~a~n\" argv))))", 
			    env);
}
#endif
