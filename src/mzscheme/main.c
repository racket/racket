/*
  MzScheme
  Copyright (c) 2004-2007 PLT Scheme Inc.
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
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301 USA.

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

#ifdef MZ_XFORM
START_XFORM_SUSPEND;
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
#ifdef MACINTOSH_EVENTS
# ifndef OS_X
#  include "simpledrop.h"
# endif
#endif

#ifdef MZ_XFORM
END_XFORM_SUSPEND;
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
  Scheme_Thread * volatile p;
  mz_jmp_buf * volatile save, newbuf;

  p = scheme_get_current_thread();
  save = p->error_buf;
  p->error_buf = &newbuf;

  if (!scheme_setjmp(newbuf)) {
    f = scheme_builtin_value("find-system-path");
    if (f) {
      Scheme_Object *a[1];

      a[0] = scheme_intern_symbol("init-file");

      f = _scheme_apply(f, 1, a);

      if (SCHEME_PATHP(f)) {
	p->error_buf = save;
	return SCHEME_PATH_VAL(f);
      }
    }
  }
  p->error_buf = save;

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
#define INITIAL_BIN_TYPE "zi"
#define BANNER scheme_banner()
#define MZSCHEME_CMD_LINE

/*========================================================================*/
/*                        command-line parsing                            */
/*========================================================================*/

#include "cmdline.inc"

/*========================================================================*/
/*                             OSKit glue                                 */
/*========================================================================*/

#include "oskglue.inc"

/*========================================================================*/
/*                           ctl-C handler                                */
/*========================================================================*/

#ifndef NO_USER_BREAK_HANDLER

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
/*          Prepare for delayload, then call main_after_dlls              */

static int main_after_dlls(int argc, MAIN_char **MAIN_argv);

# ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
# endif

int MAIN(int argc, MAIN_char **MAIN_argv)
{
#ifdef DOS_FILE_SYSTEM
  /* Order matters: load dependencies first */
# ifndef MZ_PRECISE_GC
  load_delayed_dll(NULL, "libmzgcxxxxxxx.dll");
# endif
  load_delayed_dll(NULL, "libmzsch" DLL_3M_SUFFIX "xxxxxxx.dll");
  record_dll_path();
#endif

  return main_after_dlls(argc, MAIN_argv);
}

# ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
# endif

/************************     main_after_dlls    **************************/
/*            Phase 1 setup, then call actual_main (indirectly)           */

static int main_after_dlls(int argc, MAIN_char **MAIN_argv)
{
  void *stack_start;
  int rval;
#ifdef WINDOWS_UNICODE_MAIN
  char **argv;
#endif

  stack_start = (void *)&stack_start;

#if defined(MZ_PRECISE_GC)
  stack_start = (void *)&__gc_var_stack__;
#endif

  scheme_set_stack_base(stack_start, 1);

#if defined(OSKIT) && !defined(OSKIT_TEST) && !KNIT
  oskit_prepare(&argc, &argv);
#endif

  scheme_set_actual_main(actual_main);

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

#ifndef NO_USER_BREAK_HANDLER
  MZ_SIGSET(SIGINT, user_break_hit);
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
