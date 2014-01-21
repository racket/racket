/*
  Racket
  Copyright (c) 2004-2014 PLT Design Inc.
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

/* This file defines Racket's main(), which is a jumble of
   platform-specific initialization. The included file "cmdline.inc"
   implements command-line parsing. (GRacket also uses "cmdline.inc".)

   The rest of the source code resides in the `src' subdirectory
   (except for the garbage collector, which is in `gc', `sgc', or
   `gc2', depending on which one you're using). */

#define __MINGW32_DELAY_LOAD__ 1
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

#if defined(MZ_XFORM) && defined(__MINGW32__) && !defined(USE_THREAD_LOCAL)
XFORM_GC_VARIABLE_STACK_THROUGH_DIRECT_FUNCTION;
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

#ifdef INSTRUMENT_PRIMITIVES 
extern int g_print_prims;
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

#ifndef UNIX_INIT_FILENAME
# define UNIX_INIT_FILENAME "~/.racketrc"
# define WINDOWS_INIT_FILENAME "%%HOMEDIRVE%%\\%%HOMEPATH%%\\racketrc.rktl"
# define MACOS9_INIT_FILENAME "PREFERENCES:racketrc.rktl"
# define GET_INIT_FILENAME get_init_filename
# define PRINTF printf
# define PROGRAM "Racket"
# define PROGRAM_LC "racket"
# define INITIAL_BIN_TYPE "zi"
# define RACKET_CMD_LINE
# define INITIAL_NAMESPACE_MODULE "racket/init"
#endif

#ifdef EXPAND_FILENAME_TILDE
# define INIT_FILENAME UNIX_INIT_FILENAME
#else
# ifdef DOS_FILE_SYSTEM
#  define INIT_FILENAME WINDOWS_INIT_FILENAME
# else
#  define INIT_FILENAME MACOS9_INIT_FILENAME
# endif
#endif

#define CMDLINE_FFLUSH fflush

#define BANNER scheme_banner()

/*========================================================================*/
/*                            OS process name                             */
/*========================================================================*/

#if defined(linux)
# include <sys/prctl.h>
# ifdef PR_SET_NAME
#  define CAN_SET_OS_PROCESS_NAME 1
void set_os_process_name(char *sprog)
{
  int i = strlen(sprog) - 1;
  while (i && (sprog[i - 1] != '/')) {
    --i;
  }
  prctl(PR_SET_NAME, sprog + i);
}
# endif
#endif

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

#if !defined(NO_USER_BREAK_HANDLER) || defined(DOS_FILE_SYSTEM)

static void *break_handle;
static void *signal_handle;

# ifndef NO_USER_BREAK_HANDLER

static void user_break_hit(int ignore)
  XFORM_SKIP_PROC
{
  scheme_break_main_thread_at(break_handle);
  scheme_signal_received_at(signal_handle);

#  ifdef SIGSET_NEEDS_REINSTALL
  MZ_SIGSET(SIGINT, user_break_hit);
#  endif
}

# ifndef NO_SIGTERM_HANDLER
static void term_hit(int ignore)
  XFORM_SKIP_PROC
{
  scheme_break_kind_main_thread_at(break_handle, MZEXN_BREAK_TERMINATE);
  scheme_signal_received_at(signal_handle);

#  ifdef SIGSET_NEEDS_REINSTALL
  MZ_SIGSET(SIGTERM, term_hit);
#  endif
}
# endif

# ifndef NO_SIGHUP_HANDLER
static void hup_hit(int ignore)
  XFORM_SKIP_PROC
{
  scheme_break_kind_main_thread_at(break_handle, MZEXN_BREAK_HANG_UP);
  scheme_signal_received_at(signal_handle);

#  ifdef SIGSET_NEEDS_REINSTALL
  MZ_SIGSET(SIGHUP, hup_hit);
#  endif
}
# endif

# endif

# ifdef DOS_FILE_SYSTEM
static BOOL WINAPI ConsoleBreakHandler(DWORD op)
{
  scheme_break_main_thread_at(break_handle);
  scheme_signal_received_at(signal_handle);
  return TRUE;
}
#endif

#endif

/*========================================================================*/
/*                                 main                                   */
/*========================================================================*/

#ifdef USE_SENORA_GC
# include "sgc/sgc.h"
#endif

/* Forward declarations: */
static void do_scheme_rep(Scheme_Env *, FinishArgs *f);
static int cont_run(FinishArgs *f);

#if defined(__MINGW32__)
# define MAIN zmain
# define MAIN_char char
# define MAIN_argv argv
#elif defined(WINDOWS_UNICODE_SUPPORT) && !defined(__CYGWIN32__) && !defined(MZ_DEFINE_UTF8_MAIN)
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
static int main_after_stack(void *data);

# ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
# endif

#if defined(__MINGW32__) || defined(WINMAIN_ALREADY)
# include "parse_cmdl.inc"
#endif

#ifdef IMPLEMENT_THREAD_LOCAL_VIA_WIN_TLS
extern intptr_t _tls_index;
# ifdef __MINGW32__
static __thread void *tls_space;
# else
static __declspec(thread) void *tls_space;
# endif
#endif

#ifdef DOS_FILE_SYSTEM
void load_delayed()
{
  (void)SetErrorMode(SEM_FAILCRITICALERRORS);

  /* Order matters: load dependencies first */
# ifndef MZ_PRECISE_GC
  load_delayed_dll(NULL, "libmzgcxxxxxxx.dll");
# endif
  load_delayed_dll(NULL, "libracket" DLL_3M_SUFFIX "xxxxxxx.dll");
  record_dll_path();
# ifdef IMPLEMENT_THREAD_LOCAL_VIA_WIN_TLS
#  ifdef __MINGW32__
  {
    /* gcc declares space for the thread-local variable in a way that
       the OS can set up, but its doesn't actually map variables
       through the OS-supplied mechanism. Just assume that the first
       thread-local variable is ours. */
    void **base;
#  ifdef _WIN64
    asm("mov %%gs:(0x58), %0;" :"=r"(base));
#  else
    asm("mov %%fs:(0x2C), %0;" :"=r"(base));
#  endif
    scheme_register_tls_space(*base, _tls_index);
  }
#  else
  scheme_register_tls_space(&tls_space, _tls_index);
#  endif
# endif
}
#endif

int MAIN(int argc, MAIN_char **MAIN_argv)
{
#if defined(DOS_FILE_SYSTEM) && !defined(__MINGW32__)
  load_delayed();
#endif

  return main_after_dlls(argc, MAIN_argv);
}

#if defined(__MINGW32__) && !defined(WINMAIN_ALREADY)
int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR ignored, int nCmdShow)
{
  int argc;
  char **argv;

  load_delayed();

  scheme_set_atexit(atexit);

  argv = cmdline_to_argv(&argc, NULL);

  return zmain(argc, argv);
}
#endif

# ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
# endif

/************************     main_after_dlls    **************************/
/*        Prep stack for GC, then call main_after_stack (indirectly)      */

typedef struct {
  int argc;
  MAIN_char **argv;
} Main_Args;

# ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
# endif

static int main_after_dlls(int argc, MAIN_char **argv)
{
  Main_Args ma;
  ma.argc = argc;
  ma.argv = argv;
  return scheme_main_stack_setup(1, main_after_stack, &ma);
}

# ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
# endif

/************************     main_after_stack    *************************/
/*               Setup, parse command-line, and go to cont_run            */

static int main_after_stack(void *data)
{
  int rval;
  int argc;
  MAIN_char **MAIN_argv;
#ifdef WINDOWS_UNICODE_MAIN
  char **argv;
#endif

  argc = ((Main_Args *)data)->argc;
  MAIN_argv = ((Main_Args *)data)->argv;

#if defined(OSKIT) && !defined(OSKIT_TEST) && !KNIT
  oskit_prepare(&argc, &argv);
#endif

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


#if !defined(NO_USER_BREAK_HANDLER) || defined(DOS_FILE_SYSTEM)
  break_handle = scheme_get_main_thread_break_handle();
  signal_handle = scheme_get_signal_handle();
# ifndef NO_USER_BREAK_HANDLER
  MZ_SIGSET(SIGINT, user_break_hit);
#  ifndef NO_SIGTERM_HANDLER
  MZ_SIGSET(SIGTERM, term_hit);
#  endif
#  ifndef NO_SIGHUP_HANDLER
  MZ_SIGSET(SIGHUP, hup_hit);
#  endif
# endif
# ifdef DOS_FILE_SYSTEM
  SetConsoleCtrlHandler(ConsoleBreakHandler, TRUE);      
# endif
#endif

#ifdef PRE_FILTER_CMDLINE_ARGUMENTS
  pre_filter_cmdline_arguments(&argc, &MAIN_argv);
#endif

  rval = run_from_cmd_line(argc, argv, scheme_basic_env, cont_run);

#ifndef DEFER_EXPLICIT_EXIT
  scheme_immediate_exit(rval);
  /* shouldn't get here */
#endif

  return rval;
}

/*************************      cont_run     ******************************/
/*                          Go to do_scheme_rep                           */

static int cont_run(FinishArgs *f)
{
  return finish_cmd_line_run(f, do_scheme_rep);
}

/*************************   do_scheme_rep   *****************************/
/*                  Finally, do a read-eval-print-loop                   */

static void do_scheme_rep(Scheme_Env *env, FinishArgs *fa)
{
  /* enter read-eval-print loop */
  Scheme_Object *rep, *a[2];
  int ending_newline = 1;

#ifdef GRAPHICAL_REPL
  if (!fa->a->alternate_rep) {
    a[0] = scheme_intern_symbol("racket/gui/init");
    a[1] = scheme_intern_symbol("graphical-read-eval-print-loop");
    ending_newline = 0;
  } else
#endif
    {
      a[0] = scheme_intern_symbol("racket/base");
      a[1] = scheme_intern_symbol("read-eval-print-loop");
    }

  rep = scheme_dynamic_require(2, a);
    
  if (rep) {
    scheme_apply(rep, 0, NULL);
    if (ending_newline)
      printf("\n");
  }
}

/*========================================================================*/
/*                         win32 manifest                                 */
/*========================================================================*/

#if _MSC_VER >= 1400
#pragma comment(linker,"/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='*' publicKeyToken='6595b64144ccf1df' language='*'\"")
#endif

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
