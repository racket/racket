/*
  Racket
  Copyright (c) 2004-2012 PLT Scheme Inc.
  Copyright (c) 1995-2001 Matthew Flatt

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

/* This file implements the most platform-specific aspects of Racket
   port types, which means it deals with all the messy FILE and file
   descriptor issues, as well as implementing TCP. Also, `subprocess'
   is implemented here, since much of the work has to do with
   ports. */

#include "schpriv.h"
#include "schmach.h"
#ifdef UNISTD_INCLUDE
# include <unistd.h>
#endif
#ifdef USE_ULIMIT
# include <ulimit.h>
#endif
#ifdef FILES_HAVE_FDS
# include <fcntl.h>
# include <sys/types.h>
# include <sys/time.h>
# include <sys/file.h>
# ifdef BSTRING_INCLUDE
#  include <bstring.h>
# endif
# ifdef SELECT_INCLUDE
#  include <sys/select.h>
# endif
# ifdef HAVE_POLL_SYSCALL
#  include <poll.h>
# endif
#endif
#ifdef USE_ITIMER
# include <sys/types.h>
# include <sys/time.h>
# include <signal.h>
#endif
#if defined(UNIX_PROCESSES)
# include <signal.h>
# include <sys/types.h>
# include <sys/wait.h>
#endif
#ifdef IO_INCLUDE
# include <io.h>
#endif
#ifdef NO_ERRNO_GLOBAL
static int mzerrno = 0;
# define errno mzerrno
#else
# include <errno.h>
#endif
#ifndef DONT_IGNORE_PIPE_SIGNAL
# include <signal.h>
#endif
#ifdef USE_OSKIT_CONSOLE
# ifndef OSKIT_TEST
#  include <x86/pc/direct_cons.h>
# endif
extern int osk_not_console; /* set by cmd-line flag */
#endif
#include <math.h> /* for fmod , used by default_sleep */

#ifndef MZ_BINARY
# define MZ_BINARY 0
#endif

#define mzAssert(x) /* if (!(x)) abort() */

/******************** Generic FILEs ********************/

typedef struct {
  MZTAG_IF_REQUIRED
  FILE *f;
} Scheme_Input_File;

typedef struct {
  MZTAG_IF_REQUIRED
  FILE *f;
} Scheme_Output_File;

/******************** Windows I/O and Subprocesses ********************/

#if defined(WINDOWS_PROCESSES) || defined(WINDOWS_FILE_HANDLES)

static void init_thread_memory();

# define WIN32_FD_HANDLES
# include <winsock2.h>
# include <windows.h>
# include <process.h>
# include <signal.h>
# include <io.h>
# include <fcntl.h>
# define OS_SEMAPHORE_TYPE HANDLE
# define OS_MUTEX_TYPE CRITICAL_SECTION
# define OS_THREAD_TYPE HANDLE
#endif

#include "schfd.h"

#ifdef WINDOWS_FILE_HANDLES

# define MZ_FDS

typedef struct Win_FD_Input_Thread {
  /* This is malloced for use in a Win32 thread */
  HANDLE fd;
  volatile int avail, err, checking;
  int *refcount;
  HANDLE eof;
  unsigned char *buffer;
  HANDLE checking_sema, ready_sema, you_clean_up_sema;
  HANDLE thread;
} Win_FD_Input_Thread;

typedef struct Win_FD_Output_Thread {
  /* This is malloced for use in a Win32 thread */
  HANDLE fd;
  int nonblocking;  /* non-zero => an NT pipe where non-blocking WriteFile
		       works. We still use a thread to detect when the
		       write has ben flushed, which in turn is needed to
		       know whether future writes will immediately succeed. */
  volatile flushed, needflush; /* Used for non-blocking, only. The flushed
				  flag communicates from the flush-testing thread
				  to the main thread. For efficiency, we request
				  flush checking only when needed (instead of
				  after every write); needflush indicates that
				  a flush check is currently needed, but hasn't
				  been started. */
  volatile int done, err_no;
  volatile unsigned int buflen, bufstart, bufend; /* used for blocking, only */
  unsigned char *buffer; /* used for blocking, only */
  int *refcount;
  HANDLE lock_sema, work_sema, ready_sema, you_clean_up_sema;
  /* lock_sema protects the fields, work_sema starts the flush or
     flush-checking thread to work, ready_sema indicates that a flush
     finished, and you_clean_up_sema is essentially a reference
     count */
  HANDLE thread;
} Win_FD_Output_Thread;

int scheme_stupid_windows_machine;

#endif

#if defined(WINDOWS_PROCESSES)
# include <ctype.h>
#endif

/******************** Unix Subprocesses ********************/

#if defined(UNIX_PROCESSES) && !defined(MZ_PLACES_WAITPID)
/* For process & system: */
typedef struct System_Child {
  MZTAG_IF_REQUIRED
  pid_t id;
  short done;
  int status;
  struct System_Child *next;
} System_Child;

System_Child *scheme_system_children;
#endif

typedef struct Scheme_Subprocess {
  Scheme_Object so;
  void *handle;
  int pid;
  int is_group;
#if defined(MZ_PLACES_WAITPID)
  short done;
  int status;
#endif
  Scheme_Custodian_Reference *mref;
} Scheme_Subprocess;

#ifdef USE_FD_PORTS
# include <fcntl.h>
# include <sys/stat.h>
# define MZ_FDS
#endif

#ifdef CLOSE_ALL_FDS_AFTER_FORK
static void close_fds_after_fork(int skip1, int skip2, int skip3);
#endif

/******************** refcounts ********************/

#if defined(WINDOWS_FILE_HANDLES) || defined(MZ_USE_PLACES)
# define MZ_LOCK_REFCOUNTS
static mzrt_mutex *refcount_mutex;
#endif

static int *malloc_refcount(int val, int free_on_zero)
{
  int *rc;

#ifdef MZ_LOCK_REFCOUNTS
  if (!refcount_mutex)
    mzrt_mutex_create(&refcount_mutex);
#endif

  rc = (int *)malloc(2 * sizeof(int));
  *rc = val;
  rc[1] = free_on_zero;

  return rc;
}

static int adj_refcount(int *refcount, int amt)
  XFORM_SKIP_PROC
{
  int rc;

  if (!refcount)
    return 0;

#ifdef MZ_LOCK_REFCOUNTS
  mzrt_mutex_lock(refcount_mutex);
#endif
  if (amt > 0) {
    /* don't increment up from 0 */
    if (*refcount)
      *refcount += amt;
  } else
    *refcount += amt;
  rc = *refcount;
#ifdef MZ_LOCK_REFCOUNTS
  mzrt_mutex_unlock(refcount_mutex);
#endif

  if (!rc && refcount[1])
    free(refcount);

  return rc;
}

/******************** file-descriptor I/O ********************/

/* Windows/Mac I/O is piggy-backed on Unix file-descriptor I/O.  Making
   Windows file HANDLEs behave as nicely as file descriptors for
   non-blocking I/O requires a lot of work, and often a separate
   thread. The "th" and "oth" fields of Scheme_FD point to malloced
   (non-GCed) records that mediate the threads. */

#ifdef MZ_FDS

static int *stdin_refcount, *stdout_refcount, *stderr_refcount;

# define MZPORT_FD_BUFFSIZE 4096
# define MZPORT_FD_DIRECT_THRESHOLD MZPORT_FD_BUFFSIZE

/* The Scheme_FD type is used for both input and output */
typedef struct Scheme_FD {
  MZTAG_IF_REQUIRED
  intptr_t fd;                   /* fd is really a HANDLE in Windows */
  intptr_t bufcount, buffpos;
  char flushing, regfile, flush;
  char textmode; /* Windows: textmode => CRLF conversion; SOME_FDS_... => select definitely works */
  unsigned char *buffer;
  int *refcount;

# ifdef WINDOWS_FILE_HANDLES
  Win_FD_Input_Thread *th;   /* input mode */
  Win_FD_Output_Thread *oth; /* output mode */
  int unblocked; /* whether non-blocking mode is installed */
# endif
} Scheme_FD;

Scheme_Object *scheme_port_name(Scheme_Object *p) {
  if (p->type == scheme_input_port_type)
    return ((Scheme_Input_Port *)p)->name;
  else
    return ((Scheme_Output_Port *)p)->name;
}

int scheme_get_serialized_fd_flags(Scheme_Object* p, Scheme_Serialized_File_FD *so) {
  Scheme_FD *fds;
  if (p->type == scheme_input_port_type) {
    fds = (Scheme_FD *) ((Scheme_Input_Port *)p)->port_data;
    so->name = ((Scheme_Input_Port *)p)->name;
  }
  else {
    fds = (Scheme_FD *) ((Scheme_Output_Port *)p)->port_data;
    so->name = ((Scheme_Output_Port *)p)->name;
  }
  so->regfile = fds->regfile;
  so->textmode = fds->textmode;
  so->flush_mode = fds->flush;
  return 1;
}


#endif

#define MZ_FLUSH_NEVER 0
#define MZ_FLUSH_BY_LINE 1
#define MZ_FLUSH_ALWAYS 2

#ifdef SOME_FDS_ARE_NOT_SELECTABLE
# include <fcntl.h>
#endif

#if defined(WINDOWS_FILE_HANDLES)
# define FILENAME_EXN_E "%E"
#else
# define FILENAME_EXN_E "%e"
#endif

#if defined(DOS_FILE_SYSTEM)
# define fseeko _fseeki64
# define ftello _ftelli64
#endif


/******************** Globals and Prototypes ********************/

/* globals */
READ_ONLY Scheme_Object scheme_eof[1];
THREAD_LOCAL_DECL(Scheme_Object *scheme_orig_stdout_port);
THREAD_LOCAL_DECL(Scheme_Object *scheme_orig_stderr_port);
THREAD_LOCAL_DECL(Scheme_Object *scheme_orig_stdin_port);

THREAD_LOCAL_DECL(struct mz_fd_set *scheme_fd_set);
THREAD_LOCAL_DECL(struct mz_fd_set *scheme_semaphore_fd_set);
THREAD_LOCAL_DECL(Scheme_Hash_Table *scheme_semaphore_fd_mapping);

#ifdef USE_FCNTL_AND_FORK_FOR_FILE_LOCKS
THREAD_LOCAL_DECL(Scheme_Hash_Table *locked_fd_process_map);
static void release_lockf(int fd);
#endif

HOOK_SHARED_OK Scheme_Object *(*scheme_make_stdin)(void) = NULL;
HOOK_SHARED_OK Scheme_Object *(*scheme_make_stdout)(void) = NULL;
HOOK_SHARED_OK Scheme_Object *(*scheme_make_stderr)(void) = NULL;

SHARED_OK MZ_DLLSPEC int scheme_binary_mode_stdio = 0;
void scheme_set_binary_mode_stdio(int v) { scheme_binary_mode_stdio =  v; }

THREAD_LOCAL_DECL(static int special_is_ok);

/* locals */
#ifdef USE_FD_PORTS
THREAD_LOCAL_DECL(static int fd_reserved);
THREAD_LOCAL_DECL(static int the_fd);
#endif
#ifdef MZ_FDS
READ_ONLY static Scheme_Object *fd_input_port_type;
#endif
#ifdef USE_OSKIT_CONSOLE
READ_ONLY static Scheme_Object *oskit_console_input_port_type;
#endif
READ_ONLY static Scheme_Object *file_input_port_type;
READ_ONLY Scheme_Object *scheme_string_input_port_type;
#ifdef USE_TCP
READ_ONLY Scheme_Object *scheme_tcp_input_port_type;
READ_ONLY Scheme_Object *scheme_tcp_output_port_type;
#endif
#ifdef MZ_FDS
READ_ONLY static Scheme_Object *fd_output_port_type;
#endif
READ_ONLY static Scheme_Object *file_output_port_type;
READ_ONLY Scheme_Object *scheme_string_output_port_type;
READ_ONLY Scheme_Object *scheme_user_input_port_type;
READ_ONLY Scheme_Object *scheme_user_output_port_type;
READ_ONLY Scheme_Object *scheme_pipe_read_port_type;
READ_ONLY Scheme_Object *scheme_pipe_write_port_type;
READ_ONLY Scheme_Object *scheme_null_output_port_type;
READ_ONLY Scheme_Object *scheme_redirect_output_port_type;

THREAD_LOCAL_DECL(int scheme_force_port_closed);

SHARED_OK static int flush_out;
SHARED_OK static int flush_err;

THREAD_LOCAL_DECL(static Scheme_Custodian *new_port_cust); /* back-door argument */

#if defined(FILES_HAVE_FDS)
THREAD_LOCAL_DECL(static int external_event_fd);
THREAD_LOCAL_DECL(static int put_external_event_fd);
#endif

static void register_port_wait();

#ifdef MZ_FDS
static intptr_t flush_fd(Scheme_Output_Port *op,
		     const char * volatile bufstr, volatile uintptr_t buflen,
		     volatile uintptr_t offset, int immediate_only, int enable_break);
static void flush_if_output_fds(Scheme_Object *o, Scheme_Close_Custodian_Client *f, void *data);
#endif

static Scheme_Object *subprocess(int c, Scheme_Object *args[]);
static Scheme_Object *subprocess_status(int c, Scheme_Object *args[]);
static Scheme_Object *subprocess_kill(int c, Scheme_Object *args[]);
static Scheme_Object *subprocess_pid(int c, Scheme_Object *args[]);
static Scheme_Object *subprocess_p(int c, Scheme_Object *args[]);
static Scheme_Object *subprocess_wait(int c, Scheme_Object *args[]);
static Scheme_Object *sch_shell_execute(int c, Scheme_Object *args[]);
static Scheme_Object *current_subproc_cust_mode (int, Scheme_Object *[]);
static Scheme_Object *subproc_group_on (int, Scheme_Object *[]);
static void register_subprocess_wait();

typedef struct Scheme_Read_Write_Evt {
  Scheme_Object so;
  Scheme_Object *port;
  Scheme_Object *v; /* peek skip or writeable special */
  char *str;
  intptr_t start, size;
} Scheme_Read_Write_Evt;

static int rw_evt_ready(Scheme_Object *rww, Scheme_Schedule_Info *sinfo);
static void rw_evt_wakeup(Scheme_Object *rww, void *fds);

static int progress_evt_ready(Scheme_Object *rww, Scheme_Schedule_Info *sinfo);
static int closed_evt_ready(Scheme_Object *rww, Scheme_Schedule_Info *sinfo);

static Scheme_Object *
_scheme_make_named_file_input_port(FILE *fp, Scheme_Object *name, int regfile);
static void default_sleep(float v, void *fds);
#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

#if defined(WIN32_FD_HANDLES)
THREAD_LOCAL_DECL(void *scheme_break_semaphore;)
#endif

#ifdef MZ_FDS
static Scheme_Object *make_fd_input_port(int fd, Scheme_Object *name, int regfile, int textmode, int *refcount, int internal);
static Scheme_Object *make_fd_output_port(int fd, Scheme_Object *name, int regfile, int textmode, int read_too, int flush_mode,
					  int *refcount);
#endif
#ifdef USE_OSKIT_CONSOLE
static Scheme_Object *make_oskit_console_input_port();
#endif

static void force_close_output_port(Scheme_Object *port);
static void force_close_input_port(Scheme_Object *port);

ROSYM static Scheme_Object *text_symbol, *binary_symbol;
ROSYM static Scheme_Object *append_symbol, *error_symbol, *update_symbol, *can_update_symbol;
ROSYM static Scheme_Object *replace_symbol, *truncate_symbol, *truncate_replace_symbol;
ROSYM static Scheme_Object *must_truncate_symbol;

ROSYM Scheme_Object *scheme_none_symbol, *scheme_line_symbol, *scheme_block_symbol;

ROSYM static Scheme_Object *exact_symbol;

#define READ_STRING_BYTE_BUFFER_SIZE 1024
THREAD_LOCAL_DECL(static char *read_string_byte_buffer);

#define fail_err_symbol scheme_false

#include "schwinfd.h"

/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/

void
scheme_init_port (Scheme_Env *env)
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  REGISTER_SO(text_symbol);
  REGISTER_SO(binary_symbol);
  REGISTER_SO(append_symbol);
  REGISTER_SO(error_symbol);
  REGISTER_SO(replace_symbol);
  REGISTER_SO(truncate_symbol);
  REGISTER_SO(truncate_replace_symbol);
  REGISTER_SO(update_symbol);
  REGISTER_SO(can_update_symbol);
  REGISTER_SO(must_truncate_symbol);

  text_symbol = scheme_intern_symbol("text");
  binary_symbol = scheme_intern_symbol("binary");
  append_symbol = scheme_intern_symbol("append");
  error_symbol = scheme_intern_symbol("error");
  replace_symbol = scheme_intern_symbol("replace");
  truncate_symbol = scheme_intern_symbol("truncate");
  truncate_replace_symbol = scheme_intern_symbol("truncate/replace");
  update_symbol = scheme_intern_symbol("update");
  can_update_symbol = scheme_intern_symbol("can-update");
  must_truncate_symbol = scheme_intern_symbol("must-truncate");

  REGISTER_SO(scheme_none_symbol);
  REGISTER_SO(scheme_line_symbol);
  REGISTER_SO(scheme_block_symbol);

  scheme_none_symbol = scheme_intern_symbol("none");
  scheme_line_symbol = scheme_intern_symbol("line");
  scheme_block_symbol = scheme_intern_symbol("block");

  REGISTER_SO(exact_symbol);

  exact_symbol = scheme_intern_symbol("exact");

#ifdef MZ_FDS
  REGISTER_SO(fd_input_port_type);
  REGISTER_SO(fd_output_port_type);
#endif
#ifdef USE_OSKIT_CONSOLE
  REGISTER_SO(oskit_console_input_port_type);
#endif
  REGISTER_SO(file_input_port_type);
  REGISTER_SO(scheme_string_input_port_type);
#ifdef USE_TCP
  REGISTER_SO(scheme_tcp_input_port_type);
  REGISTER_SO(scheme_tcp_output_port_type);
#endif
  REGISTER_SO(file_output_port_type);
  REGISTER_SO(scheme_string_output_port_type);
  REGISTER_SO(scheme_user_input_port_type);
  REGISTER_SO(scheme_user_output_port_type);
  REGISTER_SO(scheme_pipe_read_port_type);
  REGISTER_SO(scheme_pipe_write_port_type);
  REGISTER_SO(scheme_null_output_port_type);
  REGISTER_SO(scheme_redirect_output_port_type);

#if defined(UNIX_PROCESSES) && !defined(MZ_PLACES_WAITPID)
  REGISTER_SO(scheme_system_children);
#endif

#ifndef DONT_IGNORE_PIPE_SIGNAL
  START_XFORM_SKIP;
  MZ_SIGSET(SIGPIPE, SIG_IGN);
  END_XFORM_SKIP;
#endif

  if (!scheme_sleep)
    scheme_sleep = default_sleep;

  scheme_eof->type = scheme_eof_type;

  scheme_string_input_port_type = scheme_make_port_type("<string-input-port>");
  scheme_string_output_port_type = scheme_make_port_type("<string-output-port>");

#ifdef MZ_FDS
  fd_input_port_type = scheme_make_port_type("<stream-input-port>");
  fd_output_port_type = scheme_make_port_type("<stream-output-port>");
#endif
#ifdef USE_OSKIT_CONSOLE
  oskit_console_input_port_type = scheme_make_port_type("<console-input-port>");
#endif

  file_input_port_type = scheme_make_port_type("<file-input-port>");
  file_output_port_type = scheme_make_port_type("<file-output-port>");

  scheme_user_input_port_type = scheme_make_port_type("<user-input-port>");
  scheme_user_output_port_type = scheme_make_port_type("<user-output-port>");

  scheme_pipe_read_port_type = scheme_make_port_type("<pipe-input-port>");
  scheme_pipe_write_port_type = scheme_make_port_type("<pipe-output-port>");

#ifdef USE_TCP
  scheme_tcp_input_port_type = scheme_make_port_type("<tcp-input-port>");
  scheme_tcp_output_port_type = scheme_make_port_type("<tcp-output-port>");
#endif

  scheme_null_output_port_type = scheme_make_port_type("<null-output-port>");
  scheme_redirect_output_port_type = scheme_make_port_type("<redirect-output-port>");

#ifdef WIN32_FD_HANDLES
  /* We'll need to know whether this is Win95 or WinNT: */
  {
    OSVERSIONINFO info;
    info.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    GetVersionEx(&info);
    if (info.dwPlatformId == VER_PLATFORM_WIN32_NT)
      scheme_stupid_windows_machine = -1; /* not as stupid */
    else
      scheme_stupid_windows_machine = 1;
  }
#endif

  register_port_wait();

  scheme_add_global_constant("subprocess", scheme_make_prim_w_arity2(subprocess, "subprocess", 4, -1, 4, 4), env);
  scheme_add_global_constant("subprocess-status", scheme_make_prim_w_arity(subprocess_status, "subprocess-status", 1, 1), env);
  scheme_add_global_constant("subprocess-kill", scheme_make_prim_w_arity(subprocess_kill, "subprocess-kill", 2, 2), env);
  scheme_add_global_constant("subprocess-pid", scheme_make_prim_w_arity(subprocess_pid, "subprocess-pid", 1, 1), env);
  scheme_add_global_constant("subprocess?", scheme_make_prim_w_arity(subprocess_p, "subprocess?", 1, 1), env);
  scheme_add_global_constant("subprocess-wait", scheme_make_prim_w_arity(subprocess_wait, "subprocess-wait", 1, 1), env);

  GLOBAL_PARAMETER("subprocess-group-enabled", subproc_group_on, MZCONFIG_SUBPROC_GROUP_ENABLED, env);
  GLOBAL_PARAMETER("current-subprocess-custodian-mode", current_subproc_cust_mode, MZCONFIG_SUBPROC_CUSTODIAN_MODE, env);

  register_subprocess_wait();

  scheme_add_global_constant("shell-execute", scheme_make_prim_w_arity(sch_shell_execute, "shell-execute", 5, 5), env);

  scheme_add_evt(scheme_progress_evt_type, (Scheme_Ready_Fun)progress_evt_ready, NULL, NULL, 1);
  scheme_add_evt(scheme_write_evt_type, (Scheme_Ready_Fun)rw_evt_ready, rw_evt_wakeup, NULL, 1);
  scheme_add_evt(scheme_port_closed_evt_type, (Scheme_Ready_Fun)closed_evt_ready, NULL, NULL, 1);
}

void scheme_init_port_places(void)
{

#ifdef MZ_FDS
  scheme_add_atexit_closer(flush_if_output_fds);
  /* Note: other threads might continue to write even after
     the flush completes, but that's the threads' problem.
     All writing by the main thread will get flushed on exit
     (but not, of course, if the thread is shutdown via a
     custodian). */

  if (!stdin_refcount) {
    /* Referece counts are needed for stdio and places; start
       at 1 in main place, but then cancel initial count */
    stdin_refcount = malloc_refcount(1, 0);
    stdout_refcount = malloc_refcount(1, 0);
    stderr_refcount = malloc_refcount(1, 0);
  }
#endif

  REGISTER_SO(read_string_byte_buffer);
  REGISTER_SO(scheme_orig_stdout_port);
  REGISTER_SO(scheme_orig_stderr_port);
  REGISTER_SO(scheme_orig_stdin_port);
  scheme_orig_stdin_port = (scheme_make_stdin
			    ? scheme_make_stdin()
#ifdef USE_OSKIT_CONSOLE
			    : (osk_not_console
			       ? scheme_make_named_file_input_port(stdin, scheme_intern_symbol("stdin"))
			       : make_oskit_console_input_port())
#else
# ifdef MZ_FDS
#  ifdef WINDOWS_FILE_HANDLES
			    : make_fd_input_port((int)GetStdHandle(STD_INPUT_HANDLE), scheme_intern_symbol("stdin"), 0, 0, 
						 stdin_refcount, 0)
#  else
			    : make_fd_input_port(0, scheme_intern_symbol("stdin"), 0, 0, stdin_refcount, 0)
#  endif
# else
			    : scheme_make_named_file_input_port(stdin, scheme_intern_symbol("stdin"))
# endif
#endif
			    );

  scheme_orig_stdout_port = (scheme_make_stdout
			     ? scheme_make_stdout()
#ifdef MZ_FDS
# ifdef WINDOWS_FILE_HANDLES
			     : make_fd_output_port((int)GetStdHandle(STD_OUTPUT_HANDLE), 
						   scheme_intern_symbol("stdout"), 0, 0, 0,
                                                   -1, stdout_refcount)
# else
			     : make_fd_output_port(1, scheme_intern_symbol("stdout"), 0, 0, 0, -1,
						   stdout_refcount)
# endif
#else
			     : scheme_make_file_output_port(stdout)
#endif
			     );

  scheme_orig_stderr_port = (scheme_make_stderr
			     ? scheme_make_stderr()
#ifdef MZ_FDS
# ifdef WINDOWS_FILE_HANDLES
			     : make_fd_output_port((int)GetStdHandle(STD_ERROR_HANDLE), 
						   scheme_intern_symbol("stderr"), 0, 0, 0,
                                                   MZ_FLUSH_ALWAYS, stderr_refcount)
# else
			     : make_fd_output_port(2, scheme_intern_symbol("stderr"), 0, 0, 0,
                                                   MZ_FLUSH_ALWAYS, stderr_refcount)
# endif
#else
			     : scheme_make_file_output_port(stderr)
#endif
			     );

#ifdef MZ_FDS
  if (!scheme_current_place_id) {
    adj_refcount(stdin_refcount, -1);
    adj_refcount(stdout_refcount, -1);
    adj_refcount(stderr_refcount, -1);
  }
#endif

#if defined(FILES_HAVE_FDS)
# ifndef USE_OSKIT_CONSOLE
  /* Set up a pipe for signaling external events: */
  {
    int fds[2];
    if (!pipe(fds)) {
      external_event_fd = fds[0];
      put_external_event_fd = fds[1];
      fcntl(external_event_fd, F_SETFL, MZ_NONBLOCKING);
      fcntl(put_external_event_fd, F_SETFL, MZ_NONBLOCKING);
    } else {
      if (!scheme_current_place_id) {
        scheme_log_abort("creation of scheduler pipe failed");
        abort();
      } else {
        /* place will call scheme_check_place_port_ok() to discover failure */
      }
    }
  }
# endif
#endif

#ifdef WIN32_FD_HANDLES
  scheme_break_semaphore = (void*)CreateSemaphore(NULL, 0, 1, NULL);
#endif
  
  flush_out = SCHEME_TRUEP(scheme_terminal_port_p(1, &scheme_orig_stdout_port));
  flush_err = SCHEME_TRUEP(scheme_terminal_port_p(1, &scheme_orig_stderr_port));
}

void scheme_init_port_config(void)
{
  Scheme_Config *config;

  config = scheme_current_config();

  scheme_set_param(config, MZCONFIG_INPUT_PORT,   scheme_orig_stdin_port);
  scheme_set_param(config, MZCONFIG_OUTPUT_PORT,  scheme_orig_stdout_port);
  scheme_set_param(config, MZCONFIG_ERROR_PORT,   scheme_orig_stderr_port);
}

Scheme_Object * scheme_make_eof (void)
{
  return scheme_eof;
}

void scheme_set_stdio_makers(Scheme_Stdio_Maker_Proc in,
			     Scheme_Stdio_Maker_Proc out,
			     Scheme_Stdio_Maker_Proc err)
{
  scheme_make_stdin = in;
  scheme_make_stdout = out;
  scheme_make_stderr = err;
}

#ifdef MZ_USE_PLACES
void scheme_check_place_port_ok()
{
# if defined(FILES_HAVE_FDS)
#  ifndef USE_OSKIT_CONSOLE
  if (!external_event_fd && !put_external_event_fd) {
    scheme_signal_error("place: scheduler pipe failed");
  }
#  endif
# endif
}
#endif

/*========================================================================*/
/*                                fd arrays                               */
/*========================================================================*/

/* Implement fd arrays (FD_SET, etc) with a runtime-determined size.
   Also implement special hooks for Windows "descriptors", like
   even queues and semaphores. */

void scheme_alloc_global_fdset() {
#ifdef USE_FAR_MZ_FDCALLS
  REGISTER_SO(scheme_fd_set);
  scheme_fd_set = (struct mz_fd_set *)scheme_alloc_fdset_array(3, 0);
#endif

  REGISTER_SO(scheme_semaphore_fd_set);
#ifdef USE_FAR_MZ_FDCALLS
  scheme_semaphore_fd_set = (struct mz_fd_set *)scheme_alloc_fdset_array(3, 0);
#else
  scheme_semaphore_fd_set = (struct mz_fd_set *)scheme_malloc_atomic(3 * sizeof(fd_set));
#endif
  scheme_fdzero(MZ_GET_FDSET(scheme_semaphore_fd_set, 0));
  scheme_fdzero(MZ_GET_FDSET(scheme_semaphore_fd_set, 1));
  scheme_fdzero(MZ_GET_FDSET(scheme_semaphore_fd_set, 2));

  REGISTER_SO(scheme_semaphore_fd_mapping);
  scheme_semaphore_fd_mapping = scheme_make_hash_table_eqv();
}

#ifdef HAVE_POLL_SYSCALL

# define PFD_EXTRA_SPACE 1

void *scheme_alloc_fdset_array(int count, int permanent)
{
  struct mz_fd_set_data *data;
  struct mz_fd_set *r, *w, *e;
  struct pollfd *pfd;

  data = (struct mz_fd_set_data *)scheme_malloc(sizeof(struct mz_fd_set_data));
  r = (struct mz_fd_set *)scheme_malloc(sizeof(struct mz_fd_set));
  w = (struct mz_fd_set *)scheme_malloc(sizeof(struct mz_fd_set));
  e = (struct mz_fd_set *)scheme_malloc(sizeof(struct mz_fd_set));

  r->w = w;
  r->e = e;
  r->data = data;
  w->data = data;
  e->data = data;

  r->flags = scheme_make_integer(POLLIN);
  w->flags = scheme_make_integer(POLLOUT);
  e->flags = scheme_make_integer(0);

  data->size = scheme_make_integer(32);
  data->count = scheme_make_integer(0);

  pfd = (struct pollfd *)scheme_malloc_atomic(sizeof(struct pollfd) * (32 + PFD_EXTRA_SPACE));
  data->pfd = pfd;

  if (permanent)
    scheme_dont_gc_ptr(r);

  return r;
}

void *scheme_init_fdset_array(void *fdarray, int count)
{
  ((struct mz_fd_set *)fdarray)->data->count = scheme_make_integer(0);
  return fdarray;
}

void *scheme_get_fdset(void *fdarray, int pos)
{
  switch (pos) {
  case 0: 
    return fdarray;
  case 1: 
    return ((struct mz_fd_set *)fdarray)->w;
  case 2: 
  default:
    return ((struct mz_fd_set *)fdarray)->e;
  }
}

void scheme_fdzero(void *fd)
{
  ((struct mz_fd_set *)fd)->data->count = scheme_make_integer(0);
}

static int find_fd_pos(struct mz_fd_set_data *data, int n)
{
  intptr_t count = SCHEME_INT_VAL(data->count);
  intptr_t i;
  
  /* This linear search probably isn't good enough for hundreds or
     thousands of descriptors, but epoll()/kqueue() mode should handle
     that case, anyway. */
  for (i = 0; i < count; i++) {
    if (data->pfd[i].fd == n) {
      return i;
    }
  }

  return -1;
}

void scheme_fdclr(void *fd, int n)
{
  struct mz_fd_set_data *data = ((struct mz_fd_set *)fd)->data;
  intptr_t flag = SCHEME_INT_VAL(((struct mz_fd_set *)fd)->flags);
  intptr_t pos;

  if (!flag) return;

  pos = find_fd_pos(data, n);
  if (pos >= 0) {
    data->pfd[pos].events -= (data->pfd[pos].events & flag);
  }
}

void scheme_fdset(void *fd, int n)
{
  struct mz_fd_set_data *data = ((struct mz_fd_set *)fd)->data;
  intptr_t flag = SCHEME_INT_VAL(((struct mz_fd_set *)fd)->flags);
  intptr_t count, size, pos;
  struct pollfd *pfd;

  if (!flag) return;

  pos = find_fd_pos(data, n);
  if (pos >= 0) {
    data->pfd[pos].events |= flag;
    return;
  }

  count = SCHEME_INT_VAL(data->count);
  size = SCHEME_INT_VAL(data->size);
  if (count >= size) {
    size = size * 2;
    pfd = scheme_malloc_atomic(sizeof(struct pollfd) * (size + PFD_EXTRA_SPACE));
    memcpy(pfd, data->pfd, sizeof(struct pollfd) * count);
    data->pfd = pfd;
    data->size = scheme_make_integer(size);
  }

  data->pfd[count].fd = n;
  data->pfd[count].events = flag;
  count++;
  data->count = scheme_make_integer(count);
}

int scheme_fdisset(void *fd, int n)
{
  struct mz_fd_set_data *data = ((struct mz_fd_set *)fd)->data;
  intptr_t flag = SCHEME_INT_VAL(((struct mz_fd_set *)fd)->flags);
  intptr_t pos;

  if (!flag) flag = (POLLERR | POLLHUP);

  pos = find_fd_pos(data, n);
  if (pos >= 0) {
    if (data->pfd[pos].revents & flag)
      return 1;
    else
      return 0;
  }

  return 0;
}

static int cmp_fd(const void *_a, const void *_b)
{
  struct pollfd *a = (struct pollfd *)_a;
  struct pollfd *b = (struct pollfd *)_b;
  return a->fd - b->fd;
}

void *scheme_merge_fd_sets(void *fds, void *src_fds)
{
  struct mz_fd_set_data *data = ((struct mz_fd_set *)fds)->data;
  struct mz_fd_set_data *src_data = ((struct mz_fd_set *)src_fds)->data;
  int i, si, c, sc, j, nc;
  struct pollfd *pfds;

  scheme_clean_fd_set(fds);
  scheme_clean_fd_set(src_fds);

  c = SCHEME_INT_VAL(data->count);
  sc = SCHEME_INT_VAL(src_data->count);

  if (!c)
    return src_fds;
  if (!sc)
    return fds;

  qsort(data->pfd, c, sizeof(struct pollfd), cmp_fd);
  qsort(src_data->pfd, sc, sizeof(struct pollfd), cmp_fd);

  nc = c + sc;
  pfds = (struct pollfd *)scheme_malloc_atomic(sizeof(struct pollfd) * (nc + PFD_EXTRA_SPACE));
  j = 0;
  for (i = 0, si = 0; (i < c) && (si < sc); ) {
    if (data->pfd[i].fd == src_data->pfd[si].fd) {
      pfds[j].fd = data->pfd[i].fd;
      pfds[j].events = (data->pfd[i].events | src_data->pfd[si].events);
      i++;
      si++;
    } else if (data->pfd[i].fd < src_data->pfd[si].fd) {
      pfds[j].fd = data->pfd[i].fd;
      pfds[j].events = data->pfd[i].events;
      i++;
    } else {
      pfds[j].fd = src_data->pfd[si].fd;
      pfds[j].events = src_data->pfd[si].events;
      si++;
    }
    j++;
  }
  for ( ; i < c; i++, j++) {
    pfds[j].fd = data->pfd[i].fd;
    pfds[j].events = data->pfd[i].events;
  }
  for ( ; si < sc; si++, j++) {
    pfds[j].fd = src_data->pfd[si].fd;
    pfds[j].events = src_data->pfd[si].events;
  }

  if (nc > SCHEME_INT_VAL(data->size)) {
    data->pfd = pfds;
    data->size = scheme_make_integer(nc);
  } else
    memcpy(data->pfd, pfds, j * sizeof(struct pollfd));
  data->count = scheme_make_integer(j);

  return fds;
}

void scheme_clean_fd_set(void *fds)
{
  struct mz_fd_set_data *data = ((struct mz_fd_set *)fds)->data;
  intptr_t count = SCHEME_INT_VAL(data->count);
  intptr_t i, j = 0;

  for (i = 0; i < count; i++) {
    if (data->pfd[i].events) {
      if (j < i) {
        data->pfd[j].fd = data->pfd[i].fd;
        data->pfd[j].events = data->pfd[i].events;
      }
      j++;
    }
  }
  
  count = j;
  data->count = scheme_make_integer(count);
}

int scheme_get_fd_limit(void *fds)
{
  return 0;
}

#else

# if defined(USE_DYNAMIC_FDSET_SIZE)
/* initialized early via scheme_alloc_global_fdset */
SHARED_OK static int dynamic_fd_size;

# define STORED_ACTUAL_FDSET_LIMIT
# define FDSET_LIMIT(fd) (*(int *)((char *)fd XFORM_OK_PLUS dynamic_fd_size))

void *scheme_alloc_fdset_array(int count, int permanent)
  XFORM_SKIP_PROC
{
  /* Note: alloc only at the end, because this function
     isn't annotated. We skip annotation so that it's
     ok with OS X use from default_sleep() */

  if (!dynamic_fd_size) {
# ifdef USE_ULIMIT
    dynamic_fd_size = ulimit(4, 0);
# else
    dynamic_fd_size = getdtablesize();
# endif
    /* divide by bits-per-byte: */
    dynamic_fd_size = (dynamic_fd_size + 7) >> 3;
    /* word-align: */
    if (dynamic_fd_size % sizeof(void*))
      dynamic_fd_size += sizeof(void*) - (dynamic_fd_size % sizeof(void*));
  }

  if (permanent)
    return scheme_malloc_eternal(count * (dynamic_fd_size + sizeof(intptr_t)));
  else
    return scheme_malloc_atomic_allow_interior(count * (dynamic_fd_size + sizeof(intptr_t)));
}

void *scheme_init_fdset_array(void *fdarray, int count)
{
  return fdarray;
}

void *scheme_get_fdset(void *fdarray, int pos)
{
  return ((char *)fdarray) + (pos * (dynamic_fd_size + sizeof(intptr_t)));
}

void scheme_fdzero(void *fd)
{
  memset(fd, 0, dynamic_fd_size + sizeof(intptr_t));
}

# else

# if defined(WIN32_FD_HANDLES)
#  define fdset_type win_extended_fd_set
# else
#  define fdset_type fd_set
# endif

void *scheme_alloc_fdset_array(int count, int permanent)
{
# if defined(FILES_HAVE_FDS) || defined(USE_SOCKETS_TCP) || defined(WIN32_FD_HANDLES)
  void *fdarray;
#  if defined(WIN32_FD_HANDLES)
  if (count) {
    fdarray = scheme_malloc_allow_interior(count * sizeof(fdset_type));
    if (permanent)
      scheme_dont_gc_ptr(fdarray);
    
    scheme_init_fdset_array(fdarray, count);
  } else
    fdarray = NULL;
#  else
  if (permanent)
    fdarray = scheme_malloc_eternal(count * sizeof(fdset_type));
  else
    fdarray = scheme_malloc_atomic(count * sizeof(fdset_type));
#  endif
  return fdarray;
# else
  return NULL;
# endif
}

# if defined(WIN32_FD_HANDLES)
static void reset_wait_array(win_extended_fd_set *efd)
{
  /* Allocate an array that may be big enough to hold all events
     when we eventually call WaitForMultipleObjects. One of the three
     arrays will be big enough. */
  int sz = (3 * (SCHEME_INT_VAL(efd->alloc) + SCHEME_INT_VAL(efd->num_handles))) + 2;
  HANDLE *wa;
  wa = MALLOC_N_ATOMIC(HANDLE, sz);
  efd->wait_array = wa;
}
# endif

void *scheme_init_fdset_array(void *fdarray, int count)
{
# if defined(WIN32_FD_HANDLES)
  if (count) {
    int i;
    win_extended_fd_set *fd;
    for (i = 0; i < count; i++) {
      fd = (win_extended_fd_set *)scheme_get_fdset(fdarray, i);
      fd->sockets = NULL;
      fd->added = scheme_make_integer(0);
      fd->alloc = scheme_make_integer(0);
      fd->handles = NULL;
      fd->num_handles = scheme_make_integer(0);
      fd->no_sleep = NULL;
      fd->wait_event_mask = scheme_make_integer(0);
      fd->wait_array = NULL;
      reset_wait_array(fdarray);
    }
  }
# endif
  return fdarray;
}

void *scheme_get_fdset(void *fdarray, int pos)
{
# if defined(FILES_HAVE_FDS) || defined(USE_SOCKETS_TCP) || defined(WIN32_FD_HANDLES)
  return ((fdset_type *)fdarray) + pos;
# else
  return NULL;
# endif
}

void scheme_fdzero(void *fd)
{
# if defined(WIN32_FD_HANDLES)
  scheme_init_fdset_array(fd, 1);
# else
#  if defined(FILES_HAVE_FDS) || defined(USE_SOCKETS_TCP)
  FD_ZERO((fd_set *)fd);
#  endif
# endif
}

# endif

void scheme_fdclr(void *fd, int n)
{
#if defined(WIN32_FD_HANDLES)
  win_extended_fd_set *efd = (win_extended_fd_set *)fd;
  int i;
  for (i = SCHEME_INT_VAL(efd->added); i--; ) {
    if (efd->sockets[i] == n)
      efd->sockets[i] = INVALID_SOCKET;
  }
#else
# if defined(FILES_HAVE_FDS) || defined(USE_SOCKETS_TCP)
  FD_CLR((unsigned)n, ((fd_set *)fd));
# endif
#endif
}

void scheme_fdset(void *fd, int n)
{
#if defined(WIN32_FD_HANDLES)
  win_extended_fd_set *efd = (win_extended_fd_set *)fd;
  if (SCHEME_INT_VAL(efd->added) >= SCHEME_INT_VAL(efd->alloc)) {
    SOCKET *naya;
    int na;
    na = (SCHEME_INT_VAL(efd->alloc) * 2) + 10;
    naya = (SOCKET *)scheme_malloc_atomic(na * sizeof(SOCKET));
    memcpy(naya, efd->sockets, SCHEME_INT_VAL(efd->alloc) * sizeof(SOCKET));
    efd->sockets = naya;
    efd->alloc = scheme_make_integer(na);
    reset_wait_array(efd);
  }
  efd->sockets[SCHEME_INT_VAL(efd->added)] = n;
  efd->added = scheme_make_integer(1 + SCHEME_INT_VAL(efd->added));
#else
# if defined(FILES_HAVE_FDS) || defined(USE_SOCKETS_TCP)
#  ifdef STORED_ACTUAL_FDSET_LIMIT
  int mx;
  mx = FDSET_LIMIT(fd);
  if (n > mx)
    FDSET_LIMIT(fd) = n;
#  endif
  FD_SET(n, ((fd_set *)fd));
# endif
#endif
}

int scheme_fdisset(void *fd, int n)
{
#if defined(WIN32_FD_HANDLES)
  win_extended_fd_set *efd = (win_extended_fd_set *)fd;
  int i;
  for (i = SCHEME_INT_VAL(efd->added); i--; ) {
    if (efd->sockets[i] == n)
      return 1;
  }
  return 0;
#else
# if defined(FILES_HAVE_FDS) || defined(USE_SOCKETS_TCP)
  return FD_ISSET(n, ((fd_set *)fd));
# else
  return 0;
# endif
#endif
}

void *scheme_merge_fd_sets(void *fds, void *src_fds)
{
#if defined(WIN32_FD_HANDLES)
  win_extended_fd_set *efd = (win_extended_fd_set *)src_fds;
  int i;
  for (i = SCHEME_INT_VAL(efd->added); i--; ) {
    if (efd->sockets[i] != INVALID_SOCKET)
      scheme_fdset(fds, efd->sockets[i]);
  }
  return fds;
#else
  int i, j;
  GC_CAN_IGNORE unsigned char *p, *sp;
  for (j = 0; j < 3; j++) {
    p = scheme_get_fdset(fds, j);
    sp = scheme_get_fdset(src_fds, j);
# ifdef STORED_ACTUAL_FDSET_LIMIT
    if (FDSET_LIMIT(sp) > FDSET_LIMIT(p)) {
      i = FDSET_LIMIT(sp);
      FDSET_LIMIT(p) = i;
    }
# endif
# if defined(USE_DYNAMIC_FDSET_SIZE)
    i = dynamic_fd_size;
# else
    i = sizeof(fd_set);
# endif
    for (; i--; p++, sp++) {
      *p |= *sp;
    }
  }
  return fds;
#endif
}

void scheme_clean_fd_set(void *fds)
{
}

int scheme_get_fd_limit(void *fds)
  XFORM_SKIP_PROC
/* This function must not allocate or call GC-cooperating functions.
   It's constrained because it's used by default_sleep, which
   must not allocate on Mac OS X. */
{
  int limit, actual_limit;
  fd_set *rd, *wr, *ex;

#  ifdef USE_WINSOCK_TCP
  limit = 0;
#  else
#   ifdef USE_ULIMIT
  limit = ulimit(4, 0);
#   else
#    ifdef FIXED_FD_LIMIT
  limit = FIXED_FD_LIMIT;
#    else
  limit = getdtablesize();
#    endif
#   endif
#  endif
  
  rd = (fd_set *)fds;
  wr = (fd_set *)MZ_GET_FDSET(fds, 1);
  ex = (fd_set *)MZ_GET_FDSET(fds, 2);
#  ifdef STORED_ACTUAL_FDSET_LIMIT
  actual_limit = FDSET_LIMIT(rd);
  if (FDSET_LIMIT(wr) > actual_limit)
    actual_limit = FDSET_LIMIT(wr);
  if (FDSET_LIMIT(ex) > actual_limit)
    actual_limit = FDSET_LIMIT(ex);
  actual_limit++;
#  else
  actual_limit = limit;
#  endif
  
  return actual_limit;
}

#endif

void scheme_add_fd_handle(void *h, void *fds, int repost)
{
#if defined(WIN32_FD_HANDLES)
  win_extended_fd_set *efd = (win_extended_fd_set *)fds;
  OS_SEMAPHORE_TYPE *hs;
  int i, *rps;

  i = SCHEME_INT_VAL(efd->num_handles);
  hs = MALLOC_N_ATOMIC(OS_SEMAPHORE_TYPE, i + 1);
  rps = MALLOC_N_ATOMIC(int, i + 1);
  hs[i] = (OS_SEMAPHORE_TYPE)h;
  rps[i] = repost;
  while (i--) {
    hs[i] = efd->handles[i];
    rps[i] = efd->repost_sema[i];
  }
  efd->num_handles = scheme_make_integer(1 + SCHEME_INT_VAL(efd->num_handles));
  efd->handles = hs;
  efd->repost_sema = rps;
  reset_wait_array(efd);
#else
  /* Do nothing. */
#endif
}

void scheme_add_fd_nosleep(void *fds)
{
#if defined(WIN32_FD_HANDLES)
  win_extended_fd_set *efd = (win_extended_fd_set *)fds;
  efd->no_sleep = scheme_true;
#else
#endif
}

void scheme_add_fd_eventmask(void *fds, int mask)
{
#if defined(WIN32_FD_HANDLES)
  win_extended_fd_set *efd = (win_extended_fd_set *)fds;
  efd->wait_event_mask = scheme_make_integer(mask | SCHEME_INT_VAL(efd->wait_event_mask));
#endif
}

#if defined(WIN32_FD_HANDLES)
void WSAEventSelect_plus_check(SOCKET s, WSAEVENT e, long mask)
{
  fd_set rd[1], wr[1], ex[1];
  struct timeval t = {0, 0};

  WSAEventSelect(s, e, mask);
  
  /* double-check with select(), because WSAEventSelect only
     handles new activity (I think) */
  FD_ZERO(rd);
  FD_ZERO(wr);
  FD_ZERO(ex);

  if (mask & FD_READ)
    FD_SET(s, rd);
  if (mask & FD_WRITE)
    FD_SET(s, wr);
  if (mask & FD_OOB)
    FD_SET(s, ex);

  if (select(1, rd, wr, ex, &t)) {
    /* already ready */
    WSAEventSelect(s, NULL, 0);
    SetEvent(e);
  }
}
#endif

void scheme_collapse_win_fd(void *fds)
{
#if defined(WIN32_FD_HANDLES)
  win_extended_fd_set *rfd, *wfd, *efd;
  HANDLE *wa, e;
  int i, p = 0, mask, j;
  SOCKET s;

  rfd = (win_extended_fd_set *)fds;
  wfd = (win_extended_fd_set *)scheme_get_fdset(fds, 1);
  efd = (win_extended_fd_set *)scheme_get_fdset(fds, 2);

  if (rfd->combined_wait_array) {
    /* clean up */
    for (i = SCHEME_INT_VAL(rfd->added); i--; ) {
      if (rfd->sockets[i] != INVALID_SOCKET)
	WSAEventSelect(rfd->sockets[i], NULL, 0);
    }
    for (i = SCHEME_INT_VAL(wfd->added); i--; ) {
      if (wfd->sockets[i] != INVALID_SOCKET)
	WSAEventSelect(wfd->sockets[i], NULL, 0);
    }
    for (i = SCHEME_INT_VAL(efd->added); i--; ) {
      if (efd->sockets[i] != INVALID_SOCKET)
	WSAEventSelect(efd->sockets[i], NULL, 0);
    }
    p = SCHEME_INT_VAL(rfd->num_handles);
    for (i = SCHEME_INT_VAL(rfd->combined_len); i-- > p; ) {
      WSACloseEvent(rfd->combined_wait_array[i]);
    }
    rfd->combined_wait_array = NULL;
  } else {
    /* merge */
    if (SCHEME_INT_VAL(rfd->alloc) < SCHEME_INT_VAL(wfd->alloc)) {
      if (SCHEME_INT_VAL(wfd->alloc) < SCHEME_INT_VAL(efd->alloc))
	wa = efd->wait_array;
      else
	wa = wfd->wait_array;
    } else {
      if (SCHEME_INT_VAL(rfd->alloc) < SCHEME_INT_VAL(efd->alloc))
	wa = efd->wait_array;
      else
	wa = rfd->wait_array;
    }

    rfd->combined_wait_array = wa;

    p = SCHEME_INT_VAL(rfd->num_handles);
    for (i = 0; i < p; i++) {
      wa[i] = rfd->handles[i];
    }
  
    for (i = SCHEME_INT_VAL(rfd->added); i--; ) {
      s = rfd->sockets[i];
      if (s != INVALID_SOCKET) {
	mask = FD_READ | FD_ACCEPT | FD_CLOSE;
	
	for (j = SCHEME_INT_VAL(wfd->added); j--; ) {
	  if (wfd->sockets[j] == s) {
	    mask |= FD_WRITE;
	    break;
	  }
	}

	for (j = SCHEME_INT_VAL(efd->added); j--; ) {
	  if (efd->sockets[j] == s) {
	    mask |= FD_OOB;
	    break;
	  }
	}

	e = WSACreateEvent();
	wa[p++] = e;
	WSAEventSelect_plus_check(s, e, mask);
      }
    }

    for (i = SCHEME_INT_VAL(wfd->added); i--; ) {
      s = wfd->sockets[i];
      if (s != INVALID_SOCKET) {
	mask = FD_WRITE | FD_CONNECT | FD_CLOSE;
	
	for (j = SCHEME_INT_VAL(rfd->added); j--; ) {
	  if (rfd->sockets[j] == s) {
	    mask = 0;
	    break;
	  }
	}

	if (mask) {
	  for (j = SCHEME_INT_VAL(efd->added); j--; ) {
	    if (efd->sockets[j] == s) {
	      mask |= FD_OOB;
	      break;
	    }
	  }
	  
	  e = WSACreateEvent();
	  wa[p++] = e;
	  WSAEventSelect_plus_check(s, e, mask);
	}
      }
    }

    for (i = SCHEME_INT_VAL(efd->added); i--; ) {
      s = efd->sockets[i];
      if (s != INVALID_SOCKET) {
	mask = FD_OOB | FD_CLOSE;
	
	for (j = SCHEME_INT_VAL(rfd->added); j--; ) {
	  if (rfd->sockets[j] == s) {
	    mask = 0;
	    break;
	  }
	}

	if (mask) {
	  for (j = SCHEME_INT_VAL(wfd->added); j--; ) {
	    if (wfd->sockets[j] == s) {
	      mask = 0;
	      break;
	    }
	  }
	  
	  if (mask) {
	    e = WSACreateEvent();
	    wa[p++] = e;
	    WSAEventSelect_plus_check(s, e, mask);
	  }
	}
      }
    }

    rfd->combined_len = scheme_make_integer(p);
  }
#endif
}

intptr_t scheme_dup_file(intptr_t fd) {
#ifdef WINDOWS_FILE_HANDLES
  HANDLE  newhandle;
  BOOL rc;

  rc = DuplicateHandle(GetCurrentProcess(), (HANDLE) fd,
          GetCurrentProcess(), &newhandle,
          0, FALSE, DUPLICATE_SAME_ACCESS);

  if (rc == FALSE) {
    return -1;
  }
  else {
    return (intptr_t) newhandle;
  }
#else
  intptr_t nfd;
  do {
    nfd = dup(fd);
  } while (nfd == -1 && errno == EINTR);
  return nfd;
#endif
}

void scheme_close_file_fd(intptr_t fd) {
#ifdef WINDOWS_FILE_HANDLES
  CloseHandle((HANDLE)fd);
#else
  {
    intptr_t rc;
    do {
      rc = close(fd);
    } while (rc == -1 && errno == EINTR);
  }
#endif
}


/*========================================================================*/
/*                      Windows thread suspension                         */
/*========================================================================*/

/* Racket creates Windows threads for various purposes, including
   non-blocking FILE reads. Unfortunately, these threads can confuse
   the Boehm GC if they move virtual pages around while its marking. So we
   remember each created thread and suspend it during GC.

   This work is not necessary if GC_use_registered_statics is set. */

#ifndef MZ_PRECISE_GC

# ifdef WINDOWS_PROCESSES
typedef struct Scheme_Thread_Memory {
  MZTAG_IF_REQUIRED
  void *handle;
  void *subhandle;
  int autoclose;
  struct Scheme_Thread_Memory *prev;
  struct Scheme_Thread_Memory *next;
} Scheme_Thread_Memory;

Scheme_Thread_Memory *tm_start, *tm_next;

void scheme_init_thread_memory()
{
  REGISTER_SO(tm_start);
  REGISTER_SO(tm_next);

  /* We start with a pre-allocated tm because we
     want to register a thread before performing any
     allocations. */
  tm_next = MALLOC_ONE_RT(Scheme_Thread_Memory);
#  ifdef MZTAG_REQUIRED
  tm_next->type = scheme_rt_thread_memory;
#  endif

  /* scheme_init_thread() will replace these: */
  GC_set_collect_start_callback(scheme_suspend_remembered_threads);
  GC_set_collect_end_callback(scheme_resume_remembered_threads);
}

Scheme_Thread_Memory *scheme_remember_thread(void *t, int autoclose)
{
  Scheme_Thread_Memory *tm = tm_next;

  tm->handle = t;
  tm->subhandle = NULL;
  tm->autoclose = autoclose;

  tm->prev = NULL;
  tm->next = tm_start;
  if (tm->next)
    tm->next->prev = tm;
  tm_start = tm;

  tm_next = MALLOC_ONE_RT(Scheme_Thread_Memory);
#  ifdef MZTAG_REQUIRED
  tm_next->type = scheme_rt_thread_memory;
#  endif

  return tm;
}

void scheme_remember_subthread(struct Scheme_Thread_Memory *tm, void *t)
{
  tm->subhandle = t;
}

void scheme_forget_thread(struct Scheme_Thread_Memory *tm)
  XFORM_SKIP_PROC
{
  if (tm->prev)
    tm->prev->next = tm->next;
  else
    tm_start = tm->next;

  if (tm->next)
    tm->next->prev = tm->prev;

  tm->next = NULL;
  tm->prev = NULL;
}

void scheme_forget_subthread(struct Scheme_Thread_Memory *tm)
  XFORM_SKIP_PROC
{
  tm->subhandle = NULL;
}

void scheme_suspend_remembered_threads(void)
  XFORM_SKIP_PROC
{
  Scheme_Thread_Memory *tm, *next, *prev = NULL;
  int keep;

  for (tm = tm_start; tm; tm = next) {
    next = tm->next;

    keep = 1;
    if (tm->autoclose) {
      if (WaitForSingleObject(tm->handle, 0) == WAIT_OBJECT_0) {
	CloseHandle((HANDLE)tm->handle);
	tm->handle = NULL;
	if (prev)
	  prev->next = tm->next;
	else
	  tm_start = tm->next;
	if (tm->next)
	  tm->next->prev = prev;
	tm->next = NULL;
	tm->prev = NULL;
	keep = 0;
      }
    }

    if (keep) {
      SuspendThread((HANDLE)tm->handle);
      if (tm->subhandle)
	SuspendThread((HANDLE)tm->subhandle);
      prev = tm;
    }
  }
}

void scheme_resume_remembered_threads(void)
  XFORM_SKIP_PROC
{
  Scheme_Thread_Memory *tm;

  for (tm = tm_start; tm; tm = tm->next) {
    if (tm->subhandle)
      ResumeThread((HANDLE)tm->subhandle);
    ResumeThread((HANDLE)tm->handle);
  }
}

# endif

#else

typedef struct Scheme_Thread_Memory Scheme_Thread_Memory;

void scheme_init_thread_memory() { }
Scheme_Thread_Memory *scheme_remember_thread(void *t, int autoclose) { return NULL; }
void scheme_remember_subthread(struct Scheme_Thread_Memory *tm, void *t) { }
void scheme_forget_thread(struct Scheme_Thread_Memory *tm) { }
void scheme_forget_subthread(struct Scheme_Thread_Memory *tm) { }
void scheme_suspend_remembered_threads() { }
void scheme_resume_remembered_threads(void) { }

#endif


/*========================================================================*/
/*                        Generic port support                            */
/*========================================================================*/


Scheme_Object *scheme_make_port_type(const char *name)
{
  return scheme_make_symbol(name);
}

static void init_port_locations(Scheme_Port *ip)
{
  int cl;

  ip->position = 0;
  ip->readpos = 0; /* like position, but post UTF-8 decoding, collapses CRLF, etc. */
  ip->lineNumber = 1;
  ip->oldColumn = 0;
  ip->column = 0;
  ip->charsSinceNewline = 1;
  cl = SCHEME_TRUEP(scheme_get_param(scheme_current_config(), MZCONFIG_PORT_COUNT_LINES));
  ip->count_lines = cl;
}

void scheme_set_next_port_custodian(Scheme_Custodian *c)
{
  new_port_cust = c;
}

Scheme_Input_Port *
scheme_make_input_port(Scheme_Object *subtype,
		       void *data,
		       Scheme_Object *name,
		       Scheme_Get_String_Fun get_string_fun,
		       Scheme_Peek_String_Fun peek_string_fun,
		       Scheme_Progress_Evt_Fun progress_evt_fun,
		       Scheme_Peeked_Read_Fun peeked_read_fun,
		       Scheme_In_Ready_Fun byte_ready_fun,
		       Scheme_Close_Input_Fun close_fun,
		       Scheme_Need_Wakeup_Input_Fun need_wakeup_fun,
		       int must_close)
{
  Scheme_Input_Port *ip;
  Scheme_Custodian *cust = new_port_cust;

  new_port_cust = NULL;

  ip = MALLOC_ONE_TAGGED(Scheme_Input_Port);
  ip->p.so.type = scheme_input_port_type;
  ip->sub_type = subtype;
  ip->port_data = data;
  ip->get_string_fun = get_string_fun;
  ip->peek_string_fun = peek_string_fun;
  ip->progress_evt_fun = progress_evt_fun;
  ip->peeked_read_fun = peeked_read_fun;
  ip->byte_ready_fun = byte_ready_fun;
  ip->need_wakeup_fun = need_wakeup_fun;
  ip->close_fun = close_fun;
  ip->name = name;
  ip->ungotten_count = 0;
  ip->closed = 0;
  ip->read_handler = NULL;
  init_port_locations((Scheme_Port *)ip);
  if (ip->p.count_lines) ip->slow = 1;

  if (progress_evt_fun == scheme_progress_evt_via_get)
    ip->unless_cache = scheme_false;

  if (must_close) {
    Scheme_Custodian_Reference *mref;
    mref = scheme_add_managed(cust,
			      (Scheme_Object *)ip,
			      (Scheme_Close_Custodian_Client *)force_close_input_port,
			      NULL, must_close);
    ip->mref = mref;
  } else
    ip->mref = NULL;

  return (ip);
}

void scheme_set_port_location_fun(Scheme_Port *port,
				  Scheme_Location_Fun location_fun)
{
  port->location_fun = location_fun;
}

void scheme_set_port_count_lines_fun(Scheme_Port *port,
				     Scheme_Count_Lines_Fun count_lines_fun)
{
  port->count_lines_fun = count_lines_fun;
}

static int evt_input_port_p(Scheme_Object *p)
{
  return 1;
}

Scheme_Output_Port *
scheme_make_output_port(Scheme_Object *subtype,
			void *data,
			Scheme_Object *name,
			Scheme_Write_String_Evt_Fun write_string_evt_fun,
			Scheme_Write_String_Fun write_string_fun,
			Scheme_Out_Ready_Fun ready_fun,
			Scheme_Close_Output_Fun close_fun,
			Scheme_Need_Wakeup_Output_Fun need_wakeup_fun,
			Scheme_Write_Special_Evt_Fun write_special_evt_fun,
			Scheme_Write_Special_Fun write_special_fun,
			int must_close)
{
  Scheme_Output_Port *op;
  Scheme_Custodian *cust = new_port_cust;

  new_port_cust = NULL;

  op = MALLOC_ONE_TAGGED(Scheme_Output_Port);
  op->p.so.type = scheme_output_port_type;
  op->sub_type = subtype;
  op->port_data = data;
  op->name = name;
  op->write_string_evt_fun = write_string_evt_fun;
  op->write_string_fun = write_string_fun;
  op->close_fun = close_fun;
  op->ready_fun = ready_fun;
  op->need_wakeup_fun = need_wakeup_fun;
  op->write_special_evt_fun = write_special_evt_fun;
  op->write_special_fun = write_special_fun;
  op->closed = 0;
  op->display_handler = NULL;
  op->write_handler = NULL;
  op->print_handler = NULL;
  init_port_locations((Scheme_Port *)op);

  if (must_close) {
    Scheme_Custodian_Reference *mref;
    mref = scheme_add_managed(cust,
			      (Scheme_Object *)op,
			      (Scheme_Close_Custodian_Client *)force_close_output_port,
			      NULL, must_close);
    op->mref = mref;
  } else
    op->mref = NULL;

  return op;
}

static int evt_output_port_p(Scheme_Object *p)
{
  return 1;
}

static int output_ready(Scheme_Object *port, Scheme_Schedule_Info *sinfo)
{
  Scheme_Output_Port *op;

  op = scheme_output_port_record(port);

  if (op->closed)
    return 1;

  if (SAME_OBJ(scheme_user_output_port_type, op->sub_type)) {
    /* We can't call the normal ready because that might run Racket
       code, and this function is called by the scheduler when
       false_pos_ok is true. So, in that case, we assume that if the
       port's evt is ready, then the port is ready. (After
       all, false positives are ok in that mode.) Even when the
       scheduler isn't requesting the status, we need sinfo. */
    return scheme_user_port_write_probably_ready(op, sinfo);
  }

  if (op->ready_fun) {
    Scheme_Out_Ready_Fun_FPC rf;
    rf = (Scheme_Out_Ready_Fun_FPC)op->ready_fun;
    return rf(op, sinfo);
  }

  return 1;
}

static void output_need_wakeup (Scheme_Object *port, void *fds)
{
  Scheme_Output_Port *op;

  /* If this is a user output port and its evt needs a wakeup, we
     shouldn't get here. The target use above will take care of it. */

  op = scheme_output_port_record(port);
  if (op->need_wakeup_fun) {
    Scheme_Need_Wakeup_Output_Fun f;
    f = op->need_wakeup_fun;
    f(op, fds);
  }
}

static int byte_input_ready (Scheme_Object *port, Scheme_Schedule_Info *sinfo);

int scheme_byte_ready_or_user_port_ready(Scheme_Object *p, Scheme_Schedule_Info *sinfo)
{
  Scheme_Input_Port *ip;

  ip = scheme_input_port_record(p);

  if (ip->closed)
    return 1;

  if (SAME_OBJ(scheme_user_input_port_type, ip->sub_type)) {
    /* We can't call the normal byte_ready because that runs Racket
       code, and this function is called by the scheduler when
       false_pos_ok is true. So, in that case, we assume that if the
       port's evt is ready, then the port is ready. (After
       all, false positives are ok in that mode.) Even when the
       scheduler isn't requesting the status, we need sinfo. */
    return scheme_user_port_byte_probably_ready(ip, sinfo);
  } else
    return byte_input_ready(p, sinfo);
}

static void register_port_wait()
{
  scheme_add_evt(scheme_input_port_type,
                 (Scheme_Ready_Fun)scheme_byte_ready_or_user_port_ready, scheme_need_wakeup,
                 evt_input_port_p, 1);
  scheme_add_evt(scheme_output_port_type,
		  (Scheme_Ready_Fun)output_ready, output_need_wakeup,
		  evt_output_port_p, 1);
}

XFORM_NONGCING static int pipe_char_count(Scheme_Object *p)
{
  if (p) {
    Scheme_Pipe *pipe;
    Scheme_Input_Port *ip;

    ip = (Scheme_Input_Port *)p;
    pipe = (Scheme_Pipe *)ip->port_data;

    if (pipe->bufstart <= pipe->bufend)
      return pipe->bufend - pipe->bufstart;
    else
      return (pipe->buflen - pipe->bufstart) + pipe->bufend;
  } else
    return 0;
}

int scheme_pipe_char_count(Scheme_Object *p)
{
  return pipe_char_count(p);
}

/****************************** main input reader ******************************/

static void post_progress(Scheme_Input_Port *ip)
{
  scheme_post_sema_all(ip->progress_evt);
  ip->progress_evt = NULL;
}

XFORM_NONGCING static void inc_pos(Scheme_Port *ip, int a)
{
  ip->column += a;
  ip->readpos += a;
  ip->charsSinceNewline += a;
  ip->utf8state = 0;
}

static Scheme_Object *quick_plus(Scheme_Object *s, intptr_t v)
{
  if (SCHEME_INTP(s)) {
    int k;
    k = SCHEME_INT_VAL(s);
    if ((k < 0x1000000) && (v < 0x1000000)) {
      k += v;
      return scheme_make_integer(k);
    }
  }

  /* Generic addition, but we might not be in a position to allow
     thread swaps */
  scheme_start_atomic();
  s = scheme_bin_plus(s, scheme_make_integer(v));
  scheme_end_atomic_no_swap();

  return s;
}

#define state_len(state) ((state >> 3) & 0x7)

XFORM_NONGCING static void do_count_lines(Scheme_Port *ip, const char *buffer, intptr_t offset, intptr_t got)
{
  intptr_t i;
  int c, degot = 0;

  mzAssert(ip->lineNumber >= 0);
  mzAssert(ip->column >= 0);
  mzAssert(ip->position >= 0);

  ip->oldColumn = ip->column; /* works for a single-char read, like `read' */

  ip->readpos += got; /* add for CR LF below */

  /* Find start of last line: */
  for (i = got, c = 0; i--; c++) {
    if (buffer[offset + i] == '\n' || buffer[offset + i] == '\r') {
      break;
    }
  }

  /* Count UTF-8-decoded chars, up to last line: */
  if (i >= 0) {
    int state = ip->utf8state;
    int n;
    degot += state_len(state);
    n = scheme_utf8_decode_count((const unsigned char *)buffer, offset, offset + i + 1, &state, 0, 0xFFFD);
    degot += (i + 1 - n);
    ip->utf8state = 0; /* assert: state == 0, because we ended with a newline */
  }
	
  if (i >= 0) {
    int n = 0;
    ip->charsSinceNewline = c + 1;
    i++;
    /* Continue walking, back over the previous lines, to find
       out how many there were: */
    while (i--) {
      if (buffer[offset + i] == '\n') {
	if (!(i && (buffer[offset + i - 1] == '\r'))
	    && !(!i && ip->was_cr)) {
	  n++;
	} else
	  degot++; /* adjust positions for CRLF -> LF conversion */
      } else if (buffer[offset + i] == '\r') {
	n++;
      }
    }
	 	  
    mzAssert(n > 0);
    ip->lineNumber += n;
    ip->was_cr = (buffer[offset + got - 1] == '\r');
    /* Now reset column to 0: */
    ip->column = 0;
  } else {
    ip->charsSinceNewline += c;
  }

  /* Do the last line to get the column count right and to
     further adjust positions for UTF-8 decoding: */
  {
    int col = ip->column, n;
    int prev_i = got - c;
    int state = ip->utf8state;
    n = state_len(state);
    degot += n;
    col -= n;
    for (i = prev_i; i < got; i++) {
      if (buffer[offset + i] == '\t') {
	n = scheme_utf8_decode_count((const unsigned char *)buffer, offset + prev_i, offset + i, &state, 0, 0xFFFD);
	degot += ((i - prev_i) - n);
	col += n;
	col = col - (col & 0x7) + 8;
	prev_i = i + 1;
      }
    }
    if (prev_i < i) {
      n = scheme_utf8_decode_count((const unsigned char *)buffer, offset + prev_i, offset + i, &state, 1, 0xFFFD);
      n += state_len(state);
      col += n;
      degot += ((i - prev_i) - n);
    }
    ip->column = col;
    ip->utf8state = state;
  }

  ip->readpos -= degot;

  mzAssert(ip->lineNumber >= 0);
  mzAssert(ip->column >= 0);
  mzAssert(ip->position >= 0);
}

void scheme_port_count_lines(Scheme_Port *ip, const char *buffer, intptr_t offset, intptr_t got)
{
  if (ip->position >= 0)
    ip->position += got;
    
  if (ip->count_lines)
    do_count_lines(ip, buffer, offset, got);
}

intptr_t scheme_get_byte_string_unless(const char *who,
				   Scheme_Object *port,
				   char *buffer, intptr_t offset, intptr_t size,
				   int only_avail,
				   int peek, Scheme_Object *peek_skip,
				   Scheme_Object *unless_evt)
{
  Scheme_Input_Port *ip;
  intptr_t got = 0, total_got = 0, gc;
  int special_ok = special_is_ok, check_special;
  Scheme_Get_String_Fun gs;
  Scheme_Peek_String_Fun ps;

  /* See also get_one_byte, below. Any change to this function
     may require a change to 1-byte specialization of get_one_byte. */

  /* back-door argument: */
  special_is_ok = 0;

  if (!size) {
    if (only_avail == -1) {
      /* We might need to break. */
      if (scheme_current_thread->external_break) {
	scheme_thread_block_enable_break(0.0, 1);
	scheme_current_thread->ran_some = 1;
      }
    }
    return 0;
  }
  if (!peek_skip)
    peek_skip = scheme_make_integer(0);

  ip = scheme_input_port_record(port);

  gs = ip->get_string_fun;
  ps = ip->peek_string_fun;

  while (1) {
    SCHEME_USE_FUEL(1);

    CHECK_PORT_CLOSED(who, "input", port, ip->closed);

    if (ip->input_lock)
      scheme_wait_input_allowed(ip, only_avail);

    if (only_avail == -1) {
      /* We might need to break. */
      if (scheme_current_thread->external_break) {
	scheme_thread_block_enable_break(0.0, 1);
	scheme_current_thread->ran_some = 1;
      }
    }

    if ((ip->ungotten_count || pipe_char_count(ip->peeked_read))
	&& (!total_got || !peek)) {
      intptr_t l, i;
      unsigned char *s;

      i = ip->ungotten_count;
      /* s will be in reverse order */

      if (peek) {
	if (!SCHEME_INTP(peek_skip) || (i < SCHEME_INT_VAL(peek_skip))) {
	  peek_skip = scheme_bin_minus(peek_skip, scheme_make_integer(i));
	  i = 0;
	} else {
	  i -= SCHEME_INT_VAL(peek_skip);
	  peek_skip = scheme_make_integer(0);
	}
      }

      if (i < size)
	l = i;
      else
	l = size;

      size -= l;
      s = (unsigned char *)ip->ungotten; /* Not GC-safe! */
      while (l--) {
	buffer[offset + got++] = s[--i];
      }
      s = NULL;

      if (!peek) {
	ip->ungotten_count = i;
        ip->slow = 1;
      }

      l = pipe_char_count(ip->peeked_read);
      if (size && l) {
	if (SCHEME_INTP(peek_skip) && (l > SCHEME_INT_VAL(peek_skip))) {
	  l -= SCHEME_INT_VAL(peek_skip);

	  if (l > size)
	    l = size;

	  if (l) {
	    scheme_get_byte_string("depipe", ip->peeked_read,
				   buffer, offset + got, l,
				   1, peek, peek_skip);
	    size -= l;
	    got += l;
	    peek_skip = scheme_make_integer(0);
	    if (!peek && ip->progress_evt)
	      post_progress(ip);
	  }
	} else
	  peek_skip = scheme_bin_minus(peek_skip, scheme_make_integer(l));
      }
      check_special = (!got || peek);
    } else
      check_special = 1;

    if (check_special && ip->ungotten_special) {
      if (!special_ok) {
	if (!peek) {
	  if (ip->progress_evt)
	    post_progress(ip);
	  ip->ungotten_special = NULL;
	}
	scheme_bad_time_for_special(who, port);
      }
      if (!peek) {
	ip->special = ip->ungotten_special;
	ip->ungotten_special = NULL;
      } else {
	if (peek_skip != scheme_make_integer(0))
	  scheme_bad_time_for_special(who, port);
      }

      if (!peek) {
	if (ip->p.position >= 0)
	  ip->p.position++;
	if (ip->p.count_lines)
	  inc_pos((Scheme_Port *)ip, 1);
      }

      if (!peek && ip->progress_evt)
	post_progress(ip);

      return SCHEME_SPECIAL;
    }

    if (got && ((only_avail == 1) || (only_avail == -1)))
      only_avail = 2;

    /* If we get this far in peek mode, ps is NULL, peek_skip is non-zero, and
       we haven't gotten anything so far, it means that we need to read before we
       can actually peek. Handle this case with a recursive peek that starts
       from the current position, then set peek_skip to 0 and go on. */
    while (peek && !ps && (peek_skip != scheme_make_integer(0)) && !total_got && !got
	   && (ip->pending_eof < 2)) {
      char *tmp;
      int v, pcc;
      intptr_t skip;
      Scheme_Cont_Frame_Data cframe;


#     define MAX_SKIP_TRY_AMOUNT 65536

      if (SCHEME_INTP(peek_skip)) {
	skip = SCHEME_INT_VAL(peek_skip);
	if (skip > MAX_SKIP_TRY_AMOUNT)
	  skip = MAX_SKIP_TRY_AMOUNT;
      } else
	skip = MAX_SKIP_TRY_AMOUNT;

      tmp = (char *)scheme_malloc_atomic(skip);
      pcc = pipe_char_count(ip->peeked_read);

      if (only_avail == -1) {
	/* To implement .../enable-break, we enable
	   breaks during the skip-ahead peek. */
	scheme_push_break_enable(&cframe, 1, 1);
      }

      v = scheme_get_byte_string_unless(who, port, tmp, 0, skip,
					(only_avail == 2) ? 2 : 0,
					1, scheme_make_integer(ip->ungotten_count + pcc),
					unless_evt);

      if (only_avail == -1) {
	scheme_pop_break_enable(&cframe, 0);
      }

      if (v == EOF) {
	ip->p.utf8state = 0;
	return EOF;
      } else if (v == SCHEME_SPECIAL) {
	ip->special = NULL;
	scheme_bad_time_for_special(who, port);
      } else if (v == skip) {
	peek_skip = scheme_bin_minus(peek_skip, scheme_make_integer(skip));
	/* Ok... ready to continue (if skip == peek_skip) */
      } else
	return 0;
    }

    if (size) {
      int nonblock;

      if (only_avail == 2) {
	if (got)
	  nonblock = 2;
	else
	  nonblock = 1;
      } else if (only_avail == -1)
	nonblock = -1;
      else
	nonblock = 0;

      if (unless_evt && SAME_TYPE(SCHEME_TYPE(unless_evt), scheme_progress_evt_type))
	unless_evt = SCHEME_PTR2_VAL(unless_evt);

      if (ip->pending_eof > 1) {
	ip->pending_eof = 1;
	gc = EOF;
      } else {
	/* Call port's get or peek function. But first, set up
	   an "unless" to detect other accesses of the port
	   if we block. */
	Scheme_Object *unless;
	  
	if (nonblock > 0) {
	  if (ip->unless)
	    unless = ip->unless;
	  else
	    unless = NULL;
	} else if (ip->unless_cache) {
	  if (ip->unless) {
	    unless = ip->unless;
	    /* Setting car to #f means that it can't be recycled */
	    SCHEME_CAR(unless) = scheme_false;
	  } else if (SCHEME_TRUEP(ip->unless_cache)) {
	    unless = ip->unless_cache;
	    ip->unless_cache = scheme_false;
	    ip->unless = unless;
	  } else {
	    unless = scheme_make_raw_pair(NULL, NULL);
	    ip->unless = unless;
	  }
	  if (unless_evt)
	    SCHEME_CDR(unless) = unless_evt;
	} else
	  unless = unless_evt;

	/* Finally, call port's get or peek: */
	if (peek && ps)
	  gc = ps(ip, buffer, offset + got, size, peek_skip, nonblock, unless);
	else {
	  gc = gs(ip, buffer, offset + got, size, nonblock, unless);

	  if (!peek && gc && ip->progress_evt
	      && (gc != EOF) 
	      && (gc != SCHEME_UNLESS_READY))
	    post_progress(ip);
	}

	/* Let other threads know that something happened,
	   and/or deregister this thread's request for information. */
	if (unless && ip->unless_cache) {
	  if (!SCHEME_CAR(unless)) {
	    /* Recycle "unless", since we were the only user */
	    ip->unless_cache = unless;
	    SCHEME_CDR(unless) = NULL;
	  } else {
	    if (SCHEME_TRUEP(SCHEME_CAR(unless))) {
	      /* gc should be SCHEME_UNLESS_READY; only a user
		 port without a peek can incorrectly produce something 
		 else */
	      if (gc == SCHEME_UNLESS_READY) {
		gc = 0;
	      }
	    } else if (gc) {
	      /* Notify other threads that something happened */
	      SCHEME_CAR(unless) = scheme_true;
	    }
	  }
	  ip->unless = NULL;
	}
      }

      if (gc == SCHEME_SPECIAL) {
	if (!got && !total_got && special_ok) {
	  if (!peek) {
	    if (ip->p.position >= 0)
	      ip->p.position++;
	    if (ip->p.count_lines)
	      inc_pos((Scheme_Port *)ip, 1);
	  }
	  
	  return SCHEME_SPECIAL;
	}

	if ((got || total_got) && only_avail) {
          ip->slow = 1;
	  ip->ungotten_special = ip->special;
	  ip->special = NULL;
	  gc = 0;
	} else {
	  ip->special = NULL;
	  scheme_bad_time_for_special(who, port);
	  return 0;
	}
      } else if (gc == EOF) {
	ip->p.utf8state = 0;
	if (!got && !total_got) {
	  if (peek && ip->pending_eof) {
	    ip->pending_eof = 2;
            ip->slow = 1;
          }
	  return EOF;
	}
	/* remember the EOF for next time */
	if (ip->pending_eof) {
	  ip->pending_eof = 2;
          ip->slow = 1;
        }
	gc = 0;
	size = 0; /* so that we stop */
      } else if (gc == SCHEME_UNLESS_READY) {
	gc = 0;
	size = 0; /* so that we stop */
      }
      mzAssert(gc >= 0);
    } else
      gc = 0;

    got += gc;
    if (peek)
      peek_skip = quick_plus(peek_skip, gc);
    size -= gc;

    if (!peek) {
      /****************************************************/
      /* Adjust position information for chars got so far */
      /****************************************************/

      /* We don't get here if SCHEME_SPECIAL is returned, so
	 the positions are updated separately in the two
	 returning places above. */

      if (ip->p.position >= 0)
	ip->p.position += got;
      if (ip->p.count_lines)
	do_count_lines((Scheme_Port *)ip, buffer, offset, got);
    } else if (!ps) {
      /***************************************************/
      /* save newly peeked string for future peeks/reads */
      /***************************************************/
      if (gc) {
        ip->slow = 1;
	if ((gc == 1) && !ip->ungotten_count && !ip->peeked_write)
	  ip->ungotten[ip->ungotten_count++] = buffer[offset];
	else {
	  if (!ip->peeked_write) {
	    Scheme_Object *rd, *wt;
	    scheme_pipe(&rd, &wt);
	    ip->peeked_read = rd;
	    ip->peeked_write = wt;
	  }

	  scheme_put_byte_string("peek", ip->peeked_write,
				 buffer, offset + got - gc, gc, 0);
	}
      }
    }

    offset += got;
    total_got += got;
    got = 0; /* for next round, if any */

    if (!size
	|| (total_got && ((only_avail == 1) || (only_avail == -1)))
	|| (only_avail == 2))
      break;

    /* Need to try to get more. */
  }
  
  return total_got;
}

intptr_t scheme_get_byte_string_special_ok_unless(const char *who,
					      Scheme_Object *port,
					      char *buffer, intptr_t offset, intptr_t size,
					      int only_avail,
					      int peek, Scheme_Object *peek_skip,
					      Scheme_Object *unless_evt)
{
  special_is_ok = 1;
  return scheme_get_byte_string_unless(who, port, buffer, offset, size, 
				       only_avail, peek, peek_skip, unless_evt);
}

intptr_t scheme_get_byte_string(const char *who,
			    Scheme_Object *port,
			    char *buffer, intptr_t offset, intptr_t size,
			    int only_avail,
			    int peek, Scheme_Object *peek_skip)
{
  return scheme_get_byte_string_unless(who, port,
				       buffer, offset, size,
				       only_avail,
				       peek, peek_skip,
				       NULL);
}

int scheme_unless_ready(Scheme_Object *unless)
{
  if (!unless)
    return 0;

  if (SCHEME_CAR(unless) && SCHEME_TRUEP(SCHEME_CAR(unless)))
    return 1;

  if (SCHEME_CDR(unless))
    return scheme_try_plain_sema(SCHEME_CDR(unless));

  return 0;
}


void scheme_wait_input_allowed(Scheme_Input_Port *ip, int nonblock)
{
  while (ip->input_lock) {
    scheme_post_sema_all(ip->input_giveup);
    scheme_wait_sema(ip->input_lock, nonblock ? -1 : 0);
  }
}

static void release_input_lock(Scheme_Input_Port *ip)
{
  scheme_post_sema_all(ip->input_lock);
  ip->input_lock = NULL;
  ip->input_giveup = NULL;

  if (scheme_current_thread->running & MZTHREAD_NEED_SUSPEND_CLEANUP)
    scheme_current_thread->running -= MZTHREAD_NEED_SUSPEND_CLEANUP;
}

static void elect_new_main(Scheme_Input_Port *ip)
{
  if (ip->input_extras_ready) {
    scheme_post_sema_all(ip->input_extras_ready);
    ip->input_extras = NULL;
    ip->input_extras_ready = NULL;
  }
}

static void release_input_lock_and_elect_new_main(void *_ip)
{
  Scheme_Input_Port *ip;

  ip = scheme_input_port_record(_ip);

  release_input_lock(ip);
  elect_new_main(ip);
}

static void check_suspended()
{
  if (scheme_current_thread->running & MZTHREAD_USER_SUSPENDED)
    scheme_thread_block(0.0);
}

static void remove_extra(void *ip_v)
{
  Scheme_Input_Port *ip;
  Scheme_Object *v = SCHEME_CDR(ip_v), *ll, *prev;

  ip = scheme_input_port_record(SCHEME_CAR(ip_v));

  prev = NULL;
  for (ll = ip->input_extras; ll; prev = ll, ll = SCHEME_CDR(ll)) {
    if (SAME_OBJ(ll, SCHEME_CDR(v))) {
      if (prev)
	SCHEME_CDR(prev) = SCHEME_CDR(ll);
      else
	ip->input_extras = SCHEME_CDR(ll);
      SCHEME_CDR(ll) = NULL;
      break;
    }
  }

  /* Tell the main commit thread (if any) to reset */
  if (ip->input_giveup)
    scheme_post_sema_all(ip->input_giveup);
}

static int complete_peeked_read_via_get(Scheme_Input_Port *ip,
					intptr_t size)
{
  Scheme_Get_String_Fun gs;
  int did;
  char *buf, _buf[16];
  int buf_size = 16;
  buf = _buf;
  
  did = 0;
  
  /* Target event is ready, so commit must succeed */
  
  /* First remove ungotten_count chars */
  if (ip->ungotten_count) {
    int i, amt;

    if (ip->ungotten_count > size) {
      amt = size;
      ip->ungotten_count -= size;
    } else {
      amt = ip->ungotten_count;
      size -= ip->ungotten_count;
      ip->ungotten_count = 0;
    }

    if (ip->p.position >= 0)
      ip->p.position += amt;
    if (ip->p.count_lines) {
      if (buf_size < amt) {
        buf = scheme_malloc_atomic(amt);
        buf_size = amt;
      }
      for (i = 0; i < amt; i++) {
        buf[i] = ip->ungotten[ip->ungotten_count + amt - i - 1];
      }
      do_count_lines((Scheme_Port *)ip, buf, 0, amt);
    }

    if (ip->progress_evt)
      post_progress(ip);
    did = 1;
  }
  
  if (size) {
    Scheme_Input_Port *pip;

    if (ip->peek_string_fun) {
      /* If the port supplies its own peek, then we don't
	 have peeked_r, so pass NULL as a buffer to the port's
	 read proc. The read proc must not block. */
      gs = ip->get_string_fun;
      pip = ip;
    } else {
      /* Otherwise, peek was implemented through peeked_{w,r}: */
      if (ip->peeked_read) {
	int cnt;
	cnt = pipe_char_count(ip->peeked_read);
	if ((cnt < size) && (ip->pending_eof == 2))
	  ip->pending_eof = 1;
	pip = (Scheme_Input_Port *)ip->peeked_read;
	gs = pip->get_string_fun;
      } else {
	gs = NULL;
	pip = NULL;
      }
    }
      
    if (gs) {
      if (ip->p.count_lines) {
        if (buf_size < size) {
          buf = scheme_malloc_atomic(size);
          buf_size = size;
        }
      } else
        buf = NULL;
      size = gs(pip, buf, 0, size, 1, NULL);
      if (size > 0) {
	if (ip->progress_evt)
	  post_progress(ip);
        if (ip->p.position >= 0)
          ip->p.position += size;
        if (buf)
          do_count_lines((Scheme_Port *)ip, buf, 0, size);
	did = 1;
      }
    }
  }
   
  return did;
}

static Scheme_Object *return_data(void *data, int argc, Scheme_Object **argv)
{
  return (Scheme_Object *)data;
}

int scheme_peeked_read_via_get(Scheme_Input_Port *ip,
			       intptr_t _size,
			       Scheme_Object *unless_evt,
			       Scheme_Object *_target_evt)
{
  Scheme_Object * volatile v, *sema, *a[3], ** volatile aa, * volatile l;
  volatile intptr_t size = _size;
  volatile int n, current_leader = 0;
  volatile Scheme_Type t;
  Scheme_Object * volatile target_evt = _target_evt;

  /* Check whether t's event value is known to be always itself: */
  t = SCHEME_TYPE(target_evt);
  if (!SAME_TYPE(t, scheme_sema_type)
      && !SAME_TYPE(t, scheme_channel_put_type)
      && !SAME_TYPE(t, scheme_always_evt_type)
      && !SAME_TYPE(t, scheme_never_evt_type)
      && !SAME_TYPE(t, scheme_semaphore_repost_type)) {
    /* Make an event whose value is itself */
    a[0] = target_evt;
    v = scheme_make_closed_prim(return_data, target_evt);
    a[1] = v;
    target_evt = scheme_wrap_evt(2, a);
    ((Scheme_Closed_Primitive_Proc *)v)->data = target_evt;
  }

  /* This commit implementation is essentially CML style, but we avoid
     actually allocating a manager thread. Instead the various
     committing threads elect a leader, and we rely on being in the
     kernel to detect when the leader is killed or suspended, in which
     case we elect a new leader. */

  while (1) {
    if (scheme_wait_sema(unless_evt, 1)) {
      if (current_leader)
	elect_new_main(ip);
      return 0;
    }

    if (!current_leader && ip->input_giveup) {
      /* Some other thread is already trying to commit.
	 Ask it to sync on our target, too */
      v = scheme_make_pair(scheme_make_integer(_size), target_evt);
      l = scheme_make_raw_pair(v, ip->input_extras);
      ip->input_extras = l;

      scheme_post_sema_all(ip->input_giveup);

      if (!ip->input_extras_ready) {
	sema = scheme_make_sema(0);
	ip->input_extras_ready = sema;
      }

      a[0] = ip->input_extras_ready;
      l = scheme_make_pair((Scheme_Object *)ip, v);
      BEGIN_ESCAPEABLE(remove_extra, l);
      scheme_sync(1, a);
      END_ESCAPEABLE();

      if (!SCHEME_CDR(v)) {
	/* We were selected, so the commit succeeded. */
	return SCHEME_TRUEP(SCHEME_CAR(v)) ? 1 : 0;
      }
    } else {
      /* No other thread is trying to commit. This one is hereby
	 elected "main" if multiple threads try to commit. */

      if (SAME_TYPE(t, scheme_always_evt_type)) {
	/* Fast path: always-evt is ready */
	return complete_peeked_read_via_get(ip, size);
      }

      /* This sema makes other threads wait before reading: */
      sema = scheme_make_sema(0);
      ip->input_lock = sema;
      ip->slow = 1;
      
      /* This sema lets other threads try to make progress,
	 if the current target doesn't work out */
      sema = scheme_make_sema(0);
      ip->input_giveup = sema;
      
      if (ip->input_extras) {
	/* There are other threads trying to commit, and
	   as main thread, we'll help them out. */
	n = 3;
	for (l = ip->input_extras; l; l = SCHEME_CDR(l)) {
	  n++;
	}
	aa = MALLOC_N(Scheme_Object *, n);
	n = 3;
	for (l = ip->input_extras; l; l = SCHEME_CDR(l)) {
	  aa[n++] = SCHEME_CDR(SCHEME_CAR(l));
	}
      } else {
	/* This is the only thread trying to commit */
	n = 3;
	aa = a;
      }

      /* Suspend here is a problem if another thread
	 tries to commit, because this thread will be
	 responsible for multiplexing the commits. That's
	 why the thread waits on its own suspend event. */
      
      aa[0] = target_evt;
      aa[1] = ip->input_giveup;
      v = scheme_get_thread_suspend(scheme_current_thread);
      aa[2] = v;

      scheme_current_thread->running |= MZTHREAD_NEED_SUSPEND_CLEANUP;
      BEGIN_ESCAPEABLE(release_input_lock_and_elect_new_main, ip);
      v = scheme_sync(n, aa);
      END_ESCAPEABLE();

      release_input_lock(ip);
      
      if (SAME_OBJ(v, target_evt)) {
	int r;
	elect_new_main(ip);
	r = complete_peeked_read_via_get(ip, size);
	check_suspended();
	return r;
      }

      if (n > 3) {
	/* Check whether one of the others was selected: */
	for (l = ip->input_extras; l; l = SCHEME_CDR(l)) {
	  if (SAME_OBJ(v, SCHEME_CDR(SCHEME_CAR(l)))) {
	    /* Yep. Clear the cdr to tell the relevant thread
	       that it was selected, and reset the extras. */
	    v = SCHEME_CAR(l);
	    SCHEME_CDR(v) = NULL;
	    size = SCHEME_INT_VAL(SCHEME_CAR(v));
	    elect_new_main(ip);
	    if (complete_peeked_read_via_get(ip, size))
	      SCHEME_CAR(v) = scheme_true;
	    else
	      SCHEME_CAR(v) = scheme_false;
	    check_suspended();
	    return 0;
	  }
	}
      }

      if (scheme_current_thread->running & MZTHREAD_USER_SUSPENDED) {
	elect_new_main(ip);
	current_leader = 0;
	check_suspended();
      } else {
	current_leader = 1;
	
	/* Technically redundant, but avoid a thread swap
	   if we know the commit isn't going to work: */
	if (scheme_wait_sema(unless_evt, 1)) {
	  elect_new_main(ip);
	  return 0;
	}
      
	scheme_thread_block(0.0);
      }
    }
  }
}

int scheme_peeked_read(Scheme_Object *port,
		       intptr_t size,
		       Scheme_Object *unless_evt,
		       Scheme_Object *target_evt)
{
  Scheme_Input_Port *ip;
  Scheme_Peeked_Read_Fun pr;
  
  ip = scheme_input_port_record(port);

  unless_evt = SCHEME_PTR2_VAL(unless_evt);

  pr = ip->peeked_read_fun;

  return pr(ip, size, unless_evt, target_evt);
}

Scheme_Object *scheme_progress_evt_via_get(Scheme_Input_Port *port)
{
  Scheme_Object *sema;

  if (port->progress_evt)
    return port->progress_evt;

  sema = scheme_make_sema(0);

  port->progress_evt = sema;
  port->slow = 1;

  return sema;
}

Scheme_Object *scheme_progress_evt(Scheme_Object *port)
{  
  Scheme_Input_Port *ip;
  
  ip = scheme_input_port_record(port);
  
  if (ip->progress_evt_fun) {
    Scheme_Progress_Evt_Fun ce;
    Scheme_Object *evt, *o;

    ce = ip->progress_evt_fun;

    evt = ce(ip);

    o = scheme_alloc_object();
    o->type = scheme_progress_evt_type;
    SCHEME_PTR1_VAL(o) = (Scheme_Object *)port;
    SCHEME_PTR2_VAL(o) = evt;

    return o;
  }

  return NULL;
}

static int progress_evt_ready(Scheme_Object *evt, Scheme_Schedule_Info *sinfo)
{
  scheme_set_sync_target(sinfo, SCHEME_PTR2_VAL(evt), evt, NULL, 0, 1, NULL);
  return 0;
}

static int closed_evt_ready(Scheme_Object *evt, Scheme_Schedule_Info *sinfo)
{
  scheme_set_sync_target(sinfo, SCHEME_PTR_VAL(evt), evt, NULL, 0, 1, NULL);
  return 0;
}

intptr_t scheme_get_char_string(const char *who,
			    Scheme_Object *port,
			    mzchar *buffer, intptr_t offset, intptr_t size,
			    int peek, Scheme_Object *peek_skip)
{
  int ahead_skip = 0;
  char *s;
  int total_got = 0, bsize, leftover = 0, got;

  /* read_string_byte_buffer helps avoid allocation */
  if (read_string_byte_buffer) {
    s = read_string_byte_buffer;
    read_string_byte_buffer = NULL;
  } else
    s = (char *)scheme_malloc_atomic(READ_STRING_BYTE_BUFFER_SIZE);

  while (1) {
    /* Since we want "size" more chars and we don't have leftovers, we
       need at least "size" more bytes.

       "leftover" is the number of bytes (<< READ_STRING_BYTE_BUFFER_SIZE) that
       we already have toward the first character. If the next
       character doesn't continue a leftover sequence, the next
       character actually belongs to a (leftover+1)th character. Thus,
       if leftover is positive and we're not merely peeking, ask for
       at leat one byte, but otherwise no more than size - leftover
       bytes. If size is 1, then we are forced to peek in all cases.

       Overall, if the size is big enough, we only read as many
       characters as our buffer holds. */

    bsize = size;
    if (leftover) {
      bsize -= leftover;
      if (bsize < 1) {
	/* This is the complex case. Need to peek a byte to see
	   whether it continues the leftover sequence or ends it an in
	   an error. */
	if (!peek_skip)
	  peek_skip = scheme_make_integer(0);
	special_is_ok = 1;
	got = scheme_get_byte_string_unless(who, port,
					    s, leftover, 1,
					    0, 1 /* => peek */, 
					    quick_plus(peek_skip, ahead_skip),
					    NULL);
	if (got > 0) {
	  intptr_t ulen, glen;
	  glen = scheme_utf8_decode_as_prefix((const unsigned char *)s, 0, got + leftover,
					      buffer, offset, offset + size,
					      &ulen, 0, 0xFFFD);
	  if (glen && (ulen < got + leftover)) {
	    /* Got one, with a decoding error. If we weren't peeking,
	       don't read the lookahead bytes after all, yet. */
	    total_got++;
	    bsize = 0;
	    ahead_skip++;
	    size--;
	    offset++;
	    /* leftover stays the same */
	    memmove(s, s + 1, leftover);
	  } else {
	    /* Either we got one character, or we're still continuing. */
	    ahead_skip++;
	    if (!glen) {
	      /* Continuing */
	      leftover++;
	    } else {
	      /* Got one (no encoding error) */
	      leftover = 0;
	      offset++;
	      --size;
	      total_got++;
	      if (!peek) {
		/* Read the lookahead bytes and discard them */
		scheme_get_byte_string_unless(who, port,
					      s, 0, ahead_skip,
					      0, 0, scheme_make_integer(0),
					      NULL);
	      } else {
		peek_skip = quick_plus(peek_skip, ahead_skip);
	      }
	      ahead_skip = 0;
	    }
	    /* Continue with the normal decoing process (but get 0
	       more characters this time around) */
	    bsize = 0;
	  }
	} else {
	  /* Either EOF or SPECIAL -- either one ends the leftover
	     sequence in an error. We may have more leftover chars
	     than we need, but they haven't been read, yet. */
	  while (leftover && size) {
	    buffer[offset++] = 0xFFFD;
	    total_got++;
	    --leftover;
	    --size;
	  }
	  return total_got;
	}
      }
    }

    if (bsize) {
      /* Read bsize bytes */
      if (bsize + leftover > READ_STRING_BYTE_BUFFER_SIZE)
	bsize = READ_STRING_BYTE_BUFFER_SIZE - leftover;
      
      got = scheme_get_byte_string_unless(who, port,
					  s, leftover, bsize,
					  0, peek, peek_skip,
					  NULL);
    } else
      got = 0;

    if (got >= 0) {
      intptr_t ulen, glen;

      glen = scheme_utf8_decode_as_prefix((const unsigned char *)s, 0, got + leftover,
					  buffer, offset, offset + size,
					  &ulen, 0, 0xFFFD);
      
      total_got += glen;
      if (glen == size) {
	/* Got enough */
	read_string_byte_buffer = s;
	return total_got;
      }
      offset += glen;
      size -= glen;
      leftover = (got + leftover) - ulen;
      memmove(s, s + ulen, leftover);
      if (peek) {
	peek_skip = quick_plus(peek_skip, got);
      }
    } else {
      read_string_byte_buffer = s;

      /* Leftover bytes must be decoding-error bytes: */
      while (leftover) {
	buffer[offset++] = 0xFFFD;
	total_got++;
	--leftover;
      }

      if (!total_got)
	return got; /* must be EOF */
      else
	return total_got;
    }
  }
}

MZ_DO_NOT_INLINE(static intptr_t get_one_byte_slow(const char *who,
                                                   Scheme_Object *port,
                                                   char *buffer, intptr_t offset,
                                                   int only_avail));

static intptr_t get_one_byte_slow(const char *who,
                                  Scheme_Object *port,
                                  char *buffer, intptr_t offset,
                                  int only_avail)
{
  Scheme_Input_Port *ip;
  intptr_t gc;
  int special_ok = special_is_ok;
  Scheme_Get_String_Fun gs;

  special_is_ok = 0;

  ip = scheme_input_port_record(port);

  CHECK_PORT_CLOSED(who, "input", port, ip->closed);

  if (ip->input_lock)
    scheme_wait_input_allowed(ip, only_avail);

  if (ip->ungotten_count) {
    buffer[offset] = ip->ungotten[--ip->ungotten_count];
    gc = 1;
  } else if (ip->peeked_read && pipe_char_count(ip->peeked_read)) {
    int ch;
    ch = scheme_get_byte(ip->peeked_read);
    buffer[offset] = ch;
    gc = 1;
  } else if (ip->ungotten_special) {
    if (ip->progress_evt)
      post_progress(ip);
    if (!special_ok) {
      ip->ungotten_special = NULL;
      scheme_bad_time_for_special(who, port);
      return 0;
    }
    ip->special = ip->ungotten_special;
    ip->ungotten_special = NULL;
    if (ip->p.position >= 0)
      ip->p.position++;
    if (ip->p.count_lines)
      inc_pos((Scheme_Port *)ip, 1);
    return SCHEME_SPECIAL;
  } else {
    if (ip->pending_eof > 1) {
      ip->pending_eof = 1;
      return EOF;
    } else {
      if (!ip->progress_evt && !ip->p.count_lines)
        ip->slow = 0;

      /* Call port's get function. */
      gs = ip->get_string_fun;

      gc = gs(ip, buffer, offset, 1, 0, NULL);
	
      if (ip->progress_evt && (gc > 0))
        post_progress(ip);

      if (gc < 1) {
        if (gc == SCHEME_SPECIAL) {
          if (special_ok) {
            if (ip->p.position >= 0)
              ip->p.position++;
            if (ip->p.count_lines)
              inc_pos((Scheme_Port *)ip, 1);
            return SCHEME_SPECIAL;
          } else {
            scheme_bad_time_for_special(who, port);
            return 0;
          }
        } else if (gc == EOF) {
          ip->p.utf8state = 0;
          return EOF;
        } else {
          /* didn't get anything the first try, so use slow path: */
          special_is_ok = special_ok;
          return scheme_get_byte_string_unless(who, port,
                                               buffer, offset, 1,
                                               0, 0, NULL, NULL);
        }
      }
    }
  }

  /****************************************************/
  /* Adjust position information for chars got so far */
  /****************************************************/
  
  if (ip->p.position >= 0)
    ip->p.position++;
  if (ip->p.count_lines)
    do_count_lines((Scheme_Port *)ip, buffer, offset, 1);
  
  return gc;
}

static MZ_INLINE intptr_t get_one_byte(GC_CAN_IGNORE const char *who,
                                       Scheme_Object *port, char *buffer)
{
  if (!special_is_ok && SCHEME_INPORTP(port)) {
    GC_CAN_IGNORE Scheme_Input_Port *ip;
    ip = (Scheme_Input_Port *)port;
    if (!ip->slow) {
      Scheme_Get_String_Fun gs;
      intptr_t v;

      gs = ip->get_string_fun;

      v = gs(ip, buffer, 0, 1, 0, NULL);
    
      if (v) {
        if (v == SCHEME_SPECIAL) {
          scheme_bad_time_for_special(who, port);
        }
        if (v != EOF) {
          ip = (Scheme_Input_Port *)port; /* since `ip is ignored by GC */
          if (ip->p.position >= 0)
            ip->p.position++;
        }

        return v;
      }
    }
  }
  
  return get_one_byte_slow(who, port, buffer, 0, 0);
}

int
scheme_getc(Scheme_Object *port)
{
  char s[MAX_UTF8_CHAR_BYTES];
  unsigned int r[1];
  int v, delta = 0;

  while(1) {
    if (delta) {
      v = scheme_get_byte_string_unless("read-char", port,
					s, delta, 1,
					0,
					delta > 0, scheme_make_integer(delta-1),
					NULL);
    } else {
      v = get_one_byte("read-char", port, s);
    }

    if ((v == EOF) || (v == SCHEME_SPECIAL)) {
      if (!delta)
	return v;
      else {
	/* This counts as a decoding error. The high bit
	   on the first character must be set. */
	return 0xFFFD;
      }
    } else {
      v = scheme_utf8_decode_prefix((const unsigned char *)s, delta + 1, r, 0);
      if (v > 0) {
	if (delta) {
	  /* Need to read the peeked bytes (will ignore) */
	  v = scheme_get_byte_string_unless("read-char", port,
					    s, 0, delta,
					    0,
					    0, 0,
					    NULL);
	}
	return r[0];
      } else if (v == -2) {
	/* -2 => decoding error */
	return 0xFFFD;
      } else if (v == -1) {
	/* In middle of sequence; start/continue peeking bytes */
	delta++;
      }
    }
  }
}

int
scheme_get_byte(Scheme_Object *port)
{
  char s[1];
  int v;

  v = get_one_byte("read-byte", port, s);

  if ((v == EOF) || (v == SCHEME_SPECIAL))
    return v;
  else
    return ((unsigned char *)s)[0];
}

int
scheme_getc_special_ok(Scheme_Object *port)
{
  special_is_ok = 1;
  return scheme_getc(port);
}

int
scheme_get_byte_special_ok(Scheme_Object *port)
{
  special_is_ok = 1;
  return scheme_get_byte(port);
}

intptr_t scheme_get_bytes(Scheme_Object *port, intptr_t size, char *buffer, int offset)
{
  int n;
  int only_avail = 0;

  if (size < 0) {
    size = -size;
    only_avail = 1;
  }

  n = scheme_get_byte_string_unless("read-bytes", port,
				    buffer, offset, size,
				    only_avail,
				    0, 0,
				    NULL);

  if (n == EOF)
    n = 0;

  mzAssert(n >= 0);

  return n;
}

int scheme_peek_byte_skip(Scheme_Object *port, Scheme_Object *skip, Scheme_Object *unless_evt)
{
  char s[1];
  int v;

  v = scheme_get_byte_string_unless("peek-byte", port,
				    s, 0, 1,
				    0,
				    1, skip,
				    unless_evt);

  if ((v == EOF) || (v == SCHEME_SPECIAL))
    return v;
  else
    return ((unsigned char *)s)[0];
}

int scheme_peek_byte(Scheme_Object *port)
{
  return scheme_peek_byte_skip(port, NULL, NULL);
}

int
scheme_peek_byte_special_ok_skip(Scheme_Object *port, Scheme_Object *skip, Scheme_Object *unless_evt)
{
  special_is_ok = 1;
  return scheme_peek_byte_skip(port, skip, unless_evt);
}

static int do_peekc_skip(Scheme_Object *port, Scheme_Object *skip, 
			 int only_avail, int *unavail)
{
  char s[MAX_UTF8_CHAR_BYTES];
  unsigned int r[1];
  int v, delta = 0;
  Scheme_Object *skip2;

  if (unavail)
    *unavail = 0;

  while(1) {
    if (delta) {
      if (!skip)
	skip = scheme_make_integer(0);
      skip2 = quick_plus(skip, delta);
    } else
      skip2 = skip;

    v = scheme_get_byte_string_unless("peek-char", port,
				      s, delta, 1,
				      only_avail,
				      1, skip2,
				      NULL);

    if (!v) {
      if (unavail)
        *unavail = 1;
      return 0;
    }

    if ((v == EOF) || (v == SCHEME_SPECIAL)) {
      if (!delta)
	return v;
      else {
	/* This counts as a decoding error, so return 0xFFFD */
	return 0xFFFD;
      }
    } else {
      v = scheme_utf8_decode_prefix((const unsigned char *)s, delta + 1, r, 0);
      if (v > 0)
	return r[0];
      else if (v == -2) {
	/* -2 => decoding error */
	return 0xFFFD;
      } else if (v == -1) {
	/* In middle of sequence - keep getting bytes. */
	delta++;
      }
    }
  }
}

int scheme_peekc_skip(Scheme_Object *port, Scheme_Object *skip)
{
  return do_peekc_skip(port, skip, 0, NULL);
}

int scheme_peekc(Scheme_Object *port)
{
  return scheme_peekc_skip(port, scheme_make_integer(0));
}

int
scheme_peekc_special_ok_skip(Scheme_Object *port, Scheme_Object *skip)
{
  special_is_ok = 1;
  return scheme_peekc_skip(port, skip);
}

int
scheme_peekc_special_ok(Scheme_Object *port)
{
  return scheme_peekc_special_ok_skip(port, scheme_make_integer(0));
}

int scheme_peekc_is_ungetc(Scheme_Object *port)
{
  Scheme_Input_Port *ip;

  ip = scheme_input_port_record(port);

  return !ip->peek_string_fun;
}

Scheme_Object *make_read_write_evt(Scheme_Type type, 
				   Scheme_Object *port, Scheme_Object *skip, 
				   char *str, intptr_t start, intptr_t size)
{
  Scheme_Read_Write_Evt *rww;

  rww = MALLOC_ONE_TAGGED(Scheme_Read_Write_Evt);
  rww->so.type = type;
  rww->port = port;
  rww->v = skip;
  rww->str = str;
  rww->start = start;
  rww->size = size;

  return (Scheme_Object *)rww;
}

static int rw_evt_ready(Scheme_Object *_rww, Scheme_Schedule_Info *sinfo)
{
  Scheme_Read_Write_Evt *rww = (Scheme_Read_Write_Evt *)_rww;
  intptr_t v;

  if (sinfo->false_positive_ok) {
    /* Causes the thread to swap in, which we need in case there's an
       exception: */
    sinfo->potentially_false_positive = 1;
    return 1;
  }
  
  if (rww->v) {
    Scheme_Output_Port *op;
    Scheme_Write_Special_Fun ws;

    op = scheme_output_port_record(rww->port);
    ws = op->write_special_fun;

    v = ws(op, rww->v, 1);
    if (v) {
      scheme_set_sync_target(sinfo, scheme_true, NULL, NULL, 0, 0, NULL);
      return 1;
    } else	
      return 0;
  } else {
    v = scheme_put_byte_string("write-evt", rww->port,
			       rww->str, rww->start, rww->size,
			       2);
    if (v < 1)
      return 0;
    else if (!v && rww->size)
      return 0;
    else {
      scheme_set_sync_target(sinfo, scheme_make_integer(v), NULL, NULL, 0, 0, NULL);
      return 1;
    }
  }
}

static void rw_evt_wakeup(Scheme_Object *_rww, void *fds)
{
  Scheme_Read_Write_Evt *rww = (Scheme_Read_Write_Evt *)_rww;

  if (rww->port) {
    if (rww->so.type == scheme_write_evt_type)
      output_need_wakeup(rww->port, fds);
    else
      scheme_need_wakeup(rww->port, fds);
  }
}

Scheme_Object *scheme_write_evt_via_write(Scheme_Output_Port *port,
					  const char *str, intptr_t offset, intptr_t size)
{
  return make_read_write_evt(scheme_write_evt_type, (Scheme_Object *)port, NULL, 
			     (char *)str, offset, size);
}

Scheme_Object *scheme_write_special_evt_via_write_special(Scheme_Output_Port *port, 
							  Scheme_Object *special)
{
  return make_read_write_evt(scheme_write_evt_type, (Scheme_Object *)port, special, 
			     NULL, 0, 1);
}
	
Scheme_Object *scheme_make_write_evt(const char *who, Scheme_Object *port,
				     Scheme_Object *special, char *str, intptr_t start, intptr_t size)
{
  Scheme_Output_Port *op;

  op = scheme_output_port_record(port);

  if (!special) {
    if (op->write_string_evt_fun) {
      Scheme_Write_String_Evt_Fun wse;
      wse = op->write_string_evt_fun;
      return wse(op, str, start, size);
    }
  } else {
    if (op->write_special_evt_fun) {
      Scheme_Write_Special_Evt_Fun wse = op->write_special_evt_fun;
      return wse(op, special);
    }
  }

  scheme_contract_error("write-bytes-avail-evt",
                        "port does not support atomic writes",
                        "port", 1, port,
                        NULL);
  return NULL;
}

void
scheme_ungetc (int ch, Scheme_Object *port)
{
  Scheme_Input_Port *ip;

  ip = scheme_input_port_record(port);

  CHECK_PORT_CLOSED("#<primitive:peek-port-char>", "input", port, ip->closed);

  ip->slow = 1;

  if (ch == EOF) {
    if (ip->pending_eof) /* non-zero means that EOFs are tracked */
      ip->pending_eof = 2;
    return;
  } else if (ch == SCHEME_SPECIAL) {
    ip->ungotten_special = ip->special;
    ip->special = NULL;
  } else if (ch > 127) {
    unsigned char e[MAX_UTF8_CHAR_BYTES];
    unsigned int us[1];
    int len;

    us[0] = ch;
    len = scheme_utf8_encode_all(us, 1, e);

    if (ip->ungotten_count + len >= 24)
      scheme_signal_error("ungetc overflow");
    while (len) {
      ip->ungotten[ip->ungotten_count++] = e[--len];
    }
  } else {
    if (ip->ungotten_count == 24)
      scheme_signal_error("ungetc overflow");
    ip->ungotten[ip->ungotten_count++] = ch;
  }

  if (ip->p.position > 0)
    --ip->p.position;
  if (ip->p.count_lines) {
    --ip->p.column;
    --ip->p.readpos;
    if (!(--ip->p.charsSinceNewline)) {
      mzAssert(ip->p.lineNumber > 0);
      --ip->p.lineNumber;
      ip->p.column = ip->p.oldColumn;
    } else if (ch == '\t')
      ip->p.column = ip->p.oldColumn;
  }
}

int byte_input_ready (Scheme_Object *port, Scheme_Schedule_Info *sinfo)
{
  Scheme_Input_Port *ip;
  int retval;

  ip = scheme_input_port_record(port);

  CHECK_PORT_CLOSED("char-ready?", "input", port, ip->closed);

  if (ip->slow
      && (ip->ungotten_count || ip->ungotten_special
          || (ip->pending_eof > 1)
          || pipe_char_count(ip->peeked_read)))
    retval = 1;
  else {
    Scheme_In_Ready_Fun_FPC f;
    f = (Scheme_In_Ready_Fun_FPC)ip->byte_ready_fun;
    retval = f(ip, NULL);
  }

  return retval;
}

int
scheme_byte_ready (Scheme_Object *port)
{
  return byte_input_ready(port, NULL);
}

int
scheme_char_ready (Scheme_Object *port)
{
  int unavail;

  if (!scheme_byte_ready(port))
    return 0;

  do_peekc_skip(port, scheme_make_integer(0), 2, &unavail);
  
  return !unavail;
}

Scheme_Object *scheme_get_special(Scheme_Object *port,
				  Scheme_Object *src, intptr_t line, intptr_t col, intptr_t pos,
				  int peek, Scheme_Hash_Table **for_read)
{
  int cnt;
  Scheme_Object *a[4], *special;
  Scheme_Input_Port *ip;
  Scheme_Cont_Frame_Data cframe;

  SCHEME_USE_FUEL(1);

  ip = scheme_input_port_record(port);

  /* Only `read' and similar internals should call this function. A
     caller must should ensure that there are no ungotten
     characters. */

  if (ip->ungotten_count) {
    scheme_signal_error("ungotten characters at get-special");
    return NULL;
  }
  if (!ip->special) {
    scheme_signal_error("no ready special");
    return NULL;
  }

  CHECK_PORT_CLOSED("#<primitive:get-special>", "input", port, ip->closed);

  special = ip->special;
  ip->special = NULL;

  if (peek) {
    /* do location increment, since read didn't */
    if (line > 0)
      line++;
    if (col >= 0)
      col++;
    if (pos > 0)
      pos++;
  }

  a[0] = special;
  if (!src && scheme_check_proc_arity(NULL, 2, 0, 1, a))
    cnt = 0;
  else {
    cnt = 4;
    a[0] = (src ? src : scheme_false);
    a[1] = (line > 0) ? scheme_make_integer(line) : scheme_false;
    a[2] = (col > 0) ? scheme_make_integer(col-1) : scheme_false;
    a[3] = (pos > 0) ? scheme_make_integer(pos) : scheme_false;
  }

  scheme_push_continuation_frame(&cframe);
  scheme_set_in_read_mark(src, for_read);

  special = scheme_apply(special, cnt, a);

  scheme_pop_continuation_frame(&cframe);

  return special;
}

static Scheme_Object *do_get_ready_special(Scheme_Object *port, 
					   Scheme_Object *stxsrc,
					   int peek,
					   Scheme_Hash_Table **ht)
{
  intptr_t line, col, pos;

  if (!stxsrc) {
    Scheme_Input_Port *ip;
    ip = scheme_input_port_record(port);
    stxsrc = ip->name;
  }

  /* Don't use scheme_tell_all(), because we always want the
     Racket-computed values here. */
  line = scheme_tell_line(port);
  col = scheme_tell_column(port);
  pos = scheme_tell(port);

  return scheme_get_special(port, stxsrc, line, col, pos, peek, ht);
}

Scheme_Object *scheme_get_ready_read_special(Scheme_Object *port, Scheme_Object *stxsrc, Scheme_Hash_Table **ht)
{
  return do_get_ready_special(port, stxsrc, 0, ht);
}

Scheme_Object *scheme_get_ready_special(Scheme_Object *port, 
					Scheme_Object *stxsrc,
					int peek)
{
  return do_get_ready_special(port, stxsrc, peek, NULL);
}

void scheme_bad_time_for_special(const char *who, Scheme_Object *port)
{
  scheme_contract_error(who, "non-character in an unsupported context",
                        "port", 1, port,
                        NULL);
}

static Scheme_Object *check_special_args(void *sbox, int argc, Scheme_Object **argv)
{
  Scheme_Object *special;
  Scheme_Cont_Frame_Data cframe;

  if (SCHEME_TRUEP(argv[1]))
    if (!scheme_nonneg_exact_p(argv[1]) || (SAME_OBJ(argv[1], scheme_make_integer(0))))
      scheme_wrong_contract("read-special", "(or/c exact-positive-integer? #f)", 1, argc, argv);
  if (SCHEME_TRUEP(argv[2]))
    if (!scheme_nonneg_exact_p(argv[2]))
      scheme_wrong_contract("read-special", "(or/c exact-nonnegative-integer? #f)", 2, argc, argv);
  if (SCHEME_TRUEP(argv[3]))
    if (!scheme_nonneg_exact_p(argv[3]) || (SAME_OBJ(argv[3], scheme_make_integer(0))))
      scheme_wrong_contract("read-special", "(or/c exact-positive-integer? #f)", 3, argc, argv);

  special = *(Scheme_Object **)sbox;
  if (!special)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "read-special: cannot be called a second time");
  *(Scheme_Object **)sbox = NULL;

  scheme_push_continuation_frame(&cframe);
  scheme_set_in_read_mark(NULL, NULL);

  special = _scheme_apply(special, 4, argv);

  scheme_pop_continuation_frame(&cframe);

  return special;
}

Scheme_Object *scheme_get_special_proc(Scheme_Object *inport)
{
  Scheme_Object *special, **sbox;
  Scheme_Input_Port *ip;

  ip = scheme_input_port_record(inport);
  special = ip->special;
  ip->special = NULL;
  
  sbox = MALLOC_ONE(Scheme_Object *);
  *sbox = special;
  return scheme_make_closed_prim_w_arity(check_special_args, 
					 sbox, "read-special",
					 4, 4);
}

void
scheme_need_wakeup (Scheme_Object *port, void *fds)
{
  Scheme_Input_Port *ip;

  ip = scheme_input_port_record(port);

  if (ip->need_wakeup_fun) {
    Scheme_Need_Wakeup_Input_Fun f = ip->need_wakeup_fun;
    f(ip, fds);
  }
}

#define CHECK_IOPORT_CLOSED(who, port) \
        if (SCHEME_INPORTP((Scheme_Object *)port)) {                          \
          CHECK_PORT_CLOSED(who, "input", port, ((Scheme_Input_Port *)port)->closed); \
        } else { \
          CHECK_PORT_CLOSED(who, "output", port, ((Scheme_Output_Port *)port)->closed); \
        }

intptr_t
scheme_tell (Scheme_Object *port)
{
  Scheme_Port *ip;
  intptr_t pos;

  ip = scheme_port_record(port);
  
  CHECK_IOPORT_CLOSED("get-file-position", ip);

  if (!ip->count_lines || (ip->position < 0))
    pos = ip->position;
  else
    pos = ip->readpos;

  return pos;
}

intptr_t
scheme_tell_line (Scheme_Object *port)
{
  Scheme_Port *ip;
  intptr_t line;

  ip = scheme_port_record(port);

  if (!ip->count_lines || (ip->position < 0))
    return -1;

  CHECK_IOPORT_CLOSED("get-file-line", ip);

  line = ip->lineNumber;

  return line;
}

intptr_t
scheme_tell_column (Scheme_Object *port)
{
  Scheme_Port *ip;
  intptr_t col;

  ip = scheme_port_record(port);

  if (!ip->count_lines || (ip->position < 0))
    return -1;
  
  CHECK_IOPORT_CLOSED("get-file-column", ip);

  col = ip->column;

  return col;
}


static void extract_next_location(const char *who, int argc, Scheme_Object **a, int delta,
                                  intptr_t *_line, intptr_t *_col, intptr_t *_pos)
{
  int i, j;
  intptr_t v;
  intptr_t line = -1, col = -1, pos = -1;

  for (j = 0; j < 3; j++) {
    v = -1;
    i = j + delta;
    if (SCHEME_TRUEP(a[i])) {
      if (scheme_nonneg_exact_p(a[i])) {
        if (SCHEME_INTP(a[i])) {
          v = SCHEME_INT_VAL(a[i]);
          if ((j != 1) && !v) {
            v = -1;
          }
        }
      }
      if (v == -1) {
        if (argc < 0)
          a[0] = a[i];
        scheme_wrong_contract(who, 
                              ((j == 1) ? "(or/c exact-nonnegative-integer? #f)" : "(or/c exact-positive-integer? #f)"),
                              ((argc > 0) ? i : -1), argc, a);
        return;
      }
    }

    switch (j) {
    case 0:
      line = v;
      break;
    case 1:
      col = v;
      break;
    case 2:
      pos = v;
      break;
    }
  }

  /* Internally, positions count from 0 instead of 1 */
  if (pos > -1)
    pos--;

  if (_line) *_line = line;
  if (_col) *_col = col;
  if (_pos) *_pos = pos;
}

void
scheme_tell_all (Scheme_Object *port, intptr_t *_line, intptr_t *_col, intptr_t *_pos)
{
  Scheme_Port *ip;
  
  ip = scheme_port_record(port);

  if (ip->count_lines && ip->location_fun) {
    Scheme_Location_Fun location_fun;
    Scheme_Object *r, *a[3];
    int got;
    
    location_fun = ip->location_fun;
    r = location_fun(ip);

    got = (SAME_OBJ(r, SCHEME_MULTIPLE_VALUES) ? scheme_multiple_count : 1);
    if (got != 3) {
      scheme_wrong_return_arity("user port next-location",
				3, got, 
				(got == 1) ? (Scheme_Object **)r : scheme_multiple_array,
				"calling port-next-location procedure");
      return;
    }

    a[0] = scheme_multiple_array[0];
    a[1] = scheme_multiple_array[1];
    a[2] = scheme_multiple_array[2];

    extract_next_location("user port next-location", -1, a, 0, _line, _col, _pos);
  } else {
    intptr_t line, col, pos;

    line = scheme_tell_line(port);
    col = scheme_tell_column(port);
    pos = scheme_tell(port);

    if (_line) *_line = line;
    if (_col) *_col = col;
    if (_pos) *_pos = pos;
  }
}

void scheme_set_port_location(int argc, Scheme_Object **argv)
{
  Scheme_Port *ip;
  intptr_t line, col, pos;
  
  extract_next_location("set-port-next-location!", argc, argv, 
                        1, &line, &col, &pos);

  
  ip = scheme_port_record(argv[0]);
  
  if (ip->count_lines) {
    ip->readpos = pos;
    ip->lineNumber = line;
    ip->column = col;
  }
}

void
scheme_count_lines (Scheme_Object *port)
{
  Scheme_Port *ip;

  ip = scheme_port_record(port);

  if (!ip->count_lines) {
    ip->count_lines = 1;
    if (ip->count_lines_fun) {
      Scheme_Count_Lines_Fun cl = ip->count_lines_fun;
      cl(ip);
    }
    
    if (scheme_is_input_port(port)) {
      Scheme_Input_Port *iip;
      iip = scheme_input_port_record(port);
      if (iip)
        iip->slow = 1;
    }
  }
}

void
scheme_close_input_port (Scheme_Object *port)
{
  Scheme_Input_Port *ip;

  ip = scheme_input_port_record(port);

  if (!ip->closed) {
    if (ip->close_fun) {
      Scheme_Close_Input_Fun f = ip->close_fun;
      f(ip);
    }

    if (ip->progress_evt) {
      scheme_post_sema_all(ip->progress_evt);
      ip->progress_evt = NULL;
    }

    if (ip->mref) {
      scheme_remove_managed(ip->mref, (Scheme_Object *)ip);
      ip->mref = NULL;
    }

    ip->closed = 1;
    ip->slow = 1;
    ip->ungotten_count = 0;
    ip->ungotten_special = NULL;
    if (ip->closed_evt)
      scheme_post_sema_all(SCHEME_PTR_VAL(ip->closed_evt));
  }
}

static void
force_close_input_port(Scheme_Object *port)
{
  scheme_force_port_closed = 1;
  scheme_close_input_port(port);
  scheme_force_port_closed = 0;
}

int scheme_close_should_force_port_closed()
{
  return scheme_force_port_closed;
}

/****************************** main output writer ******************************/

intptr_t
scheme_put_byte_string(const char *who, Scheme_Object *port,
		       const char *str, intptr_t d, intptr_t len,
		       int rarely_block)
{
  /* Unlike the main reader, the main writer is simple. It doesn't
     have to deal with peeks and specials, so it's a thin wrapper on
     the port's function. */

  Scheme_Output_Port *op;
  Scheme_Write_String_Fun ws;
  intptr_t out, llen, oout;
  int enable_break;

  op = scheme_output_port_record(port);

  CHECK_PORT_CLOSED(who, "output", port, op->closed);

  ws = op->write_string_fun;

  if (rarely_block == -1) {
    enable_break = 1;
    rarely_block = 1;
  } else
    enable_break = 0;

  if (enable_break) {
    if (scheme_current_thread->external_break) {
      scheme_thread_block_enable_break(0.0, 1);
      scheme_current_thread->ran_some = 1;
    }
  }

  if ((rarely_block == 1) && !len)
    /* By definition, a partial-progress write on a 0-length string is
       the same as a blocking flush */
    rarely_block = 0;

  llen = len;
  oout = 0;
  while (llen || !len) {
    out = ws(op, str, d, llen, rarely_block, enable_break);
    
    /* If out is 0, it might be because the port got closed: */
    if (!out) {
      CHECK_PORT_CLOSED(who, "output", port, op->closed);
    }
    
    if (out > 0) {
      op->p.position += out;
      oout += out;
      if (op->p.count_lines)
	do_count_lines((Scheme_Port *)op, str, d, out);
    }

    if (rarely_block || !len)
      break;

    llen -= out;
    d += out;
  }

  mzAssert(!rarely_block ? (oout == len) : 1);
  mzAssert((oout < 0) ? (rarely_block == 2) : 1);

  return oout;
}

void scheme_write_byte_string(const char *str, intptr_t len, Scheme_Object *port)
{
  (void)scheme_put_byte_string("write-string", port, str, 0, len, 0);
}

void scheme_write_char_string(const mzchar *str, intptr_t len, Scheme_Object *port)
{
  intptr_t blen;
  char *bstr, buf[64];

  bstr = scheme_utf8_encode_to_buffer_len(str, len, buf, 64, &blen);
  
  scheme_write_byte_string(bstr, blen, port);
}

intptr_t
scheme_put_char_string(const char *who, Scheme_Object *port,
		       const mzchar *str, intptr_t d, intptr_t len)
{
  intptr_t blen;
  char *bstr, buf[64];

  blen = scheme_utf8_encode(str, d, d + len, NULL, 0, 0);
  if (blen < 64)
    bstr = buf;
  else
    bstr = (char *)scheme_malloc_atomic(blen);
  scheme_utf8_encode(str, d, d + len, (unsigned char *)bstr, 0, 0);

  return scheme_put_byte_string(who, port, bstr, 0, blen, 0);
}

intptr_t
scheme_output_tell(Scheme_Object *port)
{
  return scheme_tell(port);
}

void
scheme_close_output_port(Scheme_Object *port)
{
  Scheme_Output_Port *op;

  op = scheme_output_port_record(port);

  if (!op->closed) {
    /* call close function first; it might raise an exception */
    if (op->close_fun) {
      Scheme_Close_Output_Fun f = op->close_fun;
      f(op);
    }

    /* NOTE: Allow the possibility that some other thread finishes the
       close while f blocks. */

    if (op->mref) {
      scheme_remove_managed(op->mref, (Scheme_Object *)op);
      op->mref = NULL;
    }
    
    op->closed = 1;
    if (op->closed_evt)
      scheme_post_sema_all(SCHEME_PTR_VAL(op->closed_evt));
  }
}

static void
force_close_output_port(Scheme_Object *port)
{
  scheme_force_port_closed = 1;
  scheme_close_output_port(port);
  scheme_force_port_closed = 0;
}

/*========================================================================*/
/*                           File port utils                              */
/*========================================================================*/

void scheme_flush_orig_outputs(void)
{
  /* Flush original output ports: */
  if (flush_out)
    scheme_flush_output(scheme_orig_stdout_port);
  if (flush_err)
    scheme_flush_output(scheme_orig_stderr_port);
}

void scheme_flush_output(Scheme_Object *o)
{
  scheme_put_byte_string("flush-output", o,
			 NULL, 0, 0,
			 0);
}

Scheme_Object *
scheme_file_stream_port_p (int argc, Scheme_Object *argv[])
{
  Scheme_Object *p = argv[0];

  if (SCHEME_INPUT_PORTP(p)) {
    Scheme_Input_Port *ip;

    ip = scheme_input_port_record(p);

    if (SAME_OBJ(ip->sub_type, file_input_port_type))
      return scheme_true;
#ifdef MZ_FDS
    else if (SAME_OBJ(ip->sub_type, fd_input_port_type))
      return scheme_true;
#endif
  } else if (SCHEME_OUTPUT_PORTP(p)) {
    Scheme_Output_Port *op;

    op = scheme_output_port_record(p);

    if (SAME_OBJ(op->sub_type, file_output_port_type))
      return scheme_true;
#ifdef MZ_FDS
    else if (SAME_OBJ(op->sub_type, fd_output_port_type))
      return scheme_true;
#endif
  } else {
    scheme_wrong_contract("file-stream-port?", "port?", 0, argc, argv);
  }

  return scheme_false;
}

int scheme_get_port_file_descriptor(Scheme_Object *p, intptr_t *_fd)
{
  intptr_t fd = 0;
  int fd_ok = 0;

  if (SCHEME_INPUT_PORTP(p)) {
    Scheme_Input_Port *ip;

    ip = scheme_input_port_record(p);

    if (!ip->closed) {
      if (SAME_OBJ(ip->sub_type, file_input_port_type)) {
	fd = MSC_IZE(fileno)((FILE *)((Scheme_Input_File *)ip->port_data)->f);
	fd_ok = 1;
      }
#ifdef MZ_FDS
      else if (SAME_OBJ(ip->sub_type, fd_input_port_type)) {
	fd = ((Scheme_FD *)ip->port_data)->fd;
	fd_ok = 1;
      }
#endif
    }
  } else if (SCHEME_OUTPUT_PORTP(p)) {
    Scheme_Output_Port *op;

    op = scheme_output_port_record(p);

    if (!op->closed) {
      if (SAME_OBJ(op->sub_type, file_output_port_type))  {
	fd = MSC_IZE (fileno)((FILE *)((Scheme_Output_File *)op->port_data)->f);
	fd_ok = 1;
      }
#ifdef MZ_FDS
      else if (SAME_OBJ(op->sub_type, fd_output_port_type))  {
	fd = ((Scheme_FD *)op->port_data)->fd;
	fd_ok = 1;
      }
#endif
    }
  }

  if (!fd_ok)
    return 0;

  *_fd = fd;
  return 1;
}

intptr_t scheme_get_port_fd(Scheme_Object *p)
{
  intptr_t fd;

  if (scheme_get_port_file_descriptor(p, &fd))
    return fd;
  else
    return -1;
}

Scheme_Object *scheme_file_identity(int argc, Scheme_Object *argv[])
{
  intptr_t fd = 0;
  int fd_ok = 0;
  Scheme_Object *p;

  p = argv[0];

  fd_ok = scheme_get_port_file_descriptor(p, &fd);

  if (!fd_ok) {
    /* Maybe failed because it was closed... */
    if (SCHEME_INPUT_PORTP(p)) {
      Scheme_Input_Port *ip;

      ip = scheme_input_port_record(p);
      
      CHECK_PORT_CLOSED("port-file-identity", "input", p, ip->closed);
    } else if (SCHEME_OUTPUT_PORTP(p)) {
      Scheme_Output_Port *op;
      
      op = scheme_output_port_record(p);
      
      CHECK_PORT_CLOSED("port-file-identity", "output", p, op->closed);
    }

    /* Otherwise, it's just the wrong type: */
    scheme_wrong_contract("port-file-identity", "file-stream-port?", 0, argc, argv);
    return NULL;
  }

  return scheme_get_fd_identity(p, fd, NULL);
}

static int is_fd_terminal(int fd)
{
#if defined(WIN32_FD_HANDLES)
  if (GetFileType((HANDLE)fd) == FILE_TYPE_CHAR) {
    DWORD mode;
    if (GetConsoleMode((HANDLE)fd, &mode))
      return 1;
    else
      return 0;
  } else
    return 0;
#else
  return isatty(fd);
#endif
}

Scheme_Object *scheme_terminal_port_p(int argc, Scheme_Object *argv[])
{
  intptr_t fd = 0;
  int fd_ok = 0;
  Scheme_Object *p;

  p = argv[0];

  if (SCHEME_INPUT_PORTP(p)) {
    Scheme_Input_Port *ip;

    ip = scheme_input_port_record(p);

    if (ip->closed)
      return scheme_false;

    if (SAME_OBJ(ip->sub_type, file_input_port_type)) {
      fd = MSC_IZE(fileno)((FILE *)((Scheme_Input_File *)ip->port_data)->f);
      fd_ok = 1;
    }
#ifdef MZ_FDS
    else if (SAME_OBJ(ip->sub_type, fd_input_port_type)) {
      fd = ((Scheme_FD *)ip->port_data)->fd;
      fd_ok = 1;
    }
#endif
  } else if (SCHEME_OUTPUT_PORTP(p)) {
    Scheme_Output_Port *op;

    op = scheme_output_port_record(p);

    if (op->closed)
      return scheme_false;

    if (SAME_OBJ(op->sub_type, file_output_port_type))  {
      fd = MSC_IZE (fileno)((FILE *)((Scheme_Output_File *)op->port_data)->f);
      fd_ok = 1;
    }
#ifdef MZ_FDS
    else if (SAME_OBJ(op->sub_type, fd_output_port_type))  {
      fd = ((Scheme_FD *)op->port_data)->fd;
      fd_ok = 1;
    }
#endif
  }

  if (!fd_ok)
    return scheme_false;

  return is_fd_terminal(fd) ? scheme_true : scheme_false;
}

static void filename_exn(char *name, char *msg, char *filename, int err)
{
  char *dir, *drive;
  int len;
  char *pre, *rel, *post;

  len = strlen(filename);

  if (scheme_is_relative_path(filename, len, SCHEME_PLATFORM_PATH_KIND)) {
    dir = scheme_os_getcwd(NULL, 0, NULL, 1);
    drive = NULL;
  } else if (scheme_is_complete_path(filename, len, SCHEME_PLATFORM_PATH_KIND)) {
    dir = NULL;
    drive = NULL;
  } else {
    dir = NULL;
    drive = scheme_getdrive();
  }

  pre = dir ? "\n  in directory: " : (drive ? "\n  on drive: " : "");
  rel = dir ? dir : (drive ? drive : "");
  post = dir ? "" : "";

  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		   "%s: %s\n"
                   "  path: %q%s%q%s\n"
                   "  system error: " FILENAME_EXN_E,
		   name, msg, filename,
		   pre, rel, post,
		   err);
}

Scheme_Object *
scheme_do_open_input_file(char *name, int offset, int argc, Scheme_Object *argv[], 
                          int internal, char **err, int *eerrno)
{
#ifdef USE_FD_PORTS
  int fd;
  struct stat buf;
#else
  char *mode = "rb";
# ifdef WINDOWS_FILE_HANDLES
  HANDLE fd;
# else
  FILE *fp;
# endif
#endif
  char *filename;
  int regfile, i;
  int m_set = 0;
  Scheme_Object *result;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract(name, "path-string?", 0, argc, argv);

  for (i = 1 + offset; argc > i; i++) {
    if (!SCHEME_SYMBOLP(argv[i]))
      scheme_wrong_contract(name, "symbol?", i, argc, argv);

    if (SAME_OBJ(argv[i], text_symbol)) {
#ifndef USE_FD_PORTS
      mode = "rt";
#endif
      m_set++;
    } else if (SAME_OBJ(argv[i], binary_symbol)) {
      /* This is the default */
      m_set++;
    } else {
      char *astr;
      intptr_t alen;

      astr = scheme_make_args_string("other ", i, argc, argv, &alen);
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "%s: bad mode symbol\n"
                       "  given symbol: %s%t", name,
		       scheme_make_provided_string(argv[i], 1, NULL),
		       astr, alen);
    }

    if (m_set > 1) {
      char *astr;
      intptr_t alen;

      astr = scheme_make_args_string("", -1, argc, argv, &alen);
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "%s: conflicting or redundant file modes given%t", 
                       name,
		       astr, alen);
    }
  }

  filename = scheme_expand_string_filename(argv[0],
					   name,
					   NULL,
					   (internal ? 0 : SCHEME_GUARD_FILE_READ));

  if (!internal)
    scheme_custodian_check_available(NULL, name, "file-stream");

#ifdef USE_FD_PORTS
  /* Note: assuming there's no difference between text and binary mode */
  do {
    fd = open(filename, O_RDONLY | MZ_NONBLOCKING | MZ_BINARY);
  } while ((fd == -1) && (errno == EINTR));

  if (fd == -1) {
    if (err) {
      *err = "cannot open source file";
      *eerrno = errno;
    } else
      filename_exn(name, "cannot open input file", filename, errno);
    return NULL;
  } else {
    int ok;

    do {
      ok = fstat(fd, &buf);
    } while ((ok == -1) && (errno == EINTR));

    if (S_ISDIR(buf.st_mode)) {
      int cr;
      do {
	cr = close(fd);
      } while ((cr == -1) && (errno == EINTR));
      if (err) {
        *err = "source is a directory";
        *eerrno = 0;
      } else
        filename_exn(name, "cannot open directory as a file", filename, 0);
      return NULL;
    } else {
      regfile = S_ISREG(buf.st_mode);
      result = make_fd_input_port(fd, scheme_make_path(filename), regfile, 0, NULL, internal);
    }
  }
#else
# ifdef WINDOWS_FILE_HANDLES
  fd = CreateFileW(WIDE_PATH(filename),
		   GENERIC_READ,
		   FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
		   NULL,
		   OPEN_EXISTING,
		   0,
		   NULL);

  if (fd == INVALID_HANDLE_VALUE) {
    if (err) {
      int errv;
      errv = GetLastError();
      *err = "cannot open source file";
      *eerrno = errv;
    } else
      filename_exn(name, "cannot open input file", filename, GetLastError());
    return NULL;
  } else
    regfile = (GetFileType(fd) == FILE_TYPE_DISK);

  if ((mode[1] == 't') && !regfile) {
    CloseHandle(fd);
    filename_exn(name, "cannot use text-mode on a non-file device", filename, 0);
    return NULL;
  }

  result = make_fd_input_port((int)fd, scheme_make_path(filename), regfile, mode[1] == 't', NULL, internal);
# else
  if (scheme_directory_exists(filename)) {
    if (err) {
      *err = "source is a directory";
      *eerrno = 0;
    } else
      filename_exn(name, err, filename, 0);
    return NULL;
  }

  regfile = scheme_is_regular_file(filename);

  fp = fopen(filename, mode);
  if (!fp) {
    if (err) {
      *err = "cannot open source file";
      *eerrno = errno;
    } else
      filename_exn(name, "cannot open input file", filename, errno);
    return NULL;
  }

  result = scheme_make_named_file_input_port(fp, scheme_make_path(filename));
# endif
#endif

  return result;
}

Scheme_Object *
scheme_do_open_output_file(char *name, int offset, int argc, Scheme_Object *argv[], int and_read, 
                           int internal, char **err, int *eerrno)
{
#ifdef USE_FD_PORTS
  int fd;
  int flags, regfile;
  struct stat buf;
  int ok;
#else
# ifdef WINDOWS_FILE_HANDLES
  HANDLE fd;
  int hmode, regfile;
  BY_HANDLE_FILE_INFORMATION info;
# else
  FILE *fp;
# endif
#endif
  int e_set = 0, m_set = 0, i;
  int existsok = 0, must_exist = 0;
  char *filename;
  char mode[4];
  int typepos;

  mode[0] = 'w';
  mode[1] = 'b';
  mode[2] = 0;
  mode[3] = 0;
  typepos = 1;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract(name, "path-string?", 0, argc, argv);

  for (i = 1 + offset; argc > i; i++) {
    if (!SCHEME_SYMBOLP(argv[i]))
      scheme_wrong_contract(name, "symbol?", i, argc, argv);

    if (SAME_OBJ(argv[i], append_symbol)) {
      mode[0] = 'a';
      existsok = -1;
      e_set++;
    } else if (SAME_OBJ(argv[i], replace_symbol)) {
      existsok = 1;
      e_set++;
    } else if (SAME_OBJ(argv[i], truncate_symbol)) {
      existsok = -1;
      e_set++;
    } else if (SAME_OBJ(argv[i], must_truncate_symbol)) {
      existsok = -1;
      must_exist = 1;
      e_set++;
    } else if (SAME_OBJ(argv[i], truncate_replace_symbol)) {
      existsok = -2;
      e_set++;
    } else if (SAME_OBJ(argv[i], update_symbol)) {
      existsok = 2;
      must_exist = 1;
      if (typepos == 1) {
	mode[2] = mode[1];
	typepos = 2;
      }
      mode[0] = 'r';
      mode[1] = '+';
      e_set++;
    } else if (SAME_OBJ(argv[i], can_update_symbol)) {
      existsok = 3;
      if (typepos == 1) {
	mode[2] = mode[1];
	typepos = 2;
      }
      mode[0] = 'r';
      mode[1] = '+';
      e_set++;
    } else if (SAME_OBJ(argv[i], error_symbol)) {
      /* This is the default */
      e_set++;
    } else if (SAME_OBJ(argv[i], text_symbol)) {
      mode[typepos] = 't';
      m_set++;
    } else if (SAME_OBJ(argv[i], binary_symbol)) {
      /* This is the default */
      m_set++;
    } else {
      char *astr;
      intptr_t alen;

      astr = scheme_make_args_string("other ", i, argc, argv, &alen);
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "%s: bad mode symbol\n"
                       "  given symbol: : %s%s", name,
		       scheme_make_provided_string(argv[i], 1, NULL),
		       astr, alen);
    }

    if (m_set > 1 || e_set > 1) {
      char *astr;
      intptr_t alen;

      astr = scheme_make_args_string("", -1, argc, argv, &alen);
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "%s: conflicting or redundant file modes given%t", 
                       name,
		       astr, alen);
    }
  }

  filename = scheme_expand_string_filename(argv[0],
					   name, NULL,
                                           (internal
                                            ? 0
                                            : (SCHEME_GUARD_FILE_WRITE
                                               | ((existsok && ((existsok == 1) || (existsok == -2)))
                                                  ? SCHEME_GUARD_FILE_DELETE
                                                  : 0)
                                               /* append mode: */
                                               | ((mode[0] == 'a')
                                                  ? SCHEME_GUARD_FILE_READ
                                                  : 0)
                                               /* update mode: */
                                               | ((existsok > 1)
                                                  ? SCHEME_GUARD_FILE_READ
                                                  : 0))));

  scheme_custodian_check_available(NULL, name, "file-stream");

#ifdef USE_FD_PORTS
  /* Note: assuming there's no difference between text and binary mode */

  flags = (and_read ? O_RDWR : O_WRONLY) | (must_exist ? 0 : O_CREAT);

  if (mode[0] == 'a')
    flags |= O_APPEND;
  else if (existsok < 0)
    flags |= O_TRUNC;

  if ((existsok <= 1) && (existsok > -1))
    flags |= O_EXCL;

  do {
    fd = open(filename, flags | MZ_NONBLOCKING | MZ_BINARY, 0666);
  } while ((fd == -1) && (errno == EINTR));

  if (errno == ENXIO) {
    /* FIFO with no reader? Try opening in RW mode: */
    flags -= O_WRONLY;
    flags |= O_RDWR;
    do {
      fd = open(filename, flags | MZ_NONBLOCKING | MZ_BINARY, 0666);
    } while ((fd == -1) && (errno == EINTR));
  }

  if (fd == -1) {
    if (errno == EISDIR) {
      if (err) {
        *err = "destination is a directory path";
        *eerrno = errno;
        return NULL;
      } else
        scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_EXISTS,
                         "%s: path is a directory\n"
                         "  path: %q",
                         name, filename);
    } else if (errno == EEXIST) {
      if (!existsok) {
        if (err) {
          *err = "destination already exists";
          *eerrno = errno;
          return NULL;
        } else
          scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_EXISTS,
                           "%s: file exists\n"
                           "  path: %q", name, filename);
      } else {
	do {
	  ok = unlink(filename);
	} while ((ok == -1) && (errno == EINTR));

	if (ok)
	  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
			   "%s: error deleting file\n"
                           "  path: %q",
			   name, filename);
	do {
	  fd = open(filename, flags | MZ_BINARY, 0666);
	} while ((fd == -1) && (errno == EINTR));
      }
    }

    if (fd == -1) {
      if (err) {
        *err = "cannot open destination file";
        *eerrno = errno;
      } else
        filename_exn(name, "cannot open output file", filename, errno);
      return NULL;
    }
  }

  do {
    ok = fstat(fd, &buf);
  } while ((ok == -1) && (errno == EINTR));

  regfile = S_ISREG(buf.st_mode);
  return make_fd_output_port(fd, scheme_make_path(filename), regfile, 0, and_read, 
			     -1, NULL);
#else
# ifdef WINDOWS_FILE_HANDLES
  if (!existsok)
    hmode = CREATE_NEW;
  else if (existsok < 0) {
    if (must_exist)
      hmode = TRUNCATE_EXISTING;
    else
      hmode = OPEN_ALWAYS;
  } else if (existsok  == 1) {
    /* assert: !must_exist */
    hmode = CREATE_ALWAYS;
  } else if (existsok == 2) {
    hmode = OPEN_EXISTING;
  } else if (existsok == 3) {
    hmode = OPEN_ALWAYS;
  }

  fd = CreateFileW(WIDE_PATH(filename),
		   GENERIC_WRITE | (and_read ? GENERIC_READ : 0),
		   FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
		   NULL,
		   hmode,
		   FILE_FLAG_BACKUP_SEMANTICS, /* lets us detect directories in NT */
		   NULL);

  if (fd == INVALID_HANDLE_VALUE) {
    int errv;
    errv = GetLastError();
    if ((errv == ERROR_ACCESS_DENIED) && (existsok < -1)) {
      /* Delete and try again... */
      if (DeleteFileW(WIDE_PATH(filename))) {
	fd = CreateFileW(WIDE_PATH(filename),
                         GENERIC_WRITE,
                         FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                         NULL,
                         hmode,
                         0,
                         NULL);
	if (fd == INVALID_HANDLE_VALUE)
	  errv = GetLastError();
      } else {
	scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
			 "%s: error deleting file\n"
                         "  path: %q\n"
                         "  system error: %E",
			 name, filename, GetLastError());
	return NULL;
      }
    } else if (errv == ERROR_FILE_EXISTS) {
      if (err) {
        *err = "destination already exists";
        *eerrno = errv;
      } else
        scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_EXISTS,
                         "%s: file exists\n"
                         "  path: %q", name, filename);
      return NULL;
    }

    if (fd == INVALID_HANDLE_VALUE) {
      if (err) {
        *err = "cannot open destination";
        *eerrno = errv;
      } else
        filename_exn(name, "cannot open output file", filename, errv);
      return NULL;
    }
  }

  if (GetFileInformationByHandle(fd, &info)) {
    if (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
      CloseHandle(fd);
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_EXISTS,
		       "%s: path is a directory\n"
                       "  path: %q",
		       name, filename);
      return NULL;
    }
  }

  regfile = (GetFileType(fd) == FILE_TYPE_DISK);

  if ((mode[1] == 't') && !regfile) {
    CloseHandle(fd);
    filename_exn(name, "cannot use text-mode on a non-file device", filename, 0);
    return NULL;
  }

  if (regfile && (existsok < 0)) {
    if (mode[0] == 'a')
      SetFilePointer(fd, 0, NULL, FILE_END);
    else
      SetEndOfFile(fd);
  }

  return make_fd_output_port((int)fd, scheme_make_path(filename), regfile, mode[1] == 't', and_read, 
			     -1, NULL);
# else
  if (scheme_directory_exists(filename)) {
    if (err) {
      *err = "destination is a directory path";
      *eerrno = 0;
    } else if (!existsok)
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_EXISTS,
		       "%s: path is a directory\n"
                       "  path: %q",
		       name, filename);
    else
      filename_exn(name, "cannot open directory as a file", filename, errno);
    return NULL;
  }


  if (and_read) {
    scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		     "%s: " NOT_SUPPORTED_STR,
		     name);
    return NULL;
  }

  if (scheme_file_exists(filename)) {
    int uok;

    if (!existsok) {
      if (err) {
        *err = "destination exists already";
        *eerrno = 0;
      } else
        scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_EXISTS,
                         "%s: file exists\n"
                         "  path: %q", name, filename);
      return NULL;
    }

    do {
      uok = MSC_IZE(unlink)(filename);
    } while ((uok == -1) && (errno == EINTR));

    if (uok)
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_EXISTS,
		       "%s: error deleting file\n"
                       "  path: %q\n"
                       "  system error: %e",
		       name, filename, errno);
  }

  fp = fopen(filename, mode);
  if (!fp) {
    if (existsok < -1) {
      /* Can't truncate; try to replace */
      if (scheme_file_exists(filename)) {
	int uok;

	do {
	  uok = MSC_IZE(unlink)(filename);
	} while ((uok == -1) && (errno == EINTR));

	if (uok)
	  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_EXISTS,
			   "%s: error deleting file\n"
                           "  path: %q",
			   name, filename);
	else {
	  fp = fopen(filename, mode);
	}
      }
    }
    if (!fp) {
      if (err) {
        *err = "cannot open destination";
        *eerrno = errno;
      } else
        filename_exn(name, "cannot open output file", filename, errno);
      return NULL;
    }
  }

  return scheme_make_file_output_port(fp);
# endif
#endif
}

Scheme_Object *scheme_open_input_file(const char *name, const char *who)
{
  Scheme_Object *a[1];

  a[0]= scheme_make_path(name);
  return scheme_do_open_input_file((char *)who, 0, 1, a, 0, NULL, NULL);
}

Scheme_Object *scheme_open_output_file(const char *name, const char *who)
{
  Scheme_Object *a[2];

  a[0]= scheme_make_path(name);
  a[1] = truncate_replace_symbol;
  return scheme_do_open_output_file((char *)who, 0, 2, a, 0, 0, NULL, NULL);
}

Scheme_Object *scheme_open_input_output_file(const char *name, const char *who, Scheme_Object **oport)
{
  Scheme_Object *a[2];

  a[0]= scheme_make_path(name);
  a[1] = truncate_replace_symbol;
  scheme_do_open_output_file((char *)who, 0, 2, a, 1, 0, NULL, NULL);
  *oport = scheme_multiple_array[1];
  return scheme_multiple_array[0];
}

Scheme_Object *scheme_open_output_file_with_mode(const char *name, const char *who, int text)
{
  Scheme_Object *a[3];

  a[0]= scheme_make_path(name);
  a[1] = truncate_replace_symbol;
  a[2] = (text ? text_symbol : binary_symbol);
  return scheme_do_open_output_file((char *)who, 0, 3, a, 0, 0, NULL, NULL);
}

#ifdef WINDOWS_FILE_HANDLES
static int win_seekable(int fd)
{
  /* SetFilePointer() requires " a file stored on a seeking device".
     I'm not sure how to test that, so we approximate as "regular
     file". */
  return GetFileType((HANDLE)fd) == FILE_TYPE_DISK;
}
#endif

Scheme_Object *
scheme_file_position(int argc, Scheme_Object *argv[])
{
  FILE *f;
  Scheme_Indexed_String *is;
  intptr_t fd;
#ifdef MZ_FDS
  int had_fd;
#endif
  int wis;

  if (!SCHEME_OUTPUT_PORTP(argv[0]) && !SCHEME_INPUT_PORTP(argv[0]))
    scheme_wrong_contract("file-position", "port?", 0, argc, argv);
  if (argc == 2) {
    if (!SCHEME_EOFP(argv[1])) {
      int ok = 0;

      if (SCHEME_INTP(argv[1])) {
	ok = (SCHEME_INT_VAL(argv[1]) >= 0);
      }
      
      if (SCHEME_BIGNUMP(argv[1])) {
	ok = SCHEME_BIGPOS(argv[1]);
      }
      
      if (!ok)
	scheme_wrong_contract("file-position", "(or/c exact-nonnegative-integer? eof-object?)", 1, argc, argv);
    }
  }

  f = NULL;
  is = NULL;
  wis = 0;
  fd = 0;
#ifdef MZ_FDS
  had_fd = 0;
#endif

  if (!SCHEME_INPUT_PORTP(argv[0])) {
    Scheme_Output_Port *op;

    op = scheme_output_port_record(argv[0]);

    if (SAME_OBJ(op->sub_type, file_output_port_type)) {
      f = ((Scheme_Output_File *)op->port_data)->f;
#ifdef MZ_FDS
    } else if (SAME_OBJ(op->sub_type, fd_output_port_type)) {
      fd = ((Scheme_FD *)op->port_data)->fd;
      had_fd = 1;
#endif
    } else if (SAME_OBJ(op->sub_type, scheme_string_output_port_type)) {
      is = (Scheme_Indexed_String *)op->port_data;
      wis = 1;
    } else if (argc < 2)
      return scheme_make_integer(scheme_output_tell(argv[0]));
  } else {
    Scheme_Input_Port *ip;

    ip = scheme_input_port_record(argv[0]);

    if (SAME_OBJ(ip->sub_type, file_input_port_type)) {
      f = ((Scheme_Input_File *)ip->port_data)->f;
#ifdef MZ_FDS
    } else if (SAME_OBJ(ip->sub_type, fd_input_port_type)) {
      fd = ((Scheme_FD *)ip->port_data)->fd;
      had_fd = 1;
#endif
    } else if (SAME_OBJ(ip->sub_type, scheme_string_input_port_type))
      is = (Scheme_Indexed_String *)ip->port_data;
    else if (argc < 2) {
      intptr_t pos;
      pos = ip->p.position;
      if (pos < 0) {
	scheme_raise_exn(MZEXN_FAIL,
			 "the port's current position is not known\n"
                         "  port: %v",
			 ip);
      }
      return scheme_make_integer_value(pos);
    }
  }

  if (!f
#ifdef MZ_FDS
      && !had_fd
#endif
      && !is)
    scheme_contract_error("file-position",
                          "setting position allowed for file-stream and string ports only",
                          "port", 1, argv[0],
                          "position", 1, argv[1],
                          NULL);

  if (argc > 1) {
    mzlonglong nll;
    int whence;

    if (SCHEME_EOFP(argv[1])) {
      nll = 0;
      whence = SEEK_END;
    } else if (scheme_get_long_long_val(argv[1], &nll)) {
      whence = SEEK_SET;
      if ((mzlonglong)(mz_off_t)nll != nll) {
	nll = -1;
      }
    } else {
      whence = SEEK_SET; /* not used */
      nll = -1;
    }

    if (nll < 0) {
      scheme_contract_error("file-position",
                            "new position is too large",
                            "port", 1, argv[0],
                            "position", 1, argv[1],
                            NULL);
      return NULL;
    }
      
    if (f) {
      if (BIG_OFF_T_IZE(fseeko)(f, nll, whence)) {
	scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
			 "file-position: position change failed on file\n"
                         "  system error: %e",
			 errno);
      }
#ifdef MZ_FDS
    } else if (had_fd) {
      intptr_t lv;
      int errid = 0;
      
      if (!SCHEME_INPUT_PORTP(argv[0])) {
	flush_fd(scheme_output_port_record(argv[0]), NULL, 0, 0, 0, 0);
      }
      
# ifdef WINDOWS_FILE_HANDLES
      if (win_seekable(fd)) {
	DWORD r;
	LONG lo_w, hi_w;
	lo_w = (LONG)(nll & 0xFFFFFFFF);
	hi_w = (LONG)(nll >> 32);
        r = SetFilePointer((HANDLE)fd, lo_w, &hi_w,
			   ((whence == SEEK_SET) ? FILE_BEGIN : FILE_END));
	if ((r == INVALID_SET_FILE_POINTER)
	    && GetLastError() != NO_ERROR) {
	  errid = GetLastError();
          lv = -1;
	} else
	  lv = 0;
      } else {
	lv = -1;
	errid = ERROR_UNSUPPORTED_TYPE;
      }
# else
      lv = BIG_OFF_T_IZE(lseek)(fd, nll, whence);
      if (lv < 0) errid = errno;
# endif

      if (lv < 0) {
	scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
			 "file-position: position change failed on stream\n"
                         "  system error: " FILENAME_EXN_E,
			 errid);
      }

      if (SCHEME_INPUT_PORTP(argv[0])) {
	/* Get rid of buffered data: */
	Scheme_FD *sfd;
        Scheme_Input_Port *ip;
        ip = scheme_input_port_record(argv[0]);
	sfd = (Scheme_FD *)ip->port_data;
	sfd->bufcount = 0;
	sfd->buffpos = 0;
	/* 1 means no pending eof, but can set: */
	ip->pending_eof = 1;
      }
#endif
    } else {
      intptr_t n;

      if (whence == SEEK_SET) {
        if (!scheme_get_int_val(argv[1], &n)) {
          scheme_raise_out_of_memory(NULL, NULL);
        }
      } else {
        n = 0;
      }

      if (whence == SEEK_END) {
        if (wis)
          n = is->u.hot;
        else
          n = is->size;
      }
      if (wis) {
	if (is->index > is->u.hot)
	  is->u.hot = is->index;
	if (is->size < n) {
	  /* Expand string up to n: */
	  char *old;

	  old = is->string;
	  {
	    char *ca;
	    ca = (char *)scheme_malloc_fail_ok(scheme_malloc_atomic, n + 1);
	    is->string = ca;
          }
	  is->size = n;
	  memcpy(is->string, old, is->u.hot);
	}
	if (n > is->u.hot) {
	  memset(is->string + is->u.hot, 0, n - is->u.hot);
          is->u.hot = n;
        }
      } else {
	/* Can't really move past end of read string, but pretend we do: */
	if (n > is->size) {
	  is->u.pos = n;
	  n = is->size;
	} else
	  is->u.pos = 0;
      }
      is->index = n;
    }

    /* Remove any chars saved from peeks: */
    if (SCHEME_INPUT_PORTP(argv[0])) {
      Scheme_Input_Port *ip;
      ip = scheme_input_port_record(argv[0]);
      ip->ungotten_count = 0;
      if (pipe_char_count(ip->peeked_read)) {
	ip->peeked_read = NULL;
	ip->peeked_write = NULL;
      }
    }

    return scheme_void;
  } else {
    mzlonglong pll;
    if (f) {
      pll = BIG_OFF_T_IZE(ftello)(f);
#ifdef MZ_FDS
    } else if (had_fd) {
# ifdef WINDOWS_FILE_HANDLES
      if (win_seekable(fd)) {
	DWORD lo_w, hi_w;
	hi_w = 0;
        lo_w = SetFilePointer((HANDLE)fd, 0, &hi_w, FILE_CURRENT);
	if ((lo_w == INVALID_SET_FILE_POINTER)
	    && GetLastError() != NO_ERROR)
          pll = -1;
        else
          pll = ((mzlonglong)hi_w << 32) | lo_w;
      } else
	pll = -1;
# else
      pll = BIG_OFF_T_IZE(lseek)(fd, 0, 1);
# endif
      if (pll < 0) {
	if (SCHEME_INPUT_PORTP(argv[0])) {
	  pll = scheme_tell(argv[0]);
	} else {
	  pll = scheme_output_tell(argv[0]);
	}
      } else {
	if (SCHEME_INPUT_PORTP(argv[0])) {          
          Scheme_Input_Port *ip;
          ip = scheme_input_port_record(argv[0]);
	  pll -= ((Scheme_FD *)ip->port_data)->bufcount;
	} else {
          Scheme_Output_Port *op;
          op = scheme_output_port_record(argv[0]);
	  pll += ((Scheme_FD *)op->port_data)->bufcount;
	}
      }
#endif
    } else if (wis)
      pll = is->index;
    else {
      /* u.pos > index implies we previously moved past the end with file-position */
      if (is->u.pos > is->index)
	pll = is->u.pos;
      else
	pll = is->index;
    }

    /* Back up for un-gotten & peeked chars: */
    if (SCHEME_INPUT_PORTP(argv[0])) {
      Scheme_Input_Port *ip;
      ip = scheme_input_port_record(argv[0]);
      pll -= ip->ungotten_count;
      pll -= pipe_char_count(ip->peeked_read);
    }

    return scheme_make_integer_value_from_long_long(pll);
  }
}

intptr_t scheme_set_file_position(Scheme_Object *port, intptr_t pos)
{
  if (pos >= 0) {
    Scheme_Object *a[2];

    a[0] = port;
    a[1] = scheme_make_integer(pos);
    (void)scheme_file_position(2, a);
    return 0;
  } else {
    Scheme_Object *n;
    n = scheme_file_position(1, &port);
    return SCHEME_INT_VAL(n);
  }
}

Scheme_Object *
scheme_file_buffer(int argc, Scheme_Object *argv[])
{
  Scheme_Port *p = NULL;

  if (!SCHEME_OUTPUT_PORTP(argv[0]) && !SCHEME_INPUT_PORTP(argv[0]))
    scheme_wrong_contract("file-stream-buffer-mode", "port?", 0, argc, argv);

  p = scheme_port_record(argv[0]);

  if (argc == 1) {
    Scheme_Buffer_Mode_Fun bm;

    bm = p->buffer_mode_fun;
    if (bm) {
      switch (bm(p, -1)) {
      case MZ_FLUSH_NEVER:
	return scheme_block_symbol;
      case MZ_FLUSH_BY_LINE:
	return scheme_line_symbol;
      case MZ_FLUSH_ALWAYS:
	return scheme_none_symbol;
      }
    }

    return scheme_false;
  } else {
    Scheme_Object *s = argv[1];
    Scheme_Buffer_Mode_Fun bm;

    if (!SAME_OBJ(s, scheme_block_symbol)
	&& !SAME_OBJ(s, scheme_line_symbol)
	&& !SAME_OBJ(s, scheme_none_symbol))
      scheme_wrong_contract("file-stream-buffer-mode", "(or/c 'none 'line 'block)", 1, argc, argv);

    if (SCHEME_INPUT_PORTP(argv[0]) && SAME_OBJ(s, scheme_line_symbol))
      scheme_contract_error("file-stream-buffer-mode", 
                            "'line buffering not supported for an input port",
                            "port", 1, argv[0],
                            NULL);

    bm = p->buffer_mode_fun;
    if (bm) {
      int mode;
      if (SAME_OBJ(s, scheme_block_symbol))
	mode = MZ_FLUSH_NEVER;
      else if (SAME_OBJ(s, scheme_line_symbol))
	mode = MZ_FLUSH_BY_LINE;
      else
	mode = MZ_FLUSH_ALWAYS;

      bm(p, mode);
    } else {
      scheme_contract_error("file-stream-buffer-mode", 
                            "cannot set buffer mode on port",
                            "port", 1, argv[0],
                            NULL);
    }

    return scheme_void;
  }
}

static int try_lock(int fd, int writer, int *_errid)
{
#ifdef UNIX_FILE_SYSTEM
# ifdef USE_FLOCK_FOR_FILE_LOCKS
  {
    int ok;

    do {
      ok = flock(fd, (writer ? LOCK_EX : LOCK_SH) | LOCK_NB);
    } while ((ok == -1) && (errno == EINTR));

    if (ok == 0)
      return 1;

    if (errno == EWOULDBLOCK) {
      *_errid = 0;
      return 0;
    }
    
    *_errid = errno;
    return 0;
  }
# elif defined(USE_FCNTL_AND_FORK_FOR_FILE_LOCKS)
  /* An lockf() is cancelled if *any* file descriptor to the same file
     is closed within the same process. We avoid that problem by forking
     a new process whose only job is to use lockf(). */
  {
    int ifds[2], ofds[2], cr;

    if (locked_fd_process_map)
      if (scheme_hash_get(locked_fd_process_map, scheme_make_integer(fd)))
        /* already have a lock */
        return 1;

    if (!pipe(ifds)) {
      if (!pipe(ofds)) {
        int pid;

        pid = fork();
      
        if (pid > 0) {
          /* Original process: */
          int errid = 0;
        
          do {
            cr = close(ifds[1]);
          } while ((cr == -1) && (errno == EINTR));
          do {
            cr = close(ofds[0]);
          } while ((cr == -1) && (errno == EINTR));

          do{
            cr = read(ifds[0], &errid, sizeof(int));
          } while ((cr == -1) && (errno == EINTR));
          if (cr == -1)
            errid = errno;

          do {
            cr = close(ifds[0]);
          } while ((cr == -1) && (errno == EINTR));

          if (errid) {
            do {
              cr = close(ofds[1]);
            } while ((cr == -1) && (errno == EINTR));
            
            if (errid == EAGAIN)
              *_errid = 0;
            else
              *_errid = errid;

            return 0;
          } else {
            /* got lock; record fd -> pipe mapping */
            if (!locked_fd_process_map) {
              REGISTER_SO(locked_fd_process_map);
              locked_fd_process_map = scheme_make_hash_table(SCHEME_hash_ptr);
            }
            scheme_hash_set(locked_fd_process_map, 
                            scheme_make_integer(fd), 
                            scheme_make_pair(scheme_make_integer(ofds[1]),
                                             scheme_make_integer(pid)));
            return 1;
          }
        } else if (!pid) {
          /* Child process */
          int ok = 0;
          struct flock fl;

          do {
            cr = close(ifds[0]);
          } while ((cr == -1) && (errno == EINTR));
          do {
            cr = close(ofds[1]);
          } while ((cr == -1) && (errno == EINTR));
#ifdef CLOSE_ALL_FDS_AFTER_FORK
          close_fds_after_fork(ifds[1], ofds[0], fd);
#endif
   
          fl.l_start = 0;
          fl.l_len = 0;
          fl.l_type = (writer ? F_WRLCK : F_RDLCK);
          fl.l_whence = SEEK_SET;
          fl.l_pid = getpid();

          if (!fcntl(fd, F_SETLK, &fl)) {
            /* report success: */
            do {
              cr = write(ifds[1], &ok, sizeof(int));
            } while ((cr == -1) && (errno == EINTR));
            /* wait until a signal to exit: */
            do {
              cr = read(ofds[0], &ok, sizeof(int));
            } while ((cr == -1) && (errno == EINTR));
          }

          if (!ok) {
            int errid = errno;
            do {
              cr = write(ifds[1], &errid, sizeof(int));
            } while ((cr == -1) && (errno == EINTR));
          }
          _exit(0);
        } else {
          int i;
          *_errid = errno;
          for (i = 0; i < 2; i++) {
            do {
              cr = close(ifds[i]);
            } while ((cr == -1) && (errno == EINTR));
            do {
              cr = close(ofds[i]);
            } while ((cr == -1) && (errno == EINTR));
          }
          return 0;
        }
      } else {
        int i;
        *_errid = errno;
        for (i = 0; i < 2; i++) {
          do {
            cr = close(ifds[i]);
          } while ((cr == -1) && (errno == EINTR));
        }
        return 0;
      }
    } else {
      *_errid = errno;
      return 0;
    }
  }
# else
  *_errid = ENOTSUP;
  return 0;
# endif
#endif
#ifdef WINDOWS_FILE_HANDLES
  {
    OVERLAPPED o;
    int errid;

# define LOCK_ALL_FILE_LO 0
# define LOCK_ALL_FILE_HI 0x10000

    memset(&o, 0, sizeof(OVERLAPPED));
    if (LockFileEx((HANDLE)fd, 
		   (LOCKFILE_FAIL_IMMEDIATELY
		    | (writer ? LOCKFILE_EXCLUSIVE_LOCK : 0)),
		   0, 
		   LOCK_ALL_FILE_LO, LOCK_ALL_FILE_HI,
		   &o))
      return 1;
   
    errid = GetLastError();
    if (errid == ERROR_LOCK_VIOLATION)
      *_errid = 0;
    else
      *_errid = errid;
    
    return 0;
  }
#endif
}

static void check_already_closed(const char *name, Scheme_Object *p)
{
  int is_closed;
  if (SCHEME_INPUT_PORTP(p)) {
    is_closed = scheme_input_port_record(p)->closed;
  } else {
    is_closed = scheme_output_port_record(p)->closed;
  }
  if (is_closed) {
    scheme_contract_error(name,
                          "port is closed",
                          "port", 1, p,
                          NULL);
  }
}

Scheme_Object *scheme_file_try_lock(int argc, Scheme_Object **argv)
{
  intptr_t fd;
  int writer = 0, errid;

  if (!scheme_get_port_file_descriptor(argv[0], &fd))
    scheme_wrong_contract("port-try-file-lock?", "file-stream-port?", 0, argc, argv);

  if (SCHEME_SYMBOLP(argv[1]) && !SCHEME_SYM_WEIRDP(argv[1])) {
    if (!strcmp(SCHEME_SYM_VAL(argv[1]), "exclusive"))
      writer = 1;
    else if (!strcmp(SCHEME_SYM_VAL(argv[1]), "shared"))
      writer = 0;
    else
      writer = -1;
  } else
    writer = -1;

  if (writer == -1)
    scheme_wrong_contract("port-try-file-lock?", "(or/c 'shared 'exclusive)", 1, argc, argv);

  if (writer && !SCHEME_OUTPORTP(argv[0]))
    scheme_contract_error("port-try-file-lock?",
                          "port for 'exclusive locking is not an output port",
                          "port", 1, argv[0],
                          NULL);
  else if (!writer && !SCHEME_INPORTP(argv[0]))
    scheme_contract_error("port-try-file-lock?",
                          "port for 'shared locking is not an input port",
                          "port", 1, argv[0],
                          NULL);

  check_already_closed("port-try-file-lock?", argv[0]);

  if (try_lock(fd, writer, &errid))
    return scheme_true;
  
  if (errid) {
    scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
                     "port-try-file-lock?: error getting file %s lock\n"
                     "  system error: %E",
                     (writer ? "exclusive" : "shared"),
                     errid);
  }
   
  return scheme_false;
}

#ifdef USE_FCNTL_AND_FORK_FOR_FILE_LOCKS
static void release_lockf(int fd)
{
  if (locked_fd_process_map) {
    Scheme_Object *v;
    v = scheme_hash_get(locked_fd_process_map, scheme_make_integer(fd));
    if (v) {
      int fd2, cr, pid, status;

      fd2 = SCHEME_INT_VAL(SCHEME_CAR(v));
      pid = SCHEME_INT_VAL(SCHEME_CDR(v));
      scheme_hash_set(locked_fd_process_map, scheme_make_integer(fd), NULL);

      scheme_block_child_signals(1);
      do {
	cr = close(fd2); /* makes the fork()ed process exit */
      } while ((cr == -1) && (errno == EINTR));
      waitpid(pid, &status, 0);
      scheme_block_child_signals(0);
    }
  }
}
#endif

Scheme_Object *scheme_file_unlock(int argc, Scheme_Object **argv)
{
  int ok, errid;
  intptr_t fd;

  if (!scheme_get_port_file_descriptor(argv[0], &fd))
    scheme_wrong_contract("port-file-unlock", "file-stream-port?", 0, argc, argv);

  check_already_closed("port-file-unlock", argv[0]);

#ifdef UNIX_FILE_SYSTEM
# ifdef USE_FLOCK_FOR_FILE_LOCKS
  do {
    ok = flock(fd, LOCK_UN);
  } while ((ok == -1) && (errno == EINTR));
  ok = !ok;
  errid = errno;
# elif defined(USE_FCNTL_AND_FORK_FOR_FILE_LOCKS)
  release_lockf(fd);
  ok = 1;
  errid = 0;
# else
  ok = 0;
  errid = ENOTSUP;
# endif
#endif
#ifdef WINDOWS_FILE_HANDLES
  ok = UnlockFile((HANDLE)fd, 0, 0, LOCK_ALL_FILE_LO, LOCK_ALL_FILE_HI);
  if (!ok)
    errid = GetLastError();
  else
    errid = 0;
#endif

  if (!ok) {
    scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
                     "port-file-unlock: error unlocking file\n"
                     "  system error: %E",
                     errid);
  }

  return scheme_void;
}

/*========================================================================*/
/*                          FILE input ports                              */
/*========================================================================*/

static int
file_byte_ready (Scheme_Input_Port *port)
{
  return 1;
}

static intptr_t file_get_string(Scheme_Input_Port *port,
			    char *buffer, intptr_t offset, intptr_t size,
			    int nonblock,
			    Scheme_Object *unless_evt)
{
  FILE *fp;
  Scheme_Input_File *fip;
  int c;

  fip = (Scheme_Input_File *)port->port_data;
  fp = fip->f;

  c = fread(buffer XFORM_OK_PLUS offset, 1, size, fp);

  if (c <= 0) {
    if (!feof(fp)) {
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		       "error reading from file port\n"
                       "  port: %V\n"
                       "  system error: %e",
		       port->name, errno);
      return 0;
    } else
      c = EOF;
#ifndef DONT_CLEAR_FILE_EOF
    clearerr(fp);
#endif
  }

  return c;
}

static void
file_close_input(Scheme_Input_Port *port)
{
  Scheme_Input_File *fip;

  fip = (Scheme_Input_File *)port->port_data;

  fclose(fip->f);
}

static void
file_need_wakeup(Scheme_Input_Port *port, void *fds)
{
}

static int
file_buffer_mode(Scheme_Port *p, int mode)
{
  FILE *f;
  int bad;

  if (mode < 0)
    return -1; /* unknown mode */

  if (SCHEME_INPORTP((Scheme_Object *)p)) {
    Scheme_Input_Port *ip = (Scheme_Input_Port *)p;
    f = ((Scheme_Output_File *)ip->port_data)->f;
  } else {
    Scheme_Output_Port *op = (Scheme_Output_Port *)p;
    f = ((Scheme_Output_File *)op->port_data)->f;
  }
  
  if (mode == MZ_FLUSH_NEVER)
    bad = setvbuf(f, NULL, _IOFBF, 0);
  else if (mode == MZ_FLUSH_BY_LINE)
    bad = setvbuf(f, NULL, _IOLBF, 0);
  else
    bad = setvbuf(f, NULL, _IONBF, 0);
  
  if (bad) {
    scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		     "file-stream-buffer-mode: error changing buffering\n"
                     "  system error: %e",
		     errno);
  }

  return mode;
}


static Scheme_Object *
_scheme_make_named_file_input_port(FILE *fp, Scheme_Object *name, int regfile)
{
  Scheme_Input_Port *ip;
  Scheme_Input_File *fip;

  if (!fp)
    scheme_signal_error("make-file-input-port(internal): "
			"null file pointer");

  fip = MALLOC_ONE_RT(Scheme_Input_File);
#ifdef MZTAG_REQUIRED
  fip->type = scheme_rt_input_file;
#endif

  fip->f = fp;

  ip = scheme_make_input_port(file_input_port_type,
			      fip,
			      name,
			      file_get_string,
			      NULL,
			      scheme_progress_evt_via_get,
			      scheme_peeked_read_via_get,
			      file_byte_ready,
			      file_close_input,
			      file_need_wakeup,
			      1);
  ip->p.buffer_mode_fun = file_buffer_mode;

  return (Scheme_Object *)ip;
}

Scheme_Object *
scheme_make_named_file_input_port(FILE *fp, Scheme_Object *name)
{
  return _scheme_make_named_file_input_port(fp, name, 0);
}

Scheme_Object *
scheme_make_file_input_port(FILE *fp)
{
  return scheme_make_named_file_input_port(fp, scheme_intern_symbol("file"));
}

/*========================================================================*/
/*                           fd input ports                               */
/*========================================================================*/

#ifdef MZ_FDS

# ifdef WINDOWS_FILE_HANDLES
static long WINAPI WindowsFDReader(Win_FD_Input_Thread *th);
static void WindowsFDICleanup(Win_FD_Input_Thread *th);
typedef BOOL (WINAPI* CSI_proc)(HANDLE);

static CSI_proc get_csi(void)
{
  static int tried_csi = 0;
  static CSI_proc csi;
  
  START_XFORM_SKIP;      
  if (!tried_csi) {
    HMODULE hm;
    hm = LoadLibrary("kernel32.dll");
    if (hm)
      csi = (CSI_proc)GetProcAddress(hm, "CancelSynchronousIo");
    else
      csi = NULL;
    tried_csi = 1;
  }
  END_XFORM_SKIP;
  return csi;
}

# endif

/* forward decl: */
static void fd_need_wakeup(Scheme_Input_Port *port, void *fds);

#ifdef SOME_FDS_ARE_NOT_SELECTABLE
static int try_get_fd_char(int fd, int *ready)
{
  int old_flags, c;
  unsigned char buf[1];

  old_flags = fcntl(fd, F_GETFL, 0);
  fcntl(fd, F_SETFL, old_flags | MZ_NONBLOCKING);
  do {
    c = read(fd, buf, 1);
  } while ((c == -1) && errno == EINTR);
  fcntl(fd, F_SETFL, old_flags);

  if (c < 0) {
    *ready = 0;
    return 0;
  } else {
    *ready = 1;
    if (!c)
      return EOF;
    else
      return buf[0];
  }
}
#endif

static int
fd_byte_ready (Scheme_Input_Port *port)
{
  Scheme_FD *fip;

  fip = (Scheme_FD *)port->port_data;

  if (fip->regfile || port->closed)
    return 1;

  if (fip->bufcount)
    return 1;
  else {
#ifdef WINDOWS_FILE_HANDLES
    if (!fip->th) {
      /* No thread -- so wait works. This case isn't actually used
	 right now, because wait doesn't seem to work reliably for
	 anything that we can recognize other than regfiles, which are
	 handled above. */
      if (WaitForSingleObject((HANDLE)fip->fd, 0) == WAIT_OBJECT_0)
	return 1;
    } else {
      /* Has the reader thread pulled in data? */
      if (fip->th->checking) {
	/* The thread is still trying, last we knew. Check the
	   data-is-ready sema: */
	if (WaitForSingleObject(fip->th->ready_sema, 0) == WAIT_OBJECT_0) {
	  fip->th->checking = 0;
	  return 1;
	}
      } else if (fip->th->avail || fip->th->err || fip->th->eof)
	return 1; /* other thread found data */
      else {
	/* Doesn't have anything, and it's not even looking. Tell it
	   to look: */
	fip->th->checking = 1;
	ReleaseSemaphore(fip->th->checking_sema, 1, NULL);
      }
    }

    return 0;
#else
    int r;
# ifdef HAVE_POLL_SYSCALL
    GC_CAN_IGNORE struct pollfd pfd[1];
    pfd[0].fd = fip->fd;
    pfd[0].events = POLLIN;
    do {
      r = poll(pfd, 1, 0);
    } while ((r == -1) && (errno == EINTR));
# else
    DECL_FDSET(readfds, 1);
    DECL_FDSET(exnfds, 1);
    struct timeval time = {0, 0};

    INIT_DECL_RD_FDSET(readfds);
    INIT_DECL_ER_FDSET(exnfds);

    MZ_FD_ZERO(readfds);
    MZ_FD_ZERO(exnfds);
    MZ_FD_SET(fip->fd, readfds);
    MZ_FD_SET(fip->fd, exnfds);

    do {
      r = select(fip->fd + 1, readfds, NULL, exnfds, &time);
    } while ((r == -1) && (errno == EINTR));
# endif

# ifdef SOME_FDS_ARE_NOT_SELECTABLE
    /* Try a non-blocking read: */
    if (!r && !fip->textmode) {
      int c, ready;

      c = try_get_fd_char(fip->fd, &ready);
      if (ready) {
	if (c != EOF) {
	  fip->buffpos = 0;
	  fip->buffer[0] = (unsigned char)c;
	  fip->bufcount = 1;
	}
	r = 1;
      }
    }
# endif

    return r;
#endif
  }
}

MZ_DO_NOT_INLINE(static intptr_t fd_get_string_slow(Scheme_Input_Port *port,
                                                    char *buffer, intptr_t offset, intptr_t size,
                                                    int nonblock,
                                                    Scheme_Object *unless));

static intptr_t fd_get_string_slow(Scheme_Input_Port *port,
                               char *buffer, intptr_t offset, intptr_t size,
                               int nonblock,
                               Scheme_Object *unless)
{
  Scheme_FD *fip;
  intptr_t bc;

  fip = (Scheme_FD *)port->port_data;

  while (1) {
    /* Loop until a read succeeds. */
    int none_avail = 0;
    int target_size, target_offset, ext_target;
    char *target;
    Scheme_Object *sema;

    /* If no chars appear to be ready, go to sleep. */
    while (!fd_byte_ready(port)) {
      if (nonblock > 0)
        return 0;
      
#ifdef WINDOWS_FILE_HANDLES
      sema = NULL;
#else
      sema = scheme_fd_to_semaphore(fip->fd, MZFD_CREATE_READ, 0);
#endif
      if (sema)
        scheme_wait_sema(sema, nonblock ? -1 : 0);
      else 
        scheme_block_until_unless((Scheme_Ready_Fun)fd_byte_ready,
                                  (Scheme_Needs_Wakeup_Fun)fd_need_wakeup,
                                  (Scheme_Object *)port,
                                  0.0, unless,
                                  nonblock);

      scheme_wait_input_allowed(port, nonblock);

      if (scheme_unless_ready(unless))
        return SCHEME_UNLESS_READY;
    }

    if (port->closed) {
      /* Another thread closed the input port while we were waiting. */
      /* Call scheme_getc to signal the error */
      scheme_get_byte((Scheme_Object *)port);
    }

    /* Another thread might have filled the buffer, or
       if SOME_FDS_ARE_NOT_SELECTABLE is set,
       fd_byte_ready might have read one character. */
    if (fip->bufcount) {
      bc = ((size <= fip->bufcount)
            ? size
            : fip->bufcount);

      memcpy(buffer + offset, fip->buffer + fip->buffpos, bc);
      fip->buffpos += bc;
      fip->bufcount -= bc;

      return bc;
    }

    if ((size >= MZPORT_FD_DIRECT_THRESHOLD) && (fip->flush != MZ_FLUSH_ALWAYS)) {
      ext_target = 1;
      target = buffer;
      target_offset = offset;
      target_size = size;
    } else {
      ext_target = 0;
      target = (char *)fip->buffer;
      target_offset = 0;
      if (fip->flush == MZ_FLUSH_ALWAYS)
        target_size = 1;
      else
        target_size = MZPORT_FD_BUFFSIZE;
    }

#ifdef WINDOWS_FILE_HANDLES
    if (!fip->th) {
      /* We can read directly. This must be a regular file, where
         reading never blocks. */
      DWORD rgot, delta;

      if (fip->textmode) {
        ext_target = 0;
        target = fip->buffer;
        target_offset = 0;
        if (fip->flush == MZ_FLUSH_ALWAYS)
          target_size = 1;
        else
          target_size = MZPORT_FD_BUFFSIZE;
      }

      rgot = target_size;

      /* Pending CR in text mode? */
      if (fip->textmode == 2) {
        delta = 1;
        if (rgot > 1)
          rgot--;
        fip->buffer[0] = '\r';
      } else
        delta = 0;

      if (ReadFile((HANDLE)fip->fd, target XFORM_OK_PLUS target_offset + delta, rgot, &rgot, NULL)) {
        bc = rgot;
      } else {
        int errid;
        bc = -1;
        errid = GetLastError();
        errno = errid;
      }

      /* bc == 0 and no err => EOF */

      /* Finish text-mode handling: */
      if (fip->textmode && (bc >= 0)) {
        int i, j;
        unsigned char *buf;

        if (fip->textmode == 2) {
          /* we had added a CR */
          bc++;
          fip->textmode = 1;
        }

        /* If bc is only 1, then we've reached the end, and
           any leftover CR there should stay. */
        if (bc > 1) {
          /* Collapse CR-LF: */
          buf = fip->buffer;
          for (i = 0, j = 0; i < bc - 1; i++) {
            if ((buf[i] == '\r')
                && (buf[i+1] == '\n')) {
              buf[j++] = '\n';
              i++;
            } else
              buf[j++] = buf[i];
          }
          if (i < bc) /* common case: didn't end with CRLF */
            buf[j++] = buf[i];
          bc = j;
          /* Check for CR at end; if there, save it to maybe get a
             LF on the next read: */
          if (buf[bc - 1] == '\r') {
            bc--;
            fip->textmode = 2; /* 2 indicates a leftover CR */
          }
        }
      }

    } else {
      ext_target = 0;

      /* If we get this far, there's definitely data available.
         Extract data made available by the reader thread. */
      if (fip->th->eof) {
        bc = 0;
        if (fip->th->eof != INVALID_HANDLE_VALUE) {
          ReleaseSemaphore(fip->th->eof, 1, NULL);
          fip->th->eof = NULL;
        }
      } else if (fip->th->err) {
        bc = -1;
        errno = fip->th->err;
      } else {
        bc = fip->th->avail;
        fip->th->avail = 0;
      }
    }
#else
    if (fip->regfile) {
      do {
        bc = read(fip->fd, target + target_offset, target_size);
      } while ((bc == -1) && (errno == EINTR));
    } else {
      /* We use a non-blocking read here, even though we've waited
         for input above, because an external process might have
         gobbled the characters that we expected to get. */
      int old_flags;

      old_flags = fcntl(fip->fd, F_GETFL, 0);
      fcntl(fip->fd, F_SETFL, old_flags | MZ_NONBLOCKING);
      do {
        bc = read(fip->fd, target + target_offset, target_size);
      } while ((bc == -1) && errno == EINTR);
      fcntl(fip->fd, F_SETFL, old_flags);

      if ((bc == -1) && (errno == EAGAIN)) {
        none_avail = 1;
        bc = 0;
      }
    }
#endif

    if (!none_avail) {
      if (ext_target && (bc > 0)) {
        return bc;
      }

      fip->bufcount = bc;

      if (fip->bufcount < 0) {
        fip->bufcount = 0;
        fip->buffpos = 0;
        scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
                         "error reading from stream port\n"
                         "  port: %V\n"
                         "  system error: " FILENAME_EXN_E,
                         port->name, errno);
        return 0;
      }

      if (!fip->bufcount) {
        fip->buffpos = 0;
        return EOF;
      } else {
        bc = ((size <= fip->bufcount)
              ? size
              : fip->bufcount);

        memcpy(buffer + offset, fip->buffer, bc);
        fip->buffpos = bc;
        fip->bufcount -= bc;

        return bc;
      }
    } else if (nonblock > 0) {
      return 0;
    }
  }
}

static intptr_t fd_get_string(Scheme_Input_Port *port,
			  char *buffer, intptr_t offset, intptr_t size,
			  int nonblock,
			  Scheme_Object *unless)
{
  Scheme_FD *fip;
  intptr_t bc;

  /* Buffer-reading fast path is designed to avoid GC, 
     and thus avoid MZ_PRECISE_GC instrumentation. */

  if (unless && scheme_unless_ready(unless))
    return SCHEME_UNLESS_READY;

  fip = (Scheme_FD *)port->port_data;

  if (fip->bufcount) {
    if (size == 1) {
      buffer[offset] = fip->buffer[fip->buffpos++];
      --fip->bufcount;
      return 1;
    } else {
      bc = ((size <= fip->bufcount)
	    ? size
	    : fip->bufcount);

      memcpy(buffer + offset, fip->buffer + fip->buffpos, bc);
      fip->buffpos += bc;
      fip->bufcount -= bc;

      return bc;
    }
  } else {
    if ((nonblock == 2) && (fip->flush == MZ_FLUSH_ALWAYS))
      return 0;

    return fd_get_string_slow(port, buffer, offset, size, nonblock, unless);
  }
}

static void
fd_close_input(Scheme_Input_Port *port)
{
  Scheme_FD *fip;

  fip = (Scheme_FD *)port->port_data;

#ifdef WINDOWS_FILE_HANDLES
  if (fip->th) {
    CSI_proc csi;

    /* -1 for checking means "shut down" */
    fip->th->checking = -1;
    ReleaseSemaphore(fip->th->checking_sema, 1, NULL);

    if (fip->th->eof && (fip->th->eof != INVALID_HANDLE_VALUE)) {
      ReleaseSemaphore(fip->th->eof, 1, NULL);
      fip->th->eof = NULL;
    }

    csi = get_csi();
    if (csi) {
      /* Helps thread wake up. Otherwise, it's possible for the
         thread to stay stuck trying to read, in which case the
         file handle (probably a pipe) doesn't get closed. */
      csi(fip->th->thread);
    }

    /* Try to get out of cleaning up the records (since they can't be
       cleaned until the thread is also done: */
    if (WaitForSingleObject(fip->th->you_clean_up_sema, 0) != WAIT_OBJECT_0) {
      /* The other thread exited and left us with clean-up: */
      WindowsFDICleanup(fip->th);
    } /* otherwise, thread is responsible for clean-up */
  } else {
    int rc;
    rc = adj_refcount(fip->refcount, -1);
    if (!rc) {
      CloseHandle((HANDLE)fip->fd);
    }
  }
#else
 {
   int rc;
   rc = adj_refcount(fip->refcount, -1);
   if (!rc) {
     int cr;
     do {
       cr = close(fip->fd);
     } while ((cr == -1) && (errno == EINTR));
# ifdef USE_FCNTL_AND_FORK_FOR_FILE_LOCKS
     release_lockf(fip->fd);
# endif
     (void)scheme_fd_to_semaphore(fip->fd, MZFD_REMOVE, 0);
   }
 }
#endif
}

static void
fd_init_close_input(Scheme_Input_Port *port)
{
  /* never actually opened! */
}

static void
fd_need_wakeup(Scheme_Input_Port *port, void *fds)
{
  Scheme_FD *fip;

#ifdef WINDOWS_FILE_HANDLES
#else
  void *fds2;
  int n;
#endif

  fip = (Scheme_FD *)port->port_data;

#ifdef WINDOWS_FILE_HANDLES
  if (fip->th) {
    /* See fd_byte_ready */
    if (!fip->th->checking) {
      if (fip->th->avail || fip->th->err || fip->th->eof) {
	/* Data is ready. We shouldn't be trying to sleep, so force an
	   immediate wake-up: */
	scheme_add_fd_nosleep(fds);
      } else {
	fip->th->checking = 1;
	ReleaseSemaphore(fip->th->checking_sema, 1, NULL);
	scheme_add_fd_handle((void *)fip->th->ready_sema, fds, 1);
      }
    } else
      scheme_add_fd_handle((void *)fip->th->ready_sema, fds, 1);
  } else if (fip->regfile) {
    /* regular files never block */
    scheme_add_fd_nosleep(fds);
  } else {
    /* This case is not currently used. See fd_byte_ready. */
    scheme_add_fd_handle((void *)fip->fd, fds, 0);
  }
#else
  n = fip->fd;
  MZ_FD_SET(n, (fd_set *)fds);
  fds2 = MZ_GET_FDSET(fds, 2);
  MZ_FD_SET(n, (fd_set *)fds2);
#endif
}

static int fd_input_buffer_mode(Scheme_Port *p, int mode)
{
  Scheme_FD *fd;
  Scheme_Input_Port *ip = (Scheme_Input_Port *)p;
    
  fd = (Scheme_FD *)ip->port_data;

  if (mode < 0) {
    return fd->flush;
  } else {
    fd->flush = mode;
    return mode;
  }
}

static Scheme_Object *
make_fd_input_port(int fd, Scheme_Object *name, int regfile, int win_textmode, int *refcount, int internal)
{
  Scheme_Input_Port *ip;
  Scheme_FD *fip;
  unsigned char *bfr;
  int start_closed = 0;

  fip = MALLOC_ONE_RT(Scheme_FD);
#ifdef MZTAG_REQUIRED
  fip->type = scheme_rt_input_fd;
#endif

  bfr = (unsigned char *)scheme_malloc_atomic(MZPORT_FD_BUFFSIZE);
  fip->buffer = bfr;

  fip->fd = fd;
  fip->bufcount = 0;

  fip->regfile = regfile;
#ifdef SOME_FDS_ARE_NOT_SELECTABLE
  if (regfile || isatty(fd))
    fip->textmode = 1;
#else
  fip->textmode = win_textmode;  
#endif

  if (refcount) {
    fip->refcount = refcount;
    if (!adj_refcount(refcount, 1)) {
      /* fd is already closed! */
      start_closed = 1;
    }
  }

  fip->flush = MZ_FLUSH_NEVER;

  ip = scheme_make_input_port(fd_input_port_type,
			      fip,
			      name,
			      fd_get_string,
			      NULL,
			      scheme_progress_evt_via_get,
			      scheme_peeked_read_via_get,
			      fd_byte_ready,
			      (start_closed 
			       ? fd_init_close_input
			       : fd_close_input),
			      fd_need_wakeup,
			      !internal);
  ip->p.buffer_mode_fun = fd_input_buffer_mode;

  ip->pending_eof = 1; /* means that pending EOFs should be tracked */

#ifdef WINDOWS_FILE_HANDLES
  if (!regfile && !start_closed) {
    /* To get non-blocking I/O for anything that can block, we create
       a separate reader thread.

       Yes, Windows NT pipes support non-blocking reads, but there
       doesn't seem to be any way to use WaitForSingleObject to sleep
       until characters are ready. PeekNamedPipe can be used for
       polling, but not sleeping. */

    Win_FD_Input_Thread *th;
    DWORD id;
    HANDLE h;
    OS_SEMAPHORE_TYPE sm;

    th = (Win_FD_Input_Thread *)malloc(sizeof(Win_FD_Input_Thread));
    fip->th = th;

    /* Replace buffer with a malloced one: */
    bfr = (unsigned char *)malloc(MZPORT_FD_BUFFSIZE);
    fip->buffer = bfr;
    th->buffer = bfr;

    th->fd = (HANDLE)fd;
    th->avail = 0;
    th->err = 0;
    th->eof = NULL;
    th->checking = 0;
    
    sm = CreateSemaphore(NULL, 0, 1, NULL);
    th->checking_sema = sm;
    sm = CreateSemaphore(NULL, 0, 1, NULL);
    th->ready_sema = sm;
    sm = CreateSemaphore(NULL, 1, 1, NULL);
    th->you_clean_up_sema = sm;
    th->refcount = refcount;

    h = CreateThread(NULL, 4096, (LPTHREAD_START_ROUTINE)WindowsFDReader, th, 0, &id);

    th->thread = h;

    scheme_remember_thread(h, 1);
  }
#endif

  if (start_closed)
    scheme_close_input_port((Scheme_Object *)ip);

  return (Scheme_Object *)ip;
}

# ifdef WINDOWS_FILE_HANDLES

static long WINAPI WindowsFDReader(Win_FD_Input_Thread *th)
  XFORM_SKIP_PROC
{
  DWORD toget, got;
  int perma_eof = 0;
  HANDLE eof_wait = NULL;

  if (GetFileType((HANDLE)th->fd) == FILE_TYPE_PIPE) {
    /* Reading from a pipe will return early when data is available. */
    toget = MZPORT_FD_BUFFSIZE;
  } else {
    /* Non-pipe: get one char at a time: */
    toget = 1;
  }

  while (!perma_eof && !th->err) {
    /* Wait until we're supposed to look for input: */
    WaitForSingleObject(th->checking_sema, INFINITE);

    if (th->checking < 0)
      break;

    if (ReadFile(th->fd, th->buffer, toget, &got, NULL)) {
      th->avail = got;
      if (!got) {
	/* We interpret a send of 0 bytes as a mid-stream EOF. */
	eof_wait = CreateSemaphore(NULL, 0, 1, NULL);
	th->eof = eof_wait;
      }
    } else {
      int err;
      err = GetLastError();
      if (err == ERROR_BROKEN_PIPE) {
	th->eof = INVALID_HANDLE_VALUE;
	perma_eof = 1;
      } else
	th->err = err;
    }

    /* Notify main program that we found something: */
    ReleaseSemaphore(th->ready_sema, 1, NULL);

    if (eof_wait) {
      WaitForSingleObject(eof_wait, INFINITE);
      eof_wait = NULL;
    }
  }

  /* We have to clean up if the main program has abandoned us: */
  if (WaitForSingleObject(th->you_clean_up_sema, 0) != WAIT_OBJECT_0) {
    WindowsFDICleanup(th);
  } /* otherwise, main program is responsible for clean-up */

  return 0;
}

static void WindowsFDICleanup(Win_FD_Input_Thread *th)
  XFORM_SKIP_PROC
{
  int rc;

  CloseHandle(th->checking_sema);
  CloseHandle(th->ready_sema);
  CloseHandle(th->you_clean_up_sema);

  rc = adj_refcount(th->refcount, -1);
  if (!rc) CloseHandle(th->fd);

  free(th->buffer);
  free(th);
}

# endif

#endif

Scheme_Object *
scheme_make_fd_input_port(int fd, Scheme_Object *name, int regfile, int textmode)
{
#ifdef MZ_FDS
  return make_fd_input_port(fd, name, regfile, textmode, NULL, 0);
#else
  return NULL;
#endif
}

/*========================================================================*/
/*                    OSKit console input ports                           */
/*========================================================================*/

#ifdef USE_OSKIT_CONSOLE

# ifdef OSKIT_TEST
static Scheme_Object *normal_stdin;
static int direct_cons_trygetchar() { return scheme_byte_ready(normal_stdin) ? scheme_get_byte(normal_stdin) : -1; }
static void direct_cons_putchar(int c) { }
# define convert_scan_code(x) x
# else
#  include "pc_keys.inc"
# endif

typedef struct osk_console_input {
  MZTAG_IF_REQUIRED
  int count, size, ready;
  unsigned char *buffer;
  struct osk_console_input *next; /* typeahead */
} osk_console_input;

static int
osk_byte_ready (Scheme_Input_Port *port)
{
  osk_console_input *osk, *orig;
  int k;

  if (port->closed)
    return 1;

  osk = orig = (osk_console_input *)port->port_data;

  while (osk->ready) {
    if (osk->next)
      osk = osk->next;
    else {
      osk->next = MALLOC_ONE_RT(osk_console_input);
#ifdef MZTAG_REQUIRED
      osk->type = scheme_rt_oskit_console_input;
#endif
      osk = osk->next;
      osk->count = osk->size = osk->ready = 0;
      osk->buffer = NULL;
      osk->next = NULL;
    }
  }

  k = direct_cons_trygetchar();
  k = convert_scan_code(k); /* defined in pc_keys.inc; handles ctl-alt-del */
  if (k > 0) {
    if (k == 3) { /* Ctl-C */
      scheme_break_thread(NULL);
    } else if (k == 4) { /* Ctl-D */
      if (!osk->count)
	/* ready with !count => EOF */
	osk->ready = 1;
    } else if (k == 8) { /* Backspace */
      if (osk->count) {
	direct_cons_putchar(8);
	direct_cons_putchar(' '); /* space erases old letter */
	direct_cons_putchar(8);
	--osk->count;
      }
    } else {
      if (osk->count == osk->size) {
	char *naya;
	osk->size = osk->size ? 2 * osk->size : 256;
	naya = scheme_malloc_atomic(osk->size);
	memcpy(naya, osk->buffer, osk->count);
	osk->buffer = naya;
      }
      osk->buffer[osk->count++] = k;
      if (k == 13 || k == 10) { /* Return/newline */
	direct_cons_putchar(13);
	direct_cons_putchar(10);
	osk->ready = 1;
      } else
	direct_cons_putchar(k);
    }
  }

  if (orig->ready)
    return 1;
  else
    return 0;
}

static int osk_get_string(Scheme_Input_Port *port,
			  char *buffer, int offset, int size,
			  int nonblock, Scheme_Object *unless)
{
  int c;
  osk_console_input *osk;

  while (!osk_byte_ready(port)) {
    if (nonblock > 0) {
      return 0;
    }
    
    scheme_block_until_unless(osk_byte_ready, NULL, (Scheme_Object *)port, 0.0,
			      unless,
			      nonblock);

    scheme_wait_input_allowed(port, nonblock);
    
    if (scheme_unless_ready(unless))
      return SCHEME_UNLESS_READY;
  }

  if (port->closed) {
    /* Another thread closed the input port while we were waiting. */
    /* Call scheme_getc to signal the error */
    scheme_getc((Scheme_Object *)port);
  }

  osk = (osk_console_input *)port->port_data;

  if (!osk->count) {
    /* EOF */
    osk->ready = 0;
    return EOF;
  }

  c = osk->buffer[osk->ready - 1];
  osk->ready++;
  if (osk->ready > osk->count) {
    if (osk->next) {
      /* Copy typeahead to here */
      osk_console_input *next = osk->next;
      memcpy(osk, next, sizeof(osk_console_input));
    } else
      osk->ready = osk->count = 0;
  }

  buffer[offset] = c;
  return 1;
}

static void
osk_close_input(Scheme_Input_Port *port)
{
}

static void
osk_need_wakeup(Scheme_Input_Port *port, void *fds)
{
# ifdef OSKIT_TEST
  /* for testing, write to stdout is almost certainly ready: */
  void *fdw;
  fdw = MZ_GET_FDSET(fds, 1);
  MZ_FD_SET(1, (fd_set *)fdw);
# endif

  /* In OSKit, makes select() return immediately */
  MZ_FD_SET(0, (fd_set *)fds);
}

static Scheme_Object *
make_oskit_console_input_port()
{
  Scheme_Input_Port *ip;
  osk_console_input *osk;

  osk = MALLOC_ONE_RT(osk_console_input);
#ifdef MZTAG_REQUIRED
  osk->type = scheme_rt_oskit_console_input;
#endif

  osk->count = osk->size = osk->ready = 0;
  osk->buffer = NULL;
  osk->next = NULL;

# ifdef OSKIT_TEST
  REGISTER_SO(normal_stdin);
  normal_stdin = scheme_make_named_file_input_port(stdin, scheme_intern_symbol("stdin"));
# endif

  ip = scheme_make_input_port(oskit_console_input_port_type,
			      osk,
			      scheme_intern_symbol("stdin"),
			      osk_get_string,
			      NULL,
			      scheme_progress_evt_via_get,
			      scheme_get_string_unless_via_get,
			      osk_byte_ready,
			      osk_close_input,
			      osk_need_wakeup,
			      1);

  return (Scheme_Object *)ip;
}

void scheme_check_keyboard_input(void)
{
  if (!osk_not_console)
    osk_byte_ready((Scheme_Input_Port *)scheme_orig_stdin_port);
}

#endif

/*========================================================================*/
/*                           FILE output ports                            */
/*========================================================================*/

/* Note that we don't try to implement non-blocking writes on FILE
   objects. In Unix, a program could conceiveably open a named pipe
   and block on it. */

static void file_flush(Scheme_Output_Port *port)
{
  if (fflush(((Scheme_Output_File *)port->port_data)->f)) {
    scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		     "error flushing file port\n"
                     "  system error: %e",
		     errno);
  }
}

static intptr_t
file_write_string(Scheme_Output_Port *port,
		  const char *str, intptr_t d, intptr_t llen,
		  int rarely_block, int enable_break)
{
  FILE *fp;
  intptr_t len = llen;

  fp = ((Scheme_Output_File *)port->port_data)->f;

  if (!len) {
    file_flush(port);
    return 0;
  }

  if (fwrite(str XFORM_OK_PLUS d, len, 1, fp) != 1) {
    scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		     "error writing to file port\n"
                     "  system error: %e",
		     errno);
    return 0;
  }

  if (rarely_block) {
    file_flush(port);
  } else {
    while (len--) {
      if (str[d] == '\n' || str[d] == '\r') {
	file_flush(port);
	break;
      }
      d++;
    }
  }

  return llen;
}

static void
file_close_output(Scheme_Output_Port *port)
{
  Scheme_Output_File *fop = (Scheme_Output_File *)port->port_data;
  FILE *fp = fop->f;

  fclose(fp);
}

Scheme_Object *
scheme_make_file_output_port(FILE *fp)
{
  Scheme_Output_File *fop;
  Scheme_Output_Port *op;

  if (!fp)
    scheme_signal_error("make-file-out-port(internal): "
			"null file pointer");

  fop = MALLOC_ONE_RT(Scheme_Output_File);
#ifdef MZTAG_REQUIRED
  fop->type = scheme_rt_output_file;
#endif

  fop->f = fp;

  op = scheme_make_output_port(file_output_port_type,
			       fop,
			       scheme_intern_symbol("file"),
			       scheme_write_evt_via_write,
			       file_write_string,
			       NULL,
			       file_close_output,
			       NULL,
			       NULL,
			       NULL,
			       1);
  op->p.buffer_mode_fun = file_buffer_mode;

  return (Scheme_Object *)op;
}

/*========================================================================*/
/*                             fd output ports                            */
/*========================================================================*/

#ifdef MZ_FDS

#ifdef WINDOWS_FILE_HANDLES
static long WINAPI WindowsFDWriter(Win_FD_Output_Thread *oth);
static void WindowsFDOCleanup(Win_FD_Output_Thread *oth);
#endif

static int
fd_flush_done(Scheme_Object *port)
{
  Scheme_FD *fop;
  Scheme_Output_Port *op;

  op = scheme_output_port_record(port);

  fop = (Scheme_FD *)op->port_data;

  return !fop->flushing;
}

static void wait_until_fd_flushed(Scheme_Output_Port *op, int enable_break)
{
  scheme_block_until_enable_break(fd_flush_done, NULL, (Scheme_Object *)op, 
				  0.0, enable_break);
}

#ifdef WINDOWS_FILE_HANDLES
static int win_fd_flush_done(Scheme_Object *_oth)
{
  /* For checking whether the output thread has finished a flush. */

  Win_FD_Output_Thread *oth = (Win_FD_Output_Thread *)_oth;
  int done;

  WaitForSingleObject(oth->lock_sema, INFINITE);
  if (oth->nonblocking) {
    if (oth->needflush) {
      oth->needflush = 0;
      oth->flushed = 0;
      ReleaseSemaphore(oth->work_sema, 1, NULL); /* start trying to flush */
      done = 0;
    } else
      done = oth->flushed;
  } else
    done = (oth->err_no || !oth->buflen);
  ReleaseSemaphore(oth->lock_sema, 1, NULL);

  return done;
}

static void win_fd_flush_needs_wakeup(Scheme_Object *_oth, void *fds)
{
  /* For sleping until the output thread has finished a flush. */

  /* Double-check that we're not already done: */
  if (win_fd_flush_done(_oth))
    scheme_add_fd_nosleep(fds);
  else {
    /* Not done. Thread will notify us through ready_sema: */
    Win_FD_Output_Thread *oth = (Win_FD_Output_Thread *)_oth;

    scheme_add_fd_handle(oth->ready_sema, fds, 1);
  }
}
#endif

static int
fd_write_ready (Scheme_Object *port)
{
  /* As always, the result of this function is only meaningful when
     the port has been flushed. */

  Scheme_FD *fop;
  Scheme_Output_Port *op;

  op = scheme_output_port_record(port);
  fop = (Scheme_FD *)op->port_data;

  if (fop->regfile || op->closed)
    return 1;

#ifdef WINDOWS_FILE_HANDLES
  if (fop->oth) {
    /* Pipe output that can block... */
    int retval;
    Win_FD_Output_Thread *oth = fop->oth;

    WaitForSingleObject(oth->lock_sema, INFINITE);
    if (oth->nonblocking) {
      if (oth->needflush) {
	oth->needflush = 0;
	oth->flushed = 0;
	ReleaseSemaphore(oth->work_sema, 1, NULL); /* start trying to flush */
	retval = 0;
      } else
	retval = oth->flushed;
    } else
      retval = (oth->err_no || (oth->buflen < MZPORT_FD_BUFFSIZE));
    if (!retval)
      WaitForSingleObject(oth->ready_sema, 0); /* clear any leftover state */
    ReleaseSemaphore(oth->lock_sema, 1, NULL);

    return retval;
  } else
    return 1; /* non-blocking output, such as a console, or haven't written yet */
#else
  {
    int sr;
# ifdef HAVE_POLL_SYSCALL
    GC_CAN_IGNORE struct pollfd pfd[1];
    pfd[0].fd = fop->fd;
    pfd[0].events = POLLOUT;
    do {
      sr = poll(pfd, 1, 0);
    } while ((sr == -1) && (errno == EINTR));
# else
    DECL_FDSET(writefds, 1);
    DECL_FDSET(exnfds, 1);
    struct timeval time = {0, 0};

    INIT_DECL_WR_FDSET(writefds);
    INIT_DECL_ER_FDSET(exnfds);

    MZ_FD_ZERO(writefds);
    MZ_FD_ZERO(exnfds);
    MZ_FD_SET(fop->fd, writefds);
    MZ_FD_SET(fop->fd, exnfds);

    do {
      sr = select(fop->fd + 1, NULL, writefds, exnfds, &time);
    } while ((sr == -1) && (errno == EINTR));
#endif

    return sr;
  }
#endif
}


static void
fd_write_need_wakeup(Scheme_Object *port, void *fds)
{
  Scheme_Output_Port *op;
  Scheme_FD *fop;

#ifdef WINDOWS_FILE_HANDLES
#else
  void *fds2;
  int n;
#endif

  op = scheme_output_port_record(port);
  fop = (Scheme_FD *)op->port_data;

#ifdef WINDOWS_FILE_HANDLES
  if (fop->oth && !fd_write_ready(port))
    scheme_add_fd_handle(fop->oth->ready_sema, fds, 1);
  else
    scheme_add_fd_nosleep(fds);
#else
  n = fop->fd;
  fds2 = MZ_GET_FDSET(fds, 1);
  MZ_FD_SET(n, (fd_set *)fds2);
  fds2 = MZ_GET_FDSET(fds, 2);
  MZ_FD_SET(n, (fd_set *)fds2);
#endif
}

static void release_flushing_lock(void *_fop)
{
  Scheme_FD *fop;

  fop = (Scheme_FD *)_fop;

  fop->flushing = 0;
}

static intptr_t flush_fd(Scheme_Output_Port *op,
		     const char * volatile bufstr, volatile uintptr_t buflen, volatile uintptr_t offset,
		     int immediate_only, int enable_break)
     /* immediate_only == 1 => write at least one character, then give up;
	immediate_only == 2 => never block */
{
  Scheme_FD * volatile fop = (Scheme_FD *)op->port_data;
  volatile intptr_t wrote = 0;

  if (fop->flushing) {
    if (scheme_force_port_closed) {
      /* Give up */
      return 0;
    }

    if (immediate_only == 2)
      return 0;

    wait_until_fd_flushed(op, enable_break);

    if (op->closed)
      return 0;
  }

  if (!bufstr) {
    bufstr = (char *)fop->buffer;
    buflen = fop->bufcount;
  }

  if (buflen) {
    fop->flushing = 1;
    fop->bufcount = 0;
    /* If write is interrupted, we drop chars on the floor.
       Not ideal, but we'll go with it for now.
       Note that write_string_avail supports break-reliable
       output through `immediate_only'. */

    while (1) {
      intptr_t len;
      int errsaved, full_write_buffer;

#ifdef WINDOWS_FILE_HANDLES
      DWORD winwrote;

      full_write_buffer = 0;

      if (fop->regfile) {
	/* Regular files never block, so this code looks like the Unix
	   code.  We've cheated in the make_fd proc and called
	   consoles regular files, because they cannot block, either. */
	int orig_len;

	if (fop->textmode) {
	  /* Convert LF to CRLF. We're relying on the fact that WriteFile
	     will write everything. */
	  int c = 0;
	  unsigned int i;

	  for (i = offset; i < buflen; i++) {
	    if (bufstr[i] == '\n')
	      c++;
	  }

	  orig_len = buflen - offset;

	  if (c) {
	    char *naya;
	    int j;

	    naya = scheme_malloc_atomic(orig_len + c);

	    for (i = offset, j = 0; i < buflen; i++) {
	      if (bufstr[i] == '\n') {
		naya[j++] = '\r';
		naya[j++] = '\n';
	      } else
		naya[j++] = bufstr[i];
	    }

	    bufstr = naya;
	    offset = 0;
	    buflen = orig_len + c;
	  }
	} else
	  orig_len = 0; /* not used */

	/* Write bytes. If we try to write too much at once, the result
	   is ERROR_NOT_ENOUGH_MEMORY (as opposed to a partial write). */
	{
	  int ok;
	  intptr_t towrite = buflen - offset;

	  while (1) {
	    ok = WriteFile((HANDLE)fop->fd, bufstr XFORM_OK_PLUS offset, towrite, &winwrote, NULL);
	    if (!ok)
	      errsaved = GetLastError();
	    
	    if (!ok && (errsaved == ERROR_NOT_ENOUGH_MEMORY)) {
	      towrite = towrite >> 1;
	      if (!towrite)
		break;
	    } else
	      break;
	  }

	  if (ok) {
	    if (fop->textmode) {
	      if (winwrote != buflen) {
		/* Trouble! This shouldn't happen. We pick an random error msg. */
		errsaved = ERROR_NEGATIVE_SEEK;
		len = -1;
	      } else {
		len = orig_len;
		buflen = orig_len; /* so we don't loop! */
	      }
	    } else
	      len = winwrote;
	  } else {
	    len = -1;
	  }
	}
      } else {
	errsaved = 0;
	len = -1;

	/* If we don't have a thread yet, we'll need to start it. If
	   we have a non-blocking pipe, we can try the write (and
	   we'll still need the thread to determine when the data is
	   flushed). */
	if (!fop->oth || fop->oth->nonblocking) {
	  int nonblocking;

	  /* If we don't have a thread, this is our first write attempt.
	     Determine whether this is a non-blocking pipe: */
	  if (!fop->oth) {
	    /* The FILE_TYPE_PIPE test is currently redundant, I think,
	       but better safe than sorry. */
	    nonblocking = ((scheme_stupid_windows_machine < 0)
			   && (GetFileType((HANDLE)fop->fd) == FILE_TYPE_PIPE));
	  } else
	    nonblocking = 1; /* must be, or we would not have gotten here */

	  if (nonblocking) {
	    /* Unless we're still trying to flush old data, write to the
	       pipe and have the other thread start flushing it. */
	    DWORD nonblock = PIPE_NOWAIT;
	    int ok, flushed;

	    if (fop->oth) {
	      if (fop->oth->needflush) {
		/* Not flushed, but we haven't promised not to block: */
		flushed = 1;
	      } else {
		WaitForSingleObject(fop->oth->lock_sema, INFINITE);
		flushed = fop->oth->flushed;
		ReleaseSemaphore(fop->oth->lock_sema, 1, NULL);
	      }
	    } else
	      flushed = 1; /* haven't written anything before */

	    if (flushed) {
	      /* Put the pipe in non-blocking mode and write. */

	      int towrite;

	      towrite = buflen - offset;

	      /* Apparently, the semantics of non-blocking pipe writes
	         is not partial writes, but giving up entirely when
	         the other end isn't being read. In other words, if we
	         try to write too much and nothing is being pulled
	         from the pipe, winwrote will be set to 0. Also, if
		 we try to write too much at once, the result is a
		 ERROR_NOT_ENOUGH_MEMORY error. Account for these
	         behaviors by trying to write less each iteration when the
	         write fails. (Yuck.) */
	      while (1) {
		if (!fop->unblocked) {
		  ok = SetNamedPipeHandleState((HANDLE)fop->fd, &nonblock, NULL, NULL);
		  if (ok)
		    fop->unblocked = 1;
		  else
		    errsaved = GetLastError();
		} else
		  ok = 1;
		if (ok) {
		  ok = WriteFile((HANDLE)fop->fd, bufstr XFORM_OK_PLUS offset, towrite, &winwrote, NULL);
		  if (!ok)
		    errsaved = GetLastError();
		}

		if ((ok && !winwrote)
		    || (!ok && (errsaved == ERROR_NOT_ENOUGH_MEMORY))) {
		  towrite = towrite >> 1;
		  if (!towrite) {
		    break;
		  }
		} else
		  break;
	      }
	    } else {
	      /* Don't try to write while flushing. */
	      ok = 1;
	      winwrote = 0;
	    }

	    if (ok) {
	      if (!winwrote) {
		full_write_buffer = 1;
	      } else {
		len = winwrote;
	      }
	    }
	  } else
	    full_write_buffer = 0; /* and create the writer thread... */

	  if (!fop->oth) {
	    /* We create a thread even for pipes that can be put in
	       non-blocking mode, because that seems to be the only
	       way to get evt behavior. */
	    Win_FD_Output_Thread *oth;
	    HANDLE h;
	    DWORD id;
	    unsigned char *bfr;
	    OS_SEMAPHORE_TYPE sm;

	    oth = malloc(sizeof(Win_FD_Output_Thread));
	    fop->oth = oth;

	    oth->nonblocking = nonblocking;

	    if (!nonblocking) {
	      bfr = (unsigned char *)malloc(MZPORT_FD_BUFFSIZE);
	      oth->buffer = bfr;
	      oth->flushed = 0;
	      oth->needflush = 0;
	    } else {
	      oth->buffer = NULL;
	      oth->flushed = (len <= 0);
	      oth->needflush = 1;
	    }

	    oth->buflen = 0;
	    oth->bufstart = 0;
	    oth->bufend = 0;

	    oth->fd = (HANDLE)fop->fd;
	    oth->err_no = 0;
	    oth->done = 0;
	    sm = CreateSemaphore(NULL, 1, 1, NULL);
	    oth->lock_sema = sm;
	    sm = CreateSemaphore(NULL, 0, 1, NULL);
	    oth->work_sema = sm;
	    sm = CreateSemaphore(NULL, 1, 1, NULL);
	    oth->ready_sema = sm;
	    sm = CreateSemaphore(NULL, 1, 1, NULL);
	    oth->you_clean_up_sema = sm;
	    oth->refcount = fop->refcount;
            
	    h = CreateThread(NULL, 4096, (LPTHREAD_START_ROUTINE)WindowsFDWriter, oth, 0, &id);

	    scheme_remember_thread(h, 1);

	    /* scheme_remember_thread() is in charge of releasing h, so
	       duplicate it for use in closing: */
	    DuplicateHandle(GetCurrentProcess(), 
			    h,
			    GetCurrentProcess(),
			    &h, 
			    0,
			    FALSE,
			    DUPLICATE_SAME_ACCESS);

	    oth->thread = h;

	  }
	}

	/* We have a thread, if only to watch when the flush is
	   done... */

	if (!fop->oth->nonblocking) {
	  /* This case is for Win 95/98/Me anonymous pipes and
	     character devices.  We haven't written anything yet! We
	     write to a buffer read by the other thread, and return --
	     the other thread takes care of writing. Thus, as long as
	     there's room in the buffer, we don't block, and we can
	     tell whether there's room. Technical problem: if multiple
	     ports are attched to the same underlying pipe (different
	     handle, same "device"), the port writes can get out of
	     order. We try to avoid the problem by sleeping. */

	  Win_FD_Output_Thread *oth = fop->oth;

	  WaitForSingleObject(oth->lock_sema, INFINITE);
	  if (oth->err_no)
	    errsaved = oth->err_no;
	  else if (oth->buflen == MZPORT_FD_BUFFSIZE) {
	    full_write_buffer = 1;
	    WaitForSingleObject(oth->ready_sema, 0); /* clear any leftover state */
	  } else {
	    intptr_t topp;
	    int was_pre;

	    if (!oth->buflen) {
	      /* Avoid fragmenting in circular buffer: */
	      oth->bufstart = 0;
	      oth->bufend = 0;
	    }

	    /* Write to top part of circular buffer, then bottom part
	       if anything's left. */

	    if (oth->bufstart <= oth->bufend) {
	      was_pre = 1;
	      topp = MZPORT_FD_BUFFSIZE;
	    } else {
	      was_pre = 0;
	      topp = oth->bufstart;
	    }

	    winwrote = topp - oth->bufend;
	    if (winwrote > buflen - offset)
	      winwrote = buflen - offset;

	    memcpy(oth->buffer + oth->bufend, bufstr + offset, winwrote);
	    oth->buflen += winwrote;
	    len = winwrote;

	    oth->bufend += winwrote;
	    if (oth->bufend == MZPORT_FD_BUFFSIZE)
	      oth->bufend = 0;

	    if (was_pre) {
	      if (winwrote < buflen - offset) {
		/* Try continuing with a wrap-around: */
		winwrote = oth->bufstart - oth->bufend;
		if (winwrote > buflen - offset - len)
		  winwrote = buflen - offset - len;

		memcpy(oth->buffer + oth->bufend, bufstr + offset + len, winwrote);
		oth->buflen += winwrote;
		oth->bufend += winwrote;
		len += winwrote;
	      }
	    }
	    /* Let the other thread know that it should start trying
	       to write, if it isn't already: */
	    ReleaseSemaphore(oth->work_sema, 1, NULL);
	    Sleep(0); /* to decrease the chance of re-ordering flushes */
	  }
	  ReleaseSemaphore(oth->lock_sema, 1, NULL);
	} else if (len > 0) {
	  /* We've already written, which implies that no flush is
	     in progress. We'll need a flush check in the future. */
	  fop->oth->needflush = 1;
	}
      }
#else
      int flags;

      flags = fcntl(fop->fd, F_GETFL, 0);
      fcntl(fop->fd, F_SETFL, flags | MZ_NONBLOCKING);

      do {
	len = write(fop->fd, bufstr + offset, buflen - offset);
      } while ((len == -1) && (errno == EINTR));

      errsaved = errno;
      fcntl(fop->fd, F_SETFL, flags);

      full_write_buffer = (errsaved == EAGAIN);
#endif

      if (len < 0) {
	if (scheme_force_port_closed) {
	  /* Don't signal exn or wait. Just give up. */
	  return wrote;
	} else if (full_write_buffer) {
	  /* Need to block; remember that we're holding a lock. */
          Scheme_Object *sema;

	  if (immediate_only == 2) {
	    fop->flushing = 0;
	    return wrote;
	  }

#ifdef WINDOWS_FILE_HANDLES
          sema = NULL;
#else
          sema = scheme_fd_to_semaphore(fop->fd, MZFD_CREATE_WRITE, 0);
#endif

	  BEGIN_ESCAPEABLE(release_flushing_lock, fop);
          if (sema)
            scheme_wait_sema(sema, enable_break ? -1 : 0);
          else
            scheme_block_until_enable_break(fd_write_ready,
                                            fd_write_need_wakeup,
                                            (Scheme_Object *)op, 0.0,
                                            enable_break);
	  END_ESCAPEABLE();
	} else {
	  fop->flushing = 0;
	  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
			   "error writing to stream port\n"
                           "  system error: " FILENAME_EXN_E,
			   errsaved);
	  return 0; /* doesn't get here */
	}
      } else if ((len + offset == buflen) || immediate_only) {
	fop->flushing = 0;
	return wrote + len;
      } else {
	offset += len;
	wrote += len;
      }
    }
  }

  return wrote;
}

static intptr_t
fd_write_string(Scheme_Output_Port *port,
		const char *str, intptr_t d, intptr_t len,
		int rarely_block, int enable_break)
{
  /* Note: !flush => !rarely_block, !len => flush */

  Scheme_FD *fop;
  intptr_t l;
  int flush = (!len || rarely_block);

  fop = (Scheme_FD *)port->port_data;

  if (!len) {
    if (fop->bufcount)
      flush_fd(port, NULL, 0, 0, rarely_block, enable_break);

    if (fop->bufcount)
      return -1;
    else
      return 0;
  }

  if (!fop->bufcount && flush) {
    /* Nothing buffered. Write directly. */
    return flush_fd(port, str, d + len, d, rarely_block, enable_break);
  }

  if (fop->flushing) {
    if (rarely_block == 2)
      return -1; /* -1 means 0 written && still have unflushed */
    wait_until_fd_flushed(port, enable_break);
  }

  /* Might have been closed while we waited */
  if (port->closed)
    return 0;

  l = MZPORT_FD_BUFFSIZE - fop->bufcount;
  if ((len <= l) && (!flush || !rarely_block)) {
    memcpy(fop->buffer + fop->bufcount, str + d, len);
    fop->bufcount += len;
  } else {
    if (fop->bufcount) {
      flush_fd(port, NULL, 0, 0, (rarely_block == 2) ? 2 : 0, enable_break);
      if (rarely_block && fop->bufcount)
	return -1; /* -1 means 0 written && still have unflushed */
    }

    if (!flush && (len <= MZPORT_FD_BUFFSIZE)) {
      memcpy(fop->buffer, str + d, len);
      fop->bufcount = len;
    } else
      return flush_fd(port, str, len + d, d, rarely_block, enable_break);
  }

  /* If we got this far, !rarely_block. */

  if ((flush || (fop->flush == MZ_FLUSH_ALWAYS)) && fop->bufcount) {
    flush_fd(port, NULL, 0, 0, 0, enable_break);
  } else if (fop->flush == MZ_FLUSH_BY_LINE) {
    intptr_t i;

    for (i = len; i--; ) {
      if (str[d] == '\n' || str[d] == '\r') {
	flush_fd(port, NULL, 0, 0, 0, enable_break);
	break;
      }
      d++;
    }
  }

  return len;
}

static void
fd_close_output(Scheme_Output_Port *port)
{
  Scheme_FD *fop = (Scheme_FD *)port->port_data;

  if (fop->bufcount)
    flush_fd(port, NULL, 0, 0, 0, 0);

  if (fop->flushing && !scheme_force_port_closed)
    wait_until_fd_flushed(port, 0);

#ifdef WINDOWS_FILE_HANDLES
  if (fop->oth) {
    if (!scheme_force_port_closed) {
      /* If there's a work thread, wait until the port
	 is *really* flushed! */
      scheme_block_until(win_fd_flush_done, win_fd_flush_needs_wakeup, (Scheme_Object *)fop->oth, 0.0);
    }
  }
#endif

  /* Make sure no close happened while we blocked above! */
  if (port->closed)
    return;

#ifdef WINDOWS_FILE_HANDLES
  if (fop->oth) {
    CSI_proc csi;

    csi = get_csi();

    if (csi) {
      /* See also call to csi in fd_close_input */
      csi(fop->oth->thread);
    }
    CloseHandle(fop->oth->thread);
    fop->oth->done = 1;
    ReleaseSemaphore(fop->oth->work_sema, 1, NULL);

    /* Try to leave clean-up to the other thread: */
    if (WaitForSingleObject(fop->oth->you_clean_up_sema, 0) != WAIT_OBJECT_0) {
      /* Other thread is already done, so we're stuck with clean-up: */
      WindowsFDOCleanup(fop->oth);
    } /* otherwise, thread is responsible for clean-up */
    fop->oth = NULL;
  } else {
    int rc;
    rc = adj_refcount(fop->refcount, -1);
    if (!rc) {
      CloseHandle((HANDLE)fop->fd);
    }
  }
#else
 {
   int rc;
   rc = adj_refcount(fop->refcount, -1);

   if (!rc) {
     int cr;
     do {
       cr = close(fop->fd);
     } while ((cr == -1) && (errno == EINTR));
# ifdef USE_FCNTL_AND_FORK_FOR_FILE_LOCKS
     release_lockf(fop->fd);
# endif
     (void)scheme_fd_to_semaphore(fop->fd, MZFD_REMOVE, 0);
   }
 }
#endif
}

static void
fd_init_close_output(Scheme_Output_Port *port)
{
  /* never actually opened */
}

static int fd_output_buffer_mode(Scheme_Port *p, int mode)
{
  Scheme_FD *fd;
  Scheme_Output_Port *op = (Scheme_Output_Port *)p;

  fd = (Scheme_FD *)op->port_data;
  
  if (mode < 0) {
    return fd->flush;
  } else {
    int go;
    go = (mode > fd->flush);
    fd->flush = mode;
    if (go)
      flush_fd(op, NULL, 0, 0, 0, 0);
    return mode;
  }
}

static Scheme_Object *
make_fd_output_port(int fd, Scheme_Object *name, int regfile, int win_textmode, int and_read,
                    int flush_mode, int *refcount)
{
  Scheme_FD *fop;
  unsigned char *bfr;
  Scheme_Object *the_port;
  int start_closed = 0;

  fop = MALLOC_ONE_RT(Scheme_FD);
#ifdef MZTAG_REQUIRED
  fop->type = scheme_rt_input_fd;
#endif

  bfr = (unsigned char *)scheme_malloc_atomic(MZPORT_FD_BUFFSIZE);
  fop->buffer = bfr;

  fop->fd = fd;
  fop->bufcount = 0;

#ifdef WINDOWS_FILE_HANDLES
  /* Character devices can't block output, right? */
  if (is_fd_terminal(fop->fd))
    regfile = 1;
  /* The work thread is created on demand in fd_flush. */
#endif

  fop->regfile = regfile;
  fop->textmode = win_textmode;

  if (flush_mode > -1) {
    fop->flush = flush_mode;
  } else if (is_fd_terminal(fd)) {
    /* Line-buffering for terminal: */
    fop->flush = MZ_FLUSH_BY_LINE;
  } else {
    /* Block-buffering for everything else: */
    fop->flush = MZ_FLUSH_NEVER;
  }

  if (refcount) {
    fop->refcount = refcount;
    if (!adj_refcount(refcount, 1)) {
      /* fd is already closed! */
      start_closed = 1;
    }
  }

  the_port = (Scheme_Object *)scheme_make_output_port(fd_output_port_type,
						      fop,
						      name,
						      scheme_write_evt_via_write,
						      fd_write_string,
						      (Scheme_Out_Ready_Fun)fd_write_ready,
						      (start_closed
						       ? fd_init_close_output
						       : fd_close_output),
						      (Scheme_Need_Wakeup_Output_Fun)fd_write_need_wakeup,
						      NULL,
						      NULL,
						      1);
  ((Scheme_Port *)the_port)->buffer_mode_fun = fd_output_buffer_mode;

  if (start_closed)
    scheme_close_output_port(the_port);

  if (and_read) {
    int *rc;
    Scheme_Object *a[2];
    rc = malloc_refcount(1, 1);
    fop->refcount = rc;
    a[1] = the_port;
    a[0] = make_fd_input_port(fd, name, regfile, win_textmode, rc, 0);
    return scheme_values(2, a);
  } else
    return the_port;
}

static void flush_if_output_fds(Scheme_Object *o, Scheme_Close_Custodian_Client *f, void *data)
{
  if (SCHEME_OUTPUT_PORTP(o)) {
    Scheme_Output_Port *op;
    op = scheme_output_port_record(o);
    if (SAME_OBJ(op->sub_type, fd_output_port_type))
      scheme_flush_output(o);
  }
}

#ifdef WINDOWS_FILE_HANDLES

static long WINAPI WindowsFDWriter(Win_FD_Output_Thread *oth)
  XFORM_SKIP_PROC
{
  DWORD towrite, wrote, start;
  int ok, more_work = 0, err_no;

  if (oth->nonblocking) {
    /* Non-blocking mode (Win NT pipes). Just flush. */
    while (!oth->done) {
      WaitForSingleObject(oth->work_sema, INFINITE);

      FlushFileBuffers(oth->fd);

      WaitForSingleObject(oth->lock_sema, INFINITE);
      oth->flushed = 1;
      ReleaseSemaphore(oth->ready_sema, 1, NULL);
      ReleaseSemaphore(oth->lock_sema, 1, NULL);
    }
  } else {
    /* Blocking mode. We do the writing work.  This case is for
       Win 95/98/Me anonymous pipes and character devices (such 
       as LPT1). */
    while (!oth->err_no) {
      if (!more_work)
	WaitForSingleObject(oth->work_sema, INFINITE);

      if (oth->done)
	break;

      WaitForSingleObject(oth->lock_sema, INFINITE);
      towrite = oth->buflen;
      if (towrite > (MZPORT_FD_BUFFSIZE - oth->bufstart))
	towrite = MZPORT_FD_BUFFSIZE - oth->bufstart;
      start = oth->bufstart;
      ReleaseSemaphore(oth->lock_sema, 1, NULL);

      ok = WriteFile(oth->fd, oth->buffer + start, towrite, &wrote, NULL);
      if (!ok)
	err_no = GetLastError();
      else
	err_no = 0;

      WaitForSingleObject(oth->lock_sema, INFINITE);
      if (!ok)
	oth->err_no = err_no;
      else {
	oth->bufstart += wrote;
	oth->buflen -= wrote;
	if (oth->bufstart == MZPORT_FD_BUFFSIZE)
	  oth->bufstart = 0;
	more_work = oth->buflen > 0;
      }
      if ((oth->buflen < MZPORT_FD_BUFFSIZE) || oth->err_no)
	ReleaseSemaphore(oth->ready_sema, 1, NULL);
      ReleaseSemaphore(oth->lock_sema, 1, NULL);
    }
  }
  if (WaitForSingleObject(oth->you_clean_up_sema, 0) != WAIT_OBJECT_0) {
    WindowsFDOCleanup(oth);
  } /* otherwise, main thread is responsible for clean-up */

  return 0;
}

static void WindowsFDOCleanup(Win_FD_Output_Thread *oth)
  XFORM_SKIP_PROC
{
  int rc;

  CloseHandle(oth->lock_sema);
  CloseHandle(oth->work_sema);
  CloseHandle(oth->you_clean_up_sema);
  
  rc = adj_refcount(oth->refcount, -1);
  if (!rc) CloseHandle(oth->fd);

  if (oth->buffer)
    free(oth->buffer);
  free(oth);
}

#endif

#endif

Scheme_Object *
scheme_make_fd_output_port(int fd, Scheme_Object *name, int regfile, int textmode, int read_too)
{
#ifdef MZ_FDS
  return make_fd_output_port(fd, name, regfile, textmode, read_too, -1, NULL);
#else
  return NULL;
#endif
}

/*========================================================================*/
/*                        system/process/execute                          */
/*========================================================================*/

/* Unix, and Windows support --- all mixed together */

#define MZ_FAILURE_STATUS -1

#if defined(PROCESS_FUNCTION) || defined(MZ_USE_PLACES)

# define USE_CREATE_PIPE

#ifdef WINDOWS_PROCESSES
# ifdef USE_CREATE_PIPE
#  define _EXTRA_PIPE_ARGS
static int MyPipe(intptr_t *ph, int near_index) {
  HANDLE r, w;
  SECURITY_ATTRIBUTES saAttr;

  /* Set the bInheritHandle flag so pipe handles are inherited. */
  saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
  saAttr.bInheritHandle = TRUE;
  saAttr.lpSecurityDescriptor = NULL;

  if (CreatePipe(&r, &w, &saAttr, 0)) {
    HANDLE a[2], naya;

    a[0] = r;
    a[1] = w;

    if (near_index != -1) {
      /* Change the near end to make it non-inheritable, then
         close the inheritable one: */
      if (!DuplicateHandle(GetCurrentProcess(), a[near_index],
                           GetCurrentProcess(), &naya, 0,
                           0, /* not inherited */
                           DUPLICATE_SAME_ACCESS)) {
        CloseHandle(a[0]);
        CloseHandle(a[1]);
        return 1;
      } else {
        CloseHandle(a[near_index]);
        a[near_index] = naya;
      }
    }

    ph[0] = (long)a[0];
    ph[1] = (long)a[1];

    return 0;
  } else
    return 1;
}
#  define PIPE_FUNC MyPipe
#  define PIPE_HANDLE_t intptr_t
# else
#  include <Process.h>
#  include <fcntl.h>
#  define PIPE_FUNC(pa, nearh) MSC_IZE(pipe)(pa)
#  define PIPE_HANDLE_t int
#  define _EXTRA_PIPE_ARGS , 256, _O_BINARY
# endif
#else
# define _EXTRA_PIPE_ARGS
# define PIPE_FUNC(pa, nearh) MSC_IZE(pipe)(pa)
# define PIPE_HANDLE_t int
#endif

int scheme_os_pipe(intptr_t *a, int nearh)
/* If nearh != -1, then the handle at the index
   other than nearh is made inheritable so that
   a subprocess can use it. */
{
  PIPE_HANDLE_t la[2];

  if (PIPE_FUNC(la, nearh _EXTRA_PIPE_ARGS))
    return 1;
  a[0] = la[0];
  a[1] = la[1];
  return 0;
}

#endif

/**************** Unix: signal stuff ******************/

#if defined(UNIX_PROCESSES) && !defined(MZ_PLACES_WAITPID)

#ifndef MZ_PRECISE_GC
# define GC_write_barrier(x) /* empty */
#endif

/* See `unused_pid_statuses' in "places.c" for
   a reminder of why this is needed (in both 
   implementations): */
SHARED_OK static void *unused_pids;

static int need_to_check_children;

void scheme_block_child_signals(int block)
  XFORM_SKIP_PROC
{
#if !defined(MZ_PLACES_WAITPID)
  sigset_t sigs;

  sigemptyset(&sigs);
  sigaddset(&sigs, SIGCHLD);
# ifdef USE_ITIMER
  sigaddset(&sigs, SIGPROF);
# endif
  sigprocmask(block ? SIG_BLOCK : SIG_UNBLOCK, &sigs, NULL);
#endif
}

static void child_done(int ingored)
  XFORM_SKIP_PROC
{
  need_to_check_children = 1;
  scheme_signal_received();

# ifdef SIGSET_NEEDS_REINSTALL
  MZ_SIGSET(SIGCHLD, child_done);
# endif
}

static int sigchld_installed = 0;

static void init_sigchld(void)
{
#if !defined(MZ_PLACES_WAITPID)
  if (!sigchld_installed) {
    /* Catch child-done signals */
    START_XFORM_SKIP;
    MZ_SIGSET(SIGCHLD, child_done);
    END_XFORM_SKIP;

    sigchld_installed = 1;
  }
#endif
}

static void check_child_done(pid_t pid)
{
  pid_t result, check_pid;
  int status, is_unused;
  System_Child *sc, *prev;
  void **unused = (void **)unused_pids, **unused_prev = NULL;

  if (scheme_system_children) {
    do {
      if (!pid && unused) {
        check_pid = (pid_t)(intptr_t)unused[0];
        is_unused = 1;
      } else {
        check_pid = pid;
        is_unused = 0;
      }

      do {
        START_XFORM_SKIP;
        result = waitpid(check_pid, &status, WNOHANG);
        END_XFORM_SKIP;
      } while ((result == -1) && (errno == EINTR));

      if (result > 0) {
        if (is_unused) {
          /* done with an inaccessible group id */
          void *next;
          next = (void **)unused[1];
          if (unused_prev)
            unused_prev[1] = unused[1];
          else
            unused_pids = unused[1];
          free(unused);
          unused = (void **)next;
        }

        status = scheme_extract_child_status(status);

        prev = NULL;
        for (sc = scheme_system_children; sc; prev = sc, sc = sc->next) {
          if (sc->id == result) {
            sc->done = 1;
            sc->status = status;

            if (prev) {
              prev->next = sc->next;
            } else
              scheme_system_children = sc->next;
          }
        }
      } else {
        if (is_unused) {
          unused_prev = unused;
          unused = unused[1];
        }
      }
    } while ((result > 0) || is_unused);
  }
}

void scheme_check_child_done(void)
{
  if (need_to_check_children) {
    need_to_check_children = 0;
    check_child_done(0);
  }
}

#endif

#if defined(UNIX_PROCESSES)
int scheme_extract_child_status(int status) XFORM_SKIP_PROC
{
  if (WIFEXITED(status))
    status = WEXITSTATUS(status);
  else if (WIFSIGNALED(status))
    status = WTERMSIG(status) + 128;
  else
    status = MZ_FAILURE_STATUS;

  return status;
}
#endif

/*========================================================================*/
/*                           null output ports                            */
/*========================================================================*/

static intptr_t
null_write_bytes(Scheme_Output_Port *port,
		 const char *str, intptr_t d, intptr_t len,
		 int rarely_block, int enable_break)
{
  return len;
}

static void
null_close_out (Scheme_Output_Port *port)
{
}

static Scheme_Object *
null_write_evt(Scheme_Output_Port *op, const char *str, intptr_t offset, intptr_t size)
{
  Scheme_Object *a[2];
  a[0] = scheme_always_ready_evt;
  a[1] = scheme_make_closed_prim(return_data, scheme_make_integer(size));
  return scheme_wrap_evt(2, a);
}

static Scheme_Object *
null_write_special_evt(Scheme_Output_Port *op, Scheme_Object *v)
{
  Scheme_Object *a[2];
  a[0] = scheme_always_ready_evt;
  a[1] = scheme_make_closed_prim(return_data, scheme_true);
  return scheme_wrap_evt(2, a);
}

static int 
null_write_special(Scheme_Output_Port *op, Scheme_Object *v, int nonblock)
{
  return 1;
}

Scheme_Object *
scheme_make_null_output_port(int can_write_special)
{
  Scheme_Output_Port *op;

  op = scheme_make_output_port(scheme_null_output_port_type,
			       NULL,
			       scheme_intern_symbol("null"),
			       null_write_evt,
			       null_write_bytes,
			       NULL,
			       null_close_out,
			       NULL,
			       (can_write_special
				? null_write_special_evt
				: NULL),
			       (can_write_special
				? null_write_special
				: NULL),
			       0);

  return (Scheme_Object *)op;
}

/*========================================================================*/
/*                         redirect output ports                          */
/*========================================================================*/

static Scheme_Object *redirect_write_bytes_k(void);

static intptr_t
redirect_write_bytes(Scheme_Output_Port *op,
		     const char *str, intptr_t d, intptr_t len,
		     int rarely_block, int enable_break)
{
  /* arbitrary nesting means we can overflow the stack */
#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Thread *p = scheme_current_thread;
    Scheme_Object *n;

    p->ku.k.p1 = (void *)op;
    p->ku.k.p2 = (void *)str;
    p->ku.k.i1 = d;
    p->ku.k.i2 = len;
    p->ku.k.i3 = rarely_block;
    p->ku.k.i4 = enable_break;

    n = scheme_handle_stack_overflow(redirect_write_bytes_k);
    return SCHEME_INT_VAL(n);
  }
#endif

  return scheme_put_byte_string("redirect-output",
				(Scheme_Object *)op->port_data,
				str, d, len,
				rarely_block);
}

static Scheme_Object *redirect_write_bytes_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Output_Port *op = (Scheme_Output_Port *)p->ku.k.p1;
  const char *str = (const char *)p->ku.k.p2;
  intptr_t d = p->ku.k.i1;
  intptr_t len = p->ku.k.i2;
  int rarely_block = p->ku.k.i3;
  int enable_break = p->ku.k.i4;
  intptr_t n;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  n = redirect_write_bytes(op, str, d, len, rarely_block, enable_break);

  return scheme_make_integer(n);
}

static void
redirect_close_out (Scheme_Output_Port *port)
{
}

static Scheme_Object *
redirect_write_evt(Scheme_Output_Port *op, const char *str, intptr_t offset, intptr_t size)
{
  return scheme_make_write_evt("redirect-write-evt", 
			       (Scheme_Object *)op->port_data,
			       NULL, (char *)str, offset, size);
}

static Scheme_Object *
redirect_write_special_evt(Scheme_Output_Port *op, Scheme_Object *special)
{
  return scheme_make_write_evt("redirect-write-evt", 
			       (Scheme_Object *)op->port_data,
			       special, NULL, 0, 0);
}

static int 
redirect_write_special(Scheme_Output_Port *op, Scheme_Object *special, int nonblock)
{
  Scheme_Object *v, *a[2];

  a[0] = (Scheme_Object *)op->port_data;
  a[1] = special;

  if (nonblock)
    v = scheme_write_special(2, a);
  else
    v = scheme_write_special(2, a);
  
  return SCHEME_TRUEP(v);
}

Scheme_Object *
scheme_make_redirect_output_port(Scheme_Object *port)
{
  Scheme_Output_Port *op;
  int can_write_special;

  op = scheme_output_port_record(port);
  can_write_special = !!op->write_special_fun;

  op = scheme_make_output_port(scheme_redirect_output_port_type,
			       port,
			       scheme_intern_symbol("redirect"),
			       redirect_write_evt,
			       redirect_write_bytes,
			       NULL,
			       redirect_close_out,
			       NULL,
			       (can_write_special
				? redirect_write_special_evt
				: NULL),
			       (can_write_special
				? redirect_write_special
				: NULL),
			       0);

  return (Scheme_Object *)op;
}

/*********** Unix/Windows: process status stuff *************/

#if defined(UNIX_PROCESSES) || defined(WINDOWS_PROCESSES)

static void child_mref_done(Scheme_Subprocess *sp)
{
  if (sp->mref) {
    scheme_remove_managed(sp->mref, (Scheme_Object *)sp);
    sp->mref = NULL;
  }
}

static int subp_done(Scheme_Object *so)
{
  Scheme_Subprocess *sp;
  sp = (Scheme_Subprocess*) so;

#if defined(UNIX_PROCESSES)
# if defined(MZ_PLACES_WAITPID)
  {
    int status;
    if (!sp->done) {
      if (scheme_get_child_status(sp->pid, sp->is_group, &status)) {
        sp->done = 1;
        sp->status = status;
        child_mref_done(sp);
        return 1;
      }
      return 0;
    }
    else
      return 1;
  }
# else
  {
    System_Child *sc;
    sc = (System_Child *)sp->handle;
    /* Check specific pid, in case the child has its own group
       (either given by Racket or given to itself): */
    check_child_done(sp->pid);
    if (sc->done)
      child_mref_done(sp);
    return sc->done;
  }
# endif
#endif
#ifdef WINDOWS_PROCESSES
  {
    HANDLE sci = (HANDLE) ((Scheme_Subprocess *)sp)->handle;
    DWORD w;
    if (sci) {
      if (GetExitCodeProcess(sci, &w))
        return w != STILL_ACTIVE;
      else
        return 1;
    } else
      return 1;
  }
#endif
}

static void subp_needs_wakeup(Scheme_Object *sp, void *fds)
{
#ifdef WINDOWS_PROCESSES
  void *sci = ((Scheme_Subprocess *)sp)->handle;
  scheme_add_fd_handle((void *)(HANDLE)sci, fds, 0);
#endif
}

#endif

static Scheme_Object *subprocess_status(int argc, Scheme_Object **argv)
{
  Scheme_Subprocess *sp = (Scheme_Subprocess *)argv[0];

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_subprocess_type))
    scheme_wrong_contract("subprocess-status", "subprocess?", 0, argc, argv);

#if defined(PROCESS_FUNCTION) && !defined(MAC_CLASSIC_PROCESS_CONTROL)
  {
    int going = 0, status = MZ_FAILURE_STATUS;

#if defined(UNIX_PROCESSES)
# if defined(MZ_PLACES_WAITPID)
  if (sp->done)
    status = sp->status;
  else {
    if (!scheme_get_child_status(sp->pid, sp->is_group, &status)) {
      going = 1;
    } else {
      child_mref_done(sp);
      sp->done = 1;
      sp->status = status;
    }
  }
# else
  System_Child *sc = (System_Child *)sp->handle;
  check_child_done(sp->pid);

  if (sc->done) {
    child_mref_done(sp);
    status = sc->status;
  } else
   going = 1;
# endif
#else
# ifdef WINDOWS_PROCESSES
    DWORD w;
    if (sp->handle) {
      if (GetExitCodeProcess((HANDLE)sp->handle, &w)) {
	if (w == STILL_ACTIVE)
	  going = 1;
	else
	  status = w;
      }
    }
# endif
#endif

    if (going)
      return scheme_intern_symbol("running");
    else
      return scheme_make_integer_value(status);
  }
#else
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		   "%s: " NOT_SUPPORTED_STR,
		   "subprocess-status");
#endif
}


static void register_subprocess_wait()
{
#if defined(UNIX_PROCESSES) || defined(WINDOWS_PROCESSES)
  scheme_add_evt(scheme_subprocess_type, subp_done,
		  subp_needs_wakeup, NULL, 0);
#endif
}

static Scheme_Object *subprocess_wait(int argc, Scheme_Object **argv)
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_subprocess_type))
    scheme_wrong_contract("subprocess-wait", "subprocess?", 0, argc, argv);

#if defined(UNIX_PROCESSES) || defined(WINDOWS_PROCESSES)
  {
    Scheme_Subprocess *sp = (Scheme_Subprocess *)argv[0];

    scheme_block_until(subp_done, subp_needs_wakeup, (Scheme_Object *)sp, (float)0.0);

    return scheme_void;
  }
#else
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
                 "%s: " NOT_SUPPORTED_STR,
                 "subprocess-wait");
#endif
}

#if defined(UNIX_PROCESSES) || defined(WINDOWS_PROCESSES)
static Scheme_Object *do_subprocess_kill(Scheme_Object *_sp, Scheme_Object *killp, int can_error)
{
  Scheme_Subprocess *sp = (Scheme_Subprocess *)_sp;

#if defined(UNIX_PROCESSES)
# if defined(MZ_PLACES_WAITPID)
  {
    int status;

    if (sp->done)
      return scheme_void;

    scheme_wait_suspend();

    /* Don't pass sp->is_group, because we don't want to wait
       on a group if we haven't already: */
    if (scheme_get_child_status(sp->pid, 0, &status)) {
      sp->status = status;
      sp->done = 1;
      child_mref_done(sp);
      scheme_wait_resume();
      return scheme_void;
    }
  }
# else
  {
    System_Child *sc = (System_Child *)sp->handle;

    /* Don't pass sp->pid, because we don't want to wait
       on a group if we haven't already: */
    check_child_done(0);
    if (sc->done) {
      child_mref_done(sp);
      return scheme_void;
    }
  }
# define scheme_wait_resume() /* empty */
# endif

  while (1) {

    if (sp->is_group) {
      if (!killpg(sp->pid, SCHEME_TRUEP(killp) ? SIGKILL : SIGINT)) {
        scheme_wait_resume();
        return scheme_void;
      }
    } else {
      if (!kill(sp->pid, SCHEME_TRUEP(killp) ? SIGKILL : SIGINT)) {
        scheme_wait_resume();
        return scheme_void;
      }
    }
    
    if (errno != EINTR)
      break;
    /* Otherwise we were interrupted. Try `kill' again. */
  }

  scheme_wait_resume();

#else
  if (SCHEME_TRUEP(killp) || sp->is_group) {
    DWORD w;
    int errid;

    if (!sp->handle)
      return scheme_void;

    if (SCHEME_FALSEP(killp)) {
      /* must be for a group; we don't care whether the
         original process is still running */
      if (GenerateConsoleCtrlEvent(CTRL_BREAK_EVENT, sp->pid))
        return scheme_void;
    } else if (GetExitCodeProcess((HANDLE)sp->handle, &w)) {
      if (w != STILL_ACTIVE)
        return scheme_void;
      if (TerminateProcess((HANDLE)sp->handle, 1))
        return scheme_void;
    }
    errid = GetLastError();
    errno = errid;
  } else
    return scheme_void;
#endif

  if (can_error)
    scheme_raise_exn(MZEXN_FAIL, 
                     "subprocess-kill: operation failed\n"
                     "  system error: %E", errno);

  return NULL;
}

static void kill_subproc(Scheme_Object *o, void *data)
{
  (void)do_subprocess_kill(o, scheme_true, 0);
}

static void interrupt_subproc(Scheme_Object *o, void *data)
{
  (void)do_subprocess_kill(o, scheme_true, 0);
}
#endif

static Scheme_Object *subprocess_kill(int argc, Scheme_Object **argv)
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_subprocess_type))
    scheme_wrong_contract("subprocess-kill", "subprocess?", 0, argc, argv);

#if defined(UNIX_PROCESSES) || defined(WINDOWS_PROCESSES)
  return do_subprocess_kill(argv[0], argv[1], 1);
#else
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		   "%s: " NOT_SUPPORTED_STR,
		   "subprocess-wait");
  return NULL;
#endif
}

static Scheme_Object *subprocess_pid(int argc, Scheme_Object **argv)
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_subprocess_type))
    scheme_wrong_contract("subprocess-pid", "subprocess?", 0, argc, argv);

  return scheme_make_integer_value(((Scheme_Subprocess *)argv[0])->pid);
}

static Scheme_Object *subprocess_p(int argc, Scheme_Object **argv)
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_subprocess_type)
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *subproc_cust_mode_p(int argc, Scheme_Object **argv)
{
  if (SCHEME_FALSEP(argv[0]))
    return argv[0];
  if (SCHEME_SYMBOLP(argv[0]) && !SCHEME_SYM_WEIRDP(argv[0])) {
    if (!strcmp(SCHEME_SYM_VAL(argv[0]), "kill")
        || !strcmp(SCHEME_SYM_VAL(argv[0]), "interrupt"))
      return argv[0];
  }
    
  return NULL;
}

static Scheme_Object *current_subproc_cust_mode (int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-subprocess-custodian-mode", scheme_make_integer(MZCONFIG_SUBPROC_CUSTODIAN_MODE),
			     argc, argv,
			     -1, subproc_cust_mode_p, "'interrupt, 'kill, or #f", 0);
}

static Scheme_Object *subproc_group_on (int argc, Scheme_Object *argv[])
{
  return scheme_param_config("subprocess-group-enabled", scheme_make_integer(MZCONFIG_SUBPROC_GROUP_ENABLED), 
                             argc, argv, 
                             -1, NULL, NULL, 1);
}

#ifdef UNIX_PROCESSES
static void unused_process_record(void *_sp, void *ignored)
{
  Scheme_Subprocess *sp = (Scheme_Subprocess *)_sp;

# if defined(MZ_PLACES_WAITPID)
  if (!sp->done)
    scheme_done_with_process_id(sp->pid, sp->is_group);
# else
  if (!((System_Child *)sp->handle)->done) {
    void **unused_pid;
    unused_pid = malloc(sizeof(void *) * 2);
    unused_pid[0] = (void *)(intptr_t)sp->pid;
    unused_pid[1] = unused_pids;
    need_to_check_children = 1;
  }
# endif
}
#endif

/*********** Windows: command-line construction *************/

#ifdef WINDOWS_PROCESSES
static char *cmdline_protect(char *s)
{
  char *naya;
  int ds;
  int has_space = 0, has_quote = 0, was_slash = 0;

  for (ds = 0; s[ds]; ds++) {
    if (isspace(s[ds]) || (s[ds] == '\'')) {
      has_space = 1;
      was_slash = 0;
    } else if (s[ds] == '"') {
      has_quote += 1 + (2 * was_slash);
      was_slash = 0;
    } else if (s[ds] == '\\') {
      was_slash++;
    } else
      was_slash = 0;
  }

  if (has_space || has_quote) {
    char *p;
    int wrote_slash = 0;

    naya = scheme_malloc_atomic(strlen(s) + 3 + 3*has_quote + was_slash);
    naya[0] = '"';
    for (p = naya + 1; *s; s++) {
      if (*s == '"') {
	while (wrote_slash--) {
	  *(p++) = '\\';
	}
	*(p++) = '"'; /* endquote */
	*(p++) = '\\';
	*(p++) = '"'; /* protected */
	*(p++) = '"'; /* start quote again */
	wrote_slash = 0;
      } else if (*s == '\\') {
	*(p++) = '\\';
	wrote_slash++;
      } else {
	*(p++) = *s;
	wrote_slash = 0;
      }
    }
    while (wrote_slash--) {
      *(p++) = '\\';
    }
    *(p++) = '"';
    *p = 0;

    return naya;
  }

  return s;
}

static intptr_t mz_spawnv(char *command, const char * const *argv,
			  int exact_cmdline, intptr_t sin, intptr_t sout, intptr_t serr, int *pid,
			  int new_process_group)
{
  int i, l, len = 0;
  intptr_t cr_flag;
  char *cmdline;
  STARTUPINFOW startup;
  PROCESS_INFORMATION info;

  if (exact_cmdline) {
    cmdline = (char *)argv[1];
  } else {
    for (i = 0; argv[i]; i++) {
      len += strlen(argv[i]) + 1;
    }

    cmdline = (char *)scheme_malloc_atomic(len);

    len = 0;
    for (i = 0; argv[i]; i++) {
      l = strlen(argv[i]);
      memcpy(cmdline + len, argv[i], l);
      cmdline[len + l] = ' ';
      len += l + 1;
    }
    --len;
    cmdline[len] = 0;
  }

  memset(&startup, 0, sizeof(startup));
  startup.cb = sizeof(startup);
  startup.dwFlags = STARTF_USESTDHANDLES;
  startup.hStdInput = (HANDLE)sin;
  startup.hStdOutput = (HANDLE)sout;
  startup.hStdError = (HANDLE)serr;

  /* If none of the stdio handles are consoles, specifically
     create the subprocess without a console: */
  if (!is_fd_terminal((int)startup.hStdInput)
      && !is_fd_terminal((int)startup.hStdOutput)
      && !is_fd_terminal((int)startup.hStdError))
    cr_flag = CREATE_NO_WINDOW;
  else
    cr_flag = 0;
  if (new_process_group)
    cr_flag |= CREATE_NEW_PROCESS_GROUP;

  if (CreateProcessW(WIDE_PATH_COPY(command), WIDE_PATH_COPY(cmdline), 
		     NULL, NULL, 1 /*inherit*/,
		     cr_flag, NULL, NULL,
		     &startup, &info)) {
    CloseHandle(info.hThread);
    *pid = info.dwProcessId;
    return (intptr_t)info.hProcess;
  } else
    return -1;
}

static void close_subprocess_handle(void *sp, void *ignored)
{
  Scheme_Subprocess *subproc = (Scheme_Subprocess *)sp;

  CloseHandle(subproc->handle);
}

static void CopyFileHandleForSubprocess(intptr_t *hs, int pos)
{
  HANDLE h2;
  int alt_pos = (pos ? 0 : 1);

  if (DuplicateHandle(GetCurrentProcess(),
		      (HANDLE)hs[pos],
		      GetCurrentProcess(),
		      &h2,
		      0,
		      TRUE,
		      DUPLICATE_SAME_ACCESS)) {
    hs[pos] = (int)h2;
    hs[alt_pos] = 1;
  } else {
    hs[alt_pos] = 0;
  }
}

static void CloseFileHandleForSubprocess(intptr_t *hs, int pos)
{
  int alt_pos = (pos ? 0 : 1);
  if (hs[alt_pos]) {
    CloseHandle((HANDLE)hs[pos]);
  }
}

#define mzCOPY_FILE_HANDLE(array, pos) CopyFileHandleForSubprocess(array, pos)
#define mzCLOSE_FILE_HANDLE(array, pos) CloseFileHandleForSubprocess(array, pos)

#endif /* WINDOWS_PROCESSES */

#ifndef mzCOPY_FILE_HANDLE
# define mzCOPY_FILE_HANDLE(array, pos) /* empty */
# define mzCLOSE_FILE_HANDLE(array, pos) /* empty */
#endif

/*********** All: The main system/process/execute function *************/

static Scheme_Object *subprocess(int c, Scheme_Object *args[])
     /* subprocess(out, in, err, exe, arg ...) */
{
  const char *name = "subprocess";
#if defined(PROCESS_FUNCTION) && !defined(MAC_CLASSIC_PROCESS_CONTROL)
  char *command;
  intptr_t to_subprocess[2], from_subprocess[2], err_subprocess[2];
  int i, pid, errid;
  char **argv;
  Scheme_Object *in, *out, *err;
#if defined(UNIX_PROCESSES)
# if !defined(MZ_PLACES_WAITPID)
  System_Child *sc;
# endif
  int fork_errno = 0;
#else
  void *sc = 0;
#endif
  Scheme_Object *inport;
  Scheme_Object *outport;
  Scheme_Object *errport;
  int stderr_is_stdout = 0;
  Scheme_Object *a[4];
  Scheme_Subprocess *subproc;
  Scheme_Object *cust_mode;
  int new_process_group;
#if defined(WINDOWS_PROCESSES)
  int exact_cmdline = 0;
#endif
#if defined(WINDOWS_PROCESSES)
  int spawn_status;
#endif

  /*--------------------------------------------*/
  /* Sort out ports (create later if necessary) */
  /*--------------------------------------------*/

  if (SCHEME_TRUEP(args[0])) {
    outport = args[0];
    if (SCHEME_OUTPUT_PORTP(outport) && SCHEME_TRUEP(scheme_file_stream_port_p(1, &outport))) {
#ifdef PROCESS_FUNCTION
      Scheme_Output_Port *op;

      op = scheme_output_port_record(outport);

      if (SAME_OBJ(op->sub_type, file_output_port_type)) {
	int tmp;
	tmp = MSC_IZE(fileno)(((Scheme_Output_File *)op->port_data)->f);
	from_subprocess[1] = tmp;
      }
# ifdef MZ_FDS
      else if (SAME_OBJ(op->sub_type, fd_output_port_type))
	from_subprocess[1] = ((Scheme_FD *)op->port_data)->fd;
# endif
      mzCOPY_FILE_HANDLE(from_subprocess, 1);
#endif
    } else
      scheme_wrong_contract(name, "(or/c (and/c file-stream-port? output-port?) #f)", 0, c, args);
  } else
    outport = NULL;

  if (SCHEME_TRUEP(args[1])) {
    inport = args[1];
    if (SCHEME_INPUT_PORTP(inport) && SCHEME_TRUEP(scheme_file_stream_port_p(1, &inport))) {
#ifdef PROCESS_FUNCTION
      Scheme_Input_Port *ip;

      ip = scheme_input_port_record(inport);

      if (SAME_OBJ(ip->sub_type, file_input_port_type)) {
	int tmp;
	tmp = MSC_IZE(fileno)(((Scheme_Input_File *)ip->port_data)->f);
	to_subprocess[0] = tmp;
      }
# ifdef MZ_FDS
      else if (SAME_OBJ(ip->sub_type, fd_input_port_type))
	to_subprocess[0] = ((Scheme_FD *)ip->port_data)->fd;
# endif
      mzCOPY_FILE_HANDLE(to_subprocess, 0);
#endif
    } else
      scheme_wrong_contract(name, "(or/c (and/c file-stream-port? input-port?) #f)", 1, c, args);
  } else
    inport = NULL;

  if (SCHEME_SYMBOLP(args[2]) && !SCHEME_SYM_WEIRDP(args[2])
      && !strcmp("stdout", SCHEME_SYM_VAL(args[2]))) {
    errport = NULL;
    stderr_is_stdout = 1;
  } else if (SCHEME_TRUEP(args[2])) {
    errport = args[2];
    if (SCHEME_OUTPUT_PORTP(errport) && SCHEME_TRUEP(scheme_file_stream_port_p(1, &errport))) {
#ifdef PROCESS_FUNCTION
      Scheme_Output_Port *op;

      op = scheme_output_port_record(errport);

      if (SAME_OBJ(op->sub_type, file_output_port_type)) {
	int tmp;
	tmp = MSC_IZE(fileno)(((Scheme_Output_File *)op->port_data)->f);
	err_subprocess[1] = tmp;
      }
# ifdef MZ_FDS
      else if (SAME_OBJ(op->sub_type, fd_output_port_type))
	err_subprocess[1] = ((Scheme_FD *)op->port_data)->fd;
# endif
      mzCOPY_FILE_HANDLE(err_subprocess, 1);
#endif
    } else
      scheme_wrong_contract(name, "(or/c (and/c file-stream-port? output-port?) #f 'stdout)", 2, c, args);
  } else
    errport = NULL;

  if (!SCHEME_PATH_STRINGP(args[3]))
    scheme_wrong_contract(name, "path-string?", 3, c, args);

  /*--------------------------------------*/
  /*          Sort out arguments          */
  /*--------------------------------------*/

  argv = MALLOC_N(char *, c - 3 + 1);
  {
    char *ef;
    ef = scheme_expand_string_filename(args[3],
				       (char *)name, 
				       NULL,
				       SCHEME_GUARD_FILE_EXECUTE);
    argv[0] = ef;
  }
  {
    /* This is for Windows: */
    char *np;
    int nplen;
    nplen = strlen(argv[0]);
    np = scheme_normal_path_seps(argv[0], &nplen, 0);
    argv[0] = np;
  }

  if ((c == 6) && SAME_OBJ(args[4], exact_symbol)) {
    argv[2] = NULL;
    if (!SCHEME_CHAR_STRINGP(args[5]) || scheme_any_string_has_null(args[5]))
      scheme_wrong_contract(name, CHAR_STRING_W_NO_NULLS, 5, c, args);
    {
      Scheme_Object *bs;
      bs = scheme_char_string_to_byte_string(args[5]);
      argv[1] = SCHEME_BYTE_STR_VAL(bs);
    }
#ifdef WINDOWS_PROCESSES
    exact_cmdline = 1;
#else
    /* 'exact-full only works in windows */
    scheme_contract_error(name,
                          "exact command line not supported on this platform",
                          "exact command", 1, args[5],
                          NULL);
#endif
  } else {
    for (i = 4; i < c; i++) {
      if (((!SCHEME_CHAR_STRINGP(args[i]) && !SCHEME_BYTE_STRINGP(args[i]))
           || scheme_any_string_has_null(args[i]))
          && !SCHEME_PATHP(args[i]))
	scheme_wrong_contract(name, 
                              ("(or/c path?\n"
                               "      (and/c string? (lambda (s) (not (memv #\\nul (string->list s)))))\n"
                               "      (and/c bytes? (lambda (bs) (not (memv 0 (bytes->list bs))))))"),
                              i, c, args);
        
      {
	Scheme_Object *bs;
        bs = args[i];
        if (SCHEME_CHAR_STRINGP(args[i]))
          bs = scheme_char_string_to_byte_string_locale(bs);
	argv[i - 3] = SCHEME_BYTE_STR_VAL(bs);
      }
    }
    argv[c - 3] = NULL;
  }

  command = argv[0];

  if (!inport || !outport || !errport)
    scheme_custodian_check_available(NULL, name, "file-stream");

  cust_mode = scheme_get_param(scheme_current_config(), MZCONFIG_SUBPROC_GROUP_ENABLED);
  new_process_group = SCHEME_TRUEP(cust_mode);
  cust_mode = scheme_get_param(scheme_current_config(), MZCONFIG_SUBPROC_CUSTODIAN_MODE);

  /*--------------------------------------*/
  /*          Create needed pipes         */
  /*--------------------------------------*/

  if (!inport && scheme_os_pipe(to_subprocess, 1)) {
    errid = scheme_errno();
    if (outport) { mzCLOSE_FILE_HANDLE(from_subprocess, 1); }
    if (errport) { mzCLOSE_FILE_HANDLE(err_subprocess, 1); }
    scheme_system_error(name, "pipe", errid);
  }
  if (!outport && scheme_os_pipe(from_subprocess, 0)) {
    errid = scheme_errno();
    if (!inport) {
      scheme_close_file_fd(to_subprocess[0]);
      scheme_close_file_fd(to_subprocess[1]);
    } else {
      mzCLOSE_FILE_HANDLE(to_subprocess, 0);
    }
    if (errport) { mzCLOSE_FILE_HANDLE(err_subprocess, 1); }
    scheme_system_error(name, "pipe", errid);
  }
  if (!errport && stderr_is_stdout) {
    err_subprocess[0] = from_subprocess[0];
    err_subprocess[1] = from_subprocess[1];
  } else if (!errport && scheme_os_pipe(err_subprocess, 0)) {
    errid = scheme_errno();
    if (!inport) {
      scheme_close_file_fd(to_subprocess[0]);
      scheme_close_file_fd(to_subprocess[1]);
    } else {
      mzCLOSE_FILE_HANDLE(to_subprocess, 0);
    }
    if (!outport) {
      scheme_close_file_fd(from_subprocess[0]);
      scheme_close_file_fd(from_subprocess[1]);
    } else {
      mzCLOSE_FILE_HANDLE(from_subprocess, 1);
    }
    scheme_system_error(name, "pipe", errid);
  }

#if defined(WINDOWS_PROCESSES)

  /*--------------------------------------*/
  /*        Execute: Windows              */
  /*--------------------------------------*/

  /* Windows: quasi-stdin is locked, and we'll say it doesn't matter */
  fflush(stdin);
  fflush(stdout);
  fflush(stderr);

  {
    Scheme_Object *tcd;

    if (!exact_cmdline) {
      /* protect spaces, etc. in the arguments: */
      for (i = 0; i < (c - 3); i++) {
	char *cla;
	cla = cmdline_protect(argv[i]);
	argv[i] = cla;
      }
    }

    /* Set real CWD before spawn: */
    tcd = scheme_get_param(scheme_current_config(), MZCONFIG_CURRENT_DIRECTORY);
    scheme_os_setcwd(SCHEME_BYTE_STR_VAL(tcd), 0);

    spawn_status = mz_spawnv(command, (const char * const *)argv,
			     exact_cmdline,
			     to_subprocess[0],
			     from_subprocess[1],
			     err_subprocess[1],
			     &pid,
                             new_process_group);

    if (spawn_status != -1)
      sc = (void *)spawn_status;
  }

#else


  /*--------------------------------------*/
  /*            Execute: Unix             */
  /*--------------------------------------*/

  {
#if !defined(MZ_PLACES_WAITPID)
    init_sigchld();

    sc = MALLOC_ONE_RT(System_Child);
#ifdef MZTAG_REQUIRED
    sc->type = scheme_rt_system_child;
#endif
    sc->id = 0;
    sc->done = 0;

    scheme_block_child_signals(1);
#endif

    pid = fork();

    if (pid > 0) {
      if (new_process_group)
        /* there's a race condition between this use and the exec(),
           and there's a race condition between the other setpgid() in
           the child processand sending signals from the parent
           process; so, we set in both, and at least one will
           succeed; we could perform better error checking, since
           EACCES is the only expected error */
        setpgid(pid, pid);

#if defined(MZ_PLACES_WAITPID)
      {
        int *signal_fd;
        int status;
        signal_fd = scheme_get_signal_handle();
        scheme_places_register_child(pid, new_process_group, signal_fd, &status);

        /* printf("SUBPROCESS  %i\n", pid); */
      }
#else
      sc->next = scheme_system_children;
      scheme_system_children = sc;
      sc->id = pid;
#endif
    } else if (!pid) {
#ifdef USE_ITIMER
      /* Turn off the timer. */
      /* SIGPROF is masked at this point due to
	 block_child_signals() */
      struct itimerval t, old;
      sigset_t sigs;

      t.it_value.tv_sec = 0;
      t.it_value.tv_usec = 0;
      t.it_interval.tv_sec = 0;
      t.it_interval.tv_usec = 0;

      setitimer(ITIMER_PROF, &t, &old);

      /* Clear already-queued PROF signal, if any: */
      START_XFORM_SKIP;
      sigemptyset(&sigs);
      while (!sigpending(&sigs)) {
	if (sigismember(&sigs, SIGPROF)) {
	  sigprocmask(SIG_SETMASK, NULL, &sigs);
	  sigdelset(&sigs, SIGPROF);
	  sigsuspend(&sigs);
	  sigemptyset(&sigs);
	} else
	  break;
      }
      END_XFORM_SKIP;
#endif
      if (new_process_group)
        /* see also setpgid above */
        setpgid(getpid(), getpid()); /* setpgid(0, 0) would work on some platforms */
    } else {
      fork_errno = errno;
    }

#if !defined(MZ_PLACES_WAITPID)
    scheme_block_child_signals(0);
#else
    if (!pid)
      scheme_places_unblock_child_signal();
#endif
  }

  switch (pid)
    {
    case -1:
      /* Close unused descriptors. */
      if (!inport) {
	scheme_close_file_fd(to_subprocess[0]);
	scheme_close_file_fd(to_subprocess[1]);
      } else {
	mzCLOSE_FILE_HANDLE(to_subprocess, 0);
      }
      if (!outport) {
	scheme_close_file_fd(from_subprocess[0]);
	scheme_close_file_fd(from_subprocess[1]);
      } else {
	mzCLOSE_FILE_HANDLE(from_subprocess, 1);
      }
      if (!errport) {
        if (!stderr_is_stdout) {
          scheme_close_file_fd(err_subprocess[0]);
          scheme_close_file_fd(err_subprocess[1]);
        }
      } else {
	mzCLOSE_FILE_HANDLE(err_subprocess, 1);
      }
      scheme_system_error(name, "fork", fork_errno);
      return scheme_false;

    case 0: /* child */

      {
	/* Copy pipe descriptors to stdin and stdout */
	do {
	  errid = MSC_IZE(dup2)(to_subprocess[0], 0);
	} while (errid == -1 && errno == EINTR);
	do {
	  errid = MSC_IZE(dup2)(from_subprocess[1], 1);
	} while (errid == -1 && errno == EINTR);
	do {
	  errid = MSC_IZE(dup2)(err_subprocess[1], 2);
	} while (errid == -1 && errno == EINTR);

	/* Close unwanted descriptors. */
	if (!inport) {
	  scheme_close_file_fd(to_subprocess[0]);
	  scheme_close_file_fd(to_subprocess[1]);
	}
	if (!outport) {
	  scheme_close_file_fd(from_subprocess[0]);
	  scheme_close_file_fd(from_subprocess[1]);
	}
	if (!errport) {
          if (!stderr_is_stdout) {
            scheme_close_file_fd(err_subprocess[0]);
            scheme_close_file_fd(err_subprocess[1]);
          }
	}

#ifdef CLOSE_ALL_FDS_AFTER_FORK
        close_fds_after_fork(0, 1, 2);
#endif
      }

      /* Set real CWD */
      {
	Scheme_Object *dir;
	dir = scheme_get_param(scheme_current_config(), MZCONFIG_CURRENT_DIRECTORY);
	if (!scheme_os_setcwd(SCHEME_PATH_VAL(dir), 1)) {
          scheme_console_printf("racket: chdir failed to: %s\n", SCHEME_BYTE_STR_VAL(dir));
          _exit(1);
        }
      }

      /* Exec new process */

      {
	int err;

	/* Reset ignored signals: */
	START_XFORM_SKIP;
#ifndef DONT_IGNORE_FPE_SIGNAL
	MZ_SIGSET(SIGFPE, SIG_DFL);
#endif
#ifndef DONT_IGNORE_PIPE_SIGNAL
	MZ_SIGSET(SIGPIPE, SIG_DFL);
#endif
	END_XFORM_SKIP;

	err = MSC_IZE(execv)(command, argv);
        if (err)
          err = errno;

	/* If we get here it failed; give up */

        /* using scheme_signal_error will leave us in the forked process,
	   so use scheme_console_printf instead */
        scheme_console_printf("racket: exec failed (%s%serrno=%d)\n", 
#ifdef NO_STRERROR_AVAILABLE
                              "", "",
#else
                              strerror(err), "; ",
#endif
                              err);

	/* back to Racket signal dispositions: */
	START_XFORM_SKIP;
#ifndef DONT_IGNORE_FPE_SIGNAL
	MZ_SIGSET(SIGFPE, SIG_IGN);
#endif
#ifndef DONT_IGNORE_PIPE_SIGNAL
	MZ_SIGSET(SIGPIPE, SIG_IGN);
#endif
	END_XFORM_SKIP;

	_exit(1);
      }

    default: /* parent */

      break;
    }
#endif

  /*--------------------------------------*/
  /*      Close unneeded descriptors      */
  /*--------------------------------------*/

  if (!inport) {
    scheme_close_file_fd(to_subprocess[0]);
    out = NULL;
  } else {
    mzCLOSE_FILE_HANDLE(to_subprocess, 0);
    out = scheme_false;
  }
  if (!outport) {
    scheme_close_file_fd(from_subprocess[1]);
    in = NULL;
  } else {
    mzCLOSE_FILE_HANDLE(from_subprocess, 1);
    in = scheme_false;
  }
  if (!errport) {
    if (!stderr_is_stdout)
      scheme_close_file_fd(err_subprocess[1]);
    err = NULL;
  } else {
    mzCLOSE_FILE_HANDLE(err_subprocess, 1);
    err = scheme_false;
  }

  /*--------------------------------------*/
  /*        Create new port objects       */
  /*--------------------------------------*/

  in = (in ? in : make_fd_input_port(from_subprocess[0], scheme_intern_symbol("subprocess-stdout"), 0, 0, NULL, 0));
  out = (out ? out : make_fd_output_port(to_subprocess[1], scheme_intern_symbol("subprocess-stdin"), 0, 0, 0, -1, NULL));
  if (stderr_is_stdout)
    err = scheme_false;
  else
    err = (err ? err : make_fd_input_port(err_subprocess[0], scheme_intern_symbol("subprocess-stderr"), 0, 0, NULL, 0));

  /*--------------------------------------*/
  /*          Return result info          */
  /*--------------------------------------*/

  subproc = MALLOC_ONE_TAGGED(Scheme_Subprocess);
  subproc->so.type = scheme_subprocess_type;
#if !defined(MZ_PLACES_WAITPID)
  subproc->handle = (void *)sc;
#endif
  subproc->pid = pid;
  subproc->is_group = new_process_group;
# if defined(WINDOWS_PROCESSES)
  scheme_add_finalizer(subproc, close_subprocess_handle, NULL);
# else
  scheme_add_finalizer(subproc, unused_process_record, NULL);
# endif

  if (SCHEME_TRUEP(cust_mode)) {
    Scheme_Custodian_Reference *mref;
    Scheme_Close_Custodian_Client *closer;

    if (!strcmp(SCHEME_SYM_VAL(cust_mode), "kill"))
      closer = kill_subproc;
    else
      closer = interrupt_subproc;

    mref = scheme_add_managed(NULL, (Scheme_Object *)subproc, closer, NULL, 1);
    subproc->mref = mref;
  }

#define cons scheme_make_pair

  a[0] = (Scheme_Object *)subproc;
  a[1] = in;
  a[2] = out;
  a[3] = err;

  return scheme_values(4, a);

#else
# ifdef MAC_CLASSIC_PROCESS_CONTROL

  /*--------------------------------------*/
  /*            Macintosh hacks           */
  /*--------------------------------------*/

  {
    int i;
    Scheme_Object *a[4], *appname;
    Scheme_Subprocess *subproc;

    for (i = 0; i < 3; i++) {
      if (!SCHEME_FALSEP(args[i]))
	scheme_contract_error(name,
                              "non-#f port argument not allowed on this platform",
                              "port", 1, args[i],
                              NULL));
    }

    if (c > 4) {
      if (c == 5) {
	Scheme_Object *bs;
	if (!SCHEME_PATH_STRINGP(args[3]))
	  scheme_wrong_contract(name, "path-string?", 3, c, args);
	if (SCHEME_PATHP(args[3]))
	  bs = args[3];
	else
	  bs = scheme_char_string_to_path(args[3]);
	if (strcmp(SCHEME_PATH_VAL(bs), "by-id"))
	  scheme_contract_error(name,
                                "in five-argument mode on this platform, the 4th argument must be \"by-id\"",
                                "given", 1, args[3],
                                NULL);

	appname = args[4];
	i = scheme_mac_start_app((char *)name, 1, appname);
      } else
	scheme_contract_error(name,
                              "extra arguments after the application id are "
                              "not allowed on this platform",
                              "first extra argument", 1, args[5],
                              NULL);
    } else {
      appname = args[3];
      i = scheme_mac_start_app((char *)name, 0, appname);
    }

    if (!i) {
      scheme_raise_exn(MZEXN_FAIL, 
                       "%s: launch failed\n"
                       "  application: %Q", 
                       name, appname);
      return NULL;
    }

    subproc = MALLOC_ONE_TAGGED(Scheme_Subprocess);
    subproc->type = scheme_subprocess_type;

    a[0] = (Scheme_Object *)subproc;
    a[1] = scheme_false;
    a[2] = scheme_false;
    a[3] = scheme_false;

    return scheme_values(4, a);
  }

# else
  /*--------------------------------------*/
  /*  Subprocess functionality disabled   */
  /*--------------------------------------*/

  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		   "%s: " NOT_SUPPORTED_STR,
		   name);
  return NULL;
# endif
#endif
}

#ifdef CLOSE_ALL_FDS_AFTER_FORK
static void close_fds_after_fork(int skip1, int skip2, int skip3)
{
  int i;

# ifdef USE_ULIMIT
  i = ulimit(4, 0);
# else
  i = getdtablesize();
# endif
  while (i--) {
    int cr;
    if ((i != skip1) && (i != skip2) && (i != skip3)) {
      do {
        cr = close(i);
      } while ((cr == -1) && (errno == EINTR));
    }
  }
}
#endif


static Scheme_Object *sch_shell_execute(int c, Scheme_Object *argv[])
{
#ifdef WINDOWS_PROCESSES
  char *dir;
  int show = 0;
# define mzseSHOW(s, x) s = x
#else
# define mzseSHOW(s, x) /* empty */
#endif

  if (!SCHEME_FALSEP(argv[0]) && !SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract("shell-execute", "(or/c string? #f)", 0, c, argv);
  if (!SCHEME_CHAR_STRINGP(argv[1]))
    scheme_wrong_contract("shell-execute", "string?", 1, c, argv);
  if (!SCHEME_CHAR_STRINGP(argv[2]))
    scheme_wrong_contract("shell-execute", "string?", 2, c, argv);
  if (!SCHEME_PATH_STRINGP(argv[3]))
    scheme_wrong_contract("shell-execute", "path-string?", 3, c, argv);
  {
    int show_set = 0;
# define mzseCMP(id, str)			       \
    if (SAME_OBJ(scheme_intern_symbol(str), argv[4])   \
        || SAME_OBJ(scheme_intern_symbol(# id), argv[4])) { \
      mzseSHOW(show, id); show_set = 1; }
    mzseCMP(SW_HIDE, "sw_hide");
    mzseCMP(SW_MAXIMIZE, "sw_maximize");
    mzseCMP(SW_MINIMIZE, "sw_minimize");
    mzseCMP(SW_RESTORE, "sw_restore");
    mzseCMP(SW_SHOW, "sw_show");
    mzseCMP(SW_SHOWDEFAULT, "sw_showdefault");
    mzseCMP(SW_SHOWMAXIMIZED, "sw_showmaximized");
    mzseCMP(SW_SHOWMINIMIZED, "sw_showminimized");
    mzseCMP(SW_SHOWMINNOACTIVE, "sw_showminnoactive");
    mzseCMP(SW_SHOWNA, "sw_showna");
    mzseCMP(SW_SHOWNOACTIVATE, "sw_shownoactivate");
    mzseCMP(SW_SHOWNORMAL, "sw_shownormal");

    if (!show_set)
      scheme_wrong_type("shell-execute", "show-mode symbol", 4, c, argv);
  }

#ifdef WINDOWS_PROCESSES
  dir = 
#endif
    scheme_expand_string_filename(argv[3],
                                  "shell-execute", NULL,
                                  SCHEME_GUARD_FILE_EXISTS);
#ifdef WINDOWS_PROCESSES
  {
    SHELLEXECUTEINFOW se;
    int nplen;
    Scheme_Object *sv, *sf, *sp;

    nplen = strlen(dir);
    dir = scheme_normal_path_seps(dir, &nplen, 0);

    if (SCHEME_FALSEP(argv[0]))
      sv = scheme_false;
    else
      sv = scheme_char_string_to_byte_string(argv[0]);
    sf = scheme_char_string_to_byte_string(argv[1]);
    sp = scheme_char_string_to_byte_string(argv[2]);

    memset(&se, 0, sizeof(se));
    se.fMask = SEE_MASK_NOCLOSEPROCESS | SEE_MASK_FLAG_DDEWAIT;
    se.cbSize = sizeof(se);
    if (SCHEME_FALSEP(sv))
      se.lpVerb = NULL;
    else {
      se.lpVerb = WIDE_PATH_COPY(SCHEME_BYTE_STR_VAL(sv));
    }
    se.lpFile = WIDE_PATH_COPY(SCHEME_BYTE_STR_VAL(sf));
    se.lpParameters = WIDE_PATH_COPY(SCHEME_BYTE_STR_VAL(sp));
    se.lpDirectory = WIDE_PATH_COPY(dir);
    se.nShow = show;
    se.hwnd = NULL;

    /* Used to use ShellExecuteEx(&se) here. Not sure why it doesn't work,
       and the problem was intermittent (e.g., worked for opening a URL
       with IE as the default browser, but failed with Netscape). */
    if (ShellExecuteW(se.hwnd, se.lpVerb, se.lpFile, se.lpParameters, se.lpDirectory, se.nShow)) {
      if (se.hProcess) {
	Scheme_Subprocess *subproc;

	subproc = MALLOC_ONE_TAGGED(Scheme_Subprocess);

	subproc->so.type = scheme_subprocess_type;
	subproc->handle = (void *)se.hProcess;
	subproc->pid = 0;
	scheme_add_finalizer(subproc, close_subprocess_handle, NULL);

	return (Scheme_Object *)subproc;
      } else
	return scheme_false;
    } else {
      scheme_signal_error("shell-execute: execute failed\n"
                          "  command: %V\n"
                          "  system error: %E",
			  argv[1],
			  GetLastError());
      return NULL;
    }
  }
#else
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		   "shell-execute: " NOT_SUPPORTED_STR);
  return NULL;
#endif
}

/*========================================================================*/
/*                          fd reservation                                */
/*========================================================================*/

/* We don't want on-demand loading of code to fail because we run out of
   file descriptors. So, keep one in reserve. */

void scheme_reserve_file_descriptor(void)
{
#ifdef USE_FD_PORTS
  if (!fd_reserved) {
    the_fd = open("/dev/null", O_RDONLY); 
    if (the_fd != -1)
      fd_reserved = 1;
  }
#endif
}

void scheme_release_file_descriptor(void)
{
#ifdef USE_FD_PORTS
  if (fd_reserved) {
    close(the_fd);
    fd_reserved = 0;
  }
#endif
}


/*========================================================================*/
/*                             sleeping                                   */
/*========================================================================*/

/* This code is used to implement sleeping when Racket is completely
   blocked on external objects, such as ports. For Unix, sleeping is
   essentially just a select(). */

/****************** Windows cleanup  *****************/

#if defined(WIN32_FD_HANDLES)

static void clean_up_wait(intptr_t result, OS_SEMAPHORE_TYPE *array,
			  int *rps, int count)
{
  if ((result >= (intptr_t)WAIT_OBJECT_0) && (result < (intptr_t)WAIT_OBJECT_0 + count)) {
    result -= WAIT_OBJECT_0;
    if (rps[result])
      ReleaseSemaphore(array[result], 1, NULL);
  }

  /* Clear out break semaphore */
  WaitForSingleObject((HANDLE)scheme_break_semaphore, 0);
}

static int made_progress;
static DWORD max_sleep_time;

void scheme_notify_sleep_progress()
{
  made_progress = 1;
}

#else

void scheme_notify_sleep_progress()
{
}

#endif

/******************** Main sleep function  *****************/
/* The simple select() stuff is buried in Windows complexity. */

static void clear_signal()
  XFORM_SKIP_PROC
{
#if defined(FILES_HAVE_FDS)
  /* Clear external event flag */
  if (external_event_fd) {
    int rc;
    char buf[10];
    do {
      rc = read(external_event_fd, buf, 10);
    } while ((rc == -1) && errno == EINTR);
  }
#endif
}

static void default_sleep(float v, void *fds)
#ifdef OS_X
  XFORM_SKIP_PROC
#endif
/* This sleep function is not allowed to allocate in OS X, because it
   is called in a non-main thread. */
{
  /* REMEMBER: don't allocate in this function (at least not GCable
     memory) for OS X. Not that FD setups are ok, because they use
     eternal mallocs. */

#ifdef USE_OSKIT_CONSOLE
  /* Don't really sleep; keep polling the keyboard: */
  if (!v || (v > 0.01))
    v = 0.01;
#endif

  if (!fds) {
    /* Nothing to block on - just sleep for some amount of time. */
#if defined(FILES_HAVE_FDS)
# ifdef HAVE_POLL_SYSCALL
    int timeout;
    if (v <= 0.0)
      timeout = -1;
    else {
      timeout = (int)(v * 1000.0);
      if (timeout < 0) 
        timeout = 0;
    }
    if (external_event_fd) {
      GC_CAN_IGNORE struct pollfd pfd[1];
      pfd[0].fd = external_event_fd;
      pfd[0].events = POLLIN;
      poll(pfd, 1, timeout);
    } else {
      poll(NULL, 0, timeout);
    }
# else
    /* Sleep by selecting on the external event fd */
    struct timeval time;
    intptr_t secs = (intptr_t)v;
    intptr_t usecs = (intptr_t)(fmod(v, 1.0) * 1000000);

    if (v && (v > 100000))
      secs = 100000;
    if (usecs < 0)
      usecs = 0;
    if (usecs >= 1000000)
      usecs = 999999;

    time.tv_sec = secs;
    time.tv_usec = usecs;

    if (external_event_fd) {
      DECL_FDSET(readfds, 1);

      INIT_DECL_RD_FDSET(readfds);

      MZ_FD_ZERO(readfds);
      MZ_FD_SET(external_event_fd, readfds);

      select(external_event_fd + 1, readfds, NULL, NULL, &time);
    } else {
      select(0, NULL, NULL, NULL, &time);
    }
# endif
#else
# ifndef NO_SLEEP
#  ifndef NO_USLEEP
   usleep((unsigned)(v * 1000));
#   else
   sleep(v);
#  endif
# endif
#endif
  } else {
    /* Something to block on - sort our the parts in Windows. */

#if defined(FILES_HAVE_FDS) || defined(USE_WINSOCK_TCP)
# ifndef HAVE_POLL_SYSCALL
    int actual_limit;
    fd_set *rd, *wr, *ex;
    struct timeval time;
# endif

# ifdef SIGCHILD_DOESNT_INTERRUPT_SELECT
    if (scheme_system_children) {
      /* Better poll every second or so... */
      if (!v || (v > 1))
	v = 1;
    }
# endif

# ifndef HAVE_POLL_SYSCALL
    {
      intptr_t secs = (intptr_t)v;
      intptr_t usecs = (intptr_t)(fmod(v, 1.0) * 1000000);

      if (v && (v > 100000))
	secs = 100000;
      if (usecs < 0)
	usecs = 0;
      if (usecs >= 1000000)
	usecs = 999999;

      time.tv_sec = secs;
      time.tv_usec = usecs;
    }

    rd = (fd_set *)fds;
    wr = (fd_set *)MZ_GET_FDSET(fds, 1);
    ex = (fd_set *)MZ_GET_FDSET(fds, 2);

    actual_limit = scheme_get_fd_limit(fds);
# endif

    /******* Start Windows stuff *******/

#if defined(WIN32_FD_HANDLES)
    {
      intptr_t result;
      OS_SEMAPHORE_TYPE *array, just_two_array[2], break_sema;
      int count, rcount, *rps;

      if (((win_extended_fd_set *)rd)->no_sleep)
	return;

      scheme_collapse_win_fd(fds); /* merges */

      rcount = SCHEME_INT_VAL(((win_extended_fd_set *)fds)->num_handles);
      count = SCHEME_INT_VAL(((win_extended_fd_set *)fds)->combined_len);
      array = ((win_extended_fd_set *)fds)->combined_wait_array;
      rps = ((win_extended_fd_set *)fds)->repost_sema;

      /* add break semaphore: */
      if (!count) {
	array = just_two_array;
      }
      break_sema = (HANDLE)scheme_break_semaphore;
      array[count++] = break_sema;

      /* Extensions may handle events.
	 If the event queue is empty (as reported by GetQueueStatus),
	 everything's ok.

	 Otherwise, we have trouble sleeping until an event is ready. We
	 sometimes leave events on th queue because, say, an eventspace is
	 not ready. The problem is that MsgWait... only unblocks when a new
	 event appears. Since extensions may check the queue using a sequence of
	 PeekMessages, it's possible that an event is added during the
	 middle of the sequence, but doesn't get handled.

	 To avoid this problem, we don't actually sleep indefinitely if an event
	 is pending. Instead, we slep 10 ms, then 20 ms, etc. This exponential 
	 backoff ensures that we eventually handle a pending event, but we don't 
	 spin and eat CPU cycles. The back-off is reset whenever a thread makes
	 progress. */


      if (SCHEME_INT_VAL(((win_extended_fd_set *)fds)->wait_event_mask)
	  && GetQueueStatus(SCHEME_INT_VAL(((win_extended_fd_set *)fds)->wait_event_mask))) {
	if (!made_progress) {
	  /* Ok, we've gone around at least once. */
	  if (max_sleep_time < 0x20000000)
	    max_sleep_time *= 2;
	} else {
	  /* Starting back-off mode */
	  made_progress = 0;
	  max_sleep_time = 5;
	}
      } else {
	/* Disable back-off mode */
	made_progress = 1;
	max_sleep_time = 0;
      }

      /* Wait for HANDLE-based input: */
      {
	DWORD msec;
	if (v) {
	  if (v > 100000)
	    msec = 100000000;
	  else
	    msec = (DWORD)(v * 1000);
	  if (max_sleep_time && (msec > max_sleep_time))
	    msec = max_sleep_time;
	} else {
	  if (max_sleep_time)
	    msec = max_sleep_time;
	  else
	    msec = INFINITE;
	}

	result = MsgWaitForMultipleObjects(count, array, FALSE, msec,
					   SCHEME_INT_VAL(((win_extended_fd_set *)fds)->wait_event_mask));
      }
      clean_up_wait(result, array, rps, rcount);
      scheme_collapse_win_fd(fds); /* cleans up */

      return;
    }
#endif

#ifdef USE_WINSOCK_TCP
    /* Stupid Windows: give select() empty fd_sets and it ignores the timeout. */
    if (!rd->fd_count && !wr->fd_count && !ex->fd_count) {
      if (v)
	Sleep((DWORD)(v * 1000));
      return;
    }
#endif

    /******* End Windows stuff *******/

# ifdef HAVE_POLL_SYSCALL
    {
      struct mz_fd_set_data *data = ((struct mz_fd_set *)fds)->data;
      intptr_t count = SCHEME_INT_VAL(data->count);
      int timeout;

      if (v <= 0.0)
        timeout = -1;
      else {
        timeout = (int)(v * 1000.0);
        if (timeout < 0) 
          timeout = 0;
      }
      
      if (external_event_fd) {
        data->pfd[count].fd = external_event_fd;
        data->pfd[count].events = POLLIN;
        count++;
      }

      poll(data->pfd, count, timeout);
    }
#else
# if defined(FILES_HAVE_FDS)
    /* Watch for external events, too: */
    if (external_event_fd) {
      MZ_FD_SET(external_event_fd, rd);
      if (external_event_fd >= actual_limit)
        actual_limit = external_event_fd + 1;
    }
# endif

    select(actual_limit, rd, wr, ex, v ? &time : NULL);
#endif

#endif
  }

  clear_signal();
}

void scheme_signal_received_at(void *h)
  XFORM_SKIP_PROC
/* Ensure that Racket wakes up if asleep. */
{
#if defined(FILES_HAVE_FDS)
  int put_ext_event_fd = *(int *)h;
  if (put_ext_event_fd) {
    int v;
    do {
      v = write(put_ext_event_fd, "!", 1);
    } while ((v == -1) && (errno == EINTR));
  }
#endif
#if defined(WINDOWS_PROCESSES) || defined(WINDOWS_FILE_HANDLES)
  ReleaseSemaphore(*(OS_SEMAPHORE_TYPE *)h, 1, NULL);
#endif
}

void *scheme_get_signal_handle()
  XFORM_SKIP_PROC
{
#if defined(FILES_HAVE_FDS)
  return &put_external_event_fd;
#else
# if defined(WINDOWS_PROCESSES) || defined(WINDOWS_FILE_HANDLES)
  return &scheme_break_semaphore;
# else
  return NULL;
# endif
#endif
}

void scheme_signal_received(void)
  XFORM_SKIP_PROC
{
  scheme_signal_received_at(scheme_get_signal_handle());
}

void scheme_wait_until_signal_received(void)
  XFORM_SKIP_PROC
{
#if defined(FILES_HAVE_FDS)
  int r;
# ifdef HAVE_POLL_SYSCALL
  GC_CAN_IGNORE struct pollfd pfd[1];
  pfd[0].fd = external_event_fd;
  pfd[0].events = POLLIN;
  do {
    r = poll(pfd, 1, -1);
  } while ((r == -1) && (errno == EINTR));
# else
  DECL_FDSET(readfds, 1);
  
  INIT_DECL_RD_FDSET(readfds);
  
  MZ_FD_ZERO(readfds);
  MZ_FD_SET(external_event_fd, readfds);
  
  do {
    r = select(external_event_fd + 1, readfds, NULL, NULL, NULL);
  } while ((r == -1) && (errno == EINTR));
# endif
#else
# if defined(WINDOWS_PROCESSES) || defined(WINDOWS_FILE_HANDLES)
  WaitForSingleObject((HANDLE)scheme_break_semaphore, 0);
# endif
#endif
  
  clear_signal();
}

int scheme_get_external_event_fd(void)
{
#if defined(FILES_HAVE_FDS)
  return external_event_fd;
#else
  return 0;
#endif
}

#ifdef USE_WIN32_THREAD_TIMER

typedef struct ITimer_Data {
  int done;
  HANDLE itimer;
  intptr_t delay;
  OS_SEMAPHORE_TYPE semaphore;
  OS_SEMAPHORE_TYPE done_semaphore;
  int volatile *fuel_counter_ptr;
  uintptr_t volatile *jit_stack_boundary_ptr;
} ITimer_Data;

THREAD_LOCAL_DECL(static ITimer_Data *itimerdata);

static long WINAPI ITimer(void *data)
  XFORM_SKIP_PROC
{
  ITimer_Data *d = (ITimer_Data *)data;

  WaitForSingleObject(d->semaphore, INFINITE);

  while (!d->done) {
    if (WaitForSingleObject(d->semaphore, d->delay / 1000) == WAIT_TIMEOUT) {
      *d->fuel_counter_ptr = 0;
      *d->jit_stack_boundary_ptr = (uintptr_t)-1;
      if (!d->done)
	WaitForSingleObject(d->semaphore, INFINITE);
    }
  }

  ReleaseSemaphore(d->done_semaphore, 1, NULL);

  return 0;
}

static void scheme_start_itimer_thread(intptr_t usec)
{
  DWORD id;

  if (!itimerdata) {
    ITimer_Data *d;
    HANDLE itimer, sema;

    d = malloc(sizeof(ITimer_Data));
    memset(d, 0, sizeof(ITimer_Data));

    d->fuel_counter_ptr = &scheme_fuel_counter;
    d->jit_stack_boundary_ptr = &scheme_jit_stack_boundary;

    sema = CreateSemaphore(NULL, 0, 1, NULL);
    d->semaphore = sema;
    sema = CreateSemaphore(NULL, 0, 1, NULL);
    d->done_semaphore = sema;

    itimer = CreateThread(NULL, 4096, (LPTHREAD_START_ROUTINE)ITimer, 
			  d, 0, &id);
    scheme_remember_thread(itimer, 0);
    d->itimer = itimer;

    itimerdata = d;
  }

  itimerdata->delay = usec;
  ReleaseSemaphore(itimerdata->semaphore, 1, NULL);
}

static void scheme_stop_itimer_thread()
{
  ITimer_Data *d = itimerdata;

  scheme_forget_thread(d->itimer);

  d->done = 1;
  ReleaseSemaphore(d->semaphore, 1, NULL);

  WaitForSingleObject(d->done_semaphore, INFINITE);

  CloseHandle(d->semaphore);
  CloseHandle(d->done_semaphore);
  CloseHandle(d->itimer);

  free(d);
}

#endif

#ifdef USE_PTHREAD_THREAD_TIMER

#include <pthread.h>
typedef struct ITimer_Data {
  int itimer;
  int state;
  int die;
  mz_proc_thread *thread;
  pthread_mutex_t mutex;
  pthread_cond_t cond;
  int delay;
  volatile int * fuel_counter_ptr;
  volatile uintptr_t * jit_stack_boundary_ptr;
} ITimer_Data;

THREAD_LOCAL_DECL(static ITimer_Data *itimerdata);

static void *green_thread_timer(void *data)
  XFORM_SKIP_PROC
{
  ITimer_Data *itimer_data;
  itimer_data = (ITimer_Data *)data;

  while (1) {
    if (itimer_data->die)
      return NULL;
    usleep(itimer_data->delay);
    *(itimer_data->fuel_counter_ptr) = 0;
    *(itimer_data->jit_stack_boundary_ptr) = (uintptr_t)-1;

    pthread_mutex_lock(&itimer_data->mutex);
    if (!itimer_data->die) {
      if (itimer_data->state) {
        itimer_data->state = 0;
      } else {
        itimer_data->state = -1;
        pthread_cond_wait(&itimer_data->cond, &itimer_data->mutex);
      }
    }
    pthread_mutex_unlock(&itimer_data->mutex);
  }

  return NULL;
}

static void start_green_thread_timer(intptr_t usec) 
{
  mz_proc_thread *tmp;
  itimerdata->die = 0;
  itimerdata->delay = usec;
  itimerdata->fuel_counter_ptr = &scheme_fuel_counter;
  itimerdata->jit_stack_boundary_ptr = &scheme_jit_stack_boundary;
  pthread_mutex_init(&itimerdata->mutex, NULL);
  pthread_cond_init(&itimerdata->cond, NULL);
  tmp = mz_proc_thread_create_w_stacksize(green_thread_timer, itimerdata, 4096);
  itimerdata->thread = tmp;
  itimerdata->itimer = 1;
}

static void kill_green_thread_timer() 
{
  pthread_mutex_lock(&itimerdata->mutex);
  itimerdata->die = 1;
  if (!itimerdata->state) {
    /* itimer thread is currently running working */
  } else if (itimerdata->state < 0) {
    /* itimer thread is waiting on cond */
    pthread_cond_signal(&itimerdata->cond);
  } else {
    /* itimer thread is working, and we've already
       asked it to continue */
  }
  pthread_mutex_unlock(&itimerdata->mutex);
  (void)mz_proc_thread_wait(itimerdata->thread);
  free(itimerdata);
  itimerdata = NULL;
}

static void kickoff_green_thread_timer(intptr_t usec) 
{
  pthread_mutex_lock(&itimerdata->mutex);
  itimerdata->delay = usec;
  if (!itimerdata->state) {
    /* itimer thread is currently running working */
    itimerdata->state = 1;
  } else if (itimerdata->state < 0) {
    /* itimer thread is waiting on cond */
    itimerdata->state = 0;
    pthread_cond_signal(&itimerdata->cond);
  } else {
    /* itimer thread is working, and we've already
       asked it to continue */
  }
  pthread_mutex_unlock(&itimerdata->mutex);
}

static void scheme_start_itimer_thread(intptr_t usec)
{
  if (!itimerdata) {
    itimerdata = (ITimer_Data *)malloc(sizeof(ITimer_Data));
    memset(itimerdata, 0, sizeof(ITimer_Data));
  }

  if (!itimerdata->itimer) {
    start_green_thread_timer(usec);
  } else {
    kickoff_green_thread_timer(usec);
  }
}

#endif

#ifdef USE_ITIMER

static void itimer_expired(int ignored)
  XFORM_SKIP_PROC
{
  scheme_fuel_counter = 0;
  scheme_jit_stack_boundary = (uintptr_t)-1;
#  ifdef SIGSET_NEEDS_REINSTALL
  MZ_SIGSET(SIGPROF, itimer_expired);
#  endif
}

static void kickoff_itimer(intptr_t usec) 
  XFORM_SKIP_PROC
{
  struct itimerval t;
  struct itimerval old;
  static int itimer_handler_installed = 0;

  if (!itimer_handler_installed) {
    itimer_handler_installed = 1;
    MZ_SIGSET(SIGPROF, itimer_expired);
  }

  t.it_value.tv_sec = 0;
  t.it_value.tv_usec = usec;
  t.it_interval.tv_sec = 0;
  t.it_interval.tv_usec = 0;

  setitimer(ITIMER_PROF, &t, &old);
}

#endif

void scheme_kickoff_green_thread_time_slice_timer(intptr_t usec) {
#ifdef USE_ITIMER
  kickoff_itimer(usec);
#elif defined(USE_WIN32_THREAD_TIMER)
  scheme_start_itimer_thread(usec);
#elif defined(USE_PTHREAD_THREAD_TIMER)
  scheme_start_itimer_thread(usec);
#endif
}

void scheme_kill_green_thread_timer()
{
#if defined(USE_PTHREAD_THREAD_TIMER)
  kill_green_thread_timer();
#elif defined(USE_WIN32_THREAD_TIMER)
  scheme_stop_itimer_thread();
#endif

#if defined(FILES_HAVE_FDS)
# ifndef USE_OSKIT_CONSOLE
  close(external_event_fd);
  close(put_external_event_fd);
# endif
#endif
#ifdef WIN32_FD_HANDLES
  CloseHandle((HANDLE)scheme_break_semaphore);
#endif
}

#ifdef OS_X

/* Sleep-in-thread support needed for GUIs Mac OS X.
   To merge waiting on a CoreFoundation event with a select(), an embedding
   application can attach a single socket to an event callback, and then
   create a Mac thread to call the usual sleep and write to the socket when
   data is available. */

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

typedef struct {
  pthread_mutex_t lock;
  pthread_cond_t cond;
  int count;
} pt_sema_t;

void pt_sema_init(pt_sema_t *sem)
{
  pthread_mutex_init(&sem->lock, NULL);
  pthread_cond_init(&sem->cond, NULL);
  sem->count = 0;
}

void pt_sema_wait(pt_sema_t *sem)
{
  pthread_mutex_lock(&sem->lock);
  while (sem->count <= 0)
    pthread_cond_wait(&sem->cond, &sem->lock);
  sem->count--;
  pthread_mutex_unlock(&sem->lock);
}

void pt_sema_post(pt_sema_t *sem)
{
  pthread_mutex_lock(&sem->lock);
  sem->count++;
  if (sem->count > 0)
    pthread_cond_signal(&sem->cond);
  pthread_mutex_unlock(&sem->lock);
}

static pthread_t watcher;
static pt_sema_t sleeping_sema, done_sema;
static float sleep_secs;
static int slept_fd;
static void *sleep_fds;
static void (*sleep_sleep)(float seconds, void *fds);

static void *do_watch(void *other)
{
  scheme_init_os_thread_like(other);
  while (1) {
    pt_sema_wait(&sleeping_sema);

    sleep_sleep(sleep_secs, sleep_fds);
    write(slept_fd, "y", 1);

    pt_sema_post(&done_sema);
  }
  return NULL;
}

void scheme_start_sleeper_thread(void (*given_sleep)(float seconds, void *fds), float secs, void *fds, int hit_fd)
{
  if (!watcher) {
    pt_sema_init(&sleeping_sema);
    pt_sema_init(&done_sema);

    if (pthread_create(&watcher, NULL, do_watch, scheme_get_os_thread_like())) {
      scheme_log_abort("pthread_create failed");
      abort();
    }
  }

  sleep_sleep = given_sleep;
  sleep_fds = fds;
  sleep_secs = secs;
  slept_fd = hit_fd;
  pt_sema_post(&sleeping_sema);
}

void scheme_end_sleeper_thread()
{
  scheme_signal_received();
  pt_sema_wait(&done_sema);

  /* Clear external event flag */
  if (external_event_fd) {
    char buf[10];
    read(external_event_fd, buf, 10);
  }
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

#else

void scheme_start_sleeper_thread(void (*given_sleep)(float seconds, void *fds), float secs, void *fds, int hit_fd)
{
}
void scheme_end_sleeper_thread()
{
}

#endif

/*========================================================================*/
/*                       memory debugging help                            */
/*========================================================================*/


#ifdef MEMORY_COUNTING_ON
void scheme_count_input_port(Scheme_Object *port, intptr_t *s, intptr_t *e,
			     Scheme_Hash_Table *ht)
{
  Scheme_Input_Port *ip;

  ip = scheme_input_port_record(port);

  *e = (ht ? scheme_count_memory(ip->read_handler, ht) : 0);
  *s = sizeof(Scheme_Input_Port);

  if (ip->sub_type == file_input_port_type)
    *s += sizeof(Scheme_Input_File);
  else if (ip->sub_type == scheme_string_input_port_type) {
    Scheme_Indexed_String *is;
    is = (Scheme_Indexed_String *)ip->port_data;
    *s += (sizeof(Scheme_Indexed_String)
	   + is->size);
  } else if (ip->sub_type == scheme_tcp_input_port_type) {
    if (ht && !scheme_hash_get(ht, (Scheme_Object *)ip->port_data)) {
      scheme_hash_set(ht, (Scheme_Object *)ip->port_data, scheme_true);
    }
  } else if (ip->sub_type == scheme_user_input_port_type) {
    Scheme_Object **d;
    d = (Scheme_Object **)ip->port_data;
    *s += (3 * sizeof(Scheme_Object *));
    *e += (ht
	   ? (scheme_count_memory(d[0], ht)
	      + scheme_count_memory(d[1], ht)
	      + scheme_count_memory(d[2], ht))
	   : 0);
  } else if (ip->sub_type == scheme_pipe_read_port_type) {
    if (ht && !scheme_hash_get(ht, (Scheme_Object *)ip->port_data)) {
      Scheme_Pipe *p = (Scheme_Pipe *)ip->port_data;
      scheme_hash_set(ht, (Scheme_Object *)ip->port_data, scheme_true);
      *s += (sizeof(Scheme_Pipe) + p->buflen);
    }
  }
}

void scheme_count_output_port(Scheme_Object *port, intptr_t *s, intptr_t *e,
			      Scheme_Hash_Table *ht)
{
  Scheme_Output_Port *op;

  op = scheme_output_port_record(port);

  *e = 0;
  *s = sizeof(Scheme_Output_Port);

  if (op->sub_type == file_output_port_type)
    *s += sizeof(Scheme_Output_File);
  else if (op->sub_type == scheme_string_output_port_type) {
    Scheme_Indexed_String *is;
    is = (Scheme_Indexed_String *)op->port_data;
    *s += (sizeof(Scheme_Indexed_String)
	   + is->size);
  } else if (op->sub_type == scheme_tcp_output_port_type) {
    if (!scheme_hash_get(ht, (Scheme_Object *)op->port_data)) {
      scheme_hash_set(ht, (Scheme_Object *)op->port_data, scheme_true);
    }
  } else if (op->sub_type == scheme_user_output_port_type) {
    Scheme_Object **d;
    d = (Scheme_Object **)op->port_data;
    *s += (2 * sizeof(Scheme_Object *));
    *e += (ht
	   ? (scheme_count_memory(d[0], ht)
	      + scheme_count_memory(d[1], ht))
	   : 0);
  } else if (op->sub_type == scheme_pipe_read_port_type) {
    if (!scheme_hash_get(ht, (Scheme_Object *)op->port_data)) {
      Scheme_Pipe *p = (Scheme_Pipe *)op->port_data;
      scheme_hash_set(ht, (Scheme_Object *)op->port_data, scheme_true);
      *s += (sizeof(Scheme_Pipe) + p->buflen);
    }
  }
}
#endif

/*========================================================================*/
/*                       precise GC traversers                            */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_port.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_input_file, mark_input_file);
  GC_REG_TRAV(scheme_rt_output_file, mark_output_file);

#ifdef MZ_FDS
  GC_REG_TRAV(scheme_rt_input_fd, mark_input_fd);
#endif

#if defined(UNIX_PROCESSES) && !defined(MZ_PLACES_WAITPID)
  GC_REG_TRAV(scheme_rt_system_child, mark_system_child);
#endif

#ifdef USE_OSKIT_CONSOLE
  GC_REG_TRAV(scheme_rt_oskit_console_input, mark_oskit_console_input);
#endif

  GC_REG_TRAV(scheme_subprocess_type, mark_subprocess);
  GC_REG_TRAV(scheme_write_evt_type, mark_read_write_evt);
}

END_XFORM_SKIP;

#endif
