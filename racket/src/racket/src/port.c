/*
  Racket
  Copyright (c) 2004-2017 PLT Design Inc.
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
#include "schrktio.h"
#include <errno.h>
#ifndef DONT_IGNORE_PIPE_SIGNAL
# include <signal.h>
#endif
#ifdef USE_ITIMER
# include <sys/time.h>
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

/******************** Subprocesses ********************/

typedef struct Scheme_Subprocess {
  Scheme_Object so;
  rktio_process_t *proc;
  Scheme_Custodian_Reference *mref;
} Scheme_Subprocess;

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

static int *stdin_refcount, *stdout_refcount, *stderr_refcount;

# define MZPORT_FD_BUFFSIZE 4096
# define MZPORT_FD_DIRECT_THRESHOLD MZPORT_FD_BUFFSIZE

/* The Scheme_FD type is used for both input and output */
typedef struct Scheme_FD {
  MZTAG_IF_REQUIRED
  rktio_fd_t *fd;
  intptr_t bufcount, buffpos;
  char flushing, flush;
  char *buffer;
  int *refcount;
  Scheme_Object *flush_handle; /* output port: registration with plumber */
  /* For text mode and `port-file-position`: */
  char *bufwidths;
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
  so->flush_mode = fds->flush;
  return 1;
}

#define MZ_FLUSH_NEVER 0
#define MZ_FLUSH_BY_LINE 1
#define MZ_FLUSH_ALWAYS 2

#if defined(DOS_FILE_SYSTEM)
# if defined(__MINGW32__)
#  define fseeko fseek
#  define ftello ftell
# else
#  define fseeko _fseeki64
#  define ftello _ftelli64
# endif
#endif

/******************** Globals and Prototypes ********************/

/* globals */

READ_ONLY Scheme_Object scheme_eof[1];
THREAD_LOCAL_DECL(Scheme_Object *scheme_orig_stdout_port);
THREAD_LOCAL_DECL(Scheme_Object *scheme_orig_stderr_port);
THREAD_LOCAL_DECL(Scheme_Object *scheme_orig_stdin_port);

HOOK_SHARED_OK Scheme_Object *(*scheme_make_stdin)(void) = NULL;
HOOK_SHARED_OK Scheme_Object *(*scheme_make_stdout)(void) = NULL;
HOOK_SHARED_OK Scheme_Object *(*scheme_make_stderr)(void) = NULL;

SHARED_OK MZ_DLLSPEC int scheme_binary_mode_stdio = 0;
void scheme_set_binary_mode_stdio(int v) { scheme_binary_mode_stdio =  v; }

THREAD_LOCAL_DECL(static int special_is_ok);

/* locals */

THREAD_LOCAL_DECL(static int fd_reserved);
THREAD_LOCAL_DECL(static rktio_fd_t *the_fd);

READ_ONLY static Scheme_Object *fd_input_port_type;
READ_ONLY static Scheme_Object *file_input_port_type;
READ_ONLY Scheme_Object *scheme_string_input_port_type;
READ_ONLY Scheme_Object *scheme_tcp_input_port_type;
READ_ONLY Scheme_Object *scheme_tcp_output_port_type;
READ_ONLY static Scheme_Object *fd_output_port_type;
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

static void register_port_wait();

static intptr_t flush_fd(Scheme_Output_Port *op,
                         const char * volatile bufstr, volatile uintptr_t buflen,
                         volatile uintptr_t offset, int immediate_only, int enable_break);
static void flush_if_output_fds(Scheme_Object *o, Scheme_Close_Custodian_Client *f, void *data);

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

static void block_timer_signals(int block);

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
static int filesystem_change_evt_ready(Scheme_Object *evt, Scheme_Schedule_Info *sinfo);

static void filesystem_change_evt_need_wakeup (Scheme_Object *port, void *fds);

static Scheme_Object *
_scheme_make_named_file_input_port(FILE *fp, Scheme_Object *name, int regfile);
static void default_sleep(float v, void *fds);
#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

static Scheme_Object *make_fd_input_port(rktio_fd_t *fd, Scheme_Object *name, int *refcount, int internal);
static Scheme_Object *make_fd_output_port(rktio_fd_t *fd, Scheme_Object *name, int read_too, int flush_mode, int *refcount);

static void force_close_output_port(Scheme_Object *port);
static void force_close_input_port(Scheme_Object *port);

ROSYM static Scheme_Object *text_symbol, *binary_symbol, *module_symbol;
ROSYM static Scheme_Object *append_symbol, *error_symbol, *update_symbol, *can_update_symbol;
ROSYM static Scheme_Object *replace_symbol, *truncate_symbol, *truncate_replace_symbol;
ROSYM static Scheme_Object *must_truncate_symbol;

ROSYM Scheme_Object *scheme_none_symbol, *scheme_line_symbol, *scheme_block_symbol;

ROSYM static Scheme_Object *exact_symbol;

#define READ_STRING_BYTE_BUFFER_SIZE 1024
THREAD_LOCAL_DECL(static char *read_string_byte_buffer);

#define fail_err_symbol scheme_false

typedef struct Scheme_Filesystem_Change_Evt {
  Scheme_Object so;
  rktio_fs_change_t *rfc;
  Scheme_Custodian_Reference *mref;
} Scheme_Filesystem_Change_Evt;

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
  REGISTER_SO(module_symbol);
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
  module_symbol = scheme_intern_symbol("module");
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

  REGISTER_SO(fd_input_port_type);
  REGISTER_SO(fd_output_port_type);
  REGISTER_SO(file_input_port_type);
  REGISTER_SO(scheme_string_input_port_type);
  REGISTER_SO(scheme_tcp_input_port_type);
  REGISTER_SO(scheme_tcp_output_port_type);
  REGISTER_SO(file_output_port_type);
  REGISTER_SO(scheme_string_output_port_type);
  REGISTER_SO(scheme_user_input_port_type);
  REGISTER_SO(scheme_user_output_port_type);
  REGISTER_SO(scheme_pipe_read_port_type);
  REGISTER_SO(scheme_pipe_write_port_type);
  REGISTER_SO(scheme_null_output_port_type);
  REGISTER_SO(scheme_redirect_output_port_type);

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

  fd_input_port_type = scheme_make_port_type("<stream-input-port>");
  fd_output_port_type = scheme_make_port_type("<stream-output-port>");

  file_input_port_type = scheme_make_port_type("<file-input-port>");
  file_output_port_type = scheme_make_port_type("<file-output-port>");

  scheme_user_input_port_type = scheme_make_port_type("<user-input-port>");
  scheme_user_output_port_type = scheme_make_port_type("<user-output-port>");

  scheme_pipe_read_port_type = scheme_make_port_type("<pipe-input-port>");
  scheme_pipe_write_port_type = scheme_make_port_type("<pipe-output-port>");

  scheme_tcp_input_port_type = scheme_make_port_type("<tcp-input-port>");
  scheme_tcp_output_port_type = scheme_make_port_type("<tcp-output-port>");

  scheme_null_output_port_type = scheme_make_port_type("<null-output-port>");
  scheme_redirect_output_port_type = scheme_make_port_type("<redirect-output-port>");

  scheme_add_global_constant("subprocess", scheme_make_prim_w_arity2(subprocess, "subprocess", 4, -1, 4, 4), env);
  scheme_add_global_constant("subprocess-status", scheme_make_prim_w_arity(subprocess_status, "subprocess-status", 1, 1), env);
  scheme_add_global_constant("subprocess-kill", scheme_make_prim_w_arity(subprocess_kill, "subprocess-kill", 2, 2), env);
  scheme_add_global_constant("subprocess-pid", scheme_make_prim_w_arity(subprocess_pid, "subprocess-pid", 1, 1), env);
  scheme_add_global_constant("subprocess?", scheme_make_prim_w_arity(subprocess_p, "subprocess?", 1, 1), env);
  scheme_add_global_constant("subprocess-wait", scheme_make_prim_w_arity(subprocess_wait, "subprocess-wait", 1, 1), env);

  GLOBAL_PARAMETER("subprocess-group-enabled", subproc_group_on, MZCONFIG_SUBPROC_GROUP_ENABLED, env);
  GLOBAL_PARAMETER("current-subprocess-custodian-mode", current_subproc_cust_mode, MZCONFIG_SUBPROC_CUSTODIAN_MODE, env);

  scheme_add_global_constant("shell-execute", scheme_make_prim_w_arity(sch_shell_execute, "shell-execute", 5, 5), env);
}

void scheme_init_port_wait()
{
  register_port_wait();
  register_subprocess_wait();

  scheme_add_evt(scheme_progress_evt_type, (Scheme_Ready_Fun)progress_evt_ready, NULL, NULL, 1);
  scheme_add_evt(scheme_write_evt_type, (Scheme_Ready_Fun)rw_evt_ready, rw_evt_wakeup, NULL, 1);
  scheme_add_evt(scheme_port_closed_evt_type, (Scheme_Ready_Fun)closed_evt_ready, NULL, NULL, 1);
  scheme_add_evt(scheme_filesystem_change_evt_type, (Scheme_Ready_Fun)filesystem_change_evt_ready, 
                 filesystem_change_evt_need_wakeup, NULL, 1);
}

void scheme_init_port_places(void)
{

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

  REGISTER_SO(read_string_byte_buffer);
  REGISTER_SO(scheme_orig_stdout_port);
  REGISTER_SO(scheme_orig_stderr_port);
  REGISTER_SO(scheme_orig_stdin_port);
  scheme_orig_stdin_port = (scheme_make_stdin
			    ? scheme_make_stdin()
			    : make_fd_input_port(rktio_std_fd(scheme_rktio, RKTIO_STDIN), scheme_intern_symbol("stdin"),
						 stdin_refcount, 0));

  scheme_orig_stdout_port = (scheme_make_stdout
			     ? scheme_make_stdout()
			     : make_fd_output_port(rktio_std_fd(scheme_rktio, RKTIO_STDOUT), scheme_intern_symbol("stdout"), 0,
                                                   -1, stdout_refcount));

  scheme_orig_stderr_port = (scheme_make_stderr
			     ? scheme_make_stderr()
			     : make_fd_output_port(rktio_std_fd(scheme_rktio, RKTIO_STDERR), scheme_intern_symbol("stderr"), 0,
                                                   MZ_FLUSH_ALWAYS, stderr_refcount));

  if (!scheme_current_place_id) {
    adj_refcount(stdin_refcount, -1);
    adj_refcount(stdout_refcount, -1);
    adj_refcount(stderr_refcount, -1);
  }

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

/*========================================================================*/
/*                                fd arrays                               */
/*========================================================================*/

/* This compatibility layer assumes that poll sets are allocated at
   8-byte boundaries, so bites 2 and 3 can be used to indicate the
   write and exception offsets. */

#define EXTRACT_FD_OFFSET(p) ((((uintptr_t)p)&0x6) >> 1)
#define EXTRACT_FD_BASE(p) ((void *)(((uintptr_t)p)&~(uintptr_t)0x6))

void *scheme_alloc_fdset_array(int count, int permanent)
{
  return rktio_make_poll_set(scheme_rktio);
}

void *scheme_get_fdset(void *fdarray, int pos)
{
  return (void *)(((uintptr_t)fdarray) | (2 * pos));
}

void scheme_fdzero(void *fds)
{  
  scheme_signal_error("scheme_fdzero is not supported");
}

void scheme_fdset(void *fds, int pos)
{
  int offset = EXTRACT_FD_OFFSET(fds);
  fds = EXTRACT_FD_BASE(fds);

  if (offset != 2) {
    rktio_fd_t *rfd;
    rfd = rktio_system_fd(scheme_rktio, pos, RKTIO_OPEN_SOCKET);
    rktio_poll_add(scheme_rktio, rfd, fds, (offset ? RKTIO_OPEN_WRITE : RKTIO_OPEN_READ));
    rktio_forget(scheme_rktio, rfd);
  }
}

void scheme_fdclr(void *fds, int pos)
{
  scheme_signal_error("scheme_fdclr is not supported");
}

int scheme_fdisset(void *fds, int pos)
{
  scheme_signal_error("scheme_fdisset is not supported");
  return 0;
}

void scheme_collapse_win_fd(void *fds)
{
  scheme_signal_error("scheme_collapse_win_fd is not supported");
}

void scheme_add_fd_handle(void *h, void *fds, int repost)
{
  fds = EXTRACT_FD_BASE(fds);
  rktio_poll_set_add_handle(scheme_rktio, (intptr_t)h, fds, repost);
}

void scheme_add_fd_nosleep(void *fds)
{
  fds = EXTRACT_FD_BASE(fds);
  rktio_poll_set_add_nosleep(scheme_rktio, fds);
}

void scheme_add_fd_eventmask(void *fds, int mask)
{
  fds = EXTRACT_FD_BASE(fds);
  rktio_poll_set_add_eventmask(scheme_rktio, fds, mask);
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
  if (ip->column >= 0)
    ip->column += a;
  if (ip->readpos >= 0)
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

  ip->oldColumn = ip->column; /* works for a single-char read, like `read' */

  if (ip->readpos >= 0)
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
    if (ip->lineNumber >= 0)
      ip->lineNumber += n;
    ip->was_cr = (buffer[offset + got - 1] == '\r');
    /* Now reset column to 0: */
    if (ip->column >= 0)
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
    if (ip->column >= 0)
      ip->column = col;
    ip->utf8state = state;
  }

  if (ip->readpos >= 0)
    ip->readpos -= degot;
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

    if (ip->input_lock)
      scheme_wait_input_allowed(ip, only_avail);

    /* check progress evt before checking for closed: */
    if (unless_evt 
        && SAME_TYPE(SCHEME_TYPE(unless_evt), scheme_progress_evt_type)
        && SCHEME_SEMAP(SCHEME_PTR2_VAL(unless_evt))
        && scheme_try_plain_sema(SCHEME_PTR2_VAL(unless_evt)))
      return 0;

    CHECK_PORT_CLOSED(who, "input", port, ip->closed);

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
        if (!peek) {
          ip->pending_eof = 1;
          if (ip->progress_evt)
            post_progress(ip);
        }
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
	      && ((gc != EOF) || ip->pending_eof)
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
        if ((cnt < size) && (ip->pending_eof == 2)) {
	  ip->pending_eof = 1;
          size--;
          did = 1;
        }
	pip = (Scheme_Input_Port *)ip->peeked_read;
	gs = pip->get_string_fun;
      } else {
        if (ip->pending_eof == 2) {
          ip->pending_eof = 1;
          did = 1;
          if (ip->progress_evt)
            post_progress(ip);
        }
	gs = NULL;
	pip = NULL;
      }
    }
      
    if (gs && size) {
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

  /* We used to return `did', but since an event has already been
     selected, claim success at this point always. */

  return 1 || did;
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

  if (port->closed) {
    scheme_post_sema_all(sema);
    return sema;
  }

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

int scheme_getc(Scheme_Object *port)
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
scheme_get_byte(Scheme_Object *port) XFORM_ASSERT_NO_CONVERSION
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

    if (ip->p.position > (len - 1))
      ip->p.position -= (len - 1);

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

static void check_input_port_lock(Scheme_Port *ip)
{
  if (SCHEME_INPORTP(ip)) {
    Scheme_Input_Port *iip = (Scheme_Input_Port *)ip;
    if (iip->input_lock)
      scheme_wait_input_allowed(iip, 0);
  }
}

static intptr_t
do_tell (Scheme_Object *port, int not_via_loc)
{
  Scheme_Port *ip;
  intptr_t pos;

  ip = scheme_port_record(port);
  
  check_input_port_lock(ip);
  
  CHECK_IOPORT_CLOSED("get-file-position", ip);

  if (not_via_loc || !ip->count_lines || (ip->position < 0))
    pos = ip->position;
  else
    pos = ip->readpos;

  return pos;
}

intptr_t
scheme_tell (Scheme_Object *port)
{
  return do_tell(port, 0);
}

intptr_t
scheme_tell_line (Scheme_Object *port)
{
  Scheme_Port *ip;
  intptr_t line;

  ip = scheme_port_record(port);

  if (!ip->count_lines || (ip->position < 0))
    return -1;

  check_input_port_lock(ip);

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

  check_input_port_lock(ip);

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
    pos = scheme_tell_can_redirect(port, 0);

    if (_line) *_line = line;
    if (_col) *_col = col;
    if (_pos) *_pos = pos;
  }
}

intptr_t
scheme_tell_can_redirect (Scheme_Object *port, int not_via_loc)
{
  Scheme_Port *ip;
  Scheme_Object *v;

  while (1) {
    ip = scheme_port_record(port);
    
    if (ip->position_redirect) {
      if (SCHEME_INPUT_PORTP(ip->position_redirect)
          || SCHEME_OUTPUT_PORTP(ip->position_redirect)) {
        SCHEME_USE_FUEL(1);
        port = ip->position_redirect;
      } else {
        v = scheme_apply(ip->position_redirect, 0, NULL);
        if (SCHEME_INTP(v) && (SCHEME_INT_VAL(v) >= 1))
          return SCHEME_INT_VAL(v) - 1;
        else if (SCHEME_FALSEP(v) || (SCHEME_BIGNUMP(v) && SCHEME_BIGPOS(v)))
          return -1;
        else {
          Scheme_Object *a[1];
          a[0] = v;
          scheme_wrong_contract("file-position", "exact-positive-integer?", 0, -1, a);
          return -1;
        }
      }
    } else
      break;
  }

  return do_tell(port, not_via_loc);
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

  if (ip->input_lock && scheme_force_port_closed)
    scheme_wait_input_allowed(ip, 0);

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

static intptr_t
put_byte_string_slow(const char *who, Scheme_Object *port,
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

intptr_t
scheme_put_byte_string(GC_CAN_IGNORE const char *who, Scheme_Object *port,
		       GC_CAN_IGNORE const char *str, intptr_t d, intptr_t len,
		       int rarely_block)
{
  intptr_t out;

  if (SCHEME_OUTPORTP(port)
      && !((Scheme_Output_Port *)port)->closed
      && (rarely_block != -1)
      && (len == 1)
      && !((Scheme_Output_Port *)port)->p.count_lines) {
    Scheme_Output_Port *op = (Scheme_Output_Port *)port;
    Scheme_Write_String_Fun ws;
    ws = op->write_string_fun;
    out = ws(op, str, d, 1, rarely_block, 0);
    if (out) {
      op->p.position += out;
      return out;
    } else if (rarely_block)
      return 0;
  }

  return put_byte_string_slow(who, port, str, d, len, rarely_block);
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
    else if (SAME_OBJ(ip->sub_type, fd_input_port_type))
      return scheme_true;
  } else if (SCHEME_OUTPUT_PORTP(p)) {
    Scheme_Output_Port *op;

    op = scheme_output_port_record(p);

    if (SAME_OBJ(op->sub_type, file_output_port_type))
      return scheme_true;
    else if (SAME_OBJ(op->sub_type, fd_output_port_type))
      return scheme_true;
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
      } else if (SAME_OBJ(ip->sub_type, fd_input_port_type)) {
	fd = rktio_fd_system_fd(scheme_rktio, ((Scheme_FD *)ip->port_data)->fd);
	fd_ok = 1;
      }
    }
  } else if (SCHEME_OUTPUT_PORTP(p)) {
    Scheme_Output_Port *op;

    op = scheme_output_port_record(p);

    if (!op->closed) {
      if (SAME_OBJ(op->sub_type, file_output_port_type))  {
	fd = MSC_IZE (fileno)((FILE *)((Scheme_Output_File *)op->port_data)->f);
	fd_ok = 1;
      } else if (SAME_OBJ(op->sub_type, fd_output_port_type))  {
	fd = rktio_fd_system_fd(scheme_rktio, ((Scheme_FD *)op->port_data)->fd);
	fd_ok = 1;
      }
    }
  }

  if (!fd_ok)
    return 0;

  *_fd = fd;
  return 1;
}

int scheme_get_port_rktio_file_descriptor(Scheme_Object *p, rktio_fd_t **_fd)
{
  if (SCHEME_INPUT_PORTP(p)) {
    Scheme_Input_Port *ip;

    ip = scheme_input_port_record(p);

    if (!ip->closed) {
      if (SAME_OBJ(ip->sub_type, fd_input_port_type)) {
        *_fd = ((Scheme_FD *)ip->port_data)->fd;
        return 1;
      }
    }
  } else if (SCHEME_OUTPUT_PORTP(p)) {
    Scheme_Output_Port *op;

    op = scheme_output_port_record(p);

    if (!op->closed) {
      if (SAME_OBJ(op->sub_type, fd_output_port_type))  {
        *_fd = ((Scheme_FD *)op->port_data)->fd;
	return 1;
      }
    }
  }

  return 0;
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

  return scheme_get_fd_identity(p, fd, NULL, 0);
}

static int is_fd_terminal(intptr_t fd)
{
  rktio_fd_t *rfd;
  int is_term;
  
  rfd = rktio_system_fd(scheme_rktio, fd, RKTIO_OPEN_NOT_REGFILE);
  is_term = rktio_fd_is_terminal(scheme_rktio, rfd);
  rktio_forget(scheme_rktio, rfd);

  return is_term;
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
    else if (SAME_OBJ(ip->sub_type, fd_input_port_type)) {
      fd = rktio_fd_system_fd(scheme_rktio, ((Scheme_FD *)ip->port_data)->fd);
      fd_ok = 1;
    }
  } else if (SCHEME_OUTPUT_PORTP(p)) {
    Scheme_Output_Port *op;

    op = scheme_output_port_record(p);

    if (op->closed)
      return scheme_false;

    if (SAME_OBJ(op->sub_type, file_output_port_type))  {
      fd = MSC_IZE (fileno)((FILE *)((Scheme_Output_File *)op->port_data)->f);
      fd_ok = 1;
    }
    else if (SAME_OBJ(op->sub_type, fd_output_port_type))  {
      fd = rktio_fd_system_fd(scheme_rktio, ((Scheme_FD *)op->port_data)->fd);
      fd_ok = 1;
    }
  }

  if (!fd_ok)
    return scheme_false;

  return is_fd_terminal(fd) ? scheme_true : scheme_false;
}

static void filename_exn(char *name, char *msg, char *filename, int maybe_module_errno)
{
  char *dir, *drive;
  int len;
  char *pre, *rel, *post;
  Scheme_Object *mod_path, *mp;

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

  if (maybe_module_errno && scheme_last_error_is_racket(maybe_module_errno)) {
    mod_path = scheme_get_param(scheme_current_config(), MZCONFIG_CURRENT_MODULE_LOAD_PATH);
    if (SCHEME_TRUEP(mod_path)) {
      if (SCHEME_STXP(mod_path)) {
        char *srcloc;
        intptr_t srcloc_len;
        mp = scheme_syntax_to_datum(mod_path, 0, NULL);
        srcloc = scheme_make_srcloc_string(mod_path, &srcloc_len);
        scheme_raise_exn(MZEXN_FAIL_SYNTAX_MISSING_MODULE,
                         scheme_make_pair(mod_path, scheme_null),
                         mp,
                         "%t%s: %s\n"
                         "  module path: %W\n"
                         "  path: %q%s%q%s\n"
                         "  system error: %R",
                         srcloc, srcloc_len,
                         srcloc_len ? "" : name,
                         "cannot open module file", 
                         mp, filename,
                         pre, rel, post);
      } else {
        scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_MISSING_MODULE,
                         mod_path,
                         "%s: %s\n"
                         "  module path: %W\n"
                         "  path: %q%s%q%s\n"
                         "  system error: %R",
                         name, "cannot open module file", 
                         mod_path, filename,
                         pre, rel, post);
      }
      return;
    }
  }

  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		   "%s: %s\n"
                   "  path: %q%s%q%s\n"
                   "  system error: %R",
		   name, msg, filename,
		   pre, rel, post);
}

Scheme_Object *
scheme_do_open_input_file(char *name, int offset, int argc, Scheme_Object *argv[],
                          int internal, int for_module)
{
  char *filename;
  int i;
  int m_set = 0, mm_set = 0, text_mode = 0;
  rktio_fd_t *fd;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract(name, "path-string?", 0, argc, argv);

  for (i = 1 + offset; argc > i; i++) {
    if (!SCHEME_SYMBOLP(argv[i]))
      scheme_wrong_contract(name, "symbol?", i, argc, argv);

    if (SAME_OBJ(argv[i], text_symbol)) {
      text_mode = 1;
      m_set++;
    } else if (SAME_OBJ(argv[i], binary_symbol)) {
      /* This is the default */
      m_set++;
    } else if (SAME_OBJ(argv[i], module_symbol)) {
      mm_set++;
      for_module = 1;
    } else if (SAME_OBJ(argv[i], scheme_none_symbol)) {
      mm_set++;
      for_module = 0;
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

    if ((m_set > 1) || (mm_set > 1)) {
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

  fd = rktio_open(scheme_rktio, filename, (RKTIO_OPEN_READ
                                           | (text_mode ? RKTIO_OPEN_TEXT : 0)));

  if (!fd) {
    filename_exn(name, "cannot open input file", filename, (for_module ? RKTIO_ERROR_DOES_NOT_EXIST : 0));
    return NULL;
  }
  
  return make_fd_input_port(fd, scheme_make_path(filename), NULL, internal);
}

Scheme_Object *
scheme_do_open_output_file(char *name, int offset, int argc, Scheme_Object *argv[], int and_read, 
                           int internal)
{
  int e_set = 0, m_set = 0, i;
  int open_flags = 0, try_replace = 0;
  char *filename;
  char mode[4];
  int typepos;
  rktio_fd_t *fd;

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
      open_flags = RKTIO_OPEN_APPEND;
      e_set++;
    } else if (SAME_OBJ(argv[i], replace_symbol)) {
      try_replace = 1;
      e_set++;
    } else if (SAME_OBJ(argv[i], truncate_symbol)) {
      open_flags = RKTIO_OPEN_TRUNCATE | RKTIO_OPEN_CAN_EXIST;
      e_set++;
    } else if (SAME_OBJ(argv[i], must_truncate_symbol)) {
      open_flags = RKTIO_OPEN_MUST_EXIST | RKTIO_OPEN_TRUNCATE;
      e_set++;
    } else if (SAME_OBJ(argv[i], truncate_replace_symbol)) {
      open_flags = RKTIO_OPEN_TRUNCATE | RKTIO_OPEN_CAN_EXIST;
      try_replace = 1;
      e_set++;
    } else if (SAME_OBJ(argv[i], update_symbol)) {
      open_flags = RKTIO_OPEN_MUST_EXIST;
      if (typepos == 1) {
	mode[2] = mode[1];
	typepos = 2;
      }
      mode[0] = 'r';
      mode[1] = '+';
      e_set++;
    } else if (SAME_OBJ(argv[i], can_update_symbol)) {
      open_flags = RKTIO_OPEN_CAN_EXIST;
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
                                               | (try_replace
                                                  ? SCHEME_GUARD_FILE_DELETE
                                                  : 0)
                                               /* append mode: */
                                               | ((mode[0] == 'a')
                                                  ? SCHEME_GUARD_FILE_READ
                                                  : 0)
                                               /* update mode: */
                                               | ((open_flags & (RKTIO_OPEN_CAN_EXIST | RKTIO_OPEN_MUST_EXIST)
                                                   && !(open_flags & (RKTIO_OPEN_TRUNCATE
                                                                      | RKTIO_OPEN_APPEND))
                                                   && !try_replace)
                                                  ? SCHEME_GUARD_FILE_READ
                                                  : 0))));

  scheme_custodian_check_available(NULL, name, "file-stream");

  while (1) {
    fd = rktio_open(scheme_rktio, filename, (RKTIO_OPEN_WRITE
                                             | open_flags
                                             | (and_read ? RKTIO_OPEN_READ : 0)
                                             | ((mode[1] == 't') ? RKTIO_OPEN_TEXT : 0)));
    
    if (!fd
        && try_replace
        && (scheme_last_error_is_racket(RKTIO_ERROR_EXISTS)
            || (scheme_last_error_is_racket(RKTIO_ERROR_ACCESS_DENIED)
                && rktio_file_exists(scheme_rktio, filename)))) {
      /* In replace mode, delete file and try again */
      if (!rktio_delete_file(scheme_rktio, filename, scheme_can_enable_write_permission())) {
        scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
                         "%s: error deleting file\n"
                         "  path: %q\n"
                         "  system error: %R",
                         name, filename);
      }
      try_replace = 0;
    } else
      break;
  }

  if (!fd) {
    if (scheme_last_error_is_racket(RKTIO_ERROR_EXISTS)) {
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_EXISTS,
                       "%s: file exists\n"
                       "  path: %q", name, filename);
    } else if (scheme_last_error_is_racket(RKTIO_ERROR_IS_A_DIRECTORY)) {
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_EXISTS,
                       "%s: path is a directory\n"
                       "  path: %q",
                       name, filename);
    } else
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
                         "%s: cannot open output file\n"
                         "  path: %q\n"
                         "  system error: %R",
                         name, filename);
  }
    
  return make_fd_output_port(fd, scheme_make_path(filename), and_read, -1, NULL);
}

Scheme_Object *scheme_open_input_file(const char *name, const char *who)
{
  Scheme_Object *a[1];

  a[0]= scheme_make_path(name);
  return scheme_do_open_input_file((char *)who, 0, 1, a, 0, 0);
}

Scheme_Object *scheme_open_output_file(const char *name, const char *who)
{
  Scheme_Object *a[2];

  a[0]= scheme_make_path(name);
  a[1] = truncate_replace_symbol;
  return scheme_do_open_output_file((char *)who, 0, 2, a, 0, 0);
}

Scheme_Object *scheme_open_input_output_file(const char *name, const char *who, Scheme_Object **oport)
{
  Scheme_Object *a[2];

  a[0]= scheme_make_path(name);
  a[1] = truncate_replace_symbol;
  scheme_do_open_output_file((char *)who, 0, 2, a, 1, 0);
  *oport = scheme_multiple_array[1];
  return scheme_multiple_array[0];
}

Scheme_Object *scheme_open_output_file_with_mode(const char *name, const char *who, int text)
{
  Scheme_Object *a[3];

  a[0]= scheme_make_path(name);
  a[1] = truncate_replace_symbol;
  a[2] = (text ? text_symbol : binary_symbol);
  return scheme_do_open_output_file((char *)who, 0, 3, a, 0, 0);
}

#ifdef WINDOWS_FILE_HANDLES
static int win_seekable(intptr_t fd)
{
  /* SetFilePointer() requires " a file stored on a seeking device".
     I'm not sure how to test that, so we approximate as "regular
     file". */
  return GetFileType((HANDLE)fd) == FILE_TYPE_DISK;
}
#endif

static Scheme_Object *
do_file_position(const char *who, int argc, Scheme_Object *argv[], int can_false)
{
  FILE *f;
  Scheme_Indexed_String *is;
  rktio_fd_t *fd;
  int wis;

  if (!SCHEME_OUTPUT_PORTP(argv[0]) && !SCHEME_INPUT_PORTP(argv[0]))
    scheme_wrong_contract(who, "port?", 0, argc, argv);
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
	scheme_wrong_contract(who, "(or/c exact-nonnegative-integer? eof-object?)", 1, argc, argv);
    }
  }

  f = NULL;
  is = NULL;
  wis = 0;
  fd = NULL;

  if (!SCHEME_INPUT_PORTP(argv[0])) {
    Scheme_Output_Port *op;

    op = scheme_output_port_record(argv[0]);

    if (SAME_OBJ(op->sub_type, file_output_port_type)) {
      f = ((Scheme_Output_File *)op->port_data)->f;
    } else if (SAME_OBJ(op->sub_type, fd_output_port_type)) {
      fd = ((Scheme_FD *)op->port_data)->fd;
    } else if (SAME_OBJ(op->sub_type, scheme_string_output_port_type)) {
      is = (Scheme_Indexed_String *)op->port_data;
      wis = 1;
    } else if (argc < 2) {
      intptr_t pos;
      pos = scheme_tell_can_redirect(argv[0], 1);
      if (pos < 0) {
        if (can_false) return scheme_false;
	scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
			 "the port's current position is not known\n"
                         "  port: %v",
			 op);
      } else
        return scheme_make_integer(pos);
    }
  } else {
    Scheme_Input_Port *ip;

    ip = scheme_input_port_record(argv[0]);

    if (ip->input_lock)
      scheme_wait_input_allowed(ip, 0);

    if (SAME_OBJ(ip->sub_type, file_input_port_type)) {
      f = ((Scheme_Input_File *)ip->port_data)->f;
    } else if (SAME_OBJ(ip->sub_type, fd_input_port_type)) {
      fd = ((Scheme_FD *)ip->port_data)->fd;
    } else if (SAME_OBJ(ip->sub_type, scheme_string_input_port_type))
      is = (Scheme_Indexed_String *)ip->port_data;
    else if (argc < 2) {
      intptr_t pos;
      pos = scheme_tell_can_redirect((Scheme_Object *)ip, 1);
      if (pos < 0) {
        if (can_false) return scheme_false;
	scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
			 "the port's current position is not known\n"
                         "  port: %v",
			 ip);
      }
      return scheme_make_integer_value(pos);
    }
  }

  if (!f && !fd && !is)
    scheme_contract_error(who,
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
      scheme_contract_error(who,
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
    } else if (fd) {
      if (!SCHEME_INPUT_PORTP(argv[0])) {
	flush_fd(scheme_output_port_record(argv[0]), NULL, 0, 0, 0, 0);
      }

      if (!rktio_set_file_position(scheme_rktio, fd, nll,
                                   ((whence == SEEK_SET)
                                    ? RKTIO_POSITION_FROM_START
                                    : RKTIO_POSITION_FROM_END))) {
	scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
			 "file-position: position change failed on stream\n"
                         "  system error: %R");
        return NULL;
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
    } else if (fd) {
      rktio_filesize_t *sz;
      
      sz = rktio_get_file_position(scheme_rktio, fd);
      if (!sz) {
        pll = do_tell(argv[0], 0);
      } else {
        pll = *sz;
        free(sz);

	if (SCHEME_INPUT_PORTP(argv[0])) {          
          Scheme_Input_Port *ip;
          ip = scheme_input_port_record(argv[0]);
	  pll -= ((Scheme_FD *)ip->port_data)->bufcount;
          if (rktio_fd_is_text_converted(scheme_rktio, fd)) {
            /* Correct for CRLF->LF conversion of buffer content */
            int bp, bd;
            bd = ((Scheme_FD *)ip->port_data)->buffpos;
            for (bp = ((Scheme_FD *)ip->port_data)->bufcount; bp--; ) {
              if (((Scheme_FD *)ip->port_data)->bufwidths[bp + bd]) {
                /* this is a LF converted from CRLF */
                pll--;
              }
            }
            pll -= rktio_buffered_byte_count(scheme_rktio, ((Scheme_FD *)ip->port_data)->fd);
          }
	} else {
          Scheme_Output_Port *op;
          op = scheme_output_port_record(argv[0]);
	  pll += ((Scheme_FD *)op->port_data)->bufcount;
	}
      }
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

Scheme_Object *
scheme_file_position(int argc, Scheme_Object *argv[])
{
  return do_file_position("file-position", argc, argv, 0);
}

Scheme_Object *
scheme_file_position_star(int argc, Scheme_Object *argv[])
{
  return do_file_position("file-position*", argc, argv, 1);
}

Scheme_Object *scheme_file_truncate(int argc, Scheme_Object *argv[])
{
  mzlonglong nll;
  Scheme_Output_Port *op;
  rktio_fd_t *fd;
  int free_fd = 0, ok;

  if (!SCHEME_OUTPUT_PORTP(argv[0])
      || SCHEME_FALSEP(scheme_file_stream_port_p(1, argv)))
    scheme_wrong_contract("file-truncate", "(and/c output-port? file-stream-port?)", 0, argc, argv);

  if (!(SCHEME_INTP(argv[1]) && (SCHEME_INT_VAL(argv[1]) >= 0))
      && !(SCHEME_BIGNUMP(argv[1]) && SCHEME_BIGPOS(argv[1])))
    scheme_wrong_contract("file-truncate", "exact-nonnegative-integer?", 1, argc, argv);

  if (!scheme_get_long_long_val(argv[1], &nll)) {
    scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
                     "file-truncate: size change failed\n"
                     "  reason: size too large");
  }

  op = scheme_output_port_record(argv[0]);
  
  if (SAME_OBJ(op->sub_type, file_output_port_type)) {
    intptr_t sfd;
    sfd = MSC_IZE (fileno)((FILE *)((Scheme_Output_File *)op->port_data)->f);
    fd = rktio_system_fd(scheme_rktio, sfd, RKTIO_OPEN_NOT_REGFILE);
    free_fd = 1;
  } else if (SAME_OBJ(op->sub_type, fd_output_port_type)) {
    fd = ((Scheme_FD *)op->port_data)->fd;
  } else
    return scheme_void;

  flush_fd(scheme_output_port_record(argv[0]), NULL, 0, 0, 0, 0);

  ok = rktio_set_file_size(scheme_rktio, fd, nll);

  if (free_fd) rktio_forget(scheme_rktio, fd);
  
  if (!ok) {
    scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
                     "file-truncate: size change failed\n"
                     "  system error: %R");
  }

  return scheme_void;
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
  rktio_fd_t *rfd = NULL;
  intptr_t fd;
  int writer = 0, r;

  if (!scheme_get_port_rktio_file_descriptor(argv[0], &rfd)
      && !scheme_get_port_file_descriptor(argv[0], &fd))
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

  if (!rfd) {
    rfd = rktio_system_fd(scheme_rktio, fd, RKTIO_OPEN_READ | RKTIO_OPEN_WRITE | RKTIO_OPEN_NOT_REGFILE);
    r = rktio_file_lock_try(scheme_rktio, rfd, writer);
    rktio_forget(scheme_rktio, rfd);
  } else
    r = rktio_file_lock_try(scheme_rktio, rfd, writer);

  if (r == RKTIO_LOCK_ACQUIRED)
    return scheme_true;
  
  if (r == RKTIO_LOCK_ERROR) {
    scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
                     "port-try-file-lock?: error getting file %s lock\n"
                     "  system error: %R",
                     (writer ? "exclusive" : "shared"));
  }
   
  return scheme_false;
}

Scheme_Object *scheme_file_unlock(int argc, Scheme_Object **argv)
{
  intptr_t fd;
  rktio_fd_t *rfd = NULL;
  int r;

  if (!scheme_get_port_rktio_file_descriptor(argv[0], &rfd)
      && !scheme_get_port_file_descriptor(argv[0], &fd))
    scheme_wrong_contract("port-file-unlock", "file-stream-port?", 0, argc, argv);

  check_already_closed("port-file-unlock", argv[0]);

  if (!rfd) {
    rfd = rktio_system_fd(scheme_rktio, fd, RKTIO_OPEN_READ | RKTIO_OPEN_WRITE | RKTIO_OPEN_NOT_REGFILE);
    r = rktio_file_unlock(scheme_rktio, rfd);
    rktio_forget(scheme_rktio, rfd);
  } else
    r = rktio_file_unlock(scheme_rktio, rfd);

  if (!r) {
    scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
                     "port-file-unlock: error unlocking file\n"
                     "  system error: %R");
  }

  return scheme_void;
}

/*========================================================================*/
/*                        filesystem change events                        */
/*========================================================================*/

static void filesystem_change_evt_fnl(void *fc, void *data)
{
  scheme_filesystem_change_evt_cancel((Scheme_Object *)fc, NULL);
}

Scheme_Object *scheme_filesystem_change_evt(Scheme_Object *path, int flags, int signal_errs)
{
  char *filename;
  rktio_fs_change_t *rfc;

  filename = scheme_expand_string_filename(path,
					   "filesystem-change-evt",
					   NULL,
					   SCHEME_GUARD_FILE_EXISTS);
  
  rfc = rktio_fs_change(scheme_rktio, filename, scheme_semaphore_fd_set);

  if (!rfc
      && !(rktio_fs_change_properties(scheme_rktio) & RKTIO_FS_CHANGE_FILE_LEVEL)
      && scheme_file_exists(filename)) {
    Scheme_Object *base;
    int is_dir;
    char *try_filename;
    (void)scheme_split_path(filename, strlen(filename), &base, &is_dir, SCHEME_PLATFORM_PATH_KIND);
    try_filename = scheme_expand_string_filename(base,
						 "filesystem-change-evt",
						 NULL,
						 SCHEME_GUARD_FILE_EXISTS);
    rfc = rktio_fs_change(scheme_rktio, try_filename, scheme_semaphore_fd_set);
  }

  if (!rfc) {
    if (scheme_last_error_is_racket(RKTIO_ERROR_UNSUPPORTED)) {
      scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
                       "filesystem-change-evt: " NOT_SUPPORTED_STR "\n"
                       "  path: %q\n",
                       filename);
    } else {
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
                       "filesystem-change-evt: error generating event\n"
                       "  path: %q\n"
                       "  system error: %R",
                       filename);
    }
    
    return NULL;
  }

  {
    Scheme_Filesystem_Change_Evt *fc;
    Scheme_Custodian_Reference *mref;

    fc = MALLOC_ONE_TAGGED(Scheme_Filesystem_Change_Evt);
    fc->so.type = scheme_filesystem_change_evt_type;
    fc->rfc = rfc;

    mref = scheme_add_managed(NULL, (Scheme_Object *)fc, scheme_filesystem_change_evt_cancel, NULL, 1);
    fc->mref = mref;

    scheme_add_finalizer(fc, filesystem_change_evt_fnl, NULL);

    return (Scheme_Object *)fc;
  }
}

void scheme_filesystem_change_evt_cancel(Scheme_Object *evt, void *ignored_data)
{
  Scheme_Filesystem_Change_Evt *fc = (Scheme_Filesystem_Change_Evt *)evt;

  if (fc->rfc) {
    rktio_fs_change_forget(scheme_rktio, fc->rfc);
    fc->rfc = NULL;
  }

  if (fc->mref) {
    scheme_remove_managed(fc->mref, (Scheme_Object *)fc);
    fc->mref = NULL;
  }
}

static int filesystem_change_evt_ready(Scheme_Object *evt, Scheme_Schedule_Info *sinfo)
{
  Scheme_Filesystem_Change_Evt *fc = (Scheme_Filesystem_Change_Evt *)evt;

  if (!fc->rfc)
    return 1;

  if (rktio_poll_fs_change_ready(scheme_rktio, fc->rfc))
    return 1;
  
  return 0;
}

static void filesystem_change_evt_need_wakeup(Scheme_Object *evt, void *fds)
{
  Scheme_Filesystem_Change_Evt *fc = (Scheme_Filesystem_Change_Evt *)evt;
    
  if (fc->rfc)
    rktio_poll_add_fs_change(scheme_rktio, fc->rfc, fds);
}

void scheme_fs_change_properties(int *_supported, int *_scalable, int *_low_latency, int *_file_level)
{
  int props;

  props = rktio_fs_change_properties(scheme_rktio);
  if ((props & RKTIO_FS_CHANGE_NEED_LTPS) && !scheme_semaphore_fd_set)
    props = 0;
  
  *_supported = ((props & RKTIO_FS_CHANGE_SUPPORTED) ? 1 : 0);
  *_scalable = ((props & RKTIO_FS_CHANGE_SCALABLE) ? 1 : 0);
  *_low_latency = ((props & RKTIO_FS_CHANGE_LOW_LATENCY) ? 1 : 0);
  *_file_level = ((props & RKTIO_FS_CHANGE_FILE_LEVEL) ? 1 : 0);
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

static int
fd_byte_ready (Scheme_Input_Port *port)
{
  Scheme_FD *fip = (Scheme_FD *)port->port_data;

  if (port->closed || rktio_fd_is_regular_file(scheme_rktio, fip->fd))
    return 1;

  if (fip->bufcount)
    return 1;
  else {
    if (rktio_poll_read_ready(scheme_rktio, fip->fd))
      return 1;
    else
      return 0;
  }
}

static void
fd_need_wakeup(Scheme_Input_Port *port, void *fds)
{
  Scheme_FD *fip =  (Scheme_FD *)port->port_data;

  rktio_poll_add(scheme_rktio, fip->fd, fds, RKTIO_POLL_READ);
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
      
      sema = scheme_rktio_fd_to_semaphore(fip->fd, MZFD_CREATE_READ);

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

    if (rktio_fd_is_text_converted(scheme_rktio, fip->fd)) {
      /* Always read into the port buffer so that `bufwidths` can be
         filled in parallel to the buffer. */
      ext_target = 0;
      target = fip->buffer;
      target_offset = 0;
      if (fip->flush == MZ_FLUSH_ALWAYS)
        target_size = 1;
      else
        target_size = MZPORT_FD_BUFFSIZE;

      bc = rktio_read_converted(scheme_rktio, fip->fd, fip->buffer, target_size, fip->bufwidths);
    } else {
      bc = rktio_read(scheme_rktio, fip->fd, target + target_offset, target_size);
    }

    if (bc == 0)
      none_avail = 1;
    else if (bc == RKTIO_READ_EOF)
      bc = 0; /* EOF */
    else if (bc == RKTIO_READ_ERROR)
      bc = -1; /* error */
    
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
                         "  system error: %R",
                         port->name);
        return 0;
      }

      if (!fip->bufcount) {
        if (rktio_buffered_byte_count(scheme_rktio, fip->fd)) {
          /* maybe have a CR pending for text conversion, so maybe keep trying */
          if (nonblock > 0)
            return 0;
        } else {
          fip->buffpos = 0;
          return EOF;
        }
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
  XFORM_ASSERT_NO_CONVERSION
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
  int rc;

  fip = (Scheme_FD *)port->port_data;

  rc = adj_refcount(fip->refcount, -1);
  if (!rc)
    rktio_close(scheme_rktio, fip->fd);
  else
    rktio_forget(scheme_rktio, fip->fd);
}

static void
fd_init_close_input(Scheme_Input_Port *port)
{
  /* never actually opened! */
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
make_fd_input_port(rktio_fd_t *fd, Scheme_Object *name, int *refcount, int internal)
{
  Scheme_Input_Port *ip;
  Scheme_FD *fip;
  char *bfr;
  int start_closed = 0;

  fip = MALLOC_ONE_RT(Scheme_FD);
#ifdef MZTAG_REQUIRED
  fip->type = scheme_rt_input_fd;
#endif

  bfr = (char *)scheme_malloc_atomic(MZPORT_FD_BUFFSIZE);
  fip->buffer = bfr;
  if (rktio_fd_is_text_converted(scheme_rktio, fd)) {
    char *bws;
    bws = scheme_malloc_atomic(MZPORT_FD_BUFFSIZE);
    fip->bufwidths = bws;
  }

  fip->fd = fd;
  fip->bufcount = 0;

  if (refcount) {
    fip->refcount = refcount;
    if (!adj_refcount(refcount, 1)) {
      /* fd is already closed! */
      start_closed = 1;
      rktio_forget(scheme_rktio, fd);
      fip->fd = NULL;
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

  if (start_closed)
    scheme_close_input_port((Scheme_Object *)ip);

  return (Scheme_Object *)ip;
}

Scheme_Object *
scheme_make_fd_input_port(int fd, Scheme_Object *name, int regfile, int textmode)
{
  rktio_fd_t *rfd;

  rfd = rktio_system_fd(scheme_rktio,
                        fd,
                        (RKTIO_OPEN_READ
                         | (regfile
                            ? RKTIO_OPEN_REGFILE
                            : RKTIO_OPEN_NOT_REGFILE)
                         | (textmode ? RKTIO_OPEN_TEXT : 0)));
  
  return make_fd_input_port(rfd, name, NULL, 0);
}

Scheme_Object *
scheme_make_rktio_fd_input_port(rktio_fd_t *rfd, Scheme_Object *name)
{
  return make_fd_input_port(rfd, name, NULL, 0);
}

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

static int
fd_write_ready (Scheme_Object *port)
{
  /* As always, the result of this function is only meaningful when
     the port has been flushed. */

  Scheme_FD *fop;
  Scheme_Output_Port *op;

  op = scheme_output_port_record(port);
  fop = (Scheme_FD *)op->port_data;

  if (op->closed)
    return 1;

  return rktio_poll_write_ready(scheme_rktio, fop->fd);
}

static void
fd_write_need_wakeup(Scheme_Object *port, void *fds)
{
  Scheme_Output_Port *op;
  Scheme_FD *fop;

  op = scheme_output_port_record(port);
  fop = (Scheme_FD *)op->port_data;

  rktio_poll_add(scheme_rktio, fop->fd, fds, RKTIO_POLL_WRITE);
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

      len = rktio_write(scheme_rktio, fop->fd, bufstr + offset, buflen - offset);

      if (!len) {
        /* Need to block; remember that we're holding a lock. */
        Scheme_Object *sema;

        if (immediate_only == 2) {
          fop->flushing = 0;
          return wrote;
        }

        sema = scheme_rktio_fd_to_semaphore(fop->fd, MZFD_CREATE_WRITE);

        BEGIN_ESCAPEABLE(release_flushing_lock, fop);
        if (sema)
          scheme_wait_sema(sema, enable_break ? -1 : 0);
        else
          scheme_block_until_enable_break(fd_write_ready,
                                          fd_write_need_wakeup,
                                          (Scheme_Object *)op, 0.0,
                                          enable_break);
        END_ESCAPEABLE();
      } else if (len == RKTIO_WRITE_ERROR) {
	if (scheme_force_port_closed) {
	  /* Don't signal exn or wait. Just give up. */
	  return wrote;
	} else {
	  fop->flushing = 0;
	  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
			   "error writing to stream port\n"
                           "  system error: %R");
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
fd_write_string_slow(Scheme_Output_Port *port,
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

static intptr_t
fd_write_string(Scheme_Output_Port *port,
		const char *str, intptr_t d, intptr_t len,
		int rarely_block, int enable_break)
  XFORM_ASSERT_NO_CONVERSION
{
  Scheme_FD *fop;
  intptr_t l;
  int flush = (!len || rarely_block);

  fop = (Scheme_FD *)port->port_data;

  if (!flush && !fop->flushing && (fop->flush == MZ_FLUSH_NEVER)) {
    l = MZPORT_FD_BUFFSIZE - fop->bufcount;
    if (len <= l) {
      memcpy(fop->buffer + fop->bufcount, str + d, len);
      fop->bufcount += len;
      return len;
    }
  }

  return fd_write_string_slow(port, str, d, len, rarely_block, enable_break);
}

static int end_fd_flush_done(Scheme_Object *fop)
{
  return rktio_poll_write_flushed(scheme_rktio, ((Scheme_FD *)fop)->fd);
}

static void end_fd_flush_needs_wakeup(Scheme_Object *fop, void *fds)
{
  rktio_poll_add(scheme_rktio, ((Scheme_FD *)fop)->fd, fds, RKTIO_POLL_FLUSH);
}
  
static void
fd_close_output(Scheme_Output_Port *port)
{
  Scheme_FD *fop = (Scheme_FD *)port->port_data;
  int rc;

  if (fop->bufcount)
    flush_fd(port, NULL, 0, 0, 0, 0);

  if (fop->flushing && !scheme_force_port_closed)
    wait_until_fd_flushed(port, 0);

  if (!scheme_force_port_closed && fop->fd) {
    /* Check for flushing at the rktio level (not to be confused
       with pulmber flushes): */
    while (!rktio_poll_write_flushed(scheme_rktio, fop->fd)) {
      scheme_block_until(end_fd_flush_done, end_fd_flush_needs_wakeup, (Scheme_Object *)fop, 0.0);
    }
  }

  scheme_remove_flush(fop->flush_handle);

  /* Make sure no close happened while we blocked above! */
  if (port->closed)
    return;

  rc = adj_refcount(fop->refcount, -1);
  if (fop->fd) {
    if (!rc)
      rktio_close(scheme_rktio, fop->fd);
    else
      rktio_forget(scheme_rktio, fop->fd);
  }
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
make_fd_output_port(rktio_fd_t *fd, Scheme_Object *name, int and_read, int flush_mode, int *refcount)
{
  Scheme_FD *fop;
  char *bfr;
  Scheme_Object *the_port, *fh;
  int start_closed = 0;

  fop = MALLOC_ONE_RT(Scheme_FD);
#ifdef MZTAG_REQUIRED
  fop->type = scheme_rt_input_fd;
#endif

  bfr = (char *)scheme_malloc_atomic(MZPORT_FD_BUFFSIZE);
  fop->buffer = bfr;

  fop->fd = fd;
  fop->bufcount = 0;

  if (flush_mode > -1) {
    fop->flush = flush_mode;
  } else if (rktio_fd_is_terminal(scheme_rktio, fd)) {
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

  fh = scheme_add_flush(NULL, the_port, 0);
  fop->flush_handle = fh;

  if (start_closed)
    scheme_close_output_port(the_port);

  if (and_read) {
    int *rc;
    Scheme_Object *a[2];
    rc = malloc_refcount(1, 1);
    fop->refcount = rc;
    fd = rktio_system_fd(scheme_rktio, rktio_fd_system_fd(scheme_rktio, fd), rktio_fd_modes(scheme_rktio, fd));
    a[1] = the_port;
    a[0] = make_fd_input_port(fd, name, rc, 0);
    return scheme_values(2, a);
  } else
    return the_port;
}

static void flush_if_output_fds(Scheme_Object *o, Scheme_Close_Custodian_Client *f, void *data)
{
  if (SCHEME_OUTPORTP(o)) {
    scheme_flush_if_output_fds(o);
  }
}

void scheme_flush_if_output_fds(Scheme_Object *o)
{
  Scheme_Output_Port *op;
  op = scheme_output_port_record(o);
  if (SAME_OBJ(op->sub_type, fd_output_port_type))
    scheme_flush_output(o);
}

Scheme_Object *
scheme_make_fd_output_port(int fd, Scheme_Object *name, int regfile, int textmode, int read_too)
{
  rktio_fd_t *rfd;

  rfd = rktio_system_fd(scheme_rktio,
                        fd,
                        (RKTIO_OPEN_WRITE
                         | (regfile ? RKTIO_OPEN_REGFILE : RKTIO_OPEN_NOT_REGFILE)
                         | (read_too ? RKTIO_OPEN_READ : 0)
                         | (textmode ? RKTIO_OPEN_TEXT : 0)));
  
  return make_fd_output_port(rfd, name, read_too, -1, NULL);
}

Scheme_Object *
scheme_make_rktio_fd_output_port(rktio_fd_t *rfd, Scheme_Object *name, int read_too)
{
  return make_fd_output_port(rfd, name, read_too, -1, NULL);
}

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

intptr_t
scheme_redirect_write_bytes(Scheme_Output_Port *op,
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
				(Scheme_Object *)op,
				str, d, len,
				(enable_break && !rarely_block) ? -1 : rarely_block);
}

static intptr_t
redirect_write_bytes(Scheme_Output_Port *op,
                            const char *str, intptr_t d, intptr_t len,
                            int rarely_block, int enable_break)
{
  return scheme_redirect_write_bytes(scheme_output_port_record((Scheme_Object *)op->port_data),
                                     str, d, len,
                                     rarely_block, enable_break);
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

  n = scheme_redirect_write_bytes(op, str, d, len, rarely_block, enable_break);

  return scheme_make_integer(n);
}

static Scheme_Object *redirect_write_special_k(void);

int scheme_redirect_write_special (Scheme_Output_Port *op, Scheme_Object *v, int nonblock)
{
  Scheme_Object *a[2];

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      Scheme_Object *n;
      
      p->ku.k.p1 = (void *)op;
      p->ku.k.p2 = (void *)v;
      p->ku.k.i1 = nonblock;
      
      n = scheme_handle_stack_overflow(redirect_write_special_k);
      return SCHEME_INT_VAL(n);
    }
  }
#endif

  a[0] = (Scheme_Object *)v;
  a[1] = (Scheme_Object *)op;
  
  if (nonblock)
    v = scheme_write_special_nonblock(2, a);
  else
    v = scheme_write_special(2, a);

  return SCHEME_TRUEP(v);
}

static Scheme_Object *redirect_write_special_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Output_Port *op = (Scheme_Output_Port *)p->ku.k.p1;
  Scheme_Object *v = (Scheme_Object *)p->ku.k.p2;
  intptr_t nonblock = p->ku.k.i1;
  intptr_t n;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  n = scheme_redirect_write_special(op, v, nonblock);

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
  return scheme_redirect_write_special(scheme_output_port_record((Scheme_Object *)op->port_data),
                                       special,
                                       nonblock);
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

static Scheme_Object *redirect_get_or_peek_bytes_k(void);

intptr_t scheme_redirect_get_or_peek_bytes(Scheme_Input_Port *orig_port,
                                           Scheme_Input_Port *port,
                                           char *buffer, intptr_t offset, intptr_t size,
                                           int nonblock,
                                           int peek, Scheme_Object *peek_skip,
                                           Scheme_Object *unless,
                                           Scheme_Schedule_Info *sinfo)
{
  int r;

  if (sinfo) {
    scheme_set_sync_target(sinfo, (Scheme_Object *)port, (Scheme_Object *)orig_port, NULL, 0, 1, NULL);
    return 0;
  }

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      Scheme_Object *n;
      
      p->ku.k.p1 = (void *)port;
      p->ku.k.p2 = (void *)buffer;
      p->ku.k.p3 = (void *)peek_skip;
      p->ku.k.p4 = (void *)unless;
      p->ku.k.p4 = (void *)orig_port;
      p->ku.k.i1 = offset;
      p->ku.k.i1 = size;
      p->ku.k.i2 = nonblock;
      p->ku.k.i3 = peek;
      
      n = scheme_handle_stack_overflow(redirect_get_or_peek_bytes_k);
      return SCHEME_INT_VAL(n);
    }
  }
#endif

  r = scheme_get_byte_string_special_ok_unless("redirect-read-or-peek",
                                               (Scheme_Object *)port,
                                               buffer, offset, size, 
                                               ((nonblock == -1)
                                                ? -1
                                                : (nonblock ? 2 : 1)),
                                               peek, (peek ? peek_skip : NULL),
                                               unless);

  if (r == SCHEME_SPECIAL) {
    Scheme_Object *res;
    res = scheme_get_special_proc((Scheme_Object *)port);
    orig_port->special = res;
  }

  return r;
}

static Scheme_Object *redirect_get_or_peek_bytes_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Input_Port *ip = (Scheme_Input_Port *)p->ku.k.p1;
  char *buffer = (char *)p->ku.k.p2;
  Scheme_Object *peek_skip = (Scheme_Object *)p->ku.k.p3;
  Scheme_Object *unless = (Scheme_Object *)p->ku.k.p4;
  Scheme_Input_Port *orig_port = (Scheme_Input_Port *)p->ku.k.p5;
  intptr_t d = p->ku.k.i1;
  intptr_t len = p->ku.k.i2;
  int nonblock = p->ku.k.i3;
  int peek = p->ku.k.i4;
  intptr_t n;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;
  p->ku.k.p5 = NULL;

  n = scheme_redirect_get_or_peek_bytes(orig_port, ip, buffer, d, len,
                                        nonblock, 
                                        peek, peek_skip,
                                        unless, NULL);

  return scheme_make_integer(n);
}

/*========================================================================*/
/*                             subprocess                                 */
/*========================================================================*/

static void close_subprocess_handle(void *so, void *ignored)
{
  Scheme_Subprocess *sp = (Scheme_Subprocess *)so;

  if (sp->proc) {
    rktio_process_forget(scheme_rktio, sp->proc);
    sp->proc = NULL;
  }
}

static void child_mref_done(Scheme_Subprocess *sp)
{
  if (sp->mref) {
    scheme_remove_managed(sp->mref, (Scheme_Object *)sp);
    sp->mref = NULL;
  }
}

static int subp_done(Scheme_Object *so)
{
  Scheme_Subprocess *sp = (Scheme_Subprocess*)so;

  if (!sp->proc) return 1;

  return rktio_poll_process_done(scheme_rktio, sp->proc);
}

static void subp_needs_wakeup(Scheme_Object *so, void *fds)
{
  Scheme_Subprocess *sp = (Scheme_Subprocess*)so;

  if (sp->proc)
    rktio_poll_add_process(scheme_rktio, sp->proc, fds);
}

static Scheme_Object *subprocess_status(int argc, Scheme_Object **argv)
{
  Scheme_Subprocess *sp = (Scheme_Subprocess *)argv[0];
  rktio_status_t *st;
  
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_subprocess_type))
    scheme_wrong_contract("subprocess-status", "subprocess?", 0, argc, argv);
  
  st = rktio_process_status(scheme_rktio, sp->proc);
  
  if (!st) {
    scheme_raise_exn(MZEXN_FAIL,
                     "subprocess-status: error getting status\n"
                     "  system error: %R");
  }

  if (st->running) {
    free(st);
    return scheme_intern_symbol("running");
  } else {
    int status = st->result;
    free(st);
    child_mref_done(sp);
    return scheme_make_integer_value(status);
  }
}


static void register_subprocess_wait()
{
  scheme_add_evt(scheme_subprocess_type, subp_done,
                 subp_needs_wakeup, NULL, 0);
}

static Scheme_Object *subprocess_wait(int argc, Scheme_Object **argv)
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_subprocess_type))
    scheme_wrong_contract("subprocess-wait", "subprocess?", 0, argc, argv);

  {
    Scheme_Subprocess *sp = (Scheme_Subprocess *)argv[0];

    scheme_block_until(subp_done, subp_needs_wakeup, (Scheme_Object *)sp, (float)0.0);

    return scheme_void;
  }
}

static Scheme_Object *do_subprocess_kill(Scheme_Object *_sp, Scheme_Object *killp, int can_error)
{
  Scheme_Subprocess *sp = (Scheme_Subprocess *)_sp;
  int ok;

  if (!sp->proc)
    return scheme_void;

  if (SCHEME_TRUEP(killp))
    ok = rktio_process_kill(scheme_rktio, sp->proc);
  else
    ok = rktio_process_interrupt(scheme_rktio, sp->proc);

  if (!ok) {
    if (can_error)
      scheme_raise_exn(MZEXN_FAIL, 
                       "subprocess-kill: operation failed\n"
                       "  system error: %R");
  }

  return scheme_void;
}

static void kill_subproc(Scheme_Object *o, void *data)
{
  (void)do_subprocess_kill(o, scheme_true, 0);
}

static void interrupt_subproc(Scheme_Object *o, void *data)
{
  (void)do_subprocess_kill(o, scheme_false, 0);
}

static Scheme_Object *subprocess_kill(int argc, Scheme_Object **argv)
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_subprocess_type))
    scheme_wrong_contract("subprocess-kill", "subprocess?", 0, argc, argv);

  return do_subprocess_kill(argv[0], argv[1], 1);
}

static Scheme_Object *subprocess_pid(int argc, Scheme_Object **argv)
{
  intptr_t pid;
  
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_subprocess_type))
    scheme_wrong_contract("subprocess-pid", "subprocess?", 0, argc, argv);

  pid = rktio_process_pid(scheme_rktio, ((Scheme_Subprocess *)argv[0])->proc);

  return scheme_make_integer_value(pid);
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
  return scheme_param_config2("current-subprocess-custodian-mode", scheme_make_integer(MZCONFIG_SUBPROC_CUSTODIAN_MODE),
                              argc, argv,
                              -1, subproc_cust_mode_p, "(or/c 'interrupt 'kill #f)", 1);
}

static Scheme_Object *subproc_group_on (int argc, Scheme_Object *argv[])
{
  return scheme_param_config("subprocess-group-enabled", scheme_make_integer(MZCONFIG_SUBPROC_GROUP_ENABLED), 
                             argc, argv, 
                             -1, NULL, NULL, 1);
}

static Scheme_Object *subprocess(int c, Scheme_Object *args[])
     /* subprocess(out, in, err, exe, arg ...) */
{
  const char *name = "subprocess";
  Scheme_Object *inport;
  Scheme_Object *outport;
  Scheme_Object *errport;
  Scheme_Object *a[4];
  Scheme_Subprocess *subproc;
  Scheme_Object *cust_mode, *current_dir;
  int flags = 0;
  rktio_fd_t *stdout_fd = NULL;
  rktio_fd_t *stdin_fd = NULL;
  rktio_fd_t *stderr_fd = NULL;
  int need_forget_out = 0, need_forget_in = 0, need_forget_err = 0;
  rktio_envvars_t *envvars;
  rktio_process_result_t *result;
  Scheme_Config *config;
  int argc;
  char **argv, *command;

  /*--------------------------------------------*/
  /* Sort out ports (create later if necessary) */
  /*--------------------------------------------*/

  if (SCHEME_TRUEP(args[0])) {
    outport = args[0];
    if (SCHEME_OUTPUT_PORTP(outport) && SCHEME_TRUEP(scheme_file_stream_port_p(1, &outport))) {
      Scheme_Output_Port *op;

      op = scheme_output_port_record(outport);

      if (SAME_OBJ(op->sub_type, file_output_port_type)) {
	int tmp;
	tmp = MSC_IZE(fileno)(((Scheme_Output_File *)op->port_data)->f);
        stdout_fd = rktio_system_fd(scheme_rktio, tmp, RKTIO_OPEN_WRITE | RKTIO_OPEN_NOT_REGFILE);
        need_forget_out = 1;
      } else if (SAME_OBJ(op->sub_type, fd_output_port_type))
	stdout_fd = ((Scheme_FD *)op->port_data)->fd;
    } else
      scheme_wrong_contract(name, "(or/c (and/c file-stream-port? output-port?) #f)", 0, c, args);
  }

  if (SCHEME_TRUEP(args[1])) {
    inport = args[1];
    if (SCHEME_INPUT_PORTP(inport) && SCHEME_TRUEP(scheme_file_stream_port_p(1, &inport))) {
      Scheme_Input_Port *ip;

      ip = scheme_input_port_record(inport);

      if (SAME_OBJ(ip->sub_type, file_input_port_type)) {
	int tmp;
	tmp = MSC_IZE(fileno)(((Scheme_Input_File *)ip->port_data)->f);
        stdin_fd = rktio_system_fd(scheme_rktio, tmp, RKTIO_OPEN_READ | RKTIO_OPEN_NOT_REGFILE);
        need_forget_in = 1;
      } else if (SAME_OBJ(ip->sub_type, fd_input_port_type))
	stdin_fd = ((Scheme_FD *)ip->port_data)->fd;
    } else
      scheme_wrong_contract(name, "(or/c (and/c file-stream-port? input-port?) #f)", 1, c, args);
  }

  if (SCHEME_SYMBOLP(args[2]) && !SCHEME_SYM_WEIRDP(args[2])
      && !strcmp("stdout", SCHEME_SYM_VAL(args[2]))) {
    flags |= RKTIO_PROCESS_STDOUT_AS_STDERR;
  } else if (SCHEME_TRUEP(args[2])) {
    errport = args[2];
    if (SCHEME_OUTPUT_PORTP(errport) && SCHEME_TRUEP(scheme_file_stream_port_p(1, &errport))) {
      Scheme_Output_Port *op;

      op = scheme_output_port_record(errport);

      if (SAME_OBJ(op->sub_type, file_output_port_type)) {
	int tmp;
	tmp = MSC_IZE(fileno)(((Scheme_Output_File *)op->port_data)->f);
        stderr_fd = rktio_system_fd(scheme_rktio, tmp, RKTIO_OPEN_WRITE | RKTIO_OPEN_NOT_REGFILE);
        need_forget_err = 1;
      } else if (SAME_OBJ(op->sub_type, fd_output_port_type))
	stderr_fd = ((Scheme_FD *)op->port_data)->fd;
    } else
      scheme_wrong_contract(name, "(or/c (and/c file-stream-port? output-port?) #f 'stdout)", 2, c, args);
  }

  if (!SCHEME_PATH_STRINGP(args[3]))
    scheme_wrong_contract(name, "path-string?", 3, c, args);

  /*--------------------------------------*/
  /*          Sort out arguments          */
  /*--------------------------------------*/

  argc = c - 3;
  argv = MALLOC_N(char *, argc);
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

    if (rktio_process_allowed_flags(scheme_rktio) & RKTIO_PROCESS_WINDOWS_EXACT_CMDLINE)
      flags |= RKTIO_PROCESS_WINDOWS_EXACT_CMDLINE;
    else 
      scheme_contract_error(name,
                            "exact command line not supported on this platform",
                            "exact command", 1, args[5],
                            NULL);
  } else {
    int i;
    for (i = 4; i < c; i++) {
      if (((!SCHEME_CHAR_STRINGP(args[i]) && !SCHEME_BYTE_STRINGP(args[i]))
           || scheme_any_string_has_null(args[i]))
          && !SCHEME_PATHP(args[i]))
	scheme_wrong_contract(name,
                              "(or/c path? string-no-nuls? bytes-no-nuls?)",
                              i, c, args);
      {
	Scheme_Object *bs;
        bs = args[i];
        if (SCHEME_CHAR_STRINGP(args[i]))
          bs = scheme_char_string_to_byte_string_locale(bs);
	argv[i - 3] = SCHEME_BYTE_STR_VAL(bs);
      }
    }
  }

  command = argv[0];

  if (!stdin_fd || !stdout_fd || !stderr_fd)
    scheme_custodian_check_available(NULL, name, "file-stream");

  /*--------------------------------------*/
  /*        Create subprocess             */
  /*--------------------------------------*/

  config = scheme_current_config();
  
  cust_mode = scheme_get_param(config, MZCONFIG_SUBPROC_GROUP_ENABLED);
  if (SCHEME_TRUEP(cust_mode))
    flags |= RKTIO_PROCESS_NEW_GROUP;

  cust_mode = scheme_get_param(config, MZCONFIG_SUBPROC_CUSTODIAN_MODE);
  if (SCHEME_SYMBOLP(cust_mode)
      && !strcmp(SCHEME_SYM_VAL(cust_mode), "kill")
      && (rktio_process_allowed_flags(scheme_rktio) & RKTIO_PROCESS_WINDOWS_CHAIN_TERMINATION))
    flags |= RKTIO_PROCESS_WINDOWS_CHAIN_TERMINATION;

  current_dir = scheme_get_param(config, MZCONFIG_CURRENT_DIRECTORY);

  envvars = scheme_environment_variables_to_envvars(scheme_get_param(config, MZCONFIG_CURRENT_ENV_VARS));

  block_timer_signals(1);
  
  result = rktio_process(scheme_rktio,
                         command, argc, argv,
                         stdout_fd, stdin_fd, stderr_fd,
                         SCHEME_PATH_VAL(current_dir), envvars,
                         flags);

  block_timer_signals(0);

  if (need_forget_in) rktio_forget(scheme_rktio, stdin_fd);
  if (need_forget_out) rktio_forget(scheme_rktio, stdout_fd);
  if (need_forget_err) rktio_forget(scheme_rktio, stderr_fd);

  if (envvars)
    rktio_envvars_free(scheme_rktio, envvars);

  if (!result) {
    scheme_raise_exn(MZEXN_FAIL,
                     "subprocess: process creation failed");
  }

  /*--------------------------------------*/
  /*        Create new port objects       */
  /*--------------------------------------*/

  {
    Scheme_Object *in = scheme_false, *out = scheme_false, *err = scheme_false;
    
    if (result->stdout_fd)
      in = make_fd_input_port(result->stdout_fd, scheme_intern_symbol("subprocess-stdout"), NULL, 0);
    if (result->stdin_fd)
      out = make_fd_output_port(result->stdin_fd, scheme_intern_symbol("subprocess-stdin"), 0, -1, NULL);
    if (result->stderr_fd)
      err = make_fd_input_port(result->stderr_fd, scheme_intern_symbol("subprocess-stderr"), NULL, 0);
    
    /*--------------------------------------*/
    /*          Return result info          */
    /*--------------------------------------*/
    
    subproc = MALLOC_ONE_TAGGED(Scheme_Subprocess);
    subproc->so.type = scheme_subprocess_type;
    subproc->proc = result->process;
    scheme_add_finalizer(subproc, close_subprocess_handle, NULL);

    if (SCHEME_TRUEP(cust_mode)) {
      Scheme_Custodian_Reference *mref;
      Scheme_Close_Custodian_Client *closer;

      if (!strcmp(SCHEME_SYM_VAL(cust_mode), "kill"))
        closer = kill_subproc;
      else
        closer = interrupt_subproc;

      mref = scheme_add_managed_close_on_exit(NULL, (Scheme_Object *)subproc, closer, NULL);
      subproc->mref = mref;
    }

    free(result);

    a[0] = (Scheme_Object *)subproc;
    a[1] = in;
    a[2] = out;
    a[3] = err;

    return scheme_values(4, a);
  }
}

static Scheme_Object *sch_shell_execute(int c, Scheme_Object *argv[])
{
  char *dir;
  int show = 0;
  int nplen;
  Scheme_Object *sv, *sf, *sp;

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
      show = RKTIO_ ## id; show_set = 1; }
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

  dir = scheme_expand_string_filename(argv[3],
				      "shell-execute", NULL,
				      SCHEME_GUARD_FILE_EXISTS);

  nplen = strlen(dir);
  dir = scheme_normal_path_seps(dir, &nplen, 0);
    
  if (SCHEME_FALSEP(argv[0]))
    sv = NULL;
  else
    sv = scheme_char_string_to_byte_string(argv[0]);
  sf = scheme_char_string_to_byte_string(argv[1]);
  sp = scheme_char_string_to_byte_string(argv[2]);

  if (rktio_shell_execute(scheme_rktio,
			  sv ? SCHEME_BYTE_STR_VAL(sv) : NULL,
			  SCHEME_BYTE_STR_VAL(sf),
			  SCHEME_BYTE_STR_VAL(sp),
			  dir,
			  show))
    return scheme_false;
  else {
    scheme_raise_exn(MZEXN_FAIL,
		     "shell-execute: execute failed\n"
		     "  command: %V\n"
		     "  system error: %R",
		     argv[1]);
    return NULL;
  }
}

/*========================================================================*/
/*                          fd reservation                                */
/*========================================================================*/

/* We don't want on-demand loading of code to fail because we run out of
   file descriptors. So, keep one in reserve. */

void scheme_reserve_file_descriptor(void)
{
#ifndef DOS_FILE_SYSTEM
  if (!fd_reserved) {
    the_fd = rktio_open(scheme_rktio, "/dev/null", RKTIO_OPEN_READ); 
    if (the_fd)
      fd_reserved = 1;
  }
#endif
}

void scheme_release_file_descriptor(void)
{
#ifndef DOS_FILE_SYSTEM
  if (fd_reserved) {
    rktio_close(scheme_rktio, the_fd);
    fd_reserved = 0;
  }
#endif
}

/*========================================================================*/
/*                             sleeping                                   */
/*========================================================================*/

static void default_sleep(float v, void *fds)
{
  rktio_sleep(scheme_rktio, v, fds, scheme_semaphore_fd_set);
}

void scheme_signal_received_at(void *h)
  XFORM_SKIP_PROC
/* Ensure that Racket wakes up if asleep. */
{
  rktio_signal_received_at(h);
}

void *scheme_get_signal_handle()
  XFORM_SKIP_PROC
{
  return (void *)rktio_get_signal_handle(scheme_rktio);
}

void scheme_signal_received(void)
  XFORM_SKIP_PROC
{
  scheme_signal_received_at(scheme_get_signal_handle());
}

void scheme_wait_until_signal_received(void)
  XFORM_SKIP_PROC
{
  rktio_wait_until_signal_received(scheme_rktio);
}

#ifdef USE_WIN32_THREAD_TIMER

typedef struct ITimer_Data {
  int done;
  HANDLE itimer;
  intptr_t delay;
  HANDLE semaphore;
  HANDLE done_semaphore;
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
#include <unistd.h>
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
  tmp = mz_proc_thread_create_w_stacksize(green_thread_timer, itimerdata, 16384);
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

static void block_timer_signals(int block)
/* Doesn't actually block the signal, because we don't want
   a new subprocess to start with the signal blocked,
   but turns off the timer and makes sure that no signal
   is pending. */
{
  static intptr_t saved_usec;

  if (block) {
    struct itimerval t, old;
    sigset_t sigs;

    t.it_value.tv_sec = 0;
    t.it_value.tv_usec = 0;
    t.it_interval.tv_sec = 0;
    t.it_interval.tv_usec = 0;
    
    setitimer(ITIMER_PROF, &t, &old);

    saved_usec = old.it_value.tv_usec;

    /* Clear already-queued PROF signal, if any
       --- unlikely, but possible */
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
  } else {
    kickoff_itimer(saved_usec);
  }
}

#else

static void block_timer_signals(int block) { }

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
  rktio_flush_signals_received(scheme_rktio);
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
/*                           thread helper                                */
/*========================================================================*/

/* The scheme_call_sequence() functionc an be used, with some care,
   via the FFI to run a computation in a foreign thread and thread
   results through. Keeping the number of procedures below
   `NUM_COPIED_SEQUENCE_PROCS` can potentially simplify things, too */

#define NUM_COPIED_SEQUENCE_PROCS 5

typedef void *(*Scheme_Sequenced_Proc)(void *);

struct Scheme_Proc_Sequence {
  Scheme_Object *num_procs; /* pointer simplifies allocation issues */
  void *init_data;
  Scheme_Sequenced_Proc p[mzFLEX_ARRAY_DECL];
};

void *scheme_call_sequence_of_procedures(struct Scheme_Proc_Sequence *s)
  XFORM_SKIP_PROC
{
  int i, num_procs = SCHEME_INT_VAL(s->num_procs);
  void *data = s->init_data;
  Scheme_Sequenced_Proc copied[NUM_COPIED_SEQUENCE_PROCS];

  if (num_procs <= NUM_COPIED_SEQUENCE_PROCS) {
    for (i = 0; i < num_procs; i++) {
      copied[i] = s->p[i];
    }
  }

  for (i = 0; i < num_procs; i++) {
    if (num_procs <= NUM_COPIED_SEQUENCE_PROCS)
      data = copied[i](data);
    else
      data = s->p[i](data);
  }

  return data;
}

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

  GC_REG_TRAV(scheme_rt_input_fd, mark_input_fd);

  GC_REG_TRAV(scheme_subprocess_type, mark_subprocess);
  GC_REG_TRAV(scheme_write_evt_type, mark_read_write_evt);

  GC_REG_TRAV(scheme_filesystem_change_evt_type, mark_filesystem_change_evt);
}

END_XFORM_SKIP;

#endif
