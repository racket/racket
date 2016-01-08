/*
  Racket
  Copyright (c) 2006-2016 PLT Design Inc.

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
*/

#include "schpriv.h"
#include "schmach.h"

static Scheme_Object *future_p(int argc, Scheme_Object *argv[])
{
  if (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_future_type))
    return scheme_true;
  else
    return scheme_false;
}

Scheme_Object *scheme_fsemaphore_p(int argc, Scheme_Object *argv[])
{
  if (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_fsemaphore_type)) 
    return scheme_true;
  else 
    return scheme_false;
}

static Scheme_Object *futures_enabled(int argc, Scheme_Object *argv[])
{
#ifdef MZ_USE_FUTURES
  return scheme_true;
#else 
  return scheme_false;
#endif
}


#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

#ifndef MZ_USE_FUTURES

/* Futures not enabled, but make a stub module and implementation */

typedef struct future_t {
  Scheme_Object so;
  Scheme_Object *running_sema;
  Scheme_Object *orig_lambda;
  Scheme_Object *retval;
  int multiple_count;
  Scheme_Object **multiple_array;
  int no_retval;
} future_t;

typedef struct fsemaphore_t {
  Scheme_Object so;
  Scheme_Object *sema;
} fsemaphore_t;

Scheme_Object *scheme_future(int argc, Scheme_Object *argv[])
{
  future_t *ft;

  scheme_check_proc_arity("future", 0, 0, argc, argv);

  ft = MALLOC_ONE_TAGGED(future_t);
  ft->so.type = scheme_future_type;

  ft->orig_lambda = argv[0];

  return (Scheme_Object *)ft;
}

static Scheme_Object *touch(int argc, Scheme_Object *argv[])
{
  future_t * volatile ft;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_future_type))
    scheme_wrong_contract("touch", "future?", 0, argc, argv);

  ft = (future_t *)argv[0];

  while (1) {
    if (ft->retval) {
      if (SAME_OBJ(ft->retval, SCHEME_MULTIPLE_VALUES)) {
        Scheme_Thread *p = scheme_current_thread;
        p->ku.multiple.array = ft->multiple_array;
        p->ku.multiple.count = ft->multiple_count;
      }
      return ft->retval;
    }
    if (ft->no_retval)
      scheme_signal_error("touch: future previously aborted");
    
    if (ft->running_sema) {
      scheme_wait_sema(ft->running_sema, 0);
      scheme_post_sema(ft->running_sema);
    } else {
      Scheme_Object *sema;
      future_t *old_ft;
      mz_jmp_buf newbuf, * volatile savebuf;
      Scheme_Thread *p = scheme_current_thread;
      
      /* In case another Racket thread touches the future. */
      sema = scheme_make_sema(0);
      ft->running_sema = sema;

      old_ft = p->current_ft;
      p->current_ft = ft;
      
      savebuf = p->error_buf;
      p->error_buf = &newbuf;
      if (scheme_setjmp(newbuf)) {
        ft->no_retval = 1;
        p->current_ft = old_ft;
        scheme_post_sema(ft->running_sema);
        scheme_longjmp(*savebuf, 1);
      } else {
        GC_CAN_IGNORE Scheme_Object *retval, *proc;
        proc = ft->orig_lambda;
        ft->orig_lambda = NULL; /* don't hold on to proc */
        retval = scheme_apply_multi(proc, 0, NULL);
        ft->retval = retval;
        if (SAME_OBJ(retval, SCHEME_MULTIPLE_VALUES)) {
          ft->multiple_array = p->ku.multiple.array;
          ft->multiple_count = p->ku.multiple.count;
          p->ku.multiple.array = NULL;
        }
        scheme_post_sema(ft->running_sema);
        p->current_ft = old_ft;
        p->error_buf = savebuf;
      }
    }
  }

  return NULL;
}



static Scheme_Object *processor_count(int argc, Scheme_Object *argv[])
{
  return scheme_make_integer(1);
}

int scheme_is_multithreaded(int now)
{
  return 0;
}

Scheme_Object *scheme_current_future(int argc, Scheme_Object *argv[])
{
  future_t *ft = scheme_current_thread->current_ft;

  return (ft ? (Scheme_Object *)ft : scheme_false);
}

Scheme_Object *scheme_make_fsemaphore(int argc, Scheme_Object *argv[])
{
  intptr_t v;
  fsemaphore_t *fsema;
  Scheme_Object *sema;
  
  v = scheme_get_semaphore_init("make-fsemaphore", argc, argv);

  fsema = MALLOC_ONE_TAGGED(fsemaphore_t);
  fsema->so.type = scheme_fsemaphore_type;
  sema = scheme_make_sema(v);
  fsema->sema = sema;

  return (Scheme_Object*)fsema;
}

Scheme_Object *scheme_fsemaphore_post(int argc, Scheme_Object *argv[])
{
  fsemaphore_t *fsema;
  if (argc != 1 || !SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_fsemaphore_type)) 
    scheme_wrong_contract("fsemaphore-post", "fsemaphore?", 0, argc, argv);

  fsema = (fsemaphore_t*)argv[0];
  scheme_post_sema(fsema->sema);

  return scheme_void;
}

Scheme_Object *scheme_fsemaphore_wait(int argc, Scheme_Object *argv[])
{
  fsemaphore_t *fsema;
  if (argc != 1 || !SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_fsemaphore_type)) 
    scheme_wrong_contract("fsemaphore-wait", "fsemaphore?", 0, argc, argv);

  fsema = (fsemaphore_t*)argv[0];
  scheme_wait_sema(fsema->sema, 0);

  return scheme_void;
}

Scheme_Object *scheme_fsemaphore_try_wait(int argc, Scheme_Object *argv[])
{
  fsemaphore_t *fsema;
  if (argc != 1 || !SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_fsemaphore_type))
    scheme_wrong_contract("fsemaphore-try-wait?", "fsemaphore?", 0, argc, argv);

  fsema = (fsemaphore_t*)argv[0];
  if (scheme_wait_sema(fsema->sema, 1))
    return scheme_true;

  return scheme_false;
}

Scheme_Object *scheme_fsemaphore_count(int argc, Scheme_Object *argv[])
{
  fsemaphore_t *fsema;
  if (argc != 1 || !SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_fsemaphore_type)) 
    scheme_wrong_contract("fsemaphore-count", "fsemaphore?", 0, argc, argv);

  fsema = (fsemaphore_t*)argv[0];
  return scheme_make_integer(((Scheme_Sema *)fsema->sema)->value);
}

static Scheme_Object *would_be_future(int argc, Scheme_Object *argv[]) 
{
  scheme_check_proc_arity("would-be-future", 0, 0, argc, argv);
  return scheme_future(argc, argv);  
}

static Scheme_Object *reset_future_logs_for_tracking(int argc, Scheme_Object *argv[])
{
  return scheme_void;
}

static Scheme_Object *mark_future_trace_end(int argc, Scheme_Object *argv[])
{
  return scheme_void;
}

# define FUTURE_PRIM_W_ARITY(name, func, a1, a2, env) GLOBAL_PRIM_W_ARITY(name, func, a1, a2, env)

void scheme_init_futures(Scheme_Env *newenv)
{
  FUTURE_PRIM_W_ARITY("future?",          future_p,         1, 1, newenv);
  FUTURE_PRIM_W_ARITY("future",           scheme_future,    1, 1, newenv);
  FUTURE_PRIM_W_ARITY("processor-count",  processor_count,  0, 0, newenv);
  FUTURE_PRIM_W_ARITY("touch",            touch,            1, 1, newenv);
  FUTURE_PRIM_W_ARITY("current-future",   scheme_current_future,   0, 0, newenv);
  FUTURE_PRIM_W_ARITY("make-fsemaphore",  scheme_make_fsemaphore,  1, 1, newenv);
  FUTURE_PRIM_W_ARITY("fsemaphore?",      scheme_fsemaphore_p,     1, 1, newenv);
  FUTURE_PRIM_W_ARITY("fsemaphore-post",  scheme_fsemaphore_post,  1, 1, newenv);
  FUTURE_PRIM_W_ARITY("fsemaphore-wait",  scheme_fsemaphore_wait,  1, 1, newenv);
  FUTURE_PRIM_W_ARITY("fsemaphore-try-wait?", scheme_fsemaphore_try_wait, 1, 1, newenv);
  FUTURE_PRIM_W_ARITY("fsemaphore-count", scheme_fsemaphore_count, 1, 1, newenv);
  FUTURE_PRIM_W_ARITY("would-be-future", would_be_future, 1, 1, newenv);
  FUTURE_PRIM_W_ARITY("futures-enabled?", futures_enabled, 0, 0, newenv);
  FUTURE_PRIM_W_ARITY("reset-future-logs-for-tracing!", reset_future_logs_for_tracking, 0, 0, newenv);
  GLOBAL_PRIM_W_ARITY("mark-future-trace-end!", mark_future_trace_end, 0, 0, newenv);

  scheme_finish_primitive_module(newenv);
  scheme_protect_primitive_provide(newenv, NULL);

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif
}

void scheme_init_futures_once()
{
}

void scheme_init_futures_per_place()
{
}

void scheme_end_futures_per_place()
{
}

#else

#include "future.h"
#include <stdlib.h>
#include <string.h>
#include "jit.h"

#ifdef OS_X
# define CHECK_FUTURE_ASSERTS
#endif

#ifdef CHECK_FUTURE_ASSERTS
# include <assert.h>
# define FUTURE_ASSERT(x) assert(x)
#else
# define FUTURE_ASSERT(x) /* empty */
#endif

static Scheme_Object *make_fsemaphore(int argc, Scheme_Object *argv[]);
static Scheme_Object *touch(int argc, Scheme_Object *argv[]);
static Scheme_Object *processor_count(int argc, Scheme_Object *argv[]);
static void futures_init(void);
static void init_future_thread(struct Scheme_Future_State *fs, int i);
static Scheme_Future_Thread_State *alloc_future_thread_state();
static void requeue_future(struct future_t *future, struct Scheme_Future_State *fs);
static void future_do_runtimecall(Scheme_Future_Thread_State *fts,
                                  void *func,
                                  int is_atomic,
                                  int can_suspend,
                                  int for_overflow);
static int capture_future_continuation(struct Scheme_Future_State *fs, future_t *ft, void **storage, 
                                       int need_lock, int for_overflow);

#define FUTURE_C_STACK_SIZE 500000
#define FUTURE_RUNSTACK_SIZE 2000

#define FEVENT_BUFFER_SIZE    512
#define NO_FUTURE_ID -1

enum {
  FEVENT_CREATE,
  FEVENT_COMPLETE,
  FEVENT_START_WORK,
  FEVENT_START_RTONLY_WORK,
  FEVENT_RESUME_WORK,
  FEVENT_END_WORK,
  FEVENT_RTCALL_ATOMIC,
  FEVENT_HANDLE_RTCALL_ATOMIC,
  FEVENT_RTCALL,
  FEVENT_RTCALL_TOUCH,
  FEVENT_HANDLE_RTCALL,
  FEVENT_RTCALL_RESULT,
  FEVENT_HANDLE_RTCALL_RESULT,
  FEVENT_RTCALL_ABORT,
  FEVENT_HANDLE_RTCALL_ABORT,
  FEVENT_RTCALL_SUSPEND,
  FEVENT_OVERFLOW,
  FEVENT_TOUCH_PAUSE,
  FEVENT_TOUCH_RESUME,
  FEVENT_MISSING, 
  FEVENT_STOP_TRACE, 
  _FEVENT_COUNT_
};

static const char * const fevent_strs[] = { "create", "complete",
                                            "start-work", "start-0-work", "start-overflow-work", 
                                            "end-work",
                                            "sync", "sync", "block", "touch", "block",
                                            "result", "result", "abort", "abort", 
                                            "suspend", "overflow",
                                            "touch-pause", "touch-resume", "missing", "stop-trace" };
static const char * const fevent_long_strs[] = { "created", "completed",
                                                 "started work", "started (process 0, only)", "started (overflow)", 
                                                 "ended work",
                                                 "synchronizing with process 0", "synchronizing", 
                                                 "BLOCKING on process 0", "touching future", "HANDLING",
                                                 "result from process 0", "result determined",
                                                 "abort from process 0", "abort determined",
                                                 "suspended", "overflow",
                                                 "paused for touch", "resumed for touch",
                                                 "events missing", "stop future tracing" };


typedef struct Scheme_Future_State {
  int thread_pool_size;
  Scheme_Future_Thread_State **pool_threads;
  int busy_thread_count;

  void *signal_handle;

  int future_queue_count;
  future_t *future_queue;
  future_t *future_queue_end;
  future_t *future_waiting_atomic;
  future_t *future_waiting_lwc;
  future_t *future_waiting_touch;
  int next_futureid;

  mzrt_mutex *future_mutex; /* BEWARE: don't allocate while holding this lock */
  mzrt_sema *future_pending_sema;
  mzrt_sema *gc_ok_c;
  mzrt_sema *gc_done_c;

  int gc_not_ok, wait_for_gc, need_gc_ok_post, need_gc_done_post;
  int abort_all_futures;

  int *gc_counter_ptr;

  int future_threads_created;

  Fevent_Buffer runtime_fevents;
  Scheme_Object **fevent_syms;
  Scheme_Struct_Type *fevent_prefab;
} Scheme_Future_State;


THREAD_LOCAL_DECL(static Scheme_Future_State *scheme_future_state);
THREAD_LOCAL_DECL(void *jit_future_storage[4]);

#ifdef MZ_PRECISE_GC
THREAD_LOCAL_DECL(extern uintptr_t GC_gen0_alloc_page_ptr);
THREAD_LOCAL_DECL(extern uintptr_t GC_gen0_alloc_page_end);
THREAD_LOCAL_DECL(extern int GC_gen0_alloc_only);
#endif

static void start_gc_not_ok(Scheme_Future_State *fs);
static void end_gc_not_ok(Scheme_Future_Thread_State *fts, 
                          Scheme_Future_State *fs, 
                          Scheme_Object **current_rs);

static void *worker_thread_future_loop(void *arg);
static void invoke_rtcall(Scheme_Future_State * volatile fs, future_t * volatile future, volatile int is_atomic);
static future_t *enqueue_future(Scheme_Future_State *fs, future_t *ft);;
static future_t *get_pending_future(Scheme_Future_State *fs);
static void receive_special_result(future_t *f, Scheme_Object *retval, int clear);
static void send_special_result(future_t *f, Scheme_Object *retval);
static Scheme_Object *_apply_future_lw(future_t *ft);
static Scheme_Object *apply_future_lw(future_t *ft);
static int fsemaphore_ready(Scheme_Object *obj);
static void init_fevent(Fevent_Buffer *b);
static void free_fevent(Fevent_Buffer *b);
static int future_in_runtime(Scheme_Future_State *fs, future_t * volatile ft, int what);
static Scheme_Object *would_be_future(int argc, Scheme_Object *argv[]);
static void push_suspended_lw(Scheme_Future_State *fs, future_t *ft);
static void pop_suspended_lw(Scheme_Future_State *fs, future_t *ft);

static Scheme_Object *bad_multi_result_proc;
static Scheme_Object *bad_multi_result(int argc, Scheme_Object **argv);
static Scheme_Object *reset_future_logs_for_tracking(int argc, Scheme_Object *argv[]);
static Scheme_Object *mark_future_trace_end(int argc, Scheme_Object *argv[]);

READ_ONLY static int cpucount;
static void init_cpucount(void);

#ifdef MZ_PRECISE_GC
# define scheme_future_setjmp(newbuf) scheme_jit_setjmp((newbuf).jb)
# define scheme_future_longjmp(newbuf, v) scheme_jit_longjmp((newbuf).jb, v)
#else
# define scheme_future_setjmp(newbuf) scheme_setjmp(newbuf)
# define scheme_future_longjmp(newbuf, v) scheme_longjmp(newbuf, v)
#endif

/**********************************************************************/
/* Arguments for a newly created future thread                        */
/**********************************************************************/

typedef struct future_thread_params_t {
  mzrt_sema *ready_sema;
  struct NewGC *shared_GC;
  Scheme_Future_State *fs;
  Scheme_Future_Thread_State *fts;
  Scheme_Object **runstack_start;

  Scheme_Object ***scheme_current_runstack_ptr;
  Scheme_Object ***scheme_current_runstack_start_ptr;
  Scheme_Thread **current_thread_ptr;
  void **jit_future_storage_ptr;
  Scheme_Current_LWC *lwc;
} future_thread_params_t;

/**********************************************************************/
/* Plumbing for Racket initialization                                 */
/**********************************************************************/

/* Invoked by the runtime on startup to make primitives known */
void scheme_init_futures(Scheme_Env *newenv)
{
  Scheme_Object *p;

  scheme_add_global_constant(
                             "future?", 
                             scheme_make_folding_prim(
                                                      future_p, 
                                                      "future?", 
                                                      1, 
                                                      1,
                                                      1), 
                             newenv);

  p = scheme_make_prim_w_arity(scheme_future, "future", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_add_global_constant("future", p, newenv);

  scheme_add_global_constant(
                             "processor-count", 
                             scheme_make_prim_w_arity(
                                                      processor_count, 
                                                      "processor-count", 
                                                      0, 
                                                      0), 
                             newenv);

  p = scheme_make_prim_w_arity(touch, "touch", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_add_global_constant("touch", p, newenv);

  p = scheme_make_immed_prim( 
                              scheme_current_future, 
                              "current-future", 
                              0, 
                              0);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_NARY_INLINED);
  scheme_add_global_constant("current-future", p, newenv);

  p = scheme_make_immed_prim(
                              scheme_fsemaphore_p, 
                              "fsemaphore?", 
                              1, 
                              1);

  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_add_global_constant("fsemaphore?", p, newenv);

  p = scheme_make_immed_prim(
                              make_fsemaphore, 
                              "make-fsemaphore", 
                              1, 
                              1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_add_global_constant("make-fsemaphore", p, newenv);

  p = scheme_make_immed_prim(
                              scheme_fsemaphore_count, 
                              "fsemaphore-count", 
                              1, 
                              1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_add_global_constant("fsemaphore-count", p, newenv);
  
  p = scheme_make_immed_prim(
                              scheme_fsemaphore_wait, 
                              "fsemaphore-wait",
                              1, 
                              1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_add_global_constant("fsemaphore-wait", p, newenv);

  p = scheme_make_immed_prim(
                              scheme_fsemaphore_post, 
                              "fsemaphore-post", 
                              1, 
                              1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_add_global_constant("fsemaphore-post", p, newenv);

  p = scheme_make_immed_prim(
                              scheme_fsemaphore_try_wait, 
                              "fsemaphore-try-wait?", 
                              1, 
                              1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_add_global_constant("fsemaphore-try-wait?", p, newenv);  

  GLOBAL_PRIM_W_ARITY("would-be-future", would_be_future, 1, 1, newenv);
  GLOBAL_PRIM_W_ARITY("futures-enabled?", futures_enabled, 0, 0, newenv);
  GLOBAL_PRIM_W_ARITY("reset-future-logs-for-tracing!", reset_future_logs_for_tracking, 0, 0, newenv);
  GLOBAL_PRIM_W_ARITY("mark-future-trace-end!", mark_future_trace_end, 0, 0, newenv);

  scheme_finish_primitive_module(newenv);
  scheme_protect_primitive_provide(newenv, NULL);
}

void scheme_init_futures_once()
{
  init_cpucount();

  REGISTER_SO(bad_multi_result_proc);
  bad_multi_result_proc = scheme_make_prim(bad_multi_result);
}

void scheme_init_futures_per_place()
{
  futures_init();
}

static Scheme_Object *set_fts_thread(Scheme_Object *ignored)
{
  scheme_future_thread_state->thread = scheme_current_thread;
  return ignored;
}

void futures_init(void)
{
  Scheme_Future_State *fs;
  Scheme_Future_Thread_State *rt_fts;
  Scheme_Future_Thread_State **ftss;
  void *hand;
  Scheme_Object **syms, *sym;
  Scheme_Struct_Type *stype;
  int pool_size;

  fs = (Scheme_Future_State *)malloc(sizeof(Scheme_Future_State));
  memset(fs, 0, sizeof(Scheme_Future_State));
  scheme_future_state = fs;

  pool_size = cpucount * 2;
  ftss = (Scheme_Future_Thread_State **)malloc(pool_size * sizeof(Scheme_Future_Thread_State*));
  memset(ftss, 0, pool_size * sizeof(Scheme_Future_Thread_State*));
  fs->pool_threads = ftss;
  fs->thread_pool_size = pool_size;

  mzrt_mutex_create(&fs->future_mutex);
  mzrt_sema_create(&fs->future_pending_sema, 0);
  mzrt_sema_create(&fs->gc_ok_c, 0);
  mzrt_sema_create(&fs->gc_done_c, 0);
  fs->gc_counter_ptr = &scheme_did_gc_count;

  /* Create a 'dummy' FTS for the RT thread */
  rt_fts = alloc_future_thread_state();
  rt_fts->is_runtime_thread = 1;
  rt_fts->gen0_size = 1;
  scheme_future_thread_state = rt_fts;

  scheme_add_swap_callback(set_fts_thread, scheme_false);
  set_fts_thread(scheme_false);

  REGISTER_SO(fs->future_queue);
  REGISTER_SO(fs->future_queue_end);
  REGISTER_SO(fs->future_waiting_atomic);
  REGISTER_SO(fs->future_waiting_lwc);
  REGISTER_SO(fs->future_waiting_touch);
  REGISTER_SO(fs->fevent_syms);
  REGISTER_SO(fs->fevent_prefab);
  REGISTER_SO(jit_future_storage);

  hand = scheme_get_signal_handle();
  fs->signal_handle = hand;

  syms = MALLOC_N(Scheme_Object*, _FEVENT_COUNT_);
  fs->fevent_syms = syms;
  sym = scheme_intern_symbol(fevent_strs[FEVENT_HANDLE_RTCALL_ATOMIC]);
  syms[FEVENT_HANDLE_RTCALL_ATOMIC] = sym;
  sym = scheme_intern_symbol(fevent_strs[FEVENT_HANDLE_RTCALL]);
  syms[FEVENT_HANDLE_RTCALL] = sym;

  sym = scheme_intern_symbol("future-event");
  stype = scheme_lookup_prefab_type(sym, 6);
  fs->fevent_prefab = stype;

  init_fevent(&fs->runtime_fevents);

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif
}

static void init_future_thread(Scheme_Future_State *fs, int i)
{
  Scheme_Future_Thread_State *fts;
  GC_CAN_IGNORE future_thread_params_t params;
  Scheme_Thread *skeleton;
  Scheme_Object **runstack_start;
  mz_proc_thread *t;

  /* Create the worker thread pool.  These threads will
     'queue up' and wait for futures to become available. */

  fts = alloc_future_thread_state();
  fts->id = i;

  fts->gen0_size = 1;

  fts->use_fevents1 = 1;
  init_fevent(&fts->fevents1);
  init_fevent(&fts->fevents2);

  params.shared_GC = GC_instance;
  params.fts = fts;
  params.fs = fs;

  /* Make enough of a thread record to deal with multiple values. */
  skeleton = MALLOC_ONE_TAGGED(Scheme_Thread);
  skeleton->so.type = scheme_thread_type;

  fts->thread = skeleton;

  {
    Scheme_Object **rs_start;
    intptr_t init_runstack_size = FUTURE_RUNSTACK_SIZE;
    rs_start = scheme_alloc_runstack(init_runstack_size);
    runstack_start = rs_start;
    fts->runstack_size = init_runstack_size;
  }

  /* Fill in GCable values just before creating the thread,
     because the GC ignores `params': */
  params.runstack_start = runstack_start;

  mzrt_sema_create(&params.ready_sema, 0);
  t = mz_proc_thread_create_w_stacksize(worker_thread_future_loop, &params, FUTURE_C_STACK_SIZE);
  mzrt_sema_wait(params.ready_sema);
  mzrt_sema_destroy(params.ready_sema);
  params.ready_sema = NULL;

  fts->t = t;
	
  scheme_register_static(params.scheme_current_runstack_ptr, sizeof(void*));
  scheme_register_static(params.scheme_current_runstack_start_ptr, sizeof(void*));	
  scheme_register_static(params.jit_future_storage_ptr, 4 * sizeof(void*));
  scheme_register_static(params.current_thread_ptr, sizeof(void*));

  fs->pool_threads[i] = fts;
}

static Scheme_Future_Thread_State *alloc_future_thread_state()
{
  Scheme_Future_Thread_State *fts;

  fts = (Scheme_Future_Thread_State *)malloc(sizeof(Scheme_Future_Thread_State));
  memset(fts, 0, sizeof(Scheme_Future_Thread_State));
  scheme_register_static(&fts->thread, sizeof(Scheme_Thread*));  

  return fts;
}

void scheme_end_futures_per_place()
{
  Scheme_Future_State *fs = scheme_future_state;

  if (fs) {
    int i;

    mzrt_mutex_lock(fs->future_mutex);
    fs->abort_all_futures = 1;
    fs->wait_for_gc = 1;
    mzrt_mutex_unlock(fs->future_mutex);

    /* post enough semas to ensure that every future
       wakes up and tries to disable GC: */
    for (i = 0; i < fs->thread_pool_size; i++) { 
      if (fs->pool_threads[i]) {
        mzrt_sema_post(fs->future_pending_sema);
        mzrt_sema_post(fs->pool_threads[i]->worker_can_continue_sema);
      }
    }
    
    scheme_future_block_until_gc();

    /* wait for all future threads to end: */
    for (i = 0; i < fs->thread_pool_size; i++) { 
      if (fs->pool_threads[i]) {
        (void)mz_proc_thread_wait(fs->pool_threads[i]->t);

        free_fevent(&fs->pool_threads[i]->fevents1);
        free_fevent(&fs->pool_threads[i]->fevents2);

        free(fs->pool_threads[i]);
      }
    }

    free_fevent(&fs->runtime_fevents);

    mzrt_mutex_destroy(fs->future_mutex);
    mzrt_sema_destroy(fs->future_pending_sema);
    mzrt_sema_destroy(fs->gc_ok_c);
    mzrt_sema_destroy(fs->gc_done_c);

    free(fs->pool_threads);
    free(fs);

    scheme_future_state = NULL;
  }
}

static void check_future_thread_creation(Scheme_Future_State *fs)
{
  if (!fs->future_threads_created && !fs->future_queue_count)
    return;

  if (fs->future_threads_created < fs->thread_pool_size) {
    int count, busy;

    mzrt_mutex_lock(fs->future_mutex);
    count = fs->future_queue_count;
    busy = fs->busy_thread_count;
    mzrt_mutex_unlock(fs->future_mutex);

    if (count >= (fs->future_threads_created - busy)) {
      init_future_thread(fs, fs->future_threads_created);
      fs->future_threads_created++;
    }
  }
}

static void start_gc_not_ok(Scheme_Future_State *fs)
/* must have mutex_lock */
{
  while (fs->wait_for_gc) {
    int quit = fs->abort_all_futures;
    fs->need_gc_done_post++;
    mzrt_mutex_unlock(fs->future_mutex);
    if (quit) mz_proc_thread_exit(NULL);
    mzrt_sema_wait(fs->gc_done_c);
    mzrt_mutex_lock(fs->future_mutex);
  }

  fs->gc_not_ok++;

#ifdef MZ_PRECISE_GC
  {
    Scheme_Future_Thread_State *fts = scheme_future_thread_state;
    if (fts->worker_gc_counter != *fs->gc_counter_ptr) {
      GC_gen0_alloc_page_ptr = 0; /* forces future to ask for memory */
      GC_gen0_alloc_page_end = 0;
      fts->gen0_start = 0;
      if (fts->gen0_size > 1)
        fts->gen0_size >>= 1;
      fts->worker_gc_counter = *fs->gc_counter_ptr;
    }
  }
#endif
}

static void end_gc_not_ok(Scheme_Future_Thread_State *fts, 
                          Scheme_Future_State *fs, 
                          Scheme_Object **current_rs)
/* must have mutex_lock */
{
  Scheme_Thread *p;

  scheme_set_runstack_limits(MZ_RUNSTACK_START, 
                             fts->runstack_size,
                             (current_rs
                              ? current_rs XFORM_OK_MINUS MZ_RUNSTACK_START
                              : fts->runstack_size),
                             fts->runstack_size);
  p = scheme_current_thread;
  p->runstack = MZ_RUNSTACK;
  p->runstack_start = MZ_RUNSTACK_START;
  p->cont_mark_stack = MZ_CONT_MARK_STACK;
  p->cont_mark_pos = MZ_CONT_MARK_POS;

  /* FIXME: clear scheme_current_thread->ku.multiple.array ? */

  --fs->gc_not_ok;
  if (fs->need_gc_ok_post) {
    fs->need_gc_ok_post = 0;
    mzrt_sema_post(fs->gc_ok_c);
  }
}

void scheme_future_block_until_gc()
{
  Scheme_Future_State *fs = scheme_future_state;
  int i;

  if (!fs) return;
  if (!fs->future_threads_created) return;

  mzrt_mutex_lock(fs->future_mutex);
  fs->wait_for_gc = 1;
  mzrt_mutex_unlock(fs->future_mutex);

  for (i = 0; i < fs->thread_pool_size; i++) { 
    if (fs->pool_threads[i]) {
      *(fs->pool_threads[i]->need_gc_pointer) = 1;
      if (*(fs->pool_threads[i]->fuel_pointer)) {
        *(fs->pool_threads[i]->fuel_pointer) = 0;
        *(fs->pool_threads[i]->stack_boundary_pointer) += FUTURE_C_STACK_SIZE;
      }
    }
  }

  if (cpucount > 1) {
    /* In principle, we need some sort of fence to ensure that future
       threads see the change to the fuel pointer. The MFENCE
       instruction would do that, but it requires SSE2. The CPUID
       instruction is a non-privileged serializing instruction that
       should be available on any x86 platform that runs threads. */
#if defined(i386) || defined(__i386__) || defined(__x86_64) || defined(__x86_64__) || defined(__amd64__)
# ifdef _MSC_VER
    {
      int r[4];
      __cpuid(r, 0);
    }
# else
    {
#  if defined(i386) || defined(__i386__)
#   define MZ_PUSH_EBX "pushl %%ebx"
#   define MZ_POP_EBX "popl %%ebx"
#  endif
#  if defined(__x86_64) || defined(__x86_64__) || defined(__amd64__)
#   define MZ_PUSH_EBX "pushq %%rbx"
#   define MZ_POP_EBX "popq %%rbx"
#  endif
      int _eax, _ebx, _ecx, _edx, op = 0;
      /* we can't always use EBX, so save and restore it: */
      asm (MZ_PUSH_EBX "\n\t"
           "cpuid \n\t" 
           "movl %%ebx, %1 \n\t"
           MZ_POP_EBX
           : "=a" (_eax), "=r" (_ebx), "=c" (_ecx), "=d" (_edx) : "a" (op));
    }
#  undef MZ_PUSH_EBX
#  undef MZ_POP_EBX
# endif
#endif
  }

  mzrt_mutex_lock(fs->future_mutex);
  while (fs->gc_not_ok) {
    fs->need_gc_ok_post = 1;
    mzrt_mutex_unlock(fs->future_mutex);
    mzrt_sema_wait(fs->gc_ok_c);
    mzrt_mutex_lock(fs->future_mutex);
  }
  mzrt_mutex_unlock(fs->future_mutex);
}

void scheme_future_continue_after_gc()
{
  Scheme_Future_State *fs = scheme_future_state;
  int i;

  if (!fs) return;

  for (i = 0; i < fs->thread_pool_size; i++) {
    if (fs->pool_threads[i]) {
      *(fs->pool_threads[i]->need_gc_pointer) = 0;

      if (!fs->pool_threads[i]->thread->current_ft
          || scheme_custodian_is_available(fs->pool_threads[i]->thread->current_ft->cust)) {
        *(fs->pool_threads[i]->fuel_pointer) = 1;
        *(fs->pool_threads[i]->stack_boundary_pointer) -= FUTURE_C_STACK_SIZE;
      } else {
        /* leave fuel exhausted, which will force the thread into a slow 
           path when it resumes to suspend the computation */
      }
    }
  }

  mzrt_mutex_lock(fs->future_mutex);
  fs->wait_for_gc = 0;
  while (fs->need_gc_done_post) {
    --fs->need_gc_done_post;
    mzrt_sema_post(fs->gc_done_c);
  }
  mzrt_mutex_unlock(fs->future_mutex);
}

void scheme_future_gc_pause()
/* Called in future thread */
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  Scheme_Future_State *fs = scheme_future_state;

  mzrt_mutex_lock(fs->future_mutex); 
  end_gc_not_ok(fts, fs, MZ_RUNSTACK);
  start_gc_not_ok(fs); /* waits until wait_for_gc is 0 */
  mzrt_mutex_unlock(fs->future_mutex);
}

void scheme_future_check_custodians()
{
  scheme_future_block_until_gc();
  scheme_future_continue_after_gc();
}

int scheme_future_is_runtime_thread()
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  return fts->is_runtime_thread;
}

/**********************************************************************/
/* Future-event logging                                               */
/**********************************************************************/

static Scheme_Object *reset_future_logs_for_tracking(int argc, Scheme_Object **argv) 
{
  Scheme_Future_State *fs;
  Scheme_Future_Thread_State *fts;
  Scheme_Future_Thread_State *rt_fts;

  fs = scheme_future_state;
  rt_fts = scheme_future_thread_state;
  if (fs) {
    int i;
    mzrt_mutex_lock(fs->future_mutex);
    init_fevent(&fs->runtime_fevents);

    if (rt_fts) {
      init_fevent(&rt_fts->fevents1);
      init_fevent(&rt_fts->fevents2);
      rt_fts->use_fevents1 = 1;
    }

    for (i = 0; i < fs->thread_pool_size; i++) {
      fts = fs->pool_threads[i];
      if (fts) {
        init_fevent(&fts->fevents1);
        init_fevent(&fts->fevents2);
        fts->use_fevents1 = 1;
      }
    }  
    mzrt_mutex_unlock(fs->future_mutex);

  }

  return scheme_void;
}

static double get_future_timestamp() XFORM_SKIP_PROC {
#if 1
  return scheme_get_inexact_milliseconds();
#else
  return 0.0;
#endif
}

static void init_fevent(Fevent_Buffer *b) XFORM_SKIP_PROC
{
  if (b->a) free(b->a);

  b->pos = 0;
  b->overflow = 0;
  b->a = (Fevent *)malloc(FEVENT_BUFFER_SIZE * sizeof(Fevent));
  memset(b->a, 0, FEVENT_BUFFER_SIZE * sizeof(Fevent));
}

static void free_fevent(Fevent_Buffer *b)
{
  if (b->a) {
    free(b->a);
    b->a = NULL;
  }
}

static void record_fevent_with_data(int what, int fid, int data) 
  XFORM_SKIP_PROC 
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  Fevent_Buffer *b;
  
  if (!fts->is_runtime_thread) {
    if (fts->use_fevents1)
      b = &fts->fevents1;
    else
      b = &fts->fevents2;
  } else
    b = &scheme_future_state->runtime_fevents;
  
  b->a[b->pos].timestamp = get_future_timestamp();
  b->a[b->pos].what = what;
  b->a[b->pos].fid = fid;
  b->a[b->pos].data = data;

  b->pos++;
  if (b->pos == FEVENT_BUFFER_SIZE) {
    b->overflow = 1;
    b->pos = 0;
  }
}

static void record_fevent(int what, int fid) XFORM_SKIP_PROC
/* call with the lock or in the runtime thread */
{
  record_fevent_with_data(what, fid, 0);  
}

static void init_traversal(Fevent_Buffer *b)
{
  if (b->overflow) {
    b->count = FEVENT_BUFFER_SIZE;
    b->i = b->pos;
  } else {
    b->i = 0;
    b->count = b->pos;
  }
}

static void end_traversal(Fevent_Buffer *b)
{
  b->overflow = 0;
  b->pos = 0;
}

static void log_future_event(Scheme_Future_State *fs, 
                             const char *msg_str, 
                             const char *extra_str, 
                             int which, 
                             int what, 
                             double timestamp, 
                             int fid, 
                             Scheme_Object *user_data) 
{
  Scheme_Object *data, *v;
  Scheme_Logger *fl;

  fl = scheme_get_future_logger();
  if (!scheme_log_level_p(fl, SCHEME_LOG_DEBUG))
    return;

  data = scheme_make_blank_prefab_struct_instance(fs->fevent_prefab);
  if (what == FEVENT_MISSING || fid == NO_FUTURE_ID)
    ((Scheme_Structure *)data)->slots[0] = scheme_false;
  else
    ((Scheme_Structure *)data)->slots[0] = scheme_make_integer(fid);
  ((Scheme_Structure *)data)->slots[1] = scheme_make_integer((which+1));
  v = fs->fevent_syms[what];
  if (!v) {
    v = scheme_intern_symbol(fevent_strs[what]);
    fs->fevent_syms[what] = v;
  }
  ((Scheme_Structure *)data)->slots[2] = v;
  v = scheme_make_double(timestamp);
  ((Scheme_Structure *)data)->slots[3] = v;
  if (what == FEVENT_HANDLE_RTCALL || what == FEVENT_HANDLE_RTCALL_ATOMIC) {
    v = scheme_intern_symbol(extra_str);
    ((Scheme_Structure *)data)->slots[4] = v;
  } else 
    ((Scheme_Structure *)data)->slots[4] = scheme_false;

  /* User data (target fid for creates, alloc amount for allocation */
  if (!user_data) 
    user_data = scheme_false;

  ((Scheme_Structure *)data)->slots[5] = user_data;
  
  scheme_log_w_data(fl, SCHEME_LOG_DEBUG, 0,
                    data,                 
                    msg_str,
                    fid,
                    which+1,
                    fevent_long_strs[what],
                    extra_str,
                    timestamp);

}

static Scheme_Object *mark_future_trace_end(int argc, Scheme_Object **argv) 
{
  Scheme_Future_State *fs;
  fs = scheme_future_state;
  log_future_event(fs,
                   "id %d, process %d: %s: %s; time: %f",
                   "tracing",
                   -1, 
                   FEVENT_STOP_TRACE, 
                   get_future_timestamp(),
                   0, 
                   0);

  return scheme_void;
}

static void log_overflow_event(Scheme_Future_State *fs, int which, double timestamp)
{
  log_future_event(fs,
                   "id ??%-, process %d: %s%s; before time: %f",
                   "",
                   which, 
                   FEVENT_MISSING, 
                   timestamp, 
                   0, 
                   NULL);
}

static void flush_future_logs(Scheme_Future_State *fs)
{
  Scheme_Future_Thread_State *fts;
  double t, min_t;
  int i, min_which, min_set;
  Fevent_Buffer *b, *min_b;
  Scheme_Object *data_val;

  if (scheme_log_level_p(scheme_get_future_logger(), SCHEME_LOG_DEBUG)) {
    /* Hold lock while swapping buffers: */
    mzrt_mutex_lock(fs->future_mutex);
    for (i = 0; i < fs->thread_pool_size; i++) {
      fts = fs->pool_threads[i];
      if (fts) {
        fts->use_fevents1 = !fts->use_fevents1;
        if (fts->use_fevents1)
          b = &fts->fevents2;
        else
          b = &fts->fevents1;
        init_traversal(b);
      }
    }  
    mzrt_mutex_unlock(fs->future_mutex);
    init_traversal(&fs->runtime_fevents);

    if (fs->runtime_fevents.overflow)
      log_overflow_event(fs, -1, fs->runtime_fevents.a[fs->runtime_fevents.i].timestamp);
    for (i = 0; i < fs->thread_pool_size; i++) {
        fts = fs->pool_threads[i];
        if (fts) {
          if (fts->use_fevents1)
            b = &fts->fevents2;
          else
            b = &fts->fevents1;
          if (b->overflow)
            log_overflow_event(fs, i, b->a[b->i].timestamp);
        }
    }

    while (1) {
      min_set = 0;
      min_t = 0;
      min_b = NULL;
      min_which = -1;
      if (fs->runtime_fevents.count) {
        t = fs->runtime_fevents.a[fs->runtime_fevents.i].timestamp;
        if (!min_set || (t < min_t)) {
          min_t = t;
          min_b = &fs->runtime_fevents;
          min_set = 1;
        }
      }
      for (i = 0; i < fs->thread_pool_size; i++) {
        fts = fs->pool_threads[i];
        if (fts) {
          if (fts->use_fevents1)
            b = &fts->fevents2;
          else
            b = &fts->fevents1;
      
          if (b->count) {
            t = b->a[b->i].timestamp;
            if (!min_set || (t < min_t)) {
              min_t = t;
              min_b = b;
              min_which = i;
              min_set = 1;
            }
          }
        }
      }

      if (!min_b)
        break;

      data_val = scheme_make_integer(min_b->a[min_b->i].data);
      log_future_event(fs,
                       "id %d, process %d: %s%s; time: %f",
                       "",
                       min_which, 
                       min_b->a[min_b->i].what, 
                       min_b->a[min_b->i].timestamp, 
                       min_b->a[min_b->i].fid, 
                       data_val);

      --min_b->count;
      min_b->i++;
      if (min_b->i == FEVENT_BUFFER_SIZE)
        min_b->i = 0;
    }

    for (i = 0; i < fs->thread_pool_size; i++) {
      fts = fs->pool_threads[i];
      if (fts) {
        if (fts->use_fevents1)
          b = &fts->fevents2;
        else
          b = &fts->fevents1;
        end_traversal(b);
      }
    }  
    end_traversal(&fs->runtime_fevents);
  }
}

/**********************************************************************/
/* Primitive implementations                                          */
/**********************************************************************/

void scheme_wrong_contract_from_ft(const char *who, const char *expected_type, int what, int argc, Scheme_Object **argv);

#define SCHEME_WRONG_CONTRACT_MAYBE_IN_FT(who, expected_type, what, argc, argv) \
  if (scheme_use_rtcall) \
    scheme_wrong_contract_from_ft(who, expected_type, what, argc, argv); \
  else \
    scheme_wrong_contract(who, expected_type, what, argc, argv);


static Scheme_Object *make_future(Scheme_Object *lambda, int enqueue, future_t *cur_ft)
/* Called in runtime thread --- as atomic on behalf of a future thread
   if `lambda' is known to be a thunk */ 
{
  Scheme_Future_State *fs = scheme_future_state;
  int futureid;
  future_t *ft;
  Scheme_Native_Closure *nc;
  Scheme_Native_Closure_Data *ncd;
  Scheme_Custodian *c;

  if (SAME_TYPE(SCHEME_TYPE(lambda), scheme_native_closure_type)) {
    nc = (Scheme_Native_Closure*)lambda;
    ncd = nc->code;
  } else {
    nc = NULL;
    ncd = NULL;
  }

  /* Create the future descriptor and add to the queue as 'pending' */
  ft = MALLOC_ONE_TAGGED(future_t);     
  ft->so.type = scheme_future_type;

  ft->orig_lambda = lambda;
  ft->status = PENDING;

  if (scheme_current_thread->mref)
    c = scheme_custodian_extract_reference(scheme_current_thread->mref);
  else {
    /* must be in a future thread (if futures can be created in future threads) */
    c = scheme_current_thread->current_ft->cust;
  }
  ft->cust = c;
   
  /* JIT the code if not already JITted */
  if (ncd) {
    scheme_jit_now(lambda);
  
    if (ncd->max_let_depth > FUTURE_RUNSTACK_SIZE * sizeof(void*)) {
      /* Can't even call it in a future thread */
      ft->status = PENDING_OVERSIZE;
    }
  } else
    ft->status = PENDING_OVERSIZE;

  mzrt_mutex_lock(fs->future_mutex);
  futureid = ++fs->next_futureid;
  ft->id = futureid;
  record_fevent_with_data(FEVENT_CREATE, (cur_ft ? cur_ft->id : NO_FUTURE_ID), futureid);
  if (enqueue) { 
    if (ft->status != PENDING_OVERSIZE)
      enqueue_future(fs, ft);
  }

  mzrt_mutex_unlock(fs->future_mutex);
  if (enqueue)
    check_future_thread_creation(fs);

  return (Scheme_Object*)ft;
}

int scheme_can_apply_native_in_future(Scheme_Object *proc)
  XFORM_SKIP_PROC /* can be called from future thread */
{
  return (((Scheme_Native_Closure *)proc)->code->max_let_depth < FUTURE_RUNSTACK_SIZE * sizeof(void*));
}

static Scheme_Object *do_make_future(int argc, Scheme_Object *argv[], future_t *cur_ft)
{
  Scheme_Future_State *fs;
  scheme_check_proc_arity("future", 0, 0, argc, argv);
  
  fs = scheme_future_state;
  flush_future_logs(fs);

  return make_future(argv[0], 1, cur_ft);
}

Scheme_Object *scheme_future(int argc, Scheme_Object *argv[])
  XFORM_SKIP_PROC /* can be called from future thread */
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  if (fts->is_runtime_thread) {
    return do_make_future(argc, argv, (scheme_current_thread ? scheme_current_thread->current_ft : NULL));
  }
  else {
    Scheme_Object *proc = argv[0];
#ifdef MZ_PRECISE_GC
    if (SAME_TYPE(SCHEME_TYPE(proc), scheme_native_closure_type)
        && scheme_native_arity_check(proc, 0)
        && (((Scheme_Native_Closure *)proc)->code->start_code != scheme_on_demand_jit_code)
        && scheme_can_apply_native_in_future(proc)) {
      /* try to alocate a future in the future thread */
      future_t *ft;
      ft = MALLOC_ONE_TAGGED(future_t);
      if (ft) {
        future_t *cur_ft = scheme_current_thread->current_ft;
        Scheme_Future_State *fs = scheme_future_state;

        ft->so.type = scheme_future_type;
        ft->orig_lambda = proc;
        ft->status = PENDING;
        ft->cust = scheme_current_thread->current_ft->cust;

        mzrt_mutex_lock(fs->future_mutex);
        ft->id = ++fs->next_futureid;
        record_fevent_with_data(FEVENT_CREATE, (cur_ft ? cur_ft->id : NO_FUTURE_ID), ft->id);
        enqueue_future(fs, ft);
        mzrt_mutex_unlock(fs->future_mutex);

        return (Scheme_Object *)ft;
      } else {
        /* It would be nice to encourage allocation of a page for
           the future thread in this case, since it might try to
           allocate more futures. */
        return scheme_rtcall_make_future(proc);
      }
    } else {
      return scheme_rtcall_make_future(proc);
    }
#else
    /* future-local allocation is not supported */
    return scheme_rtcall_make_future(proc);
#endif
  }
}

static Scheme_Object *would_be_future(int argc, Scheme_Object *argv[]) 
  /* Called in runtime thread */
{
  future_t *ft;
  Scheme_Future_Thread_State *fts;
  scheme_check_proc_arity("would-be-future", 0, 0, argc, argv);
  fts = scheme_future_thread_state;  

  ft = (future_t*)make_future(argv[0], 0, (fts->thread ? fts->thread->current_ft : NULL));
  ft->in_tracing_mode = 1;
  ft->fts = scheme_future_thread_state;

  return (Scheme_Object*)ft;
}

static void run_would_be_future(future_t *ft)
{
  mz_jmp_buf newbuf, *savebuf;
  Scheme_Thread *p;
  Scheme_Future_State *fs;
  int aborted = 0;
  
  p = scheme_current_thread;
  fs = scheme_future_state;
  
  /* Setup the future thread state */
  p->futures_slow_path_tracing++;
  scheme_use_rtcall++;

  savebuf = p->error_buf;
  p->error_buf = &newbuf;

  /* Run the future */
  if (scheme_setjmp(newbuf)) { 
    aborted = 1;
  } else { 
    future_in_runtime(fs, ft, FEVENT_START_WORK);
  }

  scheme_use_rtcall--;
  p->futures_slow_path_tracing--;
  ft->in_tracing_mode = 0;

  p->error_buf = savebuf;
  if (aborted) 
    scheme_longjmp(*savebuf, 1);

  return;
}

void fsemaphore_finalize(void *p, void *data)
{
  fsemaphore_t *sema;
  sema = (fsemaphore_t*)p;
  mzrt_mutex_destroy(sema->mut);
}

Scheme_Object *scheme_make_fsemaphore_inl(Scheme_Object *ready)
/* Called in runtime thread */
{
  fsemaphore_t *sema;
  intptr_t v;

  v = scheme_get_semaphore_init("make-fsemaphore", 1, &ready);

  sema = MALLOC_ONE_TAGGED(fsemaphore_t);
  sema->so.type = scheme_fsemaphore_type;
  
  mzrt_mutex_create(&sema->mut);
  sema->ready = v;

  scheme_register_finalizer((void*)sema, fsemaphore_finalize, NULL, NULL, NULL);

  return (Scheme_Object*)sema;
}


Scheme_Object *make_fsemaphore(int argc, Scheme_Object **argv)
  /* Called in runtime thread (atomic/synchronized) */
{
  return scheme_make_fsemaphore_inl(argv[0]);
}

Scheme_Object *scheme_fsemaphore_count(int argc, Scheme_Object **argv)
  XFORM_SKIP_PROC 
{
  fsemaphore_t *sema;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_fsemaphore_type)) { 
    SCHEME_WRONG_CONTRACT_MAYBE_IN_FT("fsemaphore-count", "fsemaphore?", 0, argc, argv);
  }

  sema = (fsemaphore_t*)argv[0]; 
  return scheme_make_integer(sema->ready);
}

static void requeue_future_within_lock(future_t *future, Scheme_Future_State *fs) 
  XFORM_SKIP_PROC
/* called in any thread with lock held */
{
  if (scheme_custodian_is_available(future->cust)) {
    future->status = PENDING;
    enqueue_future(fs, future);
  } else {
    /* The future's constodian is shut down, so don't try to
       run it in a future thread anymore */
    future->status = SUSPENDED;
  }
}

static void requeue_future(future_t *future, Scheme_Future_State *fs) 
{
  mzrt_mutex_lock(fs->future_mutex);
  requeue_future_within_lock(future, fs);
  mzrt_mutex_unlock(fs->future_mutex);
}

static int try_resume_future_from_fsema_wait(fsemaphore_t *sema, Scheme_Future_State *fs) 
  XFORM_SKIP_PROC
{
  future_t *ft;

  if (!sema->queue_front)
    return 0;

  ft = sema->queue_front;
  sema->queue_front = ft->next_in_fsema_queue;
  ft->next_in_fsema_queue = NULL;

  if (!sema->queue_front)
    sema->queue_end = NULL;
  else
    sema->queue_front->prev_in_fsema_queue = NULL;

  sema->ready--;

  ft->retval_s = scheme_void;
    
  /* Place the waiting future back on the run queue */
  requeue_future(ft, fs);

  return 1;
}

Scheme_Object *scheme_fsemaphore_post(int argc, Scheme_Object **argv)
  XFORM_SKIP_PROC
{
  fsemaphore_t *sema;
  Scheme_Future_State *fs;
  int old_count;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_fsemaphore_type)) { 
    SCHEME_WRONG_CONTRACT_MAYBE_IN_FT("fsemaphore-post", "fsemaphore?", 0, argc, argv);
  }

  sema = (fsemaphore_t*)argv[0];

#ifdef FSEMA_LOGGING 
  printf("[Thread %p]: scheme_fsemaphore_post for sema at %p. Count before V: %d\n", 
    pthread_self(), 
    sema, 
    sema->ready);
#endif

  fs = scheme_future_state;
  mzrt_mutex_lock(sema->mut);

  old_count = sema->ready;
  sema->ready++;
  if (!old_count) { 
    try_resume_future_from_fsema_wait(sema, fs);
  }

  mzrt_mutex_unlock(sema->mut);
  return scheme_void;
}

static void enqueue_future_for_fsema(future_t *ft, fsemaphore_t *sema) 
  /* This function assumed sema->mut has already been acquired! */
{
  future_t *front;

  /* Enqueue this future in the semaphore's queue */ 
  front = sema->queue_front;
  if (!front) { 
    sema->queue_front = ft;
    sema->queue_end = ft;
  } else {
    future_t *end = sema->queue_end;
    end->next_in_fsema_queue = ft;
    ft->prev_in_fsema_queue = end;
    sema->queue_end = ft;
  }
}

static fsemaphore_t *block_until_sema_ready(fsemaphore_t *sema)
{
  /* This little function cooperates with the GC, unlike the
     function that calls it. */
  scheme_block_until(fsemaphore_ready, NULL, (Scheme_Object*)sema, 0);
  return sema;
}

Scheme_Object *scheme_fsemaphore_wait(int argc, Scheme_Object **argv)
  XFORM_SKIP_PROC
{
  fsemaphore_t *sema;
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  Scheme_Future_State *fs = scheme_future_state;
  void *storage[4];

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_fsemaphore_type)) { 
    SCHEME_WRONG_CONTRACT_MAYBE_IN_FT("fsemaphore-wait", "fsemaphore?", 0, argc, argv);
  }

  sema = (fsemaphore_t*)argv[0];

#ifdef FSEMA_LOGGING 
  printf("[Thread %p]: scheme_fsemaphore_wait for sema at %p. Count before P: %d\n", 
    pthread_self(), 
    sema, 
    sema->ready);
#endif

  mzrt_mutex_lock(sema->mut);
  if (!sema->ready) { 
    if (fts->is_runtime_thread) { 
      /* Then we are on the runtime thread -- if in would-be-future 
          mode, should we just return?  Otherwise, block and wait 
          for the fsema to be ready while cooperating with the 
          scheduler */ 
      if (scheme_current_thread->futures_slow_path_tracing) { 
        mzrt_mutex_unlock(sema->mut);
        return scheme_void;
      }

      mzrt_mutex_unlock(sema->mut);
      sema = block_until_sema_ready(sema);
      mzrt_mutex_lock(sema->mut);
    } else {
      /* On a future thread, suspend the future (to be 
        resumed whenever the fsema becomes ready */
      future_t *future = fts->thread->current_ft;
      jit_future_storage[0] = (void*)sema;
      jit_future_storage[1] = (void*)future;
      if (!future) { 
        /* Should never be here */
        scheme_log_abort("fsemaphore-wait: future was NULL for future thread.");
        abort();
      }

      /* Setup for LWC capture */
      mzrt_mutex_unlock(sema->mut);
      scheme_fill_lwc_end();
      future->lwc = scheme_current_lwc;
      future->fts = fts;
      future->prim_protocol = SIG_s_s;

      /* Try to capture it locally (on this thread) */
      if (GC_gen0_alloc_page_ptr 
          && capture_future_continuation(fs, future, storage, 0, 0)) {
        /* capture sets fts->thread->current_ft to NULL */
        mzrt_mutex_lock(fs->future_mutex);
      } else { 
        /* Can't capture the continuation locally, so ask the runtime 
            thread to do it */
        mzrt_mutex_lock(fs->future_mutex);
        if (!future->in_queue_waiting_for_lwc) { 
          future->next_waiting_lwc = fs->future_waiting_lwc;
          fs->future_waiting_lwc = future;
          future->in_queue_waiting_for_lwc = 1;
        }
        future->want_lw = 1;
      }   
      future->status = WAITING_FOR_FSEMA;

      scheme_signal_received_at(fs->signal_handle);
      if (fts->thread->current_ft) { 
        /* Will get here if relying on runtime thread to capture for us -- 
            wait for the signal that LW continuation was captured 
            by the runtime thread. */ 
        future->can_continue_sema = fts->worker_can_continue_sema;
        end_gc_not_ok(fts, fs, MZ_RUNSTACK);
        mzrt_mutex_unlock(fs->future_mutex);
      
        mzrt_sema_wait(fts->worker_can_continue_sema);

        mzrt_mutex_lock(fs->future_mutex);
        start_gc_not_ok(fs); 
      }
      mzrt_mutex_unlock(fs->future_mutex);

      FUTURE_ASSERT(!fts->thread->current_ft);

      /* Fetch the future and sema pointers again, in case moved by a GC */
      sema = (fsemaphore_t*)jit_future_storage[0];
      future = (future_t*)jit_future_storage[1];

      FUTURE_ASSERT(future->suspended_lw);
      FUTURE_ASSERT(!future->can_continue_sema);

      /* Check again to see whether the sema has become ready */ 
      mzrt_mutex_lock(sema->mut);
      if (sema->ready) { 
        /* Then resume the future immediately (requeue) */
        sema->ready--;
        requeue_future(future, fs);
      } else {
        /* Add the future to the sema's wait queue */ 
        enqueue_future_for_fsema(future, sema);
      }

      mzrt_mutex_unlock(sema->mut);
      
      /* Jump back to the worker thread future loop (this thread 
          is now free */ 
      scheme_future_longjmp(*scheme_current_thread->error_buf, 1);
    }
  } 

  /* Semaphore is ready -- decrement and continue */
  sema->ready--;
  mzrt_mutex_unlock(sema->mut);
  return scheme_void;
}

Scheme_Object *scheme_fsemaphore_try_wait(int argc, Scheme_Object **argv)
  XFORM_SKIP_PROC
{
  fsemaphore_t *sema;
  Scheme_Object *ret;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_fsemaphore_type)) { 
    SCHEME_WRONG_CONTRACT_MAYBE_IN_FT("fsemaphore-try-wait?", "fsemaphore?", 0, argc, argv);
  }

  sema = (fsemaphore_t*)argv[0];
  mzrt_mutex_lock(sema->mut);
  if (!sema->ready) { 
    ret = scheme_false;
  } else { 
    sema->ready--;
    ret = scheme_true;
  }

  mzrt_mutex_unlock(sema->mut);
  return ret;
}

static int fsemaphore_ready(Scheme_Object *obj) 
/* Called in runtime thread by Racket scheduler */
{
  int ret = 0;
  fsemaphore_t *fsema = (fsemaphore_t*)obj;
  mzrt_mutex_lock(fsema->mut);
  ret = fsema->ready;
  mzrt_mutex_unlock(fsema->mut);
  return ret;
}


int future_ready(Scheme_Object *obj)
/* Called in runtime thread by Racket scheduler */
{
  Scheme_Future_State *fs = scheme_future_state;
  int ret = 0;
  future_t *ft = (future_t*)obj;

  mzrt_mutex_lock(fs->future_mutex);
  if ((ft->status != RUNNING)
      && (ft->status != WAITING_FOR_FSEMA)
      && (ft->status != HANDLING_PRIM)) {
    ret = 1;
  }
  mzrt_mutex_unlock(fs->future_mutex);

  return ret;
}

static void dequeue_future(Scheme_Future_State *fs, future_t *ft)
  XFORM_SKIP_PROC
/* called from both future and runtime threads */
{
  if (ft->prev == NULL)
    fs->future_queue = ft->next;
  else
    ft->prev->next = ft->next;
  
  if (ft->next == NULL)
    fs->future_queue_end = ft->prev;
  else
    ft->next->prev = ft->prev;

  ft->next = NULL;
  ft->prev = NULL;

  --fs->future_queue_count;
}

static void complete_rtcall(Scheme_Future_State *fs, future_t *future)
  XFORM_SKIP_PROC
/* called in any thread with lock held */
{
  if (future->suspended_lw) {
    /* Re-enqueue the future so that some future thread can continue */
    requeue_future_within_lock(future, fs);
  } else {
    /* Signal the waiting worker thread that it
       can continue running machine code */
    future->want_lw = 0; /* needed if we get here via direct_future_to_future_touch() */
    if (future->can_continue_sema) {
      mzrt_sema *can_continue_sema = future->can_continue_sema;
      future->can_continue_sema = NULL;
      mzrt_sema_post(can_continue_sema);
    }
  }
}

static void direct_future_to_future_touch(Scheme_Future_State *fs, future_t *ft, future_t *t_ft)
  XFORM_SKIP_PROC
/* called in any thread with lock held */
{
  Scheme_Object *retval = ft->retval;

  receive_special_result(ft, retval, 0);
  t_ft->retval_s = retval;
  send_special_result(t_ft, retval);

  t_ft->arg_S1 = NULL;
  t_ft->status = HANDLING_PRIM; /* handled as if by runtime thread */

  complete_rtcall(fs, t_ft);
}

static future_t *get_future_for_touch(future_t *ft)
  XFORM_SKIP_PROC
/* called in any thread with lock held */
{
  if ((ft->status == WAITING_FOR_PRIM) && (ft->prim_func == touch)) {
    /* try to enqueue it... */
    Scheme_Object **a = ft->arg_S1;
    if (ft->suspended_lw)
      a = scheme_adjust_runstack_argument(ft->suspended_lw, a);
    return (future_t *)a[0];
  } else
    return NULL;
}

static void trigger_added_touches(Scheme_Future_State *fs, future_t *ft)
  XFORM_SKIP_PROC
/* lock held; called from both future and runtime threads */ 
{
  if (ft->touching) {
    Scheme_Object *touching = ft->touching;
    ft->touching = NULL;
    while (!SCHEME_NULLP(touching)) {
      Scheme_Object *wb = SCHEME_CAR(touching);
      future_t *t_ft = (future_t *)SCHEME_WEAK_BOX_VAL(wb);

      if (t_ft && (get_future_for_touch(t_ft) == ft)) {
        direct_future_to_future_touch(fs, ft, t_ft);
      }

      touching = SCHEME_CDR(touching);
    }
  }
}

static Scheme_Object *shallower_apply_future_lw_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  future_t *ft = (future_t *)p->ku.k.p1;

  p->ku.k.p1 = NULL;

  return apply_future_lw(ft);
}

static int future_in_runtime(Scheme_Future_State *fs, future_t * volatile ft, int what)
{
  mz_jmp_buf newbuf, * volatile savebuf;
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object * volatile retval;
  future_t * volatile old_ft;
  int done;

  old_ft = p->current_ft;
  p->current_ft = ft;

  savebuf = p->error_buf;
  p->error_buf = &newbuf;

  record_fevent(what, ft->id);

  if (scheme_setjmp(newbuf)) {
    ft->no_retval = 1;
    retval = NULL;
  } else {
    if (ft->suspended_lw) {
      if (scheme_can_apply_lightweight_continuation(ft->suspended_lw, 1) > 1) {
        p->ku.k.p1 = ft;
        retval = scheme_handle_stack_overflow(shallower_apply_future_lw_k);
      } else
        retval = apply_future_lw(ft);
    } else {
      if (ft->suspended_lw_stack) {
        Scheme_Object *rator, **argv;
        int argc;
        Scheme_Lightweight_Continuation *lc;
        rator = (Scheme_Object *)ft->suspended_lw_stack[2];
        argc = SCHEME_INT_VAL((Scheme_Object *)ft->suspended_lw_stack[3]);
        argv = (Scheme_Object **)ft->suspended_lw_stack[4];
        ft->suspended_lw_stack[2] = NULL;
        ft->suspended_lw_stack[4] = NULL;

        lc = (Scheme_Lightweight_Continuation *)ft->suspended_lw_stack[1];
        scheme_restore_lightweight_continuation_marks(lc);

        if (ft->suspended_lw_stack[5])
          retval = _scheme_apply_multi(rator, argc, argv);
        else
          retval = _scheme_apply(rator, argc, argv);
      } else
        retval = scheme_apply_multi(ft->orig_lambda, 0, NULL);
    }
    send_special_result(ft, retval);
  }

  p->error_buf = savebuf;
  p->current_ft = old_ft;

  ft->retval = retval;

  mzrt_mutex_lock(fs->future_mutex);

  if (ft->suspended_lw_stack && retval) {
    pop_suspended_lw(fs, ft);
    done = 0;
  } else {
    if (!retval)
      ft->suspended_lw_stack = NULL;
    ft->status = FINISHED;
    trigger_added_touches(fs, ft);
    done = 1;
  }
  record_fevent(FEVENT_COMPLETE, ft->id);
  mzrt_mutex_unlock(fs->future_mutex);

  record_fevent(FEVENT_END_WORK, ft->id);

  if (!retval) {
    scheme_longjmp(*savebuf, 1);
  }

  return done;
}

static int prefer_to_apply_future_in_runtime()
/* Called with the future mutex held. */
{
  /* Policy question: if the runtime thread is blocked on a
     future, should we just run the future (or its suspended continuation)
     directly in the runtime thread?

     If we don't, then we're better able to handle non-blocking requests
     from future threads. At the same time, the runtime thread shouldn't
     wait if no one is working on the future. We err on the safe side
     and always run when we're waiting on the future: */
  return 1;
}

Scheme_Object *general_touch(int argc, Scheme_Object *argv[])
/* Called in runtime thread */
{
  Scheme_Future_State *fs = scheme_future_state;
  Scheme_Object *retval = NULL;
  future_t *ft;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_future_type))
    scheme_wrong_contract("touch", "future?", 0, argc, argv);

  ft = (future_t*)argv[0];

  /* Spin waiting for primitive calls or a return value from
     the worker thread */
  while (1) {
    mzrt_mutex_lock(fs->future_mutex);
    if ((((ft->status == PENDING)
          && prefer_to_apply_future_in_runtime())
         || (ft->status == PENDING_OVERSIZE)
         || (ft->status == SUSPENDED))
        && (!ft->suspended_lw
            || scheme_can_apply_lightweight_continuation(ft->suspended_lw, 0))) 
      {
        int what = FEVENT_START_WORK;
        if (ft->status == PENDING_OVERSIZE) {
          what = FEVENT_START_RTONLY_WORK;
        } else if (ft->status != SUSPENDED) {
          dequeue_future(fs, ft);
          if (ft->suspended_lw_stack)
            what = FEVENT_RESUME_WORK;
        }
        ft->status = RUNNING;
        mzrt_mutex_unlock(fs->future_mutex);
        if (ft->in_tracing_mode) {
          run_would_be_future(ft);
          retval = ft->retval;
          break;
        } else {
          if (future_in_runtime(fs, ft, what)) {
            retval = ft->retval;
            break;
          }
        }
      } 
    else if ((ft->status == RUNNING)
        || (ft->status == WAITING_FOR_FSEMA)
        || (ft->status == HANDLING_PRIM)) 
      {
        /* someone else got to it first */
        mzrt_mutex_unlock(fs->future_mutex);
      } 
    else if (ft->status == FINISHED)
      {
        retval = ft->retval;

        mzrt_mutex_unlock(fs->future_mutex);
        
        break;
      }
    else if (ft->status == WAITING_FOR_PRIM)
      {
        /* Invoke the primitive and stash the result.
           Release the lock so other threads can manipulate the queue
           while the runtime call executes. */
        ft->status = HANDLING_PRIM;
        ft->want_lw = 0;
        mzrt_mutex_unlock(fs->future_mutex);
        invoke_rtcall(fs, ft, 0);
      }
    else if (ft->maybe_suspended_lw && (ft->status != WAITING_FOR_FSEMA))
      {
        ft->maybe_suspended_lw = 0;
        if (ft->suspended_lw) {
          if (scheme_can_apply_lightweight_continuation(ft->suspended_lw, 0)
              && prefer_to_apply_future_in_runtime()) {
            if (ft->status != SUSPENDED)
              dequeue_future(fs, ft);
            ft->status = RUNNING;
            /* may raise an exception or escape: */
            mzrt_mutex_unlock(fs->future_mutex);
            (void)future_in_runtime(fs, ft, FEVENT_START_WORK);
          } else {
            /* Someone needs to handle the future. We're banking on some
               future thread eventually picking up the future, which is
               not actually guaranteed if they're all busy looping... */
            mzrt_mutex_unlock(fs->future_mutex);
          }
        } else {
          mzrt_mutex_unlock(fs->future_mutex);
        }
      }
    else
      {
        mzrt_mutex_unlock(fs->future_mutex);
      }

    scheme_thread_block(0.0); /* to ensure check for futures work */

    record_fevent(FEVENT_TOUCH_PAUSE, ft->id);
    scheme_block_until(future_ready, NULL, (Scheme_Object*)ft, 0);
    record_fevent(FEVENT_TOUCH_RESUME, ft->id);
  }

  if (!retval) {
    scheme_signal_error("touch: future previously aborted");
  }

  receive_special_result(ft, retval, 0);

  flush_future_logs(fs);

  return retval;
}

Scheme_Object *touch(int argc, Scheme_Object *argv[])
  XFORM_SKIP_PROC
/* can be called in future thread */
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  if (fts->is_runtime_thread) {
    future_t *ft;
    if (fts->thread 
        && (ft = fts->thread->current_ft) 
        && ft->in_tracing_mode) { 
      future_t *targ_ft = (future_t*)argv[0];
      Scheme_Future_State *fs = scheme_future_state;
      Scheme_Object *targid_obj;
      targid_obj = scheme_make_integer(targ_ft->id);
      log_future_event(fs,
                       "id %d, process %d: %s: %s; time: %f",
                       "touch",
                       -1, 
                       FEVENT_RTCALL_TOUCH, 
                       get_future_timestamp(),
                       ft->id, 
                       targid_obj);
    }
      
    return general_touch(argc, argv);
  } else {
    if (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_future_type)) {
      Scheme_Future_State *fs = scheme_future_state;
      future_t *ft = (future_t *)argv[0];
      int status;

      mzrt_mutex_lock(fs->future_mutex);      
      status = ft->status;
      mzrt_mutex_unlock(fs->future_mutex);

      if (status == FINISHED) {
        Scheme_Object *retval = ft->retval;
        receive_special_result(ft, retval, 0);
        return retval;
      } else {
#ifdef MZ_PRECISE_GC
        /* Try adding current future to ft's chain of touching futures */
        Scheme_Object *wb, *pr;
        future_t *current_ft = scheme_current_thread->current_ft;

        wb = GC_malloc_weak_box(current_ft, NULL, 0, 0);
        if (wb) {
          pr = GC_malloc_pair(wb, scheme_null);
          if (pr) {
            mzrt_mutex_lock(fs->future_mutex);
            if (ft->status != FINISHED) {
              if (ft->touching)
                SCHEME_CDR(pr) = ft->touching;
              ft->touching = pr;
              current_ft->in_future_specific_touch_queue = 1;
              mzrt_mutex_unlock(fs->future_mutex);
            } else {
              /* `ft' switched to FINISHED while we were trying add,
                 so carry on with its result */
              Scheme_Object *retval = ft->retval;
              mzrt_mutex_unlock(fs->future_mutex);
              receive_special_result(ft, retval, 0);
              return retval;
            }
          }
        }
#endif
      }
    }
    return scheme_rtcall_iS_s("touch", FSRC_PRIM, touch, argc, argv);
  }
}

#if defined(__linux__) || defined(__QNX__)
# include <unistd.h>
#elif defined(OS_X)
# include <sys/param.h>
# include <sys/sysctl.h>
#elif defined(DOS_FILE_SYSTEM)
# include <windows.h>
#endif 

static void init_cpucount(void)
/* Called in runtime thread */
{
#if defined(__linux__) || defined(__QNX__)
  cpucount = sysconf(_SC_NPROCESSORS_ONLN);
#elif defined(OS_X)
  size_t size = sizeof(cpucount);

  if (sysctlbyname("hw.ncpu", &cpucount, &size, NULL, 0))
    cpucount = 2;
#elif defined(DOS_FILE_SYSTEM)
  SYSTEM_INFO sysinfo;
  GetSystemInfo(&sysinfo);
  cpucount = sysinfo.dwNumberOfProcessors;
#else
  /* Conservative guess! */
  /* A result of 1 is not conservative, because claiming a
     uniprocessor means that atomic cmpxchg operations are not used
     for setting pair flags and hash codes. */
  cpucount = 2;
#endif
}

int scheme_is_multithreaded(int now)
{
  if (!now)
    return 1;
  else {
    Scheme_Future_State *fs = scheme_future_state;
    return (fs && fs->future_threads_created);
  }
}

Scheme_Object *processor_count(int argc, Scheme_Object *argv[])
/* Called in runtime thread */
{
  return scheme_make_integer(cpucount);
}

Scheme_Object *scheme_current_future(int argc, Scheme_Object *argv[])
  XFORM_SKIP_PROC
/* Called from any thread (either runtime or future) */
{
  future_t *ft = scheme_current_thread->current_ft;

  return (ft ? (Scheme_Object *)ft : scheme_false);
}

/* Entry point for a worker thread allocated for
   executing futures.  This function will never terminate
   (until the process dies). */
void *worker_thread_future_loop(void *arg)
  XFORM_SKIP_PROC
/* Called in future thread; runtime thread is blocked until ready_sema
  is signaled. */
{
  /* valid only until signaling */
  future_thread_params_t *params = (future_thread_params_t *)arg;
  Scheme_Future_Thread_State *fts = params->fts;
  Scheme_Future_State *fs = params->fs;
  Scheme_Object *v;
  Scheme_Native_Proc *jitcode;
  future_t *ft;
  mz_jmp_buf newbuf;
  int fid, what;

  scheme_future_state = fs;
  scheme_future_thread_state = fts;

  GC_instance = params->shared_GC;

  GC_gen0_alloc_only = 1;

  /* Set processor affinity */
  /*mzrt_mutex_lock(fs->future_mutex);
      static uintptr_t cur_cpu_mask = 1;
    if (pthread_setaffinity_np(pthread_self(), sizeof(g_cur_cpu_mask), &g_cur_cpu_mask))
    {
    printf(
    "Could not set CPU affinity (%lu) for thread %p!\n", 
    ++g_cur_cpu_mask, 
    pthread_self());
    }

    mzrt_mutex_unlock(fs->future_mutex);
  */

  scheme_configure_floating_point();

  mzrt_sema_create(&fts->worker_can_continue_sema, 0);

  scheme_use_rtcall = 1;

  scheme_current_thread = fts->thread;

  scheme_fuel_counter = 1;
  scheme_jit_stack_boundary = ((uintptr_t)&v) - FUTURE_C_STACK_SIZE;

  fts->need_gc_pointer = &scheme_future_need_gc_pause;
  fts->fuel_pointer = &scheme_fuel_counter;
  fts->stack_boundary_pointer = &scheme_jit_stack_boundary;

  MZ_RUNSTACK_START = params->runstack_start;
  MZ_RUNSTACK = MZ_RUNSTACK_START + fts->runstack_size;

  params->scheme_current_runstack_ptr = &scheme_current_runstack;
  params->scheme_current_runstack_start_ptr = &scheme_current_runstack_start;
  params->current_thread_ptr = &scheme_current_thread;
  params->jit_future_storage_ptr = &jit_future_storage[0];

  scheme_init_thread_lwc();
  params->lwc = scheme_current_lwc;

  mzrt_sema_post(params->ready_sema);

  while (1) {
    mzrt_sema_wait(fs->future_pending_sema);
    mzrt_mutex_lock(fs->future_mutex);
    start_gc_not_ok(fs);

    ft = get_pending_future(fs);

    if (ft) {
      fs->busy_thread_count++;

      if (ft->suspended_lw_stack)
        what = FEVENT_RESUME_WORK;
      else
        what = FEVENT_START_WORK;
      fid = ft->id;
      record_fevent(what, fid);

      /* Work is available for this thread */
      ft->status = RUNNING;
      ft->maybe_suspended_lw = 0;
      mzrt_mutex_unlock(fs->future_mutex);

      ft->thread_short_id = fts->id;

      /* Set up the JIT compiler for this thread  */
      scheme_jit_fill_threadlocal_table();

      fts->thread->current_ft = ft;

      MZ_RUNSTACK = MZ_RUNSTACK_START + fts->runstack_size;
      MZ_CONT_MARK_STACK = 0;
      MZ_CONT_MARK_POS = (MZ_MARK_POS_TYPE)1;

      if (ft->suspended_lw) {
        /* invoke a lightweight continuation */
        scheme_current_thread->error_buf = &newbuf;
        if (scheme_future_setjmp(newbuf)) {
          /* failed or suspended */
          v = NULL;
        } else {
          v = _apply_future_lw(ft);
        }
      } else {
        /* Run the code:
           The lambda passed to a future will always be a parameterless
           function.
           From this thread's perspective, this call will never return
           until all the work to be done in the future has been completed,
           including runtime calls. 
           If jitcode asks the runtime thread to do work, then
           a GC can occur. */

        scheme_current_thread->error_buf = &newbuf;
        if (scheme_future_setjmp(newbuf)) {
          /* failed or suspended */
          v = NULL;
        } else {
          Scheme_Object *rator, **argv;
          int argc;

          if (ft->suspended_lw_stack) {
            Scheme_Lightweight_Continuation *lc;

            lc = (Scheme_Lightweight_Continuation *)ft->suspended_lw_stack[1];
            scheme_restore_lightweight_continuation_marks(lc); /* might trigger GC */
            ft = fts->thread->current_ft;

            rator = (Scheme_Object *)ft->suspended_lw_stack[2];
            argc = SCHEME_INT_VAL((Scheme_Object *)ft->suspended_lw_stack[3]);
            argv = (Scheme_Object **)ft->suspended_lw_stack[4];
            ft->suspended_lw_stack[2] = NULL;
            ft->suspended_lw_stack[4] = NULL;
          } else {
            rator = ft->orig_lambda;
            argc = 0;
            argv = NULL;
          }

          scheme_fill_lwc_start();
          jitcode = ((Scheme_Native_Closure *)rator)->code->start_code;
          v = scheme_call_as_lightweight_continuation(jitcode, rator, argc, argv);
          if (SAME_OBJ(v, SCHEME_TAIL_CALL_WAITING))
            v = scheme_force_value_same_mark_as_lightweight_continuation(v);
        }
      }

      /* Get future again, since a GC may have occurred or
         future may have been suspended */
      ft = fts->thread->current_ft;

      mzrt_mutex_lock(fs->future_mutex);

      if (!ft) {
        /* continuation of future will be requeued, and this future
           thread can do something else */
      } else {
        FUTURE_ASSERT(v || ft->no_retval);

        if (ft->no_retval >= 0) {
          /* Set the return val in the descriptor */
          ft->retval = v;
          /* In case of multiple values: */
          send_special_result(ft, v);

          if (ft->suspended_lw_stack) {
            if (!ft->suspended_lw_stack[5] && SAME_OBJ(v, SCHEME_MULTIPLE_VALUES)) {
              /* multiple results are not allowed; keep the same lw stack,
                 but replace the function to call: */
              ft->status = PENDING_OVERSIZE;
              ft->suspended_lw_stack[2] = bad_multi_result_proc;
              ft->suspended_lw_stack[3] = scheme_make_integer(ft->multiple_count);
              ft->suspended_lw_stack[4] = ft->multiple_array;
              ft->retval_s = NULL;
              ft->multiple_array = NULL;
            } else
              pop_suspended_lw(fs, ft);
          } else {
            /* Update the status */
            ft->status = FINISHED;
            trigger_added_touches(fs, ft);
          }
          record_fevent(FEVENT_COMPLETE, fid);
        } else {
          ft->suspended_lw_stack = NULL;
        }

        fts->thread->current_ft = NULL;
      }

      /* Clear stacks */
      MZ_RUNSTACK = MZ_RUNSTACK_START + fts->runstack_size;
      MZ_CONT_MARK_STACK = 0;

      if (ft)
        scheme_signal_received_at(fs->signal_handle);

      record_fevent(FEVENT_END_WORK, fid);

      --fs->busy_thread_count;
    }

    end_gc_not_ok(fts, fs, NULL);
    mzrt_mutex_unlock(fs->future_mutex);
  }

  return NULL;
}

static Scheme_Object *_apply_future_lw(future_t *ft)
  XFORM_SKIP_PROC
/* Called from any thread (either runtime or future) */
{
  struct Scheme_Lightweight_Continuation *lw = ft->suspended_lw;
  Scheme_Object *v;
  int result_is_rs_plus_two;

  ft->suspended_lw = NULL;
  
  v = ft->retval_s;
  if (ft->retval_is_rs_plus_two) {
    result_is_rs_plus_two = 1;
    ft->retval_is_rs_plus_two = 0;
  } else {
    ft->retval_s = NULL;
    receive_special_result(ft, v, 1);
    result_is_rs_plus_two = 0;
  }

  FUTURE_ASSERT((ft->prim_protocol != SIG_ON_DEMAND) == !result_is_rs_plus_two);
  FUTURE_ASSERT(v || (ft->prim_protocol != SIG_ALLOC));

  v = scheme_apply_lightweight_continuation(lw, v, result_is_rs_plus_two, 
                                            FUTURE_RUNSTACK_SIZE);
  
  if (SAME_OBJ(v, SCHEME_TAIL_CALL_WAITING)) {
    if (scheme_future_thread_state->is_runtime_thread)
      v = scheme_force_value_same_mark(v);
    else
      v = scheme_force_value_same_mark_as_lightweight_continuation(v);
  }

  return v;
}

static void *apply_future_lw_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  future_t *ft = (future_t *)p->ku.k.p1;

  p->ku.k.p1 = NULL;
  
  return _apply_future_lw(ft);
}

static Scheme_Object *apply_future_lw(future_t *ft)
{
  Scheme_Thread *p = scheme_current_thread;

  p->ku.k.p1 = ft;

  return (Scheme_Object *)scheme_top_level_do(apply_future_lw_k, 0);
}

static int capture_future_continuation(Scheme_Future_State *fs, future_t *ft, void **storage, 
                                       int need_lock, int for_overflow)
  XFORM_SKIP_PROC
/* The lock is *not* held when calling this function.
   This function explicitly cooperates with the GC by storing the
   pointers it needs to save across a collection in `storage', so
   it can be used in a future thread. If future-thread-local 
   allocation fails, the result is 0. */
{
  Scheme_Lightweight_Continuation *lw;
  Scheme_Object **arg_S;
  void **stack;

  storage[2] = ft;

  if (for_overflow) {
    stack = MALLOC_N(void*, 6);
    if (!stack) return 0;
    storage[3] = stack;
    ft = (future_t *)storage[2];
  } else
    stack = NULL;

  lw = scheme_capture_lightweight_continuation(ft->fts->thread, ft->lwc, storage);
  if (!lw) return 0;
       
  ft = (future_t *)storage[2];
  stack = (void **)storage[3];

  if (need_lock) {
    mzrt_mutex_lock(fs->future_mutex);
  
    if (!ft->want_lw) {
      /* Future can already continue. This can only happen
         if ft was blocked on another future, and the other
         future decided that it could continue while we were
         trying to grab the continuation. Drop the captured
         continuation. */
      return 1;
    }

    ft->want_lw = 0;
  }
   
  ft->fts->thread->current_ft = NULL; /* tells worker thread that it no longer
                                         needs to handle the future */
  
  ft->suspended_lw = lw;
  ft->maybe_suspended_lw = 1;
  
  if (ft->arg_S0) {
    arg_S = scheme_adjust_runstack_argument(lw, ft->arg_S0);
    ft->arg_S0 = arg_S;
  }
  if (ft->arg_S1) {
    arg_S = scheme_adjust_runstack_argument(lw, ft->arg_S1);
    ft->arg_S1 = arg_S;
  }
  if (ft->arg_S2) {
    arg_S = scheme_adjust_runstack_argument(lw, ft->arg_S2);
    ft->arg_S2 = arg_S;
  }

  if (for_overflow) {
    stack[0] = ft->suspended_lw_stack;
    stack[5] = ((for_overflow > 1) ? scheme_true : NULL);
    ft->suspended_lw_stack = stack;
  }

  return 1;
}

static void push_suspended_lw(Scheme_Future_State *fs, future_t *ft)
  XFORM_SKIP_PROC
/* called in any thread; needs lock */
{
  ft->suspended_lw_stack[1] = ft->suspended_lw;
  ft->suspended_lw = NULL;

  ft->suspended_lw_stack[2] = ft->arg_s0;
  ft->arg_s0 = NULL;
  ft->suspended_lw_stack[3] = scheme_make_integer(ft->arg_i0);
  ft->suspended_lw_stack[4] = ft->arg_S0;
  ft->arg_S0 = NULL;

  ft->status = PENDING;
  (void)enqueue_future(fs, ft);
}

static void pop_suspended_lw(Scheme_Future_State *fs, future_t *ft)
  XFORM_SKIP_PROC
/* called in any thread; needs lock */
{
  ft->retval_s = ft->retval;
  ft->retval = NULL;

  ft->suspended_lw = (Scheme_Lightweight_Continuation *)ft->suspended_lw_stack[1];
  ft->maybe_suspended_lw = 1;

  ft->suspended_lw_stack = (void **)ft->suspended_lw_stack[0];

  ft->status = PENDING;
  (void)enqueue_future(fs, ft);
}

void scheme_check_future_work()
/* Called in the runtime thread by the scheduler */
{
  /* Check for work that future threads need from the runtime thread
     and that can be done in any Racket thread (e.g., get a new page
     for allocation). */
  future_t *ft, *other_ft;
  Scheme_Future_State *fs = scheme_future_state;
  mzrt_sema *can_continue_sema;
  int more;

  if (!fs) return;

  flush_future_logs(fs);

  check_future_thread_creation(fs);

  if (!fs->future_threads_created) return;

  more = 1;
  while (more) {
    /* Try to get a future waiting on a atomic operation */
    mzrt_mutex_lock(fs->future_mutex);
    ft = fs->future_waiting_atomic;
    if (ft) {
      fs->future_waiting_atomic = ft->next_waiting_atomic;
      ft->next_waiting_atomic = NULL;
      if ((ft->status == WAITING_FOR_PRIM) && ft->rt_prim_is_atomic) {
        ft->status = HANDLING_PRIM;
        ft->want_lw = 0; /* we expect to handle it quickly,
                            so the future thread should just wait */
      } else
        ft = NULL;
      more = 1;
    } else
      more = 0;
    mzrt_mutex_unlock(fs->future_mutex);

    if (ft)
      invoke_rtcall(fs, ft, 1);
  }

  more = 1;
  while (more) {
    /* Try to get a future that's waiting to touch another future: */
    mzrt_mutex_lock(fs->future_mutex);
    ft = fs->future_waiting_touch;
    if (ft) {
      fs->future_waiting_touch = ft->next_waiting_touch;
      ft->next_waiting_touch = NULL;
      ft->in_touch_queue = 0;
      other_ft = get_future_for_touch(ft);
      more = 1;
    } else {
      other_ft = NULL;
      more = 0;
    }
    mzrt_mutex_unlock(fs->future_mutex);

    if (other_ft) {
      /* Chain to `ft' from `other_ft': */
      Scheme_Object *wb, *pr;
      
      wb = scheme_make_weak_box((Scheme_Object *)ft);
      pr = scheme_make_pair(wb, scheme_null);

      mzrt_mutex_lock(fs->future_mutex);
      if (other_ft->status == FINISHED) {
        /* Completed while we tried to allocate a chain link. */
        direct_future_to_future_touch(fs, other_ft, ft);
      } else {
        /* enqueue */
        if (other_ft->touching)
          SCHEME_CDR(pr) = other_ft->touching;
        other_ft->touching = pr;
      }
      mzrt_mutex_unlock(fs->future_mutex);
    }
  }

  while (1) {
    /* Try to get a future waiting to be suspended */
    mzrt_mutex_lock(fs->future_mutex);
    ft = fs->future_waiting_lwc;
    if (ft) {
      fs->future_waiting_lwc = ft->next_waiting_lwc;
      ft->next_waiting_lwc = NULL;
      ft->in_queue_waiting_for_lwc = 0;
      if (!ft->want_lw)
        ft = NULL;
      else {
        /* If ft is touching another future, then the other
           future may resume ft while we grab the continuation. 
           Withold ft->can_continue_sema for now, so that we can
           capture the continuation, and then double-check 
           afterward whether the continuation wants a lwc: */
        can_continue_sema = ft->can_continue_sema;
        ft->can_continue_sema = NULL;
      }
    }
    mzrt_mutex_unlock(fs->future_mutex);

    if (ft) {
      void *storage[4];

      if (capture_future_continuation(fs, ft, storage, 1, 
                                      ((ft->status == WAITING_FOR_OVERFLOW)
                                       ? ft->arg_i1
                                       : 0))) {
        /* capture performs mzrt_mutex_lock(fs->future_mutex) on success. */
        if (ft->suspended_lw) {
          FUTURE_ASSERT((ft->status == WAITING_FOR_PRIM)
                        || (ft->status == WAITING_FOR_FSEMA)
                        || (ft->status == WAITING_FOR_OVERFLOW));
          if (ft->status == WAITING_FOR_OVERFLOW) {
            push_suspended_lw(fs, ft);
          }
        } else
          FUTURE_ASSERT(ft->status != RUNNING);
        mzrt_mutex_unlock(fs->future_mutex);
      } else {
        /* Couldn't capture the continuation. */
        FUTURE_ASSERT(ft->status != RUNNING);
        if (can_continue_sema) {
          /* may need to reinstall the semaphore */
          mzrt_mutex_lock(fs->future_mutex);
          if ((ft->status == WAITING_FOR_PRIM) 
              || (ft->status == WAITING_FOR_FSEMA)) {
            ft->can_continue_sema = can_continue_sema;
            can_continue_sema = NULL;
          }
          mzrt_mutex_unlock(fs->future_mutex);
        }
      }
      /* Signal the waiting worker thread that it can continue, since
         we either captured the continuation or the result became
         available meanwhile: */
      if (can_continue_sema)
        mzrt_sema_post(can_continue_sema);
    } else
      break;
  }

  /* If any future thread has its fuel revoked (must have been a custodian
     shutdown) but doesn't have a future (shutdown future must have been
     handled), then we can restore the thread's fuel. Races are
     possible, but they should be rare, and they lead at worst to bad
     performance. */
  {
    int i;
    for (i = 0; i < fs->thread_pool_size; i++) { 
      if (fs->pool_threads[i]) {
        if (!*(fs->pool_threads[i]->fuel_pointer)
            && !fs->pool_threads[i]->thread->current_ft) {
          *(fs->pool_threads[i]->fuel_pointer) = 1;
          *(fs->pool_threads[i]->stack_boundary_pointer) -= FUTURE_C_STACK_SIZE;
        }
      }
    }
  }
}

static void future_do_runtimecall(Scheme_Future_Thread_State *fts,
                                  void *func,
                                  int is_atomic,
                                  int can_suspend,
                                  int for_overflow)
  XFORM_SKIP_PROC
/* Called in either future or runtime thread.  Can only be called in the runtime thread 
  if we are in slow-path trace mode (i.e. we are running a future that is bound to the 
  runtime thread so we can log all of its primitive applications). */ 
{
  future_t *future;
  Scheme_Future_State *fs = scheme_future_state;
  void *storage[4];
  int insist_to_suspend, prefer_to_suspend, fid;

#ifdef MZ_PRECISE_GC 
  if (for_overflow && (!GC_gen0_alloc_page_ptr || fts->local_capture_failed)) {
    /* To increase the chance that later overflows can be handled
       without blocking, get more memory for this future thread. The
       `local_capture_failed' flag is a heuristic that might be
       improved by checking the available memory against an estimate
       of the needed memory. */
    fts->local_capture_failed = 0;
    GC_gen0_alloc_page_ptr = scheme_rtcall_alloc();
  }
#endif

  /* Fetch the future descriptor for this thread */
  future = fts->thread->current_ft;

  if (!for_overflow) {
    /* Check if this prim in fact does have a 
        safe C version */
    if (func == scheme_even_p || func == scheme_odd_p) {
      prim_iS_s f = (prim_iS_s)func;
      Scheme_Object *ret;
      ret = f(future->arg_i0, future->arg_S1);
      future->retval_s = ret;
      return;
    }
  }

  /* Check whether we are in slow-path trace mode */ 
  if (fts->is_runtime_thread) { 
    /* On runtime thread - must be slow-path tracing */ 
    future->prim_func = func; 
    future->status = WAITING_FOR_PRIM;
    invoke_rtcall(scheme_future_state, future, 0);
    fts->worker_gc_counter = *fs->gc_counter_ptr;

    return;
  }  

  scheme_fill_lwc_end();
  future->lwc = scheme_current_lwc;
  future->fts = fts;

  fid = future->id;

  /* If for_overflow, then a suspend is required. Otherwise...
     Policy question: When should the future thread suspend
     the current future? It costs something to suspend and
     resume a future.
     The current policy:
     Always suspend for a non-atomic (i.e, "unsafe") operation,
     because there's no guarantee that `touch' will allow progress
     anytime soon. For atomic operations, only suspend if there's
     more work available in the future queue, and only if we
     can suspend ourselves (because asking the runtime thread
     to suspend wouldn't accomplish anything). */
  insist_to_suspend = !is_atomic || for_overflow;
  prefer_to_suspend = (insist_to_suspend || fs->future_queue_count);

  if (!scheme_custodian_is_available(future->cust)) {
    insist_to_suspend = 1;
    prefer_to_suspend = 1;
  }

  if (!can_suspend) {
    insist_to_suspend = 0;
    prefer_to_suspend = 0;
  }

  if (prefer_to_suspend
      && GC_gen0_alloc_page_ptr
      && capture_future_continuation(fs, future, storage, 0, for_overflow)) {
    /* this future thread will suspend handling the future
       continuation until the result of the blocking call is ready;
       fts->thread->current_ft was set to NULL */
  }

  mzrt_mutex_lock(fs->future_mutex);

  if (for_overflow) {
    record_fevent(FEVENT_OVERFLOW, fid);
  } else if (func == touch) {
    record_fevent(FEVENT_RTCALL_TOUCH, fid);
  } else {
    record_fevent(is_atomic ? FEVENT_RTCALL_ATOMIC : FEVENT_RTCALL, fid);
  }

  if (for_overflow) {
    if (!fts->thread->current_ft) {
      /* capture complete; re-enqueue so that it continues on fresh stack */
      push_suspended_lw(fs, future);
    } else {
      future->status = WAITING_FOR_OVERFLOW;
      future->arg_i1 = for_overflow;
      fts->local_capture_failed = 1;
    }
  } else {
    /* Set up the arguments for the runtime call
       to be picked up by the main rt thread */
    future->prim_func = func;
    future->rt_prim_is_atomic = is_atomic;
    future->status = WAITING_FOR_PRIM;
  }

  if (is_atomic) {
    future->next_waiting_atomic = fs->future_waiting_atomic;
    fs->future_waiting_atomic = future;
  }

  if (fts->thread->current_ft) {
    if (insist_to_suspend) {
      /* couldn't capture the continuation locally, so ask
         the runtime thread to capture it: */
      if (!future->in_queue_waiting_for_lwc) { 
        future->next_waiting_lwc = fs->future_waiting_lwc;
        fs->future_waiting_lwc = future;
        future->in_queue_waiting_for_lwc = 1;
      }
      future->want_lw = 1;
      /* In case of for_overflow, runtime thread is responsible for
         enqueuing the future to continue. */
    }
  }

  if (func == touch) {
    if (!future->in_future_specific_touch_queue) {
      /* Ask the runtime thread to put this future on the queue
         of the future being touched: */
      if (!future->in_touch_queue) {
        future->next_waiting_touch = fs->future_waiting_touch;
        fs->future_waiting_touch = future;
        future->in_touch_queue = 1;
      }
    } else {
      future->in_future_specific_touch_queue = 0; /* done with back-door argument */
    }
  }

  scheme_signal_received_at(fs->signal_handle);

  if (fts->thread->current_ft) {
    /* Wait for the signal that the RT call is finished
       or a lightweight continuation has been captured: */
    future->can_continue_sema = fts->worker_can_continue_sema;
    end_gc_not_ok(fts, fs, MZ_RUNSTACK); /* we rely on this putting MZ_CONT_MARK_STACK into the thread record */
    mzrt_mutex_unlock(fs->future_mutex);

    mzrt_sema_wait(fts->worker_can_continue_sema);

    mzrt_mutex_lock(fs->future_mutex);
    start_gc_not_ok(fs);
  }

  /* Fetch the future instance again, in case the GC has moved the pointer
     or the future has been requeued. */
  future = fts->thread->current_ft;

  FUTURE_ASSERT(!future || !future->can_continue_sema);
  FUTURE_ASSERT(!future || !for_overflow);

  if (future) {
    future->want_lw = 0;
    FUTURE_ASSERT(future->status == HANDLING_PRIM);
    if (future->no_retval) {
      record_fevent(FEVENT_RTCALL_ABORT, fid);
      future->status = FINISHED;
      trigger_added_touches(fs, future);
    } else {
      record_fevent(FEVENT_RTCALL_RESULT, fid);
      future->status = RUNNING;
    }
  } else {
    if (!for_overflow)
      record_fevent(FEVENT_RTCALL_SUSPEND, fid);
  }

  mzrt_mutex_unlock(fs->future_mutex);

  if (!future) {
    /* future continuation was requeued */
    scheme_future_longjmp(*scheme_current_thread->error_buf, 1);
  } else if (future->no_retval) {
    /* there was an error => abort the future */
    future->no_retval = -1;
    scheme_future_longjmp(*scheme_current_thread->error_buf, 1);
  } else {
    FUTURE_ASSERT(future->status == RUNNING);
    record_fevent(FEVENT_START_WORK, fid);
  }
}

static Scheme_Object *bad_multi_result(int argc, Scheme_Object **argv)
{
  scheme_wrong_return_arity(NULL, 1, argc, argv, NULL);
  return NULL;
}

/**********************************************************************/
/* Functions for primitive invocation          			      */
/**********************************************************************/
void scheme_wrong_contract_from_ft(const char *who, const char *expected_type, int what, int argc, Scheme_Object **argv) 
  XFORM_SKIP_PROC 
/* Called in future thread */
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future = fts->thread->current_ft;

  future->prim_protocol = SIG_WRONG_TYPE_EXN;
  future->arg_str0 = who;
  future->arg_str1 = expected_type;
  future->arg_i2 = what;
  future->arg_i3 = argc;
  future->arg_S4 = argv;

  future->time_of_request = get_future_timestamp();
  future->source_of_request = who;
  future_do_runtimecall(fts, NULL, 0, 1, 0);
  
  /* Fetch the future again, in case moved by a GC */ 
  future = fts->thread->current_ft;
}

Scheme_Object **scheme_rtcall_on_demand(Scheme_Object **argv)
  XFORM_SKIP_PROC
/* Called in future thread */
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future = fts->thread->current_ft;

  future->prim_protocol = SIG_ON_DEMAND;

  if (argv != (MZ_RUNSTACK + 2)) {
    if (future->in_tracing_mode) {
      return scheme_on_demand(argv);
    }

    FUTURE_ASSERT(0);
  }

  future->arg_S0 = MZ_RUNSTACK;

  future->time_of_request = get_future_timestamp();
  future->source_of_request = "[jit_on_demand]";
  future->source_type = FSRC_OTHER;

  future_do_runtimecall(fts, NULL, 1, 1, 0);

  /* Fetch the future again, in case moved by a GC */ 
  future = fts->thread->current_ft;

  future->arg_S0 = NULL;
  future->retval_is_rs_plus_two = 0;

  return MZ_RUNSTACK + 2;
}

Scheme_Object *scheme_rtcall_make_fsemaphore(Scheme_Object *ready)
  XFORM_SKIP_PROC
/* Called in future thread */
{
  Scheme_Object *retval;
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future = fts->thread->current_ft;
  int is_atomic;
  
  future->prim_protocol = SIG_MAKE_FSEMAPHORE;
  future->arg_s1 = ready;
  future->time_of_request = get_future_timestamp();
  future->source_of_request = "[make_fsemaphore]";
  future->source_type = FSRC_OTHER;

  /* conservative check for when creation can succeed atomically 
     (because it won't raise an error): */
  if (SCHEME_INTP(ready) 
      && (SCHEME_INT_VAL(ready) >= 0)
      && (SCHEME_INT_VAL(ready) < 1024))
    is_atomic = 1;
  else
    is_atomic = 0;
  
  future_do_runtimecall(fts, NULL, is_atomic, 1, 0);

  /* Fetch the future again, in case moved by a GC */ 
  future = fts->thread->current_ft;

  retval = future->retval_s;
  future->retval_s = NULL;

  return retval;
}

Scheme_Object *scheme_rtcall_make_future(Scheme_Object *proc)
  XFORM_SKIP_PROC
/* Called in future thread */
{
  Scheme_Object *retval;
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future = fts->thread->current_ft;
  int is_atomic = 0;
  
  if (SAME_TYPE(SCHEME_TYPE(proc), scheme_native_closure_type)
      && scheme_native_arity_check(proc, 0)) {
    is_atomic = 1;
  }
  
  future->prim_protocol = SIG_FUTURE;
  future->arg_s1 = proc;
  future->time_of_request = get_future_timestamp();
  future->source_of_request = "future";
  future->source_type = FSRC_OTHER;
  
  future_do_runtimecall(fts, NULL, is_atomic, 1, 0);

  /* Fetch the future again, in case moved by a GC */ 
  future = fts->thread->current_ft;

  retval = future->retval_s;
  future->retval_s = NULL;

  return retval;  
}

void scheme_rtcall_allocate_values(int count, Scheme_Thread *t)
  XFORM_SKIP_PROC
/* Called in future thread */
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future = fts->thread->current_ft;

  future->prim_protocol = SIG_ALLOC_VALUES;

  future->arg_i0 = count;
  future->arg_s0 = (Scheme_Object *)t;

  future->time_of_request = get_future_timestamp();
  future->source_of_request = "[allocate_values]";
  future->source_type = FSRC_OTHER;

  future_do_runtimecall(fts, NULL, 1, 0, 0);

  /* Fetch the future again, in case moved by a GC */ 
  future = fts->thread->current_ft;

  future->arg_s0 = NULL;
}

Scheme_Structure *scheme_rtcall_allocate_structure(int count, Scheme_Struct_Type *t)
  XFORM_SKIP_PROC
/* Called in future thread */
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future = fts->thread->current_ft;
  Scheme_Object *retval;

  future->prim_protocol = SIG_ALLOC_STRUCT;

  future->arg_i0 = count;
  future->arg_s0 = (Scheme_Object *)t;

  future->time_of_request = get_future_timestamp();
  future->source_of_request = "[allocate_structure]";
  future->source_type = FSRC_OTHER;

  future_do_runtimecall(fts, NULL, 1, 0, 0);

  /* Fetch the future again, in case moved by a GC */ 
  future = fts->thread->current_ft;

  future->arg_s0 = NULL;

  retval = future->retval_s;
  future->retval_s = NULL;

  return (Scheme_Structure *)retval;
}

Scheme_Object *scheme_rtcall_tail_apply(Scheme_Object *rator, int argc, Scheme_Object **argv)
  XFORM_SKIP_PROC
/* Called in future thread */
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future = fts->thread->current_ft;
  Scheme_Object *retval;

  future->prim_protocol = SIG_TAIL_APPLY;

  future->arg_s0 = rator;
  future->arg_i0 = argc;
  future->arg_S0 = argv;

  future->time_of_request = get_future_timestamp();
  future->source_of_request = "[tail-call]";
  future->source_type = FSRC_OTHER;

  future_do_runtimecall(fts, NULL, 1, 0, 0);

  /* Fetch the future again, in case moved by a GC */ 
  future = fts->thread->current_ft;

  future->arg_s0 = NULL;
  future->arg_S0 = NULL;

  retval = future->retval_s;
  future->retval_s = NULL;

  receive_special_result(future, retval, 1);

  return retval;
}

Scheme_Object *scheme_rtcall_apply_with_new_stack(Scheme_Object *rator, int argc, Scheme_Object **argv, 
                                                  int multi)
  XFORM_SKIP_PROC
/* Called in future thread; rator is a native closure with a runstack limit that fits */
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future = fts->thread->current_ft;
  Scheme_Object *retval;

  future->prim_protocol = SIG_APPLY_AFRESH;

  future->arg_s0 = rator;
  future->arg_i0 = argc;
  future->arg_S0 = argv;
  future->arg_i1 = multi;

  future->time_of_request = get_future_timestamp();
  future->source_of_request = "[stack-overflow]";
  future->source_type = FSRC_OTHER;

  future_do_runtimecall(fts, NULL, 1, 1, (multi ? 2 : 1));

  /* Fetch the future again, in case moved by a GC */ 
  future = fts->thread->current_ft;

  future->arg_s0 = NULL;
  future->arg_S0 = NULL;

  retval = future->retval_s;
  future->retval_s = NULL;

  receive_special_result(future, retval, 1);

  return retval;
}

#ifdef MZ_PRECISE_GC 
uintptr_t scheme_rtcall_alloc()
  XFORM_SKIP_PROC
/* Called in future thread, possibly during future_do_runtimecall()  */
{
  future_t *future;
  uintptr_t retval;
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  intptr_t align, sz;
  double time_of_request;
  const char *source_of_request;
  int source_type;
  int prim_protocol;
  int arg_i0;
  
  align = GC_alloc_alignment();

  /* Do we actually still have space? */
  if (fts->gen0_start) {
    intptr_t cur;
    cur = GC_gen0_alloc_page_ptr;
    if (cur < (GC_gen0_alloc_page_end - align)) {
      if (cur & (align - 1)) {
        /* round up to next page boundary */
        cur &= ~(align - 1);
        cur += align;
      }
      cur += fts->gen0_initial_offset;
      return cur;
    }
  }

  /* Grow nursery size as long as we don't trigger a GC */
  if (fts->gen0_size < 16)
    fts->gen0_size <<= 1;

  future = fts->thread->current_ft;
  time_of_request = future->time_of_request;
  source_of_request = future->source_of_request;
  source_type = future->source_type;
  prim_protocol = future->prim_protocol;
  arg_i0 = future->arg_i0;

  while (1) {
    future->time_of_request = get_future_timestamp();
    future->source_of_request = "[allocate memory]";
    future->source_type = FSRC_OTHER;

    future->prim_protocol = SIG_ALLOC;
    future->arg_i0 = fts->gen0_size;

    /* don't suspend, because this might be a nested call: */
    future_do_runtimecall(fts, NULL, 1, 0, 0);

    future = fts->thread->current_ft;
    retval = future->alloc_retval;
    sz = future->alloc_sz_retval;
    future->alloc_retval = 0;

    if (fts->worker_gc_counter == future->alloc_retval_counter) {
      fts->gen0_start = retval;
      fts->gen0_initial_offset = retval & (align - 1);
      break;
    }
  }

  future->time_of_request = time_of_request;
  future->source_of_request = source_of_request;
  future->source_type = source_type;
  future->prim_protocol = prim_protocol;
  future->arg_i0 = arg_i0;

  GC_gen0_alloc_page_end = retval + sz;

  return retval;
}
#endif

void scheme_rtcall_new_mark_segment(Scheme_Thread *p)
  XFORM_SKIP_PROC
/* Called in future thread */
{
  future_t *future;
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;

  future = fts->thread->current_ft;
  future->time_of_request = get_future_timestamp();
  future->source_of_request = "[allocate_mark_segment]";
  future->source_type = FSRC_OTHER;
  
  future->prim_protocol = SIG_ALLOC_MARK_SEGMENT;
  future->arg_s0 = (Scheme_Object *)p;
  
  future_do_runtimecall(fts, NULL, 1, 0, 0);
}

static int push_marks(future_t *f, Scheme_Cont_Frame_Data *d)
{
  if (f->suspended_lw) {
    return scheme_push_marks_from_lightweight_continuation(f->suspended_lw, d);
  } else if (f->fts->thread) {
    return scheme_push_marks_from_thread(f->fts->thread, d);
  }

  return 0;
}

static void pop_marks(Scheme_Cont_Frame_Data *d)
{
  scheme_pop_continuation_frame(d);
}

static void receive_special_result(future_t *f, Scheme_Object *retval, int clear)
  XFORM_SKIP_PROC
/* Called in future or runtime thread */
{
  if (SAME_OBJ(retval, SCHEME_MULTIPLE_VALUES)) {
    Scheme_Thread *p = scheme_current_thread;

    p->ku.multiple.array = f->multiple_array;
    p->ku.multiple.count = f->multiple_count;
    if (clear)
      f->multiple_array = NULL;
  } else if (SAME_OBJ(retval, SCHEME_TAIL_CALL_WAITING)) {
    Scheme_Thread *p = scheme_current_thread;

    p->ku.apply.tail_rator = f->tail_rator;
    p->ku.apply.tail_rands = f->tail_rands;
    p->ku.apply.tail_num_rands = f->num_tail_rands;
    if (clear) {
      f->tail_rator = NULL;
      f->tail_rands = NULL;
    }
  }
}

#include "jit_ts_future_glue.c"

static void send_special_result(future_t *f, Scheme_Object *retval)
  XFORM_SKIP_PROC
/* Called in future or runtime thread */
{
  if (SAME_OBJ(retval, SCHEME_MULTIPLE_VALUES)) {
    Scheme_Thread *p = scheme_current_thread;

    f->multiple_array = p->ku.multiple.array;
    f->multiple_count = p->ku.multiple.count;
    if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
      p->values_buffer = NULL;
    p->ku.multiple.array = NULL;
  } else if (SAME_OBJ(retval, SCHEME_TAIL_CALL_WAITING)) {
    Scheme_Thread *p = scheme_current_thread;

    f->tail_rator = p->ku.apply.tail_rator;
    f->tail_rands = p->ku.apply.tail_rands;
    f->num_tail_rands = p->ku.apply.tail_num_rands;
    p->ku.apply.tail_rator = NULL;
    p->ku.apply.tail_rands = NULL;

    if (f->tail_rands == p->tail_buffer) {
      /* This only happens in the runtime thread; we need to
         disconnect the tail buffer from `f->tail_rands' in
         case of a GC. Beware that XFORM is disabled here. */
      Scheme_Object **tb;
      p->tail_buffer = NULL; /* so args aren't zeroed */
      tb = MALLOC_N(Scheme_Object *, p->tail_buffer_size);
      p = scheme_current_thread; /* in case GC moves the thread */
      p->tail_buffer = tb;
    }
  }
}

#define ADJUST_RS_ARG(ft, arg_Sx) if (ft->suspended_lw) arg_Sx = scheme_adjust_runstack_argument(ft->suspended_lw, arg_Sx)

/* Does the work of actually invoking a primitive on behalf of a 
   future.  This function is always invoked on the main (runtime) 
   thread. */
static void do_invoke_rtcall(Scheme_Future_State *fs, future_t *future)
/* Called in runtime thread */
{
  Scheme_Cont_Frame_Data mark_d;
  int need_pop;

#ifdef DEBUG_FUTURES
  g_rtcall_count++;
#endif

  if (scheme_log_level_p(scheme_get_future_logger(), SCHEME_LOG_DEBUG)) {
    const char *src;
    Scheme_Object *userdata;

    src = future->source_of_request;
    if (future->source_type == FSRC_RATOR) {
      int len;
      if (SCHEME_PROCP(future->arg_s0)) {
        const char *src2;
        src2 = scheme_get_proc_name(future->arg_s0, &len, 1);
        if (src2) src = src2;
      }
    } else if (future->source_type == FSRC_PRIM) {
      const char *src2;
      src2 = scheme_look_for_primitive(future->prim_func);
      if (src2) src = src2;
    }


    flush_future_logs(fs);
   
    /* use lg_future_event so we can include `str' in the message: */
    userdata = NULL;
    switch (future->prim_protocol) 
      {
      case SIG_ALLOC: 
        {
          userdata = scheme_make_integer(future->arg_i0);
          break;
        }
      case SIG_ON_DEMAND:
        { 
          /* Closure is first in runstack */
          GC_CAN_IGNORE Scheme_Object **rs = future->arg_S0;
          ADJUST_RS_ARG(future, rs);
          userdata = scheme_object_name(rs[0]);
          if (!userdata) 
            userdata = scheme_intern_symbol("[unknown]");
          
          break;
        }
      }

    log_future_event(fs,
                      "id %d, process %d: %s: %s; time: %f",
                      src,
                      -1, 
                      (future->rt_prim_is_atomic ? FEVENT_HANDLE_RTCALL_ATOMIC : FEVENT_HANDLE_RTCALL),
                      get_future_timestamp(),
                      future->id, 
                      userdata);
  }

  if (((future->source_type == FSRC_RATOR)
          || (future->source_type == FSRC_MARKS)
          || (future->source_type == FSRC_PRIM)) 
        && !future->in_tracing_mode)
    need_pop = push_marks(future, &mark_d);
  else
    need_pop = 0;
  
  switch (future->prim_protocol)
    {
    case SIG_ON_DEMAND:
      {
        GC_CAN_IGNORE Scheme_Object **arg_S0 = future->arg_S0;
        future->arg_S0 = NULL;

        ADJUST_RS_ARG(future, arg_S0);
        
        scheme_on_demand_with_args(arg_S0, arg_S0, 2);

        future->retval_is_rs_plus_two = 1;

        break;
      }
#ifdef MZ_PRECISE_GC
    case SIG_ALLOC:
      {
        uintptr_t ret, sz;
        int amt = future->arg_i0;
        ret = GC_make_jit_nursery_page(amt, &sz);
        future->alloc_retval = ret;
        future->alloc_sz_retval = sz;
        future->alloc_retval_counter = scheme_did_gc_count;
        break;
      }
#endif
    case SIG_ALLOC_MARK_SEGMENT:
      {
        GC_CAN_IGNORE Scheme_Thread *p_seg;
        p_seg = (Scheme_Thread *)future->arg_s0;
        future->arg_s0 = NULL;
        scheme_new_mark_segment(p_seg);
        break;
      }
    case SIG_MAKE_FSEMAPHORE: 
      {
        Scheme_Object *s = future->arg_s1; 
        future->arg_s1 = NULL;
        s = scheme_make_fsemaphore_inl(s);
        future->retval_s = s;
        break;
      }
    case SIG_FUTURE: 
      {
        GC_CAN_IGNORE Scheme_Object *s = future->arg_s1;
        future->arg_s1 = NULL;
        s = make_future(s, 1, future);
        future->retval_s = s;
        break;
      }
    case SIG_ALLOC_VALUES:
      {
        GC_CAN_IGNORE Scheme_Object *arg_s0 = future->arg_s0;

        future->arg_s0 = NULL;

        scheme_jit_allocate_values(future->arg_i0, (Scheme_Thread *)arg_s0);

        break;
      }
    case SIG_ALLOC_STRUCT:
      {
        GC_CAN_IGNORE Scheme_Object *arg_s0 = future->arg_s0;
        GC_CAN_IGNORE Scheme_Structure *res;

        future->arg_s0 = NULL;

        res = scheme_jit_allocate_structure(future->arg_i0, (Scheme_Struct_Type *)arg_s0);

        future->retval_s = (Scheme_Object *)res;

        break;
      }
    case SIG_TAIL_APPLY:
      {
        GC_CAN_IGNORE Scheme_Object *arg_s0 = future->arg_s0;
        GC_CAN_IGNORE Scheme_Object **arg_S0 = future->arg_S0;
        GC_CAN_IGNORE Scheme_Object *retval;

        future->arg_s0 = NULL;
        future->arg_S0 = NULL;

        retval = _scheme_tail_apply(arg_s0, future->arg_i0, arg_S0);

        future->retval_s = retval;
        send_special_result(future, retval);

        break;
      }
    case SIG_WRONG_TYPE_EXN:
      {
        const char *who;
        const char *expected_type;
        int what;
        int argc;
        Scheme_Object **argv;

        who = future->arg_str0;
        expected_type = future->arg_str1;
        what = future->arg_i2;
        argc = future->arg_i3;
        argv = future->arg_S4;
        
        future->arg_str0 = NULL;
        future->arg_str1 = NULL;
        future->arg_S4 = NULL;

        ADJUST_RS_ARG(future, argv);

        scheme_wrong_contract(who, expected_type, what, argc, argv);

        /* doesn't return */

        break;
      }
    case SIG_APPLY_AFRESH:
      {
        GC_CAN_IGNORE Scheme_Object *arg_s0 = future->arg_s0;
        GC_CAN_IGNORE Scheme_Object **arg_S0 = future->arg_S0;
        GC_CAN_IGNORE Scheme_Object *retval;

        /* This code is used only for would-be futures: */
        FUTURE_ASSERT(future->in_tracing_mode);

        future->arg_s0 = NULL;
        future->arg_S0 = NULL;

        if (future->arg_i1)
          retval = _scheme_apply_multi(arg_s0, future->arg_i0, arg_S0);
        else
          retval = _scheme_apply(arg_s0, future->arg_i0, arg_S0);

        future->retval_s = retval;
        send_special_result(future, retval);     

        break;
      }
# define JIT_TS_LOCALIZE(t, f) GC_CAN_IGNORE t f = future->f
# include "jit_ts_runtime_glue.c"
    default:
      scheme_signal_error("unknown protocol %d", future->prim_protocol);
      break;
    }

  if (need_pop)
    pop_marks(&mark_d);

  record_fevent(FEVENT_HANDLE_RTCALL_RESULT, future->id);

  mzrt_mutex_lock(fs->future_mutex);
  complete_rtcall(fs, future);
  mzrt_mutex_unlock(fs->future_mutex);
}

typedef Scheme_Object *(*overflow_k_t)(void);

static void *do_invoke_rtcall_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Future_State *fs = (Scheme_Future_State *)p->ku.k.p1;
  future_t *future = (future_t *)p->ku.k.p2;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    return scheme_handle_stack_overflow((overflow_k_t)do_invoke_rtcall_k);
  }
#endif

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  
  do_invoke_rtcall(fs, future);

  return scheme_void;
}

static void invoke_rtcall(Scheme_Future_State * volatile fs, future_t * volatile future,
                          volatile int is_atomic)
{
  Scheme_Thread *p = scheme_current_thread;
  mz_jmp_buf newbuf, * volatile savebuf;

  FUTURE_ASSERT(!future->want_lw);
  FUTURE_ASSERT(!is_atomic || future->rt_prim_is_atomic);

  savebuf = p->error_buf;
  p->error_buf = &newbuf;
  if (scheme_setjmp(newbuf)) {
    record_fevent(FEVENT_HANDLE_RTCALL_ABORT, future->id);
    mzrt_mutex_lock(fs->future_mutex);
    future->no_retval = 1;
    
    /* If running on a would-be future, no extra work required here */ 
    if (future->suspended_lw || scheme_current_thread->futures_slow_path_tracing) {
      /* Abandon the future */
      future->status = FINISHED;
      future->retval = 0;
      future->suspended_lw = NULL;
      trigger_added_touches(fs, future);
      mzrt_mutex_unlock(fs->future_mutex);
    } else {
      /* Signal the waiting worker thread that it
         can continue running machine code */
      mzrt_sema *can_continue_sema = future->can_continue_sema;
      future->can_continue_sema = NULL;
      mzrt_sema_post(can_continue_sema);
      mzrt_mutex_unlock(fs->future_mutex);
    }
    if (is_atomic) {
      scheme_log_abort("internal error: failure during atomic");
      abort();
    }
    scheme_longjmp(*savebuf, 1);
  } else {
    if (future->rt_prim_is_atomic) {
      do_invoke_rtcall(fs, future);
    } else {
      /* call with continuation barrier. */
      p->ku.k.p1 = fs;
      p->ku.k.p2 = future;

      (void)scheme_top_level_do(do_invoke_rtcall_k, 1);
    }
  }
  p->error_buf = savebuf;
}


/**********************************************************************/
/* Helpers for manipulating the futures queue                         */
/**********************************************************************/

future_t *enqueue_future(Scheme_Future_State *fs, future_t *ft)
  XFORM_SKIP_PROC
/* called in any thread with lock held */
{
  if (fs->future_queue_end) {
    fs->future_queue_end->next = ft;
    ft->prev = fs->future_queue_end;
  }
  fs->future_queue_end = ft;
  if (!fs->future_queue)
    fs->future_queue = ft;
  fs->future_queue_count++;
  
  /* Signal that a future is pending */
  mzrt_sema_post(fs->future_pending_sema);
  
  return ft;
}

future_t *get_pending_future(Scheme_Future_State *fs)
  XFORM_SKIP_PROC
/* Called in future thread with lock held */
{
  future_t *f;

  while (1) {
    f = fs->future_queue;
    if (f) {
      dequeue_future(fs, f);
      if (!scheme_custodian_is_available(f->cust)) {
        f->status = SUSPENDED;
      } else {
        return f;
      }
    } else
      return NULL;
  }
}

#endif

/**********************************************************************/
/*                           Precise GC                               */
/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_future.inc"

static void register_traversers(void)
{
#ifdef MZ_USE_FUTURES
  GC_REG_TRAV(scheme_future_type, future);
  GC_REG_TRAV(scheme_fsemaphore_type, fsemaphore);
#else
  GC_REG_TRAV(scheme_future_type, sequential_future);
  GC_REG_TRAV(scheme_fsemaphore_type, sequential_fsemaphore);
#endif
}

END_XFORM_SKIP;

#endif
