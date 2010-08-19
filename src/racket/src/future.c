/*
  Racket
  Copyright (c) 2006-2010 PLT Scheme Inc.

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

static Scheme_Object *future_p(int argc, Scheme_Object *argv[])
{
  if (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_future_type))
    return scheme_true;
  else
    return scheme_false;
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

Scheme_Object *future(int argc, Scheme_Object *argv[])
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
    scheme_wrong_type("touch", "future", 0, argc, argv);

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
      mz_jmp_buf newbuf, * volatile savebuf;
      Scheme_Thread *p = scheme_current_thread;
      
      /* In case another Scheme thread touchs the future. */
      sema = scheme_make_sema(0);
      ft->running_sema = sema;
      
      savebuf = p->error_buf;
      p->error_buf = &newbuf;
      if (scheme_setjmp(newbuf)) {
        ft->no_retval = 1;
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

Scheme_Object *current_future(int argc, Scheme_Object *argv[])
{
  return scheme_false;
}

# define FUTURE_PRIM_W_ARITY(name, func, a1, a2, env) GLOBAL_PRIM_W_ARITY(name, func, a1, a2, env)

void scheme_init_futures(Scheme_Env *newenv)
{
  FUTURE_PRIM_W_ARITY("future?",          future_p,         1, 1, newenv);
  FUTURE_PRIM_W_ARITY("future",           future,           1, 1, newenv);
  FUTURE_PRIM_W_ARITY("processor-count",  processor_count,  0, 0, newenv);
  FUTURE_PRIM_W_ARITY("current-future",   current_future,   0, 0, newenv);
  FUTURE_PRIM_W_ARITY("touch",            touch,            1, 1, newenv);

  scheme_finish_primitive_module(newenv);
  scheme_protect_primitive_provide(newenv, NULL);

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif
}

void scheme_init_futures_once()
{
}

#else

#include "future.h"
#include <stdlib.h>
#include <string.h>

#ifdef DEBUG_FUTURES 
#define DO_LOG(pr) do { pthread_t self; self = pthread_self(); fprintf(stderr, "%x:%s:%s:%d ", (unsigned) self, __FILE__, __FUNCTION__, __LINE__); pr; fprintf(stderr, "\n"); fflush(stdout); } while(0)
#define LOG0(t) DO_LOG(fprintf(stderr, t))
#define LOG(t, a) DO_LOG(fprintf(stderr, t, a))
#define LOG2(t, a, b) DO_LOG(fprintf(stderr, t, a, b))
#define LOG3(t, a, b, c) DO_LOG(fprintf(stderr, t, a, b, c))
#define LOG4(t, a, b, c, d) DO_LOG(fprintf(stderr, t, a, b, c, d))
#define LOG_THISCALL LOG(__FUNCTION__)
#else
#define LOG0(t)
#define LOG(t, a)
#define LOG2(t, a, b)
#define LOG3(t, a, b, c)
#define LOG4(t, a, b, c, d)
#define LOG_THISCALL
#endif

#define LOG_RTCALL_VOID_VOID_3ARGS(f) LOG("(function=%p)", f)
#define LOG_RTCALL_ALLOC(f) LOG("(function=%p)", f)
#define LOG_RTCALL_OBJ_INT_POBJ_OBJ(f,a,b,c) LOG4("(function = %p, a=%p, b=%d, c=%p)", f, a, b, c)
#define LOG_RTCALL_OBJ_INT_POBJ_VOID(a,b,c) LOG3("(%p, %d, %p)", a, b,c)
#define LOG_RTCALL_INT_OBJARR_OBJ(a,b) LOG2("(%d, %p)", a, b)
#define LOG_RTCALL_LONG_OBJ_OBJ(a,b) LOG2("(%ld, %p)", a, b)
#define LOG_RTCALL_OBJ_OBJ(a) LOG("(%p)", a)
#define LOG_RTCALL_OBJ_OBJ_OBJ(a,b) LOG2("(%p, %p)", a, b)
#define LOG_RTCALL_SNCD_OBJ(a) LOG("(%p)", a)
#define LOG_RTCALL_OBJ_VOID(a) LOG("(%p)", a)
#define LOG_RTCALL_LONG_OBJ(a) LOG("(%ld)", a)
#define LOG_RTCALL_BUCKET_OBJ_INT_VOID(a,b,c) LOG3("(%p, %p, %d)", a, b, c)
#define LOG_RTCALL_INT_INT_POBJ_VOID(a,b,c) LOG3("(%d, %d, %p)", a, b, c)
#define LOG_RTCALL_OBJ_OBJ_MZST(a,b) LOG2("(%p, %p)", a, b)
#define LOG_RTCALL_BUCKET_VOID(a) LOG("(%p)", a)
#define LOG_RTCALL_POBJ_LONG_OBJ(a,b) LOG2("(%p, %ld)", a, b)
#define LOG_RTCALL_INT_POBJ_INT_OBJ(a,b,c) LOG3("(%d, %p, %d)", a, b, c)
#define LOG_RTCALL_INT_POBJ_OBJ_OBJ(a,b,c) LOG3("(%d, %p, %p)", a, b, c)
#define LOG_RTCALL_ENV_ENV_VOID(a,b) LOG2("(%p, %p)", a, b) 

static Scheme_Object *touch(int argc, Scheme_Object *argv[]);
static Scheme_Object *processor_count(int argc, Scheme_Object *argv[]);
static void futures_init(void);
static void init_future_thread(struct Scheme_Future_State *fs, int i);

#define THREAD_POOL_SIZE 16
#define INITIAL_C_STACK_SIZE 500000
#define FUTURE_RUNSTACK_SIZE 1000

typedef struct Scheme_Future_State {
  struct Scheme_Future_Thread_State *pool_threads[THREAD_POOL_SIZE];

  void *signal_handle;

  int future_queue_count;
  future_t *future_queue;
  future_t *future_queue_end;
  future_t *future_waiting_atomic;
  int next_futureid;

  mzrt_mutex *future_mutex;
  mzrt_sema *future_pending_sema;
  mzrt_sema *gc_ok_c;
  mzrt_sema *gc_done_c;

  int gc_not_ok, wait_for_gc, need_gc_ok_post, need_gc_done_post;

  int *gc_counter_ptr;

  int future_threads_created;
} Scheme_Future_State;

typedef struct Scheme_Future_Thread_State {
  int id;
  int worker_gc_counter;
  mzrt_sema *worker_can_continue_sema;
  future_t *current_ft;
  long runstack_size;

  volatile int *fuel_pointer;
  volatile unsigned long *stack_boundary_pointer;
  volatile int *need_gc_pointer;

  unsigned long gen0_start;
  unsigned long gen0_size;
  unsigned long gen0_initial_offset;
} Scheme_Future_Thread_State;

THREAD_LOCAL_DECL(static Scheme_Future_State *scheme_future_state);
THREAD_LOCAL_DECL(void *jit_future_storage[2]);

#ifdef MZ_PRECISE_GC
THREAD_LOCAL_DECL(extern unsigned long GC_gen0_alloc_page_ptr);
#endif

static void start_gc_not_ok(Scheme_Future_State *fs);
static void end_gc_not_ok(Scheme_Future_Thread_State *fts, 
                          Scheme_Future_State *fs, 
                          Scheme_Object **current_rs);

static void *worker_thread_future_loop(void *arg);
static void invoke_rtcall(Scheme_Future_State * volatile fs, future_t * volatile future);
static future_t *enqueue_future(Scheme_Future_State *fs, future_t *ft);;
static future_t *get_pending_future(Scheme_Future_State *fs);
static void receive_special_result(future_t *f, Scheme_Object *retval, int clear);
static void send_special_result(future_t *f, Scheme_Object *retval);
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
  Scheme_Thread *thread_skeleton;
  Scheme_Object **runstack_start;

  Scheme_Object ***scheme_current_runstack_ptr;
  Scheme_Object ***scheme_current_runstack_start_ptr;
  Scheme_Thread **current_thread_ptr;
  void *jit_future_storage_ptr;
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

  scheme_add_global_constant(
                             "future", 
                             scheme_make_prim_w_arity(
                                                      future, 
                                                      "future", 
                                                      1, 
                                                      1), 
                             newenv);

  scheme_add_global_constant(
                             "processor-count", 
                             scheme_make_prim_w_arity(
                                                      processor_count, 
                                                      "processor-count", 
                                                      0, 
                                                      0), 
                             newenv);

  scheme_add_global_constant(
                             "touch", 
                             scheme_make_prim_w_arity(
                                                      touch, 
                                                      "touch", 
                                                      1, 
                                                      1), 
                             newenv);

  p = scheme_make_immed_prim( 
                              current_future, 
                              "current-future", 
                              0, 
                              0);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_NARY_INLINED;
  scheme_add_global_constant("current-future", p, newenv);

  scheme_finish_primitive_module(newenv);
  scheme_protect_primitive_provide(newenv, NULL);
}

void scheme_init_futures_once()
{
  init_cpucount();
}

void scheme_init_futures_per_place()
{
  futures_init();
}

void futures_init(void)
{
  Scheme_Future_State *fs;
  void *hand;

  fs = (Scheme_Future_State *)malloc(sizeof(Scheme_Future_State));
  memset(fs, 0, sizeof(Scheme_Future_State));
  scheme_future_state = fs;

  REGISTER_SO(fs->future_queue);
  REGISTER_SO(fs->future_queue_end);
  REGISTER_SO(fs->future_waiting_atomic);
  REGISTER_SO(jit_future_storage);

  /* Create a 'dummy' future thread state object for the runtime 
      thread, so that current-future will work even for 
      thunks that are touched before fetched by a worker thread 
      and are executed on the runtime thread */ 
  scheme_future_thread_state = (Scheme_Future_Thread_State*)malloc(sizeof(Scheme_Future_Thread_State));
  memset(scheme_future_thread_state, 0, sizeof(Scheme_Future_Thread_State));
  
  mzrt_mutex_create(&fs->future_mutex);
  mzrt_sema_create(&fs->future_pending_sema, 0);
  mzrt_sema_create(&fs->gc_ok_c, 0);
  mzrt_sema_create(&fs->gc_done_c, 0);

  fs->gc_counter_ptr = &scheme_did_gc_count;

  hand = scheme_get_signal_handle();
  fs->signal_handle = hand;

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

  /* Create the worker thread pool.  These threads will
     'queue up' and wait for futures to become available. */

  fts = (Scheme_Future_Thread_State *)malloc(sizeof(Scheme_Future_Thread_State));
  memset(fts, 0, sizeof(Scheme_Future_Thread_State));
  fts->id = i;

  fts->gen0_size = 1;

  params.shared_GC = GC_instance;
  params.fts = fts;
  params.fs = fs;

  /* Make enough of a thread record to deal with multiple values. */
  skeleton = MALLOC_ONE_TAGGED(Scheme_Thread);
  skeleton->so.type = scheme_thread_type;

  {
    Scheme_Object **rs_start, **rs;
    long init_runstack_size = FUTURE_RUNSTACK_SIZE;
    rs_start = scheme_alloc_runstack(init_runstack_size);
    rs = rs_start XFORM_OK_PLUS init_runstack_size;
    runstack_start = rs_start;
    fts->runstack_size = init_runstack_size;
  }

  /* Fill in GCable values just before creating the thread,
     because the GC ignores `params': */
  params.thread_skeleton = skeleton;
  params.runstack_start = runstack_start;

  mzrt_sema_create(&params.ready_sema, 0);
  mz_proc_thread_create_w_stacksize(worker_thread_future_loop, &params, INITIAL_C_STACK_SIZE);
  mzrt_sema_wait(params.ready_sema);
  mzrt_sema_destroy(params.ready_sema);
	
  scheme_register_static(&fts->current_ft, sizeof(void*));
  scheme_register_static(params.scheme_current_runstack_ptr, sizeof(void*));
  scheme_register_static(params.scheme_current_runstack_start_ptr, sizeof(void*));	
  scheme_register_static(params.jit_future_storage_ptr, 2 * sizeof(void*));
  scheme_register_static(params.current_thread_ptr, sizeof(void*));

  fs->pool_threads[i] = fts;
}

static void start_gc_not_ok(Scheme_Future_State *fs)
/* must have mutex_lock */
{
  while (fs->wait_for_gc) {
    fs->need_gc_done_post++;
    mzrt_mutex_unlock(fs->future_mutex);
    mzrt_sema_wait(fs->gc_done_c);
    mzrt_mutex_lock(fs->future_mutex);
  }

  fs->gc_not_ok++;

#ifdef MZ_PRECISE_GC
  {
    Scheme_Future_Thread_State *fts = scheme_future_thread_state;
    if (fts->worker_gc_counter != *fs->gc_counter_ptr) {
      GC_gen0_alloc_page_ptr = 0; /* forces future to ask for memory */
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

  for (i = 0; i < THREAD_POOL_SIZE; i++) { 
    if (fs->pool_threads[i]) {
      *(fs->pool_threads[i]->need_gc_pointer) = 1;
      *(fs->pool_threads[i]->fuel_pointer) = 0;
      *(fs->pool_threads[i]->stack_boundary_pointer) += INITIAL_C_STACK_SIZE;
    }
  }

  if (cpucount > 1) {
    /* `cpucount' is not actually a complete test for whether mfence
       should work, but the probability of someone using futures
       on a multiprocessor system without SSE2 seems very low. */
#ifdef _MSC_VER
    __asm { 
      mfence 
        }
#else
    asm("mfence");
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

  for (i = 0; i < THREAD_POOL_SIZE; i++) {
    if (fs->pool_threads[i]) {
      *(fs->pool_threads[i]->need_gc_pointer) = 0;
      *(fs->pool_threads[i]->fuel_pointer) = 1;
      *(fs->pool_threads[i]->stack_boundary_pointer) -= INITIAL_C_STACK_SIZE;
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

/**********************************************************************/
/* Primitive implementations                                          */
/**********************************************************************/

Scheme_Object *future(int argc, Scheme_Object *argv[])
/* Called in runtime thread */
{
  Scheme_Future_State *fs = scheme_future_state;
  int futureid, count;
  future_t *ft;
  Scheme_Native_Closure *nc;
  Scheme_Native_Closure_Data *ncd;
  Scheme_Object *lambda = argv[0];

  /* Input validation */
  scheme_check_proc_arity("future", 0, 0, argc, argv);

  if (fs->future_threads_created < THREAD_POOL_SIZE) {
    mzrt_mutex_lock(fs->future_mutex);
    count = fs->future_queue_count;
    mzrt_mutex_unlock(fs->future_mutex);
    if (count >= fs->future_threads_created) {
      init_future_thread(fs, fs->future_threads_created);
      fs->future_threads_created++;
    }
  }

  nc = (Scheme_Native_Closure*)lambda;
  ncd = nc->code;

  /* Create the future descriptor and add to the queue as 'pending' */
  ft = MALLOC_ONE_TAGGED(future_t);     
  ft->so.type = scheme_future_type;

  futureid = ++fs->next_futureid;
  ft->id = futureid;
  ft->orig_lambda = lambda;
  ft->status = PENDING;
   
  /* JIT the code if not already JITted */
  if (ncd->code == scheme_on_demand_jit_code)
    {
      scheme_on_demand_generate_lambda(nc, 0, NULL);
    }

  if (ncd->max_let_depth > FUTURE_RUNSTACK_SIZE * sizeof(void*)) {
    /* Can't even call it in a future thread */
    ft->status = PENDING_OVERSIZE;
  }

  ft->code = (void*)ncd->code;

  mzrt_mutex_lock(fs->future_mutex);
  enqueue_future(fs, ft);
  /* Signal that a future is pending */
  mzrt_sema_post(fs->future_pending_sema);
  mzrt_mutex_unlock(fs->future_mutex);

  return (Scheme_Object*)ft;
}

int future_ready(Scheme_Object *obj)
/* Called in runtime thread by Scheme scheduler */
{
  Scheme_Future_State *fs = scheme_future_state;
  int ret = 0;
  future_t *ft = (future_t*)obj;

  mzrt_mutex_lock(fs->future_mutex);
  if (ft->work_completed || ft->rt_prim) {
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

Scheme_Object *touch(int argc, Scheme_Object *argv[])
/* Called in runtime thread */
{
  Scheme_Future_State *fs = scheme_future_state;
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  Scheme_Object *retval = NULL;
  future_t *ft;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_future_type))
    scheme_wrong_type("touch", "future", 0, argc, argv);

  ft = (future_t*)argv[0];

#ifdef DEBUG_FUTURES 
  LOG("touch (future %d)", futureid);	
  dump_state();
#endif

  mzrt_mutex_lock(fs->future_mutex);
  if ((ft->status == PENDING) || (ft->status == PENDING_OVERSIZE)) {
    if (ft->status == PENDING_OVERSIZE) {
      scheme_log(scheme_main_logger, SCHEME_LOG_DEBUG, 0,
                 "future: oversize procedure deferred to runtime thread");
    }
    ft->status = RUNNING;
    mzrt_mutex_unlock(fs->future_mutex);

    fts->current_ft = ft;
    retval = scheme_apply_multi(ft->orig_lambda, 0, NULL);
    send_special_result(ft, retval);

    fts->current_ft = NULL;
    mzrt_mutex_lock(fs->future_mutex);
    ft->work_completed = 1;
    ft->retval = retval;
    ft->status = FINISHED;
    dequeue_future(fs, ft);
    mzrt_mutex_unlock(fs->future_mutex);

    receive_special_result(ft, retval, 0);

    return retval;
  }
  mzrt_mutex_unlock(fs->future_mutex);

  /* Spin waiting for primitive calls or a return value from
     the worker thread */
  while (1) {
    scheme_block_until(future_ready, NULL, (Scheme_Object*)ft, 0);
    mzrt_mutex_lock(fs->future_mutex);
    if (ft->work_completed)
      {
        retval = ft->retval;

        LOG("Successfully touched future %d\n", ft->id);

        mzrt_mutex_unlock(fs->future_mutex);
        break;
      }
    else if (ft->rt_prim)
      {
        /* Invoke the primitive and stash the result.
           Release the lock so other threads can manipulate the queue
           while the runtime call executes. */
        mzrt_mutex_unlock(fs->future_mutex);
        LOG2("Invoking primitive %p on behalf of future %d...", ft->rt_prim, ft->id);
        invoke_rtcall(fs, ft);
        LOG0("done.\n");
      }
    else
      {
        mzrt_mutex_unlock(fs->future_mutex);
      }
  }

  if (!retval) {
    scheme_signal_error("touch: future previously aborted");
  }

  receive_special_result(ft, retval, 0);

  return retval;
}

#if defined(linux)
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
#if defined(linux)
  cpucount = sysconf(_SC_NPROCESSORS_ONLN);
#elif defined(OS_X)
  size_t size = sizeof(cpucount);

  if (sysctlbyname("hw.ncpu", &cpucount, &size, NULL, 0))
	{
	  cpucount = 1;
	}
#elif defined(DOS_FILE_SYSTEM)
  SYSTEM_INFO sysinfo;
  GetSystemInfo(&sysinfo);
  cpucount = sysinfo.dwNumberOfProcessors;
#else
  cpucount = THREAD_POOL_SIZE;
#endif
}

Scheme_Object *processor_count(int argc, Scheme_Object *argv[])
/* Called in runtime thread */
{
  return scheme_make_integer(cpucount);
}

Scheme_Object *current_future(int argc, Scheme_Object *argv[])
/* Called from any thread (either runtime or future) */
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  if (NULL == fts || NULL == fts->current_ft)
    return scheme_false;
 
  return (Scheme_Object*)(fts->current_ft);
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
  Scheme_Object* (*jitcode)(Scheme_Object*, int, Scheme_Object**);
  future_t *ft;
  mz_jmp_buf newbuf;

  scheme_future_state = fs;
  scheme_future_thread_state = fts;

  GC_instance = params->shared_GC;
  scheme_current_thread = params->thread_skeleton;

  /* Set processor affinity */
  /*mzrt_mutex_lock(fs->future_mutex);
      static unsigned long cur_cpu_mask = 1;
    if (pthread_setaffinity_np(pthread_self(), sizeof(g_cur_cpu_mask), &g_cur_cpu_mask))
    {
    printf(
    "Could not set CPU affinity (%lu) for thread %p!\n", 
    ++g_cur_cpu_mask, 
    pthread_self());
    }

    mzrt_mutex_unlock(fs->future_mutex);
  */

  mzrt_sema_create(&fts->worker_can_continue_sema, 0);

  scheme_use_rtcall = 1;

  scheme_fuel_counter = 1;
  scheme_jit_stack_boundary = ((unsigned long)&v) - INITIAL_C_STACK_SIZE;

  fts->need_gc_pointer = &scheme_future_need_gc_pause;
  fts->fuel_pointer = &scheme_fuel_counter;
  fts->stack_boundary_pointer = &scheme_jit_stack_boundary;

  MZ_RUNSTACK_START = params->runstack_start;
  MZ_RUNSTACK = MZ_RUNSTACK_START + fts->runstack_size;

  params->scheme_current_runstack_ptr = &scheme_current_runstack;
  params->scheme_current_runstack_start_ptr = &scheme_current_runstack_start;
  params->current_thread_ptr = &scheme_current_thread;
  params->jit_future_storage_ptr = &jit_future_storage[0];

  mzrt_sema_post(params->ready_sema);

  while (1) {
    mzrt_sema_wait(fs->future_pending_sema);
    mzrt_mutex_lock(fs->future_mutex);
    start_gc_not_ok(fs);
    ft = get_pending_future(fs);

    if (ft) {
      LOG0("Got a signal that a future is pending...");
        
      /* Work is available for this thread */
      ft->status = RUNNING;
      mzrt_mutex_unlock(fs->future_mutex);

      ft->thread_short_id = fts->id;

      /* Set up the JIT compiler for this thread  */
      scheme_jit_fill_threadlocal_table();
        
      jitcode = (Scheme_Object* (*)(Scheme_Object*, int, Scheme_Object**))(ft->code);

      fts->current_ft = ft;

      /* Run the code:
         The lambda passed to a future will always be a parameterless
         function.
         From this thread's perspective, this call will never return
         until all the work to be done in the future has been completed,
         including runtime calls. 
         If jitcode asks the runrtime thread to do work, then
         a GC can occur. */
      LOG("Running JIT code at %p...\n", ft->code);

      MZ_RUNSTACK = MZ_RUNSTACK_START + fts->runstack_size;
      MZ_CONT_MARK_STACK = 0;
      MZ_CONT_MARK_POS = (MZ_MARK_POS_TYPE)1;

      scheme_current_thread->error_buf = &newbuf;
      if (scheme_future_setjmp(newbuf)) {
        /* failed */
        v = NULL;
      } else {
        v = jitcode(ft->orig_lambda, 0, NULL);
        if (SAME_OBJ(v, SCHEME_TAIL_CALL_WAITING)) {
          v = scheme_ts_scheme_force_value_same_mark(v);
        }
      }

      LOG("Finished running JIT code at %p.\n", ft->code);

      /* Get future again, since a GC may have occurred */
      ft = fts->current_ft;
  
      /* Set the return val in the descriptor */
      mzrt_mutex_lock(fs->future_mutex);
      ft->work_completed = 1;
      ft->retval = v;

      /* In case of multiple values: */
      send_special_result(ft, v);

      /* Update the status */
      ft->status = FINISHED;
      dequeue_future(fs, ft);

      /* Clear stacks */
      MZ_RUNSTACK = MZ_RUNSTACK_START + fts->runstack_size;
      MZ_CONT_MARK_STACK = 0;
      
      scheme_signal_received_at(fs->signal_handle);

    }
    end_gc_not_ok(fts, fs, NULL);
    mzrt_mutex_unlock(fs->future_mutex);
  }

  return NULL;
}

void scheme_check_future_work()
/* Called in the runtime thread by the scheduler */
{
  /* Check for work that future threads need from the runtime thread
     and that can be done in any Scheme thread (e.g., get a new page
     for allocation). */
  future_t *ft;
  Scheme_Future_State *fs = scheme_future_state;

  if (!fs) return;

  while (1) {
    /* Try to get a future waiting on a atomic operation */
    mzrt_mutex_lock(fs->future_mutex);
    ft = fs->future_waiting_atomic;
    if (ft) {
      fs->future_waiting_atomic = ft->next_waiting_atomic;
      ft->next_waiting_atomic = NULL;
      ft->waiting_atomic = 0;
    }
    mzrt_mutex_unlock(fs->future_mutex);

    if (ft) {
      if (ft->rt_prim && ft->rt_prim_is_atomic) {
        invoke_rtcall(fs, ft);
      }
    } else
      break;
  }
}

static void future_do_runtimecall(Scheme_Future_Thread_State *fts,
                                  void *func,
                                  int is_atomic)
  XFORM_SKIP_PROC
/* Called in future thread */
{
  future_t *future;
  Scheme_Future_State *fs = scheme_future_state;

  /* Fetch the future descriptor for this thread */
  future = fts->current_ft;

  /* Set up the arguments for the runtime call
     to be picked up by the main rt thread */
  mzrt_mutex_lock(fs->future_mutex);

  future->prim_func = func;
  future->rt_prim = 1;
  future->rt_prim_is_atomic = is_atomic;

  if (is_atomic) {
    if (!future->waiting_atomic) {
      future->next_waiting_atomic = fs->future_waiting_atomic;
      fs->future_waiting_atomic = future;
      future->waiting_atomic = 1;
    }
  }

  /* Update the future's status to waiting */
  future->status = WAITING_FOR_PRIM;

  scheme_signal_received_at(fs->signal_handle);

  future->arg_p = scheme_current_thread;

  /* Wait for the signal that the RT call is finished */
  future->can_continue_sema = fts->worker_can_continue_sema;
  end_gc_not_ok(fts, fs, MZ_RUNSTACK); /* we rely on this putting MZ_CONT_MARK_STACK into the thread record */
  mzrt_mutex_unlock(fs->future_mutex);

  mzrt_sema_wait(fts->worker_can_continue_sema);

  mzrt_mutex_lock(fs->future_mutex);
  start_gc_not_ok(fs);
  mzrt_mutex_unlock(fs->future_mutex);

  /* Fetch the future instance again, in case the GC has moved the pointer */
  future = fts->current_ft;
  
  if (future->no_retval) {
    future->no_retval = 0;
    scheme_future_longjmp(*scheme_current_thread->error_buf, 1);
  }
}

/**********************************************************************/
/* Functions for primitive invocation          			      */
/**********************************************************************/
void scheme_rtcall_void_void_3args(const char *who, int src_type, prim_void_void_3args_t f)
  XFORM_SKIP_PROC
/* Called in future thread */
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future = fts->current_ft;

  future->prim_protocol = SIG_VOID_VOID_3ARGS;

  future->arg_S0 = MZ_RUNSTACK;

  future->time_of_request = scheme_get_inexact_milliseconds();
  future->source_of_request = who;
  future->source_type = src_type;

  future_do_runtimecall(fts, (void*)f, 1);

  future->arg_S0 = NULL;
}

#ifdef MZ_PRECISE_GC

unsigned long scheme_rtcall_alloc(const char *who, int src_type)
  XFORM_SKIP_PROC
/* Called in future thread */
{
  future_t *future;
  unsigned long retval;
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  long align;
  
  align = GC_alloc_alignment();

  /* Do we actually still have space? */
  if (fts->gen0_start) {
    long cur;
    cur = GC_gen0_alloc_page_ptr;
    if (cur < (fts->gen0_start + (fts->gen0_size - 1) * align)) {
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

  while (1) {
    future = fts->current_ft;
    future->time_of_request = scheme_get_inexact_milliseconds();
    future->source_of_request = who;
    future->source_type = src_type;
  
    future->prim_protocol = SIG_ALLOC;
    future->arg_i0 = fts->gen0_size;

    future_do_runtimecall(fts, (void*)GC_make_jit_nursery_page, 1);

    future = fts->current_ft;
    retval = future->alloc_retval;
    future->alloc_retval = 0;

    if (fts->worker_gc_counter == future->alloc_retval_counter) {
      fts->gen0_start = retval;
      fts->gen0_initial_offset = retval & (align - 1);
      break;
    }
  }

  return retval;
}

#endif

void scheme_rtcall_new_mark_segment(Scheme_Thread *p)
  XFORM_SKIP_PROC
/* Called in future thread */
{
  future_t *future;
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;

  future = fts->current_ft;
  future->time_of_request = scheme_get_inexact_milliseconds();
  future->source_of_request = "[allocate_mark_segment]";
  future->source_type = FSRC_OTHER;
  
  future->prim_protocol = SIG_ALLOC_MARK_SEGMENT;
  future->arg_s0 = (Scheme_Object *)p;
  
  future_do_runtimecall(fts, (void*)scheme_new_mark_segment, 1);
}

static int push_marks(future_t *f, Scheme_Cont_Frame_Data *d)
{
  Scheme_Thread *p2, *p;
  long i, pos, delta;
  Scheme_Cont_Mark *seg;

  if (f->arg_p) {
    p2 = f->arg_p;
    if (p2->cont_mark_stack) {
      scheme_push_continuation_frame(d);

      p = scheme_current_thread;

      delta = MZ_CONT_MARK_POS - p2->cont_mark_pos;
      if (delta < 0) delta = 0;
      
      for (i = p2->cont_mark_stack; i--; ) {
        seg = p2->cont_mark_stack_segments[i >> SCHEME_LOG_MARK_SEGMENT_SIZE];
        pos = i & SCHEME_MARK_SEGMENT_MASK;

        MZ_CONT_MARK_POS = seg[pos].pos + delta;
        scheme_set_cont_mark(seg[pos].key, seg[pos].val);
      }

      MZ_CONT_MARK_POS = p2->cont_mark_pos + delta;

      return 1;
    }
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
  }
}

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

  future->rt_prim = 0;

  if (scheme_log_level_p(scheme_main_logger, SCHEME_LOG_DEBUG)) {
    const char *src;

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

    scheme_log(scheme_main_logger, SCHEME_LOG_DEBUG, 0,
               "future: %d waiting for runtime at %f: %s",
               (long)future->thread_short_id,
               future->time_of_request,
               src);
  }

  if ((future->source_type == FSRC_RATOR)
      || (future->source_type == FSRC_MARKS))
    need_pop = push_marks(future, &mark_d);
  else
    need_pop = 0;
  future->arg_p = NULL;
  
  switch (future->prim_protocol)
    {
    case SIG_VOID_VOID_3ARGS:
      {
        prim_void_void_3args_t func = (prim_void_void_3args_t)future->prim_func;

        func(future->arg_S0);

        break;
      }
#ifdef MZ_PRECISE_GC
    case SIG_ALLOC:
      {
        unsigned long ret;
        ret = GC_make_jit_nursery_page(future->arg_i0);
        future->alloc_retval = ret;
        future->alloc_retval_counter = scheme_did_gc_count;
        break;
      }
#endif
    case SIG_ALLOC_MARK_SEGMENT:
      {
        Scheme_Thread *p_seg;
        p_seg = (Scheme_Thread *)future->arg_s0;
        future->arg_s0 = NULL;
        scheme_new_mark_segment(p_seg);
        break;
      }
# include "jit_ts_runtime_glue.c"
    default:
      scheme_signal_error("unknown protocol %d", future->prim_protocol);
      break;
    }

  if (need_pop)
    pop_marks(&mark_d);

  mzrt_mutex_lock(fs->future_mutex);
  /* Signal the waiting worker thread that it
     can continue running machine code */
  if (future->can_continue_sema) {
    mzrt_sema_post(future->can_continue_sema);
    future->can_continue_sema= NULL;
  }
  mzrt_mutex_unlock(fs->future_mutex);
}

static void *do_invoke_rtcall_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Future_State *fs = (Scheme_Future_State *)p->ku.k.p1;
  future_t *future = (future_t *)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  
  do_invoke_rtcall(fs, future);

  return scheme_void;
}

static void invoke_rtcall(Scheme_Future_State * volatile fs, future_t * volatile future)
{
  Scheme_Thread *p = scheme_current_thread;
  mz_jmp_buf newbuf, * volatile savebuf;

  savebuf = p->error_buf;
  p->error_buf = &newbuf;
  if (scheme_setjmp(newbuf)) {
    mzrt_mutex_lock(fs->future_mutex);
    future->no_retval = 1;
    /* Signal the waiting worker thread that it
       can continue running machine code */
    mzrt_sema_post(future->can_continue_sema);
    future->can_continue_sema = NULL;
    mzrt_mutex_unlock(fs->future_mutex);
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
/* Called in runtime thread */
{
  if (fs->future_queue_end) {
    fs->future_queue_end->next = ft;
    ft->prev = fs->future_queue_end;
  }
  fs->future_queue_end = ft;
  if (!fs->future_queue)
    fs->future_queue = ft;
  fs->future_queue_count++;
    
  return ft;
}

future_t *get_pending_future(Scheme_Future_State *fs)
  XFORM_SKIP_PROC
/* Called in future thread */
{
  future_t *f;

  for (f = fs->future_queue; f != NULL; f = f->next) {
    if (f->status == PENDING)
      return f;
  }

  return NULL;
}

#endif

/**********************************************************************/
/*                           Precise GC                               */
/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_FUTURE_C
#include "mzmark.c"

static void register_traversers(void)
{
#ifdef MZ_USE_FUTURES
  GC_REG_TRAV(scheme_future_type, future);
#else
  GC_REG_TRAV(scheme_future_type, sequential_future);
#endif
}

END_XFORM_SKIP;

#endif
