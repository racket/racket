
#ifndef UNIT_TEST
# include "schpriv.h"
#endif

//This will be TRUE if primitive tracking has been enabled 
//by the program
int g_print_prims = 0;

#ifndef FUTURES_ENABLED

/* Futures not enabled, but make a stub module */

static Scheme_Object *future(int argc, Scheme_Object *argv[])
{
  scheme_signal_error("future: not enabled");
  return NULL;
}

static Scheme_Object *touch(int argc, Scheme_Object *argv[])
{
  scheme_signal_error("touch: not enabled");
  return NULL;
}

static Scheme_Object *processor_count(int argc, Scheme_Object *argv[])
{
  scheme_signal_error("processor-count: not enabled");
  return NULL;
}

static Scheme_Object *start_primitive_tracking(int argc, Scheme_Object *argv[])
{
  scheme_signal_error("start-primitive-tracking: not enabled");
  return NULL;
}

static Scheme_Object *end_primitive_tracking(int argc, Scheme_Object *argv[])
{
  scheme_signal_error("end-primitive-tracking: not enabled");
  return NULL;
}

# define FUTURE_PRIM_W_ARITY(name, func, a1, a2, env) GLOBAL_PRIM_W_ARITY(name, func, a1, a2, env)

void scheme_init_futures(Scheme_Env *env)
{
  Scheme_Env *newenv;
  
  newenv = scheme_primitive_module(scheme_intern_symbol("#%futures"), 
                                   env);

  FUTURE_PRIM_W_ARITY("future",           future,           1, 1, newenv);
  FUTURE_PRIM_W_ARITY("touch",            touch,            1, 1, newenv);
  FUTURE_PRIM_W_ARITY("processor-count",  processor_count,  1, 1, newenv);
  FUTURE_PRIM_W_ARITY("start-primitive-tracking",  start_primitive_tracking,  0, 0, newenv);
  FUTURE_PRIM_W_ARITY("end-primitive-tracking",  end_primitive_tracking,  0, 0, newenv);

  scheme_finish_primitive_module(newenv);
  scheme_protect_primitive_provide(newenv, NULL);
}

#else

#include "future.h"
#include <stdlib.h>
#include <string.h>
#ifdef UNIT_TEST
# include "./tests/unit_test.h"
#endif 

extern void *on_demand_jit_code;

#define THREAD_POOL_SIZE 12
#define INITIAL_C_STACK_SIZE 500000
static pthread_t g_pool_threads[THREAD_POOL_SIZE];
static int *g_fuel_pointers[THREAD_POOL_SIZE];
static unsigned long *g_stack_boundary_pointers[THREAD_POOL_SIZE];
static int *g_need_gc_pointers[THREAD_POOL_SIZE];
static int g_num_avail_threads = 0;
static unsigned long g_cur_cpu_mask = 1;
static void *g_signal_handle = NULL;

static struct NewGC *g_shared_GC;
static future_t *g_future_queue = NULL;
static future_t *g_future_waiting_atomic = NULL;
static Scheme_Object *g_thread_skeleton;
int g_next_futureid = 0;
pthread_t g_rt_threadid = 0;

static pthread_mutex_t g_future_queue_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t g_future_pending_cv = PTHREAD_COND_INITIALIZER;

THREAD_LOCAL_DECL(static pthread_cond_t worker_can_continue_cv);

static pthread_mutex_t gc_ok_m = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t gc_ok_c = PTHREAD_COND_INITIALIZER;
static pthread_cond_t gc_done_c = PTHREAD_COND_INITIALIZER;
static int gc_not_ok, wait_for_gc;
#ifdef MZ_PRECISE_GC
THREAD_LOCAL_DECL(extern unsigned long GC_gen0_alloc_page_ptr);
#endif

static future_t **g_current_ft;
static Scheme_Object ***g_scheme_current_runstack;
static Scheme_Object ***g_scheme_current_runstack_start;
static void **g_jit_future_storage;
static Scheme_Object **g_current_thread;
static int *gc_counter_ptr;
THREAD_LOCAL_DECL(static int worker_gc_counter);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif
extern void scheme_on_demand_generate_lambda(Scheme_Native_Closure *nc, int argc, Scheme_Object **argv);

static void start_gc_not_ok(int with_lock);
static void end_gc_not_ok(future_t *ft, int with_lock);

static void future_do_runtimecall(void *func, int is_atomic);

THREAD_LOCAL_DECL(static future_t *current_ft);

//Stuff for scheme runstack 
//Some of these may mimic defines in thread.c, but are redefined here 
//to avoid making any changes to that file for now (moving anything out into common 
//headers, etc.)
#ifndef DEFAULT_INIT_STACK_SIZE 
#define DEFAULT_INIT_STACK_SIZE 1000 
#endif

//Functions
#ifndef UNIT_TEST
static void *worker_thread_future_loop(void *arg);
static void invoke_rtcall(future_t *future);
static future_t *enqueue_future(future_t *ft);;
static future_t *get_pending_future(void);
static future_t *get_last_future(void);
#else
//Garbage stubs for unit testing 
#define START_XFORM_SKIP 
#define END_XFORM_SKIP 
void scheme_add_global(char *name, int arity, Scheme_Env *env) { }
int scheme_make_prim_w_arity(prim_t func, char *name, int arg1, int arg2) { return 1; }
Scheme_Object *future_touch(int futureid)
{
  Scheme_Object *args[1] = { &futureid };
  return touch(1, args);
}
#endif 

void *g_funcargs[5];
void *func_retval = NULL;


#ifdef MZ_PRECISE_GC
# define scheme_future_setjmp(newbuf) scheme_jit_setjmp((newbuf).jb)
# define scheme_future_longjmp(newbuf, v) scheme_jit_longjmp((newbuf).jb, v)
#else
# define scheme_future_setjmp(newbuf) scheme_setjmp(newbuf)
# define scheme_future_longjmp(newbuf, v) scheme_longjmp(newbuf, v)
#endif


/**********************************************************************/
/* Helpers for debugging                    						  */
/**********************************************************************/
#ifdef DEBUG_FUTURES 
int g_rtcall_count = 0;

static Scheme_Object **get_thread_runstack(void)
{
  return MZ_RUNSTACK;
}


static Scheme_Object **get_thread_runstack_start(void)
{
  return MZ_RUNSTACK_START;
}

void dump_state(void)
{
  future_t *f;
  pthread_mutex_lock(&g_future_queue_mutex);
  printf("\n");
  printf("FUTURES STATE:\n");
  printf("-------------------------------------------------------------\n");
  if (NULL == g_future_queue)
    {
      printf("No futures currently running.  %d thread(s) available in the thread pool.\n\n", g_num_avail_threads);
      pthread_mutex_unlock(&g_future_queue_mutex);
      return;
    }

  for (f = g_future_queue; f != NULL; f = f->next)
    {
      printf("Future %d [Thread: %p, Runstack start = %p, Runstack = %p]: ", f->id, f->threadid, f->runstack_start, f->runstack);		
      fflush(stdout);
      switch (f->status)
        {
        case PENDING: 
          printf("Waiting to be assigned to thread\n");
          break;
        case RUNNING: 
          printf("Executing JIT code\n");
          break;
        case WAITING_FOR_PRIM:
          printf("Waiting for runtime primitive invocation (prim=%p)\n", (void*)f->rt_prim);
          break;
        case FINISHED: 
          printf("Finished work, waiting for cleanup\n");
          break;
        }

      fflush(stdout);
      printf("%d thread(s) available in the thread pool.\n", g_num_avail_threads);
      printf("\n");
      fflush(stdout);
    }

  pthread_mutex_unlock(&g_future_queue_mutex);
}
#endif

/**********************************************************************/
/*   Semaphore helpers                                                */
/**********************************************************************/

typedef struct sema_t {
  int ready;
  pthread_mutex_t m;
  pthread_cond_t c;
} sema_t;

#define SEMA_INITIALIZER { 0, PTHREAD_MUTEX_INITIALIZER,        \
      PTHREAD_COND_INITIALIZER }

static void sema_wait(sema_t *s)
{
  pthread_mutex_lock(&s->m);
  while (!s->ready) {
    pthread_cond_wait(&s->c, &s->m);
  }
  --s->ready;
  pthread_mutex_unlock(&s->m);
}

static void sema_signal(sema_t *s)
{
  pthread_mutex_lock(&s->m);
  s->ready++;
  pthread_cond_signal(&s->c);
  pthread_mutex_unlock(&s->m);
}

static sema_t ready_sema = SEMA_INITIALIZER;

/**********************************************************************/
/* Plumbing for MzScheme initialization                               */
/**********************************************************************/

//Invoked by the runtime on startup to make 
//primitives known
void scheme_init_futures(Scheme_Env *env)
{
  Scheme_Object *v;
  Scheme_Env *newenv;

  futures_init();

  v = scheme_intern_symbol("#%futures");
  newenv = scheme_primitive_module(v, env);

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

  scheme_add_global_constant(
                             "start-primitive-tracking", 
                             scheme_make_prim_w_arity(
                                                      start_primitive_tracking, 
                                                      "start-primitive-tracking", 
                                                      0,
                                                      0), 
                             newenv);

  scheme_add_global_constant(
                             "end-primitive-tracking", 
                             scheme_make_prim_w_arity(
                                                      end_primitive_tracking, 
                                                      "end-primitive-tracking", 
                                                      0,
                                                      0), 
                             newenv);

  scheme_finish_primitive_module(newenv);
  scheme_protect_primitive_provide(newenv, NULL);

  REGISTER_SO(g_future_queue);
  REGISTER_SO(g_future_waiting_atomic);
}


//Setup code here that should be invoked on
//the runtime thread.
void futures_init(void)
{
  int i;
  pthread_t threadid;
  GC_CAN_IGNORE pthread_attr_t attr;
  g_rt_threadid = pthread_self();
  g_signal_handle = scheme_get_signal_handle();

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  //Create the worker thread pool.  These threads will
  //'queue up' and wait for futures to become available   
  pthread_attr_init(&attr);
  pthread_attr_setstacksize(&attr, INITIAL_C_STACK_SIZE); 
  for (i = 0; i < THREAD_POOL_SIZE; i++)
    {
      /* FIXME: insteda of using global variables, we need to
         commuincate though some record. Global variables
         won't work with places, since the relevant values
         are all place-specific. */
      gc_counter_ptr = &scheme_did_gc_count;
      g_shared_GC = GC;

      /* Make enough of a thread record to deal with multiple values. */
      g_thread_skeleton = (Scheme_Object *)MALLOC_ONE_TAGGED(Scheme_Thread);
      g_thread_skeleton->type = scheme_thread_type;

      pthread_create(&threadid, &attr, worker_thread_future_loop, &i);
      sema_wait(&ready_sema);
	
      scheme_register_static(g_current_ft, sizeof(void*));
      scheme_register_static(g_scheme_current_runstack, sizeof(void*));
      scheme_register_static(g_scheme_current_runstack_start, sizeof(void*));	
      scheme_register_static(g_jit_future_storage, 2 * sizeof(void*));
      scheme_register_static(g_current_thread, sizeof(void*));

      g_pool_threads[i] = threadid;
    }

  g_num_avail_threads = THREAD_POOL_SIZE;
}

static void start_gc_not_ok(int with_lock)
{
  if (with_lock)
    pthread_mutex_lock(&gc_ok_m);

  while (wait_for_gc) {
    pthread_cond_wait(&gc_done_c, &gc_ok_m);
  }

  gc_not_ok++;
  if (with_lock)
    pthread_mutex_unlock(&gc_ok_m);
#ifdef MZ_PRECISE_GC
  if (worker_gc_counter != *gc_counter_ptr) {
    GC_gen0_alloc_page_ptr = 0; /* forces future to ask for memory */
    worker_gc_counter = *gc_counter_ptr;
  }
#endif
}

static void end_gc_not_ok(future_t *ft, int with_lock)
{
  if (ft) {
    scheme_set_runstack_limits(ft->runstack_start, 
                               ft->runstack_size,
                               ft->runstack - ft->runstack_start,
                               ft->runstack_size);
  }

  /* FIXME: clear scheme_current_thread->ku.multiple.array ? */

  if (with_lock)
    pthread_mutex_lock(&gc_ok_m);
  --gc_not_ok;
  pthread_cond_signal(&gc_ok_c);
  if (with_lock)
    pthread_mutex_unlock(&gc_ok_m);
}

void scheme_future_block_until_gc()
{
  int i;

  pthread_mutex_lock(&gc_ok_m);
  wait_for_gc = 1;
  pthread_mutex_unlock(&gc_ok_m);

  for (i = 0; i < THREAD_POOL_SIZE; i++) { 
    if (g_fuel_pointers[i] != NULL)
      {
        *(g_need_gc_pointers[i]) = 1;
        *(g_fuel_pointers[i]) = 0;
    	*(g_stack_boundary_pointers[i]) += INITIAL_C_STACK_SIZE;
      }    
  }
  asm("mfence");

  pthread_mutex_lock(&gc_ok_m);
  while (gc_not_ok) {
    pthread_cond_wait(&gc_ok_c, &gc_ok_m);
  }
  pthread_mutex_unlock(&gc_ok_m);
}

void scheme_future_continue_after_gc()
{
  int i;

  for (i = 0; i < THREAD_POOL_SIZE; i++) {
    if (g_fuel_pointers[i] != NULL)
      {
        *(g_need_gc_pointers[i]) = 0;
        *(g_fuel_pointers[i]) = 1;
    	*(g_stack_boundary_pointers[i]) -= INITIAL_C_STACK_SIZE;
      }
    
  }

  pthread_mutex_lock(&gc_ok_m);
  wait_for_gc = 0;
  pthread_cond_broadcast(&gc_done_c);
  pthread_mutex_unlock(&gc_ok_m);
}

void scheme_future_gc_pause()
/* Called in future thread */
{
  pthread_mutex_lock(&gc_ok_m);
  end_gc_not_ok(current_ft, 0);
  start_gc_not_ok(0); /* waits until wait_for_gc is 0 */
  pthread_mutex_unlock(&gc_ok_m);  
}

/**********************************************************************/
/* Primitive implementations                    					  */
/**********************************************************************/

static long start_ms = 0;

Scheme_Object *start_primitive_tracking(int argc, Scheme_Object *argv[])
{
  //Get the start time
  struct timeval now;
  long ms;
  gettimeofday(&now, NULL);
	
  start_ms = now.tv_usec / 1000.0;
	
  g_print_prims = 1;
  printf("Primitive tracking started at ");
  print_ms_and_us();
  printf("\n");
  return scheme_void;
}

Scheme_Object *end_primitive_tracking(int argc, Scheme_Object *argv[])
{
  g_print_prims = 0;
  printf("Primitive tracking ended at ");
  print_ms_and_us();
  printf("\n");
  return scheme_void;
}

void print_ms_and_us()
{
  struct timeval now;
  long ms, us;
  gettimeofday(&now, NULL);

  //ms = (now.tv_sec * 1000.0) - start_ms;
  ms = (now.tv_usec / 1000) - start_ms;
  us = now.tv_usec - (ms * 1000) - (start_ms * 1000);
  printf("%ld.%ld", ms, us);
}

Scheme_Object *future(int argc, Scheme_Object *argv[])
/* Called in runtime thread */
{
#ifdef DEBUG_FUTURES 
  LOG_THISCALL;
#endif

  int init_runstack_size;
  int futureid;
  future_t *ft;
  Scheme_Native_Closure *nc;
  Scheme_Native_Closure_Data *ncd;
  Scheme_Object *lambda = argv[0];

  //Input validation
  scheme_check_proc_arity("future", 0, 0, argc, argv);

  nc = (Scheme_Native_Closure*)lambda;
  ncd = nc->code;

  //Create the future descriptor and add to the queue as 'pending'    
  ft = MALLOC_ONE_TAGGED(future_t);     
  ft->so.type = scheme_future_type;

  futureid = ++g_next_futureid;
  ft->id = futureid;
  ft->orig_lambda = lambda;
  ft->status = PENDING;
   
  //Allocate a new scheme stack for the future 
  //init_runstack_size = MZ_RUNSTACK - MZ_RUNSTACK_START;
  init_runstack_size = 1000;

#ifdef DEBUG_FUTURES
  printf("Allocating Scheme stack of %d bytes for future %d.\n", init_runstack_size, futureid); 
#endif

  {
    Scheme_Object **rs_start, **rs;
    rs_start = scheme_alloc_runstack(init_runstack_size);
    rs = rs_start XFORM_OK_PLUS init_runstack_size;
    ft->runstack_start = rs_start;
    ft->runstack = rs;
    ft->runstack_size = init_runstack_size;
  }   

  //JIT compile the code if not already jitted
  //Temporarily repoint MZ_RUNSTACK
  //to the worker thread's runstack -
  //in case the JIT compiler uses the stack address
  //when generating code
  if (ncd->code == on_demand_jit_code)
    {
      scheme_on_demand_generate_lambda(nc, 0, NULL);
    }

  ft->code = (void*)ncd->code;

  pthread_mutex_lock(&g_future_queue_mutex);
  enqueue_future(ft);
  //Signal that a future is pending
  pthread_cond_signal(&g_future_pending_cv);
  pthread_mutex_unlock(&g_future_queue_mutex);

  return (Scheme_Object*)ft;
}


int future_ready(Scheme_Object *obj)
/* Called in runtime thread by Scheme scheduler */
{
  int ret = 0;
  future_t *ft = (future_t*)obj;
  pthread_mutex_lock(&g_future_queue_mutex);
  if (ft->work_completed || ft->rt_prim)
    {
      ret = 1;
    }

  pthread_mutex_unlock(&g_future_queue_mutex);
  return ret;
}

static void dequeue_future(future_t *ft)
/* called from both future and runtime threads */
{
  START_XFORM_SKIP;
  if (ft->prev == NULL)
    {
      //Set next to be the head of the queue
      g_future_queue = ft->next;
      if (g_future_queue != NULL)
        g_future_queue->prev = NULL;
    }
  else
    {
      ft->prev->next = ft->next;
      if (NULL != ft->next)
        ft->next->prev = ft->prev;
    }
  END_XFORM_SKIP;
}


Scheme_Object *touch(int argc, Scheme_Object *argv[])
/* Called in runtime thread */
{
  Scheme_Object *retval = NULL;
  future_t *ft;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_future_type))
    {
      scheme_wrong_type("touch", "future", 0, argc, argv);
    }

  ft = (future_t*)argv[0];

#ifdef DEBUG_FUTURES 
  LOG("touch (future %d)", futureid);	
  dump_state();
#endif

  pthread_mutex_lock(&g_future_queue_mutex);
  if (ft->status == PENDING) {
    ft->status = RUNNING;
    pthread_mutex_unlock(&g_future_queue_mutex);

    retval = _scheme_apply(ft->orig_lambda, 0, NULL);

    pthread_mutex_lock(&g_future_queue_mutex);
    ft->work_completed = 1;
    ft->retval = retval;
    ft->status = FINISHED;
    dequeue_future(ft);
    pthread_mutex_unlock(&g_future_queue_mutex);

    return retval;
  }
  pthread_mutex_unlock(&g_future_queue_mutex);

  //Spin waiting for primitive calls or a return value from
  //the worker thread
 wait_for_rtcall_or_completion:
  scheme_block_until(future_ready, NULL, (Scheme_Object*)ft, 0);
  pthread_mutex_lock(&g_future_queue_mutex);
  if (ft->work_completed)
    {
      retval = ft->retval;

      LOG("Successfully touched future %d\n", ft->id);
      // fflush(stdout);

      //Increment the number of available pool threads
      g_num_avail_threads++;	
      pthread_mutex_unlock(&g_future_queue_mutex);
    }
  else if (ft->rt_prim)
    {
      //Invoke the primitive and stash the result
      //Release the lock so other threads can manipulate the queue
      //while the runtime call executes
      pthread_mutex_unlock(&g_future_queue_mutex);
      LOG("Invoking primitive %p on behalf of future %d...", ft->rt_prim, ft->id);
      invoke_rtcall(ft);
      LOG("done.\n");

      goto wait_for_rtcall_or_completion;
    }
  else
    {
      pthread_mutex_unlock(&g_future_queue_mutex);
      goto wait_for_rtcall_or_completion;
    }

  if (!retval) {
    scheme_signal_error("touch: future previously aborted");
  }

  return retval;
}

#ifdef linux 
#include <unistd.h>
#elif OS_X 
#include <sys/sysctl.h>
#elif WINDOWS 
#include <windows.h>
#endif 

Scheme_Object *processor_count(int argc, Scheme_Object *argv[])
/* Called in runtime thread */
{
  int cpucount = 0;

#ifdef linux 
  cpucount = sysconf(_SC_NPROCESSORS_ONLN);
#elif OS_X 
  int mib[4];
  size_t len; 

  /* set the mib for hw.ncpu */
  mib[0] = CTL_HW;
  mib[1] = HW_AVAILCPU;  // alternatively, try HW_NCPU;

  /* get the number of CPUs from the system */
  sysctl(mib, 2, &cpucount, &len, NULL, 0);
  if (cpucount < 1) 
  {
    mib[1] = HW_NCPU;
    sysctl(mib, 2, &cpucount, &len, NULL, 0);
    if(cpucount < 1)
    {
      cpucount = 1;
    }
  }
#elif WINDOWS 
  SYSTEM_INFO sysinfo;
  GetSystemInfo(&sysinfo);
  cpucount = sysinfo.dwNumberOfProcessors;
#else
  cpucount = THREAD_POOL_SIZE;
#endif

  return scheme_make_integer(cpucount);
}

//Entry point for a worker thread allocated for
//executing futures.  This function will never terminate
//(until the process dies).
void *worker_thread_future_loop(void *arg)
/* Called in future thread; runtime thread is blocked until ready_sema
  is signaled. */
{
  START_XFORM_SKIP;
  Scheme_Object *v;
  Scheme_Object* (*jitcode)(Scheme_Object*, int, Scheme_Object**);
  future_t *ft;
  int id = *(int *)arg;
  mz_jmp_buf newbuf;

  scheme_init_os_thread();

  GC = g_shared_GC;
  scheme_current_thread = g_thread_skeleton;

  //Set processor affinity
  /*pthread_mutex_lock(&g_future_queue_mutex);
    if (pthread_setaffinity_np(pthread_self(), sizeof(g_cur_cpu_mask), &g_cur_cpu_mask))
    {
    printf(
    "Could not set CPU affinity (%lu) for thread %p!\n", 
    ++g_cur_cpu_mask, 
    pthread_self());
    }

    pthread_mutex_unlock(&g_future_queue_mutex);
  */

  pthread_cond_init(&worker_can_continue_cv, NULL);

  scheme_use_rtcall = 1;

  scheme_fuel_counter = 1;
  scheme_jit_stack_boundary = ((unsigned long)&v) - INITIAL_C_STACK_SIZE;

  g_need_gc_pointers[id] = &scheme_future_need_gc_pause;
  g_fuel_pointers[id] = &scheme_fuel_counter;
  g_stack_boundary_pointers[id] = &scheme_jit_stack_boundary;

  g_current_ft = &current_ft;
  g_scheme_current_runstack = &scheme_current_runstack;
  g_scheme_current_runstack_start = &scheme_current_runstack_start;
  g_jit_future_storage = &jit_future_storage[0];
  g_current_thread = &scheme_current_thread;
  sema_signal(&ready_sema);

 wait_for_work:
  start_gc_not_ok(1);
  pthread_mutex_lock(&g_future_queue_mutex);
  while (!(ft = get_pending_future()))
    {
      end_gc_not_ok(NULL, 1);
      pthread_cond_wait(&g_future_pending_cv, &g_future_queue_mutex);
      start_gc_not_ok(1);
    }

  LOG("Got a signal that a future is pending...");
        
  //Work is available for this thread
  ft->status = RUNNING;
  pthread_mutex_unlock(&g_future_queue_mutex);

  ft->threadid = pthread_self();

  //Decrement the number of available pool threads 
  g_num_avail_threads--;

  //Initialize the runstack for this thread 
  //MZ_RUNSTACK AND MZ_RUNSTACK_START should be thread-local
  MZ_RUNSTACK = ft->runstack;
  MZ_RUNSTACK_START = ft->runstack_start;		

  //Set up the JIT compiler for this thread 
  scheme_jit_fill_threadlocal_table();
        
  jitcode = (Scheme_Object* (*)(Scheme_Object*, int, Scheme_Object**))(ft->code);

  current_ft = ft;

  //Run the code
  //Passing no arguments for now.
  //The lambda passed to a future will always be a parameterless
  //function.
  //From this thread's perspective, this call will never return
  //until all the work to be done in the future has been completed,
  //including runtime calls. 
  //If jitcode asks the runrtime thread to do work, then
  //a GC can occur.
  LOG("Running JIT code at %p...\n", ft->code);    

  scheme_current_thread->error_buf = &newbuf;
  if (scheme_future_setjmp(newbuf)) {
    /* failed */
    v = NULL;
  } else {
    v = jitcode(ft->orig_lambda, 0, NULL);
  }

  LOG("Finished running JIT code at %p.\n", ft->code);

  // Get future again, since a GC may have occurred
  ft = current_ft;

  //Set the return val in the descriptor
  pthread_mutex_lock(&g_future_queue_mutex);
  ft->work_completed = 1;
  ft->retval = v;

  ft->runstack = NULL;
  ft->runstack_start = NULL;

  //Update the status 
  ft->status = FINISHED;
  dequeue_future(ft);

  scheme_signal_received_at(g_signal_handle);
  pthread_mutex_unlock(&g_future_queue_mutex);

  end_gc_not_ok(NULL, 1);

  goto wait_for_work;

  return NULL;
  END_XFORM_SKIP;
}

void scheme_check_future_work()
/* Called in the runtime thread by the scheduler */
{
  /* Check for work that future threads need from the runtime thread
     and that can be done in any Scheme thread (e.g., get a new page
     for allocation). */
  future_t *ft;

  while (1) {
    /* Try to get a future waiting on a atomic operation */
    pthread_mutex_lock(&g_future_queue_mutex);
    ft = g_future_waiting_atomic;
    if (ft) {
      g_future_waiting_atomic = ft->next_waiting_atomic;
    }
    pthread_mutex_unlock(&g_future_queue_mutex);

    if (ft && ft->rt_prim && ft->rt_prim_is_atomic) {
      invoke_rtcall(ft);
    } else
      break;
  }
  
}

//Returns 0 if the call isn't actually executed by this function,
//i.e. if we are already running on the runtime thread.  Otherwise returns
//1, and 'retval' is set to point to the return value of the runtime
//call invocation.
void future_do_runtimecall(void *func,
                           int is_atomic)
/* Called in future thread */
{
  START_XFORM_SKIP;
  future_t *future;

  //If already running on the main thread
  //or no future is involved, do nothing
  //and return FALSE
  if (pthread_self() == g_rt_threadid)
    {
      //Should never get here!  This check should be done 
      //by the caller using the macros defined in scheme-futures.h!
      return;
    }

  //Fetch the future descriptor for this thread
  future = current_ft;

  //set up the arguments for the runtime call
  //to be picked up by the main rt thread
  //pthread_mutex_lock(&future->mutex);
  pthread_mutex_lock(&g_future_queue_mutex);

  //Update the stack pointer for this future 
  //to be in sync with MZ_RUNSTACK - the runtime thread 
  //will use this value to temporarily swap its stack 
  //for the worker thread's
  future->runstack = MZ_RUNSTACK;
  future->prim_func = func;
  future->rt_prim = 1;
  future->rt_prim_is_atomic = is_atomic;

  if (is_atomic) {
    future->next_waiting_atomic = g_future_waiting_atomic;
    g_future_waiting_atomic = future;
  }

  //Update the future's status to waiting 
  future->status = WAITING_FOR_PRIM;

  scheme_signal_received_at(g_signal_handle);

  //Wait for the signal that the RT call is finished
  future->can_continue_cv = &worker_can_continue_cv;
  end_gc_not_ok(future, 1);
  pthread_cond_wait(&worker_can_continue_cv, &g_future_queue_mutex);
  start_gc_not_ok(1);

  //Fetch the future instance again, in case the GC has moved the pointer
  future = current_ft;

  pthread_mutex_unlock(&g_future_queue_mutex);

  if (future->no_retval) {
    future->no_retval = 0;
    scheme_future_longjmp(*scheme_current_thread->error_buf, 1);
  }

  END_XFORM_SKIP;
}


/**********************************************************************/
/* Functions for primitive invocation                   			  */
/**********************************************************************/
void rtcall_void_void_3args(void (*f)())
/* Called in future thread */
{
  START_XFORM_SKIP;

  current_ft->prim_protocol = SIG_VOID_VOID_3ARGS;

  future_do_runtimecall((void*)f, 1);

  END_XFORM_SKIP;
}

void *rtcall_alloc_void_pvoid(void (*f)())
/* Called in future thread */
{
  START_XFORM_SKIP;
  future_t *future;
  void *retval;

  while (1) {
    current_ft->prim_protocol = SIG_ALLOC_VOID_PVOID;

    future_do_runtimecall((void*)f, 1);

    future = current_ft;
    retval = future->alloc_retval;
    future->alloc_retval = NULL;

    if (*gc_counter_ptr == future->alloc_retval_counter)
      break;
  }

  return retval;
  END_XFORM_SKIP;
}

static void receive_special_result(future_t *f, Scheme_Object *retval)
{
  if (SAME_OBJ(retval, SCHEME_MULTIPLE_VALUES)) {
    Scheme_Thread *p = scheme_current_thread;

    p->ku.multiple.array = f->multiple_array;
    p->ku.multiple.count = f->multiple_count;
    f->multiple_array = NULL;
  } else if (SAME_OBJ(retval, SCHEME_TAIL_CALL_WAITING)) {
    Scheme_Thread *p = scheme_current_thread;

    p->ku.apply.tail_rator = f->tail_rator;
    p->ku.apply.tail_rands = f->tail_rands;
    p->ku.apply.tail_num_rands = f->num_tail_rands;
  }
}

#include "jit_ts_future_glue.c"

static void send_special_result(future_t *f, Scheme_Object *retval)
{
  if (SAME_OBJ(retval, SCHEME_MULTIPLE_VALUES)) {
    Scheme_Thread *p = scheme_current_thread;

    f->multiple_array = p->ku.multiple.array;
    f->multiple_count = p->ku.multiple.count;
    if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
      p->values_buffer = NULL;
  } else if (SAME_OBJ(retval, SCHEME_TAIL_CALL_WAITING)) {
    Scheme_Thread *p = scheme_current_thread;

    f->tail_rator = p->ku.apply.tail_rator;
    f->tail_rands = p->ku.apply.tail_rands;
    f->num_tail_rands = p->ku.apply.tail_num_rands;
    p->ku.apply.tail_rator = NULL;
    p->ku.apply.tail_rands = NULL;
  }
}

//Does the work of actually invoking a primitive on behalf of a 
//future.  This function is always invoked on the main (runtime) 
//thread.
static void do_invoke_rtcall(future_t *future)
/* Called in runtime thread */
{
#ifdef DEBUG_FUTURES
  g_rtcall_count++;
#endif

  future->rt_prim = 0;

  switch (future->prim_protocol)
    {
    case SIG_VOID_VOID_3ARGS:
      {
        prim_void_void_3args_t func = (prim_void_void_3args_t)future->prim_func;

        func(future->runstack);

        break;
      }
    case SIG_ALLOC_VOID_PVOID:
      {
        void *ret;
        prim_alloc_void_pvoid_t func = (prim_alloc_void_pvoid_t)future->prim_func;
        ret = func();
        future->alloc_retval = ret;
        ret = NULL;
        future->alloc_retval_counter = scheme_did_gc_count;
        break;
      }
# include "jit_ts_runtime_glue.c"
    default:
      scheme_signal_error("unknown protocol %d", future->prim_protocol);
      break;
    }

  pthread_mutex_lock(&g_future_queue_mutex);
  //Signal the waiting worker thread that it
  //can continue running machine code
  pthread_cond_signal(future->can_continue_cv);
  pthread_mutex_unlock(&g_future_queue_mutex);
}

static void invoke_rtcall(future_t * volatile future)
{
  Scheme_Thread *p = scheme_current_thread;
  mz_jmp_buf newbuf, * volatile savebuf;

  savebuf = p->error_buf;
  p->error_buf = &newbuf;
  if (scheme_setjmp(newbuf)) {
    pthread_mutex_lock(&g_future_queue_mutex);
    future->no_retval = 1;
    //Signal the waiting worker thread that it
    //can continue running machine code
    pthread_cond_signal(future->can_continue_cv);
    pthread_mutex_unlock(&g_future_queue_mutex);
    scheme_longjmp(*savebuf, 1);
  } else {
    do_invoke_rtcall(future);
  }
  p->error_buf = savebuf;
}


/**********************************************************************/
/* Helpers for manipulating the futures queue                         */
/**********************************************************************/

future_t *enqueue_future(future_t *ft)
/* Called in runtime thread */
{
  future_t *last;
  last = get_last_future();
  if (NULL == last)
    {
      g_future_queue = ft;
      return ft;
    }

  ft->prev = last;
  last->next = ft;
  ft->next = NULL;
    
  return ft;
}


future_t *get_pending_future(void)
/* Called in future thread */
{
  START_XFORM_SKIP;
  future_t *f;
  for (f = g_future_queue; f != NULL; f = f->next)
    {
      if (f->status == PENDING)
        return f;
    }

  return NULL;
  END_XFORM_SKIP;
}

future_t *get_last_future(void)
/* Called in runtime thread */
{
  future_t *ft = g_future_queue;
  if (NULL == ft)
    {
      return ft;
    }
    
  while (ft->next != NULL)
    {
      ft = ft->next;
    }

  return ft;
}


void clear_futures(void)
{
  int i;
  future_t *f, *tmp;
  pthread_mutex_lock(&g_future_queue_mutex);
  for (i = 0; i < THREAD_POOL_SIZE; i++)
    {
      pthread_cancel(g_pool_threads[i]);
    }

  pthread_mutex_unlock(&g_future_queue_mutex);
  f = get_last_future();
  if (NULL == f)
    return;

  while (1)
    {
      tmp = f->prev;
      free(f);
      if (tmp == NULL)
        {
          break;
        }

      tmp->next = NULL;
      f = tmp;
    }

  g_future_queue = NULL;
}

/**********************************************************************/
/*                           Precise GC                               */
/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_FUTURE_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_future_type, future);
}

END_XFORM_SKIP;

#endif

#endif
