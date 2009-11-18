
#ifndef UNIT_TEST
# include "schpriv.h"
#endif

#ifdef INSTRUMENT_PRIMITIVES 
int g_print_prims = 0;
#endif

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

# define FUTURE_PRIM_W_ARITY(name, func, a1, a2, env) GLOBAL_PRIM_W_ARITY(name, func, a1, a2, env)

void scheme_init_futures(Scheme_Env *env)
{
  Scheme_Env *newenv;
  
  newenv = scheme_primitive_module(scheme_intern_symbol("#%futures"), 
                                   env);

  FUTURE_PRIM_W_ARITY("future",      future,       1, 1, newenv);
  FUTURE_PRIM_W_ARITY("touch",       touch,        1, 1, newenv);

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

#define THREAD_POOL_SIZE 3
#define INITIAL_C_STACK_SIZE 500000
static pthread_t g_pool_threads[THREAD_POOL_SIZE];
static int *g_fuel_pointers[THREAD_POOL_SIZE];
static unsigned long *g_stack_boundary_pointers[THREAD_POOL_SIZE];
static int g_num_avail_threads = 0;
static unsigned long g_cur_cpu_mask = 1;
static void *g_signal_handle = NULL;

future_t *g_future_queue = NULL;
int g_next_futureid = 0;
pthread_t g_rt_threadid = 0;

static pthread_mutex_t g_future_queue_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t g_future_pending_cv = PTHREAD_COND_INITIALIZER;

THREAD_LOCAL_DECL(static pthread_cond_t worker_can_continue_cv);

static pthread_mutex_t gc_ok_m = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t gc_ok_c = PTHREAD_COND_INITIALIZER;
static int gc_not_ok;
#ifdef MZ_PRECISE_GC
THREAD_LOCAL_DECL(extern unsigned long GC_gen0_alloc_page_ptr);
#endif

static future_t **g_current_ft;
static Scheme_Object ***g_scheme_current_runstack;
static Scheme_Object ***g_scheme_current_runstack_start;
static void **g_jit_future_storage;
static int *gc_counter_ptr;
THREAD_LOCAL_DECL(static int worker_gc_counter);

static void register_traversers(void);
extern void scheme_on_demand_generate_lambda(Scheme_Native_Closure *nc, int argc, Scheme_Object **argv);

static void start_gc_not_ok();
static void end_gc_not_ok();

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
static void *invoke_rtcall(future_t *future);
static future_t *enqueue_future(void);
static future_t *get_pending_future(void);
static future_t *get_my_future(void);
static future_t *get_future_by_threadid(pthread_t threadid);
static future_t *get_future(int futureid);
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

#define SEMA_INITIALIZER { 0, PTHREAD_MUTEX_INITIALIZER, \
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
		"num-processors", 
		scheme_make_prim_w_arity(
			num_processors, 
			"num-processors", 
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

	#ifdef INSTRUMENT_PRIMITIVES
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
	#endif

	scheme_finish_primitive_module(newenv);
	scheme_protect_primitive_provide(newenv, NULL);

  REGISTER_SO(g_future_queue);
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
      gc_counter_ptr = &scheme_did_gc_count;
      pthread_create(&threadid, &attr, worker_thread_future_loop, &i);
      sema_wait(&ready_sema);
	
			scheme_register_static(g_current_ft, sizeof(void*));
			scheme_register_static(g_scheme_current_runstack, sizeof(void*));
			scheme_register_static(g_scheme_current_runstack_start, sizeof(void*));	
                        scheme_register_static(g_jit_future_storage, 2 * sizeof(void*));

      g_pool_threads[i] = threadid;
  }

	g_num_avail_threads = THREAD_POOL_SIZE;
}

static void start_gc_not_ok()
{
  pthread_mutex_lock(&gc_ok_m);
  gc_not_ok++;
  pthread_mutex_unlock(&gc_ok_m);
#ifdef MZ_PRECISE_GC
  if (worker_gc_counter != *gc_counter_ptr) {
    GC_gen0_alloc_page_ptr = 0; /* forces future to ask for memory */
    worker_gc_counter = *gc_counter_ptr;
  }
#endif
}

static void end_gc_not_ok()
{
  pthread_mutex_lock(&gc_ok_m);
  --gc_not_ok;
  pthread_cond_signal(&gc_ok_c);
  pthread_mutex_unlock(&gc_ok_m);
}

void scheme_future_block_until_gc()
{
  int i;

  for (i = 0; i < THREAD_POOL_SIZE; i++) { 
		if (g_fuel_pointers[i] != NULL)
		{
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
			*(g_fuel_pointers[i]) = 1;
    	*(g_stack_boundary_pointers[i]) -= INITIAL_C_STACK_SIZE;
		}
    
  }
}

/**********************************************************************/
/* Primitive implementations                    					  */
/**********************************************************************/

#ifdef INSTRUMENT_PRIMITIVES
long start_ms = 0;

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
#endif

Scheme_Object *future(int argc, Scheme_Object *argv[])
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
    pthread_mutex_lock(&g_future_queue_mutex);
    ft = enqueue_future();
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
		}   

    //pthread_mutex_unlock(&g_future_queue_mutex);

    //JIT compile the code if not already jitted
    //Temporarily repoint MZ_RUNSTACK
    //to the worker thread's runstack -
    //in case the JIT compiler uses the stack address
    //when generating code
		if (ncd->code == on_demand_jit_code)
		{
			scheme_on_demand_generate_lambda(nc, 0, NULL);
		}

		//pthread_mutex_lock(&g_future_queue_mutex);
		ft->code = (void*)ncd->code;

    //Signal that a future is pending
    pthread_cond_signal(&g_future_pending_cv);
    pthread_mutex_unlock(&g_future_queue_mutex);

		return (Scheme_Object*)ft;
}


Scheme_Object *num_processors(int argc, Scheme_Object *argv[])
{
	return scheme_make_integer(THREAD_POOL_SIZE);
}


int future_ready(Scheme_Object *obj)
{
	int ret = 0;
	future_t *ft = (future_t*)obj;
	pthread_mutex_lock(&g_future_queue_mutex);
	if (ft->work_completed || ft->rt_prim != NULL)
	{
		ret = 1;
	}

	pthread_mutex_unlock(&g_future_queue_mutex);
	return ret;
}


Scheme_Object *touch(int argc, Scheme_Object *argv[])
{
    Scheme_Object *retval = NULL;
    void *rtcall_retval = NULL;
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

    //Spin waiting for primitive calls or a return value from
    //the worker thread
    wait_for_rtcall_or_completion:
				scheme_block_until(future_ready, NULL, (Scheme_Object*)ft, 0);
        pthread_mutex_lock(&g_future_queue_mutex);
        if (ft->work_completed)
        {
            retval = ft->retval;

						LOG("Successfully touched future %d\n", ft->id);
						fflush(stdout);

            //Destroy the future descriptor
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

						//Increment the number of available pool threads
						g_num_avail_threads++;	
            pthread_mutex_unlock(&g_future_queue_mutex);
        }
        else if (ft->rt_prim != NULL)
        {
            //Invoke the primitive and stash the result
            //Release the lock so other threads can manipulate the queue
            //while the runtime call executes
            pthread_mutex_unlock(&g_future_queue_mutex);
						LOG("Invoking primitive %p on behalf of future %d...", ft->rt_prim, ft->id);
            rtcall_retval = invoke_rtcall(ft);
						LOG("done.\n");
            pthread_mutex_lock(&g_future_queue_mutex);

            ft->rt_prim_retval = rtcall_retval;
            ft->rt_prim = NULL;

            //Signal the waiting worker thread that it
            //can continue running machine code
            pthread_cond_signal(ft->can_continue_cv);
            pthread_mutex_unlock(&g_future_queue_mutex);

            goto wait_for_rtcall_or_completion;
        }
        else
        {
            pthread_mutex_unlock(&g_future_queue_mutex);
            goto wait_for_rtcall_or_completion;
        }

    return retval;
}

//Entry point for a worker thread allocated for
//executing futures.  This function will never terminate
//(until the process dies).
void *worker_thread_future_loop(void *arg)
{
	START_XFORM_SKIP;
	Scheme_Object *v;
	Scheme_Object* (*jitcode)(Scheme_Object*, int, Scheme_Object**);
        future_t *ft;
        int id = *(int *)arg;

        scheme_init_os_thread();

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

        scheme_fuel_counter = 1;
        scheme_jit_stack_boundary = ((unsigned long)&v) - INITIAL_C_STACK_SIZE;

        g_fuel_pointers[id] = &scheme_fuel_counter;
        g_stack_boundary_pointers[id] = &scheme_jit_stack_boundary;

				g_current_ft = &current_ft;
				g_scheme_current_runstack = &scheme_current_runstack;
				g_scheme_current_runstack_start = &scheme_current_runstack_start;
                                g_jit_future_storage = &jit_future_storage[0];
        sema_signal(&ready_sema);

    wait_for_work:
        start_gc_not_ok();
        pthread_mutex_lock(&g_future_queue_mutex);
				while (!(ft = get_pending_future()))
				{
          end_gc_not_ok();
					pthread_cond_wait(&g_future_pending_cv, &g_future_queue_mutex);
          start_gc_not_ok();
				}

        LOG("Got a signal that a future is pending...");
        
        //Work is available for this thread
				ft->status = RUNNING;
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
        pthread_mutex_unlock(&g_future_queue_mutex);

        current_ft = ft;

        //Run the code
        //Passing no arguments for now.
        //The lambda passed to a future will always be a parameterless
        //function.
        //From this thread's perspective, this call will never return
        //until all the work to be done in the future has been completed,
        //including runtime calls. 
				LOG("Running JIT code at %p...\n", ft->code);    
				v = jitcode(ft->orig_lambda, 0, NULL);
				LOG("Finished running JIT code at %p.\n", ft->code);
      	ft = current_ft;

        //Set the return val in the descriptor
        pthread_mutex_lock(&g_future_queue_mutex);
        ft->work_completed = 1;
        ft->retval = v;

				//Update the status 
				ft->status = FINISHED;
				scheme_signal_received_at(g_signal_handle);
        pthread_mutex_unlock(&g_future_queue_mutex);

        end_gc_not_ok();

        goto wait_for_work;

    return NULL;
	END_XFORM_SKIP;
}


//Returns 0 if the call isn't actually executed by this function,
//i.e. if we are already running on the runtime thread.  Otherwise returns
//1, and 'retval' is set to point to the return value of the runtime
//call invocation.
int future_do_runtimecall(
    void *func,
    //int sigtype,
    //void *args,
    void *retval)
{
		future_t *future;

    //If already running on the main thread
    //or no future is involved, do nothing
    //and return FALSE
    if (pthread_self() == g_rt_threadid)
    {
		//Should never get here!  This check should be done 
		//by the caller using the macros defined in scheme-futures.h!
        return 0;
    }

    //Fetch the future descriptor for this thread
    future = get_my_future();

    //set up the arguments for the runtime call
    //to be picked up by the main rt thread
    //pthread_mutex_lock(&future->mutex);
    pthread_mutex_lock(&g_future_queue_mutex);

		//Update the stack pointer for this future 
		//to be in sync with MZ_RUNSTACK - the runtime thread 
		//will use this value to temporarily swap its stack 
		//for the worker thread's
		future->runstack = MZ_RUNSTACK;
    future->rt_prim = func;

		//Update the future's status to waiting 
		future->status = WAITING_FOR_PRIM;

		scheme_signal_received_at(g_signal_handle);

    //Wait for the signal that the RT call is finished
                future->can_continue_cv = &worker_can_continue_cv;
    end_gc_not_ok();
    pthread_cond_wait(&worker_can_continue_cv, &g_future_queue_mutex);
    start_gc_not_ok();

		//Fetch the future instance again, in case the GC has moved the pointer
		future = get_my_future();

    //Clear rt call fields before releasing the lock on the descriptor
    future->rt_prim = NULL;

    retval = future->rt_prim_retval;
    future->rt_prim_retval = NULL;
    pthread_mutex_unlock(&g_future_queue_mutex);
    return 1;
}


/**********************************************************************/
/* Functions for primitive invocation                   			  */
/**********************************************************************/
int rtcall_void_void(void (*f)())
{
	START_XFORM_SKIP;
	future_t *future;
	prim_data_t data;
	memset(&data, 0, sizeof(prim_data_t));
	if (!IS_WORKER_THREAD)
	{
		return 0;
	}

	data.void_void = f;
	data.sigtype = SIG_VOID_VOID;

	future = get_my_future();
	future->rt_prim = (void*)f;
	future->prim_data = data;

	future_do_runtimecall((void*)f, NULL);

	return 1;
	END_XFORM_SKIP;
}


int rtcall_alloc_void_pvoid(void (*f)(), void **retval)
{
	START_XFORM_SKIP;
	future_t *future;
	prim_data_t data;

        while (1) {
          memset(&data, 0, sizeof(prim_data_t));
          if (!IS_WORKER_THREAD)
            {
              return 0;
            }

          data.alloc_void_pvoid = f;
          data.sigtype = SIG_ALLOC_VOID_PVOID;

          future = get_my_future();
          future->rt_prim = (void*)f;
          future->prim_data = data;

          future_do_runtimecall((void*)f, NULL);

          *retval = future->alloc_retval;
          future->alloc_retval = NULL;

          if (*gc_counter_ptr == future->alloc_retval_counter)
            break;
        }

	return 1;
	END_XFORM_SKIP;
}


int rtcall_obj_int_pobj_obj(
	prim_obj_int_pobj_obj_t f, 
	Scheme_Object *rator, 
	int argc, 
	Scheme_Object **argv, 
	Scheme_Object **retval)
{
	START_XFORM_SKIP;
	future_t *future;
	prim_data_t data;
	memset(&data, 0, sizeof(prim_data_t));
	if (!IS_WORKER_THREAD)	
	{
		return 0;
	}

	#ifdef DEBUG_FUTURES
	printf("scheme_fuel_counter = %d\n", scheme_fuel_counter);
	printf("scheme_jit_stack_boundary = %p\n", (void*)scheme_jit_stack_boundary);
	printf("scheme_current_runstack = %p\n", scheme_current_runstack);
	printf("scheme_current_runstack_start = %p\n", scheme_current_runstack_start);
	printf("stack address = %p\n", &future);
	#endif

	data.obj_int_pobj_obj = f;
	data.p = rator;
	data.argc = argc;
	data.argv = argv;
	data.sigtype = SIG_OBJ_INT_POBJ_OBJ;
	
	future = get_my_future();
	future->rt_prim = (void*)f;
	future->prim_data = data;

	future_do_runtimecall((void*)f, NULL);
	*retval = future->prim_data.retval;
        future->prim_data.retval = NULL;

	return 1;
	END_XFORM_SKIP;
}


int rtcall_int_pobj_obj(
	prim_int_pobj_obj_t f, 
	int argc, 
	Scheme_Object **argv, 
	Scheme_Object **retval)
{
	START_XFORM_SKIP;
	future_t *future;
	prim_data_t data;
	memset(&data, 0, sizeof(prim_data_t));
	if (!IS_WORKER_THREAD)	
	{
		return 0;
	}

	#ifdef DEBUG_FUTURES
	printf("scheme_fuel_counter = %d\n", scheme_fuel_counter);
	printf("scheme_jit_stack_boundary = %p\n", (void*)scheme_jit_stack_boundary);
	printf("scheme_current_runstack = %p\n", scheme_current_runstack);
	printf("scheme_current_runstack_start = %p\n", scheme_current_runstack_start);
	printf("stack address = %p\n", &future);
	#endif

	data.int_pobj_obj = f;
	data.argc = argc;
	data.argv = argv;
	data.sigtype = SIG_INT_OBJARR_OBJ;
	
	future = get_my_future();
	future->rt_prim = (void*)f;
	future->prim_data = data;

	future_do_runtimecall((void*)f, NULL);
	*retval = future->prim_data.retval;
        future->prim_data.retval = NULL;

	return 1;
	END_XFORM_SKIP;
}


int rtcall_pvoid_pvoid_pvoid(
	prim_pvoid_pvoid_pvoid_t f,
	void *a, 
	void *b, 
	void **retval)
{
	START_XFORM_SKIP;
	future_t *future;
	prim_data_t data;
	memset(&data, 0, sizeof(prim_data_t));
	if (!IS_WORKER_THREAD)
	{
		return 0;
	}
	
	#ifdef DEBUG_FUTURES
	printf("scheme_fuel_counter = %d\n", scheme_fuel_counter);
	printf("scheme_jit_stack_boundary = %p\n", (void*)scheme_jit_stack_boundary);
	printf("scheme_current_runstack = %p\n", scheme_current_runstack);
	printf("scheme_current_runstack_start = %p\n", scheme_current_runstack_start);
	printf("stack address = %p\n", &future);
	#endif

	data.pvoid_pvoid_pvoid = f;
	data.a = a;
	data.b = b;
	data.sigtype = SIG_PVOID_PVOID_PVOID;

	future = get_my_future();
	future->rt_prim = (void*)f;
	future->prim_data = data;

	future_do_runtimecall((void*)f, NULL);
	*retval = future->prim_data.c;

	return 1;
	END_XFORM_SKIP;
}


int rtcall_int_pobj_obj_obj(
	prim_int_pobj_obj_obj_t f, 
	int argc, 
	Scheme_Object **argv, 
	Scheme_Object *p, 
	Scheme_Object **retval)
{
	START_XFORM_SKIP;
	future_t *future;
	prim_data_t data;
	memset(&data, 0, sizeof(prim_data_t));
	if (!IS_WORKER_THREAD)	
	{
		return 0;
	}

	#ifdef DEBUG_FUTURES
	printf("scheme_fuel_counter = %d\n", scheme_fuel_counter);
	printf("scheme_jit_stack_boundary = %p\n", (void*)scheme_jit_stack_boundary);
	printf("scheme_current_runstack = %p\n", scheme_current_runstack);
	printf("scheme_current_runstack_start = %p\n", scheme_current_runstack_start);
	printf("stack address = %p\n", &future);
	#endif

	data.int_pobj_obj_obj = f;
	data.argc = argc;
	data.argv = argv;
	data.p = p;
	data.sigtype = SIG_INT_POBJ_OBJ_OBJ;
	
	future = get_my_future();
	future->rt_prim = (void*)f;
	future->prim_data = data;

	future_do_runtimecall((void*)f, NULL);
	*retval = future->prim_data.retval;
        future->prim_data.retval = NULL;

	return 1;
	END_XFORM_SKIP;
}

//Does the work of actually invoking a primitive on behalf of a 
//future.  This function is always invoked on the main (runtime) 
//thread.
void *invoke_rtcall(future_t *future)
{
  void *pret = NULL, *dummy_ret;
	prim_data_t *pdata;

	//Temporarily use the worker thread's runstack 
	Scheme_Object *ret;
	Scheme_Object **old_rs = MZ_RUNSTACK, **old_rs_start = MZ_RUNSTACK_START;
	MZ_RUNSTACK = future->runstack;
	MZ_RUNSTACK_START = future->runstack_start;
	#ifdef DEBUG_FUTURES
	g_rtcall_count++;
	#endif

	pdata = &future->prim_data;
	switch (future->prim_data.sigtype)
  {
    case SIG_VOID_VOID:
		{
			prim_void_void_t func = pdata->void_void;
			func();

      pret = &dummy_ret;
      break;
		}
    case SIG_ALLOC_VOID_PVOID:
		{
			prim_alloc_void_pvoid_t func = pdata->alloc_void_pvoid;
			ret = func();
                        future->alloc_retval = ret;
                        future->alloc_retval_counter = scheme_did_gc_count;
                        break;
		}
    case SIG_OBJ_INT_POBJ_OBJ:
		{
			prim_obj_int_pobj_obj_t func = pdata->obj_int_pobj_obj;
			ret = func(
				pdata->p, 
				pdata->argc, 
				pdata->argv);

			pdata->retval = ret;

			/*pdata->retval = pdata->prim_obj_int_pobj_obj(
				pdata->p, 
				pdata->argc, 
				pdata->argv); */
                    
			break;
		}
    case SIG_INT_OBJARR_OBJ:
		{
			prim_int_pobj_obj_t func = pdata->int_pobj_obj;
			ret = func(
				pdata->argc, 
				pdata->argv);			

			pdata->retval = ret;

			/*pdata->retval = pdata->prim_int_pobj_obj(
				pdata->argc, 
				pdata->argv);
      */
			break;
		}
		case SIG_INT_POBJ_OBJ_OBJ:
		{
			prim_int_pobj_obj_obj_t func = pdata->int_pobj_obj_obj;
			ret = func(
				pdata->argc, 
				pdata->argv, 
				pdata->p);

			pdata->retval = ret;
			/*pdata->retval = pdata->prim_int_pobj_obj_obj(
				pdata->argc, 
				pdata->argv, 
				pdata->p);
			*/      
			break;
		}
		case SIG_PVOID_PVOID_PVOID: 
		{			
			prim_pvoid_pvoid_pvoid_t func = pdata->pvoid_pvoid_pvoid;
			pret = func(pdata->a, pdata->b);

			pdata->c = pret;
			/*pdata->c = pdata->prim_pvoid_pvoid_pvoid(
				pdata->a, 
				pdata->b);
			*/			
			break;
		}
 	}

		//Restore main thread's runstack 
		MZ_RUNSTACK = old_rs;
		MZ_RUNSTACK_START = old_rs_start;

    return ret;
}


/**********************************************************************/
/* Helpers for manipulating the futures queue                         */
/**********************************************************************/

future_t *enqueue_future(void)
{
    future_t *last, *ft;
		last = get_last_future();
		ft = MALLOC_ONE_TAGGED(future_t);     
    ft->so.type = scheme_future_type;
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


future_t *get_my_future(void)
{
	START_XFORM_SKIP;
    return get_future_by_threadid(pthread_self());
	END_XFORM_SKIP;
}


future_t *get_future_by_threadid(pthread_t threadid)
{
	START_XFORM_SKIP;
  future_t *ft = g_future_queue;
  if (NULL == ft)
	{
			printf("Couldn't find a future with thread ID %p!\n", (void*)threadid);
      return NULL;
	}
	
	while (1)
	{
		if (ft->threadid == threadid)
		{
			return ft;
		}

		ft = ft->next;
	}

	printf("Couldn't find a future with thread ID %p!\n", (void*)threadid);
	return NULL;
	END_XFORM_SKIP;
}


future_t *get_future(int futureid)
{
	START_XFORM_SKIP;
  future_t *ft = g_future_queue;
  if (NULL == ft)
	{
    return ft;
	}
    
  while (ft->id != futureid)
	{
    ft = ft->next;
	}

    //Sanity check
  if (ft->id != futureid)
	{
    return NULL;
	}

    return ft;
	END_XFORM_SKIP;
}


future_t *get_last_future(void)
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
