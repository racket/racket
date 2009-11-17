
#ifndef UNIT_TEST
# include "schpriv.h"
#endif

#ifdef INSTRUMENT_PRIMITIVES 
int g_print_prims = 0;
#endif

#ifdef FUTURES_ENABLED

#include "future.h"
#include <stdlib.h>
#include <string.h>
#ifdef UNIT_TEST
# include "./tests/unit_test.h"
#endif 

extern void *on_demand_jit_code;

#define THREAD_POOL_SIZE 2 
#define INITIAL_C_STACK_SIZE 500000
static pthread_t g_pool_threads[THREAD_POOL_SIZE];
static int g_num_avail_threads = 0;
static unsigned long g_cur_cpu_mask = 1;
static void *g_signal_handle = NULL;

future_t *g_future_queue = NULL;
int g_next_futureid = 0;
pthread_t g_rt_threadid = 0;

static pthread_mutex_t g_future_queue_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t g_future_pending_cv = PTHREAD_COND_INITIALIZER;

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

void debug_save_context(void)
{
	future_t *future;
	rtcall_context_t *context;
	future = get_my_future();
	context = (rtcall_context_t*)malloc(sizeof(rtcall_context_t));

	future->context = context;
	future->context->mz_runstack_start = MZ_RUNSTACK_START;
	future->context->mz_runstack = MZ_RUNSTACK;
}

void debug_assert_context(future_t *future)
{
	rtcall_context_t *context = future->context;
	if (MZ_RUNSTACK_START != future->context->mz_runstack_start)
	{
		printf("Future %d (thread %p) reports MZ_RUNSTACK_START was %p, but future runstack start should be %p.\n", 
			future->id, 
			future->threadid, 
			MZ_RUNSTACK_START, 
			future->runstack_start);
	}

	if (MZ_RUNSTACK != context->mz_runstack)
	{
		printf("Future %d (thread %p) reports MZ_RUNSTACK was %p, but future runstack should be %p.\n", 
			future->id, 
			future->threadid, 
			MZ_RUNSTACK, 
			future->runstack);
	}
}

void debug_kill_context(void)
{
	future_t *future;
	future = get_my_future();
	free(future->context);
	future->context = NULL;
}

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
/* Plumbing for MzScheme initialization                               */
/**********************************************************************/

//Invoked by the runtime on startup to make 
//primitives known
void scheme_init_futures(Scheme_Env *env)
{
	START_XFORM_SKIP;
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
	END_XFORM_SKIP;
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

  //Create the worker thread pool.  These threads will
  //'queue up' and wait for futures to become available   
	pthread_attr_init(&attr);
	pthread_attr_setstacksize(&attr, INITIAL_C_STACK_SIZE); 
  for (i = 0; i < THREAD_POOL_SIZE - 1; i++)
  {
      pthread_create(&threadid, &attr, worker_thread_future_loop, NULL);
      g_pool_threads[i] = threadid;
  }

	g_num_avail_threads = THREAD_POOL_SIZE;
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
	START_XFORM_SKIP;

		#ifdef DEBUG_FUTURES 
		LOG_THISCALL;
		#endif

    int init_runstack_size, main_runstack_size;
		int futureid;
    future_t *ft;
    Scheme_Native_Closure *nc;
    Scheme_Native_Closure_Data *ncd;
    Scheme_Object *lambda = argv[0];
    Scheme_Type type = SCHEME_TYPE(lambda);
    nc = (Scheme_Native_Closure*)lambda;
    ncd = nc->code;

    //Create the future descriptor and add to the queue as 'pending'    
    pthread_mutex_lock(&g_future_queue_mutex);
    ft = enqueue_future();
    pthread_cond_init(&ft->can_continue_cv, NULL);
		futureid = ++g_next_futureid;
    ft->id = futureid;
    ft->orig_lambda = lambda;
    ft->pending = 1;
   
    //Allocate a new scheme stack for the future 
		//init_runstack_size = MZ_RUNSTACK - MZ_RUNSTACK_START;
		init_runstack_size = 1000;

		#ifdef DEBUG_FUTURES
		printf("Allocating Scheme stack of %d bytes for future %d.\n", init_runstack_size, futureid); 
		#endif

    ft->runstack_start = scheme_alloc_runstack(init_runstack_size);
    ft->runstack = ft->runstack_start + init_runstack_size;

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

    return scheme_make_integer(futureid);
    END_XFORM_SKIP;
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
	START_XFORM_SKIP;
    Scheme_Object *retval = NULL;
    void *rtcall_retval = NULL;
		future_t *ft;
		int futureid;

		futureid = SCHEME_INT_VAL(argv[0]);

		#ifdef DEBUG_FUTURES 
		LOG("touch (future %d)", futureid);	
		dump_state();
		
		if (SCHEME_INTP(argv[0]))
		{
			printf("Future id passed to touch was %d, is a Scheme integer.\n", futureid);		
		}
		else
		{
			printf("Arg passed to touch was a %d.\n", SCHEME_TYPE(argv[0]));
		}
		fflush(stdout);
		#endif

    pthread_mutex_lock(&g_future_queue_mutex);
    ft = get_future(futureid);
    pthread_mutex_unlock(&g_future_queue_mutex);

    //Spin waiting for primitive calls or a return value from
    //the worker thread
    wait_for_rtcall_or_completion:
				scheme_block_until(future_ready, NULL, (Scheme_Object*)ft, 0);
        pthread_mutex_lock(&g_future_queue_mutex);
        if (ft->work_completed)
        {
            retval = ft->retval;

						printf("Successfully touched future %d\n", ft->id);
						fflush(stdout);

            //Destroy the future descriptor
            if (ft->prev == NULL)
            {
                //Set next to be the head of the queue
                g_future_queue = ft->next;
                if (g_future_queue != NULL)
                    g_future_queue->prev = NULL;
                
                free(ft);
            }
            else
            {
                ft->prev->next = ft->next;
                if (NULL != ft->next)
                    ft->next->prev = ft->prev;

                free(ft);
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
            ft->rt_prim_sigtype = 0;
            ft->rt_prim_args = NULL;

            //Signal the waiting worker thread that it
            //can continue running machine code
            pthread_cond_signal(&ft->can_continue_cv);
            pthread_mutex_unlock(&g_future_queue_mutex);

            goto wait_for_rtcall_or_completion;
        }
        else
        {
            pthread_mutex_unlock(&g_future_queue_mutex);
            goto wait_for_rtcall_or_completion;
        }

    return retval;
	END_XFORM_SKIP;
}


//Entry point for a worker thread allocated for
//executing futures.  This function will never terminate
//(until the process dies).
void *worker_thread_future_loop(void *arg)
{
	START_XFORM_SKIP;
	Scheme_Object *v;
	Scheme_Object* (*jitcode)(Scheme_Object*, int, Scheme_Object**);

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

		future_t *ft;
    wait_for_work:
        //LOG("Waiting for new future work...");
        pthread_mutex_lock(&g_future_queue_mutex);
				while (!(ft = get_pending_future()))
				{
					pthread_cond_wait(&g_future_pending_cv, &g_future_queue_mutex);
				}

        LOG("Got a signal that a future is pending...");
        
        //Work is available for this thread

				scheme_fuel_counter = 1;
				scheme_jit_stack_boundary = ((unsigned long)&v) - INITIAL_C_STACK_SIZE;

        ft->pending = 0;
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

        //Set the return val in the descriptor
        pthread_mutex_lock(&g_future_queue_mutex);
        ft->work_completed = 1;
        ft->retval = v;

				//Update the status 
				ft->status = FINISHED;
				scheme_signal_received_at(g_signal_handle);
        pthread_mutex_unlock(&g_future_queue_mutex);

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
    int sigtype,
    void *args,
    void *retval)
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
    future->rt_prim_sigtype = sigtype;
    future->rt_prim_args = args;

		//Update the future's status to waiting 
		future->status = WAITING_FOR_PRIM;

		scheme_signal_received_at(g_signal_handle);

    //Wait for the signal that the RT call is finished
    pthread_cond_wait(&future->can_continue_cv, &g_future_queue_mutex);

    //Clear rt call fields before releasing the lock on the descriptor
    future->rt_prim = NULL;
    future->rt_prim_sigtype = 0;
    future->rt_prim_args = NULL;

    retval = future->rt_prim_retval;
    pthread_mutex_unlock(&g_future_queue_mutex);
    return 1;
	END_XFORM_SKIP;
}


/**********************************************************************/
/* Functions for primitive invocation                   			  */
/**********************************************************************/
int rtcall_void_void(void (*f)())
{
	START_XFORM_SKIP;
	future_t *future;
	sig_void_void_t data;
	memset(&data, 0, sizeof(sig_void_void_t));
	if (!IS_WORKER_THREAD)
	{
		return 0;
	}

	LOG_RTCALL_VOID_VOID(f);

	#ifdef DEBUG_FUTURES
	//debug_save_context();
	#endif

	data.prim = f;

	future = get_my_future();
	future->rt_prim_sigtype = SIG_VOID_VOID;
	future->rt_prim = (void*)f;
	future->calldata.void_void = data;

	future_do_runtimecall((void*)f, SIG_VOID_VOID, NULL, NULL);

	#ifdef DEBUG_FUTURES
	//debug_kill_context();
	#endif

	return 1;
	END_XFORM_SKIP;
}


int rtcall_obj_int_pobj_obj(
	Scheme_Object* (*f)(Scheme_Object*, int, Scheme_Object**), 
	Scheme_Object *a, 
	int b, 
	Scheme_Object **c, 
	Scheme_Object **retval)
{
	START_XFORM_SKIP;
	future_t *future;
	sig_obj_int_pobj_obj_t data;
	memset(&data, 0, sizeof(sig_obj_int_pobj_obj_t));
	if (!IS_WORKER_THREAD)	
	{
		return 0;
	}

	LOG_RTCALL_OBJ_INT_POBJ_OBJ(f, a, b, c);

	#ifdef DEBUG_FUTURES
	//debug_save_context();
		
		#endif

	printf("scheme_fuel_counter = %d\n", scheme_fuel_counter);
	printf("scheme_jit_stack_boundary = %p\n", scheme_jit_stack_boundary);
	printf("scheme_current_runstack = %p\n", scheme_current_runstack);
	printf("scheme_current_runstack_start = %p\n", scheme_current_runstack_start);
	printf("stack address = %p\n", &future);

	data.prim = f;
	data.a = a;
	data.b = b;
	data.c = c;
	
	future = get_my_future();
	future->rt_prim_sigtype = SIG_OBJ_INT_POBJ_OBJ;
	future->rt_prim = (void*)f;
	future->calldata.obj_int_pobj_obj = data;

	future_do_runtimecall((void*)f, SIG_OBJ_INT_POBJ_OBJ, NULL, NULL);
	*retval = future->calldata.obj_int_pobj_obj.retval;

	#ifdef DEBUG_FUTURES
	//debug_kill_context();
	#endif

	return 1;
	END_XFORM_SKIP;
}


//Does the work of actually invoking a primitive on behalf of a 
//future.  This function is always invoked on the main (runtime) 
//thread.
void *invoke_rtcall(future_t *future)
{
  START_XFORM_SKIP;
  void *ret = NULL, *dummy_ret, *args = future->rt_prim_args;
  void **arr = NULL;
  MZ_MARK_STACK_TYPE lret = 0;

	//Temporarily use the worker thread's runstack 
	Scheme_Object **old_rs = MZ_RUNSTACK, **old_rs_start = MZ_RUNSTACK_START;
	MZ_RUNSTACK = future->runstack;
	MZ_RUNSTACK_START = future->runstack_start;
	#ifdef DEBUG_FUTURES
	//debug_assert_context(future);
	g_rtcall_count++;
	#endif

  switch (future->rt_prim_sigtype)
  {
    case SIG_VOID_VOID:
		{
			sig_void_void_t *data = &future->calldata.void_void;
			data->prim();

      //((void (*)(void))future->rt_prim)();
      ret = &dummy_ret;
      break;
		}
    case SIG_OBJ_INT_POBJ_OBJ:
		{
			sig_obj_int_pobj_obj_t *data = &future->calldata.obj_int_pobj_obj;
			data->retval = data->prim(
				data->a, 
				data->b, 
				data->c);	

        //arr = (void**)args;
        //ret = (void*)((Scheme_Object* (*)(Scheme_Object*, int, Scheme_Object**))future->rt_prim)(
        //    (Scheme_Object*)arr[0],
        //    GET_INT(arr[1]),
        //    (Scheme_Object**)arr[2]);
                    
			break;
		}
            case SIG_OBJ_INT_POBJ_VOID:
                    arr = (void**)args;
                    ((Scheme_Object* (*)(Scheme_Object*, int, Scheme_Object**))future->rt_prim)(
                        (Scheme_Object*)arr[0],
                        GET_INT(arr[1]),
                        (Scheme_Object**)arr[2]);

                    ret = (void*)0x1;
            case SIG_INT_OBJARR_OBJ:
                    arr = (void**)args;
                    ret = (void*)((Scheme_Object* (*)(int, Scheme_Object*[]))future->rt_prim)(
                        GET_INT(arr[0]),
                        (Scheme_Object**)arr[1]);
                    break;
            case SIG_LONG_OBJ_OBJ:
                    arr = (void**)args;
                    ret = (void*)((Scheme_Object* (*)(long, Scheme_Object*))future->rt_prim)(
                        GET_LONG(arr[0]),
                        (Scheme_Object*)arr[1]);
                    break;
            case SIG_OBJ_OBJ:
                    ret = (void*)((Scheme_Object* (*)(Scheme_Object*))future->rt_prim)((Scheme_Object*)args);
                    break;
            case SIG_OBJ_OBJ_OBJ:
                    arr = (void**)args;
                    ret = (void*)((Scheme_Object * (*)(Scheme_Object*, Scheme_Object*))future->rt_prim)(
                        (Scheme_Object*)arr[0],
                        (Scheme_Object*)arr[1]);
                    break;
            case SIG_VOID_PVOID:
                    ret = ((void* (*)(void))future->rt_prim)();
                    break;
            case SIG_SNCD_OBJ:
                    ret = (void*)((Scheme_Object* (*)(Scheme_Native_Closure_Data*))future->rt_prim)(
                        (Scheme_Native_Closure_Data*)args);
                    break;
            case SIG_OBJ_VOID:
                    ((void (*)(Scheme_Object*))future->rt_prim)((Scheme_Object*)args);
                    ret = &dummy_ret;
                    break;
            case SIG_LONG_OBJ:
                    ret = ((Scheme_Object* (*)(long))future->rt_prim)(GET_LONG(args));
                    break;
            case SIG_BUCKET_OBJ_INT_VOID:
                    arr = (void**)args;
                    ((void (*)(Scheme_Bucket*, Scheme_Object*, int))future->rt_prim)(
                        (Scheme_Bucket*)arr[0],
                        (Scheme_Object*)arr[1],
                        GET_INT(arr[2]));

                    ret = &dummy_ret;
                    break;
            case SIG_INT_INT_POBJ_VOID:
                    arr = (void**)args;
                    ((void (*)(int, int, Scheme_Object**))future->rt_prim)(
                        GET_INT(arr[0]),
                        GET_INT(arr[1]),
                        (Scheme_Object**)arr[2]);
                    break;
            case SIG_OBJ_OBJ_MZST:
                    arr = (void**)args;
                    lret = ((MZ_MARK_STACK_TYPE (*)(Scheme_Object*, Scheme_Object*))future->rt_prim)(
                        (Scheme_Object*)arr[0],
                        (Scheme_Object*)arr[1]);

                    ret = malloc(sizeof(MZ_MARK_STACK_TYPE));
                    *((MZ_MARK_STACK_TYPE*)ret) = lret;
                    break;
            case SIG_BUCKET_VOID:
                    ((void (*)(Scheme_Bucket*))future->rt_prim)((Scheme_Bucket*)args);
                    ret = &dummy_ret;
                    break;
            case SIG_POBJ_LONG_OBJ:
                    arr = (void**)args;
                    ret = ((Scheme_Object* (*)(Scheme_Object**, long))future->rt_prim)(
                        (Scheme_Object**)arr[0],
                        GET_LONG(arr[1]));
                    break;
            case SIG_INT_POBJ_INT_OBJ:
                    arr = (void**)args;
                    ret = ((Scheme_Object* (*)(int, Scheme_Object**, int))future->rt_prim)(
                        GET_INT(arr[0]),
                        (Scheme_Object**)arr[1],
                        GET_INT(arr[2]));
                    break;
            case SIG_INT_POBJ_OBJ_OBJ:
                    arr = (void**)args;
                    ret = ((Scheme_Object* (*)(int, Scheme_Object**, Scheme_Object*))future->rt_prim)(
                        GET_INT(arr[0]),
                        (Scheme_Object**)arr[1],
                        (Scheme_Object*)arr[2]);
                    break;
						case SIG_ENV_ENV_VOID: 
										arr = (void**)args;
										((void (*)(Scheme_Env*, Scheme_Env*))future->rt_prim)(
											GET_SCHEMEENV(arr[0]), 
											GET_SCHEMEENV(arr[1]));
										break;
    }

		//Restore main thread's runstack 
		MZ_RUNSTACK = old_rs;
		MZ_RUNSTACK_START = old_rs_start;

    return ret;
    END_XFORM_SKIP;
}


/**********************************************************************/
/* Helpers for manipulating the futures queue                         */
/**********************************************************************/

future_t *enqueue_future(void)
{
	START_XFORM_SKIP;
    future_t *last = get_last_future();
    future_t *ft = (future_t*)malloc(sizeof(future_t));
    memset(ft, 0, sizeof(future_t));
    if (NULL == last)
    {
        g_future_queue = ft;
        return ft;
    }

    ft->prev = last;
    last->next = ft;
    ft->next = NULL;
    
    return ft;
	END_XFORM_SKIP;
}


future_t *get_pending_future(void)
{
	START_XFORM_SKIP;
    future_t *f;
    for (f = g_future_queue; f != NULL; f = f->next)
    {
        if (f->pending)
            return f;
    }

    return NULL;
	END_XFORM_SKIP;
}


future_t *get_my_future(void)
{
    return get_future_by_threadid(pthread_self());
}


future_t *get_future_by_threadid(pthread_t threadid)
{
	START_XFORM_SKIP;
  future_t *ft = g_future_queue;
  if (NULL == ft)
	{
			printf("Couldn't find a future with thread ID %p!\n", threadid);
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

	printf("Couldn't find a future with thread ID %p!\n", threadid);
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
	START_XFORM_SKIP;
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
	END_XFORM_SKIP;
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
