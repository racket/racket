#ifndef SCHEME_FUTURES_H
#define SCHEME_FUTURES_H

#ifndef UNIT_TEST
#include "schpriv.h"
typedef Scheme_Object*(*prim_t)(int, Scheme_Object**);
#else
#define Scheme_Object void
#define Scheme_Bucket void
#define Scheme_Env void
#define Scheme_Type int
#define scheme_void NULL
#define scheme_false 0x0
#define START_XFORM_SKIP
#define END_XFORM_SKIP 
#define MZ_MARK_STACK_TYPE long
#define Scheme_Native_Closure_Data void
typedef Scheme_Object*(*prim_t)(int, Scheme_Object**);
void scheme_add_global(char *name, int arity, Scheme_Env *env);
int scheme_make_prim_w_arity(prim_t func, char *name, int arg1, int arg2);
#endif

#include "pthread.h"
#include <stdio.h>

extern pthread_t g_rt_threadid;
extern Scheme_Object *start_primitive_tracking(int argc, Scheme_Object *argv[]);
extern Scheme_Object *end_primitive_tracking(int argc, Scheme_Object *argv[]);
extern Scheme_Object *future(int argc, Scheme_Object *argv[]);
extern Scheme_Object *touch(int argc, Scheme_Object *argv[]);
extern Scheme_Object *num_processors(int argc, Scheme_Object *argv[]);
extern void scheme_init_futures(Scheme_Env *env);
extern int future_do_runtimecall(void *func, int sigtype, void *args, void *retval);
extern void futures_init(void);

#ifdef DEBUG_FUTURES
//Debugging structure that contains 
//all relevant data at the time of a 
//runtime call.
typedef struct rtcall_context {
	Scheme_Object **mz_runstack_start; 
	Scheme_Object **mz_runstack; 
} rtcall_context_t;
#endif

typedef struct { 
	void (*prim)();
} sig_void_void_t;

typedef struct { 
	Scheme_Object* (*prim)(Scheme_Object*, int, Scheme_Object**);
	Scheme_Object *a;
	int b;
	Scheme_Object **c;
	Scheme_Object *retval;
} sig_obj_int_pobj_obj_t;

typedef struct {
	int sig_type;
	union {
		sig_void_void_t  void_void;
		sig_obj_int_pobj_obj_t  obj_int_pobj_obj;
	} calldata;
} rtcall_args_t;

#define PENDING 0
#define RUNNING 1
#define WAITING_FOR_PRIM 2
#define FINISHED 3

typedef struct future {
  Scheme_Object so;

	int id;
	pthread_t threadid;
	int status;
  int pending;
	int work_completed;
	pthread_cond_t can_continue_cv;

	Scheme_Object **runstack;
	Scheme_Object **runstack_start;
	Scheme_Object *orig_lambda;
	void *code;

	//Runtime call stuff
	void *rt_prim;
	int rt_prim_sigtype;
	void *rt_prim_args;
	void *rt_prim_retval;

	union {
		sig_void_void_t  void_void;
		sig_obj_int_pobj_obj_t  obj_int_pobj_obj;
	} calldata;

	Scheme_Object *retval;
	struct future *prev;
	struct future *next;

	#ifdef DEBUG_FUTURES 
	rtcall_context_t *context;
	#endif
} future_t;

#ifdef DEBUG_FUTURES
extern void debug_save_context(void);
extern void debug_kill_context(void);
#else
#define debug_save_context(...) 
#define debug_kill_context(...) 
#endif

#ifdef UNIT_TEST
//If unit testing, expose internal functions and vars to
//the test suite
extern future_t *g_future_queue;
extern int g_next_futureid;
extern pthread_t g_rt_threadid;

extern void *worker_thread_future_loop(void *arg);
extern void *invoke_rtcall(future_t *future);
extern future_t *enqueue_future(void);
extern future_t *get_pending_future(void);
extern future_t *get_my_future(void);
extern future_t *get_future_by_threadid(pthread_t threadid);
extern future_t *get_future(int futureid);
extern future_t *get_last_future(void);
extern void clear_futures(void);
#endif

//Primitive instrumentation stuff 
#ifdef INSTRUMENT_PRIMITIVES 
extern int g_print_prims;
extern void print_ms_and_us(void);
#define LOG_PRIM_START(p) \
	if (g_print_prims) \
	{ \
		printf("%p ", p); \
		print_ms_and_us(); \
		printf("\n"); \
	} 

#define LOG_PRIM_END(p) 
/*
#define LOG_PRIM_END(p) \
	if (g_print_prims) \
	{ \
		print_ms_and_us(); \
		printf("\n"); \
	}
*/

#define LOG_PRIM_W_NAME(name) \
	if (g_print_prims) \
	{ \
		printf("%s ", name); \
		print_ms_and_us(); \
		printf("\n"); \
	} 
#else
#define LOG_PRIM_START(p) 
#define LOG_PRIM_END(p) 
#define LOG_PRIM_W_NAME(name) 
#endif

//Signature flags for primitive invocations
//Here the convention is SIG_[arg1type]_[arg2type]..._[return type]
#define SIG_VOID_VOID 1 						//void -> void
#define SIG_OBJ_INT_POBJ_OBJ 2 			//Scheme_Object* -> int -> Scheme_Object** -> Scheme_Object*
#define SIG_INT_OBJARR_OBJ 3 				//int -> Scheme_Object*[] -> Scheme_Object*
#define SIG_LONG_OBJ_OBJ 4 					//long -> Scheme_Object* -> Scheme_Object*
#define SIG_OBJ_OBJ 5 							//Scheme_Object* -> Scheme_Object*
#define SIG_OBJ_OBJ_OBJ 6 					//Scheme_Object* -> Scheme_Object* -> Scheme_Object*
#define SIG_VOID_PVOID 7 						//void -> void*
#define SIG_SNCD_OBJ 8							//Scheme_Native_Closure_Data* -> Scheme_Object*
#define SIG_OBJ_VOID 9							//Scheme_Object* -> void 
#define SIG_LONG_OBJ 10							//long -> Scheme_Object* 
#define SIG_BUCKET_OBJ_INT_VOID 11	//Scheme_Bucket* -> Scheme_Object* -> int -> void 
#define SIG_INT_INT_POBJ_VOID 12		//int -> int -> Scheme_Object** -> void 
#define SIG_OBJ_OBJ_MZST 13					//Scheme_Object* -> Scheme_Object* -> MZ_MARK_STACK_TYPE 
#define SIG_BUCKET_VOID 14					//Scheme_Bucket* -> void 
#define SIG_POBJ_LONG_OBJ 15				//Scheme_Object** -> long -> Scheme_Object* 
#define SIG_INT_POBJ_INT_OBJ 16			//int -> Scheme_Object** -> int -> Scheme_Object* 
#define SIG_INT_POBJ_OBJ_OBJ 17			//int -> Scheme_Object** -> Scheme_Object* -> Scheme_Object*
#define SIG_OBJ_INT_POBJ_VOID 18		//Scheme_Object* -> int -> Scheme_Object** -> void 
#define SIG_ENV_ENV_VOID 19					//Scheme_Env* -> Scheme_Env* -> void

//Helper macros for argument marshaling
#ifdef FUTURES_ENABLED
extern void *g_funcargs[];
extern void *func_retval;

#define GET_INT(x) *((int*)(x))
#define GET_LONG(x) *((long*)(x))
#define GET_SCHEMEOBJ(x) (Scheme_Object*)(x)
#define GET_PSCHEMEOBJ(x) (Scheme_Object**)(x)
#define GET_SCHEMEENV(x) (Scheme_Env*)(x)

#define IS_WORKER_THREAD (g_rt_threadid != 0 && pthread_self() != g_rt_threadid)
#define ASSERT_CORRECT_THREAD if (g_rt_threadid != 0 && pthread_self() != g_rt_threadid) \
															{ \
																printf("%s invoked on wrong thread!\n", __FUNCTION__); \
																/*GDB_BREAK;*/ \
															}

extern int rtcall_void_void(void (*f)());
extern int rtcall_obj_int_pobj_obj(
	Scheme_Object* (*f)(Scheme_Object*, int, Scheme_Object**), 
	Scheme_Object *a, 
	int b, 
	Scheme_Object **c, 
	Scheme_Object **retval);


/*
#define RTCALL_VOID_VOID(f) \
	if (IS_WORKER_THREAD) \
	{ \
		debug_save_context(); \
		future_do_runtimecall((void*)f, SIG_VOID_VOID, NULL, NULL); \
		debug_kill_context(); \
		return; \
	}
*/

/*
#define RTCALL_OBJ_INT_POBJ_OBJ(f,a,b,c) \
	g_funcargs[0] = a; \
	g_funcargs[1] = &b; \
	g_funcargs[2] = c; \
	LOG_RTCALL_OBJ_INT_POBJ_OBJ(a, b, c); \
	if (IS_WORKER_THREAD) \
	{ \
		debug_save_context(); \
		future_do_runtimecall((void*)f, SIG_OBJ_INT_POBJ_OBJ, &g_funcargs, func_retval); \
		debug_kill_context(); \
        return (Scheme_Object*)func_retval; \
	}
*/

#define RTCALL_OBJ_INT_POBJ_VOID(f,a,b,c) \
	g_funcargs[0] = a; \
	g_funcargs[1] = &b; \
	g_funcargs[2] = c; \
	if (IS_WORKER_THREAD) \
	{ \
		future_do_runtimecall((void*)f, SIG_OBJ_INT_POBJ_VOID, &g_funcargs, NULL); \
	}

#define RTCALL_INT_OBJARR_OBJ(f,a,b) \
	g_funcargs[0] = &a; \
	g_funcargs[1] = b; \
	if (IS_WORKER_THREAD) \
	{ \
		future_do_runtimecall( \
			(void*)f, \
			SIG_INT_OBJARR_OBJ, \
			&g_funcargs, \
            func_retval); \
\
            return (Scheme_Object*)func_retval; \
	}

#define RTCALL_LONG_OBJ_OBJ(f,a,b) \
	g_funcargs[0] = &a; \
	g_funcargs[1] = b; \
	if (IS_WORKER_THREAD) \
	{ \
		future_do_runtimecall( \
			(void*)f, \
			SIG_LONG_OBJ_OBJ, \
			&g_funcargs, \
                        func_retval); \
\
                return (Scheme_Object*)func_retval; \
	} 

#define RTCALL_OBJ_OBJ(f,a) \
	if (IS_WORKER_THREAD) \
	{ \
		future_do_runtimecall( \
			(void*)f, \
			SIG_OBJ_OBJ, \
			a, \
                        func_retval); \
\
                return (Scheme_Object*)func_retval; \
	}

#define RTCALL_OBJ_OBJ_OBJ(f,a,b) \
	g_funcargs[0] = a; \
	g_funcargs[1] = b; \
	if (IS_WORKER_THREAD) \
	{ \
		future_do_runtimecall( \
			(void*)f, \
			SIG_OBJ_OBJ_OBJ, \
			&g_funcargs, \
                        func_retval); \
\
                return (Scheme_Object*)func_retval; \
	}

#define RTCALL_SNCD_OBJ(f,a) \
	if (IS_WORKER_THREAD) \
	{ \
		future_do_runtimecall( \
			(void*)f, \
			SIG_SNCD_OBJ, \
			(void*)a, \
                        func_retval); \
\
                return (Scheme_Object*)func_retval; \
	}

#define RTCALL_OBJ_VOID(f,a) \
	if (IS_WORKER_THREAD) \
	{ \
		future_do_runtimecall( \
			(void*)f, \
			SIG_OBJ_VOID, \
			(void*)a, \
                        NULL); \
\
		return; \
	}

#define RTCALL_LONG_OBJ(f,a) \
	if (IS_WORKER_THREAD) \
	{ \
		future_do_runtimecall( \
			(void*)f, \
			SIG_LONG_OBJ, \
			&a, \
                        func_retval); \
\
                return (Scheme_Object*)func_retval; \
	}

#define RTCALL_BUCKET_OBJ_INT_VOID(f,a,b,c) \
	g_funcargs[0] = a; \
	g_funcargs[1] = b; \
	g_funcargs[2] = &c; \
	if (IS_WORKER_THREAD) \
	{ \
		future_do_runtimecall( \
			(void*)f, \
			SIG_BUCKET_OBJ_INT_VOID, \
			&g_funcargs, \
                        NULL); \
		return; \
	}

#define RTCALL_INT_INT_POBJ_VOID(f,a,b,c) \
	g_funcargs[0] = &a; \
	g_funcargs[1] = &b; \
	g_funcargs[2] = c; \
	if (IS_WORKER_THREAD) \
	{ \
		future_do_runtimecall( \
			(void*)f, \
			SIG_INT_INT_POBJ_VOID, \
			&g_funcargs, \
                        NULL); \
		return; \
	}

#define RTCALL_OBJ_OBJ_MZST(f,a,b) \
	MZ_MARK_STACK_TYPE v; \
	MZ_MARK_STACK_TYPE *r; \
	g_funcargs[0] = a; \
	g_funcargs[1] = b; \
	if (IS_WORKER_THREAD) \
	{ \
		future_do_runtimecall( \
			(void*)f, \
			SIG_OBJ_OBJ_MZST, \
			&g_funcargs, \
                        func_retval); \
\
                r = (MZ_MARK_STACK_TYPE*)func_retval; \
		v = *r; \
		free(r); \
		return v; \
	}

#define RTCALL_BUCKET_VOID(f,a) \
	if (IS_WORKER_THREAD) \
	{ \
		future_do_runtimecall( \
			(void*)f, \
			SIG_BUCKET_VOID, \
			(void*)a, \
                        NULL); \
		return; \
	}

#define RTCALL_POBJ_LONG_OBJ(f,a,b) \
	g_funcargs[0] = a; \
	g_funcargs[1] = &b; \
	if (IS_WORKER_THREAD) \
	{ \
		future_do_runtimecall( \
			(void*)f, \
			SIG_POBJ_LONG_OBJ, \
			&g_funcargs, \
                        func_retval); \
\
                return (Scheme_Object*)func_retval; \
	}

#define RTCALL_INT_POBJ_INT_OBJ(f,a,b,c) \
	g_funcargs[0] = &a; \
	g_funcargs[1] = b; \
	g_funcargs[2] = &c; \
	if (IS_WORKER_THREAD) \
	{ \
		future_do_runtimecall( \
			(void*)f, \
			SIG_INT_POBJ_INT_OBJ, \
			&g_funcargs, \
                        func_retval); \
\
                return (Scheme_Object*)func_retval; \
	}

#define RTCALL_INT_POBJ_OBJ_OBJ(f,a,b,c) \
	g_funcargs[0] = &a; \
	g_funcargs[1] = b; \
	g_funcargs[2] = c; \
	if (IS_WORKER_THREAD) \
	{ \
		future_do_runtimecall( \
			(void*)f, \
			SIG_INT_POBJ_OBJ_OBJ, \
			&g_funcargs, \
                        func_retval); \
\
                return (Scheme_Object*)func_retval; \
	}

#define RTCALL_ENV_ENV_VOID(f,a,b) \
	g_funcargs[0] = a; \
	g_funcargs[1] = b; \
	if (IS_WORKER_THREAD) \
	{ \
		future_do_runtimecall( \
			(void*)f, \
			SIG_ENV_ENV_VOID, \
			&g_funcargs, \
			func_retval); \
	} 

#else 

#define RTCALL_VOID_VOID(f)
#define RTCALL_OBJ_INT_POBJ_OBJ(f,a,b,c) LOG_RTCALL_OBJ_INT_POBJ_OBJ(a,b,c)
#define RTCALL_OBJ_INT_POBJ_VOID(f,a,b,c) LOG_RTCALL_OBJ_INT_POBJ_VOID(a,b,c)
#define RTCALL_INT_OBJARR_OBJ(f,a,b) LOG_RTCALL_INT_OBJARR_OBJ(a,b)
#define RTCALL_LONG_OBJ_OBJ(f,a,b) LOG_RTCALL_LONG_OBJ_OBJ(a,b)
#define RTCALL_OBJ_OBJ(f,a) LOG_RTCALL_OBJ_OBJ(a)
#define RTCALL_OBJ_OBJ_OBJ(f,a,b) LOG_RTCALL_OBJ_OBJ_OBJ(a,b)
#define RTCALL_SNCD_OBJ(f,a) LOG_RTCALL_SNCD_OBJ(a)
#define RTCALL_OBJ_VOID(f,a) LOG_RTCALL_OBJ_VOID(a)
#define RTCALL_LONG_OBJ(f,a) LOG_RTCALL_LONG_OBJ(a)
#define RTCALL_BUCKET_OBJ_INT_VOID(f,a,b,c) LOG_RTCALL_BUCKET_OBJ_INT_VOID(a,b,c)
#define RTCALL_INT_INT_POBJ_VOID(f,a,b,c) LOG_RTCALL_INT_INT_POBJ_VOID(a,b,c)
#define RTCALL_OBJ_OBJ_MZST(f,a,b) LOG_RTCALL_OBJ_OBJ_MZST(a,b)
#define RTCALL_BUCKET_VOID(f,a) LOG_RTCALL_BUCKET_VOID(a)
#define RTCALL_POBJ_LONG_OBJ(f,a,b) LOG_RTCALL_POBJ_LONG_OBJ(a,b)
#define RTCALL_INT_POBJ_INT_OBJ(f,a,b,c) LOG_RTCALL_INT_POBJ_INT_OBJ(a,b,c)
#define RTCALL_INT_POBJ_OBJ_OBJ(f,a,b,c) LOG_RTCALL_INT_POBJ_OBJ_OBJ(a,b,c) 
#define RTCALL_ENV_ENV_VOID(f,a,b) LOG_RTCALL_ENV_ENV_VOID(a,b) 

#define IS_WORKER_THREAD 0
#define ASSERT_CORRECT_THREAD 

#endif 

#ifdef LOG_ARGS 
#define LOG(a...) do { fprintf(stderr, "%x:%s:%s:%d ", (unsigned) pthread_self(), __FILE__, __FUNCTION__, __LINE__); fprintf(stderr, a); fprintf(stderr, "\n"); fflush(stdout); } while(0)
#define LOG_THISCALL LOG(__FUNCTION__)

#define LOG_RTCALL_VOID_VOID(f) LOG("(function=%p)", f)
#define LOG_RTCALL_OBJ_INT_POBJ_OBJ(f,a,b,c) LOG("(function = %p, a=%p, b=%d, c=%p)", f, a, b, c)
#define LOG_RTCALL_OBJ_INT_POBJ_VOID(a,b,c) LOG("(%p, %d, %p)", a, b,c)
#define LOG_RTCALL_INT_OBJARR_OBJ(a,b) LOG("(%d, %p)", a, b)
#define LOG_RTCALL_LONG_OBJ_OBJ(a,b) LOG("(%ld, %p)", a, b)
#define LOG_RTCALL_OBJ_OBJ(a) LOG("(%p)", a)
#define LOG_RTCALL_OBJ_OBJ_OBJ(a,b) LOG("(%p, %p)", a, b)
#define LOG_RTCALL_SNCD_OBJ(a) LOG("(%p)", a)
#define LOG_RTCALL_OBJ_VOID(a) LOG("(%p)", a)
#define LOG_RTCALL_LONG_OBJ(a) LOG("(%ld)", a)
#define LOG_RTCALL_BUCKET_OBJ_INT_VOID(a,b,c) LOG("(%p, %p, %d)", a, b, c)
#define LOG_RTCALL_INT_INT_POBJ_VOID(a,b,c) LOG("(%d, %d, %p)", a, b, c)
#define LOG_RTCALL_OBJ_OBJ_MZST(a,b) LOG("(%p, %p)", a, b)
#define LOG_RTCALL_BUCKET_VOID(a) LOG("(%p)", a)
#define LOG_RTCALL_POBJ_LONG_OBJ(a,b) LOG("(%p, %ld)", a, b)
#define LOG_RTCALL_INT_POBJ_INT_OBJ(a,b,c) LOG("(%d, %p, %d)", a, b, c)
#define LOG_RTCALL_INT_POBJ_OBJ_OBJ(a,b,c) LOG("(%d, %p, %p)", a, b, c)
#define LOG_RTCALL_ENV_ENV_VOID(a,b) LOG("(%p, %p)", a, b) 
#else
#define LOG(a...)
#define LOG_THISCALL

#define LOG_RTCALL_VOID_VOID(f)
#define LOG_RTCALL_OBJ_INT_POBJ_OBJ(f,a,b,c)
#define LOG_RTCALL_OBJ_INT_POBJ_VOID(a,b,c)
#define LOG_RTCALL_INT_OBJARR_OBJ(a,b)
#define LOG_RTCALL_LONG_OBJ_OBJ(a,b)
#define LOG_RTCALL_OBJ_OBJ(a)
#define LOG_RTCALL_OBJ_OBJ_OBJ(a,b)
#define LOG_RTCALL_SNCD_OBJ(a)
#define LOG_RTCALL_OBJ_VOID(a)
#define LOG_RTCALL_LONG_OBJ(a)
#define LOG_RTCALL_BUCKET_OBJ_INT_VOID(a,b,c)
#define LOG_RTCALL_INT_INT_POBJ_VOID(a,b,c)
#define LOG_RTCALL_OBJ_OBJ_MZST(a,b)
#define LOG_RTCALL_BUCKET_VOID(a)
#define LOG_RTCALL_POBJ_LONG_OBJ(a,b)
#define LOG_RTCALL_INT_POBJ_INT_OBJ(a,b,c)
#define LOG_RTCALL_INT_POBJ_OBJ_OBJ(a,b,c)
#define LOG_RTCALL_ENV_ENV_VOID(a,b) 
#endif


#ifdef UNIT_TEST
//These forwarding decls only need to be here to make 
//primitives visible to test cases written in C
extern int future_begin_invoke(void *code);
extern Scheme_Object *touch(int argc, Scheme_Object **argv);
extern Scheme_Object *future_touch(int futureid);
#endif

#endif
