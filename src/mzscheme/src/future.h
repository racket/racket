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
extern int future_do_runtimecall(void *func, void *retval);
extern void futures_init(void);

typedef void (*prim_void_void_t)(void);
typedef Scheme_Object* (*prim_obj_int_pobj_obj_t)(Scheme_Object*, int, Scheme_Object**);
typedef Scheme_Object* (*prim_int_pobj_obj_t)(int, Scheme_Object**);
typedef Scheme_Object* (*prim_int_pobj_obj_obj_t)(int, Scheme_Object**, Scheme_Object*);
typedef void* (*prim_pvoid_pvoid_pvoid_t)(void*, void*);

typedef struct { 
	unsigned int sigtype;

	prim_void_void_t void_void;
	prim_obj_int_pobj_obj_t obj_int_pobj_obj;
	prim_int_pobj_obj_t int_pobj_obj;
	prim_int_pobj_obj_obj_t int_pobj_obj_obj;
	prim_pvoid_pvoid_pvoid_t pvoid_pvoid_pvoid;

	//Scheme_Object* (*prim_obj_int_pobj_obj)(Scheme_Object* rator, int argc, Scheme_Object** argv);
	//Scheme_Object* (*prim_int_pobj_obj)(int argc, Scheme_Object** argv);
	//Scheme_Object* (*prim_int_pobj_obj_obj)(int argc, Scheme_Object** argv, Scheme_Object* p);
	//void (*prim_void_void)(void);
	//void* (*prim_pvoid_pvoid_pvoid)(void *a, void *b);

	Scheme_Object *p;
	int argc;
	Scheme_Object **argv;
	Scheme_Object *retval;	

	void *a;
	void *b;
	void *c;

} prim_data_t;

#define PENDING 0
#define RUNNING 1
#define WAITING_FOR_PRIM 2
#define FINISHED 3

typedef struct future {
  Scheme_Object so;

	int id;
	pthread_t threadid;
	int status;
	int work_completed;
	pthread_cond_t can_continue_cv;

	Scheme_Object **runstack;
	Scheme_Object **runstack_start;
	Scheme_Object *orig_lambda;
	void *code;

	//Runtime call stuff
	void *rt_prim;
	void *rt_prim_retval;

	prim_data_t prim_data;

	Scheme_Object *retval;
	struct future *prev;
	struct future *next;
} future_t;

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
#define SIG_INT_OBJARR_OBJ 3 				//int -> Scheme_Object*[] -> Scheme_Object
#define SIG_INT_POBJ_OBJ_OBJ 17			//int -> Scheme_Object** -> Scheme_Object* -> Scheme_Object* 
#define SIG_PVOID_PVOID_PVOID	18		//void* -> void* -> void*

//Helper macros for argument marshaling
#ifdef FUTURES_ENABLED

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

extern int rtcall_int_pobj_obj(
	Scheme_Object* (*f)(int, Scheme_Object**), 
	int argc, 
	Scheme_Object **argv, 
	Scheme_Object **retval);

#else 

#define IS_WORKER_THREAD 0
#define ASSERT_CORRECT_THREAD 

#endif 

#ifdef DEBUG_FUTURES 
#define LOG(a...) do { pthread_t self; self = pthread_self(); fprintf(stderr, "%x:%s:%s:%d ", (unsigned) self, __FILE__, __FUNCTION__, __LINE__); fprintf(stderr, a); fprintf(stderr, "\n"); fflush(stdout); } while(0)
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

void scheme_future_block_until_gc();
void scheme_future_continue_after_gc();


#ifdef UNIT_TEST
//These forwarding decls only need to be here to make 
//primitives visible to test cases written in C
extern int future_begin_invoke(void *code);
extern Scheme_Object *touch(int argc, Scheme_Object **argv);
extern Scheme_Object *future_touch(int futureid);
#endif

#endif
