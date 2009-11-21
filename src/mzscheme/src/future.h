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

typedef void (*prim_void_void_3args_t)(Scheme_Object **);
typedef unsigned long (*prim_alloc_void_pvoid_t)();
typedef Scheme_Object* (*prim_obj_int_pobj_obj_t)(Scheme_Object*, int, Scheme_Object**);
typedef Scheme_Object* (*prim_int_pobj_obj_t)(int, Scheme_Object**);
typedef Scheme_Object* (*prim_int_pobj_obj_obj_t)(int, Scheme_Object**, Scheme_Object*);
typedef void* (*prim_pvoid_pvoid_pvoid_t)(void*, void*);

#define PENDING 0
#define RUNNING 1
#define WAITING_FOR_PRIM 2
#define FINISHED 3

#define FSRC_OTHER 0
#define FSRC_RATOR 1
#define FSRC_PRIM 2

typedef struct future_t {
  Scheme_Object so;

  int id;
  pthread_t threadid;
  int thread_short_id;
  int status;
  int work_completed;
  pthread_cond_t *can_continue_cv;

  Scheme_Object *orig_lambda;
  void *code;

  //Runtime call stuff
  int rt_prim; /* flag to indicate waiting for a prim call */
  int rt_prim_is_atomic;
  double time_of_request;
  const char *source_of_request;
  int source_type;

  unsigned long alloc_retval;
  int alloc_retval_counter;

  void *prim_func;
  int prim_protocol;
  Scheme_Object *arg_s0;
  Scheme_Object **arg_S0;
  Scheme_Bucket *arg_b0;
  int arg_i0;
  long arg_l0;
  size_t arg_z0;
  Scheme_Native_Closure_Data *arg_n0;
  Scheme_Object *arg_s1;
  Scheme_Object **arg_S1;
  int arg_i1;
  long arg_l1;
  Scheme_Object *arg_s2;
  Scheme_Object **arg_S2;
  int arg_i2;

  Scheme_Object *retval_s;
  void *retval_p; /* use only with conservative GC */
  MZ_MARK_STACK_TYPE retval_m;
  int no_retval;

  Scheme_Object **multiple_array;
  int multiple_count;

  Scheme_Object *tail_rator;
  Scheme_Object **tail_rands;
  int num_tail_rands;

  Scheme_Object *retval;
  struct future_t *prev;
  struct future_t *next;

  int waiting_atomic;
  struct future_t *next_waiting_atomic;
} future_t;

//Primitive instrumentation stuff 

//Signature flags for primitive invocations
//Here the convention is SIG_[arg1type]_[arg2type]..._[return type]
#define SIG_VOID_VOID_3ARGS 1 						//void -> void, copy 3 args from runstack
#define SIG_ALLOC_VOID_PVOID 2 						//void -> void*

# include "jit_ts_protos.h"

//Helper macros for argument marshaling
#ifdef FUTURES_ENABLED

#define IS_WORKER_THREAD (g_rt_threadid != 0 && pthread_self() != g_rt_threadid)
#define ASSERT_CORRECT_THREAD if (g_rt_threadid != 0 && pthread_self() != g_rt_threadid) \
															{ \
																printf("%s invoked on wrong thread!\n", __FUNCTION__); \
																/*GDB_BREAK;*/ \
															}

extern void scheme_rtcall_void_void_3args(const char *who, int src_type, prim_void_void_3args_t f);
extern unsigned long scheme_rtcall_alloc_void_pvoid(const char *who, int src_type, prim_alloc_void_pvoid_t f);

#else 

#define IS_WORKER_THREAD 0
#define ASSERT_CORRECT_THREAD 

#endif 

#ifdef DEBUG_FUTURES 
#define LOG(a...) do { pthread_t self; self = pthread_self(); fprintf(stderr, "%x:%s:%s:%d ", (unsigned) self, __FILE__, __FUNCTION__, __LINE__); fprintf(stderr, a); fprintf(stderr, "\n"); fflush(stdout); } while(0)
#define LOG_THISCALL LOG(__FUNCTION__)

#define LOG_RTCALL_VOID_VOID_3ARGS(f) LOG("(function=%p)", f)
#define LOG_RTCALL_ALLOC_VOID_PVOID(f) LOG("(function=%p)", f)
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

#define LOG_RTCALL_VOID_VOID_3ARGS(f)
#define LOG_RTCALL_ALLOC_VOID_PVOID(f)
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

extern void *scheme_on_demand_jit_code;
extern void scheme_on_demand_generate_lambda(Scheme_Native_Closure *nc, int argc, Scheme_Object **argv);

void scheme_future_block_until_gc();
void scheme_future_continue_after_gc();
void scheme_check_future_work();
void scheme_future_gc_pause();

#ifdef UNIT_TEST
//These forwarding decls only need to be here to make 
//primitives visible to test cases written in C
extern int future_begin_invoke(void *code);
extern Scheme_Object *touch(int argc, Scheme_Object **argv);
extern Scheme_Object *future_touch(int futureid);
#endif

#endif
