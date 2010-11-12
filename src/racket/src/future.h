#ifndef SCHEME_FUTURES_H
#define SCHEME_FUTURES_H

#ifdef MZ_USE_FUTURES

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

#include <stdio.h>

typedef void (*prim_void_void_3args_t)(Scheme_Object **);
typedef Scheme_Object* (*prim_obj_int_pobj_obj_t)(Scheme_Object*, int, Scheme_Object**);
typedef Scheme_Object* (*prim_int_pobj_obj_t)(int, Scheme_Object**);
typedef Scheme_Object* (*prim_int_pobj_obj_obj_t)(int, Scheme_Object**, Scheme_Object*);
typedef void* (*prim_pvoid_pvoid_pvoid_t)(void*, void*);

#define PENDING 0
#define RUNNING 1
#define WAITING_FOR_PRIM 2
#define FINISHED 3
#define PENDING_OVERSIZE 4
#define WAITING_FOR_REQUEUE 5

#define FSRC_OTHER 0
#define FSRC_RATOR 1
#define FSRC_PRIM 2
#define FSRC_MARKS 3

typedef struct future_t {
  Scheme_Object so;

  int id;
  int thread_short_id;
  int status;
  int work_completed;
  mzrt_sema *can_continue_sema;

  Scheme_Object *orig_lambda;
  void *code;

  /* Runtime call stuff */
  int rt_prim; /* flag to indicate waiting for a prim call */
  int want_lw; /* flag to indicate waiting for lw capture */
  int rt_prim_is_atomic;
  double time_of_request;
  const char *source_of_request;
  int source_type;

  unsigned long alloc_retval;
  int alloc_retval_counter;

  void *prim_func;
  int prim_protocol;
  Scheme_Object *arg_s0;
  const Scheme_Object *arg_t0;
  Scheme_Object **arg_S0;
  Scheme_Bucket *arg_b0;
  int arg_i0;
  long arg_l0;
  size_t arg_z0;
  Scheme_Native_Closure_Data *arg_n0;
  Scheme_Object *arg_s1;
  const Scheme_Object *arg_t1;
  Scheme_Object **arg_S1;
  int arg_i1;
  long arg_l1;
  Scheme_Object *arg_s2;
  Scheme_Object **arg_S2;
  int arg_i2;

  Scheme_Thread *arg_p;
  struct Scheme_Current_LWC *lwc;
  struct Scheme_Future_Thread_State *fts;

  struct Scheme_Lightweight_Continuation *suspended_lw;
  int maybe_suspended_lw; /* set to 1 with suspended_lw untl test in runtime thread */

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

  struct future_t *next_waiting_atomic;
  struct future_t *next_waiting_lwc;
} future_t;

/* Primitive instrumentation stuff */

/* Signature flags for primitive invocations */
#define SIG_VOID_VOID_3ARGS    1
#define SIG_ALLOC              2
#define SIG_ALLOC_MARK_SEGMENT 3

# include "jit_ts_protos.h"

extern Scheme_Object *scheme_ts_scheme_force_value_same_mark(Scheme_Object *v);

//Helper macros for argument marshaling
#ifdef MZ_USE_FUTURES

#define IS_WORKER_THREAD (g_rt_threadid != 0 && pthread_self() != g_rt_threadid)
#define ASSERT_CORRECT_THREAD if (g_rt_threadid != 0 && pthread_self() != g_rt_threadid) \
															{ \
																printf("%s invoked on wrong thread!\n", __FUNCTION__); \
																/*GDB_BREAK;*/ \
															}

extern void scheme_rtcall_void_void_3args(const char *who, int src_type, prim_void_void_3args_t f);
extern unsigned long scheme_rtcall_alloc(const char *who, int src_type);
extern void scheme_rtcall_new_mark_segment(Scheme_Thread *p);

#else 

#define IS_WORKER_THREAD 0
#define ASSERT_CORRECT_THREAD 

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

#endif /* MZ_USE_FUTURES */

/* always defined: */
Scheme_Object *scheme_future(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_current_future(int argc, Scheme_Object *argv[]);

#endif
