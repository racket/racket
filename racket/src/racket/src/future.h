#ifndef SCHEME_FUTURES_H
#define SCHEME_FUTURES_H

#ifdef MZ_USE_FUTURES

typedef Scheme_Object* (*prim_obj_int_pobj_obj_t)(Scheme_Object*, int, Scheme_Object**);
typedef Scheme_Object* (*prim_int_pobj_obj_t)(int, Scheme_Object**);
typedef Scheme_Object* (*prim_int_pobj_obj_obj_t)(int, Scheme_Object**, Scheme_Object*);
typedef void* (*prim_pvoid_pvoid_pvoid_t)(void*, void*);

/* PENDING is ready to run: */
#define PENDING 0
/* RUNNING is running in some thread: */
#define RUNNING 1
/* WAITING_FOR_PRIM is waiting for the runtime thread to start runnin
   a primitive -- possibiliy atomic, possibly not, and possibly a LWC
   capture happens while waiting: */
#define WAITING_FOR_PRIM 2
/* FINISHED means the result (or failure) is ready: */
#define FINISHED 3
/* PENDING is ready to run, but won't work in a future thread: */
#define PENDING_OVERSIZE 4
/* WAITING_FOR_PRIM is the runtime thread working on a primitive: */
#define HANDLING_PRIM 5
/* WAITING_FOR_FSEMA is in the queue of an fsemaphore: */
#define WAITING_FOR_FSEMA 6
/* SUSPENDED is the owning custodian is gone, so the future will never finish: */
#define SUSPENDED 7
/* WAITING_FOR_OVERFLOW is waiting for an LCW capture to continue
   for a stack overflow: */
#define WAITING_FOR_OVERFLOW 8

/* FSRC_OTHER means: descriptive string is provided for logging,
   called function *DOES NOT NEED* to lookup continuation marks. */
#define FSRC_OTHER 0
/* FSRC_RATOR means: Racket function provided, so use it in logging,
   called function can lookup continuation marks. */
#define FSRC_RATOR 1
/* FSRC_PRIM means: Racket primitive provided, so use it in logging,
   called function can lookup continuation marks. */
#define FSRC_PRIM  2
/* FSRC_MARKS means: like FSRC_OTHER, but
   called function may need to lookup continuation marks. */
#define FSRC_MARKS 3

typedef struct Fevent {
  double timestamp;
  int what, fid, data;
} Fevent;

typedef struct Fevent_Buffer {
  Fevent *a;
  int pos, overflow;
  int i, count; /* used during flush */
} Fevent_Buffer;

typedef struct Scheme_Future_Thread_State {
  int is_runtime_thread;
  mz_proc_thread *t;
  int id;
  int worker_gc_counter;
  mzrt_sema *worker_can_continue_sema;
  intptr_t runstack_size;

  /* After a future thread starts, only the runtime thread
     modifies the values at these pointers. Future threads
     read them without any locks; assembly-level instructions,
     such as mfence, ensure that future threads eventually see 
     changes made by the runtime thread, and the runtime thread 
     waits as needed. */
  volatile int *fuel_pointer;
  volatile uintptr_t *stack_boundary_pointer;
  volatile int *need_gc_pointer;

  Scheme_Thread *thread;

  uintptr_t gen0_start;
  uintptr_t gen0_size;
  uintptr_t gen0_initial_offset;

  int local_capture_failed;

  int use_fevents1;
  Fevent_Buffer fevents1;
  Fevent_Buffer fevents2;
} Scheme_Future_Thread_State;

typedef struct future_t {
  Scheme_Object so;

  int id;
  int thread_short_id;

  int status;
  /* The status field is the main locking mechanism. It
     should only be read and written when holding a lock
     (and all associated fields for a status should be 
     set at the same time). */

  mzrt_sema *can_continue_sema;
  /* this semcpahore is non_NULL when a future thread is blocked
     while trying to run the future; th want_lw flag may be set in
     that case */

  Scheme_Object *orig_lambda;

  Scheme_Custodian *cust; /* an approximate custodian; don't use a future
                             thread if this custodian is shut down */

  /* Runtime call stuff */

  char want_lw; 
  /* flag to indicate waiting for lw capture; if this flag is set,
     then the future thread currently running the future must be
     blocked, and the runtime thread must not already be working on
     behalf of the future; since a future thread is blocked on this
     future, then can_continue_sema is normally set, but the runtime
     thread sets can_continue_sema to NULL while trying to capture the
     continuation --- in case anoter thread tries to let the original
     future thread continue because it was blocked on a touch for a
     future that completed; the `want_lw' flag should be changed only
     while holding a lock */

  char in_queue_waiting_for_lwc;
  /* flag to indicate whether the future is in the "waiting for lwc"
     queue; the future might be in the queue even if want_lw is set to
     0, and so this flag just prevents  */

  char in_touch_queue;   
  /* like `in_queue_waiting_for_lwc' but for being in a `touch'
     future */

  char in_future_specific_touch_queue; /* a back-door argument */

  char rt_prim_is_atomic;
  /* when a future thread is blocked on this future, it sets
     `rt_prim_is_atomic' if the blocking operation can run
     in any thread atomically (i.e., it's a "synchronizing" 
     operation insteda of a general blocking operation) */

  double time_of_request;
  const char *source_of_request;
  int source_type;

  uintptr_t alloc_retval;
  uintptr_t alloc_sz_retval;
  int alloc_retval_counter;

  void *prim_func;
  int prim_protocol;
  Scheme_Object *arg_s0;
  const Scheme_Object *arg_t0;
  Scheme_Object **arg_S0;
  Scheme_Bucket *arg_b0;
  int arg_i0;
  intptr_t arg_l0;
  size_t arg_z0;
  Scheme_Native_Closure_Data *arg_n0;
  Scheme_Object *arg_s1;
  const Scheme_Object *arg_t1;
  Scheme_Object **arg_S1;
  int arg_i1;
  intptr_t arg_l1;
  Scheme_Object *arg_s2;
  Scheme_Object **arg_S2;
  int arg_i2;
  void *arg_p2;

  const char *arg_str0;
  const char *arg_str1;
  int arg_i3;
  Scheme_Object **arg_S4;

  Scheme_Thread *arg_p;
  /* when a future thread is blocked while running this future,
     `arg_p' s set along with the blocking-operation arguments to
     indicate the future thread's (fake) Racket thread, which has the
     runstack, etc. */
  struct Scheme_Current_LWC *lwc;
  /* when a future thread is blocked while running this future,
     if `want_lw' is set, then `lwc' points to information for
     capturing a lightweight continuation */
  struct Scheme_Future_Thread_State *fts;
  /* when a future thread is blocked while running this future,
     `fts' is set to identify the future thread */

  struct Scheme_Lightweight_Continuation *suspended_lw;
  /* holds a lightweight continuation captured for the operation,
     if any */
  int maybe_suspended_lw;
  /* set to 1 with suspended_lw untl test in runtime thread; this
     extra flag avoids spinning if the suspended continuation
     cannot be resumed in the main thread for some reason */

  void **suspended_lw_stack; /* for overflow handling */

  Scheme_Object *retval_s;
  void *retval_p; /* use only with conservative GC */
  MZ_MARK_STACK_TYPE retval_m;
  int retval_i;
  signed char no_retval;
  char retval_is_rs_plus_two; /* => special result handling for on-demand JIT */

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
  struct future_t *next_waiting_touch;

  struct future_t *prev_in_fsema_queue;
  struct future_t *next_in_fsema_queue;

  int in_tracing_mode;

  Scheme_Object *touching; /* a list of weak pointers to futures touching this one */
} future_t;

typedef struct fsemaphore_t {
  Scheme_Object so;

  int ready;
  mzrt_mutex *mut; 
  future_t *queue_front;
  future_t *queue_end;
} fsemaphore_t;

/* Primitive instrumentation stuff */

/* Signature flags for primitive invocations */
#define SIG_ON_DEMAND          1
#define SIG_ALLOC              2
#define SIG_ALLOC_MARK_SEGMENT 3
#define SIG_ALLOC_VALUES       4
#define SIG_ALLOC_STRUCT       5
#define SIG_MAKE_FSEMAPHORE    6
#define SIG_FUTURE             7
#define SIG_WRONG_TYPE_EXN     8
#define SIG_TAIL_APPLY         9
#define SIG_APPLY_AFRESH       10

# include "jit_ts_protos.h"

extern Scheme_Object *scheme_force_value_same_mark_as_lightweight_continuation(Scheme_Object *v);

extern Scheme_Object **scheme_rtcall_on_demand(Scheme_Object **argv);
extern uintptr_t scheme_rtcall_alloc(void);
extern void scheme_rtcall_new_mark_segment(Scheme_Thread *p);
extern void scheme_rtcall_allocate_values(int count, Scheme_Thread *t);
extern Scheme_Structure *scheme_rtcall_allocate_structure(int argc, Scheme_Struct_Type *stype);
extern Scheme_Object *scheme_rtcall_make_fsemaphore(Scheme_Object *ready);
extern Scheme_Object *scheme_rtcall_make_future(Scheme_Object *proc);
extern Scheme_Object *scheme_rtcall_tail_apply(Scheme_Object *rator, int argc, Scheme_Object **argv);
extern Scheme_Object *scheme_rtcall_apply_with_new_stack(Scheme_Object *rator, int argc, Scheme_Object **argv, int multi);

int scheme_can_apply_native_in_future(Scheme_Object *proc);

void scheme_future_block_until_gc();
void scheme_future_continue_after_gc();
void scheme_check_future_work();
void scheme_future_gc_pause();
void scheme_future_check_custodians();
int scheme_future_is_runtime_thread();

#endif /* MZ_USE_FUTURES */

/* always defined: */
Scheme_Object *scheme_future(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_current_future(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_fsemaphore_p(int argc, Scheme_Object *argv[]);

Scheme_Object *scheme_fsemaphore_count(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_make_fsemaphore(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_make_fsemaphore_inl(Scheme_Object *ready);
Scheme_Object *scheme_fsemaphore_wait(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_fsemaphore_post(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_fsemaphore_try_wait(int argc, Scheme_Object *argv[]);

Scheme_Object *scheme_box_cas(int argc, Scheme_Object *argv[]);

#endif
