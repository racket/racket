/*
  Racket
  Copyright (c) 2004-2014 PLT Design Inc.
  Copyright (c) 1995-2001 Matthew Flatt

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

/* This file implements Racket threads.

   Usually, Racket threads are implemented by copying the stack.
   The scheme_thread_block() function is called occasionally by the
   evaluator so that the current thread can be swapped out.
   do_swap_thread() performs the actual swap. Threads can also be
   implemented by the OS; the bottom part of this file contains
   OS-specific thread code.

   Much of the work in thread management is knowning when to go to
   sleep, to be nice to the OS outside of Racket. The rest of the
   work is implementing custodians (called "custodians" in the code),
   parameters, and wills. */

/* Some copilers don't like re-def of GC_malloc in schemef.h: */
#ifndef MZ_PRECISE_GC
# define SCHEME_NO_GC_PROTO
#endif

#include "schpriv.h"
#include "schmach.h"
#include "schgc.h"
#ifdef MZ_USE_FUTURES
# include "future.h"
#endif
#ifndef PALMOS_STUFF
# include <time.h>
#endif
#ifdef FILES_HAVE_FDS
# include <sys/types.h>
# include <sys/time.h>
# ifdef SELECT_INCLUDE
#  include <sys/select.h>
# endif
# ifdef USE_BEOS_SOCKET_INCLUDE
#  include <be/net/socket.h>
# endif
# ifdef HAVE_POLL_SYSCALL
#  include <poll.h>
# endif
# ifdef HAVE_EPOLL_SYSCALL
#  include <sys/epoll.h>
# endif
# ifdef HAVE_KQUEUE_SYSCALL
#  include <sys/types.h>
#  include <sys/event.h>
#  include <sys/time.h>
# endif
# include <errno.h>
#endif
#ifdef USE_WINSOCK_TCP
# ifdef USE_TCP
#  include <winsock.h>
# endif
#endif
#ifdef USE_BEOS_PORT_THREADS
# include <be/net/socket.h>
#endif
#ifdef USE_STACKAVAIL
# include <malloc.h>
#endif
#ifdef UNISTD_INCLUDE
# include <unistd.h>
#endif

#ifndef SIGNMZTHREAD
# define SIGMZTHREAD SIGUSR2
#endif

#if defined(WINDOWS_PROCESSES) || defined(WINDOWS_FILE_HANDLES)
# include <windows.h>
THREAD_LOCAL_DECL(extern void *scheme_break_semaphore;)
#endif

#if defined(FILES_HAVE_FDS) \
     || defined(USE_BEOS_PORT_THREADS) \
     || (defined(USE_WINSOCK_TCP) && defined(USE_TCP)) \
     || (defined(WINDOWS_PROCESSES) || defined(WINDOWS_FILE_HANDLES))
# define USING_FDS
# if (!defined(USE_WINSOCK_TCP) || !defined(USE_TCP)) && !defined(FILES_HAVE_FDS)
#  include <sys/types.h>
# endif
#endif

#include "schfd.h"

#define DEFAULT_INIT_STACK_SIZE 1000
#define MAX_INIT_STACK_SIZE 100000

#ifdef SGC_STD_DEBUGGING
# define SENORA_GC_NO_FREE
#endif

/* If a finalization callback invokes Racket code,
   we can end up with a thread swap in the middle of a thread
   swap (where the outer swap was interrupted by GC). The
   following is a debugging flag to help detect and fix
   such problems. */
#define WATCH_FOR_NESTED_SWAPS 0

#if WATCH_FOR_NESTED_SWAPS
static int swapping = 0;
#endif

extern void scheme_gmp_tls_init(intptr_t *s);
extern void *scheme_gmp_tls_load(intptr_t *s);
extern void scheme_gmp_tls_unload(intptr_t *s, void *p);
extern void scheme_gmp_tls_snapshot(intptr_t *s, intptr_t *save);
extern void scheme_gmp_tls_restore_snapshot(intptr_t *s, void *data, intptr_t *save, int do_free);

static void check_ready_break();

THREAD_LOCAL_DECL(extern int scheme_num_read_syntax_objects);
THREAD_LOCAL_DECL(extern intptr_t scheme_hash_request_count);
THREAD_LOCAL_DECL(extern intptr_t scheme_hash_iteration_count);
THREAD_LOCAL_DECL(extern intptr_t scheme_code_page_total);
#ifdef MZ_USE_JIT
extern int scheme_jit_malloced;
#else
# define scheme_jit_malloced 0
#endif

/*========================================================================*/
/*                    local variables and prototypes                      */
/*========================================================================*/

#define INIT_TB_SIZE  20

#ifndef MZ_THREAD_QUANTUM_USEC
# define MZ_THREAD_QUANTUM_USEC 10000
#endif

THREAD_LOCAL_DECL(static int buffer_init_size);

THREAD_LOCAL_DECL(Scheme_Thread *scheme_current_thread = NULL);
THREAD_LOCAL_DECL(Scheme_Thread *scheme_main_thread = NULL);
THREAD_LOCAL_DECL(Scheme_Thread *scheme_first_thread = NULL);
THREAD_LOCAL_DECL(static Scheme_Thread *gc_prep_thread_chain = NULL);

XFORM_NONGCING Scheme_Thread *scheme_get_current_thread() { return scheme_current_thread; }
XFORM_NONGCING intptr_t scheme_get_multiple_count() { return scheme_current_thread->ku.multiple.count; }
XFORM_NONGCING Scheme_Object **scheme_get_multiple_array() { return scheme_current_thread->ku.multiple.array; }
XFORM_NONGCING void scheme_set_current_thread_ran_some() { scheme_current_thread->ran_some = 1; }

THREAD_LOCAL_DECL(Scheme_Thread_Set *scheme_thread_set_top);

THREAD_LOCAL_DECL(static int num_running_threads); /* not counting original */

#ifdef LINK_EXTENSIONS_BY_TABLE
Scheme_Thread **scheme_current_thread_ptr;
volatile int *scheme_fuel_counter_ptr;
#endif
THREAD_LOCAL_DECL(static int swap_no_setjmp = 0);

THREAD_LOCAL_DECL(static int thread_swap_count);
THREAD_LOCAL_DECL(int scheme_did_gc_count);
THREAD_LOCAL_DECL(static intptr_t process_time_at_swap);

SHARED_OK static int init_load_on_demand = 1;

#ifdef RUNSTACK_IS_GLOBAL
THREAD_LOCAL_DECL(Scheme_Object **scheme_current_runstack_start);
THREAD_LOCAL_DECL(Scheme_Object **scheme_current_runstack);
THREAD_LOCAL_DECL(MZ_MARK_STACK_TYPE scheme_current_cont_mark_stack);
THREAD_LOCAL_DECL(MZ_MARK_POS_TYPE scheme_current_cont_mark_pos);
#endif

THREAD_LOCAL_DECL(int scheme_semaphore_fd_kqueue);

THREAD_LOCAL_DECL(static Scheme_Custodian *main_custodian);
THREAD_LOCAL_DECL(static Scheme_Custodian *last_custodian);
THREAD_LOCAL_DECL(static Scheme_Hash_Table *limited_custodians = NULL);
READ_ONLY static Scheme_Object *initial_inspector;

THREAD_LOCAL_DECL(static Scheme_Plumber *initial_plumber);

THREAD_LOCAL_DECL(Scheme_Config *initial_config);

#ifndef MZ_PRECISE_GC
static int cust_box_count, cust_box_alloc;
static Scheme_Custodian_Box **cust_boxes;
# ifndef USE_SENORA_GC
extern int GC_is_marked(void *);
# endif
#endif

READ_ONLY Scheme_At_Exit_Proc replacement_at_exit;

ROSYM Scheme_Object *scheme_parameterization_key;
ROSYM Scheme_Object *scheme_exn_handler_key;
ROSYM Scheme_Object *scheme_break_enabled_key;

THREAD_LOCAL_DECL(static Scheme_Object *configuration_callback_cache[2]);

THREAD_LOCAL_DECL(intptr_t scheme_total_gc_time);
THREAD_LOCAL_DECL(static intptr_t start_this_gc_time);
THREAD_LOCAL_DECL(static intptr_t end_this_gc_time);
THREAD_LOCAL_DECL(static double start_this_gc_real_time);
THREAD_LOCAL_DECL(static double end_this_gc_real_time);
static void get_ready_for_GC(void);
static void done_with_GC(void);
#ifdef MZ_PRECISE_GC
static void inform_GC(int master_gc, int major_gc, intptr_t pre_used, intptr_t post_used,
                      intptr_t pre_admin, intptr_t post_admin, intptr_t post_child_places_used);
#endif

THREAD_LOCAL_DECL(static volatile short delayed_break_ready);
THREAD_LOCAL_DECL(static Scheme_Thread *main_break_target_thread);

THREAD_LOCAL_DECL(Scheme_Sleep_Proc scheme_place_sleep);
HOOK_SHARED_OK void (*scheme_sleep)(float seconds, void *fds);
HOOK_SHARED_OK void (*scheme_notify_multithread)(int on);
HOOK_SHARED_OK void (*scheme_wakeup_on_input)(void *fds);
HOOK_SHARED_OK int (*scheme_check_for_break)(void);
THREAD_LOCAL_DECL(static Scheme_On_Atomic_Timeout_Proc on_atomic_timeout);
THREAD_LOCAL_DECL(static int atomic_timeout_auto_suspend);
THREAD_LOCAL_DECL(static int atomic_timeout_atomic_level);

THREAD_LOCAL_DECL(struct Scheme_GC_Pre_Post_Callback_Desc *gc_prepost_callback_descs);

ROSYM static Scheme_Object *read_symbol, *write_symbol, *execute_symbol, *delete_symbol, *exists_symbol;
ROSYM static Scheme_Object *client_symbol, *server_symbol;

THREAD_LOCAL_DECL(static int do_atomic = 0);
THREAD_LOCAL_DECL(static int missed_context_switch = 0);
THREAD_LOCAL_DECL(static int have_activity = 0);
THREAD_LOCAL_DECL(int scheme_active_but_sleeping = 0);
THREAD_LOCAL_DECL(static int thread_ended_with_activity);
THREAD_LOCAL_DECL(int scheme_no_stack_overflow);
THREAD_LOCAL_DECL(int all_breaks_disabled = 0);
THREAD_LOCAL_DECL(static int needs_sleep_cancelled);
THREAD_LOCAL_DECL(static double needs_sleep_time_end); /* back-door result */
THREAD_LOCAL_DECL(static int tls_pos = 0);
/* On swap, put target in a static variable, instead of on the stack,
   so that the swapped-out thread is less likely to have a pointer
   to the target thread. */
THREAD_LOCAL_DECL(static Scheme_Thread *swap_target);
THREAD_LOCAL_DECL(static Scheme_Object *scheduled_kills);
THREAD_LOCAL_DECL(static Scheme_Object *the_nested_exn_handler);
THREAD_LOCAL_DECL(static Scheme_Object *cust_closers);
THREAD_LOCAL_DECL(static Scheme_Object *thread_swap_callbacks);
THREAD_LOCAL_DECL(static Scheme_Object *thread_swap_out_callbacks);
THREAD_LOCAL_DECL(static Scheme_Object *recycle_cell);
THREAD_LOCAL_DECL(static Scheme_Object *maybe_recycle_cell);
THREAD_LOCAL_DECL(static int recycle_cc_count);
THREAD_LOCAL_DECL(static Scheme_Struct_Type *gc_info_prefab);

THREAD_LOCAL_DECL(struct Scheme_Hash_Table *place_local_misc_table);

#if defined(MZ_PRECISE_GC) && defined(MZ_USE_PLACES)
extern intptr_t GC_is_place();
#endif

typedef struct Thread_Cell {
  Scheme_Object so;
  char inherited, assigned;
  Scheme_Object *def_val;
} Thread_Cell;

#ifdef MZ_PRECISE_GC
/* This is a trick to get the types right. Note that 
   the layout of the weak box is defined by the
   GC spec. */
typedef struct {
  short type;
  short hash_key;
  Scheme_Custodian *val;
} Scheme_Custodian_Weak_Box;

# define MALLOC_MREF() (Scheme_Custodian_Reference *)scheme_make_late_weak_box(NULL)
# define CUSTODIAN_FAM(x) ((Scheme_Custodian_Weak_Box *)x)->val
# define xCUSTODIAN_FAM(x) SCHEME_BOX_VAL(x)
# define SET_MREF_POSITION(mref, i) (((Scheme_Custodian_Weak_Box *)mref)->hash_key = (i & 0xFFFF))
# define EXTRACT_MREF_START_POSITION(mref, c) (((Scheme_Custodian_Weak_Box *)mref)->hash_key | ((c) & ~0xFFFF))
# define EXTRACT_MREF_POSITION_DELTA(mref, c) 0x10000
#else
# define MALLOC_MREF() MALLOC_ONE_WEAK(Scheme_Custodian_Reference)
# define CUSTODIAN_FAM(x) (*(x))
# define xCUSTODIAN_FAM(x) (*(x))
# define SET_MREF_POSITION(mref, i) /* empty */
# define EXTRACT_MREF_START_POSITION(mref, c) ((c)-1)
# define EXTRACT_MREF_POSITION_DELTA(mref, c) 1
#endif

typedef struct Proc_Global_Rec {
  const char *key;
  void *val;
  struct Proc_Global_Rec *next;
} Proc_Global_Rec;

SHARED_OK static Proc_Global_Rec *process_globals;
#if defined(MZ_USE_MZRT)
static mzrt_mutex *process_global_lock;
#endif

typedef struct {
  Scheme_Object so;
  intptr_t size;
} Scheme_Phantom_Bytes;

struct Scheme_Plumber {
  Scheme_Object so;
  Scheme_Hash_Table *handles;
  Scheme_Bucket_Table *weak_handles;
};

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

static Scheme_Object *custodian_require_mem(int argc, Scheme_Object *args[]);
static Scheme_Object *custodian_limit_mem(int argc, Scheme_Object *args[]);
static Scheme_Object *custodian_can_mem(int argc, Scheme_Object *args[]);
static Scheme_Object *new_tracking_fun(int argc, Scheme_Object *args[]);
static Scheme_Object *union_tracking_val(int argc, Scheme_Object *args[]);

static Scheme_Object *collect_garbage(int argc, Scheme_Object *args[]);
static Scheme_Object *current_memory_use(int argc, Scheme_Object *args[]);

static Scheme_Object *sch_thread(int argc, Scheme_Object *args[]);
static Scheme_Object *sch_thread_nokill(int argc, Scheme_Object *args[]);
static Scheme_Object *sch_sleep(int argc, Scheme_Object *args[]);
static Scheme_Object *thread_p(int argc, Scheme_Object *args[]);
static Scheme_Object *thread_running_p(int argc, Scheme_Object *args[]);
static Scheme_Object *thread_dead_p(int argc, Scheme_Object *args[]);
static Scheme_Object *thread_wait(int argc, Scheme_Object *args[]);
static Scheme_Object *sch_current(int argc, Scheme_Object *args[]);
static Scheme_Object *kill_thread(int argc, Scheme_Object *args[]);
static Scheme_Object *break_thread(int argc, Scheme_Object *args[]);
static Scheme_Object *thread_suspend(int argc, Scheme_Object *args[]);
static Scheme_Object *thread_resume(int argc, Scheme_Object *args[]);
static Scheme_Object *make_thread_suspend(int argc, Scheme_Object *args[]);
static Scheme_Object *make_thread_resume(int argc, Scheme_Object *args[]);
static Scheme_Object *make_thread_dead(int argc, Scheme_Object *args[]);
static void register_thread_sync();

static Scheme_Object *sch_sync(int argc, Scheme_Object *args[]);
static Scheme_Object *sch_sync_timeout(int argc, Scheme_Object *args[]);
static Scheme_Object *sch_sync_enable_break(int argc, Scheme_Object *args[]);
static Scheme_Object *sch_sync_timeout_enable_break(int argc, Scheme_Object *args[]);
static Scheme_Object *evt_p(int argc, Scheme_Object *args[]);
static Scheme_Object *evts_to_evt(int argc, Scheme_Object *args[]);

static Scheme_Object *make_custodian(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_custodian_from_main(int argc, Scheme_Object *argv[]);
static Scheme_Object *custodian_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *custodian_close_all(int argc, Scheme_Object *argv[]);
static Scheme_Object *custodian_to_list(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_custodian(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_custodian_box(int argc, Scheme_Object *argv[]);
static Scheme_Object *custodian_box_value(int argc, Scheme_Object *argv[]);
static Scheme_Object *custodian_box_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *call_as_nested_thread(int argc, Scheme_Object *argv[]);

static Scheme_Object *make_plumber(int argc, Scheme_Object *argv[]);
static Scheme_Object *plumber_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *plumber_flush_all(int argc, Scheme_Object *argv[]);
static Scheme_Object *plumber_add_flush(int argc, Scheme_Object *argv[]);
static Scheme_Object *plumber_remove_flush(int argc, Scheme_Object *argv[]);
static Scheme_Object *plumber_flush_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_plumber(int argc, Scheme_Object *argv[]);

static Scheme_Object *current_namespace(int argc, Scheme_Object *args[]);
static Scheme_Object *namespace_p(int argc, Scheme_Object *args[]);

static Scheme_Object *parameter_p(int argc, Scheme_Object *args[]);
static Scheme_Object *parameter_procedure_eq(int argc, Scheme_Object *args[]);
static Scheme_Object *make_parameter(int argc, Scheme_Object *args[]);
static Scheme_Object *make_derived_parameter(int argc, Scheme_Object *args[]);
static Scheme_Object *extend_parameterization(int argc, Scheme_Object *args[]);
static Scheme_Object *parameterization_p(int argc, Scheme_Object *args[]);
static Scheme_Object *reparameterize(int argc, Scheme_Object **argv);

static Scheme_Object *make_thread_cell(int argc, Scheme_Object *args[]);
static Scheme_Object *thread_cell_p(int argc, Scheme_Object *args[]);
static Scheme_Object *thread_cell_get(int argc, Scheme_Object *args[]);
static Scheme_Object *thread_cell_set(int argc, Scheme_Object *args[]);
static Scheme_Object *thread_cell_values(int argc, Scheme_Object *args[]);
static Scheme_Object *is_thread_cell_values(int argc, Scheme_Object *args[]);

static Scheme_Object *make_security_guard(int argc, Scheme_Object *argv[]);
static Scheme_Object *security_guard_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_security_guard(int argc, Scheme_Object *argv[]);

static Scheme_Object *cache_configuration(int argc, Scheme_Object **argv);

static Scheme_Object *make_thread_set(int argc, Scheme_Object *argv[]);
static Scheme_Object *thread_set_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_thread_set(int argc, Scheme_Object *argv[]);

static Scheme_Object *current_thread_initial_stack_size(int argc, Scheme_Object *argv[]);

static Scheme_Object *phantom_bytes_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_phantom_bytes(int argc, Scheme_Object *argv[]);
static Scheme_Object *set_phantom_bytes(int argc, Scheme_Object *argv[]);

static void adjust_custodian_family(void *pr, void *ignored);

static Scheme_Object *make_will_executor(int argc, Scheme_Object *args[]);
static Scheme_Object *will_executor_p(int argc, Scheme_Object *args[]);
static Scheme_Object *register_will(int argc, Scheme_Object *args[]);
static Scheme_Object *will_executor_try(int argc, Scheme_Object *args[]);
static Scheme_Object *will_executor_go(int argc, Scheme_Object *args[]);
static Scheme_Object *will_executor_sema(Scheme_Object *w, int *repost);

static Scheme_Object *check_break_now(int argc, Scheme_Object *args[]);
static int syncing_ready(Scheme_Object *s, Scheme_Schedule_Info *sinfo);

static void make_initial_config(Scheme_Thread *p);

static int do_kill_thread(Scheme_Thread *p);
static void suspend_thread(Scheme_Thread *p);

static int check_sleep(int need_activity, int sleep_now);

static void remove_thread(Scheme_Thread *r);
static void exit_or_escape(Scheme_Thread *p);

static int resume_suspend_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo);
static int dead_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo);
static int cust_box_ready(Scheme_Object *o);

static int can_break_param(Scheme_Thread *p);

static int post_system_idle();

static Scheme_Object *current_stats(int argc, Scheme_Object *args[]);

SHARED_OK static Scheme_Object **config_map;

typedef struct {
  MZTAG_IF_REQUIRED
  short is_derived;
  Scheme_Object *key;
  Scheme_Object *guard;
  Scheme_Object *extract_guard;
  Scheme_Object *defcell;
} ParamData;

enum {
  CONFIG_DIRECT,
  CONFIG_INDIRECT
};

typedef struct Scheme_Thread_Custodian_Hop {
  Scheme_Object so;
  Scheme_Thread *p; /* really an indirection with precise gc */
} Scheme_Thread_Custodian_Hop;

SHARED_OK static Scheme_Custodian_Extractor *extractors;

#define SETJMP(p) scheme_setjmpup(&p->jmpup_buf, p, p->stack_start)
#define LONGJMP(p) scheme_longjmpup(&p->jmpup_buf)
#define RESETJMP(p) scheme_reset_jmpup_buf(&p->jmpup_buf)

#ifdef WIN32_THREADS
/* Only set up for Boehm GC that thinks it's a DLL: */
# define GC_THINKS_ITS_A_DLL_BUT_ISNT

# ifdef GC_THINKS_ITS_A_DLL_BUT_ISNT
extern BOOL WINAPI DllMain(HINSTANCE inst, ULONG reason, LPVOID reserved);
# endif
#endif

#ifndef MZ_PRECISE_GC
# define scheme_thread_hop_type scheme_thread_type
#endif

SHARED_OK Scheme_Object *initial_cmdline_vec;

/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/

void scheme_init_thread(Scheme_Env *env)
{
#ifdef MZ_PRECISE_GC
    register_traversers();
#endif

  REGISTER_SO(read_symbol);
  REGISTER_SO(write_symbol);
  REGISTER_SO(execute_symbol);
  REGISTER_SO(delete_symbol);
  REGISTER_SO(exists_symbol);
  REGISTER_SO(client_symbol);
  REGISTER_SO(server_symbol);

  read_symbol = scheme_intern_symbol("read");
  write_symbol = scheme_intern_symbol("write");
  execute_symbol = scheme_intern_symbol("execute");
  delete_symbol = scheme_intern_symbol("delete");
  exists_symbol = scheme_intern_symbol("exists");
  client_symbol = scheme_intern_symbol("client");
  server_symbol = scheme_intern_symbol("server");
  
  GLOBAL_PRIM_W_ARITY("dump-memory-stats"            , scheme_dump_gc_stats, 0, -1, env);
  GLOBAL_PRIM_W_ARITY("vector-set-performance-stats!", current_stats       , 1, 2, env);

  GLOBAL_PRIM_W_ARITY("make-empty-namespace", scheme_make_namespace, 0, 0, env);

  GLOBAL_PRIM_W_ARITY("thread"                , sch_thread         , 1, 1, env);
  GLOBAL_PRIM_W_ARITY("thread/suspend-to-kill", sch_thread_nokill  , 1, 1, env);
  GLOBAL_PRIM_W_ARITY("sleep"                 , sch_sleep          , 0, 1, env);
  GLOBAL_FOLDING_PRIM("thread?"               , thread_p           , 1, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("thread-running?"       , thread_running_p   , 1, 1, env);
  GLOBAL_PRIM_W_ARITY("thread-dead?"          , thread_dead_p      , 1, 1, env);
  GLOBAL_PRIM_W_ARITY("thread-wait"           , thread_wait        , 1, 1, env);
  GLOBAL_PRIM_W_ARITY("current-thread"        , sch_current        , 0, 0, env);
  GLOBAL_PRIM_W_ARITY("kill-thread"           , kill_thread        , 1, 1, env);
  GLOBAL_PRIM_W_ARITY("break-thread"          , break_thread       , 1, 2, env);
  GLOBAL_PRIM_W_ARITY("thread-suspend"        , thread_suspend     , 1, 1, env);
  GLOBAL_PRIM_W_ARITY("thread-resume"         , thread_resume      , 1, 2, env);
  GLOBAL_PRIM_W_ARITY("thread-resume-evt"     , make_thread_resume , 1, 1, env);
  GLOBAL_PRIM_W_ARITY("thread-suspend-evt"    , make_thread_suspend, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("thread-dead-evt"       , make_thread_dead   , 1, 1, env);

  register_thread_sync();
  scheme_add_evt(scheme_thread_suspend_type, (Scheme_Ready_Fun)resume_suspend_ready, NULL, NULL, 1);
  scheme_add_evt(scheme_thread_resume_type, (Scheme_Ready_Fun)resume_suspend_ready, NULL, NULL, 1);
  scheme_add_evt(scheme_thread_dead_type, (Scheme_Ready_Fun)dead_ready, NULL, NULL, 1);
  scheme_add_evt(scheme_cust_box_type, cust_box_ready, NULL, NULL, 0);


  GLOBAL_PARAMETER("current-custodian"        , current_custodian    , MZCONFIG_CUSTODIAN, env);
  GLOBAL_PRIM_W_ARITY("make-custodian"        , make_custodian       , 0, 1, env);
  GLOBAL_FOLDING_PRIM("custodian?"            , custodian_p          , 1, 1, 1  , env);
  GLOBAL_PRIM_W_ARITY("custodian-shutdown-all", custodian_close_all  , 1, 1, env);
  GLOBAL_PRIM_W_ARITY("custodian-managed-list", custodian_to_list    , 2, 2, env);
  GLOBAL_PRIM_W_ARITY("make-custodian-box"    , make_custodian_box   , 2, 2, env);
  GLOBAL_PRIM_W_ARITY("custodian-box-value"   , custodian_box_value  , 1, 1, env);
  GLOBAL_FOLDING_PRIM("custodian-box?"        , custodian_box_p      , 1, 1, 1  , env);
  GLOBAL_PRIM_W_ARITY("call-in-nested-thread" , call_as_nested_thread, 1, 2, env);

  GLOBAL_PARAMETER("current-plumber"          , current_plumber      , MZCONFIG_PLUMBER, env);
  GLOBAL_PRIM_W_ARITY("make-plumber"          , make_plumber       , 0, 0, env);
  GLOBAL_FOLDING_PRIM("plumber?"              , plumber_p          , 1, 1, 1  , env);
  GLOBAL_PRIM_W_ARITY("plumber-flush-all"     , plumber_flush_all   , 1, 1, env);
  GLOBAL_PRIM_W_ARITY("plumber-add-flush!"    , plumber_add_flush   , 2, 3, env);
  GLOBAL_PRIM_W_ARITY("plumber-flush-handle-remove!" , plumber_remove_flush, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("plumber-flush-handle?" , plumber_flush_p     , 1, 1, env);

  GLOBAL_PARAMETER("current-namespace"      , current_namespace, MZCONFIG_ENV, env);
  GLOBAL_PRIM_W_ARITY("namespace?"          , namespace_p          , 1, 1, env);

  GLOBAL_PRIM_W_ARITY("security-guard?"    , security_guard_p   , 1, 1, env);
  GLOBAL_PRIM_W_ARITY("make-security-guard", make_security_guard, 3, 4, env);
  GLOBAL_PARAMETER("current-security-guard", current_security_guard, MZCONFIG_SECURITY_GUARD, env);

  GLOBAL_PRIM_W_ARITY("thread-group?"    , thread_set_p   , 1, 1, env);
  GLOBAL_PRIM_W_ARITY("make-thread-group", make_thread_set, 0, 1, env);
  GLOBAL_PARAMETER("current-thread-group", current_thread_set, MZCONFIG_THREAD_SET, env);

  GLOBAL_PRIM_W_ARITY("parameter?"            , parameter_p           , 1, 1, env);
  GLOBAL_PRIM_W_ARITY("make-parameter"        , make_parameter        , 1, 2, env);
  GLOBAL_PRIM_W_ARITY("make-derived-parameter", make_derived_parameter, 3, 3, env);
  GLOBAL_PRIM_W_ARITY("parameter-procedure=?" , parameter_procedure_eq, 2, 2, env);
  GLOBAL_PRIM_W_ARITY("parameterization?"     , parameterization_p    , 1, 1, env);

  GLOBAL_PRIM_W_ARITY("thread-cell?"                        , thread_cell_p     , 1, 1, env);
  GLOBAL_PRIM_W_ARITY("make-thread-cell"                    , make_thread_cell  , 1, 2, env);
  GLOBAL_PRIM_W_ARITY("thread-cell-ref"                     , thread_cell_get   , 1, 1, env);
  GLOBAL_PRIM_W_ARITY("thread-cell-set!"                    , thread_cell_set   , 2, 2, env);
  GLOBAL_PRIM_W_ARITY("current-preserved-thread-cell-values", thread_cell_values, 0, 1, env);
  GLOBAL_FOLDING_PRIM("thread-cell-values?"                 , is_thread_cell_values, 1, 1, 1, env);

  GLOBAL_PRIM_W_ARITY("make-will-executor", make_will_executor, 0, 0, env);
  GLOBAL_PRIM_W_ARITY("will-executor?"    , will_executor_p   , 1, 1, env);
  GLOBAL_PRIM_W_ARITY("will-register"     , register_will     , 3, 3, env);
  GLOBAL_PRIM_W_ARITY("will-try-execute"  , will_executor_try , 1, 1, env);
  GLOBAL_PRIM_W_ARITY("will-execute"      , will_executor_go  , 1, 1, env);
  
  scheme_add_evt_through_sema(scheme_will_executor_type, will_executor_sema, NULL);


  GLOBAL_PRIM_W_ARITY("collect-garbage"                       , collect_garbage      , 0, 0, env);
  GLOBAL_PRIM_W_ARITY("current-memory-use"                    , current_memory_use   , 0, 1, env);

  GLOBAL_PRIM_W_ARITY("custodian-require-memory"              , custodian_require_mem, 3, 3, env);
  GLOBAL_PRIM_W_ARITY("custodian-limit-memory"                , custodian_limit_mem  , 2, 3, env);
  GLOBAL_PRIM_W_ARITY("custodian-memory-accounting-available?", custodian_can_mem    , 0, 0, env);
  

  GLOBAL_FOLDING_PRIM("evt?"                      , evt_p                        , 1, 1 , 1, env);
  GLOBAL_PRIM_W_ARITY2("sync"                     , sch_sync                     , 1, -1, 0, -1, env);
  GLOBAL_PRIM_W_ARITY2("sync/timeout"             , sch_sync_timeout             , 2, -1, 0, -1, env);
  GLOBAL_PRIM_W_ARITY2("sync/enable-break"        , sch_sync_enable_break        , 1, -1, 0, -1, env);
  GLOBAL_PRIM_W_ARITY2("sync/timeout/enable-break", sch_sync_timeout_enable_break, 2, -1, 0, -1, env);
  GLOBAL_PRIM_W_ARITY("choice-evt"                , evts_to_evt                  , 0, -1, env);

  GLOBAL_PARAMETER("current-thread-initial-stack-size", current_thread_initial_stack_size, MZCONFIG_THREAD_INIT_STACK_SIZE, env);

  GLOBAL_PRIM_W_ARITY("phantom-bytes?", phantom_bytes_p, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("make-phantom-bytes", make_phantom_bytes, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("set-phantom-bytes!", set_phantom_bytes, 2, 2, env);
}

void scheme_init_thread_places(void) {
  buffer_init_size = INIT_TB_SIZE;
  REGISTER_SO(recycle_cell);
  REGISTER_SO(maybe_recycle_cell);
  REGISTER_SO(gc_prepost_callback_descs);
  REGISTER_SO(place_local_misc_table);
  REGISTER_SO(gc_info_prefab);
  REGISTER_SO(initial_config);
  gc_info_prefab = scheme_lookup_prefab_type(scheme_intern_symbol("gc-info"), 10);
}

void scheme_init_memtrace(Scheme_Env *env)
{
  Scheme_Object *v;
  Scheme_Env *newenv;

  v = scheme_intern_symbol("#%memtrace");
  newenv = scheme_primitive_module(v, env);
    
  v = scheme_make_symbol("memory-trace-continuation-mark");
  scheme_add_global("memory-trace-continuation-mark", v , newenv);
  v = scheme_make_prim_w_arity(new_tracking_fun, 
                              "new-memtrace-tracking-function", 1, 1);
  scheme_add_global("new-memtrace-tracking-function", v, newenv);
  v = scheme_make_prim_w_arity(union_tracking_val, 
                               "unioned-memtrace-tracking-value", 1, 1);
  scheme_add_global("unioned-memtrace-tracking-value", v, newenv);
  scheme_finish_primitive_module(newenv);
}

void scheme_init_inspector() {
  REGISTER_SO(initial_inspector);
  initial_inspector = scheme_make_initial_inspectors();
  /* Keep the initial inspector in case someone resets Racket (by
     calling scheme_basic_env() a second time. Using the same
     inspector after a reset lets us use the same initial module
     instances. */
}

Scheme_Object *scheme_get_current_inspector()
  XFORM_SKIP_PROC
{
  Scheme_Config *c;

  if (scheme_defining_primitives) 
    return initial_inspector;

  c = scheme_current_config();
  return scheme_get_param(c, MZCONFIG_INSPECTOR);
}

Scheme_Object *scheme_get_initial_inspector(void)
{
  return initial_inspector;
}
  
void scheme_init_parameterization()
{
  REGISTER_SO(scheme_exn_handler_key);
  REGISTER_SO(scheme_parameterization_key);
  REGISTER_SO(scheme_break_enabled_key);
  scheme_exn_handler_key = scheme_make_symbol("exnh");
  scheme_parameterization_key = scheme_make_symbol("paramz");
  scheme_break_enabled_key = scheme_make_symbol("break-on?");
}

void scheme_init_paramz(Scheme_Env *env)
{
  Scheme_Object *v;
  Scheme_Env *newenv;

  v = scheme_intern_symbol("#%paramz");
  newenv = scheme_primitive_module(v, env);
  
  scheme_add_global_constant("exception-handler-key", scheme_exn_handler_key     , newenv);
  scheme_add_global_constant("parameterization-key" , scheme_parameterization_key, newenv);
  scheme_add_global_constant("break-enabled-key"    , scheme_break_enabled_key   , newenv);

  GLOBAL_PRIM_W_ARITY("extend-parameterization" , extend_parameterization , 1, -1, newenv);
  GLOBAL_PRIM_W_ARITY("check-for-break"         , check_break_now         , 0,  0, newenv);
  GLOBAL_PRIM_W_ARITY("reparameterize"          , reparameterize          , 1,  1, newenv);
  GLOBAL_PRIM_W_ARITY("make-custodian-from-main", make_custodian_from_main, 0,  0, newenv);

  GLOBAL_PRIM_W_ARITY("cache-configuration"     , cache_configuration, 2,  2, newenv);

  scheme_finish_primitive_module(newenv);
  scheme_protect_primitive_provide(newenv, NULL);
}

static Scheme_Object *collect_garbage(int c, Scheme_Object *p[])
{
  scheme_collect_garbage();

  return scheme_void;
}

static Scheme_Object *current_memory_use(int argc, Scheme_Object *args[])
{
  Scheme_Object *arg = NULL;
  uintptr_t retval = 0;

  if (argc) {
    if (SCHEME_FALSEP(args[0])) {
      arg = args[0];
    } else if (SAME_TYPE(SCHEME_TYPE(args[0]), scheme_custodian_type)) {
      arg = args[0];
    } else {
      scheme_wrong_contract("current-memory-use", 
                            "(or/c custodian? #f)", 
                            0, argc, args);
    }
  }

#ifdef MZ_PRECISE_GC
  retval = GC_get_memory_use(arg);
#else
  scheme_unused_object(arg);
  retval = GC_get_memory_use();
#endif
  
  return scheme_make_integer_value_from_unsigned(retval);
}

static Scheme_Object *cache_configuration(int argc, Scheme_Object **argv)
{
  int pos;

  if (!SCHEME_INTP(argv[0]))
    return scheme_false;

  pos = SCHEME_INT_VAL(argv[0]);
  
  if ((pos < 0) || (pos >= 2))
    return scheme_false;

  if (!configuration_callback_cache[pos]) {
    Scheme_Object *v;
    v = _scheme_apply(argv[1], 0, NULL);
    REGISTER_SO(configuration_callback_cache[pos]);
    configuration_callback_cache[pos] = v;
  }

  return configuration_callback_cache[pos];
}

/*========================================================================*/
/*                              custodians                                */
/*========================================================================*/

static void adjust_limit_table(Scheme_Custodian *c)
{
  /* If a custodian has a limit and any object or children, then it
     must not be collected and merged with its parent. To prevent
     collection, we register the custodian in the `limite_custodians'
     table. */
  if (c->has_limit) {
    if (c->elems || CUSTODIAN_FAM(c->children)) {
      if (!c->recorded) {
        c->recorded = 1;
        if (!limited_custodians)
          limited_custodians = scheme_make_hash_table(SCHEME_hash_ptr);
        scheme_hash_set(limited_custodians, (Scheme_Object *)c, scheme_true);
      }
    } else if (c->recorded) {
      c->recorded = 0;
      if (limited_custodians)
        scheme_hash_set(limited_custodians, (Scheme_Object *)c, NULL);
    }
  }
}

static Scheme_Object *custodian_require_mem(int argc, Scheme_Object *args[])
{
  intptr_t lim;
  Scheme_Custodian *c1, *c2, *cx;

  if(NOT_SAME_TYPE(SCHEME_TYPE(args[0]), scheme_custodian_type)) {
    scheme_wrong_contract("custodian-require-memory", "custodian?", 0, argc, args);
    return NULL;
  }

  if (SCHEME_INTP(args[1]) && (SCHEME_INT_VAL(args[1]) > 0)) {
    lim = SCHEME_INT_VAL(args[1]);
  } else if (SCHEME_BIGNUMP(args[1]) && SCHEME_BIGPOS(args[1])) {
    lim = 0x3fffffff; /* more memory than we actually have */
  } else {
    scheme_wrong_contract("custodian-require-memory", "exact-positive-integer?", 1, argc, args);
    return NULL;
  }

  if(NOT_SAME_TYPE(SCHEME_TYPE(args[2]), scheme_custodian_type)) {
    scheme_wrong_contract("custodian-require-memory", "custodian?", 2, argc, args);
    return NULL;
  }

  c1 = (Scheme_Custodian *)args[0];
  c2 = (Scheme_Custodian *)args[2];

  /* Check whether c1 is super to c2: */
  if (c1 == c2) {
    cx = NULL;
  } else {
    for (cx = c2; cx && NOT_SAME_OBJ(cx, c1); ) {
      cx = CUSTODIAN_FAM(cx->parent);
    }
  }
  if (!cx) {
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                     "custodian-require-memory: second custodian is not a sub-custodian of the first custodian");
  }

#ifdef MZ_PRECISE_GC
  if (GC_set_account_hook(MZACCT_REQUIRE, c1, lim, c2))
    return scheme_void;
#else
  scheme_unused_intptr(lim);
#endif

  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		   "custodian-require-memory: " NOT_SUPPORTED_STR);
  return NULL; /* doesn't get here */
}

static Scheme_Object *custodian_limit_mem(int argc, Scheme_Object *args[])
{
  intptr_t lim;
  
  if (NOT_SAME_TYPE(SCHEME_TYPE(args[0]), scheme_custodian_type)) {
    scheme_wrong_contract("custodian-limit-memory", "custodian?", 0, argc, args);
    return NULL;
  }

  if (SCHEME_INTP(args[1]) && (SCHEME_INT_VAL(args[1]) > 0)) {
    lim = SCHEME_INT_VAL(args[1]);
  } else if (SCHEME_BIGNUMP(args[1]) && SCHEME_BIGPOS(args[1])) {
    lim = 0x3fffffff; /* more memory than we actually have */
  } else {
    scheme_wrong_contract("custodian-limit-memory", "exact-positive-integer?", 1, argc, args);
    return NULL;
  }

  if (argc > 2) {
    if (NOT_SAME_TYPE(SCHEME_TYPE(args[2]), scheme_custodian_type)) {
      scheme_wrong_contract("custodian-require-memory", "custodian?", 2, argc, args);
      return NULL;
    }
  }

  ((Scheme_Custodian *)args[0])->has_limit = 1;
  adjust_limit_table((Scheme_Custodian *)args[0]);
  if (argc > 2) {
    ((Scheme_Custodian *)args[2])->has_limit = 1;
    adjust_limit_table((Scheme_Custodian *)args[2]);
  }

#ifdef MZ_PRECISE_GC
  if (GC_set_account_hook(MZACCT_LIMIT, args[0], lim, (argc > 2) ? args[2] : args[0]))
    return scheme_void;
#else
  scheme_unused_intptr(lim);
#endif

  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		   "custodian-limit-memory: " NOT_SUPPORTED_STR);
  return NULL; /* doesn't get here */
}

static Scheme_Object *custodian_can_mem(int argc, Scheme_Object *args[])
{
#ifdef MZ_PRECISE_GC
  return (GC_accouting_enabled() ? scheme_true : scheme_false);
#else
  return scheme_false;
#endif
}

static Scheme_Object *new_tracking_fun(int argc, Scheme_Object *args[])
{
  int retval = 0;

#ifdef MZ_PRECISE_GC
  retval = GC_mtrace_new_id(args[0]);
#endif

  return scheme_make_integer(retval);
}

static Scheme_Object *union_tracking_val(int argc, Scheme_Object *args[])
{
  int retval = 0;

#ifdef MZ_PRECISE_GC
  retval = GC_mtrace_union_current_with(SCHEME_INT_VAL(args[0]));
#endif

  return scheme_make_integer(retval);
}

static void ensure_custodian_space(Scheme_Custodian *m, int k)
{
  int i;

  if (m->count + k >= m->alloc) {
    Scheme_Object ***naya_boxes;
    Scheme_Custodian_Reference **naya_mrefs;
    Scheme_Close_Custodian_Client **naya_closers;
    void **naya_data;

    m->alloc = (m->alloc ? (2 * m->alloc) : 4);
    if (m->alloc < k)
      m->alloc += k;
    
    naya_boxes = MALLOC_N(Scheme_Object**, m->alloc);
    naya_closers = MALLOC_N_ATOMIC(Scheme_Close_Custodian_Client*, m->alloc);
    naya_data = MALLOC_N(void*, m->alloc);
    naya_mrefs = MALLOC_N(Scheme_Custodian_Reference*, m->alloc);

    for (i = m->count; i--; ) {
      naya_boxes[i] = m->boxes[i];
      m->boxes[i] = NULL;
      naya_closers[i] = m->closers[i];
      m->closers[i] = NULL;
      naya_data[i] = m->data[i];
      m->data[i] = NULL;
      naya_mrefs[i] = m->mrefs[i];
      m->mrefs[i] = NULL;
    }

    m->boxes = naya_boxes;
    m->closers = naya_closers;
    m->data = naya_data;
    *m->data_ptr = naya_data;
    m->mrefs = naya_mrefs;
  }
}

static void add_managed_box(Scheme_Custodian *m, 
			    Scheme_Object **box, Scheme_Custodian_Reference *mref,
			    Scheme_Close_Custodian_Client *f, void *data)
{
  int i, saw = 0;

  for (i = m->count; i--; ) {
    if (!m->boxes[i]) {
      m->boxes[i] = box;
      m->closers[i] = f;
      m->data[i] = data;
      m->mrefs[i] = mref;
      SET_MREF_POSITION(mref, i);

      m->elems++;
      adjust_limit_table(m);

      return;
    } else {
      saw++;
      if (i + saw == m->elems)
        break; /* no empty spaces left */
    }
  }

  ensure_custodian_space(m, 1);

  m->boxes[m->count] = box;
  m->closers[m->count] = f;
  m->data[m->count] = data;
  m->mrefs[m->count] = mref;
  SET_MREF_POSITION(mref, m->count);

  m->elems++;
  adjust_limit_table(m);

  m->count++;
}

static void remove_managed(Scheme_Custodian_Reference *mr, Scheme_Object *o,
			   Scheme_Close_Custodian_Client **old_f, void **old_data)
{
  Scheme_Custodian *m;
  int i, delta;

  if (!mr)
    return;
  m = CUSTODIAN_FAM(mr);
  if (!m)
    return;

  i = EXTRACT_MREF_START_POSITION(mr, m->count);
  delta = EXTRACT_MREF_POSITION_DELTA(mr, m->count);

  while (i >= 0) {
    if (i < m->count) {
      if (m->boxes[i] && SAME_OBJ((xCUSTODIAN_FAM(m->boxes[i])),  o)) {
        xCUSTODIAN_FAM(m->boxes[i]) = 0;
        m->boxes[i] = NULL;
        CUSTODIAN_FAM(m->mrefs[i]) = 0;
        m->mrefs[i] = NULL;
        if (old_f)
          *old_f = m->closers[i];
        if (old_data)
          *old_data = m->data[i];
        m->data[i] = NULL;
        --m->elems;
        adjust_limit_table(m);
        break;
      }
    }
    i -= delta;
  }

  while (m->count && !m->boxes[m->count - 1]) {
    --m->count;
  }
}

static void adjust_custodian_family(void *mgr, void *skip_move)
{
  /* Threads note: because this function is only called as a
     finalization callback, it is automatically syncronized by the GC
     locks. And it is synchronized against all finalizations, so a
     managee can't try to unregister while we're shuffling its
     custodian. */
  Scheme_Custodian *r = (Scheme_Custodian *)mgr, *parent, *m;
  int i;

  parent = CUSTODIAN_FAM(r->parent);

  if (parent) {
    /* Remove from parent's list of children: */
    if (CUSTODIAN_FAM(parent->children) == r) {
      CUSTODIAN_FAM(parent->children) = CUSTODIAN_FAM(r->sibling);
    } else {
      m = CUSTODIAN_FAM(parent->children);
      while (m && CUSTODIAN_FAM(m->sibling) != r) {
	m = CUSTODIAN_FAM(m->sibling);
      }
      if (m)
	CUSTODIAN_FAM(m->sibling) = CUSTODIAN_FAM(r->sibling);
    }

    /* Remove from global list: */
    if (CUSTODIAN_FAM(r->global_next))
      CUSTODIAN_FAM(CUSTODIAN_FAM(r->global_next)->global_prev) = CUSTODIAN_FAM(r->global_prev);
    else
      last_custodian = CUSTODIAN_FAM(r->global_prev);
    CUSTODIAN_FAM(CUSTODIAN_FAM(r->global_prev)->global_next) = CUSTODIAN_FAM(r->global_next);
    
    /* Add children to parent's list: */
    for (m = CUSTODIAN_FAM(r->children); m; ) {
      Scheme_Custodian *next = CUSTODIAN_FAM(m->sibling);
      
      CUSTODIAN_FAM(m->parent) = parent;
      CUSTODIAN_FAM(m->sibling) = CUSTODIAN_FAM(parent->children);
      CUSTODIAN_FAM(parent->children) = m;

      m = next;
    }

    adjust_limit_table(parent);

    /* Add remaining managed items to parent: */
    if (!skip_move) {
      for (i = 0; i < r->count; i++) {
	if (r->boxes[i]) {
	  CUSTODIAN_FAM(r->mrefs[i]) = parent;
	  add_managed_box(parent, r->boxes[i], r->mrefs[i], r->closers[i], r->data[i]);
#ifdef MZ_PRECISE_GC
	  {
	    Scheme_Object *o;
	    o = xCUSTODIAN_FAM(r->boxes[i]);
	    if (SAME_TYPE(SCHEME_TYPE(o), scheme_thread_hop_type)) {
	      o = WEAKIFIED(((Scheme_Thread_Custodian_Hop *)o)->p);
	      if (o)
		GC_register_thread(o, parent);
	    } else if (SAME_TYPE(SCHEME_TYPE(o), scheme_place_type)) {
              GC_register_thread(o, parent);
            }
	  }
#endif
	}
      }
    }
  }

  CUSTODIAN_FAM(r->parent) = NULL;
  CUSTODIAN_FAM(r->sibling) = NULL;
  if (!skip_move)
    CUSTODIAN_FAM(r->children) = NULL;
  CUSTODIAN_FAM(r->global_prev) = NULL;
  CUSTODIAN_FAM(r->global_next) = NULL;
}

static void do_adjust_custodian_family(void *mgr, void *for_retain)
{
  adjust_custodian_family(mgr, NULL);
}

void insert_custodian(Scheme_Custodian *m, Scheme_Custodian *parent)
{
  /* insert into parent's list: */
  CUSTODIAN_FAM(m->parent) = parent;
  if (parent) {
    CUSTODIAN_FAM(m->sibling) = CUSTODIAN_FAM(parent->children);
    CUSTODIAN_FAM(parent->children) = m;
  } else
    CUSTODIAN_FAM(m->sibling) = NULL;

  /* Insert into global chain. A custodian is always inserted
     directly after its parent, so families stay together, and
     the local list stays in the same order as the sibling list. */
  if (parent) {
    Scheme_Custodian *next;
    next = CUSTODIAN_FAM(parent->global_next);
    CUSTODIAN_FAM(m->global_next) = next;
    CUSTODIAN_FAM(m->global_prev) = parent;
    CUSTODIAN_FAM(parent->global_next) = m;
    if (next)
      CUSTODIAN_FAM(next->global_prev) = m;
    else
      last_custodian = m;
  } else {
    CUSTODIAN_FAM(m->global_next) = NULL;
    CUSTODIAN_FAM(m->global_prev) = NULL;
  }

  if (parent)
    adjust_limit_table(parent);
}

Scheme_Custodian *scheme_make_custodian(Scheme_Custodian *parent) 
{
  Scheme_Custodian *m;
  Scheme_Custodian_Reference *mw;
  void ***data_ptr;

  if (!parent)
    parent = main_custodian; /* still NULL if we're creating main; that's ok */
  
  m = MALLOC_ONE_TAGGED(Scheme_Custodian);

  m->so.type = scheme_custodian_type;

  m->alloc = m->count = 0;

  mw = MALLOC_MREF();
  m->parent = mw;
  mw = MALLOC_MREF();
  m->children = mw;
  mw = MALLOC_MREF();
  m->sibling = mw;
  mw = MALLOC_MREF();
  m->global_next = mw;
  mw = MALLOC_MREF();
  m->global_prev = mw;

  CUSTODIAN_FAM(m->children) = NULL;

  data_ptr = (void ***)scheme_malloc(sizeof(void**));
  m->data_ptr = data_ptr;

  insert_custodian(m, parent);

  scheme_add_finalizer(m, do_adjust_custodian_family, data_ptr);

  return m;
}

static void rebox_willdone_object(void *o, void *mr)
{
  Scheme_Custodian *m = CUSTODIAN_FAM((Scheme_Custodian_Reference *)mr);
  Scheme_Close_Custodian_Client *f;
  void *data;

  /* Still needs management? */
  if (m) {
#ifdef MZ_PRECISE_GC
    Scheme_Object *b;
#else
    Scheme_Object **b;
#endif

    remove_managed(mr, o, &f, &data);

#ifdef MZ_PRECISE_GC
    b = scheme_box(NULL);
#else
    b = MALLOC_ONE(Scheme_Object*); /* not atomic this time */
#endif
    xCUSTODIAN_FAM(b) = o;
    
    /* Put the custodian back: */
    CUSTODIAN_FAM((Scheme_Custodian_Reference *)mr) = m;

    add_managed_box(m, (Scheme_Object **)b, (Scheme_Custodian_Reference *)mr, f, data);
  }
}

static void managed_object_gone(void *o, void *mr)
{
  Scheme_Custodian *m = CUSTODIAN_FAM((Scheme_Custodian_Reference *)mr);

  /* Still has management? */
  if (m)
    remove_managed(mr, o, NULL, NULL);
}

int scheme_custodian_is_available(Scheme_Custodian *m) XFORM_SKIP_PROC 
/* may be called from a future thread */
{
  if (m->shut_down)
    return 0;
  return 1;
}

void scheme_custodian_check_available(Scheme_Custodian *m, const char *who, const char *what)
{
  if (!m)
    m = (Scheme_Custodian *)scheme_get_param(scheme_current_config(), MZCONFIG_CUSTODIAN);
  
  if (!scheme_custodian_is_available(m))
    scheme_contract_error(who, "the custodian has been shut down",
                          "custodian", 1, m,
                          NULL);
}

Scheme_Custodian_Reference *scheme_add_managed(Scheme_Custodian *m, Scheme_Object *o, 
					       Scheme_Close_Custodian_Client *f, void *data, 
					       int must_close)
{
#ifdef MZ_PRECISE_GC
    Scheme_Object *b;
#else
    Scheme_Object **b;
#endif
  Scheme_Custodian_Reference *mr;

  if (!m)
    m = (Scheme_Custodian *)scheme_get_param(scheme_current_config(), MZCONFIG_CUSTODIAN);
  
  if (m->shut_down) {
    /* The custodian was shut down in the time that it took
       to allocate o. This situation should be avoided if at
       all possible, but here's the fail-safe. */
    if (f)
      f(o, data);
    return NULL;
  }

#ifdef MZ_PRECISE_GC
  b = scheme_make_late_weak_box(NULL);
#else
  b = MALLOC_ONE_WEAK(Scheme_Object*);
#endif
  xCUSTODIAN_FAM(b) = o;

  mr = MALLOC_MREF();

  CUSTODIAN_FAM(mr) = m;

  /* The atomic link via the box `b' allows the execution of wills for
     o. After this, we should either drop the object or we have to
     hold on to the object strongly (for when custodian-close-all is
     called). */
  if (must_close)
    scheme_add_finalizer(o, rebox_willdone_object, mr);
  else
    scheme_add_finalizer(o, managed_object_gone, mr);

  add_managed_box(m, (Scheme_Object **)b, mr, f, data);

  return mr;
}

static void chain_close_at_exit(Scheme_Object *o, void *_data)
/* This closer is recognized specially in scheme_run_atexit_closers() */
{
  Scheme_Object *data = (Scheme_Object *)_data;
  Scheme_Close_Custodian_Client *f;
  void **fp;
  
  fp = (void **)SCHEME_CAR(data);
  
  if (fp) {
    f = (Scheme_Close_Custodian_Client *)*fp;
    SCHEME_CAR(data) = NULL;
    f(o, SCHEME_CDR(data));
  }
}

Scheme_Custodian_Reference *scheme_add_managed_close_on_exit(Scheme_Custodian *m, Scheme_Object *o, 
                                                             Scheme_Close_Custodian_Client *f, void *data)
{
  void **p;

  p = (void **)scheme_malloc_atomic(sizeof(void *));
  *p = f;

  return scheme_add_managed(m, o, 
                            chain_close_at_exit, scheme_make_raw_pair((Scheme_Object *)p, 
                                                                      (Scheme_Object *)data),
                            1);
}

void scheme_remove_managed(Scheme_Custodian_Reference *mr, Scheme_Object *o)
{
  /* Is this a good idea? I'm not sure: */
  scheme_subtract_finalizer(o, managed_object_gone, mr);
  scheme_subtract_finalizer(o, rebox_willdone_object, mr);

  remove_managed(mr, o, NULL, NULL);
}

Scheme_Thread *scheme_do_close_managed(Scheme_Custodian *m, Scheme_Exit_Closer_Func cf)
{
  Scheme_Thread *kill_self = NULL;
  Scheme_Custodian *c, *start, *next_m;
  int i, is_thread;
  Scheme_Thread *the_thread;
  Scheme_Object *o;
  Scheme_Close_Custodian_Client *f;
  void *data;

  if (!m)
    m = main_custodian;

  if (m->shut_down)
    return NULL;

  m->shut_down = 1;

  /* Need to kill children first, transitively, so find
     last decendent. The family will be the global-list from
     m to this last decendent, inclusive. */
  for (c = m; CUSTODIAN_FAM(c->children); ) {
    for (c = CUSTODIAN_FAM(c->children); CUSTODIAN_FAM(c->sibling); ) {
      c = CUSTODIAN_FAM(c->sibling);
    }
  }

  start = m;
  m = c;
  while (1) {
    /* It matters that this loop starts at the top. See
       the m->count = i assignment below. */
    for (i = m->count; i--; ) {
      if (m->boxes[i]) {

	o = xCUSTODIAN_FAM(m->boxes[i]);

	f = m->closers[i];
	data = m->data[i];

	if (o && !cf && (SAME_TYPE(SCHEME_TYPE(o), scheme_thread_hop_type))) {
	  /* We've added an indirection and made it weak. See mr_hop note above. */
	  is_thread = 1;
	  the_thread = (Scheme_Thread *)WEAKIFIED(((Scheme_Thread_Custodian_Hop *)o)->p);
	} else {
	  is_thread = 0;
	  the_thread = NULL;
	}

	xCUSTODIAN_FAM(m->boxes[i]) = NULL;
	CUSTODIAN_FAM(m->mrefs[i]) = NULL;
	
	/* Set m->count to i in case a GC happens while
	   the closer is running. */
	m->count = i;

        if (!o) {
          /* weak link disappeared */
        } else if (is_thread && !the_thread) {
	  /* Thread is already collected, so skip */
	} else if (cf) {
	  cf(o, f, data);
	} else {
	  if (is_thread) {
	    if (the_thread) {
	      /* Only kill the thread if it has no other custodians */
	      if (SCHEME_NULLP(the_thread->extra_mrefs)) {
		if (do_kill_thread(the_thread))
		  kill_self = the_thread;
	      } else {
		Scheme_Custodian_Reference *mref;

		mref = m->mrefs[i];
		if (mref == the_thread->mref) {
		  /* Designate a new main custodian for the thread */
		  mref = (Scheme_Custodian_Reference *)SCHEME_CAR(the_thread->extra_mrefs);
		  the_thread->mref = mref;
		  the_thread->extra_mrefs = SCHEME_CDR(the_thread->extra_mrefs);
#ifdef MZ_PRECISE_GC
		  GC_register_thread(the_thread, CUSTODIAN_FAM(mref));
#endif
		} else {
		  /* Just remove mref from the list of extras */
		  Scheme_Object *l, *prev = NULL;
		  for (l = the_thread->extra_mrefs; 1; l = SCHEME_CDR(l)) {
		    if (SAME_OBJ(SCHEME_CAR(l), (Scheme_Object *)mref)) {
		      if (prev)
			SCHEME_CDR(prev) = SCHEME_CDR(l);
		      else
			the_thread->extra_mrefs = SCHEME_CDR(l);
		      break;
		    }
		    prev = l;
		  }
		}
	      }
	    }
	  } else if (f) {
	    f(o, data);
	  }
	}
      }
    }

#ifdef MZ_PRECISE_GC
    {
      Scheme_Object *pr = m->cust_boxes, *wb;
      Scheme_Custodian_Box *cb;
      while (pr) {
        wb = SCHEME_CAR(pr);
        cb = (Scheme_Custodian_Box *)SCHEME_BOX_VAL(wb);
        if (cb) cb->v = NULL;
        pr = SCHEME_CDR(pr);
      }
      m->cust_boxes = NULL;
    }
#endif

    m->count = 0;
    m->alloc = 0;
    m->elems = 0;
    m->boxes = NULL;
    m->closers = NULL;
    m->data = NULL;
    *m->data_ptr = NULL;
    m->mrefs = NULL;
    m->shut_down = 1;
    
    if (SAME_OBJ(m, start))
      break;
    next_m = CUSTODIAN_FAM(m->global_prev);

    /* Remove this custodian from its parent */
    adjust_custodian_family(m, m);

    adjust_limit_table(m);
    
    m = next_m;
  }

#ifdef MZ_USE_FUTURES
  scheme_future_check_custodians();
#endif

  return kill_self;
}

static void do_close_managed(Scheme_Custodian *m)
/* The trick is that we may need to kill the thread
   that is running us. If so, delay it to the very
   end. */
{
  if (scheme_do_close_managed(m, NULL)) {
    /* Kill/suspend self */
    if (scheme_current_thread->suspend_to_kill)
      suspend_thread(scheme_current_thread);
    else
      scheme_thread_block(0.0);
  }
}

void scheme_close_managed(Scheme_Custodian *m)
{
  do_close_managed(m);

  /* Give killed threads time to die: */
  scheme_thread_block(0);
  scheme_current_thread->ran_some = 1;
}

static Scheme_Object *make_custodian(int argc, Scheme_Object *argv[])
{
  Scheme_Custodian *m;

  if (argc) {
    if (!SCHEME_CUSTODIANP(argv[0]))
      scheme_wrong_contract("make-custodian", "custodian?", 0, argc, argv);
    m = (Scheme_Custodian *)argv[0];
  } else
    m = (Scheme_Custodian *)scheme_get_param(scheme_current_config(), MZCONFIG_CUSTODIAN);

  if (m->shut_down)
    scheme_contract_error("make-custodian", 
                          "the custodian has been shut down", 
                          "custodian", 1, m,
                          NULL);

  return (Scheme_Object *)scheme_make_custodian(m);
}

static Scheme_Object *make_custodian_from_main(int argc, Scheme_Object *argv[])
{
  return (Scheme_Object *)scheme_make_custodian(NULL);
}

static Scheme_Object *custodian_p(int argc, Scheme_Object *argv[])
{
  return SCHEME_CUSTODIANP(argv[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *custodian_close_all(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_CUSTODIANP(argv[0]))
    scheme_wrong_contract("custodian-shutdown-all", "custodian?", 0, argc, argv);

  scheme_close_managed((Scheme_Custodian *)argv[0]);

  return scheme_void;
}

Scheme_Custodian* scheme_custodian_extract_reference(Scheme_Custodian_Reference *mr)
{
  return CUSTODIAN_FAM(mr);
}

int scheme_custodian_is_shut_down(Scheme_Custodian* c)
{
  return c->shut_down;
}

static Scheme_Object *extract_thread(Scheme_Object *o)
{
  return (Scheme_Object *)WEAKIFIED(((Scheme_Thread_Custodian_Hop *)o)->p);
}

void scheme_init_custodian_extractors()
{
  if (!extractors) {
    int n;
    n = scheme_num_types();
    REGISTER_SO(extractors);
    extractors = MALLOC_N_ATOMIC(Scheme_Custodian_Extractor, n);
    memset(extractors, 0, sizeof(Scheme_Custodian_Extractor) * n);
    extractors[scheme_thread_hop_type] = extract_thread;
  }
}

void scheme_add_custodian_extractor(Scheme_Type t, Scheme_Custodian_Extractor e)
{
  if (t) {
    extractors[t] = e;
  }
}

static Scheme_Object *custodian_to_list(int argc, Scheme_Object *argv[])
{
  Scheme_Custodian *m, *m2, *c;
  Scheme_Object **hold, *o;
  int i, j, cnt, kids;
  Scheme_Type type;
  Scheme_Custodian_Extractor ex;

  if (!SCHEME_CUSTODIANP(argv[0]))
    scheme_wrong_contract("custodian-managed-list", "custodian?", 0, argc, argv);
  if (!SCHEME_CUSTODIANP(argv[1]))
    scheme_wrong_contract("custodian-managed-list", "custodian?", 1, argc, argv);

  m = (Scheme_Custodian *)argv[0];
  m2 = (Scheme_Custodian *)argv[1];

  /* Check that the second manages the first: */
  c = CUSTODIAN_FAM(m->parent);
  while (c && NOT_SAME_OBJ(m2, c)) {
    c = CUSTODIAN_FAM(c->parent);
  }
  if (!c) {
    scheme_contract_error("custodian-managed-list",
                                 "the second custodian does not "
                                 "manage the first custodian",
                                 "first custodian", 1, argv[0],
                                 "second custodian", 1, argv[1],
                                 NULL);
  }

  /* Init extractors: */
  scheme_add_custodian_extractor(0, NULL);

  /* Count children: */
  kids = 0;
  for (c = CUSTODIAN_FAM(m->children); c; c = CUSTODIAN_FAM(c->sibling)) {
    kids++;
  }

  /* Do all allocation first, since custodian links are weak.
     Furthermore, allocation may trigger collection of an otherwise
     unreferenced custodian, folding its items into this one,
     so loop until we've allocated enough. */
  do {
    cnt = m->count;
    hold = MALLOC_N(Scheme_Object *, cnt + kids);
  } while (cnt < m->count);
  
  /* Put managed items into hold array: */
  for (i = m->count, j = 0; i--; ) {
    if (m->boxes[i]) {
      o = xCUSTODIAN_FAM(m->boxes[i]);
      
      type = SCHEME_TYPE(o);
      ex = extractors[type];
      if (ex) {
	o = ex(o);
      }

      if (o) {
	hold[j] = o;
	j++;
      }
    }
  }
  /* Add kids: */
  for (c = CUSTODIAN_FAM(m->children); c; c = CUSTODIAN_FAM(c->sibling)) {
    hold[j] = (Scheme_Object *)c;
    j++;
  }

  /* Convert the array to a list: */
  return scheme_build_list(j, hold);
}

static Scheme_Object *current_custodian(int argc, Scheme_Object *argv[])
{
  return scheme_param_config2("current-custodian", 
                              scheme_make_integer(MZCONFIG_CUSTODIAN),
                              argc, argv,
                              -1, custodian_p, "custodian?", 0);
}

Scheme_Custodian *scheme_get_current_custodian()
{
  return (Scheme_Custodian *) current_custodian(0, NULL);
}

static Scheme_Object *make_custodian_box(int argc, Scheme_Object *argv[])
{
  Scheme_Custodian_Box *cb;

  if (!SCHEME_CUSTODIANP(argv[0]))
    scheme_wrong_contract("make-custodian-box", "custodian?", 0, argc, argv);

  cb = MALLOC_ONE_TAGGED(Scheme_Custodian_Box);
  cb->so.type = scheme_cust_box_type;
  cb->cust = (Scheme_Custodian *)argv[0];
  cb->v = argv[1];

#ifdef MZ_PRECISE_GC
  /* 3m  */
  {
    Scheme_Object *wb, *pr, *prev;
    wb = GC_malloc_weak_box(cb, NULL, 0, 1);
    pr = scheme_make_raw_pair(wb, cb->cust->cust_boxes);
    cb->cust->cust_boxes = pr;
    cb->cust->num_cust_boxes++;
    
    /* The GC prunes the list of custodian boxes in accounting mode,
       but prune here in case accounting is never triggered. */
    if (cb->cust->num_cust_boxes > 2 * cb->cust->checked_cust_boxes) {
      prev = pr;
      pr = SCHEME_CDR(pr);
      while (pr) {
        wb = SCHEME_CAR(pr);
        if (!SCHEME_BOX_VAL(pr)) {
          SCHEME_CDR(prev) = SCHEME_CDR(pr);
          --cb->cust->num_cust_boxes;
        } else {
          prev = pr;
        }
        pr = SCHEME_CDR(pr);
      } 
      cb->cust->checked_cust_boxes = cb->cust->num_cust_boxes;
    }
  }
#else
  /* CGC */
  if (cust_box_count >= cust_box_alloc) {
    Scheme_Custodian_Box **cbs;
    if (!cust_box_alloc) {
      cust_box_alloc = 16;
      REGISTER_SO(cust_boxes);
    } else {
      cust_box_alloc = 2 * cust_box_alloc;
    }
    cbs = (Scheme_Custodian_Box **)scheme_malloc_atomic(cust_box_alloc * sizeof(Scheme_Custodian_Box *));
    memcpy(cbs, cust_boxes, cust_box_count * sizeof(Scheme_Custodian_Box *));
    cust_boxes = cbs;
  }
  cust_boxes[cust_box_count++] = cb;
#endif

  return (Scheme_Object *)cb;
}

static Scheme_Object *custodian_box_value(int argc, Scheme_Object *argv[])
{
  Scheme_Custodian_Box *cb;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_cust_box_type))
    scheme_wrong_contract("custodian-box-value", "custodian-box?", 0, argc, argv);

  cb = (Scheme_Custodian_Box *)argv[0];
  if (cb->cust->shut_down)
    return scheme_false;

  return cb->v;
}

static Scheme_Object *custodian_box_p(int argc, Scheme_Object *argv[])
{
  if (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_cust_box_type))
    return scheme_true;
  else
    return scheme_false;
}

static int cust_box_ready(Scheme_Object *o)
{
  return ((Scheme_Custodian_Box *)o)->cust->shut_down;
}


#ifndef MZ_PRECISE_GC
void scheme_clean_cust_box_list(void)
{
  int src = 0, dest = 0;
  Scheme_Custodian_Box *cb;
  void *b;

  while (src < cust_box_count) {
    cb = cust_boxes[src];
    b = GC_base(cb);
    if (b 
#ifndef USE_SENORA_GC
        && GC_is_marked(b)
#endif
        ) {
      cust_boxes[dest++] = cb;
      if (cb->v) {
        if (cb->cust->shut_down) {
          cb->v = NULL;
        }
      }
    }
    src++;
  }
  cust_box_count = dest;
}

static void shrink_cust_box_array(void)
{
  /* Call this function periodically to clean up. */
  if (cust_box_alloc > 128 && (cust_box_count * 4 < cust_box_alloc)) {
    Scheme_Custodian_Box **cbs;
    cust_box_alloc = cust_box_count * 2;
    cbs = (Scheme_Custodian_Box **)scheme_malloc_atomic(cust_box_alloc * sizeof(Scheme_Custodian_Box *));
    memcpy(cbs, cust_boxes, cust_box_count * sizeof(Scheme_Custodian_Box *));
    cust_boxes = cbs;
  }
}
#else
# define shrink_cust_box_array() /* empty */
# define clean_cust_box_list()   /* empty */
#endif

void scheme_run_atexit_closers(Scheme_Object *o, Scheme_Close_Custodian_Client *f, void *data)
{
  Scheme_Object *l;

  if (cust_closers) {
    for (l = cust_closers; SCHEME_RPAIRP(l); l = SCHEME_CDR(l)) {
      Scheme_Exit_Closer_Func cf;
      cf = (Scheme_Exit_Closer_Func)SCHEME_CAR(l);
      cf(o, f, data);
    }
  }

  if (f == chain_close_at_exit)
    f(o, data);
}

void scheme_run_atexit_closers_on_all(Scheme_Exit_Closer_Func alt)
{
  mz_jmp_buf newbuf, *savebuf;

  /* scheme_start_atomic(); */
  /* Atomic would be needed if this was run to implement
     a custodian shutdown, but an actual custodian shutdown
     will have terminated everything else anyway. For a
     polite exit, other threads can run. */

  savebuf = scheme_current_thread->error_buf;
  scheme_current_thread->error_buf = &newbuf;
  if (!scheme_setjmp(newbuf)) {  
    scheme_do_close_managed(NULL, alt ? alt : scheme_run_atexit_closers);
  }
  scheme_current_thread->error_buf = savebuf;
}

void do_run_atexit_closers_on_all()
{
  scheme_run_atexit_closers_on_all(NULL);
}

void scheme_set_atexit(Scheme_At_Exit_Proc p)
{
  replacement_at_exit = p;
}

void scheme_add_atexit_closer(Scheme_Exit_Closer_Func f)
{
  if (!cust_closers) {
    if (replacement_at_exit) {
      replacement_at_exit(do_run_atexit_closers_on_all);
    } else {
#ifdef USE_ON_EXIT_FOR_ATEXIT
      on_exit(do_run_atexit_closers_on_all, NULL);
#else
      atexit(do_run_atexit_closers_on_all);
#endif
    }

    REGISTER_SO(cust_closers);
    cust_closers = scheme_null;
  }

  cust_closers = scheme_make_raw_pair((Scheme_Object *)f, cust_closers);
}

void scheme_schedule_custodian_close(Scheme_Custodian *c)
{
  /* This procedure might be called by a garbage collector to register
     a resource-based kill. */

  if (!scheduled_kills) {
    REGISTER_SO(scheduled_kills);
    scheduled_kills = scheme_null;
  }

  scheduled_kills = scheme_make_pair((Scheme_Object *)c, scheduled_kills);
  scheme_fuel_counter = 0;
  scheme_jit_stack_boundary = (uintptr_t)-1;
}

static void check_scheduled_kills()
{
  if (scheme_no_stack_overflow) {
    /* don't shutdown something that may be in an atomic callback */
    return;
  }

  while (scheduled_kills && !SCHEME_NULLP(scheduled_kills)) {
    Scheme_Object *k;
    k = SCHEME_CAR(scheduled_kills);
    scheduled_kills = SCHEME_CDR(scheduled_kills);
    do_close_managed((Scheme_Custodian *)k);
  }
}

static void check_current_custodian_allows(const char *who, Scheme_Thread *p)
{
  Scheme_Object *l;
  Scheme_Custodian_Reference *mref;
  Scheme_Custodian *m, *current;

  /* Check management of the thread: */
  current = (Scheme_Custodian *)scheme_get_param(scheme_current_config(), MZCONFIG_CUSTODIAN);

  for (l = p->extra_mrefs; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    mref = (Scheme_Custodian_Reference *)SCHEME_CAR(l);
    m = CUSTODIAN_FAM(mref);
    while (NOT_SAME_OBJ(m, current)) {
      m = CUSTODIAN_FAM(m->parent);
      if (!m)
	goto bad;
    }
  }

  mref = p->mref;
  if (!mref)
    return;
  m = CUSTODIAN_FAM(mref);
  if (!m)
    return;

  while (NOT_SAME_OBJ(m, current)) {
    m = CUSTODIAN_FAM(m->parent);
    if (!m)
      goto bad;
  }

  return;

 bad:
  scheme_contract_error(who,
                        "the current custodian does not "
                        "solely manage the specified thread",
                        "thread", 1, p,
                        NULL);
}

void scheme_free_all(void)
{
  scheme_do_close_managed(NULL, NULL);
  scheme_free_dynamic_extensions();
#ifdef MZ_PRECISE_GC
  GC_free_all();
#endif
}

/*========================================================================*/
/*                              plumbers                                  */
/*========================================================================*/

#define FLUSH_HANDLE_FLAGS(h) MZ_OPT_HASH_KEY(&((Scheme_Small_Object *)h)->iso)

Scheme_Object *get_plumber_handles(Scheme_Plumber *p)
{
  Scheme_Object *v, *r = scheme_null;
  Scheme_Bucket_Table *bt;
  Scheme_Hash_Table *ht;
  int i;

  bt = p->weak_handles;
  if (bt) {
    for (i = bt->size; i--; ) {
      if (bt->buckets[i]) {
        v = (Scheme_Object *)HT_EXTRACT_WEAK(bt->buckets[i]->key);
        if (v) {
          r = scheme_make_pair(v, r);
          SCHEME_USE_FUEL(1);
        }
      }
    }
  }

  ht = p->handles;
  for (i = ht->size; i--; ) {
    if (ht->vals[i])
      r = scheme_make_pair(ht->keys[i], r);
    SCHEME_USE_FUEL(1);
  }

  return r;
}

int scheme_flush_managed(Scheme_Plumber *p, int catch_errors)
{
  Scheme_Object *r, *h, *o, *a[1];
  Scheme_Thread *pt;
  mz_jmp_buf * volatile saved_error_buf;
  mz_jmp_buf new_error_buf;
  volatile int escaped = 0;

  if (!p) p = initial_plumber;

  if (catch_errors) {
    pt = scheme_current_thread;
    saved_error_buf = pt->error_buf;
    pt->error_buf = &new_error_buf;
  } else
    saved_error_buf = NULL;
  
  if (!scheme_setjmp(new_error_buf)) {
    r = get_plumber_handles(p);
    
    while (!SCHEME_NULLP(r)) {
      h = SCHEME_CAR(r);
      
      o = SCHEME_PTR2_VAL(h);

      if (SCHEME_OUTPORTP(o)) {
        scheme_flush_if_output_fds(o);
      } else {
        a[0] = h;
        (void)scheme_apply_multi(o, 1, a);
      }
      
      r = SCHEME_CDR(r);
    }
  } else {
    escaped = 1;
  }

  if (catch_errors)
    scheme_current_thread->error_buf = saved_error_buf;

  return escaped;
}

static Scheme_Object *make_plumber(int argc, Scheme_Object *argv[])
{
  Scheme_Plumber *p;
  Scheme_Hash_Table *ht;

  p = MALLOC_ONE_TAGGED(Scheme_Plumber);
  p->so.type = scheme_plumber_type;
  
  ht = scheme_make_hash_table(SCHEME_hash_ptr);
  p->handles = ht;

  return (Scheme_Object *)p;
}

static Scheme_Object *plumber_p(int argc, Scheme_Object *argv[])
{
  return SCHEME_PLUMBERP(argv[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *plumber_flush_all(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_PLUMBERP(argv[0]))
    scheme_wrong_contract("plumber-flush-all", "plumber?", 0, argc, argv);
  
  scheme_flush_managed((Scheme_Plumber *)argv[0], 0);

  return scheme_void;
}

Scheme_Object *scheme_add_flush(Scheme_Plumber *p, Scheme_Object *proc_or_port, int weak_flush)
{
  Scheme_Object *h;

  if (!p)
    p = (Scheme_Plumber *)scheme_get_param(scheme_current_config(), MZCONFIG_PLUMBER);
  
  h = scheme_alloc_object();
  h->type = scheme_plumber_handle_type;
  SCHEME_PTR1_VAL(h) = (Scheme_Object *)p;
  SCHEME_PTR2_VAL(h) = proc_or_port;

  if (weak_flush) {
    FLUSH_HANDLE_FLAGS(h) |= 0x1;
    if (!p->weak_handles) {
      Scheme_Bucket_Table *bt;
      bt = scheme_make_bucket_table(4, SCHEME_hash_weak_ptr);
      p->weak_handles = bt;
    }
    scheme_add_to_table(p->weak_handles, (const char *)h, scheme_true, 0);
  } else
    scheme_hash_set(p->handles, h, scheme_true);

  return h;
}

static Scheme_Object *plumber_add_flush(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_PLUMBERP(argv[0]))
    scheme_wrong_contract("plumber-add-flush!", "plumber?", 0, argc, argv);
  scheme_check_proc_arity("plumber-add-flush!", 1, 1, argc, argv);

  return scheme_add_flush((Scheme_Plumber *)argv[0], argv[1], 
                          (argc > 2) && SCHEME_TRUEP(argv[2]));
}

void scheme_remove_flush(Scheme_Object *h)
{
  Scheme_Plumber *p;

  p = (Scheme_Plumber *)SCHEME_PTR1_VAL(h);

  if (p) {
    if (FLUSH_HANDLE_FLAGS(h) & 0x1) {
      Scheme_Bucket *b;
      b = scheme_bucket_or_null_from_table(p->weak_handles, (char *)h, 0);
      if (b) {
        HT_EXTRACT_WEAK(b->key) = NULL;
        b->val = NULL;
      }
    } else
      scheme_hash_set(p->handles, h, NULL);
    SCHEME_PTR1_VAL(h) = NULL;
    SCHEME_PTR2_VAL(h) = NULL;
  }
}

static Scheme_Object *plumber_remove_flush(int argc, Scheme_Object *argv[])
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_plumber_handle_type))
    scheme_wrong_contract("plumber-flush-handle-remove!", "plumber-handle?", 0, argc, argv);
  
  scheme_remove_flush(argv[0]);

  return scheme_void;  
}

static Scheme_Object *plumber_flush_p(int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_plumber_handle_type)
          ? scheme_true
          : scheme_false);
}

static Scheme_Object *current_plumber(int argc, Scheme_Object *argv[])
{
  return scheme_param_config2("current-plumber", 
                              scheme_make_integer(MZCONFIG_PLUMBER),
                              argc, argv,
                              -1, plumber_p, "plumber?", 0);
}

/*========================================================================*/
/*                             thread sets                                */
/*========================================================================*/

#define TSET_IL MZ_INLINE

static Scheme_Thread_Set *create_thread_set(Scheme_Thread_Set *parent)
{
  Scheme_Thread_Set *t_set;

  t_set = MALLOC_ONE_TAGGED(Scheme_Thread_Set);
  t_set->so.type = scheme_thread_set_type;

  t_set->parent = parent;

  /* Everything in t_set is zeroed */

  return t_set;
}

static Scheme_Object *make_thread_set(int argc, Scheme_Object *argv[])
{
  Scheme_Thread_Set *parent;

  if (argc) {
    if (!(SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_thread_set_type)))
      scheme_wrong_contract("make-thread-group", "thread-group?", 0, argc, argv);
    parent = (Scheme_Thread_Set *)argv[0];
  } else
    parent = (Scheme_Thread_Set *)scheme_get_param(scheme_current_config(), MZCONFIG_THREAD_SET);

  return (Scheme_Object *)create_thread_set(parent);
}

static Scheme_Object *thread_set_p(int argc, Scheme_Object *argv[])
{
  return ((SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_thread_set_type)) 
	  ? scheme_true 
	  : scheme_false);
}

static Scheme_Object *current_thread_set(int argc, Scheme_Object *argv[])
{
  return scheme_param_config2("current-thread-group", 
                              scheme_make_integer(MZCONFIG_THREAD_SET),
                              argc, argv,
                              -1, thread_set_p, "thread-group?", 0);
}

static TSET_IL void set_t_set_next(Scheme_Object *o, Scheme_Object *n)
{
  if (SCHEME_THREADP(o))
    ((Scheme_Thread *)o)->t_set_next = n;
  else
    ((Scheme_Thread_Set *)o)->next = n;
}

static TSET_IL void set_t_set_prev(Scheme_Object *o, Scheme_Object *n)
{
  if (SCHEME_THREADP(o))
    ((Scheme_Thread *)o)->t_set_prev = n;
  else
    ((Scheme_Thread_Set *)o)->prev = n;
}

static TSET_IL Scheme_Object *get_t_set_next(Scheme_Object *o)
{
  if (SCHEME_THREADP(o))
    return ((Scheme_Thread *)o)->t_set_next;
  else
    return ((Scheme_Thread_Set *)o)->next;
}

static TSET_IL Scheme_Object *get_t_set_prev(Scheme_Object *o)
{
  if (SCHEME_THREADP(o))
    return ((Scheme_Thread *)o)->t_set_prev;
  else
    return ((Scheme_Thread_Set *)o)->prev;
}

static void schedule_in_set(Scheme_Object *s, Scheme_Thread_Set *t_set)
{
  num_running_threads += 1;

  while (1) {
    set_t_set_next(s, t_set->first);
    if (t_set->first)
      set_t_set_prev(t_set->first, s);
    t_set->first = s;
    if (t_set->current)
      break;

    t_set->current = s;

    s = (Scheme_Object *)t_set;
    t_set = t_set->parent;
  }
}

static void unschedule_in_set(Scheme_Object *s, Scheme_Thread_Set *t_set)
{
  Scheme_Object *prev;
  Scheme_Object *next;

  num_running_threads -= 1;

  while (1) {
    prev = get_t_set_prev(s);
    next = get_t_set_next(s);

    if (!prev)
      t_set->first = next;
    else
      set_t_set_next(prev, next);
    if (next)
      set_t_set_prev(next, prev);
    set_t_set_prev(s, NULL);
    set_t_set_next(s, NULL);
    
    if (t_set->current == s) {
      if (next) {
	t_set->current = next;
      } else {
	t_set->current = t_set->first;
      }
    }
    
    if (t_set->current)
      break;
    
    s = (Scheme_Object *)t_set;
    t_set = t_set->parent;
  }
}

/*========================================================================*/
/*                      thread record creation                            */
/*========================================================================*/

static Scheme_Thread *make_thread(Scheme_Config *config, 
				  Scheme_Thread_Cell_Table *cells,
				  Scheme_Object *init_break_cell,
				  Scheme_Custodian *mgr,
          void *stack_base)
{
  Scheme_Thread *process;
  int prefix = 0;

  process = MALLOC_ONE_TAGGED(Scheme_Thread);

  process->so.type = scheme_thread_type;

  if (!scheme_main_thread) {
    /* Creating the first thread... */
    REGISTER_SO(scheme_current_thread);
    REGISTER_SO(scheme_main_thread);
    REGISTER_SO(scheme_first_thread);
    REGISTER_SO(thread_swap_callbacks);
    REGISTER_SO(thread_swap_out_callbacks);
    REGISTER_SO(swap_target);

    scheme_current_thread = process;
    scheme_first_thread = scheme_main_thread = process;
    process->prev = NULL;
    process->next = NULL;

    gc_prep_thread_chain = process;
    scheme_current_thread->gc_prep_chain = process;

    process->suspend_break = 1; /* until start-up finished */

    process->error_buf = NULL;

    thread_swap_callbacks = scheme_null;
    thread_swap_out_callbacks = scheme_null;

    GC_set_collect_start_callback(get_ready_for_GC);
    GC_set_collect_end_callback(done_with_GC);
#ifdef MZ_PRECISE_GC
    GC_set_collect_inform_callback(inform_GC);
#endif

#ifdef LINK_EXTENSIONS_BY_TABLE
    scheme_current_thread_ptr = &scheme_current_thread;
    scheme_fuel_counter_ptr = &scheme_fuel_counter;
#endif
    
#if defined(MZ_PRECISE_GC)
    GC_set_get_thread_stack_base(scheme_get_current_thread_stack_start);
#endif
    process->stack_start = stack_base;

  } else {
    prefix = 1;
  }

  process->engine_weight = 10000;

  process->cont_mark_pos = (MZ_MARK_POS_TYPE)1;
  process->cont_mark_stack = 0;
  process->cont_mark_stack_segments = NULL;
  process->cont_mark_seg_count = 0;

  if (!config) {
    make_initial_config(process);
    config = process->init_config;
  } else {
    process->init_config = config;
    process->cell_values = cells;
  }

  if (init_break_cell) {
    process->init_break_cell = init_break_cell;
  } else {
    Scheme_Object *v;
    v = scheme_make_thread_cell(scheme_false, 1);
    process->init_break_cell = v;
  }

  if (!mgr)
    mgr = (Scheme_Custodian *)scheme_get_param(config, MZCONFIG_CUSTODIAN);

#ifdef MZ_PRECISE_GC
  GC_register_new_thread(process, mgr);
#endif

  {
    Scheme_Object *t_set;
    t_set = scheme_get_param(config, MZCONFIG_THREAD_SET);
    process->t_set_parent = (Scheme_Thread_Set *)t_set;
  }
  
  if (SAME_OBJ(process, scheme_first_thread)) {
    REGISTER_SO(scheme_thread_set_top);
    scheme_thread_set_top = process->t_set_parent;
    scheme_thread_set_top->first = (Scheme_Object *)process;
    scheme_thread_set_top->current = (Scheme_Object *)process;
  } else
    schedule_in_set((Scheme_Object *)process, process->t_set_parent);
    
  scheme_init_jmpup_buf(&process->jmpup_buf);

  process->running = MZTHREAD_RUNNING;

  process->dw = NULL;

  process->block_descriptor = NOT_BLOCKED;
  process->block_check = NULL;
  process->block_needs_wakeup = NULL;
  process->sleep_end = 0;

  process->current_local_env = NULL;

  process->external_break = 0;

  process->ran_some = 1;

  process->list_stack = NULL;

  scheme_gmp_tls_init(process->gmp_tls);
  
  if (prefix) {
    process->next = scheme_first_thread;
    process->prev = NULL;
    process->next->prev = process;
    scheme_first_thread = process;
  }

  if (!buffer_init_size) /* => before place init */
    buffer_init_size = INIT_TB_SIZE;

  {
    Scheme_Object **tb;
    tb = MALLOC_N(Scheme_Object *, buffer_init_size);
    process->tail_buffer = tb;
  }
  process->tail_buffer_size = buffer_init_size;
 
  {
    int init_stack_size;
    Scheme_Object *iss;

    iss = scheme_get_thread_param(config, cells, MZCONFIG_THREAD_INIT_STACK_SIZE);
    if (SCHEME_INTP(iss))
      init_stack_size = SCHEME_INT_VAL(iss);
    else if (SCHEME_BIGNUMP(iss))
      init_stack_size = 0x7FFFFFFF;
    else
      init_stack_size = DEFAULT_INIT_STACK_SIZE;
    
    /* A too-large stack size won't help performance.
       A too-small stack size is unsafe for certain kinds of
       tail calls. */
    if (init_stack_size > MAX_INIT_STACK_SIZE)
      init_stack_size = MAX_INIT_STACK_SIZE;
    if (init_stack_size < SCHEME_TAIL_COPY_THRESHOLD)
      init_stack_size = SCHEME_TAIL_COPY_THRESHOLD;

    process->runstack_size = init_stack_size;
    {
      Scheme_Object **sa;
      sa = scheme_alloc_runstack(init_stack_size);
      process->runstack_start = sa;
    }
    process->runstack = process->runstack_start + init_stack_size;
  }
  
  process->runstack_saved = NULL;

#ifdef RUNSTACK_IS_GLOBAL
  if (!prefix) {
# ifndef MZ_PRECISE_GC
    /* Precise GC: we intentionally don't register MZ_RUNSTACK. See done_with_GC() */
    REGISTER_SO(MZ_RUNSTACK);
# endif
    REGISTER_SO(MZ_RUNSTACK_START);

    MZ_RUNSTACK = process->runstack;
    MZ_RUNSTACK_START = process->runstack_start;
    MZ_CONT_MARK_STACK = process->cont_mark_stack;
    MZ_CONT_MARK_POS = process->cont_mark_pos;
  }
#endif

  process->on_kill = NULL;

  process->user_tls = NULL;
  process->user_tls_size = 0;
  
  process->nester = process->nestee = NULL;

  process->mbox_first = NULL;
  process->mbox_last = NULL;
  process->mbox_sema = NULL;

  process->mref = NULL;
  process->extra_mrefs = NULL;

    

  /* A thread points to a lot of stuff, so it's bad to put a finalization
     on it, which is what registering with a custodian does. Instead, we
     register a weak indirection with the custodian. That way, the thread
     (and anything it points to) can be collected one GC cycle earlier. 

     It's possible that the thread will be collected before the indirection
     record, so when we use the indirection (e.g., in custodian traversals),
     we'll need to check for NULL. */
  {
    Scheme_Thread_Custodian_Hop *hop;
    Scheme_Custodian_Reference *mref;
    hop = MALLOC_ONE_WEAK_RT(Scheme_Thread_Custodian_Hop);
    process->mr_hop = hop;
    hop->so.type = scheme_thread_hop_type;
    {
      Scheme_Thread *wp;
      wp = (Scheme_Thread *)WEAKIFY((Scheme_Object *)process);
      hop->p = wp;
    }

    mref = scheme_add_managed(mgr, (Scheme_Object *)hop, NULL, NULL, 0);
    process->mref = mref;
    process->extra_mrefs = scheme_null;

#ifndef MZ_PRECISE_GC
    scheme_weak_reference((void **)(void *)&hop->p);
#endif
  }

  return process;
}

Scheme_Thread *scheme_make_thread(void *stack_base)
{
  /* Makes the initial process. */
  return make_thread(NULL, NULL, NULL, NULL, stack_base);
}

static void scheme_check_tail_buffer_size(Scheme_Thread *p)
{
  if (p->tail_buffer_size < buffer_init_size) {
    Scheme_Object **tb;
    tb = MALLOC_N(Scheme_Object *, buffer_init_size);
    p->tail_buffer = tb;
    p->tail_buffer_size = buffer_init_size;
  }
}

void scheme_set_tail_buffer_size(int s)
{
  if (s > buffer_init_size) {
    Scheme_Thread *p;

    buffer_init_size = s;

    for (p = scheme_first_thread; p; p = p->next) {
      scheme_check_tail_buffer_size(p);
    }
  }
}

int scheme_tls_allocate()
{
  return tls_pos++;
}

void scheme_tls_set(int pos, void *v)
{
  Scheme_Thread *p = scheme_current_thread;

  if (p->user_tls_size <= pos) {
    int oldc = p->user_tls_size;
    void **old_tls = p->user_tls, **va;

    p->user_tls_size = tls_pos;
    va = MALLOC_N(void*, tls_pos);
    p->user_tls = va;
    while (oldc--) {
      p->user_tls[oldc] = old_tls[oldc];
    }
  }

  p->user_tls[pos] = v;
}

void *scheme_tls_get(int pos)
{
  Scheme_Thread *p = scheme_current_thread;

  if (p->user_tls_size <= pos)
    return NULL;
  else
    return p->user_tls[pos];
}

Scheme_Object **scheme_alloc_runstack(intptr_t len)
  XFORM_SKIP_PROC
{
#ifdef MZ_PRECISE_GC
  intptr_t sz;
  void **p;
  sz = sizeof(Scheme_Object*) * (len + 4);
  p = (void **)GC_malloc_tagged_allow_interior(sz);
  *(Scheme_Type *)(void *)p = scheme_rt_runstack;
  ((intptr_t *)(void *)p)[1] = gcBYTES_TO_WORDS(sz);
  ((intptr_t *)(void *)p)[2] = 0;
  ((intptr_t *)(void *)p)[3] = len;
  return (Scheme_Object **)(p + 4);
#else
  return (Scheme_Object **)scheme_malloc_allow_interior(sizeof(Scheme_Object*) * len);
#endif
}

void scheme_set_runstack_limits(Scheme_Object **rs, intptr_t len, intptr_t start, intptr_t end)
  XFORM_SKIP_PROC
/* With 3m, we can tell the GC not to scan the unused parts, and we
   can have the fixup function zero out the unused parts; that avoids
   writing and scanning pages that could be skipped for a minor
   GC. For CGC, we have to just clear out the unused part. */
{
#ifdef MZ_PRECISE_GC
  if (((intptr_t *)(void *)rs)[-2] != start)
    ((intptr_t *)(void *)rs)[-2] = start;
  if (((intptr_t *)(void *)rs)[-1] != end)
    ((intptr_t *)(void *)rs)[-1] = end;
#else
  memset(rs, 0, start * sizeof(Scheme_Object *));
  memset(rs + end, 0, (len - end) * sizeof(Scheme_Object *));
#endif
}

void *scheme_register_process_global(const char *key, void *val)
{
  void *old_val = NULL;
  char *key2;
  Proc_Global_Rec *pg;
  intptr_t len;

#if defined(MZ_USE_MZRT)
  if (process_global_lock)
    mzrt_mutex_lock(process_global_lock);
#endif

  for (pg = process_globals; pg; pg = pg->next) {
    if (!strcmp(pg->key, key)) {
      old_val = pg->val;
      break;
    }
  }

  if (!old_val && val) {
    len = strlen(key);
    key2 = (char *)malloc(len + 1);
    memcpy(key2, key, len + 1);
    pg = (Proc_Global_Rec *)malloc(sizeof(Proc_Global_Rec));
    pg->key = key2;
    pg->val = val;
    pg->next = process_globals;
    process_globals = pg;
  }

#if defined(MZ_USE_MZRT)
  if (process_global_lock)
    mzrt_mutex_unlock(process_global_lock);
#endif

  return old_val;
}

void scheme_init_process_globals(void)
{
#if defined(MZ_USE_MZRT)
  mzrt_mutex_create(&process_global_lock);
#endif
}

Scheme_Hash_Table *scheme_get_place_table(void)
{
  if (!place_local_misc_table)
    place_local_misc_table = scheme_make_hash_table(SCHEME_hash_ptr);
  return place_local_misc_table;
}

/*========================================================================*/
/*                     thread creation and swapping                       */
/*========================================================================*/

int scheme_in_main_thread(void)
{
  return !scheme_current_thread->next;
}

static void stash_current_marks()
{
  Scheme_Object *m;
  m = scheme_current_continuation_marks(scheme_current_thread->returned_marks);
  scheme_current_thread->returned_marks = m;
  swap_target = scheme_current_thread->return_marks_to;
  scheme_current_thread->return_marks_to = NULL;
}

static void do_swap_thread()
{
 start:

  scheme_zero_unneeded_rands(scheme_current_thread);

#if WATCH_FOR_NESTED_SWAPS
  if (swapping)
    printf("death\n");
  swapping = 1;
#endif

#ifdef MZ_USE_PLACES
  if (GC_is_using_master()) {
    scheme_log_abort("attempted thread swap during master GC use");
    abort();
  }
#endif

  if (!swap_no_setjmp && SETJMP(scheme_current_thread)) {
    /* We're back! */
    /* See also initial swap in in start_child() */
    thread_swap_count++;
#ifdef RUNSTACK_IS_GLOBAL
    MZ_RUNSTACK = scheme_current_thread->runstack;
    MZ_RUNSTACK_START = scheme_current_thread->runstack_start;
    MZ_CONT_MARK_STACK = scheme_current_thread->cont_mark_stack;
    MZ_CONT_MARK_POS = scheme_current_thread->cont_mark_pos;
#endif
    RESETJMP(scheme_current_thread);
#if WATCH_FOR_NESTED_SWAPS
    swapping = 0;
#endif
    scheme_gmp_tls_unload(scheme_current_thread->gmp_tls, scheme_current_thread->gmp_tls_data);
    scheme_current_thread->gmp_tls_data = NULL;

    {
      Scheme_Object *l, *o;
      Scheme_Closure_Func f;
      for (l = thread_swap_callbacks; SCHEME_RPAIRP(l); l = SCHEME_CDR(l)) {
	o = SCHEME_CAR(l);
	f = SCHEME_CLOS_FUNC(o);
	o = SCHEME_CLOS_DATA(o);
	f(o);
      }
    }
    if ((scheme_current_thread->runstack_owner
	 && ((*scheme_current_thread->runstack_owner) != scheme_current_thread))
	|| (scheme_current_thread->cont_mark_stack_owner
	    && ((*scheme_current_thread->cont_mark_stack_owner) != scheme_current_thread))) {
      scheme_takeover_stacks(scheme_current_thread);
    }

    scheme_current_thread->current_start_process_msec = process_time_at_swap;

    if (scheme_current_thread->return_marks_to) {
      stash_current_marks();
      goto start;
    }
  } else {
    Scheme_Thread *new_thread = swap_target;

    {
      intptr_t cpm;
      cpm = scheme_get_process_milliseconds();
      scheme_current_thread->accum_process_msec += (cpm - scheme_current_thread->current_start_process_msec);
      process_time_at_swap = cpm;
    }

    swap_target = NULL;

    swap_no_setjmp = 0;

    /* We're leaving... */

    {
      Scheme_Object *l, *o;
      Scheme_Closure_Func f;
      for (l = thread_swap_out_callbacks; SCHEME_RPAIRP(l); l = SCHEME_CDR(l)) {
	o = SCHEME_CAR(l);
	f = SCHEME_CLOS_FUNC(o);
	o = SCHEME_CLOS_DATA(o);
	f(o);
      }
    }

    if (scheme_current_thread->init_break_cell) {
      int cb;
      cb = can_break_param(scheme_current_thread);
      scheme_current_thread->can_break_at_swap = cb;
    }
    {
      GC_CAN_IGNORE void *data;
      data = scheme_gmp_tls_load(scheme_current_thread->gmp_tls);
      scheme_current_thread->gmp_tls_data = data;
    }
#ifdef RUNSTACK_IS_GLOBAL
    scheme_current_thread->runstack = MZ_RUNSTACK;
    scheme_current_thread->runstack_start = MZ_RUNSTACK_START;
    scheme_current_thread->cont_mark_stack = MZ_CONT_MARK_STACK;
    scheme_current_thread->cont_mark_pos = MZ_CONT_MARK_POS;
#endif

#ifdef MZ_USE_FUTURES
    scheme_use_rtcall = new_thread->futures_slow_path_tracing;
#endif

    scheme_current_thread = new_thread;
    if (!new_thread->gc_prep_chain) {
      new_thread->gc_prep_chain = gc_prep_thread_chain;
      gc_prep_thread_chain = new_thread;
    }

    /* Fixup current pointers in thread sets */
    if (!scheme_current_thread->return_marks_to) {
      Scheme_Thread_Set *t_set = new_thread->t_set_parent;
      t_set->current = (Scheme_Object *)new_thread;
      while (t_set->parent) {
	t_set->parent->current = (Scheme_Object *)t_set;
	t_set = t_set->parent;
      }
    }

    LONGJMP(scheme_current_thread);
  }
}

void scheme_swap_thread(Scheme_Thread *new_thread)
{
  swap_target = new_thread;
  new_thread = NULL;
  do_swap_thread();
}

static void select_thread()
{
  Scheme_Thread *new_thread;
  Scheme_Object *o;
  Scheme_Thread_Set *t_set;

  /* Try to pick a next thread to avoid DOS attacks
     through whatever kinds of things call select_thread() */
  o = (Scheme_Object *)scheme_thread_set_top;
  while (!SCHEME_THREADP(o)) {
    t_set = (Scheme_Thread_Set *)o;
    o = get_t_set_next(t_set->current);
    if (!o)
      o = t_set->first;
  }
  /* It's possible that o won't work out. So o is a suggestion for the
     new thread, but the loop below will pick a definitely suitable
     thread. */
  
  new_thread = (Scheme_Thread *)o;
  do {
    if (!new_thread)
      new_thread = scheme_first_thread;
    
    /* Can't swap in a thread with a nestee: */
    while (new_thread 
	   && (new_thread->nestee
	       || (new_thread->running & MZTHREAD_SUSPENDED)
	       /* USER_SUSPENDED should only happen if new_thread is the main thread
		  or if the thread has MZTHREAD_NEED_SUSPEND_CLEANUP */
	       || ((new_thread->running & MZTHREAD_USER_SUSPENDED)
		   && !(new_thread->running & MZTHREAD_NEED_SUSPEND_CLEANUP)))) {
      new_thread = new_thread->next;
    }

    if (!new_thread && !o) {
      /* The main thread must be blocked on a nestee, and everything
	 else is suspended. But we have to go somewhere.  Weakly
	 resume the main thread's innermost nestee. If it's
         suspended by the user, then we've deadlocked. */
      new_thread = scheme_main_thread;
      while (new_thread->nestee) {
	new_thread = new_thread->nestee;
      }
      if ((new_thread->running & MZTHREAD_USER_SUSPENDED)
	  && !(new_thread->running & MZTHREAD_NEED_SUSPEND_CLEANUP)) {
        if (post_system_idle()) {
          /* Aha! Someone was waiting for us to do nothing. Try again... */
        } else {
          scheme_console_printf("unbreakable deadlock\n");
          if (scheme_exit)
            scheme_exit(1);
          /* We really have to exit: */
          exit(1);
        }
      } else {
	scheme_weak_resume_thread(new_thread);
      }
      break;
    }
    o = NULL;
  } while (!new_thread);

  swap_target = new_thread;
  new_thread = NULL;
  o = NULL;
  t_set = NULL;
  do_swap_thread();
}

static void thread_is_dead(Scheme_Thread *r)
{
  if (r->dead_box) {
    Scheme_Object *o;
    o = SCHEME_PTR_VAL(r->dead_box);
    scheme_post_sema_all(o);
  }
  if (r->sync_box) {
    scheme_post_sema_all(r->sync_box);
    r->sync_box = NULL;
  }
  if (r->running_box) {
    SCHEME_PTR_VAL(r->running_box) = NULL;
    r->running_box = NULL;
  }
  r->suspended_box = NULL;
  r->resumed_box = NULL;
  
  r->list_stack = NULL;

  r->dw = NULL;
  r->init_config = NULL;
  r->cell_values = NULL;
  r->init_break_cell = NULL;
  r->cont_mark_stack_segments = NULL;
  r->overflow = NULL;

  r->blocker = NULL;

  r->transitive_resumes = NULL;
  
  r->error_buf = NULL;

  r->spare_runstack = NULL;

  r->mbox_first = NULL;
  r->mbox_last = NULL;
  r->mbox_sema = NULL;
}

static void remove_thread(Scheme_Thread *r)
{
  Scheme_Saved_Stack *saved;
  Scheme_Object *l;

  r->running = 0;

  if (r->prev) {
    r->prev->next = r->next;
    r->next->prev = r->prev;
  } else if (r->next) {
    r->next->prev = NULL;
    scheme_first_thread = r->next;
  }
  r->next = r->prev = NULL;

  unschedule_in_set((Scheme_Object *)r, r->t_set_parent);
  
#ifdef RUNSTACK_IS_GLOBAL
  if (r == scheme_current_thread) {
    r->runstack = MZ_RUNSTACK;
    MZ_RUNSTACK = NULL;
    r->runstack_start = MZ_RUNSTACK_START;
    MZ_RUNSTACK_START = NULL;
    r->cont_mark_stack = MZ_CONT_MARK_STACK;
    r->cont_mark_pos = MZ_CONT_MARK_POS;
  }
#endif

  if (r->runstack_owner) {
    /* Drop ownership, if active, and clear the stack */
    if (r == *(r->runstack_owner)) {
      if (r->runstack_start) {
        scheme_set_runstack_limits(r->runstack_start, r->runstack_size, 0, 0);
        r->runstack_start = NULL;
      }
      for (saved = r->runstack_saved; saved; saved = saved->prev) {
        scheme_set_runstack_limits(saved->runstack_start, saved->runstack_size, 0, 0);
      }
      r->runstack_saved = NULL;
      *(r->runstack_owner) = NULL;
      r->runstack_owner = NULL;
    }
  } else {
    /* Only this thread used the runstack, so clear/free it
       as aggressively as possible */
#if defined(SENORA_GC_NO_FREE) || defined(MZ_PRECISE_GC)
    memset(r->runstack_start, 0, r->runstack_size * sizeof(Scheme_Object*));
#else
    GC_free(r->runstack_start);
#endif
    r->runstack_start = NULL;
    for (saved = r->runstack_saved; saved; saved = saved->prev) {
#if defined(SENORA_GC_NO_FREE) || defined(MZ_PRECISE_GC)
      memset(saved->runstack_start, 0, saved->runstack_size * sizeof(Scheme_Object*));
#else
      GC_free(saved->runstack_start);
#endif
      saved->runstack_start = NULL;
    }
  }

  r->runstack = NULL;
  r->runstack_swapped = NULL;

  if (r->cont_mark_stack_owner
      && ((*r->cont_mark_stack_owner) == r)) {
    *r->cont_mark_stack_owner = NULL;
  }

  r->cont_mark_stack = 0;
  r->cont_mark_stack_owner = NULL;
  r->cont_mark_stack_swapped = NULL;

  r->ku.apply.tail_rator = NULL;
  r->ku.apply.tail_rands = NULL;
  r->tail_buffer = NULL;
  r->ku.multiple.array = NULL;
  r->values_buffer = NULL;

#ifndef SENORA_GC_NO_FREE
  if (r->list_stack)
    GC_free(r->list_stack);
#endif

  thread_is_dead(r);

  /* In case we kill a thread while in a bignum operation: */
  scheme_gmp_tls_restore_snapshot(r->gmp_tls, r->gmp_tls_data, 
                                  NULL, ((r == scheme_current_thread) ? 1 : 2));

  if (r == scheme_current_thread) {
    /* We're going to be swapped out immediately. */
    swap_no_setjmp = 1;
  } else
    RESETJMP(r);

  scheme_remove_managed(r->mref, (Scheme_Object *)r->mr_hop);
  for (l = r->extra_mrefs; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    scheme_remove_managed((Scheme_Custodian_Reference *)SCHEME_CAR(l), (Scheme_Object *)r->mr_hop);
  }
  r->extra_mrefs = scheme_null;
}

void scheme_end_current_thread(void)
{
  remove_thread(scheme_current_thread);
  
  thread_ended_with_activity = 1;
  
  if (scheme_notify_multithread && !scheme_first_thread->next) {
    scheme_notify_multithread(0);
    have_activity = 0;
  }
  
  select_thread();
}

static void start_child(Scheme_Thread * volatile child,
			Scheme_Object * volatile child_eval)
{
  if (SETJMP(child)) {
    /* Initial swap in: */
    Scheme_Object * volatile result = NULL;

    thread_swap_count++;
#ifdef RUNSTACK_IS_GLOBAL
    MZ_RUNSTACK = scheme_current_thread->runstack;
    MZ_RUNSTACK_START = scheme_current_thread->runstack_start;
    MZ_CONT_MARK_STACK = scheme_current_thread->cont_mark_stack;
    MZ_CONT_MARK_POS = scheme_current_thread->cont_mark_pos;
#endif
    scheme_gmp_tls_unload(scheme_current_thread->gmp_tls, scheme_current_thread->gmp_tls_data);
    scheme_current_thread->gmp_tls_data = NULL;
    {
      Scheme_Object *l, *o;
      Scheme_Closure_Func f;
      for (l = thread_swap_callbacks; SCHEME_RPAIRP(l); l = SCHEME_CDR(l)) {
	o = SCHEME_CAR(l);
	f = SCHEME_CLOS_FUNC(o);
	o = SCHEME_CLOS_DATA(o);
	f(o);
      }
    }

    scheme_current_thread->current_start_process_msec = process_time_at_swap;

    RESETJMP(child);

#if WATCH_FOR_NESTED_SWAPS
    swapping = 0;
#endif

    if (scheme_current_thread->running & MZTHREAD_KILLED) {
      /* This thread is dead! Give up now. */
      exit_or_escape(scheme_current_thread);
    }

    if (scheme_current_thread->return_marks_to) {
      stash_current_marks();
      do_swap_thread();
    }

    {
      mz_jmp_buf newbuf;
      scheme_current_thread->error_buf = &newbuf;
      if (!scheme_setjmp(newbuf)) {
	/* Run the main thunk: */
	/* (checks for break before doing anything else) */
	result = scheme_apply_thread_thunk(child_eval);
      }
    }

    /* !! At this point, scheme_current_thread can turn out to be a
       different thread, which invoked the original thread's
       continuation. */

    /* If we still have a meta continuation, then it means we
       should be resuming at a prompt, not exiting. */
    while (scheme_current_thread->meta_continuation) {
      Scheme_Thread *p = scheme_current_thread;
      Scheme_Overflow *oflow;

      p->cjs.val = result;

      if (!SAME_OBJ(p->meta_continuation->prompt_tag, scheme_default_prompt_tag)) {
        scheme_signal_error("thread ended with meta continuation that isn't for the default prompt");
      } else {
        Scheme_Meta_Continuation *mc;
        mc = p->meta_continuation;
        oflow = mc->overflow;
        p->meta_continuation = mc->next;
        if (!oflow->eot) {
          p->stack_start = oflow->stack_start;
          p->decompose_mc = mc;
          scheme_longjmpup(&oflow->jmp->cont);
        }
      }
    }

    scheme_end_current_thread();

    /* Shouldn't get here! */
    scheme_signal_error("bad thread switch");
  }
}

static Scheme_Object *make_subprocess(Scheme_Object *child_thunk,
				      void *child_start, 
				      Scheme_Config *config,
				      Scheme_Thread_Cell_Table *cells,
				      Scheme_Object *break_cell,
				      Scheme_Custodian *mgr,
				      int normal_kill)
{
  Scheme_Thread *child;
  int turn_on_multi;
  Scheme_Object *name_sym = NULL;
 
  turn_on_multi = !scheme_first_thread->next;
  
  if (!config)
    config = scheme_current_config();
  if (!cells)
    cells = scheme_inherit_cells(NULL);
  if (!break_cell) {
    break_cell = scheme_current_break_cell();
    if (SAME_OBJ(break_cell, maybe_recycle_cell))
      maybe_recycle_cell = NULL;
  }

  /* Use child_thunk name, if any, for the thread name.
     (Get it before calling make_thread(), in case
     getting the name blocks.) */
  {
    const char *s;
    int len;
    
    s = scheme_get_proc_name(child_thunk, &len, -1);
    if (s)  {
      if (len < 0)
	name_sym = (Scheme_Object *)s;
      else
	name_sym = scheme_intern_exact_symbol(s, len);
    }
  }

  child = make_thread(config, cells, break_cell, mgr, child_start);
  if (name_sym)
    child->name = name_sym;

  {
    Scheme_Object *v;
    v = scheme_thread_cell_get(break_cell, cells);
    child->can_break_at_swap = SCHEME_TRUEP(v);
  }

  if (!normal_kill)
    child->suspend_to_kill = 1;

  child->stack_start = child_start;

  /* Sets the child's jmpbuf for swapping in later: */
  start_child(child, child_thunk);

  if (scheme_notify_multithread && turn_on_multi) {
    scheme_notify_multithread(1);
    have_activity = 1;
  }

  SCHEME_USE_FUEL(1000);
  
  return (Scheme_Object *)child;
}

Scheme_Object *scheme_thread(Scheme_Object *thunk)
{
  return scheme_thread_w_details(thunk, NULL, NULL, NULL, NULL, 0);
}

static Scheme_Object *sch_thread(int argc, Scheme_Object *args[])
{
  scheme_check_proc_arity("thread", 0, 0, argc, args);
  scheme_custodian_check_available(NULL, "thread", "thread");

  return scheme_thread(args[0]);
}

static Scheme_Object *sch_thread_nokill(int argc, Scheme_Object *args[])
{
  scheme_check_proc_arity("thread/suspend-to-kill", 0, 0, argc, args);
  scheme_custodian_check_available(NULL, "thread/suspend-to-kill", "thread");

  return scheme_thread_w_details(args[0], NULL, NULL, NULL, NULL, 1);
}

static Scheme_Object *sch_current(int argc, Scheme_Object *args[])
{
  return (Scheme_Object *)scheme_current_thread;
}

static Scheme_Object *thread_p(int argc, Scheme_Object *args[])
{
  return SCHEME_THREADP(args[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *thread_running_p(int argc, Scheme_Object *args[])
{
  int running;

  if (!SCHEME_THREADP(args[0]))
    scheme_wrong_contract("thread-running?", "thread?", 0, argc, args);

  running = ((Scheme_Thread *)args[0])->running;

  return ((MZTHREAD_STILL_RUNNING(running) && !(running & MZTHREAD_USER_SUSPENDED))
	  ? scheme_true 
	  : scheme_false);
}

static Scheme_Object *thread_dead_p(int argc, Scheme_Object *args[])
{
  int running;

  if (!SCHEME_THREADP(args[0]))
    scheme_wrong_contract("thread-running?", "thread?", 0, argc, args);

  running = ((Scheme_Thread *)args[0])->running;

  return MZTHREAD_STILL_RUNNING(running) ? scheme_false : scheme_true;
}

static int thread_wait_done(Scheme_Object *p, Scheme_Schedule_Info *sinfo)
{
  int running = ((Scheme_Thread *)p)->running;
  if (MZTHREAD_STILL_RUNNING(running)) {
    /* Replace the direct thread reference with an event, so that
       the blocking thread can be dequeued: */
    Scheme_Object *evt;
    evt = scheme_get_thread_dead((Scheme_Thread *)p);
    scheme_set_sync_target(sinfo, evt, p, NULL, 0, 0, NULL);
    return 0;
  } else
    return 1;
}

static Scheme_Object *thread_wait(int argc, Scheme_Object *args[])
{
  Scheme_Thread *p;

  if (!SCHEME_THREADP(args[0]))
    scheme_wrong_contract("thread-wait", "thread?", 0, argc, args);

  p = (Scheme_Thread *)args[0];

  if (MZTHREAD_STILL_RUNNING(p->running)) {
    sch_sync(1, args);
  }

  return scheme_void;
}

void scheme_thread_wait(Scheme_Object *thread) {
  thread_wait(1, &thread);
}

static void register_thread_sync()
{
  scheme_add_evt(scheme_thread_type, 
                 (Scheme_Ready_Fun)thread_wait_done, 
                 NULL, NULL, 0);
}

void scheme_add_swap_callback(Scheme_Closure_Func f, Scheme_Object *data)
{
  Scheme_Object *p;

  p = scheme_make_raw_pair((Scheme_Object *)f, data);
  thread_swap_callbacks = scheme_make_raw_pair(p, thread_swap_callbacks);
}

void scheme_add_swap_out_callback(Scheme_Closure_Func f, Scheme_Object *data)
{
  Scheme_Object *p;

  p = scheme_make_raw_pair((Scheme_Object *)f, data);
  thread_swap_out_callbacks = scheme_make_pair(p, thread_swap_out_callbacks);
}

/**************************************************************************/
/* Ensure that a new thread has a reasonable starting stack */

#ifdef DO_STACK_CHECK
# define THREAD_STACK_SPACE (STACK_SAFETY_MARGIN / 2)

int scheme_is_stack_too_shallow()
{
# define SCHEME_PLUS_STACK_DELTA(x) ((x) - THREAD_STACK_SPACE)
# include "mzstkchk.h"
  {
    return 1;
  }
  return 0;
}

static Scheme_Object *thread_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *thunk, *result, *break_cell;
  Scheme_Config *config;
  Scheme_Custodian *mgr;
  Scheme_Thread_Cell_Table *cells;
  int suspend_to_kill = p->ku.k.i1;
  
  thunk = (Scheme_Object *)p->ku.k.p1;
  config = (Scheme_Config *)p->ku.k.p2;
  mgr = (Scheme_Custodian *)p->ku.k.p3;
  cells = (Scheme_Thread_Cell_Table *)SCHEME_CAR((Scheme_Object *)p->ku.k.p4);
  break_cell = SCHEME_CDR((Scheme_Object *)p->ku.k.p4);

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;
  
  result = make_subprocess(thunk, PROMPT_STACK(result),
			   config, cells, break_cell, mgr, !suspend_to_kill);

  /* Don't get rid of `result'; it keeps the
     Precise GC xformer from "optimizing" away
     the __gc_var_stack__ frame. */
  return result;
}

#endif /* DO_STACK_CHECK */

Scheme_Object *scheme_thread_w_details(Scheme_Object *thunk, 
				       Scheme_Config *config, 
				       Scheme_Thread_Cell_Table *cells,
				       Scheme_Object *break_cell,
				       Scheme_Custodian *mgr, 
				       int suspend_to_kill)
{
  Scheme_Object *result;
#ifndef MZ_PRECISE_GC
  void *stack_marker;
#endif

#ifdef DO_STACK_CHECK
  /* Make sure the thread starts out with a reasonable stack size, so
     it doesn't thrash right away: */
  if (scheme_is_stack_too_shallow()) {
    Scheme_Thread *p = scheme_current_thread;

    p->ku.k.p1 = thunk;
    p->ku.k.p2 = config;
    p->ku.k.p3 = mgr;
    result = scheme_make_pair((Scheme_Object *)cells, break_cell);
    p->ku.k.p4 = result;
    p->ku.k.i1 = suspend_to_kill;

    return scheme_handle_stack_overflow(thread_k);
  }
#endif

  result = make_subprocess(thunk, PROMPT_STACK(stack_marker),
			   config, cells, break_cell, mgr, !suspend_to_kill);

  /* Don't get rid of `result'; it keeps the
     Precise GC xformer from "optimizing" away
     the __gc_var_stack__ frame. */
  return result;
}

/**************************************************************************/
/* Nested threads */

static Scheme_Object *def_nested_exn_handler(int argc, Scheme_Object *argv[])
{
  if (scheme_current_thread->nester) {
    Scheme_Thread *p = scheme_current_thread;
    p->cjs.jumping_to_continuation = (Scheme_Object *)scheme_current_thread;
    p->cjs.alt_full_continuation = NULL;
    p->cjs.val = argv[0];
    p->cjs.is_kill = 0;
    p->cjs.skip_dws = 0;
    scheme_longjmp(*p->error_buf, 1);
  }

  return scheme_void; /* misuse of exception handler (wrong kind of thread or under prompt) */
}

MZ_DO_NOT_INLINE(Scheme_Object *scheme_call_as_nested_thread(int argc, Scheme_Object *argv[], void *max_bottom));

Scheme_Object *scheme_call_as_nested_thread(int argc, Scheme_Object *argv[], void *max_bottom)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Thread * volatile np;
  Scheme_Custodian *mgr;
  Scheme_Object * volatile v;
  mz_jmp_buf newbuf;
  volatile int failure;

  scheme_check_proc_arity("call-in-nested-thread", 0, 0, argc, argv);
  if (argc > 1) {
    if (SCHEME_CUSTODIANP(argv[1]))
      mgr = (Scheme_Custodian *)argv[1];
    else {
      scheme_wrong_contract("call-in-nested-thread", "custodian?", 1, argc, argv);
      return NULL;
    }
  } else
    mgr = (Scheme_Custodian *)scheme_get_param(scheme_current_config(), MZCONFIG_CUSTODIAN);

  scheme_custodian_check_available(mgr, "call-in-nested-thread", "thread");

  SCHEME_USE_FUEL(25);

  scheme_wait_until_suspend_ok();

  np = MALLOC_ONE_TAGGED(Scheme_Thread);
  np->so.type = scheme_thread_type;
#ifdef MZ_PRECISE_GC
  GC_register_new_thread(np, mgr);
#endif
  np->running = MZTHREAD_RUNNING;
  np->ran_some = 1;

#ifdef RUNSTACK_IS_GLOBAL
  p->runstack = MZ_RUNSTACK;
  p->runstack_start = MZ_RUNSTACK_START;
  p->cont_mark_stack = MZ_CONT_MARK_STACK;
  p->cont_mark_pos = MZ_CONT_MARK_POS;
#endif

  /* zero out anything we need now, because nestee disables
     GC cleaning for this thread: */
  scheme_prepare_this_thread_for_GC(p);

  if (!p->runstack_owner) {
    Scheme_Thread **owner;
    owner = MALLOC_N(Scheme_Thread *, 1);
    p->runstack_owner = owner;
    *owner = p;
  }

  np->runstack = p->runstack;
  np->runstack_start = p->runstack_start;
  np->runstack_size = p->runstack_size;
  np->runstack_saved = p->runstack_saved;
  np->runstack_owner = p->runstack_owner;
  *np->runstack_owner = np;
  np->stack_start = p->stack_start;
  np->engine_weight = p->engine_weight;
  {
    Scheme_Object **tb;
    tb = MALLOC_N(Scheme_Object *, p->tail_buffer_size);
    np->tail_buffer = tb;
  }
  np->tail_buffer_size = p->tail_buffer_size;

  np->list_stack = p->list_stack;
  np->list_stack_pos = p->list_stack_pos;

  scheme_gmp_tls_init(np->gmp_tls);

  /* np->prev = NULL; - 0ed by allocation */
  np->next = scheme_first_thread;
  scheme_first_thread->prev = np;
  scheme_first_thread = np;

  np->t_set_parent = p->t_set_parent;
  schedule_in_set((Scheme_Object *)np, np->t_set_parent);

  {
    Scheme_Thread_Cell_Table *cells;
    cells = scheme_inherit_cells(p->cell_values);
    np->cell_values = cells;
  }
  {
    Scheme_Config *config;
    config = scheme_current_config();
    np->init_config = config;
  }
  {
    int cb;
    Scheme_Object *bc;
    cb = scheme_can_break(p);
    p->can_break_at_swap = cb;
    bc = scheme_current_break_cell();
    np->init_break_cell = bc;
    if (SAME_OBJ(bc, maybe_recycle_cell))
      maybe_recycle_cell = NULL;
  }
  np->cont_mark_pos = (MZ_MARK_POS_TYPE)1;
  /* others 0ed already by allocation */

  check_ready_break();

  np->nester = p;
  p->nestee = np;
  np->external_break = p->external_break;
  p->external_break = 0;

  {
    Scheme_Thread_Custodian_Hop *hop;
    Scheme_Custodian_Reference *mref;
    hop = MALLOC_ONE_WEAK_RT(Scheme_Thread_Custodian_Hop);
    np->mr_hop = hop;
    hop->so.type = scheme_thread_hop_type;
    {
      Scheme_Thread *wp;
      wp = (Scheme_Thread *)WEAKIFY((Scheme_Object *)np);
      hop->p = wp;
    }
    mref = scheme_add_managed(mgr, (Scheme_Object *)hop, NULL, NULL, 0);
    np->mref = mref;
    np->extra_mrefs = scheme_null;
#ifndef MZ_PRECISE_GC
    scheme_weak_reference((void **)(void *)&hop->p);
#endif
  }

  np->gc_prep_chain = gc_prep_thread_chain;
  gc_prep_thread_chain = np;

#ifdef RUNSTACK_IS_GLOBAL
  MZ_CONT_MARK_STACK = np->cont_mark_stack;
  MZ_CONT_MARK_POS = np->cont_mark_pos;
#endif

  scheme_current_thread = np;

  if (p != scheme_main_thread)
    scheme_weak_suspend_thread(p);

  if (!the_nested_exn_handler) {
    REGISTER_SO(the_nested_exn_handler);
    the_nested_exn_handler = scheme_make_prim_w_arity(def_nested_exn_handler,
                                                      "nested-thread-exception-handler",
                                                      1, 1);
  }
  scheme_set_cont_mark(scheme_exn_handler_key, the_nested_exn_handler);

  /* Call thunk, catch escape: */
  np->error_buf = &newbuf;
  if (scheme_setjmp(newbuf)) {
    if (!np->cjs.is_kill)
      v = np->cjs.val;
    else
      v = NULL;
    failure = 1;
  } else {
    v = scheme_apply(argv[0], 0, NULL);
    failure = 0;
  }

  scheme_remove_managed(np->mref, (Scheme_Object *)np->mr_hop);
  {
    Scheme_Object *l;
    for (l = np->extra_mrefs; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
      scheme_remove_managed((Scheme_Custodian_Reference *)SCHEME_CAR(l), 
			    (Scheme_Object *)np->mr_hop);
    }
  }
  np->extra_mrefs = scheme_null;
#ifdef MZ_PRECISE_GC
  WEAKIFIED(np->mr_hop->p) = NULL;
#else
  scheme_unweak_reference((void **)(void *)&np->mr_hop->p);
#endif
  scheme_remove_all_finalization(np->mr_hop);

  if (np->prev)
    np->prev->next = np->next;
  else
    scheme_first_thread = np->next;
  np->next->prev = np->prev;

  np->next = NULL;
  np->prev = NULL;

  unschedule_in_set((Scheme_Object *)np, np->t_set_parent);

  if (np->cont_mark_stack_owner
      && ((*np->cont_mark_stack_owner) == np)) {
    *np->cont_mark_stack_owner = NULL;
  }

  np->running = 0;

  *p->runstack_owner = p;

  p->external_break = np->external_break;
  p->nestee = NULL;
  np->nester = NULL;

  thread_is_dead(np);

  scheme_current_thread = p;

  if (!p->gc_prep_chain) {
    p->gc_prep_chain = gc_prep_thread_chain;
    gc_prep_thread_chain = p;
  }

  if (p != scheme_main_thread)
    scheme_weak_resume_thread(p);

#ifdef RUNSTACK_IS_GLOBAL
  MZ_CONT_MARK_STACK = p->cont_mark_stack;
  MZ_CONT_MARK_POS = p->cont_mark_pos;
#endif

  if ((p->running & MZTHREAD_KILLED)
      || (p->running & MZTHREAD_USER_SUSPENDED))
    scheme_thread_block(0.0);

  if (failure) {
    if (!v)
      scheme_raise_exn(MZEXN_FAIL, 
		       "call-in-nested-thread: the thread was killed, or it exited via the default error escape handler");
    else
      scheme_raise(v);
  }

  /* May have just moved a break to a breakable thread: */
  /* Check for external break again after swap or sleep */
  scheme_check_break_now();

  return v;
}

static Scheme_Object *call_as_nested_thread(int argc, Scheme_Object *argv[])
{
  Scheme_Object *result;
  result = scheme_call_as_nested_thread(argc, argv, PROMPT_STACK(result));
  return result;
}

/*========================================================================*/
/*                     thread scheduling and termination                  */
/*========================================================================*/

void scheme_init_kqueue(void)
{
#if defined(HAVE_KQUEUE_SYSCALL) || defined(HAVE_EPOLL_SYSCALL)
  scheme_semaphore_fd_kqueue = -1;
#endif
}

void scheme_release_kqueue(void)
{
#if defined(HAVE_KQUEUE_SYSCALL) || defined(HAVE_EPOLL_SYSCALL)
  if (scheme_semaphore_fd_kqueue >= 0) {
    intptr_t rc;
    do {
      rc = close(scheme_semaphore_fd_kqueue);
    } while ((rc == -1) && (errno == EINTR));
  }
#endif
}

#if defined(HAVE_KQUEUE_SYSCALL) || defined(HAVE_EPOLL_SYSCALL)
static void log_kqueue_error(const char *action, int kr)
{
  if (kr < 0) {
    Scheme_Logger *logger;
    logger = scheme_get_main_logger();
    scheme_log(logger, SCHEME_LOG_WARNING, 0, 
#ifdef HAVE_KQUEUE_SYSCALL
	       "kqueue"
#else
	       "epoll"
#endif
	       " error at %s: %E", 
	       action, errno);
  }
}

static void log_kqueue_fd(int fd, int flags)
{
  Scheme_Logger *logger;
  logger = scheme_get_main_logger();
  scheme_log(logger, SCHEME_LOG_WARNING, 0, 
#ifdef HAVE_KQUEUE_SYSCALL
             "kqueue"
#else
             "epoll"
#endif
             " expected event %d %d", 
             fd, flags);
}
#endif

Scheme_Object *scheme_fd_to_semaphore(intptr_t fd, int mode, int is_socket)
{
#ifdef USE_WINSOCK_TCP
  return NULL;
#else
  Scheme_Object *key, *v, *s = NULL;
#if defined(HAVE_KQUEUE_SYSCALL) || defined(HAVE_EPOLL_SYSCALL)
# else
  void *r, *w, *e;
# endif

  if (!scheme_semaphore_fd_mapping)
    return NULL;

# ifdef HAVE_KQUEUE_SYSCALL
  if (!is_socket) {
    if (!scheme_fd_regular_file(fd, 2))
      return NULL; /* kqueue() might not work on devices, such as ttys */
  }
  if (scheme_semaphore_fd_kqueue < 0) {
    scheme_semaphore_fd_kqueue = kqueue();
    if (scheme_semaphore_fd_kqueue < 0) {
      log_kqueue_error("create", scheme_semaphore_fd_kqueue);
      return NULL;
    }
  }
# endif
# ifdef HAVE_EPOLL_SYSCALL
  if (scheme_semaphore_fd_kqueue < 0) {
    scheme_semaphore_fd_kqueue = epoll_create(5);
    if (scheme_semaphore_fd_kqueue < 0) {
      log_kqueue_error("create", scheme_semaphore_fd_kqueue);
      return NULL;
    }
  }
# endif

  key = scheme_make_integer_value(fd);
  v = scheme_hash_get(scheme_semaphore_fd_mapping, key);
  if (!v && ((mode == MZFD_CHECK_READ)
             || (mode == MZFD_CHECK_WRITE)
             || (mode == MZFD_CHECK_VNODE)
             || (mode == MZFD_REMOVE)
             || (mode == MZFD_REMOVE_VNODE)))
    return NULL;

  if (!v) {
    v = scheme_make_vector(2, scheme_false);
    scheme_hash_set(scheme_semaphore_fd_mapping, key, v);
  }

# if !defined(HAVE_KQUEUE_SYSCALL) && !defined(HAVE_EPOLL_SYSCALL)
  r = MZ_GET_FDSET(scheme_semaphore_fd_set, 0);
  w = MZ_GET_FDSET(scheme_semaphore_fd_set, 1);
  e = MZ_GET_FDSET(scheme_semaphore_fd_set, 2);
# endif

  if ((mode == MZFD_REMOVE) || (mode == MZFD_REMOVE_VNODE)) {
    s = SCHEME_VEC_ELS(v)[0];
    if (!SCHEME_FALSEP(s))
      scheme_post_sema_all(s);
    s = SCHEME_VEC_ELS(v)[1];
    if (!SCHEME_FALSEP(s))
      scheme_post_sema_all(s);
    s = NULL;
    scheme_hash_set(scheme_semaphore_fd_mapping, key, NULL);
# ifdef HAVE_KQUEUE_SYSCALL
    {
      GC_CAN_IGNORE struct kevent kev[2];
      struct timespec timeout = {0, 0};
      int kr, pos = 0;
      if (mode == MZFD_REMOVE_VNODE) {
        EV_SET(&kev[pos], fd, EVFILT_VNODE, EV_DELETE, 0, 0, NULL);
        pos++;
      } else {
        if (SCHEME_TRUEP(SCHEME_VEC_ELS(v)[0])) {
          EV_SET(&kev[pos], fd, EVFILT_READ, EV_DELETE, 0, 0, NULL);
          pos++;
        }
        if (SCHEME_TRUEP(SCHEME_VEC_ELS(v)[1])) {
          EV_SET(&kev[pos], fd, EVFILT_WRITE, EV_DELETE, 0, 0, NULL);
          pos++;
        }
      }
      do {
        kr = kevent(scheme_semaphore_fd_kqueue, kev, pos, NULL, 0, &timeout);
      } while ((kr == -1) && (errno == EINTR));
      log_kqueue_error("remove", kr);
    }
# elif defined(HAVE_EPOLL_SYSCALL)
    {
      int kr;
      kr = epoll_ctl(scheme_semaphore_fd_kqueue, EPOLL_CTL_DEL, fd, NULL);
      log_kqueue_error("remove", kr);
    }
# else
    MZ_FD_CLR(fd, r);
    MZ_FD_CLR(fd, w);
    MZ_FD_CLR(fd, e);
# endif
    s = NULL;
  } else if ((mode == MZFD_CHECK_READ)
             || (mode == MZFD_CREATE_READ)
             || (mode == MZFD_CHECK_VNODE)
             || (mode == MZFD_CREATE_VNODE)) {
    s = SCHEME_VEC_ELS(v)[0];
    if (SCHEME_FALSEP(s)) {
      if ((mode == MZFD_CREATE_READ)
          || (mode == MZFD_CREATE_VNODE)) {
        s = scheme_make_sema(0);
        SCHEME_VEC_ELS(v)[0] = s;
# ifdef HAVE_KQUEUE_SYSCALL
        {
          GC_CAN_IGNORE struct kevent kev;
          struct timespec timeout = {0, 0};
          int kr;
          if (mode == MZFD_CREATE_READ)
            EV_SET(&kev, fd, EVFILT_READ, EV_ADD | EV_ONESHOT, 0, 0, NULL);
          else
            EV_SET(&kev, fd, EVFILT_VNODE, EV_ADD | EV_ONESHOT, 
                   (NOTE_DELETE | NOTE_WRITE | NOTE_EXTEND 
                    | NOTE_RENAME | NOTE_ATTRIB),
                   0, NULL);
          do {
            kr = kevent(scheme_semaphore_fd_kqueue, &kev, 1, NULL, 0, &timeout);
          } while ((kr == -1) && (errno == EINTR));
	  log_kqueue_error("read", kr);
        }
# elif defined(HAVE_EPOLL_SYSCALL)
	{
	  GC_CAN_IGNORE struct epoll_event ev;
	  int already = !SCHEME_FALSEP(SCHEME_VEC_ELS(v)[1]), kr;
	  ev.data.fd = fd;
	  ev.events = EPOLLIN | (already ? EPOLLOUT : 0);
	  kr = epoll_ctl(scheme_semaphore_fd_kqueue, 
			 (already ? EPOLL_CTL_MOD : EPOLL_CTL_ADD), fd, &ev);
	  log_kqueue_error("read", kr);
	}
# else
        MZ_FD_SET(fd, r);
        MZ_FD_SET(fd, e);
#endif
      } else
        s = NULL;
    }
  } else if ((mode == MZFD_CHECK_WRITE)
             || (mode == MZFD_CREATE_WRITE)) {
    s = SCHEME_VEC_ELS(v)[1];
    if (SCHEME_FALSEP(s)) {
      if (mode == MZFD_CREATE_WRITE) {
        s = scheme_make_sema(0);
        SCHEME_VEC_ELS(v)[1] = s;
# ifdef HAVE_KQUEUE_SYSCALL
        {
          GC_CAN_IGNORE struct kevent kev;
          struct timespec timeout = {0, 0};
          int kr;
          EV_SET(&kev, fd, EVFILT_WRITE, EV_ADD | EV_ONESHOT, 0, 0, NULL);
          do {
            kr = kevent(scheme_semaphore_fd_kqueue, &kev, 1, NULL, 0, &timeout);
          } while ((kr == -1) && (errno == EINTR));
	  log_kqueue_error("write", kr);
        }

# elif defined(HAVE_EPOLL_SYSCALL)
	{
	  GC_CAN_IGNORE struct epoll_event ev;
	  int already = !SCHEME_FALSEP(SCHEME_VEC_ELS(v)[0]), kr;
	  ev.data.fd = fd;
	  ev.events = EPOLLOUT | (already ? EPOLLIN : 0);
	  kr = epoll_ctl(scheme_semaphore_fd_kqueue, 
			 (already ? EPOLL_CTL_MOD : EPOLL_CTL_ADD), fd, &ev);
	  log_kqueue_error("write", kr);
	}
# else
        MZ_FD_SET(fd, w);
        MZ_FD_SET(fd, e);
#endif
      } else
        s = NULL;
    }
  }

  return s;
#endif
}

static int check_fd_semaphores()
{
#ifdef USE_WINSOCK_TCP
  return 0;
#elif defined(HAVE_KQUEUE_SYSCALL)
  Scheme_Object *v, *s, *key;
  GC_CAN_IGNORE struct kevent kev;
  struct timespec timeout = {0, 0};
  int kr, hit = 0;

  if (!scheme_semaphore_fd_mapping || (scheme_semaphore_fd_kqueue < 0))
    return 0;

  while (1) {
    do {
      kr = kevent(scheme_semaphore_fd_kqueue, NULL, 0, &kev, 1, &timeout);
    } while ((kr == -1) && (errno == EINTR));
    log_kqueue_error("wait", kr);
 
    if (kr > 0) {
      key = scheme_make_integer_value(kev.ident);
      v = scheme_hash_get(scheme_semaphore_fd_mapping, key);
      if (v) {
        if ((kev.filter == EVFILT_READ) || (kev.filter == EVFILT_VNODE)) {
          s = SCHEME_VEC_ELS(v)[0];
          if (!SCHEME_FALSEP(s)) {
            scheme_post_sema_all(s);
            hit = 1;
            SCHEME_VEC_ELS(v)[0] = scheme_false;
          }
        } else if (kev.filter == EVFILT_WRITE) {
          s = SCHEME_VEC_ELS(v)[1];
          if (!SCHEME_FALSEP(s)) {
            scheme_post_sema_all(s);
            hit = 1;
            SCHEME_VEC_ELS(v)[1] = scheme_false;
          }
        }
        if (SCHEME_FALSEP(SCHEME_VEC_ELS(v)[0])
            && SCHEME_FALSEP(SCHEME_VEC_ELS(v)[1]))
          scheme_hash_set(scheme_semaphore_fd_mapping, key, NULL);
      } else {
        log_kqueue_fd(kev.ident, kev.filter);
      }
    } else
      break;
  }

  return hit;
#elif defined(HAVE_EPOLL_SYSCALL)
  Scheme_Object *v, *s, *key;
  int kr, hit = 0;
  GC_CAN_IGNORE struct epoll_event ev;

  if (!scheme_semaphore_fd_mapping || (scheme_semaphore_fd_kqueue < 0))
    return 0;

  while (1) {
    
    do {
      kr = epoll_wait(scheme_semaphore_fd_kqueue, &ev, 1, 0);
    } while ((kr == -1) && (errno == EINTR));
    log_kqueue_error("wait", kr);
 
    if (kr > 0) {
      key = scheme_make_integer_value(ev.data.fd);
      v = scheme_hash_get(scheme_semaphore_fd_mapping, key);
      if (v) {
	if (ev.events & (POLLIN | POLLHUP | POLLERR)) {
	  s = SCHEME_VEC_ELS(v)[0];
          if (!SCHEME_FALSEP(s)) {
            scheme_post_sema_all(s);
            hit = 1;
            SCHEME_VEC_ELS(v)[0] = scheme_false;
          }
	}
	if (ev.events & (POLLOUT | POLLHUP | POLLERR)) {
	  s = SCHEME_VEC_ELS(v)[1];
          if (!SCHEME_FALSEP(s)) {
            scheme_post_sema_all(s);
            hit = 1;
            SCHEME_VEC_ELS(v)[1] = scheme_false;
          }
	}
        if (SCHEME_FALSEP(SCHEME_VEC_ELS(v)[0])
            && SCHEME_FALSEP(SCHEME_VEC_ELS(v)[1])) {
          scheme_hash_set(scheme_semaphore_fd_mapping, key, NULL);
	  kr = epoll_ctl(scheme_semaphore_fd_kqueue, EPOLL_CTL_DEL, ev.data.fd, NULL);
	  log_kqueue_error("remove*", kr);
	} else {
	  ev.events = ((SCHEME_FALSEP(SCHEME_VEC_ELS(v)[0]) ? 0 : POLLIN)
		       | (SCHEME_FALSEP(SCHEME_VEC_ELS(v)[1]) ? 0 : POLLOUT));
	  kr = epoll_ctl(scheme_semaphore_fd_kqueue, EPOLL_CTL_MOD, ev.data.fd, &ev);
	  log_kqueue_error("update", kr);
	}
      } else {
        log_kqueue_fd(ev.data.fd, ev.events);
      }
    } else
      break;
  }

  return hit;
#elif defined(HAVE_POLL_SYSCALL)
  struct pollfd *pfd;
  intptr_t i, c;
  Scheme_Object *v, *s, *key;
  int sr, hit = 0;

  if (!scheme_semaphore_fd_mapping || !scheme_semaphore_fd_mapping->count)
    return 0;

  scheme_clean_fd_set(scheme_semaphore_fd_set);
  c = SCHEME_INT_VAL(scheme_semaphore_fd_set->data->count);
  pfd = scheme_semaphore_fd_set->data->pfd;

  do {
    sr = poll(pfd, c, 0);
  } while ((sr == -1) && (errno == EINTR));  

  if (sr > 0) {
    for (i = 0; i < c; i++) {
      if (pfd[i].revents) {
        key = scheme_make_integer_value(pfd[i].fd);
        v = scheme_hash_get(scheme_semaphore_fd_mapping, key);
        if (v) {
          if (pfd[i].revents & (POLLIN | POLLHUP | POLLERR)) {
            s = SCHEME_VEC_ELS(v)[0];
            if (!SCHEME_FALSEP(s)) {
              scheme_post_sema_all(s);
              hit = 1;
              SCHEME_VEC_ELS(v)[0] = scheme_false;
            }
            pfd[i].revents -= (pfd[i].revents & POLLIN);
          }
          if (pfd[i].revents & (POLLOUT | POLLHUP | POLLERR)) {
            s = SCHEME_VEC_ELS(v)[1];
            if (!SCHEME_FALSEP(s)) {
              scheme_post_sema_all(s);
              hit = 1;
              SCHEME_VEC_ELS(v)[1] = scheme_false;
            }
            pfd[i].revents -= (pfd[i].revents & POLLOUT);
          }
          if (SCHEME_FALSEP(SCHEME_VEC_ELS(v)[0])
              && SCHEME_FALSEP(SCHEME_VEC_ELS(v)[1]))
            scheme_hash_set(scheme_semaphore_fd_mapping, key, NULL);
        }
      }
    }
  }

  return hit;
#else
  void *fds;
  struct timeval time = {0, 0};
  int i, actual_limit, r, w, e, sr, hit = 0;
  Scheme_Object *key, *v, *s;
  DECL_FDSET(set, 3);
  fd_set *set1, *set2;

  if (!scheme_semaphore_fd_mapping || !scheme_semaphore_fd_mapping->count)
    return 0;

  INIT_DECL_FDSET(set, set1, set2);
  set1 = (fd_set *) MZ_GET_FDSET(set, 1);
  set2 = (fd_set *) MZ_GET_FDSET(set, 2);
  
  fds = (void *)set;
  MZ_FD_ZERO(set);
  MZ_FD_ZERO(set1);
  MZ_FD_ZERO(set2);

  scheme_merge_fd_sets(fds, scheme_semaphore_fd_set);

  actual_limit = scheme_get_fd_limit(fds);

  do {
    sr = select(actual_limit, set, set1, set2, &time);
  } while ((sr == -1) && (errno == EINTR));

  if (sr > 0) {
    for (i = 0; i < actual_limit; i++) {
      r = MZ_FD_ISSET(i, set);
      w = MZ_FD_ISSET(i, set1);
      e = MZ_FD_ISSET(i, set2);
      if (r || w || e) {
        key = scheme_make_integer_value(i);
        v = scheme_hash_get(scheme_semaphore_fd_mapping, key);
        if (v) {
          if (r || e) {
            s = SCHEME_VEC_ELS(v)[0];
            if (!SCHEME_FALSEP(s)) {
              scheme_post_sema_all(s);
              hit = 1;
              SCHEME_VEC_ELS(v)[0] = scheme_false;
            }
            MZ_FD_CLR(i, MZ_GET_FDSET(scheme_semaphore_fd_set, 0));
          }
          if (w || e) {
            s = SCHEME_VEC_ELS(v)[1];
            if (!SCHEME_FALSEP(s)) {
              scheme_post_sema_all(s);
              hit = 1;
              SCHEME_VEC_ELS(v)[1] = scheme_false;
            }
            MZ_FD_CLR(i, MZ_GET_FDSET(scheme_semaphore_fd_set, 1));
          }
          if (SCHEME_FALSEP(SCHEME_VEC_ELS(v)[0])
              && SCHEME_FALSEP(SCHEME_VEC_ELS(v)[1])) {
            MZ_FD_CLR(i, MZ_GET_FDSET(scheme_semaphore_fd_set, 2));
            scheme_hash_set(scheme_semaphore_fd_mapping, key, NULL);
          }
        }
      }
    }
  }

  return hit;
#endif
}

void scheme_check_fd_semaphores(void)
{
  (void)check_fd_semaphores();
}

typedef struct {
  int running;
  double sleep_end;
  int block_descriptor;
  Scheme_Object *blocker;
  Scheme_Ready_Fun block_check;
  Scheme_Needs_Wakeup_Fun block_needs_wakeup;
  Scheme_Kill_Action_Func private_on_kill;
  void *private_kill_data;
  void **private_kill_next;
} Thread_Schedule_State_Record;

static void save_thread_schedule_state(Scheme_Thread *p,
				       Thread_Schedule_State_Record *s,
				       int save_kills)
{
  s->running = p->running;
  s->sleep_end = p->sleep_end;
  s->block_descriptor = p->block_descriptor;
  s->blocker = p->blocker;
  s->block_check = p->block_check;
  s->block_needs_wakeup = p->block_needs_wakeup;

  if (save_kills) {
    s->private_on_kill = p->private_on_kill;
    s->private_kill_data = p->private_kill_data;
    s->private_kill_next = p->private_kill_next;
  }

  p->running = MZTHREAD_RUNNING;
  p->sleep_end = 0.0;
  p->block_descriptor = 0;
  p->blocker = NULL;
  p->block_check = NULL;
  p->block_needs_wakeup = NULL;
}

static void restore_thread_schedule_state(Scheme_Thread *p,
					  Thread_Schedule_State_Record *s,
					  int save_kills)
{
  p->running = s->running;
  p->sleep_end = s->sleep_end;
  p->block_descriptor = s->block_descriptor;
  p->blocker = s->blocker;
  p->block_check = s->block_check;
  p->block_needs_wakeup = s->block_needs_wakeup;

  if (save_kills) {
    p->private_on_kill = s->private_on_kill;
    p->private_kill_data = s->private_kill_data;
    p->private_kill_next = s->private_kill_next;
  }
}

static int check_sleep(int need_activity, int sleep_now)
/* Signals should be suspended */
{
  Scheme_Thread *p, *p2;
  int end_with_act;
#if defined(USING_FDS)
  DECL_FDSET(set, 3);
  fd_set *set1, *set2;
#endif
  void *fds;

  if (scheme_no_stack_overflow)
    return 0;
  
  /* Is everything blocked? */
  if (!do_atomic) {
    p = scheme_first_thread;
    while (p) {
      if (!p->nestee
          && (p->ran_some || p->block_descriptor == NOT_BLOCKED)
          && (p->next || !(p->running & MZTHREAD_USER_SUSPENDED)))
	break;
      p = p->next;
    }
  } else
    p = NULL;
  
  p2 = scheme_first_thread;
  while (p2) {
    if (p2->ran_some) {
      scheme_notify_sleep_progress();
      p2->ran_some = 0;
    }
    p2 = p2->next;
  }
  
  end_with_act = thread_ended_with_activity;
  thread_ended_with_activity = 0;
  
  if (need_activity 
      && !end_with_act 
      && (do_atomic 
	  || (!p && ((!sleep_now && scheme_wakeup_on_input)
		     || (sleep_now && (scheme_sleep || scheme_place_sleep)))))) {
    double max_sleep_time = 0;

    /* Poll from top-level process, and all subprocesses are blocked. */
    /* So, everything is blocked pending external input. */
    /* Build a list of file descriptors that we're waiting on */
    /* and turn off polling. */
    if (have_activity)
      scheme_active_but_sleeping = 1;
    if (have_activity && scheme_notify_multithread)
      scheme_notify_multithread(0);
    
#if defined(USING_FDS)
    INIT_DECL_FDSET(set, set1, set2);
    set1 = (fd_set *) MZ_GET_FDSET(set, 1);
    set2 = (fd_set *) MZ_GET_FDSET(set, 2);

    fds = (void *)set;
    MZ_FD_ZERO(set);
    MZ_FD_ZERO(set1);
    MZ_FD_ZERO(set2);
#else
    fds = NULL;
#endif
    
    needs_sleep_cancelled = 0;

    p = scheme_first_thread;
    while (p) {
      int merge_time = 0;
      double p_time;

      if (p->nestee) {
	/* nothing */
      } else if (p->block_descriptor == GENERIC_BLOCKED) {
        needs_sleep_time_end = -1.0;
	if (p->block_needs_wakeup) {
	  Scheme_Needs_Wakeup_Fun f = p->block_needs_wakeup;
	  Scheme_Object *blocker = p->blocker;
	  Thread_Schedule_State_Record ssr;
	  save_thread_schedule_state(scheme_current_thread, &ssr, 0);
	  f(blocker, fds);
	  restore_thread_schedule_state(scheme_current_thread, &ssr, 0);
	}
        p_time = p->sleep_end;
	merge_time = (p_time > 0.0);
        if (needs_sleep_time_end > 0.0) {
          if (!merge_time || (needs_sleep_time_end < p_time)) {
            p_time = needs_sleep_time_end;
            merge_time = 1;
          }
        }
      } else if (p->block_descriptor == SLEEP_BLOCKED) {
	merge_time = 1;
        p_time = p->sleep_end;
      }

      if (merge_time) {
	double d;
	double t;

	d = (p_time - scheme_get_inexact_milliseconds());

	t = (d / 1000);
	if (t <= 0) {
	  t = (float)0.00001;
	  needs_sleep_cancelled = 1;
	}
	if (!max_sleep_time || (t < max_sleep_time))
	  max_sleep_time = t;
      } 
      p = p->next;
    }
  
    if (needs_sleep_cancelled)
      return 0;

    if (post_system_idle()) {
      return 0;
    }

# if defined(HAVE_KQUEUE_SYSCALL) || defined(HAVE_EPOLL_SYSCALL) 
    if (scheme_semaphore_fd_mapping && (scheme_semaphore_fd_kqueue >= 0)) {
      MZ_FD_SET(scheme_semaphore_fd_kqueue, set);
      MZ_FD_SET(scheme_semaphore_fd_kqueue, set2);
    }
# else
    fds = scheme_merge_fd_sets(fds, scheme_semaphore_fd_set); 
# endif

    scheme_clean_fd_set(fds);
 
    if (sleep_now) {
      float mst = (float)max_sleep_time;

      /* Make sure that mst didn't go to infinity: */
      if (mst && !((double)mst < (2 * max_sleep_time))) {
        mst = 1000000.0;
      }

      {
        Scheme_Sleep_Proc slp;
        if (scheme_place_sleep)
          slp = scheme_place_sleep;
        else
          slp = scheme_sleep;

        slp(mst, fds);
      }
    } else if (scheme_wakeup_on_input)
      scheme_wakeup_on_input(fds);

    return 1;
  }

  return 0;
}

void scheme_set_wakeup_time(void *fds, double end_time)
{
  /* should be called only during a needs_wakeup callback */
  needs_sleep_time_end = end_time;
}

void scheme_set_place_sleep(Scheme_Sleep_Proc slp)
{
  scheme_place_sleep = slp;
}

static int post_system_idle()
{
  return scheme_try_channel_get(scheme_system_idle_channel);
}

void scheme_cancel_sleep()
{
  needs_sleep_cancelled = 1;
}

void scheme_check_threads(void)
{
  double start, now;

  start = scheme_get_inexact_milliseconds();
  
  while (1) {
    scheme_current_thread->suspend_break++;
    scheme_thread_block((float)0);
    --scheme_current_thread->suspend_break;
    
    if (check_sleep(have_activity, 0))
      break;

    now = scheme_get_inexact_milliseconds();
    if (((now - start) * 1000) > MZ_THREAD_QUANTUM_USEC)
      break;
  }
}

void scheme_wake_up(void)
{
  scheme_active_but_sleeping = 0;
  if (have_activity && scheme_notify_multithread)
    scheme_notify_multithread(1);
}

void scheme_out_of_fuel(void)
{
  if (scheme_defining_primitives) return;

  scheme_thread_block((float)0);
  scheme_current_thread->ran_some = 1;
}

static void init_schedule_info(Scheme_Schedule_Info *sinfo, Scheme_Thread *false_pos_ok, 
			       int no_redirect, double sleep_end)
{
  sinfo->false_positive_ok = false_pos_ok;
  sinfo->potentially_false_positive = 0;
  sinfo->current_syncing = NULL;
  sinfo->spin = 0;
  sinfo->is_poll = 0;
  sinfo->no_redirect = no_redirect;
  sinfo->sleep_end = sleep_end;
}

Scheme_Object *scheme_current_break_cell()
{
  return scheme_extract_one_cc_mark(NULL, scheme_break_enabled_key);
}

static int can_break_param(Scheme_Thread *p)
{
  if (p == scheme_current_thread) {
    Scheme_Object *v;
    
    v = scheme_extract_one_cc_mark(NULL, scheme_break_enabled_key);
    
    v = scheme_thread_cell_get(v, p->cell_values);
    
    return SCHEME_TRUEP(v);
  } else
    return p->can_break_at_swap;
}

int scheme_can_break(Scheme_Thread *p)
{
  if (!p->suspend_break && !all_breaks_disabled && !scheme_no_stack_overflow) {
    return can_break_param(p);
  } else
    return 0;
}

void scheme_set_can_break(int on)
{
  Scheme_Object *v;
      
  v = scheme_extract_one_cc_mark(NULL, scheme_break_enabled_key);

  scheme_thread_cell_set(v, scheme_current_thread->cell_values, 
			 (on ? scheme_true : scheme_false));

  if (SAME_OBJ(v, maybe_recycle_cell))
    maybe_recycle_cell = NULL;
}

void scheme_check_break_now(void)
{
  Scheme_Thread *p = scheme_current_thread;

  check_ready_break();

  if (p->external_break && scheme_can_break(p)) {
    scheme_thread_block_w_thread(0.0, p);
    p->ran_some = 1;
  }
}

static Scheme_Object *check_break_now(int argc, Scheme_Object *args[])
{
  scheme_check_break_now();
  return scheme_void;
}

void scheme_push_break_enable(Scheme_Cont_Frame_Data *cframe, int on, int post_check)
{
  Scheme_Object *v = NULL;

  if (recycle_cell) {
    if (!SCHEME_TRUEP(((Thread_Cell *)recycle_cell)->def_val) == !on) {
      v = recycle_cell;
      recycle_cell = NULL;
    }
  }

  if (!v)
    v = scheme_make_thread_cell(on ? scheme_true : scheme_false, 1);
  scheme_push_continuation_frame(cframe);
  scheme_set_cont_mark(scheme_break_enabled_key, v);
  if (post_check)
    scheme_check_break_now();    

  cframe->cache = v;
  maybe_recycle_cell = v;
  recycle_cc_count = scheme_cont_capture_count;
}

void scheme_pop_break_enable(Scheme_Cont_Frame_Data *cframe, int post_check)
{
  scheme_pop_continuation_frame(cframe);
  if (post_check)
    scheme_check_break_now();    

  if (cframe->cache == maybe_recycle_cell) {
    if (recycle_cc_count == scheme_cont_capture_count) {
      recycle_cell = maybe_recycle_cell;
    }
    maybe_recycle_cell = NULL;
  }
}

static Scheme_Object *raise_user_break(void *data, int argc, Scheme_Object ** volatile argv)
{
  /* The main action here is buried in code to free temporary bignum
     space on escapes. Aside from a thread kill, this is the only
     place where we have to worry about freeing bignum space, because
     kill and escape are the only possible actions within a bignum
     calculaion. It is possible to have nested bignum calculations,
     though (if the break handler performs bignum arithmetic), so
     that's why we save and restore an old snapshot. */
  mz_jmp_buf *savebuf, newbuf;
  intptr_t save[4];
  int kind;

  kind = SCHEME_INT_VAL((Scheme_Object *)data);

  savebuf = scheme_current_thread->error_buf;
  scheme_current_thread->error_buf = &newbuf;
  scheme_gmp_tls_snapshot(scheme_current_thread->gmp_tls, save);

  if (!scheme_setjmp(newbuf)) {
    /* >>>> This is the main action <<<< */
    scheme_raise_exn(kind, argv[0], ((kind == MZEXN_BREAK_TERMINATE)
                                     ? "terminate break"
                                     : ((kind == MZEXN_BREAK_HANG_UP)
                                        ? "hang-up break"
                                        : "user break")));
    /* will definitely escape (or thread will die) */
  } else {
    /* As expected, we're escaping. Unless we're continuing, then
       reset temporary bignum memory. */
    int cont;
    cont = SAME_OBJ((Scheme_Object *)scheme_jumping_to_continuation,
		    argv[0]);
    scheme_gmp_tls_restore_snapshot(scheme_current_thread->gmp_tls, NULL, save, !cont);
    scheme_longjmp(*savebuf, 1);
  }

  /* Can't get here */
  return NULL;
}

static void raise_break(Scheme_Thread *p)
{
  Thread_Schedule_State_Record ssr;
  Scheme_Object *a[1];
  Scheme_Cont_Frame_Data cframe;
  int kind;

  kind = p->external_break;
  p->external_break = 0;

  if (p->blocker && (p->block_check == (Scheme_Ready_Fun)syncing_ready)) {
    /* Get out of lines for channels, etc., before calling a break exn handler. */
    scheme_post_syncing_nacks((Syncing *)p->blocker);
  }

  save_thread_schedule_state(p, &ssr, 0);
  p->ran_some = 1;
  
  a[0] = scheme_make_closed_prim((Scheme_Closed_Prim *)raise_user_break, scheme_make_integer(kind));

  /* Continuation frame ensures that this doesn't
     look like it's in tail position with respect to
     an existing escape continuation */
  scheme_push_continuation_frame(&cframe);

  scheme_call_ec(1, a);

  scheme_pop_continuation_frame(&cframe);

  /* Continue from break... */
  restore_thread_schedule_state(p, &ssr, 0);
}

static void escape_to_kill(Scheme_Thread *p)
{
  p->cjs.jumping_to_continuation = (Scheme_Object *)p;
  p->cjs.alt_full_continuation = NULL;
  p->cjs.is_kill = 1;
  p->cjs.skip_dws = 1;
  scheme_longjmp(*p->error_buf, 1);
}

static void exit_or_escape(Scheme_Thread *p)
{
  /* Maybe this killed thread is nested: */
  if (p->nester) {
    if (p->running & MZTHREAD_KILLED)
      p->running -= MZTHREAD_KILLED;
    escape_to_kill(p);
  }

  if (SAME_OBJ(p, scheme_main_thread)) {
    /* Hard exit: */
    if (scheme_current_place_id)
      escape_to_kill(p);

    if (scheme_exit)
      scheme_exit(0);
    
    /* We really have to exit: */
    exit(0);
  }

  remove_thread(p);
  select_thread();
}

void scheme_break_kind_main_thread_at(void *p, int kind)
/* This function can be called from an interrupt handler. 
   On some platforms, it will even be called from multiple
   OS threads. In the case of multiple threads, there's a
   tiny chance that a single Ctl-C will trigger multiple
   break exceptions. */
  XFORM_SKIP_PROC
{
  if (kind > *(volatile short *)p)
    *(volatile short *)p = kind;
}

void scheme_break_main_thread_at(void *p)
  XFORM_SKIP_PROC
{
  scheme_break_kind_main_thread_at(p, MZEXN_BREAK);
}

void scheme_break_main_thread()
/* Calling this function from an arbitary
   thread is dangerous when therad locals are enabled. */
{
  scheme_break_main_thread_at((void *)&delayed_break_ready);
}

void *scheme_get_main_thread_break_handle()
{
  return (void *)&delayed_break_ready;
}

void scheme_set_break_main_target(Scheme_Thread *p)
{
  if (!main_break_target_thread) {
    REGISTER_SO(main_break_target_thread);
  }
  main_break_target_thread = p;
}

static void check_ready_break()
{
#if defined(MZ_USE_PLACES)
  if (!do_atomic)
    scheme_place_check_for_interruption();
#endif

  if (delayed_break_ready) {
    if (scheme_main_thread) {
      int kind = delayed_break_ready;
      delayed_break_ready = 0;
      scheme_break_kind_thread(main_break_target_thread, kind);
    }
  }
}

void scheme_break_kind_thread(Scheme_Thread *p, int kind)
{
  if (!p) {
    p = scheme_main_thread;
    if (!p)
      return;
  }

  /* Propagate breaks: */
  while (p->nestee) {
    p = p->nestee;
  }

  if (kind > p->external_break)
    p->external_break = kind;

  if (p == scheme_current_thread) {
    if (scheme_can_break(p)) {
      scheme_fuel_counter = 0;
      scheme_jit_stack_boundary = (uintptr_t)-1;
    }
  }
  scheme_weak_resume_thread(p);
# if defined(WINDOWS_PROCESSES) || defined(WINDOWS_FILE_HANDLES)
  if (SAME_OBJ(p, scheme_main_thread))
    ReleaseSemaphore((HANDLE)scheme_break_semaphore, 1, NULL);
# endif
}

void scheme_break_thread(Scheme_Thread *p)
{
  scheme_break_kind_thread(p, MZEXN_BREAK);
}

static void call_on_atomic_timeout(int must)
{
  Scheme_Thread *p = scheme_current_thread;
  Thread_Schedule_State_Record ssr;
  Scheme_On_Atomic_Timeout_Proc oat;

  /* Save any state that has to do with the thread blocking or 
     sleeping, in case on_atomic_timeout() runs Racket code. */
  save_thread_schedule_state(p, &ssr, 1);

  /* When on_atomic_timeout is thread-local, need a
     local variable so that the function call isn't
     obscured to xform: */
  oat = on_atomic_timeout;
  oat(must);

  restore_thread_schedule_state(p, &ssr, 1);
}

static void find_next_thread(Scheme_Thread **return_arg) {
  Scheme_Thread *next;
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *next_in_set;
  Scheme_Thread_Set *t_set;

  double msecs = 0.0;

  /* Find the next process. Skip processes that are definitely
     blocked. */

  /* Start from the root */
  next_in_set = (Scheme_Object *)scheme_thread_set_top;
  t_set = NULL; /* this will get set at the beginning of the loop */

  /* Each thread may or may not be available. If it's not available,
     we search thread by thread to find something that is available. */
  while (1) {
    /* next_in_set is the thread or set to try... */

    /* While it's a set, go down into the set, choosing the next
       item after the set's current. For each set, remember where we
       started searching for something to run, so we'll know when
       we've tried everything in the set. */
    while (!SCHEME_THREADP(next_in_set)) {
      t_set = (Scheme_Thread_Set *)next_in_set;
      next_in_set = get_t_set_next(t_set->current);
      if (!next_in_set)
        next_in_set = t_set->first;
      t_set->current = next_in_set;
      t_set->search_start = next_in_set;
    }

    /* Now `t_set' is the set we're trying, and `next' will be the
       thread to try: */
    next = (Scheme_Thread *)next_in_set;

    /* If we get back to the current thread, then
       no other thread was ready. */
    if (SAME_PTR(next, p)) {
      next = NULL;
      break;
    }

    /* Check whether `next' is ready... */

    if (next->nestee) {
      /* Blocked on nestee */
    } else if (next->running & MZTHREAD_USER_SUSPENDED) {
      if (next->next || (next->running & MZTHREAD_NEED_SUSPEND_CLEANUP)) {
        /* If a non-main thread is still in the queue, 
           it needs to be swapped in so it can clean up
           and suspend itself. */
        break;
      }
    } else if (next->running & MZTHREAD_KILLED) {
      /* This one has been terminated. */
      if ((next->running & MZTHREAD_NEED_KILL_CLEANUP) 
          || next->nester
          || !next->next) {
        /* The thread needs to clean up. Swap it in so it can die. */
        break;
      } else
        remove_thread(next);
      break;
    } else if (next->external_break && scheme_can_break(next)) {
      break;
    } else {
      if (next->block_descriptor == GENERIC_BLOCKED) {
        if (next->block_check) {
          Scheme_Ready_Fun_FPC f = (Scheme_Ready_Fun_FPC)next->block_check;
	  Scheme_Object *blocker = next->blocker;
          Scheme_Schedule_Info sinfo;
	  Thread_Schedule_State_Record ssr;
	  int b;

	  save_thread_schedule_state(p, &ssr, 0);

          init_schedule_info(&sinfo, next, 1, next->sleep_end);
	  b = f(blocker, &sinfo);
	  
	  restore_thread_schedule_state(p, &ssr, 0);

          if (b)
            break;
          next->sleep_end = sinfo.sleep_end;
          msecs = 0.0; /* that could have taken a while */
        }
      } else if (next->block_descriptor == SLEEP_BLOCKED) {
        if (!msecs)
          msecs = scheme_get_inexact_milliseconds();
        if (next->sleep_end <= msecs)
          break;
      } else
        break;
    }

    /* Look for the next thread/set in this set */
    if (next->t_set_next)
      next_in_set = next->t_set_next;
    else
      next_in_set = t_set->first;

    /* If we run out of things to try in this set,
       go up to find the next set. */
    if (SAME_OBJ(next_in_set, t_set->search_start)) {
      /* Loop to go up past exhausted sets, clearing search_start
         from each exhausted set. */
      while (1) {
        t_set->search_start = NULL;
        t_set = t_set->parent;

        if (t_set) {
          next_in_set = get_t_set_next(t_set->current);
          if (!next_in_set)
            next_in_set = t_set->first;

          if (SAME_OBJ(next_in_set, t_set->search_start)) {
            t_set->search_start = NULL;
            /* continue going up */
          } else {
            t_set->current = next_in_set;
            break;
          }
        } else
          break;
      }

      if (!t_set) {
        /* We ran out of things to try. If we
           start again with the top, we should
           land back at p. */
        next = NULL;
        break;
      }
    } else {
      /* Set current... */
      t_set->current = next_in_set;
    } 
    /* As we go back to the top of the loop, we'll check whether
       next_in_set is a thread or set, etc. */
  }

  p           = NULL;
  next_in_set = NULL;
  t_set       = NULL;
  *return_arg = next;
  next        = NULL;
}

void scheme_thread_block(float sleep_time)
     /* If we're blocked, `sleep_time' is a max sleep time,
	not a min sleep time. Otherwise, it's a min & max sleep time.
	This proc auto-resets p's blocking info if an escape occurs. */
{
  double sleep_end;
  Scheme_Thread *next;
  Scheme_Thread *p = scheme_current_thread;
  int skip_sleep;

  if (p->return_marks_to) /* just in case we get here */
    return;

  if (p->running & MZTHREAD_KILLED) {
    /* This thread is dead! Give up now. */
    if (!do_atomic)
      exit_or_escape(p);
  }

  if ((p->running & MZTHREAD_USER_SUSPENDED)
      && !(p->running & MZTHREAD_NEED_SUSPEND_CLEANUP)) {
    /* This thread was suspended. */
    scheme_wait_until_suspend_ok();
    if (!p->next) {
      /* Suspending the main thread... */
      select_thread();
    } else
      scheme_weak_suspend_thread(p);
  }

  /* Check scheduled_kills early and often. */
  check_scheduled_kills();

#if defined(UNIX_PROCESSES) && !defined(MZ_PLACES_WAITPID)
  /* Reap zombie processes: */
  scheme_check_child_done();
#endif

  shrink_cust_box_array();

  if (scheme_active_but_sleeping)
    scheme_wake_up();

  if (sleep_time > 0) {
    sleep_end = scheme_get_inexact_milliseconds();
    sleep_end += (sleep_time * 1000.0);
  } else
    sleep_end = 0;

 start_sleep_check:

  check_ready_break();

  if (!p->external_break && !p->next && scheme_check_for_break && scheme_check_for_break())
    p->external_break = 1;

  if (p->external_break && !p->suspend_break && scheme_can_break(p)) {
    raise_break(p);
    goto start_sleep_check;
  }
  
 swap_or_sleep:

#ifdef USE_OSKIT_CONSOLE
  scheme_check_keyboard_input();
#endif

  /* Check scheduled_kills early and often. */
  check_scheduled_kills();

#ifdef MZ_USE_FUTURES
  if (!do_atomic)
    scheme_check_future_work();
#endif
#if defined(MZ_USE_MZRT) && !defined(DONT_USE_FOREIGN)
  if (!do_atomic)
    scheme_check_foreign_work();
#endif

  skip_sleep = 0;
  if (check_fd_semaphores()) {
    /* double check whether a semaphore for this thread woke up: */
    if (!do_atomic && (p->block_descriptor == GENERIC_BLOCKED)) {
      if (p->block_check) {
        Scheme_Ready_Fun_FPC f = (Scheme_Ready_Fun_FPC)p->block_check;
	Scheme_Object *blocker = p->blocker;
        Scheme_Schedule_Info sinfo;
	Thread_Schedule_State_Record ssr;
	int b;

	save_thread_schedule_state(p, &ssr, 0);

        init_schedule_info(&sinfo, p, 1, sleep_end);
	b = f(blocker, &sinfo);

	restore_thread_schedule_state(p, &ssr, 0);

        if (b) {
          sleep_end = 0;
          skip_sleep = 1;
        } else {
          sleep_end = sinfo.sleep_end;
        }
      }
    }
  }

  if (!do_atomic && (sleep_end >= 0.0)) {
    find_next_thread(&next);
  } else
    next = NULL;
  
  if (next) {
    /* Clear out search_start fields */
    Scheme_Thread_Set *t_set;
    t_set = next->t_set_parent;
    while (t_set) {
      t_set->search_start = NULL;
      t_set = t_set->parent;
    }
    t_set = NULL;
  }

  if ((sleep_end > 0.0) && (p->block_descriptor == NOT_BLOCKED)) {
    p->block_descriptor = SLEEP_BLOCKED;
    p->sleep_end = sleep_end;
  } else if ((sleep_end > 0.0) && (p->block_descriptor == GENERIC_BLOCKED)) {
    p->sleep_end = sleep_end;
  }

  if (next && (!next->running || (next->running & MZTHREAD_SUSPENDED))) {
    /* In the process of selecting another thread, it was suspended or
       removed. Very unusual, but possible if a block checker does
       strange things??? */
    next = NULL;
  }

#if 0
  /* Debugging: next must be in the chain of processes */
  if (next) {
    Scheme_Thread *p = scheme_first_thread;
    while (p != next) {
      p = p->next;
      if (!p) {
	printf("error: tried to switch to bad thread\n");
	exit(1);
      }
    }
  }
#endif

  /*####################################*/
  /* THREAD CONTEXT SWITCH HAPPENS HERE */
  /*####################################*/

  if (next) {
    /* Swap in `next', but first clear references to other threads. */
    swap_target = next;
    next = NULL;
    do_swap_thread();
  } else if (do_atomic && on_atomic_timeout
             && (atomic_timeout_auto_suspend < 2)) {
    if (!atomic_timeout_auto_suspend
        || (do_atomic <= atomic_timeout_atomic_level)) {
      if (atomic_timeout_auto_suspend) {
        atomic_timeout_auto_suspend++;
        scheme_fuel_counter = p->engine_weight;
        scheme_jit_stack_boundary = scheme_stack_boundary;
      }
      call_on_atomic_timeout(0);
      if (atomic_timeout_auto_suspend > 1)
        --atomic_timeout_auto_suspend;
    }
  } else {
    /* If all processes are blocked, check for total process sleeping: */
    if ((p->block_descriptor != NOT_BLOCKED) && !skip_sleep) {
      check_sleep(1, 1);
    }
  }

  if (p->block_descriptor == SLEEP_BLOCKED) {
    p->block_descriptor = NOT_BLOCKED;
  }
  p->sleep_end = 0.0;

  /* Killed while I was asleep? */
  if (p->running & MZTHREAD_KILLED) {
    /* This thread is dead! Give up now. */
    if (p->running & MZTHREAD_NEED_KILL_CLEANUP) {
      /* The thread needs to clean up. It will block immediately to die. */
      return;
    } else {
      if (!do_atomic)
	exit_or_escape(p);
    }
  }

  /* Suspended while I was asleep? */
  if ((p->running & MZTHREAD_USER_SUSPENDED)
      && !(p->running & MZTHREAD_NEED_SUSPEND_CLEANUP)) {
    scheme_wait_until_suspend_ok();
    if (!p->next)
      scheme_thread_block(0.0); /* main thread handled at top of this function */
    else
      scheme_weak_suspend_thread(p);
  }

  /* Check for external break again after swap or sleep */
  check_ready_break();
  if (p->external_break && !p->suspend_break && scheme_can_break(p)) {
    raise_break(p);
  }

  /* Check for major GC request from master GC */
#if defined(MZ_PRECISE_GC) && defined(MZ_USE_PLACES)
  if (!do_atomic)
    GC_check_master_gc_request();
#endif

  /* Propagate memory-use information and check for custodian-based
     GC triggers due to child place memory use: */
#if defined(MZ_PRECISE_GC) && defined(MZ_USE_PLACES)
  scheme_place_check_memory_use();
  check_scheduled_kills();
#endif
  
  if (sleep_end > 0) {
    if (sleep_end > scheme_get_inexact_milliseconds()) {
      /* Still have time to sleep if necessary, but make sure we're
	 not ready (because maybe that's why we were swapped back in!) */
      if (p->block_descriptor == GENERIC_BLOCKED) {
	if (p->block_check) {
	  Scheme_Ready_Fun_FPC f = (Scheme_Ready_Fun_FPC)p->block_check;
	  Scheme_Object *blocker = p->blocker;
	  Scheme_Schedule_Info sinfo;
	  Thread_Schedule_State_Record ssr;
	  int b;
	  
	  save_thread_schedule_state(p, &ssr, 0);

	  init_schedule_info(&sinfo, p, 1, sleep_end);
	  b = f(blocker, &sinfo);

	  restore_thread_schedule_state(p, &ssr, 0);

	  if (b) {
	    sleep_end = 0;
	  } else {
	    sleep_end = sinfo.sleep_end;
	  }
	}
      }

      if (sleep_end > 0)
	goto swap_or_sleep;
    }
  }

  if (do_atomic)
    missed_context_switch = 1;

  scheme_fuel_counter = p->engine_weight;
  scheme_jit_stack_boundary = scheme_stack_boundary;

  scheme_kickoff_green_thread_time_slice_timer(MZ_THREAD_QUANTUM_USEC);

  /* Check scheduled_kills early and often. */
  check_scheduled_kills();
}

void scheme_making_progress()
{
  scheme_current_thread->ran_some = 1;
}

int scheme_block_until(Scheme_Ready_Fun _f, Scheme_Needs_Wakeup_Fun fdf,
		       Scheme_Object *data, float delay)
{
  int result;
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Ready_Fun_FPC f = (Scheme_Ready_Fun_FPC)_f;
  Scheme_Schedule_Info sinfo;
  double sleep_end;

  if (!delay)
    sleep_end = 0.0;
  else {
    sleep_end = scheme_get_inexact_milliseconds();
    sleep_end += (delay * 1000.0);    
  }

  /* We make an sinfo to be polite, but we also assume
     that f will not generate any redirections! */
  init_schedule_info(&sinfo, NULL, 1, sleep_end);

  while (!(result = f((Scheme_Object *)data, &sinfo))) {
    sleep_end = sinfo.sleep_end;
    if (sinfo.spin) {
      init_schedule_info(&sinfo, NULL, 1, 0.0);
      scheme_thread_block(0.0);
      scheme_current_thread->ran_some = 1;
    } else {
      if (sleep_end) {
	delay = (float)(sleep_end - scheme_get_inexact_milliseconds());
	delay /= 1000.0;
	if (delay <= 0)
	  delay = (float)0.00001;
      } else
	delay = 0.0;

      p->block_descriptor = GENERIC_BLOCKED;
      p->blocker = (Scheme_Object *)data;
      p->block_check = (Scheme_Ready_Fun)f;
      p->block_needs_wakeup = fdf;
      
      scheme_thread_block(delay);
      
      p->block_descriptor = NOT_BLOCKED;
      p->blocker = NULL;
      p->block_check = NULL;
      p->block_needs_wakeup = NULL;
    }
  }
  p->ran_some = 1;

  return result;
}

int scheme_block_until_enable_break(Scheme_Ready_Fun _f, Scheme_Needs_Wakeup_Fun fdf,
				    Scheme_Object *data, float delay, int enable_break)
{
  if (enable_break) {
    int v;
    Scheme_Cont_Frame_Data cframe;

    scheme_push_break_enable(&cframe, 1, 1);
    v = scheme_block_until(_f, fdf, data, delay);
    scheme_pop_break_enable(&cframe, 0);

    return v;
  } else
    return scheme_block_until(_f, fdf, data, delay);
}

static int ready_unless(Scheme_Object *o)
{
  Scheme_Object *data;
  Scheme_Ready_Fun f;

  data = (Scheme_Object *)((void **)o)[0];
  f = (Scheme_Ready_Fun)((void **)o)[2];

  return f(data);
}

static void needs_wakeup_unless(Scheme_Object *o, void *fds)
{
  Scheme_Object *data;
  Scheme_Needs_Wakeup_Fun fdf;

  data = (Scheme_Object *)((void **)o)[0];
  fdf = (Scheme_Needs_Wakeup_Fun)((void **)o)[3];

  fdf(data, fds);
}


int scheme_block_until_unless(Scheme_Ready_Fun f, Scheme_Needs_Wakeup_Fun fdf,
			      Scheme_Object *data, float delay, 
			      Scheme_Object *unless,
			      int enable_break)
{
  if (unless) {
    void **a;
    a = MALLOC_N(void *, 4);
    a[0] = data;
    a[1] = unless;
    a[2] = f;
    a[3] = fdf;

    data = (Scheme_Object *) mzALIAS a;
    f = ready_unless;
    if (fdf)
      fdf = needs_wakeup_unless;
  }
   
  return scheme_block_until_enable_break(f, fdf, data, delay, enable_break);
}

void scheme_thread_block_enable_break(float sleep_time, int enable_break)
{
  if (enable_break) {
    Scheme_Cont_Frame_Data cframe;
    
    scheme_push_break_enable(&cframe, 1, 1);
    scheme_thread_block(sleep_time);
    scheme_pop_break_enable(&cframe, 0);
  } else
    scheme_thread_block(sleep_time);
}

int scheme_is_atomic(void)
{
  return !!do_atomic;
}

void scheme_start_atomic(void)
{
  do_atomic++;
}

void scheme_start_atomic_no_break(void)
{
  scheme_start_atomic();
  all_breaks_disabled++;
}

void scheme_end_atomic_no_swap(void)
{
  int v = --do_atomic;
  if (v < 0) {
    scheme_log_abort("unbalanced end-atomic");
    abort();
  }
}

void scheme_start_in_scheduler(void)
{
  do_atomic++;
  scheme_no_stack_overflow++;
}
      
void scheme_end_in_scheduler(void)
{
  int v = --do_atomic;
  --scheme_no_stack_overflow;
  if (v < 0) {
    scheme_log_abort("unbalanced end-atomic");
    abort();
  }
}

void scheme_end_atomic(void)
{
  scheme_end_atomic_no_swap();
  if (!do_atomic && missed_context_switch) {
    missed_context_switch = 0;
    scheme_thread_block(0.0);
    scheme_current_thread->ran_some = 1;    
  }
}

void scheme_end_atomic_can_break(void)
{
  --all_breaks_disabled;
  scheme_end_atomic();
  if (!all_breaks_disabled)
    scheme_check_break_now();
}

int scheme_wait_until_suspend_ok(void)
{
  int did = 0;

  if (on_atomic_timeout) {
    /* new-style atomic timeout */
    if (do_atomic > atomic_timeout_atomic_level) {
      scheme_log_abort("attempted to wait for suspend in nested atomic mode");
      abort();
    }
  }

  while (do_atomic && on_atomic_timeout) {
    did = 1;
    if (atomic_timeout_auto_suspend)
      atomic_timeout_auto_suspend++;
    call_on_atomic_timeout(1);
    if (atomic_timeout_auto_suspend > 1)
      --atomic_timeout_auto_suspend;
  }

  if (do_atomic) {
    scheme_log_abort("about to suspend in atomic mode");
    abort();
  }

  return did;
}

Scheme_On_Atomic_Timeout_Proc scheme_set_on_atomic_timeout(Scheme_On_Atomic_Timeout_Proc p)
{
  Scheme_On_Atomic_Timeout_Proc old;

  old = on_atomic_timeout;
  on_atomic_timeout = p;
  if (p) {
    atomic_timeout_auto_suspend = 1;
    atomic_timeout_atomic_level = do_atomic;
  } else {
    atomic_timeout_auto_suspend = 0;
  }

  return old;
}

void scheme_weak_suspend_thread(Scheme_Thread *r)
{
  if (r->running & MZTHREAD_SUSPENDED)
    return;

  if (r->prev) {
    r->prev->next = r->next;
    r->next->prev = r->prev;
  } else {
    r->next->prev = NULL;
    scheme_first_thread = r->next;
  }

  r->next = r->prev = NULL;
  unschedule_in_set((Scheme_Object *)r, r->t_set_parent);

  r->running |= MZTHREAD_SUSPENDED;

  scheme_prepare_this_thread_for_GC(r);

  if (r == scheme_current_thread) {
    select_thread();

    /* Killed while suspended? */
    if ((r->running & MZTHREAD_KILLED) && !(r->running & MZTHREAD_NEED_KILL_CLEANUP))
      scheme_thread_block(0);
  }
}

void scheme_weak_resume_thread(Scheme_Thread *r)
     /* This function can be called from an interrupt handler, but
	only for the main thread, which is never suspended. */
{
  if (!(r->running & MZTHREAD_USER_SUSPENDED)) {
    if (r->running & MZTHREAD_SUSPENDED) {
      r->running -= MZTHREAD_SUSPENDED;
      r->next = scheme_first_thread;
      r->prev = NULL;
      scheme_first_thread = r;
      r->next->prev = r;
      r->ran_some = 1;
      schedule_in_set((Scheme_Object *)r, r->t_set_parent);
      scheme_check_tail_buffer_size(r);
    }
  }
}

void scheme_about_to_move_C_stack(void)
{
}

static Scheme_Object *
sch_sleep(int argc, Scheme_Object *args[])
{
  float t;

  if (argc && !SCHEME_REALP(args[0]))
    scheme_wrong_contract("sleep", "(>=/c 0.0)", 0, argc, args);

  if (argc) {
    t = (float)scheme_real_to_double(args[0]);
    if (t < 0)
      scheme_wrong_contract("sleep", "(>=/c 0.0)", 0, argc, args);
  } else
    t = 0;

  scheme_thread_block(t);
  scheme_current_thread->ran_some = 1;

  return scheme_void;
}

static Scheme_Object *break_thread(int argc, Scheme_Object *args[])
{
  Scheme_Thread *p;
  int kind = MZEXN_BREAK;

  if (!SAME_TYPE(SCHEME_TYPE(args[0]), scheme_thread_type))
    scheme_wrong_contract("break-thread", "thread?", 0, argc, args);

  if ((argc > 1) && SCHEME_TRUEP(args[1])) {
    if (SCHEME_SYMBOLP(args[1]) 
        && !SCHEME_SYM_WEIRDP(args[1]) 
        && !strcmp(SCHEME_SYM_VAL(args[1]), "hang-up"))
      kind = MZEXN_BREAK_HANG_UP;
    else if (SCHEME_SYMBOLP(args[1]) 
             && !SCHEME_SYM_WEIRDP(args[1]) 
             && !strcmp(SCHEME_SYM_VAL(args[1]), "terminate"))
      kind = MZEXN_BREAK_TERMINATE;
    else
      scheme_wrong_contract("break-thread", "(or/c #f 'hang-up 'terminate)", 1, argc, args);
  }

  p = (Scheme_Thread *)args[0];

  scheme_break_kind_thread(p, kind);

  scheme_check_break_now();

  return scheme_void;
}

static int do_kill_thread(Scheme_Thread *p)
{
  int kill_self = 0;

  if (!MZTHREAD_STILL_RUNNING(p->running)) {
    return 0;
  }

  if (p->suspend_to_kill) {
    if (p == scheme_current_thread)
      return 1; /* suspend in caller */
    suspend_thread(p);
    return 0;
  }

  if (p->nestee)
    scheme_break_thread(p->nestee);

  while (p->private_on_kill) {
    p->private_on_kill(p->private_kill_data);
    if (p->private_kill_next) {
      p->private_on_kill = (Scheme_Kill_Action_Func)p->private_kill_next[0];
      p->private_kill_data = p->private_kill_next[1];
      p->private_kill_next = (void **)p->private_kill_next[2];
    } else {
      p->private_on_kill = NULL;
      p->private_kill_data = NULL;
    }
  }

  if (p->on_kill)
    p->on_kill(p);

  scheme_remove_managed(p->mref, (Scheme_Object *)p->mr_hop);
  {
    Scheme_Object *l;
    for (l = p->extra_mrefs; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
      scheme_remove_managed((Scheme_Custodian_Reference *)SCHEME_CAR(l), 
			    (Scheme_Object *)p->mr_hop);
    }
  }

  if (p->running) {
    if (p->running & MZTHREAD_USER_SUSPENDED) {
      /* end user suspension, because we need to kill the thread */
      p->running -= MZTHREAD_USER_SUSPENDED;
    }

    p->running |= MZTHREAD_KILLED;
    if ((p->running & MZTHREAD_NEED_KILL_CLEANUP)
	|| p->nester)
      scheme_weak_resume_thread(p);
    else if (p != scheme_current_thread) {
      /* Do kill stuff... */
      if (p->next)
	remove_thread(p);
    }
  }
  if (p == scheme_current_thread)
    kill_self = 1;

  return kill_self;
}

void scheme_kill_thread(Scheme_Thread *p)
{
  if (do_kill_thread(p)) {
    /* Suspend/kill self: */
    scheme_wait_until_suspend_ok();
    if (p->suspend_to_kill)
      suspend_thread(p);
    else
      scheme_thread_block(0.0);
  }

  /* Give killed threads time to die: */
  scheme_thread_block(0.0);
  scheme_current_thread->ran_some = 1;
}

static Scheme_Object *kill_thread(int argc, Scheme_Object *argv[])
{
  Scheme_Thread *p = (Scheme_Thread *)argv[0];

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_thread_type))
    scheme_wrong_contract("kill-thread", "thread?", 0, argc, argv);

  if (!MZTHREAD_STILL_RUNNING(p->running))
    return scheme_void;

  check_current_custodian_allows("kill-thread", p);

  scheme_kill_thread(p);

  return scheme_void;
}

void scheme_push_kill_action(Scheme_Kill_Action_Func f, void *d)
{
  Scheme_Thread *p = scheme_current_thread;

  if (p->private_on_kill) {
    /* Pretty unlikely that these get nested. An exception handler
       would have to block on and within operations that need special
       kill handling. But it could happen. */
    void **next;
    next = MALLOC_N(void *, 3);
    next[0] = (void *)p->private_on_kill;
    next[1] = p->private_kill_data;
    next[2] = (void *)p->private_kill_next;
    p->private_kill_next = next;
  }

  p->private_on_kill = f;
  p->private_kill_data = d;
}

void scheme_pop_kill_action()
{
  Scheme_Thread *p = scheme_current_thread;

  if (p->private_kill_next) {
    p->private_on_kill = (Scheme_Kill_Action_Func)p->private_kill_next[0];
    p->private_kill_data = p->private_kill_next[1];
    p->private_kill_next = (void **)p->private_kill_next[2];
  } else {
    p->private_on_kill = NULL;
    p->private_kill_data = NULL;
  }
}

/*========================================================================*/
/*                      suspend/resume and evts                          */
/*========================================================================*/

/* Forward decl: */
static void transitive_resume(Scheme_Object *resumes);
static void transitive_promote(Scheme_Thread *p, Scheme_Custodian *c);
static void promote_thread(Scheme_Thread *p, Scheme_Custodian *to_c);

static Scheme_Object *thread_suspend(int argc, Scheme_Object *argv[])
{
  Scheme_Thread *p;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_thread_type))
    scheme_wrong_contract("thread-suspend", "thread?", 0, argc, argv);

  p = (Scheme_Thread *)argv[0];

  check_current_custodian_allows("thread-suspend", p);

  suspend_thread(p);

  return scheme_void;
}

static void suspend_thread(Scheme_Thread *p)
{
  int running;

  if (!MZTHREAD_STILL_RUNNING(p->running))
    return;
  
  if (p->running & MZTHREAD_USER_SUSPENDED)
    return;

  /* Get running now, just in case the thread is waiting on its own
     suspend event (in which case posting to the sema will unsuspend
     the thread) */
  running = p->running;

  p->resumed_box = NULL;
  if (p->suspended_box) {
    SCHEME_PTR2_VAL(p->suspended_box) = (Scheme_Object *)p;
    scheme_post_sema_all(SCHEME_PTR1_VAL(p->suspended_box));
  }
  if (p->running_box && (!(p->running & MZTHREAD_SUSPENDED))) {
    /* Make transitive-resume link strong, instead of weak: */
    SCHEME_PTR_VAL(p->running_box) = (Scheme_Object *)p;
  }

  if (SAME_OBJ(p, scheme_main_thread)) {
    /* p is the main thread, which we're not allowed to
       suspend in the normal way. */
    p->running |= MZTHREAD_USER_SUSPENDED;
    scheme_main_was_once_suspended = 1;
    if (p == scheme_current_thread) {
      scheme_thread_block(0.0);
      p->ran_some = 1;
    }
  } else if ((running & (MZTHREAD_NEED_KILL_CLEANUP
			 | MZTHREAD_NEED_SUSPEND_CLEANUP))
	     && (running & MZTHREAD_SUSPENDED)) {
    /* p probably needs to get out of semaphore-wait lines, etc. */
    scheme_weak_resume_thread(p);
    p->running |= MZTHREAD_USER_SUSPENDED;
  } else {
    if (p == scheme_current_thread) {
      scheme_wait_until_suspend_ok();
    }
    p->running |= MZTHREAD_USER_SUSPENDED;
    scheme_weak_suspend_thread(p); /* ok if p is scheme_current_thread */
    if (p == scheme_current_thread) {
      /* Need to check for breaks */
      scheme_check_break_now();
    }
  }
}

static void add_transitive_resume(Scheme_Thread *promote_to, Scheme_Thread *p)
{
  Scheme_Object *running_box;
  Scheme_Hash_Table *ht;

  if (!p->running_box) {
    Scheme_Object *b, *wb;
    if ((p->running & MZTHREAD_USER_SUSPENDED)
        && !(p->running & MZTHREAD_SUSPENDED))
      wb = (Scheme_Object *)p;
    else
      wb = scheme_make_weak_box((Scheme_Object *)p);
    b = scheme_alloc_small_object();
    b->type = scheme_thread_dead_type;
    SCHEME_PTR_VAL(b) = (Scheme_Object *)wb;
    p->running_box = b;
  }
  running_box = p->running_box;

  if (!promote_to->transitive_resumes) {
    /* Create table */
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    promote_to->transitive_resumes = (Scheme_Object *)ht;
  } else {
    /* Purge ht entries for threads that are now dead: */
    Scheme_Hash_Table *gone= NULL;
    int i;

    ht = (Scheme_Hash_Table *)promote_to->transitive_resumes;
    for (i = ht->size; i--; ) {
      if (ht->vals[i]) {
	if (!SCHEME_PTR_VAL(ht->keys[i])
            || (SAME_TYPE(SCHEME_TYPE(ht->keys[i]), scheme_weak_box_type)
                && !SCHEME_WEAK_BOX_VAL(ht->vals[i]))) {
	  /* This one is dead */
	  if (!gone)
	    gone = scheme_make_hash_table(SCHEME_hash_ptr);
	  scheme_hash_set(gone, ht->keys[i], scheme_true);
	}
      }
    }

    if (gone) {
      /* Remove dead ones: */
      for (i = gone->size; i--; ) {
	if (gone->vals[i]) {
	  scheme_hash_set(ht, gone->keys[i], NULL);
	}
      }
    }
  }

  scheme_hash_set(ht, running_box, scheme_true);
}

static Scheme_Object *transitive_resume_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *r = (Scheme_Object *)p->ku.k.p1;
  
  p->ku.k.p1 = NULL;

  transitive_resume(r);

  return scheme_true;
}

static void transitive_resume(Scheme_Object *resumes)
{
  Scheme_Hash_Table *ht;
  Scheme_Object *a[1];
  int i;

#ifdef DO_STACK_CHECK
#include "mzstkchk.h"
  {
    Scheme_Thread *p = scheme_current_thread;
    
    p->ku.k.p1 = resumes;

    p->suspend_break++;
    scheme_start_atomic();
    scheme_handle_stack_overflow(transitive_resume_k);
    scheme_end_atomic_no_swap();
    --p->suspend_break;

    return;
  }
#endif

  ht = (Scheme_Hash_Table *)resumes;
  
  for (i = ht->size; i--; ) {
    if (ht->vals[i]) {
      a[0] = SCHEME_PTR_VAL(ht->keys[i]);
      if (a[0]) {
        if (SAME_TYPE(SCHEME_TYPE(a[0]), scheme_weak_box_type))
          a[0] = SCHEME_WEAK_BOX_VAL(a[0]);
        if (a[0])
          thread_resume(1, a);
      }
    }
  }
}

static Scheme_Object *transitive_promote_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Thread *pp = (Scheme_Thread *)p->ku.k.p1;
  Scheme_Custodian *c = (Scheme_Custodian *)p->ku.k.p2;
  
  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  transitive_promote(pp, c);

  return scheme_true;
}

static void transitive_promote(Scheme_Thread *p, Scheme_Custodian *c)
{
  Scheme_Hash_Table *ht;
  Scheme_Object *t;
  int i;

#ifdef DO_STACK_CHECK
#include "mzstkchk.h"
  {
    Scheme_Thread *pp = scheme_current_thread;
    
    pp->ku.k.p1 = p;
    pp->ku.k.p2 = c;

    pp->suspend_break++;
    scheme_start_atomic();
    scheme_handle_stack_overflow(transitive_promote_k);
    scheme_end_atomic_no_swap();
    --pp->suspend_break;

    return;
  }
#endif

  if (!p->transitive_resumes)
    return;

  ht = (Scheme_Hash_Table *)p->transitive_resumes;
  
  for (i = ht->size; i--; ) {
    if (ht->vals[i]) {
      t = SCHEME_PTR_VAL(ht->keys[i]);
      if (SAME_TYPE(SCHEME_TYPE(t), scheme_weak_box_type))
        t = SCHEME_WEAK_BOX_VAL(t);
      if (t)
	promote_thread((Scheme_Thread *)t, c);
    }
  }
}

static void promote_thread(Scheme_Thread *p, Scheme_Custodian *to_c)
{
  Scheme_Custodian *c, *cx;
  Scheme_Custodian_Reference *mref;  
  Scheme_Object *l;

  /* This function also handles transitive promotion. Every transitive
     target for p always has at least the custodians of p, so if we don't
     add a custodian to p, we don't need to check the rest. */
  
  if (!p->mref || !CUSTODIAN_FAM(p->mref)) {
    /* The thread has no running custodian, so fall through to
       just use to_c */
  } else {
    c = CUSTODIAN_FAM(p->mref);

    /* Check whether c is an ancestor of to_c (in which case we do nothing) */
    for (cx = to_c; cx && NOT_SAME_OBJ(cx, c); ) {
      cx = CUSTODIAN_FAM(cx->parent);
    }
    if (cx) return;

    /* Check whether any of the extras are super to to_c. 
       If so, do nothing. */
    for (l = p->extra_mrefs; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
      mref = (Scheme_Custodian_Reference *)SCHEME_CAR(l);
      c = CUSTODIAN_FAM(mref);
      
      for (cx = to_c; cx && NOT_SAME_OBJ(cx, c); ) {
	cx = CUSTODIAN_FAM(cx->parent);
      }
      if (cx) return;
    }

    /* Check whether to_c is super of c: */
    for (cx = c; cx && NOT_SAME_OBJ(cx, to_c); ) {
      cx = CUSTODIAN_FAM(cx->parent);
    }
    
    /* If cx, fall through to replace the main custodian with to_c, 
       because it's an ancestor of the current one. Otherwise, they're
       unrelated. */
    if (!cx) {
      /* Check whether any of the extras should be replaced by to_c */
      for (l = p->extra_mrefs; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
	/* Is to_c super of c? */
	for (cx = c; cx && NOT_SAME_OBJ(cx, to_c); ) {
	  cx = CUSTODIAN_FAM(cx->parent);
	}
	if (cx) {
	  /* Replace this custodian with to_c */
	  mref = (Scheme_Custodian_Reference *)SCHEME_CAR(l);
	  scheme_remove_managed(mref, (Scheme_Object *)p->mr_hop);
	  mref = scheme_add_managed(to_c, (Scheme_Object *)p->mr_hop, NULL, NULL, 0);
	  SCHEME_CAR(l) = (Scheme_Object *)mref;

	  /* It's possible that one of the other custodians is also
	     junior to to_c. Remove it if we find one. */
	  {
	    Scheme_Object *prev;
	    prev = l;
	    for (l = SCHEME_CDR(l); !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
	      mref = (Scheme_Custodian_Reference *)SCHEME_CAR(l);
	      c = CUSTODIAN_FAM(mref);
	      for (cx = c; cx && NOT_SAME_OBJ(cx, to_c); ) {
		cx = CUSTODIAN_FAM(cx->parent);
	      }
	      if (cx)
		SCHEME_CDR(prev) = SCHEME_CDR(l);
	    }
	  }

	  transitive_promote(p, to_c);

	  return;
	}
      }

      /* Otherwise, this is custodian is unrelated to the existing ones.
	 Add it as an extra custodian. */
      mref = scheme_add_managed(to_c, (Scheme_Object *)p->mr_hop, NULL, NULL, 0);
      l = scheme_make_raw_pair((Scheme_Object *)mref, p->extra_mrefs);
      p->extra_mrefs = l;

      transitive_promote(p, to_c);
      return;
    }
  }

  /* Replace p's main custodian (if any) with to_c */
  scheme_remove_managed(p->mref, (Scheme_Object *)p->mr_hop);
  mref = scheme_add_managed(to_c, (Scheme_Object *)p->mr_hop, NULL, NULL, 0);
  p->mref = mref;
#ifdef MZ_PRECISE_GC
  GC_register_thread(p, to_c);
#endif
  
  transitive_promote(p, to_c);
}

static Scheme_Object *thread_resume(int argc, Scheme_Object *argv[])
{
  Scheme_Thread *p, *promote_to = NULL;
  Scheme_Custodian *promote_c = NULL;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_thread_type))
    scheme_wrong_contract("thread-resume", "thread?", 0, argc, argv);

  p = (Scheme_Thread *)argv[0];

  if (argc > 1) {
    if (SAME_TYPE(SCHEME_TYPE(argv[1]), scheme_thread_type))
      promote_to = (Scheme_Thread *)argv[1];
    else if (SAME_TYPE(SCHEME_TYPE(argv[1]), scheme_custodian_type)) {
      promote_c = (Scheme_Custodian *)argv[1];
      if (promote_c->shut_down)
	promote_c = NULL;
    } else {
      scheme_wrong_contract("thread-resume", "(or/c thread? custodian?)", 1, argc, argv);
      return NULL;
    }
  }

  if (!MZTHREAD_STILL_RUNNING(p->running))
    return scheme_void;

  /* Change/add custodians for p from promote_p */
  if (promote_to) {
    Scheme_Object *l;
    Scheme_Custodian_Reference *mref;

    /* If promote_to doesn't have a working custodian, there's
       nothing to donate */
    if (promote_to->mref && CUSTODIAN_FAM(promote_to->mref)) {
      promote_thread(p, CUSTODIAN_FAM(promote_to->mref));
      
      for (l = p->extra_mrefs; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
	mref = (Scheme_Custodian_Reference *)SCHEME_CAR(l);
	promote_thread(p, CUSTODIAN_FAM(mref));
      }
    }
  }
  if (promote_c)
    promote_thread(p, promote_c);

  /* Set up transitive resume for future resumes of promote_to: */
  if (promote_to 
      && MZTHREAD_STILL_RUNNING(promote_to->running)
      && !SAME_OBJ(promote_to, p))
    add_transitive_resume(promote_to, p);

  /* Check whether the thread has a non-shut-down custodian */
  {
    Scheme_Custodian *c;
    
    if (p->mref)
      c = CUSTODIAN_FAM(p->mref);
    else
      c = NULL;

    if (!c || c->shut_down)
      return scheme_void;
  }

  if (p->running & MZTHREAD_USER_SUSPENDED) {
    p->suspended_box = NULL;
    if (p->resumed_box) {
      SCHEME_PTR2_VAL(p->resumed_box) = (Scheme_Object *)p;
      scheme_post_sema_all(SCHEME_PTR1_VAL(p->resumed_box));
    }

    if (p->running_box && !(p->running & MZTHREAD_SUSPENDED)) {
      /* Make transitive-resume weak: */
      Scheme_Object *wb;
      wb = scheme_make_weak_box((Scheme_Object *)p);
      SCHEME_PTR_VAL(p->running_box) = wb;
    }
    
    p->running -= MZTHREAD_USER_SUSPENDED;
    
    scheme_weak_resume_thread(p);

    if (p->transitive_resumes)
      transitive_resume(p->transitive_resumes);
  }

  return scheme_void;
}

static Scheme_Object *make_thread_suspend(int argc, Scheme_Object *argv[])
{
  Scheme_Thread *p;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_thread_type))
    scheme_wrong_contract("thread-suspend-evt", "thread?", 0, argc, argv);

  p = (Scheme_Thread *)argv[0];

  return scheme_get_thread_suspend(p);
}

Scheme_Object *scheme_get_thread_suspend(Scheme_Thread *p)
{
  if (!p->suspended_box) {
    Scheme_Object *b;
    b = scheme_alloc_object();
    b->type = scheme_thread_suspend_type;
    if (MZTHREAD_STILL_RUNNING(p->running) && (p->running & MZTHREAD_USER_SUSPENDED))
      SCHEME_PTR2_VAL(b) = (Scheme_Object *)p;
    else {
      Scheme_Object *sema;
      sema = scheme_make_sema(0);
      SCHEME_PTR1_VAL(b) = sema;
    }
    p->suspended_box = b;
  }

  return p->suspended_box;
}

static Scheme_Object *make_thread_resume(int argc, Scheme_Object *argv[])
{
  Scheme_Thread *p;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_thread_type))
    scheme_wrong_contract("thread-resume-evt", "thread?", 0, argc, argv);

  p = (Scheme_Thread *)argv[0];

  if (!p->resumed_box) {
    Scheme_Object *b;
    b = scheme_alloc_object();
    b->type = scheme_thread_resume_type;
    if (MZTHREAD_STILL_RUNNING(p->running) && !(p->running & MZTHREAD_USER_SUSPENDED))
      SCHEME_PTR2_VAL(b) = (Scheme_Object *)p;
    else {
      Scheme_Object *sema;
      sema = scheme_make_sema(0);
      SCHEME_PTR1_VAL(b) = sema;
    }
    p->resumed_box = b;
  }

  return p->resumed_box;
}

static int resume_suspend_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo)
{
  Scheme_Object *t;

  t = SCHEME_PTR2_VAL(o);
  if (t) {
    scheme_set_sync_target(sinfo, o, t, NULL, 0, 0, NULL);
    return 1;
  }

  scheme_set_sync_target(sinfo, SCHEME_PTR1_VAL(o), o, NULL, 0, 1, NULL);
  return 0;
}

static Scheme_Object *make_thread_dead(int argc, Scheme_Object *argv[])
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_thread_type))
    scheme_wrong_contract("thread-dead-evt", "thread?", 0, argc, argv);

  return scheme_get_thread_dead((Scheme_Thread *)argv[0]);
}

Scheme_Object *scheme_get_thread_dead(Scheme_Thread *p)
{
  if (!p->dead_box) {
    Scheme_Object *b;
    Scheme_Object *sema;

    b = scheme_alloc_small_object();
    b->type = scheme_thread_dead_type;
    sema = scheme_make_sema(0);
    SCHEME_PTR_VAL(b) = sema;
    if (!MZTHREAD_STILL_RUNNING(p->running))
      scheme_post_sema_all(sema);

    p->dead_box = b;
  }

  return p->dead_box;
}

static int dead_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo)
{
  scheme_set_sync_target(sinfo, SCHEME_PTR_VAL(o), o, NULL, 0, 1, NULL);
  return 0;
}

Scheme_Object *scheme_get_thread_sync(Scheme_Thread *p)
{
  if (!p->sync_box) {
    Scheme_Object *sema;
    sema = scheme_make_sema(0);
    p->sync_box = sema;
  }
  
  return p->sync_box;
}

void scheme_clear_thread_sync(Scheme_Thread *p)
{
  if (p->sync_box)
    p->sync_box = NULL;
}

/*========================================================================*/
/*                              syncing                                   */
/*========================================================================*/

static void syncing_needs_wakeup(Scheme_Object *s, void *fds);
static Evt_Set *make_evt_set(const char *name, int argc, Scheme_Object **argv, int delta, int flatten);

typedef struct Evt {
  MZTAG_IF_REQUIRED
  Scheme_Type sync_type;
  Scheme_Ready_Fun_FPC ready;
  Scheme_Needs_Wakeup_Fun needs_wakeup;
  Scheme_Sync_Sema_Fun get_sema;
  Scheme_Sync_Filter_Fun filter;
  int can_redirect;
} Evt;


/* PLACE_THREAD_DECL */
static int evts_array_size;
static Evt **evts;
#if defined(MZ_PRECISE_GC) && defined(MZ_USE_PLACES)
THREAD_LOCAL_DECL(static int place_evts_array_size);
THREAD_LOCAL_DECL(static Evt **place_evts);
#endif

void scheme_add_evt_worker(Evt ***evt_array,
                           int *evt_size,
                           Scheme_Type type,
                           Scheme_Ready_Fun ready, 
                           Scheme_Needs_Wakeup_Fun wakeup, 
                           Scheme_Sync_Filter_Fun filter,
                           int can_redirect)
{
  Evt *naya;

  if (*evt_size <= type) {
    Evt **nevts;
    int new_size;
    new_size = type + 1;
    if (new_size < _scheme_last_type_)
      new_size = _scheme_last_type_;
    nevts = MALLOC_N(Evt*, new_size);
    memcpy(nevts, (*evt_array), (*evt_size) * sizeof(Evt*));
    (*evt_array) = nevts;
    (*evt_size) = new_size;
  }

  naya = MALLOC_ONE_RT(Evt);
#ifdef MZTAG_REQUIRED
  naya->type = scheme_rt_evt;
#endif
  naya->sync_type = type;
  naya->ready = (Scheme_Ready_Fun_FPC)ready;
  naya->needs_wakeup = wakeup;
  naya->filter = filter;
  naya->can_redirect = can_redirect;

  (*evt_array)[type] = naya;
}

void scheme_add_evt(Scheme_Type type,
		    Scheme_Ready_Fun ready, 
		    Scheme_Needs_Wakeup_Fun wakeup, 
		    Scheme_Sync_Filter_Fun filter,
		    int can_redirect)
{
#if defined(MZ_PRECISE_GC) && defined(MZ_USE_PLACES)
  if (GC_is_place()) {
    if (!place_evts) {
      REGISTER_SO(place_evts);
    }
    scheme_add_evt_worker(&place_evts, &place_evts_array_size, type, ready, wakeup, filter, can_redirect);
  }
  else {
#endif
    if (!evts) {
      REGISTER_SO(evts);
    }
    scheme_add_evt_worker(&evts, &evts_array_size, type, ready, wakeup, filter, can_redirect);
#if defined(MZ_PRECISE_GC) && defined(MZ_USE_PLACES)
  }
#endif
}

void scheme_add_evt_through_sema(Scheme_Type type,
				  Scheme_Sync_Sema_Fun get_sema, 
				  Scheme_Sync_Filter_Fun filter)
{
  scheme_add_evt(type, NULL, NULL, filter, 0);
  evts[type]->get_sema = get_sema;
}

static Evt *find_evt(Scheme_Object *o)
{
  Scheme_Type t;
  Evt *w = NULL;

  t = SCHEME_TYPE(o);
  if (t < evts_array_size)
    w = evts[t];
#if defined(MZ_PRECISE_GC) && defined(MZ_USE_PLACES)
  if (place_evts && w == NULL)
    w = place_evts[t];
#endif

  if (w && w->filter) {
    Scheme_Sync_Filter_Fun filter;
    filter = w->filter;
    if (!filter(o))
      return NULL;
  }
  return w;
}

int scheme_is_evt(Scheme_Object *o)
{
  if (SCHEME_EVTSETP(o))
    return 1;

  return !!find_evt(o);
}

static Syncing *make_syncing(Evt_Set *evt_set, float timeout, double start_time) 
{
  Syncing *syncing;
  int pos;

  syncing = MALLOC_ONE_RT(Syncing);
#ifdef MZTAG_REQUIRED
  syncing->type = scheme_rt_syncing;
#endif
  syncing->set = evt_set;
  syncing->timeout = timeout;
  if (timeout >= 0)
    syncing->sleep_end = start_time + (timeout * 1000);
  else
    syncing->sleep_end = 0.0;

  if (evt_set->argc > 1) {
    Scheme_Config *config;
    Scheme_Object *rand_state;
    config = scheme_current_config();
    rand_state = scheme_get_param(config, MZCONFIG_SCHEDULER_RANDOM_STATE);
    pos = scheme_rand((Scheme_Random_State *)rand_state);
    syncing->start_pos = (pos % evt_set->argc);
  }
  syncing->thread = scheme_current_thread;

  return syncing;
}

static void *splice_ptr_array(void **a, int al, void **b, int bl, int i)
{
  void **r;
  int j;
  
  r = MALLOC_N(void*, al + bl - 1);

  if (a)
    memcpy(r, a, i * sizeof(void*));
  if (b)
    memcpy(r + i, b, bl * sizeof(void*));
  else {
    for (j = 0; j < bl; j++) {
      r[i+j] = a[i];
    }
  }
  if (a)
    memcpy(r + (i + bl), a + (i + 1), (al - i - 1) * sizeof(void*));

  return r;
}

static void set_sync_target(Syncing *syncing, int i, Scheme_Object *target, 
			    Scheme_Object *wrap, Scheme_Object *nack, 
			    int repost, int retry, Scheme_Accept_Sync accept)
/* Not ready, deferred to target. */
{
  Evt_Set *evt_set = syncing->set;

  if (wrap) {
    if (!syncing->wrapss) {
      Scheme_Object **wrapss;
      wrapss = MALLOC_N(Scheme_Object*, evt_set->argc);
      syncing->wrapss = wrapss;
    }
    if (!syncing->wrapss[i])
      syncing->wrapss[i] = scheme_null;
    wrap = scheme_make_pair(wrap, syncing->wrapss[i]);
    syncing->wrapss[i] = wrap;
  }

  if (nack) {
    if (!syncing->nackss) {
      Scheme_Object **nackss;
      nackss = MALLOC_N(Scheme_Object*, evt_set->argc);
      syncing->nackss = nackss;
    }
    if (!syncing->nackss[i])
      syncing->nackss[i] = scheme_null;
    nack = scheme_make_pair(nack, syncing->nackss[i]);
    syncing->nackss[i] = nack;
  }

  if (repost) {
    if (!syncing->reposts) {
      char *s;
      s = (char *)scheme_malloc_atomic(evt_set->argc);
      memset(s, 0, evt_set->argc);
      syncing->reposts = s;
    }
    syncing->reposts[i] = 1;
  }

  if (accept) {
    if (!syncing->accepts) {
      Scheme_Accept_Sync *s;
      s = (Scheme_Accept_Sync *)scheme_malloc_atomic(sizeof(Scheme_Accept_Sync) * evt_set->argc);
      memset(s, 0, evt_set->argc * sizeof(Scheme_Accept_Sync));
      syncing->accepts = s;
    }
    syncing->accepts[i] = accept;
  }

  if (SCHEME_EVTSETP(target) && retry) {
    /* Flatten the set into this one */
    Evt_Set *wts;

    if (SCHEME_EVTSET_UNFLATTENEDP(target)) {
      Scheme_Object *a[1];
      a[0] = target;
      wts = make_evt_set("sync", 1, a, 0, 1);
    } else
      wts = (Evt_Set *)target;

    if (wts->argc == 1) {
      /* 1 thing in set? Flattening is easy! */
      evt_set->argv[i] = wts->argv[0];
      evt_set->ws[i] = wts->ws[0];
    } else {
      /* Inline the set (in place) */
      Scheme_Object **argv;
      Evt **ws;
       
      argv = (Scheme_Object **)splice_ptr_array((void **)evt_set->argv, 
						evt_set->argc,
						(void **)wts->argv, 
						wts->argc,
						i);
      ws = (Evt **)splice_ptr_array((void **)evt_set->ws, 
					 evt_set->argc,
					 (void **)wts->ws, 
					 wts->argc,
					 i);

      evt_set->argv = argv;
      evt_set->ws = ws;

      if (syncing->wrapss) {
	argv = (Scheme_Object **)splice_ptr_array((void **)syncing->wrapss, 
						  evt_set->argc,
						  (void **)NULL,
						  wts->argc,
						  i);
	syncing->wrapss = argv;
      }
      if (syncing->nackss) {
	argv = (Scheme_Object **)splice_ptr_array((void **)syncing->nackss, 
						  evt_set->argc,
						  (void **)NULL,
						  wts->argc,
						  i);
	syncing->nackss = argv;
      }
      if (syncing->reposts) {
	char *s;
	int len;
	
	len = evt_set->argc + wts->argc - 1;
	
	s = (char *)scheme_malloc_atomic(len);
	memset(s, 0, len);
	
	memcpy(s, syncing->reposts, i);
	memcpy(s + i + wts->argc, syncing->reposts + i + 1, evt_set->argc - i - 1);
	syncing->reposts = s;
      }
      if (syncing->accepts) {
	Scheme_Accept_Sync *s;
	int len;
	
	len = evt_set->argc + wts->argc - 1;
	
	s = (Scheme_Accept_Sync *)scheme_malloc_atomic(len * sizeof(Scheme_Accept_Sync));
	memset(s, 0, len * sizeof(Scheme_Accept_Sync));
	
	memcpy(s, syncing->accepts, i * sizeof(Scheme_Accept_Sync));
	memcpy(s + i + wts->argc, syncing->accepts + i + 1, (evt_set->argc - i - 1) * sizeof(Scheme_Accept_Sync));
	syncing->accepts = s;
      }

      evt_set->argc += (wts->argc - 1);

      /* scheme_channel_syncer_type needs to know its location, which
	 might have changed: */
      argv = evt_set->argv;
      for (i = evt_set->argc; i--; ) {
	if (SAME_TYPE(SCHEME_TYPE(argv[i]), scheme_channel_syncer_type)) {
	  ((Scheme_Channel_Syncer *)argv[i])->syncing_i = i;
	}
      }

    }
  } else {
    Evt *ww;
    evt_set->argv[i] = target;
    ww = find_evt(target);
    evt_set->ws[i] = ww;
  }
}

void scheme_set_sync_target(Scheme_Schedule_Info *sinfo, Scheme_Object *target, 
			    Scheme_Object *wrap, Scheme_Object *nack, 
			    int repost, int retry, Scheme_Accept_Sync accept)
{
  set_sync_target((Syncing *)sinfo->current_syncing, sinfo->w_i,
		  target, wrap, nack, repost, retry, accept);
  if (retry) {
    /* Rewind one step to try new ones (or continue
       if the set was empty). */
    sinfo->w_i--;
  }
}

static int syncing_ready(Scheme_Object *s, Scheme_Schedule_Info *sinfo)
{
  int i, redirections = 0, all_semas = 1, j, result = 0;
  Evt *w;
  Scheme_Object *o;
  Scheme_Schedule_Info r_sinfo;
  Syncing *syncing = (Syncing *)s;
  Evt_Set *evt_set;
  int is_poll;
  double sleep_end;
  
  sleep_end = syncing->sleep_end;

  if (syncing->result) {
    result = 1;
    goto set_sleep_end_and_return;
  }

  /* We must handle target redirections in the objects on which we're
     syncing. We never have to redirect the evt_set itself, but
     a evt_set can show up as a target, and we inline it in
     that case. */

  evt_set = syncing->set;

  is_poll = (syncing->timeout == 0.0);

  /* Anything ready? */
  for (j = 0; j < evt_set->argc; j++) {
    Scheme_Ready_Fun_FPC ready;

    i = (j + syncing->start_pos) % evt_set->argc;

    o = evt_set->argv[i];
    w = evt_set->ws[i];
    ready = w->ready;

    if (!SCHEME_SEMAP(o)
	&& !SCHEME_CHANNELP(o) && !SCHEME_CHANNEL_PUTP(o)
	&& !SAME_TYPE(SCHEME_TYPE(o), scheme_channel_syncer_type)
	&& !SAME_TYPE(SCHEME_TYPE(o), scheme_never_evt_type))
      all_semas = 0;

    if (ready) {
      int yep;

      init_schedule_info(&r_sinfo, sinfo->false_positive_ok, 0, sleep_end);

      r_sinfo.current_syncing = (Scheme_Object *)syncing;
      r_sinfo.w_i = i;
      r_sinfo.is_poll = is_poll;

      yep = ready(o, &r_sinfo);

      sleep_end = r_sinfo.sleep_end;

      /* Calling a guard can allow thread swap, which might choose a
         semaphore or a channel, so check for a result: */
      if (syncing->result) {
        result = 1;
        goto set_sleep_end_and_return;
      }

      if ((i > r_sinfo.w_i) && sinfo->false_positive_ok) {
	/* There was a redirection. Assert: !yep. 
	   Give up if we've chained too much. */
	redirections++;
	if (redirections > 10) {
	  sinfo->potentially_false_positive = 1;
	  result = 1;
	  goto set_sleep_end_and_return;
	}
      }

      j += (r_sinfo.w_i - i);

      if (yep) {
	/* If it was a potentially false positive,
	   don't set result permanently. Otherwise,
	   propagate the false-positive indicator.*/
	if (!r_sinfo.potentially_false_positive) {
	  syncing->result = i + 1;
	  if (syncing->disable_break)
	    syncing->disable_break->suspend_break++;
	  if (syncing->reposts && syncing->reposts[i])
	    scheme_post_sema(o);
          if (syncing->accepts && syncing->accepts[i])
            scheme_accept_sync(syncing, i);
	  scheme_post_syncing_nacks(syncing);
	  result = 1;
	  goto set_sleep_end_and_return;
	} else {
	  sinfo->potentially_false_positive = 1;
	  result = 1;
	  goto set_sleep_end_and_return;
	}
      } else if (r_sinfo.spin) {
	sinfo->spin = 1;
      }
    } else if (w->get_sema) {
      int repost = 0;
      Scheme_Sync_Sema_Fun get_sema = w->get_sema;
      Scheme_Object *sema;
      
      sema = get_sema(o, &repost);
      set_sync_target(syncing, i, sema, o, NULL, repost, 1, NULL);
      j--; /* try again with this sema */
    }

    if (syncing->result)
      scheme_signal_error("internal error: sync result set unexpectedly");
  }

  if (syncing->timeout >= 0.0) {
    if (syncing->sleep_end <= scheme_get_inexact_milliseconds())
      result = 1;
  } else if (all_semas) {
    /* Try to block in a GCable way: */
    if (sinfo->false_positive_ok) {
      /* In scheduler. Swap us in so we can suspend. */
      sinfo->potentially_false_positive = 1;
      result = 1;
    } else {
      /* Not in scheduler --- we're allowed to block via suspend,
	 which makes the thread GCable. */
      scheme_wait_semas_chs(syncing->set->argc, syncing->set->argv, 0, syncing);

      /* In case a break appeared after we chose something,
	 check for a break, because scheme_wait_semas_chs() won't: */
      scheme_check_break_now();

      result = 1;
    }
  }

 set_sleep_end_and_return:

  syncing->sleep_end = sleep_end;
  if (syncing->sleep_end
      && (!sinfo->sleep_end
	  || (sinfo->sleep_end > syncing->sleep_end)))
    sinfo->sleep_end = syncing->sleep_end;

  return result;
}

void scheme_accept_sync(Syncing *syncing, int i)
{
  /* run atomic accept action to revise the wrap */
  Scheme_Accept_Sync accept;
  Scheme_Object *v, *pr;
  
  accept = syncing->accepts[i];
  syncing->accepts[i] = NULL;
  pr = syncing->wrapss[i];
  
  v = SCHEME_CAR(pr);
  pr = SCHEME_CDR(pr);
  
  v = accept(v);
  
  pr = scheme_make_pair(v, pr);
  syncing->wrapss[i] = pr;
}

static void syncing_needs_wakeup(Scheme_Object *s, void *fds)
{
  int i;
  Scheme_Object *o;
  Evt *w;
  Evt_Set *evt_set = ((Syncing *)s)->set;

  for (i = 0; i < evt_set->argc; i++) {
    o = evt_set->argv[i];
    w = evt_set->ws[i];

    if (w->needs_wakeup) {
      Scheme_Needs_Wakeup_Fun nw = w->needs_wakeup;
      
      nw(o, fds);
    }
  }
}

static Scheme_Object *evt_p(int argc, Scheme_Object *argv[])
{
  return (scheme_is_evt(argv[0])
	  ? scheme_true
	  : scheme_false);
}

static int evt_set_flatten(Evt_Set *e, int pos, Scheme_Object **args, Evt **ws)
{
  Scheme_Object *stack = scheme_null;
  int i;

  while (1) {
    for (i = e->argc; i--; ) {
      if (!SCHEME_EVTSETP(e->argv[i])) {
        if (args) {
          args[pos] = e->argv[i];
          ws[pos] = e->ws[i];
        }
        pos++;
      } else
        stack = scheme_make_pair(e->argv[i], stack);
    }

    if (!SCHEME_NULLP(stack)) {
      e = (Evt_Set *)SCHEME_CAR(stack);
      stack = SCHEME_CDR(stack);
    } else
      break;
  }

  return pos;
}

static Evt_Set *make_evt_set(const char *name, int argc, Scheme_Object **argv, int delta, int flatten)
{
  Evt *w, **iws, **ws;
  Evt_Set *evt_set, *subset;
  Scheme_Object **args;
  int i, j, count = 0, reuse = 1, unflattened = 0;

  iws = MALLOC_N(Evt*, argc-delta);
  
  /* Find Evt record for each non-set argument, and compute size --- possibly flattened. */
  for (i = 0; i < (argc - delta); i++) {
    if (!SCHEME_EVTSETP(argv[i+delta])) {
      w = find_evt(argv[i+delta]);
      if (!w) {
	scheme_wrong_contract(name, "evt?", i+delta, argc, argv);
	return NULL;
      }
      iws[i] = w;
      count++;
    } else if (flatten) {
      int n;
      if (SCHEME_EVTSET_UNFLATTENEDP(argv[i+delta])) {
        n = evt_set_flatten((Evt_Set *)argv[i+delta], 0, NULL, NULL);
      } else {
        n = ((Evt_Set *)argv[i+delta])->argc;
      }
      if (n != 1)
        reuse = 0;
      count += n;
    } else {
      count++;
      unflattened = 1;
    }
  }

  evt_set = MALLOC_ONE_TAGGED(Evt_Set);
  evt_set->iso.so.type = scheme_evt_set_type;
  evt_set->argc = count;
  if (unflattened)
    SCHEME_SET_EVTSET_UNFLATTENED(evt_set);

  if (reuse && (count == (argc - delta)))
    ws = iws;
  else
    ws = MALLOC_N(Evt*, count);

  args = MALLOC_N(Scheme_Object*, count);
  for (i = delta, j = 0; i < argc; i++, j++) {
    if (flatten && SCHEME_EVTSETP(argv[i])) {
      if (SCHEME_EVTSET_UNFLATTENEDP(argv[i])) {
        j = evt_set_flatten((Evt_Set *)argv[i], j, args, ws);
        j--;
      } else {
        int k, n;
        subset = (Evt_Set *)argv[i];
        n = subset->argc;
        for (k = 0; k < n; k++, j++) {
          args[j] = subset->argv[k];
          ws[j] = subset->ws[k];
        }
        --j;
      }
    } else {
      ws[j] = iws[i-delta];
      args[j] = argv[i];
    }
  }

  evt_set->ws = ws;
  evt_set->argv = args;

  return evt_set;
}

Scheme_Object *scheme_make_evt_set(int argc, Scheme_Object **argv)
{
  return (Scheme_Object *)make_evt_set("internal-make-evt-set", argc, argv, 0, 1);
}

void scheme_post_syncing_nacks(Syncing *syncing)
     /* Also removes channel-syncers. Can be called multiple times. */
{
  int i, c;
  Scheme_Object *l;

  if (syncing->thread && syncing->thread->sync_box)
    syncing->thread->sync_box = NULL;

  if (syncing->set) {
    c = syncing->set->argc;
    
    for (i = 0; i < c; i++) {
      if (SAME_TYPE(SCHEME_TYPE(syncing->set->argv[i]), scheme_channel_syncer_type))
	scheme_get_outof_line((Scheme_Channel_Syncer *)syncing->set->argv[i]);
      if (syncing->nackss) {
	if ((i + 1) != syncing->result) {
	  l = syncing->nackss[i];
	  if (l) {
	    for (; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
	      scheme_post_sema_all(SCHEME_CAR(l));
	    }
	  }
	  syncing->nackss[i] = NULL;
	}
      }
    }
  }
}

static void escape_during_sync(Syncing *syncing)
{
  Scheme_Thread *p = syncing->thread;

  syncing->thread = NULL;

  if (p && p->sync_box)
    scheme_post_sema_all(p->sync_box);
  scheme_post_syncing_nacks(syncing);

#ifdef MZ_PRECISE_GC
  if (p && p->place_channel_msg_in_flight) {
    GC_destroy_orphan_msg_memory(p->place_channel_msg_in_flight);
    p->place_channel_msg_in_flight = NULL;
  }
#endif
}

static Scheme_Object *do_sync(const char *name, int argc, Scheme_Object *argv[], 
			      int with_break, int with_timeout, int _tailok)
{
  volatile int tailok = _tailok;
  Evt_Set * volatile evt_set;
  Syncing * volatile syncing;
  volatile float timeout = -1.0;
  double start_time;
  Scheme_Cont_Frame_Data cframe;

  if (with_timeout) {
    if (!SCHEME_FALSEP(argv[0])) {
      if (SCHEME_REALP(argv[0]))
	timeout = (float)scheme_real_to_double(argv[0]);
      else if (scheme_check_proc_arity(NULL, 0, 0, argc, argv))
        timeout = 0.0;
      
      if (timeout < 0.0) {
	scheme_wrong_contract(name, "(>=/c 0.0)", 0, argc, argv);
	return NULL;
      }
      
      start_time = scheme_get_inexact_milliseconds();
    } else
      start_time = 0;
  } else {
    start_time = 0;
  }

  /* Special case: no timeout, only object is a semaphore */
  if (argc == (with_timeout + 1) && !start_time && SCHEME_SEMAP(argv[with_timeout])) {
    scheme_wait_sema(argv[with_timeout], with_break ? -1 : 0);
    return argv[with_timeout];
  }

  evt_set = NULL;

  /* Special case: only argument is an immutable evt set: */
  if ((argc == (with_timeout + 1)) 
      && SCHEME_EVTSETP(argv[with_timeout])
      && !SCHEME_EVTSET_UNFLATTENEDP(argv[with_timeout])) {
    int i;
    evt_set = (Evt_Set *)argv[with_timeout];
    for (i = evt_set->argc; i--; ) {
      if (evt_set->ws[i]->can_redirect) {
	/* Need to copy this set to handle redirections. */
	evt_set = NULL;
	break;
      }
    }
  }

  if (!evt_set)
    evt_set = make_evt_set(name, argc, argv, with_timeout, 1);

  if (with_break) {
    scheme_push_break_enable(&cframe, 1, 1);
  }

  /* Check for another special case: syncing on a set of semaphores
     without a timeout. Use general code for channels.
     (Note that we check for this case after evt-set flattening.) */
  if (timeout < 0.0) {
    int i;
    for (i = evt_set->argc; i--; ) {
      if (!SCHEME_SEMAP(evt_set->argv[i]))
	break;
    }
    if (i < 0) {
      /* Hit the special case. */
      i = scheme_wait_semas_chs(evt_set->argc, evt_set->argv, 0, NULL);

      if (with_break) {
	scheme_pop_break_enable(&cframe, 1);
      } else {
	/* In case a break appeared after we received a post,
	   check for a break, because scheme_wait_semas_chs() won't: */
	scheme_check_break_now();
      }

      if (i)
	return evt_set->argv[i - 1];
      else
	return (tailok ? scheme_false : NULL);
    }
  }

  syncing = make_syncing(evt_set, timeout, start_time);

  if (timeout < 0.0)
    timeout = 0.0; /* means "no timeout" to block_until */

  if (with_break) {
    /* Suspended breaks when something is selected. */
    syncing->disable_break = scheme_current_thread;
  }

  BEGIN_ESCAPEABLE(escape_during_sync, syncing);
  scheme_block_until((Scheme_Ready_Fun)syncing_ready, syncing_needs_wakeup, 
		     (Scheme_Object *)syncing, timeout);
  END_ESCAPEABLE();

  if (!syncing->result)
    scheme_post_syncing_nacks(syncing);

  if (with_break) {
    scheme_pop_break_enable(&cframe, 0);
  }

  if (with_break) {
    /* Reverse low-level break disable: */
    --syncing->disable_break->suspend_break;
  }

  if (syncing->result) {
    /* Apply wrap functions to the selected evt: */
    Scheme_Object *o, *l, *a, *to_call = NULL, *args[1], **mv = NULL;
    int to_call_is_handle = 0, rc = 1;

    o = evt_set->argv[syncing->result - 1];
    if (SAME_TYPE(SCHEME_TYPE(o), scheme_channel_syncer_type)) {
      /* This is a put that got changed to a syncer, but not changed back */
      o = ((Scheme_Channel_Syncer *)o)->obj;
    }
    if (syncing->wrapss) {
      l = syncing->wrapss[syncing->result - 1];
      if (l) {
	for (; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
	  a = SCHEME_CAR(l);
	  if (to_call) {
            if (rc == 1) {
              mv = args;
              args[0] = o;
            }
	    
	    /* Call wrap proc with breaks disabled */
	    scheme_push_break_enable(&cframe, 0, 0);
	    
	    o = scheme_apply_multi(to_call, rc, mv);

            if (SAME_OBJ(o, SCHEME_MULTIPLE_VALUES)) {
              rc = scheme_multiple_count;
              mv = scheme_multiple_array;
              scheme_detach_multple_array(mv);
            } else {
              rc = 1;
              mv = NULL;
            }
	    
	    scheme_pop_break_enable(&cframe, 0);

	    to_call = NULL;
	  }
	  if (SCHEME_BOXP(a) || SCHEME_PROCP(a)) {
	    if (SCHEME_BOXP(a)) {
	      a = SCHEME_BOX_VAL(a);
	      to_call_is_handle = 1;
	    }
	    to_call = a;
	  } else if (SAME_TYPE(scheme_thread_suspend_type, SCHEME_TYPE(a))
		     || SAME_TYPE(scheme_thread_resume_type, SCHEME_TYPE(a))) {
	    o = SCHEME_PTR2_VAL(a);
            rc = 1;
          } else {
	    o = a;
            rc = 1;
          }
	}

	if (to_call) {
          if (rc == 1) {
            mv = args;
            args[0] = o;
          }
	  
	  /* If to_call is still a wrap-evt (not a handle-evt),
	     then set the config one more time: */
	  if (!to_call_is_handle) {
	    scheme_push_break_enable(&cframe, 0, 0);
	    tailok = 0;
	  }

	  if (tailok) {
	    return _scheme_tail_apply(to_call, rc, mv);
	  } else {
	    o = scheme_apply_multi(to_call, rc, mv);

            if (SAME_OBJ(o, SCHEME_MULTIPLE_VALUES)) {
              rc = scheme_multiple_count;
              mv = scheme_multiple_array;
              scheme_detach_multple_array(mv);
              if (!to_call_is_handle)
                scheme_pop_break_enable(&cframe, 1);
              return scheme_values(rc, mv);
            } else {
              if (!to_call_is_handle)
                scheme_pop_break_enable(&cframe, 1);
              return o;
            }
	  }
	}
      }
    }
    return o;
  } else {
    if (with_timeout && SCHEME_PROCP(argv[0])) {
      if (tailok)
        return _scheme_tail_apply(argv[0], 0, NULL);
      else
        return _scheme_apply(argv[0], 0, NULL);
    } else if (tailok)
      return scheme_false;
    else
      return NULL;
  }
}

static Scheme_Object *sch_sync(int argc, Scheme_Object *argv[])
{
  return do_sync("sync", argc, argv, 0, 0, 1);
}

static Scheme_Object *sch_sync_timeout(int argc, Scheme_Object *argv[])
{
  return do_sync("sync/timeout", argc, argv, 0, 1, 1);
}

Scheme_Object *scheme_sync(int argc, Scheme_Object *argv[])
{
  return do_sync("sync", argc, argv, 0, 0, 0);
}

Scheme_Object *scheme_sync_timeout(int argc, Scheme_Object *argv[])
{
  return do_sync("sync/timeout", argc, argv, 0, 1, 0);
}

static Scheme_Object *do_scheme_sync_enable_break(const char *who, int with_timeout, int tailok, int argc, Scheme_Object *argv[])
{
  Scheme_Object *sema;

  if (with_timeout && (argc == 2) && SCHEME_FALSEP(argv[0]) && SCHEME_SEMAP(argv[1]))
    sema = argv[1];
  else if (!with_timeout && (argc == 1) && SCHEME_SEMAP(argv[0]))
    sema = argv[0];
  else
    sema = NULL;
  
  if (sema) {
    scheme_wait_sema(sema, -1);
    return sema;
  }

  return do_sync(who, argc, argv, 1, with_timeout, tailok);
}

Scheme_Object *scheme_sync_enable_break(int argc, Scheme_Object *argv[])
{
  return do_scheme_sync_enable_break("sync/enable-break", 0, 0, argc, argv);
}

static Scheme_Object *sch_sync_enable_break(int argc, Scheme_Object *argv[])
{
  return do_scheme_sync_enable_break("sync/enable-break", 0, 1, argc, argv);
}

static Scheme_Object *sch_sync_timeout_enable_break(int argc, Scheme_Object *argv[])
{
  return do_scheme_sync_enable_break("sync/timeout/enable-break", 1, 1, argc, argv);
}

static Scheme_Object *evts_to_evt(int argc, Scheme_Object *argv[])
{
  return (Scheme_Object *)make_evt_set("choice-evt", argc, argv, 0, 0);
}

/*========================================================================*/
/*                             thread cells                               */
/*========================================================================*/

#define SCHEME_THREAD_CELLP(x) (SAME_TYPE(SCHEME_TYPE(x), scheme_thread_cell_type))

Scheme_Object *scheme_make_thread_cell(Scheme_Object *def_val, int inherited)
{
  Thread_Cell *c;

  c = MALLOC_ONE_TAGGED(Thread_Cell);
  c->so.type = scheme_thread_cell_type;
  c->def_val = def_val;
  c->inherited = !!inherited;

  return (Scheme_Object *)c;
}

static Scheme_Object *do_thread_cell_get(Scheme_Object *cell, Scheme_Thread_Cell_Table *cells)
{
  Scheme_Object *v;

  if (((Thread_Cell *)cell)->assigned) {
    v = scheme_lookup_in_table(cells, (const char *)cell);
    if (v)
      return scheme_ephemeron_value(v);
  }

  return ((Thread_Cell *)cell)->def_val;
}

Scheme_Object *scheme_thread_cell_get(Scheme_Object *cell, Scheme_Thread_Cell_Table *cells)
{
  if (!((Thread_Cell *)cell)->assigned)
    return ((Thread_Cell *)cell)->def_val;
  else
    return do_thread_cell_get(cell, cells);
}

void scheme_thread_cell_set(Scheme_Object *cell, Scheme_Thread_Cell_Table *cells, Scheme_Object *v)
{
  if (!((Thread_Cell *)cell)->assigned)
    ((Thread_Cell *)cell)->assigned = 1;
  v = scheme_make_ephemeron(cell, v);
  scheme_add_to_table(cells, (const char *)cell, (void *)v, 0);
}

Scheme_Thread_Cell_Table *scheme_empty_cell_table(void)
{
  return scheme_make_bucket_table(20, SCHEME_hash_weak_ptr);
}

static Scheme_Thread_Cell_Table *inherit_cells(Scheme_Thread_Cell_Table *cells,
					       Scheme_Thread_Cell_Table *t,
					       int inherited)
{
  Scheme_Bucket *bucket;
  Scheme_Object *cell, *v;
  int i;

  if (!cells)
    cells = scheme_current_thread->cell_values;
  
  if (!t)
    t = scheme_empty_cell_table();
  
  for (i = cells->size; i--; ) {
    bucket = cells->buckets[i];
    if (bucket && bucket->val && bucket->key) {
      cell = (Scheme_Object *)HT_EXTRACT_WEAK(bucket->key);
      if (cell && (((Thread_Cell *)cell)->inherited == inherited)) {
	v = (Scheme_Object *)bucket->val;
	scheme_add_to_table(t, (char *)cell, v, 0);
      }
    }
  }

  return t;
}

Scheme_Thread_Cell_Table *scheme_inherit_cells(Scheme_Thread_Cell_Table *cells)
{
  return inherit_cells(cells, NULL, 1);
}

static Scheme_Object *thread_cell_values(int argc, Scheme_Object *argv[])
{
  if (argc == 1) {
    Scheme_Thread_Cell_Table *naya;

    if (!SAME_TYPE(scheme_thread_cell_values_type, SCHEME_TYPE(argv[0]))) {
      scheme_wrong_contract("current-preserved-thread-cell-values", "thread-cell-values?", 0, argc, argv);
      return NULL;
    }

    naya = inherit_cells(NULL, NULL, 0);
    inherit_cells((Scheme_Thread_Cell_Table *)SCHEME_PTR_VAL(argv[0]), naya, 1);

    scheme_current_thread->cell_values = naya;

    return scheme_void;
  } else {
    Scheme_Object *o, *ht;

    ht = (Scheme_Object *)inherit_cells(NULL, NULL, 1);

    o = scheme_alloc_small_object();
    o->type = scheme_thread_cell_values_type;
    SCHEME_PTR_VAL(o) = ht;

    return o;
  }
}

static Scheme_Object *is_thread_cell_values(int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(scheme_thread_cell_values_type, SCHEME_TYPE(argv[0]))
          ? scheme_true
          : scheme_false);
}

static Scheme_Object *make_thread_cell(int argc, Scheme_Object *argv[])
{
  return scheme_make_thread_cell(argv[0], (argc > 1) && SCHEME_TRUEP(argv[1]));
}

static Scheme_Object *thread_cell_p(int argc, Scheme_Object *argv[])
{
  return (SCHEME_THREAD_CELLP(argv[0])
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *thread_cell_get(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_THREAD_CELLP(argv[0]))
    scheme_wrong_contract("thread-cell-ref", "thread-cell?", 0, argc, argv);
  return scheme_thread_cell_get(argv[0], scheme_current_thread->cell_values);
}

static Scheme_Object *thread_cell_set(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_THREAD_CELLP(argv[0]))
    scheme_wrong_contract("thread-cell-set!", "thread-cell?", 0, argc, argv);
  scheme_thread_cell_set(argv[0], scheme_current_thread->cell_values, argv[1]);
  return scheme_void;
}


/*========================================================================*/
/*                              parameters                                */
/*========================================================================*/

SHARED_OK static int max_configs = __MZCONFIG_BUILTIN_COUNT__;
static Scheme_Object *do_param(int argc, Scheme_Object *argv[], Scheme_Object *self);

static Scheme_Config *config_fail()
{
  /* in a separate function to help xform */
  scheme_longjmp(scheme_error_buf, 1);
  return NULL;
}

Scheme_Config *scheme_current_config()
{
  GC_CAN_IGNORE Scheme_Object *v;

  v = scheme_extract_one_cc_mark(NULL, scheme_parameterization_key);

  if (!SAME_TYPE(scheme_config_type, SCHEME_TYPE(v))) {
    /* Someone has grabbed parameterization-key out of #%paramz
       and misused it.
       Printing an error message requires consulting parameters,
       so just escape. */
    return config_fail();
  }

  return (Scheme_Config *)v;
}

static Scheme_Config *do_extend_config(Scheme_Config *c, Scheme_Object *key, Scheme_Object *val)
{
  Scheme_Config *naya;
  Scheme_Hash_Tree *ht;

  /* In principle, the key+cell link should be weak, but it's
     difficult to imagine a parameter being GC'ed while an active
     `parameterize' is still on the stack (or, at least, difficult to
     imagine that it matters). */

  naya = MALLOC_ONE_TAGGED(Scheme_Config);
  naya->so.type = scheme_config_type;
  ht = scheme_hash_tree_set(c->ht, key, scheme_make_thread_cell(val, 1));
  naya->ht = ht;
  naya->root = c->root;

  return naya;
}

Scheme_Config *scheme_extend_config(Scheme_Config *c, int pos, Scheme_Object *init_val)
{
  return do_extend_config(c, scheme_make_integer(pos), init_val);
}

void scheme_install_config(Scheme_Config *config)
{
  scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);
}

Scheme_Object *find_param_cell(Scheme_Config *c, Scheme_Object *k, int force_cell)
{
  Scheme_Object *v;
  Scheme_Parameterization *p;

  v = scheme_eq_hash_tree_get(c->ht, k);
  if (v)
    return v;
  
  p = c->root;
  if (SCHEME_INTP(k))
    return p->prims[SCHEME_INT_VAL(k)];
  else {
    if (p->extensions)
      return scheme_lookup_in_table(p->extensions, (const char *)k);
    else
      return NULL;
  }
}

Scheme_Object *scheme_get_thread_param(Scheme_Config *c, Scheme_Thread_Cell_Table *cells, int pos)
{
  Scheme_Object *cell;

  cell = find_param_cell(c, scheme_make_integer(pos), 0);
  return scheme_thread_cell_get(cell, cells);
}

Scheme_Object *scheme_get_param(Scheme_Config *c, int pos)
{
  return scheme_get_thread_param(c, scheme_current_thread->cell_values, pos);
}

void scheme_set_thread_param(Scheme_Config *c, Scheme_Thread_Cell_Table *cells, int pos, Scheme_Object *o)
{
  scheme_thread_cell_set(find_param_cell(c, scheme_make_integer(pos), 1), cells, o);
}

void scheme_set_param(Scheme_Config *c, int pos, Scheme_Object *o)
{
  scheme_thread_cell_set(find_param_cell(c, scheme_make_integer(pos), 1), 
			 scheme_current_thread->cell_values, o);
}

static Scheme_Parameterization *malloc_paramz()
{
  return (Scheme_Parameterization *)scheme_malloc_tagged(sizeof(Scheme_Parameterization) + 
                                                         (max_configs - mzFLEX_DELTA) * sizeof(Scheme_Object*));
}

void scheme_flatten_config(Scheme_Config *orig_c)
{
}

static Scheme_Object *parameterization_p(int argc, Scheme_Object **argv)
{
  Scheme_Object *v = argv[0];

  return (SCHEME_CONFIGP(v)
	  ? scheme_true
	  : scheme_false);
}


#define SCHEME_PARAMETERP(v) ((SCHEME_PRIMP(v) || SCHEME_CLSD_PRIMP(v)) \
                              && ((((Scheme_Primitive_Proc *)v)->pp.flags & SCHEME_PRIM_OTHER_TYPE_MASK) \
                                  == SCHEME_PRIM_TYPE_PARAMETER))

static Scheme_Object *extend_parameterization(int argc, Scheme_Object *argv[])
{
  Scheme_Object *key, *a[2], *param;
  Scheme_Config *c;
  int i;

  c = (Scheme_Config *)argv[0];

  if (argc < 2) {
    scheme_flatten_config(c);
  } else if (SCHEME_CONFIGP(c) && (argc & 1)) {
    for (i = 1; i < argc; i += 2) {
      param = argv[i];
      if (!SCHEME_PARAMETERP(param)
          && !(SCHEME_CHAPERONEP(param) && SCHEME_PARAMETERP(SCHEME_CHAPERONE_VAL(param)))) {
        a[0] = param;
	scheme_wrong_contract("parameterize", "parameter?", -2, 1, a);
	return NULL;
      }
      key = argv[i + 1];
      if (SCHEME_CHAPERONEP(param)) {
        a[0] = key;
        key = scheme_apply_chaperone(param, 1, a, scheme_void, 0);
        param = SCHEME_CHAPERONE_VAL(param);
      }
      a[0] = key;
      a[1] = scheme_false;
      while (1) {
        if (!(((Scheme_Primitive_Proc *)param)->pp.flags & SCHEME_PRIM_IS_CLOSURE)) {
          Scheme_Prim *proc;
          proc = (Scheme_Prim *)((Scheme_Primitive_Proc *)param)->prim_val;
          key = proc(2, a); /* leads to scheme_param_config to set a[1] */
          break;
        } else {
          /* sets a[1] */
          key = do_param(2, a, param);
          if (SCHEME_PARAMETERP(key)) {
            param = key;
            a[0] = a[1];
          } else
            break;
        }
      }
      c = do_extend_config(c, key, a[1]);
    }
  }

  return (Scheme_Object *)c;
}

static Scheme_Object *reparameterize(int argc, Scheme_Object **argv)
{
  /* Clones values of all built-in parameters in a new parameterization.
     This could be implemented in Racket by enumerating all built-in parameters,
     but it's easier and faster here. We need this for the Planet resolver. */
  Scheme_Config *c, *naya;
  Scheme_Parameterization *pz, *npz;
  Scheme_Object *v;
  Scheme_Hash_Tree *ht;
  int i;

  if (!SCHEME_CONFIGP(argv[0]))
    scheme_wrong_contract("reparameterize", "parameterization?", 0, argc, argv);
  
  c = (Scheme_Config *)argv[0];
  scheme_flatten_config(c);

  pz = c->root;
  npz = malloc_paramz();
  memcpy(npz, pz, sizeof(Scheme_Parameterization));

  naya = MALLOC_ONE_TAGGED(Scheme_Config);
  naya->so.type = scheme_config_type;
  ht = scheme_make_hash_tree(0);
  naya->ht = ht;
  naya->root = npz;

  for (i = 0; i < max_configs; i++) {
    v = scheme_thread_cell_get(pz->prims[i], scheme_current_thread->cell_values);
    v = scheme_make_thread_cell(v, 1);
    npz->prims[i] = v;
  }

  return (Scheme_Object *)naya;
}

static Scheme_Object *parameter_p(int argc, Scheme_Object **argv)
{
  Scheme_Object *v = argv[0];

  if (SCHEME_CHAPERONEP(v)) v = SCHEME_CHAPERONE_VAL(v);

  return (SCHEME_PARAMETERP(v)
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *do_param(int argc, Scheme_Object *argv[], Scheme_Object *self)
{
  Scheme_Object *guard, **argv2, *pos[2];
    ParamData *data = (ParamData *)SCHEME_PRIM_CLOSURE_ELS(self)[0];

  if (argc && argv[0]) {
    guard = data->guard;
    if (guard) {
      Scheme_Object *v;
      
      v = scheme_apply(guard, 1, argv);

      if (argc == 2) {
	/* Special hook for parameterize: */
	argv[1] = v;
	return data->key;
      }

      argv2 = MALLOC_N(Scheme_Object *, argc);
      memcpy(argv2, argv, argc * sizeof(Scheme_Object *));
      argv2[0] = v;
    } else if (argc == 2) {
      /* Special hook for parameterize: */
      argv[1] = argv[0];
      return data->key;
    } else
      argv2 = argv;
  } else
    argv2 = argv;

  if (data->is_derived) {
    if (!argc) {
      Scheme_Object *v;
      v = _scheme_apply(data->key, argc, argv2);
      pos[0] = v;
      return _scheme_tail_apply(data->extract_guard, 1, pos);
    } else {
      return _scheme_tail_apply(data->key, argc, argv2);
    }
  }

  pos[0] = data->key;
  pos[1] = data->defcell;

  return scheme_param_config("parameter-procedure", 
			     (Scheme_Object *)(void *)pos,
			     argc, argv2,
			     -2, NULL, NULL, 0);
}

static Scheme_Object *extract_param(Scheme_Config *config, Scheme_Object *key, Scheme_Object *defcell)
{
  Scheme_Object *cell;
  
  cell = find_param_cell(config, key, 0);
  if (!cell)
    cell = defcell;
  
  if (SCHEME_THREAD_CELLP(cell))
    return scheme_thread_cell_get(cell, scheme_current_thread->cell_values);
  else
    return cell; /* it's really the value, instead of a cell */
}

static Scheme_Object *do_param_fast(int argc, Scheme_Object *argv[], Scheme_Object *self)
{
  ParamData *data = (ParamData *)SCHEME_PRIM_CLOSURE_ELS(self)[0];

  if (!argc && !data->is_derived)
    return extract_param(scheme_current_config(), data->key, data->defcell);

  return do_param(argc, argv, self);
}

static Scheme_Object *make_parameter(int argc, Scheme_Object **argv)
{
  Scheme_Object *p, *cell, *a[1];
  ParamData *data;
  void *k;

  k = scheme_make_pair(scheme_true, scheme_false); /* generates a key */

  if (argc > 1)
    scheme_check_proc_arity("make-parameter", 1, 1, argc, argv);

  data = MALLOC_ONE_RT(ParamData);
#ifdef MZTAG_REQUIRED
  data->type = scheme_rt_param_data;
#endif
  data->key = (Scheme_Object *)k;
  cell = scheme_make_thread_cell(argv[0], 1);
  data->defcell = cell;
  data->guard = ((argc > 1) ? argv[1] : NULL);

  a[0] = (Scheme_Object *)data;
  p = scheme_make_prim_closure_w_arity(do_param_fast, 1, a, 
                                       "parameter-procedure", 0, 1);
  ((Scheme_Primitive_Proc *)p)->pp.flags |= SCHEME_PRIM_TYPE_PARAMETER;

  return p;
}

static Scheme_Object *make_derived_parameter(int argc, Scheme_Object **argv)
{
  Scheme_Object *p, *a[1];
  ParamData *data;

  if (!SCHEME_PARAMETERP(argv[0]))
    scheme_wrong_contract("make-derived-parameter", "(and/c parameter? (not/c impersonator?))", 0, argc, argv);

  scheme_check_proc_arity("make-derived-parameter", 1, 1, argc, argv);
  scheme_check_proc_arity("make-derived-parameter", 1, 2, argc, argv);

  data = MALLOC_ONE_RT(ParamData);
#ifdef MZTAG_REQUIRED
  data->type = scheme_rt_param_data;
#endif
  data->is_derived = 1;
  data->key = argv[0];
  data->guard = argv[1];
  data->extract_guard = argv[2];

  a[0] = (Scheme_Object *)data;
  p = scheme_make_prim_closure_w_arity(do_param, 1, a, 
                                       "parameter-procedure", 0, 1);
  ((Scheme_Primitive_Proc *)p)->pp.flags |= SCHEME_PRIM_TYPE_PARAMETER;

  return p;
}

static Scheme_Object *parameter_procedure_eq(int argc, Scheme_Object **argv)
{
  Scheme_Object *a, *b;

  a = argv[0];
  b = argv[1];

  if (SCHEME_CHAPERONEP(a)) a = SCHEME_CHAPERONE_VAL(a);
  if (SCHEME_CHAPERONEP(b)) b = SCHEME_CHAPERONE_VAL(b);

  if (!SCHEME_PARAMETERP(a))
    scheme_wrong_contract("parameter-procedure=?", "parameter?", 0, argc, argv);
  if (!SCHEME_PARAMETERP(b))
    scheme_wrong_contract("parameter-procedure=?", "parameter?", 1, argc, argv);

  return (SAME_OBJ(a, b)
	  ? scheme_true
	  : scheme_false);
}

void scheme_set_command_line_arguments(Scheme_Object *vec)
{
  if (!initial_cmdline_vec)
    REGISTER_SO(initial_cmdline_vec);
  initial_cmdline_vec = vec;
}

int scheme_new_param(void)
{
  return max_configs++;
}

static void init_param(Scheme_Thread_Cell_Table *cells,
		       Scheme_Parameterization *params,
		       int pos,
		       Scheme_Object *v)
{
  Scheme_Object *cell;
  cell = scheme_make_thread_cell(v, 1);
  params->prims[pos] = cell;
}

void scheme_set_root_param(int p, Scheme_Object *v)
{
  Scheme_Parameterization *paramz;
  paramz = scheme_current_thread->init_config->root;
  ((Thread_Cell *)(paramz->prims[p]))->def_val = v;
}

static void make_initial_config(Scheme_Thread *p)
{
  Scheme_Thread_Cell_Table *cells;
  Scheme_Parameterization *paramz;
  Scheme_Config *config;

  cells = scheme_make_bucket_table(5, SCHEME_hash_weak_ptr);
  p->cell_values = cells;

  paramz = malloc_paramz();
#ifdef MZTAG_REQUIRED
  paramz->type = scheme_rt_parameterization;
#endif

  config = MALLOC_ONE_TAGGED(Scheme_Config);
  config->so.type = scheme_config_type;
  config->root = paramz;
  {
    Scheme_Hash_Tree *ht;
    ht = scheme_make_hash_tree(0);
    config->ht = ht;
  }

  p->init_config = config;

  init_param(cells, paramz, MZCONFIG_READTABLE, scheme_make_default_readtable());
  
  init_param(cells, paramz, MZCONFIG_CAN_READ_GRAPH, scheme_true);
  init_param(cells, paramz, MZCONFIG_CAN_READ_COMPILED, scheme_false);
  init_param(cells, paramz, MZCONFIG_CAN_READ_BOX, scheme_true);
  init_param(cells, paramz, MZCONFIG_CAN_READ_PIPE_QUOTE, scheme_true);
  init_param(cells, paramz, MZCONFIG_CAN_READ_DOT, scheme_true);
  init_param(cells, paramz, MZCONFIG_CAN_READ_INFIX_DOT, scheme_true);
  init_param(cells, paramz, MZCONFIG_CAN_READ_QUASI, scheme_true);
  init_param(cells, paramz, MZCONFIG_READ_DECIMAL_INEXACT, scheme_true);
  init_param(cells, paramz, MZCONFIG_CAN_READ_READER, scheme_false);
  init_param(cells, paramz, MZCONFIG_CAN_READ_LANG, scheme_true);
  init_param(cells, paramz, MZCONFIG_LOAD_DELAY_ENABLED, init_load_on_demand ? scheme_true : scheme_false);
  init_param(cells, paramz, MZCONFIG_DELAY_LOAD_INFO, scheme_false);

  init_param(cells, paramz, MZCONFIG_PRINT_GRAPH, scheme_false);
  init_param(cells, paramz, MZCONFIG_PRINT_STRUCT, scheme_true);
  init_param(cells, paramz, MZCONFIG_PRINT_BOX, scheme_true);
  init_param(cells, paramz, MZCONFIG_PRINT_VEC_SHORTHAND, scheme_false);
  init_param(cells, paramz, MZCONFIG_PRINT_HASH_TABLE, scheme_true);
  init_param(cells, paramz, MZCONFIG_PRINT_UNREADABLE, scheme_true);
  init_param(cells, paramz, MZCONFIG_PRINT_PAIR_CURLY, scheme_false);
  init_param(cells, paramz, MZCONFIG_PRINT_MPAIR_CURLY, scheme_true);
  init_param(cells, paramz, MZCONFIG_PRINT_READER, scheme_false);
  init_param(cells, paramz, MZCONFIG_PRINT_LONG_BOOLEAN, scheme_false);
  init_param(cells, paramz, MZCONFIG_PRINT_AS_QQ, scheme_true);
  init_param(cells, paramz, MZCONFIG_PRINT_SYNTAX_WIDTH, scheme_make_integer(32));

  init_param(cells, paramz, MZCONFIG_COMPILE_MODULE_CONSTS, scheme_true);
  init_param(cells, paramz, MZCONFIG_USE_JIT, scheme_startup_use_jit ? scheme_true : scheme_false);

  {
    Scheme_Object *s;
    s = scheme_make_immutable_sized_utf8_string("", 0);
    init_param(cells, paramz, MZCONFIG_LOCALE, s);
  }

  init_param(cells, paramz, MZCONFIG_CASE_SENS, (scheme_case_sensitive ? scheme_true : scheme_false));
  init_param(cells, paramz, MZCONFIG_SQUARE_BRACKETS_ARE_PARENS, (scheme_square_brackets_are_parens
								 ? scheme_true : scheme_false));
  init_param(cells, paramz, MZCONFIG_CURLY_BRACES_ARE_PARENS, (scheme_curly_braces_are_parens
							      ? scheme_true : scheme_false));

  init_param(cells, paramz, MZCONFIG_ERROR_PRINT_WIDTH, scheme_make_integer(256));
  init_param(cells, paramz, MZCONFIG_ERROR_PRINT_CONTEXT_LENGTH, scheme_make_integer(16));
  init_param(cells, paramz, MZCONFIG_ERROR_PRINT_SRCLOC, scheme_true);

  REGISTER_SO(main_custodian);
  REGISTER_SO(last_custodian);
  REGISTER_SO(limited_custodians);
  main_custodian = scheme_make_custodian(NULL);
#ifdef MZ_PRECISE_GC
  GC_register_root_custodian(main_custodian);
#endif
  last_custodian = main_custodian;
  init_param(cells, paramz, MZCONFIG_CUSTODIAN, (Scheme_Object *)main_custodian);

  REGISTER_SO(initial_plumber);
  initial_plumber = (Scheme_Plumber *)make_plumber(0, NULL);
  init_param(cells, paramz, MZCONFIG_PLUMBER, (Scheme_Object *)initial_plumber);

  init_param(cells, paramz, MZCONFIG_ALLOW_SET_UNDEFINED, (scheme_allow_set_undefined
							  ? scheme_true
							  : scheme_false));

  init_param(cells, paramz, MZCONFIG_COLLECTION_PATHS,  scheme_null);
  init_param(cells, paramz, MZCONFIG_COLLECTION_LINKS,  scheme_null);

  {
    Scheme_Security_Guard *sg;

    sg = MALLOC_ONE_TAGGED(Scheme_Security_Guard);
    sg->so.type = scheme_security_guard_type;
    init_param(cells, paramz, MZCONFIG_SECURITY_GUARD, (Scheme_Object *)sg);
  }

  {
    Scheme_Object *s;
    char *pwd;
    s = scheme_make_path(scheme_os_getcwd(NULL, 0, NULL, 1));
    s = scheme_path_to_directory_path(s);
    init_param(cells, paramz, MZCONFIG_CURRENT_DIRECTORY, s);
#ifndef DOS_FILE_SYSTEM
    pwd = scheme_getenv("PWD");
    if (pwd) {
      Scheme_Object *id1, *id2, *a[2];
      id1 = scheme_get_fd_identity(NULL, 0, pwd, 1);
      if (id1) {
        id2 = scheme_get_fd_identity(NULL, 0, SCHEME_PATH_VAL(s), 1);
        if (id2 && scheme_eqv(id1, id2)) {
          s = scheme_make_path(pwd);
          a[0] = s;
          a[1] = scheme_true;
          s = scheme_simplify_path(2, a);
          s = scheme_path_to_directory_path(s);
          init_param(cells, paramz, MZCONFIG_CURRENT_DIRECTORY, s);
        }
      }
    }
#endif
    init_param(cells, paramz, MZCONFIG_CURRENT_USER_DIRECTORY, s);
    scheme_set_original_dir(s);
  }

  {
    Scheme_Object *ev;
    ev = scheme_make_environment_variables(NULL);
    init_param(cells, paramz, MZCONFIG_CURRENT_ENV_VARS, ev);
  }

  {
    Scheme_Object *rs;
    rs = scheme_make_random_state(scheme_get_milliseconds());
    init_param(cells, paramz, MZCONFIG_RANDOM_STATE, rs);
    rs = scheme_make_random_state(scheme_get_milliseconds());
    init_param(cells, paramz, MZCONFIG_SCHEDULER_RANDOM_STATE, rs);
  }

  {
    Scheme_Object *eh;
    eh = scheme_make_prim_w_arity2(scheme_default_eval_handler,
				   "default-eval-handler",
				   1, 1,
				   0, -1);
    init_param(cells, paramz, MZCONFIG_EVAL_HANDLER, eh);
  }
  
  {
    Scheme_Object *eh;
    eh = scheme_make_prim_w_arity(scheme_default_compile_handler,
				  "default-compile-handler",
				  2, 2);
    init_param(cells, paramz, MZCONFIG_COMPILE_HANDLER, eh);
  }
  
  {
    Scheme_Object *ph;

    ph = scheme_make_prim_w_arity(scheme_default_print_handler,
				  "default-print-handler",
				  1, 1);
    init_param(cells, paramz, MZCONFIG_PRINT_HANDLER, ph);

    ph = scheme_make_prim_w_arity(scheme_default_prompt_read_handler,
                                  "default-prompt-read-handler",
                                  0, 0);
    init_param(cells, paramz, MZCONFIG_PROMPT_READ_HANDLER, ph);

    ph = scheme_make_prim_w_arity(scheme_default_read_input_port_handler,
                                  "default-get-interaction-input-port",
                                  0, 0);
    init_param(cells, paramz, MZCONFIG_READ_INPUT_PORT_HANDLER, ph);

    ph = scheme_make_prim_w_arity(scheme_default_read_handler,
                                  "default-read-interaction-handler",
                                  2, 2);
    init_param(cells, paramz, MZCONFIG_READ_HANDLER, ph);
  }
  init_param(cells, paramz, MZCONFIG_PORT_COUNT_LINES, scheme_false);

  {
    Scheme_Object *lh;
    lh = scheme_make_prim_w_arity2(scheme_default_load_extension,
				   "default-load-extension-handler",
				   2, 2,
				   0, -1);
    init_param(cells, paramz, MZCONFIG_LOAD_EXTENSION_HANDLER, lh);
  }

  {
    Scheme_Object *ins = initial_inspector;
    init_param(cells, paramz, MZCONFIG_INSPECTOR, ins);
    init_param(cells, paramz, MZCONFIG_CODE_INSPECTOR, ins);
  }
  
  {
    Scheme_Object *zlv;
    if (initial_cmdline_vec)
      zlv = initial_cmdline_vec;
    else
      zlv = scheme_make_vector(0, NULL);
    init_param(cells, paramz, MZCONFIG_CMDLINE_ARGS, zlv);
  }

  {
    Scheme_Thread_Set *t_set;
    t_set = create_thread_set(NULL);
    init_param(cells, paramz, MZCONFIG_THREAD_SET, (Scheme_Object *)t_set);
  }
  
  init_param(cells, paramz, MZCONFIG_THREAD_INIT_STACK_SIZE, scheme_make_integer(DEFAULT_INIT_STACK_SIZE));

  {
    int i;
    for (i = 0; i < max_configs; i++) {
      if (!paramz->prims[i])
	init_param(cells, paramz, i, scheme_false);      
    }
  }

  initial_config = config;
}

Scheme_Config *scheme_minimal_config(void)
{
  return initial_config;
}

void scheme_set_startup_load_on_demand(int on)
{
  init_load_on_demand = on;
}

Scheme_Object *scheme_register_parameter(Scheme_Prim *function, char *name, int which)
{
  Scheme_Object *o;

  if (!config_map) {
    REGISTER_SO(config_map);
    config_map = MALLOC_N(Scheme_Object*, max_configs);
  }

  if (config_map[which])
    return config_map[which];

  o = scheme_make_prim_w_arity(function, name, 0, 1);
  ((Scheme_Primitive_Proc *)o)->pp.flags |= SCHEME_PRIM_TYPE_PARAMETER;

  config_map[which] = o;

  return o;
}

typedef Scheme_Object *(*PCheck_Proc)(int, Scheme_Object **, Scheme_Config *);

static Scheme_Object *do_param_config(char *name, Scheme_Object *pos,
                                      int argc, Scheme_Object **argv,
                                      int arity,
                                      /* -3 => like -1, plus use check to unmarshall the value
                                         -2 => user parameter; pos is array [key, defcell]
                                         -1 => use check; if isboolorfilter, check is a filter
                                         (and expected is ignored), and if check is NULL,
                                         parameter is boolean-valued
                                         0+ => check argument for this arity */
                                      Scheme_Object *(*check)(int, Scheme_Object **), 
                                      /* Actually called with (int, S_O **, Scheme_Config *) */
                                      char *expected,
                                      int isboolorfilter,
                                      int expected_is_contract)
{
  Scheme_Config *config;

  config = scheme_current_config();

  if (argc == 0) {
    if (arity == -2) {
      return extract_param(config, ((Scheme_Object **)pos)[0], ((Scheme_Object **)pos)[1]);
    } else {
      Scheme_Object *s;
      s = scheme_get_param(config, SCHEME_INT_VAL(pos));
      if (arity == -3) {
	Scheme_Object *a[1];
	PCheck_Proc checkp = (PCheck_Proc)check;
	a[0] = s;
	s = checkp(1, a, config);
      }
      return s;
    }
  } else {
    Scheme_Object *naya = argv[0];

    if (arity != -2) {
      if (arity < 0) {
	if (check) {
	  PCheck_Proc checkp = (PCheck_Proc)check;
	  Scheme_Object *r;

	  r = checkp(1, argv, config);
	  
	  if (!isboolorfilter && SCHEME_FALSEP(r))
	    r = NULL;
	  
	  if (!r) {
            if (expected_is_contract)
              scheme_wrong_contract(name, expected, 0, 1, argv);
            else
              scheme_wrong_type(name, expected, 0, 1, argv);
	    return NULL;
	  }
	  
	  if (isboolorfilter)
	    naya = r;
	}
      } else 
	scheme_check_proc_arity(name, arity, 0, argc, argv);

      if (isboolorfilter && !check)
	naya = ((SCHEME_TRUEP(naya)) ? scheme_true : scheme_false);

      if (argc == 2) {
	/* Special hook for parameterize: */
	argv[1] = naya;
	return pos;
      } else
	scheme_set_param(config, SCHEME_INT_VAL(pos), naya);
    } else {
      Scheme_Object *cell;
      
      cell = find_param_cell(config, ((Scheme_Object **)pos)[0], 1);
      if (!cell)
	cell = ((Scheme_Object **)pos)[1];

      scheme_thread_cell_set(cell, scheme_current_thread->cell_values, naya);
    }
  
    return scheme_void;
  }
}

Scheme_Object *scheme_param_config(char *name, Scheme_Object *pos,
				   int argc, Scheme_Object **argv,
				   int arity,
				   Scheme_Object *(*check)(int, Scheme_Object **), 
				   char *expected_type,
				   int isboolorfilter)
{
  return do_param_config(name, pos, argc, argv, arity, check, 
                         expected_type, isboolorfilter, 0);
}

Scheme_Object *scheme_param_config2(char *name, Scheme_Object *pos,
                                    int argc, Scheme_Object **argv,
                                    int arity,
                                    Scheme_Object *(*check)(int, Scheme_Object **), 
                                    char *expected_contract,
                                    int isboolorfilter)
{
  return do_param_config(name, pos, argc, argv, arity, check, 
                         expected_contract, isboolorfilter, 1);
}

static Scheme_Object *
exact_positive_integer_p (int argc, Scheme_Object *argv[])
{
  Scheme_Object *n = argv[argc-1];
  if (SCHEME_INTP(n) && (SCHEME_INT_VAL(n) > 0))
    return scheme_true;
  if (SCHEME_BIGNUMP(n) && SCHEME_BIGPOS(n))
    return scheme_true;

  return scheme_false;
}

static Scheme_Object *current_thread_initial_stack_size(int argc, Scheme_Object *argv[])
{
  return scheme_param_config2("current-thread-initial-stack-size", 
                              scheme_make_integer(MZCONFIG_THREAD_INIT_STACK_SIZE),
                              argc, argv,
                              -1, exact_positive_integer_p, "exact-positive-integer?", 0);
}

static Scheme_Object *phantom_bytes_p(int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_phantom_bytes_type)
          ? scheme_true
          : scheme_false);
}

static Scheme_Object *make_phantom_bytes(int argc, Scheme_Object *argv[])
{
  Scheme_Phantom_Bytes *pb;

  if (!scheme_nonneg_exact_p(argv[0]))
    scheme_wrong_contract("make-phantom-bytes", "exact-nonnegative-integer?", 0, argc, argv);

  if (!SCHEME_INTP(argv[0]))
    scheme_raise_out_of_memory("make-phantom-bytes", NULL);

  pb = MALLOC_ONE_TAGGED(Scheme_Phantom_Bytes);
  pb->so.type = scheme_phantom_bytes_type;
  pb->size = SCHEME_INT_VAL(argv[0]);

# ifdef MZ_PRECISE_GC
  if (!GC_allocate_phantom_bytes(pb->size))
    scheme_raise_out_of_memory("make-phantom-bytes", NULL);
# endif

  return (Scheme_Object *)pb;
}

static Scheme_Object *set_phantom_bytes(int argc, Scheme_Object *argv[])
{
  Scheme_Phantom_Bytes *pb;
  intptr_t amt;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_phantom_bytes_type))
    scheme_wrong_contract("set-phantom-bytes!", "phantom-bytes?", 0, argc, argv);
  if (!scheme_nonneg_exact_p(argv[1]))
    scheme_wrong_contract("set-phantom-bytes!", "exact-nonnegative-integer?", 1, argc, argv);

  pb = (Scheme_Phantom_Bytes *)argv[0];
  amt = SCHEME_INT_VAL(argv[1]);

# ifdef MZ_PRECISE_GC
  if (!GC_allocate_phantom_bytes(amt - pb->size))
    scheme_raise_out_of_memory("make-phantom-bytes", NULL);
# endif

  pb->size = amt;

  return scheme_void;
}

/*========================================================================*/
/*                              namespaces                                */
/*========================================================================*/

Scheme_Env *scheme_get_env(Scheme_Config *c)
  XFORM_SKIP_PROC
{
  Scheme_Object *o;

  if (!c)
    c = scheme_current_config();

  o = scheme_get_param(c, MZCONFIG_ENV);
  return (Scheme_Env *)o;
}

Scheme_Object *scheme_make_namespace(int argc, Scheme_Object *argv[])
{
  Scheme_Env *genv, *env;
  intptr_t phase;

  genv = scheme_get_env(NULL);
  env = scheme_make_empty_env();
  
  for (phase = genv->phase; phase--; ) {
    scheme_prepare_exp_env(env);
    env = env->exp_env;
  }

  return (Scheme_Object *)env;
}

static Scheme_Object *namespace_p(int argc, Scheme_Object **argv)
{
  return ((SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_namespace_type)) 
	  ? scheme_true 
	  : scheme_false);
}

static Scheme_Object *current_namespace(int argc, Scheme_Object *argv[])
{
  return scheme_param_config2("current-namespace", 
                              scheme_make_integer(MZCONFIG_ENV),
                              argc, argv,
                              -1, namespace_p, "namespace?", 0);
}

/*========================================================================*/
/*                           security guards                              */
/*========================================================================*/

static Scheme_Object *make_security_guard(int argc, Scheme_Object *argv[])
{
  Scheme_Security_Guard *sg;

  if (!(SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_security_guard_type)))
    scheme_wrong_contract("make-security-guard", "security-guard?", 0, argc, argv);
  scheme_check_proc_arity("make-security-guard", 3, 1, argc, argv);
  scheme_check_proc_arity("make-security-guard", 4, 2, argc, argv);
  if (argc > 3)
    scheme_check_proc_arity2("make-security-guard", 3, 3, argc, argv, 1);

  sg = MALLOC_ONE_TAGGED(Scheme_Security_Guard);
  sg->so.type = scheme_security_guard_type;
  sg->parent = (Scheme_Security_Guard *)argv[0];
  sg->file_proc = argv[1];
  sg->network_proc = argv[2];
  if ((argc > 3) && SCHEME_TRUEP(argv[3]))
    sg->link_proc = argv[3];

  return (Scheme_Object *)sg;
}

static Scheme_Object *security_guard_p(int argc, Scheme_Object *argv[])
{
  return ((SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_security_guard_type)) 
	  ? scheme_true 
	  : scheme_false);
}

static Scheme_Object *current_security_guard(int argc, Scheme_Object *argv[])
{
  return scheme_param_config2("current-security-guard", 
                              scheme_make_integer(MZCONFIG_SECURITY_GUARD),
                              argc, argv,
                              -1, security_guard_p, "security-guard?", 0);
}


void scheme_security_check_file(const char *who, const char *filename, int guards)
{
  Scheme_Security_Guard *sg;

  sg = (Scheme_Security_Guard *)scheme_get_param(scheme_current_config(), MZCONFIG_SECURITY_GUARD);

  if (sg->file_proc) {
    Scheme_Object *l = scheme_null, *a[3];

    if (guards & SCHEME_GUARD_FILE_EXISTS)
      l = scheme_make_pair(exists_symbol, l);
    if (guards & SCHEME_GUARD_FILE_DELETE)
      l = scheme_make_pair(delete_symbol, l);
    if (guards & SCHEME_GUARD_FILE_EXECUTE)
      l = scheme_make_pair(execute_symbol, l);
    if (guards & SCHEME_GUARD_FILE_WRITE)
      l = scheme_make_pair(write_symbol, l);
    if (guards & SCHEME_GUARD_FILE_READ)
      l = scheme_make_pair(read_symbol, l);

    a[0] = scheme_intern_symbol(who);
    a[1] = (filename ? scheme_make_sized_path((char *)filename, -1, 1) : scheme_false);
    a[2] = l;

    while (sg->parent) {
      scheme_apply(sg->file_proc, 3, a);
      sg = sg->parent;
    }
  }
}

void scheme_security_check_file_link(const char *who, const char *filename, const char *content)
{
  Scheme_Security_Guard *sg;

  sg = (Scheme_Security_Guard *)scheme_get_param(scheme_current_config(), MZCONFIG_SECURITY_GUARD);

  if (sg->file_proc) {
    Scheme_Object *a[3];

    a[0] = scheme_intern_symbol(who);
    a[1] = scheme_make_sized_path((char *)filename, -1, 1);
    a[2] = scheme_make_sized_path((char *)content, -1, 1);

    while (sg->parent) {
      if (sg->link_proc)
	scheme_apply(sg->link_proc, 3, a);
      else {
	scheme_signal_error("%s: security guard does not allow any link operation; attempted from: %s to: %s",
			    who,
			    filename,
			    content);
      }
      sg = sg->parent;
    }
  }
}

void scheme_security_check_network(const char *who, const char *host, int port, int client)
{
  Scheme_Security_Guard *sg;

  sg = (Scheme_Security_Guard *)scheme_get_param(scheme_current_config(), MZCONFIG_SECURITY_GUARD);

  if (sg->network_proc) {
    Scheme_Object *a[4];

    a[0] = scheme_intern_symbol(who);
    a[1] = (host ? scheme_make_sized_utf8_string((char *)host, -1) : scheme_false);
    a[2] = ((port < 1) ? scheme_false : scheme_make_integer(port));
    a[3] = (client ? client_symbol : server_symbol);

    while (sg->parent) {
      scheme_apply(sg->network_proc, 4, a);
      sg = sg->parent;
    }
  }
}

/*========================================================================*/
/*                         wills and will executors                       */
/*========================================================================*/

typedef struct ActiveWill {
  MZTAG_IF_REQUIRED
  Scheme_Object *o;
  Scheme_Object *proc;
  struct WillExecutor *w;  /* Set to will executor when executed */
  struct ActiveWill *next;
} ActiveWill;

typedef struct WillExecutor {
  Scheme_Object so;
  Scheme_Object *sema;
  ActiveWill *first, *last;
  int is_stubborn;
} WillExecutor;

static void activate_will(void *o, void *data) 
{
  ActiveWill *a;
  WillExecutor *w;
  Scheme_Object *proc;

  if (SCHEME_PAIRP(data)) {
    w = (WillExecutor *)SCHEME_CAR(data);
    proc = SCHEME_CDR(data);
  } else {
    w = (WillExecutor *)scheme_ephemeron_key(data);
    proc = scheme_ephemeron_value(data);
  }

  if (w) {
    a = MALLOC_ONE_RT(ActiveWill);
#ifdef MZTAG_REQUIRED
    a->type = scheme_rt_will;
#endif
    a->o = (Scheme_Object *)o;
    a->proc = proc;
  
    if (w->last)
      w->last->next = a;
    else
      w->first = a;
    w->last = a;
    scheme_post_sema(w->sema);
  }
}

static Scheme_Object *do_next_will(WillExecutor *w)
{
  ActiveWill *a;
  Scheme_Object *o[1];

  a = w->first;
  w->first = a->next;
  if (!w->first)
    w->last = NULL;
  
  o[0] = a->o;
  a->o = NULL;

  return scheme_apply_multi(a->proc, 1, o);
}

static Scheme_Object *make_will_executor(int argc, Scheme_Object **argv)
{
  WillExecutor *w;
  Scheme_Object *sema;

  w = MALLOC_ONE_TAGGED(WillExecutor);
  sema = scheme_make_sema(0);

  w->so.type = scheme_will_executor_type;
  w->first = NULL;
  w->last = NULL;
  w->sema = sema;
  w->is_stubborn = 0;

  return (Scheme_Object *)w;
}

Scheme_Object *scheme_make_stubborn_will_executor()
{
  WillExecutor *w;

  w = (WillExecutor *)make_will_executor(0, NULL);
  w->is_stubborn = 1;

  return (Scheme_Object *)w;
}

static Scheme_Object *will_executor_p(int argc, Scheme_Object **argv)
{
  return ((SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_will_executor_type)) 
	  ? scheme_true 
	  : scheme_false);
}

static Scheme_Object *register_will(int argc, Scheme_Object **argv)
{
  Scheme_Object *e;

  if (NOT_SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_will_executor_type))
    scheme_wrong_contract("will-register", "will-executor?", 0, argc, argv);
  scheme_check_proc_arity("will-register", 1, 2, argc, argv);

  if (((WillExecutor *)argv[0])->is_stubborn) {
    e = scheme_make_pair(argv[0], argv[2]);
    scheme_add_finalizer(argv[1], activate_will, e);
  } else {
    /* If we lose track of the will executor, then drop the finalizer. */
    e = scheme_make_ephemeron(argv[0], argv[2]);
    scheme_add_scheme_finalizer(argv[1], activate_will, e);
  }


  return scheme_void;
}

static Scheme_Object *will_executor_try(int argc, Scheme_Object **argv)
{
  WillExecutor *w;

  if (NOT_SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_will_executor_type))
    scheme_wrong_contract("will-try-execute", "will-executor?", 0, argc, argv);
  
  w = (WillExecutor *)argv[0];

  if (scheme_wait_sema(w->sema, 1))
    return do_next_will(w);
  else
    return scheme_false;
}

static Scheme_Object *will_executor_go(int argc, Scheme_Object **argv)
{
  WillExecutor *w;

  if (NOT_SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_will_executor_type))
    scheme_wrong_contract("will-execute", "will-executor?", 0, argc, argv);
  
  w = (WillExecutor *)argv[0];

  scheme_wait_sema(w->sema, 0);

  return do_next_will(w);
}

static Scheme_Object *will_executor_sema(Scheme_Object *w, int *repost)
{
  *repost = 1;
  return ((WillExecutor *)w)->sema;
}

/*========================================================================*/
/*                         GC preparation and timing                      */
/*========================================================================*/

typedef struct Scheme_GC_Pre_Post_Callback_Desc {
  /* All pointer fields => allocate with GC_malloc() */
  Scheme_Object *boxed_key;
  Scheme_Object *pre_desc;
  Scheme_Object *post_desc;
  struct Scheme_GC_Pre_Post_Callback_Desc *prev;
  struct Scheme_GC_Pre_Post_Callback_Desc *next;
} Scheme_GC_Pre_Post_Callback_Desc;


Scheme_Object *scheme_add_gc_callback(Scheme_Object *pre, Scheme_Object *post)
{
  Scheme_GC_Pre_Post_Callback_Desc *desc;
  Scheme_Object *key, *boxed;

  desc = (Scheme_GC_Pre_Post_Callback_Desc *)GC_malloc(sizeof(Scheme_GC_Pre_Post_Callback_Desc));
  desc->pre_desc = pre;
  desc->post_desc = post;

  key = scheme_make_vector(1, scheme_false);
  boxed = scheme_make_weak_box(key);
  desc->boxed_key = boxed;

  desc->next = gc_prepost_callback_descs;
  gc_prepost_callback_descs = desc;
  
  return key;
}

void scheme_remove_gc_callback(Scheme_Object *key)
{
  Scheme_GC_Pre_Post_Callback_Desc *prev = NULL, *desc;

  desc = gc_prepost_callback_descs; 
  while (desc) {
    if (SAME_OBJ(SCHEME_WEAK_BOX_VAL(desc->boxed_key), key)) {
      if (prev)
        prev->next = desc->next;
      else
        gc_prepost_callback_descs = desc->next;
      if (desc->next)
        desc->next->prev = desc->prev;
    }
    prev = desc;
    desc = desc->next;
  }
}

#if defined(_MSC_VER) || defined(__MINGW32__)
# define mzOSAPI WINAPI
#else
# define mzOSAPI /* empty */
#endif

typedef void (*gccb_Ptr_Ptr_Ptr_Int_to_Void)(void*, void*, void*, int);
typedef void (*gccb_Ptr_Ptr_Ptr_to_Void)(void*, void*, void*);
typedef void (*gccb_Ptr_Ptr_Float_to_Void)(void*, void*, float);
typedef void (*gccb_Ptr_Ptr_Double_to_Void)(void*, void*, double);
typedef void (*gccb_Ptr_Ptr_Ptr_Nine_Ints)(void*,void*,void*,int,int,int,int,int,int,int,int,int);
typedef void (mzOSAPI *gccb_OSapi_Ptr_Int_to_Void)(void*, int);
typedef void (mzOSAPI *gccb_OSapi_Ptr_Ptr_to_Void)(void*, void*);
typedef void (mzOSAPI *gccb_OSapi_Ptr_Four_Ints_Ptr_Int_Int_Long_to_Void)(void*, int, int, int, int, 
                                                                          void*, int, int, long);

#ifdef DONT_USE_FOREIGN
# define scheme_extract_pointer(x) NULL
#endif

static void run_gc_callbacks(int pre) 
  XFORM_SKIP_PROC
{
  Scheme_GC_Pre_Post_Callback_Desc *prev = NULL, *desc;
  Scheme_Object *acts, *act, *protocol;
  int j;

  desc = gc_prepost_callback_descs; 
  while (desc) {
    if (!SCHEME_WEAK_BOX_VAL(desc->boxed_key)) {
      if (prev)
        prev->next = desc->next;
      else
        gc_prepost_callback_descs = desc->next;
      if (desc->next)
        desc->next->prev = desc->prev;
    } else {
      if (pre)
        acts = desc->pre_desc;
      else
        acts = desc->post_desc;
      for (j = 0; j < SCHEME_VEC_SIZE(acts); j++) {
        act = SCHEME_VEC_ELS(acts)[j];
        protocol = SCHEME_VEC_ELS(act)[0];
        /* The set of suported protocols is arbitary, based on what we've needed
           so far. */
        if (!strcmp(SCHEME_SYM_VAL(protocol), "ptr_ptr_ptr_int->void")) {
          gccb_Ptr_Ptr_Ptr_Int_to_Void proc;
          void *a, *b, *c;
          int i;

          proc = (gccb_Ptr_Ptr_Ptr_Int_to_Void)scheme_extract_pointer(SCHEME_VEC_ELS(act)[1]);
          a = scheme_extract_pointer(SCHEME_VEC_ELS(act)[2]);
          b = scheme_extract_pointer(SCHEME_VEC_ELS(act)[3]);
          c = scheme_extract_pointer(SCHEME_VEC_ELS(act)[4]);
          i = SCHEME_INT_VAL(SCHEME_VEC_ELS(act)[5]);

          proc(a, b, c, i);
        } else if (!strcmp(SCHEME_SYM_VAL(protocol), "ptr_ptr_ptr->void")) {
          gccb_Ptr_Ptr_Ptr_to_Void proc;
          void *a, *b, *c;

          proc = (gccb_Ptr_Ptr_Ptr_to_Void)scheme_extract_pointer(SCHEME_VEC_ELS(act)[1]);
          a = scheme_extract_pointer(SCHEME_VEC_ELS(act)[2]);
          b = scheme_extract_pointer(SCHEME_VEC_ELS(act)[3]);
          c = scheme_extract_pointer(SCHEME_VEC_ELS(act)[4]);

          proc(a, b, c);
        } else if (!strcmp(SCHEME_SYM_VAL(protocol), "ptr_ptr_float->void")) {
          gccb_Ptr_Ptr_Float_to_Void proc;
          void *a, *b;
          float f;

          proc = (gccb_Ptr_Ptr_Float_to_Void)scheme_extract_pointer(SCHEME_VEC_ELS(act)[1]);
          a = scheme_extract_pointer(SCHEME_VEC_ELS(act)[2]);
          b = scheme_extract_pointer(SCHEME_VEC_ELS(act)[3]);
          f = SCHEME_DBL_VAL(SCHEME_VEC_ELS(act)[4]);

          proc(a, b, f);
        } else if (!strcmp(SCHEME_SYM_VAL(protocol), "ptr_ptr_double->void")) {
          gccb_Ptr_Ptr_Double_to_Void proc;
          void *a, *b;
          double d;

          proc = (gccb_Ptr_Ptr_Double_to_Void)scheme_extract_pointer(SCHEME_VEC_ELS(act)[1]);
          a = scheme_extract_pointer(SCHEME_VEC_ELS(act)[2]);
          b = scheme_extract_pointer(SCHEME_VEC_ELS(act)[3]);
          d = SCHEME_DBL_VAL(SCHEME_VEC_ELS(act)[4]);

          proc(a, b, d);
        } else if (!strcmp(SCHEME_SYM_VAL(protocol), "ptr_ptr_ptr_int_int_int_int_int_int_int_int_int->void")) {
          gccb_Ptr_Ptr_Ptr_Nine_Ints proc;
          void *a, *b, *c;
          int i1, i2, i3, i4, i5, i6, i7, i8, i9;

          proc = (gccb_Ptr_Ptr_Ptr_Nine_Ints)scheme_extract_pointer(SCHEME_VEC_ELS(act)[1]);
          a = scheme_extract_pointer(SCHEME_VEC_ELS(act)[2]);
          b = scheme_extract_pointer(SCHEME_VEC_ELS(act)[3]);
          c = scheme_extract_pointer(SCHEME_VEC_ELS(act)[4]);
          i1 = SCHEME_INT_VAL(SCHEME_VEC_ELS(act)[5]);
          i2 = SCHEME_INT_VAL(SCHEME_VEC_ELS(act)[6]);
          i3 = SCHEME_INT_VAL(SCHEME_VEC_ELS(act)[7]);
          i4 = SCHEME_INT_VAL(SCHEME_VEC_ELS(act)[8]);
          i5 = SCHEME_INT_VAL(SCHEME_VEC_ELS(act)[9]);
          i6 = SCHEME_INT_VAL(SCHEME_VEC_ELS(act)[10]);
          i7 = SCHEME_INT_VAL(SCHEME_VEC_ELS(act)[11]);
          i8 = SCHEME_INT_VAL(SCHEME_VEC_ELS(act)[12]);
          i9 = SCHEME_INT_VAL(SCHEME_VEC_ELS(act)[13]);

          proc(a, b, c, i1, i2, i3, i4, i5, i6, i7, i8, i9);
        } else if (!strcmp(SCHEME_SYM_VAL(protocol), "osapi_ptr_ptr->void")) {
          gccb_OSapi_Ptr_Ptr_to_Void proc;
          void *a, *b;

          proc = (gccb_OSapi_Ptr_Ptr_to_Void)scheme_extract_pointer(SCHEME_VEC_ELS(act)[1]);
          a = scheme_extract_pointer(SCHEME_VEC_ELS(act)[2]);
          b = scheme_extract_pointer(SCHEME_VEC_ELS(act)[3]);

          proc(a, b);
        } else if (!strcmp(SCHEME_SYM_VAL(protocol), "osapi_ptr_int->void")) {
          gccb_OSapi_Ptr_Int_to_Void proc;
          void *a;
          int i;

          proc = (gccb_OSapi_Ptr_Int_to_Void)scheme_extract_pointer(SCHEME_VEC_ELS(act)[1]);
          a = scheme_extract_pointer(SCHEME_VEC_ELS(act)[2]);
          i = SCHEME_INT_VAL(SCHEME_VEC_ELS(act)[3]);

          proc(a, i);
        } else if (!strcmp(SCHEME_SYM_VAL(protocol), "osapi_ptr_int_int_int_int_ptr_int_int_long->void")) {
          gccb_OSapi_Ptr_Four_Ints_Ptr_Int_Int_Long_to_Void proc;
          void *a, *b;
          int i1, i2, i3, i4, i5, i6;
          long l1;

          proc = (gccb_OSapi_Ptr_Four_Ints_Ptr_Int_Int_Long_to_Void)scheme_extract_pointer(SCHEME_VEC_ELS(act)[1]);
          a = scheme_extract_pointer(SCHEME_VEC_ELS(act)[2]);
          i1 = SCHEME_INT_VAL(SCHEME_VEC_ELS(act)[3]);
          i2 = SCHEME_INT_VAL(SCHEME_VEC_ELS(act)[4]);
          i3 = SCHEME_INT_VAL(SCHEME_VEC_ELS(act)[5]);
          i4 = SCHEME_INT_VAL(SCHEME_VEC_ELS(act)[6]);
          b = scheme_extract_pointer(SCHEME_VEC_ELS(act)[7]);
          i5 = SCHEME_INT_VAL(SCHEME_VEC_ELS(act)[8]);
          i6 = SCHEME_INT_VAL(SCHEME_VEC_ELS(act)[9]);
          l1 = SCHEME_INT_VAL(SCHEME_VEC_ELS(act)[10]);

          proc(a, i1, i2, i3, i4, b, i5, i6, l1);
        }
        prev = desc;
      }
    }
    desc = desc->next;
  }
}

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif

void scheme_zero_unneeded_rands(Scheme_Thread *p)
{
  /* Call this procedure before GC or before copying out
     a thread's stack. */
}

static void prepare_thread_for_GC(Scheme_Object *t)
{
  Scheme_Thread *p = (Scheme_Thread *)t;

  if (!p->running) return;

  /* zero ununsed part of env stack in each thread */

  if (!p->nestee) {
    Scheme_Saved_Stack *saved;
# define RUNSTACK_TUNE(x) /* x   - Used for performance tuning */
    RUNSTACK_TUNE( intptr_t size; );

    if ((!p->runstack_owner
         || (p == *p->runstack_owner))
        && p->runstack_start) {
      intptr_t rs_end;
      Scheme_Object **rs_start;

      /* If there's a meta-prompt, we can also zero out past the unused part */
      if (p->meta_prompt && (p->meta_prompt->runstack_boundary_start == p->runstack_start)) {
        rs_end = p->meta_prompt->runstack_boundary_offset;
      } else {
        rs_end = p->runstack_size;
      }

      if ((p->runstack_tmp_keep >= p->runstack_start)
          && (p->runstack_tmp_keep < p->runstack))
        rs_start = p->runstack_tmp_keep;
      else
        rs_start = p->runstack;

      scheme_set_runstack_limits(p->runstack_start, 
                                 p->runstack_size,
                                 rs_start - p->runstack_start,
                                 rs_end);
      
      RUNSTACK_TUNE( size = p->runstack_size - (p->runstack - p->runstack_start); );
      
      for (saved = p->runstack_saved; saved; saved = saved->prev) {
	RUNSTACK_TUNE( size += saved->runstack_size; );

        if (p->meta_prompt && (p->meta_prompt->runstack_boundary_start == saved->runstack_start)) {
          rs_end = p->meta_prompt->runstack_boundary_offset;
        } else {
          rs_end = saved->runstack_size;
        }

        scheme_set_runstack_limits(saved->runstack_start,
                                   saved->runstack_size,
                                   saved->runstack_offset,
                                   rs_end);
      }
    }

    RUNSTACK_TUNE( printf("%ld\n", size); );

    if (p->tail_buffer && (p->tail_buffer != p->runstack_tmp_keep)) {
      int i;
      for (i = 0; i < p->tail_buffer_size; i++) {
	p->tail_buffer[i] = NULL;
      }
    }
  }
      
  if ((!p->cont_mark_stack_owner
       || (p == *p->cont_mark_stack_owner))
      && p->cont_mark_stack) {
    int segcount, i, segpos;

    /* release unused cont mark stack segments */
    if (p->cont_mark_stack)
      segcount = ((intptr_t)(p->cont_mark_stack - 1) >> SCHEME_LOG_MARK_SEGMENT_SIZE) + 1;
    else
      segcount = 0;
    for (i = segcount; i < p->cont_mark_seg_count; i++) {
      p->cont_mark_stack_segments[i] = NULL;
    }
    if (segcount < p->cont_mark_seg_count)
      p->cont_mark_seg_count = segcount;
      
    /* zero unused part of last mark stack segment */
    segpos = ((intptr_t)p->cont_mark_stack >> SCHEME_LOG_MARK_SEGMENT_SIZE);
    
    if (segpos < p->cont_mark_seg_count) {
      Scheme_Cont_Mark *seg = p->cont_mark_stack_segments[segpos];
      int stackpos = ((intptr_t)p->cont_mark_stack & SCHEME_MARK_SEGMENT_MASK);
      if (seg) {
        for (i = stackpos; i < SCHEME_MARK_SEGMENT_SIZE; i++) {
          if (seg[i].key) {
            seg[i].key = NULL;
            seg[i].val = NULL;
            seg[i].cache = NULL;
          } else {
            /* NULL means we already cleared from here on. */
            break;
          }
        }
      }
    }

    {
      MZ_MARK_STACK_TYPE pos;
      /* also zero out slots before the current bottom */
      for (pos = 0; pos < p->cont_mark_stack_bottom; pos++) {
        Scheme_Cont_Mark *seg;
        int stackpos;
        segpos = ((intptr_t)pos >> SCHEME_LOG_MARK_SEGMENT_SIZE);
        seg = p->cont_mark_stack_segments[segpos];
        if (seg) {
          stackpos = ((intptr_t)pos & SCHEME_MARK_SEGMENT_MASK);
          seg[stackpos].key = NULL;
          seg[stackpos].val = NULL;
          seg[stackpos].cache = NULL;
        }
      }
    }
  }

  if (p->values_buffer) {
    if (p->values_buffer_size > 128)
      p->values_buffer = NULL;
    else {
      memset(p->values_buffer, 0, sizeof(Scheme_Object*) * p->values_buffer_size);
    }
  }

  p->spare_runstack = NULL;

  /* zero ununsed part of list stack */
  scheme_clean_list_stack(p);
}

void scheme_prepare_this_thread_for_GC(Scheme_Thread *p)
{
  if (p == scheme_current_thread) {
#ifdef RUNSTACK_IS_GLOBAL
    scheme_current_thread->runstack = MZ_RUNSTACK;
    scheme_current_thread->runstack_start = MZ_RUNSTACK_START;
    scheme_current_thread->cont_mark_stack = MZ_CONT_MARK_STACK;
    scheme_current_thread->cont_mark_pos = MZ_CONT_MARK_POS;
#endif
  }
  prepare_thread_for_GC((Scheme_Object *)p);
}

static void get_ready_for_GC()
{
  start_this_gc_real_time = scheme_get_inexact_milliseconds();
  start_this_gc_time = scheme_get_process_milliseconds();

#ifdef MZ_USE_FUTURES
  scheme_future_block_until_gc();
#endif

  run_gc_callbacks(1);

  scheme_zero_unneeded_rands(scheme_current_thread);

  scheme_clear_modidx_cache();
  scheme_clear_shift_cache();
  scheme_clear_prompt_cache();
  scheme_clear_rx_buffers();
  scheme_clear_bignum_cache();
  scheme_clear_delayed_load_cache();
#ifdef MZ_USE_PLACES
  scheme_clear_place_ifs_stack();
#endif

#ifdef RUNSTACK_IS_GLOBAL
  if (scheme_current_thread->running) {
    scheme_current_thread->runstack = MZ_RUNSTACK;
    scheme_current_thread->runstack_start = MZ_RUNSTACK_START;
    scheme_current_thread->cont_mark_stack = MZ_CONT_MARK_STACK;
    scheme_current_thread->cont_mark_pos = MZ_CONT_MARK_POS;
  }
#endif

  /* Prepare each thread that has run: */
  if (gc_prep_thread_chain) {
    Scheme_Thread *p, *next;
    p = gc_prep_thread_chain;
    while (p != p->gc_prep_chain) {
      prepare_thread_for_GC((Scheme_Object *)p);
      next = p->gc_prep_chain;
      p->gc_prep_chain = NULL;
      p = next;
    }
    prepare_thread_for_GC((Scheme_Object *)p);
    p->gc_prep_chain = NULL;
    gc_prep_thread_chain = NULL;
  }

#ifdef MZ_PRECISE_GC
  scheme_flush_stack_copy_cache();
#endif

  scheme_fuel_counter = 0;
  scheme_jit_stack_boundary = (uintptr_t)-1;

#ifdef WINDOWS_PROCESSES
  scheme_suspend_remembered_threads();
#endif
#if defined(UNIX_PROCESSES) && !defined(MZ_PLACES_WAITPID)
  scheme_block_child_signals(1);
#endif

  {
    GC_CAN_IGNORE void *data;
    data = scheme_gmp_tls_load(scheme_current_thread->gmp_tls);
    scheme_current_thread->gmp_tls_data = data;
  }

  scheme_did_gc_count++;
}

extern int GC_words_allocd;

static void done_with_GC()
{
  scheme_gmp_tls_unload(scheme_current_thread->gmp_tls, scheme_current_thread->gmp_tls_data);
  scheme_current_thread->gmp_tls_data = NULL;

#ifdef RUNSTACK_IS_GLOBAL
# ifdef MZ_PRECISE_GC
  if (scheme_current_thread->running) {
    MZ_RUNSTACK = scheme_current_thread->runstack;
    MZ_RUNSTACK_START = scheme_current_thread->runstack_start;
  }
# endif
#endif
#ifdef WINDOWS_PROCESSES
  scheme_resume_remembered_threads();
#endif
#if defined(UNIX_PROCESSES) && !defined(MZ_PLACES_WAITPID)
  scheme_block_child_signals(0);
#endif

  end_this_gc_time = scheme_get_process_milliseconds();
  end_this_gc_real_time = scheme_get_inexact_milliseconds();
  scheme_total_gc_time += (end_this_gc_time - start_this_gc_time);

  gc_prep_thread_chain = scheme_current_thread;
  scheme_current_thread->gc_prep_chain = scheme_current_thread;

  run_gc_callbacks(0);

#ifdef MZ_USE_FUTURES
  scheme_future_continue_after_gc();
#endif

#ifndef MZ_PRECISE_GC
  {
    Scheme_Logger *logger = scheme_get_gc_logger();
    if (logger) {
      char buf[64];
      intptr_t buflen;

      sprintf(buf,
              "in %" PRIdPTR " msec",
              end_this_gc_time - start_this_gc_time);
      buflen = strlen(buf);

      scheme_log_message(logger, SCHEME_LOG_DEBUG, buf, buflen, NULL);
    }
  }
#endif
}

#ifdef MZ_PRECISE_GC
static char *gc_num(char *nums, intptr_t v)
/* format a number with commas */
{
  int i, j, len, clen, c, d;
  for (i = 0; nums[i] || nums[i+1]; i++) {
  }
  i++;

  v /= 1024; /* bytes => kbytes */

  sprintf(nums+i, "%" PRIdPTR, v);
  for (len = 0; nums[i+len]; len++) { }
  clen = len + ((len + ((nums[i] == '-') ? -2 : -1)) / 3);
  
  c = 0;
  d = (clen - len);
  for (j = i + clen - 1; j > i; j--) {
    if (c == 3) {
      nums[j] = ',';
      d--;
      c = 0;
    } else {
      nums[j] = nums[j - d];
      c++;
    }
  }

  return nums + i;
}

#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

static void inform_GC(int master_gc, int major_gc, 
                      intptr_t pre_used, intptr_t post_used,
                      intptr_t pre_admin, intptr_t post_admin,
                      intptr_t post_child_places_used)
{
  Scheme_Logger *logger;
  logger = scheme_get_gc_logger();
  if (logger && scheme_log_level_p(logger, SCHEME_LOG_DEBUG)) {
    /* Don't use scheme_log(), because it wants to allocate a buffer
       based on the max value-print width, and we may not be at a
       point where parameters are available. */
    char buf[256], nums[128];
    intptr_t buflen, delta, admin_delta;
    Scheme_Object *vec, *v;

#ifdef MZ_USE_PLACES
# define PLACE_ID_FORMAT "%d:"
#else
# define PLACE_ID_FORMAT ""
#endif

    vec = scheme_false;
    if (!master_gc && gc_info_prefab) {
      vec = scheme_make_vector(11, scheme_false);
      SCHEME_VEC_ELS(vec)[1] = (major_gc ? scheme_true : scheme_false);
      SCHEME_VEC_ELS(vec)[2] = scheme_make_integer(pre_used);
      SCHEME_VEC_ELS(vec)[3] = scheme_make_integer(pre_admin);
      SCHEME_VEC_ELS(vec)[4] = scheme_make_integer(scheme_code_page_total);
      SCHEME_VEC_ELS(vec)[5] = scheme_make_integer(post_used);
      SCHEME_VEC_ELS(vec)[6] = scheme_make_integer(post_admin);
      v = scheme_make_integer_value(start_this_gc_time);
      SCHEME_VEC_ELS(vec)[7] = v;
      v = scheme_make_integer_value(end_this_gc_time);
      SCHEME_VEC_ELS(vec)[8] = v;
      v = scheme_make_double(start_this_gc_real_time);
      SCHEME_VEC_ELS(vec)[9] = v;
      v = scheme_make_double(end_this_gc_real_time);
      SCHEME_VEC_ELS(vec)[10] = v;
      vec = scheme_make_prefab_struct_instance(gc_info_prefab, vec);
    }

    START_XFORM_SKIP;

    memset(nums, 0, sizeof(nums));

    delta = pre_used - post_used;
    admin_delta = (pre_admin - post_admin) - delta;
    sprintf(buf,
            "" PLACE_ID_FORMAT "%s @ %sK(+%sK)[+%sK];"
            " free %sK(%s%sK) %" PRIdPTR "ms @ %" PRIdPTR,
#ifdef MZ_USE_PLACES
            scheme_current_place_id,
#endif
            (master_gc ? "MST" : (major_gc ? "MAJ" : "min")),
            gc_num(nums, pre_used), gc_num(nums, pre_admin - pre_used),
            gc_num(nums, scheme_code_page_total),
            gc_num(nums, delta), ((admin_delta < 0) ? "" : "+"),  gc_num(nums, admin_delta),
            (master_gc ? 0 : (end_this_gc_time - start_this_gc_time)),
            start_this_gc_time);
    buflen = strlen(buf);

    END_XFORM_SKIP;

    scheme_log_message(logger, SCHEME_LOG_DEBUG, buf, buflen, vec);
  }

#ifdef MZ_USE_PLACES
  if (!master_gc) {
    scheme_place_set_memory_use(post_used + post_child_places_used);
  }
#endif
}
#endif

/*========================================================================*/
/*                                 stats                                  */
/*========================================================================*/

static void set_perf_vector(Scheme_Object *v, Scheme_Object *ov, int i, Scheme_Object *a)
{
  if (SAME_OBJ(v, ov))
    SCHEME_VEC_ELS(v)[i] = a;
  else
    scheme_chaperone_vector_set(ov, i, a);
}

static Scheme_Object *current_stats(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v, *ov;
  Scheme_Thread *t = NULL;
  
  v = argv[0];

  ov = v;
  if (SCHEME_CHAPERONEP(v))
    v = SCHEME_CHAPERONE_VAL(v);

  if (!SCHEME_MUTABLE_VECTORP(v))
    scheme_wrong_contract("vector-set-performance-stats!", "(and/c vector? (not/c immutable?))", 0, argc, argv);
  if (argc > 1) {
    if (!SCHEME_FALSEP(argv[1])) {
      if (!SCHEME_THREADP(argv[1]))
	scheme_wrong_contract("vector-set-performance-stats!", "(or/c thread? #f)", 0, argc, argv);
      t = (Scheme_Thread *)argv[1];
    }
  }
  
  if (t) {
    switch (SCHEME_VEC_SIZE(v)) {
    default:
    case 4:
      {
	/* Stack size: */
	intptr_t sz = 0;

	if (MZTHREAD_STILL_RUNNING(t->running)) {
	  Scheme_Overflow *overflow;
	  Scheme_Saved_Stack *runstack_saved;
	  
	  /* C stack */
	  if (t == scheme_current_thread) {
	    void *stk_start, *stk_end;
	    stk_start = t->stack_start;
	    stk_end = (void *)&stk_end;
#         ifdef STACK_GROWS_UP
	    sz = (intptr_t)stk_end XFORM_OK_MINUS (intptr_t)stk_start;
#         endif
#         ifdef STACK_GROWS_DOWN
	    sz = (intptr_t)stk_start XFORM_OK_MINUS (intptr_t)stk_end;
#         endif
	  } else {
	    if (t->jmpup_buf.stack_copy)
	      sz = t->jmpup_buf.stack_size;
	  }
	  for (overflow = t->overflow; overflow; overflow = overflow->prev) {
	    sz += overflow->jmp->cont.stack_size;
	  }
	  
	  /* Scheme stack */
	  {
	    int ssz;
	    if (t == scheme_current_thread) {
	      ssz = (MZ_RUNSTACK_START + t->runstack_size) - MZ_RUNSTACK;
	    } else {
	      ssz = (t->runstack_start + t->runstack_size) - t->runstack;
	    }
	    for (runstack_saved = t->runstack_saved; runstack_saved; runstack_saved = runstack_saved->prev) {
	      ssz += runstack_saved->runstack_size;
	    }
	    sz += sizeof(Scheme_Object *) * ssz;
	  }
	  
	  /* Mark stack */
	  if (t == scheme_current_thread) {
	    sz += ((intptr_t)scheme_current_cont_mark_pos >> 1) * sizeof(Scheme_Cont_Mark);
	  } else {
	    sz += ((intptr_t)t->cont_mark_pos >> 1) * sizeof(Scheme_Cont_Mark);
	  }
	}

        set_perf_vector(v, ov, 3, scheme_make_integer(sz));
      }
    case 3:
      set_perf_vector(v, ov, 2, (t->block_descriptor 
                                 ? scheme_true 
                                 : ((t->running & MZTHREAD_SUSPENDED)
                                    ? scheme_true
                                    : scheme_false)));
    case 2:
      {
	Scheme_Object *dp;
	dp = thread_dead_p(1, (Scheme_Object **) mzALIAS &t);
	set_perf_vector(v, ov, 1, dp);
      }
    case 1:
      {
	Scheme_Object *rp;
	rp = thread_running_p(1, (Scheme_Object **) mzALIAS &t);
	set_perf_vector(v, ov, 0, rp);
      }
    case 0:
      break;
    }
  } else {
    intptr_t cpuend, end, gcend;

    cpuend = scheme_get_process_milliseconds();
    end = scheme_get_milliseconds();
    gcend = scheme_total_gc_time;
    
    switch (SCHEME_VEC_SIZE(v)) {
    default:
    case 11:
      set_perf_vector(v, ov, 10, scheme_make_integer(scheme_jit_malloced));
    case 10:
      set_perf_vector(v, ov, 9, scheme_make_integer(scheme_hash_iteration_count));
    case 9:
      set_perf_vector(v, ov, 8, scheme_make_integer(scheme_hash_request_count));
    case 8:
      set_perf_vector(v, ov, 7, scheme_make_integer(scheme_num_read_syntax_objects));
    case 7:
      set_perf_vector(v, ov, 6, scheme_make_integer(num_running_threads+1));
    case 6:
      set_perf_vector(v, ov, 5, scheme_make_integer(scheme_overflow_count));
    case 5:
      set_perf_vector(v, ov, 4, scheme_make_integer(thread_swap_count));
    case 4:
      set_perf_vector(v, ov, 3, scheme_make_integer(scheme_did_gc_count));
    case 3:
      set_perf_vector(v, ov, 2, scheme_make_integer(gcend));
    case 2:
      set_perf_vector(v, ov, 1, scheme_make_integer(end));
    case 1:
      set_perf_vector(v, ov, 0, scheme_make_integer(cpuend));
    case 0:
      break;
    }
  }

  return scheme_void;
}

/*========================================================================*/
/*                             gmp allocation                             */
/*========================================================================*/

/* Allocate atomic, immobile memory for GMP. Although we have set up
   GMP to reliably free anything that it allocates, we allocate via
   the GC to get accounting with 3m. The set of allocated blocks are
   stored in a "mem_pool" variable, which is a linked list; GMP
   allocates with a stack discipline, so maintaining the list is easy.
   Meanwhile, scheme_gmp_tls_unload, etc., attach to the pool to the
   owning thread as needed for GC. */

void *scheme_malloc_gmp(uintptr_t amt, void **mem_pool)
{
  void *p, *mp;

  p = scheme_malloc_atomic_allow_interior(amt);

  mp = scheme_make_raw_pair(p, *mem_pool);
  *mem_pool = mp;

  return p;
}

void scheme_free_gmp(void *p, void **mem_pool)
{
  if (p != SCHEME_CAR(*mem_pool))
    scheme_log(NULL,
               SCHEME_LOG_FATAL,
               0,
               "bad GMP memory free");
  *mem_pool = SCHEME_CDR(*mem_pool);
}

/*========================================================================*/
/*                               precise GC                               */
/*========================================================================*/

Scheme_Jumpup_Buf_Holder *scheme_new_jmpupbuf_holder(void)
/* Scheme_Jumpup_Buf_Holder exists for precise GC, and for external
   programs that want to store Jumpup_Bufs, because the GC interaction
   is tricky. For example, we use it above for a special trampoline
   implementation. */
{
  Scheme_Jumpup_Buf_Holder *h;

  h = MALLOC_ONE_RT(Scheme_Jumpup_Buf_Holder);
#ifdef MZ_PRECISE_GC
  h->type = scheme_rt_buf_holder;
#endif

  return h;
}

#ifdef MZ_PRECISE_GC
uintptr_t scheme_get_current_thread_stack_start(void)
{
  Scheme_Thread *p;
  p = scheme_current_thread;
  return (uintptr_t)p->stack_start;
}
#endif

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_thread.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_will_executor_type, mark_will_executor_val);
  GC_REG_TRAV(scheme_custodian_type, mark_custodian_val);
  GC_REG_TRAV(scheme_cust_box_type, mark_custodian_box_val);
  GC_REG_TRAV(scheme_thread_hop_type, mark_thread_hop);
  GC_REG_TRAV(scheme_evt_set_type, mark_evt_set);
  GC_REG_TRAV(scheme_thread_set_type, mark_thread_set);
  GC_REG_TRAV(scheme_config_type, mark_config);
  GC_REG_TRAV(scheme_thread_cell_type, mark_thread_cell);
  GC_REG_TRAV(scheme_plumber_type, mark_plumber);

  GC_REG_TRAV(scheme_rt_param_data, mark_param_data);
  GC_REG_TRAV(scheme_rt_will, mark_will);
  GC_REG_TRAV(scheme_rt_evt, mark_evt);
  GC_REG_TRAV(scheme_rt_syncing, mark_syncing);
  GC_REG_TRAV(scheme_rt_parameterization, mark_parameterization);
}

END_XFORM_SKIP;

#endif
