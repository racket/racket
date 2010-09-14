#include "schpriv.h"

/* READ ONLY SHARABLE GLOBALS */

#ifdef MZ_USE_PLACES

#include "mzrt.h"
#ifdef UNIX_PROCESSES
# include <unistd.h>
#endif

READ_ONLY static Scheme_Object *scheme_def_place_exit_proc;

SHARED_OK mz_proc_thread *scheme_master_proc_thread;
THREAD_LOCAL_DECL(mz_proc_thread *proc_thread_self);
static Scheme_Object *scheme_place(int argc, Scheme_Object *args[]);
static Scheme_Object *scheme_place_wait(int argc, Scheme_Object *args[]);
static Scheme_Object *scheme_place_sleep(int argc, Scheme_Object *args[]);
static Scheme_Object *scheme_place_p(int argc, Scheme_Object *args[]);
static Scheme_Object *scheme_place_send(int argc, Scheme_Object *args[]);
static Scheme_Object *scheme_place_recv(int argc, Scheme_Object *args[]);
static Scheme_Object *scheme_place_channel_p(int argc, Scheme_Object *args[]);
static Scheme_Object *def_place_exit_handler_proc(int argc, Scheme_Object *args[]);
static Scheme_Object *scheme_place_channel(int argc, Scheme_Object *args[]);

static Scheme_Place_Async_Channel *scheme_place_async_channel_create();
static Scheme_Place_Bi_Channel *scheme_place_bi_channel_create();
static Scheme_Place_Bi_Channel *scheme_place_bi_peer_channel_create(Scheme_Place_Bi_Channel *orig);
static int scheme_place_channel_ready(Scheme_Object *so);


static void scheme_place_async_send(Scheme_Place_Async_Channel *ch, Scheme_Object *o, void *msg_memory);
static Scheme_Object *scheme_place_async_recv(Scheme_Place_Async_Channel *ch, void **msg_memory);

static Scheme_Object *scheme_places_deep_copy_to_master(Scheme_Object *so);
/* Scheme_Object *scheme_places_deep_copy(Scheme_Object *so); */

#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
static Scheme_Object *scheme_places_deep_copy_worker(Scheme_Object *so, Scheme_Hash_Table *ht);
#endif

# ifdef MZ_PRECISE_GC
static void register_traversers(void);
# endif

static void *place_start_proc(void *arg);
static void *place_start_proc_after_stack(void *data_arg, void *stack_base);

# define PLACE_PRIM_W_ARITY(name, func, a1, a2, env) GLOBAL_PRIM_W_ARITY(name, func, a1, a2, env)

#else

# define PLACE_PRIM_W_ARITY(name, func, a1, a2, env) GLOBAL_PRIM_W_ARITY(name, not_implemented, a1, a2, env)

static Scheme_Object *not_implemented(int argc, Scheme_Object **argv)
{
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED, "not supported");
  return NULL;
}

# ifdef MZ_PRECISE_GC
static void register_traversers(void) { }
# endif

#endif

/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/
void scheme_init_place(Scheme_Env *env)
{
  Scheme_Env *plenv;

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif
  
  plenv = scheme_primitive_module(scheme_intern_symbol("#%place"), env);

  PLACE_PRIM_W_ARITY("place",          scheme_place,       2, 2, plenv);
  PLACE_PRIM_W_ARITY("place-sleep",    scheme_place_sleep, 1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-wait",     scheme_place_wait,  1, 1, plenv);
  PLACE_PRIM_W_ARITY("place?",         scheme_place_p,     1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-channel",  scheme_place_channel,  0, 0, plenv);
  PLACE_PRIM_W_ARITY("place-channel-send",  scheme_place_send,  1, 2, plenv);
  PLACE_PRIM_W_ARITY("place-channel-recv",  scheme_place_recv,  1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-channel?",      scheme_place_channel_p,  1, 1, plenv);

#ifdef MZ_USE_PLACES
  REGISTER_SO(scheme_def_place_exit_proc);
  scheme_def_place_exit_proc = scheme_make_prim_w_arity(def_place_exit_handler_proc, "default-place-exit-handler", 1, 1);
#endif
  scheme_finish_primitive_module(plenv);

}

void scheme_init_places_once() {
#ifdef MZ_USE_PLACES
  scheme_add_evt(scheme_place_type,            (Scheme_Ready_Fun)scheme_place_channel_ready, NULL, NULL, 1); 
  scheme_add_evt(scheme_place_bi_channel_type, (Scheme_Ready_Fun)scheme_place_channel_ready, NULL, NULL, 1);
#endif
}

#ifdef MZ_USE_PLACES

/************************************************************************/
/************************************************************************/
/************************************************************************/

typedef struct Place_Start_Data {
  /* Allocated as array of objects, so all
     field must be pointers */
  Scheme_Object *module;
  Scheme_Object *function;
  Scheme_Object *channel;
  Scheme_Object *current_library_collection_paths;
  mzrt_sema *ready;
} Place_Start_Data;

static Scheme_Object *def_place_exit_handler_proc(int argc, Scheme_Object *argv[])
{
  long status;

  if (SCHEME_INTP(argv[0])) {
    status = SCHEME_INT_VAL(argv[0]);
    if (status < 1 || status > 255)
      status = 0;
  } else
    status = 0;

  mz_proc_thread_exit((void *) status);
  return scheme_void; /* Never get here */
}

static void null_out_runtime_globals() {
  scheme_current_thread           = NULL;
  scheme_first_thread             = NULL;
  scheme_main_thread              = NULL;
                                                                   
  scheme_current_runstack_start   = NULL;
  scheme_current_runstack         = NULL;
  scheme_current_cont_mark_stack  = 0;
  scheme_current_cont_mark_pos    = 0;
}

Scheme_Object *scheme_place_sleep(int argc, Scheme_Object *args[]) {
  mzrt_sleep(SCHEME_INT_VAL(args[0]));
  return scheme_void;
}

Scheme_Object *scheme_place(int argc, Scheme_Object *args[]) {
  Scheme_Place          *place;
  Place_Start_Data      *place_data;
  mz_proc_thread        *proc_thread;
  Scheme_Object         *collection_paths;
  mzrt_sema             *ready;

  /* create place object */
  place = MALLOC_ONE_TAGGED(Scheme_Place);
  place->so.type = scheme_place_type;

  mzrt_sema_create(&ready, 0);

  /* pass critical info to new place */
  place_data = MALLOC_ONE(Place_Start_Data);
  place_data->ready    = ready;

  if (argc == 2) {
    Scheme_Object *so;

    if (!scheme_is_module_path(args[0]) && !SCHEME_PATHP(args[0])) {
      scheme_wrong_type("place", "module-path or path", 0, argc, args);
    }
    if (!SCHEME_SYMBOLP(args[1])) {
      scheme_wrong_type("place", "symbol", 1, argc, args);
    }

    so = scheme_places_deep_copy_to_master(args[0]);
    place_data->module   = so;
    so = scheme_places_deep_copy_to_master(args[1]);
    place_data->function = so;
    place_data->ready    = ready;
    
    /* create channel */
    {
      Scheme_Place_Bi_Channel *channel;
      channel = scheme_place_bi_channel_create();
      place->channel = (Scheme_Object *) channel;
      channel = scheme_place_bi_peer_channel_create(channel);
      place_data->channel = (Scheme_Object *) channel;
    }
  }
  else {
    scheme_wrong_count_m("place", 2, 2, argc, args, 0);
  }

  collection_paths = scheme_current_library_collection_paths(0, NULL);
  collection_paths = scheme_places_deep_copy_to_master(collection_paths);
  place_data->current_library_collection_paths = collection_paths;

  /* create new place */
  proc_thread = mz_proc_thread_create(place_start_proc, place_data);

  /* wait until the place has started and grabbed the value
     from `place_data'; it's important that a GC doesn't happen
     here until the other place is far enough. */
  mzrt_sema_wait(ready);
  mzrt_sema_destroy(ready);
  
  place->proc_thread = proc_thread;

  return (Scheme_Object*) place;
}

# if defined(MZ_PLACES_WAITPID)
/*============= SIGCHLD SIGNAL HANDLING =============*/

/* If SIGCHLD is unblocked, it gets delivered to a random thread
   --- not necessarily on in the right place for the subprocess.
   To avoid that problem, we centralize SIGCHLD handling here, and
   then dispatch back out to specific places as they request 
   information. */

#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>

typedef struct Child_Status {
  int pid;
  int status;
  char done;
  char unneeded; /* not in a group; result not needed */
  char is_group;
  void *signal_fd;
  struct Child_Status *next;
  struct Child_Status *next_group; /* see child_group_statuses */
} Child_Status;

SHARED_OK static Child_Status *child_statuses = NULL;
SHARED_OK static mzrt_mutex* child_status_lock = NULL;
SHARED_OK static mzrt_mutex* child_wait_lock = NULL; /* ordered before status lock */

/* When a process for a process group becomes unreachable 
   before a waitpid() on the process, then we need to keep
   waiting on it to let the OS gc the process. Otherwise,
   we wait only on processes in the same group as Racket. 
   This list is protect by the wait lock. */
SHARED_OK static Child_Status *child_group_statuses = NULL;

static void add_group_signal_fd(void *signal_fd);
static void remove_group_signal_fd(void *signal_fd);
static void do_group_signal_fds();

static void add_child_status(int pid, int status) {
  Child_Status *st;

  /* Search for existing record, which will have a signal_fd: */
  mzrt_mutex_lock(child_status_lock);
  for (st = child_statuses; st; st = st->next) {
    if (st->pid == pid)
      break;
  }

  if (!st) {
    /* must have terminated before it was registered
       (and since we detected it, it must not be a group) */
    st = malloc(sizeof(Child_Status));
    st->pid = pid;
    st->signal_fd = NULL;
    st->next = child_statuses;
    child_statuses = st;
    st->next_group = NULL;
    st->unneeded = 0;
    st->is_group = 0;
  }
  st->status = status;
  st->done = 1;

  if (st->signal_fd && st->is_group)
    remove_group_signal_fd(st->signal_fd);

  mzrt_mutex_unlock(child_status_lock);
  
  if (st->signal_fd)
    scheme_signal_received_at(st->signal_fd);
  if (st->unneeded)
    (void)scheme_get_child_status(st->pid, 0, NULL);
}

static int raw_get_child_status(int pid, int *status, int done_only, int do_remove, int do_free) {
  Child_Status *st;
  Child_Status *prev;
  int found = 0;

  for (st = child_statuses, prev = NULL; st; prev = st, st = st->next) {
    if (st->pid == pid) {
      if (!done_only || st->done) {
        if (status)
          *status = st->status;
        found = 1;
        if (do_remove) {
          if (prev)
            prev->next = st->next;
          else
            child_statuses = st->next;
        }
        if (do_free)
          free(st);
      }
      break;
    }
  }
  return found;
}

int scheme_get_child_status(int pid, int is_group, int *status) {
  int found = 0;

  if (is_group) {
    /* need to specifically try the pid, since we don't
       wait on other process groups in the background thread */
    pid_t pid2;
    int status;

    do {
      pid2 = waitpid((pid_t)pid, &status, WNOHANG);
    } while ((pid2 == -1) && (errno == EINTR));

    if (pid2 > 0)
      add_child_status(pid, status);
  }

  mzrt_mutex_lock(child_status_lock);
  found = raw_get_child_status(pid, status, 1, 1, 1);
  mzrt_mutex_unlock(child_status_lock);
  /* printf("scheme_get_child_status found %i pid %i status %i\n", found,  pid, *status); */

  return found;
}

int scheme_places_register_child(int pid, int is_group, void *signal_fd, int *status)
{
  int found = 0;

  mzrt_mutex_lock(child_status_lock);

  /* The child may have terminated already: */
  found = raw_get_child_status(pid, status, 0, 0, 0);

  if (!found) {
    /* Create a record for the child: */
    Child_Status *st;
    st = malloc(sizeof(Child_Status));
    st->pid = pid;
    st->signal_fd = signal_fd;
    st->status = 0;
    st->unneeded = 0;
    st->done = 0;
    st->is_group = is_group;

    st->next = child_statuses;
    child_statuses = st;
    st->next_group = NULL;

    if (is_group)
      add_group_signal_fd(signal_fd);
  }

  mzrt_mutex_unlock(child_status_lock);
  return found;
}

static void *mz_proc_thread_signal_worker(void *data) {
  int status;
  int pid, check_pid, is_group;
  sigset_t set;
  Child_Status *group_status, *prev_group, *next;

  sigemptyset(&set);
  sigaddset(&set, SIGCHLD);

  while (1) {
    int rc;
    int signalid;

    do {
      rc = sigwait(&set, &signalid);
      if (rc == -1) {
        if (errno != EINTR) {
          fprintf(stderr, "unexpected error from sigwait(): %d\n", errno);
        }
      }
    } while (rc == -1 && errno == EINTR);

    mzrt_mutex_lock(child_status_lock);
    do_group_signal_fds();
    mzrt_mutex_unlock(child_status_lock);

    mzrt_mutex_lock(child_wait_lock);

    group_status = child_group_statuses;
    prev_group = NULL;

    do {
      if (group_status) {
        check_pid = group_status->pid;
        is_group = 1;
      } else {
        check_pid = 0; /* => processes in the same group as Racket */
        is_group = 0;
      }

      pid = waitpid(check_pid, &status, WNOHANG);

      if (pid == -1) {
        if (errno == EINTR) {
          /* try again */
          pid = 1;
        } else if (!is_group && (errno == ECHILD)) {
          /* no more to check */
        } else {
          fprintf(stderr, "unexpected error from waitpid(%d[%d]): %d\n", 
                  check_pid, is_group, errno);
          if (is_group) {
            prev_group = group_status;
            group_status = group_status->next;
          } 
        }
      } else if (pid > 0) {
        /* printf("SIGCHILD pid %i with status %i %i\n", pid, status, WEXITSTATUS(status)); */
        if (is_group) {
          next = group_status->next;
          if (prev_group)
            prev_group->next_group = next;
          else
            child_group_statuses = next;
          free(group_status);
          group_status = next;
        } else
          add_child_status(pid, status);
      } else {
        if (is_group) {
          prev_group = group_status;
          group_status = group_status->next;
        }
      }
    } while ((pid > 0) || is_group);

    mzrt_mutex_unlock(child_wait_lock);
  }

  return NULL;
}

void scheme_done_with_process_id(int pid, int is_group)
{
  Child_Status *st;

  mzrt_mutex_lock(child_wait_lock); /* protects child_group_statuses */
  mzrt_mutex_lock(child_status_lock);

  for (st = child_statuses; st; st = st->next) {
    if (st->pid == pid) {
      if (!st->done) {
        if (is_group) {
          st->next_group = child_group_statuses;
          child_group_statuses = st;
          if (st->signal_fd)
            remove_group_signal_fd(st->signal_fd);
        } else
          st->unneeded = 1;
        st->signal_fd = NULL;
      }
      break;
    }
  }
      
  if (st && (is_group || st->done)) {
    /* remove it from normal list: */
    raw_get_child_status(pid, NULL, 0, 1, !st->done);
  }

  mzrt_mutex_unlock(child_status_lock);
  mzrt_mutex_unlock(child_wait_lock);
}

static void got_sigchld() XFORM_SKIP_PROC
{ 
  if(-1 == write(2, "SIGCHLD handler called (some thread has SIGCHLD unblocked)\n", 59)) {
    
  }
}

void scheme_places_block_child_signal() XFORM_SKIP_PROC
{
  sigset_t set;
  sigemptyset(&set);
  sigaddset(&set, SIGCHLD);
  sigprocmask(SIG_BLOCK, &set, NULL);

  /* Mac OS X seems to need a handler installed for SIGCHLD to be
     delivered, since the default is to drop the signal. Also, this
     handler serves as a back-up alert if some thread is created that
     does not block SIGCHLD. */
  MZ_SIGSET(SIGCHLD, got_sigchld);
}

void scheme_places_start_child_signal_handler()
{
  mz_proc_thread *signal_thread;

  mzrt_mutex_create(&child_status_lock);
  mzrt_mutex_create(&child_wait_lock);

  signal_thread = mz_proc_thread_create(mz_proc_thread_signal_worker, NULL);
  mz_proc_thread_detach(signal_thread);
}

void scheme_wait_suspend()
{
  mzrt_mutex_lock(child_wait_lock);
}

void scheme_wait_resume()
{
  mzrt_mutex_unlock(child_wait_lock);
}

/* ---------------------------------------------------------------------- */

/* When a place has a process-group that it may be waiting on, the we
   need to wake up the place whenever any SIGCHLD is received, since
   the SIGDCHLD may apply to one of those places.
   The list of signal_fds is protected by the status lock. */

typedef struct Group_Signal_FD {
  void *signal_fd;
  int refcount;
} Group_Signal_FD;

SHARED_OK static Group_Signal_FD *signal_fds;
SHARED_OK static int signal_fd_count;

static void add_group_signal_fd(void *signal_fd)
{
  int i, count = 0;
  Group_Signal_FD *a;

  for (i = 0; i < signal_fd_count; i++) {
    if (signal_fds[i].refcount) {
      count++;
      if (signal_fds[i].signal_fd == signal_fd) {
        signal_fds[i].refcount++;
        return;
      }      
    }
  }

  if (count == signal_fd_count) {
    signal_fd_count = (signal_fd_count + 4) * 2;
    a = (Group_Signal_FD *)malloc(sizeof(Group_Signal_FD) * signal_fd_count);
    memset(a, 0, sizeof(Group_Signal_FD) * signal_fd_count);
    memcpy(a, signal_fds, sizeof(Group_Signal_FD) * count);
    signal_fds = a;
  }

  for (i = 0; i < signal_fd_count; i++) {
    if (!signal_fds[i].refcount) {
      signal_fds[i].signal_fd = signal_fd;
      signal_fds[i].refcount = 1;
      break;
    }
  }
}

static void remove_group_signal_fd(void *signal_fd)
{
  int i;

  for (i = 0; i < signal_fd_count; i++) {
    if (signal_fds[i].refcount) {
      if (signal_fds[i].signal_fd == signal_fd) {
        --signal_fds[i].refcount;
        return;
      }
    }
  }
}

static void do_group_signal_fds()
{
  int i;

  for (i = 0; i < signal_fd_count; i++) {
    if (signal_fds[i].refcount) {
      scheme_signal_received_at(signal_fds[i].signal_fd);
    }
  }
}

#endif

# if defined(MZ_PRECISE_GC)

/*============= THREAD JOIN HANDLER =============*/
typedef struct {
  mz_proc_thread *proc_thread;
  Scheme_Place   *waiting_place; 
  int            *wake_fd;
  int             ready;
  long            rc;
} proc_thread_wait_data;


static void *mz_proc_thread_wait_worker(void *data) {
  void           *rc;
  proc_thread_wait_data *wd = (proc_thread_wait_data*) data;

  rc = mz_proc_thread_wait(wd->proc_thread);
  wd->rc = (long) rc;
  wd->ready = 1;
  scheme_signal_received_at(wd->wake_fd);
  return NULL;
}

static int place_wait_ready(Scheme_Object *o) {
  proc_thread_wait_data *wd = (proc_thread_wait_data*) o;
  if (wd->ready) {
    return 1;
  }
  return 0;
}
# endif

static Scheme_Object *scheme_place_wait(int argc, Scheme_Object *args[]) {
  Scheme_Place          *place;
  place = (Scheme_Place *) args[0];

  if (argc != 1) {
    scheme_wrong_count_m("place-wait", 1, 1, argc, args, 0);
  }
  if (!SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type)) {
    scheme_wrong_type("place-wait", "place", 0, argc, args);
  }
 
# ifdef MZ_PRECISE_GC
   {
    Scheme_Object *rc;
    mz_proc_thread *worker_thread;
    Scheme_Place *waiting_place;
    int *wake_fd;

    proc_thread_wait_data *wd;
    wd = (proc_thread_wait_data*) malloc(sizeof(proc_thread_wait_data));
    wd->proc_thread = (mz_proc_thread *)place->proc_thread;
    wd->waiting_place = waiting_place;
    wake_fd = scheme_get_signal_handle();
    wd->wake_fd = wake_fd;
    wd->ready   = 0;

    worker_thread = mz_proc_thread_create(mz_proc_thread_wait_worker, wd);
    mz_proc_thread_detach(worker_thread);
    scheme_block_until(place_wait_ready, NULL, (Scheme_Object *) wd, 0);

    rc = scheme_make_integer((long)wd->rc);
    free(wd);
    return rc;
  }
# else
  {
    void *rcvoid;
    rcvoid = mz_proc_thread_wait((mz_proc_thread *)place->proc_thread);
    return scheme_make_integer((long) rcvoid);
  }
# endif
}

static Scheme_Object *scheme_place_p(int argc, Scheme_Object *args[])
{
  return SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type) ? scheme_true : scheme_false;
}

Scheme_Object *scheme_places_deep_copy(Scheme_Object *so) {
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  Scheme_Object *new_so = so;
  if (SCHEME_INTP(so)) {
    return so;
  }

  switch (so->type) {
    case scheme_pair_type:
    case scheme_vector_type:
    case scheme_struct_type_type:
    case scheme_structure_type:
      {
        Scheme_Hash_Table *ht;
        ht = scheme_make_hash_table(SCHEME_hash_ptr);
        new_so = scheme_places_deep_copy_worker(so, ht);
      }
      break;
    default:
      new_so = scheme_places_deep_copy_worker(so, NULL);
      break;
  }
  return new_so;
#else
  return so;
#endif
}

#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
Scheme_Object *scheme_places_deep_copy_worker(Scheme_Object *so, Scheme_Hash_Table *ht)
{
  Scheme_Object *new_so = so;
  if (SCHEME_INTP(so)) {
    return so;
  }
  if (ht) {
    Scheme_Object *r; 
    if ((r = scheme_hash_get(ht, so))) {
      return r;
    }
  }

  switch (so->type) {
    case scheme_true_type:
    case scheme_false_type:
    case scheme_null_type:
    /* place_bi_channels are allocated in the master and can be passed along as is */
    case scheme_place_bi_channel_type:
      new_so = so;
      break;
    case scheme_char_type:
      new_so = scheme_make_char(SCHEME_CHAR_VAL(so));
      break;
    case scheme_rational_type:
      {
        Scheme_Object *n;
        Scheme_Object *d;
        n = scheme_rational_numerator(so);
        d = scheme_rational_denominator(so);
        n = scheme_places_deep_copy_worker(n, ht);
        d = scheme_places_deep_copy_worker(d, ht);
        new_so = scheme_make_rational(n, d);
      }
      break;
    case scheme_float_type:
      new_so = scheme_make_char(SCHEME_FLT_VAL(so));
      break;
    case scheme_double_type:
      new_so = scheme_make_char(SCHEME_DBL_VAL(so));
      break;
    case scheme_complex_type:
      {
        Scheme_Object *r;
        Scheme_Object *i;
        r = scheme_complex_real_part(so);
        i = scheme_complex_imaginary_part(so);
        r = scheme_places_deep_copy_worker(r, ht);
        i = scheme_places_deep_copy_worker(i, ht);
        new_so = scheme_make_complex(r, i);
      }
      break;
    case scheme_char_string_type:
      new_so = scheme_make_sized_offset_char_string(SCHEME_CHAR_STR_VAL(so), 0, SCHEME_CHAR_STRLEN_VAL(so), 1);
      break;
    case scheme_byte_string_type:
      if (SHARED_ALLOCATEDP(so)) {
        new_so = so;
      }
      else {
        new_so = scheme_make_sized_offset_byte_string(SCHEME_BYTE_STR_VAL(so), 0, SCHEME_BYTE_STRLEN_VAL(so), 1);
      }
      break;
    case scheme_unix_path_type:
      new_so = scheme_make_sized_offset_path(SCHEME_BYTE_STR_VAL(so), 0, SCHEME_BYTE_STRLEN_VAL(so), 1);
      break;
    case scheme_symbol_type:
      if (SCHEME_SYM_UNINTERNEDP(so)) {
        scheme_log_abort("cannot copy uninterned symbol");
        abort();
      } else {
        new_so = scheme_make_sized_offset_byte_string(SCHEME_SYM_VAL(so), 0, SCHEME_SYM_LEN(so), 1);
        new_so->type = scheme_serialized_symbol_type;
      }
      break;
    case scheme_serialized_symbol_type:
        new_so = scheme_intern_exact_symbol(SCHEME_BYTE_STR_VAL(so), SCHEME_BYTE_STRLEN_VAL(so));
      break;
    case scheme_pair_type:
      {
        Scheme_Object *car;
        Scheme_Object *cdr;
        Scheme_Object *pair;
        car = scheme_places_deep_copy_worker(SCHEME_CAR(so), ht);
        cdr = scheme_places_deep_copy_worker(SCHEME_CDR(so), ht);
        pair = scheme_make_pair(car, cdr);
        new_so = pair;
      }
      break;
    case scheme_vector_type:
      {
        Scheme_Object *vec;
        long i;
        long size = SCHEME_VEC_SIZE(so);
        vec = scheme_make_vector(size, 0);
        for (i = 0; i <size ; i++) {
          Scheme_Object *tmp;
          tmp = scheme_places_deep_copy_worker(SCHEME_VEC_ELS(so)[i], ht);
          SCHEME_VEC_ELS(vec)[i] = tmp;
        }
        SCHEME_SET_IMMUTABLE(vec);
        new_so = vec;
      }
      break;
    case scheme_flvector_type:
      if (SHARED_ALLOCATEDP(so)) {
        new_so = so;
      }
      else {
        Scheme_Double_Vector *vec;
        long i;
        long size = SCHEME_FLVEC_SIZE(so);
        vec = scheme_alloc_flvector(size);

        for (i = 0; i < size; i++) {
          SCHEME_FLVEC_ELS(vec)[i] = SCHEME_FLVEC_ELS(so)[i];
        }
        new_so = (Scheme_Object *) vec;
      }
      break;
    case scheme_structure_type:
      {
        Scheme_Structure *st = (Scheme_Structure*)so;
        Scheme_Serialized_Structure *nst;
        Scheme_Struct_Type *stype = st->stype;
        Scheme_Struct_Type *ptype = stype->parent_types[stype->name_pos - 1];
        Scheme_Object *nprefab_key;
        long size = stype->num_slots;
        int local_slots = stype->num_slots - (ptype ? ptype->num_slots : 0);
        int i = 0;

        if (!stype->prefab_key) {
          scheme_log_abort("cannot copy non prefab structure");
          abort();
        }
        {
          for (i = 0; i < local_slots; i++) {
            if (!stype->immutables || stype->immutables[i] != 1) {
              scheme_log_abort("cannot copy mutable prefab structure");
              abort();
            }
          }
        }
        nprefab_key = scheme_places_deep_copy_worker(stype->prefab_key, ht);
        nst = (Scheme_Serialized_Structure*) scheme_make_serialized_struct_instance(nprefab_key, size);
        for (i = 0; i <size ; i++) {
          Scheme_Object *tmp;
          tmp = scheme_places_deep_copy_worker((Scheme_Object*) st->slots[i], ht);
          nst->slots[i] = tmp;
        }
        new_so = (Scheme_Object*) nst;
      }
      break;

    case scheme_serialized_structure_type:
      {
        Scheme_Serialized_Structure *st = (Scheme_Serialized_Structure*)so;
        Scheme_Struct_Type *stype;
        Scheme_Structure *nst;
        long size;
        int i = 0;
      
        size = st->num_slots;
        stype = scheme_lookup_prefab_type(SCHEME_CDR(st->prefab_key), size);
        nst = (Scheme_Structure*) scheme_make_blank_prefab_struct_instance(stype);
        for (i = 0; i <size ; i++) {
          Scheme_Object *tmp;
          tmp = scheme_places_deep_copy_worker((Scheme_Object*) st->slots[i], ht);
          nst->slots[i] = tmp;
        }
        new_so = (Scheme_Object*)nst;
      }
      break;

    case scheme_resolved_module_path_type:
    default:
      scheme_log_abort("cannot copy object");
      abort();
      break;
  }
  if (ht) {
    scheme_hash_set(ht, so, new_so);
  }
  return new_so;
}
#endif


#if 0
/* unused code, may be useful when/if we revive shared symbol and prefab key tables */
Scheme_Struct_Type *scheme_make_prefab_struct_type_in_master(Scheme_Object *base,
					Scheme_Object *parent,
					int num_fields,
					int num_uninit_fields,
					Scheme_Object *uninit_val,
					char *immutable_array)
{
# ifdef MZ_PRECISE_GC
  void *original_gc;
# endif
  Scheme_Object *cname;
  Scheme_Object *cuninit_val;
  char *cimm_array = NULL;
  int local_slots = num_fields + num_uninit_fields;
  Scheme_Struct_Type *stype;

# ifdef MZ_PRECISE_GC
  original_gc = GC_switch_to_master_gc();
  scheme_start_atomic();
# endif

  cname = scheme_places_deep_copy(base);
  cuninit_val = scheme_places_deep_copy(uninit_val);
  if (local_slots) {
    cimm_array   = (char *)scheme_malloc_atomic(local_slots);
    memcpy(cimm_array, immutable_array, local_slots);
  }
  stype = scheme_make_prefab_struct_type_raw(cname, parent, num_fields, num_uninit_fields, cuninit_val, cimm_array);

# ifdef MZ_PRECISE_GC
  scheme_end_atomic_no_swap();
  GC_switch_back_from_master(original_gc);
# endif

  return stype;
}
#endif

static void *place_start_proc(void *data_arg) {
  void *stack_base;
  void *rc;
  stack_base = PROMPT_STACK(stack_base);
  rc = place_start_proc_after_stack(data_arg, stack_base);
  stack_base = NULL;
  return rc;
}
  
static void *place_start_proc_after_stack(void *data_arg, void *stack_base) {
  Place_Start_Data *place_data;
  Scheme_Object *place_main;
  Scheme_Object *a[2], *channel;
  mzrt_thread_id ptid;
  long rc = 0;
  ptid = mz_proc_thread_self();
  
  place_data = (Place_Start_Data *) data_arg;
  data_arg = NULL;
 
  /* printf("Startin place: proc thread id%u\n", ptid); */

  /* create pristine THREAD_LOCAL variables*/
  null_out_runtime_globals();

  /* scheme_make_thread behaves differently if the above global vars are not null */
  scheme_place_instance_init(stack_base);

  a[0] = scheme_places_deep_copy(place_data->current_library_collection_paths);
  scheme_current_library_collection_paths(1, a);

  a[0] = scheme_places_deep_copy(place_data->module);
  a[1] = scheme_places_deep_copy(place_data->function);
  a[1] = scheme_intern_exact_symbol(SCHEME_SYM_VAL(a[1]), SCHEME_SYM_LEN(a[1]));
  if (!SAME_TYPE(SCHEME_TYPE(place_data->channel), scheme_place_bi_channel_type)) {
    channel = scheme_places_deep_copy(place_data->channel);
  }
  else {
    channel = place_data->channel;
  }

  mzrt_sema_post(place_data->ready);
  place_data = NULL;
# ifdef MZ_PRECISE_GC
  /* this prevents a master collection attempt from deadlocking with the 
     place_data->ready semaphore above */
  GC_allow_master_gc_check();
# endif


  /* at point point, don't refer to place_data or its content
     anymore, because it's allocated in the other place */

  scheme_set_root_param(MZCONFIG_EXIT_HANDLER, scheme_def_place_exit_proc);

  {
    Scheme_Thread * volatile p;
    mz_jmp_buf * volatile saved_error_buf;
    mz_jmp_buf new_error_buf;

    p = scheme_get_current_thread();
    saved_error_buf = p->error_buf;
    p->error_buf = &new_error_buf;
    if (!scheme_setjmp(new_error_buf)) {
      Scheme_Object *dynamic_require;
      dynamic_require = scheme_builtin_value("dynamic-require");
      place_main = scheme_apply(dynamic_require, 2, a);
      a[0] = channel;
      scheme_apply(place_main, 1, a);
    }
    else {
      rc = 1;
    }
    p->error_buf = saved_error_buf;
  }

  /*printf("Leavin place: proc thread id%u\n", ptid);*/
  scheme_place_instance_destroy();

  return (void*) rc;
}

# ifdef MZ_PRECISE_GC
Scheme_Hash_Table *force_hash(Scheme_Object *so);
# endif

Scheme_Object *scheme_places_deep_copy_to_master(Scheme_Object *so) {
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  Scheme_Object *o;
  void *original_gc;
  Scheme_Hash_Table *ht;

  ht = force_hash(so);

  original_gc = GC_switch_to_master_gc();
  scheme_start_atomic();

  o = scheme_places_deep_copy_worker(so, ht);

  scheme_end_atomic_no_swap();
  GC_switch_back_from_master(original_gc);
  return o;
#else
  return so;
#endif
}

Scheme_Object *scheme_places_deserialize_worker(Scheme_Object *so)
{
  Scheme_Object *new_so = so;
  if (SCHEME_INTP(so)) {
    return so;
  }
  switch (so->type) {
    case scheme_true_type:
    case scheme_false_type:
    case scheme_null_type:
    /* place_bi_channels are allocated in the master and can be passed along as is */
    case scheme_place_bi_channel_type:
    case scheme_char_type:
    case scheme_rational_type:
    case scheme_float_type:
    case scheme_double_type:
    case scheme_complex_type:
    case scheme_char_string_type:
    case scheme_byte_string_type:
    case scheme_unix_path_type:
    case scheme_flvector_type:
        new_so = so;
      break;
    case scheme_symbol_type:
      scheme_log_abort("scheme_symbol_type: shouldn't be seen during deserialization step");
      break;
    case scheme_serialized_symbol_type:
        new_so = scheme_intern_exact_symbol(SCHEME_BYTE_STR_VAL(so), SCHEME_BYTE_STRLEN_VAL(so));
      break;
    case scheme_pair_type:
      {
        Scheme_Object *tmp;
        tmp = scheme_places_deserialize_worker(SCHEME_CAR(so));
        SCHEME_CAR(so) = tmp;
        tmp = scheme_places_deserialize_worker(SCHEME_CDR(so));
        SCHEME_CDR(so) = tmp;
        new_so = so;
      }
      break;
    case scheme_vector_type:
      {
        long i;
        long size = SCHEME_VEC_SIZE(so);
        for (i = 0; i <size ; i++) {
          Scheme_Object *tmp;
          tmp = scheme_places_deserialize_worker(SCHEME_VEC_ELS(so)[i]);
          SCHEME_VEC_ELS(so)[i] = tmp;
        }
        new_so = so;
      }
      break;
    case scheme_structure_type:
      scheme_log_abort("scheme_structure_type: shouldn't be seen during deserialization step");
      break;
    case scheme_serialized_structure_type:
      {
        Scheme_Serialized_Structure *st = (Scheme_Serialized_Structure*)so;
        Scheme_Struct_Type *stype;
        Scheme_Structure *nst;
        long size;
        int i = 0;
      
        size = st->num_slots;
        stype = scheme_lookup_prefab_type(SCHEME_CDR(st->prefab_key), size);
        nst = (Scheme_Structure*) scheme_make_blank_prefab_struct_instance(stype);
        for (i = 0; i <size ; i++) {
          Scheme_Object *tmp;
          tmp = scheme_places_deserialize_worker((Scheme_Object*) st->slots[i]);
          nst->slots[i] = tmp;
        }
        new_so = (Scheme_Object*)nst;
      }
      break;

    case scheme_resolved_module_path_type:
    default:
      scheme_log_abort("cannot deserialize object");
      abort();
      break;
  }
  return new_so;
}

Scheme_Object *scheme_places_serialize(Scheme_Object *so, void **msg_memory) {
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  Scheme_Object *new_so;
  Scheme_Object *tmp;
  GC_create_message_allocator();
  new_so = scheme_places_deep_copy(so);
  tmp = GC_finish_message_allocator();
  (*msg_memory) = tmp;
  return new_so;
#else
  return so;
#endif
}

Scheme_Object *scheme_places_deserialize(Scheme_Object *so, void *msg_memory) {
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  Scheme_Object *new_so;
  new_so = scheme_places_deserialize_worker(so);
  GC_adopt_message_allocator(msg_memory);
  return new_so;
#else
  return so;
#endif
}

Scheme_Object *scheme_place_send(int argc, Scheme_Object *args[]) {
  if (argc == 2) {
    Scheme_Object *mso;
    Scheme_Place_Bi_Channel *ch;
    if (SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type)) {
      ch = (Scheme_Place_Bi_Channel *) ((Scheme_Place *) args[0])->channel;
    }
    else if (SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_bi_channel_type)) {
      ch = (Scheme_Place_Bi_Channel *) args[0];
    }
    else {
      ch = NULL;
      scheme_wrong_type("place-channel-send", "place-channel", 0, argc, args);
    }
    {
      void *msg_memory;
      mso = scheme_places_serialize(args[1], &msg_memory);
      scheme_place_async_send((Scheme_Place_Async_Channel *) ch->sendch, mso, msg_memory);
    }
  }
  else {
    scheme_wrong_count_m("place-channel-send", 2, 2, argc, args, 0);
  }
  return scheme_true;
}

Scheme_Object *scheme_place_recv(int argc, Scheme_Object *args[]) {
  if (argc == 1) {
    Scheme_Object *mso;
    Scheme_Place_Bi_Channel *ch;
    if (SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type)) {
      ch = (Scheme_Place_Bi_Channel *) ((Scheme_Place *) args[0])->channel;
    }
    else if (SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_bi_channel_type)) {
      ch = (Scheme_Place_Bi_Channel *) args[0];
    }
    else {
      ch = NULL;
      scheme_wrong_type("place-channel-recv", "place-channel", 0, argc, args);
    }
    {
      void *msg_memory = NULL;
      mso = scheme_place_async_recv((Scheme_Place_Async_Channel *) ch->recvch, &msg_memory);
      return scheme_places_deserialize(mso, msg_memory);
    }
  }
  else {
    scheme_wrong_count_m("place-channel-recv", 1, 1, argc, args, 0);
  }
  return scheme_true;
}

# ifdef MZ_PRECISE_GC
void force_hash_worker(Scheme_Object *so, Scheme_Hash_Table *ht);
Scheme_Hash_Table *force_hash(Scheme_Object *so) {
  if (SCHEME_INTP(so)) {
    return NULL;
  }

  switch (so->type) {
    case scheme_pair_type:
    case scheme_vector_type:
    case scheme_struct_type_type:
    case scheme_structure_type:
      {
        Scheme_Hash_Table *ht;
        ht = scheme_make_hash_table(SCHEME_hash_ptr);
        force_hash_worker(so, ht);
        return ht;
      }
      break;
    default:
      break;
  }
  return NULL;
}

void force_hash_worker(Scheme_Object *so, Scheme_Hash_Table *ht)
{
  if (SCHEME_INTP(so)) {
    return;
  }
  if (ht) {
    Scheme_Object *r; 
    if ((r = scheme_hash_get(ht, so))) {
      return;
    }
  }

  switch (so->type) {
    case scheme_true_type:
    case scheme_false_type:
    case scheme_null_type:
    case scheme_char_type:
    case scheme_rational_type:
    case scheme_float_type:
    case scheme_double_type:
    case scheme_complex_type:
    case scheme_char_string_type:
    case scheme_byte_string_type:
    case scheme_unix_path_type:
    case scheme_symbol_type:
    case scheme_place_bi_channel_type:
    case scheme_flvector_type:
      break;
    case scheme_pair_type:
      {
        force_hash_worker(SCHEME_CAR(so), ht);
        force_hash_worker(SCHEME_CDR(so), ht);
      }
      break;
    case scheme_vector_type:
      {
        long i;
        long size = SCHEME_VEC_SIZE(so);
        for (i = 0; i <size ; i++) {
          force_hash_worker(SCHEME_VEC_ELS(so)[i], ht);
        }
      }
      break;
    case scheme_structure_type:
      {
        Scheme_Structure *st = (Scheme_Structure*)so;
        Scheme_Struct_Type *stype = st->stype;
        long i;
        long size = stype->num_slots;

        if (stype->prefab_key)
          force_hash_worker((Scheme_Object*)stype->prefab_key, ht);
        
        for (i = 0; i <size ; i++) {
          force_hash_worker((Scheme_Object*) st->slots[i], ht);
        }
      }
      break;
    case scheme_resolved_module_path_type:
    default:
      scheme_log_abort("cannot force hash");
      abort();
      break;
  }
  if (ht) {
    scheme_hash_set(ht, so, NULL);
  }
  return;
}

void scheme_spawn_master_place() {
  mzrt_proc_first_thread_init();
  

  /* scheme_master_proc_thread = mz_proc_thread_create(master_scheme_place, NULL); */
  scheme_master_proc_thread = (void*) ~0;

}
# endif

/*========================================================================*/
/*                       places async channels                            */
/*========================================================================*/

static void* GC_master_malloc(size_t size) {
  void *ptr;
#ifdef MZ_PRECISE_GC
  void *original_gc;
  original_gc = GC_switch_to_master_gc();
#endif
  ptr = GC_malloc(size);
#ifdef MZ_PRECISE_GC
  GC_switch_back_from_master(original_gc);
#endif
  return ptr;
}

static void* GC_master_malloc_tagged(size_t size) {
  void *ptr;
#ifdef MZ_PRECISE_GC
  void *original_gc;
  original_gc = GC_switch_to_master_gc();
#endif
  ptr = scheme_malloc_small_tagged(size);
#ifdef MZ_PRECISE_GC
  GC_switch_back_from_master(original_gc);
#endif
  return ptr;
}

Scheme_Place_Async_Channel *scheme_place_async_channel_create() {
  Scheme_Object **msgs;
  Scheme_Place_Async_Channel *ch;
  void **msg_memory;

  ch = GC_master_malloc_tagged(sizeof(Scheme_Place_Async_Channel));
  msgs = GC_master_malloc(sizeof(Scheme_Object*) * 8);
  msg_memory = GC_master_malloc(sizeof(void*) * 8);

  ch->so.type = scheme_place_async_channel_type;
  ch->in = 0;
  ch->out = 0;
  ch->count = 0;
  ch->size = 8;
  mzrt_mutex_create(&ch->lock);
  ch->msgs = msgs;
  ch->msg_memory = msg_memory;
  ch->wakeup_signal = NULL;
  return ch;
}

Scheme_Place_Bi_Channel *scheme_place_bi_channel_create() {
  Scheme_Place_Async_Channel *tmp;
  Scheme_Place_Bi_Channel *ch;

  ch = GC_master_malloc_tagged(sizeof(Scheme_Place_Bi_Channel));
  ch->so.type = scheme_place_bi_channel_type;

  tmp = scheme_place_async_channel_create();
  ch->sendch = tmp;
  tmp = scheme_place_async_channel_create();
  ch->recvch = tmp;
  return ch;
}

Scheme_Place_Bi_Channel *scheme_place_bi_peer_channel_create(Scheme_Place_Bi_Channel *orig) {
  Scheme_Place_Bi_Channel *ch;

  ch = GC_master_malloc_tagged(sizeof(Scheme_Place_Bi_Channel));
  ch->so.type = scheme_place_bi_channel_type;

  ch->sendch = orig->recvch;
  ch->recvch = orig->sendch;
  return ch;
}

static Scheme_Object *scheme_place_channel(int argc, Scheme_Object *args[]) {
  if (argc == 0) {
    Scheme_Place_Bi_Channel *ch;
    Scheme_Object *a[2];
    ch = scheme_place_bi_channel_create();
    a[0] = (Scheme_Object *) ch;
    a[1] = (Scheme_Object *) scheme_place_bi_peer_channel_create(ch);
    return scheme_values(2, a);
  }
  else {
    scheme_wrong_count_m("place-channel", 0, 0, argc, args, 0);
  }
  return scheme_true;
}

static Scheme_Object *scheme_place_channel_p(int argc, Scheme_Object *args[])
{
  return SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_bi_channel_type) ? scheme_true : scheme_false;
}

static void scheme_place_async_send(Scheme_Place_Async_Channel *ch, Scheme_Object *o, void *msg_memory) {
  int cnt;
  mzrt_mutex_lock(ch->lock);
  {
    cnt = ch->count;
    if (ch->count == ch->size) { /* GROW QUEUE */
      Scheme_Object **new_msgs;
      void **new_msg_memory;

      new_msgs = GC_master_malloc(sizeof(Scheme_Object*) * ch->size * 2);
      new_msg_memory = GC_master_malloc(sizeof(void*) * ch->size * 2);

      if (ch->out < ch->in) {
        memcpy(new_msgs, ch->msgs + ch->out, sizeof(Scheme_Object *) * (ch->in - ch->out));
        memcpy(new_msg_memory, ch->msg_memory + ch->out, sizeof(void*) * (ch->in - ch->out));
      }
      else {
        int s1 = (ch->size - ch->out);
        memcpy(new_msgs, ch->msgs + ch->out, sizeof(Scheme_Object *) * s1);
        memcpy(new_msgs + s1, ch->msgs, sizeof(Scheme_Object *) * ch->in);

        memcpy(new_msg_memory, ch->msg_memory + ch->out, sizeof(void*) * s1);
        memcpy(new_msg_memory + s1, ch->msg_memory, sizeof(void*) * ch->in);
      }
      
      ch->msgs = new_msgs;
      ch->msg_memory = new_msg_memory;
      ch->in = ch->size;
      ch->out = 0;
      ch->size *= 2;
    }

    ch->msgs[ch->in] = o;
    ch->msg_memory[ch->in] = msg_memory;
    ++ch->count;
    ch->in = (++ch->in % ch->size);
  }
  mzrt_mutex_unlock(ch->lock);

  if (!cnt && ch->wakeup_signal) {
    /*wake up possibly sleeping receiver */  
    scheme_signal_received_at(ch->wakeup_signal);
  }
}

static int scheme_place_async_ch_ready(Scheme_Place_Async_Channel *ch) {
  int ready = 0;
  mzrt_mutex_lock(ch->lock);
  {
    if (ch->count > 0) ready = 1;
  }
  mzrt_mutex_unlock(ch->lock);
  return ready;
}

static int scheme_place_channel_ready(Scheme_Object *so) {
  Scheme_Place_Bi_Channel *ch;
  if (SAME_TYPE(SCHEME_TYPE(so), scheme_place_type)) {
    ch = (Scheme_Place_Bi_Channel *) ((Scheme_Place *) so)->channel;
  }
  else {
    ch = (Scheme_Place_Bi_Channel *)so;
  }

  return scheme_place_async_ch_ready((Scheme_Place_Async_Channel *) ch->recvch);
}

static Scheme_Object *scheme_place_async_recv(Scheme_Place_Async_Channel *ch, void **msg_memory) {
  Scheme_Object *msg = NULL;
  while(1) {
    mzrt_mutex_lock(ch->lock);
    {
      if (ch->count > 0) { /* GET MSG */
        msg = ch->msgs[ch->out];
        *msg_memory = ch->msg_memory[ch->out];

        ch->msgs[ch->out] = NULL;
        ch->msg_memory[ch->out] = NULL;

        --ch->count;
        ch->out = (++ch->out % ch->size);
      }
    }
    mzrt_mutex_unlock(ch->lock);
    if(msg) break;
    else {
      void *signaldescr;
      signaldescr = scheme_get_signal_handle();
      ch->wakeup_signal = signaldescr;
    
      scheme_thread_block(0);
      scheme_block_until((Scheme_Ready_Fun) scheme_place_async_ch_ready, NULL, (Scheme_Object *) ch, 0);
    }
  }
  return msg;
}

/*========================================================================*/
/*                       precise GC traversers                            */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_PLACES_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_place_type, place_val);
  GC_REG_TRAV(scheme_place_async_channel_type, place_async_channel_val);
  GC_REG_TRAV(scheme_place_bi_channel_type, place_bi_channel_val);
}

END_XFORM_SKIP;

#endif

/************************************************************************/
/************************************************************************/
/************************************************************************/

#endif
