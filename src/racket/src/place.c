#include "schpriv.h"

static Scheme_Object* scheme_place_enabled(int argc, Scheme_Object *args[]);
static Scheme_Object* scheme_place_shared(int argc, Scheme_Object *args[]);

THREAD_LOCAL_DECL(int scheme_current_place_id);

#ifdef MZ_USE_PLACES

#include "mzrt.h"
#ifdef UNIX_PROCESSES
# include <unistd.h>
#endif

#include "schmach.h"

READ_ONLY static Scheme_Object *scheme_def_place_exit_proc;
SHARED_OK static int scheme_places_enabled = 1;

static int id_counter;
static mzrt_mutex *id_counter_mutex;

SHARED_OK mz_proc_thread *scheme_master_proc_thread;
THREAD_LOCAL_DECL(struct Scheme_Place_Object *place_object);
static Scheme_Object *scheme_place(int argc, Scheme_Object *args[]);
static Scheme_Object *place_wait(int argc, Scheme_Object *args[]);
static Scheme_Object *place_kill(int argc, Scheme_Object *args[]);
static Scheme_Object *place_break(int argc, Scheme_Object *args[]);
static Scheme_Object *place_sleep(int argc, Scheme_Object *args[]);
static Scheme_Object *place_p(int argc, Scheme_Object *args[]);
static Scheme_Object *place_send(int argc, Scheme_Object *args[]);
static Scheme_Object *place_receive(int argc, Scheme_Object *args[]);
static Scheme_Object *place_channel_p(int argc, Scheme_Object *args[]);
static Scheme_Object *def_place_exit_handler_proc(int argc, Scheme_Object *args[]);
static Scheme_Object *place_channel(int argc, Scheme_Object *args[]);
static Scheme_Object* place_allowed_p(int argc, Scheme_Object *args[]);
static void cust_kill_place(Scheme_Object *pl, void *notused);

static Scheme_Place_Async_Channel *place_async_channel_create();
static Scheme_Place_Bi_Channel *place_bi_channel_create();
static Scheme_Place_Bi_Channel *place_bi_peer_channel_create(Scheme_Place_Bi_Channel *orig);
static int place_channel_ready(Scheme_Object *so, Scheme_Schedule_Info *sinfo);
static void place_async_send(Scheme_Place_Async_Channel *ch, Scheme_Object *o);
static Scheme_Object *place_async_receive(Scheme_Place_Async_Channel *ch);
static Scheme_Object *places_deep_copy_to_master(Scheme_Object *so);
static Scheme_Object *make_place_dead(int argc, Scheme_Object *argv[]);
static int place_dead_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo);
static void* GC_master_malloc_tagged(size_t size);

#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
static Scheme_Object *places_deep_copy_worker(Scheme_Object *so, Scheme_Hash_Table **ht, 
                                              int copy, int gcable, int can_raise_exn);
#endif

# ifdef MZ_PRECISE_GC
static void register_traversers(void);
# endif

static void *place_start_proc(void *arg);
static void *place_start_proc_after_stack(void *data_arg, void *stack_base);

# define PLACE_PRIM_W_ARITY(name, func, a1, a2, env) GLOBAL_PRIM_W_ARITY(name, func, a1, a2, env)

#else

SHARED_OK static int scheme_places_enabled = 0;

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

  GLOBAL_PRIM_W_ARITY("place-enabled?",       scheme_place_enabled,   0, 0, plenv);
  GLOBAL_PRIM_W_ARITY("place-shared?",        scheme_place_shared,    1, 1, plenv);
  PLACE_PRIM_W_ARITY("dynamic-place",         scheme_place,           2, 2, plenv);
  PLACE_PRIM_W_ARITY("place-sleep",           place_sleep,     1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-wait",            place_wait,      1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-kill",            place_kill,      1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-break",           place_break,     1, 1, plenv);
  PLACE_PRIM_W_ARITY("place?",                place_p,         1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-channel",         place_channel,   0, 0, plenv);
  PLACE_PRIM_W_ARITY("place-channel-put",     place_send,      2, 2, plenv);
  PLACE_PRIM_W_ARITY("place-channel-get",     place_receive,   1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-channel?",        place_channel_p, 1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-message-allowed?", place_allowed_p, 1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-dead-evt",        make_place_dead, 1, 1, plenv);

#ifdef MZ_USE_PLACES
  REGISTER_SO(scheme_def_place_exit_proc);
  scheme_def_place_exit_proc = scheme_make_prim_w_arity(def_place_exit_handler_proc, "default-place-exit-handler", 1, 1);
#endif
  scheme_finish_primitive_module(plenv);

}

static Scheme_Object* scheme_place_enabled(int argc, Scheme_Object *args[]) {
  return (scheme_places_enabled == 0) ? scheme_false : scheme_true;
}

static Scheme_Object* scheme_place_shared(int argc, Scheme_Object *args[]) {
  return SHARED_ALLOCATEDP(args[0]) ? scheme_true : scheme_false;
}

void scheme_init_places_once() {
#ifdef MZ_USE_PLACES
  scheme_add_evt(scheme_place_type,            (Scheme_Ready_Fun)place_channel_ready, NULL, NULL, 1); 
  scheme_add_evt(scheme_place_bi_channel_type, (Scheme_Ready_Fun)place_channel_ready, NULL, NULL, 1);
  scheme_add_evt(scheme_place_dead_type,       (Scheme_Ready_Fun)place_dead_ready, NULL, NULL, 1);
  mzrt_mutex_create(&id_counter_mutex);
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
  mzrt_sema *ready;  /* malloc'ed item */
  struct Scheme_Place_Object *place_obj;   /* malloc'ed item */
} Place_Start_Data;

static void null_out_runtime_globals() {
  scheme_current_thread           = NULL;
  scheme_first_thread             = NULL;
  scheme_main_thread              = NULL;
                                                                   
  scheme_current_runstack_start   = NULL;
  scheme_current_runstack         = NULL;
  scheme_current_cont_mark_stack  = 0;
  scheme_current_cont_mark_pos    = 0;
}

Scheme_Object *place_sleep(int argc, Scheme_Object *args[]) {
  mzrt_sleep(SCHEME_INT_VAL(args[0]));
  return scheme_void;
}

Scheme_Object *scheme_make_place_object() {
  Scheme_Place_Object   *place_obj;
  place_obj = GC_master_malloc_tagged(sizeof(Scheme_Place_Object));
  place_obj->so.type = scheme_place_object_type;
  mzrt_mutex_create(&place_obj->lock);
  place_obj->die = 0;
  place_obj->pbreak = 0;
  place_obj->result = 1;
  return (Scheme_Object *)place_obj;
}

Scheme_Object *scheme_place(int argc, Scheme_Object *args[]) {
  Scheme_Place          *place;
  Place_Start_Data      *place_data;
  mz_proc_thread        *proc_thread;
  Scheme_Object         *collection_paths;
  Scheme_Place_Object   *place_obj;
  mzrt_sema             *ready;

  /* create place object */
  place = MALLOC_ONE_TAGGED(Scheme_Place);
  place->so.type = scheme_place_type;
  place_obj = (Scheme_Place_Object *) scheme_make_place_object();
  place->place_obj = place_obj;
  {
    GC_CAN_IGNORE void *handle;
    handle = scheme_get_signal_handle();
    place_obj->parent_signal_handle = handle;
  }

  mzrt_sema_create(&ready, 0);

  /* pass critical info to new place */
  place_data = MALLOC_ONE(Place_Start_Data);
  place_data->ready    = ready;
  place_data->place_obj = place_obj;

  {
    Scheme_Object *so;

    if (!scheme_is_module_path(args[0]) && !SCHEME_PATHP(args[0])) {
      scheme_wrong_type("dynamic-place", "module-path or path", 0, argc, args);
    }
    if (!SCHEME_SYMBOLP(args[1])) {
      scheme_wrong_type("dynamic-place", "symbol", 1, argc, args);
    }

    so = places_deep_copy_to_master(args[0]);
    place_data->module   = so;
    so = places_deep_copy_to_master(args[1]);
    place_data->function = so;
    place_data->ready    = ready;
    
    /* create channel */
    {
      Scheme_Place_Bi_Channel *channel;
      channel = place_bi_channel_create();
      place->channel = (Scheme_Object *) channel;
      channel = place_bi_peer_channel_create(channel);
      place_data->channel = (Scheme_Object *) channel;
    }
  }

  collection_paths = scheme_current_library_collection_paths(0, NULL);
  collection_paths = places_deep_copy_to_master(collection_paths);
  place_data->current_library_collection_paths = collection_paths;

  /* create new place */
  proc_thread = mz_proc_thread_create(place_start_proc, place_data);

  if (!proc_thread) {
      mzrt_sema_destroy(ready);
    scheme_signal_error("place: place creation failed");
  }

  mz_proc_thread_detach(proc_thread);

  /* wait until the place has started and grabbed the value
     from `place_data'; it's important that a GC doesn't happen
     here until the other place is far enough. */
  mzrt_sema_wait(ready);
  mzrt_sema_destroy(ready);

  place_data->ready = NULL;
  place_data->place_obj = NULL;
  
  {
    Scheme_Custodian *cust;
    Scheme_Custodian_Reference *mref;
    cust = scheme_get_current_custodian();
    mref = scheme_add_managed(NULL,
                              (Scheme_Object *)place,
                              cust_kill_place,
                              NULL,
                              1);
    place->mref = mref;
  }

  return (Scheme_Object*) place;
}

static void do_place_kill(Scheme_Place *place) 
{
  Scheme_Place_Object *place_obj;
  place_obj = place->place_obj;

  if (!place_obj) return;

  {
    mzrt_mutex_lock(place_obj->lock);

    place_obj->die = 1;

    if (place_obj->signal_handle) { scheme_signal_received_at(place_obj->signal_handle); }

    place->result = place_obj->result;

    mzrt_mutex_unlock(place_obj->lock);
  }

  scheme_remove_managed(place->mref, (Scheme_Object *)place);
  place->place_obj = NULL;
}

static int do_place_break(Scheme_Place *place) {
  Scheme_Place_Object *place_obj;
  place_obj = (Scheme_Place_Object*) place->place_obj;

  {
    mzrt_mutex_lock(place_obj->lock);

    place_obj->pbreak = 1;

    scheme_signal_received_at(place_obj->signal_handle);

    mzrt_mutex_unlock(place_obj->lock);
  }

  return 0;
}

static void cust_kill_place(Scheme_Object *pl, void *notused) {
  do_place_kill((Scheme_Place *)pl);
}

static Scheme_Object *place_kill(int argc, Scheme_Object *args[]) {
  Scheme_Place          *place;
  place = (Scheme_Place *) args[0];

  if (!SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type))
    scheme_wrong_type("place-kill", "place", 0, argc, args);

  do_place_kill(place);
  return scheme_void;
}

static Scheme_Object *place_break(int argc, Scheme_Object *args[]) {
  Scheme_Place          *place;
  place = (Scheme_Place *) args[0];

  if (!SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type))
    scheme_wrong_type("place-break", "place", 0, argc, args);

  return scheme_make_integer(do_place_break(place));
}

static int place_deadp(Scheme_Object *place) {
  Scheme_Place_Object *place_obj;
  int dead = 0;
  place_obj = (Scheme_Place_Object*) ((Scheme_Place *)place)->place_obj;

  if (place_obj == NULL) {
    return 1;
  }
  else
  {
    mzrt_mutex_lock(place_obj->lock);

    dead = place_obj->die;

    mzrt_mutex_unlock(place_obj->lock);
  }
  if (dead) { return 1; }
  return 9;
}

static Scheme_Object *make_place_dead(int argc, Scheme_Object *argv[])
{ 
  Scheme_Object *b;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_place_type))
    scheme_wrong_type("thread-dead-evt", "place", 0, argc, argv);

  b = scheme_alloc_small_object();
  b->type = scheme_place_dead_type;
  SCHEME_PTR_VAL(b) = argv[0];
  return b;
} 
  
static int place_dead_ready(Scheme_Object *o, Scheme_Schedule_Info *sinfo) {
  if (place_deadp(SCHEME_PTR_VAL(o))) {
    scheme_set_sync_target(sinfo, o, NULL, NULL, 0, 0, NULL);
    return 1;
  }
  return 0;
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
  struct Child_Status *next_unused; /* see unused_pid_statuses */
} Child_Status;

SHARED_OK static Child_Status *child_statuses = NULL;
SHARED_OK static mzrt_mutex* child_status_lock = NULL;
SHARED_OK static mzrt_mutex* child_wait_lock = NULL; /* ordered before status lock */

/* When the Racket process value for a process in a different group becomes 
   GC-unreachable before a waitpid() on the process, then we 
   need to keep waiting on the pid to let the OS gc the process.
   This list is especially needed for processes that we create in
   their own group, but it's also needed for processes that put
   themselves in their own group (which we conservatively assume
   can be any child process).
   This list is protect by the wait lock. */
SHARED_OK static Child_Status *unused_pid_statuses = NULL;

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
    st->next_unused = NULL;
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

  /* Check specific pid, in case the child has its own group
     (either given by Racket or given to itself): */
  {
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
    st->next_unused = NULL;

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
  Child_Status *unused_status, *prev_unused, *next;

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

    unused_status = unused_pid_statuses;
    prev_unused = NULL;

    do {
      if (unused_status) {
        /* See unused_pid_statuses above */
        check_pid = unused_status->pid;
        is_group = 1;
      } else {
        /* We wait only on processes in the same group as Racket,
           because detecting the termination of a group's main process
           disables our ability to terminate all processes in the group. */
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
            prev_unused = unused_status;
            unused_status = unused_status->next;
          } 
        }
      } else if (pid > 0) {
        /* printf("SIGCHILD pid %i with status %i %i\n", pid, status, WEXITSTATUS(status)); */
        if (is_group) {
          next = unused_status->next;
          if (prev_unused)
            prev_unused->next_unused = next;
          else
            unused_pid_statuses = next;
          free(unused_status);
          unused_status = next;
        } else
          add_child_status(pid, status);
      } else {
        if (is_group) {
          prev_unused = unused_status;
          unused_status = unused_status->next;
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
  int keep_unused = 1; /* assume that any process can be in a new group */

  mzrt_mutex_lock(child_wait_lock); /* protects unused_pid_statuses */
  mzrt_mutex_lock(child_status_lock);

  for (st = child_statuses; st; st = st->next) {
    if (st->pid == pid) {
      if (!st->done) {
        if (keep_unused) {
          st->next_unused = unused_pid_statuses;
          unused_pid_statuses = st;
          if (st->signal_fd)
            remove_group_signal_fd(st->signal_fd);
        } else
          st->unneeded = 1;
        st->signal_fd = NULL;
      }
      break;
    }
  }
      
  if (st && (keep_unused || st->done)) {
    /* remove it from normal list: */
    raw_get_child_status(pid, NULL, 0, 1, st->done);
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

void scheme_places_unblock_child_signal() XFORM_SKIP_PROC
{
  sigset_t set;

  MZ_SIGSET(SIGCHLD, SIG_DFL);

  sigemptyset(&set);
  sigaddset(&set, SIGCHLD);
  sigprocmask(SIG_UNBLOCK, &set, NULL);
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

/* ---------------------------------------------------------------------- */

static int place_wait_ready(Scheme_Object *_p) {
  Scheme_Place *p = (Scheme_Place *)_p;
  int done;

  if (!p->place_obj) return 1;

  mzrt_mutex_lock(p->place_obj->lock);
  done = !p->place_obj->parent_signal_handle;
  mzrt_mutex_unlock(p->place_obj->lock);

  if (done) {
    do_place_kill(p); /* sets result, frees place */
    return 1;
  }

  return 0;
}

static Scheme_Object *place_wait(int argc, Scheme_Object *args[]) {
  Scheme_Place          *place;
  place = (Scheme_Place *) args[0];

  if (!SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type))
    scheme_wrong_type("place-wait", "place", 0, argc, args);
  
  scheme_block_until(place_wait_ready, NULL, (Scheme_Object*)place, 0);

  return scheme_make_integer(place->result);
}

static Scheme_Object *place_p(int argc, Scheme_Object *args[])
{
  return SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type) ? scheme_true : scheme_false;
}

static Scheme_Object *do_places_deep_copy(Scheme_Object *so, int gcable) {
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  Scheme_Hash_Table *ht = NULL;
  return places_deep_copy_worker(so, &ht, 1, gcable, gcable);
#else
  return so;
#endif
}

Scheme_Object *scheme_places_deep_copy(Scheme_Object *so) {
  return do_places_deep_copy(so, 1);
}

static void bad_place_message(Scheme_Object *so) {
  scheme_arg_mismatch("place-channel-put", 
                      "value not allowed in a message: ", 
                      so);
}

static Scheme_Object *trivial_copy(Scheme_Object *so)
{
  switch (SCHEME_TYPE(so)) {
    case scheme_integer_type:
    case scheme_true_type:
    case scheme_false_type:
    case scheme_null_type:
    case scheme_void_type:
    case scheme_place_bi_channel_type: /* allocated in the master and can be passed along as is */
      return so;
    case scheme_place_type:
      return ((Scheme_Place *) so)->channel;
      break;
    case scheme_byte_string_type:
    case scheme_flvector_type:
    case scheme_fxvector_type:
      if (SHARED_ALLOCATEDP(so))
        return so;
  }

  return NULL;
}

static Scheme_Object *shallow_types_copy(Scheme_Object *so, Scheme_Hash_Table *ht, int copy, int can_raise_exn) {
  Scheme_Object *new_so;

  new_so = trivial_copy(so);
  if (new_so) return new_so;

  new_so = so;

  switch (SCHEME_TYPE(so)) {
    case scheme_char_type:
      if (copy)
        new_so = scheme_make_char(SCHEME_CHAR_VAL(so));
      break;
    case scheme_bignum_type:
      if (copy)
        new_so = scheme_bignum_copy(so);
      break;
    case scheme_rational_type:
      {
        Scheme_Object *n;
        Scheme_Object *d;
        n = scheme_rational_numerator(so);
        d = scheme_rational_denominator(so);
        n = shallow_types_copy(n, NULL, copy, can_raise_exn);
        d = shallow_types_copy(d, NULL, copy, can_raise_exn);
        if (copy)
          new_so = scheme_make_rational(n, d);
      }
      break;
    case scheme_float_type:
      if (copy)
        new_so = scheme_make_float(SCHEME_FLT_VAL(so));
      break;
    case scheme_double_type:
      if (copy)
        new_so = scheme_make_double(SCHEME_DBL_VAL(so));
      break;
    case scheme_complex_type:
      {
        Scheme_Object *r;
        Scheme_Object *i;
        r = scheme_complex_real_part(so);
        i = scheme_complex_imaginary_part(so);
        r = shallow_types_copy(r, NULL, copy, can_raise_exn);
        i = shallow_types_copy(i, NULL, copy, can_raise_exn);
        if (copy)
          new_so = scheme_make_complex(r, i);
      }
      break;
    case scheme_char_string_type:
      if (copy) {
        new_so = scheme_make_sized_offset_char_string(SCHEME_CHAR_STR_VAL(so), 0, SCHEME_CHAR_STRLEN_VAL(so), 1);
        SCHEME_SET_IMMUTABLE(new_so);
      }
      break;
    case scheme_byte_string_type:
      /* not allocated as shared, since that's covered above */
      if (copy) {
        new_so = scheme_make_sized_offset_byte_string(SCHEME_BYTE_STR_VAL(so), 0, SCHEME_BYTE_STRLEN_VAL(so), 1);
        SCHEME_SET_IMMUTABLE(new_so);
      }
      break;
    case scheme_unix_path_type:
    case scheme_windows_path_type:
      if (copy)
        new_so = scheme_make_sized_offset_kind_path(SCHEME_BYTE_STR_VAL(so), 0, SCHEME_BYTE_STRLEN_VAL(so), 1,
                                                    SCHEME_TYPE(so));
      break;
    case scheme_symbol_type:
      if (SCHEME_SYM_UNINTERNEDP(so)) {
        if (can_raise_exn)
          bad_place_message(so);
        else
          return NULL;
      } else {
        if (copy) {
          new_so = scheme_make_sized_offset_byte_string((char *)so, SCHEME_SYMSTR_OFFSET(so), SCHEME_SYM_LEN(so), 1);
          new_so->type = scheme_serialized_symbol_type;
        }
      }
      break;
    case scheme_serialized_symbol_type:
      if (copy)
        new_so = scheme_intern_exact_symbol(SCHEME_BYTE_STR_VAL(so), SCHEME_BYTE_STRLEN_VAL(so));
      break;
    case scheme_fxvector_type:
      /* not allocated as shared, since that's covered above */
      if (copy) {
        Scheme_Vector *vec;
        intptr_t i;
        intptr_t size = SCHEME_FXVEC_SIZE(so);
        vec = scheme_alloc_fxvector(size);

        for (i = 0; i < size; i++) {
          SCHEME_FXVEC_ELS(vec)[i] = SCHEME_FXVEC_ELS(so)[i];
        }
        new_so = (Scheme_Object *) vec;
      }
      break;
    case scheme_flvector_type:
      /* not allocated as shared, since that's covered above */
      if (copy) {
        Scheme_Double_Vector *vec;
        intptr_t i;
        intptr_t size = SCHEME_FLVEC_SIZE(so);
        vec = scheme_alloc_flvector(size);

        for (i = 0; i < size; i++) {
          SCHEME_FLVEC_ELS(vec)[i] = SCHEME_FLVEC_ELS(so)[i];
        }
        new_so = (Scheme_Object *) vec;
      }
      break;
    default:
      new_so = NULL;
      break;
  }
  if (ht && new_so) {
    scheme_hash_set(ht, so, new_so);
  }
  return new_so;
}

/* InFinite Stack */
#define IFS_SIZE 512
#define IFS_CACHE_SLOT (IFS_SIZE - 1)
#define IFS_SEGMENT_BOTTOM 1
#define IFS_PREV_SEG_SLOT  0
static Scheme_Object* create_infinite_stack(int gcable) { 
  Scheme_Object **v;

  if (gcable) {
    /* If a GC is not possible, then we prefer to malloc() the stack
       space so that the space doesn't show up as part of the
       message allocation. */
    v = GC_malloc(IFS_SIZE * sizeof(Scheme_Object*));
  } else {
    v = malloc(IFS_SIZE * sizeof(Scheme_Object*));
    v[IFS_PREV_SEG_SLOT] = NULL;
    v[IFS_CACHE_SLOT] = NULL;
  }

  return (Scheme_Object *) v;
}
static void  free_infinite_stack(Scheme_Object** st, int gcable) {
  Scheme_Object **prev;
  if (st[IFS_CACHE_SLOT]) {
    if (!gcable) free(st[IFS_CACHE_SLOT]);
    st[IFS_CACHE_SLOT] = NULL;
  }
  prev = (Scheme_Object **) st[IFS_PREV_SEG_SLOT];
  if (prev) {
    prev[IFS_CACHE_SLOT] = NULL;
  }
  if (!gcable) free(st);
}

static MZ_INLINE void inf_push(Scheme_Object **instack, Scheme_Object *item, uintptr_t *indepth, int gcable) {
  Scheme_Object **stack = (Scheme_Object **) *instack;
  if (*indepth == IFS_CACHE_SLOT) {
    if (stack[IFS_CACHE_SLOT]) { /* cached */
      stack = (Scheme_Object **) stack[IFS_CACHE_SLOT];
    }
    else { /* no cache */ 
      Scheme_Object *tmp;
      tmp = create_infinite_stack(gcable);
      stack[IFS_CACHE_SLOT] = tmp;
      stack = (Scheme_Object **)stack[IFS_CACHE_SLOT];
      stack[IFS_PREV_SEG_SLOT] = (Scheme_Object *) (*instack);
    }
    *instack = (Scheme_Object *) stack;
    *indepth = IFS_SEGMENT_BOTTOM;
  }

  /* printf("PUSH %p %li %p\n", stack, *indepth, item); */
  stack[((*indepth)++)] = item;
  return;
}

static MZ_INLINE Scheme_Object *inf_pop(Scheme_Object **instack, uintptr_t *indepth, int gcable) {
  Scheme_Object **stack = (Scheme_Object **) *instack;
  Scheme_Object *val;
  if (*indepth == IFS_SEGMENT_BOTTOM) {
    Scheme_Object *item;
    if (stack[IFS_CACHE_SLOT]) { /* already have cached segment, free it*/
      free_infinite_stack((Scheme_Object **) stack[IFS_CACHE_SLOT], gcable);
      stack[IFS_CACHE_SLOT] = NULL;
    }
    if (stack[IFS_PREV_SEG_SLOT]) {
      stack = (Scheme_Object **) stack[IFS_PREV_SEG_SLOT];
      stack[IFS_CACHE_SLOT] = (Scheme_Object *)(*instack);
      *instack = (Scheme_Object*) stack;
      *indepth = IFS_CACHE_SLOT;
    }
    else {
      printf("pop beyond start of inf stack\n");
      abort();
      return NULL;
    }
  }

  val = stack[--(*indepth)];
  /* printf("Pop %p %li %p\n", stack, *indepth, val); */
  stack[*indepth] = NULL;
  return val;
}

static MZ_INLINE Scheme_Object *inf_set(Scheme_Object **instack, int pos, Scheme_Object *item, uintptr_t *indepth) {
  Scheme_Object **stack = (Scheme_Object **) *instack;
  Scheme_Object *old;
  int realpos;
  if (*indepth <= pos + 1) {
    if (stack[IFS_PREV_SEG_SLOT]) {
      stack = (Scheme_Object **) stack[IFS_PREV_SEG_SLOT];
      realpos = (IFS_CACHE_SLOT - (pos + 2)) + *indepth;
    }
    else {
      printf("set beyond start of inf stack\n");
      abort();
      return NULL;
    }
  }
  else { realpos = *indepth - 1 - pos; }

  /* printf("Set %p %i %li %i %p\n", stack, pos, *indepth, realpos, item); */
  old = stack[realpos];
  stack[realpos] = item;
  return old;
}

static MZ_INLINE Scheme_Object *inf_get(Scheme_Object **instack, int pos, uintptr_t *indepth) {
  Scheme_Object **stack = (Scheme_Object **) *instack;
  Scheme_Object *item;
  int realpos;
  if (*indepth <= pos + 1) {
    if (stack[IFS_PREV_SEG_SLOT]) {
      stack = (Scheme_Object **) stack[IFS_PREV_SEG_SLOT];
      realpos = (IFS_CACHE_SLOT - (pos + 2)) + *indepth;
    }
    else {
      printf("get beyond start of inf stack\n");
      abort();
      return NULL;
    }
  }
  else { realpos = *indepth - 1 - pos; }

  item = stack[realpos];

  /* printf("Get %p %i %li %i %p\n", stack, pos, *indepth, realpos, item); */
  return item;
}

/* VERY SPECIAL C CODE */

/* This code often executes with the master GC switched on */
/* It cannot use the usual stack overflow mechanism */
/* Therefore it must use its own stack implementation for recursion */
static Scheme_Object *places_deep_copy_worker(Scheme_Object *so, Scheme_Hash_Table **ht, 
                                              int copy, int gcable, int can_raise_exn) {
  Scheme_Object *inf_stack = NULL;
  Scheme_Object *reg0 = NULL;
  uintptr_t inf_stack_depth = 0;
  
  /* lifted variables for xform*/
  Scheme_Object *pair;
  Scheme_Object *vec;
  intptr_t i;
  intptr_t size;
  Scheme_Structure *st;
  Scheme_Serialized_Structure *sst;
  Scheme_Struct_Type *stype;
  Scheme_Struct_Type *ptype;
  int local_slots;

#define DEEP_DO_CDR 1
#define DEEP_DO_FIN_PAIR 2
#define DEEP_VEC1 3
#define DEEP_ST1 4   
#define DEEP_ST2 5      
#define DEEP_SST1 6
#define DEEP_SST2 7      
#define DEEP_RETURN 8
#define DEEP_DONE 9 
#define RETURN do { goto DEEP_RETURN_L; } while(0);
#define ABORT do { goto DEEP_DONE_L; } while(0);
#define IFS_PUSH(x) inf_push(&inf_stack, x, &inf_stack_depth, gcable)
#define IFS_POP inf_pop(&inf_stack, &inf_stack_depth, gcable)
#define IFS_POPN(n) do { int N = (n); while (N > 0) { IFS_POP; N--;} } while(0);
#define IFS_GET(n) inf_get(&inf_stack, (n), &inf_stack_depth)
#define IFS_SET(n, x) inf_set(&inf_stack, (n), x, &inf_stack_depth)
#define GOTO_NEXT_CONT(dest, cont) do { IFS_PUSH(scheme_make_integer(cont)); goto DEEP_DO; } while(0);
#define SET_R0(x) reg0 = x
#define GET_R0() (reg0)

  Scheme_Object *new_so = so;
  int ctr = 0;

  /* First, check for simple values that don't need to be hashed: */
  new_so = shallow_types_copy(so, *ht, copy, can_raise_exn);
  if (new_so) return new_so;

  if (*ht) {
    Scheme_Object *r; 
    if ((r = scheme_hash_get(*ht, so))) {
      return r;
    }
  }

  if (!*ht) {
    Scheme_Hash_Table *_ht;
    _ht = scheme_make_hash_table(SCHEME_hash_ptr);
    *ht = _ht;
  }

  inf_stack = create_infinite_stack(gcable);
  inf_stack_depth = 1;

  IFS_PUSH(scheme_make_integer(DEEP_DONE));
  SET_R0(so);

DEEP_DO:
  ctr++;
    
  so = GET_R0();
  new_so = trivial_copy(so);
  if (new_so) RETURN;

  if (*ht) {
    if ((new_so = scheme_hash_get(*ht, so))) {
      SET_R0(new_so);
      RETURN;
    }
  }

  new_so = shallow_types_copy(so, *ht, copy, can_raise_exn);
  if (new_so) RETURN;
  new_so = so;

  switch (SCHEME_TYPE(so)) {
    case scheme_pair_type:
      /* handle cycles: */
      if (copy) {
        pair = scheme_make_pair(scheme_false, scheme_false);
        SCHEME_PAIR_COPY_FLAGS(pair, so);
      } else
        pair = so;
      scheme_hash_set(*ht, so, pair);

      IFS_PUSH(so); 
      IFS_PUSH(pair); 
      SET_R0(SCHEME_CAR(so));
      GOTO_NEXT_CONT(DEEP_DO, DEEP_DO_CDR);

DEEP_DO_CDR_L:
      pair = IFS_GET(0);
      so   = IFS_GET(1);
      if (copy) {
        SCHEME_CAR(pair) = GET_R0();
      }
      SET_R0(SCHEME_CDR(so));
      GOTO_NEXT_CONT(DEEP_DO, DEEP_DO_FIN_PAIR);

DEEP_DO_FIN_PAIR_L:
      pair = IFS_POP; 
      so   = IFS_POP; 
      if (copy) {
        SCHEME_CDR(pair) = GET_R0();
        new_so = pair;
      }
      RETURN;
      break;
    case scheme_vector_type:
      size = SCHEME_VEC_SIZE(so);

      if (copy)
        vec = scheme_make_vector(size, 0);
      else
        vec = so;

      /* handle cycles: */
      scheme_hash_set(*ht, so, vec);
      i = 0;
      
      IFS_PUSH(vec);
      IFS_PUSH(so);
      IFS_PUSH(scheme_make_integer(size));
      IFS_PUSH(scheme_make_integer(i));
      
      if (i < size) {
        SET_R0(SCHEME_VEC_ELS(so)[i]);
        GOTO_NEXT_CONT(DEEP_DO, DEEP_VEC1);
      }
      else {
        goto DEEP_VEC2;
      }

DEEP_VEC1_L:
      /* vector loop*/
      i    = SCHEME_INT_VAL(IFS_GET(0));
      size = SCHEME_INT_VAL(IFS_GET(1));
      so   = IFS_GET(2);
      vec  = IFS_GET(3);
      if (copy) {
        SCHEME_VEC_ELS(vec)[i] = GET_R0();
      }
      i++;
      if (i < size) {
        IFS_SET(0, scheme_make_integer(i));
        SET_R0(SCHEME_VEC_ELS(so)[i]);
        GOTO_NEXT_CONT(DEEP_DO, DEEP_VEC1);
      }
      else {
        goto DEEP_VEC2;
      }

DEEP_VEC2:
      i    = SCHEME_INT_VAL(IFS_POP);
      size = SCHEME_INT_VAL(IFS_POP);
      so   = IFS_POP;
      vec  = IFS_POP;

      if (copy) {
        SCHEME_SET_IMMUTABLE(vec);
        new_so = vec;
      }
      RETURN;
      break;
    case scheme_structure_type:
      st = (Scheme_Structure*)so;
      stype = st->stype;
      ptype = stype->parent_types[stype->name_pos - 1];
      size = stype->num_slots;
      local_slots = stype->num_slots - (ptype ? ptype->num_slots : 0);

      if (!stype->prefab_key) {
        if (can_raise_exn)
          bad_place_message(so);
        else {
          new_so = NULL;
          ABORT;
        }
      }
      for (i = 0; i < local_slots; i++) {
        if (!stype->immutables || stype->immutables[i] != 1) {
          if (can_raise_exn)
            bad_place_message(so);
          else {
            new_so = NULL;
            ABORT;
          }
        }
      }

      IFS_PUSH((Scheme_Object *)st);
      SET_R0(SCHEME_CDR(stype->prefab_key));
      GOTO_NEXT_CONT(DEEP_DO, DEEP_ST1);

DEEP_ST1_L:
      st = (Scheme_Structure*) IFS_GET(0);
      so = (Scheme_Object *) st;
      size = st->stype->num_slots;
      if (copy) {
        new_so = scheme_make_serialized_struct_instance(GET_R0(), size);
        sst = (Scheme_Serialized_Structure*)new_so;
      } else
        sst = NULL;

      /* handle cycles: */
      scheme_hash_set(*ht, so, new_so);

      i = 0;
      if (i < size) {
        IFS_PUSH(scheme_make_integer(size));
        IFS_PUSH(scheme_make_integer(i));
        IFS_PUSH((Scheme_Object *)sst);
        SET_R0( st->slots[i]);
        GOTO_NEXT_CONT(DEEP_DO, DEEP_ST2);
      }
      else {
        if (copy)
          new_so = IFS_GET(0);
        IFS_POP;
        RETURN;
      }

DEEP_ST2_L:
      i    = SCHEME_INT_VAL(IFS_GET(1)); 
      size = SCHEME_INT_VAL(IFS_GET(2));
      st = (Scheme_Structure*) IFS_GET(3);
      so = (Scheme_Object *) st;
      if (copy) {
        sst = (Scheme_Serialized_Structure *) IFS_GET(0); 
        sst->slots[i] = GET_R0();
      }
      i++;
      if (i < size) {
        IFS_SET(1, scheme_make_integer(i));
        SET_R0(st->slots[i]);
        GOTO_NEXT_CONT(DEEP_DO, DEEP_ST2);
      }
      else {
        if (copy)
          new_so = IFS_GET(0);
        IFS_POPN(4);
        RETURN;
      }
    break;
    case scheme_serialized_structure_type:
      sst = (Scheme_Serialized_Structure*)so;
      
      IFS_PUSH((Scheme_Object *)sst);
      SET_R0(sst->prefab_key);
      GOTO_NEXT_CONT(DEEP_DO, DEEP_SST1);

DEEP_SST1_L:
      sst = (Scheme_Serialized_Structure*) IFS_GET(0);
      so = (Scheme_Object *) sst;
      size = sst->num_slots;
      if (copy) {
        stype = scheme_lookup_prefab_type(GET_R0(), size);
        new_so = scheme_make_blank_prefab_struct_instance(stype);

        st = (Scheme_Structure*)new_so;
      } else
        st = NULL;

      /* handle cycles: */
      scheme_hash_set(*ht, so, new_so);

      i = 0;
      if (i < size) {
        IFS_PUSH(scheme_make_integer(size));
        IFS_PUSH(scheme_make_integer(i));
        IFS_PUSH((Scheme_Object *)st);
        SET_R0(sst->slots[i]);
        GOTO_NEXT_CONT(DEEP_DO, DEEP_SST2);
      }
      else {
        if (copy)
          new_so = IFS_GET(0);
        IFS_POP;
        RETURN;
      }

DEEP_SST2_L:
      i    = SCHEME_INT_VAL(IFS_GET(1));
      size = SCHEME_INT_VAL(IFS_GET(2));
      sst = (Scheme_Serialized_Structure*) IFS_GET(3);
      so = (Scheme_Object *) sst;
      if (copy) {
        st = (Scheme_Structure *) IFS_GET(0); 
        st->slots[i] = GET_R0();
      }
      i++;
      if (i < size) {
        IFS_SET(1, scheme_make_integer(i));
        SET_R0(sst->slots[i]);
        GOTO_NEXT_CONT(DEEP_DO, DEEP_SST2);
      }
      else {
        if (copy)
          new_so = IFS_GET(0);
        IFS_POPN(4);
        RETURN;
      }
      break;
    default:
      if (can_raise_exn)
        bad_place_message(so);
      else {
        new_so = NULL;
        ABORT;
      }
      break;
  }

DEEP_RETURN_L:
  {
    ctr--;
    SET_R0(new_so);
    switch(SCHEME_INT_VAL(IFS_POP)) {
      case DEEP_DO_CDR:      goto DEEP_DO_CDR_L;
      case DEEP_DO_FIN_PAIR: goto DEEP_DO_FIN_PAIR_L;
      case DEEP_VEC1:        goto DEEP_VEC1_L;
      case DEEP_ST1:         goto DEEP_ST1_L;
      case DEEP_ST2:         goto DEEP_ST2_L;
      case DEEP_SST1:        goto DEEP_SST1_L;
      case DEEP_SST2:        goto DEEP_SST2_L;
      case DEEP_RETURN:      goto DEEP_RETURN_L;
      case DEEP_DONE:        goto DEEP_DONE_L;
      default:
        printf("Invalid places_deep_copy_worker state\n");
        abort();
    }
  }

DEEP_DONE_L:
  free_infinite_stack((Scheme_Object **) inf_stack, gcable);
  return new_so;

#undef DEEP_DO_CDR
#undef DEEP_DO_FIN_PAIR
#undef DEEP_VEC1
#undef DEEP_ST1
#undef DEEP_ST2
#undef DEEP_RETURN
#undef DEEP_DONE
#undef RETURNS
#undef IFS_PUSH
#undef IFS_POP
#undef IFS_POPN
#undef IFS_GET
#undef IFS_SET
#undef GOTO_NEXT_CONT
#undef GOTO_NEXT

}

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

void scheme_place_check_for_interruption() 
{
  Scheme_Place_Object *place_obj;
  char local_die;
  char local_break;

  place_obj = place_object;
  if (!place_obj)
    return;

  mzrt_mutex_lock(place_obj->lock);
  
  local_die = place_obj->die;
  local_break = place_obj->pbreak;
  place_obj->pbreak = 0;
  
  mzrt_mutex_unlock(place_obj->lock);
  
  if (local_die)
    scheme_kill_thread(scheme_main_thread);
  if (local_break)
    scheme_break_thread(NULL);
}

static void place_set_result(Scheme_Object *result)
/* always called as a place terminates */
{
  intptr_t status;

  if (SCHEME_INTP(result)) {
    status = SCHEME_INT_VAL(result);
    if (status < 1 || status > 255)
      status = 0;
  } else
    status = 0;

  mzrt_mutex_lock(place_object->lock);
  place_object->result = status;
  scheme_signal_received_at(place_object->parent_signal_handle);
  place_object->parent_signal_handle = NULL;
  place_object->signal_handle = NULL;
  mzrt_mutex_unlock(place_object->lock);
}

static Scheme_Object *def_place_exit_handler_proc(int argc, Scheme_Object *argv[])
{
  scheme_log(NULL, SCHEME_LOG_DEBUG, 0, "place %d: exiting via (exit)", scheme_current_place_id);

  place_set_result(argv[0]);

  /*printf("Leavin place: proc thread id%u\n", ptid);*/
  scheme_place_instance_destroy(0);

  mz_proc_thread_exit(NULL);

  return scheme_void; /* Never get here */
}
  
static void *place_start_proc_after_stack(void *data_arg, void *stack_base) {
  Place_Start_Data *place_data;
  Scheme_Place_Object *place_obj;
  Scheme_Object *place_main;
  Scheme_Object *a[2], *channel;
  mzrt_thread_id ptid;
  ptid = mz_proc_thread_self();
  
  place_data = (Place_Start_Data *) data_arg;
  data_arg = NULL;
 
  /* printf("Startin place: proc thread id%u\n", ptid); */

  /* create pristine THREAD_LOCAL variables*/
  null_out_runtime_globals();

  mzrt_mutex_lock(id_counter_mutex);
  scheme_current_place_id = ++id_counter;
  mzrt_mutex_unlock(id_counter_mutex);

  /* scheme_make_thread behaves differently if the above global vars are not null */
  scheme_place_instance_init(stack_base);

  a[0] = scheme_places_deep_copy(place_data->current_library_collection_paths);
  scheme_current_library_collection_paths(1, a);
  scheme_seal_parameters();

  a[0] = scheme_places_deep_copy(place_data->module);
  a[1] = scheme_places_deep_copy(place_data->function);
  a[1] = scheme_intern_exact_symbol(SCHEME_SYM_VAL(a[1]), SCHEME_SYM_LEN(a[1]));
  if (!SAME_TYPE(SCHEME_TYPE(place_data->channel), scheme_place_bi_channel_type)) {
    channel = scheme_places_deep_copy(place_data->channel);
  }
  else {
    channel = place_data->channel;
  }
  place_obj = place_data->place_obj;
  place_object = place_obj;
  
  {
    void *signal_handle;
    signal_handle = scheme_get_signal_handle();
    place_obj->signal_handle = signal_handle;
  }

  mzrt_sema_post(place_data->ready);
  place_data = NULL;
# ifdef MZ_PRECISE_GC
  /* this prevents a master collection attempt from deadlocking with the 
     place_data->ready semaphore above */
  GC_allow_master_gc_check();
# endif


  /* at point, don't refer to place_data or its content
     anymore, because it's allocated in the other place */

  scheme_set_root_param(MZCONFIG_EXIT_HANDLER, scheme_def_place_exit_proc);

  
  scheme_log(NULL, SCHEME_LOG_DEBUG, 0, "place %d: started", scheme_current_place_id);

  {
    Scheme_Thread * volatile p;
    mz_jmp_buf * volatile saved_error_buf;
    mz_jmp_buf new_error_buf;
    Scheme_Object * volatile rc = scheme_false;
    
    p = scheme_get_current_thread();
    saved_error_buf = p->error_buf;
    p->error_buf = &new_error_buf;
    if (!scheme_setjmp(new_error_buf)) {
      Scheme_Object *dynamic_require;

      scheme_check_place_port_ok();

      dynamic_require = scheme_builtin_value("dynamic-require");
      place_main = scheme_apply(dynamic_require, 2, a);
      a[0] = channel;
      (void)scheme_apply(place_main, 1, a);
      rc = scheme_make_integer(0);
    } else {
      rc = scheme_make_integer(1);
    }
    p->error_buf = saved_error_buf;
    
    place_set_result(rc);
  }

  scheme_log(NULL, SCHEME_LOG_DEBUG, 0, "place %d: exiting", scheme_current_place_id);

  /*printf("Leavin place: proc thread id%u\n", ptid);*/
  scheme_place_instance_destroy(place_obj->die);

  return NULL;
}

Scheme_Object *places_deep_copy_to_master(Scheme_Object *so) {
  Scheme_Hash_Table *ht = NULL;
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  Scheme_Object *o;
  void *original_gc;

  /* forces hash codes: */
  (void)places_deep_copy_worker(so, &ht, 0, 1, 1);
  ht = NULL;

  original_gc = GC_switch_to_master_gc();
  scheme_start_atomic();

  o = places_deep_copy_worker(so, &ht, 1, 1, 0);

  scheme_end_atomic_no_swap();
  GC_switch_back_from_master(original_gc);
  return o;
#else
  return places_deep_copy_worker(so, &ht, 1, 1, 1);
#endif
}

#ifdef DO_STACK_CHECK
static void places_deserialize_worker(Scheme_Object **pso, Scheme_Hash_Table **ht);

static Scheme_Object *places_deserialize_worker_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *pso = (Scheme_Object *)p->ku.k.p1;
  Scheme_Hash_Table*ht = (Scheme_Hash_Table *)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  places_deserialize_worker(&pso, &ht);
  p = scheme_current_thread;
  p->ku.k.p1 = ht;

  return pso;
}
#endif


static void places_deserialize_worker(Scheme_Object **pso, Scheme_Hash_Table **ht)
{
  Scheme_Object *so;
  Scheme_Object *tmp;
  Scheme_Serialized_Structure *sst;
  Scheme_Structure *st;
  Scheme_Struct_Type *stype;
  intptr_t i;
  intptr_t size;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p;
      p = scheme_current_thread;
      p->ku.k.p1 = *pso;
      p->ku.k.p2 = *ht;
      tmp = scheme_handle_stack_overflow(places_deserialize_worker_k);
      *pso = tmp;
      p = scheme_current_thread;
      *ht = p->ku.k.p1;
      p->ku.k.p1 = NULL;
      return;
    }
  }
#endif
  SCHEME_USE_FUEL(1);

  if (*pso) 
    so = *pso;
  else
    return;

  switch (SCHEME_TYPE(so)) {
    case scheme_true_type:
    case scheme_false_type:
    case scheme_null_type:
    case scheme_void_type:
    case scheme_integer_type:
    case scheme_place_bi_channel_type: /* allocated in the master and can be passed along as is */
    case scheme_char_type:
    case scheme_rational_type:
    case scheme_float_type:
    case scheme_double_type:
    case scheme_complex_type:
    case scheme_char_string_type:
    case scheme_byte_string_type:
    case scheme_unix_path_type:
    case scheme_windows_path_type:
    case scheme_flvector_type:
    case scheme_fxvector_type:
      break;
    case scheme_symbol_type:
      break;
    case scheme_serialized_symbol_type:
      tmp = scheme_intern_exact_symbol(SCHEME_BYTE_STR_VAL(so), SCHEME_BYTE_STRLEN_VAL(so));
      *pso = tmp;
      break;
    case scheme_pair_type:
      if (*ht) {
        if ((st = (Scheme_Structure *) scheme_hash_get(*ht, so)))
          break;
        else 
          scheme_hash_set(*ht, so, so);
      }
      else {
        tmp = (Scheme_Object *) scheme_make_hash_table(SCHEME_hash_ptr);
        *ht = (Scheme_Hash_Table *) tmp; 
        scheme_hash_set(*ht, so, so);
      }
      tmp = SCHEME_CAR(so);
      places_deserialize_worker(&tmp, ht);
      SCHEME_CAR(so) = tmp;
      tmp = SCHEME_CDR(so);
      places_deserialize_worker(&tmp, ht);
      SCHEME_CDR(so) = tmp;
      break;
    case scheme_vector_type:
      if (*ht) {
        if ((st = (Scheme_Structure *) scheme_hash_get(*ht, so)))
          break;
        else 
          scheme_hash_set(*ht, so, so);
      }
      else {
        tmp = (Scheme_Object *) scheme_make_hash_table(SCHEME_hash_ptr);
        *ht = (Scheme_Hash_Table *) tmp; 
        scheme_hash_set(*ht, so, so);
      }
      size = SCHEME_VEC_SIZE(so);
      for (i = 0; i <size ; i++) {
        tmp = SCHEME_VEC_ELS(so)[i];
        places_deserialize_worker(&tmp, ht);
        SCHEME_VEC_ELS(so)[i] = tmp;
      }
      break;
    case scheme_structure_type:
      break;
    case scheme_serialized_structure_type:
      if (*ht) {
        if ((st = (Scheme_Structure *) scheme_hash_get(*ht, so))) {
          *pso = (Scheme_Object *) st;
          break;
        }
      }
      else {
        tmp = (Scheme_Object *) scheme_make_hash_table(SCHEME_hash_ptr);
        *ht = (Scheme_Hash_Table *) tmp; 
      }
        
      sst = (Scheme_Serialized_Structure*)so;
      size = sst->num_slots;
      tmp = sst->prefab_key;
      places_deserialize_worker(&tmp, ht);
      sst->prefab_key = tmp;
      stype = scheme_lookup_prefab_type(sst->prefab_key, size);
      st = (Scheme_Structure *) scheme_make_blank_prefab_struct_instance(stype);
      scheme_hash_set(*ht, so, (Scheme_Object *) st);

      for (i = 0; i <size ; i++) {
        tmp = sst->slots[i];
        places_deserialize_worker(&tmp, ht);
        st->slots[i] = tmp;
      }
      *pso = (Scheme_Object *) st;
      break;

    default:
      scheme_log_abort("cannot deserialize object");
      abort();
      break;
  }
}

Scheme_Object *scheme_places_serialize(Scheme_Object *so, void **msg_memory) {
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  Scheme_Object *new_so;
  Scheme_Object *tmp;

  new_so = trivial_copy(so);
  if (new_so) return new_so;

  GC_create_message_allocator();
  new_so = do_places_deep_copy(so, 0);
  tmp = GC_finish_message_allocator();
  (*msg_memory) = tmp;
  return new_so;
#else
  return so;
#endif
}

Scheme_Object *scheme_places_deserialize(Scheme_Object *so, void *msg_memory) {
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  Scheme_Object *new_so = so;

  new_so = trivial_copy(so);
  if (new_so) return new_so;

  /* small messages are deemed to be < 1k, this could be tuned in either direction */
  if (GC_message_allocator_size(msg_memory) < 1024) {
    new_so = do_places_deep_copy(so, 1);
    GC_dispose_short_message_allocator(msg_memory);
  }
  else {
    Scheme_Object *ht = NULL;
    GC_adopt_message_allocator(msg_memory);
#if !defined(SHARED_TABLES)
    new_so = so;
    places_deserialize_worker(&new_so, (Scheme_Hash_Table **) &ht);
#endif
  }
  return new_so;
#else
  return so;
#endif
}

Scheme_Object *place_send(int argc, Scheme_Object *args[]) 
{
  Scheme_Place_Bi_Channel *ch;
  if (SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type)) {
    ch = (Scheme_Place_Bi_Channel *) ((Scheme_Place *) args[0])->channel;
  }
  else if (SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_bi_channel_type)) {
    ch = (Scheme_Place_Bi_Channel *) args[0];
  }
  else {
    ch = NULL;
    scheme_wrong_type("place-channel-put", "place-channel", 0, argc, args);
  }
  place_async_send((Scheme_Place_Async_Channel *) ch->sendch, args[1]);
  return scheme_void;
}

Scheme_Object *place_receive(int argc, Scheme_Object *args[]) {
  Scheme_Place_Bi_Channel *ch;
  if (SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type)) {
    ch = (Scheme_Place_Bi_Channel *) ((Scheme_Place *) args[0])->channel;
  }
  else if (SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_bi_channel_type)) {
    ch = (Scheme_Place_Bi_Channel *) args[0];
  }
  else {
    ch = NULL;
    scheme_wrong_type("place-channel-get", "place-channel", 0, argc, args);
  }
  return place_async_receive((Scheme_Place_Async_Channel *) ch->recvch);
}

static Scheme_Object* place_allowed_p(int argc, Scheme_Object *args[])
{
  Scheme_Hash_Table *ht = NULL;
  
  if (places_deep_copy_worker(args[0], &ht, 0, 1, 0))
    return scheme_true;
  else
    return scheme_false;
}

# ifdef MZ_PRECISE_GC
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

static void async_channel_finalize(void *p, void* data) {
  Scheme_Place_Async_Channel *ch;
  int i;
  ch = (Scheme_Place_Async_Channel*)p;
  mzrt_mutex_destroy(ch->lock);
  for (i = 0; i < ch->size ; i++) {
    ch->msgs[i] = NULL;
#ifdef MZ_PRECISE_GC
    if (ch->msg_memory[i]) {
      GC_destroy_orphan_msg_memory(ch->msg_memory[i]);
    }
#endif
    ch->msg_memory[i] = NULL;
  }
  ch->in = 0;
  ch->out = 0;
  ch->count = 0;
}

Scheme_Place_Async_Channel *place_async_channel_create() {
  Scheme_Object **msgs;
  Scheme_Place_Async_Channel *ch;
  void **msg_memory;
#ifdef MZ_PRECISE_GC
  void *original_gc;
#endif

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

#ifdef MZ_PRECISE_GC
  original_gc = GC_switch_to_master_gc();
  GC_set_finalizer(ch, 1, 1, async_channel_finalize, NULL, NULL, NULL);
  GC_switch_back_from_master(original_gc);
#endif
  /* FIXME? Need finalizer for non-precise GC if places become supported 
     in that mode. */

  return ch;
}

Scheme_Place_Bi_Channel *place_bi_channel_create() {
  Scheme_Place_Async_Channel *tmp;
  Scheme_Place_Bi_Channel *ch;

  ch = GC_master_malloc_tagged(sizeof(Scheme_Place_Bi_Channel));
  ch->so.type = scheme_place_bi_channel_type;

  tmp = place_async_channel_create();
  ch->sendch = tmp;
  tmp = place_async_channel_create();
  ch->recvch = tmp;
  return ch;
}

Scheme_Place_Bi_Channel *place_bi_peer_channel_create(Scheme_Place_Bi_Channel *orig) {
  Scheme_Place_Bi_Channel *ch;

  ch = GC_master_malloc_tagged(sizeof(Scheme_Place_Bi_Channel));
  ch->so.type = scheme_place_bi_channel_type;

  ch->sendch = orig->recvch;
  ch->recvch = orig->sendch;
  return ch;
}

static Scheme_Object *place_channel(int argc, Scheme_Object *args[]) {
  Scheme_Place_Bi_Channel *ch;
  Scheme_Object *a[2];
  ch = place_bi_channel_create();
  a[0] = (Scheme_Object *) ch;
  a[1] = (Scheme_Object *) place_bi_peer_channel_create(ch);
  return scheme_values(2, a);
}

static Scheme_Object *place_channel_p(int argc, Scheme_Object *args[])
{
  return SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_bi_channel_type) ? scheme_true : scheme_false;
}

static Scheme_Object *GC_master_make_vector(int size) {
  Scheme_Object *v;
#ifdef MZ_PRECISE_GC
  void *original_gc;
  original_gc = GC_switch_to_master_gc();
#endif
  v = scheme_make_vector(size, NULL);
#ifdef MZ_PRECISE_GC
  GC_switch_back_from_master(original_gc);
#endif
  return v;
}

static void place_async_send(Scheme_Place_Async_Channel *ch, Scheme_Object *uo) {
  void *msg_memory = NULL;
  Scheme_Object *o;
  int cnt;

  o = scheme_places_serialize(uo, &msg_memory);
  if (!o) bad_place_message(uo);

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

  if (!cnt && ch->wakeup_signal) {
    /*wake up possibly sleeping single receiver */  
    if (SCHEME_PLACE_OBJECTP(ch->wakeup_signal))
      scheme_signal_received_at(((Scheme_Place_Object *) ch->wakeup_signal)->signal_handle);
    /*wake up possibly sleeping multiple receiver */  
    else if (SCHEME_VECTORP(ch->wakeup_signal)) {
      Scheme_Object *v = ch->wakeup_signal;
      int i;
      int size = SCHEME_VEC_SIZE(v);
      int alive = 0;
      for (i = 0; i < size; i++) {
        Scheme_Place_Object *o3;
        o3 = (Scheme_Place_Object *)SCHEME_VEC_ELS(v)[i];
        if (o3 && o3->signal_handle != NULL) {
          scheme_signal_received_at(o3->signal_handle);
          alive++;
        }
        else
          SCHEME_VEC_ELS(v)[i] = NULL;
      }
      /* shrink if more than half are unused */
      if (alive < (size / 2)) {
        if (alive == 1) {
          ch->wakeup_signal = NULL;
          for (i = 0; i < size; i++) {
            Scheme_Place_Object *o2 = (Scheme_Place_Object *)SCHEME_VEC_ELS(v)[i];
            if (o2 && o2->signal_handle != NULL) {
              ch->wakeup_signal = (Scheme_Object *)o2;
              break;
            }
          }
        }
        else {
          Scheme_Object *nv;
          int ncnt = 0;
          nv = GC_master_make_vector(size/2);
          for (i = 0; i < size; i++) {
            Scheme_Place_Object *o2 = (Scheme_Place_Object *)SCHEME_VEC_ELS(v)[i];
            if (o2 && o2->signal_handle != NULL) {
              SCHEME_VEC_ELS(nv)[ncnt] = (Scheme_Object *)o2;
              ncnt++;
            }
          }
          ch->wakeup_signal = nv;
        }
      }
    }
    else {
      printf("Oops not a valid ch->wakeup_signal\n");
      exit(1);
    }
  }
  mzrt_mutex_unlock(ch->lock);
}

static void register_place_object_with_channel(Scheme_Place_Async_Channel *ch, Scheme_Object *o) {
  if (ch->wakeup_signal == o) {
    return;
  }
  else if (!ch->wakeup_signal)
    ch->wakeup_signal = o;
  else if (SCHEME_PLACE_OBJECTP(ch->wakeup_signal) 
           && ( (Scheme_Place_Object *) ch->wakeup_signal)->signal_handle == NULL)
    ch->wakeup_signal = o;
  else if (SCHEME_VECTORP(ch->wakeup_signal)) {
    int i = 0;
    Scheme_Object *v = ch->wakeup_signal;
    int size = SCHEME_VEC_SIZE(v);
    /* look for unused slot in wakeup vector */
    for (i = 0; i < size; i++) {
      Scheme_Object *vo = SCHEME_VEC_ELS(v)[i];
      if (vo == o) { 
        return;
      }
      else if (!vo) {
        SCHEME_VEC_ELS(v)[i] = o;
        return;
      }
      else if (SCHEME_PLACE_OBJECTP(vo) &&
          ((Scheme_Place_Object *)vo)->signal_handle == NULL) {
        SCHEME_VEC_ELS(v)[i] = o; 
        return;
      }
    }
    /* fall through to here, need to grow wakeup vector */
    {
      Scheme_Object *nv;
      nv = GC_master_make_vector(size*2);
      for (i = 0; i < size; i++) {
        SCHEME_VEC_ELS(nv)[i] = SCHEME_VEC_ELS(v)[i];
      }
      SCHEME_VEC_ELS(nv)[size+1] = o;
      ch->wakeup_signal = nv;
    }
  }
  /* grow from single wakeup to multiple wakeups */
  else if (SCHEME_PLACE_OBJECTP(ch->wakeup_signal)) {
    Scheme_Object *v;
    v = GC_master_make_vector(2);
    SCHEME_VEC_ELS(v)[0] = ch->wakeup_signal;
    SCHEME_VEC_ELS(v)[1] = o;
    ch->wakeup_signal = v;
  }
  else {
    printf("Oops not a valid ch->wakeup_signal\n");
    exit(1);
  }
}

static Scheme_Object *scheme_place_async_try_receive(Scheme_Place_Async_Channel *ch) {
  Scheme_Object *msg = NULL;
  void *msg_memory = NULL;

  mzrt_mutex_lock(ch->lock);
  {
    register_place_object_with_channel(ch, (Scheme_Object *) place_object);
    if (ch->count > 0) { /* GET MSG */
      msg = ch->msgs[ch->out];
      msg_memory = ch->msg_memory[ch->out];

      ch->msgs[ch->out] = NULL;
      ch->msg_memory[ch->out] = NULL;

      --ch->count;
      ch->out = (++ch->out % ch->size);
    }
  }
  mzrt_mutex_unlock(ch->lock);
  
  if (msg) {
    return scheme_places_deserialize(msg, msg_memory);
  }
  return msg;
}

static int scheme_place_async_ch_ready(Scheme_Place_Async_Channel *ch) {
  int ready = 0;
  mzrt_mutex_lock(ch->lock);
  {
    register_place_object_with_channel(ch, (Scheme_Object *) place_object);
    if (ch->count > 0) ready = 1;
  }
  mzrt_mutex_unlock(ch->lock);
  return ready;
}

static int place_channel_ready(Scheme_Object *so, Scheme_Schedule_Info *sinfo) {
  Scheme_Place_Bi_Channel *ch;
  Scheme_Object *msg = NULL;
  if (SAME_TYPE(SCHEME_TYPE(so), scheme_place_type)) {
    ch = (Scheme_Place_Bi_Channel *) ((Scheme_Place *) so)->channel;
  }
  else {
    ch = (Scheme_Place_Bi_Channel *)so;
  }
  
  msg = scheme_place_async_try_receive((Scheme_Place_Async_Channel *) ch->recvch);
  if (msg != NULL) {
    scheme_set_sync_target(sinfo, msg, NULL, NULL, 0, 0, NULL);
    return 1;
  }
  return 0;
}

static Scheme_Object *place_async_receive(Scheme_Place_Async_Channel *ch) {
  Scheme_Object *msg = NULL;
  while(1) {
    msg = scheme_place_async_try_receive(ch);
    if(msg) break;
    else {
      /*
      mzrt_mutex_lock(ch->lock);
      register_place_object_with_channel(ch, (Scheme_Object *) place_object);
      mzrt_mutex_unlock(ch->lock);
      */
    
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

#include "mzmark_place.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_place_type, place_val);
  GC_REG_TRAV(scheme_place_object_type, place_object_val);
  GC_REG_TRAV(scheme_place_async_channel_type, place_async_channel_val);
  GC_REG_TRAV(scheme_place_bi_channel_type, place_bi_channel_val);
}

END_XFORM_SKIP;

#endif

/************************************************************************/
/************************************************************************/
/************************************************************************/

#endif
