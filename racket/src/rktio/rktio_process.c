#include "rktio.h"
#include "rktio_private.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#if defined(RKTIO_SYSTEM_UNIX)
# include <signal.h>
# include <sys/types.h>
# include <sys/wait.h>
# include <errno.h>
# include <unistd.h>
# ifdef USE_ULIMIT
#  include <ulimit.h>
# endif
#endif

#if defined(RKTIO_SYSTEM_UNIX) && defined(RKTIO_USE_PTHREADS)
#define CENTRALIZED_SIGCHILD
#endif

/*========================================================================*/
/* Process data structure                                                 */
/*========================================================================*/

struct rktio_process_t {
  void *handle;
  int pid;
  int is_group;
#ifdef CENTRALIZED_SIGCHILD
  short done;
  int status;
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  int got_time;
#endif
};

/*========================================================================*/
/* Status helper                                                          */
/*========================================================================*/

#if defined(RKTIO_SYSTEM_UNIX)
static int extract_child_status(int status)
{
  if (WIFEXITED(status))
    status = WEXITSTATUS(status);
  else if (WIFSIGNALED(status))
    status = WTERMSIG(status) + 128;
  else
    status = -1;

  return status;
}
#endif

/*========================================================================*/
/* SIGCHLD management for a multi-threaded environment                    */
/*========================================================================*/

/* If SIGCHLD is unblocked, it gets delivered to a random thread
   --- not necessarily on in the right place for the subprocess.
   To avoid that problem, we centralize SIGCHLD handling here, and
   then dispatch back out to specific places as they request 
   information. */

#ifdef CENTRALIZED_SIGCHILD

typedef struct Child_Status {
  int pid;
  int status;
  char done;
  char unneeded; /* not in a group; result not needed */
  char is_group;
  rktio_signal_handle_t *signal_fd;
  struct Child_Status *next;
  struct Child_Status *next_unused; /* see unused_pid_statuses */
} Child_Status;

static Child_Status *child_statuses = NULL;
static pthread_mutex_t child_status_lock;
static pthread_mutex_t child_wait_lock; /* ordered before status lock */
static int status_lock_initialized;

static int started_thread, pending_children;

/* When the process value for a process in a different group
   is released before a waitpid() on the process, then we 
   need to keep waiting on the pid to let the OS gc the process.
   This list is especially needed for processes that we create in
   their own group, but it's also needed for processes that put
   themselves in their own group (which we conservatively assume
   can be any child process).
   This list is protected by the wait lock. */
static Child_Status *unused_pid_statuses = NULL;

static void add_group_signal_fd(rktio_signal_handle_t *signal_fd);
static void remove_group_signal_fd(rktio_signal_handle_t *signal_fd);
static void do_group_signal_fds();
static int centralized_get_child_status(int pid, int is_group, int can_check_group, int *status);

static void add_child_status(int pid, int status)
{
  Child_Status *st;

  /* Search for existing record, which will have a signal_fd: */
  pthread_mutex_lock(&child_status_lock);
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

  pthread_mutex_unlock(&child_status_lock);
  
  if (st->signal_fd)
    rktio_signal_received_at(st->signal_fd);
  if (st->unneeded)
    (void)centralized_get_child_status(st->pid, 0, 0, NULL);
}

static int raw_get_child_status(int pid, int *status, int done_only, int do_remove, int do_free)
{
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

int centralized_get_child_status(int pid, int is_group, int can_check_group, int *status)
{
  int found = 0;

  /* Check specific pid, in case the child has its own group
     (either given by Racket or given to itself): */
  if (can_check_group) {
    pid_t pid2;
    int status;

    do {
      pid2 = waitpid((pid_t)pid, &status, WNOHANG);
    } while ((pid2 == -1) && (errno == EINTR));

    if (pid2 > 0)
      add_child_status(pid, extract_child_status(status));
  }

  pthread_mutex_lock(&child_status_lock);
  found = raw_get_child_status(pid, status, 1, 1, 1);
  pthread_mutex_unlock(&child_status_lock);
  /* printf("centralized_get_child_status found %i pid %i status %i\n", found,  pid, *status); */

  return found;
}

static int centralized_register_child(int pid, int is_group, rktio_signal_handle_t *signal_fd, int *status)
{
  int found = 0;

  pthread_mutex_lock(&child_status_lock);

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

  pthread_mutex_unlock(&child_status_lock);
  return found;
}

static void *thread_signal_worker(void *data)
{
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

    pthread_mutex_lock(&child_status_lock);
    do_group_signal_fds();
    pthread_mutex_unlock(&child_status_lock);
    
    pthread_mutex_lock(&child_wait_lock);

    unused_status = unused_pid_statuses;
    prev_unused = NULL;

    do {
      if (unused_status) {
        /* See unused_pid_statuses above */
        check_pid = unused_status->pid;
        is_group = 1;
      } else {
        /* We wait only on processes in the same group as the current process,
           because detecting the termination of a group's main process
           disables our ability to terminate all processes in the group. */
        if (pending_children)
          check_pid = 0; /* => processes in the same group as the current process */
        else
          check_pid = -1; /* don't check */
        is_group = 0;
      }

      if (check_pid == -1) {
        pid = -1;
        errno = ECHILD;
      } else
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
          next = unused_status->next_unused;
          if (prev_unused)
            prev_unused->next_unused = next;
          else
            unused_pid_statuses = next;
          free(unused_status);
          unused_status = next;
        } else {
          /* Double-check for pid in unused_pid_statuses, since
             it may have completed between the pid-specific waitpid and the
             non-group waitpid: */
          prev_unused = NULL;
          for (unused_status = unused_pid_statuses; unused_status; unused_status = unused_status->next_unused) {
            if (unused_status->pid == pid)
              break;
            prev_unused = unused_status;
          }
          if (!unused_status) {
            /* not in unused_pid_statuses: */
            add_child_status(pid, extract_child_status(status));
          } else {
            if (prev_unused)
              prev_unused->next_unused = unused_status->next_unused;
            else
              unused_pid_statuses = unused_status->next_unused;
            free(unused_status);
            unused_status = NULL;
          }
        }
      } else {
        if (is_group) {
          prev_unused = unused_status;
          unused_status = unused_status->next_unused;
        }
      }
    } while ((pid > 0) || is_group);

    pthread_mutex_unlock(&child_wait_lock);
  }

  return NULL;
}

void centralized_done_with_process_id(int pid, int is_group)
{
  Child_Status *st;
  int keep_unused = 1; /* assume that any process can be in a new group */

  pthread_mutex_lock(&child_wait_lock); /* protects unused_pid_statuses */
  pthread_mutex_lock(&child_status_lock);

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

  pthread_mutex_unlock(&child_status_lock);
  pthread_mutex_unlock(&child_wait_lock);
}

static void got_sigchld()
{ 
  /* handle doesn't need to to anything, since sigwait()
     in a thread does the work. */
}

static void block_sigchld()
{
  sigset_t set;
  sigemptyset(&set);
  sigaddset(&set, SIGCHLD);
  sigprocmask(SIG_BLOCK, &set, NULL);
}

void centralized_block_child_signal()
{
  /* SIGCHLD is always blocked, since it's managed via sigwait() */
}

void centralized_unblock_child_signal()
{
}

void centralized_start_child_signal_handler()
{
  if (!status_lock_initialized) {
    pthread_mutex_init(&child_status_lock, NULL);
    pthread_mutex_init(&child_wait_lock, NULL);
    status_lock_initialized = 1;
  }
}

void centralized_wait_suspend()
{
  pthread_mutex_lock(&child_wait_lock);
}

void centralized_wait_resume()
{
  pthread_mutex_unlock(&child_wait_lock);
}

void centralized_starting_child()
{
  pthread_mutex_lock(&child_wait_lock);

  if (!started_thread) {
    pthread_t signal_thread;

    /* Mac OS X seems to need a handler installed for SIGCHLD to be
       delivered, since the default is to drop the signal. Also, this
       handler serves as a back-up alert if some thread is created that
       does not block SIGCHLD.
       Solaris, meanwhile, seems to unmask SIGCHLD as a result of
       setting a handler, so do this before masking the signal. */
    signal(SIGCHLD, got_sigchld);
    
    /* Block SIGCLHD (again), because the worker thread will use sigwait(). */
    block_sigchld();
      
    (void)pthread_create(&signal_thread, NULL, thread_signal_worker, NULL);

    (void)pthread_detach(signal_thread);
    started_thread = 1;
  }

  pending_children++;

  pthread_mutex_unlock(&child_wait_lock);
}

void centralized_ended_child()
{
  pthread_mutex_lock(&child_wait_lock);
  --pending_children;
  pthread_mutex_unlock(&child_wait_lock);
}

void rktio_reap_processes(rktio_t *rktio)
{
  /* Not needed, since the worker thread is already reaping
     processes. */
}

/* ---------------------------------------------------------------------- */

/* When a place has a process-group that it may be waiting on, the we
   need to wake up the place whenever any SIGCHLD is received, since
   the SIGDCHLD may apply to one of those places.
   The list of signal_fds is protected by the status lock. */

typedef struct Group_Signal_FD {
  rktio_signal_handle_t *signal_fd;
  int refcount;
} Group_Signal_FD;

static Group_Signal_FD *signal_fds;
static int signal_fd_count;

static void add_group_signal_fd(rktio_signal_handle_t *signal_fd)
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
    a = malloc(sizeof(Group_Signal_FD) * signal_fd_count);
    memset(a, 0, sizeof(Group_Signal_FD) * signal_fd_count);
    memcpy(a, signal_fds, sizeof(Group_Signal_FD) * count);
    if (signal_fds) free(signal_fds);
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

static void remove_group_signal_fd(rktio_signal_handle_t *signal_fd)
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
      rktio_signal_received_at(signal_fds[i].signal_fd);
    }
  }
}

#endif


/*========================================================================*/
/* Unix signal handling (without pthreads)                                */
/*========================================================================*/

#if defined(RKTIO_SYSTEM_UNIX) && !defined(CENTRALIZED_SIGCHILD)

typedef struct System_Child {
  pid_t id;
  short done;
  int status;
  struct System_Child *next;
} System_Child;

static void block_child_signals(rktio_t*rktio, int block)
{
  sigset_t sigs;
  
  sigemptyset(&sigs);
  sigaddset(&sigs, SIGCHLD);
  sigprocmask(block ? SIG_BLOCK : SIG_UNBLOCK, &sigs, NULL);
}

/* See `unused_pid_statuses' for
   a reminder of why this is needed (in both 
   implementations): */
static void *unused_pids;

/* We get only on signal handler for all rktio instances,
   so we have to chain them: */
static rktio_t *all_rktios;
static int sigchld_installed = 0;

static void child_done(int ingored)
{
  rktio_t *rktio = all_rktios;

  while (rktio) {
    rktio->need_to_check_children = 1;
    rktio_signal_received(rktio);
    rktio = rktio->next;
  }

# ifdef SIGNAL_NEEDS_REINSTALL
  signal(SIGCHLD, child_done);
# endif
}

static void init_sigchld(rktio_t *rktio)
{
#if !defined(CENTRALIZED_SIGCHILD)
  if (!sigchld_installed) {
    signal(SIGCHLD, child_done);
    sigchld_installed = 1;
  }

  if (!rktio->in_sigchld_chain) {
    /* Signals are blocked by the caller of init_sigchild */
    rktio->in_sigchld_chain = 1;
    rktio->next = all_rktios;
    all_rktios = rktio;
  }
#endif
}

#if !defined(CENTRALIZED_SIGCHILD)
static void remove_from_sigchld_chain(rktio_t *rktio)
{
  if (rktio->in_sigchld_chain) {
    rktio_t *rio = all_rktios, *prev = NULL;
    while (rio) {
      if (rio == rktio) {
        if (prev)
          prev->next = rktio->next;
        else
          all_rktios = rktio->next;
        return;
      }
      prev = rio;
      rio = rio->next;
    }
  }
}
#endif

static void check_child_done(rktio_t *rktio, pid_t pid)
{
  pid_t result, check_pid;
  int status, is_unused;
  System_Child *sc, *prev;
  void **unused = (void **)unused_pids, **unused_prev = NULL;

  if (pid && rktio->need_to_check_children) {
    rktio->need_to_check_children = 0;
    check_child_done(rktio, 0);
  }
  
  if (rktio->system_children) {
    do {
      if (!pid && unused) {
        check_pid = (pid_t)(intptr_t)unused[0];
        is_unused = 1;
      } else {
        check_pid = pid;
        is_unused = 0;
      }

      do {
        result = waitpid(check_pid, &status, WNOHANG);
      } while ((result == -1) && (errno == EINTR));

      if (result > 0) {
        if (is_unused) {
          /* done with an inaccessible group id */
          void *next;
          next = (void **)unused[1];
          if (unused_prev)
            unused_prev[1] = unused[1];
          else
            unused_pids = unused[1];
          free(unused);
          unused = (void **)next;
        }

        status = extract_child_status(status);

        prev = NULL;
        for (sc = rktio->system_children; sc; prev = sc, sc = sc->next) {
          if (sc->id == result) {
            sc->done = 1;
            sc->status = status;

            if (prev) {
              prev->next = sc->next;
            } else
              rktio->system_children = sc->next;
          }
        }
      } else {
        if (is_unused) {
          unused_prev = unused;
          unused = unused[1];
        }
      }
    } while ((result > 0) || is_unused);
  }
}

void rktio_reap_processes(rktio_t *rktio)
{
  if (rktio->need_to_check_children) {
    rktio->need_to_check_children = 0;
    check_child_done(rktio, 0);
  }
}

#endif

#ifdef RKTIO_SYSTEM_WINDOWS
void rktio_reap_processes(rktio_t *rktio) { }
#endif

/*========================================================================*/
/* Windows process times                                                  */
/*========================================================================*/

/* Unix provides a counter for time consumed by subprocesses, but
   Windows doesn't. */

#if defined(RKTIO_SYSTEM_WINDOWS)
static void collect_process_time(rktio_t *rktio, DWORD w, rktio_process_t *sp)
{
  if ((w != STILL_ACTIVE) && !sp->got_time) {
    FILETIME cr, ex, kr, us;
    if (GetProcessTimes(sp->handle, &cr, &ex, &kr, &us)) {
      rktio_int64_t v;
      uintptr_t msecs;
      v = ((((rktio_int64_t)kr.dwHighDateTime << 32) + kr.dwLowDateTime)
	   + (((rktio_int64_t)us.dwHighDateTime << 32) + us.dwLowDateTime));
      msecs = (uintptr_t)(v / 10000);
      
      rktio->process_children_msecs += msecs;
    }
    sp->got_time = 1;
  }
}
#endif

/*========================================================================*/
/* Process status functions                                               */
/*========================================================================*/

int rktio_poll_process_done(rktio_t *rktio, rktio_process_t *sp)
{
#if defined(RKTIO_SYSTEM_UNIX)
# if defined(CENTRALIZED_SIGCHILD)
  {
    int status;
    if (!sp->done) {
      if (centralized_get_child_status(sp->pid, sp->is_group, 1, &status)) {
        sp->done = 1;
        sp->status = status;
        centralized_ended_child();
        return 1;
      }
      return 0;
    }
    else
      return RKTIO_PROCESS_DONE;
  }
# else
  {
    System_Child *sc;
    sc = (System_Child *)sp->handle;
    /* Check specific pid, in case the child has its own group
       (either given by us or given to itself): */
    check_child_done(rktio, sp->pid);
    return sc->done;
  }
# endif
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  {
    HANDLE sci = (HANDLE)sp->handle;
    DWORD w;
    if (sci) {
      if (GetExitCodeProcess(sci, &w)) {
        collect_process_time(rktio, w, sp);
        return (w != STILL_ACTIVE);
      } else
        return RKTIO_PROCESS_DONE;
    } else
      return RKTIO_PROCESS_DONE;

    get_windows_error();

    return RKTIO_PROCESS_ERROR;
  }
#endif
}

void rktio_poll_add_process(rktio_t *rktio, rktio_process_t *sp, rktio_poll_set_t *fds)
{
  if (rktio_poll_process_done(rktio, sp)) {
    rktio_poll_set_add_nosleep(rktio, fds);
    return;
  }
  
#ifdef RKTIO_SYSTEM_WINDOWS
  rktio_poll_set_add_handle(rktio, (intptr_t)sp->handle, fds, 0);
#endif
}

rktio_status_t *rktio_process_status(rktio_t *rktio, rktio_process_t *sp)
{
  int going = 0, status = 0;
  rktio_status_t *result;

#if defined(RKTIO_SYSTEM_UNIX)
# if defined(CENTRALIZED_SIGCHILD)
  if (sp->done) {
    status = sp->status;
  } else {
    if (!centralized_get_child_status(sp->pid, sp->is_group, 1, &status)) {
      going = 1;
    } else {
      sp->done = 1;
      sp->status = status;
      centralized_ended_child();
    }
  }
# else
  System_Child *sc = (System_Child *)sp->handle;
  check_child_done(rktio, sp->pid);

  if (sc->done) {
    status = sc->status;
  } else
   going = 1;
# endif
#else
# ifdef RKTIO_SYSTEM_WINDOWS
  DWORD w;
  if (sp->handle) {
    if (GetExitCodeProcess((HANDLE)sp->handle, &w)) {
      collect_process_time(rktio, w, sp);
      if (w == STILL_ACTIVE)
        going = 1;
      else
        status = w;
    } else {
      get_windows_error();
      return NULL;
    }
  }
# endif
#endif

  result = malloc(sizeof(rktio_status_t));
  result->running = going;
  result->result = (going ? 0 : status);
  return result;
}

static int do_subprocess_kill(rktio_t *rktio, rktio_process_t *sp, int as_kill)
{
#if defined(RKTIO_SYSTEM_UNIX)
# if defined(CENTRALIZED_SIGCHILD)
  {
    int status;

    if (sp->done)
      return 1;

    centralized_wait_suspend();

    /* Don't allow group checking, because we don't want to wait
       on a group if we haven't already: */
    if (centralized_get_child_status(sp->pid, 0, 0, &status)) {
      sp->status = status;
      sp->done = 1;
      centralized_wait_resume();
      centralized_ended_child();
      return 1;
    }
  }
# else
  {
    System_Child *sc = (System_Child *)sp->handle;

    /* Don't pass sp->pid, because we don't want to wait
       on a group if we haven't already: */
    check_child_done(rktio, 0);
    if (sc->done)
      return 1;
  }
# define centralized_wait_resume() /* empty */
# endif

  while (1) {

    if (sp->is_group) {
      if (!killpg(sp->pid, as_kill ? SIGKILL : SIGINT)) {
        centralized_wait_resume();
        return 1;
      }
    } else {
      if (!kill(sp->pid, as_kill ? SIGKILL : SIGINT)) {
        centralized_wait_resume();
        return 1;
      }
    }
    
    if (errno != EINTR)
      break;
    /* Otherwise we were interrupted. Try `kill' again. */
  }

  get_posix_error();

  centralized_wait_resume();

  return 0;
#endif
#if defined(RKTIO_SYSTEM_WINDOWS)  
  if (as_kill || sp->is_group) {
    DWORD w;

    if (!sp->handle)
      return 1;

    if (!as_kill) {
      /* must be for a group; we don't care whether the
         original process is still running */
      if (GenerateConsoleCtrlEvent(CTRL_BREAK_EVENT, sp->pid))
        return 1;
    } else if (GetExitCodeProcess((HANDLE)sp->handle, &w)) {
      collect_process_time(rktio, w, sp);
      if (w != STILL_ACTIVE)
        return 1;
      if (TerminateProcess((HANDLE)sp->handle, 1))
        return 1;
    }
    get_windows_error();

    return 0;
  } else
    return 1;
#endif
}

int rktio_process_kill(rktio_t *rktio, rktio_process_t *sp)
{
  return do_subprocess_kill(rktio, sp, 1);
}

int rktio_process_interrupt(rktio_t *rktio, rktio_process_t *sp)
{
  return do_subprocess_kill(rktio, sp, 0);
}

void rktio_process_forget(rktio_t *rktio, rktio_process_t *sp)
{
#ifdef RKTIO_SYSTEM_UNIX
# if defined(CENTRALIZED_SIGCHILD)
  if (!sp->done) {
    centralized_done_with_process_id(sp->pid, sp->is_group);
    centralized_ended_child();
  }
# else
  if (!((System_Child *)sp->handle)->done) {
    void **unused_pid;
    unused_pid = malloc(sizeof(void *) * 2);
    unused_pid[0] = (void *)(intptr_t)sp->pid;
    unused_pid[1] = unused_pids;
    rktio->need_to_check_children = 1;
  }
  free(sp->handle);
# endif
#endif

#ifdef RKTIO_SYSTEM_WINDOWS
  CloseHandle(sp->handle);
#endif

  free(sp);
}

int rktio_process_init(rktio_t *rktio)
{
#if defined(CENTRALIZED_SIGCHILD)
  /* Block SIGCHLD as early as possible, because
     it's a per-thread setting on Linux, and we want SIGCHLD blocked everywhere. */
  block_sigchld();

  centralized_start_child_signal_handler();
#endif

  return 1;
}

void rktio_process_deinit(rktio_t *rktio)
{
#ifdef RKTIO_SYSTEM_WINDOWS
  if (rktio->process_job_object) {
    TerminateJobObject(rktio->process_job_object, 1);
    CloseHandle(rktio->process_job_object);
    rktio->process_job_object = NULL;
  }
#endif
#if defined(RKTIO_SYSTEM_UNIX) && !defined(CENTRALIZED_SIGCHILD)
  remove_from_sigchld_chain(rktio);
#endif
}

/*========================================================================*/
/* Windows command-line construction                                      */
/*========================================================================*/

#ifdef RKTIO_SYSTEM_WINDOWS
static char *cmdline_protect(char *s)
{
  char *naya;
  int ds;
  int has_space = 0, has_quote = 0, was_slash = 0;

  if (!*s) return MSC_IZE(strdup)("\"\""); /* quote an empty argument */

  for (ds = 0; s[ds]; ds++) {
    if (isspace(s[ds]) || (s[ds] == '\'')) {
      has_space = 1;
      was_slash = 0;
    } else if (s[ds] == '"') {
      has_quote += 1 + (2 * was_slash);
      was_slash = 0;
    } else if (s[ds] == '\\') {
      was_slash++;
    } else
      was_slash = 0;
  }

  if (has_space || has_quote) {
    char *p;
    int wrote_slash = 0;

    naya = malloc(strlen(s) + 3 + 3*has_quote + was_slash);
    naya[0] = '"';
    for (p = naya + 1; *s; s++) {
      if (*s == '"') {
	while (wrote_slash--) {
	  *(p++) = '\\';
	}
	*(p++) = '"'; /* endquote */
	*(p++) = '\\';
	*(p++) = '"'; /* protected */
	*(p++) = '"'; /* start quote again */
	wrote_slash = 0;
      } else if (*s == '\\') {
	*(p++) = '\\';
	wrote_slash++;
      } else {
	*(p++) = *s;
	wrote_slash = 0;
      }
    }
    while (wrote_slash--) {
      *(p++) = '\\';
    }
    *(p++) = '"';
    *p = 0;

    return naya;
  }

  return MSC_IZE(strdup)(s);
}

static intptr_t do_spawnv(rktio_t *rktio,
                          const char *command, int argc, const char * const *argv,
			  int exact_cmdline, intptr_t sin, intptr_t sout, intptr_t serr, int *pid,
			  int new_process_group, int chain_termination_here_to_child,
                          void *env, const char *wd)
{
  intptr_t i, l, len = 0;
  int use_jo;
  intptr_t cr_flag;
  char *cmdline;
  wchar_t *cmdline_w, *wd_w, *command_w;
  STARTUPINFOW startup;
  PROCESS_INFORMATION info;

  if (exact_cmdline) {
    cmdline = (char *)argv[1];
  } else {
    for (i = 0; i < argc; i++) {
      len += strlen(argv[i]) + 1;
    }

    cmdline = malloc(len);

    len = 0;
    for (i = 0; i < argc; i++) {
      l = strlen(argv[i]);
      memcpy(cmdline + len, argv[i], l);
      cmdline[len + l] = ' ';
      len += l + 1;
    }
    --len;
    cmdline[len] = 0;
  }

  memset(&startup, 0, sizeof(startup));
  startup.cb = sizeof(startup);
  startup.dwFlags = STARTF_USESTDHANDLES;
  startup.hStdInput = (HANDLE)sin;
  startup.hStdOutput = (HANDLE)sout;
  startup.hStdError = (HANDLE)serr;

  /* If none of the stdio handles are consoles, specifically
     create the subprocess without a console: */
  if (!rktio_system_fd_is_terminal(rktio, (intptr_t)startup.hStdInput)
      && !rktio_system_fd_is_terminal(rktio, (intptr_t)startup.hStdOutput)
      && !rktio_system_fd_is_terminal(rktio, (intptr_t)startup.hStdError))
    cr_flag = CREATE_NO_WINDOW;
  else
    cr_flag = 0;
  if (new_process_group)
    cr_flag |= CREATE_NEW_PROCESS_GROUP;
  cr_flag |= CREATE_UNICODE_ENVIRONMENT;

  use_jo = chain_termination_here_to_child;
  if (use_jo) {
    /* Use a job object to ensure that the new process will be terminated
       if this process ends for any reason (including a crash) */
    if (!rktio->process_job_object) {
      JOBOBJECT_EXTENDED_LIMIT_INFORMATION jeli;

      rktio->process_job_object = CreateJobObject(NULL, NULL);

      memset(&jeli, 0, sizeof(jeli));
      jeli.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
      SetInformationJobObject(rktio->process_job_object,
			      JobObjectExtendedLimitInformation,
			      &jeli,
			      sizeof(jeli));
    }
  }

  
  cmdline_w = WIDE_PATH_copy(cmdline);
  if (!exact_cmdline)
    free(cmdline);
  wd_w = WIDE_PATH_copy(wd);
  command_w = WIDE_PATH_temp(command);

  if (cmdline_w
      && wd_w
      && command_w
      && CreateProcessW(command_w, cmdline_w, 
                        NULL, NULL, 1 /*inherit*/,
                        cr_flag, env, wd_w,
                        &startup, &info)) {
    if (use_jo)
      AssignProcessToJobObject(rktio->process_job_object, info.hProcess);
    CloseHandle(info.hThread);
    *pid = info.dwProcessId;
    free(cmdline_w);
    free(wd_w);
    return (intptr_t)info.hProcess;
  } else {
    if (cmdline_w) free(cmdline_w);
    if (wd_w) free(wd_w);
    return -1;
  }
}

static void CopyFileHandleForSubprocess(intptr_t *hs, int pos)
{
  HANDLE h2;
  int alt_pos = (pos ? 0 : 1);

  if (DuplicateHandle(GetCurrentProcess(),
		      (HANDLE)hs[pos],
		      GetCurrentProcess(),
		      &h2,
		      0,
		      TRUE,
		      DUPLICATE_SAME_ACCESS)) {
    hs[pos] = (intptr_t)h2;
    hs[alt_pos] = 1;
  } else {
    hs[alt_pos] = 0;
  }
}

static void CloseFileHandleForSubprocess(intptr_t *hs, int pos)
{
  int alt_pos = (pos ? 0 : 1);
  if (hs[alt_pos]) {
    CloseHandle((HANDLE)hs[pos]);
  }
}

#define RKTIO_COPY_FOR_SUBPROCESS(array, pos) CopyFileHandleForSubprocess(array, pos)
#define RKTIO_CLOSE_SUBPROCESS_COPY(array, pos) CloseFileHandleForSubprocess(array, pos)
#define RKTIO_CLOSE(fd) CloseHandle((HANDLE)fd)

#endif /* RKTIO_SYSTEM_WINDOWS */

#ifdef RKTIO_SYSTEM_UNIX

# define RKTIO_COPY_FOR_SUBPROCESS(array, pos) /* empty */
# define RKTIO_CLOSE_SUBPROCESS_COPY(array, pos) /* empty */
# define RKTIO_CLOSE(fd) rktio_reliably_close(fd)

#endif

int rktio_process_allowed_flags(rktio_t *rktio)
{
  int flags = (RKTIO_PROCESS_NEW_GROUP
               | RKTIO_PROCESS_STDOUT_AS_STDERR);
#ifdef RKTIO_SYSTEM_WINDOWS
  flags |= (RKTIO_PROCESS_WINDOWS_EXACT_CMDLINE
            | RKTIO_PROCESS_WINDOWS_CHAIN_TERMINATION);
#endif
  return flags;
}

/*========================================================================*/
/* Main process-creation function                                         */
/*========================================================================*/

rktio_process_result_t *rktio_process(rktio_t *rktio,
                                      const char *command, int argc, char **argv,
                                      rktio_fd_t *stdout_fd, rktio_fd_t *stdin_fd, rktio_fd_t *stderr_fd,
                                      const char *current_directory, rktio_envvars_t *envvars,
                                      int flags)
{
  rktio_process_result_t *result;
  intptr_t to_subprocess[2], from_subprocess[2], err_subprocess[2];
  int pid;
#if defined(RKTIO_SYSTEM_UNIX)
# if !defined(CENTRALIZED_SIGCHILD)
  System_Child *sc;
# endif
#else
  void *sc = 0;
#endif
  void *env;
  rktio_process_t *subproc;
#if defined(RKTIO_SYSTEM_WINDOWS)
  intptr_t spawn_status;
#endif
  int new_process_group = (flags & RKTIO_PROCESS_NEW_GROUP);
  int stderr_is_stdout = (flags & RKTIO_PROCESS_STDOUT_AS_STDERR);
#if defined(RKTIO_SYSTEM_WINDOWS)
  int windows_exact_cmdline = (flags & RKTIO_PROCESS_WINDOWS_EXACT_CMDLINE);
  int windows_chain_termination_to_child = (flags & RKTIO_PROCESS_WINDOWS_CHAIN_TERMINATION);
  int i;
#endif

  /* avoid compiler warnings: */
  to_subprocess[0] = -1;
  to_subprocess[1] = -1;
  from_subprocess[0] = -1;
  from_subprocess[1] = -1;
  err_subprocess[0] = -1;
  err_subprocess[1] = -1;

  /*--------------------------------------*/
  /*          Create needed pipes         */
  /*--------------------------------------*/

  if (stdout_fd) {
    from_subprocess[1] = rktio_fd_system_fd(rktio, stdout_fd);
    RKTIO_COPY_FOR_SUBPROCESS(from_subprocess, 1);
  } else if (rktio_make_os_pipe(rktio, from_subprocess, RKTIO_NO_INHERIT_INPUT)) {
    return NULL;
  }

  if (stdin_fd) {
    to_subprocess[0] = rktio_fd_system_fd(rktio, stdin_fd);
    RKTIO_COPY_FOR_SUBPROCESS(to_subprocess, 0);
  } else if (rktio_make_os_pipe(rktio, to_subprocess, RKTIO_NO_INHERIT_OUTPUT)) {
    if (stdout_fd) { RKTIO_CLOSE_SUBPROCESS_COPY(from_subprocess, 1); }
    return NULL;
  }

  if (stderr_fd) {
    err_subprocess[1] = rktio_fd_system_fd(rktio, stderr_fd);
    RKTIO_COPY_FOR_SUBPROCESS(err_subprocess, 1);
  } else if (stderr_is_stdout) {
    err_subprocess[0] = from_subprocess[0];
    err_subprocess[1] = from_subprocess[1];
  } else if (rktio_make_os_pipe(rktio, err_subprocess, RKTIO_NO_INHERIT_INPUT)) {
    if (stdout_fd) { RKTIO_CLOSE_SUBPROCESS_COPY(from_subprocess, 1); }
    if (stdin_fd) { RKTIO_CLOSE_SUBPROCESS_COPY(to_subprocess, 0); }
    return NULL;
  }

  if (envvars)
    env = rktio_envvars_to_block(rktio, envvars);
  else
    env = NULL;

#if defined(RKTIO_SYSTEM_WINDOWS)

  /*--------------------------------------*/
  /*        Execute: Windows              */
  /*--------------------------------------*/

  /* Windows: quasi-stdin is locked, and we'll say it doesn't matter */
  fflush(stdin);
  fflush(stdout);
  fflush(stderr);

  {
    char **new_argv;

    if (!windows_exact_cmdline) {
      /* protect spaces, etc. in the arguments: */
      new_argv = malloc(sizeof(char *) * argc);
      for (i = 0; i < argc; i++) {
	new_argv[i] = cmdline_protect(argv[i]);
      }
      argv = new_argv;
    }

    pid = 0;

    spawn_status = do_spawnv(rktio,
                             command, argc, (const char * const *)argv,
			     windows_exact_cmdline,
			     to_subprocess[0],
			     from_subprocess[1],
			     err_subprocess[1],
			     &pid,
                             new_process_group,
                             windows_chain_termination_to_child,
                             env, current_directory);

    if (!windows_exact_cmdline) {
      for (i = 0; i < argc; i++) {
        free(argv[i]);
      }
      free(argv);
    }

    if (spawn_status != -1)
      sc = (void *)spawn_status;
  }

#else


  /*--------------------------------------*/
  /*            Execute: Unix             */
  /*--------------------------------------*/

  {
#if defined(CENTRALIZED_SIGCHILD)
    centralized_starting_child();
#else
    sc = malloc(sizeof(System_Child));
    sc->id = 0;
    sc->done = 0;

    block_child_signals(rktio, 1);

    /* Relies on signals blocked: */
    init_sigchld(rktio);
#endif

#if defined(__QNX__)
    pid = vfork();
#elif defined(SUBPROCESS_USE_FORK1)
    pid = fork1();
#else
    pid = fork();
#endif

    if (pid > 0) {
      /* This is the original process, which needs to manage the 
         newly created child process. */
      
      if (new_process_group)
        /* there's a race condition between this use and the exec(),
           and there's a race condition between the other setpgid() in
           the child processand sending signals from the parent
           process; so, we set in both, and at least one will
           succeed; we could perform better error checking, since
           EACCES is the only expected error */
        setpgid(pid, pid);

#if defined(CENTRALIZED_SIGCHILD)
      {
        rktio_signal_handle_t *signal_fd;
        int status;
        signal_fd = rktio_get_signal_handle(rktio);
        centralized_register_child(pid, new_process_group, signal_fd, &status);

        /* printf("SUBPROCESS  %i\n", pid); */
      }
#else
      sc->next = rktio->system_children;
      rktio->system_children = sc;
      sc->id = pid;
#endif
    } else if (!pid) {
      /* This is the new child process */
      if (new_process_group)
        /* see also setpgid above */
        setpgid(getpid(), getpid()); /* setpgid(0, 0) would work on some platforms */
    } else {
      get_posix_error();
    }

#if !defined(CENTRALIZED_SIGCHILD)
    block_child_signals(rktio, 0);
#else
    if (!pid)
      centralized_unblock_child_signal();
    else if (pid == -1)
      centralized_ended_child();
#endif
  }

  switch (pid)
    {
    case -1:
      /* Close all created descriptors */
      if (!stdin_fd) {
	rktio_reliably_close(to_subprocess[0]);
	rktio_reliably_close(to_subprocess[1]);
      } else {
	RKTIO_CLOSE_SUBPROCESS_COPY(to_subprocess, 0);
      }
      if (!stdout_fd) {
	rktio_reliably_close(from_subprocess[0]);
	rktio_reliably_close(from_subprocess[1]);
      } else {
	RKTIO_CLOSE_SUBPROCESS_COPY(from_subprocess, 1);
      }
      if (!stderr_fd) {
        if (!stderr_is_stdout) {
          rktio_reliably_close(err_subprocess[0]);
          rktio_reliably_close(err_subprocess[1]);
        }
      } else {
	RKTIO_CLOSE_SUBPROCESS_COPY(err_subprocess, 1);
      }
      return NULL;

    case 0: /* child */

      {
        int errid;
        
	/* Copy pipe descriptors to stdin and stdout */
	do {
	  errid = MSC_IZE(dup2)(to_subprocess[0], 0);
	} while (errid == -1 && errno == EINTR);
	do {
	  errid = MSC_IZE(dup2)(from_subprocess[1], 1);
	} while (errid == -1 && errno == EINTR);
	do {
	  errid = MSC_IZE(dup2)(err_subprocess[1], 2);
	} while (errid == -1 && errno == EINTR);

	/* Close unwanted descriptors */
	if (!stdin_fd) {
	  rktio_reliably_close(to_subprocess[0]);
	  rktio_reliably_close(to_subprocess[1]);
	}
	if (!stdout_fd) {
	  rktio_reliably_close(from_subprocess[0]);
	  rktio_reliably_close(from_subprocess[1]);
	}
	if (!stderr_fd) {
          if (!stderr_is_stdout) {
            rktio_reliably_close(err_subprocess[0]);
            rktio_reliably_close(err_subprocess[1]);
          }
	}

        rktio_close_fds_after_fork(0, 1, 2);
      }

      /* Set real CWD: */
      if (!rktio_set_current_directory(rktio, current_directory)) {
        fprintf(stderr, "racket: chdir failed to: %s\n", current_directory);
        _exit(1);
      }

      /* Exec new process */

      {
	int err, i;
        char **new_argv;

        /* add a NULL terminator */
        new_argv = malloc(sizeof(char *) * (argc + 1));
        for (i = 0; i < argc; i++) {
          new_argv[i] = argv[i];
        }
        new_argv[i] = NULL;

        if (!env)
          env = rktio_get_environ_array();
        
	err = MSC_IZE(execve)(command, new_argv, (char **)env);
        if (err)
          err = errno;

        if (envvars)
          free(env);

	/* If we get here it failed; give up */

        fprintf(stderr, "exec failed (%s%serrno=%d)\n", 
                strerror(err), "; ",
                err);

	_exit(1);
      }

    default: /* parent */

      break;
    }
#endif

  /*--------------------------------------*/
  /*      Close unneeded descriptors      */
  /*--------------------------------------*/

  free(env);

  if (!stdin_fd)
    RKTIO_CLOSE(to_subprocess[0]);
  else
    RKTIO_CLOSE_SUBPROCESS_COPY(to_subprocess, 0);
  
  if (!stdout_fd) 
    RKTIO_CLOSE(from_subprocess[1]);
  else
    RKTIO_CLOSE_SUBPROCESS_COPY(from_subprocess, 1);
  
  if (!stderr_fd) {
    if (!stderr_is_stdout)
      RKTIO_CLOSE(err_subprocess[1]);
  } else
    RKTIO_CLOSE_SUBPROCESS_COPY(err_subprocess, 1);

  /*--------------------------------------*/
  /*  Create new file-descriptor objects  */
  /*--------------------------------------*/

  result = malloc(sizeof(rktio_process_result_t));

  if (!stdout_fd)
    result->stdout_fd = rktio_system_fd(rktio, from_subprocess[0], RKTIO_OPEN_READ);
  else
    result->stdout_fd = NULL;
  if (!stdin_fd)
    result->stdin_fd = rktio_system_fd(rktio, to_subprocess[1], RKTIO_OPEN_WRITE);
  else
    result->stdin_fd = NULL;
  if (!stderr_fd && !stderr_is_stdout)
    result->stderr_fd = rktio_system_fd(rktio, err_subprocess[0], RKTIO_OPEN_READ);
  else
    result->stderr_fd = NULL;

  /*--------------------------------------*/
  /*          Return result info          */
  /*--------------------------------------*/

  subproc = malloc(sizeof(rktio_process_t));
  memset(subproc, 0, sizeof(rktio_process_t));
#if !defined(CENTRALIZED_SIGCHILD)
  subproc->handle = (void *)sc;
#endif
  subproc->pid = pid;
  subproc->is_group = new_process_group;

  result->process = subproc;

  return result;
}

int rktio_process_pid(rktio_t *rktio, rktio_process_t *sp)
{
  return sp->pid;
}

#ifdef RKTIO_SYSTEM_UNIX
void rktio_close_fds_after_fork(int skip1, int skip2, int skip3)
{
  int i;

# ifdef USE_ULIMIT
  i = ulimit(4, 0);
# elif defined(__ANDROID__)
  i = sysconf(_SC_OPEN_MAX);
# else
  i = getdtablesize();
# endif
  while (i--) {
    int cr;
    if ((i != skip1) && (i != skip2) && (i != skip3)) {
      do {
        cr = close(i);
      } while ((cr == -1) && (errno == EINTR));
    }
  }
}
#endif
