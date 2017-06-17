#include "rktio.h"
#include "rktio_private.h"
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#if defined(HAVE_KQUEUE_SYSCALL)
# include <fcntl.h>
#endif
#if defined(HAVE_INOTIFY_SYSCALL)
# include <poll.h>
# include <unistd.h>
# include <sys/inotify.h>
#endif

#if defined(HAVE_KQUEUE_SYSCALL)                \
  || defined(RKTIO_SYSTEM_WINDOWS)              \
  || defined(HAVE_INOTIFY_SYSCALL)		\
  || defined(FILESYSTEM_NEVER_CHANGES)
# define HAVE_FILESYSTEM_CHANGE_EVTS
#else
# define NO_FILESYSTEM_CHANGE_EVTS
#endif

#if defined(HAVE_INOTIFY_SYSCALL)
static void do_inotify_init(rktio_t *rktio);
static int do_inotify_ready(rktio_t *rktio);
static int do_inotify_errid(rktio_t *rktio);
static int do_inotify_add(rktio_t *rktio, const char *filename);
static void do_inotify_remove(rktio_t *rktio, int p2);
static int do_inotify_poll(rktio_t *rktio, int p2);
static void do_inotify_stop(rktio_t *rktio);
static int do_inotify_fd(rktio_t *rktio);
#endif

/*========================================================================*/
/* Common type and functions                                              */
/*========================================================================*/

struct rktio_fs_change_t {
  int done;
  intptr_t fd;
#if defined(HAVE_KQUEUE_SYSCALL) && !defined(FILESYSTEM_NEVER_CHANGES)
  rktio_ltps_t *lt;
  rktio_ltps_handle_t *lth;
#endif
};

int rktio_fs_change_properties(rktio_t *rktio)
{
  int flags = 0;
  
#ifdef NO_FILESYSTEM_CHANGE_EVTS
#else
  flags |= RKTIO_FS_CHANGE_SUPPORTED;
# if defined(HAVE_KQUEUE_SYSCALL)
  flags |= RKTIO_FS_CHANGE_NEED_LTPS;
# else
  flags |= RKTIO_FS_CHANGE_SCALABLE;
# endif
# if !defined(HAVE_INOTIFY_SYSCALL)
  flags |= RKTIO_FS_CHANGE_LOW_LATENCY;
# endif
# if !defined(RKTIO_SYSTEM_WINDOWS)
  flags |= RKTIO_FS_CHANGE_FILE_LEVEL;
# endif
#endif

  return flags;
}

rktio_fs_change_t *rktio_fs_change(rktio_t *rktio, const char *path, rktio_ltps_t *lt)
{
  int ok = 0;
#ifndef NO_FILESYSTEM_CHANGE_EVTS
# if defined(HAVE_KQUEUE_SYSCALL)
  rktio_ltps_handle_t *lth;
# endif
#endif
  intptr_t fd;
  rktio_fs_change_t *fc;

  fd = 0;

#if defined(NO_FILESYSTEM_CHANGE_EVTS)
  set_racket_error(RKTIO_ERROR_UNSUPPORTED);
  ok = 0;
#elif defined(FILESYSTEM_NEVER_CHANGES)
  ok = 1;
#elif defined(HAVE_KQUEUE_SYSCALL)
  if (!lt) {
    set_racket_error(RKTIO_ERROR_UNSUPPORTED);
    ok = 0;
  } else {
    do {
      fd = open(path, RKTIO_BINARY, 0666);
    } while ((fd == -1) && (errno == EINTR));
    if (fd == -1)
      get_posix_error();
    else {
      rktio_fd_t *rfd;
      rfd = rktio_system_fd(rktio, fd, 0);
      lth = rktio_ltps_add(rktio, lt, rfd, RKTIO_LTPS_CREATE_VNODE);
      if (!lth) {
        rktio_reliably_close(fd);
      } else {
        /* Put any pointer in the handle, and set it to auto-handle mode
           to clear the pointer if it gets signalled. */
        rktio_ltps_handle_set_data(rktio, lth, lth);
        rktio_ltps_handle_set_auto(rktio, lth, RKTIO_LTPS_HANDLE_ZERO);
        ok = 1;
      }
      rktio_forget(rktio, rfd);
    }
  }
#elif defined(HAVE_INOTIFY_SYSCALL)
  do_inotify_init(rktio);
  if (!do_inotify_ready(rktio)) {
    errno = do_inotify_errid(rktio);
    get_posix_error();
  } else {
    fd = do_inotify_add(rktio, path);
    if (fd == -1)
      get_posix_error();
    else
      ok = 1;
  }
#elif defined(RKTIO_SYSTEM_WINDOWS)
  {
    HANDLE h;
    
    h = FindFirstChangeNotificationW(WIDE_PATH_temp(path), FALSE,
                                     (FILE_NOTIFY_CHANGE_FILE_NAME
                                      | FILE_NOTIFY_CHANGE_DIR_NAME
                                      | FILE_NOTIFY_CHANGE_SIZE
                                      | FILE_NOTIFY_CHANGE_LAST_WRITE
                                      | FILE_NOTIFY_CHANGE_ATTRIBUTES));
    if (h == INVALID_HANDLE_VALUE)
      get_windows_error();
    else {
      fd = (intptr_t)h;
      ok = 1;
    }
  }
#endif

  if (!ok)
    return NULL;

  fc = malloc(sizeof(rktio_fs_change_t));
  fc->done = 0;
  fc->fd = fd;
#if defined(HAVE_KQUEUE_SYSCALL) && !defined(NO_FILESYSTEM_CHANGE_EVTS)
  fc->lt = lt;
  fc->lth = lth;
#endif

  return fc;
}

static void fs_change_release(rktio_t *rktio, rktio_fs_change_t *fc)
{
  if (fc->done)
    return;
  
# if defined(FILESYSTEM_NEVER_CHANGES)
  /* nothing to do */
# elif defined(RKTIO_SYSTEM_WINDOWS)
  FindCloseChangeNotification((HANDLE)fc->fd);
# elif defined(HAVE_INOTIFY_SYSCALL)
  do_inotify_remove(rktio, fc->fd);
# elif defined(HAVE_KQUEUE_SYSCALL)
  if (rktio_ltps_handle_get_data(rktio, fc->lth)) {
    /* Not zeroed, so never signaled. Change the auto behavior
       to free the handle, and deregsiter the file descriptor. */
    rktio_fd_t *rfd;
    rktio_ltps_handle_set_auto(rktio, fc->lth, RKTIO_LTPS_HANDLE_FREE);
    rfd = rktio_system_fd(rktio, fc->fd, 0);
    (void)rktio_ltps_add(rktio, fc->lt, rfd, RKTIO_LTPS_REMOVE_VNODE);
    rktio_close(rktio, rfd);
  } else {
    /* Was signaled, so we need to free it. */
    free(fc->lth);
  }
#endif

  fc->done = 1;
}

void rktio_fs_change_forget(rktio_t *rktio, rktio_fs_change_t *fc)
{
  fs_change_release(rktio, fc);
  free(fc);
}

int rktio_poll_fs_change_ready(rktio_t *rktio, rktio_fs_change_t *fc)
{
#if defined(NO_FILESYSTEM_CHANGE_EVTS)
  return 0;
#elif defined(RKTIO_SYSTEM_WINDOWS)
  if (!fc->done) {
    if (WaitForSingleObject((HANDLE)fc->fd, 0) == WAIT_OBJECT_0)
      fs_change_release(rktio, fc);
  }
  
  return (fc->done ? RKTIO_POLL_READY : 0);
#elif defined(HAVE_INOTIFY_SYSCALL)
  if (!fc->done) {
    int r = do_inotify_poll(rktio, fc->fd);
    if (r < 0)
      return RKTIO_POLL_ERROR;
    if (r)
      fs_change_release(rktio, fc);
  }
  
  return (fc->done ? RKTIO_POLL_READY : 0);
#elif defined(FILESYSTEM_NEVER_CHANGES)
  return 0;
#elif defined(HAVE_KQUEUE_SYSCALL)
  if (!fc->done) {
    (void)rktio_ltps_poll(rktio, fc->lt);
    if (!rktio_ltps_handle_get_data(rktio, fc->lth)) {
      /* NULL value means that it was signaled; can free, etc. */
      fs_change_release(rktio, fc);
    }
  }

  return (fc->done ? RKTIO_POLL_READY : 0);
#endif
}

void rktio_poll_add_fs_change(rktio_t *rktio, rktio_fs_change_t *fc, rktio_poll_set_t *fds)
{
  if (fc->done) {
    rktio_poll_set_add_nosleep(rktio, fds);
    return;
  }

#if defined(NO_FILESYSTEM_CHANGE_EVTS)
#elif defined(RKTIO_SYSTEM_WINDOWS)
  rktio_poll_set_add_handle(rktio, fc->fd, fds, 0);
#elif defined(HAVE_INOTIFY_SYSCALL)
  int fd;
  fd = do_inotify_fd(rktio);
  if (fd >= 0) {
    rktio_poll_set_t *fds2;
    RKTIO_FD_SET(fd, fds);
    fds2 = RKTIO_GET_FDSET(fds, 2);
    RKTIO_FD_SET(fd, fds2);
  } else if (fd == -2) {
    rktio_poll_set_add_nosleep(rktio, fds);
  }
#elif defined(FILESYSTEM_NEVER_CHANGES)
#elif defined(HAVE_KQUEUE_SYSCALL)
  {
    int fd = rktio_ltps_get_fd(fc->lt);
    rktio_poll_set_t *fds2;
    RKTIO_FD_SET(fd, fds);
    fds2 = RKTIO_GET_FDSET(fds, 2);
    RKTIO_FD_SET(fd, fds2);
  }
#endif
}

void rktio_stop_fs_change(rktio_t *rktio)
{
#ifdef HAVE_INOTIFY_SYSCALL
  do_inotify_stop(rktio);
#endif
}

/*========================================================================*/
/* inotify                                                                */
/*========================================================================*/

/* Multiplex multiple filesystem change events onto a single
   inotify connection. That's almost as easy as using watch
   descriptors in place of file descriptors, but using the
   same filesystem path multiple times produces the same
   watch descriptors, so reference-count it. Also, each watch
   can be removed as soon as it fires, since filesystem
   change events are single-shot.

   The values returned by do_inotify_add() are indices into an array
   of watch descriptors. There's room for a better data structure if
   the watch-descriptor-to-index mapping becomes too slow. */

#ifdef HAVE_INOTIFY_SYSCALL

typedef struct rin_wd_t {
  int wd;
  int refcount;
  int val;
} rin_wd_t;

typedef struct rin_inotify_state_t {
  int ready, errid, fd;
  rin_wd_t *wds;
  int size, count;
  int got;
} rin_inotify_state_t;

static int rin_find_wd(int wd, rin_wd_t *wds, int size)
{
  int i;
  for (i = 0; i < size; i++) {
    if (wds[i].wd == wd) return i;
  }

  return -1;
}

static int rin_add_wd(int wd, rin_wd_t *wds, int size)
{
  int i;

  for (i = 0; i < size; i++) {
    if (wds[i].wd == wd) {
      wds[i].refcount++;
      return i;
    }
  }

  for (i = 0; i < size; i++) {
    if (!wds[i].refcount) {
      wds[i].wd = wd;
      wds[i].refcount = 1;
      wds[i].val = 0;
      return i;
    }
  }

  abort();
  return -1;
}

static int rin_pull_events(rktio_t *rktio, int fd, rin_wd_t *wds, int size)
{
  struct inotify_event _ev, *ev;
  void *b = NULL;
  int rc, p, got = 0;
  int bsize;
  struct pollfd pfd[1];

  ev = &_ev;
  bsize = sizeof(_ev);

  pfd[0].fd = fd;
  pfd[0].events = POLLIN;

  while (poll(pfd, 1, 0)) {
    rc = read(fd, ev, bsize);
    if (rc > 0) {
      p = rin_find_wd(ev->wd, wds, size);
      if (p != -1) {
	got = 1;
	wds[p].val = 1;
	wds[p].wd = -1;
	inotify_rm_watch(fd, ev->wd);
      }
    } else if (rc == -1) {
      if (errno == EAGAIN)
	break;
      else if (errno == EINTR) {
	/* try again */
      } else if (errno == EINVAL) {
	bsize *= 2;
	if (b) free(b);
	b = malloc(bsize);
	ev = (struct inotify_event *)b;
      } else {
        get_posix_error();
        return -1;
      }
    } else
      break;
  }

  if (b)
    free (b);

  return got;
}

static void rin_inotify_start(rin_inotify_state_t *s)
{
  int fd;

  fd = inotify_init();
  if (fd == -1) {
    s->errid = errno;
  } else {
    s->errid = 0;
    s->ready = 1;
    s->fd = fd;
  }
}

static void rin_inotify_end(rin_inotify_state_t *s)
{
  int rc;

  if (s->ready) {
    do {
      rc = close(s->fd);
    } while (rc == -1 && errno == EINTR);
  }

  if (s->wds) free(s->wds);

  free(s);
}

static void do_inotify_init(rktio_t *rktio)
{
  rin_inotify_state_t *s = rktio->inotify_server;

  if (!s) {
    s = (rin_inotify_state_t *)malloc(sizeof(rin_inotify_state_t));
    memset(s, 0, sizeof(rin_inotify_state_t));

    rktio->inotify_server = s;
  }

  if (!s->ready)
    rin_inotify_start(s);
}

static int do_inotify_ready(rktio_t *rktio)
{
  rin_inotify_state_t *s = rktio->inotify_server;
  
  return s->ready;
}

static int do_inotify_errid(rktio_t *rktio)
{
  rin_inotify_state_t *s = rktio->inotify_server;

  return s->errid;
}

/* Other functions are called only if do_inotify_ready() returns 1. */

static int do_inotify_add(rktio_t *rktio, const char *filename)
{
  rin_inotify_state_t *s = rktio->inotify_server;
  int wd;

  if (s->count == s->size) {
    int new_size = (s->size ? (2 * s->size) : 32);
    rin_wd_t *new_wds;
    int i;
    new_wds = (rin_wd_t *)malloc(sizeof(rin_wd_t) * new_size);
    memcpy(new_wds, s->wds, s->size * sizeof(rin_wd_t));
    if (s->wds) free(s->wds);
    s->wds = new_wds;
    s->size = new_size;
    for (i = s->count; i < s->size; i++)
    {
      s->wds[i].wd = -1;
      s->wds[i].refcount = 0;
    }
  }

  wd = inotify_add_watch(s->fd, filename, 
			 (IN_CREATE | IN_DELETE | IN_DELETE_SELF
			  | IN_MODIFY | IN_MOVE_SELF | IN_MOVED_TO
			  | IN_ATTRIB | IN_ONESHOT));

  if (wd == -1)
    return -1;
  else {
    int p;

    p = rin_add_wd(wd, s->wds, s->size);
    if (s->wds[p].refcount == 1)
      s->count++;
    
    return p+1;
  }
}

static void do_inotify_remove(rktio_t *rktio, int p2)
{
  rin_inotify_state_t *s = rktio->inotify_server;
  int p = p2 - 1;

  if (s->wds[p].refcount == 1) {
    if (s->wds[p].wd != -1) {
      inotify_rm_watch(s->fd, s->wds[p].wd);
      s->wds[p].wd = -1;
      /* in case the wd gets reused: */
      if (rin_pull_events(rktio, s->fd, s->wds, s->size) > 0)
	s->got = 1;
    }
    --s->count;
  }
  s->wds[p].refcount -= 1;
}

static int do_inotify_poll(rktio_t *rktio, int p2)
{
  rin_inotify_state_t *s = rktio->inotify_server;
  int p = p2 - 1, r;

  r = rin_pull_events(rktio, s->fd, s->wds, s->size);
  if (r > 0)
    s->got = 1;

  if (r < 0)
    return -1;
  else if (s->wds[p].val)
    return 1;
  else
    return 0;
}

static void do_inotify_stop(rktio_t *rktio)
{
  rin_inotify_state_t *s = rktio->inotify_server;

  if (s) {
    rin_inotify_end(s);
    rktio->inotify_server = NULL;
  }
}

static int do_inotify_fd(rktio_t *rktio)
{
  rin_inotify_state_t *s = rktio->inotify_server;

  if (s->got) {
    /* In case we received something for Y in a poll for X */
    s->got = 0;
    return -2;
  }

  return s->fd;
}
#endif
