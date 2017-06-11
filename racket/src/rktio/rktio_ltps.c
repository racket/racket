#include "rktio.h"
#include "rktio_private.h"
#ifdef RKTIO_SYSTEM_UNIX
# include <errno.h>
#endif
#if defined(HAVE_KQUEUE_SYSCALL) || defined(HAVE_EPOLL_SYSCALL)
# include <stdio.h>
#endif
#ifdef HAVE_KQUEUE_SYSCALL
# include <unistd.h>
# include <sys/types.h>
# include <sys/event.h>
# include <sys/time.h>
#endif
#ifdef HAVE_POLL_SYSCALL
# include <poll.h>
#endif
#include <stdlib.h>

struct rktio_ltps_t {
#if defined(HAVE_KQUEUE_SYSCALL) || defined(HAVE_EPOLL_SYSCALL)
  int fd;
#else
  rktio_poll_set_t *fd_set;
#endif
  /* List of pending signaled handles: */
  struct rktio_ltps_handle_t *signaled;
  /* Hash table mapping fds to handles */
  struct ltps_bucket_t *buckets;
  intptr_t size, count;
};

struct rktio_ltps_handle_t {
  void *data; /* arbitrary data from client */
  struct rktio_ltps_handle_t *next; /* in signaled chain */
};

typedef struct rktio_ltps_handle_pair_t {
  rktio_ltps_handle_t *read_handle;
  rktio_ltps_handle_t *write_handle;
} rktio_ltps_handle_pair_t;

static rktio_ltps_handle_pair_t *ltps_hash_get(rktio_ltps_t *lt, intptr_t fd);
static void ltps_hash_set(rktio_ltps_t *lt, intptr_t fd, rktio_ltps_handle_pair_t *v);
static void ltps_hash_remove(rktio_ltps_t *lt, intptr_t fd);
static int ltps_is_hash_empty(rktio_ltps_t *lt);
static void ltps_hash_init(rktio_ltps_t *lt);
static void ltps_hash_free(rktio_ltps_t *lt);

/*========================================================================*/

rktio_ltps_handle_pair_t *make_ltps_handle_pair()
{
  rktio_ltps_handle_pair_t *v;
  v = malloc(sizeof(rktio_ltps_handle_pair_t));
  v->read_handle = NULL;
  v->write_handle = NULL;
  return v;
}

rktio_ltps_handle_t *make_ltps_handle()
{
  rktio_ltps_handle_t *s;
  s = malloc(sizeof(rktio_ltps_handle_t));
  s->data = NULL;
  return s;
}

void ltps_signal_handle(rktio_ltps_t *lt, rktio_ltps_handle_t *s)
{
  s->next = lt->signaled;
  lt->signaled = s;
}

void rktio_ltps_handle_set_data(rktio_ltps_t *lt, rktio_ltps_handle_t *s, void *data)
{
  s->data = data;
}

void *rktio_ltps_handle_get_data(rktio_ltps_t *lt, rktio_ltps_handle_t *s)
{
  return s->data;
}

/*========================================================================*/

rktio_ltps_t *rktio_open_ltps(rktio_t *rktio)
{
  rktio_ltps_t *lt;

  lt = malloc(sizeof(rktio_ltps_t));
#if defined(HAVE_KQUEUE_SYSCALL) || defined(HAVE_EPOLL_SYSCALL)
  lt->fd = -1;
#else
  lt->fd_set = rktio_alloc_fdset_array(3);
#endif

  lt->signaled = NULL;

  ltps_hash_init(lt);

  return lt;
}

#if defined(HAVE_KQUEUE_SYSCALL) || defined(HAVE_EPOLL_SYSCALL)
int rktio_ltps_get_fd(rktio_ltps_t *lt)
{
  return lt->fd;
}
#else
rktio_poll_set_t *rktio_ltps_get_fd_set(rktio_ltps_t *lt)
{
  return lt->fd_set;
}
#endif


int rktio_ltps_close(rktio_t *rktio, rktio_ltps_t *lt)
{
  rktio_ltps_handle_t *s;

  while ((s = rktio_ltps_get_signaled_handle(rktio, lt)))
    free(s);

  ltps_hash_free(lt);

#if defined(HAVE_KQUEUE_SYSCALL) || defined(HAVE_EPOLL_SYSCALL)
  if (lt->fd >= 0) {
    intptr_t rc;
    do {
      rc = close(lt->fd);
    } while ((rc == -1) && (errno == EINTR));
  }
  free(lt);
#else
  rktio_free_fdset_array(lt->fd_set, 3);
#endif
  
  return 1;
}

rktio_ltps_handle_t *rktio_ltps_get_signaled_handle(rktio_t *rktio, rktio_ltps_t *lt)
{
  rktio_ltps_handle_t *s;
  s = lt->signaled;
  if (s)
    lt->signaled = s->next;
  else
    set_racket_error(RKTIO_ERROR_LTPS_NOT_FOUND);
  return s;
}

#if defined(HAVE_KQUEUE_SYSCALL) || defined(HAVE_EPOLL_SYSCALL)
static void log_kqueue_error(const char *action, int kr)
{
  if (kr < 0) {
    fprintf(stderr, "%s error at %s: %d",
#ifdef HAVE_KQUEUE_SYSCALL
            "kqueue",
#else
            "epoll",
#endif
            action, errno);
  }
}

static void log_kqueue_fd(int fd, int flags)
{
}
#endif

rktio_ltps_handle_t *rktio_ltps_add(rktio_t *rktio, rktio_ltps_t *lt, rktio_fd_t *rfd, int mode)
{
#ifdef RKTIO_SYSTEM_WINDOWS
  set_racket_error(RKTIO_ERROR_UNSUPPORTED);
  return NULL;
#else
# if !defined(HAVE_KQUEUE_SYSCALL) && !defined(HAVE_EPOLL_SYSCALL)
  rktio_poll_set_t *r, *w, *e;
# endif
  rktio_ltps_handle_pair_t *v;
  rktio_ltps_handle_t *s;
  intptr_t fd = rktio_fd_system_fd(rktio, rfd);

# ifdef HAVE_KQUEUE_SYSCALL
  if (!rktio_fd_is_socket(rktio, rfd)) {
    /* kqueue() might not work on devices, such as ttys; also, while
       Mac OS X kqueue() claims to work on FIFOs, there are problems:
       watching for reads on a FIFO sometimes disables watching for
       writes on the same FIFO with a different file descriptor */
    if (!rktio_fd_is_regular_file(rktio, rfd)) {
      set_racket_error(RKTIO_ERROR_UNSUPPORTED);
      return NULL;
    }
  }
  if (lt->fd < 0) {
    lt->fd = kqueue();
    if (lt->fd < 0) {
      get_posix_error();
      log_kqueue_error("create", lt->fd);
      return NULL;
    }
  }
# endif
# ifdef HAVE_EPOLL_SYSCALL
  if (lt->fd < 0) {
    lt->fd = epoll_create(5);
    if (lt->fd < 0) {
      get_posix_error();
      log_kqueue_error("create", lt->fd);      
      return NULL;
    }
  }
# endif

  v = ltps_hash_get(lt, fd);
  if (!v && ((mode == RKTIO_LTPS_CHECK_READ)
             || (mode == RKTIO_LTPS_CHECK_WRITE)
             || (mode == RKTIO_LTPS_CHECK_VNODE)
             || (mode == RKTIO_LTPS_REMOVE)
             || (mode == RKTIO_LTPS_REMOVE_VNODE))) {
    set_racket_error(RKTIO_ERROR_LTPS_NOT_FOUND);
    return NULL;
  }

  if (!v) {
    v = make_ltps_handle_pair();
    ltps_hash_set(lt, fd, v);
  }

# if !defined(HAVE_KQUEUE_SYSCALL) && !defined(HAVE_EPOLL_SYSCALL)
  r = RKTIO_GET_FDSET(lt->fd_set, 0);
  w = RKTIO_GET_FDSET(lt->fd_set, 1);
  e = RKTIO_GET_FDSET(lt->fd_set, 2);
# endif

  if ((mode == RKTIO_LTPS_REMOVE) || (mode == RKTIO_LTPS_REMOVE_VNODE)) {
    s = v->read_handle;
    if (s) ltps_signal_handle(lt, s);
    s = v->write_handle;
    if (s) ltps_signal_handle(lt, s);
    ltps_hash_remove(lt, fd);
    s = NULL;
# ifdef HAVE_KQUEUE_SYSCALL
    {
      struct kevent kev[2];
      struct timespec timeout = {0, 0};
      int kr, pos = 0;
      if (mode == RKTIO_LTPS_REMOVE_VNODE) {
        EV_SET(&kev[pos], fd, EVFILT_VNODE, EV_DELETE, 0, 0, NULL);
        pos++;
      } else {
        if (v->read_handle) {
          EV_SET(&kev[pos], fd, EVFILT_READ, EV_DELETE, 0, 0, NULL);
          pos++;
        }
        if (v->write_handle) {
          EV_SET(&kev[pos], fd, EVFILT_WRITE, EV_DELETE, 0, 0, NULL);
          pos++;
        }
      }
      do {
        kr = kevent(lt->fd, kev, pos, NULL, 0, &timeout);
      } while ((kr == -1) && (errno == EINTR));
      log_kqueue_error("remove", kr);
    }
# elif defined(HAVE_EPOLL_SYSCALL)
    {
      int kr;
      kr = epoll_ctl(lt->fd, EPOLL_CTL_DEL, fd, NULL);
      log_kqueue_error("remove", kr);
    }
# else
    RKTIO_FD_CLR(fd, r);
    RKTIO_FD_CLR(fd, w);
    RKTIO_FD_CLR(fd, e);
# endif
    free(v);
    set_racket_error(RKTIO_ERROR_LTPS_REMOVED); /* success, not failure */
  } else if ((mode == RKTIO_LTPS_CHECK_READ)
             || (mode == RKTIO_LTPS_CREATE_READ)
             || (mode == RKTIO_LTPS_CHECK_VNODE)
             || (mode == RKTIO_LTPS_CREATE_VNODE)) {
    s = v->read_handle;
    if (!s) {
      if ((mode == RKTIO_LTPS_CREATE_READ)
          || (mode == RKTIO_LTPS_CREATE_VNODE)) {
        s = make_ltps_handle();
        v->read_handle = s;
# ifdef HAVE_KQUEUE_SYSCALL
        {
          struct kevent kev;
          struct timespec timeout = {0, 0};
          int kr;
          if (mode == RKTIO_LTPS_CREATE_READ)
            EV_SET(&kev, fd, EVFILT_READ, EV_ADD | EV_ONESHOT, 0, 0, NULL);
          else
            EV_SET(&kev, fd, EVFILT_VNODE, EV_ADD | EV_ONESHOT, 
                   (NOTE_DELETE | NOTE_WRITE | NOTE_EXTEND 
                    | NOTE_RENAME | NOTE_ATTRIB),
                   0, NULL);
          do {
            kr = kevent(lt->fd, &kev, 1, NULL, 0, &timeout);
          } while ((kr == -1) && (errno == EINTR));
	  log_kqueue_error("read", kr);
        }
# elif defined(HAVE_EPOLL_SYSCALL)
	{
	  struct epoll_event ev;
	  int already = !RKTIO_FALSEP(RKTIO_VEC_ELS(v)[1]), kr;
	  memset(&ev, 0, sizeof(ev));
	  ev.data.fd = fd;
	  ev.events = EPOLLIN | (already ? EPOLLOUT : 0);
	  kr = epoll_ctl(lt->fd, 
			 (already ? EPOLL_CTL_MOD : EPOLL_CTL_ADD), fd, &ev);
	  log_kqueue_error("read", kr);
	}
# else
        RKTIO_FD_SET(fd, r);
        RKTIO_FD_SET(fd, e);
#endif
      } else
        s = NULL;
    }
  } else if ((mode == RKTIO_LTPS_CHECK_WRITE)
             || (mode == RKTIO_LTPS_CREATE_WRITE)) {
    s = v->write_handle;
    if (!s) {
      if (mode == RKTIO_LTPS_CREATE_WRITE) {
        s = make_ltps_handle();
        v->write_handle = s;
# ifdef HAVE_KQUEUE_SYSCALL
        {
          struct kevent kev;
          struct timespec timeout = {0, 0};
          int kr;
          EV_SET(&kev, fd, EVFILT_WRITE, EV_ADD | EV_ONESHOT, 0, 0, NULL);
          do {
            kr = kevent(lt->fd, &kev, 1, NULL, 0, &timeout);
          } while ((kr == -1) && (errno == EINTR));
	  log_kqueue_error("write", kr);
        }

# elif defined(HAVE_EPOLL_SYSCALL)
	{
	  struct epoll_event ev;
	  int already = !RKTIO_FALSEP(RKTIO_VEC_ELS(v)[0]), kr;
	  memset(&ev, 0, sizeof(ev));
	  ev.data.fd = fd;
	  ev.events = EPOLLOUT | (already ? EPOLLIN : 0);
	  kr = epoll_ctl(lt->fd, 
			 (already ? EPOLL_CTL_MOD : EPOLL_CTL_ADD), fd, &ev);
	  log_kqueue_error("write", kr);
	}
# else
        RKTIO_FD_SET(fd, w);
        RKTIO_FD_SET(fd, e);
#endif
      } else
        s = NULL;
    }
  }

  return s;
#endif
}

int rktio_ltps_poll(rktio_t *rktio, rktio_ltps_t *lt)
{
#ifdef RKTIO_SYSTEM_WINDOWS
  return 0;
#elif defined(HAVE_KQUEUE_SYSCALL)
  rktio_ltps_handle_t *s;
  rktio_ltps_handle_pair_t *v;
  int key;
  struct kevent kev;
  struct timespec timeout = {0, 0};
  int kr, hit = 0;

  if (lt->fd < 0)
    return 0;

  while (1) {
    do {
      kr = kevent(lt->fd, NULL, 0, &kev, 1, &timeout);
    } while ((kr == -1) && (errno == EINTR));
    log_kqueue_error("wait", kr);
 
    if (kr > 0) {
      key = kev.ident;
      v = ltps_hash_get(lt, key);
      if (v) {
        if ((kev.filter == EVFILT_READ) || (kev.filter == EVFILT_VNODE)) {
          s = v->read_handle;
          if (s) {
            ltps_signal_handle(lt, s);
            hit = 1;
            v->read_handle = NULL;
          }
        } else if (kev.filter == EVFILT_WRITE) {
          s = v->write_handle;
          if (s) {
            ltps_signal_handle(lt, s);
            hit = 1;
            v->write_handle = NULL;
          }
        }
        if (!v->read_handle && !v->write_handle) {
          ltps_hash_remove(lt, key);
          free(v);
        }
      } else {
        log_kqueue_fd(kev.ident, kev.filter);
      }
    } else
      break;
  }

  return hit;
#elif defined(HAVE_EPOLL_SYSCALL)
  rktio_ltps_handle_t *s;
  rktio_ltps_handle_pair_t *v;
  int key;
  int kr, hit = 0;
  struct epoll_event ev;
  memset(&ev, 0, sizeof(ev));

  if (lt->fd < 0)
    return 0;

  while (1) {
    
    do {
      kr = epoll_wait(lt->fd, &ev, 1, 0);
    } while ((kr == -1) && (errno == EINTR));
    log_kqueue_error("wait", kr);
 
    if (kr > 0) {
      key = ev.data.fd;
      v = ltps_hash_get(lt, key);
      if (v) {
	if (ev.events & (POLLIN | POLLHUP | POLLERR)) {
	  s = v->read_handle;
          if (s) {
            ltps_signal_handle(lt, s);
            hit = 1;
            v->read_handle = NULL;
          }
	}
	if (ev.events & (POLLOUT | POLLHUP | POLLERR)) {
	  s = v->write_handle;
          if (s) {
            ltps_signal_handle(lt, s);
            hit = 1;
            v->write_handle = NULL;
          }
	}
        if (!v->read_handle && !v->write_handle) {
          ltps_hash_remove(lt, key);
          free(v);
          kr = epoll_ctl(lt->fd, EPOLL_CTL_DEL, ev.data.fd, NULL);
          log_kqueue_error("remove*", kr);
        } else {
	  ev.events = ((!v->read_handle ? 0 : POLLIN)
		       | (!v->write_handle ? 0 : POLLOUT));
	  kr = epoll_ctl(lt->fd, EPOLL_CTL_MOD, ev.data.fd, &ev);
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
  rktio_ltps_handle_t *s;
  rktio_ltps_handle_pair_t *v;
  int key;
  int sr, hit = 0;

  if (ltps_is_hash_empty(lt))
    return 0;

  rktio_clean_fd_set(lt->fd_set);
  c = rktio_get_poll_count(lt->fd_set);
  pfd = rktio_get_poll_fd_array(lt->fd_set);

  do {
    sr = poll(pfd, c, 0);
  } while ((sr == -1) && (errno == EINTR));  

  if (sr > 0) {
    for (i = 0; i < c; i++) {
      if (pfd[i].revents) {
        key = pfd[i].fd;
        v = ltps_hash_get(lt, key);
        if (v) {
          if (pfd[i].revents & (POLLIN | POLLHUP | POLLERR)) {
            s = v->read_handle;
            if (s) {
              ltps_signal_handle(lt, s);
              hit = 1;
              v->read_handle = NULL;
            }
            pfd[i].events -= (pfd[i].events & POLLIN);
          }
          if (pfd[i].revents & (POLLOUT | POLLHUP | POLLERR)) {
            s = v->write_handle;
            if (s) {
              ltps_signal_handle(lt, s);
              hit = 1;
              v->write_handle = NULL;
            }
            pfd[i].events -= (pfd[i].events & POLLOUT);
          }
          if (!v->read_handle && !v->write_handle) {
            ltps_hash_remove(lt, key);
            free(v);
          }
        }
      }
    }
  }

  return hit;
#else
  rktio_poll_set_t *fds;
  struct timeval time = {0, 0};
  int i, actual_limit, r, w, e, sr, hit = 0;
  rktio_ltps_handle_t *s;
  rktio_ltps_handle_pair_t *v;
  intptr_t key;

  DECL_FDSET(set, 3);
  rktio_poll_set_t *set1, *set2;

  INIT_DECL_FDSET(set, set1, set2);
  set1 = RKTIO_GET_FDSET(set, 1);
  set2 = RKTIO_GET_FDSET(set, 2);
  
  fds = set;
  RKTIO_FD_ZERO(set);
  RKTIO_FD_ZERO(set1);
  RKTIO_FD_ZERO(set2);

  if (ltps_is_hash_empty(lt))
    return 0;

  rktio_merge_fd_sets(fds, lt->fd_set);

  actual_limit = rktio_get_fd_limit(fds);

  do {
    sr = select(actual_limit, RKTIO_FDS(set), RKTIO_FDS(set1), RKTIO_FDS(set2), &time);
  } while ((sr == -1) && (errno == EINTR));

  if (sr > 0) {
    for (i = 0; i < actual_limit; i++) {
      r = RKTIO_FD_ISSET(i, set);
      w = RKTIO_FD_ISSET(i, set1);
      e = RKTIO_FD_ISSET(i, set2);
      if (r || w || e) {
        key = i;
        v = ltps_hash_get(lt, key);
        if (v) {
          if (r || e) {
            s = v->read_handle;
            if (s) {
              ltps_signal_handle(lt, s);
              hit = 1;
              v->read_handle = NULL;
            }
            RKTIO_FD_CLR(i, RKTIO_GET_FDSET(lt->fd_set, 0));
          }
          if (w || e) {
            s = v->write_handle;
            if (s) {
              ltps_signal_handle(lt, s);
              hit = 1;
              v->write_handle = NULL;
            }
            RKTIO_FD_CLR(i, RKTIO_GET_FDSET(lt->fd_set, 1));
          }
          if (!v->read_handle && !v->write_handle) {
            RKTIO_FD_CLR(i, RKTIO_GET_FDSET(lt->fd_set, 2));
            ltps_hash_remove(lt, key);
            free(v);
          }
        }
      }
    }
  }

  return hit;
#endif
}

/*========================================================================*/

typedef struct ltps_bucket_t {
  /* v is non-NULL => bucket is filled */
  /* v is NULL and fd is -1 => was removed */
  intptr_t fd;
  rktio_ltps_handle_pair_t *v;
} ltps_bucket_t;

static void ltps_rehash(rktio_ltps_t *lt, intptr_t new_size)
{
  if (new_size >= 16) {
    ltps_bucket_t *old_buckets = lt->buckets;
    intptr_t old_size = lt->size, i;

    lt->size = new_size;
    lt->buckets = calloc(new_size, sizeof(ltps_bucket_t));
    lt->count = 0;

    for (i = old_size; --i; ) {
      if (lt->buckets[i].v)
        ltps_hash_set(lt, lt->buckets[i].fd, lt->buckets[i].v);
    }

    free(old_buckets);
  }
}

static rktio_ltps_handle_pair_t *ltps_hash_get(rktio_ltps_t *lt, intptr_t fd)
{
  if (lt->buckets) {
    intptr_t mask = (lt->size - 1);
    intptr_t hc = fd & mask;
    intptr_t d = ((fd >> 3) & mask) | 0x1;

    while (1) {
      if (lt->buckets[hc].fd == fd)
        return lt->buckets[hc].v;
      else if (lt->buckets[hc].v
          || (lt->buckets[hc].fd == -1)) {
        /* keep looking */
        hc = (hc + d) & mask;
      } else
        return NULL;
    }
  } else
    return NULL;
}

static void ltps_hash_remove(rktio_ltps_t *lt, intptr_t fd)
{
  if (lt->buckets) {
    intptr_t mask = (lt->size - 1);
    intptr_t hc = fd & mask;
    intptr_t d = ((fd >> 3) & mask) | 0x1;
    
    while (1) {
      if (lt->buckets[hc].fd == fd) {
        lt->buckets[hc].fd = -1;
        lt->buckets[hc].v = NULL;
        --lt->count;
        if (4 * lt->count <= lt->size)
          ltps_rehash(lt, lt->size >> 1);
      } else if (lt->buckets[hc].v
                 || (lt->buckets[hc].fd == -1)) {
        /* keep looking */
        hc = (hc + d) & mask;
      } else
        break;
    }
  }
}

static void ltps_hash_set(rktio_ltps_t *lt, intptr_t fd, rktio_ltps_handle_pair_t *v)
{
  if (!lt->buckets) {
    lt->size = 16;
    lt->buckets = calloc(lt->size, sizeof(ltps_bucket_t));
  }
  
  {
    intptr_t mask = (lt->size - 1);
    intptr_t hc = fd & mask;
    intptr_t d = ((fd >> 3) & mask) | 0x1;
    
    while (1) {
      if (lt->buckets[hc].v) {
        if (lt->buckets[hc].fd == -1) {
          /* use bucket whos content ws previouslt removed */
          break;
        } else {
          /* keep looking for a spot */
          hc = (hc + d) & mask;
        }
      } else
        break;
    }
    
    lt->buckets[hc].fd = fd;
    lt->buckets[hc].v = v;
    lt->count++;

    if (2 * lt->count >= lt->size)
      ltps_rehash(lt, lt->size << 1);
  }
}

static int ltps_is_hash_empty(rktio_ltps_t *lt)
{
  return (lt->count == 0);
}

static void ltps_hash_init(rktio_ltps_t *lt)
{
  lt->buckets = NULL;
  lt->size = 0;
  lt->count = 0;
}

static void ltps_hash_free(rktio_ltps_t *lt)
{
  if (lt->buckets) {
    intptr_t i;

    for (i = lt->size; --i; ) {
      if (lt->buckets[i].v)
        free(lt->buckets[i].v);
    }
    
    free(lt->buckets);
  }
}
