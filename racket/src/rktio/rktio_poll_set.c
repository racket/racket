#include "rktio.h"
#include "rktio_private.h"
#ifdef RKTIO_SYSTEM_UNIX
# include <sys/select.h>
# include <unistd.h>
# include <fcntl.h>
# include <errno.h>
# include <math.h>
# ifdef USE_ULIMIT
#  include <ulimit.h>
# endif
#endif
#ifdef HAVE_POLL_SYSCALL
# include <poll.h>
#endif
#include <string.h>
#include <stdlib.h>

/*========================================================================*/
/* Poll variant                                                           */
/*========================================================================*/

#ifdef HAVE_POLL_SYSCALL

# define PFD_EXTRA_SPACE 1

struct rktio_poll_set_t {
  struct rktio_fd_set_data_t *data;
  rktio_poll_set_t *w;
  rktio_poll_set_t *e;
  int flags;
};

struct rktio_fd_set_data_t {
  struct pollfd *pfd;
  intptr_t size, count;
  int skip_sleep;
};

static rktio_poll_set_t *alloc_fdset_arrays()
{
  struct rktio_fd_set_data_t *data;
  rktio_poll_set_t *r, *w, *e;
  struct pollfd *pfd;

  data = malloc(sizeof(struct rktio_fd_set_data_t));
  r = malloc(sizeof(struct rktio_poll_set_t));
  w = malloc(sizeof(struct rktio_poll_set_t));
  e = malloc(sizeof(struct rktio_poll_set_t));

  r->w = w;
  r->e = e;
  r->data = data;
  w->data = data;
  e->data = data;

  r->flags = POLLIN;
  w->flags = POLLOUT;
  e->flags = 0;

  data->size = 32;
  data->count = 0;
  data->skip_sleep = 0;

  pfd = malloc(sizeof(struct pollfd) * (32 + PFD_EXTRA_SPACE));
  data->pfd = pfd;

  return r;
}

static void free_fdset_arrays(rktio_poll_set_t *fds)
{
  struct rktio_fd_set_data_t *data = fds->data;
  free(fds->w);
  free(fds->e);
  free(fds);
  free(data->pfd);
  free(data);
}

rktio_poll_set_t *rktio_get_fdset(rktio_poll_set_t *fdarray, int pos)
{
  switch (pos) {
  case 0: 
    return fdarray;
  case 1: 
    return fdarray->w;
  case 2: 
  default:
    return fdarray->e;
  }
}

void rktio_fdzero(rktio_poll_set_t *fd)
{
  fd->data->count = 0;
  fd->data->skip_sleep = 0;
}

static int find_fd_pos(struct rktio_fd_set_data_t *data, intptr_t n)
{
  intptr_t count = data->count;
  intptr_t i;
  
  /* This linear search probably isn't good enough for hundreds or
     thousands of descriptors, but epoll()/kqueue() mode should handle
     that case, anyway. */
  for (i = 0; i < count; i++) {
    if (data->pfd[i].fd == n) {
      return i;
    }
  }

  return -1;
}

void rktio_fdclr(rktio_poll_set_t *fd, intptr_t n)
{
  struct rktio_fd_set_data_t *data = fd->data;
  intptr_t flag = fd->flags;
  intptr_t pos;

  if (!flag) return;

  pos = find_fd_pos(data, n);
  if (pos >= 0) {
    data->pfd[pos].events -= (data->pfd[pos].events & flag);
  }
}

void rktio_fdset(rktio_poll_set_t *fd, intptr_t n)
{
  struct rktio_fd_set_data_t *data = fd->data;
  intptr_t flag = fd->flags;
  intptr_t count, size, pos;
  struct pollfd *pfd;

  if (!flag) return;

  pos = find_fd_pos(data, n);
  if (pos >= 0) {
    data->pfd[pos].events |= flag;
    return;
  }

  count = data->count;
  size = data->size;
  if (count >= size) {
    size = size * 2;
    pfd = malloc(sizeof(struct pollfd) * (size + PFD_EXTRA_SPACE));
    memcpy(pfd, data->pfd, sizeof(struct pollfd) * count);
    free(data->pfd);
    data->pfd = pfd;
    data->size = size;
  }

  data->pfd[count].fd = n;
  data->pfd[count].events = flag;
  count++;
  data->count = count;
}

int rktio_fdisset(rktio_poll_set_t *fd, intptr_t n)
{
  struct rktio_fd_set_data_t *data = fd->data;
  intptr_t flag = fd->flags;
  intptr_t pos;

  if (!flag) flag = (POLLERR | POLLHUP);

  pos = find_fd_pos(data, n);
  if (pos >= 0) {
    if (data->pfd[pos].revents & flag)
      return 1;
    else
      return 0;
  }

  return 0;
}

static int cmp_fd(const void *_a, const void *_b)
{
  struct pollfd *a = (struct pollfd *)_a;
  struct pollfd *b = (struct pollfd *)_b;
  return a->fd - b->fd;
}

void rktio_merge_fd_sets(rktio_poll_set_t *fds, rktio_poll_set_t *src_fds)
{
  struct rktio_fd_set_data_t *data = fds->data;
  struct rktio_fd_set_data_t *src_data = src_fds->data;
  int i, si, c, sc, j, nc;
  struct pollfd *pfds;

  rktio_clean_fd_set(fds);
  rktio_clean_fd_set(src_fds);

  if (src_data->skip_sleep)
    data->skip_sleep = 1;

  c = data->count;
  sc = src_data->count;

  if (!sc)
    return;

  qsort(data->pfd, c, sizeof(struct pollfd), cmp_fd);
  qsort(src_data->pfd, sc, sizeof(struct pollfd), cmp_fd);

  nc = c + sc;
  pfds = malloc(sizeof(struct pollfd) * (nc + PFD_EXTRA_SPACE));
  j = 0;
  for (i = 0, si = 0; (i < c) && (si < sc); ) {
    if (data->pfd[i].fd == src_data->pfd[si].fd) {
      pfds[j].fd = data->pfd[i].fd;
      pfds[j].events = (data->pfd[i].events | src_data->pfd[si].events);
      i++;
      si++;
    } else if (data->pfd[i].fd < src_data->pfd[si].fd) {
      pfds[j].fd = data->pfd[i].fd;
      pfds[j].events = data->pfd[i].events;
      i++;
    } else {
      pfds[j].fd = src_data->pfd[si].fd;
      pfds[j].events = src_data->pfd[si].events;
      si++;
    }
    j++;
  }
  for ( ; i < c; i++, j++) {
    pfds[j].fd = data->pfd[i].fd;
    pfds[j].events = data->pfd[i].events;
  }
  for ( ; si < sc; si++, j++) {
    pfds[j].fd = src_data->pfd[si].fd;
    pfds[j].events = src_data->pfd[si].events;
  }

  if (nc > data->size) {
    free(data->pfd);
    data->pfd = pfds;
    data->size = nc;
  } else {
    memcpy(data->pfd, pfds, j * sizeof(struct pollfd));
    free(pfds);
  }
  data->count = j;
}

void rktio_clean_fd_set(rktio_poll_set_t *fds)
{
  struct rktio_fd_set_data_t *data = fds->data;
  intptr_t count = data->count;
  intptr_t i, j = 0;

  for (i = 0; i < count; i++) {
    if (data->pfd[i].events) {
      if (j < i) {
        data->pfd[j].fd = data->pfd[i].fd;
        data->pfd[j].events = data->pfd[i].events;
      }
      j++;
    }
  }
  
  count = j;
  data->count = count;
}

int rktio_get_fd_limit(rktio_poll_set_t *fds)
{
  return 0;
}

int rktio_get_poll_count(rktio_poll_set_t *fds)
{
  return fds->data->count;
}

struct pollfd *rktio_get_poll_fd_array(rktio_poll_set_t *fds)
{
  return fds->data->pfd;
}

void rktio_poll_set_add_nosleep(rktio_t *rktio, rktio_poll_set_t *fds)
{
  fds->data->skip_sleep = 1;
}

static int fdset_has_nosleep(rktio_poll_set_t *fds)
{
  return fds->data->skip_sleep;
}

#elif defined(USE_DYNAMIC_FDSET_SIZE)

/*========================================================================*/
/* Variant with run-time determined fd_set length                         */
/*========================================================================*/

struct rktio_poll_set_t {
  fd_set data;
};

/* initialized early via rktio_alloc_global_poll_set */
static int dynamic_fd_size;

# define STORED_ACTUAL_FDSET_LIMIT
# define FDSET_LIMIT(fd) (*(int *)((char *)fd + dynamic_fd_size))

static rktio_poll_set_t *alloc_fdset_arrays()
{
  void *p;
  
  if (!dynamic_fd_size) {
# ifdef USE_ULIMIT
    dynamic_fd_size = ulimit(4, 0);
# else
    dynamic_fd_size = getdtablesize();
# endif
    /* divide by bits-per-byte: */
    dynamic_fd_size = (dynamic_fd_size + 7) >> 3;
    /* word-align: */
    if (dynamic_fd_size % sizeof(void*))
      dynamic_fd_size += sizeof(void*) - (dynamic_fd_size % sizeof(void*));
  }

  /* Allocate an array with 1 extra intptr_t in each set to hold a
     "max" fd counter, and 1 extra intger used to record "no
     sleeping" */

  p = malloc(3 * (dynamic_fd_size + sizeof(intptr_t) + sizeof(int)));
  
  return p;
}

static void free_fdset_arrays(rktio_poll_set_t *fds)
{
  free(fds);
}

rktio_poll_set_t *rktio_get_fdset(rktio_poll_set_t *fdarray, int pos)
{
  return (rktio_poll_set_t *)(((char *)fdarray) + (pos * (dynamic_fd_size + sizeof(intptr_t) + sizeof(int))));
}

void rktio_fdzero(rktio_poll_set_t *fd)
{
  memset(fd, 0, dynamic_fd_size + sizeof(intptr_t) + sizeof(int));
}

void rktio_poll_set_add_nosleep(rktio_t *rktio, rktio_poll_set_t *fds)
{
  *(int *)((char *)fds + dynamic_fd_size + sizeof(intptr_t)) = 1;
}

static int fdset_has_nosleep(rktio_poll_set_t *fds)
{
  return *(int *)((char *)fds + dynamic_fd_size + sizeof(intptr_t));
}

/* Continues below: */
#define USE_PLAIN_FDS_SET_OPS

#elif defined (RKTIO_SYSTEM_WINDOWS)

/*========================================================================*/
/* Windows variant                                                        */
/*========================================================================*/

struct  rktio_poll_set_t {
  SOCKET *sockets;

  intptr_t added, alloc, last_alloc;

  intptr_t num_handles, alloc_handles, last_alloc_handles;
  HANDLE *handles;

  int *repost_sema;

  int no_sleep; /* boolean */

  intptr_t wait_event_mask;

  HANDLE *wait_array;

  HANDLE *combined_wait_array;
  intptr_t combined_len;
};

static void reset_wait_array(rktio_poll_set_t *efd);

static void init_fdset_array(rktio_poll_set_t *fdarray, int count)
{
  if (count) {
    int i;
    rktio_poll_set_t *fd;
    for (i = 0; i < count; i++) {
      int reset = 0;
      fd = rktio_get_fdset(fdarray, i);
      fd->added = 0;
      if (fd->alloc > (2 * fd->last_alloc)) {
	fd->alloc = 0;
        if (fd->sockets) free(fd->sockets);
	fd->sockets = NULL;
	reset = 1;
      }
      fd->last_alloc = 0;
      fd->num_handles = 0;
      if (fd->alloc_handles > (2 * fd->last_alloc_handles)) {
	fd->alloc_handles = 0;
        if (fd->handles) free(fd->handles);
        if (fd->repost_sema) free(fd->repost_sema);
	fd->handles = NULL;
	fd->repost_sema = NULL;
	reset = 1;
      }
      fd->last_alloc_handles = 0;
      fd->no_sleep = 0;
      fd->wait_event_mask = 0;
      if (reset)
	reset_wait_array(fdarray);
    }
  }
}

static rktio_poll_set_t *alloc_fdset_arrays()
{
  rktio_poll_set_t *fdarray;

  fdarray = calloc(3, sizeof(rktio_poll_set_t));
  init_fdset_array(fdarray, 3);

  return fdarray;
}

static void free_fdset_arrays(rktio_poll_set_t *fds)
{
  int i;
  
  for (i = 0; i < 3; i++) {
    if (fds[i].handles)
      free(fds[i].handles);
    if (fds[i].repost_sema)
      free(fds[i].repost_sema);
    if (fds[i].wait_array)
      free(fds[i].wait_array);
  }
  free(fds);
}

static void reset_wait_array(rktio_poll_set_t *efd)
{
  /* Allocate an array that may be big enough to hold all events
     when we eventually call WaitForMultipleObjects. One of the three
     arrays will be big enough. */
  int sz = (3 * (efd->alloc + efd->alloc_handles)) + 2;
  HANDLE *wa;
  if (efd->wait_array) free(efd->wait_array);
  wa = calloc(sz, sizeof(HANDLE));
  efd->wait_array = wa;
}

rktio_poll_set_t *rktio_get_fdset(rktio_poll_set_t *fdarray, int pos)
{
  return fdarray + pos;
}

void rktio_fdzero(rktio_poll_set_t *fd)
{
  init_fdset_array(fd, 1);
}

void rktio_fdclr(rktio_poll_set_t *fd, intptr_t n)
{
  intptr_t i;
  for (i = fd->added; i--; ) {
    if (fd->sockets[i] == n)
      fd->sockets[i] = INVALID_SOCKET;
  }
}

static intptr_t next_size(intptr_t v) { return (v ? (2 * v) : 10); }

void rktio_fdset(rktio_poll_set_t *fd, intptr_t n)
{
  if (fd->added >= fd->last_alloc) {
    intptr_t na;
    na = next_size(fd->last_alloc);
    fd->last_alloc = na;
  }
  if (fd->added >= fd->alloc) {
    SOCKET *naya;
    intptr_t na;
    na = next_size(fd->alloc);
    naya = malloc(na * sizeof(SOCKET));
    memcpy(naya, fd->sockets, fd->alloc * sizeof(SOCKET));
    if (fd->sockets) free(fd->sockets);
    fd->sockets = naya;
    fd->alloc = na;
    reset_wait_array(fd);
  }
  fd->sockets[fd->added++] = (SOCKET)n;
}

int rktio_fdisset(rktio_poll_set_t *fd, intptr_t n)
{
  intptr_t i;
  for (i = fd->added; i--; ) {
    if (fd->sockets[i] == (SOCKET)n)
      return 1;
  }
  return 0;
}

void rktio_merge_fd_sets(rktio_poll_set_t *all_fds, rktio_poll_set_t *src_all_fds)
{
  int j;
  intptr_t i;

  for (j = 0; j < 3; j++) {
    rktio_poll_set_t *fds;
    rktio_poll_set_t *src_fds;
    fds = rktio_get_fdset(all_fds, j);
    src_fds = rktio_get_fdset(src_all_fds, j);
    for (i = src_fds->added; i--; ) {
      if (src_fds->sockets[i] != INVALID_SOCKET)
	rktio_fdset(fds, (intptr_t)src_fds->sockets[i]);
    }
    if (src_fds->no_sleep)
      fds->no_sleep = 1;
    fds->wait_event_mask |= src_fds->wait_event_mask;
  }
}

void rktio_clean_fd_set(rktio_poll_set_t *fds)
{
}

int rktio_get_fd_limit(rktio_poll_set_t *fds)
{
  return 0;
}

void rktio_poll_set_add_handle(rktio_t *rktio, intptr_t _h, rktio_poll_set_t *fds, int repost)
{
  HANDLE h = (HANDLE)_h;
  rktio_poll_set_t *efd = fds;
  HANDLE *hs;
  intptr_t i, new_i;
  int *rps;

  if (efd->num_handles == efd->last_alloc_handles) {
    i = next_size(efd->last_alloc_handles);
    efd->last_alloc_handles = 1;
  }
  if (efd->num_handles == efd->alloc_handles) {
    i = efd->alloc_handles;
    new_i = next_size(i);
    hs = malloc(sizeof(HANDLE) * new_i);
    rps = malloc(sizeof(int) * new_i);
    memcpy(hs, efd->handles, sizeof(HANDLE)*i);
    memcpy(rps, efd->repost_sema, sizeof(int)*i);
    if (efd->handles) free(efd->handles);
    if (efd->repost_sema) free(efd->repost_sema);
    efd->handles = hs;
    efd->repost_sema = rps;
    efd->alloc_handles = new_i;
    reset_wait_array(efd);
  }
  i = efd->num_handles;
  efd->handles[i] = h;
  efd->repost_sema[i] = repost;
  efd->num_handles++;
}

void rktio_poll_set_add_nosleep(rktio_t *rktio, rktio_poll_set_t *fds)
{
  fds->no_sleep = 1;
}

static int fdset_has_nosleep(rktio_poll_set_t *fds)
{
  return fds->no_sleep;
}

void rktio_poll_set_add_eventmask(rktio_t *rktio, rktio_poll_set_t *fds, int mask)
{
  fds->wait_event_mask |= mask;
}

static void WSAEventSelect_plus_check(SOCKET s, WSAEVENT e, long mask)
{
  fd_set rd[1], wr[1], ex[1];
  struct timeval t = {0, 0};

  WSAEventSelect(s, e, mask);
  
  /* double-check with select(), because WSAEventSelect only
     handles new activity (I think) */
  FD_ZERO(rd);
  FD_ZERO(wr);
  FD_ZERO(ex);

  if (mask & FD_READ)
    FD_SET(s, rd);
  if (mask & FD_WRITE)
    FD_SET(s, wr);
  if (mask & FD_OOB)
    FD_SET(s, ex);

  if (select(1, rd, wr, ex, &t)) {
    /* already ready */
    WSAEventSelect(s, NULL, 0);
    SetEvent(e);
  }
}

void rktio_collapse_win_fd(rktio_poll_set_t *fds)
{
  rktio_poll_set_t *rfd, *wfd, *efd;
  HANDLE *wa, e;
  intptr_t i, p = 0, mask, j;
  SOCKET s;

  rfd = fds;
  wfd = rktio_get_fdset(fds, 1);
  efd = rktio_get_fdset(fds, 2);

  if (rfd->combined_wait_array) {
    /* clean up */
    for (i = rfd->added; i--; ) {
      if (rfd->sockets[i] != INVALID_SOCKET)
	WSAEventSelect(rfd->sockets[i], NULL, 0);
    }
    for (i = wfd->added; i--; ) {
      if (wfd->sockets[i] != INVALID_SOCKET)
	WSAEventSelect(wfd->sockets[i], NULL, 0);
    }
    for (i = efd->added; i--; ) {
      if (efd->sockets[i] != INVALID_SOCKET)
	WSAEventSelect(efd->sockets[i], NULL, 0);
    }
    p = rfd->num_handles;
    for (i = rfd->combined_len; i-- > p; ) {
      WSACloseEvent(rfd->combined_wait_array[i]);
    }
    rfd->combined_wait_array = NULL;
  } else {
    /* merge */
    if (rfd->alloc < wfd->alloc) {
      if (wfd->alloc < efd->alloc)
	wa = efd->wait_array;
      else
	wa = wfd->wait_array;
    } else {
      if (rfd->alloc < efd->alloc)
	wa = efd->wait_array;
      else
	wa = rfd->wait_array;
    }

    rfd->combined_wait_array = wa;

    p = rfd->num_handles;
    for (i = 0; i < p; i++) {
      wa[i] = rfd->handles[i];
    }
  
    for (i = rfd->added; i--; ) {
      s = rfd->sockets[i];
      if (s != INVALID_SOCKET) {
	mask = FD_READ | FD_ACCEPT | FD_CLOSE;
	
	for (j = wfd->added; j--; ) {
	  if (wfd->sockets[j] == s) {
	    mask |= FD_WRITE;
	    break;
	  }
	}

	for (j = efd->added; j--; ) {
	  if (efd->sockets[j] == s) {
	    mask |= FD_OOB;
	    break;
	  }
	}

	e = WSACreateEvent();
	wa[p++] = e;
	WSAEventSelect_plus_check(s, e, mask);
      }
    }

    for (i = wfd->added; i--; ) {
      s = wfd->sockets[i];
      if (s != INVALID_SOCKET) {
	mask = FD_WRITE | FD_CONNECT | FD_CLOSE;
	
	for (j = rfd->added; j--; ) {
	  if (rfd->sockets[j] == s) {
	    mask = 0;
	    break;
	  }
	}

	if (mask) {
	  for (j = efd->added; j--; ) {
	    if (efd->sockets[j] == s) {
	      mask |= FD_OOB;
	      break;
	    }
	  }
	  
	  e = WSACreateEvent();
	  wa[p++] = e;
	  WSAEventSelect_plus_check(s, e, mask);
	}
      }
    }

    for (i = efd->added; i--; ) {
      s = efd->sockets[i];
      if (s != INVALID_SOCKET) {
	mask = FD_OOB | FD_CLOSE;
	
	for (j = rfd->added; j--; ) {
	  if (rfd->sockets[j] == s) {
	    mask = 0;
	    break;
	  }
	}

	if (mask) {
	  for (j = wfd->added; j--; ) {
	    if (wfd->sockets[j] == s) {
	      mask = 0;
	      break;
	    }
	  }
	  
	  if (mask) {
	    e = WSACreateEvent();
	    wa[p++] = e;
	    WSAEventSelect_plus_check(s, e, mask);
	  }
	}
      }
    }

    rfd->combined_len = p;
  }
}

#else

/*========================================================================*/
/* Plain fd_set variant                                                   */
/*========================================================================*/

static rktio_poll_set_t *alloc_fdset_arrays()
{
  return malloc(3 * sizeof(rktio_poll_set_t));
}

static void free_fdset_arrays(rktio_poll_set_t *fds)
{
  free(fds);
}

rktio_poll_set_t *rktio_get_fdset(rktio_poll_set_t *fdarray, int pos)
{
  return fdarray + pos;
}

void rktio_fdzero(rktio_poll_set_t *fd)
{
  FD_ZERO(&(fd)->data);
  fd->nosleep = 0;
}

void rktio_poll_set_add_nosleep(rktio_t *rktio, rktio_poll_set_t *fds)
{
  fds->nosleep = 1;
}

static int fdset_has_nosleep(rktio_poll_set_t *fds)
{
  return fds->nosleep;
}

#define USE_PLAIN_FDS_SET_OPS

#endif

#ifdef USE_PLAIN_FDS_SET_OPS

void rktio_fdclr(rktio_poll_set_t *fd, intptr_t n)
{
  FD_CLR(n, &(fd)->data);
}

void rktio_fdset(rktio_poll_set_t *fd, intptr_t n)
{
# ifdef STORED_ACTUAL_FDSET_LIMIT
  int mx;
  mx = FDSET_LIMIT(fd);
  if (n > mx)
    FDSET_LIMIT(fd) = n;
# endif
  FD_SET(n, &(fd)->data);
}

int rktio_fdisset(rktio_poll_set_t *fd, intptr_t n)
{
  return FD_ISSET(n, &(fd)->data);
}

void rktio_merge_fd_sets(rktio_poll_set_t *fds, rktio_poll_set_t *src_fds)
{
  int i, j;
  unsigned char *p, *sp;
  for (j = 0; j < 3; j++) {
    p = (unsigned char *)rktio_get_fdset(fds, j);
    sp = (unsigned char *)rktio_get_fdset(src_fds, j);
# ifdef STORED_ACTUAL_FDSET_LIMIT
    i = FDSET_LIMIT(p);
    if (FDSET_LIMIT(sp) > i) {
      i = FDSET_LIMIT(sp);
      FDSET_LIMIT(p) = i;
    }
    /* `i` is max fd, so add 1 to get count, then convert to bytes (rounding up) */
    i = (i + 1 + 7) >> 3;
# elif defined(USE_DYNAMIC_FDSET_SIZE)
    i = dynamic_fd_size;
# else
    i = sizeof(fd_set);
# endif
    for (; i--; p++, sp++) {
      *p |= *sp;
    }
  }
  if (fdset_has_nosleep(src_fds))
    rktio_poll_set_add_nosleep(NULL, fds);
}

void rktio_clean_fd_set(rktio_poll_set_t *fds)
{
}

int rktio_get_fd_limit(rktio_poll_set_t *fds)
{
  int actual_limit;

# ifdef STORED_ACTUAL_FDSET_LIMIT
  {
    fd_set *rd, *wr, *ex;

    rd = RKTIO_FDS(fds);
    wr = RKTIO_FDS(RKTIO_GET_FDSET(fds, 1));
    ex = RKTIO_FDS(RKTIO_GET_FDSET(fds, 2));

    actual_limit = FDSET_LIMIT(rd);
    if (FDSET_LIMIT(wr) > actual_limit)
      actual_limit = FDSET_LIMIT(wr);
    if (FDSET_LIMIT(ex) > actual_limit)
      actual_limit = FDSET_LIMIT(ex);
    actual_limit++;
  }
# elif defined (USE_ULIMIT)
  actual_limit = ulimit(4, 0);
#elif defined(FIXED_FD_LIMIT)
  actual_limit = FIXED_FD_LIMIT;
#else
  actual_limit = getdtablesize();
# endif

  return actual_limit;
}

#endif

#ifndef RKTIO_SYSTEM_WINDOWS
void rktio_poll_set_add_handle(rktio_t *rktio, intptr_t h, rktio_poll_set_t *fds, int repost) { }
void rktio_poll_set_add_eventmask(rktio_t *rktio, rktio_poll_set_t *fds, int mask) { }
void rkio_reset_sleep_backoff(rktio_t *rktio) { }
#endif

/*========================================================================*/
/* Shared internal poll set                                               */
/*========================================================================*/

/* Generalize fd arrays (FD_SET, etc) with a runtime-determined size,
   special hooks for Windows "descriptors" like even queues and
   semaphores, etc. */

void rktio_alloc_global_poll_set(rktio_t *rktio) {
#ifdef USE_FAR_RKTIO_FDCALLS
  rktio->rktio_global_poll_set = alloc_fdset_arrays();
#endif
}

void rktio_free_global_poll_set(rktio_t *rktio) {
#ifdef USE_FAR_RKTIO_FDCALLS
  free_fdset_arrays(rktio->rktio_global_poll_set);
#endif  
}

/*========================================================================*/
/* Create a poll set                                                      */
/*========================================================================*/

/* Internally, poll sets are used with macros like DECL_FDSET(), but this
   is the API for external use. */

rktio_poll_set_t *rktio_make_poll_set(rktio_t *rktio)
{
  rktio_poll_set_t *fds = alloc_fdset_arrays();

  RKTIO_FD_ZERO(fds);
  RKTIO_FD_ZERO(RKTIO_GET_FDSET(fds, 1));
  RKTIO_FD_ZERO(RKTIO_GET_FDSET(fds, 2));

  return fds;
}

void rktio_poll_set_forget(rktio_t *rktio, rktio_poll_set_t *fds)
{
  free_fdset_arrays(fds);
}

/*========================================================================*/
/* Sleeping as a generalized select()                                     */
/*========================================================================*/

int rktio_initialize_signal(rktio_t *rktio)
{
#ifdef RKTIO_SYSTEM_UNIX
  /* Set up a pipe for signaling external events: */
  int fds[2];
  if (!pipe(fds)) {
    rktio->external_event_fd = fds[0];
    rktio->put_external_event_fd = fds[1];
    fcntl(rktio->external_event_fd, F_SETFL, RKTIO_NONBLOCKING);
    fcntl(rktio->put_external_event_fd, F_SETFL, RKTIO_NONBLOCKING);
    return 1;
  } else {
    set_racket_error(RKTIO_ERROR_INIT_FAILED);
    return 0;
  }
#endif

#ifdef RKTIO_SYSTEM_WINDOWS
  rktio->break_semaphore = (void*)CreateSemaphore(NULL, 0, 1, NULL);
  if (rktio->break_semaphore == INVALID_HANDLE_VALUE) {
    get_windows_error();
    return 0;
  } else
    return 1;
#endif
}

rktio_signal_handle_t *rktio_get_signal_handle(rktio_t *rktio)
{
#ifdef RKTIO_SYSTEM_UNIX
  return (rktio_signal_handle_t *)&rktio->put_external_event_fd;
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  return (rktio_signal_handle_t *)&rktio->break_semaphore;
#endif
}

void rktio_signal_received(rktio_t *rktio)
{
  rktio_signal_received_at(rktio_get_signal_handle(rktio));
}

void rktio_signal_received_at(rktio_signal_handle_t *h)
{
#ifdef RKTIO_SYSTEM_UNIX
  int put_ext_event_fd = *(int *)h;
  int saved_errno = errno;
  if (put_ext_event_fd) {
    int v;
    do {
      v = write(put_ext_event_fd, "!", 1);
    } while ((v == -1) && (errno == EINTR));
  }
  errno = saved_errno;
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  ReleaseSemaphore(*(HANDLE *)h, 1, NULL);
#endif
}

void rktio_flush_signals_received(rktio_t *rktio)
{
#ifdef RKTIO_SYSTEM_UNIX
  /* Clear external event flag */
  if (rktio->external_event_fd) {
    int rc;
    char buf[10];
    do {
      rc = read(rktio->external_event_fd, buf, 10);
    } while ((rc == -1) && errno == EINTR);
  }
#endif
}

void rktio_wait_until_signal_received(rktio_t *rktio)
{
#ifdef RKTIO_SYSTEM_UNIX
  int r;
# ifdef HAVE_POLL_SYSCALL
  struct pollfd pfd[1];
  pfd[0].fd = rktio->external_event_fd;
  pfd[0].events = POLLIN;
  do {
    r = poll(pfd, 1, -1);
  } while ((r == -1) && (errno == EINTR));
# else
  DECL_FDSET(readfds, 1);
  
  INIT_DECL_RD_FDSET(readfds);
  
  RKTIO_FD_ZERO(readfds);
  RKTIO_FD_SET(rktio->external_event_fd, readfds);
  
  do {
    r = select(rktio->external_event_fd + 1, RKTIO_FDS(readfds), NULL, NULL, NULL);
  } while ((r == -1) && (errno == EINTR));
# endif
#endif
#ifdef RKTIO_SYSTEM_WINDOWS  
  WaitForSingleObject(rktio->break_semaphore, INFINITE);
#endif

  rktio_flush_signals_received(rktio);
}

/****************** Windows cleanup  *****************/

#ifdef RKTIO_SYSTEM_WINDOWS

static void clean_up_wait(rktio_t *rktio,
                          intptr_t result, HANDLE *array,
			  int *rps, int count)
{
  if ((result >= (intptr_t)WAIT_OBJECT_0) && (result < (intptr_t)WAIT_OBJECT_0 + count)) {
    result -= WAIT_OBJECT_0;
    if (rps[result])
      ReleaseSemaphore(array[result], 1, NULL);
  }

  /* Clear out break semaphore */  
  WaitForSingleObject(rktio->break_semaphore, 0);
}

void rkio_reset_sleep_backoff(rktio_t *rktio)
{
  rktio->made_progress = 1;
}

#endif

/******************** Main sleep function  *****************/
/* The simple select() stuff is buried in various kinds of complexity. */

/* FIXME: don't forget SIGCHILD_DOESNT_INTERRUPT_SELECT handling in Racket */

void rktio_sleep(rktio_t *rktio, float nsecs, rktio_poll_set_t *fds, rktio_ltps_t *lt)
{
  if (fds && fdset_has_nosleep(fds))
    return;

  if (fds && lt) {
#if defined(HAVE_KQUEUE_SYSCALL) || defined(HAVE_EPOLL_SYSCALL)
    int fd = rktio_ltps_get_fd(lt);
    RKTIO_FD_SET(fd, fds);
    RKTIO_FD_SET(fd, RKTIO_GET_FDSET(fds, 2));
#else
    rktio_merge_fd_sets(fds, rktio_ltps_get_fd_set(lt)); 
#endif
  }

  if (!fds) {
    /* Nothing to block on - just sleep for some amount of time. */
#ifdef RKTIO_SYSTEM_UNIX
# ifdef HAVE_POLL_SYSCALL
    int timeout;
    if (nsecs <= 0.0)
      timeout = -1;
    else {
      timeout = (int)(nsecs * 1000.0);
      if (timeout < 0) 
        timeout = 0;
    }
    if (rktio->external_event_fd) {
      struct pollfd pfd[1];
      pfd[0].fd = rktio->external_event_fd;
      pfd[0].events = POLLIN;
      poll(pfd, 1, timeout);
    } else {
      poll(NULL, 0, timeout);
    }
# else
    /* Sleep by selecting on the external event fd */
    struct timeval time;
    intptr_t secs = (intptr_t)nsecs;
    intptr_t usecs = (intptr_t)(fmod(nsecs, 1.0) * 1000000);

    if (nsecs && (nsecs > 100000))
      secs = 100000;
    if (usecs < 0)
      usecs = 0;
    if (usecs >= 1000000)
      usecs = 999999;

    time.tv_sec = secs;
    time.tv_usec = usecs;

    if (rktio->external_event_fd) {
      DECL_FDSET(readfds, 1);

      INIT_DECL_RD_FDSET(readfds);

      RKTIO_FD_ZERO(readfds);
      RKTIO_FD_SET(rktio->external_event_fd, readfds);

      select(rktio->external_event_fd + 1, RKTIO_FDS(readfds), NULL, NULL, &time);
    } else {
      select(0, NULL, NULL, NULL, &time);
    }
# endif
#else
# ifdef RKTIO_SYSTEM_WINDOWS
    Sleep((DWORD)(nsecs * 1000));
# else
#  ifndef NO_SLEEP
#   ifndef NO_USLEEP
    usleep((unsigned)(nsecs * 1000));
#    else
    sleep(nsecs);
#   endif
#  endif
# endif
#endif
  } else {
    /* Something to block on.... */
    
#ifdef HAVE_POLL_SYSCALL

    /******* poll() variant *******/
    
    {
      struct rktio_fd_set_data_t *data = fds->data;
      intptr_t count = data->count;
      int timeout;

      if (nsecs <= 0.0)
        timeout = -1;
      else if (nsecs > 100000)
        timeout = 100000000;
      else {
        timeout = (int)(nsecs * 1000.0);
        if (timeout < 0) 
          timeout = 0;
      }
      
      if (rktio->external_event_fd) {
        data->pfd[count].fd = rktio->external_event_fd;
        data->pfd[count].events = POLLIN;
        count++;
      }

      poll(data->pfd, count, timeout);
    }
#elif !defined(RKTIO_SYSTEM_WINDOWS)

    /******* select() variant *******/
    
    {
      int actual_limit;
      fd_set *rd, *wr, *ex;
      struct timeval time;
      intptr_t secs = (intptr_t)nsecs;
      intptr_t usecs = (intptr_t)(fmod(nsecs, 1.0) * 1000000);

      if (nsecs && (nsecs > 100000))
	secs = 100000;
      if (usecs < 0)
	usecs = 0;
      if (usecs >= 1000000)
	usecs = 999999;

      time.tv_sec = secs;
      time.tv_usec = usecs;

      rd = RKTIO_FDS(fds);
      wr = RKTIO_FDS(RKTIO_GET_FDSET(fds, 1));
      ex = RKTIO_FDS(RKTIO_GET_FDSET(fds, 2));
      
      actual_limit = rktio_get_fd_limit(fds);
      
      /* Watch for external events, too: */
      if (rktio->external_event_fd) {
        FD_SET(rktio->external_event_fd, rd);
        if (rktio->external_event_fd >= actual_limit)
          actual_limit = rktio->external_event_fd + 1;
      }
      
      select(actual_limit, rd, wr, ex, nsecs ? &time : NULL);
    }
    
#else

    /******* Windows variant *******/
    
    {
      intptr_t result;
      HANDLE *array, just_two_array[2];
      intptr_t count, rcount;
      int *rps;

      rktio_collapse_win_fd(fds); /* merges */

      rcount = fds->num_handles;
      count = fds->combined_len;
      array = fds->combined_wait_array;
      rps = fds->repost_sema;

      /* add break semaphore: */
      if (!count)
	array = just_two_array;
      array[count++] = rktio->break_semaphore;

      /* Extensions may handle events.
	 If the event queue is empty (as reported by GetQueueStatus),
	 everything's ok.

	 Otherwise, we have trouble sleeping until an event is ready. We
	 sometimes leave events on th queue because, say, an eventspace is
	 not ready. The problem is that MsgWait... only unblocks when a new
	 event appears. Since extensions may check the queue using a sequence of
	 PeekMessages, it's possible that an event is added during the
	 middle of the sequence, but doesn't get handled.

	 To avoid this problem, we don't actually sleep indefinitely if an event
	 is pending. Instead, we slep 10 ms, then 20 ms, etc. This exponential 
	 backoff ensures that we eventually handle a pending event, but we don't 
	 spin and eat CPU cycles. The back-off is reset whenever a thread makes
	 progress. */

      if (fds->wait_event_mask && GetQueueStatus(fds->wait_event_mask)) {
	if (!rktio->made_progress) {
	  /* Ok, we've gone around at least once. */
	  if (rktio->max_sleep_time < 0x20000000) {
	    rktio->max_sleep_time *= 2;
	  }
	} else {
	  /* Starting back-off mode */
	  rktio->made_progress = 0;
	  rktio->max_sleep_time = 5;
	}
      } else {
	/* Disable back-off mode */
	rktio->made_progress = 1;
	rktio->max_sleep_time = 0;
      }

      /* Wait for HANDLE-based input: */
      {
	DWORD msec;
	if (nsecs) {
	  if (nsecs > 100000)
	    msec = 100000000;
	  else
	    msec = (DWORD)(nsecs * 1000);
	  if (rktio->max_sleep_time && (msec > rktio->max_sleep_time))
	    msec = rktio->max_sleep_time;
	} else {
	  if (rktio->max_sleep_time)
	    msec = rktio->max_sleep_time;
	  else
	    msec = INFINITE;
	}

	result = MsgWaitForMultipleObjects(count, array, FALSE, msec, fds->wait_event_mask);
      }
      clean_up_wait(rktio, result, array, rps, rcount);
      rktio_collapse_win_fd(fds); /* cleans up */

      return;
    }
#endif
  }

  rktio_flush_signals_received(rktio);
}
