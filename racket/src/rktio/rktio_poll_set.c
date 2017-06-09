#include "rktio.h"
#include "rktio_private.h"
#ifdef RKTIO_SYSTEM_UNIX
# include <sys/select.h>
# include <unistd.h>
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
# include <windows.h>
#endif
#include <stdlib.h>

/* Generalize fd arrays (FD_SET, etc) with a runtime-determined size,
   special hooks for Windows "descriptors" like even queues and
   semaphores, etc. */

#ifdef USE_FAR_RKTIO_FDCALLS
THREAD_LOCAL_DECL(rktio_poll_set_t *rktio_global_poll_set);
#endif

void rktio_alloc_global_poll_set() {
#ifdef USE_FAR_RKTIO_FDCALLS
  rktio_global_poll_set = rktio_alloc_fdset_array(3);
#endif
}

/************************************************************/
/* Poll variant                                             */
/************************************************************/

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
};

rktio_poll_set_t *rktio_alloc_fdset_array(int count)
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

  pfd = malloc(sizeof(struct pollfd) * (32 + PFD_EXTRA_SPACE));
  data->pfd = pfd;

  return data;
}

rktio_poll_set_t *rktio_init_fdset_array(rktio_poll_set_t *fdarray, int count)
{
  ((struct rktio_fd_set *)fdarray)->data->count = 0;
  return fdarray;
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
}

static int find_fd_pos(struct rktio_fd_set_data_t *data, int n)
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

void rktio_fdclr(rktio_poll_set_t *fd, int n)
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

void rktio_fdset(rktio_poll_set_t *fd, int n)
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

int rktio_fdisset(rktio_poll_set_t *fd, int n)
{
  struct rktio_fd_set_data_t *data = data->data;
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

rktio_poll_set_t *rktio_merge_fd_sets(rktio_poll_set_t *fds, rktio_poll_set_t *src_fds)
{
  struct rktio_fd_set_data_t *data = fds->data;
  struct rktio_fd_set_data_t *src_data = src_fds->data;
  int i, si, c, sc, j, nc;
  struct pollfd *pfds;

  rktio_clean_fd_set(fds);
  rktio_clean_fd_set(src_fds);

  c = data->count;
  sc = src_data->count;

  if (!c)
    return src_fds;
  if (!sc)
    return fds;

  qsort(data->pfd, c, sizeof(struct pollfd), cmp_fd);
  qsort(src_data->pfd, sc, sizeof(struct pollfd), cmp_fd);

  nc = c + sc;
  pfds = (struct pollfd *)rktio_malloc_atomic(sizeof(struct pollfd) * (nc + PFD_EXTRA_SPACE));
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

  if (nc > RKTIO_INT_VAL(data->size)) {
    data->pfd = pfds;
    data->size = rktio_make_integer(nc);
  } else
    memcpy(data->pfd, pfds, j * sizeof(struct pollfd));
  data->count = rktio_make_integer(j);

  return fds;
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

#elif defined(USE_DYNAMIC_FDSET_SIZE)

/************************************************************/
/* Variant with run-time determined fd_set length           */
/************************************************************/

struct rktio_poll_set_t {
  fd_set data;
};

/* initialized early via rktio_alloc_global_poll_set */
static int dynamic_fd_size;

# define STORED_ACTUAL_FDSET_LIMIT
# define FDSET_LIMIT(fd) (*(int *)((char *)fd + dynamic_fd_size))

rktio_poll_set_t *rktio_alloc_fdset_array(int count)
{
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

  return malloc(count * (dynamic_fd_size + sizeof(intptr_t)));
}

rktio_poll_set_t *rktio_init_fdset_array(rktio_poll_set_t *fdarray, int count)
{
  return fdarray;
}

rktio_poll_set_t *rktio_get_fdset(rktio_poll_set_t *fdarray, int pos)
{
  return (rktio_poll_set_t *)(((char *)fdarray) + (pos * (dynamic_fd_size + sizeof(intptr_t))));
}

void rktio_fdzero(rktio_poll_set_t *fd)
{
  memset(fd, 0, dynamic_fd_size + sizeof(intptr_t));
}

#elif defined (RKTIO_SYSTEM_WINDOWS)

/************************************************************/
/* Windows variant                                          */
/************************************************************/

typedef struct {
  SOCKET *sockets;

  intptr_t added, alloc, last_alloc;

  intptr_t num_handles, alloc_handles, last_alloc_handles;
  OS_SEMAPHORE_TYPE *handles;

  int *repost_sema;

  int no_sleep; /* boolean */

  intptr_t wait_event_mask;

  HANDLE *wait_array;

  HANDLE *combined_wait_array;
  intptr_t combined_len;
} rktio_poll_set_t;

rktio_poll_set_t *rktio_alloc_fdset_array(int count)
{
  rktio_poll_set_t *fdarray;
  if (count) {
    fdarray = calloc(count, sizeof(rktio_poll_set_t));
    rktio_init_fdset_array(fdarray, count);
  } else
    fdarray = NULL;

  return fdarray;
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

rktio_poll_set_t *rktio_init_fdset_array(rktio_poll_set_t *fdarray, int count)
{
  if (count) {
    int i;
    rktio_poll_set_t *fd;
    for (i = 0; i < count; i++) {
      int reset = 0;
      fd = rktio_get_fdset(fdarray, i);
      fd->added = 0;
      if (RKTIO_INT_VAL(fd->alloc) > (2 * RKTIO_INT_VAL(fd->last_alloc))) {
	fd->alloc = 0;
        if (fd->sockets) free(fd->sockets);
	fd->sockets = NULL;
	reset = 1;
      }
      fd->last_alloc = 0;
      fd->num_handles = 0;
      if (RKTIO_INT_VAL(fd->alloc_handles) > (2 * RKTIO_INT_VAL(fd->last_alloc_handles))) {
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

rktio_poll_set_t *rktio_get_fdset(rktio_poll_set_t *fdarray, int pos)
{
  return fdarray + pos;
}

void rktio_fdzero(rktio_poll_set_t *fd)
{
  rktio_init_fdset_array(fd, 1);
}

void rktio_fdclr(rktio_poll_set_t *fd, int n)
{
  int i;
  for (i = fd->added; i--; ) {
    if (fd->sockets[i] == n)
      fd->sockets[i] = INVALID_SOCKET;
  }
}

static int next_size(int v) { return (v ? (2 * v) : 10); }

void rktio_fdset(rktio_poll_set_t *fd, int n)
{
  if (fd->added >= fd->last_alloc) {
    int na;
    na = next_size(fd->last_alloc);
    efd->last_alloc = na;
  }
  if (fd->added >= fd->alloc) {
    SOCKET *naya;
    int na;
    na = next_size(fd->alloc);
    naya = malloc(na * sizeof(SOCKET));
    memcpy(naya, fd->sockets, RKTIO_INT_VAL(fd->alloc) * sizeof(SOCKET));
    if (fd->sockets) free(fd->sockets);
    fd->sockets = naya;
    fd->alloc = na;
    reset_wait_array(fd);
  }
  fd->sockets[fd->added++] = n;
}

int rktio_fdisset(rktio_poll_set_t *fd, int n)
{
  int i;
  for (i = fd->added; i--; ) {
    if (fd->sockets[i] == n)
      return 1;
  }
  return 0;
}

rktio_poll_set_t *rktio_merge_fd_sets(rktio_poll_set_t *fds, rktio_poll_set_t *src_fds)
{
  int i;
  for (i = src_fd->added; i--; ) {
    if (stv_fd->sockets[i] != INVALID_SOCKET)
      rktio_fdset(fds, src_fd->sockets[i]);
  }
  return fds;
}

void rktio_clean_fd_set(rktio_poll_set_t *fds)
{
}

int rktio_get_fd_limit(rktio_poll_set_t *fds)
{
  return 0;
}

void rktio_fdset_add_handle(HANDLE h, rktio_poll_set_t *fds, int repost)
{
  rktio_poll_set_t *efd = fds;
  OS_SEMAPHORE_TYPE *hs;
  int i, new_i, *rps;

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

void rktio_add_fd_nosleep(rktio_poll_set_t *fds)
{
  fds->no_sleep = 1;
}

void rktio_fdset_add_eventmask(rktio_poll_set_t *fds, int mask)
{
  fds->wait_event_mask |= mask;
}

void WSAEventSelect_plus_check(SOCKET s, WSAEVENT e, long mask)
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
  int i, p = 0, mask, j;
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
    if (rfd->alloc < RKTIO_INT_VAL(wfd->alloc)) {
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

/************************************************************/
/* Plain fd_set variant                                     */
/************************************************************/

rktio_poll_set_t *rktio_alloc_fdset_array(int count)
{
  return malloc(count * sizeof(fd_set));
}

rktio_poll_set_t *rktio_init_fdset_array(rktio_poll_set_t *fdarray, int count)
{
  return fdarray;
}

rktio_poll_set_t *rktio_get_fdset(rktio_poll_set_t *fdarray, int pos)
{
  return fdarray + pos;
}

void rktio_fdzero(rktio_poll_set_t *fd)
{
  FD_ZERO(&(fd)->data);
}

void rktio_fdclr(rktio_poll_set_t *fd, int n)
{
  FD_CLR(n, &(fd)->data);
}

void rktio_fdset(rktio_poll_set_t *fd, int n)
{
# ifdef STORED_ACTUAL_FDSET_LIMIT
  int mx;
  mx = FDSET_LIMIT(fd);
  if (n > mx)
    FDSET_LIMIT(fd) = n;
# endif
  FD_SET(n, &(fd)->data);
}

int rktio_fdisset(rktio_poll_set_t *fd, int n)
{
  return FD_ISSET(n, &(fd)->data);
}

rktio_poll_set_t *rktio_merge_fd_sets(rktio_poll_set_t *fds, rktio_poll_set_t *src_fds)
{
  int i, j;
  unsigned char *p, *sp;
  for (j = 0; j < 3; j++) {
    p = (unsigned char *)rktio_get_fdset(fds, j);
    sp = (unsigned char *)rktio_get_fdset(src_fds, j);
# ifdef STORED_ACTUAL_FDSET_LIMIT
    if (FDSET_LIMIT(sp) > FDSET_LIMIT(p)) {
      i = FDSET_LIMIT(sp);
      FDSET_LIMIT(p) = i;
    }
# endif
# if defined(USE_DYNAMIC_FDSET_SIZE)
    i = dynamic_fd_size;
# else
    i = sizeof(fd_set);
# endif
    for (; i--; p++, sp++) {
      *p |= *sp;
    }
  }
  return fds;
}

void rktio_clean_fd_set(rktio_poll_set_t *fds)
{
}

int rktio_get_fd_limit(rktio_poll_set_t *fds)
{
  int actual_limit;

# ifdef STORED_ACTUAL_FDSET_LIMIT
  actual_limit = FDSET_LIMIT(rd);
  if (FDSET_LIMIT(wr) > actual_limit)
    actual_limit = FDSET_LIMIT(wr);
  if (FDSET_LIMIT(ex) > actual_limit)
    actual_limit = FDSET_LIMIT(ex);
  actual_limit++;
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
