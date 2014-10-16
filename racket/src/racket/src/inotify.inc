/* #included by "port.c" */

/* Multiplex multiple filesystem change events onto a single
   inotify connection. That's almost as easy as using watch
   descriptors in place of file descriptors, but using the
   same filesystem path multiple times produces the same
   watch descriptors, so reference-count it. Also, each watch
   can be removed as soon as it fires, since filesystem
   change events are single-shot.

   The values returned by mz_inotify_add() are indices into an array
   of watch descriptors. There's room for a better data structure if
   the watch-descriptor-to-index mapping becomes too slow. */

#ifdef MZ_XFORM
START_XFORM_SUSPEND;
#endif

typedef struct mz_wd_t {
  int wd;
  int refcount;
  int val;
} mz_wd_t;

typedef struct mz_inotify_state_t {
  int ready, errid, fd;
  mz_wd_t *wds;
  int size, count;
  int got;
} mz_inotify_state_t;

static int mzi_find_wd(int wd, mz_wd_t *wds, int size)
{
  int i;
  for (i = 0; i < size; i++) {
    if (wds[i].wd == wd) return i;
  }

  return -1;
}

static int mzi_add_wd(int wd, mz_wd_t *wds, int size)
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

static int mzi_pull_events(int fd, mz_wd_t *wds, int size)
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
      p = mzi_find_wd(ev->wd, wds, size);
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
      } else
	scheme_signal_error("inotify read failed on %d (%e)", fd, errno);
    } else
      break;
  }

  if (b)
    free (b);

  return got;
}

static void mz_inotify_start(mz_inotify_state_t *s)
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

static void mz_inotify_end(mz_inotify_state_t *s)
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

static void mz_inotify_init()
{
  mz_inotify_state_t *s = scheme_inotify_server;

  if (!s) {
    s = (mz_inotify_state_t *)malloc(sizeof(mz_inotify_state_t));
    memset(s, 0, sizeof(mz_inotify_state_t));

    scheme_inotify_server = s;
  }

  if (!s->ready)
    mz_inotify_start(s);
}

static int mz_inotify_ready()
{
  mz_inotify_state_t *s = (mz_inotify_state_t *)scheme_inotify_server;
  
  return s->ready;
}

static int mz_inotify_errid()
{
  mz_inotify_state_t *s = (mz_inotify_state_t *)scheme_inotify_server;

  return s->errid;
}

/* Other functions are called only if mz_inotify_ready() returns 1. */

static int mz_inotify_add(char *filename)
{
  mz_inotify_state_t *s = (mz_inotify_state_t *)scheme_inotify_server;
  int wd;

  if (s->count == s->size) {
    int new_size = (s->size ? (2 * s->size) : 32);
    mz_wd_t *new_wds;
    int i;
    new_wds = (mz_wd_t *)malloc(sizeof(mz_wd_t) * new_size);
    memcpy(new_wds, s->wds, s->size * sizeof(mz_wd_t));
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

    p = mzi_add_wd(wd, s->wds, s->size);
    if (s->wds[p].refcount == 1)
      s->count++;
    
    return p+1;
  }
}

static void mz_inotify_remove(int p2)
{
  mz_inotify_state_t *s = (mz_inotify_state_t *)scheme_inotify_server;
  int p = p2 - 1;

  if (s->wds[p].refcount == 1) {
    if (s->wds[p].wd != -1) {
      inotify_rm_watch(s->fd, s->wds[p].wd);
      s->wds[p].wd = -1;
      /* in case the wd gets reused: */
      if (mzi_pull_events(s->fd, s->wds, s->size))
	s->got = 1;
    }
    --s->count;
  }
  s->wds[p].refcount -= 1;
}

static int mz_inotify_poll(int p2)
{
  mz_inotify_state_t *s = (mz_inotify_state_t *)scheme_inotify_server;
  int p = p2 - 1;

  if (mzi_pull_events(s->fd, s->wds, s->size))
    s->got = 1;
  if (s->wds[p].val)
    return 1;
  else
    return 0;
}

static void mz_inotify_stop()
{
  mz_inotify_state_t *s = (mz_inotify_state_t *)scheme_inotify_server;

  if (s) {
    mz_inotify_end(s);
    scheme_inotify_server = NULL;
  }
}

static int mz_inotify_fd()
{
  mz_inotify_state_t *s = (mz_inotify_state_t *)scheme_inotify_server;

  if (s->got) {
    /* In case we received something for Y in a poll for X */
    s->got = 0;
    return -2;
  }

  return s->fd;
}

#ifdef MZ_XFORM
END_XFORM_SUSPEND;
#endif
