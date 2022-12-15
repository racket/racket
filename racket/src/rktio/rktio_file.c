#include "rktio.h"
#include "rktio_private.h"
#include <errno.h>
#include <stdlib.h>
#ifdef RKTIO_SYSTEM_UNIX
# include <sys/stat.h>
# include <fcntl.h>
# include <unistd.h>
# include <sys/select.h>
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
# include <windows.h>
#endif
#ifdef RKTIO_USE_PENDING_OPEN
#include <string.h>
#endif

#ifdef RKTIO_SYSTEM_UNIX
static rktio_fd_t *finish_unix_fd_creation(rktio_t *rktio, int fd, int modes, rktio_fd_t *existing_rfd,
                                           int perm_bits, int replace_perms);
#endif

#ifdef RKTIO_USE_PENDING_OPEN
static rktio_fd_t *open_via_thread(rktio_t *rktio, const char *filename, int modes, int flags,
                                   int perm_bits, int replace_perms);
static int do_pending_open_release(rktio_t *rktio, struct open_in_thread_t *data, int close_fd);
#endif

/*========================================================================*/
/* Opening a file                                                         */
/*========================================================================*/

static rktio_fd_t *open_read(rktio_t *rktio, const char *filename, int modes)
{
#ifdef RKTIO_SYSTEM_UNIX
  int fd;
  struct stat buf;

  do {
    fd = open(filename, O_RDONLY | RKTIO_NONBLOCKING | RKTIO_BINARY);
  } while ((fd == -1) && (errno == EINTR));

  if (fd == -1) {
    if (errno == ENOENT) {
      set_racket_error(RKTIO_ERROR_DOES_NOT_EXIST);
    } else
      get_posix_error();
    return NULL;
  } else {
    int cr;

    do {
      cr = fstat(fd, &buf);
    } while ((cr == -1) && (errno == EINTR));

    if (cr) {
      get_posix_error();
      do {
	cr = close(fd);
      } while ((cr == -1) && (errno == EINTR));
      return NULL;
    }

    if (S_ISDIR(buf.st_mode)) {
      do {
	cr = close(fd);
      } while ((cr == -1) && (errno == EINTR));
      set_racket_error(RKTIO_ERROR_IS_A_DIRECTORY);
      return NULL;
    } else {
      return rktio_system_fd(rktio, fd, (RKTIO_OPEN_READ
                                         | (S_ISREG(buf.st_mode)
                                            ? RKTIO_OPEN_REGFILE
                                            : RKTIO_OPEN_NOT_REGFILE)));
    }
  }
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  HANDLE fd;
  rktio_fd_t *rfd;
  wchar_t *wp;

  wp = WIDE_PATH_temp(filename);
  if (!wp)
    return NULL;
  
  fd = CreateFileW(wp,
		   GENERIC_READ,
		   FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
		   NULL,
		   OPEN_EXISTING,
		   0,
		   NULL);

  if (fd == INVALID_HANDLE_VALUE) {
    if (GetLastError() == ERROR_FILE_NOT_FOUND) {
      set_racket_error(RKTIO_ERROR_DOES_NOT_EXIST);
    } else
      get_windows_error();
    return NULL;
  }

  rfd = rktio_system_fd(rktio, (intptr_t)fd, (RKTIO_OPEN_READ | RKTIO_OPEN_NOT_DIR
					      | (modes & RKTIO_OPEN_TEXT)));

  if (modes & RKTIO_OPEN_TEXT) {
    if (!rktio_fd_is_regular_file(rktio, rfd)) {
      rktio_forget(rktio, rfd);
      set_racket_error(RKTIO_ERROR_UNSUPPORTED_TEXT_MODE);
      return NULL;
    }
  }

  return rfd;
#endif
}

static rktio_fd_t *open_write(rktio_t *rktio, const char *filename, int modes, int perm_bits)
{
#ifdef RKTIO_SYSTEM_UNIX
  int fd;
  int flags;

  flags = (((modes & RKTIO_OPEN_READ) ? O_RDWR : O_WRONLY)
           | ((modes & RKTIO_OPEN_MUST_EXIST ? 0 : O_CREAT)));

  if (modes & RKTIO_OPEN_APPEND)
    flags |= O_APPEND;
  else if (modes & RKTIO_OPEN_TRUNCATE)
    flags |= O_TRUNC;
  else if (!(modes & RKTIO_OPEN_CAN_EXIST))
    flags |= O_EXCL;

  do {
    fd = open(filename, flags | RKTIO_NONBLOCKING | RKTIO_BINARY, perm_bits);
  } while ((fd == -1) && (errno == EINTR));

  if (errno == ENXIO) {
    /* FIFO with no reader? */
#ifdef RKTIO_USE_PENDING_OPEN
    return open_via_thread(rktio, filename, modes, flags | RKTIO_BINARY,
                           perm_bits, modes & RKTIO_OPEN_REPLACE_PERMS);
#else
    /* Try opening in RW mode: */
    flags -= O_WRONLY;
    flags |= O_RDWR;
    do {
      fd = open(filename, flags | RKTIO_NONBLOCKING | RKTIO_BINARY, perm_bits);
    } while ((fd == -1) && (errno == EINTR));
#endif
  }

  return finish_unix_fd_creation(rktio, fd, modes, NULL, perm_bits, modes & RKTIO_OPEN_REPLACE_PERMS);
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  HANDLE fd;
  int hmode;
  rktio_fd_t *rfd;
  wchar_t *wp;

  if (modes & RKTIO_OPEN_MUST_EXIST) {
    if (modes & RKTIO_OPEN_TRUNCATE)
      hmode = TRUNCATE_EXISTING;
    else
      hmode = OPEN_EXISTING;
  } else if (modes & (RKTIO_OPEN_CAN_EXIST | RKTIO_OPEN_APPEND))
    hmode = OPEN_ALWAYS;
  else
    hmode = CREATE_NEW;

  wp = WIDE_PATH_temp(filename);
  if (!wp) return NULL;
    
  fd = CreateFileW(wp,
		   GENERIC_WRITE | ((modes & RKTIO_OPEN_READ) ? GENERIC_READ : 0),
		   FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
		   NULL,
		   hmode,
		   (FILE_FLAG_BACKUP_SEMANTICS /* lets us detect directories in NT */
                    | ((perm_bits & RKTIO_PERMISSION_WRITE)
                       ? 0
                       : FILE_ATTRIBUTE_READONLY)),
		   NULL);

  if (fd == INVALID_HANDLE_VALUE) {
    int errv = GetLastError();
    if (errv == ERROR_ACCESS_DENIED) {
      set_racket_error(RKTIO_ERROR_ACCESS_DENIED);
      return NULL;
    } else if (errv == ERROR_FILE_EXISTS) {
      set_racket_error(RKTIO_ERROR_EXISTS);
      return NULL;
    } else {
      get_windows_error();
      return NULL;
    }
  }

  if (modes & RKTIO_OPEN_REPLACE_PERMS) {
    FILE_BASIC_INFO info;
    if (GetFileInformationByHandleEx(fd, FileBasicInfo, &info, sizeof(info))) {
      DWORD attr = ((perm_bits & RKTIO_PERMISSION_WRITE) ? 0 : FILE_ATTRIBUTE_READONLY);
      if ((info.FileAttributes & FILE_ATTRIBUTE_READONLY) != attr) {
        if (attr)
          info.FileAttributes |= attr;
        else
          info.FileAttributes -= attr;
        if (!SetFileInformationByHandle(fd, FileBasicInfo, &info, sizeof(info))) {
          get_windows_error();
          CloseHandle(fd);
          return NULL;
        }
      }
    } else {
      get_windows_error();
      CloseHandle(fd);
      return NULL;
    }
  }

  rfd = rktio_system_fd(rktio, (intptr_t)fd, modes);

  if (rktio_fd_is_directory(rktio, rfd)) {
    rktio_close(rktio, rfd);
    set_racket_error(RKTIO_ERROR_IS_A_DIRECTORY);
    return NULL;
  }

  if (modes & RKTIO_OPEN_TEXT) {
    if (!rktio_fd_is_regular_file(rktio, rfd)) {
      rktio_forget(rktio, rfd);
      set_racket_error(RKTIO_ERROR_UNSUPPORTED_TEXT_MODE);
      return NULL;
    }
  }

  if ((modes & (RKTIO_OPEN_APPEND |RKTIO_OPEN_TRUNCATE))
      && rktio_fd_is_regular_file(rktio, rfd)) {
    if (modes & RKTIO_OPEN_APPEND)
      SetFilePointer(fd, 0, NULL, FILE_END);
    else
      SetEndOfFile(fd);
  }

  return rfd;
#endif
}

#ifdef RKTIO_SYSTEM_UNIX
static rktio_fd_t *finish_unix_fd_creation(rktio_t *rktio, int fd, int modes, rktio_fd_t *existing_rfd,
                                           int perm_bits, int replace_perms)
{
  struct stat buf;
  int cr;

  if (fd == -1) {
    if (errno == EISDIR) {
      set_racket_error(RKTIO_ERROR_IS_A_DIRECTORY);
      return NULL;
    } else if (errno == EEXIST) {
      set_racket_error(RKTIO_ERROR_EXISTS);
      return NULL;
    } else if (errno == EACCES) {
      set_racket_error(RKTIO_ERROR_ACCESS_DENIED);
      return NULL;
    }

    if (fd == -1) {
      get_posix_error();
      return NULL;
    }
  }

  if (replace_perms) {
    do {
      cr = fchmod(fd, perm_bits);
    } while ((cr == -1) && (errno == EINTR));
    if (cr == -1) {
      get_posix_error();
      do {
        cr = close(fd);
      } while ((cr == -1) && (errno == EINTR));
      return NULL;
    }
  }

  do {
    cr = fstat(fd, &buf);
  } while ((cr == -1) && (errno == EINTR));

  if (cr) {
    get_posix_error();
    do {
      cr = close(fd);
    } while ((cr == -1) && (errno == EINTR));
    return NULL;
  }

  modes |= (S_ISREG(buf.st_mode)
            ? RKTIO_OPEN_REGFILE
            : RKTIO_OPEN_NOT_REGFILE);

# ifdef RKTIO_USE_PENDING_OPEN
  if (existing_rfd) {
    rktio_update_system_fd(rktio, existing_rfd, fd, modes);
    return existing_rfd;
  } else
# endif
    return rktio_system_fd(rktio, fd, modes);
}
#endif

rktio_fd_t *rktio_open_with_create_permissions(rktio_t *rktio, const char *filename, int modes, int perm_bits)
{
  if (modes & RKTIO_OPEN_WRITE)
    return open_write(rktio, filename, modes, perm_bits);
  else
    return open_read(rktio, filename, modes);
}

rktio_fd_t *rktio_open(rktio_t *rktio, const char *filename, int modes)
{
  return rktio_open_with_create_permissions(rktio, filename, modes, RKTIO_DEFAULT_PERM_BITS);
}

/*========================================================================*/
/* File positions                                                         */
/*========================================================================*/

#ifdef RKTIO_SYSTEM_WINDOWS
static int win_seekable(intptr_t fd)
{
  /* SetFilePointer() requires "a file stored on a seeking device".
     I'm not sure how to test that, so we approximate as "regular
     file". */
  return GetFileType((HANDLE)fd) == FILE_TYPE_DISK;
}
#endif

rktio_ok_t rktio_set_file_position(rktio_t *rktio, rktio_fd_t *rfd, rktio_filesize_t pos, int whence)
{
  intptr_t fd = rktio_fd_system_fd(rktio, rfd);
  
#ifdef RKTIO_SYSTEM_UNIX
  if (whence == RKTIO_POSITION_FROM_START)
    whence = SEEK_SET;
  else
    whence = SEEK_END;
  if (BIG_OFF_T_IZE(lseek)(fd, pos, whence) < 0) {
    get_posix_error();
    return 0;
  }
  return 1;
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (win_seekable(fd)) {
    DWORD r;
    LONG lo_w, hi_w;
    lo_w = (LONG)(pos & 0xFFFFFFFF);
    hi_w = (LONG)(pos >> 32);
    r = SetFilePointer((HANDLE)fd, lo_w, &hi_w,
                       ((whence == RKTIO_POSITION_FROM_START) ? FILE_BEGIN : FILE_END));
    if ((r == INVALID_SET_FILE_POINTER)
        && GetLastError() != NO_ERROR) {
      get_windows_error();
      return 0;
    } else
      return 1;
  } else {
    set_racket_error(RKTIO_ERROR_CANNOT_FILE_POSITION);
    return 0;
  }
#endif
}

rktio_filesize_t *rktio_get_file_position(rktio_t *rktio, rktio_fd_t *rfd)
{
  intptr_t fd = rktio_fd_system_fd(rktio, rfd);
  rktio_filesize_t pll, *r;

#ifdef RKTIO_SYSTEM_UNIX
  pll = BIG_OFF_T_IZE(lseek)(fd, 0, 1);
  if (pll < 0) {
    get_posix_error();
    return NULL;
  }
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (win_seekable(fd)) {
    DWORD lo_w;
    LONG hi_w;
    hi_w = 0;
    lo_w = SetFilePointer((HANDLE)fd, 0, &hi_w, FILE_CURRENT);
    if ((lo_w == INVALID_SET_FILE_POINTER)
        && GetLastError() != NO_ERROR) {
      get_windows_error();
      return NULL;
    } else
      pll = ((rktio_int64_t)hi_w << 32) | lo_w;
  } else {
    set_racket_error(RKTIO_ERROR_CANNOT_FILE_POSITION);
    return NULL;
  }
#endif

  r = malloc(sizeof(rktio_filesize_t));
  *r = pll;
  return r;
}

rktio_ok_t rktio_set_file_size(rktio_t *rktio, rktio_fd_t *rfd, rktio_filesize_t sz)
{
  intptr_t fd = rktio_fd_system_fd(rktio, rfd);

#ifdef RKTIO_SYSTEM_UNIX
  if (!BIG_OFF_T_IZE(ftruncate)(fd, sz))
    return 1;
  get_posix_error();
  return 0;
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (win_seekable(fd)) {
    DWORD r;
    LONG lo_w, hi_w, old_lo_w, old_hi_w;
    old_hi_w = 0;
    old_lo_w = SetFilePointer((HANDLE)fd, 0, &old_hi_w, FILE_CURRENT);
    if ((old_lo_w == INVALID_SET_FILE_POINTER)
        && GetLastError() != NO_ERROR) {
      get_windows_error();
      return 0;
    } else {
      lo_w = (LONG)(sz & 0xFFFFFFFF);
      hi_w = (LONG)(sz >> 32);
      r = SetFilePointer((HANDLE)fd, lo_w, &hi_w, FILE_BEGIN);
      if ((r == INVALID_SET_FILE_POINTER)
	  && GetLastError() != NO_ERROR) {
        get_windows_error();
        return 0;
      } else {
	if (SetEndOfFile((HANDLE)fd)) {
	  /* we assume that this works: */
	  (void)SetFilePointer((HANDLE)fd, old_lo_w, &old_hi_w, FILE_BEGIN);
	  return 1;
	}
        get_windows_error();
        return 0;
      }
    }
  } else {
    set_racket_error(RKTIO_ERROR_CANNOT_FILE_POSITION);
    return 0;
  }
#endif
}

/*========================================================================*/
/* Thread for blocking open                                               */
/*========================================================================*/

/* When opening a fifo for writing, then there's no way to open in
   non-blocking mode and then poll for whether reader is ready.
   Instead, we have to open in a separate thread. When the open
   succeeds, post to a waiting rktio_t's signal handle.

   To allow a blocked-on-opening file descriptor to be transferred
   across rktio_t domains, the record that represents the extra thread
   must be sharable among multiple OS threads, and we must in general
   keep an array of handles.

   If the blocked-omn-opening file descriptor is closed, then we have
   to be able to cancel the thread. This isn't so bad, since we want
   to support canceling only while the `open` system call is
   blocked. */

#ifdef RKTIO_USE_PENDING_OPEN

/* An instance of `open_in_thread_t` can be shared by multiple threads
   (i.e., multiple `rktio_t` instances) */
typedef struct open_in_thread_t {
  pthread_mutex_t lock;
  int ready;
  pthread_cond_t ready_cond; /* wait until helper thread is ready (including cleanup) */
  char *filename;
  int flags;
  int perm_bits;
  int replace_perms;
  int done;
  int fd;
  int errval;
  int refcount;
  pthread_t th;
  int num_handles;
  rktio_signal_handle_t **handles;
} open_in_thread_t;


static void free_open_in_thread(open_in_thread_t *data)
{
  pthread_detach(data->th);
  if (data->handles)
    free(data->handles);
  free(data->filename);
  free(data);
}

static void cleanup_open_thread(void *_data)
{
  int i, refcount;
  open_in_thread_t *data = (open_in_thread_t *)_data;

  pthread_mutex_lock(&data->lock);
  for (i = 0; i < data->num_handles; i++)
    if (data->handles[i])
      rktio_signal_received_at(data->handles[i]);
  refcount = data->refcount;
  data->done = 1;
  pthread_mutex_unlock(&data->lock);

  if (!refcount) {
    if (data->fd != -1)
      rktio_reliably_close(data->fd);
    free_open_in_thread(data);
  }
}

static void *do_open_in_thread(void *_data)
{
  open_in_thread_t *data = (open_in_thread_t *)_data;
  int fd;
  int old_type;

  /* To be on the safe side, disable cancelation except 
     just around the call to `open` */
  pthread_setcanceltype(PTHREAD_CANCEL_DISABLE, &old_type);
  pthread_cleanup_push(cleanup_open_thread, data);

  pthread_mutex_lock(&data->lock);
  data->ready = 1;
  pthread_cond_signal(&data->ready_cond);
  pthread_mutex_unlock(&data->lock);

  data->fd = -1;

  pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, NULL);

  /* Cancelation only possible during `open`, a which point it's ok
     and sufficient to run `cleanup_open_thread`; we're assuming that
     either `open` returns with a file descriptor or the thread is
     canceled before `open` returns, but not both (otherwise there
     could be a space leak) */
  do {
    fd = open(data->filename, data->flags, data->perm_bits);
  } while ((fd == -1) && (errno == EINTR));

  pthread_setcanceltype(PTHREAD_CANCEL_DISABLE, NULL);

  data->fd = fd;
  if (fd == -1)
    data->errval = errno;

  /* Runs `cleanup_open_thread` while popping it: */
  pthread_cleanup_pop(1);

  /* In general, a function tha changes the cancelation type should
     restore it before returning */
  pthread_setcanceltype(old_type, NULL);

  return NULL;
}

static rktio_fd_t *open_via_thread(rktio_t *rktio, const char *filename, int modes, int flags,
                                   int perm_bits, int replace_perms)
{
  open_in_thread_t *data;

  data = calloc(1, sizeof(open_in_thread_t));

  data->refcount = 1;

  data->filename = strdup(filename);
  data->flags = flags;
  data->perm_bits = perm_bits;
  data->replace_perms = replace_perms;
  pthread_mutex_init(&data->lock, NULL);
  pthread_cond_init(&data->ready_cond, NULL);

  data->num_handles = 1;
  data->handles = malloc(sizeof(rktio_signal_handle_t*));
  data->handles[0] = rktio_get_signal_handle(rktio);

  (void)pthread_create(&data->th, NULL, do_open_in_thread, data);

  pthread_mutex_lock(&data->lock);
  if (!data->ready)
    pthread_cond_wait(&data->ready_cond, &data->lock);
  pthread_mutex_unlock(&data->lock);

  return rktio_pending_system_fd(rktio, data, modes);
}

int rktio_pending_open_poll(rktio_t *rktio, rktio_fd_t *existing_rfd, struct open_in_thread_t *data)
/* non-zero result is an errno value */
{
  int done;

  pthread_mutex_lock(&data->lock);
  done = data->done;
  pthread_mutex_unlock(&data->lock);

  if (done) {
    if (data->fd == -1)
      return data->errval;
    else {
      int fd = data->fd;
      int perm_bits = data->perm_bits;
      int replace_perms = data->replace_perms;
      (void)do_pending_open_release(rktio, data, 0);
      if (!finish_unix_fd_creation(rktio, fd, 0, existing_rfd, perm_bits, replace_perms)) {
        /* Posix error must be saved in `rktio` */
        return rktio->errid;
      }
      return 0;
    }
  } else
    return 0;
}

void rktio_poll_add_pending_open(rktio_t *rktio, rktio_fd_t *rfd, struct open_in_thread_t *data, rktio_poll_set_t *fds)
{
  int done;

  pthread_mutex_lock(&data->lock);
  done = data->done;
  pthread_mutex_unlock(&data->lock);

  if (done)
    rktio_poll_set_add_nosleep(rktio, fds);
}

void rktio_pending_open_attach(rktio_t *rktio, struct open_in_thread_t *data)
{
  int i;
  rktio_signal_handle_t *h = rktio_get_signal_handle(rktio);

  pthread_mutex_lock(&data->lock);
  if (!data->done) {
    for (i = 0; i < data->num_handles; i++)
      if (!data->handles[i]) {
        data->handles[i] = h;
        break;
      }
    if (i >= data->num_handles) {
      rktio_signal_handle_t **old = data->handles;
      int n = (2 * data->num_handles);
      data->handles = calloc(n, sizeof(rktio_signal_handle_t*));
      memcpy(data->handles, old, data->num_handles * sizeof(rktio_signal_handle_t*));
      data->handles[data->num_handles] = h;
      data->num_handles = n;
    }
  }
  pthread_mutex_unlock(&data->lock);
}

static void do_detach(rktio_t *rktio, struct open_in_thread_t *data)
{
  int i;
  rktio_signal_handle_t *h = rktio_get_signal_handle(rktio);

  for (i = 0; i < data->num_handles; i++)
    if (data->handles[i] == h)
      data->handles[i] = NULL;
}

void rktio_pending_open_detach(rktio_t *rktio, struct open_in_thread_t *data)
{
  pthread_mutex_lock(&data->lock);
  do_detach(rktio, data);
  pthread_mutex_unlock(&data->lock);
}


void rktio_pending_open_retain(rktio_t *rktio, struct open_in_thread_t *data)
{
  pthread_mutex_lock(&data->lock);
  data->refcount++;
  pthread_mutex_unlock(&data->lock);
}

int do_pending_open_release(rktio_t *rktio, struct open_in_thread_t *data, int close_fd)
/* The rktio argument can be NULL for a detached use */
{
  int bye;

  pthread_mutex_lock(&data->lock);
  --data->refcount;
  bye = (data->done && !data->refcount);
  if (!bye) {
    do_detach(rktio, data);
    if (!data->refcount)
      pthread_cancel(data->th);
  }
  pthread_mutex_unlock(&data->lock);

  if (bye) {
    if (close_fd && (data->fd != -1))
      rktio_reliably_close(data->fd);
    free_open_in_thread(data);
  }

  return 0;
}

int rktio_pending_open_release(rktio_t *rktio, struct open_in_thread_t *data)
{
  return do_pending_open_release(rktio, data, 1);
}

#endif
