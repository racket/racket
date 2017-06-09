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

#ifndef RKTIO_BINARY
# define RKTIO_BINARY 0
#endif

struct rktio_fd_t {
  int modes;

#ifdef RKTIO_SYSTEM_UNIX
  intptr_t fd;
# ifdef SOME_FDS_ARE_NOT_SELECTABLE
  int bufcount;
  char buffer[1];
# endif
#endif
  
#ifdef RKTIO_SYSTEM_WINDOWS
  HANDLE fd;
  Win_FD_Input_Thread *th; /* input mode */
  Win_FD_Output_Thread *oth; /* output mode */
#endif

  int regfile;
};

/*************************************************************/
/* creating an fd                                            */
/*************************************************************/

static void init_read_fd(rktio_fd_t *rfd)
{
#ifdef RKTIO_SYSTEM_UNIX
# ifdef SOME_FDS_ARE_NOT_SELECTABLE
  rfd->bufcount = 0;
# endif
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (!rfd->regfile) {
    /* To get non-blocking I/O for anything that can block, we create
       a separate reader thread.

       Yes, Windows NT pipes support non-blocking reads, but there
       doesn't seem to be any way to use WaitForSingleObject to sleep
       until characters are ready. PeekNamedPipe can be used for
       polling, but not sleeping. */

    Win_FD_Input_Thread *th;
    DWORD id;
    HANDLE h;
    OS_SEMAPHORE_TYPE sm;

    th = malloc(sizeof(Win_FD_Input_Thread));
    rfd->th = th;

    /* Replace buffer with a malloced one: */
    bfr = malloc(MZPORT_FD_BUFFSIZE);
    fip->buffer = bfr;
    th->buffer = bfr;

    th->fd = (HANDLE)fd;
    th->avail = 0;
    th->err = 0;
    th->eof = NULL;
    th->checking = 0;
    
    sm = CreateSemaphore(NULL, 0, 1, NULL);
    th->checking_sema = sm;
    sm = CreateSemaphore(NULL, 0, 1, NULL);
    th->ready_sema = sm;
    sm = CreateSemaphore(NULL, 1, 1, NULL);
    th->you_clean_up_sema = sm;
    th->refcount = refcount;

    h = CreateThread(NULL, 4096, (LPTHREAD_START_ROUTINE)WindowsFDReader, th, 0, &id);

    th->thread = h;
  }
#endif
}

rktio_fd_t *rktio_system_fd(rktio_t *rktio, intptr_t system_fd, int modes)
{
  rktio_fd_t *rfd;

  rfd = malloc(sizeof(rktio_fd_t));
  rfd->modes = modes;
  

#ifdef RKTIO_SYSTEM_UNIX
  rfd->fd = system_fd;
  {
    struct stat buf;
    int cr;
    do {
      cr = fstat(rfd->fd, &buf);
    } while ((cr == -1) && (errno == EINTR));
    rfd->regfile = S_ISREG(buf.st_mode);
  }
#endif

#ifdef RKTIO_SYSTEM_WINDOWS
  rfd->fd = (HANDLE)system_fd;
  rfd->regfile = (GetFileType(rfd->fd) == FILE_TYPE_DISK);
#endif
  
  if (modes & RKTIO_OPEN_READ)
    init_read_fd(rfd);
  
  return rfd;
}

intptr_t rktio_fd_system_fd(rktio_t *rktio, rktio_fd_t *rfd)
{
  return rfd->fd;
}

/*************************************************************/
/* opening a file fd                                         */
/*************************************************************/

static rktio_fd_t *open_read(rktio_t *rktio, char *filename)
{
#ifdef RKTIO_SYSTEM_UNIX
  int fd;
  struct stat buf;
  rktio_fd_t *rfd;

  do {
    fd = open(filename, O_RDONLY | RKTIO_NONBLOCKING | RKTIO_BINARY);
  } while ((fd == -1) && (errno == EINTR));

  if (fd == -1) {
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
      rfd = malloc(sizeof(rktio_fd_t));
      rfd->modes = RKTIO_OPEN_READ;
      rfd->fd = fd;
      rfd->regfile = S_ISREG(buf.st_mode);

      init_read_fd(rfd);

      return rfd;
    }
  }
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  HANDLE fd;
  rktio_fd_t *rfd;
  
  fd = CreateFileW(WIDE_PATH(filename),
		   GENERIC_READ,
		   FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
		   NULL,
		   OPEN_EXISTING,
		   0,
		   NULL);

  if (fd == INVALID_HANDLE_VALUE) {
    get_windows_error();
    return NULL;
  }

  rfd = malloc(sizeof(rktio_fd_t));
  rfd->modes = RKTIO_OPEN_READ;
  rfd->fd = fd;
  rfd->regfile = (GetFileType(fd) == FILE_TYPE_DISK);

  init_read_fd(rfd);
  
  return rfd;
#endif
}

static rktio_fd_t *open_write(rktio_t *rktio, char *filename, int modes)
{
#ifdef RKTIO_SYSTEM_UNIX
  int fd;
  int flags;
  struct stat buf;
  int cr;
  rktio_fd_t *rfd;

  flags = (((modes & RKTIO_OPEN_READ) ? O_RDWR : O_WRONLY)
           | ((modes & RKTIO_OPEN_MUST_EXIST ? 0 : O_CREAT)));

  if (modes & RKTIO_OPEN_APPEND)
    flags |= O_APPEND;
  else if (modes & RKTIO_OPEN_TRUNCATE)
    flags |= O_TRUNC;
  else if (!(modes & RKTIO_OPEN_CAN_EXIST))
    flags |= O_EXCL;

  do {
    fd = open(filename, flags | RKTIO_NONBLOCKING | RKTIO_BINARY, 0666);
  } while ((fd == -1) && (errno == EINTR));

  if (errno == ENXIO) {
    /* FIFO with no reader? Try opening in RW mode: */
    flags -= O_WRONLY;
    flags |= O_RDWR;
    do {
      fd = open(filename, flags | RKTIO_NONBLOCKING | RKTIO_BINARY, 0666);
    } while ((fd == -1) && (errno == EINTR));
  }

  if (fd == -1) {
    if (errno == EISDIR) {
      set_racket_error(RKTIO_ERROR_IS_A_DIRECTORY);
      return NULL;
    } else if (errno == EEXIST) {
      if (!(modes & RKTIO_OPEN_REPLACE)) {
        set_racket_error(RKTIO_ERROR_EXISTS);
        return NULL;
      } else {
	do {
	  cr = unlink(filename);
	} while ((cr == -1) && (errno == EINTR));

	if (cr) {
          get_posix_error();
          return NULL;
        }
        
	do {
	  fd = open(filename, flags | RKTIO_BINARY, 0666);
	} while ((fd == -1) && (errno == EINTR));
      }
    }

    if (fd == -1) {
      get_posix_error();
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

  rfd = malloc(sizeof(rktio_fd_t));
  rfd->modes = modes;
  rfd->fd = fd;
  rfd->regfile = S_ISREG(buf.st_mode);
  return rfd;
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  HANDLE fd;
  int hmode, regfile;
  BY_HANDLE_FILE_INFORMATION info;
  rktio_fd_t *rfd;

  if (modes & RKTIO_OPEN_MUST_EXIST) {
    if (modes & RKTIO_OPEN_TRUNNCATE)
      hmode = TRUNCATE_EXISTING;
    else
      hmode = OPEN_EXISTING;
  } else if (modes & RKTIO_OPEN_CAN_EXIST)
    hmode = OPEN_ALWAYS;
  else
    hmode = CREATE_NEW;

  fd = CreateFileW(WIDE_PATH_temp(filename),
		   GENERIC_WRITE | ((modes & RKTIO_OPEN_READ) ? GENERIC_READ : 0),
		   FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
		   NULL,
		   hmode,
		   FILE_FLAG_BACKUP_SEMANTICS, /* lets us detect directories in NT */
		   NULL);

  if (fd == INVALID_HANDLE_VALUE) {
    int errv;
    errv = GetLastError();
    if ((errv == ERROR_ACCESS_DENIED) && (existsok < -1)) {
      /* Delete and try again... */
      if (DeleteFileW(WIDE_PATH_temp(filename))) {
	fd = CreateFileW(WIDE_PATH_temp(filename),
                         GENERIC_WRITE | ((modes & RKTIO_OPEN_READ) ? GENERIC_READ : 0),
                         FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                         NULL,
                         hmode,
                         0,
                         NULL);
	if (fd == INVALID_HANDLE_VALUE) {
	  get_windows_error();
          return NULL;
        }
      } else {
        get_windows_error();
        return NULL;
      }
    } else if (errv == ERROR_FILE_EXISTS) {
      set_racket_error(RKTIO_ERROR_EXISTS);
      return NULL;
    }

    if (fd == INVALID_HANDLE_VALUE) {
      get_windows_error();
      return NULL;
    }
  }

  if (GetFileInformationByHandle(fd, &info)) {
    if (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
      CloseHandle(fd);
      set_racket_error(RKTIO_ERROR_IS_A_DIRECTORY);
      return NULL;
    }
  }

  rfd = malloc(sizeof(rktio_fd_t));
  rfd->modes = modes;
  rfd->fd = fd;
  rfd->regfile = (GetFileType(fd) == FILE_TYPE_DISK);

  if (rfd->regfile && (modes & RKTIO_OPEN_APPEND)) {
    SetFilePointer(fd, 0, NULL, FILE_END);
  }

  return rfd;
#endif
}

rktio_fd_t *rktio_open(rktio_t *rktio, char *filename, int modes)
{
  if (modes & RKTIO_OPEN_WRITE)
    return open_write(rktio, filename, modes);
  else
    return open_read(rktio, filename);
}

/*************************************************************/
/* closing                                                   */
/*************************************************************/

int rktio_close(rktio_t *rktio, rktio_fd_t *rfd)
{
#ifdef RKTIO_SYSTEM_UNIX
  int cr;
  
# ifdef USE_FCNTL_AND_FORK_FOR_FILE_LOCKS
  release_lockf(rfd->fd);
# endif

  do {
    cr = close(rfd->fd);
  } while ((cr == -1) && (errno == EINTR));
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (rfd->th) {
    CSI_proc csi;

    /* -1 for checking means "shut down" */
    rfd->th->checking = -1;
    ReleaseSemaphore(rfd->th->checking_sema, 1, NULL);

    if (rfd->th->eof && (rfd->th->eof != INVALID_HANDLE_VALUE)) {
      ReleaseSemaphore(rfd->th->eof, 1, NULL);
      rfd->th->eof = NULL;
    }

    csi = get_csi();
    if (csi) {
      /* Helps thread wake up. Otherwise, it's possible for the
         thread to stay stuck trying to read, in which case the
         file handle (probably a pipe) doesn't get closed. */
      csi(rfd->th->thread);
    }

    /* Try to get out of cleaning up the records (since they can't be
       cleaned until the thread is also done: */
    if (WaitForSingleObject(rfd->th->you_clean_up_sema, 0) != WAIT_OBJECT_0) {
      /* The other thread exited and left us with clean-up: */
      WindowsFDICleanup(rfd->th);
    } /* otherwise, thread is responsible for clean-up */
  }
  if (rfd->oth) {
    CSI_proc csi;

    csi = get_csi();

    if (csi) {
      /* See also call to csi in fd_close_input */
      csi(rfd->oth->thread);
    }
    CloseHandle(rfd->oth->thread);
    rfd->oth->done = 1;
    ReleaseSemaphore(rfd->oth->work_sema, 1, NULL);

    /* Try to leave clean-up to the other thread: */
    if (WaitForSingleObject(rfd->oth->you_clean_up_sema, 0) != WAIT_OBJECT_0) {
      /* Other thread is already done, so we're stuck with clean-up: */
      WindowsFDOCleanup(rfd->oth);
    } /* otherwise, thread is responsible for clean-up */
  }
  if (!rfp->th && !rfp->oth) {
    CloseHandle(rfd->fd);
  }
#endif

  free(rfd);
  
  return 1;
}

/*************************************************************/
/* polling                                                   */
/*************************************************************/

#ifdef SOME_FDS_ARE_NOT_SELECTABLE
static int try_get_fd_char(int fd, int *ready)
{
  int old_flags, c;
  unsigned char buf[1];

  old_flags = fcntl(fd, F_GETFL, 0);
  if (!(old_flags & RKTIO_NONBLOCKING))
    fcntl(fd, F_SETFL, old_flags | RKTIO_NONBLOCKING);
  do {
    c = read(fd, buf, 1);
  } while ((c == -1) && errno == EINTR);
  if (!(old_flags & RKTIO_NONBLOCKING))
    fcntl(fd, F_SETFL, old_flags);

  if (c < 0) {
    *ready = 0;
    return 0;
  } else {
    *ready = 1;
    if (!c)
      return EOF;
    else
      return buf[0];
  }
}
#endif

int rktio_poll_read_ready(rktio_t *rktio, rktio_fd_t *rfd)
{
  if (rfd->regfile)
    return 1;

#ifdef RKTIO_SYSTEM_UNIX
  {
    int r;

# ifdef SOME_FDS_ARE_NOT_SELECTABLE
    if (rfd->bufcount)
      return 1;
# endif

# ifdef HAVE_POLL_SYSCALL
    struct pollfd pfd[1];
    pfd[0].fd = rfd->fd;
    pfd[0].events = POLLIN;
    do {
      r = poll(pfd, 1, 0);
    } while ((r == -1) && (errno == EINTR));
# else
    DECL_FDSET(readfds, 1);
    DECL_FDSET(exnfds, 1);
    struct timeval time = {0, 0};

    INIT_DECL_RD_FDSET(readfds);
    INIT_DECL_ER_FDSET(exnfds);

    RKTIO_FD_ZERO(readfds);
    RKTIO_FD_ZERO(exnfds);
    RKTIO_FD_SET(rfd->fd, readfds);
    RKTIO_FD_SET(rfd->fd, exnfds);

    do {
      r = select(rfd->fd + 1, RKTIO_FDS(readfds), NULL, RKTIO_FDS(exnfds), &time);
    } while ((r == -1) && (errno == EINTR));
# endif

# ifdef SOME_FDS_ARE_NOT_SELECTABLE
    /* Try a non-blocking read: */
    if (!r && !rfd->textmode) {
      int c, ready;

      c = try_get_fd_char(rfd->fd, &ready);
      if (ready) {
        if (c != EOF) {
          rfd->buffpos = 0;
          rfd->buffer[0] = (unsigned char)c;
          rfd->bufcount = 1;
        }
        r = 1;
      }
    }
# endif

    return r != 0;
  }
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (!rfd->th) {
    /* No thread -- so wait works. This case isn't actually used
       right now, because wait doesn't seem to work reliably for
       anything that we can recognize other than regfiles, which are
       handled above. */
    if (WaitForSingleObject(rfd->fd, 0) == WAIT_OBJECT_0)
      return 1;
  } else {
    /* Has the reader thread pulled in data? */
    if (rfd->th->checking) {
      /* The thread is still trying, last we knew. Check the
         data-is-ready sema: */
      if (WaitForSingleObject(rfd->th->ready_sema, 0) == WAIT_OBJECT_0) {
        rfd->th->checking = 0;
        return 1;
      }
    } else if (rfd->th->avail || rfd->th->err || rfd->th->eof)
      return 1; /* other thread found data */
    else {
      /* Doesn't have anything, and it's not even looking. Tell it
         to look: */
      rfd->th->checking = 1;
      ReleaseSemaphore(rfd->th->checking_sema, 1, NULL);
    }
  }

  return 0;
#endif
}

int poll_write_ready_or_flushed(rktio_t *rktio, rktio_fd_t *rfd, int check_flushed)
{
#ifdef RKTIO_SYSTEM_UNIX
  if (check_flushed)
    return 1;
  else {
    int sr;
# ifdef HAVE_POLL_SYSCALL
    GC_CAN_IGNORE struct pollfd pfd[1];
    pfd[0].fd = rfd->fd;
    pfd[0].events = POLLOUT;
    do {
      sr = poll(pfd, 1, 0);
    } while ((sr == -1) && (errno == EINTR));
# else
    DECL_FDSET(writefds, 1);
    DECL_FDSET(exnfds, 1);
    struct timeval time = {0, 0};
  
    INIT_DECL_WR_FDSET(writefds);
    INIT_DECL_ER_FDSET(exnfds);
  
    RKTIO_FD_ZERO(writefds);
    RKTIO_FD_ZERO(exnfds);
    RKTIO_FD_SET(rfd->fd, writefds);
    RKTIO_FD_SET(rfd->fd, exnfds);
  
    do {
      /* Mac OS X 10.8 and 10.9: select() seems to claim that a pipe
         is always ready for output. To work around that problem,
         kqueue() support is enabled for pipes, so we shouldn't get
         here much for pipes. */
      sr = select(rfd->fd + 1, NULL, RKTIO_FDS(writefds), RKTIO_FDS(exnfds), &time);
    } while ((sr == -1) && (errno == EINTR));
# endif

    if (sr == -1) {
      get_posix_error();
      return RKTIO_POLL_ERROR;
    } else
      return (sr != 0);
  }
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (rfd->oth) {
    /* Pipe output that can block... */
    int retval;
    Win_FD_Output_Thread *oth = rfd->oth;

    WaitForSingleObject(oth->lock_sema, INFINITE);
    if (oth->nonblocking) {
      if (oth->needflush) {
	oth->needflush = 0;
	oth->flushed = 0;
	ReleaseSemaphore(oth->work_sema, 1, NULL); /* start trying to flush */
	retval = 0;
      } else
	retval = oth->flushed;
    } else
      retval = (oth->err_no || (check_flushed
                                ? !oth->buflen
                                : (oth->buflen < MZPORT_FD_BUFFSIZE)));
    if (!retval && !check_flushed)
      WaitForSingleObject(oth->ready_sema, 0); /* clear any leftover state */
    ReleaseSemaphore(oth->lock_sema, 1, NULL);

    return retval;
  } else
    return 1; /* non-blocking output, such as a console, or haven't written yet */
#endif
}

int rktio_poll_write_ready(rktio_t *rktio, rktio_fd_t *rfd)
{
  return poll_write_ready_or_flushed(rktio, rfd, 0);
}

int rktio_poll_write_flushed(rktio_t *rktio, rktio_fd_t *rfd)
{
  return poll_write_ready_or_flushed(rktio, rfd, 1);
}

void rktio_poll_add(rktio_t *rktio, rktio_fd_t *rfd, rktio_poll_set_t *fds, int modes)
{
#ifdef RKTIO_SYSTEM_UNIX
  rktio_poll_set_t *fds2;

  if (modes & RKTIO_POLL_READ) {
    RKTIO_FD_SET(rfd->fd, fds);
  }
  if (modes & RKTIO_POLL_WRITE) {
    fds2 = RKTIO_GET_FDSET(fds, 1);
    RKTIO_FD_SET(rfd->fd, fds2);
  }
  fds2 = RKTIO_GET_FDSET(fds, 2);
  RKTIO_FD_SET(rfd->fd, fds2);
#endif  
#ifdef RKTIO_SYSTEM_WINDOWS
  if (modes & RKTIO_POLL_READ) {
    if (rfd->th) {
      /* See fd_byte_ready */
      if (!rfd->th->checking) {
        if (rfd->th->avail || rfd->th->err || rfd->th->eof) {
          /* Data is ready. We shouldn't be trying to sleep, so force an
             immediate wake-up: */
          rktio_fdset_add_nosleep(fds);
        } else {
          rfd->th->checking = 1;
          ReleaseSemaphore(rfd->th->checking_sema, 1, NULL);
          rktio_fdset_add_handle(rfd->th->ready_sema, fds, 1);
        }
      } else
        rktio_fdset_add_handle(rfd->th->ready_sema, fds, 1);
    } else if (rfd->regfile) {
      /* regular files never block */
      rktio_fdset_add_nosleep(fds);
    } else {
      /* This case is not currently used. See fd_byte_ready. */
      rktio_fdset_add_handle(rfd->fd, fds, 0);
    }
  }

  if (modes & RKTIO_POLL_WRITE) {
    if (rfp->oth && !fd_write_ready(port))
      rktio_fdset_add_handle(rfp->oth->ready_sema, fds, 1);
    else
      rktio_fdset_nosleep(fds);
  }
#endif
}

/*************************************************************/
/* reading                                                   */
/*************************************************************/

intptr_t rktio_read(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t len)
{
#ifdef RKTIO_SYSTEM_UNIX
  intptr_t bc;
  
  if (rfd->regfile) {
    /* Reading regular file never blocks */
    do {
      bc = read(rfd->fd, buffer, len);
    } while ((bc == -1) && (errno == EINTR));

    if (bc == -1) {
      get_posix_error();
      return RKTIO_READ_ERROR;
    } else if (bc == 0)
      return RKTIO_READ_EOF;
    else
      return bc;
  } else {
    /* We use a non-blocking read here, even though we've waited
       for input above, because an external process might have
       gobbled the characters that we expected to get. */
    int old_flags;
    
    old_flags = fcntl(rfd->fd, F_GETFL, 0);
    if (!(old_flags & RKTIO_NONBLOCKING))
      fcntl(rfd->fd, F_SETFL, old_flags | RKTIO_NONBLOCKING);
    
    do {
      bc = read(rfd->fd, buffer, len);
    } while ((bc == -1) && errno == EINTR);

    if ((bc == -1) && (errno != EAGAIN))
      get_posix_error();
    
    if (!(old_flags & RKTIO_NONBLOCKING))
      fcntl(rfd->fd, F_SETFL, old_flags);
    
    if (bc == -1) {
      if (errno == EAGAIN)
        return 0; /* no bytes from a non-blocking read */
      else
        return RKTIO_READ_ERROR;
    } else if (bc == 0)
      return RKTIO_READ_EOF;
    else
      return bc;
  }
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (!rfd->th) {
    /* We can read directly. This must be a regular file, where
       reading never blocks. */
    DWORD rgot, delta;
    
    if (!ReadFile((HANDLE)rfd->fd, buffer, len, &rgot, NULL)) {
      get_windows_error();
      return RKTIO_READ_ERROR;
    }
    
    if (!rgot)
      return RKTIO_READ_EOF;
    else
      return rgot;
  } else {
    if (!rktio_poll_read(rfd))
      return 0;
    
    /* If we get this far, there's definitely data available.
       Extract data made available by the reader thread. */
    if (rfd->th->eof) {
      if (rfd->th->eof != INVALID_HANDLE_VALUE) {
        ReleaseSemaphore(rfd->th->eof, 1, NULL);
        rfd->th->eof = NULL;
      }
      return RKTIO_READ_EOF;
    } else if (rfd->th->err) {
      set_windows_error(rfd->th->err);
      return RKTIO_READ_ERROR;
    } else {
      intptr_t bc = rfd->th->avail;
      rfd->th->avail = 0;
      FIXME read the data;
      return bc;
    }
  }
#endif
}

#ifdef RKTIO_SYSTEM_WINDOWS

static long WINAPI WindowsFDReader(Win_FD_Input_Thread *th)
{
  DWORD toget, got;
  int perma_eof = 0;
  HANDLE eof_wait = NULL;

  if (GetFileType((HANDLE)th->fd) == FILE_TYPE_PIPE) {
    /* Reading from a pipe will return early when data is available. */
    toget = MZPORT_FD_BUFFSIZE;
  } else {
    /* Non-pipe: get one char at a time: */
    toget = 1;
  }

  while (!perma_eof && !th->err) {
    /* Wait until we're supposed to look for input: */
    WaitForSingleObject(th->checking_sema, INFINITE);

    if (th->checking < 0)
      break;

    if (ReadFile(th->fd, th->buffer, toget, &got, NULL)) {
      th->avail = got;
      if (!got) {
	/* We interpret a send of 0 bytes as a mid-stream EOF. */
	eof_wait = CreateSemaphore(NULL, 0, 1, NULL);
	th->eof = eof_wait;
      }
    } else {
      int err;
      err = GetLastError();
      if (err == ERROR_BROKEN_PIPE) {
	th->eof = INVALID_HANDLE_VALUE;
	perma_eof = 1;
      } else
	th->err = err;
    }

    /* Notify main program that we found something: */
    ReleaseSemaphore(th->ready_sema, 1, NULL);

    if (eof_wait) {
      WaitForSingleObject(eof_wait, INFINITE);
      eof_wait = NULL;
    }
  }

  /* We have to clean up if the main program has abandoned us: */
  if (WaitForSingleObject(th->you_clean_up_sema, 0) != WAIT_OBJECT_0) {
    WindowsFDICleanup(th);
  } /* otherwise, main program is responsible for clean-up */

  return 0;
}

static void WindowsFDICleanup(Win_FD_Input_Thread *th)
{
  int rc;

  CloseHandle(th->checking_sema);
  CloseHandle(th->ready_sema);
  CloseHandle(th->you_clean_up_sema);

  rc = adj_refcount(th->refcount, -1);
  if (!rc) CloseHandle(th->fd);

  free(th->buffer);
  free(th);
}

#endif

/*************************************************************/
/* writing                                                   */
/*************************************************************/

intptr_t rktio_write(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t len)
{
#ifdef RKTIO_SYSTEM_UNIX
  int flags, errsaved;
  intptr_t amt;
  
  flags = fcntl(rfd->fd, F_GETFL, 0);
  if (!(flags & RKTIO_NONBLOCKING))
    fcntl(rfd->fd, F_SETFL, flags | RKTIO_NONBLOCKING);
  
  amt = len;
  
  do {
    do {
      len = write(rfd->fd, buffer, amt);
    } while ((len == -1) && (errno == EINTR));
    
    /* If there was no room to write `amt` bytes, then it's
       possible that writing fewer bytes will succeed. That seems
       to be the case with FIFOs on Mac OS X, for example. */
    amt = amt >> 1;
  } while ((len == -1) && (errno == EAGAIN) && (amt > 0));

  if (len == -1) {
    errsaved = errno;
    get_posix_error();
  }
  
  if (!(flags & RKTIO_NONBLOCKING))
    fcntl(rfd->fd, F_SETFL, flags);

  if (len == -1) {
    if (errsaved == EAGAIN)
      return 0;
    else
      return RKTIO_WRITE_ERROR;
  } else
    return len;
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  DWORD winwrote;
  
  if (rfd->regfile) {
    /* Regular files never block, so this code looks like the Unix
       code.  We've cheated in the make_fd proc and called
       consoles regular files, because they cannot block, either. */

    /* If we try to write too much at once, the result
       is ERROR_NOT_ENOUGH_MEMORY (as opposed to a partial write). */
    int ok;
    intptr_t towrite = len;
    
    while (1) {
      ok = WriteFile((HANDLE)rfd->fd, buffer, towrite, &winwrote, NULL);
      if (!ok)
        errsaved = GetLastError();
	    
      if (!ok && (errsaved == ERROR_NOT_ENOUGH_MEMORY)) {
        towrite = towrite >> 1;
        if (!towrite)
          break;
      } else
        break;
    }

    if (!ok) {
      get_windows_error();
      return RKTIO_WRITE_ERROR;
    }

    return winwrote;
  } else {
    /* If we don't have a thread yet, we'll need to start it. If
       we have a non-blocking pipe, we can try the write (and
       we'll still need the thread to determine when the data is
       flushed). */
    intptr_t out_len = 0;
    int ok, errsaved;
    
    if (!rfd->oth || rfd->oth->nonblocking) {
      int nonblocking;

      /* If we don't have a thread, this is our first write attempt.
         Determine whether this is a non-blocking pipe: */
      if (!rfd->oth) {
        /* The FILE_TYPE_PIPE test is currently redundant, I think,
           but better safe than sorry. */
        nonblocking = (rktio_windows_nt_or_later()
                       && (GetFileType((HANDLE)rfd->fd) == FILE_TYPE_PIPE));
      } else
        nonblocking = 1; /* must be, or we would not have gotten here */

      if (nonblocking) {
        /* Unless we're still trying to flush old data, write to the
           pipe and have the other thread start flushing it. */
        DWORD nonblock = PIPE_NOWAIT;
        int flushed;

        if (rfd->oth) {
          if (rfd->oth->needflush) {
            /* Not flushed, but we haven't promised not to block: */
            flushed = 1;
          } else {
            WaitForSingleObject(rfd->oth->lock_sema, INFINITE);
            flushed = rfd->oth->flushed;
            ReleaseSemaphore(rfd->oth->lock_sema, 1, NULL);
          }
        } else
          flushed = 1; /* haven't written anything before */

        if (flushed) {
          /* Put the pipe in non-blocking mode and write. */
          int towrite;

          towrite = len;

          /* Apparently, the semantics of non-blocking pipe writes
             is not partial writes, but giving up entirely when
             the other end isn't being read. In other words, if we
             try to write too much and nothing is being pulled
             from the pipe, winwrote will be set to 0. Also, if
             we try to write too much at once, the result is a
             ERROR_NOT_ENOUGH_MEMORY error. Account for these
             behaviors by trying to write less each iteration when the
             write fails. (Yuck.) */
          while (1) {
            if (!rfd->unblocked) {
              ok = SetNamedPipeHandleState((HANDLE)rfd->fd, &nonblock, NULL, NULL);
              if (ok)
                rfd->unblocked = 1;
              else
                errsaved = GetLastError();
            } else
              ok = 1;
            if (ok) {
              ok = WriteFile((HANDLE)rfd->fd, buffer, towrite, &winwrote, NULL);
              if (!ok)
                errsaved = GetLastError();
            }

            if ((ok && !winwrote)
                || (!ok && (errsaved == ERROR_NOT_ENOUGH_MEMORY))) {
              towrite = towrite >> 1;
              if (!towrite) {
                set_windows_error();
                return RKTIO_WRITE_ERROR;
              }
            } else
              break;
          }
        } else {
          /* Don't try to write while flushing. */
          ok = 1;
          winwrote = 0;
        }

        if (ok)
          out_len = winwrote;
      }
      /* and create the writer thread... */

      if (!rfd->oth) {
        /* We create a thread even for pipes that can be put in
           non-blocking mode, because that seems to be the only
           way to get evt behavior. */
        Win_FD_Output_Thread *oth;
        HANDLE h;
        DWORD id;
        unsigned char *bfr;
        OS_SEMAPHORE_TYPE sm;

        oth = malloc(sizeof(Win_FD_Output_Thread));
        rfd->oth = oth;

        oth->nonblocking = nonblocking;

        if (!nonblocking) {
          bfr = malloc(MZPORT_FD_BUFFSIZE);
          oth->buffer = bfr;
          oth->flushed = 0;
          oth->needflush = 0;
        } else {
          oth->buffer = NULL;
          oth->flushed = 1;
          oth->needflush = 1;
        }

        oth->buflen = 0;
        oth->bufstart = 0;
        oth->bufend = 0;

        oth->fd = (HANDLE)rfd->fd;
        oth->err_no = 0;
        oth->done = 0;
        sm = CreateSemaphore(NULL, 1, 1, NULL);
        oth->lock_sema = sm;
        sm = CreateSemaphore(NULL, 0, 1, NULL);
        oth->work_sema = sm;
        sm = CreateSemaphore(NULL, 1, 1, NULL);
        oth->ready_sema = sm;
        sm = CreateSemaphore(NULL, 1, 1, NULL);
        oth->you_clean_up_sema = sm;
        oth->refcount = rfd->refcount;
            
        h = CreateThread(NULL, 4096, (LPTHREAD_START_ROUTINE)WindowsFDWriter, oth, 0, &id);

        oth->thread = h;
      }
    }

    /* We have a thread, if only to watch when the flush is
       done... */
    
    if (!rfd->oth->nonblocking) {
      /* This case is for Win 95/98/Me anonymous pipes and
         character devices.  We haven't written anything yet! We
         write to a buffer read by the other thread, and return --
         the other thread takes care of writing. Thus, as long as
         there's room in the buffer, we don't block, and we can
         tell whether there's room. Technical problem: if multiple
         ports are attched to the same underlying pipe (different
         handle, same "device"), the port writes can get out of
         order. We try to avoid the problem by sleeping. */

      Win_FD_Output_Thread *oth = rfd->oth;

      WaitForSingleObject(oth->lock_sema, INFINITE);
      if (oth->err_no) {
        errsaved = oth->err_no;
        ok = 0;
      } else if (oth->buflen == MZPORT_FD_BUFFSIZE) {
        WaitForSingleObject(oth->ready_sema, 0); /* clear any leftover state */
        ok = 1;
      } else {
        intptr_t topp;
        int was_pre;

        if (!oth->buflen) {
          /* Avoid fragmenting in circular buffer: */
          oth->bufstart = 0;
          oth->bufend = 0;
        }

        /* Write to top part of circular buffer, then bottom part
           if anything's left. */

        if (oth->bufstart <= oth->bufend) {
          was_pre = 1;
          topp = MZPORT_FD_BUFFSIZE;
        } else {
          was_pre = 0;
          topp = oth->bufstart;
        }

        winwrote = topp - oth->bufend;
        if (winwrote > len)
          winwrote = len;

        memcpy(oth->buffer + oth->bufend, buffer, winwrote);
        oth->buflen += winwrote;
        out_len = winwrote;

        oth->bufend += winwrote;
        if (oth->bufend == MZPORT_FD_BUFFSIZE)
          oth->bufend = 0;

        if (was_pre) {
          if (winwrote < len) {
            /* Try continuing with a wrap-around: */
            winwrote = oth->bufstart - oth->bufend;
            if (winwrote > len - out_len)
              winwrote = len - out_len;

            memcpy(oth->buffer + oth->bufend, buffer + out_len, winwrote);
            oth->buflen += winwrote;
            oth->bufend += winwrote;
            out_len += winwrote;
          }
        }
        /* Let the other thread know that it should start trying
           to write, if it isn't already: */
        ReleaseSemaphore(oth->work_sema, 1, NULL);
        Sleep(0); /* to decrease the chance of re-ordering flushes */

        ok = 1;
      }
      ReleaseSemaphore(oth->lock_sema, 1, NULL);
    } else if (out_len > 0) {
      /* We've already written, which implies that no flush is
         in progress. We'll need a flush check in the future. */
      rfd->oth->needflush = 1;
    }

    if (ok)
      return out_len;

    set_windows_error(errsaved);
    return RKTIO_WRITE_ERROR;
  }
#endif
}
