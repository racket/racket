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

static rktio_fd_t *open_write(rktio_t *rktio, const char *filename, int modes)
{
#ifdef RKTIO_SYSTEM_UNIX
  int fd;
  int flags;
  struct stat buf;
  int cr;

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

  return rktio_system_fd(rktio, fd, (modes
                                     | (S_ISREG(buf.st_mode)
                                        ? RKTIO_OPEN_REGFILE
                                        : RKTIO_OPEN_NOT_REGFILE)));
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
		   FILE_FLAG_BACKUP_SEMANTICS, /* lets us detect directories in NT */
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

rktio_fd_t *rktio_open(rktio_t *rktio, const char *filename, int modes)
{
  if (modes & RKTIO_OPEN_WRITE)
    return open_write(rktio, filename, modes);
  else
    return open_read(rktio, filename, modes);
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
	  (void)SetFilePointer((HANDLE)fd, lo_w, &hi_w, FILE_BEGIN);
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
