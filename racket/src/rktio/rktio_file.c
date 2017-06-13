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

/*************************************************************/
/* opening a file fd                                         */
/*************************************************************/

static rktio_fd_t *open_read(rktio_t *rktio, char *filename)
{
#ifdef RKTIO_SYSTEM_UNIX
  int fd;
  struct stat buf;

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
      return rktio_system_fd(rktio, fd, (RKTIO_OPEN_READ
                                         | (S_ISREG(buf.st_mode)
                                            ? RKTIO_OPEN_REGFILE
                                            : RKTIO_OPEN_NOT_REGFILE)));
    }
  }
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  HANDLE fd;
  
  fd = CreateFileW(WIDE_PATH_temp(filename),
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

  return rktio_system_fd(rktio, (intptr_t)fd, RKTIO_OPEN_READ);
#endif
}

static rktio_fd_t *open_write(rktio_t *rktio, char *filename, int modes)
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

  return rktio_system_fd(rktio, fd, (modes
                                     | (S_ISREG(buf.st_mode)
                                        ? RKTIO_OPEN_REGFILE
                                        : RKTIO_OPEN_NOT_REGFILE)));
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  HANDLE fd;
  int hmode, regfile;
  BY_HANDLE_FILE_INFORMATION info;
  rktio_fd_t *rfd;

  if (modes & RKTIO_OPEN_MUST_EXIST) {
    if (modes & RKTIO_OPEN_TRUNCATE)
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
    if ((errv == ERROR_ACCESS_DENIED) && (modes & RKTIO_OPEN_REPLACE)) {
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

  rfd = rktio_system_fd(rktio, (intptr_t)fd, modes);

  if ((modes & RKTIO_OPEN_APPEND) && rktio_fd_is_regular_file(rktio, rfd)) {
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

