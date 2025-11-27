#include "rktio.h"
#include "rktio_private.h"
#include <errno.h>
#include <string.h>
#include <stdlib.h>

typedef struct err_str_t {
  int id;
  char *str;
} err_str_t;

err_str_t err_strs[]  = { 
  { RKTIO_ERROR_UNSUPPORTED, "unsupported" },
  { RKTIO_ERROR_INVALID_PATH, "invalid path encoding" },
  { RKTIO_ERROR_DOES_NOT_EXIST, "no such file or directory" },
  { RKTIO_ERROR_EXISTS, "file or directory already exists" },
  { RKTIO_ERROR_LINK_FAILED, "link creation failed" },
  { RKTIO_ERROR_NOT_A_LINK, "not a link" },
  { RKTIO_ERROR_BAD_PERMISSION, "unsupported permission value" },
  { RKTIO_ERROR_IS_A_DIRECTORY, "path refers to a directory" },
  { RKTIO_ERROR_NOT_A_DIRECTORY, "path does not refer to a directory" },
  { RKTIO_ERROR_UNSUPPORTED_TEXT_MODE, "cannot use text mode on a non-file device" },
  { RKTIO_ERROR_CANNOT_FILE_POSITION, "cannot get/set position/size on device" },
  { RKTIO_ERROR_NO_TILDE, "path does not start with a tilde" },
  { RKTIO_ERROR_ILL_FORMED_USER, "ill-formed username in path" },
  { RKTIO_ERROR_UNKNOWN_USER, "unknown username in path" },
  { RKTIO_ERROR_INIT_FAILED, "initialization failed" },
  { RKTIO_ERROR_LTPS_NOT_FOUND, "handle not found" },
  { RKTIO_ERROR_LTPS_REMOVED, "handles successfully removed" },
  { RKTIO_ERROR_CONNECT_TRYING_NEXT, "connection failed, but can try again" },
  { RKTIO_ERROR_ACCEPT_NOT_READY, "no connection ready to accept" }, 
  { RKTIO_ERROR_HOST_AND_PORT_BOTH_UNSPECIFIED, "neither hostname nor port number specified" },
  { RKTIO_ERROR_INFO_TRY_AGAIN, "spurious empty UDP message; try again" },
  { RKTIO_ERROR_TRY_AGAIN, "no UDP message available" },
  { RKTIO_ERROR_TRY_AGAIN_WITH_IPV4, "listen failed, but try again with just IPv4 addresses" },
  { RKTIO_ERROR_TIME_OUT_OF_RANGE, "time value out-of-range for date conversion" },
  { RKTIO_ERROR_NO_SUCH_ENVVAR, "no value as an environment variable" },
  { RKTIO_ERROR_SHELL_EXECUTE_FAILED, "ShellExecute failed" },
  { RKTIO_ERROR_CONVERT_NOT_ENOUGH_SPACE, "encoding conversion needs more output space" },
  { RKTIO_ERROR_CONVERT_BAD_SEQUENCE, "ill-formed input encountered in encoding conversion" },
  { RKTIO_ERROR_CONVERT_PREMATURE_END, "input encoding ended prematurely" },
  { RKTIO_ERROR_CONVERT_OTHER, "encoding conversion encountered an error" },
  { RKTIO_ERROR_DLL, "error is from dlopen" },
  { 0, NULL }
};

void rktio_get_posix_error(rktio_err_t *err)
{
  err->errid = errno;
  err->errkind = RKTIO_ERROR_KIND_POSIX;
}

void rktio_set_racket_error(rktio_err_t *err, int new_errid)
{
  err->errid = new_errid;
  err->errkind = RKTIO_ERROR_KIND_RACKET;
}

#ifdef RKTIO_SYSTEM_WINDOWS
void rktio_get_windows_error(rktio_err_t *err)
{
  err->errid = GetLastError();
  err->errkind = RKTIO_ERROR_KIND_WINDOWS;
}

void rktio_set_windows_error(rktio_err_t *err, int errid)
{
  err->errid = errid;
  err->errkind = RKTIO_ERROR_KIND_WINDOWS;
}
#endif

int rktio_get_last_error(rktio_t *rktio)
{
  return rktio->err.errid;
}

int rktio_get_error(rktio_result_t *res)
{
  return res->err.errid;
}

int rktio_get_last_error_kind(rktio_t *rktio)
{
  return rktio->err.errkind;
}

int rktio_get_error_kind(rktio_result_t *res)
{
  return res->err.errkind;
}

int rktio_get_last_error_step(rktio_t *rktio)
{
  return rktio->err.errstep;
}

int rktio_get_error_step(rktio_result_t *res)
{
  return res->err.errstep;
}

void rktio_set_last_error(rktio_t *rktio, int kind, int errid)
{
  rktio->err.errkind = kind;
  rktio->err.errid = errid;
}

void rktio_set_error(rktio_err_t *err, int kind, int errid)
{
  err->errkind = kind;
  err->errid = errid;
}

void rktio_set_last_error_step(rktio_t *rktio, int new_errstep)
{
  rktio->err.errstep = new_errstep;
}

static void do_remap_error(rktio_err_t *err)
{
  if (err->errkind == RKTIO_ERROR_KIND_RACKET) {
    switch (err->errid) {
    case RKTIO_ERROR_DOES_NOT_EXIST:
#ifdef RKTIO_SYSTEM_UNIX
      rktio_set_error(err, RKTIO_ERROR_KIND_POSIX, ENOENT);
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
      rktio_set_error(err, RKTIO_ERROR_KIND_WINDOWS, ERROR_FILE_NOT_FOUND);
#endif
      break;
    case RKTIO_ERROR_EXISTS:
#ifdef RKTIO_SYSTEM_UNIX
      rktio_set_error(err, RKTIO_ERROR_KIND_POSIX, EEXIST);
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
      rktio_set_error(err, RKTIO_ERROR_KIND_WINDOWS, ERROR_FILE_EXISTS);
#endif
      break;
    case RKTIO_ERROR_ACCESS_DENIED:
#ifdef RKTIO_SYSTEM_UNIX
      rktio_set_error(err, RKTIO_ERROR_KIND_POSIX, EACCES);
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
      rktio_set_error(err, RKTIO_ERROR_KIND_WINDOWS, ERROR_ACCESS_DENIED);
#endif
      break;
    }
  }
}

void rktio_remap_last_error(rktio_t *rktio)
{
  do_remap_error(&rktio->err);
}

void rktio_remap_error(rktio_result_t *res)
{
  do_remap_error(&res->err);
}

const char *rktio_get_error_string(rktio_t *rktio, int kind, int errid)
{
  const char *s = NULL;
  if (kind == RKTIO_ERROR_KIND_RACKET) {
    int i;
    for (i = 0; err_strs[i].str; i++) {
      if (err_strs[i].id == errid)
        return err_strs[i].str;
    }
  } else if (kind == RKTIO_ERROR_KIND_POSIX) {
#ifndef NO_STRERROR_AVAILABLE
    s = strerror(errid);
#endif
  } else if (kind == RKTIO_ERROR_KIND_GAI)
    s = rktio_gai_strerror(rktio, errid); /* may use `last_err_str` */
#ifdef RKTIO_SYSTEM_WINDOWS
  else if (kind == RKTIO_ERROR_KIND_WINDOWS) {
    wchar_t mbuf[256];
    intptr_t len, i;
    if ((len = FormatMessageW((FORMAT_MESSAGE_FROM_SYSTEM
                               | FORMAT_MESSAGE_IGNORE_INSERTS), 
                              NULL,
                              errid, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                              mbuf, 255, NULL))) {
      char *es;
      if (len == 255)
        mbuf[254] = 0;
      else
        mbuf[len] = 0;
      es = NARROW_PATH_copy(mbuf);
      /* Remove newlines: */
      for (i = strlen(es) - 1; i > 0; i--) {
        if (isspace(es[i]))
          es[i] = 0;
        else
          break;
      }
      
      if (rktio->last_err_str)
        free(rktio->last_err_str);
      rktio->last_err_str = es;
      
      return es;
    }
  }
#endif
  if (s) return s;
  return "???";
}

const char *rktio_get_last_error_string(rktio_t *rktio)
{
  return rktio_get_error_string(rktio,
                                rktio_get_last_error_kind(rktio),
                                rktio_get_last_error(rktio));
}

void rktio_error_clean(rktio_t *rktio)
{
#ifdef RKTIO_SYSTEM_WINDOWS
  if (rktio->last_err_str)
    free(rktio->last_err_str);
#endif
}

rktio_result_t *rktio_make_error(rktio_err_t *err)
{
  rktio_result_t *res = (rktio_result_t *)malloc(sizeof(rktio_result_t));
  res->is_success = 0;
  memcpy(&res->err, err, sizeof(rktio_err_t));
  return res;
}

rktio_result_t *rktio_make_success(void)
{
  rktio_result_t *res = (rktio_result_t *)malloc(sizeof(rktio_result_t));
  res->is_success = 1;
  return res;
}

rktio_bool_t rktio_result_is_success(rktio_result_t *res)
{
  return res->is_success;
}

intptr_t rktio_result_integer(rktio_result_t *res)
{
  return res->success.i;
}

char *rktio_result_string(rktio_result_t *res)
{
  return res->success.str;
}

rktio_directory_list_t *rktio_result_directory_list(rktio_result_t *res)
{
  return res->success.dir_list;
}

rktio_const_string_t rktio_classify_error(int errkind, int errid)
{
  rktio_err_t err;

  err.errkind = errkind;
  err.errid = errid;
  err.errstep = 0;

  do_remap_error(&err);

  if (err.errkind == RKTIO_ERROR_KIND_POSIX) {
    switch (err.errid) {
# define case_errno(E) case E: return #E
# ifdef E2BIG
      case_errno(E2BIG);
# endif
# ifdef EACCES
      case_errno(EACCES);
# endif
# ifdef EADDRINUSE
      case_errno(EADDRINUSE);
# endif
# ifdef EADDRNOTAVAIL
      case_errno(EADDRNOTAVAIL);
# endif
# ifdef EAFNOSUPPORT
      case_errno(EAFNOSUPPORT);
# endif
# ifdef EAGAIN
      case_errno(EAGAIN);
# endif
# ifdef EALREADY
      case_errno(EALREADY);
# endif
# ifdef EBADF
      case_errno(EBADF);
# endif
# ifdef EBADMSG
      case_errno(EBADMSG);
# endif
# ifdef EBUSY
      case_errno(EBUSY);
# endif
# ifdef ECANCELED
      case_errno(ECANCELED);
# endif
# ifdef ECHILD
      case_errno(ECHILD);
# endif
# ifdef ECONNABORTED
      case_errno(ECONNABORTED);
# endif
# ifdef ECONNREFUSED
      case_errno(ECONNREFUSED);
# endif
# ifdef ECONNRESET
      case_errno(ECONNRESET);
# endif
# ifdef EDEADLK
      case_errno(EDEADLK);
# endif
# ifdef EDESTADDRREQ
      case_errno(EDESTADDRREQ);
# endif
# ifdef EDOM
      case_errno(EDOM);
# endif
# ifdef EDQUOT
      case_errno(EDQUOT);
# endif
# ifdef EEXIST
      case_errno(EEXIST);
# endif
# ifdef EFAULT
      case_errno(EFAULT);
# endif
# ifdef EFBIG
      case_errno(EFBIG);
# endif
# ifdef EHOSTUNREACH
      case_errno(EHOSTUNREACH);
# endif
# ifdef EIDRM
      case_errno(EIDRM);
# endif
# ifdef EILSEQ
      case_errno(EILSEQ);
# endif
# ifdef EINPROGRESS
      case_errno(EINPROGRESS);
# endif
# ifdef EINTR
      case_errno(EINTR);
# endif
# ifdef EINVAL
      case_errno(EINVAL);
# endif
# ifdef EIO
      case_errno(EIO);
# endif
# ifdef EISCONN
      case_errno(EISCONN);
# endif
# ifdef EISDIR
      case_errno(EISDIR);
# endif
# ifdef ELOOP
      case_errno(ELOOP);
# endif
# ifdef EMFILE
      case_errno(EMFILE);
# endif
# ifdef EMLINK
      case_errno(EMLINK);
# endif
# ifdef EMSGSIZE
      case_errno(EMSGSIZE);
# endif
# ifdef EMULTIHOP
      case_errno(EMULTIHOP);
# endif
# ifdef ENAMETOOLONG
      case_errno(ENAMETOOLONG);
# endif
# ifdef ENETDOWN
      case_errno(ENETDOWN);
# endif
# ifdef ENETRESET
      case_errno(ENETRESET);
# endif
# ifdef ENETUNREACH
      case_errno(ENETUNREACH);
# endif
# ifdef ENFILE
      case_errno(ENFILE);
# endif
# ifdef ENOBUFS
      case_errno(ENOBUFS);
# endif
# ifdef ENODATA
      case_errno(ENODATA);
# endif
# ifdef ENODEV
      case_errno(ENODEV);
# endif
# ifdef ENOENT
      case_errno(ENOENT);
# endif
# ifdef ENOEXEC
      case_errno(ENOEXEC);
# endif
# ifdef ENOLCK
      case_errno(ENOLCK);
# endif
# ifdef ENOLINK
      case_errno(ENOLINK);
# endif
# ifdef ENOMEM
      case_errno(ENOMEM);
# endif
# ifdef ENOMSG
      case_errno(ENOMSG);
# endif
# ifdef ENOPROTOOPT
      case_errno(ENOPROTOOPT);
# endif
# ifdef ENOSPC
      case_errno(ENOSPC);
# endif
# ifdef ENOSR
      case_errno(ENOSR);
# endif
# ifdef ENOSTR
      case_errno(ENOSTR);
# endif
# ifdef ENOSYS
      case_errno(ENOSYS);
# endif
# ifdef ENOTCONN
      case_errno(ENOTCONN);
# endif
# ifdef ENOTDIR
      case_errno(ENOTDIR);
# endif
# ifdef ENOTEMPTY
      case_errno(ENOTEMPTY);
# endif
# ifdef ENOTRECOVERABLE
      case_errno(ENOTRECOVERABLE);
# endif
# ifdef ENOTSOCK
      case_errno(ENOTSOCK);
# endif
# ifdef ENOTSUP
      case_errno(ENOTSUP);
# endif
# ifdef ENOTTY
      case_errno(ENOTTY);
# endif
# ifdef ENXIO
      case_errno(ENXIO);
# endif
# ifdef EOVERFLOW
      case_errno(EOVERFLOW);
# endif
# ifdef EOWNERDEAD
      case_errno(EOWNERDEAD);
# endif
# ifdef EPERM
      case_errno(EPERM);
# endif
# ifdef EPIPE
      case_errno(EPIPE);
# endif
# ifdef EPROTO
      case_errno(EPROTO);
# endif
# ifdef EPROTONOSUPPORT
      case_errno(EPROTONOSUPPORT);
# endif
# ifdef EPROTOTYPE
      case_errno(EPROTOTYPE);
# endif
# ifdef ERANGE
      case_errno(ERANGE);
# endif
# ifdef EROFS
      case_errno(EROFS);
# endif
# ifdef ESPIPE
      case_errno(ESPIPE);
# endif
# ifdef ESRCH
      case_errno(ESRCH);
# endif
# ifdef ESTALE
      case_errno(ESTALE);
# endif
# ifdef ETIME
      case_errno(ETIME);
# endif
# ifdef ETIMEDOUT
      case_errno(ETIMEDOUT);
# endif
# ifdef ETXTBSY
      case_errno(ETXTBSY);
# endif
# ifdef EXDEV
      case_errno(EXDEV);
# endif
# if defined(EWOULDBLOCK) && defined(EAGAIN)
#  if  EWOULDBLOCK != EAGAIN
    case EWOULDBLOCK: return "EAGAIN";
#  endif
# endif
# if defined(EOPNOTSUPP) && defined(ENOTSUP)
#  if  EOPNOTSUPP != ENOTSUP
    case EOPNOTSUPP: return "ENOTSUP";
#  endif
# endif
    }
  } else if (err.errkind == RKTIO_ERROR_KIND_WINDOWS) {
# ifdef RKTIO_SYSTEM_WINDOWS
    switch (err.errid) {
#  define case_winerr(ERR, E) case ERR: return #E
      case_winerr(ERROR_FILE_NOT_FOUND, ENOENT);
      case_winerr(ERROR_PATH_NOT_FOUND, ENOENT);
      case_winerr(ERROR_TOO_MANY_OPEN_FILES, EMFILE);
      case_winerr(ERROR_ACCESS_DENIED, EACCES);
      case_winerr(ERROR_INVALID_HANDLE, EBADF);
      case_winerr(ERROR_NOT_ENOUGH_MEMORY, ENOMEM);
      case_winerr(ERROR_INVALID_ACCESS, EINVAL);
      case_winerr(ERROR_INVALID_DRIVE, ENODEV);
      case_winerr(ERROR_CURRENT_DIRECTORY, EBUSY);
      case_winerr(ERROR_NOT_SAME_DEVICE, EXDEV);
      case_winerr(ERROR_NO_MORE_FILES, ENOENT);
      case_winerr(ERROR_WRITE_PROTECT, EROFS);
      case_winerr(ERROR_BAD_UNIT, ENODEV);
      case_winerr(ERROR_NOT_READY, EBUSY);
      case_winerr(ERROR_SHARING_VIOLATION, EACCES);
      case_winerr(ERROR_LOCK_VIOLATION, EACCES);
      case_winerr(ERROR_HANDLE_DISK_FULL, ENOSPC);
      case_winerr(ERROR_NOT_SUPPORTED, ENOTSUP);
      case_winerr(ERROR_FILE_EXISTS, EEXIST);
      case_winerr(ERROR_INVALID_PARAMETER, EINVAL);
      case_winerr(ERROR_BROKEN_PIPE, EPIPE);
      case_winerr(ERROR_DISK_FULL, ENOSPC);
      case_winerr(ERROR_CALL_NOT_IMPLEMENTED, ENOSYS);
      case_winerr(ERROR_INVALID_NAME, EINVAL);
      case_winerr(ERROR_DIR_NOT_EMPTY, ENOTEMPTY);
      case_winerr(ERROR_ALREADY_EXISTS, EEXIST);
      case_winerr(ERROR_FILENAME_EXCED_RANGE, ENAMETOOLONG);
      case_winerr(ERROR_DIRECTORY, ENOTDIR);
      case_winerr(ERROR_TOO_MANY_LINKS, EMLINK);
      case_winerr(ERROR_WAIT_NO_CHILDREN, ECHILD);
      case_winerr(ERROR_CHILD_NOT_COMPLETE, ECHILD);
      case_winerr(ERROR_BUSY, EBUSY);
      case_winerr(ERROR_NO_PROC_SLOTS, EAGAIN);
      case_winerr(ERROR_PRIVILEGE_NOT_HELD, EPERM);
      case_winerr(ERROR_INVALID_OWNER, EPERM);
      case_winerr(ERROR_INVALID_PRIMARY_GROUP, EPERM);
      case_winerr(ERROR_SEM_TIMEOUT, ETIMEDOUT);
      case_winerr(ERROR_OPERATION_ABORTED, ECANCELED);
      case_winerr(ERROR_IO_INCOMPLETE, EAGAIN);
      case_winerr(ERROR_IO_PENDING, EINPROGRESS);
      case_winerr(ERROR_INVALID_FUNCTION, EINVAL);
      case_winerr(ERROR_BAD_LENGTH, EINVAL);
      case_winerr(ERROR_SEEK, EINVAL);
      case_winerr(ERROR_NEGATIVE_SEEK, EINVAL);
      case_winerr(ERROR_BUSY_DRIVE, EBUSY);
      case_winerr(ERROR_BAD_PIPE, EPIPE);
      case_winerr(ERROR_PIPE_BUSY, EBUSY);
      case_winerr(ERROR_NO_DATA, EPIPE);
      case_winerr(ERROR_PIPE_NOT_CONNECTED, EPIPE);
      case_winerr(ERROR_MORE_DATA, EMSGSIZE);
      case_winerr(ERROR_NO_MORE_ITEMS, ENOENT);
      case_winerr(ERROR_PARTIAL_COPY, EIO);
      case_winerr(ERROR_INVALID_ADDRESS, EFAULT);
      case_winerr(ERROR_ARITHMETIC_OVERFLOW, ERANGE);
      case_winerr(ERROR_PIPE_CONNECTED, EBUSY);
      case_winerr(ERROR_PIPE_LISTENING, EBUSY);
      case_winerr(ERROR_INVALID_AT_INTERRUPT_TIME, EINTR);
    }
# endif
  }

  return NULL;
}
