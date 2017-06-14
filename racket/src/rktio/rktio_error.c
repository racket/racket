#include "rktio.h"
#include "rktio_private.h"
#ifdef RKTIO_SYSTEM_WINDOWS
# include <windows.h>
#endif
#include <errno.h>
#include <string.h>

typedef struct err_str_t {
  int id;
  char *str;
} err_str_t;

err_str_t err_strs[]  = { 
  { RKTIO_ERROR_UNSUPPORTED, "unsupported" },
  { RKTIO_ERROR_EXISTS, "file or directory already exists"},
  { RKTIO_ERROR_LINK_FAILED, "link creation failed" },
  { RKTIO_ERROR_NOT_A_LINK, "not a link" },
  { RKTIO_ERROR_BAD_PERMISSION, "unsupported permission value" },
  { RKTIO_ERROR_IS_A_DIRECTORY, "path refers to a directory" },
  { RKTIO_ERROR_NOT_A_DIRECTORY, "path does not refer to a directory" },
  { RKTIO_ERROR_NO_TILDE, "path does not start with a tilde" },
  { RKTIO_ERROR_ILL_FORMED_USER, "ill-formed username in path" },
  { RKTIO_ERROR_UNKNOWN_USER, "unknown username in path" },
  { RKTIO_ERROR_INIT_FAILED, "initialization failed" },
  { RKTIO_ERROR_LTPS_NOT_FOUND, "not handle found" },
  { RKTIO_ERROR_LTPS_REMOVED, "handles successfully removed" },
  { RKTIO_ERROR_CONNECT_TRYING_NEXT, "connection failed, but can try again" },
  { RKTIO_ERROR_ACCEPT_NOT_READY, "no connection ready to accept" }, 
  { RKTIO_ERROR_HOST_AND_PORT_BOTH_UNSPECIFIED, "neither hostname nor port number specified" },
  { RKTIO_ERROR_INFO_TRY_AGAIN, "spurious empty UDP message; try again" },
  { RKTIO_ERROR_TRY_AGAIN, "no UDP message available" },
  { RKTIO_ERROR_TRY_AGAIN_WITH_IPV4, "listen failed, but try again with just IPv4 addresses" },
  { RKTIO_ERROR_TIME_OUT_OF_RANGE, "time value out-of-range for date conversion" },
  { 0, NULL }
};

void rktio_get_posix_error(rktio_t *rktio)
{
  rktio->errid = errno;
  rktio->errkind = RKTIO_ERROR_KIND_POSIX;
}

void rktio_set_racket_error(rktio_t *rktio, int new_errid)
{
  rktio->errid = new_errid;
  rktio->errkind = RKTIO_ERROR_KIND_RACKET;
}

#ifdef RKTIO_SYSTEM_WINDOWS
void rktio_get_windows_error(rktio_t *rktio)
{
  rktio->errid = GetLastError();
  rktio->errkind = RKTIO_ERROR_KIND_WINDOWS;
}

void rktio_set_windows_error(rktio_t *rktio, int errid)
{
  rktio->errid = errid;
  rktio->errkind = RKTIO_ERROR_KIND_WINDOWS;
}
#endif

int rktio_get_last_error(rktio_t *rktio)
{
  return rktio->errid;
}

int rktio_get_last_error_kind(rktio_t *rktio)
{
  return rktio->errkind;
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
    s = rktio_gai_strerror(errid);
#ifdef RKTIO_SYSTEM_WINDOWS
  else if (kind == RKTIO_ERROR_KIND_WINDOWS) {
    wchar_t mbuf[256];
    int len;
    if ((type != 'e') && !es) {
      if ((len = FormatMessageW((FORMAT_MESSAGE_FROM_SYSTEM
                                 | FORMAT_MESSAGE_IGNORE_INSERTS), 
                                NULL,
                                en, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                                mbuf, 255, NULL))) {
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
  if (rktio->last_error_str)
    free(rktio->last_err_str);
#endif
}
