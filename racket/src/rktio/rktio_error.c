#include "rktio.h"
#include "rktio_private.h"
#ifdef RKTIO_SYSTEM_WINDOWS
# include <windows.h>
#endif
#include <errno.h>
#include <string.h>

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
  if (kind == RKTIO_ERROR_KIND_POSIX) {
#ifndef NO_STRERROR_AVAILABLE
    s = strerror(errid);
#endif
  } else if (kind == RKTIO_ERROR_KIND_GAI)
    s = rktio_gai_strerror(errid);
  if (s) return s;
  return "???";
}
