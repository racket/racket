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
#endif

int rktio_get_last_error(rktio_t *rktio)
{
  return rktio->errid;
}

int rktio_get_last_error_kind(rktio_t *rktio)
{
  return rktio->errkind;
}

char *rktio_get_error_string(rktio_t *rktio, int kind, int errid)
{
  char *s = NULL;
  if (kind == RKTIO_ERROR_KIND_POSIX)
    s = strerror(errid);
  if (s) return s;
  return "???";
}
