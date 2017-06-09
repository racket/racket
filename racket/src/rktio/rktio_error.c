#include "rktio.h"
#include "rktio_private.h"
#ifdef RKTIO_SYSTEM_WINDOWS
# include <windows.h>
#endif
#include <errno.h>

THREAD_LOCAL_DECL(static intptr_t errid);
THREAD_LOCAL_DECL(static int errkind);

void rktio_get_posix_error(void)
{
  errid = errno;
  errkind = RKTIO_ERROR_KIND_POSIX;
}

void rktio_set_racket_error(int new_errid)
{
  errid = new_errid;
  errkind = RKTIO_ERROR_KIND_RACKET;
}

#ifdef RKTIO_SYSTEM_WINDOWS
void rktio_get_windows_error(void)
{
  errid = GetLastError();
  errkind = RKTIO_ERROR_KIND_WINDOWS;
}
#endif

int rktio_get_last_error(void)
{
  return errid;
}

int rktio_get_last_error_kind(void)
{
  return errkind;
}

char *rktio_get_error_string(int kind, int errid)
{
  return "???";
}
