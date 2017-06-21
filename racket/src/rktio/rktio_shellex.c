#include "rktio.h"
#include "rktio_private.h"

#define rktio_SHOW_CONVERT(v) case RKTIO_ ## v: show_mode = v; break

rktio_ok_t rktio_shell_execute(rktio_t *rktio,
			       const char *verb,
			       const char *target,
			       const char *arg,
			       const char *dir,
			       int show_mode)
/* dir needs to have normalized path separators */
{
#ifdef RKTIO_SYSTEM_WINDOWS
  SHELLEXECUTEINFOW se;
  int ok, r;

  switch(show_mode) {
    rktio_SHOW_CONVERT(SW_HIDE);
    rktio_SHOW_CONVERT(SW_MAXIMIZE);
    rktio_SHOW_CONVERT(SW_MINIMIZE);
    rktio_SHOW_CONVERT(SW_RESTORE);
    rktio_SHOW_CONVERT(SW_SHOW);
    rktio_SHOW_CONVERT(SW_SHOWDEFAULT);
    rktio_SHOW_CONVERT(SW_SHOWMAXIMIZED);
    rktio_SHOW_CONVERT(SW_SHOWMINIMIZED);
    rktio_SHOW_CONVERT(SW_SHOWMINNOACTIVE);
    rktio_SHOW_CONVERT(SW_SHOWNA);
    rktio_SHOW_CONVERT(SW_SHOWNOACTIVATE);
    rktio_SHOW_CONVERT(SW_SHOWNORMAL);
  }

  memset(&se, 0, sizeof(se));
  se.fMask = SEE_MASK_NOCLOSEPROCESS | SEE_MASK_FLAG_DDEWAIT;
  se.cbSize = sizeof(se);
  if (!verb)
    se.lpVerb = NULL;
  else
    se.lpVerb = WIDE_PATH_copy(verb);
  se.lpFile = WIDE_PATH_copy(target);
  se.lpParameters = WIDE_PATH_copy(arg);
  se.lpDirectory = WIDE_PATH_copy(dir);
  se.nShow = show_mode;
  se.hwnd = NULL;
  
  r = (int)(intptr_t)ShellExecuteW(se.hwnd, se.lpVerb, se.lpFile, se.lpParameters,
				   se.lpDirectory, se.nShow);

  ok = (r > 32);

  if (!ok) {
    switch(r) {
    case ERROR_FILE_NOT_FOUND:
    case ERROR_PATH_NOT_FOUND:
    case ERROR_BAD_FORMAT:
      set_windows_error(r);
      break;
    default:
      /* Other results are not Windows error codes,
	 so just collapse them to a Racket error */
      set_racket_error(RKTIO_ERROR_SHELL_EXECUTE_FAILED);
      break;
    }
  }

  if (se.lpVerb) free((char *)se.lpVerb);
  free((char *)se.lpFile);
  free((char *)se.lpParameters);
  free((char *)se.lpDirectory);

  return ok;
#else
  set_racket_error(RKTIO_ERROR_UNSUPPORTED);
  return 0;
#endif
}
