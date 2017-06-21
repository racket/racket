#include "rktio.h"
#include "rktio_private.h"

#ifdef RKTIO_SYSTEM_UNIX
# include <syslog.h>
#endif

#ifdef RKTIO_SYSTEM_WINDOWS
# include <stdlib.h>
# include <string.h>
static int event_procs_ready;
typedef HANDLE (WINAPI *do_RegisterEventSourceProc)(wchar_t *lpUNCServerName, wchar_t *lpSourceName);
typedef BOOL (WINAPI *do_ReportEventProc)(HANDLE hEventLog, WORD wType, WORD wCategory, DWORD dwEventID,
                                          PSID lpUserSid, WORD wNumStrings, DWORD dwDataSize, const wchar_t** lpStrings,
                                          LPVOID lpRawData);
static do_RegisterEventSourceProc do_RegisterEventSource;
static do_ReportEventProc do_ReportEvent;
#endif

void rktio_syslog_init(rktio_t *rktio)
{
#ifdef RKTIO_SYSTEM_WINDOWS
  if (!event_procs_ready) {
    HMODULE hm;
    hm = LoadLibraryW(L"advapi32.dll");
    if (hm) {
      do_RegisterEventSource = (do_RegisterEventSourceProc)GetProcAddress(hm, "RegisterEventSourceW");
      do_ReportEvent = (do_ReportEventProc)GetProcAddress(hm, "ReportEventW");
    }
    event_procs_ready = 1;
  }
  
  rktio->hEventLog = INVALID_HANDLE_VALUE;
#endif
}

void rktio_syslog_clean(rktio_t *rktio)
{
#ifdef RKTIO_SYSTEM_WINDOWS
  if (rktio->hEventLog != INVALID_HANDLE_VALUE)
    CloseHandle(rktio->hEventLog);
#endif
}

rktio_ok_t rktio_syslog(rktio_t *rktio, int level, const char *name, const char *msg, const char *exec_name)
{
#ifdef RKTIO_SYSTEM_UNIX
  int pri;
  switch (level) {
  case RKTIO_LOG_FATAL:
    pri = LOG_CRIT;
    break;
  case RKTIO_LOG_ERROR:
    pri = LOG_ERR;
    break;
  case RKTIO_LOG_WARNING:
    pri = LOG_WARNING;
    break;
  case RKTIO_LOG_INFO:
    pri = LOG_INFO;
    break;
  case RKTIO_LOG_DEBUG:
  default:
    pri = LOG_DEBUG;
    break;
  }
  if (name)
    syslog(pri, "%s: %s", name, msg);
  else
    syslog(pri, "%s", msg);
  return 1;
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  if (do_RegisterEventSource) {
    WORD ty;
    unsigned long sev;
    const wchar_t *a[1];
    char *naya = NULL;
    int ok;

    if (rktio->hEventLog == INVALID_HANDLE_VALUE) {
      rktio->hEventLog = do_RegisterEventSource(NULL, WIDE_PATH_temp(exec_name));
      if (rktio->hEventLog == INVALID_HANDLE_VALUE) {
        get_windows_error();
        return 0;
      }
    }
    
    switch (level) {
    case RKTIO_LOG_FATAL:
      ty = EVENTLOG_ERROR_TYPE;
      sev = 3;
      break;
    case RKTIO_LOG_ERROR:
      ty = EVENTLOG_ERROR_TYPE;
      sev = 3;
      break;
    case RKTIO_LOG_WARNING:
      ty = EVENTLOG_WARNING_TYPE;
      sev = 2;
      break;
    case RKTIO_LOG_INFO:
      ty = EVENTLOG_INFORMATION_TYPE;
      sev = 1;
      break;
    case RKTIO_LOG_DEBUG:
    default:
      ty = EVENTLOG_AUDIT_SUCCESS;
      sev = 0;
      break;
    }
    if (name) {
      intptr_t len, slen;
      slen = strlen(name);
      len = strlen(msg);
      naya = malloc(slen + 2 + len + 1);
      memcpy(naya, name, slen);
      memcpy(naya + slen, ": ", 2);
      memcpy(naya + slen + 2, msg, len);
      naya[slen + 2 + len] = 0;
      msg = naya;
    }
    a[0] = WIDE_PATH_temp(msg);

    ok = do_ReportEvent(rktio->hEventLog, ty, 1 /* category */,
                        (sev << 30) | 2 /* message */,
                        NULL, 
                        1, 0,
                        a, NULL);
    if (!ok)
      get_windows_error();
    
    if (naya)
      free(naya);

    return ok;
  } else {
    set_racket_error(RKTIO_ERROR_UNSUPPORTED);
    return 0;
  }
#endif
}
