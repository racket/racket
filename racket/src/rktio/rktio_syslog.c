#include "rktio.h"
#include "rktio_private.h"

#ifdef RKTIO_SYSTEM_UNIX
# include <syslog.h>
#endif

#ifdef RKTIO_SYSTEM_WINDOWS
# include <stdlib.h>
# include <string.h>
#endif

void rktio_syslog_init(rktio_t *rktio)
{
#ifdef RKTIO_SYSTEM_WINDOWS
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

static rktio_ok_t do_syslog(rktio_t *rktio, int level, const char *name, const char *msg, const char *exec_name, int record_err)
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
  WORD ty;
  unsigned long sev;
  const wchar_t *a[1];
  char *naya = NULL;
  int ok;

  EnterCriticalSection(&rktio_global_cs);
  if (rktio->hEventLog == INVALID_HANDLE_VALUE) {
    int ok = 1;
    rktio_err_t err;
    wchar_t *wp = WIDE_PATH_copy(exec_name, &err);
    if (wp) {
      rktio->hEventLog = RegisterEventSourceW(NULL, wp);
      if (rktio->hEventLog == INVALID_HANDLE_VALUE) {
        if (record_err)
          get_windows_error();
        ok = 0;
      }
      free(wp);
    } else {
      if (record_err)
        memcpy(&rktio->err, &err, sizeof(rktio_err_t));
      ok = 0;
    }
    if (!ok) {
      LeaveCriticalSection(&rktio_global_cs);
      return 0;
    }
  }
  LeaveCriticalSection(&rktio_global_cs);
    
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

  ok = ReportEventW(rktio->hEventLog, ty, 1 /* category */,
                    (sev << 30) | 2 /* message */,
                    NULL, 
                    1, 0,
                    a, NULL);
  if (!ok && record_err)
    get_windows_error();
    
  if (naya)
    free(naya);

  return ok;
#endif
}

rktio_ok_t rktio_syslog(rktio_t *rktio, int level, const char *name, const char *msg, const char *exec_name)
{
  return do_syslog(rktio, level, name, msg, exec_name, 1);
}

void rktio_syslog_best_effort(rktio_t *rktio, int level,
                              rktio_const_string_t name, rktio_const_string_t msg,
                              rktio_const_string_t exec_name)
{
  (void)do_syslog(rktio, level, name, msg, exec_name, 0);
}
