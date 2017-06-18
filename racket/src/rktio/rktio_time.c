#include "rktio.h"
#include "rktio_private.h"
#include <stdlib.h>
#include <string.h>

#if !defined(DONT_USE_GETRUSAGE) && defined(RKTIO_SYSTEM_UNIX)
# define USE_GETRUSAGE
#endif

#ifdef RKTIO_SYSTEM_UNIX
#  include <time.h>
#  include <sys/time.h>
#  ifdef USE_GETRUSAGE
#   include <sys/types.h>
#   include <sys/time.h>
#   include <sys/resource.h>
#   include <errno.h>
#  endif /* USE_GETRUSAGE */
#  ifdef USE_SYSCALL_GETRUSAGE
#   include <sys/syscall.h>
#   define getrusage(a, b)  syscall(SYS_GETRUSAGE, a, b)
#   define USE_GETRUSAGE
#  endif /* USE_SYSCALL_GETRUSAGE */
#  if !defined(USE_GETRUSAGE) && !defined(USER_TIME_IS_CLOCK)
#   include <sys/times.h>
#  endif
#endif

#ifdef RKTIO_SYSTEM_WINDOWS
typedef BOOL (WINAPI*GetTimeZoneInformationForYearProc_t)(USHORT wYear, void* pdtzi, LPTIME_ZONE_INFORMATION ptzi);
static GetTimeZoneInformationForYearProc_t GetTimeZoneInformationForYearProc;

typedef BOOL (WINAPI*SystemTimeToTzSpecificLocalTimeExProc_t)(void *lpTimeZoneInformation, 
							      const SYSTEMTIME *lpUniversalTime,
							      LPSYSTEMTIME lpLocalTime);
static SystemTimeToTzSpecificLocalTimeExProc_t SystemTimeToTzSpecificLocalTimeExProc;
#endif


void rktio_init_time(rktio_t *rktio)
{
#ifdef RKTIO_SYSTEM_WINDOWS
  static int time_inited = 0;
  if (!time_inited) {
    HMODULE hm;
    hm = LoadLibraryW(L"kernel32.dll");

    GetTimeZoneInformationForYearProc
      = (GetTimeZoneInformationForYearProc_t)GetProcAddress(hm, "GetTimeZoneInformationForYear");
    SystemTimeToTzSpecificLocalTimeExProc
      = (SystemTimeToTzSpecificLocalTimeExProc_t)GetProcAddress(hm, "SystemTimeToTzSpecificLocalTimeEx");

    FreeLibrary(hm);

    time_inited = 1;
  }
#endif
}

/*========================================================================*/
/* Time                                                                   */
/*========================================================================*/

#ifndef CLOCKS_PER_SEC
#define CLOCKS_PER_SEC 1000000
#endif

#ifdef RKTIO_SYSTEM_WINDOWS
/* Number of milliseconds from 1601 to 1970: */
# define MSEC_OFFSET 11644473600000

rktio_int64_t get_hectonanoseconds_as_longlong()
/* this function can be called from any OS thread */
{
  FILETIME ft;
  rktio_int64_t v;
  GetSystemTimeAsFileTime(&ft);
  v = ((rktio_int64_t)ft.dwHighDateTime << 32) | ft.dwLowDateTime;
  v -= ((rktio_int64_t)MSEC_OFFSET * 10000);
  return v;
}
#endif

intptr_t rktio_get_milliseconds(void)
/* this function can be called from any OS thread */
{
#ifdef RKTIO_SYSTEM_WINDOWS
  return (intptr_t)(get_hectonanoseconds_as_longlong() / (rktio_int64_t)10000);
#else
  struct timeval now;
  gettimeofday(&now, NULL);
  return now.tv_sec * 1000 + now.tv_usec / 1000;
#endif
}

double rktio_get_inexact_milliseconds(void)
/* this function can be called from any OS thread */
{
#ifdef RKTIO_SYSTEM_WINDOWS
  rktio_int64_t v;
  v = get_hectonanoseconds_as_longlong();
  return (double)(v / 10000) + (((double)(v % 10000)) / 10000.0);
#else
  struct timeval now;
  gettimeofday(&now, NULL);
  return (double)now.tv_sec * 1000.0 + (double)now.tv_usec / 1000;
#endif
}

intptr_t rktio_get_process_milliseconds(rktio_t *rktio)
{
#ifdef USER_TIME_IS_CLOCK
  return scheme_get_milliseconds();
#else
# ifdef USE_GETRUSAGE
  struct rusage use;
  intptr_t s, u;

  do {
    if (!getrusage(RUSAGE_SELF, &use))
      break;
  } while (errno == EINTR);

  s = use.ru_utime.tv_sec + use.ru_stime.tv_sec;
  u = use.ru_utime.tv_usec + use.ru_stime.tv_usec;

  return s * 1000 + u / 1000;
# else
#  ifdef RKTIO_SYSTEM_WINDOWS
  {
    FILETIME cr, ex, kr, us;
    if (GetProcessTimes(GetCurrentProcess(), &cr, &ex, &kr, &us)) {
      rktio_int64_t v;
      v = ((((rktio_int64_t)kr.dwHighDateTime << 32) + kr.dwLowDateTime)
	   + (((rktio_int64_t)us.dwHighDateTime << 32) + us.dwLowDateTime));
      return (uintptr_t)(v / 10000);
    } else
      return 0; /* anything better to do? */
  }
#  else
  return clock()  * 1000 / CLOCKS_PER_SEC;

#  endif
# endif
#endif
}

intptr_t rktio_get_process_children_milliseconds(rktio_t *rktio)
{
#ifdef USER_TIME_IS_CLOCK
  return 0;
#else
# ifdef USE_GETRUSAGE
  struct rusage use;
  intptr_t s, u;
  
  do {
    if (!getrusage(RUSAGE_CHILDREN, &use))
      break;
  } while (errno == EINTR);

  s = use.ru_utime.tv_sec + use.ru_stime.tv_sec;
  u = use.ru_utime.tv_usec + use.ru_stime.tv_usec;

  return (s * 1000 + u / 1000);
# else
#  ifdef RKTIO_SYSTEM_WINDOWS
  return rktio->process_children_msecs;
#  else
  clock_t t;
  times(&t);
  return (t.tms_cutime + t.tms_cstime) * 1000 / CLK_TCK;
#  endif
# endif
#endif
}

rktio_timestamp_t rktio_get_seconds(rktio_t *rktio)
{
#ifdef RKTIO_SYSTEM_WINDOWS
  return (intptr_t)(get_hectonanoseconds_as_longlong() / (rktio_int64_t)10000000);
#else
#   ifdef USE_PLAIN_TIME
  time_t now;
  now = time(NULL);
  return now;
#   else
  struct timeval now;
  gettimeofday(&now, NULL);
  return now.tv_sec;
# endif
#endif
}

/*========================================================================*/
/* Date                                                                   */
/*========================================================================*/

#if defined(RKTIO_SYSTEM_WINDOWS)
/* Assuming not a leap year (and adjusted elsewhere): */
static int month_offsets[13] = { 0, 31, 59, 90,
				 120, 151, 181, 212,
				 243, 273, 304, 334,
                                 365};

# define dtxCOMP(f) if (a->f < b->f) return 1; if (a->f > b->f) return 0;

static int is_start_day_before(SYSTEMTIME *a, SYSTEMTIME *b)
{
  dtxCOMP(wYear);

  /* When comparing DST boundaries, we expect to get here,
     because wYear will be 0 to mean "every year". */

  dtxCOMP(wMonth);

  /* When comparing DST boundaries, it's unlikely that we'll get here,
     because that would mean that StdT and DST start in the same month. */

  dtxCOMP(wDay); /* for DST boundaires, this is a week number */
  dtxCOMP(wDayOfWeek);
  dtxCOMP(wHour);
  dtxCOMP(wMinute);

  return 0;
}

static int is_day_before(SYSTEMTIME *a, SYSTEMTIME *b)
/* a is a date, and b is a DST boundary spec */
{
  int dos, doc;

  if (b->wYear) {
    dtxCOMP(wYear);
  }

  dtxCOMP(wMonth);

  /* "Date" of a Sunday this month, 0 to 6: */
  dos = ((a->wDay - a->wDayOfWeek) + 7) % 7;
  /* Date of first b->wDayOfWeek this month, 1 to 7: */
  doc = (dos + b->wDayOfWeek) % 7;
  if (doc == 0) doc = 7;
  /* Date of change this year: */
  doc = doc + ((b->wDay - 1) * 7);
  if (doc > (month_offsets[b->wMonth] - month_offsets[b->wMonth-1]))
    doc -= 7;
  /* Above assumes that a time change doesn't occur on a leap day! */

  if (a->wDay < doc)
    return 1;
  if (a->wDay > doc)
    return 0;

  dtxCOMP(wHour);
  dtxCOMP(wMinute);

  return 0;
}
# undef dtxCOMP
#endif

#if defined(OS_X) && defined(__x86_64__)
/* work around a bug in localtime() in 10.6.8 */
# include <sys/param.h>
# include <sys/sysctl.h>
static int VALID_TIME_RANGE(intptr_t lnow)
{
  /* Fits in 32 bits? */
  int ilnow = (int)lnow;
  if (lnow == (intptr_t)ilnow)
    return 1;

  /* 10.7 or later? */
  {
    int a[2];
    size_t len;
    char *vers;

    a[0] = CTL_KERN;
    a[1] = KERN_OSRELEASE;
    sysctl(a, 2, NULL, &len, NULL, 0);
    vers = malloc(len * sizeof(char));
    sysctl(a, 2, vers, &len, NULL, 0);

    if ((vers[0] == '1') && (vers[1] == '0') && (vers[2] == '.')) {
      /* localtime() in 10.6.x (= 10.x at the kernel layer) doesn't seem
         to work right with negative numbers that don't fit into 32 bits */
      free(vers);
      return 0;
    }
    free(vers);
  }

  return 1;
}
#else

# ifdef MIN_VALID_DATE_SECONDS
#  define VALID_TIME_RANGE_MIN(x) ((x) >= MIN_VALID_DATE_SECONDS)
# else
#  define VALID_TIME_RANGE_MIN(x) 1
# endif

# if defined(MAX_VALID_DATE_SECONDS_BITS) && defined(SIXTY_FOUR_BIT_INTEGERS)
#  define VALID_TIME_RANGE_BITS(x) (((x) >= 0)                          \
                                    ? ((x) == ((x) & (((intptr_t)1 << MAX_VALID_DATE_SECONDS_BITS) - 1))) \
                                    : ((-(x)) == ((-(x)) & (((intptr_t)1 << MAX_VALID_DATE_SECONDS_BITS) - 1))))
# else
#  define VALID_TIME_RANGE_BITS(x) 1
# endif

# define VALID_TIME_RANGE(x) (VALID_TIME_RANGE_MIN(x) && VALID_TIME_RANGE_BITS(x))

#endif

rktio_date_t *rktio_seconds_to_date(rktio_t *rktio, rktio_timestamp_t seconds, int nanoseconds, int get_gmt)
{
  int hour, min, sec, month, day, wday, yday, dst;
  intptr_t year;
  long tzoffset;
#ifdef RKTIO_SYSTEM_WINDOWS
# define CHECK_TIME_T uintptr_t
  SYSTEMTIME localTime;
#else
# define CHECK_TIME_T time_t
  struct tm *localTime;
#endif
  CHECK_TIME_T now;
  char *tzn;
  rktio_date_t *result;

  if ((((intptr_t)(now = (CHECK_TIME_T)seconds)) == seconds)
      && VALID_TIME_RANGE(seconds)) {
    int success;

#ifdef RKTIO_SYSTEM_WINDOWS
    {
      rktio_uint64_t tmpC;
      tmpC = ((rktio_uint64_t)seconds * 10000000);
      if ((rktio_int64_t)tmpC / 10000000 != seconds) {
	/* overflow */
	success = 0;
      } else {
	rktio_int64_t nsC;
	FILETIME ft;
	nsC = tmpC + ((rktio_uint64_t)MSEC_OFFSET * 10000);
	if (nsC < (rktio_int64_t)tmpC) {
	  /* overflow */
	  success = 0;
	} else {
	  ft.dwLowDateTime = nsC & (rktio_int64_t)0xFFFFFFFF;
	  ft.dwHighDateTime = nsC >> 32;
	  success = FileTimeToSystemTime(&ft, &localTime);
	  if (success && !get_gmt) {
	    SYSTEMTIME t2 = localTime;
	    if (SystemTimeToTzSpecificLocalTimeExProc)
	      success = SystemTimeToTzSpecificLocalTimeExProc(NULL, &t2, &localTime);
	    else
	      success = SystemTimeToTzSpecificLocalTime(NULL, &t2, &localTime);
	  }
	}
      }
    }
#else
    if (get_gmt)
      localTime = gmtime(&now);
    else
      localTime = localtime(&now);
    success = !!localTime;
#endif

    if (success) {
#ifdef RKTIO_SYSTEM_WINDOWS

      hour = localTime.wHour;
      min = localTime.wMinute;
      sec = localTime.wSecond;

      month = localTime.wMonth;
      day = localTime.wDay;
      year = localTime.wYear;

      wday = localTime.wDayOfWeek;
      yday = month_offsets[localTime.wMonth-1] + day-1;
      /* leap-year adjustment: */
      if ((month > 2)
	  && ((year % 4) == 0)
	  && (((year % 100) != 0) || ((year % 400) == 0)))
	yday++;

      dst = 0;
      if (get_gmt) {
	tzoffset = 0;
	tzn = MSC_IZE(strdup)("UTC");
      } else {
	TIME_ZONE_INFORMATION tz;
	if (GetTimeZoneInformationForYearProc)
	  GetTimeZoneInformationForYearProc(localTime.wYear, NULL, &tz);
	else
	  (void)GetTimeZoneInformation(&tz);
	if (tz.StandardDate.wMonth) {
	  if (is_start_day_before(&tz.DaylightDate, &tz.StandardDate)) {
	    /* northern hemisphere */
	    dst = (!is_day_before(&localTime, &tz.DaylightDate)
		   && is_day_before(&localTime, &tz.StandardDate));
	  } else {
	    /* southern hemisphere */
	    dst = (is_day_before(&localTime, &tz.StandardDate)
		   || !is_day_before(&localTime, &tz.DaylightDate));
	  }
	}
	if (dst) {
	  tzoffset = (tz.Bias + tz.DaylightBias) * -60;
	  tzn = NARROW_PATH_copy(tz.DaylightName);
	} else {
	  tzoffset = (tz.Bias + tz.StandardBias) * -60;
	  tzn = NARROW_PATH_copy(tz.StandardName);
	}
      }
# define TZN_STRDUP(s) s
#else
      hour = localTime->tm_hour;
      min = localTime->tm_min;
      sec = localTime->tm_sec;

      month = localTime->tm_mon + 1;
      day = localTime->tm_mday;
      year = (uintptr_t)localTime->tm_year + 1900;

      wday = localTime->tm_wday;
      yday = localTime->tm_yday;

      if (get_gmt)
        dst = 0;
      else
        dst = localTime->tm_isdst;

      tzoffset = 0;
      if (!get_gmt) {
# ifdef USE_TIMEZONE_VAR
        tzoffset = -MSC_IZE(timezone);
# endif
# ifdef USE_TOD_FOR_TIMEZONE
        {
	  struct timezone xtz;
	  struct timeval xtv;
	  gettimeofday(&xtv, &xtz);
	  tzoffset = -(xtz.tz_minuteswest * 60);
        }
# endif
# ifdef USE_TIMEZONE_VAR_W_DLS
        tzoffset = -(MSC_IZE(timezone) - (dst ? 3600 : 0));
# endif
# ifdef USE_TIMEZONE_AND_ALTZONE_VAR
        if (dst)
          tzoffset = -altzone;
        else
          tzoffset = -timezone;
# endif
# ifdef USE_TM_GMTOFF_FIELD
        tzoffset = localTime->tm_gmtoff;
# endif
# ifdef USE_TZNAME_VAR
        tzn = MSC_IZE(tzname)[localTime->tm_isdst];
# elif defined(USE_TM_ZONE_FIELD)
        tzn = localTime->tm_zone;
# else
        tzn = NULL;
# endif
      } else
        tzn = "UTC";

# define TZN_STRDUP(s) strdup(s)
#endif

      if (!tzn)
        tzn = "?";

      result = malloc(sizeof(rktio_date_t));
      
      result->nanosecond = nanoseconds;
      result->second = sec;
      result->minute = min;
      result->hour = hour;
      result->day = day;
      result->month = month;
      result->year = year;
      result->day_of_week = wday;
      result->day_of_year = yday;
      result->is_dst = (dst ? 1 : 0);
      result->zone_offset = tzoffset;
      if (tzn)
        result->zone_name = TZN_STRDUP(tzn);
      else
        result->zone_name = NULL;

      return result;
    }
  }
  
  set_racket_error(RKTIO_ERROR_TIME_OUT_OF_RANGE);
  return NULL;
}
