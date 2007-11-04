/* 
   Provides:
      initialize_signal_handler();
   Requires:
      generations_available - mutable int, Windows only
      designate_modified
      macosx_init_exception_handler() --- OS X, only
*/

/******************************************************************************/
/*                      platform-specific handlers                            */
/******************************************************************************/

/* ========== Linux signal handler ========== */
#if defined(linux)
# include <signal.h>
void fault_handler(int sn, struct siginfo *si, void *ctx)
{
  if (!designate_modified(si->si_addr))
    abort();
#  define NEED_SIGACTION
#  define USE_SIGACTON_SIGNAL_KIND SIGSEGV
}
#endif

/* ========== FreeBSD/NetBSD/OpenBSD signal handler ========== */
/*  As of 2007/06/29, this is a guess for NetBSD!  */
#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
# include <signal.h>
# include <sys/param.h>
void fault_handler(int sn, siginfo_t *si, void *ctx)
{
  if (!designate_modified(si->si_addr))
    abort();
}
#  define NEED_SIGACTION
#  if defined(__FreeBSD__) && (__FreeBSD_version < 700000)
#    define USE_SIGACTON_SIGNAL_KIND SIGBUS
#  else
#    define USE_SIGACTON_SIGNAL_KIND SIGSEGV
#  endif
#endif

/* ========== Solaris signal handler ========== */
#if defined(sun)
# include <signal.h>
void fault_handler(int sn, struct siginfo *si, void *ctx)
{
  if (!designate_modified(si->si_addr))
    abort();
}
# define NEED_SIGACTION
# define USE_SIGACTON_SIGNAL_KIND SIGSEGV
#endif

/* ========== Windows signal handler ========== */
#if defined(_WIN32)
LONG WINAPI fault_handler(LPEXCEPTION_POINTERS e) 
{
  if ((e->ExceptionRecord->ExceptionCode == EXCEPTION_ACCESS_VIOLATION)
      && (e->ExceptionRecord->ExceptionInformation[0] == 1)) {
    if (designate_modified((void *)e->ExceptionRecord->ExceptionInformation[1]))
      return EXCEPTION_CONTINUE_EXECUTION;
    else
      return EXCEPTION_CONTINUE_SEARCH;
  } else
    return EXCEPTION_CONTINUE_SEARCH;
}
# define NEED_SIGWIN
typedef LONG (WINAPI*gcPVECTORED_EXCEPTION_HANDLER)(LPEXCEPTION_POINTERS e);
#endif

/* ========== Mac OS X signal handler ========== */
#if defined(OS_X)
/* Normally supplied by vm_osx.c: */
# define NEED_OSX_MACH_HANDLER
#endif

/* ========== Generic Unix signal handler ========== */
/* There's little guarantee that this will work, since
   Unix variants differ in the types of the arguments.
   When a platform doesn't match, make a special case
   for it, like all the others above. */
#if !defined(NEED_SIGACTION) && !defined(NEED_SIGWIN) && !defined(NEED_OSX_MACH_HANDLER)
# include <signal.h>
void fault_handler(int sn, siginfo_t *si, void *ctx)
{
  if (!designate_modified(si->si_addr))
    abort();
#  define NEED_SIGACTION
#  define USE_SIGACTON_SIGNAL_KIND SIGSEGV
}
#endif

/******************************************************************************/
/*                             init function                                  */
/******************************************************************************/

static void initialize_signal_handler()
{
# ifdef NEED_OSX_MACH_HANDLER
  macosx_init_exception_handler();
# endif
# ifdef NEED_SIGACTION
  {
    struct sigaction act, oact;
    memset(&act, 0, sizeof(sigaction));
    act.sa_sigaction = fault_handler;
    sigemptyset(&act.sa_mask);
    /* In MzScheme, SIGCHLD or SIGINT handling may trigger a write barrier: */
    sigaddset(&act.sa_mask, SIGINT);
    sigaddset(&act.sa_mask, SIGCHLD);
    act.sa_flags = SA_SIGINFO;
    sigaction(USE_SIGACTON_SIGNAL_KIND, &act, &oact);
  }
# endif
# ifdef NEED_SIGWIN
  {
    HMODULE hm;
    PVOID (WINAPI*aveh)(ULONG, gcPVECTORED_EXCEPTION_HANDLER);

    hm = LoadLibrary("kernel32.dll");
    if (hm)
      aveh = (PVOID (WINAPI*)(ULONG, gcPVECTORED_EXCEPTION_HANDLER))GetProcAddress(hm, "AddVectoredExceptionHandler");
    else
      aveh = NULL;
    if (aveh)
      aveh(TRUE, fault_handler);
    else
      generations_available = 0;
  }
# endif
}
