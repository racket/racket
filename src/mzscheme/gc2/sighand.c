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
# include <linux/version.h>
# if LINUX_VERSION_CODE >= KERNEL_VERSION(2,4,0)
/* New linux */
void fault_handler(int sn, struct siginfo *si, void *ctx)
{
  designate_modified(si->si_addr);
#  define NEED_SIGACTION
#  define USE_SIGACTON_SIGNAL_KIND SIGSEGV
}
#else
/* Old linux */
void fault_handler(int sn, struct sigcontext sc)
{
#  if (defined(powerpc) || defined(__powerpc__))
   /* PowerPC */
   designate_modified((void *)sc.regs->dar);
#  else
   /* x86 */
   designate_modified((void *)sc.cr2);
#  endif
   signal(SIGSEGV, (void (*)(int))fault_handler);
#  define NEED_SIGSEGV
}
# endif
#endif

/* ========== FreeBSD signal handler ========== */
#if defined(__FreeBSD__)
# include <signal.h>
void fault_handler(int sn, int code, struct sigcontext *sc, char *addr)
{
  designate_modified(addr);
}
# define NEED_SIGBUS
#endif

/* ========== Solaris signal handler ========== */
#if defined(sun)
# include <signal.h>
void fault_handler(int sn, struct siginfo *si, void *ctx)
{
  designate_modified(si->si_addr);
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
    designate_modified((void *)e->ExceptionRecord->ExceptionInformation[1]);

    return EXCEPTION_CONTINUE_EXECUTION;
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

/******************************************************************************/
/*                             init function                                  */
/******************************************************************************/

static void initialize_signal_handler()
{
# ifdef NEED_SIGSEGV
  signal(SIGSEGV, (void (*)(int))fault_handler);
# endif
# ifdef NEED_SIGBUS
  signal(SIGBUS, (void (*)(int))fault_handler);
# endif
# ifdef NEED_OSX_MACH_HANDLER
  macosx_init_exception_handler();
# endif
# ifdef NEED_SIGACTION
  {
    struct sigaction act, oact;
    memset(&act, sizeof(sigaction), 0);
    act.sa_sigaction = fault_handler;
    sigemptyset(&act.sa_mask);
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
