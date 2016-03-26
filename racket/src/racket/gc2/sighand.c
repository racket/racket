/* 
   Provides:
      initialize_signal_handler(GCTYPE *gc)
      remove_signal_handler();
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
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>

#ifndef WAIT_FOR_GDB
# define WAIT_FOR_GDB 0
#endif

#if WAIT_FOR_GDB
void launchgdb() {
  {
    pid_t pid = getpid();
    fprintf(stderr, "pid # %i run gdb\nsudo gdb ./racket3m %i\nor kill process.\n", pid, pid);
    fflush(stderr);
  }

  pid_t fpid = fork();
  if (fpid == 0)                /* child */
  {
    char _pidstr[20];
    char *pidstr = (char*) _pidstr;
    snprintf(pidstr, 20, "%i", getpid());
    execl("/usr/bin/bash", "rgdb", pidstr);
  }
  else if (fpid < 0)            /* failed to fork */
  {
    printf("Failed to fork\n");
    exit(1);
  }
  else                                   /* parent */
  {
    kill(getpid(), SIGSTOP);
  }

  kill(getpid(), SIGSTOP);
  {
    char inbuffer[10];
    while(read(fileno(stdin), inbuffer, 10) <= 0){
      if(errno != EINTR){
        fprintf(stderr, "Error detected %i\n", errno);
      }
    }
  }
}
#endif

void fault_handler(int sn, siginfo_t *si, void *ctx)
{
  void *p = si->si_addr;
  /* quick access to SIGSEGV info in GDB */
  int c = si->si_code;
#ifdef MZ_USE_PLACES
  int m = 0;
#endif
  if (si->si_code != SEGV_ACCERR) { /*SEGV_MAPERR*/
    if (c == SEGV_MAPERR) {
      printf("SIGSEGV MAPERR si_code %i fault on addr %p\n", c, p);
      /* SIGSEGV MAPERRs are invalid addresses. Possible reasons:
         An object is prematurely freed because it isn't getting marked correctly
         An unsafe operation was used incorrectly
         The stack grew beyond its bounds.
      */
    }
    if (c == 0) {
      /* When running w/ places in gdb, the debugger
         sometimes propagates extra copies of signals
         that crash the process. Ignore them (but they're
         rare enough that it's worth reporting that the signal
         was received). */
      printf("Signal as SI_USER (from debugger?) - ignoring\n");
      return;
    }
    if (c == 128) {
      /* A mysterious signal on Linux, probably the OS providing some
         kind of alert. These can be frequent enough that printing
         an alert is too noisy. */
      if (0) {
        printf("Signal as SI_KERNEL - ignoring\n");
      }
      return;
    }
#if WAIT_FOR_GDB
    launchgdb();
#endif
    abort();
  }

  if (!designate_modified(p)) {
    if (si->si_code == SEGV_ACCERR) {
#ifdef MZ_USE_PLACES
      if(pagemap_find_page(MASTERGC->page_maps, p)) {
        m = 1;
        printf("ADDR %p OWNED BY MASTER %i\n", p, m);
      }
#endif
      printf("SIGSEGV SEGV_ACCERR SI_CODE %i fault on %p\n", c, p);
    }
    else {
      printf("SIGSEGV ???? SI_CODE %i fault on %p\n", c, p);
    }
#if WAIT_FOR_GDB
    launchgdb();
#endif
    abort();
  }
#  define NEED_SIGSTACK
#  define NEED_SIGACTION
#  define USE_SIGACTON_SIGNAL_KIND SIGSEGV
}
#endif

/* ========== FreeBSD/NetBSD/OpenBSD signal handler ========== */
/*  As of 2007/06/29, this is a guess for NetBSD!  */
#if defined(__FreeBSD__) \
 || defined(__FreeBSD_kernel__) \
 || defined(__NetBSD__) \
 || defined(__OpenBSD__) \
 || defined(__DragonFly__)
# include <signal.h>
# include <sys/param.h>
void fault_handler(int sn, siginfo_t *si, void *ctx)
{
  if (!designate_modified(si->si_addr))
    abort();
}
#  define NEED_SIGACTION
#  if (defined(__FreeBSD__) && (__FreeBSD_version < 700000))
#    define USE_SIGACTON_SIGNAL_KIND SIGBUS
#  else
#    define USE_SIGACTON_SIGNAL_KIND SIGSEGV
#    ifdef __FreeBSD_kernel__
       /* Some versions of the kFreeBSD C library use SIGBUS */
#      define USE_ANOTHER_SIGACTON_SIGNAL_KIND SIGBUS
#    endif
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
#if defined(_WIN32) || defined(__CYGWIN32__)
# if defined(__CYGWIN32__)
#  include <windows.h>
# endif
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

static void initialize_signal_handler(GCTYPE *gc)
{
# ifdef NEED_OSX_MACH_HANDLER
#  if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  macosx_init_exception_handler(MASTERGC == 0);
#  else
  macosx_init_exception_handler(1);
#  endif
# endif
# ifdef NEED_SIGSTACK
  {
    stack_t ss;
    uintptr_t sz = 10*SIGSTKSZ;
    
    ss.ss_sp = malloc(sz);
    ss.ss_size = sz;
    ss.ss_flags = 0;
    
    sigaltstack(&ss, NULL);
  }
# endif
# ifdef NEED_SIGACTION
  {
    struct sigaction act, oact;
    memset(&act, 0, sizeof(act));
    act.sa_sigaction = fault_handler;
    sigemptyset(&act.sa_mask);
    /* In Racket, SIGCHLD or SIGINT handling may trigger a write barrier: */
    sigaddset(&act.sa_mask, SIGINT);
    sigaddset(&act.sa_mask, SIGCHLD);
    act.sa_flags = SA_SIGINFO;
#  ifdef NEED_SIGSTACK
    act.sa_flags |= SA_ONSTACK;
#  endif
    sigaction(USE_SIGACTON_SIGNAL_KIND, &act, &oact);
#  ifdef USE_ANOTHER_SIGACTON_SIGNAL_KIND
    sigaction(USE_ANOTHER_SIGACTON_SIGNAL_KIND, &act, &oact); 
#  endif
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
    else  /* older than Windows XP */
      gc->generations_available = 0;
  }
# endif
}

static void remove_signal_handler(GCTYPE *gc)
{
# ifdef NEED_OSX_MACH_HANDLER
# endif
# ifdef NEED_SIGACTION
  {
    struct sigaction act, oact;
    memset(&act, 0, sizeof(act));
    act.sa_handler = SIG_DFL;
    sigemptyset(&act.sa_mask);
    act.sa_flags = SA_SIGINFO;
    sigaction(USE_SIGACTON_SIGNAL_KIND, &act, &oact);
#  ifdef USE_ANOTHER_SIGACTON_SIGNAL_KIND
    sigaction(USE_ANOTHER_SIGACTON_SIGNAL_KIND, &act, &oact); 
#  endif
  }
# endif
# ifdef NEED_SIGWIN
  if (gc->generations_available) {
    HMODULE hm;

    hm = LoadLibrary("kernel32.dll");
    if (hm) {
      ULONG (WINAPI*rveh)(gcPVECTORED_EXCEPTION_HANDLER);
      rveh = (ULONG (WINAPI*)(gcPVECTORED_EXCEPTION_HANDLER))GetProcAddress(hm, "RemoveVectoredExceptionHandler");
      rveh(fault_handler);
    }
  }
# endif
}
