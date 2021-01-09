#include "rktio.h"
#include "rktio_private.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#if defined(RKTIO_SYSTEM_UNIX)
# include <signal.h>
#endif

static int handlers_installed = 0;
static rktio_t *signal_rktio = NULL;

/*========================================================================*/
/* Signal handlers                                                        */
/*========================================================================*/

/* Called by signal handlers: */
static void signal_received(int i)
{
  if (signal_rktio) {
    signal_rktio->pending_os_signals[i] = 1;
    rktio_signal_received(signal_rktio);
  }
}

#if defined(RKTIO_SYSTEM_WINDOWS)
static BOOL WINAPI ConsoleBreakHandler(DWORD op)
{
  rktio_console_ctl_c();
  signal_received(RKTIO_OS_SIGNAL_INT);
  return TRUE;
}

void rktio_set_console_handler(void)
{
  SetConsoleCtrlHandler(ConsoleBreakHandler, TRUE);
}
#endif

#if defined(RKTIO_SYSTEM_UNIX)
static void user_break_hit(int ignore) {
  signal_received(RKTIO_OS_SIGNAL_INT);
}

static void term_hit(int ignore)
{
  signal_received(RKTIO_OS_SIGNAL_TERM);
}

static void hup_hit(int ignore)
{
  signal_received(RKTIO_OS_SIGNAL_HUP);
}
#endif

/*========================================================================*/
/* Manage signal handlers                                                 */
/*========================================================================*/

void rktio_install_os_signal_handler(rktio_t *rktio)
{
  signal_rktio = rktio;
  if (handlers_installed)
    return;

  handlers_installed = 1;

#if defined(RKTIO_SYSTEM_UNIX)
  rktio_set_signal_handler(SIGINT, user_break_hit);
  rktio_set_signal_handler(SIGTERM, term_hit);
  rktio_set_signal_handler(SIGHUP, hup_hit);
#endif

#if defined(RKTIO_SYSTEM_WINDOWS)
  rktio_set_console_handler();
#endif
}

void rktio_forget_os_signal_handler(rktio_t *rktio)
{
  if (signal_rktio == rktio) {
    signal_rktio = NULL;
  }
}

int rktio_poll_os_signal(rktio_t *rktio)
{
  int i;
  for (i = 0; i < RKTIO_NUM_OS_SIGNALS; i++) {
    if (rktio->pending_os_signals[i]) {
      rktio->pending_os_signals[i] = 0;
      return i;
    }
  }

  return RKTIO_OS_SIGNAL_NONE;
}

#if defined(RKTIO_SYSTEM_UNIX)
void rktio_set_signal_handler(int sig_id, void (*proc)(int))
{
  struct sigaction sa;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = 0;
  sa.sa_handler = proc;
  sigaction(sig_id, &sa, NULL);
}

void rktio_set_default_signal_handlers_for_subprocess()
{
  /* This is a list of all signals potentially used by Racket. */
# ifdef SIGBUS
  rktio_set_signal_handler(SIGBUS, SIG_DFL);
# endif
# ifdef SIGCHLD
  rktio_set_signal_handler(SIGCHLD, SIG_DFL);
# endif
# ifdef SIGFPE
  rktio_set_signal_handler(SIGFPE, SIG_DFL);
# endif
# ifdef SIGHUP
  rktio_set_signal_handler(SIGHUP, SIG_DFL);
# endif
# ifdef SIGILL
  rktio_set_signal_handler(SIGILL, SIG_DFL);
# endif
# ifdef SIGINFO
  rktio_set_signal_handler(SIGINFO, SIG_DFL);
# endif
# ifdef SIGINT
  rktio_set_signal_handler(SIGINT, SIG_DFL);
# endif
# ifdef SIGIO
  rktio_set_signal_handler(SIGIO, SIG_DFL);
# endif
# ifdef SIGPIPE
  rktio_set_signal_handler(SIGPIPE, SIG_DFL);
# endif
# ifdef SIGPROF
  rktio_set_signal_handler(SIGPROF, SIG_DFL);
# endif
# ifdef SIGQUIT
  rktio_set_signal_handler(SIGQUIT, SIG_DFL);
# endif
# ifdef SIGSEGV
  rktio_set_signal_handler(SIGSEGV, SIG_DFL);
# endif
# ifdef SIGTERM
  rktio_set_signal_handler(SIGTERM, SIG_DFL);
# endif
# ifdef SIGTSTP
  rktio_set_signal_handler(SIGTSTP, SIG_DFL);
# endif
# ifdef SIGUSR1
  rktio_set_signal_handler(SIGUSR1, SIG_DFL);
# endif
# ifdef SIGUSR2
  rktio_set_signal_handler(SIGUSR2, SIG_DFL);
# endif
# ifdef SIGWINCH
  rktio_set_signal_handler(SIGWINCH, SIG_DFL);
# endif
}
#endif
