#include <stdio.h>
#include <signal.h>

/* Make sure a child process is started with normal properties, such
   as no blocked signals */

int main()
{
  sigset_t set, old_set;
  struct sigaction sa;
  int i;

  /* SIGPROF tends to be near the end of the range of signal IDs */
  for (i = 0; i < SIGPROF; i++) {
    sigaction(i, NULL, &sa);
    if (sa.sa_handler != SIG_DFL)
      return 1;
  }

  sigemptyset(&set);
  sigprocmask(SIG_BLOCK, &set, &old_set);

  return (sigismember(&old_set, SIGCHLD)
          || sigismember(&old_set, SIGINT)
          || sigismember(&old_set, SIGHUP)
          || sigismember(&old_set, SIGQUIT));
}
