#include <stdio.h>
#include <signal.h>
#include <stdio.h>

/* Make sure a child process is started with normal properties, such
   as no blocked signals */

int main()
{
  sigset_t set, old_set;
  struct sigaction sa;
  int i;

  /* This would be nice to check, but tests might be run in a nohup
     environment, and it seems that signals get set ot ignored by
     shells for various reasons. */
#if 0
  /* SIGPROF tends to be near the end of the range of signal IDs */
  for (i = 1; i <= SIGPROF; i++) {
    if (sigaction(i, NULL, &sa) == 0) {
      if (sa.sa_handler != SIG_DFL) {
        printf("handler %d: %p\n", i, (void *)sa.sa_handler);
        return 1;
      }
    } else {
      /* sigaction sometimes isn't allowed at all on SIGKILL,
         for example, so ignore that failure */
    }
  }
#endif

  sigemptyset(&set);
  sigprocmask(SIG_BLOCK, &set, &old_set);

  if (sigismember(&old_set, SIGCHLD)) {
    printf("masked: SIGCHLD\n");
    return 1;
  }
  if (sigismember(&old_set, SIGINT)) {
    printf("masked: SIGINT\n");
    return 1;
  }
  if (sigismember(&old_set, SIGHUP)) {
    printf("masked: SIGHUP\n");
    return 1;
  }
  if (sigismember(&old_set, SIGQUIT)) {
    printf("masked: SIGQUIT\n");
    return 1;
  }

  return 0;
}
