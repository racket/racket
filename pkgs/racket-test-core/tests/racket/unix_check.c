#include <stdio.h>
#include <signal.h>

/* Make sure a child process is started with normal properties, such
   as no blocked signals */

int main()
{
  sigset_t set, old_set;
  sigemptyset(&set);
  sigprocmask(SIG_BLOCK, &set, &old_set);

  return (sigismember(&old_set, SIGCHLD)
          || sigismember(&old_set, SIGINT)
          || sigismember(&old_set, SIGHUP)
          || sigismember(&old_set, SIGQUIT));
}
