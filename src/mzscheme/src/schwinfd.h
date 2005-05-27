
#if defined(WINDOWS_PROCESSES) || defined(DETECT_WIN32_CONSOLE_STDIN) || defined(USE_BEOS_PORT_THREADS)
# ifndef NO_STDIO_THREADS
typedef struct {
  fd_set set;

  int added;

  int num_handles;
  OS_SEMAPHORE_TYPE *handles;

  int *repost_sema;

  int no_sleep;

  int wait_event_mask;
} win_extended_fd_set;
# endif
#endif
