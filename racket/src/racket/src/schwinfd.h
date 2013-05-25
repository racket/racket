
#if defined(WINDOWS_PROCESSES) || defined(DETECT_WIN32_CONSOLE_STDIN) || defined(USE_BEOS_PORT_THREADS)
# ifndef NO_STDIO_THREADS
typedef struct {
  /* All fields must be pointers */

  SOCKET *sockets;

  Scheme_Object *added; /* fixnum */
  Scheme_Object *alloc; /* fixnum */

  Scheme_Object *num_handles; /* fixnum */
  OS_SEMAPHORE_TYPE *handles;

  int *repost_sema;

  Scheme_Object *no_sleep; /* boolean */

  Scheme_Object *wait_event_mask; /* fixnum */

  HANDLE *wait_array;

  HANDLE *combined_wait_array;
  Scheme_Object *combined_len; /* fixnum */
} win_extended_fd_set;
# endif
#endif
