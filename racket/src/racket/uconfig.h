
/* Standard settings for Unix platforms. */
/* Used by sconfig.h for known architectures. */

#define SYSTEM_TYPE_NAME "unix"
#define UNIX_FILE_SYSTEM

#define TIME_SYNTAX
#define PROCESS_FUNCTION
#define DIR_FUNCTION
#define GETENV_FUNCTION

#define USE_FD_PORTS
#define HAS_STANDARD_IOB
#define FILES_HAVE_FDS
#define USE_UNIX_SOCKETS_TCP
#define USE_FLOCK_FOR_FILE_LOCKS

#define UNIX_PROCESSES
#define CLOSE_ALL_FDS_AFTER_FORK

#define USE_C_SYSLOG

#define EXPAND_FILENAME_TILDE

#define DO_STACK_CHECK
#ifndef ASSUME_FIXED_STACK_SIZE
# define UNIX_FIND_STACK_BOUNDS
#endif
#define UNIX_STACK_MAXIMUM 8388608

#define UNIX_DYNAMIC_LOAD

#define UNISTD_INCLUDE
#define USE_FCHDIR

#define USE_GETRUSAGE

#ifndef MZ_USE_DETERMINSTIC_FUEL
# if defined(MZ_USE_PLACES) || defined(USE_PTHREAD_INSTEAD_OF_ITIMER)
#  define USE_PTHREAD_THREAD_TIMER
# else
#  define USE_ITIMER
# endif
#endif
