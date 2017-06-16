
/* Standard settings for Unix platforms. */
/* Used by sconfig.h for known architectures. */

#define SYSTEM_TYPE_NAME "unix"
#define UNIX_FILE_SYSTEM

#define USE_C_SYSLOG

#define DO_STACK_CHECK
#ifndef ASSUME_FIXED_STACK_SIZE
# define UNIX_FIND_STACK_BOUNDS
#endif
#define UNIX_STACK_MAXIMUM 8388608

#define UNIX_DYNAMIC_LOAD

#ifndef MZ_USE_DETERMINSTIC_FUEL
# if defined(MZ_USE_PLACES) || defined(USE_PTHREAD_INSTEAD_OF_ITIMER)
#  define USE_PTHREAD_THREAD_TIMER
# else
#  define USE_ITIMER
# endif
#endif
