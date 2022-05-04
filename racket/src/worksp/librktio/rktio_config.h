#define RKTIO_SYSTEM_WINDOWS

#include <stddef.h>

#if defined(_MSC_VER)
typedef _int64 rktio_int64_t;
typedef unsigned _int64 rktio_uint64_t;
#else
typedef long long rktio_int64_t;
typedef unsigned long long rktio_uint64_t;
#endif

/* whether getaddrinfo works */
#define HAVE_GETADDRINFO 1
