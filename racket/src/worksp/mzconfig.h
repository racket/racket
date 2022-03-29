
/* This file contains information for Windows that is collected using the
 * "configure" script on other platforms.  See src/racket/mzconfig.h.in for
 * things that should be defined here.
 */

#ifndef __MZSCHEME_CONFIGURATION_INFO__
#define __MZSCHEME_CONFIGURATION_INFO__


/* Undefined items are not used on Windows. */

/* The size of a `int', as computed by sizeof. */
#undef SIZEOF_INT

/* The size of a `short', as computed by sizeof. */
#undef SIZEOF_SHORT

/* The size of a `long', as computed by sizeof. */
#undef SIZEOF_LONG

/* The size of a `long long', as computed by sizeof. */
#undef SIZEOF_LONG_LONG

#ifdef _WIN64
# define SIZEOF_VOID_P 8
#endif

/* Direction of stack growth: 1 = up, -1 = down, 0 = unknown */
#define STACK_DIRECTION -1

/* whether nl_langinfo works */
#undef HAVE_CODESET

/* whether getaddrinfo works */
#define HAVE_GETADDRINFO 1

/* Enable futures: */
#ifndef _M_ARM64
# define MZ_USE_FUTURES
#endif

/* Enable places --- 3m only: */
#ifdef MZ_PRECISE_GC
# ifndef _M_ARM64
#  define MZ_USE_PLACES
# endif
#endif

/* Enable single-precision floats: */
#define USE_SINGLE_FLOATS

/* Always enable generations in GC */
#define USE_GC_GENS 1

/* Use terminal support from Chez Scheme: */
#define MZ_TERMINAL_SUPPORT 1

/* Might define COMPILED_PATH_AS_BC */
#include "bc_suffix.h"

#endif
