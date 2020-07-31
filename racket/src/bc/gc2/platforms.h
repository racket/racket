#ifdef _WIN32
# include <winsock2.h>
# include <windows.h>
# define bzero(m, s) memset(m, 0, s)
# ifndef __MINGW32__
#  define inline _inline
# endif
#endif

#include "mzconfig.h"

#ifdef SIZEOF_LONG
# if SIZEOF_LONG == 8
#  define SIXTY_FOUR_BIT_INTEGERS
# endif
#endif

#if defined(sparc) || defined(__sparc) || defined(__sparc__)
/* Required for `double' operations: */
# define GC_ALIGN_EIGHT
#endif

/* Even when 8-byte alginment is not required by the processor, it's
   better for floating-point performance (PowerPC) and may be required
   for some libraries (VecLib in Mac OS X, including x86).

   Under Windows, Mac OS X, and Linux x86_64, malloc() returns 16-byte
   aligned data. And, actually, VecLib says that it requires
   16-byte-aligned data. So, in those cases, GC_ALIGN_SIXTEEN might be
   better --- but that's a lot more expensive, increasing DrRacket's
   initial footprint by almost 10%. */
#ifndef GC_ALIGN_EIGHT
# define GC_ALIGN_EIGHT
#endif
