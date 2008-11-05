
/******************************************************************************/
/*                     OS-specific low-level allocator                        */
/******************************************************************************/

#ifndef GCPRINT
# define GCPRINT fprintf
# define GCOUTF stderr
#endif

#if _WIN32            /* Windows */
# include "vm_win.c"
#elif defined(OSKIT)  /* OSKit */
# include "vm_osk.c"
#elif defined(OS_X)   /* OS X */
# include "vm_osx.c"
#else                 /* Default: mmap */
# include "vm_mmap.c"
#endif
