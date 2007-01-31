
/******************************************************************************/
/*                     OS-specific low-level allocator                        */
/******************************************************************************/

/******************************************************************************/
/* Windows */

#if _WIN32
# include "vm_win.c"
# define MALLOCATOR_DEFINED
#endif

/******************************************************************************/
/* OSKit */

#if OSKIT
# include "vm_osk.c"
# define MALLOCATOR_DEFINED
#endif

/******************************************************************************/
/* OS X */

#if defined(OS_X)
# if GENERATIONS
static int designate_modified(void *p);
# endif

# define TEST 0
# include "vm_osx.c"

# define MALLOCATOR_DEFINED
#endif

/******************************************************************************/
/* Default: mmap */

#ifndef MALLOCATOR_DEFINED
# include "vm_mmap.c"
#endif
