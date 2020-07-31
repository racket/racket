
/* Some answers for `system-type`, also consulted for cross-compilation */

#if defined(OS_X) && !defined(XONX)
# define MZ_SYSTEM_TYPE_LINK "framework"
#else
# ifdef DOS_FILE_SYSTEM
#  define MZ_SYSTEM_TYPE_LINK "dll"
# else
#  ifdef MZ_USES_SHARED_LIB
#   define MZ_SYSTEM_TYPE_LINK "shared"
#  else
#   define MZ_SYSTEM_TYPE_LINK "static"
#  endif
# endif
#endif

#ifdef DOS_FILE_SYSTEM
# define MZ_SYSTEM_TYPE_SO_SUFFIX ".dll"
#else
# ifdef OS_X
#  define MZ_SYSTEM_TYPE_SO_SUFFIX ".dylib"
# else
#  ifdef USE_CYGWIN_SO_SUFFIX
#   define MZ_SYSTEM_TYPE_SO_SUFFIX ".dll"
#  else
#   define MZ_SYSTEM_TYPE_SO_SUFFIX ".so"
#  endif
# endif
#endif


#ifdef USE_DLOPEN_GLOBAL_BY_DEFAULT
# define MZ_SYSTEM_TYPE_SO_MODE "global"
#else
# define MZ_SYSTEM_TYPE_SO_MODE "local"
#endif
