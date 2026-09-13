/* This file is run through `cpp` by "../mksystem.rkt"
   in cross-compilation mode. */

#include "schpriv.h"
#include "systype.inc"
#ifndef SCHEME_PLATFORM_LIBRARY_SUBPATH
# include "schsys.h"
#endif
#ifndef SPLS_SUFFIX
# define SPLS_SUFFIX ""
#endif

string system_type_os = SYSTEM_TYPE_NAME;
string system_type_os_star = SCHEME_OS;
string system_type_arch = SCHEME_ARCH;
string system_type_link = MZ_SYSTEM_TYPE_LINK;
string system_type_so_suffix = MZ_SYSTEM_TYPE_SO_SUFFIX;
string system_type_so_mode = MZ_SYSTEM_TYPE_SO_MODE;
string system_library_subpath = SCHEME_PLATFORM_LIBRARY_SUBPATH SPLS_SUFFIX;
string system_library_subpath_suffix = SPLS_SUFFIX;

int system_pointer_size = SIZEOF_VOID_P;
