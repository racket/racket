/*
  Racket
  Copyright (c) 2004-2015 PLT Design Inc.
  Copyright (c) 1995 Matthew Flatt
  All rights reserved.

  Please see the full copyright in the documentation.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

/* This file should be linked with any Racket extension dynamic
   object. */

#include "escheme.h"
#ifdef INCLUDE_WITHOUT_PATHS
# include "schvers.h"
#else
# include "../src/schvers.h"
#endif

#ifdef MZ_PRECISE_GC
# define PLAIN_OR_3M "@3m"
#else
# define PLAIN_OR_3M ""
#endif

#ifdef LINK_EXTENSIONS_BY_TABLE
Scheme_Extension_Table *scheme_extension_table;
#endif

#ifdef CODEFRAGMENT_DYNAMIC_LOAD
#pragma export on
char *scheme_initialize_internal(
#ifdef LINK_EXTENSIONS_BY_TABLE
				 Scheme_Extension_Table *table
#endif
				 );
#pragma export off
#endif

char *scheme_initialize_internal(
#ifdef LINK_EXTENSIONS_BY_TABLE
				 Scheme_Extension_Table *table
#endif
				 )
{
#ifdef LINK_EXTENSIONS_BY_TABLE
  scheme_extension_table = table;
#endif

  return MZSCHEME_VERSION PLAIN_OR_3M;
}
