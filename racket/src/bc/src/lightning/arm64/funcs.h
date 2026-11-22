/******************************** -*- C -*- ****************************
 *
 *	Platform-independent layer inline functions (AArch64)
 *
 ***********************************************************************/

/***********************************************************************
 *
 * Copyright 2011, 2026 Free Software Foundation, Inc.
 *
 * This file is part of GNU lightning.
 *
 * GNU lightning is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * GNU lightning is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with GNU lightning; see the file COPYING.LESSER; if not, write to the
 * Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 * Authors:
 *	Paulo Cesar Pereira de Andrade
 ***********************************************************************/

#ifndef __lightning_funcs_h
#define __lightning_funcs_h

#include <stdlib.h>
#ifndef _WIN32
# include <unistd.h>
#endif

#if defined(__APPLE__)
# include <libkern/OSCacheControl.h>
#elif defined(_WIN32)
# include <windows.h>
#endif

#define jit_notify_freed_code() /* empty */

__jit_inline void
jit_flush_code(void *start, void *end)
{
  if (start < end) {
#if defined(__APPLE__)
    sys_icache_invalidate(start, (char *)end - (char *)start);
#elif defined(_WIN32)
    FlushInstructionCache(GetCurrentProcess(), start, (char *)end - (char *)start);
#else
    __builtin___clear_cache((char *)start, (char *)end);
#endif
  }
}

#endif /* __lightning_funcs_h */
