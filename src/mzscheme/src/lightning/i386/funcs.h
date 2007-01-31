/******************************** -*- C -*- ****************************
 *
 *	Platform-independent layer inline functions (i386)
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright 2000, 2001, 2002 Free Software Foundation, Inc.
 * Written by Paolo Bonzini.
 *
 * This file is part of GNU lightning.
 *
 * GNU lightning is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1, or (at your option)
 * any later version.
 * 
 * GNU lightning is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with GNU lightning; see the file COPYING.LESSER; if not, write to the
 * Free Software Foundation, 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA.
 *
 ***********************************************************************/



#ifndef __lightning_funcs_h
#define __lightning_funcs_h

#ifdef MZ_JIT_USE_MPROTECT
# include <unistd.h>
# include <sys/mman.h>
#endif
#ifdef MZ_JIT_USE_WINDOWS_VIRTUAL_ALLOC
# include <windows.h>
#endif

#if defined(MZ_JIT_USE_MPROTECT) || defined(MZ_JIT_USE_WINDOWS_VIRTUAL_ALLOC)
static unsigned long jit_prev_page = 0, jit_prev_length = 0;
#endif

static void
jit_notify_freed_code(void)
{
#if defined(MZ_JIT_USE_MPROTECT) || defined(MZ_JIT_USE_WINDOWS_VIRTUAL_ALLOC)
  jit_prev_page = jit_prev_length = 0;
#endif
}

static void
jit_flush_code(void *dest, void *end)
{
  /* On the x86, the PROT_EXEC bits are not handled by the MMU.
     However, the kernel can emulate this by setting the code
     segment's limit to the end address of the highest page
     whose PROT_EXEC bit is set.

     Linux kernels that do so and that disable by default the
     execution of the data and stack segment are becoming more
     and more common (Fedora, for example), so we implement our
     jit_flush_code as an mprotect.  */
#if defined(MZ_JIT_USE_MPROTECT) || defined(MZ_JIT_USE_WINDOWS_VIRTUAL_ALLOC)
  unsigned long page, length;
# ifdef PAGESIZE
  const long page_size = PAGESIZE;
# else
  static unsigned long page_size = -1;
  if (page_size == -1) {
#  ifdef MZ_JIT_USE_WINDOWS_VIRTUAL_ALLOC
    SYSTEM_INFO info;
    GetSystemInfo(&info);
    page_size = info.dwPageSize;
#  else
    page_size = sysconf (_SC_PAGESIZE);
#  endif
  }
# endif

  page = (long) dest & ~(page_size - 1);
  length = ((char *) end - (char *) page + page_size - 1) & ~(page_size - 1);

  /* Simple-minded attempt at optimizing the common case where a single
     chunk of memory is used to compile multiple functions.  */
  if (page >= jit_prev_page && page + length <= jit_prev_page + jit_prev_length)
    return;

# ifdef MZ_JIT_USE_WINDOWS_VIRTUAL_ALLOC
  {
    DWORD old;
    VirtualProtect((void *)page, length, PAGE_EXECUTE_READWRITE, &old);
  }
# else
  mprotect ((void *) page, length, PROT_READ | PROT_WRITE | PROT_EXEC);
# endif

  /* See if we can extend the previously mprotect'ed memory area towards
     higher addresses: the starting address remains the same as before.  */
  if (page >= jit_prev_page && page <= jit_prev_page + jit_prev_length)
    jit_prev_length = page + length - jit_prev_page;

  /* See if we can extend the previously mprotect'ed memory area towards
     lower addresses: the highest address remains the same as before. */
  else if (page < jit_prev_page && page + length >= jit_prev_page 
	   && page + length <= jit_prev_page + jit_prev_length)
    jit_prev_length += jit_prev_page - page, jit_prev_page = page;

  /* Nothing to do, replace the area.  */
  else
    jit_prev_page = page, jit_prev_length = length;
#endif
}

#endif /* __lightning_funcs_h */
