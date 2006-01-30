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

#ifdef __linux__
#include <unistd.h>
#include <sys/mman.h>
#endif

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
#ifdef __linux__
  static unsigned long prev_page = 0, prev_length = 0;
  int page, length;
#ifdef PAGESIZE
  const int page_size = PAGESIZE;
#else
  static int page_size = -1;
  if (page_size == -1)
    page_size = sysconf (_SC_PAGESIZE);
#endif

  page = (long) dest & ~(page_size - 1);
  length = ((char *) end - (char *) page + page_size - 1) & ~(page_size - 1);

  /* Simple-minded attempt at optimizing the common case where a single
     chunk of memory is used to compile multiple functions.  */
  if (page >= prev_page && page + length <= prev_page + prev_length)
    return;

  mprotect ((void *) page, length, PROT_READ | PROT_WRITE | PROT_EXEC);

  /* See if we can extend the previously mprotect'ed memory area towards
     higher addresses: the starting address remains the same as before.  */
  if (page >= prev_page && page <= prev_page + prev_length)
    prev_length = page + length - prev_page;

  /* See if we can extend the previously mprotect'ed memory area towards
     lower addresses: the highest address remains the same as before.  */
  else if (page < prev_page && page + length <= prev_page + prev_length)
    prev_length += prev_page - page, prev_page = page;

  /* Nothing to do, replace the area.  */
  else
    prev_page = page, prev_length = length;
#endif
}

#endif /* __lightning_funcs_h */
