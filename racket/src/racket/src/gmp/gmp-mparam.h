/* gmp-mparam.h -- Compiler/machine parameter header file.

Copyright (C) 1991, 1993, 1994 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA. */

#ifdef SIZEOF_LONG
# if SIZEOF_LONG == 8
#  define SIXTY_FOUR_BIT_INTEGERS
# endif
#endif

#if defined(SIXTY_FOUR_BIT_INTEGERS) || defined(_LONG_LONG_LIMB)
# define BITS_PER_MP_LIMB 64
# define BYTES_PER_MP_LIMB 8
#else
# define BITS_PER_MP_LIMB 32
# define BYTES_PER_MP_LIMB 4
#endif
