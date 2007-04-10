/*
  MzScheme
  Copyright (c) 2004-2007 PLT Scheme Inc.
  Copyright (c) 1995-2001 Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301 USA.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

/* Images are long since unsupported, so all that's left is this
   little trampoline. */

#include "schpriv.h"

MZ_DLLSPEC int (*scheme_actual_main)(int argc, char **argv);

void scheme_set_actual_main(int (*m)(int argc, char **argv))
{
  scheme_actual_main = m;
}

int scheme_image_main(int argc, char **argv)
{
  return scheme_actual_main(argc, argv);
}

void scheme_no_dumps(char *why)
{
}
