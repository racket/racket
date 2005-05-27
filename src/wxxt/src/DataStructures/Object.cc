/*								-*- C++ -*-
 *
 * Purpose: Top level object and memory debugging for wxWindows
 *
 * Authors: Markus Holzem, Julian Smart and Arthur Seaton
 *
 * Copyright: (C) 2004-2005 PLT Scheme, Inc.
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian, Arthur)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifdef __GNUG__
#pragma implementation "Object.h"
#endif

#define Uses_wxDebugStreamBuf
#define Uses_wxObject
#define Uses_wxHashTable
#include "wx.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

//-----------------------------------------------------------------------------
// wxObject: top level object
//-----------------------------------------------------------------------------

int wx_object_count;

wxObject::wxObject(void)
{
  __type = wxTYPE_ANY;

  wx_object_count++;
}

wxObject::wxObject(Bool cleanup) : gc_cleanup((int)cleanup)
{
  __type = wxTYPE_ANY;

  wx_object_count++;
}

wxObject::~wxObject(void)
{
  if (__type < 0) {
    printf("bad!\n");
  }

  --wx_object_count;
  __type = -1;
}

#ifdef MEMORY_USE_METHOD
long wxObject::MemoryUse(void)
{
  return 0;
}
#endif
