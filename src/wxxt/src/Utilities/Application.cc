/*								-*- C++ -*-
 *
 * Purpose: global utilities for wxWindows application and main loop
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 2004-2005 PLT Scheme, Inc.
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
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

#define  Uses_XtIntrinsic
#define  Uses_wxApp
#include "wx.h"

//-----------------------------------------------------------------------------
// exit the application with retvalue of OnExit
//-----------------------------------------------------------------------------

void wxExit(void)
{
  int v;
  v = wxTheApp->OnExit();
  exit(v);
}

//-----------------------------------------------------------------------------
// flush all events in the queue
//-----------------------------------------------------------------------------

void wxFlushEvents(void)
{
}

//-----------------------------------------------------------------------------
// yield to incomming messages
//-----------------------------------------------------------------------------

Bool wxYield(void)
{
  int ever = 0;
  int one_more;

  do {
    XFlush(wxAPP_DISPLAY);
    XSync(wxAPP_DISPLAY, FALSE);
    one_more = 0;
    while (wxTheApp->Pending()) {
      one_more = 1;
      ever = 1;
      wxTheApp->Dispatch();
    }
  } while (one_more);

  return ever;
}
