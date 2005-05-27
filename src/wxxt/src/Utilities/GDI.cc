/*								-*- C++ -*-
 *
 * Purpose: common GDI routines
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

#define  Uses_XLib
#include "wx.h"

extern int wx_visual_depth;

int wxDisplayDepth(void)
{
  return wx_visual_depth;
}

Bool wxColourDisplay(void)
{
    return (wxDisplayDepth() > 1);
}

void wxDisplaySize(int *width, int *height, int flags)
{
  if (wxTheApp) {
      *width = DisplayWidth(wxAPP_DISPLAY, DefaultScreen(wxAPP_DISPLAY));
      *height = DisplayHeight(wxAPP_DISPLAY, DefaultScreen(wxAPP_DISPLAY));
  } else {
      *width = 1024; // a good bet
      *height = 768;
  }
}

void wxDisplayOrigin(int *x, int *y)
{
  *x = *y = 0;
}
