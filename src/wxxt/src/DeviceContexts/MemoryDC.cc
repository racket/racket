/*								-*- C++ -*-
 *
 * Purpose: device context to draw into wxBitmaps
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

#ifdef __GNUG__
#pragma implementation "MemoryDC.h"
#endif

#define  Uses_XLib
#define  Uses_wxBitmap
#define  Uses_wxMemoryDC
#include "wx.h"

wxMemoryDC::wxMemoryDC(Bool ro) : wxCanvasDC()
{
    __type = wxTYPE_DC_MEMORY;

    device = wxDEVICE_MEMORY;

    read_only = ro;

    // the memory DC is at construction a unusable class because
    // there is no drawable. The initialization will be done with
    // wxMemoryDC::SelectObject(wxBitmap *bitmap)
}

wxMemoryDC::~wxMemoryDC(void)
{
  if (selected) {
    selected->selectedIntoDC = 0;
    selected->selectedTo = NULL;
    selected = NULL;
  }

#ifdef WX_USE_XRENDER
  X->picture = 0;
#endif
}

void wxMemoryDC::SelectObject(wxBitmap *bitmap)
{
  if (bitmap == selected)
    return;

  EndSetPixel();

  /* Skip wxMemoryDC, because the bitmap's cache is fine. */
  wxWindowDC::FreeGetPixelCache(); 
#ifdef WX_USE_CAIRO
  ReleaseCairoDev();
#endif

  if (!read_only) {
    /* Bitmap selection memory and safety */
    if (bitmap && bitmap->selectedIntoDC)
      bitmap = NULL;

    if (selected) {
      selected->selectedIntoDC = 0;
      selected->selectedTo = NULL;
#ifdef USE_GL
      if (X->wx_gl) {
	X->gl_cfg = NULL;
	X->wx_gl->Reset(NULL, 0, 0);
      }
#endif      
    }
  } else if (bitmap && bitmap->selectedTo) {
    bitmap->selectedTo->FreeGetPixelCache();
#ifdef WX_USE_CAIRO
    bitmap->selectedTo->ReleaseCairoDev();
#endif
  }

  // free all associated GCs
#ifdef WX_USE_XRENDER
  X->picture = 0;
#endif
  Destroy();
  
  if (bitmap && bitmap->Ok()) {
    // The bitmap must use the display and screen of the application.
    // The drawable is the associated pixmap, width, height and depth
    // will be queried with XGetGeometry.
    wxWindowDC_Xinit *init;
    Pixmap pm;
    init = new wxWindowDC_Xinit;
    init->dpy      = wxAPP_DISPLAY;
    init->scn      = wxAPP_SCREEN;
    pm = GETPIXMAP(bitmap);
    init->drawable = pm;
    Initialize(init);
#ifdef USE_GL
    if (X->wx_gl) {
      int depth;
      depth = bitmap->GetDepth();
      X->gl_cfg = bitmap->gl_cfg;
      X->wx_gl->Reset(bitmap->gl_cfg, (depth == 1) ? 0 : (long)pm, 1);
    }
#endif
    // If another colourmap is associated with the bitmap,
    //  use it instead of the current colourmap.
    if (bitmap->GetColourMap() != current_cmap) {
      wxColourMap *cm;
      cm = bitmap->GetColourMap();
      SetColourMap(cm);
    }
    selected = bitmap;
    if (!read_only) {
      bitmap->selectedIntoDC = -1;
      selected->selectedTo = this;
    }
  } else {
    DRAWABLE = 0;
    WIDTH = HEIGHT = 0;
    selected = NULL;
  }
}

#ifdef WX_USE_XRENDER
void wxMemoryDC::InitPicture()
{
  long p;
  p = selected->GetPicture();
  X->picture = p;
  if (!read_only)
    InitPictureClip();
}
#endif

wxBitmap *wxMemoryDC::GetObject()
{
  return selected;
}

void wxMemoryDC::GetSize(double *w, double *h)
{
  if (selected) {
    double v;
    v = selected->GetWidth();
    *w = v;
    v = selected->GetHeight();
    *h = v;
  } else {
    *w = 0;
    *h = 0;
  }
}

void wxMemoryDC::FreeGetPixelCache()
{
  if (selected)
    selected->FreeMaskBit();
  wxWindowDC::FreeGetPixelCache();
}
