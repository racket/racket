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

#ifndef MemoryDC_h
#define MemoryDC_h

#ifdef __GNUG__
#pragma interface
#endif

class wxBitmap;

class wxMemoryDC : public wxCanvasDC {
public:
    wxMemoryDC(Bool read_only = 0);
    ~wxMemoryDC(void);

    virtual void GetSize(double *w, double *h);

    void SelectObject(wxBitmap *bitmap);
    wxBitmap* GetObject();

#ifdef WX_USE_XRENDER
    virtual void InitPicture();
#endif

  virtual void FreeGetPixelCache(void);

private:
    wxBitmap *selected; /* Need to keep pointer or it will be GC'ed */
    Bool read_only;
};

#endif // MemoryDC_h
