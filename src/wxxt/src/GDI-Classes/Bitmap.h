/*								-*- C++ -*-
 *
 * Purpose: bitmap classes to implement pixmaps, icons, and cursors
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

#ifndef Bitmap_h
#define Bitmap_h

#ifdef __GNUG__
#pragma interface
#endif

#ifdef Have_X_Types
// easier access to X-Pixmap and X-Cursor
#define GETPIXMAP(obj) (*((Pixmap*)((obj)->GetHandle())))
#define GETCURSOR(obj) (*((Cursor*)((obj)->GetHandle())))
#endif

class wxBitmap_Xintern;
class wxCursor_Xintern;

class wxItem;
class wxMemoryDC;

class wxGLConfig;

class wxBitmap : public wxObject { // bitmap representation
public:
    wxBitmap(void);
    wxBitmap(char bits[], int width, int height);
    wxBitmap(int width, int height, Bool blackAndWhite = FALSE);
#if USE_XPM
    wxBitmap(char **data, wxItem *anItem = NULL);
#endif
    wxBitmap(char *name, long flags = wxBITMAP_DEFAULT, wxColour *bg = NULL);
    ~wxBitmap(void);
    // create and destroy
    Bool Create(int width, int height, int depth = -1);
    void Destroy(void);
    // get information about bitmap
    int  GetDepth(void);
    int  GetHeight(void);
    void GetHotSpot(int *x, int *y);
    int  GetWidth(void);
    // set and get colourmap
    wxColourMap* GetColourMap(void)
        { return cmap; }
    void SetColourMap(wxColourMap *new_cmap)
        { cmap = (new_cmap ? new_cmap : wxAPP_COLOURMAP); }
    // load and save bitmap
    Bool LoadFile(char *name, long flags = wxBITMAP_DEFAULT, wxColour *bg = NULL);
    Bool SaveFile(char *name, int type, int quality = 75, wxColourMap *cmap = NULL);
    // X representation
    virtual Bool  Ok(void) { return (Xbitmap != NULL); }
    virtual void* GetHandle(void); // return type Pixmap*
#ifdef WX_USE_XRENDER
    long GetPicture(void); // return type Picture or XftDraw*
#endif

  void SetMask(wxBitmap *bm) { loaded_mask = bm; }
  wxBitmap *GetMask() { return loaded_mask; }

  wxBitmap *GetMaskBit();
  void FreeMaskBit();

  void *GetLabelPixmap(Bool for_button = FALSE);
  void ReleaseLabel();

  void SetGLConfig(wxGLConfig *gl_cfg);
  wxGLConfig *GetGLConfig(void);

public:
    wxBitmap_Xintern *Xbitmap;
    wxColourMap      *cmap;
    wxBitmap         *loaded_mask;
    wxBitmap         *maskBit;
    wxBitmap         *label_bm, *button_label_bm;
    wxGLConfig       *gl_cfg;

public:
    int selectedIntoDC;
    wxMemoryDC *selectedTo;
};

class wxCursor : public wxBitmap { // cursor representation
public:
    wxCursor(void);
    wxCursor(wxBitmap *bm, wxBitmap *mask, int x=0, int y=0);
    wxCursor(int cursor_type);
    ~wxCursor(void);
    // X representation
    virtual Bool  Ok(void) { return (Xcursor != NULL); }
    virtual void* GetHandle(void); // return type Cursor*, GetPixmap returns NULL!
private:
    wxCursor_Xintern *Xcursor;
};

#endif // Bitmap_h
