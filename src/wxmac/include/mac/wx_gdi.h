///////////////////////////////////////////////////////////////////////////////
// File:	wx_gdi.h (Macintosh version)
// Purpose:	Declaration of various graphics objects - fonts, pens, icons etc.
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_gdih
#define wx_gdih

#ifndef WX_CARBON
#include <QDOffscreen.h>
#endif

#include "wb_gdi.h"

#ifdef IN_CPROTO
typedef       void    *wxFont ;
typedef       void    *wxColourMap;
typedef       void    *wxPen;
typedef       void    *wxBrush;
typedef       void    *wxCursor;
typedef       void    *wxBitmap;
typedef       void    *XFontInfo;
typedef       void    *XFontPool;
#else

// Font
class wxFont: public wxbFont
{
 private:
	short macFontId;

	void Create(int PointSize, int FontId, int Family, int Style, int Weight, Bool underlined, int smoothing, Bool sip);

 public:
	wxFont(void);
	wxFont(int PointSize, int FontOrFamilyId, int Style, int Weight, Bool underlined = FALSE, 
	       int smoothing = wxSMOOTHING_DEFAULT, Bool sip = FALSE);
	wxFont(int PointSize, const char *Face, int Family, int Style, int Weight, 
	       Bool underlined = FALSE, int smoothing = wxSMOOTHING_DEFAULT, Bool sip = FALSE);
	~wxFont(void);

	double GetCharHeight(double scale_x = 1.0, double scale_y = 1.0);
	double GetCharWidth(double scale_x = 1.0, double scale_y = 1.0);
	void  GetTextExtent(char* string, int delta, int len, double* x, double* y,
			    double* descent, double* externalLeading, 
			    Bool qd_spacing = TRUE, Bool use16 = FALSE,
			    double scale_x = 1.0, double scale_y = 1.0);

	int GetMacFontNum(void); // mac platform only
	Style GetMacFontStyle(void); // mac platform only

	int CanRotate(void);

	int GetEffectiveSmoothing(double yscale);

	Bool ScreenGlyphAvailable(int c, Bool for_label = FALSE);
};

class wxColourMap: public wxObject
{
 public:
 	CGrafPtr cmap;

  wxColourMap(void);
  ~wxColourMap(void);
};

#define wxColorMap wxColourMap

// Pen
class wxPen: public wxbPen
{
 public:
  wxPen(void);
  wxPen(wxColour *col, double width, int style);
  wxPen(char *col, double width, int style);
  ~wxPen(void);

};

// Brush
class wxBrush: public wxbBrush
{
 public:
  wxBrush(void);
  wxBrush(wxColour *col, int style);
  wxBrush(char *col, int style);
  ~wxBrush(void);

};


// Bitmap
class wxItem;
class wxMemoryDC;
class wxGLConfig;

class wxBitmap: public wxObject
{
 protected:
  int width;
  int height;
  int depth;
  Bool ok;
 public:
  GWorldPtr x_pixmap;
  Bool freePixmap;
  wxMemoryDC *selectedInto;
  Bool selectedIntoDC;
  wxBitmap *mask;
  void *accounting;
  wxGLConfig *gl_cfg;

  wxBitmap(void) ;
  wxBitmap(char bits[], int width, int height);
  // Load a file or resource
  wxBitmap(char *name, long flags = wxBITMAP_DISCARD_COLOURMAP | wxBITMAP_TYPE_RESOURCE, wxColour *bg = NULL);

  // If depth is omitted, will create a bitmap compatible with the display
  wxBitmap(int width, int height, Bool bandw = FALSE);
  ~wxBitmap(void);

  virtual Bool Create(int width, int height, int depth = -1);
  virtual Bool LoadFile(char *name, long flags = wxBITMAP_DISCARD_COLOURMAP | wxBITMAP_TYPE_RESOURCE, wxColour *bg = NULL);
  virtual Bool SaveFile(char *name, int type, int quality = 75, wxColourMap *cmap = NULL);

  inline Bool Ok(void) { return ok; }
  inline int GetWidth(void) { return width; }
  inline int GetHeight(void) { return height; }
  inline int GetDepth(void) { return depth; }
  inline void SetWidth(int w) { width = w; }
  inline void SetHeight(int h) {height = h; }
  inline void SetDepth(int d) { depth = d; }
  inline void SetOk(Bool isOk) { ok = isOk; }
  // Heres a couple of methods used by wxExtend/wxPython exgdi.cc
  void SetColourMap(wxColourMap *cmap);
  wxColourMap* GetColourMap(void);


  inline void SetMask(wxBitmap *newmask) { mask = newmask; }
  inline wxBitmap *GetMask(void) { return mask; }

  void DrawMac(void);
  void DrawMac(int x, int y, int mode = srcCopy);
  
  void SetGLConfig(wxGLConfig *gl_cfg);
  wxGLConfig *GetGLConfig(void);
};

// Cursor
class wxCursor: public wxObject
{
 public:
  wxCursor(void);
  wxCursor(char bits[], int width, int height, int depth = 1);
  wxCursor(wxBitmap *bm, wxBitmap *mask, int hotSpotX, int hotSpotY);
  wxCursor(char *name);
  wxCursor(int cursor_type);
  ~wxCursor(void);

  Bool Ok(void);

  CursHandle cMacCursor;
  CursPtr cMacCustomCursor; // i.e., locally allocated
};

extern int wxGetControlFontSize();

#endif // IN_CPROTO
#endif // wx_gdih

