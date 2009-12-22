/*
 * File:	wx_gdi.h
 * Purpose:	Declaration of various graphics objects - fonts, pens, icons etc.
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_gdih
#define wx_gdih

#include "wb_gdi.h"
#include "wx_pltgdi.h"

// Font
class wxFont: public wxbFont
{
 public:
  wxFont *redirect;
  HFONT screen_cfont;
  HFONT general_cfont;
  wxList *rotated_font;
  wxList *substitute_font;

  Font *c_f;
  HFONT c_f_cfont;

  void *glyph_cache;
  void *size_cache;

  wxFont(void);
  wxFont(int PointSize, int Family, int Style, int Weight, Bool underlined = FALSE, 
	 int smoothing = wxSMOOTHING_DEFAULT, Bool sip = FALSE, double Rotation = 0.0,
	 Bool redirect_ok = TRUE);
  wxFont(int PointSize, const char *Face, int Family, int Style, int Weight, 
	 Bool underlined = FALSE, int smoothing = wxSMOOTHING_DEFAULT, Bool sip = FALSE,
	 Bool redirect_ok = TRUE);
  ~wxFont(void);
  Bool Create(int PointSize, int Family, int Style, int Weight, Bool underlined,
	      int smoothing, Bool sip, double Rotation, Bool redirect_ok);
  HFONT BuildInternalFont(HDC dc, Bool screen_font = TRUE, double angle = 0.0);
  inline HFONT GetInternalFont(HDC dc, double angle = 0.0) { return BuildInternalFont(dc, TRUE, angle); }

  Bool ScreenGlyphAvailable(int c, Bool for_label = FALSE);
  Bool GlyphAvailable(int c, HDC hdc, int screen_font);

  Bool GlyphAvailableNow(int c, HDC hdc, int screen_font);
  wxFont *Substitute(int c, HDC hdc, int screen_font);

  Bool ScreenGlyphAvailabilityCached(int c, Bool for_label, Bool *avail);
  void CacheScreenGlyphAvailability(int c, Bool for_label, Bool avail);
};

class wxColourMap: public wxObject
{
 public:
  HPALETTE ms_palette;
  wxColourMap(void);
  ~wxColourMap(void);
};

#define wxColorMap wxColourMap

// Pen
class wxPen: public wxbPen
{
 public:
  double old_width, current_scale, old_scale;
  int old_style;
  int old_join;
  int old_cap;
  int old_nb_dash;
  wxDash *old_dash;
  wxBitmap *old_stipple;
  COLORREF old_color;
  Pen *g_p, *a_g_p;
  double g_alpha, a_g_alpha;
  Bool use_const;
  wxPen *const_pen;

  HPEN cpen;
  HPEN my_old_cpen;

  wxPen(void);
  wxPen(wxColour *col, double width, int style, Bool use_const = TRUE);
  wxPen(const char *col, double width, int style);
  ~wxPen(void);

  void ChangePen();
  HPEN SelectPen(HDC dc, double scale = 1.0);

  Pen *GraphicsPen(Bool align, double sx, double alpha);
  void ReleaseGraphics();
};

int wx2msPenStyle(int wx_style);

// Brush
class wxBrush: public wxbBrush
{
 public:
  HBRUSH cbrush;
  HBRUSH my_old_cbrush;
  int old_style;
  wxBitmap *old_stipple;
  COLORREF old_color;
  Brush *g_b;
  double g_alpha;
  Bool use_const;
  wxBrush *const_brush;

  wxBrush(void);
  wxBrush(wxColour *col, int style, Bool use_const = TRUE);
  wxBrush(const char *col, int style);
  ~wxBrush(void);

  void ChangeBrush();
  HBRUSH SelectBrush(HDC dc);

  Brush *GraphicsBrush(double alpha);
  void ReleaseGraphics();
};

// Bitmap
class wxDC;
class wxMemoryDC;
class wxItem;
class wxGLConfig;

class wxBitmap: public wxObject
{
 protected:
  int width;
  int height;
  int depth;
  int is_dib;
  Bool ok;
  int numColors;
  wxColourMap *bitmapColourMap;
 public:
  wxBitmap *mask;
  HBITMAP ms_bitmap;
  HBITMAP label_bitmap; /* Gray background, drawn with mask */
  HBITMAP button_label_bitmap; /* White background, drawn with mask */
  void *accounting;
  wxDC *selectedInto; // So bitmap knows whether it's been selected into
                      // a device context (for error checking)
  wxMemoryDC *mask_cache; // the cached mask
  int cache_xsrc1, cache_ysrc1, cache_iw, cache_ih;
  Bool selectedIntoDC;
  wxGLConfig *gl_cfg;

  wxBitmap(void); // Platform-specific

  // Initialize with raw data
  wxBitmap(char bits[], int width, int height);

#if USE_XPM_IN_MSW
  // Initialize with XPM data
  wxBitmap(char **data, wxItem *anItem = NULL);
#endif

  // Load a file or resource
  wxBitmap(char *name, long flags = 0, wxColour *bg = NULL);

  // If depth is omitted, will create a bitmap compatible with the display
  wxBitmap(int width, int height, Bool b_and_w = FALSE);
  ~wxBitmap(void);

  virtual Bool Create(int width, int height, int depth = -1);
  virtual Bool LoadFile(char *name, long flags = 0, wxColour *bg = NULL);
  virtual Bool SaveFile(char *name, int type, int quality = 75, wxColourMap *cmap = NULL);

  inline Bool Ok(void) { return ok; }
  inline int GetWidth(void) { return width; }
  inline int GetHeight(void) { return height; }
  inline int GetDepth(void) { return depth; }
  inline void SetWidth(int w) { width = w; }
  inline void SetHeight(int h) { height = h; }
  inline void SetDepth(int d) { depth = d; }
  inline void SetOk(Bool isOk) { ok = isOk; }
  inline wxColourMap *GetColourMap(void) { return bitmapColourMap; }
  inline void SetColourMap(wxColourMap *cmap) { bitmapColourMap = cmap; }
  inline void SetMask(wxBitmap *newmask) { mask = newmask; }
  inline wxBitmap *GetMask(void) { return mask; }

  void *ChangeToDIBSection(Bool copy_old = FALSE);
  Bool IsDIB();

  HBITMAP GetLabelBitmap(Bool panel_bg);
  void ReleaseLabel();

  void SetGLConfig(wxGLConfig *gl_cfg);
  wxGLConfig *GetGLConfig(void);

  void ReleaseCachedMask();
};

// Cursor
class wxCursor: public wxBitmap
{
 public:
  HCURSOR ms_cursor;
  Bool destroyCursor;
  wxCursor(void);
  wxCursor(char bits[], int width, int height);
  wxCursor(wxBitmap *bm, wxBitmap *mask, int hotSpotX = 0, int hotSpotY = 0);
  wxCursor(int cursor_type);
  ~wxCursor(void);
};

extern int wxGetControlFontSize();

#endif // wx_gdih
