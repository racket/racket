/*
 * File:	wx_gdi.cc
 * Purpose:	GDI (Graphics Device Interface) objects and functions
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include "wx_graphics.h"

#include "..\..\utils\dib\dib.h"

#include "../../../wxcommon/wxGLConfig.h"

#include "xpm34.h"
#include "wximgfil.h"
#include "wximgxbm.h"

#include "../../../racket/include/scheme.h"

// Resource counting
#if 0
int pen_count, brush_count, font_count, bitmap_count;
# define COUNT_P(c) c++
# define COUNT_M(c) --c
#else
# define COUNT_P(c) 
# define COUNT_M(c) 
#endif

#pragma optimize("", off)

void RegisterGDIObject(HANDLE x);
void DeleteRegisteredGDIObject(HANDLE x);

Bool wxMakeBitmapAndPalette(LPBITMAPINFOHEADER lpInfo, HPALETTE * phPal, HBITMAP * phBitmap);

extern int read_JPEG_file(char *filename, wxBitmap *bm);
extern int write_JPEG_file(char *filename, wxBitmap *bm, int quality_val);

extern int wx_read_png(char *file_name, wxBitmap *bm, int w_mask, wxColour *bg);
extern int wx_write_png(char *file_name, wxBitmap *bm);

extern void wxAlphaBlit(wxBitmap *label_bm, wxBitmap *bm, wxBitmap *loaded_mask, 
			int br, int bg, int bb);

wxFont::wxFont(void)
{
  COUNT_P(font_count);

  Create(12, wxDEFAULT, wxNORMAL, wxNORMAL, FALSE, wxSMOOTHING_DEFAULT, FALSE, 0.0, TRUE);
}

/* Constructor for a font. Note that the real construction is done
 * in wxDC::SetFont, when information is available about scaling etc.
 */
wxFont::wxFont(int PointSize, int Family, int Style, int Weight, Bool Underlined, int Smoothing, Bool sip, 
	       double Rotation, Bool redirect_ok):
  wxbFont(PointSize, Family, Style, Weight, Underlined, Smoothing, sip, Rotation)
{
  COUNT_P(font_count);

  Create(PointSize, Family, Style, Weight, Underlined, Smoothing, sip, Rotation, redirect_ok);
}

wxFont::wxFont(int PointSize, const char *Face, int Family, int Style, int Weight, Bool Underlined, 
	       int Smoothing, Bool sip, Bool redirect_ok):
  wxbFont(PointSize, Family, Style, Weight, Underlined, Smoothing, sip)
{
  int id;

  COUNT_P(font_count);

  id = wxTheFontNameDirectory->FindOrCreateFontId(Face, Family);

  Create(PointSize, id, Style, Weight, Underlined, Smoothing, sip, 0.0, redirect_ok);
}

Bool wxFont::Create(int PointSize, int FontId, int Style, int Weight, Bool Underlined, int Smoothing, 
		    Bool sip, double Rotation, Bool redirect_ok)
{
  fontid = FontId;
  family = wxTheFontNameDirectory->GetFamily(fontid);
  style = Style;
  weight = Weight;
  point_size = PointSize;
  underlined = Underlined;
  smoothing = Smoothing;
  size_in_pixels = sip;
  rotation = Rotation;

  temporary = FALSE;

  screen_cfont = NULL;
  general_cfont = NULL;

  if (redirect_ok) {
    redirect = wxTheFontList->FindOrCreateFont(PointSize, FontId, Style, Weight, Underlined,
					       Smoothing, sip, Rotation);
  }

  return TRUE;
}

wxFont::~wxFont()
{
  if (screen_cfont)
    DeleteRegisteredGDIObject(screen_cfont);
  if (general_cfont)
    DeleteRegisteredGDIObject(general_cfont);
  
  if (rotated_font) {
    wxNode *node;
    wxFont *rot;
    node = rotated_font->First();
    while (node) {
      rot = (wxFont*)node->Data();
      DELETE_OBJ rot;
      node = node->Next();
    }
    DELETE_OBJ rotated_font;
  }

  if (substitute_font) {
    wxNode *node;
    wxFont *rot;
    node = substitute_font->First();
    while (node) {
      rot = (wxFont*)node->Data();
      DELETE_OBJ rot;
      node = node->Next();
    }
    DELETE_OBJ substitute_font;
  }

  redirect = NULL;

  COUNT_M(font_count);
}

typedef struct wxWCRANGE {
  WCHAR wcLow;
  USHORT cGlyphs;
} wxWCRANGE;
typedef struct {  
  DWORD cbThis;
  DWORD flAccel;
  DWORD cGlyphsSupported; 
  DWORD cRanges;  
  wxWCRANGE ranges[1]; 
} wxGLYPHSET;
typedef DWORD (WINAPI *wxGET_FONT_UNICODE_RANGES_PROC)(HDC, wxGLYPHSET*);
static wxGET_FONT_UNICODE_RANGES_PROC wxGetFontUnicodeRanges;
static int gfur_tried = 0;

static int glyph_exists_in_selected_font(HDC hdc, int c)
{
  if (!gfur_tried) {
    HMODULE hm;
    hm = LoadLibrary("gdi32.dll");
    if (hm) {
      wxGetFontUnicodeRanges = (wxGET_FONT_UNICODE_RANGES_PROC)GetProcAddress(hm, "GetFontUnicodeRanges");
    }
    gfur_tried = 1;
  }

  if (wxGetFontUnicodeRanges) {
    DWORD sz;
    sz = wxGetFontUnicodeRanges(hdc, NULL);
    if (sz) {
      wxGLYPHSET *gs;
      char *bytes;
      int i;
      bytes = new WXGC_ATOMIC char[sz];
      gs = (wxGLYPHSET *)bytes;
      gs->cbThis = sz;
      wxGetFontUnicodeRanges(hdc, gs);
      for (i = 0; i < gs->cRanges; i++) {
	if (gs->ranges[i].wcLow <= c
	    && ((gs->ranges[i].wcLow + gs->ranges[i].cGlyphs) > c))
	  return 1;
      }
    }
    return 0;
  } else {
    /* GetFontUnicodeRanges isn't available. Give up,
       and assume that the character is here. */
    return 1;
  }
}

typedef struct {
  HDC hdc;
  int c;
  int just_face;
  int just_tt;
  wchar_t *face;
} GlyphFindData;

#include "wx_bitfield.inc"

static int CALLBACK glyph_exists(ENUMLOGFONTW FAR* lpelf, 
				 NEWTEXTMETRICW FAR* lpntm, 
				 DWORD type, 
				 LPARAM _data)
{
  GlyphFindData *gfd = (GlyphFindData *)_data;

  if (((lpntm->tmFirstChar <= gfd->c)
       && (lpntm->tmLastChar >= gfd->c))) {
    /* This font might work...  */
    int ok = 1;
      
    if (gfd->just_face)
      ok = !memcmp(gfd->face, lpelf->elfLogFont.lfFaceName, 
		   wx_wstrlen(gfd->face));
    else if (gfd->just_tt)
      ok = (type == TRUETYPE_FONTTYPE);
    else
      ok = (type != TRUETYPE_FONTTYPE);
    
    if (ok && (type == TRUETYPE_FONTTYPE)) {
      /* Use the unicode bitfield to avoid unnecessary font loading */
      DWORD *usb;
      int x;
      usb = ((NEWTEXTMETRICEXW *)lpntm)->ntmFontSig.fsUsb;
      x = get_bitfield(gfd->c); /* in wx_bitfield.inc */
      if (!(usb[x >> 5] & (1 << (x & 0x1F))))
	ok = 0;
    }

    if (ok) {
      HFONT old, cfont;
      
      cfont = CreateFontIndirectW(&lpelf->elfLogFont);
      
      old = (HFONT)::SelectObject(gfd->hdc, cfont);
      
      ok = glyph_exists_in_selected_font(gfd->hdc, gfd->c);
      
      ::SelectObject(gfd->hdc, old);
      
      DeleteObject(cfont);

      if (gfd->face && !gfd->just_face) {
	memcpy(gfd->face, lpelf->elfLogFont.lfFaceName, LF_FACESIZE * sizeof(wchar_t));
      }
      
      if (ok)
	return 0;
    }
  }
  return 1;
}

Bool wxFont::GlyphAvailable(int c, HDC hdc, int screen_font)
{
  GlyphFindData gfd;

  if (redirect)
    return redirect->GlyphAvailable(c, hdc, screen_font);

  gfd.hdc = hdc;
  gfd.c = c;
  gfd.face = NULL;
  gfd.just_face = 0;

  gfd.just_tt = 1;
  if (!EnumFontFamiliesW(hdc, NULL, (FONTENUMPROCW)glyph_exists, (LPARAM)&gfd))
    return 1;

  gfd.just_tt = 0;
  if (!EnumFontFamiliesW(hdc, NULL, (FONTENUMPROCW)glyph_exists, (LPARAM)&gfd))
    return 1;

  return 0;
}

Bool wxFont::ScreenGlyphAvailabilityCached(int c, Bool for_label, Bool *avail)
{
  if (glyph_cache) {
    Scheme_Hash_Table *ht;
    Scheme_Object *v;
    ht = (Scheme_Hash_Table *)glyph_cache;
    if (for_label)
      c = -(c + 1);
    v = scheme_hash_get(ht, scheme_make_integer(c));
    if (v) {
      *avail = SCHEME_TRUEP(v);
      return TRUE;
    }
  }
  return FALSE;
}

void wxFont::CacheScreenGlyphAvailability(int c, Bool for_label, Bool avail)
{
  Scheme_Hash_Table *ht;
  ht = (Scheme_Hash_Table *)glyph_cache;
  if (!ht) {
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    glyph_cache = ht;
  }
  if (for_label)
    c = -(c + 1);
  scheme_hash_set(ht, scheme_make_integer(c), avail ? scheme_true : scheme_false);
}

Bool wxFont::GlyphAvailableNow(int c, HDC hdc, int screen_font)
{
  Bool avail;

  if (redirect)
    return redirect->GlyphAvailableNow(c, hdc, screen_font);

  if (screen_font) {
    if (ScreenGlyphAvailabilityCached(c, 0, &avail))
      return avail;
  }

  avail = glyph_exists_in_selected_font(hdc, c);

  if (screen_font) {
    CacheScreenGlyphAvailability(c, 0, avail);
  }

  return avail;
}

Bool wxFont::ScreenGlyphAvailable(int c, Bool for_label)
{
  HDC hdc;
  Bool r, avail;

  if (redirect)
    return redirect->ScreenGlyphAvailable(c, for_label);

  if (ScreenGlyphAvailabilityCached(c, for_label, &avail))
    return avail;

  hdc = ::GetDC(NULL);

  if (for_label) {
    HFONT old, cfont;
    
    cfont = BuildInternalFont(hdc, 1, 0.0);
    old = (HFONT)::SelectObject(hdc, cfont);
    r = glyph_exists_in_selected_font(hdc, c);
    ::SelectObject(hdc, old);

    if (!r) {
      /* Look for font links */
      HKEY key;
      DWORD nlen, vlen, vlen2;
      wchar_t value_name[256], *value;
      char *face;
      face = wxTheFontNameDirectory->GetScreenName(fontid, weight, style);
      if (face) {
	if (ERROR_SUCCESS
	    == RegOpenKeyExW(HKEY_LOCAL_MACHINE, 
			     L"SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\FontLink\\SystemLink",
			     0, KEY_READ,
			     &key)) {
	  int i = 0;
	  while (1) {
	    nlen = 256;
	    vlen = 0;
	    if (ERROR_SUCCESS
		!= RegEnumValueW(key, i, value_name, &nlen,
				 NULL, NULL, NULL, &vlen))
              break;
	    if (!strcmp(face, wxNARROW_STRING(value_name))) {
	      value = (wchar_t *)(new WXGC_ATOMIC char[vlen]);
	      vlen2 = vlen;
	      nlen++;
	      if (ERROR_SUCCESS
		  == RegEnumValueW(key, i, value_name, &nlen,
				   NULL, NULL, (BYTE *)value, &vlen2)) {
		if (vlen2 == vlen) {
		  int j, k;
		  vlen >>= 1;
		  r = 0;
		  for (j = 0; j < vlen; j++) {
		    if (value[j] == ',') {
		      j++;
		      for (k = j; k < vlen; k++) {
			if (!value[k])
			  break;
		      }
		      /* Range from j (inclusive) to k (exclusive) is a font name */
		      {
			GlyphFindData gfd;
			wchar_t *face;
			face = (wchar_t *)(new WXGC_ATOMIC char[(k - j + 1) * sizeof(wchar_t)]);
			memcpy(face, value + j, (k - j) * sizeof(wchar_t));
			face[k - j] = 0;
			
			gfd.hdc = hdc;
			gfd.c = c;
			gfd.face = face;
			gfd.just_tt = 0;
			gfd.just_face = 1;
			
			if (!EnumFontFamiliesW(hdc, NULL, (FONTENUMPROCW)glyph_exists, (LPARAM)&gfd)) {
			  r = 1;
			  break;
			}
		      }
		    }
		  }
		  if (r)
		    break;
		}
	      }
	      break;
	    }
	    i++;
	  }
	  RegCloseKey(key);
	}
      }
    }
  } else {
    r = GlyphAvailable(c, hdc, 1);
  }

  ReleaseDC(NULL, hdc);

  CacheScreenGlyphAvailability(c, for_label, r);

  return r;
}

wxFont *wxFont::Substitute(int c, HDC dc, Bool screen_font)
{
  wxFont *sub;
  wxNode *node;

  if (redirect)
    return redirect->Substitute(c, dc, screen_font);

  if (screen_font) {
    if (!substitute_font) {
      substitute_font = new wxList(wxKEY_INTEGER);
    }
    node = substitute_font->Find(c);
  } else
    node = NULL;

  if (node)
    sub = (wxFont *)node->Data();
  else {
    int found;
    GlyphFindData gfd;
    wchar_t facebuf[LF_FACESIZE];
    
    gfd.hdc = dc;
    gfd.c = c;
    gfd.face = facebuf;
    gfd.just_face = 0;
    
    gfd.just_tt = 1;
    found = !EnumFontFamiliesW(dc, NULL, (FONTENUMPROCW)glyph_exists, (LPARAM)&gfd);

    if (!found) {
      gfd.just_tt = 0;
      found = !EnumFontFamiliesW(dc, NULL, (FONTENUMPROCW)glyph_exists, (LPARAM)&gfd);
    }

    if (found) {
      /* Found substitute font */
      int sid;
      sid = wxTheFontNameDirectory->FindOrCreateFontId(wxNARROW_STRING(facebuf), family);
      sub = new wxFont(point_size, sid, style, weight,
		       underlined, smoothing, size_in_pixels, rotation);
    } else {
      sub = this;
    }
     
    if (screen_font)
      substitute_font->Append(c, (wxObject*)sub);
  }

  return sub;
}

static int CALLBACK check_font_charset(ENUMLOGFONTEX *lpelfe, NEWTEXTMETRICEX *lpntme,
				       DWORD FontType, LPARAM lParam)
{
  *(int *)lParam = lpelfe->elfLogFont.lfCharSet;
  return 0;
}

HFONT wxFont::BuildInternalFont(HDC dc, Bool screenFont, double angle)
{
  int nHeight;
  HFONT cfont;
  BYTE ff_italic;
  int ff_weight = 0;
  int ff_family = 0;
  char *ff_face = NULL;
  int charset = ANSI_CHARSET;
  Bool ff_underline = underlined;
  int ff_qual;
  int orientation;

  if (redirect)
    return redirect->BuildInternalFont(dc, screenFont, angle);

  if (angle != rotation) {
    int int_angle = (int)(angle * 1800 / 3.14159);
    wxNode *node;
    wxFont *rot;

    if (!rotated_font) {
      rotated_font = new wxList(wxKEY_INTEGER);
    }
    node = rotated_font->Find(int_angle);
    if (node)
      rot = (wxFont *)node->Data();
    else {
      rot = new wxFont(point_size, fontid, style, weight,
		       underlined, smoothing, size_in_pixels, angle);
      rotated_font->Append(int_angle, (wxObject*)rot);
    }

    return rot->BuildInternalFont(dc, screenFont, angle);
  }
    
  if (screenFont && screen_cfont)
    return screen_cfont;
  if (!screenFont && general_cfont)
    return general_cfont;

  if (screenFont && !size_in_pixels) {
    int dpi;
    dpi = ::GetDeviceCaps(dc, LOGPIXELSY);
    nHeight = MulDiv(point_size, dpi, 72);
  } else
    nHeight = point_size;
  
  ff_face = wxTheFontNameDirectory->GetScreenName(fontid, weight, style);
  if (!*ff_face)
    ff_face = NULL;
  
  switch (family) {
  case wxSCRIPT:
    ff_family = FF_SCRIPT;
    break;
  case wxDECORATIVE: 
    ff_family = FF_DECORATIVE;
    break;
  case wxROMAN: 
    ff_family = FF_ROMAN;
    break;
  case wxTELETYPE:
  case wxMODERN:
    ff_family = FF_MODERN;
    break;
  case wxSWISS: 
    ff_family = FF_SWISS;
    break;
  case wxDEFAULT:
  case wxSYMBOL:
  default: 
    ff_family = FF_DONTCARE;
  }
  
  /* Determine whether the face exists. */
  {
    LOGFONT lf;
    lf.lfCharSet = DEFAULT_CHARSET;
    if (strlen(ff_face) < 32)
      strcpy(lf.lfFaceName, ff_face);
    else {
      memcpy(lf.lfFaceName, ff_face, 31);
      lf.lfFaceName[32] = NULL;
    }
    lf.lfPitchAndFamily = 0;
    EnumFontFamiliesEx(dc, &lf, (FONTENUMPROC)check_font_charset, (LPARAM)&charset, 0);
  }

  if (style == wxITALIC || style == wxSLANT)
    ff_italic = 1;
  else
    ff_italic = 0;
  
  if (weight == wxNORMAL)
    ff_weight = FW_NORMAL;
  else if (weight == wxLIGHT)
    ff_weight = FW_LIGHT;
  else if (weight == wxBOLD)
    ff_weight = FW_BOLD;

  if (smoothing == wxSMOOTHING_DEFAULT) {
    if ((family == wxMODERN) && (nHeight > 8) && (nHeight < 14))
      ff_qual = ANTIALIASED_QUALITY;
    else
      ff_qual = PROOF_QUALITY;
  } else if (smoothing == wxSMOOTHING_PARTIAL)
    ff_qual = ANTIALIASED_QUALITY;
  else if (smoothing == wxSMOOTHING_ON) {
#ifndef CLEARTYPE_QUALITY
# define CLEARTYPE_QUALITY 5
#endif
    ff_qual = CLEARTYPE_QUALITY;
  } else
    ff_qual = NONANTIALIASED_QUALITY;

  orientation = (int)(angle * 1800 / 3.14159);
  
  cfont = CreateFont(-nHeight, 0, orientation, orientation, 
		     ff_weight, ff_italic, (BYTE)ff_underline,
		     0, charset, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
		     ff_qual, DEFAULT_PITCH | ff_family, ff_face);
  
  if (!cfont) {
    /* Try defaulting to family: */
    ff_face = wxTheFontNameDirectory->GetScreenName(family, weight, style);
    cfont = CreateFont(-nHeight, 0, orientation, orientation, 
		       ff_weight, ff_italic, (BYTE)ff_underline,
		       0, charset, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
		       ff_qual, DEFAULT_PITCH | ff_family, ff_face);
  }

  if (!cfont)
    cfont = CreateFont(12, 0, orientation, orientation, 
		       FW_NORMAL, 0,(BYTE)0,
		       0, charset, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
		       ff_qual, DEFAULT_PITCH | FF_SWISS, NULL);

  RegisterGDIObject((HANDLE)cfont);

  if (screenFont)
    screen_cfont = cfont;
  else
    general_cfont = cfont;

  return cfont;
}

/*
 * Colour map
 *
 */

wxColourMap::wxColourMap(void)
{
  ms_palette = 0;
}

wxColourMap::~wxColourMap(void)
{
  if (ms_palette)
    DeleteRegisteredGDIObject(ms_palette);
}

// Pens

wxPen::wxPen(void)
{
  wxColour *c;

  COUNT_P(pen_count);

  use_const = TRUE;

  stipple = NULL;
  style = wxSOLID;
  width = 0;
  join = wxJOIN_ROUND;
  cap = wxCAP_ROUND;
  nb_dash = 0;
  dash = NULL;
  cpen = NULL;
  my_old_cpen = NULL;
  old_width = -1;
  old_style = -1;
  old_join  = -1;
  old_cap  = -1;
  old_nb_dash  = -1;
  old_dash  = NULL;
  old_color  = 0;
  old_stipple = NULL;
  current_scale = 1.0;

  c = new wxColour(wxBLACK);
  c->Lock(1);
  colour = c;
}

wxPen::~wxPen()
{
  COUNT_M(pen_count);

  ReleaseGraphics();

  if (cpen)
    DeleteRegisteredGDIObject(cpen);

  cpen = NULL;
  const_pen = NULL;
}

wxPen::wxPen(wxColour *col, double Width, int Style, Bool _use_const)
{
  wxColour *c;

  COUNT_P(pen_count);

  c = new wxColour(col);
  c->Lock(1);
  colour = c;

  use_const = _use_const;

  stipple = NULL;
  width = Width;
  style = Style;
  join = wxJOIN_ROUND;
  cap = wxCAP_ROUND;
  nb_dash = 0;
  dash = NULL;
  cpen = NULL;
  my_old_cpen = NULL;
  old_width = -1;
  old_style = -1;
  old_join  = -1;
  old_cap  = -1;
  old_nb_dash  = -1;
  old_dash  = NULL;
  old_color  = 0;
  old_stipple = NULL;
  current_scale = 1.0;
  g_p = NULL;
  a_g_p = NULL;

  ChangePen();
}

wxPen::wxPen(const char *col, double Width, int Style)
{
  wxColour *c;

  COUNT_P(pen_count);

  c = new wxColour(col);
  c->Lock(1);
  colour = c;

  use_const = TRUE;

  stipple = NULL;
  width = Width;
  style = Style;
  join = wxJOIN_ROUND;
  cap = wxCAP_ROUND;
  nb_dash = 0;
  dash = NULL;
  cpen = NULL;
  old_width = -1;
  old_style = -1;
  old_join  = -1;
  old_cap  = -1;
  old_nb_dash  = -1;
  old_dash  = NULL;
  old_color  = 0;
  old_stipple = NULL;
  current_scale = 1.0;
  g_p = NULL;
  a_g_p = NULL;
    
  ChangePen();
}

static LineCap graphics_caps[] = { LineCapRound, LineCapSquare, LineCapFlat };
static LineJoin graphics_joins[] = { LineJoinBevel, LineJoinMiter, LineJoinRound };

static REAL gp_dotted[] = {2, 5, /* offset */ 2};
static REAL gp_short_dashed[] = {4, 4, /* offset */ 2};
static REAL gp_long_dashed[] = {4, 8, /* offset */ 2};
static REAL gp_dotted_dashed[] = {6, 6, 2, 6, /* offset */ 4};

Pen *wxPen::GraphicsPen(Bool align, double xs, double alpha)
{
  if (const_pen)
    return const_pen->GraphicsPen(align, xs, alpha);

  if (!align) {
    if (g_p && (g_alpha != alpha)) {
      wxGPenRelease(g_p);
      g_p = NULL;
    }
  } else {
    if (a_g_p && (a_g_alpha != alpha)) {
      wxGPenRelease(g_p);
      a_g_p = NULL;
    }
  }

  if ((!align && !g_p)
      || (align && !a_g_p)) {
    Pen *p;
    double pw;
    REAL offset, *dashes;
    int ndash;
    COLORREF px;
    
    switch (style) {
    case wxDOT:
      dashes = gp_dotted;
      ndash = (sizeof(gp_dotted) / sizeof(REAL)) - 1;
      break;
    case wxSHORT_DASH:
      dashes = gp_short_dashed;
      ndash = (sizeof(gp_short_dashed) / sizeof(REAL)) - 1;
      break;
    case wxLONG_DASH:
      dashes = gp_long_dashed;
      ndash = (sizeof(gp_long_dashed) / sizeof(REAL)) - 1;
      break;
    case wxDOT_DASH:
      dashes = gp_dotted_dashed;
      ndash = (sizeof(gp_dotted_dashed) / sizeof(REAL)) - 1;
      break;
    case wxSOLID:
    case wxTRANSPARENT:
    default:
      dashes = NULL;
      ndash = 0;
      break;
    }
    if (ndash)
      offset = dashes[ndash];
    else
      offset = 0;
    
    if (align) {
      pw = (int)width;
      pw = (int)(pw * xs);
      if (!pw)
	pw = 1;
    } else
      pw = width;

    p = wxGPenNew(colour->pixel, alpha, pw, 
		  graphics_caps[cap - wxCAP_ROUND],
		  graphics_joins[join - wxJOIN_BEVEL],
		  ndash, dashes, offset);
    if (align) {
      a_g_p = p;
      a_g_alpha = alpha;
    } else {
      g_p = p;
      g_alpha = alpha;
    }
  }
  return (align ? a_g_p : g_p);
}

void wxPen::ReleaseGraphics()
{
  if (g_p) {
    wxGPenRelease(g_p);
    g_p = NULL;
  }
  if (a_g_p) {
    wxGPenRelease(a_g_p);
    a_g_p = NULL;
  }
}

void wxPen::ChangePen(void)
{
  Bool must_change = FALSE;
  COLORREF ms_colour = 0;
  wxBitmap *bm;

  ReleaseGraphics();

  if (style==wxTRANSPARENT)
    return;

  ms_colour = colour->pixel;

  if (cpen==NULL)
    must_change = TRUE;
  else
    must_change = !(width==old_width     &&
                    join==old_join       &&
                    cap==old_cap         &&
                    dash==old_dash       &&
                    nb_dash==old_nb_dash &&
                    style==old_style     &&
                    stipple==old_stipple &&
                    old_color==ms_colour &&
		    old_scale == current_scale);

  if (!must_change)
    return;

  old_width = width;
  old_join = join;
  old_cap = cap;
  old_dash = dash;
  old_nb_dash = nb_dash;
  old_style = style;
  old_stipple = stipple;
  old_color = ms_colour;
  old_scale = current_scale;

  if (cpen) {
    /* Note: the pen can't be selected anywhere if we're changing it, so
       delete is ok */
    DeleteRegisteredGDIObject(cpen);
    cpen = NULL;
  }

  const_pen = NULL;

  bm = GetStipple();
  if (bm && !bm->Ok())
    bm = NULL;

  if (use_const
      && join==wxJOIN_ROUND
      && cap==wxCAP_ROUND
      && !bm) {
    wxPen *cp;
    cp = wxThePenList->FindOrCreatePen(colour, width, style);
    const_pen = cp;
  } else {
    if (join==wxJOIN_ROUND        &&
      cap==wxCAP_BUTT           &&
      style!=wxUSER_DASH        &&
      !bm                       &&
      (width || style == wxSOLID)) {
      HPEN naya;
      naya = CreatePen(wx2msPenStyle(style), width, ms_colour);
      cpen = naya;
    } else {
      LOGBRUSH logb;
      int xwidth = width;
      DWORD ms_style;
      wxDash *real_dash;

      ms_style = wx2msPenStyle(style);

      if (!width) {
	xwidth = 1;
      } else {
	xwidth = (int)(current_scale * xwidth);
	if (!xwidth)
	  xwidth = 1;
      }

      ms_style |= PS_GEOMETRIC;
    
      switch(join) {
      case wxJOIN_BEVEL: ms_style |= PS_JOIN_BEVEL; break;
      case wxJOIN_MITER: ms_style |= PS_JOIN_MITER; break;
      default:
      case wxJOIN_ROUND: ms_style |= PS_JOIN_ROUND; break;
      }

      switch(cap) {
      case wxCAP_PROJECTING: ms_style |= PS_ENDCAP_SQUARE; break;
      case wxCAP_BUTT:       ms_style |= PS_ENDCAP_FLAT;   break;
      default:
      case wxCAP_ROUND:      ms_style |= PS_ENDCAP_ROUND;  break;
      }

      if (bm) {
	logb.lbStyle = BS_PATTERN;
	logb.lbHatch = (LONG)stipple->ms_bitmap;
      } else {
	switch(style) {
	case wxBDIAGONAL_HATCH:
	  logb.lbStyle = BS_HATCHED;
	  logb.lbHatch = HS_BDIAGONAL;
	  break;
	case wxCROSSDIAG_HATCH:
	  logb.lbStyle = BS_HATCHED;
	  logb.lbHatch = HS_DIAGCROSS;
	  break;
	case wxFDIAGONAL_HATCH:
	  logb.lbStyle = BS_HATCHED;
	  logb.lbHatch = HS_FDIAGONAL;
	  break;
	case wxCROSS_HATCH:
	  logb.lbStyle = BS_HATCHED;
	  logb.lbHatch = HS_CROSS;
	  break;
	case wxHORIZONTAL_HATCH:
	  logb.lbStyle = BS_HATCHED;
	  logb.lbHatch = HS_HORIZONTAL;
	  break;
	case wxVERTICAL_HATCH:
	  logb.lbStyle = BS_HATCHED;
	  logb.lbHatch = HS_VERTICAL;
	  break;
	default:
	  logb.lbStyle = BS_SOLID;
	  break;
	}
      }
      logb.lbColor = ms_colour;

      if (style==wxUSER_DASH && nb_dash && dash) {
	int i;
	real_dash = new wxDash[nb_dash];
	for (i=0;i<nb_dash;i++) {
	  real_dash[i] = dash[i] * xwidth;
	}
      } else
	real_dash = NULL;
    
      cpen = ExtCreatePen(ms_style, xwidth, &logb,
			  style == wxUSER_DASH ? nb_dash : 0,
			  real_dash);
    }

    RegisterGDIObject(cpen);
  }
}

HPEN wxPen::SelectPen(HDC dc, double scale)
{
  HPEN prev_pen;

  if (const_pen)
    return const_pen->SelectPen(dc, scale);

  if (scale != current_scale) {
    current_scale = scale;
    ChangePen();
  }

  if (cpen && style!=wxTRANSPARENT)
    prev_pen = (HPEN)::SelectObject(dc,cpen);
  else {
    HPEN nullPen;
    nullPen = (HPEN)::GetStockObject(NULL_PEN);
    prev_pen = (HPEN)::SelectObject(dc , nullPen);
  }
  
  return prev_pen;
}

int wx2msPenStyle(int wx_style)
{
  int cstyle;

  switch (wx_style) {  
  case wxXOR_DOT:
  case wxDOT:
    cstyle = PS_DOT;
    break;
  case wxSHORT_DASH:
  case wxXOR_SHORT_DASH:
  case wxLONG_DASH:
  case wxXOR_LONG_DASH:
    cstyle = PS_DASH;
    break;
  case wxDOT_DASH:
  case wxXOR_DOT_DASH:
    cstyle = PS_DASHDOT;
    break;
  case wxTRANSPARENT:
    cstyle = PS_NULL;
    break;
  case wxUSER_DASH:
    cstyle = PS_DOT;
    break;
  case wxSOLID:
  default:
    cstyle = PS_SOLID;
    break;
  }

  return cstyle;
}


// Brushes

wxBrush::wxBrush(void)
{
  wxColour *c;

  COUNT_P(brush_count);
  
  use_const = TRUE;

  c = new wxColour(wxBLACK);
  c->Lock(1);
  colour = c;

  style = wxSOLID;
  stipple = NULL;
  cbrush = NULL;
  old_color = 0;
  old_style = -1;
  old_stipple = NULL;
}

wxBrush::~wxBrush()
{
  COUNT_M(brush_count);

  ReleaseGraphics();
  
  if (cbrush)
    DeleteRegisteredGDIObject(cbrush);

  cbrush = NULL;
  const_brush = NULL;
}

wxBrush::wxBrush(wxColour *col, int Style, Bool _use_const)
{
  wxColour *c;

  COUNT_P(brush_count);

  use_const = _use_const;

  c = new wxColour(col);
  c->Lock(1);
  colour = c;

  style = Style;
  stipple = NULL;
  cbrush = NULL;
  old_color = 0;
  old_style = -1;
  old_stipple = NULL;

  ChangeBrush();
}

Brush *wxBrush::GraphicsBrush(double alpha)
{
  if (const_brush)
    return const_brush->GraphicsBrush(alpha);

  if (g_b && (g_alpha != alpha)) {
    ReleaseGraphics();
  }

  if (!g_b) {
    Brush *b;
    b = wxGBrushNew(colour->pixel, alpha);
    g_b = b;
    g_alpha = alpha;
  }
  return g_b;
}

void wxBrush::ReleaseGraphics()
{
  if (g_b) {
    wxGBrushRelease(g_b);
    g_b = NULL;
  }
}



void wxBrush::ChangeBrush(void) 
{
  Bool must_change = FALSE;
  COLORREF ms_colour = 0;
  wxBitmap *bm;

  ReleaseGraphics();

  if (style==wxTRANSPARENT)
    return;

  ms_colour = colour->pixel;

  if (cbrush==NULL)
    must_change = TRUE;
  else
    must_change = ((style != old_style)
		   || (stipple != old_stipple)
		   || (old_color != ms_colour));

  if (!must_change)
    return;

  if (cbrush) {
    /* Note: brush isn't selected anywhere if we can change it. */
    DeleteRegisteredGDIObject(cbrush);
    cbrush = NULL;
  }

  const_brush = NULL;

  bm = GetStipple();
  if (bm && !bm->Ok())
    bm = NULL;

  if (use_const && !bm) {
    wxBrush *cb;
    cb = wxTheBrushList->FindOrCreateBrush(colour, style);
    const_brush = cb;
  } else {
    if (bm) {
      cbrush = CreatePatternBrush(bm->ms_bitmap);
    } else {
      switch (style) {
      case wxTRANSPARENT:
	break;
      case wxBDIAGONAL_HATCH:
	{
	  cbrush = CreateHatchBrush(HS_BDIAGONAL, ms_colour);
	  break;
	}
      case wxCROSSDIAG_HATCH:
	{
	  cbrush = CreateHatchBrush(HS_DIAGCROSS, ms_colour);
	  break;
	}
      case wxFDIAGONAL_HATCH:
	{
	  cbrush = CreateHatchBrush(HS_FDIAGONAL, ms_colour);
	  break;
	}
      case wxCROSS_HATCH:
	{
	  cbrush = CreateHatchBrush(HS_CROSS, ms_colour);
	  break;
	}
      case wxHORIZONTAL_HATCH:
	{
	  cbrush = CreateHatchBrush(HS_HORIZONTAL, ms_colour);
	  break;
	}
      case wxVERTICAL_HATCH:
	{
	  cbrush = CreateHatchBrush(HS_VERTICAL, ms_colour);
	  break;
	}
      case wxSOLID:
      default:
	{
	  cbrush = CreateSolidBrush(ms_colour);
	  break;
	}
      }
    }

    RegisterGDIObject(cbrush);
  }
  
  old_style = style;
  old_stipple = stipple;
  old_color = ms_colour;
}

HBRUSH wxBrush::SelectBrush(HDC dc)
{
  HBRUSH prev_brush;

  if (const_brush)
    return const_brush->SelectBrush(dc);

  if (cbrush && style!=wxTRANSPARENT) {
    prev_brush = (HBRUSH)::SelectObject(dc, cbrush);
  } else {
    HBRUSH nullBrush;
    nullBrush = (HBRUSH)::GetStockObject(NULL_BRUSH);
    prev_brush = (HBRUSH)::SelectObject(dc, nullBrush);
  }

  return prev_brush;
}

wxBrush::wxBrush(const char *col, int Style)
{
  wxColour *c;

  COUNT_P(brush_count);

  c = new wxColour(col);
  c->Lock(1);
  colour = c;

  use_const = TRUE;

  style = Style;
  stipple = NULL;
  cbrush = NULL;
  old_color = 0;
  old_style = -1;
  old_stipple = NULL;

  ChangeBrush();
}

// Cursors

wxCursor::wxCursor(void)
{
  __type = wxTYPE_CURSOR;
  width = 32; height = 32;
  ms_cursor = NULL;
  destroyCursor = FALSE;
//  wxTheCursorList->Append(this);
}

wxCursor::wxCursor(char WXUNUSED(bits)[], int WXUNUSED(width), int WXUNUSED(height))
{
  __type = wxTYPE_CURSOR;
  ms_cursor = NULL;
  destroyCursor = FALSE;
//  wxTheCursorList->Append(this);
}

static wxMemoryDC *temp_mdc, *temp_mask_mdc;

wxCursor::wxCursor(wxBitmap *bm, wxBitmap *mask, int hotSpotX, int hotSpotY)
{
  int w, h, bw, bh, i, j, delta, bit, s;
  unsigned char r, g, b;
  unsigned char *ands, *xors;
  wxColour *c;
  wxMemoryDC *mask_dc;

  __type = wxTYPE_CURSOR;
  destroyCursor = FALSE;
  ms_cursor = 0;
  ok = FALSE;

  /* Get the allowed size for cursors: */
  w = GetSystemMetrics(SM_CXCURSOR);
  h = GetSystemMetrics(SM_CYCURSOR);

  bw = bm->GetWidth();
  bh = bm->GetHeight();

  /* If the given cursor doesn't fit, give up. (MrEd constrains the
     bitmap to be 16x16, which surely will fit.) */
  if ((bw > w) || (bh > h))
    return;

  /* Make read-only DCs for reading bits from the bitmaps: */
  if (!temp_mdc) {
    wxREGGLOB(temp_mdc);
    wxREGGLOB(temp_mask_mdc);
    temp_mdc = new wxMemoryDC(1);
    temp_mask_mdc = new wxMemoryDC(1);
  }

  temp_mdc->SelectObject(bm);
  /* Might fail, so we double-check: */
  if (!temp_mdc->GetObject())
    return;
  /* If bm and mask arethe same, use one DC (since re-selecting
     will fail, anyway). */
  if (mask == bm) {
    mask_dc = temp_mdc;
  } else {
    temp_mask_mdc->SelectObject(mask);
    if (!temp_mask_mdc->GetObject()) {
      temp_mdc->SelectObject(NULL);
      return;
    }
    mask_dc = temp_mask_mdc;
  }

  c = new wxColour(); /* to receive bit values */

  /* Windows wants cursor data in terms of an "and" bit array and
     "xor" bit array. */
  s = (w * h) >> 3; /* size of arrays in bytes */
  ands = new uchar[s];
  xors = new uchar[s];

  /* Init arrays to a value that means "the screen" */
  for (i = 0; i < s; i++) {
    ands[i] = 255;
    xors[i] = 0;
  }

  /* Read bits from mask and bm and set the corresponding bits in
     `ands' and `xors' */
  bit = 128;
  delta = 0;
  for (j = 0; j < bh; j++) {
    for (i = 0; i < w; i++) {
      if (i < bw) {
	mask_dc->GetPixel(i, j, c);
	c->Get(&r, &g, &b);
	
	/* black bit in mask? */
	if (!r && !g && !b) {
	  temp_mdc->GetPixel(i, j, c);
	  c->Get(&r, &g, &b);
	  
	  if (!r && !g && !b) {
	    /* black bit for cursor */
	    ands[delta] -= bit;
	  } else {
	    /* white bit for cursor */
	    ands[delta] -= bit;	    
	    xors[delta] |= bit;	    
	  }
	} /* otherwise, leave as screen */
      }

      bit = bit >> 1;
      if (!bit) {
	delta++;
	bit = 128;
      }
    }
  }

  ms_cursor = CreateCursor(wxhInstance, hotSpotX, hotSpotY, w, h, ands, xors);

  /* Clean up */
  temp_mdc->SelectObject(NULL);
  temp_mask_mdc->SelectObject(NULL);

  ok = !!ms_cursor;
}

static HCURSOR blank_cursor;

// Cursors by stock number
wxCursor::wxCursor(int cursor_type)
{
  __type = wxTYPE_CURSOR;
  switch (cursor_type) {
  case wxCURSOR_WAIT:
    {
      ms_cursor = LoadCursor(NULL, IDC_APPSTARTING);
      break;
    }
  case wxCURSOR_WATCH:
    {
      ms_cursor = LoadCursor(NULL, IDC_WAIT);
      break;
    }
  case wxCURSOR_IBEAM:
    {
      ms_cursor = LoadCursor(NULL, IDC_IBEAM);
      break;
    }
  case wxCURSOR_CROSS:
    {
      ms_cursor = LoadCursor(NULL, IDC_CROSS);
      break;
    }
  case wxCURSOR_SIZENWSE:
    {
      ms_cursor = LoadCursor(NULL, IDC_SIZENWSE);
      break;
    }
  case wxCURSOR_SIZENESW:
    {
      ms_cursor = LoadCursor(NULL, IDC_SIZENESW);
      break;
    }
  case wxCURSOR_SIZEWE:
    {
      ms_cursor = LoadCursor(NULL, IDC_SIZEWE);
      break;
    }
  case wxCURSOR_SIZENS:
    {
      ms_cursor = LoadCursor(NULL, IDC_SIZENS);
      break;
    }
  case wxCURSOR_CHAR:
    {
      ms_cursor = LoadCursor(NULL, IDC_ARROW);
      break;
    }
  case wxCURSOR_HAND:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_HAND");
      break;
    }
  case wxCURSOR_BULLSEYE:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_BULLSEYE");
      break;
    }
  case wxCURSOR_PENCIL:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_PENCIL");
      break;
    }
  case wxCURSOR_MAGNIFIER:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_MAGNIFIER");
      break;
    }
  case wxCURSOR_NO_ENTRY:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_NO_ENTRY");
      break;
    }
  case wxCURSOR_LEFT_BUTTON:
    {
      ms_cursor = LoadCursor(NULL, IDC_ARROW);
      break;
    }
  case wxCURSOR_RIGHT_BUTTON:
    {
      ms_cursor = LoadCursor(NULL, IDC_ARROW);
      break;
    }
  case wxCURSOR_MIDDLE_BUTTON:
    {
      ms_cursor = LoadCursor(NULL, IDC_ARROW);
      break;
    }
  case wxCURSOR_SIZING:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_SIZING");
      break;
    }
  case wxCURSOR_SPRAYCAN:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_ROLLER");
      break;
    }
  case wxCURSOR_PAINT_BRUSH:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_PBRUSH");
      break;
    }
  case wxCURSOR_POINT_LEFT:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_PLEFT");
      break;
    }
  case wxCURSOR_POINT_RIGHT:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_PRIGHT");
      break;
    }
  case wxCURSOR_QUESTION_ARROW:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_QARROW");
      break;
    }
  case wxCURSOR_BLANK:
    {
      if (!blank_cursor) {
	void *zero, *one;
	int w, h, s;
	w = GetSystemMetrics(SM_CXCURSOR);
	h = GetSystemMetrics(SM_CYCURSOR);
	s = w * h;
	zero = new WXGC_ATOMIC char[s];
	one = new WXGC_ATOMIC char[s];
	memset(zero, 0, s);
	memset(one, 255, s);
	blank_cursor = CreateCursor(wxhInstance, 
				    0, 0,
				    w, h,
				    one, zero);
      }
      ms_cursor = blank_cursor;
      break;
    }
  default:
  case wxCURSOR_ARROW:
    {
      ms_cursor = LoadCursor(NULL, IDC_ARROW);
      break;
    }
  }
//  wxTheCursorList->Append(this);
  ok = !!ms_cursor;
}

wxCursor::~wxCursor(void)
{
}

// Global cursor setting
void wxSetCursor(wxCursor *cursor)
{
  if (cursor && cursor->ms_cursor)
    ::SetCursor(cursor->ms_cursor);

  wxFlushEvents();
}

// Misc. functions

// Return TRUE if we have a colour display
Bool wxColourDisplay(void)
{
  HDC dc;
  Bool flag;
  int num;

  dc = ::GetDC(NULL);
  num = GetDeviceCaps(dc, NUMCOLORS);
  if ((num < 0) || (num > 2))
    flag = TRUE;
  else
    flag = FALSE;
  ReleaseDC(NULL, dc);
  return flag;
}

// Returns depth of screen
int wxDisplayDepth(void)
{
  HDC dc;
  int planes, bitsPerPixel, depth;
  dc = ::GetDC(NULL);
  planes = GetDeviceCaps(dc, PLANES);
  bitsPerPixel = GetDeviceCaps(dc, BITSPIXEL);
  depth = planes*bitsPerPixel;
  ReleaseDC(NULL, dc);
  return depth;
}

// Get size of display
void wxDisplaySize(int *width, int *height, int flags)
{
  RECT r;

  {
    HDC dc;
    int dw, dh;
    dc = ::GetDC(NULL);
    dw = GetDeviceCaps(dc, HORZRES);
    *width = dw;
    dh = GetDeviceCaps(dc, VERTRES);
    *height = dh;
    ReleaseDC(NULL, dc);
  }
}

void wxDisplayOrigin(int *x, int *y, int flags)
{
  RECT r;

  if (flags && SystemParametersInfo(SPI_GETWORKAREA, 0, &r, 0)) {
    *x = r.left;
    *y = r.top;
  } else {
    *x = 0;
    *y = 0;
  }
}

wxBitmap::wxBitmap(void)
{
  COUNT_P(bitmap_count);

  __type = wxTYPE_BITMAP;
  ok = FALSE;
  width = 0;
  height = 0;
  depth = 0;
  ms_bitmap = NULL;
  selectedInto = NULL;
  numColors = 0;
  bitmapColourMap = NULL;
  WXGC_IGNORE(this, selectedInto);
}

static char *map;

wxBitmap::wxBitmap(char bits[], int the_width, int the_height)
{
  int i, j;
  int rowwidth, offset;
  char *copy;
  int sp, cp;

  COUNT_P(bitmap_count);

  __type = wxTYPE_BITMAP;
  width = the_width;
  height = the_height;
  depth = 1;
  numColors = 0;
  bitmapColourMap = NULL;

  rowwidth = ((width + 7) / 8);
  if (rowwidth % sizeof(WORD))
    /* byte-aligned => word aligned */
    offset = 1;
  else
    offset = 0;

  if (!map) {
    wxREGGLOB(map);
    map = new char[256];
    for (i = 0; i < 256; i++) {
      j = (((i & 0x1) << 7)
	   | ((i & 0x2) << 5)
	   | ((i & 0x4) << 3)
	   | ((i & 0x8) << 1)
	   | ((i & 0x10) >> 1)
	   | ((i & 0x20) >> 3)
	   | ((i & 0x40) >> 5)
	   | ((i & 0x80) >> 6));
      j = 0xFF ^ j;
      map[i] = (char)j;
    }
  }

  copy = new char[(rowwidth + offset) * height];
  sp = 0; cp = 0;
  for (i = 0; i < height; i++) {
    for (j = 0; j < rowwidth; j++, sp++, cp++) {
      copy[cp] = map[((unsigned char *)bits)[sp]];
    }
    cp += offset;
  }

  ms_bitmap = CreateBitmap(the_width, the_height, 1, 1, copy);

  RegisterGDIObject(ms_bitmap);

  if (ms_bitmap) {
    ok = TRUE;
    accounting = GC_malloc_accounting_shadow((the_width * the_height) >> 3); 
  } else
    ok = FALSE;

  selectedInto = NULL;
  WXGC_IGNORE(this, selectedInto);
}

wxBitmap::wxBitmap(int w, int h, Bool b_and_w)
{
  COUNT_P(bitmap_count);

  __type = wxTYPE_BITMAP;
  ok = FALSE;
  width = w;
  height = h;
  depth = b_and_w ? 1 : -1;
  numColors = 0;
  selectedInto = NULL;
  bitmapColourMap = NULL;

  (void)Create(w, h, b_and_w ? 1 : -1);
  WXGC_IGNORE(this, selectedInto);
}

wxBitmap::wxBitmap(char *bitmap_file, long flags, wxColour *bg)
{
  COUNT_P(bitmap_count);

  __type = wxTYPE_BITMAP;
  ok = FALSE;
  width = 0;
  height = 0;
  depth = 0;
  selectedInto = NULL;
  numColors = 0;
  bitmapColourMap = NULL;

  LoadFile(bitmap_file, (int)flags, bg);
  WXGC_IGNORE(this, selectedInto);
}

#if USE_XPM_IN_MSW
// Create from data
wxBitmap::wxBitmap(char **data, wxItem *WXUNUSED(anItem))
{
  XImage *ximage;
  int     ErrorStatus;
  XpmAttributes xpmAttr;
  HDC     dc;

  COUNT_P(bitmap_count);

  __type = wxTYPE_BITMAP;
  selectedInto = NULL;
  bitmapColourMap = NULL;

  ok = FALSE;
  numColors = 0;

  dc = ::CreateCompatibleDC(NULL);	/* memory DC */

  if (dc)
  {
    xpmAttr.valuemask = XpmReturnInfos;	/* get infos back */
    ErrorStatus = XpmCreateImageFromData(&dc, data,
         &ximage, (XImage **) NULL, &xpmAttr);

    if (ErrorStatus == XpmSuccess)
    {
      BITMAP  bm;

      /* ximage is malloced and contains bitmap and attributes */
      ms_bitmap = ximage->bitmap;
      RegisterGDIObject(ms_bitmap);

      GetObject(ms_bitmap, sizeof(bm), (LPSTR) & bm);

      width = (bm.bmWidth);
      height = (bm.bmHeight);
      depth = (bm.bmPlanes * bm.bmBitsPixel);
      numColors = xpmAttr.npixels;
      XpmFreeAttributes(&xpmAttr);

      accounting = GC_malloc_accounting_shadow(width * height * 4); 

      XImageFree(ximage);	// releases the malloc, but does not detroy
			// the bitmap
      ok = TRUE;

    } else
    {
      ok = False;
//  XpmDebugError(ErrorStatus, NULL);
    }
    DeleteDC(dc);
  }
  WXGC_IGNORE(this, selectedInto);
}
#endif

Bool wxBitmap::Create(int w, int h, int d)
{
  width = w;
  height = h;
  depth = d;

  if (d > 0) {
    ms_bitmap = CreateBitmap(w, h, d, 1, NULL);
  } else {
    HDC dc;
    dc = GetDC(NULL);
    ms_bitmap = ::CreateCompatibleBitmap(dc, w, h);
    ReleaseDC(NULL, dc);
    depth = wxDisplayDepth();
  }
  RegisterGDIObject(ms_bitmap);
  if (ms_bitmap) {
    ok = TRUE;
    accounting = GC_malloc_accounting_shadow((width * height * ((d == 1) ? 1 : 32)) >> 3);
  } else
    ok = FALSE;

  is_dib = 0;

  return ok;
}

void *wxBitmap::ChangeToDIBSection(Bool copy_old)
{
  /* Called only when the bitmap is not selected! */
  BITMAPINFO bmp;
  HBITMAP bm;
  void *pBits;

  memset(&bmp, 0, sizeof(bmp));
  bmp.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
  bmp.bmiHeader.biWidth = width;
  bmp.bmiHeader.biHeight = height;
  bmp.bmiHeader.biPlanes = 1;
  bmp.bmiHeader.biBitCount = 32;

  bm = CreateDIBSection(NULL, &bmp, DIB_RGB_COLORS, &pBits, NULL, NULL);
  
  if (bm) {
    if (ms_bitmap) {
      if (copy_old) {
	int copied_ok = 0;
	HDC src, dest;

	src = CreateCompatibleDC(NULL);
	if (src) {
	  dest = CreateCompatibleDC(NULL);
	  if (dest) {
	    HANDLE src_old, dest_old;

	    src_old = SelectObject(src, ms_bitmap);
	    if (src_old != ERROR) {
	      dest_old = SelectObject(dest, bm);
	      if (dest_old != ERROR) {
		BitBlt(dest, 0, 0, width, height,
		       src, 0, 0,
		       SRCCOPY);
		copied_ok = 1;
		SelectObject(dest, dest_old);
	      }
	      SelectObject(dest, src_old);
	    }
	    DeleteDC(dest);
	  }
	  DeleteDC(src);
	}

	if (!copied_ok) {
	  DeleteObject(bm);
	  return NULL;
	}
      }

      DeleteRegisteredGDIObject(ms_bitmap);
    }
    ms_bitmap = bm;
    RegisterGDIObject(ms_bitmap);
    is_dib = 1;
    return pBits;
  } else
    return NULL;
}

Bool wxBitmap::IsDIB()
{
  return is_dib;
}

extern int wxsGetImageType(char *fn);

Bool wxBitmap::LoadFile(char *bitmap_file, long flags, wxColour *bg)  
{
  Bool getMask;
  wxMemoryDC *oldSel;

  if (selectedIntoDC)
    return FALSE;

  getMask = !!(flags & wxBITMAP_TYPE_MASK);

  if (!flags || (flags == wxBITMAP_TYPE_MASK))
    flags = wxsGetImageType(bitmap_file);

  /* Nevermind the palette */
  flags |= wxBITMAP_DISCARD_COLOURMAP;

  oldSel = (wxMemoryDC *)selectedInto;

  ok = FALSE;
  width = 0;
  height = 0;
  depth = 0;
  is_dib = 0;
  
  if (oldSel)
    oldSel->SelectObject(NULL);

  if (ms_bitmap) {
    DeleteRegisteredGDIObject(ms_bitmap);
    ms_bitmap = NULL;
    GC_free_accounting_shadow(accounting);
    accounting = NULL;
  }

  if (flags & wxBITMAP_TYPE_BMP_RESOURCE)
  {
    wchar_t *ws;
    ws = wxWIDE_STRING(bitmap_file);
    ms_bitmap = LoadBitmapW(wxhInstance, ws);
    if (ms_bitmap) {
      BITMAP bm;
      RegisterGDIObject(ms_bitmap);
      ok = TRUE;
      GetObject(ms_bitmap, sizeof(BITMAP), (LPSTR) &bm);
      width = bm.bmWidth;
      height = bm.bmHeight;
      depth = bm.bmPlanes;
      accounting = GC_malloc_accounting_shadow(width * height * 4);
    }
  }

  else if (flags & wxBITMAP_TYPE_XBM)
  {
    char *c;
    int w, h;

    c = wxLoadXBM(bitmap_file, &w, &h);
    if (c) {
      HDC glob_dc;
      glob_dc = GetDC(NULL);
      ms_bitmap = CreateBitmap(w, h, 1, 1, NULL);
      RegisterGDIObject(ms_bitmap);
      ReleaseDC(NULL, glob_dc);
      if (ms_bitmap) {
	HDC dc;
	
	dc = ::CreateCompatibleDC(NULL);
	
	if (dc)
	  {
	    HGDIOBJ orig;
	    int p;
	    COLORREF white = RGB(255, 255, 255);
	    COLORREF black = RGB(0, 0, 0);
	    int i, j;
				
	    orig = ::SelectObject(dc, ms_bitmap);

	    for (i = 0, p = 0; i < h; i++) {
	      for (j = 0; j < w; j++, p++) {
		::SetPixelV(dc, j, i, c[p] ? black : white);
	      }
	    }

	    ::SelectObject(dc, orig);
	    DeleteDC(dc);

	    ok = TRUE;
	    width = w;
	    height = h;
	    depth = 1;
	    accounting = GC_malloc_accounting_shadow((w * h) >> 3);
	  } else {
	    DeleteRegisteredGDIObject(ms_bitmap);
	    ms_bitmap = NULL;
	  }

      }
    }
  }
#if USE_XPM_IN_MSW
  else if (flags & wxBITMAP_TYPE_XPM)
    {
      XImage *ximage;
      XpmAttributes xpmAttr;
      HDC     dc;

      ok = False;
      dc = ::CreateCompatibleDC(NULL);
      if (dc) {
	int errorStatus;
	xpmAttr.valuemask = XpmReturnPixels;
	errorStatus = XpmReadFileToImage(&dc, bitmap_file, &ximage, (XImage **) NULL, &xpmAttr);
	DeleteDC(dc);
	if (errorStatus == XpmSuccess) {
	  BITMAP  bm;

	  ms_bitmap = ximage->bitmap;
	  RegisterGDIObject(ms_bitmap);

	  GetObject(ms_bitmap, sizeof(bm), (LPSTR) & bm);

	  width = (bm.bmWidth);
	  height = (bm.bmHeight);
	  depth = (bm.bmPlanes * bm.bmBitsPixel);
	  numColors = xpmAttr.npixels;
	  XpmFreeAttributes(&xpmAttr);
	  XImageFree(ximage);
	
	  accounting = GC_malloc_accounting_shadow(width * height * 4);


	  ok = TRUE;
	} else {
	  ok = FALSE;
	}
      }
    }
#endif
#if USE_IMAGE_LOADING_IN_MSW
  else if ((flags & wxBITMAP_TYPE_BMP) 
	  || (flags & wxBITMAP_TYPE_ANY))
  {
    wxColourMap *cmap = NULL;
    Bool success = FALSE;
    if (flags & wxBITMAP_DISCARD_COLOURMAP)
      success = wxLoadIntoBitmap(bitmap_file, this);
    else
      success = wxLoadIntoBitmap(bitmap_file, this, &cmap);
    if (!success && cmap) {
      delete cmap;
      cmap = NULL;
    }
    if (cmap)
      bitmapColourMap = cmap;
  }
#endif
  else if (flags & wxBITMAP_TYPE_GIF)
  {
    Bool success = FALSE;
    if (flags & wxBITMAP_DISCARD_COLOURMAP)
      success = wxLoadGifIntoBitmap(bitmap_file, this, NULL, getMask);
    else
      success = wxLoadGifIntoBitmap(bitmap_file, this, NULL, getMask);
  }
  else if (flags & wxBITMAP_TYPE_JPEG)
  {
    Bool success;
    success = read_JPEG_file(bitmap_file, this);
    if (!success) {
      if (ms_bitmap) {
	DeleteRegisteredGDIObject(ms_bitmap);
	ms_bitmap = NULL;
	GC_free_accounting_shadow(accounting);
	accounting = NULL;
      }
      ok = FALSE;
    }
  }
  else if (flags & wxBITMAP_TYPE_PNG)
  {
    Bool success;
    success = wx_read_png(bitmap_file, this, getMask, bg);
    if (!success) {
      if (ms_bitmap) {
	DeleteRegisteredGDIObject(ms_bitmap);
	ms_bitmap = NULL;
	GC_free_accounting_shadow(accounting);
	accounting = NULL;
      }
      ok = FALSE;
    }
  }
  
  if (oldSel && ok)
    oldSel->SelectObject(this);

  return ok;
}

wxBitmap::~wxBitmap(void)
{
  COUNT_M(bitmap_count);

  ReleaseCachedMask();

  if (selectedInto) {
    ((wxMemoryDC *)selectedInto)->SelectObject(NULL);
    selectedInto = NULL;
  }
  if (ms_bitmap) {
    DeleteRegisteredGDIObject(ms_bitmap);
  }
  ms_bitmap = NULL;
  if (label_bitmap) {
    DeleteRegisteredGDIObject(label_bitmap);
  }
  label_bitmap = NULL;
  if (button_label_bitmap) {
    DeleteRegisteredGDIObject(button_label_bitmap);
  }
  button_label_bitmap = NULL;
  if (accounting) {
    GC_free_accounting_shadow(accounting);
    accounting = NULL;
  }

  if (bitmapColourMap)
    delete bitmapColourMap;
}

Bool wxBitmap::SaveFile(char *filename, int typ, int quality, wxColourMap *cmap)
{
  if (!ok) return FALSE;

  switch (typ)
  {
#if USE_IMAGE_LOADING_IN_MSW
    case wxBITMAP_TYPE_BMP:
    {
      wxColourMap *actualCmap = cmap;
      if (!actualCmap)
        actualCmap = bitmapColourMap;
      return wxSaveBitmap(filename, this, actualCmap);
      break;
    }
#endif
    case wxBITMAP_TYPE_XBM:
      {
	char *c;
	int p;
	HGDIOBJ orig = NULL;
	int i, j;
	HDC dc;

	c = new char[width * height];
	
	dc = (selectedInto 
	      ? selectedInto->cdc
	      : CreateCompatibleDC(NULL));
			
	if (dc && !selectedInto) {
	  orig = SelectObject(dc, ms_bitmap);
	  if (!orig) {
	    DeleteDC(dc);
	    dc = NULL;
	  }
	}

	if (!dc) return FALSE;

	for (i = 0, p = 0; i < height; i++) {
	  for (j = 0; j < width; j++, p++) {
	    int v;
	    v = (::GetPixel(dc, j, i) ? 1 : 0);
	    c[p] = v;
	  }
	}
	
	if (!selectedInto) {
	  SelectObject(dc, orig);
	  DeleteDC(dc);
	}

	return wxSaveXBM(filename, c, width, height);
	break;
      }
    case wxBITMAP_TYPE_XPM:
      {
	HGDIOBJ orig = NULL;
	Visual *visual = NULL;
	XImage  ximage;
	HDC dc;
	int errorStatus;

	dc = (selectedInto 
	      ? selectedInto->cdc
	      : CreateCompatibleDC(NULL));

	if (dc && !selectedInto) {
	  orig = SelectObject(dc, ms_bitmap);
	  if (!orig) {
	    DeleteDC(dc);
	    dc = NULL;
	  }
	}

	if (!dc) return FALSE;

	/* for following SetPixel */
	/* fill the XImage struct 'by hand' */
	ximage.width = width; ximage.height = height;
	ximage.depth = depth; ximage.bitmap = ms_bitmap;
	errorStatus = XpmWriteFileFromImage(&dc, filename,
					    &ximage, (XImage *) NULL,
					    (XpmAttributes *) NULL);

	if (!selectedInto) {
	  SelectObject(dc, orig);
	  DeleteDC(dc);
	}

	if (errorStatus == XpmSuccess)
	  return TRUE;		/* no error */
	else
	  return FALSE;

	break;
      }
    case wxBITMAP_TYPE_JPEG:
      return write_JPEG_file(filename, this, quality);
      break;
    case wxBITMAP_TYPE_PNG:
      return wx_write_png(filename, this);
      break;
    default:
      break;
  }
  return FALSE;
}

/****************************************/


Bool wxLoadIntoBitmap(char *filename, wxBitmap *bitmap, wxColourMap **pal)
{
  HBITMAP hBitmap;
  HPALETTE hPalette;

  Bool success;

  success = ReadDIB(filename, &hBitmap, &hPalette);

  if (!success)
  {
    DeleteObject(hPalette);
    return FALSE;
  }

  if (hPalette)
  {
    if (pal)
    {
      wxColourMap *tp;
      tp = new wxColourMap;
      *pal = tp;
      (*pal)->ms_palette = hPalette;
    }
    else
      DeleteObject(hPalette);
  }
  else if (pal)
    *pal = NULL;

  if (hBitmap)
  {
    BITMAP bm;
    GetObject(hBitmap, sizeof(bm), (LPSTR)&bm);

    bitmap->ms_bitmap = hBitmap;
    bitmap->SetWidth(bm.bmWidth);
    bitmap->SetHeight(bm.bmHeight);
    bitmap->SetDepth(bm.bmPlanes * bm.bmBitsPixel);
    bitmap->SetOk(TRUE);
    bitmap->accounting = GC_malloc_accounting_shadow((bm.bmWidth * bm.bmHeight) >> 3);

    return TRUE;
  }
  else return FALSE;
}

wxBitmap *wxLoadBitmap(char *filename, wxColourMap **pal)
{
  wxBitmap *bitmap;
  bitmap = new wxBitmap;
  if (wxLoadIntoBitmap(filename, bitmap, pal))
    return bitmap;
  else
  {
    delete bitmap;
    return NULL;
  }
}

void wxBitmap::SetGLConfig(wxGLConfig *_gl_cfg)
{
  if (_gl_cfg)
    _gl_cfg = _gl_cfg->Clone();
  gl_cfg = _gl_cfg;
}

wxGLConfig *wxBitmap::GetGLConfig(void)
{
  if (gl_cfg)
    return gl_cfg->Clone();
  else
    return NULL;
}

/****************************************/

static int tried_ab = 0, got_alpha = 0;

HBITMAP wxBitmap::GetLabelBitmap(Bool panel_bg)
{
  wxBitmap *bm;
  DWORD v;
  wxColor *c;

  if (panel_bg) {
    if (label_bitmap)
      return label_bitmap;
  } else {
    if (button_label_bitmap)
      return button_label_bitmap;
  }

  if (!mask || (mask->GetWidth() != GetWidth())
      || (mask->GetHeight() != GetHeight()))
    return ms_bitmap;

  /* Draw with mask into a background-gray area... */
  bm = new wxBitmap(GetWidth(), GetHeight(), 0);

  if (!bm->Ok())
    return ms_bitmap;

  if (panel_bg) {
    v = GetSysColor(COLOR_BTNFACE);
    c = new wxColour(GetRValue(v), GetGValue(v), GetBValue(v));
  } else
    c = wxWHITE;
  
  if (!tried_ab) {
    HMODULE mod;
    mod = LoadLibrary("Msimg32.dll");
    if (mod)
      got_alpha = !!GetProcAddress(mod, "AlphaBlend");
    tried_ab = 1;
  }

  if (!got_alpha && (mask->GetDepth() != 1)) {
    /* Blit can't alpha-blend, so do it ourselves */
    int r, g, b;
    r = c->Red();
    g = c->Green();
    b = c->Blue();
    wxAlphaBlit(bm, this, mask, r, g, b);
  } else {
    wxMemoryDC *dc;

    dc = new wxMemoryDC(0);
    dc->SelectObject(bm);
    dc->SetBackground(c);
    dc->Clear();
    dc->Blit(0, 0, GetWidth(), GetHeight(),
	     this, 0, 0, wxSOLID,
	     wxBLACK, mask);
    dc->SelectObject(NULL);
  }

  /* Take over ownership of label_bitmap: */
  if (panel_bg)
    label_bitmap = bm->ms_bitmap;
  else
    button_label_bitmap = bm->ms_bitmap;
  bm->ms_bitmap = 0;
  DELETE_OBJ bm;

  return (panel_bg ? label_bitmap : button_label_bitmap);
}

void wxBitmap::ReleaseLabel()
{
  if (!selectedIntoDC && label_bitmap) {
    DeleteRegisteredGDIObject(label_bitmap);
    label_bitmap = NULL;
  }
}
