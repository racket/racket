 /*								-*- C++ -*-
 *
 * Purpose: wxWindows font handling
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 2004-2010 PLT Scheme Inc.
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */

#ifdef __GNUG__
#pragma implementation "Font.h"
#pragma GCC diagnostic ignored "-Wwrite-strings"
#endif

#define  Uses_XLib
#define  Uses_wxFont
#define  Uses_wxFontDirectory
#define  Uses_wxWindowDC
#include "wx.h"

#ifdef WX_USE_XFT
# include <X11/Xft/Xft.h>
#endif

char *wx_font_spec [] = {
    "wxDEFAULT",
    // families
    "wxDECORATIVE", "wxMODERN", "wxROMAN", "wxSCRIPT", "wxSWISS", "wxTELETYPE",
    // style
    "wxNORMAL", "wxSLANT", "wxITALIC",
    // weight
    "wxNORMAL", "wxBOLD", "wxLIGHT",
    // More families
    "wxSYSTEM", "wxSYMBOL"
};

// local function prototypes
static XFontStruct *wxLoadQueryNearestFont(const char *main_screen_name,
					   int point_size, double scale_x, double scale_y,
					   int fontid, int family,
					   int style, int weight, 
					   Bool underlined, Bool size_in_pixels,
					   double angle);
#ifdef WX_USE_XFT
static wxFontStruct *wxLoadQueryNearestAAFont(const char *main_screen_name,
					      int point_size, double scale_x, double scale_y,
					      int style, int weight, 
					      Bool underlined, int smoothing, 
					      Bool size_in_pixels,
					      double angle);
#endif

//-----------------------------------------------------------------------------
// Face list for substitutions
//-----------------------------------------------------------------------------

#ifdef WX_USE_XFT

static int complete_face_list_size;
static char **complete_face_list;
static wxFontStruct **complete_font_list;

char **wxGetCompleteFaceList(int *_len, int mono_only)
{
  char buf[256], *s, *copy;
  int ssize, i, j, pos, len, scalable;
  XftFontSet *fs;
  int face_list_size;
  char **face_list;
  wxFontStruct **font_list;

  if (complete_face_list && !mono_only) {
    if (_len)
      *_len = complete_face_list_size;
    return complete_face_list;
  }

  if (mono_only) {
    fs = XftListFonts(wxAPP_DISPLAY, DefaultScreen(wxAPP_DISPLAY), 
                      XFT_SPACING, XftTypeInteger, XFT_MONO, NULL,
                      /* I'm assuming that every family is either
                         scalable or not. We inspect scalability
                         to bias substitution to scalable fonts */
                      XFT_FAMILY, XFT_SCALABLE,
                      NULL);
  } else {
    fs = XftListFonts(wxAPP_DISPLAY, DefaultScreen(wxAPP_DISPLAY), NULL,
                      XFT_FAMILY, XFT_SCALABLE,
                      NULL);
  }

  face_list_size = fs->nfont;
  face_list = new WXGC_PTRS char*[face_list_size];
  font_list = (wxFontStruct **)(new WXGC_ATOMIC char[sizeof(wxFontStruct*) * face_list_size]);

  pos = 0;
  for (i = 0; i < fs->nfont; i++) {
    s = buf;
    ssize = 256;
    do {
      if (XftNameUnparse(fs->fonts[i], s, ssize))
	break;
      ssize *= 2;
      s = new WXGC_ATOMIC char[ssize];
    } while (1);

    len = strlen(s);

    scalable = ((len > 2) && (s[len - 2] == 'u')); /* "u" in "...True" */
    
    /* Get scalability, then truncate at ':' */
    for (j = 0; j < len; j++) {
      if (s[j] == ':')
	break;
    }
    len = j;

    /* Add a space at the font to indicate "Xft" */
    copy = new WXGC_ATOMIC char[len + 2];
    memcpy(copy + 1, s, len);
    copy[0] = ' ';
    copy[len + 1] = 0;

    if (scalable) {
      face_list[pos] = copy;
      font_list[pos] = NULL;
      pos++;
    } else {
      /* unscalable at end, to discourage use in substitutions */
      j = fs->nfont - (i - pos) - 1;
      face_list[j] = copy;
      font_list[j] = NULL;
    }
  }
  XftFontSetDestroy(fs);

  if (!mono_only) {
    wxREGGLOB(complete_face_list);
    wxREGGLOB(complete_font_list);
    complete_face_list_size = face_list_size;
    complete_face_list = face_list;
    complete_font_list = font_list;
  }

  if (_len)
    *_len = face_list_size;
  return face_list;
}

static wxFontStruct *prev_subs;
static Display *prev_subs_display;

static wxFontStruct *doFindAAFont(Display *dpy, wxFontStruct *xfont, int c, int *index)
{
  wxFontStruct *naya;
  int i;

  wxGetCompleteFaceList(NULL, 0);

  for (i = 0; i < complete_face_list_size; i++) {
    if (!complete_font_list[i]) {
      naya = wxLoadQueryNearestAAFont(complete_face_list[i], 
				      13, 1.0, 1.0,
				      wxNORMAL, wxNORMAL_WEIGHT,
				      FALSE, wxSMOOTHING_DEFAULT,
				      TRUE, 0.0);
      complete_font_list[i] = naya;
    }
    
    if (XftGlyphExists(dpy, complete_font_list[i], c)) {
      /* Need the right size, weight, etc. */
      int sz, wt, sl, sip;
      XftPattern *pat;
      XftResult res;

      if (index) {
	*index = i;
	return xfont;
      }

      if (XftPatternGetInteger(xfont->pattern, XFT_PIXEL_SIZE, 0, &sz) 
	  == XftResultMatch) {
	sip = 1;
      } else if (XftPatternGetInteger(xfont->pattern, XFT_SIZE, 0, &sz) 
		 == XftResultMatch) {
	sip = 0;
      } else {
	sz = 13;
	sip = 1;
      }
      if (XftPatternGetInteger(xfont->pattern, XFT_WEIGHT, 0, &wt)
	  != XftResultMatch) {
	wt = XFT_WEIGHT_MEDIUM;
      }
      if (XftPatternGetInteger(xfont->pattern, XFT_SLANT, 0, &sl)
	  != XftResultMatch) {
	sl = XFT_SLANT_ROMAN;
      }

      if ((sz == 13) && sip
	  && (wt == XFT_WEIGHT_MEDIUM)
	  && (sl == XFT_SLANT_ROMAN))
	return complete_font_list[i];

      if (prev_subs) {
	XftFontClose(prev_subs_display, prev_subs);
	prev_subs = NULL;
      }


      pat = XftNameParse(complete_face_list[i] XFORM_OK_PLUS 1);
      pat = XftPatternBuild(pat,
			    (sip ? XFT_PIXEL_SIZE : XFT_SIZE), XftTypeInteger, sz,
			    XFT_WEIGHT, XftTypeInteger, wt,
			    XFT_SLANT, XftTypeInteger, sl,
			    NULL);
      pat = XftFontMatch(wxAPP_DISPLAY, DefaultScreen(dpy), pat, &res);
      prev_subs = XftFontOpenPattern(dpy, pat);
      prev_subs_display = dpy;

      return prev_subs ? prev_subs : xfont;
    }
  }

  return xfont;
}

extern "C" {
  wxFontStruct *wxFindAAFont(Display *dpy, wxFontStruct *xfont, int c)
  {
    return doFindAAFont(dpy, xfont, c, NULL);
  }
};


#endif

//-----------------------------------------------------------------------------
// wxFont create and destroy
//-----------------------------------------------------------------------------

wxFont::wxFont(void)
{
    font_id       = wxDEFAULT;
    family        = wxTheFontNameDirectory->GetFamily(font_id);
    style         = wxNORMAL;
    weight        = wxNORMAL_WEIGHT;
    point_size    = 12;
    underlined    = FALSE;
    rotation      = 0.0;

    InitFont();
}

wxFont::wxFont(int PointSize, int FontIdOrFamily, int Style, int Weight,
	       Bool Underlined, int Smoothing, Bool sip, double Rotation)
{
    font_id       = FontIdOrFamily;
    family        = wxTheFontNameDirectory->GetFamily(FontIdOrFamily);
    style         = Style;
    weight        = Weight == wxNORMAL ? wxNORMAL_WEIGHT : Weight;
    point_size    = PointSize;
    underlined    = Underlined;
    smoothing     = Smoothing;
    size_in_pixels = sip;
    rotation      = Rotation;

    InitFont();
}

wxFont::wxFont(int PointSize, const char *Face, int Family, int Style, 
	       int Weight, Bool Underlined, int Smoothing, Bool sip)
{
    font_id       = wxTheFontNameDirectory->FindOrCreateFontId(Face, Family);
    family        = wxTheFontNameDirectory->GetFamily(font_id);
    style         = Style;
    weight        = Weight == wxNORMAL ? wxNORMAL_WEIGHT : Weight;
    point_size    = PointSize;
    underlined    = Underlined;
    smoothing     = Smoothing;
    size_in_pixels = sip;
    rotation      = 0.0;

    InitFont();
}

void wxFont::InitFont(void)
{
  wxList *sl;
  
  __type = wxTYPE_FONT;
  
  sl = new wxList(wxKEY_STRING, FALSE);
  scaled_xfonts = sl;

#ifdef WX_USE_XFT
  sl = new wxList(wxKEY_STRING, FALSE);
  scaled_xft_fonts = sl;
#endif

  main_screen_name = wxTheFontNameDirectory->GetScreenName(font_id, weight, style);
}

wxFont::~wxFont(void)
{
  wxNode *node;
  node = scaled_xfonts->First();
  while (node) {
    XFontStruct *xfont;
    wxNode *next;
    xfont = (XFontStruct*)node->Data();
    next = node->Next();
    XFreeFont(wxAPP_DISPLAY, xfont);
    node = next;
  }
  DELETE_OBJ scaled_xfonts;

#ifdef  WX_USE_XFT
  node = scaled_xft_fonts->First();
  while (node) {
    wxFontStruct *xfont;
    xfont = (wxFontStruct*)node->Data();
    if (xfont != (wxFontStruct *)0x1)
      XftFontClose(wxAPP_DISPLAY, xfont);
    node = node->Next();
  }
  DELETE_OBJ scaled_xft_fonts;

  if (substitute_xft_fonts) {
    node = substitute_xft_fonts->First();
    while (node) {
      wxFont *sfont;
      sfont = (wxFont*)node->Data();
      DELETE_OBJ sfont;
      node = node->Next();
    }
    DELETE_OBJ substitute_xft_fonts;
  }
#endif

  if (rotated_fonts) {
    node = rotated_fonts->First();
    while (node) {
      wxFont *rot;
      rot = (wxFont*)node->Data();
      DELETE_OBJ rot;
      node = node->Next();
    }
    DELETE_OBJ rotated_fonts;
  }
}

char *wxFont::GetFaceString(void)
{
  /* If it's one of the portable facelss fonts, return NULL. */
  switch (font_id) {
  case wxDEFAULT:
  case wxDECORATIVE:
  case wxMODERN:
  case wxROMAN:
  case wxSCRIPT:
  case wxSWISS:
  case wxTELETYPE:
  case wxSYSTEM:
  case wxSYMBOL:
    return NULL;
  default:
    return wxTheFontNameDirectory->GetFontName(font_id); 
  }
}

Bool wxFont::ScreenGlyphAvailable(int c, Bool)
{
  XFontStruct *fontinfo;
#ifdef WX_USE_XFT
  wxFontStruct *xfontinfo;
#endif
  unsigned int byte1, byte2;
  int char_metric_offset;

#ifdef WX_USE_XFT
  xfontinfo = (wxFontStruct*)GetInternalAAFont(1.0, 1.0);
  if (xfontinfo)
    fontinfo = NULL;
  else
#endif
    fontinfo = (XFontStruct*)GetInternalFont(1.0, 1.0);

# ifdef WX_USE_XFT
  if (xfontinfo) {
    int index = 1;
    while (1) {
      if (XftGlyphExists(wxAPP_DISPLAY, xfontinfo, c))
	return TRUE;
      
      xfontinfo = (wxFontStruct*)GetNextAASubstitution(index++, c, 1.0, 1.0, 0.0);
      if (!xfontinfo)
	return FALSE;
    }
  }
#endif

  byte2 = c & 0xff;
  byte1 = c >> 8;

  if ((byte1 < fontinfo->min_byte1) 
      || (byte1 > fontinfo->max_byte1))
    return FALSE;

  if ((byte2 < fontinfo->min_char_or_byte2) 
      || (byte2 > fontinfo->max_char_or_byte2)) {
    return FALSE;
  }

  if (fontinfo->all_chars_exist
      || !fontinfo->per_char)
    return TRUE;

  char_metric_offset = ((byte1 - fontinfo->min_byte1)
			* (fontinfo->max_char_or_byte2 - fontinfo->min_char_or_byte2 + 1)
			- fontinfo->min_char_or_byte2
			+ byte2);

  if (!fontinfo->per_char[char_metric_offset].width
      && !fontinfo->per_char[char_metric_offset].ascent
      && !fontinfo->per_char[char_metric_offset].descent)
    return FALSE;
  
  return TRUE;
}

//-----------------------------------------------------------------------------
// rotation
//-----------------------------------------------------------------------------

int wxFont::CanRotate()
{
  return 1;
}

wxFont *wxFont::GetRotated(double angle)
{
  int int_angle = (int)(angle * 1000);
  wxNode *node;
  wxFont *rot;

  if (!rotated_fonts) {
    rotated_fonts = new wxList(wxKEY_INTEGER);
  }

  node = rotated_fonts->Find(int_angle);
  if (node)
    return (wxFont *)node->Data();

  rot = new wxFont(point_size, font_id, style, weight,
		   underlined, smoothing, size_in_pixels, angle);
  
  rotated_fonts->Append(int_angle, (wxObject*)rot);

  return rot;
}

//-----------------------------------------------------------------------------
// get internal representation of font
//-----------------------------------------------------------------------------

static void limit_point_scale(int size, double *scale_x, double *scale_y)
{
  if ((size * (*scale_x)) > 256)
    *scale_x = 1;
  if ((size * (*scale_y)) > 256)
    *scale_y = 1;
}

void *wxFont::GetInternalFont(double scale_x, double scale_y, double angle)
{
  char        sbuf[128];
  wxNode      *node=NULL;
  XFontStruct *xfont;

  if (angle != rotation) {
    wxFont *rot;
    rot = GetRotated(angle);
    return rot->GetInternalFont(scale_x, scale_y, angle);
  }

  limit_point_scale(point_size, &scale_x, &scale_y);
  sprintf(sbuf, "%g %g", scale_x, scale_y);

  if ((node = scaled_xfonts->Find(sbuf))) {
    xfont = (XFontStruct*)node->Data();
  } else {
    xfont = wxLoadQueryNearestFont(main_screen_name,
				   point_size, scale_x, scale_y, 
				   font_id, family, style, weight,
				   underlined, size_in_pixels, angle);
    scaled_xfonts->Append(sbuf, (wxObject*)xfont);
  }
  return (void*)xfont;
}

//-----------------------------------------------------------------------------
// get internal representation of font
//-----------------------------------------------------------------------------

void *wxFont::GetInternalAAFont(double scale_x, double scale_y, double angle)
{
#ifdef WX_USE_XFT
  if (wxXRenderHere()) {
    char        sbuf[128];
    wxNode      *node=NULL;
    wxFontStruct *xft_font;


    if (angle != rotation) {
      wxFont *rot;
      rot = GetRotated(angle);
      return rot->GetInternalAAFont(scale_x, scale_y, angle);
    }

    limit_point_scale(point_size, &scale_x, &scale_y);
    sprintf(sbuf, "%g %g", scale_x, scale_y);

    if ((node = scaled_xft_fonts->Find(sbuf))) {
      xft_font = (wxFontStruct*)node->Data();
    } else {
      xft_font = wxLoadQueryNearestAAFont(main_screen_name,
					  point_size, scale_x, scale_y, 
					  style, weight,
					  underlined, smoothing, size_in_pixels, angle);

      /* Record a 0x1 to mean "no AA font": */
      if (!xft_font)
	xft_font = (wxFontStruct*)0x1;

      scaled_xft_fonts->Append(sbuf, (wxObject*)xft_font);
    }
    if (xft_font == (wxFontStruct*)0x1)
      return NULL;    
    return (void*)xft_font;
  } else
    return NULL;
#else
    return GetInternalFont(scale_x, scale_y, angle);
#endif
}

#ifdef WX_USE_XFT
int wxFont::HasAASubstitutions()
{
  char *name;
  int i;

  name = main_screen_name;

  if (name[0] == ' ') {
    for (i = 1; name[i]; i++) {
      if (name[i] == ',')
	return 1;
    }
  }

  return 0;
}

void *wxFont::GetNextAASubstitution(int index, int cval, double scale_x, double scale_y, double angle)
{
  wxFont *subs;
  wxNode *node;

  if (!substitute_xft_fonts) {
    wxList *sl;
    sl = new wxList(wxKEY_INTEGER);
    substitute_xft_fonts = sl;
  }

  node = substitute_xft_fonts->Find(index);
  if (node)
    subs = (wxFont *)node->Data();
  else {
    char *name, *next_name;
    int i, c = 0, len;
    
    name = main_screen_name;

    for (i = 0; name[i]; i++) {
      if (name[i] == ',') {
	c++;
	if (c == index)
	  break;
      }
    }
    if (!name[i]) {
      if (index == c + 1) {
	wxGetCompleteFaceList(NULL, 0);
	c = -1;
	doFindAAFont(wxAPP_DISPLAY, NULL, cval, &c);
	if (c >= 0) {
	  index += c;
	  node = substitute_xft_fonts->Find(index);
	  if (node) {
	    subs = (wxFont *)node->Data();
	    next_name = NULL;
	  } else {
	    subs = NULL;
	    next_name = complete_face_list[c];
	  }
	} else
	  return NULL;
      } else
	return NULL;
    } else {
      i++;
      len = strlen(name XFORM_OK_PLUS i);
      next_name = new WXGC_ATOMIC char[len + 2];
      memcpy(next_name + 1, name + i, len + 1);
      next_name[0] = ' ';
      subs = NULL;
    }

    if (!subs) {
      subs = new wxFont(point_size, next_name, family, style, weight,
			underlined, smoothing, size_in_pixels);
      
      substitute_xft_fonts->Append(index, (wxObject*)subs);
    }
  }

  return subs->GetInternalAAFont(scale_x, scale_y, angle);
}
#endif

//-----------------------------------------------------------------------------
// wxFontList
//-----------------------------------------------------------------------------

wxFontList::wxFontList(void)
: wxObject(WXGC_NO_CLEANUP)
{
  list = new wxChildList;
}

wxFontList::~wxFontList(void)
{
}

void wxFontList::AddFont(wxFont *Font) 
{ 
  list->Append(Font); 
  list->Show(Font, -1); /* so it can be collected */
} 

//-----------------------------------------------------------------------------
// search for font in fontlist
//-----------------------------------------------------------------------------

wxFont *wxFontList::FindOrCreateFont(int PointSize, int FontIdOrFamily, 
				     int Style, int Weight, Bool underline,
				     int smoothing, Bool sip)
{
  wxFont *font;
  wxChildNode *node;
  int i = 0;
  
  while ((node = list->NextNode(i))) {
    wxFont *each_font;
    each_font = (wxFont*)node->Data();
    if (each_font &&
	each_font->GetPointSize() == PointSize &&
	each_font->GetStyle() == Style &&
	each_font->GetWeight() == Weight &&
	each_font->GetFontId() == FontIdOrFamily &&
	each_font->GetUnderlined() == underline &&
	each_font->GetSmoothing() == smoothing &&
	each_font->GetSizeInPixels() == sip)
      return each_font;
  }
  
  font = new wxFont(PointSize, FontIdOrFamily, Style, Weight, underline, smoothing, sip);

#if WXGARBAGE_COLLECTION_ON
  AddFont(font);
#endif

  return font;
}

wxFont *wxFontList::FindOrCreateFont(int PointSize, const char *Face, 
				     int Family, int Style, int Weight, 
				     Bool underline, int smoothing, Bool sip)
{
  int id;
  id = wxTheFontNameDirectory->FindOrCreateFontId(Face, Family);

  return FindOrCreateFont(PointSize,
			  id,
			  Style,
			  Weight,
			  underline,
			  smoothing,
			  sip);
}

//-----------------------------------------------------------------------------
// local utilities
//-----------------------------------------------------------------------------

#ifdef WX_USE_XFT

static wxFontStruct *wxLoadQueryNearestAAFont(const char *name,
					      int point_size, double scale_x, double scale_y,
					      int style, int weight,
					      Bool underlined, int smoothing, Bool sip,
					      double angle)
{
  wxFontStruct *fs;

  if (name && (name[0] != ' '))
    /* Not an Xft font name */
    return NULL;

  {
    int sl, wt;
    const char *ex_tags[2];
    int ex_types[2];
    long ex_vals[2];
    int ex_pos = 0;
    XftMatrix rot;
    int use_rot = 0;

    wt = ((weight == wxBOLD)
	  ? XFT_WEIGHT_BOLD
	  : ((weight == wxLIGHT)
	     ? XFT_WEIGHT_LIGHT
	     : XFT_WEIGHT_MEDIUM));
    sl = ((style == wxITALIC)
	  ? XFT_SLANT_ITALIC
	  : ((weight == wxSLANT)
	     ? XFT_SLANT_OBLIQUE
	     : XFT_SLANT_ROMAN));

    ex_tags[0] = NULL;
    ex_types[0] = 0;
    ex_vals[0] = 0;
    ex_tags[1] = NULL;
    ex_types[1] = 0;
    ex_vals[1] = 0;

    switch (smoothing) {
    case wxSMOOTHING_OFF:
      ex_vals[ex_pos] = 0;
      ex_types[ex_pos] = XftTypeBool;
      ex_tags[ex_pos++] = XFT_ANTIALIAS;
      break;
    case wxSMOOTHING_ON:
    case wxSMOOTHING_PARTIAL:
      ex_vals[ex_pos] = 1;
      ex_types[ex_pos] = XftTypeBool;
      ex_tags[ex_pos++] = XFT_ANTIALIAS;
      break;
    default:
      break;
    }

    if (angle || (scale_x != 1.0) || (scale_y != 1.0)) {
      XftMatrixInit(&rot);
      XftMatrixRotate(&rot, cos(angle), sin(angle));
      XftMatrixScale(&rot, scale_x, scale_y);
      use_rot = 1;
    }

    if (name) {
      XftPattern *pat;
      XftResult res;
      
      pat = XftNameParse(name XFORM_OK_PLUS 1);
      if (!pat) return NULL;

      pat = XftPatternBuild(pat,
			    (sip ? XFT_PIXEL_SIZE : XFT_SIZE), XftTypeInteger, point_size,
			    XFT_WEIGHT, XftTypeInteger, wt,
			    XFT_SLANT, XftTypeInteger, sl,
			    ex_tags[0], ex_types[0], ex_vals[0],
			    ex_tags[1], ex_types[1], ex_vals[1],
			    NULL);

      pat = XftFontMatch(wxAPP_DISPLAY, DefaultScreen(wxAPP_DISPLAY), pat, &res);
      if (!pat) return NULL;
     
      if (use_rot) {
	/* We add a transform after match, because Xft/fontconfig
	   seems to sometimes free a non-malloced pointer if we
	   include the transformation in the pattern to match. The
	   transformation presumably doesn't affect matching,
	   anyway, so adding it now should be fine. */
	XftMatrix *old_m;
	
	if (!XftPatternGetMatrix(pat, XFT_MATRIX, 0, &old_m)) {
	  /* Has existing matrix, so add rotation and scale: */
	  XftMatrixRotate(old_m, cos(angle), sin(angle));
	  XftMatrixScale(old_m, scale_x, scale_y);
	} else {
	  pat = XftPatternBuild(pat,
				XFT_MATRIX, XftTypeMatrix, &rot,
				NULL);
	}
      }

      fs = XftFontOpenPattern(wxAPP_DISPLAY, pat);
    } else
      fs = NULL;

    if (!fs) {
      /* accept most any default: */
      fs = XftFontOpen(wxAPP_DISPLAY, DefaultScreen(wxAPP_DISPLAY),
		       (sip ? XFT_PIXEL_SIZE : XFT_SIZE), XftTypeInteger, point_size,
		       XFT_WEIGHT, XftTypeInteger, wt,
		       XFT_SLANT, XftTypeInteger, sl,
		       ex_tags[0], ex_types[0], ex_vals[0],
		       ex_tags[1], ex_types[1], ex_vals[1],
		       NULL);
    }
  }
  
  return fs;
}

#endif

static XFontStruct *wxLoadQueryFont(const char *name,
				    int point_size, double scale_x, double scale_y,
				    int fontid, int style,
				    int weight, Bool underlined, 
				    int si_try_again, Bool sip, double angle)
{
  char *buffer;
  long len, i, found = 0;
  XFontStruct *s;

  if (!name)
    name = wxTheFontNameDirectory->GetScreenName(fontid, weight, style);

  if (!name)
    name = "-*-*-*-*-*-*-*-%d-*-*-*-*-*-*";

  len = strlen(name);
  buffer = new WXGC_ATOMIC char[len + 128];

  /* Make sure there's %d and no other format directives: */
  for (i = 0; i < len; i++) {
    if (name[i] == '%') {
      if (name[i + 1] == '%')
	i++;
      else if (name[i + 1] == 'd') {
	if (found)
	  return NULL;
	found = i + 1;
      } else
	return NULL;
    }
  }

  /* If the size is in pixels, try to change
     ...-*-%d-... to ...-%d-*-... */
  if (sip && found) {
    if ((found > 4) 
	&& (name[found+1] == '-')
	&& (name[found-2] == '-')
	&& (name[found-3] == '*')
	&& (name[found-4] == '-')) {
      char *rename;
      rename = new WXGC_ATOMIC char[len + 1];
      memcpy(rename, name, len + 1);
      rename[found-3] = '%';
      rename[found-2] = 'd';
      rename[found-1] = '-';
      rename[found] = '*';
      name = rename;
    } else
      sip = 0;
  } else
    sip = 0;

  if (found && ((angle != 0.0) 
		|| (scale_x != 1.0)
		|| (scale_y != 1.0))) {
    /* Replace %d with %s: */
    char *rename, *matrix_str;
    double matrix[4];
    
    rename = new WXGC_ATOMIC char[len + 1];
    memcpy(rename, name, len + 1);
    for (i = 0; i < len; i++) {
      if (rename[i] == '%') {
	if (rename[i + 1] == 'd') {
	  rename[i + 1] = 's';
	  break;
	}
	i++;
      }
    }

    matrix[0] = ((double)point_size * scale_x) * cos(angle);
    matrix[1] = ((double)point_size * scale_y) * sin(angle);
    matrix[2] = -((double)point_size * scale_x) * sin(angle);
    matrix[3] = ((double)point_size * scale_y) * cos(angle);

    matrix_str = new WXGC_ATOMIC char[128];
    sprintf(matrix_str, "[%g %g %g %g]", 
	    matrix[0], matrix[1],
	    matrix[2], matrix[3]);
    for (i = 0; matrix_str[i]; i++) {
      if (matrix_str[i] == '-')
	matrix_str[i] = '~';
    }
    
    sprintf(buffer, rename, matrix_str);
  } else {
    sprintf(buffer, name, 
	    (sip ? point_size : point_size * 10));
  }

  s = XLoadQueryFont(wxAPP_DISPLAY, buffer);

  if (!s && si_try_again && ((style == wxSLANT) || (style == wxITALIC))) {
    /* Try slant/italic instead of italic/slant: */
    s = wxLoadQueryFont(NULL, point_size, scale_x, scale_y, 
			fontid, (style == wxSLANT) ? wxITALIC : wxSLANT, 
			weight, underlined, 
			0, sip, angle);
  }

  return s;
}

static XFontStruct *wxLoadQueryNearestFont(const char *name,
					   int point_size, double scale_x, double scale_y,
					   int fontid, int family,
					   int style, int weight,
					   Bool underlined, Bool sip, double angle)
{
  XFontStruct *font;
  int tried_once = 0;

  while (1) {

    font = wxLoadQueryFont(name, point_size, scale_x, scale_y, 
			   fontid, style, weight, underlined, 
			   1, sip, angle);

    if (!font) {
      // search up and down by stepsize 1
      int max_size = point_size + 2 * (1 + (point_size/18));
      int min_size = point_size - 2 * (1 + (point_size/18));
      int i;

      // Try plain style
      font = wxLoadQueryFont(NULL, point_size, scale_x, scale_y, 
			     fontid, wxNORMAL, wxNORMAL_WEIGHT, underlined, 
			     1, sip, angle);

      // Search for smaller size (approx.)
      for (i=point_size-1; !font && i >= 1 && i >= min_size; i -= 1) {
	font = wxLoadQueryFont(name, i, scale_x, scale_y,
			       fontid, style, weight, underlined, 
			       1, sip, angle);
	if (!font)
	  font = wxLoadQueryFont(NULL, i, scale_x, scale_y, fontid,
				 wxNORMAL, wxNORMAL_WEIGHT, underlined, 
				 1, sip, angle);
      }
      // Search for larger size (approx.)
      for (i=point_size+1; !font && i <= max_size; i += 1) {
	font = wxLoadQueryFont(name, i, scale_x, scale_y, 
			       fontid, style, weight, underlined, 
			       1, sip, angle);
	if (!font)
	  font = wxLoadQueryFont(NULL, i, scale_x, scale_y, 
				 fontid,  wxNORMAL, wxNORMAL_WEIGHT, underlined, 
				 1, sip, angle);
      }
    }
    
    if (font || tried_once)
      break;
    else {
      tried_once = 1;
      fontid = family;
    }
  }

  /* Last-ditch efforts */
  if (!font) {
    char buffer[40];
    sprintf(buffer, "-*-*-*-*-*-*-*-%d-*-*-*-*-*-*", point_size * 10);
    font = XLoadQueryFont(wxAPP_DISPLAY, buffer);
    
    if (!font) /* really last-ditch */
      font = XLoadQueryFont(wxAPP_DISPLAY, "-*-*-*-*-*-*-*-*-*-*-*-*-*-*");
  }

  return font;
}

int wxGetControlFontSize()
{
  return wxNORMAL_FONT->GetPointSize();
}
