/*
 * File:      wb_gdi.cc
 * Purpose:     GDI (Graphics Device Interface) objects and functions
 * Author:      Julian Smart
 * Created:     1993
 * Updated:     August 1994
 * Copyright:   (c) 2004-2010 PLT Scheme Inc.
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include "wx_graphics.h"

#include "../../../wxcommon/Region.h"

#include <stdio.h>
#include <ctype.h>
#include <math.h>

wxbFont::wxbFont (void)
{
  __type = wxTYPE_FONT;
}

/* Constructor for a font. Note that the real construction is done
 * in wxDC::SetFont, when information is available about scaling etc.
 */
wxbFont::wxbFont (int WXUNUSED(PointSize), int WXUNUSED(Family), int WXUNUSED(Style), int WXUNUSED(Weight), Bool WXUNUSED(Underline), int WXUNUSED(smoothing), Bool WXUNUSED(sip), double WXUNUSED(Rotation))
{
  __type = wxTYPE_FONT;
}

wxbFont::~wxbFont ()
{
}

char *wxbFont::GetFaceString(void)
{
  /* If it's one of the portable faceless fonts, return NULL */
  switch (fontid)
  {
  case wxDECORATIVE:
  case wxROMAN:
  case wxSCRIPT:
  case wxSWISS:
  case wxMODERN:
  case wxTELETYPE:
  case wxSYSTEM:
  case wxSYMBOL:
    return NULL;
  default:
    return wxTheFontNameDirectory->GetFontName(fontid); 
  }
}

// Colour

wxColour::wxColour (void)
: wxObject(WXGC_NO_CLEANUP)
{
  __type = wxTYPE_COLOUR;
  isInit = FALSE;
  locked = 0;
  pixel = 0;
}

wxColour::wxColour (const unsigned char r, const unsigned char g, const unsigned char b)
: wxObject(WXGC_NO_CLEANUP)
{
  __type = wxTYPE_COLOUR;
  red = r;
  green = g;
  blue = b;
  isInit = TRUE;
  pixel = RGB (red, green, blue);
}

wxColour::wxColour (const wxColour *col)
: wxObject(WXGC_NO_CLEANUP)
{
  __type = wxTYPE_COLOUR;
  locked = 0;
  CopyFrom(col);
}

wxColour* wxColour::CopyFrom(const wxColour* col)
{
  red = col->red;
  green = col->green;
  blue = col->blue;
  isInit = col->isInit;
  pixel = col->pixel;
  return this;
}

#ifndef MZ_PRECISE_GC
wxColour& wxColour::operator=(const wxColour& col)
{
  /* This method should never be called! */
  return *(CopyFrom(&col));
}
#endif

wxColour::wxColour (const char *col)
: wxObject(WXGC_NO_CLEANUP)
{
  __type = wxTYPE_COLOUR;
  locked = 0;

  CopyFrom(col);
}

wxColour::~wxColour (void)
{
}

wxColour* wxColour::CopyFrom(const char *col)
{
  wxColour *the_colour;

  the_colour = wxTheColourDatabase->FindColour (col);

  if (the_colour) {
    red = the_colour->Red ();
    green = the_colour->Green ();
    blue = the_colour->Blue ();
    isInit = TRUE;
  } else {
    red = 0;
    green = 0;
    blue = 0;
    isInit = FALSE;
  }
  pixel = RGB (red, green, blue);

  return this;
}

void wxColour::Set (unsigned char r, unsigned char g, unsigned char b)
{
  red = r;
  green = g;
  blue = b;
  isInit = TRUE;
  pixel = RGB (red, green, blue);

}

void wxColour::Get (unsigned char *r, unsigned char *g, unsigned char *b)
{
  *r = red;
  *g = green;
  *b = blue;
}

wxColourDatabase::wxColourDatabase (int type):
wxList ((KeyType)type)
{
}

wxColourDatabase::~wxColourDatabase (void)
{
  // Cleanup Colour allocated in Initialize()
  wxNode *node;
  node = First();
  while (node) {
    wxColour *col;
    wxNode *next;
    col = (wxColour *) node->Data ();
    next = node->Next ();
    delete col;
    node = next;
  }
}

// Colour database stuff
void wxColourDatabase::Initialize (void)
{
  // Don't initialize into regular map for X: colours are found
  // in FindColour below. But to ensure that all of these names
  // are present, add them to an auxiliary list:

# define APPEND_TO_DB Append

  wxColour *tmpc;
#define APPEND_C(name, c) tmpc = c; tmpc->Lock(1); APPEND_TO_DB(name, tmpc);
# include "../../../wxcommon/DBColors.inc"
}

/*
 * Changed by Ian Brown, July 1994.
 *
 * When running under X, the Colour Database starts off empty. The X server
 * is queried for the colour first time after which it is entered into the
 * database. This allows our client to use the server colour database which
 * is hopefully gamma corrected for the display being used.
 */

wxColour *wxColourDatabase::FindColour(const char *colour)
{
  // Force capital so lc matches as in X
  char uc_colour[256];
  int i;
  wxNode *node;

  for (i = 0; colour[i] && i < 255; i++) {
    uc_colour[i] = colour[i];
    if ((uc_colour[i] >= 'a') && (uc_colour[i] <= 'z'))
      uc_colour[i] -= ('a' - 'A');
  }
  uc_colour[i] = 0;
  colour = uc_colour;

  node = Find(colour);
  if (node)
    return (wxColour *)node->Data();

  return NULL;
}

char *wxColourDatabase::FindName (wxColour * colour)
{
  unsigned char red, rd;
  unsigned char green, gn;
  unsigned char blue, bl;
  wxNode * node;

  red = colour->Red();
  green = colour->Green();
  blue = colour->Blue();

  for (node = First(); node; node = node->Next ()) {
    wxColour *col;
    col = (wxColour *)node->Data();
    rd = col->Red();
    gn = col->Green();
    bl = col->Blue();
    if (rd == red && gn == green && bl == blue) {
      char *found = node->string_key;
      if (found)
	return found;
    }
  }
  return NULL;			// Not Found

}


void 
wxInitializeStockObjects (void)
{
  wxREGGLOB(wxTheBrushList);
  wxREGGLOB(wxThePenList);
  wxREGGLOB(wxTheFontList);

  wxREGGLOB(wxNORMAL_FONT);

  wxREGGLOB(wxBLACK_PEN);

  wxREGGLOB(wxWHITE_BRUSH);
  wxREGGLOB(wxBLACK_BRUSH);

  wxREGGLOB(wxBLACK);
  wxREGGLOB(wxWHITE);

  wxREGGLOB(wxSTANDARD_CURSOR);
  wxREGGLOB(wxHOURGLASS_CURSOR);
  wxREGGLOB(wxCROSS_CURSOR);
  wxREGGLOB(wxIBEAM_CURSOR);

  wxTheBrushList = new wxBrushList;
  wxThePenList = new wxPenList;
  wxTheFontList = new wxFontList;

  wxNORMAL_FONT = new wxFont (12, wxSYSTEM, wxNORMAL, wxNORMAL);

  wxBLACK_PEN = new wxPen ("BLACK", 0, wxSOLID);

  wxBLACK_PEN->Lock(1);

  wxWHITE_BRUSH = new wxBrush ("WHITE", wxSOLID);
  wxBLACK_BRUSH = new wxBrush ("BLACK", wxSOLID);

  wxWHITE_BRUSH->Lock(1);
  wxBLACK_BRUSH->Lock(1);

  wxBLACK = new wxColour ("BLACK");
  wxWHITE = new wxColour ("WHITE");

  wxSTANDARD_CURSOR = new wxCursor (wxCURSOR_ARROW);
  wxHOURGLASS_CURSOR = new wxCursor (wxCURSOR_WAIT);
  wxCROSS_CURSOR = new wxCursor (wxCURSOR_CROSS);
  wxIBEAM_CURSOR = new wxCursor (wxCURSOR_IBEAM);
}

void 
wxDeleteStockObjects (void)
{
}

// Pens

wxbPen::wxbPen (void)
{
  __type = wxTYPE_PEN;
  locked = 0;
}

wxbPen::~wxbPen ()
{
  if (stipple)
    --stipple->selectedIntoDC;
}

wxbPen::wxbPen (wxColour * WXUNUSED(col), double WXUNUSED(Width), int WXUNUSED(Style))
{
  __type = wxTYPE_PEN;
  locked = 0;
}

wxbPen::wxbPen (const char *WXUNUSED(col), double WXUNUSED(Width), int WXUNUSED(Style))
{
  __type = wxTYPE_PEN;
  locked = 0;
}

int wxbPen::GetWidth (void)
{
  return (int)width;
}

double wxbPen::GetWidthF(void)
{
  return width;
}

int wxbPen::GetStyle (void)
{
  return style;
}

int wxbPen::GetJoin (void)
{
  return join;
}

wxBitmap *wxbPen::GetStipple (void)
{
  return stipple;
}

int wxbPen::GetCap (void)
{
  return cap;
}

int wxbPen::GetDashes (wxDash ** ptr)
{
  *ptr = dash;
  return nb_dash;
}

wxColour *wxbPen::GetColour (void)
{
  return colour;
}

void wxbPen::SetColour (wxColour *col)
{
  colour->CopyFrom(col);
}

void wxbPen::SetColour (const char *col)
{
  colour->CopyFrom(col);
}

void wxbPen::SetColour (char red, char green, char blue)
{
 colour->Set(red, green, blue);
}

void wxbPen::SetWidth (double Width)
{
  width = Width;
}

void wxbPen::SetCap (int Cap)
{
  cap = Cap;
}

void wxbPen::SetJoin (int Join)
{
  join = Join;
}

void wxbPen::SetStyle (int Style)
{
  style = Style;
}

void wxbPen::SetDashes (int nbDash, wxDash * Dash)
{
  nb_dash = nbDash;
  dash = Dash;
}

void wxbPen::SetStipple (wxBitmap * Stipple)
{
  if (Stipple && (Stipple->selectedIntoDC < 0))
    return;
  if (Stipple)
    Stipple->selectedIntoDC++;
  if (stipple)
    --stipple->selectedIntoDC;

  stipple = Stipple;
}

// Brushes

wxbBrush::wxbBrush (void)
{
  __type = wxTYPE_BRUSH;
}

wxbBrush::~wxbBrush ()
{
  if (stipple)
    --stipple->selectedIntoDC;
}

wxbBrush::wxbBrush (wxColour * WXUNUSED(col), int WXUNUSED(Style))
{
  __type = wxTYPE_BRUSH;
  locked = 0;
}

wxbBrush::wxbBrush (char *WXUNUSED(col), int WXUNUSED(Style))
{
  __type = wxTYPE_BRUSH;
  locked = 0;
}

int wxbBrush::GetStyle (void)
{
  return style;
}

wxBitmap *wxbBrush::GetStipple (void)
{
  return stipple;
}

wxColour *wxbBrush::GetColour (void)
{
  return colour;
}

void wxbBrush::SetColour (wxColour *col)
{
  colour->CopyFrom(col);
}

void wxbBrush::SetColour (const char *col)
{
  colour->CopyFrom(col);
}

void wxbBrush::SetColour (char red, char green, char blue)
{
  colour->Set(red, green, blue);
}

void wxbBrush::SetStyle (int Style)
{
  style = Style;
}

void wxbBrush::SetStipple (wxBitmap * Stipple)
{
  if (Stipple && (Stipple->selectedIntoDC < 0))
    return;
  if (Stipple)
    Stipple->selectedIntoDC++;
  if (stipple)
    --stipple->selectedIntoDC;

  stipple = Stipple;
}

// Pen and Brush lists

wxPenList::wxPenList(void)
: wxObject(WXGC_NO_CLEANUP)
{
  list = new wxChildList;
}

wxPenList::~wxPenList(void)
{
}

void wxPenList::AddPen (wxPen * pen)
{
  list->Append(pen); 
  list->Show(pen, -1); /* so it can be collected */
}

wxPen *wxPenList::FindOrCreatePen (wxColour * colour, double width, int style, int cap, int join)
{
  wxPen *pen;
  int i = 0;
  wxChildNode *node;

  if (!colour)
    return NULL;

  while ((node = list->NextNode(i))) {
    wxPen *each_pen;
    each_pen = (wxPen *) node->Data ();
    if (each_pen &&
	each_pen->GetWidthF() == width &&
	each_pen->GetStyle() == style &&
        each_pen->GetCap() == cap &&
        each_pen->GetJoin() == join) {
      wxColour *col;
      col = each_pen->GetColour();
      if (col->Red () == colour->Red () &&
	  col->Green () == colour->Green () &&
	  col->Blue () == colour->Blue ()) {
	return each_pen;
      }
    }
  }
  pen = new wxPen (colour, width, style, FALSE);

  if (cap != wxCAP_ROUND) pen->SetCap(cap);
  if (join != wxJOIN_ROUND) pen->SetJoin(join);

  pen->Lock(1);

  AddPen(pen);

  return pen;
}

wxPen *wxPenList::FindOrCreatePen (char *colour, double width, int style, int cap, int join)
{
  wxColour *the_colour;
  the_colour = wxTheColourDatabase->FindColour (colour);
  if (the_colour)
    return FindOrCreatePen (the_colour, width, style, cap, join);
  else
    return NULL;
}

wxBrushList::wxBrushList(void)
: wxObject(WXGC_NO_CLEANUP)
{
  list = new wxChildList;
}

wxBrushList::~wxBrushList(void)
{
}

void wxBrushList::AddBrush(wxBrush *Brush) 
{ 
  list->Append(Brush); 
  list->Show(Brush, -1); /* so it can be collected */
} 


wxBrush *wxBrushList::FindOrCreateBrush (wxColour * colour, int style)
{
  wxBrush *brush;
  int i = 0;
  wxChildNode *node;

  if (!colour)
    return NULL;

  while ((node = list->NextNode(i))) {
    wxBrush *each_brush;
    each_brush = (wxBrush *) node->Data ();
    if (each_brush &&
	each_brush->GetStyle() == style) {
      wxColour *col;
      col = each_brush->GetColour();
      if (col->Red () == colour->Red () &&
	  col->Green () == colour->Green () &&
	  col->Blue () == colour->Blue ())
	return each_brush;
    }
  }

  brush = new wxBrush (colour, style, FALSE);

  brush->Lock(1);

  AddBrush(brush);

  return brush;
}

wxBrush *wxBrushList::FindOrCreateBrush (char *colour, int style)
{
  wxColour *the_colour;
  the_colour = wxTheColourDatabase->FindColour (colour);
  if (the_colour)
    return FindOrCreateBrush (the_colour, style);
  else
    return NULL;
}


wxFontList::wxFontList (void)
: wxObject(WXGC_NO_CLEANUP)
{
  list = new wxChildList;
}

wxFontList::~wxFontList (void)
{
}

void wxFontList::AddFont (wxFont * font)
{
  list->Append(font);
  list->Show(font, -1); /* so it can be collected */
}

wxFont *wxFontList::
FindOrCreateFont (int PointSize, int FamilyOrFontId, int Style, int Weight, Bool underline, 
		  int smoothing, Bool sip, double rotation)
{
  wxFont *fnt;
  int i = 0;
  wxChildNode *node;

  while ((node = list->NextNode(i))) {
    wxFont *each_font;
    each_font = (wxFont *) node->Data ();
    if (each_font &&
	each_font->GetPointSize () == PointSize &&
	each_font->GetStyle () == Style &&
	each_font->GetWeight () == Weight &&
	each_font->GetFontId () == FamilyOrFontId &&
	each_font->GetUnderlined () == underline &&
	each_font->GetSmoothing () == smoothing &&
	each_font->GetSizeInPixels () == sip &&
	each_font->GetRotation () == rotation)
      return each_font;
  }

  fnt = new wxFont(PointSize, FamilyOrFontId, Style, Weight, underline, smoothing, sip, rotation, FALSE);

  AddFont(fnt);

  return fnt;
}

wxFont *wxFontList::
FindOrCreateFont (int PointSize, const char *Face, int Family, int Style, int Weight, Bool underline,
		  int smoothing, Bool sip, double rotation)
{
  int fid;
  fid = wxTheFontNameDirectory->FindOrCreateFontId(Face, Family);
  return FindOrCreateFont(PointSize,
			  fid,
			  Style,
			  Weight,
			  underline,
			  smoothing,
			  sip,
			  rotation);
}

wxPoint::wxPoint (void) : wxObject(WXGC_NO_CLEANUP)
{
}

wxPoint::wxPoint (double the_x, double the_y) : wxObject(WXGC_NO_CLEANUP)
{
  x = the_x;
  y = the_y;
}

wxPoint::~wxPoint (void)
{
}

wxIntPoint::wxIntPoint (void) : wxObject(WXGC_NO_CLEANUP)
{
}

wxIntPoint::wxIntPoint (int the_x, int the_y) : wxObject(WXGC_NO_CLEANUP)
{
  x = the_x;
  y = the_y;
}

wxIntPoint::~wxIntPoint (void)
{
}

/**************************************************************************/

#include "../../../wxcommon/FontDirectory.cxx"

/**************************************************************************/

#ifdef wx_msw

class LazyRgn : public wxObject {  
public:
  int used_refcount;
  HRGN cached_rgn, used_rgn;
  LazyRgn *lazy_cache_prev, *lazy_cache_next;

  LazyRgn();
  HRGN GetRgn();
  virtual HRGN DetatchRgn(HRGN rgn);
  virtual void DoneRgn(HRGN rgn);
  virtual HRGN ToRegion();

  void Chain();
  void Unchain();
};

static LazyRgn *lazy_rgn_cache;
int lazy_cache_count;
#define MAX_CACHE_SIZE 100

LazyRgn::LazyRgn() : wxObject(WXGC_NO_CLEANUP) { }

HRGN LazyRgn::DetatchRgn(HRGN rgn)
{ 
  if (rgn) {
    HRGN rgn2;

    rgn2 = CreateRectRgn(0, 0, 1, 1);
    CombineRgn(rgn2, rgn, rgn2, RGN_COPY);

    DoneRgn(rgn);

    return rgn2;
  } else {
    DoneRgn(rgn);
    return NULL;
  }
}

void LazyRgn::DoneRgn(HRGN rgn)
{
  if (rgn && (rgn == used_rgn)) {
    --used_refcount;
    if (!used_refcount) {
      used_rgn = NULL;
      if (rgn) {
	cached_rgn = rgn;
	if (lazy_cache_count >= MAX_CACHE_SIZE) {
	  LazyRgn *l;
	  for (l = lazy_rgn_cache; l->lazy_cache_next; l = l->lazy_cache_next) { }
	  l->Unchain();
	  DeleteObject(l->cached_rgn);
	  l->cached_rgn = NULL;
	}
	Chain();
      }
    }
  } else {
    if (rgn)
      DeleteObject(rgn);
  }
}

HRGN LazyRgn::GetRgn()
{
  if (used_rgn) {
  } else if (cached_rgn) {
    used_rgn = cached_rgn;
    Unchain();
    cached_rgn = NULL;
  } else {
    used_rgn = ToRegion();
  }

  if (used_rgn)
    used_refcount++;

  return used_rgn;
}

HRGN LazyRgn::ToRegion() { return NULL; }

void LazyRgn::Chain()
{
  lazy_cache_next = lazy_rgn_cache;
  lazy_cache_prev = NULL;
  lazy_rgn_cache = this;
  if (lazy_cache_next)
    lazy_cache_next->lazy_cache_prev = this;
  lazy_cache_count++;
}

void LazyRgn::Unchain()
{
  if (lazy_cache_next)
    lazy_cache_next->lazy_cache_prev = lazy_cache_prev;
  if (lazy_cache_prev)
    lazy_cache_prev->lazy_cache_next = lazy_cache_next;
  else
    lazy_rgn_cache = lazy_cache_next;
  --lazy_cache_count;
}

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

void wx_release_lazy_regions(void)
{
  LazyRgn *l;
  for (l = lazy_rgn_cache; l; l = l->lazy_cache_next) {
    DeleteObject(l->cached_rgn);
    l->cached_rgn = NULL;
  }
  lazy_rgn_cache = NULL;
  lazy_cache_count = 0;
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

/* - - -  - - -  - - -  - - -  - - -  - - -  */

class RectLazyRgn : public LazyRgn {  
public:
  int ix, iy, iw, ih;
  RectLazyRgn(int _ix, int _iy, int _iw, int _ih);
  virtual HRGN ToRegion();
};

RectLazyRgn::RectLazyRgn(int _ix, int _iy, int _iw, int _ih) {
  ix = _ix;
  iy = _iy;
  iw = _iw;
  ih = _ih;
}

HRGN RectLazyRgn::ToRegion()
{
  return CreateRectRgn(ix, iy, ix + iw, iy + ih);
}

/* - - -  - - -  - - -  - - -  - - -  - - -  */

class RoundRectLazyRgn : public LazyRgn {  
public:
  int ix, iy, iw, ih, xradius, yradius;
  RoundRectLazyRgn(int _ix, int _iy, int _iw, int _ih, int _xr, int _yr);
  virtual HRGN ToRegion();
};

RoundRectLazyRgn::RoundRectLazyRgn(int _ix, int _iy, int _iw, int _ih, int _xradius, int _yradius) {
  ix = _ix;
  iy = _iy;
  iw = _iw;
  ih = _ih;
  xradius = _xradius;
  yradius = _yradius;
}

HRGN RoundRectLazyRgn::ToRegion()
{
  return CreateRoundRectRgn(ix, iy, ix + iw, iy + ih, xradius, yradius);
}

/* - - -  - - -  - - -  - - -  - - -  - - -  */

class EllipticLazyRgn : public LazyRgn {  
public:
  int ix, iy, iw, ih;
  EllipticLazyRgn(int _ix, int _iy, int _iw, int _ih);
  virtual HRGN ToRegion();
};

EllipticLazyRgn::EllipticLazyRgn(int _ix, int _iy, int _iw, int _ih) {
  ix = _ix;
  iy = _iy;
  iw = _iw;
  ih = _ih;
}

HRGN EllipticLazyRgn::ToRegion()
{
  return CreateEllipticRgn(ix, iy, ix + iw, iy + ih);
}

/* - - -  - - -  - - -  - - -  - - -  - - -  */

class PolygonLazyRgn : public LazyRgn {  
public:
  POINT *cpoints;
  int n, mode;
  PolygonLazyRgn(POINT *_cpoints, int _n, int _mode);
  virtual HRGN ToRegion();
};

PolygonLazyRgn::PolygonLazyRgn(POINT *_cpoints, int _n, int _mode)
{
  cpoints = _cpoints;
  n = _n;
  mode = _mode;
}

HRGN PolygonLazyRgn::ToRegion()
{
  return CreatePolygonRgn(cpoints, n, mode);
}

/* - - -  - - -  - - -  - - -  - - -  - - -  */

class UnionLazyRgn : public LazyRgn {  
public:
  LazyRgn *a, *b;
  int mode;
  UnionLazyRgn(LazyRgn *_a, LazyRgn *_b, int _mode);
  virtual HRGN ToRegion();
};

UnionLazyRgn::UnionLazyRgn(LazyRgn *_a, LazyRgn *_b, int _mode)
{
  a = _a;
  b = _b;
  mode = _mode;
}

HRGN UnionLazyRgn::ToRegion()
{
  HRGN ar, br;
  ar = a->GetRgn();
  br = b->GetRgn();
  ar = a->DetatchRgn(ar);
  CombineRgn(ar, ar, br, mode);
  b->DoneRgn(br);
  return ar;
}

#endif

/**************************************************************************/

#include "../../../wxcommon/Region.cxx"
