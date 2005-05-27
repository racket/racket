/*
 * File:      wb_gdi.cc
 * Purpose:     GDI (Graphics Device Interface) objects and functions
 * Author:      Julian Smart
 * Created:     1993
 * Updated:     August 1994
 * Copyright:   (c) 2004-2005 PLT Scheme, Inc.
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#ifdef __GNUG__
#pragma implementation
#endif
#ifdef __GNUG__
#pragma implementation "FontDirectory.h"
#endif
#endif

#include "common.h"
#include "wx_setup.h"
#include "wx_list.h"
#include "wx_utils.h"
#include "wx_main.h"
#include "wx_gdi.h"
#include "wx_image.h"
#include "wx_dcps.h"

/* For scheme_malloc_atomic: */
#include "scheme.h"

#include "FontDirectory.cxx"


wxbFont::wxbFont (void)
{
  __type = wxTYPE_FONT;
}

/* Constructor for a font. Note that the real construction is done
 * in wxDC::SetFont, when information is available about scaling etc.
 */
wxbFont::wxbFont (int PointSize, int Family, int Style, int Weight, Bool Underline, int smoothing, Bool sip)
{
  __type = wxTYPE_FONT;
}

wxbFont::~wxbFont ()
{
}

char *wxbFont::GetFamilyString(void)
{
  char *fam = NULL;
  switch (GetFamily())
  {
    case wxDECORATIVE:
      fam = "wxDECORATIVE";
      break;
    case wxROMAN:
      fam = "wxROMAN";
      break;
    case wxSCRIPT:
      fam = "wxSCRIPT";
      break;
    case wxSWISS:
      fam = "wxSWISS";
      break;
    case wxMODERN:
      fam = "wxMODERN";
      break;
    case wxTELETYPE:
      fam = "wxTELETYPE";
      break;
    case wxSYSTEM:
      fam = "wxSYSTEM";
      break;
    case wxSYMBOL:
      fam = "wxSYMBOL";
      break;
    default:
      fam = "wxDEFAULT";
      break;
  }
  return fam;
}

char *wxbFont::GetFaceString(void)
{
  return wxTheFontNameDirectory->GetFontName(fontid); 
}

char *wxbFont::GetStyleString(void)
{
  char *styl = NULL;
  switch (GetStyle())
  {
    case wxITALIC:
      styl = "wxITALIC";
      break;
    case wxSLANT:
      styl = "wxSLANT";
      break;
    default:
      styl = "wxNORMAL";
      break;
  }
  return styl;
}

char *wxbFont::GetWeightString(void)
{
  char *w = NULL;
  switch (GetWeight())
  {
    case wxBOLD:
      w = "wxBOLD";
      break;
    case wxLIGHT:
      w = "wxLIGHT";
      break;
    default:
      w = "wxNORMAL";
      break;
  }
  return w;
}

// Colour

wxColour::wxColour (void)
: wxObject(WXGC_NO_CLEANUP)
{
  __type = wxTYPE_COLOUR;
  isInit = FALSE;
  red = 0; green = 0; blue = 0;
  pixel.red = 0;
  pixel.green = 0;
  pixel.blue = 0;
  locked = 0;
}

wxColour::wxColour (unsigned char r, unsigned char g, unsigned char b)
: wxObject(WXGC_NO_CLEANUP)
{
  __type = wxTYPE_COLOUR;
  red = r;
  green = g;
  blue = b;
  isInit = TRUE;
  pixel.red = red << 8;
  pixel.green = green << 8;
  pixel.blue = blue << 8;
  locked = 0;
}

wxColour::wxColour (wxColour *src)
: wxObject(WXGC_NO_CLEANUP)
{
  CopyFrom(src);
}


wxColour::wxColour (const char *col)
: wxObject(WXGC_NO_CLEANUP)
{
  __type = wxTYPE_COLOUR;
  CopyFrom(col);
  locked = 0;
}

wxColour::~wxColour (void)
{
}

wxColour *wxColour::CopyFrom(wxColour *src)
{
  red = src->red;
  green = src->green;
  blue = src->blue;
  pixel = src->pixel;
  isInit = src->isInit;

  pixel.red = red << 8;
  pixel.green = green << 8;
  pixel.blue = blue << 8;

  return this;
}

wxColour *wxColour::CopyFrom(const char *col)
{
  wxColour *the_colour;
  the_colour = wxTheColourDatabase->FindColour (col);
  if (the_colour) {
    red = the_colour->Red ();
    green = the_colour->Green ();
    blue = the_colour->Blue ();
    pixel = the_colour->pixel;
    isInit = TRUE;
  } else {
    red = 0;
    green = 0;
    blue = 0;
    isInit = FALSE;
  }

  return this;
}

void wxColour::Set (unsigned char r, unsigned char g, unsigned char b)
{
  red = r;
  green = g;
  blue = b;
  isInit = TRUE;

  pixel.red = red << 8;
  pixel.green = green << 8;
  pixel.blue = blue << 8;
}

void wxColour::Get (unsigned char *r, unsigned char *g, unsigned char *b)
{
  *r = red;
  *g = green;
  *b = blue;
}

wxColourDatabase::wxColourDatabase()
  : wxList(wxKEY_STRING)
{
}

wxColourDatabase::~wxColourDatabase (void)
{
}

// Colour database stuff
void wxColourDatabase::Initialize (void)
{
  // Don't initialize for X: colours are found
  // in FindColour below.
#if defined(wx_msw) || defined(wx_mac)
#define APPEND_C(name, c) tmpc = c; tmpc->Lock(1); Append(name, tmpc)
wxColor *tmpc;
#include "DBColors.inc"
#endif
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
  wxNode *node;
  int q;

  // Insure upcased:
  for (q = 0; colour[q] && !islower(colour[q]); q++) {
  }

  if (colour[q]) {
    char *naya;
    naya = new char[strlen(colour) + 1];
    for (q = 0; colour[q]; q++) {
      naya[q] = toupper(colour[q]);
    }
    naya[q] = 0;
    colour = naya;
  }

  node = Find(colour);
  if (node)
    return (wxColour *)(node->Data());
  else 
    return NULL;
}

char *wxColourDatabase::FindName (wxColour *colour)
{
  unsigned char red, green, blue;
  wxNode *node;
  wxColour *col;

  red = colour->Red();
  green = colour->Green();
  blue = colour->Blue();

  for (node = First (); node; node = node->Next ()) {
    col = (wxColour *) (node->Data ());
    if (col->Red () == red && col->Green () == green && col->Blue () == blue) {
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
  SetOutlinePreferred(TRUE);

  wxREGGLOB(wxTheBrushList);
  wxTheBrushList = new wxBrushList;
  wxREGGLOB(wxThePenList);
  wxThePenList = new wxPenList;
  wxREGGLOB(wxTheFontList);
  wxTheFontList = new wxFontList;

  {
    Str255 name;
    SInt16 big_size, small_size;
    Style style, small_style;

    GetThemeFont(kThemeSystemFont,
		 smSystemScript,
		 name,
		 &big_size,
		 &style);
    GetThemeFont(kThemeSmallSystemFont,
		 smSystemScript,
		 name,
		 &small_size,
		 &small_style);

    wxREGGLOB(wxNORMAL_FONT);
    wxNORMAL_FONT = new wxFont (big_size, wxSYSTEM, wxNORMAL, wxNORMAL);
    wxREGGLOB(wxSMALL_FONT);
    wxSMALL_FONT = new wxFont (small_size, wxSWISS, wxNORMAL, wxNORMAL);
    wxREGGLOB(wxITALIC_FONT);
    wxITALIC_FONT = new wxFont (big_size, wxROMAN, wxITALIC, wxNORMAL);
    wxREGGLOB(wxSWISS_FONT);
    wxSWISS_FONT = new wxFont (big_size, wxSWISS, wxNORMAL, wxNORMAL);
  }

  wxREGGLOB(wxRED_PEN);
  wxRED_PEN = new wxPen ("RED", 0, wxSOLID);
  wxREGGLOB(wxCYAN_PEN);
  wxCYAN_PEN = new wxPen ("CYAN", 0, wxSOLID);
  wxREGGLOB(wxGREEN_PEN);
  wxGREEN_PEN = new wxPen ("GREEN", 0, wxSOLID);
  wxREGGLOB(wxBLACK_PEN);
  wxBLACK_PEN = new wxPen ("BLACK", 0, wxSOLID);
  wxREGGLOB(wxWHITE_PEN);
  wxWHITE_PEN = new wxPen ("WHITE", 0, wxSOLID);
  wxREGGLOB(wxTRANSPARENT_PEN);
  wxTRANSPARENT_PEN = new wxPen ("BLACK", 0, wxTRANSPARENT);
  wxREGGLOB(wxBLACK_DASHED_PEN);
  wxBLACK_DASHED_PEN = new wxPen ("BLACK", 0, wxSHORT_DASH);
  wxREGGLOB(wxGREY_PEN);
  wxGREY_PEN = new wxPen ("GRAY", 0, wxSOLID);
  wxREGGLOB(wxMEDIUM_GREY_PEN);
  wxMEDIUM_GREY_PEN = new wxPen ("MEDIUM GRAY", 0, wxSOLID);
  wxREGGLOB(wxLIGHT_GREY_PEN);
  wxLIGHT_GREY_PEN = new wxPen ("LIGHT GRAY", 0, wxSOLID);

  wxWHITE_PEN->Lock(1);
  wxBLACK_PEN->Lock(1);

  wxREGGLOB(wxBLUE_BRUSH);
  wxBLUE_BRUSH = new wxBrush ("BLUE", wxSOLID);
  wxREGGLOB(wxGREEN_BRUSH);
  wxGREEN_BRUSH = new wxBrush ("GREEN", wxSOLID);
  wxREGGLOB(wxWHITE_BRUSH);
  wxWHITE_BRUSH = new wxBrush ("WHITE", wxSOLID);
  wxREGGLOB(wxBLACK_BRUSH);
  wxBLACK_BRUSH = new wxBrush ("BLACK", wxSOLID);
  wxREGGLOB(wxTRANSPARENT_BRUSH);
  wxTRANSPARENT_BRUSH = new wxBrush ("BLACK", wxTRANSPARENT);
  wxREGGLOB(wxCYAN_BRUSH);
  wxCYAN_BRUSH = new wxBrush ("CYAN", wxSOLID);
  wxREGGLOB(wxRED_BRUSH);
  wxRED_BRUSH = new wxBrush ("RED", wxSOLID);
  wxREGGLOB(wxGREY_BRUSH);
  wxGREY_BRUSH = new wxBrush ("GRAY", wxSOLID);
  wxREGGLOB(wxMEDIUM_GREY_BRUSH);
  wxMEDIUM_GREY_BRUSH = new wxBrush ("MEDIUM GRAY", wxSOLID);
  wxREGGLOB(wxLIGHT_GREY_BRUSH);
  wxLIGHT_GREY_BRUSH = new wxBrush ("LIGHT GRAY", wxSOLID);
  
  wxWHITE_BRUSH->Lock(1);
  wxBLACK_BRUSH->Lock(1);
  
  {
    wxColour *ctlGray;
    ctlGray = new wxColour(0xE8, 0xE8, 0xE8);
    wxREGGLOB(wxCONTROL_BACKGROUND_BRUSH);
    wxCONTROL_BACKGROUND_BRUSH = new wxBrush(ctlGray, wxSOLID);
    wxCONTROL_BACKGROUND_BRUSH->Lock(1);
  }

  wxREGGLOB(wxBLACK);
  wxBLACK = new wxColour ("BLACK");
  wxREGGLOB(wxWHITE);
  wxWHITE = new wxColour ("WHITE");
  wxREGGLOB(wxRED);
  wxRED = new wxColour ("RED");
  wxREGGLOB(wxBLUE);
  wxBLUE = new wxColour ("BLUE");
  wxREGGLOB(wxGREEN);
  wxGREEN = new wxColour ("GREEN");
  wxREGGLOB(wxCYAN);
  wxCYAN = new wxColour ("CYAN");
  wxREGGLOB(wxLIGHT_GREY);
  wxLIGHT_GREY = new wxColour ("LIGHT GRAY");

  wxREGGLOB(wxSTANDARD_CURSOR);
  wxSTANDARD_CURSOR = new wxCursor (wxCURSOR_ARROW);
  wxREGGLOB(wxHOURGLASS_CURSOR);
  wxHOURGLASS_CURSOR = new wxCursor (wxCURSOR_WATCH);
  wxREGGLOB(wxCROSS_CURSOR);
  wxCROSS_CURSOR = new wxCursor (wxCURSOR_CROSS);
  wxREGGLOB(wxIBEAM_CURSOR);
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

wxbPen::wxbPen (wxColour *col, double Width, int Style)
{
  locked = 0;
  __type = wxTYPE_PEN;
}

wxbPen::wxbPen (const char *col, double Width, int Style)
{
  locked = 0;
  __type = wxTYPE_PEN;
}

int wxbPen::GetWidth (void)
{
  return (int)width;
}

double wxbPen::GetWidthF (void)
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
  locked = 0;
}

wxbBrush::~wxbBrush ()
{
  if (stipple)
    --stipple->selectedIntoDC;
}

wxbBrush::wxbBrush (wxColour *col, int Style)
{
  __type = wxTYPE_BRUSH;
  locked = 0;
}

wxbBrush::wxbBrush (char *col, int Style)
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
  list->Show(pen, -1);
  pen->Lock(1);
}

wxPen *wxPenList::FindOrCreatePen (wxColour * colour, double width, int style)
{
  wxPen *pen;
  wxChildNode *node;
  int i = 0;
  wxPen *each_pen;

  if (!colour)
    return NULL;

  while ((node = list->NextNode(i))) {
    each_pen = (wxPen *) (node->Data ());
    if (each_pen) {
      wxColour *c;
      c = each_pen->GetColour();
      if (each_pen->GetWidthF() == width &&
	  each_pen->GetStyle() == style &&
	  c->Red () == colour->Red () &&
	  c->Green () == colour->Green () &&
	  c->Blue () == colour->Blue ())
	return each_pen;
    }
  }
  pen = new wxPen (colour, width, style);
  AddPen(pen);
  return pen;
}

wxPen *wxPenList::FindOrCreatePen (char *colour, double width, int style)
{
  wxColour *the_colour;
  the_colour = wxTheColourDatabase->FindColour(colour);
  if (the_colour)
    return FindOrCreatePen (the_colour, width, style);
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

void wxBrushList::AddBrush (wxBrush * brush)
{
  brush->Lock(1);
  list->Append(brush);
  list->Show(brush, -1);
}

wxBrush *wxBrushList::FindOrCreateBrush (wxColour * colour, int style)
{
  wxBrush *brush;
  wxChildNode *node;
  int i = 0;
  wxBrush *each_brush;

  if (!colour)
    return NULL;

  while ((node = list->NextNode(i))) {
    each_brush = (wxBrush *) (node->Data ());
    if (each_brush) {
      wxColour *c;
      c = each_brush->GetColour();
      if (each_brush->GetStyle() == style &&
	  c->Red() == colour->Red() &&
	  c->Green() == colour->Green() &&
	  c->Blue() == colour->Blue())
	return each_brush;
    }
  }
  brush = new wxBrush (colour, style);
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


wxFontList::wxFontList(void)
 : wxObject(WXGC_NO_CLEANUP)
{
  list = new wxChildList;
}

wxFontList::~wxFontList(void)
{
}

void wxFontList::AddFont (wxFont * font)
{
  list->Append(font);
  list->Show(font, -1);
}

wxFont *wxFontList::
FindOrCreateFont (int PointSize, int FamilyOrFontId, int Style, int Weight, Bool underline, int smoothing, Bool sip)
{
  wxFont *font;
  wxChildNode *node;
  int i = 0;
  wxFont *each_font;

  while ((node = list->NextNode(i))) {
    each_font = (wxFont *) ( node->Data ());
    if (each_font &&
	each_font->GetPointSize () == PointSize &&
	each_font->GetStyle () == Style &&
	each_font->GetWeight () == Weight &&
	each_font->GetFontId () == FamilyOrFontId &&
	each_font->GetUnderlined () == underline &&
	each_font->GetSmoothing () == smoothing &&
	each_font->GetSizeInPixels () == sip)
      return each_font;
  }
  font = new wxFont (PointSize, FamilyOrFontId, Style, Weight, underline, smoothing, sip);
  AddFont(font);
  return font;
}

wxFont *wxFontList::
FindOrCreateFont (int PointSize, const char *Face, int Family, int Style, int Weight, Bool underline, int smoothing, Bool sip)
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

#if (!USE_TYPEDEFS)
wxPoint::wxPoint (void) : wxObject(WXGC_NO_CLEANUP)
{
}

wxPoint::wxPoint (double the_x, double the_y)  : wxObject(WXGC_NO_CLEANUP)
{
  x = the_x;
  y = the_y;
}

wxPoint::~wxPoint (void)
{
}
#endif

#if (!USE_TYPEDEFS)
wxIntPoint::wxIntPoint (void)  : wxObject(WXGC_NO_CLEANUP)
{
}

wxIntPoint::wxIntPoint (int the_x, int the_y)  : wxObject(WXGC_NO_CLEANUP)
{
  x = the_x;
  y = the_y;
}

wxIntPoint::~wxIntPoint (void)
{
}
#endif

#include "Region.h"
#include "Region.cxx"

