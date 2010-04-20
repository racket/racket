/*
 * File:      wb_ps.cc
 * Purpose:     Device context implementation (PostScript)
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	August 1994
 * Copyright:   (c) 2004-2010 PLT Scheme Inc.
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

/* This file is the same for all three version of wxWindows from
   PLT. */

#if defined(_MSC_VER)
# include "wx.h"
#else

#ifdef wx_xt
# define  Uses_XLib
# define  Uses_wxList
# define  Uses_wxWindowDC
# define  Uses_wxMemoryDC
# define  Uses_wxPostScriptDC
# define  Uses_wxPrintSetup
# define  Uses_wxFontNameDirectory
# define  Uses_wxDialogBox
# define  Uses_wxButton
# define  Uses_wxRadioBox
# define  Uses_wxText
# define  Uses_wxChoice
# define  Uses_wxCheckBox
# include "wx.h"

# include <math.h>
# include <string.h>

#else

# ifdef __GNUG__
# pragma implementation "wx_dcps.h"
# pragma implementation
# pragma interface
#endif

# include "common.h"
# include "wx_frame.h"
# include "wx_dcps.h"
# include "wx_dcmem.h"
# include "wx_utils.h"
# include "wx_dialg.h"
# include "wx_cmdlg.h"
# include "wx_main.h"
# include "wx_lbox.h"
# include "wx_rbox.h"
# include "wx_buttn.h"
# include "wx_choic.h"
# include "wx_check.h"
# include "wx_messg.h"

#ifdef wx_mac
# include "wx_print.h"
#endif

#endif
#endif

#include "wx_rgn.h"
#include "../racket/include/scheme.h"

extern void *wxPostScriptDrawText(Scheme_Object *f, const char *fontname, 
                                  const char *text, int dt, Bool combine, int use16, 
                                  double font_size, int symbol_map, void *used_fonts);
extern void wxPostScriptGetTextExtent(const char *fontname, 
				      const char *text, int dt, int len, Bool combine, int use16, 
				      double font_size,
				      double *x, double *y, double *descent, double *topSpace,
				      int symbol_map);
extern char *wxPostScriptFixupFontName(const char *fontname);
extern Bool wxPostScriptGlyphExists(const char *fontname, int c, int symbol_map);
extern void *wxPostScriptRecordFont(const char *fontname, void *used_fonts);
extern char *wxPostScriptFontsToString(void *used_fonts);

# define YSCALE(y) ((paper_h) - ((y) * user_scale_y + device_origin_y))
# define XSCALE(x) ((x) * user_scale_x + device_origin_x)
# define YOFFSET(y) ((paper_h) - ((y) + device_origin_y))
# define XOFFSET(x) ((x) + device_origin_x)
# define YSCALEREL(dy) ((dy) * user_scale_y)
# define XSCALEREL(dx) ((dx) * user_scale_x)
# define XSCALEBND(dx) (XSCALEREL(dx) + device_origin_x)
# define YSCALEBND(dy) (YSCALEREL(dy) + device_origin_y)

# define ASCALEREL(a) ((a) * ascale)

# define PIE pie

#define RESET_FONT 0x1
#define RESET_COLOR 0x2

static double pie = 0.0;

#ifndef WXUNUSED
# define WXUNUSED(x) x
#endif

#ifdef wx_xt
# define WXXTUNUSED(c) /* empty */
#else
# define WXXTUNUSED(x) x
#endif

#define DEFAULT_PAPER "Letter 8 1/2 x 11 in"

class wxCanvas;

#ifdef wx_msw
# include "wx_privt.h"
#endif

#include <math.h>
#include <stdlib.h>
#include <limits.h>

static char *default_afm_path = NULL;

Bool XPrinterDialog(wxWindow *parent);

#ifdef wx_mac
wxPrintPaperDatabase *wxThePrintPaperDatabase;
#endif

#ifndef wx_xt
# define current_font font
#else
# define current_bk_mode current_text_bgmode
# define current_text_foreground current_text_fg
# define current_text_background current_text_bg
#endif

/**************************************************/

wxPSStream::wxPSStream(char *file) {
  Scheme_Object *o;
  f_in = scheme_open_input_output_file(file, "post-script-dc%", &o);
  f = o;
  int_width = 0;
}
 
wxPSStream::~wxPSStream(void) {
  if (f_in) {
    scheme_close_input_port((Scheme_Object *)f_in);
    f_in = NULL;
  }
  if (f) {
    scheme_close_output_port((Scheme_Object *)f);
    f = NULL;
  }
}

int wxPSStream::good(void) {
  return !!f;
}

void wxPSStream::Out(char s) {
  char s2[2];
  s2[0] = s;
  s2[1] = 0;
  Out(s2);
}

void wxPSStream::Out(const char *s) {
  scheme_put_byte_string("post-script-dc%", (Scheme_Object *)f, s, 0, strlen(s), 0);
}

void wxPSStream::Out(int i) {
  Out((long)i);
}

void wxPSStream::Out(double n)
{
  char buf[64];

  if ((double)(long)n == n) {
    Out((long)n);
    return;
  }
  sprintf(buf, "%f", n);
  Out(buf);
}

void wxPSStream::Out(long l)
{
  char buf[64];

  if (int_width > 0) {
    char buffer[50];
    sprintf(buffer, "%% %d.%dld", int_width, int_width);
    sprintf(buf, buffer, l);
    int_width = 0;
  } else
    sprintf(buf, "%ld", l);
  Out(buf);
}

void wxPSStream::flush() {
  scheme_flush_output((Scheme_Object *)f);
}

long wxPSStream::tellp(void) {
  return scheme_set_file_position((Scheme_Object *)f, -1);
}
 
void wxPSStream::seekp(long pos) {
  scheme_set_file_position((Scheme_Object *)f, pos);
}

void wxPSStream::width(int w) {
  int_width = w;
}

long wxPSStream::read_at(long pos, char *buf, long amt) {
  scheme_set_file_position((Scheme_Object *)f_in, pos);
  return scheme_get_byte_string("post-script%", (Scheme_Object *)f_in,
                                buf, 0, amt,
                                0, 0, NULL);
}


/**************************************************/


wxPostScriptDC::wxPostScriptDC (Bool interactive, wxWindow *parent, Bool usePaperBBox, Bool asEPS)
{
  Create(interactive, parent, usePaperBBox, asEPS);
}

Bool wxPostScriptDC::Create(Bool interactive, wxWindow *parent, Bool usePaperBBox, Bool asEPS)
{
  wxPrintSetupData *wxThePrintSetupData;
  char *paperType;
  wxPrintPaperType *paper;

  if (!pie)
    pie = 2 * asin((double)1.0);

  __type = wxTYPE_DC_POSTSCRIPT;
#ifndef wx_xt
  wx_interactive = interactive;
#endif
  current_font = wxNORMAL_FONT;
  device = wxDEVICE_EPS;
  clipping = NULL;

#ifndef wx_xt
# ifdef wx_mac
  logical_origin_x = 0;
  logical_origin_y = 0;
  logical_scale_x = 1.0;
  logical_scale_y = 1.0;
# endif

  device_origin_x = 0;
  device_origin_y = 0;

  user_scale_x = 1.0;
  user_scale_y = 1.0;

  current_pen = NULL;
  current_brush = NULL;
  current_background_color = new WXGC_PTRS wxColour(wxWHITE);

  current_text_foreground = new WXGC_PTRS wxColour(wxBLACK);

  mapping_mode = MM_TEXT;
#else
  current_pen = wxBLACK_PEN;
  current_pen->Lock(1);
  current_brush = wxWHITE_BRUSH;
  current_brush->Lock(1);
  current_background_color->CopyFrom(wxWHITE);
#endif

  title = NULL;

  filename = NULL;

  pstream = NULL;

  min_x = 10000.0;
  min_y = 10000.0;
  max_x = -10000.0;
  max_y = -10000.0;

  clipx = -100000.0;
  clipy = -100000.0;
  clipw = 200000.0;
  cliph = 200000.0;

  as_eps = asEPS;
  ok = PrinterDialog(interactive, parent, usePaperBBox);

  /* We set these even if !ok, for use with text sizing: */
  wxThePrintSetupData = wxGetThePrintSetupData();
  level2ok = wxThePrintSetupData->GetLevel2();
  afm_path = wxThePrintSetupData->GetAFMPath();

  if (!ok)
    return FALSE;

  currentRed = 0;
  currentGreen = 0;
  currentBlue = 0;

  Colour = TRUE;
  
  paperType = wxThePrintSetupData->GetPaperName();
  if (!paperType)
    paperType = DEFAULT_PAPER;

  paper = wxThePrintPaperDatabase->FindPaperType(paperType);
  if (!paper)
    paper = wxThePrintPaperDatabase->FindPaperType(DEFAULT_PAPER);
  if (paper) {
    paper_w = (double)paper->widthPixels;
    paper_h = (double)paper->heightPixels;
  } else {
    paper_w = 1000;
    paper_h = 1000;
  }

  if (wxThePrintSetupData) {
    wxThePrintSetupData->GetPrinterTranslation(&paper_x, &paper_y);
    wxThePrintSetupData->GetPrinterScaling(&paper_x_scale, &paper_y_scale);
    if (wxThePrintSetupData->GetPrinterOrientation() == PS_LANDSCAPE)
      landscape = 1;
    else
      landscape = 0;
    wxThePrintSetupData->GetMargin(&paper_margin_x, &paper_margin_y);
  } else {
    paper_x = paper_y = 0;
    paper_x_scale = paper_y_scale = 1;
    paper_margin_x = paper_margin_y = 0;
    landscape = 0;
  }

  if (landscape) {
    double tmp;

    tmp = paper_w;
    paper_w = paper_h;
    paper_h = tmp;
  }

  paper_w -= (paper_margin_x * 2);
  paper_h -= (paper_margin_y * 2);

  paper_w /= paper_x_scale;
  if (paper_w <= 0)
    paper_w = 1;
  paper_h /= paper_y_scale;
  if (paper_h <= 0)
    paper_h = 1;

  anti_alias = 1;

  return ok;
}

wxPostScriptDC::~wxPostScriptDC (void)
{
  if (current_brush) current_brush->Lock(-1);
  if (current_pen) current_pen->Lock(-1);

  if (pstream)
    DELETE_OBJ pstream;
}

Bool wxPostScriptDC::PrinterDialog(Bool interactive, wxWindow *parent, Bool usePaperBBox)
{
  wxPrintSetupData *wxThePrintSetupData;
  char *s;

  if (interactive) {
    ok = XPrinterDialog(parent);
    if (!ok)
      return FALSE;
  } else
    ok = TRUE;
  
  wxThePrintSetupData = wxGetThePrintSetupData();

  mode = wxThePrintSetupData->GetPrinterMode();
  s = wxThePrintSetupData->GetPrintPreviewCommand();
  preview_cmd = copystring(s);
  s = wxThePrintSetupData->GetPrinterCommand();
  print_cmd = copystring(s);
  s = wxThePrintSetupData->GetPrinterOptions();
  print_opts = copystring(s);

  use_paper_bbox = usePaperBBox;

  if ((mode == PS_PREVIEW) || (mode == PS_PRINTER)) {
    // For PS_PRINTER action this depends on a Unix-style print spooler
    // since the wx_printer_file can be destroyed during a session
    char userId[256];
    char tmp[256];
    wxGetUserId (userId, sizeof (userId) / sizeof (char));
    strcpy(tmp, "/tmp/preview_");
    strcat(tmp, userId);
    strcat(tmp, ".ps");
    filename = copystring(tmp);
  } else if (mode == PS_FILE) {
    char *file;
    file = interactive ? (char *)NULL : wxThePrintSetupData->GetPrinterFile();
    if (!file) {
      char *dir = NULL;
      file = wxThePrintSetupData->GetPrinterFile();
      if (file) {
	dir = wxPathOnly(file);
	file = wxFileNameFromPath(file);
      }
      file = wxFileSelector("Save PostScript As", dir, file, "ps", NULL, wxSAVE, parent, -1, -1);
    }
    if (!file) {
      ok = FALSE;
      return FALSE;
    }
    filename = copystring(file);
    ok = TRUE;
  }

  return ok;
}

void wxPostScriptDC::SetClippingRect(double cx, double cy, double cw, double ch)
{
  wxRegion *r;

  if (!pstream)
    return;

  r = new WXGC_PTRS wxRegion(this);
  r->SetRectangle(cx, cy, cw, ch);

  SetClippingRegion(r);
}

wxRegion *wxPostScriptDC::GetClippingRegion()
{
  return clipping;
}

void wxPostScriptDC::SetClippingRegion(wxRegion *r)
{
  if (!pstream)
    return;
  if (r && (r->GetDC() != this))
    return;

  if (r) {
    double x, y, w, h;
    r->BoundingBox(&x, &y, &w, &h);
    clipx = XSCALEBND(x);
    clipy = YSCALEBND(y);
    clipw = XSCALEREL(w);
    cliph = YSCALEREL(h);
  } else {
    clipx = -100000.0;
    clipy = -100000.0;
    clipw = 200000.0;
    cliph = 200000.0;
  }

  if (clipping) {
    --clipping->locked;
    clipping = NULL;
    pstream->Out("initclip\n");
  }

  if (r) {
    r->InstallPS(this, pstream);

    clipping = r;
    clipping->locked++;
  }
}

void wxPostScriptDC::CalcBoundingBoxClip(double x, double y)
{
  if (x < clipx)
    x = clipx;
  else if (x >= (clipx + clipw))
    x = clipx + clipw;
  
  if (y < clipy)
    y = clipy;
  else if (y >= (clipy + cliph))
    y = clipy + cliph;

  if (x < min_x) min_x = x;
  if (y < min_y) min_y = y;
  if (x > max_x) max_x = x;
  if (y > max_y) max_y = y;
}

void wxPostScriptDC::SetAntiAlias(int mode)
{
  /* Don't change */
}

void wxPostScriptDC::Clear(void)
{
  unsigned char red, blue, green;

  if (!pstream)
    return;

  red = current_background_color->Red();
  blue = current_background_color->Blue();
  green = current_background_color->Green();

  {
    double redPS = (double) (((int) red) / 255.0);
    double bluePS = (double) (((int) blue) / 255.0);
    double greenPS = (double) (((int) green) / 255.0);
    
    /* Fill with current background */
    pstream->Out("gsave newpath\n");
    pstream->Out(redPS); pstream->Out(" "); pstream->Out(greenPS); pstream->Out(" "); pstream->Out(bluePS); pstream->Out(" setrgbcolor\n");
    pstream->Out(0); pstream->Out(" "); pstream->Out(0); pstream->Out(" moveto\n");
    pstream->Out(0); pstream->Out(" "); pstream->Out(paper_h); pstream->Out(" lineto\n");
    pstream->Out(paper_w); pstream->Out(" "); pstream->Out(paper_h); pstream->Out(" lineto\n");
    pstream->Out(paper_w); pstream->Out(" "); pstream->Out(0); pstream->Out(" lineto\n");
    pstream->Out("closepath\n");
    pstream->Out("fill grestore\n");
  }
}

Bool wxPostScriptDC::GetPixel(double WXUNUSED(x), double WXUNUSED(y), wxColour * WXUNUSED(col))
{
  return FALSE;
}

void wxPostScriptDC::DrawLine (double x1, double y1, double x2, double y2)
{
  if (!pstream)
    return;
  if (current_pen)
    SetPen (current_pen);
  pstream->Out("newpath\n");
  pstream->Out(XSCALE(x1)); pstream->Out(" "); pstream->Out(YSCALE (y1)); pstream->Out(" moveto\n");
  pstream->Out(XSCALE(x2)); pstream->Out(" "); pstream->Out(YSCALE (y2)); pstream->Out(" lineto\n");
  pstream->Out("stroke\n");
  
  {
    /* Need to make bounding box wide enough to show the pen.
       (Part of the reason for this is to avoid zero-sized boounding boxes.) */
    double width;

    if (current_pen) {
      width = current_pen->GetWidthF();
      width /= 2;
    } else
      width = 0;

    if (!width) width = 0.01;

    if (x1 == x2) {
      CalcBoundingBoxClip(XSCALEBND(x1 - width), YSCALEBND(y1));
      CalcBoundingBoxClip(XSCALEBND(x2 + width), YSCALEBND(y2));
    } else if (y1 == y2) {
      CalcBoundingBoxClip(XSCALEBND(x1), YSCALEBND(y1 - width));
      CalcBoundingBoxClip(XSCALEBND(x2), YSCALEBND(y2 + width));
    } else {
      CalcBoundingBoxClip(XSCALEBND(x1 - width), YSCALEBND(y1 - width));
      CalcBoundingBoxClip(XSCALEBND(x2 + width), YSCALEBND(y2 + width));
    }
  }
}

void wxPostScriptDC::DrawArc (double x, double y, double w, double h, double start, double end)
{
  if (!pstream)
    return;

  if (start != end) {
    double a1, a2, radius, xscale;

    /* Before we scale: */
    CalcBoundingBoxClip(XSCALEBND(x), YSCALEBND(y));
    CalcBoundingBoxClip(XSCALEBND(x + w), YSCALEBND(y + h));

    x = XSCALE(x);
    y = YSCALE(y);
    w = XSCALEREL(w);
    h = YSCALEREL(h);

    radius = (h / 2);
    xscale = (w / h);

    a1 = start * (180 / pie);
    a2 = end * (180 / pie);

    pstream->Out("gsave\n");
    pstream->Out((x + w/2)); pstream->Out(" "); 
    pstream->Out((y - h/2)); pstream->Out(" translate\n");
    pstream->Out(xscale); pstream->Out(" "); pstream->Out(1); pstream->Out(" scale\n");

    if (current_brush && current_brush->GetStyle () != wxTRANSPARENT) {
      SetBrush(current_brush);
      
      pstream->Out("newpath\n");
      pstream->Out(0); pstream->Out(" "); 
      pstream->Out(0); pstream->Out(" moveto\n");
      pstream->Out("0 0 "); pstream->Out(radius); pstream->Out(" "); pstream->Out(a1); 
      pstream->Out(" "); pstream->Out(a2); pstream->Out(" arc\n");

      pstream->Out("closepath\n");

      pstream->Out("fill\n");
    }
    if (current_pen && current_pen->GetStyle () != wxTRANSPARENT) {
      SetPen(current_pen);

      pstream->Out("newpath\n");
      pstream->Out("0 0 "); pstream->Out(radius); pstream->Out(" ");
      pstream->Out(a1); pstream->Out(" "); pstream->Out(a2); pstream->Out(" arc\n");
      pstream->Out("stroke\n");
    }

    pstream->Out("grestore\n");
    resetFont |= RESET_COLOR;
  }
}

void wxPostScriptDC::DrawPoint (double x, double y)
{
  if (!pstream)
    return;
  if (current_pen)
    SetPen (current_pen);
  pstream->Out("newpath\n");
  pstream->Out(XSCALE(x)); pstream->Out(" "); pstream->Out(YSCALE (y)); pstream->Out(" moveto\n");
  pstream->Out(XSCALE(x+1)); pstream->Out(" "); pstream->Out(YSCALE (y)); pstream->Out(" lineto\n");
  pstream->Out("stroke\n");
  CalcBoundingBoxClip(XSCALEBND(x), YSCALEBND(y));
}

void wxPostScriptDC::DrawSpline(double x1, double y1, double x2, double y2, double x3, double y3)
{
  double x21, y21, x22, y22;
  double xm1, ym1, xm2, ym2;

  if (!pstream)
    return;

  if (current_pen)
    SetPen (current_pen);

  pstream->Out("newpath\n");

  pstream->Out(XSCALE(x1)); pstream->Out(" "); pstream->Out(YSCALE(y1)); pstream->Out(" moveto ");

  x21 = (x1 + x2) / 2;
  y21 = (y1 + y2) / 2;

  pstream->Out(XSCALE(x21)); pstream->Out(" "); pstream->Out(YSCALE(y21)); pstream->Out(" lineto\n");

  x22 = (x2 + x3) / 2;
  y22 = (y2 + y3) / 2;
  
  xm1 = (x21 + x2) / 2;
  ym1 = (y21 + y2) / 2;

  xm2 = (x2 + x22) / 2;
  ym2 = (y2 + y22) / 2;

  pstream->Out(XSCALE(xm1)); pstream->Out(" "); pstream->Out(YSCALE(ym1)); pstream->Out(" "); 

  pstream->Out(XSCALE(xm2)); pstream->Out(" "); pstream->Out(YSCALE(ym2)); pstream->Out(" "); 

  pstream->Out(XSCALE(x22)); pstream->Out(" "); pstream->Out(YSCALE(y22)); pstream->Out(" curveto\n");

  pstream->Out(XSCALE(x3)); pstream->Out(" "); pstream->Out(YSCALE(y3)); pstream->Out(" lineto\n");

  pstream->Out("stroke\n");

  CalcBoundingBoxClip(XSCALEBND(x1), YSCALEBND(y1));
  CalcBoundingBoxClip(XSCALEBND(x2), YSCALEBND(y2));
  CalcBoundingBoxClip(XSCALEBND(x3), YSCALEBND(y3));
}

void wxPostScriptDC::DrawPolygon (int n, wxPoint points[], double xoffset, double yoffset, int fillStyle)
{
  if (!pstream)
    return;
  if (n > 0)
    {
      if (current_brush && current_brush->GetStyle () != wxTRANSPARENT)
	{
	  int i;
	  double xx, yy;

	  SetBrush (current_brush);
	  pstream->Out("newpath\n");

	  xx = points[0].x + xoffset;
	  yy = (points[0].y + yoffset);
	  pstream->Out(XSCALE(xx)); pstream->Out(" "); pstream->Out(YSCALE(yy)); pstream->Out(" moveto\n");
	  CalcBoundingBoxClip(XSCALEBND(xx), YSCALEBND(yy));

	  for (i = 1; i < n; i++)
	    {
	      xx = points[i].x + xoffset;
	      yy = (points[i].y + yoffset);
	      pstream->Out(XSCALE(xx)); pstream->Out(" "); pstream->Out(YSCALE(yy)); pstream->Out(" lineto\n");
	      CalcBoundingBoxClip(XSCALEBND(xx), YSCALEBND(yy));
	    }
	  pstream->Out(((fillStyle == wxODDEVEN_RULE) ? "eofill\n" : "fill\n"));
	}

      if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
	{
	  int i;
	  double xx, yy;

	  SetPen (current_pen);
	  pstream->Out("newpath\n");

	  xx = points[0].x + xoffset;
	  yy = (points[0].y + yoffset);
	  pstream->Out(XSCALE(xx)); pstream->Out(" "); pstream->Out(YSCALE(yy)); pstream->Out(" moveto\n");
	  CalcBoundingBoxClip(XSCALEBND(xx), YSCALEBND(yy));

	  for (i = 1; i < n; i++)
	    {
	      xx = points[i].x + xoffset;
	      yy = (points[i].y + yoffset);
	      pstream->Out(XSCALE(xx)); pstream->Out(" "); pstream->Out(YSCALE(yy)); pstream->Out(" lineto\n");
	      CalcBoundingBoxClip(XSCALEBND(xx), YSCALEBND(yy));
	    }

	  // Close the polygon
	  pstream->Out(" closepath\n");

	  // Output the line
	  pstream->Out("stroke\n");
	}
    }
}

void wxPostScriptDC::DrawPath(wxPath *p, double xoff, double yoff, int fillStyle)
{
  int did = 0;

  if (!pstream)
    return;
  
  if (current_brush && current_brush->GetStyle () != wxTRANSPARENT) {
    SetBrush (current_brush);
    pstream->Out("newpath\n");
    p->InstallPS(this, pstream, xoff, yoff);
    pstream->Out(((fillStyle == wxODDEVEN_RULE) ? "eofill\n" : "fill\n"));
    did = 1;
  }

  if (current_pen && current_pen->GetStyle () != wxTRANSPARENT) {
    SetPen (current_pen);
    pstream->Out("newpath\n");
    p->InstallPS(this, pstream, xoff, yoff);
    pstream->Out("stroke\n");
    did = 1;
  }

  if (did) {
    double x1, x2, y1, y2;
    p->BoundingBox(&x1, &y1, &x2, &y2);
    x1 += xoff; 
    x2 += xoff;
    y1 += yoff; 
    y2 += yoff;
    CalcBoundingBoxClip(XSCALEBND(x1), YSCALEBND(y1));
    CalcBoundingBoxClip(XSCALEBND(x2), YSCALEBND(y2));
  }
}


void wxPostScriptDC::DrawLines (int n, wxPoint points[], double xoffset, double yoffset)
{
  if (!pstream)
    return;

  if (n > 0 && current_pen && (current_pen->GetStyle () != wxTRANSPARENT)) {
    int i;
    double xx, yy;

    SetPen(current_pen);
    
    pstream->Out("newpath\n");
    
    xx = points[0].x + xoffset;
    yy = (points[0].y + yoffset);
    pstream->Out(XSCALE(xx)); pstream->Out(" "); pstream->Out(YSCALE(yy)); pstream->Out(" moveto\n");
    CalcBoundingBoxClip(XSCALEBND(xx), YSCALEBND(yy));
    
    for (i = 1; i < n; i++)
      {
	xx = points[i].x + xoffset;
	yy = (points[i].y + yoffset);
	pstream->Out(XSCALE(xx)); pstream->Out(" "); pstream->Out(YSCALE(yy)); pstream->Out(" lineto\n");
	CalcBoundingBoxClip(XSCALEBND(xx), YSCALEBND(yy));
      }
    pstream->Out("stroke\n");
  }
}

void wxPostScriptDC::DrawRectangle (double x, double y, double width, double height)
{
  if (!pstream)
    return;
  if (current_brush && current_brush->GetStyle () != wxTRANSPARENT)
    {
      SetBrush (current_brush);

      pstream->Out("newpath\n");
      pstream->Out(XSCALE(x)); pstream->Out(" "); pstream->Out(YSCALE (y)); pstream->Out(" moveto\n");
      pstream->Out(XSCALE(x + width)); pstream->Out(" "); pstream->Out(YSCALE (y)); pstream->Out(" lineto\n");
      pstream->Out(XSCALE(x + width)); pstream->Out(" "); pstream->Out(YSCALE (y + height)); pstream->Out(" lineto\n");
      pstream->Out(XSCALE(x)); pstream->Out(" "); pstream->Out(YSCALE (y + height)); pstream->Out(" lineto\n");
      pstream->Out("closepath\n");
      pstream->Out("fill\n");

      CalcBoundingBoxClip(XSCALEBND(x), YSCALEBND(y));
      CalcBoundingBoxClip(XSCALEBND(x + width), YSCALEBND(y + height));
    }
  if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
    {
      double pw;

      SetPen (current_pen);

      pstream->Out("newpath\n");
      pstream->Out(XSCALE(x)); pstream->Out(" "); pstream->Out(YSCALE (y)); pstream->Out(" moveto\n");
      pstream->Out(XSCALE(x + width)); pstream->Out(" "); pstream->Out(YSCALE (y)); pstream->Out(" lineto\n");
      pstream->Out(XSCALE(x + width)); pstream->Out(" "); pstream->Out(YSCALE (y + height)); pstream->Out(" lineto\n");
      pstream->Out(XSCALE(x)); pstream->Out(" "); pstream->Out(YSCALE (y + height)); pstream->Out(" lineto\n");
      pstream->Out("closepath\n");
      pstream->Out("stroke\n");

      if (current_pen) {
        pw = current_pen->GetWidthF();
        pw /= 2;
      } else
        pw = 0;

      CalcBoundingBoxClip(XSCALEBND(x - pw), YSCALEBND(y - pw));
      CalcBoundingBoxClip(XSCALEBND(x + width + pw),  YSCALEBND(y + height + pw));
    }
}

void wxPostScriptDC::DrawRoundedRectangle (double x, double y, double width, double height, double radius)
{
  double ascale;

  if (!pstream)
    return;

  if (radius < 0.0)
    {
      // Now, a negative radius is interpreted to mean
      // 'the proportion of the smallest X or Y dimension'
      double smallest = 0.0;
      if (width < height)
	smallest = width;
      else
	smallest = height;
      radius = (double) (-radius * smallest);
    }

  ascale = (user_scale_x < user_scale_y) ? user_scale_x : user_scale_y;

  if (current_brush && current_brush->GetStyle () != wxTRANSPARENT)
    {
      SetBrush (current_brush);
      // Draw rectangle anticlockwise
      pstream->Out("newpath\n");

      pstream->Out(XSCALE(x) + ASCALEREL(radius)); pstream->Out(" "); 
      pstream->Out(YSCALE(y)); pstream->Out(" moveto\n");

      pstream->Out(XSCALE(x) + ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(YSCALE(y) - ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(ASCALEREL(radius)); pstream->Out(" 90 180 arc\n");

      pstream->Out(XSCALE(x) + ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(YSCALE(y + height) + ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(ASCALEREL(radius)); pstream->Out(" 180 270 arc\n");

      pstream->Out(XSCALE(x + width) - ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(YSCALE(y + height) + ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(ASCALEREL(radius)); pstream->Out(" 270 0 arc\n");

      pstream->Out(XSCALE(x + width) - ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(YSCALE(y) - ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(ASCALEREL(radius)); pstream->Out(" 0 90 arc\n");

      pstream->Out("closepath\n");

      pstream->Out("fill\n");

      CalcBoundingBoxClip(XSCALEBND(x), YSCALEBND(y));
      CalcBoundingBoxClip(XSCALEBND(x + width), YSCALEBND(y + height));
    }
  if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
    {
      SetPen (current_pen);
      // Draw rectangle anticlockwise
      pstream->Out("newpath\n");

      pstream->Out(XSCALE(x) + ASCALEREL(radius)); pstream->Out(" "); 
      pstream->Out(YSCALE(y)); pstream->Out(" moveto\n");

      pstream->Out(XSCALE(x) + ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(YSCALE(y) - ASCALEREL(radius)); pstream->Out(" "); 
      pstream->Out(ASCALEREL(radius)); pstream->Out(" 90 180 arc\n");

      pstream->Out(XSCALE(x) + ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(YSCALE(y + height) + ASCALEREL(radius)); pstream->Out(" "); 
      pstream->Out(ASCALEREL(radius)); pstream->Out(" 180 270 arc\n");

      pstream->Out(XSCALE(x + width) - ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(YSCALE(y + height) + ASCALEREL(radius)); pstream->Out(" "); 
      pstream->Out(ASCALEREL(radius)); pstream->Out(" 270 0 arc\n");

      pstream->Out(XSCALE(x + width) - ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(YSCALE(y) - ASCALEREL(radius)); pstream->Out(" "); 
      pstream->Out(ASCALEREL(radius)); pstream->Out(" 0 90 arc\n");

      pstream->Out("closepath\n");

      pstream->Out("stroke\n");

      {
        double pw;

        if (current_pen) {
          pw = current_pen->GetWidthF();
          pw /= 2;
        } else
          pw = 0;
        
        CalcBoundingBoxClip(XSCALEBND(x - pw), YSCALEBND(y - pw));
        CalcBoundingBoxClip(XSCALEBND(x + width + pw),  YSCALEBND(y + height + pw));
      }
    }
}

void wxPostScriptDC::DrawEllipse (double x, double y, double width, double height)
{
  if (!pstream)
    return;
  if (current_brush && current_brush->GetStyle () != wxTRANSPARENT)
    {
      SetBrush (current_brush);

      pstream->Out("newpath\n");
      pstream->Out(XSCALE(x + width / 2)); pstream->Out(" "); pstream->Out(YSCALE(y + height / 2)); pstream->Out(" ");
      pstream->Out(XSCALEREL(width / 2)); pstream->Out(" "); pstream->Out(YSCALEREL(height / 2)); pstream->Out(" 0 360 ellipse\n");
      pstream->Out("fill\n");

      CalcBoundingBoxClip(XSCALEBND(x), YSCALEBND(y));
      CalcBoundingBoxClip(XSCALEBND(x + width), YSCALEBND(y + height));
    }
  if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
    {
      SetPen (current_pen);

      pstream->Out("newpath\n");
      pstream->Out(XSCALE(x + width / 2)); pstream->Out(" "); pstream->Out(YSCALE(y + height / 2)); pstream->Out(" ");
      pstream->Out(XSCALEREL(width / 2)); pstream->Out(" "); pstream->Out(YSCALEREL(height / 2)); pstream->Out(" 0 360 ellipse\n");
      pstream->Out("stroke\n");
      
      {
        double pw;

        if (current_pen) {
          pw = current_pen->GetWidthF();
          pw /= 2;
        } else
          pw = 0;
        
        CalcBoundingBoxClip(XSCALEBND(x - pw), YSCALEBND(y - pw));
        CalcBoundingBoxClip(XSCALEBND(x + width + pw),  YSCALEBND(y + height + pw));
      }
    }
}

void wxPostScriptDC::SetFont (wxFont * the_font)
{
  char *name;
  int family, style, weight, size;

  if (!pstream)
    return;
  if ((current_font == the_font) && !(resetFont & RESET_FONT))
    return;

  resetFont -= (resetFont & RESET_FONT);

  current_font = the_font;
  family = current_font->GetFontId();
  style = current_font->GetStyle();
  weight = current_font->GetWeight();

  name = wxTheFontNameDirectory->GetPostScriptName(family, weight, style);
  if (!name)
    name = "Times-Roman";

  size = current_font->GetPointSize();

  next_font_name = name;
  next_font_size = size;
}

static void set_pattern(wxPostScriptDC *dc, wxPSStream *pstream, wxBitmap *bm, int rop, wxColour *col)
{
  int width, height;

  width = bm->GetWidth();
  height = bm->GetHeight();

  pstream->Out("8 dict\n");
  pstream->Out("dup\n");
  pstream->Out("begin\n");
  pstream->Out(" /PatternType 1 def\n");
  pstream->Out(" /PaintType 1 def\n");
  pstream->Out(" /TilingType 1 def\n");
  pstream->Out(" /BBox [ 0 0 "); pstream->Out(width); pstream->Out(" "); pstream->Out(height); pstream->Out(" ] def\n");
  pstream->Out(" /XStep "); pstream->Out(width); pstream->Out(" def\n");
  pstream->Out(" /YStep "); pstream->Out(height); pstream->Out(" def\n");

  dc->Blit(0, 0, width, height, bm, 0, 0, -rop - 1, col);

  pstream->Out("end\n");
  pstream->Out(" matrix makepattern setpattern\n");
}

static char *dotted = "[2 5] 2";
static char *short_dashed = "[4 4] 2";
static char *long_dashed = "[4 8] 2";
static char *dotted_dashed = "[6 6 2 6] 4";

void wxPostScriptDC::SetPen (wxPen * pen)
{
  wxPen *oldPen = current_pen;
  char *psdash = NULL;
  unsigned char red, blue, green;
  int val;
  double width;

  if (!pstream)
    return;

  if (current_pen) current_pen->Lock(-1);
  if (pen) pen->Lock(1);

  if ((current_pen = pen) == NULL)
    return;			/* NIL */

  // Line width
  width = pen->GetWidthF();
  pstream->Out(XSCALEREL(width));
  pstream->Out(" setlinewidth\n");

  if (level2ok) {
    wxBitmap *stipple;
    stipple = pen->GetStipple();
    if (stipple && stipple->Ok()) {
      int ps;
      wxColour *pc;
      ps = pen->GetStyle();
      pc = pen->GetColour();
      set_pattern(this, pstream, stipple, ps, pc);
      resetFont |= RESET_COLOR;
      return;
    }
  }

  switch (pen->GetStyle ())
    {
    case wxDOT:
      psdash = dotted;
      break;
    case wxSHORT_DASH:
      psdash = short_dashed;
      break;
    case wxLONG_DASH:
      psdash = long_dashed;
      break;
    case wxDOT_DASH:
      psdash = dotted_dashed;
      break;
    case wxSOLID:
    case wxTRANSPARENT:
    default:
      psdash = "[] 0";
      break;
    }
  if (oldPen != pen) {
    pstream->Out(psdash); pstream->Out(" setdash\n");
  }

  switch (pen->GetCap()) {
  case wxCAP_ROUND:
    val = 1;
    break;
  case wxCAP_PROJECTING:
    val = 2;
    break;
  default:
  case wxCAP_BUTT:
    val = 0;
    break;
  }
  pstream->Out(val);
  pstream->Out(" setlinecap\n");

  switch (pen->GetJoin()) {
  case wxJOIN_ROUND:
    val = 1;
    break;
  case wxJOIN_BEVEL:
    val = 2;
    break;
  default:
  case wxJOIN_MITER:
    val = 0;
    break;
  }
  pstream->Out(val);
  pstream->Out(" setlinejoin\n");

  // Line colour
  {
    wxColour *pc;
    pc = pen->GetColour();
    red = pc->Red();
    blue = pc->Blue();
    green = pc->Green();
  }

  if (!Colour)
    {
      // Anything not white is black
      if (!(red == (unsigned char) 255 && blue == (unsigned char) 255
	    && green == (unsigned char) 255))
	{
	  red = (unsigned char) 0;
	  green = (unsigned char) 0;
	  blue = (unsigned char) 0;
	}
    }

  if (!(red == currentRed && green == currentGreen && blue == currentBlue)
      || (resetFont & RESET_COLOR)) {
    double redPS = (double) (((int) red) / 255.0);
    double bluePS = (double) (((int) blue) / 255.0);
    double greenPS = (double) (((int) green) / 255.0);

    pstream->Out(redPS); pstream->Out(" "); pstream->Out(greenPS); 
    pstream->Out(" "); pstream->Out(bluePS); pstream->Out(" setrgbcolor\n");
    
    currentRed = red;
    currentBlue = blue;
    currentGreen = green;
    resetFont -= (resetFont & RESET_COLOR);
  }
}

static char *ps_brush_hatch[] = { " 0 0 moveto 8 8",
				  " 0 0 moveto 8 8 lineto closepath stroke 8 0 moveto 0 8",
				  " 8 0 moveto 0 8",
				  " 0 4 moveto 8 4 lineto closepath stroke 4 0 moveto 4 8",
				  " 0 4 moveto 8 4",
				  " 4 0 moveto 4 8",
				  " 0 0 moveto 0.1 0.1" };

void wxPostScriptDC::SetBrush(wxBrush * brush)
{
  unsigned char red, blue, green;
  int hatch_id;
  char *hatch_size;
  double redPS, bluePS, greenPS;

  if (!pstream)
    return;

  if (current_brush) current_brush->Lock(-1);
  if (brush) brush->Lock(1);

  if ((current_brush = brush) == NULL)
    return; 

  if (level2ok) {
    wxBitmap *stipple;
    stipple = brush->GetStipple();
    if (stipple && stipple->Ok()) {
      int bs;
      wxColour *bc;
      bs = brush->GetStyle();
      bc = brush->GetColour();
      set_pattern(this, pstream, stipple, bs, bc);
      resetFont |= RESET_COLOR;
      return;
    }
  }

  // Brush colour
  {
    wxColour *bc;
    bc = brush->GetColour(); 
    red = bc->Red();
    blue = bc->Blue();
    green = bc->Green();
  }

  if (!Colour) {
    // Anything not black is white
    if (!(red == (unsigned char) 0 && blue == (unsigned char) 0
	  && green == (unsigned char) 0)) {
      red = (unsigned char) 255;
      green = (unsigned char) 255;
      blue = (unsigned char) 255;
    }
  }

  hatch_id = -1;
  hatch_size = "8";
  switch (brush->GetStyle()) {
  case wxBDIAGONAL_HATCH:
    hatch_id = 0;
    break;
  case wxCROSSDIAG_HATCH:
    hatch_id = 1;
    break;
  case wxFDIAGONAL_HATCH:
    hatch_id = 2;
    break;
  case wxCROSS_HATCH:
    hatch_id = 3;
    break;
  case wxHORIZONTAL_HATCH:
    hatch_id = 4;
    break;
  case wxVERTICAL_HATCH:
    hatch_id = 5;
    break;
  case wxCOLOR:
    hatch_id = 6;
    hatch_size = "0.3";
    break;
  }

  redPS = (double) (((int) red) / 255.0);
  bluePS = (double) (((int) blue) / 255.0);
  greenPS = (double) (((int) green) / 255.0);

  if (hatch_id > -1) {
    pstream->Out("7 dict\n");
    pstream->Out("dup\n");
    pstream->Out("begin\n");
    pstream->Out(" /PatternType 1 def\n");
    pstream->Out(" /PaintType 1 def\n");
    pstream->Out(" /TilingType 1 def\n");
    pstream->Out(" /BBox [ 0 0 ");
    pstream->Out(hatch_size);
    pstream->Out(" ");
    pstream->Out(hatch_size);
    pstream->Out(" ] def\n");
    pstream->Out(" /XStep ");
    pstream->Out(hatch_size);
    pstream->Out(" def\n");
    pstream->Out(" /YStep ");
    pstream->Out(hatch_size);
    pstream->Out(" def\n");
    pstream->Out(" /PaintProc { begin gsave \n");

    pstream->Out(" 0.05 setlinewidth\n");
    pstream->Out(" [] 0 setdash\n");
    pstream->Out(" "); pstream->Out(redPS); pstream->Out(" "); pstream->Out(greenPS); 
    pstream->Out(" "); pstream->Out(bluePS); pstream->Out(" setrgbcolor\n");

    pstream->Out(" "); pstream->Out(ps_brush_hatch[hatch_id]); pstream->Out(" lineto closepath stroke \n");
    
    pstream->Out("grestore\n } def \n");
    
    pstream->Out("end\n"); pstream->Out(" matrix makepattern setpattern\n");

    resetFont |= RESET_COLOR;

    return;
  }

  if (!(red == currentRed && green == currentGreen && blue == currentBlue)
      || (resetFont & RESET_COLOR)) {
    pstream->Out(redPS); pstream->Out(" "); pstream->Out(greenPS); pstream->Out(" "); 
    pstream->Out(bluePS); pstream->Out(" setrgbcolor\n");
    currentRed = red;
    currentBlue = blue;
    currentGreen = green;
    resetFont -= (resetFont & RESET_COLOR);
  }
}

void wxPostScriptDC::DrawText(DRAW_TEXT_CONST char *text, double x, double y,
			      Bool combine, Bool use16, int dt, double angle)
{
  double tw, th;
  const char *name;
  int sym_map;

  if (!pstream)
    return;
  if (current_font)
    SetFont (current_font);

  GetTextExtent(text, &tw, &th, NULL, NULL, NULL, combine, use16, dt);

  if (current_bk_mode == wxSOLID) {
    unsigned char red, blue, green;
    
    red = current_text_background->Red();
    blue = current_text_background->Blue();
    green = current_text_background->Green();
    
    {
      double redPS = (double) (((int) red) / 255.0);
      double bluePS = (double) (((int) blue) / 255.0);
      double greenPS = (double) (((int) green) / 255.0);
      
      pstream->Out("gsave newpath\n");
      pstream->Out(redPS); pstream->Out(" "); pstream->Out(greenPS); pstream->Out(" "); 
      pstream->Out(bluePS); pstream->Out(" setrgbcolor\n");
      pstream->Out(XSCALE(x)); pstream->Out(" "); pstream->Out(YSCALE (y)); pstream->Out(" moveto\n");
      pstream->Out(XSCALE(x + tw)); pstream->Out(" "); pstream->Out(YSCALE (y)); pstream->Out(" lineto\n");
      pstream->Out(XSCALE(x + tw)); pstream->Out(" "); pstream->Out(YSCALE (y + th)); pstream->Out(" lineto\n");
      pstream->Out(XSCALE(x)); pstream->Out(" "); pstream->Out(YSCALE (y + th)); pstream->Out(" lineto\n");
      pstream->Out("closepath\n");
      pstream->Out("fill grestore\n");
    }
  }

  if (current_text_foreground->Ok()) {
    unsigned char red, blue, green;

    red = current_text_foreground->Red();
    blue = current_text_foreground->Blue();
    green = current_text_foreground->Green();
    
    if (!Colour) {
      // Anything not white is black
      if (!(red == (unsigned char) 255 && blue == (unsigned char) 255
	    && green == (unsigned char) 255))
	{
	  red = (unsigned char) 0;
	  green = (unsigned char) 0;
	  blue = (unsigned char) 0;
	}
    }
    if (!(red == currentRed && green == currentGreen && blue == currentBlue)
	|| (resetFont & RESET_COLOR)) {
      double redPS = (double) (((int) red) / 255.0);
      double bluePS = (double) (((int) blue) / 255.0);
      double greenPS = (double) (((int) green) / 255.0);
      pstream->Out(redPS); pstream->Out(" "); pstream->Out(greenPS); pstream->Out(" "); 
      pstream->Out(bluePS); pstream->Out(" setrgbcolor\n");
      
      currentRed = red;
      currentBlue = blue;
      currentGreen = green;
      resetFont -= (resetFont & RESET_COLOR);
    }
  }
  
  if (next_font_name) {
    if (!current_font_name
	|| (next_font_size != current_font_size)
	|| strcmp(next_font_name, current_font_name)) {
      char *fn;
      pstream->Out("/"); fn = wxPostScriptFixupFontName(next_font_name); pstream->Out(fn); pstream->Out(" findfont\n");
      pstream->Out(next_font_size); pstream->Out(" scalefont setfont\n");
      
      used_fonts = wxPostScriptRecordFont(fn, used_fonts);

      current_font_size = next_font_size;
      current_font_name = next_font_name;
    }
    name = next_font_name;
    next_font_name = NULL;
  } else {
    int family, style, weight;
    if (current_font) {
      family = current_font->GetFontId();
      style = current_font->GetStyle();
      weight = current_font->GetWeight();
    } else {
      family = wxDEFAULT;
      style = wxNORMAL;
      weight = wxNORMAL;
    }
    
    name = wxTheFontNameDirectory->GetPostScriptName(family, weight, style);
    if (!name)
      name = "Times-Roman";
  }

  if (angle != 0.0) {
    pstream->Out("gsave\n");
  }

  if (angle != 0.0) {
    pstream->Out(XSCALE(x)); pstream->Out(" "); pstream->Out(YSCALE(y)); 
    pstream->Out(" translate\n");
    if ((user_scale_x != 1) || (user_scale_y != 1)) {
      pstream->Out(user_scale_x); pstream->Out(" "); pstream->Out(user_scale_y); pstream->Out(" scale\n");
    }
    pstream->Out(angle * 180 / pie);
    pstream->Out(" rotate 0 0 moveto\n"); 
  } else {
    pstream->Out(XSCALE(x)); pstream->Out(" "); pstream->Out(YSCALE(y)); 
    pstream->Out(" moveto\n");
    if ((user_scale_x != 1) || (user_scale_y != 1)) {
      pstream->Out("gsave\n");
      pstream->Out(user_scale_x); pstream->Out(" "); pstream->Out(user_scale_y); pstream->Out(" scale\n");
    }
  }

  sym_map = current_font->GetFamily() == wxSYMBOL;
  used_fonts = wxPostScriptDrawText((Scheme_Object *)pstream->f, name, text, dt, combine, use16, current_font_size,
                                    sym_map, used_fonts);

  if ((angle != 0.0) || (user_scale_x != 1) || (user_scale_y != 1)) {
    pstream->Out("grestore\n"); 
  }

  CalcBoundingBoxClip(XSCALEBND(x), YSCALEBND(y));
  if (angle != 0.0) {
    double xe, ye;
    xe = x + (tw * cos(angle)) + (th * sin(angle));
    ye = y - (th * cos(angle)) - (tw * sin(angle));
    CalcBoundingBoxClip(XSCALEBND(xe), YSCALEBND(ye));
  } else {
    CalcBoundingBoxClip(XSCALEBND(x + tw), YSCALEBND(y + th));
  }
}

Bool wxPostScriptDC::GlyphAvailable(int c, wxFont *f)
{
  const char *name;
  int family, style, weight, sym_map;

  if (!f)
    f = current_font;

  family = f->GetFontId();
  style = f->GetStyle();
  weight = f->GetWeight();

  name = wxTheFontNameDirectory->GetPostScriptName(family, weight, style);
  if (!name)
    name = "Times-Roman";

  sym_map = current_font->GetFamily() == wxSYMBOL;

  return wxPostScriptGlyphExists(name, c, sym_map);
}

void wxPostScriptDC::SetBackground (wxColour * c)
{
  current_background_color->CopyFrom(c);
}

void wxPostScriptDC::SetBackgroundMode(int mode)
{
  current_bk_mode = mode;
}

void wxPostScriptDC::SetTextBackground(wxColour *col)
{
  current_text_background->CopyFrom(col);  
}

void wxPostScriptDC::SetTextForeground(wxColour *col)
{
  current_text_foreground->CopyFrom(col);
}

void wxPostScriptDC::TryColour(wxColour *src, wxColour *dest)
{
  if (!Colour) {
    if ((src->Red() == 255)
	&& (src->Green() == 255)
	&& (src->Blue() == 255))
      dest->Set(255, 255, 255);
    else
      dest->Set(0, 0, 0);
  } else
    dest->CopyFrom(src);
}

static const char *wxPostScriptHeaderEllipse = "\
/ellipsedict 8 dict def\n\
ellipsedict /mtrx matrix put\n\
/ellipse\n\
{ ellipsedict begin\n\
  /endangle exch def\n\
  /startangle exch def\n\
  /yrad exch def\n\
  /xrad exch def\n\
  /y exch def\n\
  /x exch def\n\
  /savematrix mtrx currentmatrix def\n\
  x y translate\n\
  xrad yrad scale\n\
  0 0 1 endangle startangle arcn\n\
  savematrix setmatrix\n\
  end\n\
  } def\n\
";

Bool wxPostScriptDC::StartDoc (char *message)
{
  char userID[256];

  if (device == wxDEVICE_EPS) {
    wxPSStream *pss;
    pss = new WXGC_PTRS wxPSStream(filename);
    pstream = pss;

    if (!pstream || !pstream->good()) {
      ok = FALSE;
      pstream = NULL;
      return FALSE;
    }
    ok = TRUE;
  }

  pstream->Out("%!PS-Adobe-2.0"); /* PostScript magic strings */
  if (as_eps) {
    pstream->Out(" EPSF-2.0"); /* EPS magic strings */
  }
  pstream->Out("\n");
  if (title) {
    pstream->Out("%%Title: "); pstream->Out(title); pstream->Out("\n");
  }
  pstream->Out("%%Creator: "); pstream->Out("PLT Scheme"); pstream->Out("\n");
  pstream->Out("%%CreationDate: "); pstream->Out(wxNow()); pstream->Out("\n");

  // User Id information
  if (wxGetEmailAddress(userID, sizeof(userID))) {
    char userName[245];
    pstream->Out("%%For: "); pstream->Out((char *)userID);
    if (wxGetUserName(userName, sizeof(userName))) {
      pstream->Out(" ("); pstream->Out((char *)userName); pstream->Out(")");
    }
    pstream->Out("\n");
  } else if ( wxGetUserName(userID, sizeof(userID))) {
    pstream->Out("%%For: "); pstream->Out((char *)userID); pstream->Out("\n");
  }

  boundingboxpos = pstream->tellp();
  pstream->Out("%%BoundingBox: -00000 -00000 -00000 -00000\n");
  pstream->Out("%%Pages: -00000\n");

  if (landscape)
    pstream->Out("%%Orientation: Landscape\n");

  {
    int i;
    pstream->Out("%%DocumentFonts: ");  
    fontlistpos = pstream->tellp();
    for (i = 0; i < 5; i++) {
      pstream->Out("          ");  
    }
    pstream->Out("\n");  
  }

  pstream->Out("%%EndComments\n\n");

  pstream->Out(wxPostScriptHeaderEllipse);

  SetBrush(wxWHITE_BRUSH);
  SetPen(wxBLACK_PEN);

  page_number = 1;
  if (message) {
    title = copystring (message);
  }

  return TRUE;
}

#ifdef wx_x
extern void wxsExecute(char **);
#endif

void wxPostScriptDC::EndDoc (void)
{
  double llx, lly, urx, ury;
  double minx, miny, maxx, maxy;
  long last_pos;

  if (!pstream)
    return;
  if (clipping) {
    clipping = FALSE;
    pstream->Out("grestore\n");
  }

  // Compute the bounding box.  Note that it is in the default user
  // coordinate system, thus we have to convert the values.
  // If we're landscape, our sense of "x" and "y" is reversed.
  if (use_paper_bbox) {
    minx = 0;
    miny = 0;
    maxx = paper_w;
    maxy = paper_h;
  } else {
    minx = min_x;
    miny = min_y;
    maxx = max_x;
    maxy = max_y;
  }
   
  if (landscape) {
    llx = miny * paper_y_scale + paper_y + paper_margin_y;
    lly = minx * paper_x_scale + paper_x + paper_margin_x;
    urx = maxy * paper_y_scale + paper_y + paper_margin_y;
    ury = maxx * paper_x_scale + paper_x + paper_margin_x;
  } else {
    llx = minx * paper_x_scale + paper_x + paper_margin_x;
    lly = paper_h * paper_y_scale - (maxy * paper_y_scale) + paper_y + paper_margin_y;
    urx = maxx * paper_x_scale + paper_x + paper_margin_x;
    ury = paper_h * paper_y_scale - (miny * paper_y_scale) + paper_y + paper_margin_y;
  }

  /* Don't allow a negative-sized bounding box! */
  if (urx <= llx)
    urx = llx + 1;
  if (ury <= lly)
    ury = lly + 1;

  last_pos = pstream->tellp();

  // The Adobe specifications call for integers; we round as to make
  // the bounding larger.
  pstream->seekp(boundingboxpos);
  pstream->Out("%%BoundingBox: ");
  pstream->width(5);
  pstream->Out(floor(llx)); pstream->Out(" ");
  pstream->width(5);
  pstream->Out(floor(lly)); pstream->Out(" ");
  pstream->width(5);
  pstream->Out(ceil(urx) ); pstream->Out(" ");
  pstream->width(5);
  pstream->Out(ceil(ury)); pstream->Out("\n");
  pstream->Out("%%Pages: ");
  pstream->width(5);
  pstream->Out((page_number - 1)); pstream->Out("\n");

  {
    char *fnts;
    long len;
    fnts = wxPostScriptFontsToString(used_fonts);
    pstream->seekp(fontlistpos);
    len = strlen(fnts);
    if (len <= 50) {
      pstream->Out(fnts);
    } else {
      long a, bot, delta = len - 50;
      char *buf;
      buf = new WXGC_ATOMIC char[4096];
      for (a = last_pos; a > fontlistpos; ) {
        bot = a - 4095;
        if (bot < fontlistpos)
          bot = fontlistpos;
        pstream->read_at(bot, buf, a - bot);
        buf[a - bot] = 0;
        pstream->seekp(bot + delta);
        pstream->Out(buf);
        pstream->flush();
        a = bot;
      }
      pstream->seekp(fontlistpos);
      pstream->Out(fnts);
    }
  }

  DELETE_OBJ pstream;
  pstream = NULL;

#ifdef wx_x
  if (ok /* && wx_interactive */)
    {
      switch (mode) {
	case PS_PREVIEW:
	{
          char *argv[3];
	  argv[0] = preview_cmd;
          argv[1] = filename;
          argv[2] = NULL;
	  wxsExecute (argv);
	}
	break;

	case PS_PRINTER:
	{
          char *argv[4];
	  char *opts;
	  int i;
          argv[0] = print_cmd;
	  i = 1;
	  opts = print_opts;
	  if (opts && *opts)
	    argv[i++] = opts;
	  argv[i++] = filename;
	  argv[i] = NULL;
	  wxsExecute(argv);
	}
	break;

	case PS_FILE:
	  break;
	}
    }
#endif
}

void wxPostScriptDC::StartPage (void)
{
  if (!pstream)
    return;
  pstream->Out("%%Page: "); pstream->Out(page_number++); pstream->Out("\n");
  pstream->Out("%%BeginPageSetup\n");
  /* pstream->Out("userdict /pgsave save put\n"); */

  pstream->Out((paper_x + paper_margin_x + (landscape ? (paper_h * paper_y_scale) : 0)));
  pstream->Out(" "); pstream->Out(paper_y + paper_margin_y); pstream->Out(" translate\n");
  if (landscape) {
    pstream->Out(paper_y_scale); pstream->Out(" "); pstream->Out(paper_x_scale); pstream->Out(" scale\n");
    pstream->Out("90 rotate\n");
  } else {
    pstream->Out(paper_x_scale); pstream->Out(" "); pstream->Out(paper_y_scale); pstream->Out(" scale\n");
  }
  pstream->Out("2 setlinecap\n");
  pstream->Out("%%EndPageSetup\n");

  resetFont = RESET_FONT | RESET_COLOR;
  current_font_name = NULL;

  if (clipping)
    SetClippingRegion(clipping);
}

void wxPostScriptDC::EndPage (void)
{
  if (!pstream)
    return;
  /* pstream->Out("userdict /pgsave get restore\n"); */
  pstream->Out("showpage\n");
}


static void printhex(wxPSStream *pstream, int v)
{
  int h, l;
  char s[3];

  s[2] = 0;
  
  h = (v >> 4) & 0xF;
  l = v & 0xF;
  
  if (h <= 9)
    s[0] = '0' + h;
  else
    s[0] = 'a' + (h - 10);
  if (l <= 9)
    s[1] = '0' + l;
  else
    s[1] = 'a' + (l - 10);

  pstream->Out(s);
}


Bool wxPostScriptDC::
Blit (double xdest, double ydest, double fwidth, double fheight,
      wxMemoryDC *src, double xsrc, double ysrc, int rop, wxColour *dcolor,
      wxMemoryDC *mask)
{
  int mono;
  long j, i;
  wxColour *c;
  int pixel;
  int pr, pg, pb;

  wxCanvasDC *source = (wxCanvasDC *)src;
  long width, height, x, y;
  Bool asColour = level2ok;

  if (!pstream)
    return FALSE;

  width = (long)floor(fwidth);
  height = (long)floor(fheight);

  if (rop >= 0) {
    fwidth = XSCALEREL(fwidth);
    fheight = YSCALEREL(fheight);
  }

  x = (long)floor(xsrc);
  y = (long)floor(ysrc);

  c = new WXGC_PTRS wxColour;

  /* Since we want a definition, may need to start a dictionary: */
  if (rop >= 0) {
    pstream->Out("1 dict begin\n");
  }

  /* Allocate space. */
  pstream->Out("/DataString ");
  pstream->Out((width * (asColour ? 3 : 1) * ((rop < 0) ? height : 1)));
  pstream->Out(" string def\n");

  if (rop < 0) {
    pstream->Out(" /PaintProc { begin \n");
  }

  /* PostScript setup: */
  pstream->Out("gsave\n");
  if (rop >= 0) {
    pstream->Out(XSCALE(xdest)); pstream->Out(" "); pstream->Out(YSCALE(ydest) - fheight); pstream->Out(" translate\n");
  }

  /* Mask => clip */
  if (mask) {
    int red, green, blue;
    int skip_start, skip;
    
    pstream->Out("newpath\n");
    for (i = 0; i < width; i++) {
      skip = 0;
      skip_start = 0;
      for (j = 0; j < height + 1; j++) {
	mask->GetPixel(i, j, c);
	
	if (j == height) {
	  red = green = blue = 255;
	} else {
	  red = c->Red();
	  green = c->Green();
	  blue = c->Blue();
	}

	if ((red < 255) || (green < 255) || (blue < 255)) {
	  skip++;
	} else {
	  if (skip) {
	    double si, sip, ss, ssk;
	    si = XSCALEREL(i);
	    sip = XSCALEREL(i+1);
	    ss = fheight - YSCALEREL(skip_start);
	    ssk = fheight - YSCALEREL(skip_start + skip);
	    pstream->Out(si); pstream->Out(" "); pstream->Out(ss); pstream->Out(" moveto\n");
	    pstream->Out(sip); pstream->Out(" "); pstream->Out(ss); pstream->Out(" lineto\n");
	    pstream->Out(sip); pstream->Out(" "); pstream->Out(ssk); pstream->Out(" lineto\n");
	    pstream->Out(si); pstream->Out(" "); pstream->Out(ssk); pstream->Out(" lineto\n");
	  }

	  skip = 0;
	  skip_start = j + 1;
	}
      }
    }
    pstream->Out("clip\n");
  }

  /* Image scale */
  pstream->Out(fwidth); pstream->Out(" "); pstream->Out(fheight); pstream->Out(" scale\n");

  /* Image matrix */
  pstream->Out(width); pstream->Out(" "); pstream->Out(height); pstream->Out(" 8 [ ");
  pstream->Out(width); pstream->Out(" 0 0 "); pstream->Out((-height)); pstream->Out(" 0 "); pstream->Out(height);
  pstream->Out(" ]\n");
  if (rop >= 0) {
    pstream->Out("{\n");
    pstream->Out("  currentfile DataString readhexstring pop\n");
    pstream->Out("} bind");
  } else {
    pstream->Out(" { DataString } ");
  }
  if (asColour) {
    pstream->Out(" false 3 colorimage\n");
  } else {
    pstream->Out(" image\n");
  }
  
  if (rop < 0) {
    pstream->Out("grestore\n } def \n");
    pstream->Out(" { currentfile DataString readhexstring pop pop } exec\n");
  }

  /* Output data as hex digits: */
  {
    wxBitmap *sbm;
    sbm = src->GetObject();
    mono = (sbm->GetDepth() == 1);
   }

  if (mono && dcolor) {
    pr = dcolor->Red();
    pg = dcolor->Green();
    pb = dcolor->Blue();
  } else
    pr = pg = pb = 0;

  for (j = 0; j < height; j++) {
    for (i = 0; i < width; i++) {
      int red, green, blue;

      source->GetPixel(i, j, c);
      
      red = c->Red();
      green = c->Green();
      blue = c->Blue();

      if (mono && !red && !green && !blue) {
	red = pr;
	green = pg;
	blue = pb;
      } else if (mono) {
	if ((rop != wxSOLID) && (rop != (-wxSOLID - 1))) {
	  red = current_background_color->Red();
	  green = current_background_color->Green();
	  blue = current_background_color->Blue();
	}
      }

      if (asColour) {
	printhex(pstream, red);
	printhex(pstream, green);
	printhex(pstream, blue);

	/* Avoid making lines longer than 255 chars: */
	if (i && !(i & 0x1F))
	  pstream->Out("\n");
      } else {
	double r, gr, b;

	r = ((double)(red) / 255);
	gr = ((double)(green) / 255);
	b = ((double)(blue) / 255);

	pixel = (int)(255 * sqrt(((r * r) + (gr * gr) + (b * b)) / 3));
	
	printhex(pstream, pixel);

	/* Avoid making lines longer than 255 chars: */
	if (i && !(i & 0x3F))
	  pstream->Out("\n");
      }

    }
    pstream->Out("\n");
  }

  if (rop >= 0) {
    pstream->Out("grestore\n");
    /* End dictionary: */
    pstream->Out("end\n");
  }

  if (rop >= 0) {
    CalcBoundingBoxClip(XSCALEBND(xdest), YSCALEBND(ydest));
    /* Bitmap isn't scaled: */
    CalcBoundingBoxClip(XSCALEBND(xdest) + fwidth, YSCALEBND(ydest) + fheight);
  }

  return TRUE;
}

static wxMemoryDC *temp_mdc, *temp_mask_mdc;

Bool wxPostScriptDC::Blit (double xdest, double ydest, double fwidth, double fheight,
      wxBitmap *bm, double xsrc, double ysrc, int rop, wxColour *c, wxBitmap *mask)
{
  Bool v;
  wxMemoryDC *mask_dc = NULL, *main_dc = NULL;

#ifdef wx_msw
  main_dc = (wxMemoryDC *)bm->selectedInto;
#endif
  if (!main_dc) {
    if (!temp_mdc) {
      wxREGGLOB(temp_mdc);
      temp_mdc = new WXGC_PTRS wxMemoryDC(1);
    }
    temp_mdc->SelectObject(bm);
    /* Might fail, so we double-check: */
    if (temp_mdc->GetObject())
      main_dc = temp_mdc;
  }

  if (mask) {
#ifdef wx_msw
   mask_dc = (wxMemoryDC *)mask->selectedInto;
#endif
   if (!mask_dc) {
     if (!temp_mask_mdc) {
       wxREGGLOB(temp_mask_mdc);
       temp_mask_mdc = new WXGC_PTRS wxMemoryDC(1);
     } 
     temp_mask_mdc->SelectObject(mask);
     if (temp_mask_mdc->GetObject()) {
       mask_dc = temp_mask_mdc;
     }
   }
  }
  
  if (main_dc) {
    v = Blit(xdest, ydest, fwidth, fheight,
	     main_dc, xsrc, ysrc, rop, c, mask_dc);
    if (main_dc == temp_mdc)
      temp_mdc->SelectObject(NULL);
  } else
    v = FALSE;

  if (mask_dc && (mask_dc == temp_mask_mdc)) {
    mask_dc->SelectObject(NULL);
  }

  return v;
}

double wxPostScriptDC::GetCharHeight (void)
{
  if (current_font)
    return (double) current_font->GetPointSize ();
  else
    return 12.0;
}


double wxPostScriptDC::GetCharWidth (void)
{
  return 0;
}

void wxPostScriptDC::GetTextExtent (const char *string, double *x, double *y,
				    double *descent, double *topSpace, wxFont *theFont,
				    Bool combine, Bool use16, int dt, int slen)
{
  wxFont *fontToUse = theFont;
  int family;
  int size;
  int style;
  int weight;
  int sym_map;
  const char *name;

  if (!fontToUse)
    fontToUse = current_font;

  family = fontToUse->GetFontId();
  size =   fontToUse->GetPointSize();
  style =  fontToUse->GetStyle();
  weight = fontToUse->GetWeight();

  name = wxTheFontNameDirectory->GetPostScriptName(family, weight, style);
  if (!name)
    name = "Times-Roman";

  sym_map = fontToUse->GetFamily() == wxSYMBOL;

  wxPostScriptGetTextExtent(name, string, dt, slen, combine, use16, size,
			    x, y, descent, topSpace, sym_map);
}

int wxPostScriptDC::CacheFontMetricsKey()
{
  return 2;
}

void wxPostScriptDC::SetMapMode (int WXXTUNUSED(mode))
{
#ifndef wx_xt
  mapping_mode = mode;
#endif
  return;
}

void wxPostScriptDC::SetUserScale (double x, double y)
{
  user_scale_x = x;
  user_scale_y = y;
  resetFont |= RESET_FONT;
}

double wxPostScriptDC::DeviceToLogicalX(int x)
{
  return (x - device_origin_x) / user_scale_x;
}

double wxPostScriptDC::DeviceToLogicalXRel(int x)
{
  return x / user_scale_x;
}

double wxPostScriptDC::UnscrolledDeviceToLogicalX(int x)
{
  return DeviceToLogicalX(x);
}

double wxPostScriptDC::DeviceToLogicalY(int y)
{
  double y2 = -(y - paper_h);
  return (y2 - device_origin_y) / user_scale_y;
}

double wxPostScriptDC::DeviceToLogicalYRel(int y)
{
  return y / user_scale_y;
}

double wxPostScriptDC::UnscrolledDeviceToLogicalY(int y)
{
  return DeviceToLogicalY(y);
}

int wxPostScriptDC::LogicalToDeviceX(double x)
{
  return (int)floor(XSCALE(x));
}

int wxPostScriptDC::LogicalToDeviceXRel(double x)
{
  return (int)floor(XSCALEREL(x));
}

int wxPostScriptDC::LogicalToUnscrolledDeviceX(double x)
{
  return LogicalToDeviceX(x);
}

int wxPostScriptDC::LogicalToDeviceY(double y)
{
  return (int)floor(YSCALE(y));
}

int wxPostScriptDC::LogicalToDeviceYRel(double y)
{
  return (int)floor(YSCALEREL(y));
}

int wxPostScriptDC::LogicalToUnscrolledDeviceY(double y)
{
  return LogicalToDeviceY(y);
}

double wxPostScriptDC::FLogicalToDeviceX(double x)
{
  return XSCALE(x);
}

double wxPostScriptDC::FLogicalToDeviceXRel(double x)
{
  return XSCALEREL(x);
}

double wxPostScriptDC::FLogicalToUnscrolledDeviceX(double x)
{
  return FLogicalToDeviceX(x);
}

double wxPostScriptDC::FLogicalToDeviceY(double y)
{
  return YSCALE(y);
}

double wxPostScriptDC::FLogicalToDeviceYRel(double y)
{
  return YSCALEREL(y);
}

double wxPostScriptDC::FLogicalToUnscrolledDeviceY(double y)
{
  return FLogicalToDeviceY(y);
}

double wxPostScriptDC::FsLogicalToDeviceX(double x, double device_origin_x, double user_scale_x)
{
  /* Intentional capture of arguments by macro! */
  return XSCALE(x);
}

double wxPostScriptDC::FsLogicalToDeviceXRel(double x, double device_origin_x, double user_scale_x)
{
  /* Intentional capture of arguments by macro! */
  return XSCALEREL(x);
}

double wxPostScriptDC::FsLogicalToDeviceY(double y, double device_origin_y, double user_scale_y)
{
  /* Intentional capture of arguments by macro! */
  return YSCALE(y);
}

double wxPostScriptDC::FsLogicalToDeviceYRel(double y, double device_origin_y, double user_scale_y)
{
  /* Intentional capture of arguments by macro! */
  return YSCALEREL(y);
}

void wxPostScriptDC::GetSize(double *width, double *height)
{
  if (width)
    *width = paper_w;
  if (height)
    *height = paper_h;
}

void wxPostScriptDC::GetSizeMM(double *WXUNUSED(width), double *WXUNUSED(height))
{
}

extern Bool wxsPrinterDialog(wxWindow *parent);

Bool XPrinterDialog(wxWindow *parent)
{
  return wxsPrinterDialog(parent);
}

//-----------------------------------------------------------------------------
// wxPrintSetup implementation
//-----------------------------------------------------------------------------

#define PS_DEFAULT_PAPER  "Letter 8 1/2 x 11 in"

#define PS_PREVIEW_COMMAND "gv"
#define PS_PRINTER_COMMAND "lpr"
#define PS_PRINTER_OPTIONS ""
#define PS_AFM_PATH		NULL

wxPrintSetupData::wxPrintSetupData(void)
{
    printer_command = PS_PRINTER_COMMAND;
    preview_command = PS_PREVIEW_COMMAND;
    printer_flags = PS_PRINTER_OPTIONS;
    printer_orient = PS_PORTRAIT;
    printer_scale_x = 0.8;
    printer_scale_y = 0.8;
    printer_translate_x = 0.0;
    printer_translate_y = 0.0;
#ifdef wx_x
    printer_mode = PS_PREVIEW;
#else
    printer_mode = PS_FILE;
#endif
    afm_path = default_afm_path;
    paper_name = DEFAULT_PAPER;
    print_colour = TRUE;
    print_level_2 = TRUE;
    printer_file = NULL;
    emargin_v = emargin_h = 20;
    ps_margin_v = ps_margin_h = 16;
}

wxPrintSetupData::~wxPrintSetupData(void)
{
}

void wxPrintSetupData::SetPrinterCommand(char *cmd)
{
    if (cmd == printer_command)
	return;
    if (cmd) {
	printer_command = copystring(cmd);
    } else
	printer_command = NULL;
}

void wxPrintSetupData::SetPrintPreviewCommand(char *cmd)
{
    if (cmd == preview_command)
	return;
    if (cmd) {
	preview_command = copystring(cmd);
    } else
	preview_command = NULL;
}

void wxPrintSetupData::SetPaperName(char *name)
{
  if (name == paper_name)
    return;
  if (name) {
    paper_name = copystring(name);
  } else
    paper_name = NULL;
}

void wxPrintSetupData::SetPrinterOptions(char *flags)
{
    if (printer_flags == flags)
      return;
    if (flags) {
      printer_flags = copystring(flags);
    } else
      printer_flags = NULL;
}

void wxPrintSetupData::SetPrinterFile(char *f)
{
    if (f == printer_file)
	return;
    if (f) {
	printer_file = copystring(f);
    } else
	printer_file = NULL;
}

void wxPrintSetupData::SetPrinterMode(int mode)
{
    printer_mode = PS_FILE;

    if (mode == PS_PREVIEW && preview_command
    ||  mode == PS_PRINTER && printer_command)
	printer_mode = mode;
}

void wxPrintSetupData::SetPrinterOrientation(int orient)
{ 
  printer_orient = orient; 
#ifdef wx_mac
  if (native) {
    native->SetLandscape(printer_orient == PS_LANDSCAPE);
  }
#endif
}

void wxPrintSetupData::SetAFMPath(char *f)
{
    if (f && !default_afm_path) {
      wxREGGLOB(default_afm_path);
      default_afm_path = f;
    }
  
    if (f == afm_path)
	return;
    if (f) {
      afm_path = copystring(f);
    } else
	afm_path = NULL;
}

void wxPrintSetupData::copy(wxPrintSetupData* data)
{
  double x, y;
  long lx, ly;
  char *s;
  int i;
  
  s = data->GetPrinterCommand();
  SetPrinterCommand(s);
  s = data->GetPrintPreviewCommand();
  SetPrintPreviewCommand(s);
  s = data->GetPrinterOptions();
  SetPrinterOptions(s);
  i = data->GetPrinterOrientation();
  SetPrinterOrientation(i);
  i = data->GetPrinterMode();
  SetPrinterMode(i);
  s = data->GetAFMPath();
  SetAFMPath(s);
  s = data->GetPaperName();
  SetPaperName(s);
  i = data->GetColour();
  SetColour(i);
  
  data->GetPrinterTranslation(&x, &y);
  SetPrinterTranslation(x, y);
  data->GetPrinterScaling(&x, &y);
  SetPrinterScaling(x, y);
  data->GetMargin(&x, &y);
  SetMargin(x, y);
  data->GetEditorMargin(&lx, &ly);
  SetEditorMargin(lx, ly);

#ifdef wx_mac
  if (data->native) {
    wxPrintData *n;
    n = data->native->copy();
    native = n;
  }
#endif
}

Bool wxPrintSetupData::CanShowNative()
{
#ifdef wx_mac
  return TRUE;
#else
  return FALSE;
#endif
}

Bool wxPrintSetupData::ShowNative(wxWindow *parent)
{
#ifdef wx_mac
  wxPrintDialog *d;
  int ls;
  Bool ok;

  if (!native) {
    native = new WXGC_PTRS wxPrintData();
    native->SetLandscape(printer_orient == PS_LANDSCAPE);
    native->SetScale(printer_scale_y);
  }

  d = new WXGC_PTRS wxPrintDialog(parent, native);
  ok = d->UseIt();
  DELETE_OBJ d;

  if (ok) {
    ls = native->GetLandscape();
    printer_orient = (ls ? PS_LANDSCAPE : PS_PORTRAIT);
    printer_scale_y = native->GetScale();
    printer_scale_x = printer_scale_y;
  }
  return ok;
#else
  return TRUE;
#endif
}

//-----------------------------------------------------------------------------
// wxInitializePrintSetupData
//-----------------------------------------------------------------------------

void wxInitializePrintSetupData(Bool /* init */)
{
  wxPrintSetupData *wxThePrintSetupData;
  
  wxThePrintSetupData = new WXGC_PTRS wxPrintSetupData;
  
  wxThePrintSetupData->SetPrintPreviewCommand(PS_PREVIEW_COMMAND);
  wxThePrintSetupData->SetPrinterOrientation(PS_PORTRAIT);
#ifdef wx_x
  wxThePrintSetupData->SetPrinterMode(PS_PREVIEW);
#else
  wxThePrintSetupData->SetPrinterMode(PS_FILE);
#endif
  wxThePrintSetupData->SetPaperName(PS_DEFAULT_PAPER);
  wxThePrintSetupData->SetPrinterCommand(PS_PRINTER_COMMAND);
  wxThePrintSetupData->SetPrinterOptions(PS_PRINTER_OPTIONS);
  wxThePrintSetupData->SetAFMPath(PS_AFM_PATH);
  
  wxSetThePrintSetupData(wxThePrintSetupData);
}

//-----------------------------------------------------------------------------
// wxPrintPaperType implementation
//-----------------------------------------------------------------------------

wxPrintPaperType::wxPrintPaperType(char *name, int wmm, int hmm, int wp, int hp)
{
    widthMM = wmm;
    heightMM = hmm;
    widthPixels = wp;
    heightPixels = hp;
    pageName = copystring(name);
}

wxPrintPaperType::~wxPrintPaperType(void)
{
}


//-----------------------------------------------------------------------------
// wxPrintPaperDatabase implementation
//-----------------------------------------------------------------------------

wxPrintPaperDatabase::wxPrintPaperDatabase(void) : wxList(wxKEY_STRING)
{
    DeleteContents(TRUE);
}

wxPrintPaperDatabase::~wxPrintPaperDatabase(void)
{
}

void wxPrintPaperDatabase::CreateDatabase(void)
{
    // Need correct values for page size in pixels.
    // Each unit is one 'point' = 1/72 of an inch.
    // NOTE: WE NEED ALSO TO MAKE ADJUSTMENTS WHEN TRANSLATING
    // in wxPostScriptDC code, so we can start from top left.
    // So access this database and translate by appropriate number
    // of points for this paper size. OR IS IT OK ALREADY?
    // Can't remember where the PostScript origin is by default.
    // Heck, someone will know how to make it hunky-dory...
    // JACS 25/5/95
  
    AddPaperType("A4 210 x 297 mm", 210, 297,         595, 842);
    AddPaperType("A3 297 x 420 mm", 297, 420,         842, 1191);
    AddPaperType("Letter 8 1/2 x 11 in", 216, 279,    612, 791);
    AddPaperType("Legal 8 1/2 x 14 in", 216, 356,     612, 1009);
}

void wxPrintPaperDatabase::ClearDatabase(void)
{
    Clear();
}

void wxPrintPaperDatabase::AddPaperType(char *name, int wmm, int hmm,
					int wp, int hp)
{
  wxPrintPaperType *ppt;
  ppt = new WXGC_PTRS wxPrintPaperType(name, wmm, hmm, wp, hp);
  Append(name, ppt);
}

wxPrintPaperType *wxPrintPaperDatabase::FindPaperType(char *name)
{
  wxNode *node;

  if ((node = Find(name)))
    return (wxPrintPaperType*)node->Data();
  else
    return NULL;
}
