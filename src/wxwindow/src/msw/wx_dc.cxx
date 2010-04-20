/*
 * File:	wx_dc.cc
 * Purpose:	Device context implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include "../../../wxcommon/Region.h"
#include "wx_pdf.h"
#include "wx_graphics.h"
#include "../../../racket/include/scheme.h"

#include "../../../wxcommon/wxGLConfig.h"

#include <math.h>

#include <commdlg.h>

// Declarations local to this file

static wxMemoryDC *blit_dc, *blit_mdc;

#define wxPI 3.141592653589793

#define YSCALE(y) (yorigin - (y))

static HANDLE null_brush;
static HANDLE null_pen;

void RegisterGDIObject(HANDLE x);

extern Bool wx_gdi_plus;
extern void wxInitGraphicsPlus(void);

typedef struct {
  double sx, sy, angle;
} wxSizeKey;

static Scheme_Object *theSizeKey;

void wxGDIStartup(void)
{
  wxInitGraphicsPlus();
  if (wx_gdi_plus)
    wxGStartup();
}

void wxGDIShutdown(void)
{
  if (wx_gdi_plus)
    wxGShutdown();
}

static int is_nt()
{
  static int nt = -1;
  if (nt < 0) {
    OSVERSIONINFO info;
    info.dwOSVersionInfoSize = sizeof(info);
    GetVersionEx(&info);
    if (info.dwPlatformId == VER_PLATFORM_WIN32_NT)
      nt = 1;
    else
      nt = 0;
  }
  return nt;
}

class wxWinGL : public wxGL {
public:
  wxWinGL();

  int Ok();

  void SwapBuffers(void);
  void ThisContextCurrent(void);

  void Reset(wxGLConfig *cfg, HDC dc, int offscreen);

  void SetupPalette(PIXELFORMATDESCRIPTOR *pfd);
  wxColourMap* CreateDefaultPalette(PIXELFORMATDESCRIPTOR *pfd);

  HGLRC m_hGLRC;
  HDC m_hDC;
  wxColourMap *m_palette;
  Bool m_deletePalette;
};

/******************************************************************/

// Default constructor
wxDC::wxDC(void)
{
  __type = wxTYPE_DC;
  filename = NULL;
  selected_bitmap = NULL;
  canvas = NULL;
  cur_dc = NULL;
  cur_bk = 0;
  old_bitmap = 0;
  old_pen = 0;
  old_brush = 0;
  old_font = 0;
  old_palette = 0;
  cur_rop = -1;
  font = wxNORMAL_FONT;
  device_origin_x = 0;
  device_origin_y = 0;
  user_scale_x = 1.0;
  user_scale_y = 1.0;
  logical_scale_x = 1.0;
  logical_scale_y = 1.0;
  canvas_scroll_dx = 0.0;
  canvas_scroll_dy = 0.0;
  mapping_mode = wxPIXELS_MAP;
  scaling_mode = wxWX_SCALE;
  title = NULL;
  dont_delete = FALSE;
  cdc = NULL;
  clipping = NULL;
  screen_font = TRUE;
  ok = TRUE;
  window_ext_x = VIEWPORT_EXTENT;
  window_ext_y = VIEWPORT_EXTENT;
  current_pen = NULL;
  current_brush = NULL;
  current_background_color = new wxColour(wxWHITE);
  current_text_foreground = new wxColour(wxBLACK);
  current_text_background = new wxColour(wxWHITE);
  current_bk_mode = wxTRANSPARENT;
  Colour = wxColourDisplay();

  null_pen = ::GetStockObject(NULL_PEN);
  null_brush = ::GetStockObject(NULL_BRUSH);

  if (!theSizeKey) {
    wxREGGLOB(theSizeKey);
    theSizeKey = scheme_alloc_byte_string(sizeof(wxSizeKey), 0);
  }
}


wxDC::~wxDC(void)
{
  ReleaseGraphics();
  
  if (current_pen) current_pen->Lock(-1);
  if (current_brush) current_brush->Lock(-1);
  if (clipping) clipping->Lock(-1);

  if (filename)
    delete[] filename;

  if (wx_gl) {
    wx_gl->Reset(NULL, 0, 0);
    wx_gl = NULL;
  }

  if (cdc)
  {
    SelectOldObjects(cdc);
    DeleteDC(cdc);
  }
}

// This will select current objects out of the DC,
// which is what you have to do before deleting the
// DC.
void wxDC::SelectOldObjects(HDC dc)
{
  if (dc)
  {
    if (old_bitmap) {
      ::SelectObject(dc, old_bitmap);
      if (selected_bitmap) {
        selected_bitmap->selectedInto = NULL;
        selected_bitmap->selectedIntoDC = 0;
      }
      selected_bitmap = NULL;
    }
    old_bitmap = NULL;

    if (old_pen) {
      ::SelectObject(dc, old_pen);
    }
    old_pen = NULL;

    if (old_brush) {
      ::SelectObject(dc, old_brush);
    }
    old_brush = NULL;

    if (old_font) {
      ::SelectObject(dc, old_font);
    }
    old_font = NULL;

    if (old_palette) {
      ::SelectPalette(dc, old_palette, TRUE);
    }
    old_palette = NULL;
  }
}

HDC wxDC::ThisDC(Bool flush_cache)
{
  HDC dc = NULL;
  wxWnd *wnd = NULL;

  if (flush_cache)
    ReleaseSelectedCache();

  if (cdc)
    dc = cdc;
  else {
    if (canvas) {
      wnd = (wxWnd *)canvas->handle;
      if (wnd)
	dc = wnd->GetHDC();
    }
  }

  if (!old_pen) {
    HPEN op;
    HBRUSH ob;
    op = (HPEN)::SelectObject(dc, null_pen);
    old_pen = op;
    ob = (HBRUSH)::SelectObject(dc, null_brush);
    old_brush = ob;
    ResetMapMode(dc);
  }

  return dc;
}

void wxDC::DoneDC(HDC dc)
{
  if (dc && !cdc) {
    wxWnd *wnd = NULL;
    if (canvas) wnd = (wxWnd *)canvas->handle;
    if (!cdc && wnd) {
      ReleaseGraphics(dc);
      wnd->ReleaseHDC();
    }
  }
}

void wxDC::ReleaseSelectedCache()
{
  if (selected_bitmap) {
    if (selected_bitmap->mask_cache) {
      selected_bitmap->ReleaseCachedMask();
    }
  }
}

void wxBitmap::ReleaseCachedMask()
{
  wxBitmap *bm;
  wxMemoryDC *mdc;

  if (mask_cache) {
    mdc = mask_cache;
    mask_cache = NULL;
    bm = mdc->selected_bitmap;
    if (bm) {
      mdc->SelectObject(NULL);
      DELETE_OBJ bm;
      bm = NULL;
    }
    mdc->refcount--;
    if (!mdc->refcount) {
      DELETE_OBJ mdc;
      mdc = NULL;
    }
  }
}

wxGL *wxDC::GetGL()
{
  if (!wx_gl) {
    if (__type == wxTYPE_DC_CANVAS) {
      wx_gl = new wxWinGL();
      wx_gl->Reset(wx_gl_cfg, cdc, 0);
    }
  }

  return wx_gl;
}

void wxDC::ShiftXY(double x, double y, int *ix, int *iy)
{
  if (scaling_mode == wxWINDOWS_SCALE) {
    *ix = (int)floor(x) + canvas_scroll_dx;
    *iy = (int)floor(y) + canvas_scroll_dy;
  } else {
    *ix = MS_XLOG2DEV(x);
    *iy = MS_YLOG2DEV(y);
  }
}


Bool wxDC::AlignSmoothing()
{
  return (anti_alias == 2);
}

double wxDC::GetPenSmoothingOffset()
{
  int pw;
  pw = current_pen->GetWidth();
  pw = (int)(user_scale_x * pw);
  if (!pw)
    pw = 1;
  return ((pw & 1) * 0.5);
}

double wxDC::SmoothingXFormX(double x)
{
  if (AlignSmoothing())
    return floor((x * user_scale_x) + device_origin_x + canvas_scroll_dx) + GetPenSmoothingOffset();
  else
    return x;
}

double wxDC::SmoothingXFormY(double y)
{
  if (AlignSmoothing())
    return floor((y * user_scale_y) + device_origin_y + canvas_scroll_dy) + GetPenSmoothingOffset();
  else
    return y;
}

double wxDC::SmoothingXFormW(double w, double x)
{
  if (AlignSmoothing())
    return SmoothingXFormX(x + w) - SmoothingXFormX(x);
  else
    return w;
}

double wxDC::SmoothingXFormH(double h, double y)
{
  if (AlignSmoothing())
    return SmoothingXFormY(y + h) - SmoothingXFormY(y);
  else
    return h;
}

double wxDC::SmoothingXFormXB(double x)
{
  if (AlignSmoothing())
    return floor((x * user_scale_x) + device_origin_x + canvas_scroll_dx);
  else
    return x;
}

double wxDC::SmoothingXFormYB(double y)
{
  if (AlignSmoothing())
    return floor((y * user_scale_y) + device_origin_y + canvas_scroll_dy);
  else
    return y;
}

double wxDC::SmoothingXFormWL(double w, double x)
{
  if (AlignSmoothing()) {
    w = SmoothingXFormW(w, x);
    if (w >= 1.0)
      return w - 1.0;
    else
      return w;
  } else
    return w;
}

double wxDC::SmoothingXFormHL(double h, double y)
{
  if (AlignSmoothing()) {
    h = SmoothingXFormH(h, y);
    if (h >= 1.0)
      return h - 1.0;
    else
      return h;
  } else
    return h;
}

void wxDC::SetClippingRect(double cx, double cy, double cw, double ch)
{
  wxRegion *c;

  c = new wxRegion(this);
  c->SetRectangle(cx, cy, cw, ch);
  SetClippingRegion(c);
}

wxRegion* wxDC::GetClippingRegion()
{
  return clipping;
}

void wxDC::SetClippingRegion(wxRegion *c)
{
  HDC dc;

  ReleaseGraphics();

  if (c && (c->dc != this)) return;

  if (clipping)
    clipping->Lock(-1);

  clipping = c;

  if (clipping)
    clipping->Lock(1);

  dc = ThisDC(FALSE);
  if (dc) DoClipping(dc);
  DoneDC(dc);
}

static HRGN empty_rgn, full_rgn;

void wxDC::DoClipping(HDC dc)
{
  if (clipping) {
    HRGN rgn;
    rgn = clipping->GetRgn();
    if (rgn) {
      if (limit_rgn) {
	HRGN together_rgn;
	together_rgn = CreateRectRgn(0,0,0,0);
	CombineRgn(together_rgn, rgn, together_rgn, RGN_COPY);
	OffsetRgn(together_rgn, canvas_scroll_dx, canvas_scroll_dy);
	CombineRgn(together_rgn, limit_rgn, together_rgn, RGN_AND);
	SelectClipRgn(dc, together_rgn);
	DeleteObject(together_rgn);
      } else {
	SelectClipRgn(dc, rgn);
	OffsetClipRgn(dc, canvas_scroll_dx, canvas_scroll_dy);
      }
    } else {
      if (!empty_rgn)
	empty_rgn = CreateRectRgn(0, 0, 0, 0);
      SelectClipRgn(dc, empty_rgn);
    }
  } else {
    if (limit_rgn) {
      SelectClipRgn(dc, limit_rgn);
    } else {
      if (!full_rgn)
	full_rgn = CreateRectRgn(0, 0, 32000, 32000);
      SelectClipRgn(dc, full_rgn);
    }
  }
}

Bool wxDC::CanDrawBitmap(void)
{
  return TRUE;
}

Bool wxDC::CanGetTextExtent(void)
{
  HDC dc;
  Bool tok;

  dc = ThisDC(FALSE);
  
  // What sort of display is it?

  if (dc) {
    int technology;
    
    technology = ::GetDeviceCaps(dc, TECHNOLOGY);
    
    if (technology != DT_RASDISPLAY && technology != DT_RASPRINTER)
      tok = FALSE;
    else 
      tok = TRUE;
  } else
    tok = FALSE;

  DoneDC(dc);
  
  return tok;
}

int wxDC::CacheFontMetricsKey()
{
  if ((user_scale_x == 1.0)
      && (user_scale_y == 1.0))
    return 1;
  return 0;
}

void wxDC::SetColourMap(wxColourMap *cmap)
{
  HDC dc;

  dc = ThisDC();

  if (!dc) return;

  if (!cmap) {
    // Setting a NULL colourmap is a way of restoring
    // the original colourmap
    if (old_palette) {
      ::SelectPalette(dc, old_palette, TRUE);
      old_palette = 0;
    }
  }
    
  if (cmap && cmap->ms_palette) {
    HPALETTE oldPal;
    oldPal = ::SelectPalette(dc, cmap->ms_palette, TRUE);
    if (!old_palette)
      old_palette = oldPal;
      
    ::RealizePalette(dc);
  }

  DoneDC(dc);
}

void wxDC::InitGraphics(HDC dc)
{
  if (!g) {
    /* Turn off GDI-level scaling: */
    SetScaleMode(wxWX_SCALE, dc); 
    /* Turn off GDI-level clipping: */
    {
      HRGN rgn;
      rgn = CreateRectRgn(0, 0, 32000, 32000);
      SelectClipRgn(dc, rgn);
      DeleteObject(rgn);
    }

    g = wxGMake(dc);

    if (wxSubType(__type, wxTYPE_DC_PRINTER))
      wxGSetPageUnit(g, UnitPoint);

    /* Scroll translate before clip: */
    wxGTranslate(g, canvas_scroll_dx, canvas_scroll_dy);

    /* Clip before scale and user offset, because the region has its
       own internal scale (remembered at the time that the region was
       created). */
    if (clipping)
      clipping->Install((long)g, AlignSmoothing());

    if (!AlignSmoothing()) {
      wxGTranslate(g, device_origin_x, device_origin_y);
      wxGScale(g, user_scale_x, user_scale_y);
    } else {
      /* Undo scroll translate: */
      wxGTranslate(g, -canvas_scroll_dx, -canvas_scroll_dy);
    }
  }
}

void wxDC::ReleaseGraphics(HDC given_dc)
{
  if (g) {
    HDC dc;

    wxGRelease(g);
    g = NULL;

    if (given_dc)
      dc = given_dc;
    else
      dc = ThisDC(FALSE);
    if (dc) {
      DoClipping(dc);
      if (!given_dc)
	DoneDC(dc);
    }
  }
}

void wxDC::OnCalcScroll(void)
{
  if (canvas) {
    int dx, dy;
    wxWnd *wnd = (wxWnd *)canvas->handle;
    wnd->CalcScrolledPosition(0, 0, &dx, &dy);
    canvas_scroll_dx = dx;
    canvas_scroll_dy = dy;
  }

  ReleaseGraphics();
  if (clipping) {
    HDC dc;
    dc = ThisDC(FALSE);
    DoClipping(dc);
    DoneDC(dc);
  }
}

void wxDC::SetAntiAlias(Bool v)
{
  if (wx_gdi_plus && Colour)
    wxbDC::SetAntiAlias(v);
}

void wxDC::Clear(void)
{
  HDC dc;
  RECT rect;

  dc = ThisDC();

  if (!dc) return;

  /* Get the DC's width and height */
  if (canvas)
    GetClientRect(((wxWnd *)canvas->handle)->handle, &rect);
  else if (selected_bitmap) {
    rect.left = 0; rect.top = 0;
    rect.right = selected_bitmap->GetWidth();
    rect.bottom = selected_bitmap->GetHeight();
  } else {
    rect.left = 0;
    rect.top = 0;
    rect.right = ::GetDeviceCaps(dc, HORZRES);
    rect.bottom = ::GetDeviceCaps(dc, VERTRES);
  }

  if (anti_alias) {
    GraphicsState s;

    InitGraphics(dc);

    s = wxGSave(g);
    wxGResetTransform(g);
    wxGFillRectangleColor(g, current_background_color->pixel, 0, 0, rect.right, rect.bottom);
    wxGRestore(g, s);

    DoneDC(dc);
    return;
  } else
    ReleaseGraphics(dc);
  
  SetScaleMode(wxWX_SCALE, dc); /* => no scale */

  /* Here's the actual clear operation */
  {
    HBRUSH brush;
    brush = CreateSolidBrush(GetBkColor(dc));
    SetRop(dc, wxSOLID);
    FillRect(dc, &rect, brush);
    DeleteObject(brush);
  }

  DoneDC(dc);
}

void wxDC::BeginDrawing(void)
{
}

void wxDC::EndDrawing(void)
{
}

Bool wxDC::GlyphAvailable(int c, wxFont *f)
{
  HDC dc;
  Bool r;

  if (!f)
    f = font;

  dc = ThisDC(FALSE);
  if (!dc) return 0;

  r = f->GlyphAvailable(c, dc, screen_font);

  DoneDC(dc);

  return r;
}

Bool wxDC::GetPixel(double x, double y, wxColour *col)
{
  int xx1;
  int yy1;
  HDC dc;
  COLORREF pixelcolor;

  dc = ThisDC(FALSE);

  if (!dc) return FALSE;

  ReleaseGraphics(dc);

  ShiftXY(x, y, &xx1, &yy1);

  // get the color of the pixel
  pixelcolor = ::GetPixel(dc, xx1, yy1);
  
  DoneDC(dc);

  if (pixelcolor == CLR_INVALID)
    return FALSE;
  
  // return the color of the pixel
  if (col)
    col->Set(GetRValue(pixelcolor),GetGValue(pixelcolor),GetBValue(pixelcolor));
  
  return TRUE;
}

void wxDC::DrawLine(double x1, double y1, double x2, double y2)
{
  HDC dc;

  dc = ThisDC();

  if (!dc) return;

  if (anti_alias) {
    if (current_pen && (current_pen->GetStyle() != wxTRANSPARENT)) {
      double xx1, yy1, xx2, yy2;

      InitGraphics(dc);

      xx1 = SmoothingXFormX(x1);
      yy1 = SmoothingXFormY(y1);
      xx2 = SmoothingXFormX(x2);
      yy2 = SmoothingXFormY(y2);

      wxGDrawLine(g, current_pen->GraphicsPen(AlignSmoothing(), user_scale_x, current_alpha), 
		  xx1, yy1, xx2, yy2);
    }
    DoneDC(dc);
    return;
  } else
    ReleaseGraphics(dc);

  SetScaleMode(wxWX_SCALE, dc);

  if (StartPen(dc)) {
    int xx1, yy1, xx2, yy2;
    int pw;
    int forward;

    ShiftXY(x1, y1, &xx1, &yy1);
    ShiftXY(x2, y2, &xx2, &yy2);
    
    /* Convention across platforms: line includes pixel on endpoint */
    pw = current_pen->GetWidth();
    forward = 0;
    if (pw <= 1) {
      if (current_pen->GetCap() != wxCAP_BUTT) {
	/* Pen size 1: no need to forward under NT */
	forward = !is_nt();
      } else
	forward = 0;
    }
    if (forward) {
      int dx = (xx2 - xx1);
      int dy = (yy2 - yy1);

      if (!dx && !dy) {
	xx2++;
      } else {
	int adx = ((dx < 0) ? -dx : dx);
	int ady = ((dy < 0) ? -dy : dy);

	if (ady >= adx) {
	  if (yy1 < yy2)
	    yy2++;
	  else
	    --yy2;
	}
	
	if (adx >= ady) {
	  if (xx1 < xx2)
	    xx2++;
	  else
	    --xx2;
	}
      }
    }

    (void)MoveToEx(dc, xx1, yy1, NULL);
    (void)LineTo(dc, xx2, yy2);

    DonePen(dc);
  }

  DoneDC(dc);
}

static void FillWithStipple(wxDC *dc, wxRegion *r, wxBrush *brush)
{
  double x, y, w, h, bw, bh;
  int xstart, xend, ystart, yend, i, j;
  wxRegion *old;
  wxBitmap *bm;
  int style;
  wxColour *c;

  bm = brush->GetStipple();
  style = brush->GetStyle();
  c = brush->GetColour();

  old = dc->GetClippingRegion();
  if (old) r->Intersect(old);

  r->BoundingBox(&x, &y, &w, &h);
  bw = bm->GetWidth();
  bh = bm->GetHeight();

  x = dc->LogicalToUnscrolledDeviceX(x);
  y = dc->LogicalToUnscrolledDeviceY(y);
  w = dc->LogicalToDeviceXRel(w);
  h = dc->LogicalToDeviceYRel(h);
  
  xstart = (int)floor(x / bw);
  xend = (int)floor((x + w + bw - 0.00001) / bw);

  ystart = (int)floor(y / bh);
  yend = (int)floor((y + h + bh - 0.00001) / bh);

  dc->SetClippingRegion(r);

  for (i = xstart; i < xend; i++) {
    for (j = ystart; j < yend; j++) {
      dc->Blit(dc->UnscrolledDeviceToLogicalX(i * bw), 
	       dc->UnscrolledDeviceToLogicalY(j * bh), 
	       dc->DeviceToLogicalXRel(bw), 
	       dc->DeviceToLogicalYRel(bh),
	       bm, 0, 0, style, c);
    }
  }

  dc->SetClippingRegion(old);
}

static int round(double f)
{
  double d;
  
  (void)modf(f, &d);

  return (int)d;
}

void wxDC::DrawArc(double x, double y, double w, double h, double start, double end)
{
  int xx1, yy1, xx2, yy2, hh, ww;
  double cx, cy;
  double rx1, ry1, rx2, ry2;

  HDC dc;

  dc = ThisDC();

  if (!dc) return;

  if (anti_alias) {
    double span, init;

    InitGraphics(dc);

    if ((start == 0.0) && (end == 2 * wxPI)) {
      init = 0.0;
      span = 360.0;
    } else {
      init = (2 * wxPI - start) * 180 / wxPI;
      init = fmod(init, 360.0);
      if (init < 0.0)
	init += 360.0;
      
      span = (start - end) * 180 / wxPI;
      span = fmod(span, 360.0);
      if (span > 0)
	span -= 360.0;
    }

    if (current_brush && (current_brush->GetStyle() != wxTRANSPARENT)) {
      double xx, yy, ww, hh;

      xx = SmoothingXFormXB(x);
      yy = SmoothingXFormYB(y);
      ww = SmoothingXFormW(w, x);
      hh = SmoothingXFormH(h, y);

      wxGFillPie(g, current_brush->GraphicsBrush(current_alpha), xx, yy, ww, hh, init, span);
    }

    if (current_pen && (current_pen->GetStyle() != wxTRANSPARENT)) {
      double xx, yy, ww, hh;

      xx = SmoothingXFormX(x);
      yy = SmoothingXFormY(y);
      ww = SmoothingXFormWL(w, x);
      hh = SmoothingXFormHL(h, y);
      
      wxGDrawArc(g, current_pen->GraphicsPen(AlignSmoothing(), user_scale_x, current_alpha), 
		 xx, yy, ww, hh, init, span);
    }

    DoneDC(dc);
    return;
  } else
    ReleaseGraphics(dc);

  if (StippleBrush()) {
    wxRegion *r;
    r = new wxRegion(this);
    r->SetArc(x, y, w, h, start, end);
    FillWithStipple(this, r, current_brush);
  }

  SetScaleMode(wxWX_SCALE, dc);
  
  ShiftXY(x, y, &xx1, &yy1);
  ShiftXY(x + w, y + h, &xx2, &yy2);
  hh = yy2 - yy1;
  ww = xx2 - xx1;
  
  /* Adjust w & h for Windows conventions: */
  hh++; xx2++;
  ww++; yy2++;

  cx = xx1 + (double)ww/2;
  cy = yy1 + (double)hh/2;

  rx1 = cx + ((double)ww / 2) * cos(start);
  ry1 = cy - (((double)hh / 2) * sin(start));
  rx2 = cx + ((double)ww / 2) * cos(end);
  ry2 = cy - (((double)hh / 2) * sin(end));

  if (StartBrush(dc, 1)) {
    Pie(dc, xx1, yy1, xx2, yy2, 
	round(rx1), round(ry1), 
	round(rx2), round(ry2));
    DoneBrush(dc);
  }

  if (StartPen(dc)) {
    Arc(dc, xx1, yy1, xx2, yy2, 
	round(rx1), round(ry1), 
	round(rx2), round(ry2));
    DonePen(dc);
  }
  
  DoneDC(dc);
}

void wxDC::DrawPoint(double x, double y)
{
  if (current_pen)
    SetPixel(x, y, NULL);
}

void wxDC::SetPixel(double x, double y, wxColour *c)
{
  wxWnd *wnd = NULL;
  int xx1, yy1;
  HDC dc;

  dc = ThisDC();

  if (!dc) return;

  ReleaseGraphics(dc);
  SetScaleMode(wxWX_SCALE, dc);

  ShiftXY(x, y, &xx1, &yy1);
  
  if (!c) {
    c = current_pen->GetColour();
    if (StartPen(dc)) {
      ::SetPixelV(dc, xx1, yy1, c->pixel);
      DonePen(dc);
    }
  } else {
    SetRop(dc, wxSOLID);
    ::SetPixelV(dc, xx1, yy1, c->pixel);
  }

  DoneDC(dc);
}

Bool wxDC::BeginSetPixelFast(int x, int y, int w, int h)
{
  double ww, hh;

  ReleaseGraphics();
  SetScaleMode(wxWINDOWS_SCALE);
  
  GetSize(&ww, &hh);

  return ((x >= 0) && (y >= 0)
	  && ((x + w) <= (int)ww)
	  && ((y + h) <= (int)hh));
}

void wxDC::EndSetPixelFast()
{
}

void wxDC::SetPixelFast(int x1, int y1, int r, int g, int b)
{
  ::SetPixelV(cdc, x1, y1, RGB(r, g, b));
}

Bool wxDC::BeginGetPixelFast(int x, int y, int w, int h)
{
  return BeginSetPixelFast(w, y, w, h);
}

void wxDC::EndGetPixelFast()
{
}

void wxDC::GetPixelFast(int x1, int y1, int *r, int *g, int *b)
{
  COLORREF pixelcolor;
  
  pixelcolor = ::GetPixel(cdc, x1, y1);
  
  *r = GetRValue(pixelcolor);
  *g = GetGValue(pixelcolor);
  *b = GetBValue(pixelcolor);
}

#ifdef MZ_PRECISE_GC
static PointF *newPointFs(int n);
START_XFORM_SKIP;
#endif

static PointF *newPointFs(int n)
{
  return new WXGC_ATOMIC PointF[n];
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

void wxDC::DrawPolygon(int n, wxPoint points[], double xoffset, double yoffset,int fillStyle)
{
  HDC dc;
  int xoffset1;
  int yoffset1;
  POINT *cpoints;
  int i, prev;

  dc = ThisDC();

  if (!dc) return;

  if (anti_alias) {
    PointF *pts;
    
    InitGraphics(dc);
    
    pts = newPointFs(n);
    for (i = 0; i < n; i++) {
      double x, y;
      x = SmoothingXFormX(points[i].x + xoffset);
      y = SmoothingXFormY(points[i].y + yoffset);
      pts[i].X = x;
      pts[i].Y = y;
    }

    if (current_brush && (current_brush->GetStyle() != wxTRANSPARENT)) {
      wxGFillPolygon(g, current_brush->GraphicsBrush(current_alpha), pts, n, 
		     (fillStyle == wxODDEVEN_RULE) ? FillModeAlternate : FillModeWinding);
    }

    if (current_pen && (current_pen->GetStyle() != wxTRANSPARENT)) {
      wxGDrawPolygon(g, current_pen->GraphicsPen(AlignSmoothing(), user_scale_x, current_alpha),
		     pts, n);
    }

	DoneDC(dc);
    return;
  } else
    ReleaseGraphics(dc);

  if (StippleBrush()) {
    wxRegion *r;
    r = new wxRegion(this);
    r->SetPolygon(n, points, xoffset, yoffset, fillStyle);
    FillWithStipple(this, r, current_brush);
  }

  SetScaleMode(wxWX_SCALE, dc);

  cpoints = new POINT[n];
  for (i = 0; i < n; i++) {
    ShiftXY(points[i].x + xoffset, points[i].y + yoffset, &xoffset1, &yoffset1);
    cpoints[i].x = xoffset1;
    cpoints[i].y = yoffset1;
  }

  prev = SetPolyFillMode(dc, (fillStyle == wxODDEVEN_RULE) ? ALTERNATE : WINDING);

  if (StartBrush(dc, 1)) {
    (void)Polygon(dc, cpoints, n);
    DoneBrush(dc);
  }
  if (StartPen(dc)) {
    (void)Polygon(dc, cpoints, n);
    DonePen(dc);
  }

  SetPolyFillMode(dc, prev);

  DoneDC(dc);
}

void wxDC::DrawPath(wxPath *p, double xoffset, double yoffset,int fillStyle)
{
  HDC dc;
  double **ptss;
  int *lens, cnt, i, total_cnt, j, k;
  POINT *pts;
  int xoffset1;
  int yoffset1;
  int prev;

  dc = ThisDC();

  if (!dc) return;

  if (anti_alias) {
    GraphicsPath *gp;

    InitGraphics(dc);
    
    gp = wxGPathNew((fillStyle == wxODDEVEN_RULE) ? FillModeAlternate : FillModeWinding);

    if (AlignSmoothing()) {
      double pw;
      pw = GetPenSmoothingOffset();
      p->Install((long)gp, xoffset, yoffset,
		 device_origin_x + canvas_scroll_dx, 
		 device_origin_y + canvas_scroll_dy, 
		 user_scale_x, user_scale_y,
		 TRUE, pw, pw);
    } else {
      p->Install((long)gp, xoffset, yoffset,
		 0, 0, 1, 1, FALSE, 0, 0);
    }

    if (current_brush && (current_brush->GetStyle() != wxTRANSPARENT)) {
      wxGFillPath(g, current_brush->GraphicsBrush(current_alpha), gp);
    }

    if (current_pen && (current_pen->GetStyle() != wxTRANSPARENT)) {    
      wxGDrawPath(g, current_pen->GraphicsPen(AlignSmoothing(), user_scale_x, current_alpha), gp);
    }

    wxGPathRelease(gp);

    DoneDC(dc);
    return;
  } else
    ReleaseGraphics(dc);

  SetScaleMode(wxWX_SCALE, dc);

  if (clipping && clipping->Empty())
    return;

  cnt = p->ToPolygons(&lens, &ptss, user_scale_x, user_scale_y);

  if (!cnt)
    return;

  total_cnt = 0;
  for (i = 0; i < cnt; i++) {
    total_cnt += (lens[i] / 2);
  }
  
  pts = new WXGC_ATOMIC POINT[total_cnt];

  prev = SetPolyFillMode(dc, (fillStyle == wxODDEVEN_RULE) ? ALTERNATE : WINDING);

  /* Scaled version */
  for (i = 0, k = 0; i < cnt; i++) {
    for (j = 0; j < lens[i]; j += 2) {
      xoffset1 = MS_XLOG2DEV(ptss[i][j] + xoffset);
      yoffset1 = MS_YLOG2DEV(ptss[i][j+1] + yoffset);
      pts[k].x = xoffset1;
      pts[k].y = yoffset1;
      k++;
    }
  }
  
  /* Turn of GDI scale. We do it ourselves, so we can
     plot half points that scale up to full points. */
  SetScaleMode(wxWX_SCALE, dc);

  if (StartBrush(dc, 1)) {
    if (cnt == 1) {
      (void)Polygon(dc, pts, total_cnt);
    } else {
      HRGN rgn = 0, rgn1;

      for (i = 0, k = 0; i < cnt; i++) {
	j = (lens[i] / 2);
	rgn1 = CreatePolygonRgn(pts XFORM_OK_PLUS k, j, (fillStyle == wxODDEVEN_RULE) ? ALTERNATE : WINDING);
	if (rgn) {
	  /* Xoring implements the even-odd rule */
	  CombineRgn(rgn, rgn1, rgn, RGN_XOR);
	  DeleteObject(rgn1);
	} else {
	  rgn = rgn1;
	}
	k += j;
      }

      if (clipping) {
	HRGN crgn;
	crgn = clipping->GetRgn();
	if (crgn) {
	  CombineRgn(rgn, crgn, rgn, RGN_AND);
	}
      }

      SelectClipRgn(dc, rgn);
      
      (void)Rectangle(dc, 0, 0, 32000, 32000);

      DoClipping(dc);

      DeleteObject(rgn);
    }
    DoneBrush(dc);
  }
  if (StartPen(dc)) {
    for (i = 0, k = 0; i < cnt; i++) {
      j = (lens[i] / 2);
      if ((i + 1 == cnt) && p->IsOpen()) {
	(void)Polyline(dc, pts XFORM_OK_PLUS k, j);
      } else {
	(void)Polygon(dc, pts XFORM_OK_PLUS k, j);
      }
      k += j;
    }
    DonePen(dc);
  }

  SetPolyFillMode(dc, prev);

  DoneDC(dc);
}

void wxDC::DrawLines(int n, wxPoint points[], double xoffset, double yoffset)
{
  HDC dc;

  dc = ThisDC();

  if (!dc) return;

  if (anti_alias) {
    if (current_pen && (current_pen->GetStyle() != wxTRANSPARENT)) {
      PointF *pts;
      int i;
      
      InitGraphics(dc);

      pts = newPointFs(n);
      for (i = 0; i < n; i++) {
	double x, y;
	x = SmoothingXFormX(points[i].x + xoffset);
	y = SmoothingXFormY(points[i].y + yoffset);
	pts[i].X = x;
	pts[i].Y = y;
      }

      
      wxGDrawLines(g, current_pen->GraphicsPen(AlignSmoothing(), user_scale_x, current_alpha), pts, n);
    }

    DoneDC(dc);
    return;
  } else
    ReleaseGraphics(dc);

  SetScaleMode(wxWX_SCALE, dc);

  if (StartPen(dc)) {
    int xoffset1;
    int yoffset1;
    POINT *cpoints;
    int i;
    
    cpoints = new POINT[n];
    for (i = 0; i < n; i++) {
      ShiftXY(points[i].x + xoffset, points[i].y + yoffset, &xoffset1, &yoffset1);
      cpoints[i].x = xoffset1;
      cpoints[i].y = yoffset1;
    }
    
    (void)Polyline(dc, cpoints, n);

    DonePen(dc);
  }

  DoneDC(dc);  
}

void wxDC::DrawRectangle(double x, double y, double width, double height)
{
  int x1, y1, x2, y2, dd;
  HDC dc;

  dc = ThisDC();

  if (!dc) return;

  if (anti_alias) {
    InitGraphics(dc);

    if (current_brush && (current_brush->GetStyle() != wxTRANSPARENT)) {
      double xx, yy, ww, hh;

      xx = SmoothingXFormXB(x);
      yy = SmoothingXFormYB(y);
      ww = SmoothingXFormW(width, x);
      hh = SmoothingXFormH(height, y);

      wxGFillRectangle(g, current_brush->GraphicsBrush(current_alpha), xx, yy, ww, hh);
    }

    if (current_pen && (current_pen->GetStyle() != wxTRANSPARENT)) {
      double xx, yy, ww, hh;

      xx = SmoothingXFormX(x);
      yy = SmoothingXFormY(y);
      ww = SmoothingXFormWL(width, x);
      hh = SmoothingXFormHL(height, y);

      wxGDrawRectangle(g, current_pen->GraphicsPen(AlignSmoothing(), user_scale_x, current_alpha), xx, yy, ww, hh);
    }
    
    DoneDC(dc);
    return;
  } else
    ReleaseGraphics(dc);

  if (StippleBrush()) {
    wxRegion *r;
    r = new wxRegion(this);
    r->SetRectangle(x, y, width, height);
    FillWithStipple(this, r, current_brush);
  }

  SetScaleMode(wxWX_SCALE, dc);
  
  ShiftXY(x, y, &x1, &y1);
  ShiftXY(x + width, y + height, &x2, &y2);

  if (::GetGraphicsMode(dc) == GM_ADVANCED)
    dd = 0;
  else
    dd = 1;

  if (StartBrush(dc, 1)) {
    (void)Rectangle(dc, x1, y1, x2+dd, y2+dd);
    DoneBrush(dc);
  }
  if (StartPen(dc)) {
    (void)Rectangle(dc, x1, y1, x2-1+dd, y2-1+dd);
    DonePen(dc);
  }

  DoneDC(dc);
}

void wxDC::DrawRoundedRectangle(double x, double y, double width, double height, double radius)
{
  HDC dc;
  int x1, y1, x2, y2, r1, r2;

  dc = ThisDC();

  if (!dc) return;
  
  // A negative radius value is interpreted to mean
  // 'the proportion of the smallest X or Y dimension'
  if (radius < 0.0) {
    double smallest = 0.0;
    if (width < height)
      smallest = width;
    else
      smallest = height;
    radius = (double)(- radius * smallest);
  }

  if (anti_alias) {
    GraphicsPath *gp;

    InitGraphics(dc);

    if (current_brush && (current_brush->GetStyle() != wxTRANSPARENT)) {
      double xx, yy, ww, hh, rr, rr2;

      xx = SmoothingXFormXB(x);
      yy = SmoothingXFormYB(y);
      ww = SmoothingXFormW(width, x);
      hh = SmoothingXFormH(height, y);

      rr = SmoothingXFormW(radius, 0);
      rr2 = SmoothingXFormH(radius, 0);
      if (rr2 < rr)
	rr = rr2;
      
      gp = wxGPathNew(FillModeWinding);
      wxGPathAddArc(gp, xx, yy, rr * 2, rr * 2, 180, 90);
      wxGPathAddLine(gp, xx + rr, yy, xx + ww - rr, yy);
      wxGPathAddArc(gp, xx + ww - 2 * rr, yy, rr * 2, rr * 2, 270, 90);
      wxGPathAddLine(gp, xx + ww, yy + rr, xx + ww, yy + hh - rr);
      wxGPathAddArc(gp, xx + ww - 2 * rr, yy + hh - 2 * rr, 2 * rr, 2 * rr, 0, 90);
      wxGPathAddLine(gp, xx + ww - rr, yy + hh, xx + rr, yy + hh);
      wxGPathAddArc(gp, xx, yy + hh - 2 * rr, 2 * rr, 2 * rr, 90, 90);
      wxGPathCloseFigure(gp);
      wxGFillPath(g, current_brush->GraphicsBrush(current_alpha), gp);
      wxGPathRelease(gp);
    }

    if (current_pen && (current_pen->GetStyle() != wxTRANSPARENT)) {
      double xx, yy, ww, hh, rr, rr2;

      xx = SmoothingXFormX(x);
      yy = SmoothingXFormY(y);
      ww = SmoothingXFormWL(width, x);
      hh = SmoothingXFormHL(height, y);
      
      rr = SmoothingXFormWL(radius, 0);
      rr2 = SmoothingXFormHL(radius, 0);
      if (rr2 < rr)
	rr = rr2;

      gp = wxGPathNew(FillModeWinding);
      wxGPathAddArc(gp, xx, yy, rr * 2, rr * 2, 180, 90);
      wxGPathAddLine(gp, xx + rr, yy, xx + ww - rr, yy);
      wxGPathAddArc(gp, xx + ww - 2 * rr, yy, rr * 2, rr * 2, 270, 90);
      wxGPathAddLine(gp, xx + ww, yy + rr, xx + ww, yy + hh - rr);
      wxGPathAddArc(gp, xx + ww - 2 * rr, yy + hh - 2 * rr, 2 * rr, 2 * rr, 0, 90);
      wxGPathAddLine(gp, xx + ww - rr, yy + hh, xx + rr, yy + hh);
      wxGPathAddArc(gp, xx, yy + hh - 2 * rr, 2 * rr, 2 * rr, 90, 90);
      wxGPathCloseFigure(gp);
      wxGDrawPath(g, current_pen->GraphicsPen(AlignSmoothing(), user_scale_x, current_alpha), gp);
      wxGPathRelease(gp);
    }


    DoneDC(dc);
    return;
  } else
    ReleaseGraphics(dc);
  
  if (StippleBrush()) {
    wxRegion *r;
    r = new wxRegion(this);
    r->SetRoundedRectangle(x, y, width, height);
    FillWithStipple(this, r, current_brush);
  }

  SetScaleMode(wxWX_SCALE, dc);

  ShiftXY(x, y, &x1, &y1);
  ShiftXY(x + width, y + height, &x2, &y2);
  r1 = MS_XLOG2DEVREL(radius);
  r2 = MS_YLOG2DEVREL(radius);
  if (r2 < r1)
    r1 = r2;

  if (StartBrush(dc, 1)) {
    (void)RoundRect(dc, x1, y1, x2 + 1, y2 + 1, r1, r1);
    DoneBrush(dc);
  }
  if (StartPen(dc)) {
    (void)RoundRect(dc, x1, y1, x2, y2, r1, r1);
    DonePen(dc);
  }

  DoneDC(dc);
}

void wxDC::DrawEllipse(double x, double y, double width, double height)
{
  HDC dc;
  int x1, y1, x2, y2;

  if (anti_alias) {
    DrawArc(x, y, width, height, 0, 2 * wxPI);
    return;
  }

  dc = ThisDC();

  if (!dc) return;

  ReleaseGraphics(dc);

  if (StippleBrush()) {
    wxRegion *r;
    r = new wxRegion(this);
    r->SetEllipse(x, y, width, height);
    FillWithStipple(this, r, current_brush);
  }

  SetScaleMode(wxWX_SCALE, dc);

  ShiftXY(x, y, &x1, &y1);
  ShiftXY(x + width, y + height, &x2, &y2);

  if (StartBrush(dc, 1)) {
    (void)Ellipse(dc, x1, y1, x2 + 1, y2 + 1);
    DoneBrush(dc);
  }
  if (StartPen(dc)) {
    (void)Ellipse(dc, x1, y1, x2, y2);
    DonePen(dc);
  }

  DoneDC(dc);
}

void wxDC::SetFont(wxFont *the_font)
{
  HDC dc;

  dc = ThisDC(FALSE);
  if (!dc) return;

  ReleaseGraphics(dc);

  font = the_font;

  if (!the_font) {
    if (old_font)
      ::SelectObject(dc, old_font);
    old_font = NULL;
  }

  if (font) {
    HFONT cfont;
    cfont = font->BuildInternalFont(dc, screen_font);

    if (cfont) {
      HFONT f;
      f = (HFONT)::SelectObject(dc, cfont);
      if (!old_font)
	old_font = f;
    }
  }
  
  DoneDC(dc);
}

void wxDC::SetPen(wxPen *pen)
{
  if (pen != current_pen) {
    if (current_pen) current_pen->Lock(-1);
    current_pen = pen;
    if (current_pen) current_pen->Lock(1);
    
    if (pen)
      pen->ChangePen();
  }
}

void wxDC::SetBrush(wxBrush *brush)
{
  if (brush != current_brush) {
    if (current_brush) current_brush->Lock(-1);
    current_brush = brush;
    if (current_brush) current_brush->Lock(1);
    
    if (brush)
      brush->ChangeBrush();
  }
}

static int ucs4_strlen(const unsigned int *c)
{
  int i;
  
  for (i = 0; c[i]; i++) {
  }

  return i;
}

#define QUICK_UBUF_SIZE 1024
static wchar_t u_buf[QUICK_UBUF_SIZE];

static int symbol_map[] = { 0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 8704, 0, 8707, 0, 0, 8717,
			    0, 0, 8727, 0, 0, 8722, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    8773, 913, 914, 935, 916, 917, 934, 915,
			    919, 921, 977, 922, 923, 924, 925, 927,
			    928, 920, 929, 931, 932, 933, 962, 937,
			    926, 936, 918, 0, 8756, 0, 8869, 0,
			    0, 945, 946, 967, 948, 949, 966, 947,
			    951, 953, 981, 954, 955, 956, 957, 959,
			    960, 952, 961, 963, 964, 965, 982, 969,
			    958, 968, 950, 0, 0, 0, 8764, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 978, 8242, 8804, 8260, 8734, 402, 9827,
			    9830, 9829, 9824, 8596, 8592, 8593, 8594, 8595,
			    0, 177, 8243, 8805, 215, 8733, 8706, 8729,
			    247, 8800, 8801, 8776, 8230, 9168, /* 9135 */8212, 8629,
			    8501, 8465, 8476, 8472, 8855, 8853, 8709, 8745,
			    8746, 8835, 8839, 8836, 8834, 8838, 8712, 8713,
			    8736, 8711, 174, 169, 8482, 8719, 8730, 8901,
			    172, 8743, 8744, 8660, 8656, 8657, 8658, 8659,
			    9674, 9001, 174, 169, 8482, 8721, 9115, 9116,
			    9117, 9121, 9122, 9123, 9127, 9128, 9129, 9130,
			    8364, 9002, 8747, 8992, 9134, 8993, 9118, 9119,
			    9120, 9124, 9125, 9126, 9131, 9132, 9133, 0 };

wchar_t *convert_to_drawable_format(const char *text, int d, int ucs4, long *_ulen, Bool is_sym)
{
  int ulen, alloc_ulen;
  wchar_t *unicode;
  int theStrlen;

  theStrlen = *_ulen;
  if (theStrlen < 0) {
    if (ucs4) {
      theStrlen = ucs4_strlen((const unsigned int *)text XFORM_OK_PLUS d);
    } else {
      theStrlen = strlen(text XFORM_OK_PLUS d);
    }
  }

  if (ucs4) {
    int i, extra;
    unsigned int v;

    /* Count characters that fall outside UCS-2: */
    for (i = 0, extra = 0; i < theStrlen; i++) {
      if (((unsigned int *)text)[d+i] > 0xFFFF)
	extra++;
    }

    ulen = theStrlen + extra;
    alloc_ulen = ulen;
    if (alloc_ulen > QUICK_UBUF_SIZE)
      unicode = (wchar_t *)GC_malloc_atomic(sizeof(wchar_t) * alloc_ulen);
    else
      unicode = u_buf;
    
    /* UCS-4 -> UTF-16 conversion */
    for (i = 0, extra = 0; i < theStrlen; i++) {
      v = ((unsigned int *)text)[d+i];
      if (is_sym && (v < 256) && symbol_map[v])
	v = symbol_map[v];
      if (v > 0xFFFF) {
	v -= 0x10000;
	unicode[i+extra] = 0xD800 | ((v >> 10) & 0x3FF);
	extra++;
	unicode[i+extra] = 0xDC00 | (v & 0x3FF);
      } else
	unicode[i+extra] = v;
    }
  } else {
    /* UTF-8 -> UTF-16 conversion */
    ulen = scheme_utf8_decode((unsigned char *)text, d, 
			      theStrlen, NULL, 0, -1, 
			      NULL, 1 /*UTF-16*/, '?');
    alloc_ulen = ulen;
    if (alloc_ulen > QUICK_UBUF_SIZE)
      unicode = (wchar_t *)GC_malloc_atomic(sizeof(wchar_t) * alloc_ulen);
    else
      unicode = u_buf;
    ulen = scheme_utf8_decode((unsigned char *)text, d, theStrlen, 
			      (unsigned int *)unicode, 0, -1, 
			      NULL, 1 /*UTF-16*/, '?');
    if (is_sym) {
      int i, v;
      for (i = 0; i < ulen; i++) {
	v = ((wchar_t *)unicode)[i];
	if ((v < 256) && symbol_map[v]) {
	  v = symbol_map[v];
	  ((wchar_t *)unicode)[i] = v;
	}
      }
    }
  }
  
  *_ulen = ulen;
  return unicode;
}

static int substitute_font(wchar_t *ustring, int d, int alen, wxFont *font, HDC dc, Bool screen_font, double angle, int *reset)
{
  int v, i;

  if (*reset) {
    HFONT cfont;
    *reset = 0;
    cfont = font->BuildInternalFont(dc, screen_font, angle);
    if (cfont) {
      ::SelectObject(dc, cfont);
    }
  }

  v = ustring[d];
  if (!font->GlyphAvailableNow(v, dc, screen_font)) {
    wxFont *sub;
    sub = font->Substitute(v, dc, screen_font);
    if (sub) {
      HFONT cfont;
      int max_alen;

      /* Continue using this substituion as long as
	 glyphs are in this font and not in the original font. */
      max_alen = alen;
      for (i = 1; i < alen; i++) {
	v = ustring[d+i];
	if (font->GlyphAvailableNow(v, dc, screen_font)) {
	  max_alen = i;
	  break;
	}
      }

      cfont = sub->BuildInternalFont(dc, screen_font, angle);
      if (cfont) {
	font = sub;
	::SelectObject(dc, cfont);
	*reset = 1;

	for (i = 1; i < max_alen; i++) {
	  v = ustring[d+i];
	  if (!font->GlyphAvailableNow(v, dc, screen_font)) {
	    max_alen = i;
	    break;
	  }
	}
	return max_alen;
      }
    }

    /* Continue for as long as glyphs are in this font */
    for (i = 1; i < alen; i++) {
      v = ustring[d+i];
      if (!font->GlyphAvailableNow(v, dc, screen_font)) {
	return i;
      }
    }
  }

  return alen;
}

static Scheme_Hash_Table *wxSizeHashTable(wxFont *font, Bool screen_font, Bool combine, double sx, double sy)
{
  if (screen_font && !combine) {
    Scheme_Hash_Table *ht;
    double a;
    Scheme_Object *szht;

    ((wxSizeKey *)SCHEME_BYTE_STR_VAL(theSizeKey))->sx = sx;
    ((wxSizeKey *)SCHEME_BYTE_STR_VAL(theSizeKey))->sy = sy;
    a = font->GetRotation();
    ((wxSizeKey *)SCHEME_BYTE_STR_VAL(theSizeKey))->angle = a;

    if (font->size_cache) {
      ht = (Scheme_Hash_Table *)font->size_cache;
    } else {
      ht = scheme_make_hash_table_equal();
      font->size_cache = ht;
    }

    szht = scheme_hash_get(ht, theSizeKey);
    if (!szht) {
      Scheme_Object *k2;
      k2 = scheme_alloc_byte_string(sizeof(wxSizeKey), 0);
      memcpy(SCHEME_BYTE_STR_VAL(k2), SCHEME_BYTE_STR_VAL(theSizeKey), sizeof(wxSizeKey));
      szht = (Scheme_Object *)scheme_make_hash_table(SCHEME_hash_ptr);
      scheme_hash_set(ht, k2, szht);
    }
    return (Scheme_Hash_Table *)szht;
  }

  return NULL;
}

static void wxTextSize(HDC dc, Scheme_Hash_Table *ht, wchar_t *ustring, int d, int alen, double *ow, double *oh)
{
  /* Gets the text size, caching the result in font when alen == 1 */ 

  if ((alen == 1) && is_nt()) {
    double *sz;

    if (ht) {
      sz = (double *)scheme_hash_get(ht, scheme_make_integer(ustring[d]));
    } else {
      sz = NULL;
    }

    if (sz) {
      *ow = sz[0];
      *oh = sz[1];
    } else {
      ABCFLOAT cw;
      SIZE sizeRect;
      GetCharABCWidthsFloatW(dc, ustring[d], ustring[d], &cw);
      *ow = (cw.abcfA + cw.abcfB + cw.abcfC);
      GetTextExtentPointW(dc, ustring XFORM_OK_PLUS d, alen, &sizeRect);
      *oh = (double)sizeRect.cy;

      if (ht) {
	sz = (double *)scheme_malloc_atomic(sizeof(double) * 2);
	sz[0] = *ow;
	sz[1] = *oh;
	scheme_hash_set(ht, scheme_make_integer(ustring[d]), (Scheme_Object *)sz);
      }
    }
  } else {
    SIZE sizeRect;
    GetTextExtentPointW(dc, ustring XFORM_OK_PLUS d, alen, &sizeRect);
    *ow = (double)sizeRect.cx;
    *oh = (double)sizeRect.cy;
  }
}

void wxDC::DrawText(const char *text, double x, double y, Bool combine, Bool ucs4, int d, double angle)
{
  HDC dc;
  DWORD old_background;
  double w, h, ws, hs, ow, oh;
  wchar_t *ustring;
  long len = -1, alen;
  double oox, ooy;
  int fam, reset = 0;
  wxFont *theFont;
  Scheme_Hash_Table *ht;

  dc = ThisDC();

  if (!dc) return;

  ReleaseGraphics(dc);
  
  if (font) {
    HFONT cfont;
    cfont = font->BuildInternalFont(dc, screen_font, angle);
    if (cfont) {
      HFONT f;
      f = (HFONT)::SelectObject(dc, cfont);
      if (!old_font)
        old_font = f;
    }
    fam = font->GetFamily();
  } else {
    fam = wxDEFAULT;
  }
  
  theFont = font;
  if (theFont->redirect)
    theFont = theFont->redirect;
  ht = wxSizeHashTable(theFont, screen_font, combine, MS_XLOG2DEVREL(1), MS_YLOG2DEVREL(1));

  ustring = convert_to_drawable_format(text, d, ucs4, &len, fam == wxSYMBOL);

  if (current_text_foreground->Ok())
    SetTextColor(dc, current_text_foreground->pixel);
  if (current_text_background->Ok()) {
    old_background = SetBkColor(dc, current_text_background->pixel);
  }
  
  SetBkMode(dc, (((current_bk_mode == wxTRANSPARENT) 
		  || (angle != 0.0))
		 ? TRANSPARENT
		 : OPAQUE));
  SetRop(dc, wxSOLID);
  SetScaleMode(wxWINDOWS_SCALE, dc);

  if (current_alpha != 1.0) {
    /* Approximate alpha blending... */
    UINT c;
    int r, gr, b;
    
    if (current_text_foreground->Ok())
      c = current_text_foreground->pixel;
    else
      c = RGB(0, 0, 0);

    r = 255 - (int)((255 - GetRValue(c)) * current_alpha);
    gr = 255 - (int)((255 - GetGValue(c)) * current_alpha);
    b = 255 - (int)((255 - GetBValue(c)) * current_alpha);
    
    SetTextColor(dc, RGB(r, gr, b));

    SetROP2(dc, R2_MASKPEN);
  }
  
  w = 0;
  h = 0;
  d = 0;

  oox = device_origin_x;
  ooy = device_origin_y;

  ws = cos(angle);
  hs = -sin(angle);

  while (len) {
    if (combine)
      alen = len;
    else
      alen = 1;
    
    alen = substitute_font(ustring, d, alen, font, dc, screen_font, angle, &reset);
    
    SetDeviceOrigin(((x + w)*user_scale_x) + oox + canvas_scroll_dx, 
		    ((y + h)*user_scale_y) + ooy + canvas_scroll_dy);

    (void)TextOutW(dc, 0, 0, ustring XFORM_OK_PLUS d, alen);

    if (alen < len) {
      wxTextSize(dc, ht, ustring, d, alen, &ow, &oh);
      w += ow * ws;
      h += ow * hs;
    }

    len -= alen;
    d += alen;
  }

  SetDeviceOrigin(oox, ooy);

  if (current_alpha != 1.0) {
    SetROP2(dc, R2_COPYPEN);
  }

  if (current_text_background->Ok())
    (void)SetBkColor(dc, old_background);

  DoneDC(dc);

  if (reset)
    SetFont(font);
}

void wxDC::SetBackground(wxColour *c)
{
  HDC dc;
  COLORREF new_color;

  current_background_color->CopyFrom(c);

#if 0
  if (canvas) {
    wxCanvasWnd *wnd = (wxCanvasWnd *)canvas->handle;
    wxBrush *br = wxTheBrushList->FindOrCreateBrush(c, wxSOLID);
    wnd->SetBackgroundBrush(br->cbrush, FALSE);
    wnd->background_colour = RGB(c->Red(), c->Green(), c->Blue());
    wnd->background_transparent = FALSE;
  }
#endif
  
  dc = ThisDC(FALSE);

  new_color = c->pixel;
  if (new_color != cur_bk || dc != cur_dc) {
    (void)SetBkColor(dc, new_color);
    cur_bk = new_color;
    cur_dc = dc;
  }

  DoneDC(dc);
}

void wxDC::SetBackgroundMode(int mode)
{
  current_bk_mode = mode;
}

void wxDC::SetRop(HDC dc, int style)
{
  int c_rop;

  if (!dc) return;

  if (style == cur_rop)
    return;
  cur_rop = style;
  
  switch (style) {
  case wxXOR_DOT:
  case wxXOR_SHORT_DASH:
  case wxXOR_LONG_DASH:
  case wxXOR_DOT_DASH:
  case wxXOR: 
    c_rop = R2_NOTXORPEN;
    break;
  case wxCOLOR: 
    if (Colour)
      c_rop = R2_MERGEPENNOT;
    else
      c_rop = R2_NOTXORPEN;
    break;
  default:
    c_rop = R2_COPYPEN;
    break;
  }
  SetROP2(dc, c_rop);
}

static HBRUSH hilite_brush;

int wxDC::StartBrush(HDC dc, Bool no_stipple)
{
  if (current_brush && current_brush->GetStyle() !=wxTRANSPARENT) {
    int ps;
    ps = current_brush->GetStyle();
    if (Colour && (ps == wxCOLOR)) {
      if (!hilite_brush) {
	hilite_brush = CreateSolidBrush(GetSysColor(COLOR_HIGHLIGHT));
	RegisterGDIObject(hilite_brush);
      }
      SelectObject(dc, hilite_brush);
      SetRop(dc, wxCOLOR);
    } else {
      if (no_stipple) {
	wxBitmap *bm;
	bm = current_brush->GetStipple();
	if (bm && bm->Ok())
	  return FALSE;
      }
      current_brush->SelectBrush(dc);
      SetRop(dc, ps);
    }
    return TRUE;
  } else
    return FALSE;
}

void wxDC::DoneBrush(HDC dc)
{
  ::SelectObject(dc, null_brush);
}

static HPEN hilite_pens[256];

int wxDC::StartPen(HDC dc)
{
  if (current_pen && (current_pen->GetStyle() != wxTRANSPARENT)) {
    int ps;
    ps = current_pen->GetStyle();
    if (Colour && (ps == wxCOLOR)) {
      int size;
      size = current_pen->GetWidth();
      if (scaling_mode == wxWX_SCALE)
	size = MS_XLOG2DEV(size);
      if (!hilite_pens[size]) {
	HPEN p;
	p = CreatePen(PS_SOLID, size, GetSysColor(COLOR_HIGHLIGHT));
	hilite_pens[size] = p;
	RegisterGDIObject(p);
      }
      SelectObject(dc, hilite_pens[size]);
      SetRop(dc, wxCOLOR);
    } else {
      current_pen->SelectPen(dc, (scaling_mode == wxWX_SCALE) ? (user_scale_x*logical_scale_x) : 1.0);
      SetRop(dc, ps);
    }
    return TRUE;
  } else
    return FALSE;
}

void wxDC::DonePen(HDC dc)
{
  ::SelectObject(dc, null_pen);
}

wxBitmap *wxDC::StippleBrush()
{
  if (current_brush) {
    wxBitmap *bm;
    bm = current_brush->GetStipple();
    if (bm && bm->Ok())
      return bm;
  }
  return NULL;
}

Bool wxDC::StartDoc(char *message)
{
  Bool flag = FALSE;

  DOCINFO docinfo;

  if (!wxSubType(__type, wxTYPE_DC_PRINTER))
    return TRUE;
    
  docinfo.cbSize = sizeof(DOCINFO);
  docinfo.lpszDocName = message;
  docinfo.lpszOutput = filename;
  docinfo.lpszDatatype = NULL;
  docinfo.fwType = 0;
  if (cdc) {
    int res;
    res = ::StartDoc(cdc, &docinfo);
    flag = (SP_ERROR != res);
  } else
    flag = FALSE;

  return flag;
}

void wxDC::EndDoc(void)
{
  if (!wxSubType(__type, wxTYPE_DC_PRINTER))
    return;
  if (cdc) {
    ReleaseGraphics(cdc);
    ::EndDoc(cdc);
  }
}

void wxDC::StartPage(void)
{
  if (!wxSubType(__type, wxTYPE_DC_PRINTER))
    return;
  if (cdc) {
    ::StartPage(cdc);
    ResetMapMode(cdc);
  }
}

void wxDC::EndPage(void)
{
  if (!wxSubType(__type, wxTYPE_DC_PRINTER))
    return;
  if (cdc) {
    ReleaseGraphics(cdc);
    ::EndPage(cdc);
  }
}

double wxDC::GetCharHeight(void)
{
  TEXTMETRIC lpTextMetric;
  HDC dc;

  dc = ThisDC(FALSE);

  if (!dc) return 10;

  GetTextMetrics(dc, &lpTextMetric);

  DoneDC(dc);

  return (double)lpTextMetric.tmHeight;
}

double wxDC::GetCharWidth(void)
{
  TEXTMETRIC lpTextMetric;
  HDC dc;

  dc = ThisDC(FALSE);

  if (!dc) return 5;

  GetTextMetrics(dc, &lpTextMetric);

  DoneDC(dc);

  return (double)lpTextMetric.tmAveCharWidth;
}

void wxDC::GetTextExtent(const char *string, double *x, double *y,
                         double *descent, double *topSpace, 
			 wxFont *theFont, Bool combine, Bool ucs4, int d, int slen)
{
  wxFont *oldFont = NULL;
  HDC dc;
  TEXTMETRIC tm;
  long len = slen, alen;
  double tx, ty, ow, oh;
  wchar_t *ustring;
  int once = 1, fam, reset = 0;
  Scheme_Hash_Table *ht;

  if (theFont) {
    oldFont = font;
    SetFont(theFont);
  } else {
    theFont = font;
    SetFont(font);
  }

  if (theFont->redirect)
    theFont = theFont->redirect;
  ht = wxSizeHashTable(theFont, screen_font, combine, MS_XLOG2DEVREL(1), MS_YLOG2DEVREL(1));

  fam = theFont->GetFamily();

  dc = ThisDC(FALSE);

  ReleaseGraphics(dc);

  if (!dc) {
    *x = 5;
    *y = 10;
    if (descent) *descent = 0;
    if (topSpace) *topSpace= 0;
    return;
  }

  ustring = convert_to_drawable_format(string, d, ucs4, &len, fam == wxSYMBOL);
  
  SetScaleMode(wxWINDOWS_SCALE, dc);  

  d = 0;
  tx = 0;
  ty = 0;
  
  while (len || once) {
    if (!len)
      alen = 0;
    else if (combine)
      alen = len;
    else
      alen = 1;

    alen = substitute_font(ustring, d, alen, theFont, dc, screen_font, 0.0, &reset);

    wxTextSize(dc, ht, ustring, d, alen, &ow, &oh);

    tx += ow;
    if (oh > ty)
      ty = oh;

    len -= alen;
    d += alen;
    once = 0;
  }

  if (descent || topSpace || (!len && !ty))
    GetTextMetrics(dc, &tm);

  DoneDC(dc);

  *x = (double)tx;
  if (!len && !ty) {
    *y = (double)tm.tmHeight;
  } else {
    *y = (double)ty;
  }
  if (descent) *descent = (double)tm.tmDescent;
  if (topSpace) *topSpace = (double)tm.tmInternalLeading;
  
  if (oldFont)
    SetFont(oldFont);
  else if (reset)
    SetFont(font);
}

void wxDC::ResetMapMode(HDC given_dc)
{
  double sx, sy, ox, oy, lx, ly;
  HDC dc;

  if (given_dc)
    dc = given_dc;
  else {
    dc = ThisDC(FALSE);
    
    if (!dc) return;
  }

  switch (mapping_mode) {
  case wxPOINTS_MAP:
    {
      double pixelsX;
      double pixelsY;
  
      pixelsX = GetDeviceCaps(dc, LOGPIXELSX) / 72.0;
      pixelsY = GetDeviceCaps(dc, LOGPIXELSY) / 72.0;
	
      if (!pixelsX || !pixelsY) {
	/* Guess 300 dpi. */
	pixelsX = 300.0 / 72.0;
	pixelsY = 300.0 / 72.0;
      }
	
      logical_scale_x = pixelsX;
      logical_scale_y = pixelsY;
    }
    break;
  case wxPIXELS_MAP:
  default:
    {
      logical_scale_x = 1.0;
      logical_scale_y = 1.0;
      break;
    }
  }

  if (scaling_mode == wxWX_SCALE) {
    /* disable user xform */
    sx = 1.0;
    sy = 1.0;
    lx = 1.0;
    ly = 1.0;
    ox = 0.0;
    oy = 0.0;
  } else {
    sx = user_scale_x;
    sy = user_scale_y;
    lx = logical_scale_x;
    ly = logical_scale_y;
    ox = device_origin_x;
    oy = device_origin_y;
  }

  if (::GetMapMode(dc) != MM_ANISOTROPIC)
    ::SetMapMode(dc, MM_ANISOTROPIC);

  if ((__type != wxTYPE_DC_PRINTER)
      && (::SetGraphicsMode(dc, GM_ADVANCED) != 0)) {
    /* Note: logical scale is used only for printers */
    XFORM xform;
    xform.eM11 = (FLOAT)sx;
    xform.eM21 = 0;
    xform.eM12 = 0;
    xform.eM22 = (FLOAT)sy;
    xform.eDx = (FLOAT)ox;
    xform.eDy = (FLOAT)oy;
    ::SetWorldTransform(dc, &xform);
  } else {
    ::SetViewportExtEx(dc, 
		       (int)floor(1000*lx*sx), 
		       (int)floor(1000*ly*sy),
		       NULL);
    ::SetWindowExtEx(dc, 1000, 1000, NULL);
    ::SetViewportOrgEx(dc, (int)floor(ox*lx), (int)floor(oy*ly), NULL);
    ::SetWindowOrgEx(dc, (int)0, (int)0, NULL);
  }

  if (!given_dc)
    DoneDC(dc);
}

void wxDC::SetMapMode(int m, HDC given_dc)
{
  if (m != mapping_mode) {
    mapping_mode = m;
    ResetMapMode(given_dc);
  }
}

void wxDC::SetScaleMode(int m, HDC given_dc)
{
  if (m != scaling_mode) {
    scaling_mode = m;
    ResetMapMode(given_dc);
  }
}

void wxDC::SetUserScale(double x, double y)
{
  user_scale_x = x;
  user_scale_y = y;

  ReleaseGraphics();
  if (scaling_mode != wxWX_SCALE)
    ResetMapMode();
}

void wxDC::SetDeviceOrigin(double x, double y)
{
  device_origin_x = x;
  device_origin_y = y;

  ReleaseGraphics();
  if (scaling_mode != wxWX_SCALE)
    ResetMapMode();
}

double wxDC::DeviceToLogicalX(int x)
{
  return (double)MS_XDEV2LOG(x);
}

double wxDC::DeviceToLogicalXRel(int x)
{
  return (double)MS_XDEV2LOGREL(x);
}

double wxDC::UnscrolledDeviceToLogicalX(int x)
{
  return (double)MS_XUDEV2LOG(x);
}

double wxDC::DeviceToLogicalY(int y)
{
  return (double)MS_YDEV2LOG(y);
}

double wxDC::DeviceToLogicalYRel(int y)
{
  return (double)MS_YDEV2LOGREL(y);
}

double wxDC::UnscrolledDeviceToLogicalY(int y)
{
  return (double)MS_YUDEV2LOG(y);
}

int wxDC::LogicalToDeviceX(double x)
{
  return MS_XLOG2DEV(x);
}

int wxDC::LogicalToDeviceXRel(double x)
{
  return MS_XLOG2DEVREL(x);
}

int wxDC::LogicalToUnscrolledDeviceX(double x)
{
  return MS_XLOG2UDEV(x);
}

int wxDC::LogicalToDeviceY(double y)
{
  return MS_YLOG2DEV(y);
}

int wxDC::LogicalToDeviceYRel(double y)
{
  return MS_YLOG2DEVREL(y);
}

int wxDC::LogicalToUnscrolledDeviceY(double y)
{
  return MS_YLOG2UDEV(y);
}

double wxDC::FLogicalToDeviceX(double x)
{
  return MS_XLOG2DEV(x);
}

double wxDC::FLogicalToDeviceXRel(double x)
{
  return MS_XLOG2DEVREL(x);
}

double wxDC::FLogicalToUnscrolledDeviceX(double x)
{
  return MS_XLOG2UDEV(x);
}

double wxDC::FLogicalToDeviceY(double y)
{
  return MS_YLOG2DEV(y);
}

double wxDC::FLogicalToDeviceYRel(double y)
{
  return MS_YLOG2DEVREL(y);
}

double wxDC::FLogicalToUnscrolledDeviceY(double y)
{
  return MS_YLOG2UDEV(y);
}

#define wxKEEPDEST (DWORD)0x00AA0029

typedef BOOL (WINAPI *wxALPHA_BLEND)(HDC,int,int,int,int,HDC,int,int,int,int,BLENDFUNCTION);
static wxALPHA_BLEND wxAlphaBlend;
static int tried_ab = 0;
#ifndef AC_SRC_ALPHA
# define AC_SRC_ALPHA 0x01
#endif

Bool wxDC::Blit(double xdest, double ydest, double width, double height,
                wxBitmap *source, double xsrc, double ysrc, int rop,
		wxColour *c, wxBitmap *mask)
{
  int xdest1, ydest1, xsrc1, ysrc1, iw, ih, diw, dih;
  HDC dc, dc_src, invented_dc, mdc = NULL;
  wxMemoryDC *sel, *msel = NULL, *invented_memdc = NULL;
  wxBitmap *invented = NULL;
  Bool success = 1, invented_col = 0, use_alpha = 0;
  DWORD op = 0;

  dc = ThisDC();

  if (!dc) return FALSE;

  ReleaseGraphics(dc);

  /* Use our own scaling so that we don't lose precision
     in terms of the starting pixel and the width. Also,
     turning off scaling ensures that everything works if
     `source' is selected into this DC. */
  SetScaleMode(wxWX_SCALE, dc);

  /* Compute scaling and offset before adjusting source, in case the
     source is the dest. */
  ShiftXY(xdest, ydest, &xdest1, &ydest1);
  ShiftXY(xdest + width, ydest + height, &diw, &dih);
  diw -= xdest1;
  dih -= ydest1;
  /* diw and dih are dest width/height */

  xsrc1 = (int)floor(xsrc);
  ysrc1 = (int)floor(ysrc);

  /* Source w/h */
  iw = (int)floor(xsrc + width) - xsrc1;
  ih = (int)floor(ysrc + height) - ysrc1;
 
  if (!blit_dc) {
    wxREGGLOB(blit_dc);
    blit_dc = new wxMemoryDC(1);
  }

  sel = (wxMemoryDC *)source->selectedInto;
  if (sel) {
    sel->SetScaleMode(wxWX_SCALE);
    dc_src = sel->ThisDC(FALSE);
  } else {
    blit_dc->SelectObject(source);
    dc_src = blit_dc->ThisDC(FALSE);
  }

  if (!dc_src) {
    DoneDC(dc);
    return FALSE;
  }

  if (mask && ((mask->GetDepth() > 1) || !is_nt() || (iw != diw) || (ih != dih))) {
    /* Either:
        * No MaskBlt in 95/98/Me, or we need stretching that MaskBlt
          doesn't support, so we invent a bitmap like the source, but
          with white where the mask has white.
	* When AlphaBlend is available, we want to create a bitmap
          with alphas in it. 
    */
    int mono_src;

    if (mask == source) {
      /* This is ok. Just use dc_src as mdc. */
      mdc = dc_src;
    } else {
      msel = (wxMemoryDC *)mask->selectedInto;
      if (msel) {
	msel->SetScaleMode(wxWX_SCALE);
	mdc = msel->ThisDC(FALSE);
      } else {
	if (!blit_mdc) {
	  wxREGGLOB(blit_mdc);
	  blit_mdc = new wxMemoryDC(1);
	}
	
	blit_mdc->SelectObject(mask);
	mdc = blit_mdc->ThisDC(FALSE);
      }
    }

    mono_src = (source->GetDepth() == 1);

    if (mask->mask_cache
	&& (mask->mask_cache == source->mask_cache)
	&& (mask->cache_xsrc1 == xsrc1)
	&& (mask->cache_ysrc1 == ysrc1)
	&& (mask->cache_iw == iw)
	&& (mask->cache_ih == ih)
	&& (mask != selected_bitmap)
	&& (source != selected_bitmap)
	&& (source->mask_cache->selected_bitmap)) {
      invented = NULL;
      invented_memdc = source->mask_cache;
      invented_dc = invented_memdc->ThisDC();
      if (mask->GetDepth() > 1) {
	if (wxAlphaBlend) {
	  invented = invented_memdc->selected_bitmap;
	  if (invented->IsDIB())
	    use_alpha = 1;
	  invented = NULL;
	}
      }
    } else {
      wxBitmap *orig_mask = mask;
      invented = new wxBitmap(iw, ih, mono_src);
      if (invented->Ok()) {
	GC_CAN_IGNORE void *pBits = NULL; /* set with use_alpha... */

	if (mask->GetDepth() > 1) {
	  if (!tried_ab) {
	    HMODULE mod;
	    mod = LoadLibrary("Msimg32.dll");
	    if (mod)
	      wxAlphaBlend = (wxALPHA_BLEND)GetProcAddress(mod, "AlphaBlend");
	    tried_ab = 1;
	  }
	  if (wxAlphaBlend)
	    use_alpha = 1;
	  /* Otherwise, no AlphaBlend. The result is somewhat unpredictable,
	     but somewhat as intended --- especially if we happend
	     to be drawing onto white. :) */
	}

	if (use_alpha) {
	  pBits = invented->ChangeToDIBSection();
	  if (!pBits) {
	    use_alpha = 0;
	    /* half-failure... act like AlphaBlend isn't there */
	  }
	}

	invented_memdc = new wxMemoryDC();
	invented_memdc->SelectObject(invented);

	if (invented_memdc->Ok()) {
	  invented_dc = invented_memdc->ThisDC();

	  /* Copy original src image here: */
	  BitBlt(invented_dc, 0, 0,
		 iw, ih,
		 dc_src, xsrc1, ysrc1,
		 SRCCOPY);

	  if (use_alpha) {
	    /* "Pre-compute" alpha in the invented DC */
	    GC_CAN_IGNORE BYTE *pPixel;
	    COLORREF mcol;
	    int i, j, gray;

	    GdiFlush();
	    for (j = 0; j < ih; j++) {
	      pPixel = (BYTE *) pBits + iw * 4 * (ih - j - 1);
	      for (i = 0; i < iw; i++) {
		mcol = ::GetPixel(mdc, i + xsrc1, j + ysrc1);
		gray = ((int)GetRValue(mcol)
			+ (int)GetGValue(mcol)
			+ (int)GetBValue(mcol)) / 3;
		pPixel[0] = pPixel[0] * (255 - gray) / 255; 
		pPixel[1] = pPixel[1] * (255 - gray) / 255; 
		pPixel[2] = pPixel[2] * (255 - gray) / 255; 
		pPixel[3] = (255 - gray);
	      
		pPixel += 4;
	      }
	    }
	  } else {
	    /* Continues below */
	  }
	} else {
	  /* Failed (Rest of failure handling below since !invented_memdc) */
	  invented_memdc->SelectObject(NULL);
	  DELETE_OBJ invented_memdc;
	  invented_memdc = NULL;
	  DELETE_OBJ invented;
	}
      }

      if (!invented_memdc) {
	/* Failed */
	if (msel) {
	  msel->DoneDC(mdc);
	} else {
	  blit_mdc->DoneDC(mdc);
	  blit_mdc->SelectObject(NULL);
	}
	DoneDC(dc);
	if (sel) {
	  sel->DoneDC(dc_src);
	} else {
	  blit_dc->DoneDC(dc_src);
	  blit_dc->SelectObject(NULL);
	}
	return 0;
      }

      if ((orig_mask != selected_bitmap)
	  && (source != selected_bitmap)) {
	orig_mask->ReleaseCachedMask();
	source->ReleaseCachedMask();
	orig_mask->mask_cache = invented_memdc;
	source->mask_cache = invented_memdc;

	orig_mask->cache_xsrc1 = xsrc1;
	orig_mask->cache_ysrc1 = ysrc1;
	orig_mask->cache_iw = iw;
	orig_mask->cache_ih = ih;
	source->cache_xsrc1 = xsrc1;
	source->cache_ysrc1 = ysrc1;
	source->cache_iw = iw;
	source->cache_ih = ih;

	if (source == orig_mask)
	  invented_memdc->refcount = 1;
	else
	  invented_memdc->refcount = 2;

	invented = NULL; /* indicates that we cached invented */
      }
    }
  }

  if (invented_memdc && !use_alpha) {
    /* Want white where mask was white,
       src otherwise: */
    BitBlt(invented_dc, 0, 0,
	   iw, ih,
	   mdc, xsrc1, ysrc1,
	   SRCPAINT /* DSo */);

    /* Ignore the mask and... */
    mask = NULL;
    if (source->GetDepth() == 1) {
      /* Mono source: Now use invented_dc instead of src_dc,
	 and it all works out. */
      xsrc1 = 0;
      xsrc1 = 0;
    } else {
      /* Paint on dest using mask, then "and" invented image
	 with dest. */
      invented_col = 1;
    }
  }

  if (use_alpha) {
    BLENDFUNCTION bf;

    bf.BlendOp = AC_SRC_OVER;
    bf.BlendFlags = 0;
    bf.SourceConstantAlpha = 0xff;
    bf.AlphaFormat = AC_SRC_ALPHA;
    
    success = wxAlphaBlend(dc, 
			   xdest1, ydest1, diw, dih,
			   invented_dc,
			   0, 0, iw, ih,
			   bf);
  } else {
    SetTextColor(dc, 0);     /* 0 = black */
    if ((source->GetDepth() == 1) || invented_col)  {
      if ((rop == wxSOLID) || invented_col) {
	/* White pixels in the src aren't supposed to count.
	   First, paint black everywhere where the bitmap has black.
	   Then, below, "or" over the destination with a coloring of the image.
	   
	   If we're painting a color src through an invented image,
	   use the mask instead of src for this first step. */
	SetTextColor(dc, wxBLACK->pixel);
	if (mask) {
	  success = MaskBlt(dc, xdest1, ydest1, 
			    iw, ih,
			    dc_src, xsrc1, ysrc1,
			    mask->ms_bitmap, xsrc1, ysrc1,
			    MAKEROP4(wxKEEPDEST, MERGEPAINT));
	} else {
	  success = StretchBlt(dc, xdest1, ydest1, 
			       diw, dih,
			       (invented_col
				? mdc 
				: (invented_memdc 
				   ? invented_dc 
				   : dc_src)), 
			       xsrc1, ysrc1, iw, ih,
			       MERGEPAINT);
	  if (invented_col) {
	    /* zero src offset for second step, which uses the invented_dc */
	    xsrc1 = ysrc1 = 0;
	  }
	}
	op = SRCAND;
      } else {
	/* Straightforward copy (including white pixels) */
	op = ((rop == wxXOR) 
	      ? 0x00990066 /* => DSnx */
	      : SRCCOPY);  /* opaque */
      }
      SetTextColor(dc, c ? c->pixel : wxBLACK->pixel);
    } else {
      op = SRCCOPY;
      SetTextColor(dc, wxBLACK->pixel);
    }
    
    if (op && success) {
      if (mask) {
	success = MaskBlt(dc, xdest1, ydest1, 
			  iw, ih,
			  dc_src, xsrc1, ysrc1,
			  mask->ms_bitmap, xsrc1, ysrc1,
			  MAKEROP4(wxKEEPDEST, op));
      } else {
	success = StretchBlt(dc, xdest1, ydest1, 
			     diw, dih,
			     invented_memdc ? invented_dc : dc_src,
			     xsrc1, ysrc1, iw, ih,
			     op);
      }
    }
  }

  DoneDC(dc);
  if (sel) {
    sel->DoneDC(dc_src);
  } else {
    blit_dc->DoneDC(dc_src);
    blit_dc->SelectObject(NULL);
  }
  if (mdc && (mdc != dc_src)) {
    if (msel) {
      msel->DoneDC(mdc);
    } else {
      blit_mdc->DoneDC(mdc);
      blit_mdc->SelectObject(NULL);
    }
  }
  if (invented_memdc) {
    /* If invented, then cached, so don't delete here. */
    if (invented) {
      invented_memdc->DoneDC(invented_dc);
      invented_memdc->SelectObject(NULL);
      DELETE_OBJ invented_memdc;
      DELETE_OBJ invented;
    }
  }

  return success;
}

void wxDC::GetSize(double *width, double *height)
{
  HDC dc;
  int w, h;

  dc = ThisDC(FALSE);

  if (!dc) {
    *width = *height = 0;
    return;
  }

  w = ::GetDeviceCaps(dc,HORZRES);
  h = ::GetDeviceCaps(dc,VERTRES);
  *width = (double)MS_XDEV2LOGREL(w);
  *height = (double)MS_YDEV2LOGREL(h);

  DoneDC(dc);
}

void wxDC::GetSizeMM(double *width, double *height)
{
  HDC dc;
  int w, h;

  dc = ThisDC(FALSE);

  if (!dc) {
    *width = *height = 0;
    return;
  }

  w=::GetDeviceCaps(dc,HORZSIZE);
  h=::GetDeviceCaps(dc,VERTSIZE);
  *width = (double)w;
  *height = (double)h;

  DoneDC(dc);
}

void wxDC::SetAlpha(double a)
{
  wxbDC::SetAlpha(a);
}

wxCanvasDC::wxCanvasDC(void)
{
  __type = wxTYPE_DC_CANVAS;
  device = wxDEVICE_WINDOWS;
}

wxCanvasDC::wxCanvasDC(wxCanvas *the_canvas) : wxbCanvasDC()
{
  __type = wxTYPE_DC_CANVAS;
  canvas = the_canvas;
  WXGC_IGNORE(this, canvas);
  device = wxDEVICE_WINDOWS;
  SetBrush(wxWHITE_BRUSH);
  SetPen(wxBLACK_PEN);

  if (canvas) {
    cdc = ((wxWnd *)canvas->handle)->GetHDC();
  }
}

wxCanvasDC::~wxCanvasDC(void)
{
  if (wx_gl) {
    wx_gl->Reset(NULL, 0, 0);
    wx_gl = NULL;
  }

  if (canvas) {
    ((wxWnd *)canvas->handle)->ReleaseHDC();
    canvas = NULL;
  }
}

void wxCanvasDC::GetSize(double *width, double *height)
{
  int ww, hh;

  canvas->GetVirtualSize(&ww, &hh);
  *width = ww;
  *height = hh;
}

void wxCanvasDC::TryColour(wxColour *src, wxColour *dest)
{
  COLORREF result, col;
  HDC dc;
  int r, gr, b;

  dc = ThisDC(FALSE);
  if (!dc) {
    dest->Set(0, 0, 0);
    return;
  }

  r = src->Red();
  gr = src->Green();
  b = src->Blue();
  col = RGB(r, gr, b);
  result = GetNearestColor(dc, col);
  dest->Set(GetRValue(result), GetGValue(result), GetBValue(result));
  DoneDC(dc);
}

Bool wxCanvasDC::GCBlit(double xdest, double ydest, double width, double height,
			wxBitmap *source, double xsrc, double ysrc)
{
  if (blit_dc)
    return Blit(xdest, ydest, width, height, source, xsrc, ysrc, wxSTIPPLE, NULL);
  else
    return FALSE;
}

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

static BOOL DoPrintDlg(PRINTDLG *pd, HWND parent)
{
  if (!pd->hwndOwner)
    pd->hwndOwner = parent;

  return PrintDlg(pd);
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

wxPrinterDC::wxPrinterDC(wxWindow *parent, char *driver_name, char *device_name, char *file, Bool interactive)
{
  HWND hwnd = NULL;

  __type = wxTYPE_DC_PRINTER;
  wx_interactive = interactive;
  device = wxDEVICE_WINDOWS;

  if (file) {
    filename = copystring(file);
  } else
    filename = NULL;

  if (parent) {
    wxWnd *wnd = (wxWnd *)parent->handle;
    hwnd = wnd->handle;
  }

  screen_font = FALSE;

  if (interactive) {
    PRINTDLG *pd;

    pd = (PRINTDLG*)malloc(sizeof(PRINTDLG));
    
    memset(pd, 0, sizeof(PRINTDLG));
    pd->lStructSize = sizeof(PRINTDLG);
    pd->hwndOwner=hwnd;
    pd->hDevMode=(HANDLE)NULL;
    pd->hDevNames=(HANDLE)NULL;
    pd->Flags = PD_RETURNDC | PD_NOSELECTION | PD_NOPAGENUMS;
    pd->nFromPage=0xFFFF;
    pd->nToPage=0xFFFF;
    pd->nMinPage=1;
    pd->nMaxPage=0xFFFF;
    pd->nCopies=1;
    pd->hInstance=(HINSTANCE)NULL;
    
    if (wxPrimitiveDialog((wxPDF)DoPrintDlg, pd, 0)) {
      cdc = pd->hDC;
      ok = TRUE;
      free(pd);
    } else {
      ok = FALSE;
      free(pd);    
      return;
    }

    dont_delete = TRUE; // ??? WHY???
  } else if (driver_name && device_name && file) {
    cdc = CreateDC(driver_name, device_name, file, NULL);
    ok = cdc ? TRUE : FALSE;
  } else {
    cdc = wxGetPrinterDC();
    ok = cdc ? TRUE : FALSE;
  }
  
  if (cdc) {
    SetMapMode(wxPOINTS_MAP);
  }

  SetBrush(wxBLACK_BRUSH);
  SetPen(wxBLACK_PEN);
}

wxPrinterDC::wxPrinterDC(HDC theDC)
{
  __type = wxTYPE_DC_PRINTER;
  wx_interactive = FALSE;
  device = wxDEVICE_WINDOWS;

  filename = NULL;

  screen_font = FALSE;

  cdc = theDC;
  ok = TRUE;

  if (cdc) {
    SetMapMode(wxPOINTS_MAP);
  }

  SetBrush(wxBLACK_BRUSH);
  SetPen(wxBLACK_PEN);
}

int wxPrinterDC::CacheFontMetricsKey()
{
  return 0;
}

wxPrinterDC::~wxPrinterDC(void)
{
}

HDC wxGetPrinterDC(void)
{
    HDC         hDC;
    LPDEVMODE   lpDevMode = NULL;
    LPDEVNAMES  lpDevNames;
    LPSTR       lpszDriverName;
    LPSTR       lpszDeviceName;
    LPSTR       lpszPortName;

    PRINTDLG *pd;

    pd = (PRINTDLG*)malloc(sizeof(PRINTDLG));

    memset(pd, 0, sizeof(PRINTDLG));
    pd->lStructSize    = sizeof(PRINTDLG);
    pd->hwndOwner      = (HWND)NULL;
    pd->hDevMode       = NULL;
    pd->hDevNames      = NULL;
    pd->Flags          = PD_RETURNDEFAULT;
    pd->nCopies        = 1;

    if (!wxPrimitiveDialog((wxPDF)DoPrintDlg, pd, 0)) {
      free(pd);
      return NULL;
    }

    if (!pd->hDevNames) {
      free(pd);
      return NULL;
    }

    lpDevNames = (LPDEVNAMES)GlobalLock(pd->hDevNames);
    lpszDriverName = (LPSTR)lpDevNames XFORM_OK_PLUS lpDevNames->wDriverOffset;
    lpszDeviceName = (LPSTR)lpDevNames XFORM_OK_PLUS lpDevNames->wDeviceOffset;
    lpszPortName   = (LPSTR)lpDevNames XFORM_OK_PLUS lpDevNames->wOutputOffset;
    GlobalUnlock(pd->hDevNames);

    if (pd->hDevMode)
      lpDevMode = (LPDEVMODE)GlobalLock(pd->hDevMode);

    hDC = CreateDC(lpszDriverName, lpszDeviceName, lpszPortName, (DEVMODE *)lpDevMode);

    if (pd->hDevMode && lpDevMode)
      GlobalUnlock(pd->hDevMode);

    if (pd->hDevNames) {
	GlobalFree(pd->hDevNames);
	pd->hDevNames=NULL;
    }
    if (pd->hDevMode) {
       GlobalFree(pd->hDevMode);
       pd->hDevMode=NULL;
    }

    free(pd);

    return hDC;
}

/*
 * Memory DC
 *
 */

wxMemoryDC::wxMemoryDC(Bool ro)
{
  __type = wxTYPE_DC_MEMORY;
  device = wxDEVICE_WINDOWS;

  read_only = ro;

  cdc = wxwmCreateCompatibleDC(NULL);
  ok = (cdc != NULL);

  SetBrush(wxWHITE_BRUSH);
  SetPen(wxBLACK_PEN);
}

wxMemoryDC::wxMemoryDC(wxCanvasDC *old_dc):wxbMemoryDC(old_dc)
{
  wxWnd *wnd = NULL;
  HDC dc = NULL;

  __type = wxTYPE_DC_MEMORY;
  device = wxDEVICE_WINDOWS;

  if (old_dc->canvas)
    wnd = (wxWnd *)old_dc->canvas->handle;

  if (old_dc->cdc)
    dc = old_dc->cdc;
  else if (wnd)
    dc = wnd->GetHDC();

  cdc = wxwmCreateCompatibleDC(dc);
  ok = (cdc != NULL);

  if (!old_dc->cdc && wnd)
    wnd->ReleaseHDC();

  SetBrush(wxWHITE_BRUSH);
  SetPen(wxBLACK_PEN);
}

wxMemoryDC::~wxMemoryDC(void)
{
  if (selected_bitmap)
    SelectObject(NULL);
}

Bool wxMemoryDC::Ok(void)
{
  return (ok && selected_bitmap);
}

void wxMemoryDC::SelectObject(wxBitmap *bitmap)
{
  HBITMAP bm;
  wxColourMap *cm;

  if (bitmap == selected_bitmap)
    return;

  if (!cdc)
    return;

  ReleaseGraphics();

  if (wx_gl)
    wx_gl->Reset(NULL, 0, 1);

  if (!bitmap)
  {
    // Selecting nothing means, select old bitmap
    // out of the device context (e.g. so it can be deleted)
    if (old_bitmap)
    {
      ::SelectObject(cdc, old_bitmap);
      if (selected_bitmap)
      {
	if (!read_only) {
	  selected_bitmap->selectedInto = NULL;
	  selected_bitmap->selectedIntoDC = 0;
	}
        selected_bitmap = NULL;
      }
    }

    if (old_palette) {
      SelectPalette(cdc, old_palette, TRUE);
      old_palette = NULL;
    }
    return;
  }

  // Do own check for whether the bitmap is already selected into
  // a device context
  if ((!read_only && bitmap->selectedIntoDC) || !bitmap->Ok())
  {
    return;
  }

  if (selected_bitmap) {
    if (!read_only) {
      selected_bitmap->selectedInto = NULL;
      selected_bitmap->selectedIntoDC = 0;
    }
  }

  if (wx_gl)
    bitmap->ChangeToDIBSection(TRUE);
  
  selected_bitmap = bitmap;
  if (!read_only) {
    bitmap->selectedInto = this;
    bitmap->selectedIntoDC = -1;
  }

  bm = (HBITMAP)::SelectObject(cdc, bitmap->ms_bitmap);

  if (bm == ERROR)
  {
    selected_bitmap = NULL;
    if (!read_only) {
      bitmap->selectedInto = NULL;
      bitmap->selectedIntoDC = 0;
    }

   if (old_bitmap) {
     ::SelectObject(cdc, old_bitmap);
     old_bitmap = NULL;
   }

   bitmap = NULL;
  } else if (!old_bitmap)
    old_bitmap = bm;

  cm = (bitmap ? bitmap->GetColourMap() : NULL);
  if (cm && cm->ms_palette) {
    HPALETTE p;
    p = SelectPalette(cdc, cm->ms_palette, TRUE);
    if (p) {
      RealizePalette(cdc);
      old_palette = p;
    }
  } else if (old_palette) {
    SelectPalette(cdc, old_palette, TRUE);
    RealizePalette(cdc);
    old_palette = NULL;
  }

  if (bitmap && (bitmap->GetDepth() == 1)) {
    Colour = 0;
    if (anti_alias)
      anti_alias = 0;
  } else {
    Colour = 1;
  }

  if (wx_gl && selected_bitmap)
    wx_gl->Reset(selected_bitmap->gl_cfg, cdc, 1);
}

wxBitmap* wxMemoryDC::GetObject(void)
{
  return selected_bitmap;
}

void wxMemoryDC::GetSize(double *width, double *height)
{
  double bw, bh;

  if (!selected_bitmap)
  {
    *width = 0.0; *height = 0.0;
    return;
  }
  bw = selected_bitmap->GetWidth();
  *width = bw;
  bh = selected_bitmap->GetHeight();
  *height = bh;
}

wxGL *wxMemoryDC::GetGL()
{
  if (!wx_gl) {
    if (cdc) {
      if (selected_bitmap && !selected_bitmap->IsDIB()) {
	::SelectObject(cdc, old_bitmap);
	selected_bitmap->ChangeToDIBSection(TRUE);
	::SelectObject(cdc, selected_bitmap->ms_bitmap);
      }

      wx_gl = new wxWinGL();
      wx_gl->Reset(selected_bitmap ? selected_bitmap->gl_cfg : wx_gl_cfg, cdc, 1);
    }
  }

  return wx_gl;
}

/**************************************************/

/*
 * wxGL implementation
 */

#include <gl/gl.h>
#include <gl/glu.h>
#if _MSC_VER < 1500
# include <gl/glaux.h>
#endif
#include "wx_wglext.h"

#include "../../../wxcommon/wxGLConfig.cxx"

static wxWinGL *current_gl_context;

wxGL::wxGL()
  : wxObject(WXGC_NO_CLEANUP)
{
}

wxGL::~wxGL()
{
}

wxWinGL::wxWinGL()
{
}

void wxWinGL::Reset(wxGLConfig *cfg, HDC dc, int offscreen)
{
  if (current_gl_context == this) {
    wglMakeCurrent(NULL, NULL);
  }

  if (m_hGLRC) {
    wglDeleteContext(m_hGLRC);
    m_hGLRC = NULL;
  }
  if (m_deletePalette) {
    DELETE_OBJ m_palette;
    m_palette = NULL;
    m_deletePalette = 0;
  }

  if (dc) {
    int pixelFormat = 0;
#ifdef MZ_PRECISE_GC
    START_XFORM_SKIP;
#endif
    PIXELFORMATDESCRIPTOR pfd = {
      sizeof(PIXELFORMATDESCRIPTOR),	/* size */
      1,				/* version */
      (PFD_SUPPORT_OPENGL | PFD_DRAW_TO_WINDOW | PFD_DOUBLEBUFFER),
      PFD_TYPE_RGBA,			/* color type */
      24,				/* prefered color depth */
      0, 0, 0, 0, 0, 0,		        /* color bits (ignored) */
      0,				/* no alpha buffer */
      0,				/* alpha bits (ignored) */
      0,				/* no accumulation buffer */
      0, 0, 0, 0,			/* accum bits (ignored) */
      0,				/* depth buffer */
      0,				/* no stencil buffer */
      0,				/* no auxiliary buffers */
      PFD_MAIN_PLANE,			/* main layer */
      0,				/* reserved */
      0, 0, 0			/* no layer, visible, damage masks */
    };
#ifdef MZ_PRECISE_GC
    END_XFORM_SKIP;
#endif

    if (offscreen) {
      pfd.dwFlags = (PFD_SUPPORT_OPENGL 
		     | PFD_DRAW_TO_BITMAP
		     | PFD_SUPPORT_GDI);
      pfd.cColorBits = 32;
      pfd.cDepthBits = 32;
    }

    if (cfg) {
      pfd.cDepthBits = cfg->depth;
      pfd.cStencilBits = cfg->stencil;
      if (cfg->stereo)
	pfd.dwFlags |= PFD_STEREO;
      if (cfg->doubleBuffered && !offscreen)
	pfd.dwFlags |= PFD_DOUBLEBUFFER;
      else
	pfd.dwFlags -= (pfd.dwFlags & PFD_DOUBLEBUFFER);
      pfd.cAccumBits = 4 * cfg->accum;
      pfd.cAccumRedBits = cfg->accum;
      pfd.cAccumBlueBits = cfg->accum;
      pfd.cAccumGreenBits = cfg->accum;
      pfd.cAccumAlphaBits = cfg->accum;
    }

    if (cfg && cfg->multisample) {
      /* Based on code from http://nehe.gamedev.net/data/lessons/lesson.asp?lesson=46 */
      PFNWGLCHOOSEPIXELFORMATARBPROC wglChoosePixelFormatARB;

      wglChoosePixelFormatARB = (PFNWGLCHOOSEPIXELFORMATARBPROC)wglGetProcAddress("wglChoosePixelFormatARB");
      if (wglChoosePixelFormatARB) {
	int a[30];
	BOOL valid;
	UINT numFormats;
	GC_CAN_IGNORE float fAttributes[] = {0,0};
	
	a[0] = (offscreen ? WGL_DRAW_TO_BITMAP_ARB : WGL_DRAW_TO_WINDOW_ARB);
	a[1] = GL_TRUE;
	a[2] = WGL_SUPPORT_OPENGL_ARB;
	a[3] = GL_TRUE;
	a[4] = WGL_ACCELERATION_ARB;
	a[5] = WGL_FULL_ACCELERATION_ARB;
	a[6] = WGL_COLOR_BITS_ARB;
	a[7] = 24;
	a[8] = WGL_ALPHA_BITS_ARB;
	a[9] = 8;
	a[10] = WGL_DEPTH_BITS_ARB;
	a[11] = cfg->depth;
	a[12] = WGL_STENCIL_BITS_ARB;
	a[13] = cfg->stencil;
	a[14] = WGL_DOUBLE_BUFFER_ARB;
	a[15] = ((cfg->doubleBuffered && !offscreen) ? GL_TRUE : GL_FALSE);
	a[16] = WGL_SAMPLE_BUFFERS_ARB;
	a[17] = GL_TRUE;
	a[18] = WGL_SAMPLES_ARB;
	a[19] = cfg->multisample;
	a[20] = 0;
	a[21] = 0;

	// First we check to see if we can get a pixel format for given samples:
	valid = wglChoosePixelFormatARB(dc,a,fAttributes,1,&pixelFormat,&numFormats);
 
	// If returned true, and our format count is greater than 1
	if (valid && numFormats >= 1) {
	  /* Done */
	} else {
	  // Our pixel format with the given number of samples failed; test for 2 samples:
	  a[19] = 2;
	  valid = wglChoosePixelFormatARB(dc,a,fAttributes,1,&pixelFormat,&numFormats);
	  if (valid && numFormats >= 1) {
	    /* Done */
	  } else
	    pixelFormat = 0;
	}
      }
    }

    if (!pixelFormat)
      pixelFormat = ChoosePixelFormat(dc, &pfd);

    if (pixelFormat != 0) {
      if (SetPixelFormat(dc, pixelFormat, &pfd)) {
	DescribePixelFormat(dc, pixelFormat, sizeof(PIXELFORMATDESCRIPTOR), &pfd);
	if (pfd.dwFlags & PFD_NEED_PALETTE)
	  SetupPalette(&pfd);

	m_hGLRC = wglCreateContext(dc);
	m_hDC = dc;

	if (current_gl_context == this) {
	  current_gl_context = NULL;
	  ThisContextCurrent();
	}
      }
    }
  }
}

int wxWinGL::Ok(void)
{
  return !!m_hGLRC;
}

void wxWinGL::SwapBuffers(void)
{
  if (m_hDC) {
    ::SwapBuffers(m_hDC);
  }
}

void wxWinGL::ThisContextCurrent(void)
{
  if (current_gl_context != this) {
    current_gl_context = this;
    if (m_hGLRC && m_hDC)
      wglMakeCurrent(m_hDC, m_hGLRC);
    else
      wglMakeCurrent(NULL, NULL);
  }
}

void wxWinGL::SetupPalette(PIXELFORMATDESCRIPTOR *pfd)
{
  m_palette = CreateDefaultPalette(pfd);
  m_deletePalette = TRUE;
  
  if (m_palette && m_palette->ms_palette) {
    SelectPalette(m_hDC, m_palette->ms_palette, FALSE);
    RealizePalette(m_hDC);
  }
}

 wxColourMap* wxWinGL::CreateDefaultPalette(PIXELFORMATDESCRIPTOR *pfd)
{
  int paletteSize;
  LOGPALETTE* pPal;
  HPALETTE hPalette;
  wxColourMap* cmap;

  paletteSize = 1 << pfd->cColorBits;
  
  pPal = (LOGPALETTE*)new char[sizeof(LOGPALETTE) + paletteSize * sizeof(PALETTEENTRY)];
  pPal->palVersion = 0x300;
  pPal->palNumEntries = paletteSize;

  /* build a simple RGB color palette */
  {
    int redMask = (1 << pfd->cRedBits) - 1;
    int greenMask = (1 << pfd->cGreenBits) - 1;
    int blueMask = (1 << pfd->cBlueBits) - 1;
    int i;

    for (i=0; i<paletteSize; ++i) {
      pPal->palPalEntry[i].peRed =
	(((i >> pfd->cRedShift) & redMask) * 255) / redMask;
      pPal->palPalEntry[i].peGreen =
	(((i >> pfd->cGreenShift) & greenMask) * 255) / greenMask;
      pPal->palPalEntry[i].peBlue =
	(((i >> pfd->cBlueShift) & blueMask) * 255) / blueMask;
      pPal->palPalEntry[i].peFlags = 0;
    }
  }

  hPalette = CreatePalette(pPal);
  free(pPal);

  cmap = new wxColourMap;
  cmap->ms_palette = hPalette;
  
  return cmap;
}

void wxGLNoContext()
{
  current_gl_context = NULL;
  wglMakeCurrent(NULL, NULL);
}
