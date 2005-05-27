///////////////////////////////////////////////////////////////////////////////
// File:	wx_dccan2.cc
// Purpose:	Canvas device context implementation (Macintosh version) (part 2)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "common.h"
#include "wx_dccan.h"
#include "wx_utils.h"
#include "wx_canvs.h"
#include "wx_area.h"
#include "wx_rgn.h"

extern CGrafPtr wxMainColormap;

// constant to convert radian to degree
#define RAD2DEG 57.2957795131
#define wxPI 3.141592653589793

//-----------------------------------------------------------------------------
void wxCanvasDC::Clear(void)
     //-----------------------------------------------------------------------------
{
  int w, h;
  Rect theClearRect;
  
  if (!Ok() || !cMacDC) return;

  if (canvas)
    canvas->GetVirtualSize(&w, &h);
  else {
    w = pixmapWidth;
    h = pixmapHeight;
  }
  
  if (anti_alias) {
    CGContextRef cg;
    wxColor *c = current_background_color;
    int r, g, b;
    CGrafPtr qdp;
    Rect portRect;

    SetCurrentDC(TRUE);
    cg = GetCG();

    CGContextSaveGState(cg);

    /* Need to clear the whole background, so turn off scaling and offset */
    qdp = cMacDC->macGrafPort();
    SyncCGContextOriginWithPort(cg, qdp);
    GetPortBounds(qdp, &portRect);
    CGContextTranslateCTM(cg, gdx, (float)(portRect.bottom - portRect.top - gdy));
    CGContextScaleCTM(cg, 1.0, -1.0 );

    r = c->Red();
    g = c->Green();
    b = c->Blue();
      
    CGContextSetRGBFillColor(cg, r / 255.0, g / 255.0, b / 255.0, 1.0);

    CGContextMoveToPoint(cg, 0, 0);
    CGContextAddLineToPoint(cg, w, 0);
    CGContextAddLineToPoint(cg, w, h);
    CGContextAddLineToPoint(cg, 0, h);
    CGContextFillPath(cg);

    CGContextRestoreGState(cg);

    ReleaseCurrentDC();

    return;
  }
  
  SetCurrentDC();
  wxMacSetCurrentTool(kBGTool);

  ::SetRect(&theClearRect, 0, 0, w, h);
  OffsetRect(&theClearRect,SetOriginX,SetOriginY);
  ::EraseRect(&theClearRect);

  ReleaseCurrentDC();
}

void wxCanvasDC::GetSize(double *width, double *height)
{
  if (canvas) {
    int w, h;
    canvas->GetVirtualSize(&w, &h);
    *width = w;
    *height = h;
  } else {
    *width = pixmapWidth;
    *height = pixmapHeight;
  }
}

//-----------------------------------------------------------------------------
Bool wxCanvasDC::GetPixel(double x, double y, wxColour *col)
     //=============================================================================
{
  RGBColor rgb;
  int i, j;

  if (!Ok() || !cMacDC) return FALSE;
  
  SetCurrentDC();

  i = XLOG2DEV(x) + SetOriginX;
  j = YLOG2DEV(y) + SetOriginY;
  GetCPixel(i, j, &rgb);
  col->Set(rgb.red >> 8, rgb.green >> 8, rgb.blue >> 8);

  ReleaseCurrentDC();

  return ((i >= 0) && (j >= 0)
	  && (i < pixmapWidth)
	  && (j < pixmapHeight));
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetPixel(double x, double y, wxColour *col)
     //=============================================================================
{
  if (!Ok() || !cMacDC) return;
  
  SetCurrentDC();

  SetPixelCore(XLOG2DEV(x) + SetOriginX, YLOG2DEV(y) + SetOriginY,
	       col->Red(), col->Green(), col->Blue());

  ReleaseCurrentDC();
}

Bool wxCanvasDC::BeginSetPixelFast(int x, int y, int w, int h)
{
  if ((x >= 0) && (y >= 0)
      && ((x + w) <= pixmapWidth)
      && ((y + h) <= pixmapHeight)) {
    PixMapHandle ph;
    cMacDC->EndCG();
    ph = GetGWorldPixMap(cMacDC->macGrafPort());
    fast_pb = GetPixBaseAddr(ph);
    fast_rb = GetPixRowBytes(ph);
    return TRUE;
  } else
    return FALSE;
}

void wxCanvasDC::EndSetPixelFast()
{
  fast_pb = NULL;
}

void wxCanvasDC::SetPixelFast(int i, int j, int r, int g, int b)
{
  if (Colour) {
    UInt32 *p;
    
    p = (UInt32 *)fast_pb;
    p[(j * (fast_rb >> 2)) + i] = ((r << 16) | (g << 8) | (b << 0));
  } else {
    unsigned char *p, v, bit;
    int pos;

    p = (unsigned char *)fast_pb;
    bit = 1 << (7 - (i & 0x7));
    pos = (j * fast_rb) + (i >> 3);
    v = p[pos];
    if (r || g || b)
      v -= (v & bit);
    else
      v |= bit;
    p[pos] = v;
  }
}

void wxCanvasDC::SetPixelCore(int i, int j, int r, int g, int b)
{
  RGBColor rgb;

  if (Colour) {
    rgb.red = r;
    rgb.red = (rgb.red << 8) | rgb.red;
    rgb.green = g;
    rgb.green = (rgb.green << 8) | rgb.green;
    rgb.blue = b;
    rgb.blue = (rgb.blue << 8) | rgb.blue;
    SetCPixel(i, j, &rgb);
  } else {
    int qcol;

    if ((r == 255) && (b == 255) && (g == 255)) {
      qcol = whiteColor;
    } else {
      qcol = blackColor;      
    }

    GetForeColor(&rgb);
    ForeColor(qcol);
    wxMacDrawPoint(i, j);
    if (rgb.red) {
      if (qcol != whiteColor)
	ForeColor(whiteColor);
    } else {
      if (qcol != blackColor)
	ForeColor(blackColor);
    }
  }
}

Bool wxCanvasDC::BeginGetPixelFast(int x, int y, int w, int h)
{
  return BeginSetPixelFast(x, y, w, h);
}

void wxCanvasDC::EndGetPixelFast()
{
  EndSetPixelFast();
}

void wxCanvasDC::GetPixelFast(int x, int y, int *r, int *g, int *b)
{
  if (Colour) {
    UInt32 *p, v;

    p = (UInt32 *)fast_pb;
    v = p[(y * (fast_rb >> 2)) + x];
    *r = (v >> 16) & 0xFF;
    *g = (v >> 8) & 0xFF;
    *b = v & 0xFF;
  } else {
    unsigned char *p, v, bit;

    p = (unsigned char *)fast_pb;
    bit = 1 << (7 - (x & 0x7));
    v = p[(y * fast_rb) + (x >> 3)];
    if (v & bit)
      *r = *b = *g = 0;
    else
      *r = *b = *g = 255;
  }
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawLine(double x1, double y1, double x2, double y2)
     //-----------------------------------------------------------------------------
{
  int dpx, dpy;

  if (!Ok() || !cMacDC) return;
  
  if (!current_pen || current_pen->GetStyle() == wxTRANSPARENT)
    return;

  if (anti_alias) {
    double xx1, yy1, xx2, yy2;
    CGContextRef cg;

    SetCurrentDC(TRUE);
    cg = GetCG();

    CGContextSaveGState(cg);

    xx1 = SmoothingXFormX(x1);
    yy1 = SmoothingXFormY(y1);
    xx2 = SmoothingXFormX(x2);
    yy2 = SmoothingXFormY(y2);

    CGContextMoveToPoint(cg, xx1, yy1);
    CGContextAddLineToPoint(cg, xx2, yy2);
    
    wxMacSetCurrentTool(kPenTool);
    CGContextStrokePath(cg);
    wxMacSetCurrentTool(kNoTool);

    CGContextRestoreGState(cg);

    ReleaseCurrentDC();

    return;
  }

  dpx = (XLOG2DEVREL(current_pen->GetWidth()) >> 1);
  dpy = (YLOG2DEVREL(current_pen->GetWidth()) >> 1);

  SetCurrentDC();
  wxMacSetCurrentTool(kPenTool);
  wxMacDrawLine(XLOG2DEV(x1)-dpx, YLOG2DEV(y1)-dpy, XLOG2DEV(x2)-dpx, YLOG2DEV(y2)-dpy);
  ReleaseCurrentDC();
}

//-----------------------------------------------------------------------------
static void FillWithStipple(wxDC *dc, wxRegion *r, wxBrush *brush)
{
  double x, y, w, h, bw, bh;
  int xstart, xend, ystart, yend, i, j, ibw, ibh;
  wxRegion *old;
  int style;
  wxColour *c;
  wxBitmap *bm;

  old = dc->GetClippingRegion();
  if (old)
    r->Intersect(old);

  if (r->Empty())
    return;

  bm = brush->GetStipple();
  style = brush->GetStyle();
  c = brush->GetColour();

  r->BoundingBox(&x, &y, &w, &h);
  bw = bm->GetWidth();
  bh = bm->GetHeight();

  x = dc->LogicalToDeviceX(x);
  y = dc->LogicalToDeviceY(y);
  w = dc->LogicalToDeviceXRel(w);
  h = dc->LogicalToDeviceYRel(h);
  
  xstart = (int)floor(x / bw);
  xend = (int)floor((x + w + bw - 0.00001) / bw);

  ystart = (int)floor(y / bh);
  yend = (int)floor((y + h + bh - 0.00001) / bh);

  ibw = (int)floor(bw);
  ibh = (int)floor(bh);

  dc->SetClippingRegion(r);

  for (i = xstart; i < xend; i++) {
    for (j = ystart; j < yend; j++) {
      dc->Blit(dc->DeviceToLogicalX(i * ibw), 
               dc->DeviceToLogicalY(j * ibh), 
               dc->DeviceToLogicalXRel(ibw), 
               dc->DeviceToLogicalYRel(ibh),
               bm, 0, 0, style, c);
    }
  }

  dc->SetClippingRegion(old);
}

wxRegion *wxCanvasDC::BrushStipple()
{
  if (current_brush) {
    wxBitmap *bm;
    bm = current_brush->GetStipple();
    if (bm && bm->Ok())
      return new wxRegion(this);
  }
  return NULL;
}

void wxCanvasDC::PaintStipple(wxRegion *r)
{
  FillWithStipple(this, r, current_brush);
}

static void AdjustPenRect(wxPen *current_pen, Rect *theRect,
			  double user_scale_x, double user_scale_y)
{
  int pw, pwx, pwy;
  pw = current_pen->GetWidth();
  pwx = (int)(user_scale_x * pw);
  pwx >>= 1;
  theRect->left -= pwx;
  if (pwx > 0)
    --pwx;
  theRect->right += pwx;
  pwy = (int)(user_scale_y * pw);
  pwy >>= 1;
  theRect->top -= pwy;
  if (pwy > 0)
    --pwy;
  theRect->bottom += pwy;
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawArc(double x,double y,double w,double h,double start,double end)
{
  wxRegion *rgn;
  int xx, yy, xx2, yy2;
  int alpha1, alpha2;
  double degrees1, degrees2;
  Rect rect;

  if (!Ok() || !cMacDC) return;

  if (anti_alias) {
    CGContextRef cg;
    CGMutablePathRef path;
    CGAffineTransform xform;

    SetCurrentDC(TRUE);
    cg = GetCG();

    CGContextSaveGState(cg);

    start = (2 * wxPI) - start;
    end = (2 * wxPI) - end;

    if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
      double xx, yy, ww, hh;

      xx = SmoothingXFormXB(x);
      yy = SmoothingXFormYB(y);
      ww = SmoothingXFormW(w, x);
      hh = SmoothingXFormH(h, y);

      path = CGPathCreateMutable();
      xform = CGAffineTransformScale(CGAffineTransformMakeTranslation(xx, yy), ww, hh);
      CGPathAddArc(path, &xform, 0.5, 0.5, 0.5, start, end, TRUE);

      wxMacSetCurrentTool(kBrushTool);
      CGContextBeginPath(cg);
      CGContextAddPath(cg, path);
      if ((end != 0) || (start != (2 * wxPI))) {
	CGContextAddLineToPoint(cg, xx + ww/2, yy + hh/2);
	CGContextClosePath(cg);
      }
      CGContextFillPath(cg);

      CGPathRelease(path);
    }
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
      double xx, yy, ww, hh;

      xx = SmoothingXFormX(x);
      yy = SmoothingXFormY(y);
      ww = SmoothingXFormWL(w, x);
      hh = SmoothingXFormHL(h, y);

      path = CGPathCreateMutable();
      xform = CGAffineTransformScale(CGAffineTransformMakeTranslation(xx, yy), ww, hh);
      CGPathAddArc(path, &xform, 0.5, 0.5, 0.5, start, end, TRUE);

      wxMacSetCurrentTool(kPenTool);
      CGContextBeginPath(cg);
      CGContextAddPath(cg, path);
      CGContextStrokePath(cg);

      CGPathRelease(path);
    }
    wxMacSetCurrentTool(kNoTool);

    CGContextRestoreGState(cg);

    ReleaseCurrentDC();

    return;
  }
  
  if (start == end) {
    DrawEllipse(x, y, w, h);
    return;
  }

  if ((rgn = BrushStipple())) {
    rgn->SetArc(x, y, w, h, start, end);
    PaintStipple(rgn);
    DELETE_OBJ rgn;
  }

  SetCurrentDC();

  xx = XLOG2DEV(x); yy = YLOG2DEV(y);
  xx2 = XLOG2DEV(x+w); yy2 = YLOG2DEV(y+h);
  
  degrees1 = start * RAD2DEG;
  degrees2 = end * RAD2DEG;
  
  /* Convert to QD angles for clockwise arc: */
   alpha1 = (int)(-degrees2 + 90) % 360;
  if (alpha1 < 0)
    alpha1 += 360;
  alpha2 = (int)(-degrees1 + 90) % 360;
  if (alpha2 < 0)
    alpha2 += 360;
  
  /* Alpha2 should be positive difference: */  
  alpha2 -= alpha1;
  if (alpha2 < 0)
    alpha2 += 360;

  rect.left = xx;
  rect.top = yy;
  rect.right = xx2;
  rect.bottom = yy2;
  OffsetRect(&rect,SetOriginX,SetOriginY);

  if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
    if (!rgn) {
      wxMacSetCurrentTool(kBrushTool);
      if (paint_brush_with_erase)
	EraseArc(&rect, alpha1, alpha2);
      else
	PaintArc(&rect, alpha1, alpha2);
    }
  }
  if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
    wxMacSetCurrentTool(kPenTool);
    AdjustPenRect(current_pen, &rect, user_scale_x, user_scale_y);
    FrameArc(&rect, alpha1, alpha2);
  }
  
  ReleaseCurrentDC();
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawPoint(double x, double y)
     //-----------------------------------------------------------------------------
{
  if (!Ok() || !cMacDC) return;
  
  if (!current_pen || current_pen->GetStyle() == wxTRANSPARENT)
    return;

  SetCurrentDC();
  wxMacSetCurrentTool(kPenTool);
  wxMacDrawPoint(XLOG2DEV(x), YLOG2DEV(y));
  ReleaseCurrentDC();
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawPolygon(int n, wxPoint points[],
			     double xoffset, double yoffset, int fillStyle)
{
  wxRegion *rgn;
  Point *xpoints1;
  int i, j, dpx, dpy;
  PolyHandle thePolygon;

  if (!Ok() || !cMacDC) return;
  
  if (n <= 0) return;

  if (anti_alias) {
    CGContextRef cg;
    CGMutablePathRef path;

    SetCurrentDC(TRUE);
    cg = GetCG();

    CGContextSaveGState(cg);

    path = CGPathCreateMutable();
    CGPathMoveToPoint(path, NULL, SmoothingXFormX(points[0].x + xoffset), SmoothingXFormY(points[0].y + yoffset));
    for (i = 1; i < n; i++) {
      CGPathAddLineToPoint(path, NULL, SmoothingXFormX(points[i].x + xoffset), SmoothingXFormY(points[i].y + yoffset));
    }
    CGPathCloseSubpath(path);

    if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
      wxMacSetCurrentTool(kBrushTool);
      CGContextBeginPath(cg);
      CGContextAddPath(cg, path);
      if (fillStyle == wxODDEVEN_RULE)
	CGContextEOFillPath(cg);
      else
	CGContextFillPath(cg);
    }
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
      wxMacSetCurrentTool(kPenTool);
      CGContextBeginPath(cg);
      CGContextAddPath(cg, path);
      CGContextStrokePath(cg);
    }
    wxMacSetCurrentTool(kNoTool);

    CGPathRelease(path);

    CGContextRestoreGState(cg);

    ReleaseCurrentDC();

    return;
  }

  if ((rgn = BrushStipple())) {
    rgn->SetPolygon(n, points, xoffset, yoffset, fillStyle);
    PaintStipple(rgn);
    DELETE_OBJ rgn;
  }

  SetCurrentDC();

  xpoints1 = new Point[n+1];
  for (i = 0; i < n; i++) {
    xpoints1[i].h = XLOG2DEV(points[i].x + xoffset);
    xpoints1[i].v = YLOG2DEV(points[i].y + yoffset);
  }

  // Close figure
  xpoints1[n].h = xpoints1[0].h;
  xpoints1[n].v = xpoints1[0].v;

  thePolygon = OpenPoly();
  MoveTo(xpoints1[0].h + SetOriginX, xpoints1[0].v + SetOriginY);
  for (j = 1; j <= n; j++) {
    LineTo(xpoints1[j].h + SetOriginX, xpoints1[j].v + SetOriginY);
  }
  ClosePoly();

  if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
    if (!rgn) {
      if (cMacCurrentTool != kBrushTool) wxMacSetCurrentTool(kBrushTool);
      if (paint_brush_with_erase)
	ErasePoly(thePolygon);
      else
	PaintPoly(thePolygon);
    }
  }

  dpx = (XLOG2DEVREL(current_pen->GetWidth()) >> 1);
  dpy = (YLOG2DEVREL(current_pen->GetWidth()) >> 1);

  OffsetPoly(thePolygon, -dpx, -dpy);

  if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
    wxMacSetCurrentTool(kPenTool);
    FramePoly(thePolygon);
  }

  KillPoly(thePolygon);

  ReleaseCurrentDC();
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawPath(wxPath *p, double xoffset, double yoffset, int fillStyle)
{
  wxRegion *rgn;
  double **ptss;
  int *lens, cnt, i, total_cnt, j, k, m;
  Point *pts;
  PolyHandle thePolygon = 0;

  if (!Ok() || !cMacDC) return;
  
  if (anti_alias) {
    CGContextRef cg;
    CGMutablePathRef path;

    SetCurrentDC(TRUE);
    cg = GetCG();

    CGContextSaveGState(cg);

    path = CGPathCreateMutable();
    if (AlignSmoothing()) {
      double pw;
      pw = GetPenSmoothingOffset();
      p->Install((long)path, xoffset, yoffset,
		 device_origin_x, device_origin_y, user_scale_x, user_scale_y,
		 TRUE, pw, pw);
    } else
      p->Install((long)path, xoffset, yoffset,
		 0, 0, 1, 1, FALSE, 0, 0);

    if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
      wxMacSetCurrentTool(kBrushTool);
      CGContextBeginPath(cg);
      CGContextAddPath(cg, path);
      if (fillStyle == wxODDEVEN_RULE)
	CGContextEOFillPath(cg);
      else
	CGContextFillPath(cg);
    }
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
      wxMacSetCurrentTool(kPenTool);
      CGContextBeginPath(cg);
      CGContextAddPath(cg, path);
      CGContextStrokePath(cg);
    }
    wxMacSetCurrentTool(kNoTool);

    CGPathRelease(path);

    CGContextRestoreGState(cg);

    ReleaseCurrentDC();

    return;
  }

  if ((rgn = BrushStipple())) {
    rgn->SetPath(p, xoffset, yoffset, fillStyle);
    PaintStipple(rgn);
    DELETE_OBJ rgn;
  }

  SetCurrentDC();


  cnt = p->ToPolygons(&lens, &ptss, user_scale_x, user_scale_y);

  if (!cnt)
    return;

  total_cnt = 0;
  for (i = 0; i < cnt; i++) {
    total_cnt += (lens[i] / 2);
    if ((i + 1 < cnt) || !p->IsOpen())
      total_cnt++;
  }
  
  pts = new WXGC_ATOMIC Point[total_cnt];

  for (i = 0, k = 0; i < cnt; i++) {
    for (j = 0; j < lens[i]; j += 2) {
      pts[k].h = XLOG2DEV(ptss[i][j]+xoffset);
      pts[k].v = YLOG2DEV(ptss[i][j+1]+yoffset);
      k++;
    }
    if ((i + 1 < cnt) || !p->IsOpen()) {
      pts[k].h = XLOG2DEV(ptss[i][0]+xoffset);
      pts[k].v = YLOG2DEV(ptss[i][1]+yoffset);
      k++;
    }
  }

  if (cnt == 1) {
    thePolygon = OpenPoly();
    MoveTo(pts[0].h + SetOriginX, pts[0].v + SetOriginY);
    for (j = 1; j < total_cnt; j++) {
      LineTo(pts[j].h + SetOriginX, pts[j].v + SetOriginY);
    }
    ClosePoly();
  }

  if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
    wxMacSetCurrentTool(kBrushTool);
    if (cnt == 1) {
      if (paint_brush_with_erase)
	ErasePoly(thePolygon);
      else
	PaintPoly(thePolygon);
    } else {
      wxRegion *r;

      r = new wxRegion(this);
      r->SetPath(p, xoffset, yoffset);
      ::OffsetRgn(r->rgn,SetOriginX,SetOriginY);
      
      if (paint_brush_with_erase)
	EraseRgn(r->rgn);
      else
	PaintRgn(r->rgn);

      DELETE_OBJ r;
    }
  }
  if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
    int dpx, dpy;

    wxMacSetCurrentTool(kPenTool);

    dpx = (XLOG2DEVREL(current_pen->GetWidth()) >> 1);
    dpy = (YLOG2DEVREL(current_pen->GetWidth()) >> 1);

    if (cnt == 1) {
      OffsetPoly(thePolygon, -dpx, -dpy);
      FramePoly(thePolygon);
    } else {
      for (i = 0, k = 0; i < cnt; i++) {
	j = (lens[i] / 2);
	if ((i < cnt - 1) || !p->IsOpen())
	  j++;
	
	thePolygon = OpenPoly();
	MoveTo(pts[k].h + SetOriginX, pts[k].v + SetOriginY);
	k++;
	for (m = 1; m < j; m++) {
	  LineTo(pts[k].h + SetOriginX, pts[k].v + SetOriginY);
	  k++;
	}
	ClosePoly();
	OffsetPoly(thePolygon, -dpx, -dpy);
	FramePoly(thePolygon);
	KillPoly(thePolygon);
	thePolygon = NULL;
      }
    }
  }

  if (thePolygon)
    KillPoly(thePolygon);

  ReleaseCurrentDC();
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawLines(int n, wxPoint points[], double xoffset, double yoffset)
{
  if (!Ok() || !cMacDC) return;
  
  if (n <= 0) return;

  if (!current_pen || current_pen->GetStyle() == wxTRANSPARENT)
    return;

  if (anti_alias) {
    CGContextRef cg;
    int i;

    SetCurrentDC(TRUE);
    cg = GetCG();

    CGContextSaveGState(cg);

    CGContextMoveToPoint(cg, SmoothingXFormX(points[0].x + xoffset), SmoothingXFormY(points[0].y + yoffset));
    for (i = 1; i < n; i++) {
      CGContextAddLineToPoint(cg, SmoothingXFormX(points[i].x + xoffset), SmoothingXFormY(points[i].y + yoffset));
    }
    
    wxMacSetCurrentTool(kPenTool);
    CGContextStrokePath(cg);
    wxMacSetCurrentTool(kNoTool);

    CGContextRestoreGState(cg);

    ReleaseCurrentDC();

    return;
  }

  {
    Point *xpoints;
    int i, j, dpx, dpy;
    PolyHandle thePolygon;

    SetCurrentDC();
    wxMacSetCurrentTool(kPenTool);
    
    xpoints = new Point[n];
      
    dpx = (XLOG2DEVREL(current_pen->GetWidth()) >> 1);
    dpy = (YLOG2DEVREL(current_pen->GetWidth()) >> 1);

    for (i = 0; i < n; i++) {
      xpoints[i].h = XLOG2DEV(points[i].x + xoffset);
      xpoints[i].v = YLOG2DEV(points[i].y + yoffset);
    }
      
    thePolygon = OpenPoly();
    MoveTo(xpoints[0].h + SetOriginX - dpx, xpoints[0].v + SetOriginY - dpy);
    for (j = 1; j < n; j++) {
      LineTo(xpoints[j].h + SetOriginX - dpx, xpoints[j].v + SetOriginY - dpy);
    }
    ClosePoly();
    
    FramePoly(thePolygon);
    
    KillPoly(thePolygon);
    
    ReleaseCurrentDC();
  }
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawRectangle(double x, double y, double width, double height)
     //-----------------------------------------------------------------------------
{
  wxRegion *rgn;

  if (!Ok() || !cMacDC) return;

  if (anti_alias) {
    CGContextRef cg;

    SetCurrentDC(TRUE);
    cg = GetCG();

    CGContextSaveGState(cg);

    if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
      double xx, yy, ww, hh;

      xx = SmoothingXFormXB(x);
      yy = SmoothingXFormYB(y);
      ww = SmoothingXFormW(width, x);
      hh = SmoothingXFormH(height, y);

      wxMacSetCurrentTool(kBrushTool);
      CGContextMoveToPoint(cg, xx, yy);
      CGContextAddLineToPoint(cg, xx + ww, yy);
      CGContextAddLineToPoint(cg, xx + ww, yy + hh);
      CGContextAddLineToPoint(cg, xx, yy + hh);
      CGContextClosePath(cg);
      CGContextFillPath(cg);
    }

    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
      double xx, yy, ww, hh;

      xx = SmoothingXFormX(x);
      yy = SmoothingXFormY(y);
      ww = SmoothingXFormWL(width, x);
      hh = SmoothingXFormHL(height, y);
    
      wxMacSetCurrentTool(kPenTool);
      CGContextMoveToPoint(cg, xx, yy);
      CGContextAddLineToPoint(cg, xx + ww, yy);
      CGContextAddLineToPoint(cg, xx + ww, yy + hh);
      CGContextAddLineToPoint(cg, xx, yy + hh);
      CGContextClosePath(cg);
      CGContextStrokePath(cg);
    }

    wxMacSetCurrentTool(kNoTool);

    CGContextRestoreGState(cg);

    ReleaseCurrentDC();

    return;
  }
  
  if ((rgn = BrushStipple())) {
    rgn->SetRectangle(x, y, width, height);
    PaintStipple(rgn);
    DELETE_OBJ rgn;
  }

  SetCurrentDC();
  
  {
    int top = YLOG2DEV(y);
    int left = XLOG2DEV(x);
    int bottom = YLOG2DEV(y + height);
    int right = XLOG2DEV(x + width);
    Rect theRect = {top, left, bottom, right};
    OffsetRect(&theRect,SetOriginX,SetOriginY);
    if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
      if (!rgn) {
	wxMacSetCurrentTool(kBrushTool);
	if (paint_brush_with_erase)
	  EraseRect(&theRect);
	else
	  PaintRect(&theRect);
      }
    }
    
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
      wxMacSetCurrentTool(kPenTool);
      AdjustPenRect(current_pen, &theRect, user_scale_x, user_scale_y);
      FrameRect(&theRect);
    }
  }

  ReleaseCurrentDC();
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawRoundedRectangle
(double x, double y, double width, double height, double radius)
{
  wxRegion *rgn;

  if (!Ok() || !cMacDC) return;

  if (radius < 0.0) {
    double w = width;
    if (height < w)
      w = height;
    radius = (-radius) * w;
  }
  
  if (anti_alias) {
    CGContextRef cg;

    SetCurrentDC(TRUE);
    cg = GetCG();

    CGContextSaveGState(cg);

    if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
      double xx, yy, ww, hh, rr, rr2;

      xx = SmoothingXFormXB(x);
      yy = SmoothingXFormYB(y);
      ww = SmoothingXFormW(width, x);
      hh = SmoothingXFormH(height, y);

      rr = SmoothingXFormW(radius, 0);
      rr2 = SmoothingXFormH(radius, 0);
      if (rr2 < rr)
	rr = rr2;

      wxMacSetCurrentTool(kBrushTool);
      CGContextMoveToPoint(cg, xx + rr, yy);
      CGContextAddLineToPoint(cg, xx + ww - rr, yy);
      CGContextAddArc(cg, xx + ww - rr, yy + rr, rr, 1.5 * wxPI, 2 * wxPI, FALSE);
      CGContextAddLineToPoint(cg, xx + ww, yy + hh - rr);
      CGContextAddArc(cg, xx + ww - rr, yy + hh - rr, rr, 0, 0.5 * wxPI, FALSE);
      CGContextAddLineToPoint(cg, xx + rr, yy + hh);
      CGContextAddArc(cg, xx + rr, yy + hh - rr, rr, 0.5 * wxPI, 1.0 * wxPI, FALSE);
      CGContextAddLineToPoint(cg, xx, yy + rr);
      CGContextAddArc(cg, xx + rr, yy + rr, rr, 1.0 * wxPI, 1.5 * wxPI, FALSE);
      CGContextClosePath(cg);
      CGContextFillPath(cg);
    }

    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
      double xx, yy, ww, hh, rr, rr2;

      xx = SmoothingXFormX(x);
      yy = SmoothingXFormY(y);
      ww = SmoothingXFormWL(width, x);
      hh = SmoothingXFormHL(height, y);
      
      rr = SmoothingXFormWL(radius, 0);
      rr2 = SmoothingXFormHL(radius, 0);
      if (rr2 < rr)
	rr = rr2;

      wxMacSetCurrentTool(kPenTool);
      CGContextMoveToPoint(cg, xx + rr, yy);
      CGContextAddLineToPoint(cg, xx + ww - rr, yy);
      CGContextAddArc(cg, xx + ww - rr, yy + rr, rr, 1.5 * wxPI, 2 * wxPI, FALSE);
      CGContextAddLineToPoint(cg, xx + ww, yy + hh - rr);
      CGContextAddArc(cg, xx + ww - rr, yy + hh - rr, rr, 0, 0.5 * wxPI, FALSE);
      CGContextAddLineToPoint(cg, xx + rr, yy + hh);
      CGContextAddArc(cg, xx + rr, yy + hh - rr, rr, 0.5 * wxPI, 1.0 * wxPI, FALSE);
      CGContextAddLineToPoint(cg, xx, yy + rr);
      CGContextAddArc(cg, xx + rr, yy + rr, rr, 1.0 * wxPI, 1.5 * wxPI, FALSE);
      CGContextClosePath(cg);
      CGContextStrokePath(cg);
    }

    wxMacSetCurrentTool(kNoTool);

    CGContextRestoreGState(cg);

    ReleaseCurrentDC();

    return;
  }
 
  if ((rgn = BrushStipple())) {
    rgn->SetRoundedRectangle(x, y, width, height, radius);
    PaintStipple(rgn);
    DELETE_OBJ rgn;
  }

  SetCurrentDC();
  
  {
    int phys_radius = XLOG2DEVREL(radius);
    
    int phys_rwidth = phys_radius * 2;
    int phys_rheight = phys_rwidth;
    
    int top = YLOG2DEV(y);
    int left = XLOG2DEV(x);
    int bottom = YLOG2DEV(y + height);
    int right = XLOG2DEV(x + width);
    Rect theRect = {top, left, bottom, right};

    OffsetRect(&theRect,SetOriginX,SetOriginY);
    
    if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
      if (!rgn) {
	wxMacSetCurrentTool(kBrushTool);
	if (paint_brush_with_erase)
	  EraseRoundRect(&theRect, phys_rwidth, phys_rheight);
	else
	  PaintRoundRect(&theRect, phys_rwidth, phys_rheight);
      }
    }

    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
      wxMacSetCurrentTool(kPenTool);
      AdjustPenRect(current_pen, &theRect, user_scale_x, user_scale_y);
      FrameRoundRect(&theRect, phys_rwidth, phys_rheight);
    }
  }

  ReleaseCurrentDC();
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawEllipse(double x, double y, double width, double height)
{
  wxRegion *rgn;

  if (!Ok() || !cMacDC) return;
  
  if (anti_alias) {
    DrawArc(x, y, width, height, 0, 2 * wxPI);
    return;
  }

  if ((rgn = BrushStipple())) {
    rgn->SetEllipse(x, y, width, height);
    PaintStipple(rgn);
    DELETE_OBJ rgn;
  }

  SetCurrentDC();
  
  {
    int top = YLOG2DEV(y);
    int left = XLOG2DEV(x);
    int bottom = YLOG2DEV(y + height);
    int right = XLOG2DEV(x + width);
    Rect theRect = {top, left, bottom, right};
    OffsetRect(&theRect,SetOriginX,SetOriginY);
    if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
      if (!rgn) {
	wxMacSetCurrentTool(kBrushTool);
	if (paint_brush_with_erase)
	  EraseOval(&theRect);
	else
	  PaintOval(&theRect);
      }
    }

    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
      wxMacSetCurrentTool(kPenTool);
      AdjustPenRect(current_pen, &theRect, user_scale_x, user_scale_y);
      FrameOval(&theRect);
    }
  }

  ReleaseCurrentDC();
}

static int noDCSet = 0; /* back door for GCBlit */

Bool wxCanvasDC::Blit(double xdest, double ydest, double width, double height,
		      wxBitmap *source, double xsrc, double ysrc, int rop, wxColour *c,
		      wxBitmap *mask)
{
  if (!Ok() || !cMacDC || !source->Ok()) return FALSE;
  if (mask && !mask->Ok()) return FALSE;

  if (!noDCSet) {
    SetCurrentDC();
  
    if (source->GetDepth() == 1) {
      wxMacSetCurrentTool(kColorBlitTool);
      if (rop == wxSOLID) BackColor(whiteColor);
      if (c)
	InstallColor(c, TRUE);
      else
	ForeColor(blackColor);
    } else {
      wxMacSetCurrentTool(kBlitTool);
      rop = wxSTIPPLE;
    }
  }

  {
    int mode;
    int ixsrc, iysrc, h, w, x, y;

    if (source->GetDepth() == 1) {
      switch (rop)
	{
	case wxXOR:  
	  mode = srcXor; 
	  break;
	case wxSOLID:
	  mode = srcOr;
	  break;
	case wxSTIPPLE: /* = opaque */
	default:
	  mode = srcCopy;
	  break;
	}
    } else
      mode = srcCopy;
    
    ixsrc = (int)floor(xsrc);
    iysrc = (int)floor(ysrc);
    
    if (ixsrc > source->GetWidth()) {
      if (!noDCSet)
	ReleaseCurrentDC();
      return TRUE;
    }
    if (iysrc > source->GetHeight()) {
      if (!noDCSet)
	ReleaseCurrentDC();
      return TRUE;
    }

    if (iysrc + height > source->GetHeight())
      height = source->GetHeight() - iysrc;
    if (ixsrc + width > source->GetWidth())
      width = source->GetWidth() - ixsrc;

    x = XLOG2DEV(xdest);
    y = YLOG2DEV(ydest);
    h = YLOG2DEV(height + ydest) - y;
    w = XLOG2DEV(width + xdest) - x;

    {
      Rect srcr = {iysrc, ixsrc, iysrc + (int)height, ixsrc + (int)width};
      Rect destr = {y, x, y+h, x+w };
      CGrafPtr theMacGrafPort;
      const BitMap *dstbm;
      const BitMap *srcbm;

      OffsetRect(&destr,SetOriginX,SetOriginY);
      
      theMacGrafPort = cMacDC->macGrafPort();

      dstbm = GetPortBitMapForCopyBits(theMacGrafPort);
      srcbm = GetPortBitMapForCopyBits(source->x_pixmap);
      
      if (mask) {
	const BitMap *maskbm;
	
	maskbm = GetPortBitMapForCopyBits(mask->x_pixmap);

	::CopyDeepMask(srcbm, maskbm, dstbm, &srcr, &srcr, &destr, mode, NULL);
      } else {
	::CopyBits(srcbm, dstbm, &srcr, &destr, mode, NULL);
      }
    }
  }

  if (!noDCSet)
    ReleaseCurrentDC();

  return TRUE;
}

Bool wxCanvasDC::GCBlit(double xdest, double ydest, double width, double height,
			wxBitmap *source, double xsrc, double ysrc)
{
  /* Non-allocating (i.e. no collectable allocation) Blit. Looks like
     the normal one will work, but we need to be careful about shifting the
     current drawing port. So we do the setup manually here and restore it
     completely. */
  Bool isok;
  CGrafPtr savep;
  GDHandle savegd;
  ThemeDrawingState state;
  long ox, oy;
  Rect clientRect = {-32767, -32767, 32767, 32767};
  CGrafPtr theMacGrafPort;
  RgnHandle rgn;

  ::GetGWorld(&savep, &savegd);  

  theMacGrafPort = cMacDC->macGrafPort();
  if (IsPortOffscreen(theMacGrafPort)) {
    ::SetGWorld(theMacGrafPort, NULL);
  } else {
    ::SetGWorld(theMacGrafPort, GetMainDevice());
  }

  ox = SetOriginX;
  oy = SetOriginY;
  SetOriginX = SetOriginY = 0;
  if (canvas) {
    wxArea *area;
    int aw, ah;
    area = canvas->ClientArea();
    area->FrameContentAreaOffset(&SetOriginX, &SetOriginY);
    aw = area->Width();
    ah = area->Height();
    ::SetRect(&clientRect, SetOriginX, SetOriginY, SetOriginX + aw, SetOriginY + ah);
  }

  GetThemeDrawingState(&state);

  noDCSet = 1;

  ForeColor(blackColor);
  BackColor(whiteColor);
  BackPat(GetWhitePattern());
  PenMode(patCopy);

  rgn = NewRgn();
  if (rgn) {
    GetClip(rgn);
    ::ClipRect(&clientRect);
  }

  isok = Blit(xdest, ydest, width, height, source, xsrc, ysrc, wxSTIPPLE, NULL);

  noDCSet = 0;

  if (rgn) {
    SetClip(rgn);
    DisposeRgn(rgn);
  }

  SetThemeDrawingState(state, TRUE);
  SetOriginX = ox;
  SetOriginY = oy;

  ::SetGWorld(savep, savegd);

  if (canvas)
    canvas->FlushDisplay();

  return isok;
}

void wxCanvasDC::TryColour(wxColour *src, wxColour *dest)
{
  if (!Ok() || !cMacDC) return;
  
  SetCurrentDC();

  if (Colour) {
    RGBColor pixel = src->pixel;
    
    Index2Color(Color2Index(&pixel), &pixel);
    
    dest->Set(pixel.red >> 8, pixel.green >> 8, pixel.blue >> 8); 
  } else {
    unsigned char red, blue, green;
    Bool isWhiteColour;

    red = src->Red();
    blue = src->Blue();
    green = src->Green();
    isWhiteColour =
      (red == (unsigned char )255 &&
       blue == (unsigned char)255 &&
       green == (unsigned char)255);
    if (isWhiteColour)
      dest->Set(255, 255, 255);
    else
      dest->Set(0, 0, 0);
  }

  ReleaseCurrentDC();
}
