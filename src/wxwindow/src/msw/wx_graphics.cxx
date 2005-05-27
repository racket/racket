
#define WX_GRAPHICS_EXPORT extern "C" __declspec(dllexport)
#define WX_GPROC(x) x
#include <windows.h>
#include "wx_graphics.h"

/* This file effectively converts the GDI+ API from C++ to C, to make
   GDI+ easier to use via LoadModule() and GetProcAddress(). */

/* ********************************************************************** */

Graphics *wxGMake(HDC dc)
{
  Graphics *g;
  g = new Graphics(dc);
  g->SetSmoothingMode(SmoothingModeHighQuality);
  g->SetPixelOffsetMode(PixelOffsetModeHalf);
  return g;
}

void wxGRelease(Graphics *g)
{
  delete g;
}

void wxGSetPageUnit(Graphics *g, Unit u)
{
  g->SetPageUnit(u);
}

GraphicsState wxGSave(Graphics *g)
{
  return g->Save();
}

void wxGRestore(Graphics *g, GraphicsState s)
{
  g->Restore(s);
}

void wxGResetClip(Graphics *g)
{
  g->ResetClip();
}

void wxGSetClip(Graphics *g, GraphicsPath *gp, CombineMode m)
{
  g->SetClip(gp, m);
}

void wxGResetTransform(Graphics *g)
{
  g->ResetTransform();
}

void wxGTranslate(Graphics *g, double x, double y)
{
  g->TranslateTransform(x, y);
}

void wxGScale(Graphics *g, double x, double y)
{
  g->ScaleTransform(x, y);
}

void wxGDrawLine(Graphics *g, Pen *p, double x1, double y1, double x2, double y2)
{
  g->DrawLine(p, (REAL)x1, (REAL)y1, (REAL)x2, (REAL)y2);
}

void wxGDrawLines(Graphics *g, Pen *p, PointF *pts, int n)
{
  g->DrawLines(p, pts, n);
}

void wxGFillRectangleColor(Graphics *g, COLORREF c, double x, double y, double w, double h)
{
  Color col(255, GetRValue(c), GetGValue(c), GetBValue(c));
  SolidBrush b(col);
  g->FillRectangle(&b, (REAL)x, (REAL)y, (REAL)w, (REAL)h);
}

void wxGFillRectangle(Graphics *g, Brush *b, double x, double y, double w, double h)
{
  g->FillRectangle(b, (REAL)x, (REAL)y, (REAL)w, (REAL)h);
}

void wxGDrawRectangle(Graphics *g, Pen *p, double x, double y, double w, double h)
{
  g->DrawRectangle(p, (REAL)x, (REAL)y, (REAL)w, (REAL)h);
}

void wxGFillPie(Graphics *g, Brush *b, double x, double y, double w, double h, double start, double span)
{
  g->FillPie(b, (REAL)x, (REAL)y, (REAL)w, (REAL)h, start, span);
}

void wxGDrawArc(Graphics *g, Pen *p, double x, double y, double w, double h, double start, double span)
{
  g->DrawArc(p, x, y, (REAL)w, (REAL)h, (REAL)start, (REAL)span);
}

void wxGFillPolygon(Graphics *g, Brush *b, PointF *pts, int n, FillMode m)
{
  g->FillPolygon(b, pts, n, m);
}

void wxGDrawPolygon(Graphics *g, Pen *p, PointF *pts, int n)
{
  g->DrawPolygon(p, pts, n);
}

void wxGFillPath(Graphics *g, Brush *b, GraphicsPath *gp)
{
  g->FillPath(b, gp);
}

void wxGDrawPath(Graphics *g, Pen *p, GraphicsPath *gp)
{
  g->DrawPath(p, gp);
}

void wxGDrawString(Graphics *g, wchar_t *w, int len, Font *f, PointF *pos, StringFormat *fmt, COLORREF c)
{
  Color col(255, GetRValue(c), GetGValue(c), GetBValue(c));
  SolidBrush b(col);
  g->DrawString(w, len, f, *pos, fmt, &b);
}

void wxGMeasureString(Graphics *g, wchar_t *w, int len, Font *f, PointF *pos, StringFormat *fmt, RectF *r)
{
  g->MeasureString(w, len, f, *pos, fmt, r);
}

GraphicsPath *wxGPathNew(FillMode m)
{
  return new GraphicsPath(m);
}

void wxGPathRelease(GraphicsPath *gp)
{
  delete gp;
}

void wxGPathAddArc(GraphicsPath *gp, double x, double y, double w, double h, double start, double span)
{
  gp->AddArc((REAL)x, (REAL)y, (REAL)w, (REAL)h, start, span);
}

void wxGPathAddPie(GraphicsPath *gp, double x, double y, double w, double h, double start, double span)
{
  gp->AddPie((REAL)x, (REAL)y, (REAL)w, (REAL)h, start, span);
}

void wxGPathAddLine(GraphicsPath *gp, double x1, double y1, double x2, double y2)
{
  gp->AddLine((REAL)x1, (REAL)y1, (REAL)x2, (REAL)y2);
}

void wxGPathAddBezier(GraphicsPath *gp, double x1, double y1, double x2, double y2,
		      double x3, double y3, double x4, double y4)
{
  gp->AddBezier((REAL)x1, (REAL)y1, (REAL)x2, (REAL)y2,
		(REAL)x3, (REAL)y3, (REAL)x4, (REAL)y4);
}

void wxGPathAddPath(GraphicsPath *gp, GraphicsPath *gp2, int conn)
{
  gp->AddPath(gp2, conn);
}

void wxGPathCloseFigure(GraphicsPath *gp)
{
  gp->CloseFigure();
}

void wxGPathTransform(GraphicsPath *gp, Matrix *m)
{
  gp->Transform(m);
}

Matrix *wxGMatrixNew()
{
  return new Matrix();
}

void wxGMatrixRelease(Matrix *m)
{
  delete m;
}

void wxGMatrixTranslate(Matrix *m, double x, double y)
{
  m->Translate((REAL)x, (REAL)y);
}

void wxGMatrixScale(Matrix *m, double x, double y)
{
  m->Scale((REAL)x, (REAL)y);
}

Brush *wxGBrushNew(COLORREF c)
{
  Color col(255, GetRValue(c), GetGValue(c), GetBValue(c));
  return new SolidBrush(col);
}

void wxGBrushRelease(Brush *b)
{
  delete b;
}

Pen *wxGPenNew(COLORREF c, double pw, LineCap cap, LineJoin join, int ndash, REAL *dashes, REAL offset)
{
  Pen *p;
  Color col(255, GetRValue(c), GetGValue(c), GetBValue(c));
  p = new Pen(col, pw);
  p->SetEndCap(cap);
  p->SetLineJoin(join);
  p->SetDashOffset(offset);
  p->SetDashPattern(dashes, ndash);

  return p;
}

void wxGPenRelease(Pen *p)
{
  delete p;
}

Font *wxGFontNew(HDC dc)
{
  return new Font(dc);
}

void wxGFontRelease(Font *f)
{
  delete f;
}

StringFormat *wxGNewStringFormat(int flags)
{
  return new StringFormat();
}

/* ********************************************************************** */

static ULONG_PTR           gdiplusToken;

void wxGStartup()
{
  GdiplusStartupInput gdiplusStartupInput;
  GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);
}

void wxGShutdown()
{
  GdiplusShutdown(gdiplusToken);
}
