
#include "wx.h"
#include "wx_graphics.h"

Bool wx_gdi_plus = FALSE;

/* ********************************************************************** */

#define ULONG_PTR ULONG * 
#define INT int
#define GDIPCONST const
#define ARGB UINT

typedef int GpMatrixOrder;

#define QualityModeHigh 2
#define SmoothingModeHighQuality QualityModeHigh
#define PixelOffsetModeHalf QualityModeHigh + 2
#define UnitWorld 0
#define MatrixOrderPrepend 0

static inline UINT32 COLORREF_ARGB(UINT c) 
{
  return (0xFF000000
	  | GetRValue(c) << 16
	  | GetGValue(c) << 8
	  | GetBValue(c));
}

/* Typedefs */
#define GDIPLUS_PROC_DECL(name, args) typedef GpStatus (__stdcall * name ## _t) args;
#include "wx_gdiplus.inc"
#undef GDIPLUS_PROC_DECL

/* Function ptrs: */
#define GDIPLUS_PROC_DECL(name, args) static name ## _t  p ## name;
#include "wx_gdiplus.inc"
#undef GDIPLUS_PROC_DECL

/* Function ptrsookup: */
void *GetGgipProcAddress(HMODULE m, char *s)
{
  void *v;
  v = GetProcAddress(m, s);
  if (!v) {
    MessageBox(NULL, s, "GDI+ lookup failed", MB_OK);
  }
  return v;
}
#define GDIPLUS_PROC_DECL(name, args) p ## name = (name ## _t) GetGgipProcAddress(m, #name);
static void GetProcs(HMODULE m) {
# include "wx_gdiplus.inc"
}

#define ck(x) x

/* ********************************************************************** */

void wxInitGraphicsPlus()
{
  HMODULE hm;
  hm = LoadLibrary("gdiplus.dll");
  if (hm) {
    GetProcs(hm);
    wx_gdi_plus = TRUE;
  }
}

/* ********************************************************************** */

Graphics *wxGMake(HDC dc)
{
  Graphics *g;
  ck(pGdipCreateFromHDC(dc, &g));
  ck(pGdipSetSmoothingMode(g, SmoothingModeHighQuality));
  ck(pGdipSetPixelOffsetMode(g, PixelOffsetModeHalf));
  return g;
}

void wxGRelease(Graphics *g)
{
  pGdipDeleteGraphics(g);
}

void wxGSetPageUnit(Graphics *g, Unit u)
{
  ck(pGdipSetPageUnit(g, u));
}

GraphicsState wxGSave(Graphics *g)
{
  GraphicsState s;
  pGdipSaveGraphics(g, &s);
  return s;
}

void wxGRestore(Graphics *g, GraphicsState s)
{
  pGdipRestoreGraphics(g, s);
}

void wxGResetClip(Graphics *g)
{
  ck(pGdipResetClip(g));
}

void wxGSetClip(Graphics *g, GraphicsPath *gp, CombineMode m)
{
  ck(pGdipSetClipPath(g, gp, m));
}

void wxGResetTransform(Graphics *g)
{
  ck(pGdipResetWorldTransform(g));
}

void wxGTranslate(Graphics *g, double x, double y)
{
  ck(pGdipTranslateWorldTransform(g, x, y, MatrixOrderPrepend));
}

void wxGScale(Graphics *g, double x, double y)
{
  ck(pGdipScaleWorldTransform(g, x, y, MatrixOrderPrepend));
}

void wxGDrawLine(Graphics *g, Pen *p, double x1, double y1, double x2, double y2)
{
  pGdipDrawLine(g, p, (REAL)x1, (REAL)y1, (REAL)x2, (REAL)y2);
}

void wxGDrawLines(Graphics *g, Pen *p, PointF *pts, int n)
{
  pGdipDrawLines(g, p, pts, n);
}

void wxGFillRectangleColor(Graphics *g, COLORREF c, double x, double y, double w, double h)
{
  Brush *b;
  ck(pGdipCreateSolidFill(COLORREF_ARGB(c), &b));
  ck(pGdipFillRectangle(g, b, (REAL)x, (REAL)y, (REAL)w, (REAL)h));
  ck(pGdipDeleteBrush(b));
}

void wxGFillRectangle(Graphics *g, Brush *b, double x, double y, double w, double h)
{
  ck(pGdipFillRectangle(g, b, (REAL)x, (REAL)y, (REAL)w, (REAL)h));
}

void wxGDrawRectangle(Graphics *g, Pen *p, double x, double y, double w, double h)
{
  pGdipDrawRectangle(g, p, (REAL)x, (REAL)y, (REAL)w, (REAL)h);
}

void wxGFillPie(Graphics *g, Brush *b, double x, double y, double w, double h, double start, double span)
{
  pGdipFillPie(g, b, (REAL)x, (REAL)y, (REAL)w, (REAL)h, start, span);
}

void wxGDrawArc(Graphics *g, Pen *p, double x, double y, double w, double h, double start, double span)
{
  pGdipDrawArc(g, p, x, y, (REAL)w, (REAL)h, (REAL)start, (REAL)span);
}

void wxGFillPolygon(Graphics *g, Brush *b, PointF *pts, int n, FillMode m)
{
  pGdipFillPolygon(g, b, pts, n, m);
}

void wxGDrawPolygon(Graphics *g, Pen *p, PointF *pts, int n)
{
  pGdipDrawPolygon(g, p, pts, n);
}

void wxGFillPath(Graphics *g, Brush *b, GraphicsPath *gp)
{
  pGdipFillPath(g, b, gp);
}

void wxGDrawPath(Graphics *g, Pen *p, GraphicsPath *gp)
{
  pGdipDrawPath(g, p, gp);
}

GraphicsPath *wxGPathNew(FillMode m)
{
  GraphicsPath *p;
  pGdipCreatePath(m, &p);
  return p;
}

void wxGPathRelease(GraphicsPath *gp)
{
  pGdipDeletePath(gp);
}

void wxGPathAddArc(GraphicsPath *gp, double x, double y, double w, double h, double start, double span)
{
  pGdipAddPathArc(gp, (REAL)x, (REAL)y, (REAL)w, (REAL)h, start, span);
}

void wxGPathAddPie(GraphicsPath *gp, double x, double y, double w, double h, double start, double span)
{
  pGdipAddPathPie(gp, (REAL)x, (REAL)y, (REAL)w, (REAL)h, start, span);
}

void wxGPathAddLine(GraphicsPath *gp, double x1, double y1, double x2, double y2)
{
  pGdipAddPathLine(gp, (REAL)x1, (REAL)y1, (REAL)x2, (REAL)y2);
}

void wxGPathAddBezier(GraphicsPath *gp, double x1, double y1, double x2, double y2,
		      double x3, double y3, double x4, double y4)
{
  pGdipAddPathBezier(gp, (REAL)x1, (REAL)y1, (REAL)x2, (REAL)y2,
		     (REAL)x3, (REAL)y3, (REAL)x4, (REAL)y4);
}

void wxGPathAddPath(GraphicsPath *gp, GraphicsPath *gp2, int conn)
{
  pGdipAddPathPath(gp, gp2, conn);
}

void wxGPathCloseFigure(GraphicsPath *gp)
{
  pGdipClosePathFigure(gp);
}

void wxGPathTransform(GraphicsPath *gp, Matrix *m)
{
  pGdipTransformPath(gp, m);
}

Matrix *wxGMatrixNew()
{
  Matrix *m;
  pGdipCreateMatrix(&m);
  return m;
}

void wxGMatrixRelease(Matrix *m)
{
  pGdipDeleteMatrix(m);
}

void wxGMatrixTranslate(Matrix *m, double x, double y)
{
  pGdipTranslateMatrix(m, (REAL)x, (REAL)y, MatrixOrderPrepend);
}

void wxGMatrixScale(Matrix *m, double x, double y)
{
  pGdipScaleMatrix(m, (REAL)x, (REAL)y, MatrixOrderPrepend);
}

Brush *wxGBrushNew(COLORREF c)
{
  Brush *b;
  pGdipCreateSolidFill(COLORREF_ARGB(c), &b);
  return b;
}

void wxGBrushRelease(Brush *b)
{
  pGdipDeleteBrush(b);
}

Pen *wxGPenNew(COLORREF c, double pw, LineCap cap, LineJoin join, int ndash, REAL *dashes, REAL offset)
{
  Pen *p;

  pGdipCreatePen1(COLORREF_ARGB(c), pw, UnitWorld, &p);
  
  pGdipSetPenEndCap(p, cap);
  pGdipSetPenLineJoin(p, join);
  pGdipSetPenDashOffset(p, offset);
  pGdipSetPenDashArray(p, dashes, ndash);

  return p;
}

void wxGPenRelease(Pen *p)
{
  pGdipDeletePen(p);
}

Font *wxGFontNew(HDC dc)
{
  Font *f;
  pGdipCreateFontFromDC(dc, &f);
  return f;
}

void wxGFontRelease(Font *f)
{
  pGdipDeleteFont(f);
}

/* ********************************************************************** */

static ULONG_PTR           gdiplusToken;


typedef void (*DebugEventProc)();

typedef struct GdiplusStartupInput {
  UINT32 GdiplusVersion;             // Must be 1
  DebugEventProc DebugEventCallback; // Ignored on free builds
  BOOL SuppressBackgroundThread;     // FALSE unless you're prepared to call 
  // the hook/unhook functions properly
  BOOL SuppressExternalCodecs;       // FALSE unless you want GDI+ only to use
  // its internal image codecs.
} GdiplusStartupInput;

void wxGStartup()
{
  GdiplusStartupInput s;
  s.GdiplusVersion = 1;
  s.DebugEventCallback = NULL;
  s.SuppressBackgroundThread = 0;
  s.SuppressExternalCodecs = 0;
  pGdiplusStartup(&gdiplusToken, &s, NULL);
}

void wxGShutdown()
{
  pGdiplusShutdown(gdiplusToken);
}
