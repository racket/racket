
#include "wx.h"
#include "wx_graphics.h"
#include <gdiplus.h>

#include "../../../mzscheme/src/schvers.h"

Bool wx_gdi_plus = FALSE;

/* ********************************************************************** */

/* Declare types to mask cast easier: */

typedef void (*p_wxGStartup)();
typedef void (*p_wxGShutdown)();

typedef Graphics *(*p_wxGMake)(HDC dc);
typedef void (*p_wxGRelease)(Graphics *g);

typedef void (*p_wxGSetPageUnit)(Graphics *g, Unit u);

typedef void (*p_wxGResetClip)(Graphics *g);
typedef void (*p_wxGSetClip)(Graphics *g, GraphicsPath *gp, CombineMode m);

typedef GraphicsState (*p_wxGSave)(Graphics *g);
typedef void (*p_wxGRestore)(Graphics *g, GraphicsState s);

typedef void (*p_wxGResetTransform)(Graphics *g);
typedef void (*p_wxGTranslate)(Graphics *g, double x, double y);
typedef void (*p_wxGScale)(Graphics *g, double x, double y);

typedef void (*p_wxGDrawLine)(Graphics *g, Pen *p, double x1, double y1, double x2, double y2);
typedef void (*p_wxGDrawLines)(Graphics *g, Pen *p, PointF *pts, int n);

typedef void (*p_wxGFillRectangleColor)(Graphics *g, COLORREF c, double x, double y, double w, double h);
typedef void (*p_wxGFillRectangle)(Graphics *g, Brush *b, double x, double y, double w, double h);
typedef void (*p_wxGDrawRectangle)(Graphics *g, Pen *p, double x, double y, double w, double h);

typedef void (*p_wxGFillPie)(Graphics *g, Brush *b, double x, double y, double w, double h, double start, double span);
typedef void (*p_wxGDrawArc)(Graphics *g, Pen *p, double x, double y, double w, double h, double start, double span);

typedef void (*p_wxGFillPolygon)(Graphics *g, Brush *b, PointF *pts, int n, FillMode m);
typedef void (*p_wxGDrawPolygon)(Graphics *g, Pen *p, PointF *pts, int n);

typedef void (*p_wxGFillPath)(Graphics *g, Brush *b, GraphicsPath *gp);
typedef void (*p_wxGDrawPath)(Graphics *g, Pen *p, GraphicsPath *gp);

typedef void (*p_wxGDrawString)(Graphics *g, wchar_t *w, int len, Font *f, PointF *pos, StringFormat *fmt, COLORREF c);
typedef void (*p_wxGMeasureString)(Graphics *g, wchar_t *w, int len, Font *f, PointF *pos, StringFormat *fmt, RectF *r);

typedef GraphicsPath *(*p_wxGPathNew)(FillMode m);
typedef void (*p_wxGPathRelease)(GraphicsPath *gp);

typedef void (*p_wxGPathAddArc)(GraphicsPath *gp, double x, double y, double w, double h, double start, double span);
typedef void (*p_wxGPathAddPie)(GraphicsPath *gp, double x, double y, double w, double h, double start, double span);
typedef void (*p_wxGPathAddLine)(GraphicsPath *gp, double x1, double y1, double x2, double y2);
typedef void (*p_wxGPathAddBezier)(GraphicsPath *gp, double x1, double y1, double x2, double y2,
				   double x3, double y3, double x4, double y4);
typedef void (*p_wxGPathAddPath)(GraphicsPath *gp, GraphicsPath *gp2, int conn);

typedef void (*p_wxGPathCloseFigure)(GraphicsPath *gp);
typedef void (*p_wxGPathTransform)(GraphicsPath *gp, Matrix *m);

typedef Matrix *(*p_wxGMatrixNew)();
typedef void (*p_wxGMatrixRelease)(Matrix *m);
typedef void (*p_wxGMatrixTranslate)(Matrix *m, double x, double y);
typedef void (*p_wxGMatrixScale)(Matrix *m, double x, double y);

typedef Brush *(*p_wxGBrushNew)(COLORREF c);
typedef void (*p_wxGBrushRelease)(Brush *b);

typedef Pen *(*p_wxGPenNew)(COLORREF c, double pw, LineCap cap, LineJoin join, int ndash, REAL *dashes, REAL offset);
typedef void (*p_wxGPenRelease)(Pen *b);

typedef Font *(*p_wxGFontNew)(HDC dc);
typedef void (*p_wxGFontRelease)(Font *f);

typedef StringFormat *(*p_wxGNewStringFormat)(int flags);

/* ********************************************************************** */

/* Declare procedure pointers: */

p_wxGStartup wxGStartup;
p_wxGShutdown wxGShutdown;

p_wxGMake wxGMake;
p_wxGRelease wxGRelease;

p_wxGSetPageUnit wxGSetPageUnit;

p_wxGResetClip wxGResetClip;
p_wxGSetClip wxGSetClip;

p_wxGSave wxGSave;
p_wxGRestore wxGRestore;

p_wxGResetTransform wxGResetTransform;
p_wxGTranslate wxGTranslate;
p_wxGScale wxGScale;

p_wxGDrawLine wxGDrawLine;
p_wxGDrawLines wxGDrawLines;

p_wxGFillRectangleColor wxGFillRectangleColor;
p_wxGFillRectangle wxGFillRectangle;
p_wxGDrawRectangle wxGDrawRectangle;

p_wxGFillPie wxGFillPie;
p_wxGDrawArc wxGDrawArc;

p_wxGFillPolygon wxGFillPolygon;
p_wxGDrawPolygon wxGDrawPolygon;

p_wxGFillPath wxGFillPath;
p_wxGDrawPath wxGDrawPath;

p_wxGDrawString wxGDrawString;
p_wxGMeasureString wxGMeasureString;

p_wxGPathNew wxGPathNew;
p_wxGPathRelease wxGPathRelease;

p_wxGPathAddArc wxGPathAddArc;
p_wxGPathAddPie wxGPathAddPie;
p_wxGPathAddLine wxGPathAddLine;
p_wxGPathAddBezier wxGPathAddBezier;
p_wxGPathAddPath wxGPathAddPath;

p_wxGPathCloseFigure wxGPathCloseFigure;
p_wxGPathTransform wxGPathTransform;

p_wxGMatrixNew wxGMatrixNew;
p_wxGMatrixRelease wxGMatrixRelease;
p_wxGMatrixTranslate wxGMatrixTranslate;
p_wxGMatrixScale wxGMatrixScale;

p_wxGBrushNew wxGBrushNew;
p_wxGBrushRelease wxGBrushRelease;

p_wxGPenNew wxGPenNew;
p_wxGPenRelease wxGPenRelease;

p_wxGFontNew wxGFontNew;
p_wxGFontRelease wxGFontRelease;

p_wxGNewStringFormat wxGNewStringFormat;

/* ********************************************************************** */

/* Init procs: */

static void GetProcs(HMODULE m)
{
  wxGStartup = (p_wxGStartup)GetProcAddress(m, "wxGStartup");
  wxGShutdown = (p_wxGShutdown)GetProcAddress(m, "wxGShutdown");

  wxGMake = (p_wxGMake)GetProcAddress(m, "wxGMake");
  wxGRelease = (p_wxGRelease)GetProcAddress(m, "wxGRelease");

  wxGSetPageUnit = (p_wxGSetPageUnit)GetProcAddress(m, "wxGSetPageUnit");

  wxGResetClip = (p_wxGResetClip)GetProcAddress(m, "wxGResetClip");
  wxGSetClip = (p_wxGSetClip)GetProcAddress(m, "wxGSetClip");

  wxGSave = (p_wxGSave)GetProcAddress(m, "wxGSave");
  wxGRestore = (p_wxGRestore)GetProcAddress(m, "wxGRestore");

  wxGResetTransform = (p_wxGResetTransform)GetProcAddress(m, "wxGResetTransform");
  wxGTranslate = (p_wxGTranslate)GetProcAddress(m, "wxGTranslate");
  wxGScale = (p_wxGScale)GetProcAddress(m, "wxGScale");

  wxGDrawLine = (p_wxGDrawLine)GetProcAddress(m, "wxGDrawLine");
  wxGDrawLines = (p_wxGDrawLines)GetProcAddress(m, "wxGDrawLines");

  wxGFillRectangleColor = (p_wxGFillRectangleColor)GetProcAddress(m, "wxGFillRectangleColor");
  wxGFillRectangle = (p_wxGFillRectangle)GetProcAddress(m, "wxGFillRectangle");
  wxGDrawRectangle = (p_wxGDrawRectangle)GetProcAddress(m, "wxGDrawRectangle");

  wxGFillPie = (p_wxGFillPie)GetProcAddress(m, "wxGFillPie");
  wxGDrawArc = (p_wxGDrawArc)GetProcAddress(m, "wxGDrawArc");

  wxGFillPolygon = (p_wxGFillPolygon)GetProcAddress(m, "wxGFillPolygon");
  wxGDrawPolygon = (p_wxGDrawPolygon)GetProcAddress(m, "wxGDrawPolygon");

  wxGFillPath = (p_wxGFillPath)GetProcAddress(m, "wxGFillPath");
  wxGDrawPath = (p_wxGDrawPath)GetProcAddress(m, "wxGDrawPath");

  wxGDrawString = (p_wxGDrawString)GetProcAddress(m, "wxGDrawString");
  wxGMeasureString = (p_wxGMeasureString)GetProcAddress(m, "wxGMeasureString");

  wxGPathNew = (p_wxGPathNew)GetProcAddress(m, "wxGPathNew");
  wxGPathRelease = (p_wxGPathRelease)GetProcAddress(m, "wxGPathRelease");

  wxGPathAddArc = (p_wxGPathAddArc)GetProcAddress(m, "wxGPathAddArc");
  wxGPathAddPie = (p_wxGPathAddPie)GetProcAddress(m, "wxGPathAddPie");
  wxGPathAddLine = (p_wxGPathAddLine)GetProcAddress(m, "wxGPathAddLine");
  wxGPathAddBezier = (p_wxGPathAddBezier)GetProcAddress(m, "wxGPathAddBezier");
  wxGPathAddPath = (p_wxGPathAddPath)GetProcAddress(m, "wxGPathAddPath");

  wxGPathCloseFigure = (p_wxGPathCloseFigure)GetProcAddress(m, "wxGPathCloseFigure");
  wxGPathTransform = (p_wxGPathTransform)GetProcAddress(m, "wxGPathTransform");

  wxGMatrixNew = (p_wxGMatrixNew)GetProcAddress(m, "wxGMatrixNew");
  wxGMatrixRelease = (p_wxGMatrixRelease)GetProcAddress(m, "wxGMatrixRelease");
  wxGMatrixTranslate = (p_wxGMatrixTranslate)GetProcAddress(m, "wxGMatrixTranslate");
  wxGMatrixScale = (p_wxGMatrixScale)GetProcAddress(m, "wxGMatrixScale");

  wxGBrushNew = (p_wxGBrushNew)GetProcAddress(m, "wxGBrushNew");
  wxGBrushRelease = (p_wxGBrushRelease)GetProcAddress(m, "wxGBrushRelease");

  wxGPenNew = (p_wxGPenNew)GetProcAddress(m, "wxGPenNew");
  wxGPenRelease = (p_wxGPenRelease)GetProcAddress(m, "wxGPenRelease");

  wxGFontNew = (p_wxGFontNew)GetProcAddress(m, "wxGFontNew");
  wxGFontRelease = (p_wxGFontRelease)GetProcAddress(m, "wxGFontRelease");

  wxGNewStringFormat = (p_wxGNewStringFormat)GetProcAddress(m, "wxGNewStringFormat");
}

void wxInitGraphicsPlus()
{
  HMODULE hm;
  hm = LoadLibrary("gdiplus.dll");
  if (hm) {
    char s[40];
    sprintf(s, "pltgdi_%d%d_000000", MZSCHEME_VERSION_MAJOR, MZSCHEME_VERSION_MINOR);
    s[14] = '.';
    s[15] = 'd';
    s[16] = 'l';
    s[17] = 'l';
    s[18] = 0;
    hm = LoadLibraryA(s);
    if (!hm)
      hm = LoadLibraryA("pltgdi_xxxxxxx.dll");
    
    if (hm) {
      GetProcs(hm);
      wx_gdi_plus = TRUE;
    }
  }
}
