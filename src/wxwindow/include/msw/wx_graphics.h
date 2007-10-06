
#include "wx_pltgdi.h"

#define WX_GRAPHICS_EXPORT extern
#define WX_GPROC(x) x

WX_GRAPHICS_EXPORT void WX_GPROC(wxGStartup)();
WX_GRAPHICS_EXPORT void WX_GPROC(wxGShutdown)();

WX_GRAPHICS_EXPORT Graphics *WX_GPROC(wxGMake)(HDC dc);
WX_GRAPHICS_EXPORT void WX_GPROC(wxGRelease)(Graphics *g);

WX_GRAPHICS_EXPORT void WX_GPROC(wxGSetPageUnit)(Graphics *g, Unit u);

WX_GRAPHICS_EXPORT GraphicsState WX_GPROC(wxGSave)(Graphics *g);
WX_GRAPHICS_EXPORT void WX_GPROC(wxGRestore)(Graphics *g, GraphicsState s);

WX_GRAPHICS_EXPORT void WX_GPROC(wxGResetClip)(Graphics *g);
WX_GRAPHICS_EXPORT void WX_GPROC(wxGSetClip)(Graphics *g, GraphicsPath *gp, CombineMode m);

WX_GRAPHICS_EXPORT void WX_GPROC(wxGResetTransform)(Graphics *g);
WX_GRAPHICS_EXPORT void WX_GPROC(wxGTranslate)(Graphics *g, double x, double y);
WX_GRAPHICS_EXPORT void WX_GPROC(wxGScale)(Graphics *g, double x, double y);

WX_GRAPHICS_EXPORT void WX_GPROC(wxGDrawLine)(Graphics *g, Pen *p, double x1, double y1, double x2, double y2);
WX_GRAPHICS_EXPORT void WX_GPROC(wxGDrawLines)(Graphics *g, Pen *p, PointF *pts, int n);

WX_GRAPHICS_EXPORT void WX_GPROC(wxGFillRectangleColor)(Graphics *g, COLORREF c, double x, double y, double w, double h);
WX_GRAPHICS_EXPORT void WX_GPROC(wxGFillRectangle)(Graphics *g, Brush *b, double x, double y, double w, double h);
WX_GRAPHICS_EXPORT void WX_GPROC(wxGDrawRectangle)(Graphics *g, Pen *p, double x, double y, double w, double h);

WX_GRAPHICS_EXPORT void WX_GPROC(wxGFillPie)(Graphics *g, Brush *b, double x, double y, double w, double h, double start, double span);
WX_GRAPHICS_EXPORT void WX_GPROC(wxGDrawArc)(Graphics *g, Pen *p, double x, double y, double w, double h, double start, double span);

WX_GRAPHICS_EXPORT void WX_GPROC(wxGFillPolygon)(Graphics *g, Brush *b, PointF *pts, int n, FillMode m);
WX_GRAPHICS_EXPORT void WX_GPROC(wxGDrawPolygon)(Graphics *g, Pen *p, PointF *pts, int n);

WX_GRAPHICS_EXPORT void WX_GPROC(wxGFillPath)(Graphics *g, Brush *b, GraphicsPath *gp);
WX_GRAPHICS_EXPORT void WX_GPROC(wxGDrawPath)(Graphics *g, Pen *p, GraphicsPath *gp);

WX_GRAPHICS_EXPORT GraphicsPath *WX_GPROC(wxGPathNew)(FillMode m);
WX_GRAPHICS_EXPORT void WX_GPROC(wxGPathRelease)(GraphicsPath *gp);

WX_GRAPHICS_EXPORT void WX_GPROC(wxGPathAddArc)(GraphicsPath *gp, double x, double y, double w, double h, double start, double span);
WX_GRAPHICS_EXPORT void WX_GPROC(wxGPathAddPie)(GraphicsPath *gp, double x, double y, double w, double h, double start, double span);

WX_GRAPHICS_EXPORT void WX_GPROC(wxGPathAddLine)(GraphicsPath *gp, double x1, double y1, double x2, double y2);
WX_GRAPHICS_EXPORT void WX_GPROC(wxGPathAddBezier)(GraphicsPath *gp, double x1, double y1, double x2, double y2,
						   double x3, double y3, double x4, double y4);

WX_GRAPHICS_EXPORT void WX_GPROC(wxGPathAddPath)(GraphicsPath *gp, GraphicsPath *gp2, int conn);
WX_GRAPHICS_EXPORT void WX_GPROC(wxGPathCloseFigure)(GraphicsPath *gp);
WX_GRAPHICS_EXPORT void WX_GPROC(wxGPathTransform)(GraphicsPath *gp, Matrix *m);

WX_GRAPHICS_EXPORT Matrix *WX_GPROC(wxGMatrixNew)();
WX_GRAPHICS_EXPORT void WX_GPROC(wxGMatrixRelease)(Matrix *m);
WX_GRAPHICS_EXPORT void WX_GPROC(wxGMatrixTranslate)(Matrix *m, double x, double y);
WX_GRAPHICS_EXPORT void WX_GPROC(wxGMatrixScale)(Matrix *m, double x, double y);

WX_GRAPHICS_EXPORT Brush *WX_GPROC(wxGBrushNew)(COLORREF c, double alpha);
WX_GRAPHICS_EXPORT void WX_GPROC(wxGBrushRelease)(Brush *b);

WX_GRAPHICS_EXPORT Pen *WX_GPROC(wxGPenNew)(COLORREF c, double alpha, double pw, LineCap cap, LineJoin join, int ndash, REAL *dashes, REAL offset);
WX_GRAPHICS_EXPORT void WX_GPROC(wxGPenRelease)(Pen *b);

WX_GRAPHICS_EXPORT Font *WX_GPROC(wxGFontNew)(HDC dc);
WX_GRAPHICS_EXPORT void WX_GPROC(wxGFontRelease)(Font *f);
