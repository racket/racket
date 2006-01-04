
GDIPLUS_PROC_DECL(GdiplusStartup, (ULONG_PTR *token, const struct GdiplusStartupInput *input, void *output))
GDIPLUS_PROC_DECL(GdiplusShutdown, (ULONG_PTR token))

GDIPLUS_PROC_DECL(GdipCreateFromHDC, (HDC hdc, GpGraphics **graphics))
GDIPLUS_PROC_DECL(GdipDeleteGraphics, (GpGraphics *graphics))

GDIPLUS_PROC_DECL(GdipSetSmoothingMode, (GpGraphics *graphics, SmoothingMode smoothingMode))
GDIPLUS_PROC_DECL(GdipSetPixelOffsetMode, (GpGraphics* graphics, PixelOffsetMode pixelOffsetMode))
GDIPLUS_PROC_DECL(GdipSetPageUnit, (GpGraphics *graphics, GpUnit unit))

GDIPLUS_PROC_DECL(GdipSaveGraphics, (GpGraphics *graphics, GraphicsState *state))
GDIPLUS_PROC_DECL(GdipRestoreGraphics, (GpGraphics *graphics, GraphicsState state))

GDIPLUS_PROC_DECL(GdipResetClip, (GpGraphics *graphics))
GDIPLUS_PROC_DECL(GdipSetClipPath, (GpGraphics *graphics, GpPath *path, CombineMode combineMode))

GDIPLUS_PROC_DECL(GdipResetWorldTransform, (GpGraphics *graphics))
GDIPLUS_PROC_DECL(GdipTranslateWorldTransform, (GpGraphics *graphics, REAL dx, REAL dy, GpMatrixOrder order))
GDIPLUS_PROC_DECL(GdipScaleWorldTransform, (GpGraphics *graphics, REAL sx, REAL sy, GpMatrixOrder order))

GDIPLUS_PROC_DECL(GdipDrawLine, (GpGraphics *graphics, GpPen *pen, REAL x1, REAL y1, REAL x2, REAL y2))
GDIPLUS_PROC_DECL(GdipDrawLines, (GpGraphics *graphics, GpPen *pen, GDIPCONST GpPointF *points, INT count))

GDIPLUS_PROC_DECL(GdipFillRectangle, (GpGraphics *graphics, GpBrush *brush, REAL x, REAL y, REAL width, REAL height))
GDIPLUS_PROC_DECL(GdipDrawRectangle, (GpGraphics *graphics, GpPen *pen, REAL x, REAL y, REAL width, REAL height))

GDIPLUS_PROC_DECL(GdipFillPie, (GpGraphics *graphics, GpBrush *brush, REAL x, REAL y, REAL width, REAL height, REAL startAngle, REAL sweepAngle))
GDIPLUS_PROC_DECL(GdipDrawArc, (GpGraphics *graphics, GpPen *pen, REAL x, REAL y, REAL width, REAL height, REAL startAngle, REAL sweepAngle))

GDIPLUS_PROC_DECL(GdipFillPolygon, (GpGraphics *graphics, GpBrush *brush, GDIPCONST GpPointF *points, INT count, GpFillMode fillMode))
GDIPLUS_PROC_DECL(GdipDrawPolygon, (GpGraphics *graphics, GpPen *pen, GDIPCONST GpPointF *points, INT count))

GDIPLUS_PROC_DECL(GdipFillPath, (GpGraphics *graphics, GpBrush *brush, GpPath *path))
GDIPLUS_PROC_DECL(GdipDrawPath, (GpGraphics *graphics, GpPen *pen, GpPath *path))

GDIPLUS_PROC_DECL(GdipCreatePath, (GpFillMode brushMode, GpPath **path))
GDIPLUS_PROC_DECL(GdipDeletePath, (GpPath* path))

GDIPLUS_PROC_DECL(GdipAddPathArc, (GpPath *path, REAL x, REAL y, REAL width, REAL height, REAL startAngle, REAL sweepAngle))
GDIPLUS_PROC_DECL(GdipAddPathPie, (GpPath *path, REAL x, REAL y, REAL width, REAL height, REAL startAngle, REAL sweepAngle))
GDIPLUS_PROC_DECL(GdipAddPathLine, (GpPath *path, REAL x1, REAL y1, REAL x2, REAL y2))
GDIPLUS_PROC_DECL(GdipAddPathBezier, (GpPath *path, REAL x1, REAL y1, REAL x2, REAL y2, REAL x3, REAL y3, REAL x4, REAL y4))
GDIPLUS_PROC_DECL(GdipAddPathPath, (GpPath *path, GDIPCONST GpPath* addingPath, BOOL connect))

GDIPLUS_PROC_DECL(GdipClosePathFigure, (GpPath *path))
GDIPLUS_PROC_DECL(GdipTransformPath, (GpPath* path, GpMatrix* matrix))

GDIPLUS_PROC_DECL(GdipCreateMatrix, (GpMatrix **matrix))
GDIPLUS_PROC_DECL(GdipDeleteMatrix, (GpMatrix *matrix))
GDIPLUS_PROC_DECL(GdipTranslateMatrix, (GpMatrix *matrix, REAL offsetX, REAL offsetY, GpMatrixOrder order))
GDIPLUS_PROC_DECL(GdipScaleMatrix, (GpMatrix *matrix, REAL scaleX, REAL scaleY, GpMatrixOrder order))

GDIPLUS_PROC_DECL(GdipCreateSolidFill, (ARGB color, GpSolidFill **brush))
GDIPLUS_PROC_DECL(GdipDeleteBrush, (GpBrush *brush))

GDIPLUS_PROC_DECL(GdipCreatePen1, (ARGB color, REAL width, GpUnit unit, GpPen **pen))
GDIPLUS_PROC_DECL(GdipDeletePen, (GpPen *pen))

GDIPLUS_PROC_DECL(GdipSetPenEndCap, (GpPen *pen, GpLineCap endCap))
GDIPLUS_PROC_DECL(GdipSetPenLineJoin, (GpPen *pen, GpLineJoin lineJoin))
GDIPLUS_PROC_DECL(GdipSetPenDashOffset, (GpPen *pen, REAL offset))
GDIPLUS_PROC_DECL(GdipSetPenDashArray, (GpPen *pen, GDIPCONST REAL *dash, INT count))

GDIPLUS_PROC_DECL(GdipCreateFontFromDC, ( HDC hdc, GpFont **font))
GDIPLUS_PROC_DECL(GdipDeleteFont, (GpFont* font))
