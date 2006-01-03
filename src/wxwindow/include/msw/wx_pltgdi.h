
#ifndef _WX_PLT_GDI_H_
#define _WX_PLT_GDI_H_

#define GDIP_DECL(x) typedef struct x x

GDIP_DECL(GpGraphics);
GDIP_DECL(GpPath);
GDIP_DECL(GpPen);
GDIP_DECL(GpSolidFill);
GDIP_DECL(GpMatrix);
GDIP_DECL(GpFont);

#define GpBrush GpSolidFill

typedef int GpStatus;

typedef int GpFillMode;
typedef int GpCombineMode;
typedef int GpLineCap;
typedef int GpLineJoin;
typedef int GpPixelOffsetMode;
typedef int GpSmoothingMode;
typedef int GpUnit;

typedef UINT GraphicsState;
typedef float REAL;

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

class GpPointF {
 public:
  REAL X;
  REAL Y;
};

class GpRectF {
 public:
  REAL X;
  REAL Y;
  REAL WIDTH;
  REAL HEIGHT;
};

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

#define LineCapRound  2
#define LineCapSquare 1
#define LineCapFlat   0

#define LineJoinBevel 1
#define LineJoinMiter 0
#define LineJoinRound 2

#define FillModeAlternate 0
#define FillModeWinding   1

#define CombineModeReplace   0
#define CombineModeIntersect 1

#define UnitPoint 3

#define Graphics GpGraphics
#define GraphicsPath GpPath
#define Pen GpPen
#define Brush GpSolidFill
#define Matrix GpMatrix
#define Font GpFont
#define PointF GpPointF
#define RectF GpRectF
#define FillMode GpFillMode
#define CombineMode GpCombineMode
#define LineCap GpLineCap
#define LineJoin GpLineJoin
#define PixelOffsetMode GpPixelOffsetMode
#define SmoothingMode GpSmoothingMode
#define Unit GpUnit

#endif
