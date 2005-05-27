
#ifndef _WX_PLT_GDI_H_
#define _WX_PLT_GDI_H_

#ifdef MZ_PRECISE_GC
namespace Gdiplus {
  class Graphics;
  class GraphicsPath;
  class Pen;
  class Brush;
  class Matrix;
};
START_XFORM_SKIP;
#endif

#include <gdiplus.h>
using namespace Gdiplus;

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

#endif
