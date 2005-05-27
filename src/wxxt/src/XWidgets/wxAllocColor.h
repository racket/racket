
#ifdef __cplusplus
extern "C" 
{
#endif


extern Status wxAllocColor(Display *d, Colormap cm, XColor *c);
extern int wxQueryColor(Display *display, Colormap colormap, XColor *def_in_out);

#ifdef __cplusplus
}
#endif

#include "wx_visual.h"
