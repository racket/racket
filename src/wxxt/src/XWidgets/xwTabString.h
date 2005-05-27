/*
 *	Functions for drawing String's with tab characters in them
 */

#ifdef WX_USE_XFT
# include <X11/Xft/Xft.h>
#endif

#if (NeedFunctionPrototypes > 0)

#ifdef WX_USE_XFT
# define wxExtFont XftFont*
# define wxEXT_FONT(x) x
# define wx_ASCENT(f, xf) (xf ? xf->ascent : f->ascent)
# define wx_DESCENT(f, xf) (xf ? xf->descent : f->descent)
#else
# define wxExtFont void*
# define wxEXT_FONT(x) NULL
# define wx_ASCENT(f, xf) (f->ascent)
# define wx_DESCENT(f, xf) (f->descent)
#endif

extern void	XfwfDrawImageString(Display *display, Drawable drawable,
				    GC gc, int x, int y, String string, int length,
				    int *tabs, XFontStruct *fnt, wxExtFont f, int xon, 
				    Region clip);
extern void     XfwfDrawString(Display *display, Drawable drawable,
			       GC gc, int x, int y, String string, int length,
			       int *tabs, XFontStruct *fnt, wxExtFont f, 
			       int xon, int drawLine, Region clip);
extern int *	XfwfTablist2Tabs(char *tablist);
extern int	XfwfTextWidth(Display *display, XFontStruct *font, wxExtFont f, String str, int length,
			      int *tabs);
extern char *	strnchr(char *s, int c, int n);

extern void wxDrawBitmapLabel(Display *display, 
			      Pixmap pixmap, Pixmap maskmap, 
			      Drawable drawable, GC gc,
			      int x, int y, int width, int height, 
			      int depth, int mask_depth,
			      Region reg,
			      GC gray_gc,
			      Pixel bg_pixel);

#else

extern void	XfwfDrawImageString();
extern void	XfwfDrawString();
extern void	XtabDrawString();
extern int *	XfwfTablist2Tabs();
extern int	XfwfTextWidth();
extern char *	strnchr();

#endif

extern int wx_enough_colors(Screen *s);
