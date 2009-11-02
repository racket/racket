/*								-*- C++ -*-
 *
 * Purpose: device context to draw drawables
 *          (windows and pixmaps, even if pixmaps are covered by wxMemoryDC)
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 2004-2009 PLT Scheme Inc.
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */

#ifdef __GNUG__
#pragma implementation "WindowDC.h"
#endif

#define  Uses_XLib
#define  Uses_wxWindowDC
#define  Uses_wxMemoryDC
#define  Uses_wxList
#include "wx.h"

#ifdef USE_GL
# include <GL/glx.h>
# include <X11/Xcms.h>
#endif
#include "../../../wxcommon/wxGLConfig.h"

#ifdef WX_USE_XRENDER
# include <X11/Xcms.h>
# include <X11/extensions/Xrender.h>
# ifdef WX_USE_XFT
#  include <X11/Xft/Xft.h>
# endif
#endif
#include <X11/Intrinsic.h>

#include "../wx_cairo.h"

#define  UseXtRegions
#include "wx_rgn.h"

#include <math.h>
#include <string.h>

extern "C" { 
#include "XWidgets/wxAllocColor.h"
#include "XWidgets/xwTabString.h"
extern int wx_alloc_color_is_fast;
extern int wx_simple_r_start, wx_simple_g_start, wx_simple_b_start;
};
#include "wx_visual.h"

// constant to convert radian to degree
#define RAD2DEG 57.2957795131

// shift between wxWindows RGB- and XColor RGB-values
// (necessary because the values specify an intensity)
#define SHIFT (8*(sizeof(short int)-sizeof(char)))

// translate defines from wxDefines.h to XLib constants
static int join_style[] = { JoinBevel, JoinMiter, JoinRound };
static int cap_style[]  = { CapRound, CapProjecting, CapButt, CapNotLast };
static int fill_rule[]  = { EvenOddRule, WindingRule };

#ifdef WX_USE_CAIRO
static cairo_line_join_t c_join_style[] = { CAIRO_LINE_JOIN_BEVEL, CAIRO_LINE_JOIN_MITER, CAIRO_LINE_JOIN_ROUND };
static cairo_line_cap_t c_cap_style[]  = { CAIRO_LINE_CAP_ROUND, CAIRO_LINE_CAP_SQUARE, CAIRO_LINE_CAP_BUTT, CAIRO_LINE_CAP_BUTT };
#endif

// hatches, used for stippling in any DC
#include <DeviceContexts/bdiag.xbm>
#include <DeviceContexts/fdiag.xbm>
#include <DeviceContexts/cdiag.xbm>
#include <DeviceContexts/horiz.xbm>
#include <DeviceContexts/verti.xbm>
#include <DeviceContexts/cross.xbm>
#define  num_hatches 6

// This normalizes the graphics code to behave inm a standard way when
// WX_STANDARD_GRAPHICS is 1.
#if WX_STANDARD_GRAPHICS
#define WX_GC_CF 1
#else
#define WX_GC_CF 0
#endif

#define IS_COLOR (DEPTH > 1)

static Pixmap* hatch_bitmaps = NULL;

#undef GETCOLORMAP
#define GETCOLORMAP(x) wx_default_colormap

#define wxMINI_SIZE 8

#define wxPI 3.141592653589793

//-----------------------------------------------------------------------------
// create and destroy wxWindowDC
//-----------------------------------------------------------------------------

wxWindowDC::wxWindowDC(void) : wxDC()
{
    __type = wxTYPE_DC_CANVAS;

    device = wxDEVICE_CANVAS;

    X = new wxWindowDC_Xintern; // allocate space for X data

    PEN_GC = BRUSH_GC = TEXT_GC = BG_GC = NULL;
    USER_REG = EXPOSE_REG = CURRENT_REG = NULL;
    DPY = NULL;
    SCN = NULL;
    DRAWABLE = 0;
    DRAW_WINDOW = 0;
    WIDTH = HEIGHT = DEPTH = 0;

    X->get_pixel_image_cache = NULL;

    if (!hatch_bitmaps) {
	Display *dpy = wxAPP_DISPLAY;
	Window  win  = RootWindow(dpy, DefaultScreen(dpy));
	wxREGGLOB(hatch_bitmaps);
	hatch_bitmaps = new WXGC_ATOMIC Pixmap[num_hatches];
	hatch_bitmaps[0] = XCreateBitmapFromData(dpy, win, bdiag_bits,
						 bdiag_width, bdiag_height);
	hatch_bitmaps[1] = XCreateBitmapFromData(dpy, win, cdiag_bits,
						 cdiag_width, cdiag_height);
	hatch_bitmaps[2] = XCreateBitmapFromData(dpy, win, fdiag_bits,
						 fdiag_width, fdiag_height);
	hatch_bitmaps[3] = XCreateBitmapFromData(dpy, win, cross_bits,
						 cross_width, cross_height);
	hatch_bitmaps[4] = XCreateBitmapFromData(dpy, win, horiz_bits,
						 horiz_width, horiz_height);
	hatch_bitmaps[5] = XCreateBitmapFromData(dpy, win, verti_bits,
						 verti_width, verti_height);
    }

    current_background_color->CopyFrom(wxWHITE);
    current_brush = wxWHITE_BRUSH;
    current_brush->Lock(1);
    current_pen = wxBLACK_PEN;
    current_pen->Lock(1);
    current_font = wxNORMAL_FONT;

    need_x_set_font = 1;
}

wxWindowDC::~wxWindowDC(void)
{
    if (current_pen) current_pen->Lock(-1);
    if (current_brush) current_brush->Lock(-1);
    if (clipping) --clipping->locked;

    Destroy();

#ifdef USE_GL
    X->wx_gl = NULL;
#endif
}

#ifdef USE_GL
wxGL *wxWindowDC::GetGL()
{
  wxGL *gl;

  if (X->wx_gl)
    return X->wx_gl;

  gl = new wxGL();
  X->wx_gl = gl;

  if (DRAWABLE) {
    gl->Reset(X->gl_cfg, (long)DRAWABLE, __type == wxTYPE_DC_MEMORY);
  }

  return gl;
}

void wxWindowDC::SetGLConfig(wxGLConfig *cfg)
{
  if (cfg)
    cfg = cfg->Clone();
  X->gl_cfg = cfg;
}
#endif

//-----------------------------------------------------------------------------
// drawing methods
//-----------------------------------------------------------------------------

static wxBitmap *ScaleBitmap(wxBitmap *src, 
			     int tw, int th,
			     double xsrc, double ysrc, double w, double h, 
			     Display *dpy, 
			     wxBitmap **_tmp, int *retval,
			     int forceMono, unsigned long whiteVal)
{
  int sw, sh, i, j, ti, tj, xs, ys, mono;
  unsigned long pixel;
  wxBitmap *tmp;
  double scale_x, scale_y;

  *retval = TRUE;

  xs = (int)xsrc;
  ys = (int)ysrc;
  
  sw = src->GetWidth();
  sh = src->GetHeight();

  if (xs > sw)
    return NULL;
  if (ys > sh)
    return NULL;

  if (sw > w)
    sw = (int)w;
  if (sh > h)
    sh = (int)h;

  scale_x = (double)tw / sw;
  scale_y = (double)th / sh;

  mono = (src->GetDepth() == 1);
  if (forceMono && !mono)
    mono = 1;
  else
    forceMono = 0;
  tmp = new wxBitmap(tw, th, mono);
  *_tmp = tmp;
      
  if (tmp->Ok()) {
    XImage *simg, *timg;
    XGCValues values;
    GC agc;
    Pixmap spm, tpm;
    
    if (src->selectedTo)
      src->selectedTo->EndSetPixel();
    
    spm = GETPIXMAP(src);
    simg = XGetImage(dpy, spm, xs, ys, sw, sh, AllPlanes, ZPixmap);
    tpm = GETPIXMAP(tmp);
    timg = XGetImage(dpy, tpm, 0, 0, tw, th, AllPlanes, ZPixmap);

    for (ti = 0; ti < tw; ti++) {
      for (tj = 0; tj < th; tj++) {
	i = (int)(ti / scale_x);
	j = (int)(tj / scale_y);
	pixel = XGetPixel(simg, i, j);
	if (forceMono) {
	  if (pixel == whiteVal)
	    pixel = 0;
	  else
	    pixel = 1;
	}
	XPutPixel(timg, ti, tj, pixel);
      }
    }

    agc = XCreateGC(dpy, tpm, 0, &values);
    if (agc) {
      XPutImage(dpy, tpm, agc, timg, 0, 0, 0, 0, tw, th);
      XFreeGC(dpy, agc);
      *retval = 1;
    } else
      *retval = 0;

    XDestroyImage(simg);
    XDestroyImage(timg);

    xsrc = ysrc = 0;
    src = tmp;

    if (!*retval) {
      DELETE_OBJ tmp;
      *retval = FALSE;
      return NULL;
    }
  } else {
    DELETE_OBJ tmp;
    *retval = FALSE;
    return NULL;
  }

  return src;
}

static wxBitmap *IntersectBitmapRegion(GC agc, Region user_reg, Region expose_reg, wxBitmap *bmask, 
				       Region *_free_rgn,
				       int *_tx, int *_ty,
				       int *_scaled_width, int *_scaled_height,
				       double *_xsrc, double *_ysrc,
				       Display *dpy, unsigned long whiteVal)
{
  int overlap;
  Region free_rgn = *_free_rgn, rgn = NULL;
  int tx = *_tx, ty = *_ty;
  int scaled_width = *_scaled_width, scaled_height = *_scaled_height;
  double xsrc = *_xsrc, ysrc = *_ysrc;

  if (user_reg || expose_reg) {
    if (user_reg && expose_reg) {
      rgn = XCreateRegion();
      free_rgn = rgn;
      XIntersectRegion(expose_reg, user_reg, rgn);
    } else if (user_reg)
      rgn = user_reg;
    else
      rgn = expose_reg;

    if (bmask) {
      overlap = XRectInRegion(rgn, tx, ty, scaled_width, scaled_height);

      if (overlap == RectangleIn) {
	/* Overwriting the region later will be fine. */
	rgn = NULL;
      } else if (overlap == RectangleOut) {
	/* Mask will have no effect - all drawing is masked anyway */
	bmask = NULL;
      } else {
	/* This is the difficult case: mask bitmap and region combine. */
	XRectangle encl;
	long tx2, ty2, sw2, sh2;

	/* Common case: rgn is a sub-rectangle of the target: */
	XClipBox(rgn, &encl);
	tx2 = max(encl.x, tx);
	ty2 = max(encl.y, ty);
	sw2 = min(encl.x + encl.width, tx + scaled_width) - tx2;
	sh2 = min(encl.y + encl.height, ty + scaled_height) - ty2;
	
	if (XRectInRegion(rgn, tx2, ty2, sw2, sh2) == RectangleIn) {
	  xsrc += (tx2 - tx);
	  ysrc += (ty2 - ty);
	  tx = tx2;
	  ty = ty2;
	  scaled_width = sw2;
	  scaled_height = sh2;
	  /* clipping is essentially manual, now: */
	  rgn = NULL;
	} else {
	  /* This is as complex as it gets. */
	  /* We create a new region with the pixels of the bitmap. */
	  Pixmap bpm;
	  XImage *simg;
	  Region bmrgn;
	  int mi, mj;
	  
	  bpm = GETPIXMAP(bmask);
	  simg = XGetImage(dpy, bpm, (long)xsrc, (long)ysrc, scaled_width, scaled_height, AllPlanes, ZPixmap);
	  
	  bmrgn = XCreateRegion();

	  if (bmask->GetDepth() == 1)
	    whiteVal = 0;
	  
	  for (mj = 0; mj < scaled_height; mj++) {
	    encl.y = mj + ty;
	    encl.height = 1;
	    encl.width = 0;
	    for (mi = 0; mi < scaled_width; mi++) {
	      if (XGetPixel(simg, mi + (long)xsrc, mj + (long)ysrc) == whiteVal) {
		if (encl.width) {
		  XUnionRectWithRegion(&encl, bmrgn, bmrgn);
		  encl.width = 0;
		}
	      } else {
		if (!encl.width)
		  encl.x = mi + tx;
		encl.width++;
	      }
	    }

	    if (encl.width)
	      XUnionRectWithRegion(&encl, bmrgn, bmrgn);
	  }
	  
	  if (!free_rgn) {
	    free_rgn = XCreateRegion();
	    XUnionRegion(free_rgn, rgn, free_rgn);
	    rgn = free_rgn;
	  }

	  /* Insert the bitmap-based region with the old one: */
	  XIntersectRegion(bmrgn, rgn, rgn);
	  XDestroyRegion(bmrgn);

	  XDestroyImage(simg);

	  bmask = NULL; /* done via rgn */
	}
      }
    }
  }

  if (rgn)
    XSetRegion(dpy, agc, rgn);

  if (bmask) {
    Pixmap mpm;
    int monoized = 0;

    if (bmask->GetDepth() != 1) {
      int ok;
      wxBitmap *tmp;
      monoized = 1;
      bmask = ScaleBitmap(bmask, bmask->GetWidth(), bmask->GetHeight(), 
			  0, 0, 
			  bmask->GetWidth(), bmask->GetHeight(),
			  dpy, &tmp, &ok,
			  1, whiteVal);
      mpm = GETPIXMAP(bmask);
      bmask = tmp;
    } else
      mpm = GETPIXMAP(bmask);

    XSetClipMask(dpy, agc, mpm);
    XSetClipOrigin(dpy, agc, tx - (long)xsrc, ty - (long)ysrc);

    if (!monoized)
      bmask = NULL; /* => no special mask to free */
  }
  
  *_free_rgn = free_rgn;
  *_tx = tx;
  *_ty = ty;
  *_scaled_width = scaled_width;
  *_scaled_height = scaled_height,
  *_xsrc = xsrc;
  *_ysrc = ysrc;

  return bmask;
}

#ifdef WX_USE_XRENDER
static int xrender_here = -1;

int wxXRenderHere(void)
{
  if (xrender_here < 0) {
    /* Check whether Xrender is present at run time */
    int event_base, error_base;
    if (XRenderQueryExtension(wxAPP_DISPLAY, &event_base, &error_base) &&
	(XRenderFindVisualFormat(wxAPP_DISPLAY, wxAPP_VISUAL) != 0)) {
      xrender_here = 1;
    } else
      xrender_here = 0;
  }

  return xrender_here;
}

#define WX_RENDER_CAN_SCALE 1
#if RENDER_MAJOR < 1
# if RENDER_MINOR < 6
#  undef WX_RENDER_CAN_SCALE
#  define WX_RENDER_CAN_SCALE 0
# endif
#endif

#ifndef WX_USE_XFT
# define WX_XR_PICTURE
#else
# ifndef XFT_MAJOR
/* No XftDrawPicture, so still use direct Xrender interface */
#  define WX_OLD_XFT
#  define WX_XR_PICTURE
# endif
#endif

static XRenderPictFormat *format, *mask_format, *alpha_format;

long wxMakeXrenderPicture(Drawable d, int color)
{
  /* Create format records, if not done already: */
  if (!format) {
    XRenderPictFormat pf;
    
    format = XRenderFindVisualFormat(wxAPP_DISPLAY, wxAPP_VISUAL);
    
    pf.type = PictTypeDirect;
    pf.depth = 1;
    pf.direct.alpha = 0;
    pf.direct.alphaMask = 1;
    mask_format = XRenderFindFormat (wxAPP_DISPLAY,
				     (PictFormatType|PictFormatDepth|PictFormatAlpha|PictFormatAlphaMask),
				     &pf,
				     0);

    pf.type = PictTypeDirect;
    pf.depth = 8;
    pf.direct.alpha = 0;
    pf.direct.alphaMask = 0xFF;
    alpha_format = XRenderFindFormat (wxAPP_DISPLAY,
				      (PictFormatType|PictFormatDepth|PictFormatAlpha|PictFormatAlphaMask),
				      &pf,
				      0);
  }

  return (long)XRenderCreatePicture(wxAPP_DISPLAY,
				    d,
				    color ? format : mask_format,
				    0,
				    NULL);
}

long wxMakePicture(Drawable d, int color)
{
#ifdef WX_USE_XFT
  if (color) {
    return (long)XftDrawCreate(wxAPP_DISPLAY, d, wxAPP_VISUAL, wx_default_colormap);
  } else 
    return (long)XftDrawCreateBitmap(wxAPP_DISPLAY, d);
#else
  return wxMakeXrenderPicture(d, color);
#endif
}

void wxWindowDC::InitPicture() 
{
  if (!X->picture) {
    unsigned long p;
    p = wxMakePicture(DRAWABLE, Colour);
    X->picture = p;
    InitPictureClip();
  }
}

void wxWindowDC::InitPictureClip()
{
# ifdef WX_USE_XFT
  if (CURRENT_REG)
    XftDrawSetClip(XFTDRAW, CURRENT_REG);
# endif
}

void wxFreePicture(long p) {
# ifdef WX_USE_XFT
  if (p) {
    XftDrawDestroy((XftDraw *)p);
  }
# else
  if (p) {
    XRenderFreePicture(wxAPP_DISPLAY, (Picture)p);
  }
# endif
}
#endif

#ifndef WX_RENDER_CAN_SCALE
# define WX_RENDER_CAN_SCALE 0
#endif

Bool wxWindowDC::Blit(double xdest, double ydest, double w, double h, wxBitmap *src,
		      double xsrc, double ysrc, int rop, wxColor *dcolor, wxBitmap *mask)
{
    Bool retval = FALSE;
    wxPen *savePen, *apen;
    wxColor *saveBack;
    int scaled_width;
    int scaled_height;
    int tx, ty;
    wxBitmap *tmp = NULL, *tmp_mask = NULL;
    int should_xrender = 0;

    if (!DRAWABLE) // ensure that a drawable has been associated
      return FALSE;
    
    if (!src->Ok())
      return FALSE;

    if (src->selectedTo)
      src->selectedTo->EndSetPixel();
    if (mask && mask->selectedTo)
      mask->selectedTo->EndSetPixel();

#ifdef WX_USE_XRENDER
    /* Decide whether to use Xrender before scaling...

       Non-monochrome masks require the Xrender extension. We expect
       that Xrender it's also better when we have a monochrome mask.

       Use Xrender extension when:

         - It's available at compile time and run time
	 - There's a mask or scale
	 - One of:
             * the rop is ignored (because the src is not mono)
             * the rop is wxSOLID (not wxSTIPPLE, which means "opaque")
	 - One of:
             * the color is ignored (because the src is not mono)
	     * the color is black/unspecified

      Under other circumstances, it doesn't work right. */
    
    
    
    should_xrender= (wxXRenderHere()
		     && (mask 
			 || (WX_RENDER_CAN_SCALE && ((user_scale_x != 1.0) || (user_scale_y != 1.0))))
		     && ((src->GetDepth() > 1)
			 || ((rop == wxSOLID)
			     && (!dcolor || (!dcolor->Red() && !dcolor->Green() && !dcolor->Blue())))));

#endif

    tx = (int)XLOG2DEV(xdest);
    ty = (int)YLOG2DEV(ydest);

    w = ((src->GetWidth()  < w) ? src->GetWidth() : w);
    h = ((src->GetHeight() < h) ? src->GetHeight() : h);

    scaled_width = (int)XLOG2DEV(xdest + w) - tx;
    scaled_height = (int)YLOG2DEV(ydest + h) - ty;

    /* Handle scaling by creating a new, temporary bitmap: */
    if ((!should_xrender || !WX_RENDER_CAN_SCALE)
	&& ((scaled_width != (int)w) || (scaled_height != (int)h))) {
      int retval;
      src = ScaleBitmap(src, scaled_width, scaled_height, xsrc, ysrc, w, h, DPY, &tmp, &retval, 0, 0);
      if (!src)
	return retval;
      if (mask) {
	mask = ScaleBitmap(mask, scaled_width, scaled_height, xsrc, ysrc, w, h, DPY, &tmp_mask, &retval, 
			   !should_xrender, wx_white_pixel);
	if (!mask) {
	  DELETE_OBJ tmp;
	  return retval;
	}
      }

      /* Temp bitmaps use only the relevant section: */
      xsrc = 0;
      ysrc = 0;
    }

    xsrc = floor(xsrc);
    ysrc = floor(ysrc);
    
#ifdef WX_USE_XRENDER
    if (should_xrender) {
      /* Using Xrender... */
      Picture destp, srcp, maskp;
      wxBitmap *free_bmp = NULL;
      int mono_src;
# ifndef WX_XR_PICTURE
      XftDraw *maskd = NULL;
# endif

      mono_src = (src->GetDepth() == 1);

      /* Create an Xrender picture for each X pixmap.
	 With old XFT version, give up on caching Picture. */
# ifdef WX_OLD_XFT
      destp = wxMakeXrenderPicture(DRAWABLE, Colour);
# else
      if (!X->picture)
	InitPicture();
      destp = PICTURE;
# endif

# ifdef WX_OLD_XFT
      {
	int sd;
	Pixmap spm;
	spm = GETPIXMAP(src);
	sd = src->GetDepth();
	srcp = wxMakeXrenderPicture(spm, sd != 1);
      }
# else
      {
	long p;
	p = src->GetPicture();
	srcp = TO_PICTURE(p);
      }
# endif

      /* Mask case is more difficult if it's not 1 bit: */
      if (mask) {
	if (mask->GetDepth() == 1) {
	  /* Easy: */
# ifdef WX_OLD_XFT
	  Pixmap mpm;
	  mpm = GETPIXMAP(mask);
	  maskp = wxMakeXrenderPicture(mpm, 0);
# else
	  long p;
	  p = mask->GetPicture();
	  maskp = TO_PICTURE(p);
# endif
	} else {
	  /* Need an 8-bit alpha (grayscale) mask. */
	  wxBitmap *bm;

	  bm = mask->GetMaskBit();
	  if (bm) {
	    Pixmap bpm;
	    
	    bpm = GETPIXMAP(bm);

# ifdef WX_XR_PICTURE
	    maskp = XRenderCreatePicture(wxAPP_DISPLAY,
					 bpm,
					 alpha_format,
					 0,
					 NULL);
# else
	    maskd = XftDrawCreateAlpha(wxAPP_DISPLAY,
				       bpm,
				       8);
	    maskp = TO_PICTURE(maskd);
# endif
	    
	    free_bmp = bm;
	  } else
	    maskp = 0;
	}
      } else
	maskp = 0;

# ifdef WX_XR_PICTURE
      /* Need to install clipping region, if any: */
      if (CURRENT_REG) {
	XRenderSetPictureClipRegion(wxAPP_DISPLAY, destp, CURRENT_REG);
      }
# endif

# if WX_RENDER_CAN_SCALE
      if ((scaled_width != (int)w) || (scaled_height != (int)h)) {
 	XTransform xform;
	int ih = (int)h, iw = (int)w;

	xform.matrix[0][0] = scaled_height * iw;
	xform.matrix[0][1] = 0;
	xform.matrix[0][2] = 0;

	xform.matrix[1][0] = 0;
	xform.matrix[1][1] = scaled_width * ih; 
	xform.matrix[1][2] = 0;

	xform.matrix[2][0] = 0;
	xform.matrix[2][1] = 0;
	xform.matrix[2][2] = scaled_width * scaled_height; 

	XRenderSetPictureTransform(wxAPP_DISPLAY, srcp, &xform);
	if (maskp)
	  XRenderSetPictureTransform(wxAPP_DISPLAY, maskp, &xform);
      }
# endif

      /* This is the actual blit. */
      XRenderComposite(wxAPP_DISPLAY,
		       (mask || mono_src) ? PictOpOver : PictOpSrc,
		       srcp,
		       mask ? maskp : (mono_src ? srcp : 0),
		       destp,
		       (long)xsrc, (long)ysrc,
		       (long)xsrc, (long)ysrc,
		       tx, ty,
		       scaled_width,
		       scaled_height);
      
      retval = 1; /* or so we assume */

      /* Free temporary data (old Xft) */
# ifdef WX_OLD_XFT
      XRenderFreePicture(wxAPP_DISPLAY, destp);
      XRenderFreePicture(wxAPP_DISPLAY, srcp);
      srcp = 0;
      if (!free_bmp) {
	XRenderFreePicture(wxAPP_DISPLAY, maskp);
	maskp = 0;
      }
# else
#  ifdef WX_XR_PICTURE
      if (CURRENT_REG) {
	XRenderPictureAttributes attribs;
	attribs.clip_mask = None;
	XRenderChangePicture(wxAPP_DISPLAY, destp, CPClipMask, &attribs);
      }
#  endif
# endif

      /* Free temporary data (all modes) */
      if (free_bmp) {
# ifdef WX_XR_PICTURE
	XRenderFreePicture(wxAPP_DISPLAY, maskp);
# else
	XftDrawDestroy(maskd);	
# endif
	maskp = 0;
      }

# if WX_RENDER_CAN_SCALE
      if ((srcp || maskp) 
	  && ((scaled_width != (int)w) || (scaled_height != (int)h))) {
	XTransform xform;

	xform.matrix[0][0] = 1;
	xform.matrix[0][1] = 0;
	xform.matrix[0][2] = 0;

	xform.matrix[1][0] = 0;
	xform.matrix[1][1] = 1;
	xform.matrix[1][2] = 0;

	xform.matrix[2][0] = 0;
	xform.matrix[2][1] = 0;
	xform.matrix[2][2] = 1;

	if (srcp)
	  XRenderSetPictureTransform(wxAPP_DISPLAY, srcp, &xform);
	if (maskp)
	  XRenderSetPictureTransform(wxAPP_DISPLAY, maskp, &xform);
      }
# endif
    } else {
#endif
      /* Non-Xrender mode... */

      if (src->GetDepth() > 1) {
	/* This is color to mono/color.
	   Neither rop nor dcolor matter, so use GCBlit. */
	retval = GCBlit(xdest, ydest, w, h, src, xsrc, ysrc, mask);
      } else {

	/* This is mono to mono/color */

	FreeGetPixelCache();

	savePen = current_pen;
	saveBack = new wxColour(current_background_color);
	/* Pen GC used for blit: */
	apen = wxThePenList->FindOrCreatePen(dcolor ? dcolor : wxBLACK, 0, rop);
	SetPen(apen);

	if (DRAWABLE && src->Ok()) {
	  Region free_rgn = NULL;

	  if (mask)
	    tmp_mask = IntersectBitmapRegion(PEN_GC, EXPOSE_REG, USER_REG, mask,
					     &free_rgn,
					     &tx, &ty,
					     &scaled_width, &scaled_height,
					     &xsrc, &ysrc,
					     DPY, wx_white_pixel);

	  // Check if we're copying from a mono bitmap
	  retval = TRUE;
	  if ((rop == wxSOLID) || (rop == wxXOR)) {
	    /* Seems like the easiest way to implement transparent backgrounds is to
	       use a stipple. */
	    XGCValues values;
	    unsigned long mask = GCFillStyle | GCStipple | GCTileStipXOrigin | GCTileStipYOrigin;
	    values.stipple = GETPIXMAP(src);
	    values.fill_style = FillStippled;
	    values.ts_x_origin = ((tx - (long)xsrc) % src->GetWidth());
	    values.ts_y_origin = ((ty - (long)ysrc) % src->GetHeight());
	    XChangeGC(DPY, PEN_GC, mask, &values);
	    XFillRectangle(DPY, DRAWABLE, PEN_GC, tx, ty, scaled_width, scaled_height);

	    /* Restore pen: */
	    values.fill_style = FillSolid;
	    XChangeGC(DPY, PEN_GC, GCFillStyle, &values);
	  } else {
	    /* Opaque (i.e., white pixels transferred as background) */
	    Pixmap pm;
	    pm = GETPIXMAP(src);
	    XCopyPlane(DPY, pm, DRAWABLE, PEN_GC,
		       (long)xsrc, (long)ysrc,
		       scaled_width, scaled_height,
		       tx, ty, 1);
	  }
	  
	  /* restore pen: */
	  if (mask)
	    SetCanvasClipping();
	  
	  if (free_rgn)
	    XDestroyRegion(free_rgn);
	}

	SetPen(savePen);
	SetBackground(saveBack);
      }
#ifdef WX_USE_XRENDER
    }
#endif
    
    if (tmp) {
      DELETE_OBJ tmp;
    }
    if (tmp_mask) {
      DELETE_OBJ tmp_mask;
    }

    return retval; // #f => something wrong with the drawables
}

Bool wxWindowDC::GCBlit(double xdest, double ydest, double w, double h, wxBitmap *src,
			double xsrc, double ysrc, wxBitmap *bmask)
{
  /* A non-allocating (of collectable memory) blit, but may allocate
     when bmask is non-NULL.  Note that there's no rop or color. We do
     the simplest thing (wxSOLID with black) always. */
  Bool retval = FALSE;
  int scaled_width;
  int scaled_height;
  GC agc;

    if (!DRAWABLE) // ensure that a drawable has been associated
      return FALSE;
    
    if (!src->Ok())
      return FALSE;

    FreeGetPixelCache();

    if (src->selectedTo)
      src->selectedTo->EndSetPixel();
    
    xsrc = floor(xsrc);
    ysrc = floor(ysrc);

    scaled_width = src->GetWidth()  < XLOG2DEVREL(w) ? src->GetWidth()  : XLOG2DEVREL(w);
    scaled_height = src->GetHeight() < YLOG2DEVREL(h) ? src->GetHeight() : YLOG2DEVREL(h);

    if (DRAWABLE && src->Ok()) {
      XGCValues values;
      int mask = 0;
      Region free_rgn = (Region)NULL;
      int tx, ty;
      wxBitmap *tmp_mask;

      tx = XLOG2DEV(xdest);
      ty = YLOG2DEV(ydest);

      if ((DEPTH == 1) && (src->GetDepth() > 1)) {
	/* May need to flip 1 & 0... */
	if (wx_black_pixel == 1) {
	  mask = GCFunction;
	  values.function = GXcopyInverted;
	}
      }

      agc = XCreateGC(DPY, DRAWABLE, mask, &values);
      tmp_mask = IntersectBitmapRegion(agc, EXPOSE_REG, USER_REG, bmask, 
				       &free_rgn,
				       &tx, &ty,
				       &scaled_width, &scaled_height,
				       &xsrc, &ysrc,
				       DPY, wx_white_pixel);
	  
      retval = TRUE;
      if ((src->GetDepth() == 1) || (DEPTH == 1)) {
	/* mono to color/mono  or  color/mono to mono */
	Pixmap pm;
	pm = GETPIXMAP(src);
	XCopyPlane(DPY, pm, DRAWABLE, agc,
		   (long)xsrc, (long)ysrc,
		   scaled_width, scaled_height,
		   tx, ty, 1);
      } else if (src->GetDepth() == (int)DEPTH) {
	/* color to color */
	Pixmap pm;
	pm = GETPIXMAP(src);
	XCopyArea(DPY, pm, DRAWABLE, agc,
		  (long)xsrc, (long)ysrc,
		  scaled_width, scaled_height,
		  tx, ty);
      } else
	retval = FALSE;

      XFreeGC(DPY, agc);

      if (free_rgn)
	XDestroyRegion(free_rgn);

      if (tmp_mask)
	DELETE_OBJ tmp_mask;
    }

    return retval; // !retval => something is wrong with the drawables
}

#ifdef WX_USE_XRENDER
static void SetToGray(XRenderColor *col, Pixel bg_pixel)
{
  wxColour *c;
  int a, v;

  c = ((bg_pixel == wxGREY_PIXEL) ? wxGREY : wxBUTTON_COLOR);

  a = 0xD0;
  col->alpha = a << 8;

  /* Pre-multiply alpha. In the end, we want to shift the
     result by 8, so we skip the division by 255 that would
     scale alpha (since 255 is close enough to 256). */
  v = c->Red();
  col->red = v * a;
  v = c->Green();
  col->green = v * a;
  v = c->Blue();
  col->blue = v * a;
}
#endif

void doDrawBitmapLabel(Display *dpy, 
		       Pixmap pixmap, Pixmap maskmap,
		       Drawable drawable, GC agc,
		       int x, int y, int width, int height, 
		       int depth, int mask_depth,
		       Region reg,
		       GC gray_gc,
		       Pixel bg_pixel)
{
#ifdef WX_USE_XRENDER
  if (maskmap && (mask_depth > 1)) {
    Picture dest, src, mask;

    dest = (Picture)wxMakeXrenderPicture(drawable, 1);
    src = (Picture)wxMakeXrenderPicture(pixmap, (depth > 1));
    mask = XRenderCreatePicture(wxAPP_DISPLAY,
				maskmap,
				alpha_format,
				0,
				NULL);

    XRenderSetPictureClipRegion(wxAPP_DISPLAY, dest, reg);

    XRenderComposite(wxAPP_DISPLAY,
		     PictOpOver,
		     src,
		     mask,
		     dest,
		     0, 0,
		     0, 0,
		     x, y, width, height);

    if (gray_gc) {
      XRenderColor col;

      SetToGray(&col, bg_pixel);

      XRenderFillRectangle(wxAPP_DISPLAY, PictOpOver, dest, &col,
			   x, y, width, height);
    }

    XRenderFreePicture(dpy, dest);
    XRenderFreePicture(wxAPP_DISPLAY, src);
    XRenderFreePicture(wxAPP_DISPLAY, mask);
    return;
  }
#endif
   
  if (maskmap && (mask_depth == 1)) {
    XSetClipMask(dpy, agc, maskmap);
    XSetClipOrigin(dpy, agc, x, y);
  }
  
  if (depth == 1) {
    XCopyPlane(dpy, pixmap, drawable, agc,
	       0, 0, width, height, x, y, 1);
  } else {
    XCopyArea(dpy, pixmap, drawable, agc,
	      0, 0, width, height, x, y);
  }
  
  if (maskmap && (mask_depth == 1)) {
    XSetClipMask(dpy, agc, None);
    XSetClipOrigin(dpy, agc, 0, 0);
  }

  if (gray_gc) {
#ifdef WX_USE_XRENDER
    if (wxXRenderHere()) {
      Picture dest;
      XRenderColor col;
      
      dest = (Picture)wxMakeXrenderPicture(drawable, 1);
      XRenderSetPictureClipRegion(wxAPP_DISPLAY, dest, reg);

      SetToGray(&col, bg_pixel);
      
      XRenderFillRectangle(wxAPP_DISPLAY, PictOpOver, dest, &col,
			   x, y, width, height);
      
      XRenderFreePicture(dpy, dest);
      return;
    }
#endif
    XSetRegion(dpy, gray_gc, reg);
    XFillRectangle(dpy, drawable, gray_gc, x, y, width, height);
    XSetClipMask(dpy, gray_gc, None);
  }
}

extern "C" {
  void wxDrawBitmapLabel(Display *dpy, 
			 Pixmap pixmap, Pixmap maskmap,
			 Drawable drawable, GC agc,
			 int x, int y, int width, int height, 
			 int depth, int mask_depth,
			 Region reg, GC gray_gc, Pixel bg) {
    return doDrawBitmapLabel(dpy, pixmap, maskmap,
			     drawable, agc,
			     x, y, width, height, 
			     depth, mask_depth,
			     reg, gray_gc, bg);
  }
};

void wxWindowDC::Clear(void)
{
  unsigned int w, h;
  Window wdummy; int sdummy; unsigned int udummy;
  
  if (!DRAWABLE) // ensure that a drawable has been associated
    return;
  
  FreeGetPixelCache();
  
  XGetGeometry(DPY, DRAWABLE, &wdummy, &sdummy, &sdummy,
	       &w, &h, &udummy, &udummy);
  
#ifdef WX_USE_CAIRO
  if (anti_alias) {
    wxColour *c;
    int r, g, b;
    
    InitCairoDev();

    c = current_background_color;
    r = c->Red();
    g = c->Green();
    b = c->Blue();
    cairo_set_source_rgba(CAIRO_DEV, 
                          r / 255.0, g / 255.0, b / 255.0,
                          current_alpha);

    cairo_new_path(CAIRO_DEV);
    cairo_move_to(CAIRO_DEV, 0, 0);
    cairo_line_to(CAIRO_DEV, w, 0);
    cairo_line_to(CAIRO_DEV, w, h);
    cairo_line_to(CAIRO_DEV, 0, h);
    cairo_fill(CAIRO_DEV);

    return;
  }
#endif
 
  XFillRectangle(DPY, DRAWABLE, BG_GC, 0, 0, w, h);
}

void wxWindowDC::DrawArc(double x, double y, double w, double h, double start, double end)
{
  int xx, yy, ww, hh;
  double xw, yh;
  double degrees1, degrees2;
  int alpha1, alpha2;

  if (!DRAWABLE) // ensure that a drawable has been associated
    return;
  
  FreeGetPixelCache();

#ifdef WX_USE_CAIRO
  if (anti_alias) {
    InitCairoDev();

    start = -start;
    end = -end;

    if (SetCairoBrush()) {
      double xx, yy, ww, hh;

      xx = SmoothingXFormXB(x);
      yy = SmoothingXFormYB(y);
      ww = SmoothingXFormW(w, x);
      hh = SmoothingXFormH(h, y);

      cairo_save(CAIRO_DEV);
      cairo_translate(CAIRO_DEV, xx, yy);
      cairo_scale(CAIRO_DEV, ww, hh);
      cairo_new_path(CAIRO_DEV);
      cairo_move_to(CAIRO_DEV, 0.5, 0.5);
      cairo_arc_negative(CAIRO_DEV, 0.5, 0.5, 0.5, start, end);
      cairo_fill(CAIRO_DEV);
      cairo_restore(CAIRO_DEV);
    }
    
    
    if (SetCairoPen()) {
      double xx, yy, ww, hh;
      cairo_matrix_p m;

      xx = SmoothingXFormX(x);
      yy = SmoothingXFormY(y);
      ww = SmoothingXFormWL(w, x);
      hh = SmoothingXFormHL(h, y);

      if ((ww > 0.0) && (hh > 0.0)) {
        cairo_save(CAIRO_DEV);
        cairo_set_matrix_create(m);
        cairo_current_matrix (CAIRO_DEV, m);
        cairo_translate(CAIRO_DEV, xx, yy);
        cairo_scale(CAIRO_DEV, ww, hh);
        cairo_new_path(CAIRO_DEV);
        cairo_arc_negative(CAIRO_DEV, 0.5, 0.5, 0.5, start, end);
        cairo__set_matrix(CAIRO_DEV, m);
        cairo_stroke(CAIRO_DEV);
        cairo_restore(CAIRO_DEV);
        cairo_matrix_destroy(m);
      }
    }

    return;
  }
#endif

  xw = x + w, yh = y + h;
  
  xx = XLOG2DEV(x); yy = YLOG2DEV(y);
  ww = XLOG2DEV(xw) - xx; hh = YLOG2DEV(yh) - yy;
  
  degrees1 = start * RAD2DEG;
  degrees2 = end * RAD2DEG;
  alpha1 = int(degrees1 * 64.0);
  alpha2 = int((degrees2 - degrees1) * 64.0);
  while (alpha2 <= 0) {
    alpha2 += 360*64;
  }
  while (alpha1 > 360*64) {
    alpha1 -= 360*64;
  }
  
  if (current_brush && current_brush->GetStyle() != wxTRANSPARENT)
    XFillArc(DPY,DRAWABLE,BRUSH_GC,xx,yy,ww,hh,alpha1,alpha2);
  if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
    XDrawArc(DPY,DRAWABLE,PEN_GC,xx,yy,ww,hh,alpha1,alpha2);
}

void wxWindowDC::DrawEllipse(double x, double y, double w, double h)
{
  int x1, y1, w1, h1;
  double xw, yh;

  if (!DRAWABLE) // ensure that a drawable has been associated
    return;
  
  FreeGetPixelCache();

#ifdef WX_USE_CAIRO
  if (anti_alias) {
    DrawArc(x, y, w, h, 0, 2 * wxPI);
    return;
  }
#endif

  xw = x + w, yh = y + h;
  
  x1 = XLOG2DEV(x);
  y1 = YLOG2DEV(y);
  w1 = XLOG2DEV(xw) - x1;
  h1 = YLOG2DEV(yh) - y1;
  
  if (current_brush && current_brush->GetStyle() != wxTRANSPARENT)
    XFillArc(DPY, DRAWABLE, BRUSH_GC, x1, y1,
	     w1 - WX_GC_CF, h1 - WX_GC_CF, 0, 64*360);
  if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
    XDrawArc(DPY, DRAWABLE, PEN_GC, x1, y1,
	     w1 - WX_GC_CF, h1 - WX_GC_CF, 0, 64*360);
}

void wxWindowDC::DrawLine(double x1, double y1, double x2, double y2)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    FreeGetPixelCache();

    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
#ifdef WX_USE_CAIRO
      if (anti_alias) {
	double xx1, yy1, xx2, yy2;

	InitCairoDev();
	
	SetCairoPen();

	xx1 = SmoothingXFormX(x1);
	yy1 = SmoothingXFormY(y1);
	xx2 = SmoothingXFormX(x2);
	yy2 = SmoothingXFormY(y2);

	cairo_new_path(CAIRO_DEV);
	cairo_move_to(CAIRO_DEV, xx1, yy1);
	cairo_line_to(CAIRO_DEV, xx2, yy2);
	cairo_stroke(CAIRO_DEV);
      } else
#endif
	XDrawLine(DPY, DRAWABLE, PEN_GC,
		  XLOG2DEV(x1), YLOG2DEV(y1), XLOG2DEV(x2), YLOG2DEV(y2));
    }
}

void wxWindowDC::DrawLines(int n, wxPoint pts[], double xoff, double yoff)
{
  XPoint *xpts;
  int i;

  if (!DRAWABLE) // ensure that a drawable has been associated
    return;

  if (!current_pen || (current_pen->GetStyle() == wxTRANSPARENT))
    return;

  if (n < 2)
    return;

  FreeGetPixelCache();
  
#ifdef WX_USE_CAIRO
  if (anti_alias) {
    int i;

    InitCairoDev();
    
    SetCairoPen();

    cairo_new_path(CAIRO_DEV);
    cairo_move_to(CAIRO_DEV, SmoothingXFormX(pts[0].x + xoff), SmoothingXFormY(pts[0].y + yoff));
    for (i = 1; i < n; i++) {
      cairo_line_to(CAIRO_DEV, SmoothingXFormX(pts[i].x + xoff), SmoothingXFormY(pts[i].y + yoff));
    }
    cairo_stroke(CAIRO_DEV);    

    return;
  }
#endif

  xpts = new WXGC_ATOMIC XPoint[n];
  for (i=0; i<n; ++i) {
    short x, y;
    x = XLOG2DEV(pts[i].x + xoff);
    xpts[i].x = x;
    y = YLOG2DEV(pts[i].y + yoff);
    xpts[i].y = y;
  }
  XDrawLines(DPY, DRAWABLE, PEN_GC, xpts, n, 0);
}

void wxWindowDC::DrawPoint(double x, double y)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    FreeGetPixelCache();
    
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	XDrawPoint(DPY, DRAWABLE, PEN_GC, XLOG2DEV(x), YLOG2DEV(y));
}

void wxWindowDC::DrawPolygon(int n, wxPoint pts[], double xoff, double yoff,
			     int fill)
{
  XPoint *xpts;

  if (!DRAWABLE) // ensure that a drawable has been associated
    return;

  FreeGetPixelCache();

#ifdef WX_USE_CAIRO
  if (anti_alias) {
    InitCairoDev();

    if (SetCairoBrush()) {
      int i;     

      if (fill == wxODDEVEN_RULE)
	cairo_set_fill_rule(CAIRO_DEV, CAIRO_FILL_RULE_EVEN_ODD);
      
      cairo_new_path(CAIRO_DEV);
      cairo_move_to(CAIRO_DEV, SmoothingXFormX(pts[0].x + xoff), SmoothingXFormY(pts[0].y + yoff));
      for (i = 1; i < n; i++) {
	cairo_line_to(CAIRO_DEV, SmoothingXFormX(pts[i].x + xoff), SmoothingXFormY(pts[i].y + yoff));
      }
      cairo_fill(CAIRO_DEV); 

      if (fill == wxODDEVEN_RULE)
	cairo_set_fill_rule(CAIRO_DEV, CAIRO_FILL_RULE_WINDING);
    }

    if (SetCairoPen()) {
      int i;

      cairo_new_path(CAIRO_DEV);
      cairo_move_to(CAIRO_DEV, SmoothingXFormX(pts[0].x + xoff), SmoothingXFormY(pts[0].y + yoff));
      for (i = 1; i < n; i++) {
	cairo_line_to(CAIRO_DEV, SmoothingXFormX(pts[i].x + xoff), SmoothingXFormY(pts[i].y + yoff));
      }
      cairo_close_path(CAIRO_DEV);
      cairo_stroke(CAIRO_DEV);
    }

    return;
  }
#endif
  
  xpts = new WXGC_ATOMIC XPoint[n+1];
  for (int i=0; i<n; ++i) {
    short x, y;
    x = XLOG2DEV(pts[i].x + xoff);
    xpts[i].x = x;
    y = YLOG2DEV(pts[i].y + yoff);
    xpts[i].y = y;
  }
  xpts[n].x = xpts[0].x; // close figure
  xpts[n].y = xpts[0].y;
  if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
    XSetFillRule(DPY, BRUSH_GC, fill_rule[fill]);
    XFillPolygon(DPY, DRAWABLE, BRUSH_GC, xpts, n, Complex, 0);
  }
  if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
    XDrawLines(DPY, DRAWABLE, PEN_GC, xpts, n+1, 0);
}

void wxWindowDC::DrawRectangle(double x, double y, double w, double h)
{
    int x1, y1, w1, h1;
    double xw, yh;

    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    FreeGetPixelCache();
    
#ifdef WX_USE_CAIRO
    if (anti_alias) {
      double xx, yy, ww, hh;

      InitCairoDev();
      if (SetCairoBrush()) {
	xx = SmoothingXFormXB(x);
	yy = SmoothingXFormYB(y);
	ww = SmoothingXFormW(w, x);
	hh = SmoothingXFormH(h, y);

	cairo_new_path(CAIRO_DEV);
	cairo_move_to(CAIRO_DEV, xx, yy);
	cairo_line_to(CAIRO_DEV, xx+ww, yy);
	cairo_line_to(CAIRO_DEV, xx+ww, yy+hh);
	cairo_line_to(CAIRO_DEV, xx, yy+hh);
	cairo_fill(CAIRO_DEV);    
      }

      if (SetCairoPen()) {
	xx = SmoothingXFormX(x);
	yy = SmoothingXFormY(y);
	ww = SmoothingXFormWL(w, x);
	hh = SmoothingXFormHL(h, y);
      
	cairo_new_path(CAIRO_DEV);
	cairo_move_to(CAIRO_DEV, xx, yy);
	cairo_line_to(CAIRO_DEV, xx+ww, yy);
	cairo_line_to(CAIRO_DEV, xx+ww, yy+hh);
	cairo_line_to(CAIRO_DEV, xx, yy+hh);
	cairo_close_path(CAIRO_DEV);
	cairo_stroke(CAIRO_DEV);
      }

      return;
    }
#endif

    xw = x + w, yh = y + h;
   
    x1 = XLOG2DEV(x);
    y1 = YLOG2DEV(y);
    w1 = XLOG2DEV(xw) - x1;
    h1 = YLOG2DEV(yh) - y1;

    if (current_brush && current_brush->GetStyle() != wxTRANSPARENT)
      XFillRectangle(DPY, DRAWABLE, BRUSH_GC, x1, y1, w1, h1);
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
      XDrawRectangle(DPY, DRAWABLE, PEN_GC, x1, y1, w1 - WX_GC_CF, h1 - WX_GC_CF);
}

void wxWindowDC::DrawRoundedRectangle(double x, double y, double w, double h,
				      double radius)
{
  int xx, yy, ww, hh, rr, dd;
  double xw, yh;

    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    FreeGetPixelCache();
    
    if (radius < 0.0)
      radius = - radius * ((w < h) ? w : h);

#ifdef WX_USE_CAIRO
    if (anti_alias) {
      double xx, yy, ww, hh, rr, rr2;

      InitCairoDev();
      if (SetCairoBrush()) {
	xx = SmoothingXFormXB(x);
	yy = SmoothingXFormYB(y);
	ww = SmoothingXFormW(w, x);
	hh = SmoothingXFormH(h, y);

	rr = SmoothingXFormW(radius, 0);
	rr2 = SmoothingXFormH(radius, 0);
	if (rr2 < rr)
	  rr = rr2;

	cairo_move_to(CAIRO_DEV, xx, yy + rr);
	cairo_line_to(CAIRO_DEV, xx, yy + hh - rr);
	cairo_arc_negative(CAIRO_DEV, xx + rr, yy + hh - rr, rr, wxPI, 0.5 * wxPI);
	cairo_line_to(CAIRO_DEV, xx + ww - rr, yy + hh);
	cairo_arc_negative(CAIRO_DEV, xx + ww - rr, yy + hh - rr, rr, 0.5 * wxPI, 0);
	cairo_line_to(CAIRO_DEV, xx + ww, yy + rr);
	cairo_arc_negative(CAIRO_DEV, xx + ww - rr, yy + rr, rr, 2 * wxPI, 1.5 * wxPI);
	cairo_line_to(CAIRO_DEV, xx + rr, yy);
	cairo_arc_negative(CAIRO_DEV, xx + rr, yy + rr, rr, 1.5 * wxPI, wxPI);
	cairo_line_to(CAIRO_DEV, xx, yy + rr);
	cairo_fill(CAIRO_DEV);    
      }

      if (SetCairoPen()) {
	xx = SmoothingXFormX(x);
	yy = SmoothingXFormY(y);
	ww = SmoothingXFormWL(w, x);
	hh = SmoothingXFormHL(h, y);

	rr = SmoothingXFormWL(radius, 0);
	rr2 = SmoothingXFormHL(radius, 0);
	if (rr2 < rr)
	  rr = rr2;

	cairo_move_to(CAIRO_DEV, xx, yy + rr);
	cairo_line_to(CAIRO_DEV, xx, yy + hh - rr);
	cairo_arc_negative(CAIRO_DEV, xx + rr, yy + hh - rr, rr, wxPI, 0.5 * wxPI);
	cairo_line_to(CAIRO_DEV, xx + ww - rr, yy + hh);
	cairo_arc_negative(CAIRO_DEV, xx + ww - rr, yy + hh - rr, rr, 0.5 * wxPI, 0);
	cairo_line_to(CAIRO_DEV, xx + ww, yy + rr);
	cairo_arc_negative(CAIRO_DEV, xx + ww - rr, yy + rr, rr, 2 * wxPI, 1.5 * wxPI);
	cairo_line_to(CAIRO_DEV, xx + rr, yy);
	cairo_arc_negative(CAIRO_DEV, xx + rr, yy + rr, rr, 1.5 * wxPI, wxPI);
	cairo_line_to(CAIRO_DEV, xx, yy + rr);
	cairo_close_path(CAIRO_DEV);
	cairo_stroke(CAIRO_DEV);
      }

      return;
    }
#endif

    xw = x + w, yh = y + h;
    
    xx = XLOG2DEV(x);       yy = YLOG2DEV(y);
    ww = XLOG2DEV(xw) - xx; hh = YLOG2DEV(yh) - yy;
    if (scale_x < scale_y)
      rr = XLOG2DEVREL(radius);
    else
      rr = YLOG2DEVREL(radius);
    dd = 2 * rr;

    if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
	XFillRectangle(DPY, DRAWABLE, BRUSH_GC, xx+rr, yy, ww-dd, hh);
	XFillRectangle(DPY, DRAWABLE, BRUSH_GC, xx, yy+rr, ww, hh-dd);
	XFillArc(DPY, DRAWABLE, BRUSH_GC, xx, yy, dd - WX_GC_CF, dd - WX_GC_CF, 90*64, 90*64);
	XFillArc(DPY, DRAWABLE, BRUSH_GC, xx+ww-dd, yy, dd - WX_GC_CF, dd - WX_GC_CF, 0, 90*64);
	XFillArc(DPY, DRAWABLE, BRUSH_GC, xx+ww-dd, yy+hh-dd, dd - WX_GC_CF, dd - WX_GC_CF,
		 270*64, 90*64);
	XFillArc(DPY, DRAWABLE, BRUSH_GC, xx, yy+hh-dd, dd - WX_GC_CF, dd - WX_GC_CF, 180*64, 90*64);
    }
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT){
        ww -= WX_GC_CF;
        hh -= WX_GC_CF;
	XDrawLine(DPY, DRAWABLE, PEN_GC, xx+rr, yy, xx+ww-rr, yy);
	XDrawLine(DPY, DRAWABLE, PEN_GC, xx+rr, yy+hh, xx+ww-rr, yy+hh);
	XDrawLine(DPY, DRAWABLE, PEN_GC, xx, yy+rr, xx, yy+hh-rr);
	XDrawLine(DPY, DRAWABLE, PEN_GC, xx+ww, yy+rr, xx+ww, yy+hh-rr);
	XDrawArc(DPY, DRAWABLE, PEN_GC, xx, yy, dd, dd, 90*64, 90*64);
	XDrawArc(DPY, DRAWABLE, PEN_GC, xx+ww-dd, yy, dd, dd, 0, 90*64);
	XDrawArc(DPY, DRAWABLE, PEN_GC, xx+ww-dd, yy+hh-dd, dd, dd,
		 270*64, 90*64);
	XDrawArc(DPY, DRAWABLE, PEN_GC, xx, yy+hh-dd, dd, dd, 180*64, 90*64);
    }
}

void wxWindowDC::DrawPath(wxPath *p, double xoff, double yoff, int fill)
{
  double **ptss;
  int *lens, cnt, i, total_cnt, j, k, ix, iy;
  XPoint *xpts;

  if (!DRAWABLE) // ensure that a drawable has been associated
    return;

  FreeGetPixelCache();

#ifdef WX_USE_CAIRO
  if (anti_alias) {

    InitCairoDev();
    if (SetCairoBrush()) {
      if (fill == wxODDEVEN_RULE)
	cairo_set_fill_rule(CAIRO_DEV, CAIRO_FILL_RULE_EVEN_ODD);

      if (AlignSmoothing()) {
	double pw;
	pw = GetPenSmoothingOffset();
	p->Install((long)CAIRO_DEV, xoff, yoff, 
		   device_origin_x, device_origin_y, user_scale_x, user_scale_y, 
		   TRUE, pw, pw);
      } else
	p->Install((long)CAIRO_DEV, xoff, yoff, 0, 0, 1, 1, FALSE, 0, 0);
      cairo_fill(CAIRO_DEV);

      if (fill == wxODDEVEN_RULE)
	cairo_set_fill_rule(CAIRO_DEV, CAIRO_FILL_RULE_WINDING);
    }

    if (SetCairoPen()) {
      if (AlignSmoothing()) {
	double pw;
	pw = GetPenSmoothingOffset();
	p->Install((long)CAIRO_DEV, xoff, yoff, 
		   device_origin_x, device_origin_y, user_scale_x, user_scale_y, 
		   TRUE, pw, pw);
      } else
	p->Install((long)CAIRO_DEV, xoff, yoff, 0, 0, 1, 1, FALSE, 0, 0);
      cairo_stroke(CAIRO_DEV);
    }

    return;
  }
#endif

  cnt = p->ToPolygons(&lens, &ptss, user_scale_x, user_scale_y);

  if (!cnt)
    return;

  total_cnt = 0;
  for (i = 0; i < cnt; i++) {
    total_cnt += (lens[i] / 2) + 1;
  }
  
  xpts = new WXGC_ATOMIC XPoint[total_cnt];

  for (i = 0, k = 0; i < cnt; i++) {
    for (j = 0; j < lens[i]; j += 2) {
      ix = XLOG2DEV(ptss[i][j]+xoff);
      iy = YLOG2DEV(ptss[i][j+1]+yoff);
      xpts[k].x = ix;
      xpts[k].y = iy;
      k++;
    }
    ix = XLOG2DEV(ptss[i][0]+xoff);
    iy = YLOG2DEV(ptss[i][1]+yoff);
    xpts[k].x = ix;
    xpts[k].y = iy;
    k++;
  }

  if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
    XSetFillRule(DPY, BRUSH_GC, fill_rule[fill]);
    if (cnt == 1) {
      XFillPolygon(DPY, DRAWABLE, BRUSH_GC, xpts, total_cnt, Complex, 0);
    } else {
      Region rgn = 0, rgn1;

      for (i = 0, k = 0; i < cnt; i++) {
	j = (lens[i] / 2) + 1;
	rgn1 = XPolygonRegion(xpts XFORM_OK_PLUS k, j, fill_rule[fill]);
	if (rgn) {
	  /* Xoring implements the even-odd rule */
	  XXorRegion(rgn, rgn1, rgn);
	  XDestroyRegion(rgn1);
	} else {
	  rgn = rgn1;
	}
	k += j;
      }

      if (CURRENT_REG)
	XIntersectRegion(rgn, CURRENT_REG, rgn);

      XSetRegion(DPY, BRUSH_GC, rgn);
      XFillRectangle(DPY, DRAWABLE, BRUSH_GC, 0, 0, 32000, 32000);

      if (CURRENT_REG)
	XSetRegion(DPY, BRUSH_GC, CURRENT_REG);
      else
	XSetClipMask(DPY, BRUSH_GC, None);

      XDestroyRegion(rgn);
    }      
  }
  if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
    for (i = 0, k = 0; i < cnt; i++) {
      j = (lens[i] / 2) + 1;
      if ((i + 1 == cnt) && p->IsOpen())
	--j;
      XDrawLines(DPY, DRAWABLE, PEN_GC, xpts XFORM_OK_PLUS k, j, 0);
      k += j;
    }
  }
}

//-----------------------------------------------------------------------------
// drawing tools
//-----------------------------------------------------------------------------

void wxWindowDC::SetBackground(wxColour *c)
{
  int style;
  unsigned long pixel;

  if (!DRAWABLE)
    return;
  
  if (c != current_background_color)
    current_background_color->CopyFrom(c);
  
  pixel = current_background_color->GetPixel(current_cmap, IS_COLOR, 0);
  
  if (DRAW_WINDOW)
    XSetWindowBackground(DPY, DRAW_WINDOW, pixel);
  XSetForeground(DPY, BG_GC, pixel);
  XSetBackground(DPY, PEN_GC, pixel);
  XSetBackground(DPY, BRUSH_GC, pixel);
  
  // use the correct pixel values for XOR
  
  style = current_pen->GetStyle();
  if ((style >= wxXOR_DOT) && (style <= wxXOR_DOT_DASH))
    style = wxXOR;
  if (current_pen && ((style == wxXOR) || (style == wxCOLOR)))
    ResetPen(current_pen);
  
  if (current_brush && (current_brush->GetStyle() == wxXOR))
    ResetBrush(current_brush);
}

void wxWindowDC::SetBrush(wxBrush *brush)
{
  if (brush == current_brush)
    return;

  ResetBrush(brush);
}
    
void wxWindowDC::ResetBrush(wxBrush *brush)
{
  XGCValues     values;
  unsigned long mask;
  wxBitmap *bm;
  unsigned long pixel;
  int bstyle;

  if (!DRAWABLE)
    return;

  if (current_brush) current_brush->Lock(-1);

  if (!(current_brush = brush)) // nothing to do without brush
    return;

  if (current_brush) current_brush->Lock(1);

  // for XChangeGC
  mask = GCFillStyle | GCForeground | GCFunction;

  values.fill_style = FillSolid;
  bstyle = brush->GetStyle();
  if (bstyle == wxCOLOR) {
    pixel = wxCTL_HIGHLIGHT_PIXEL;
  } else {
    wxColour *bcol;
    bcol = brush->GetColour();
    pixel = bcol->GetPixel(current_cmap, IS_COLOR, 1);
  }
  if (bstyle == wxXOR) {
    XGCValues values_req;
    XGetGCValues(DPY, BRUSH_GC, GCBackground, &values_req);
    values.foreground = pixel ^ values_req.background;
    values.function = GXxor;
  } else if (bstyle == wxCOLOR) {
    values.foreground = pixel;
    values.function = GXorReverse;
  } else {
    values.foreground = pixel;
    values.function = GXcopy;
  }

  bm = brush->GetStipple();
  if (bm && !bm->Ok())
    bm = NULL;

  if (bm) {
    Pixmap stipple = (Pixmap)0; // for FillStippled
    Pixmap tile    = (Pixmap)0; // for FillTiled
    if (bm->GetDepth() == 1) {
      if (bm->selectedTo) bm->selectedTo->EndSetPixel();
      stipple = GETPIXMAP(bm);
      values.fill_style = ((brush->GetStyle()==wxSTIPPLE) ? FillOpaqueStippled : FillStippled);
    } else if (bm->GetDepth() == (signed)DEPTH) {
      if (bm->selectedTo) bm->selectedTo->EndSetPixel();
      tile = GETPIXMAP(bm);
      values.fill_style = FillTiled;
    } // else wrong depth
    if (stipple) {
      values.stipple = stipple;
      mask |= GCStipple;
    }
    if (tile) {
      values.tile = tile;
      mask |= GCTile;
      values.foreground = wx_black_pixel;
      values.function = GXcopy;
    }
  } else {
    int style;
    style = brush->GetStyle();
    if (wxIS_HATCH(style)) {
      Pixmap stipple = (Pixmap)0; // for FillStippled
      stipple = hatch_bitmaps[style-wxFIRST_HATCH];
      values.fill_style = FillStippled;
      if (stipple) {
	values.stipple = stipple;
	mask |= GCStipple;
      }
    }
  }
  XChangeGC(DPY, BRUSH_GC, mask, &values);
}

void wxWindowDC::SetColourMap(wxColourMap *new_cmap)
{
  current_cmap = new_cmap ? new_cmap : wxAPP_COLOURMAP;
  
  if (DRAW_WINDOW) {
    Window w;
    Colormap cm;
    w = DRAW_WINDOW;
    cm = CMAP;
    XSetWindowColormap(DPY, w, cm);
  }
}

static wxDash dashdefs[4][4] = {
  { 2, 5, 0, 0 }, // wxDOT
  { 4, 8, 0, 0 }, // wxLONG_DASH
  { 4, 4, 0, 0 }, // wxSHORT_DASH
  { 6, 6, 2, 6 }  // wxDOT_DASH
};
static int    num_dashes[] = { 2, 2, 2, 4 };	

void wxWindowDC::SetPen(wxPen *pen)
{
  if (pen == current_pen)
    return;

  ResetPen(pen);
}

void wxWindowDC::ResetPen(wxPen *pen)
{
  XGCValues     values;
  unsigned long mask;
  wxBitmap *bm;
  int scale;
  int style, doXor, js, cs, pw;
  unsigned long pixel;

    if (!DRAWABLE) /* MATTHEW: [5] */
      return;

    if (current_pen) current_pen->Lock(-1);
    
    if (!(current_pen = pen)) // nothing to do without pen
      return;

    if (current_pen) current_pen->Lock(1);

    // for XChangeGC
    mask = (GCCapStyle  | GCFillStyle | GCForeground
	    | GCJoinStyle | GCLineStyle | GCLineWidth
	    | GCFunction);

    cs = pen->GetCap();
    values.cap_style  = cap_style[cs];
    values.fill_style = FillSolid;
    js = pen->GetJoin();
    values.join_style = join_style[js];
    values.line_style = LineSolid;
    pw = pen->GetWidth();
    scale = XLOG2DEVREL(pw);
    values.line_width = scale;
    style = pen->GetStyle();
    if (style == wxCOLOR) {
      pixel = wxCTL_HIGHLIGHT_PIXEL;
    } else {
      wxColour *pcol;
      pcol = pen->GetColour();
      pixel = pcol->GetPixel(current_cmap, IS_COLOR, 1);
    }
    doXor = 0;

    switch (style) {
    case wxXOR:
      doXor = 1;
      break;
    case wxXOR_DOT:
    case wxXOR_SHORT_DASH:
    case wxXOR_LONG_DASH:
    case wxXOR_DOT_DASH:
      doXor = 1;
      style -= (wxXOR_DOT - wxDOT);
      break;
    }

    if (doXor) {
      XGCValues values_req;
      XGetGCValues(DPY, PEN_GC, GCBackground, &values_req);
      values.foreground = pixel ^ values_req.background;
      values.function = GXxor;
    } else if (style == wxCOLOR) {
      values.foreground = pixel;
      values.function = GXorReverse;
    } else {
      values.foreground = pixel;
      values.function = GXcopy;
    }

    bm = pen->GetStipple();
    if (bm && !bm->Ok())
      bm = NULL;

    if (bm) {
      Pixmap stipple = (Pixmap)0; // for FillStippled
      Pixmap tile    = (Pixmap)0; // for FillTiled
      if (bm->GetDepth() == 1) {
	if (bm->selectedTo) bm->selectedTo->EndSetPixel();
	stipple = GETPIXMAP(bm);
	values.fill_style = FillStippled;
      } else if (bm->GetDepth() == (signed)DEPTH) {
	if (bm->selectedTo) bm->selectedTo->EndSetPixel();
	tile = GETPIXMAP(bm);
	values.fill_style = FillTiled;
      } // else wrong depth
      if (stipple) {
	values.stipple = stipple;
	mask |= GCStipple;
      }
      if (tile) {
	values.tile = tile;
	mask |= GCTile;
	values.foreground = wx_black_pixel;
	values.function = GXcopy;
      }
    } else {
      if (wxIS_DASH(style) || style==wxUSER_DASH) {
	int           num_dash;
	wxDash        *dashdef, *scaleddef;
	if (style==wxUSER_DASH) {
	  num_dash = pen->GetDashes(&dashdef);
	} else {
	  num_dash = num_dashes[style-wxFIRST_DASH];
	  dashdef  = dashdefs[style-wxFIRST_DASH];
	}
	if ((scaleddef = new WXGC_ATOMIC wxDash[num_dash])) {
	  int dscale = scale, i;
	  if (!dscale) dscale = 1;
	  for (i = 0; i < num_dash; i++) {
	    scaleddef[i] = dscale * dashdef[i];
	  }
	  XSetDashes(DPY, PEN_GC, 0, (char*)scaleddef, num_dash);
	} else { // not enough memory to scale
	  XSetDashes(DPY, PEN_GC, 0, (char*)dashdef, num_dash);
	}
	values.line_style = LineOnOffDash;
      }
    }
    XChangeGC(DPY, PEN_GC, mask, &values);
}

void wxWindowDC::TryColour(wxColour *src, wxColour *dest)
{
  XColor xcol;

  if (!DRAWABLE)
    return;

  xcol.pixel = src->GetPixel(current_cmap, IS_COLOR, 1);

  if (IS_COLOR) {
    Colormap cm;
    cm = GETCOLORMAP(current_cmap);
    wxQueryColor(wxAPP_DISPLAY, cm, &xcol);
    
    dest->Set(xcol.red >> SHIFT, xcol.green >> SHIFT, xcol.blue >> SHIFT);
  } else if (xcol.pixel == 1) { /* not wx_black_pixel */
    dest->Set(0, 0, 0);
  } else {
    dest->Set(255, 255, 255);
  }
}


void wxWindowDC::FillPrivateColor(wxColour *c)
{
  XColor xcol;
  int free = 0;
  XGCValues values;
  int mask = 0;
  GC agc;
  double w, h;
  Colormap cm;

  if (!DRAWABLE)
    return;

  xcol.red = c->Red() << SHIFT;
  xcol.green = c->Green() << SHIFT;
  xcol.blue = c->Blue() << SHIFT;

  cm = GETCOLORMAP(current_cmap);

  if (XAllocColor(wxAPP_DISPLAY, cm, &xcol) == 1) {
    wxQueryColor(wxAPP_DISPLAY, cm, &xcol);
    c->Set(xcol.red >> SHIFT, xcol.green >> SHIFT, xcol.blue >> SHIFT);
    free = 1;
  } else {
    xcol.pixel = wx_black_pixel;
    c->Set(0, 0, 0);
  }

  values.foreground = xcol.pixel;
  values.fill_style = FillSolid;
  mask |= GCForeground | GCFillStyle;

  agc = XCreateGC(DPY, DRAWABLE, mask, &values);
  
  GetSize(&w, &h);

  XFillRectangle(DPY, DRAWABLE, agc, 0, 0, (int)w, (int)h);

  XFreeGC(DPY, agc);

  if (free)
    XFreeColors(wxAPP_DISPLAY, cm, &xcol.pixel, 1, 0);
}

//-----------------------------------------------------------------------------
// text and font methods
//-----------------------------------------------------------------------------

#ifdef WX_USE_XFT

static int symbol_map[] = { 0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 8704, 0, 8707, 0, 0, 8717,
			    0, 0, 8727, 0, 0, 8722, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    8773, 913, 914, 935, 916, 917, 934, 915,
			    919, 921, 977, 922, 923, 924, 925, 927,
			    928, 920, 929, 931, 932, 933, 962, 937,
			    926, 936, 918, 0, 8756, 0, 8869, 0,
			    0, 945, 946, 967, 948, 949, 966, 947,
			    951, 953, 981, 954, 955, 956, 957, 959,
			    960, 952, 961, 963, 964, 965, 982, 969,
			    958, 968, 950, 0, 0, 0, 8764, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 978, 8242, 8804, 8260, 8734, 402, 9827,
			    9830, 9829, 9824, 8596, 8592, 8593, 8594, 8595,
			    0, 177, 8243, 8805, 215, 8733, 8706, 8729,
			    247, 8800, 8801, 8776, 8230, 9168, /* 9135 */8212, 8629,
			    8501, 8465, 8476, 8472, 8855, 8853, 8709, 8745,
			    8746, 8835, 8839, 8836, 8834, 8838, 8712, 8713,
			    8736, 8711, 174, 169, 8482, 8719, 8730, 8901,
			    172, 8743, 8744, 8660, 8656, 8657, 8658, 8659,
			    9674, 9001, 174, 169, 8482, 8721, 9115, 9116,
			    9117, 9121, 9122, 9123, 9127, 9128, 9129, 9130,
			    8364, 9002, 8747, 8992, 9134, 8993, 9118, 9119,
			    9120, 9124, 9125, 9126, 9131, 9132, 9133, 0 };

static unsigned int *XlateSym(unsigned int *text, int dt, int textlen, unsigned int *buf, int buflen)
{
  int i, v;
  unsigned int *us;

  if (text != buf) {
    if (buflen <= textlen)
      us = new WXGC_ATOMIC unsigned[textlen];
    else
      us = buf;
    memcpy(us, text + (dt * sizeof(int)), textlen * sizeof(unsigned int));
  } else {
    us = text;
    if (dt)
      memmove(us, us + (dt * sizeof(int)), textlen * sizeof(unsigned int));
  }

  for (i = 0; i < textlen; i++) {
    v = us[i];
    if ((v < 256) && symbol_map[v])
      v = symbol_map[v];
    us[i] = v;
  }

  return us;
}

#endif

extern "C" {
  int scheme_utf8_decode(const unsigned char *s, int start, int len, 
			 unsigned int *us, int dstart, int dlen,
			 long *ipos, char utf16, int permissive);
  int scheme_utf8_decode_all(const unsigned char *s, int len, unsigned int *us, 
			     int permissive);
};

static unsigned int *convert_to_drawable_format(const char *s, int ds, long *_ulen, unsigned int *buf, int bufsize, 
						int isUnicode, int non_xft)
{
  unsigned int *us;
  long ulen = *_ulen;

  if (isUnicode) {
    us = (unsigned int *)s;
    if (ulen < 0) {
      for (ulen = ds; us[ulen]; ulen++) {
      }
      ulen -= ds;
    }
    if (ds) {
      if (ulen <= bufsize)
	us = buf;
      else
	us = new WXGC_ATOMIC unsigned[ulen];
      memcpy(us, s + (ds * sizeof(int)), ulen * sizeof(unsigned int));
    }
  } else {
    int length;

    if (ulen < 0)
      length = strlen(s XFORM_OK_PLUS ds);
    else
      length = ulen;
    
    ulen = scheme_utf8_decode((const unsigned char *)s, ds, ds + length, NULL, 0, -1, NULL, 0, '?');
    if (ulen <= bufsize)
      us = buf;
    else
      us = new WXGC_ATOMIC unsigned[ulen];
    ulen = scheme_utf8_decode((const unsigned char *)s, ds, ds + length, us, 0, -1, NULL, 0, '?');
  }

  if (non_xft) {
    /* Squash 32-bit encoding into 16-bit encoding.
       Since we overwrite the array, it's important
       to start at position 0 and go up: */
    int i, v;
    XChar2b *dest;
    
    if (isUnicode) {
      if (2 * ulen <= bufsize)
	dest = (XChar2b *)buf;
      else
	dest = new WXGC_ATOMIC XChar2b[ulen];
    } else
      dest = (XChar2b *)us;

    for (i = 0; i < ulen; i++) {
      if (us[i] > 0xFFFF)
	v = '?';
      else
	v = us[i];
      dest[i].byte2 = v & 0xff;
      dest[i].byte1 = v >> 8;
    }

    us = (unsigned int *)dest;
  }

  *_ulen = ulen;
  
  return us;
}

#define WX_CVT_BUF_SIZE 1024
#ifdef WX_USE_XFT
# define WX_XFT_ONLY(x) x
#else
# define WX_XFT_ONLY(x) 1
#endif
static unsigned int cvt_buf[WX_CVT_BUF_SIZE];

/* Xft measurements use a `short'. Avoid string lengths that are
   likely to overflow it. */
#define MAX_TEXT_LENGTH_FOR_MEASURE 100

void wxWindowDC::DrawText(char *orig_text, double x, double y, 
			  Bool combine, Bool isUnicode, int dt,
			  double angle)
{
  XFontStruct *fontinfo;
#ifdef WX_USE_XFT
  wxFontStruct *xfontinfo;
#endif
  double       cx, cy;
  int         ascent;
  int         dev_x;
  int         dev_y;
  long        textlen;
  double       e_scale_x, e_scale_y;
  double       ca, sa;
  unsigned int *text;
  

  if (!DRAWABLE) // ensure that a drawable has been associated
    return;

  if (!current_font) // a font must be associated for drawing
    return;

  /* e_scale_x and e_scale_y are the effective font scales
     considering that the font is rotated before scaled,
     and the horizontal and vertical scales can be different.
     The angle of the text is also affected if the scales
     are different; sa and ca are the effective sin and cos
     of the angle of the stretched/squashed baseline */
  if (angle == 0.0) {
    e_scale_x = scale_x;
    e_scale_y = scale_y;
    sa = 0.0;
    ca = 1.0;
  } else {
    ca = cos(angle);
    sa = sin(angle);
    if (scale_x == scale_y) {
      e_scale_x = scale_x;
      e_scale_y = scale_y;
    } else {
      double a2;
      e_scale_x = sqrt(((scale_x * ca) * (scale_x * ca)) + ((scale_y * sa) * (scale_y * sa)));
      e_scale_y = sqrt(((scale_y * ca) * (scale_y * ca)) + ((scale_x * sa) * (scale_x * sa)));
      a2 = atan2(sa * scale_y, ca * scale_x);
      ca = cos(a2);
      sa = sin(a2);
    }
  }
  
#ifdef WX_USE_XFT
  InitPicture();
  xfontinfo = (wxFontStruct*)current_font->GetInternalAAFont(scale_x, scale_y, angle);
  if (xfontinfo)
    fontinfo = NULL;
  else
#endif
    fontinfo = (XFontStruct *)current_font->GetInternalFont(e_scale_x, e_scale_y, 0.0);

  dev_x = XLOG2DEV(x);
  dev_y = YLOG2DEV(y);

  {
    double tw, th, td, ts;
    GetTextExtent(orig_text, &tw, &th, &td, &ts, current_font, combine, isUnicode, dt, -1);
    cx = (tw * e_scale_x);
    cy = (th * e_scale_y);
    ascent = (int)((th - td) * e_scale_y);
  }

  textlen = -1;
  text = convert_to_drawable_format(orig_text, dt, &textlen, cvt_buf, WX_CVT_BUF_SIZE, 
				    isUnicode, WX_XFT_ONLY(!xfontinfo));
  dt = 0;

# ifdef WX_USE_XFT
  if (xfontinfo) {
    if ((current_font->GetFamily() == wxSYMBOL)) {
      text = XlateSym(text, dt, textlen, cvt_buf, WX_CVT_BUF_SIZE);
      dt = 0;
    }
  }
#endif

#ifdef WX_USE_XFT
  if (xfontinfo) {
    int xasc;
    int v;
    XftColor col;
    /* Note: Xft wants colors with pre-multiplied alpha,
       according to my experiments. */
    col.pixel = current_text_fg->GetPixel();
    v = current_text_fg->Red();
    v = (int)(v * current_alpha);
    col.color.red = (v << 8) | v;
    v = current_text_fg->Green();
    v = (int)(v * current_alpha);
    col.color.green = (v << 8) | v;
    v = current_text_fg->Blue();
    v = (int)(v * current_alpha);
    col.color.blue = (v << 8) | v;
    col.color.alpha = (int)(current_alpha * 0xFFFF);

    if ((angle == 0.0) && (current_text_bgmode == wxSOLID)) {
      /* For B & W target, XftDrawRect doesn't seem to work right. */
      int rw;
      rw = (int)floor((x * scale_x) + device_origin_x + cx) - dev_x;
      if (Colour) {
	XftColor bg;
	bg.pixel = current_text_bg->GetPixel();
	v = current_text_bg->Red();
	v = (int)(v * current_alpha);
	bg.color.red = (v << 8) | v;
	v = current_text_bg->Green();
	v = (int)(v * current_alpha);
	bg.color.green = (v << 8) | v;
	v = current_text_bg->Blue();
	v = (int)(v * current_alpha);
	bg.color.blue = (v << 8) | v;
	bg.color.alpha = (int)(current_alpha * 0xFFFF);
	XftDrawRect(XFTDRAW, &bg, dev_x, dev_y, rw, xfontinfo->ascent + xfontinfo->descent);
      } else {
	unsigned long pixel;
	pixel = current_text_fg->GetPixel(current_cmap, IS_COLOR, 0);
	XSetForeground(DPY, TEXT_GC, pixel);
	XFillRectangle(DPY, DRAWABLE, TEXT_GC, dev_x, dev_y, rw, xfontinfo->ascent + xfontinfo->descent);
	pixel = current_text_fg->GetPixel(current_cmap, IS_COLOR, 1);
	XSetForeground(DPY, TEXT_GC, pixel);
      }
    }

    if (angle != 0.0) {
      xasc = (int)((double)ascent * sa);
      ascent = (int)((double)ascent * ca);
    } else
      xasc = 0;

    {
      wxFontStruct *this_time, *this_time_no_rotate, *no_rotate;
      int partlen, try_sub;

      try_sub = 1;

      while(textlen) {
	int nowlen;
	nowlen = textlen;
	if (nowlen > MAX_TEXT_LENGTH_FOR_MEASURE) {
	  nowlen = MAX_TEXT_LENGTH_FOR_MEASURE;
	}

	if (angle != 0.0)
	  no_rotate = (wxFontStruct*)current_font->GetInternalAAFont(e_scale_x, e_scale_y, 0.0);
	else
	  no_rotate = xfontinfo;

	if (try_sub) {
	  int index = 1, cval;
	  partlen = 1;
	  this_time = xfontinfo;
	  this_time_no_rotate = no_rotate;
	  while (1) {
	    cval = text[dt];
	    if (!XftGlyphExists(DPY, this_time, cval)) {
	      this_time = (wxFontStruct*)current_font->GetNextAASubstitution(index, cval, scale_x, scale_y, angle);
	      if (!this_time) {
		this_time = xfontinfo;
		this_time_no_rotate = no_rotate;
		break;
	      }
	      if (angle != 0.0)
		this_time_no_rotate = (wxFontStruct*)current_font->GetNextAASubstitution(index, cval, e_scale_x, e_scale_y, 0.0);
	      else
		this_time_no_rotate = this_time;
              index++;
	    } else
	      break;
	  }
	  
	  /* Get a longer range that won't need a substitution */
	  if (this_time == xfontinfo) {
	    while (partlen < nowlen) {
	      cval = text[dt + partlen];
	      if (((this_time != xfontinfo)
		   && XftGlyphExists(DPY, xfontinfo, cval))
		  || !XftGlyphExists(DPY, this_time, cval))
		break;
	      partlen++;
	    }
	  }
	} else {
	  partlen = nowlen;
	  this_time = xfontinfo;
	  this_time_no_rotate = no_rotate;
	}

	XftDrawString32(XFTDRAW, &col, this_time, dev_x+xasc, dev_y+ascent, ((XftChar32 *)text) + dt, partlen);

	if (partlen < textlen) {
	  XGlyphInfo overall;
	  double w;
	  XftTextExtents32(DPY, this_time_no_rotate, ((XftChar32 *)text) + dt, partlen, &overall);
	  w = overall.xOff;
	  if (angle != 0.0) {
	    xasc += (int)((double)w * ca);
	    ascent -= (int)((double)w * sa);
	  } else
	    xasc += (int)w;
	}

	textlen -= partlen;
	dt += partlen;
      }
    }
  } else
#endif
    {
      if ((angle == 0.0) && (current_text_bgmode == wxSOLID)) {
	if (need_x_set_font) {
	  XSetFont(DPY, TEXT_GC, fontinfo->fid);
	  need_x_set_font = 0;
	}
	XDrawImageString16(DPY, DRAWABLE, TEXT_GC, dev_x, dev_y+ascent, (XChar2b *)text + dt, textlen);
      } else {
	if (angle != 0.0) {
	  double offset;
	  double quadrant, pie = 3.14159;
	  int the_x, the_y, i;
	  XFontStruct *zfontinfo;
	  XChar2b *text2b;
	  
	  zfontinfo = fontinfo;
	  fontinfo = (XFontStruct *)current_font->GetInternalFont(scale_x, scale_y, angle);
	  XSetFont(DPY, TEXT_GC, fontinfo->fid);

	  quadrant = fmod(angle, 2 * pie);
	  if (quadrant < 0)
	    quadrant += (2 * pie);
	  
	  dev_y += (int)((double)ascent * ca);
	  dev_x += (int)((double)ascent * sa);

	  offset = 0.0;
	  text2b = (XChar2b *)text;
	  for (i = 0; i < textlen; i++) {
	    int char_metric_offset;

	    char_metric_offset = ((text2b[dt].byte1 - zfontinfo->min_byte1)
				  * (zfontinfo->max_char_or_byte2 - zfontinfo->min_char_or_byte2 + 1)
				  - zfontinfo->min_char_or_byte2
				  + text2b[dt].byte2);
	    
	    the_x = (int)((double)dev_x + offset * ca);
	    the_y = (int)((double)dev_y - offset * sa);
	    
	    XDrawString16(DPY, DRAWABLE, TEXT_GC, the_x, the_y, text2b + dt, 1);
	    dt++;
	  	    
	    offset += (double)(zfontinfo->per_char ?
			       zfontinfo->per_char[char_metric_offset].width :
			       zfontinfo->min_bounds.width);
	  }

	  XSetFont(DPY, TEXT_GC, zfontinfo->fid);
	} else {
	  if (need_x_set_font) {
	    XSetFont(DPY, TEXT_GC, fontinfo->fid);
	    need_x_set_font = 0;
	  }
	  XDrawString16(DPY, DRAWABLE, TEXT_GC, dev_x, dev_y+ascent, ((XChar2b *)text) + dt, textlen);
	}
      }
    }
}

double wxWindowDC::GetCharHeight(void)
{
  double w, h, descent, topspace;

  if (!current_font) // no font
    return YDEV2LOGREL(12);

  GetTextExtent("x", &w, &h, &descent, &topspace, current_font, 0, 0, 0);

  return h;
}

double wxWindowDC::GetCharWidth(void)
{
  double w, h, descent, topspace;

  if (!current_font) // no font
    return YDEV2LOGREL(12);

  GetTextExtent("x", &w, &h, &descent, &topspace, current_font, 0, 0, 0);

  return w;
}

void wxGetTextExtent(Display *dpy, double scale_x, double scale_y,
		     const char *orig_s, double *_w, double *_h, double *_descent,
		     double *_topspace, wxFont *font_to_use,
		     Bool combine, Bool isUnicode, int dt, int len)
{
  int         ascent, descent, space = 0;
  long        textlen;
  XFontStruct *fontinfo;
#ifdef WX_USE_XFT
  wxFontStruct *xfontinfo;
#endif
  double w, h;
  unsigned *s;

#ifdef WX_USE_XFT
  xfontinfo = (wxFontStruct*)font_to_use->GetInternalAAFont(scale_x, scale_y);
  if (xfontinfo)
    fontinfo = NULL;
  else
#endif
    fontinfo = (XFontStruct*)font_to_use->GetInternalFont(scale_x, scale_y);

  textlen = len;
  s = convert_to_drawable_format(orig_s, dt, &textlen, cvt_buf, WX_CVT_BUF_SIZE, 
				 isUnicode, WX_XFT_ONLY(!xfontinfo));
  dt = 0;

# ifdef WX_USE_XFT
  if (xfontinfo) {
    XGlyphInfo overall;
    wxFontStruct *this_time;
    int partlen, try_sub;
    
    if ((font_to_use->GetFamily() == wxSYMBOL)) {
      s = XlateSym(s, dt, textlen, cvt_buf, WX_CVT_BUF_SIZE);
      dt = 0;
    }
    try_sub = 1;

    w = 0;

    while (textlen) {
      int nowlen;
      nowlen = textlen;
      if (nowlen > MAX_TEXT_LENGTH_FOR_MEASURE) {
	nowlen = MAX_TEXT_LENGTH_FOR_MEASURE;
      }

      if (try_sub) {
	int index = 1, cval;
	partlen = 1;
	this_time = xfontinfo;
	
	while (1) {
	  cval = s[dt];
	  if (!XftGlyphExists(dpy, this_time, cval)) {
	    this_time = (wxFontStruct*)font_to_use->GetNextAASubstitution(index++, cval, scale_x, scale_y, 0.0);
	    if (!this_time) {
	      this_time = xfontinfo;
	      break;
	    }
	  } else
	    break;
	}

	/* Get a longer range that won't need a substitution */
	if (this_time == xfontinfo) {
	  while (partlen < nowlen) {
	    cval = s[dt + partlen];
	    if (((this_time != xfontinfo)
		 && XftGlyphExists(dpy, xfontinfo, cval))
		|| !XftGlyphExists(dpy, this_time, cval))
	      break;
	    partlen++;
	  }
	}
      } else {
	partlen = nowlen;
	this_time = xfontinfo;
      }
      
      XftTextExtents32(dpy, this_time, ((XftChar32 *)s) + dt, partlen, &overall);
      w += overall.xOff;

      textlen -= partlen;
      dt += partlen;
    }

    ascent = xfontinfo->ascent;
    descent = xfontinfo->descent;
    space = xfontinfo->height - xfontinfo->ascent - xfontinfo->descent;
    if (space < 0)
      space = -space; /* This means that we're computing the space wrong! */
  } else
#endif
    {
      int direction;
      XCharStruct overall;

      XTextExtents16(fontinfo, (XChar2b *)s + dt, textlen,
		     &direction, &ascent, &descent, &overall);
      w = overall.width;
    }

  *_w = w;
  h = ascent + descent;
  *_h = h;
  if (_descent) {
    double d;
    d = descent;
    *_descent = d;
  }
  if (_topspace)
    *_topspace = space;
}

void wxWindowDC::GetTextExtent(const char *orig_s, double *_w, double *_h, double *_descent,
			       double *_topspace, wxFont *_font,
			       Bool combine, Bool isUnicode, int dt, int len)
{
  wxFont *font_to_use;
  double v;

  if (!DRAWABLE) // ensure that a drawable has been associated
    return;

  font_to_use = _font ? _font : current_font;
  if (!font_to_use) {
    wxError("set a font before calling GetTextExtent", "wxWindowDC");
    if (_w)
      *_w = -1.0;
    if (_h)
      *_h = -1.0;
    return;
  }

  wxGetTextExtent(DPY, scale_x, scale_y,
		  orig_s, _w, _h, _descent, _topspace, 
		  font_to_use, combine, isUnicode, dt, len);

  if (_w) {
    v = XDEV2LOGREL((int)*_w);
    *_w = v;
  }
  if (_h) {
    v = YDEV2LOGREL((int)*_h);
    *_h = v;
  }
  if (_descent) {
    v = YDEV2LOGREL((int)*_descent);
    *_descent = v;
  }
  if (_topspace) {
    v = YDEV2LOGREL((int)*_topspace);
    *_topspace = v;
  }
}

Bool wxWindowDC::GlyphAvailable(int c, wxFont *font)
{
  if (!font)
    font = current_font;

  return font->ScreenGlyphAvailable(c);
}

void wxWindowDC::SetFont(wxFont *font)
{
  if (!DRAWABLE)
    return;

  if (!(current_font = font)) // nothing to do without a font
    return;

  need_x_set_font = 1;
}

void wxWindowDC::SetTextForeground(wxColour *col)
{
    unsigned long pixel;

    if (!DRAWABLE)
	return;

    if (!col)
      return;
    if (col != current_text_fg)
      current_text_fg->CopyFrom(col);
    pixel = current_text_fg->GetPixel(current_cmap, IS_COLOR, 1);
    XSetForeground(DPY, TEXT_GC, pixel);
}

void wxWindowDC::SetTextBackground(wxColour *col)
{
    unsigned long pixel;

    if (!DRAWABLE) /* MATTHEW: [5] */
	return;

    if (!col)
      return;
    if (col != current_text_bg)
      current_text_bg->CopyFrom(col);
    pixel = current_text_bg->GetPixel(current_cmap, IS_COLOR, 0);
    XSetBackground(DPY, TEXT_GC, pixel);
}

int wxCanvasDC::CacheFontMetricsKey()
{
  if ((scale_x == 1.0)
      && (scale_y == 1.0))
    return 1;
  return 0;
}

//-----------------------------------------------------------------------------
// clipping region
//-----------------------------------------------------------------------------

static Region empty_rgn;

void wxWindowDC::SetClippingRect(double x, double y, double w, double h)
{
  wxRegion *r;
  r = new wxRegion(this);
  r->SetRectangle(x, y, w, h);

  SetClippingRegion(r);
}

void wxWindowDC::SetClippingRegion(wxRegion *r)
{
  if (clipping)
    --clipping->locked;

  clipping = r;

  if (clipping)
    clipping->locked++;

  if (r) {
    if (r->rgn) {
      USER_REG = r->rgn;
    } else {
      /* NULL r->rgn means empty region */
      if (!empty_rgn)
	empty_rgn = XCreateRegion();
      USER_REG = empty_rgn;
    }
  } else
    USER_REG = NULL;
  SetCanvasClipping();
}

wxRegion *wxWindowDC:: GetClippingRegion()
{
  return clipping;
}

void wxWindowDC::GetSize(double *w, double *h)
{
  Window wdummy; int sdummy; unsigned int udummy, width, height;

  if (DRAWABLE) {
    XGetGeometry(DPY, DRAWABLE, &wdummy, &sdummy, &sdummy,
		 &width, &height, &udummy, &DEPTH);
    
    *w = width;
    *h = height;
  } else {
    *w = *h = 0;
  }
}

//-----------------------------------------------------------------------------
// methods unique to wxWindowDC
//-----------------------------------------------------------------------------

void wxWindowDC::Initialize(wxWindowDC_Xinit* init)
{
    Drawable GC_drawable; // necessary create a GC (needed for depth,...)
    wxFont *font;
    XGCValues values;
    unsigned long mask;
    int width, height;

    DPY = init->dpy; SCN = init->scn;

    if (init->drawable) {
      Window wdummy; int sdummy; unsigned int udummy;
      // I have a specified drawable -> get width, height, and depth
      GC_drawable = DRAWABLE = init->drawable;
      XGetGeometry(DPY, DRAWABLE, &wdummy, &sdummy, &sdummy,
		   &WIDTH, &HEIGHT, &udummy, &DEPTH);
    } else {
      GC_drawable = wxAPP_ROOT; // defaults to root
      DEPTH = wxDisplayDepth(); // depth is display depth
    }
    Colour = (DEPTH != 1); // accept everything else than depth one as colour display
    if (!Colour && anti_alias)
      anti_alias = 0;

#ifdef WX_USE_XRENDER
    X->picture = 0;
#endif

    values.foreground = wx_black_pixel;
    values.background = wx_white_pixel;
    values.graphics_exposures = FALSE;
    values.line_width = 1;
    mask = GCForeground | GCBackground | GCGraphicsExposures | GCLineWidth;
    PEN_GC   = XCreateGC(DPY, GC_drawable, mask, &values);
    TEXT_GC  = XCreateGC(DPY, GC_drawable, mask, &values);
    values.foreground = wx_white_pixel;
    values.background = wx_black_pixel;
    BG_GC    = XCreateGC(DPY, GC_drawable, mask, &values);
    BRUSH_GC = XCreateGC(DPY, GC_drawable, mask, &values);

    // set drawing tools
    SetTextForeground(current_text_fg);
    SetTextBackground(current_text_bg);
    SetBackground(current_background_color); 
    ResetBrush(current_brush);
    ResetPen(current_pen);

    font = current_font;
    current_font = NULL;
    SetFont(font ? font : wxNORMAL_FONT);

    // set display scaling
    width  = WidthOfScreen(SCN);
    height = HeightOfScreen(SCN);
    mm_to_pix_x = ((double)width)  / ((double)WidthMMOfScreen(SCN));
    mm_to_pix_y = ((double)height) / ((double)HeightMMOfScreen(SCN));
}

void wxWindowDC::Destroy(void)
{
#ifdef WX_USE_CAIRO
    ReleaseCairoDev();
#endif

    if (PEN_GC)    XFreeGC(DPY, PEN_GC);
    if (BRUSH_GC)  XFreeGC(DPY, BRUSH_GC);
    if (TEXT_GC)   XFreeGC(DPY, TEXT_GC);
    if (BG_GC)     XFreeGC(DPY, BG_GC);
    PEN_GC = BRUSH_GC = TEXT_GC = BG_GC = NULL;

    if (CURRENT_REG) XDestroyRegion(CURRENT_REG);
    if (EXPOSE_REG) XDestroyRegion(EXPOSE_REG);
    CURRENT_REG = USER_REG = EXPOSE_REG = NULL;

#ifdef WX_USE_XRENDER
    wxFreePicture(X->picture);
    X->picture = 0;
#endif
#ifdef USE_GL
    if (X->wx_gl) {
      X->wx_gl->Reset(NULL, 0, 0);
    }
#endif
}

void wxWindowDC::SetCanvasClipping(void)
{
    if (!DRAWABLE)
	return;

#ifdef WX_USE_CAIRO
    X->reset_cairo_clip = 1;
#endif

    if (CURRENT_REG)
	XDestroyRegion(CURRENT_REG);
    if (USER_REG || EXPOSE_REG) {
	CURRENT_REG = XCreateRegion();
	XIntersectRegion(EXPOSE_REG ? EXPOSE_REG : USER_REG,
			 USER_REG ? USER_REG : EXPOSE_REG,
			 CURRENT_REG);
	XSetRegion(DPY, PEN_GC, CURRENT_REG);
	XSetRegion(DPY, BRUSH_GC, CURRENT_REG);
	XSetRegion(DPY, BG_GC, CURRENT_REG);
	XSetRegion(DPY, TEXT_GC, CURRENT_REG);
#ifdef WX_USE_XFT
	if (XFTDRAW)
	  XftDrawSetClip(XFTDRAW, CURRENT_REG);
#endif
    } else {
	CURRENT_REG = NULL;
	XSetClipMask(DPY, PEN_GC, None);
	XSetClipMask(DPY, BRUSH_GC, None);
	XSetClipMask(DPY, BG_GC, None);
	XSetClipMask(DPY, TEXT_GC, None);
#ifdef WX_USE_XFT
	if (XFTDRAW)
	  XftDrawSetClip(XFTDRAW, None);
#endif
    }
}

Bool wxWindowDC::Ok(void)
{
  return !!DRAWABLE;
}


Bool wxWindowDC::GetPixel(double x, double y, wxColour * col)
{
  int i, j;
  Bool mini = 1;
  unsigned int w, h;

  if (!DRAWABLE)
    return FALSE;

  i = XLOG2DEV(x);
  j = YLOG2DEV(y);

  w = X->width;
  h = X->height;

  if (i < 0 || (unsigned int)i >= w
      || j < 0 || (unsigned int)j >= h)
    return FALSE;

#define NUM_GETPIX_CACHE_COLORS 256

  if (X->get_pixel_image_cache
      && ((i < X->cache_dx)
	  || (i >= X->cache_dx + X->get_pixel_image_cache->width)
	  || (j < X->cache_dy)
	  || (j >= X->cache_dy + X->get_pixel_image_cache->height))) {
    /* Turns out that getting a small part wasn't such a
       good idea. Go into whole-image mode. */
    EndSetPixel();
    mini = 0;
  }

  if (!X->get_pixel_image_cache) {
    BeginSetPixel(mini, i, j);

    if (X->get_pixel_image_cache->depth == 1) {
      XColor *get_pixel_color_cache;
      
      get_pixel_color_cache = X->get_pixel_color_cache;
      
      get_pixel_color_cache[0].pixel = 1;
      get_pixel_color_cache[0].red = 0;
      get_pixel_color_cache[0].green = 0;
      get_pixel_color_cache[0].blue = 0;

      get_pixel_color_cache[1].pixel = 0;
      get_pixel_color_cache[1].red = 255;
      get_pixel_color_cache[1].green = 255;
      get_pixel_color_cache[1].blue = 255;
      
      X->get_pixel_cache_pos = 2;
    }
  }

  {
    int r, g, b;
    GetPixelFast(i, j, &r, &g, &b);
    col->Set(r, g, b);
  }

  return TRUE;
}

void wxWindowDC::BeginSetPixel(int mini, int near_i, int near_j)
{
  unsigned int w, h, dx, dy, ni = (unsigned int)near_i, nj = (unsigned int)near_j;

  if (!DRAWABLE)
    return;

  if (X->get_pixel_image_cache)
    return;

  w = X->width;
  h = X->height;

  if (X->is_window) {
    /* Disallow: */
    return;
  }

  if (mini) {
    /* To lessen the cost of localized get-pixel and set-pixel
       in a large bitmap, we first try to get a small piece
       of the bitmap in "mini" mode. If we need more, then we'll
       jump to "whole-image" mode. */
    if (w > wxMINI_SIZE) {
      if (ni < (wxMINI_SIZE / 2))
	dx = 0;
      else if ((ni + (wxMINI_SIZE / 2)) > w)
	dx = w - wxMINI_SIZE;
      else
	dx = ni - (wxMINI_SIZE / 2);
      w = wxMINI_SIZE;
    } else
      dx = 0;
    if (h > wxMINI_SIZE) {
      if (nj < (wxMINI_SIZE / 2))
	dy = 0;
      else if ((nj + (wxMINI_SIZE / 2)) > h)
	dy = h - wxMINI_SIZE;
      else
	dy = nj - (wxMINI_SIZE / 2);
      h = wxMINI_SIZE;
    } else
      dy = 0;
  } else {
    dx = 0;
    dy = 0;
  }

  {
    XImage *img;
    img = XGetImage(DPY, DRAWABLE, dx, dy, w, h, AllPlanes, ZPixmap);
    X->get_pixel_image_cache = img;
  }
  
  X->get_pixel_cache_pos = 0;
  X->get_pixel_cache_full = FALSE;
  if (!wx_alloc_color_is_fast || (X->get_pixel_image_cache->depth == 1)) {
    XColor *cols;
    cols = new WXGC_ATOMIC XColor[NUM_GETPIX_CACHE_COLORS];
    X->get_pixel_color_cache = cols;

    if (X->get_pixel_image_cache->depth == 1) {
      cols[0].pixel = 1;
      cols[0].red = 0;
      cols[0].green = 0;
      cols[0].blue = 0;

      cols[1].pixel = 0;
      cols[1].red = 255;
      cols[1].green = 255;
      cols[1].blue = 255;
      X->get_pixel_cache_pos = 2;
    }
  }
  X->set_a_pixel = FALSE;
  X->cache_dx = dx;
  X->cache_dy = dy;
}

void wxWindowDC::EndSetPixel()
{
  if (!X->get_pixel_image_cache)
    return;

  if (X->set_a_pixel) {
    int w, h, dx, dy;
    w = X->get_pixel_image_cache->width;
    h = X->get_pixel_image_cache->height;
    dx = X->cache_dx;
    dy = X->cache_dy;
    
    XPutImage(DPY, DRAWABLE, PEN_GC, X->get_pixel_image_cache, 0, 0, dx, dy, w, h);
  }

  if (X->get_pixel_image_cache) {
    XDestroyImage(X->get_pixel_image_cache);
    X->get_pixel_image_cache = NULL;
    X->get_pixel_color_cache = NULL;
  }
}

void wxWindowDC::SetPixel(double x, double y, wxColour * col)
{
  int i, j;
  int w, h;
  int red, green, blue;

  i = XLOG2DEV(x);
  j = YLOG2DEV(y);

  BeginSetPixel(1, i, j);

  if (i < 0 || i >= (int)X->width
      || j < 0 || j >= (int)X->height)
    return;

  w = X->get_pixel_image_cache->width;
  h = X->get_pixel_image_cache->height;

  if (X->get_pixel_image_cache
      && ((i < X->cache_dx)
	  || (i >= X->cache_dx + w)
	  || (j < X->cache_dy)
	  || (j >= X->cache_dy + h))) {
    /* Turns out that getting a small part wasn't such a
       good idea. Go into whole-image mode. */
    EndSetPixel();
    BeginSetPixel(0, i, j);
  }

  if (!X->get_pixel_image_cache)
    return;

  red = col->Red();
  green = col->Green();
  blue = col->Blue();

  X->set_a_pixel = TRUE;

  SetPixelFast(i - X->cache_dx, j - X->cache_dy, red, green, blue);
}

Bool wxWindowDC::BeginSetPixelFast(int x, int y, int w, int h)
{
  if (BeginGetPixelFast(x, y, w, h)) {
    X->set_a_pixel = TRUE;
    return TRUE;
  } else
    return FALSE;
}

void wxWindowDC::EndSetPixelFast()
{
}

void wxWindowDC::SetPixelFast(int i, int j, int red, int green, int blue)
{
  int k;
  unsigned long pixel;
  XImage *get_pixel_image_cache;

  get_pixel_image_cache = X->get_pixel_image_cache;

  if (get_pixel_image_cache->depth == 1) {
    if ((red == 255) && (green == 255) && (blue == 255))
      pixel = 0;
    else
      pixel = 1;
  } else {
    if (wx_alloc_color_is_fast == 2) {
      pixel = ((red << wx_simple_r_start) 
	       | (green << wx_simple_g_start)
	       | (blue << wx_simple_b_start));
    } else {
      XColor xcol;

      if (!wx_alloc_color_is_fast) {
	int get_pixel_cache_pos;
	XColor *get_pixel_color_cache;
	Bool get_pixel_cache_full;

	get_pixel_cache_pos = X->get_pixel_cache_pos;
	get_pixel_color_cache = X->get_pixel_color_cache;
	get_pixel_cache_full = X->get_pixel_cache_full;

	for (k = get_pixel_cache_pos; k--; ) {
	  if ((get_pixel_color_cache[k].red == red)
	      && (get_pixel_color_cache[k].green == green)
	      && (get_pixel_color_cache[k].blue == blue)) {
	    pixel = get_pixel_color_cache[k].pixel;
	    goto put;
	  }
	}
      
	if (get_pixel_cache_full) {
	  for (k = NUM_GETPIX_CACHE_COLORS; k-- > get_pixel_cache_pos; ) {
	    if ((get_pixel_color_cache[k].red == red)
		&& (get_pixel_color_cache[k].green == green)
		&& (get_pixel_color_cache[k].blue == blue)) {
	      pixel = get_pixel_color_cache[k].pixel;
	      goto put;
	    }
	  }
	}
      }

      xcol.red = red << 8;
      xcol.green = green << 8;
      xcol.blue = blue << 8;
      
      {
	Colormap cm;
	cm = GETCOLORMAP(current_cmap);
	wxAllocColor(DPY, cm, &xcol);
      }
      
      pixel = xcol.pixel;
    }
    
    if (!wx_alloc_color_is_fast) {
      int get_pixel_cache_pos;
      XColor *get_pixel_color_cache;

      get_pixel_cache_pos = X->get_pixel_cache_pos;
      get_pixel_color_cache = X->get_pixel_color_cache;

      get_pixel_color_cache[get_pixel_cache_pos].pixel = pixel;
      get_pixel_color_cache[get_pixel_cache_pos].red = red;
      get_pixel_color_cache[get_pixel_cache_pos].green = green;
      get_pixel_color_cache[get_pixel_cache_pos].blue = blue;
      
      if (++(X->get_pixel_cache_pos) >= NUM_GETPIX_CACHE_COLORS) {
	X->get_pixel_cache_pos = 0;
	X->get_pixel_cache_full = TRUE;
      }
    }
  }

 put:
  pixel = XPutPixel(get_pixel_image_cache, i, j, pixel);  
}

void wxWindowDC::FreeGetPixelCache(void)
{
  if (X->get_pixel_image_cache) 
    EndSetPixel();
}

Bool wxWindowDC::BeginGetPixelFast(int x, int y, int w, int h)
{
  if ((x >= 0) && (y >= 0)
      && ((unsigned int)(x + w) <= X->width)
      && ((unsigned int)(y + h) <= X->height)) {

    /* Possible improvement: get only the revelant section of the
       bitmap into the image cache, instead of always the entire
       image. (In that case, use offsets in the fast Get and Put
       operations.) */

    if (X->get_pixel_image_cache
        && (X->cache_dx
            || X->cache_dy
            || (X->get_pixel_image_cache->width < (int)X->width)
            || (X->get_pixel_image_cache->height < (int)X->height))) {
      /* Need to get out of mini mode */
      EndSetPixel();
    }

    BeginSetPixel(0, 0, 0);
    return TRUE;
  } else
    return FALSE;
}

void wxWindowDC::EndGetPixelFast()
{
}

void wxWindowDC::GetPixelFast(int i, int j, int *r, int *g, int *b)
{
  unsigned long pixel;

  pixel = XGetPixel(X->get_pixel_image_cache, i, j);

  if ((wx_alloc_color_is_fast == 2) 
      && (X->get_pixel_image_cache->depth != 1)) {
    *r = ((pixel >> wx_simple_r_start) & 0xFF);
    *g = ((pixel >> wx_simple_g_start) & 0xFF);
    *b = ((pixel >> wx_simple_b_start) & 0xFF);
  } else {
    XColor xcol;

    if (!wx_alloc_color_is_fast
	|| (X->get_pixel_image_cache->depth == 1)) {
      int get_pixel_cache_pos, k;
      XColor *get_pixel_color_cache;
      Bool get_pixel_cache_full;

      get_pixel_cache_pos = X->get_pixel_cache_pos;
      get_pixel_color_cache = X->get_pixel_color_cache;
      get_pixel_cache_full = X->get_pixel_cache_full;

      for (k = get_pixel_cache_pos; k--; ) {
	if (get_pixel_color_cache[k].pixel == pixel) {
	  *r = get_pixel_color_cache[k].red;
	  *g = get_pixel_color_cache[k].green;
	  *b = get_pixel_color_cache[k].blue;
	  return;
	}
      }

      if (get_pixel_cache_full) {
	for (k = NUM_GETPIX_CACHE_COLORS; k-- > get_pixel_cache_pos; ) {
	  if (get_pixel_color_cache[k].pixel == pixel) {
	    *r = get_pixel_color_cache[k].red;
	    *g = get_pixel_color_cache[k].green;
	    *b = get_pixel_color_cache[k].blue;
	    return;
	  }
	}
      }
    }
  
    xcol.pixel = pixel;
    {
      Colormap cm;
      cm = GETCOLORMAP(current_cmap);
      wxQueryColor(wxAPP_DISPLAY, cm, &xcol);
    }

    if (!wx_alloc_color_is_fast) {
      int get_pixel_cache_pos;
      XColor *get_pixel_color_cache;
    
      get_pixel_cache_pos = X->get_pixel_cache_pos;
      get_pixel_color_cache = X->get_pixel_color_cache;

      get_pixel_color_cache[get_pixel_cache_pos].pixel = pixel;
      get_pixel_color_cache[get_pixel_cache_pos].red = xcol.red >> SHIFT;
      get_pixel_color_cache[get_pixel_cache_pos].green = xcol.green >> SHIFT;
      get_pixel_color_cache[get_pixel_cache_pos].blue = xcol.blue >> SHIFT;
    
      if (++get_pixel_cache_pos >= NUM_GETPIX_CACHE_COLORS) {
	get_pixel_cache_pos = 0;
	X->get_pixel_cache_full = TRUE;
      }

      X->get_pixel_cache_pos = get_pixel_cache_pos;
    }

    *r = (xcol.red >> SHIFT);
    *g = (xcol.green >> SHIFT);
    *b = (xcol.blue >> SHIFT);
  }
}

/********************************************************************/
/*                              GL                                  */
/********************************************************************/

// OpenGL
#ifdef USE_GL
static wxGL *current_gl_context = NULL;
static int gl_registered, display_has_glx;

static XVisualInfo *null_visual;
static int null_visual_set = 0;

typedef int (*X_Err_Handler)(Display*, XErrorEvent*);
static int errorFlagged;
static int FlagError(Display*, XErrorEvent*)
{
  errorFlagged = 1;
  return 0;
}

static XVisualInfo *GetWindowVisual(wxGLConfig *cfg, Boolean offscreen)
{
  XVisualInfo *vi = NULL;
  Visual *vis;
  GC_CAN_IGNORE XVisualInfo *visi, tmpl, *suggested_vi;
  int n, i, ac;
  GC_CAN_IGNORE int gl_attribs[20];
  X_Err_Handler old_handler;

  if (!gl_registered) {
    int a, b, c;

    wxREGGLOB(current_gl_context); 
    gl_registered = 1;

    if (XQueryExtension(wxAPP_DISPLAY, "GLX", &a, &b, &c)) {
      display_has_glx = 1;
    }
  }

  if (!display_has_glx)
    return NULL;

  if (!cfg)
    cfg = new wxGLConfig();

  /* Many parts of the MrEd drawing system rely on using the
     default colormap everywhere. So we need a GL visual that
     will have the same colormap. Get the default visual, then
     get a list of attribute-equivalent visuals, then find the'
     ones with the right GL properties... */

  while (1) {
    ac = 0;
    gl_attribs[ac++] = GLX_RGBA;
    if (!offscreen && cfg->doubleBuffered)
      gl_attribs[ac++] = GLX_DOUBLEBUFFER;
    if (cfg->depth) {
      gl_attribs[ac++] = GLX_DEPTH_SIZE;
      gl_attribs[ac++] = cfg->depth;
    }
    if (cfg->stencil) {
      gl_attribs[ac++] = GLX_STENCIL_SIZE;
      gl_attribs[ac++] = cfg->stencil;
    }
    if (cfg->accum) {
      gl_attribs[ac++] = GLX_ACCUM_RED_SIZE;
      gl_attribs[ac++] = cfg->accum;
      gl_attribs[ac++] = GLX_ACCUM_GREEN_SIZE;
      gl_attribs[ac++] = cfg->accum;
      gl_attribs[ac++] = GLX_ACCUM_BLUE_SIZE;
      gl_attribs[ac++] = cfg->accum;
      gl_attribs[ac++] = GLX_ACCUM_ALPHA_SIZE;
      gl_attribs[ac++] = cfg->accum;
    }
    if (cfg->stereo) {
      gl_attribs[ac++] = GLX_STEREO;
    }
#if defined(GL_ARB_multisample) && defined (GLX_SAMPLES_ARB)
    if (cfg->multisample) {
      gl_attribs[ac++] = GLX_SAMPLES_ARB;
      gl_attribs[ac++] = cfg->multisample;
    }
#else
# if defined(GLX_VERSION_1_1) && defined(GLX_SGIS_multisample) && defined(GLX_SAMPLES_SGIS)
    if (cfg->multisample) {
      gl_attribs[ac++] = GLX_SAMPLES_SGIS;
      gl_attribs[ac++] = cfg->multisample;
    }
# endif
#endif    
    
    gl_attribs[ac] = None;
    
    XSync(wxAPP_DISPLAY, FALSE);
    old_handler = XSetErrorHandler(FlagError);
    errorFlagged = 0;
    
    suggested_vi = glXChooseVisual(wxAPP_DISPLAY, XScreenNumberOfScreen(wxAPP_SCREEN), gl_attribs);
    if (errorFlagged) {
      suggested_vi = NULL;
      errorFlagged = 0;
    }

    XSetErrorHandler(old_handler);

    /* If we got a vi, stop. Otherwise, try changing
       the config until we find one. */
    if (suggested_vi)
      break;
    else if (cfg->multisample) {
      cfg = cfg->Clone();
      cfg->multisample = 0;
    } else
      break;
  }
    
  vis = wxAPP_VISUAL;
    
  tmpl.visualid = XVisualIDFromVisual(vis);
  visi = XGetVisualInfo(wxAPP_DISPLAY, VisualIDMask, &tmpl, &n);
  memcpy(&tmpl, visi, sizeof(tmpl));
  XFree(visi);

  /* Equivalent visuals: */
  visi = XGetVisualInfo(wxAPP_DISPLAY, 
			(VisualScreenMask
			 | VisualDepthMask
			 | VisualClassMask
			 | VisualRedMaskMask
			 | VisualGreenMaskMask
			 | VisualBlueMaskMask
			 | VisualColormapSizeMask
			 | VisualBitsPerRGBMask),
			&tmpl, &n);

  XSync(wxAPP_DISPLAY, FALSE);
  old_handler = XSetErrorHandler(FlagError);

  /* Search for double-buffered and single-buffered: */

  for (i = 0; i < n; i++) {
    if (suggested_vi) {
      if (visi[i].visualid == suggested_vi->visualid) {
	vi = suggested_vi;
	break;
      }
    }
  }
  
  /* The vi returned by XGetVisualInfo doesn't match our colormap.
     Manually search. */
  if (i >= n) {
    int pts, max_pts = 0;
    int min_aux_match = 1000;
    int min_sten_match = 1000;
    int min_depth_match = 1000;
	  
    for (i = 0; i < n; i++) {
      int v;
      glXGetConfig(wxAPP_DISPLAY, visi + i, GLX_USE_GL, &v);
      if (v && !errorFlagged) {
	glXGetConfig(wxAPP_DISPLAY, visi + i, GLX_LEVEL, &v);
	if (!v && !errorFlagged)  {
	  glXGetConfig(wxAPP_DISPLAY, visi + i, GLX_STEREO, &v);
	  if ((!v == cfg->stereo) && !errorFlagged)  {
	    glXGetConfig(wxAPP_DISPLAY, visi + i, GLX_DOUBLEBUFFER, &v);
	    if ((!v == !cfg->doubleBuffered) && !errorFlagged) {
	      pts = 0;
	      glXGetConfig(wxAPP_DISPLAY, visi + i, GLX_AUX_BUFFERS, &v);
	      if (v <= min_aux_match)
		pts += 4;
	      glXGetConfig(wxAPP_DISPLAY, visi + i, GLX_STENCIL_SIZE, &v);
	      if ((v <= min_sten_match) && (v >= cfg->stencil))
		pts += 4;
	      glXGetConfig(wxAPP_DISPLAY, visi + i, GLX_DEPTH_SIZE, &v);
	      if ((v <= min_depth_match) && (v >= cfg->depth))
		pts += 4;
	      glXGetConfig(wxAPP_DISPLAY, visi + i, GLX_ACCUM_RED_SIZE, &v);
	      if (v >= cfg->accum)
		pts++;
	      glXGetConfig(wxAPP_DISPLAY, visi + i, GLX_ACCUM_GREEN_SIZE, &v);
	      if (v >= cfg->accum)
		pts++;
	      glXGetConfig(wxAPP_DISPLAY, visi + i, GLX_ACCUM_BLUE_SIZE, &v);
	      if (v >= cfg->accum)
		pts++;
	      glXGetConfig(wxAPP_DISPLAY, visi + i, GLX_ACCUM_ALPHA_SIZE, &v);
	      if (v >= cfg->accum)
		pts++;
	      if ((pts <= max_pts) && !errorFlagged) {
		max_pts = pts;
		vi = visi + i;
	      }
	    }
	  }
	}
      }
    }
  }

  XFree(visi);

  XSetErrorHandler(old_handler);

  return vi;
}

Visual *wxGetGLCanvasVisual(wxGLConfig *cfg)
{
  XVisualInfo *vi;

  if (!cfg && null_visual_set)
    vi = null_visual;
  else {
    vi = GetWindowVisual(cfg, FALSE);
    
    if (!cfg) {
      null_visual_set = 1;
      null_visual = vi;
    }
  }

  if (vi)
    return vi->visual;
  else
    return NULL;
}

wxGL::wxGL()
: wxObject(WXGC_NO_CLEANUP)
{
}

wxGL::~wxGL()
{
}

int wxGL::Ok()
{
  return !!GLctx;
}

void wxGL::Reset(wxGLConfig *cfg, long d, int offscreen)
{
  XVisualInfo *vi;
  draw_to = 0;

  if (this == current_gl_context) {
    glXMakeCurrent(wxAPP_DISPLAY, None, NULL);
  }
  
  if (GLctx) {
    glXDestroyContext(wxAPP_DISPLAY, (GLXContext)GLctx);
    GLctx = 0;
    __type = wxTYPE_OBJECT; /* indicates "never selected" */
  }

  if (glx_pm) {
    glXDestroyGLXPixmap(wxAPP_DISPLAY, (GLXPixmap)glx_pm);
    glx_pm = 0;
  }

  if (d)
    vi = GetWindowVisual(cfg, offscreen);
  else
    vi = NULL;

  if (d) {
    GLXContext ctx;

    ctx = glXCreateContext(wxAPP_DISPLAY,
			   vi,
			   NULL,
			   offscreen ? GL_FALSE : GL_TRUE);

    GLctx = (long)ctx;

    if (GLctx) {
      if (offscreen) {
	GLXPixmap pm;
	
	pm = glXCreateGLXPixmap(wxAPP_DISPLAY, vi, (Drawable)d);
	glx_pm = (long)pm;
	draw_to = (long)pm;
      } else
	draw_to = d;
	
      if (current_gl_context == this) {
	ThisContextCurrent();
      }
    }
  }
}

void wxGL::SwapBuffers(void)
{
  if (GLctx) {
    if (!glx_pm) {
      /* Only if it's been selected before. Some X servers
	 seem to get annoyed, otherwise. */
      if (__type == wxTYPE_DC_OBJECT)
	glXSwapBuffers(wxAPP_DISPLAY, (Drawable)draw_to);
    }
  }
}

void wxGL::ThisContextCurrent(void)
{
  if (current_gl_context != this) {
    current_gl_context = this;
    if (GLctx) {
      glXMakeCurrent(wxAPP_DISPLAY, (Drawable)draw_to, (GLXContext)GLctx);
      __type = wxTYPE_DC_OBJECT; /* indicates "selected" */
    } else {
      glXMakeCurrent(wxAPP_DISPLAY, None, NULL);
    }
  }
}

void wxGLNoContext(void)
{
  glXMakeCurrent(wxAPP_DISPLAY, None, NULL);
  current_gl_context = NULL;
}

#endif

#include "../../../wxcommon/wxGLConfig.cxx"

/********************************************************************/
/*                         Cairo                                    */
/********************************************************************/

#ifdef WX_USE_CAIRO
void wxWindowDC::InitCairoDev()
{
  if (!X->cairo_dev) {
    cairo_t *dev;
    double ww, hh;

    GetSize(&ww, &hh);
    cairo_set_create_xlib(dev, wxAPP_DISPLAY, DRAWABLE, wxAPP_VISUAL, (int)ww, (int)hh);
    X->cairo_dev = (long)dev;
    X->reset_cairo_clip = 1;
  }

  cairo_default_matrix(CAIRO_DEV);
  if (X->reset_cairo_clip) {
    cairo_init_clip(CAIRO_DEV);
    if (EXPOSE_REG) {
      /* We assume a rectangular expose region. */
      XRectangle r;
      XClipBox(EXPOSE_REG, &r);
      cairo_new_path(CAIRO_DEV);
      cairo_move_to(CAIRO_DEV, r.x, r.y);
      cairo_rel_line_to(CAIRO_DEV, 0, r.height);
      cairo_rel_line_to(CAIRO_DEV, r.width, 0);
      cairo_rel_line_to(CAIRO_DEV, 0, -r.height);
      cairo_clip(CAIRO_DEV);
      cairo_new_path(CAIRO_DEV);
    }
    if (clipping)
      clipping->Install((long)CAIRO_DEV, AlignSmoothing());
    X->reset_cairo_clip = 0;
  }
  if (!AlignSmoothing()) {
    cairo_translate(CAIRO_DEV, device_origin_x, device_origin_y);
    cairo_scale(CAIRO_DEV, scale_x, scale_y);
  }
  
}

void wxWindowDC::ReleaseCairoDev()
{
  if (X->cairo_dev) {
    cairo_destroy_it(CAIRO_DEV);
    X->cairo_dev = 0;
  }
}

static double cairo_dotted[] = {2, 5, /* offset */ 2};
static double cairo_short_dashed[] = {4, 4, /* offset */ 2};
static double cairo_long_dashed[] = {4, 8, /* offset */ 2};
static double cairo_dotted_dashed[] = {6, 6, 2, 6, /* offset */ 4};


Bool wxWindowDC::SetCairoPen()
{
  if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
    wxColour *c;
    double pw;
    int cs, js;
    double *dashes;
    int ndash, r, g, b;
    double offset;

    c = current_pen->GetColour();
    r = c->Red();
    g = c->Green();
    b = c->Blue();
    cairo_set_source_rgba(CAIRO_DEV, 
                          r / 255.0, g / 255.0, b / 255.0,
                          current_alpha);

    pw = current_pen->GetWidthF();
    if (AlignSmoothing()) {
      pw = (int)pw;
      pw = (int)(pw * user_scale_x);
      if (!pw)
	pw = 1;
    } else {
      if (!pw) {
	if (scale_y > scale_x)
	  pw = 1 / scale_x;
	else
	  pw = 1 / scale_y;
      }
    }
    cairo_set_line_width(CAIRO_DEV, pw);

    cs = current_pen->GetCap();
    cairo_set_line_cap(CAIRO_DEV, c_cap_style[cs]);

    js = current_pen->GetJoin();
    cairo_set_line_join(CAIRO_DEV, c_join_style[js]);

    switch (current_pen->GetStyle ()) {
    case wxDOT:
      dashes = cairo_dotted;
      ndash = (sizeof(cairo_dotted) / sizeof(double)) - 1;
      break;
    case wxSHORT_DASH:
      dashes = cairo_short_dashed;
      ndash = (sizeof(cairo_short_dashed) / sizeof(double)) - 1;
      break;
    case wxLONG_DASH:
      dashes = cairo_long_dashed;
      ndash = (sizeof(cairo_long_dashed) / sizeof(double)) - 1;
      break;
    case wxDOT_DASH:
      dashes = cairo_dotted_dashed;
      ndash = (sizeof(cairo_dotted_dashed) / sizeof(double)) - 1;
      break;
    case wxSOLID:
    case wxTRANSPARENT:
    default:
      dashes = NULL;
      ndash = 0;
      break;
    }
    if (ndash)
      offset = dashes[ndash];
    else
      offset = 0;
    cairo_set_dash(CAIRO_DEV, dashes, ndash, offset);

    return TRUE;
  } else
    return FALSE;
}

Bool wxWindowDC::SetCairoBrush()
{
  if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
    wxColour *c;
    int r, g, b;
    c = current_brush->GetColour();
    r = c->Red();
    g = c->Green();
    b = c->Blue();
    cairo_set_source_rgba(CAIRO_DEV, 
                          r / 255.0, g / 255.0, b / 255.0,
                          current_alpha);
    return TRUE;
  } else
    return FALSE;
}

void wxWindowDC::SetAlpha(double d)
{
  wxDC::SetAlpha(d);
}

Bool wxWindowDC::AlignSmoothing()
{
  return (anti_alias == 2);
}

double wxWindowDC::GetPenSmoothingOffset()
{
  int pw;
  pw = current_pen->GetWidth();
  pw = (int)(user_scale_x * pw);
  if (!pw)
    pw = 1;
  return ((pw & 1) * 0.5);
}

double wxWindowDC::SmoothingXFormX(double x)
{
  if (AlignSmoothing())
    return floor((x * user_scale_x) + device_origin_x) + GetPenSmoothingOffset();
  else
    return x;
}

double wxWindowDC::SmoothingXFormY(double y)
{
  if (AlignSmoothing())
    return floor((y * user_scale_y) + device_origin_y) + GetPenSmoothingOffset();
  else
    return y;
}

double wxWindowDC::SmoothingXFormW(double w, double x)
{
  if (AlignSmoothing())
    return SmoothingXFormX(x + w) - SmoothingXFormX(x);
  else
    return w;
}

double wxWindowDC::SmoothingXFormH(double h, double y)
{
  if (AlignSmoothing())
    return SmoothingXFormY(y + h) - SmoothingXFormY(y);
  else
    return h;
}

double wxWindowDC::SmoothingXFormXB(double x)
{
  if (AlignSmoothing())
    return floor((x * user_scale_x) + device_origin_x);
  else
    return x;
}

double wxWindowDC::SmoothingXFormYB(double y)
{
  if (AlignSmoothing())
    return floor((y * user_scale_y) + device_origin_y);
  else
    return y;
}

double wxWindowDC::SmoothingXFormWL(double w, double x)
{
  if (AlignSmoothing()) {
    w = SmoothingXFormW(w, x);
    if (w >= 1.0)
      return w - 1.0;
    else
      return w;
  } else
    return w;
}

double wxWindowDC::SmoothingXFormHL(double h, double y)
{
  if (AlignSmoothing()) {
    h = SmoothingXFormH(h, y);
    if (h >= 1.0)
      return h - 1.0;
    else
      return h;
  } else
    return h;
}
#endif

void wxWindowDC::SetAntiAlias(int v)
{
#ifdef WX_USE_CAIRO
  if (v != anti_alias) {
    /* In case we go from aligned to not: */
    X->reset_cairo_clip = 1;
  }
    
  wxDC::SetAntiAlias(v);
#endif
}
