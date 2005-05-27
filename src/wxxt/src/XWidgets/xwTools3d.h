/*
 */

/***********************************************************
Copyright 2004-2005 PLT Scheme, Inc.
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.
Copyright 1995 by Markus Holzem

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital, MIT, or Kaleb 
Keithley not be used in advertising or publicity pertaining to distribution 
of the software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/* Values for MixType mix of Xaw3dAllocPixmap */
typedef enum _e_MixType {
    LIGHTER,			/* Pixmap shall be lighter than background */
    DARKER,			/* Pixmap shall be darker than background */
    GRAY			/* Pixmap shall seem as gray */
} MixType;

/*
 * Xaw3dAllocPixmap creates a pixmap, that seems LIGHTER or DARKER than
 * the background color or that seems GRAY
 */
Pixmap Xaw3dAllocPixmap(
    Widget  w,			/* widget, that needs the new color value */
    Pixel   bg,			/* background pixel of widget */
    MixType mix 		/* how to mix fore- and background */
);

#ifndef __sgi
/*
 * Xaw3dAllocPixel allocates a new color in 'contrast' to the background
 * color. Use values 0.5 <= contrast <= 1.5 to get good results.
 * A good triple is 0.6/0.85/1.2 for darker/something between/lighter.
 */
Pixel Xaw3dAllocPixel(
    Widget w,			/* widget, that needs the new color value */
    Pixel  bg,			/* background pixel of widget */
    float  contrast 		/* contrast of new color to background */
);
#endif

/*
 * Xaw3dGetGC gets a read-only GC with pxmap as tile or pixel as foreground
 * depending on be_nice_to_cmap. It is a good style to use (Pixmap)0 or 
 * (Pixel)0 for unnecessary values.
 */
GC Xaw3dGetGC(
    Widget  w,			/* widget, that needs the new color value */
    Boolean be_nice_to_cmap,	/* True: GC uses pxmap, False: GC uses pixel */
    Pixmap  pxmap,		/* Pixmap for GC */
    Pixel   pixel 		/* Pixel for GC */
);

GC Xaw3dRecomputeGC(
    Widget  new,
    Boolean be_nice_to_cmap,
    Pixmap  new_pxmap,
    Pixel   new_pixel,
    Widget  old,
    Pixmap  old_pxmap,
    Pixel   old_pixel,
    GC      old_GC
);

/*
 * Xaw3dFreePixmap(widget, pixmap)
 * Xaw3dFreePixel(widget, pixel)   <--- I don't know if it is useful
 * Xaw3dReleaseGC(widget, gc)
 * for consistency use this functions instead of XFreePixmap, XFreeColors
 * and XtReleaseGC
 */
#define Xaw3dFreePixmap(w,p) {\
    if (p) { XFreePixmap(XtDisplay((Widget)w), p); p = (Pixmap)0; } }
#define Xaw3dFreePixel(w,p) {\
    XFreeColors(XtDisplay((Widget)w), \
		DefaultColormapOfScreen(XtScreen((Widget)w)), &p, 1, 0); \
    p = (Pixel)-1; }
#define Xaw3dReleaseGC(w,g) {\
    XtReleaseGC((Widget)w, g); }

/* Values for ShadowType needed in 3d drawing functions */
typedef enum _e_ShadowType {
    XAW3D_BACKGROUND = 1,	/* for Xaw3dDrawRectangle to erase shadow */
    XAW3D_OUT = 2,		/*                                        */
    XAW3D_IN = 3,		/*               OUT        IN            */
    XAW3D_ETCHED_OUT = 4,	/*            ________                    */
    XAW3D_ETCHED_IN = 5,	/*           /        \  \________/       */
    XAW3D_ETCHED_OUT_DASH = 6,	/*                         ______         */
    XAW3D_ETCHED_IN_DASH = 7,	/*  ETCHED   /\______/\  \/      \/       */
    XAW3D_SINGLE_LINE = 8,	/*                                        */
    XAW3D_DOUBLE_LINE = 9,	/*  DASH:   line is dashed                */
    XAW3D_SINGLE_LINE_DASH = 10,/*  SINGLE: single line 2d                */
    XAW3D_DOUBLE_LINE_DASH = 11,/*  DOUBLE: double line 2d                */
    XAW3D_NO_LINE = 12,		/*  NO:     do not draw the line          */
    XAW3D_XED = 13,
    XAW3D_OUT_HARD = 14,
    XAW3D_IN_HARD = 15
} ShadowType;

/*
 * Xaw3dDrawRectangle draws a shadow-rectangle, ie top-left part and
 * bottom-right part in different colors (light and shadow) to make
 * the appearence of 3d.
 * Expected values for shadow_type:
 *   XAW3D_IN, XAW3D_OUT, XAW3D_ETCHED_IN, XAW3d_ETCHED_OUT,
 *   XAW3D_BACKGROUND (special for erasing shadow with backgroundGC)
 */
void Xaw3dDrawRectangle(
    Display    *dpy,		/* Display for drawing */
    Window     win,		/* Window for drawing */
    GC         lightGC,		/* GC for light color */
    GC         shadowGC,	/* GC for shadow color */
    GC         backgroundGC,	/* GC for background color */
    GC         fgGC,
    int        x,
    int        y,		/* upper left corner of rectangle */
    unsigned   width,
    unsigned   height,		/* width and height of rectangle */
    int        thickness,	/* thickness of shadow */
    ShadowType shadow_type	/* type of shadow */
);
/*
 * Xaw3dDrawLine draws a line that may have a 3d-appearence.
 * Expected values for shadow_type:
 *   XAW3D_NO_LINE, (do not draw the line),
 *   XAW3D_ETCHED_IN, XAW3D_ETCHED_OUT, XAW3D_ETCHED_IN_DASH,
 *   XAW3D_ETCHED_OUT_DASH, (3d-line solid or dashed, in or out),
 *   XAW3D_SINGLE_LINE, XAW3D_SINGLE_LINE_DASH, XAW3D_DOUBLE_LINE,
 *   XAW3D_DOUBLE_LINE_DASH, (2d-line single or double, solid or dashed).
 */
void Xaw3dDrawLine(
    Display    *dpy,		/* Display for drawing */
    Window     win,		/* Window for drawing */
    GC         lightGC,		/* GC for light color */
    GC         shadowGC,	/* GC for shadow color */
    GC         foregroundGC,	/* GC for foreground color */
    int        x,
    int        y,		/* upper left corner of line */
    unsigned   length,		/* length of line */
    int        thickness,	/* thickness of line */
    Boolean    vertical,	/* shall line be drawn vertical? */
    ShadowType shadow_type	/* type of shadow */
);

/*
 * Xaw3dDrawToggle draws a toggle set or unset in a square of
 * size width * width.
 */
void Xaw3dDrawToggle(
    Display    *dpy,		/* Display for drawing */
    Window     win,		/* Window for drawing */
    GC         lightGC,		/* GC for light color */
    GC         shadowGC,	/* GC for shadow color */
    GC         inGC,		/* GC for pushed/set toggle */
    GC         outGC,		/* GC for released/unset toggle */
    GC         fgGC,            /* GC for checkmark */
    int        x,
    int        y,		/* upper left corner */
    unsigned   width,		/* width of toggle square */
    int        thickness,	/* thickness of shadow */
    Boolean    pushed		/* is toggle pushed(in) or released(out) */
);

/*
 * Xaw3dDrawRadio draws a radio button set or unset in a square of
 * size width * width.
 */
void Xaw3dDrawRadio(
    Display    *dpy,		/* Display for drawing */
    Window     win,		/* Window for drawing */
    GC         lightGC,		/* GC for light color */
    GC         shadowGC,	/* GC for shadow color */
    GC         inGC,		/* GC for pushed/set radio */
    GC         outGC,		/* GC for released/unset readio */
    GC         fgGC,            /* GC for dot */
    int        x,
    int        y,		/* upper left corner */
    unsigned   width,		/* width of radio button */
    int        thickness,	/* thickness of shadow */
    Boolean    pushed		/* is radio pushed(in) or released(out) */
);

/* Values for ArrowType needed in Xaw3dDrawArrow*/
typedef enum _e_ArrowType {
    LEFT, RIGHT, UP, DOWN	/* Direction of arrow */
} ArrowType;

/*
 * Xaw3dDrawArrow draws an arrow pushed or released in an square
 * with size width * width.
 */
void Xaw3dDrawArrow(
    Display    *dpy,		/* Display for drawing */
    Window     win,		/* Window for drawing */
    GC         lightGC,		/* GC for light color */
    GC         shadowGC,	/* GC for shadow color */
    GC         inGC,		/* GC for pushed arrow */
    GC         outGC,		/* GC for released arrow */
    int        x,
    int        y,		/* upper left corner */
    unsigned   width,		/* width of arrow */
    unsigned   height,		/* height of arrow */
    int        thickness,	/* thickness of shadow */
    ArrowType  arrow_type,	/* LEFT, RIGHT, UP or DOWN arrow */
    Boolean    pushed		/* is radio pushed(in) or released(out) */
);
