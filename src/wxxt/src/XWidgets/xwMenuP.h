/***********************************************************
Copyright 2004-2010 PLT Scheme Inc.
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

#ifndef _MenuP_h
#define _MenuP_h

#include <X11/CoreP.h>
#include <xwMenu.h>

/* current and new state of windows */
typedef struct _menu_state {
    menu_item          *menu;
    menu_item          *selected;
    Window	       win;
    Position	       x;
    Position	       y;
    int	               delta, scrolled, can_go_down, arrow_start;
    struct _menu_item  *scroll_top;
    int                too_tall;
    long               timer;
    Dimension	       w;
    Dimension	       h;
    Dimension	       wLeft;
    Dimension	       wMiddle;
    struct _menu_state *prev;
} menu_state;

/* New fields for the Menu widget instance record */
typedef struct _MenuPart {
    /* public resources */
    Dimension       shadow_width;   /* shadow data */
    Dimension       requested_width;
    Boolean         be_nice_to_cmap;
    Pixel           top_shadow_pixel;
    Pixmap          top_shadow_pxmap;
    int             top_shadow_contrast;
    Pixel           bot_shadow_pixel;
    Pixmap          bot_shadow_pxmap;
    int             bot_shadow_contrast;
    Pixel	    foreground;	    /* foreground drawing */
    XFontStruct     *font;
#ifdef WX_USE_XFT
    XftFont         *xft_font;
#endif
    Boolean         horizontal;
    Boolean         forChoice;      /* Means extra left and right space */
    Boolean         forPopup;
    Dimension	    hmargin;	    /* margins around menu items */
    Dimension	    indicator_size; /* data for toggle, radio and cascade */
    Dimension       extra_left, extra_top, extra_right, extra_bottom;
    Pixel           indicator_pixel;
    Pixmap          indicator_pxmap;
    int             indicator_contrast;
    Pixel           highlight_pixel;
    Pixel           highlight_top_pixel;
    menu_item       *contents;	    /* menu structure */
    XtCallbackList  on_new_item;    /* callback procedures */
    XtCallbackList  on_select;
    XtCallbackList  on_no_select;
    XtCallbackList  on_destroy;
    Boolean         refresh;
    /* private data */
    Cursor          cursor;
    GC              normal_GC;
    GC              inactive_GC;
    GC              erase_GC;
    GC              top_shadow_GC;
    GC              bot_shadow_GC;
    GC              indicator_GC;
    GC              highlight_GC;
    GC              highlight_top_GC;
    Pixmap          stipple_pxmap;
    Boolean         popped_up;
    /* menu state */
    menu_state      *state;
    Boolean         grabbed;
    Boolean         moused_out;
} MenuPart;

/* Full instance record declaration */
typedef struct _MenuRec {
    CorePart	core;
    MenuPart	menu;
} MenuRec;

/* New fields for the Menu widget class record */
typedef struct { int dummy; } MenuClassPart;

/* Full class record declaration. */
typedef struct _MenuClassRec {
    CoreClassPart	core_class;
    MenuClassPart	menu_class;
} MenuClassRec;

/* Class pointer. */
extern MenuClassRec menuClassRec;

#endif /* _MenuP_h */
