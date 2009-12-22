/***********************************************************
Copyright 2004-2010 PLT Scheme Inc.
Copyright 1995 by Markus Holzem

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * Menu.c - Menu widget
 *
 */

#include <stdio.h>		/* for debugging purpose */

#include <ctype.h>

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/keysym.h>

#include <xwMenuP.h>
#include <xwTools3d.h>
#include <xwTabString.h>

#include <wxtimeout.h>

#include "wx_visual.h"
#include "wxAllocColor.h"
#include "xwGray.h"

extern int wxUseMenuHiliteBorder();

#define XtNtopShadowPixmap       "topShadowPixmap"
#define XtCTopShadowPixmap       "TopShadowPixmap"
#define XtNbottomShadowPixmap    "bottomShadowPixmap"
#define XtCBottomShadowPixmap    "BottomShadowPixmap"
#define XtNindicatorPixmap "indicatorShadowPixmap"
#define XtCIndicatorPixmap "IndicatorShadowPixmap"

#define offset(field) XtOffset(MenuWidget, field)
static XtResource MenuResources[] =
{ 
    /* cursor */
    {XtNcursor, XtCCursor, XtRCursor, sizeof(Cursor),
        offset(menu.cursor), XtRString, (XtPointer)"left_ptr"},
    /* font */
    {XtNfont,  XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        offset(menu.font),XtRString, "XtDefaultFont"},
#ifdef WX_USE_XFT
    {XtNmenuXftFont,  XtCMenuXftFont, XtRPointer, sizeof(void*),
        offset(menu.xft_font),XtRPointer, (XtPointer)0},
#endif

    /* border and shadow width */
    {XtNborderWidth, XtCBorderWidth, XtRDimension, sizeof(Dimension),
	XtOffsetOf(RectObjRec,rectangle.border_width), XtRImmediate,
	(XtPointer)0},
    {XtNshadowWidth, XtCShadowWidth, XtRDimension, sizeof(Dimension),
	offset(menu.shadow_width), XtRImmediate, (XtPointer) 2},
    {XtNrequestedWidth, XtCRequestedWidth, XtRDimension, sizeof(Dimension),
	offset(menu.requested_width), XtRImmediate, (XtPointer) 0},

    /* Foreground Colour */
    {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(menu.foreground), XtRString, "XtDefaultForeground"},

    /* Data to compute the top and bottom shadow GCs */
    {XtNbeNiceToColormap, XtCBeNiceToColormap, XtRBoolean, sizeof(Boolean),
	offset(menu.be_nice_to_cmap), XtRImmediate, (XtPointer) False},
    {XtNtopShadowPixel, XtCTopShadowPixel, XtRPixel, sizeof(Pixel),
	offset(menu.top_shadow_pixel), XtRImmediate, (XtPointer)-1},
    {XtNtopShadowPixmap, XtCTopShadowPixmap, XtRPixmap, sizeof(Pixmap),
	offset(menu.top_shadow_pxmap), XtRImmediate, (XtPointer)0},
    {XtNtopShadowContrast, XtCTopShadowContrast, XtRInt, sizeof(int),
	offset(menu.top_shadow_contrast), XtRImmediate, (XtPointer)120},
    {XtNbottomShadowPixel, XtCBottomShadowPixel, XtRPixel, sizeof(Pixel),
	offset(menu.bot_shadow_pixel), XtRImmediate, (XtPointer)-1},
    {XtNbottomShadowPixmap, XtCBottomShadowPixmap, XtRPixmap, sizeof(Pixmap),
	offset(menu.bot_shadow_pxmap), XtRImmediate, (XtPointer)0},
    {XtNbottomShadowContrast, XtCBottomShadowContrast, XtRInt, sizeof(int),
	offset(menu.bot_shadow_contrast), XtRImmediate, (XtPointer)60},

    /* data for toggle, radio and cascade indicators */
    {XtNindicatorPixel, XtCIndicatorPixel, XtRPixel, sizeof(Pixel),
         offset(menu.indicator_pixel), XtRImmediate, (XtPointer)-1},
    {XtNindicatorPixmap, XtCIndicatorPixmap, XtRPixmap, sizeof(Pixmap),
         offset(menu.indicator_pxmap), XtRImmediate, (XtPointer)0},
    {XtNindicatorContrast, XtCIndicatorContrast, XtRInt, sizeof(int),
         offset(menu.indicator_contrast), XtRImmediate, (XtPointer)85},
    {XtNindicatorSize, XtCIndicatorSize, XtRDimension, sizeof(Dimension),
        offset(menu.indicator_size), XtRImmediate, (XtPointer)0},

    {XtNextraLeft, XtCExtraLeft, XtRDimension, sizeof(Dimension),
       offset(menu.extra_left), XtRImmediate, (XtPointer)0},
    {XtNextraTop, XtCExtraTop, XtRDimension, sizeof(Dimension),
       offset(menu.extra_top), XtRImmediate, (XtPointer)0},
    {XtNextraRight, XtCExtraRight, XtRDimension, sizeof(Dimension),
       offset(menu.extra_right), XtRImmediate, (XtPointer)0},
    {XtNextraBottom, XtCExtraBottom, XtRDimension, sizeof(Dimension),
       offset(menu.extra_bottom), XtRImmediate, (XtPointer)0},

    {XtNhighlightPixel, XtCHighlightPixel, XtRPixel, sizeof(Pixel),
         offset(menu.highlight_pixel), XtRImmediate, (XtPointer)-1},

    {XtNhighlightShadowPixel, XtCHighlightShadowPixel, XtRPixel, sizeof(Pixel),
         offset(menu.highlight_top_pixel), XtRImmediate, (XtPointer)-1},

    /* margins around the menu items */
    {XtNhMargin, XtCHMargin, XtRDimension, sizeof(Dimension),
        offset(menu.hmargin), XtRImmediate, (XtPointer)1},

    {XtNhorizontal, XtCHorizontal, XtRBoolean, sizeof(Boolean),
	offset(menu.horizontal), XtRImmediate, (XtPointer) True},
    {XtNforChoice, XtCForChoice, XtRBoolean, sizeof(Boolean),
	offset(menu.forChoice), XtRImmediate, (XtPointer) False},
    {XtNforPopup, XtCForPopup, XtRBoolean, sizeof(Boolean),
	offset(menu.forPopup), XtRImmediate, (XtPointer) False},

    /* menu structure */
    {XtNmenu, XtCMenu, XtRPointer, sizeof(XtPointer),
        offset(menu.contents), XtRImmediate, (XtPointer)NULL},
    {XtNrefresh, XtCRefresh, XtRBoolean, sizeof(Boolean),
        offset(menu.refresh), XtRBoolean, (XtPointer)False},

    /* callbacks called on item change and item select */
    {XtNonNewItem, XtCCallback, XtRCallback, sizeof(XtPointer), 
        offset(menu.on_new_item), XtRCallback, (XtPointer)NULL},
    {XtNonSelect, XtCCallback, XtRCallback, sizeof(XtPointer), 
        offset(menu.on_select), XtRCallback, (XtPointer)NULL},
    {XtNonNoSelect, XtCCallback, XtRCallback, sizeof(XtPointer), 
        offset(menu.on_no_select), XtRCallback, (XtPointer)NULL},
    {XtNonMDestroy, XtCCallback, XtRCallback, sizeof(XtPointer), 
        offset(menu.on_destroy), XtRCallback, (XtPointer)NULL},
};
#undef offset

#define VMARGIN 2
#define ISPACING 4
#define CHOICE_LEFT 1
#define CHOICE_RIGHT 13

static void    MenuClassInitialize();
static void    MenuDestroy();
static void    MenuInitialize();
static void    MenuRealize();
static void    MenuRedisplay();
static void    MenuResize();
static Boolean MenuSetValues();

static void Start();
static void Drag();
static void Select();
static void Motion();
static void Key();

static XtActionsRec MenuActionsList [] = {
    {"start",	Start  },
    {"drag",	Drag   },
    {"select",	Select },
    {"motion",	Motion },
    {"key",	Key }
};

static char MenuTranslations [] = 
"<BtnDown>:		start()\n\
<Motion>:	        motion()\n\
Button1 <Motion>:	drag()\n\
Button2 <Motion>:	drag()\n\
Button3 <Motion>:	drag()\n\
<BtnUp>:		select()\n\
<KeyPress>:		key()\n\
";

#define SuperClass ((CoreWidgetClass)&coreClassRec)

MenuClassRec menuClassRec = {
  {
/* core_class fields */	
    /* superclass	  	*/	(WidgetClass) SuperClass,
    /* class_name	  	*/	"Menu",
    /* widget_size	  	*/	sizeof(MenuRec),
    /* class_initialize   	*/	MenuClassInitialize,
    /* class_part_initialize	*/	NULL,
    /* class_inited       	*/	FALSE,
    /* initialize	  	*/	MenuInitialize,
    /* initialize_hook	*/	NULL,
    /* realize		*/	MenuRealize,
    /* actions		*/	MenuActionsList,
    /* num_actions	*/	XtNumber(MenuActionsList),
    /* resources	  	*/	MenuResources,
    /* num_resources	*/	XtNumber(MenuResources),
    /* xrm_class	  	*/	NULLQUARK,
    /* compress_motion	*/	TRUE,
    /* compress_exposure  	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest	*/	FALSE,
    /* destroy		*/	MenuDestroy,
    /* resize		*/	MenuResize,
    /* expose		*/	MenuRedisplay,
    /* set_values	  	*/	MenuSetValues,
    /* set_values_hook	*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook	*/	NULL,
    /* accept_focus	*/	NULL,
    /* version		*/	XtVersion,
    /* callback_private   	*/	NULL,
    /* tm_table		*/	MenuTranslations,
    /* query_geometry	*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
/* Menu class fields initialization */
  {
    /* ignore 		*/	0
  }
};

WidgetClass menuWidgetClass = (WidgetClass) &menuClassRec;

void FreeTimer(long timer)
{
  if (timer) {
    wxRemoveTimeOut(timer);
  }
}

static int HasHotKey(char *l, int key)
{
  int i;

  if (!l)
    return 0;

  for (i = 0; l[i]; i++) {
    if ((l[i] == '&') 
	&& (((l[i+1] > 0)
	     && (key > 0)
	     && (key < 128)
	     && (tolower(l[i+1]) == tolower(key)))
	    || (l[i+1] == key)))
      return 1;
  }

  return 0;
}


/******************************************************************************
 *
 * Intrinsic Methods
 *
 *****************************************************************************/

static void CreateGCs(MenuWidget mw);
static void CreateShadowGCs(MenuWidget mw);
static void ReleaseGCs(MenuWidget mw);
static void ReleaseShadowGCs(MenuWidget mw);
static void ComputeMenuSize(MenuWidget mw, menu_state *ms);
static void DisplayMenu(MenuWidget mw, menu_state *ms);

/* ARGSUSED */
static void MenuInitialize(request, new, args, num_args)
    Widget request, new;
    ArgList args;
    Cardinal *num_args;
{
    MenuWidget mw   = (MenuWidget)new;

    CreateGCs(mw);
    CreateShadowGCs(mw);

    if (!mw->menu.indicator_size
	||  mw->menu.indicator_size > wx_ASCENT(mw->menu.font, mw->menu.xft_font))
      mw->menu.indicator_size = wx_ASCENT(mw->menu.font, mw->menu.xft_font);

    mw->menu.popped_up       = FALSE;
    mw->menu.state           = (menu_state*)XtMalloc(sizeof(menu_state));
    mw->menu.state->menu     = mw->menu.contents;
    mw->menu.state->selected = NULL;
    mw->menu.state->prev     = NULL;
    mw->menu.state->delta    = 0;
    mw->menu.state->scrolled = 0;
    mw->menu.state->timer    = 0;

    mw->menu.moused_out = 0;
    mw->menu.grabbed = FALSE;

    ComputeMenuSize(mw, mw->menu.state);
    mw->core.width  = mw->menu.state->w;
    mw->core.height = mw->menu.state->h;
}

/* ARGSUSED */
static void MenuClassInitialize()
{
}

/* ARGSUSED */
static void MenuRealize(w, value_mask, window_attributes)
    Widget               w;
    Mask                 *value_mask;
    XSetWindowAttributes *window_attributes;
{
    MenuWidget mw             = (MenuWidget)w;
    XSetWindowAttributes xswa;

    (*menuWidgetClass->core_class.superclass->core_class.realize)
	(w, value_mask, window_attributes);
    xswa.save_under = TRUE;
    xswa.cursor     = mw->menu.cursor;
    XChangeWindowAttributes(XtDisplay(w), XtWindow(w),
			    CWSaveUnder|CWCursor, &xswa);
    mw->menu.state->win = XtWindow(w);
    mw->menu.state->w   = w->core.width;
    mw->menu.state->h   = w->core.height;
}

/* ARGSUSED */
static void MenuRedisplay(w, ev, region)
    Widget  w;
    XEvent  *ev;
    Region  region;
{
    MenuWidget  mw = (MenuWidget)w;
    menu_state  *ms;

/* MATTHEW: Only translate menu bar.
    for (ms=mw->menu.state; ms->prev; ms=ms->prev);
    XtTranslateCoords(w, w->core.x, w->core.y, &ms->x, &ms->y);
*/

    for (ms=mw->menu.state; ms; ms=ms->prev)
	DisplayMenu(mw, ms);
}

/* ARGSUSED */
static void MenuDestroy(w)
    Widget  w;
{
    MenuWidget  mw  = (MenuWidget)w;
    menu_state  *ms = mw->menu.state,
	        *last;

    ReleaseGCs(mw);
    ReleaseShadowGCs(mw);
    while (ms != mw->menu.state) { /* don't destroy the widget's window! */
	XDestroyWindow(XtDisplay(mw), ms->win);
	last = ms;
	ms   = ms->prev;
	FreeTimer(last->timer);
	XtFree((char*)last);
    }
    FreeTimer(ms->timer);
    XtFree((char*)ms); /* free menu_state of widget's window */

    XtCallCallbackList(w, mw->menu.on_destroy, (XtPointer)NULL);
}

#define CHANGED_bg_  (new->core.background_pixel != old->core.background_pixel)
#define CHANGED(val) (new->menu.val != old->menu.val)
#define NEW(val)     (new->menu.val)
#define OLD(val)     (old->menu.val)

/* ARGUSED */
static Boolean MenuSetValues(gcurrent, grequest, gnew)
    Widget gcurrent, grequest, gnew;
{
    MenuWidget  old = (MenuWidget)gcurrent;
    MenuWidget  new = (MenuWidget)gnew;
    Boolean     redisplay;

    redisplay = (*SuperClass->core_class.set_values)(gcurrent, grequest, gnew, NULL, 0);

    {
      int ow, oh;
      ow = new->menu.state->w;
      oh = new->menu.state->h;
      new->menu.state->menu = new->menu.contents;
      ComputeMenuSize(new, new->menu.state);
      new->menu.state->w = new->core.width;
      new->menu.state->h = new->core.height;
      if ((ow != new->menu.state->w)
	  || (oh != new->menu.state->h)) {
	redisplay = TRUE;
      }
    }

    if (new->menu.refresh) {
      new->menu.refresh = 0;
      redisplay = TRUE;
    }

    if (CHANGED_bg_
	|| CHANGED(top_shadow_pixel) || CHANGED(top_shadow_contrast)
	|| CHANGED(bot_shadow_pixel) || CHANGED(bot_shadow_contrast)) {
	ReleaseShadowGCs(new);
	CreateShadowGCs(new);
	redisplay = TRUE;
    }
    if (CHANGED_bg_
	|| CHANGED(foreground) || CHANGED(font)
#ifdef WX_USE_XFT
	|| CHANGED(xft_font)
#endif
	|| CHANGED(indicator_pixel)  || CHANGED(indicator_contrast)) {
	ReleaseGCs(new);
	CreateGCs(new);
	redisplay = TRUE;
    }

    return redisplay;
}

#undef CHANGED_bg_
#undef CHANGED
#undef NEW
#undef OLD

/* ARGUSED */
static void MenuResize(w)
    Widget w;
{
    MenuWidget  mw = (MenuWidget)w;

    ComputeMenuSize(mw, mw->menu.state);
    mw->menu.state->w = mw->core.width;
    mw->menu.state->h = mw->core.height;
}
    

/******************************************************************************
 *
 * Action Procedures
 *
 *****************************************************************************/

static int HandleMotionEvent(MenuWidget mw, XMotionEvent *ev, int is_click);
static void UnhighlightItem(MenuWidget mw, menu_state *ms, menu_item *item);
static void HighlightItem(MenuWidget mw, menu_state *ms, menu_item *item);
static void MoveSelection(MenuWidget mw, int direction);

static void DoSelect(Widget w, Time time, int force);

extern void wxAddGrab(Widget);
extern void wxRemoveGrab(Widget);

/* ARGSUSED */
static void Start(w, ev, params, num_params)
    Widget    w;
    XEvent    *ev;
    String    *params;
    Cardinal  *num_params;
{
    MenuWidget  mw = (MenuWidget)w;

    XSync(XtDisplay(mw), FALSE);

    if (!mw->menu.state->prev) {
      mw->menu.state->x        = ev->xmotion.x_root - ev->xmotion.x;
      mw->menu.state->y        = ev->xmotion.y_root - ev->xmotion.y;
    }

    if (!mw->menu.grabbed) {
      XtGrabPointer((Widget)mw, FALSE,
		    (ButtonMotionMask | PointerMotionMask |
		     ButtonReleaseMask | ButtonPressMask),
		    GrabModeAsync, GrabModeAsync,
		    None, mw->menu.cursor, 
		    /* ev->xmotion.time */ CurrentTime);
      XtGrabKeyboard((Widget)mw, FALSE,
		     GrabModeAsync, GrabModeAsync,
		     /* ev->xmotion.time */ CurrentTime);
      wxAddGrab((Widget)mw);
      mw->menu.grabbed = TRUE;
    }

    if (!HandleMotionEvent(mw, &ev->xmotion, 1)
        /* *num_params is positive when called to start a popup menu */
        && !*num_params)
      DoSelect(w, CurrentTime, 1);
}

/* ARGSUSED */
static void Drag(w, event, params, num_params)
    Widget    w;
    XEvent    *event;
    String    *params;
    Cardinal  *num_params;
{
    MenuWidget    mw    = (MenuWidget)w;
    XMotionEvent  *ev   = &event->xmotion;
    int           x     = ev->x_root;
    int           y     = ev->y_root;
    int           state = ev->state;

    HandleMotionEvent(mw, ev, 0);
    XSync(XtDisplay(mw), FALSE);
    /* allow motion events to be generated again */
    if ((!ev->is_hint
	 || XQueryPointer(XtDisplay(mw), ev->window,
			  &ev->root, &ev->subwindow,
			  &ev->x_root, &ev->y_root,
			  &ev->x, &ev->y, &ev->state))
	&& ev->state == state
	&& (ev->x_root != x || ev->y_root != y)) {
      HandleMotionEvent(mw, ev, 0);
      XSync(XtDisplay(mw), FALSE);
    }
}

/* ARGSUSED */
static void DoSelect(w, time, force)
    Widget    w;
    Time      time;
    int       force;
{
    MenuWidget  mw = (MenuWidget)w;
    menu_item   *selected_item = mw->menu.state->selected;
    menu_state  *ms;

    if (!force && !selected_item)
      return;

    if (mw->menu.grabbed) {
      XtUngrabPointer(w,time);
      XtUngrabKeyboard(w, time);
      wxRemoveGrab(w);
      mw->menu.grabbed = FALSE;
    }

    for (ms=mw->menu.state; ms->prev!=NULL; ms=ms->prev)
	;
    UnhighlightItem(mw, ms, ms->selected);
    ms->selected = NULL;
    ms->delta = 0;
    if (mw->menu.popped_up) {
	mw->menu.popped_up = FALSE;
	XtPopdown(XtParent(mw));
    }

    XFlush(XtDisplay(mw));

    if (selected_item
	&& selected_item->enabled 
	&& selected_item->type != MENU_TEXT
	&& selected_item->type != MENU_SEPARATOR
	&& selected_item->type != MENU_PUSHRIGHT)
      XtCallCallbackList(w, mw->menu.on_select, (XtPointer)selected_item);
    else
      XtCallCallbackList(w, mw->menu.on_no_select, (XtPointer)NULL);
}

static void Select(w, event, params, num_params)
    Widget    w;
    XEvent    *event;
    String    *params;
    Cardinal  *num_params;
{
  MenuWidget  mw = (MenuWidget)w;
  XMotionEvent  *ev   = event ? &event->xmotion : NULL;
  int force;

  mw->menu.moused_out = 0;

  if (!mw->menu.forPopup 
      && (!mw->menu.state || !mw->menu.state->selected))
    return;

  force = !HandleMotionEvent(mw, ev, 0);
  if (!force)
    force = mw->menu.moused_out;
  
  DoSelect(w, event ? event->xmotion.time : 0L, force);
}

static void Motion(w, event, params, num_params)
    Widget    w;
    XEvent    *event;
    String    *params;
    Cardinal  *num_params;
{
  MenuWidget  mw = (MenuWidget)w;

  if (mw->menu.grabbed)
    Drag(w, event, params, num_params);
}

static void Key(w, event, params, num_params)
    Widget    w;
    XEvent    *event;
    String    *params;
    Cardinal  *num_params;
{
  MenuWidget  mw = (MenuWidget)w;
  XKeyEvent  *ev   = &event->xkey;
  KeySym	   keysym;

  (void)XLookupString(ev, NULL, 0, &keysym, NULL);

  switch (keysym) {
  case XK_Right:
  case XK_Left:
    if (mw->menu.state && mw->menu.state->prev && mw->menu.state->prev->prev) {
      /* In a submenu */
      if (keysym == XK_Right) {
	if (!mw->menu.state->selected) {
	  /* select first in submenu */
	  if (mw->menu.state->menu)
	    HighlightItem(mw, mw->menu.state, mw->menu.state->menu);
	}
      } else {
	if (mw->menu.state->selected) {
	  UnhighlightItem(mw, mw->menu.state, mw->menu.state->selected);
	}
      }
    } else {
      /* Change top-level menu: */
      if (mw->menu.state && mw->menu.state->prev && mw->menu.state->prev->selected) {
	menu_item *item, *orig_item;
	item = orig_item = mw->menu.state->prev->selected;
	do {
	  if (keysym == XK_Right) {
	    if (item->next)
	      item = item->next;
	    else
	      item = mw->menu.state->prev->menu;
	  } else {
	    if (item->prev)
	      item = item->prev;
	    else {
	      while (item->next)
		item = item->next;
	    }
	  }
	} while (item && (item != orig_item) && !item->enabled);
	if (item && (item != orig_item)) {
	  UnhighlightItem(mw, mw->menu.state->prev, mw->menu.state->prev->selected);
	  HighlightItem(mw, mw->menu.state, item);
	}
      }
    }
    break;
  case XK_Down:
    MoveSelection(mw, 1);
    break;
  case XK_Up:
    MoveSelection(mw, -1);
    break;
  case XK_Escape:
    if (mw->menu.state->selected)
      UnhighlightItem(mw, mw->menu.state, mw->menu.state->selected);
    DoSelect(w, event ? event->xkey.time : 0L, 1);
    break;
  case XK_Return:
    DoSelect(w, event ? event->xkey.time : 0L, 1);
    break;
  default:
    /* Look for menu to select */
    {
      menu_item *item;
      if (mw->menu.state->prev)
	item = mw->menu.state->prev->selected->contents;
      else
	item = NULL;
      for (; item; item = item->next) {
	if (item->enabled && HasHotKey(item->label, keysym)) {
	  if (mw->menu.state->selected != item) {
	    UnhighlightItem(mw, mw->menu.state, mw->menu.state->selected);
	    HighlightItem(mw, mw->menu.state, item);
	  }
	  if (!item->contents)
	    DoSelect(w, event ? event->xkey.time : 0L, 1);
	  break;
	}
      }
    }
    break;
  }
}


/******************************************************************************
 *
 * Create and Release GCs Code
 *
 *****************************************************************************/

extern Boolean  get_scaled_color(Widget,float ,Pixel ,Pixel *);

static void CreateGCs(MenuWidget mw)
{
    Display    *dpy = XtDisplay((Widget)mw);
    Screen     *scr = XtScreen((Widget)mw);
    Window     win  = RootWindowOfScreen(DefaultScreenOfDisplay(dpy));
    XGCValues  xgcv;
    int        gcf_flag = 0;

    mw->menu.stipple_pxmap = XCreatePixmapFromBitmapData(dpy, win,
				 wx_gray_bits, wx_gray_width, wx_gray_height, 1, 0, 1);

    if (mw->menu.font) {
      xgcv.font = mw->menu.font->fid;
      gcf_flag = GCFont;
    }

    xgcv.foreground = mw->core.background_pixel;
    xgcv.background = mw->menu.foreground;
    mw->menu.erase_GC = XtGetGC((Widget)mw,
				gcf_flag|GCForeground|GCBackground,
				&xgcv);

    xgcv.foreground = mw->menu.foreground;
    xgcv.background = mw->core.background_pixel;
    mw->menu.normal_GC = XtGetGC((Widget)mw,
				 gcf_flag|GCForeground|GCBackground,
				 &xgcv);
    
    if (wx_enough_colors(scr)) {
      Pixel r;
      get_scaled_color((Widget)mw, 0.6, xgcv.background, &r);
      xgcv.foreground = r;
      mw->menu.inactive_GC = XtGetGC((Widget)mw,
				     gcf_flag|GCForeground|GCBackground,
				     &xgcv);
    } else {
      xgcv.fill_style = FillStippled;
      xgcv.stipple    = mw->menu.stipple_pxmap;
      mw->menu.inactive_GC = XtGetGC((Widget)mw,
				     gcf_flag|GCForeground|GCBackground|
				     GCFillStyle|GCStipple,
				     &xgcv);
    }

    if (DefaultDepthOfScreen(scr) == 1) {
	mw->menu.indicator_pxmap = Xaw3dAllocPixmap((Widget)mw,
						    mw->core.background_pixel,
						    GRAY);
    } else {
	if (mw->menu.indicator_pixel == -1) {
	  Pixel res;
	  get_scaled_color((Widget)mw, 
			   mw->menu.indicator_contrast/100.0, 
			   mw->core.background_pixel, &res);
	  mw->menu.indicator_pixel = res;
	  mw->menu.indicator_pxmap = (Pixmap)0;
	}
	if (mw->menu.highlight_pixel == -1) {
	  XColor color;
	  color.red = 0;
	  color.green = 0;
	  color.blue = 180 << 8;
	  wxAllocColor(XtDisplay(mw), wx_default_colormap, &color);
	  mw->menu.highlight_pixel = color.pixel;
	}
	if (mw->menu.highlight_top_pixel == -1) {
	  Pixel res;
	  get_scaled_color((Widget)mw, 1.35, mw->menu.highlight_pixel, &res);
	  mw->menu.highlight_top_pixel = res;
	}
    }
    mw->menu.indicator_GC = Xaw3dGetGC((Widget)mw, 0,
				       mw->menu.indicator_pxmap,
				       mw->menu.indicator_pixel);
    mw->menu.highlight_GC = Xaw3dGetGC((Widget)mw, 0,
				       0,
				       mw->menu.highlight_pixel);
    mw->menu.highlight_top_GC = Xaw3dGetGC((Widget)mw, 0,
					   0,
					   mw->menu.highlight_top_pixel);
}

static void CreateShadowGCs(MenuWidget mw)
{
    Screen  *scr = XtScreen((Widget)mw);
    Pixel   bg   = mw->core.background_pixel, res;

    if (DefaultDepthOfScreen (scr) == 1) {
	mw->menu.top_shadow_pxmap = Xaw3dAllocPixmap((Widget)mw, bg, LIGHTER);
	mw->menu.bot_shadow_pxmap = Xaw3dAllocPixmap((Widget)mw, bg, DARKER);
    } else {
	if (mw->menu.top_shadow_pixel == -1) {
	  get_scaled_color((Widget)mw, mw->menu.top_shadow_contrast/100.0, bg, &res);
	  mw->menu.top_shadow_pixel = res;
	  mw->menu.top_shadow_pxmap = (Pixmap)0;
	}
	if (mw->menu.bot_shadow_pixel == -1) {
	  get_scaled_color((Widget)mw, mw->menu.bot_shadow_contrast/100.0, bg, &res);
	  mw->menu.bot_shadow_pixel = res;
	  mw->menu.bot_shadow_pxmap = (Pixmap)0;
	}
    }
    mw->menu.top_shadow_GC = Xaw3dGetGC((Widget)mw, 0,
					mw->menu.top_shadow_pxmap,
					mw->menu.top_shadow_pixel);
    mw->menu.bot_shadow_GC = Xaw3dGetGC((Widget)mw, 0,
					mw->menu.bot_shadow_pxmap,
					mw->menu.bot_shadow_pixel);
}

static void ReleaseGCs(MenuWidget mw)
{
    XtReleaseGC((Widget)mw, mw->menu.erase_GC);
    XtReleaseGC((Widget)mw, mw->menu.normal_GC);
    XtReleaseGC((Widget)mw, mw->menu.inactive_GC);
    XFreePixmap(XtDisplay(mw), mw->menu.stipple_pxmap);
    Xaw3dReleaseGC(mw, mw->menu.indicator_GC);
    Xaw3dFreePixmap(mw, mw->menu.indicator_pxmap);
    Xaw3dReleaseGC(mw, mw->menu.highlight_GC);
}

static void ReleaseShadowGCs(MenuWidget mw)
{
    Xaw3dReleaseGC(mw, mw->menu.top_shadow_GC);
    Xaw3dReleaseGC(mw, mw->menu.bot_shadow_GC);
    Xaw3dFreePixmap(mw, mw->menu.top_shadow_pxmap);
    Xaw3dFreePixmap(mw, mw->menu.bot_shadow_pxmap);
    Xaw3dReleaseGC(mw, mw->menu.highlight_top_GC);
}


/******************************************************************************
 *
 * Size Code
 *
 *****************************************************************************/

/* Utilities */

static unsigned StringWidth(MenuWidget mw, char *s)
{
    return XfwfTextWidth(XtDisplay(mw),
			 mw->menu.font, wxEXT_FONT(mw->menu.xft_font), 
			 s, strlen(s), NULL);
}

static void GetResourceName(char *in, char *out)
{
    char *first = out;

    while (*in)
	if (isalnum((unsigned char)*in) || (*in)=='_')
	    *out++ = *in++;
	else
	    in++;
    *first = tolower(*first);
    *out   = '\0';
}

/*
Moved to Menu.c

typedef enum _e_Subresource {
    SUBRESOURCE_LABEL = 0,
    SUBRESOURCE_HELP = 1,
    SUBRESOURCE_KEY = 2
} Subresource;
*/

char *ResourcedText(MenuWidget mw, menu_item *item, Subresource type)
{
    static XtResource labelResource[] = {
	{ "label", "Label", XtRString, sizeof(String), 0, XtRImmediate, 0 },
	{ "help",  "Help",  XtRString, sizeof(String), 0, XtRImmediate, 0 },
	{ "key",   "Key",   XtRString, sizeof(String), 0, XtRImmediate, 0 },
    };

    char resource_name[1024];
    char *resourced_text=NULL;

    GetResourceName(item->label, resource_name);
    XtGetSubresources((Widget)mw, (XtPointer)(&resourced_text),
		      resource_name, resource_name, &labelResource[type],
		      1, NULL, 0);
    if (!resourced_text)
	switch (type) {
	case SUBRESOURCE_LABEL:  return item->label;
	case SUBRESOURCE_HELP:   return item->help_text;
	case SUBRESOURCE_KEY:    return item->key_binding;
	}
    return (resourced_text);
}

/* Size Functions */

static void MenuTextSize(MenuWidget mw, menu_item *item, Boolean in_menubar,
			 unsigned *l, unsigned *m, unsigned *r, unsigned *h)
{
  *h = (wx_ASCENT(mw->menu.font, mw->menu.xft_font) 
	+ wx_DESCENT(mw->menu.font, mw->menu.xft_font)
	+ 2*VMARGIN + 2*mw->menu.shadow_width);
  *l = *r = mw->menu.hmargin + mw->menu.shadow_width;
  if (mw->menu.forChoice) {
    *l += CHOICE_LEFT;
    *r += CHOICE_RIGHT;
  }
  *m = StringWidth(mw, ResourcedText(mw, item, SUBRESOURCE_LABEL));
}

static void MenuButtonSize(MenuWidget mw, menu_item *item, Boolean in_menubar,
			   unsigned *l, unsigned *m, unsigned *r, unsigned *h)
{
    MenuTextSize(mw, item, in_menubar, l, m, r, h);
    if (!in_menubar && item->key_binding)
	*r += StringWidth(mw, ResourcedText(mw, item, SUBRESOURCE_KEY)) 
	      + (3 * ISPACING);
}

static void MenuToggleSize(MenuWidget mw, menu_item *item, Boolean in_menubar,
			   unsigned *l, unsigned *m, unsigned *r, unsigned *h)
{
    MenuButtonSize(mw, item, in_menubar, l, m, r, h);
    *l += mw->menu.indicator_size + ISPACING;
}

static void MenuCascadeSize(MenuWidget mw, menu_item *item, Boolean in_menubar,
			    unsigned *l, unsigned *m, unsigned *r, unsigned *h)
{
    MenuTextSize(mw, item, in_menubar, l, m, r, h);
    if (!in_menubar)
	*r += mw->menu.indicator_size + ISPACING;
}

static void MenuPushrightSize(MenuWidget mw,menu_item *item,Boolean in_menubar,
			      unsigned *l,unsigned *m,unsigned *r,unsigned *h)
{
    *l = *m =  *r = *h = 0;
}

static void MenuSeparatorSize(MenuWidget mw,menu_item *item,Boolean in_menubar,
			      unsigned *l,unsigned *m,unsigned *r,unsigned *h)
{
    *l = *m =  *r = *h = 0;
    if (!in_menubar) {
	*h = mw->menu.shadow_width;
	*m = 1;
    }
}

typedef void (*SizeFunction)(MenuWidget, menu_item*, Boolean, unsigned*,
			     unsigned*, unsigned*, unsigned*);
static SizeFunction SizeFunctionList[] = {
    MenuTextSize,
    MenuButtonSize,
    MenuToggleSize,
    MenuToggleSize,
    MenuCascadeSize,
    MenuSeparatorSize,
    MenuPushrightSize,
    MenuCascadeSize
};

/* Compute Menu */

#define TOO_TALL_SCROLL_HEIGHT 14

static void ComputeMenuSize(MenuWidget mw, menu_state *ms)
{
    unsigned  left_width, label_width, right_width, height;
    unsigned  max_left_width, max_label_width, max_right_width, max_height;
    unsigned  screen_h, usable_h, vis_count = 0, item_count = 0;
    Boolean   in_menubar = (mw->menu.horizontal && !ms->prev);
    menu_item *item, *pushright_item=NULL;

    screen_h = HeightOfScreen(XtScreen(mw));
    usable_h = screen_h - (2*mw->menu.shadow_width + 2*TOO_TALL_SCROLL_HEIGHT);

    ms->too_tall = 0;

    max_left_width = max_label_width = max_right_width = max_height = 0;
    for (item=ms->menu; item; item=item->next) {
	SizeFunctionList[item->type](mw, item, in_menubar, &left_width,
				     &label_width, &right_width, &height);
#       define SET_MAX_VALUE(val) if (val > max_##val) max_##val = val;	
	if (in_menubar) {
	    if (!pushright_item && item->type == MENU_PUSHRIGHT)
		pushright_item = item;
	    if ((item->type == MENU_HELP) && !item->next 
		&& (mw->core.parent->core.parent->core.width > max_label_width + left_width + label_width + right_width)) {
	      item->start      = mw->core.parent->core.parent->core.width - (left_width + label_width + right_width) - mw->menu.shadow_width;
	      item->end        = item->start + left_width + label_width + right_width;
	      max_label_width  = mw->core.parent->core.parent->core.width;
	    } else {
	      item->start      = max_label_width + mw->menu.shadow_width;
	      max_label_width += left_width + label_width + right_width;
	      item->end        = max_label_width + mw->menu.shadow_width;
	    }
	    SET_MAX_VALUE(height);
	} else {
	    SET_MAX_VALUE(left_width);
	    SET_MAX_VALUE(label_width);
	    SET_MAX_VALUE(right_width);
	    if ((max_height + height) < usable_h) {
	      vis_count++;
	    } else if (!ms->too_tall) {
	      screen_h = max_height + 2*TOO_TALL_SCROLL_HEIGHT;
	      ms->too_tall = 1;
	    }
	    item->start  = max_height + mw->menu.shadow_width;
	    max_height  += height;
	    item->end    = max_height + mw->menu.shadow_width;
	    item_count++;
	}
#       undef SET_MAX_VALUE
    }

    if (ms->too_tall)
      max_height = screen_h;

    if (!max_height && in_menubar) {
      /* For menu bar: make it at least as tall as with an item */
      max_height = (wx_ASCENT(mw->menu.font, mw->menu.xft_font) 
		    + wx_DESCENT(mw->menu.font, mw->menu.xft_font)
		    + 2*VMARGIN + 2*mw->menu.shadow_width);
    }
    ms->w       = max_left_width + max_label_width + max_right_width
	          + 2*mw->menu.shadow_width;
    if (ms->w < mw->menu.requested_width)
      ms->w = mw->menu.requested_width;
    ms->h       = max_height + 2*mw->menu.shadow_width;
    ms->wLeft   = max_left_width;
    ms->wMiddle = max_label_width;
    if (in_menubar) {
	if (pushright_item)  pushright_item->end = ms->w - pushright_item->end;
	ms->wLeft = mw->menu.hmargin + mw->menu.shadow_width;
    }
}


/******************************************************************************
 *
 * Display Code
 *
 *****************************************************************************/

/* Draw Functions */

static void DrawTextItem(MenuWidget mw, menu_state *ms, menu_item *item,
			 unsigned x, unsigned y)
{
    Boolean    in_menubar = (mw->menu.horizontal && !ms->prev);
    Dimension  extra_x = 0;
    char       *label;
    int on, width, height;

    if (in_menubar) {
	if (item->type == MENU_TOGGLE || item->type == MENU_RADIO) {
	    extra_x = mw->menu.indicator_size + ISPACING;
	}
    }

    on = ((ms->selected==item) && (item->enabled));

    width = (in_menubar? item->end-item->start : ms->w-2*mw->menu.shadow_width);
    height = (in_menubar? ms->h-2*mw->menu.shadow_width : item->end-item->start);

    XFillRectangle(XtDisplay(mw), ms->win, 
		   (on ? mw->menu.highlight_GC : mw->menu.erase_GC), 
		   x, y, width, height);

    if ((label=ResourcedText(mw, item, SUBRESOURCE_LABEL))) {
      XfwfDrawString(XtDisplay(mw), ms->win,
		     (wxEXT_FONT(mw->menu.xft_font)
		      ? (on
			 ? mw->menu.highlight_GC
			 : mw->menu.erase_GC)
		      : ((item->enabled || item->type==MENU_TEXT) 
			 ? (on
			    ? mw->menu.erase_GC 
			    : mw->menu.normal_GC)
			 : mw->menu.inactive_GC)),
		     x+ms->wLeft+extra_x,
		     y+mw->menu.shadow_width+VMARGIN+wx_ASCENT(mw->menu.font,
							       mw->menu.xft_font),
		     label, strlen(label), NULL,
		     mw->menu.font, wxEXT_FONT(mw->menu.xft_font), 
		     (on ? -1 : (item->enabled || item->type==MENU_TEXT)), 
		     1, NULL, 1);
    }

    if (wxUseMenuHiliteBorder()) {
      if (item->enabled && item->type!=MENU_TEXT)
	Xaw3dDrawRectangle(
	    XtDisplay((Widget)mw), ms->win,
	    (on
	     ? mw->menu.highlight_top_GC
	     : mw->menu.top_shadow_GC),
	    mw->menu.bot_shadow_GC,
	    (on
	     ? mw->menu.highlight_GC
	     : mw->menu.erase_GC),
	    mw->menu.indicator_GC,
	    x,
	    y,
	    width,
	    height,
	    mw->menu.shadow_width,
	    (ms->selected==item) ? XAW3D_OUT_HARD : XAW3D_BACKGROUND);
    }
}

static void DrawButtonItem(MenuWidget mw, menu_state *ms, menu_item *item,
			   unsigned x, unsigned y)
{
    char *key;

    DrawTextItem(mw, ms, item, x, y);
    if ((!mw->menu.horizontal || ms->prev)
	&& (key=ResourcedText(mw, item, SUBRESOURCE_KEY))) {
      int on;
      on = ((ms->selected==item) && (item->enabled));
      XfwfDrawString(XtDisplay(mw), ms->win,
		     (wxEXT_FONT(mw->menu.xft_font)
		      ? (on
			 ? mw->menu.highlight_GC 
			 : mw->menu.erase_GC)
		      : (item->enabled 
			 ? (on
			    ? mw->menu.erase_GC 
			    : mw->menu.normal_GC)
			 : mw->menu.inactive_GC)),
		     x+ms->wLeft+ms->wMiddle+(3 * ISPACING),
		     y+mw->menu.shadow_width+VMARGIN+wx_ASCENT(mw->menu.font,
							       mw->menu.xft_font),
		     key, strlen(key), NULL, mw->menu.font, 
		     wxEXT_FONT(mw->menu.xft_font),
		     (on ? -1 : item->enabled), 1, NULL, 1);
    }
}

static void DrawRadioItem(MenuWidget mw, menu_state *ms, menu_item *item,
			  unsigned x, unsigned y)
{
    DrawButtonItem(mw, ms, item, x, y);
    Xaw3dDrawRadio(XtDisplay((Widget)mw), ms->win,
		   mw->menu.top_shadow_GC,
		   mw->menu.bot_shadow_GC,
		   mw->menu.indicator_GC,
		   mw->menu.erase_GC,
		   item->enabled ? mw->menu.normal_GC : mw->menu.inactive_GC,
		   x+mw->menu.shadow_width+mw->menu.hmargin,
		   y+mw->menu.shadow_width+VMARGIN
		   +(wx_ASCENT(mw->menu.font,
			       mw->menu.xft_font)
		     +wx_DESCENT(mw->menu.font,
				 mw->menu.xft_font)
		     -mw->menu.indicator_size)/2,
		   mw->menu.indicator_size, mw->menu.shadow_width, 
		   item->set);
}

static void DrawToggleItem(MenuWidget mw, menu_state *ms, menu_item *item,
			   unsigned x, unsigned y)
{
    DrawButtonItem(mw, ms, item, x, y);
    if (item->set) {
      Display *dpy = XtDisplay((Widget)mw);
      Window win = ms->win;
      GC gc;
      int h, h2, h4, h34;

      x += mw->menu.shadow_width + mw->menu.hmargin;
      y += (mw->menu.shadow_width + VMARGIN
	    + (wx_ASCENT(mw->menu.font,
			 mw->menu.xft_font)
	       + wx_DESCENT(mw->menu.font,
			   mw->menu.xft_font)
	       - mw->menu.indicator_size)/2) + 1;
      h = mw->menu.indicator_size - 2;
      h2 = h / 2;
      h4 = h / 4;
      h34 = h - h4;

      gc = (item->enabled 
	    ? ((ms->selected==item)
	       ? mw->menu.erase_GC 
	       : mw->menu.normal_GC)
	    : mw->menu.inactive_GC);

      XDrawLine(dpy, win, gc, x + h4, y + h34, x + h2, y + h);
      XDrawLine(dpy, win, gc, x + h2, y + h, x + h, y);

      x++;

      XDrawLine(dpy, win, gc, x + h4, y + h34, x + h2, y + h);
      XDrawLine(dpy, win, gc, x + h2, y + h, x + h, y);
    }
}

static void DrawCascadeItem(MenuWidget mw, menu_state *ms, menu_item *item,
			    unsigned x, unsigned y)
{
    DrawTextItem(mw, ms, item, x, y);
    if (!mw->menu.horizontal || ms->prev) {
      int on, size;
      on = item->enabled && ms->selected==item;
      size = mw->menu.indicator_size;
      if (size & 0x1) size--;
      Xaw3dDrawArrow(XtDisplay((Widget)mw), ms->win,
		     mw->menu.top_shadow_GC,
		     mw->menu.bot_shadow_GC,
		     (on ? mw->menu.top_shadow_GC : mw->menu.normal_GC),
		     (on ? mw->menu.top_shadow_GC : mw->menu.normal_GC),
		     x+ms->w
		     -(3*mw->menu.shadow_width
		       +mw->menu.hmargin
		       +mw->menu.indicator_size),
		     y+mw->menu.shadow_width+VMARGIN
		     +(wx_ASCENT(mw->menu.font,
				 mw->menu.xft_font)
		       + wx_DESCENT(mw->menu.font,
				    mw->menu.xft_font)
		       - size)/2,
		     size, size, 0,
		     RIGHT, 0);
    }
}

static void DrawSeparatorItem(MenuWidget mw, menu_state *ms, menu_item *item,
			      unsigned x, unsigned y)
{
    if (!mw->menu.horizontal ||ms->prev)
	Xaw3dDrawLine(
	    XtDisplay((Widget)mw), ms->win,
	    mw->menu.top_shadow_GC,
	    mw->menu.bot_shadow_GC,
	    mw->menu.normal_GC,
	    x, y, ms->w, mw->menu.shadow_width, FALSE, XAW3D_ETCHED_IN);
}

static void DrawPushrightItem(MenuWidget mw, menu_state *ms, menu_item *item,
			      unsigned x, unsigned y)
{
}

typedef void (*DrawFunction)(MenuWidget, menu_state*, menu_item*,
			     unsigned, unsigned);
static DrawFunction DrawFunctionList[] = {
    DrawTextItem,
    DrawButtonItem,
    DrawRadioItem,
    DrawToggleItem,
    DrawCascadeItem,
    DrawSeparatorItem,
    DrawPushrightItem,
    DrawCascadeItem
};

/* Draw Menu */

static void DisplayMenu(MenuWidget mw, menu_state *ms)
{
    menu_item *item;
    unsigned  x, y;
    int s, final;
    Boolean   in_menubar = (mw->menu.horizontal && !ms->prev);

    x = y = mw->menu.shadow_width;
    item = ms->menu;
    if (ms->too_tall) {
      if (ms->scrolled) {
	Xaw3dDrawArrow(XtDisplay((Widget)mw), ms->win,
		       mw->menu.top_shadow_GC,
		       mw->menu.bot_shadow_GC,
		       mw->menu.normal_GC,
		       mw->menu.normal_GC,
		       x + ((ms->w - TOO_TALL_SCROLL_HEIGHT) / 2),
		       y + 2,
		       TOO_TALL_SCROLL_HEIGHT - 4, TOO_TALL_SCROLL_HEIGHT - 4,
		       0,
		       UP,
		       0);
      }
      y += TOO_TALL_SCROLL_HEIGHT;
      for (s = ms->scrolled; s--; ) {
	if (item) {
	  y = item->end + ms->delta;
	  item = item->next;
	}
      }
      final = (ms->h - (TOO_TALL_SCROLL_HEIGHT + mw->menu.shadow_width)) - ms->delta;
    } else
      final = 35000;
    for (; item && (item->end < final); item=item->next) {
        if (item->type == MENU_HELP)
	  x = item->start;
	DrawFunctionList[item->type](mw, ms, item, x, y);
	if (in_menubar) {
	    if (item->type == MENU_PUSHRIGHT) {
		if (x+item->end <= ms->w)
		    x = ms->w - item->end;
	    } else
		x = item->end;
	} else {
	  y = item->end + ms->delta;
	}
    }

    ms->arrow_start = y;
    if (ms->too_tall && item) {
      y = ms->h - (TOO_TALL_SCROLL_HEIGHT + mw->menu.shadow_width);
      Xaw3dDrawArrow(XtDisplay((Widget)mw), ms->win,
		     mw->menu.top_shadow_GC,
		     mw->menu.bot_shadow_GC,
		     mw->menu.normal_GC,
		     mw->menu.normal_GC,
		     x + ((ms->w - TOO_TALL_SCROLL_HEIGHT) / 2),
		     y + 2,
		     TOO_TALL_SCROLL_HEIGHT - 4, TOO_TALL_SCROLL_HEIGHT - 4,
		     0,
		     DOWN,
		     0);
      ms->can_go_down = 1;
    } else
      ms->can_go_down = 0;

    Xaw3dDrawRectangle(
	XtDisplay((Widget)mw), ms->win,
	mw->menu.top_shadow_GC,
	mw->menu.bot_shadow_GC,
	mw->menu.erase_GC,
	mw->menu.indicator_GC,
	0, 0, ms->w, ms->h, 
	in_menubar ? 1 : mw->menu.shadow_width,
	in_menubar ? XAW3D_OUT : XAW3D_OUT_HARD);
}


/******************************************************************************
 *
 * Highlight and Unhighlight Code
 *
 *****************************************************************************/

/* Utilities */

static void ComputeItemPos(MenuWidget mw, menu_state *ms, menu_item *item, 
			   unsigned *x, unsigned *y)
{
    if (!ms->prev && mw->menu.horizontal) { /* in menubar ? */
	menu_item  *i;
	Dimension  pushright = 0;
	for (i=ms->menu; i && i!=item; i=i->next)
	    if (!pushright && i->type==MENU_PUSHRIGHT)
		pushright = ms->w - i->end - i->start;
	*x = item->start + pushright;
	*y = mw->menu.shadow_width;
    } else {
	*x = mw->menu.shadow_width;
	*y = item->start + ms->delta;
    }
}

static void MakeNewMenuWindow(MenuWidget mw, menu_state *prev, menu_item *item,
			      unsigned x, unsigned y)
{
    int        scr_width  = WidthOfScreen(XtScreen(mw));
    int        scr_height = HeightOfScreen(XtScreen(mw));
    menu_state *new       = (menu_state*)XtMalloc(sizeof(menu_state));
    int        mask;
    XSetWindowAttributes xswa;

    if (mw->menu.state->timer) {
      FreeTimer(mw->menu.state->timer);
      mw->menu.state->timer = 0;
    }

    /* Create new menu_state, initialize it and compute menu size */
    new->menu      = item->contents;
    new->selected  = NULL;
    new->prev      = prev;
    new->timer     = 0;
    mw->menu.state = new;
    ComputeMenuSize(mw, new);
    new->delta     = (new->too_tall ? TOO_TALL_SCROLL_HEIGHT : 0);
    new->scrolled  = 0;
    new->scroll_top = new->menu;

    /* position window on screen */
    if (mw->menu.horizontal && !prev->prev) { /* item in menubar? */
	new->x = prev->x + x;
	if (new->x + new->w > scr_width)
	    new->x = scr_width -  new->w;
	new->y = prev->y + prev->h - mw->menu.shadow_width;
	if (new->y + new->h > scr_height) /* menu doesn't below menubar -> */
	    if (new->y > scr_height/2) /* is more place above the menubar ?*/
		new->y = prev->y - new->h +mw->menu.shadow_width;
    } else {
	if (prev->x + prev->w + new->w < scr_width) /* place right of menu? */
	    new->x = prev->x + prev->w;
	else if (prev->x - new->w > 0)              /* place left of menu? */
	    new->x = prev->x - new->w;
	else			                    /* place on screen border*/
	    new->x = scr_width - new->w;

	new->y = prev->y + y - mw->menu.shadow_width;
	if (new->y + new->h > scr_height)
	    new->y = scr_height - new->h;
    }

    /* Create new window */
    xswa.save_under        = TRUE;
    xswa.override_redirect = TRUE;
    xswa.background_pixel  = mw->core.background_pixel;
    xswa.border_pixel      = mw->core.background_pixel;
    xswa.event_mask        = ExposureMask | ButtonMotionMask | 
	                     PointerMotionMask | ButtonReleaseMask |
	                     ButtonPressMask;
    xswa.cursor            = mw->menu.cursor;
    xswa.colormap          = wx_default_colormap;
    mask                   = (CWSaveUnder | CWOverrideRedirect | CWBackPixel
			      | CWBorderPixel | CWEventMask | CWCursor
			      | CWColormap);

    new->win = XCreateWindow(XtDisplay(mw),
			     RootWindowOfScreen(DefaultScreenOfDisplay(XtDisplay(mw))),
			     new->x, new->y, new->w, new->h, 0,
			     wx_visual_depth, InputOutput, wxAPP_VISUAL,
			     mask, &xswa);
}

static void HighlightItem(MenuWidget mw, menu_state *ms, menu_item *item)
{
    unsigned x, y;

    if (!item) /* nothing to highlight */
	return;
    ms->selected = item;
    ComputeItemPos(mw, ms, item, &x, &y);
    DrawFunctionList[item->type](mw, ms, item, x, y);
    if (((item->type == MENU_CASCADE) || (item->type == MENU_HELP)) && item->enabled) {
	MakeNewMenuWindow(mw, ms, item, x, y);
	XClearWindow(XtDisplay(mw), mw->menu.state->win);
	XMapRaised(XtDisplay(mw), mw->menu.state->win);
	DisplayMenu(mw, mw->menu.state);
    }
}

static void UnhighlightItem(MenuWidget mw, menu_state *ms, menu_item *item)
{
    unsigned   x, y;
    menu_state *state, *last;

    if (!item) /* nothing to unhighlight */
	return;
    ms->selected = NULL;
    ComputeItemPos(mw, ms, item, &x, &y);
    DrawFunctionList[item->type](mw, ms, item, x, y);
    if (((item->type == MENU_CASCADE) || (item->type == MENU_HELP)) && item->enabled) {
	state = mw->menu.state;
	while (state != ms) {
	    XDestroyWindow(XtDisplay(mw), state->win);
	    last  = state;
	    state = state->prev;
	    FreeTimer(last->timer);
	    XtFree((char*)last);
	}
	mw->menu.state = ms;
    }
}

static void timer_callback(XtPointer client_data, XtIntervalId * timer)
{
  MenuWidget mw = (MenuWidget)client_data;
  XMotionEvent ev;

  XQueryPointer(XtDisplay(mw), XtWindow(mw),
		&ev.root, &ev.subwindow,
		&ev.x_root, &ev.y_root,
		&ev.x, &ev.y, &ev.state);

  HandleMotionEvent(mw, &ev, 0);
}

static int HandleMotionEvent(MenuWidget mw, XMotionEvent *ev, int is_click)
{
    menu_state *ms = NULL;
    menu_item  *item = NULL;
    Dimension  pushright = 0;
    Boolean    foundone = 0;
    int        scroll = 0, in_extra_region = 0;

    if (mw->menu.state->timer) {
      FreeTimer(mw->menu.state->timer);
      mw->menu.state->timer = 0;
    }

    /* find menu_state belonging to event */
    if (ev) {
      for (ms = mw->menu.state; ms; ms = ms->prev) {
	if (ms->x <= ev->x_root && ev->x_root <= ms->x + ms->w
	    &&  ms->y <= ev->y_root && ev->y_root <= ms->y + ms->h) {
	  if (ms->too_tall) {
	    if (ev->y_root <= (ms->y + TOO_TALL_SCROLL_HEIGHT + mw->menu.shadow_width))
	      scroll = -1;
	    else if (ev->y_root >= ms->arrow_start)
	      scroll = 1;
	  }

	  if (!scroll) {
	    foundone = 1;
	    /* find menu_item belonging to event */
	    for (item = ms->menu; item; item = item->next)
	      if (mw->menu.horizontal && !ms->prev) {
		if (!pushright && item->type == MENU_PUSHRIGHT)
		  pushright = ms->w - item->end - item->start;
		else if (ms->x + pushright + item->start <= ev->x_root
			 && ev->x_root < ms->x + pushright + item->end)
		  break;
	      } else {
		if (ms->y + item->start + ms->delta <= ev->y_root
		    &&  ev->y_root < ms->y + item->end + ms->delta)
		  break;
	      }
	  }
	  break;
	}
      }      
    }

    {
      menu_state *tms = NULL;
      for (tms = mw->menu.state; tms && tms->prev; tms = tms->prev) {
      }
      if (tms && ev) {
	if (!(tms->x <= ev->x_root 
	      && ev->x_root <= tms->x + tms->w
	      && tms->y <= ev->y_root 
	      && ev->y_root <= tms->y + tms->h)) {
	  if (tms->x - mw->menu.extra_left <= ev->x_root 
	      && ev->x_root <= tms->x + tms->w + mw->menu.extra_right
	      && tms->y - mw->menu.extra_top <= ev->y_root 
	      && ev->y_root <= tms->y + tms->h + mw->menu.extra_bottom) {
	    in_extra_region = 1;
	  }
	}
      }
    }
    
    if (!foundone)
      mw->menu.moused_out = !in_extra_region;

    if (!mw->menu.forPopup 
        && (is_click && ms && (item == ms->selected))) { /* pointer on the same item and a click */
      return 0;
    }

    if (!item) { /* if pointer not on menu_item unhighlight last selected */
      UnhighlightItem(mw, mw->menu.state, mw->menu.state->selected);
      if (scroll) {
	if (scroll < 0) {
	  if (ms->scrolled) {
	    ms->scrolled -= 1;
	    ms->scroll_top = ms->scroll_top->prev;
	    ms->delta += (ms->scroll_top->end - ms->scroll_top->start);
	  } else
	    scroll = 0;
	} else {
	  if (ms->can_go_down) {
	    ms->scrolled += 1;
	    ms->delta -= (ms->scroll_top->end - ms->scroll_top->start);
	    ms->scroll_top = ms->scroll_top->next;
	  }  else
	    scroll = 0;
	}

	if (scroll) {
	  XFillRectangle(XtDisplay((Widget)mw), ms->win, 
			 mw->menu.erase_GC,
			 0, 0, ms->w, ms->h);
	  DisplayMenu(mw, mw->menu.state);

	  ms->timer = wxAppAddTimeOut(XtWidgetToApplicationContext((Widget)mw),
				      100, timer_callback, mw, (Widget)mw);
	}
      }
      return in_extra_region;
    }

    if (item == ms->selected) { /* pointer on the same item (and not a click) */
      return 1;
    }

    /* unhighlight old item on same level (ms!) and highlight new item */
    UnhighlightItem(mw, ms, ms->selected);
    HighlightItem(mw, ms, item);
    if (item->enabled 
	&& item->type != MENU_TEXT
	&& item->type != MENU_SEPARATOR
	&& item->type != MENU_PUSHRIGHT)
	XtCallCallbackList((Widget)mw, mw->menu.on_new_item, (XtPointer)item);
        /* XtCallCallbackList((Widget)mw, mw->menu.on_new_item,
	   (XtPointer)ResourcedText(mw, item, SUBRESOURCE_HELP)); */

    return 1;
}

static void MoveSelection(MenuWidget mw, int direction)
{
  menu_state *ms = mw->menu.state;

  if (!ms)
    return;

  if (!ms->selected && ms->prev && ms->prev->prev) {
    /* Submenu popped up, nothing selected. */
    ms = ms->prev;
  }

  if (ms->selected) {
    menu_item  *item = ms->selected;

    do {
      if (direction > 0)
	item = item->next;
      else
	item = item->prev;
    } while (item && ((item->type == MENU_SEPARATOR)
		      || !item->enabled));

    if (!item) {
      /* Wraparound: highlight first/last: */
      if (direction > 0)
	item = ms->menu;
      else {
	item = ms->menu;
	while (item->next)
	  item = item->next;
      }

      while (item && ((item->type == MENU_SEPARATOR)
		      || !item->enabled)) {
	if (direction > 0)
	  item = item->next;
	else
	  item = item->prev;
      }
    }

    if (item) {
      UnhighlightItem(mw, ms, ms->selected);
      HighlightItem(mw, ms, item);
    }
  } else if (direction > 0) {
    menu_item  *item = ms->menu;

    while (item && ((item->type == MENU_SEPARATOR)
		    || !item->enabled))
      item = item->next;

    if (item)
      HighlightItem(mw, ms, item);
  } else {
    menu_item  *item = ms->menu;
    if (item) {
      while (item->next)
	item = item->next;
      while (item && ((item->type == MENU_SEPARATOR)
		      || !item->enabled))
	item = item->prev;
      
      if (item)
	HighlightItem(mw, ms, item);
    }
  }
}


/******************************************************************************
 *
 * Special Code to Popup a Menu
 *
 *****************************************************************************/

void Xaw3dPopupMenu(MenuWidget mw, Widget calling_widget)
{
    XButtonPressedEvent ev;

    /* get position of pointer in calling widget */
    ev.type = ButtonPress;
    ev.serial = 0;
    ev.send_event = 0;
    ev.display = XtDisplay(calling_widget);
    ev.window = XtWindow(calling_widget);
    ev.time = CurrentTime;
    ev.button = 0;
    XQueryPointer(ev.display, ev.window, &ev.root,
                  &ev.subwindow, &ev.x_root, &ev.y_root,
                  &ev.x, &ev.y, &ev.state);
    Xaw3dPopupMenuAtPos(mw, ev.x_root, ev.y_root);
}

void Xaw3dPopupMenuAtPos(MenuWidget mw, int x, int y)
{
    Screen  *scr         = XtScreen(mw);
    Widget  popup_shell  = XtParent(mw);
    int     border_width = popup_shell->core.border_width;
    int     w, h;
    XMotionEvent ev;

    /* compute size and position of popup menu */
    mw->menu.popped_up = TRUE;
    mw->menu.horizontal = FALSE;
    ComputeMenuSize(mw, mw->menu.state);
    mw->menu.state->delta     = (mw->menu.state->too_tall ? TOO_TALL_SCROLL_HEIGHT : 0);
    mw->menu.state->scrolled  = 0;
    mw->menu.state->scroll_top = mw->menu.state->menu;
    w = mw->menu.state->w;
    h = mw->menu.state->h;
    if (x + w > WidthOfScreen(scr))
	x = WidthOfScreen(scr) - w - 2*border_width;
    if (y + h > HeightOfScreen(scr))
	y = HeightOfScreen(scr) - h - 2*border_width;
    x = (x > border_width ? x - border_width : border_width);
    y = (y > border_width ? y - border_width : border_width);
    XtConfigureWidget(popup_shell, x, y, w, h, border_width);
    /* popup, display and handle menu */
    XtPopup(popup_shell, XtGrabNone);
    DisplayMenu(mw, mw->menu.state);
    mw->menu.state->x = x+border_width;
    mw->menu.state->y = y+border_width;
    /* init first motion event */
    ev.x_root = x; ev.y_root = y;
    HandleMotionEvent(mw, (XMotionEvent*)&ev, 0);
}

int xwMenuIsPoppedUp(Widget w)
{
  return ((MenuWidget)w)->menu.grabbed;
}
