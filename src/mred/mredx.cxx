/*
 * File:        mredx.cc
 * Purpose:     MrEd X Windows event loop
 * Author:      Matthew Flatt
 * Created:     1996
 * Copyright:   (c) 2004-2005 PLT Scheme, Inc.
 * Copyright:   (c) 1996, Matthew Flatt
 */

#define Uses_XtIntrinsic
#define Uses_XtIntrinsicP
#define Uses_XLib

#include "wx_main.h"
#include "wx_media.h"
#include "wx_clipb.h"
#include "scheme.h"

#include "mred.h"

#include <X11/Shell.h>

static int short_circuit = 0, just_check = 0, checking_for_break = 0;
static Widget just_this_one;

static Widget orig_top_level;
static Widget save_top_level = 0;

static KeyCode breaking_code;
static int breaking_code_set = 0;

static Widget *grab_stack, grabber;
static int grab_stack_pos = 0, grab_stack_size = 0;
#define WSTACK_INC 3

extern Widget wx_clipWindow, wx_selWindow;

wxWindow *wxLocationToWindow(int x, int y);

extern "C" {
  void wxAddGrab(Widget w)
    {
      if (!grab_stack_pos) {
	Widget *naya;
	if (!grab_stack)
	  wxREGGLOB(grab_stack);
	grab_stack_size += WSTACK_INC;
	naya = (Widget *)scheme_malloc(grab_stack_size * sizeof(Widget));
	memcpy(naya + WSTACK_INC, grab_stack, (grab_stack_size - WSTACK_INC) * sizeof(Widget));
	grab_stack = naya;
	grab_stack_pos = WSTACK_INC;
      }

      grabber = grab_stack[--grab_stack_pos] = w;
    }

  void wxRemoveGrab(Widget w)
    {
      if (w != grabber)
	return;

      if (++grab_stack_pos < grab_stack_size)
	grabber = grab_stack[grab_stack_pos];
      else
	grabber = NULL;
    }
};

Widget wxGetAppToplevel()
{
  if (save_top_level)
    return save_top_level;
  else {
    MrEdContext *c;
    c = MrEdGetContext();
    return c->finalized->toplevel;
  }
}

void wxPutAppToplevel(Widget w)
{
  save_top_level = w;
}

void MrEdInitFirstContext(MrEdContext *c)
{
  orig_top_level = save_top_level;
  c->finalized->toplevel = save_top_level;
  save_top_level = 0;
}

void MrEdInitNewContext(MrEdContext *c)
{
  wxInitNewToplevel();
  c->finalized->toplevel = save_top_level;
  save_top_level = 0;
}

void MrEdDestroyContext(MrEdFinalizedContext *c)
{
  XtDestroyWidget(c->toplevel);
}

static Window GetEventWindow(XEvent *e)
{
  Window window = 0;

#define WINCASEEX(type, record, field) case type: window = e->record.field; break
#define WINCASE(type, record) WINCASEEX(type, record, window)

  switch (e->type) {
    WINCASE(KeyPress, xkey);
    WINCASE(KeyRelease, xkey);
    WINCASE(ButtonPress, xbutton);
    WINCASE(ButtonRelease, xbutton);
    WINCASE(MotionNotify, xmotion);
    WINCASE(EnterNotify, xcrossing);
    WINCASE(LeaveNotify, xcrossing);
    WINCASE(FocusIn, xfocus);
    WINCASE(FocusOut, xfocus);
    WINCASE(KeymapNotify, xkeymap);
    WINCASE(Expose, xexpose);
    WINCASEEX(GraphicsExpose, xgraphicsexpose, drawable);
    WINCASEEX(NoExpose, xnoexpose, drawable);
    WINCASE(VisibilityNotify, xvisibility);
    WINCASE(CreateNotify, xcreatewindow);
    WINCASE(DestroyNotify, xdestroywindow);
    WINCASE(UnmapNotify, xunmap);
    WINCASE(MapNotify, xmap);
    WINCASE(MapRequest, xmaprequest);
    WINCASE(ReparentNotify, xreparent);
    WINCASE(ConfigureNotify, xconfigure);
    WINCASE(ConfigureRequest, xconfigurerequest);
    WINCASE(GravityNotify, xgravity);
    WINCASE(ResizeRequest, xresizerequest);
    WINCASE(CirculateNotify, xcirculate);
    WINCASE(CirculateRequest, xcirculaterequest);
    WINCASE(PropertyNotify, xproperty);
    WINCASE(SelectionClear, xselectionclear);
    WINCASEEX(SelectionRequest, xselectionrequest, owner);
    WINCASEEX(SelectionNotify, xselection, requestor);
    WINCASE(ColormapNotify, xcolormap);
    WINCASE(ClientMessage, xclient);
    WINCASE(MappingNotify, xmapping);
  default:
    break;
  }

  return window;
}

static unsigned long lastUngrabTime;
static unsigned long lastUnhideTime;

static Bool CheckPred(Display *display, XEvent *e, char *args)
{
  Window window;
  Widget widget;

  switch (e->type) {
  case ButtonPress:
  case ButtonRelease:
  case MotionNotify:
    if (e->xbutton.time > lastUnhideTime) {
      lastUnhideTime = e->xbutton.time;
      wxUnhideAllCursors();
    }
    break;
  default:
    break;
  }

  if (short_circuit)
    return FALSE;

#if 0
  printf("trying %s\n", get_event_type(e));
#endif

  window = GetEventWindow(e);

  if (window) {
    widget = XtWindowToWidget(display, window);
#if 1
    if (widget)
      if (e->type == DestroyNotify)
	printf("DestroyNotified window %lx is still widget-mapped; BadWindow error is imminent.\n", window);
#endif
  } else
    widget = 0;

  /* Check for mouse-down events outside the indicated window.  That
     might indicate a mouse grab gone awry, and we need to fix it.
     The only legitimate grabs that operate on other windows are with
     menus, and those have no wx counterpart. */
  if (widget && (e->type == ButtonPress)) {
    /* lastUngrabTime keeps us from checking the same events
       over and over again. */
    if (e->xbutton.time > lastUngrabTime) {
      Window root;
      int x, y;
      unsigned w, h, b, d;
      
      XGetGeometry(XtDisplay(widget), e->xbutton.window, 
		   &root, &x, &y, &w, &h,
		   &b, &d);
      if ((e->xbutton.x < 0) || (e->xbutton.y < 0)
	  || ((unsigned int)e->xbutton.x > w) || ((unsigned int)e->xbutton.y > h)) {
	/* Looks bad, but is it a click in a MrEd window
	   that we could care about? */

	wxWindow *w;
	w = wxLocationToWindow(e->xbutton.x_root, e->xbutton.y_root);
	
	if (w) {
	  /* Looks like we need to ungrab */
	  XUngrabPointer(XtDisplay(widget), 0);
	  XUngrabKeyboard(XtDisplay(widget), 0);
	}
      }
      
      lastUngrabTime = e->xbutton.time;
    }
  }

  if (widget) {
    Widget parent = 0;

    /* Special hack in cooperation with Clipboard.cc
       to make clipboard operations happen in the right
       eventspace. */
    if (widget == wx_clipWindow) {
      wxClipboardClient *clipOwner;
      clipOwner = wxTheClipboard->GetClipboardClient();
      if (clipOwner) {
	MrEdContext *cc = (MrEdContext *)clipOwner->context;
	if (cc)
	  parent = cc->finalized->toplevel;
      }
    }
    if (widget == wx_selWindow) {
      wxClipboardClient *clipOwner;
      clipOwner = wxTheSelection->GetClipboardClient();
      if (clipOwner) {
	MrEdContext *cc = (MrEdContext *)clipOwner->context;
	if (cc)
	  parent = cc->finalized->toplevel;
      }
    }

    if (!parent) {
      for (parent = widget; XtParent(parent); parent = XtParent(parent)) {
      }
    }
    
#if 0
    printf("parent: %lx context: %lx\n", parent, parent_context);
#endif

    if (just_this_one) {
      if (parent == just_this_one) {
	if (checking_for_break) {
	  if (e->type == KeyPress) {
	    if ((e->xkey.state & ControlMask) 
#if BREAKING_REQUIRES_SHIFT
		&& (e->xkey.state & ShiftMask)
#endif
		&& (e->xkey.keycode == breaking_code))
	      goto found;
	  }
	} else {
	  goto found;
	}
      } else {
#if 0
	printf("wrong eventspace (%lx != %lx)\n", just_this_one, parent_context);
#endif
      }
    } else {
      MrEdContext *c;
      
      for (c = mred_contexts; c; c = c->next) {
	if (c->finalized->toplevel == parent) {
	  if (!c->ready) {
#if 0
	    printf("not ready\n");
#endif
	    return FALSE;
	  } else {
	    if (args)
	      *(MrEdContext **)args = c;
	    goto found;
	  }
	}
      }

      /* Toplevel without context; handle in the main context: */
#if 0
      printf("Can't map top-level to eventspace for %lx\n", window);
#endif
      if (checking_for_break)
	return FALSE;
      else {
	if (args)
	  *(MrEdContext **)args = NULL;
	goto found;
      }
    }

  } else {
#if 0
    printf("warning: window->widget mapping failed: %lx; event: %d; parent: %lx\n", 
	   window, e->type, ((XCreateWindowEvent *)e)->parent);
#endif
    if (checking_for_break)
      return FALSE;
    else if (just_this_one)
      return FALSE;
    else {
      /* Toplevel without context; handle in the main context: */
      if (args)
	*(MrEdContext **)args = NULL;
      goto found;
    }
  }

  return FALSE;

 found:
  if (just_check) {
    short_circuit = TRUE;
    return FALSE;
  } else
    return TRUE;
}


int MrEdGetNextEvent(int check_only, int current_only, 
		     XEvent *event, MrEdContext **which)
{
  Display *d;

  if (which)
    *which = NULL;

  just_check = check_only;
  just_this_one = (current_only ? wxGetAppToplevel() : (Widget)NULL);

  if (!orig_top_level)
    d = XtDisplay(save_top_level);
  else
    d = XtDisplay(orig_top_level);

  if (XCheckIfEvent(d, event, CheckPred, (char *)which)) {
    just_check = 0;
    return 1;
  } else if (short_circuit) {
    short_circuit = 0;
    return 1;
  }

  return 0;
}

static Scheme_Hash_Table *disabled_widgets;

#ifdef MZ_PRECISE_GC
static void widget_hash_indices(void *_key, long *_h, long *_h2)
{
  long lkey;
  long h, h2;
  
  lkey = (long)_key;

  h = (lkey >> 2);
  h2 = (lkey >> 3);

  *_h = h;
  *_h2 = h2;
}
#endif

void wxSetSensitive(Widget w, Bool enabled)
{
  if (!disabled_widgets) {
    if (enabled)
      return;

    wxREGGLOB(disabled_widgets);
    disabled_widgets = scheme_make_hash_table(SCHEME_hash_ptr);
#ifdef MZ_PRECISE_GC
    disabled_widgets->make_hash_indices = widget_hash_indices;
#endif
  }

  if (enabled) {
    scheme_hash_set(disabled_widgets, (Scheme_Object *)w, NULL);
  } else {
    scheme_hash_set(disabled_widgets, (Scheme_Object *)w, (Scheme_Object *)0x1);
  }
}

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif
/* No GC here because it's used to draw GC bitmaps */

Display *MrEdGetXDisplay(void)
{
  if (!orig_top_level)
    return XtDisplay(save_top_level);
  else
    return XtDisplay(orig_top_level);
}

int MrEdGetDoubleTime(void)
{
  return XtGetMultiClickTime(MrEdGetXDisplay());
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

void MrEdDispatchEvent(XEvent *event)
{
  if (disabled_widgets) {
    int type = event->type;
    Display *d;
    
    d = MrEdGetXDisplay();

    if ((type == KeyPress)
	|| (type == KeyRelease)
	|| (type == ButtonPress)
	|| (type == ButtonRelease)
	|| (type == MotionNotify)
	|| (type == EnterNotify)
	|| (type == LeaveNotify)
	|| ((type == ClientMessage)
	    && !strcmp(XGetAtomName(d, event->xclient.message_type), "WM_PROTOCOLS")
	    && !strcmp(XGetAtomName(d, event->xclient.data.l[0]), "WM_DELETE_WINDOW"))) {
      Window window;
      Widget widget, ow, exempt = 0;
      MrEdContext *c;
      wxWindow *ew;

      window = GetEventWindow(event);

      if (window)
	widget = XtWindowToWidget(d, window);
      else
	widget = 0;
      ow = widget;

      c = MrEdGetContext();
      ew = c->modal_window;
      if (ew) {
	wxWindow_Xintern *ph;
	ph = ew->GetHandle();
	exempt = ph->frame;
      }

      while (widget) {
	if (widget == grabber)
	  break;

	/* Only start checking the enabled state with the first
	   top-level window. That way, PreOnChar and PreOnEvent are
           called appropriately. wxWindows/Xt ensures that key and mouse
           events are not dispatched to disabled items. */

	if (XtIsSubclass(widget, transientShellWidgetClass)
	    || XtIsSubclass(widget, topLevelShellWidgetClass)) {
	  
	  if (scheme_hash_get(disabled_widgets, (Scheme_Object *)widget)) {
#if 0
	    printf("disabled: %lx from %lx\n", widget, ow);
#endif
	    return;
	  }
	}

	if (widget == exempt)
	  break;

	widget = XtParent(widget);
      }
    }
  }

  XtDispatchEvent(event);
}

int MrEdCheckForBreak(void)
{
  int br;
  GC_CAN_IGNORE XEvent e;
  Display *d;

  if (!orig_top_level)
    d = XtDisplay(save_top_level);
  else
    d = XtDisplay(orig_top_level);

  if (!breaking_code_set) {
    breaking_code = XKeysymToKeycode(d, 'c');
    breaking_code_set = 1;
  }

  XFlush(d);

  checking_for_break = 1;
  br = MrEdGetNextEvent(0, 1, &e, NULL);
  checking_for_break = 0;

  return br;
}

#include "wx_timer.h"

class wxXtTimer : public wxTimer
{
public:
  XtTimerCallbackProc callback;
  XtPointer data;
  int ok;
  Widget wgt;

  wxXtTimer(Widget w, XtTimerCallbackProc c, XtPointer d);

  Bool Start(int millisec = -1, Bool one_shot = FALSE);

  void Stopped() { ok = 0; }

  void Notify(void);
};

wxXtTimer::wxXtTimer(Widget w, XtTimerCallbackProc c, XtPointer d)
: wxTimer()
{
  callback = c;
  wgt = w;
  data = d;
  ok = 1;
}

void wxXtTimer::Notify(void) {
  /* Used to try to avoid starving other events, but yielding 
     has its own problems. In particular, it messes up dialogs
     that expect show #f to immediately lead to a return from
     show #t. */
  // wxYield();

  if (ok)
    callback(data, NULL);
}

Bool wxXtTimer::Start(int millisec, Bool one_shot)
{
  Widget parent;

  /* Only start the timer if the context is consistnt with
     the original widget, the context is still running,
     etc. */
  for (parent = wgt; XtParent(parent); parent = XtParent(parent)) {
  }

  if (context
      && !((MrEdContext *)context)->killed
      && ((MrEdContext *)context)->finalized
      && (((MrEdContext *)context)->finalized->toplevel == parent)) {
    return wxTimer::Start(millisec, one_shot);
  }
  return FALSE;
}

extern "C" {

  void wxRemoveTimeOut(long timer)
    {
      wxXtTimer *t;
#ifdef MZ_PRECISE_GC
      t = *(wxXtTimer **)timer;
      GC_free_immobile_box((void **)timer);
#else
      t = (wxXtTimer *)timer;
#endif

      t->Stop();
      t->Stopped();

#ifdef MZ_PRECISE_GC
      XFORM_RESET_VAR_STACK;
#endif
    }
  
  long wxAppAddTimeOut(XtAppContext app_ctx, unsigned long interval, 
		       XtTimerCallbackProc callback, XtPointer data,
		       Widget w)
    {
      wxTimer *t;
      long result;

      t = new wxXtTimer(w, callback, data);
      t->Start(interval, TRUE);
#ifdef MZ_PRECISE_GC
      result = (long)GC_malloc_immobile_box(t);
#else
      result = (long)t;
#endif

#ifdef MZ_PRECISE_GC
      XFORM_RESET_VAR_STACK;
#endif

      return result;
    }
}

/***********************************************************************/

typedef struct {
  Widget w;
  wxWindow *wx;
} FindRec;

void *IsWidgetFrame(wxObject *f, void *d)
{
  FindRec *fr = (FindRec *)d;
  wxWindow_Xintern *i;
  
  i = ((wxWindow *)f)->GetHandle();
  if (i->frame == fr->w) {
    fr->wx = (wxWindow *)f;
  }

  return d;
}

static wxWindow *FindMrEdWindow(Display *d, Window xw)
{
  Widget w;
  w = XtWindowToWidget(d, xw);
  if (w) {
    FindRec fr;
    fr.w = w;
    fr.wx = NULL;
    MrEdForEachFrame(IsWidgetFrame, &fr);
    return fr.wx;
  } else {
    wxWindow *m;
    Window root, parent, *children;
    unsigned int n, i;
    if (XQueryTree(d, xw, &root, &parent, &children, &n)) {
      if (children) {
	m = NULL;
	for (i = 0; i < n; i++) {
	  m = FindMrEdWindow(d, children[i]);
	  if (m)
	    break;
	}
	XFree(children);
	return m;
      }
    }
     
    return NULL;
  }
}

wxWindow *wxLocationToWindow(int x, int y)
{
  Display *d;
  Window root, parent, *children;
  unsigned int n, i;
  XWindowAttributes a;
  wxWindow *result = NULL;

  if (!orig_top_level)
    d = XtDisplay(save_top_level);
  else
    d = XtDisplay(orig_top_level);

  if (XQueryTree(d, DefaultRootWindow(d),
		 &root, &parent, &children, &n)) {
    for (i = n; i--; ) {
      XGetWindowAttributes(d, children[i], &a);

      if (a.map_state == IsViewable
	  && (a.x <= x) && (a.x + a.width >= x)
	  && (a.y <= y) && (a.y + a.height >= y)) {
	/* Found the X window, now see if it's a MrEd window: */
	result = FindMrEdWindow(d, children[i]);
	break;
      }
    }
    
    if (children)
      XFree(children);
  }
 
 return result;
}
