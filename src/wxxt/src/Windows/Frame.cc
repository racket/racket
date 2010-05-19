/*								-*- C++ -*-
 *
 * Purpose: base class for all frames
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 2004-2010 PLT Scheme Inc.
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
#pragma implementation "Frame.h"
#pragma GCC diagnostic ignored "-Wwrite-strings"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxApp
#define  Uses_wxFrame
#define  Uses_wxGDI
#define  Uses_wxLayout
#define  Uses_wxList
#define  Uses_wxMenuBar
#define  Uses_wxMessage
#define  Uses_wxTypeTree
#define  Uses_wxMemoryDC
#include "wx.h"
#define  Uses_ShellWidget
#define  Uses_BoardWidget
#include "widgets.h"
#include "../../contrib/xpm/lib/xpm.h"
#include <X11/Xatom.h>
#include "wx_visual.h"

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

/* XPM */
static char * plt_xpm[] = {
"16 16 5 1",
" 	c None",
".	c #000000",
"-	c #FF0000",
",	c #0000FF",
"!	c #FFFFFF",
"................",
".....,,,,,,.....",
"...--!!,,,,,,...",
"..-----!,,,,,,..",
"..-----!!,,,,,..",
".-------!,,,,,,.",
".-------!!,,,,,.",
".------!!!,,,,,.",
".-----!!-!!,,,,.",
".-----!---!,,,,.",
".----!!---!!,,,.",
"..---!-----!,,..",
"..--!!-----!!,..",
"...-!-------!...",
".....------.....",
"................"};

#define plt_width 16
#define plt_height 16
static char plt_xbm[] = {
 0xe0,0x07,0xf8,0x1f,0xfc,0x3f,0xfe,0x7f,0xfe,0x7f,0xff,0xff,0xff,0xff,0xff,
 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xfe,0x7f,0xfe,0x7f,0xfc,0x3f,0xf8,0x1f,
 0xe0,0x07};

Pixmap plt_icon, plt_mask;

//-----------------------------------------------------------------------------
// create and destroy frame
//-----------------------------------------------------------------------------

wxFrame::wxFrame(void) : wxPanel()
{
    __type = wxTYPE_FRAME;

    menubar         = NULL;
    status          = NULL;
    num_status      = 0;

    SetShown(FALSE);
}

wxFrame::wxFrame(wxFrame *parent, char *title,
		 int x, int y, int width, int height, int style, char *name)
    : wxPanel()
{
    __type = wxTYPE_FRAME;

    menubar         = NULL;
    status          = NULL;
    num_status      = 0;

    Create(parent, title, x, y, width, height, style, name);

    SetShown(FALSE);
}

wxFrame::~wxFrame(void)
{
    wxChildList *tlf;

    // hide frame
    Show(FALSE);
    // destroy children first to popdown child frames
    DestroyChildren();
    // adjust list of top level frames
    tlf = wxTopLevelFrames(this);
    tlf->DeleteObject(this);
}

/* Used to ensure that hide-&-show within an event cycle works */
static void wxFrameMapProc(Widget w, XtPointer clientData, 
			   XCrossingEvent * event)
{
  wxFrame *frame = (wxFrame *)GET_SAFEREF(clientData);

  if (frame) {
    XEvent *e = (XEvent *)event;

    if (e->xany.type == MapNotify && e->xmap.window == XtWindow(w)) {
      if (!frame->IsShown()) {
	/* We really wanted this to be hidden! */
	XtUnmapWidget(w);
      }
    }
  }
}

extern "C" void *scheme_current_process;

/* Copied off a newsgroup somewhere: */
typedef struct {
  long flags;
  long functions;
  long decorations;
  long inputMode;
  long unknown;
} wxMWM_Hints;

/* bit definitions for MwmHints.decorations */
#define MWM_DECOR_ALL           (1L << 0)
#define MWM_DECOR_BORDER        (1L << 1)
#define MWM_DECOR_RESIZEH       (1L << 2)
#define MWM_DECOR_TITLE         (1L << 3)
#define MWM_DECOR_MENU          (1L << 4)
#define MWM_DECOR_MINIMIZE      (1L << 5)
#define MWM_DECOR_MAXIMIZE      (1L << 6)

Bool wxFrame::Create(wxFrame *frame_parent, char *title,
		     int x, int y, int width, int height,
		     int _style, char *name)
{
    Widget parent_widget, wgt;
    wxChildList *tlf;
    Atom WM_DELETE_WINDOW;

    context = wxGetContextForFrame();

    // chain child <-> parent
    if ((parent = frame_parent)) {
      wxWindow_Xintern *ph;
      ph = frame_parent->GetHandle();
      parent_widget = ph->frame;
      parent->AddChild(this);
    } else {
	parent_widget = wxAPP_TOPLEVEL;
    }
    tlf = wxTopLevelFrames(this);
    tlf->Append(this);
    tlf->Show(this, FALSE);

    // create top level or transient shell
    if ( (style = _style) & wxTRANSIENT ) {
      // create transient shell with WM_TRANSIENT_FOR property
      wxWindow *p;
      Widget pw;

      for (p = parent; p; p = p->GetParent()) {
	if (wxSubType(p->__type, wxTYPE_FRAME)
	    && !(p->GetWindowStyleFlag() & wxTRANSIENT)) // frame must not be transient
	  break;
      }
      if (p) {
	wxWindow_Xintern *ph;
	ph = p->GetHandle();
	pw = ph->frame;
      } else
	pw = wxAPP_TOPLEVEL;
      X->frame = XtVaCreatePopupShell
	(name ? name : "shell", transientShellWidgetClass, parent_widget,
	 XtNsaveUnder, FALSE,
	 XtNtransientFor, pw,
	 XtNvisual, wxAPP_VISUAL,
	 XtNdepth, wx_visual_depth,
	 XtNcolormap, wx_default_colormap,
	 NULL);
    } else {
      // create top level shell
      X->frame = XtVaCreatePopupShell
	(name ? name : "shell", 
	 (((style & wxFLOAT_FRAME)  && (style & wxNO_CAPTION))
	  ? overrideShellWidgetClass 
	  : topLevelShellWidgetClass), 
	 parent_widget, 
	 XtNvisual, wxAPP_VISUAL,
	 XtNdepth, wx_visual_depth,
	 XtNcolormap, wx_default_colormap,
	 NULL);
    }
    // set common data
    SetSize(x, y, width, height, wxSIZE_AUTO | wxPOS_USE_MINUS_ONE);
    // create board widget
    wgt = XtVaCreateManagedWidget(
	name, xfwfBoardWidgetClass, X->frame,
	XtNhighlightThickness, 0,
	XtNbackground, wxGREY_PIXEL,
	NULL);
    X->handle = wgt;
    AddEventHandlers();

    XtRealizeWidget(X->frame);
    SetTitle(title);
    // make a WM_PROTOCOLS atom if necessary
    XInternAtom(XtDisplay(X->frame), "WM_PROTOCOLS", False);
    // make a WM_DELETE_WINDOW atom
    WM_DELETE_WINDOW = XInternAtom(XtDisplay(X->frame),
				   "WM_DELETE_WINDOW",
				   False);
    XSetWMProtocols(XtDisplay(X->frame),
		    XtWindow(X->frame),
		    &WM_DELETE_WINDOW,
		    1);

    /* part of show-&-hide fix */
    XtAddEventHandler(X->frame, StructureNotifyMask,
		      False, (XtEventHandler)wxFrameMapProc,
		      (XtPointer)saferef);

    cursor = wxSTANDARD_CURSOR;

    if (wxIsBusy())
      wxXSetBusyCursor(this, wxHOURGLASS_CURSOR);

    if ((_style & wxNO_RESIZE_BORDER)
	|| (_style & wxNO_CAPTION)) {
      /* Copied off a newsgroup somewhere: */
      Atom WM_HINTS;
      Display *display;
      Window window;

      display = XtDisplay(X->frame);
      window = XtWindow(X->frame);

      /* First try to set MWM hints */
      WM_HINTS = XInternAtom(display, "_MOTIF_WM_HINTS", True);
      if ( WM_HINTS != None ) {
#define MWM_HINTS_DECORATIONS (1L << 1)
	wxMWM_Hints MWMHints = { MWM_HINTS_DECORATIONS, 0, 0, 0, 0 };
	
	if (!(_style & wxNO_RESIZE_BORDER)
	    || !(_style & wxNO_CAPTION))
	  MWMHints.decorations |= MWM_DECOR_BORDER;

	if (!(_style & wxNO_RESIZE_BORDER))
	  MWMHints.decorations |= (MWM_DECOR_RESIZEH
				   | MWM_DECOR_MINIMIZE
				   | MWM_DECOR_MAXIMIZE);

	if (!(_style & wxNO_CAPTION))
	  MWMHints.decorations |= (MWM_DECOR_TITLE
				   | MWM_DECOR_MENU);

	XChangeProperty(display, window, WM_HINTS, WM_HINTS, 32,
			PropModeReplace, (unsigned char *)&MWMHints,
			sizeof(MWMHints)/sizeof(long));
      }
      /* Now try to set KWM hints */
      if (_style & wxNO_CAPTION) {
	WM_HINTS = XInternAtom(display, "KWM_WIN_DECORATION", True);
	if ( WM_HINTS != None ) {
	  long KWMHints = 0;
	  
	  if (!(_style & wxNO_RESIZE_BORDER))
	    KWMHints = 2; /* tiny decoration */
	  
	  XChangeProperty(display, window, WM_HINTS, WM_HINTS, 32,
			  PropModeReplace, (unsigned char *)&KWMHints,
			  sizeof(KWMHints)/sizeof(long));
	}
      }
      /* Now try to set GNOME hints */
      if ((_style & wxNO_RESIZE_BORDER)
	  && (_style & wxNO_CAPTION)) {
	WM_HINTS = XInternAtom(display, "_WIN_HINTS", True);
	if (WM_HINTS != None) {
	  long GNOMEHints = 0;
	  
	  XChangeProperty(display, window, WM_HINTS, XA_CARDINAL, 32,
			  PropModeReplace, (unsigned char *)&GNOMEHints,
			  sizeof(GNOMEHints)/sizeof(long));
	}
      }
    }


    if ((x > wxDEFAULT_POSITION) && (y > wxDEFAULT_POSITION)) {
      /* Tell the window manager that we really meant the initial position: */
      XSizeHints hints;
      hints.flags = USPosition;
      if ((width >= 0) && (height >= 0))
	hints.flags |= USSize;
      hints.x = x;
      hints.y = y;
      hints.width = width;
      hints.height = height;
      XSetWMNormalHints(XtDisplay(X->frame), XtWindow(X->frame), &hints);
    }

    if (!plt_mask) {
      plt_mask = XCreateBitmapFromData(wxAPP_DISPLAY, wxAPP_ROOT, plt_xbm, plt_width, plt_height);
    }
    if (!plt_icon) {
      XpmAttributes *xpm;
#ifdef MZ_PRECISE_GC
      xpm = (XpmAttributes *)GC_malloc_atomic(sizeof(XpmAttributes));
#else
      xpm = new WXGC_ATOMIC XpmAttributes;
#endif
      xpm->valuemask = (XpmReturnInfos | XpmReturnPixels | XpmCloseness
			| XpmVisual | XpmDepth | XpmColormap);
      xpm->closeness = 40000;
      xpm->visual = wxAPP_VISUAL;
      xpm->depth = wx_visual_depth;
      xpm->colormap = wx_default_colormap;
      if (XpmCreatePixmapFromData(wxAPP_DISPLAY, wxAPP_ROOT,
				  plt_xpm, &plt_icon,
				  (Pixmap*)NULL, xpm)
	  != XpmSuccess)
	plt_icon = (Pixmap)NULL;
    }

    if ((style & wxTRANSIENT) && frame_parent) {
      /* Dialog: use parent frame's icon. */
      Pixmap icon, mask;

      XtVaGetValues(frame_parent->X->frame, 
		    XtNiconMask, &mask, 
		    XtNiconPixmap, &icon, NULL);

      if (mask && icon) {
	XtVaSetValues(X->frame, XtNiconMask, mask, NULL);
	XtVaSetValues(X->frame, XtNiconPixmap, icon, NULL);
      }
    } else {
      if (plt_mask && plt_icon) {
	XtVaSetValues(X->frame, XtNiconMask, plt_mask, NULL);
	XtVaSetValues(X->frame, XtNiconPixmap, plt_icon, NULL);
      }
    }

    /* Let window managers know that the frame can receive the focus
       (patch from Alexey Voinov): */
    XtVaSetValues(X->frame, XtNinput, True, NULL);

    return TRUE;
}

//-----------------------------------------------------------------------------
// leave place for menubar and statusline
//-----------------------------------------------------------------------------

void wxFrame::GetSize(int *width, int *height)
{
  if (X->frame && XtIsRealized(X->frame)) {
    /* Get the actual window size, insteda of waiting for events
       to update the widget */
    int x_pos, y_pos;
    unsigned int border, depth;
    Window root;
    Display *disp;
    Window win;

    disp = XtDisplay(X->frame);
    win = XtWindow(X->frame);
    
    XGetGeometry(disp, win,
		 &root, &x_pos, &y_pos,
		 (unsigned int *)width, (unsigned int *)height,
		 &border, &depth);
  } else
    wxWindow::GetSize(width, height);

}

void wxFrame::GetPosition(int *x, int *y)
{
  if (X->frame && XtIsRealized(X->frame)) {
    /* Get the actual window position, instead of waiting for events
       to update the widget */
    Display *disp;
    Window win;
    Window child;

    disp = XtDisplay(X->frame);
    win = XtWindow(X->frame);

    XTranslateCoordinates(disp, 
			  win, 
			  DefaultRootWindow(disp),
			  0, 0, 
			  x, y, &child);
  } else
    wxWindow::GetPosition(x, y);
}

void wxFrame::Fit(void)
{
    int hsize=0, vsize=0;

    if (children) {
        wxChildNode *node;
	for (node = children->First(); node; node = node->Next()) {
	    wxWindow *child;
	    child = (wxWindow*)(node->Data());
	    if (child) {
		// skip menubar and status line for computation
		int x, y, w, h;
		int i=0;
		for ( /* i=0 */; i<num_status; ++i) {
		  if (child == status[i])
		    break;
		}
		if (child == menubar || i < num_status) {
		  continue;
		}
		// compute maximal size
		child->GetPosition(&x, &y); child->GetSize(&w, &h);
		hsize = max(hsize, x + w);
		vsize = max(vsize, y + h);
	    }
	}
	hsize -= xoff; vsize -= yoff;
    } else {
	hsize = PANEL_HMARGIN;
	vsize = PANEL_VMARGIN;
    }
    hsize += /* PANEL_HMARGIN + */ (style & wxBORDER ? 4 : 0 );
    vsize += /* PANEL_VMARGIN + */ (style & wxBORDER ? 4 : 0 );
    SetClientSize(hsize, vsize);
}

// void wxFrame::Layout(void)
// --> wxLayout.cc

void wxFrame::GetClientSize(int *width, int *height)
{
    int dummy, h1=0, h2=0, i;

    GetSize(width, height);
    if (menubar)  menubar->GetSize(&dummy, &h1);   // get menubar's height
    for (i = 0; i < num_status; i++) {
      status[i]->GetSize(&dummy, &h2); // get status lines's height
      h1 += h2;
    }
    *height -= h1;                            // adjust height
}

void wxFrame::SetClientSize(int width, int height)
{
    int dummy, h1=0, h2=0;

    if (menubar)  menubar->GetSize(&dummy, &h1);   // get menubar's height
    if (status)   status[0]->GetSize(&dummy, &h2); // get status lines's height
    height += h1 + h2;		                   // adjust height
    wxWindow::SetClientSize(width, height);
}

void wxFrame::EnforceSize(int minw, int minh, int maxw, int maxh, int incw, int inch)
{
  XSizeHints sh;
  int x, y;

  if (minw < 0)
    minw = 0;
  if (minh < 0)
    minh = 0;
  if (maxw < 0)
    maxw = 32000;
  if (maxh < 0)
    maxh = 32000;

  sh.flags = (PMinSize | PMaxSize | PResizeInc | USPosition);
  sh.min_width = minw;
  sh.min_height = minh;
  sh.max_width = maxw;
  sh.max_height = maxh;
  sh.width_inc = incw;
  sh.height_inc = inch;

  GetPosition(&x, &y);
  sh.x = x;
  sh.y = y;

  XSetWMNormalHints(XtDisplay(X->frame), 
		    XtWindow(X->frame),
		    &sh);
}

//-----------------------------------------------------------------------------
// iconize, maximize
//-----------------------------------------------------------------------------

void wxFrame::Iconize(Bool iconize)
{
  if (!IsShown())
    return;
  
  if (iconize) {
    XIconifyWindow(XtDisplay(X->frame), 
		   XtWindow(X->frame), 
		   XScreenNumberOfScreen(XtScreen(X->frame)));
  } else {
    XtMapWidget(X->frame);
  }
}

Bool wxFrame::Iconized(void)
{
  XWindowAttributes wa;

  if (!IsShown())
    return FALSE;

  XSync(XtDisplay(X->frame), FALSE);

  XGetWindowAttributes(XtDisplay(X->frame), XtWindow(X->frame), &wa);

  return (wa.map_state == IsUnmapped);
}

void wxFrame::Maximize(Bool WXUNUSED(maximize))
{
}

Bool wxFrame::IsMaximized()
{
  return FALSE;
}

//-----------------------------------------------------------------------------
// status line
//-----------------------------------------------------------------------------

void wxFrame::CreateStatusLine(int number, char *)
{
    if (StatusLineExists())
	return;

    status = new WXGC_PTRS wxMessage* [num_status = min(number, wxMAX_STATUS)];
    for (int i = 0; i < num_status; ++i) {
	wxLayoutConstraints *constr;
	int ww, hh;
	wxMessage *sm;
	wxWindow **sr;

	sm = DEBUG_NEW wxMessage(this, "", 0, 0, wxBORDER, NULL, "status");
	status[i] = sm;
	sm->AllowResize(FALSE);
	sm->SetAlignment(wxALIGN_LEFT);
	sm->GetSize(&ww, &hh);
	constr = DEBUG_NEW wxLayoutConstraints;
	sr = GetWinSafeRef();
	wxLC_MEM(constr->left, PercentOf(sr, wxWidth, i*(100/num_status)));
	wxLC_MEM(constr->top, Below(sr, 0)); // wxBottom of client area
	wxLC_MEM(constr->height, Absolute(hh));
	if (i != num_status-1) {
	  wxLC_MEM(constr->width, PercentOf(sr, wxWidth, 100 / num_status));
	} else {
	  wxLC_MEM(constr->right, SameAs(sr, wxRight, 0));
	  wxLC_MEM(constr->width, Unconstrained());
	}
	status[i]->SetConstraints(constr);

    }

    Layout();
}

void wxFrame::SetStatusText(char *text, int number)
{
    if (number < num_status)
	status[number]->SetLabel(text ? text : (char *)"");
}

Bool wxFrame::StatusLineExists(void)
{
    return (num_status != 0);
}

//-----------------------------------------------------------------------------
// associated GDI objects
//-----------------------------------------------------------------------------

wxMenuBar *wxFrame::GetMenuBar(void)
{
    return menubar;
}

void wxFrame::SetIcon(wxBitmap *icon, wxBitmap *mask, int kind)
{
  if (kind == 2) /* large */
    return;

  if (icon->Ok()) {
    wxBitmap *bm;
    int w, h;

    w = icon->GetWidth();
    h = icon->GetHeight();
    bm = new wxBitmap(w, h);
    if (bm->Ok()) {
      wxMemoryDC *mdc;
      Pixmap pm;

      mdc = new wxMemoryDC();
      mdc->SelectObject(bm);
      mdc->Blit(0, 0, w, h, icon, 0, 0, wxSTIPPLE, NULL);
      mdc->SelectObject(NULL);

      if (mask && !mask->Ok())
	mask = NULL;
      
      pm = mask ? GETPIXMAP(mask) : (Pixmap)NULL;
      XtVaSetValues(X->frame, XtNiconMask, pm, NULL);
      pm = GETPIXMAP(bm);
      XtVaSetValues(X->frame, XtNiconPixmap, pm, (Pixmap)NULL, NULL);
      
      frame_icon = bm;
      frame_mask = mask;
    }
  }
}

void wxFrame::SetMenuBar(wxMenuBar *new_menubar)
{
  /* MATTHEW: Enforce safety */
  if (new_menubar && new_menubar->GetParent())
    return;

  if (menubar)
    menubar->Destroy();	// destroy X internal representation
  if ((menubar = new_menubar)) {
    int ww, hh;
    
    menubar->Create(this);
    menubar->GetSize(&ww, &hh);
    yoff = hh; // offset off client area inside frame
  }
}

//-----------------------------------------------------------------------------
// miscellaneous
//-----------------------------------------------------------------------------

void wxFrame::Command(int id)
{
  OnMenuCommand(id);
}

static void ForceFocus(Widget frame)
{
  static int force_focus = 0;

  if (!force_focus) {
    if (!wxGetBoolPreference("forceFocus", &force_focus))
      force_focus = 0;
    force_focus = !force_focus ? -1 : 1;
  }

  if (force_focus > 0) {
    Window current;
    int old_revert;
    XGetInputFocus(XtDisplay(frame), &current, &old_revert);
    if (current != PointerRoot) {
      XWindowAttributes attrib;

      XFlush(XtDisplay(frame));
      XGrabServer(XtDisplay(frame));
      
      /* Sleep for corce_focus usecs: */
      {
	struct timeval time;
	if (force_focus > 1000)
	  force_focus = 999;

	time.tv_sec = 0;
	time.tv_usec = force_focus;
	select(0, NULL, NULL, NULL, &time);
      }

      XGetWindowAttributes(XtDisplay(frame), XtWindow(frame), &attrib);
      if (attrib.map_state == IsViewable)
	XSetInputFocus(XtDisplay(frame), XtWindow(frame),
		       RevertToNone, CurrentTime);
    }
    XUngrabServer(XtDisplay(frame));
  }
}

extern "C" long scheme_get_milliseconds(void);

Bool wxFrame::Show(Bool show)
{
  wxChildList *tlf;
  
  if (show == IsShown()) { // do nothing if state doesn't change
    if (show) {
      wxUnpopMenu();
      /* Make sure window isn't iconized: */
      Iconize(FALSE);
      XRaiseWindow(XtDisplay(X->frame), XtWindow(X->frame));		   
      ForceFocus(X->frame);      
    }
    return TRUE;
  }

  tlf = wxTopLevelFrames(this);
  tlf->Show(this, show);
  if (parent) {
    wxChildList *cl;
    cl = parent->GetChildren();
    cl->Show(this, show);
  }
  
  SetShown(show);
  if (show) {
    wxUnpopMenu();
    XtMapWidget(X->frame);
    XRaiseWindow(XtDisplay(X->frame), XtWindow(X->frame));
    ForceFocus(X->frame);
    last_shown_time = scheme_get_milliseconds();
  } else {
    /* XWithdrawWindow tells the window manager to get rid of icons
       for iconified windows. Unfortunately, it also destroys the
       window under some (unknown) circumstances with CTWM - which is
       what I like to use. If we have waited a little while, CTWM
       seems happy. Solution: just don't call XWidthdrawWindow if the
       window was shown recently - the user hasn't had time to iconize
       it, anyway. */
    if (last_shown_time + 1000 < scheme_get_milliseconds())
      XWithdrawWindow(XtDisplay(X->frame), XtWindow(X->frame), XScreenNumberOfScreen(XtScreen(X->frame)));
    XtUnmapWidget(X->frame);
  }

  XFlush(XtDisplay(X->frame));
  XSync(XtDisplay(X->frame), FALSE);

  return TRUE;
}

void wxFrame::SetFrameModified(Bool mod)
{
  if (!!show_as_mod != !!mod) {
    char *t;
    t = GetTitle();
    t = copystring(t);
    show_as_mod = mod;
    SetTitle(t);
  }
}

char *wxFrame::GetTitle(void)
{
  char *t;
  t = wxWindow::GetTitle();
  if (t && show_as_mod) {
    int len;
    len = strlen(t);
    /* Double-check for asterisk: */
    if (len && t[len-1] == '*') {
      char *s;
      s = copystring(t);
      s[len-1] = 0;
      t = s;
    }
  }
  return t;
}

void wxFrame::SetTitle(char *title)
{
  if (show_as_mod && title) {
    int len;
    char *s;
    len = strlen(title);
    s = new WXGC_ATOMIC char[len + 2];
    memcpy(s, title, len);
    s[len] = '*';
    s[len+1] = 0;
    title = s;
  }
  wxWindow::SetTitle(title);
}


//-----------------------------------------------------------------------------
// virtual event functions
//-----------------------------------------------------------------------------

void wxFrame::OnMenuSelect(long id)
{
  SetStatusText(menubar->GetHelpString(id));
}

void wxFrame::OnMenuClick()
{
}

void wxFrame::OnToolbarButton()
{
}

void wxFrame::OnMDIActivate(Bool WXUNUSED(flag))
{
}
