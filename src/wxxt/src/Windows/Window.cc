/*								-*- C++ -*-
 *
 * Purpose: base class for all windows
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
#pragma implementation "Window.h"
#pragma GCC diagnostic ignored "-Wwrite-strings"
#endif

#define  Uses_XtIntrinsicP
#define  Uses_wxGDI
#define  Uses_wxLayout
#define  Uses_wxMenu
#define  Uses_wxMenuBar
#define  Uses_wxTypeTree
#define  Uses_wxWindow
#define  Uses_wxDialogBox
#define  Uses_wxItem
#define  Uses_wxCanvas
#define  Uses_wxApp
#define  Uses_wxClipboard
#include "wx.h"
#define  Uses_ScrollWinWidget
#define  Uses_Scrollbar
#define  Uses_ShellWidget
#define  Uses_SimpleWidget
#define  Uses_EnforcerWidget
#define  Uses_LabelWidget
#define  Uses_MultiListWidget
#define  Uses_ScrollbarWidget
#include "widgets.h"

#include "xdnd.h"

#include <X11/Xatom.h>
#include <X11/keysym.h> // needed for IsFunctionKey, etc.
#ifdef WX_USE_XFT
# include <X11/Xft/Xft.h>
#endif

static Atom utf8_atom = 0, net_wm_name_atom, net_wm_icon_name_atom;

extern void wxSetSensitive(Widget, Bool enabled);
extern int wxLocaleStringToChar(char *str, int slen);
extern int wxUTF8StringToChar(char *str, int slen);
extern wxWindow *wxLocationToWindow(int x, int y);

static wxWindow *grabbing_panel;
static Time grabbing_panel_time;
static Bool grabbing_panel_regsitered;

#include "xdnd.c"

static int dnd_inited = 0;
static DndClass dnd;

Atom wx_single_instance_tag = 0;

#ifndef NO_XMB_LOOKUP_STRING
static XIM the_im;
#endif

class Accum_Single_Instance_Message {
public:
  long src;
  char *accum;
  int len, size;
  Accum_Single_Instance_Message *next;
};
static int si_registered;
static Accum_Single_Instance_Message *si_msgs;
static void parse_and_drop_runtime(int len, char *s);
static void decode_percent_escapes(char *s);
extern void wxDrop_Runtime(char **argv, int argc);

//-----------------------------------------------------------------------------
// wxWindow constructor
//-----------------------------------------------------------------------------

#define ACTIVE_VIA_POINTER_FLAG 0x1
#define DISABLED_FLAG 0x2
#define SHOWN_FLAG 0x4
#define NO_AUTO_SCROLL_FLAG 0x8
#define FOCUS_FLAG 0x10
#define REPORT_ZERO_WIDTH_FLAG 0x20
#define REPORT_ZERO_HEIGHT_FLAG 0x40
#define LAST_WAS_ALT_DOWN_FLAG 0x80

wxWindow::wxWindow(void)
{ 
    __type = wxTYPE_WINDOW;

    // X representation
    X = new wxWindow_Xintern;
    X->frame = X->handle = X->scroll = NULL;
    X->translations_eventmask = 0;
    X->last_clicktime = 0;
    X->last_clickbutton = 0;
    // device context
    dc = NULL;
    // child <-> parent relationships
    parent   = NULL;
    children = DEBUG_NEW wxChildList;
    // layout information
    constraints = DEBUG_NEW wxLayoutConstraints;
    wxLC_MEM(constraints->left, Absolute(0));
    wxLC_MEM(constraints->top, Absolute(0));
    wxLC_MEM(constraints->width, AsIs());
    wxLC_MEM(constraints->height, AsIs());
    xoff = yoff = 0;
    // GDI objects
    cmap   = wxAPP_COLOURMAP;
    cursor = NULL /* wxSTANDARD_CURSOR */;
    font   = wxSYSTEM_FONT;
    // misc info
    allow_dclicks    = FALSE;
    captured         = FALSE;
    drag_accept      = FALSE;
    painting_enabled = TRUE;
    style            = 0;
    user_edit_mode   = FALSE;

    {
      wxWindow **wa;
      wa = (wxWindow **)MALLOC_SAFEREF();
      saferef = wa;
    }
    SET_SAFEREF(saferef, this);
    misc_flags = 0;
    /* except for frames, windows start out shown: */
    if (!wxSubType(__type, wxTYPE_FRAME))
      misc_flags |= SHOWN_FLAG;
    internal_disabled = 0;

    WXGC_IGNORE(this, parent);
}

wxWindow::~wxWindow(void)
{
#ifndef NO_XMB_LOOKUP_STRING
    if (X->ic) XDestroyIC(X->ic);
#endif

    // destroy children
    DestroyChildren(); DELETE_OBJ children; children = NULL;
    // destroy device context
    if (dc) DELETE_OBJ dc; dc = NULL;
    // remove from parents list
    if (parent)	parent->RemoveChild(this); parent = NULL;
    // destroy widgets
    wxSetSensitive(X->frame, TRUE);

    *saferef = NULL;

    dndTarget = NULL; /* just in case */

    if (X->frame) XtDestroyWidget(X->frame); X->frame = X->handle = X->scroll = NULL;
    DELETE_OBJ constraints; constraints = NULL;
    DELETE_OBJ X; X = NULL;
}

//-----------------------------------------------------------------------------
// child - parent relationships
//-----------------------------------------------------------------------------

wxWindow *wxWindow::GetGrandParent(void)
{
    return (parent ? parent->parent : (wxWindow*)NULL);
}

void wxWindow::AddChild(wxWindow *child)
{
  /* Propagate busy cursor flag */
  child->user_edit_mode = user_edit_mode;

  children->Append(child);
}

void wxWindow::DestroyChildren(void)
{
  wxChildNode *node;
  while ( (node=children->First()) != NULL ) {
    wxWindow *child;
    child = (wxWindow*)(node->Data());
    if (child) {
      DELETE_OBJ child;
    }
  }
}

void wxWindow::RemoveChild(wxWindow *child)
{
    children->DeleteObject(child);
}

//-----------------------------------------------------------------------------
// label, name, title of wxWindow
//-----------------------------------------------------------------------------

char *wxWindow::GetLabel(void)
{
  char *label = NULL;

  if (!X->frame) // forbid, if no widget associated
    return NULL;
  
  XtVaGetValues(X->frame, XtNlabel, &label, NULL);
  return label;
}

char *wxWindow::GetName(void)
{
    if (!X->handle) // forbid, if no widget associated
	return NULL;

    return XtName(X->handle);
}

char *wxWindow::GetTitle(void)
{
  char *title = NULL;
  
  if (!X->frame) // forbid, if no widget associated
    return NULL;
  
  XtVaGetValues(X->frame, XtNtitle, &title, NULL);
  return title;
}

void wxWindow::SetLabel(char *label)
{
  char *oldlabel = NULL;

  if (!X->frame) // forbid, if no widget associated
    return;

  XtVaGetValues(X->frame, XtNlabel, &oldlabel, NULL);
  if (oldlabel) {
    label = wxGetCtlLabel(label);
    XtVaSetValues(X->frame, XtNlabel, label, NULL);
  }
}

void wxWindow::SetName(char *name)
{
  // overwrite quark computed on widget creation
  XrmQuark q;
  q = XrmStringToName((name != NULL) ? name : "");
  X->handle->core.xrm_name = q;
}

void wxWindow::SetTitle(char *title)
{
    /* Note: widget must be realized */

    if (!X->frame) // forbid, if no widget associated
	return;

    if (!utf8_atom) {
      utf8_atom = XInternAtom(XtDisplay(X->frame), "UTF8_STRING", FALSE);
      net_wm_name_atom = XInternAtom(XtDisplay(X->frame), "_NET_WM_NAME", FALSE);
      net_wm_icon_name_atom = XInternAtom(XtDisplay(X->frame), "_NET_WM_ICON_NAME", FALSE);
    }

    /* Set title and icon title as string sintead fo utf-8. If
       a Window manager can handle UTF-8, we expect it to use the
       _NET variants. */
    XtVaSetValues(X->frame, 
		  XtNtitle, title, 
		  XtNiconName, title, 
		  XtNtitleEncoding, XA_STRING,
		  XtNiconNameEncoding, XA_STRING,
		  NULL);

    {
      int i;
      for (i = 0; i < 2; i++) {
	XChangeProperty(XtDisplay(X->frame), XtWindow(X->frame),
			!i ? net_wm_name_atom: net_wm_icon_name_atom,
			utf8_atom,
			8, PropModeReplace,
			(unsigned char *)title, strlen(title));
      }
    }
}

//-----------------------------------------------------------------------------
// set & query size and position of wxWindow
//-----------------------------------------------------------------------------

void wxWindow::Centre(int direction)
{
    int x=-1,       y=-1,       width=0,      height=0,
	parent_x=0, parent_y=0, parent_width, parent_height;

    // get position and width of parent
    if (parent) {
	if ( wxSubType(__type, wxTYPE_FRAME) )
	    parent->GetPosition(&parent_x, &parent_y);
	parent->GetClientSize(&parent_width, &parent_height);
    } else {
	wxDisplaySize(&parent_width, &parent_height);
    }
    // get position and size of THIS window
    GetPosition(&x, &y); GetSize(&width, &height);

    if (direction & wxCENTRE_TOPLEFT) {
	x = parent_x + parent_width / 2;
	y = parent_y + parent_height / 2;
    } else {
	// compute centered position
	if (direction & wxHORIZONTAL) {
	    x = parent_x + (parent_width - width) / 2;
	    if (x < 0)
	      x = 0;
	}
	if (direction & wxVERTICAL) {
	    y = parent_y + (parent_height - height) / 2;
	    if (y < 0)
	      y = 0;
       }
    }
    // move window
    Move(x, y);
}

void wxWindow::ClientToScreen(int *x, int *y)
{
  if (!X->handle) // forbid, if no widget associated
    return;

  {
    Display *dpy  = XtDisplay(X->handle);
    Screen  *scn  = XtScreen(X->handle);
    Window  root  = RootWindowOfScreen(scn);
    Window  win   = XtWindow(X->handle);
    Window  child;
    int xx = *x;
    int yy = *y;
    XTranslateCoordinates(dpy, win, root, xx, yy, x, y, &child);
  }
}

void wxWindow::Configure(int x, int y, int width, int height, int flags)
{
    Arg    args[4];
    int    i = 0;
    int _xoff = 0, _yoff = 0;
    Position cx, cy;
    Dimension cw, ch;

    if (!X->frame) // forbid, if no widget associated
	return;

    if (!wxSubType(__type, wxTYPE_FRAME) && parent) {
	_xoff = parent->xoff;
	_yoff = parent->yoff;
    }

    /* Make sure width, height != 0 */
    if (!width) {
      width = 1;
      misc_flags |= REPORT_ZERO_WIDTH_FLAG;
    } else
      misc_flags -= (misc_flags & REPORT_ZERO_WIDTH_FLAG);

    if (!height) {
      height = 1;
      misc_flags |= REPORT_ZERO_HEIGHT_FLAG;
    } else
      misc_flags -= (misc_flags & REPORT_ZERO_HEIGHT_FLAG);

    XtSetArg(args[0], XtNx, &cx);
    XtSetArg(args[1], XtNy, &cy);
    XtSetArg(args[2], XtNwidth, &cw);
    XtSetArg(args[3], XtNheight, &ch);
    
    XtGetValues(X->frame, args, 4);

    if (((x > -1) || ((flags & wxPOS_USE_MINUS_ONE) && (x > wxDEFAULT_POSITION)))
	&& ((Position)(x + _xoff) != cx))
      { args[i].name = XtNx; args[i].value = (Position)(x+_xoff); ++i; }
    if (((y > -1) || ((flags & wxPOS_USE_MINUS_ONE) && (y > wxDEFAULT_POSITION)))
	&& ((Position)(y + _yoff) != cy))
      { args[i].name = XtNy; args[i].value = (Position)(y+_yoff); ++i; }
    if ((width > -1) && ((Dimension)width != cw))
      { args[i].name = XtNwidth; args[i].value = (Dimension)width; ++i; }
    if ((height > -1) && ((Dimension)height != ch))
      { args[i].name = XtNheight; args[i].value = (Dimension)height; ++i; }

    if (i) {
      XtSetValues(X->frame, args, i);
      OnSize(width, height);
    }
}

void wxWindow::GetPosition(int *x, int *y)
{
    int _xoff = 0, _yoff = 0;
    Position xx, yy;

    if (!X->frame) // forbid, if no widget associated
	return;

    if (!wxSubType(__type, wxTYPE_FRAME) && parent) {
	_xoff = parent->xoff;
	_yoff = parent->yoff;
    }

    XtVaGetValues(X->frame, XtNx, &xx, XtNy, &yy, NULL);
    *x = (int)xx - _xoff; *y = (int)yy - _yoff;
}

void wxWindow::GetSize(int *width, int *height)
{
    Dimension ww, hh;

    if (!X->frame) // forbid, if no widget associated
	return;

    XtVaGetValues(X->frame, XtNwidth, &ww, XtNheight, &hh, NULL);
    *width = ww; *height = hh;
    
    if (misc_flags & REPORT_ZERO_WIDTH_FLAG)
      *width = 0;
    if (misc_flags & REPORT_ZERO_HEIGHT_FLAG)
      *height = 0;
}

/* Client size is different from size */
void wxWindow::GetClientSize(int *width, int *height)
{
    Dimension dww, dhh, fw, fh;
    int ww, hh;

    if (!X->handle) // forbid, if no widget associated
	return;

    XtVaGetValues(X->handle, XtNwidth, &dww, XtNheight, &dhh, NULL);
    if (X->scroll && !(misc_flags & NO_AUTO_SCROLL_FLAG)) {
      /* Handle window is possibly bigger than actual visible area */
      Dimension xs, ys;
      xws_get_scroll_area(X->scroll, &xs, &ys);
      if (wxSubType(__type, wxTYPE_LIST_BOX)) {
	dww = xs;
	dhh = ys;
      } else {
	if (xs < dww)
	  dww = xs;
	if (ys < dhh)
	  dhh = ys;
      }
    }

    XtVaGetValues(X->frame, XtNwidth, &fw, XtNheight, &fh, NULL);

    /* If frame < contained, don't believe the number! */
    if (fw < dww)
      dww = 0;
    if (fh < dhh)
      dhh = 0;

    ww = (int)dww;
    hh = (int)dhh;

    if (XtIsSubclass(X->handle, xfwfFrameWidgetClass)) {
      Dimension fw, oo, io;

      XtVaGetValues(X->handle, 
		    XtNframeWidth, &fw, 
		    XtNouterOffset, &oo, 
		    XtNinnerOffset, &io, 
		    NULL);

      ww = ww - fw - oo - io;
      hh = hh - fw - oo - io;
    }

    *width = ww; *height = hh;
}

void wxWindow::ScreenToClient(int *x, int *y)
{
  if (!X->handle) // forbid, if no widget associated
    return;
    
  {
    Display *dpy  = XtDisplay(X->handle);
    Screen  *scn  = XtScreen(X->handle);
    Window  root  = RootWindowOfScreen(scn);
    Window  win   = XtWindow(X->handle);
    Window  child;
    int xx = *x;
    int yy = *y;
    XTranslateCoordinates(dpy, root, win, xx, yy, x, y, &child);
  }
}

void wxWindow::SetSize(int x, int y, int width, int height, int flags)
{
  if ((x > -1) || ((flags & wxPOS_USE_MINUS_ONE) && (x > wxDEFAULT_POSITION)))
    wxLC_MEM(constraints->left, Absolute(x));
  if ((y > -1) || ((flags & wxPOS_USE_MINUS_ONE) && (y > wxDEFAULT_POSITION)))
    wxLC_MEM(constraints->top, Absolute(y));

  if (width > -1)
    wxLC_MEM(constraints->width, Absolute(width));
  else
    wxLC_MEM(constraints->width, AsIs());
  if (height > -1)
    wxLC_MEM(constraints->height, Absolute(height));
  else
    wxLC_MEM(constraints->height, AsIs());

  Configure(x, y, width, height, flags);
}

//-----------------------------------------------------------------------------
// GDI objects (font, cursor)
//-----------------------------------------------------------------------------

wxCursor *wxWindow::SetCursor(wxCursor *new_cursor)
{
  wxCursor *previous;

  if (!X->handle) // forbid, if no widget associated
    return NULL;
  
  previous = cursor;
  
  if (!new_cursor || (new_cursor && new_cursor->Ok())) {
    cursor = new_cursor;
    if (!user_edit_mode) { /* really indicates busy_cursor */
      Cursor c;
      c = (new_cursor ? GETCURSOR(new_cursor) : None);
      XtVaSetValues(X->handle, XtNcursor, c, NULL);
      if (__type == wxTYPE_LIST_BOX) {
	/* Yuck. Set cursor for total client area of listbox */
	XtVaSetValues(XtParent(X->handle), XtNcursor, c, NULL);
      }
      if ((__type == wxTYPE_PANEL)
	  || (__type == wxTYPE_FRAME)
	  || (__type == wxTYPE_DIALOG_BOX)) {
	/* Yuck. If the child panel has grabbed the cursor, update
	   it. */
	if (grabbing_panel) {
	  wxWindow *p = grabbing_panel;
	  while (p) {
	    if (p->cursor)
	      break;
	    if (wxSubType(p->__type, wxTYPE_FRAME)
		|| wxSubType(p->__type, wxTYPE_DIALOG_BOX))
	      p = NULL;
	    else
	      p = p->GetParent();
	  }
	
	  if (p == this) {
	    /* Grabbing panel uses this cursor */
	    XChangeActivePointerGrab(wxAPP_DISPLAY, 
				     (ButtonPressMask | ButtonReleaseMask
				      | ButtonMotionMask | PointerMotionMask | PointerMotionHintMask
				      | EnterWindowMask | LeaveWindowMask),
				     c, 
				     grabbing_panel_time);
	  }
	}
      }
    }
  }
  
  return previous;
}

//-----------------------------------------------------------------------------
// layout
//-----------------------------------------------------------------------------

void wxWindow::SetConstraints(wxLayoutConstraints *constr)
{
  if (constr)
    constraints = constr;
}

// void wxWindow::Layout(void)
// --> wxLayout.cc

//-----------------------------------------------------------------------------
// Scrolling
//-----------------------------------------------------------------------------

void wxWindow::EnableScrolling(Bool x, Bool y)
{
  if (X->scroll) {
    XtVaSetValues(X->scroll, XtNhideHScrollbar, !x, XtNhideVScrollbar, !y, NULL);
  }
}

int wxWindow::GetScrollPos(int orient)
{
    if (!X->scroll) return 0; // window is not scrollable

    if (!(misc_flags & NO_AUTO_SCROLL_FLAG))
      return 0;
 
    if (misc_flags & NO_AUTO_SCROLL_FLAG) {
      return orient == wxHORIZONTAL ? hs_pos : vs_pos;
    } else {
      Position pos;
      XtVaGetValues(X->handle, orient == wxHORIZONTAL ? XtNx : XtNy, &pos, NULL);
      return -pos;
    }
}

int wxWindow::GetScrollRange(int orient)
{
    if (!X->scroll) return 0; // window is not scrollable

    if (!(misc_flags & NO_AUTO_SCROLL_FLAG))
      return 0;
 
    return orient == wxHORIZONTAL ? hs_width : vs_width;
}

int wxWindow::GetScrollPage(int orient)
{
  if (!X->scroll) return 0; // window is not scrollable
  
    if (!(misc_flags & NO_AUTO_SCROLL_FLAG))
      return 0;
 
  if (orient == wxHORIZONTAL) {
    if (!hs_width)
      return 0;
    return hs_page;
  } else {
    if (!vs_width)
      return 0;
    return vs_page;
  }
}

void wxWindow::Scroll(int x_pos, int y_pos)
{
    if (!X->scroll)
	return; // don't want to scroll or window not scrollable

    if (misc_flags & NO_AUTO_SCROLL_FLAG) {
      if (x_pos >= 0) {
	hs_pos = x_pos;
	if (hs_pos > hs_width)
	  hs_pos = hs_width;
      }
      if (y_pos >= 0) {
	vs_pos = y_pos;
	if (vs_pos > vs_width)
	  vs_pos = vs_width;
      }

      xws_set_scroll_direct(X->scroll, hs_width, hs_page, hs_pos, vs_width, vs_page, vs_pos);
    } else {
      Position dummy; int wd, ht; Dimension gwd, ght;
      // size of view port
      XfwfCallComputeInside(X->scroll, &dummy, &dummy, &wd, &ht);
      // size of scrollable window
      XtVaGetValues(X->handle, XtNheight, &ght, XtNwidth, &gwd, NULL);
      // get missing position if any (x_pos <0 || y_pos < 0)
      if (x_pos < 0) { XtVaGetValues(X->handle, XtNx, &dummy, NULL); x_pos = -dummy; }
      if (y_pos < 0) { XtVaGetValues(X->handle, XtNy, &dummy, NULL); y_pos = -dummy; }
      // compute correct (x,y)-position - 0<=x<=gwd-wd, 0<=y<=ght-ht
      x_pos = min(x_pos, gwd-wd); x_pos = max(0, x_pos);
      y_pos = min(y_pos, ght-ht); y_pos = max(0, y_pos); 
      // set position
      XtVaSetValues(X->handle, XtNx, (Position)(-x_pos), XtNy, (Position)(-y_pos), NULL);
    }
}

void wxWindow::SetScrollArea(int gwd, int ght)
{
    Dimension d;
    int wd, ht; 
    Position p, x, y;

    if ((gwd <= 0 && ght <= 0) || !X->scroll)
	return; // don't want to resize or window not scrollable

    // position of scrollable window
    XtVaGetValues(X->handle, XtNx, &x, XtNy, &y, NULL);
    // size of viewport
    XfwfCallComputeInside(X->scroll, &p, &p, &wd, &ht);
    // get missing sizes if any (gwd <0 || ght < 0)
    if (gwd < 0) { XtVaGetValues(X->handle, XtNwidth, &d, NULL);  gwd = d; }
    if (!gwd) gwd = 1;
    if (ght < 0) { XtVaGetValues(X->handle, XtNheight, &d, NULL); ght = d; }
    if (!ght) ght = 1;
    // compute correct (x,y)-position - 0<=x<=gwd-wd, 0<=y<=ght-ht
    x = min(x, gwd-wd); x = max(0, x);
    y = min(y, ght-ht); y = max(0, y); 
    // set size and reposition if necessary (x,y changed)
    XtVaSetValues(X->handle,
		  XtNx,     x,              XtNy,      y,
		  XtNwidth, (Dimension)gwd, XtNheight, (Dimension)ght,
		  NULL);
}

void wxWindow::SetScrollPos(int orient, int pos)
{
  if (!(misc_flags & NO_AUTO_SCROLL_FLAG))
    return;
  
  if (orient == wxHORIZONTAL)	wxWindow::Scroll(pos < 0 ? 0 : pos, -1);
  else			        wxWindow::Scroll(-1, pos < 0 ? 0 : pos);
}

void wxWindow::SetScrollRange(int orient, int range)
{
  if (!(misc_flags & NO_AUTO_SCROLL_FLAG))
    return;

  if (orient == wxHORIZONTAL) {
    hs_width = range;
    if (hs_pos > hs_width)
      hs_pos = hs_width;
  } else {
    vs_width = range;
    if (vs_pos > vs_width)
      vs_pos = vs_width;
  }
  
  xws_set_scroll_direct(X->scroll, hs_width, hs_page, hs_pos, vs_width, vs_page, vs_pos);
}

void wxWindow::SetScrollPage(int orient, int range)
{
  if (!(misc_flags & NO_AUTO_SCROLL_FLAG))
    return;
  
  if (range <= 0)
    range = 1;
  if (orient == wxHORIZONTAL) {
    if (!hs_width)
      hs_page = 1;
    else
      hs_page = range;
  } else {
    if (!vs_width)
      vs_page = 1;
    else
      vs_page = range;
  }

  xws_set_scroll_direct(X->scroll, hs_width, hs_page, hs_pos, vs_width, vs_page, vs_pos);
}

//-----------------------------------------------------------------------------
// miscellaneous
//-----------------------------------------------------------------------------

void wxWindow::CaptureMouse(void)
{
    if (!X->frame) // forbid, if no widget associated
	return;

    if (!captured) {
	XtAddGrab(X->frame, TRUE, FALSE);
	captured = TRUE;
    }
};

void wxWindow::ChangeToGray(Bool gray)
{
  if (XtIsSubclass(X->handle, xfwfLabelWidgetClass)
      || XtIsSubclass(X->handle, xfwfMultiListWidgetClass))
    XtVaSetValues(X->handle, XtNdrawgray, (Boolean)gray, NULL);

  if (X->scroll)
    XtVaSetValues(X->scroll, XtNdrawgrayScrollWin, (Boolean)gray, NULL);

  if (XtIsSubclass(X->frame, xfwfEnforcerWidgetClass))
    XtVaSetValues(X->frame, XtNdrawgray, (Boolean)gray, NULL);

  if (gray)
    ReleaseFocus();
}

void wxWindow::ReleaseFocus()
{
  /* If disabling and this window has the focus, get rid of it: */
  if (misc_flags & FOCUS_FLAG) {
    wxWindow *p;
    p = GetParent();
    while (p) {
      if (wxSubType(p->__type, wxTYPE_FRAME)) {
	p->SetFocus();
	break;
      }
      p = p->GetParent();
    }
  }
}

void wxWindow::ReleaseAllFocus()
{
  ReleaseFocus();
}

Bool wxWindow::IsGray(void)
{
  return (misc_flags & DISABLED_FLAG) || internal_gray_disabled;
}

void wxWindow::InternalEnable(Bool enable, Bool gray)
{
  Bool do_something;
  short start_igd = internal_gray_disabled;
  
  if (!X->frame || !X->handle) // forbid, if no widget associated
    return;
  
  if (!enable) {
    do_something = !internal_disabled;
    internal_disabled++;
    if (gray)
      internal_gray_disabled++;
  } else { 
    --internal_disabled;
    do_something = !internal_disabled;
    if (gray)
      --internal_gray_disabled;
  }

  if (do_something && !(misc_flags & DISABLED_FLAG))
    wxSetSensitive(X->frame, enable);

  if ((!!internal_gray_disabled != !!start_igd) && !(misc_flags & DISABLED_FLAG))
    ChangeToGray(!!internal_gray_disabled);
}

void wxWindow::Enable(Bool enable)
{
  Bool orig_enabled = !(misc_flags & DISABLED_FLAG);

  if (!X->frame || !X->handle) // forbid, if no widget associated
    return;
  
  if (orig_enabled == !!enable)
    return;

  if (!enable)
    misc_flags |= DISABLED_FLAG;
  else
    misc_flags -= DISABLED_FLAG;

  if (!internal_disabled)
    wxSetSensitive(X->frame, enable);

  /* Doing handle sensitive makes it gray: */
  if (!internal_gray_disabled)
    ChangeToGray(!enable);
}

Bool wxWindow::PopupMenu(wxMenu *menu, double x, double y, Bool for_choice, int top_extra)
{
  int dev_x = (int)x;
  int dev_y = (int)y;
  
  if (!X->frame || !X->handle) // forbid, if no widget associated
    return FALSE;

  ClientToScreen(&dev_x, &dev_y);
  menu->PopupMenu(X->frame, dev_x, dev_y, for_choice, top_extra);
  return TRUE;
}

void wxWindow::GetRefreshSize(int *w, int *h)
{
  GetSize(w, h);
}

void wxWindow::Refresh(void)
{
    XExposeEvent  dummyEvent;
    int           width, height;

    if (!X->handle) // forbid, if no widget associated
	return;

    GetRefreshSize(&width, &height);

    dummyEvent.type	  = Expose;
    dummyEvent.display	  = XtDisplay(X->handle);
    dummyEvent.send_event = True;
    dummyEvent.window	  = XtWindow(X->handle);
    dummyEvent.x	  = 0;
    dummyEvent.y	  = 0;
    dummyEvent.width	  = width;
    dummyEvent.height	  = height;
    dummyEvent.count	  = 0;

    XSendEvent(XtDisplay(X->handle), XtWindow(X->handle),
	       False, ExposureMask, (XEvent*)&dummyEvent);
}

void wxWindow::ReleaseMouse(void)
{
    if (!X->frame) // forbid, if no widget associated
	return;

    if (captured) {
	XtRemoveGrab(X->frame);
	captured = FALSE;
    }
};

void wxWindow::SetFocus(void)
{
  wxWindow *win;

  if (!X->frame) // forbid, if no widget associated
    return;

  if (IsGray() || !IsShown())
    return;

  if (!WantsFocus())
    return;

  if (misc_flags & FOCUS_FLAG)
    /* focus is already here */
    return;

  // search for the frame of this widget
  win = this;
  for (/*wxWindow *win = this*/; win; win = win->parent) {
    if (wxSubType(win->__type, wxTYPE_FRAME))
      break;
  }

  // if found: set focus
  if (win)
    XtSetKeyboardFocus(win->X->frame, X->frame);
}

Bool wxWindow::Show(Bool show)
{
    if (parent) {
      wxChildList *cl;
      cl = parent->GetChildren();
      cl->Show(this, show);
    }

    if (!X->handle) // forbid, if no widget associated
      return TRUE;

    if (!show)
      ReleaseAllFocus();

    /* Get rid of or restore focus traversal */
    if (XtIsSubclass(X->frame, xfwfCommonWidgetClass))
      XtVaSetValues(X->frame, XtNtraversalOn, (Boolean)show, NULL);

    /* Show/hide frame */
    if (show)
      XtManageChild(X->frame);
    else
      XtUnmanageChild(X->frame);

    SetShown(show);

    return TRUE;
}

Bool wxWindow::IsShown(void)
{
  return !!(misc_flags & SHOWN_FLAG);
}

void wxWindow::SetShown(Bool shown)
{
  if (shown)
    misc_flags |= SHOWN_FLAG;
  else
    misc_flags -= (misc_flags & SHOWN_FLAG);
}
    

//-----------------------------------------------------------------------------
// virtual event functions, that implement the default behaviour
//-----------------------------------------------------------------------------

//-- DIRTY -- DIRTY -- DIRTY -- DIRTY -- DIRTY --
//
_XFUNCPROTOBEGIN
extern EventMask _XtConvertTypeToMask(int type);	  // internal Xt function
extern void      _XtTranslateEvent(Widget w, XEvent *ev); // internal Xt function
_XFUNCPROTOEND
//
// I've used the following way to intercept the incomming events:
// - first Xt calls the expose method of the widget
// - second it calls all event handlers installed by XtAddEventHandler
// - third it evaluates the widget's translation table
// --> I forbid the evaluation of the translation table and call
// 	_XtTranslateEvent by myself. 
//
//-- DIRTY -- DIRTY -- DIRTY -- DIRTY -- DIRTY --

void wxWindow::OnChar(wxKeyEvent* wxevent)
{
    GC_CAN_IGNORE XEvent onstack;
    XEvent *xev = (XEvent*)wxevent->eventHandle; // X event
    if (!xev) {
     xev = &onstack;
     xev->xkey.type = KeyPress;
     xev->xkey.display = wxAPP_DISPLAY;
     xev->xkey.window = XtWindow(X->handle);
     xev->xkey.root = RootWindowOfScreen(wxAPP_SCREEN);
     xev->xkey.subwindow = XtWindow(X->handle);
     xev->xkey.time = 0L;
     xev->xkey.send_event = 0;
     xev->xkey.same_screen = 0;
     xev->xkey.serial = 0;
    }

    // check if widget has translations and if this event is selected by the widget
    if (X->handle->core.tm.translations
	&& (X->translations_eventmask & _XtConvertTypeToMask(xev->type))) {
      // translate wxKeyEvent to XEvent
      KeySym keysym;
      keysym = CharCodeWXToX(wxevent->keyCode);
      if (keysym != 0) {
	long kc;
	kc = XKeysymToKeycode(xev->xkey.display, keysym);
	xev->xkey.keycode = kc;
	xev->xkey.x	 = (int)wxevent->x;
	xev->xkey.y	 = (int)wxevent->y;
	xev->xkey.state &= ~(ShiftMask | ControlMask | Mod1Mask | Mod3Mask | LockMask);
	xev->xkey.state |= (wxevent->altDown     ? Mod3Mask    : 0) |
			   (wxevent->controlDown ? ControlMask : 0) |
			   (wxevent->metaDown    ? Mod1Mask    : 0) |
			   (wxevent->shiftDown   ? ShiftMask   : 0) |
			   (wxevent->capsDown    ? LockMask    : 0);
	// call Widget methods to handle this event
	_XtTranslateEvent(X->handle, xev);
      }
    }
}

void wxWindow::OnCommand(wxWindow* win, wxCommandEvent* event)
{
    // OnCommand events are routed to the parent by default
    if (parent)
      parent->OnCommand(win, event);
}

void wxWindow::OnEvent(wxMouseEvent* wxevent)
{
  EventMask  mask;
  XEvent    *xev = (XEvent*)wxevent->eventHandle; // X event

  if (!xev) return;

  mask = _XtConvertTypeToMask(xev->xany.type); // eventmask of event

  // adapt converted mask (Xt error????)
  if (mask & ButtonMotionMask)
    mask |= Button1MotionMask | Button2MotionMask | Button3MotionMask |
      Button4MotionMask | Button5MotionMask;
  // check if widget has translations and if this event is selected by the widget
  if (X->handle->core.tm.translations && (X->translations_eventmask & mask)) {
    // no translation of wxMouseEvents to XEvents as for OnChar
    // --- may be added on request ---
    // call Widget methods to handle this event
    _XtTranslateEvent(X->handle, xev);
  }
}

Bool wxWindow::PreOnChar(wxWindow *, wxKeyEvent *)
{
  return FALSE;
}

Bool wxWindow::PreOnEvent(wxWindow *, wxMouseEvent *)
{
  return FALSE;
}

Bool wxWindow::CallPreOnChar(wxWindow *win, wxKeyEvent *event)
{
  wxWindow *p;

  p = win->GetParent();

  if (wxSubType(win->__type, wxTYPE_MENU_BAR)
      || wxSubType(win->__type, wxTYPE_MENU))
    return FALSE;

  if (wxSubType(win->__type, wxTYPE_FRAME)
      || wxSubType(win->__type, wxTYPE_DIALOG_BOX))
    p = NULL;

  return ((p && CallPreOnChar(p, event))
	  || win->IsGray()
	  || win->PreOnChar(this, event));
}

Bool wxWindow::CallPreOnEvent(wxWindow *win, wxMouseEvent *event)
{
  wxWindow *p;

  p = win->GetParent();

  if (wxSubType(win->__type, wxTYPE_MENU_BAR)
      || wxSubType(win->__type, wxTYPE_MENU))
    return FALSE;

  if (wxSubType(win->__type, wxTYPE_FRAME)
      || wxSubType(win->__type, wxTYPE_DIALOG_BOX))
    p = NULL;

  return ((p && CallPreOnEvent(p, event)) 
	  || win->IsGray()
	  || win->PreOnEvent(this, event));
}

void wxWindow::OnPaint(void)
{
    // This works only for subclasses of the xfwfCommonWidgetClass
    XfwfCallExpose(X->handle, X->expose_event, X->expose_region);
}

void wxWindow::OnScroll(wxScrollEvent*)
{
}

//-----------------------------------------------------------------------------
// apply event handling to a wxWindow
//-----------------------------------------------------------------------------

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

static void FreeSaferef(Widget WXUNUSED(w), wxWindow** winp,
			XtPointer WXUNUSED(null))
{
  FREE_SAFEREF((char *)winp);

  /* No XFORM_RESET_VAR_STACK because this one isn't xformed.  No need
     to xform because FREE_SAFEREF won't set the GC variable stack. */
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

void wxWindow::FocusChangeCallback(void*,
				   wxWindow **winp,
				   void*on)
{
  wxWindow *win = (wxWindow *)GET_SAFEREF(winp);

  if (!win) {
#ifdef MZ_PRECISE_GC
    XFORM_RESET_VAR_STACK;
#endif
    return;
  }

  if (on) {
    win->misc_flags |= FOCUS_FLAG;
    win->OnSetFocus();
  } else { 
    win->misc_flags -= (win->misc_flags & FOCUS_FLAG);
    win->OnKillFocus();
  }

#ifdef MZ_PRECISE_GC
  XFORM_RESET_VAR_STACK;
#endif
}

void wxWindow::RegisterAll(Widget ww)
{
  XtInsertEventHandler
    (ww,
     ButtonPressMask |	// for OnEvent
     ButtonReleaseMask |
     ButtonMotionMask |
     PointerMotionMask | PointerMotionHintMask,
     FALSE,
     (XtEventHandler)wxWindow::WindowEventHandler,
     (XtPointer)saferef,
     XtListHead);
  
  if (XtIsComposite(ww)) {
    Widget *w;
    Cardinal c, i;

    XtVaGetValues(ww, XtNchildren, &w, XtNnumChildren, &c, NULL);
    for (i = 0; i < c; i++) {
      RegisterAll(w[i]);
    }
  }
}

void wxWindow::AddEventHandlers(void)
{
  wxWindow * win;
  long mask, extra_mask;

  if (!X->frame || !X->handle) // forbid, if no widget associated
    return;

    // event handler for frame
    XtInsertEventHandler(X->frame,		// handle events for frame widget
			 StructureNotifyMask |	// for OnSize, OnEvent
			 SubstructureNotifyMask,// for adding of window-eventhandler
			 TRUE,			// for OnClose
			 (XtEventHandler)wxWindow::FrameEventHandler,
			 (XtPointer)saferef,
			 XtListHead);
    // handle expose events (works only for subclasses of xfwfCommonWidgetClass)
    if (XtIsSubclass(X->handle, xfwfCommonWidgetClass)) {
	XtAddCallback(X->handle, XtNexposeCallback,
		      (XtCallbackProc)wxWindow::ExposeEventHandler,
		      (XtPointer)saferef);
	XtVaSetValues(X->handle, XtNuseExposeCallback, TRUE, NULL);
      XtAddCallback(X->handle, XtNfocusHiliteChange,
		    (XtCallbackProc)FocusChangeCallback, 
		    (XtPointer)saferef);
    }
    // handle scroll events (works only for scrollable widgets)
    if (X->scroll) {
      XtAddCallback(X->scroll, XtNscrollCallback,
		    (XtCallbackProc)wxWindow::ScrollEventHandler,
		    (XtPointer)saferef);
      if (XtIsSubclass(X->scroll, xfwfCommonWidgetClass))
	XtAddCallback(X->scroll, XtNfocusHiliteChange,
		      (XtCallbackProc)FocusChangeCallback, 
		      (XtPointer)saferef);
    }

    if (XtIsSubclass(X->frame, xfwfCommonWidgetClass)) {
      XtAddCallback(X->frame, XtNonDestroy,
		    (XtCallbackProc)FreeSaferef,
		    (XtPointer)saferef);
      XtAddCallback(X->frame, XtNfocusHiliteChange,
		    (XtCallbackProc)FocusChangeCallback, 
		    (XtPointer)saferef);
    }

    win = this;

    // for OnPaint (non-xfwfCommonWidget-subclasses)
    extra_mask = (XtIsSubclass(win->X->handle, xfwfCommonWidgetClass)
		  ? NoEventMask 
		  : ExposureMask);

    mask = XtBuildEventMask(win->X->handle);
    win->X->translations_eventmask = mask;
    XtInsertEventHandler
      (win->X->handle,	// handle events for client area widget
       KeyPressMask |	// for OnChar
       KeyReleaseMask |
       ButtonPressMask |	// for OnEvent
       ButtonReleaseMask |
       ButtonMotionMask |
       PointerMotionMask | PointerMotionHintMask |
       EnterWindowMask |
       LeaveWindowMask |
       extra_mask,
       FALSE,
       (XtEventHandler)wxWindow::WindowEventHandler,
       (XtPointer)saferef,
       XtListHead);

    if (__type == wxTYPE_LIST_BOX) {
      /* Yuck. Get Mouse-moved events in total client area of listbox */
      XtInsertEventHandler
	(XtParent(win->X->handle),
	 ButtonPressMask |	// for OnEvent
	 ButtonReleaseMask |
	 ButtonMotionMask |
	 PointerMotionMask | PointerMotionHintMask,
	 FALSE,
	 (XtEventHandler)wxWindow::WindowEventHandler,
	 (XtPointer)saferef,
	 XtListHead);
    }

    if (win->X->scroll)
      RegisterAll(win->X->scroll);

    /* Yucky hack to make PreOnChar work for messages, sliders, and gauges: */
    extra_mask = ((wxSubType(win->__type, wxTYPE_MESSAGE) 
		   || wxSubType(win->__type, wxTYPE_SLIDER) 
		   || wxSubType(win->__type, wxTYPE_GAUGE))
		  ? (KeyPressMask | KeyReleaseMask) : NoEventMask);

    XtInsertEventHandler
      (win->X->frame,	// handle events for frame widget
       EnterWindowMask |
       LeaveWindowMask |
       FocusChangeMask | // for OnKillFocus, OnSetFocus
       extra_mask,
       FALSE,
       (XtEventHandler)wxWindow::WindowEventHandler,
       (XtPointer)saferef,
       XtListHead);
}

void wxWindow::ExposeEventHandler(Widget     WXUNUSED(w),
				  wxWindow** winp,
				  XtPointer  p_XfwfExposeInfo)
{
  XfwfExposeInfo *einfo;
  wxWindow *win = (wxWindow *)GET_SAFEREF(winp);

  if (!win) {
#ifdef MZ_PRECISE_GC
    XFORM_RESET_VAR_STACK;
#endif
    return;
  }

  einfo = (XfwfExposeInfo*)p_XfwfExposeInfo;

  if (win->painting_enabled) { // painting is allowed
    Region myregion;

    if (win->dc) {
      if (!(win->dc->ok)) { // setup drawable of dc on first expose
	win->dc->X->drawable = XtWindow(win->X->handle);
	win->dc->X->draw_window = win->dc->X->drawable;
	win->dc->SetBackground(win->dc->current_background_color);
	win->dc->Clear();
	win->dc->ok = TRUE;
      }
      // Set up clipping region.
      // Make a copy because Xt apparently has only one region that it uses
      myregion = XCreateRegion(); 
      XUnionRegion(myregion, einfo->region, myregion);

      win->dc->X->expose_reg = myregion;
      win->dc->SetCanvasClipping();
    } else
      myregion = NULL;

    // call refresh method
    win->X->expose_region = einfo->region;
    win->X->expose_event  = einfo->event;
    win->Paint();

    if (win->dc) {
      // reset clipping region
      win->dc->X->expose_reg = NULL;
      win->dc->SetCanvasClipping();

      XDestroyRegion(myregion);
    }
  }

#ifdef MZ_PRECISE_GC
  XFORM_RESET_VAR_STACK;
#endif
}

void wxWindow::FrameEventHandler(Widget w,
				 wxWindow **winp,
				 XEvent *xev,
				 Boolean *WXUNUSED(continue_to_dispatch_return))
{
  wxWindow *win = (wxWindow *)GET_SAFEREF(winp);
  if (!win) {
#ifdef MZ_PRECISE_GC
    XFORM_RESET_VAR_STACK;
#endif
    return;
  }

  switch (xev->xany.type) {
  case ClientMessage:
    // only wxFrames have a WM_DELETE_WINDOW property
    if(!strcmp(XGetAtomName(XtDisplay(w),xev->xclient.message_type),"WM_PROTOCOLS")
       && !strcmp(XGetAtomName(XtDisplay(w),xev->xclient.data.l[0]),"WM_DELETE_WINDOW")){
      // I've reveived a WM_DELETE_WINDOW message for win
      wxWindow *current_modal;
      current_modal = wxGetModalWindow(win);
      if (current_modal && (current_modal != win))
	return;
	
      // close frame, if allowed
      if (win->OnClose())
	win->Show(FALSE);
    }
    if (wx_single_instance_tag) {
      if (xev->xclient.message_type == wx_single_instance_tag) {
	/* Accumulate msg data */
	long src = 0;
	int i;
	Accum_Single_Instance_Message *msg, *prev = NULL;

	if (!si_registered) {
	  wxREGGLOB(si_msgs);
	}

	for (i = sizeof(Window); i--; ) {
	  src = (src << 8) | ((int)xev->xclient.data.b[i]);
	}

	for (msg = si_msgs; msg; msg = msg->next) {
	  if (msg->src == src)
	    break;
	}
	if (!msg) {
	  char *s;
	  s = new WXGC_ATOMIC char[128];
	  msg = new WXGC_PTRS Accum_Single_Instance_Message;
	  msg->next = si_msgs;
	  si_msgs = msg;
	  msg->src = src;
	  msg->accum = s;
	  msg->len = 0;
	  msg->size = 128;
	}
	
	{
	  int len = sizeof(Window);
	  while (len < 20 && xev->xclient.data.b[len]) {
	    len++;
	  }
	  len -= sizeof(Window);

	  if (len) {
	    /* accumulate data */
	    if (msg->size < msg->len + 1 + len) {
	      char *naya;
	      int new_size = msg->size * 2;
	      naya = new WXGC_ATOMIC char[new_size];
	      memcpy(naya, msg->accum, msg->len);
	      msg->accum = naya;
	      msg->size = new_size;
	    }
	    memcpy(msg->accum + msg->len, 
		   xev->xclient.data.b + sizeof(Window),
		   len);
	    msg->len += len;
	    if (len < (int)(20 - sizeof(Window)))
	      len = 0; /* inidicate that we're done */
	  } 

	  if (!len) {
	    /* done */
	    if (prev)
	      prev->next = msg->next;
	    else
	      si_msgs = msg->next;
	    msg->accum[msg->len] = 0;

	    parse_and_drop_runtime(msg->len, msg->accum);
	  }
	}
      }
    }
    if (dnd_inited) {
      if (xev->xclient.message_type == dnd.XdndEnter) {
	/* Ok... */
      } else if (xev->xclient.message_type == dnd.XdndPosition) {
	wxWindow *target = NULL;
        
	/* Find immediate target window: */
        {
          Display *dpy  = XtDisplay(w);
          Screen  *scn  = XtScreen(w);
          Window  root  = RootWindowOfScreen(scn);
          Window  xwin  = XtWindow(w);
          Window  child = 0;
          int cx, cy;
	  cx = XDND_POSITION_ROOT_X(xev);
	  cy = XDND_POSITION_ROOT_Y(xev);
	  while (1) {
	    if (XTranslateCoordinates(dpy, root, xwin, cx, cy,
				      &cx, &cy, &child)) {
	      if (!child)
		break;
	      else {
		root = xwin;
		xwin = child;
	      }
	    } else
	      break;
	  }
	  if (xwin) {
	    Widget cw;
	    cw = XtWindowToWidget(dpy, xwin);
	    if (cw) {
	      target = win->FindChildByWidget(cw);
	    }
          }
        }

	/* Does this window (if found) accept drops? Or maybe a parent? */
        while (target && !target->drag_accept) {
          if (wxSubType(target->__type, wxTYPE_FRAME)
	      || wxSubType(target->__type, wxTYPE_DIALOG_BOX)) {
            target = NULL;
            break;
          } else {
            target = target->GetParent();
          }
        }

	xdnd_send_status(&dnd, XDND_ENTER_SOURCE_WIN(xev), xev->xclient.window, 
                         !!target,
			 0, 0, 0, 10, 10, dnd.XdndActionPrivate);
        
	win->dndTarget = target;
      } else if (xev->xclient.message_type == dnd.XdndDrop) {
	if (win->dndTarget) {
	  wxWindow *target = win->dndTarget;
	  win->dndTarget = NULL;
	  if (!xdnd_convert_selection(&dnd, XDND_DROP_SOURCE_WIN(xev), xev->xclient.window, dnd.text_uri_list)) {
	    long len;
	    char *data;
	    data = wxTheClipboard->GetClipboardData("text/uri-list", &len, XDND_DROP_TIME(xev), dnd.XdndSelection);
	    if (data) {
	      /* Newline-separate elements... */
	      long offset = 0;
	      while (offset < len) {
		long elem_end;
		for (elem_end = offset; 
		     (elem_end < len) && (data[elem_end] != '\r');
		     elem_end++) {
		}
		/* If file://... prefix (then drop it) */
		if ((offset + 7 <= len)
		    && !strncmp(data XFORM_OK_PLUS offset, "file://", 7)) {
		  /* Now skip the root: */
		  long i = offset + 7;
		  char *data2;
		  while ((i < elem_end) && (data[i] != '/')) {
		    i++;
		  }
		  if (i < elem_end) {
		    data2 = new WXGC_ATOMIC char[elem_end - i + 1];
		    memcpy(data2, data + i, elem_end - i);
		    data2[elem_end - i] = 0;
		    decode_percent_escapes(data2);
		    target->OnDropFile(data2);
		  }
		}
		offset = elem_end + 2; /* assume CRLF */
	      }
	    }
	  }
	}
        xdnd_send_finished(&dnd, XDND_DROP_SOURCE_WIN(xev), XtWindow(w), 0);
      } else if (xev->xclient.message_type == dnd.XdndLeave) {
	win->dndTarget = NULL;
        xdnd_send_finished(&dnd, XDND_LEAVE_SOURCE_WIN(xev), XtWindow(w), 0);
      }
    }
    break;
  case CreateNotify:
    break;
  case ConfigureNotify:
    // layout window
    win->Layout();
    // notify size and position change
    win->OnMove(xev->xconfigure.width, xev->xconfigure.height);
    win->OnSize(xev->xconfigure.width, xev->xconfigure.height);
    break;
  case UnmapNotify:
    if (wxSubType(win->__type, wxTYPE_DIALOG_BOX)) {
      /* Check for a frame in the parent hierarchy: */
      wxWindow *p;
      p = win->GetParent();
      while (p) {
	if (!wxSubType(p->__type, wxTYPE_DIALOG_BOX))
	  break;
	p = p->GetParent();
      }
      /* No parent? Can't iconize. */
      if (!p) {
	if (win->IsShown()) {
	  ((wxDialogBox *)win)->Iconize(FALSE);
	}
      }
    }
    break;
  }

#ifdef MZ_PRECISE_GC
  XFORM_RESET_VAR_STACK;
#endif
}

void wxWindow::ScrollEventHandler(Widget    WXUNUSED(w),
				  wxWindow  **winp,
				  XtPointer p_XfwfScrollInfo)
{
  XfwfScrollInfo *sinfo = (XfwfScrollInfo*)p_XfwfScrollInfo;
  wxScrollEvent *wxevent;
  int dir = 0, not_understood = 0;

  wxWindow *win = (wxWindow *)GET_SAFEREF(winp);
  if (!win) {
#ifdef MZ_PRECISE_GC
    XFORM_RESET_VAR_STACK;
#endif
    return;
  }

  wxevent = new wxScrollEvent();
  
  if (win->misc_flags & NO_AUTO_SCROLL_FLAG) {
    switch (sinfo->reason) {
    case XfwfSUp:	
      win->SetScrollPos(dir = wxVERTICAL, win->vs_pos - 1);
      break;
    case XfwfSLeft:
      win->SetScrollPos(dir = wxHORIZONTAL, win->hs_pos - 1);
      break;
    case XfwfSDown:
      win->SetScrollPos(dir = wxVERTICAL, win->vs_pos + 1);
      break;
    case XfwfSRight:
      win->SetScrollPos(dir = wxHORIZONTAL, win->hs_pos + 1);
      break;
    case XfwfSPageUp:
      win->SetScrollPos(dir = wxVERTICAL, win->vs_pos - win->vs_page);
      break;
    case XfwfSPageLeft:
      win->SetScrollPos(dir = wxHORIZONTAL, win->hs_pos - win->hs_page);
      break;
    case XfwfSPageDown:
      win->SetScrollPos(dir = wxVERTICAL, win->vs_pos + win->vs_page);
      break;
    case XfwfSPageRight:
      win->SetScrollPos(dir = wxHORIZONTAL, win->hs_pos + win->hs_page);
      break;
    case XfwfSTop:
    case XfwfSBottom:
      dir = wxVERTICAL;
      break;
    case XfwfSLeftSide:
    case XfwfSRightSide:
      dir = wxHORIZONTAL;
      break;
    case XfwfSDrag:
      { 
	double x, y;
	xws_get_scroll_pos(win->X->scroll, &x, &y);
	win->wxWindow::Scroll((int)(win->hs_width * x), (int)(win->vs_width * y));
	if (sinfo->flags & XFWF_VPOS)
	  dir = wxVERTICAL;
	else
	  dir = wxHORIZONTAL;
      }
      break;
    default:
      not_understood = 1;
      break;
    }
    {
      int pos;
      pos = win->GetScrollPos(dir);
      wxevent->pos = pos;
    }
  } else {
    // sinfo->gx and sinfo->gy are set by the ScrolledWindow widget
    XtMoveWidget(win->X->handle, sinfo->gx, sinfo->gy);
    win->Refresh();
  }
  
  if (win->misc_flags & NO_AUTO_SCROLL_FLAG) {
    wxevent->eventHandle = (char*)p_XfwfScrollInfo;
    wxevent->direction = dir;
    switch (sinfo->reason) {
    case XfwfSUp:
    case XfwfSLeft:	wxevent->moveType = wxEVENT_TYPE_SCROLL_LINEUP;
      break;
    case XfwfSDown:
    case XfwfSRight:	wxevent->moveType = wxEVENT_TYPE_SCROLL_LINEDOWN;
      break;
    case XfwfSPageUp:
    case XfwfSPageLeft:	wxevent->moveType = wxEVENT_TYPE_SCROLL_PAGEUP;
      break;
    case XfwfSPageDown:
    case XfwfSPageRight:wxevent->moveType = wxEVENT_TYPE_SCROLL_PAGEDOWN;
      break;
    case XfwfSTop:
    case XfwfSLeftSide:	wxevent->moveType = wxEVENT_TYPE_SCROLL_TOP;
      break;
    case XfwfSBottom:
    case XfwfSRightSide:wxevent->moveType = wxEVENT_TYPE_SCROLL_BOTTOM;
      break;
    case XfwfSDrag:	wxevent->moveType = wxEVENT_TYPE_SCROLL_THUMBTRACK;
    default:
      break;
    }

    if (!not_understood)
      win->OnScroll(wxevent);

    wxevent->eventHandle = NULL;
  }

#ifdef MZ_PRECISE_GC
  XFORM_RESET_VAR_STACK;
#endif
}


static void AdjustMousePosition(Window xevwin, Widget handle, wxWindow *win, wxMouseEvent *wxevent)
{
  if (xevwin != XtWindow(handle)) {
    Widget wgt;
    wgt = XtWindowToWidget(XtDisplay(handle), xevwin);
    if (wgt) {
      Position cx, cy, wx, wy;
      XtTranslateCoords(wgt, 0, 0, &cx, &cy);
      XtTranslateCoords(handle, 0, 0, &wx, &wy);
      wxevent->x += (cx - wx);
      wxevent->y += (cy - wy);
    }
  } else if (wxSubType(win->__type, wxTYPE_CANVAS)) {
    /* Reverse scroll effects: */
    int dx, dy;
    ((wxCanvas *)win)->ViewStart(&dx, &dy);
    wxevent->x -= dx;
    wxevent->y -= dy;
  }
}

extern Bool wxIsAlt(KeySym key_sym);

/* Used for XLookupString */
static XComposeStatus compose_status;
#ifndef NO_XMB_LOOKUP_STRING
# define XMB_KC_STATUS(status) ((status == XLookupKeySym) || (status == XLookupBoth))
# define XMB_STR_STATUS(status) ((status == XLookupChars) || (status == XLookupBoth))
# define XMB_STR_PREFERRED_STATUS(status, xev) ((status == XLookupChars) || ((status == XLookupBoth) && !(xev->xkey.state & ControlMask)))
# define DEFAULT_XMB_STATUS XLookupKeySym
# ifdef X_HAVE_UTF8_STRING
#  define X___LookupString Xutf8LookupString
# else
#  define X___LookupString XmbLookupString
# endif
#else
# define XMB_KC_STATUS(status) (status)
# define XMB_STR_STATUS(status) 0
# define XMB_STR_PREFERRED_STATUS(status, xev) 0
# define DEFAULT_XMB_STATUS 1
#endif

static XModifierKeymap *xmodkeymap;

static int extract_string_key(char *str, int slen)
{
  if (slen > 9)
    slen = 9;
  str[slen] = 0;
#ifdef X_HAVE_UTF8_STRING
  return wxUTF8StringToChar(str, slen);
#else
  return wxLocaleStringToChar(str, slen);
#endif
}

Status wxWindow::LookupKey(int unshifted, int unalted, int caps_mode,
                           Widget w, wxWindow *win, XEvent *xev, KeySym *_keysym, char *str, int *_len)
{
  KeySym keysym;
  Status status;
  int len;
  XKeyPressedEvent evt;

  memcpy(&evt, &(xev->xkey), sizeof(XKeyPressedEvent));

  if ((evt.state & ControlMask) && !(evt.state & Mod1Mask)) {
    /* Control (and not AltGr) => cancel Caps Lock */
    evt.state -= (evt.state & LockMask);
  }

  if (unshifted) {
    if (evt.state & ShiftMask)
      evt.state -= ShiftMask;
    else
      evt.state |= ShiftMask;
  }
  if (unalted) {
    if (!(evt.state & Mod1Mask) == !(evt.state & ControlMask)) {
      if (evt.state & Mod1Mask)
        evt.state -= Mod1Mask;
      else
        evt.state |= Mod1Mask;
      if (evt.state & ControlMask)
        evt.state -= ControlMask;
      else
        evt.state |= ControlMask;
    }
  }
  if (caps_mode != 1) {
    if (evt.state & LockMask)
      evt.state -= LockMask;
    else if (caps_mode == 2) {
      evt.state |= LockMask;
    }
  }
    
#ifndef NO_XMB_LOOKUP_STRING
  if (!the_im) {
    the_im = XOpenIM(wxAPP_DISPLAY, NULL, NULL, NULL);
  }
  if (the_im) {
    if (!win->X->ic) {
      win->X->ic = XCreateIC(the_im, 
			     XNInputStyle, XIMPreeditNothing | XIMStatusNothing,
			     NULL);
      win->X->us_ic = XCreateIC(the_im, 
				XNInputStyle, XIMPreeditNothing | XIMStatusNothing,
				NULL);
    }
  }

  if (win->X->ic && (xev->xany.type == KeyPress)) {
    XIC ic;
    ic = unshifted ? win->X->ic : win->X->ic;
    XSetICValues(ic, 
		 XNClientWindow, XtWindow(w),
		 XNFocusWindow,  XtWindow(w),
		 NULL);
    XSetICFocus(ic);

    len = X___LookupString(ic, &evt, str, 10, &keysym, &status);
  } else
#endif
    {
      (void)XLookupString(&evt, str, 10, &keysym, &compose_status);
      status = DEFAULT_XMB_STATUS;
      len = 0;
    }

  *_len = len;
  *_keysym = keysym;
  return status;
}

static int status_to_kc(Status status, XEvent *xev, KeySym keysym, char *str, int slen)
{
  if (XMB_STR_PREFERRED_STATUS(status, xev))
    return extract_string_key(str, slen);
  else if (XMB_KC_STATUS(status))
    return CharCodeXToWX(keysym);
  else if (XMB_STR_STATUS(status))
    return extract_string_key(str, slen);
  else 
    return 0;
}

void wxWindow::WindowEventHandler(Widget w,
				  wxWindow **winp,
				  XEvent *xev,
				  Boolean *continue_to_dispatch_return)
{
  Bool subWin;
  wxWindow *win = (wxWindow *)GET_SAFEREF(winp);
  Bool Enter=FALSE, Press=FALSE;

  if (!win) {
    *continue_to_dispatch_return = FALSE;
#ifdef MZ_PRECISE_GC
    XFORM_RESET_VAR_STACK;
#endif
    return;
  }

  if (XFilterEvent(xev, None)) {
    win->ReleaseMouse();
    *continue_to_dispatch_return = FALSE;
#ifdef MZ_PRECISE_GC
    XFORM_RESET_VAR_STACK;
#endif
    return;
  }

  subWin = (w != win->X->handle) && (w != win->X->frame);

    switch (xev->xany.type) {
    case KeyRelease:
        {
	  KeySym	   keysym;

	  *continue_to_dispatch_return = FALSE;
	  if (win->misc_flags & LAST_WAS_ALT_DOWN_FLAG) {
	    win->misc_flags -= LAST_WAS_ALT_DOWN_FLAG;

	    (void)XLookupString(&(xev->xkey), NULL, 0, &keysym, NULL);
	    if (wxIsAlt(keysym)) {
	      /* Find frame. */
	      wxWindow *p = win;
	      while (p) {
		if (wxSubType(p->__type, wxTYPE_FRAME)) {
		  wxMenuBar *mb;
		  mb = ((wxFrame *)p)->GetMenuBar();
		  if (mb) {
		    ((wxFrame *)p)->OnMenuClick();
		    mb->SelectAMenu();
		  }
		  break;
		}
		p = p->GetParent();
	      }
	    }
	    win->current_state = xev->xkey.state;
	    break;
	  }
	}
    case KeyPress: 
      win->current_state = xev->xkey.state;
      { /* ^^^ fallthrough !!!! ^^^ */
	wxKeyEvent *wxevent;
	KeySym	   keysym, other_keysym, alt_keysym, other_alt_keysym, caps_keysym;
	long       kc, other_kc, alt_kc, other_alt_kc, caps_kc;
	Status     status, other_status, alt_status, other_alt_status, caps_status;
	char       str[10], other_str[10], alt_str[10], other_alt_str[10], caps_str[10];
	int        slen, other_slen, alt_slen, other_alt_slen, caps_slen;

	wxevent = new wxKeyEvent(wxEVENT_TYPE_CHAR);

	status = LookupKey(0, 0, 1, w, win, xev, &keysym, str, &slen);
	other_status = LookupKey(1, 0, 0, w, win, xev, &other_keysym, other_str, &other_slen);
	alt_status = LookupKey(0, 1, 0, w, win, xev, &alt_keysym, alt_str, &alt_slen);
	other_alt_status = LookupKey(1, 1, 0, w, win, xev, &other_alt_keysym, other_alt_str, &other_alt_slen);
	caps_status = LookupKey(0, 0, 2, w, win, xev, &caps_keysym, caps_str, &caps_slen);

	if (xev->xany.type == KeyPress) {
	  static int handle_alt = 0;

	  if (!handle_alt) {
	    if (!wxGetBoolPreference("altUpSelectsMenu", &handle_alt))
	      handle_alt = 0;
	    handle_alt = !handle_alt ? -1 : 1;
	  }

	  if (handle_alt > 0) {
	    if (win->misc_flags & LAST_WAS_ALT_DOWN_FLAG)
	      win->misc_flags -= LAST_WAS_ALT_DOWN_FLAG;
	    else if (wxIsAlt(keysym) && !(xev->xkey.state & (ShiftMask | ControlMask)))
	      win->misc_flags |= LAST_WAS_ALT_DOWN_FLAG;
	  }
	}

        kc = status_to_kc(status, xev, keysym, str, slen);
        other_kc = status_to_kc(other_status, xev, other_keysym, other_str, other_slen);
        alt_kc = status_to_kc(alt_status, xev, alt_keysym, alt_str, alt_slen);
        other_alt_kc = status_to_kc(other_alt_status, xev, other_alt_keysym, other_alt_str, other_alt_slen);
        caps_kc = status_to_kc(caps_status, xev, caps_keysym, caps_str, caps_slen);

        /* Figure out key state *after* event: */
        {
          int i, j;
          if (!xmodkeymap)
            xmodkeymap = XGetModifierMapping(wxAPP_DISPLAY);
          for (i = 0; i < 8; i++) {
            for (j = 0; j < xmodkeymap->max_keypermod; j++) {
              if (xev->xkey.keycode == xmodkeymap->modifiermap[(i * xmodkeymap->max_keypermod) + j]) {
                if (xev->xany.type == KeyPress)
                  win->current_state |= (1 << i);
                else
                  win->current_state -= (1 << i);
              }
            }
          }
        }

	// set wxWindows event structure
	wxevent->eventHandle	= (char*)xev;
	wxevent->keyCode	= (xev->xany.type == KeyPress) ? kc : WXK_RELEASE;
	wxevent->keyUpCode	= (xev->xany.type == KeyRelease) ? kc : WXK_PRESS;
	wxevent->otherKeyCode	= other_kc;
	wxevent->altKeyCode	= alt_kc;
	wxevent->otherAltKeyCode = other_alt_kc;
	wxevent->capsKeyCode    = caps_kc;
	wxevent->x		= xev->xkey.x;
	wxevent->y		= xev->xkey.y;
	wxevent->altDown	= /* xev->xkey.state & Mod3Mask */ FALSE;
	wxevent->controlDown	= xev->xkey.state & ControlMask;
	wxevent->metaDown	= xev->xkey.state & Mod1Mask;
	wxevent->shiftDown	= xev->xkey.state & ShiftMask;
	wxevent->capsDown	= xev->xkey.state & LockMask;
	wxevent->timeStamp      = xev->xkey.time;

	/* Reverse scroll effects: */
	if (wxSubType(win->__type, wxTYPE_CANVAS)) {
	  int dx, dy;
	  ((wxCanvas *)win)->ViewStart(&dx, &dy);
	  wxevent->x -= dx;
	  wxevent->y -= dy;
	}

	*continue_to_dispatch_return = FALSE;
	if (!win->CallPreOnChar(win, wxevent)) {
	  /* hack: ignore SubWin for a choice item key event: */
	  if (subWin && (win->__type == wxTYPE_CHOICE))
	    subWin = 0;

	  if (subWin)
	    *continue_to_dispatch_return = TRUE;
	  else {
	    /* Double-check that pre-on-char didn't disable: */
	    if (!win->IsGray())
	      win->OnChar(wxevent);
	  }
	}
	wxevent->eventHandle = NULL;
        /* Event was handled by OnFunctionKey and/or OnChar */ }
	break;
    case ButtonPress:
      /* X grab doesn't work the way we'd like for panels (since they
	 have children), unless a grab cursor is installed.
	 Unfortunately, we also need to watch for changes to the
	 cursor via SetCursor(). */
      if (win->__type == wxTYPE_PANEL) {
	wxWindow *p = win;
	while (p) {
	  if (p->cursor)
	    break;
	  if (wxSubType(p->__type, wxTYPE_FRAME)
	      || wxSubType(p->__type, wxTYPE_DIALOG_BOX))
	    p = NULL;
	  else
	    p = p->GetParent();
	}
	
	if (p && p->cursor->Ok()) {
	  Cursor c;
	  c = GETCURSOR(p->cursor);
	  XChangeActivePointerGrab(wxAPP_DISPLAY, 
				   (ButtonPressMask | ButtonReleaseMask
				    | ButtonMotionMask | PointerMotionMask | PointerMotionHintMask
				    | EnterWindowMask | LeaveWindowMask),
				   c, 
				   xev->xbutton.time);
	  if (!grabbing_panel_regsitered) {
	    wxREGGLOB(grabbing_panel);
	    grabbing_panel_regsitered = 1;
	  }

	  grabbing_panel = win;
	  grabbing_panel_time = xev->xbutton.time;
	}
      }
      Press = TRUE;
    case ButtonRelease:  /* ^^^^ fallthrough */
      win->current_state = xev->xbutton.state;
      if (!Press)
	grabbing_panel = NULL;
      if (win->misc_flags & LAST_WAS_ALT_DOWN_FLAG)
	win->misc_flags -= LAST_WAS_ALT_DOWN_FLAG;
      if ((xev->xbutton.button == Button4)
	  || (xev->xbutton.button == Button5)) {
	/* Button4 and Button5 seem to be mapped to wheel up and down, now: */
	if (Press) {
	  wxKeyEvent *wxevent;

	  wxevent = new wxKeyEvent(wxEVENT_TYPE_CHAR);

	  wxevent->eventHandle	= NULL;
	  wxevent->keyCode	= ((xev->xbutton.button == Button5) 
				   ? WXK_WHEEL_DOWN 
				   : WXK_WHEEL_UP);
	  wxevent->x		= xev->xbutton.x;
	  wxevent->y		= xev->xbutton.y;
	  wxevent->altDown	= FALSE;
	  wxevent->controlDown	= xev->xbutton.state & ControlMask;
	  wxevent->metaDown	= xev->xbutton.state & Mod1Mask;
	  wxevent->shiftDown	= xev->xbutton.state & ShiftMask;
	  wxevent->capsDown	= xev->xbutton.state & LockMask;
	  wxevent->timeStamp    = xev->xbutton.time;

	  *continue_to_dispatch_return = FALSE;
	  if (!win->CallPreOnChar(win, wxevent)) {
	    if (subWin && (win->__type == wxTYPE_CHOICE))
	      subWin = 0;
	    
	    if (subWin)
	      *continue_to_dispatch_return = TRUE;
	    else {
	      /* Double-check that pre-on-char didn't disable: */
	      if (!win->IsGray())
		win->OnChar(wxevent);
	    }
	  }	  
	}
        
        switch (xev->xbutton.button) {
	case Button4: 
	  if (Press)
	    win->current_state |= Button4Mask;
	  else
	    win->current_state -= Button4Mask;
	  break;
	case Button5: 
	  if (Press)
	    win->current_state |= Button5Mask;
	  else
	    win->current_state -= Button5Mask;
	  break;
	}
      } else {
        wxMouseEvent *wxevent;

	wxevent = new wxMouseEvent;
	
	switch (xev->xbutton.button) {
	case Button1: 
	  wxevent->eventType = wxEVENT_TYPE_LEFT;
	  if (Press)
	    win->current_state |= Button1Mask;
	  else
	    win->current_state -= Button1Mask;
	  break;
	case Button2: 
	  wxevent->eventType = wxEVENT_TYPE_MIDDLE; 
	  if (Press)
	    win->current_state |= Button2Mask;
	  else
	    win->current_state -= Button2Mask;
	  break;
	case Button3: 
	  wxevent->eventType = wxEVENT_TYPE_RIGHT;
	  if (Press)
	    win->current_state |= Button3Mask;
	  else
	    win->current_state -= Button3Mask;
	  break;
	}
	if (Press) {
	  // button is down
	  wxevent->eventType |= wxEVENT_TYPE_DOWN;
	  if (win->allow_dclicks) { // doubleclick handling wanted?
	    if (xev->xbutton.button == win->X->last_clickbutton
		&&  (xev->xbutton.time - win->X->last_clicktime
		     <= (unsigned int)XtGetMultiClickTime(wxAPP_DISPLAY))) {
	      // double click has arrived
	      wxevent->eventType |= wxEVENT_TYPE_DOUBLE;
	      win->X->last_clicktime = 0; 
	    } else {
	      // single click has arrived
	      win->X->last_clickbutton = xev->xbutton.button;
	      win->X->last_clicktime   = xev->xbutton.time;
	    }
	  }
	}
	// set wxWindows event structure
	wxevent->eventHandle	= (char*)xev;
	wxevent->x		= xev->xbutton.x;
	wxevent->y		= xev->xbutton.y;
	wxevent->altDown		= /* xev->xbutton.state & Mod3Mask */ FALSE;
	wxevent->controlDown	= xev->xbutton.state & ControlMask;
	wxevent->metaDown	= xev->xbutton.state & Mod1Mask;
	wxevent->shiftDown	= xev->xbutton.state & ShiftMask;
	wxevent->capsDown	= xev->xbutton.state & LockMask;
	wxevent->leftDown	= ((wxevent->eventType == wxEVENT_TYPE_LEFT_DOWN)
				   || (xev->xbutton.state & Button1Mask));
	wxevent->middleDown	= ((wxevent->eventType == wxEVENT_TYPE_MIDDLE_DOWN)
				   || (xev->xbutton.state & Button2Mask));
	wxevent->rightDown	= ((wxevent->eventType == wxEVENT_TYPE_RIGHT_DOWN)
				   || (xev->xbutton.state & Button3Mask));
	wxevent->timeStamp       = xev->xbutton.time;

	/* Adjust location of mouse-moved events when it's
	   over sub-parts, and counter canvas scroll: */
	AdjustMousePosition(xev->xbutton.window, win->X->handle, win, wxevent);

	*continue_to_dispatch_return = FALSE;
	if (!win->CallPreOnEvent(win, wxevent)) {
	  if (subWin) {
	    *continue_to_dispatch_return = TRUE;
	  } else {
	    if (Press) {
	      if (wxSubType(win->__type, wxTYPE_MENU_BAR)) {
		if (!((wxMenuBar *)win)->InProgress()) {
		  wxFrame *f;
		  f = (wxFrame *)(win->GetParent());
		  f->OnMenuClick();
		}
	      } else {
                if (win->WantsFocus())
                  win->SetFocus();
	      }
	    }

	    /* It's possible that the window has become disabled... */
	    if (!win->IsGray())
	      win->OnEvent(wxevent);
	  }
	}
	wxevent->eventHandle = NULL; /* MATTHEW: [5] */
      }
      break;
    case EnterNotify:
      Enter = TRUE;
    case LeaveNotify: /* ^^^^ fallthrough! */
      win->current_state = xev->xcrossing.state;
      if (win->misc_flags & LAST_WAS_ALT_DOWN_FLAG)
	win->misc_flags -= LAST_WAS_ALT_DOWN_FLAG;
      if (w == win->X->frame) {
	/* If Focus == PointerRoot, manage activation */
	if (xev->xcrossing.detail != NotifyInferior) {
	  Window current;
	  int old_revert;
	  XGetInputFocus(XtDisplay(win->X->frame), &current, &old_revert);
	  if (current == PointerRoot) {
	    if (Enter)
	      win->misc_flags |= ACTIVE_VIA_POINTER_FLAG;
	    else
	      win->misc_flags -= (win->misc_flags & ACTIVE_VIA_POINTER_FLAG);
	    win->OnActivate(Enter);
	  }
	}
      } else {
        wxMouseEvent *wxevent;

	wxevent = new wxMouseEvent(Enter 
				   ? wxEVENT_TYPE_ENTER_WINDOW 
				   : wxEVENT_TYPE_LEAVE_WINDOW);

	// set wxWindows event structure
	wxevent->eventHandle	= (char*)xev;
	wxevent->x		= xev->xcrossing.x;
	wxevent->y		= xev->xcrossing.y;
	wxevent->altDown		= /* xev->xcrossing.state & Mod3Mask */ FALSE;
	wxevent->controlDown	= xev->xcrossing.state & ControlMask;
	wxevent->metaDown	= xev->xcrossing.state & Mod1Mask;
	wxevent->shiftDown	= xev->xcrossing.state & ShiftMask;
	wxevent->capsDown	= xev->xcrossing.state & LockMask;
	wxevent->leftDown	= xev->xcrossing.state & Button1Mask;
	wxevent->middleDown	= xev->xcrossing.state & Button2Mask;
	wxevent->rightDown	= xev->xcrossing.state & Button3Mask;
	wxevent->timeStamp       = xev->xbutton.time; /* MATTHEW */
	*continue_to_dispatch_return = FALSE; /* Event was handled by OnEvent */ 

	/* Reverse scroll effects: */
	if (wxSubType(win->__type, wxTYPE_CANVAS)) {
	  int dx, dy;
	  ((wxCanvas *)win)->ViewStart(&dx, &dy);
	  wxevent->x -= dx;
	  wxevent->y -= dy;
	}

	if (!win->CallPreOnEvent(win, wxevent)) {
	  if (!win->IsGray())
	    win->OnEvent(wxevent);
	}
	wxevent->eventHandle = NULL; /* MATTHEW: [5] */
      }
      break;
    case MotionNotify: 
      {
	wxMouseEvent *wxevent;
	int skip = 0;

	wxevent = new wxMouseEvent(wxEVENT_TYPE_MOTION);

	if (xev->xmotion.is_hint == NotifyHint) {
	  // hints need a XQueryPointer
	  Window root, child;
	  XQueryPointer(XtDisplay(w), XtWindow(w), &root, &child,
			&(xev->xmotion.x_root), &(xev->xmotion.y_root),
			&(xev->xmotion.x),      &(xev->xmotion.y),
			&(xev->xmotion.state));
	  if (xev->xmotion.state != win->current_state)
	    skip = 1;
	} else
	  win->current_state = xev->xmotion.state;

	if (!skip) {
	  // set wxWindows event structure
	  wxevent->eventHandle	= (char*)xev;
	  wxevent->x		= xev->xmotion.x;
	  wxevent->y		= xev->xmotion.y;
	  wxevent->altDown		= /* xev->xmotion.state & Mod3Mask */ FALSE;
	  wxevent->controlDown	= xev->xmotion.state & ControlMask;
	  wxevent->metaDown	= xev->xmotion.state & Mod1Mask;
	  wxevent->shiftDown	= xev->xmotion.state & ShiftMask;
	  wxevent->capsDown	= xev->xmotion.state & LockMask;
	  wxevent->leftDown	= xev->xmotion.state & Button1Mask;
	  wxevent->middleDown	= xev->xmotion.state & Button2Mask;
	  wxevent->rightDown	= xev->xmotion.state & Button3Mask;
	  wxevent->timeStamp       = xev->xbutton.time;
	  *continue_to_dispatch_return = FALSE; /* Event was handled by OnEvent */

	  /* Reverse scroll effects: */
	  AdjustMousePosition(xev->xbutton.window, win->X->handle, win, wxevent);

	  if (!win->CallPreOnEvent(win, wxevent)) {
	    if (subWin)
	      *continue_to_dispatch_return = TRUE;
	    else {
	      if (!win->IsGray())
		win->OnEvent(wxevent);
	    }
	  }
	  wxevent->eventHandle = NULL;
	}
      }
      break;
	/* Use focus in/out for OnActivate */
    case FocusIn:
        Enter = TRUE;
    case FocusOut:
      if (win->misc_flags & LAST_WAS_ALT_DOWN_FLAG)
	win->misc_flags -= LAST_WAS_ALT_DOWN_FLAG;
      if (xev->xfocus.detail != NotifyInferior) {
	Window current;
	if (xev->xfocus.detail == NotifyPointer) {
	  /* NotifyPointer is meaningful if the focus is PointerRoot
	     or we're active via the pointer */
	  if (!Enter && (win->misc_flags & ACTIVE_VIA_POINTER_FLAG)) {
	    current = PointerRoot;
	  } else {
	    int old_revert;
	    XGetInputFocus(XtDisplay(win->X->frame), &current, &old_revert);
	  }
	} else
	  current = PointerRoot;

	if (current == PointerRoot) {
	  if (xev->xfocus.detail == NotifyPointer) {
	    if (Enter)
	      win->misc_flags |= ACTIVE_VIA_POINTER_FLAG;
	    else
	      win->misc_flags -= (win->misc_flags & ACTIVE_VIA_POINTER_FLAG);
	  }
	  win->OnActivate(Enter);
	}
      }
      break;
    case Expose: // arrives for non-xfwfCommonWidget-subclasses only
	if (win->dc && win->painting_enabled) { // expose only if DC available
	    // setup drawable of dc if dc available
	    if (!(win->dc->ok)) { // first expose call
	      win->dc->X->drawable = XtWindow(win->X->handle);
	      win->dc->X->draw_window = win->dc->X->drawable;
	      win->dc->SetBackground(win->dc->current_background_color);
	      win->dc->Clear();
	      win->dc->ok = TRUE;
	    }
	    // call refresh method
	    win->Paint();
	}
        break;
    }

#ifdef MZ_PRECISE_GC
  XFORM_RESET_VAR_STACK;
#endif
}

Bool wxWindow::WantsFocus(void)
{
  return TRUE;
}

//-----------------------------------------------------------------------------
// create and destroy associated device context
//-----------------------------------------------------------------------------

void wxWindow::CreateDC(void)
{
    wxWindowDC_Xinit *init;

    if (dc) return; // only create once!

    dc = DEBUG_NEW wxWindowDC;
    // Initialize wxWindowDC
    init = new wxWindowDC_Xinit;
    init->dpy      = wxAPP_DISPLAY; // display is global to application
    init->scn      = wxAPP_SCREEN;  //  screen is global to application
    init->owner    = this;
    init->drawable = XtWindow(X->handle);
    dc->ok = TRUE;
    
    dc->Initialize(init);

    dc->X->is_window = TRUE;
}

void wxWindow::DestroyDC(void)
{
    if (!dc) return; // no DC to destroy
    // destroy device context
    DELETE_OBJ dc;
    dc = NULL;
}

wxWindowDC *wxWindow::GetDC(void)
{ 
  if (!dc) {
    if (!(style & wxNO_DC))
      CreateDC();
  }
  
  return dc; 
}

void wxWindow::GetTextExtent(const char *s, double *w, double *h, double *descent,
			     double *ext_leading, wxFont *theFont,
			     Bool use16bit)
{
  if (dc) {
    dc->GetTextExtent(s, w, h, descent, ext_leading, theFont, use16bit);
    return;
  }
  
  if (!theFont) theFont = font;
  
  wxGetTextExtent(wxAPP_DISPLAY, 1.0, 1.0,
		  s, w, h, descent, ext_leading, theFont,
		  1, use16bit, 0, -1);
}


void wxWindow::ForEach(void (*foreach)(wxWindow *w, void *data), void *data)
{
  wxChildNode *node, *next;

#ifdef MZ_PRECISE_GC
  if (__type == wxTYPE_MENU_BAR)
    return;
#endif

  for (node = children->First(); node; node = next) {
    wxWindow *child;
    next = node->Next();
    child = (wxWindow*)(node->Data());
    if (child) {
      child->ForEach(foreach, data);
    }
  }

  foreach(this, data);
}

long wxWindow::GetWindowHandle()
{
  return (long)X->handle;
}

//-----------------------------------------------------------------------------
// drag & drop
//-----------------------------------------------------------------------------

void wxWindow::DragAcceptFiles(Bool accept)
{
  wxWindow *p;

  if (!drag_accept == !accept)
    return;

  drag_accept = accept;
  
  if (!dnd_inited) {
    xdnd_init(&dnd, wxAPP_DISPLAY);
    dnd_inited = 1;
  }

  /* Declare drag-and-drop possible at this
     window's top-level frame: */

  p = this;
  while (p) {
    if (wxSubType(p->__type, wxTYPE_FRAME)
	|| wxSubType(p->__type, wxTYPE_DIALOG_BOX))
      break;
    p = p->GetParent();
  }
  
  {
    Atom l[2];
    l[0] = dnd.text_uri_list;
    l[1] = 0;
    xdnd_set_dnd_aware(&dnd, XtWindow(p->X->frame), l);
  }
}

wxWindow *wxWindow::FindChildByWidget(Widget w)
{
  wxChildNode *node, *next;
  wxWindow *r;

  if (X) {
    if ((w == X->frame)
        || (w == X->handle))
      return this;
  }

  if (children) {
    for (node = children->First(); node; node = next) {
      wxWindow *child;
      next = node->Next();
      child = (wxWindow*)(node->Data());
      if (child) {
        r = child->FindChildByWidget(w);
        if (r)
          return r;
      }
    }
  }

  return NULL;
}

static void parse_and_drop_runtime(int len, char *s)
{
  char **argv, *a;
  int cnt = 0, pos = 0;
  int sz;

  while (pos < len) {
    sz = 0;
    while ((pos < len) && (s[pos] != ':')) {
      sz = (sz * 10) + (s[pos] - '0');
      pos++;
    }
    pos++;
    if (sz > 0)
      pos += sz;
    cnt++;
  }
  
  argv = new WXGC_PTRS char*[cnt];
  
  pos = cnt = 0;
  while (pos < len) {
    sz = 0;
    while ((pos < len) && (s[pos] != ':')) {
      sz = (sz * 10) + (s[pos] - '0');
      pos++;
    }
    pos++;

    if (sz > len - pos)
      sz = len - pos;
    if (sz < 0)
      sz = 0;
    a = new WXGC_ATOMIC char[sz + 1];
    memcpy(a, s + pos, sz);
    a[sz] = 0;
    argv[cnt] = a;
    
    if (sz > 0)
      pos += sz;
    cnt++;
  }
  
  wxDrop_Runtime(argv, cnt);
}

static int ishexdig(char s) { 
  return (((s >= '0') && (s <= '9'))
	  || ((s >= 'a') && (s <= 'f'))
	  || ((s >= 'A') && (s <= 'F'))); 
}
static int hexval(char s) { 
  return (((s >= '0') && (s <= '9'))
	  ? s - '0'
	  : (((s >= 'a') && (s <= 'f'))
	     ? s - 'a' + 10
	     : s - 'A' + 10));
}

static void decode_percent_escapes(char *s)
{
  int src = 0, dest = 0;
  while (s[src]) {
    if ((s[src] == '%')
	&& ishexdig(s[src+1])
	&& ishexdig(s[src+2])) {
      int v;
      v = ((hexval(s[src+1]) << 4) + hexval(s[src+2]));
      s[dest++] = v;
      src += 3;
    } else {
      s[dest++] = s[src++];
    }
  }
  s[dest] = 0;
}
