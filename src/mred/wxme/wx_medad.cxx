/*
 * File:        wx_medad.cc
 * Purpose:     wxMediaCanvas & wxDrawableMediaAdmin implementation
 * Author:      Matthew Flatt
 * Created:     1995
 * Copyright:   (c) 2004-2006 PLT Scheme Inc.
 * Copyright:   (c) 1995, Matthew Flatt
 */

#if defined(_MSC_VER) && defined(MZ_PRECISE_GC)
# include "wx.h"
#endif
#include "common.h"

#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "wx_canvs.h"
#include "wx_dcmem.h"
#include "wx_media.h"
#include "wx_types.h"
#include "wx_gcrct.h"
#include "wx_ptreq.h"
#include "wx_timer.h"
#include "wx_main.h"

static wxMemoryDC *wx_canvasless_offscreen;

extern void *MrEdGetWindowContext(wxWindow *w);

class SimpleScroll
{
  Bool horizontal;

  int count, pageStep, value;

 public:
  SimpleScroll(wxMediaCanvas *, long style = 0,
	      int length = 0, int stepsPerPage = 4,
	      int position = 0);
  ~SimpleScroll();

  void SetValue(int);
  int GetValue(void);
  void SetScroll(int len = -1, int page = -1, int position = -1);  
};

class wxUpdateCursorTimer : public wxTimer 
{
  wxCanvasMediaAdmin *admin;
 public:
  wxUpdateCursorTimer(wxCanvasMediaAdmin *a);
  void Notify(void);
  void Cancel();
};

wxUpdateCursorTimer::wxUpdateCursorTimer(wxCanvasMediaAdmin *a)
  : wxTimer(MrEdGetWindowContext(a->canvas))
{
  admin = a;
  Start(0, TRUE);
}

void wxUpdateCursorTimer::Notify(void) {
  Stop();
  if (admin) {
    admin->updateCursorTimer = NULL;
    admin->canvas->UpdateCursorNow();
  }
}

void wxUpdateCursorTimer::Cancel() {
  admin = NULL;
}

#define BLINK_DELAY 500

class wxBlinkTimer : public wxTimer 
{
  wxMediaCanvas *canvas;
 public:
  wxBlinkTimer(wxMediaCanvas *c);
  void Notify(void);
  void Kill();
};

wxBlinkTimer::wxBlinkTimer(wxMediaCanvas *c) {
  canvas = c;
  SetContext(MrEdGetWindowContext(c));
}

void wxBlinkTimer::Notify(void) {
  /* Used to try to avoid starving other events, but yielding 
     has its own problems. In particular, it messes up dialogs
     that expect show #f to immediately lead to a return from
     show #t. */
  // wxYield();

  if (canvas)
    canvas->BlinkCaret();
}

void wxBlinkTimer::Kill() {
  canvas = NULL;
  Stop();
}

#define AUTO_DRAG_DELAY 100

class wxAutoDragTimer : public wxTimer 
{
  wxMediaCanvas *canvas;
  wxMouseEvent *event;
 public:
  wxAutoDragTimer(wxMediaCanvas *c, wxMouseEvent *e);
  void Notify(void);
  void Kill(void);
};

wxAutoDragTimer::wxAutoDragTimer(wxMediaCanvas *c, wxMouseEvent *e) {
  canvas = c;
  SetContext(MrEdGetWindowContext(c));
  event = new WXGC_PTRS wxMouseEvent(0);
  memcpy(event, e, sizeof(wxMouseEvent));
  Start(AUTO_DRAG_DELAY, TRUE);
}

void wxAutoDragTimer::Notify(void) {
  /* See note above about wxYield */
  // wxYield(); /* In case we get too much time */

  if (canvas) {
    event->timeStamp += AUTO_DRAG_DELAY;
    canvas->OnEvent(event);
  }
}

void wxAutoDragTimer::Kill(void) {
  canvas = NULL;
  Stop();
}

/************************************************************************/

static int default_wheel_amt;

#ifndef wxOVERRIDE_KEY_TRANSLATIONS
#define wxOVERRIDE_KEY_TRANSLATIONS 0
#endif

#define INIT_SB ((style & (wxMCANVAS_NO_H_SCROLL | wxMCANVAS_HIDE_H_SCROLL)) ? 0 : wxHSCROLL) \
                + ((style & (wxMCANVAS_NO_V_SCROLL | wxMCANVAS_HIDE_V_SCROLL)) ? 0 : wxVSCROLL)

#ifdef wx_x
# define wxmeBORDER wxBORDER
# define wxCONTROL_BORDER 0
#else
# define wxmeBORDER wxBORDER
#endif
#ifndef wx_mac
# define wxRESIZE_CORNER 0
#endif

wxMediaCanvas::wxMediaCanvas(wxWindow *parent,
			     int x, int y,
			     int width, int height,
			     char *name,
			     long style,
			     int scrollsPP,
			     wxMediaBuffer *m,
			     wxGLConfig *gl_cfg)
: wxCanvas(parent, x, y, width, height,
	   (((style & wxBORDER) ? wxmeBORDER : 0)
	    + wxOVERRIDE_KEY_TRANSLATIONS 
	    + INIT_SB 
	    + (style & wxINVISIBLE ? wxINVISIBLE : 0)
	    + (style & wxTRANSPARENT_WIN ? wxTRANSPARENT_WIN : 0)
	    + (style & wxCONTROL_BORDER ? wxCONTROL_BORDER : 0)
	    + (style & wxCOMBO_SIDE ? wxCOMBO_SIDE : 0)
	    + (style & wxRESIZE_CORNER ? wxRESIZE_CORNER : 0)), 
	   name, gl_cfg)
{
  static int type_added = FALSE;

#if USE_OLD_TYPE_SYSTEM
  if (!type_added) {
    wxAllTypes->AddType(wxTYPE_MEDIA_CANVAS, wxTYPE_CANVAS, "media-canvas");
    type_added = TRUE;
  }

  __type = wxTYPE_MEDIA_CANVAS;
#endif

  givenHScrollsPerPage = scrollsPP;

  xmargin = 5;
  ymargin = 5;

  allowXScroll = !(style  & wxMCANVAS_NO_H_SCROLL);
  allowYScroll = !(style  & wxMCANVAS_NO_V_SCROLL);
  fakeXScroll = !allowXScroll || (style & wxMCANVAS_HIDE_H_SCROLL);
  fakeYScroll = !allowYScroll || (style & wxMCANVAS_HIDE_V_SCROLL);

  auto_x = (!fakeXScroll && ((style & wxMCANVAS_AUTO_H_SCROLL) ? 1 : 0));
  auto_y = (!fakeYScroll && ((style & wxMCANVAS_AUTO_V_SCROLL) ? 1 : 0));
  xscroll_on = (!fakeXScroll && !auto_x);
  yscroll_on = (!fakeYScroll && !auto_y);
  EnableScrolling(xscroll_on, yscroll_on);
  noloop = TRUE;
  wxCanvas::SetScrollbars(fakeXScroll ? -1 : 1, fakeYScroll ? -1 : 1,
#ifdef wx_msw
			  fakeXScroll ? -1 : 1, fakeYScroll ? -1 : 1,
#else
			  1, 1,
#endif
			  1, 1, 0, 0, FALSE);
  if (fakeXScroll) {
    SimpleScroll *ss;
    ss = new WXGC_PTRS SimpleScroll(this, wxHORIZONTAL, 0, 1, 0) ;
    hscroll = ss;
  } else
    hscroll = (SimpleScroll *)NULL;
  if (fakeYScroll) {
    SimpleScroll *ss;
    ss = new WXGC_PTRS SimpleScroll(this, wxVERTICAL, 0, 1, 0);
    vscroll = ss;
  } else
    vscroll = (SimpleScroll *)NULL;
  scrollWidth = fakeXScroll ? 0 : 1;
  scrollHeight = fakeYScroll ? 0 : 1;

  vscrollsPerPage = hscrollsPerPage = 1;
  hpixelsPerScroll = 0;

  noloop = FALSE;

  {
    wxCanvasMediaAdmin *cma;
    cma = new WXGC_PTRS wxCanvasMediaAdmin(this);
    admin = cma;
  }
  admin->standard = 1;

  customCursor = NULL;
  customCursorOn = FALSE;

  focuson = FALSE;
  focusforcedon = FALSE;

  scrollToLast = FALSE;
  scrollBottomBased = FALSE;
  scrollOffset = 0;

  lastwidth = lastheight = -1;

  lazy_refresh = need_refresh = FALSE;

  autoDragger = NULL;

  if (!default_wheel_amt) {
    wxGetPreference("wheelStep", &default_wheel_amt);
    if (!default_wheel_amt)
      default_wheel_amt = 3;
    if (default_wheel_amt > 1000)
      default_wheel_amt = 1000;
  }
  wheel_amt = default_wheel_amt;

  if (m)
    SetMedia(m);

#ifndef wx_mac
  {
    wxDC *adc;
    adc = GetDC();
    adc->SetOptimization(TRUE);
  }
#endif
}

wxMediaCanvas::~wxMediaCanvas()
{
  if (autoDragger) {
    autoDragger->Kill();
    autoDragger = NULL;
  }

  if (blinkTimer) {
    ((wxBlinkTimer *)blinkTimer)->Kill();
    blinkTimer = NULL;
  }

  /* If we're managing an editor, we would like to disconnect from the
     editor --- but we don't want to execute any Scheme code to
     perform disconnection notices. (After all, this destruction could
     be a finalization callback.) So we don't disconnect, and
     fortunately, we have a layer oif indirection through the
     administrator -- we set the "dead" flag. Later actions directly
     on the canvas (possible because the Scheme code keeps a list of
     canvas) will trigger "invalidated object" errors. */
#if 0
  if (media) {
    if (admin->nextadmin || admin->prevadmin)
      SetMedia(NULL);
    else
      DELETE_OBJ media;
  }
  DELETE_OBJ admin;
#else
  admin->canvas = NULL;
#endif
}

void wxMediaCanvas::OnSize(int w, int h)
{
#ifndef wx_msw
  wxCanvas::OnSize(w, h);
#endif

  if (noloop)
    return;

  if (w == lastwidth
      && h == lastheight)
    return;

  if (media && media->printing)
    return;

  ResetSize();
}

void wxMediaCanvas::ResetSize()
{
  ResetVisual(FALSE);

#if defined(wx_mac)
  {
    wxDC *adc;
    wxColor *bg;
    bg = GetCanvasBackground();
    if (bg) {
      adc = GetDC();
      adc->SetBackground(bg);
      adc->Clear();
    }
  }
#endif

  Refresh();
}

void wxMediaCanvas::SetXMargin(int x)
{
  if (x != xmargin) {
    xmargin = x;
    ResetSize();
  }
}

void wxMediaCanvas::SetYMargin(int y)
{
  if (y != ymargin) {
    ymargin = y;
    ResetSize();
  }
}

int wxMediaCanvas::GetXMargin()
{
  return xmargin;
}

int wxMediaCanvas::GetYMargin()
{
  return ymargin;
}

void wxMediaCanvas::SetCanvasBackground(wxColour *c)
{
  wxCanvas::SetCanvasBackground(c);

  Refresh();
}

void wxMediaCanvas::OnFocus(Bool focus)
{
  if (focuson == focus)
    return;

  focuson = focus;
  if (media && !media->printing) {
    wxCanvasMediaAdmin *oldadmin;
    
    if (PTRNE((oldadmin = (wxCanvasMediaAdmin *)media->GetAdmin()), admin)) {
      media->SetAdmin(admin);
    }
    
    media->OwnCaret(focus);
    
    if (PTRNE(oldadmin, admin)) {
      media->SetAdmin(oldadmin);
    }
  }

  if (focuson) {
    if (!blinkTimer) {
      wxBlinkTimer *bt;
      bt = new WXGC_PTRS wxBlinkTimer(this);
      blinkTimer = bt;
    }
    blinkTimer->Start(BLINK_DELAY, 1);
  }
}

void wxMediaCanvas::BlinkCaret()
{
  if (focuson) {
    if (media) {
      wxCanvasMediaAdmin *oldadmin;
    
      if (PTRNE((oldadmin = (wxCanvasMediaAdmin *)media->GetAdmin()), admin)) {
	media->SetAdmin(admin);
      }

      media->BlinkCaret();

      if (PTRNE(oldadmin, admin)) {
	media->SetAdmin(oldadmin);
      }
    }

    blinkTimer->Start(BLINK_DELAY, 1);
  }
}

void *wxMediaCanvas::CallAsPrimaryOwner(void *(*f)(void *), void *data)
{
  void *r;

  if (media) {
    wxCanvasMediaAdmin *oldadmin;
    
    if (PTRNE((oldadmin = (wxCanvasMediaAdmin *)media->GetAdmin()), admin)) {
      media->SetAdmin(admin);
    }
    
    r = f(data);
    
    if (PTRNE(oldadmin, admin)) {
      media->SetAdmin(oldadmin);
    }
  } else
    r = f(data);

  return r;
}

#if defined(wx_msw) || defined(wx_mac)
extern struct MrEdContext *MrEdGetContext(wxObject *w);
extern void MrEdQueueInEventspace(void *context, Scheme_Object *thunk);
#endif

#ifdef wx_msw

static Scheme_Object *on_set_focus_cb(Scheme_Object *b_cnvs, int, Scheme_Object **)
{
  wxMediaCanvas *c;
  int on;
  on = SCHEME_TRUEP(SCHEME_CAR(b_cnvs));
  c = (wxMediaCanvas *)SCHEME_CDR(b_cnvs);
  c->OnFocus(on);
  return scheme_void;
}

static void QueueOnFocusCallback(wxMediaCanvas *canvas, int on)
{
  Scheme_Object *thunk, *b_cnvs;
  wxWindow *tl;
  void *context;

  b_cnvs = scheme_make_pair((on ? scheme_true : scheme_false),
			    (Scheme_Object *)canvas);

  thunk = scheme_make_closed_prim((Scheme_Closed_Prim *)on_set_focus_cb, b_cnvs);

  tl = canvas->GetTopLevel();
  context = MrEdGetContext(tl);

  MrEdQueueInEventspace(context, thunk);
}
#endif

void wxMediaCanvas::OnSetFocus()
{
#ifdef wx_msw
  // Need trampoline
  QueueOnFocusCallback(this, TRUE);
#else
  wxCanvas::OnSetFocus();
  OnFocus(TRUE);
#endif
}

void wxMediaCanvas::OnKillFocus()
{
#ifdef wx_msw
  // Need trampoline
  QueueOnFocusCallback(this, FALSE);
#else
  wxCanvas::OnKillFocus();
  OnFocus(FALSE);
#endif
}

Bool wxMediaCanvas::IsFocusOn()
{
  return focuson;
}

void wxMediaCanvas::ForceDisplayFocus(Bool on)
{
  int old = focusforcedon;

  focusforcedon = on;

  admin->AdjustStdFlag();

  if ((focuson || focusforcedon) != ((focuson || old)))
    Repaint();
}

void wxMediaCanvas::OnEvent(wxMouseEvent *event)
{
  /* Turn of auto-dragger if there is one. */
  if (autoDragger) {
    autoDragger->Kill();
    autoDragger = NULL;
  }

  last_x = event->x;
  last_y = event->y;

#ifdef wx_msw
  if (!focuson && event->ButtonDown()) {
    SetFocus();
    OnFocus(TRUE);
  }
#endif
  
  if (media && !media->printing) {
    wxCanvasMediaAdmin *oldadmin;
    
    if (PTRNE((oldadmin = (wxCanvasMediaAdmin *)media->GetAdmin()), admin)) {
      media->SetAdmin(admin);
    }

    {
      wxCursor *c;
      c = media->AdjustCursor(event);
      SetCustomCursor(c);
    }
    media->OnEvent(event);
    
    if (PTRNE(oldadmin, admin)) {
      media->SetAdmin(oldadmin);
    }

    if (event->Dragging()) {
      int ch, cw;
      GetClientSize(&cw, &ch);
      if (event->x < 0 || event->y < 0 || event->x > cw || event->y > ch) {
	/* Dragging outside the canvas: auto-generate more events because the buffer
	   is probably scrolling. But make sure we're shown. */
	wxWindow *w = this;
	while (w && w->IsShown()) {
	  if (wxSubType(w->__type, wxTYPE_FRAME)
	      || wxSubType(w->__type, wxTYPE_DIALOG_BOX))
	    w = NULL;
	  else
	    w = w->GetParent();
	}
	if (!w) {
	  autoDragger = new WXGC_PTRS wxAutoDragTimer(this, event);
	}
      }
    }
  }
}

void wxMediaCanvas::UpdateCursorNow(void)
{
  wxMouseEvent *event;
  wxCanvasMediaAdmin *oldadmin;
    
  if (!media)
    return;

  event = new WXGC_PTRS wxMouseEvent(wxEVENT_TYPE_MOTION);
  
  event->x = last_x;
  event->y = last_y;
  event->timeStamp = 0L;

  if (PTRNE((oldadmin = (wxCanvasMediaAdmin *)media->GetAdmin()), admin))
    media->SetAdmin(admin);
  
  {
    wxCursor *c;
    c = media->AdjustCursor(event);
    SetCustomCursor(c);
  }
    
  if (PTRNE(oldadmin, admin))
    media->SetAdmin(oldadmin);
}

wxMenu *wxMediaCanvas::PopupForMedia(wxMediaBuffer *WXUNUSED(b), void *WXUNUSED(m))
{
  return NULL;
}

void wxMediaCanvas::OnChar(wxKeyEvent *event)
{
  if (wheel_amt > 0) {
    /* Handle wheel here */
    switch (event->KeyCode()) {
    case WXK_WHEEL_UP:
    case WXK_WHEEL_DOWN:
      if (allowYScroll && !fakeYScroll) {
	int x, y;
	
	GetScroll(&x, &y);
	y += (wheel_amt * ((event->KeyCode() == WXK_WHEEL_UP) ? -1 : 1));
	if (y < 0) y = 0;	
	Scroll(x, y, 1);
      }
      return;
    }
  }

  if (media && !media->printing) {
    wxCanvasMediaAdmin *oldadmin;
    
    if (PTRNE((oldadmin = (wxCanvasMediaAdmin *)media->GetAdmin()), admin)) {
      media->SetAdmin(admin);
    }

    media->OnChar(event);
    
    if (PTRNE(oldadmin, admin)) {
      media->SetAdmin(oldadmin);
    }
  }
}

void wxMediaCanvas::OnPaint(void)
{
  need_refresh = FALSE;

  if (media) {
    if (!media->printing) {
      double w, h, x, y;
      GetView(&x, &y, &w, &h);
      Redraw(x, y, w, h);
    }
  } else {
    wxDC *adc;
    wxColor *bg;
    bg = GetCanvasBackground();
    if (bg) {
      adc = GetDC();
      adc->SetBackground(bg);
      adc->Clear();
    }
  }
  
  wxCanvas::OnPaint();
}

void wxMediaCanvas::Repaint(void)
{
  if (need_refresh)
    return;

  if (lazy_refresh || !GetCanvasBackground()) {
    need_refresh = TRUE;
    Refresh();
  } else
    OnPaint();
}

void wxMediaCanvas::PaintScrolls(void)
{
}

void wxMediaCanvas::SetLazyRefresh(Bool on)
{
  lazy_refresh = on;
  if (!on && need_refresh)
    OnPaint();
}

Bool wxMediaCanvas::GetLazyRefresh(void)
{
  return lazy_refresh;
}

void wxMediaCanvas::SetCustomCursor(wxCursor *cursor)
{
  if (!cursor) {
    NoCustomCursor();
  } else {
    customCursorOn = TRUE;
    customCursor = cursor;
    SetCursor(customCursor);
  }
}

void wxMediaCanvas::NoCustomCursor(void)
{
  static wxCursor *arrow = NULL;
  
  if (!arrow) {
    wxREGGLOB(arrow);
    arrow = new WXGC_PTRS wxCursor(wxCURSOR_ARROW);
  }

  if (customCursorOn) {
    customCursorOn = FALSE;
    SetCursor(arrow);
  }
}

wxDC *wxMediaCanvas::GetDCAndOffset(double *fx, double *fy)
{
  int x, y;

  if (fx || fy) {
    GetScroll(&x, &y);
    if (fx)
      *fx = x * hpixelsPerScroll - xmargin;
    if (fy) {
      if (media && (y  || scrollBottomBased)) {
	int h, w;
	GetClientSize(&w, &h);
	h -= 2 * ymargin;
	if (h < 0)
	  h = 0;
	{
	  double v;
	  v = media->ScrollLineLocation(y + scrollOffset) - ymargin;
	  *fy = v;
	}
	if (scrollBottomBased && (scrollHeight || scrollToLast))
	  (*fy) -= h;
      } else
	*fy = -ymargin;
    }
  }

  return GetDC();
}

void wxMediaCanvas::GetView(double *fx, double *fy, double *fw, double *fh, 
			    Bool WXUNUSED(full))
{
  int h, w;

  GetClientSize(&w, &h);
  GetDCAndOffset(fx, fy);
  if (1 /* !full */) {
    if (fx)
      *fx += xmargin;
    if (fy)
      *fy += ymargin;
  }
  if (0 /* full */) {
    if (fh)
      *fh = h;
    if (fw)
      *fw = w;
  } else {
    if (fh) {
      if (h > 2 * ymargin)
	*fh = h - 2 * ymargin;
      else
	*fh = 0;
    }
    if (fw) {
      if (w > 2 * xmargin)
	*fw = w - 2 * xmargin;
      else
	*fw = 0;
    }
  }
}

void wxMediaCanvas::Redraw(double localx, double localy, double fw, double fh)
{
  double x, y, w, h, right, bottom;

  if (!media || media->printing)
    return;

  GetView(&x, &y, &w, &h);

  right = x + w;
  bottom = y + h;

  if (localx > x)
    x = localx;
  if (localy > y)
    y = localy;

  if (right > localx + fw)
    right = localx + fw;
  if (bottom > localy + fh)
    bottom = localy + fh;
  w = right - x;
  h = bottom - y;

  if (w < 0)
    w = 0;
  if (h < 0)
    h = 0;

  if (w && h) {
    wxCanvasMediaAdmin *oldadmin;

    if (PTRNE((oldadmin = (wxCanvasMediaAdmin *)media->GetAdmin()), admin)) {
      media->SetAdmin(admin);
    }

    media->Refresh(x, y, w, h, 
		   (focuson || focusforcedon)
		   ? wxSNIP_DRAW_SHOW_CARET
		   : wxSNIP_DRAW_SHOW_INACTIVE_CARET,
		   GetCanvasBackground());

    if (PTRNE(oldadmin, admin)) {
      media->SetAdmin(oldadmin);
    }
  }
}

Bool wxMediaCanvas::ScrollTo(double localx, double localy, double fw, double fh,
			     Bool refresh, int bias)
{
  int cy, cx, sy, sx;
  double iw, ih;
  double x, y;
  double find_dy;

  if (!media || media->printing || (!allowXScroll && !allowYScroll))
    return FALSE;

  GetView(&x, &y, &iw, &ih);

  if (!iw || !ih)
    return FALSE;

  if (scrollBottomBased)
    find_dy = ih;
  else
    find_dy = 0;

  GetScroll(&cx, &cy);

  if (allowYScroll) {
    if (// doesn't fit and bias is set:
	(bias == -1 && fh > ih) 
	// fits, need to shift down into view:
	|| (fh <= ih && localy < y) 
	// doesn't fit, no conflicting bias, can shift up to see more:
	|| (fh > ih && bias != 1 && localy < y)) 
      sy = media->FindScrollLine(find_dy + localy) - scrollOffset;
    else if (// doesn't fit, bias is set:
	     (bias == 1 && fh > ih) 
	     // fits, need to shift up into view:
	     || (fh <= ih && y + ih < localy + fh))  {
      double l = find_dy + localy + fh - ih;
      // Find scroll pos for top of region to show:
      sy = media->FindScrollLine(l);
      // Unless l is exactly the top of a line, move down to the next whole line:
      if (media->ScrollLineLocation(sy) != l)
	sy++;
      sy -= scrollOffset;
    } else if (// doesn't fit, no conflicting bias, maybe shift down to see more:
	       (fh > ih && bias != -1 && localy + fh > y + ih)) {
      // Shift to one more than the first scroll position that shows last line
      long my;
      my = media->FindScrollLine(find_dy + localy + fh - ih) + 1 - scrollOffset;
      // But only shift down the extra line if doing so doesn't skip the whole area
      if (media->ScrollLineLocation(my) < find_dy + localy + fh)
	sy = my;
      else if (my > 0)
	sy = my - 1;
      else
        sy = 0;
    } else
      sy = cy;
  } else
    sy = cy;

  if (allowXScroll) {
    if (hpixelsPerScroll) {
      if ((bias == -1 && fw > iw)
	  || (fw < iw && localx < x)
	  || (fw > iw && bias != 1 && localx < x))
	sx = (int)(localx / hpixelsPerScroll);
      else if ((bias == 1 && fw > iw)
	       || (fw < iw && x + iw < localx + fw)
	       || (fw > iw && bias != -1 && localx + fw > x + iw))
	sx = (int)((localx + fw - iw) / hpixelsPerScroll) + 1;
      else
	sx = cx;
    } else
      sx = 0;
  } else
    sx = cx;

  if (sy != cy || sx != cx) {
    if (hscroll)
      hscroll->SetValue(sx);
    if (vscroll)
      vscroll->SetValue(sy);
    Scroll(sx, sy, refresh);
    return TRUE;
  } else
    return FALSE;
}

Bool wxMediaCanvas::ResetVisual(Bool reset_scroll)
{
  int x, y, sx, sy, lw, lh;
  double w, h;
  int hnumScrolls, vnumScrolls, hspp, vspp;
  long tw;
  double totalHeight, totalWidth;
  Bool retval = FALSE;

  if (givenHScrollsPerPage < 0) {
    givenHScrollsPerPage = -2;
    return FALSE;
  }

  while (1) {
    GetScroll(&sx, &sy);

    GetSize(&lw, &lh);
    lastwidth = lw;
    lastheight = lh;

    if (media && (allowXScroll || allowYScroll)) {
      if (reset_scroll)
	x = y = 0;
      else {
	x = sx;
	y = sy;
      }
      
      w = h = 0.0;
      GetView(NULL, NULL, &w, &h);
      totalWidth = totalHeight = 0.0;
      media->GetExtent(&totalWidth, &totalHeight);

      if (!h || (!scrollToLast && (h >= totalHeight))) {
	vnumScrolls = 0;
	scrollOffset = 0;
      } else {
	if (scrollBottomBased) {
	  vnumScrolls = media->NumScrollLines() - 1;
	  scrollOffset = 1;
	  if (!scrollToLast) {
	    long start;
	    start = media->FindScrollLine(h + 1) - 1;
	    scrollOffset += start;
	    vnumScrolls -= start;
	  }
	} else {
	  long top = (long)(totalHeight - (scrollToLast ? 0 : h));
	  if (top)
	    --top;
	  vnumScrolls = media->FindScrollLine(top) + 1;
	  if (vnumScrolls >= media->NumScrollLines())
	    vnumScrolls = media->NumScrollLines() - 1;
	  scrollOffset = 0;
	}
      }

      if (vnumScrolls > 0) {
	int numLines;
	numLines = media->NumScrollLines() - 1;
	vspp = (long)(((double)h * numLines) / totalHeight) - 1;
	if (vspp < 1)
	  vspp = 1;
      } else {
	vnumScrolls = 0;
	vspp = 1;
      }

      if (totalWidth >= w) {
	tw = (long)(totalWidth - w);

	hpixelsPerScroll = (long)(w / givenHScrollsPerPage);
	if (!hpixelsPerScroll)
	  hpixelsPerScroll = 2;

	if (tw % hpixelsPerScroll)
	  tw += (hpixelsPerScroll - (tw % hpixelsPerScroll));
	
	hnumScrolls = tw / hpixelsPerScroll;
	hspp = givenHScrollsPerPage;
      } else {
	hnumScrolls = 0;
	hspp = 1;
      }
    } else {
      x = y = 0;
      hnumScrolls = vnumScrolls = 0;
      vspp = hspp = 1;
      if (!media) {
	wxDC *adc;
	wxColor *bg;
	bg = GetCanvasBackground();
	if (bg) {
	  adc = GetDC();
	  adc->SetBackground(bg);
	  adc->Clear();
	}
      }
    }    

    if (scrollWidth != hnumScrolls || scrollHeight != vnumScrolls
	|| vspp != vscrollsPerPage
	|| hspp != hscrollsPerPage
	|| x != sx || y != sy) {
      Bool goAgain = FALSE;
      int savenoloop;
      int saveHSPP;
      int xon, yon;
      
      if (hscroll)
	hscroll->SetScroll(hnumScrolls, hspp, x);
      if (vscroll)
	vscroll->SetScroll(vnumScrolls, vspp, y);
      
      savenoloop = noloop;
      saveHSPP = givenHScrollsPerPage;
      
      noloop = TRUE;
      givenHScrollsPerPage = -1;

      xon = !fakeXScroll && hnumScrolls;
      yon = !fakeYScroll && vnumScrolls;
      if ((auto_x && (xon != xscroll_on)) || (auto_y && (yon != yscroll_on))) {
	if (auto_x)
	  xscroll_on = xon;
	if (auto_y)
	  yscroll_on = yon;
	EnableScrolling(xscroll_on, yscroll_on);
	OnScrollOnChange();
	goAgain = TRUE;
      }
      
      if (!fakeXScroll) {
	if (x > hnumScrolls)
	  x = hnumScrolls;
	if (hspp < hscrollsPerPage)
	  SetScrollPage(wxHORIZONTAL, hspp);
	if (x < sx)
	  SetScrollPos(wxHORIZONTAL, x);
	if (scrollWidth != hnumScrolls)
	  SetScrollRange(wxHORIZONTAL, hnumScrolls);
	if (x > sx)
	  SetScrollPos(wxHORIZONTAL, x);
	if (hspp > hscrollsPerPage)
	  SetScrollPage(wxHORIZONTAL, hspp);
      }

      if (!fakeYScroll) {
	if (y > vnumScrolls)
	  y = vnumScrolls;
	if (vspp < vscrollsPerPage)
	  SetScrollPage(wxVERTICAL, vspp);
	if (y < sy)
	  SetScrollPos(wxVERTICAL, y);
	if (scrollHeight != vnumScrolls)
	  SetScrollRange(wxVERTICAL, vnumScrolls);
	if (y > sy)
	  SetScrollPos(wxVERTICAL, y);
	if (vspp > vscrollsPerPage)
	  SetScrollPage(wxVERTICAL, vspp);
      }

      if (givenHScrollsPerPage < -1)
	goAgain = TRUE;
      givenHScrollsPerPage = saveHSPP;
      
      noloop = savenoloop;
      hscrollsPerPage = hspp;
      vscrollsPerPage = vspp;
      scrollWidth = hnumScrolls;
      scrollHeight = vnumScrolls;

      if (!goAgain)
	return TRUE;
      else
	retval = TRUE;
    } else
      return retval;
  }
}

void wxMediaCanvas::Scroll(int x, int y, Bool refresh)
{
  int savenoloop = noloop;
  noloop = TRUE;

  if (x > -1 && !fakeXScroll) {
    if (scrollWidth) {
      if (x > scrollWidth)
	x = scrollWidth;
      SetScrollPos(wxHORIZONTAL, x);
    }
  }

  if (y > -1 && !fakeYScroll) {
    if (scrollHeight) {
      if (y > scrollHeight)
	y = scrollHeight;
      SetScrollPos(wxVERTICAL, y);
    }
  }
  
  noloop = savenoloop;

  if (refresh)
    Repaint();
}

void wxMediaCanvas::Scroll(int, int)
{
  /* Nothing */
}

void wxMediaCanvas::SetScrollbars(int, int, int, int,
				  int, int, int, int,
				  Bool)
{
  /* Nothing */
}

void wxMediaCanvas::GetScroll(int *x, int *y)
{
  int v;
  
  /* Get fake scroll values if available */
  if (hscroll) {
    v = hscroll->GetValue();
    *x = v;
  }
  if (vscroll) {
    v = vscroll->GetValue();
    *y = v;
  }

  if (!hscroll) {
    int v;
    v = GetScrollPos(wxHORIZONTAL);
    *x = v;
  }
  if (!vscroll) {
    int v;
    v = GetScrollPos(wxVERTICAL);
    *y = v;
  }
}

#if defined(wx_msw) || defined(wx_mac)
static Scheme_Object *repaint_cb(Scheme_Object *_cnvs, int, Scheme_Object **)
{
  wxMediaCanvas *c;
  c = (wxMediaCanvas *)_cnvs;
  c->Repaint();
  return scheme_void;
}
#endif

void wxMediaCanvas::OnScroll(wxScrollEvent *)
{
  if (noloop)
    return;

#if defined(wx_msw) || defined(wx_mac)
  // Need trampoline
  {
    Scheme_Object *thunk;
    wxWindow *tl;
    void *context;

    thunk = scheme_make_closed_prim((Scheme_Closed_Prim *)repaint_cb, (Scheme_Object *)this);

# ifdef wx_msw
    tl = GetTopLevel();
# else
    tl = GetRootFrame();
# endif
    context = MrEdGetContext(tl);
    
    MrEdQueueInEventspace(context, thunk);
  }
#else
  Repaint();
#endif
}

void wxMediaCanvas::OnScrollOnChange()
{
}

wxMediaBuffer *wxMediaCanvas::GetMedia(void)
{
  return media;
}

void wxMediaCanvas::SetMedia(wxMediaBuffer *m, Bool update)
{
  if (media == m)
    return;

  if (media) {
    if (PTREQ((wxCanvasMediaAdmin *)media->GetAdmin(), admin)) {
      if (admin->nextadmin)
	media->SetAdmin(admin->nextadmin);
      else if (admin->prevadmin)
	media->SetAdmin(admin->prevadmin);
      else
	media->SetAdmin(NULL);
    }

    if (admin->nextadmin) {
      admin->nextadmin->prevadmin = admin->prevadmin;
      admin->nextadmin->AdjustStdFlag();
      admin->nextadmin = NULL;
    }
    if (admin->prevadmin) {
      admin->prevadmin->nextadmin = admin->nextadmin;
      admin->prevadmin->AdjustStdFlag();
      admin->prevadmin = NULL;
    }
    if (customCursor) {
      NoCustomCursor();
      customCursor = NULL;
    }
  }
  media = m;
  if (media) {
    wxMediaAdmin *oldadmin;
    if ((oldadmin = media->GetAdmin())) {
      if (!oldadmin->standard) {
	media = NULL;
	return;
      }
      admin->nextadmin = (wxCanvasMediaAdmin *)oldadmin;
      admin->prevadmin = admin->nextadmin->prevadmin;
      admin->nextadmin->prevadmin = admin;
      admin->nextadmin->AdjustStdFlag();
      if (admin->prevadmin) {
	admin->prevadmin->nextadmin = admin;
	admin->prevadmin->AdjustStdFlag();
      }
      /* Get the right cursor: */
      admin->UpdateCursor();
    } else {
      admin->nextadmin = admin->prevadmin = NULL;
      media->SetAdmin(admin);
      media->OwnCaret(focuson);
    }
  }

  admin->AdjustStdFlag();

  ResetVisual(TRUE);
  if (update)
    Repaint();
}

void wxMediaCanvas::AllowScrollToLast(Bool toLast)
{
  scrollToLast = toLast;
  ResetVisual(FALSE);
  Repaint();
}

void wxMediaCanvas::ScrollWithBottomBase(Bool bottom)
{
  scrollBottomBased = bottom;
  ResetVisual(FALSE);
  Repaint();
}

/************************************************************************/

wxCanvasMediaAdmin::wxCanvasMediaAdmin(wxMediaCanvas *c)
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_CANVAS_MEDIA_ADMIN;
#endif

  canvas = c;
  resetFlag = FALSE;
  nextadmin = prevadmin = NULL;
  WXGC_IGNORE(this, canvas);

  updateCursorTimer = NULL;

  updateBlock = resizedBlock = FALSE;
}

wxCanvasMediaAdmin::~wxCanvasMediaAdmin()
{
  if (updateCursorTimer)
    updateCursorTimer->Cancel();
  canvas = NULL;
}

wxDC *wxCanvasMediaAdmin::GetDC(double *fx, double *fy)
{
  if (!canvas) {
    if (!wx_canvasless_offscreen) {
      wxREGGLOB(wx_canvasless_offscreen);
      wx_canvasless_offscreen = new WXGC_PTRS wxMemoryDC();
    }
    if (fx)
      *fx = 0;
    if (fy)
      *fy = 0;      
    return wx_canvasless_offscreen;
  } else if (canvas->media && canvas->media->printing) {
    if (fx)
      *fx = 0;
    if (fy)
      *fy = 0;
    return canvas->media->printing;
  } else
    return canvas->GetDCAndOffset(fx, fy);
}

void wxCanvasMediaAdmin::GetView(double *fx, double *fy, double *fh, double *fw, 
				 Bool full)
{
  if (!canvas) {
    if (fx)
      *fx = 0;
    if (fy)
      *fy = 0;
    if (fh)
      *fh = 1;
    if (fw)
      *fw = 1;
  } else if (canvas->media && canvas->media->printing) {
    if (fx)
      *fx = 0;
    if (fy)
      *fy = 0;
    if (fh)
      *fh = 10000;
    if (fw)
      *fw = 10000;
  } else
    canvas->GetView(fx, fy, fh, fw, full);
}

void wxCanvasMediaAdmin::GetMaxView(double *fx, double *fy, double *fw, double *fh, 
				    Bool full)
{
  if ((!nextadmin && !prevadmin) || !canvas || (canvas->media && canvas->media->printing)) {
    GetView(fx, fy, fw, fh, full);
  } else {
    wxCanvasMediaAdmin *a;
    double cx, x, cy, y, cw, w, ch, h, cr, r, cb, b;

    a = this;
    while (a->prevadmin) {
      a = a->prevadmin;
    }
    a->GetView(&cx, &cy, &cw, &ch);
    cr = cx + cw;
    cb = cy + ch;
    for (a = a->nextadmin; a; a = a->nextadmin) {
      a->GetView(&x, &y, &w, &h);
      r = x + w;
      b = y + h;

      if (x < cx)
	cx = x;
      if (y < cy)
	cy = y;
      if (r > cr)
	cr = r;
      if (b > cb)
	cb = b;
    }

    cw = cr - cx;
    ch = cb - cy;

    if (fx)
      *fx = cx;
    if (fy)
      *fy = cy;
    if (fw)
      *fw = cw;
    if (fh)
      *fh = ch;
  }
}

Bool wxCanvasMediaAdmin::ScrollTo(double localx, double localy,
				  double w, double h, Bool refresh, int bias)
{
  if (!canvas)
    return FALSE;

  if (!canvas->IsFocusOn()) {
    wxCanvasMediaAdmin *a;
    
    for (a = nextadmin; a; a = a->nextadmin) {
      if (a->canvas->IsFocusOn())
	return a->ScrollTo(localx, localy, w, h, refresh, bias);
    }
    for (a = prevadmin; a; a = a->prevadmin) {
      if (a->canvas->IsFocusOn())
	return a->ScrollTo(localx, localy, w, h, refresh, bias);
    }
  }

  return canvas->ScrollTo(localx, localy, w, h, refresh, bias);
}

void wxCanvasMediaAdmin::GrabCaret(int dist)
{
  if (canvas) {
    if (dist == wxFOCUS_GLOBAL)
      canvas->SetFocus();
  }
}

void wxCanvasMediaAdmin::NeedsUpdate(double localx, double localy, 
				     double w, double h)
{
  int is_shown;
  wxWindow *win;

  if (updateBlock || !canvas)
    return;

  updateBlock = TRUE;

  is_shown = 1;
  win = canvas;
  while (win)  {
    if (!win->IsShown()) {
      is_shown = 0;
      win = NULL;
    } else if (wxSubType(win->__type, wxTYPE_FRAME)
	       || wxSubType(win->__type, wxTYPE_DIALOG_BOX))
      win = NULL;
    else
      win = win->GetParent();
  }

  if (resetFlag) {
    if (is_shown)
      canvas->Repaint();
    resetFlag = FALSE;
  } else if (is_shown) {
    if (!canvas->GetCanvasBackground())
      canvas->Repaint();
    else
      canvas->Redraw(localx, localy, w, h);
  }

  if (nextadmin)
    nextadmin->NeedsUpdate(localx, localy, w, h);
  if (prevadmin)
    prevadmin->NeedsUpdate(localx, localy, w, h);

  updateBlock = FALSE;
}

void wxCanvasMediaAdmin::Resized(Bool update)
{
  if (resizedBlock || !canvas)
    return;

  resizedBlock = TRUE;

  if (canvas->ResetVisual(FALSE))
    resetFlag = TRUE;

  if (update) {
    canvas->Repaint();
    resetFlag = FALSE;
  }
  if (nextadmin)
    nextadmin->Resized(update);
  if (prevadmin)
    prevadmin->Resized(update);

  resizedBlock = FALSE;
}

void wxCanvasMediaAdmin::UpdateCursor()
{
  if (!updateCursorTimer && canvas) {
    updateCursorTimer = new WXGC_PTRS wxUpdateCursorTimer(this);

    if (nextadmin)
      nextadmin->UpdateCursor();
    if (prevadmin)
      prevadmin->UpdateCursor();
  }
}

Bool wxCanvasMediaAdmin::PopupMenu(void *m, double x, double y)
{
  double dx, dy;
  wxMenu *menu;

  if (canvas && canvas->media) {
    menu = canvas->PopupForMedia(canvas->media, m);
    if (menu) {
      (void)canvas->GetDCAndOffset(&dx, &dy);
      return canvas->PopupMenu(menu, x - dx, y - dy);
    }
  }

  return FALSE;
}

void wxCanvasMediaAdmin::AdjustStdFlag(void)
{ 
  /* 1 indicates that this is the sole, main admin. 
     This info is used for quick (Xor) caret refreshing
     by an editor buffer. */

  standard = (nextadmin 
	      || prevadmin 
	      || (canvas && canvas->focusforcedon)) 
    ? -1 : 1; 
}

void wxCanvasMediaAdmin::Modified(Bool modified)
{
  /* nothing to do */
}

void wxMediaAdmin::GetMaxView(double *fx, double *fy, double *fh, double *fw, 
			      Bool full)
{
  GetView(fx, fy, fh, fw, full);
}

Bool wxMediaAdmin::DelayRefresh()
{
  return FALSE;
}

/*************************************************************/

#define MIN_THUMB_WIDTH 8

SimpleScroll::SimpleScroll(wxMediaCanvas *,
			   long style,
			   int length, int stepsPerPage,
			   int position)
{
  count = length;
  pageStep = stepsPerPage;
  value = position;

  horizontal = !!(style & wxHORIZONTAL);
  SetScroll(length, stepsPerPage, position);
}

SimpleScroll::~SimpleScroll()
{
}

void SimpleScroll::SetValue(int position)
{
  if (position < 0)
    position = 0;
  if (position >= count)
    position = count;

  value = position;
}

void SimpleScroll::SetScroll(int length, int stepsPerPage, int position)
{
  if (length > -1)
    count = length;
  if (stepsPerPage > 0)
    pageStep = stepsPerPage;
  if (position > -1)
    value = position;

  if (value < 0)
    value = 0;
  if (value > count)
    value = count;
}

int SimpleScroll::GetValue(void)
{
  return value;
}
