/*
 * File:        wx_canvs.cc
 * Purpose:     wxCanvas implementation (MSW)
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	August 1994
 * Copyright:   (c) 2004-2005 PLT Scheme, Inc.
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include "../../../wxcommon/wxGLConfig.h"

#include <math.h>

#define COMBO_WIDTH 18

extern char wxCanvasClassName[];
extern void RegisterGDIObject(HANDLE x);

wxCanvas::wxCanvas (void)
{
  wxWinType = wxTYPE_XWND;
  handle = NULL;
  window_parent = NULL;
  horiz_units = 0;
  vert_units = 0;
  wx_dc = NULL;
}

wxCanvas::wxCanvas (wxWindow *parent, int x, int y, int width, int height, long style,
	  char *name, wxGLConfig *cfg):
wxbCanvas (parent, x, y, width, height, style, name)
{
  Create(parent, x, y, width, height, style, name, cfg);
}

Bool wxCanvas::
Create (wxWindow * parent, int x, int y, int width, int height, long style,
	char *name, wxGLConfig *cfg)
{
  wxWnd *cparent;
  DWORD msflags = 0, exflags = 0;
  wxCanvasWnd *wnd;

  wxWinType = wxTYPE_XWND;
  windowStyle = style;
  cparent = NULL;
  if (parent)
    cparent = (wxWnd *) parent->handle;

  bgcol = ((style & wxTRANSPARENT_WIN) ? NULL : wxWHITE);

  if (wxSubType(parent->__type, wxTYPE_PANEL))
    ((wxPanel *)parent)->GetValidPosition(&x, &y);

  if (style & wxCONTROL_BORDER) {
    exflags |= WS_EX_CLIENTEDGE;
  } else if (style & wxBORDER) {
    // msflags |= WS_BORDER;
    exflags |= WS_EX_STATICEDGE;
  }

  msflags |= WS_CHILD | ((style & wxINVISIBLE) ? 0 : WS_VISIBLE);
  if (style & wxHSCROLL)
    msflags |= WS_HSCROLL;
  if (style & wxVSCROLL)
    msflags |= WS_VSCROLL;
  msflags |= WS_CLIPSIBLINGS;

  if (style & wxCOMBO_SIDE) {
    combo = new wxCombo(this,
			(wxPanel *)parent, NULL, NULL,
			x, y, width, height,
			0, NULL,
			(style & wxINVISIBLE));
  }

  wnd = new wxCanvasWnd (cparent, this, 
			 wxNEG_POS_IS_DEFAULT(x), wxNEG_POS_IS_DEFAULT(y), 
			 width, height, msflags, exflags);
  handle = (char *) wnd;

  if (parent)
    parent->AddChild (this);
  window_parent = parent;

  horiz_units = 0;
  vert_units = 0;

  if ((style & wxHSCROLL) || (style & wxVSCROLL))
    SetScrollbars(style & wxHSCROLL, style & wxVSCROLL,
		  0, 0, 1, 1, 0, 0, FALSE);

  wx_dc = new wxCanvasDC(this);
  if (cfg) {
    cfg = cfg->Clone();
    wx_dc->wx_gl_cfg = cfg;
  }

  if (wxSubType(parent->__type, wxTYPE_PANEL))
    ((wxPanel *)parent)->AdvanceCursor(this);
  else
    InitEnable();

  if (style & wxINVISIBLE)
    Show(FALSE);

  {
    wnd->x_scrolling_enabled = ((style & wxHSCROLL) ? 1 : 0);
    wnd->y_scrolling_enabled = ((style & wxVSCROLL) ? 1 : 0);
  }

  return TRUE;
}

wxCanvas::~wxCanvas (void)
{
  if (wx_dc) {
    wxWnd *wnd = (wxWnd *)handle;
    HDC dc;
    dc = wxwmGetDC(wnd->handle);
    wx_dc->SelectOldObjects(dc);
    wxwmReleaseDC(wnd->handle, dc);
    delete wx_dc;
  }
}

void wxCanvas::GetSize(int *width, int *height)
{
  wxWindow::GetSize(width, height);
  if (width && combo)
    *width += COMBO_WIDTH;
}

void wxCanvas::SetSize (int x, int y, int w, int h, int sizeFlags)
{
  int currentX, currentY;
  int ww, hh;
  wxWnd *wnd = (wxWnd *) handle;

  GetPosition (&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  GetSize(&ww, &hh);
  if (w == -1)
    w = ww;
  if (h == -1)
    h = hh;

  if (wnd) {
    MoveWindow(wnd->handle, x, y, w - (combo ? COMBO_WIDTH : 0), h, TRUE);
    if (combo)
      combo->SetSize(x, y, w, h);
    {
      wxDC *dc;
      dc = GetDC();
      if (dc)
	dc->ReleaseGraphics();
    }
    OnSize(w, h);
  }
}

Bool wxCanvas::Show(Bool show)
{
  if (combo)
    combo->Show(show);
    
  return wxbCanvas::Show(show);
}

wxWindow *wxCanvas::FindFocusWindow()
{
  if (!wxSubType(__type, wxTYPE_PANEL))
    return IsShown() ? this : NULL;
  else
    return wxWindow::FindFocusWindow();
}

/*
 * horizontal/vertical: number of pixels per unit (e.g. pixels per text line)
 * x/y_length:        : no. units per scrollbar
 * x/y_page:          : no. units per page scrolled
 */
void wxCanvas::
SetScrollbars (int horizontal, int vertical,
	       int x_length, int y_length,
	       int x_page, int y_page,
	       int x_pos, int y_pos, Bool setVirtualSize)
{
  wxWnd *wnd = (wxWnd *)handle;

  if (!(GetWindowStyleFlag() & wxHSCROLL))
    horizontal = -1;
  if (!(GetWindowStyleFlag() & wxVSCROLL))
    vertical = -1;
  
  if (!horizontal) horizontal = -1;
  if (!vertical) vertical = -1;
  if (x_length < 1) horizontal = -1;
  if (y_length < 1) vertical = -1;
  if (x_page < 1) x_page = 1;
  if (y_page < 1) y_page = 1;
  
  if (x_pos < 0)
    x_pos = 0;
  if (x_pos > x_length)
    x_pos = x_length;
  if (y_pos < 0)
    y_pos = 0;
  if (y_pos > y_length)
    y_pos = y_length;
  
  horiz_units = horizontal;
  vert_units = vertical;
  
  if (wnd) {
    Bool h_is_on, v_is_on;
    int w, h;
    RECT rect;
    SCROLLINFO hinfo, vinfo;
    
    wnd->calcScrolledOffset = setVirtualSize;
    
    GetClientRect(wnd->handle, &rect);
    w = rect.right - rect.left;
    h = rect.bottom - rect.top;

    if (!w) w = 1;
    if (!h) h = 1;
    
    hinfo.cbSize = vinfo.cbSize = sizeof(SCROLLINFO);
    hinfo.fMask = vinfo.fMask = SIF_PAGE | SIF_RANGE | SIF_POS;
    hinfo.nMin = vinfo.nMin = 0;

    if (wnd->x_scroll_visible)
      hinfo.fMask |= SIF_DISABLENOSCROLL;
    if (wnd->y_scroll_visible)
      vinfo.fMask |= SIF_DISABLENOSCROLL;
    
    // Recalculate scroll bar range and position
    // ShowScrollBar(handle, SB_HORZ, wnd->xscroll_lines > 0);
    if (horizontal > 0)	{
      int nHscrollMax;

      h_is_on = 1;
      
      if (setVirtualSize) {
	wnd->xscroll_pixels_per_line = 1;
	wnd->xscroll_lines = (x_length * horizontal);
      } else {
	wnd->xscroll_pixels_per_line = horizontal;
	wnd->xscroll_lines = x_length;
	wnd->xscroll_lines_per_page = x_page;
      }

      if (setVirtualSize) {
	int nMaxWidth = wnd->xscroll_lines;
	nHscrollMax = (nMaxWidth - w);
	nHscrollMax = max(0, nHscrollMax);
	wnd->xscroll_lines_per_page = nHscrollMax ? w : 1;
      } else
	nHscrollMax = wnd->xscroll_lines;

      wnd->xscroll_position = min(nHscrollMax, x_pos);
      
      hinfo.nPos = wnd->xscroll_position;
      hinfo.nPage = wnd->xscroll_lines_per_page;
      hinfo.nMax = nHscrollMax + hinfo.nPage - 1;
    } else {
      h_is_on = 0;
      
      wnd->xscroll_pixels_per_line = -1;
      wnd->xscroll_lines = -1;
      wnd->xscroll_lines_per_page = 0;
      
      hinfo.nPos = 0;
      hinfo.nPage = 1;
      hinfo.nMax = 0;
    }
    
    
    // ShowScrollBar(handle, SB_VERT, wnd->yscroll_lines > 0);
    if (vertical > 0) {
      int nVscrollMax;

      v_is_on = 1;
      
      if (setVirtualSize) {
	wnd->yscroll_pixels_per_line = 1;
	wnd->yscroll_lines = (y_length * vertical);
      } else {
	wnd->yscroll_pixels_per_line = vertical;
	wnd->yscroll_lines = y_length;
	wnd->yscroll_lines_per_page = y_page;
      }
      
      if (setVirtualSize) {
	int nMaxHeight = wnd->yscroll_lines;
	nVscrollMax = (nMaxHeight - h);
	nVscrollMax = max(nVscrollMax, 0);
	wnd->yscroll_lines_per_page = nVscrollMax ? h : 1;
      } else
	nVscrollMax  = wnd->yscroll_lines;
      
      wnd->yscroll_position = min (nVscrollMax, y_pos);
      
      vinfo.nPos = wnd->yscroll_position;
      vinfo.nPage = wnd->yscroll_lines_per_page;
      vinfo.nMax = nVscrollMax + vinfo.nPage - 1;
    } else {
      v_is_on = 0;
      
      wnd->yscroll_pixels_per_line = -1;
      wnd->yscroll_lines = -1;
      wnd->yscroll_lines_per_page = 0;
      
      vinfo.nPos = 0;
      vinfo.nPage = 1;
      vinfo.nMax = 0;
    }
    
    if (GetWindowStyleFlag() & wxVSCROLL) {
      ::SetScrollInfo(wnd->handle, SB_VERT, &vinfo, TRUE);
    }
    if (GetWindowStyleFlag() & wxHSCROLL) {
      ::SetScrollInfo(wnd->handle, SB_HORZ, &hinfo, TRUE);
    }
    
    if (setVirtualSize)
      OnCalcScroll();

    InvalidateRect(wnd->handle, NULL, TRUE);
    UpdateWindow(wnd->handle);
  }
}

void wxCanvas::GetScrollUnitsPerPage (int *x_page, int *y_page)
{
  wxWnd *wnd = (wxWnd *) handle;
  if (wnd) {
    *x_page = wnd->xscroll_lines_per_page;
    *y_page = wnd->yscroll_lines_per_page;
  }
}

/*
 * Scroll to given position (scroll position, not pixel position)
 */
void wxCanvas::Scroll (int x_pos, int y_pos)
{
  if (x_pos > -1)
    SetScrollPos(-wxHORIZONTAL, x_pos);
  if (y_pos > -1)
    SetScrollPos(-wxVERTICAL, y_pos);
}

void wxCanvas::ScrollPercent(double x, double y)
{
  wxWnd *wnd = (wxWnd *) handle;
  if (!wnd) 
    return;

  if (!wnd->calcScrolledOffset) {
    /* Not managing  - do nothing */
  } else {
    /* Managing */
    int xp, yp, vw, vh, cw, ch;
    GetVirtualSize(&vw, &vh);
    GetClientSize(&cw, &ch);

    if (vw > cw)
      vw -= cw;
    else
      vw = 0;
    if (vh > ch)
      vh -= ch;
    else
      vh = 0;

    if (x >= 0)
      xp = (int)floor(x * vw);
    else
      xp = -1;

    if (y >= 0)
      yp = (int)floor(y * vh);
    else
      yp = -1;

    Scroll(xp, yp);

    if ((xp > 0) || (yp > 0))
      Refresh();
  }
}

void wxCanvas::EnableScrolling (Bool x_scroll, Bool y_scroll)
{
  wxWnd *wnd = (wxWnd *)handle;

  ShowScrollBar(wnd->handle, SB_HORZ, x_scroll);
  ShowScrollBar(wnd->handle, SB_VERT, y_scroll);
  wnd->x_scroll_visible = x_scroll;
  wnd->y_scroll_visible = y_scroll;
}

void wxCanvas::GetVirtualSize (int *x, int *y)
{
  wxWnd *wnd = (wxWnd *) handle;

  GetClientSize(x, y);

  if (wnd && wnd->calcScrolledOffset) {
    if (wnd->xscroll_lines > 0)
      *x = wnd->xscroll_pixels_per_line * wnd->xscroll_lines;
    if (wnd->yscroll_lines > 0)
      *y = wnd->yscroll_pixels_per_line * wnd->yscroll_lines;
  }
}

void wxCanvas::WarpPointer (int x_pos, int y_pos)
{
  // Move the pointer to (x_pos,y_pos) coordinates. They are expressed in
  // pixel coordinates, relatives to the canvas -- So, we first need to
  // substract origin of the window, then convert to screen position

  wxWnd *wnd = (wxWnd *) handle;

  if (wnd) {
    RECT rect;

    x_pos -= wnd->xscroll_position * wnd->xscroll_pixels_per_line;
    y_pos -= wnd->yscroll_position * wnd->yscroll_pixels_per_line;
    
    GetWindowRect (wnd->handle, &rect);
    
    x_pos += rect.left;
    y_pos += rect.top;
    
    SetCursorPos (x_pos, y_pos);
  }
}

// Where the current view starts from
void wxCanvas::ViewStart(int *x, int *y, Bool)
{
  wxWnd *wnd = (wxWnd *) handle;

  if (!wnd->calcScrolledOffset) {
    *x = *y = 0;
  } else {
    *x = wnd->xscroll_position;
    *y = wnd->yscroll_position;
  }
}

void wxCanvas::OnCalcScroll()
{
  wxDC *dc;
  dc = GetDC();
  dc->OnCalcScroll();
}

void wxWnd::DeviceToLogical (double *x, double *y)
{
  if (is_canvas) {
    wxCanvas *canvas;
    canvas = (wxCanvas *) wx_window;
    if (canvas->wx_dc) {
      int x2, y2;
      x2 = canvas->wx_dc->DeviceToLogicalX ((int) *x);
      y2 = canvas->wx_dc->DeviceToLogicalY ((int) *y);
      *x = x2;
      *y = y2;
    }
  }
}

wxCanvasWnd::wxCanvasWnd (wxWnd * parent, wxWindow * wx_win,
			  int x, int y, int width, int height, 
			  DWORD style, DWORD exstyle)
: wxSubWnd (parent, wxCanvasClassName, 
	    wx_win, x, y, width, height, style, NULL, exstyle)
{
  is_canvas = TRUE;
}

static HBRUSH btnface_brush;

BOOL wxCanvasWnd::OnEraseBkgnd (HDC pDC)
{
  long wstyle;

  wstyle = wx_window->GetWindowStyleFlag();

  if (!(wstyle & wxNO_AUTOCLEAR)) {
    RECT rect;
    wxCanvas *canvas;
    int mode, rop;
    HBRUSH brsh;
    int free_brush = 0;
    wxColor *bgcol;
	
    canvas = (wxCanvas *)wx_window;
    bgcol = canvas->GetCanvasBackground();
    
    GetClientRect(handle, &rect);
    mode = SetMapMode(pDC, MM_TEXT);
    rop = SetROP2(pDC, R2_COPYPEN);
    if (wstyle & wxTRANSPARENT_WIN) {
      if (!btnface_brush) {
	btnface_brush = CreateSolidBrush(GetSysColor(COLOR_BTNFACE));
	RegisterGDIObject(btnface_brush);
      }
      brsh = btnface_brush;
    } else if (bgcol == wxWHITE) {
      brsh = (HBRUSH)GetStockObject(WHITE_BRUSH);
    } else {
      brsh = CreateSolidBrush(bgcol->pixel);
      free_brush = 1;
    }
    FillRect(pDC, &rect, brsh);
    SetMapMode(pDC, mode);
    SetROP2(pDC, rop);

    if (free_brush)
      DeleteObject(brsh);

    if (canvas->wx_dc) {
      SetViewportExtEx(pDC, VIEWPORT_EXTENT, VIEWPORT_EXTENT, NULL);
      SetWindowExtEx(pDC, canvas->wx_dc->window_ext_x, canvas->wx_dc->window_ext_y, NULL);
    }
  }
  
  return TRUE;
}

void wxCanvas::SetCanvasBackground(wxColor *c)
{
  if (!bgcol || !c)
    return;
  
  if (c && c->IsMutable()) {
    c = new wxColour(c);
    c->Lock(1);
  }
   
  bgcol = c;
}

wxColour *wxCanvas::GetCanvasBackground()
{
  return bgcol;
}

typedef void (WINAPI *wxCLOSE_THEME_DATA_PROC)(HANDLE);
typedef HANDLE (WINAPI *wxOPEN_THEME_DATA_PROC)(HWND, LPCWSTR);
typedef HRESULT (WINAPI *wxDRAW_THEME_BACKGROUND_PROC)(HANDLE, HDC, int, int, const RECT *, const RECT *);
typedef HRESULT (WINAPI *wxDRAW_THEME_EDGE_PROC)(HANDLE, HDC, int, int, const RECT *, UINT, UINT, RECT *);

static int theme_tried;
static wxCLOSE_THEME_DATA_PROC wxCloseThemeData;
static wxOPEN_THEME_DATA_PROC wxOpenThemeData;
static wxDRAW_THEME_BACKGROUND_PROC wxDrawThemeBackground;
static wxDRAW_THEME_EDGE_PROC wxDrawThemeEdge;

BOOL wxCanvasWnd::NCPaint(WPARAM wParam, LPARAM lParam, LONG *result)
{
  long wstyle;

  wstyle = wx_window->GetWindowStyleFlag();

  if ((wstyle & wxCONTROL_BORDER) && !(wstyle & (wxHSCROLL | wxVSCROLL))) {
    if (!theme_tried) {
      HMODULE hm;
      hm = LoadLibrary("UxTheme.dll");
      if (hm) {
	wxCloseThemeData = (wxCLOSE_THEME_DATA_PROC)GetProcAddress(hm, "CloseThemeData");
	wxOpenThemeData = (wxOPEN_THEME_DATA_PROC)GetProcAddress(hm, "OpenThemeData");
	wxDrawThemeBackground = (wxDRAW_THEME_BACKGROUND_PROC)GetProcAddress(hm, "DrawThemeBackground");
	wxDrawThemeEdge = (wxDRAW_THEME_EDGE_PROC)GetProcAddress(hm, "DrawThemeEdge");
      }
      theme_tried = 1;
    }

    if (wxOpenThemeData) {
      RECT wr;
      HDC hdc;
      HWND hWnd;
      long r;
      static HANDLE gray;
    
      hWnd = (HWND)handle;
    
      if (!control_theme) {
	control_theme = wxOpenThemeData(hWnd, L"Edit");
      }

      GetWindowRect(hWnd, &wr);
      
      r = DefWindowProc(WM_NCPAINT, wParam, lParam);
      *result = r;

      if (control_theme) {    
	hdc = GetDCEx(hWnd, (HRGN)wParam, DCX_WINDOW|DCX_INTERSECTRGN);

	wr.right -= wr.left;
	wr.bottom -= wr.top;
	wr.top = wr.left = 0;

#ifndef EP_EDITTEXT
# define EP_EDITTEXT 1
#endif
#ifndef ETS_NORMAL
# define ETS_NORMAL 1
#endif
#ifndef ETS_DISBALED
# define ETS_DISABLE 4
#endif
	wxDrawThemeBackground(control_theme, hdc,
			      EP_EDITTEXT, ETS_NORMAL /* or ETS_DISABLED */,
			      &wr, NULL);
	
	ReleaseDC(hWnd, hdc);
      }

      return 1;
    }
  }

  return 0;
}

void wxCanvasWnd::OnWinThemeChange()
{
  if (control_theme && wxCloseThemeData) {
    wxCloseThemeData(control_theme);
    control_theme = NULL;
  }
}

extern void MrEdQueuePaint(wxWindow *wx_window);

BOOL wxCanvasWnd::OnPaint(void)
{
  int retval = 0;

  if (wx_window) {
    HRGN tRgn;
    tRgn = CreateRectRgn(0,0,0,0);
    
    if (GetUpdateRgn(handle, tRgn, FALSE)) {
      PAINTSTRUCT ps;

      BeginPaint(handle, &ps);

      /* We used to call wx_window->OnPaint directly;
	 now we queue an event. */
      MrEdQueuePaint(wx_window);

      EndPaint(handle, &ps);
      cdc = NULL;
      
      retval = 1;
    }
    
    DeleteObject(tRgn);
  }

  return retval;
}
