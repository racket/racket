/*
 * File:	wx_win.cc
 * Purpose:	wxWindow class implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include <math.h>
#include <shellapi.h>
#include <windowsx.h>

#include "fafa.h"

extern "C" void scheme_start_atomic();
extern "C" void scheme_end_atomic_no_swap();

// Global variables
static wxWindow *current_mouse_wnd = NULL;
static void *current_mouse_context = NULL;

// Hook for new window just as it's being created,
// when the window isn't yet associated with the handle
wxWnd *wxWndHook = NULL;

extern long last_msg_time; /* timeStamp implementation */

extern int WM_IS_MRED;

static int sakc_initialized;

static void wxDoOnMouseLeave(wxWindow *wx_window, int x, int y, UINT flags);
static void wxDoOnMouseEnter(wxWindow *wx_window, int x, int y, UINT flags);

void wxWindowInit(void)
{
  wxREGGLOB(current_mouse_wnd);
  wxREGGLOB(current_mouse_context);
  wxREGGLOB(wxWndHook);
}

// Find an item given the MS Windows id
wxWindow *wxWindow::FindItem(int id)
{
  wxChildNode *current;

  if (!children)
    return NULL;
  current = children->First();
  while (current)
  {
    wxObject *obj;
    obj = (wxObject *)current->Data();
    if (wxSubType(obj->__type, wxTYPE_PANEL)) {
      // Do a recursive search.
      wxPanel *panel = (wxPanel*)obj;
      wxWindow *wnd;
      wnd = panel->FindItem(id);
      if (wnd)
        return wnd;
    } else if (wxSubType(obj->__type, wxTYPE_CANVAS)
	     || wxSubType(obj->__type, wxTYPE_TEXT_WINDOW)) {
      // Do nothing
    } else {
      wxItem *item;
      item = (wxItem *)current->Data();
      if (item->windows_id == id)
        return item;
      else {
	// In case it's a 'virtual' control (e.g. radiobox)
	if (item->subControls) {
	  int j;
	  for (j = item->numSubControls; j--; ) {
	    if (item->subControls[j] == id)
	      return item;
	  }
	}
      }
    }
    current = current->Next();
  }
  return NULL;
}

// Find an item given the MS Windows handle
wxWindow *wxWindow::FindItemByHWND(HWND hWnd)
{
  wxChildNode *current;

  if (!children)
    return NULL;
  current = children->First();
  while (current) {
    wxObject *obj;
    obj = (wxObject *)current->Data();
    if (wxSubType(obj->__type,wxTYPE_PANEL)) {
      // Do a recursive search.
      wxPanel *panel = (wxPanel*)obj;
      wxWindow *wnd;
      wnd = panel->FindItemByHWND(hWnd);
      if (wnd)
        return wnd;
    } else {
      wxItem *item;
      item = (wxItem *)current->Data();
      if ((HWND)(item->ms_handle) == hWnd)
        return item;
      else {
        // In case it's a 'virtual' control (e.g. radiobox)
        if (item->__type == wxTYPE_RADIO_BOX) {
          wxRadioBox *rbox = (wxRadioBox *)item;
          int i;
          for (i = 0; i < rbox->no_items; i++) {
            if (rbox->radioButtons[i] == hWnd)
              return item;
	  }
        }
      }
    }
    current = current->Next();
  }
  return NULL;
}

// Default command handler
BOOL wxWindow::MSWCommand(UINT WXUNUSED(param), WORD WXUNUSED(id))
{
  return FALSE;
}

void wxWindow::PreDelete(HDC WXUNUSED(dc))
{
}

HWND wxWindow::GetHWND(void)
{
  HWND hWnd = 0;

  switch (wxWinType)
  {
    case wxTYPE_XWND:
    case wxTYPE_MDICHILD:
    {
      wxWnd *wnd = (wxWnd *)handle;
      if (wnd)
        hWnd = (HWND)wnd->handle;
      break;
    }
    default:
    {
      hWnd = (HWND)ms_handle;
      break;
    }
  }
  return hWnd;
}

// Constructor

wxWindow::wxWindow(void)
{
  ms_handle = 0;
  handle = NULL;
  winEnabled = TRUE;
  windows_id = 0;
  winEnabled = TRUE;

  focusWindow = NULL;

  WXGC_IGNORE(this, focusWindow); // can be a self pointer in a frame
}

// Destructor
wxWindow::~wxWindow(void)
{
  if (current_mouse_wnd == this)
    current_mouse_wnd = NULL;

  if (window_parent)
    window_parent->RemoveChild(this);

  wxbWindow::DestroyChildren();
  switch (wxWinType)
  {
    case wxTYPE_XWND:
    {
      if (handle)
      {
        wxWnd *wnd = (wxWnd *)handle;
		  HDC dc;

	dc = wxwmGetDC(wnd->handle);
	PreDelete(dc);
	wxwmReleaseDC(wnd->handle, dc);

        wnd->DestroyWindow();
        delete wnd;
        handle = NULL;
      }
      break;
    }
    case wxTYPE_MDICHILD:
    {
      wxMDIChild *child = (wxMDIChild *)handle;
      child->DestroyWindow();
      delete child;
      handle = NULL;
      break;
    }
    case wxTYPE_HWND:
    {
      if (ms_handle) {
	wxwmDestroyWindow((HWND)ms_handle);
      }
      handle = NULL;
      
      if (wxControlHandleList)
        wxControlHandleList->DeleteObject(this);

      break;
    }
    default:
      break;
    }

  delete children;
  children = NULL;
}

wxWindow *wxWindow::GetTopLevel()
{
  wxWindow *p = this;
  while (p && !(wxSubType(p->__type, wxTYPE_FRAME)
		|| wxSubType(p->__type, wxTYPE_DIALOG_BOX))) {
    p = p->GetParent();
  }
  
  return p;
}

void wxWindow::SetFocus(void)
{
  wxWindow *p;
  int is_front = 0;

  if (!IsShownTree())
    return;

  if (!AcceptsExplicitFocus())
    return;

  p = GetTopLevel();
  
  if (p && wxSubType(p->__type, wxTYPE_FRAME)
      && (((wxFrame *)p)->frame_type == wxMDI_CHILD)) {
    wxWindow *mdip;
    DWORD r;

    mdip = p->GetParent();

    if (mdip && (GetActiveWindow() != mdip->GetHWND()))
      r = 0;
    else
      r = ::SendMessage(((wxMDIFrame *)mdip->handle)->client_hwnd, WM_MDIGETACTIVE,
			(WPARAM)NULL, (LPARAM)NULL);
 
    if ((HWND)r != p->GetHWND()) {
      /* This frame not active within parent; remember local focus */
      p->focusWindow = this;
      return;
    }
    is_front = 1;
  }
  
  // If the frame/dialog is not active, just set the focus
  //  locally.
  if (p) {
    p->focusWindow = this;
    
    if (is_front || (GetActiveWindow() == p->GetHWND())) {
      HWND hWnd;
      hWnd = GetHWND();
      if (hWnd) {
	wxwmSetFocus(hWnd);
      }
    }
  }
}

Bool wxWindow::AcceptsExplicitFocus()
{
  return TRUE;
}

/* Enable state flags:
     winEnabled = whether the specific window has been enabled or disabled;
                  implies graying
     internal_disable = whether window disable due to disabling of the
                        parent, or some other influence; counts up, but
                        rarely exceeds 1; doesn't necessary imply graying
     internal_gray_disable = like internal_disable, but implies graying
*/

void wxWindow::ChangeToGray(Bool gray)
{
  /* Nothing extra to do over enabling */
}

Bool wxWindow::IsGray(void)
{
  return !winEnabled || internal_gray_disabled;
}

void wxWindow::DoEnableWindow(int on)
{
  HWND hWnd;
  hWnd = GetHWND();
  if (hWnd)
    ::EnableWindow(hWnd, (BOOL)on); 
  if (!on) {
    wxWindow *p;
    p = GetTopLevel();
    if (p->focusWindow == this)
      p->SetFocus();
  }
}

void wxWindow::InternalEnable(Bool enable, Bool gray)
{
  Bool do_something;
  short start_igd = internal_gray_disabled;

  /* See state-flag docs above! */

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

  if (do_something && winEnabled) {
    DoEnableWindow((BOOL)enable);
  }

  if ((!!internal_gray_disabled != !!start_igd) && winEnabled)
    ChangeToGray(!!internal_gray_disabled);
}

void wxWindow::Enable(Bool enable)
{
  /* See state-flag docs above! */

  if (winEnabled == !!enable)
    return;

  winEnabled = enable;
  
  if (!internal_disabled) {
    DoEnableWindow((BOOL)enable);
  }

  /* Doing handle sensitive makes it gray: */
  if (!internal_gray_disabled)
    ChangeToGray(!enable);
}

void wxWindow::InternalGrayChildren(Bool gray)
{
  /* Called by ChangeToGray */
  wxChildNode *cn;
  wxChildList *cl;
  cl = GetChildren();
  for (cn = cl->First(); cn; cn = cn->Next()) {
    wxWindow *w;
    w = (wxWindow *)cn->Data();
    w->InternalEnable(!gray, TRUE);
  }
}

void wxWindow::InitEnable()
{
  /* Called when an child is created */
  /* See state-flag docs above! */

  wxWindow *p;
  p = GetParent();
  if (!p->winEnabled || p->internal_gray_disabled)
    InternalEnable(FALSE, TRUE);
}

void wxWindow::CaptureMouse(void)
{
  HWND hWnd;
  hWnd = GetHWND();
  if (hWnd && !winCaptured) {
    SetCapture(hWnd);
    winCaptured = TRUE;
  }
}

void wxWindow::ReleaseMouse(void)
{
  if (winCaptured) {
    ReleaseCapture();
    winCaptured = FALSE;
  }
}

void wxWindow::DragAcceptFiles(Bool accept)
{
  HWND hWnd;
  hWnd = GetHWND();
  if (hWnd)
    ::DragAcceptFiles(hWnd, (BOOL)accept);
}

// Get total size
void wxWindow::GetSize(int *x, int *y)
{
  RECT rect;
  HWND hWnd;

  hWnd = GetHWND();
  GetWindowRect(hWnd, &rect);
  *x = rect.right - rect.left;
  *y = rect.bottom - rect.top;
}

void wxWindow::GetPosition(int *x, int *y)
{
  HWND hWnd;
  HWND hParentWnd = 0;
  RECT rect;
  POINT point;
  wxWindow *par;

  hWnd = GetHWND();
  par = GetParent();
  if (par)
    hParentWnd = par->GetHWND();
  
  GetWindowRect(hWnd, &rect);

  // Since we now have the absolute screen coords,
  // if there's a parent we must subtract its top left corner
  point.x = rect.left;
  point.y = rect.top;
  if (hParentWnd)
    ::ScreenToClient(hParentWnd, &point);
  *x = point.x;
  *y = point.y;

  if (*x < -10000)
    *x = -10000;
  if (*y < -10000)
    *y = -10000;
}

void wxWindow::ScreenToClient(int *x, int *y)
{
  HWND hWnd;
  POINT pt;

  hWnd = GetHWND();
  pt.x = *x;
  pt.y = *y;
  ::ScreenToClient(hWnd, &pt);

  if (pt.x < -10000)
    pt.x = 10000;
  if (pt.x > 10000)
    pt.x = 10000;
  if (pt.y < -10000)
    pt.y = 10000;
  if (pt.y > 10000)
    pt.y = 10000;

  *x = pt.x;
  *y = pt.y;
}

void wxWindow::ClientToScreen(int *x, int *y)
{
  HWND hWnd;
  POINT pt;

  hWnd = GetHWND();
  pt.x = *x;
  pt.y = *y;
  ::ClientToScreen(hWnd, &pt);

  if (pt.x < -10000)
    pt.x = 10000;
  if (pt.x > 10000)
    pt.x = 10000;
  if (pt.y < -10000)
    pt.y = 10000;
  if (pt.y > 10000)
    pt.y = 10000;

  *x = pt.x;
  *y = pt.y;
}

HCURSOR wxMSWSetCursor(HCURSOR c)
{
  return SetCursor(c);
}

wxWindow *wxLocationToWindow(int x, int y)
{
  POINT p;
  HWND hwnd;

  p.x = x;
  p.y = y;

  hwnd = WindowFromPoint(p);

  if (hwnd) {
    wxWnd *wnd = NULL;
    while (hwnd) {
      wnd = wxFindWinFromHandle(hwnd);
      if (wnd)
	break;
      else
	hwnd = GetParent(hwnd);
    }

    if (wnd && wnd->wx_window)
      return wnd->wx_window->GetTopLevel();
  }

  return NULL;
}

static wxWnd *wxCurrentWindow(int in_content)
{
  HWND hwnd;
  wxWnd *wnd = NULL;

  hwnd = GetCapture();
  if (!hwnd) {
    POINT pos;
    if (!GetCursorPos(&pos))
      return NULL;
    
    hwnd = WindowFromPoint(pos);
  } else
    /* Grab => always considered inside: */
    in_content = 0;
  if (!hwnd)
    return NULL;

  while (hwnd) {
    wnd = wxFindWinFromHandle(hwnd);
    if (wnd)
      break;
    else
      hwnd = GetParent(hwnd);
  }

  if (wnd && in_content) {
    /* Check content vs. non-content area: */
    POINT pos;
    RECT wind;

    GetCursorPos(&pos);
    ScreenToClient(wnd->handle, &pos);

    GetClientRect(wnd->handle, &wind);

    if (!PtInRect(&wind, pos))
      return NULL;
  }

  return wnd;
}

void wxResetCurrentCursor(void)
{
  wxWnd *wnd;
  wxWindow *w;
  wxCursor *cursor = wxSTANDARD_CURSOR;

  wnd = wxCurrentWindow(1);
  if (!wnd) return;

  w = wnd->wx_window;

  while (w) {
    if (w->wx_cursor) {
      cursor = w->wx_cursor;
      break;
    }
    w = w->GetParent();
  }

  wxMSWSetCursor(cursor->ms_cursor);
}

wxCursor *wxWindow::SetCursor(wxCursor *cursor)
{
  wxCursor *old_cursor = wx_cursor;

  if (cursor && !cursor->Ok())
    return old_cursor;

  wx_cursor = cursor;

  if (!wxIsBusy())
    wxResetCurrentCursor();

  return old_cursor;
}

// Get size *available for subwindows* i.e. excluding menu bar etc.
// For XView, this is the same as GetSize
void wxWindow::GetClientSize(int *x, int *y)
{
  HWND hWnd;
  RECT rect;

  hWnd = GetHWND();
  GetClientRect(hWnd, &rect);
  *x = rect.right;
  *y = rect.bottom;
}

void wxWindow::SetSize(int x, int y, int width, int height, int WXUNUSED(sizeFlags))
{
  int currentX, currentY;
  int actualWidth = width;
  int actualHeight = height;
  int currentW,currentH;
  HWND hWnd;

  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  GetSize(&currentW, &currentH);
  if (width == -1)
    actualWidth = currentW;
  if (height == -1)
    actualHeight = currentH;

  hWnd = GetHWND();
  if (hWnd)
    MoveWindow(hWnd, x, y, actualWidth, actualHeight, (BOOL)TRUE);

  ((wxWnd *)handle)->OnSize(actualWidth, actualHeight, 0);
}

void wxWindow::SetClientSize(int width, int height)
{
  wxWindow *parent;
  HWND hWnd;
  HWND hParentWnd;
  RECT rect;
  RECT rect2;
  POINT point;
  int actual_width, actual_height;

  parent = GetParent();
  hWnd = GetHWND();
  hParentWnd = parent->GetHWND();

  GetClientRect(hWnd, &rect);

  GetWindowRect(hWnd, &rect2);

  // Find the difference between the entire window (title bar and all)
  // and the client area; add this to the new client size to move the
  // window
  actual_width = rect2.right - rect2.left - rect.right + width;
  actual_height = rect2.bottom - rect2.top - rect.bottom + height;

  // If there's a parent, must subtract the parent's top left corner
  // since MoveWindow moves relative to the parent

  point.x = rect2.left;
  point.y = rect2.top;
  if (parent)
  {
    ::ScreenToClient(hParentWnd, &point);
  }

  MoveWindow(hWnd, point.x, point.y, actual_width, actual_height, (BOOL)TRUE);
  OnSize(actual_width, actual_height);
}

Bool wxWindow::Show(Bool show)
{
  HWND hWnd;
  int cshow;

  SetShown(show);

  if (window_parent) {
    wxChildList *cl;
    cl = window_parent->GetChildren();
    cl->Show(this, show);
  }

  hWnd = GetHWND();
  if (show)
    cshow = SW_SHOW;
  else
    cshow = SW_HIDE;
  ShowWindow(hWnd, (BOOL)cshow);
  if (show && (__type != wxTYPE_GROUP_BOX) && (__type != wxTYPE_TAB_CHOICE))
    BringWindowToTop(hWnd);

  {
    wxWindow *p;
    p = GetTopLevel();
    if (p->focusWindow == this)
      p->focusWindow = NULL;
  }

  return TRUE;
}

static wxMemoryDC *measure_dc;

void wxWindow::GetTextExtent(const char *string, double *x, double *y,
			     double *descent, double *externalLeading, 
			     wxFont *theFont, Bool use16bit)
{
  wxFont *fontToUse = theFont;
  
  if (!fontToUse)
    fontToUse = GetFont();

  if (!measure_dc) {
    wxBitmap *bm;
    wxREGGLOB(measure_dc);
    bm = new wxBitmap(1, 1, 0);
    measure_dc = new wxMemoryDC();
    measure_dc->SelectObject(bm);
  }

  measure_dc->GetTextExtent(string, x, y, descent, externalLeading, fontToUse, 1, use16bit);
}

void wxWindow::Refresh(void)
{
  HWND hWnd;
  hWnd = GetHWND();
  if (hWnd)
  {
    ::InvalidateRect(hWnd, NULL, TRUE);
  }
}

wxWindow *wxWindow::FindFocusWindow()
{
  if (IsShown()) {
    wxChildNode *cn;
    wxChildList *cl;
    cl = GetChildren();
    for (cn = cl->First(); cn; cn = cn->Next()) {
      wxWindow *w;
      w = (wxWindow *)cn->Data();
      w = w->FindFocusWindow();
      if (w)
	return w;
    }
  }

  return NULL;
}

class wxDWP_Closure {
public:
  wxWnd *wnd;
  UINT message;
  WPARAM wParam;
  LPARAM lParam;
};

static int call_dwp(void *_c) {
  wxDWP_Closure *c;
  wxWnd *wnd;

  c = (wxDWP_Closure *)_c;
  wnd = c->wnd;

  return wnd->DefWindowProc(c->message, c->wParam, c->lParam);  
}

extern int wxHiEventTrampoline(int (*f)(void *), void *data);
extern void wxCopyData(LPARAM lparam);

extern int wx_start_win_event(const char *who, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam, int tramp, LONG *_retval);
extern void wx_end_win_event(const char *who, HWND hWnd, UINT message, int tramps);

// Main window proc
static LONG WindowProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam, int dialog, int tramp)
{
  LONG retval;
  wxWnd *wnd;

  if (dialog) {
    wnd = wxFindWinFromHandle(hWnd);
    retval = 1; /* dialog: handled */
  } else {
#ifdef MZ_PRECISE_GC
    wnd = wxFindWinFromHandle(hWnd);
#else
    wnd = (wxWnd *)GetWindowLong(hWnd, 0);
#endif
    retval = 0; /* most common expected result for windows */
  }

  if (!wnd) {
    if (wxWndHook) {
      wnd = wxWndHook;
      wnd->handle = hWnd;
    } else if (dialog) {
      return 0;
    } else {
      wnd = wxFindWinFromHandle(hWnd);
      if (!wnd)
	return ::DefWindowProcW(hWnd, message, wParam, lParam);
    }
  }

  /* Stop here if we don't have a valid handle in our wxWnd object. */
  if (!wnd->handle) {
    wnd->handle = hWnd;
    retval = wnd->DefWindowProc(message, wParam, lParam);
    wnd->handle = NULL;
    return retval;
  }

  if (!wx_start_win_event(dialog ? "dialog" : "window", hWnd, message, wParam, lParam, tramp, &retval)) {
    /* Something has gone wrong. Give up. */
    return retval;
  }


  if (!dialog && tramp) {
    switch (message) {
    case WM_NCLBUTTONDOWN:
    case WM_NCRBUTTONDOWN:
    case WM_NCMBUTTONDOWN:
    case WM_NCLBUTTONDBLCLK:
    case WM_NCRBUTTONDBLCLK:
    case WM_NCMBUTTONDBLCLK:
      if (wParam != HTMENU)
	break;
    case WM_SYSKEYUP: /* ^^^ fallthrough ^^^ */
      /* Guess that this could trigger a menu pop-up. */
      /* Pre-emptively simulate WM_INITMENU message. */
      wnd->OnMenuClick(NULL);
      break;
    }
  }

  wnd->last_msg = message;
  wnd->last_wparam = wParam;
  wnd->last_lparam = lParam;

  switch (message) {
  case WM_INPUTLANGCHANGE:
    sakc_initialized = 0;
    break;
  case WM_COPYDATA:
    wxCopyData(lParam);
    retval = 0;
    break;
  case WM_SETFONT:
    retval = 0;
    break;
  case WM_INITDIALOG:
    retval = 0;
    break;
  case WM_ACTIVATE:
    {
      WORD state = LOWORD(wParam);
      WORD minimized = HIWORD(wParam);
      HWND hwnd = (HWND)lParam;
      wnd->OnActivate(state, minimized, hwnd);
      if (dialog) retval = 0;
      break;
    }
  case WM_NCACTIVATE:
    {
      WORD state = LOWORD(wParam);
      wnd->OnNCActivate(state, hWnd);
      retval = wnd->DefWindowProc(message, wParam, lParam);
      break;
    }
  case WM_SETFOCUS:
    {
      HWND hwnd = (HWND)wParam;
      wnd->OnSetFocus(hwnd);
      break;
    }
  case WM_KILLFOCUS:
    {
      HWND hwnd = (HWND)lParam;
      wnd->OnKillFocus(hwnd);
      break;
    }
  case WM_CREATE:
    {
      wnd->OnCreate((LPCREATESTRUCT)lParam);
      if (dialog) retval = 0;
      break;
    }
  case WM_PAINT:
    {
      if (!wnd->OnPaint()) {
	if (dialog)
	  retval = ::DefWindowProcW(hWnd, message, wParam, lParam);
	else
	  retval = wnd->DefWindowProc(message, wParam, lParam);
      } else if (dialog)
	retval = 0;
      break;
    }
  case WM_QUERYDRAGICON:
    {
      HICON hIcon;
      hIcon = wnd->OnQueryDragIcon();
      if (hIcon)
	retval = (LONG)hIcon;
      else 
	retval = wnd->DefWindowProc(message, wParam, lParam);
      break;
    }
  case WM_SIZE:
    if (wParam == SIZE_MINIMIZED)
      break;
  case WM_MOVE:
    {
      /* w & h ignored... */
      wnd->OnSize(0, 0, wParam);
      if (dialog) retval = 0;
      break;
    }
  case WM_GETMINMAXINFO:
    wnd->GetMinMaxInfo((MINMAXINFO *)lParam);
    break;
  case WM_RBUTTONDOWN:
  case WM_RBUTTONUP:
  case WM_RBUTTONDBLCLK:
  case WM_MBUTTONDOWN:
  case WM_MBUTTONUP:
  case WM_MBUTTONDBLCLK:
  case WM_LBUTTONDOWN:
  case WM_LBUTTONUP:
  case WM_LBUTTONDBLCLK:
    {
      int x = (short)LOWORD(lParam);
      int y = (short)HIWORD(lParam);
      int et;

      switch(message) {
      case WM_RBUTTONDOWN:
      case WM_RBUTTONDBLCLK:
	et = wxEVENT_TYPE_RIGHT_DOWN;
	break;
      case WM_RBUTTONUP:
	et = wxEVENT_TYPE_RIGHT_UP;
	break;
      case WM_MBUTTONDOWN:
      case WM_MBUTTONDBLCLK:
	et = wxEVENT_TYPE_MIDDLE_DOWN;
	break;
      case WM_MBUTTONUP:
	et = wxEVENT_TYPE_MIDDLE_UP;
	break;
      case WM_LBUTTONDOWN:
      case WM_LBUTTONDBLCLK:
	et = wxEVENT_TYPE_LEFT_DOWN;
	break;
      case WM_LBUTTONUP:
	et = wxEVENT_TYPE_LEFT_UP;
	break;
      }

      wnd->OnButton(x, y, wParam, et);
      break;
    }
  case WM_NCLBUTTONDOWN:
  case WM_NCRBUTTONDOWN:
  case WM_NCMBUTTONDOWN:
  case WM_NCLBUTTONDBLCLK:
  case WM_NCRBUTTONDBLCLK:
  case WM_NCMBUTTONDBLCLK:
  case WM_NCLBUTTONUP:
  case WM_NCRBUTTONUP:
  case WM_NCMBUTTONUP:
    if ((wParam == HTVSCROLL) || (wParam == HTHSCROLL)) {
	/* Fall through below */
      int x = (short)LOWORD(lParam);
      int y = (short)HIWORD(lParam);
      int et;

      switch(message) {
      case WM_NCRBUTTONDOWN:
      case WM_NCRBUTTONDBLCLK:
	et = wxEVENT_TYPE_RIGHT_DOWN;
	break;
      case WM_NCRBUTTONUP:
	et = wxEVENT_TYPE_RIGHT_UP;
	break;
      case WM_NCMBUTTONDOWN:
      case WM_NCMBUTTONDBLCLK:
	et = wxEVENT_TYPE_MIDDLE_DOWN;
	break;
      case WM_NCMBUTTONUP:
	et = wxEVENT_TYPE_MIDDLE_UP;
	break;
      case WM_NCLBUTTONDOWN:
      case WM_NCLBUTTONDBLCLK:
	et = wxEVENT_TYPE_LEFT_DOWN;
	break;
      case WM_NCLBUTTONUP:
	et = wxEVENT_TYPE_LEFT_UP;
	break;
      }

      if (!wnd->OnButton(x, y, 0, et, 1)) {
	retval = 1;
	break;
      }
    }
     
    if (dialog)
      retval = 0;
    else {
      if (tramp 
	  && ((wParam == HTVSCROLL) || (wParam == HTHSCROLL))
	  && wnd->wx_window
	  && wxSubType(wnd->wx_window->__type, wxTYPE_CANVAS)) {
	/* To support interactive scrolling in canvases, we
	   use a trampoline. See wxHiEventTrampoline in mred.cxx. */
	wxDWP_Closure *c;
	c = new wxDWP_Closure;
	c->wnd = wnd;
	c->message = message;
	c->wParam = wParam;
	c->lParam = lParam;
	retval = wxHiEventTrampoline(call_dwp, (void *)c);
      } else {
	retval = wnd->DefWindowProc(message, wParam, lParam);
      }
    }
    break;
  case WM_MOUSEMOVE:
    {
      int x = (short)LOWORD(lParam);
      int y = (short)HIWORD(lParam);
      wnd->OnMouseMove(x, y, wParam);
      break;
    }
  case WM_NCMOUSEMOVE:
    {
      int x = (short)LOWORD(lParam);
      int y = (short)HIWORD(lParam);
      if (!wnd->OnMouseMove(x, y, wParam, 1)) {
	retval = 0;
	break;
      }
      
      if (dialog)
	retval = 1;
      else
	retval = wnd->DefWindowProc(message, wParam, lParam);
      break;
    }
#ifndef WM_MOUSEWHEEL
# define WM_MOUSEWHEEL 0x020A
# define WHEEL_DELTA 120
#endif
  case WM_MOUSEWHEEL:
    {
      int delta = ((short)HIWORD(wParam)) / WHEEL_DELTA;
      while (delta) {
	WORD code;
	if (delta < 0) {
	  code = WXK_WHEEL_DOWN;
	  delta++;
	} else {
	  code = WXK_WHEEL_UP;
	  delta--;
	}
	wnd->OnChar(code, lParam, 1);
      }
      break;
    }
  case WM_DESTROY:
    {
      if (!wnd->OnDestroy())
	retval = wnd->DefWindowProc(message, wParam, lParam);
      break;
    }
  case WM_COMMAND:
    {
      WORD id = LOWORD(wParam);
      HWND hwnd = (HWND)lParam;
      WORD cmd = HIWORD(wParam);
      if (!wnd->OnCommand(id, cmd, hwnd))
	retval = wnd->DefWindowProc(message, wParam, lParam);
      else if (dialog) 
	retval = 0;
      break;
    }
  case WM_INITMENU:
    {
      if (dialog)
	retval = 0;
      else {
	/* We skip the call to OnMenuClick() because it's handled
	   by WM_SYSKEYUP. WM_INITMENU seems to happen too late,
	   at a point where we can't execute Scheme code. */
	// wnd->OnMenuClick(wParam);
      }
      break;
    }
  case WM_MENUSELECT:
    {
      if (dialog)
	retval = 0;
      else {
	// WORD id = LOWORD(wParam);
	WORD flags = HIWORD(wParam);
	HMENU sysmenu = (HMENU)lParam;
	wnd->OnMenuSelect((WORD)wParam, flags, sysmenu);
      }
      break;
    }
  case WM_NOTIFY:
    {
      NMHDR *nm = (NMHDR *)lParam;
      if (!nm || !wnd->OnCommand(LOWORD(wParam), nm->code, nm->hwndFrom))
	retval = wnd->DefWindowProc(message, wParam, lParam);
      else if (dialog) 
	retval = 0;
    }
    break;
  case WM_SYSKEYDOWN:
    if ((wParam == VK_MENU) || (wParam == VK_F4)) { /* F4 is close */
      wxUnhideCursor();
      retval = wnd->DefWindowProc(message, wParam, lParam);
    }
  case WM_KEYUP:   /* ^^^ fallthrough */
  case WM_KEYDOWN: /* ^^^ fallthrough */
    {
      wnd->OnChar((WORD)wParam, lParam, FALSE, message == WM_KEYUP);
      break;
    }
  case WM_SYSCHAR:
    if (wParam == VK_MENU) {
      wxUnhideCursor();
      retval = wnd->DefWindowProc(message, wParam, lParam);
    }
  case WM_CHAR: /* ^^^ fallthrough */
    {
      wnd->OnChar((WORD)wParam, lParam, TRUE);
      break;
    }
  case WM_HSCROLL:
  case WM_VSCROLL:
    {
      WORD code = LOWORD(wParam);
      WORD pos = HIWORD(wParam);
      HWND control = (HWND)lParam;
      if (message == WM_HSCROLL)
	wnd->OnHScroll(code, pos, control);
      else
	wnd->OnVScroll(code, pos, control);
      break;
    }
  case WM_ERASEBKGND:
    {
      // Prevents flicker when dragging
      if (!dialog && IsIconic(hWnd))
	retval = 1;
      else if (!wnd->OnEraseBkgnd((HDC)wParam))
	retval = wnd->DefWindowProc(message, wParam, lParam);
      else
	retval = 1;
      break;
    }
  case WM_MDIACTIVATE:
    {
      HWND hWndActivate = GET_WM_MDIACTIVATE_HWNDACTIVATE(wParam,lParam);
      HWND hWndDeactivate = GET_WM_MDIACTIVATE_HWNDDEACT(wParam,lParam);
      BOOL activate = GET_WM_MDIACTIVATE_FACTIVATE(hWnd,wParam,lParam);
      retval = wnd->OnMDIActivate(activate, hWndActivate, hWndDeactivate);
      break;
    }
  case WM_DROPFILES:
    {
      wnd->OnDropFiles(wParam);
      break;
    }
  case WM_QUERYENDSESSION:
    retval = 1;
    break;
  case WM_ENDSESSION:
  case WM_CLOSE:
    {
      retval = 0;
      if (wnd->wx_window && wnd->wx_window->IsShown()) {
	if (wnd->OnClose()) {
	  wnd->wx_window->Show(FALSE);
	}
      }
      break;
    }
#ifndef WM_THEMECHANGED
# define WM_THEMECHANGED 0x031A
#endif
  case WM_THEMECHANGED:
    {
      wnd->OnWinThemeChange();
      if (dialog)
	retval = 0;
      else
	retval = wnd->DefWindowProc(message, wParam, lParam);
    }
  case WM_NCPAINT:
    {
      if (wnd->NCPaint(wParam, lParam, &retval))
	break;
    }
  default: /* ^^^ fallthrough ^^^ */
    {
      if (message == WM_IS_MRED) {
	retval = 79;
      } else if (dialog)
	retval = 0;
      else
	retval = wnd->DefWindowProc(message, wParam, lParam);
      break;
    }
  }
  
  wnd->Propagate(message, wParam, lParam);
	
  wx_end_win_event(dialog ? "dialog" : "window", hWnd, message, tramp);

  return retval;
}


extern DWORD wx_original_thread_id;
static int cut_off_callbacks;

void wxNoMoreCallbacks(void)
{
  cut_off_callbacks = 1;
}

static int invalid_callback_context()
{
  if (GetCurrentThreadId() != wx_original_thread_id) {
    return 1;
  }
  if (cut_off_callbacks)
    return 1;

  return 0;
}

extern int wx_trampolining;

// Main window proc
LRESULT APIENTRY wxWndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{ 
  LRESULT res;
  int tramp = wx_trampolining;

  if (invalid_callback_context())
    return ::DefWindowProcW(hWnd, message, wParam, lParam);

  wx_trampolining = 0;

  /* WM_NCHITTEST is extremely common, and we do nothing with it.
     Make handling fast, just in case. */
  if (message == WM_NCHITTEST) {
    return ::DefWindowProcW(hWnd, message, wParam, lParam);
  }

  /* See mredmsw.cxx: */
  if (!tramp)
    if (wxEventTrampoline(hWnd, message, wParam, lParam, &res, wxWndProc))
      return res;

  if (!tramp)
    scheme_start_atomic();
  res = WindowProc(hWnd, message, wParam, lParam, 0, tramp);
  if (!tramp)
    scheme_end_atomic_no_swap();

  return res;
}

// Dialog window proc
LONG APIENTRY wxDlgProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  LRESULT res;
  int tramp = wx_trampolining;

  if (invalid_callback_context())
    return ::DefWindowProcW(hWnd, message, wParam, lParam);

  wx_trampolining = 0;

  /* See mredmsw.cxx: */
  if (!tramp)
    if (wxEventTrampoline(hWnd, message, wParam, lParam, &res, wxDlgProc))
      return res;

  if (!tramp)
    scheme_start_atomic();
  res = WindowProc(hWnd, message, wParam, lParam, 1, tramp);
  if (!tramp)
    scheme_end_atomic_no_swap();

  return res;
}

wxNonlockingHashTable *wxWinHandleList = NULL;
extern wxNonlockingHashTable *wxSliderList;

wxWnd *wxFindWinFromHandle(HWND hWnd)
{
  return (wxWnd *)wxWinHandleList->Find((long)hWnd);
}

/* wxWnd class used to implement all Windows 3 windows
 */
wxWnd::wxWnd(void)
{
  x_scrolling_enabled = TRUE;
  y_scrolling_enabled = TRUE;
  calcScrolledOffset = TRUE;
  last_msg = 0;
  last_wparam = 0;
  last_lparam = 0;
  hMenu = 0;

  xscroll_pixels_per_line = 0;
  yscroll_pixels_per_line = 0;
  xscroll_lines = 0;
  yscroll_lines = 0;
  xscroll_lines_per_page = 0;
  yscroll_lines_per_page = 0;
  xscroll_position = 0;
  yscroll_position = 0;

  last_x_pos = -1;
  last_y_pos = -1;
  last_event = -1;
  is_canvas = FALSE;
  cdc = NULL;
  ldc = NULL;
  dc_count = 0;
}

wxWnd::~wxWnd(void)
{
  wxWinHandleList->DeleteObject(this);

  if (wx_window) {
    wxWindow *p;
    p = wx_window->GetTopLevel();
    if (p->focusWindow == wx_window)
      p->focusWindow = NULL;
    wx_window = NULL;
  }
}

HDC wxWnd::GetHDC(void)
{
  if (cdc)
    return cdc;
  if (dc_count==0) {
    ldc = wxwmGetDC(handle);
  }
  dc_count++;
  return ldc;
}

void wxWnd::ReleaseHDC(void)
{
  if (cdc)
    return;
  dc_count--;
  if (dc_count==0)
    wxwmReleaseDC(handle,ldc);
  if (dc_count < 0)
    dc_count = 0;
}

// Default destroyer - override if you destroy it in some other way
// (e.g. with MDI child windows)
void wxWnd::DestroyWindow(void)
{
  HWND oldHandle = handle;

  DetachWindowMenu();
#ifndef MZ_PRECISE_GC
  SetWindowLong(handle, 0, (long)0);
#endif
  handle = NULL;

  wxwmDestroyWindow(oldHandle);
}

extern HICON wxSTD_FRAME_ICON;

void wxWnd::Create(wxWnd *parent, char *wclass, wxWindow *wx_win, char *title,
		   int x, int y, int width, int height,
		   DWORD style, char *dialog_template, DWORD extendedStyle)
{
  RECT parent_rect;
  int x1 = 0;
  int y1 = 0;
  int w2 = 5;
  int h2 = 5;
  HWND hParent = NULL;

  WXGC_IGNORE(this, wx_window);

  wx_window = wx_win;
  if (wx_window)
    wx_window->handle = (char *)this;
    
  is_dialog = (dialog_template != NULL);

  if (!parent) {
    x1 = y1 = CW_USEDEFAULT;
  }

  // Find parent's size, if it exists, to set up a possible default
  // panel size the size of the parent window
  if (parent) {
    // Was GetWindowRect: JACS 5/5/95
    GetClientRect(parent->handle, &parent_rect);

    // Convert from screen coordinates to parent coordinates
    w2 = parent_rect.right - parent_rect.left;
    h2 = parent_rect.bottom - parent_rect.top;
  }

  if ((x != wxDEFAULT_POSITION) || (y != wxDEFAULT_POSITION)) {
    if (x != wxDEFAULT_POSITION)
      x1 = x;
    else
      x1 = 0;
    if (y != wxDEFAULT_POSITION)
      y1 = y;
    else
      y1 = 0;
  }
  if (width > -1) w2 = width;
  if (height > -1) h2 = height;

  if (parent)
    hParent = parent->handle;

  wxWndHook = this;

  if (is_dialog) {
    /* Creating a dialog */
    wchar_t *ws;
    ws = wxWIDE_STRING(dialog_template);
    handle = ::CreateDialogW(wxhInstance, ws, hParent,
			     (DLGPROC)wxDlgProc);
    
    if (handle == 0) {
      char buf[300];
      sprintf(buf, "Can't create dialog from %s (%u)!",
	      dialog_template, GetLastError());
      wxFatalError(buf, "Fatal wxWindows Error");
    }
  } else {
    /* Creating a non-dialog */
    {
      wchar_t *ws, *ws2;
      ws = wxWIDE_STRING_COPY(wclass);
      ws2 = wxWIDE_STRING(title);
      handle = CreateWindowExW(extendedStyle, 
			       ws,
			       ws2,
			       style,
			       x1, y1,
			       w2, h2,
			       hParent, NULL, wxhInstance,
			       NULL);
    }
    
    if (handle == 0) {
      char buf[300];
      sprintf(buf, "Can't create window of class %s (%u)!",
	      wclass, GetLastError());
      wxFatalError(buf, "Fatal wxWindows Error");
    }
  }

  wxWndHook = NULL;
  wxWinHandleList->Append((long)handle, this);

  if (is_dialog) {
    if (!parent) {
      /* Install PLT icon: */
      if (wxTheApp->wx_frame)
	SendMessage(handle, WM_SETICON, (WORD)TRUE, (DWORD)wxSTD_FRAME_ICON);
    }
    MoveWindow(handle, x1, y1, w2, h2, FALSE);
  } else {
#ifndef MZ_PRECISE_GC
    // Only for non-dialogs:
    SetWindowLong(handle, 0, (long)this);
#endif
  }
}

void wxWnd::OnCreate(LPCREATESTRUCT WXUNUSED(cs))
{
}

BOOL wxWnd::OnPaint(void)
{
  return 1;
}

BOOL wxWnd::OnClose(void)
{
  return FALSE;
}

BOOL wxWnd::OnDestroy(void)
{
  return TRUE;
}

void wxWnd::OnSize(int WXUNUSED(x), int WXUNUSED(y), UINT WXUNUSED(flag))
{
}

// Deal with child commands from buttons etc.

BOOL wxWnd::OnCommand(WORD WXUNUSED(id), WORD WXUNUSED(cmd), HWND WXUNUSED(control))
{
  return FALSE;
}

void wxWnd::OnMenuSelect(WORD WXUNUSED(item), WORD WXUNUSED(flags), HMENU WXUNUSED(sysmenu))
{
}

void wxWnd::OnMenuClick(WPARAM mnu)
{
}

BOOL wxWnd::NCPaint(WPARAM wParam, LPARAM lParam, LONG *result)
{
  return FALSE;
}

void wxWnd::OnWinThemeChange()
{
}

BOOL wxWnd::OnActivate(BOOL state, BOOL minimized, HWND WXUNUSED(activate))
{
  if (wx_window)
  {
    if ((state == WA_ACTIVE) || (state == WA_CLICKACTIVE)) {
      if (minimized) return TRUE; /* Ignore spurious activate while iconized */

      if (!wx_window->focusWindow) {
	/* Try to find one... */
	wx_window->focusWindow = wx_window->FindFocusWindow();
      }

      if (wx_window->focusWindow) {
	wxWindow *win = wx_window->focusWindow;
	wx_window->focusWindow = NULL;
	win->SetFocus();
      }
    }

    wx_window->OnActivate(((state == WA_ACTIVE) 
			   || (state == WA_CLICKACTIVE)));

    return 0;
  } else 
    return TRUE;
}

BOOL wxWnd::OnSetFocus(HWND WXUNUSED(hwnd))
{
  if (wx_window) {
    if (wx_window->IsShownTree()) {
      wxWindow *p;
      p = wx_window->GetTopLevel();
      p->focusWindow = wx_window;
      
      wx_window->OnSetFocus();
    }
    
    return TRUE;
  } else 
    return FALSE;
}

BOOL wxWnd::OnKillFocus(HWND WXUNUSED(hwnd))
{
  if (wx_window) {
    wx_window->OnKillFocus();

    return TRUE;
  } else 
    return FALSE;
}

void wxWnd::OnDropFiles(WPARAM wParam)
{
  HDROP hFilesInfo = (HDROP)wParam;
  POINT dropPoint;
  WORD gwFilesDropped;
  char **files, *a_file;
  wchar_t *w_file;
  int wIndex, len;

  DragQueryPoint(hFilesInfo, (LPPOINT) &dropPoint);

  // Get the total number of files dropped
  gwFilesDropped = (WORD)DragQueryFile((HDROP)hFilesInfo,
				       (UINT)-1,
				       (LPSTR)0,
				       (UINT)0);

  files = new char *[gwFilesDropped];

  for (wIndex=0; wIndex < (int)gwFilesDropped; wIndex++) {
    len = DragQueryFileW(hFilesInfo, wIndex, NULL, 0);
    w_file = new WXGC_ATOMIC wchar_t[len + 1];
    DragQueryFileW(hFilesInfo, wIndex, w_file, len + 1);
    a_file = wxNARROW_STRING(w_file);
    files[wIndex] = a_file;
  }
  DragFinish (hFilesInfo);

  if (wx_window)
    for (wIndex=0; wIndex < (int)gwFilesDropped; wIndex++) {
      wx_window->OnDropFile(files[wIndex]);
    }
}

void wxWnd::OnVScroll(WORD WXUNUSED(code), WORD WXUNUSED(pos), HWND WXUNUSED(control))
{
}

void wxWnd::OnHScroll(WORD WXUNUSED(code), WORD WXUNUSED(pos), HWND WXUNUSED(control))
{
}

void wxWnd::CalcScrolledPosition(int x, int y, int *xx, int *yy)
{
  *xx = (calcScrolledOffset ? (x - xscroll_position * xscroll_pixels_per_line) : x);
  *yy = (calcScrolledOffset ? (y - yscroll_position * yscroll_pixels_per_line) : y);
}

void wxWnd::CalcUnscrolledPosition(int x, int y, double *xx, double *yy)
{
  *xx = (double)(calcScrolledOffset ? (x + xscroll_position * xscroll_pixels_per_line) : x);
  *yy = (double)(calcScrolledOffset ? (y + yscroll_position * yscroll_pixels_per_line) : y);
}

BOOL wxWnd::OnEraseBkgnd(HDC WXUNUSED(pDC))
{
  return FALSE;
}

LONG wxWnd::DefWindowProc(UINT nMsg, WPARAM wParam, LPARAM lParam)
{
  return ::DefWindowProcW(handle, nMsg, wParam, lParam);
}

LONG wxWnd::Propagate(UINT nMsg, WPARAM wParam, LPARAM lParam)
{
  /* Used for MDI */
  return 0;
}

BOOL wxWnd::ProcessMessage(MSG* WXUNUSED(pMsg))
{
  return FALSE;
}

BOOL wxWnd::OnMDIActivate(BOOL WXUNUSED(flag), HWND WXUNUSED(activate), HWND WXUNUSED(deactivate))
{
  return 1;
}

BOOL wxWnd::OnNCActivate(BOOL WXUNUSED(flag), HWND WXUNUSED(activate))
{
  return 1;
}

void wxWnd::DetachWindowMenu(void)
{
  if (hMenu)
  {
    int N;
    int i;
    N = GetMenuItemCount(hMenu);
    for (i = 0; i < N; i++) {
      char buf[100];
      int chars;
      chars = GetMenuString(hMenu, i, buf, 100, MF_BYPOSITION);
      if ((chars > 0) && (strcmp(buf, "&Window") == 0)) {
        RemoveMenu(hMenu, i, MF_BYPOSITION);
        break;
      }
    }
  }
}

void wxWnd::GetMinMaxInfo(MINMAXINFO *mmi)
{
  /* Leave it alone */
}

/*
 * Subwindow - used for panels and canvases
 *
 */

wxSubWnd::wxSubWnd(wxWnd *parent, char *wclass, wxWindow *wx_win,
		   int x, int y, int width, int height,
		   DWORD style, char *dialog_template,
		   DWORD extendedStyle)
{
  Create(parent, wclass, wx_win, NULL, x, y, width, height, style, 
	 dialog_template, extendedStyle);
}

wxSubWnd::~wxSubWnd(void)
{
}


BOOL wxSubWnd::OnPaint(void)
{
  return FALSE;
}

void wxSubWnd::OnSize(int bad_w, int bad_h, UINT WXUNUSED(flag))
{
  if (!handle)
    return;

  if (wx_window && wxSubType(wx_window->__type, wxTYPE_CANVAS)) {
    wxCanvas * c;
    wxDC *dc;
    c = (wxCanvas *)wx_window;
    dc = c->GetDC();
    if (dc)
      dc->ReleaseGraphics();
  }

  if (calcScrolledOffset) {
    if ((xscroll_lines > 0) || (yscroll_lines > 0)) {
      wxCanvas * c;
      c = (wxCanvas *)wx_window;
      if (c) {
	c->SetScrollbars(c->horiz_units, c->vert_units,
			 xscroll_lines, yscroll_lines,
			 xscroll_lines_per_page, yscroll_lines_per_page,
			 xscroll_position, yscroll_position, TRUE);
      }
    }
  }

  if (wx_window)
    wx_window->OnSize(bad_w, bad_h);
}

// Deal with child commands from buttons etc.
BOOL wxSubWnd::OnCommand(WORD id, WORD cmd, HWND WXUNUSED(control))
{
  wxWindow *item;
  item = wx_window->FindItem(id);
  if (item) {
    return item->MSWCommand(cmd, id);
  } else
    return FALSE;
}

int wxWnd::OnButton(int x, int y, UINT flags, int evttype, int for_nc)
{
  wxMouseEvent *event;

  event = new wxMouseEvent(evttype);

  event->x = x;
  event->y = y;

  event->shiftDown = (flags & MK_SHIFT);
  event->controlDown = (flags & MK_CONTROL);
  event->leftDown = (flags & MK_LBUTTON);
  event->middleDown = (flags & MK_MBUTTON);
  event->rightDown = (flags & MK_RBUTTON);
  {
    int cd;
    cd = (::GetKeyState(VK_CAPITAL) >> 1);
    event->capsDown = cd;
  }
  event->SetTimestamp(last_msg_time);

  if (!for_nc && wx_window && (is_canvas || is_panel)) {
    if ((evttype == wxEVENT_TYPE_LEFT_DOWN)
	|| (evttype == wxEVENT_TYPE_MIDDLE_DOWN)
	|| (evttype == wxEVENT_TYPE_RIGHT_DOWN))
      wx_window->CaptureMouse();
    if ((evttype == wxEVENT_TYPE_LEFT_UP)
	|| (evttype == wxEVENT_TYPE_MIDDLE_UP)
	|| (evttype == wxEVENT_TYPE_RIGHT_UP))
      wx_window->ReleaseMouse();
  }

  last_x_pos = event->x; last_y_pos = event->y; last_event = evttype;
  if (wx_window)
    if (!wx_window->CallPreOnEvent(wx_window->PreWindow(), event)) {
      if (for_nc)
	return 1;
      else if (!wx_window->IsGray())
	wx_window->OnEvent(event);
    } else
      return 0;

  return 1;
}

static wxWindow *el_PARENT(wxWindow *w)
{
  /* Don't follow frame-parent hierarchy: */
  if (wxSubType(w->__type, wxTYPE_FRAME)
      || wxSubType(w->__type, wxTYPE_DIALOG_BOX))
    return NULL;

  return w->GetParent();
}

int wxCheckMousePosition()
{
  if (current_mouse_wnd && !wxCurrentWindow(0)) {
    wxWindow *imw;

    for (imw = current_mouse_wnd; imw; imw = el_PARENT(imw)) {
      wxQueueLeaveEvent(current_mouse_context, imw, -10, -10, 0);
    }

    current_mouse_wnd = NULL;
    current_mouse_context = NULL;

    return 1;
  }

  return 0;
}

void wxDoLeaveEvent(wxWindow *w, int x, int y, int flags)
{
  wxDoOnMouseLeave(w, x, y, flags);
}

void wxEntered(wxWindow *mw, int x, int y, int flags)
{
  wxWindow *imw, *join, *nextw;
  void *curr_context;
  POINT glob, pos;
  wxWindow *mouse_wnd;
  void *mouse_context;

  curr_context = wxGetContextForFrame();

  glob.x = x;
  glob.y = y;
  ::ClientToScreen(mw->GetHWND(), &glob);
  
  mouse_wnd = current_mouse_wnd;
  mouse_context = current_mouse_context;

  current_mouse_wnd = NULL;
  current_mouse_context = NULL;

  join = mouse_wnd;
  while (join) {
    for (imw = mw; imw; imw = el_PARENT(imw)) {
      if (join == imw)
	break;
    }
    if (join == imw)
      break;
    join = el_PARENT(join);
  }
  
  /* Leave old window(s) */
  for (imw = mouse_wnd; imw != join; imw = el_PARENT(imw)) {
    pos = glob;
    ::ScreenToClient(imw->GetHWND(), &pos);
    if (mouse_context == curr_context)
      wxDoOnMouseLeave(imw, pos.x, pos.y, flags);
    else
      wxQueueLeaveEvent(mouse_context, imw, pos.x, pos.y, flags);
  }
  
  /* Enter new window(s) - outside to inside */
  while (join != mw) {
    imw = mw;
    for (nextw = el_PARENT(imw); nextw != join; nextw = el_PARENT(nextw)) {
      imw = nextw;
    }
    pos = glob;
    ::ScreenToClient(imw->GetHWND(), &pos);
    wxDoOnMouseEnter(imw, pos.x, pos.y, flags);
    join = imw;
  }

  if (!current_mouse_wnd) {
    current_mouse_wnd = mw;
    current_mouse_context = curr_context;
  }
}

int wxWnd::OnMouseMove(int x, int y, UINT flags, int for_nc)
{
  wxMouseEvent *event;

  if (wxIsBusy())
    wxMSWSetCursor(wxHOURGLASS_CURSOR->ms_cursor);
  else
    wxResetCurrentCursor();

  /* Check mouse-position based stuff */
  if (wx_window)
    wxEntered(wx_window, x, y, flags);
  else {
    /* Could be status line... */
    /* We'd like to re-dispatch to the frame... */
  }

  event = new wxMouseEvent(wxEVENT_TYPE_MOTION);

  event->x = x;
  event->y = y;

  event->shiftDown = (flags & MK_SHIFT);
  event->controlDown = (flags & MK_CONTROL);
  event->leftDown = (flags & MK_LBUTTON);
  event->middleDown = (flags & MK_MBUTTON);
  event->rightDown = (flags & MK_RBUTTON);
  {
    int cd;
    cd = (::GetKeyState(VK_CAPITAL) >> 1);
    event->capsDown = cd;
  }
  event->SetTimestamp(last_msg_time);

  // Window gets a click down message followed by a mouse move
  // message even if position isn't changed!  We want to discard
  // the trailing move event if x and y are the same.
  if ((last_event == wxEVENT_TYPE_RIGHT_DOWN || last_event == wxEVENT_TYPE_LEFT_DOWN ||
       last_event == wxEVENT_TYPE_MIDDLE_DOWN) &&
      (last_x_pos == event->x && last_y_pos == event->y)) {
    last_x_pos = event->x; last_y_pos = event->y;
    last_event = wxEVENT_TYPE_MOTION;
    return 1;
  }

  last_event = wxEVENT_TYPE_MOTION;
  last_x_pos = event->x; last_y_pos = event->y;
  if (wx_window) 
    if (!wx_window->CallPreOnEvent(wx_window->PreWindow(), event)) {
      if (for_nc)
	return 1;
      else if (!wx_window->IsGray())
	wx_window->OnEvent(event);
    } else
      return 0;

  return 1;
}

void wxWnd::OnMouseEnter(int x, int y, UINT flags)
{
  if (wx_window)
    wxDoOnMouseEnter(wx_window, x, y, flags);
}

static void wxDoOnMouseEnter(wxWindow *wx_window, int x, int y, UINT flags)
{
  wxMouseEvent *event;

  event = new wxMouseEvent(wxEVENT_TYPE_ENTER_WINDOW);

  event->x = x;
  event->y = y;

  event->shiftDown = (flags & MK_SHIFT);
  event->controlDown = (flags & MK_CONTROL);
  event->leftDown = (flags & MK_LBUTTON);
  event->middleDown = (flags & MK_MBUTTON);
  event->rightDown = (flags & MK_RBUTTON);
  {
    int cd;
    cd = (::GetKeyState(VK_CAPITAL) >> 1);
    event->capsDown = cd;
  }
  event->SetTimestamp(last_msg_time);

  if (!wx_window->CallPreOnEvent(wx_window->PreWindow(), event))
    if (!wx_window->IsGray())
      wx_window->OnEvent(event);
}

void wxWnd::OnMouseLeave(int x, int y, UINT flags)
{
  if (wx_window)
    wxDoOnMouseLeave(wx_window, x, y, flags);
}

static void wxDoOnMouseLeave(wxWindow *wx_window, int x, int y, UINT flags)
{
  wxMouseEvent *event;

  event = new wxMouseEvent(wxEVENT_TYPE_LEAVE_WINDOW);

  event->x = x;
  event->y = y;
  
  event->shiftDown = (flags & MK_SHIFT);
  event->controlDown = (flags & MK_CONTROL);
  event->leftDown = (flags & MK_LBUTTON);
  event->middleDown = (flags & MK_MBUTTON);
  event->rightDown = (flags & MK_RBUTTON);
  {
    int cd;
    cd = (::GetKeyState(VK_CAPITAL) >> 1);
    event->capsDown = cd;
  }
  event->SetTimestamp(last_msg_time);

  if (!wx_window->CallPreOnEvent(wx_window->PreWindow(), event))
    if (!wx_window->IsGray())
      wx_window->OnEvent(event);
}

static int numpad_scan_codes[10];
static int plus_scan_code;
static int minus_scan_code;
static int times_scan_code;
static int divide_scan_code;
static int dot_scan_code;

static int generic_ascii_code[256];

/* The characters in find_shift_alts are things that we'll try
   to include in keyboard events as char-if-Shift-weren't-pressed,
   char-if-AltGr-weren't-pressed, etc. */
static const char *find_shift_alts = ("!@#$%^&*()_+-=\\|[]{}:\";',.<>/?~`"
                                      "abcdefghijklmnopqrstuvwxyz"
                                      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                      "0123456789");
static int other_key_codes[98];

static void init_sakc()
{
  int j;

  if (!sakc_initialized) {
    for (j = 0; find_shift_alts[j]; j++) {
      other_key_codes[j] = VkKeyScan(find_shift_alts[j]);
    }
    sakc_initialized = 1;
  }
}

#define THE_SCAN_CODE(lParam) ((((unsigned long)lParam) >> 16) & 0x1FF)

wxKeyEvent *wxMakeCharEvent(BOOL just_check, WORD wParam, LPARAM lParam, Bool isASCII, Bool isRelease, HWND handle)
{
  int id, other_id = 0, other_alt_id = 0, alt_id = 0;
  Bool tempControlDown, tempAltDown, tempShiftDown, tempCapsDown;
  
  tempControlDown = (::GetKeyState(VK_CONTROL) >> 1);
  tempShiftDown = (::GetKeyState(VK_SHIFT) >> 1);
  tempCapsDown = (::GetKeyState(VK_CAPITAL) >> 1);
  tempAltDown = ((HIWORD(lParam) & KF_ALTDOWN) == KF_ALTDOWN);

  if (isASCII) {
    int sc;

    id = wParam;

    /* Remember scan codes to help with some key-release events: */
    sc = THE_SCAN_CODE(lParam);
    if ((id >= 0) && (id <= 255))
      generic_ascii_code[id] = sc;

    {
      /* Look for elements of find_shift_alts that have a different
         shift/AltGr state: */
      short k;
      k = MapVirtualKey(sc, 1);
      if (k) {
        int j;
        init_sakc();
        for (j = 0; find_shift_alts[j]; j++) {
          if ((other_key_codes[j] & 0xFF) == k) {
            /* Figure out whether it's different in the shift
               for AltGr dimension, or both: */
            if (!((other_key_codes[j] & 0x100) != !tempShiftDown)) {
              /* shift is different */
              if (((other_key_codes[j] & 0x600) == 0x600)
                  == (tempControlDown && tempAltDown))
                other_id = find_shift_alts[j];
              else
                other_alt_id = find_shift_alts[j];
            } else {
              /* Shift is the same */
              if (((other_key_codes[j] & 0x600) == 0x600)
                  == (tempControlDown && tempAltDown)) {
                /* Shift and ctrl-alt states are the same.
                   Hopefully, so is the character! */
              } else
                alt_id = find_shift_alts[j];
            }
          }
        }
      }
    }
  } else {
    int override_mapping = (tempControlDown && !tempAltDown);

    if ((id = wxCharCodeMSWToWX(wParam)) == 0) {
      if (override_mapping || isRelease) {
	int j;

        /* Non-AltGr Ctl- combination, or a release event: 
           Map manually, because the default mapping is
           unsatisfactory. */

        /* Set id to the unshifted key: */
	id = MapVirtualKeyW(wParam, 2);
	id &= 0xFFFF;
	if (!id)
	  id = -1;
	else {
          if (id < 128)
            id = tolower(id);
        }

	/* Look for shifted alternate: */
        init_sakc();
	for (j = 0; find_shift_alts[j]; j++) {
	  if ((other_key_codes[j] & 0xFF) == wParam) {
            if (other_key_codes[j] & 0x100) {
              if ((other_key_codes[j] & 0x600) == 0x600)
                other_alt_id = find_shift_alts[j];
              else
                other_id = find_shift_alts[j];
            } else if ((other_key_codes[j] & 0x600) == 0x600) {
              alt_id = find_shift_alts[j];
            }
	  }
	}

        if ((id > -1) && tempShiftDown) {
          /* shift was pressed, so swap role of shifted and unshifted */
          int t;
          t = id;
          id = other_id;
          other_id = t;
          t = other_alt_id;
          other_alt_id = alt_id;
          alt_id = t;
        }
      } else
	id = -1;
    } else {
      /* Don't generate control-key down events: */
      if (!isRelease && (wParam == VK_CONTROL))
	return NULL;

      if (!override_mapping && !isRelease) {
	/* Let these get translated to WM_CHAR or skipped
	   entirely: */
	if ((wParam == VK_ESCAPE) 
	    || (wParam == VK_SHIFT) 
	    || (wParam == VK_CONTROL) 
	    || (wParam == VK_SPACE) 
	    || (wParam == VK_RETURN) 
	    || (wParam == VK_TAB) 
	    || (wParam == VK_BACK))
	  id = -1;
      }
    }

    if (isRelease && (id < 0)) {
      /* Try to generate a sensible release key: */
      int i, sc;
      sc = THE_SCAN_CODE(lParam);
      for (i = 0; i < 256; i++) {
	if (generic_ascii_code[i] == sc) {
	  id = i;
	  if (id < 127)
	    id = tolower(id);
	  break;
	}
      }
    }

    if (id < 0) {
      return NULL;
    }
  } 

  if (id > -1) {
    POINT pt;
    RECT rect;
    wxKeyEvent *event;

    if (just_check)
      return (wxKeyEvent *)0x1;

    event = new wxKeyEvent(wxEVENT_TYPE_CHAR);

    if (tempShiftDown)
      event->shiftDown = TRUE;
    if (tempControlDown)
      event->controlDown = TRUE;
    if (tempAltDown)
      event->metaDown = TRUE;
    if (tempCapsDown)
      event->capsDown = TRUE;

    event->keyCode = (isRelease ? WXK_RELEASE : id);
    event->keyUpCode = (isRelease ? id : WXK_PRESS);
    event->otherKeyCode = other_id;
    event->altKeyCode = alt_id;
    event->otherAltKeyCode = other_alt_id;
    event->SetTimestamp(last_msg_time);

    GetCursorPos(&pt);
    GetWindowRect(handle,&rect);
    pt.x -= rect.left;
    pt.y -= rect.top;

    event->x = pt.x;
    event->y = pt.y;

    return event;
  } else
    return NULL;
}

BOOL wxTranslateMessage(MSG *m)
{
  if ((m->message == WM_KEYDOWN) || (m->message == WM_SYSKEYDOWN)
      || (m->message == WM_KEYUP) || (m->message == WM_SYSKEYUP))
    if (!wxMakeCharEvent(TRUE, m->wParam, m->lParam, FALSE, 
			 (m->message == WM_KEYUP) || (m->message == WM_SYSKEYUP), 
			 m->hwnd))
      return TranslateMessage(m);
  
  return FALSE;
}

void wxWnd::OnChar(WORD wParam, LPARAM lParam, Bool isASCII, Bool isRelease)
{
  wxKeyEvent *event;

  event = wxMakeCharEvent(FALSE, wParam, lParam, isASCII, isRelease, handle);

  if (event && wx_window) {
    if (!wx_window->CallPreOnChar(wx_window->PreWindow(), event))
      if (!wx_window->IsGray())
	wx_window->OnChar(event);
  }
}

void wxSubWnd::OnVScroll(WORD wParam, WORD pos, HWND control)
{
  wxScrollEvent *event;

  if (control) {
    wxSlider *slider;
    slider = (wxSlider *)wxSliderList->Find((long)control);
    if (slider)
      wxSliderEvent(control, wParam, pos);
    return;
  }

  event = new wxScrollEvent;
  
  event->pos = pos;
  event->direction = wxVERTICAL;
  switch (wParam) {
  case SB_TOP:
    event->moveType = wxEVENT_TYPE_SCROLL_TOP;
    break;
    
  case SB_BOTTOM:
    event->moveType = wxEVENT_TYPE_SCROLL_BOTTOM;
    break;
    
  case SB_LINEUP:
    event->moveType = wxEVENT_TYPE_SCROLL_LINEUP;
    break;
    
  case SB_LINEDOWN:
    event->moveType = wxEVENT_TYPE_SCROLL_LINEDOWN;
    break;
    
  case SB_PAGEUP:
    event->moveType = wxEVENT_TYPE_SCROLL_PAGEUP;
    break;
    
  case SB_PAGEDOWN:
    event->moveType = wxEVENT_TYPE_SCROLL_PAGEDOWN;
    break;
    
  case SB_THUMBTRACK:
    event->moveType = wxEVENT_TYPE_SCROLL_THUMBTRACK;
    break;
    
  default:
    return;
    break;
  }
  if (wx_window)
    wx_window->DoScroll(event);
}

void wxSubWnd::OnHScroll( WORD wParam, WORD pos, HWND control)
{
  wxScrollEvent *event;

  if (control) {
    wxSlider *slider;
    slider = (wxSlider *)wxSliderList->Find((long)control);
    if (slider)
      wxSliderEvent(control, wParam, pos);
    return;
  }

  event = new wxScrollEvent;
  
  event->pos = pos;
  event->direction = wxHORIZONTAL;
  switch (wParam) {
  case SB_TOP:
    event->moveType = wxEVENT_TYPE_SCROLL_TOP;
    break;
    
  case SB_BOTTOM:
    event->moveType = wxEVENT_TYPE_SCROLL_BOTTOM;
    break;
    
  case SB_LINEUP:
    event->moveType = wxEVENT_TYPE_SCROLL_LINEUP;
    break;
    
  case SB_LINEDOWN:
    event->moveType = wxEVENT_TYPE_SCROLL_LINEDOWN;
    break;
    
  case SB_PAGEUP:
    event->moveType = wxEVENT_TYPE_SCROLL_PAGEUP;
    break;
    
  case SB_PAGEDOWN:
    event->moveType = wxEVENT_TYPE_SCROLL_PAGEDOWN;
    break;
    
  case SB_THUMBTRACK:
    event->moveType = wxEVENT_TYPE_SCROLL_THUMBTRACK;
    {
      /* Work-around for 16-bit limit on incoming `pos' */
      SCROLLINFO si;
      ZeroMemory(&si, sizeof(si));
      si.cbSize = sizeof(si);
      si.fMask = SIF_TRACKPOS;
      if (GetScrollInfo(handle, SB_HORZ, &si)) {
        pos = si.nTrackPos;
        event->pos = pos;
      }
    }
    
    break;
    
  default:
    return;
    break;
  }
  if (wx_window)
    wx_window->DoScroll(event);
}

void wxGetCharSize(HWND wnd, int *x, int *y,wxFont *the_font)
{
  TEXTMETRIC tm;
  HDC dc;
  HFONT fnt =0;
  HFONT was = 0;

  dc = wxwmGetDC(wnd);

  if (the_font&&(fnt=the_font->GetInternalFont(dc)))
  {
    was = (HFONT)SelectObject(dc,fnt);
  }
  GetTextMetrics(dc, &tm);
  if (the_font && fnt && was)
  {
    SelectObject(dc,was);
  }
  wxwmReleaseDC(wnd, dc);
  *x = tm.tmAveCharWidth;
  *y = tm.tmHeight + tm.tmExternalLeading;
}

// Returns 0 if was a normal ASCII value, not a special key. This indicates that
// the key should be ignored by WM_KEYDOWN and processed by WM_CHAR instead.
int wxCharCodeMSWToWX(int keySym)
{
  int id = 0;
  switch (keySym)
  {
    case VK_CANCEL:             id = WXK_CANCEL; break;
    case VK_BACK:               id = WXK_BACK; break;
    case VK_TAB:	        id = WXK_TAB; break;
    case VK_CLEAR:		id = WXK_CLEAR; break;
    case VK_RETURN:		id = WXK_RETURN; break;
    case VK_SHIFT:		id = WXK_SHIFT; break;
    case VK_CONTROL:		id = WXK_CONTROL; break;
    case VK_MENU :		id = WXK_MENU; break;
    case VK_PAUSE:		id = WXK_PAUSE; break;
    case VK_SPACE:		id = WXK_SPACE; break;
    case VK_ESCAPE:		id = WXK_ESCAPE; break;
    case VK_PRIOR:		id = WXK_PRIOR; break;
    case VK_NEXT :		id = WXK_NEXT; break;
    case VK_END:		id = WXK_END; break;
    case VK_HOME :		id = WXK_HOME; break;
    case VK_LEFT :		id = WXK_LEFT; break;
    case VK_UP:		        id = WXK_UP; break;
    case VK_RIGHT:		id = WXK_RIGHT; break;
    case VK_DOWN :		id = WXK_DOWN; break;
    case VK_SELECT:		id = WXK_SELECT; break;
    case VK_PRINT:		id = WXK_PRINT; break;
    case VK_EXECUTE:		id = WXK_EXECUTE; break;
    case VK_INSERT:		id = WXK_INSERT; break;
    case VK_DELETE:		id = WXK_DELETE; break;
    case VK_HELP :		id = WXK_HELP; break;
    case VK_NUMPAD0:		id = WXK_NUMPAD0; break;
    case VK_NUMPAD1:		id = WXK_NUMPAD1; break;
    case VK_NUMPAD2:		id = WXK_NUMPAD2; break;
    case VK_NUMPAD3:		id = WXK_NUMPAD3; break;
    case VK_NUMPAD4:		id = WXK_NUMPAD4; break;
    case VK_NUMPAD5:		id = WXK_NUMPAD5; break;
    case VK_NUMPAD6:		id = WXK_NUMPAD6; break;
    case VK_NUMPAD7:		id = WXK_NUMPAD7; break;
    case VK_NUMPAD8:		id = WXK_NUMPAD8; break;
    case VK_NUMPAD9:		id = WXK_NUMPAD9; break;
    case VK_MULTIPLY:		id = WXK_MULTIPLY; break;
    case VK_ADD:		id = WXK_ADD; break;
    case VK_SUBTRACT:		id = WXK_SUBTRACT; break;
    case VK_DECIMAL:		id = WXK_DECIMAL; break;
    case VK_DIVIDE:		id = WXK_DIVIDE; break;
    case VK_F1:		id = WXK_F1; break;
    case VK_F2:		id = WXK_F2; break;
    case VK_F3:		id = WXK_F3; break;
    case VK_F4:		id = WXK_F4; break;
    case VK_F5:		id = WXK_F5; break;
    case VK_F6:		id = WXK_F6; break;
    case VK_F7:		id = WXK_F7; break;
    case VK_F8:		id = WXK_F8; break;
    case VK_F9:		id = WXK_F9; break;
    case VK_F10:		id = WXK_F10; break;
    case VK_F11:		id = WXK_F11; break;
    case VK_F12:		id = WXK_F12; break;
    case VK_F13:		id = WXK_F13; break;
    case VK_F14:		id = WXK_F14; break;
    case VK_F15:		id = WXK_F15; break;
    case VK_F16:		id = WXK_F16; break;
    case VK_F17:		id = WXK_F17; break;
    case VK_F18:		id = WXK_F18; break;
    case VK_F19:		id = WXK_F19; break;
    case VK_F20:		id = WXK_F20; break;
    case VK_F21:		id = WXK_F21; break;
    case VK_F22:		id = WXK_F22; break;
    case VK_F23:		id = WXK_F23; break;
    case VK_F24:		id = WXK_F24; break;
    case VK_NUMLOCK:		id = WXK_NUMLOCK; break;
    case VK_SCROLL:		id = WXK_SCROLL; break;
    default:
    {
      return 0;
    }
  }
  return id;
}

int wxCharCodeWXToMSW(int id, Bool *isVirtual)
{
  int keySym = 0;
  *isVirtual = TRUE;
  switch (id)
  {
    case WXK_CANCEL:            keySym = VK_CANCEL; break;
    case WXK_CLEAR:		keySym = VK_CLEAR; break;
    case WXK_SHIFT:		keySym = VK_SHIFT; break;
    case WXK_CONTROL:		keySym = VK_CONTROL; break;
    case WXK_MENU :		keySym = VK_MENU; break;
    case WXK_PAUSE:		keySym = VK_PAUSE; break;
    case WXK_PRIOR:		keySym = VK_PRIOR; break;
    case WXK_NEXT :		keySym = VK_NEXT; break;
    case WXK_END:		keySym = VK_END; break;
    case WXK_HOME :		keySym = VK_HOME; break;
    case WXK_LEFT :		keySym = VK_LEFT; break;
    case WXK_UP:		keySym = VK_UP; break;
    case WXK_RIGHT:		keySym = VK_RIGHT; break;
    case WXK_DOWN :		keySym = VK_DOWN; break;
    case WXK_SELECT:		keySym = VK_SELECT; break;
    case WXK_PRINT:		keySym = VK_PRINT; break;
    case WXK_EXECUTE:		keySym = VK_EXECUTE; break;
    case WXK_INSERT:		keySym = VK_INSERT; break;
    case WXK_DELETE:		keySym = VK_DELETE; break;
    case WXK_HELP :		keySym = VK_HELP; break;
    case WXK_NUMPAD0:		keySym = VK_NUMPAD0; break;
    case WXK_NUMPAD1:		keySym = VK_NUMPAD1; break;
    case WXK_NUMPAD2:		keySym = VK_NUMPAD2; break;
    case WXK_NUMPAD3:		keySym = VK_NUMPAD3; break;
    case WXK_NUMPAD4:		keySym = VK_NUMPAD4; break;
    case WXK_NUMPAD5:		keySym = VK_NUMPAD5; break;
    case WXK_NUMPAD6:		keySym = VK_NUMPAD6; break;
    case WXK_NUMPAD7:		keySym = VK_NUMPAD7; break;
    case WXK_NUMPAD8:		keySym = VK_NUMPAD8; break;
    case WXK_NUMPAD9:		keySym = VK_NUMPAD9; break;
    case WXK_MULTIPLY:		keySym = VK_MULTIPLY; break;
    case WXK_ADD:		keySym = VK_ADD; break;
    case WXK_SUBTRACT:		keySym = VK_SUBTRACT; break;
    case WXK_DECIMAL:		keySym = VK_DECIMAL; break;
    case WXK_DIVIDE:		keySym = VK_DIVIDE; break;
    case WXK_F1:		keySym = VK_F1; break;
    case WXK_F2:		keySym = VK_F2; break;
    case WXK_F3:		keySym = VK_F3; break;
    case WXK_F4:		keySym = VK_F4; break;
    case WXK_F5:		keySym = VK_F5; break;
    case WXK_F6:		keySym = VK_F6; break;
    case WXK_F7:		keySym = VK_F7; break;
    case WXK_F8:		keySym = VK_F8; break;
    case WXK_F9:		keySym = VK_F9; break;
    case WXK_F10:		keySym = VK_F10; break;
    case WXK_F11:		keySym = VK_F11; break;
    case WXK_F12:		keySym = VK_F12; break;
    case WXK_F13:		keySym = VK_F13; break;
    case WXK_F14:		keySym = VK_F14; break;
    case WXK_F15:		keySym = VK_F15; break;
    case WXK_F16:		keySym = VK_F16; break;
    case WXK_F17:		keySym = VK_F17; break;
    case WXK_F18:		keySym = VK_F18; break;
    case WXK_F19:		keySym = VK_F19; break;
    case WXK_F20:		keySym = VK_F20; break;
    case WXK_F21:		keySym = VK_F21; break;
    case WXK_F22:		keySym = VK_F22; break;
    case WXK_F23:		keySym = VK_F23; break;
    case WXK_F24:		keySym = VK_F24; break;
    case WXK_NUMLOCK:		keySym = VK_NUMLOCK; break;
    case WXK_SCROLL:		keySym = VK_SCROLL; break;
    default:
    {
      *isVirtual = FALSE;
      keySym = id;
      break;
    }
  }
  return keySym;
}

void wxWindow::DoScroll(wxScrollEvent *event)
{
  long orient = event->direction;
  wxWnd *wnd = (wxWnd *)handle;
  int nScrollInc;
  HWND hWnd;

  nScrollInc = CalcScrollInc(event);
  if (nScrollInc == 0)
    return;

  hWnd = GetHWND();

  if (orient == wxHORIZONTAL) {
    int newPos = wnd->xscroll_position + nScrollInc;
    ::SetScrollPos(hWnd, SB_HORZ, newPos, TRUE);
    newPos = ::GetScrollPos(hWnd, SB_HORZ);
    nScrollInc = newPos - wnd->xscroll_position;
    wnd->xscroll_position = newPos;
    event->pos = newPos;
  } else {
    int newPos = wnd->yscroll_position + nScrollInc;
    ::SetScrollPos(hWnd, SB_VERT, newPos, TRUE );
    newPos = ::GetScrollPos(hWnd, SB_VERT);
    nScrollInc = newPos - wnd->yscroll_position;
    wnd->yscroll_position = newPos;
    event->pos = newPos;
  }

  if (!wnd->calcScrolledOffset) {
    OnScroll(event);
  } else {
    if (orient == wxHORIZONTAL) {
      ::ScrollWindow(hWnd, nScrollInc, 0, NULL, NULL);
    } else {
      ::ScrollWindow(hWnd, 0, nScrollInc, NULL, NULL);      
    }
  
    OnCalcScroll();

    InvalidateRect(hWnd, NULL, FALSE);
  }
}

void wxWindow::OnCalcScroll()
{
}

int wxWindow::CalcScrollInc(wxScrollEvent *event)
{
  int pos = event->pos;
  long orient = event->direction;
  int nScrollInc = 0;
  wxWnd *wnd = (wxWnd *)handle;
  HWND hWnd;

  switch (event->moveType)
  {
    case wxEVENT_TYPE_SCROLL_TOP:
    {
      if (orient == wxHORIZONTAL)
        nScrollInc = - wnd->xscroll_position;
      else
        nScrollInc = - wnd->yscroll_position;
      break;
    }
    case wxEVENT_TYPE_SCROLL_BOTTOM:
    {
      if (orient == wxHORIZONTAL)
        nScrollInc = wnd->xscroll_lines - wnd->xscroll_position;
      else
        nScrollInc = wnd->yscroll_lines - wnd->yscroll_position;
      break;
    }
    case wxEVENT_TYPE_SCROLL_LINEUP:
    {
      nScrollInc = -1;
      break;
    }
    case wxEVENT_TYPE_SCROLL_LINEDOWN:
    {
      nScrollInc = 1;
      break;
    }
    case wxEVENT_TYPE_SCROLL_PAGEUP:
    {
      if (orient == wxHORIZONTAL)
        nScrollInc = -wnd->xscroll_lines_per_page;
      else
        nScrollInc = -wnd->yscroll_lines_per_page;
      break;
    }
    case wxEVENT_TYPE_SCROLL_PAGEDOWN:
    {
      if (orient == wxHORIZONTAL)
        nScrollInc = wnd->xscroll_lines_per_page;
      else
        nScrollInc = wnd->yscroll_lines_per_page;
      break;
    }
    case wxEVENT_TYPE_SCROLL_THUMBTRACK:
    {
      if (orient == wxHORIZONTAL)
        nScrollInc = pos - wnd->xscroll_position;
      else
        nScrollInc = pos - wnd->yscroll_position;
      break;
    }
    default:
    {
      break;
    }
  }

  hWnd = GetHWND();
  if (orient == wxHORIZONTAL) {
    if (wnd->calcScrolledOffset) {
      // We're scrolling automatically
      int w;
      RECT rect;
      int nMaxWidth;
      int nHscrollMax;

      GetClientRect(hWnd, &rect);
      w = rect.right - rect.left;
      nMaxWidth = wnd->xscroll_lines*wnd->xscroll_pixels_per_line;
      
      nHscrollMax = (int)ceil((nMaxWidth - w)/(double)wnd->xscroll_pixels_per_line);
      nHscrollMax = max(0, nHscrollMax);

      nScrollInc = max(-wnd->xscroll_position,
		       min(nScrollInc, nHscrollMax - wnd->xscroll_position));
      return nScrollInc;
    } else {
      // We're not scrolling automatically so we don't care about pixel-per-line
      int newPosition = wnd->xscroll_position + nScrollInc;
      if (newPosition < 0)
	return -wnd->xscroll_position;
      else if (newPosition > wnd->xscroll_lines)
	return wnd->xscroll_lines - wnd->xscroll_position;
      else
        return nScrollInc;
    }
  } else {
    if (wnd->calcScrolledOffset) {
      // We're scrolling automatically
      RECT rect;
      int h;
      int nMaxHeight;
      int nVscrollMax;

      GetClientRect(hWnd, &rect);
      h = rect.bottom - rect.top;
      
      nMaxHeight = wnd->yscroll_lines*wnd->yscroll_pixels_per_line;
      
      nVscrollMax = (int)ceil((nMaxHeight - h)/(double)wnd->yscroll_pixels_per_line);
      nVscrollMax = max(0, nVscrollMax);
      
      nScrollInc = max(-wnd->yscroll_position,
		       min(nScrollInc, nVscrollMax - wnd->yscroll_position));
      return nScrollInc;
    } else {
      // We're not scrolling automatically so we don't care about pixel-per-line
      int newPosition = wnd->yscroll_position + nScrollInc;
      if (newPosition < 0)
	return -wnd->yscroll_position;
      else if (newPosition > wnd->yscroll_lines)
	return wnd->yscroll_lines - wnd->yscroll_position;
      else
	return nScrollInc;
    }
  }
}

void wxWindow::OnScroll(wxScrollEvent *event)
{
}

void wxWindow::SetScrollPos(int orient, int pos)
{
  wxWnd *wnd = (wxWnd *)handle;
  int wOrient;
  HWND hWnd;

  if (orient < 0) {
    /* Hack to avoid calcScrolledOffset check */
    orient = -orient;
  } else {
    if (wnd->calcScrolledOffset)
      return;
  }

  if (orient == wxHORIZONTAL)
    wOrient = SB_HORZ;
  else
    wOrient = SB_VERT;
    
  hWnd = GetHWND();
  if (hWnd) {
    ::SetScrollPos(hWnd, wOrient, pos, TRUE);

    if (orient == wxHORIZONTAL) {
      int x;
      x = ::GetScrollPos(hWnd, SB_HORZ);
      wnd->xscroll_position = x;
    } else {
      int y;
      y = ::GetScrollPos(hWnd, SB_VERT);
      wnd->yscroll_position = y;
    }

    OnCalcScroll();
  }
}

void wxWindow::SetScrollRange(int orient, int range)
{
  wxWnd *wnd = (wxWnd *)handle;
  HWND hWnd;
  int wOrient, page, vis;
  SCROLLINFO info;

  if (wnd->calcScrolledOffset) return;

  if (orient == wxHORIZONTAL)
    wOrient = SB_HORZ;
  else
    wOrient = SB_VERT;
    
  if (orient == wxHORIZONTAL) {
    page = wnd->xscroll_lines_per_page;
    vis = wnd->x_scroll_visible;
  } else {
    page = wnd->yscroll_lines_per_page;
    vis = wnd->y_scroll_visible;
  }

  info.cbSize = sizeof(SCROLLINFO);
  info.nPage = page;
  info.nMin = 0;
  info.nMax = range + page - 1;

  info.fMask = SIF_PAGE | SIF_RANGE | (vis ? SIF_DISABLENOSCROLL : 0);

  hWnd = GetHWND();

  if (hWnd) {
    ::SetScrollInfo(hWnd, wOrient, &info, TRUE);
  }

  if (orient == wxHORIZONTAL)
    wnd->xscroll_lines = range;
  else
    wnd->yscroll_lines = range;
}

void wxWindow::SetScrollPage(int orient, int page)
{
  wxWnd *wnd = (wxWnd *)handle;
  SCROLLINFO info;
  HWND hWnd;
  int dir, range, vis;

  if (wnd->calcScrolledOffset) return;

  if (orient == wxHORIZONTAL) {
    dir = SB_HORZ;    
    range = wnd->xscroll_lines;
    wnd->xscroll_lines_per_page = page;
    vis = wnd->x_scroll_visible;
  } else {
    dir = SB_VERT;
    range = wnd->yscroll_lines;
    wnd->yscroll_lines_per_page = page;
    vis = wnd->y_scroll_visible;
  }

  info.cbSize = sizeof(SCROLLINFO);
  info.nPage = page;
  info.nMin = 0;
  info.nMax = range + page - 1;
  info.fMask = SIF_PAGE | SIF_RANGE | (vis ? SIF_DISABLENOSCROLL : 0);

  hWnd = GetHWND();
  if (hWnd) {
    ::SetScrollInfo(hWnd, dir, &info, TRUE);
  }
}

int wxWindow::GetScrollPos(int orient)
{
  wxWnd *wnd = (wxWnd *)handle;
  HWND hWnd;
  int wOrient;

  if (wnd->calcScrolledOffset) return 0;

  if (orient == wxHORIZONTAL)
    wOrient = SB_HORZ;
  else
    wOrient = SB_VERT;
  hWnd = GetHWND();
  if (hWnd)
    return ::GetScrollPos(hWnd, wOrient);
  else
    return 0;
}

int wxWindow::GetScrollRange(int orient)
{
  wxWnd *wnd;
  wnd = (wxWnd *)handle;
  if (wnd->calcScrolledOffset) return 0;

  if (orient == wxHORIZONTAL)
    return max(0, wnd->xscroll_lines);
  else
    return max(0, wnd->yscroll_lines);
}

int wxWindow::GetScrollPage(int orient)
{
  wxWnd *wnd;
  wxCanvas *c;

  wnd = (wxWnd *)handle;
  if (wnd->calcScrolledOffset) return 0;

  c = (wxCanvas *)this;

  if (orient == wxHORIZONTAL) {
    if (c->horiz_units <= 0)
      return 0;
    return wnd->xscroll_lines_per_page;
  } else {
    if (c->vert_units <= 0)
      return 0;
    return wnd->yscroll_lines_per_page;
  }
}

// Default OnSize resets scrollbars, if any
void wxWindow::OnSize(int bad_w, int bad_h)
{
  wxWnd *wnd;

  if (wxWinType != wxTYPE_XWND)
    return;

  wnd = (wxWnd *)handle;
    
  if (wxSubType(__type, wxTYPE_DIALOG_BOX)) {
    wxChildNode* node;
    wxChildList *cl;
    cl = GetChildren();
    node = cl->First(); 

    if (node && !node->Next()) {
      wxWindow *win;
      Bool hasSubPanel;

      win = (wxWindow *)node->Data();
      hasSubPanel = ((wxSubType(win->__type, wxTYPE_PANEL)
		      && !wxSubType(win->__type, wxTYPE_DIALOG_BOX))
		     || wxSubType(win->__type, wxTYPE_CANVAS)
		     || wxSubType(win->__type, wxTYPE_TEXT_WINDOW));
      
      if (hasSubPanel) {
	int w, h;
	GetClientSize(&w, &h);
	win->SetSize(0, 0, w, h);
      }
    }
  }
}


Bool wxWindow::CallPreOnEvent(wxWindow *win, wxMouseEvent *evt)
{
  wxWindow *p;
  if (wxSubType(win->__type, wxTYPE_FRAME)
      || wxSubType(win->__type, wxTYPE_DIALOG_BOX))
    p = NULL;
  else
    p = win->GetParent();

  return ((p && CallPreOnEvent(p, evt)) || win->PreOnEvent(this, evt));
}

Bool wxWindow::CallPreOnChar(wxWindow *win, wxKeyEvent *evt)
{
  wxWindow *p;
  if (wxSubType(win->__type, wxTYPE_FRAME)
      || wxSubType(win->__type, wxTYPE_DIALOG_BOX))
    p = NULL;
  else
    p = win->GetParent();

  return ((p && CallPreOnChar(p, evt)) || win->PreOnChar(this, evt));
}

Bool wxWindow::PreOnEvent(wxWindow *, wxMouseEvent *)
{
  return FALSE;
}

Bool wxWindow::PreOnChar(wxWindow *, wxKeyEvent *)
{
  return FALSE;
}

wxWindow *wxWindow::PreWindow()
{
  return this;
}

wxWindow *wxGetActiveWindow(void)
{
  HWND hWnd;
  hWnd = GetActiveWindow();
  if (hWnd != 0)
  {
    wxWnd *wnd;
    wnd = wxFindWinFromHandle(hWnd);
    if (wnd && wnd->wx_window)
    {
      return wnd->wx_window;
    }
  }
  return NULL;
}
