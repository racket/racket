/*
 * File:	wx_dialg.cc
 * Purpose:	wxDialogBox and miscellaneous dialog functions
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#if USE_COMMON_DIALOGS
# include <commdlg.h>
#endif

#define wxDIALOG_DEFAULT_X 300
#define wxDIALOG_DEFAULT_Y 300

class wxDialogWnd : public wxSubWnd
{
public:
  MINMAXINFO mmi;

  wxDialogWnd(wxWnd *parent, wxWindow *wx_win,
              int x, int y, int width, int height,
              char *dialog_template);

  // Handlers
  LONG DefWindowProc(UINT nMsg, WPARAM wParam, LPARAM lParam);
  BOOL ProcessMessage(MSG* pMsg);
  BOOL OnEraseBkgnd(HDC pDC);
  BOOL OnClose(void);
  virtual void GetMinMaxInfo(MINMAXINFO *mmi);
};

wxDialogWnd::wxDialogWnd(wxWnd *parent, wxWindow *wx_win,
			 int x, int y, int width, int height,
			 char *dialog_template):
  wxSubWnd(parent, NULL, wx_win, x, y, width, height, 0, dialog_template)
{
}
 
LONG wxDialogWnd::DefWindowProc(UINT nMsg, WPARAM wParam, LPARAM lParam)
{
  return FALSE; /* Not processed */
}

BOOL wxDialogWnd::ProcessMessage(MSG* pMsg)
{
  return FALSE;
}

BOOL wxDialogWnd::OnClose(void)
{
  if (wx_window) {
    wxWindow *modal;
    modal = wxGetModalWindow(wx_window);
    if (modal && (modal != wx_window))
      return FALSE;
    
    if (wx_window->OnClose())
      return TRUE;
    else 
      return FALSE;
  }
  return FALSE;
}

BOOL wxDialogWnd::OnEraseBkgnd(HDC pDC)
{
  return FALSE;
}

void wxDialogWnd::GetMinMaxInfo(MINMAXINFO *_mmi)
{
  wxSubWnd::GetMinMaxInfo(_mmi);
  if (mmi.ptMinTrackSize.x > 0)
    _mmi->ptMinTrackSize.x = mmi.ptMinTrackSize.x;
  if (mmi.ptMinTrackSize.y > 0)
    _mmi->ptMinTrackSize.y = mmi.ptMinTrackSize.y;
  if (mmi.ptMaxTrackSize.x > 0)
    _mmi->ptMaxTrackSize.x = mmi.ptMaxTrackSize.x;
  if (mmi.ptMaxTrackSize.y > 0)
    _mmi->ptMaxTrackSize.y = mmi.ptMaxTrackSize.y;
}

wxDialogBox::wxDialogBox(wxWindow *Parent, char *Title, Bool Modal, 
               int x, int y, int width, int height, long style, char *name):
  wxbDialogBox((wxWindow *)Parent, Title, Modal, x, y, width, height, style, name)
{
  Create(Parent, Title, Modal, x, y, width, height, style, name);
}
  
Bool wxDialogBox::Create(wxWindow *Parent, char *Title, Bool Modal, 
                         int x, int y, int width, int height, long style, char *name)
{
  wxWnd *cparent = NULL;
  wxDialogWnd *wnd;

  // Do anything that needs to be done in the generic base class
  wxbDialogBox::Create(Parent, Title, Modal, x, y, width, height, style, name);

  has_child = FALSE;

  hSpacing = PANEL_HSPACING;
  vSpacing = PANEL_VSPACING;
  
  initial_hspacing = hSpacing;
  initial_vspacing = vSpacing;

  current_hspacing = hSpacing;
  current_vspacing = vSpacing;
  
  if (Parent) Parent->AddChild(this);
  window_parent = Parent;

  if (x == wxDEFAULT_POSITION) x = wxDIALOG_DEFAULT_X;
  if (y == wxDEFAULT_POSITION) y = wxDIALOG_DEFAULT_Y;

  wxWinType = wxTYPE_XWND;
  windowStyle = style;
  if (Parent)
    cparent = (wxWnd *)Parent->handle;

  if (width < 0)
    width = 0;
  if (height < 0)
    height = 0;

  // Allows creation of dialogs with & without captions under MSWindows
  wnd = new wxDialogWnd(cparent, this, x, y, width, height,
			!(style & wxNO_CAPTION)
			? ((style & wxMAXIMIZE) ? "wxCaptionResizeDialog" : "wxCaptionDialog")
			: ((style & wxMAXIMIZE) ? "wxNoCaptionResizeDialog" : "wxNoCaptionDialog"));

  handle = (char *)wnd;
  {
    wchar_t *ws;
    ws = wxWIDE_STRING(Title);
    SetWindowTextW(wnd->handle, ws);
  }

  wx_cursor = wxSTANDARD_CURSOR;  

  modal = Modal;
  return TRUE;
}

wxDialogBox::~wxDialogBox()
{
  wxWnd *wnd = (wxWnd *)handle;
  if (wnd && disabled_windows)
    Show(FALSE);

  if (wnd) {
    ShowWindow(wnd->handle, SW_HIDE);
  }
}

void wxDialogBox::Fit(void)
{
  wxPanel::Fit();
}

void wxDialogBox::ChangeToGray(Bool gray)
{
  wxWindow::ChangeToGray(gray);
  InternalGrayChildren(gray);
}


void wxDialogBox::Iconize(Bool WXUNUSED(iconize))
{
  // Windows dialog boxes can't be iconized
}

Bool wxDialogBox::Iconized(void)
{
  return FALSE;
}

void wxDialogBox::SetSize(int x, int y, int width, int height, int WXUNUSED(sizeFlags))
{
  wxWindow::SetSize(x, y, width, height);
}

void wxDialogBox::SetClientSize(int width, int height)
{
  wxWnd *wnd = (wxWnd *)handle;
  RECT rect;
  RECT rect2;
  int actual_width;
  int actual_height;

  GetClientRect(wnd->handle, &rect);

  GetWindowRect(wnd->handle, &rect2);

  // Find the difference between the entire window (title bar and all)
  // and the client area; add this to the new client size to move the
  // window
  actual_width = rect2.right - rect2.left - rect.right + width;
  actual_height = rect2.bottom - rect2.top - rect.bottom + height;

  MoveWindow(wnd->handle, rect2.left, rect2.top, actual_width, actual_height, TRUE);
  OnSize(actual_width, actual_height);
}

void wxDialogBox::GetPosition(int *x, int *y)
{
  HWND hWnd;
  RECT rect;

  hWnd = GetHWND();

  GetWindowRect(hWnd, &rect);

  *x = rect.left;
  *y = rect.top;

  if (*x < -10000)
    *x = -10000;
  if (*y < -10000)
    *y = -10000;
}

extern void wxDispatchEventsUntil(int (*f)(void *), void *data);

static int CheckDialogShowing(void *data)
{
  return !((wxDialogBox *)data)->disabled_windows;
}

Bool wxDialogBox::Show(Bool show)
{
  wxWnd *dialog = (wxWnd *)handle;

  if (modal) {
    if (show) {
      if (!!show == !!IsShown()) {
	wxwmBringWindowToTop(dialog->handle);
      }

      if (disabled_windows) {
	wxDispatchEventsUntil(CheckDialogShowing, (void *)this);
      } else {    
	wxChildNode *cnode;
	wxChildList *twl;

	SetShown(TRUE);
	
	twl = wxTopLevelWindows(this);
	twl->Show(this, TRUE);
	if (window_parent) {
	  wxChildList *cl;
	  cl = window_parent->GetChildren();
	  cl->Show(this, TRUE);
	}
      
	disabled_windows = new wxList();
	
	wxPushModalWindow(this, this);
	
	ShowWindow(dialog->handle, SW_SHOW);

	wxwmBringWindowToTop(dialog->handle);
	
	// Make list of windows that are disabled:
	for (cnode = twl->First(); cnode; cnode = cnode->Next()) {
	  wxWindow *w;
	  w = (wxWindow *)cnode->Data();
	  if (w && cnode->IsShown() && w != this) {
	    disabled_windows->Append(w);
	    w->InternalEnable(FALSE);
	  }
	}
    
	wxDispatchEventsUntil(CheckDialogShowing, (void *)this);
      }
    } else {
      if (disabled_windows) {
	wxNode *node;
	wxChildList *twl;

	SetShown(FALSE);
	
	twl = wxTopLevelWindows(this);
	twl->Show(this, FALSE);
	if (window_parent) {
	  wxChildList *cl;
	  cl = window_parent->GetChildren();
	  cl->Show(this, FALSE);
	}
	
	wxPopModalWindow(this, this);
	    
	node = disabled_windows->First();

	for (; node; node = node->Next()) {
	  wxWindow *w;
	  w = (wxWindow *)node->Data();
	  w->InternalEnable(TRUE);
	}

	disabled_windows = NULL;
      
	ShowWindow(dialog->handle, SW_HIDE);
	    
	if (GetParent()) {
	  wxWindow *par;
	  par = GetParent();
	  wxwmBringWindowToTop(par->GetHWND());
	}
      }
    }
  } else {
    wxChildList *twl;

    if (!!show == !!IsShown()) {
      if (show)
	wxwmBringWindowToTop(dialog->handle);
      return TRUE;
    }
    
    SetShown(show);
    
    twl = wxTopLevelWindows(this);
    twl->Show(this, show);
    if (window_parent) {
      wxChildList *cl;
      cl = window_parent->GetChildren();
      cl->Show(this, show);
    }

    if (show) {
      ShowWindow(dialog->handle, SW_SHOW);
      wxwmBringWindowToTop(dialog->handle);
    } else {
      // Try to highlight the correct window (the parent)
      HWND hWndParent = 0;
      if (GetParent()) {
	wxWindow *par;
	par = GetParent();
        hWndParent = par->GetHWND();
        if (hWndParent)
	  wxwmBringWindowToTop(hWndParent);
      }
      ShowWindow(dialog->handle, SW_HIDE);
    }
  }

  return TRUE;
}

void wxDialogBox::SetTitle(char *title)
{
  wxWnd *wnd = (wxWnd *)handle;
  SetWindowText(wnd->handle, title);
}

char *wxDialogBox::GetTitle(void)
{
  wxWnd *wnd = (wxWnd *)handle;
  GetWindowText(wnd->handle, wxBuffer, 1000);
  return wxBuffer;
}

void wxDialogBox::SystemMenu(void)
{
  ::DefWindowProc(GetHWND(), WM_SYSKEYDOWN, ' ', 1 << 29);
  ::DefWindowProc(GetHWND(), WM_SYSCHAR, ' ', 1 << 29);
}

void wxDialogBox::EnforceSize(int minw, int minh, int maxw, int maxh, int incw, int inch)
{
  wxDialogWnd *wnd = (wxDialogWnd *)handle;

  wnd->mmi.ptMinTrackSize.x = minw;
  wnd->mmi.ptMinTrackSize.y = minh;
  wnd->mmi.ptMaxTrackSize.x = maxw;
  wnd->mmi.ptMaxTrackSize.y = maxh;
}
