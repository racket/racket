/*
 * File:	wx_frame.cc
 * Purpose:	wxFrame implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include "fafa.h"

static HMENU emptyMenu;

// This range gives a maximum of 500
// MDI children. Should be enough :-)
#define wxFIRST_MDI_CHILD 4100
#define wxLAST_MDI_CHILD 4600

// Status border dimensions
#define         wxTHICK_LINE_BORDER 1
#define         wxTHICK_LINE_WIDTH  0

extern char wxFrameClassName[];
extern char wxMDIFrameClassName[];
extern char wxMDIChildFrameClassName[];
extern char wxPanelClassName[];

wxFrame::wxFrame(wxFrame *Parent, char *title, int x, int y,
                 int width, int height, long style, char *name):
  wxbFrame(Parent, title, x, y, width, height, style, name)
{
  Create(Parent, title, x, y, width, height, style, name);
}

Bool wxFrame::Create(wxFrame *Parent, char *title, int x, int y,
                 int width, int height, long style, char *name)
{
  wxWnd *cparent = NULL;

  if ((x == -1) && (y == -1)) {
    RECT r;
    SystemParametersInfo(SPI_GETWORKAREA, 0, &r, 0);
    x = r.left;
    y = r.top;
  }

  wxbFrame::Create(Parent, title, x, y, width, height, style, name);
  
  frame_type = style & (wxSDI | wxMDI_PARENT | wxMDI_CHILD);
  if (!frame_type) frame_type = wxSDI;
  windowStyle = style;
  wx_menu_bar = NULL;
  status_line_exists = FALSE;
  modal_showing = FALSE;
  handle = NULL;
  
  if (Parent) Parent->AddChild(this);
  window_parent = Parent;

  status_window = NULL;

  hiddenmax = 0;
  if (Parent)
    cparent = (wxWnd *)Parent->handle;

  switch (frame_type)
  {
    case wxMDI_PARENT:
      {
	wxMDIFrame *mf;
	wxWinType = wxTYPE_XWND;
	mf = new wxMDIFrame(NULL, this, title, x, y, width, height, style);
	handle = (char *)mf;
      }
      break;
    case wxMDI_CHILD:
      {
	wxMDIChild *mc;
	wxWinType = wxTYPE_MDICHILD;
	mc = new wxMDIChild((wxMDIFrame *)cparent, this, title, x, y, width, height, style);
	handle = (char *)mc;
      }
      break;
    default:
    case wxSDI:
      {
	wxFrameWnd *fw;
	wxWinType = wxTYPE_XWND;
	fw = new wxFrameWnd(cparent, wxFrameClassName, this, title,
			    x, y, width, height, style);
	handle = (char *)fw;
      }
      break;
  }

  wx_cursor = wxSTANDARD_CURSOR;  

  {
    /* Initialize client_d{w,h}, needed when GetCLientSize()
       is called while the frame is iconized. */
    int w, h, cw, ch;
    GetSize(&w, &h);
    GetClientSize(&cw, &ch);
    client_dh = h - ch;
    client_dw = w - cw;
  }

  return TRUE;
}

wxFrame::~wxFrame(void)
{
  if (wx_menu_bar)
    DELETE_OBJ wx_menu_bar;

  if (status_window) {
    status_window->DestroyWindow();
    delete status_window;
  }
}

HMENU wxFrame::GetWinMenu(void)
{
  if (handle)
    return ((wxWnd *)handle)->hMenu;
  else
    return 0;
}

void wxFrame::DrawMenuBar(void)
{
  wxFrame *frame;

  switch (frame_type) {
  case wxMDI_CHILD:
    frame = (wxFrame *)GetParent();
    break;
  default:
    frame = this;
    break;
  }

  {
    wxWnd *cframe = (wxWnd*)frame->handle;
    HWND hand = (HWND)cframe->handle;
    ::DrawMenuBar(hand);
  }
}

void wxFrame::ChangeToGray(Bool gray)
{
  wxWindow::ChangeToGray(gray);
  InternalGrayChildren(gray);
}

// Get size *available for subwindows* i.e. excluding menu bar etc.
// For XView, this is the same as GetSize
void wxFrame::GetClientSize(int *x, int *y)
{
  RECT rect;

  if (Iconized()) {
    /* Iconized window's client size is always (0,0).
       Use stored client_d{w,h} to calculate the client
       size for the window as shown. */
    GetSize(x, y);
    rect.top = rect.left = 0;
    rect.right = (*x) - client_dw;
    rect.bottom = (*y) - client_dh;
  } else {
    GetClientRect(GetHWND(), &rect);
  }

  switch (frame_type)
  {
    case wxMDI_PARENT:
    {
      int cwidth = rect.right;
      int cheight = rect.bottom;
      int ctop = 0;

      if (status_window)
        cheight -= status_window->height;
      *x = cwidth;
      *y = cheight;
      break;
    }
    default:
    case wxMDI_CHILD:
    case wxSDI:
    {
      if (status_window)
        rect.bottom -= status_window->height;

      *x = rect.right;
      *y = rect.bottom;
      break;
    }
  }
}

// Set the client size (i.e. leave the calculation of borders etc.
// to wxWindows)
void wxFrame::SetClientSize(int width, int height)
{
  wxFrame *parent;
  HWND hWnd;
  HWND hParentWnd = 0;
  RECT rect;
  RECT rect2;
  int actual_width;
  int actual_height;
  POINT point;

  parent = (wxFrame *)GetParent();
  hWnd = GetHWND();
  if (parent)
    hParentWnd = parent->GetHWND();

  GetClientRect(hWnd, &rect);

  GetWindowRect(hWnd, &rect2);

  // Find the difference between the entire window (title bar and all)
  // and the client area; add this to the new client size to move the
  // window
  actual_width = rect2.right - rect2.left - rect.right + width;
  actual_height = rect2.bottom - rect2.top - rect.bottom + height;

  if (status_window)
    actual_height += status_window->height;

  point.x = rect2.left;
  point.y = rect2.top;

  // If there's an MDI parent, must subtract the parent's top left corner
  // since MoveWindow moves relative to the parent
  if (parent && (wxWinType == wxTYPE_MDICHILD)) {
    ::ScreenToClient(hParentWnd, &point);
  }

  MoveWindow(hWnd, point.x, point.y, actual_width, actual_height, (BOOL)TRUE);
  OnSize(actual_width, actual_height);
}

void wxFrame::GetSize(int *width, int *height)
{
  HWND hwnd;

  hwnd = GetHWND();

  if (::IsIconic(hwnd)) {
    WINDOWPLACEMENT wp;
    wp.length = sizeof(wp);
    GetWindowPlacement(hwnd, &wp);
    *width = wp.rcNormalPosition.right - wp.rcNormalPosition.left;
    *height = wp.rcNormalPosition.bottom - wp.rcNormalPosition.top;
  } else {
    RECT rect;
    GetWindowRect(hwnd, &rect);
    *width = rect.right - rect.left;
    *height = rect.bottom - rect.top;
  }
}

void wxFrame::GetPosition(int *x, int *y)
{
  RECT rect;
  wxWindow *parent;
  POINT point;
  HWND hwnd;

  parent = GetParent();

  hwnd = GetHWND();
  if (::IsIconic(hwnd)) {
    WINDOWPLACEMENT wp;
    wp.length = sizeof(wp);
    GetWindowPlacement(hwnd, &wp);
    point.x = wp.rcNormalPosition.left;
    point.y = wp.rcNormalPosition.top;
  } else {
    GetWindowRect(hwnd, &rect);
    point.x = rect.left;
    point.y = rect.top;
  }

  // Since we now have the absolute screen coords,
  // if there's a parent we must subtract its top left corner
  if (parent)
  {
    wxWnd *cparent = (wxWnd *)(parent->handle);
    if (wxWinType == wxTYPE_MDICHILD)
    {
      wxMDIFrame *mdiParent = (wxMDIFrame *)cparent;
      ::ScreenToClient(mdiParent->client_hwnd, &point);
    }
  }
  *x = point.x;
  *y = point.y;

  if (*x < -10000)
    *x = -10000;
  if (*y < -10000)
    *y = -10000;
}

void wxFrame::SetSize(int x, int y, int width, int height, int WXUNUSED(sizeFlags))
{
  int currentX, currentY;
  int ww,hh ;

#if 0
  /* This doesn't seem to be true: */
  // Can't set size of an iconized frame. (We could actually play games
  // with SetWindowPlacement, but it doesn't seem worthwhile.)
  if (Iconized())
    Iconize(FALSE);
#endif

  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  GetSize(&ww,&hh) ;
  if (width == -1) width = ww ;
  if (height==-1) height = hh ;

  if (handle)
  {
    MoveWindow(GetHWND(), x, y, width, height, (BOOL)TRUE);
    OnSize(width, height);
  }
}

void wxFrame::EnforceSize(int minw, int minh, int maxw, int maxh, int incw, int inch)
{
  wxFrameWnd *wnd = (wxFrameWnd *)handle;

  wnd->mmi.ptMinTrackSize.x = minw;
  wnd->mmi.ptMinTrackSize.y = minh;
  wnd->mmi.ptMaxTrackSize.x = maxw;
  wnd->mmi.ptMaxTrackSize.y = maxh;
}

Bool wxFrame::Show(Bool show)
{
  int skipShow;
  int cshow;
  wxChildList *tlw;

  skipShow = (!show == !IsShown());

  SetShown(show);

  if (show) {
    if (hiddenmax) {
      hiddenmax = 0;
      cshow = SW_SHOWMAXIMIZED;
    } else {
      if ((windowStyle & wxFLOAT_FRAME)
	  && (windowStyle & wxNO_CAPTION))
	cshow = SW_SHOWNOACTIVATE; /* don't activate */
      else
	cshow = SW_RESTORE; /* Show */
    }
  } else {
    if (!skipShow) {
      int hm;
      hm = IsZoomed(GetHWND());
      hiddenmax = hm;
    }
    cshow = SW_HIDE;
  }
  
  tlw = wxTopLevelWindows(this);
  if (frame_type != wxMDI_CHILD) {
    tlw->Show(this, show);
  } else {
    wxWindow *p;
    p = GetParent();
    if (p->IsShown()) {
      tlw->Show(this, show);
    }
  }

  if (window_parent) {
    wxChildList *cl;
    cl = window_parent->GetChildren();
    cl->Show(this, show);
  }

  if (!show) {
    // Try to highlight the correct window (the parent)
    HWND hWndParent = 0;
    if (GetParent()) {
      wxWindow *par;
      par = GetParent();
      hWndParent = par->GetHWND();
      if (hWndParent)
	wxwmBringWindowToTop(hWndParent);
    }
  }
  
  if (!skipShow) {
    ShowWindow(GetHWND(), cshow);
  }
  if (show && (cshow != SW_SHOWNOACTIVATE)) {
    wxwmBringWindowToTop(GetHWND());
    /* OnActivate(TRUE); */
  }

  if (!skipShow) {
    if (frame_type == wxMDI_CHILD) {
      wxMDIFrame *cparent;
      wxWindow *par;
      WINDOWINFO winfo;

      par = GetParent();
      cparent = (wxMDIFrame *)par->handle;
      if (cshow == SW_HIDE) {
	if (cparent->current_child == (wxFrameWnd *)handle) {
	  HMENU new_menu;
	  HWND next;

	  cparent->current_child = NULL;

	  cparent->parent_frame_active = TRUE;
	  new_menu = ((wxFrame *)par)->GetWinMenu();
	  
	  if (!new_menu) {
	    if (!emptyMenu)
	      emptyMenu = wxwmCreateMenu();
	    new_menu = emptyMenu;
	  }
	  
	  ::SendMessage(cparent->client_hwnd, WM_MDISETMENU,
			(WPARAM)new_menu,
			(LPARAM)NULL);
	  
	  ::DrawMenuBar(cparent->handle);

	  OnMDIActivate(FALSE);

	  GetWindowInfo(cparent->handle, &winfo);
	  if (winfo.dwWindowStatus == WS_ACTIVECAPTION)
	    OnActivate(FALSE);

	  next = GetNextWindow(GetHWND(), GW_HWNDNEXT);
	  while (next) {
	    wxWnd *nh;
	    nh = wxFindWinFromHandle(next);
	    if (nh && nh->wx_window 
		&& wxSubType(nh->wx_window->__type, wxTYPE_FRAME)
		&& (((wxFrame *)nh->wx_window)->frame_type == wxMDI_CHILD)) {
	      ::SendMessage(cparent->client_hwnd, WM_MDIACTIVATE,
			    (WPARAM)next,
			    (LPARAM)NULL);
	      next = NULL;
	    } else
	      next = GetNextWindow(next, GW_HWNDNEXT);
	  }
	}
      } else if (cshow != SW_HIDE) {
	HWND cur;
	cur = (HWND)::SendMessage(cparent->client_hwnd, WM_MDIGETACTIVE, 0, 0);
	if (cur == GetHWND()) {
	  HMENU new_menu;
	  cparent->current_child = (wxFrameWnd *)handle;
	  cparent->parent_frame_active = FALSE;
	  new_menu = GetWinMenu();
	  if (new_menu) {
	    ::SendMessage(cparent->client_hwnd, WM_MDISETMENU,
			  (WPARAM)new_menu,
			  (LPARAM)NULL);
	    ::DrawMenuBar(cparent->handle);
	  }
	  OnMDIActivate(TRUE);
	  GetWindowInfo(cparent->handle, &winfo);
	  if (winfo.dwWindowStatus == WS_ACTIVECAPTION)
	    OnActivate(TRUE);
	}
      }
    }
  }

  if (frame_type == wxMDI_PARENT) {
    /* Add/remove shown children in top-level-windows list */
    wxChildList *cl;
    wxChildNode *cn;
    cl = GetChildren();
    for (cn = cl->First(); cn; cn = cn->Next()) {
      wxWindow *w;
      w = (wxWindow *)cn->Data();
      if (wxSubType(w->__type, wxTYPE_FRAME)) {
	wxFrame *cf;
	cf = (wxFrame *)w;
	if ((cf->frame_type == wxMDI_CHILD)
	    && cf->IsShown()) {
	  tlw->Show(cf, show);
	}
      }
    }
  }

  return TRUE;
}

void wxFrame::Iconize(Bool iconize)
{
  int cshow;
  HWND hwnd;

  if (!IsShown())
    return;

  if (!iconize && !Iconized())
    return; /* Otherwise we'd mess up maximizations */

  hwnd = GetHWND();
  if (iconize) {
    cshow = SW_MINIMIZE;
  } else {
    cshow = SW_RESTORE;
  }

  ShowWindow(GetHWND(), cshow);
}

// Equivalent to maximize/restore in Windows
void wxFrame::Maximize(Bool maximize)
{
  if (Iconized())
    return;

  if (IsShown()) {
    int cshow;
    if (maximize)
      cshow = SW_MAXIMIZE;
    else
      cshow = SW_RESTORE;
    ShowWindow(GetHWND(), cshow);
  } else
    hiddenmax = maximize;
}

Bool wxFrame::Iconized(void)
{
  return (Bool)::IsIconic(GetHWND());
}

Bool wxFrame::IsMaximized()
{
  if (IsShown())
    return (Bool)::IsZoomed(GetHWND());
  else
    return hiddenmax;
}

void wxFrame::SetTitle(char *title)
{
  if (is_mod) {
    int len;
    len = strlen(title);
    memcpy(wxBuffer, title, len);
    wxBuffer[len] = '*';
    wxBuffer[len+1] = 0;
    title = wxBuffer;
  }
  SetWindowTextW(GetHWND(), wxWIDE_STRING(title));
}

char *wxFrame::GetTitle(void)
{
  GetWindowTextW(GetHWND(), (wchar_t *)wxBuffer, 500);
  if (is_mod) {
    int len;
    len = wx_wstrlen((wchar_t *)wxBuffer);
    ((wchar_t *)wxBuffer)[len - 1] = 0;
  }
  return wx_convert_from_wchar((wchar_t *)wxBuffer);
}

void wxFrame::SetFrameModified(Bool mod)
{
  if (is_mod != !!mod) {
    int len;
    char *s;

    s = GetTitle();
    len = strlen(s);
    memcpy(wxBuffer, s, len + 1);
    is_mod = !!mod;

    if (mod) {
      wxBuffer[len] = '*';
      wxBuffer[len+1] = 0;
    }
    SetWindowTextW(GetHWND(), wxWIDE_STRING(wxBuffer));
  }
}
 
void wxFrame::OnToolbarButton(void)
{
}

void wxFrame::OnMDIActivate(Bool WXUNUSED(act))
{
}

static wxBitmap *black_bg = NULL;

void wxFrame::SetIcon(wxBitmap *icon, wxBitmap *bg, int kind)
/* kind: 1 = small, 2 = large, 0 = both */
{
  HICON wnd_icon;
  wxFrameWnd *wnd = (wxFrameWnd *)handle;
  int bigP = ((kind == 2) || (kind == 0));
  int smallP = ((kind == 1) || (kind == 0));

  if (bigP && wnd->bigIcon)
    DestroyIcon(wnd->bigIcon);
  if (smallP && wnd->icon)
    DestroyIcon(wnd->icon);

  if (!icon || !icon->Ok())
    wnd_icon = 0;
  else {
    ICONINFO info;

    if (!bg || !bg->Ok()) {
      if (!black_bg || (black_bg->GetWidth() != icon->GetWidth())
	  || (black_bg->GetHeight() != icon->GetHeight())) {
	wxMemoryDC *mdc;
	black_bg = new wxBitmap(icon->GetWidth(), icon->GetHeight());
	mdc = new wxMemoryDC();
	mdc->SelectObject(black_bg);
	mdc->SetBackground(wxBLACK);
	mdc->Clear();
	mdc->SelectObject(NULL);
	delete mdc;
      }
      bg = black_bg;
    }

    if (bg->Ok()) {
      info.fIcon = TRUE;
      info.hbmMask = bg->ms_bitmap;
      info.hbmColor = icon->ms_bitmap;
      wnd_icon = CreateIconIndirect(&info);
    } else
      wnd_icon = NULL;
  }

  if (bigP)
    wnd->bigIcon = wnd_icon;
  if (smallP)
    wnd->icon = wnd_icon;

  if (bigP)
    SendMessage(GetHWND(), WM_SETICON, (WORD)TRUE, (DWORD)wnd_icon);
  if (smallP)
    SendMessage(GetHWND(), WM_SETICON, (WORD)FALSE, (DWORD)wnd_icon);
}

void wxFrame::CreateStatusLine(int number, char *WXUNUSED(name))
{
  wxFrameWnd *cframe = (wxFrameWnd *)handle;
  TEXTMETRIC tm;
  HDC dc;
  int char_height;
  int status_window_height;

  if (status_line_exists)
    return;

  status_line_exists = TRUE;

  nb_status = number;

  if (!wxSTATUS_LINE_FONT) {
    wxFont *f;
    HDC dc;
    f = wxTheFontList->FindOrCreateFont(8, wxSYSTEM, wxNORMAL, wxNORMAL, FALSE);
    dc = ::GetDC(NULL);
    wxSTATUS_LINE_FONT = f->GetInternalFont(dc);
    ::ReleaseDC(NULL, dc);
  }

  dc = GetDC(cframe->handle);
  SelectObject(dc, wxSTATUS_LINE_FONT);
  GetTextMetrics(dc, &tm);
  ReleaseDC(cframe->handle, dc);
  char_height = tm.tmHeight + tm.tmExternalLeading;
  status_window_height =
    (int)((char_height * 4.0/3.0) + 2*wxTHICK_LINE_BORDER);

  status_window = new wxStatusWnd(cframe, status_window_height);
  PositionStatusWindow();
}

void wxFrame::SetStatusText(char *text, int number)
{
  HDC dc;
  RECT rect;
  int width, height, cy, y;
  TEXTMETRIC tm;
  
  if (!status_line_exists)
    return;

  if ((number < 0) || (number >= nb_status))
    return;

  // Microsoft standard: use button colors for status line
  status_window->light_grey_brush = brushFace ;

  if (text) {
    char *s;
    s = copystring(text);
    status_window->status_text = s;
  } else 
    status_window->status_text = NULL;

  dc = GetDC(status_window->handle);
  SelectObject(dc, wxSTATUS_LINE_FONT);

  GetClientRect(status_window->handle, &rect );

  width = rect.right;
  height = rect.bottom;

  SetBkMode(dc, TRANSPARENT);

  ::SetTextColor(dc, GetSysColor( COLOR_BTNTEXT ) );

  GetTextMetrics(dc, &tm);
  cy = tm.tmHeight + tm.tmExternalLeading;
  y = (int)((rect.bottom - cy)/2);

  rect.left += wxTHICK_LINE_BORDER + 1;
  rect.top += wxTHICK_LINE_BORDER + 1;
  rect.right -= (wxTHICK_LINE_BORDER + 1);
  rect.bottom -= (wxTHICK_LINE_BORDER + 1);
  FillRect(dc, &rect, status_window->light_grey_brush);

  IntersectClipRect(dc, wxTHICK_LINE_BORDER + 3, y-1,
                            width - wxTHICK_LINE_BORDER - 1, height);

  if (status_window->status_text)
    TextOut(dc, wxTHICK_LINE_BORDER + 4, y,
                status_window->status_text, strlen(status_window->status_text));

  SelectClipRgn(dc, NULL);
  ReleaseDC(status_window->handle, dc);
}

void wxFrame::PositionStatusWindow(void)
{
  // We will assume that in a multi status line, all fields have the
  //  same width.
  RECT rect;
  int cwidth, cheight;

  GetClientRect(GetHWND(), &rect);
  cwidth = rect.right;
  cheight = rect.bottom;

  {
    int real_width = (int)(cwidth/nb_status);
    MoveWindow(status_window->handle, 
	       0, cheight - status_window->height,
	       real_width, status_window->height, TRUE);
  }
}

void wxFrame::SystemMenu(void)
{
  wxWnd *wnd = (wxWnd *)handle;
  wnd->DefWindowProc(WM_SYSKEYDOWN, ' ', 1 << 29);
  wnd->DefWindowProc(WM_SYSCHAR, ' ', 1 << 29);
}

void wxFrame::Fit(void)
{
  // Work out max. size
  wxChildNode *node;
  int max_width = 0;
  int max_height = 0;

  node = children->First();
  while (node) {
    // Find a child that's a subwindow, but not a dialog box.
    wxWindow *win;
    win = (wxWindow *)node->Data();

    if ((wxSubType(win->__type, wxTYPE_PANEL) &&
         !wxSubType(win->__type, wxTYPE_DIALOG_BOX)) ||
        wxSubType(win->__type, wxTYPE_TEXT_WINDOW) ||
        wxSubType(win->__type, wxTYPE_CANVAS))
    {
      int width, height;
      int x, y;
      win->GetSize(&width, &height);
      win->GetPosition(&x, &y);

      if ((x + width) > max_width)
        max_width = x + width;
      if ((y + height) > max_height)
        max_height = y + height;
    }
    node = node->Next();
  }
  SetClientSize(max_width, max_height);
}

/*
 * Windows 3 specific windows
 *
 */

wxStatusWnd::wxStatusWnd(wxFrameWnd *parent, int the_height)
{
  status_text = NULL;
  height = the_height;
  // Microsoft standard: use button colors for status line
  light_grey_brush = brushFace;

  Create(parent, wxPanelClassName, NULL, NULL, 0, 0, 100, 100, WS_CHILD);
  ShowWindow(handle, SW_SHOW);
}

wxStatusWnd::~wxStatusWnd(void)
{
}

BOOL wxStatusWnd::OnPaint()
{
  RECT rect;
  if (GetUpdateRect(handle, &rect, FALSE)) {
    PAINTSTRUCT ps;
    int width, _height, cy, y;
    HBRUSH old_brush;
    HPEN old_pen;
    TEXTMETRIC tm;

    // Microsoft standard: use button colors for status line
    light_grey_brush = brushFace ;

    // Hold a pointer to the dc so long as the OnPaint() message
    // is being processed
    cdc = BeginPaint(handle, &ps);
    SelectObject(cdc, wxSTATUS_LINE_FONT);

    ::GetClientRect(handle, &rect);

    width = rect.right;
    _height = rect.bottom;

    ::SetBkMode(cdc, TRANSPARENT);
    ::FillRect(cdc, &rect, light_grey_brush);

    old_brush = (HBRUSH)::SelectObject(cdc,brushFace) ;

#if wxTHICK_LINE_WIDTH
    // Draw border
    // Have grey background, plus 3-d border -
    // One black rectangle.
    // Inside this, left and top sides - dark grey. Bottom and right -
    // white.

    // Right and bottom white lines
    old_pen = (HPEN)::SelectObject(cdc,penLight) ;
    MoveToEx(cdc, width-wxTHICK_LINE_BORDER,
	     wxTHICK_LINE_BORDER, NULL);
    LineTo(cdc, width-wxTHICK_LINE_BORDER,
	   _height-wxTHICK_LINE_BORDER);
    LineTo(cdc, wxTHICK_LINE_BORDER,
	   _height-wxTHICK_LINE_BORDER);

    // Left and top grey lines
    ::SelectObject(cdc,penShadow) ;
    LineTo(cdc, wxTHICK_LINE_BORDER, wxTHICK_LINE_BORDER);
    LineTo(cdc, width-wxTHICK_LINE_BORDER, wxTHICK_LINE_BORDER);
#else
    old_pen = NULL;
#endif

    SetTextColor(cdc, GetSysColor(COLOR_BTNTEXT) );

    ::GetTextMetrics(cdc, &tm);
    cy = tm.tmHeight + tm.tmExternalLeading;
    y = (int)((rect.bottom - cy)/2);

    ::IntersectClipRect(cdc, wxTHICK_LINE_BORDER + 3, y-1,
                            rect.right - wxTHICK_LINE_BORDER - 1, rect.bottom);

    if (status_text)
      ::TextOut(cdc, wxTHICK_LINE_BORDER + 4, y,
                  status_text, strlen(status_text));

    ::SelectClipRgn(cdc, NULL);
    if (old_pen)
      SelectObject(cdc, old_pen);
    old_pen = NULL;
    if (old_brush)
      SelectObject(cdc, old_brush);
    old_brush = NULL;

    EndPaint(handle, &ps);
    cdc = NULL;
    return 0;
  }
  return 1;
}

/*
 * Frame window
 *
 */

wxFrameWnd::wxFrameWnd(void)
{
}
		   
wxFrameWnd::wxFrameWnd(wxWnd *parent, char *WXUNUSED(wclass), wxWindow *wx_win, char *title,
		       int x, int y, int width, int height, long style)
{
  DWORD msflags = WS_POPUP;  
  DWORD extendedStyle = 0;

  defaultIcon = wxSTD_FRAME_ICON;

  if (!(style & wxNO_THICK_FRAME) && !(style & wxNO_RESIZE_BORDER)) {
    msflags |= WS_THICKFRAME | WS_BORDER;
    msflags |= WS_MAXIMIZEBOX;
  }
  if (!(style & wxNO_SYSTEM_MENU))
    msflags |= WS_SYSMENU;
  if (style & wxICONIZE)
    msflags |= WS_MINIMIZE;
  if (style & wxMAXIMIZE)
    msflags |= WS_MAXIMIZE;
  if (!(style & wxNO_CAPTION)) {
    msflags |= WS_CAPTION;
    msflags |= WS_MINIMIZEBOX;
  }

  if (style & wxSTAY_ON_TOP)
    extendedStyle |= WS_EX_TOPMOST;
  if (style & wxFLOAT_FRAME) {
    if (style & wxNO_CAPTION)
      extendedStyle |= WS_EX_TOPMOST;
    else
      extendedStyle |= WS_EX_PALETTEWINDOW;
    extendedStyle |= WS_EX_TOOLWINDOW;
  }

  icon = NULL;
  iconized = FALSE;
  Create(parent, wxFrameClassName, wx_win, title, x, y, width, height,
         msflags, NULL, extendedStyle);
  // Seems to be necessary if we use WS_POPUP
  // style instead of WS_OVERLAPPED
  if (width > -1 && height > -1)
    ::PostMessage(handle, WM_SIZE, SIZE_RESTORED, MAKELPARAM(width, height));
}

wxFrameWnd::~wxFrameWnd(void)
{
  if (icon)
    DestroyIcon(icon);
  if (bigIcon)
    DestroyIcon(bigIcon);
}

BOOL wxFrameWnd::OnPaint(void)
{
  RECT rect;

  if (GetUpdateRect(handle, &rect, FALSE)) {
    PAINTSTRUCT ps;
    // Hold a pointer to the dc so long as the OnPaint() message
    // is being processed
    cdc = BeginPaint(handle, &ps);
      
    if (iconized) {
      HICON the_icon = icon;
      if (the_icon == 0)
        the_icon = defaultIcon;

      // Erase background before painting or we get white background
      this->DefWindowProc(WM_ICONERASEBKGND,(WORD)ps.hdc,0L);
      
      if (the_icon) {
        RECT rect;
        GetClientRect(handle, &rect);
	{
	  int icon_width = 32;
	  int icon_height = 32;
	  int icon_x = (int)((rect.right - icon_width)/2);
	  int icon_y = (int)((rect.bottom - icon_height)/2);
	  DrawIcon(cdc, icon_x, icon_y, the_icon);
	}
      }
    }

    if (!iconized && wx_window)
      wx_window->OnPaint();

    EndPaint(handle, &ps);
    cdc = NULL;
    return 0;
  }

  return 1;
}

HICON wxFrameWnd::OnQueryDragIcon(void)
{
  return NULL;
}

void wxFrameWnd::OnSize(int bad_x, int bad_y, UINT id)
{
  switch (id)
  {
    case SIZEFULLSCREEN:
    case SIZENORMAL:
      iconized = FALSE;
    break;
    case SIZEICONIC:
      iconized = TRUE;
    break;
  }

 if (!iconized)
 {
  wxFrame *frame = (wxFrame *)wx_window;
  if (frame && frame->status_window)
    frame->PositionStatusWindow();

  if (wx_window && wx_window->handle)
    wx_window->OnSize(bad_x, bad_y);
 }
}

BOOL wxFrameWnd::OnClose(void)
{
  if (wx_window) {
    wxWindow *modal;
    modal = wxGetModalWindow(wx_window);
    if (modal && (modal != wx_window))
      return FALSE;

    if (wx_window->OnClose()) {
      return TRUE;
    } else return FALSE;
  }
  return FALSE;
}

BOOL wxFrameWnd::OnCommand(WORD menuId, WORD cmd, HWND WXUNUSED(control))
{
  if (cmd == 0 || cmd == 1 ) { // Can be either a menu command or an accelerator.
    wxFrame *frame = (wxFrame *)wx_window;
    wxMenuBar *mb;

    if (mb = frame->GetMenuBar()) {
      wxMenuItem *i;
      i = mb->FindItemForMenuId(menuId);
      if (i) {
	if (i->checkable)
	  mb->Check(i->itemId, !mb->Checked(i->itemId));
    
	((wxFrame *)wx_window)->Command(i->itemId);
	return TRUE;
      }
    }
  }

  return FALSE;
}

void wxFrameWnd::OnMenuClick(WPARAM mnu)
{
  wxFrame *frame = (wxFrame *)wx_window;
  frame->OnMenuClick();
}

void wxFrameWnd::OnMenuSelect(WORD nItem, WORD nFlags, HMENU hSysMenu)
{
  wxFrame *frame = (wxFrame *)wx_window;
  if (nFlags == 0xFFFF && hSysMenu == NULL)
    frame->OnMenuSelect(-1);
  else if (nFlags != MF_SEPARATOR)
    frame->OnMenuSelect(nItem);
}

BOOL wxFrameWnd::ProcessMessage(MSG* pMsg)
{
  return FALSE;
}

void wxFrameWnd::GetMinMaxInfo(MINMAXINFO *_mmi)
{
  wxWnd::GetMinMaxInfo(_mmi);
  if (mmi.ptMinTrackSize.x > 0)
    _mmi->ptMinTrackSize.x = mmi.ptMinTrackSize.x;
  if (mmi.ptMinTrackSize.y > 0)
    _mmi->ptMinTrackSize.y = mmi.ptMinTrackSize.y;
  if (mmi.ptMaxTrackSize.x > 0)
    _mmi->ptMaxTrackSize.x = mmi.ptMaxTrackSize.x;
  if (mmi.ptMaxTrackSize.y > 0)
    _mmi->ptMaxTrackSize.y = mmi.ptMaxTrackSize.y;
}

/*
 * Windows MDI stuff
 */

wxMDIFrame::wxMDIFrame(wxWnd *parent, wxWindow *wx_win, char *title,
                   int x, int y, int width, int height, long style)
{
  DWORD msflags = WS_OVERLAPPED;

  defaultIcon = wxSTD_FRAME_ICON;
  icon = NULL;
  iconized = FALSE;
  parent_frame_active = TRUE;
  current_child = NULL;

  window_menu = ::LoadMenu(wxhInstance, "wxDefaultMenu");
  
  if (!(style & wxNO_RESIZE_BORDER)) {
    msflags |= WS_MAXIMIZEBOX;
  }
  if (!(style & wxNO_THICK_FRAME))
    msflags |= WS_THICKFRAME;
  if (!(style & wxNO_SYSTEM_MENU))
    msflags |= WS_SYSMENU;
  if (style & wxMINIMIZE)
    msflags |= WS_MINIMIZE;
  if (style & wxMAXIMIZE)
    msflags |= WS_MAXIMIZE;
  if (!(style & wxNO_CAPTION)) {
    msflags |= WS_CAPTION;
    msflags |= WS_MINIMIZEBOX;
  }

  Create(parent, wxMDIFrameClassName, wx_win, title, x, y, width, height,
         msflags);
}

wxMDIFrame::~wxMDIFrame(void)
{
  wxwmDestroyMenu(window_menu); // Destroy dummy "Window" menu
}

BOOL wxMDIFrame::OnDestroy(void)
{
  return FALSE;
}

void wxMDIFrame::OnCreate(LPCREATESTRUCT WXUNUSED(cs))
{
  CLIENTCREATESTRUCT ccs;
	
  ccs.hWindowMenu = window_menu;
  ccs.idFirstChild = wxFIRST_MDI_CHILD;

  client_hwnd = wxwmCreateWindowEx(0, "mdiclient", NULL,
				   WS_VISIBLE | WS_CHILD | WS_CLIPCHILDREN, 0, 0, 0, 0, 
				   handle, NULL,
				   wxhInstance, (LPSTR)(LPCLIENTCREATESTRUCT)&ccs);
}

void wxMDIFrame::OnSize(int bad_x, int bad_y, UINT id)
{
  switch (id)
  {
    case SIZEFULLSCREEN:
    case SIZENORMAL:
      iconized = FALSE;
    break;
    case SIZEICONIC:
      iconized = TRUE;
    break;
  }

 if (!iconized)
 {
  wxFrame *frame = (wxFrame *)wx_window;

  if (frame && (frame->status_window))
  {
    RECT rect;
    int cwidth, cheight, ctop = 0;

    GetClientRect(handle, &rect);
    cwidth = rect.right;
    cheight = rect.bottom;

    cheight -= frame->status_window->height;

    MoveWindow(client_hwnd, 0, ctop, cwidth, cheight, TRUE);

    frame->PositionStatusWindow();
  } else 
    (void)DefWindowProc(last_msg, last_wparam, last_lparam);

  if (wx_window && wx_window->handle)
    wx_window->OnSize(bad_x, bad_y);
  }
}

BOOL wxMDIFrame::OnCommand(WORD id, WORD cmd, HWND control)
{
  if (parent_frame_active) {
    return wxFrameWnd::OnCommand(id, cmd, control);
  } else if (current_child) {
    return current_child->OnCommand(id, cmd, control);
  }
  
  return FALSE;
}

void wxMDIFrame::OnMenuSelect(WORD nItem, WORD nFlags, HMENU hSysMenu)
{
  if (parent_frame_active) {
    wxFrameWnd::OnMenuSelect(nItem, nFlags, hSysMenu);
  } else if (current_child) {
    current_child->OnMenuSelect(nItem, nFlags, hSysMenu);
  }
}

long wxMDIFrame::DefWindowProc(UINT message, WPARAM wParam, LPARAM lParam)
{
  return ::DefFrameProcW(handle, client_hwnd, message, wParam, lParam);
}

long wxMDIFrame::Propagate(UINT nMsg, WPARAM wParam, LPARAM lParam)
{
  if ((nMsg == WM_SETFOCUS) || (nMsg == WM_SIZE))
    return DefWindowProc(nMsg, wParam, lParam);
  else
    return 0;
}

BOOL wxMDIFrame::ProcessMessage(MSG* pMsg)
{
  if ((current_child != NULL) 
      && (current_child->handle != NULL)
      && current_child->ProcessMessage(pMsg))
    return TRUE;

#if 0	
  if (accelerator_table != NULL &&
      ::TranslateAccelerator(handle, (HACCEL)accelerator_table, pMsg))
    return TRUE;
	
  if (pMsg->message == WM_KEYDOWN || pMsg->message == WM_SYSKEYDOWN)
  {
    if (::TranslateMDISysAccel(client_hwnd, pMsg))
      return TRUE;
  }
#endif

  return FALSE;
}

BOOL wxMDIFrame::OnEraseBkgnd(HDC WXUNUSED(pDC))
{
  return TRUE;
}

extern wxWnd *wxWndHook;
extern wxNonlockingHashTable *wxWinHandleList;

wxMDIChild::wxMDIChild(wxMDIFrame *parent, wxWindow *wx_win, char *title,
                   int x, int y, int width, int height, long style)
{
  int mcs_x, mcs_y, mcs_cx, mcs_cy;
  DWORD msflags = WS_OVERLAPPED;
  HWND o;

  defaultIcon = wxSTD_FRAME_ICON;
  icon = NULL;
  iconized = FALSE;
  wx_window = wx_win;
  active = FALSE;
  is_dialog = FALSE;

  wxWndHook = this;

  if (x > -1) mcs_x = x;
  else mcs_x = CW_USEDEFAULT;

  if (y > -1) mcs_y = y;
  else mcs_y = CW_USEDEFAULT;

  if (width > -1) mcs_cx = width;
  else mcs_cx = 1;

  if (height > -1) mcs_cy = height;
  else mcs_cy = 1;

  if (!(style & wxNO_RESIZE_BORDER)) {
    msflags |= WS_MAXIMIZEBOX;
  }
  if (!(style & wxNO_THICK_FRAME))
    msflags |= WS_THICKFRAME;
  if (!(style & wxNO_SYSTEM_MENU))
    msflags |= WS_SYSMENU;
  if (style & wxMINIMIZE)
    msflags |= WS_MINIMIZE;
  if (style & wxMAXIMIZE)
    msflags |= WS_MAXIMIZE;
  if (!(style & wxNO_CAPTION)) {
    msflags |= WS_MINIMIZEBOX;
    msflags |= WS_CAPTION;
  }

  o = (HWND)::SendMessage(parent->client_hwnd, WM_MDIGETACTIVE, 0, 0);

  // turn off redrawing in the MDI client window
  SendMessage(parent->client_hwnd, WM_SETREDRAW, FALSE, 0L);
  
  {
    wchar_t *ws, *ws2;
    ws = wxWIDE_STRING_COPY(wxMDIChildFrameClassName);
    ws2 = wxWIDE_STRING(title);
    handle = CreateMDIWindowW(ws, ws2, msflags,
			      mcs_x, mcs_y, mcs_cx, mcs_cy, 
			      parent->client_hwnd,
			      wxhInstance, 0);
  }

  wxWndHook = NULL;
  wxWinHandleList->Append((long)handle, this);
#ifndef MZ_PRECISE_GC
  SetWindowLong(handle, 0, (long)this);
#endif
  
  ShowWindow(handle, SW_HIDE);

  // turn redrawing in the MDI client back on,
  // and force an immediate update
  SendMessage(parent->client_hwnd, WM_SETREDRAW, TRUE, 0L);
  InvalidateRect(parent->client_hwnd, NULL, TRUE);
  UpdateWindow(parent->client_hwnd);

  if (o) {
    InvalidateRect(o, NULL, TRUE); /* Because focus moved. */
    wxwmBringWindowToTop(o);
  }
}

static HWND invalidHandle = 0;
void wxMDIChild::OnSize(int bad_x, int bad_y, UINT id)
{
  if (!handle) return;

  if (invalidHandle == handle)
    return;
  
  (void)DefWindowProc(last_msg, last_wparam, last_lparam);
  
  switch (id)
  {
    case SIZEFULLSCREEN:
    case SIZENORMAL:
      iconized = FALSE;
    break;
    case SIZEICONIC:
      iconized = TRUE;
    break;
  }

 if (!iconized)
 {
  wxFrame *frame = (wxFrame *)wx_window;
  if (frame && frame->status_window)
    frame->PositionStatusWindow();

  if (wx_window && wx_window->handle)
    wx_window->OnSize(bad_x, bad_y);
 }
}

BOOL wxMDIChild::OnClose(void)
{
  if (wx_window && handle) {
    if (wx_window->OnClose()) {
      return TRUE;
    } else 
      return FALSE;
  }
  return FALSE;
}

BOOL wxMDIChild::OnCommand(WORD id, WORD cmd, HWND control)
{
  return wxFrameWnd::OnCommand(id, cmd, control);
}

long wxMDIChild::DefWindowProc(UINT message, UINT wParam, LONG lParam)
{
  if (handle)
    return ::DefMDIChildProcW(handle, message, wParam, lParam);
  else
    return 0;
}

long wxMDIChild::Propagate(UINT nMsg, WPARAM wParam, LPARAM lParam)
{
  if ((nMsg == WM_SETFOCUS) || (nMsg == WM_MOVE))
    return DefWindowProc(nMsg, wParam, lParam);
  else
    return 0;
}

BOOL wxMDIChild::ProcessMessage(MSG *msg)
{
  return FALSE;
}

BOOL wxMDIChild::OnMDIActivate(BOOL bActivate, HWND WXUNUSED(one), HWND WXUNUSED(two))
{
  wxFrame *parent, *child;
  HMENU new_menu, child_menu;
  wxMDIFrame *cparent;

  parent = (wxFrame *)wx_window->GetParent();
  child = (wxFrame *)wx_window;

  cparent = (wxMDIFrame *)parent->handle;
  if (bActivate) {
    if (child->IsShown()) {
      active = TRUE;
      cparent->current_child = this;
      if (child)
	child_menu = child->GetWinMenu();
      else
	child_menu = NULL;
    } else {
      cparent->current_child = NULL;
      child_menu = NULL;
    }
  } else {
    if (cparent->current_child == this)
      cparent->current_child = NULL;
    active = FALSE;
    child_menu = NULL;
  }

  if (child_menu) {
    cparent->parent_frame_active = FALSE;
    new_menu = child_menu;
  } else {
    cparent->parent_frame_active = TRUE;
    new_menu = parent->GetWinMenu();

    if (!new_menu) {
      if (!emptyMenu)
	emptyMenu = wxwmCreateMenu();
      new_menu = emptyMenu;
    }
  }

  ::SendMessage(cparent->client_hwnd, WM_MDISETMENU,
		(WPARAM)new_menu,
		(LPARAM)NULL);

  ::SendMessage(cparent->client_hwnd, WM_MDIACTIVATE,
		(WPARAM)handle,
		(LPARAM)NULL);

  ::DrawMenuBar(cparent->handle);

  if (child->IsShown())
    child->OnMDIActivate(bActivate);
  
  return 0;
}

BOOL wxMDIChild::OnNCActivate(BOOL bActivate, HWND WXUNUSED(one))
{
  if (wx_window->IsShown())
    return wxFrameWnd::OnActivate(bActivate, 0, 0);
  else
    return 1;
}

void wxMDIChild::OnChar(WORD wParam, LPARAM lParam, Bool isASCII, Bool isRelease)
{
  if (!wx_window || wx_window->IsShown())
    wxFrameWnd::OnChar(wParam, lParam, isASCII, isRelease);
  else {
    wxFrame *parent;
    wxMDIFrame *cparent;

    parent = (wxFrame *)wx_window->GetParent();
    cparent = (wxMDIFrame *)parent->handle;

    cparent->OnChar(wParam, lParam, isASCII, isRelease);
  }
}

wxMDIChild::~wxMDIChild(void)
{
}

void wxMDIChild::DestroyWindow(void)
{
  wxFrame *parent;
 wxMDIFrame *cparent;

  DetachWindowMenu();
  invalidHandle = handle;

  parent = (wxFrame *)wx_window->GetParent();
  cparent = (wxMDIFrame *)parent->handle;

  // Must make sure this handle is invalidated (set to NULL)
  // since all sorts of things could happen after the
  // child client is destroyed, but before the wxFrame is
  // destroyed.

  {
    HWND oldHandle = (HWND)handle;
    SendMessage(cparent->client_hwnd, WM_MDIDESTROY, (WPARAM)oldHandle, (LPARAM)0);
  }

  invalidHandle = 0;

  if (hMenu) {
    wxwmDestroyMenu(hMenu);
    hMenu = 0;
  }
}
