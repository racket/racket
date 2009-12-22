/*
 * File:	wx_panel.cc
 * Purpose:	wxPanel class implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

extern char wxPanelClassName[];

class wxPanelWnd : public wxSubWnd
{
public:
  wxPanelWnd(wxWnd *parent, char *winClass, wxWindow *wx_win,
	     int x, int y, int width, int height, DWORD flags, 
	     DWORD exflags);

  // Handlers
  LONG DefWindowProc(UINT nMsg, UINT wParam, LONG lParam);
  BOOL ProcessMessage(MSG* pMsg);
  BOOL OnEraseBkgnd(HDC pDC);
};

wxPanelWnd::wxPanelWnd(wxWnd *parent, char *winClass, wxWindow *wx_win,
		       int x, int y, int width, int height, DWORD flags,
		       DWORD extendedStyle)
  : wxSubWnd(parent, winClass, wx_win, x, y, width, height, flags, 
	     NULL, extendedStyle)
{
  is_panel = 1;
}

LONG wxPanelWnd::DefWindowProc(UINT nMsg, UINT wParam, LONG lParam)
{
  return ::DefWindowProc(handle, nMsg, wParam, lParam);
}

BOOL wxPanelWnd::ProcessMessage(MSG* pMsg)
{
  return FALSE;
}

BOOL wxPanelWnd::OnEraseBkgnd(HDC pDC)
{
  return FALSE;
}

wxPanel::wxPanel(void)
{
  window_parent = NULL;
  cursor_x = PANEL_LEFT_MARGIN;
  cursor_y = PANEL_TOP_MARGIN;
  max_height = 0;
  max_line_height = 0;
  max_width = 0;
  hSpacing = PANEL_HSPACING;
  vSpacing = PANEL_VSPACING;
  initial_hspacing = hSpacing;
  initial_vspacing = vSpacing;
  current_hspacing = hSpacing;
  current_vspacing = vSpacing;

  new_line = FALSE;
  label_position = wxHORIZONTAL;
  wxWinType = wxTYPE_XWND;
  handle = NULL;
  has_child = FALSE;
  last_created = 0;
}

// Constructor
wxPanel::wxPanel(wxWindow *parent, int x, int y, int width, int height,
                 long style, char *name):
  wxbPanel(parent, x, y, width, height, style, name)
{
  wx_dc = NULL;

  Create(parent, x, y, width, height, style, name);
}

// Constructor
Bool wxPanel::Create(wxWindow *parent, int x, int y, int width, int height, long style,
                     char *name)
{
  wxPanelWnd *wnd;
  wxWnd *cparent;
  DWORD msflags = 0, exflags = 0;
  
  if (!parent)
    return FALSE;

  if (wxSubType(parent->__type, wxTYPE_PANEL)) {
    wxPanel *parentPanel = (wxPanel *)parent;
    parentPanel->GetValidPosition(&x,&y);
  }

  cursor_x = PANEL_LEFT_MARGIN;
  cursor_y = PANEL_TOP_MARGIN;
  max_height = 0;
  max_line_height = 0;
  max_width = 0;
  hSpacing = PANEL_HSPACING;
  vSpacing = PANEL_VSPACING;
  initial_hspacing = hSpacing;
  initial_vspacing = vSpacing;
  current_hspacing = hSpacing;
  current_vspacing = vSpacing;
  new_line = FALSE;
  label_position = wxHORIZONTAL;
  wxWinType = wxTYPE_XWND;
  windowStyle = style;
  has_child = FALSE;
  last_created = 0;

  window_parent = parent;

  cparent = NULL;
  if (parent)
    cparent = (wxWnd *)parent->handle;

  if (style & wxBORDER)
    exflags |= WS_EX_STATICEDGE;
  msflags |= WS_CHILD | WS_CLIPSIBLINGS | ((style & wxINVISIBLE) ? 0 : WS_VISIBLE);

  wnd = new wxPanelWnd(cparent, wxPanelClassName, this, 
		       wxNEG_POS_IS_DEFAULT(x), wxNEG_POS_IS_DEFAULT(y),
		       width, height, 
		       msflags, exflags);

  handle = (char *)wnd;
  if (parent) parent->AddChild(this);

  if (wxSubType(parent->__type, wxTYPE_PANEL))
    ((wxPanel *)parent)->AdvanceCursor(this);
  else
    InitEnable();

  if (style & wxINVISIBLE)
    Show(FALSE);

  return TRUE;
}

wxPanel::~wxPanel(void)
{
}

void wxPanel::SetLabelPosition(int pos)  // wxHORIZONTAL or wxVERTICAL
{
  label_position = pos;
}

void wxPanel::SetSize(int x, int y, int w, int h, int WXUNUSED(sizeFlags))
{
  int currentX, currentY;
  wxWnd *wnd;
  int currentW,currentH;

  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  GetSize(&currentW, &currentH);
  if (w == -1)
    w = currentW;
  if (h == -1)
    h = currentH;

  wnd = (wxWnd *)handle;
  if (wnd)
    MoveWindow(wnd->handle, x, y, w, h, TRUE);

  OnSize(w, h);
}

/*****************************************************************
 * ITEM PLACEMENT FUNCTIONS
 *****************************************************************/

 // Start a new line
void wxPanel::RealNewLine(void)
{
  //cursor_x = PANEL_LEFT_MARGIN;
  cursor_x = initial_hspacing;
  if (max_line_height == 0)
  {
    cursor_y += current_vspacing;
  }
  else
    cursor_y += current_vspacing + max_line_height;
  max_line_height = 0;
  new_line = FALSE;
}
  
void wxPanel::NewLine(int pixels)
{
  if (new_line)
    current_vspacing += pixels;
  else
    current_vspacing = pixels;
  new_line = TRUE;
}

void wxPanel::NewLine(void)
{
  if (new_line)
    current_vspacing += vSpacing;
  else
    current_vspacing = vSpacing;
  new_line = TRUE;
}

void wxPanel::GetCursor(int *x, int *y)
{
  RealAdvanceCursor();
  *x = cursor_x;
  *y = cursor_y;
}

void wxPanel::SetItemCursor(int x, int y)
{
  last_created = NULL;
  new_line = FALSE;
  cursor_x = x;
  cursor_y = y;
}

// Fits the panel around the items
void wxPanel::Fit(void)
{
  RealAdvanceCursor();
  SetClientSize(max_width + initial_hspacing,
                max_height + initial_vspacing);
}

// Update next cursor position
void wxPanel::RealAdvanceCursor(void)
{
  wxWindow *item = last_created;
  if (item)
  {
    int width, height;
    int x, y;
    item->GetSize(&width, &height);
    item->GetPosition(&x, &y);

    if ((x + width) > max_width)
      max_width = x + width;
    if ((y + height) > max_height)
      max_height = y + height;
    if (height > max_line_height)
      max_line_height = height;

    cursor_x = x + width + current_hspacing;
    cursor_y = y;
    last_created = NULL;
  }
  if (new_line)
    RealNewLine();
}


// Update next cursor position
void wxPanel::AdvanceCursor(wxWindow *item)
{
  item->InitEnable();

  last_created = item;
}

// If x or y are not specified (i.e. < 0), supply
// values based on left to right, top to bottom layout.
// Internal use only.
void wxPanel::GetValidPosition(int *x, int *y)
{
  if (*x < 0)
    *x = cursor_x;

  if (*y < 0)
    *y = cursor_y;
}

void wxPanel::Centre(int direction)
{
  int x, y, width, height, panel_width, panel_height, new_x, new_y;
  wxPanel *father;

  father = (wxPanel *)GetParent();
  if (!father)
    return;

  father->GetClientSize(&panel_width, &panel_height);
  GetSize(&width, &height);
  GetPosition(&x, &y);

  new_x = -1;
  new_y = -1;

  if (direction & wxHORIZONTAL)
    new_x = (int)((panel_width - width)/2);

  if (direction & wxVERTICAL)
    new_y = (int)((panel_height - height)/2);

  SetSize(new_x, new_y, -1, -1);

}

void wxPanel::ChangeToGray(Bool gray)
{
  wxWindow::ChangeToGray(gray);
  InternalGrayChildren(gray);
}

void wxPanel::AddChild(wxObject *child)
{
  if (!has_child)
  {
    initial_hspacing = hSpacing;
    initial_vspacing = vSpacing;
  }
  has_child = TRUE;

  cursor_x = hSpacing;
  cursor_y = vSpacing;
  RealAdvanceCursor();
  current_hspacing = hSpacing;
  current_vspacing = vSpacing;

  children->Append(child);
}

void wxPanel::OnPaint(void)
{
}
