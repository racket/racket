/*
 * File:	wx_gauge.cc
 * Purpose:	Gauge implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	April 1995
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include <commctrl.h>

wxGauge::wxGauge(wxPanel *panel, char *label,
		 int range, int x, int y, int width, int height,
		 long style, wxFont *_font, char *name):
  wxbGauge(panel, label, range, x, y, width, height, style, name)
{
  SetFont(_font);
  Create(panel, label, range, x, y, width, height, style, name);
}

Bool wxGauge::Create(wxPanel *panel, char *label,
		     int range, int x, int y, int width, int height,
		     long style, char *name)
{
  INITCOMMONCONTROLSEX icex;
  wxWnd *cparent;
  HWND wx_button;

  icex.dwSize = sizeof(INITCOMMONCONTROLSEX);
  icex.dwICC  = ICC_PROGRESS_CLASS;
  InitCommonControlsEx(&icex);

  panel->AddChild(this);

  static_label = 0;
  wxWinType = wxTYPE_HWND;
  windowStyle = style;

  cparent = (wxWnd *)(panel->handle);

  if (style & wxVERTICAL_LABEL)
    labelPosition = wxVERTICAL;
  else if (style & wxHORIZONTAL_LABEL)
    labelPosition = wxHORIZONTAL;
  else
    labelPosition = panel->label_position;
  panel->GetValidPosition(&x, &y);

  // If label exists, create a static control for it.
  if (label) {
    int nid;
    wchar_t *ws;

    nid = NewId(this);
    ws = wxWIDE_STRING(label);
    static_label = CreateWindowExW(0, LSTATIC_CLASS, ws,
				   STATIC_FLAGS | WS_CLIPSIBLINGS,
				   0, 0, 0, 0, cparent->handle, (HMENU)nid,
				   wxhInstance, NULL);

    wxSetWinFont(font, (HANDLE)static_label);
  } else
    static_label = NULL;

  windows_id = NewId(this);
  
  wx_button =
    wxwmCreateWindowEx(0, PROGRESS_CLASS, label, 
		       WS_CHILD | WS_TABSTOP | WS_CLIPSIBLINGS
		       | ((windowStyle & wxHORIZONTAL) ? 0 : PBS_VERTICAL),
		       0, 0, 0, 0, cparent->handle, (HMENU)windows_id,
		       wxhInstance, NULL);
  
  ms_handle = (HANDLE)wx_button;
  
  SubclassControl(wx_button);

  SendMessage(wx_button, PBM_SETRANGE, 0, MAKELPARAM(0, range));

  wxSetWinFont(font, ms_handle);

  SetSize(x, y, width, height, wxSIZE_AUTO);

  if (static_label)
    BringWindowToTop(static_label);

  if (!(style & wxINVISIBLE)) {
    ShowWindow(wx_button, SW_SHOW);
    if (static_label)
      ShowWindow(static_label, SW_SHOW);
  }

  panel->AdvanceCursor(this);

  if (style & wxINVISIBLE)
    Show(FALSE);

  return TRUE;
}

wxGauge::~wxGauge(void)
{
  isBeingDeleted = TRUE;
  
  if (static_label)
    wxwmDestroyWindow(static_label);
  static_label = NULL;
}

void wxGauge::SetSize(int x, int y, int width, int height, int sizeFlags)
{
  int currentX, currentY;
  int clx; // label font dimensions
  int cly;
  double label_width, label_height, label_x, label_y;
  double control_width, control_height, control_x, control_y;
  int defwidth, defheight;

  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  // If we're prepared to use the existing size, then...
  if (width == -1 && height == -1 && ((sizeFlags & wxSIZE_AUTO) != wxSIZE_AUTO))
    GetSize(&width, &height);

  defwidth = ((windowStyle & wxHORIZONTAL) ? 100 : 24);
  defheight = ((windowStyle & wxHORIZONTAL) ? 24 : 100);

  if (static_label) {
    // Find size of label
    wxGetCharSize((HWND)ms_handle, &clx, &cly, font);
    GetWindowTextW(static_label, (wchar_t *)wxBuffer, 300);
    GetLabelExtent(wxStripMenuCodes(wxNARROW_STRING((wchar_t *)wxBuffer)), 
		   &label_width, &label_height);

    // Given size is total label + edit size, find individual
    // control sizes on that basis.
    if (labelPosition == wxHORIZONTAL) {
      label_x = (double)x;
      label_y = (double)y;

      control_x = label_x + label_width + clx;
      control_y = (double)y;
      if (width <= 0)
	control_width = defwidth;
      else
	control_width = width - (control_x - label_x);
      if (height <= 0)
	control_height = defheight;
      else
	control_height = (double)height;
    } else { // wxVERTICAL
      label_x = (double)x;
      label_y = (double)y;

      control_x = (double)x;
      control_y = label_y + label_height + 3;
      if (width <= 0)
	control_width = defwidth;
      else
	control_width = (double)width;
      if (height <= 0)
	control_height = defheight;
      else
	control_height = height - (label_height + 3);
    }

    MoveWindow(static_label, (int)label_x, (int)label_y,
               (int)label_width, (int)label_height, TRUE);
  } else {
    // Deal with default size (using -1 values)
    if (width <= 0)
      width = defwidth;
    if (height <= 0)
      height = defheight;

    control_x = (double)x;
    control_y = (double)y;
    control_width = (double)width;
    control_height = (double)height;
  }

  MoveWindow((HWND)ms_handle, (int)control_x, (int)control_y, 
	     (int)control_width, (int)control_height, TRUE);
  
  OnSize(width, height);
}

void wxGauge::GetSize(int *width, int *height)
{
  RECT rect;

  rect.left = -1; rect.right = -1; rect.top = -1; rect.bottom = -1;

  wxFindMaxSize((HWND)ms_handle, &rect);

  if (static_label)
  {
    wxFindMaxSize(static_label, &rect);
  }

  *width = rect.right - rect.left;
  *height = rect.bottom - rect.top;
}

void wxGauge::GetPosition(int *x, int *y)
{
  wxWindow *parent;
  RECT rect;
  POINT point;

  parent = GetParent();

  rect.left = -1; rect.right = -1; rect.top = -1; rect.bottom = -1;

  wxFindMaxSize((HWND)ms_handle, &rect);
  if (static_label)
    wxFindMaxSize(static_label, &rect);

  // Since we now have the absolute screen coords,
  // if there's a parent we must subtract its top left corner
  point.x = rect.left;
  point.y = rect.top;
  if (parent)
  {
    wxWnd *cparent = (wxWnd *)(parent->handle);
    ::ScreenToClient(cparent->handle, &point);
  }

  *x = point.x;
  *y = point.y;
}

Bool wxGauge::Show(Bool show)
{
  wxWindow::Show(show);
  if (static_label) 
    ShowWindow(static_label, show ? SW_SHOW : SW_HIDE);
  return TRUE;
}

void wxGauge::SetRange(int r)
{
  SendMessage((HWND)ms_handle, PBM_SETRANGE, 0, MAKELPARAM(0, r));
}

void wxGauge::SetValue(int pos)
{
  SendMessage((HWND)ms_handle, PBM_SETPOS, pos, 0);
}

void wxGauge::SetLabel(char *label)
{
  if (static_label)
  {
    double w, h;
    RECT rect;
    POINT point;
    wxWindow *parent;

    parent = GetParent();
    GetWindowRect(static_label, &rect);

    // Since we now have the absolute screen coords,
    // if there's a parent we must subtract its top left corner
    point.x = rect.left;
    point.y = rect.top;
    if (parent) {
      wxWnd *cparent = (wxWnd *)(parent->handle);
      ::ScreenToClient(cparent->handle, &point);
    }

    GetLabelExtent(wxStripMenuCodes(label), &w, &h);
    MoveWindow(static_label, point.x, point.y, (int)(w + 10), (int)h,
               TRUE);
    SetWindowTextW(static_label, wxWIDE_STRING(label));
  }
}

void wxGauge::ChangeToGray(Bool gray)
{
  wxWindow::ChangeToGray(gray);
  if (static_label)
    ::EnableWindow(static_label, !gray);
}
