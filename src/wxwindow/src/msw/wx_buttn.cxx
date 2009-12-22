/*
 * File:	wx_buttn.cc
 * Purpose:	Button implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	April 1995
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#define BUTTON_HEIGHT_FACTOR (EDIT_CONTROL_FACTOR * 1.1)

// Buttons

BOOL wxButton::MSWCommand(UINT param, WORD WXUNUSED(id))
{
  if (param == BN_CLICKED) {
    wxCommandEvent *event;
    event = new wxCommandEvent(wxEVENT_TYPE_BUTTON_COMMAND);
    ProcessCommand(event);
    return TRUE;
  } else
    return FALSE;
}

wxButton::wxButton(wxPanel *panel, wxFunction Function, char *label,
		   int x, int y, int width, int height,
                   long style, wxFont *_font, char *name):
  wxbButton(panel, Function, label, x, y, width, height, style, name)
{
  SetFont(_font);
  Create(panel, Function, label, NULL, x, y, width, height, style, name);
}

wxButton::wxButton(wxPanel *panel, wxFunction Function, wxBitmap *bitmap,
		   int x, int y, int width, int height,
                   long style, wxFont *_font, char *name):
  wxbButton(panel, Function, bitmap, x, y, width, height, style, name)
{
  SetFont(_font);
  Create(panel, Function, NULL, bitmap, x, y, width, height, style, name);
}

Bool wxButton::Create(wxPanel *panel, wxFunction Function, 
		      char *label, wxBitmap *bitmap,
		      int x, int y, int width, int height,
		      long style, char *name)
{
  wxWnd *cparent;
  HWND wx_button;
  
  if (bitmap) {
    if (!bitmap->Ok() || (bitmap->selectedIntoDC < 0))
      return Create(panel, Function, "<bad-image>", NULL, x, y, width, height, style, name);
    
    bitmap->selectedIntoDC++;
    bm_label = bitmap;
  }

  panel->AddChild(this);
  
  wxWinType = wxTYPE_HWND;

  cparent = (wxWnd *)(panel->handle);

  panel->GetValidPosition(&x, &y);

  windows_id = NewId(this);

  if (bitmap) {
    HBITMAP lbm;
    if (width < 0)
      width = bitmap->GetWidth();
    if (height < 0)
      height = bitmap->GetHeight();
    width += FB_MARGIN;
    height+= FB_MARGIN;
    
    wx_button =
      wxwmCreateWindowEx(0, FafaButt, "?", 
			 FB_BITMAP | WS_CHILD | WS_CLIPSIBLINGS,
			 0, 0, 0, 0, cparent->handle, (HMENU)windows_id,
			 wxhInstance, NULL);
    lbm = bitmap->GetLabelBitmap(0);
    SetBitmapDimensionEx(lbm,
			 bitmap->GetWidth(),
			 bitmap->GetHeight(),
			 NULL);
    SendMessage((HWND)wx_button, WM_CHANGEBITMAP,
		(WPARAM)0xFFFF,
		(LPARAM)lbm);
  } else {
    wx_button =
      CreateWindowExW(0, L"wxBUTTON", wxWIDE_STRING(label), 
		      (((style & 1) ? BS_DEFPUSHBUTTON : BS_PUSHBUTTON)
		       | WS_CHILD | WS_CLIPSIBLINGS),
		      0, 0, 0, 0, cparent->handle, (HMENU)windows_id,
		      wxhInstance, NULL);
  }

  ms_handle = (HANDLE)wx_button;

  SubclassControl(wx_button);

  wxSetWinFont(font, ms_handle);

  SetSize(x, y, width, height);

  Callback(Function);

  if (!(style & wxINVISIBLE))
    ShowWindow(wx_button, SW_SHOW);

  panel->AdvanceCursor(this);

  if (style & wxINVISIBLE)
    Show(FALSE);

  return TRUE;
}

wxButton::~wxButton(void)
{
  if (bm_label) {
    --bm_label->selectedIntoDC;
    bm_label->ReleaseLabel();
    bm_label = NULL;
  }
}

void wxButton::SetLabel(char *label)
{
  if (bm_label)
      return;

  // This message will switch from FB_BITMAP style to FB_TEXT, if needed.
  SendMessage((HWND)ms_handle,WM_CHANGEBITMAP,
	      (WPARAM)0,
	      (LPARAM)NULL);
  
  SetWindowTextW((HWND)ms_handle, wxWIDE_STRING(label));
}

void wxButton::SetLabel(wxBitmap *bitmap)
{
  HBITMAP lbm;

  if (!bm_label || !bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return;

  --bm_label->selectedIntoDC;
  bm_label->ReleaseLabel();
  bm_label = bitmap;
  bm_label->selectedIntoDC++;

  lbm = bitmap->GetLabelBitmap(0);
  SetBitmapDimensionEx(lbm,
		       bitmap->GetWidth(),
		       bitmap->GetHeight(),
		       NULL);
  SendMessage((HWND)ms_handle,WM_CHANGEBITMAP,
	      (WPARAM)0xFFFF/*((bitmap->GetHeight()<<8)+bitmap->GetWidth())*/,
	      (LPARAM)lbm);
}

char *wxButton::GetLabel(void)
{
  GetWindowText((HWND)ms_handle, wxBuffer, 300);
  return wxBuffer;
}

void wxButton::SetSize(int x, int y, int width, int height, int sizeFlags)
{
  HWND button = (HWND)ms_handle;
  int currentX, currentY;
  int actualWidth = width;
  int actualHeight = height;
  int ww, hh;
  double current_width;
  double cyf;
  wchar_t buf[300];

  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  GetSize(&ww, &hh);

  GetWindowTextW(button, buf, 300);
  GetLabelExtent(wxStripMenuCodes(wxNARROW_STRING(buf)), &current_width, &cyf);

  // If we're prepared to use the existing width, then...
  if (width == -1 && ((sizeFlags & wxSIZE_AUTO_WIDTH) != wxSIZE_AUTO_WIDTH))
    actualWidth = ww;
  else if (width == -1)
  {
    int cx;
    int cy;
    wxGetCharSize(button, &cx, &cy,font);
    actualWidth = (int)(current_width + 3*cx) ;
  }
  
  // If we're prepared to use the existing height, then...
  if (height == -1 && ((sizeFlags & wxSIZE_AUTO_HEIGHT) != wxSIZE_AUTO_HEIGHT))
    actualHeight = hh;
  else if (height == -1)
  {
    actualHeight = (int)(cyf*BUTTON_HEIGHT_FACTOR) ;
  }

  MoveWindow(button, x, y, actualWidth, actualHeight, TRUE);

  if (!((width == -1) && (height == -1)))
    OnSize(width, height);
}

void wxButton::SetDefault(void)
{
  wxPanel *panel;
  wxWnd *wnd;

  panel = (wxPanel *)GetParent();
  panel->defaultItem = this;

  wnd = (wxWnd *)panel->handle;
  SendMessage(wnd->handle, DM_SETDEFID, windows_id, 0L);
}

void wxButton::SetBorder(Bool on)
{
  SendMessage((HWND)ms_handle, BM_SETSTYLE, on ? BS_DEFPUSHBUTTON : BS_PUSHBUTTON, TRUE);
}
