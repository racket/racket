/*
 * File:	wx_check.cc
 * Purpose:	Check box implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	April 1995
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

BOOL wxCheckBox::MSWCommand(UINT param, WORD WXUNUSED(id))
{
  if (param == BN_CLICKED) {
    wxCommandEvent *event;
    event = new wxCommandEvent(wxEVENT_TYPE_CHECKBOX_COMMAND);
    ProcessCommand(event);
    return TRUE;
  }
  return FALSE;
}

// Single check box item
wxCheckBox::wxCheckBox(wxPanel *panel, wxFunction func, char *Title,
                       int x, int y, int width, int height, long style, 
		       wxFont *_font, char *name):
  wxbCheckBox(panel, func, Title, x, y, width, height, style, name)
{
  SetFont(_font);
  Create(panel, func, Title, NULL, x, y, width, height, style, name);
}

wxCheckBox::wxCheckBox(wxPanel *panel, wxFunction func, wxBitmap *bitmap,
                       int x, int y, int width, int height, long style, 
		       wxFont *_font, char *name):
  wxbCheckBox(panel, func, bitmap, x, y, width, height, style, name)
{
  SetFont(_font);
  Create(panel, func, NULL, bitmap, x, y, width, height, style, name);
}

Bool wxCheckBox::Create(wxPanel *panel, wxFunction func, char *Title, wxBitmap *bitmap,
                       int x, int y, int width, int height, long style, char *name)
{
  wxWnd *cparent = NULL;
  HWND wx_button;

  if (bitmap) {
    if (!bitmap->Ok() || (bitmap->selectedIntoDC < 0))
      return Create(panel, func, "<bad-image>", NULL, x, y, width, height, style, name);
    
    bitmap->selectedIntoDC++;
    bm_label = bitmap;
  } else if (!Title)
    Title = " "; // Apparently needed or checkbox won't show

  panel->AddChild(this);
  wxWinType = wxTYPE_HWND;
  windowStyle = style;
  cparent = (wxWnd *)panel->handle;

  panel->GetValidPosition(&x, &y);

  windows_id = NewId(this);

  if (bitmap) {
    HBITMAP lbm;

    isFafa = TRUE;
    if (width < 0)
      width = bitmap->GetWidth();
    if (height < 0)
      height = bitmap->GetHeight();
    checkWidth = width;
    checkHeight = height;
    width += FB_MARGIN;
    height += FB_MARGIN;

    wx_button = wxwmCreateWindowEx(0, FafaChck, "toggle",
				   BITCHECK_FLAGS | WS_CLIPSIBLINGS,
				   0, 0, 0, 0, cparent->handle, (HMENU)windows_id,
				   wxhInstance, NULL);
    lbm = bitmap->GetLabelBitmap(0);
    SetBitmapDimensionEx(lbm,
			 bitmap->GetWidth(),
			 bitmap->GetHeight(),
			 NULL);
    SendMessage((HWND)wx_button,WM_CHANGEBITMAP,
		(WPARAM)0xFFFF,
		(LPARAM)lbm);
  } else {
    isFafa = FALSE;
    checkWidth = -1;
    checkHeight = -1;
    wx_button = CreateWindowExW(0, L"wxBUTTON", wxWIDE_STRING(Title),
				CHECK_FLAGS | WS_CLIPSIBLINGS,
				0, 0, 0, 0, cparent->handle, (HMENU)windows_id,
				wxhInstance, NULL);
  }

  SubclassControl(wx_button);

  ms_handle = (HANDLE)wx_button;

  wxSetWinFont(font, ms_handle);

  SetSize(x, y, width, height);

  if (!(style & wxINVISIBLE))
    ShowWindow(wx_button, SW_SHOW);

  panel->AdvanceCursor(this);
  Callback(func);
  
  if (style & wxINVISIBLE)
    Show(FALSE);

  return TRUE;
}

wxCheckBox::~wxCheckBox(void)
{
 if (bm_label) {
    --bm_label->selectedIntoDC;
    bm_label->ReleaseLabel();
    bm_label = NULL;
  }
}

void wxCheckBox::SetLabel(char *label)
{
  if (bm_label)
    return;

  checkWidth = checkHeight = -1 ;
  // This message will switch from FB_BITMAP style to FB_TEXT, if needed.
  SendMessage((HWND)ms_handle,WM_CHANGEBITMAP,
	      (WPARAM)0,
	      (LPARAM)NULL);

  SetWindowTextW((HWND)ms_handle, wxWIDE_STRING(label));
}

char *wxCheckBox::GetLabel()
{
  char buf[300];
  GetWindowText((HWND)ms_handle, buf, 300);
  return copystring(buf);
}

void wxCheckBox::SetLabel(wxBitmap *bitmap)
{
  HBITMAP lbm;

  if (!bm_label || !bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return;

  --bm_label->selectedIntoDC;
  bm_label->ReleaseLabel();
  bm_label = bitmap;
  bm_label->selectedIntoDC++;

  checkWidth = bitmap->GetWidth() ;
  checkHeight = bitmap->GetHeight() ;
  lbm = bitmap->GetLabelBitmap(0);
  SetBitmapDimensionEx(lbm,
		       bitmap->GetWidth(),
		       bitmap->GetHeight(),
		       NULL);
  SendMessage((HWND)ms_handle,WM_CHANGEBITMAP,
	      (WPARAM)0xFFFF,
	      (LPARAM)lbm);
}

void wxCheckBox::SetSize(int x, int y, int width, int height, int WXUNUSED(sizeFlags))
{
  int currentX, currentY;
  wchar_t buf[300];
  double current_width;
  int cx;
  int cy;
  double cyf;
  HWND button = (HWND)ms_handle;

  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  if (checkWidth < 0) {
    wxGetCharSize(button, &cx, &cy, font);

    GetWindowTextW(button, buf, 300);
    GetLabelExtent(wxStripMenuCodes(wxNARROW_STRING(buf)), &current_width, &cyf);
    if (width < 0)
      width = (int)(current_width + RADIO_SIZE);
    if (height<0)
      height = (int)(cyf);
  } else {
    if (width<0)
      width = checkWidth + FB_MARGIN;
    if (height<0)
      height = checkHeight + FB_MARGIN;
  }

  MoveWindow(button, x, y, width, height, TRUE);

  OnSize(width, height);
}


void wxCheckBox::SetValue(Bool val)
{
  SendMessage((HWND)ms_handle, isFafa ? FAFA_SETCHECK : BM_SETCHECK, val, 0);
}

Bool wxCheckBox::GetValue(void)
{
  return (Bool)(0x003 & SendMessage((HWND)ms_handle,
				    isFafa ? FAFA_GETCHECK : BM_GETCHECK, 
				    0, 0));
}
