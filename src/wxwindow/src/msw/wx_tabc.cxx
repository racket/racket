/*
 * File:	wx_tabc.cc
 * Purpose:	Tab choice implementation
 * Author:	Matthew
 * Created:	2002
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 2002, PLT
 */

#include "wx.h"

#include "wx_tabc.h"

#include <commctrl.h>

#define wx_TabCtrl_InsertItem(h, nv, tie) \
   SendMessageW(h, (UINT)TCM_INSERTITEMW, nv, (LPARAM)tie)
#define wx_TabCtrl_SetItem(h, nv, tie) \
   SendMessageW(h, (UINT)TCM_SETITEMW, nv, (LPARAM)tie)

BOOL wxTabChoice::MSWCommand(UINT param, WORD WXUNUSED(id))
{
  if (param == 64985 /* (UINT)TCN_SELCHANGE  ? */) {
    wxCommandEvent *event;
    event = new wxCommandEvent(wxEVENT_TYPE_TAB_CHOICE_COMMAND);
    ProcessCommand(event);
    return TRUE;
  } else
    return FALSE;
}


wxTabChoice::wxTabChoice(wxPanel *panel, wxFunction func, char *label,
			 int n, char **choices, int style, wxFont *_font)
  : wxItem(panel)
{
  int x = 0, y = 0, i;
  wxWnd *cparent = NULL;
  TCITEMW tie;
  INITCOMMONCONTROLSEX icex;
  HWND hwndTab;
  int width, height, nid;
  RECT prc;

  __type = wxTYPE_TAB_CHOICE;

  SetFont(_font);

  icex.dwSize = sizeof(INITCOMMONCONTROLSEX);
  icex.dwICC  = ICC_TAB_CLASSES;
  InitCommonControlsEx(&icex);

  panel->AddChild(this);
  wxWinType = wxTYPE_HWND;
  windowStyle = (style & wxBORDER);
  cparent = (wxWnd *)panel->handle;

  panel->GetValidPosition(&x, &y);

  windows_id = NewId(this);
  
  {
    int cx, cy;
    double current_width, cyf, total_width = 0;

    wxGetCharSize(cparent->handle, &cx, &cy, font);
    
    for (i = 0; i < n; i++) {
      GetLabelExtent(wxStripMenuCodes(choices[i]), &current_width, &cyf);
      if (current_width < 40)
	current_width = 40;
      total_width += current_width + cy;
    }

    width = (int)total_width;
    height = 2 * cy;
  }

  hwndTab = CreateWindowW(L"wxTABCONTROL", L"", 
			  WS_CHILD | WS_CLIPSIBLINGS,
			  0, 0, width ? width : 40, height,
			  cparent->handle, (HMENU)windows_id, wxhInstance, NULL);

  nid = NewId(this);

  if (1) {
    /* For comctl32 version6, makes the panel background gray: */
    bgStatic = wxwmCreateWindowEx(0, STATIC_CLASS, "",
				  STATIC_FLAGS | WS_CHILD | WS_CLIPSIBLINGS | WS_VISIBLE,
				  0, 0, 200, 200, hwndTab, (HMENU)nid,
				  wxhInstance, NULL);
  }

  tie.mask = TCIF_TEXT;

  TabCtrl_SetUnicodeFormat(hwndTab, TRUE);
 
  if (n) {
    for (i = 0; i < n; i++) { 
      tie.pszText = wxWIDE_STRING(choices[i]);
      wx_TabCtrl_InsertItem(hwndTab, i, &tie);
    } 
  } else {
    /* for height-meausing purposes, add one: */
    tie.pszText = L"Dummy";
    wx_TabCtrl_InsertItem(hwndTab, 0, &tie);
  }
    
  SubclassControl(hwndTab);

  ms_handle = (HANDLE)hwndTab;

  wxSetWinFont(font, ms_handle);

  prc.left = prc.top = prc.right = prc.bottom = 0;
  TabCtrl_AdjustRect(hwndTab, TRUE, &prc);
    
  if (!n)
    Delete(0);

  SetSize(x, y, width, prc.bottom - prc.top);

  if (style & wxINVISIBLE)
    Show(FALSE);
  else
    ShowWindow(hwndTab, SW_SHOW);

  panel->AdvanceCursor(this);
  Callback(func);
}

wxTabChoice::~wxTabChoice()
{
  if (bgStatic)
    wxwmDestroyWindow(bgStatic);
}

int wxTabChoice::GetSelection(void) {
  return TabCtrl_GetCurSel((HWND)ms_handle);
}

int wxTabChoice::Number(void) { 
  return TabCtrl_GetItemCount((HWND)ms_handle);
}

void wxTabChoice::SetSelection(int n) { 
  if ((n >= 0) && (n < Number()))
    TabCtrl_SetCurSel((HWND)ms_handle, n);
}

void wxTabChoice::Enable(Bool enable) { 
  wxItem::Enable(enable);
}

void wxTabChoice::SetSize(int x, int y, int width, int height, int WXUNUSED(sizeFlags))
{
  int currentX, currentY;

  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  if (width < 0)
    GetSize(&width, &height);

  if (!orig_height)
    orig_height = height;

  MoveWindow((HWND)ms_handle, x, y, width, height, TRUE);
  if (bgStatic) {
    int dx = 2, dy = orig_height-4, dw = 5, dh = orig_height;
    if (!(windowStyle & wxBORDER)) {
      dx -= 3;
      dw -= 6;
      dh -= 4;
    }
  
    MoveWindow((HWND)bgStatic, dx, dy, width-dw, height-dh, TRUE);
  }

  OnSize(width, height);
}

void wxTabChoice::Append(char *s)
{
  TCITEMW tie;
  int shownhide = 0, nv;

  /* The control misupdates when going from 0 to non-zero
     tabs. Hide before making the transition, then show
     after. */
  if (!Number() && IsShown())
    shownhide = 1;

  if (shownhide)
    ShowWindow((HWND)ms_handle, SW_HIDE);

  tie.mask = TCIF_TEXT;
  tie.pszText = wxWIDE_STRING(s);
  nv = Number();
  wx_TabCtrl_InsertItem((HWND)ms_handle, nv, &tie);

  if (shownhide)
    ShowWindow((HWND)ms_handle, SW_SHOW);
}

void wxTabChoice::Delete(int i)
{
  if ((i >= 0) && (i < Number()))
    TabCtrl_DeleteItem((HWND)ms_handle, i);
}

void wxTabChoice::SetLabel(int i, char *s)
{
  if ((i >= 0) && (i < Number())) {
    TCITEMW ti;
    ti.mask = TCIF_TEXT;
    ti.pszText = wxWIDE_STRING(s);
    wx_TabCtrl_SetItem((HWND)ms_handle, i, &ti);
  }
}

Bool wxTabChoice::Show(Bool show) 
{
  wxWindow::Show(show);
  return TRUE;
}

void wxTabChoice::Set(int N, char **Choices)
{
  int i, sel;

  sel = GetSelection();
  for (i = Number(); i--; ) {
    TabCtrl_DeleteItem((HWND)ms_handle, i);
  }
  for (i = 0; i < N; i++) {
    Append(Choices[i]);
  }
  if (N) {
    if (sel >= N)
      sel = N-1;
    SetSelection(sel);
  }
}

int wxTabChoice::ButtonFocus(int n)
{
  if (n < 0)
    return TabCtrl_GetCurFocus((HWND)ms_handle);
  else {
    SetFocus();
    TabCtrl_SetCurFocus((HWND)ms_handle, n);
    return n;
  }
}
