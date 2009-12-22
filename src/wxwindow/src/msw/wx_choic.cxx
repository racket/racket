/*
 * File:	wx_choic.cc
 * Purpose:	Choice item implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	April 1995
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

extern Bool wxIsPrimEventspace();

int wx_choice_dropped;

BOOL wxChoice::MSWCommand(UINT param, WORD id)
{
  if (param == CBN_DROPDOWN)
    wx_choice_dropped = TRUE;
  if (param == CBN_CLOSEUP)
    wx_choice_dropped = FALSE;

  if (param == CBN_SELENDOK) {
    /* Callback possibly via popup window, which does not
       know its eventspace. If so, re-post the event to get
       eventspaces right. */
    if (wxIsPrimEventspace()) {
      wxWindow *par;
      LPARAM lp;
      par = GetParent();
      lp = (LPARAM)GetHWND();
      PostMessage(par->GetHWND(),
	          WM_COMMAND,
		  (WPARAM)MAKELONG(id, param),
		  lp);
    } else {
      wxCommandEvent *event;
      event = new wxCommandEvent(wxEVENT_TYPE_CHOICE_COMMAND);
      ProcessCommand(event);
    }
    return TRUE;
  }
  return FALSE;
}

wxChoice::wxChoice(wxPanel *panel, wxFunction func, char *Title,
                   int x, int y, int width, int height, int N, char **Choices,
                   long style, wxFont *_font, char *name):
  wxbChoice(panel, func, Title, x, y, width, height, N, Choices, style, name)
{
  SetFont(_font);
  Create(panel, func, Title, x, y, width, height, N, Choices, style, name);
}

Bool wxChoice::Create(wxPanel *panel, wxFunction func, char *Title,
                   int x, int y, int width, int height, int N, char **Choices,
                   long style, char *name)
{
  wxWnd *cparent = NULL;
  char *the_label = NULL;
  HWND wx_combo;

  panel->AddChild(this);

  no_strings = N;

  wxWinType = wxTYPE_HWND;
  windowStyle = style;

  if (panel)
    cparent = (wxWnd *)(panel->handle);

  if (style & wxVERTICAL_LABEL)
    labelPosition = wxVERTICAL;
  else if (style & wxHORIZONTAL_LABEL)
    labelPosition = wxHORIZONTAL;
  else
    labelPosition = panel->label_position;
  panel->GetValidPosition(&x, &y);

  if (Title) {
    the_label = new char[strlen(Title)+1];
    strcpy(the_label,Title);
    the_label[strlen(Title)] = '\0';
  }

  if (Title) {
    HMENU nid;
    wchar_t *ws;

    nid = (HMENU)NewId(this);
    ws = wxWIDE_STRING(the_label);
    static_label = CreateWindowExW(0, LSTATIC_CLASS, ws,
				   STATIC_FLAGS | WS_CLIPSIBLINGS
				   | ((style & wxINVISIBLE) ? 0 : WS_VISIBLE),
				   0, 0, 0, 0, cparent->handle, nid,
				   wxhInstance, NULL);

    wxSetWinFont(font, (HANDLE)static_label);
  } else
    static_label = NULL;
  
  windows_id = NewId(this);

  wx_combo = wxwmCreateWindowEx(0, "wxCOMBOBOX", NULL,
				WS_CHILD | CBS_DROPDOWNLIST | WS_HSCROLL | WS_VSCROLL
				| WS_BORDER | WS_TABSTOP | WS_CLIPSIBLINGS
				| ((style & wxINVISIBLE) ? 0 : WS_VISIBLE),
				0, 0, 0, 0, cparent->handle, (HMENU)windows_id,
				wxhInstance, NULL);
  ms_handle = (HANDLE)wx_combo;

  SubclassControl(wx_combo);

  wxSetWinFont(font, ms_handle);

  {
    int i;
    wchar_t *ws;
    for (i = 0; i < N; i++) {
      ws = wxWIDE_STRING(Choices[i]);
      SendMessageW(wx_combo, CB_INSERTSTRING, i, (LONG)ws);
    }
    SendMessage(wx_combo, CB_SETCURSEL, i, 0);
  }

  SetSize(x, y, width, height);
  panel->AdvanceCursor(this);
  Callback(func);

  SetSelection(0);

  if (static_label)
    BringWindowToTop(static_label);
  BringWindowToTop(wx_combo);

  if (style & wxINVISIBLE)
    Show(FALSE);

  return TRUE;
}

wxChoice::~wxChoice(void)
{
  isBeingDeleted = TRUE;
  
  if (static_label)
    wxwmDestroyWindow(static_label);
  static_label = NULL;
}

void wxChoice::Append(char *Item)
{
  wchar_t *ws;
  ws = wxWIDE_STRING(Item);
  SendMessageW((HWND)ms_handle, CB_ADDSTRING, 0, (LONG)ws);
  no_strings++;
  if (no_strings == 1)
    SetSelection(0);
}

void wxChoice::Clear(void)
{
  SendMessage((HWND)ms_handle, CB_RESETCONTENT, 0, 0);

  no_strings = 0;
}


int wxChoice::GetSelection(void)
{
  if (!no_strings)
    return 0;
  return (int)SendMessage((HWND)ms_handle, CB_GETCURSEL, 0, 0);
}

void wxChoice::SetSelection(int n)
{
  SendMessage((HWND)ms_handle, CB_SETCURSEL, n, 0);
}

int wxChoice::FindString(char *s)
{
  int pos;
  pos = (int)SendMessage((HWND)ms_handle, CB_FINDSTRINGEXACT, -1, (LPARAM)(LPSTR)s);
  if (pos == LB_ERR)
    return -1;
  else
    return pos;
}

char *wxChoice::GetString(int n)
{
  int len;

  if (!no_strings) return NULL;
  if (n < 0 || n > Number())
    return NULL;

  len = (int)SendMessageW((HWND)ms_handle, CB_GETLBTEXT, n, (long)wxBuffer);
  ((wchar_t *)wxBuffer)[len] = 0;

  return wxNARROW_STRING((wchar_t *)wxBuffer);
}

void wxChoice::SetSize(int x, int y, int width, int height, int sizeFlags)
{
  int currentX, currentY;
  int cx; // button font dimensions
  int cy;
  int clx; // label font dimensions
  int cly;
  double label_width, label_height, label_x, label_y;
  double control_width, control_height, control_x, control_y;

  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  // If we're prepared to use the existing size, then...
  if (width == -1 && height == -1 && ((sizeFlags & wxSIZE_AUTO) != wxSIZE_AUTO))
    GetSize(&width, &height);

  wxGetCharSize((HWND)ms_handle, &cx, &cy, font);

  // Ignore height parameter because height doesn't
  // mean 'initially displayed' height, it refers to the
  // drop-down menu as well. The wxWindows interpretation
  // is different; also, getting the size returns the
  // _displayed_ size (NOT the drop down menu size)
  // so setting-getting-setting size would not work.
  height = -1;

  // Deal with default size (using -1 values)
  if (width <= 0)
  {
    // Find the longest string
    if (no_strings == 0)
      control_width = 100;
    else
    {
      double len, ht;
      double longest = 0.0;
      int i;
      for (i = 0; i < no_strings; i++)
      {
        char *s;
	s = GetString(i);
        GetLabelExtent(s, &len, &ht);
        if ( len > longest) longest = len;
      }

      control_width = (double)(int)(longest + cx*5);
    }
  }

  // Choice drop-down list depends on number of items (limited to 4 < x < 10)
  if (height <= 0)
    height = (int)(EDIT_CONTROL_FACTOR*cy*(min(10, max(4, no_strings)) + 1));

  if (static_label)
  {
    // Find size of label
    wxGetCharSize((HWND)ms_handle, &clx, &cly,font);
    GetWindowTextW(static_label, (wchar_t *)wxBuffer, 300);
    GetLabelExtent(wxStripMenuCodes(wxNARROW_STRING((wchar_t*)wxBuffer)), 
		   &label_width, &label_height);

    // Given size is total label + edit size, so find individual
    // control sizes on that basis.
    if (labelPosition == wxHORIZONTAL)
    {
      if (height<=0)
        height = (int)((max(cy,cly))*EDIT_CONTROL_FACTOR) ;

      label_x = (double)x;
      label_y = (double)y + 4;
      label_width += (double)clx/2;

      control_x = label_x + label_width;
      control_y = (double)y;
      if (width >= 0)
        control_width = width - (control_x - label_x);
      control_height = (double)height;
    }
    else // wxVERTICAL
    {
      label_x = (double)x;
      label_y = (double)y;

      control_x = (double)x;
      control_y = label_y + label_height + 3; // Allow for 3D border

      if (width >= 0)
        control_width = (double)width;

      if (height<=0)
        control_height = (double)(int)(cy*EDIT_CONTROL_FACTOR) ;
      else
        control_height = height - label_height - 3;
    }
    MoveWindow(static_label, (int)label_x, (int)label_y,
               (int)label_width, (int)label_height, TRUE);
  }
  else
  {
    control_x = (double)x;
    control_y = (double)y;
    if (width >= 0)
      control_width = (double)width;
    control_height = (double)height;
  }

  // Calculations may have made text size too small
  if (control_height <= 0)
    control_height = (double)(int)(cy*EDIT_CONTROL_FACTOR) ;

  if (control_width <= 0)
    control_width = 100;

  MoveWindow((HWND)ms_handle, (int)control_x, (int)control_y,
                              (int)control_width, (int)control_height, TRUE);

  OnSize(width, height);
}

void wxChoice::GetSize(int *width, int *height)
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

void wxChoice::GetPosition(int *x, int *y)
{
  HWND wnd = (HWND)ms_handle;
  wxWindow *parent;
  RECT rect;
  POINT point;

  rect.left = -1; rect.right = -1; rect.top = -1; rect.bottom = -1;
  parent = GetParent();

  wxFindMaxSize(wnd, &rect);
  if (static_label)
    wxFindMaxSize(static_label, &rect);

  // Since we now have the absolute screen coords,
  // if there's a parent we must subtract its top left corner
  point.x = rect.left;
  point.y = rect.top;
  if (parent) {
    wxWnd *cparent = (wxWnd *)(parent->handle);
    ::ScreenToClient(cparent->handle, &point);
  }

  *x = point.x;
  *y = point.y;
}

char *wxChoice::GetLabel(void)
{
  if (static_label) {
    GetWindowText(static_label, wxBuffer, 300);
    return wxBuffer;
  }
  else return NULL;
}

void wxChoice::SetLabel(char *label)
{
  if (static_label) {
    double w, h;
    RECT rect;
    wxWindow *parent;
    POINT point;

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

Bool wxChoice::Show(Bool show)
{
  HWND wnd = (HWND)ms_handle;
  int cshow;
  wxChildList *childs;

  SetShown(show);

  childs = window_parent->GetChildren();
  childs->Show(this, show);

  if (show)
    cshow = SW_SHOW;
  else
    cshow = SW_HIDE;
 
  ShowWindow(wnd, cshow);
 
  if (static_label)
    ShowWindow(static_label, cshow);

  return TRUE;
}

void wxChoice::ChangeToGray(Bool gray)
{
  wxWindow::ChangeToGray(gray);
  if (static_label)
    ::EnableWindow(static_label, !gray);
}

/*****************************************************************************/

static wxFont *combo_box_font;
int reg;

void wxSetComboBoxFont(wxFont *f)
{
  if (!reg) {
    wxREGGLOB(combo_box_font);
  }
  combo_box_font = f;
}

wxCombo::wxCombo(wxWindow *for_canvas,
		 wxPanel *panel, wxFunction func, char *Title,
		 int x, int y, int w, int h,
		 int N, char **Choices,
		 long style, char *name) :
  wxChoice(panel, func, Title, x, y, w, h, N, Choices, style, combo_box_font, name)
{
  WXGC_IGNORE(this, forCanvas);
  forCanvas = for_canvas;
}

wxCombo::~wxCombo(void)
{
  forCanvas = NULL;
}

Bool wxCombo::PreOnEvent(wxWindow *win, wxMouseEvent *e)
{
  forCanvas->PreOnEvent(win, e);
  return TRUE;
}

Bool wxCombo::PreOnChar(wxWindow *win, wxKeyEvent *e)
{
  return forCanvas->PreOnChar(win, e);
}

wxWindow *wxCombo::PreWindow()
{
  return forCanvas;
}
