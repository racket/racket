/*
 * File:	wx_lbox.cc
 * Purpose:	List box implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	April 1995
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#define DEFAULT_ITEM_WIDTH  80
#define DEFAULT_ITEM_HEIGHT 80

BOOL wxListBox::MSWCommand(UINT param, WORD WXUNUSED(id))
{
  if (param == LBN_SELCHANGE) {
    wxCommandEvent *event;
    event = new wxCommandEvent(wxEVENT_TYPE_LISTBOX_COMMAND);
    ProcessCommand(event);
    return TRUE;
  } else if (param == LBN_DBLCLK) {
    wxCommandEvent *event;
    event = new wxCommandEvent(wxEVENT_TYPE_LISTBOX_DCLICK_COMMAND);
    ProcessCommand(event);
    return TRUE;
  }

  return FALSE;
}

// Listbox item

wxListBox::wxListBox(wxPanel *panel, wxFunction func,
		     char *Title, Bool Multiple,
		     int x, int y, int width, int height,
		     int N, char **Choices, long style, 
		     wxFont *_font, wxFont *_label_font, char *name):
  wxbListBox(panel, func, Title, Multiple, x, y, width, height, N, Choices,
             style, name)
{
  SetFont(_font);
  label_font = (_label_font ? label_font : font);
  Create(panel, func, Title, Multiple, x, y, width, height, N, Choices,
         style, name);
}

Bool wxListBox::Create(wxPanel *panel, wxFunction func,
                       char *Title, Bool Multiple,
                       int x, int y, int width, int height,
                       int N, char **Choices, long style, char *name)
{
  wxWnd *cparent;
  char *the_label;
  DWORD wstyle;
  HWND wx_list;
  int i;
  wchar_t *ws;
    
  panel->AddChild(this);
  multiple = Multiple & wxMULTIPLE_MASK;
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

  the_label = NULL;

  if (Title)
    the_label = copystring(Title);

  // If label exists, create a static control for it.
  if (Title) {
    int nid;
    nid = NewId(this);
    ws = wxWIDE_STRING(the_label);
    static_label = CreateWindowExW(0, LSTATIC_CLASS, ws,
				   STATIC_FLAGS | WS_CLIPSIBLINGS,
				   0, 0, 0, 0, cparent->handle, (HMENU)nid,
				   wxhInstance, NULL);

    wxSetWinFont(font, (HANDLE)static_label);
  }
  else
    static_label = NULL;


  // Windows sense of MULTIPLE & EXTENDED is backwards from ours.
  if (multiple == wxEXTENDED)
    wstyle = WS_VSCROLL | LBS_MULTIPLESEL | LBS_NOTIFY;
  else if (multiple == wxMULTIPLE)
    wstyle = WS_VSCROLL | LBS_EXTENDEDSEL | LBS_NOTIFY;
  else
    wstyle = WS_VSCROLL | LBS_NOTIFY;
  if ((Multiple&wxALWAYS_SB) || (style & wxALWAYS_SB))
    wstyle |= LBS_DISABLENOSCROLL ;
  if (style & wxHSCROLL)
    wstyle |= WS_HSCROLL;

  windows_id = NewId(this);

  wx_list = CreateWindowExW(WS_EX_CLIENTEDGE, L"wxLISTBOX", NULL,
			    wstyle | WS_CHILD | WS_CLIPSIBLINGS,
			    0, 0, 0, 0, cparent->handle, (HMENU)windows_id,
			    wxhInstance, NULL);

  user_data = new char*[N];
  for (i = 0; i < N; i++) {
    user_data[i] = NULL;
  }

  for (i = 0; i < N; i++) {
    ws = wxWIDE_STRING(Choices[i]);
    SendMessageW(wx_list, LB_ADDSTRING, 0, (LONG)ws);
  }
  if (!Multiple)
    SendMessage(wx_list, LB_SETCURSEL, 0, 0);

  no_items = N;

  ms_handle = (HANDLE)wx_list;

  // Subclass again for purposes of dialog editing mode
  SubclassControl(wx_list);

  wxSetWinFont(font, ms_handle);

  SetSize(x, y, width, height);

  if (static_label)
    BringWindowToTop(static_label);
  BringWindowToTop(wx_list);

  if (!(style & wxINVISIBLE)) {
    if (static_label)
      ShowWindow(static_label, SW_SHOW);
    ShowWindow(wx_list, SW_SHOW);
  }

  panel->AdvanceCursor(this);
  Callback(func);

  if (style & wxINVISIBLE)
    Show(FALSE);

  return TRUE;
}

wxListBox::~wxListBox(void)
{
  isBeingDeleted = TRUE;
  
  if (static_label)
    wxwmDestroyWindow(static_label);
  static_label = NULL;
}

void wxListBox::SetFirstItem(int N)
{
  SendMessage((HWND)ms_handle,LB_SETTOPINDEX,(WPARAM)N,(LPARAM)0) ;
}

void wxListBox::SetFirstItem(char *s)
{
  int N;
  N = FindString(s);

  if (N>=0)
    SetFirstItem(N);
}

int wxListBox::NumberOfVisibleItems(void)
{
  int h;
  int cw, ch;
  
  h = SendMessage((HWND)ms_handle,LB_GETITEMHEIGHT,(WPARAM)0,(LPARAM)0);

  GetClientSize(&cw, &ch);
  ch = ch / h;

  return max(ch, 1);
}

int wxListBox::GetFirstItem(void)
{
  return SendMessage((HWND)ms_handle,LB_GETTOPINDEX,(WPARAM)0,(LPARAM)0);
}

void wxListBox::Delete(int N)
{
  int i;

  SendMessage((HWND)ms_handle, LB_DELETESTRING, N, 0);
  no_items --;
  for (i = N; i < no_items; i++) {
    user_data[i] = user_data[i + 1];
  }
  SetHorizontalExtent(NULL);
}

void wxListBox::Append(char *Item)
{
  Append(Item, NULL);
}

void wxListBox::Append(char *Item, char *Client_data)
{
  wchar_t *ws;
  char **old = user_data;
  int i, index;

  user_data = new char*[no_items + 1];
  for (i = no_items; i--; ) {
    user_data[i] = old[i];
  }
  user_data[no_items] = Client_data;

  ws = wxWIDE_STRING(Item);
  index = (int)SendMessageW((HWND)ms_handle, LB_ADDSTRING, 0, (LONG)ws);
  no_items++;
  SetHorizontalExtent(Item);
}

void wxListBox::Set(int n, char *choices[])
{
  int i;
  wchar_t *ws;

  Clear();

  user_data = new char*[n];

  ShowWindow((HWND)ms_handle, SW_HIDE);
  for (i = 0; i < n; i++) {
    ws = wxWIDE_STRING(choices[i]);
    SendMessageW((HWND)ms_handle, LB_ADDSTRING, 0, (LONG)ws);
    user_data[i] = NULL;
  }
  no_items = n;
  SetHorizontalExtent(NULL);
  ShowWindow((HWND)ms_handle, SW_SHOW);
}

int wxListBox::FindString(char *s)
{
  int pos;
  wchar_t *ws;
  ws = wxWIDE_STRING(s);
  pos = (int)SendMessageW((HWND)ms_handle, LB_FINDSTRINGEXACT, -1, (LONG)ws);
  if (pos == LB_ERR)
    return -1;
  else
    return pos;
}

void wxListBox::Clear(void)
{
  SendMessage((HWND)ms_handle, LB_RESETCONTENT, 0, 0);

  no_items = 0;
  user_data = NULL;
  SendMessage((HWND)ms_handle, LB_SETHORIZONTALEXTENT, LOWORD(0), 0L);
}

void wxListBox::SetSelection(int N, Bool select, Bool one)
{
  if ((N < 0) || (N >= Number()))
    return;

  if (multiple != wxSINGLE) {
    if (one) {
      int nv;
      nv = Number();
      SendMessage((HWND)ms_handle, LB_SELITEMRANGE, 0, MAKELPARAM(0, nv));
    }
    SendMessage((HWND)ms_handle, LB_SETSEL, select, N);
  } else {
    if (!select)
      N = -1; /* -1 => deselect current */
    SendMessage((HWND)ms_handle, LB_SETCURSEL, N, 0);
  }
}

void wxListBox::SetOneSelection(int N)
{
  SetSelection(N, TRUE, TRUE);
}

Bool wxListBox::Selected(int N)
{
  return (Bool)SendMessage((HWND)ms_handle, LB_GETSEL, N, 0);
}

void wxListBox::Deselect(int N)
{
  SetSelection(N, 0);
}

char *wxListBox::GetClientData(int N)
{
  return user_data[N];
}

void wxListBox::SetClientData(int N, char *Client_data)
{
  user_data[N] = Client_data;
}

// Return number of selections and an array of selected integers
int wxListBox::GetSelections(int **list_selections)
{
  HWND listbox = (HWND)ms_handle;

  if (multiple == wxSINGLE) {
    int sel;
    sel = (int)SendMessage(listbox, LB_GETCURSEL, 0, 0);
    if (sel == LB_ERR)
      return 0;
    selections = new int[1];
    selections[0] = sel;
    *list_selections = selections;
    return 1;
  } else {
    int no_sel;
    no_sel = (int)SendMessage(listbox, LB_GETSELCOUNT, 0, 0);
    if (no_sel == 0)
      return 0;
    selections = new int[no_sel];
    SendMessage(listbox, LB_GETSELITEMS, no_sel, (LONG)selections);
    *list_selections = selections;
    return no_sel;
  }
}

// Get single selection, for single choice list items
int wxListBox::GetSelection(void)
{
  int c, *l;
  c = GetSelections(&l);
  if (!c)
    return -1;
  else
    return l[0];
}

// Find string for position
char *wxListBox::GetString(int N)
{
  int len;

  if (N < 0 || N >= no_items)
    return NULL;
  else
    len = (int)SendMessageW((HWND)ms_handle, LB_GETTEXT, N, (LONG)wxBuffer);

  ((wchar_t *)wxBuffer)[len] = 0;

  return wxNARROW_STRING((wchar_t *)wxBuffer);
}

void wxListBox::SetSize(int x, int y, int width, int height, int sizeFlags)
{
  int currentX, currentY;
  char buf[300];
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

  // Deal with default size (using -1 values)
  if (width <= 0)
    width = DEFAULT_ITEM_WIDTH;

  if (height <= 0)
    height = DEFAULT_ITEM_HEIGHT;

  if (static_label) {
    // Find size of label
    wxGetCharSize((HWND)ms_handle, &clx, &cly, label_font);
    GetWindowText(static_label, buf, 300);
    GetLabelExtent(wxStripMenuCodes(buf), &label_width, &label_height, label_font);

    // Given size is total label + edit size, find individual
    // control sizes on that basis.
    if (labelPosition == wxHORIZONTAL) {
      label_x = (double)x;
      label_y = (double)y;
      label_width += (double)clx;

      control_x = label_x + label_width + clx;
      control_y = (double)y;
      control_width = width - (control_x - label_x);
      control_height = (double)height;
    } else { // wxVERTICAL
      label_x = (double)x;
      label_y = (double)y;

      control_x = (double)x;
      control_y = label_y + label_height + 3; // Allow for 3D border
      control_width = (double)width;
      control_height = height - label_height - 3;
    }

    MoveWindow(static_label, (int)label_x, (int)label_y,
               (int)label_width, (int)label_height, TRUE);
  } else {
    control_x = (double)x;
    control_y = (double)y;
    control_width = (double)width;
    control_height = (double)height;
  }

  // Calculations may have made size too small
  if (control_height <= 0)
    control_height = DEFAULT_ITEM_HEIGHT;

  if (control_width <= 0)
    control_width = DEFAULT_ITEM_WIDTH;

  MoveWindow((HWND)ms_handle, (int)control_x, (int)control_y,
	     (int)control_width, (int)control_height, TRUE);

  OnSize(width, height);
}

void wxListBox::GetSize(int *width, int *height)
{
  RECT rect;

  rect.left = -1; rect.right = -1; rect.top = -1; rect.bottom = -1;

  wxFindMaxSize((HWND)ms_handle, &rect);

  if (static_label) {
    wxFindMaxSize(static_label, &rect);
  }

  *width = rect.right - rect.left;
  *height = rect.bottom - rect.top;
}

void wxListBox::GetPosition(int *x, int *y)
{
  RECT rect;
  wxWindow *parent;
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

char *wxListBox::GetLabel(void)
{
  if (static_label)
  {
    GetWindowText(static_label, wxBuffer, 300);
    return wxBuffer;
  }
  else return NULL;
}

void wxListBox::SetLabel(char *label)
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
    if (parent)
    {
      wxWnd *cparent = (wxWnd *)(parent->handle);
      ::ScreenToClient(cparent->handle, &point);
    }

    GetLabelExtent((LPSTR)label, &w, &h, label_font);
    MoveWindow(static_label, point.x, point.y, (int)(w + 10), (int)h,
               TRUE);
    SetWindowTextW(static_label, wxWIDE_STRING(label));
  }
}

// Windows-specific code to set the horizontal extent of
// the listbox, if necessary. If s is non-NULL, it's
// used to calculate the horizontal extent.
// Otherwise, all strings are used.
void wxListBox::SetHorizontalExtent(char *s)
{
  HWND hWnd;
  TEXTMETRIC lpTextMetric;

  // Only necessary if we want a horizontal scrollbar
  if (!(windowStyle & wxHSCROLL))
    return;

  hWnd = GetHWND();

  if (s)
  {
    int existingExtent;
    HDC dc;
    SIZE extentXY;
    int extentX;
    existingExtent = (int)SendMessage(hWnd, LB_GETHORIZONTALEXTENT, 0, 0L);
    dc = GetWindowDC(hWnd);
    GetTextMetrics(dc, &lpTextMetric);
    {
      wchar_t *ws;
      ws = wxWIDE_STRING(s);
      ::GetTextExtentPointW(dc, ws, wx_wstrlen(ws), &extentXY);
    }
    extentX = (int)(extentXY.cx + lpTextMetric.tmAveCharWidth);
    ReleaseDC(hWnd, dc);
    if (extentX > existingExtent)
      SendMessage(hWnd, LB_SETHORIZONTALEXTENT, LOWORD(extentX), 0L);
    return;
  }
  else
  {
    int largestExtent = 0;
    int i;
    HDC dc;
    dc = GetWindowDC(hWnd);
    GetTextMetrics(dc, &lpTextMetric);
    for (i = 0; i < no_items; i++) {
      int len;
      SIZE extentXY;
      int extentX;
      len = (int)SendMessageW(hWnd, LB_GETTEXT, i, (LONG)wxBuffer);
      ((wchar_t *)wxBuffer)[len] = 0;
      ::GetTextExtentPointW(dc, (LPWSTR)wxBuffer, len, &extentXY);
      extentX = (int)(extentXY.cx + lpTextMetric.tmAveCharWidth);
      if (extentX > largestExtent)
        largestExtent = extentX;
    }
    ReleaseDC(hWnd, dc);
    SendMessage(hWnd, LB_SETHORIZONTALEXTENT, LOWORD(largestExtent), 0L);
  }
}

void
wxListBox::InsertItems(int nItems, char **Items, int pos)
{
}

void wxListBox::SetString(int N, char *s)
{
  int sel;
  char *oldData;
  wchar_t *ws;

  if ((N < 0) || (N >= no_items))
    return;

  sel = Selected(N);
  
  oldData = (char *)wxListBox::GetClientData(N);
  
  ws = wxWIDE_STRING(s);
  SendMessageW((HWND)ms_handle, LB_INSERTSTRING, N, (LPARAM)ws);
  SendMessage((HWND)ms_handle, LB_DELETESTRING, N + 1, 0);

  if (oldData)
    wxListBox::SetClientData(N, oldData);

  if (sel)
    SetSelection(N, TRUE, FALSE);
}


Bool wxListBox::Show(Bool show)
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
  if (show)
    BringWindowToTop(wnd);

  if (static_label) {
    ShowWindow(static_label, cshow);
    if (show)
      BringWindowToTop(static_label);
  }

  return TRUE;
}

void wxListBox::ChangeToGray(Bool gray)
{
  wxWindow::ChangeToGray(gray);

  if (static_label)
    ::EnableWindow(static_label, !gray);
}

