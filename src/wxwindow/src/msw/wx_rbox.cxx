/*
 * File:	wx_rbox.cc
 * Purpose:	Radio box item implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	April 1995
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#define HAS_LABEL 1

#define GROUP_CLASS      "wxBUTTON"
#define GROUP_FLAGS      (BS_GROUPBOX|WS_CHILD|WS_VISIBLE)

extern char wxPanelClassName[];

BOOL wxRadioBox::MSWCommand(UINT param, WORD id)
{
  if (param == BN_CLICKED)  {
    return TRUE;
  } else 
    return FALSE;
}

extern LONG wxDoItemPres(wxItem *item, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam,
			long *r, int tramp);

class wxRBInfo {
public:
  FARPROC old;
  wxItem *item;
  int which;
};

extern int wx_trampolining;


// Sub-classed generic control proc
LONG APIENTRY _EXPORT
wxRadioItemProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  LRESULT res;
  int tramp = wx_trampolining;
  wxRBInfo *i;
  long r;

  wx_trampolining = 0;

  if (message == WM_GETDLGCODE)
    return DLGC_WANTMESSAGE;

  /* See mredmsw.cxx: */
  if (!tramp)
    if (wxEventTrampoline(hWnd, message, wParam, lParam, &res, wxRadioItemProc))
      return res;

  i = (wxRBInfo *)wxFindControlFromHandle(hWnd);
  
  if (!i) return FALSE;

  if (!wxDoItemPres(i->item, hWnd, message, wParam, lParam, &r, tramp))
    return r;

  if (tramp && (message == WM_LBUTTONDOWN)) {
    /* we've already trampolined, so no atomic lock needed */
    wxRadioBox *rb;
    rb = (wxRadioBox *)i->item;
    if (rb->buttonEnabled[i->which]) {
      wxCommandEvent *event;
      rb->SetSelection(i->which);
      rb->ButtonFocus(i->which);
      event = new wxCommandEvent(wxEVENT_TYPE_RADIOBOX_COMMAND);
      i->item->ProcessCommand(event);
    }
    return TRUE;
  } else
    return CallWindowProcW((WNDPROC)i->old, hWnd, message, wParam, lParam);
}

static FARPROC wxGenericRIProc;

static void *SubclassRadioButton(HWND hWnd, wxItem *item, int which)
{
  wxRBInfo *i;
  FARPROC io;

  i = new wxRBInfo;
  
  i->item = item;
  i->which = which;

  // Subclass again for purposes of dialog editing mode
  wxAddControlHandle(hWnd, (wxItem *)i);
  io = (FARPROC)GetWindowLong(hWnd, GWL_WNDPROC);
  i->old = io;
  if (!wxGenericRIProc) {
    wxGenericRIProc = MakeProcInstance((FARPROC) wxRadioItemProc, wxhInstance);
  }
  SetWindowLongW(hWnd, GWL_WNDPROC, (LONG) wxGenericRIProc);

  return i;
}

void UnsubclassRadioButton(HWND hWnd)
{
  wxRBInfo *i;
  i = (wxRBInfo *)wxFindControlFromHandle(hWnd);
  
  if (i) {
    wxRemoveControlHandle(hWnd);
    SetWindowLongW(hWnd, GWL_WNDPROC, (LONG)i->old);
  }
}

// Radio box item

wxRadioBox::wxRadioBox(wxPanel *panel, wxFunction func,
                       char *Title,
                       int x, int y, int width, int height,
                       int N, char **Choices,
                       int MajorDim,long style, 
		       wxFont *_font, char *name):
  wxbRadioBox(panel, func, Title, x, y, width, height, N, Choices,
              MajorDim, style, name)
{
  SetFont(_font);
  Create(panel, func, Title, x, y, width, height, N, Choices, NULL, MajorDim, style, name);
}

/*
 * Causes problems for the Turbo C++ for Windows linker
 *
 */
wxRadioBox::wxRadioBox(wxPanel *panel, wxFunction func,
                       char *Title,
                       int x, int y, int width, int height,
                       int N, wxBitmap **Choices,
                       int MajorDim,long style, 
		       wxFont *_font, char *name):
  wxbRadioBox(panel, func, Title, x, y, width, height, N, Choices,
              MajorDim, style, name)
{
  SetFont(_font);
  Create(panel, func, Title, x, y, width, height, N, NULL, Choices, MajorDim, style, name);
}

Bool wxRadioBox::Create(wxPanel *panel, wxFunction func,
                       char *Title,
                       int x, int y, int width, int height,
                       int N, char **Choices, wxBitmap **bmChoices,
                       int MajorDim, long _style, char *name)
{
  wxWnd *cparent;
  char *the_label;
  HWND the_handle;
  int i;

  panel->AddChild(this);
  if (MajorDim==0)
    MajorDim = N ;
  majorDim = MajorDim;
  selected = -1;
  ms_handle = 0;
  wxWinType = wxTYPE_HWND;
  isFafa = !!bmChoices;

  cparent = (wxWnd *)(panel->handle);

  if (_style & wxVERTICAL_LABEL)
    labelPosition = wxVERTICAL;
  else if (_style & wxHORIZONTAL_LABEL)
    labelPosition = wxHORIZONTAL;
  else
    labelPosition = panel->label_position;

  panel->GetValidPosition(&x, &y);

  the_label = copystring(Title ? Title : "");

  the_handle = cparent->handle;

  {
    int nid;
    wchar_t *ws;
    nid = NewId(this);
    ws = wxWIDE_STRING(the_label);
    ms_handle = CreateWindowExW(0, LSTATIC_CLASS, ws,
				STATIC_FLAGS | WS_CLIPSIBLINGS
				| ((_style & wxINVISIBLE) ? 0 : WS_VISIBLE),
				0, 0, 0, 0, cparent->handle, (HMENU)nid,
				wxhInstance, NULL);
  }

  wxSetWinFont(font, ms_handle);

  SubclassControl((HWND)ms_handle);
  
  the_handle = cparent->handle;

  radioButtons = new WXGC_ATOMIC HWND[N];
  radioWidth = new WXGC_ATOMIC int[N];
  radioHeight = new WXGC_ATOMIC int[N];
  if (bmChoices) {
    bm_labels = new wxBitmap*[N];
  } else {
    bm_labels = NULL;
  }
  buttonEnabled = new Bool[N];

  numSubControls = N;
  subControls = new WXGC_ATOMIC int[N];
  subControlPtrs = new void*[N];

  for (i = 0; i < N; i++) {
    long newId;
    long groupStyle = 0;
    void *scp;

    if (i == 0 && _style==0)
      groupStyle = WS_GROUP;

    newId = NewId(this);
    buttonEnabled[i] = TRUE;
      
    if (bmChoices) {
      char tmp[32];
      HWND rbhand;
      wxBitmap *rbm;
      int bw, bh;
      HBITMAP lbm;

      rbm = bmChoices[i];

      bw = rbm->GetWidth();
      bh = rbm->GetHeight();

      radioWidth[i]  = bw  + FB_MARGIN;
      radioHeight[i] = bh + FB_MARGIN;
      sprintf(tmp, "Toggle%d", i);
      
      bm_labels[i] = rbm;
      rbm->selectedIntoDC++;
      rbhand = wxwmCreateWindowEx(0, FafaChck, tmp,
				  groupStyle | BITRADIO_FLAGS
				  | ((_style & wxINVISIBLE) ? 0 : WS_VISIBLE),
				  0, 0, 0, 0,
				  the_handle, (HMENU)newId, wxhInstance, NULL);
      radioButtons[i] = rbhand;

      lbm = rbm->GetLabelBitmap(0);
      SetBitmapDimensionEx(lbm,
			   rbm->GetWidth(),
			   rbm->GetHeight(),
			   NULL);
      SendMessage((HWND)radioButtons[i], WM_CHANGEBITMAP,
		  (WPARAM)0xFFFF,
		  (LPARAM)lbm);
    } else {
      HWND rbhand;

      radioWidth[i] = radioHeight[i] = -1 ;
      rbhand = CreateWindowExW(0, L"wxBUTTON", wxWIDE_STRING(Choices[i]),
			       groupStyle | RADIO_FLAGS
			       | ((_style & wxINVISIBLE) ? 0 : WS_VISIBLE),
			       0, 0, 0, 0,
			       the_handle, (HMENU)newId, wxhInstance, NULL);
      radioButtons[i] = rbhand;
    }

    scp = SubclassRadioButton(radioButtons[i], this, i);
    subControlPtrs[i] = scp;
    wxSetWinFont(font, radioButtons[i]);
    subControls[i] = newId;
  }

  // Create a dummy radio control to end the group.
  {
    int nid;
    nid = NewId(this);
    wxwmCreateWindowEx(0, RADIO_CLASS, "", 
		       WS_GROUP | RADIO_FLAGS
		       | ((_style & wxINVISIBLE) ? 0 : WS_VISIBLE),
		       0, 0, 0, 0, 
		       the_handle, (HMENU)nid, wxhInstance, NULL);
  }

  no_items = N;
  SetSelection(0);
  
  style = Title ? HAS_LABEL : 0;

  SetSize(x, y, width, height);
  panel->AdvanceCursor(this);
  Callback(func);

  if (style & HAS_LABEL)
    BringWindowToTop((HWND)ms_handle);

  if (_style & wxINVISIBLE)
    Show(FALSE);

  return TRUE;
}

wxRadioBox::~wxRadioBox(void)
{
  isBeingDeleted = TRUE;
  
  if (radioButtons) {
    int i;
    for (i = 0; i < no_items; i++) {
      UnsubclassRadioButton(radioButtons[i]);
      wxwmDestroyWindow(radioButtons[i]);
    }
  }

  if (bm_labels) {
    int i;

    for (i = 0; i < no_items; i++) {
      if (bm_labels[i]) {
	wxBitmap *rbm = bm_labels[i];
	--rbm->selectedIntoDC;
	rbm->ReleaseLabel();
      }
    }

    bm_labels = NULL;
  }
  if (ms_handle)
    wxwmDestroyWindow((HWND)ms_handle) ;
  ms_handle = NULL ;

}

char *wxRadioBox::GetLabel(int item)
{
  char buf[300];
  GetWindowText((HWND)radioButtons[item], buf, 300);
  return copystring(buf);
}

void wxRadioBox::SetLabel(int item, char *label)
{
  if (bm_labels && bm_labels[item])
    return;
  
  // This message will switch from FB_BITMAP style to FB_TEXT, if needed.
  SendMessage((HWND)radioButtons[item],WM_CHANGEBITMAP,
	      (WPARAM)0,
	      (LPARAM)NULL);

  radioWidth[item] = radioHeight[item] = -1 ;
  SetWindowTextW((HWND)radioButtons[item], wxWIDE_STRING(label));
}

void wxRadioBox::SetLabel(int item,wxBitmap *bitmap)
{
  wxBitmap *rbm;
  HBITMAP lbm;

  if (!bm_labels || !bm_labels[item]
      || !bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return;

  rbm = bm_labels[item];

  --rbm->selectedIntoDC;
  rbm->ReleaseLabel();
  bm_labels[item] = bitmap;
  bitmap->selectedIntoDC++;

  lbm = bitmap->GetLabelBitmap(0);
  SetBitmapDimensionEx(lbm,
		       bitmap->GetWidth(),
		       bitmap->GetHeight(),
		       NULL);
  SendMessage((HWND)radioButtons[item],WM_CHANGEBITMAP,
	      (WPARAM)0xFFFF/*((bitmap->GetHeight()<<8)+bitmap->GetWidth())*/,
	      (LPARAM)lbm);
  {
    int bw, bh;
    bw = bitmap->GetWidth();
    bh = bitmap->GetHeight();
    radioWidth[item] = bw + FB_MARGIN ;
    radioHeight[item] = bh + FB_MARGIN ;
  }
}

int wxRadioBox::FindString(char *s)
{
  int i;
  for (i = 0; i < no_items; i++) {
    GetWindowText(radioButtons[i], wxBuffer, 1000);
    if (strcmp(wxBuffer, s) == 0)
      return i;
  }
  return -1;
}


void wxRadioBox::SetButton(int which, int value)
{
  int reset = 0;

  if (IsGray() || !buttonEnabled[which]) {
    ::EnableWindow(radioButtons[which], TRUE);
    reset = 1;
  }

  SendMessage(radioButtons[which], 
	      isFafa ? FAFA_SETCHECK : BM_SETCHECK, 
	      value, 0L);

  if (reset)
    ::EnableWindow(radioButtons[which], FALSE);
}


void wxRadioBox::SetSelection(int N)
{
  if ((N < -1) || (N >= no_items))
    return;

  if (N == selected)
    return;

  if (selected >= 0 && selected < no_items)
    SetButton(selected, 0);
    
  if (N != -1)
    SetButton(N, 1);

  selected = N;
}

// Get single selection, for single choice list items
int wxRadioBox::GetSelection(void)
{
  return selected;
}

// Find string for position
char *wxRadioBox::GetString(int N)
{
  GetWindowText(radioButtons[N], wxBuffer, 1000);
  return wxBuffer;
}

void wxRadioBox::SetSize(int x, int y, int width, int height, int WXUNUSED(sizeFlags))
{
  int currentX, currentY;
  int y_offset = y;
  int x_offset = x;
  double current_width;
  double cyf;
  HWND wnd;
  int cx1, cy1, startX, startY;
  int maxWidth =  -1;
  int maxHeight = -1;
  int hlabel = 0;
  int i;
  double label_width = 0;
  double label_height = 0;

  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  wnd = (HWND)ms_handle;

  // We find the height of the largest item, and then
  //  put maxHeight/2 space between each radio button.
  //  The label can be a different font from the rest.

  wxGetCharSize(wnd, &cx1, &cy1, font);

  if ((style & HAS_LABEL)) {
    int char_width, ignored;
    GetWindowTextW((HWND)ms_handle, (wchar_t *)wxBuffer, 300);
    GetLabelExtent(wxStripMenuCodes(wxNARROW_STRING((wchar_t*)wxBuffer)), 
		   &label_width, &label_height);
    wxGetCharSize(wnd, &char_width, &ignored, font);
  } else {
    label_height = 0;
    label_width = 0;
  }

  for (i = 0 ; i < no_items; i++) {
    int eachWidth;
    int eachHeight ;
    if (radioWidth[i] < 0) {
      // It's a labelled toggle
      GetWindowTextW(radioButtons[i], (wchar_t *)wxBuffer, 300);
      GetLabelExtent(wxStripMenuCodes(wxNARROW_STRING((wchar_t*)wxBuffer)), 
		     &current_width, &cyf);
      eachWidth = (int)(current_width + RADIO_SIZE);
      eachHeight = (int)cyf;
    } else {
      eachWidth = radioWidth[i];
      eachHeight = radioHeight[i];
    }
    if (maxWidth < eachWidth) maxWidth = eachWidth;
    if (maxHeight < eachHeight) maxHeight = eachHeight;
  }

  if (ms_handle) {
    int totWidth;
    int totHeight;
    int nbHor, nbVer;

    if (windowStyle & wxHORIZONTAL) {
      nbHor = majorDim;
      nbVer = (no_items + majorDim - 1) / majorDim;
    } else {
      nbVer = majorDim;
      nbHor = (no_items + majorDim - 1) / majorDim;
    }

    if (!(style & HAS_LABEL) || (labelPosition == wxHORIZONTAL)) {
      // hieght =       items           + between & ends space
      totHeight = (int)(nbVer*maxHeight + (nbVer-1)*maxHeight/2.0);
      totWidth  = label_width + (nbHor * (maxWidth + cx1)+ 2 * cx1) + label_height;
      if (totHeight < label_height)
	totHeight = label_height;
    } else {
      // hieght =       label         + items           + between & ends space
      totHeight = (int)(label_height + nbVer*maxHeight + nbVer*maxHeight/2.0);
      totWidth  = nbHor * (maxWidth + cx1)+ 2 * cx1;
      if (totWidth < label_width)
	totWidth = label_width;
    }

    {
      int dy;
      
      if ((style & HAS_LABEL) && (labelPosition == wxHORIZONTAL))
	dy = (totHeight - label_height) / 2;
      else
	dy = 0;

      MoveWindow((HWND)ms_handle, x_offset, y_offset+dy, label_width, label_height, TRUE);
    }
    ::InvalidateRect((HWND)ms_handle, NULL, TRUE);

    if (!(style & HAS_LABEL) || (labelPosition == wxHORIZONTAL)) {
      x_offset += label_width + (0.5 * label_height);
    } else {
      x_offset += cx1;
      y_offset += label_height + maxHeight/2.0;
    }

  }

  startX = x_offset;
  startY = y_offset;

  for ( i = 0 ; i < no_items; i++) {
    int eachWidth;
    int eachHeight;

    // Bidimensional radio adjustment
    if (i && !(i % majorDim)) {
      if (windowStyle & wxHORIZONTAL) {
        x_offset = startX;
        y_offset += maxHeight + maxHeight/2;
      } else {
        y_offset = startY;
        x_offset += maxWidth + cx1;
      }
    }

    if (radioWidth[i] < 0) {
      // It's a labeled item
      GetWindowTextW(radioButtons[i], (wchar_t *)wxBuffer, 300);
      GetLabelExtent(wxStripMenuCodes(wxNARROW_STRING((wchar_t *)wxBuffer)), 
		     &current_width, &cyf);
      eachWidth = (int)(current_width + RADIO_SIZE);
      eachHeight = (int)cyf;
    } else {
      eachWidth = radioWidth[i];
      eachHeight = radioHeight[i];
    }

    MoveWindow(radioButtons[i], x_offset, y_offset, eachWidth, eachHeight, TRUE);
    if (windowStyle & wxHORIZONTAL)
      x_offset += maxWidth + cx1;
    else
      y_offset += maxHeight + maxHeight/2;

    ::InvalidateRect(radioButtons[i], NULL, TRUE);
  }
  OnSize(width, height);
}

void wxRadioBox::GetSize(int *width, int *height)
{
  RECT rect;
  int i;

  rect.left = -1; rect.right = -1; rect.top = -1; rect.bottom = -1;

  if (ms_handle)
    wxFindMaxSize((HWND)ms_handle, &rect);

  for (i = 0; i < no_items; i++) {
    wxFindMaxSize((HWND)radioButtons[i], &rect);
  }

  *width = rect.right - rect.left;
  *height = rect.bottom - rect.top;
}

void wxRadioBox::GetClientSize(int *width, int *height)
{
  GetSize(width, height);
}

void wxRadioBox::GetPosition(int *x, int *y)
{
  RECT rect;
  wxWindow *parent;
  int i;
  POINT point;

  parent = GetParent();

  rect.left = -1; rect.right = -1; rect.top = -1; rect.bottom = -1;

  for (i = 0; i < no_items; i++) {
    wxFindMaxSize(radioButtons[i], &rect);
  }

  if (ms_handle)
    wxFindMaxSize((HWND)ms_handle, &rect);

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

char *wxRadioBox::GetLabel(void)
{
  if (ms_handle) {
    if (!GetWindowText((HWND)ms_handle, wxBuffer, 300))
      return NULL;
    return wxBuffer;
  } else
    return NULL;
}

void wxRadioBox::SetLabel(char *label)
{
  if (ms_handle) {
    if (GetWindowText((HWND)ms_handle, wxBuffer, 300))
      SetWindowTextW((HWND)ms_handle, wxWIDE_STRING(label));
  }
}

void wxRadioBox::SetFocus(void)
{
  if (no_items > 0)
   ::SetFocus(radioButtons[0]);
}

Bool wxRadioBox::Show(Bool show)
{
  int cshow;
  int i;
  wxChildList *childs;

  SetShown(show);

  childs = window_parent->GetChildren();
  childs->Show(this, show);

  if (show)
    cshow = SW_SHOW;
  else
    cshow = SW_HIDE;

  ShowWindow((HWND)ms_handle, cshow);

  for (i = 0; i < no_items; i++) {
    ShowWindow(radioButtons[i], cshow);
  }

  return TRUE;
}

// Enable a specific button
void wxRadioBox::Enable(int item, Bool enable)
{
  if (item >= 0 && item < no_items) {
    buttonEnabled[item] = enable;
    if (!IsGray()) {
      ::EnableWindow(radioButtons[item], enable);
    }
  }
}

void wxRadioBox::Enable(Bool enable)
{
  wxWindow::Enable(enable);
}

void wxRadioBox::ChangeToGray(Bool gray)
{
  int i;

  wxWindow::ChangeToGray(gray);

  for (i = 0; i < no_items; i++) {
    if (gray) {
      ::EnableWindow(radioButtons[i], FALSE);
    } else {
      ::EnableWindow(radioButtons[i], buttonEnabled[i]);
    }
  }
}

// Show a specific button
void wxRadioBox::Show(int item, Bool show)
{
  if (item < 0)
    wxRadioBox::Show(show);
  else if (item < no_items) {
    int cshow;
    if (show)
      cshow = SW_SHOW;
    else
      cshow = SW_HIDE;
    ShowWindow(radioButtons[item], cshow);
  }
}

int wxRadioBox::ButtonFocus(int which)
{
  if (which < 0) {
    int i;
    HWND fw;
    fw = ::GetFocus();
    for (i = no_items; i--; ) {
      if (fw == radioButtons[i])
	return i;
    }
  } else if (which < no_items)
    ::SetFocus(radioButtons[which]);

  return -1;
}
