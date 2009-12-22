/*
 * File:	wx_item.cc
 * Purpose:	Panel item implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

// The MakeProcInstance version of the function
FARPROC wxGenericControlSubClassProc = 0;

wxNonlockingHashTable *wxControlHandleList = NULL;
wxNonlockingHashTable *wxItemIdList = NULL;

extern "C" void scheme_start_atomic();
extern "C" void scheme_end_atomic_no_swap();

extern int wx_choice_dropped;

extern void wxEntered(wxWindow *w, int x, int y, int flags);

extern HCURSOR wxMSWSetCursor(HCURSOR c);

extern long last_msg_time; /* MATTHEW: timeStamp implementation */

long NewId(wxItem *item)
{
  WORD id;

  if (!wxItemIdList) {
    wxREGGLOB(wxItemIdList);
    wxItemIdList = new wxNonlockingHashTable;
  }

  do {
    id = (WORD)rand();
  } while (wxItemIdList->Get((long)id));

  wxItemIdList->Put(id, item);

  return id;
}

void DoneIds(wxItem *item)
{
  if (wxItemIdList)
    wxItemIdList->DeleteObject(item);
}

// Item members
wxItem::wxItem(wxPanel *pnl) : wxbItem(pnl)
{
  isFafa = FALSE ;
  oldWndProc = 0;
  isBeingDeleted = FALSE;
}

wxItem::~wxItem(void)
{
  wxPanel *panel;
  wxObject *obj;

  DoneIds(this);

  isBeingDeleted = TRUE;
  
  // item may be a menu, so check.
  obj = (wxObject *)GetParent();
  if (!obj || !wxSubType(obj->__type, wxTYPE_PANEL)) return;

  // If we delete an item, we should initialize the parent panel,
  // because it could now be invalid.
  panel = (wxPanel *)GetParent();
  if (panel)
  {
    panel->last_created = NULL;
    panel->cursor_x = PANEL_LEFT_MARGIN;
    panel->cursor_y = PANEL_TOP_MARGIN;
    panel->max_height = 0;
    panel->max_line_height = 0;
    panel->max_width = 0;
    panel->hSpacing = PANEL_HSPACING;
    panel->vSpacing = PANEL_VSPACING;
    panel->initial_hspacing = panel->hSpacing ;
    panel->initial_vspacing = panel->vSpacing ;
    panel->current_hspacing = panel->hSpacing ;
    panel->current_vspacing = panel->vSpacing ;

    panel->new_line = FALSE;
    panel->label_position = wxHORIZONTAL;
    panel->has_child = FALSE ;
    panel->last_created = 0 ;
  }
 
  UnsubclassControl((HWND)ms_handle);
}

void wxItem::GetSize(int *width, int *height)
{
  HWND wnd = (HWND)ms_handle;
  RECT rect;
  rect.left = -1; rect.right = -1; rect.top = -1; rect.bottom = -1;

  wxFindMaxSize(wnd, &rect);

  *width = rect.right - rect.left;
  *height = rect.bottom - rect.top;
}

void wxItem::GetPosition(int *x, int *y)
{
  HWND wnd = (HWND)ms_handle;
  wxWindow *parent;
  RECT rect;
  POINT point;
  
  parent = GetParent();

  rect.left = -1; rect.right = -1; rect.top = -1; rect.bottom = -1;

  wxFindMaxSize(wnd, &rect);

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

void wxItem::SetSize(int x, int y, int width, int height, int sizeFlags)
{
  wxWindow::SetSize(x, y, width, height, sizeFlags);
}

void wxItem::SetClientSize(int width, int height)
{
  SetSize(-1, -1, width, height);
}

void wxItem::SetLabel(char *label)
{
  SetWindowTextW((HWND)ms_handle, wxWIDE_STRING(label));
}

char *wxItem::GetLabel(void)
{
  GetWindowText((HWND)ms_handle, wxBuffer, 1000);
  return wxBuffer;
}

void wxItem::SetFocus(void)
{
  wxWindow::SetFocus();
}

Bool wxItem::Show(Bool show)
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
  ShowWindow(wnd, (BOOL)cshow);
  if (show && (__type != wxTYPE_GROUP_BOX) && (__type != wxTYPE_TAB_CHOICE))
    BringWindowToTop(wnd);

  return TRUE;
}

void wxItem::SubclassControl(HWND hWnd)
{
  FARPROC owp;

  // Subclass again for purposes of dialog editing mode
  wxAddControlHandle(hWnd, this);
  owp = (FARPROC)GetWindowLong(hWnd, GWL_WNDPROC);
  oldWndProc = owp;
  if (!wxGenericControlSubClassProc) {
    wxGenericControlSubClassProc = MakeProcInstance((FARPROC)wxSubclassedGenericControlProc,
						    wxhInstance);
  }
  SetWindowLongW(hWnd, GWL_WNDPROC, (LONG)wxGenericControlSubClassProc);
}

void wxItem::UnsubclassControl(HWND hWnd)
{
  if (oldWndProc) {
    wxRemoveControlHandle(hWnd);
    SetWindowLongW(hWnd, GWL_WNDPROC, (LONG)oldWndProc);
  }
}

// Call this repeatedly for several wnds to find the overall size
// of the widget.
// Call it initially with -1 for all values in rect.
// Keep calling for other widgets, and rect will be modified
// to calculate largest bounding rectangle.
void wxFindMaxSize(HWND wnd, RECT *rect)
{
  int left = rect->left;
  int right = rect->right;
  int top = rect->top;
  int bottom = rect->bottom;

  GetWindowRect(wnd, rect);

  if (left < 0)
    return;

  if (left < rect->left)
    rect->left = left;

  if (right > rect->right)
    rect->right = right;

  if (top < rect->top)
    rect->top = top;

  if (bottom > rect->bottom)
    rect->bottom = bottom;

}

static int skip_next_return;

extern int wx_start_win_event(const char *who, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam, int tramp, LONG *_retval);
extern void wx_end_win_event(const char *who, HWND hWnd, UINT message, int tramp);

LONG wxDoItemPres(wxItem *item, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam,
		 long *result, int tramp)
{
  LONG retval = 1;
  int nc = 0;
  int skip = 0;
  wxPanel *panel;

  *result = 0;
  
  // If not in edit mode (or has been removed from parent), call the default proc.
  panel = (wxPanel *)item->GetParent();

  if (!wx_start_win_event("item", hWnd, message, wParam, lParam, tramp, &retval)) {
    /* Something has gone wrong. Give up. */
    return retval;
  }

  if (panel && !item->isBeingDeleted) {
    /* Check PreOnChar or PreOnEvent */
    switch (message) {
    case WM_NCHITTEST:
      /* For messages, override hittest to claim it's always in the client area */
      if (wxSubType(item->__type, wxTYPE_MESSAGE)) {
	*result = HTCLIENT;
	retval = FALSE;
      }
      break;
    case WM_SETFOCUS:
      {
	if (item->IsShownTree()) {
	  wxWindow *p;
	  p = item->GetTopLevel();
	  p->focusWindow = item;
	  
	  item->OnSetFocus();
	}
      }
      break;
    case WM_KILLFOCUS:
      item->OnKillFocus();
      break;
    case WM_NCRBUTTONDOWN:
    case WM_NCRBUTTONUP:
    case WM_NCRBUTTONDBLCLK:
    case WM_NCMBUTTONDOWN:
    case WM_NCMBUTTONUP:
    case WM_NCMBUTTONDBLCLK:
    case WM_NCLBUTTONDOWN:
    case WM_NCLBUTTONUP:
    case WM_NCLBUTTONDBLCLK:
    case WM_NCMOUSEMOVE:
      if ((wParam != HTVSCROLL) && (wParam != HTHSCROLL))
	break;
    case WM_MOUSEMOVE: /** ^^ nc falls though ^^ */
      if (message == WM_MOUSEMOVE) {
	if (!wxIsBusy()) {
	  /* Set local cursor */
	  wxWindow *w = item;
	  while (w) {
	    if (w->wx_cursor) {
	      wxMSWSetCursor(w->wx_cursor->ms_cursor);
	      break;
	    }
	    w = w->GetParent();
	  }
	}
      } else
	nc = 1;
    case WM_RBUTTONDOWN: /** ^^ move and nc fall though ^^ */
    case WM_RBUTTONUP:
    case WM_RBUTTONDBLCLK:
    case WM_MBUTTONDOWN:
    case WM_MBUTTONUP:
    case WM_MBUTTONDBLCLK:
    case WM_LBUTTONDOWN:
    case WM_LBUTTONUP:
    case WM_LBUTTONDBLCLK:  
      {
	int et, x, y, flags;
	wxMouseEvent *event;

	switch(message) {
        case WM_RBUTTONDOWN:
        case WM_RBUTTONDBLCLK:
        case WM_NCRBUTTONDOWN:
        case WM_NCRBUTTONDBLCLK:
	  et = wxEVENT_TYPE_RIGHT_DOWN; break;
        case WM_RBUTTONUP:
        case WM_NCRBUTTONUP:
	  et = wxEVENT_TYPE_RIGHT_UP; break;
        case WM_MBUTTONDBLCLK:
        case WM_MBUTTONDOWN:
        case WM_NCMBUTTONDBLCLK:
        case WM_NCMBUTTONDOWN:
	  et = wxEVENT_TYPE_MIDDLE_DOWN; break;
        case WM_MBUTTONUP:
        case WM_NCMBUTTONUP:
	  et = wxEVENT_TYPE_MIDDLE_UP; break;
        case WM_LBUTTONDBLCLK:
        case WM_LBUTTONDOWN:
        case WM_NCLBUTTONDBLCLK:
        case WM_NCLBUTTONDOWN:
	  et = wxEVENT_TYPE_LEFT_DOWN; break;
        case WM_LBUTTONUP:
        case WM_NCLBUTTONUP:
	  et = wxEVENT_TYPE_LEFT_UP; break;
	case WM_MOUSEMOVE:
	case WM_NCMOUSEMOVE:
	  et = wxEVENT_TYPE_MOTION; break;
	}
	
	x = (int)LOWORD(lParam);
	y = (int)HIWORD(lParam);
	flags = (nc ? 0 : wParam);
	
	event = new wxMouseEvent(et);
	
	event->x = (double)x;
	event->y = (double)y;
	
	event->shiftDown = (flags & MK_SHIFT);
	event->controlDown = (flags & MK_CONTROL);
	event->leftDown = (flags & MK_LBUTTON);
	event->middleDown = (flags & MK_MBUTTON);
	event->rightDown = (flags & MK_RBUTTON);
	event->SetTimestamp(last_msg_time); /* MATTHEW: timeStamp */

	wxEntered(item, x, y, wParam);

	if (item->CallPreOnEvent(item, event))
	  retval = 0;
      }

      break;

    case WM_SYSKEYDOWN:
      if ((wParam == VK_MENU) || (wParam == VK_F4)) { /* F4 is close */
	wxUnhideCursor();
	retval = 1;
	break;
      }
    case WM_KEYDOWN:  /* ^^^ fallthrough */
      if (!((wParam != VK_ESCAPE) 
	    && (wParam != VK_SPACE) 
	    && (wParam != VK_RETURN)
	    && (wParam != VK_TAB)
	    && (wParam != VK_DELETE))) {
	/* Don't call pre-on-char for a ENTER press when
	   a choice menu is dropped-down */
	if (wx_choice_dropped) {
	  if (wParam == VK_RETURN) {
	    skip_next_return = 1;
	    retval = 1;
	    /* Otherwise, already covered by WM_CHAR */
	  } else {
	    retval = 0;
	    skip = 1;
	  }
	} else {
	  skip = 1;
	  retval = 0;
	}
      }

    case WM_SYSCHAR: /* ^^^ fallthrough */
      if (message == WM_SYSCHAR) {
	if (wParam == VK_MENU) {
	  wxUnhideCursor();
	  retval = 1;
	  break;
	}
      }
    case WM_CHAR:  /* ^^^ fallthrough */
      if (!skip) {
	wxKeyEvent *event;
	
	event = new wxKeyEvent(wxEVENT_TYPE_CHAR);
	
	event = wxMakeCharEvent(FALSE, wParam, lParam, 
				((message == WM_CHAR) || (message == WM_SYSCHAR)), 
				FALSE, hWnd);

	if (event) {
	  /* Don't call pre-on-char for a ENTER press when
	     a choice menu is dropped-down */
	  if (wx_choice_dropped)
	    if (event->keyCode == 13)
	      retval = 1;

	  if (item->CallPreOnChar(item, event))
	    retval = 0;
	  else if (event->metaDown)
	    retval = 0;
	}
      }
    }
  }

  wx_end_win_event("item", hWnd, message, tramp);

  return retval;
}

extern int wx_trampolining;

// Sub-classed generic control proc
LONG APIENTRY _EXPORT
  wxSubclassedGenericControlProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  LRESULT res;
  int tramp = wx_trampolining;
  wxItem *item;
  long r;

  wx_trampolining = 0;

  /* See mredmsw.cxx: */
  if (!tramp)
    if (wxEventTrampoline(hWnd, message, wParam, lParam, &res, wxSubclassedGenericControlProc))
      return res;

  if (message == WM_GETDLGCODE)
    return DLGC_WANTMESSAGE;

  item = wxFindControlFromHandle(hWnd);

  if (!item) {
    wxDebugMsg("Panic! Cannot find wxItem for this HWND in wxSubclassedGenericControlProc.\n");
    return NULL;
  }

  if (!wxDoItemPres(item, hWnd, message, wParam, lParam, &r, tramp))
    return r;

  if (!tramp)
    scheme_start_atomic();
  res = CallWindowProcW((WNDPROC)item->oldWndProc, hWnd, message, wParam, lParam);
  if (!tramp)
    scheme_end_atomic_no_swap();

  return res;
}

wxItem *wxFindControlFromHandle(HWND hWnd)
{
  if (!wxControlHandleList)
    return NULL;

  return (wxItem *)wxControlHandleList->Find((long)hWnd);
}

void wxAddControlHandle(HWND hWnd, wxItem *item)
{
  if (!wxControlHandleList) {
    wxREGGLOB(wxControlHandleList);
    wxControlHandleList = new wxNonlockingHashTable;
  }
  wxControlHandleList->Append((long)hWnd, item);
}

void wxRemoveControlHandle(HWND hWnd)
{
  wxControlHandleList->Delete((long)hWnd);
}

void wxSetWinFont(wxFont *buttonFont, HANDLE ms_handle)
{
  if (buttonFont) {
    HDC the_dc;
    HFONT hf;
    the_dc = GetWindowDC((HWND)ms_handle);
    hf = buttonFont->GetInternalFont(the_dc);
    if (hf) {
      SendMessage((HWND)ms_handle, WM_SETFONT, (LPARAM)hf, 0L);
    }
    ReleaseDC((HWND)ms_handle,the_dc);
  }
}

void wxItem::SetFont(wxFont *f)
{
  if (!f)
    f = wxTheFontList->FindOrCreateFont(8, wxSYSTEM, wxNORMAL, wxNORMAL, FALSE);
  font = f;
}

int wxGetControlFontSize()
{
  return 8;
}

void wxItem::GetLabelExtent(const char *string, double *x, double *y, wxFont *fnt)
{
  if (!fnt)
    fnt = font;
  GetTextExtent(string, x, y, NULL, NULL, fnt);
  if (y && ms_handle) {
    /* Keep min height consistent, even with substitutions */
    int cx, cy;
    wxGetCharSize((HWND)ms_handle, &cx, &cy, fnt);
    if (*y < cy)
      *y = cy;
  }
}
