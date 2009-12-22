/*
 * File:	wx_slidr.cc
 * Purpose:	Slider implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	April 1995
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#define SHOW_MIN_MAX 0

// Slider
wxNonlockingHashTable *wxSliderList;

wxSlider::wxSlider(wxPanel *panel, wxFunction func, char *label, int value,
           int min_value, int max_value, int width, int x, int y,
           long style, wxFont *_font, char *name):
  wxbSlider(panel, func, label, value, min_value, max_value, width, x, y,
            style, name)
{
  SetFont(_font);
  Create(panel, func, label, value, min_value, max_value, width, x, y,
         style, name);
}

Bool wxSlider::Create(wxPanel *panel, wxFunction func, char *label, int value,
           int min_value, int max_value, int width, int x, int y, long style, char *name)
{
  wxWnd *cparent = NULL;
  int cx;
  int cy;
  char *the_label = NULL ;
  long msStyle = 0;
  HWND scroll_bar;

  panel->AddChild(this);
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

  wxGetCharSize(cparent->handle, &cx, &cy,font);

  if (label)
    the_label = copystring(label);
  
  // If label exists, create a static control for it.
  if (label) {
    int nid;
    wchar_t *ws;
    nid = NewId(this);
    ws = wxWIDE_STRING(the_label);
    static_label = CreateWindowExW(0, LSTATIC_CLASS, ws,
				   STATIC_FLAGS | WS_CLIPSIBLINGS
				   | ((style & wxINVISIBLE) ? 0 : WS_VISIBLE),
				   0, 0, 0, 0, cparent->handle, (HMENU)nid,
				   wxhInstance, NULL);

    wxSetWinFont(font, (HANDLE)static_label);
  } else
    static_label = NULL;

  if (!(style & (wxHORIZONTAL << 2))) {
    int nid;
    nid = NewId(this);
    edit_value = wxwmCreateWindowEx(0, STATIC_CLASS, NULL,
				    STATIC_FLAGS | WS_CLIPSIBLINGS
				    | ((style & wxINVISIBLE) ? 0 : WS_VISIBLE),
				    0, 0, 0, 0, cparent->handle, (HMENU)nid,
				    wxhInstance, NULL);
  } else
    edit_value = NULL;

#if SHOW_MIN_MAX
  // Now create min static control
  sprintf(wxBuffer, "%d", min_value);
  static_min = wxwmCreateWindowEx(0, STATIC_CLASS, wxBuffer,
				  STATIC_FLAGS | WS_CLIPSIBLINGS
				  | ((style & wxINVISIBLE) ? 0 : WS_VISIBLE),
				  0, 0, 0, 0, cparent->handle, (HMENU)NewId(this),
				  wxhInstance, NULL);
#else
  static_min = NULL;
#endif

  // Now create slider
  windows_id = NewId(this);
  
  if (windowStyle & wxVERTICAL)
    msStyle = SBS_VERT | WS_CHILD;
  else
    msStyle = SBS_HORZ | WS_CHILD;
    
  scroll_bar = wxwmCreateWindowEx(0, "SCROLLBAR", "",
				  msStyle | WS_CLIPSIBLINGS
				  | ((style & wxINVISIBLE) ? 0 : WS_VISIBLE),
				  0, 0, 0, 0, cparent->handle, (HMENU)windows_id,
				  wxhInstance, NULL);

  page_size = (int)((max_value-min_value)/10);
  s_max = max_value;
  s_min = min_value;

  ::SetScrollRange(scroll_bar, SB_CTL, min_value, max_value, FALSE);
  ::SetScrollPos(scroll_bar, SB_CTL, value, FALSE);
  ShowWindow(scroll_bar, SW_SHOW);

  ms_handle = (HANDLE)scroll_bar;

  wxSliderList->Append((long)ms_handle, this);

  // Subclass again for purposes of dialog editing mode
  SubclassControl(scroll_bar);

#if SHOW_MIN_MAX
  // Finally, create max value static item
  sprintf(wxBuffer, "%d", max_value);
  static_max = wxwmCreateWindowEx(0, STATIC_CLASS, wxBuffer,
				  STATIC_FLAGS | WS_CLIPSIBLINGS
				  | ((style & wxINVISIBLE) ? 0 : WS_VISIBLE),
				  0, 0, 0, 0, cparent->handle, (HMENU)NewId(this),
				  wxhInstance, NULL);
#else
  static_max = NULL;
#endif

  if (edit_value) {
    wxSetWinFont(font, edit_value);
#if SHOW_MIN_MAX
    wxSetWinFont(font, static_min);
    wxSetWinFont(font, static_max);
#endif
  }

  if (windowStyle & wxVERTICAL)
    SetSize(x, y, 1, -width, wxSIZE_AUTO);
  else
    SetSize(x, y, width, -1, wxSIZE_AUTO);
  SetValue(value);

  panel->AdvanceCursor(this);
  Callback(func);

  if (static_label)
    BringWindowToTop(static_label);
  if (edit_value)
    BringWindowToTop(edit_value);

  if (style & wxINVISIBLE)
    Show(FALSE);

  return TRUE;
}

// Called from wx_win.cc: wxWnd::OnHScroll, wxWnd::OnVScroll
void wxSliderEvent(HWND bar, WORD wParam, WORD pos)
{
  wxSlider *slider;
  int position;
  int nScrollInc;

  slider = (wxSlider *)wxSliderList->Find((long)bar);
  if (!slider)
    return;

  position = GetScrollPos(bar, SB_CTL);

  switch (wParam) {
  case SB_LINEUP:
    nScrollInc = -1;
    break;
    
  case SB_LINEDOWN:
    nScrollInc = 1;
    break;
    
  case SB_PAGEUP:
    nScrollInc = -slider->page_size;
    break;
    
  case SB_PAGEDOWN:
    nScrollInc = slider->page_size;;
    break;
    
  case SB_THUMBTRACK:
    nScrollInc = (signed short)pos - position;
    break;

  default:
    nScrollInc = 0;
  }
  
  if (nScrollInc != 0)     {
    int new_pos = position + nScrollInc;
    if (!(new_pos < slider->s_min || new_pos > slider->s_max)) {
      wxCommandEvent *event;
      slider->SetValue(new_pos);
      event = new wxCommandEvent(wxEVENT_TYPE_SLIDER_COMMAND);
      slider->ProcessCommand(event);
    }
  }
}

wxSlider::~wxSlider(void)
{
  isBeingDeleted = TRUE;
  
  wxSliderList->DeleteObject(this);

#if SHOW_MIN_MAX
  if (static_min)
    wxwmDestroyWindow(static_min);
  if (static_max)
    wxwmDestroyWindow(static_max);
#endif
  if (edit_value)
    wxwmDestroyWindow(edit_value);
  if (static_label)
    wxwmDestroyWindow(static_label);
}

Bool wxSlider::Show(Bool show) 
{
  int cshow;

  wxWindow::Show(show);

  if (show)
    cshow = SW_SHOW;
  else
    cshow = SW_HIDE;
  if (static_label) ShowWindow(static_label, (BOOL)cshow);
#if SHOW_MIN_MAX  
  if (static_min) ShowWindow(static_min, (BOOL)cshow);
  if (static_max) ShowWindow(static_max, (BOOL)cshow);
#endif
  if (edit_value) ShowWindow(edit_value, (BOOL)cshow);

  return TRUE;
}

int wxSlider::GetValue(void)
{
  return ::GetScrollPos((HWND)ms_handle, SB_CTL);
}

char *wxSlider::GetLabel(void)
{
  if (static_label)
  {
    GetWindowText(static_label, wxBuffer, 300);
    return wxBuffer;
  }
  else return NULL;
}

void wxSlider::SetValue(int value)
{
  ::SetScrollPos((HWND)ms_handle, SB_CTL, value, TRUE);

  if (edit_value)
  {
    sprintf(wxBuffer, "%d", value);
    SetWindowText(edit_value, wxBuffer);
  }

  if (!winEnabled) {
    /* Windows bug? Setting the value loses disabled state. */
    ::EnableWindow((HWND)ms_handle, (BOOL)FALSE); 
  }
}

void wxSlider::SetLabel(char *label)
{
  if (static_label)
  {
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
    if (parent)
    {
      wxWnd *cparent = (wxWnd *)(parent->handle);
      ::ScreenToClient(cparent->handle, &point);
    }

    GetLabelExtent(wxStripMenuCodes(label), &w, &h);
    MoveWindow(static_label, point.x, point.y, (int)(w + 10), (int)h,
               TRUE);
    SetWindowTextW(static_label, wxWIDE_STRING(label));
  }
}

void wxSlider::GetSize(int *width, int *height)
{
  RECT rect;

  rect.left = -1; rect.right = -1; rect.top = -1; rect.bottom = -1;

  wxFindMaxSize((HWND)ms_handle, &rect);

  if (static_label)
    wxFindMaxSize(static_label, &rect);
#if SHOW_MIN_MAX
  if (static_min)
    wxFindMaxSize(static_min, &rect);
  if (static_max)
    wxFindMaxSize(static_max, &rect);
#endif
  if (edit_value)
    wxFindMaxSize(edit_value, &rect);

  *width = rect.right - rect.left;
  *height = rect.bottom - rect.top;
}

void wxSlider::GetPosition(int *x, int *y)
{
  RECT rect;
  wxWindow *parent;
  POINT point;

  parent = GetParent();

  rect.left = -1; rect.right = -1; rect.top = -1; rect.bottom = -1;

  wxFindMaxSize((HWND)ms_handle, &rect);

  if (static_label)
    wxFindMaxSize(static_label, &rect);
#if SHOW_MIN_MAX
  if (static_min)
    wxFindMaxSize(static_min, &rect);
  if (static_max)
    wxFindMaxSize(static_max, &rect);
#endif
  if (edit_value)
    wxFindMaxSize(edit_value, &rect);

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

void wxSlider::SetSize(int x, int y, int width, int height, int sizeFlags)
{
  int currentX, currentY;
  char buf[300];
  double min_len;
  double max_len, val_width;
  int x_offset = x;
  int y_offset = y;
  double label_width = 0;
  int ecx, cx;     // slider,min,max sizes
  int ecy, cy;
  int esep;
  double cyf = 0.0;
  int cxs = 0;    // label sizes
  int cys = 0;

  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  wxGetCharSize((HWND)ms_handle, &cx, &cy, font);

  if (edit_value) {
    ecx = cx;
    ecy = cy;
    esep = 1;
  } else
    ecx = ecy = esep = 0;

  // If we're prepared to use the existing size, then...
  if (width == -1 && height == -1 && ((sizeFlags & wxSIZE_AUTO) != wxSIZE_AUTO))
    GetSize(&width, &height);

#if SHOW_MIN_MAX
  GetWindowText(static_min, buf, 300);
  GetTextExtent(buf, &min_len, &cyf, NULL, NULL, font);
#else
  min_len = 0;
#endif
  
#if SHOW_MIN_MAX
  GetWindowText(static_max, buf, 300);
  GetTextExtent(buf, &max_len, &cyf, NULL, NULL, font);
#else
  max_len = 0;
#endif

  if (edit_value) {
    double min_len, max_len;
    sprintf(buf, "%d", s_min);
    GetTextExtent(buf, &min_len, &cyf, NULL, NULL, font);
    sprintf(buf, "%d", s_max);
    GetTextExtent(buf, &max_len, &cyf, NULL, NULL, font);
    val_width = (int)(max(min_len, max_len));
  } else {
    val_width = 0;
  }

  if (static_label) {
    wchar_t wbuf[300];
    wxGetCharSize((HWND)ms_handle, &cxs, &cys, font);
    GetWindowTextW(static_label, wbuf, 300);
    GetLabelExtent(wxStripMenuCodes(wxNARROW_STRING(wbuf)), &label_width, &cyf);
  }

  if ((windowStyle & wxVERTICAL) != wxVERTICAL) {
    // Horizontal
    int slider_height = cy + ecy + ecy/3;
    int slider_length;

    // Center the whole control vertically
    if (height > 0) {
      height -= slider_height;
      if (labelPosition==wxHORIZONTAL) {
	if (cys > slider_height)
	  height -= (cys - slider_height);
      } else {
	height -= cys + 1;
      }
      height /= 2;
      if (height > 0)
	y_offset += (int)height;
    }

    if (static_label) {
      int dy, dys;  // Adjustment values (vertical) if font sizes differs.

      if (labelPosition==wxHORIZONTAL) {
	// Match center of slider and center of label
	if (cys > cy) {
	  dys = 0;
	  dy = (cys - cy)/2; // center slider
	} else {
	  dys = (cy - cys)/2; // center label
	  dy = 0;
	}
      } else
	dys = dy = 0;

      MoveWindow(static_label, x_offset, y + dys, 
		 (int)label_width, (int)cyf, TRUE);

      if (labelPosition==wxHORIZONTAL) {
	int dx = (int)(label_width + cxs);
        x_offset += dx;
        y_offset += dy;
	if (width > 0) {
	  width -= dx;
	  if (width < 0)
	    width = 0;
	}
      } else {
        y_offset += cys + 2;
	if (height > 0) {
	  height -= cys + 2;
	  if (height < 0)
	    height = 0;
	}
      }
    }

    if (width >= 0)
      slider_length = (int)(width - min_len - ecx/2 - max_len - ecx/2);
    else
      slider_length = -1;
    // Slider must have a minimum/default length
    if (slider_length < 0)
      slider_length = 100;

    if (edit_value) {
      // Center current value below the slider
      MoveWindow(edit_value, 
		 x_offset + ecx/2 + min_len + (slider_length - val_width)/2,
		 y_offset + cy + ecy/3, 
		 val_width, (int)ecy, 
		 TRUE);

#if SHOW_MIN_MAX
      MoveWindow(static_min, x_offset, y_offset, (int)min_len, ecy, TRUE);
      
      MoveWindow(static_max, x_offset + ecx + min_len + slider_length, y_offset, 
		 max_len, ecy, TRUE);
#endif
    }

    MoveWindow((HWND)ms_handle, x_offset + ecx/2 + min_len, y_offset, 
	       slider_length, cy, TRUE);
  } else {
    // Vertical
    int slider_width = max(cy, val_width) + ecx/2 + val_width;
    //                         ^-- because it's the max of min & max
    int slider_length, total_height;
    int mmcy, mmsep;

    // Center the whole control horizontally
    if (width > 0) {
      width -= slider_width;
      if (labelPosition == wxVERTICAL) {
	if (label_width > slider_width)
	  width -= (label_width - slider_width);
      } else {
	width -= label_width + 1;
      }
      width /= 2;
      if (width > 0)
	x_offset += (int)width;
    }

#if SHOW_MIN_MAX
    mmcy = ecy;
    mmsep = esep;
#else
    mmcy = 0;
    mmsep = 0;
#endif

    if (height >= 0)
      slider_length = (int)(height - 2*mmcy - 2*mmsep);
    else
      slider_length = -1;
    // Slider must have a minimum/default length
    if (slider_length < 0)
      slider_length = 100;
    total_height = slider_length + 2*mmcy + 2*mmsep;

    if (static_label) {
      int dxs, dx, dys, dy;
      if (labelPosition == wxVERTICAL) {
	int slp = max(cy, val_width);
	// Match center of slider and center of label
	if (label_width > slp) {
	  dxs = 0;
	  dx = (label_width - slp)/2; // center slider
	} else {
	  dxs = (slp - label_width)/2; // center label
	  dx = 0;
	}
	dys = dy = 0;
      } else {
	// Center label and slider vertically
	if (cys > total_height) {
	  dys = 0;
	  dy = (cys - total_height) / 2;
	} else {
	  dys = (total_height - cys) / 2;
	  dy = 0;
	}
	dxs = dx = 0;
      }

      MoveWindow(static_label, x_offset + dxs, y + dys, 
		 (int)label_width, (int)cyf, TRUE);

      if (labelPosition == wxHORIZONTAL) {
	int ldx = (int)(label_width + cxs);
        x_offset += ldx;
	if (width > 0) {
	  width -= ldx;
	  if (width < 0)
	    width = 0;
	}
	y_offset += dy;
      } else {
	x_offset += dx;
        y_offset += cys + 2;
	if (height > 0) {
	  height -= cys + 2;
	  if (height < 0)
	    height = 0;
	}
	slider_length -= cys - 2;
	if (slider_length < 0)
	  slider_length = 0;
      }
    }

    if (edit_value) {
      // Center current value to the right of the slider
      MoveWindow(edit_value, 
		 x_offset + slider_width - val_width + ecx/2,
		 y_offset + mmcy + mmsep + (slider_length - ecy)/2, 
		 val_width, (int)ecy, 
		 TRUE);

#if SHOW_MIN_MAX
      MoveWindow(static_min, x_offset + (val_width - min_len)/2, y_offset, 
		 (int)min_len, ecy, TRUE);
      
      MoveWindow(static_max, x_offset + (val_width - max_len)/2, 
		 y_offset + ecy + 2 + slider_length, 
		 max_len, ecy, TRUE);
#endif
    }

    MoveWindow((HWND)ms_handle, x_offset + (val_width - ecy)/2, y_offset + mmcy + mmsep, 
	       cy, slider_length, TRUE);
  }

  OnSize(width, height);
}

void wxSlider::ChangeToGray(Bool gray)
{
  wxWindow::ChangeToGray(gray);
  if (static_label) ::EnableWindow(static_label, !gray);
#if SHOW_MIN_MAX
  if (static_min) ::EnableWindow(static_min, !gray);
  if (static_max) ::EnableWindow(static_max, !gray);
#endif
  if (edit_value) ::EnableWindow(edit_value, !gray);
}
