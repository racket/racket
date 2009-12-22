/*
 * File:	wb_dialg.cc
 * Purpose:	wxDialogBox and miscellaneous dialog functions
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

// Dialog box - like panel but doesn't need a frame, and is modal or
// non-modal

wxbDialogBox::wxbDialogBox(wxWindow *WXUNUSED(Parent), char *WXUNUSED(Title), Bool Modal, 
			   int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width), int WXUNUSED(height), 
			   long style, char *WXUNUSED(name)):
  wxPanel()
{
  __type = wxTYPE_DIALOG_BOX;
  windowStyle = style;
  modal = Modal;
  SetShown(FALSE);
}

Bool wxbDialogBox::Create(wxWindow *Parent, char *WXUNUSED(Title), Bool Modal, 
			  int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width), 
			  int WXUNUSED(height), long style, char *WXUNUSED(name))
{
  wxChildList *tlw;

  windowStyle = style;
  modal = Modal;

  context = wxGetContextForFrame();

  tlw = wxTopLevelWindows(this);
  tlw->Append(this);
  tlw->Show(this, FALSE);

  SetShown(FALSE);

  return TRUE;
}

wxbDialogBox::~wxbDialogBox()
{
  wxChildList *tlw;
  tlw = wxTopLevelWindows(this);
  tlw->DeleteObject(this);
  context = NULL;
}

void wxbDialogBox::Centre(int direction)
{
  int x_offset,y_offset;
  int display_width, display_height;
  int  width, height, x, y;
  wxFrame *frame;

  if (1 /* direction & wxCENTER_FRAME */) {
    frame = (wxFrame*)GetParent();
    if (frame) {
      frame->GetPosition(&x_offset, &y_offset);
      frame->GetSize(&display_width, &display_height);
    }
  } else
    frame = NULL;

  if (!frame) {
    wxDisplaySize(&display_width, &display_height);
    x_offset = 0;
    y_offset = 0;
  }

  GetSize(&width, &height);
  GetPosition(&x, &y);

  if (direction & wxHORIZONTAL)
    x = (int)((display_width - width)/2);
  if (direction & wxVERTICAL)
    y = (int)((display_height - height)/2);

  x += x_offset;
  y += y_offset;

  if (frame) {
    /* Stay completely on the frame's screen: */
    HWND fw;
    fw = frame->GetHWND();
    if (fw) {
      HMONITOR hm;
      hm = MonitorFromWindow(fw, MONITOR_DEFAULTTOPRIMARY);
      if (hm) {
	MONITORINFO mi;
	mi.cbSize = sizeof(mi);
	if (GetMonitorInfo(hm, &mi)) {
	  if (x + width > mi.rcWork.right)
	    x = mi.rcWork.right - width;
	  if (x < mi.rcWork.left)
	    x = mi.rcWork.left;
	  if (y + height > mi.rcWork.bottom)
	    y = mi.rcWork.bottom - height;
	  if (y < mi.rcWork.top)
	    y = mi.rcWork.top;
	}
      }
    }
  }

  SetSize(x, y, width, height);
}


