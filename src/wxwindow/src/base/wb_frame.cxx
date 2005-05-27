/*
 * File:	wb_frame.cc
 * Purpose:	wxFrame implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

wxbFrame::wxbFrame(wxFrame *WXUNUSED(Parent), char *WXUNUSED(title),
		   int WXUNUSED(x), int WXUNUSED(y),
		   int WXUNUSED(width), int WXUNUSED(height),
		   long style, char *WXUNUSED(name))
{
  __type = wxTYPE_FRAME;
  windowStyle = style;
  SetShown(FALSE);
}

Bool wxbFrame::Create(wxFrame *Parent, char *WXUNUSED(title),
		      int WXUNUSED(x), int WXUNUSED(y),
		      int WXUNUSED(width), int WXUNUSED(height),
		      long style, char *WXUNUSED(name))
{
  wxChildList *tlw;

  windowStyle = style;

  context = wxGetContextForFrame();

  tlw = wxTopLevelWindows(this);
  tlw->Append(this);
  tlw->Show(this, FALSE);

  SetShown(FALSE);

  return TRUE;
}

wxbFrame::~wxbFrame(void)
{
  wxChildList *tlw;
  tlw = wxTopLevelWindows(this);
  tlw->DeleteObject(this);
}

// Default resizing behaviour - if only ONE subwindow,
// resize to client rectangle size
void wxbFrame::OnSize(int WXUNUSED(x), int WXUNUSED(y))
{
  wxWindow *child = NULL;
  int noChildren = 0;
  wxChildNode *node;
  wxChildList *cl;
  wxWindow *win;
  WXTYPE winType;
  int client_x, client_y;

  if (frame_type == wxMDI_PARENT)
    return;

  // Search for a child which is a subwindow, not another frame.
  // Count the number of _subwindow_ children
  cl = GetChildren();
  for(node = cl->First(); node; node = node->Next())
  {
    win = (wxWindow *)node->Data();
    winType = win->__type;

    if (wxSubType(winType, wxTYPE_PANEL) ||
        wxSubType(winType, wxTYPE_TEXT_WINDOW) ||
        wxSubType(winType, wxTYPE_CANVAS))
    {
      child = win;
      noChildren ++;
    }
  }

  // If not one child, call the Layout function if compiled in
  if (!child || (noChildren > 1))
    return;

  GetClientSize(&client_x, &client_y);
  child->SetSize(0, 0, client_x, client_y);
}

// Default activation behaviour - set the focus for the first child
// subwindow found.
void wxbFrame::OnActivate(Bool WXUNUSED(flag))
{
}

// Default menu selection behaviour - display a help string
void wxbFrame::OnMenuSelect(long id)
{
  wxMenuBar *menuBar;

  if (StatusLineExists()) {
    menuBar = GetMenuBar();
    if (menuBar) {
      char *helpString;
      helpString = menuBar->GetHelpString(id);
      if (helpString) {
	SetStatusText(helpString);
	return;
      }
    }

    SetStatusText("");
  }
}

void wxbFrame::OnMenuClick(void)
{
}

wxMenuBar *wxbFrame::GetMenuBar(void)
{
  return wx_menu_bar;
}

Bool wxbFrame::StatusLineExists(void)
{
  return status_line_exists;
}

void wxbFrame::Centre(int direction)
{
  int display_width, display_height, width, height, x, y;

  wxDisplaySize(&display_width, &display_height);

  GetSize(&width, &height);
  GetPosition(&x, &y);

  if (direction & wxHORIZONTAL)
    x = (int)((display_width - width)/2);
  if (direction & wxVERTICAL)
    y = (int)((display_height - height)/2);

  SetSize(x, y, width, height);
}

// Call this to simulate a menu command
void wxbFrame::Command(long id)
{
  ProcessCommand(id);
}

void wxbFrame::ProcessCommand(long id)
{
  OnMenuCommand(id);
}
