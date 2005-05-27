/*
 * File:	wb_frame.cc
 * Purpose:	wxFrame implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#ifdef __GNUG__
#pragma implementation
#endif
#endif

#include "common.h"
#include "wx_setup.h"
#include "wx_frame.h"
#include "wx_gdi.h"
#include "wx_stdev.h"
#include "wx_main.h"
#include "wx_utils.h"
#include "wx_menu.h"
#include "wx_mnuit.h"
#include "wx_screen.h" 
#include "wx_stdev.h"

//-----------------------------------------------------------------------------
// Constructor (given parentScreen)
wxbFrame::wxbFrame (char* windowName, wxScreen* parentScreen,
		int x, int y, int width, int height, long style)
: wxWindow ( windowName, parentScreen, x, y, width, height, style)
{
  __type = wxTYPE_FRAME;
  
  modal_showing = FALSE;
  wx_menu_bar = NULL;
  icon = NULL;
  status_line_exists = FALSE;
  frame_type = style & (wxSDI | wxMDI_PARENT | wxMDI_CHILD);
  nb_status =0;
  
  context = wxGetContextForFrame();
  
  {
    wxChildList *tlw;
    tlw = wxTopLevelWindows(ContextWindow());
    tlw->Append(this);
    tlw->Show(this, FALSE);
  }
}

wxbFrame::~wxbFrame(void)
{
  wxChildList *tlw;
  tlw = wxTopLevelWindows(ContextWindow());
  tlw->DeleteObject(this);
}

// Default resizing behaviour - if only ONE subwindow,
// resize to client rectangle size
void wxbFrame::OnSize(int x, int y)
{
  wxWindow *child = NULL, *win;
  int noChildren = 0;
  wxChildList *cl;
  wxChildNode *node;
  int client_x, client_y;

  if (frame_type == wxMDI_PARENT)
    return;

  // Search for a child which is a subwindow, not another frame.
  // Count the number of _subwindow_ children
  cl = GetChildren();
  for (node = cl->First(); node; node = node->Next())
  {
    WXTYPE winType;

    win = (wxWindow *)(node->Data());
    winType = win->__type;

    if (wxSubType(winType, wxTYPE_PANEL)
        || wxSubType(winType, wxTYPE_TEXT_WINDOW)
        || wxSubType(winType, wxTYPE_CANVAS)) {
      child = win;
      noChildren ++;
    }
  }
  if (!child || (noChildren > 1))
    return;

  GetClientSize(&client_x, &client_y);
  child->SetSize(0, 0, client_x, client_y, 0x70);
}

void wxbFrame::OnActivate(Bool flag)
{
  wxWindow::OnActivate(flag);
}

// Default menu selection behaviour - display a help string
void wxbFrame::OnMenuSelect(int id)
{
  if (StatusLineExists()) {
    if (id == -1)
      SetStatusText("");
    else {
      wxMenuBar *menuBar;
      menuBar = GetMenuBar();
      if (menuBar) {
        char *helpString;
	helpString = menuBar->GetHelpString(id);
        if (helpString)
          SetStatusText(helpString);
      }
    }
  }
}

void wxbFrame::SetMenuBar(wxMenuBar *menu_bar)
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

  SetSize(x, y, width, height, wxPOS_USE_MINUS_ONE);
}

// Call this to simulate a menu command
void wxbFrame::Command(int id)
{
  ProcessCommand(id);
}

void wxbFrame::ProcessCommand(int id)
{
  wxMenuBar *bar;
  wxMenuItem *item;

  bar = GetMenuBar();

  if (!bar)
    return;

  item = bar->FindItemForId(id);
  if (item && item->IsCheckable()) {
    int c;
    c = bar->Checked(id);
    bar->Check(id, !c);
  }

  OnMenuCommand(id);
}
