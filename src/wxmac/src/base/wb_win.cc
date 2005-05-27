/*
 * File:	wb_win.cc
 * Purpose:	wxWindow class implementation
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
#include "wx_win.h"
#include "wx_gdi.h"
#include "wx_utils.h"
#include "wx_dialg.h"

// Constructor
wxbWindow::wxbWindow(void)
{
  __type = wxTYPE_WINDOW;
  windowStyle = 0;
  wx_client_data = NULL;
  font = NULL;
  handle = NULL;
  windowName = NULL;
  callback = 0;
  wx_cursor = NULL;
  paintingEnabled = TRUE;
  doubleClickAllowed = 0 ;
  winCaptured = FALSE;
}

void wxbWindow::InitDefaults(void)
{
  __type = wxTYPE_WINDOW;
  windowStyle = 0;
  doubleClickAllowed = 0 ;
  paintingEnabled = TRUE;
  wx_client_data = NULL;
  handle = NULL;
  wx_cursor = NULL /* wxSTANDARD_CURSOR */;
  callback = NULL;
  font = NULL;
}

wxbWindow::wxbWindow // Constructor
(
 char*		windowName
 ) :
 wxObject ()
{
  windowName = copystring(windowName);
  InitDefaults();
}

// Destructor
wxbWindow::~wxbWindow(void)
{
  windowName = NULL;
}

char *wxbWindow::GetHandle(void)
{
  return handle;
}

// General callback setting
void wxbWindow::Callback(wxFunction Function)
{
  if (Function)
    callback = Function;
}

// Client data handling (any window, item etc.)
void wxbWindow::SetClientData(char *data)
{
  wx_client_data = data;
}

char *wxbWindow::GetClientData(void)
{
  return wx_client_data;
}

void wxbWindow::MakeModal(Bool modal)
{
  // Disable all other windows
  if (wxSubType(__type, wxTYPE_DIALOG_BOX) || wxSubType(__type, wxTYPE_FRAME))
    {
      wxChildList *tlw;
      wxChildNode *node;
      wxWindow *win;

      tlw = wxTopLevelWindows(ContextWindow());
      node = tlw->First();
      while (node) {
	win = (wxWindow *)(node->Data());
	if (win != this)
	  win->Enable(!modal);
	
	node = node->Next();
      }
    }
}

void wxbWindow::SetName(char *name)
{
  if (name) {
    windowName = copystring(name);
  } else
    windowName = NULL;
}

wxWindow *wxbWindow::ContextWindow()
{
  if (wxSubType(__type, wxTYPE_FRAME))
    return (wxWindow *)(this);
  if (wxSubType(__type, wxTYPE_DIALOG_BOX))
    return ((wxDialogBox *)this)->cFrame;
  return NULL;
}
