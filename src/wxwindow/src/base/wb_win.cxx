/*
 * File:	wb_win.cc
 * Purpose:	wxWindow class implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

// Constructor
wxbWindow::wxbWindow(void)
{
  __type = wxTYPE_WINDOW;
  windowStyle = 0;
  window_parent = NULL;
  font = NULL;
  handle = NULL;
  callback = 0;
  wx_cursor = NULL /* wxSTANDARD_CURSOR */;
  children = new wxChildList;
  winCaptured = FALSE;
  internal_disabled = 0;
  is_shown = 1;
  WXGC_IGNORE(this, window_parent);
}

// Destructor
wxbWindow::~wxbWindow(void)
{
  window_parent = NULL;
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

wxWindow *wxbWindow::GetParent(void)
{
  return window_parent;
}

wxWindow *wxbWindow::GetGrandParent(void)
{
  wxWindow *par;
  
  par = GetParent();
  if (par)
    return par->GetParent();
  else
    return NULL;
}

void wxbWindow::AddChild(wxObject *child)
{
  children->Append(child);
}

void wxbWindow::RemoveChild(wxObject *child)
{
  if (children)
    children->DeleteObject(child);
}

void wxbWindow::DestroyChildren(void)
{
  if (children) {
    wxChildNode *node;
    while ((node = children->First()) != NULL) {
      wxWindow *child;
      if ((child = (wxWindow *)node->Data()) != (wxWindow *)NULL) {
        delete child;
      }
    }
  }
}

Bool wxbWindow::IsShown()
{
  return is_shown;
}

void wxbWindow::SetShown(Bool s)
{
  is_shown = s;
}

Bool wxbWindow::IsShownTree()
{
  if (wxSubType(__type, wxTYPE_DIALOG_BOX) || wxSubType(__type, wxTYPE_FRAME))
    return TRUE;
  else {
    wxWindow *p;
    p = GetParent();

    if (IsShown())
      return p->IsShownTree();
    else
      return FALSE;
  }
}

void wxbWindow::MakeModal(Bool modal)
{
  // Disable all other windows
  if (wxSubType(__type, wxTYPE_DIALOG_BOX) || wxSubType(__type, wxTYPE_FRAME)) {
    wxChildNode *node;
    wxChildList *tlw;
    tlw = wxTopLevelWindows(this);
    node = tlw->First();
    while (node) {
      wxWindow *win;
      win = (wxWindow *)node->Data();
      if (win != this)
        win->Enable(!modal);
      
      node = node->Next();
    }
  }
}

// If nothing defined for this, try the parent.
// E.g. we may be a button loaded from a resource, with no callback function
// defined.
void wxbWindow::OnCommand(wxWindow *win, wxCommandEvent *event)
{
  if (window_parent)
    window_parent->OnCommand(win, event);
}

void wxbWindow::OnSize(int WXUNUSED(width), int WXUNUSED(height))
{
}


/*
 * Event handler
 */

wxEvtHandler::wxEvtHandler(void)
{
}

wxEvtHandler::~wxEvtHandler(void)
{
}

void wxbWindow::ForEach(wxForEachProc foreach, void *data)
{
  wxChildNode *node;
  wxChildList *cl;

  cl = GetChildren();
  node = cl->First();
  while (node) {
    wxChildNode *next;
    wxWindow *win;

    next = node->Next();
    win = (wxWindow *)node->Data();
    win->ForEach(foreach, data);
    node = next;
  }

  foreach((wxWindow *)this, data);
}

Bool wxbWindow::GetsFocus()
{
  return TRUE;
}
