/*
 * File:	wb_panel.cc
 * Purpose:	wxPanel class implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#ifdef __GNUG__
#pragma implementation
#endif
#endif

#include "common.h"
#include "wb_panel.h"
#include "wx_buttn.h"
#include "wx_stdev.h"

class wxFrame;
class wxPanel;

// Constructors

wxbPanel::~wxbPanel(void)
{
}

wxObject* wxbPanel::GetChild(int number)
{
  wxChildList *cl;
  wxChildNode *node;
  // Return a pointer to the Nth object in the Panel
  if (!children)
    return(NULL) ;
  cl = GetChildren();
  node = cl->First();
  while (node && number--) {
    node = node->Next() ;
  }
  if (node) {
    return (wxObject *)node->Data();
  } else
    return NULL;
}

void wxbPanel::SetLabelPosition(int pos)  // wxHORIZONTAL or wxVERTICAL
{
  label_position = pos;
}

int wxbPanel::GetLabelPosition(void)
{
  return label_position;
}

void wxbPanel::OnDefaultAction(wxItem *initiatingItem)
{
}

void wxbPanel::SetBackgroundColour(wxColour *col)
{
  backColour = col;
}

//=============================================================================
// Protected constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxbPanel::wxbPanel // Constructor (given parentArea)
(
 char*		windowName,
 wxArea*		parentArea,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style
 ) :
 wxCanvas (parentArea, x, y, width, height, style, windowName)
{
  __type = wxTYPE_PANEL;
  InitDefaults();
  InitMoreDefaults();
}

//-----------------------------------------------------------------------------
wxbPanel::wxbPanel // Constructor (given parentWindow)
(
 char*		windowName,
 wxWindow*	parentWindow,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style
 ) :
 wxCanvas (parentWindow, x, y, width, height, style, windowName)
{
  __type = wxTYPE_PANEL;
  InitDefaults();
  InitMoreDefaults();
}

//=============================================================================
// Private methods
//=============================================================================

//-----------------------------------------------------------------------------
void wxbPanel::InitDefaults(void)
{
  defaultItem = NULL;

  has_child = FALSE;

  hSpacing = PANEL_HSPACING;
  vSpacing = PANEL_VSPACING;
  initial_hspacing = hSpacing;
  initial_vspacing = vSpacing;
  current_hspacing = hSpacing;
  current_vspacing = vSpacing;

  new_line = FALSE;
}

//-----------------------------------------------------------------------------
void wxbPanel::InitMoreDefaults(void) // Poor name for this method
{
  if (wxSubType(window_parent->__type, wxTYPE_PANEL)
      && (cParentArea == window_parent->ClientArea())) {
    wxPanel* parentPanel = (wxPanel*)window_parent;
    backColour = parentPanel->backColour;
    buttonColour = parentPanel->buttonColour;
    buttonFont = parentPanel->buttonFont;
    labelColour = parentPanel->labelColour;
    labelFont = parentPanel->labelFont;
    label_position = parentPanel->label_position;
  } else {
    backColour = NULL;
    buttonColour = NULL;
    buttonFont = wxNORMAL_FONT;
    labelColour = NULL;
    labelFont = wxNORMAL_FONT;
    label_position = wxHORIZONTAL;
  }
}
