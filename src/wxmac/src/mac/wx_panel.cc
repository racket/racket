///////////////////////////////////////////////////////////////////////////////
// File:	wx_panel.cc
// Purpose:	wxPanel class implementation (Mac version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wx_panel.h"
#include "wx_frame.h"
#include "wx_area.h"
#include "wxBorderArea.h"
#include "wxRectBorder.h"
#include "wxMacDC.h"

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxPanel::wxPanel // Constructor (given parentArea)
(
 wxArea*		parentArea,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 char*		windowName,
 WXTYPE		objectType
 ) :
  wxbPanel (windowName, parentArea, x, y, width, height, style | wxFLAT)
{
  CreateWxPanel(x, y, width, height);
}

//-----------------------------------------------------------------------------
wxPanel::wxPanel // Constructor (given parentFrame)
(
 wxFrame*	parentFrame,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 char*		windowName,
 WXTYPE		objectType
 ) :
  wxbPanel (windowName, parentFrame, x, y, width, height, style | wxFLAT)
{
  CreateWxPanel(x, y, width, height);
}

//-----------------------------------------------------------------------------
wxPanel::wxPanel // Constructor (given parentPanel)
(
 wxPanel*	parentPanel,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 char*		windowName,
 WXTYPE		objectType
 ) :
  wxbPanel (windowName, parentPanel, x, y, width, height, style | wxFLAT)
{
  CreateWxPanel(x, y, width, height);
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxPanel::~wxPanel(void)
{
}

//=============================================================================
// Private methods
//=============================================================================

//-----------------------------------------------------------------------------
void wxPanel::CreateWxPanel(int x, int y, int w, int h) // common constructor initialization
{
  wxWindow *parent;

  InitDefaults();

  SetEraser(wxCONTROL_BACKGROUND_BRUSH);

  if (cStyle & wxBORDER)  {
    cPanelBorder = new WXGC_PTRS wxBorderArea(this, 1, wxAll, 1);
  }

  parent = GetParent();
  if (wxSubType(parent->__type, wxTYPE_PANEL)) {
    if (parent->IsHidden())
      DoShow(FALSE);
  } else {
    if (x == -1 && y == -1 && w == -1 && h == -1) {
      wxChildList *cl;
      cl = parent->GetChildren();
      if (cl->Number() == 1) {
	/* Fill the frame/dialog */
	int w, h;
	parent->GetClientSize(&w, &h);
	SetSize(0, 0, w, h);
      }
    }
  }

#if 0
  /* Handled in wxCanvas construction: */
  if (GetParent()->IsHidden())
    DoShow(FALSE);
  InitInternalGray();
#endif
}

//-----------------------------------------------------------------------------
void wxPanel::InitDefaults(void)
{
  // For absolute-positioning auto layout
  cursor_x = PANEL_LEFT_MARGIN;
  cursor_y = PANEL_TOP_MARGIN;
  max_width = 0;
  max_height = 0;
  max_line_height = 0;
  currentRow = 0;
  currentCol = 0;
  last_created = NULL;
  cPanelBorder = NULL;
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Item placement methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxPanel::NewLine(void)
{
  AdvanceCursor(NULL); // Kludge
  cursor_x = initial_hspacing;
  cursor_y = max_height + vSpacing;
  if (cursor_y > max_height) max_height = cursor_y;
  max_line_height = 0;
}

//-----------------------------------------------------------------------------
void wxPanel::NewLine(int pixels)
{
  AdvanceCursor(NULL); // Kludge
  cursor_x = initial_hspacing;
  cursor_y = max_height + pixels;
  if (cursor_y > max_height) max_height = cursor_y;
  max_line_height = 0;
}

//-----------------------------------------------------------------------------
void wxPanel::Tab(void)
{
  AdvanceCursor(NULL); // Kludge
  cursor_x += hSpacing;
  if (cursor_x > max_width) max_width = cursor_x;
}

//-----------------------------------------------------------------------------
void wxPanel::Tab(int pixels)
{
  AdvanceCursor(NULL); // Kludge
  cursor_x += pixels;
  if (cursor_x > max_width) max_width = cursor_x;
}

//-----------------------------------------------------------------------------
int wxPanel::GetHorizontalSpacing(void) { return hSpacing; }

//-----------------------------------------------------------------------------
void wxPanel::SetHorizontalSpacing(int sp)
{
  hSpacing = sp;
  current_hspacing = sp;
}

//-----------------------------------------------------------------------------
int wxPanel::GetVerticalSpacing(void) { return vSpacing; }

//-----------------------------------------------------------------------------
void wxPanel::SetVerticalSpacing(int sp)
{
  vSpacing = sp;
  current_vspacing = sp;
}

//-----------------------------------------------------------------------------
void wxPanel::GetCursor(int* x, int* y)
{
  AdvanceCursor(NULL);
  *x = cursor_x;
  *y = cursor_y;
}

//-----------------------------------------------------------------------------
void wxPanel::SetItemCursor(int x, int y)
{
  last_created = NULL;
  cursor_x = x;
  cursor_y = y;
}

//-----------------------------------------------------------------------------
// Update next cursor position
//-----------------------------------------------------------------------------
void wxPanel::AdvanceCursor(wxWindow* item)
{
  //''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  // Major design change: I calculate cursor_x, cursor_y based on last_created,
  //                      and I store the current item, for next time.
  //
  // In this way, I can change the size of the current item without harm.
  // When I start a new item, the last_created should have correct size by then.
  //
  //''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

  if (last_created) // Kludge
    {
      int width, height;
      int x, y;
      last_created->GetSize(&width, &height);
      last_created->GetPosition(&x, &y);
	
      if ((x + width) > max_width) max_width = x + width;
      if ((y + height) > max_height) max_height = y + height;
      if (height > max_line_height) max_line_height = height;
	
      cursor_x = x + width + hSpacing;
      cursor_y = y;
    }

  last_created = item;
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Sizing methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

void wxPanel::DoSetSize(int x, int y, int width, int height)
{
  if (x==-1) 
    x= cWindowX;
  if (y==-1) 
    y = cWindowY;
  if (width==-1) 
    width = cWindowWidth;
  if (height==-1) 
    height = cWindowHeight;
 		
  wxWindow::DoSetSize(x,y,width,height);
}


//-----------------------------------------------------------------------------
void wxPanel::Centre(int direction, wxWindow *)
{
  int width, height, panel_width, panel_height, new_x, new_y;
  wxPanel* father;

  father = (wxPanel*)GetParent();
  if (!father) return;

  father->GetClientSize(&panel_width, &panel_height);
  GetSize(&width, &height);

  new_x = cWindowX;
  new_y = cWindowY;

  if (direction & wxHORIZONTAL)
    new_x = (int)((panel_width - width)/2);

  if (direction & wxVERTICAL)
    new_y = (int)((panel_height - height)/2);

  SetSize(new_x, new_y, cWindowWidth, cWindowHeight, wxPOS_USE_MINUS_ONE);
}

//-----------------------------------------------------------------------------
void wxPanel::Fit(void)
{ // Fit panel around its items
  int maxX = 0;
  int maxY = 0;
  wxChildList *cl;
  wxChildNode* childWindowNode;
  wxWindow* childWindow;
  wxArea *carea;

  carea = ClientArea();
  cl = carea->Windows();
  childWindowNode = cl->First();
  while (childWindowNode) {
    int x, y, w, h;

    childWindow = (wxWindow*)childWindowNode->Data();
    childWindow->GetPosition(&x, &y);
    childWindow->GetSize(&w, &h);
    if ((x + w) > maxX) maxX = x + w;
    if ((y + h) > maxY) maxY = y + h;
    childWindowNode = childWindowNode->Next();
  }

  SetClientSize(maxX + initial_hspacing, maxY + initial_vspacing);
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Tree methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//-----------------------------------------------------------------------------
// tom: new two are needed to be able to destroy children and be able to
// insert a new child afterwards
 
void wxPanel::DestroyChildren(void) 
{
  wxWindow::DestroyChildren();
  last_created = NULL;
}
 
void wxPanel::OnDeleteChild(wxWindow* win) 
{
  if (last_created==win)
    last_created = NULL;
}


//-----------------------------------------------------------------------------
void wxPanel::AddChild(wxObject* child) // WCH: why isn't the type wxWindow*?
{
  if (((wxWindow*)child)->ParentArea() == cClientArea) // WCH: kludge
    {
      if (!has_child)
	{
	  initial_hspacing = cursor_x; // WCH: not quite correct
	  initial_vspacing = cursor_y; // WCH: not quite correct
	  has_child = TRUE;
	}
      AdvanceCursor((wxWindow*)child); // WCH: want AddChild(wxWindow* child)
    }

  children->Append(child);
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxPanel::SetBackgroundColour(wxColour*col)
{
  backColour = col;
}

//-----------------------------------------------------------------------------
void wxPanel::DoShow(Bool show)
{
  wxNode* areaNode;
  wxArea* area;
  wxChildList *cl;
  wxChildNode* childWindowNode;
  wxWindow* childWindow;

  if (!CanShow(show))
    return;

  if (cPanelBorder)
    ((wxBorderArea *)cPanelBorder)->cBorder->DoShow(show);

  if (show)
    wxWindow::DoShow(show);

  areaNode = cAreas->First();
  while (areaNode) {
    area = (wxArea*)areaNode->Data();
    cl = area->Windows();
    childWindowNode = cl->First();
    while (childWindowNode) {
      childWindow = (wxWindow*)childWindowNode->Data();
      childWindow->DoShow(show);
      childWindowNode = childWindowNode->Next();
    }
    areaNode = areaNode->Next();
  }

  if (!show)
    wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------
void wxPanel::Paint(void)
{
  if (cHidden) return;
  wxCanvas::Paint();
}

//-----------------------------------------------------------------------------
void wxPanel::OnChar(wxKeyEvent *event)
{
}

void wxPanel::OnEventCheckMetal(wxMouseEvent *event, int metal_drag_ok)
{
  if (metal_drag_ok && MaybeMetalDrag(event)) 
    return;
  wxbPanel::OnEventCheckMetal(event, metal_drag_ok);
}

void wxPanel::AdjustMetalDragOk(int *metal_drag_ok)
{
  /* A panel is transparent to metal-dragging clicks */
}

Bool wxPanel::WantsFocus()
{
  return TRUE;
}

void wxPanel::SetSize(int x, int y, int width, int height, int flags)
{
  wxWindow::SetSize(x,y,width,height,flags);

  MaybeMoveControls();
}

//-----------------------------------------------------------------------------

void wxPanel::MaybeMoveControls()
{
  // (foreach maybe-move-controls window-children)
  wxChildNode *childNode;
  wxWindow *win;

  childNode = children->First();
  while (childNode) {
    win = (wxWindow *)childNode->Data();
    if (win) {
      win->MaybeMoveControls();
      childNode = childNode->Next();
    }
  }
}	
	

ControlHandle wxPanel::GetRootControl(void)
{
  if (paneControl)
    return paneControl;
  else
    return wxbPanel::GetRootControl();
}

void wxPanel::OnClientAreaDSize(int dW, int dH, int dX, int dY)
{
  wxbPanel::OnClientAreaDSize(dW, dH, dX, dY);
}
