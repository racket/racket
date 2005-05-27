///////////////////////////////////////////////////////////////////////////////
// File:	wxArea.cc
// Purpose:	window area (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wx_area.h"
#include "wx_win.h"
#include "wx_frame.h"
#include "wx_utils.h"
#include "wx_screen.h"

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Constructors
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxArea::wxArea
(
 wxWindow* parentWindow
 ) :
 wxObject(WXGC_NO_CLEANUP)
{
  wxList *al;

  cWindows = new wxChildList();
  __type = wxTYPE_AREA;	//cjc

  cParentWindow = parentWindow;
  al = parentWindow->Areas();
  al->Insert(this);

  WXGC_IGNORE(this, cParentWindow);
  
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Destructor
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxArea::~wxArea(void)
{
  wxChildNode *node, *next;
  wxWindow *win;

  for (node = cWindows->First(); node; node = next) {
    next = node->Next();
    win = (wxWindow *)(node->Data());
    if (win)
      DELETE_OBJ win;
  }
  if (cParentWindow) {
    cParentWindow->OnDeleteChildArea(this);
    cParentWindow = NULL;
  }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Geometry methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxMargin wxArea::Margin(void) { return cMargin; }

//-----------------------------------------------------------------------------
// Margin from this area to outer area
//-----------------------------------------------------------------------------
wxMargin wxArea::Margin(wxArea* outerArea)
{
  wxMargin result;
  wxArea* area = this;
  wxWindow* window;

  window = ParentWindow();
  while (area && area != outerArea) {
    area = area->Previous();
    if (area)
      result += area->Margin();
    else {
      if (window != wxScreen::gScreenWindow) {
	result += window->Margin(window->ParentArea());
	area = window->ParentArea();
	window = area->ParentWindow();
      }
    }
  }
  
  return result;
}

//-----------------------------------------------------------------------------
// Margin from this area to outer window
//-----------------------------------------------------------------------------
wxMargin wxArea::Margin(wxWindow* outerWindow)
{
  wxMargin result;
  wxArea* area = this;
  wxWindow* window;

  window = ParentWindow();
  while (area) {
    area = area->Previous();
    if (area)
      result += area->Margin();
    else {
      if (window != outerWindow) {
	if (window != wxScreen::gScreenWindow) {
	  result += window->Margin(window->ParentArea());
	  area = window->ParentArea();
	  window = area->ParentWindow();
	}
      }
    }
  }

  return result;
}

//-----------------------------------------------------------------------------
int wxArea::Width(void)
{
  wxMargin margin;
  wxWindow *p;
  margin = Margin(ParentWindow());
  p = ParentWindow();
  return p->Width() - margin.Offset(wxHorizontal);
}

//-----------------------------------------------------------------------------
int wxArea::Height(void)
{
  wxMargin margin;
  wxWindow *p;
  margin = Margin(ParentWindow());
  p = ParentWindow();
  return p->Height() - margin.Offset(wxVertical);
}

//-----------------------------------------------------------------------------
// Convert from this area c.s. to screen c.s.
//-----------------------------------------------------------------------------
void wxArea::AreaToScreen(int* h, int* v)
{
  wxMargin screenMargin;
  int n;

  screenMargin = Margin(wxScreen::gScreenWindow);

  n = screenMargin.Offset(wxLeft);
  (*h) += n;
  n = screenMargin.Offset(wxTop);
  (*v) += n;
}

//-----------------------------------------------------------------------------
// Convert from screen c.s. to this area c.s.
//-----------------------------------------------------------------------------
void wxArea::ScreenToArea(int* h, int* v)
{
  wxMargin screenMargin;
  int n;

  screenMargin = Margin(wxScreen::gScreenWindow);
  
  n = screenMargin.Offset(wxLeft);
  (*h) -= n;
  n = screenMargin.Offset(wxTop);
  (*v) -= n;
}

//-----------------------------------------------------------------------------
Bool wxArea::WindowPointInArea(int windowH, int windowV)
{
  wxMargin margin;
  int areaH, areaV;

  margin = Margin(ParentWindow());
  areaH = windowH - margin.Offset(wxLeft); // area c.s.
  areaV = windowV - margin.Offset(wxTop); // area c.s.
  return (0 <= areaH && areaH <= Width() && 0 <= areaV && areaV <= Height());
}

//-----------------------------------------------------------------------------
void wxArea::FrameContentAreaOffset(int* x, int* y)
{
  wxFrame* frame;
  wxArea* frameContentArea;
  wxMargin frameContentAreaMargin;
  wxWindow *p;
  int n;

  p = ParentWindow();
  frame = p->GetRootFrame();
  frameContentArea = frame->ContentArea();
  frameContentAreaMargin = Margin(frameContentArea);
  n = frameContentAreaMargin.Offset(wxLeft);
  *x = n;
  n = frameContentAreaMargin.Offset(wxTop);
  *y = n;
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Sizing methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxArea::SetSize(int width, int height)
{
  wxMargin margin;
  int newWindowWidth, newWindowHeight;
  wxWindow *p;

  margin = Margin(ParentWindow());
  newWindowWidth = width + margin.Offset(wxHorizontal);
  newWindowHeight = height + margin.Offset(wxVertical);

  p = ParentWindow();
  p->SetWidthHeight(newWindowWidth, newWindowHeight);
}

//-----------------------------------------------------------------------------
void wxArea::SetMargin(int margin, Direction direction)
{
  START_XFORM_SKIP;
  wxMargin newMargin(margin, direction);
  END_XFORM_SKIP;
  SetMargin(newMargin, direction);
}

//-----------------------------------------------------------------------------
void wxArea::SetMargin(wxMargin margin, Direction direction)
{
  int oldLeft, oldTop, oldRight, oldBottom, dL, dT, dR, dB, dW, dH, dX, dY;
  wxArea* area;

  oldLeft = cMargin.Offset(wxLeft);
  oldTop = cMargin.Offset(wxTop);
  oldRight = cMargin.Offset(wxRight);
  oldBottom = cMargin.Offset(wxBottom);

  cMargin.SetMargin(margin, direction);

  dL = cMargin.Offset(wxLeft) - oldLeft;
  dT = cMargin.Offset(wxTop) - oldTop;
  dR = cMargin.Offset(wxRight) - oldRight;
  dB = cMargin.Offset(wxBottom) - oldBottom;
  
  dW = -(dR + dL);
  dH = -(dB + dT);
  dX = dL;
  dY = dT;
  area = Next(); // Resize younger sibling areas
  while (area) {
    area->OnSiblingDSize(dW, dH, dX, dY);
    area = area->Next();
  }
}

//-----------------------------------------------------------------------------
void wxArea::SetMargin(wxMargin margin, Direction direction,
		       int parentWindowWidth, int parentWindowHeight,
		       int parentWindowX, int parentWindowY)
{
  // SetSize of parentWindow
  int oldWindowX, oldWindowY;
  int oldWindowWidth, oldWindowHeight;
  int newWindowX, newWindowY;
  int newWindowWidth, newWindowHeight, dW, dH, dX, dY;
  wxArea* area;
  int oldLeft, oldTop, oldRight, oldBottom, dL, dT, dR, dB;
  
  cParentWindow->GetPosition(&oldWindowX, &oldWindowY);
  oldWindowWidth = cParentWindow->Width();
  oldWindowHeight = cParentWindow->Height();

  cParentWindow->DoSetSize(parentWindowX, parentWindowY, parentWindowWidth, parentWindowHeight);

  cParentWindow->GetPosition(&newWindowX, &newWindowY);
  newWindowWidth = cParentWindow->Width();
  newWindowHeight = cParentWindow->Height();
  dW = newWindowWidth - oldWindowWidth;
  dH = newWindowHeight - oldWindowHeight;
  dX = newWindowX - oldWindowX;
  dY = newWindowY - oldWindowY;

  // Notify older siblings and this area of parentWindow resizing
  area = First();
  while (area && area != this) {
    area->OnSiblingDSize(dW, dH, dX, dY);
    area = area->Next();
  }
  if (area != this) wxFatalError("Error in wxArea::SetMargin");
  OnSiblingDSize(dW, dH, dX, dY);

  // SetMargin of this area
  oldLeft = cMargin.Offset(wxLeft);
  oldTop = cMargin.Offset(wxTop);
  oldRight = cMargin.Offset(wxRight);
  oldBottom = cMargin.Offset(wxBottom);

  cMargin.SetMargin(margin, direction);

  // Notify younger siblings of parentWindow resizing and margin changes for this area
  dL = cMargin.Offset(wxLeft) - oldLeft;
  dT = cMargin.Offset(wxTop) - oldTop;
  dR = cMargin.Offset(wxRight) - oldRight;
  dB = cMargin.Offset(wxBottom) - oldBottom;
  
  dW += -(dR + dL); // accumulate changes for parentWindow and this area
  dH += -(dB + dT); // accumulate changes for parentWindow and this area
  dX += dL; // accumulate changes for parentWindow and this area
  dY += dT; // accumulate changes for parentWindow and this area
  area = Next(); // Resize younger sibling areas
  while (area) {
    area->OnSiblingDSize(dW, dH, dX, dY);
    area = area->Next();
  }
}

//-----------------------------------------------------------------------------
void wxArea::OnSiblingDSize(int dW, int dH, int dX, int dY)
{
  // Must change W, H, X, Y of this area: but these are virtual variables.
  // Hence, change is effectively already done.

  OnAreaDSize(dW, dH, dX, dY);
}

//-----------------------------------------------------------------------------
// 
void wxArea::OnAreaDSize(int dW, int dH, int dX, int dY)
{
  if (dW || dH || dX || dY)
    {
      if (this == cParentWindow->ClientArea())
	{ // Notify parent window of client resize.
	  // The parent window manages the contents of the client area.
	  // Hence, must notify parent window that its client area has resized.
	  cParentWindow->OnClientAreaDSize(dW, dH, dX, dY);
	}
      else
	{ // Notify child windows of area resize.
	  wxChildNode* childWindowNode;
	  wxWindow* childWindow;
	  wxChildList *wl;
	  wl = Windows();
	  childWindowNode = wl->First();
	  while (childWindowNode) {
	    childWindow = (wxWindow*)childWindowNode->Data();
	    childWindow->OnAreaDSize(dW, dH, dX, dY);
	    childWindowNode = childWindowNode->Next();
	  }
	}
    }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Tree (windows and areas) methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxWindow* wxArea::ParentWindow(void) { return cParentWindow; }

//-----------------------------------------------------------------------------
wxChildList* wxArea::Windows(void) { return cWindows; } // kludge

//-----------------------------------------------------------------------------
wxArea* wxArea::First(void)
{
  wxArea* result = NULL;
  wxNode* nodeArea;
  wxWindow *p;
  wxList *al;

  p = ParentWindow();
  al = p->Areas();
  nodeArea = al->First();
  if (nodeArea) result = (wxArea*) nodeArea->Data();

  return result;
}

//-----------------------------------------------------------------------------
wxArea* wxArea::Previous(void)
{
  wxArea* result = NULL;
  wxNode* nodeArea;
  wxWindow *p;
  wxList *al;

  p = ParentWindow();
  al = p->Areas();
  nodeArea = al->Member(this);
  nodeArea = nodeArea->Previous();
  if (nodeArea) result = (wxArea*) nodeArea->Data();

  return result;
}

//-----------------------------------------------------------------------------
wxArea* wxArea::Next(void)
{
  wxArea* result = NULL;
  wxNode* nodeArea;
  wxWindow *p;
  wxList *al;

  p = ParentWindow();
  al = p->Areas();
  nodeArea = al->Member(this);
  nodeArea = nodeArea->Next();
  if (nodeArea) result = (wxArea*) nodeArea->Data();

  return result;
}

//-----------------------------------------------------------------------------
void wxArea::OnDeleteChildWindow(wxWindow* childWindow)
{
  cWindows->DeleteObject(childWindow);
}
