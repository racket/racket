///////////////////////////////////////////////////////////////////////////////
// File:	wx_win.cc
// Purpose:	wxWindow class implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wx_win.h"
#include "wxMacDC.h"
#include "wx_utils.h"
#include "wx_frame.h"
#include "wx_mac_utils.h"
#include "wx_gdi.h"
#include "wx_area.h"
#include "wx_stdev.h"
#include "wx_screen.h"
#include "wxScroll.h"
#include "wx_panel.h"
#include "wx_dialg.h"
#include "wx_dccan.h"
#include "wx_main.h"
#include "wx_menu.h"
#include "wx_rbox.h"
#include "wxTimeScale.h"
#include "wx_macevents.h"
#ifndef WX_CARBON
# include <QuickDraw.h>
#endif

// these offsets are used to eliminate calls to the real SetOrigin
int SetOriginX = 0;
int SetOriginY = 0;

extern int wx_leave_all_input_alone;

/* The gMouseWindow declaration confises xform.ss. */
#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

wxWindow* wxWindow::gMouseWindow = NULL; 

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

static void RestoreNormalBackground(wxBrush *erase);
extern void MrEdQueueOnSize(wxWindow *wx_window);

static HIObjectClassRef paintControlClass = NULL;

int wx_activate_anyway;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Geometry methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

wxWindow::wxWindow(void)
{
  cWindowX = 0;
  cWindowY = 0;
  cWindowWidth = 0;
  cWindowHeight = 0;
  cStyle = 0;
  cScroll = NULL;
  cAreas = new WXGC_PTRS wxList(wxList::kDestroyData);
  children = new WXGC_PTRS wxChildList();
  cActive = TRUE;
  cEnable = TRUE;

  InitDefaults();

  cParentArea = NULL;

  window_parent = NULL;

  cMacDC = NULL;
  
  cGrandcursor = FALSE;

  cClientArea = new WXGC_PTRS wxArea(this);
}


//-----------------------------------------------------------------------------
wxWindow::wxWindow // Constructor (for screen window)
(
 char*		windowName,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style
 ) :
 wxbWindow (windowName)
{
  cStyle = style;
  children = new WXGC_PTRS wxChildList();
  cScroll = NULL;
  cWindowX = (x != wxDEFAULT_POSITION ? x : 0);
  cWindowY = (y != wxDEFAULT_POSITION ? y : 0);
  cWindowHeight = (height >= 0 ? height : 0);
  cWindowWidth = (width >= 0 ? width : 0);

  cAreas = new WXGC_PTRS wxList(wxList::kDestroyData);
  cActive = TRUE;
  cEnable = TRUE;
  
  InitDefaults();

  cParentArea = NULL;

  window_parent = NULL;

  cMacDC = NULL; // WCH: should set to screen grafPort ??

  cGrandcursor = FALSE;

  cClientArea = new WXGC_PTRS wxArea(this);
}

//-----------------------------------------------------------------------------
wxWindow::wxWindow // Constructor (given parentScreen; i.e., this is frame)
(
 char*		windowName,
 wxScreen*	parentScreen,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style
 ) :
 wxbWindow (windowName)
{
  wxChildList *wl, *cl;
  
  cStyle = style;
  children = new WXGC_PTRS wxChildList();
  cScroll = NULL;
  cWindowX = (x != wxDEFAULT_POSITION ? x : 0);
  cWindowY = (y != wxDEFAULT_POSITION ? y : 0);
  cWindowHeight = (height >= 0 ? height : 0);
  cWindowWidth = (width >= 0 ? width : 0);
  cAreas = new WXGC_PTRS wxList(wxList::kDestroyData);

  cActive = TRUE;
  cEnable = TRUE;
  
  if (!parentScreen) wxFatalError("No parent screen for constructing frame.");

  InitDefaults();

  cParentArea = parentScreen->ClientArea();
  /* See note on screen's list of children below */
  if (0) {
    wl = cParentArea->Windows();
    wl->Append(this);
  }

  window_parent = parentScreen;
  /* The screen doesn't realy have a use for a list of children,
     and keeping this list means that there's a GC path from
     any frame to any other frame (which is bad for GC accounting) */
  if (0) {
    window_parent->AddChild(this);
    
    // Frames are initially hidden!
    cl = window_parent->GetChildren();
    cl->Show(this, FALSE);
    wl->Show(this, FALSE);
  }

  cMacDC = NULL; // will set cMacDC later

  cGrandcursor = FALSE;

  cClientArea = new WXGC_PTRS wxArea(this);
}

//-----------------------------------------------------------------------------
wxWindow::wxWindow // Constructor (given parentArea)
(
 char*		windowName,
 wxArea*		parentArea,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style
 ) :
 wxbWindow (windowName)
{
  wxChildList *wl;

  cStyle = style;
  children = new WXGC_PTRS wxChildList();
  cScroll = NULL;
  cWindowHeight = (height >= 0 ? height : 0);
  cWindowWidth = (width >= 0 ? width : 0);
  cAreas = new WXGC_PTRS wxList(wxList::kDestroyData);

  cActive = TRUE;
  cEnable = TRUE;

  if (!parentArea) wxFatalError("No parent area for constructing window.");

  InitDefaults();

  cParentArea = parentArea;
  wl = cParentArea->Windows();
  wl->Append(this);

  window_parent = cParentArea->ParentWindow();

  cMacDC = window_parent->MacDC();

  cGrandcursor = FALSE;

  InitWindowPostion(x, y);
  window_parent->AddChild(this);

  cClientArea = new WXGC_PTRS wxArea(this);
}

//-----------------------------------------------------------------------------
wxWindow::wxWindow // Constructor (given parentWindow)
(
 char*		windowName,
 wxWindow*	parentWindow,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style
 ) :
 wxbWindow (windowName)
{
  wxChildList *wl;

  cStyle = style;
  children = new WXGC_PTRS wxChildList();
  cScroll = NULL;
  cWindowHeight = (height >= 0 ? height : 0);
  cWindowWidth = (width >= 0 ? width : 0);
  cAreas = new WXGC_PTRS wxList(wxList::kDestroyData);

  cActive = TRUE;
  cEnable = TRUE;

  if (!parentWindow) wxFatalError("No parent window for constructing window.");

  InitDefaults();

  cParentArea = parentWindow->ClientArea();
  wl = cParentArea->Windows();
  wl->Append(this);

  window_parent = parentWindow;

  cMacDC = window_parent->MacDC();
  
  cGrandcursor = FALSE;

  InitWindowPostion(x, y);
  window_parent->AddChild(this);

  cClientArea = new WXGC_PTRS wxArea(this);
}

//-----------------------------------------------------------------------------
wxWindow::wxWindow // Constructor (given objectType; i.e., menu or menuBar)
(
 char*		windowName
 ) :
 wxbWindow (windowName)
{
  cStyle = 0;
  children = new WXGC_PTRS wxChildList();
  cScroll = NULL;
  cWindowX = 0;
  cWindowY = 0;
  cWindowHeight = 0;
  cWindowWidth = 0;
  cAreas = new WXGC_PTRS wxList(wxList::kDestroyData);

  cActive = TRUE;
  cEnable = TRUE;

  InitDefaults();

  cParentArea = NULL;

  window_parent = NULL;

  cMacDC = NULL;
  
  cGrandcursor = FALSE;

  cClientArea = new WXGC_PTRS wxArea(this);
}

//=============================================================================
// Public destructor
//=============================================================================

static wxWindow *entered = NULL;

void wxRegisterEntered();

void wxRegisterEntered()
{
  wxREGGLOB(entered);
}

//-----------------------------------------------------------------------------
wxWindow::~wxWindow(void) // Destructor
{
  
  wxPanel *panel;
  panel = (wxPanel *) GetParent ();
  if (panel)
    {
      // parent is not always a wxPanel: can be a wxMenu...
      if (panel->__type == wxTYPE_PANEL)
	{
	  //if (this == parent->last_created)
	  //	parent->last_created = NULL; cjc
	  // tom fettig suggests:
	  panel->new_line = FALSE;
	  panel->label_position = wxHORIZONTAL;
	  panel->hSpacing = PANEL_HSPACING;
	  panel->vSpacing = PANEL_VSPACING;
	  panel->initial_hspacing = panel->hSpacing;
          panel->initial_vspacing = panel->vSpacing;
          panel->current_hspacing = panel->hSpacing;
          panel->current_vspacing = panel->vSpacing;
          panel->OnDeleteChild(this);
	}
    }

  if (cParentArea) 
    cParentArea->OnDeleteChildWindow(this);

  if (window_parent) window_parent->OnDeleteChildWindow(this);

  DestroyChildren();

  if (refcon) {
    DequeueMrEdEvents(leaveEvt, (long)refcon);
#ifdef MZ_PRECISE_GC
    FREE_SAFEREF(refcon);
#endif
    refcon = NULL;
  }

  DELETE_OBJ children;
  if (cScroll)
    DELETE_OBJ cScroll;

  DELETE_OBJ cAreas;
  
  // don't send leaveEvt messages to this window anymore.
  if (entered == this) entered = NULL; 

  if (cPaintControl) {
    void *rc;

    rc = (void *)GetControlReference(cPaintControl);
    FREE_SAFEREF(rc);
    DisposeControl(cPaintControl);
    cPaintControl = NULL;
  }

  /* Must zero out fieldsd that are WXGC_IGNOREd: */
  cParentArea = NULL;
  cMacDC = NULL;
  window_parent = NULL;
}

//=============================================================================
// Private methods
//=============================================================================

//-----------------------------------------------------------------------------
void wxWindow::InitDefaults(void)
{
  cGravitate = 0;
  cJustify = 0;
  cBrush = NULL;
  cEraser = NULL;

  cColour = TRUE; // WCH: must redo this

  cHidden = cUserHidden = FALSE;
  
  WXGC_IGNORE(this, window_parent);
  WXGC_IGNORE(this, cParentArea);
  WXGC_IGNORE(this, cMacDC);
}

//-----------------------------------------------------------------------------
void wxWindow::InitWindowPostion(int x, int y)
{
  if (wxSubType(window_parent->__type, wxTYPE_PANEL) &&
      cParentArea == window_parent->ClientArea())
    {
      wxPanel* parentPanel;
      int cursorX, cursorY;
      parentPanel = (wxPanel*) window_parent;
      parentPanel->GetCursor(&cursorX, &cursorY);
      cWindowX = (x != wxDEFAULT_POSITION ? x : cursorX);
      cWindowY = (y != wxDEFAULT_POSITION ? y : cursorY);
    }
  else
    {
      cWindowX = (x != wxDEFAULT_POSITION ? x : 0);
      cWindowY = (y != wxDEFAULT_POSITION ? y : 0);
    }
}

//-----------------------------------------------------------------------------

const ControlPartCode kControlOpaqueRegionMetaPart = -3;

static OSStatus paintControlHandler(EventHandlerCallRef inCallRef,
				    EventRef inEvent,
				    void *data)
{
  OSStatus err = noErr;
  UInt32 eventClass, eventKind;

  eventClass = GetEventClass(inEvent);
  eventKind = GetEventKind(inEvent);

  switch (eventClass)  {
  case kEventClassHIObject:
    switch (eventKind) {
    case kEventHIObjectConstruct:
      {
	/* Is this really necessary? */
	data = malloc(sizeof(ControlRef));
	GetEventParameter(inEvent, kEventParamHIObjectInstance,
			  typeHIObjectRef, NULL, sizeof(HIObjectRef),
			  NULL, data);
	SetEventParameter(inEvent, kEventParamHIObjectInstance,
			  typeVoidPtr, sizeof(ControlRef*), 
			  &data);
      }
      break;
    case kEventHIObjectInitialize:
      {
	Rect bounds;

	err = CallNextEventHandler(inCallRef, inEvent);

	GetEventParameter(inEvent, 'Boun', typeQDRectangle,
			  NULL, sizeof(Rect), NULL, &bounds);
	SetControlBounds(*(ControlRef*)data, &bounds);
      }
      break;
    case kEventHIObjectDestruct:
      free(data);
      break;
    }
    break;
  case kEventClassControl:
    {
      switch (eventKind) {
      case kEventControlInitialize:
	break;
      case kEventControlDraw:
	{
	  wxWindow *wx_window;
	  ThemeDrawingState s;
	  CGrafPtr savep;
	  GDHandle savegd;
	  ControlRef controlRef;
	  void *rc;

	  controlRef = *(ControlRef*)data;
	  rc = (void *)GetControlReference(controlRef);
	  wx_window = (wxWindow *)GET_SAFEREF(rc);
	  
	  if (wx_window) {
	    RgnHandle clipRgn;
	    wxMacDC *mdc;
            int need_reset_user = 0;
          
	    GetGWorld(&savep, &savegd);
	    GetThemeDrawingState(&s);
            
	    clipRgn = NewRgn();
	    if (clipRgn)
	      GetClip(clipRgn);

	    /* In case the grafport properties have changed: */
	    mdc = wx_window->MacDC();
	    mdc->setCurrentUser(NULL);

	    wx_window->SetCurrentDC();
            ::TextMode(srcOr); /* for drawing labels */

            if (clipRgn && (wx_window->__type == wxTYPE_MESSAGE)) {
              /* Need proper clipping for messages, since we draw 
                 manually and transparently. */
              RgnHandle r2;
              r2 = NewRgn();
              if (r2) {
                CopyRgn(clipRgn, r2);
                OffsetRgn(r2, SetOriginX, SetOriginY);
                SetClip(r2);
                DisposeRgn(r2);
                need_reset_user = 1;
              }
            }
#if 0
	    {
	      Rect bounds;
	      int x, y;
	      GetControlBounds(controlRef, &bounds);
	      FrameRect(&bounds);
	    }
#endif
	    wx_window->PaintRgn(clipRgn);

            if (need_reset_user)
              mdc->setCurrentUser(NULL);

	    SetGWorld(savep, savegd);
	    SetThemeDrawingState(s, TRUE);
	    if (clipRgn) {
	      SetClip(clipRgn);
	      DisposeRgn(clipRgn);
	    }
	  }
	}
	break;
      case kEventControlHitTest:
	{
	  ControlPartCode part = 1;
	  SetEventParameter(inEvent, kEventParamControlPart, typeControlPartCode, sizeof(part), &part);
	}
	break;
      case kEventControlGetPartRegion:
	{
	  RgnHandle hrgn;
	  ControlRef controlRef;
	  wxWindow *wx_window;
	  void *rc;
	  ControlPartCode part;

	  GetEventParameter(inEvent, kEventParamControlPart, typeControlPartCode,
			    NULL, sizeof(ControlPartCode), NULL, &part);

	  if (part == kControlContentMetaPart
	      || part == kControlStructureMetaPart
	      || part == kControlOpaqueRegionMetaPart) {
	    controlRef = *(ControlRef*)data;
	    rc = (void *)GetControlReference(controlRef);
	    wx_window = (wxWindow *)GET_SAFEREF(rc);
	    
	    if (wx_window) {
	      GetEventParameter(inEvent, kEventParamControlRegion, typeQDRgnHandle, NULL,
			      sizeof(hrgn), NULL, &hrgn);
	      wx_window->GetPaintControlRegion(hrgn, part == kControlOpaqueRegionMetaPart);
	    }
	  }
	}
	break;
      }
    }
    break;
  }
 
  return err;
}

void wxWindow::CreatePaintControl(int inset, Bool opaque) 
{
  ControlHandle pane;
  Rect boundsRect;
  EventRef constructData;
  OSStatus err;
  int ox, oy;

  control_inset_extent = inset;
  control_opaque = opaque;
  
  if (!paintControlClass) {
    GC_CAN_IGNORE EventTypeSpec eventList[] = {
      { kEventClassHIObject, kEventHIObjectConstruct },
      { kEventClassHIObject, kEventHIObjectInitialize },
      { kEventClassHIObject, kEventHIObjectDestruct },
      { kEventClassControl, kEventControlInitialize },
      { kEventClassControl, kEventControlDraw },
      { kEventClassControl, kEventControlHitTest },
      { kEventClassControl, kEventControlGetPartRegion } };
    
    HIObjectRegisterSubclass(CFSTR("org.racket-lang.GRacketPaintControl"),
			     kHIViewClassID, // base class ID
			     NULL, // option bits
			     paintControlHandler,
			     GetEventTypeCount(eventList),
			     eventList,
			     NULL, // construct data,
			     &paintControlClass);
  }
   
  boundsRect.top = boundsRect.left = 0;
  boundsRect.right = cWindowWidth;
  boundsRect.bottom = cWindowHeight;
  
  GetWinOrigin(&ox, &oy);
  OffsetRect(&boundsRect, ox, oy);    

  CreateEvent(NULL, kEventClassHIObject, kEventHIObjectInitialize,
	      GetCurrentEventTime(), 0, &constructData);

  SetEventParameter(constructData, 'Boun', typeQDRectangle,
		    sizeof(Rect), &boundsRect);

  err = HIObjectCreate(CFSTR("org.racket-lang.GRacketPaintControl"),
		       constructData,
		       (HIObjectRef *)&pane);
  cPaintControl = pane;

  ReleaseEvent(constructData);
  
  ::HIViewAddSubview(GetRootControl(), cPaintControl);
  
  {
    void *rc;
    rc = WRAP_SAFEREF(this);
    SetControlReference(cPaintControl, (long)rc);
  }

  ::ShowControl(cPaintControl);
}

void wxWindow::GetPaintControlRegion(RgnHandle hrgn, Bool opaquePart)
{
  Rect bounds;
 
  if (!opaquePart || control_opaque) {
    GetControlBounds(cPaintControl, &bounds);
    bounds.right -= bounds.left;
    bounds.bottom -= bounds.top;
    bounds.left = bounds.top = 0;
    RectRgn(hrgn, &bounds); 

    if (control_inset_extent > 0) {
      RgnHandle r;
      InsetRect(&bounds, control_inset_extent, control_inset_extent);
      r = NewRgn();
      if (r) {
	RectRgn(r, &bounds); 
	DiffRgn(hrgn, r, hrgn);
	DisposeRgn(r);
      }
    }
  }
}

//-----------------------------------------------------------------------------
int wxWindow::Width(void) { return cWindowWidth; } // mac platform only

//-----------------------------------------------------------------------------
int wxWindow::Height(void) { return cWindowHeight; } // mac platform only

//-----------------------------------------------------------------------------
wxMargin wxWindow::Margin(wxArea* outerArea) // mac platform only
{
  wxMargin result;
  wxArea* parentArea;
  parentArea = ParentArea();
  if (parentArea) {
    int w, h;
    result.SetMargin(cWindowX, wxLeft);
    result.SetMargin(cWindowY, wxTop);
    w = parentArea->Width();
    result.SetMargin(w - cWindowX - cWindowWidth, wxRight);
    h = parentArea->Height();
    result.SetMargin(h - cWindowY - cWindowHeight, wxBottom);
    
    if (parentArea != outerArea) {
      result += parentArea->Margin(outerArea);
    }
  }

  return result;
}

//-----------------------------------------------------------------------------
wxMargin wxWindow::Margin(wxWindow* outerWindow) // mac platform only
{
  wxMargin result;

  if (outerWindow != this)
    {
      wxArea* parentArea;
      parentArea = ParentArea();
      if (parentArea)
	{
	  result =  Margin(parentArea);
	  result += parentArea->Margin(outerWindow);
	}
    }

  return result;
}

//-----------------------------------------------------------------------------
void wxWindow::GetPosition(int* windowX, int* windowY)
{ // Get window position w.r.t. parent area origin
  *windowX = cWindowX;
  *windowY = cWindowY;
}

//-----------------------------------------------------------------------------
void wxWindow::GetSize(int* width, int* height)
{ // Get window size
  *width = cWindowWidth;
  *height = cWindowHeight;
}

//-----------------------------------------------------------------------------
// Get size *available for subwindows* i.e. excluding menu bar etc.
//-----------------------------------------------------------------------------
void wxWindow::GetClientSize(int* width, int* height)
{
  int n;
  n = cClientArea->Width();
  *width = n;
  n = cClientArea->Height();
  *height = n;
}

//-----------------------------------------------------------------------------
void wxWindow::ClientToScreen(int* x, int* y)
{
  cClientArea->AreaToScreen(x, y);
}

//-----------------------------------------------------------------------------
void wxWindow::ScreenToClient(int* x, int* y)
{
  cClientArea->ScreenToArea(x, y);
}

//-----------------------------------------------------------------------------
void wxWindow::ClientToLogical(int* x, int* y) // mac platform only; testing
{ // Transform point from client c.s. to logical c.s. (virtual canvas, scrolling)
  // default action leaves *x and *y unchanged (logical c.s. same as client c.s.)
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Sizing methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxWindow::SetWidthHeight(int width, int height) // mac platform only
{
  SetSize(cWindowX, cWindowY, width, height);
}

void wxWindow::SetPhantomSize(int w, int h)
{
  /* Do nothing */
}

//-----------------------------------------------------------------------------
void wxWindow::SetSize(int x, int y, int width, int height, int flags) // mac platform only
{
  int oldWindowX = cWindowX;
  int oldWindowY = cWindowY;
  int oldWindowWidth = cWindowWidth;
  int oldWindowHeight = cWindowHeight;

  if (width == -1)
    width = oldWindowWidth;
  if (height == -1)
    height = oldWindowHeight;
  
  if (!(flags & wxPOS_USE_MINUS_ONE)) {
    if (x == -1)
      x = oldWindowX;
    if (y == -1)
      y = oldWindowY;
  }

  DoSetSize(x, y, width, height);

  {
    int dW = cWindowWidth - oldWindowWidth;
    int dH = cWindowHeight - oldWindowHeight;
    int dX = cWindowX - oldWindowX;
    int dY = cWindowY - oldWindowY;

    OnWindowDSize(dW, dH, dX, dY);
  }
}

//-----------------------------------------------------------------------------
void wxWindow::DoSetSize(int x, int y, int width, int height) // mac platform only
{
  cWindowX = x;
  cWindowY = y;
  cWindowWidth = width;
  cWindowHeight = height;
}

void wxWindow::Refresh(void)
{
  if (cHidden) return;

  if (cPaintControl)
    HIViewSetNeedsDisplay(cPaintControl, TRUE);
  if (cMacControl)
    HIViewSetNeedsDisplay(cMacControl, TRUE);
}

void wxWindow::RefreshIfUpdating(void)
{
}

//-----------------------------------------------------------------------------
void wxWindow::SetClientSize(int newClientWidth, int newClientHeight)
{
  cClientArea->SetSize(newClientWidth, newClientHeight);
}

//-----------------------------------------------------------------------------
void wxWindow::GravitateJustify(Direction gravitate, Direction justify,
				int left, int top, int right, int bottom)
{
  int windowX = cWindowX;
  int windowY = cWindowY;
  int windowWidth = cWindowWidth;
  int windowHeight = cWindowHeight;

  // do gravitate
  if (gravitate & wxLeft && gravitate & wxRight)
    windowX = (left + right - cWindowWidth) / 2;
  else if (gravitate & wxLeft) windowX = left;
  else if (gravitate & wxRight) windowX = right - cWindowWidth;

  if (gravitate & wxTop && gravitate & wxBottom)
    windowY = (top + bottom - cWindowHeight) / 2;
  else if (gravitate &  wxTop) windowY = top;
  else if (gravitate & wxBottom) windowY = bottom - cWindowHeight;

  // do justify
  if (justify & wxLeft)
    {
      windowWidth += windowX - left;
      windowX = left;
    }

  if (justify & wxTop)
    {
      windowHeight += windowY - top;
      windowY = top;
    }

  if (justify & wxBottom) windowHeight = bottom - windowY;

  if (justify & wxRight) windowWidth = right - windowX;

  // do size
  SetSize(windowX, windowY, windowWidth, windowHeight, wxPOS_USE_MINUS_ONE);
}

//-----------------------------------------------------------------------------
void wxWindow::Fit(void)
{ // Resize window to fit exactly around all its client children
  int maxX = 0;
  int maxY = 0;
  wxChildNode* childWindowNode;
  wxWindow* childWindow;
  wxArea *carea;
  wxChildList *wl;

  carea = ClientArea();
  wl = carea->Windows();
  childWindowNode = wl->First();
  while (childWindowNode)
    {
      int x, y, w, h;

      childWindow = (wxWindow*)childWindowNode->Data();
      childWindow->GetPosition(&x, &y);
      childWindow->GetSize(&w, &h);
      if ((x + w) > maxX) maxX = x + w;
      if ((y + h) > maxY) maxY = y + h;
      childWindowNode = childWindowNode->Next();
    }

  SetClientSize(maxX, maxY);
}

//-----------------------------------------------------------------------------
void wxWindow::OnWindowDSize(int dW, int dH, int dX, int dY)
{ 
  wxNode* areaNode;
  wxArea* area;

  OnClientAreaDSize(dW, dH, dX, dY);

  // Resize child areas
  areaNode = cAreas->First();
  while (areaNode) {
    area = (wxArea*)areaNode->Data();
    if (area == ClientArea()) {
      /* done */
    } else {
      area->OnSiblingDSize(dW, dH, dX, dY);
    }
    areaNode = areaNode->Next();
  }
  
  if ((dW || dH) && (__type != wxTYPE_FRAME)) {
    // MrEdQueueOnSize(this);
    OnSize(-1, -1);
  }
}

//-----------------------------------------------------------------------------
void wxWindow::OnAreaDSize(int dW, int dH, int dX, int dY)
{
  if (cGravitate || cJustify) {
    int left = cWindowX;
    int top = cWindowY;
    int right = cWindowX + cWindowWidth + dW;
    int bottom = cWindowY + cWindowHeight + dH;
    GravitateJustify(cGravitate, cJustify, left, top, right, bottom);
  } else {
    OnWindowDSize(dW, dH, dX, dY);
  }
}

//-----------------------------------------------------------------------------
void wxWindow::OnClientAreaDSize(int dW, int dH, int dX, int dY)
{
  if (__type == wxTYPE_FRAME) {
    ::SizeControl(cMacControl, cWindowWidth, cWindowHeight);
  }

  if (cPaintControl) {
    if (dW || dH) {
      ::SizeControl(cPaintControl, cWindowWidth, cWindowHeight);
    }
    if (dX || dY) {
      int x, y;
      GetWinOrigin(&x, &y);
      MoveControl(cPaintControl, x, y);
    }
  }

  { // Notify child windows of area resize.
    wxChildNode* childWindowNode;
    wxWindow* childWindow;
    wxArea *carea;
    wxChildList *wl;

    carea = ClientArea();
    wl = carea->Windows();
    childWindowNode = wl->First();
    while (childWindowNode) {
      childWindow = (wxWindow*)childWindowNode->Data();
      if (childWindow) {
	childWindow->OnAreaDSize(dW, dH, dX, dY);
	childWindowNode = childWindowNode->Next();
      }
    }
  }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Device context methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxMacDC* wxWindow::MacDC(void) { return cMacDC; } // mac platform only

#define wxIsWindowVisible(x) 1

//-----------------------------------------------------------------------------
int wxWindow::SetCurrentMacDCNoMargin(void) // mac platform only
{
  /* Not used anymore */
  return 0;
}

void wxWindow::GetWinOrigin(int *x, int *y)
{
  if (cParentArea && !wxSubType(__type, wxTYPE_FRAME)) {
    cParentArea->FrameContentAreaOffset(x, y);
    *x += cWindowX;
    *y += cWindowY;
  } else {
    *x = *y = 0;
  }
}

//-----------------------------------------------------------------------------
int wxWindow::SetCurrentMacDC(void) // mac platform only
{
  CGrafPtr theMacGrafPort;

  theMacGrafPort = cMacDC->macGrafPort();

  ::SetPort(theMacGrafPort);

  cClientArea->FrameContentAreaOffset(&SetOriginX, &SetOriginY);

  return 1;
}

//-----------------------------------------------------------------------------
int wxWindow::SetCurrentDC(void) // mac platform only
{
  int vis;
  CGrafPtr theMacGrafPort;

  theMacGrafPort = cMacDC->macGrafPort();

  vis = wxIsWindowVisible(GetWindowFromPort(theMacGrafPort));

  ::SetPort(theMacGrafPort);

  cClientArea->FrameContentAreaOffset(&SetOriginX, &SetOriginY);
  
  if (cMacDC->currentUser() != this) { // must setup platform
    Rect theClipRect;
    cMacDC->setCurrentUser(this);
    if (cHidden) {
      theClipRect.top = theClipRect.bottom = 0;
      theClipRect.left = theClipRect.right = 0;
    } else {
      GetClipRect(cClientArea, &theClipRect);
      OffsetRect(&theClipRect,SetOriginX,SetOriginY);
    }

    ::ClipRect(&theClipRect);
    MacSetBackground();
    SetForeground();
    
    PenMode(patCopy);
    SetTextInfo();
  }

  return vis;
}

void wxWindow::ReleaseCurrentDC(int really)
{
  /* This method is here for windows that use a white background, so
     that the background color can be reset.  Currently, that means
     wxListBox only. */
  if (really) {
    if (cMacDC->currentUser() == this) {
      CGrafPtr theMacGrafPort;
      theMacGrafPort = cMacDC->macGrafPort();
      ::SetPort(theMacGrafPort);
      cMacDC->setCurrentUser(NULL);

      RestoreNormalBackground(wxWHITE_BRUSH);
    }
  }
}

static void RestoreNormalBackground(wxBrush *eraser)
{
  if (eraser == wxWHITE_BRUSH) {
    int depth;
    depth = wxDisplayDepth();
    SetThemeBackground(kThemeBrushDialogBackgroundActive, depth, depth > 1);
  }
}

RgnHandle wxWindow::GetCoveredRegion(int x, int y, int w, int h)
{
  return NULL;
}

long wxWindow::GetWindowHandle()
{
  if (cMacControl)
    return (long)cMacControl;
  else if (cPaintControl)
    return (long)cPaintControl;
  else
    return 0;
}

//-----------------------------------------------------------------------------
void wxWindow::MacSetBackground(void) // mac platform only
{
  BackColor(whiteColor);
  BackPat(GetWhitePattern());

  if (cEraser != wxWHITE_BRUSH) {
    int depth;
    depth = wxDisplayDepth();
    SetThemeBackground(kThemeBrushDialogBackgroundActive, depth, depth > 1);
  }
}

//-----------------------------------------------------------------------------
void wxWindow::SetForeground(void) // mac platform only
{
  int theBrushStyle;
  RGBColor pixel;

  if (IsGray()) {
    RGBColor c;
    c.red = c.green = c.blue = 0x7FFF;
    RGBForeColor(&c);
    return;
  }

  if (!cBrush)
    {
      PenPat(GetBlackPattern());
      ForeColor(blackColor);
      return;
    }

  theBrushStyle = cBrush->GetStyle();
  if (theBrushStyle == wxSOLID)
    PenPat(GetBlackPattern());
  else if (theBrushStyle == wxTRANSPARENT)
    PenPat(GetWhitePattern()); // WCH: does this work??
  else if (IS_HATCH(theBrushStyle)) {
    macGetHatchPattern(theBrushStyle, &cMacPattern);
    PenPat(&cMacPattern);
  } else {
    PenPat(GetBlackPattern()); // WCH: must use PenPixPat for stipple
  }
  
  {
    wxColour *c;
    c = cBrush->GetColour();
    pixel = c->pixel;
  }
  if (cColour)
    RGBForeColor(&pixel);
  else {
    unsigned char red, blue, green;
    Bool isWhiteColour;
    wxColour *c;

    c = cBrush->GetColour();
    red = c->Red();
    blue = c->Blue();
    green = c->Green();
    isWhiteColour =
      (red == (unsigned char )255 &&
       blue == (unsigned char)255 &&
       green == (unsigned char)255);
    ForeColor(isWhiteColour ? whiteColor : blackColor);
  }
}

//-----------------------------------------------------------------------------
void wxWindow::SetTextInfo(void) // mac platform only
{
  if (!font) {
    font = wxNORMAL_FONT;
    if (!font) {
      ::TextFont(1);
      ::TextSize(12);
      ::TextFace(0);
      return;
    }
  }

  ::TextFont(font->GetMacFontNum());
  ::TextSize(font->GetPointSize());
  ::TextFace(font->GetMacFontStyle());
}

//-----------------------------------------------------------------------------
void wxWindow::GetClipRect(wxArea* area, Rect* clipRect)
{
  // get clipRect in area c.s.
  ::SetRect(clipRect, 0, 0, area->Width(), area->Height()); // area c.s.

  // Clipping to the parent area should be unnecessary.
  // Try disabling this sometime...  [2007-MAR-07]
  if (ParentArea()) {
    wxWindow* windowParent;
    Rect parentClipRect;
    int parentAreaX, parentAreaY;
    wxArea *parea;

    parea = ParentArea();
    windowParent = parea->ParentWindow();

    /* We don't want to clip to the screen anymore, because windows
       are buffered. Also, the "screen" includes only the main screen. */
    if (windowParent != wxScreen::gScreenWindow) {
      /* Ok, parent isn't the screen, so clip to parent: */
      wxMargin parentAreaMargin;

      windowParent->GetClipRect(cParentArea, &parentClipRect);
      parentAreaMargin = area->Margin(cParentArea);
      parentAreaX = parentAreaMargin.Offset(wxLeft);
      parentAreaY = parentAreaMargin.Offset(wxTop);
      ::OffsetRect(&parentClipRect, -parentAreaX, -parentAreaY); // area c.s.
      ::SectRect(&parentClipRect, clipRect, clipRect);
      if (clipRect->top < 0) clipRect->top = 0;
      if (clipRect->left < 0) clipRect->left = 0;
    }
  }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Tree methods (for windows and areas)
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxArea* wxWindow::ParentArea(void) { return cParentArea; } // mac platform only

//-----------------------------------------------------------------------------
wxList* wxWindow::Areas(void) { return cAreas; } // mac platform only (kludge)

//-----------------------------------------------------------------------------
wxArea* wxWindow::ClientArea(void) { return cClientArea; } // mac platform only

//-----------------------------------------------------------------------------
wxWindow* wxWindow::GetParent(void) { return window_parent; }

//-----------------------------------------------------------------------------
wxChildList* wxWindow::GetChildren(void) { return children; }

//-----------------------------------------------------------------------------
wxWindow* wxWindow::GetGrandParent(void)
{
  if (window_parent)
    return window_parent->window_parent;
  else return NULL;
}

//-----------------------------------------------------------------------------
/* There are two kinds of root Frames - real root Frames and DialogBox's
   which are 
   */
wxFrame* wxWindow::GetRootFrame(void) // mac platform only
{
  return (wxFrame*)GetParent()->GetRootFrame();
}

ControlHandle wxWindow::GetRootControl(void)
{
  return GetParent()->GetRootControl();
}

//-----------------------------------------------------------------------------
void wxWindow::AddChild(wxObject* child) { children->Append(child); }

//-----------------------------------------------------------------------------
void wxWindow::OnDeleteChildWindow(wxWindow* childWindow) // mac platform only
{
  if (children) children->DeleteObject(childWindow);
}

//-----------------------------------------------------------------------------
void wxWindow::OnDeleteChildArea(wxArea* childArea) // mac platform only
{
  cAreas->OnDeleteObject(childArea);
}

//-----------------------------------------------------------------------------
void wxWindow::DestroyChildren(void)
{
  if (children)
    {
      wxWindow* child;
      wxChildNode* node;
      node = children->First();
      while (node)
	{
	  child = (wxWindow*)node->Data();
	  DELETE_OBJ child; // this will also delete current node
	  node = children->First(); // must do since current node was deleted
	}
    }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Scroll methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxWindow::AddChildScrollWindow(wxWindow* childScrollWindow) // mac platform only
{
  wxScroll* scroll;
  scroll = GetScroll();
  if (!scroll) wxFatalError("No scroll for AddChildScrollWindow.");
  scroll->AddChildScrollWindow(childScrollWindow);
}

//-----------------------------------------------------------------------------
wxScroll* wxWindow::GetScroll(void) { return cScroll; }

//-----------------------------------------------------------------------------
void wxWindow::SetScrollData // Must override if window scrolls
(
 wxScrollData*		scrollData,
 wxWhatScrollData	whatScrollData,
 wxScrollEvent*		e
 )
{
  // Must override if window scrolls
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Mouse methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxWindow::CaptureMouse(void)
     /* Allows events only to this window and its children */
     /* I.e., like X-Windows, not like Windows */
{
  if (gMouseWindow != this) {
    if (gMouseWindow) gMouseWindow->ReleaseMouse();
    gMouseWindow = this;
  }
}

//-----------------------------------------------------------------------------
void wxWindow::ReleaseMouse(void)
{
  if (gMouseWindow == this) {
    gMouseWindow = NULL;
  }
}

//-----------------------------------------------------------------------------
/* Forward decl */
Bool doCallPreMouseEvent(wxWindow *in_win, wxWindow *win, wxMouseEvent *evt);

static void SendEnterLeaveEvent(wxWindow *target, int eventtype, wxWindow *evtsrc, wxMouseEvent *evt)
{
  if (!target->IsHidden()) {
    int clientHitX, clientHitY;
    wxMouseEvent *theMouseEvent;

    theMouseEvent = new WXGC_PTRS wxMouseEvent(eventtype);
    theMouseEvent->leftDown = evt->leftDown;
    theMouseEvent->middleDown = evt->middleDown;
    theMouseEvent->rightDown = evt->rightDown;
    theMouseEvent->shiftDown = evt->shiftDown;
    theMouseEvent->controlDown = evt->controlDown;
    theMouseEvent->altDown = evt->altDown;
    theMouseEvent->metaDown = evt->metaDown;
    theMouseEvent->timeStamp = evt->timeStamp;
    
    clientHitX = (int)(evt->x);
    clientHitY = (int)(evt->y);
    evtsrc->ClientToScreen(&clientHitX, &clientHitY);
    target->ScreenToClient(&clientHitX, &clientHitY);
    theMouseEvent->x = clientHitX;
    theMouseEvent->y = clientHitY;

    if (!doCallPreMouseEvent(target, target, theMouseEvent))		  
      if (!target->IsGray())
	target->OnEvent(theMouseEvent);
  }
}

static void QueueLeaveEvent(wxWindow *target, wxWindow *evtsrc, wxMouseEvent *evt)
{
  EventRecord e;
  int clientHitX = (int)(evt->x);
  int clientHitY = (int)(evt->y);

  if (!target->refcon) {
    void *rc;
    rc = WRAP_SAFEREF(target);
    target->refcon = rc;
  }

  evtsrc->ClientToScreen(&clientHitX, &clientHitY);
  target->ScreenToClient(&clientHitX, &clientHitY);
  e.message = (long)target->refcon;
  e.where.h = clientHitX;
  e.where.v = clientHitY;
  e.what = leaveEvt;
  e.when = UNSCALE_TIMESTAMP(evt->timeStamp);
  e.modifiers = ((evt->shiftDown ? shiftKey : 0)
		 + (evt->metaDown ? cmdKey : 0)
		 + (evt->altDown ? optionKey : 0)
		 + ((evt->leftDown || evt->rightDown) ? btnState : 0)
		 + (evt->rightDown ? controlKey : 0));
  
  QueueMrEdEvent(&e);
}

Bool doCallPreMouseEvent(wxWindow *in_win, wxWindow *_win, wxMouseEvent *evt)
{
  if (_win == in_win 
      && (evt->eventType != wxEVENT_TYPE_ENTER_WINDOW)
      && (evt->eventType != wxEVENT_TYPE_LEAVE_WINDOW)) {
    wxWindow *win;

    win = _win->EnterLeaveTarget();

    /* Do enter/leave events */
    if (entered != win) {
      wxWindow *p;
      if (entered) {
	/* Leave in current eventspace? */
	int same;

	same = (((wxFrame *)win->GetRootFrame())->context
		== ((wxFrame *)entered->GetRootFrame())->context);
	
	/* Send/queue leave events to non-common ancestors */
	p = entered;
	while (p) {
	  wxWindow *winp = win;
	  // is p an ancestor of win? If so, break.
	  while (winp && (winp != p)) {
	    winp = winp->GetParent();
	  }
	  if (winp == p)
	    break;
	  
	  if (same)
	    SendEnterLeaveEvent(p, wxEVENT_TYPE_LEAVE_WINDOW, win, evt);
	  else
	    QueueLeaveEvent(p, win, evt);
	  p = p->GetParent();
	}
      } else {
	p = win->GetRootFrame();
	p = p->GetParent();
      }
      
      entered = win;
      
      while (1) {
	wxWindow *winp = win;
	while (winp && (winp->GetParent() != p)) {
	  winp = winp->GetParent();
	}
	if (!winp)
	  break;
	SendEnterLeaveEvent(winp, wxEVENT_TYPE_ENTER_WINDOW, win, evt);
	if (winp == win)
	  break;
	p = winp;
      }
    }
  }

  {
    wxWindow *p;
    p = _win->GetParent();
    return ((p && doCallPreMouseEvent(in_win, p, evt)) || _win->PreOnEvent(in_win, evt));
  }
}

static Bool IsCaptureAncestorArea(wxArea *area)
{
  wxChildNode* childWindowNode;
  wxChildList *wl;
  wxWindow *p, *w;

  wl = area->Windows();
  childWindowNode = wl->First();
  while (childWindowNode) { 
    p = (wxWindow*)childWindowNode->Data();
    for (w = wxWindow::gMouseWindow; w; w = w->GetParent()) {
      if (w == p)
	return TRUE;
    }
    
    childWindowNode = childWindowNode->Next();
  }
  
  return FALSE;
}

Bool wxWindow::SeekMouseEventArea(wxMouseEvent *mouseEvent, int *metal_drag_ok)
{ 
  // For point expressed in parent area c.s., seek deepest sub-window containing it
  Bool result = FALSE;
  int hitX, hitY;
  int capThis;
  wxArea* hitArea;
  wxNode* areaNode;

  if (!IsEnable()) {
    if (!*metal_drag_ok) {
      return FALSE;
    } else {
      /* If the frame is metal, we need to look more closely
	 to determine whether a disabled control was clicked, in which case
	 we don't want to drag the frame. */
      wxFrame *f;
      f = GetRootFrame();
      if (!(f->GetWindowStyleFlag() & wxMETAL)) {
	return FALSE;
      }
      *metal_drag_ok = -1;
    }
  }

  hitX = (int)(mouseEvent->x - cWindowX); // window c.s.
  hitY = (int)(mouseEvent->y - cWindowY); // window c.s.

  capThis = (wxWindow::gMouseWindow == this);
  hitArea = NULL;
  areaNode = cAreas->Last();
  while (areaNode && !hitArea) {
    if (!capThis) {
      wxArea* area;
      area = (wxArea*)areaNode->Data();
      if ((!wxWindow::gMouseWindow && area->WindowPointInArea(hitX, hitY))
	  || (wxWindow::gMouseWindow && IsCaptureAncestorArea(area)))
	hitArea = area;
      else 
	areaNode = areaNode->Previous();
    }
    
    if (hitArea || capThis) {
      wxMouseEvent *areaMouseEvent;
      int hitAreaX, hitAreaY;

      areaMouseEvent = new WXGC_PTRS wxMouseEvent(0);
      *areaMouseEvent = *mouseEvent;
      if (hitArea) {
	wxMargin hitAreaMargin;
	hitAreaMargin = hitArea->Margin(this);
	hitAreaX = hitAreaMargin.Offset(wxLeft);
	hitAreaY = hitAreaMargin.Offset(wxTop);
      } else
	hitAreaX = hitAreaY = 0;
      areaMouseEvent->x = hitX - hitAreaX; // hit area c.s.
      areaMouseEvent->y = hitY - hitAreaY; // hit area c.s.

      if (!capThis) {
	wxChildNode* childWindowNode;
	wxWindow* childWindow;
	wxChildList *wl;
	wl = hitArea->Windows();
	childWindowNode = wl->First();
	while (childWindowNode && !result) {
	  childWindow = (wxWindow*)childWindowNode->Data();
	  result = childWindow->SeekMouseEventArea(areaMouseEvent, metal_drag_ok);
	  if (!result)
	    childWindowNode = childWindowNode->Next();
	}
      }
      
      if (!result) {
	if (capThis || ((hitArea == ClientArea())
			&& (AdjustMetalDragOk(metal_drag_ok),
			    CanAcceptEvent())
			&& (*metal_drag_ok >= 0))) {
	  result = TRUE;

	  if (wxSubType(__type, wxTYPE_CANVAS)
	      || wxSubType(__type, wxTYPE_PANEL)) {
	    if (areaMouseEvent->ButtonDown())
	      CaptureMouse();
	    else if (gMouseWindow == this
		     && !areaMouseEvent->Dragging())
	      ReleaseMouse();
	  }

	  if (!doCallPreMouseEvent(this, this, areaMouseEvent)) {
	    if (WantsFocus() && areaMouseEvent->ButtonDown()) {
	      wxFrame *fr;
	      fr = GetRootFrame();
	      if (fr)
		fr->SetFocusWindow(this);
	    }

	    /* PreOnEvent could disable the target... */
	    if (!IsGray())
	      OnEventCheckMetal(areaMouseEvent, *metal_drag_ok);
	  }
	}
      }
    }
    
    if (result)
      break;
  }

  /* Frame/dialog: hande all events, even outside the window */	
  if (!result && (__type == wxTYPE_FRAME || __type == wxTYPE_DIALOG_BOX)) {
    wxMouseEvent *areaMouseEvent;
    wxMargin hitAreaMargin;
    int hitAreaX, hitAreaY;
    hitArea = ClientArea();
    hitAreaMargin = hitArea->Margin(this);
    hitAreaX = hitAreaMargin.Offset(wxLeft);
    hitAreaY = hitAreaMargin.Offset(wxTop);
    areaMouseEvent = new WXGC_PTRS wxMouseEvent(0);
    *areaMouseEvent = *mouseEvent;
    areaMouseEvent->x = hitX - hitAreaX;
    areaMouseEvent->y = hitY - hitAreaY;
    if (!doCallPreMouseEvent(this, this, areaMouseEvent))
      if (!IsGray())
	OnEvent(areaMouseEvent);
  }
  
  return result;
}

void wxWindow::OnEventCheckMetal(wxMouseEvent *event, int metal_drag_ok)
{
  OnEvent(event);
}

void wxWindow::AdjustMetalDragOk(int *metal_drag_ok)
{
  if (!cHidden)
    *metal_drag_ok = 0;
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Keyboard methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxWindow::SetFocus(void)
{
  if (AcceptsExplicitFocus()) {
    /* Check that it's not hidden relative to the top-level frame: */
    wxWindow *win = this;
    wxFrame* rootFrame;
    rootFrame = GetRootFrame();

    while (win && (win != rootFrame)) {
      if (win->cUserHidden) {
	break;
      }
      if (!win->IsEnable()) {
	break;
      }
      win = win->GetParent();
    }

    if (win == rootFrame)
      rootFrame->SetFocusWindow(this);
  }
}

//-----------------------------------------------------------------------------
void wxWindow::OnSetFocus(void)
{
}

//-----------------------------------------------------------------------------
void wxWindow::OnKillFocus(void)
{
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Quill methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxWindow::SetFont(wxFont* theFont) // mac platform only
{
  font = theFont;
}

//-----------------------------------------------------------------------------
double wxWindow::GetCharHeight(void)
{
  double theCharHeight;
  if (font)
    theCharHeight = font->GetCharHeight();
  else
    theCharHeight =  0.0;
  return theCharHeight;
}

//-----------------------------------------------------------------------------
double wxWindow::GetCharWidth(void)
{
  double theCharWidth;
  if (font)
    theCharWidth = font->GetCharWidth();
  else
    theCharWidth =  0.0;
  return theCharWidth;
}

//-----------------------------------------------------------------------------
void wxWindow::GetTextExtent(const char* string, double* x, double* y, double* descent,
			     double* externalLeading, wxFont* the_font, Bool use16)
{
  if (the_font)
    the_font->GetTextExtent((char *)string, 0, -1, x, y, descent, externalLeading, TRUE, use16);
  else if (font)
    font->GetTextExtent((char *)string, 0, -1, x, y, descent, externalLeading, TRUE, use16);
  else {
    *x = -1;
    *y = -1;
    if (descent) *descent = 0.0;
    if (externalLeading) *externalLeading = 0.0;
  }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Activate methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------

void wxWindow::Activate(Bool flag) // mac platform only
{
  wxNode* areaNode;
  wxArea* area;
  wxChildNode* childWindowNode;
  wxWindow* childWindow;
  wxChildList *wl;

  if (!!cActive == !!flag)
    return;

  if (flag && (__type == wxTYPE_FRAME)) {
    /* If this is the root frame, try to move it behind everything
       else.  If there is any other window, the root frame shouldn't
       be frontmost. */
    if (this == wxRootFrame) {
      CGrafPtr graf;
      WindowRef win;
      graf = cMacDC->macGrafPort();
      win = GetWindowFromPort(graf);
      ::SendBehind(win, NULL);
    }

    /* Don't activate unless it's still frontmost */
    if ((FrontNonFloatingWindow() != GetWindowFromPort(cMacDC->macGrafPort()))
	&& !wx_activate_anyway)
      return;

    wx_activate_anyway = 0;
  }

  cActive = flag;
  ShowAsActive(flag);

  areaNode = cAreas->First();
  while (areaNode) {
    area = (wxArea*)areaNode->Data();
    wl = area->Windows();
    childWindowNode = wl->First();
    while (childWindowNode) {
      childWindow = (wxWindow*)childWindowNode->Data();
      childWindow->Activate(flag);
      childWindowNode = childWindowNode->Next();
    }
    areaNode = areaNode->Next();
  }

  OnActivate(flag);
}

//-----------------------------------------------------------------------------
void wxWindow::ShowAsActive(Bool flag) // mac platform only
{
  if (cPaintControl) {
    if (flag) {
      DeactivateControl(cPaintControl);
    } else {
      ActivateControl(cPaintControl);
    }
    Refresh();
  }
}

//-----------------------------------------------------------------------------
void wxWindow::OnActivate(Bool flag) // mac platform only
{
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxWindow::Paint(void)
{
}

void wxWindow::PaintRgn(RgnHandle rgn)
{
  Paint();
}

//-----------------------------------------------------------------------------

// Enabling Logic:  a window (or control) is shown as OS-activated when
// a) the window is enabled (cEnabled == true)
// b) no enclosing window is disabled (internal_gray == 0)
// These are all local properties of windows.
// So, if we imagine this as a state machine, when does this predicate
// go from true to false or false to true? The least error-prone way to
// do this is simply to compute the predicate given above, and see whether
// it changes.

// The OS_Active predicate tells whether, for the current state of the
// window, it should be displayed as "usable" or not.

Bool wxWindow::OS_Active()
{
  return ((internal_gray == 0) && cEnable);
}

void wxWindow::Enable(Bool Flag) 
     /* Disabling blocks mouse and keyboard events, not update events. */
     /* I.e., like Windows, not like X-Windows */
{
  if (!cEnable != !Flag) {
    Bool current;
    current = OS_Active();
  
    cEnable = Flag;
    
    ChildrenInternalGray(Flag ? -1 : 1);
    
    if (current != OS_Active()) {
      ChangeToGray(!OS_Active());
    } 
  }
}

void wxWindow::InternalGray(int gray_amt)
{
  Bool current;
  current = OS_Active();

  internal_gray += gray_amt;
  
  ChildrenInternalGray(gray_amt);
  
  if (current != OS_Active()) {
    ChangeToGray(!OS_Active());
  }
}

// the ChangeToGray procedures doesn't check anything:
// it just changes the window to gray or not-gray

void wxWindow::ChangeToGray(Bool gray)
{
  // Refresh();
  
  if (cMacDC->currentUser() == this)
    /* fg pen needs reset: */
    cMacDC->setCurrentUser(NULL);

  if (gray) {
    wxFrame* frame;
    frame = GetRootFrame();
    if (this == frame->GetFocusWindow()) {
      frame->SetFocusWindow(NULL);
    }
  }
}

Bool wxWindow::IsGray(void)
{
  return !cEnable || internal_gray;
}

void wxWindow::ChildrenInternalGray(int gray_amt)
{
  wxChildNode *node;
  wxWindow *w;
  wxChildList *cl;
  
  cl = GetChildren();
  for (node = cl->First(); node; node = node->Next()) {
    w = (wxWindow *)(node->Data());
    w->InternalGray(gray_amt);
  }
}

void wxWindow::InitInternalGray()
{
  wxWindow *p;

  p = GetParent();

  if (!p->cEnable || p->internal_gray) {
    InternalGray(p->internal_gray + (p->cEnable ? 0 : 1));
  }

  if (!p->cActive) {
    Activate(FALSE);
  }
}

void wxWindow::Highlight(Bool on)
{
}

int wxWindow::Track(Point p)
{
  int on = FALSE;
  Rect r;

  ::SetRect(&r, 0, 0, cWindowWidth, cWindowHeight);
  OffsetRect(&r,SetOriginX,SetOriginY);
  
  while (::StillDown()) {
    GetMouse(&p);
    if (!!PtInRect(p, &r) != on) {
      on = !on;
      Highlight(on);
    }
  }
  
  if (on)
    Highlight(FALSE);
  
  return PtInRect(p, &r);
}

void wxWindow::FlushDisplay()
{
  if (cMacDC) {
    CGrafPtr theMacGrafPort;
    theMacGrafPort = cMacDC->macGrafPort();
    QDFlushPortBuffer(theMacGrafPort, NULL);
  }
}

//-----------------------------------------------------------------------------
Bool wxWindow::IsEnable(void) { return cEnable; }

Bool wxWindow::CanAcceptEvent(void)
{
  wxWindow *w = this;
  
  if (!IsEnable())
    return FALSE;
  if (cHidden)
    return FALSE;

  if (!wxWindow::gMouseWindow)
    return TRUE;

  while (w) {
    if (w == wxWindow::gMouseWindow)
      return TRUE;
    w = w->window_parent;
  }

  return FALSE;
}

void wxWindow::Show(Bool v)
{
  v = !v;
  
  if (v == cUserHidden)
    return;
  
  if (window_parent) {
    wxChildList *cl;
    cl = window_parent->GetChildren();
    cl->Show(this, !v);
  }
  if (cParentArea) {
    wxChildList *wl;
    wl = cParentArea->Windows();
    wl->Show(this, !v);
  }

  cUserHidden = v;
  DoShow(!v);
}

Bool wxWindow::CanShow(Bool v)
{
  if (v && (cUserHidden 
	    || (window_parent && window_parent->IsHidden())))
    return FALSE;

  v = !v;

  if (v == cHidden)
    return FALSE;
  
  return TRUE;
}

void wxWindow::DoShow(Bool v)
{
  if (!CanShow(v))
    return;

  if (cPaintControl) {
    if (v) {
      ::ShowControl(cPaintControl);
    } else {
      ::HideControl(cPaintControl);
    }
  }

  v = !v;

  cHidden = v;

  if (cHidden) {
    /* Check for focus */
    wxFrame* frame;
    frame = GetRootFrame();
    if (frame) {
      wxWindow *f;
      f = frame->GetFocusWindow();
      if (f == this)
	frame->SetFocusWindow(NULL);
    }
  }
  
}

//-----------------------------------------------------------------------------
Direction wxWindow::GetGravitate(void) { return cGravitate; } // mac platform only

//-----------------------------------------------------------------------------
void wxWindow::SetGravitate(Direction direction) { cGravitate = direction; } // mac platform only

//-----------------------------------------------------------------------------
Direction wxWindow::GetJustify(void) { return cJustify; } // mac platform only

//-----------------------------------------------------------------------------
void wxWindow::SetJustify(Direction direction) { cJustify = direction; } // mac platform only

//-----------------------------------------------------------------------------
void wxWindow::DoPeriodicAction(void) // mac platform only
{
  // default is to do nothing
}

//-----------------------------------------------------------------------------
wxCursor* wxWindow::SetCursor(wxCursor* cursor)
{
  wxCursor* old_cursor;

  if (cursor && !cursor->Ok())
    cursor = wx_cursor;
  
  old_cursor = wx_cursor;
  if (old_cursor != cursor) {
    wx_cursor = cursor;
    wxTheApp->AdjustCursor();
  }
  return old_cursor;
}

//-----------------------------------------------------------------------------

void wxWindow::DragAcceptFiles(Bool accept)
{
  wxFrame *f;

  f = GetRootFrame();
  f->AddDragAccept(this, accept);
}

//-----------------------------------------------------------------------------
Bool wxWindow::IsMacWindow(void) { return FALSE; } // mac platform only

//-----------------------------------------------------------------------------
void wxWindow::SetColourMap(wxColourMap* cmap) { }

//-----------------------------------------------------------------------------
Bool wxWindow::PopupMenu(wxMenu *menu, double x, double y)
{
  MenuHandle m;
  Point pos;
  long sel;
  int di;
  int itemId;
  wxPopupEvent *event;

  m = menu->CreateCopy("popup", FALSE);

  if (menu->title) {
    int l;
    char *s;
    l = strlen(menu->title);
    s = new WXGC_ATOMIC char[l + 3];
    s[0] = s[1] = ' ';
    memcpy(s + 2, menu->title, l + 1);
    InsertMenuItem(m, wxC2P(wxItemStripLabel(s)), 0);
    DisableMenuItem(m, 1);
    InsertMenuItem(m, "\p-", 1);
    di = -2;
  } else
    di = 0;

  if (!SetCurrentDC())
    return FALSE;

  ::InsertMenu(m, -1);
  ::CalcMenuSize(m);
  pos.v = (short)y + SetOriginY;
  pos.h = (short)x + SetOriginX;
  LocalToGlobal(&pos);
  wxTracking();
  wx_leave_all_input_alone++;
  sel = ::PopUpMenuSelect(m, pos.v, pos.h, 0);
  --wx_leave_all_input_alone;

  ReleaseCurrentDC();

  if (!sel) {
    itemId = 0;
  } else {
    int macMenuId = HiWord(sel);
    int macMenuItemNum = LoWord(sel) + di;

    if (macMenuItemNum <= 0) {
      itemId = 0;
    } else {
      wxMenu *theWxMenu;
      wxNode* node;
      wxMenuItem* theWxMenuItem;

      if (macMenuId == menu->GetMacMenuId())
	theWxMenu = menu;
      else 
	theWxMenu = menu->wxMacFindSubmenu(macMenuId);
      if (!theWxMenu) wxFatalError("No submenu for menu id.");
      
      node = theWxMenu->menuItems->Nth(macMenuItemNum - 1); // counting from 0
      if (!node) wxFatalError("No wxNode for Nth menuItem.");
      
      theWxMenuItem = (wxMenuItem*) node->Data();
      if (!theWxMenuItem) wxFatalError("No wxMenuItem for wxNode.");
      
      if (theWxMenuItem->IsCheckable()) {
        theWxMenuItem->Check(!theWxMenuItem->IsChecked());
      }

      itemId = theWxMenuItem->itemId;
    }
  }

  event = new WXGC_PTRS wxPopupEvent();
  event->menuId = itemId;

  menu->ProcessCommand(event);

  return TRUE;
}

//-----------------------------------------------------------------------------
void wxWindow::SetEraser(wxBrush* eraser) { cEraser = eraser; }

//-----------------------------------------------------------------------------

Bool wxWindow::AdjustCursor(int mouseX, int mouseY)
{
  Bool result = FALSE;
  wxArea* hitArea;
  wxNode* areaNode;
  wxArea *area;

  // For point expressed in parent area c.s., seek deepest sub-window containing it
  int hitX = mouseX - cWindowX; // window c.s.
  int hitY = mouseY - cWindowY; // window c.s.

  if (wxWindow::gMouseWindow == this) {
    wxCursor *c;
    c = GetEffectiveCursor();
    wxSetCursor(c);
    return TRUE;
  }

  if (IsHidden())
    return FALSE;

  hitArea = NULL;
  areaNode = cAreas->Last();
  while (areaNode && !hitArea) {
    area = (wxArea*)areaNode->Data();
    if ((!wxWindow::gMouseWindow && area->WindowPointInArea(hitX, hitY))
	|| (wxWindow::gMouseWindow && IsCaptureAncestorArea(area)))
      hitArea = area;
    else areaNode = areaNode->Previous();
  }

  if (hitArea) {
    wxMargin hitAreaMargin;
    int hitAreaX, hitAreaY, areaX, areaY;
    wxChildNode* childWindowNode;
    wxChildList *wl;
    wxWindow* childWindow;

    hitAreaMargin = hitArea->Margin(hitArea->ParentWindow());
    hitAreaX = hitAreaMargin.Offset(wxLeft);
    hitAreaY = hitAreaMargin.Offset(wxTop);
    areaX = hitX - hitAreaX; // hit area c.s.
    areaY = hitY - hitAreaY; // hit area c.s.
    wl = hitArea->Windows();
    childWindowNode = wl->First();
    while (childWindowNode && !result) {
      childWindow = (wxWindow*)childWindowNode->Data();
      result = childWindow->AdjustCursor(areaX, areaY);
      if (!result) childWindowNode = childWindowNode->Next();
    }
    
    if (!result) {
      if (hitArea == ClientArea()) {
	wxCursor *c;
	result = TRUE;
	c = GetEffectiveCursor();
	wxSetCursor(c);
      }
    }
  }

  return result;
}

Bool wxWindow::WantsFocus(void)
{
  return FALSE;
}

//-----------------------------------------------------------------------------
// tom: fettig@dfki.uni-sb.de
// to be called by any window, which could have the focus!!
void wxWindow::DestroyFocus() 
{
  wxFrame* root;
  root = GetRootFrame();
  if (root->GetFocusWindow()==this)
    root->SetFocusWindow(NULL);	
}


Bool wxWindow::PreOnEvent(wxWindow *, wxMouseEvent *)
{
  return FALSE;
}

Bool wxWindow::PreOnChar(wxWindow *, wxKeyEvent *)
{
  return FALSE;
}

void wxWindow::ForEach(void (*foreach)(wxWindow *w, void *data), void *data)
{
  wxChildNode *node, *next;
  wxWindow *c;
  wxChildList *cl;

  cl = GetChildren();
  for (node = cl->First(); node; node = next) {
    c = (wxWindow *)(node->Data());
    next = node->Next();
    if (c) {
      c->ForEach(foreach, data);
    }
  }
  
  foreach(this, data);
}

wxCursor *wxWindow::GetEffectiveCursor(void)
{
  wxWindow *p = this;

  if (!wx_cursor && cGrandcursor) {
    p = p->GetParent();
    if (p)
      p = p->GetParent();
  }
  while (p && !p->wx_cursor) {
    p = p->GetParent();
  }
  if (p)
    return p->wx_cursor;
  else
    return NULL;
}

Bool wxWindow::GetsFocus()
{
  return AcceptsExplicitFocus();
}

Bool wxWindow::AcceptsExplicitFocus()
{
  return WantsFocus();
}

int wxWindow::MaybeMetalDrag(wxMouseEvent *event)
{
  /* Maybe drag metal frame... */
  if (event->ButtonDown(1)) {
    wxFrame *f;
    f = GetRootFrame();
    if (f->GetWindowStyleFlag() & wxMETAL) {
      int x = event->x, y = event->y;
      Point start;
      ClientToScreen(&x, &y);
      start.h = x;
      start.v = y;
      f->DragFrame(start);
      return 1;
    }
  }
  return 0;
}

//-----------------------------------------------------------------------------

void wxWindow::MaybeMoveControls()
{
  if (cPaintControl) {
    int x, y;
    GetWinOrigin(&x, &y);
    MoveControl(cPaintControl, x, y);
  }
}


wxWindow *wxWindow::EnterLeaveTarget()
{
  return this;
}


void wxFlushMacDisplay(void)
{
  CGrafPtr g;
  ::GetPort(&g);
  ::QDFlushPortBuffer(g, NULL);  
}
