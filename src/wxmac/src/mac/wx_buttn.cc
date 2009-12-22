///////////////////////////////////////////////////////////////////////////////
// File:	wx_buttn.cc
// Purpose:	Panel item button implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wx_buttn.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_panel.h"
#include "wx_area.h"
#include "wxBorderArea.h"
#include "wb_gdi.h"
#ifndef WX_CARBON
#include <Windows.h>
#endif
#include "wxButtonBorder.h"

// Under OS X, an inset is necessary because the OS draws outside of the control rectangle.
#define PAD_X 5
#define PAD_Y 5

#define IB_MARGIN_X 5
#define IB_MARGIN_Y 5

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxButton::wxButton // Constructor (given parentPanel, label)
(
 wxPanel*	parentPanel,
 wxFunction	function,
 char*		label,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 wxFont         *_font,
 char*		windowName,
 WXTYPE		objectType
 ) :
 wxbButton (parentPanel, function, x, y, width, height, style, windowName)
     
{
  SetFont(_font, 13);
  Create(parentPanel, function, label, x, y, width, height, style, windowName, objectType);
}

void wxButton::Create // Real constructor (given parentPanel, label)
(
 wxPanel*	parentPanel,
 wxFunction	function,
 char*		label,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 char*		windowName,
 WXTYPE		objectType
 ) 	
{
  Rect boundsRect = {0,0,0,0};
  SInt16 baselineOffset; // ignored
  CFStringRef title;
  CGrafPtr theMacGrafPort;

  buttonBitmap = NULL;

  padLeft = padRight = PAD_X;
  padTop = padBottom = PAD_Y;
  
  Callback(function);

  label = wxItemStripLabel(label);

  SetCurrentMacDC();
  theMacGrafPort = cMacDC->macGrafPort();

  // First, create the control with a bogus rectangle;
  ::OffsetRect(&boundsRect,SetOriginX,SetOriginY);
  title = wxCFString(label);
  ::CreatePushButtonControl(GetWindowFromPort(theMacGrafPort), &boundsRect, title, &cMacControl);
  CFRelease(title);

  wxSetControlFont(cMacControl, font);

  // Now, ignore the font data and let the control find the "best" size 
  ::SetRect(&boundsRect,0,0,0,0);
  wxGetBestControlRect(cMacControl,&boundsRect,&baselineOffset,
		       font, 17, 15, 
		       label, 20);

  cWindowWidth = boundsRect.right - boundsRect.left + (padLeft + padRight);
  cWindowHeight = boundsRect.bottom - boundsRect.top + (padTop + padBottom);
  ::SizeControl(cMacControl,boundsRect.right - boundsRect.left, boundsRect.bottom - boundsRect.top);
  
  ::EmbedControl(cMacControl, GetRootControl());

  IgnoreKeyboardEvents();

  if (style & 1) OnSetDefault(TRUE);
  
  {
    wxWindow *p;
    p = GetParent();
    if (p->IsHidden())
      DoShow(FALSE);
  }
  if (style & wxINVISIBLE)
    Show(FALSE);
  InitInternalGray();
}

//-----------------------------------------------------------------------------
wxButton::wxButton // Constructor (given parentPanel, bitmap)
(
 wxPanel*	parentPanel,
 wxFunction	function,
 wxBitmap*	bitmap,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 wxFont         *_font,
 char*		windowName,
 WXTYPE		objectType
 ) :
 wxbButton (parentPanel, function, x, y, width, height, style, windowName)
{
  CGrafPtr theMacGrafPort;
  Rect bounds;

  SetFont(_font, 13);

  if (bitmap->Ok() && (bitmap->selectedIntoDC >= 0)) {
    buttonBitmap = bitmap;
    buttonBitmap->selectedIntoDC++;
  } else {
    Create(parentPanel, function, "<bad-image>", x, y, width, height, style, windowName, objectType);
    return;
  }

  Callback(function);

  cBorderArea = new WXGC_PTRS wxArea(this);
  new WXGC_PTRS wxButtonBorder(cBorderArea);

  SetCurrentMacDC();
  theMacGrafPort = cMacDC->macGrafPort();
#if 0
  // bevel buttons for bitmap buttons can wait until other things are done. ugh.
  Rect bounds;
  ::SetRect(&bounds,0,0,0,0)
    cMacControl = ::NewControl(GetWindowFromPort(theMacGrafPort),&bounds,"\p",TRUE,
			       kControlContentIconSuiteRes...);
#endif
  cMacControl = NULL;


  ::SetRect(&bounds, 0, 0, buttonBitmap->GetWidth(), buttonBitmap->GetHeight());
  bounds.bottom += 2 * IB_MARGIN_Y;
  bounds.right += 2 * IB_MARGIN_X;
  cWindowHeight = bounds.bottom;
  cWindowWidth = bounds.right;
  OffsetRect(&bounds,SetOriginX,SetOriginY);

  if (style & 1) OnSetDefault(TRUE);

  CreatePaintControl();

  {
    wxWindow *p;
    p = GetParent();
    if (p->IsHidden())
      DoShow(FALSE);
  }
  if (style & wxINVISIBLE)
    Show(FALSE);

  InitInternalGray();
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxButton::~wxButton(void)
{
  if (buttonBitmap == NULL) {
    if (cMacControl) { ::DisposeControl(cMacControl); }
    cMacControl = NULL;
  } else
    --buttonBitmap->selectedIntoDC;
}

//-----------------------------------------------------------------------------
char* wxButton::GetLabel(void)
{
  Str255	pTitle;
  if (buttonBitmap)
    return NULL;
  if (cMacControl) {
    ::GetControlTitle(cMacControl, pTitle);
  }
  CopyPascalStringToC(pTitle, wxBuffer);
  return wxBuffer;
}

//-----------------------------------------------------------------------------
void wxButton::SetLabel(char* label)
{
  if (buttonBitmap)
    return;
  if (label) {
    if (cMacControl) {
      {
	CFStringRef llabel;
	llabel = wxCFString(wxItemStripLabel(label));
	SetControlTitleWithCFString(cMacControl, llabel);
	CFRelease(llabel);
	RefreshIfUpdating();
      }
    }
  }
}

//-----------------------------------------------------------------------------
void wxButton::SetLabel(wxBitmap* bitmap)
{
  if (!buttonBitmap || !bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return;
  --buttonBitmap->selectedIntoDC;
  buttonBitmap = bitmap;
  buttonBitmap->selectedIntoDC++;
  Refresh();
}

//-----------------------------------------------------------------------------
void wxButton::SetDefault(Bool flag) // WCH : modification of original (see below too)
{ 
  wxPanel* panel;
  wxButton* currentDefault;

  panel = (wxPanel*) GetParent();
  currentDefault = panel->defaultItem;

  if (flag) {
    if (currentDefault != this) {
      if (currentDefault) currentDefault->OnSetDefault(FALSE);
      panel->defaultItem = this;
      OnSetDefault(TRUE);
    }
  } else {
    if (currentDefault == this) {
      currentDefault->OnSetDefault(FALSE);
      panel->defaultItem = NULL;
    }
  }
}

//-----------------------------------------------------------------------------
void wxButton::OnSetDefault(Bool flag) // WCH : addition to original
{
  if (!buttonBitmap && cMacControl) {
    char byteFlag = (char)flag;
    SetControlData(cMacControl,kControlEntireControl,kControlPushButtonDefaultTag,1,&byteFlag);
  } else {
    if (buttonBitmap)
      return;
    if (flag) {
      START_XFORM_SKIP;
      wxMargin margin(4);
      END_XFORM_SKIP;
      cBorderArea->SetMargin(margin, wxAll,
			     cWindowWidth + 8, cWindowHeight + 8,
			     cWindowX - 4, cWindowY - 4);
    } else {
      START_XFORM_SKIP;
      wxMargin margin(0);
      END_XFORM_SKIP;
      cBorderArea->SetMargin(margin, wxAll,
			     cWindowWidth - 8, cWindowHeight - 8,
			     cWindowX + 4, cWindowY + 4);
    }
  }
}

//-----------------------------------------------------------------------------
static void PaintBitmapButton(Rect *r, wxBitmap *buttonBitmap, Bool pressed, Bool focused, Bool isgray)
{
  ThemeButtonDrawInfo state;
  
  state.state = (pressed
		 ? kThemeStatePressed
		 : (!isgray ? kThemeStateActive : kThemeStateInactive));
  state.value = kThemeButtonOff;
  state.adornment = focused ?  kThemeAdornmentFocus : kThemeAdornmentNone;
  if (isgray) {
    buttonBitmap->DrawMac(IB_MARGIN_X, IB_MARGIN_Y, patOr);
    DrawThemeButton(r, kThemeRoundedBevelButton, &state, NULL, NULL /* erase */, NULL, 0);
  } else {
    DrawThemeButton(r, kThemeRoundedBevelButton, &state, NULL, NULL /* erase */, NULL, 0);
    buttonBitmap->DrawMac(IB_MARGIN_X, IB_MARGIN_Y, patOr);
  }
}

void wxButton::Paint(void)
{
  if (cHidden) return;
  {
    if (buttonBitmap) {
      Rect r;
      Bool isgray;
      ::SetRect(&r, 0, 0, cWindowWidth, cWindowHeight);
      OffsetRect(&r,SetOriginX,SetOriginY);
      isgray = IsGray();
      PaintBitmapButton(&r, buttonBitmap, trackstate & 0x1, trackstate & 0x2, isgray || !cActive);
    }
    wxWindow::Paint();
  }
}

//-----------------------------------------------------------------------------
void wxButton::DoShow(Bool show)
{
  if (!CanShow(show)) return;

  if (!buttonBitmap && cMacControl) {
    if (show) {
      ::ShowControl(cMacControl);
    } else {
      ::HideControl(cMacControl);
    }
  }
  
  wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------
void wxButton::Highlight(Bool flag) // mac platform only
{
  if (buttonBitmap) {
    if (flag)
      trackstate |= 0x1;
    else
      trackstate -= (trackstate & 0x1);
    Refresh();
  } else if (cMacControl) {
    if (cEnable) {
      ::HiliteControl(cMacControl, flag ? kControlButtonPart : 0);
    }
  }
}

//-----------------------------------------------------------------------------
void wxButton::OnEvent(wxMouseEvent *event) // mac platform only
{
  if (!cActive)
    return;

  if (event->LeftDown()) {
    int startH, startV;
    Point startPt;
    int trackResult;

    SetCurrentDC();
      
    event->Position(&startH, &startV); // client c.s.
      
    startPt.v = startV - padTop;
    startPt.h = startH - padLeft;

    wxTracking();
    if (buttonBitmap == NULL && cMacControl) {
      trackResult = ::TrackControl(cMacControl, startPt, NULL);
    } else {
      trackResult = Track(startPt);
    }
    
    if (trackResult) {
      wxCommandEvent *commandEvent;
      commandEvent = new WXGC_PTRS wxCommandEvent(wxEVENT_TYPE_BUTTON_COMMAND);
      ProcessCommand(commandEvent);
    }
  }
}

void wxButton::ChangeToGray(Bool gray)
{
  wxbButton::ChangeToGray(gray);
  if (buttonBitmap)
    Refresh();
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Sizing methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxButton::OnClientAreaDSize(int dW, int dH, int dX, int dY) // mac platform only
{
  if (buttonBitmap) {
    wxWindow::OnClientAreaDSize(dW, dH, dX, dY);
    return;
  }

  if (!cMacControl)
    return;

  if (dW || dH) {
    int clientWidth, clientHeight;
    GetClientSize(&clientWidth, &clientHeight);
    ::SizeControl(cMacControl, clientWidth - (padLeft + padRight), 
		  clientHeight - (padTop + padBottom));
  }
  
  if (dX || dY)
    MaybeMoveControls();
}

//----------------------------------------
void wxButton::OnSetFocus()
{
  trackstate |= 0x2;
  wxItem::OnSetFocus();
  if (!cMacControl)
    Refresh();
}

void wxButton::OnKillFocus()
{
  trackstate -= (trackstate & 0x2);
  wxItem::OnKillFocus();
  if (!cMacControl)
    Refresh();
}
