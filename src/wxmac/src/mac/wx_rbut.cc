///////////////////////////////////////////////////////////////////////////////
// File:	wx_rbut.cc
// Purpose:	Panel item radioButton implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wx_rbut.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_panel.h"
#include "wx_area.h"
#ifndef WX_CARBON
# include <QuickDraw.h>
#endif

#define IR_CIRCLE_SIZE 18
#define IR_X_SPACE 3
#define IR_Y_SPACE 2
#define IR_MIN_HEIGHT (IR_CIRCLE_SIZE + 2 * IR_Y_SPACE)
#define IR_ON_INSET 3

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxRadioButton::wxRadioButton // Constructor (given parentPanel, label)
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
 wxItem (parentPanel, x, y, width, height, style, windowName)
{
  SetFont(_font, 13);
  Create(parentPanel, function, label, x, y, width, height, style, windowName, objectType);
}

void wxRadioButton::Create // Real constructor (given parentPanel, label)
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
  OSErr err;
  CGrafPtr theMacGrafPort;
  Rect boundsRect = {0, 0, 0, 0};
  CFStringRef theMacLabel;
  SInt16 baselineOffset; // ignored

  buttonBitmap = NULL;
  Callback(function);
  
  // First, create the control with a bogus rectangle;
  SetCurrentMacDC();
  theMacGrafPort = cMacDC->macGrafPort();
  OffsetRect(&boundsRect,SetOriginX,SetOriginY);
  theMacLabel = wxCFString(label);
  
  err = CreateRadioButtonControl(GetWindowFromPort(theMacGrafPort), &boundsRect, theMacLabel,
				 0, FALSE, &cMacControl);

  CFRelease(theMacLabel);

  wxSetControlFont(cMacControl, font);
  
  // Now, ignore the font data and let the control find the "best" size 
  wxGetBestControlRect(cMacControl,&boundsRect,&baselineOffset,
		       font, 15, 11,
		       label, 2 * IR_CIRCLE_SIZE);
  cWindowWidth = boundsRect.right - boundsRect.left;
  cWindowHeight = boundsRect.bottom - boundsRect.top;
  ::SizeControl(cMacControl, boundsRect.right - boundsRect.left, boundsRect.bottom - boundsRect.top);

  ::EmbedControl(cMacControl, GetRootControl());
  IgnoreKeyboardEvents();

  {
    wxWindow *p;
    p = GetParent();
    if (p->IsHidden())
      DoShow(FALSE);
  }
  InitInternalGray();
}

//-----------------------------------------------------------------------------
wxRadioButton::wxRadioButton // Constructor (given parentPanel, bitmap)
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
 wxItem (parentPanel, x, y, width, height, style, windowName)
{
  Rect bounds;

  SetFont(_font, 13);

  if (bitmap->Ok() && (bitmap->selectedIntoDC >= 0)) {
    buttonBitmap = bitmap;
    buttonBitmap->selectedIntoDC++;
  } else {
    Create(parentPanel, function, "<bad-image>", x, y, width, height, style, windowName, objectType);
  }

  Callback(function);
  cMacControl = NULL;
  
  ::SetRect(&bounds, 0, 0, buttonBitmap->GetWidth(), buttonBitmap->GetHeight());
  cWindowHeight = bounds.bottom;
  cWindowWidth = bounds.right + IR_CIRCLE_SIZE + IR_X_SPACE;
  if (cWindowHeight < IR_MIN_HEIGHT)
    cWindowHeight = IR_MIN_HEIGHT;
  OffsetRect(&bounds,SetOriginX,SetOriginY);

  CreatePaintControl();
  
  {
    wxWindow *p;
    p = GetParent();
    if (p->IsHidden())
      DoShow(FALSE);
  }

  InitInternalGray();
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxRadioButton::~wxRadioButton(void)
{
  if (cMacControl) { ::DisposeControl(cMacControl); }
  if (buttonBitmap)
    --buttonBitmap->selectedIntoDC;
}

//-----------------------------------------------------------------------------
char* wxRadioButton::GetLabel()
{
  if (cMacControl) {
    Str255	pLabel;

    ::GetControlTitle(cMacControl, pLabel);
    ::CopyPascalStringToC(pLabel, wxBuffer);
    return copystring(wxBuffer);
  } else if (labelString)
    return labelString;
  else
    return NULL;
}

//-----------------------------------------------------------------------------
void wxRadioButton::SetLabel(char* label)
{
  if (label && !buttonBitmap) {
    if (cMacControl) {
      SetCurrentDC();
      {
	CFStringRef llabel;
	llabel = wxCFString(label);
	SetControlTitleWithCFString(cMacControl, llabel);
	CFRelease(llabel);
      }
    } else
      labelString = label;
  }
}

//-----------------------------------------------------------------------------
void wxRadioButton::SetLabel(wxBitmap* bitmap)
{
  if (buttonBitmap && bitmap->Ok() && (bitmap->selectedIntoDC >= 0)) {
    --buttonBitmap->selectedIntoDC;
    buttonBitmap = bitmap;
    buttonBitmap->selectedIntoDC++;
    Refresh();
  }
}

//-----------------------------------------------------------------------------
void wxRadioButton::SetValue(Bool val)
{
  if (cMacControl) {
    ::SetControlValue(cMacControl, val ? 1 : 0);
  } else {
    bitmapState = !!val;
    if (!cHidden) {
      Refresh();
    }
  }
}

//-----------------------------------------------------------------------------
Bool wxRadioButton::GetValue(void)
{
  if (cMacControl) {
    short value;
    value = ::GetControlValue(cMacControl);
    return (value != 0) ? TRUE : FALSE;
  } else
    return bitmapState;
}

//-----------------------------------------------------------------------------
void wxRadioButton::Paint(void)
{
  if (cHidden) return;
  { 
    Rect r;

    SetRect(&r, 0, 0, cWindowWidth, cWindowHeight);

    OffsetRect(&r,SetOriginX,SetOriginY);
    {
      if (buttonBitmap) {
	int btop;
	btop = (cWindowHeight - buttonBitmap->GetHeight()) / 2;
	buttonBitmap->DrawMac(IR_CIRCLE_SIZE + IR_X_SPACE, btop);
      } else if (labelString) {
	Rect r = { SetOriginY, IR_CIRCLE_SIZE + IR_X_SPACE + SetOriginX, 
		   SetOriginY + cWindowHeight, SetOriginX + cWindowWidth };
	CFStringRef str;

	str = wxCFString(labelString);
	
	DrawThemeTextBox(str, kThemeSystemFont, kThemeStateActive,
			 0, &r, teJustLeft, 0);

	CFRelease(str);
      }

      {
	int top = (cWindowHeight - IR_CIRCLE_SIZE) / 2;
	Rect r = { top, 0, top + IR_CIRCLE_SIZE, IR_CIRCLE_SIZE };
	OffsetRect(&r,SetOriginX,SetOriginY);
	{
	  ThemeButtonDrawInfo state;
	  
	  state.state = ((trackState & 0x1)
			 ? kThemeStatePressed
			 : ((cEnable && cActive && !internal_gray) ? kThemeStateActive : kThemeStateInactive));
	  state.value = bitmapState ? kThemeButtonOn : kThemeButtonOff;
	  state.adornment = ((trackState & 0x2) ? kThemeAdornmentFocus : kThemeAdornmentNone);
	  
	  DrawThemeButton(&r, kThemeRadioButton, &state, NULL, NULL /* erase */, NULL, 0);
	}
      }
    }
  }
}

void wxRadioButton::ChangeToGray(Bool gray) 
{
  if (cPaintControl)
    Refresh();
  wxItem::ChangeToGray(gray);
}

void wxRadioButton::Highlight(Bool on)
{
  if (on)
    trackState |= 0x1;
  else
    trackState -= (trackState & 0x1);
  Refresh();
}

//-----------------------------------------------------------------------------
void wxRadioButton::DoShow(Bool show)
{
  if (!CanShow(show)) return;

  if (cMacControl) {
    if (show) {
      ::ShowControl(cMacControl);
    } else {
      ::HideControl(cMacControl);
    }
  }

  wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------
void wxRadioButton::OnEvent(wxMouseEvent *event) // mac platform only
{
  if (cEnable) {
    if (event->LeftDown()) {
      int startH, startV;
      Point startPt;
      int trackResult;

      SetCurrentDC();
      
      event->Position(&startH, &startV); // client c.s.
      
      startPt.v = startV - padTop;
      startPt.h = startH - padLeft;

      wxTracking();
      if (cMacControl)
	trackResult = ::TrackControl(cMacControl, startPt, NULL);
      else
	trackResult = Track(startPt);

      if (trackResult) {
	wxCommandEvent *commandEvent;
	commandEvent = new WXGC_PTRS wxCommandEvent(wxEVENT_TYPE_RADIOBOX_COMMAND);
	ProcessCommand(commandEvent);
      }
    }
  }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Sizing methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxRadioButton::OnClientAreaDSize(int dW, int dH, int dX, int dY) // mac platform only
{

  if (cMacControl) {
    int clientWidth, clientHeight, x,y;
    
    GetClientSize(&clientWidth, &clientHeight);
    GetWinOrigin(&x, &y);
    
    ::SizeControl(cMacControl, clientWidth, clientHeight);
    ::MoveControl(cMacControl, x, y);
  }

  wxWindow::OnClientAreaDSize(dW, dH, dX, dY);
}


//----------------------------------------
void wxRadioButton::OnSetFocus()
{
  trackState |= 0x2;
  wxItem::OnSetFocus();
  if (!cMacControl)
    Refresh();
}

void wxRadioButton::OnKillFocus()
{
  trackState -= (trackState & 0x2);
  wxItem::OnKillFocus();
  if (!cMacControl)
    Refresh();
}
