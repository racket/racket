///////////////////////////////////////////////////////////////////////////////
// File:	wx_item.cc
// Purpose:	Panel items implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

/* When implementing a new item, be sure to:
 *
 * - add the item to the parent panel
 * - set window_parent to the parent
 * - NULL any extra child window pointers not created for this item
 *   (e.g. label control that wasn't needed)
 * - delete any extra child windows in the destructor (e.g. label control)
 * - implement GetSize and SetSize
 * - to find panel position if coordinates are (-1, -1), use GetPosition
 * - call AdvanceCursor after creation, for panel layout mechanism.
 *
 */

#include "wx_item.h"
#include "wx_gdi.h"
#include "wx_utils.h"

extern void wxSmuggleOutEvent(EventRef);
Bool wx_propagate_key;

wxItem::wxItem(void)
: wxbItem()
{
  SetEraser(wxCONTROL_BACKGROUND_BRUSH);
}

//-----------------------------------------------------------------------------
// Constructor (given parentArea)
wxItem::wxItem (wxArea* parentArea, int x, int y, int width, int height,
		long style, char* windowName)
: wxbItem (windowName, parentArea, x, y, width, height, style)
{
  SetEraser(wxCONTROL_BACKGROUND_BRUSH);
  cMacControl = NULL;
  padLeft = padRight = padTop = padBottom = 0;
}

//-----------------------------------------------------------------------------
// Constructor (given parentWindow)
wxItem::wxItem (wxWindow* parentWindow, int x, int y, int width, int height, 
		long style, char* windowName) 
: wxbItem (windowName, parentWindow, x, y, width, height, style)
{
  SetEraser(wxCONTROL_BACKGROUND_BRUSH);
  cMacControl = NULL;
  padLeft = padRight = padTop = padBottom = 0;
}

//-----------------------------------------------------------------------------
// Constructor (given objectType; i.e., menu or menuBar)
wxItem::wxItem (char* windowName) 
: wxbItem (windowName)
{
  SetEraser(wxCONTROL_BACKGROUND_BRUSH);
  cMacControl = NULL;
  padLeft = padRight = padTop = padBottom = 0;
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxItem::~wxItem(void)
{
}

//-----------------------------------------------------------------------------
void wxItem::SetFont(wxFont *fnt, int defsize)
{
  /* See also wx_lbox.cc, which has a separate label_font */
  if (!fnt) {
    fnt = wxTheFontList->FindOrCreateFont(defsize, wxSYSTEM, wxNORMAL, wxNORMAL, FALSE);
  }
  font = fnt;
}

//-----------------------------------------------------------------------------
void wxItem::ChangeToGray(Bool gray)
{
  if (cMacControl) {
    if (gray) {
#ifdef OS_X
      DisableControl(cMacControl);
#else
      HiliteControl(cMacControl, 255);
#endif
    } else {
#ifdef OS_X
      EnableControl(cMacControl);
#else
      HiliteControl(cMacControl, 0);
#endif
    }
  }

  wxWindow::ChangeToGray(gray);
}

void wxItem::Activate(Bool on)
{
  if (cMacControl) {
    if (!on) {
      DeactivateControl(cMacControl);
    } else {
      ActivateControl(cMacControl);
    }
  }

  wxWindow::Activate(on);
}



//-----------------------------------------------------------------------------
void wxItem::OnChar(wxKeyEvent *event)
{
  // Default is to pass chars up to our panel
  wxPanel *parent;
  parent = (wxPanel *) GetParent();
  if (parent) {
    // parent is not always a wxPanel: can be a wxMenu...
    if (wxSubType(parent->__type,wxTYPE_PANEL))
      parent->OnChar(event);
  }
}

void wxSetControlFont(ControlRef c, wxFont *font)
{
  ControlFontStyleRec rec;

  rec.flags = (kControlUseFontMask
	       | kControlUseFaceMask
	       | kControlUseSizeMask);
  rec.font = font->GetMacFontNum();
  rec.size = font->GetPointSize();
  rec.style = font->GetMacFontStyle();

  SetControlFontStyle(c, &rec);
}

void wxGetBestControlRect(ControlRef c, Rect *r, SInt16 *offset, 
			  wxFont *font, int small_height, int mini_height, 
			  char *label, int width_pad)
{
  int size;
  
  ::GetBestControlRect(c, r, offset);

  size = font->GetPointSize();

  /* Enforce Apple guidelines for standard sizes,
     because GetBestControlRect() doesn't. */
  if (size == 11) {
    r->bottom = r->top + small_height;
  } else if (size == 9) {
    r->bottom = r->top + mini_height;
  }

  if (label && (size < 13)) {
    /* GetBestControlRect makes things too wide */
    double x, y;
    font->GetTextExtent(label, 0, -1, &x, &y, NULL, NULL);
    if (r->right - r->left > x + width_pad)
      r->right = r->left + (short)x + (short)width_pad;
  }
}

char *wxItemStripLabel(char *label)
{
  return wxStripMenuCodes(label, NULL);
}


//-----------------------------------------------------------------------------

void wxItem::MaybeMoveControls()
{
  if (cMacControl) {
    int x, y;
    GetWinOrigin(&x, &y);
    MoveControl(cMacControl, x + padLeft, y + padTop);
  }

  wxWindow::MaybeMoveControls();
}

//-----------------------------------------------------------------------------
static ControlRef test_control;
static WindowRef test_win;

Bool wxAllControlsWantFocus()
{
  /* Create a hidden window/control to check wherg Full Keyboard Access
     has been enabled by the user: */
  if (!test_control && !test_win) {
    OSErr result;
    Rect r = { 0 , 0, 100, 100 };

    result = ::CreateNewWindow(kDocumentWindowClass, kWindowStandardDocumentAttributes, &r, &test_win);
    if (result == noErr) {
      Rect r2 = { 10 , 10, 90, 90 };
      result = ::CreatePushButtonControl(test_win, &r2, CFSTR("Ok"), &test_control);
      if (result == noErr) {
	/* We have a test control for checking focus, now */
      } else {
	::DisposeWindow(test_win);
	test_control = NULL;
      }
    }
  }

  if (test_control) {
    /* Check for Full Keyboard Access by trying to set the focus: */
    ControlRef c;
    ::ClearKeyboardFocus(test_win);
    ::SetKeyboardFocus(test_win, test_control, kControlFocusNextPart);
    ::GetKeyboardFocus(test_win, &c);
    return (c == test_control);
  } else
    return FALSE;
}

Bool wxItem::AcceptsExplicitFocus()
{
  if (WantsFocus())
    return TRUE;

  return wxAllControlsWantFocus();
}

//-----------------------------------------------------------------------------
void wxItem::OnSetFocus()
{
  if (cMacControl) {
    ::ClearKeyboardFocus(GetWindowFromPort(cMacDC->macGrafPort()));
    ::SetKeyboardFocus(GetWindowFromPort(cMacDC->macGrafPort()),
		       cMacControl,
		       kControlFocusNextPart);
  }
}

void wxItem::OnKillFocus()
{
  ::ClearKeyboardFocus(GetWindowFromPort(cMacDC->macGrafPort()));
}

static OSStatus myEventHandler(EventHandlerCallRef inHandlerCallRef, 
			       EventRef inEvent, 
			       void *inUserData)
{
  if (wx_propagate_key) {
    return eventNotHandledErr;
  } else {
    /* We don't want anyone else to handle this event
       at the OS X level, out smuggle out the event
       so it can be picked up at the WaitNextEvent level: */
    wxSmuggleOutEvent(inEvent);
    return noErr;
  }
}

void wxItem::IgnoreKeyboardEvents()
{
  if (cMacControl) {
    EventHandlerRef ref;
    EventTypeSpec evts[3];

    evts[0].eventClass = kEventClassKeyboard;
    evts[0].eventKind = kEventRawKeyDown;
    evts[1].eventClass = kEventClassKeyboard;
    evts[1].eventKind = kEventRawKeyRepeat;
    evts[2].eventClass = kEventClassKeyboard;
    evts[2].eventKind = kEventRawKeyUp;

    ::InstallEventHandler(GetControlEventTarget(cMacControl), 
			  myEventHandler,
			  3,
			  evts,
			  NULL,
			  &ref);
  }
}
