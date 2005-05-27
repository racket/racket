/*
 * File:	wb_stdev.cc
 * Purpose:	Standard event definitions
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

/*
 * Command events
 *
 */

wxCommandEvent::wxCommandEvent(WXTYPE commandType)
{
  eventClass = wxTYPE_COMMAND_EVENT;
  eventType = commandType;
}
wxCommandEvent::~wxCommandEvent()
{
}


wxPopupEvent::wxPopupEvent()
  : wxCommandEvent(wxEVENT_TYPE_MENU_SELECT)
{
  __type = wxTYPE_POPUP_EVENT;
}

wxScrollEvent::wxScrollEvent()
{
  direction = wxHORIZONTAL;
  moveType = wxEVENT_TYPE_SCROLL_THUMBTRACK;
}

/*
 * Mouse events
 *
 */

wxMouseEvent::wxMouseEvent(WXTYPE commandType)
{
  eventClass = wxTYPE_MOUSE_EVENT;
  eventType = commandType;
  metaDown = FALSE;
  altDown = FALSE;
  controlDown = FALSE;
  shiftDown = FALSE;
}

Bool wxMouseEvent::ControlDown(void)
{
  return controlDown;
}

Bool wxMouseEvent::MetaDown(void)
{
  return metaDown;
}

Bool wxMouseEvent::AltDown(void)
{
  return altDown;
}

Bool wxMouseEvent::ShiftDown(void)
{
  return shiftDown;
}

// Is a button event (*doesn't* mean: is any button *down*?)
Bool wxMouseEvent::IsButton(void)
{
  return Button(-1);
}

// True if was a button dclick event (1 = left, 2 = middle, 3 = right)
// or any button dclick event (but = -1)
Bool wxMouseEvent::ButtonDClick(int but)
{
  switch (but) {
    case -1:
      return (LeftDClick() || MiddleDClick() || RightDClick());
    case 1:
      return LeftDClick();
    case 2:
      return MiddleDClick();
    case 3:
      return RightDClick();
    default:
      return FALSE;
  }
  // NOTREACHED
}

// True if was a button down event (1 = left, 2 = middle, 3 = right)
// or any button down event (but = -1)
Bool wxMouseEvent::ButtonDown(int but)
{
  switch (but) {
    case -1:
      return (LeftDown() || MiddleDown() || RightDown());
    case 1:
      return LeftDown();
    case 2:
      return MiddleDown();
    case 3:
      return RightDown();
    default:
      return FALSE;
  }
  // NOTREACHED
}

// True if was a button up event (1 = left, 2 = middle, 3 = right)
// or any button up event (but = -1)
Bool wxMouseEvent::ButtonUp(int but)
{
  switch (but) {
    case -1:
      return (LeftUp() || MiddleUp() || RightUp());
    case 1:
      return LeftUp();
    case 2:
      return MiddleUp();
    case 3:
      return RightUp();
    default:
      return FALSE;
  }
  // NOTREACHED
}

// True if the given button is currently changing state
Bool wxMouseEvent::Button(int but)
{
  switch (but) {
    case -1:
      return (ButtonUp(-1) || ButtonDown(-1) || ButtonDClick(-1)) ;
    case 1:
      return (LeftDown() || LeftUp() || LeftDClick());
    case 2:
      return (MiddleDown() || MiddleUp() || MiddleDClick());
    case 3:
      return (RightDown() || RightUp() || RightDClick());
    default:
      return FALSE;
  }
  // NOTREACHED
}

Bool wxMouseEvent::LeftDown(void)
{
  return (eventType == wxEVENT_TYPE_LEFT_DOWN);
}

Bool wxMouseEvent::MiddleDown(void)
{
  return (eventType == wxEVENT_TYPE_MIDDLE_DOWN);
}

Bool wxMouseEvent::RightDown(void)
{
  return (eventType == wxEVENT_TYPE_RIGHT_DOWN);
}

Bool wxMouseEvent::LeftDClick(void)
{
  return (eventType == wxEVENT_TYPE_LEFT_DCLICK);
}

Bool wxMouseEvent::MiddleDClick(void)
{
  return (eventType == wxEVENT_TYPE_MIDDLE_DCLICK);
}

Bool wxMouseEvent::RightDClick(void)
{
  return (eventType == wxEVENT_TYPE_RIGHT_DCLICK);
}

Bool wxMouseEvent::LeftUp(void)
{
  return (eventType == wxEVENT_TYPE_LEFT_UP);
}

Bool wxMouseEvent::MiddleUp(void)
{
  return (eventType == wxEVENT_TYPE_MIDDLE_UP);
}

Bool wxMouseEvent::RightUp(void)
{
  return (eventType == wxEVENT_TYPE_RIGHT_UP);
}

Bool wxMouseEvent::Dragging(void)
{
  return ((eventType == wxEVENT_TYPE_MOTION) && (LeftIsDown() || MiddleIsDown() || RightIsDown()));
}

Bool wxMouseEvent::ButtonIsDown(int but)
{
  switch (but) {
    case -1:
      return (LeftIsDown() || MiddleIsDown() || RightIsDown());
    case 1:
      return LeftIsDown();
    case 2:
      return MiddleIsDown();
    case 3:
      return RightIsDown();
    default:
      return FALSE;
  }
  // NOTREACHED
}

Bool wxMouseEvent::LeftIsDown(void)
{
  return leftDown;
}

Bool wxMouseEvent::MiddleIsDown(void)
{
  return middleDown;
}

Bool wxMouseEvent::RightIsDown(void)
{
  return rightDown;
}

Bool wxMouseEvent::Moving(void)
{
  return (eventType == wxEVENT_TYPE_MOTION);
}

Bool wxMouseEvent::Entering(void)
{
  return (eventType == wxEVENT_TYPE_ENTER_WINDOW) ;
}

Bool wxMouseEvent::Leaving(void)
{
  return (eventType == wxEVENT_TYPE_LEAVE_WINDOW) ;
}

void wxMouseEvent::Position(int *xpos, int *ypos)
{
  *xpos = x;
  *ypos = y;
}

/*
 * Keyboard events
 *
 */

wxKeyEvent::wxKeyEvent(WXTYPE type)
{
  eventClass = wxTYPE_KEY_EVENT;
  eventType = type;
  shiftDown = FALSE;
  controlDown = FALSE;
  metaDown = FALSE;
  altDown = FALSE;
  keyCode = 0;
  keyUpCode = WXK_PRESS;
}

Bool wxKeyEvent::ControlDown(void)
{
  return controlDown;
}

Bool wxKeyEvent::AltDown(void)
{
  return altDown;
}

Bool wxKeyEvent::MetaDown(void)
{
  return metaDown;
}

Bool wxKeyEvent::ShiftDown(void)
{
  return shiftDown;
}

long wxKeyEvent::KeyCode(void)
{
  return keyCode;
}

void wxKeyEvent::Position(int *xpos, int *ypos)
{
  *xpos = x;
  *ypos = y;
}
