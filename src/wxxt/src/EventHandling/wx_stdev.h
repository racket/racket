/*								-*- C++ -*-
 * File:		wx_stdev.h
 * Purpose:	Standard wxWindows event classes
 * Author:		Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wxb_stdevh
#define wxb_stdevh

#ifndef MZ_PRECISE_GC
# ifdef __GNUG__
# pragma interface
# endif
#endif

// Item or menu event class
class wxScrollEvent: public wxEvent
{
 public:
  wxScrollEvent();

  int pos;
  int moveType;
  int direction;
};


// Item or menu event class
class wxCommandEvent: public wxEvent
{
 public:
  wxCommandEvent(WXTYPE commandType = 0);
  inline ~wxCommandEvent(void) {}
};

class wxPopupEvent: public wxCommandEvent
{
public:
  wxPopupEvent();
  long menuId;
};

// Mouse event class
class wxMouseEvent: public wxEvent
{
 public:
  int x;
  int y;
  Bool leftDown;
  Bool middleDown;
  Bool rightDown;

  Bool controlDown;
  Bool shiftDown;
  Bool altDown;
  Bool metaDown;
  
  wxMouseEvent(WXTYPE mouseType = 0);

  // Was it a button event?
  virtual Bool IsButton(void);

  // Was it a down event from button 1, 2 or 3 or any?
  virtual Bool ButtonDown(int but = -1);

  // Was it a dclick event from button 1, 2 or 3 or any?
  virtual Bool ButtonDClick(int but = -1);

  // Was it a up event from button 1, 2 or 3 or any?
  virtual Bool ButtonUp(int but = -1);

  // Was the given button 1,2,3 or any changing state?
  virtual Bool Button(int but);

  // Was the given button 1,2,3 or any in Down state?
  virtual Bool ButtonIsDown(int but);

  // Find state of shift/control keys
  virtual Bool ControlDown(void);
  virtual Bool MetaDown(void);
  virtual Bool AltDown(void);
  virtual Bool ShiftDown(void);

  // Find which event was just generated
  virtual Bool LeftDown(void);
  virtual Bool MiddleDown(void);
  virtual Bool RightDown(void);

  virtual Bool LeftUp(void);
  virtual Bool MiddleUp(void);
  virtual Bool RightUp(void);

  virtual Bool LeftDClick(void);
  virtual Bool MiddleDClick(void);
  virtual Bool RightDClick(void);

  // Find the current state of the mouse buttons (regardless
  // of current event type)
  virtual Bool LeftIsDown(void);
  virtual Bool MiddleIsDown(void);
  virtual Bool RightIsDown(void);

  // True if a button is down and the mouse is moving
  virtual Bool Dragging(void);

  // True if the mouse is moving, and no button is down
  virtual Bool Moving(void);

  // True if the mouse is just entering the window
  virtual Bool Entering(void);

  // True if the mouse is just leaving the window
  virtual Bool Leaving(void);

  // Find the position of the event
  virtual void Position(int *x, int *y);
};

// Keyboard input event class
class wxKeyEvent: public wxEvent
{
 public:
  int x ;
  int y ;
  long keyCode;
  long keyUpCode;
  Bool controlDown;
  Bool shiftDown;
  Bool altDown;
  Bool metaDown;
  
  wxKeyEvent(WXTYPE keyType = 0);

  virtual Bool ControlDown(void);
  virtual Bool AltDown(void);
  virtual Bool MetaDown(void);
  virtual Bool ShiftDown(void);
  virtual long KeyCode(void);

  virtual void Position(int *x,int *y) ;
};

#endif // wxb_stdevh

