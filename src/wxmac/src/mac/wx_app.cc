////////////////////////////////////////////////////////////////////////////////
// File:	wx_app.cc
// Purpose:	wxApp implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
////////////////////////////////////////////////////////////////////////////////

#include "wx_main.h"
#include "wx_frame.h"
#include "wx_menu.h"
#include "wx_mnuit.h"
#include "wx_utils.h"
#include "wx_gdi.h"
#include "wx_stdev.h"
#include "wx_screen.h"
#include "wx_area.h"
#include "wx_timer.h"
#include "wx_dialg.h"
#include "wx_cmdlg.h"
#include "wx_dccan.h"
#include "wxTimeScale.h"
#include "wx_print.h"
#include "wx_macevents.h"

#include <stdlib.h>

extern void wxDoEvents();
extern void wxDoNextEvent();
extern int wxEventReady();

extern void wxDrop_Quit();
extern void wxDo_Pref();
extern void wxDo_About();

extern CGrafPtr gMacFontGrafPort;

extern int wxNumHelpItems;

extern Bool doCallPreMouseEvent(wxWindow *in_win, wxWindow *win, wxMouseEvent *evt);

extern WindowPtr MrEdMouseWindow(Point where);
extern WindowPtr MrEdKeyWindow();

extern void wxCheckRootFrame(WindowPtr w);
extern void wxMouseEventHandled(void);

int wxTranslateRawKey(int key);

int wxMenuBarHeight;

extern int wx_leave_all_input_alone;

extern wxApp *wxTheApp;
//-----------------------------------------------------------------------------
wxApp::wxApp():wxbApp()
{
  OSErr myErr;
  long quickdrawVersion;
  long windowMgrAttributes;
  long dontcare;
  wxArea* menuArea;
  
  wxREGGLOB(wxTheApp);
  wxTheApp = this;

  wxREGGLOB(wxWindow::gMouseWindow);

  if (::Gestalt(gestaltQuickdrawVersion, &quickdrawVersion) != noErr) {
    wxFatalError("Unable to invoke the Gestalt Manager.", "");
  } else {
    if ((quickdrawVersion >> 8) == 0) {
      wxFatalError("Color QuickDraw is required, but not present.", "");
    }
  }
  
  // Version 1.0 of the Appearance Manager appeared in 8.0. We don't support pre-8.0
  // any more.
  myErr = ::Gestalt(gestaltAppearanceAttr, &dontcare);
  if (myErr == gestaltUndefSelectorErr) {
    wxFatalError("Version 1.0 of the Appearance Manager is required.", "");
  }
  
  
  myErr = ::Gestalt(gestaltWindowMgrAttr, &windowMgrAttributes);
  if (myErr == gestaltUndefSelectorErr) {
    MacOS85WindowManagerPresent = FALSE;
  } else if (myErr == noErr) {
    if (windowMgrAttributes & gestaltWindowMgrPresent) {
      MacOS85WindowManagerPresent = TRUE;
    } else {
      MacOS85WindowManagerPresent = FALSE;
    } 
  } else {
    wxFatalError("Unable to invoke the Gestalt Manager.", "");
  }
  
  ::FlushEvents(everyEvent, 0);

  wxREGGLOB(wxScreen::gScreenWindow);
  {
    wxScreen *sc;
    sc = new WXGC_PTRS wxScreen;
    wxScreen::gScreenWindow = sc;
  }
  menuArea = (wxScreen::gScreenWindow)->MenuArea();
  wxMenuBarHeight = GetMBarHeight();
  menuArea->SetMargin(wxMenuBarHeight, wxTop);

  wx_frame = NULL;
  death_processed = FALSE;
  work_proc = NULL;
  wx_class = NULL;
  cLastMousePos.v = cLastMousePos.h = -1;  
  
  wxInitGL();
}

//-----------------------------------------------------------------------------
wxApp::~wxApp(void)
{
}

//-----------------------------------------------------------------------------
Bool wxApp::Initialized(void)
{
  return (wx_frame != NULL);
}

extern void wxSetUpAppleMenu(wxMenuBar *mbar);
extern void wxCheckFinishedSounds(void);

//-----------------------------------------------------------------------------
// Keep trying to process messages until WM_QUIT received
//-----------------------------------------------------------------------------

int wxApp::MainLoop(void)
{
  keep_going = TRUE;
  
  while (1) { wxDoEvents(); }

  return 0;
}


void wxApp::ExitMainLoop(void)
{
  death_processed = TRUE; 
  keep_going = FALSE;
}

//-----------------------------------------------------------------------------
Bool wxApp::Pending(void)
{
  return wxEventReady();
}

//-----------------------------------------------------------------------------
void wxApp::DoIdle(void)
{
  AdjustCursor();
}

//-----------------------------------------------------------------------------
void wxApp::Dispatch(void)
{
  wxDoNextEvent();
}

static wxFrame *oldFrontWindow = NULL;

void wxRegisterOldFrontWindow();

void wxRegisterOldFrontWindow()
{
  wxREGGLOB(oldFrontWindow);
}

static MenuHandle m129 = NULL;

void wxApp::doMacPreEvent()
{
  static Bool noWinMode = FALSE;
  WindowPtr w;

  w = FrontNonFloatingWindow();
  wxCheckRootFrame(w);
  if (!w) {
    /* Maybe we just showed the root frame: */
    w = FrontNonFloatingWindow();
  }

  wxCheckFinishedSounds();

  if (!w && !noWinMode) {
    wxPrepareMenuDraw();
    ::ClearMenuBar();
    wxSetUpAppleMenu(NULL);
    {
      if (!m129)
	m129 = GetMenu(129);
      if (m129) {
	::InsertMenu(m129, 0);
      }
    }
    ::InvalMenuBar();
    wxDoneMenuDraw();
    wxSetCursor(wxSTANDARD_CURSOR);
    noWinMode = TRUE;
    oldFrontWindow = NULL;
  } else if (w && noWinMode)
    noWinMode = FALSE;

  if (w) {
    wxFrame* macWxFrame;
    macWxFrame = findMacWxFrame(w);
    
    /* If this is the root frame, try to move it behind everything
       else.  If there is any other window, the root frame shouldn't
       be frontmost. */
    if (macWxFrame && (macWxFrame == wxRootFrame)) {
      wxMacDC *dc;
      CGrafPtr graf;
      dc = wxRootFrame->MacDC();
      graf = dc->macGrafPort();
      ::SendBehind(GetWindowFromPort(graf), NULL);
      w = FrontNonFloatingWindow();
      macWxFrame = findMacWxFrame(w);
    }

    if (macWxFrame) {
      wxWindow* focusWindow;
      
      if (oldFrontWindow != macWxFrame) {
	oldFrontWindow->NowFront(FALSE);
	macWxFrame->NowFront(TRUE);
	oldFrontWindow = macWxFrame;
      }
      
      focusWindow = macWxFrame->cFocusWindow;
      if (focusWindow)
	{
	  focusWindow->DoPeriodicAction();
	}
    }
  }
}

void wxApp::doMacPostEvent()
{
  DoIdle();
}

void wxApp::doMacDispatch(EventRecord *e)
{
  memcpy(&cCurrentEvent, e, sizeof(EventRecord));

  switch (e->what)
    {
    case mouseMenuDown:
    case mouseDown:
      doMacMouseDown(); break;
    case mouseUp:
      doMacMouseUp(); break;
    case keyDown:
    case wheelEvt:
    case unicodeEvt:
      doMacKeyDown(); break;
    case autoKey:
      doMacAutoKey(); break;
    case keyUp:
      doMacKeyUp(); break;
    case activateEvt:
      doMacActivateEvt(); break;
    case updateEvt:
      doMacUpdateEvt(); break;
    case diskEvt:
      doMacDiskEvt(); break;
    case osEvt:
      doMacOsEvt(); break;
    case kHighLevelEvent:
      doMacHighLevelEvent(); break;
    case leaveEvt:
      doMacMouseLeave(); break;
    case nullEvent:
      if (e->message) {
	doMacMouseMotion();
      }
      break;
    default:
      break;
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacMouseDown(void)
{
  WindowPtr window;
  short windowPart;

  windowPart = FindWindow(cCurrentEvent.where, &window);
  if (!window)
    window = FrontNonFloatingWindow();

  /* Check whether this window is blocked by a modal dialog: */
  {
    wxFrame *f;
    
    f = findMacWxFrame(window);
    if (f) {
      wxFrame *modal;
      
      modal = (wxFrame *)wxGetModalWindow(f);
      if (modal && (modal != f)) {
	/* Make sure the modal window is frontmost: */
	WindowPtr w;

	w = modal->macWindow();
	if (w != FrontNonFloatingWindow()) {
	  ::SelectWindow(w);
	}

#ifdef OS_X
	/* It's possible that the modal window is a sheet
	   in f... allow move operations. */
	if (((windowPart == inDrag) || (windowPart == inCollapseBox))
	    && (f->sheet == modal))
	  modal = NULL;
#endif

	if (modal) {
          wxMouseEventHandled();
	  SysBeep(0);
	  return;
	}
      }
    }
  }

  if ((windowPart != inContent)
      && (windowPart != inMenuBar)) {
    /* We've gotten far enough handling the mouse-down event that
       mouse-up events are ok to receive again: */
    wxMouseEventHandled();
  }

  switch (windowPart)
    {
    case inMenuBar:
      /* This code used to be guarded by a StillDown() check, but the
	 check doesn't seem to be useful or necessary. In fact, it
	 interfered with Ctl-F2 menu activation. */
      {
	long menuResult;
	WindowPtr theMacWindow;

	theMacWindow = FrontNonFloatingWindow();

	/* Give the menu bar a chance to build on-demand items: */
	if (theMacWindow) {
	  wxFrame* theMacWxFrame;
	  theMacWxFrame = findMacWxFrame(theMacWindow);
	  if (theMacWxFrame)
	    theMacWxFrame->OnMenuClick();
	}

        wxMouseEventHandled();

	wxTracking();
	wxPrepareMenuDraw();
        wx_leave_all_input_alone++;
	menuResult = MenuSelect(cCurrentEvent.where);
        --wx_leave_all_input_alone;
	wxDoneMenuDraw(!!menuResult);
	doMacInMenuBar(menuResult, FALSE);
      }
      break;
    case inContent:
      doMacInContent(window); 
      wxMouseEventHandled();
      break;
    case inDrag:
      doMacInDrag(window); 
      break;
    case inGrow:
      doMacInGrow(window);
      break;
    case inGoAway:
      doMacInGoAway(window);
      break;
    case inCollapseBox:
      {
	wxTracking();
	if (TrackBox(window, cCurrentEvent.where, inCollapseBox))
	  CollapseWindow(window, TRUE);
      }
      break;
#ifdef OS_X
    case inToolbarButton:
      {
	wxTracking();
	if (TrackBox(window, cCurrentEvent.where, inToolbarButton)) {
	  {
	    wxFrame* theMacWxFrame;
	    theMacWxFrame = findMacWxFrame(window);
	    if (theMacWxFrame)
	      theMacWxFrame->OnToolbarButton();
	  } 
	}
      }
      break;
#endif
    case inZoomIn:
    case inZoomOut:
      doMacInZoom(window, windowPart); 
      break;
    default:
      break;
    }
}

#define rightButtonKey 

//-----------------------------------------------------------------------------
void wxApp::doMacMouseUp(void)
{
  wxWindow* mouseWindow = wxWindow::gMouseWindow;
  if (mouseWindow)
    {
      int hitX = cCurrentEvent.where.h; // screen window c.s.
      int hitY = cCurrentEvent.where.v; // screen window c.s.
      wxMouseEvent *theMouseEvent;
      Bool rightButton = cCurrentEvent.modifiers & controlKey;
      int type = rightButton ? wxEVENT_TYPE_RIGHT_UP : wxEVENT_TYPE_LEFT_UP;

      mouseWindow->ScreenToClient(&hitX, &hitY); // mouseWindow client c.s.
      
      theMouseEvent = new WXGC_PTRS wxMouseEvent(type);
      theMouseEvent->leftDown = FALSE;
      theMouseEvent->middleDown = FALSE;
      theMouseEvent->rightDown = FALSE;
      theMouseEvent->shiftDown = cCurrentEvent.modifiers & shiftKey;
      theMouseEvent->controlDown = FALSE;
      theMouseEvent->altDown = cCurrentEvent.modifiers & optionKey;
      theMouseEvent->metaDown = cCurrentEvent.modifiers & cmdKey;
      theMouseEvent->capsDown = cCurrentEvent.modifiers & alphaLock;
      theMouseEvent->x = hitX;
      theMouseEvent->y = hitY;
      theMouseEvent->timeStamp = SCALE_TIMESTAMP(cCurrentEvent.when);
      
      /* Grab is now only used for grabbing on mouse-down for canvases & panels: */
      if (wxSubType(mouseWindow->__type, wxTYPE_CANVAS) 
	  || wxSubType(mouseWindow->__type, wxTYPE_PANEL))
	mouseWindow->ReleaseMouse();
      
      if (!doCallPreMouseEvent(mouseWindow, mouseWindow, theMouseEvent))
	if (!mouseWindow->IsGray())
	  mouseWindow->OnEvent(theMouseEvent);
    }
  else
    {
      wxFrame* macWxFrame;
      macWxFrame = findMacWxFrame(MrEdMouseWindow(cCurrentEvent.where));
      if (macWxFrame)
	{
	  int hitX = cCurrentEvent.where.h; // screen window c.s.
	  int hitY = cCurrentEvent.where.v; // screen window c.s.
	  wxArea* frameParentArea;
	  wxMouseEvent *theMouseEvent;
	  Bool rightButton;
	  int type, metal_drag_ok = 1;

	  frameParentArea = macWxFrame->ParentArea();
	  frameParentArea->ScreenToArea(&hitX, &hitY);

	  // RightButton is cmdKey click  on the mac platform for one-button mouse
	  rightButton = cCurrentEvent.modifiers & controlKey;
	  type = rightButton ? wxEVENT_TYPE_RIGHT_UP : wxEVENT_TYPE_LEFT_UP;
	  
	  theMouseEvent = new WXGC_PTRS wxMouseEvent(type);
	  
	  theMouseEvent->leftDown = FALSE;
	  theMouseEvent->middleDown = FALSE;
	  theMouseEvent->rightDown = FALSE;
	  theMouseEvent->shiftDown = cCurrentEvent.modifiers & shiftKey;
	  theMouseEvent->controlDown = FALSE;
	  // altKey is optionKey on the mac platform:
	  theMouseEvent->altDown = cCurrentEvent.modifiers & optionKey;
	  theMouseEvent->metaDown = cCurrentEvent.modifiers & cmdKey;
	  theMouseEvent->capsDown = cCurrentEvent.modifiers & alphaLock;
	  theMouseEvent->x = hitX;
	  theMouseEvent->y = hitY;
	  theMouseEvent->timeStamp = SCALE_TIMESTAMP(cCurrentEvent.when);

	  macWxFrame->SeekMouseEventArea(theMouseEvent, &metal_drag_ok);
	}
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacMouseMotion(void)
{
  // RightButton is controlKey click on the mac platform for one-button mouse
  Bool isRightButton = cCurrentEvent.modifiers & controlKey;
  // altKey is optionKey on the mac platform:
  Bool isAltKey = cCurrentEvent.modifiers & optionKey;
  Bool isMouseDown = (cCurrentEvent.modifiers & btnState);
  wxMouseEvent *theMouseEvent;

  theMouseEvent = new WXGC_PTRS wxMouseEvent(wxEVENT_TYPE_MOTION);
  theMouseEvent->leftDown = isMouseDown && !isRightButton;
  theMouseEvent->middleDown = FALSE;
  theMouseEvent->rightDown = isMouseDown && isRightButton;
  theMouseEvent->shiftDown = cCurrentEvent.modifiers & shiftKey;
  theMouseEvent->controlDown = FALSE;
  theMouseEvent->altDown = isAltKey;
  theMouseEvent->metaDown = cCurrentEvent.modifiers & cmdKey;
  theMouseEvent->capsDown = cCurrentEvent.modifiers & alphaLock;
  theMouseEvent->timeStamp = SCALE_TIMESTAMP(cCurrentEvent.when);
  
  if (wxWindow::gMouseWindow)
    {
      int hitX, hitY;
      wxWindow* mouseWindow = wxWindow::gMouseWindow;
      
      hitX = cCurrentEvent.where.h; // screen window c.s.
      hitY = cCurrentEvent.where.v; // screen window c.s.
      mouseWindow->ScreenToClient(&hitX, &hitY); // mouseWindow client c.s.
      theMouseEvent->x = hitX;
      theMouseEvent->y = hitY;

      /* Grab is now only used for grabbing on mouse-down for canvases & panels: */
      if ((wxSubType(mouseWindow->__type, wxTYPE_CANVAS) 
	   || wxSubType(mouseWindow->__type, wxTYPE_PANEL))
	  && !isMouseDown)
	mouseWindow->ReleaseMouse();
      
      if (!doCallPreMouseEvent(mouseWindow, mouseWindow, theMouseEvent))		  
	if (!mouseWindow->IsGray())
	  mouseWindow->OnEvent(theMouseEvent);
    }
  else
    {
      wxFrame* macWxFrame;
      macWxFrame = findMacWxFrame(MrEdMouseWindow(cCurrentEvent.where));
      if (macWxFrame)
	{
	  int hitX = cCurrentEvent.where.h; // screen window c.s.
	  int hitY = cCurrentEvent.where.v; // screen window c.s.
	  int metal_drag_ok = 1;
	  wxArea* frameParentArea;
	  frameParentArea = macWxFrame->ParentArea();
	  frameParentArea->ScreenToArea(&hitX, &hitY);
	  theMouseEvent->x = hitX; // frame parent area c.s.
	  theMouseEvent->y = hitY; // frame parent area c.s.

	  macWxFrame->SeekMouseEventArea(theMouseEvent, &metal_drag_ok);
	}
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacMouseLeave(void)
{
  // RightButton is controlKey click on the mac platform for one-button mouse
  Bool isRightButton = cCurrentEvent.modifiers & controlKey;
  // altKey is optionKey on the mac platform:
  Bool isAltKey = cCurrentEvent.modifiers & optionKey;
  Bool isMouseDown = (cCurrentEvent.modifiers & btnState);
  wxMouseEvent *theMouseEvent;
  wxWindow* win;
  void *rc;

  theMouseEvent = new WXGC_PTRS wxMouseEvent(wxEVENT_TYPE_LEAVE_WINDOW);
  theMouseEvent->leftDown = isMouseDown && !isRightButton;
  theMouseEvent->middleDown = FALSE;
  theMouseEvent->rightDown = isMouseDown && isRightButton;
  theMouseEvent->shiftDown = cCurrentEvent.modifiers & shiftKey;
  theMouseEvent->controlDown = FALSE;
  theMouseEvent->altDown = isAltKey;
  theMouseEvent->metaDown = cCurrentEvent.modifiers & cmdKey;
  theMouseEvent->capsDown = cCurrentEvent.modifiers & alphaLock;
  theMouseEvent->timeStamp = SCALE_TIMESTAMP(cCurrentEvent.when);
  
  rc = (void *)cCurrentEvent.message;
  win = (wxWindow*)GET_SAFEREF(rc);
  if (win->IsShown()) {
    theMouseEvent->x = cCurrentEvent.where.h;
    theMouseEvent->y = cCurrentEvent.where.v;

    if (!doCallPreMouseEvent(win, win, theMouseEvent))
      if (!win->IsGray())
	win->OnEvent(theMouseEvent);
  }
}

//-----------------------------------------------------------------------------
// mflatt writes:
// Probably, this should be moved into an abstracted function so that
//   doMacKeyUp can call the same code.
// john clements writes:
// Abstraction performed, 2002-01-29.
// Note that wxKeyEvent objects have two extra fields: timeStamp and
//   metaDown. (On the Mac, metaDown is really commandDown.)

static Bool doPreOnChar(wxWindow *in_win, wxWindow *win, wxKeyEvent *evt)
{
  wxWindow *p;
  p = win->GetParent();
  return ((p && doPreOnChar(in_win, p, evt)) || win->PreOnChar(in_win, evt));
}

short wxMacDisableMods; /* If a modifier key is here, handle it specially */

static void *uchrPtr;
static void *KCHRPtr;
static UInt32 key_state;
static int keyLayoutSet;
static SInt16 lastKeyLayoutID;

void wxApp::doMacKeyUpDown(Bool down)
{
  wxFrame* theMacWxFrame;
  wxKeyEvent *theKeyEvent;
  int key, otherKey = 0, optKey = 0, otherOptKey = 0, capsKey = 0;

  theMacWxFrame = findMacWxFrame(MrEdKeyWindow());
  
  if (down) {
    if (!theMacWxFrame || theMacWxFrame->CanAcceptEvent())
      if (cCurrentEvent.modifiers & cmdKey) { // is menu command key equivalent ?
	if (cCurrentEvent.what == keyDown) { // ignore autoKey
	  long menuResult;
	  wxPrepareMenuDraw();
	  menuResult = MenuEvent(&cCurrentEvent);
	  wxDoneMenuDraw(!!menuResult);
	  if (menuResult) {
	    if (doMacInMenuBar(menuResult, TRUE)) {
	      return;
	    } else {
	      wxPrepareMenuDraw();
	      // HiliteMenu(0); // calling wxPrepareMenuDraw unhlites the menu
	      wxDoneMenuDraw();
	    }
	  }
	}
      }
  }    
  
  if (!theMacWxFrame || !theMacWxFrame->IsEnable())
    return;	

  theKeyEvent = new WXGC_PTRS wxKeyEvent(wxEVENT_TYPE_CHAR);
  theKeyEvent->x = cCurrentEvent.where.h;
  theKeyEvent->y = cCurrentEvent.where.v;
  theKeyEvent->controlDown = Bool(cCurrentEvent.modifiers & controlKey);
  theKeyEvent->shiftDown = Bool(cCurrentEvent.modifiers & shiftKey);
  // altKey is optionKey on the mac platform:
  theKeyEvent->altDown = Bool(cCurrentEvent.modifiers & optionKey);
  theKeyEvent->metaDown = Bool(cCurrentEvent.modifiers & cmdKey);
  theKeyEvent->capsDown = Bool(cCurrentEvent.modifiers & alphaLock);
  theKeyEvent->timeStamp = SCALE_TIMESTAMP(cCurrentEvent.when);

  if (cCurrentEvent.what == wheelEvt) {
    if (cCurrentEvent.message)
      key = WXK_WHEEL_UP;
    else
      key = WXK_WHEEL_DOWN;
  } else if (cCurrentEvent.what == unicodeEvt) {
    key = cCurrentEvent.message;
  } else {
    int newkey;

    key = (cCurrentEvent.message & keyCodeMask) >> 8;
    newkey = wxTranslateRawKey(key);

    if (newkey) {
      key = newkey;
    } else {
      int iter, akey, orig_key = key;

      key = 0; /* let compiler know that key is assigned */
      for (iter = 0; iter < ((cCurrentEvent.modifiers & cmdKey) ? 5 : 1); iter++) {
        char cstr[3];
        int from_str = 0;

        akey = orig_key;

        if (cCurrentEvent.modifiers & (wxMacDisableMods | cmdKey)) {
          /* The following code manually translates the virtual key event
             into a character. We'd use this code all the time, except
             that dead keys have already been filtered before we get here,
             which means that option-e-e doesn't produce an accented e.
             So, instead, we only use this code to find out what would
             happen if the control/option key wasn't pressed. */
          int mods;
          OSStatus status;
          UniCharCount len;
          UniChar keys[1];
          SInt16 currentKeyLayoutID;
          static UCKeyboardLayout *key_layout;

          mods = cCurrentEvent.modifiers;

          /* Strip Caps Lock when Control is pressed. */
          if (mods & (controlKey & wxMacDisableMods))
            mods -= (mods & alphaLock);

          if (mods & cmdKey) {
            int mask;
            /* Strip control modifier when command is pressed: */
            mods -= (mods & (controlKey | cmdKey));
            if (iter && (iter != 4)) {
              mods -= (mods & alphaLock);
            }
            /* On all but first iteration, toggle shift and/or option: */
            switch (iter) {
            case 0:
              mask = 0;
              break;
            case 1:
              mask = shiftKey;
              break;
            case 2:
              mask = optionKey;
              break;
            case 3:
              mask = optionKey | shiftKey;
              break;
            default:
            case 4:
              mask = alphaLock;
              break;
            }
            mods = (mods & (~mask)) | ((~mods) & mask);
          } else {
            /* Remove effect of anything in wxMacDisableMods: */
            mods -= (mods & wxMacDisableMods);
          }

          currentKeyLayoutID = GetScriptVariable(GetScriptManagerVariable(smKeyScript), smScriptKeys);
          if (!keyLayoutSet || (currentKeyLayoutID != lastKeyLayoutID)) {
            KeyboardLayoutRef kl;
            key_state = 0;
            if (KLGetCurrentKeyboardLayout(&kl) == noErr) {
              void *p;
              if (KLGetKeyboardLayoutProperty(kl, kKLKCHRData, (const void **)&p) == noErr) {
                KCHRPtr = p;
              } else
                KCHRPtr = NULL;
              if (KLGetKeyboardLayoutProperty(kl, kKLuchrData, (const void **)&p) == noErr)
                uchrPtr = p;
              else
                uchrPtr = NULL;
            }
            lastKeyLayoutID = currentKeyLayoutID;
            keyLayoutSet = 1;
          }

          if (!uchrPtr) {
            if (!KCHRPtr) {
              akey = '?';
            } else {
              int trans;
              trans = KeyTranslate(KCHRPtr, akey | mods, &key_state);
              if (trans & 0xFF0000) {
                /* 2-byte result */
                cstr[0] = (trans & 0xFF0000) >> 16;
                cstr[1] = trans & 0xFF;
                cstr[2] = 0;
              } else {
                /* 1-byte result */
                cstr[0] = trans & 0xFF;
                cstr[1] = 0;
              }

              akey = '?'; /* temporary */
              from_str = 1;
            }
          } else {
            key_layout = (UCKeyboardLayout *)uchrPtr;

            status = UCKeyTranslate(key_layout,
                                    akey,
                                    cCurrentEvent.what - keyDown,
                                    mods >> 8,
                                    LMGetKbdType(),
                                    0 /* options */,
                                    &key_state,
                                    1,
                                    &len,
                                    keys);

            if (status == noErr)
              akey = keys[0];
            else
              akey = '?';
          }
        } else {
          akey = '?'; /* temporary */
          cstr[0] = cCurrentEvent.message & charCodeMask;
          cstr[1] = 0;
          from_str = 1;
        }

        if (from_str) {
          CFStringRef str;
          UniChar keys[1];
  
          str = CFStringCreateWithCStringNoCopy(NULL, cstr,
                                                GetScriptManagerVariable(smKeyScript),
                                                kCFAllocatorNull);
          if (str) {
            if (CFStringGetLength(str) > 0) {
              GC_CAN_IGNORE CFRange rng;
              rng = CFRangeMake(0, 1);
              CFStringGetCharacters(str, rng, keys);
            } else
              keys[0] = '?';
            CFRelease(str);
          } else
            keys[0] = '?';

          akey = keys[0];
        }

        if (!iter)
          key = akey;
        else if (iter == 1)
          otherKey = akey;
        else if (iter == 2)
          optKey = akey;
        else if (iter == 3)
          otherOptKey = akey;
        else if (iter == 4)
          capsKey = akey;
      }
    }
  }

  if (down) {
    theKeyEvent->keyCode = key;
    theKeyEvent->keyUpCode = WXK_PRESS;
  } else {
    theKeyEvent->keyCode = WXK_RELEASE;
    theKeyEvent->keyUpCode = key;
  }  
  theKeyEvent->otherKeyCode = otherKey;
  theKeyEvent->altKeyCode = optKey;
  theKeyEvent->otherAltKeyCode = otherOptKey;
  theKeyEvent->capsKeyCode = capsKey;

  {
    wxWindow *in_win;
    
    in_win = theMacWxFrame->GetFocusWindow();

    if (!in_win)
      in_win = theMacWxFrame;
    
    if (!doPreOnChar(in_win, in_win, theKeyEvent))
      if (!theMacWxFrame->IsGray())
	theMacWxFrame->OnChar(theKeyEvent);
  }
}

void wxApp::doMacKeyDown(void)
{
  doMacKeyUpDown(true);
}

void wxApp::doMacKeyUp(void)
{
  doMacKeyUpDown(false);
}

void wxApp::doMacAutoKey(void)
{
  doMacKeyUpDown(true);
}

int wxTranslateRawKey(int key)
{
  /* Better way than to use hard-wired key codes? */
  switch (key) {
#   define wxFKEY(code, wxk)  case code: key = wxk; break
    wxFKEY(122, WXK_F1);
    wxFKEY(120, WXK_F2);
    wxFKEY(99, WXK_F3);
    wxFKEY(118, WXK_F4);
    wxFKEY(96, WXK_F5);
    wxFKEY(97, WXK_F6);
    wxFKEY(98, WXK_F7);
    wxFKEY(100, WXK_F8);
    wxFKEY(101, WXK_F9);
    wxFKEY(109, WXK_F10);
    wxFKEY(103, WXK_F11);
    wxFKEY(111, WXK_F12);
    wxFKEY(105, WXK_F13);
    wxFKEY(107, WXK_F14);
    wxFKEY(113, WXK_F15);
  case 0x7e:
  case 0x3e:
    key = WXK_UP;
    break;
  case 0x7d:
  case 0x3d:
    key = WXK_DOWN;
    break;
  case 0x7b:
  case 0x3b:
    key = WXK_LEFT;
    break;
  case 0x7c:
  case 0x3c:
    key = WXK_RIGHT;
    break;
  case 0x24:
    key = WXK_RETURN;
    break;
  case 0x30:
    key = WXK_TAB;
    break;
  case 0x33:
    key = WXK_BACK;
    break;
  case 0x75:
    key = WXK_DELETE;
    break;
  case 0x73:
    key = WXK_HOME;
    break;
  case 0x77:
    key = WXK_END;
    break;   
  case 0x74:
    key = WXK_PRIOR;
    break;     
  case 0x79:
    key = WXK_NEXT;
    break;
  case 0x45:
    key = WXK_ADD;
    break;
  case 78:
    key = WXK_SUBTRACT;
    break;
  case 0x43:
    key = WXK_MULTIPLY;
    break;
  case 0x4B:
    key = WXK_DIVIDE;
    break;
  case 71:
    key = WXK_SEPARATOR;
    break;
  case 65:
    key = WXK_DECIMAL;
    break;
  case 76:
    key = 3; /* numpad enter */
    break;
  case 82:
  case 83:
  case 84:
  case 85:
  case 86:
  case 87:
  case 88:
  case 89:
    key = WXK_NUMPAD0 + (key - 82);
    break;
  case 91:
    key = WXK_NUMPAD8;
    break;
  case 92:
    key = WXK_NUMPAD9;
    break;
  default:
    key = 0;
    break;
  }

  return key;
}

//-----------------------------------------------------------------------------
void wxApp::doMacActivateEvt(void)
{
  WindowPtr theMacWindow;
  wxFrame* theMacWxFrame;
  theMacWindow = WindowPtr(cCurrentEvent.message);
  theMacWxFrame = findMacWxFrame(theMacWindow);
  if (theMacWxFrame) {
    Bool becomingActive = cCurrentEvent.modifiers & activeFlag;
    theMacWxFrame->Activate(becomingActive);
  }
}

//-----------------------------------------------------------------------------
void wxApp::doMacUpdateEvt(void)
{
  WindowPtr theMacWindow;
  wxFrame* theMacWxFrame;
  theMacWindow = (WindowPtr)cCurrentEvent.message;
  theMacWxFrame = findMacWxFrame(theMacWindow);
  if (theMacWxFrame) {
    theMacWxFrame->MacUpdateWindow();
  } else {
    BeginUpdate(theMacWindow);
    EndUpdate(theMacWindow);
  }
}

//-----------------------------------------------------------------------------
void wxApp::doMacDiskEvt(void)
{
}

//-----------------------------------------------------------------------------
void wxApp::doMacOsEvt(void)
{ // based on "Programming for System 7" by Gary Little and Tim Swihart
  switch ((cCurrentEvent.message >> 24) & 0x0ff)
    {
    case suspendResumeMessage:
      if (cCurrentEvent.message & resumeFlag)
	{
	  doMacResumeEvent();
	}
      else
	{
	  doMacSuspendEvent();
	}
      break;
    case mouseMovedMessage:
      doMacMouseMovedMessage();
      break;
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacHighLevelEvent(void)
{ // based on "Programming for System 7" by Gary Little and Tim Swihart
  ::AEProcessAppleEvent(&cCurrentEvent); // System 7 or higher
}

//-----------------------------------------------------------------------------
void wxApp::doMacResumeEvent(void)
{
  wxFrame* theMacWxFrame;
  theMacWxFrame = findMacWxFrame(FrontNonFloatingWindow());
  if (theMacWxFrame)
    {
#ifndef WX_CARBON
      if (cCurrentEvent.message & convertClipboardFlag)
	::TEFromScrap();
#endif                        
      Bool becomingActive = TRUE;
      theMacWxFrame->Activate(becomingActive);
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacSuspendEvent(void)
{
  wxFrame* theMacWxFrame;
  theMacWxFrame = findMacWxFrame(FrontNonFloatingWindow());
  if (theMacWxFrame)
    {
      Bool becomingActive = TRUE;
#ifdef WX_CARBON
      ClearCurrentScrap();
#else                
      ::ZeroScrap();
#endif                
      ::TEToScrap();
      theMacWxFrame->Activate(!becomingActive);
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacMouseMovedMessage(void)
{
}

//-----------------------------------------------------------------------------

static void UnhiliteMenu()
{
  wxPrepareMenuDraw();
  // HiliteMenu(0); // calling wxPrepareMenuDraw unhlites the menu
  wxDoneMenuDraw();
}

Bool wxApp::doMacInMenuBar(long menuResult, Bool externOnly)
{
  int macMenuId = HiWord(menuResult);
  int macMenuItemNum = LoWord(menuResult); // counting from 1
  WindowPtr theMacWindow;
  wxMenu* theWxMenu;
  wxFrame* theMacWxFrame;
  wxMenuBar* theWxMenuBar;
  wxNode* node;
  wxMenuItem* theWxMenuItem;
  
  if (macMenuId == 0) 					// no menu item selected;
    return FALSE;

  // Check for the standard menu items:
  {
    MenuRef mnu;
    MenuItemIndex idx;
    
    if (macMenuId == 128) {
      // Must be "About..."
      wxDo_About();
      UnhiliteMenu();
      return TRUE;
    }

    if (!GetIndMenuItemWithCommandID(NULL, 'quit', 1, &mnu, &idx)) {
      if ((macMenuId == GetMenuID(mnu)) && (macMenuItemNum == idx)) {
	wxDrop_Quit();
	UnhiliteMenu();
	return TRUE;
      }
    }
    
    if (!GetIndMenuItemWithCommandID(NULL, 'pref', 1, &mnu, &idx)) {
      if ((macMenuId == GetMenuID(mnu)) && (macMenuItemNum == idx)) {
	wxDo_Pref();
	UnhiliteMenu();
	return TRUE;
      }
    }
    
    if (!GetIndMenuItemWithCommandID(NULL, 'hide', 1, &mnu, &idx)) {
      if ((macMenuId == GetMenuID(mnu)) && (macMenuItemNum == idx)) {
	/* Hide application */
	
	UnhiliteMenu();
	return TRUE;
      }
    }
  }

  theMacWindow = FrontNonFloatingWindow();
  if (!theMacWindow) {
    // Must be quit
    exit(0);
    return TRUE;
  }

  theMacWxFrame = findMacWxFrame(theMacWindow);
  if (!theMacWxFrame) wxFatalError("No wxFrame for theMacWindow.");

  theWxMenuBar = theMacWxFrame->wx_menu_bar;
  if (!theWxMenuBar) {
    /* Must be the Close item. See wx_frame.cxx. */
    if (theMacWxFrame->IsModal()) {
      /* this is really a dialog */
      wxChildNode *node2;
      wxChildList *cl;
      cl = theMacWxFrame->GetChildren();
      node2 = cl->First();
      if (node2) {
	wxDialogBox *d;
	d = (wxDialogBox *)node2->Data();
	if (d) {
	  if (d->OnClose())
	    d->Show(FALSE);
	}
      }
    } else {
      if (theMacWxFrame->OnClose())
	theMacWxFrame->Show(FALSE);
    }
    UnhiliteMenu();
    return TRUE;
  }
  
  if (externOnly) {
    // Don't handle other keybindings automatically; in MrEd,
    //  they'll be handled by a frame's PreOnChar method
    return FALSE;
  }

  if (macMenuId == kHMHelpMenuID) {
    if (theWxMenuBar->wxHelpHackMenu) {
      theWxMenu = theWxMenuBar->wxHelpHackMenu;
      macMenuItemNum -= wxNumHelpItems;
    } else
      return TRUE;
  } else if (macMenuId == 128) {
    if (macMenuItemNum == 1) {
      // This will Help/About selection
      theWxMenu = theWxMenuBar->wxHelpHackMenu;
      if (theWxMenu && theWxMenuBar->iHelpMenuHackNum) {
	macMenuItemNum = theWxMenuBar->iHelpMenuHackNum;
      } else {
	wxDo_About();
	UnhiliteMenu();
	return TRUE;
      }
    } else
      return FALSE;
  } else {
    theWxMenu = theWxMenuBar->wxMacFindMenu(macMenuId);
  }
  if (!theWxMenu) wxFatalError("No wxMenu for wxMenuBar.");

  node = theWxMenu->menuItems->Nth(macMenuItemNum - 1); // counting from 0
  if (!node) wxFatalError("No wxNode for Nth menuItem.");

  theWxMenuItem = (wxMenuItem*) node->Data();
  if (!theWxMenuItem) wxFatalError("No wxMenuItem for wxNode.");

  if (theWxMenuItem->IsCheckable()) {
    theWxMenuItem->Check(!theWxMenuItem->IsChecked());
  }

  theMacWxFrame->ProcessCommand(theWxMenuItem->itemId);
  
  return TRUE;
}

//-----------------------------------------------------------------------------
void wxApp::doMacInContent(WindowPtr window)
{
  wxFrame* theMacWxFrame;
  theMacWxFrame = findMacWxFrame(window);
  if (theMacWxFrame) {
    doMacContentClick(theMacWxFrame);
  }
}

//-----------------------------------------------------------------------------
void wxApp::doMacContentClick(wxFrame* frame)
{
  // RightButton is controlKey click  on the mac platform for one-button mouse
  Bool rightButton = cCurrentEvent.modifiers & controlKey;
  // altKey is optionKey on the mac platform:
  Bool isAltKey = cCurrentEvent.modifiers & optionKey;
  wxMouseEvent *theMouseEvent;
  wxArea* frameParentArea;
  WXTYPE mouseEventType = rightButton ? wxEVENT_TYPE_RIGHT_DOWN : wxEVENT_TYPE_LEFT_DOWN;
  int hitX, hitY;
  int metal_drag_ok = 1;

  theMouseEvent = new WXGC_PTRS wxMouseEvent(mouseEventType);
  theMouseEvent->leftDown = !rightButton;
  theMouseEvent->middleDown = FALSE;
  theMouseEvent->rightDown = rightButton;
  theMouseEvent->shiftDown = cCurrentEvent.modifiers & shiftKey;
  theMouseEvent->controlDown = FALSE;
  theMouseEvent->altDown = isAltKey;
  theMouseEvent->metaDown = cCurrentEvent.modifiers & cmdKey;
  theMouseEvent->capsDown = cCurrentEvent.modifiers & alphaLock;
  theMouseEvent->timeStamp = SCALE_TIMESTAMP(cCurrentEvent.when);

  hitX = cCurrentEvent.where.h; // screen window c.s.
  hitY = cCurrentEvent.where.v; // screen window c.s.
  frameParentArea = frame->ParentArea();
  frameParentArea->ScreenToArea(&hitX, &hitY);
  theMouseEvent->x = hitX; // frame parent area c.s.
  theMouseEvent->y = hitY; // frame parent area c.s.

  // Sheets cause windows to move in lots of ways.
  // Best just to re-calculate the position before processing an event.
  frame->wxMacRecalcNewSize(FALSE);

  frame->SeekMouseEventArea(theMouseEvent, &metal_drag_ok);
}

//-----------------------------------------------------------------------------

static WindowPtr last_drag_click;
static unsigned long last_drag_click_time;

void wxApp::doMacInDrag(WindowPtr window)
{
  if (last_drag_click == window) {
    if ((cCurrentEvent.when != last_drag_click_time) // avoid bring-to-front redundancies
	&& (cCurrentEvent.when - last_drag_click_time <= GetDblTime())) {
      CollapseWindow(window, TRUE);
      return;
    }
  }
  last_drag_click = window;
  last_drag_click_time = cCurrentEvent.when;

  {
    wxFrame* theMacWxFrame;
    theMacWxFrame = findMacWxFrame(window);
    if (theMacWxFrame) {
      long oldx, oldy;
      oldx = theMacWxFrame->cWindowX;
      oldy = theMacWxFrame->cWindowY;
      
      theMacWxFrame->DragFrame(cCurrentEvent.where);
    
      if ((oldx != theMacWxFrame->cWindowX)
	  || (oldy != theMacWxFrame->cWindowY)) {
	last_drag_click = NULL;
	theMacWxFrame->OnSize(-1, -1);
      }
    }
  }
}

//-----------------------------------------------------------------------------
void wxApp::doMacInGrow(WindowPtr window)
{
  {
    wxFrame* theMacWxFrame;
    theMacWxFrame = findMacWxFrame(window);
    if (theMacWxFrame && theMacWxFrame->CanAcceptEvent()) {
      Rect growSizeRect; // WCH: growSizeRect should be a member of wxFrame class
      long windSize;

      wxTracking();

      theMacWxFrame->GetSizeLimits(&growSizeRect);

      windSize = ::GrowWindow(window, cCurrentEvent.where, &growSizeRect);
      if (windSize != 0) {
	wxArea* contentArea;
	int newContentWidth = LoWord(windSize);
	int newContentHeight = HiWord(windSize);
	contentArea = theMacWxFrame->ContentArea();
	if (newContentWidth == 0) newContentWidth = contentArea->Width(); // no change
	if (newContentHeight == 0) newContentHeight = contentArea->Height(); // no change
	contentArea->SetSize(newContentWidth, newContentHeight);
      }
    }
  }
}

//-----------------------------------------------------------------------------
void wxApp::doMacInGoAway(WindowPtr window)
{
  /* This one is used when a frame's application is in the foreground.
     The frame receives a close event through it's event handler
     when the application is in the background. */
  wxFrame* theMacWxFrame;
  theMacWxFrame = findMacWxFrame(window);
  if (theMacWxFrame && theMacWxFrame->CanAcceptEvent()) {
    wxTracking();
    if (TrackGoAway(window, cCurrentEvent.where)) {
      Bool okToDelete;
      okToDelete = theMacWxFrame->OnClose();
      if (okToDelete) {
	theMacWxFrame->Show(FALSE);
      }
    }
  }
}

//-----------------------------------------------------------------------------
void wxApp::doMacInZoom(WindowPtr window, short windowPart)
{
  { 
    wxFrame* theMacWxFrame;
    theMacWxFrame = findMacWxFrame(window);
    if (theMacWxFrame && theMacWxFrame->CanAcceptEvent()) {
      wxTracking();
      if (TrackBox(window, cCurrentEvent.where, windowPart)) {
	theMacWxFrame->Maximize(2);
      }
    }
  }
}

//-----------------------------------------------------------------------------
wxFrame* wxApp::findMacWxFrame(WindowPtr theMacWindow)
{
  wxFrame *fr;

  if (theMacWindow) {
    void *rc;
    rc = (void *)GetWRefCon(theMacWindow);
    if (rc)
      fr = (wxFrame *)GET_SAFEREF(rc);
    else
      fr = NULL;
  } else
    fr = NULL;

  return fr;
}

//-----------------------------------------------------------------------------
void wxApp::AdjustCursor(void)
{
  wxFrame* theMacWxFrame;
  theMacWxFrame = findMacWxFrame(MrEdMouseWindow(cCurrentEvent.where));
  if (theMacWxFrame) {
    if (theMacWxFrame->cBusyCursor)
      wxSetCursor(wxHOURGLASS_CURSOR);
    else {
      /* 
	 if (cCurrentEvent.what != kHighLevelEvent)
	 {
	 cLastMousePos.h = cCurrentEvent.where.h;
	 cLastMousePos.v = cCurrentEvent.where.v;
	 }
	 */
      Point p;
      wxArea* frameParentArea;
      int hitX, hitY;

      GetMouse(&p);
      LocalToGlobal(&p);
      hitX = p.h; // screen window c.s.
      hitY = p.v; // screen window c.s.
      frameParentArea = theMacWxFrame->ParentArea();
      frameParentArea->ScreenToArea(&hitX, &hitY);
      if (!theMacWxFrame->AdjustCursor(hitX, hitY))
	wxSetCursor(wxSTANDARD_CURSOR);
    }
  } else
    wxSetCursor(wxSTANDARD_CURSOR);
}

//-----------------------------------------------------------------------------
char *wxApp::GetDefaultAboutItemName(void)
{
  return "About wxWindows...";
}

void wxApp::DoDefaultAboutItem(void)
{
  wxMessageBox("This application was implemented with wxWindows,\n"
	       "Copyright 1993-94, AIAI, University of Edinburgh.\n"
	       "All Rights Reserved.",
	       "wxWindows");
}

//-----------------------------------------------------------------------------

void wxPrimDialogSetUp()
{
  wxUnhideCursor();
  wxSetCursor(wxSTANDARD_CURSOR);
  wx_leave_all_input_alone++;
}

void wxPrimDialogCleanUp()
{
  WindowPtr w;
  EventRecord event;

  --wx_leave_all_input_alone;

  wxTheApp->AdjustCursor();

  /* In case the front window wasn't active when we started: */
  w = FrontNonFloatingWindow();
  event.what = activateEvt;
  event.modifiers = activeFlag;
  event.message = (long)w;
  QueueMrEdEvent(&event);
}


//------------------------------------------------------------------------

int wxKeyCodeToVirtualKey(int wxk) {
  switch (wxk) {
#   define wxKEYF(code, wxk)  case wxk: return code
    wxKEYF(122, WXK_F1);
    wxKEYF(120, WXK_F2);
    wxKEYF(99, WXK_F3);
    wxKEYF(118, WXK_F4);
    wxKEYF(96, WXK_F5);
    wxKEYF(97, WXK_F6);
    wxKEYF(98, WXK_F7);
    wxKEYF(100, WXK_F8);
    wxKEYF(101, WXK_F9);
    wxKEYF(109, WXK_F10);
    wxKEYF(103, WXK_F11);
    wxKEYF(111, WXK_F12);
    wxKEYF(105, WXK_F13);
    wxKEYF(107, WXK_F14);
    wxKEYF(113, WXK_F15);
  case WXK_UP:
    return 0x7e;
  case WXK_DOWN:
    return 0x7d;
  case WXK_LEFT:
    return 0x7b;
  case WXK_RIGHT:
    return 0x7c;
  case WXK_RETURN:
    return 0x24;
  case WXK_TAB:
    return 0x30;
  case WXK_BACK:
    return 0x33;
  case WXK_DELETE:
    return 0x75;
  case WXK_HOME:
    return 0x73;
  case WXK_END:
    return 0x77;
  case WXK_PRIOR:
    return 0x74;
  case WXK_NEXT:
    return 0x79;
  case WXK_ADD:
    return 0x45;
  case WXK_SUBTRACT:
    return 78;
  case WXK_MULTIPLY:
    return 0x43;
  case WXK_DIVIDE:
    return 0x4B;
  case WXK_SEPARATOR:
    return 71;
  case WXK_DECIMAL:
    return 65;
  case 3:
    return 76;
  case WXK_NUMPAD0:
  case WXK_NUMPAD1:
  case WXK_NUMPAD2:
  case WXK_NUMPAD3:
  case WXK_NUMPAD4:
  case WXK_NUMPAD5:
  case WXK_NUMPAD6:
  case WXK_NUMPAD7:
    return (wxk - WXK_NUMPAD0) + 82;
  case WXK_NUMPAD8:
    return 91;
  case WXK_NUMPAD9:
    return 92;
  default:
    return 0;
  }
}
