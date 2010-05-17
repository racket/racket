/*
 * File:        mredmac.cc
 * Purpose:     GRacket MacOS event loop
 * Author:      Matthew Flatt
 * Created:     1996
 * Copyright:   (c) 2004-2010 PLT Scheme Inc.
 * Copyright:   (c) 1996, Matthew Flatt
 */

#include "common.h"

#include "wx_main.h"
#include "wx_win.h"
#include "wx_frame.h"
#include "wx_canvs.h"
#include "wx_utils.h"
#include "scheme.h"
#include "wx_macevents.h"
#include "wx_het.h"

#include "gracket.h"

#include <unistd.h>
#include <fcntl.h>

#ifdef __i386__ 
# include <CoreServices/CoreServices.h>
# define wxNATIVE_LONG(x) EndianS32_BtoN((x).bigEndianValue)
#else
# define wxNATIVE_LONG(x) x
#endif

static int dispatched = 1;

extern "C" {
  typedef void (*HANDLE_AE)(EventRecord *e);
}

class MrQueueElem; /* defined below */

static void QueueTransferredEvent(EventRecord *e);
static void MrDequeue(MrQueueElem *q);

WindowPtr MrEdMouseWindow(Point where);
WindowPtr MrEdKeyWindow();

int wx_leave_all_input_alone;

extern int wxTranslateRawKey(int key);
extern short wxMacDisableMods;

typedef MrQueueElem *MrQueueRef;

typedef int (*Checker_Func)(EventRecord *evt, MrQueueRef q, int check_only, 
			    MrEdContext *c, MrEdContext *keyOk, 
			    EventRecord *event, MrEdContext **which);

typedef struct EventFinderClosure {
  int check_only;
  MrEdContext *c;
  MrEdContext *keyOk;
  EventRecord *event;
  MrEdContext **which;
  Checker_Func checker;
} EventFinderClosure;

static int queue_size, max_queue_size;
static int mouse_down_in_flight;

Bool wx_ignore_key; /* used in wxItem */

void MrEdInitFirstContext(MrEdContext *)
{
}

void MrEdInitNewContext(MrEdContext *)
{
} 

void MrEdDestroyContext(MrEdFinalizedContext *)
{
}

int MrEdGetDoubleTime(void)
{
  return (int)(GetDblTime() * 16.67);
}

static wxFrame *_wxWindowPtrToFrame(WindowPtr w, wxChildList *l)
{
  wxChildNode *n;

  for (n = l->First(); n; n = n->Next()) {
    wxFrame *f;
    f = (wxFrame *)n->Data();
    if (f->macWindow() == w)
      return f;
  }

  return NULL;
}

static wxFrame *wxWindowPtrToFrame(WindowPtr w, MrEdContext *c)
{
  if (c)
    return _wxWindowPtrToFrame(w, c->topLevelWindowList);
  else {
    for (c = mred_contexts; c; c = c->next) {
      wxFrame *f;
      if ((f = _wxWindowPtrToFrame(w, c->topLevelWindowList)))
	return f;
    }
  }

  return NULL;
}

static void UpdateRgnToWindowCoords(WindowPtr w, RgnHandle updateRgn)
{
  Rect windowBounds;
  RgnHandle contentRgn;
  
  GetWindowBounds(w, kWindowGlobalPortRgn, &windowBounds);

  /* Avoid overflow in offset: */
  contentRgn = NewRgn();
  if (contentRgn) {
    GetWindowRegion(w, kWindowContentRgn, contentRgn);
    SectRgn(contentRgn, updateRgn, updateRgn);
    DisposeRgn(contentRgn);
  }
  
  OffsetRgn(updateRgn, -1 * windowBounds.left, -1 * windowBounds.top);
}

/***************************************************************************/
/*                            shadow event queue                           */
/***************************************************************************/

/*
   We need two things from the event queue:

    * We need to handle the event queue non-sequentially.  That is, we
      want to handle certain kinds of events before handling other
      kinds of events.

    * We need to be able to sleep until a new (potentially ready to
      handle) event arrives in the queue.

   The only solution appears to be sucking all of the events into a
   queue of our own, and dealing with them there.  This causes certain
   problems, but not horrible ones.
*/


class MrQueueElem {
public:
  EventRecord event;
  RgnHandle rgn;
  int half_done; /* e.g., window brought to front */
  MrQueueElem *next, *prev;
};

static MrQueueElem *first, *last;

/* QueueTransferredEvent takes an event and puts it
 * in the MrEd queue, with several exceptions.
 * 1. Update events.  Update events are sent by the OS
 *    whenever the OS queue does not contain an update
 *    event and the update region is not empty.  That is,
 *    the OS will keep poking you until the update region
 *    is empty.  To get around this, QTE clears the update
 *    region manually (and then must reinstate it when it's
 *    time to handle the event.  ick.
 * 2. high level events. Dispatched immediately, and the
 *    handlers queue work in Racket threads.
 * 3. suspendResumeMessage. See comment at top.
 */

static void QueueTransferredEvent(EventRecord *e)
{
  MrQueueElem *q;
  int done;
  
  dispatched = 0;
  
  done = 0;
  if (e->what == updateEvt) {
    WindowPtr w = (WindowPtr)e->message;
    for (q = first; q; q = q->next) {
      if ((q->event.what == updateEvt)
	  && (w == ((WindowPtr)q->event.message))) {
        RgnHandle updateRgn;
	updateRgn = NewRgn();

        GetWindowRegion(w, kWindowUpdateRgn, updateRgn);	

	/* Shift to window coords, because the window might
	   move before we handle the update */
	UpdateRgnToWindowCoords(w, updateRgn);

        UnionRgn(updateRgn, q->rgn, q->rgn);
	DisposeRgn(updateRgn);

        BeginUpdate(w);
        EndUpdate(w);
        return;
      }
    }
  }
    
  if (e->what == kHighLevelEvent) {
    /* We have to dispatch the event immediately */
    AEProcessAppleEvent(e);
    return;
  }

  if ((e->what == osEvt) && !(((e->message >> 24) & 0x0ff) == suspendResumeMessage))
    return;

  q = new WXGC_PTRS MrQueueElem;
  memcpy(&q->event, e, sizeof(EventRecord));
  q->next = NULL;
  q->prev = last;
  if (last)
    last->next = q;
  else
    first = q;
  last = q;
  
  queue_size++;
  if (queue_size > max_queue_size) {
    max_queue_size = queue_size;
  }

  if ((e->what == mouseDown)
      || (e->what == mouseMenuDown)) {
    mouse_down_in_flight = 1;
  }

  q->rgn = NULL;
  
  if (e->what == updateEvt) {
    WindowPtr w = (WindowPtr)e->message;
    q->rgn = NewRgn();
    GetWindowRegion(w, kWindowUpdateRgn, q->rgn);
    BeginUpdate(w);
    EndUpdate(w);

    /* Shift to window coords, because the window might
       move before we handle the update */
    UpdateRgnToWindowCoords(w, q->rgn);
  } else if (e->what == osEvt) {
    /* Must be a suspend/resume event */
    int we_are_front = e->message & resumeFlag;
    WindowPtr front;

    front = ActiveNonFloatingWindow();
    
    /* Generate an activate event */
    q->event.what = activateEvt;
    q->event.modifiers = we_are_front ? activeFlag : 0;
    q->event.message = (long)front;
  }
}

/* Called by wxWindows to queue leave and activate events: */
 
void QueueMrEdEvent(EventRecord *e)
{
  QueueTransferredEvent(e);
}

void DequeueMrEdEvents(int type, long message)
{
  /* Remove matching events: */
  MrQueueElem *qq, *next;
  for (qq = first; qq; qq = next) {
    next = qq->next;
    if ((qq->event.what == type)
	&& ((long)qq->event.message == message))
      MrDequeue(qq);
  }
}

static RgnHandle mouseRgn;
static int waiting_for_next_event;
static int wne_handlersInstalled;
static int pending_self_ae;

static void EnsureWNEReturn()
{
  /* Generate an event that WaitNextEvent() will return, but that we can
     recognize and ignore. (Note that window handlers can run nested
     event handlers, such as the resize handler for the little
     OS-provided window to implement Chinese text via pinyin. We need
     something that doesn't break those loops.) An AppleEvent is a
     heavyweight(?) but apparently reliable way to get WaitNextEvent() to
     return. Of course, don't install the standard handlers that are put
     in place by RunApplicationEventLoop(), because they'll dispatch the 
     dummy AppleEvent and defeat the purpose. */
  if (!pending_self_ae) {
    ProcessSerialNumber psn;
    AppleEvent ae, ae_target;

    if (GetCurrentProcess(&psn) == noErr) {
      if (AECreateDesc(typeProcessSerialNumber, &psn, sizeof(psn), &ae_target) == noErr) {
        if (AECreateAppleEvent('MrEd', 'Smug', &ae_target, kAutoGenerateReturnID, kAnyTransactionID, &ae) == noErr) {
          if (AESend(&ae, NULL, kAENoReply, kAENormalPriority, kNoTimeOut, NULL, NULL) == noErr) {
            pending_self_ae = 1;
          }
          AEDisposeDesc(&ae_target);
        }
        AEDisposeDesc(&ae);
      }
    }
  }
}

void wxSmuggleOutEvent(EventRef ref)
{
  EventRecord e;
  int ok = 0;

  if ((GetEventClass(ref) == kEventClassMouse)
      && (GetEventKind(ref) == 11 /* kEventMouseScroll */)) {
    GetEventParameter(ref, kEventParamEventRef, typeEventRef,
                      NULL, sizeof(ref), NULL, &ref);
  }

  if ((GetEventClass(ref) == kEventClassMouse)
      && (GetEventKind(ref) == kEventMouseWheelMoved)) {
    UInt32 modifiers;
    EventMouseWheelAxis axis;
    SInt32 delta;
    Point pos;
	
    GetEventParameter(ref, kEventParamKeyModifiers, typeUInt32, 
                      NULL, sizeof(modifiers), NULL, &modifiers);
    GetEventParameter(ref, kEventParamMouseWheelAxis, 
                      typeMouseWheelAxis, NULL, sizeof(axis), NULL, &axis);
    GetEventParameter(ref, kEventParamMouseWheelDelta, 
                      typeLongInteger, NULL, sizeof(delta), NULL, &delta);
    GetEventParameter(ref, kEventParamMouseLocation,
                      typeQDPoint, NULL, sizeof(Point), NULL, &pos);

    if (axis == kEventMouseWheelAxisY) {
      e.what = wheelEvt;
      e.message = (delta > 0);
      e.modifiers = modifiers;
      e.where.h = pos.h;
      e.where.v = pos.v;
      ok = TRUE;
    }
  } else if ((GetEventClass(ref) == kEventClassTextInput)
             && (GetEventKind(ref) == kEventTextInputUnicodeForKeyEvent)) {
    UniChar *text;
    UInt32 actualSize; 
    EventRef kref;
    
    GetEventParameter(ref, kEventParamTextInputSendKeyboardEvent,
                      typeEventRef, NULL, sizeof(EventRef), NULL, &kref);
    if (ConvertEventRefToEventRecord(kref, &e)) {
      ok = TRUE;
    } else {
      e.modifiers = 0;
      e.message = 0;
      e.where.h = 0;
      e.where.v = 0;
    }

    if ((e.modifiers & (wxMacDisableMods | cmdKey))
        || wxTranslateRawKey((e.message & keyCodeMask) >> 8)) {
      /* keep the raw event */
    } else {
      GetEventParameter(ref, kEventParamTextInputSendText,
                        typeUnicodeText, NULL, 0, &actualSize, NULL);
      if (actualSize) {
        text = (UniChar*)scheme_malloc_atomic(actualSize);
        GetEventParameter(ref, kEventParamTextInputSendText,
                          typeUnicodeText, NULL, actualSize, NULL, text);
      
        e.what = unicodeEvt;
        e.message = text[0];
        ok = TRUE;
      }
    }
  } else {
    ok = ConvertEventRefToEventRecord(ref, &e);
  }

  if (ok) {
    QueueTransferredEvent(&e);
    EnsureWNEReturn();
  }
}

static OSStatus unhide_cursor_handler(EventHandlerCallRef inHandlerCallRef, 
                                      EventRef inEvent, 
                                      void *inUserData)
{
  wxUnhideCursor();
  return eventNotHandledErr;
}

static OSStatus smuggle_handler(EventHandlerCallRef inHandlerCallRef, 
                                EventRef inEvent, 
                                void *inUserData)
{
  if (wx_leave_all_input_alone)
    return eventNotHandledErr;

  wxSmuggleOutEvent(inEvent);
  return noErr;
}

static pascal OSErr HandleSmug(const AppleEvent *evt, AppleEvent *rae, long k)
{
  pending_self_ae = 0;
  return 0;
}

/* WNE: a small wrapper for WaitNextEvent(), mostly to manage
   wake-up activities.
   It's tempting to try to use ReceiveNextEvent() to filter
   the raw events. Don't do that, because WaitNextEvent() is
   magic. In particular, WaitNextEvent() properly handles
   Cmd-~, Cmd-Q, dead keys like option-e on a U.S. keyboard,
   clicking that brings the application to the foreground,
   and the character palette. (We used ReceiveNextEvent()
   until version 352.7, and finally gave up when trying
   to get the character palette to work.) */
int WNE(EventRecord *e, double sleep_secs)
{
  int r;
  long ticks;

  if (mouse_down_in_flight) {
    /* Try hard to handle a mouse-down event before calling
       WaitNextEvent again. Otherwise, mouse events for tracking
       (e.g., menu clicks, close-window clicks, window-drag clicks,
       and button clicks) can get lost. We can't wait forever, though;
       the target eventspace might be stuck for some reason. If MrEd
       is idle enough to sleep, take that as a sign that it's ok to
       get new events. Another sign is if there's a new mouse-down or
       key-down event. Some other cases, such as a `yield' or waiting
       on an AppleEvent, are handled by explicitly turning off
       mouse_down_in_flight before we get here. */
    EventRef eref;
    EventTypeSpec poll_evts[2];

    if (!sleep_secs) {
      poll_evts[0].eventClass = kEventClassMouse;
      poll_evts[0].eventKind = kEventMouseDown;
      poll_evts[1].eventClass = kEventClassKeyboard;
      poll_evts[1].eventKind = kEventRawKeyDown;
      eref = AcquireFirstMatchingEventInQueue(GetCurrentEventQueue(),
                                              2,
                                              poll_evts,
                                              kEventQueueOptionsNone);
      if (eref) {
        ReleaseEvent(eref);
      } else {
        /* Looks like we should wait... */
        return 0;
      }
    }
  }


  wxResetCanvasBackgrounds();
  
  if (!wne_handlersInstalled) {
    EventTypeSpec evts[4];
    wne_handlersInstalled = TRUE;

    evts[0].eventClass = kEventClassMouse;
    evts[0].eventKind = kEventMouseDown;
    evts[1].eventClass = kEventClassMouse;
    evts[1].eventKind = kEventMouseMoved;
    evts[2].eventClass = kEventClassMouse;
    evts[2].eventKind = kEventMouseUp;
    evts[3].eventClass = kEventClassMouse;
    evts[3].eventKind = kEventMouseDragged;

    ::InstallEventHandler(GetEventDispatcherTarget(),
			  unhide_cursor_handler,
			  4,
			  evts,
			  NULL,
			  NULL);

    evts[0].eventClass = kEventClassMouse;
    evts[0].eventKind = 11 /* kEventMouseScroll */;
    evts[1].eventClass = kEventClassMouse;
    evts[1].eventKind = kEventMouseWheelMoved;
    evts[2].eventClass = kEventClassTextInput;
    evts[2].eventKind = kEventTextInputUnicodeForKeyEvent;

    ::InstallEventHandler(GetEventDispatcherTarget(),
			  smuggle_handler,
			  3,
			  evts,
			  NULL,
			  NULL);

    AEInstallEventHandler('MrEd', 'Smug', HandleSmug, 0, 0);

    mouseRgn = NewRgn();
    SetRectRgn(mouseRgn, 0, 0, 1, 1);
  }

  waiting_for_next_event = 1;

  if (sleep_secs < 0.0)
    ticks = 0x7FFFFFFF;
  else
    ticks = (long)(sleep_secs * 60);

  r = WaitNextEvent(everyEvent, e, ticks, mouseRgn);

  waiting_for_next_event = 0;

  return r;
}

void WakeUpMrEd()
{
  /* Make sure we wake up a sleep, if this is a callback through
     a window painter. */
  if (waiting_for_next_event) {
    EnsureWNEReturn();
    waiting_for_next_event = 0;
  }
}

/* TransferQueue sucks all of the pending events out of the
   Application queue, sticks them in the MrEd queue, and returns 1,
   unless it was called less than delay_time ago, in which case do
   nothing and return 0. */

static unsigned long lastTime;

static int wne_delay_on;
static unsigned long wne_delay_until;
 
static int TransferQueue(int all)
{
  EventRecord e;
  unsigned long tc;
  int sleep_time = 0;
  int delay_time = 0;
  
  /* Don't call WaitNextEvent() too often. */
  tc = TickCount();
  if (tc <= lastTime + delay_time)
    return 0;
  if (wne_delay_on && (tc < wne_delay_until))
    return 0;
  wne_delay_on = 0;

  while (WNE(&e, dispatched ? ((double)sleep_time/60.0) : 0)) {
    QueueTransferredEvent(&e);
  }
  
  lastTime = TickCount();
  
  return 1;
}

void wxStartRefreshSequence(void)
{
  /* Editors are not buffered offscreen under Mac OS X, instead
     relying on the OS's buffering of all windows, which are updated
     on WNE boundaries. To avoid flicker, avoid calling WNE in the
     middle of an editor refresh.  The refresh might get stuck,
     though, so we only wait a little while. */

  if (!wne_delay_on) {
    wne_delay_until = TickCount() + 10;
  }
  wne_delay_on++;
}

void wxEndRefreshSequence(void)
{
  if (wne_delay_on)
    --wne_delay_on;
}

static void MrDequeue(MrQueueElem *q)
{
  if (q->prev)
    q->prev->next = q->next;
  else
    first = q->next;
  if (q->next)
    q->next->prev = q->prev;
  else
    last = q->prev;

  --queue_size;
}

static MrQueueRef Find(EventFinderClosure *closure)
{
  MrQueueRef osq, next;

  osq = first;
  while (osq) {
    next = osq->next;

    if (closure->checker(&osq->event, osq, closure->check_only, 
			 closure->c, closure->keyOk, 
			 closure->event, closure->which)) {
      return osq;
    }

    osq = next;
  }

  return NULL;
}

/***************************************************************************/
/*                               state finder                              */
/***************************************************************************/

static MrEdContext *KeyOk(int current_only)
{
  WindowPtr w;
  wxFrame *fr;
  MrEdContext *c;
  
  c = current_only ? MrEdGetContext() : NULL;
  
  fr = wxGetFocusFrame();
  if (!fr) {
    w = ActiveNonFloatingWindow();
    fr = wxWindowPtrToFrame(w, c);
  }
  if (!fr || (c && (fr->context != (void *)c)) 
      || (!c && !((MrEdContext *)fr->context)->ready))
    return NULL;
  
  return (fr ? (MrEdContext *)fr->context : c);
}

static int WindowStillHere(WindowPtr win)
{
  return IsValidWindowPtr(win);
}

static int GetMods(void)
{
  KeyMap km;
  int mods = 0;
	  
  GetKeys(km);
  if (wxNATIVE_LONG(km[1]) & 32768)
    mods |= cmdKey;
  if (wxNATIVE_LONG(km[1]) & 1)
    mods |= shiftKey;
  if (wxNATIVE_LONG(km[1]) & 4)
    mods |= optionKey;
  if (wxNATIVE_LONG(km[1]) & 8)
    mods |= controlKey;
  
  return mods;
}

/* the cont_mouse_context is used to keep information about mouse-downs around so
 * that later mouse-ups can be properly handled.
 */
 
static MrEdContext *cont_mouse_context;
static WindowPtr cont_mouse_context_window;
static Point last_mouse;
static WindowPtr last_front_window;

void wxTracking()
{
  /* This function is called whenever wxMac lets the toolbox process
     events, normally to track some button click. In that case, we
     assume that a mouse-up event won't come through the event
     queue. */
  cont_mouse_context = NULL;
  cont_mouse_context_window = NULL;
}

void wxMouseEventHandled(void)
{
  mouse_down_in_flight = 0;
}

#ifdef RECORD_HISTORY
FILE *history;
#endif

/***************************************************************************/
/*                                event finders                            */
/***************************************************************************/

static int CheckForLeave(EventRecord *evt, MrQueueRef q, int check_only, 
			 MrEdContext *c, MrEdContext *keyOk, 
			 EventRecord *event, MrEdContext **which) {
  switch (evt->what) {
  case leaveEvt:
    {
      wxWindow *win;
      wxFrame *fr;
      MrEdContext *fc;
      void *refcon;

      refcon = (void *)evt->message;
      win = (wxWindow *)GET_SAFEREF(refcon);

      if ((win->__type != -1) && win->IsShown()) {
	fr = (wxFrame *)win->GetRootFrame();
	fc = fr ? (MrEdContext *)fr->context : NULL;
	if ((!c && !fr) || (!c && fc->ready) || (fc == c)) {
	  if (which)
	    *which = fc;

#ifdef RECORD_HISTORY
	  fprintf(history, "leave\n");
	  fflush(history);
#endif

	  if (check_only)
	    return TRUE;
	
	  MrDequeue(q);
	  memcpy(event, evt, sizeof(EventRecord));
	  return TRUE;
	}
      } else {
	MrDequeue(q);
      }
    }
  }

  return FALSE;
}

static int saw_mdown = 0, mdown_was_ctl = 0, saw_kdown = 0;

static int CheckForMouseOrKey(EventRecord *e, MrQueueRef osq, int check_only, 
			      MrEdContext *c, MrEdContext *keyOk, 
			      EventRecord *event, MrEdContext **foundc) {
  int found = 0;
  wxFrame *fr;
  MrEdContext *fc;

  switch (e->what) {
  case mouseMenuDown:
  case mouseDown:
    {
      WindowPtr window, front = NULL;
      int part;

      saw_mdown = 1;
      
      part = FindWindow(e->where, &window);
      if (part == inMenuBar) {
	front = ActiveNonFloatingWindow();
	window = front;
      }

      if (!window) {
	MrDequeue(osq);
	found = 1;
	*foundc = keyOk;
	cont_mouse_context = NULL;
      } else if (!WindowStillHere(window)) {
	MrDequeue(osq);
      } else {
	MrEdContext *clickOk;

	fr = wxWindowPtrToFrame(window, c);
	fc = fr ? (MrEdContext *)fr->context : NULL;

	if (!fr || (c && (fr->context != (void *)c)) 
	    || (!c && !((MrEdContext *)fr->context)->ready))
	  clickOk = NULL;
	else
	  clickOk = fc;

	if (!front)
	  front = ActiveNonFloatingWindow();
	if (window != front) {
	  WindowClass wc;

	  GetWindowClass(window, &wc);
	  if ((wc != kFloatingWindowClass)
	      && (wc != kUtilityWindowClass)
	      && (wc != kToolbarWindowClass)) {
	    /* Handle bring-window-to-front click immediately */
	    if (!osq->half_done) {
	      if (fc && (!fc->modal_window || (fr == fc->modal_window))) {
                if ((part == inContent) || !(e->modifiers & cmdKey))
                  SelectWindow(window);
		cont_mouse_context = NULL;
	      } else if (fc && fc->modal_window) {
		wxFrame *mfr;
		mfr = (wxFrame *)fc->modal_window;
		cont_mouse_context = NULL;
                if ((part == inContent) || !(e->modifiers & cmdKey))
                  SelectWindow(mfr->macWindow());
	      }
	      osq->half_done = 1;
	    }
	  }
	}

	*foundc = clickOk;
	if (*foundc) {
	  last_mouse.h = -1;
	  found = 1;
	  if (!check_only && (part != inMenuBar)) {
	    cont_mouse_context = *foundc;
	    cont_mouse_context_window = window;
	    mdown_was_ctl = (e->modifiers & controlKey);
	  } else
	    cont_mouse_context = NULL;
	}
      }
    }
    break;
  case mouseUp:
    if (!cont_mouse_context) {
      if (!saw_mdown) {
	MrDequeue(osq);
      }
    } else if (keyOk == cont_mouse_context) {
      *foundc = keyOk;
      if (*foundc) {
	found = 1;
	if (!check_only)
	  cont_mouse_context = NULL;
      }
    }
    break;
  case wheelEvt:
  case unicodeEvt:
  case keyDown:
  case autoKey:
  case keyUp:
    *foundc = keyOk;
    if (*foundc) {
      found = 1;
    }
    break;
  }

  if (found) {
    memcpy(event, e, sizeof(EventRecord));

    /* Preserve rightness (as opposed to leftness) of mouse clicks */
    if ((e->what == mouseUp) && mdown_was_ctl)
      event->modifiers |= controlKey;
  }

  return found;
}

static int CheckForActivate(EventRecord *evt, MrQueueRef q, int check_only, 
			    MrEdContext *c, MrEdContext *keyOk, 
			    EventRecord *event, MrEdContext **which)
{
  WindowPtr window;

  switch (evt->what) {
  case kHighLevelEvent:
    {
      MrEdContext *fc;
      fc = NULL;
      if ((!c && !fc) || (!c && fc->ready) || (fc == c)) {
	if (which)
	  *which = fc;
        if (check_only)
          return TRUE;
	memcpy(event, evt, sizeof(EventRecord));
        MrDequeue(q);
	return TRUE;
      }
    }
    break;
  case activateEvt:
    window = (WindowPtr)evt->message;
    if (WindowStillHere(window)) {
      wxFrame *fr;
      MrEdContext *fc;

      fr = wxWindowPtrToFrame(window, c);
      fc = fr ? (MrEdContext *)fr->context : NULL;
      if ((!c && !fr) || (!c && fc->ready) || (fc == c)) {
	if (which)
	  *which = fc;

#ifdef RECORD_HISTORY
	fprintf(history, "activate\n");
	fflush(history);
#endif

	if (check_only)
	  return TRUE;
	
	memcpy(event, evt, sizeof(EventRecord));
	MrDequeue(q);
	return TRUE;
      }
    } else
      MrDequeue(q);
    break;
  }

  return FALSE;
}

/***************************************************************************/
/*                             get next event                              */
/***************************************************************************/

int MrEdGetNextEvent(int check_only, int current_only,
		     EventRecord *event, MrEdContext **which)
{
  /* Search for an event. Handle clicks in non-frontmost windows
     immediately. */
  MrQueueRef osq;
  EventFinderClosure closure;
  EventRecord ebuf;
  MrEdContext *c, *keyOk, *foundc;
  int found = 0;

  saw_mdown = 0; saw_kdown = 0;

  if (!event)
    event = &ebuf;
  
  c = current_only ? MrEdGetContext() : NULL;

  wxResetCanvasBackgrounds();
    
  keyOk = KeyOk(current_only);
  
#ifdef RECORD_HISTORY
  if (!history) history = fopen("history3", "w");
  fprintf(history, "%lx %lx %lx\n",
  	  c, keyOk, cont_event_context);
#endif

#if 0
  /* Update events are supposed to happen after mouse events, etc.
     However, OS X refreshes window displays when WNE is called.  In
     particular, it looks nicer to update the frontmost window before
     calling WNE. We must do this infrequenty, though, to avoid
     dispatching only update events when other sorts of events should
     get handled. */
  static RgnHandle quickUpdateRgn;
  static UInt32 quickUpdateTimeout;
  static UInt32 quickUpdateWait;
  if (!quickUpdateWait || (quickUpdateWait <= TickCount())) {
    WindowPtr front;

    quickUpdateWait = 0;

    front = FrontNonFloatingWindow();
    if (front) {
      if (!quickUpdateRgn)
	quickUpdateRgn = NewRgn();
	  
      GetWindowRegion(front, kWindowUpdateRgn, quickUpdateRgn);	
      if (!EmptyRgn(quickUpdateRgn)) {
	/* Setup a trampoline and call WNE if the current thread
	   if the handler thread for the front window? */
	quickUpdateWait = TickCount() + 15;
      }
    }
  }
#endif

  TransferQueue(0);
    
  if (cont_mouse_context)
    if (!WindowStillHere(cont_mouse_context_window))
      cont_mouse_context = NULL;
    
  closure.c = c;
  closure.check_only = check_only;
  closure.keyOk = keyOk;
  closure.event = event;
  closure.which = which;

  /* First, service leave events: */
  closure.checker = CheckForLeave;
  if (Find(&closure))
    return TRUE; 
  
  /* Next, service mouse & key events: */
  closure.checker = CheckForMouseOrKey;
  closure.which = &foundc;
  if ((osq = Find(&closure))) {
    found = 1;
  }
  closure.which = which;
  
  if (found) {
    /* Remove intervening mouse/key events: */
    MrQueueElem *qq, *next;
    for (qq = first; qq && (qq != osq); qq = next) {
      next = qq->next;
      switch (qq->event.what) {
      case mouseUp:
	cont_mouse_context = NULL;
	/* fallthrough... */
      case mouseMenuDown:
      case mouseDown:
      case wheelEvt:
      case unicodeEvt:
      case keyDown:
      case keyUp:
      case autoKey:
	MrDequeue(qq);
	break;
      }
    }

    if (which)
      *which = foundc;

#ifdef RECORD_HISTORY
    fprintf(history, "mouse or key\n");
    fflush(history);
#endif

    if (check_only)
      return TRUE;
    
    MrDequeue(osq);
    
    return TRUE;
  }
  
  // TransferQueue(0);
    
  /* Try activate and high-level events: */
  closure.checker = CheckForActivate;
  if (Find(&closure))
    return TRUE; 
  
  /* Generate a motion event? */
  if (keyOk) {
    WindowPtr front;

    GetMouse(&event->where);
    LocalToGlobal(&event->where);
    front = MrEdMouseWindow(event->where);

    if (((event->where.v != last_mouse.v)
	 || (event->where.h != last_mouse.h)
	 || last_front_window != front)
	&& (!cont_mouse_context || (cont_mouse_context == keyOk))) {
      long ticks;

      if (which)
	*which = (cont_mouse_context ? cont_mouse_context : keyOk);
	
      if (check_only) {
#ifdef RECORD_HISTORY
	fprintf(history, "move or drag\n");
	fflush(history);
#endif
	return TRUE;
      }

      last_mouse.v = event->where.v;
      last_mouse.h = event->where.h;
      last_front_window = front;

      event->what = nullEvent;
      ticks = TickCount();
      event->when = ticks;
      if (cont_mouse_context) {
	/* Dragging... */
	int mods;
	mods = GetMods();
	if (mdown_was_ctl)
	  mods |= controlKey;
	event->modifiers = mods | btnState;
	event->message = 1;
#ifdef RECORD_HISTORY
	fprintf(history, "drag\n");
	fflush(history);
#endif
      } else {
	if (keyOk) {
	  int mods;
	  mods = GetMods();
	  event->modifiers = mods;
	} else {
	  event->modifiers = 0;
	}
	event->message = (keyOk ? 1 : 0);
#ifdef RECORD_HISTORY
	fprintf(history, "move\n");
	fflush(history);
#endif
      }
      return TRUE;
    }
  }
  
#ifdef RECORD_HISTORY
  fprintf(history, "no event\n");
  fflush(history);
#endif
  
  return FALSE;
}

extern void wxCheckFinishedSounds(void);


void MrEdDispatchEvent(EventRecord *e)
{
  dispatched = 1;

  if (e->what == updateEvt) {
    /* Find the update event for this window: */
    RgnHandle rgn = NULL;
    MrQueueElem *q;
    WindowPtr w;

    w = (WindowPtr)e->message;

    for (q = first; q; q = q->next) {
      if ((q->event.what == updateEvt)
	  && (w == ((WindowPtr)q->event.message))) {
	rgn = q->rgn;
	MrDequeue(q);
	break;
      }
    }
    
    if (rgn) {
      /* rgn is in window co-ords */
      InvalWindowRgn(w, rgn);
      DisposeRgn(rgn);
    }
  }

  wxTheApp->doMacPreEvent();
  wxTheApp->doMacDispatch(e);
  wxTheApp->doMacPostEvent();
  
  wxCheckFinishedSounds();
}

int MrEdCheckForBreak(void)
{
  MrQueueElem *q;
  
  if (!KeyOk(TRUE))
    return 0;
  
  TransferQueue(0);

  for (q = first; q; q = q->next) {
    if (q->event.what == keyDown) {
      if ((((q->event.message & charCodeMask) == '.') 
	   && (q->event.modifiers & cmdKey))
      	  || (((q->event.message & charCodeMask) == 3) 
	      && (q->event.modifiers & controlKey))) {
        MrDequeue(q);
        return TRUE;
      }
    }
  }
  
  return FALSE;
}

/***************************************************************************/
/*                                 sleep                                   */
/***************************************************************************/

#include <pthread.h>

/* These file descriptors are used for breaking the event loop. */
static int cb_socket_ready;
static int ready_sock, write_ready_sock;

static int StartFDWatcher(void (*mzs)(float secs, void *fds), float secs, void *fds)
{
  scheme_start_sleeper_thread(mzs, secs, fds, write_ready_sock);
  return 1;
}

static void EndFDWatcher(void)
{
  scheme_end_sleeper_thread();
}

void socket_callback(CFSocketRef s, CFSocketCallBackType type, CFDataRef address, const void *data, void *info)
{
  EnsureWNEReturn();
}

static const void *sock_retain(const void *info)
{
  return NULL;
}

static void sock_release(const void *info)
{
  /* do nothing */
}

static CFStringRef sock_copy_desc(const void *info)
{
  return CFSTR("sock");
}

static int going, reported_recursive_sleep;

void MrEdMacSleep(float secs, void *fds, SLEEP_PROC_PTR mzsleep)
{
  if (going) {
    if (!reported_recursive_sleep) {
      fprintf(stderr, "BUG: recursive sleep! Please submit a bug report that explains how\n");
      fprintf(stderr, "you got this message. (It won't appear again until you restart.)\n");
      reported_recursive_sleep = 1;
    }
    return;
  }

  /* If we're asked to sleep less than 1/60 of a second, then don't
     bother with WaitNextEvent(). */
  if ((secs > 0) && (secs < 1.0/60)) {
    mzsleep(secs, fds);
  } else {
    EventRecord e;

    if (!cb_socket_ready) {
      /* We set up a pipe for the purpose of breaking the Carbon
	 event manager out of its loop. When the watcher thread sees
	 that an fd is ready, it writes to write_sock_ready, which
	 means that sock_ready is ready to read, which means that
	 socket_callback is invoked, and it calls EnsureWNEReturn().

         With the current implementation of EnsureWNEReturn(), this is
         probably overkill. I think the watcher thread could call
         EnsureWNEReturn() directly. Doing it this way moves the call
         into this thread, though, which seems more robust in the long
         run (i.e., if EnsureWNEReturn() changes). */
      int fds[2];
      if (!pipe(fds)) {
	CFRunLoopRef rl;
	CFSocketRef cfs;
	CFRunLoopSourceRef source;
	CFSocketContext context;

	/* True, they're not really sockets... */
	ready_sock = fds[0];
	write_ready_sock = fds[1];

	/* The code below simply says says "please call
	   socket_callback from WNE when there's data to read on
	   ready_sock" */

	context.version = 0; /* ? */
	context.info = NULL;
	context.retain = sock_retain;
	context.release = sock_release;
	context.copyDescription = sock_copy_desc;

	rl = (CFRunLoopRef)GetCFRunLoopFromEventLoop(GetMainEventLoop());
	cfs = CFSocketCreateWithNative(CFAllocatorGetDefault(), ready_sock, kCFSocketReadCallBack, socket_callback, &context);
	source = CFSocketCreateRunLoopSource(CFAllocatorGetDefault(), cfs, 0);
	CFRunLoopAddSource(rl, source, kCFRunLoopDefaultMode);
	
	fcntl(ready_sock, F_SETFL, O_NONBLOCK);
	cb_socket_ready = 1;
      }
    }

    /* Starts a watcher thread, which runs select() on the fds,
       and also breaks when SIGINT is received. */
    if (!StartFDWatcher(mzsleep, secs, fds)) {
      secs = 0;
    }

    going++;

    if (WNE(&e, secs ? secs : kEventDurationForever))
      QueueTransferredEvent(&e);

    --going;

    /* Shut down the watcher thread */
    EndFDWatcher();
    if (cb_socket_ready) {
      /* clear out the pipe: */
      char buf[1];
      read(ready_sock, buf, 1);
    }
  }
}

/***************************************************************************/
/*               location->window (used for send-message)                  */
/***************************************************************************/

wxWindow *wxLocationToWindow(int x, int y)
{
  Point p;
  WindowPtr f;
  Rect bounds;
  int part;

  p.h = x;
  p.v = y;
  part = FindWindow(p, &f);
  
  GetWindowBounds(f, kWindowContentRgn, &bounds);
  if (IsWindowVisible(f)
      && (bounds.left <= x)
      && (bounds.right >= x)
      && (bounds.top <= y)
      && (bounds.bottom >= y)) {
    /* Found it */
    wxFrame *frame;
    void *refcon;

    refcon = (void *)GetWRefCon(f);
    frame = (wxFrame *)GET_SAFEREF(refcon);

    if (frame) {
      /* Mac: some frames really represent dialogs. Any modal frame is
	 a dialog, so extract its only child. */
      if (frame->IsModal()) {
	wxChildNode *node2;
	wxChildList *cl;
	cl = frame->GetChildren();
	node2 = cl->First();
	if (node2)
	  return (wxWindow *)node2->Data();
      } else
	return frame;
    } else
      return NULL;
  }
  
  return NULL;
}

WindowPtr MrEdMouseWindow(Point where)
{
  WindowPtr win;
  WindowClass wc;
  int part;

  part = FindWindow(where, &win);
  if (part == inMenuBar)
    return FrontNonFloatingWindow();

  GetWindowClass(win, &wc);
  if ((wc == kFloatingWindowClass)
      || (wc == kUtilityWindowClass)
      || (wc == kToolbarWindowClass)) {
    /* Floating windows always receive events: */
    return win;
  } else {
    return FrontNonFloatingWindow();
  }
}

WindowPtr MrEdKeyWindow()
{
  wxFrame *f;
  f = wxGetFocusFrame();
  if (f)
    return f->macWindow();
  else
    return FrontWindow();
}

/***************************************************************************/
/*                                gc                                       */
/***************************************************************************/

void wxmac_reg_globs(void)
{
  wxREGGLOB(first);
  wxREGGLOB(last);
  wxREGGLOB(cont_mouse_context);
}

/***************************************************************************/
/*                            AppleEvents                                  */
/***************************************************************************/

static Scheme_Object *record_symbol, *file_symbol;

static long check_four(char *name, int which, int argc, Scheme_Object **argv)
{
  Scheme_Object *o = argv[which];

  if (!SCHEME_BYTE_STRINGP(o) || (SCHEME_BYTE_STRTAG_VAL(o) != 4))
    scheme_wrong_type(name, "MacOS type/creator 4-character byte string", which, argc, argv);
  
#ifdef __POWERPC__
  return *(int *)SCHEME_BYTE_STR_VAL(o);
#else
  {
    int v;
    char tmp[4], *bs;
    bs = SCHEME_BYTE_STR_VAL(o);
    tmp[3] = bs[0];
    tmp[2] = bs[1];
    tmp[1] = bs[2];
    tmp[0] = bs[3];
    memcpy(&v, tmp, 4);
    return v;
  }
#endif
}

static int has_null(const char *s, long l)
{
  if (!l)
    return 1;

  while (l--) {
    if (!s[l])
      return 1;
  }

  return 0;
}

int scheme_mac_path_to_spec(const char *filename, FSSpec *spec)
{
  FSRef fsref;
  OSErr err;
	
  // first, convert to an FSRef
	
  err = FSPathMakeRef((const UInt8 *)filename,&fsref,NULL);
	
  if (err != noErr) {
    return 0;
  }

  memset(spec, 0, sizeof(FSSpec));
	
  // then, convert to an FSSpec
  err = FSGetCatalogInfo(&fsref, kFSCatInfoNone, NULL, NULL, spec, NULL);
	
  if (err != noErr) {
    return 0;
  }
	
  return 1;
}	

char *scheme_mac_spec_to_path(FSSpec *spec)
{
  FSRef fileRef;
  int longEnough = FALSE;
  int strLen = 256;
  char *str;
    
  str = (char *)scheme_malloc_atomic(strLen);
    
  // first, convert to an FSRef
  if (FSpMakeFSRef(spec,&fileRef) != noErr) {
    return NULL;
  }
    
  while (! longEnough) {
    if (FSRefMakePath(&fileRef,(unsigned char *)str,strLen) == pathTooLongErr) {
      strLen *= 2;
      str = (char *)scheme_malloc_atomic(strLen);
    } else {
      longEnough = TRUE;
    }
  }
    
  return str;
}

static int ae_marshall(AEDescList *ae, AEDescList *list_in, AEKeyword kw, Scheme_Object *v, 
		       char *name, OSErr *err, char **stage)
{
  DescType type;
  Ptr data;
  Size size;
  Boolean x_b;
  long x_i;
  double x_d;
  FSSpec x_fss;
  Handle alias = NULL;
  int retval = 1;
  OSErr _err;
    
  switch (SCHEME_TYPE(v)) {
  case scheme_true_type:
  case scheme_false_type:
    x_b = SCHEME_TRUEP(v) ? TRUE : FALSE;
    type = typeBoolean;
    data = (char *)&x_b;
    size = sizeof(Boolean);
    break;
  case scheme_integer_type:
    x_i = SCHEME_INT_VAL(v);
    type = typeLongInteger;
    data = (char *)&x_i;
    size = sizeof(long);
    break;
  case scheme_byte_string_type:
    type = typeChar;
    data = SCHEME_BYTE_STR_VAL(v);
    size = SCHEME_BYTE_STRTAG_VAL(v);
    break;
  case scheme_char_string_type:
    type = typeChar;
    v = scheme_char_string_to_byte_string(v);
    data = SCHEME_BYTE_STR_VAL(v);
    size = SCHEME_BYTE_STRTAG_VAL(v);
    break;
  case scheme_float_type:
  case scheme_double_type:
    x_d = SCHEME_FLOAT_VAL(v);
    type = typeFloat;
    data = (char *)&x_d;
    size = sizeof(double);
    break;
  case scheme_vector_type: /* vector => record */
    if ((SCHEME_VEC_SIZE(v) >= 1)
	&& ((SCHEME_VEC_ELS(v)[0] == record_symbol)
	    || (SCHEME_VEC_ELS(v)[0] == file_symbol))) {
      if (SCHEME_VEC_ELS(v)[0] == file_symbol) {
	if ((SCHEME_VEC_SIZE(v) == 2)
	    && SCHEME_PATH_STRINGP(SCHEME_VEC_ELS(v)[1]))  {
	  Scheme_Object *bs;
	  char *s;
	  long l;
	  bs = SCHEME_VEC_ELS(v)[1];
	  if (!SCHEME_PATHP(bs))
	    bs = scheme_char_string_to_byte_string(bs);
	  s = SCHEME_BYTE_STR_VAL(bs);
	  l = SCHEME_BYTE_STRTAG_VAL(bs);
	  if (!has_null(s, l)) {
	    if (scheme_mac_path_to_spec(s, &x_fss)) {
	      _err = NewAliasMinimal(&x_fss, (AliasHandle *)&alias);
	      *err = _err;
	      if (_err == -43) {
	        /* Can't make alias; make FSSpec, instead */
	        type = typeFSS;
	        data = (char *)&x_fss;
	        size = sizeof(FSSpec);
	        break;
	      } else if (_err) {
		*stage = "converting file to alias: ";
		return 0;
	      }
	      type = typeAlias;
	      HLock(alias);
	      data = (char *)*alias;
	      size = GetHandleSize(alias);
	      break;
	    }
	  }
	}
	scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
			 "%s: cannot interpret vector as a file specification: %V",
			 name,
			 v);
      }
      /* record case falls through to list */
    } else {
      scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		       "%s: cannot convert ill-tagged or untagged vector: %V",
		       name,
		       v);
    }
  case scheme_pair_type: /* /\ falls through */
  case scheme_null_type:
    {
      int l;
      int isrec = SCHEME_VECTORP(v);
        
      if (isrec)
	v = SCHEME_CDR(scheme_vector_to_list(v));
        
      l = scheme_proper_list_length(v);
      if (l >= 0) {
	AEDescList *list;
	list = (AEDescList *)scheme_malloc_atomic(sizeof(AEDescList));
          
        list->descriptorType = typeNull;
        list->dataHandle = NULL;
	_err = AECreateList(NULL, 0, isrec, list);
	if (_err) {
	  *err = _err;
	  *stage = "cannot create list/record: ";
	  return 0;
	}
		  
	while (!SCHEME_NULLP(v)) {
	  Scheme_Object *a = SCHEME_CAR(v);
	  AEKeyword rkw;
	  if (isrec) {
	    Scheme_Object *k;
	    if (!SCHEME_PAIRP(a)
		|| !SCHEME_PAIRP(SCHEME_CDR(a))
		|| !SCHEME_NULLP(SCHEME_CDR(SCHEME_CDR(a)))
		|| !SCHEME_BYTE_STRINGP(SCHEME_CAR(a))) {
	      /* Bad record form. */
	      scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
			       "%s: cannot interpret vector part as a record field: %s",
			       name,
			       scheme_make_provided_string(a, 1, NULL));
	    }
	    k = SCHEME_CAR(a);
	    a = SCHEME_CADR(a);
	    rkw = check_four(name, 0, 1, &k);
	  } else
	    rkw = 0;
	  if (!ae_marshall(NULL, list, rkw, a, name, err, stage)) {
	    AEDisposeDesc(list);
	    return 0;
	  }
	  v = SCHEME_CDR(v);
	}
		  
	if (list_in) {
	  if (kw)
	    _err = AEPutKeyDesc(list_in, kw, list);
	  else
	    _err = AEPutDesc(list_in, 0, list);
	  if (_err) {
	    *err = _err;
	    *stage = "cannot add list item: ";
	    AEDisposeDesc(list);
	    return 0;
	  }
	} else {
	  if (kw)
	    _err = AEPutParamDesc(ae, kw, list);
	  else
	    _err = AEPutParamDesc(ae, keyDirectObject, list);
	  if (_err) {
	    *err = _err;
	    *stage = "cannot install argument: ";
	    AEDisposeDesc(list);
	    return 0;
	  }
	}
		
	AEDisposeDesc(list);
		  
	return 1;
      }
    }
  default:
    /* Don't know how to marshall */
    scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		     "%s: cannot convert value for sending: %s",
		     name,
		     scheme_make_provided_string(v, 1, NULL));
    return 0;
  }
    
  if (list_in) {
    if (kw)
      _err = AEPutKeyPtr(list_in, kw, type, data, size);
    else
      _err = AEPutPtr(list_in, 0, type, data, size);
    if (_err) {
      *err = _err;
      *stage = "cannot add list item: ";
      retval = 0;
    }
  } else {
    if (kw)
      _err = AEPutParamPtr(ae, kw, type, data, size);
    else
      _err = AEPutParamPtr(ae, keyDirectObject, type, data, size);
    if (_err) {
      *err = _err;
      *stage = "cannot install argument: ";
      retval = 0;
    }
  }

  if (alias)
    DisposeHandle(alias);
	
  return retval;
}

static Scheme_Object *ae_unmarshall(AppleEvent *reply, AEDescList *list_in, int pos,
                                    OSErr *err, char **stage, Scheme_Object **record)
{

  DescType rtype;
  long sz;
  AEKeyword kw;
  Scheme_Object *result = NULL;
  OSErr _err;

  if (list_in) {
    if (AEGetNthPtr(list_in, pos, typeWildCard, &kw, &rtype, NULL, 0, &sz))
      return scheme_void;
  } else {
    if (AEGetParamPtr(reply, keyDirectObject, typeWildCard, &rtype, NULL, 0, &sz))
      return scheme_void;
  }
  
  {
    Boolean x_b;
    long x_i;
    double x_d;
    char *x_s = NULL;
    FSSpec x_f;
    Ptr data;
    
    switch (rtype) {
    case typeBoolean:
      data = (char *)&x_b;
      break;
    case typeLongInteger:
    case typeShortInteger:
      rtype = typeLongInteger;
      data = (char *)&x_i;
      sz = sizeof(long);
      break;
    case typeLongFloat:
    case typeShortFloat:
    case typeExtended:
      rtype = typeFloat;
      data = (char *)&x_d;
      sz = sizeof(double);
      break;
    case typeChar:
      x_s = (char *)scheme_malloc_atomic(sz + 1);
      data = x_s;
      x_s[0] = 0;
      break;
    case typeAlias:
    case typeFSS:
      rtype = typeFSS;
      data = (char *)&x_f;
      sz = sizeof(FSSpec);
      break;
    case typeAEList:
    case typeAERecord:
      {
	AEDescList *list;
	Scheme_Object *first = scheme_null, *last = NULL, *v, *rec, **recp;
	int i;
         
	list = (AEDescList *)scheme_malloc_atomic(sizeof(AEDescList));
          
	if (list_in) {
	  if (AEGetNthDesc(list_in, pos, rtype, &kw, list))
	    return NULL;
	  if (record) {
	    rec = scheme_make_sized_utf8_string((char *)&kw, sizeof(long));
	    *record = rec;
	  }
	} else {
	  if (AEGetParamDesc(reply, keyDirectObject, rtype, list))
	    return NULL;
	}
         
	if (rtype == typeAERecord)
	  recp = &rec;
	else
	  recp = NULL;
         
	for (i = 1; (v = ae_unmarshall(NULL, list, i, err, stage, recp)); i++) {
	  if (v == scheme_void)
	    break;
	  else if (!v) {
	    AEDisposeDesc(list);
	    return NULL;
	  } else {
	    Scheme_Object *pr;

	    pr = scheme_make_pair(v, scheme_null);
	    if (recp) {
	      pr = scheme_make_pair(rec, pr);
	      pr = scheme_make_pair(pr, scheme_null);
	    }
	           
	    if (last)
	      SCHEME_CDR(last) = pr;
	    else
	      first = pr;
	    last = pr;
	  }
	}
         
	if (recp)
	  first = scheme_list_to_vector(scheme_make_pair(record_symbol, first));
         
	AEDisposeDesc(list);
	return first;
      }
    default:
      /* Don't know how to un-marshall */
      *err = -1;
      *stage = "error translating the reply to a Scheme value: ";
      return NULL;
    }
    
    if (list_in) {
      _err = AEGetNthPtr(list_in, pos, rtype, &kw, &rtype, data, sz, &sz);
      if (record) {
	Scheme_Object *rec;
	rec = scheme_make_sized_utf8_string((char *)&kw, sizeof(long));
	*record = rec;
      }
      if (_err) {
	*err = _err;
        *stage = "lost a list value: ";
        return NULL;
      }
    } else {
      _err = AEGetParamPtr(reply, keyDirectObject, rtype, &rtype, data, sz, &sz);
      if (_err) {
	*err = _err;
        *stage = "lost the return value: ";
        return NULL;
      }
    }
    
    switch (rtype) {
    case typeBoolean:
      result = (x_b ? scheme_true : scheme_false);
      break;
    case typeLongInteger:
      result = scheme_make_integer(x_i);
      break;
    case typeFloat:
      result = scheme_make_double(x_d);
      break;
    case typeChar:
      result = scheme_make_sized_utf8_string(x_s, sz);
      break;
    case typeFSS:
      result = scheme_make_sized_utf8_string(scheme_mac_spec_to_path(&x_f), -1);
      break;      
    }
  }
  
  return result;
}

/* Single-threaded ok: */
static int escaped = 0;

static int handlerInstalled = 0;
class ReplyItem;
class ReplyItem {
public:
  long id;
  AppleEvent *ae;
  ReplyItem *next;
};
static ReplyItem *reply_queue;

static pascal Boolean while_waiting(EventRecord *e, long *sleeptime, RgnHandle *rgn)
{
  mz_jmp_buf *save, newbuf;
  
  if (escaped) return TRUE;
  
  QueueTransferredEvent(e);
  
  save = scheme_current_thread->error_buf;
  scheme_current_thread->error_buf = &newbuf;
  
  if (scheme_setjmp(newbuf)) {
    scheme_current_thread->error_buf = save;
    escaped = 1;
    return TRUE; /* Immediately return to AESend */
  } else {
    scheme_thread_block(0);
    scheme_current_thread->ran_some = 1;
    scheme_current_thread->error_buf = save;
  }
  
  return FALSE;
}

static pascal OSErr HandleAnswer(const AppleEvent *evt, AppleEvent *rae, long k)
{
  ReplyItem *r;
  DescType rtype;
  long sz;
  AppleEvent *ae;
  
  r = new WXGC_PTRS ReplyItem;
  ae = (AppleEvent *)scheme_malloc_atomic(sizeof(AppleEvent));
  r->ae = ae;
  
  AEGetAttributePtr(evt, keyReturnIDAttr, typeLongInteger, &rtype, &r->id, sizeof(long), &sz);
  
  AEDuplicateDesc(evt, r->ae);

  r->next = reply_queue;
  reply_queue = r;
  
  return 0;
}

static void wait_for_reply(AppleEvent *ae, AppleEvent *reply)
{
  EventRecord e;
  DescType rtype;
  long id, sz;
  ReplyItem *r, *prev;
  
  if (!handlerInstalled) {
    handlerInstalled = TRUE;
    AEInstallEventHandler(kCoreEventClass, kAEAnswer, NewAEEventHandlerUPP(HandleAnswer), 0, 0);
    wxREGGLOB(reply_queue);
  }
  
  AEGetAttributePtr(ae, keyReturnIDAttr, typeLongInteger, &rtype, &id, sizeof(long), &sz);
  
  while (1) {
    wxMouseEventHandled();
    WNE(&e, 1.0);
    if (e.what == kHighLevelEvent)
      AEProcessAppleEvent(&e);
    else {
      if (while_waiting(&e, NULL, NULL))
	break;
    }
	
    prev = NULL;
    for (r = reply_queue; r; r = r->next) {
      if (r->id == id) {
	/* Got the reply */
	memcpy(reply, r->ae, sizeof(AppleEvent));
	if (prev)
	  prev->next = r->next;
	else
	  reply_queue = r->next;
	return;
      }
      prev = r;
    }
  }
}

int scheme_mac_send_event(char *name, int argc, Scheme_Object **argv, 
			  Scheme_Object **result, int *err, char **stage)
{
  OSErr oerr;
  AEEventClass classid;
  AEEventID eventid;
  AppleEvent *ae = NULL, *reply = NULL;
  AEAddressDesc *target = NULL;
  DescType rtype;
  int retval;
  long ret, sz, dst;
  Scheme_Object *res;

  if (!record_symbol) {
    wxREGGLOB(record_symbol);
    wxREGGLOB(file_symbol);

    record_symbol = scheme_intern_symbol("record");
    file_symbol = scheme_intern_symbol("file");
  }

  dst = check_four(name, 0, argc, argv);
  classid = check_four(name, 1, argc, argv);
  eventid = check_four(name, 2, argc, argv);

  target = (AEAddressDesc *)malloc(sizeof(AEAddressDesc));
  oerr = AECreateDesc(typeApplSignature, &dst, sizeof(long), target);
  if (oerr) {
    free(target);
    target = NULL;
    *err = (int)oerr;
    *stage = "application not found: ";
    goto fail;
  }
    
  ae = (AppleEvent *)malloc(sizeof(AppleEvent));
  oerr = AECreateAppleEvent(classid, eventid, target, kAutoGenerateReturnID, 
                            kAnyTransactionID, ae);
  if (oerr) {
    free(ae);
    ae = NULL;
    *err = (int)oerr;
    *stage = "cannot create event: ";
    ae = NULL;    
    goto fail;
  }
  
  if ((argc > 3) && !SCHEME_VOIDP(argv[3])) {
    if (!ae_marshall(ae, NULL, 0, argv[3], name, &oerr, stage)) {
      *err = (int)oerr;
      goto fail;
    }
  }
  
  if (argc > 4) {
    Scheme_Object *l = argv[4];
    char *expected = "list of pairs containing a type-string and a value";
    while (SCHEME_PAIRP(l)) {
      Scheme_Object *a = SCHEME_CAR(l), *k, *v;
      AEKeyword kw;
      /* Must be a list of 2-item lists: keyword and value */
      if (!SCHEME_PAIRP(a) 
          || !SCHEME_PAIRP(SCHEME_CDR(a))
          || !SCHEME_NULLP(SCHEME_CDR(SCHEME_CDR(a)))
          || !SCHEME_BYTE_STRINGP(SCHEME_CAR(a)))
        break; /* => type error */
      k = SCHEME_CAR(a);
      v = SCHEME_CADR(a);
      kw = check_four(name, 0, 1, &k);
      if (!ae_marshall(ae, NULL, kw, v, name, &oerr, stage)) {
	*err = (int)oerr;
        goto fail;
      }
      l = SCHEME_CDR(l);
    }
    if (!SCHEME_NULLP(l))
      scheme_wrong_type(name, expected, 4, argc, argv);
  }
  
  reply = (AppleEvent *)malloc(sizeof(AppleEvent));
  oerr = AESend(ae, reply, kAEQueueReply | kAECanInteract, kAENormalPriority, kNoTimeOut, NULL, NULL);
  if (oerr) {
    free(reply);
    reply = NULL;
    *err = (int)oerr;
    *stage = "send failed: ";
    reply = NULL;
    goto fail;
  }
  wait_for_reply(ae, reply);
  if (escaped) {
     reply = NULL;
     escaped = 0;
     goto escape;
  }
  
  if (!AEGetParamPtr(reply, keyErrorString, typeChar, &rtype, NULL, 0, &sz) && sz) {
    char *st;
    *err = -1;
    if (sz > 256) sz = 256;
    st = (char *)scheme_malloc_atomic(sz + 1);
    *stage = st;
    (*stage)[sz] = 0;
    AEGetParamPtr(reply, keyErrorString, typeChar, &rtype, *stage, sz, &sz);
    goto fail;
  }
  if (!AEGetParamPtr(reply, keyErrorNumber, typeLongInteger, &rtype, &ret, sizeof(long), &sz)
      && ret) {
    *err = (int)ret;
    
    *stage = "application replied with error: ";
    goto fail;
  }
  
  res = ae_unmarshall(reply, NULL, 0, &oerr, stage, NULL);
  *result = res;
  if (!*result) {
    *err = (int)oerr;
    goto fail;
  }
  
  retval = 1;
  goto done;
escape:
  retval = -1;
  goto done;
fail:
  retval = 0;
   
done:
  if (ae) {
    AEDisposeDesc(ae);
    free(ae);
  }
  if (reply) {
    AEDisposeDesc(reply);
    free(reply);
  }
  if (target) {
    AEDisposeDesc(target);
    free(target);
  }
  
  if (retval < 0) {
    scheme_longjmp(scheme_error_buf, 1);
  }
  
  return retval;
}


/**********************************************************************/
/*          Generic control tracking with callbacks                   */
/*                 or frame painting on show                          */
/**********************************************************************/

static RgnHandle clipRgn;

class wxTC_Closure {
public:
  ControlRef ctl;
  Point start;
  ControlActionUPP proc;
};

static int call_tc(void *_c)
{
  wxTC_Closure *c;

  c = (wxTC_Closure *)_c;

  return TrackControl(c->ctl, c->start, c->proc);
}

ControlPartCode wxHETTrackControl(ControlRef theControl, Point startPoint, ControlActionUPP actionProc)
{
  wxTC_Closure *c;
  int v;

  c = new WXGC_PTRS wxTC_Closure;
  c->ctl = theControl;
  c->start = startPoint;
  c->proc = actionProc;

  v = wxHiEventTrampoline(call_tc, (void *)c);

  return v;
}

class wxSW_Closure {
public:
  WindowPtr w, pw;
};

static int call_sw(void *_c)
{
  wxSW_Closure *c;

  c = (wxSW_Closure *)_c;

  if (c->pw)
    ShowSheetWindow(c->w, c->pw);
  else
    ShowWindow(c->w);

  return 0;
}

extern void wxHETShowWindow(WindowPtr w)
{
  wxHETShowSheetWindow(w, NULL);
}

extern void wxHETShowSheetWindow(WindowPtr w, WindowPtr pw)
{
  wxSW_Closure *c;
  c = new WXGC_PTRS wxSW_Closure;
  c->w = w;
  c->pw = pw;

  wxHiEventTrampoline(call_sw, (void *)c);
}


int wxHETYield(wxWindow *win, HiEventTrampProc do_f, void *do_data)
{
  CGrafPtr savep;
  GDHandle savegd;
  ThemeDrawingState s;
  int more;
  wxMacDC *mdc;

  if (!clipRgn)
    clipRgn = NewRgn();

  GetGWorld(&savep, &savegd);  
  GetThemeDrawingState(&s);
  GetClip(clipRgn);

  /* We assume that win was the old MacDC user, and savep is win's
     MacDC. But control tracking has changed properties of the
     grafport, so indicate the need for a reset: */
  mdc = win->MacDC();
  mdc->setCurrentUser(NULL);

  more = mred_het_run_some(do_f, do_data);

  wxResetCanvasBackgrounds();

  SetGWorld(savep, savegd);
  SetThemeDrawingState(s, TRUE);
  SetClip(clipRgn);

  /* Again. win may not be the current user, but whoever
     is the current user for savep needs a reset. */
  mdc->setCurrentUser(NULL);

  return more;
}

void MrEdAtomicallyPaint(wxCanvas *win)
{
  int block_descriptor;

  block_descriptor = scheme_current_thread->block_descriptor;
  scheme_current_thread->block_descriptor = 0;

  scheme_start_atomic();
  win->OnPaint();
  scheme_end_atomic_no_swap();

  scheme_current_thread->block_descriptor = block_descriptor;
}
