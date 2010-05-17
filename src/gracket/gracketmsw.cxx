/*
 * File:        mredmsw.cc
 * Purpose:     GRacket Windows event loop
 * Author:      Matthew Flatt
 * Created:     1996
 * Copyright:   (c) 2004-2010 PLT Scheme Inc.
 * Copyright:   (c) 1996, Matthew Flatt
 */


#if defined(MZ_PRECISE_GC)
# include "wx.h"
#endif
#include "wx_main.h"
#include "wx_utils.h"
#include "scheme.h"
#include "wx_dialg.h"

#include "gracket.h"

#pragma optimize("", off)

#define wxLOG_EVENTS 0
#if wxLOG_EVENTS
static FILE *log;
#endif

#ifndef MZ_PRECISE_GC
# define HIDE_FROM_XFORM(x) x
#endif

void mred_log_msg(const char *msg, ...);

#define OS_SEMAPHORE_TYPE HANDLE

#include "../racket/src/schwinfd.h"

#include <winsock.h>

extern long last_msg_time;

extern "C" {
  struct Scheme_Thread_Memory *scheme_remember_thread(void *);
  void scheme_forget_thread(struct Scheme_Thread_Memory *);
};

static volatile int need_quit;

extern void wxDoPreGM(void);
extern void wxDoPostGM(void);
extern int wxCheckMousePosition();
extern void wxDoLeaveEvent(wxWindow *w, int x, int y, int flags);
extern LRESULT APIENTRY wxWndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
extern struct MrEdContext *MrEdGetContext(wxObject *w);
extern void MrEdQueueInEventspace(void *context, Scheme_Object *thunk);

class LeaveEvent {
public:
  wxWindow *wnd;
  int x, y, flags;
  LeaveEvent *next;
  void *saferef;
};

#ifdef MZ_PRECISE_GC
# define WRAP_SAFEREF(x) (void *)GC_malloc_immobile_box(GC_malloc_weak_box(gcOBJ_TO_PTR(x), NULL, 0))
# define FREE_SAFEREF(x) GC_free_immobile_box((void **)x)
typedef struct {
  short tag;
  short filler_used_for_hashing;
  void *val;
} wxWeak_Box;
# define GET_SAFEREF(x) ((*(void **)x) ? gcPTR_TO_OBJ((*(wxWeak_Box **)x)->val) : NULL)
#else
# define WRAP_SAFEREF(x) (scheme_dont_gc_ptr(x), x)
# define FREE_SAFEREF(x) scheme_gc_ptr_ok(x)
# define GET_SAFEREF(x) x
#endif

static void CALLBACK HETRunSome(HWND hwnd, UINT uMsg, UINT idEvent, DWORD dwTime);
static Scheme_Object *call_wnd_proc(void *data, int argc, Scheme_Object **argv);

static int WM_MRED_LEAVE;

#ifndef WM_MOUSEWHEEL
# define WM_MOUSEWHEEL 0x020A
#endif

void MrEdInitFirstContext(MrEdContext *c)
{
}

void MrEdInitNewContext(MrEdContext *c)
{
}

void MrEdDestroyContext(MrEdFinalizedContext *)
{
}

void MrEdSyncCurrentDir(void)
{
  Scheme_Object *v;
  
  v = scheme_get_param(scheme_current_config(), MZCONFIG_CURRENT_DIRECTORY);
  scheme_os_setcwd(SCHEME_PATH_VAL(v), 0);
}

int MrEdGetDoubleTime(void)
{
  return GetDoubleClickTime();
}

extern wxWindow *wxHWNDtoWindow(HWND);

static MrEdContext *GetContext(HWND hwnd)
{
  wxWindow *w;
  HWND next = hwnd, wnd;

  do {
    do {
      wnd = next;
      next = GetParent(next);
    } while (next);
    next = GetWindow(wnd, GW_OWNER);
  } while (next);

  w = wxHWNDtoWindow(wnd);

  if (!w)
    return NULL;

  if (wxSubType(w->__type, wxTYPE_FRAME))
    return (MrEdContext *)((wxFrame *)w)->context;
  else if (wxSubType(w->__type, wxTYPE_DIALOG_BOX))
    return (MrEdContext *)((wxDialogBox *)w)->context;
  else
    return NULL;
}

/**********************************************************************/

typedef struct {
  MrEdContext *c, *c_return;
  MSG *msg;
  int remove;
  HWND wnd;
} CheckInfo;

static BOOL CALLBACK CheckWindow(HWND wnd, LPARAM param)
{
  CheckInfo *info = (CheckInfo *)param;
  MrEdContext *c;

  c = GetContext(wnd);

  if ((!info->c && (!c || c->ready)) || (info->c == c)) {
    if (c && c->queued_leaves) {
      if (info->remove) {
	if (!WM_MRED_LEAVE)
	  WM_MRED_LEAVE = RegisterWindowMessage("MrEd_Leave_" MRED_GUID);
	info->wnd = wnd;
	info->c_return = c;
	info->msg->message = WM_MRED_LEAVE;
	{
	  if (!c->queued_leaves->saferef) {
	    void *sr;
	    sr = WRAP_SAFEREF(c->queued_leaves);
	    c->queued_leaves->saferef = sr;
	  }
	}
	info->msg->lParam = (long)c->queued_leaves->saferef;
	c->queued_leaves = c->queued_leaves->next;
      }
      return FALSE;
    }

    if (PeekMessage(info->msg, wnd, NULL, NULL,
                    info->remove ? PM_REMOVE : PM_NOREMOVE)) {
      info->wnd = wnd;
      info->c_return = c;
      scheme_notify_sleep_progress();
      return FALSE;
    }
  }

  return TRUE;
}

int FindReady(MrEdContext *c, MSG *msg, int remove, MrEdContext **c_return)
{
  MSG backup;
  CheckInfo info;
  int result = 0;

  if (!msg)
    msg = &backup;

  info.c = c;
  info.msg = msg;
  info.remove = remove;

  if (!EnumThreadWindows(GetCurrentThreadId(), (WNDENUMPROC)CheckWindow, (LPARAM)&info)) {
    if (c_return)
      *c_return = info.c_return;
    result = 1;
  }

  /* XP uses messages above 0x4000 to hilite items in the task bar,
     etc. In any case, these messages won't be handled by us, so they
     can't trigger Scheme code. (If 0x4000 handling ends up sending a
     window a message that we *do* handle, we'll end up ignoring it,
     as we do for all unexpected messages that can call into
     Scheme.) */
  {
    MSG pmsg;
    while (PeekMessage(&pmsg, NULL, 0x4000, 0xFFFF, PM_REMOVE)) {
      wxTranslateMessage(&pmsg);
      DispatchMessage(&pmsg);
      scheme_notify_sleep_progress();
    }
  }

  return result;
}

int MrEdGetNextEvent(int check_only, int current_only,
		     MSG *event, MrEdContext **which)
{
  MrEdContext *c;

  if (which)
    *which = NULL;

  if (need_quit) {
    /* This function can be called in any thread; it queues as necessary: */
    need_quit = 0;
    wxDrop_Quit();
  }

  if (current_only)
    c = MrEdGetContext();
  else
    c = NULL;

  wxCheckMousePosition();

  return FindReady(c, event, !check_only, which);
}

static HWND can_trampoline_win;
static HWND need_trampoline_win;
static UINT need_trampoline_message;
static WPARAM need_trampoline_wparam;
static LPARAM need_trampoline_lparam;
static WNDPROC need_trampoline_proc;
int wx_trampolining;

static int HETDispatchMessage(void *_msg)
{
  MSG *msg = (MSG *)_msg;
  DispatchMessage(msg);
  return 0;
}

void MrEdDispatchEvent(MSG *msg)
{
  switch (msg->message) {
  case WM_RBUTTONDOWN:
  case WM_RBUTTONUP:
  case WM_RBUTTONDBLCLK:
  case WM_MBUTTONDOWN:
  case WM_MBUTTONUP:
  case WM_MBUTTONDBLCLK:
  case WM_LBUTTONDOWN:
  case WM_LBUTTONUP:
  case WM_LBUTTONDBLCLK:
  case WM_MOUSEMOVE:
  case WM_MOUSEWHEEL:
  case WM_NCLBUTTONDOWN:
  case WM_NCRBUTTONDOWN:
  case WM_NCMBUTTONDOWN:
  case WM_NCLBUTTONDBLCLK:
  case WM_NCRBUTTONDBLCLK:
  case WM_NCMBUTTONDBLCLK:
  case WM_NCMOUSEMOVE:
  case WM_NCLBUTTONUP:
  case WM_NCRBUTTONUP:
  case WM_NCMBUTTONUP:
    wxUnhideCursor();
    break;
  default:
    break;
  }

  if (WM_MRED_LEAVE && (msg->message == WM_MRED_LEAVE)) {
    /* Queued leave event */
    void *sr = (void *)msg->lParam;
    LeaveEvent *e;
    e = (LeaveEvent *)GET_SAFEREF(sr);
    FREE_SAFEREF(sr);
    if (e)
      wxDoLeaveEvent(e->wnd, e->x, e->y, e->flags);
  } else if (!wxTheApp->ProcessMessage(msg)) {
#if wxLOG_EVENTS
    if (!log)
      log = fopen("evtlog", "w");
    fprintf(log, "{SEND %lx (%lx) %lx\n",
	    msg->hwnd, GetContext(msg->hwnd),
	    msg->message);
    fflush(log);
#endif

    wxTranslateMessage(msg);

    can_trampoline_win = msg->hwnd;
    last_msg_time = msg->time;
      
    DispatchMessage(msg);

#if wxLOG_EVENTS
    if (!log)
      log = fopen("evtlog", "w");
    fprintf(log, " SENT %lx (%lx) %lx %lx %lx}\n",
	    msg->hwnd, GetContext(msg->hwnd), msg->message,
	    need_trampoline_win, need_trampoline_message);
    fflush(log);
#endif

    can_trampoline_win = 0;

    /* See wxEventTrampoline, below: */
    {
      int iterations;

      for (iterations = 0; iterations < 10; iterations++) {
	if (msg->hwnd && (need_trampoline_win == msg->hwnd)) {
	  HWND win = need_trampoline_win;
	  need_trampoline_win = 0;
	  wx_trampolining = 1;
	  if (iterations < 9)
	    can_trampoline_win = win;
	  else
	    can_trampoline_win = NULL;
	  need_trampoline_proc(win, need_trampoline_message,
			       need_trampoline_wparam, need_trampoline_lparam);
	} else
	  break;
      }
    }
  }
}

void wxCopyData(LPARAM lParam)
{
  /* Is this a message from another GRacket? */
  int len;
  COPYDATASTRUCT *cd;
  len = strlen(MRED_GUID);
  cd = (COPYDATASTRUCT *)lParam;
  if ((cd->cbData > len + 4 + sizeof(DWORD)) 
      && !strncmp((char *)cd->lpData, MRED_GUID, len)) {
    if (!strncmp((char *)cd->lpData + len, "OPEN", 4)) {
      /* This is an "OPEN" event, with a command line.
	 The command line's argv (sans argv[0]) is
	 expressed as a DWORD for the number of args,
	 followed by each arg. Each arg is a DWORD
	 for the number of chars and then the chars. */
      DWORD w;
      int cnt, i, pos;
      char **argv, *s;
      memcpy(&w, (char *)cd->lpData + len + 4, sizeof(DWORD));
      cnt = w;
      pos = len + 4 + sizeof(DWORD);
      argv = new char*[cnt];
      for (i = 0; i < cnt; i++) {
	if (pos + sizeof(DWORD) <= cd->cbData) {
	  memcpy(&w, (char *)cd->lpData + pos, sizeof(DWORD));
	  pos += sizeof(DWORD);
	  if (w >= 0 && (pos + w <= cd->cbData)) {
	    s = new WXGC_ATOMIC char[w + 1];
	    memcpy(s, (char *)cd->lpData + pos, w);
	    s[w] = NULL;
	    argv[i] = s;
	    pos += w;
	  } else {
	    cnt = i;
	    break;
	  }
	} else {
	  cnt = i;
	  break;
	}
      }
      wxDrop_Runtime(argv, cnt);
    }
  }
}

int wxEventTrampoline(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam,
		      LRESULT *res, WNDPROC proc)
  /* The Windows event dispatcher doesn't like GRacket's thread
     implementation.  In particular, if a message causes a thread
     switch or kill, because it triggers Scheme code, then event
     dispatches don't return in the way that Windows expects.

     We therefore set up a special trampoline for events that trigger
     Scheme code. For example, WM_LBUTTONDOWN. Other events, such as
     WM_PAINT or WM_SETFOCUS messages, are handled through a
     "trampoline" in the form of queued callbacks, since those events
     may be received other than through an event dispatch (i.e., some
     other Windows toolbox call triggers and event that it sends
     directly).

     For trampolined events, we return from the Windows-sponsored
     message send, and then re-send the message (where the re-send
     might trigger Scheme code). These events *cannot* be handled
     without a trampoline. For example, if somehow the WM_LBUTTONDOWN
     message is sent directly to a window, we can't handle it. The
     wx_start_win_event() function returns 0 to say "give up".

     For certain kinds of events, the callback queueing is most easily
     implemented in Scheme within mred.ss. For those cases, we put
     Racket into atomic mode while handling the event. The "mred.ss"
     implementation promises to run quickly (and not call user code).

     Scrolling is a special case. To implement interactive scrolling,
     we jump into a special mode started by wxHiEventTrampoline().
     This mode calls into Windows to implement scrolling, but handles
     WM_HSCROLL and WM_VSCROLL messages specially. See mred.cxx for details
     on wxHiEventTrampoline. */
{
  int tramp;

#if wxLOG_EVENTS
  if (!log)
    log = fopen("evtlog", "w");
  fprintf(log,
	  "[TCHECK %lx %lx (%lx) %lx"
# ifdef MZ_PRECISE_GC
	  " %lx"
# endif
	  "]\n",
	  scheme_current_thread,
	  hWnd, can_trampoline_win, message
# ifdef MZ_PRECISE_GC
	  , ((void **)__gc_var_stack__[0])[0]
# endif
	  );
  fflush(log);
#endif


  switch (message) {
  case WM_HSCROLL:
  case WM_VSCROLL:
    /* Special cases */
    tramp = 0;
    break;
  case WM_QUERYENDSESSION:
    /* Always allow end-session here; see wx_pdf for the effective guardian */
    tramp = 1;
    *res = 1;
    break;
  case WM_ENDSESSION:
  case WM_CLOSE:
    tramp = 1;
    *res = 1;
    break;
  case WM_RBUTTONDOWN:
  case WM_RBUTTONUP:
  case WM_RBUTTONDBLCLK:
  case WM_MBUTTONDOWN:
  case WM_MBUTTONUP:
  case WM_MBUTTONDBLCLK:
  case WM_LBUTTONDOWN:
  case WM_LBUTTONUP:
  case WM_LBUTTONDBLCLK:
  case WM_MOUSEMOVE:
  case WM_MOUSEWHEEL:
  case WM_SYSKEYUP:
  case WM_SYSKEYDOWN:
  case WM_KEYUP:
  case WM_KEYDOWN:
  case WM_SYSCHAR:
  case WM_CHAR:
  case WM_INITMENU:
  case WM_DROPFILES:
    tramp = 1;
    *res = 1;
    break;
    /* These three are for pre-emptive WM_INITMENU
       and for on-pre-event over scrollbars plus interactive scrolling */
  case WM_NCLBUTTONDOWN:
  case WM_NCRBUTTONDOWN:
  case WM_NCMBUTTONDOWN:
  case WM_NCLBUTTONDBLCLK:
  case WM_NCRBUTTONDBLCLK:
  case WM_NCMBUTTONDBLCLK:
    if ((wParam == HTMENU) || (wParam == HTVSCROLL) || (wParam == HTHSCROLL)) {
      tramp = 1;
      *res = 1;
    } else
      tramp = 0;
    break;
    /* These are for on-pre-event over scrollbars plus interactive scrolling */
  case WM_NCMOUSEMOVE:
  case WM_NCLBUTTONUP:
  case WM_NCRBUTTONUP:
  case WM_NCMBUTTONUP:
    if ((wParam == HTVSCROLL) || (wParam == HTHSCROLL)) {
      tramp = 1;
      *res = 1;
    } else
      tramp = 1;
    break;
  default:
    tramp = 0;
    break;
  }

  if (can_trampoline_win != hWnd) {
    if (tramp)
      return 1;
    return 0;
  }

  if (tramp) {
    can_trampoline_win = 0;
    need_trampoline_win = hWnd;
    need_trampoline_proc = proc;
    need_trampoline_message = message;
    need_trampoline_wparam = wParam;
    need_trampoline_lparam = lParam;
    return 1;
  } else
    return 0;
}

int wx_start_win_event(const char *who, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam, int tramp, LONG *_retval)
{
  /* See wxEventTrampoline notes above. */

#if wxLOG_EVENTS
  if (!log)
    log = fopen("evtlog", "w");
  fprintf(log, "(%lx %lx %lx[%d] %s %d"
# ifdef MZ_PRECISE_GC
	  " <%lx %lx %lx>"
# endif
	  "\n",
	  scheme_current_thread, hWnd, message, wParam, who, tramp
# ifdef MZ_PRECISE_GC
	  , GC_variable_stack
	  , ((void **)__gc_var_stack__[0])[0]
	  , ((void **)((void **)__gc_var_stack__[0])[0])[0]
# endif
	  );
  fflush(log);
#endif

  if (!tramp && scheme_current_thread) {
    Scheme_Object *v;
    HiEventTramp *het;

    v = scheme_extract_one_cc_mark(NULL, mred_het_key);
    if (!v)
      het = NULL;
    else
      het = (HiEventTramp *)SCHEME_CAR(v);

    if (het) {
      /* we're in restricted mode; general calls into Scheme are bad */
      switch (message) {
	/* These shouldn't happen; reject them if they do! */
      case WM_ACTIVATE:
      case WM_NCACTIVATE:
      case WM_SETFOCUS:
      case WM_KILLFOCUS:
      case WM_SIZE:
      case WM_MOVE:
      case WM_COMMAND:
      case WM_MDIACTIVATE:
      case WM_CLOSE:
#if wxLOG_EVENTS
	fprintf(log, " RESTRICTED)\n");
	fflush(log);
#endif
	return 0;
      case WM_VSCROLL:
      case WM_HSCROLL:
	/* need to re-queue the scroll event (in the MrEd middle queue) */
#if wxLOG_EVENTS
	fprintf(log, "_scroll_ %lx\n", GetCurrentThreadId());
#endif
	{
	  MSG *msg;
	  Scheme_Object *thunk;
	  msg = (MSG *)scheme_malloc_atomic(sizeof(MSG));
#if wxLOG_EVENTS
	  fprintf(log, "_scroll1_\n");
#endif
	  msg->hwnd = hWnd;
	  msg->message = message;
	  msg->wParam = wParam;
	  msg->lParam = lParam;
	  thunk = scheme_make_closed_prim(call_wnd_proc, (Scheme_Object *)msg);
#if wxLOG_EVENTS
	fprintf(log, "_scroll2_\n");
#endif
	  MrEdQueueInEventspace(MrEdGetContext(NULL), thunk);
	}
#if wxLOG_EVENTS
	fprintf(log, "_scrolly_ %d\n", het->yielding);
#endif
	if (!het->yielding) {
	  if (het->timer_on) {
	    het->timer_on = 0;
	    KillTimer(NULL, het->timer_id);
	  }
#if wxLOG_EVENTS
	  fprintf(log, "{HET\n");
#endif
	  mred_het_run_some(NULL, NULL);
#if wxLOG_EVENTS
	  fprintf(log, "HET}\n");
#endif
	  if (het->in_progress && !het->timer_on) {
	    /* Make a timer event so that we get more time... */
	    het->timer_on = 1;
	    het->timer_id = SetTimer(0, NULL, 100, HETRunSome);
	  }
#if wxLOG_EVENTS
	  if (het->in_progress)
	    fprintf(log, " HET_START)\n");
	  else
	    fprintf(log, " HET_DONE)\n");
	  fflush(log);
#endif
	} else {
#if wxLOG_EVENTS
	  fprintf(log, " NESTED)\n");
	  fflush(log);
#endif
	}
	return 0;
      default:
	/* anything else is ok, because it doesn't call Scheme */
	break;
      }
    }
  }

  if (!tramp) {
    switch (message) {
    case WM_QUERYENDSESSION:
      *_retval = 1;
      return 0;
    case WM_NCRBUTTONDOWN:
    case WM_NCRBUTTONUP:
    case WM_NCRBUTTONDBLCLK:
    case WM_NCMBUTTONDOWN:
    case WM_NCMBUTTONUP:
    case WM_NCMBUTTONDBLCLK:
    case WM_NCLBUTTONDOWN:
    case WM_NCLBUTTONUP:
    case WM_NCLBUTTONDBLCLK:
    case WM_NCMOUSEMOVE:
      if ((wParam != HTVSCROLL) && (wParam != HTHSCROLL))
	break;
    case WM_CLOSE: /* ^^^^ fallthrough &&&& */
    case WM_RBUTTONDOWN:
    case WM_RBUTTONUP:
    case WM_RBUTTONDBLCLK:
    case WM_MBUTTONDOWN:
    case WM_MBUTTONUP:
    case WM_MBUTTONDBLCLK:
    case WM_LBUTTONDOWN:
    case WM_LBUTTONUP:
    case WM_LBUTTONDBLCLK:
    case WM_MOUSEMOVE:
    case WM_MOUSEWHEEL:
    case WM_SYSKEYUP:
    case WM_SYSKEYDOWN:
    case WM_KEYUP:
    case WM_KEYDOWN:
    case WM_SYSCHAR:
    case WM_CHAR:
    case WM_INITMENU:
    case WM_DROPFILES:
#if wxLOG_EVENTS
      fprintf(log, " CAN'T HANDLE!)\n");
      fflush(log);
#endif
      return 0;
      break;
    default:
      /* non-tramp ok */
      break;
    }
  }

  if (!tramp)
    scheme_start_atomic();

#if wxLOG_EVENTS
  fprintf(log, " ...\n");
#endif

  return 1;
}

void wx_end_win_event(const char *who, HWND hWnd, UINT message, int tramp)
{
  /* See wxEventTrampoline notes above. */

#if wxLOG_EVENTS
  fprintf(log, " %lx %lx %lx %s %d)"
# ifdef MZ_PRECISE_GC
	  " <%lx %lx>"
# endif
	  "\n",
	  scheme_current_thread, hWnd, message, who, tramp
# ifdef MZ_PRECISE_GC
	  , GC_variable_stack
	  , ((void **)__gc_var_stack__[0])[0]
# endif
	  );
  fflush(log);
#endif

  if (!tramp)
    scheme_end_atomic_no_swap();


  if (!tramp && ((message == WM_VSCROLL) || (message == WM_HSCROLL)) && scheme_current_thread) {
    HiEventTramp *het;
    Scheme_Object *v;

    v = scheme_extract_one_cc_mark(NULL, mred_het_key);
    if (!v)
      het = NULL;
    else
      het = (HiEventTramp *)SCHEME_CAR(v);

    if (het) {
      mred_het_run_some(NULL, NULL);
      if (het->in_progress && !het->timer_on) {
	/* Make a timer event so that we get more time... */
	het->timer_on = 1;
	het->timer_id = SetTimer(0, NULL, 100, HETRunSome);
      }
    }
  }
}

static Scheme_Object *call_wnd_proc(void *data, int argc, Scheme_Object **argv)
{
  MSG *msg = (MSG *)data;

#if wxLOG_EVENTS
  fprintf(log, "{CWP\n");
#endif

  wx_trampolining = 1;
  wxWndProc(msg->hwnd, msg->message, msg->wParam, msg->lParam);

#if wxLOG_EVENTS
  fprintf(log, " CWP}\n");
#endif

  return scheme_void;
}

static void CALLBACK HETRunSome(HWND hwnd, UINT uMsg, UINT idEvent, DWORD dwTime)
{
  HiEventTramp *het;
  Scheme_Object *v;
  
  v = scheme_extract_one_cc_mark(NULL, mred_het_key);
  if (!v)
    het = NULL;
  else
    het = (HiEventTramp *)SCHEME_CAR(v);

  if (het) {
#if wxLOG_EVENTS
    fprintf(log, "(HET_TIMER_CONT\n");
#endif
    if (het->timer_on) {
      het->timer_on = 0;
      KillTimer(NULL, het->timer_id);
    }
    mred_het_run_some(NULL, NULL);
    if (het->in_progress) {
      het->timer_on = 1;
      het->timer_id = SetTimer(0, NULL, 100, HETRunSome);
    }
#if wxLOG_EVENTS
    if (het->in_progress)
      fprintf(log, " HET_TIMER_SUSPEND)\n");
    else
      fprintf(log, " HET_TIMER_DONE)\n");
#endif
  }
}

/***************************************************************************/

void wxPostQueryEndSession()
  /* Called from non-main Windows thread */
{
  need_quit = 1;
}

/***************************************************************************/

int MrEdCheckForBreak(void)
{
  HWND w;

  w = GetActiveWindow();

  if (MrEdGetContext() != GetContext(w))
    return 0;

  {
    SHORT hit = (SHORT)0x8000;
    SHORT hitnow = (SHORT)0x0001;
    SHORT c, shift, control;

    c = GetAsyncKeyState('C');
#if BREAKING_REQUIRES_SHIFT
    shift = GetAsyncKeyState(VK_SHIFT);
#else
    shift = hit;
#endif
    control = GetAsyncKeyState(VK_CONTROL);

    return ((c & hit) && (c & hitnow) && (control & hit) && (shift & hit));
  }
}

void MrEdMSWSleep(float secs, void *fds, SLEEP_PROC_PTR mzsleep)
{
  if (fds && ((win_extended_fd_set *)fds)->no_sleep)
    return;

  if (wxCheckMousePosition())
    return;

  scheme_add_fd_eventmask(fds, QS_ALLINPUT);
  mzsleep(secs, fds);
}

void wxQueueLeaveEvent(void *ctx, wxWindow *wnd, int x, int y, int flags)
{
  MrEdContext *c = (MrEdContext *)ctx;
  LeaveEvent *e, *prev, *n;

  e = new LeaveEvent();

  e->wnd = wnd;
  e->x = x;
  e->y = y;
  e->flags = flags;
  e->next = NULL;

  prev = NULL;
  for (n = c->queued_leaves; n; n = n->next) {
    prev = n;
  }

  if (prev)
    prev->next = e;
  else
    c->queued_leaves = e;
}

/**********************************************************************/

/* For Windows 95/98/Me, it's important to release all GDI object
   handles on exit. The gdi_objects table maps integers to pairs of
   HANDLES.  The integer is the handle | 0x1, which ensures that the
   key looks like a Scheme fixnum. The pair of handles has something
   in the first slot for !(handle & 0x1), and something in the second
   slot for (handle & 0x1). */

static Scheme_Hash_Table *gdi_objects;
static void (*orig_exit)(int);

void mred_clean_up_gdi_objects(void)
{
  int i;

  if ((long)gdi_objects == 0x1)
    return;

  for (i = 0; i < gdi_objects->size; i++) {
    if (gdi_objects->vals[i]) {
      Scheme_Object *key;
      HANDLE *val;
      key = gdi_objects->keys[i];
      val = (HANDLE *)gdi_objects->vals[i];
      scheme_hash_set(gdi_objects, key, NULL);
      if (val[0])
	DeleteObject(val[0]);
      if (val[1])
	DeleteObject(val[1]);
    }
  }
}

static void clean_up_and_exit(int v)
{
  mred_clean_up_gdi_objects();
  wxGDIShutdown();
  if (orig_exit)
    orig_exit(v);
  exit(v);
}

void RegisterGDIObject(HANDLE x)
{
  if (!gdi_objects) {
    /* Only need this table if we're in 95/98/Me */
    OSVERSIONINFO info;
    info.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    GetVersionEx(&info);
    if (info.dwPlatformId != VER_PLATFORM_WIN32_NT) {
      /* Need it... */
      wxREGGLOB(gdi_objects);
      gdi_objects = scheme_make_hash_table(SCHEME_hash_ptr);
      orig_exit = scheme_exit;
      scheme_exit = clean_up_and_exit;
    } else
      gdi_objects = (Scheme_Hash_Table *)0x1;
  }

  if ((long)gdi_objects == 0x1)
    return;

  if (x) {
    Scheme_Object *key;
    HANDLE *v;
    key = (Scheme_Object *)(((long)x) | 0x1);
    v = (HANDLE *)scheme_hash_get(gdi_objects, key);
    if (!v) {
      v = (HANDLE *)scheme_malloc_atomic(sizeof(HANDLE)*2);
      v[0] = v[1] = NULL;
    }
    if (((long)x) & 1)
      v[1] = x;
    else
      v[0] = x;
    scheme_hash_set(gdi_objects, key, (Scheme_Object *)v);
  }
}

void DeleteRegisteredGDIObject(HANDLE x)
{
  Scheme_Object *key;
  HANDLE *v;

  if ((long)gdi_objects != 0x1) {
    key = (Scheme_Object *)(((long)x) | 0x1);
    v = (HANDLE *)scheme_hash_get(gdi_objects, key);
    if (v) {
      if (((long)x) & 1)
	v[1] = NULL;
      else
	v[0] = NULL;

      if (!v[0] && !v[1]) {
	/* Remove from hash table: */
	scheme_hash_set(gdi_objects, key, NULL);
      }
    }
  }

  DeleteObject(x);
}

/**************************************************/

void mred_log_msg(const char *msg, ...)
{
  long len;
  GC_CAN_IGNORE va_list args;
  FILE *f;

  f = fopen("mredlog", "a");

  fprintf(f, "0x%lx ", scheme_current_thread);

  HIDE_FROM_XFORM(va_start(args, msg));
  len = vfprintf(f, msg, args);
  HIDE_FROM_XFORM(va_end(args));

  fclose(f);
}

