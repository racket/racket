
/*
  Copyright (c) 2004-2010 PLT Scheme Inc.
  Copyright (c) 1997-02 PLT (Matthew Flatt)

  This file exists because of a problem in Windows: when
  a built-in dialog is used (such as the FindFile dialog),
  you have no control over the dispatching of events in
  in that thread's event queue. So we run each dialog in
  its own thread.

  */

#include "wx.h"

#include "wx_pdf.h"
#include "wx_timer.h"
#include <direct.h>

extern "C" {
  struct Scheme_Thread_Memory *scheme_remember_thread(void *);
  void scheme_forget_thread(struct Scheme_Thread_Memory *);
};

typedef struct {
  wxPDF f;
  void *data;
  BOOL result;
  BOOL done;
  BOOL usedir;
  HWND hwnd;
  DWORD main_thread_id;
} PrimData;

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

static long DoPrim(void *data)
{
  PrimData *_data = (PrimData *)data;
  wxPDF pdf;

  pdf = _data->f;

  _data->result = pdf(_data->data, _data->hwnd);
  
  _data->done = 1;

  /* Let the original thread know that we've finished, in case it's
     asleep. The message is arbitrary (i.e., it shouldn't mean
     anything to MrEd). */
  PostThreadMessage(_data->main_thread_id, WM_APP + 79, 0, 0);

  return 0;
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

static int Check(void *d)
{
  return ((PrimData *)d)->done;
}

extern void wxDispatchEventsUntil(int (*f)(void *), void *data);
extern wxWindow *wxHWNDtoWindow(HWND);

BOOL wxPrimitiveDialog(wxPDF f, void *data, int strict)
{
  DWORD id;
  HANDLE th;
  PrimData *_data;
  BOOL result;
  HWND top;
  wxWindow *w;
  wxList *disabled_windows;
  wxChildNode *cnode;   
  wxNode *node;   
  wxChildList *tlw;

  _data = (PrimData *)malloc(sizeof(PrimData));

  _data->f = f;
  _data->data = data;
  _data->usedir = strict;
  _data->done = 0;
  id = GetCurrentThreadId();
  _data->main_thread_id = id;

  /* Make window child of frontmost if it exists in the
     same context, and disable all others in the context. */
  top = GetForegroundWindow();
  w = wxHWNDtoWindow(top);
  if (w) {
    void *wc, *tc;
    if (wxSubType(w->__type, wxTYPE_FRAME))
      wc = ((wxFrame *)w)->context;
    else if (wxSubType(w->__type, wxTYPE_DIALOG_BOX))
      wc = ((wxDialogBox *)w)->context;
    else
      wc = NULL;
    tc = wxGetContextForFrame();
    if (wc != tc)
      top = NULL;
  } else
    top = NULL;

  _data->hwnd = top;

  // Disable other windows:
  disabled_windows = new wxList;
  tlw = wxTopLevelWindows(NULL);
  for (cnode = tlw->First(); cnode; cnode = cnode->Next()) {
    wxWindow *win;
    win = (wxWindow *)cnode->Data();
    if (win && cnode->IsShown()) {
      if (win->GetHWND() != top) {
	disabled_windows->Append(win);
	win->InternalEnable(FALSE);
      }
    }
  }
  
  wxCannotHideCursor();
  
  if (!(th = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)DoPrim, _data, 0, &id))) {
    /* Thread creation failed! We're in trouble, but do `f' anyway. */
    result = f(data, top);
  } else {
    struct Scheme_Thread_Memory *thread_memory;

    thread_memory = scheme_remember_thread((void *)th);

    /* There used to be a call to AttachThreadInput() here.
       That was disasterous, because it interefered with the
       carefully tuned event dispatching of mredmsw.cxx. */

    wxDispatchEventsUntil(Check, (void *)_data);

    WaitForSingleObject(th, INFINITE);
    scheme_forget_thread(thread_memory);
    CloseHandle(th);

    result = _data->result;
  }

  wxCanHideCursor(); 

  free(_data);

  /* Restore other windows: */
  for (node = disabled_windows->First(); node; node = node->Next()) {
    wxWindow *win;
    win = (wxWindow *)node->Data();
    win->InternalEnable(TRUE);
  }

  delete disabled_windows;

  return result;
}

static volatile HWND qes_win;
static volatile HANDLE cancel_sema;
static volatile int waiting_for_cancel = 0, qes_answer = 0;
static HANDLE cs_lock;
extern void wxPostQueryEndSession();

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

static int ResetCancelSema()
{
  int qa;
  WaitForSingleObject(cs_lock, INFINITE);
  cancel_sema = CreateSemaphore(NULL, 0, 1000, NULL);
  qa = qes_answer;
  waiting_for_cancel = 0;
  ReleaseMutex(cs_lock);
  return qa;
}

LRESULT APIENTRY wxEndSessionWatcherWndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  if (message == WM_QUERYENDSESSION) {
    /* Send message on to windows in main thread.
       We expect the main-thread windows to either disallow the
       end session or exit. */
    int qa;

    WaitForSingleObject(cs_lock, INFINITE);
    qa = qes_answer;
    waiting_for_cancel = 1;
    ReleaseMutex(cs_lock);

    if (!qa) {
      wxPostQueryEndSession();
      WaitForSingleObject(cancel_sema, INFINITE);
      qa = ResetCancelSema();
    }
    return qa;
  }

  return ::DefWindowProcW(hWnd, message, wParam, lParam);
}

static long DoEndSessionWin(void *data)
{
  MSG msg;

  qes_win = CreateWindowW(L"wxEndSessionWatcher", L"EndSession Watcher", WS_POPUP,
		                  0, 0, 10, 10,
		                  NULL, NULL, wxhInstance, NULL);

  while (GetMessage(&msg, NULL, 0, 0)) {
    TranslateMessage(&msg);
    DispatchMessage(&msg);
  }

  return 0;
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

void wxEndEndSessionThread()
{
}

void wxNotifyCancelEndSession()
{
  WaitForSingleObject(cs_lock, INFINITE);
  ReleaseSemaphore(cancel_sema, 1, NULL);
  ReleaseMutex(cs_lock);
}

void notify_cancel()
{
  int wfc;

  WaitForSingleObject(cs_lock, INFINITE);
  wfc = waiting_for_cancel;
  qes_answer = 1;
  ReleaseMutex(cs_lock);
	
  if (wfc) {
    wxNotifyCancelEndSession();
    /* Block until the qes window can handle a random message */
    SendMessage(qes_win,
		RegisterWindowMessage("MrEd_Done_B2261834-D535-44dd-8511-A26FC8F97DD0"),
		0, 0);
  }
}

void wxStartEndSessionThread()
{
  DWORD id;
  WNDCLASSW wndclass;
    
  memset(&wndclass, 0, sizeof(WNDCLASSW));

  wndclass.style         = CS_HREDRAW | CS_VREDRAW;
  wndclass.lpfnWndProc   = (WNDPROC)wxEndSessionWatcherWndProc;
  wndclass.cbClsExtra    = 0;
  wndclass.cbWndExtra    = sizeof(DWORD);
  wndclass.hInstance     = wxhInstance;
  wndclass.hIcon         = wxSTD_FRAME_ICON;
  wndclass.hCursor       = NULL;
  wndclass.hbrBackground = (HBRUSH)(COLOR_BTNFACE+1);
  wndclass.lpszMenuName  = NULL;
  wndclass.lpszClassName = L"wxEndSessionWatcher";

  RegisterClassW(&wndclass);

  cs_lock = CreateMutex(NULL, FALSE, NULL);
  ResetCancelSema();

  CreateThread(NULL, (1 << 15), (LPTHREAD_START_ROUTINE)DoEndSessionWin, NULL, 0, &id);

  atexit(notify_cancel);
}
