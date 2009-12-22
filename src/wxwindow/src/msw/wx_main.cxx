/*
 * File:	wx_main.cc
 * Purpose:	wxApp implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include <commctrl.h>
#include <string.h>

#include "fafa.h"

#include "..\..\contrib\gauge\zyzgauge.h"

#if defined(MZ_PRECISE_GC)
# include "scheme.h"
#endif

HINSTANCE wxhInstance = 0;

extern wxNonlockingHashTable *wxWinHandleList;
extern wxNonlockingHashTable *wxSliderList;
extern FARPROC wxGenericControlSubClassProc;

extern void wxWindowInit(void);

long last_msg_time;

char wxFrameClassName[]         = "wxFrameClass";
char wxMDIFrameClassName[]      = "wxMDIFrameClass";
char wxMDIChildFrameClassName[] = "wxMDIChildFrameClass";
char wxPanelClassName[]         = "wxPanelClass";
char wxCanvasClassName[]        = "wxCanvasClass";

HICON wxSTD_FRAME_ICON = NULL;

DWORD wx_original_thread_id;

HFONT wxSTATUS_LINE_FONT = NULL;
LRESULT APIENTRY wxWndProc(HWND, UINT, WPARAM, LPARAM);

__declspec(dllexport) void (*wx_post_setup)(void) = NULL;

LRESULT CALLBACK UnhideMouseHook(int nCode, WPARAM wParam, LPARAM lParam)
{
  wxUnhideCursor();
  return CallNextHookEx(0, nCode, wParam, lParam);
}

static void RegisterNoCursor(HINSTANCE hInstance,
			     char *src, char *dest, wchar_t *wsrc, wchar_t *wdest)
{
  WNDCLASSEXW c;
    
  c.cbSize = sizeof(c);
  if (!GetClassInfoExW(hInstance, wsrc, &c))
    wxFatalError("Can't get info for cursorless class");
  c.lpszClassName = wdest;
  c.hCursor = NULL;
  if (!RegisterClassExW(&c))
    wxFatalError("Can't register cursorless class");
}

static void RegisterMyClass(HINSTANCE hInstance,
			    UINT style, HICON icon, HBRUSH bg, char *name)
{
  int ok;
  WNDCLASSW wndclass;
  wchar_t *wname;
    
  memset(&wndclass, 0, sizeof(WNDCLASSW));

  wndclass.style         = style;
  wndclass.lpfnWndProc   = (WNDPROC)wxWndProc;
  wndclass.cbClsExtra    = 0;
  wndclass.cbWndExtra    = sizeof(DWORD);
  wndclass.hInstance     = hInstance;
  wndclass.hIcon         = wxSTD_FRAME_ICON;
  wndclass.hCursor       = NULL;
  wndclass.hbrBackground = bg;
  wndclass.lpszMenuName  = NULL;
  wname = wxWIDE_STRING(name);
  wndclass.lpszClassName = wname;

  ok = RegisterClassW(&wndclass);

  if (!ok)
    wxFatalError("Can't register window class");
}

void wxInitialize(HINSTANCE hInstance)
{
  wxCommonInit();
  wxWindowInit();

  InitFafa(hInstance);
  if (!gaugeInit(hInstance))
    wxFatalError("Cannot initalize Gauge library");

  wx_original_thread_id = GetCurrentThreadId();

  wxSTD_FRAME_ICON = LoadIcon(hInstance, "wxSTD_FRAME");

  {
    RegisterMyClass(hInstance,
		    CS_HREDRAW | CS_VREDRAW,
		    wxSTD_FRAME_ICON,
		    (HBRUSH)(COLOR_BTNFACE+1),
		    wxFrameClassName);

    RegisterMyClass(hInstance,
		    CS_HREDRAW | CS_VREDRAW,
		    wxSTD_FRAME_ICON,
		    (HBRUSH)(COLOR_APPWORKSPACE+1),
		    wxMDIFrameClassName);

    RegisterMyClass(hInstance,
		    CS_HREDRAW | CS_VREDRAW,
		    wxSTD_FRAME_ICON,
		    (HBRUSH)(COLOR_BTNFACE+1),
		    wxMDIChildFrameClassName);

    RegisterMyClass(hInstance,
		    CS_HREDRAW | CS_VREDRAW,
		    NULL,
		    (HBRUSH)(COLOR_BTNFACE+1),
		    wxPanelClassName);

    // Use CS_OWNDC to avoid messing about restoring the context
    // for every graphic operation.
    RegisterMyClass(hInstance,
		    CS_HREDRAW | CS_VREDRAW | CS_OWNDC | CS_DBLCLKS,
		    NULL,
		    (HBRUSH)(COLOR_WINDOW+1),
		    wxCanvasClassName);

    RegisterNoCursor(hInstance, "BUTTON", "wxBUTTON", L"BUTTON", L"wxBUTTON");
    RegisterNoCursor(hInstance, "COMBOBOX", "wxCOMBOBOX", L"COMBOBOX", L"wxCOMBOBOX");
    RegisterNoCursor(hInstance, "LISTBOX", "wxLISTBOX", L"LISTBOX", L"wxLISTBOX");
    RegisterNoCursor(hInstance, "EDIT", "wxEDIT", L"EDIT", L"wxEDIT");
    RegisterNoCursor(hInstance, "STATIC", "wxSTATIC", L"STATIC", L"wxSTATIC");
    RegisterNoCursor(hInstance, WC_TABCONTROL, "wxTABCONTROL", WC_TABCONTROLW, L"wxTABCONTROL");
  }

  wxREGGLOB(wxWinHandleList);
  wxREGGLOB(wxSliderList);

  wxWinHandleList = new wxNonlockingHashTable();
  wxSliderList = new wxNonlockingHashTable();  

  SetWindowsHookEx(WH_MOUSE, UnhideMouseHook, NULL, GetCurrentThreadId());
}


// Cleans up any wxWindows internal structures left lying around
void wxCleanUp(void)
{
  wxCommonCleanUp();

  if (wxSTD_FRAME_ICON)
    DestroyIcon(wxSTD_FRAME_ICON);

  DeleteObject(wxSTATUS_LINE_FONT);
  EndFafa();

  if (wxGenericControlSubClassProc)
    FreeProcInstance(wxGenericControlSubClassProc);
  
  if (wxWinHandleList)
    delete wxWinHandleList;
}

// Main windows entry point

extern void wxInitUserResource(char *s);

static int retValue = 0;

extern void wxCreateApp(void);
extern void wxStartEndSessionThread();

int WM_IS_MRED;

int wxWinMain(int wm_is_mred,
	      HINSTANCE hInstance, HINSTANCE WXUNUSED(hPrevInstance), 
	      int count, char **command, int nCmdShow,
	      int (*main)(int, char**))
{
  wxhInstance = hInstance;
  WM_IS_MRED = wm_is_mred;

  InitCommonControls();

  wxInitialize(hInstance);

  wxCreateApp();

  wxGDIStartup();

  wxStartEndSessionThread();

  // Get application name
  {
    char *name;
    char *d, *p;
    
    name = copystring(command[0]);

    strcpy(name, wxFileNameFromPath(name));
    wxStripExtension(name);
    wxTheApp->SetAppName(name);

    d = getenv("HOMEDRIVE");
    p = getenv("HOMEPATH");

    if (d && p) {
      char *s;
      int l;

      s = new char[strlen(d) + strlen(p) + 12];
      strcpy(s, d);
      strcat(s, p);

      l = strlen(s);
      if (l && (s[l - 1] != '\\')) {
	 s[l] = '\\';
	 s[l + 1] = 0;
      }
      strcat(s, "mred.ini");
    
      wxInitUserResource(s);
    } else {
      char name[1024], *s;
      int i;
      ::GetModuleFileName(hInstance, name, 1023);

       i = strlen(name) - 1;    
       while (i && (name[i] != '\\')) {
         --i;
       }
       if (i)
	 i++;

       s = new char[i + 12];
       memcpy(s, name, i);
       strcpy(s + i, "mred.ini");

       wxInitUserResource(s);
    }
  }

  wxTheApp->hInstance = hInstance;
  // store the show-mode parameter of MSW for (maybe) later use.
  // this can be used to inform the program about special show modes
  // under MSW
  wxTheApp->nCmdShow = nCmdShow;

  return main(count, command);
}

int wxEntry(int argc, char **argv)
{
  wxTheApp->argc = argc;
  wxTheApp->argv = argv;

  wxTheApp->OnInit();

  /* xform.ss bug: if we don't refer to last_msg_time, it gets dropped */
  last_msg_time = 0;
  
  return 0;
}

wxApp::wxApp() : wxbApp()
{
  wxREGGLOB(wxTheApp);
  wxTheApp = this;

  wx_frame = NULL;
  death_processed = FALSE;
  wx_class = NULL;
}

wxApp::~wxApp(void)
{
}

Bool wxApp::Initialized(void)
{
  if (wx_frame)
    return TRUE;
  else
    return FALSE;
}

/*
 * Get and process a message, returning FALSE if WM_QUIT
 * received.
 *
 */
MSG top_msg;
int top_use = 0;

BOOL wxApp::DoMessage(void)
{
  return FALSE;
}

extern void wxDoEvents();
extern void wxDoNextEvent();
extern int wxEventReady();

/*
 * Keep trying to process messages until WM_QUIT
 * received
 */

int wxApp::MainLoop(void)
{
  keep_going = TRUE;
  while (keep_going) {
    wxDoEvents();
  }

  return 1;
}

void wxApp::ExitMainLoop(void)
{
  keep_going = FALSE;
}

Bool wxApp::Pending(void)
{
  return wxEventReady();
}

void wxApp::Dispatch(void)
{
  wxDoNextEvent();
}

/*
 * Give all windows a chance to preprocess
 * the message. Some may have accelerator tables, or have
 * MDI complications.
 */
BOOL wxApp::ProcessMessage(MSG *msg)
{
  HWND hWnd;

  // Anyone for a message? Try youngest descendants first.
  for (hWnd = msg->hwnd; hWnd != NULL; hWnd = ::GetParent(hWnd))
  {
    wxWnd *wnd;
    wnd = wxFindWinFromHandle(hWnd);
    if (wnd)
    {
       if (wnd->ProcessMessage(msg))
         return TRUE;

       // STOP if we've reached the top of the hierarchy!
       if (wx_frame && (wnd == (wxWnd *)wx_frame->handle))
          return FALSE;
    }
  }

  if (wx_frame && ((wxWnd *)wx_frame->handle)->ProcessMessage(msg))
     return TRUE;
  else return FALSE;
}

wxWindow *wxHWNDtoWindow(HWND hwnd)
{
  wxWnd *wnd;

  wnd = wxFindWinFromHandle(hwnd);

  if (wnd)
    return wnd->wx_window;
  else
	return NULL;
}

BOOL wxApp::OnIdle(void)
{
  return FALSE;
}

void wxExit(void)
{
  if (wxTheApp)
    (void)wxTheApp->OnExit();
  wxCleanUp();
  FatalAppExit(0, "Exiting");
}

// Yield to incoming messages
Bool wxYield(void)
{
  while (wxTheApp->Pending()) {
    wxTheApp->Dispatch();
  }

  return TRUE;
}

// Reset background brushes
HBRUSH SetupBackground(HWND wnd)
{
  char tmp[128];

  CreatePensBrushes();
  GetClassName(wnd,tmp,127);
  if (strncmp(tmp,wxCanvasClassName,127)==0
      || strncmp(tmp,wxMDIChildFrameClassName,127)==0)
  {
    SetClassLong(wnd,GCL_HBRBACKGROUND,(LONG)NULL);
    return brushBack;
  } else if (strncmp(tmp,wxFrameClassName,127)==0
	     || strncmp(tmp,wxMDIFrameClassName,127)==0)
  {
    SetClassLong(wnd,GCL_HBRBACKGROUND,(LONG)NULL);
    return brushFrame;
  }

  return NULL;
}
 
