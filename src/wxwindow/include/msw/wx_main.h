/*
 * File:	wx_main.h
 * Purpose:	wxApp declaration and a few other functions.
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_mainh
#define wx_mainh

#include "common.h"
#include "wx_obj.h"
#include "wx_stdev.h"
#include "wb_main.h"

class wxFrame;
class wxKeyEvent;

// Represents the application. Derive OnInit and declare
// a new App object to start application
class wxApp: public wxbApp
{
 public:
  HANDLE hInstance;
  BOOL keep_going;

  // this variable holds the display mode, which is
  // passed to the WinMain function.
  int nCmdShow;

  wxApp();
  ~wxApp(void);

  virtual int MainLoop(void);
  void ExitMainLoop(void);
  Bool Initialized(void);
  virtual Bool Pending(void);
  virtual void Dispatch(void);

  virtual BOOL DoMessage(void);
  virtual BOOL ProcessMessage(MSG* pMsg);
  virtual BOOL OnIdle(void);
};

extern HINSTANCE wxhInstance;

MRED_EXTERN int wxEntry(int argc, char **argv);
MRED_EXTERN void wxQueueLeaveEvent(void *ctx, wxWindow *wnd, int x, int y, int flags);

MRED_EXTERN int wxWinMain(int wm_is_mred,
			  HINSTANCE hInstance, HINSTANCE hPrevInstance, 
			  int argc, char **argv, int nCmdShow,
			  int (*main)(int, char**));

extern void wxGDIStartup(void);
extern void wxGDIShutdown(void);

#endif
