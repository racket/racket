/*
 * File:	wb_main.h
 * Purpose:	wxApp declaration and a few other functions.
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wxb_mainh
#define wxb_mainh

#include "common.h"
#include "wx_obj.h"

class wxFrame;
class wxWindow;
class wxApp ;

#define wxPRINT_WINDOWS         1
#define wxPRINT_POSTSCRIPT      2

// Represents the application. Derive OnInit and declare
// a new App object to start application
class wxbApp: public wxObject
{
 public:
  char *wx_class;
  char *appName;
  wxFrame *wx_frame;
  int argc;
  char **argv;
  Bool death_processed;

  wxbApp();
  ~wxbApp(void);
  virtual wxFrame *OnInit(void);
  virtual int OnExit(void);
  virtual int MainLoop(void) = 0;
  virtual Bool Initialized(void);
  virtual Bool Pending(void) = 0 ;
  virtual void Dispatch(void) = 0 ;

  virtual char *GetAppName(void);
  virtual void SetAppName(char *name);

  virtual char *GetClassName(void);
  virtual void SetClassName(char *name);
  virtual wxWindow *GetTopWindow(void);
};

extern wxApp *wxTheApp;

void wxCleanUp(void);
void wxCommonCleanUp(void); // Call this from the platform's wxCleanUp()
void wxCommonInit(void);    // Call this from the platform's initialization

// Force an exit from main loop
void wxExit(void);

// Yield to other apps/messages
Bool wxYield(void);

#endif
