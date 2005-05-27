/*
 * File:	wb_main.h
 * Purpose:	wxApp declaration and a few other functions.
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wxb_mainh
#define wxb_mainh

#include "common.h"
#include "wx_obj.h"

#ifdef IN_CPROTO
typedef       void    *wxbApp ;
#else

class wxFrame;
class wxApp ;

// Represents the application. Derive OnInit and declare
// a new App object to start application
class wxbApp: public wxObject
{
 public:
  int wantDebugOutput ;
  char *wx_class;
  wxFrame *wx_frame;
  int argc;
  char **argv;
  Bool death_processed;
  void (*work_proc)(wxApp*app) ; // work procedure

  wxbApp();
  ~wxbApp(void);
  virtual wxFrame *OnInit(void);
  virtual int OnExit(void);
  virtual int MainLoop(void) = 0;
  virtual Bool Initialized(void);
  virtual Bool Pending(void) = 0 ;
  virtual void Dispatch(void) = 0 ;
};

extern wxApp *wxTheApp;

void wxCleanUp(void);
void wxCommonCleanUp(void); // Call this from the platform's wxCleanUp()
void wxCommonInit(void);    // Call this from the platform's initialization

// Force an exit from main loop
void wxExit(void);

// Yield to other apps/messages
Bool wxYield(void);

#endif // IN_CPROTO
#endif
