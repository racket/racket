/*
 * File:	wb_dialg.h
 * Purpose:	wxDialogBox and common dialog declarations
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wxb_dialgh
#define wxb_dialgh

#include "common.h"
#include "wx_panel.h"

#ifdef IN_CPROTO
typedef       void    *wxbDialogBox ;
#else

// Dialog boxes
class wxbDialogBox: public wxFrame
{
 public:
  Bool modal;
  Bool is_show;

  wxbDialogBox(void);
  wxbDialogBox(wxWindow *parent, char *title, Bool modal = FALSE,
              int x = wxDEFAULT_POSITION, int y = wxDEFAULT_POSITION,
              int width = -1, int height = -1, long style = 0, char *name = "panel");
  ~wxbDialogBox();

  Bool Create(wxWindow *window, char *title, Bool modal = FALSE,
              int x = wxDEFAULT_POSITION, int y = wxDEFAULT_POSITION,
              int width = -1, int height = -1, long style = 0, char *name = "panel");

  virtual void Iconize(Bool iconize) = 0;
  virtual Bool Iconized(void) = 0;

  void Centre(int direction = wxBOTH);
};

#define wxOPEN 1
#define wxSAVE 2
#define wxOVERWRITE_PROMPT 4
#define wxHIDE_READONLY 8
#define wxDIR_ONLY	16
#define wxFILES_ONLY 32
#define wxMULTIOPEN 64
#define wxGETDIR 128
#define wxBUNDLES_OK 256
#define wxBUNDLES_ENTER 512

// Generic file load dialog
char * wxLoadFileSelector(char *what = "Text", char *extension = "txt", char *default_name = NULL);

// Generic file save dialog
char * wxSaveFileSelector(char *what = "Text", char *extension = "txt", char *default_name = NULL);
// File selector
char *wxFileSelector(char *message = "Select a file", char *default_path = NULL,
                     char *default_filename = NULL, char *default_extension = NULL,
                     char *wildcard = "*.*", int flags = 0,
                     wxWindow *parent = NULL, int x = -1, int y = -1);

#endif // IN_CPROTO
#endif // wxb_dialgh
