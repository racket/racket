/*
 * File:	wb_cmdlg.h
 * Purpose:	Common dialogs: generic declarations
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1995, Julian Smart
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wb_cmdlgh
#define wb_cmdlgh

#include "wx_setup.h"
#include "wx_gdi.h"

// type is an 'or' (|) of wxOK, wxCANCEL, wxYES_NO
// Returns wxYES/NO/OK/CANCEL
int wxbMessageBox(char *message, char *caption = "Message", long style = wxOK|wxCENTRE,
  wxWindow *parent = NULL, int x = -1, int y = -1);

#define wxOPEN 1
#define wxSAVE 2
#define wxOVERWRITE_PROMPT 4
#define wxHIDE_READONLY 8
#define wxMULTIOPEN 16
#define wxGETDIR 32

// Generic file load dialog
char * wxLoadFileSelector(char *what = "Text", char *extension = "txt", char *default_name = NULL);

// Generic file save dialog
char * wxSaveFileSelector(char *what = "Text", char *extension = "txt", char *default_name = NULL);
// File selector
char *wxFileSelector(char *message = "Select a file", char *default_path = NULL,
                     char *default_filename = NULL, char *default_extension = NULL,
                     char *wildcard = "*.*", int flags = 0,
                     wxWindow *parent = NULL, int x = -1, int y = -1);

#endif
