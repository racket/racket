/*
 * File:	wb_main.cc
 * Purpose:	wxApp implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include <string.h>

extern char *wxBuffer;

wxApp *wxTheApp = NULL;

wxbApp::wxbApp()
{
  __type = wxTYPE_APP;
  wx_class = NULL;
  appName = NULL;
  argc = 0;
  argv = NULL;
  death_processed = FALSE;
  wx_frame = NULL;
}

wxbApp::~wxbApp(void)
{
}

Bool wxbApp::Initialized(void)
{
  return FALSE;
}

wxFrame *wxbApp::OnInit(void)
{
  return NULL;
}

int wxbApp::OnExit(void)
{
  return 0;
}

char *wxbApp::GetAppName(void)
{
  if (appName)
    return appName;
  else if (wx_class)
    return wx_class;
  else return NULL;
}

void wxbApp::SetAppName(char *name)
{
  if (name) {
    appName = copystring(name);
  }
}

char *wxbApp::GetClassName(void)
{
  return wx_class;
}

void wxbApp::SetClassName(char *name)
{
  if (name) {
    wx_class = copystring(name);
  }
}

wxWindow *wxbApp::GetTopWindow(void)
{
  return wx_frame;
}

void wxCommonInit(void)
{
  wxREGGLOB(wxBuffer);
  wxBuffer = new char[1500];
  
  wxInitializeFontNameDirectory();

  wxREGGLOB(wxTheColourDatabase);
  wxTheColourDatabase = new wxColourDatabase(wxKEY_STRING);
  wxTheColourDatabase->Initialize();
  wxInitializeStockObjects();
  wxInitStandardTypes();

  // For PostScript printing
  wxInitializePrintSetupData();
  wxREGGLOB(wxThePrintPaperDatabase);
  wxThePrintPaperDatabase = new wxPrintPaperDatabase;
  wxThePrintPaperDatabase->CreateDatabase();
}

void wxCommonCleanUp(void)
{
  // MrEd takes care of deleting GDI objects
}

