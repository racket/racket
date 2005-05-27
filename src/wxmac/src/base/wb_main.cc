/*
 * File:	wb_main.cc
 * Purpose:	wxApp implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#ifdef __GNUG__
#pragma implementation
#endif
#endif

#include "common.h"
#include "wx_setup.h"

#include "wx_panel.h"
#include "wx_frame.h"
#include "wx_main.h"
#include "wx_utils.h"
#include "wx_gdi.h"
#include "wx_dc.h"
#include "wx_dccan.h"
#include "wx_dialg.h"
#include "wx_types.h"
#include "wx_dcps.h"
#include "wx_sysev.h"

#include <string.h>

extern char *wxBuffer;

wxApp *wxTheApp = NULL;

wxbApp::wxbApp()
{
  __type = wxTYPE_APP;
  wx_class = NULL;
  wantDebugOutput = TRUE ;
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

// prototypes for wxREGGLOB'ing functions:
void wxRegisterSplinePointList();
void wxRegisterAbortWindow();
void wxRegisterEntered();
void wxRegisterOldFrontWindow();
void wxRegisterFocusWindow();
void wxRegisterCurCursor();
void wxRegisterLastInstalledBar();


void wxCommonInit(void)
{
  wx_init_patterns();
  wxREGGLOB(wxBuffer);
  wxBuffer = new char[1500];
  wxREGGLOB(wxTheColourDatabase);
  wxTheColourDatabase = new wxColourDatabase();
  wxTheColourDatabase->Initialize();
  wxInitializeFontNameDirectory();
  wxInitializeStockObjects();
  wxInitStandardTypes();
  wxREGGLOB(wxThePrintPaperDatabase);
  wxThePrintPaperDatabase = new wxPrintPaperDatabase;
  wxREGGLOB(wxWindow::gMouseWindow);
  wxRegisterAbortWindow();
  wxRegisterSplinePointList();
  wxRegisterEntered();
  wxRegisterOldFrontWindow();
  wxRegisterFocusWindow();
  wxRegisterCurCursor();
  wxRegisterLastInstalledBar();
}

void wxCommonCleanUp(void)
{
}

