/*
 * File:	wx_dialg.h
 * Purpose:	wxDialogBox and common dialog declarations
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_dialgh
#define wx_dialgh

#include "common.h"
#include "wx_item.h"
#include "wx_check.h"
#include "wx_messg.h"
#include "wb_dialg.h"

// Dialog boxes
class wxDialogBox: public wxbDialogBox
{
 public:
  wxList *disabled_windows;

  wxDialogBox(wxWindow *parent, char *title, Bool modal = FALSE,
              int x = wxDEFAULT_POSITION, int y = wxDEFAULT_POSITION,
              int width = -1, int height = -1, long style = 0,
              char *name = "dialogBox");
  ~wxDialogBox(void);

  Bool Create(wxWindow *parent, char *title, Bool modal=FALSE,
              int x=wxDEFAULT_POSITION, int y=wxDEFAULT_POSITION,
              int width=-1, int height=-1, long style=0,
              char *name="dialogBox");
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void SetClientSize(int width, int height);
  void GetPosition(int *x, int *y);
  Bool Show(Bool show);
  void Iconize(Bool iconize);
  Bool Iconized(void);
  void Fit(void);

  void SetTitle(char *title);
  char *GetTitle(void);

  void EnforceSize(int minw, int minh, int maxw, int maxh, int incw=1, int inch=1);

  virtual void ChangeToGray(Bool gray);

  void SystemMenu(void);
};

#endif // wx_dialgh
