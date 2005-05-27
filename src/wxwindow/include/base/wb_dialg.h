/*
 * File:	wb_dialg.h
 * Purpose:	wxDialogBox and common dialog declarations
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wxb_dialgh
#define wxb_dialgh

#include "common.h"
#include "wx_panel.h"

// Dialog boxes
class wxbDialogBox: public wxPanel
{
 protected:
  Bool modal;
  Bool is_show;
 public:

  void *context;

  wxbDialogBox(wxWindow *parent, char *title, Bool modal = FALSE,
              int x = -1, int y = -1,
              int width = -1, int height = -1, long style = 0, char *name = "panel");
  ~wxbDialogBox();

  Bool Create(wxWindow *window, char *title, Bool modal = FALSE,
              int x = -1, int y = -1,
              int width = -1, int height = -1, long style = 0, char *name = "panel");

  virtual void Iconize(Bool iconize) = 0;
  virtual Bool Iconized(void) = 0;

  void Centre(int direction = wxBOTH);
  virtual Bool IsModal(void) { return modal; }
  virtual void SetShowing(Bool show) { is_show = show; }
};

#endif // wxb_dialgh
