/*
 * File:	wb_rbox.h
 * Purpose:	Radio box
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wxb_rboxh
#define wxb_rboxh

#include "common.h"
#include "wx_panel.h"
#include "wx_item.h"

// Radio box item
class wxBitmap ;
class wxbRadioBox: public wxItem
{
 public:
  int no_items;
  int selected;

  wxbRadioBox(wxPanel *panel, wxFunction func, char *Title,
             int x = -1, int y = -1, int width = -1, int height = -1,
             int N = 0, char **Choices = NULL,
             int majorDim = 0,
             long style = wxHORIZONTAL, char *name = "radioBox");

  wxbRadioBox(wxPanel *panel, wxFunction func, char *Title,
             int x, int y, int width, int height,
             int N, wxBitmap **Choices,
             int majorDim = 0,
             long style = wxHORIZONTAL, char *name = "radioBox");

  ~wxbRadioBox(void);

  virtual int FindString(char *s) = 0;
  virtual void SetSelection(int N) = 0;
  virtual int GetSelection(void) = 0;
  virtual char *GetStringSelection(void);
  virtual Bool SetStringSelection(char *s);
  virtual int Number(void);
  virtual char *GetString(int N) = 0;
  // Avoids compiler warning
  inline void Enable(Bool enable) { wxWindow::Enable(enable) ; }
  virtual void Enable(int item, Bool enable) = 0; // Enable/disable specific item
  
  // Avoids compiler warning
  inline Bool Show(Bool show) { return wxItem::Show(show) ; }
  virtual void Show(int item, Bool show) = 0; // show/unshow specific item
};

#endif // wxb_rboxh
