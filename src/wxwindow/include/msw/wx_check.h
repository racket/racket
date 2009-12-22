/*
 * File:	wx_check.h
 * Purpose:	Check box
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_checkh
#define wx_checkh

#include "wb_check.h"

// Checkbox item
class wxBitmap;
class wxCheckBox: public wxbCheckBox
{
 public:
  int checkWidth;
  int checkHeight;

  wxCheckBox(wxPanel *panel, wxFunction func, char *Title,
             int x = -1, int y = -1, int width = -1, int height = -1,
             long style = 0, wxFont *_font = NULL, char *name = "checkBox");
  wxCheckBox(wxPanel *panel, wxFunction func, wxBitmap *bitmap,
             int x = -1, int y = -1, int width = -1, int height = -1,
             long style = 0, wxFont *_font = NULL, char *name = "checkBox");
  ~wxCheckBox(void);

  Bool Create(wxPanel *panel, wxFunction func, char *Title, wxBitmap *bitmap,
             int x = -1, int y = -1, int width = -1, int height = -1,
             long style = 0, char *name = "checkBox");

  void SetValue(Bool);
  Bool GetValue(void);
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  virtual BOOL MSWCommand(UINT param, WORD id);
  void SetLabel(char *);
  void SetLabel(wxBitmap *bitmap);
  char *GetLabel(void) ;

  wxBitmap *bm_label;
};

#endif // wx_checkh
