/*
 * File:	wx_buttn.h
 * Purpose:	Button panel item
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_buttnh
#define wx_buttnh

#include "wb_buttn.h"

// Pushbutton
class wxBitmap;
class wxButton: public wxbButton
{
 public:
  wxButton(wxPanel *panel, wxFunction func, char *label, int x = -1, int y = -1,
           int width = -1, int height = -1, long style = 0, wxFont *_font = NULL, char *name = "button");
  wxButton(wxPanel *panel, wxFunction func, wxBitmap *bitmap, int x = -1, int y = -1,
           int width = -1, int height = -1, long style = 0, wxFont *_font = NULL, char *name = "button");
  ~wxButton(void);

  Bool Create(wxPanel *panel, wxFunction func, char *label, wxBitmap *bm, int x=-1, int y=-1,
           int width=-1, int height=-1, long style=0, char *name="button");
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void SetDefault(void);
  void SetLabel(char *);
  void SetLabel(wxBitmap *bitmap);
  char *GetLabel(void);

  BOOL MSWCommand(UINT param, WORD id);

  void SetBorder(Bool on);

  wxBitmap *bm_label;
};

#endif // wx_buttnh
