/*
 * File:	wx_slidr.h
 * Purpose:	Slider
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_slidrh
#define wx_slidrh

#include "wb_slidr.h"

// Slider
class wxSlider: public wxbSlider
{
 public:
  HWND static_label;
  HWND static_min;
  HWND static_max;
  HWND edit_value;

  int s_min;
  int s_max;
  int page_size;

  wxSlider(wxPanel *panel, wxFunction func, char *label, int value,
           int min_value, int max_value, int width, int x = -1, int y = -1,
           long style = wxHORIZONTAL, wxFont *_font = NULL, char *name = "slider");
  ~wxSlider(void);

  Bool Create(wxPanel *panel, wxFunction func, char *label, int value,
           int min_value, int max_value, int width, int x = -1, int y = -1,
           long style = wxHORIZONTAL, char *name = "slider");
  virtual int GetValue(void);
  virtual char *GetLabel(void);
  virtual void SetValue(int);
  virtual void SetLabel(char *label);
  void GetSize(int *x, int *y);
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void GetPosition(int *x, int *y);

  virtual void ChangeToGray(Bool gray);
  
  Bool Show(Bool);
};

#endif // wx_slidrh
