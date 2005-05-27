/*
 * File:	wb_slidr.h
 * Purpose:	Slider
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wxb_slidrh
#define wxb_slidrh

#include "common.h"
#include "wx_panel.h"
#include "wx_item.h"

// Slider
class wxbSlider: public wxItem
{
 public:
  wxbSlider(wxPanel *panel, wxFunction func, char *label, int value,
           int min_value, int max_value, int width, int x = -1, int y = -1,
           long style = wxHORIZONTAL, char *name = "slider");
  ~wxbSlider(void);

  virtual int GetValue(void) = 0;
  virtual void SetValue(int) = 0;
};

#endif // wxb_slidrh
