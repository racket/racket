/*
 * File:	wb_slidr.h
 * Purpose:	Slider
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_slidr.h	1.2 5/9/94" */

#ifndef wxb_slidrh
#define wxb_slidrh

#include "common.h"
#include "wx_panel.h"
#include "wx_item.h"

#ifdef IN_CPROTO
typedef       void    *wxbSlider ;
#else

// Slider
class wxbSlider: public wxItem
{
 public:
  wxbSlider(wxPanel *panel, wxFunction func, char *label, int value,
           int min_value, int max_value, int width, int x = -1, int y = -1,
           long style = 0, char *name = "slider");

  ~wxbSlider(void);

  virtual int GetValue(void) = 0;
  virtual void SetValue(int) = 0;
};

#endif // IN_CPROTO
#endif // wxb_slidrh
