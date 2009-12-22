/*
 * File:	wb_gauge.h
 * Purpose:	Gauge box (experimental)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wb_gaugeh
#define wb_gaugeh

#include "common.h"
#include "wx_panel.h"
#include "wx_item.h"

#ifdef IN_CPROTO
typedef       void    *wxbGauge;
#else

// Group box
class wxbGauge: public wxItem
{
 public:
  wxbGauge(wxPanel *panel, char *label, int range, int x = -1, int y = -1,
           int width = -1, int height = -1, long style = 0, char *name = "gauge");
  ~wxbGauge(void);

  virtual void SetShadowWidth(int w) = 0;
  virtual void SetBezelFace(int w) = 0;
  virtual void SetRange(int r) = 0;
  virtual void SetValue(int pos) = 0;
};

#endif // IN_CPROTO
#endif // wb_gaugeh
