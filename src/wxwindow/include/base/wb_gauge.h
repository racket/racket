/*
 * File:	wb_gauge.h
 * Purpose:	Gauge box (experimental)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wb_gaugeh
#define wb_gaugeh

#include "common.h"
#include "wx_panel.h"
#include "wx_item.h"

// Group box
class wxbGauge: public wxItem
{
 public:
  wxbGauge(wxPanel *panel, char *label, int range, int x = -1, int y = -1,
           int width = -1, int height = -1, long style = 0, char *name = "gauge");
  ~wxbGauge(void);

  virtual void SetRange(int r) = 0;
  virtual void SetValue(int pos) = 0;
};

#endif // wb_gaugeh
