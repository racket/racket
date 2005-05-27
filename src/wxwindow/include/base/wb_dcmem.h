/*
 * File:	wb_dcmem.h
 * Purpose:	Base memory device context declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wxb_dcmemh
#define wxb_dcmemh

#include "common.h"
#include "wx_dc.h"
#include "wx_dccan.h"

class wxbMemoryDC: public wxCanvasDC
{
 public:
  wxbMemoryDC(void);
  wxbMemoryDC(wxCanvasDC *WXUNUSED(old_dc));

  ~wxbMemoryDC(void);
  virtual void SelectObject(wxBitmap *bitmap) = 0;
  virtual wxBitmap* GetObject() = 0;
};

#endif // wx_dcmemh
