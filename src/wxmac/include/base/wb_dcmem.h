/*
 * File:	wb_dcmem.h
 * Purpose:	Base memory device context declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wxb_dcmemh
#define wxb_dcmemh

#include "common.h"
#include "wx_dc.h"
#include "wx_dccan.h"

#ifdef IN_CPROTO
typedef       void    *wxbMemoryDC ;
#else

class wxbMemoryDC: public wxCanvasDC
{
 public:
  wxbMemoryDC(void);

  inline ~wxbMemoryDC(void) { }
  virtual void SelectObject(wxBitmap *bitmap) = 0;
};

#endif // IN_CPROTO
#endif // wx_dcmemh

