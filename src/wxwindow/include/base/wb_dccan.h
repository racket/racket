/*
 * File:	wb_dccan.h
 * Purpose:	Base canvas device context declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wxb_dccanh
#define wxb_dccanh

#include "common.h"
#include "wx_dc.h"

class wxbCanvasDC: public wxDC
{
 public:
  wxbCanvasDC(void);
  ~wxbCanvasDC(void);
};

#endif // wxb_dccanh
