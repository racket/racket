/*
 * File:	wx_mnuit.h
 * Purpose:	Declares menu item class (Windows)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_mnuith
#define wx_mnuith

#include "common.h"
#include "wx_obj.h"
#include "wb_mnuit.h"

class wxMenuItem: public wbMenuItem
{
 public:
  Bool checkable;
  WORD menuId;
  wxMenuItem(void);
  ~wxMenuItem(void);
};

#endif // wx_mnuith

