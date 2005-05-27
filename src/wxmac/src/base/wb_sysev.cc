/*
 * File:	wb_sysev.cc
 * Purpose:	System event implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#pragma implementation "wx_sysev.h"
#endif

#include "common.h"
#include "wx_utils.h"
#include "wx_list.h"

#include "wx.h"

#include "wx_sysev.h"
 
wxEvent::wxEvent(void)  : wxObject(WXGC_NO_CLEANUP)
{
  eventClass = 0;
  eventType = 0;
  eventHandle = NULL;
}

wxEvent::~wxEvent(void)
{
}

