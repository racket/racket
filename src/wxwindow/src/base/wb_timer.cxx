/*
 * File:	wb_timer.cc
 * Purpose:	wxTimer implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include "wx_timer.h"

wxbTimer::wxbTimer(void) : wxObject(WXGC_NO_CLEANUP)
{
  __type = wxTYPE_TIMER;
}

wxbTimer::~wxbTimer(void)
{
}

// Override me!
void wxbTimer::Notify(void)
{
}

int wxbTimer::Interval(void)
{
  return interval;
}
