/*
 * File:	wb_sysev.cc
 * Purpose:	System event implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

/*
 * A different kind of event from wxEvent: general wxWindows events, covering
 * all interesting things that might happen (button clicking, resizing,
 * setting text in widgets, etc.).
 *
 * For each completely new event type, derive a new event class.
 *
 */

wxEvent::wxEvent(void) : wxObject(WXGC_NO_CLEANUP)
{
  eventClass = 0;
  eventType = 0;
  timeStamp = 0;
}

wxEvent::~wxEvent(void)
{
}

void wxEvent::SetTimestamp(long ts)
{
  timeStamp = ts;
}
