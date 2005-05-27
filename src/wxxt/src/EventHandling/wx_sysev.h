/*								-*- C++ -*-
 * File:		wx_sysev.h
 * Purpose:	System event base declaration
 * Author:		Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wxb_sysevh
#define wxb_sysevh

#ifdef __GNUG__
#pragma interface
#endif

#ifndef wx_xt
    // wxWindows standard include mechanism
    /* sccsid[] = "@(#)wx_sysev.h	1.2 5/9/94" */
#   include "common.h"
#   include "wx_obj.h"
#   include "wx_types.h"
#endif

/*
 * wxWindows events, covering all interesting things that might happen
 * (button clicking, resizing, setting text in widgets, etc.).
 *
 * For each completely new event type, derive a new event class.
 * An event CLASS represents a C++ class defining a range of similar event TYPES;
 * examples are canvas events, panel item command events.
 * An event TYPE is a unique identifier for a particular system event,
 * such as a button press or a listbox deselection.
 *
 */

#ifdef IN_CPROTO
typedef       void    *wxEvent ;
#else

class wxEvent: public wxObject
{
 public:
  char *eventHandle;         // Handle of an underlying windowing system event
  WXTYPE   eventType;
  long timeStamp;

  wxEvent(void);
  ~wxEvent(void);

  inline WXTYPE GetEventType(void) { return eventType; }
  inline virtual long GetTimestamp(void) { return timeStamp; }
  virtual void SetTimestamp(long ts = 0);
};

#endif // IN_CPROTO
#endif // wxb_sysevh
