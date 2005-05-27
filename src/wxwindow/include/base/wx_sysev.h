/*
 * File:	wx_sysev.h
 * Purpose:	System event base declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wxb_sysevh
#define wxb_sysevh

#include "common.h"

#include "wx_obj.h"
#include "wx_types.h"

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

class wxEvent: public wxObject
{
 public:
  WXTYPE   eventType;
  WXTYPE   eventClass;
  long timeStamp;

  wxEvent(void);
  ~wxEvent(void);

  inline WXTYPE GetEventType(void) { return eventType; }
  inline WXTYPE GetEventClass(void) { return eventClass; }
  inline long GetTimestamp(void) { return timeStamp; }
  void SetTimestamp(long ts = 0);
};

#endif // wxb_sysevh
