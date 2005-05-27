/*
 * File:	wb_timer.h
 * Purpose:	wxTimer - provides simple timer functionality
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wxb_timerh
#define wxb_timerh

#include "common.h"
#include "wx_obj.h"

#ifdef IN_CPROTO
typedef       void    *wxbTimer ;
#else

class wxbTimer: public wxObject
{
 public:
  int interval;

  wxbTimer(void);
  ~wxbTimer(void);
  virtual Bool Start(int milliseconds = -1,Bool one_shot=FALSE) = 0; // Start timer
  virtual void Stop(void) = 0;                   // Stop timer
  virtual void Notify(void);                 // Override this member
  virtual int Interval(void) ; // Returns the current interval time (0 if stop)
};

// Timer functions (milliseconds)
void wxStartTimer(void);
// Gets time since last wxStartTimer or wxGetElapsedTime
long wxGetElapsedTime(Bool resetTimer = TRUE);

#endif // IN_CPROTO
#endif // wxb_timerh
