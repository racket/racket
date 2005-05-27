/*
 * File:	wx_timer.h
 * Purpose:	wxTimer - provides simple timer functionality (dummy version)
 * Author:	Julian Smart/Cecil Coupe
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_timer.h	1.2 5/9/94" */

#ifndef wx_timerh
#define wx_timerh

#include "common.h"
#include "wb_timer.h"
#ifdef IN_CPROTO
typedef       void    *wxTimer ;
#else

class wxTimer: public wxbTimer
{
 public:
#if 0
 UInt32		fireTime;		// the future Tick when the timer fires
#else
 void *context;
 double expiration;
 int one_shot;
 int interval;
 wxTimer *prev, *next;
#endif
  wxTimer(void *ctx = NULL);
  ~wxTimer(void);
  Bool Start(int milliseconds = -1,Bool one_shot = FALSE ); // Start timer
  void Stop(void);                   // Stop timer
  void Dequeue();
 void SetContext(void *ctx);
};

#endif // IN_CPROTO
#endif // wx_timerh
