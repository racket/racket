/*
 * File:		wb_sysev.cc
 * Purpose:	System event implementation
 * Author:		Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#pragma implementation "wx_sysev.h"
#endif

#ifndef wx_xt
    // wxWindows standard include mechanism
    /* static const char sccsid[] = "%W% %G%"; */
    // For compilers that support precompilation, includes "wx.h".
#   include "wx_prec.h"
#   ifdef __BORLANDC__
#	pragma hdrstop
#   endif
#   ifndef WX_PRECOMP
#	include "common.h"
#	include "wx_utils.h"
#	include "wx_list.h"
#   endif
#   include "wx_sysev.h"
#else // wx_xt
    // The Xt port uses another include mechanism
#   define  Uses_wxEvent
#   define  Uses_wxList
#   include "wx.h"
#endif // #ifndef wx_xt

wxEvent::wxEvent(void) : wxObject(WXGC_NO_CLEANUP)
{
  eventType = 0;
  eventHandle = NULL;
  timeStamp = 0;
}

wxEvent::~wxEvent(void)
{
}

void wxEvent::SetTimestamp(long ts)
{
  timeStamp = ts;
}

