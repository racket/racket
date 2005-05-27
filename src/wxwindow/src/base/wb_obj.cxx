/*
 * File:	wb_obj.cc
 * Purpose:	wxObject base class implementation
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
 * wxWindows root object.
 */

int wx_object_count;

wxObject::wxObject(void)
{
  __type = wxTYPE_ANY;
  wx_object_count++;
}

wxObject::wxObject(Bool cleanup)
: WXGC_CLEANUP_CLASS((int)cleanup)
{
  __type = wxTYPE_ANY;
  wx_object_count++;
}

wxObject::~wxObject(void)
{
  __type = -1;
  --wx_object_count;
}
