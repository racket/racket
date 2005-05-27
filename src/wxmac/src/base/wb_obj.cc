/*
 * File:	wb_obj.cc
 * Purpose:	wxObject base class implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#ifdef __GNUG__
#pragma implementation "wx_obj.h"
#endif
#endif

#include "wx_obj.h"
#include "wx_types.h"

int wx_object_count;

wxObject::wxObject(void)
{
  __type = wxTYPE_ANY;
  wx_object_count++;
}

wxObject::wxObject(int cleanup)
: gc_cleanup(cleanup)
{
  __type = wxTYPE_ANY;
  wx_object_count++;
}

wxObject::wxObject(int cleanup, WXTYPE t)
: gc_cleanup(cleanup)
{
  __type = t;
  wx_object_count++;
}


wxObject::~wxObject(void)
{
  __type = -1;
  --wx_object_count;
}

