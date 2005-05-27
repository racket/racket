///////////////////////////////////////////////////////////////////////////////
// File:	wxMacDC.cc
// Purpose:	MacDC (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////


#include "wx_obj.h"
#include "wxMacDC.h"

//-----------------------------------------------------------------------------
wxMacDC::wxMacDC(CGrafPtr port)
{
  cMacGrafPort = port;
  cCurrentUser = NULL;
  WXGC_IGNORE(this, cCurrentUser);
}

//-----------------------------------------------------------------------------
wxMacDC::~wxMacDC(void)	// destructor
{
}

//-----------------------------------------------------------------------------
Bool wxMacDC::isCurrentPort(void)
{
  return cMacGrafPort == GetQDGlobalsThePort();
}

//-----------------------------------------------------------------------------
CGrafPtr wxMacDC::macGrafPort(void)
{
  return cMacGrafPort;
}

//-----------------------------------------------------------------------------
wxObject* wxMacDC::currentUser(void)
{
  return cCurrentUser;
}

//-----------------------------------------------------------------------------
void wxMacDC::setCurrentUser(wxObject* user)
{
  if (cCurrentUser != user) {
    EndCG();
    cCurrentUser = user;
  }
}

CGContextRef wxMacDC::GetCG(Bool only_if_already)
{
  CGContextRef cgctx;

  if (cgcref)
    return cgcref;
  else if (only_if_already)
    return 0;

  QDBeginCGContext(cMacGrafPort, &cgctx);
  cgcref = cgctx;
  
  return cgcref;
}

void wxMacDC::EndCG()
{
  if (cgcref) {
    CGContextRef cgctx = cgcref;
    CGContextSynchronize(cgctx);
    QDEndCGContext(cMacGrafPort, &cgctx);
    cgcref = NULL;
  }
}
