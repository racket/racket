///////////////////////////////////////////////////////////////////////////////
// File:	wx_utils.cc
// Purpose:	Various utilities (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "common.h"
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include <stdarg.h>
#include "wx_dialg.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"

char *wxP2C(const unsigned char *p)
{
  char *c;
  c = new WXGC_ATOMIC char[p[0] + 1];
  memcpy(c, p + 1, p[0]);
  c[p[0]]= 0;
  return c;
}

unsigned char *wxC2P(const char *c)
{
  int len;
  char *p;

  len = strlen(c);
  if (len > 255)
    len = 255;
  p = new WXGC_ATOMIC char[len + 1];
  memcpy(p + 1, c, len);
  p[0] = len;
  return (unsigned char *)p;
}

//-----------------------------------------------------------------------------
void wxError(const char *msg, const char *title)
{	
  wxMessageBox((char *)msg, (char *)title,wxOK);		
}

//-----------------------------------------------------------------------------
void wxFatalError(const char* msg, const char* title)
{
  ParamText(wxC2P(title),
	    wxC2P(msg),
	    "\p",
	    "\p");	
  StopAlert(100, NULL); 		// WCH: must redo this
#ifdef OS_X
  abort();
#else
  ExitToShell();
#endif
}

//-----------------------------------------------------------------------------
void wxFlushResources(void)
{
  // Defined in wx_utils.cc
}

//-----------------------------------------------------------------------------
void wxFlushEvents(void)
{
  // Defined in wx_utils.cc
}

//-----------------------------------------------------------------------------
char* macCopyString(char* s)
{ // return a copy of the string or NULL
  if (s) return copystring(s); else return NULL;
}

//-----------------------------------------------------------------------------
char* macCopyString0(char* s)
{ // return at least an empty string
  if (s) return copystring(s); else return copystring("");
}

//-----------------------------------------------------------------------------
char* macCopyString1(char* s)
{
  // return at least a blank string
  if (s) {
    if (strlen(s) > 0)
      return copystring(s);
    else
      return copystring(" ");
  } else
    return copystring(" ");
}

static Pattern bhatch, xhatch;

//-----------------------------------------------------------------------------
void macGetHatchPattern(int hatchStyle, Pattern *pattern)
{
  int thePatListID = sysPatListID;
  int theIndex;
  switch(hatchStyle)
    {
    case wxBDIAGONAL_HATCH:
      if (!bhatch.pat[0]) {
	int i;
	GetIndPattern(&bhatch, thePatListID, 28);
	for (i = 0; i < 8; i++) {
	  bhatch.pat[i] = (((bhatch.pat[i] & 0x80) >> 7)
			   | ((bhatch.pat[i] & 0x40) >> 5)
			   | ((bhatch.pat[i] & 0x20) >> 3)
			   | ((bhatch.pat[i] & 0x10) >> 1)
			   | ((bhatch.pat[i] & 0x08) << 1)
			   | ((bhatch.pat[i] & 0x04) << 3)
			   | ((bhatch.pat[i] & 0x02) << 5)
			   | ((bhatch.pat[i] & 0x01) << 7));
	}
      }
      if (pattern) memcpy(pattern, &bhatch, sizeof(Pattern));
      return;
    case wxFDIAGONAL_HATCH:
      theIndex = 28;
      break;
    case wxCROSS_HATCH:
      theIndex = 30;
      break;
    case wxHORIZONTAL_HATCH:
      theIndex = 27;
      break;
    case wxVERTICAL_HATCH:
      theIndex = 8;
      break;
    case wxCROSSDIAG_HATCH:
      if (!xhatch.pat[0]) {
	int i;
	macGetHatchPattern(wxBDIAGONAL_HATCH, NULL);
	macGetHatchPattern(wxFDIAGONAL_HATCH, &xhatch);
	for (i = 0; i < 8; i++) {
	  xhatch.pat[i] |= bhatch.pat[i];
	}
      }
      memcpy(pattern, &xhatch, sizeof(Pattern));
      return;
    default:
      theIndex = 1; // solid pattern
      break;
    }
  GetIndPattern(pattern, thePatListID, theIndex);	
}
