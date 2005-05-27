///////////////////////////////////////////////////////////////////////////////
// File:	wx_screen.cc
// Purpose:	Macintosh wxScreen implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wx_screen.h"
#include "wx_area.h"
#include "wx_utils.h"
#ifndef WX_CARBON
# include <QuickDraw.h>
#endif

/* The gScreenWindow declaration confises xform.ss. */
#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

wxScreen* wxScreen::gScreenWindow = NULL; // mac platform only

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

//=============================================================================
// Construction methods
//=============================================================================

//-----------------------------------------------------------------------------
wxScreen::wxScreen // Constructor (for screen window)
	(
		char*		windowName,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style,
		WXTYPE		objectType
	) :
		wxWindow (windowName, x, y, width, height, style)
{
	CGrafPtr wPort;
        Rect wPortRect;

        wPort = GetQDGlobalsThePort();
        GetPortBounds(wPort,&wPortRect);
	if (x == -1) cWindowX = 0;
	if (y == -1) cWindowY = 0;
	if (width < 0) cWindowWidth = wPortRect.right - wPortRect.left;
	if (height < 0) cWindowHeight = wPortRect.bottom - wPortRect.top;

  	cMenuArea = new wxArea(this);
  	cScreenArea = new wxArea(this);
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxScreen::~wxScreen(void)
{
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Sizing methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxScreen::DoSetSize(int x, int y, int width, int height)
{
	cWindowX = x;
	cWindowY = y;
	cWindowWidth = width;
	cWindowHeight = height;
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxArea* wxScreen::ScreenArea(void) { return cScreenArea; }

//-----------------------------------------------------------------------------
wxArea* wxScreen::MenuArea(void){ return cMenuArea; } // mac platform only

//-----------------------------------------------------------------------------
void wxScreen::Show(Bool show) { }
