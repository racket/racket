///////////////////////////////////////////////////////////////////////////////
// File:	wx_screen.h
// Purpose:	Declares wxScreen (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_screenh
#define wx_screenh

#include "wx_win.h"

#ifdef IN_CPROTO
typedef       void* wxScreen ;
#else
class wxScreen: public wxWindow
{
 protected:

  // For window area
  	wxArea* cScreenArea; // mac platform only
  	wxArea* cMenuArea; // mac platform only

 public:
	static wxScreen* gScreenWindow;

	wxScreen // Constructor (for screen window)
	(
		char*		windowName = "Screen",
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		long		style = 0,
		WXTYPE		objectType = wxTYPE_SCREEN_WINDOW
	);

//=============================================================================
// Public destructor
//=============================================================================
public:

	~wxScreen(void);

//=============================================================================
// Window area methods
//=============================================================================
	wxArea* ScreenArea(void); // mac platform only
	wxArea* MenuArea(void); // mac platform only

//=============================================================================
// Window coordinate system transformation methods
//=============================================================================
	virtual void DoSetSize(int x, int y, int width, int height);
	virtual void Show(Bool show);
};

#endif // IN_CPROTO
#endif // wx_screenh
