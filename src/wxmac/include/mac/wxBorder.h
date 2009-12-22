///////////////////////////////////////////////////////////////////////////////
// File:	wxBorder.h
// Purpose:	Declares wxBorder item (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wxBorderh
#define wxBorderh

#include "wx_win.h"

#ifdef IN_CPROTO
typedef       void* wxBorder ;
#else

// Border item
class wxBorder: public wxWindow
{
//=============================================================================
// Public constructors
//=============================================================================
public:

	wxBorder // Constructor (given parentArea)
	(
		wxArea*		parentArea,
		char*		windowName = "Border",
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		long		style = 0,
		WXTYPE		objectType = wxTYPE_BORDER
	);

//=============================================================================
// Public destructor
//=============================================================================
public:

	~wxBorder(void);

//=============================================================================
// Public methods
//=============================================================================
public:

	virtual void DoShow(Bool show);
	virtual void ShowAsActive(Bool flag); // mac platform only
};

#endif // IN_CPROTO
#endif // wxBorderh
