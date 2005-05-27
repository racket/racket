///////////////////////////////////////////////////////////////////////////////
// File:	wxButtonBorder.h
// Purpose:	Declares wxButtonBorder item (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wxButtonBorderh
#define wxButtonBorderh

#include "wxBorder.h"

#ifdef IN_CPROTO
typedef       void* wxButtonBorder ;
#else

// ButtonBorder item
class wxButtonBorder: public wxBorder
{
//=============================================================================
// Public constructors
//=============================================================================
public:

	wxButtonBorder // Constructor (given parentArea)
	(
		wxArea*		parentArea,
		int			margin = 0,
		Direction	direction = wxAll,
		char*		windowName = "ButtonBorder",
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

	~wxButtonBorder(void);

//=============================================================================
// Public methods
//=============================================================================
public:
	virtual void Paint(void);
};

#endif // IN_CPROTO
#endif // wxButtonBorderh
