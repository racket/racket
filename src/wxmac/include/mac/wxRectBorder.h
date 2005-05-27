///////////////////////////////////////////////////////////////////////////////
// File:	wxRectBorder.h
// Purpose:	Declares wxRectBorder item (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wxRectBorderh
#define wxRectBorderh

#include "wxBorder.h"

#ifdef IN_CPROTO
typedef       void* wxRectBorder ;
#else

// RectBorder item
class wxRectBorder: public wxBorder
{
private:
	int cWhitespace; // white border interior width
	int cPaintFocus;
	int cTEBorder;

//=============================================================================
// Public constructors
//=============================================================================
public:

	wxRectBorder // Constructor (given parentArea)
	(
		wxArea*		parentArea,
		int			margin = 1,
		Direction	direction = wxAll,
		int			whitespace = 0,
	        int            te_border = 0,
		char*		windowName = "RectBorder",
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

	~wxRectBorder(void);
    
	void SetBrush(wxBrush *b) { cBrush = b; }

//=============================================================================
// Public methods
//=============================================================================
public:
	virtual void Paint(void);

	virtual void DoShow(Bool on);

	void PaintFocus(Bool on);
 protected:
   void ChangeToGray(Bool Gray);
};

#endif // IN_CPROTO
#endif // wxRectBorderh
