///////////////////////////////////////////////////////////////////////////////
// File:	wxBorderArea.h
// Purpose:	wxBorderArea (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wxBorderAreah
#define wxBorderAreah

#include "wx_area.h"
#include "wxDirection.h"

class wxRectBorder;

class wxBorderArea: public wxArea
{
  public:
	wxBorderArea(wxWindow* parentWindow, int margin = 1,
		     Direction direction = wxAll,
		     int whitespace = 0,
		     int te_border = 0);
	~wxBorderArea(void); 

	wxRectBorder* cBorder;
};

#endif // wxBorderAreah
