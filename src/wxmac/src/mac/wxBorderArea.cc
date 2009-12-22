///////////////////////////////////////////////////////////////////////////////
// File:	wxBorderArea.cc
// Purpose:	Label area (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "common.h"
#include "wxBorderArea.h"
#include "wxRectBorder.h"

//=============================================================================
// Construction methods
//=============================================================================

//-----------------------------------------------------------------------------
wxBorderArea::wxBorderArea(wxWindow* parentWindow, int margin, Direction direction,
			   int whitespace,
			   Bool te_border) :
			   wxArea(parentWindow)
{
  cBorder = new WXGC_PTRS wxRectBorder(this, margin, direction, whitespace, te_border);
}

//-----------------------------------------------------------------------------
wxBorderArea::~wxBorderArea(void)	// destructor
{
  // DELETE_OBJ cBorder;
}
