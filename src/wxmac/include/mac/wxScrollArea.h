///////////////////////////////////////////////////////////////////////////////
// File:	wxScrollArea.h
// Purpose:	wxScrollArea (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wxScrollAreah
#define wxScrollAreah

#include "wx_area.h"

class wxScrollBar;
class wxWindow;

class wxScrollArea: public wxArea
{
  protected:
	wxScrollBar*	cVScrollBar; /*NEW*/
	wxScrollBar*	cHScrollBar; /*NEW*/
	long			cStyle;

  public:
	wxScrollArea // constructor
	(
		wxWindow*	parentWindow,
		wxWindow*	parentScrollWindow,
		long		style
	);

	~wxScrollArea(void);		// destructor

	void ShowScrolls(Bool h, Bool v);
	void SetResizeCorner(Bool on, Bool h, Bool v);

	void MaybeMoveScrollControls();

 private:
	void ResetHPos(Bool leave_space);
	void ResetVPos(Bool leave_space);

};

#endif // wxScrollAreah
