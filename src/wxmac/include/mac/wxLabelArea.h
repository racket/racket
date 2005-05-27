///////////////////////////////////////////////////////////////////////////////
// File:	wxLabelArea.h
// Purpose:	wxLabelArea (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wxLabelAreah
#define wxLabelAreah

#include "wx_area.h"
#include "wxDirection.h"

class wxMessage;
class wxFont;

class wxLabelArea: public wxArea
{
  public:
	wxMessage*	cLabelText; 
	Direction	cDirection;

	wxLabelArea // constructor
	(
		wxWindow*	parentWindow,
		char*		label,
		wxFont*		theFont,
		Direction	direction,
		int			xoffset = 0,
		int			yoffset = 0
	);

	~wxLabelArea(void);		// destructor

	void SetLabel(char* label);
	char* GetLabel(void);

	inline wxMessage *GetMessage(void) { return cLabelText; }

    void DoShow(Bool on);

  friend class wxMessage;
};

#endif // wxLabelAreah
