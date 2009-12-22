///////////////////////////////////////////////////////////////////////////////
// File:	wx_sbar.h
// Purpose:	Declares wxScrollBar item (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_sbarh
#define wx_sbarh

#include "wx_win.h"
#ifndef WX_CARBON
# include <Controls.h>
#endif

#ifdef IN_CPROTO
typedef       void* wxScrollBar ;
#else


// why isn't wxScrollBar like other controls in inheriting directly from 
// wxbScrollBar and indirectly from wxItem? -- JBC, 2001-10

class wxScrollBar: public wxWindow
{
 public:
	ControlHandle cMacControl; // mac control for scrollbar

//=============================================================================
// Public constructors
//=============================================================================
public:

	wxScrollBar // Constructor (given parentArea)
	(
		wxArea*		parentArea,
		wxFunction	function,
		char*		label,
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		long		style = 0,
		char*		windowName = "scrollbar",
		WXTYPE		objectType = wxTYPE_SLIDER
	);

	wxScrollBar // Constructor (given parentWindow)
	(
		wxWindow*	parentWindow,
		wxFunction	function,
		char*		label,
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		long		style = 0,
		char*		windowName = "scrollbar",
		WXTYPE		objectType = wxTYPE_SLIDER
	);

//=============================================================================
// Public destructor
//=============================================================================
public:

	~wxScrollBar(void);

//=============================================================================
// Private methods
//=============================================================================
private:

	void CreateWxScrollBar(wxFunction function, char* label); // common constructor initialization
	void InitDefaults(wxFunction function);

//=============================================================================
// Public methods
//=============================================================================
public:

	virtual void SetValue(int val);
	virtual int GetValue(void);
	virtual void SetMaxValue(int maxValue);
	virtual int GetMaxValue(void);
	void SetLabel(char*);
	char* GetLabel(void) ;

	virtual void OnClientAreaDSize(int dW, int dH, int dX, int dY); // mac platform only
	virtual void Enable(Bool enable);
	virtual void ChangeToGray(Bool enable);
	virtual void Paint(void);
	virtual void DoShow(Bool show);

	virtual void ShowAsActive(Bool flag);
	virtual void OnEvent(wxMouseEvent *event); // mac platform only
	virtual void TrackAction(short part); // mac platform only

        virtual wxWindow *EnterLeaveTarget();
	virtual wxCursor *GetEffectiveCursor();

	virtual void MaybeMoveControls();

	virtual void SetScrollData // adjust scrollBar to match scroll data setting
	(
		wxScrollData*		scrollData,
		wxWhatScrollData	whatScrollData,
		wxScrollEvent*		e
	);
};

const int kVScrollBarWidth = 16;
const int kHScrollBarHeight = 16;

#endif // IN_CPROTO
#endif // wx_sbarh
