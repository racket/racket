///////////////////////////////////////////////////////////////////////////////
// File:	wx_panel.h
// Purpose:	wxPanel subwindow, for panel items (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_panelh
#define wx_panelh

#include "wb_panel.h"

#define PANEL_HSPACING  10
#define PANEL_VSPACING  8
#define PANEL_LEFT_MARGIN 4
#define PANEL_TOP_MARGIN  4

#ifdef IN_CPROTO
typedef       void* wxPanel;
#else

class wxItem;
class wxFrame;

class wxPanel: public wxbPanel
{
//=============================================================================
// Protected variables
//=============================================================================
protected:

	int			cursor_x;
	int			cursor_y;
	int			max_width;
	int			max_height;
	int			max_line_height;
	int 		currentRow;
	int 		currentCol;
	wxWindow*	last_created;

    wxArea*     cPanelBorder;

friend class wxWindow;	
//=============================================================================
// Public methods
//=============================================================================
public:

        ControlHandle paneControl;

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Constructors
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	wxPanel // Constructor (given parentArea)
	(
		wxArea*		parentArea,
		int 		x = -1,
		int		y = -1,
		int		width = -1,
		int		height = -1,
		long		style = 0,
		char*		windowName = "panel",
		WXTYPE		objectType = wxTYPE_PANEL
	);

#if 0
	// EMBEDDING
	wxPanel // Constructor (given parentArea and embedding control)
        // see implementation for explanation of this ugly hack.
	(
		wxArea*		parentArea,
                ControlHandle	parentEmbeddingPanel,
		int 		x = -1,
		int		y = -1,
		int		width = -1,
		int		height = -1,
		long		style = 0,
		char*		windowName = "panel",
		WXTYPE		objectType = wxTYPE_PANEL
	);
#endif

	wxPanel // Constructor (given parentFrame)
	(
		wxFrame*	parentFrame,
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		long		style = 0,
		char*		windowName = "panel",
		WXTYPE		objectType = wxTYPE_PANEL
	);

	wxPanel // Constructor (given parentPanel)
	(
		wxPanel*	parentPanel,
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		long		style = 0,
		char*		windowName = "panel",
		WXTYPE		objectType = wxTYPE_PANEL
	);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Destructor
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	~wxPanel(void);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Sizing methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	void DoSetSize(int x, int y, int width, int height);
        void Centre(int direction = wxBOTH, wxWindow *p = NULL);
	void Fit(void);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Item placement methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	void NewLine(void);
	void NewLine(int pixels);
	void Tab(void);
	void Tab(int pixels); // Tab specified number of pixels
	void GetCursor(int* x, int* y);
	void SetItemCursor(int x, int y);
	void SetHorizontalSpacing(int sp);
	int GetHorizontalSpacing(void);
	void SetVerticalSpacing(int sp);
	int GetVerticalSpacing(void);
	void AdvanceCursor(wxWindow* item); // Update next cursor position
	Bool WantsFocus();

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Other methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	void SetBackgroundColour(wxColour*col);
	void DoShow(Bool show);
	void Paint(void);
	virtual void OnChar(wxKeyEvent *event);
	virtual void OnEventCheckMetal(wxMouseEvent *event, int metal_drag_ok);
	virtual void AdjustMetalDragOk(int *metal_drag_ok);

 	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 	// Tree methods
 	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 	void DestroyChildren() ; // tom
 	void OnDeleteChild(wxWindow* win); //tom

	virtual void SetSize(int x, int y, int width, int height, int flags = wxSIZE_AUTO);

	virtual void MaybeMoveControls();

    virtual ControlHandle GetRootControl(void);

	void OnClientAreaDSize(int dW, int dH, int dX, int dY);
	
//=============================================================================
// Protected methods
//=============================================================================
protected:


	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Tree methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	void AddChild(wxObject* child);
	//virtual void OnDeleteChildWindow(wxWindow* childWindow); 

//=============================================================================
// Private methods
//=============================================================================
private:

	void CreateWxPanel(int x, int y, int w, int h); // common constructor initialization
	void InitDefaults(void); // used by constructors
};

#endif // IN_CPROTO
#endif // wx_panelh
