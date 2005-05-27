///////////////////////////////////////////////////////////////////////////////
// File:	wx_area.h
// Purpose:	wxArea (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_areah
#define wx_areah

#include "wx_obj.h"
#include "wxMargin.h"
#include "wxDirection.h"
#include "wx_list.h"

class wxWindow;

// Here's my understanding of areas: a window contains a list of child areas, and
// an area contains a list of child windows.  So essentially, areas are a grouping
// mechanism for child windows.  What it's good for, I don't know.

class wxArea: public wxObject
{
//=============================================================================
// Protected variables
//=============================================================================
protected:

	wxMargin	cMargin;
	wxWindow*	cParentWindow; /*OLD*/
	wxChildList		*cWindows;

//=============================================================================
// Public methods
//=============================================================================
public:

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Constructors
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	wxArea(wxWindow* parentWindow);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Destructor
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	~wxArea(void);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Geometry methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	wxMargin Margin(void);
	wxMargin Margin(wxArea* outerArea); // Margin from this area to outer area
	wxMargin Margin(wxWindow* outerWindow); // Margin from this area to outer window
	int Width(void);
	int Height(void);
	void AreaToScreen(int* h, int* v); // Convert from this area c.s. to screen c.s.
	void ScreenToArea(int* h, int* v); // Convert from screen c.s. to this area c.s.
	Bool WindowPointInArea(int windowH, int windowV);
	void FrameContentAreaOffset(int* x, int* y); // mac platform only
        
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Sizing methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	virtual void SetSize(int width, int height);
	void SetMargin(int margin, Direction direction = wxAll);
	void SetMargin(wxMargin margin, Direction direction = wxAll);
	void SetMargin(wxMargin margin, Direction direction,
					int parentWindowWidth, int parentWindowHeight,
					int parentWindowX, int parentWindowY);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Tree (windows and areas) methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	wxWindow* ParentWindow(void);
	wxChildList* Windows(void);
	wxArea* First(void);
	wxArea* Previous(void);
	wxArea* Next(void);


	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Sizing methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	virtual void OnSiblingDSize(int dW, int dH, int dX, int dY);
	virtual void OnAreaDSize(int dW, int dH, int dX, int dY); // Notify child windows of resize

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Tree (windows and areas) methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	virtual void OnDeleteChildWindow(wxWindow* childWindow);

//=============================================================================
// Friend classes
//=============================================================================
private:

	friend class wxWindow;
	friend class wxFrame;
};

#endif // wx_areah
