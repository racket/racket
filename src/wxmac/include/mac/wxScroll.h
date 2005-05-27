///////////////////////////////////////////////////////////////////////////////
// File:	wxScroll.h
// Purpose:	wxScroll (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wxScrollh
#define wxScrollh

#include "wx_stdev.h"
#include "wx_obj.h"
#include "wx_list.h"
#include "wxScrollData.h"

class wxWindow;

class wxScroll: public wxObject
{
  public:

  protected:
	wxWindow*		cScrollWindow;	// never NULL
	wxScroll*		cParentScroll;	// may be NULL
	wxList*			cScrolls;		// list of child wxScroll* objects
	wxScrollData*	cScrollData;	// root scroll ? !NULL : NULL

//=============================================================================
// Public constructors
//=============================================================================
public:

	wxScroll
	(
		wxWindow*		scrollWindow,
		wxScrollData*	scrollData
	);

	wxScroll
	(
		wxWindow*		scrollWindow,
		wxWindow*		parentScrollWindow
	);

//=============================================================================
// Public destructor
//=============================================================================
public:

  	~wxScroll(void);

//=============================================================================
// Public methods
//=============================================================================
public:

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Getter and setter methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	wxScrollData* GetScrollData (void);

	void SetScrollData
	(
		wxScrollData*		newScrollData,
		wxWhatScrollData	whatScrollData, // items to be changed
		wxScrollEvent*		e
	);

	void SetScrollData
	(
		int 				value,			// value for items to be changed
		wxWhatScrollData	whatScrollData, // items to be changed
		wxScrollEvent*			e
	);

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Scroll tree methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	wxScroll* RootScroll(void);

	void AddChildScrollWindow(wxWindow* childScrollWindow);

	void OnDeleteChildScroll(wxScroll* childScroll);

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Miscellaneous methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	void OnSetScrollData
	(
		wxScrollData*		scrollData,
		wxWhatScrollData	whatScrollData, // items to be changed
		wxScrollEvent*		e
	);

};

#endif // wxScrollh
