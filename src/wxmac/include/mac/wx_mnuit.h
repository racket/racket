///////////////////////////////////////////////////////////////////////////////
// File:	wx_mnuit.h
// Purpose:	Declares menu item class (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_mnuith
#define wx_mnuith

#include "wb_mnuit.h"

#ifdef IN_CPROTO
typedef       void    *wxMenuItem ;
#else

class wxMenu;

class wxMenuItem: public wbMenuItem
{
//=============================================================================
// Protected variables
//=============================================================================
protected:

	Bool	checkable ;
	Bool	cIsChecked;
	Bool	cIsEnabled;

	wxMenu*	parentMenu;

//=============================================================================
// Public methods
//=============================================================================
public:

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Constructors
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	wxMenuItem(void);
	wxMenuItem
	(
		wxMenu* theParentMenu,
		Bool	isCheckable = FALSE
	);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Destructor
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	~wxMenuItem(void);
  
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// tree methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	wxMenu* ParentMenu(void);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Other methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	void Check(Bool flag);
	Bool IsChecked(void);
	Bool IsCheckable(void);
	void Enable(Bool flag);
	char* GetHelpString(void);
	void SetHelpString(char* theHelpString);
	char* GetLabel(void);
	void SetLabel(char* label);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Other methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	short GetMacItemNumber(void); // mac platform only

	inline Bool IsEnabled(void) { return cIsEnabled; }
};

#endif // IN_CPROTO
#endif // wx_mnuith

