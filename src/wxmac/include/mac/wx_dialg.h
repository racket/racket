///////////////////////////////////////////////////////////////////////////////
// File:	wx_dlog.h
// Purpose:	wxDialogBox (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_dlogh
#define wx_dlogh
#include "wx_frame.h"
#include "wb_dialg.h"

#ifdef IN_CPROTO
typedef void* wxDialogBox;
#else

class wxPanel;
class wxButton;

class wxDialogBox: public wxPanel
{
 public:
	int cCloseRequested;

//=============================================================================
// Public constructors
//=============================================================================
public:
	wxFrame *cFrame;		//mflatt, cjc

	wxDialogBox // Constructor (for dialog window)
	(
		wxWindow*	parentFrame,		// this is ignored, used to be wxFrame*
		char*		windowTitle,
		Bool		modal = FALSE,
		int 		x = wxDEFAULT_POSITION,
		int			y = wxDEFAULT_POSITION,
		int			width = -1,
		int			height = -1,
		long		style = 0,
		char*		windowName = "Dialog",
		WXTYPE		objectType = wxTYPE_DIALOG_BOX
	);

//=============================================================================
// Public destructor
//=============================================================================
public:

	~wxDialogBox();

//=============================================================================
// Public methods
//=============================================================================
public:

	virtual Bool IsShown(void);

	void Show(Bool show);
	Bool IsModal(void);
	void ShowModal(void);
	Bool OnClose(void);

	virtual void Fit(void);
	virtual void Centre(int d) { cFrame->Centre(d); }

	void SetIcon(wxBitmap* icon) { cFrame->SetIcon(icon); }
	void Iconize(Bool iconize) { cFrame->Iconize(iconize); }
	Bool Iconized(void)	   { return cFrame->Iconized(); }
	
    char* GetTitle(void) { return cFrame->GetTitle(); }
	void SetTitle(char* title) { cFrame->SetTitle(title); }
	wxWindow* GetFocusWindow(void) { return cFrame->GetFocusWindow(); }
	void SetFocusWindow(wxWindow* window) { cFrame->SetFocusWindow(window); }
	void LoadAccelerators(char* table) { cFrame->LoadAccelerators(table); }
	virtual void SetSize(int x, int y, int width, int height, int flags = wxSIZE_AUTO);
	virtual void OnSize(int w, int h);

	void EnforceSize(int minw, int minh, int maxw, int maxh, int incw=1, int inch=1);
	
};

int wxMessageBox(char *message, char *caption = "Message", long style = wxOK|wxCENTRE,
		 wxWindow* parent = NULL, int x = -1, int y = -1);

#endif // IN_CPROTO
#endif // wx_dialg.h
