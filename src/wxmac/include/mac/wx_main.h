///////////////////////////////////////////////////////////////////////////////
// File:	wx_main.h
// Purpose:	wxApp declaration and a few other functions (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_mainh
#define wx_mainh

#include "wb_main.h"
#ifndef WX_CARBON
# include <Events.h>
#endif
#ifdef IN_CPROTO
typedef       void* wxApp;
#else

extern int wxEntry(int argc, char *argv[]);

class wxFrame;

// Represents the application. Derive OnInit and declare
// a new App object to start application
class wxApp: public wxbApp
{
public:
	int				keep_going;
 protected:
 	EventRecord		cCurrentEvent; // mac platform only
 	RgnHandle		cMacCursorRgn; // mac platform only
	Point			cLastMousePos; // mac platform only

 public:
	wxApp();
	~wxApp(void);
#if USE_MAC_DIALOG_PANEL
	virtual int MainLoop(void);
	virtual void ExitMainLoop(void);
	virtual Bool Initialized(void);
	virtual Bool Pending(void);
	virtual void Dispatch(void);
#else
	virtual int MainLoop(void);
	virtual void ExitMainLoop(void);
	virtual Bool Initialized(void);
	virtual Bool Pending(void);
	virtual void Dispatch(void);
	virtual void DoOneEvent(void);		// used primarily from MainLoop
#endif
	virtual void AdjustCursor(void);	//GRW

	virtual char *GetDefaultAboutItemName(void); // mflatt
	virtual void DoDefaultAboutItem(void); // mflatt

	void doMacPreEvent();
	void doMacDispatch(EventRecord*);
	void doMacPostEvent();
	
	Bool MacOS85WindowManagerPresent;

 protected:
  	virtual void DoIdle(void);

	void doMacMouseDown();
	void doMacMouseUp();
	void doMacMouseMotion();
	void doMacMouseLeave();
	void doMacKeyDown();
	void doMacAutoKey();
	void doMacKeyUp();
	void doMacKeyUpDown(Bool down); // abstraction for KeyUp, KeyDown, AutoKey
	void doMacActivateEvt();
	void doMacUpdateEvt();
	void doMacDiskEvt();
	void doMacOsEvt();
	void doMacHighLevelEvent();

	void doMacResumeEvent(void);
	void doMacSuspendEvent(void);
	void doMacMouseMovedMessage(void);

	Bool doMacInMenuBar(long menuResult, Bool externOnly = FALSE);
	void doMacInContent(WindowPtr window);
	void doMacContentClick(wxFrame* frame);
	void doMacInDrag(WindowPtr window);
	void doMacInGrow(WindowPtr window);
	void doMacInGoAway(WindowPtr window);
	void doMacInZoom(WindowPtr window, short windowPart);
	
	wxFrame* findMacWxFrame(WindowPtr theMacWindow);
};

extern void wxPrimDialogSetUp();
extern void wxPrimDialogCleanUp();

#endif // IN_CPROTO
#endif // wx_mainh
