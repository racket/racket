///////////////////////////////////////////////////////////////////////////////
// File:	wx_menu.h
// Purpose:	Declares menus and menu bars (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_menuh
#define wx_menuh

#include "wb_menu.h"

#ifdef IN_CPROTO
typedef       void* wxMenu ;
typedef       void* wxMenuBar ;
#else

class wxMenuBar;

//-----------------------------------------------------------------------------
// Menu
//-----------------------------------------------------------------------------
class wxMenu: public wxbMenu
{
//=============================================================================
// Protected variables
//=============================================================================
friend class wxChoice;
protected:

	MenuHandle		cMacMenu;		// mac platform
	short			cMacMenuId;		// mac platform
	static short	gMenuIdCounter; // mac platform (to give unique menuID's to mac menus)
	int requestedWidth;

//=============================================================================
// Public methods
//=============================================================================
public:

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Constructors
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	wxMenu // Constructor (given objectType)
	(
		char*		Title = NULL,
		wxFunction	func = NULL,
                wxFont           *_font = NULL,
		char*		windowName = "menu",
		WXTYPE		objectType = wxTYPE_MENU
	);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Destructor
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	~wxMenu(void);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Other methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	void AppendSeparator(void);
	void Append(int Id, char* Label, char* helpString=NULL, Bool checkable=FALSE);
	void Append(int Id, char* Label, wxMenu* SubMenu, char* helpString = NULL);
	Bool Delete(int Id); // mflatt
	Bool DeleteByPosition(int pos); // mflatt
	void Enable(int Id, Bool Flag);
	void Check(int Id, Bool Flag);
	Bool Checked(int Id);
	char* GetTitle(void);
	void SetTitle(char* label);

        int Number();

	char* GetLabel(void);			// override virtual from superclass
	char* GetLabel(int id);			// add new method for this class

	void SetLabel(char* label);
	void SetLabel(int id, char* label);
	void Break(void) ;

	virtual void SetSize(int x, int y, int width, int height);
	virtual void DoSetSize(int x, int y, int width, int height);
	virtual void Enable(Bool enable);
	virtual void Show(Bool show);

    MenuHandle CreateCopy(char *title, Bool doabouthack, MenuHandle toHandle = NULL);
    void CheckHelpHack(void);

    void SetWidth(int req);

//=============================================================================
// Protected methods
//=============================================================================
protected:

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Other methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	MenuHandle MacMenu(void);
	short GetMacMenuId(void);
	virtual wxMenu* wxMacFindSubmenu(int macMenuId);
	virtual void wxMacInsertSubmenu(void); // used by wxMenuBar::Install
	void MacChangeMenuText(wxMenu *menu, char *title);

//=============================================================================
// Friend classes
//=============================================================================
private:

	Bool Delete(wxMenu* menu, int Id, int pos); // mflatt: for removing deleted sunmenus

	friend class wxMenuBar;
	friend class wxMenuItem;
	friend class wxFrame;
    friend class wxWindow;
};

//-----------------------------------------------------------------------------
// Menu Bar
//-----------------------------------------------------------------------------

class wxMenuBar:public wxbMenuBar
{
//=============================================================================
// Public methods
//=============================================================================
public:
	wxMenu*	wxHelpHackMenu;
	short 	iHelpMenuHackNum;

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Constructors
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	wxMenuBar // Constructor (given objectType)
	(
		char*		windowName = "menubar",
		WXTYPE		objectType = wxTYPE_MENU_BAR
	);

	wxMenuBar // Constructor (given Menus)
	(
		int			N,
		wxMenu*		Menus[],
		char*		Titles[],
		char*		windowName = "menubar",
		WXTYPE		objectType = wxTYPE_MENU_BAR
	);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Destructor
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	~wxMenuBar(void);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Other methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	void Enable(int id, Bool flag);
	void EnableTop(int pos, Bool flag);
	void Check(int id, Bool flag);
	Bool Checked(int Id);
	char* GetLabel(void);
	void SetLabel(char* label);
	void SetLabel(int id, char* label) ;
	char* GetLabel(int id) ;
	void SetLabelTop(int pos,char* label) ;
	char* GetLabelTop(int pos) ;

	virtual void SetSize(int x, int y, int width, int height);
	virtual void DoSetSize(int x, int y, int width, int height);
	virtual void Enable(Bool enable);
	virtual void Show(Bool show);

	virtual Bool OnAppend(wxMenu * menu, char *title); // mflatt
	virtual Bool OnDelete(wxMenu *menu, int index); // mflatt

//=============================================================================
// Protected methods
//=============================================================================
protected:

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Other methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	wxMenu* wxMacFindMenu(int macMenuId);

	void Install(wxWindow *for_frame);

//=============================================================================
// Friend classes
//=============================================================================
private:

	friend class wxApp;
	friend class wxMenu;
	friend class wxFrame;
};

extern void wxPrepareMenuDraw(void);
extern void wxDoneMenuDraw(Bool menu_hilited = FALSE);

#endif // IN_CPROTO
#endif // wx_menuh
