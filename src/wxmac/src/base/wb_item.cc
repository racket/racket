/*
 * File:      wb_item.cc
 * Purpose:     Panel items implementation
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	August 1994
 * Copyright:   (c) 2004-2010 PLT Scheme Inc.
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#ifdef __GNUG__
#pragma implementation
#endif

#ifdef __GNUG__
#pragma implementation "wb_item.h"
#endif
#ifdef __GNUG__
#pragma implementation "wb_lbox.h"
#endif
#ifdef __GNUG__
#pragma implementation "wb_rbox.h"
#endif
#ifdef __GNUG__
#pragma implementation "wb_buttn.h"
#endif
#ifdef __GNUG__
#pragma implementation "wb_choic.h"
#endif
#ifdef __GNUG__
#pragma implementation "wb_check.h"
#endif
#ifdef __GNUG__
#pragma implementation "wb_messg.h"
#endif
#ifdef __GNUG__
#pragma implementation "wb_slidr.h"
#endif
#ifdef __GNUG__
#pragma implementation "wb_slidr.h"
#endif
#ifdef __GNUG__
#pragma implementation "wb_menu.h"
#endif
#ifdef __GNUG__
#pragma implementation "wb_mnuit.h"
#endif
#ifdef __GNUG__
#pragma implementation "wb_menu.h"
#endif
#ifdef __GNUG__
#pragma implementation "wb_gauge.h"
#endif
#endif

#include "common.h"
#include "wx_setup.h"

#include "wx_mac_utils.h"
#include "wx_item.h"
#include "wx_slidr.h"
#include "wx_choic.h"
#include "wx_lbox.h"
#include "wx_rbox.h"
#include "wx_buttn.h"
#include "wx_check.h"
#include "wx_messg.h"
#include "wx_menu.h"

#if USE_GAUGE
#include "wx_gauge.h"
#endif

#include "wx_stdev.h"
#include "wx_utils.h"

#include "wx_stdev.h"

/* When implementing a new item, be sure to:

 * - add the item to the parent panel
 * - set window_parent to the parent
 * - NULL any extra child window pointers not created for this item
 *   (e.g. label control that wasn't needed)
 * - delete any extra child windows in the destructor (e.g. label control)
 * - implement GetSize and SetSize
 * - to find panel position if coordinates are (-1, -1), use GetPosition
 * - call AdvanceCursor after creation, for panel layout mechanism.
 *
 */

/*
   Motif notes

   A panel is a form.
   Each item is created on a RowColumn or Form of its own, to allow a label to
   be positioned. wxListBox and wxMultiText have forms, all the others have RowColumns.
   This is to allow labels to be positioned to the top left (can't do it with a
   RowColumn as far as I know).
   AttachWidget positions widgets relative to one another (left->right, top->bottom)
   unless the x, y coordinates are given (more than -1).
 */

// Item members
wxbItem::wxbItem (void)
{
  __type = wxTYPE_ITEM;
}

wxbItem::wxbItem // Constructor (given parentArea)
	(
		char*		windowName,
		wxArea*		parentArea,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style
	) :
		wxWindow ( windowName, parentArea, wxNEGPOS_IS_DEFAULT(x), wxNEGPOS_IS_DEFAULT(y), width, height, style)
{
  if (style & wxVERTICAL_LABEL)
    labelPosition = wxVERTICAL;
  else
    labelPosition = wxHORIZONTAL;
}

//-----------------------------------------------------------------------------
wxbItem::wxbItem // Constructor (given parentWindow)
	(
		char*		windowName,
		wxWindow*	parentWindow,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style
	) :
		wxWindow ( windowName, parentWindow, wxNEGPOS_IS_DEFAULT(x), wxNEGPOS_IS_DEFAULT(y), width, height, style)
{
  if (style & wxVERTICAL_LABEL)
    labelPosition = wxVERTICAL;
  else
    labelPosition = wxHORIZONTAL;
}

//-----------------------------------------------------------------------------
wxbItem::wxbItem // Constructor (given objectType; i.e., menu or menuBar)
	(
		char*		windowName
	) :
		wxWindow ( windowName)
{
  labelPosition = wxHORIZONTAL;
}

wxbItem::~wxbItem (void)
{
  wxPanel *parent;
  parent = (wxPanel *) GetParent ();
  if (parent)
  {
    // parent is not always a wxPanel: can be a wxMenu...
    if (wxSubType(parent->__type,wxTYPE_PANEL))
    {
      if (parent->defaultItem == this)
        parent->defaultItem = NULL;
    }
  }
}

void wxbItem::Centre (int direction, wxWindow *)
{
  int x, y, width, height, panel_width, panel_height, new_x, new_y;
  wxPanel *panel;

  panel = (wxPanel *) GetParent ();
  if (!panel)
    return;

  panel->GetClientSize (&panel_width, &panel_height);
  GetSize (&width, &height);
  GetPosition (&x, &y);

  new_x = x;
  new_y = y;

  if (direction & wxHORIZONTAL)
    new_x = (int) ((panel_width - width) / 2);

  if (direction & wxVERTICAL)
    new_y = (int) ((panel_height - height) / 2);

  SetSize (new_x, new_y, width, height, wxPOS_USE_MINUS_ONE);
}

void wxbItem::Command (wxCommandEvent * event)
{
  ProcessCommand (event);
}

void wxbItem::ProcessCommand (wxCommandEvent * event)
{
  wxFunction fun = callback;
  if (fun)
    fun(this, event);
}

wxbButton::wxbButton (wxPanel * panel, wxFunction Function,
	   int x, int y, int width, int height, long style, char *name)
	: wxItem (panel, x, y, width, height, style, name)
{
  __type = wxTYPE_BUTTON;
  windowStyle = style;
  window_parent = panel;
}

wxbButton::~wxbButton (void)
{
}

// Menus

// Construct a menu with optional title (then use append)
wxbMenu::wxbMenu (char *Title, wxFunction func)
{
  __type = wxTYPE_MENU;
  no_items = 0;
  menu_bar = NULL;
  if (Title) {
    title = copystring (Title);
  } else
    title = NULL;
}

wxbMenu::wxbMenu (char* Title, char* windowName)
  : wxItem( windowName)
{
  __type = wxTYPE_MENU;
  no_items = 0;
  menu_bar = NULL;
  if (Title) {
    title = copystring (Title);
  } else
    title = NULL;
}


// The wxWindow destructor will take care of deleting the submenus.
wxbMenu::~wxbMenu (void)
{
  title = NULL;
}

// Finds the item id matching the given string, -1 if not found.
int wxbMenu::FindItem (char *itemString)
{
  char buf1[200];
  char buf2[200];
  wxNode *node;
  wxMenuItem *item;

  wxStripMenuCodes (itemString, buf1);

  for (node = menuItems->First (); node; node = node->Next ()) {
    item = (wxMenuItem *) node->Data ();
    if (item->subMenu) {
      int ans;
      ans = item->subMenu->FindItem (itemString);
      if (ans > -1)
	return ans;
    }
    if ((item->itemId > -1) && item->itemName) {
      wxStripMenuCodes (item->itemName, buf2);
      if (strcmp (buf1, buf2) == 0)
	return item->itemId;
    }
  }
  
  return -1;
}

wxMenuItem *wxbMenu::FindItemForId (int itemId, wxMenu ** itemMenu)
{
  wxNode *node;
  wxMenuItem *item;

  if (itemMenu)
    *itemMenu = NULL;
  
  for (node = menuItems->First (); node; node = node->Next ()) {
    item = (wxMenuItem *) node->Data ();

    if (item->itemId == itemId) {
      if (itemMenu)
	*itemMenu = (wxMenu *) this;
      return item;
    }

    if (item->subMenu) {
      wxMenuItem *ans;
      ans = item->subMenu->FindItemForId (itemId, itemMenu);
      if (ans)
	return ans;
    }
  }
  
  if (itemMenu)
    *itemMenu = NULL;
  return NULL;
}

void wxbMenu::SetHelpString (int itemId, char *helpString)
{
  wxMenuItem *item;
  item = FindItemForId (itemId);
  if (item) {
    if (helpString) {
      item->helpString = helpString;
    } else {
      item->helpString = copystring (helpString);
    }
  }
}

char *wxbMenu::GetHelpString (int itemId)
{
  wxMenuItem *item;
  item = FindItemForId (itemId);
  if (item)
    return item->helpString;
  else
    return NULL;
}

// Menu Bar

wxbMenuBar::wxbMenuBar (void)
{
  __type = wxTYPE_MENU_BAR;
  n = 0;
  menus = NULL;
  titles = NULL;
  menu_bar_frame = NULL;
}

wxbMenuBar::wxbMenuBar(char* windowName)
  : wxItem(windowName)
{
  __type = wxTYPE_MENU_BAR;
  n = 0;
  menus = NULL;
  titles = NULL;
  menu_bar_frame = NULL;
}

//-----------------------------------------------------------------------------
wxbMenuBar::wxbMenuBar(int N, wxMenu* Menus[],
		       char* Titles[],
		       char* windowName)
  : wxItem( windowName)
{
  wxMenu *menu;

  n = N;
  menus = Menus;
  titles = Titles;
  menu_bar_frame = NULL;
  __type = wxTYPE_MENU_BAR;
	
  for (int i = 0; i < n; i++) {
    menu = Menus[i];
    menu->menu_bar = (wxMenuBar*) this;
    menu->SetTitle(titles[i]);
  }
}

wxbMenuBar::wxbMenuBar (int N, wxMenu * Menus[], char *Titles[])
{
  wxMenu *menu;

  __type = wxTYPE_MENU_BAR;
  n = N;
  menus = Menus;
  titles = Titles;
  menu_bar_frame = NULL;
  for (int i = 0; i < N; i++) {
    menu = menus[i];
    menu->menu_bar = (wxMenuBar *) this;
  }
}

wxbMenuBar::~wxbMenuBar (void)
{
}


void wxbMenuBar::Append (wxMenu * menu, char *title)
{
  OnAppend(menu, title);
}

Bool wxbMenuBar::Delete(wxMenu * menu, int i)
{
  if (menu) {
    for (i = 0; i < n; i++) {
      if (menus[i] == menu)
	break;
    }	
    if (i >= n)
      return FALSE;
  } else {
    if (i < 0 || i >= n)
      return FALSE;
    menu = menus[i];
  }
  
  if (!OnDelete(menu, i))
    return FALSE;

  return TRUE;
}

int wxbMenuBar::Number(void)
{
  return n;
}

// Find the menu menuString, item itemString, and return the item id.
// Returns -1 if none found.
int wxbMenuBar::FindMenuItem (char *menuString, char *itemString)
{
  char buf1[200];
  char buf2[200];
  wxMenu *menu;

  wxStripMenuCodes (menuString, buf1);
  for (int i = 0; i < n; i++)
    {
      wxStripMenuCodes (titles[i], buf2);
      if (strcmp (buf1, buf2) == 0) {
	menu = menus[i];
	return menu->FindItem (itemString);
      }
    }
  return -1;
}

wxMenuItem *wxbMenuBar::FindItemForId (int Id, wxMenu ** itemMenu)
{
  wxMenuItem *item = NULL;
  wxMenu *menu;

  if (itemMenu)
    *itemMenu = NULL;

  for (int i = 0; i < n; i++) {
    menu = menus[i];
    item = menu->FindItemForId(Id, itemMenu);
    if (item)
      return item;
  }
  return NULL;
}

void wxbMenuBar::SetHelpString (int Id, char *helpString)
{
  wxMenu *menu;

  for (int i = 0; i < n; i++) {
    menu = menus[i];
    if (menu->FindItemForId (Id)) {
      menu->SetHelpString (Id, helpString);
      return;
    }
  }
}

char *wxbMenuBar::GetHelpString (int Id)
{
  wxMenu *menu;

  for (int i = 0; i < n; i++) {
    menu = menus[i];
    if (menu->FindItemForId (Id))
      return menu->GetHelpString (Id);
  }
  return NULL;
}

wxbCheckBox::wxbCheckBox (wxPanel * panel, wxFunction func,
	     int x, int y, int width, int height, long style, char *name)
	: wxItem (panel, x, y, width, height, style, name)
{
  __type = wxTYPE_CHECK_BOX;
  windowStyle = style;
  window_parent = panel;
}

wxbCheckBox::~wxbCheckBox (void)
{
}

wxbChoice::wxbChoice (void)
{
  __type = wxTYPE_CHOICE;
}


wxbChoice::wxbChoice (wxPanel * panel, wxFunction func, char *Title,
	   int x, int y, int width, int height, int N, char **Choices,
	   long style, char *name)
	: wxItem (panel, x, y, width, height, style, name)
{
  __type = wxTYPE_CHOICE;
}

wxbChoice::~wxbChoice (void)
{
}

// Listbox item

wxbListBox::wxbListBox (wxPanel * panel, wxFunction func,
	    char *Title, Bool Multiple,
	    int x, int y, int width, int height,
	    int N, char **Choices, long style, char *name)
	: wxItem (panel, x, y, width, height, style, name)
{
  __type = wxTYPE_LIST_BOX;
  windowStyle = style;
  selected = -1;
  selections = 0;
  multiple = Multiple;
  window_parent = panel;
  no_items = N;
}


wxbListBox::~wxbListBox (void)
{
}

int wxbListBox::Number (void)
{
  return no_items;
}

wxbRadioBox::wxbRadioBox (wxPanel * panel, wxFunction func,
			  char *Title,
			  int x, int y, int width, int height,
			  int N,
			  int majorDim, long style, char *name)
	: wxItem (panel, x, y, width, height, style, name)
{
  __type = wxTYPE_RADIO_BOX;
  windowStyle = style;
  selected = -1;
  window_parent = panel;
  no_items = N;
}

wxbRadioBox::~wxbRadioBox (void)
{
}

int wxbRadioBox::Number (void)
{
  return no_items;
}

// Message
wxbMessage::wxbMessage (void)
{
  __type = wxTYPE_MESSAGE;
}

wxbMessage::wxbMessage (wxPanel * panel, char *label, int x, int y, long style, char *name)
	: wxItem (panel, x, y, -1, -1, style, name)
{
  __type = wxTYPE_MESSAGE;
  windowStyle = style;
  window_parent = panel;
}

wxbMessage::wxbMessage (wxPanel * panel, wxBitmap *image, int x, int y, long style, char *name)
	: wxItem (panel, x, y, -1, -1, style, name)
{
  __type = wxTYPE_MESSAGE;
  windowStyle = style;
  window_parent = panel;
}

//-----------------------------------------------------------------------------
wxbMessage::wxbMessage // Constructor (given parentArea)
	(
		wxArea*		parentArea,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style,
		char*		windowName
	) :
		wxItem (parentArea, x, y, width, height, style, windowName)
{
  __type = wxTYPE_MESSAGE;
}

//-----------------------------------------------------------------------------
wxbMessage::wxbMessage // Constructor (given parentWindow)
	(
		wxWindow*	parentWindow,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style,
		char*		windowName
	) :
		wxItem (parentWindow, x, y, width, height, style, windowName)
{
  __type = wxTYPE_MESSAGE;
}


wxbMessage::~wxbMessage (void)
{
}

wxbSlider::wxbSlider (wxPanel * panel, wxFunction func, char *label, int value,
	   int min_value, int max_value, int width, int x, int y, long style, char *name)
	: wxItem (panel, x, y, width, -1, style, name)
{
  __type = wxTYPE_SLIDER;
  windowStyle = style;
  window_parent = panel;
}

wxbSlider::~wxbSlider (void)
{
}


wxbGauge::wxbGauge (wxPanel * panel, char *label,
	   int range, int x, int y, int width, int height, long style, char *name)
	  : wxItem (panel, x, y, width, height, style, name)
{
  __type = wxTYPE_GAUGE;
  windowStyle = style;
  window_parent = panel;
}

wxbGauge::~wxbGauge (void)
{
}


wbMenuItem::wbMenuItem()
: wxObject(WXGC_NO_CLEANUP)
{ 
  itemId = 0; itemName = NULL; topMenu = NULL; subMenu = NULL;
  menuBar = NULL; helpString = NULL;
}

wbMenuItem::~wbMenuItem()
{
}
