/*
 * File:        wb_item.cc
 * Purpose:     Panel items implementation: base (platform-independent) code
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	March 1995
 * Copyright:   (c) 2004-2010 PLT Scheme Inc.
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include <math.h>
#include <stdlib.h>

// Item members
wxbItem::wxbItem (wxPanel *panel)
{
  __type = wxTYPE_ITEM;

  if (panel) { // NULL for menus
    window_parent = panel;
    
    labelPosition = panel->label_position;
  }
}

wxbItem::~wxbItem (void)
{
  wxPanel *parent;

  parent = (wxPanel *) GetParent ();
  if (parent) {
    // parent is not always a wxPanel: can be a wxMenu...
    if (wxSubType(parent->__type,wxTYPE_PANEL)) {
      if (parent->defaultItem == this)
        parent->defaultItem = NULL;
    }
  }
}

void wxbItem::SetClientSize (int width, int height)
{
  SetSize (-1, -1, width, height);
}

void wxbItem::Centre (int direction)
{
  int x, y, width, height, panel_width, panel_height, new_x, new_y;
  int temp_x, temp_y;
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

  SetSize (new_x, new_y, width, height);
  GetPosition (&temp_x, &temp_y);
  GetPosition (&temp_x, &temp_y);
}

void wxbItem::Command (wxCommandEvent *event)
{
  ProcessCommand(event);
}

void wxbItem::ProcessCommand(wxCommandEvent *event)
{
  if (callback)
    callback(this, event);
}

/*
 * Button
 */
 
wxbButton::wxbButton (wxPanel * panel, wxFunction WXUNUSED(Function), 
		      char *WXUNUSED(label),
		      int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width), 
		      int WXUNUSED(height), long style, char *WXUNUSED(name))
: wxItem(panel)
{
  __type = wxTYPE_BUTTON;
  windowStyle = style;
  labelPosition = wxHORIZONTAL;
}

wxbButton::wxbButton (wxPanel * panel, wxFunction WXUNUSED(Function), 
		      wxBitmap * WXUNUSED(bitmap),
		      int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width),
		      int WXUNUSED(height), long style, char *WXUNUSED(name))
: wxItem(panel)
{
  __type = wxTYPE_BUTTON;
  windowStyle = style;
  labelPosition = wxHORIZONTAL;
}

wxbButton::~wxbButton (void)
{
}

/*
 * Menu item
 */

wbMenuItem::wbMenuItem(void) { 
  itemId = 0; itemName = NULL; topMenu = NULL; subMenu = NULL;
  menuBar = NULL; helpString = NULL;
  WXGC_IGNORE(this, menuBar); WXGC_IGNORE(this, topMenu);
}

wbMenuItem::~wbMenuItem(void) {
  menuBar = NULL;
  topMenu = NULL;
}

/*
 * Menu
 */

// Construct a menu with optional title (then use append)
wxbMenu::wxbMenu (char *Title, wxFunction WXUNUSED(func))
: wxItem(NULL)
{
  __type = wxTYPE_MENU;
  no_items = 0;
  menu_bar = NULL;
  WXGC_IGNORE(this, menu_bar);
  WXGC_IGNORE(this, top_level_menu);
  if (Title) {
    title = copystring (Title);
  } else
    title = NULL;
  menuItems = new wxList();
}

// The wxWindow destructor will take care of deleting the submenus.
wxbMenu::~wxbMenu (void)
{
  menu_bar= NULL;
  top_level_menu = NULL;
}

wxMenuItem *wxbMenu::FindItemForId (long itemId, wxMenu ** itemMenu, int * pos)
{
  wxNode * node;

  int i = 0;
  if (itemMenu)
    *itemMenu = NULL;
  for (node = menuItems->First (); node; node = node->Next (), i++) {
    wxMenuItem *item;

    item = (wxMenuItem *) node->Data ();

    if (item->itemId == itemId) {
      if (itemMenu)
	*itemMenu = (wxMenu *) this;
      if (pos)
	*pos = i;
      return item;
    }

    if (item->subMenu) {
      wxMenuItem *ans;

      ans = item->subMenu->FindItemForId (itemId, itemMenu, pos);
      if (ans)
	return ans;
    }
  }

  if (itemMenu)
    *itemMenu = NULL;
  return NULL;
}

void wxbMenu::SetHelpString(long itemId, char *helpString)
{
  wxMenuItem *item;
  item = FindItemForId (itemId);
  if (item) {
    if (helpString) {
      item->helpString = copystring(helpString);
    } else
      item->helpString = NULL;
  }
}

char *wxbMenu::GetHelpString(long itemId)
{
  wxMenuItem *item;
  item = FindItemForId(itemId);
  if (item)
    return item->helpString;
  else
    return NULL;
}

void wxbMenu::ProcessCommand (wxCommandEvent *event)
{
  if (callback)
    callback(this, event);
}

/*
 * Menu Bar
 */

wxbMenuBar::wxbMenuBar (void)
: wxItem(NULL)
{
  __type = wxTYPE_MENU_BAR;
  n = 0;
  menus = NULL;
  titles = NULL;
  menu_bar_frame = NULL;
  WXGC_IGNORE(this, menu_bar_frame);
}

wxbMenuBar::wxbMenuBar (int N, wxMenu * Menus[], char *Titles[])
: wxItem(NULL)
{
  wxMenu *menu;
  int i;

  __type = wxTYPE_MENU_BAR;
  n = N;
  menus = Menus;
  titles = Titles;
  menu_bar_frame = NULL;
  for (i = 0; i < N; i++) {
    menu = menus[i];
    menu->menu_bar = (wxMenuBar *)this;
  }
  WXGC_IGNORE(this, menu_bar_frame);
}

wxbMenuBar::~wxbMenuBar (void)
{
  menu_bar_frame = NULL;
}

void wxbMenuBar::Append (wxMenu * menu, char *title)
{
  wxMenu **new_menus;
  char **new_titles, *nt;
  int i;

  if (!OnAppend(menu, title))
    return;

  n++;
  new_menus = new wxMenu *[n];
  new_titles = new char *[n];

  for (i = 0; i < n - 1; i++) {
    new_menus[i] = menus[i];
    menus[i] = NULL;
    new_titles[i] = titles[i];
    titles[i] = NULL;
  }
  menus = new_menus;
  titles = new_titles;

  menus[n - 1] = menu;
  nt = copystring(title);
  titles[n - 1] = nt;

  menu->menu_bar = (wxMenuBar *)this;
  menu->SetParent(this);
}

Bool wxbMenuBar::Delete(wxMenu * menu, int i)
{
  int j;

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

  menu->SetParent(NULL);

  --n;
  for (j = i; j < n; j++) {
    menus[j] = menus[j + 1];
    titles[j] = titles[j + 1];
  }

  return TRUE;
}

int wxbMenuBar::Number(void)
{
  return n;
}

wxMenuItem *wxbMenuBar::FindItemForId (long Id, wxMenu **itemMenu)
{
  wxMenuItem *item = NULL;
  int i;
  wxMenu *menu;

  if (itemMenu)
    *itemMenu = NULL;

  for (i = 0; i < n; i++) {
    menu = menus[i];
    if ((item = menu->FindItemForId (Id, itemMenu)))
      return item;
  }
  return NULL;
}

void wxbMenuBar::SetHelpString (long Id, char *helpString)
{
  int i;
  wxMenu *menu;
  for (i = 0; i < n; i++) {
    menu = menus[i];
    if (menu->FindItemForId (Id)) {
      menu->SetHelpString (Id, helpString);
      return;
    }
  }
}

char *wxbMenuBar::GetHelpString (long Id)
{
  int i;
  wxMenu *menu;
  for (i = 0; i < n; i++) {
    menu = menus[i];
    if (menu->FindItemForId(Id))
      return menu->GetHelpString (Id);
  }
  return NULL;
}

/*
 * Single check box item
 */
 
wxbCheckBox::wxbCheckBox (wxPanel * panel, wxFunction WXUNUSED(func), char *WXUNUSED(Title),
	     int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width), int WXUNUSED(height), long style, char *WXUNUSED(name))
: wxItem(panel)
{
  __type = wxTYPE_CHECK_BOX;
  windowStyle = style;
}

wxbCheckBox::wxbCheckBox (wxPanel * panel, wxFunction WXUNUSED(func), wxBitmap * WXUNUSED(bitmap),
	     int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width), int WXUNUSED(height), long style, char *WXUNUSED(name))
: wxItem(panel)
{
  __type = wxTYPE_CHECK_BOX;
  windowStyle = style;
}

wxbCheckBox::~wxbCheckBox (void)
{
}

/*
 * Choice
 */
 
wxbChoice::wxbChoice (wxPanel * panel, wxFunction WXUNUSED(func), char *WXUNUSED(Title),
	   int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width), int WXUNUSED(height), int N, char **WXUNUSED(Choices),
	   long style, char *WXUNUSED(name))
: wxItem(panel)
{
  __type = wxTYPE_CHOICE;
  windowStyle = style;
  no_strings = N;
}

wxbChoice::~wxbChoice (void)
{
}

char *wxbChoice::GetStringSelection (void)
{
  int sel;
  sel = GetSelection ();
  if (sel > -1)
    return this->GetString (sel);
  else
    return NULL;
}

Bool wxbChoice::SetStringSelection (char *s)
{
  int sel;
  sel = FindString (s);
  if (sel > -1) {
    SetSelection (sel);
    return TRUE;
  } else
    return FALSE;
}


/*
 * Listbox
 */
 
wxbListBox::wxbListBox(wxPanel * panel, wxFunction WXUNUSED(func),
		       char *WXUNUSED(Title), Bool Multiple,
		       int WXUNUSED(x), int WXUNUSED(y), 
		       int WXUNUSED(width), int WXUNUSED(height),
		       int WXUNUSED(N), char **WXUNUSED(Choices),
		       long style, char *WXUNUSED(name))
: wxItem(panel)
{
  __type = wxTYPE_LIST_BOX;
  windowStyle = style;
  selected = -1;
  selections = 0;
  multiple = Multiple;
  no_items = 0;
}

wxbListBox::~wxbListBox (void)
{
}

int wxbListBox::Number (void)
{
  return no_items;
}

// For single selection items only
char *wxbListBox::GetStringSelection (void)
{
  int sel;
  sel = GetSelection ();
  if (sel > -1)
    return this->GetString (sel);
  else
    return NULL;
}

Bool wxbListBox::SetStringSelection (char *s)
{
  int sel;
  sel = FindString(s);
  if (sel > -1) {
    SetOneSelection(sel);
    return TRUE;
  } else
    return FALSE;
}


/*
 * Radiobox item
 */
 
wxbRadioBox::wxbRadioBox (wxPanel * panel, wxFunction WXUNUSED(func),
			  char *WXUNUSED(Title),
			  int WXUNUSED(x), int WXUNUSED(y),
			  int WXUNUSED(width), int WXUNUSED(height),
			  int WXUNUSED(N), char **WXUNUSED(Choices),
			  int WXUNUSED(majorDim), long style, char *WXUNUSED(name))
: wxItem(panel)
{
  __type = wxTYPE_RADIO_BOX;
  windowStyle = style;
  selected = -1;
  no_items = 0;
}

wxbRadioBox::wxbRadioBox (wxPanel * panel, wxFunction WXUNUSED(func),
			  char *WXUNUSED(Title),
			  int WXUNUSED(x), int WXUNUSED(y),
			  int WXUNUSED(width), int WXUNUSED(height),
			  int WXUNUSED(N), wxBitmap ** WXUNUSED(Choices),
			  int WXUNUSED(majorDim), long style, char *WXUNUSED(name))
: wxItem(panel)
{
  __type = wxTYPE_RADIO_BOX;
  windowStyle = style;
  selected = -1;
  no_items = 0;
}

wxbRadioBox::~wxbRadioBox (void)
{
}

int wxbRadioBox::Number (void)
{
  return no_items;
}

// For single selection items only
char *wxbRadioBox::GetStringSelection (void)
{
  int sel;
  sel = GetSelection ();
  if (sel > -1)
    return this->GetString (sel);
  else
    return NULL;
}

Bool wxbRadioBox::SetStringSelection (char *s)
{
  int sel;
  sel = FindString (s);
  if (sel > -1) {
    SetSelection (sel);
    return TRUE;
  } else
    return FALSE;
}


/*
 * Message
 */
 
wxbMessage::wxbMessage (wxPanel * panel, char *WXUNUSED(label),
			int WXUNUSED(x), int WXUNUSED(y),
			long style, char *WXUNUSED(name))
: wxItem(panel)
{
  __type = wxTYPE_MESSAGE;
  windowStyle = style;
}

wxbMessage::wxbMessage (wxPanel * panel, wxBitmap *WXUNUSED(image), 
			int WXUNUSED(x), int WXUNUSED(y),
			long style, char *WXUNUSED(name))
: wxItem(panel)
{
  __type = wxTYPE_MESSAGE;
  windowStyle = style;
}

wxbMessage::~wxbMessage (void)
{
}

/*
 * Slider
 */
 
wxbSlider::wxbSlider (wxPanel * panel, wxFunction WXUNUSED(func), 
		      char *WXUNUSED(label), int WXUNUSED(value),
		      int WXUNUSED(min_value), int WXUNUSED(max_value), int WXUNUSED(width),
		      int WXUNUSED(x), int WXUNUSED(y), long style, char *WXUNUSED(name))
: wxItem(panel)
{
  __type = wxTYPE_SLIDER;
  windowStyle = style;
}

wxbSlider::~wxbSlider (void)
{
}

/*
 * Gauge
 */
 
wxbGauge::wxbGauge (wxPanel * panel, char *WXUNUSED(label),
		    int WXUNUSED(range), int WXUNUSED(x), int WXUNUSED(y),
		    int WXUNUSED(width), int WXUNUSED(height), 
		    long style, char *WXUNUSED(name))
: wxItem(panel)
{
  __type = wxTYPE_GAUGE;
  windowStyle = style;
  labelPosition = wxHORIZONTAL;
}

wxbGauge::~wxbGauge (void)
{
}
