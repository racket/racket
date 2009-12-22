///////////////////////////////////////////////////////////////////////////////
// File:	wx_menu.cc (split from wx_item.cc)
// Purpose:	Menu objects implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wx_menu.h"
#include "wx_mnuit.h"
#include "wx_frame.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wx_main.h"
#ifndef WX_CARBON
# include <Strings.h>
# include <Balloons.h>	
#endif

int wxNumHelpItems;
MenuHandle wxHelpMenu;

extern int wxKeyCodeToVirtualKey(int wxk);

void wxSetUpAppleMenu(wxMenuBar *mbar);

extern int wxCan_Do_Pref();

static int mb_hidden;

///////////////////////////////////////////////////////////////////////////////
// Menu Bar
///////////////////////////////////////////////////////////////////////////////


// mflatt: In a wxMenu object:
//          menu_bar is now only used by top-level menus in the menu bar
//          top_level_menu is not set or used at all
//          window_parent is set to the menu_bar for a top-level menu, or the
//           parent menu for a submenu
//         menuBar in a wxMenuItem is not used ever
//
//    When a menu bar is deleted, it deletes all menus it conatins
//    When a menu is deleted, it deletes all submenus it contains
//    The children list of a manu/menubar is not used
//
//    When inserting a test item into a menu, Special mac characters are ignored.
//
//    NOTE: A great deal of confusion is based on the fact that wxMenu and
//          wxMenuBar are derived from wxItem. They are not windows or items
//          and should not be derived from wxItem. Perhaps we can get this fixed...

//=============================================================================
// Public constructors
//=============================================================================

wxMenuBar::wxMenuBar // Constructor (given objectType)
(
 char*		windowName,
 WXTYPE		objectType
 ) :
  wxbMenuBar (windowName),
  wxHelpHackMenu(NULL),
  iHelpMenuHackNum(0)
{
  WXGC_IGNORE(this, menu_bar_frame);
}

wxMenuBar::wxMenuBar // Constructor (given parentPanel, label)
(
 int 		N,
 wxMenu* 	Menus[],
 char* 		Titles[],
 char*		windowName,
 WXTYPE		objectType
 ) :
  wxbMenuBar (N, Menus, Titles, windowName),
  wxHelpHackMenu(NULL),
  iHelpMenuHackNum(0)
{
  WXGC_IGNORE(this, menu_bar_frame);
}

//=============================================================================
// Public destructor
//=============================================================================

static wxMenuBar *last_installed_bar;

void wxRegisterLastInstalledBar();

void wxRegisterLastInstalledBar()
{
  wxREGGLOB(last_installed_bar);
}


//-----------------------------------------------------------------------------
wxMenuBar::~wxMenuBar(void)
{
  wxMenu *menu;

  if (last_installed_bar == this) {
    wxPrepareMenuDraw();
    ::ClearMenuBar();
    wxDoneMenuDraw();
    last_installed_bar = NULL;
  }

  if (menu_bar_frame)  {
    menu_bar_frame->wx_menu_bar = 0;
    menu_bar_frame = NULL;
  }
  for (int i = 0; i < n; i++) {
    menu = menus[i];
    menu->menu_bar = NULL;  // So menu doesn't try to remove itself
    menu->window_parent = NULL; // So menu doesn't try to remove itself
    DELETE_OBJ menu;
  }
}

static int unhilite_before_change = FALSE;

void wxPrepareMenuDraw(void)
{
  if (unhilite_before_change) {
    HiliteMenu(0);
    unhilite_before_change = FALSE;
  }
}

void wxDoneMenuDraw(Bool menu_hilited)
{
  unhilite_before_change = menu_hilited;
}

static void wxInvalMenuBar(void)
{
  wxPrepareMenuDraw();
  ::DrawMenuBar();
  wxDoneMenuDraw();
}

///////////////////////////////////////////////////////////////////////////////
// Menus
///////////////////////////////////////////////////////////////////////////////

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
// Construct a menu with optional title (then use append)
//-----------------------------------------------------------------------------
wxMenu::wxMenu // Constructor (given objectType)
(
 char*		Title,
 wxFunction	function,
 wxFont         *_font,
 char*		windowName,
 WXTYPE		objectType
 ) :
  wxbMenu(Title, windowName)
{
  menuItems = new WXGC_PTRS wxList();

  SetFont(_font, 14);

  Callback(function);

  cMacMenuId = gMenuIdCounter++; // get next unique menuID
  if (title && title[0]) {
    unsigned char *s;
    s = wxC2P(title);
    cMacMenu = NewMenu(cMacMenuId, s);
  } else {
    cMacMenu = NewMenu(cMacMenuId, "\p ");
  }
  CheckMemOK(cMacMenu);
  WXGC_IGNORE(this, menu_bar);
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
// The wxWindow destructor will take care of deleting the submenus.
//-----------------------------------------------------------------------------
wxMenu::~wxMenu(void)
{
  wxNode* node;
  wxMenuItem* item;

  if (menu_bar) { // may have to remove menu from the current mac menu bar
    menu_bar->Delete(this);
    menu_bar = NULL;
  } else if (window_parent) {
    ((wxMenu *)window_parent)->Delete(this, 0, -1);
    window_parent = NULL;
  }

  ::DisposeMenu(cMacMenu); // does not dispose of submenus

  node = menuItems->First();
  // mflatt: The menuItems list is not DeleteContents(TRUE).
  //         Also, we need to delete any submenus attached to this menu
  while (node) {
    item = (wxMenuItem*)node->Data();
    if (item->subMenu) {
      item->subMenu->window_parent = NULL; // so it doesn't try to delete itself
      DELETE_OBJ item->subMenu;
    }
    DELETE_OBJ item;
    node = node->Next();
  }
}

// Helper function - Convert a wxMenu String into one you can give
// to the Mac menu mgr AppendMenu();
// setupstr should be used with AppendItem; the result should then be used with SetMenuItemText
// If stripCmds is TRUE, instead of replacing wxMenu string special chars, 
// we delete them. This is appropriate for the menu text of a pulldown Menu.
char *wxBuildMacMenuString(StringPtr setupstr, char *itemName,
			   int *spc, int *modifiers, int *is_virt)
{
  int s, d;
  char *showstr;

  showstr = copystring(itemName);

  d = 0;
  if (itemName[0] == '-') // Fix problem with leading hyphen
    showstr[d++] = ' ';
  for (s = 0; itemName[s] != '\0'; ) {
    if (itemName[s] == '\t') {
      if (spc) {
	s++;
	if (strncmp("Cmd+", itemName + s, 4) == 0) {
	  *spc = itemName[s+4];
	  *modifiers = 0;
	  *is_virt = 0;
	}
	if (strncmp("Cut=", itemName + s, 4) == 0) {
	  /* 1 byte for modfiers, then string version of a integer: */
	  int vk;
	  *modifiers = itemName[s+4] - 'A';
	  vk = strtol(itemName XFORM_OK_PLUS (s + 5), NULL, 10);
	  *spc = vk;
	  vk = wxKeyCodeToVirtualKey(*spc);
	  if (vk) {
	    *spc = vk;
	    *is_virt = 1;
	  } else
	    *is_virt = 0;
	}
      }
      break;
    } 
    else {
      showstr[d++] = itemName[s];
    }
    s++;
  }
  showstr[d] = 0;
  /* Now remove ampersands, etc.: */
  showstr = wxItemStripLabel(showstr);
  if (setupstr) {
    setupstr[1] = 'X'; // temporary menu item name
    setupstr[0] = 1;
  }

  return showstr;
}

MenuHandle wxMenu::CreateCopy(char *title, Bool doabouthack, MenuHandle toHandle)
{
  Str255 tempString;
  int i, offset;
  MenuHandle nmh;
  int helpflg;
  int hId;
  int cnt;
  int spc, modifiers, is_virt;
  wxNode* node;
  wxMenuItem* menuItem;
	
  if (!toHandle)  {
    title = wxItemStripLabel(title);
    helpflg = 0;
    nmh = ::NewMenu(cMacMenuId , "\pTemp");
    CheckMemOK(nmh);
    {
      CFStringRef ct;
      ct = wxCFString(title);
      SetMenuTitleWithCFString(nmh, ct);
      CFRelease(ct);
    }
    offset = 1;
    if (helpflg && menu_bar && menu_bar->n) {
      if (menu_bar->menus[menu_bar->n - 1] == this) {
	menu_bar->wxHelpHackMenu = this;
	menu_bar->iHelpMenuHackNum = 0;
      } else
	helpflg = 0;
    }
  } else {
    nmh = toHandle;
    offset = CountMenuItems(nmh) + 1;
    helpflg = 0;
  }
  cnt = menuItems->Number();
  // Create a new Mac Menu 
  node = menuItems->First();
  for (i = 0; i < cnt; i++) {
    // Try to recreate from the wxMenuItem
    menuItem = (wxMenuItem*)node->Data(); 
    hId = 0;
    spc = 0;
    if (menuItem->itemId == -1) {
      // Separator
      title = "-";
      tempString[0] = 1;
      tempString[1] = '-';
    } else if (menuItem->subMenu) {
      wxMenu *subMenu;
      title = wxBuildMacMenuString(tempString, menuItem->itemName, NULL, NULL, NULL);
      subMenu = menuItem->subMenu;
      subMenu->wxMacInsertSubmenu();
      ::InsertMenu(subMenu->cMacMenu, -1);
      hId = subMenu->cMacMenuId;
    } else {
      title = wxBuildMacMenuString(tempString, menuItem->itemName, &spc, &modifiers, &is_virt);
      if (!i && doabouthack && helpflg && (!strncmp("About", title, 5))) {
	if (menu_bar) {
	  // This is a very sad hack !
	  menu_bar->iHelpMenuHackNum = 1;
	}
      }
    }
    ::AppendMenu(nmh, tempString);
    {
      CFStringRef ct;
      ct = wxCFString(title);
      ::SetMenuItemTextWithCFString(nmh, i + offset, ct);
      CFRelease(ct);
    }
    if (spc) {
      SetMenuItemCommandKey(nmh, i + offset, is_virt, spc);
      SetMenuItemModifiers(nmh, i + offset, modifiers);
    }
    {
      Bool v;
      v = menuItem->IsChecked();
      if (v) {
	::CheckMenuItem(nmh, i + offset, TRUE);
      }
      v = menuItem->IsEnabled();
      if (!v || (toHandle && !cEnable)) {
	::DisableMenuItem(nmh, i + offset);
      }
    }
    if (hId) {
      ::SetMenuItemHierarchicalID(nmh, i + offset, hId);
    }
    node = node->Next();						// watch for null?	
  }

  if (GetMenuWidth(nmh) < requestedWidth)
    SetMenuWidth(nmh, requestedWidth);

  SetMenuFont(nmh, font->GetMacFontNum(), font->GetPointSize());
	
  return nmh;
}

// another Helper function - change the Title/Text of a Menu 
// We Have to change the Title in the MenuBar, which is rebuilt in wxFrame::SetMenubar
// Unfortunately, The ToolBox does'nt let you get/set the ItemText for "Menu" (items yes)
// but not the Menu. So the menu will have to be rebuilt. Plenty of room for errors!
void wxMenu::MacChangeMenuText(wxMenu *menu, char *new_title)
{
  // Get Handle to the Menu
  MenuHandle omh = menu->cMacMenu;
	
  title = macCopyString1(new_title);

  // Create a new Mac Menu 
  menu->cMacMenu = menu->CreateCopy(new_title, TRUE);

  // Remove from system Menu List if it might be there and redraw with new menu:
  if (menu->menu_bar && menu->menu_bar == last_installed_bar)
    menu->menu_bar->Install(menu->menu_bar->menu_bar_frame);

  // Dispose the old menu
  ::DisposeMenu(omh);
}


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxMenuBar::Enable(int Id, Bool Flag)
{
  int j;
  wxMenuItem* theMenuItem;
  wxMenu *menu;

  for (j = 0; j < n; j++) {
    menu = menus[j];
    theMenuItem = menu->FindItemForId(Id);
    if (theMenuItem) {
      theMenuItem->Enable(Flag);
      return;
    }
  }
}

//-----------------------------------------------------------------------------
void wxMenuBar::Check(int Id, Bool Flag)
{
  int j;
  wxMenuItem* theMenuItem;
  wxMenu *menu;

  for (j = 0; j < n; j++) {
    menu = menus[j];
    theMenuItem = menu->FindItemForId(Id);
    if (theMenuItem) {
      theMenuItem->Check(Flag);
      return;
    }
  }
}

//-----------------------------------------------------------------------------
Bool wxMenuBar::Checked(int Id)
{
  int j;
  wxMenuItem* theMenuItem;
  wxMenu *menu;

  for (j = 0; j < n; j++) {
    menu = menus[j];
    theMenuItem = menu->FindItemForId(Id);
    if (theMenuItem)
      return theMenuItem->IsChecked();
  }

  return FALSE;
}

//-----------------------------------------------------------------------------
void wxMenuBar::SetLabel(int Id, char* label)
{
  int j;
  wxMenuItem* theMenuItem;
  wxMenu *menu;

  for (j = 0; j < n; j++) {
    menu = menus[j];
    theMenuItem = menu->FindItemForId(Id);
    if (theMenuItem) {
      theMenuItem->SetLabel(label);
      return;
    }
  }
}

//-----------------------------------------------------------------------------
char* wxMenuBar::GetLabel(int Id)
{
  int j;
  wxMenuItem* theMenuItem;
  wxMenu *menu;

  for (j = 0; j < n; j++) {
    menu = menus[j];
    theMenuItem = menu->FindItemForId(Id);
    if (theMenuItem)
      return theMenuItem->GetLabel();
  }

  return NULL;
}

//-----------------------------------------------------------------------------
void wxMenuBar::SetLabelTop(int pos, char* label)
{
  char *s;
  wxMenu *menu;

  if ((pos >= 0) && (pos < n)) {
    menu = menus[pos];
    menu->SetTitle(label);
    s = copystring(label);
    titles[pos] = s;
  }
}

//-----------------------------------------------------------------------------
char* wxMenuBar::GetLabelTop(int pos)
{ 
  if ((pos >= 0) && (pos < n)) {
    wxMenu *menu;
    menu = menus[pos];
    return menu->GetTitle();
  } else
    return NULL;
}

//-----------------------------------------------------------------------------
void wxMenuBar::EnableTop(int pos, Bool flag)
{ 
  /* For consistency with other platforms, 
     disabling is not allowed to work unless the menu bar in installed: */
  if (!menu_bar_frame && !flag)
    return;
	
  if (pos >= 0 && (pos < n)) {
    wxMenu *menu;
    menu = menus[pos];
    menu->Enable(flag);
  }
}

//-----------------------------------------------------------------------------
void wxMenuBar::SetSize(int x, int y, int width, int height) { }

//-----------------------------------------------------------------------------
void wxMenuBar::DoSetSize(int x, int y, int width, int height) { }

//-----------------------------------------------------------------------------
void wxMenuBar::Enable(Bool Flag) { }

//-----------------------------------------------------------------------------
void wxMenuBar::Show(Bool show) { }

//-----------------------------------------------------------------------------
char* wxMenuBar::GetLabel(void) { return NULL; } // The menubar doesn't have a label

//-----------------------------------------------------------------------------
void wxMenuBar::SetLabel(char* label) { } // The menubar doesn't have a label

//-----------------------------------------------------------------------------
wxMenu* wxMenuBar::wxMacFindMenu(int macMenuId)
{
  wxMenu* result = NULL, *menu;
  int i = 0;

  while (i < n && !result) {
    // try to find among main menus
    menu = menus[i];
    if (menu->GetMacMenuId() == macMenuId)
      result = menu;
    else i++;
  }

  if (!result) {
    i = 0;
    while (i < n && !result) {
      // try to find among submenus
      menu = menus[i];
      result = menu->wxMacFindSubmenu(macMenuId);
      if (!result) i++;
    }
  }

  return result;
}

// Append menu to menubar and optionally, change title of menu item
Bool wxMenuBar::OnAppend (wxMenu * menu, char *title)
{
  Bool retval = TRUE, new_menu = FALSE;
  wxMenu *wasHack;

  if (!menu->window_parent) {
    menu->Enable(TRUE); /* Force the menu to be enabled (for consistency) */
    menu->window_parent = this;
    menu->menu_bar = this;
    new_menu = TRUE;
  } else if (menu->window_parent == this) {
    // Don't add, but maybe change the menu title
    retval = FALSE;
  } else
    return FALSE;
	
  if (retval) {
    wxMenu **new_menus;
    char **new_titles;
    int i;

    n++;
    new_menus = new WXGC_PTRS wxMenu *[n];
    new_titles = new WXGC_PTRS char *[n];
    
    for (i = 0; i < n - 1; i++) {
      new_menus[i] = menus[i];
      menus[i] = NULL;
      new_titles[i] = titles[i];
      titles[i] = NULL;
    }
    menus = new_menus;
    titles = new_titles;

    menus[n - 1] = menu;
    {
      char *tmps;
      tmps = copystring (title);
      titles[n - 1] = tmps;
    }

    menu->menu_bar = (wxMenuBar *) this;
  }  
  
  wasHack = wxHelpHackMenu;
  wxHelpHackMenu = NULL;

  if (title)
    menu->MacChangeMenuText(menu, title);

  if (new_menu && this == last_installed_bar) {
    menu->wxMacInsertSubmenu();
    if (wasHack || (menu == wxHelpHackMenu)) {
      Install(menu_bar_frame);
    } else {
      ::InsertMenu(menu->cMacMenu, 0);
      wxInvalMenuBar();
    }
  }

  return retval;
}

Bool wxMenuBar::OnDelete(wxMenu *menu, int pos)
{
  int j, was_last = (pos == n - 1);
  int was_hack;
  int new_hack;

  --n;
  for (j = pos; j < n; j++) {
    menus[j] = menus[j + 1];
    titles[j] = titles[j + 1];
  }
  
  menu->window_parent = NULL;
  menu->menu_bar = NULL;
  
  was_hack = (menu == wxHelpHackMenu);
  if (was_hack) {
    wxHelpHackMenu = NULL;
    iHelpMenuHackNum = 0;
  }
  new_hack = FALSE;
  if (was_last && n) {
    /* Check for new Help menu: */
    wxMenu *m = menus[n - 1];
    MenuHandle mnu;
    mnu = m->CreateCopy(titles[n - 1], TRUE, NULL);
    DisposeMenu(mnu);
    if (wxHelpHackMenu)
      new_hack = TRUE;
  }
  
  if (menu_bar_frame && menu_bar_frame->IsFrontWindow()) {
    if (was_hack || new_hack) {
      Install(menu_bar_frame);
    } else {
      ::DeleteMenu(menu->cMacMenuId);
      wxInvalMenuBar();
    }
  }

  return TRUE;
}

// Helper function - Add the Apple Menu 
void wxSetUpAppleMenu(wxMenuBar *mbar)
{
  MenuHandle appleMenuHandle;

  appleMenuHandle = GetMenuHandle(128);

  if (appleMenuHandle == NULL) {
    char t[2] = {1, appleMark};
    appleMenuHandle = ::NewMenu(128, (StringPtr)t);
    CheckMemOK(appleMenuHandle);
#ifndef WX_CARBON
    ::AppendResMenu(appleMenuHandle, 'DRVR');
#endif
  }
  if (mbar && mbar->wxHelpHackMenu && mbar->iHelpMenuHackNum) {
    Str255 t = "\pAbout\212";
    wxNode *n;
    n = mbar->wxHelpHackMenu->menuItems->Nth(mbar->iHelpMenuHackNum - 1);
    if (n) {
      wxMenuItem *i;
      i = (wxMenuItem *)n->Data();
      if (i) {
	char *s;
	s = i->GetLabel();
	CopyCStringToPascal(s,t);
      }
    }
		
    ::InsertMenuItem(appleMenuHandle, t, 0);
  } else {
    Str255 buffer;
    char *s;
    s = wxTheApp->GetDefaultAboutItemName();
    CopyCStringToPascal(s, buffer);
    ::InsertMenuItem(appleMenuHandle, buffer, 0);
  }
  ::InsertMenu(appleMenuHandle, 0);

  wxNumHelpItems = 0;

 {
   MenuRef mnu;
   MenuItemIndex idx;
   if (!::GetIndMenuItemWithCommandID (NULL, 'quit', 1, &mnu, &idx)) {
     ::SetMenuItemCommandKey(mnu, idx, FALSE, 'Q');
   }

   if (!::GetIndMenuItemWithCommandID (NULL, 'pref', 1, &mnu, &idx)) {
     if (wxCan_Do_Pref()) {
       ::SetMenuItemCommandKey(mnu, idx, FALSE, ',');
       ::EnableMenuItem(mnu, idx);
     } else {
       ::DisableMenuItem(mnu, idx);
     }
   }
 }
}

void wxMenuBar::Install(wxWindow *for_frame)
{
  int i;
  wxMenu* menu;

  wxPrepareMenuDraw();
  ::ClearMenuBar();
  wxSetUpAppleMenu(this);
  for (i = 0; i < n; i ++) {
    menu = menus[i];
    if (menu != wxHelpHackMenu) {
      MenuHandle mh;
      Bool v;
      mh = menu->MacMenu();
      ::InsertMenu(mh, 0);
      menu->wxMacInsertSubmenu();
      v = menu->IsEnable();
      if (!v) {
	::DisableMenuItem(mh, 0);
      } else {
	if (menu_bar_frame) {
          /* Don't use CanAcceptEvent(), because that depends on 
             mouse capture, which is too transient for updating menus. */
	  v = menu_bar_frame->IsEnable();
	} else
	  v = 1;
	if (!v) {
          ::DisableMenuItem(mh, 0);
	} else {
	  ::EnableMenuItem(mh, 0);
	}
      }
    }
  }

  if (for_frame
      && (for_frame->GetWindowStyleFlag() & wxHIDE_MENUBAR)) {
    HideMenuBar();
    mb_hidden = 1;
  } else if (mb_hidden) {
    mb_hidden = 0;
    ShowMenuBar();
  }

  wxInvalMenuBar();
  last_installed_bar = this;
}

///////////////////////////////////////////////////////////////////////////////
// Menus
///////////////////////////////////////////////////////////////////////////////

short wxMenu::gMenuIdCounter = 129; // mac platform (to give unique menuID's to mac menus)

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxMenu::Break(void) { } // Macintosh has just one line for the menu bar

void wxMenu::CheckHelpHack(void)
{
  if (menu_bar && this == menu_bar->wxHelpHackMenu)
    MacChangeMenuText(this, title);
}

//-----------------------------------------------------------------------------
void wxMenu::AppendSeparator(void)
{
  wxMenuItem* item;

  item = new WXGC_PTRS wxMenuItem(this);

  item->itemId = -1;
  item->itemName = copystring("-");
  menuItems->Append(item);
  no_items ++;

  // menu item string can't be empty
  if (item->itemName[0])
    AppendMenu(cMacMenu, wxC2P(item->itemName));
  else
    AppendMenu(cMacMenu, "\p ");
	
  CheckHelpHack();
}

//-----------------------------------------------------------------------------
// Ordinary menu item
//-----------------------------------------------------------------------------
void wxMenu::Append(int Id, char* Label, char* helpString, Bool checkable)
{
  //	assert(Id >= 1);
  wxMenuItem* item;
  Str255 menusetup;
  int spc = 0, modifiers, is_virt;

  item = new WXGC_PTRS wxMenuItem(this, checkable);

  item->itemId = Id;
  item->itemName = macCopyString(Label);
  item->helpString = macCopyString(helpString);
  menuItems->Append(item);
  no_items ++;

  Label = wxBuildMacMenuString(menusetup, item->itemName, &spc, &modifiers, &is_virt);
  wxPrepareMenuDraw();
  ::AppendMenu(cMacMenu, menusetup);
  {
    CFStringRef ct;
    ct = wxCFString(Label);
    ::SetMenuItemTextWithCFString(cMacMenu, no_items, ct);
    CFRelease(ct);
  }
  if (spc) {
    SetMenuItemCommandKey(cMacMenu, no_items, is_virt, spc);
    SetMenuItemModifiers(cMacMenu, no_items, modifiers);
  }
  wxDoneMenuDraw();
  CheckHelpHack();
}

//-----------------------------------------------------------------------------
// Pullright item
//-----------------------------------------------------------------------------
void wxMenu::Append(int Id, char* Label, wxMenu* SubMenu, char* helpString)
{
  //	assert(Id >= 1);
  //	assert(SubMenu->window_parent == NULL); // WCH : error if submenu is a child of another

  wxMenuItem* item;
  wxMenu *ancestor;
  Str255 menusetup;

	// mflatt: If this menu is used, give up
  if (SubMenu->window_parent)
    return;

  SubMenu->window_parent = this;

  item = new WXGC_PTRS wxMenuItem(this);
  item->subMenu = SubMenu;

  item->itemId = Id;
  item->itemName = macCopyString(Label);
  item->helpString = macCopyString(helpString);

  menuItems->Append(item);
  no_items++;

  Label = wxBuildMacMenuString(menusetup, item->itemName, NULL, NULL, NULL);
	
  wxPrepareMenuDraw();
  ::AppendMenu(cMacMenu, menusetup);
  {
    CFStringRef ct;
    ct = wxCFString(Label);
    ::SetMenuItemTextWithCFString(cMacMenu, no_items, ct);
    CFRelease(ct);
  }
  ::SetMenuItemHierarchicalID(cMacMenu, no_items, SubMenu->cMacMenuId);
  wxDoneMenuDraw();

  ancestor = this;
  while (ancestor) {
    if (ancestor->menu_bar) {
      if (ancestor->menu_bar == last_installed_bar) {
	wxPrepareMenuDraw();
	InsertMenu(SubMenu->cMacMenu, -1);
	wxDoneMenuDraw();
	SubMenu->wxMacInsertSubmenu();
      }
      break;
    }
    ancestor = (wxMenu *)ancestor->window_parent;
  }

  CheckHelpHack();
}

// mflatt
Bool wxMenu::Delete(wxMenu *menu, int Id, int delpos)
{
  int pos;
  wxMenuItem *item;
  wxNode *node;
	
  if ((Id == -1) && (delpos == -1))
    return FALSE;
  
  for (pos = 0, node = menuItems->First(); node; node = node->Next(), pos++) {
    item = (wxMenuItem *)node->Data();
    if ((menu && item->subMenu == menu) 
	|| (!menu && (delpos == -1) && item->itemId == Id)
	|| (delpos == pos)) {
      if (item->subMenu)
	item->subMenu->window_parent = NULL;
      wxPrepareMenuDraw();
      ::DeleteMenuItem(cMacMenu, pos + 1);
      wxDoneMenuDraw();
      menuItems->DeleteNode(node);
      DELETE_OBJ item;
      --no_items;
      CheckHelpHack();
      return TRUE;
    }
  }

  return FALSE;
}

Bool wxMenu::DeleteByPosition(int pos)
{
  return Delete(NULL, -1, pos);
}

// mflatt
Bool wxMenu::Delete(int Id)
{
  return Delete((wxMenu *)NULL, Id, -1);
}

int wxMenu::Number()
{
  return no_items;
}

//-----------------------------------------------------------------------------
void wxMenu::Enable(int Id, Bool Flag)
{
  wxMenuItem* theMenuItem;

  theMenuItem = FindItemForId(Id);
  if (theMenuItem)
    theMenuItem->Enable(Flag);

  CheckHelpHack();
}

//-----------------------------------------------------------------------------
void wxMenu::Check(int Id, Bool Flag)
{
  wxMenuItem* theMenuItem;

  theMenuItem = FindItemForId(Id);
  if (theMenuItem)
    theMenuItem->Check(Flag);

  CheckHelpHack();
}

//-----------------------------------------------------------------------------
Bool wxMenu::Checked(int Id)
{
  wxMenuItem* theMenuItem;

  theMenuItem = FindItemForId(Id);
  if (theMenuItem)
    return theMenuItem->IsChecked();
  else return FALSE;
}

//-----------------------------------------------------------------------------
void wxMenu::SetTitle(char* label)
{
  if (menu_bar) {
    // WCH : must reset Macintosh menu label (not easy)
    // WCH : the duplicate title in menuBar object may now be out of sync
    MacChangeMenuText(this, label);
  } else {
    title = macCopyString1(label);
  }
}

//-----------------------------------------------------------------------------
char* wxMenu::GetTitle(void) { return title; }

//-----------------------------------------------------------------------------
char* wxMenu::GetLabel(int Id)
{
  wxMenuItem* theMenuItem;
  theMenuItem = FindItemForId(Id);
  if (theMenuItem)
    return theMenuItem->GetLabel();
  else return NULL;
}

//-----------------------------------------------------------------------------
void wxMenu::SetLabel(int Id , char* label)
{
  wxMenuItem* theMenuItem;

  theMenuItem = FindItemForId(Id);
  if (theMenuItem) {
    theMenuItem->SetLabel(label);
    /* We're not changing the title, so the following isn't necessary, right? */
    // if (menu_bar)
    //  MacChangeMenuText(this, title);
  }

  CheckHelpHack();
}

//-----------------------------------------------------------------------------
void wxMenu::SetSize(int x, int y, int width, int height) { }

//-----------------------------------------------------------------------------
void wxMenu::DoSetSize(int x, int y, int width, int height) { }

//-----------------------------------------------------------------------------
void wxMenu::Enable(Bool Flag)
{
  if (cEnable != Flag)
    {
      cEnable = Flag;
      if (menu_bar)
	{
	  wxFrame* frame = menu_bar->menu_bar_frame;
	  if (frame)
	    {
	      if (frame->IsFrontWindow())
		{
		  wxPrepareMenuDraw();
		  if (cEnable)
		    EnableMenuItem(cMacMenu, 0);
		  else
		    DisableMenuItem(cMacMenu, 0);
		  wxDoneMenuDraw();
		  wxInvalMenuBar();
		  CheckHelpHack();
		}
	    }
	}
    }
}

//-----------------------------------------------------------------------------
void wxMenu::Show(Bool show) { }

//-----------------------------------------------------------------------------
char* wxMenu::GetLabel(void)
{
  return title; // WCH: confusion with title and label; cf. GetTitle
}

//-----------------------------------------------------------------------------
void wxMenu::SetLabel(char* label) 
{
}

//-----------------------------------------------------------------------------
MenuHandle wxMenu::MacMenu(void) { return cMacMenu; }

//-----------------------------------------------------------------------------
short wxMenu::GetMacMenuId(void) { return cMacMenuId; }

//-----------------------------------------------------------------------------
wxMenu* wxMenu::wxMacFindSubmenu(int macMenuId)
{
  wxMenu* result = NULL;
  wxNode* node;
  wxMenuItem* menuItem;
  wxMenu* submenu;

  node = menuItems->First();
  while (node && !result)
    {
      menuItem = (wxMenuItem*)node->Data();
      submenu = menuItem->subMenu;
      if (submenu)
	{
	  if (submenu->cMacMenuId == macMenuId)
	    result = submenu;
	  else
	    {
	      result = submenu->wxMacFindSubmenu(macMenuId);
	    }
	}

      if (!result) node = node->Next();
    }

  return result;
}

//-----------------------------------------------------------------------------
void wxMenu::wxMacInsertSubmenu(void)
{
  wxNode* node;
  wxMenuItem* menuItem;
      wxMenu* submenu;
  
  node = menuItems->First();
  while (node)
    {
      menuItem = (wxMenuItem*)node->Data();
      submenu = menuItem->subMenu;
      if (submenu) {
	wxPrepareMenuDraw();
	InsertMenu(submenu->cMacMenu, -1);
	wxDoneMenuDraw();
	submenu->wxMacInsertSubmenu();
      }
      node = node->Next();
    }
}

//-----------------------------------------------------------------------------
void wxMenu::SetWidth(int w)
{
  requestedWidth = w;
}
