/*
 * File:	wx_menu.cc
 * Purpose:	Menu implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	April 1995
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

wxNonlockingHashTable *wxMenuItemIDs = NULL;

wxMenuItem::wxMenuItem(void)
{
}

wxMenuItem::~wxMenuItem(void)
{
  if (menuId) {
    wxMenuItemIDs->Delete(menuId);
    menuId = 0;
  }
}

// Menus

// Construct a menu with optional title (then use append)

wxMenu::wxMenu(char *Title, wxFunction func, wxFont *_font):wxbMenu(Title, func)
{
  HANDLE msh;

  SetFont(_font);
  
  mustBeBreaked = FALSE;
  no_items = 0;
  menu_bar = NULL;
  wxWinType = wxTYPE_HMENU;
  msh = (HANDLE)wxwmCreatePopupMenu();
  ms_handle = msh;
  if (_font)
    wxSetWinFont(font, ms_handle);
  save_ms_handle = NULL;
  top_level_menu = this;
  if (title)
  {
    Append(-2,title);
    AppendSeparator();
  }

  Callback(func);
}

// The wxWindow destructor will take care of deleting the submenus.
wxMenu::~wxMenu(void)
{
  wxNode *node;
  wxNode *next;

  if (ms_handle)
    wxwmDestroyMenu((HMENU)ms_handle);
  ms_handle = NULL;

  node = menuItems->First();
  while (node) {
    wxMenuItem *item;
    item = (wxMenuItem *)node->Data();
    item->menuBar = NULL;
    
    // Delete child menus.
    // Beware: they must not be appended to children list!!!
    // (because order of delete is significant)
    if (item->subMenu)
      delete item->subMenu;
    item->subMenu = NULL;
    
    next = node->Next();
    delete item;
    node = next;
  }
}

void wxMenu::Break(void)
{
  mustBeBreaked = TRUE;
}

// Ordinary menu item
void wxMenu::Append(long Id, char *Label, char *helpString, Bool checkable)
{
  // 'checkable' parameter is useless for Windows.
  wxMenuItem *item;
  WORD menuId;
  int ms_flags;

  item = new wxMenuItem;
  item->checkable = checkable;
  item->itemId = Id;
  item->itemName = copystring(Label);
  item->subMenu = NULL;
  if (helpString) {
    item->helpString = copystring(helpString);
  }

  if (!wxMenuItemIDs) {
    wxREGGLOB(wxMenuItemIDs);
    wxMenuItemIDs = new wxNonlockingHashTable;
  }

  do {
    menuId = (WORD)rand();
  } while (wxMenuItemIDs->Get((long)menuId));

  wxMenuItemIDs->Put(menuId, this);

  item->menuId = menuId;

  menuItems->Append(item);

  ms_flags = mustBeBreaked? MF_MENUBREAK : 0;
  mustBeBreaked = FALSE;

  if (ms_handle)
    AppendMenuW((HMENU)ms_handle, MF_STRING|ms_flags, menuId, wxWIDE_STRING(Label));
  else if (save_ms_handle)
    AppendMenuW((HMENU)save_ms_handle, MF_STRING|ms_flags, menuId, wxWIDE_STRING(Label));

  if (Id == -2) {
    int ms_flag = MF_DISABLED;
    if (ms_handle)
      EnableMenuItem((HMENU)ms_handle, no_items, MF_BYPOSITION | ms_flag);
    else if (save_ms_handle) // For Dynamic Menu Append, Thx!!
      EnableMenuItem((HMENU)save_ms_handle, no_items, MF_BYPOSITION | ms_flag);
  }

  no_items++;
}

void wxMenu::AppendSeparator(void)
{
  int ms_flags;
  wxMenuItem *item;
  
  ms_flags = mustBeBreaked? MF_MENUBREAK : 0;

  mustBeBreaked = FALSE;

  if (ms_handle)
    AppendMenu((HMENU)ms_handle, MF_SEPARATOR|ms_flags, NULL, NULL);
  else if (save_ms_handle) // For Dynamic Manu Append, Thx!
    AppendMenu((HMENU)save_ms_handle, MF_SEPARATOR|ms_flags, NULL, NULL);

  item = new wxMenuItem;
  item->checkable = FALSE;
  item->itemId = -1;
  menuItems->Append(item);
  no_items++;
}

// Pullright item
void wxMenu::Append(long Id, char *Label, wxMenu *SubMenu, char *helpString)
{
  wxMenuItem *item;
  int ms_flags;
  HMENU menu, child;

  if (!SubMenu->ms_handle)
    return;

  SubMenu->top_level_menu = top_level_menu;

  item = new wxMenuItem;
  item->checkable = FALSE;
  item->itemId = Id;
  item->itemName = copystring(Label);
  if (helpString) {
    item->helpString = copystring(helpString);
  }
  item->subMenu = SubMenu;

  menuItems->Append(item);

  ms_flags = mustBeBreaked? MF_MENUBREAK : 0;
  mustBeBreaked = FALSE;

  menu = (HMENU)(ms_handle ? ms_handle : save_ms_handle);
  child = (HMENU)SubMenu->ms_handle;
  SubMenu->save_ms_handle = (HANDLE)child;
  SubMenu->ms_handle = NULL;
  AppendMenuW(menu, MF_POPUP | MF_STRING | ms_flags, (UINT)child, wxWIDE_STRING(Label));

  no_items++;
}

Bool wxMenu::DeleteItem(long Id, int Pos)
{
  wxNode *node;
  wxMenuItem *item;
  int pos;
  HMENU menu;

  for (pos = 0, node = menuItems->First(); node && Pos--; node = node->Next(), pos++) {
    item = (wxMenuItem *)node->Data();
    if ((Pos < 0) && (item->itemId == Id))
      break;
  }

  if (!node)
    return FALSE;
  
  item = (wxMenuItem *)node->Data();

  menu = (HMENU)(ms_handle ? ms_handle : save_ms_handle);

  if (item->subMenu) {
    RemoveMenu(menu, (UINT)pos, MF_BYPOSITION);
    item->subMenu->ms_handle = item->subMenu->save_ms_handle;
    item->subMenu->save_ms_handle = NULL;
    item->subMenu->top_level_menu = NULL;
  } else
    DeleteMenu(menu, (UINT)pos, MF_BYPOSITION);
  
  menuItems->DeleteNode(node);
  delete item;

  --no_items;

  return TRUE;
}

Bool wxMenu::Delete(long Id)
{
  return DeleteItem(Id, -1);
}

Bool wxMenu::DeleteByPosition(int pos)
{
  return DeleteItem(0, pos);
}

int wxMenu::Number()
{
  return no_items;
}

void wxMenu::SelectMenu(void)
{
  wxMenuBar *mb = menu_bar;
  if (mb) {
    if (mb->menu_bar_frame) {
      int i;
      for (i = 0; i < mb->n; i++) {
	if (mb->menus[i] == this) {
	  int key = 0;
	  GC_CAN_IGNORE char *s = mb->titles[i];
	  while (*s) {
	    if (*s == '&') {
	      key = s[1];
	      break;
	    }
	    s++;
	  }
	  if (key) {
	    wxWnd *wnd = (wxWnd *)mb->menu_bar_frame->handle;
	    if ((key >= 'A') && (key <= 'Z'))
	      key += 32;
	    if (wnd) {
	      wnd->DefWindowProc(WM_SYSKEYDOWN, key, 1 << 29);
	      wnd->DefWindowProc(WM_SYSCHAR, key, 1 << 29);
	    }
	  }
	}
      }
    }
    wxUnhideCursor();
  }
}

void wxMenu::Enable(long Id, Bool Flag)
{
  int ms_flag;
  int pos;
  wxMenuItem *item;
  HMENU mh;

  if (Flag)
    ms_flag = MF_ENABLED;
  else
    ms_flag = MF_GRAYED;
  
  item = FindItemForId(Id, NULL, &pos);
  if (item == NULL)
    return;

  mh = ms_handle ? (HMENU)ms_handle : (HMENU)save_ms_handle;
  EnableMenuItem(mh, pos, MF_BYPOSITION | ms_flag);
}

void wxMenu::Check(long Id, Bool Flag)
{
  int pos;
  int ms_flag;
  wxMenuItem *item;
  HMENU mh;

  item = FindItemForId(Id, NULL, &pos);
  if (!item || !item->checkable)
    return;
  mh = ms_handle ? (HMENU)ms_handle : (HMENU)save_ms_handle;

  if (Flag)
    ms_flag = MF_CHECKED;
  else
    ms_flag = MF_UNCHECKED;

  if (mh)
    CheckMenuItem(mh, pos, MF_BYPOSITION | ms_flag);
}

Bool wxMenu::Checked(long Id)
{
  int Flag = -1;
  HMENU mh;
  int pos;
  wxMenuItem *item;

  mh = ms_handle ? (HMENU)ms_handle : (HMENU)save_ms_handle;

  item = FindItemForId(Id, NULL, &pos);

  if (mh && item)
    Flag = GetMenuState(mh, pos, MF_BYPOSITION);

  if (Flag == -1)
    return FALSE;
  
  if (Flag & MF_CHECKED)
    return TRUE;
  else
    return FALSE;
}

void wxMenu::SetTitle(char *label)
{
  HMENU mh;

  if (label) {
    title = copystring(label);
  } else {
    title = copystring(" ");
  }

  mh = ms_handle ? (HMENU)ms_handle : (HMENU)save_ms_handle;
  if (mh)
    ModifyMenuW(mh, 0,
		MF_BYPOSITION | MF_STRING | MF_DISABLED,
		-2, wxWIDE_STRING(title));
}

char *wxMenu::GetTitle()
{
  return(title);
}

void wxMenu::SetLabel(long Id,char *label)
{
  int pos;
  HMENU mh;
  wxMenuItem *item;

  item = FindItemForId(Id, NULL, &pos);
  if (item==NULL)
    return;

  item->itemName = copystring(label);

  mh = ms_handle ? (HMENU)ms_handle : (HMENU)save_ms_handle;
  if (mh) {
    if (!item->subMenu) {
      UINT was_flag;
      was_flag = GetMenuState(mh, pos, MF_BYPOSITION);
      ModifyMenuW(mh, pos, MF_BYPOSITION|MF_STRING|was_flag, 
		 item->menuId, wxWIDE_STRING(label));
    } else {
      ModifyMenuW(mh, pos, MF_BYPOSITION|MF_STRING|MF_POPUP,
		  (UINT)item->subMenu->save_ms_handle, wxWIDE_STRING(label));
    }
  }
}

char *wxMenu::GetLabel(long Id)
{
  static wchar_t tmp[128];
  int len, pos;
  wxMenuItem *item;
  HMENU mh;

  item = FindItemForId(Id, NULL, &pos);
  if (!item)
    return NULL;

  mh = ms_handle ? (HMENU)ms_handle : (HMENU)save_ms_handle;

  if (mh)
    len = GetMenuStringW(mh,pos,tmp,127,MF_BYPOSITION);
  else
    len = 0;

  tmp[len] = '\0';
  return copystring(wxNARROW_STRING(tmp));
}

BOOL wxMenu::MSWCommand(UINT WXUNUSED(param), WORD menuId)
{
  wxMenuItem *item;

  item = FindItemForMenuId(menuId);

  if (item) {
    wxPopupEvent *event;

    if (item->checkable)
      Check(item->itemId, !Checked(item->itemId));
    
    event = new wxPopupEvent();
    event->menuId = item->itemId;
    ProcessCommand(event);
    return TRUE;
  } else
    return FALSE;
}

extern void wxResetCurrentCursor(void);
extern HCURSOR wxMSWSetCursor(HCURSOR c);

Bool wxWindow::PopupMenu(wxMenu *menu, double x, double y)
{
  int r;
  HWND hWnd;
  POINT point;
  HMENU hMenu = (HMENU)menu->ms_handle;
  UINT dropMenuId;
  HBITMAP bm = NULL;

  hWnd = GetHWND();

  if (!hMenu) return FALSE;
  point.x = (int)x;
  point.y = (int)y;
  ::ClientToScreen(hWnd, &point);

  if (menu->requestedWidth) {
    /* Really ugly hack: to force the menu to a particular
       width, create a disabled bitmap item at the end of
       the menu. Since themenu will have some extra space
       left and right of each item, subtract 3 times
       the width of the checkmark, on the grounds that it's
       probably a good approximation. */
    long w;
    long sz;
    char *ones;

    w = menu->requestedWidth - 3 * (GetMenuCheckMarkDimensions() & 0xFFFF);
    sz = (w / 8) + 1;
    
    do {
      dropMenuId = (WORD)rand();
    } while (!dropMenuId || wxMenuItemIDs->Get((long)dropMenuId));

    ones = new WXGC_ATOMIC char[sz];
    memset(ones, 255, sz);
    bm = CreateBitmap(w, 1, 1, 1, ones);

    AppendMenuW(hMenu, MF_BITMAP | MF_GRAYED, dropMenuId, (LPCWSTR)bm);
  }

  wxMSWSetCursor(wxSTANDARD_CURSOR->ms_cursor);
  r = wxwmTrackPopupMenu(hMenu, 
			 TPM_LEFTBUTTON | TPM_RIGHTBUTTON | TPM_NONOTIFY | TPM_RETURNCMD, 
			 point.x, point.y,
			 0, hWnd, NULL);
  wxResetCurrentCursor();

  if (bm) {
    DeleteMenu(hMenu, dropMenuId, MF_BYCOMMAND);
    DeleteObject(bm);
  }

  if (r){
    menu->MSWCommand(0, r);
  } else {
    wxPopupEvent *event;
    event = new wxPopupEvent();
    event->menuId = 0;
    menu->ProcessCommand(event);
  }
  return TRUE;
}

wxMenuItem *wxMenu::FindItemForMenuId(WORD menuId)
{
  wxNode *node;
  for (node = menuItems->First(); node; node = node->Next()) {
    wxMenuItem *item;
    item = (wxMenuItem *)node->Data();
    
    if (item->menuId == menuId)
      return item;
    
    if (item->subMenu) {
      wxMenuItem *ans;
      wxMenu *menu;
      menu = item->subMenu;
      ans = menu->FindItemForMenuId(menuId);
      if (ans)
	return ans;
    }
  }
  
  return NULL;
}

void wxMenu::SetWidth(int n)
{
  requestedWidth = n;
}

// Menu Bar

wxMenuBar::wxMenuBar(void)
{
  wxWinType = wxTYPE_HMENU;

  n = 0;
  menus = NULL;
  titles = NULL;
  menu_bar_frame = NULL;
}

wxMenuBar::wxMenuBar(int N, wxMenu *Menus[], char *Titles[]):wxbMenuBar(N, Menus, Titles)
{
  wxWinType = wxTYPE_HMENU;
}

wxMenuBar::~wxMenuBar(void)
{
  int i;
  wxMenu *mnu;

  for (i = 0; i < n; i++) {
    mnu = menus[i];
    delete mnu;
  }
}

void wxMenuBar::Append(wxMenu *menu, char *title)
{
  wxbMenuBar::Append(menu, title);
}

Bool wxMenuBar::OnDelete(wxMenu *a_menu, int pos)
{
  wxMenu *mnu;

  if (!menu_bar_frame)
    return TRUE;
  
  if (RemoveMenu((HMENU)ms_handle, (UINT)pos, MF_BYPOSITION)) {
    mnu = menus[pos];
    mnu->ms_handle = mnu->save_ms_handle;
    mnu->save_ms_handle = NULL;
    
    if (menu_bar_frame)
      menu_bar_frame->DrawMenuBar();
    
    return TRUE;
  }
  
  return FALSE;
}

Bool wxMenuBar::OnAppend(wxMenu *a_menu, char *title)
{
  if (!a_menu->ms_handle)
    return FALSE;

  if (!menu_bar_frame)
    return TRUE;

  a_menu->save_ms_handle = a_menu->ms_handle;
  a_menu->ms_handle = NULL;

  InsertMenuW((HMENU)ms_handle,
	      n,
	      MF_BYPOSITION | MF_POPUP | MF_STRING, 
	      (UINT)a_menu->save_ms_handle,
	      wxWIDE_STRING(title));

  menu_bar_frame->DrawMenuBar();

  return TRUE;
}

void wxMenuBar::Enable(long Id, Bool Flag)
{
  wxMenu *itemMenu = NULL;
  wxMenuItem *item;
  item = FindItemForId(Id, &itemMenu);
  if (!item)
    return;

  itemMenu->Enable(Id, Flag);
}

void wxMenuBar::EnableTop(int pos,Bool flag)
{
  int ms_flag;

  if (!menu_bar_frame)
    return;

  if (flag)
    ms_flag = MF_ENABLED;
  else
    ms_flag = MF_GRAYED;
  
  EnableMenuItem((HMENU)ms_handle, pos, MF_BYPOSITION | ms_flag);
  menu_bar_frame->DrawMenuBar();
}

void wxMenuBar::Check(long Id, Bool Flag)
{
  wxMenu *itemMenu = NULL;
  wxMenuItem *item;
  item = FindItemForId(Id, &itemMenu);
  if (!item)
   return;

  itemMenu->Check(Id, Flag);
}

Bool wxMenuBar::Checked(long Id)
{
  wxMenu *itemMenu = NULL;
  wxMenuItem *item;
  item = FindItemForId(Id, &itemMenu);
  if (!item)
    return FALSE;

  return itemMenu->Checked(Id);
}

void wxMenuBar::SetLabel(long Id,char *label)
{
  wxMenu *itemMenu = NULL;
  wxMenuItem *item;

  item = FindItemForId(Id, &itemMenu);

  if (!item)
    return;

  itemMenu->SetLabel(Id, label);
}

char *wxMenuBar::GetLabel(long Id)
{
  wxMenu *itemMenu = NULL;
  wxMenuItem *item;

  item = FindItemForId(Id, &itemMenu);

  if (!item)
    return NULL;

  return itemMenu->GetLabel(Id);
}

void wxMenuBar::SetLabelTop(int pos,char *label)
{
  UINT was_flag;
  char *s;

  if (pos < 0 || pos >= n)
    return;

  s = copystring(label);
  titles[pos] = s;
  
  was_flag = GetMenuState((HMENU)ms_handle,pos,MF_BYPOSITION);
  if (was_flag & MF_POPUP) {
    HMENU popup;
    was_flag &= 0xff;
    popup = GetSubMenu((HMENU)ms_handle,pos);
    ModifyMenuW((HMENU)ms_handle,pos,MF_BYPOSITION|MF_STRING|was_flag,(UINT)popup,wxWIDE_STRING(label));
  } else
    ModifyMenuW((HMENU)ms_handle,pos,MF_BYPOSITION|MF_STRING|was_flag,pos,wxWIDE_STRING(label));

  if (menu_bar_frame) {
    menu_bar_frame->DrawMenuBar();
  }
}

char *wxMenuBar::GetLabelTop(int pos)
{
  static char tmp[256];
  int len;

  len = GetMenuString((HMENU)ms_handle,pos,tmp,255,MF_BYPOSITION);
  
  if (!len)
    return 0L;
  tmp[len] = '\0';
  return copystring(tmp);
}

void wxFrame::SetMenuBar(wxMenuBar *menu_bar)
{
  int i;
  wxWnd *cframe;
  HMENU menu;
  wxMenu *mnu;

  menu = wxwmCreateMenu();

  if (menu_bar->menu_bar_frame)
    return;

  for (i = 0; i < menu_bar->n; i++) {
    HMENU popup;
    mnu = menu_bar->menus[i];
    popup = (HMENU)mnu->ms_handle;
    mnu->save_ms_handle = (HANDLE)popup;
    mnu->ms_handle = NULL;
    AppendMenu(menu, MF_POPUP | MF_STRING, (UINT)popup, menu_bar->titles[i]);
  }

  menu_bar->ms_handle = (HANDLE)menu;
  if (wx_menu_bar)
    delete wx_menu_bar;

  cframe = (wxWnd *)handle;
  cframe->hMenu = menu;

  switch (frame_type) {
  case wxMDI_PARENT:
    {
      wxMDIFrame *mdi_frame = (wxMDIFrame *)cframe;

      if (mdi_frame->parent_frame_active) {
	SendMessage(mdi_frame->client_hwnd, WM_MDISETMENU,
		    (WPARAM)menu,
		    (LPARAM)NULL);
	
	::DrawMenuBar(mdi_frame->handle);
      }
      break;
    }
  case wxMDI_CHILD:
    {
      wxMDIFrame *parent;
      wxWindow *par;
      par = GetParent();
      parent = (wxMDIFrame *)par->handle;
      
      if (((wxMDIChild *)cframe)->active) {
	parent->parent_frame_active = FALSE;

	SendMessage(parent->client_hwnd, WM_MDISETMENU,
		    (WPARAM)menu,
		    (LPARAM)NULL);
	
	::DrawMenuBar(parent->handle);
	break;
      }
    }
  default:
  case wxSDI:
    {
      SetMenu(cframe->handle, menu);
      break;
    }
  }

  wx_menu_bar = menu_bar;
  menu_bar->menu_bar_frame = this;
}

wxMenuItem *wxMenuBar::FindItemForMenuId(WORD menuId)
{
  int i;
  wxMenuItem *item;
  wxMenu *menu;
  for (i = 0; i < n; i++) {
    menu = menus[i];
    item = menu->FindItemForMenuId(menuId);
    if (item)
      return item;
  }
  return NULL;
}
