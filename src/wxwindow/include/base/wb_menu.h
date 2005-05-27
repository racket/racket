/*
 * File:	wb_menu.h
 * Purpose:	Declares panel items (controls/widgets)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wxb_menuh
#define wxb_menuh

#include "common.h"
#include "wx_item.h"
#include "wx_mnuit.h"

class wxMenuBar;
class wxMenu;

// Menu
class wxbMenu: public wxItem
{
 public:
  int no_items;
  char *title;
  wxMenu *top_level_menu;
  wxMenuBar *menu_bar;
  wxList *menuItems;

  wxbMenu(char *Title = NULL, wxFunction func = NULL);
  ~wxbMenu(void);
  virtual void AppendSeparator(void) = 0;
  virtual void Append(long id, char *Label, char *helpString = NULL,Bool checkable=FALSE) = 0;
  virtual void Append(long id, char *Label, wxMenu *SubMenu, char *helpString = NULL) = 0;
  // Avoids compiler warning
  inline void Enable(Bool enable) { wxWindow::Enable(enable) ; }
  virtual void Enable(long id, Bool Flag) = 0;
  virtual void Check(long id, Bool Flag) = 0;
  virtual Bool Checked(long id) = 0;
  virtual void SetHelpString(long id, char *helpString);
  virtual char *GetHelpString(long id);

  // Find wxMenuItem for item ID, and return item's
  // menu too if itemMenu is non-NULL.
  wxMenuItem *FindItemForId(long itemId, wxMenu **itemMenu = NULL, int *pos = NULL);

  void ProcessCommand(wxCommandEvent *event);
};

// Menu Bar (a la Windows)
class wxFrame;
class wxbMenuBar: public wxItem
{
 public:
  int n;
  wxMenu **menus;
  char **titles;
  wxFrame *menu_bar_frame;

  wxbMenuBar(void);
  wxbMenuBar(int n, wxMenu *menus[], char *Titles[]);
  ~wxbMenuBar(void);

  virtual void Append(wxMenu *menu, char *title);
  // Avoids compiler warning
  inline void Enable(Bool enable) { wxWindow::Enable(enable) ; }

  // Must only be used AFTER menu has been attached to frame,
  // otherwise use individual menus to enable/disable items
  virtual void Enable(long id, Bool Flag) = 0;
  virtual void EnableTop(int pos, Bool Flag) = 0;
  virtual void Check(long id, Bool Flag) = 0;
  virtual Bool Checked(long id) = 0;
  virtual void SetHelpString(long id, char *helpString);
  virtual char *GetHelpString(long id);

  // Find wxMenuItem for item ID, and return item's
  // menu too if itemMenu is non-NULL.
  wxMenuItem *FindItemForId(long itemId, wxMenu **menuItem = NULL);

  int Number();

  /* MATTHEW: [6] */
  virtual Bool Delete(wxMenu *menu, int index = 0);
  virtual Bool OnAppend(wxMenu *menu, char *title) = 0;
  virtual Bool OnDelete(wxMenu *menu, int index) = 0;
};

#endif // wxb_menuh
