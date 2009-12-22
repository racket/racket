/*
 * File:	wx_menu.h
 * Purpose:	Declares panel items (controls/widgets)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_menuh
#define wx_menuh

#include "common.h"
#include "wx_win.h"
#include "wx_panel.h"
#include "wb_menu.h"

class wxMenuBar;

// Menu
class wxMenu: public wxbMenu
{
 private:
  Bool mustBeBreaked;
 public:
  HANDLE save_ms_handle; // Used for Enable() on popup
  int requestedWidth;

  wxMenu(char *Title = NULL, wxFunction func = NULL, wxFont *_font = NULL);
  ~wxMenu(void);
  void AppendSeparator(void);
  void Append(long Id, char *Label, char *helpString=NULL, Bool checkable=FALSE);
  void Append(long Id, char *Label, wxMenu *SubMenu, char *helpString = NULL);
  void Enable(long Id, Bool Flag);
  void Check(long Id, Bool Flag);
  Bool Checked(long Id);
  void SetTitle(char *label);
  char *GetTitle(void);
  inline void SetLabel(char *label) { wxItem::SetLabel(label); };
  void SetLabel(long Id, char *label);
  inline char *GetLabel(void) {return wxItem::GetLabel(); };
  char *GetLabel(long Id);
  void Break(void);

  Bool DeleteItem(long, int);
  Bool Delete(long Id);
  Bool DeleteByPosition(int pos);

  int Number(void);

  void SelectMenu(void);

  wxMenuItem *FindItemForMenuId(WORD menuId);

  BOOL MSWCommand(UINT param, WORD id);
  
  void SetWidth(int n);
};

class wxFrame;
class wxMenuBar:public wxbMenuBar
{
 public:
  wxMenuBar(void);
  wxMenuBar(int n, wxMenu *menus[], char *Titles[]);
  ~wxMenuBar(void);

  void Append(wxMenu *menu, char *title);
  // Must only be used AFTER menu has been attached to frame,
  // otherwise use individual menus to enable/disable items
  void Enable(long Id, Bool Flag);
  void EnableTop(int pos, Bool Flag);
  void Check(long Id, Bool Flag);
  Bool Checked(long Id);
  inline void SetLabel(char *label) { wxItem::SetLabel(label); };
  void SetLabel(long Id,char *label);
  inline char *GetLabel(void) {return wxItem::GetLabel(); };
  char *GetLabel(long Id);
  void SetLabelTop(int pos, char *label);
  char *GetLabelTop(int pos);

  wxMenuItem *FindItemForMenuId(WORD menuId);

  virtual Bool OnAppend(wxMenu *menu, char *title);
  virtual Bool OnDelete(wxMenu *menu, int index);
};

#endif // wx_menuh
