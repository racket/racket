/*
 * File:	wb_frame.h
 * Purpose:	wxFrame declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wxb_frameh
#define wxb_frameh

#include "common.h"
#include "wx_win.h"

#define wxMAX_STATUS   1

class wxMenuBar;
class wxPanel ;
class wxStatusWnd;
class wxFrame;

class wxbFrame: public wxWindow
{
 public:
  void *context;

  Bool modal_showing;
  wxMenuBar *wx_menu_bar;
  Bool status_line_exists;
  long frame_type;  // SDI, MDI parent/child
  int nb_status;

  wxbFrame(wxFrame *parent, char *title,
          int x=-1, int y=-1, int width=-1, int height=-1,
          long type = 0, char *name = "frame");

  ~wxbFrame(void);

  Bool Create(wxFrame *parent, char *title,
	      int x=-1, int y=-1, int width=-1, int height=-1,
	      long type = 0, char *name = "frame");

  // Override, e.g. to resize subwindows
  void OnSize(int x, int y);

  // The default thing is to set the focus for the first child window.
  // Override for your own behaviour.
  void OnActivate(Bool flag);

  // Default behaviour is to display a help string for the menu item.
  virtual void OnMenuSelect(long id);
  inline virtual void OnMenuCommand(long WXUNUSED(id)) {}; // Called on frame menu command
  virtual void OnMenuClick(void);

  // Set menu bar
  virtual void SetMenuBar(wxMenuBar *menu_bar) = 0;
  virtual wxMenuBar *GetMenuBar(void);

  // Set icon
  virtual void SetIcon(wxBitmap *icon, wxBitmap *bg = NULL, int kind = 0) = 0;

  // Create status line
  virtual void CreateStatusLine(int number = 1, char *name = "status_line") = 0;

  // Set status line text
  virtual void SetStatusText(char *text, int number = 0) = 0;
  virtual Bool StatusLineExists(void);

  void Centre(int direction = wxBOTH);

  // Iconize
  virtual void Iconize(Bool iconize) = 0;
  virtual Bool Iconized(void) = 0;
  virtual void Maximize(Bool maximize) = 0;

  // Call this to simulate a menu command
  virtual void Command(long id);
  virtual void ProcessCommand(long id);
};

#endif // wxb_frameh
