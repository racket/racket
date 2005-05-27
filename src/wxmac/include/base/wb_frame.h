/*
 * File:	wb_frame.h
 * Purpose:	wxFrame declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_frame.h	1.2 5/9/94" */

#ifndef wxb_frameh
#define wxb_frameh

#include "common.h"
#include "wx_win.h"
#ifdef wx_mac
#include "wx_screen.h"
#endif
#define wxMAX_STATUS   5

#ifdef IN_CPROTO
typedef       void    *wxbFrame ;
#else

class wxMenuBar;
class wxPanel ;
class wxStatusWnd;
class wxFrame;
class wxToolBar;
class wxBitmap;
class wxbFrame: public wxWindow
{
 public:
  Bool modal_showing;
  wxMenuBar *wx_menu_bar;
  wxBitmap *icon;
  Bool status_line_exists;
  long frame_type;  // SDI, MDI parent/child
  int nb_status;

  wxWindow *frameToolBar ;
  
  void *context;

// Constructor (given parentScreen)
  wxbFrame (char* windowName, wxScreen* parentScreen,
	    int x, int y, int width, int height, long style);
  ~wxbFrame(void);


  // Override, e.g. to resize subwindows
  void OnSize(int x, int y);

  // The default thing is to set the focus for the first child window.
  // Override for your own behaviour.
  void OnActivate(Bool flag);

  // Default behaviour is to display a help string for the menu item.
  virtual void OnMenuSelect(int id);
  inline virtual void OnMenuCommand(long id) {};         // Called on frame menu command ( 16.2)

  // Set menu bar
  virtual void SetMenuBar(wxMenuBar *menu_bar) = 0;			//  from 16.2
  virtual wxMenuBar *GetMenuBar(void);

  // Set icon
  virtual void SetIcon(wxBitmap *icon, wxBitmap *mask = NULL, int kind = 0) = 0;

  // Create status line
  virtual void CreateStatusLine(int number = 1, char *name = "status_line") = 0;

  // Set status line text
  virtual void SetStatusText(char *text, int number = 0) = 0;
  virtual Bool StatusLineExists(void);

  void Centre(int direction = wxBOTH);

  // Iconize
  virtual void Iconize(Bool iconize) = 0;
  virtual Bool Iconized(void) = 0;
  // Windos 3.x maximize/restore
  virtual void Maximize(Bool maximize) = 0;

  virtual void LoadAccelerators(char *table) = 0;

  // Call this to simulate a menu command
  virtual void Command(int id);
  virtual void ProcessCommand(int id);

  // Toolbar (currently, for use by Windows MDI parent frames ONLY)
  virtual inline void SetToolBar(wxToolBar *toolbar) { frameToolBar = (wxWindow *)(toolbar); }
  virtual inline wxToolBar *GetToolBar(void) { return (wxToolBar *)frameToolBar; }
};

#endif // IN_CPROTO
#endif // wxb_frameh
