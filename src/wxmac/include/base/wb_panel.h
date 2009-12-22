/*
 * File:	wb_panel.h
 * Purpose:	wxPanel subwindow, for panel items (widgets/controls)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wxb_panelh
#define wxb_panelh

#include "common.h"
#include "wx_win.h"
#include "wx_frame.h"
#include "wx_canvs.h"

#ifdef IN_CPROTO
typedef       void    *wxbPanel ;
#else

class wxItem;
class wxButton;
class wxPanel;
class wxColour;
class wxBrush;

class wxbPanel: public wxCanvas
{
 public:
  Bool new_line;
  int label_position;
  wxButton *defaultItem;

  int hSpacing;
  int vSpacing;

  int current_hspacing ;
  int current_vspacing ;

  int initial_hspacing ;
  int initial_vspacing ;
  Bool has_child ;

  wxFont *labelFont ;
  wxFont *buttonFont;
  wxColour *backColour ;
  wxColour *labelColour;
  wxColour *buttonColour;

  // Constructor (given parentArea)
  wxbPanel(char* windowName, wxArea* parentArea, int x, int y, int width, int height,
		long style);
  // Constructor (given parentWindow)
  wxbPanel(char* windowName, wxWindow* parentWindow, int x, int y, int width, int height,
		long style);

  ~wxbPanel(void);

  // Set current label position, i.e. will label be on top or to the left
  void SetLabelPosition(int pos);  // wxHORIZONTAL or wxVERTICAL
  int GetLabelPosition(void);

  virtual void SetBackgroundColour(wxColour *col);

  inline virtual wxFont  *GetLabelFont(void)        { return labelFont ; }
  inline virtual wxFont  *GetButtonFont(void)       { return buttonFont ; }
  inline virtual wxColour*GetBackgroundColour(void) { return backColour ; }
  inline virtual wxColour*GetLabelColour(void)      { return labelColour ; }
  inline virtual wxColour*GetButtonColour(void)     { return buttonColour ; }

  // Start a new line
  virtual void NewLine(void) = 0;
  virtual void NewLine(int pixels) = 0;

  // Tab specified number of pixels
  virtual void Tab(void) = 0;
  virtual void Tab(int pixels) = 0;

  virtual void GetCursor(int *x, int *y) = 0;

  // Set/get horizontal spacing
  virtual void SetHorizontalSpacing(int sp) = 0;
  virtual int GetHorizontalSpacing(void) = 0;

  // Set/get vertical spacing
  virtual void SetVerticalSpacing(int sp) = 0;
  virtual int GetVerticalSpacing(void) = 0;

  // Update next cursor position
  virtual void AdvanceCursor(wxWindow *item) = 0;

  inline virtual wxButton *GetDefaultItem(void) { return defaultItem; }

  wxObject *GetChild(int number) ;

  // Override to define new behaviour for default action (e.g. double clicking
  // on a listbox)
  virtual void OnDefaultAction(wxItem *initiatingItem);

// Private methods
private:
	void InitDefaults(void);
	void InitMoreDefaults(void); // Poor name for this method

};

#endif // IN_CPROTO
#endif // wxb_panelh
