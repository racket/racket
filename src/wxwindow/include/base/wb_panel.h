/*
 * File:	wb_panel.h
 * Purpose:	wxPanel subwindow, for panel items (widgets/controls)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wxb_panelh
#define wxb_panelh

#include "common.h"
#include "wx_win.h"
#include "wx_frame.h"
#include "wx_canvs.h"

#define wxKEY_SHIFT     1
#define wxKEY_CTRL      2

class wxItem;
class wxButton;
class wxPanel;
class wxColour;
class wxBrush;
class wxPanelDC;
class wxStaticItem;

class wxbPanel: public wxCanvas
{
 public:
  Bool new_line;
  int label_position;
  wxButton *defaultItem;

  int hSpacing;
  int vSpacing;

  int current_hspacing;
  int current_vspacing;

  int initial_hspacing;
  int initial_vspacing;
  Bool has_child;

  wxbPanel(void);
  wxbPanel(wxWindow *window,
	   int x=-1, int y=-1, int width=-1, int height=-1, long style=0,
	   char *name = "panel");
  ~wxbPanel(void);

  // Set current label position, i.e. will label be on top or to the left
  virtual void SetLabelPosition(int pos);  // wxHORIZONTAL or wxVERTICAL
  int GetLabelPosition(void);

  wxObject *GetChild(int number);

  void OnEvent(wxMouseEvent *event);
};

#endif // wxb_panelh
