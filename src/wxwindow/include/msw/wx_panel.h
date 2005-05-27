/*
 * File:	wx_panel.h
 * Purpose:	wxPanel subwindow, for panel items (widgets/controls)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_panelh
#define wx_panelh

#include "common.h"
#include "wx_win.h"
#include "wb_panel.h"

#define PANEL_LEFT_MARGIN 4
#define PANEL_TOP_MARGIN  4
#define PANEL_HSPACING  8
#define PANEL_VSPACING  8

class wxItem;
class wxFrame;
class wxPanel: public wxbPanel
{
 public:
  // For panel item positioning.
  int cursor_x;
  int cursor_y;
  int max_width;
  int max_height;
  int max_line_height;
  wxWindow *last_created;

  wxBrush *backBrush;

  wxPanel(void);
  wxPanel(wxWindow *parent,
          int x=-1, int y=-1, int width=-1, int height=-1, long style=0,
          char *name = "panel");

  ~wxPanel(void);

  Bool Create(wxWindow *window,
          int x=-1, int y=-1, int width=-1, int height=-1, long style=0,
          char *name = "panel");

  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void Centre(int direction);
  void AddChild(wxObject *child);

  // Set current label position, i.e. will label be on top or to the left
  void SetLabelPosition(int pos);  // wxHORIZONTAL or wxVERTICAL

  // Start a new line
  void NewLine(void);
  void NewLine(int pixels);
  void RealNewLine(void);

  virtual void ChangeToGray(Bool gray);

  void GetCursor(int *x, int *y);
  void SetItemCursor(int x, int y);

  // Fits the panel around the items
  void Fit(void);

  // For panel item positioning.
  // Update next cursor position
  void AdvanceCursor(wxWindow *item);
  void RealAdvanceCursor(void);

  // If x or y are not specified (i.e. < 0), supply
  // values based on left to right, top to bottom layout.
  // Internal use only.
  void GetValidPosition(int *x, int *y);

  virtual void OnPaint(void);
};

#endif // wx_panelh
