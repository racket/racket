/*
 * File:	wx_canvs.h
 * Purpose:	wxCanvas subwindow declarations
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_canvsh
#define wx_canvsh

#include "wb_canvs.h"

class wxGLConfig;

// Canvas subwindow for drawing on
class wxCanvas: public wxbCanvas
{
 public:
  wxWindow *combo;

  wxCanvas(void);
  wxCanvas(wxWindow *parent, int x=-1, int y=-1, int width=-1, int height=-1,
           long style = 0, char *name = "canvas", wxGLConfig *cfg = NULL);
  ~wxCanvas(void);

  Bool Create(wxWindow *parent, int x=-1, int y=-1, int width=-1, int height=-1,
           long style = 0, char *name = "canvas", wxGLConfig *cfg = NULL);
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);

  // Number of pixels per user unit (0 or -1 for no scrollbar)
  // Length of virtual canvas in user units
  // Length of page in user units
  void SetScrollbars(int horizontal, int vertical,
                             int x_length, int y_length,
                             int x_page, int y_page,
                             int x_pos = 0, int y_pos = 0, Bool setVirtualSize = TRUE);

  // Scroll the canvas
  void Scroll(int x_pos, int y_pos);
  void ScrollPercent(double x_pos, double y_pos);
  void GetScrollUnitsPerPage(int *x_page, int *y_page);

  void OnCalcScroll();

  void ViewStart(int *x, int *y, Bool sb = FALSE);

  // Actual size in pixels when scrolling is taken into account
  void GetVirtualSize(int *x, int *y);

  // Enable/disable Windows 3.1 scrolling in either direction.
  // If TRUE, wxWindows scrolls the canvas and only a bit of
  // the canvas is invalidated; no Clear() is necessary.
  // If FALSE, the whole canvas is invalidated and a Clear() is
  // necessary. Disable for when the scroll increment is used
  // to actually scroll a non-constant distance
  void EnableScrolling(Bool x_scrolling, Bool y_scrolling);

  virtual void WarpPointer(int x_pos, int y_pos) ;
  virtual wxWindow *FindFocusWindow();

  virtual void SetCanvasBackground(wxColor *c);
  wxColor *GetCanvasBackground();

  virtual Bool Show(Bool show);

  virtual void GetSize(int *width, int *height);

 private:
  wxColour *bgcol;
};

#endif // wx_canvsh
