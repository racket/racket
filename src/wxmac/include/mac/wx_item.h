///////////////////////////////////////////////////////////////////////////////
// File:	wx_item.h
// Purpose:	Declares panel items (controls/widgets) for Mac
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_itemh
#define wx_itemh

#include "wb_item.h"

#ifdef IN_CPROTO
typedef       void* wxItem ;
#else

class wxItem: public wxbItem
{
 public:
  wxItem(void);
  // Constructor (given parentArea)
  wxItem (wxArea*	parentArea, int x = -1, int y = -1, int width = -1, int height = -1,
	  long style = 0, char* windowName = "item");
  // Constructor (given parentWindow)
  wxItem (wxWindow*	parentWindow, int x = -1, int y = -1, int width = -1, int height = -1,
	  long		style = 0, char* windowName = "item");
  // Constructor (given objectType; i.e., menu or menuBar)
  wxItem (char* windowName);
  
  ~wxItem(void);

  void SetLabel(char *label) {};	// This should be overridden in all subclasses
  
  virtual void OnChar(wxKeyEvent *event); // mac platform only
  
  virtual void MaybeMoveControls();

  virtual void Activate(Bool gray);

  virtual void OnSetFocus();
  virtual void OnKillFocus();

  virtual Bool AcceptsExplicitFocus(void);

  void IgnoreKeyboardEvents();

  void SetFont(wxFont *_font, int defsize);

 protected:
  virtual void ChangeToGray(Bool gray);
    
  // Under OS X, an inset is necessary because the OS draws outside of the control rectangle.
  int padLeft;
  int padRight;
  int padTop;
  int padBottom;
       
};

char *wxItemStripLabel(char *label);
Bool wxAllControlsWantFocus();
void wxSetControlFont(ControlRef c, wxFont *font);
void wxGetBestControlRect(ControlRef c, Rect *r, SInt16 *offset, 
			  wxFont *font, int small_height, int mini_height, 
			  char *label, int width_pad);

#endif // IN_CPROTO
#endif // wx_itemh
