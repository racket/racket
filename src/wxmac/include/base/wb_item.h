/*
 * File:	wb_item.h
 * Purpose:	Declares panel items base class
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wxb_itemh
#define wxb_itemh

#include "common.h"
#include "wx_panel.h"
#include "wx_stdev.h"

#ifdef IN_CPROTO
typedef       void    *wxbItem ;
#else

class wxFont ;

// General item class
class wxbItem: public wxWindow
{
 public:

    wxbItem(void);

    // Constructor (given parentArea)
    wxbItem(char* windowName, wxArea* parentArea, int x, int y, int width, int height,
	    long style);
    // Constructor (given parentWindow)
    wxbItem(char* windowName, wxWindow* parentWindow, int x, int y, int width, int height,
	    long style);
    // Constructor (given objectType; i.e., menu or menuBar)
    wxbItem(char* windowName);


   ~wxbItem(void);

   int labelPosition;
   virtual void SetLabel(char *label) = 0;
   virtual char *GetLabel(void) = 0;

   virtual void Command(wxCommandEvent *event);        // Simulates an event
   virtual void ProcessCommand(wxCommandEvent *event); // Calls the callback and 

  // Places item in centre of panel - so can't be used BEFORE panel->Fit()
   void Centre(int direction = wxHORIZONTAL, wxWindow *p = NULL);
};

#endif // IN_CPROTO
#endif // wxb_itemh
