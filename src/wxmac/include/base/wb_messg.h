/*
 * File:	wb_messg.h
 * Purpose:	Declares panel items (controls/widgets)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wxb_messgh
#define wxb_messgh

#include "common.h"
#include "wx_panel.h"
#include "wx_messg.h"
#include "wx_item.h"

#ifdef IN_CPROTO
typedef       void    *wxbMessage ;
#else

// Message item
class wxbMessage: public wxItem
{
 public:

  wxbMessage(void);

  wxbMessage(wxPanel *panel, char *message, int x = -1, int y = -1, long style = 0,
             char *name = "message");

  // Constructor (given parentArea)
  wxbMessage(wxArea* parentArea, int x, int y, int width, int height, long style,
		char*		windowName);
 // Constructor (given parentWindow)
  wxbMessage(wxWindow* parentWindow,int x, int y, int width, int height, long style,
		char* windowName);

  wxbMessage(wxPanel *panel, wxBitmap *image, int x = -1, int y = -1, long style = 0,
             char *name = "message");

  ~wxbMessage(void);
};

#endif // IN_CPROTO
#endif // wxb_messgh
