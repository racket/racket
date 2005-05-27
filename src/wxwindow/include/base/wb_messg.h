/*
 * File:	wb_messg.h
 * Purpose:	Declares panel items (controls/widgets)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wxb_messgh
#define wxb_messgh

#include "common.h"
#include "wx_panel.h"
#include "wx_item.h"

// Message item
class wxbMessage: public wxItem
{
 public:

  wxbMessage(wxPanel *panel, char *message, int x = -1, int y = -1, long style = 0,
             char *name = "message");
  wxbMessage(wxPanel *panel, wxBitmap *image, int x = -1, int y = -1, long style = 0,
             char *name = "message");

  ~wxbMessage(void);
};

#endif // wxb_messgh
