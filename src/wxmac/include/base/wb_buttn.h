/*
 * File:	wb_buttn.h
 * Purpose:	Buttons
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_buttn.h	1.2 5/9/94" */

#ifndef wxb_buttnh
#define wxb_buttnh

#include "common.h"
#include "wx_panel.h"
#include "wx_item.h"

#ifdef IN_CPROTO
typedef       void    *wxbButton ;
#else

// Pushbutton
class wxBitmap;
class wxbButton: public wxItem
{
 public:

  wxbButton(wxPanel *panel, wxFunction func, int x = -1, int y = -1,
           int width = -1, int height = -1, long style = 0, char *name = "button");
  ~wxbButton(void);

  virtual void SetDefault(Bool flag = TRUE) = 0;
  // Avoids compiler warning
  inline  void SetLabel(char *label) { wxItem::SetLabel(label) ; }
  virtual void SetLabel(wxBitmap *bitmap) = 0;

};

#endif // IN_CPROTO
#endif // wxb_buttnh
