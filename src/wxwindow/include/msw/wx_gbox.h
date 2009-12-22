/*
 * File:	wx_gbox.h
 * Purpose:	Group box item
 * Author:	Matthew
 * Created:	2003
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 2003, PLT
 */

#ifndef wx_gboxh
#define wx_gboxh

#include "wx_item.h"

class wxGroupBox : public wxItem
{
 public:
  wxGroupBox(wxPanel *panel, char *label, long style, wxFont *_font = NULL);
  ~wxGroupBox();

  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
};

#endif // wx_gboxh
