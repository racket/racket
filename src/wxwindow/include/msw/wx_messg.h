/*
 * File:	wx_messg.h
 * Purpose:	Message panel item
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_messgh
#define wx_messgh

#include "wb_messg.h"

// Message item
class wxMessage: public wxbMessage
{
 public:
  wxMessage(wxPanel *panel, char *message, int x = -1, int y = -1,
            long style = 0, wxFont *_font = NULL, char *name = "message");
  wxMessage(wxPanel *panel, wxBitmap *image, int x = -1, int y = -1,
            long style = 0, wxFont *_font = NULL, char *name = "message");
  wxMessage(wxPanel *panel, int iconID, int x = -1, int y = -1,
            long style = 0, wxFont *_font = NULL, char *name = "message");
  ~wxMessage(void);

  Bool Create(wxPanel *panel, char *message, wxBitmap *image, int iconID, int x, int y, long style);
  void SetLabel(wxBitmap *bitmap);
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void SetLabel(char *label);

  wxBitmap *bm_label;
  Bool is_icon;
};

#define wxMSGICON_WARNING 1
#define wxMSGICON_ERROR   2
#define wxMSGICON_APP     3

#endif // wx_messgh
