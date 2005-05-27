/*
 * File:	wb_choic.h
 * Purpose:	Choice items
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wxb_choich
#define wxb_choich

#include "common.h"
#include "wx_panel.h"
#include "wx_item.h"

// Choice item
class wxbChoice: public wxItem
{
 public:
  int no_strings;

  wxbChoice(wxPanel *panel, wxFunction func, char *Title,
           int x = -1, int y = -1, int width = -1, int height = -1,
           int N = 0, char **Choices = NULL,
           long style = 0, char *name = "choice");
  ~wxbChoice(void);

  virtual void Append(char *Item) = 0;
  virtual void Clear(void) = 0;
  virtual int GetSelection(void) = 0;
  virtual void SetSelection(int n) = 0;
  virtual int FindString(char *s) = 0;
  virtual char *GetStringSelection(void);
  virtual Bool SetStringSelection(char *s);
  virtual char *GetString(int n) = 0;
  inline int Number(void) { return no_strings; }
};

#endif // wxb_choich
