/*
 * File:	wb_lbox.h
 * Purpose:	List box
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wxb_lboxh
#define wxb_lboxh

#include "common.h"
#include "wx_panel.h"
#include "wx_item.h"

// List box item
class wxbListBox: public wxItem
{
 public:

  int no_items;
  int selected;
  int *selections;
  int multiple;

  wxbListBox(wxPanel *panel, wxFunction func, char *Title, Bool Multiple = FALSE,
             int x = -1, int y = -1, int width = -1, int height = -1,
             int N = 0, char **Choices = NULL,
             long style = 0, char *name = "listBox");
  ~wxbListBox(void);

  virtual void Append(char *Item) = 0;
  virtual void Append(char *Item, char *Client_data)= 0;
  virtual void Set(int N, char *Choices[]) = 0;
  virtual int FindString(char *s) = 0;
  virtual void Clear(void) = 0;
  virtual void SetSelection(int N, Bool select = TRUE, Bool one_only = FALSE) = 0;
  virtual void SetOneSelection(int N) = 0;
  virtual char *GetClientData(int N) = 0;
  virtual void SetClientData(int N, char *Client_data) = 0;
  virtual void Deselect(int N) = 0;
  virtual int GetSelection(void) = 0;  // For single choice list item only
  virtual char *GetStringSelection(void);
  virtual Bool SetStringSelection(char *s);
  virtual void SetString(int N, char *s) = 0;
  virtual Bool Selected(int N) = 0;

  virtual int Number(void);
  virtual void Delete(int N) = 0;

  // For single or multiple choice list item
  virtual int GetSelections(int **list_selections) = 0;
  virtual char *GetString(int N) = 0;

  // Set the specified item at the first visible item
  // or scroll to max range.
  virtual void SetFirstItem(int N)=0 ;
  virtual void SetFirstItem(char *s) = 0;

  virtual void InsertItems(int nItems, char **Items, int pos) = 0;

  virtual int NumberOfVisibleItems(void) = 0;
};

#endif // wxb_lboxh
