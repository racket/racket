/*
 * File:	wx_lbox.h
 * Purpose:	List box panel item
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_lboxh
#define wx_lboxh

#include "wb_lbox.h"

// List box item
class wxListBox: public wxbListBox
{
 private:
  HWND static_label;
  char **user_data;
  wxFont *label_font;
 public:
  wxListBox(wxPanel *panel, wxFunction func, char *Title,
             Bool Multiple = wxSINGLE|wxNEEDED_SB,
             int x = -1, int y = -1, int width = -1, int height = -1,
             int N = 0, char **Choices = NULL,
             long style = 0, wxFont *_font = NULL, wxFont *_label_font = NULL, char *name = "listBox");
  ~wxListBox(void);

  Bool Create(wxPanel *panel, wxFunction func, char *Title, Bool Multiple = FALSE,
             int x = -1, int y = -1, int width = -1, int height = -1,
             int N = 0, char **Choices = NULL,
             long style = 0, char *name = "listBox");
  BOOL MSWCommand(UINT param, WORD id);

  void Append(char *Item);
  void Append(char *Item, char *Client_data);
  void Set(int N, char *Choices[]);
  int FindString(char *s);
  void Clear(void);
  void SetSelection(int N, Bool select = TRUE, Bool one = FALSE);
  void SetOneSelection(int N);

  virtual void ChangeToGray(Bool gray);

  void Deselect(int N);

  // For single choice list item only
  int GetSelection(void);
  void Delete(int N);
  char *GetClientData(int N);
  void SetClientData(int N, char *Client_data);
  void SetString(int N, char *s);

  // For single or multiple choice list item
  int GetSelections(int **list_selections);
  Bool Selected(int N);
  char *GetString(int N);
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void GetSize(int *x, int *y);
  void GetPosition(int *x, int *y);
  char *GetLabel(void);
  void SetLabel(char *label);

  wxFont *GetLabelFont(void) { return label_font; }

  // Set the specified item at the first visible item
  // or scroll to max range.
  void SetFirstItem(int N);
  void SetFirstItem(char *s);
  int GetFirstItem();

  void InsertItems(int nItems, char **Items, int pos);

  // Windows-specific code to set the horizontal extent of
  // the listbox, if necessary. If s is non-NULL, it's
  // used to calculate the horizontal extent.
  // Otherwise, all strings are used.
  void SetHorizontalExtent(char *s = NULL);

  int NumberOfVisibleItems(void);

  virtual Bool Show(Bool s);
};

#endif // wx_lboxh
