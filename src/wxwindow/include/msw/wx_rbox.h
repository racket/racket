/*
 * File:	wx_rbox.h
 * Purpose:	Radio box panel item
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_rboxh
#define wx_rboxh

#include "wb_rbox.h"

// List box item
class wxBitmap;
class wxRadioBox: public wxbRadioBox
{
 public:
  Bool selected;
  HWND *radioButtons;
  wxBitmap **bm_labels;
  int majorDim;
  long style;
  int *radioWidth;  // for bitmaps
  int *radioHeight;
  Bool *buttonEnabled;

  wxRadioBox(wxPanel *panel, wxFunction func, char *Title,
             int x = -1, int y = -1, int width = -1, int height = -1,
             int N = 0, char **Choices = NULL,
             int majorDim = 0, long style = wxHORIZONTAL, 
	     wxFont *_font = NULL, char *name = "radioBox");

  virtual void ChangeToGray(Bool gray);
  void SetButton(int which, int value);

  wxRadioBox(wxPanel *panel, wxFunction func, char *Title,
             int x, int y, int width, int height,
             int N, wxBitmap **Choices,
             int majorDim = 0, long style = wxHORIZONTAL, 
	     wxFont *_font = NULL, char *name = "radioBox");

  ~wxRadioBox(void);

  Bool Create(wxPanel *panel, wxFunction func, char *Title,
             int x = -1, int y = -1, int width =-1, int height = -1,
             int N = 0, char **Choices = NULL, wxBitmap **bmChoices = NULL,
             int majorDim = 0, long style = wxHORIZONTAL, char *name = "radioBox");
             
  BOOL MSWCommand(UINT param, WORD id);

  int FindString(char *s);
  void SetSelection(int N);
  int GetSelection(void);
  char *GetString(int N);
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void GetSize(int *x, int *y);
  void GetPosition(int *x, int *y);
  char *GetLabel(void);
  void SetLabel(char *label);
  void SetLabel(int item, char *label);
  void SetLabel(int item, wxBitmap *bitmap);
  char *GetLabel(int item);
  Bool Show(Bool show);
  void SetFocus(void);
  void Enable(int item, Bool enable);
  void Enable(Bool enable);
  void Show(int item, Bool show);
  void GetClientSize(int *width, int *height);

  int ButtonFocus(int i);
};

#endif // wx_rboxh
