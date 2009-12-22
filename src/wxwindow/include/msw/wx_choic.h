/*
 * File:	wx_choic.h
 * Purpose:	Choice panel item
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_choich
#define wx_choich

#include "wb_choic.h"

// Choice item
class wxChoice: public wxbChoice
{
 public:
  HWND static_label;

  wxChoice(wxPanel *panel, wxFunction func, char *Title,
           int x = -1, int y = -1, int width = -1, int height = -1,
           int N = 0, char **Choices = NULL,
           long style = 0, wxFont *_font = NULL, char *name = "choice");
  ~wxChoice(void);

  Bool Create(wxPanel *panel, wxFunction func, char *Title,
           int x = -1, int y = -1, int width = -1, int height = -1,
           int N = 0, char **Choices = NULL,
           long style = 0, char *name = "choice");
  void Append(char *Item);
  void Clear(void);
  int GetSelection(void);
  void SetSelection(int n);
  int FindString(char *s);
  char *GetString(int n);
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void GetSize(int *x, int *y);
  void GetPosition(int *x, int *y);
  char *GetLabel(void);
  void SetLabel(char *label);

  BOOL MSWCommand(UINT param, WORD id);

  virtual void ChangeToGray(Bool gray);
  
  virtual Bool Show(Bool s);
};

class wxCombo : public wxChoice
{
 public:
  wxWindow *forCanvas;

  wxCombo(wxWindow *for_canvas,
	  wxPanel *panel, wxFunction func, char *Title,
	  int x = -1, int y = -1, int width = -1, int height = -1,
	  int N = 0, char **Choices = NULL,
	  long style = 0, char *name = "choice");
  ~wxCombo(void);

  virtual Bool PreOnEvent(wxWindow *, wxMouseEvent *);
  virtual Bool PreOnChar(wxWindow *, wxKeyEvent *);
  virtual wxWindow *PreWindow();
};

extern void wxSetComboBoxFont(wxFont *f);

#endif // wx_choich
