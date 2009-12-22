/*
 * File:	wx_tabc.h
 * Purpose:	Tab group panel item
 * Author:	Matthew
 * Created:	2002
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 2002, PLT
 */

#ifndef wx_tabch
#define wx_tabch

#include "wx_item.h"

class wxTabChoice : public wxItem
{
 public:
  wxTabChoice(wxPanel *panel, wxFunction func, char *label, 
              int N, char **Choices, int style, wxFont *_font = NULL);
  ~wxTabChoice();

  int   GetSelection(void);
  int   Number(void);
  void  SetSelection(int n);

  void Append(char *s, int new_sel = -1);
  void Delete(int i);

  virtual void DoShow(Bool show);
  virtual void OnClientAreaDSize(int dW, int dH, int dX, int dY);

  virtual char *GetLabel();

  void SetLabel(int i, char *s);

  virtual void Refresh(void);

  int tab_count;
  char **tab_labels;

  ControlHandle pane;

  virtual void Activate(Bool gray);
  virtual void MaybeMoveControls();

  virtual void SetPhantomSize(int w, int h);

  void Set(int N, char **Choices);

  int ButtonFocus(int n);
  virtual void OnSetFocus(void);
  virtual void OnKillFocus(void);

  int focused_button;

protected:
  virtual void ChangeToGray(Bool gray);
  virtual void Paint(void);
  virtual void OnEvent(wxMouseEvent *event);

  int phantom_height;
};

#endif // wx_tabch
