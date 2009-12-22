/*
 * File:	wx_gbox.h
 * Purpose:	Tab group panel item
 * Author:	Matthew
 * Created:	2002
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 2002, PLT
 */

#ifndef wx_gboxh
#define wx_gboxh

#include "wx_item.h"

class wxGroupBox : public wxItem
{
 public:
  wxGroupBox(wxPanel *panel, char *label, int style, wxFont *_font = NULL);
  ~wxGroupBox();

  virtual void DoShow(Bool show);
  virtual void OnClientAreaDSize(int dW, int dH, int dX, int dY);

  virtual char *GetLabel();
  virtual void SetLabel(char *);

  virtual void Refresh(void);

  ControlHandle pane;

  virtual void Activate(Bool gray);
  virtual void MaybeMoveControls();

  virtual void SetPhantomSize(int w, int h);

  virtual void OnEvent(wxMouseEvent *event);

  virtual Bool AcceptsExplicitFocus(void);

protected:
  virtual void ChangeToGray(Bool gray);
  virtual void Paint(void);

  int orig_height;
  int phantom_height;
};

#endif // wx_gboxh
