/*
 * File:	wx_choic.h
 * Purpose:	Choice panel item
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_choic.h	1.2 5/9/94" */

#ifndef wx_choich
#define wx_choich

#include "wb_choic.h"
#include "wxLabelArea.h"
#ifndef WX_CARBON
# include <Controls.h>
# include <Menus.h>
#endif

#ifdef IN_CPROTO
typedef       void    *wxChoice ;
#else

// Choice item
class wxChoice: public wxbChoice
{
  MenuHandle 	hDynMenu;
  StringPtr	sTitle;
  int             selection;
  int             labelbase;			// number pixels from top to baseline
  int             valuebase;			// ""
  short		PopUpID;			// Mac Menu Mgr ID - never reused I hope
  wxLabelArea*	cTitle;
        
 public:

  wxChoice (void);
  wxChoice(wxPanel *panel, wxFunction func, char *Title,
           int x = -1, int y = -1, int width = -1, int height = -1,
           int N = 0, char **Choices = NULL,
           long style = 0, wxFont *_font = NULL, char *name = "choice"
	);
  ~wxChoice(void);

  Bool Create(wxPanel *panel, wxFunction func, char *Title,
           int x = -1, int y = -1, int width = -1, int height = -1,
           int N = 0, char **Choices = NULL,
           long style = 0, char *name = "choice");
  void Append(char *Item);
  void Clear(void);
  int GetSelection(void);
  void SetSelection(int n);
  char *GetLabel(void);
  void SetLabel(char *label);

  void SetColumns(int n = 1 ) { /* no effect */ } ;
  int GetColumns(void) { return 1 ; };
  
  int Number(void);
  
  void OnClientAreaDSize(int dW, int dH, int dX, int dY);

  virtual void Paint(void);
  virtual void DoShow(Bool show);

  virtual void OnEvent(wxMouseEvent *event);

  virtual void OnSetFocus();
  virtual void OnKillFocus();

  void DrawChoice(Bool flag);
  void ReCalcRect(void);

  virtual void InternalGray(int gray);

  virtual void MaybeMoveControls();

  void OnChar(wxKeyEvent *e);

 protected:
  virtual void ChangeToGray(Bool gray);
};

#endif // IN_CPROTO
#endif // wx_choich
