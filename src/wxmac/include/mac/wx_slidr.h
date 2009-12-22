/*
 * File:	wx_slidr.h
 * Purpose:	Slider
 * Author:	Julian Smart (Cecil Coupe)
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_slidr.h	1.2 5/9/94" */

#ifndef wx_slidrh
#define wx_slidrh

#include "wb_slidr.h"
#include "wxLabelArea.h"
#ifdef IN_CPROTO
typedef       void    *wxSlider ;
#else

// Slider
class wxSlider: public wxbSlider
{
public:
  int s_min;
  int s_max;
  int page_size;
  wxLabelArea*	cTitle;
  Rect		controlRect;
  Rect		valueRect;
  wxFont*	valueFont;
  int		valuebase;

  wxSlider(wxPanel *panel, wxFunction func, char *label, int value,
           int min_value, int max_value, int width, int x = -1, int y = -1,
           long style = 0, wxFont *_font = NULL, char *name = "slider",
           WXTYPE objectType = wxTYPE_SLIDER);
  ~wxSlider(void);

  Bool Create(wxPanel *panel, wxFunction func, char *label, int value,
           int min_value, int max_value, int width, int x = -1, int y = -1,
           long style = 0, char *name = "slider");
  virtual int GetValue(void);
  virtual char *GetLabel(void);
  virtual void SetValue(int);
  virtual void SetLabel(char *label);
#if 0 // CJC
  void GetSize(int *x, int *y);
  void SetSize(int x, int y, int width, int height);
  void GetPosition(int *x, int *y);
#endif
  // For the mac:
  virtual void Paint(void);
  virtual void OnEvent(wxMouseEvent *event);
  virtual void OnClientAreaDSize(int dW, int dH, int dX, int dY);
  virtual void MaybeMoveControls();
  
  virtual void DoShow(Bool s);
  
  virtual void InternalGray(int gray);

  virtual void OnSetFocus();
  virtual void OnKillFocus();
  
  void TrackPart(int);
protected:
  void InsetSliderRect(Rect *r);  
};

#endif // IN_CPROTO
#endif // wx_slidrh
