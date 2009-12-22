///////////////////////////////////////////////////////////////////////////////
// File:	wx_rbox.h
// Purpose:	Declares radio box item (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_rboxh
#define wx_rboxh

#include "wb_rbox.h"

#ifdef IN_CPROTO
typedef       void* wxRadioBox;
#else

class wxBitmap;
class wxPanel;
class wxMessage;
class wxList;

class wxRadioBox: public wxbRadioBox
{
 public:
  wxPanel*	cRadioPanel;
  wxPanel*	cButtonHolder;
  wxMessage*	cRadioTitle;
  wxList*   	cRadioButtons; // list of wxRadioButton items
  int           focused_button;

  //=============================================================================
  // Public constructors
  //=============================================================================
 public:

  wxRadioBox // Constructor (given parentPanel, label choices)
    (
     wxPanel*	parentPanel,
     wxFunction	function,
     char*		Title,
     int 		x = -1,
     int			y = -1,
     int			width = -1,
     int			height = -1,
     int			N = 0,
     char**		Choices = NULL,
     int			majorDim = 0,
     long		style = 0,
     wxFont             *_font = NULL,
     char*		windowName = "radioBox",
     WXTYPE		objectType = wxTYPE_RADIO_BOX
     );

  wxRadioBox // Constructor (given parentPanel, bitmap choices)
    (
     wxPanel*	parentPanel,
     wxFunction	function,
     char*		Title,
     int 		x = -1,
     int			y = -1,
     int			width = -1,
     int			height = -1,
     int			N = 0,
     wxBitmap**	Choices = NULL,
     int			majorDim = 0,
     long		style = 0,
     wxFont             *_font = NULL,
     char*		windowName = "radioBox",
     WXTYPE		objectType = wxTYPE_RADIO_BOX
     );

  //=============================================================================
  // Public destructor
  //=============================================================================
 public:

  ~wxRadioBox(void);

  //=============================================================================
  // Public methods
  //=============================================================================
 public:

  void SetSelection(int N);
  int GetSelection(void);
  void Enable(Bool enable);
  void Enable(int item, Bool enable);
  void DoShow(Bool show);
  void Show(int item, Bool show);
  char* GetLabel(void);
  void SetLabel(char* label);
  void SetLabel(int item, char* label);
  void SetLabel(int item, wxBitmap* bitmap);
  char* GetLabel(int item);

  int ButtonFocus(int);

  virtual void OnSetFocus();
  virtual void OnKillFocus();

  virtual void OnClientAreaDSize(int dW, int dH, int dX, int dY);
  virtual void MaybeMoveControls();
};

#endif // IN_CPROTO
#endif // wx_rboxh
