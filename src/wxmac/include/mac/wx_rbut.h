///////////////////////////////////////////////////////////////////////////////
// File:	wx_rbut.h
// Purpose:	Declares wxRadioButton panel item (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_rbuth
#define wx_rbuth

#include "wx_panel.h"
#include "wx_item.h"

#ifdef IN_CPROTO
typedef       void* wxRadioButton ;
#else

class wxBitmap;

class wxRadioButton: public wxItem
{
 public:
  wxBitmap* buttonBitmap ;
  short bitmapState;
  short trackState;
  char *labelString;

  //=============================================================================
  // Public constructors
  //=============================================================================
 public:

  wxRadioButton // Constructor (given parentPanel, label)
    (
     wxPanel*	parentPanel,
     wxFunction	function,
     char*		label,
     int 		x = -1,
     int			y = -1,
     int			width = -1,
     int			height = -1,
     long		style = 0,
     wxFont             *_font = NULL,
     char*		windowName = "radioButton",
     WXTYPE		objectType = wxTYPE_ITEM // WCH: must use wxTYPE_RADIO_BUTTON
     );

  void Create // Constructor (given parentPanel, label)
    (
     wxPanel*	parentPanel,
     wxFunction	function,
     char*		label,
     int 		x = -1,
     int			y = -1,
     int			width = -1,
     int			height = -1,
     long		style = 0,
     char*		windowName = "radioButton",
     WXTYPE		objectType = wxTYPE_ITEM // WCH: must use wxTYPE_RADIO_BUTTON
     );

  wxRadioButton // Constructor (given parentPanel, bitmap)
    (
     wxPanel*	parentPanel,
     wxFunction	function,
     wxBitmap*	bitmap,
     int 		x = -1,
     int			y = -1,
     int			width = -1,
     int			height = -1,
     long		style = 0,
     wxFont             *_font = NULL,
     char*		windowName = "radioButton",
     WXTYPE		objectType = wxTYPE_ITEM // WCH: must use wxTYPE_RADIO_BUTTON
     );

  //=============================================================================
  // Public destructor
  //=============================================================================
 public:

  ~wxRadioButton(void);

  //=============================================================================
  // Public methods
  //=============================================================================
 public:

  virtual void SetValue(Bool);
  virtual Bool GetValue(void);
  void SetLabel(char*);
  void SetLabel(wxBitmap* bitmap);
  char* GetLabel(void) ;

  virtual void OnClientAreaDSize(int dW, int dH, int dX, int dY); // mac platform only
  virtual void Paint(void);
  virtual void DoShow(Bool show);

  virtual void OnEvent(wxMouseEvent *event); // mac platform only

  virtual void Highlight(Bool flag);

  virtual void OnSetFocus();
  virtual void OnKillFocus();

 protected:
  void ChangeToGray(Bool Gray);
};

#endif // IN_CPROTO
#endif // wx_rbuth
