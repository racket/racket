///////////////////////////////////////////////////////////////////////////////
// File:	wx_check.h
// Purpose:	Declares wxCheckBox panel item (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_checkh
#define wx_checkh

#include "wb_check.h"

#ifdef IN_CPROTO
typedef       void* wxCheckBox ;
#else

class wxBitmap;

class wxCheckBox: public wxbCheckBox
{
  //=============================================================================
  // Protected variables
  //=============================================================================
 protected:

  wxBitmap *buttonBitmap ;
  short bitmapState;
  short trackState;
  char *labelString;

  //=============================================================================
  // Public methods
  //=============================================================================
 public:

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Constructors
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  wxCheckBox // Constructor (given parentPanel, label)
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
     char*		windowName = "checkBox",
     WXTYPE		objectType = wxTYPE_CHECK_BOX
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
     char*		windowName = "checkBox",
     WXTYPE		objectType = wxTYPE_CHECK_BOX
     );

  wxCheckBox // Constructor (given parentPanel, bitmap)
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
     char*		windowName = "checkBox",
     WXTYPE		objectType = wxTYPE_CHECK_BOX
     );

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Destructor
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ~wxCheckBox(void);

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Item methods
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  virtual void SetValue(Bool value);
  virtual Bool GetValue(void);
  void SetLabel(char* label);
  void SetLabel(wxBitmap* bitmap);
  char* GetLabel(void) ;

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Sizing methods
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  virtual void OnClientAreaDSize(int dW, int dH, int dX, int dY); // mac platform only

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Other methods
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  virtual void DoShow(Bool show);

  virtual void Highlight(Bool flag);

  virtual void OnSetFocus();
  virtual void OnKillFocus();

  //=============================================================================
  // Protected methods
  //=============================================================================
 protected:

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Other methods
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  virtual void Paint(void);
  virtual void OnEvent(wxMouseEvent *event); // mac platform only
 protected:
  void ChangeToGray(Bool Gray);
};

#endif // IN_CPROTO
#endif // wx_checkh
