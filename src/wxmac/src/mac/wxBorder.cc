///////////////////////////////////////////////////////////////////////////////
// File:	wxBorder.cc
// Purpose:	Macintosh Border implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wxBorder.h"
#include "wxMacDC.h"
#include "wx_area.h"
#include "wx_gdi.h"
#ifndef WX_CARBON
# include <Windows.h>
# include "wxBorderArea.h"
#endif

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxBorder::wxBorder // Constructor (given parentArea)
(
 wxArea*		parentArea,
 char*		windowName,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 WXTYPE		objectType
 ) :
 wxWindow (windowName, parentArea, x, y, width, height, style)
{
  __type = wxTYPE_BORDER;
  if (width < 0) {
    cWindowWidth = parentArea->Width();
  }
  if (height < 0) {
    cWindowHeight = parentArea->Height();
  }
  SetJustify(wxAll);
  SetGravitate(wxTop | wxLeft);
  
  SetEraser(wxCONTROL_BACKGROUND_BRUSH);

  {
    wxWindow *p;
    p = GetParent();
    if (p->IsHidden())
      DoShow(FALSE);
  }
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxBorder::~wxBorder(void)
{
}

//-----------------------------------------------------------------------------
void wxBorder::DoShow(Bool show)
{
  wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------
void wxBorder::ShowAsActive(Bool flag) // mac platform only
{
}

