///////////////////////////////////////////////////////////////////////////////
// File:	wxButtonBorder.cc
// Purpose:	Macintosh ButtonBorder implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wxButtonBorder.h"
#include "wx_area.h"

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxButtonBorder::wxButtonBorder // Constructor (given parentArea)
(
 wxArea*		parentArea,
 int			margin,
 Direction	direction,
 char*		windowName,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 WXTYPE		objectType
 ) :
 wxBorder (parentArea, windowName, x, y, width, height, style, objectType)
{
  parentArea->SetMargin(margin, direction);
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxButtonBorder::~wxButtonBorder(void)
{
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxButtonBorder::Paint(void)
{
  int margin;

  if (cHidden) return;

  {
    wxArea *area;
    wxMargin m;
    area = ParentArea();
    m = area->Margin();
    margin = m.Offset(wxTop);
  }
  if (margin)
    {	
      int clientWidth, clientHeight;
      Rect clientRect;
      PenState oldPenState;
      wxArea *carea;

      carea = ClientArea();
      clientWidth = carea->Width();
      clientHeight = carea->Height();
      SetCurrentDC();
      ::SetRect(&clientRect, 0, 0, clientWidth, clientHeight);
      OffsetRect(&clientRect,SetOriginX,SetOriginY);
      ::GetPenState(&oldPenState);
      ::PenNormal();
      ::PenSize(margin -1 , margin - 1);
      ::FrameRoundRect(&clientRect, 16, 16);
      ::SetPenState(&oldPenState);
    }
}
