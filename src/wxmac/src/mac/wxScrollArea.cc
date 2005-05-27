///////////////////////////////////////////////////////////////////////////////
// File:	wxScrollArea.cc
// Purpose:	Scroll area (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wxScrollArea.h"
#include "wx_sbar.h"
#include "wx_utils.h"

#define kVScrollBarWidth 15
#define kHScrollBarHeight 15

//=============================================================================
// Construction methods
//=============================================================================

//-----------------------------------------------------------------------------
wxScrollArea::wxScrollArea
(
 wxWindow*	parentWindow,
 wxWindow*	parentScrollWindow,
 long		style
 ) :
 wxArea (parentWindow)
{
  Bool bothScrolls;

  cStyle = style;
  cVScrollBar = NULL;
  cHScrollBar = NULL;

  if (!parentScrollWindow) wxFatalError("No parentScrollWindow for wxScrollArea");

  bothScrolls = ((cStyle & wxVSCROLL) && (cStyle & wxHSCROLL));

  if (cStyle & wxVSCROLL)
    {
      cVScrollBar = new wxScrollBar(this, NULL, "",
				    0, 0, kVScrollBarWidth, 0, wxVSCROLL);
      parentScrollWindow->AddChildScrollWindow(cVScrollBar);

      ResetVPos(bothScrolls || (cStyle & wxRESIZE_CORNER));
      cVScrollBar->SetJustify(wxVertical);
      cVScrollBar->SetGravitate(wxRight);
      SetMargin(kVScrollBarWidth, wxRight);
    }

  if (cStyle & wxHSCROLL)
    {
      cHScrollBar = new wxScrollBar(this, NULL, "",
				    0, 0, 0, kHScrollBarHeight, wxHSCROLL);
      parentScrollWindow->AddChildScrollWindow(cHScrollBar);
      ResetHPos(bothScrolls || (cStyle & wxRESIZE_CORNER));
      cHScrollBar->SetJustify(wxHorizontal);
      cHScrollBar->SetGravitate(wxBottom);
      SetMargin(kHScrollBarHeight, wxBottom);
    }
}

//-----------------------------------------------------------------------------
wxScrollArea::~wxScrollArea(void)	// destructor
{
}


void wxScrollArea::ResetVPos(Bool leaveSpace)
{
  int h;
  h = Height();
  cVScrollBar->GravitateJustify(wxRight | wxTop,
				wxVertical,
				0, 0,
				Width(),
				h - (leaveSpace ? kHScrollBarHeight - 1 : 0));
}

void wxScrollArea::ResetHPos(Bool leaveSpace)
{
  int w;
  w = Width();
  cHScrollBar->GravitateJustify(wxBottom | wxLeft,
				wxHorizontal,
				0, 0,
				w - (leaveSpace ? kVScrollBarWidth - 1 : 0),
				Height());
}

//-----------------------------------------------------------------------------
void wxScrollArea::ShowScrolls(Bool h, Bool v)
{
  if (cStyle & wxHSCROLL) {
    if (!h) {
      cHScrollBar->Show(FALSE);
      SetMargin(0, wxBottom);
    }
    cHScrollBar->Show(h);
    SetMargin(h ? kHScrollBarHeight : 0, wxBottom);
    if ((cStyle & wxVSCROLL) && !(cStyle & wxRESIZE_CORNER)) {
      ResetVPos(h);
    }
  }

  if (cStyle & wxVSCROLL) {
    cVScrollBar->Show(v);
    SetMargin(v ? kHScrollBarHeight : 0, wxRight);
    if ((cStyle & wxHSCROLL) && !(cStyle & wxRESIZE_CORNER))
      ResetHPos(v);
  }
}

void wxScrollArea::SetResizeCorner(Bool on, Bool h, Bool v)
{
  if (!on != !(cStyle & wxRESIZE_CORNER)) {
    if (on)
      cStyle |= wxRESIZE_CORNER;
    else
      cStyle -= wxRESIZE_CORNER;

    ResetHPos((h && v) || on);
    ResetVPos((h && v) || on);
  }
}

void wxScrollArea::MaybeMoveScrollControls()
{
  if (cVScrollBar)
    cVScrollBar->MaybeMoveControls();
  if (cHScrollBar)
    cHScrollBar->MaybeMoveControls();
}
