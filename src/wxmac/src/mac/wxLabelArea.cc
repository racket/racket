///////////////////////////////////////////////////////////////////////////////
// File:	wxLabelArea.cc
// Purpose:	Label area (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wxLabelArea.h"
#include "wx_messg.h"
#include "wx_gdi.h"

//=============================================================================
// Construction methods
//=============================================================================

//-----------------------------------------------------------------------------
wxLabelArea::wxLabelArea
(
 wxWindow*	parentWindow,
 char*		label,
 wxFont*		theFont,
 Direction	direction,
 int			xoffset,
 int			yoffset
 ) :
 wxArea(parentWindow)
{
  int labelWidth = 0;
  int labelHeight = 0;
  wxFont* labelFont;

  __type = wxTYPE_LABEL_AREA;
  cLabelText = new wxMessage(this, label, theFont, xoffset, yoffset);
  cDirection = direction;

  labelFont = cLabelText->font;
  if (labelFont) {
      char* labelText;
      double fLabelWidth, fLabelHeight, fDescent, fExternalLeading;

      labelText = cLabelText->GetLabel();
      labelFont->GetTextExtent(labelText, 0, &fLabelWidth, &fLabelHeight,
			       &fDescent, &fExternalLeading, TRUE);
      labelWidth = (int)fLabelWidth;
      labelHeight = (int)fLabelHeight;
    }

  if (cDirection & wxTop)
    SetMargin(labelHeight + 3, wxTop);
  else
    SetMargin(labelWidth + 3, wxLeft);
}

//-----------------------------------------------------------------------------
wxLabelArea::~wxLabelArea(void)	// destructor
{
  cLabelText = NULL;
}

//=============================================================================
// Getter and setter methods
//=============================================================================

//-----------------------------------------------------------------------------
void wxLabelArea::SetLabel(char* label) { cLabelText->SetLabel(label); }

//-----------------------------------------------------------------------------
char* wxLabelArea::GetLabel(void) { return cLabelText->GetLabel(); }

void wxLabelArea::DoShow(Bool on)
{
  cLabelText->DoShow(on);
}

