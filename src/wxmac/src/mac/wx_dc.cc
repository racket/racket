///////////////////////////////////////////////////////////////////////////////
// File:	wx_dc.cc
// Purpose:	Device context implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "common.h"
#include "wx_gdi.h"
#include "wx_dc.h"

//-----------------------------------------------------------------------------
wxDC::wxDC(void)
     //-----------------------------------------------------------------------------
{
  cMacDC = NULL;
  cMacCurrentTool = kNoTool;
  current_background_color = new WXGC_PTRS wxColour(wxWHITE);
  current_text_foreground = new WXGC_PTRS wxColour(wxBLACK);
  current_text_background = new WXGC_PTRS wxColour(wxWHITE);
}

//-----------------------------------------------------------------------------
wxDC::~wxDC(void)
     //-----------------------------------------------------------------------------
{
}

wxGL *wxDC::GetGL()
{
  return NULL;
}

//-----------------------------------------------------------------------------
void wxDC::wxMacDrawPoint(int x1, int y1)
     //-----------------------------------------------------------------------------
{
  MoveTo(x1 + SetOriginX, y1 + SetOriginY);
  Line(0, 0);
}

//-----------------------------------------------------------------------------
void wxDC::wxMacDrawLine(int x1, int y1, int x2, int y2)
     //-----------------------------------------------------------------------------
{
  MoveTo(x1 + SetOriginX, y1 + SetOriginY);
  LineTo(x2 + SetOriginX, y2 + SetOriginY);
}

void wxDC::SetTextForeground(wxColour *colour)
{
  wxbDC::SetTextForeground(colour);
  ToolChanged(kTextTool);
}

void wxDC::SetTextBackground(wxColour *colour)
{
  wxbDC::SetTextBackground(colour);
  ToolChanged(kTextTool);
}

void wxDC::SetBackgroundMode(int mode)
{
  wxbDC::SetBackgroundMode(mode);
  ToolChanged(kTextTool);
}

void wxDC::ToolChanged(wxMacToolType tool)
{
  if ((tool == kNoTool) || (cMacCurrentTool == tool))
    cMacCurrentTool = kNoTool;
}

void wxDC::DrawTabBase(double x, double y, double w, double h, int state)
{
  /* Only in canvas... */
}

void wxDC::DrawTab(char *str, double x, double y, double w, double h, int state)
{
  /* Only in canvas... */
}
