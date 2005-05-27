/*
 * File:	wb_canvs.cc
 * Purpose:	wxbCanvas implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_setup.h"
#include "wx_dc.h"
#include "wx_canvs.h"

class wxFrame;

// Constructor (given parentArea)
wxbCanvas::wxbCanvas (char* windowName, wxArea* parentArea, int x, int y,
		int width, int height, long style)
  : wxWindow ( windowName, parentArea, wxNEGPOS_IS_DEFAULT(x), wxNEGPOS_IS_DEFAULT(y), width, height, style),
		is_retained (FALSE), // Can only be retained after scrollbars have been set
		horiz_units (0),
		vert_units (0)
{
  __type = wxTYPE_CANVAS;
  windowStyle = style;
}

// Constructor (given parentWindow)
wxbCanvas::wxbCanvas(char* windowName, wxWindow* parentWindow, int x, int y,
		int width, int height, long style) 
  : wxWindow ( windowName, parentWindow, wxNEGPOS_IS_DEFAULT(x), wxNEGPOS_IS_DEFAULT(y), width, height, style),
		is_retained (FALSE), // Can only be retained after scrollbars have been set
		horiz_units (0),
		vert_units (0)
{
  __type = wxTYPE_CANVAS;
  windowStyle = style;
}

wxbCanvas::~wxbCanvas(void)
{
}

void wxbCanvas::AllowDoubleClick(int value)
{
  doubleClickAllowed = value ;
}


wxCanvasDC *wxbCanvas::GetDC(void)
{
  return wx_dc;
}

// Default input behaviour for a scrolling canvas should be to scroll
// according to the cursor keys pressed
void wxbCanvas::OnChar(wxKeyEvent* event)
{
  int x_page = 0;
  int y_page = 0;
  int start_x = 0;
  int start_y = 0;
  GetScrollUnitsPerPage(&x_page, &y_page);
  ViewStart(&start_x, &start_y);

  switch (event->keyCode)
  {
    case WXK_PRIOR:
    {
      if ((y_page > 0) && (start_y >= y_page))
        Scroll(start_x, start_y - y_page);
      break;
    }
    case WXK_NEXT:
    {
      if (y_page > 0)
        Scroll(start_x, start_y + y_page);
      break;
    }
    case WXK_UP:
    {
      if ((y_page > 0) && (start_y >= 1))
        Scroll(start_x, start_y - 1);
      break;
    }
    case WXK_DOWN:
    {
      if (y_page > 0)
        Scroll(start_x, start_y + 1);
      break;
    }
    case WXK_LEFT:
    {
      if ((x_page > 0) && (start_x >= 1))
        Scroll(start_x - 1, start_y);
      break;
    }
    case WXK_RIGHT:
    {
      if (x_page > 0)
        Scroll(start_x + 1, start_y);
      break;
    }
    case WXK_HOME:
    {
      Scroll(0, 0);
      break;
    }
  }
}

double wxbCanvas::GetCharHeight(void)
{
  return wx_dc->GetCharHeight();
}

double wxbCanvas::GetCharWidth(void)
{
  return wx_dc->GetCharWidth();
}

void wxbCanvas::GetTextExtent(const char* string, double* x, double* y, double* descent,
			      double* externalLeading, wxFont* the_font, Bool use16)
{
  wx_dc->GetTextExtent(string, x, y, descent, externalLeading, the_font, FALSE, use16);
}

