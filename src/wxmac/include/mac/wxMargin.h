///////////////////////////////////////////////////////////////////////////////
// File:	wxMargin.h
// Purpose:	Margin (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wxMarginh
#define wxMarginh

#include "wxDirection.h"

/* For the precise-GC transformer, we pretend that wxMargin is
   atomic. It has no pointers, after all. */
#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

class wxMargin
{
  protected:
  int left;
  int top;
  int right;
  int bottom;
  
 public:
  
  //=============================================================================
  // Construction methods
  //=============================================================================

  //-----------------------------------------------------------------------------
  inline wxMargin(int margin = 0)
  {
    left = margin;
    top = margin;
    right = margin;
    bottom = margin;
  }
  
  //-----------------------------------------------------------------------------
  inline wxMargin(int margin, Direction direction)
  {
    left = (direction & wxLeft ? margin : 0);
    top = (direction & wxTop ? margin : 0);
    right = (direction & wxRight ? margin : 0);
    bottom = (direction & wxBottom ? margin : 0);
  }

  //-----------------------------------------------------------------------------
  inline wxMargin(const wxMargin& margin)
  {
    left = margin.left;
    top = margin.top;
    right = margin.right;
    bottom = margin.bottom;
  }

  //-----------------------------------------------------------------------------
  inline ~wxMargin(void)
  {
  }

  //=============================================================================
  // Overloaded operator methods
  //=============================================================================

  inline wxMargin& operator +=(wxMargin margin)
  {
    left += margin.left;
    top += margin.top;
    right += margin.right;
    bottom += margin.bottom;
    return *this;
  }

  //=============================================================================
  // Getter and setter methods
  //=============================================================================

  //-----------------------------------------------------------------------------
  inline void SetMargin(wxMargin margin, Direction direction)
  {
    if (direction & wxLeft) left = margin.left;
    if (direction & wxTop) top = margin.top;
    if (direction & wxRight) right = margin.right;
    if (direction & wxBottom) bottom = margin.bottom;
  }

  //-----------------------------------------------------------------------------
  inline void SetMargin(int margin, Direction direction)
  {
    if (direction & wxLeft) left = margin;
    if (direction & wxTop) top = margin;
    if (direction & wxRight) right = margin;
    if (direction & wxBottom) bottom = margin;
  }
  
  //-----------------------------------------------------------------------------
  inline int Offset(Direction direction)
  {
    int result = 0;
    if (direction & wxLeft) result += left;
    if (direction & wxTop) result += top;
    if (direction & wxRight) result += right;
    if (direction & wxBottom) result += bottom;
    
    return result;
  }
};

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

#endif // wxMarginh
