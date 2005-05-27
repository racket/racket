///////////////////////////////////////////////////////////////////////////////
// File:	wxScrollData.cc
// Purpose:	wxScrollData (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wxGC.h"
#include "wxScrollData.h"

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxScrollData::wxScrollData
(
 void
 )
{
  cOriginX = 0;
  cOriginY = 0;
  cSizeW = 0;
  cSizeH = 0;
  cPositionH = 0;
  cPositionV = 0;
  cUnitW = 0;
  cUnitH = 0;
  cPageW = 0;
  cPageH = 0;
}

//-----------------------------------------------------------------------------
wxScrollData::wxScrollData
(
 int					value,			// initial value for selected items
 wxWhatScrollData	whatScrollData	// what items to initialize with given value
 )
{
  cOriginX = (long)whatScrollData & wxWhatScrollData::wxOriginX ? value : 0;
  cOriginY = (long)whatScrollData & wxWhatScrollData::wxOriginY ? value : 0;
  cSizeW = (long)whatScrollData & wxWhatScrollData::wxSizeW ? value : 0;
  cSizeH = (long)whatScrollData & wxWhatScrollData::wxSizeH ? value : 0;
  cPositionH = (long)whatScrollData & wxWhatScrollData::wxPositionH ? value : 0;
  cPositionV = (long)whatScrollData & wxWhatScrollData::wxPositionV ? value : 0;
  cUnitW = (long)whatScrollData & wxWhatScrollData::wxUnitW ? value : 0;
  cUnitH = (long)whatScrollData & wxWhatScrollData::wxUnitH ? value : 0;
  cPageW = (long)whatScrollData & wxWhatScrollData::wxPageW ? value : 0;
  cPageH = (long)whatScrollData & wxWhatScrollData::wxPageH ? value : 0;
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxScrollData::~wxScrollData(void)	// destructor
{
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Getter and setter methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxScrollData::SetValue
(
 wxScrollData* 		newScrollData,	// may have changes only
 wxWhatScrollData 	whatScrollData	// items to be changed
 )
{
  if ((long)whatScrollData & wxWhatScrollData::wxOriginX) 	cOriginX 	= newScrollData->cOriginX;
  if ((long)whatScrollData & wxWhatScrollData::wxOriginY) 	cOriginY 	= newScrollData->cOriginY;
  if ((long)whatScrollData & wxWhatScrollData::wxSizeW) 	cSizeW 		= newScrollData->cSizeW;
  if ((long)whatScrollData & wxWhatScrollData::wxSizeH) 	cSizeH 		= newScrollData->cSizeH;
  if ((long)whatScrollData & wxWhatScrollData::wxPositionH) cPositionH 	= newScrollData->cPositionH;
  if ((long)whatScrollData & wxWhatScrollData::wxPositionV) cPositionV 	= newScrollData->cPositionV;
  if ((long)whatScrollData & wxWhatScrollData::wxUnitW) 	cUnitW 		= newScrollData->cUnitW;
  if ((long)whatScrollData & wxWhatScrollData::wxUnitH) 	cUnitH 		= newScrollData->cUnitH;
  if ((long)whatScrollData & wxWhatScrollData::wxPageW) 	cPageW 		= newScrollData->cPageW;
  if ((long)whatScrollData & wxWhatScrollData::wxPageH) 	cPageH 		= newScrollData->cPageH;
}

//-----------------------------------------------------------------------------
void wxScrollData::SetValue
(
 int 				value,	// value for items to be changed
 wxWhatScrollData 	whatScrollData	// items to be changed
 )
{
  if ((long)whatScrollData & wxWhatScrollData::wxOriginX) 	cOriginX 	= value;
  if ((long)whatScrollData & wxWhatScrollData::wxOriginY) 	cOriginY 	= value;
  if ((long)whatScrollData & wxWhatScrollData::wxSizeW) 	cSizeW 		= value;
  if ((long)whatScrollData & wxWhatScrollData::wxSizeH) 	cSizeH 		= value;
  if ((long)whatScrollData & wxWhatScrollData::wxPositionH) cPositionH 	= value;
  if ((long)whatScrollData & wxWhatScrollData::wxPositionV) cPositionV 	= value;
  if ((long)whatScrollData & wxWhatScrollData::wxUnitW) 	cUnitW 		= value;
  if ((long)whatScrollData & wxWhatScrollData::wxUnitH) 	cUnitH 		= value;
  if ((long)whatScrollData & wxWhatScrollData::wxPageW) 	cPageW 		= value;
  if ((long)whatScrollData & wxWhatScrollData::wxPageH) 	cPageH 		= value;
}

//-----------------------------------------------------------------------------
int wxScrollData::GetValue
(
 wxWhatScrollData	whatScrollData	// what item to get
 )
{
  int result = 0;

  if ((long)whatScrollData & wxWhatScrollData::wxOriginX) 	result 	= cOriginX;
  if ((long)whatScrollData & wxWhatScrollData::wxOriginY) 	result 	= cOriginY;
  if ((long)whatScrollData & wxWhatScrollData::wxSizeW) 	result 	= cSizeW;
  if ((long)whatScrollData & wxWhatScrollData::wxSizeH) 	result 	= cSizeH;
  if ((long)whatScrollData & wxWhatScrollData::wxPositionH) result 	= cPositionH;
  if ((long)whatScrollData & wxWhatScrollData::wxPositionV) result 	= cPositionV;
  if ((long)whatScrollData & wxWhatScrollData::wxUnitW) 	result 	= cUnitW;
  if ((long)whatScrollData & wxWhatScrollData::wxUnitH) 	result 	= cUnitH;
  if ((long)whatScrollData & wxWhatScrollData::wxPageW) 	result 	= cPageW;
  if ((long)whatScrollData & wxWhatScrollData::wxPageH) 	result 	= cPageH;

  return result;
}
