///////////////////////////////////////////////////////////////////////////////
// File:	wxScroll.cc
// Purpose:	wxScroll (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wxScroll.h"
#include "wx_utils.h"
#include "wx_win.h"

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxScroll::wxScroll // root scroll
(
 wxWindow*		scrollWindow,
 wxScrollData*	scrollData
 )
{
  cScrollWindow = scrollWindow;
  cScrollData = scrollData;
  cParentScroll = NULL;
  cScrolls = new wxList(wxList::kNoDestroyData);
  WXGC_IGNORE(this, cScrollWindow);
}

//-----------------------------------------------------------------------------
wxScroll::wxScroll // child scroll
(
 wxWindow*	scrollWindow,
 wxWindow*	parentScrollWindow
 )
{
  cScrollWindow = scrollWindow;
  cScrollData = NULL;
  WXGC_IGNORE(this, scrollWindow);

  cScrolls = new wxList(wxList::kNoDestroyData);
 
  cParentScroll = parentScrollWindow->GetScroll();
  if (!cParentScroll)
    wxFatalError("No parent scroll for constructing scroll.");

  cParentScroll->cScrolls->Append(this);
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxScroll::~wxScroll(void)	// destructor
{
  // NOTE: Only the wxWindow object that owns this wxScroll should invoke its deletion.
  //       Hence, owner will NULL out its cScroll link to this object

  wxNode* childScrollNode;

  childScrollNode = cScrolls->First();
  while (childScrollNode)
    {
      wxScroll* childScroll;
      childScroll = (wxScroll*)childScrollNode->Data();
      childScroll->cParentScroll = cParentScroll;
      if (cParentScroll) cParentScroll->cScrolls->Append(this);
      childScrollNode = childScrollNode->Next();
    }

  if (cParentScroll) cParentScroll->OnDeleteChildScroll(this);

  cScrollWindow = NULL;
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Getter and setter methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxScrollData* wxScroll::GetScrollData (void)
{
  wxScroll* rootScroll; 
  wxScrollData* rootScrollData;

  rootScroll = RootScroll();
  rootScrollData = rootScroll->cScrollData;

  if (!rootScrollData) wxFatalError("No scroll data for scroll.");

  return rootScrollData;
}

//-----------------------------------------------------------------------------
void wxScroll::SetScrollData
(
 wxScrollData*		newScrollData,
 wxWhatScrollData	whatScrollData, // items to be changed
 wxScrollEvent*		e
 )
{
  wxScrollData* scrollData;
  wxScroll *root;
  scrollData = GetScrollData();
  scrollData->SetValue(newScrollData, whatScrollData);
  root = RootScroll();
  root->OnSetScrollData(scrollData, whatScrollData, e);
}

//-----------------------------------------------------------------------------
void wxScroll::SetScrollData
(
 int 			value,			// value for items to be changed
 wxWhatScrollData	whatScrollData, // items to be changed
 wxScrollEvent*		e
 )
{
  wxScrollData* scrollData;
  wxScroll *root;
  scrollData = GetScrollData();
  scrollData->SetValue(value, whatScrollData);
  root = RootScroll();
  root->OnSetScrollData(scrollData, whatScrollData, e);
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Scroll tree methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxScroll* wxScroll::RootScroll(void)
{
  wxScroll* result = this;
  while (result->cParentScroll) {
    result = result->cParentScroll;
  }
  return result;
}

//-----------------------------------------------------------------------------
void wxScroll::AddChildScrollWindow(wxWindow* childScrollWindow)
{
  wxScroll* childScroll;
  wxScrollData* scrollData;

  childScroll = childScrollWindow->GetScroll();
  childScroll->cParentScroll = this;

  cScrolls->Append(childScroll);

  if (childScroll->cScrollData)
    {
      DELETE_OBJ childScroll->cScrollData;
      childScroll->cScrollData = NULL;
    }

  scrollData = GetScrollData();
  childScroll->OnSetScrollData(scrollData, wxWhatScrollData::wxAll, NULL);
}

//-----------------------------------------------------------------------------
void wxScroll::OnDeleteChildScroll(wxScroll* childScroll)
{
  cScrolls->OnDeleteObject(childScroll);
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxScroll::OnSetScrollData
(
 wxScrollData*		scrollData,
 wxWhatScrollData	whatScrollData, // items to be changed
 wxScrollEvent*		e
 )
{
  wxNode* childScrollNode;

  childScrollNode = cScrolls->First();
  while (childScrollNode) {
    wxScroll* childScroll;
    childScroll = (wxScroll*)childScrollNode->Data();
    childScroll->OnSetScrollData(scrollData, whatScrollData, e);
    childScrollNode = childScrollNode->Next();
  }
  
  cScrollWindow->SetScrollData(scrollData, whatScrollData, e);
}
