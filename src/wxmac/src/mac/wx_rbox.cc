///////////////////////////////////////////////////////////////////////////////
// File:	wx_rbox.cc
// Purpose:	Panel item radioBox implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wx_rbox.h"
#include "wx_rbut.h"
#include "wx_messg.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_gdi.h"
#include "wx_area.h"
#include "wxBorderArea.h"

//-----------------------------------------------------------------------------
static void wxRadioButtonProc(wxRadioButton *radioButton, wxCommandEvent *event)
{
  wxPanel* radioPanel;
  wxWindow *rb;
  wxRadioBox* radioBox;
  wxCommandEvent *commandEvent;
  int radioButtonIndex;

  radioPanel = (wxPanel*)radioButton->GetParent();
  rb = radioPanel;
  while (wxSubType(rb->__type, wxTYPE_PANEL)) {
    rb = rb->GetParent();
  }
  radioBox = (wxRadioBox *)rb;
  radioButtonIndex = radioBox->cRadioButtons->MemberIndex(radioButton);
  radioBox->SetSelection(radioButtonIndex);

  commandEvent = new WXGC_PTRS wxCommandEvent(wxEVENT_TYPE_RADIOBOX_COMMAND);
  radioBox->ProcessCommand(commandEvent);
}

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxRadioBox::wxRadioBox // Constructor (given parentPanel, label choices)
(
 wxPanel*	parentPanel,
 wxFunction	function,
 char*		Title,
 int 		x,
 int			y,
 int			width,
 int			height,
 int			N,
 char**		Choices,
 int			majorDim,
 long		style,
 wxFont         *_font,
 char*		windowName,
 WXTYPE		objectType
 ) : wxbRadioBox (parentPanel, function, Title, x, y, width, height, N, majorDim, style, windowName)
{
  wxPanel *buttonHolder;
  int i;

  focused_button = -1;

  SetFont(_font, 13);
  
  cRadioButtons = new WXGC_PTRS wxList(wxList::kNoDestroyData);

  Callback(function);

  cRadioPanel = new WXGC_PTRS wxPanel(this->ClientArea(), 0, 0, 0, 0, 0);

  buttonHolder = cRadioPanel;

  if (Title) {
    Title = wxItemStripLabel(Title);
    cRadioTitle = new WXGC_PTRS wxMessage(cRadioPanel, Title, font);
    if (labelPosition != wxVERTICAL) {
      buttonHolder = new WXGC_PTRS wxPanel(cRadioPanel->ClientArea(), -1, -1, 0, 0, 0);
      cButtonHolder = buttonHolder;
    } else
      buttonHolder->NewLine();
  } else 
    cRadioTitle = NULL;

  for (i = 0; i < N; i++) {
    char *choice;
    wxRadioButton* radioButton;

    choice = wxItemStripLabel(Choices[i]);
    if (i && ((style & wxVERTICAL) == wxVERTICAL))
      buttonHolder->NewLine();
    radioButton = new WXGC_PTRS wxRadioButton(buttonHolder, (wxFunction)wxRadioButtonProc, choice,
					      -1, -1, -1, -1, 0, font);
    cRadioButtons->Append(radioButton);
  }
  SetSelection(0);

  buttonHolder->Fit();
  if (buttonHolder != cRadioPanel) {
    cRadioPanel->Fit();
    cRadioTitle->Centre(wxVERTICAL);
  }

  if (style & wxBORDER) new WXGC_PTRS wxBorderArea(this);

  if (width < 0 || height < 0)
    Fit();
	
  {
    wxWindow *p;
    p = GetParent();
    if (p->IsHidden())
      DoShow(FALSE);
  }
  if (style & wxINVISIBLE)
    wxItem::Show(FALSE);
  InitInternalGray();
}

//-----------------------------------------------------------------------------
wxRadioBox::wxRadioBox // Constructor (given parentPanel, bitmap choices)
(
 wxPanel*	parentPanel,
 wxFunction	function,
 char*		Title,
 int 		x,
 int			y,
 int			width,
 int			height,
 int			N,
 wxBitmap**	Choices,
 int			majorDim,
 long		style,
 wxFont         *_font,
 char*		windowName,
 WXTYPE		objectType
 ) : wxbRadioBox (parentPanel, function, Title, x, y, width, height, N, majorDim, style, windowName)
{
  int i;
  wxPanel *buttonHolder;

  focused_button = -1;

  SetFont(_font, 13);

  cRadioButtons = new WXGC_PTRS wxList(wxList::kNoDestroyData);

  Callback(function);

  cRadioPanel = new WXGC_PTRS wxPanel(this->ClientArea(), 0, 0, 0, 0, 0);
  
  buttonHolder = cRadioPanel;
	
  if (Title) {
    cRadioTitle = new WXGC_PTRS wxMessage(cRadioPanel, Title, font);
    if (labelPosition != wxVERTICAL) {
      buttonHolder = new WXGC_PTRS wxPanel(cRadioPanel->ClientArea(), -1, -1, 0, 0, 0);
      cButtonHolder = buttonHolder;
    } else
      buttonHolder->NewLine();
  } else
    cRadioTitle = NULL;

  for (i = 0; i < N; i++) {
    wxRadioButton* radioButton;
    if (i && ((style & wxVERTICAL) == wxVERTICAL))
      buttonHolder->NewLine();
    radioButton = new WXGC_PTRS wxRadioButton(buttonHolder, (wxFunction)wxRadioButtonProc, Choices[i],
					      -1, -1, -1, -1, 0, font);
    cRadioButtons->Append(radioButton);
  }
  SetSelection(0);
  
  buttonHolder->Fit();
  if (buttonHolder != cRadioPanel) {
    cRadioPanel->Fit();
    cRadioTitle->Centre(wxVERTICAL);
  }

  if (style & wxBORDER) new WXGC_PTRS wxBorderArea(this);

  if (width < 0 || height < 0)
    Fit(); // WCH: need wxHorizontal and wxVertical for Fit(direction)

  {
    wxWindow *p;
    p = GetParent();
    if (p->IsHidden())
      DoShow(FALSE);
  }
  if (style & wxINVISIBLE)
    wxItem::Show(FALSE);
  InitInternalGray();
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxRadioBox::~wxRadioBox(void)
{
  cRadioButtons->Clear();
}


//-----------------------------------------------------------------------------
char* wxRadioBox::GetLabel(void)
{
  return (cRadioTitle ? cRadioTitle->GetLabel() : NULL);
}

//-----------------------------------------------------------------------------
void wxRadioBox::SetLabel(char* label)
{
  if (cRadioTitle) cRadioTitle->SetLabel(label);
}

//-----------------------------------------------------------------------------
char* wxRadioBox::GetLabel(int item)
{
  char* result = NULL;
  int numberItems;
  wxNode* node;
  wxRadioButton *radioButton;

  numberItems = cRadioButtons->Number();
  if (0 <= item && item < numberItems) {
    node = cRadioButtons->Nth(item);
    radioButton = (wxRadioButton*)node->Data();
    result = radioButton->GetLabel();
  }
  return result;
}

//-----------------------------------------------------------------------------
void wxRadioBox::SetLabel(int item, char* label)
{
  int numberItems;
  wxNode* node;
  wxRadioButton* radioButton;

  numberItems = cRadioButtons->Number();
  
  if (0 <= item && item < numberItems) {
    node = cRadioButtons->Nth(item);
    radioButton = (wxRadioButton*)node->Data();
    radioButton->SetLabel(label);
  }
}

//-----------------------------------------------------------------------------
void wxRadioBox::SetLabel(int item, wxBitmap* bitmap)
{
  int numberItems;
  wxNode* node;
  wxRadioButton* radioButton;

  numberItems = cRadioButtons->Number();

  if (0 <= item && item < numberItems) {
    node = cRadioButtons->Nth(item);
    radioButton = (wxRadioButton*)node->Data();
    radioButton->SetLabel(bitmap);
  }
}

//-----------------------------------------------------------------------------
void wxRadioBox::SetSelection(int N)
{
  int numberItems;
  wxNode* selectedNode;
  wxRadioButton* selectedRadioButton;
  wxNode* node;
  wxRadioButton* radioButton;

  numberItems = cRadioButtons->Number();

  if (-1 <= N && N < numberItems) {
    if (selected != N) {
      if (0 <= selected && selected < numberItems) {
	selectedNode = cRadioButtons->Nth(selected);
	selectedRadioButton = (wxRadioButton*)selectedNode->Data();
	selectedRadioButton->SetValue(FALSE);
      }
      
      if (N != -1) {
        node = cRadioButtons->Nth(N);
        radioButton = (wxRadioButton*)node->Data();
        radioButton->SetValue(TRUE);
      }
      
      selected = N;
    }
  }
}

//-----------------------------------------------------------------------------
// Get selection
//-----------------------------------------------------------------------------
int wxRadioBox::GetSelection(void)
{
  return selected;
}

//-----------------------------------------------------------------------------
void wxRadioBox::DoShow(Bool show)
{
  if (!CanShow(show)) return;

  if (show)
    wxWindow::DoShow(show);
  cRadioPanel->DoShow(show);	
  if (!show)
    wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------

void wxRadioBox::Enable(Bool enable)
{
  wxItem::Enable(enable);
}

void wxRadioBox::Enable(int item, Bool enable)
{
  int numberItems;
  wxNode* node;
  wxRadioButton* radioButton;

  numberItems = cRadioButtons->Number();
  if (0 <= item && item < numberItems) {
    node = cRadioButtons->Nth(item);
    radioButton = (wxRadioButton*)node->Data();
    radioButton->Enable(enable);
  }
}
 
//-----------------------------------------------------------------------------
void wxRadioBox::Show(int item, Bool show)
{
  int numberItems;
  wxNode* node;
  wxRadioButton* radioButton;

  numberItems = cRadioButtons->Number();
  
  if (0 <= item && item < numberItems) {
    node = cRadioButtons->Nth(item);
    radioButton = (wxRadioButton*)node->Data();
    radioButton->Show(show);
  }
}

//-----------------------------------------------------------------------------

int wxRadioBox::ButtonFocus(int n)
{
  if (n < 0) {
    if (focused_button < 0)
      return (-focused_button) - 1;
    else
      return focused_button - 1;
  } else {
    if (focused_button > 0) {
      OnKillFocus();
      focused_button = -(n + 1);
      OnSetFocus();
    } else {
      focused_button = -(n + 1);
      SetFocus();
    }
    return n;
  }
}


void wxRadioBox::MaybeMoveControls()
{
  cRadioPanel->MaybeMoveControls();
  if (cButtonHolder)
    cButtonHolder->MaybeMoveControls();
  if (cRadioTitle)
    cRadioTitle->MaybeMoveControls();
  wxWindow::MaybeMoveControls();
}

void wxRadioBox::OnClientAreaDSize(int dW, int dH, int dX, int dY)
{
  wxNode* node;
  wxWindow* rbut;

  node = cRadioButtons->First();
  while (node) {
    rbut = (wxWindow*)node->Data();
    rbut->OnClientAreaDSize(dW, dH, dX, dY);
    node = node->Next();
  }
  cRadioPanel->OnClientAreaDSize(dW, dH, dX, dY);
  if (cButtonHolder)
    cButtonHolder->OnClientAreaDSize(dW, dH, dX, dY);
  if (cRadioTitle)
    cRadioTitle->OnClientAreaDSize(dW, dH, dX, dY);

  wxWindow::OnClientAreaDSize(dW, dH, dX, dY);
}

//-----------------------------------------------------------------------------

void wxRadioBox::OnSetFocus()
{
  int n;
  wxNode *node;
  wxRadioButton *radioButton;

  n = cRadioButtons->Number();
  
  if (focused_button < 0) {
    focused_button = -focused_button;

    if (0 < focused_button && focused_button <= n) {
      node = cRadioButtons->Nth(focused_button - 1);
      radioButton = (wxRadioButton*)node->Data();
      radioButton->OnSetFocus();
    }
  }
}

void wxRadioBox::OnKillFocus()
{
  int n;
  wxNode *node;
  wxRadioButton *radioButton;

  n = cRadioButtons->Number();
  
  if (focused_button > 0) {
    if (0 < focused_button && focused_button <= n) {
      node = cRadioButtons->Nth(focused_button - 1);
      radioButton = (wxRadioButton*)node->Data();
      radioButton->OnKillFocus();
    }
    
    focused_button = -focused_button;
  }
}

