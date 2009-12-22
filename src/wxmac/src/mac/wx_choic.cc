/*
 * File:        wx_choic.cc
 * Purpose:     Choice item implementation (Mac version)
 * Author:      Julian Smart/Cecil Coupe
 * Created:     1993
 * Updated:	April 1995
 *   July 22, 1995 - First Mac version - Cecil Coupe
 *
 * Copyright:   (c) 2004-2010 PLT Scheme Inc.
 * Copyright:   (c) 1995, AIAI, University of Edinburgh
 */


#ifdef __GNUG__
#ifdef __GNUG__
#pragma implementation
#endif
#ifdef __GNUG__
#pragma implementation "wx_choic.h"
#endif
#endif

#include "common.h"
#include "wx_utils.h"
#include "wx_choic.h"
#include "wx_menu.h"
#include "wx_messg.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#ifndef WX_CARBON
# include <Controls.h>
#endif

// Because I never get this right and t,l,b,r makes sense to me - CJC
//
#define SetBounds(rect, top, left, bottom, right) ::SetRect(rect, left, top, right, bottom)
#define VSLOP 0
#define HSLOP 4
#define PAD_Y 5
#define PAD_X 2
#define MSPACEY 1

extern int wx_leave_all_input_alone;

static char *protect_leading_hyphen(char *s)
{
  if (s[0] == '-') {
    char *t;
    int len;
    len = strlen(s);
    t = new WXGC_ATOMIC char[len + 2];
    memcpy(t + 1, s, len + 1);
    t[0] = ' ';
    return t;
  } else
    return s;
}

wxChoice::wxChoice()
{
  /* dummy - not used */
}

wxChoice::wxChoice (wxPanel * panel, wxFunction func, char *Title,
		    int x, int y, int width, int height, int N, char **Choices,
		    long style,  wxFont *_font, char *name
		    ):
		    wxbChoice (panel, func, Title, x, y, width, height, N, Choices, style, name)
{
  SetFont(_font, 13);
  Create (panel, func, Title, x, y, width, height, N, Choices, style, name);
}

#define max(x, y) ((x > y) ? x : y)

Bool wxChoice::
Create (wxPanel * panel, wxFunction func, char *Title,
	int x, int y, int width, int height, int N, char **Choices,
	long style, char *name)
{
  double fWidth, fHeight, fDescent, fLeading;
  int lblw, lblh;
  int maxdfltw;
  int maxdflth;
  int n,w,h;
  OSErr err;
  Rect r = {0,0,0,0};
  SInt16 baselineOffset; // ignored
  
  windowStyle = style;
  Callback (func);
  padLeft = padRight = PAD_X;
  padTop = padBottom = PAD_Y;
  
  SetCurrentMacDC();

  if (Title)
    Title = wxItemStripLabel(Title);

  fWidth = 50.0;
  fHeight = 12.0;
  fDescent = 0.0;
  fLeading = 0.0;
  labelbase = 9;
  if (Title) {
    int n;
    char *naya_s;
    GetTextExtent(Title, &fWidth, &fHeight, &fDescent, &fLeading, font);
    if (fHeight < 12) fHeight = 12; 
    n = strlen(Title);
    naya_s = new WXGC_ATOMIC char[n+1];
    sTitle = (StringPtr)naya_s;
    sTitle[0] = n;
    memcpy(&sTitle[1], Title, n);
    labelbase = (int)(fDescent + fLeading);
  } else  {
    sTitle = NULL;
    fWidth = fHeight = 0;
  }
  lblw = (int)(fWidth + (Title ? HSLOP : 0));
  lblh = (int)(fHeight + (Title ? 2 : 0));
  
  // Build the menu
  PopUpID = wxMenu::gMenuIdCounter++;
  hDynMenu = NewMenu(PopUpID, "\p");
  CheckMemOK(hDynMenu);
  for (n = 0; n < N; n++) {
    // add a dummy label (to prevent interpretation of control characters)
    ::AppendMenu(hDynMenu, "\ptemp");
    // Now set the real label:
    {
      CFStringRef ct;
      char *s;
      s = protect_leading_hyphen(Choices[n]);
      ct = wxCFString(s);
      ::SetMenuItemTextWithCFString(hDynMenu, n + 1, ct);
      CFRelease(ct);
    }
  }
  no_strings = N;

  // First, create the control with a bogus rectangle;
  err = ::CreatePopupButtonControl(GetWindowFromPort(cMacDC->macGrafPort()), &r, NULL, -12345,
				   FALSE, 0, 0, normal, &cMacControl);
  err = ::SetControlData(cMacControl, kControlNoPart, kControlPopupButtonOwnedMenuRefTag,
			 sizeof(MenuRef), (void *)(&hDynMenu));
  ::SetControlMinimum(cMacControl, 1);
  ::SetControlMaximum(cMacControl, no_strings);

  wxSetControlFont(cMacControl, font);

  // Now, ignore the font data and let the control find the "best" size 
  err = ::GetBestControlRect(cMacControl, &r, &baselineOffset);
  maxdfltw = r.right - r.left;
  maxdflth = r.bottom - r.top;

  // compute the Rects that contain everything.
  // note valuebase and labelbase are changed from font descents
  // to number of pixels to substract from the rect bottom.
  if (labelPosition == wxVERTICAL) {
    w = max(lblw, maxdfltw) + padLeft + padRight;
    h = maxdflth + lblh + MSPACEY + padTop + padBottom;
    padTop += lblh;
  } else {
    h = max(lblh, maxdflth) + padTop + padBottom;
    w = maxdfltw + lblw + padLeft + padRight;
    padLeft += lblw;
  }

  if (width < 0 && height < 0) {
    // use the sizes we just calced
    cWindowWidth = w;
    cWindowHeight = h;
  } else {
    OnClientAreaDSize((width == -1 ? 0 : width),
		      (height == -1 ? 0 : height), 
		      (x == -1 ? 0 : x), 
		      (y == -1 ? 0 : y));
  }

  SetSelection(0);

  if (Title) {
    cTitle = new WXGC_PTRS wxLabelArea(this, Title, font,
				       ((labelPosition == wxVERTICAL) ? wxTop : wxLeft),
				       0,
				       ((labelPosition == wxVERTICAL) ? 0 : ((maxdflth - lblh) / 2) + PAD_Y + 1));
  } else
    cTitle = NULL;

  ::EmbedControl(cMacControl, GetRootControl());
  IgnoreKeyboardEvents();

  {
    wxWindow *p;
    p = GetParent();
    if (p->IsHidden())
      DoShow(FALSE);
  }
  if (style & wxINVISIBLE)
    Show(FALSE);
  InitInternalGray();

  return TRUE;
}

int wxChoice::Number(void)
{
  return no_strings;
}

wxChoice::~wxChoice (void)
{
  if (cMacControl) {
    ::DisposeControl(cMacControl);
  }
  cMacControl = NULL;
}

// ---------Draw the Choice Control -----------
void wxChoice::DrawChoice(Bool active)
{
}

void wxChoice::ChangeToGray(Bool gray)
{
  wxItem::ChangeToGray(gray);
  Refresh();
}

// --------- Event Handling -------------------
void wxChoice::Paint(void)
{
  wxWindow::Paint();
}

// Resize and/or Move the Control
void wxChoice::OnClientAreaDSize(int dW, int dH, int dX, int dY)
{
  int clientWidth, clientHeight;

  GetClientSize(&clientWidth, &clientHeight);
  ::SizeControl(cMacControl, clientWidth - HSLOP, clientHeight);
  MaybeMoveControls();

  if (cTitle)
    cTitle->cLabelText->OnClientAreaDSize(dW, dH, dX, dY);

  wxWindow::OnClientAreaDSize(dW, dH, dX, dY);
}

//-----------------------------------------------------------------------------
void wxChoice::OnChar(wxKeyEvent *event)
{
  int delta;

  switch (event->KeyCode()) {
  case WXK_UP:
    delta = -1;
    break;
  case WXK_DOWN:
    delta = 1;
    break;
  default:
    delta = 0;
    break;
  }

  if (delta) {
    int s;
    s = GetSelection();
    SetSelection(s + delta);
    if (s != GetSelection()) {
      wxCommandEvent *e;
      e = new WXGC_PTRS wxCommandEvent(wxEVENT_TYPE_CHOICE_COMMAND);
      ProcessCommand(e);
    }
  }
}

//-----------------------------------------------------------------------------
void wxChoice::DoShow(Bool show)
{
  if (!CanShow(show))
    return;

  if (!show && cTitle)
    cTitle->DoShow(show);
  if (show) {
    ::ShowControl(cMacControl);
  } else {
    ::HideControl(cMacControl);
  }

  wxWindow::DoShow(show);

  if (show && cTitle)
    cTitle->DoShow(show);
}

//-----------------------------------------------------------------------------


void wxChoice::OnEvent(wxMouseEvent *event) // mac platform only
{
  if (event->LeftDown() && (no_strings > 0))
    {
      int startH, startV;
      Point startPt;
      int trackResult;

      SetCurrentDC();
      
      event->Position(&startH, &startV); // client c.s.

      startPt.v = startV - PAD_X;
      startPt.h = startH - PAD_Y;

      wxTracking();
      wx_leave_all_input_alone++;
      trackResult = TrackControl(cMacControl,startPt,(ControlActionUPP)-1);
      --wx_leave_all_input_alone;
      if (trackResult) {
	wxCommandEvent *commandEvent;
	selection = GetControlValue(cMacControl);
	selection -= 1;
	commandEvent = new WXGC_PTRS wxCommandEvent(wxEVENT_TYPE_CHOICE_COMMAND);
	ProcessCommand(commandEvent);
      }
    }
}


// ------------ Methods available to user ------------

void wxChoice::Append (char *Item)
{
  ::InsertMenuItem(hDynMenu, "\ptemp", no_strings);
  {
    CFStringRef ct;
    char *s;
    s = protect_leading_hyphen(Item);
    ct = wxCFString(s);
    ::SetMenuItemTextWithCFString(hDynMenu, no_strings + 1, ct);
    CFRelease(ct);
  }
  no_strings++;
  ::SetControlMinimum(cMacControl,1);
  ::SetControlMaximum(cMacControl,no_strings);
  RefreshIfUpdating();

  /* this is needed, for some reason, when the menu item has focus */
  if (cTitle)
    cTitle->cLabelText->Refresh();
}

void wxChoice::Clear (void)
{
  int n;
  for (n = 0; n < no_strings; n++){
    ::DeleteMenuItem(hDynMenu, 1);
  }
  no_strings = 0;
  selection = 0;
  ::SetControlMinimum(cMacControl,0);
  ::SetControlMaximum(cMacControl,0);        
  RefreshIfUpdating();

  /* this is needed, for some reason, when the menu item has focus */
  if (cTitle)
    cTitle->cLabelText->Refresh();
}


int wxChoice::GetSelection (void)
{
  return selection;
}

void wxChoice::SetSelection (int n)
{
  if ((n < 0) || (n >= no_strings))
    return;

  ::SetControlValue(cMacControl,n+1);
  selection = n;

  /* this is needed, for some reason, when the menu item has focus */
  if (cTitle)
    cTitle->cLabelText->Refresh();
}

char* wxChoice::GetLabel(void)
{
  return (cTitle ? cTitle->GetLabel() : NULL);
}

void wxChoice::SetLabel(char *label)
{
  if (cTitle) cTitle->SetLabel(label);
}

void wxChoice::InternalGray(int gray_amt)
{
  if (cTitle) {
    wxLabelArea *la;
    wxWindow *w;
    la = (wxLabelArea *)cTitle;
    w = la->GetMessage();
    w->InternalGray(gray_amt);
  }
  wxItem::InternalGray(gray_amt);
}

void wxChoice::MaybeMoveControls()
{
  if (cTitle)
    cTitle->cLabelText->MaybeMoveControls();
  wxItem::MaybeMoveControls();
}

void wxChoice::OnSetFocus()
{
  wxItem::OnSetFocus();
  if (cTitle)
    cTitle->cLabelText->Refresh();
}

void wxChoice::OnKillFocus()
{
  wxItem::OnKillFocus();
  if (cTitle)
    cTitle->cLabelText->Refresh();
}
