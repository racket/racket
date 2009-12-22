///////////////////////////////////////////////////////////////////////////////
// File:	wx_gbox.cc
// Purpose:	Panel item tab choice implementation (Macintosh version)
// Author:	Matthew
// Created:	2002
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 2002, PLT
///////////////////////////////////////////////////////////////////////////////

#include "wx_gbox.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_area.h"
#include "wx_panel.h"

#define GBOX_EXTRA_SPACE 8
#define GBOX_EXTRA_H_SPACE 32

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxGroupBox::wxGroupBox(wxPanel *panel, char *label, int style, wxFont *_font)
 : wxItem (panel, -1, -1, -1, -1, style,  "group-box")
{
  CGrafPtr theMacGrafPort;
  Rect boundsRect = {0, 0, 10, 10};
  CFStringRef title;

  SetFont(_font, 11);
  
  SetCurrentMacDC();
  theMacGrafPort = cMacDC->macGrafPort();
  OffsetRect(&boundsRect, SetOriginX, SetOriginY);
  
  if (label)
    title = wxCFString(label);
  else
    title = NULL;

  cMacControl = NULL;
  CreateGroupBoxControl(GetWindowFromPort(theMacGrafPort), &boundsRect, title, TRUE, &cMacControl);

  wxSetControlFont(cMacControl, font);

  if (title)
    CFRelease(title);
   
  CheckMemOK(cMacControl);

#if 0
  /* #^%$^&!!! GetBestControlRect doesn't work for group widgets.
     And why should it? That would be entriely too helpful. */
  Rect r = {0,0,0,0};
  SInt16 baselineOffset; // ignored
  OSErr err;
  err = ::GetBestControlRect(cMacControl, &r, &baselineOffset);
  cWindowWidth = r.right - r.left;
  cWindowHeight = (r.bottom - r.top) + GBOX_EXTRA_SPACE;
#else
  {
    double x, y;
    wxFont *bold;
    bold = new WXGC_PTRS wxFont(font->GetPointSize(), font->GetFontId(), font->GetStyle(), wxBOLD, 0);
    bold->GetTextExtent(wxItemStripLabel(label), 0, -1, &x, &y, NULL, NULL, TRUE, FALSE);
    cWindowWidth = (int)x + GBOX_EXTRA_H_SPACE;
    cWindowHeight = (int)y + GBOX_EXTRA_SPACE;
  }
#endif

  orig_height = cWindowHeight;

  phantom_height = -1;

  ::SizeControl(cMacControl, cWindowWidth, cWindowHeight);

  ::EmbedControl(cMacControl, GetRootControl());
  
  {
    wxWindow*p;
    p = GetParent();
    if (p->IsHidden())
      DoShow(FALSE);
  }
  if (style & wxINVISIBLE)
    Show(FALSE);
  InitInternalGray();
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxGroupBox::~wxGroupBox(void)
{
  if (cMacControl) {
    ::DisposeControl(cMacControl);
    cMacControl = NULL;
  }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Sizing methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxGroupBox::OnClientAreaDSize(int dW, int dH, int dX, int dY) // mac platform only
{
  if (!cMacControl)
    return;
  
  SetCurrentDC();

  if (dW || dH) {
    wxWindow *parent;
    int clientWidth, clientHeight;
    int pClientWidth, pClientHeight;

    GetClientSize(&clientWidth, &clientHeight);

    if (phantom_height > 0) {
      pClientHeight = phantom_height;
      pClientWidth = 0;
    } else {
      parent = GetParent();
      parent->GetClientSize(&pClientWidth, &pClientHeight);
    }
    ::SizeControl(cMacControl, clientWidth - (padLeft + padRight), 
		  pClientHeight - (padTop + padBottom));
  }

  if (dX || dY) {
    MaybeMoveControls();
  }

  if (!cHidden && (dW || dH || dX || dY)) {
    Refresh();
  }
}

void wxGroupBox::MaybeMoveControls(void)
{
  wxItem::MaybeMoveControls();
}

void wxGroupBox::Refresh(void)
{
  wxWindow::Refresh();
}

void wxGroupBox::SetPhantomSize(int w, int h)
{
  if (phantom_height != h) {
    phantom_height = h;
    OnClientAreaDSize(0, 1, 0, 0);
  }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

void wxGroupBox::Paint(void)
{
  /* Shouldn't get here */
}

//-----------------------------------------------------------------------------
void wxGroupBox::DoShow(Bool show)
{
  if (!CanShow(show)) return;

  if (show) {
    ::ShowControl(cMacControl);
  } else {
    ::HideControl(cMacControl);
  }
  wxItem::DoShow(show);
}

void wxGroupBox::ChangeToGray(Bool gray)
{
  wxItem::ChangeToGray(gray);
}

void wxGroupBox::Activate(Bool on)
{
  wxItem::Activate(on);
}

char *wxGroupBox::GetLabel()
{
  return "group box"; /* not right, but I don't think anyone cares */
}

void wxGroupBox::SetLabel(char *label)
{
  if (cMacControl) {
    SetCurrentDC();
    {
      CFStringRef llabel;
      llabel = wxCFString(wxItemStripLabel(label));
      SetControlTitleWithCFString(cMacControl, llabel);
      CFRelease(llabel);

      Paint(); /* to paint custom control */
      Refresh(); /* in case an update is in progress */
    }
  }
}

void wxGroupBox::OnEvent(wxMouseEvent *event)
{
  if (MaybeMetalDrag(event)) 
    return;
  wxItem::OnEvent(event);
}

//-----------------------------------------------------------------------------
Bool wxGroupBox::AcceptsExplicitFocus()
{
  return FALSE;
}
