///////////////////////////////////////////////////////////////////////////////
// File:	wx_messg.cc
// Purpose:	Panel item message implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wx_item.h"
#include "wx_gdi.h"
#include "wx_messg.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_area.h"
#include "wxBorderArea.h"
#include "wxLabelArea.h"
#ifndef WX_CARBON
# include <QuickDraw.h>
# include <TextEdit.h>
#endif

extern FSSpec wx_app_spec;

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxMessage::wxMessage // Constructor (given parentArea)
(
 wxArea*		parentArea,
 char*		label,
 int 		x,
 int			y,
 long		style,
 wxFont         *_font,
 char*		windowName,
 WXTYPE		objectType
 ) :
  wxbMessage(parentArea, x, y, 0, 0, style, windowName)
{
  CreateWxMessage(label, _font);
}

//-----------------------------------------------------------------------------
wxMessage::wxMessage // Constructor (given parentArea, font)
(
 wxArea*		parentArea,
 char*		label,
 wxFont*		theFont,
 int 		x,
 int			y,
 long		style,
 char*		windowName,
 WXTYPE		objectType
 ) :
  wxbMessage(parentArea, x, y, 0, 0, style, windowName)
{
  CreateWxMessage(label, theFont);
}

//-----------------------------------------------------------------------------
wxMessage::wxMessage // Constructor (given parentPanel)
(
 wxPanel*	parentPanel,
 char*		label,
 int 		x,
 int			y,
 long		style,
 wxFont         *_font,
 char*		windowName,
 WXTYPE		objectType
 ) :
  wxbMessage(parentPanel, x, y, 0, 0, style, windowName)
{
  CreateWxMessage(label, _font);
}

//-----------------------------------------------------------------------------
wxMessage::wxMessage // Constructor (given parentPanel, font)
(
 wxPanel*	parentPanel,
 char*		label,
 wxFont*		theFont,
 int 		x,
 int			y,
 long		style,
 char*		windowName,
 WXTYPE		objectType
 ) :
  wxbMessage(parentPanel, x, y, 0, 0, style, windowName)
{
  CreateWxMessage(label, theFont);
}

wxMessage::wxMessage // Constructor (given parentPanel and bitmap)
(
 wxPanel*	parentPanel,
 wxBitmap*	bitmap,
 int 		x,
 int			y,
 long		style,
 wxFont         *_font,
 char*		windowName,
 WXTYPE		objectType
 ) :
  wxbMessage(parentPanel, x, y, 0, 0, style, windowName)
{
  SetEraser(wxCONTROL_BACKGROUND_BRUSH);
  if (bitmap->Ok() && (bitmap->selectedIntoDC >= 0)) {
    SetFont(_font, 13);
    sBitmap = bitmap;
    sBitmap->selectedIntoDC++;
    cMessage = NULL;
    if (cStyle & wxBORDER) new WXGC_PTRS wxBorderArea(this);
    SetClientSize(sBitmap->GetWidth(), sBitmap->GetHeight());
    CreatePaintControl();
    {
      wxWindow *p;
      p = GetParent();
      if (p->IsHidden())
	DoShow(FALSE);
    }
    if (style & wxINVISIBLE)
      Show(FALSE);
    InitInternalGray();
  } else
    CreateWxMessage("<bad-image>", _font);
}

static int icons_ready;
static IconRef msg_icons[3];

wxMessage::wxMessage // Constructor (given parentPanel and icon id)
(
 wxPanel*	parentPanel,
 int            iconID,
 int 		x,
 int			y,
 long		style,
 wxFont         *_font,
 char*		windowName,
 WXTYPE		objectType
 ) :
  wxbMessage(parentPanel, x, y, 0, 0, style, windowName)
{
  SetEraser(wxCONTROL_BACKGROUND_BRUSH);

  if (iconID) {
    if (!icons_ready) {
      SInt16 lbl;
      GetIconRefFromFile(&wx_app_spec, msg_icons + wxMSGICON_APP - 1, &lbl);
      GetIconRef(kOnSystemDisk, 'macs', kAlertCautionIcon, msg_icons + wxMSGICON_WARNING - 1);
      GetIconRef(kOnSystemDisk, 'macs', kAlertStopIcon, msg_icons + wxMSGICON_ERROR - 1);
      icons_ready = 1;
    }
  }

  switch (iconID) {
  case wxMSGICON_APP:
  case wxMSGICON_WARNING:
  case wxMSGICON_ERROR:
    break;
  default:
    CreateWxMessage("<bad-icon>", _font);
    return;
  }

  if (msg_icons[iconID - 1]) {
    icon_id = iconID;
    SetFont(_font, 13);
#ifdef OS_X    
    SetClientSize(64, 64);
#else
    SetClientSize(32, 32);
#endif
    CreatePaintControl();
    {
      wxWindow *p;
      p = GetParent();
      if (p->IsHidden())
	DoShow(FALSE);
    }
    if (style & wxINVISIBLE)
      Show(FALSE);
    InitInternalGray();
  } else
    CreateWxMessage("<icon-missing>", _font);
}


//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxMessage::~wxMessage(void)
{
  if (cMessage)
    cMessage = NULL;
  if (sBitmap) {
    --sBitmap->selectedIntoDC;
    sBitmap = NULL;
  }
  if (cParentArea->__type == wxTYPE_LABEL_AREA) {
    // CJC hack? clean up label area so it does point to us, since its 
    // about to go away.
    wxLabelArea *pa = (wxLabelArea *)cParentArea;
    pa->cLabelText = NULL;
  }
}

//=============================================================================
// Private methods
//=============================================================================

//-----------------------------------------------------------------------------
void wxMessage::CreateWxMessage(char* label, wxFont* theFont) // common constructor initialization
{
  double clientWidth;
  double clientHeight;

  SetFont(theFont, 13);

  if (cStyle & wxBORDER) new WXGC_PTRS wxBorderArea(this);
  sBitmap = NULL;
  cMessage = wxItemStripLabel(label);
	
  SetEraser(wxCONTROL_BACKGROUND_BRUSH);

  clientWidth = 20;
  clientHeight = 14;
  if (font) {
    font->GetTextExtent(cMessage, 0, -1, &clientWidth, &clientHeight, NULL, NULL, TRUE);
  }
  SetClientSize((int)floor(clientWidth) + 1, /* +1 still seems to be needed to avoid bad clipping */
                (int)floor(clientHeight));
	
  CreatePaintControl();

  {
    wxWindow *p;
    p = GetParent();
    if (p->IsHidden())
      DoShow(FALSE);
  }
  if (cStyle & wxINVISIBLE)
    Show(FALSE);

  InitInternalGray();
}


//-----------------------------------------------------------------------------
char* wxMessage::GetLabel(void)
{
  if (cMessage) {
    strcpy(wxBuffer, cMessage);
    return wxBuffer;
  } else
    return "";
}

//-----------------------------------------------------------------------------
void wxMessage::SetLabel(wxBitmap *bitmap)
{
  if (!sBitmap || !bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return;

  --sBitmap->selectedIntoDC;
  sBitmap = bitmap;
  sBitmap->selectedIntoDC++;
  
  Refresh();
}

//-----------------------------------------------------------------------------
void wxMessage::SetLabel(char* label)
{
  if (sBitmap || icon_id) return;
  {
    char *s;
    s = macCopyString0(wxItemStripLabel(label));
    cMessage = s;
  }
  if (!cHidden) {
    Refresh();
    FlushDisplay();
  }
}

//-----------------------------------------------------------------------------
void wxMessage::Paint(void)
{
  if (cHidden) return;

  {
    int clientWidth, clientHeight;

    GetClientSize(&clientWidth, &clientHeight);

    if (sBitmap) {
      sBitmap->DrawMac();
    } else if (icon_id) {
      Rect r = { SetOriginY, SetOriginX, 
		 SetOriginY + clientHeight, SetOriginX + clientWidth };
      PlotIconRef(&r, kAlignNone, kTransformNone, 0 /* kIconServicesDefaultUsageFlags */, msg_icons[icon_id - 1]);
#ifdef OS_X
      if ((icon_id != wxMSGICON_APP) && msg_icons[wxMSGICON_APP - 1]) {
	/* Add app badge: */
	r.left += 32;
	r.top += 32;
	PlotIconRef(&r, kAlignNone, kTransformNone, 0, msg_icons[wxMSGICON_APP - 1]);
      }
#endif
    } else if (font && (font != wxNORMAL_FONT)) {
      FontInfo fontInfo;
      ::GetFontInfo(&fontInfo);
      MoveTo(SetOriginX, fontInfo.ascent + SetOriginY); // move pen to start drawing text
      
      wxDrawUnicodeText(cMessage, 0, -1, 0);
    } else {
      Rect r = { SetOriginY, SetOriginX, 
		 SetOriginY + clientHeight, SetOriginX + clientWidth };
      CFStringRef str;


      str = wxCFString(cMessage);

      DrawThemeTextBox(str, kThemeSystemFont, kThemeStateActive,
		       0, &r, teJustLeft, NULL);

      CFRelease(str);
    }
  }
}

//-----------------------------------------------------------------------------
void wxMessage::DoShow(Bool show) 
{
  wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------
void wxMessage::ChangeToGray(Bool gray) 
{
  Refresh();
  wxWindow::ChangeToGray(gray);
}


void wxMessage::OnEvent(wxMouseEvent *event)
{
  if (MaybeMetalDrag(event)) 
    return;
  wxbMessage::OnEvent(event);
}

//-----------------------------------------------------------------------------
Bool wxMessage::AcceptsExplicitFocus()
{
  return FALSE;
}
