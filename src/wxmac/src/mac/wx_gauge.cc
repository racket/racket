/*
 * File:	wx_gauge.cc
 * Purpose:	Panel item gauge implementation (Macintosh version)
 * Author:	Cecil Coupe
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
 */

#include "common.h"
#include "wx_utils.h"
#include "wx_gauge.h"
#include "wx_messg.h"

// Slider
/* 
   For wxMac a wxSlider contains
   1. A gauge control (horizontal)
   2. A wxLabelArea for the Label/Title
   3. a Rect for displaying the current value

   */

#define KDEFAULTW  60	// number pixels wide for a default scroll control
#define KGAUGEH    12	
#define VSP			3	// space between scrollbar and value
#define HSP			3	

wxGauge::wxGauge(wxPanel *panel, char *label, int _range, int x, int y,
		 int width, int height, long style, wxFont *_font, 
		 char *name) :
		 wxbGauge(panel, label, _range, x, y, width, height, style, name)
{
  double fWidth;
  double fHeight;
  double fDescent;
  double fLeading;
  int lblh = 0;
  int lblw = 0;

  SetFont(_font, 13);
  
  SetCurrentDC();

  range = _range;
  value = 0;
  if (range < 1)
    range = 1;

  if (label)
    label = wxItemStripLabel(label);
  
  if (label) {
    GetTextExtent(label, &fWidth, &fHeight, &fDescent, &fLeading, font);
    lblh = (int)fHeight;
    lblw = (int)fWidth;
  }

  valueRect.top = valueRect.left = 0;
  
  if (style & wxVERTICAL) {
    if (height < 0)
      cWindowHeight = KDEFAULTW + ((labelPosition == wxVERTICAL) ? lblh + VSP : 0);
    else
      cWindowHeight = height;
    cWindowWidth = KGAUGEH + ((labelPosition == wxVERTICAL) ? 0 : lblw + HSP);
    
    valueRect.right = KGAUGEH;
    valueRect.bottom = cWindowHeight - ((labelPosition == wxVERTICAL) ? lblh + VSP : 0);
  } else {
    if (width < 0)
      cWindowWidth = KDEFAULTW + ((labelPosition == wxHORIZONTAL) ? lblw + HSP : 0);
    else
      cWindowWidth = width;
    cWindowHeight = KGAUGEH + ((labelPosition == wxVERTICAL) ? lblh + VSP : 0);
    
    valueRect.right = cWindowWidth - ((labelPosition == wxHORIZONTAL) ? lblw + HSP : 0);
    valueRect.bottom = KGAUGEH;
#ifdef WX_CARBON // for horizontal gauges, use the native control
    {
      OSErr err;
      Rect bounds = valueRect;
      
      OffsetRect(&bounds,SetOriginX,SetOriginY);
      err = CreateProgressBarControl(GetWindowFromPort(cMacDC->macGrafPort()),&bounds,
				     0,0,range,FALSE,&cMacControl);
      
      ::EmbedControl(cMacControl, GetRootControl());
    }
#endif
  }

  if (!cMacControl)
    CreatePaintControl();
  
  if (label) {
    if (labelPosition == wxVERTICAL) {
      if (cWindowWidth < lblw)
	cWindowWidth = lblw;
    } else {
      if (cWindowHeight < lblh)
	cWindowHeight = lblh;
    }
  }

  if (label)
    {
      cTitle = new WXGC_PTRS wxLabelArea(this, label, font,
					 labelPosition == wxVERTICAL ? wxTop : wxLeft);
    } else
      cTitle = NULL;
  
  {
    wxWindow *p;
    p = GetParent();
    if (p->IsHidden())
      DoShow(FALSE);
  }
  if (style & wxINVISIBLE)
    Show(FALSE);
  InitInternalGray();  
}

// ------------ Destructor ----------------------------------------
wxGauge::~wxGauge(void)
{
}


//------------ Event Handling --------------------------------------
void wxGauge::Paint(void)
{
  if (cHidden) return;

  SetCurrentDC();
  
  if (cMacControl) {
    /* Do nothing */
  } else {
    Rect s = valueRect;
    Rect r, w;
    long d;
    RGBColor save;

    OffsetRect(&s,SetOriginX,SetOriginY);
    FrameRect(&s);

    r = valueRect;
    InsetRect(&r, 1, 1);
    w = r;
    if (windowStyle & wxVERTICAL) 
      d = (valueRect.bottom - valueRect.top);
    else
      d = (valueRect.right - valueRect.left);
    if (value < range)
      d = (d * value) / range;
    if (windowStyle & wxVERTICAL) {
      r.top = r.bottom - d;
      w.bottom = r.top;
    } else {
      r.right = r.left + d;
      w.left = r.right;
    }

    GetForeColor(&save);

    if (value) {
      if (cColour) {
	RGBColor c;
	c.red = 66 << 8;
	c.green = 66 << 8;
	c.blue = 66 << 8;
	RGBForeColor(&c);
      }
      OffsetRect(&r,SetOriginX,SetOriginY);
      PaintRect(&r);
    }
    
    if (value < range) {
      if (cColour) {
	RGBColor c;
	c.red = 204 << 8;
	c.green = 204 << 8;
	c.blue = 0xFFFF;
	RGBForeColor(&c);
      } else
	ForeColor(whiteColor);
      OffsetRect(&w,SetOriginX,SetOriginY);
      PaintRect(&w);
    }

    RGBForeColor(&save);
  }

  wxWindow::Paint();
}

//-----------------------------------------------------------------------------
void wxGauge::DoShow(Bool show)
{
  if (!CanShow(show)) return;

  if (!show && cTitle)
    cTitle->DoShow(show);

  if (cMacControl) {
    if (show) {
      ShowControl(cMacControl);
    } else {
      HideControl(cMacControl);
    }
  }
  wxWindow::DoShow(show);

  if (show && cTitle)
    cTitle->DoShow(show);
}

void wxGauge::OnClientAreaDSize(int dW, int dH, int dX, int dY)
{
  SetCurrentDC();

  if (dW || dH) {	
    int clientWidth, clientHeight, vwid, vhgt;
    wxArea *carea;

    carea = ClientArea();

    clientWidth = carea->Width();
    clientHeight= carea->Height();

    vwid = valueRect.right - valueRect.left;
    vhgt = valueRect.bottom - valueRect.top;
    
    if (windowStyle & wxVERTICAL) {
      // the wid can't change
      valueRect.left = (clientWidth - vwid) / 2;
      valueRect.right = valueRect.left + vwid;
      valueRect.bottom = clientHeight;
    } else {
      // the hgt can't change
      valueRect.top = (clientHeight - vhgt) / 2;
      valueRect.bottom = valueRect.top + vhgt;
      valueRect.right = clientWidth;
    }
    if (cMacControl) {
      SizeControl(cMacControl,valueRect.right - valueRect.left, valueRect.bottom - valueRect.top);
    }
  }

  if (dX || dY) {
    if (cMacControl) {
      MaybeMoveControls();
    }
  }

  if (cTitle)
    cTitle->cLabelText->OnClientAreaDSize(dW, dH, dX, dY);

  wxWindow::OnClientAreaDSize(dW, dH, dX, dY);
}

void wxGauge::MaybeMoveControls()
{
  {
    int x, y, mx, my;
    wxArea *c;
    wxMargin margin;

    GetWinOrigin(&x, &y);
    c = ClientArea();
    margin = c->Margin(this);
    mx = margin.Offset(wxLeft);
    my = margin.Offset(wxTop);

    MoveControl(cMacControl, x + valueRect.left + mx, y + valueRect.top + my);
  }

  if (cTitle)
    cTitle->cLabelText->MaybeMoveControls();

  wxWindow::MaybeMoveControls();
}

// --------------------- Client API ---------------------

void wxGauge::SetValue(int v)
{
  value = v;
  if (value > range)
    value = range;
  else if (value < 0)
    value = 0;
  if (cMacControl) {
    SetControlValue(cMacControl,value);
  }
  Paint();
  Refresh(); /* in case an update is in progress */
}

void wxGauge::SetRange(int v)
{
  range = v;
  if (range < 1)
    range = 1;
  if (value > range)
    value = range;
  if (cMacControl) {
    SetControlMaximum(cMacControl,range);
    SetControlValue(cMacControl,value);
  }
  Paint();
  Refresh(); /* in case an update is in progress */
}

char* wxGauge::GetLabel(void)
{
  return (cTitle ? cTitle->GetLabel() : NULL);
}

void wxGauge::SetLabel(char *label)
{
  if (cTitle) cTitle->SetLabel(label);
  
}

void wxGauge::InternalGray(int gray_amt)
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

//-----------------------------------------------------------------------------
Bool wxGauge::AcceptsExplicitFocus()
{
  return FALSE;
}
