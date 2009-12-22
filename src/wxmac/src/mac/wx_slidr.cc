/*
 * File:	wx_slidr.cc
 * Purpose:	Panel item slider implementation (Macintosh version)
 * Author:	Cecil Coupe
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
 */

#include "common.h"
#include "wx_messg.h"
#include "wx_utils.h"
#include "wx_slidr.h"
#include "wx_het.h"

// Slider
/* 
   For wxMac a wxSlider contains
   1. A scroll control (horizontal)
   2. A wxLabelArea for the Label/Title
   3. a Rect for displaying the current value

*/

#define KDEFAULTW  60	// number pixels wide for a default scroll control
#ifdef WX_CARBON
# define KSCROLLH   27 // _includes_ PAD_TOP & PAD_BOTTOM
# define PAD_X 2 // these are all assuming a horizontal configuration.
# define PAD_TOP 3
# define PAD_BOTTOM 5
#else
# define KSCROLLH   16	// height of a mac scrollbar control
# define PAD_X 0
# define PAD_TOP 0
# define PAD_BOTTOM 0
#endif
#define VSP			3	// space between scrollbar and value
#define HSP			3	

static pascal void SCTrackActionProc(ControlHandle theControl, short thePart);
static ControlActionUPP SCTrackActionProcUPP = NewControlActionUPP(SCTrackActionProc);

void wxSlider::InsetSliderRect(Rect *r) 
{
  r->left += padLeft;
  r->top += padTop;
  r->right -= padRight;
  r->bottom -= padBottom;
}

wxSlider::wxSlider(wxPanel *panel, wxFunction func, char *label, int value,
		   int min_value, int max_value, int width, int x, int y,
		   long style, wxFont *_font, char *name, WXTYPE objectType
		   ):
  wxbSlider(panel, func, label, value, min_value, max_value, width, x, y, style, name)
{
  SetFont(_font, 13);
  Create(panel, func, label, value, min_value, max_value, width, x, y, style, name);
}

Bool wxSlider::Create(wxPanel *panel, wxFunction func, char *label, int value,
		      int min_value, int max_value, int width, int x, int y,
		      long style, char *name)
{
  double fWidth;
  double fHeight;
  double fDescent;
  double fLeading;
  int	lblh=0;
  int lblw=0;
  int vwid, vhgt, hsp, vsp;
  int adjust = 0;
  Rect r;
  CGrafPtr theMacGrafPort;
  
  windowStyle = style;
  window_parent = panel;
  
  label = wxItemStripLabel(label);

  Callback(func);
  SetCurrentDC();
  theMacGrafPort = cMacDC->macGrafPort();
	
  s_min = min_value;
  s_max = max_value;
  // page_size calculated 
  page_size = (s_max - s_min) / 15; // 15 == size of thumb ?
  if (page_size < 2)
    page_size = 2;

  if (label) {
    GetTextExtent(label, &fWidth, &fHeight, &fDescent, &fLeading, font);
    lblh = (int)fHeight;
    lblw = (int)fWidth;
  }
  if (style & (wxHORIZONTAL << 2)) {
    vwid = 0;
    vhgt = 0;
    hsp = 0;
    vsp = 0;
  } else {
    // evidently, the number in the value box should never be wider than the string "258".
    // this looks utterly ridiculous to me... perhaps there's a method to the madness?
    GetTextExtent("258", &fWidth, &fHeight, &fDescent, &fLeading, font);
    vwid = (int)fWidth;
    vhgt = (int)fHeight;
    hsp = HSP;
    vsp = VSP;
  }
    
  controlRect.top = controlRect.left = 0;
	
  if (style & wxVERTICAL) {
    padLeft = PAD_BOTTOM;
    padRight = PAD_TOP;
    padTop = padBottom = PAD_X;
    if (width < 0)
      cWindowHeight = KDEFAULTW + (padTop + padBottom) + ((labelPosition == wxVERTICAL) ? lblh + VSP : 0);
    else
      cWindowHeight = width;
    cWindowWidth = vwid + KSCROLLH + hsp + ((labelPosition == wxVERTICAL) ? 0 : lblw + HSP);
		
    controlRect.right = KSCROLLH;
    controlRect.bottom = cWindowHeight - ((labelPosition == wxVERTICAL) ? lblh + VSP : 0);
		
    valueRect.left = cWindowWidth - vwid + 1;
    valueRect.top = (cWindowHeight - vhgt) / 2;
    adjust = -1;
  } else {
    padLeft = padRight = PAD_X;
    padTop = PAD_TOP;
    padBottom = PAD_BOTTOM;
    if (width < 0)
      cWindowWidth = KDEFAULTW + (padLeft + padRight) + ((labelPosition == wxHORIZONTAL) ? lblw + HSP : 0);
    else
      cWindowWidth = width;
    cWindowHeight = vhgt + KSCROLLH + vsp + ((labelPosition == wxVERTICAL) ? lblh + VSP : 0);
		
    controlRect.right = cWindowWidth - ((labelPosition == wxVERTICAL) ? 0 : lblw + HSP);
    controlRect.bottom = KSCROLLH;
		
    valueRect.top = cWindowHeight - vhgt;
    valueRect.left = (cWindowWidth - vwid) / 2;
  }

  if (style & (wxHORIZONTAL << 2)) {
    valueRect.bottom = valueRect.top = 0;
    valueRect.right = valueRect.left = 0;
  } else {
    valueRect.bottom = valueRect.top + vhgt;
    valueRect.right = valueRect.left + vwid + adjust;
  }
  valuebase = (int)fDescent;
    
  r = controlRect;    
  OffsetRect(&r,SetOriginX,SetOriginY);
#ifdef WX_CARBON    
  InsetSliderRect(&r);
#endif    
  if (CreateSliderControl (GetWindowFromPort(theMacGrafPort), &r,
			   value, min_value, max_value, 
			   kControlSliderPointsDownOrRight,
			   0,
			   TRUE,
			   NULL,
			   &cMacControl))
    cMacControl = NULL;
      
  CheckMemOK(cMacControl);

  {
    void *rc;
    rc = WRAP_SAFEREF(this);
    SetControlReference(cMacControl, (long)rc);
  }

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
    }
  else
    cTitle = NULL;
         
  ::EmbedControl(cMacControl, GetRootControl());
  IgnoreKeyboardEvents();

  if (!(windowStyle & (wxHORIZONTAL << 2)))
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
	
  return TRUE;
}

// ------------ Destructor ----------------------------------------
wxSlider::~wxSlider(void)
{
  void *rc;

  rc = (void *)GetControlReference(cMacControl);
  FREE_SAFEREF(rc);

  ::DisposeControl(cMacControl);
}


//------------ Event Handling --------------------------------------
void wxSlider::Paint(void)
{
  if (cHidden) return;

  if (!(windowStyle & (wxHORIZONTAL << 2))) {
    Rect r = valueRect;
    char t[8];
    CFStringRef str;

    OffsetRect(&r,SetOriginX,SetOriginY);
    {
      int val;
      val = ::GetControlValue(cMacControl);
      sprintf(t,"%d",val);
    }

    if (font && (font != wxNORMAL_FONT)) {
      double w, h, d, dx;

      font->GetTextExtent(t, 0, -1, &w, &h, &d, NULL, FALSE);
      dx = ((r.right - r.left) - w) / 2;
      
      MoveTo((short)floor(r.left + dx), (short)floor(r.top + (h - d)));

      wxDrawUnicodeText(t, 0, -1, 0);
    } else {
      str = wxCFString(t);
      DrawThemeTextBox(str, kThemeSystemFont, kThemeStateActive, 0, &r, teJustCenter, 0);
      CFRelease(str);
    }
  }
	
  wxWindow::Paint();
}

//-----------------------------------------------------------------------------
void wxSlider::DoShow(Bool show)
{
  if (!CanShow(show)) return;

  SetCurrentDC();

  if (show) {
    ::ShowControl(cMacControl);
  } else {
    ::HideControl(cMacControl);
  }

  if (!show && cTitle)
    cTitle->DoShow(show);
		
  wxWindow::DoShow(show);

  if (show && cTitle)
    cTitle->DoShow(show);
}

void wxSlider::OnClientAreaDSize(int dW, int dH, int dX, int dY)
{
  if (dW || dH) {	
    int clientWidth, clientHeight;
    int vwid, vhgt;
    Rect r;
    wxArea *carea;

    carea = ClientArea();
    clientWidth = carea->Width();
    clientHeight= carea->Height();

    vwid = valueRect.right - valueRect.left;
    vhgt = valueRect.bottom - valueRect.top;
			
    if (windowStyle & wxVERTICAL) {
      controlRect.bottom = clientHeight;
      r = controlRect;
      InsetSliderRect(&r);
			
      ::SizeControl(cMacControl, r.right - r.left, r.bottom - r.top);
      valueRect.top = (clientHeight - vhgt) / 2;
      valueRect.bottom = valueRect.top + vhgt;
      valueRect.left = KSCROLLH + HSP;
      valueRect.right = valueRect.left + vwid;
    } else {
      controlRect.right = clientWidth;
      r = controlRect;
      InsetSliderRect(&r);
      ::SizeControl(cMacControl, r.right - r.left, r.bottom - r.top);
      valueRect.left = (clientWidth - vwid) / 2;
      valueRect.right = valueRect.left + vwid;
      valueRect.top = KSCROLLH + VSP;
      valueRect.bottom = valueRect.top + vhgt;
    }
  }

  if (cPaintControl) {
    if (dW || dH) {
      ::SizeControl(cPaintControl, 
		    (valueRect.right - valueRect.left),
		    (valueRect.bottom - valueRect.top));
      if (!dX && !dY)
	MaybeMoveControls();
    }
  }

  if (dX || dY) {	
    MaybeMoveControls();
  }

  if (cTitle)
    cTitle->cLabelText->OnClientAreaDSize(dW, dH, dX, dY);
}

void wxSlider::MaybeMoveControls()
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

    ::MoveControl(cMacControl, x + padLeft + mx, y + padTop + my);

    if (cPaintControl)
      ::MoveControl(cPaintControl, valueRect.left + x + mx, valueRect.top + y + my);
  }

  if (cTitle)
    cTitle->cLabelText->MaybeMoveControls();
}

#define max(x, y) ((x > y) ? x : y)
#define min(x, y) ((x > y) ? y : x)

void wxSlider::OnEvent(wxMouseEvent *event)
{
  if (event->leftDown) {
    int startH, startV;
    Point pt;
    int part;

    SetCurrentDC();
    event->Position(&startH, &startV); // client c.s.
    pt.v = startV - padTop;
    pt.h = startH - padLeft;
    part = ::TestControl(cMacControl, pt);
    if (StillDown()) {
      if (part) {
	wxTracking();
	if (part == kControlIndicatorPart) {
	  if (::wxHETTrackControl(cMacControl, pt, SCTrackActionProcUPP))
	    TrackPart(part);
	} else {
	  ::wxHETTrackControl(cMacControl, pt, SCTrackActionProcUPP);
	}
      }
    }
  }
}

void wxSlider::TrackPart(int part)
{
  int oldval;
  wxCommandEvent *commandEvent;

  /* Must queue callbacks only; don't run Scheme code directly: */

  oldval = ::GetControlValue(cMacControl);

  switch (part) {
  case kControlUpButtonPart:
    {
      ::SetControlValue(cMacControl, max(s_min, oldval-1));
    }
  break;
  case kControlDownButtonPart:
    {
      ::SetControlValue(cMacControl, min(s_max, oldval+1));
    }
  break;
  case kControlPageUpPart:
    {
      ::SetControlValue(cMacControl, max(s_min, oldval-page_size));
    }
  break;
  case kControlPageDownPart:
    {
      ::SetControlValue(cMacControl, min(s_max, oldval+page_size));
    }
  break;
  case kControlIndicatorPart:
    break;
  } // end switch
	
  if (!(windowStyle & (wxHORIZONTAL << 2))) {
    if (cPaintControl)
      HIViewSetNeedsDisplay(cPaintControl, TRUE);
  }
 
  commandEvent = new WXGC_PTRS wxCommandEvent(wxEVENT_TYPE_SLIDER_COMMAND);

  /* Must queue callbacks only: */
  ProcessCommand(commandEvent);
	
  // So update happens correctly as we return...
  SetCurrentDC();
}

// Update the Value rect as the thumb is dragged around
pascal void SCTrackActionProc(ControlHandle theControl, short thePart)
{
  wxSlider*	slider;
  void *rc;
  rc = (void *)GetControlReference(theControl);
  slider = (wxSlider*)GET_SAFEREF(rc);
  if (slider) {
    /* Queue callbacks only; don't run Scheme code directly: */
    slider->TrackPart(thePart);
    
    while (wxHETYield(slider, NULL, NULL) && StillDown()) { }
  }

#ifdef MZ_PRECISE_GC
# ifndef GC_STACK_CALLEE_RESTORE
  /* Restore variable stack. */
  GC_variable_stack = (void **)__gc_var_stack__[0];
# endif
#endif
}

// --------------------- Client API ---------------------
int wxSlider::GetValue(void)
{
  return GetControlValue(cMacControl);
}

void wxSlider::SetValue(int value)
{
  SetCurrentDC();
  ::SetControlValue(cMacControl, value);
  if (!(windowStyle & (wxHORIZONTAL << 2))) {
    Refresh();
  }
}

char* wxSlider::GetLabel(void)
{
  return (cTitle ? cTitle->GetLabel() : NULL);
}

void wxSlider::SetLabel(char *label)
{
  if (cTitle) cTitle->SetLabel(label);	
}

void wxSlider::InternalGray(int gray_amt)
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

void wxSlider::OnSetFocus()
{
  wxItem::OnSetFocus();
  if (cTitle)
    cTitle->cLabelText->Refresh();
}

void wxSlider::OnKillFocus()
{
  wxItem::OnKillFocus();
  if (cTitle)
    cTitle->cLabelText->Refresh();
}
