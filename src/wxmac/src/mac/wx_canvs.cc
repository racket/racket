///////////////////////////////////////////////////////////////////////////////
// File:	wx_canvs.cc
// Purpose:	wxCanvas implementation
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wx_canvs.h"
#include "wx_dccan.h"
#include "wx_utils.h"
#include "wx_area.h"
#include "wx_sbar.h"
#include "wxScroll.h"
#include "wx_frame.h"
#include "wx_item.h"
#include "wxScrollArea.h"
#include "wxBorderArea.h"
#include "wxRectBorder.h"
#include "../../../wxcommon/wxGLConfig.h"

extern void wxCallDoPaintOrQueue(wxCanvas *win);
extern void MrEdQueuePaint(wxWindow *wx_window);
extern void MrEdAtomicallyPaint(wxCanvas *win);

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxCanvas::wxCanvas // Constructor (given parentFrame)
(
 wxFrame*	parentFrame,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 char*		windowName,
 wxGLConfig         *gl_cfg,
 WXTYPE		objectType
 ) :
 wxbCanvas (windowName, parentFrame, x, y, width, height, style)
{
  InitDefaults(gl_cfg);
}


//-----------------------------------------------------------------------------
wxCanvas::wxCanvas // Constructor (given parentArea)
(
 wxArea*		parentArea,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 char*		windowName,
 wxGLConfig         *gl_cfg,
 WXTYPE		objectType
 ) :
 wxbCanvas (windowName, parentArea, x, y, width, height, style)
{
  InitDefaults(gl_cfg);

  wx_dc = new WXGC_PTRS wxCanvasDC(this);
}

//-----------------------------------------------------------------------------
wxCanvas::wxCanvas // Constructor (given parentWindow)
(
 wxWindow*	parentWindow,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 char*		windowName,
 wxGLConfig         *gl_cfg,
 WXTYPE		objectType
 ) :
 wxbCanvas (windowName, parentWindow, x, y, width, height, style)
{
  InitDefaults(gl_cfg);
}


//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxCanvas::~wxCanvas(void)
{
  if (wx_dc) DELETE_OBJ wx_dc;
  if (needs_update) {
    DisposeRgn(needs_update);
    needs_update = NULL;
  }
}

//=============================================================================
// Private methods
//=============================================================================

//-----------------------------------------------------------------------------
void wxCanvas::InitDefaults(wxGLConfig *gl_cfg)
{
  units_x = 0;
  units_y = 0;
  units_per_page_x = 0;
  units_per_page_y = 0;
  hExtent = 0;
  vExtent = 0;
  hScrollingEnabled = TRUE;
  vScrollingEnabled = TRUE;
  scrollAutomanaged = TRUE;
  bgcol = ((cStyle & wxTRANSPARENT_WIN) ? NULL : wxWHITE);

  wx_dc = new WXGC_PTRS wxCanvasDC(this);

  if (gl_cfg) {
    gl_cfg = gl_cfg->Clone();
    wx_dc->gl_cfg = gl_cfg;
  }

  if (cStyle & wxCONTROL_BORDER) {
    if (cStyle & wxBORDER)
      cStyle -= wxBORDER;
  }

  /* Make wxBORDER before scroll bars, so that it's inside the scrollbar */
  if (cStyle & wxBORDER) {
    int direction = wxAll;
    if (cStyle & wxVSCROLL)
      direction -= wxRight;
    if (cStyle & wxHSCROLL)
      direction -= wxBottom;

    canvas_border = new WXGC_PTRS wxBorderArea(this, 1, direction);
  }

  if (cStyle & wxVSCROLL || cStyle & wxHSCROLL) {
    wxScrollData* scrollData;
    scrollData = new WXGC_ATOMIC wxScrollData;
    cScroll = new WXGC_PTRS wxScroll(this, scrollData);
    cScrollArea = new WXGC_PTRS wxScrollArea(this, this, 
					     (cStyle & wxVSCROLL) | (cStyle & wxHSCROLL) | (cStyle & wxRESIZE_CORNER));
  }

  /* Make wxCONTROL_BORDER after scroll bars, so that it's outside the scrollbar */  
  if (cStyle & wxCONTROL_BORDER) {
    int direction = wxAll;

    canvas_border = new WXGC_PTRS wxBorderArea(this, 3, direction, 0, (cStyle & wxCOMBO_SIDE) ? 2 : 1);
  }

  if (!(cStyle & wxFLAT)) {
    CreatePaintControl(-1, !(cStyle & wxTRANSPARENT_WIN));
  }

  {
    wxWindow *p;
    p = GetParent();
    if (p->IsHidden())
      DoShow(FALSE);
  }
  if (cStyle & wxINVISIBLE)
    Show(FALSE);
  InitInternalGray();

  if (cStyle & wxVSCROLL || cStyle & wxHSCROLL) {
    /* Somehow fixes initial update. There's another
       call like this one in the constructor for
       wxMediaCanvas. */
    OnClientAreaDSize(1, 1, 1, 1);
  }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxCanvas::SetColourMap(wxColourMap* cmap)
{
}

#define max(x, y) ((x > y) ? x : y)

//-----------------------------------------------------------------------------
void wxCanvas::OnClientAreaDSize(int dW, int dH, int dX, int dY)
{
  // update deviceContext
  if (wx_dc) {
    int clientWidth, clientHeight, ox, oy;
    Rect paintRect;
    wxArea *carea;

    carea = ClientArea();
    clientWidth = carea->Width();
    clientHeight= carea->Height();
    paintRect.top = 0;
    paintRect.left = 0;
    paintRect.bottom = clientHeight;
    paintRect.right = clientWidth;
    wx_dc->SetPaintRegion(&paintRect);

    carea->FrameContentAreaOffset(&ox, &oy);
    wx_dc->SetGrafPtrOffsets(ox, oy);
  }
  
  if (cScroll && scrollAutomanaged
      && (requested_x_step_size > 0 || requested_y_step_size > 0)) {
    wxScrollData *sd;
    sd = cScroll->GetScrollData();
    SetScrollbars(requested_x_step_size, requested_y_step_size,
		  hExtent / max(1, requested_x_step_size), vExtent / max(1, requested_y_step_size),
		  1, 1,
		  sd->GetValue(wxWhatScrollData::wxPositionH),
		  sd->GetValue(wxWhatScrollData::wxPositionV));
  }

  ResetGLView();

  /* Calling wxWindow's OnClientAreaDSize does bad
     things to the scrollbars, and we want to use
     the client area, only. */
  if (cPaintControl) {
    if (dW || dH) {
      int w, h;
      GetClientSize(&w, &h);
      ::SizeControl(cPaintControl, w, h);
    }
    MaybeMoveControls();
  }

  if (canvas_border)
    canvas_border->cBorder->OnClientAreaDSize(dW, dH, dX, dY);
}

void wxCanvas::MaybeMoveControls()
{
  /* Unlike wxWindow::MaybeMoveControls(),
     we want the client area, only */
  {
    int x, y;
    GetWinOrigin(&x, &y);
    if (cStyle & wxCONTROL_BORDER) {
      x += 3;
      y += 3;
    } else if (cStyle & wxBORDER) {
      x += 1;
      y += 1;
    }
    MoveControl(cPaintControl, x, y);

    {
      int ox, oy;
      wxArea *carea;

      carea = ClientArea();
      carea->FrameContentAreaOffset(&ox, &oy);
      wx_dc->SetGrafPtrOffsets(ox, oy);
    }
  }

  if (canvas_border)
    canvas_border->cBorder->MaybeMoveControls();
  if (cScrollArea)
    cScrollArea->MaybeMoveScrollControls();
}


//-----------------------------------------------------------------------------
// horizontal/vertical: number of pixels per unit (e.g. pixels per text line)
// x/y_length:        : no. units per scrollbar
// x/y_page:          : no. units per page scrolled
//-----------------------------------------------------------------------------
void wxCanvas::SetScrollbars(int horizontal, int vertical,
                             int x_length, int y_length,
                             int x_page, int y_page,
                             int x_pos, int y_pos,
			     Bool automgmt)
{
  wxWhatScrollData whatScrollData; // track what scrolldata changes
  wxScrollData* oldScrollData;
  wxScrollData* scrollData;
  int sizeH, unitH, sizeW, unitW;

  if (!cScroll) 
    return;
  
  if (!(cStyle & wxHSCROLL) || (x_length <= 0))
    horizontal = -1;
  if (!(cStyle & wxVSCROLL) || (y_length <= 0))
    vertical = -1;
  
  if (automgmt) {
    if (horizontal >= 0) {
      horizontal = max(horizontal, 1);
      hExtent = horizontal * x_length;
    } else
      hExtent = 0;
    if (vertical >= 0) {
      vertical = max(vertical, 1);
      vExtent = vertical * y_length;
    } else
      vExtent = 0;
  } else {
    hExtent = 0;
    vExtent = 0;
  }
  
  if (automgmt) {
    int tw, th, w, h, dw, dh;

    requested_x_step_size = horizontal;
    requested_y_step_size = vertical;
    
    GetClientSize(&w, &h);
    if (hExtent) {
      tw = hExtent;
      dw = tw - w;
      if (dw <= 0) {
	horizontal = -1;
      } else {
	x_length = (int)ceil((double)dw / horizontal);
	x_page = (int)floor((double)w / horizontal);
      }
    }
    if (vExtent) {
      th = vExtent;
      dh = th - h;
      if (dh <= 0) {
	vertical = -1;
      } else {
	y_length = (int)ceil((double)dh / vertical);
	y_page = (int)floor((double)h / vertical);
      }
    }
  }
  
  if (x_pos > x_length) x_pos = x_length;
  if (y_pos > y_length) y_pos = y_length;

  scrollAutomanaged = automgmt;

  oldScrollData = cScroll->GetScrollData();

  scrollData = new WXGC_ATOMIC wxScrollData;

  if (oldScrollData)
    *scrollData = *oldScrollData;

  sizeH = (vertical > 0 ? max(y_length, 1) : 0);
  if (sizeH != scrollData->GetValue(wxWhatScrollData::wxSizeH))
    {
      scrollData->SetValue(sizeH, wxWhatScrollData::wxSizeH);
      whatScrollData |= wxWhatScrollData::wxSizeH;
    }
  
  unitH = (vertical > 0 ? vertical : 0);
  if (unitH != scrollData->GetValue(wxWhatScrollData::wxUnitH))
    {
      scrollData->SetValue(unitH, wxWhatScrollData::wxUnitH);
      whatScrollData |= wxWhatScrollData::wxUnitH;
    }

  if (vertical < 0) y_page = 1;
  if (y_page != scrollData->GetValue(wxWhatScrollData::wxPageH))
    {
      scrollData->SetValue(y_page, wxWhatScrollData::wxPageH);
      whatScrollData |= wxWhatScrollData::wxPageH;
    }

  if (vertical < 0) y_pos = 0;
  if (y_pos != scrollData->GetValue(wxWhatScrollData::wxPositionV))
    {
      scrollData->SetValue(y_pos, wxWhatScrollData::wxPositionV);
      whatScrollData |= wxWhatScrollData::wxPositionV;
    }

  sizeW = (horizontal > 0 ? max(x_length, 1) : 0);
  if (sizeW != scrollData->GetValue(wxWhatScrollData::wxSizeW))
    {
      scrollData->SetValue(sizeW, wxWhatScrollData::wxSizeW);
      whatScrollData |= wxWhatScrollData::wxSizeW;
    }

  unitW = (horizontal > 0 ? horizontal : 0);
  if (unitW != scrollData->GetValue(wxWhatScrollData::wxUnitW))
    {
      scrollData->SetValue(unitW, wxWhatScrollData::wxUnitW);
      whatScrollData |= wxWhatScrollData::wxUnitW;
    }

  if (horizontal < 0) x_page = 1;
  if (x_page != scrollData->GetValue(wxWhatScrollData::wxPageW))
    {
      scrollData->SetValue(x_page, wxWhatScrollData::wxPageW);
      whatScrollData |= wxWhatScrollData::wxPageW;
    }

  if (horizontal < 0) x_pos = 0;
  if (x_pos != scrollData->GetValue(wxWhatScrollData::wxPositionH))
    {
      scrollData->SetValue(x_pos, wxWhatScrollData::wxPositionH);
      whatScrollData |= wxWhatScrollData::wxPositionH;
    }

  if ((long)whatScrollData != 0)
    cScroll->SetScrollData(scrollData, whatScrollData, NULL);	
}

//-----------------------------------------------------------------------------
void wxCanvas::SetScrollData
(
 wxScrollData*		scrollData,
 wxWhatScrollData	whatScrollData,
 wxScrollEvent*		evnt
 )
{
  wxDC* theDC;

  /* This function must not call Scheme directly when evnt is
     non-NULL.  Instead, queue callbacks. */

  // if (iniatorWindow == this) return;

  if ((long)whatScrollData & wxWhatScrollData::wxSizeW) {
    units_x = scrollData->GetValue(wxWhatScrollData::wxSizeW);
  }

  if ((long)whatScrollData & wxWhatScrollData::wxSizeH) {
    units_y = scrollData->GetValue(wxWhatScrollData::wxSizeH);
  }

  if ((long)whatScrollData & wxWhatScrollData::wxUnitW) {
    horiz_units = scrollData->GetValue(wxWhatScrollData::wxUnitW);
  }

  if ((long)whatScrollData & wxWhatScrollData::wxUnitH) {
    vert_units = scrollData->GetValue(wxWhatScrollData::wxUnitH);
  }

  if ((long)whatScrollData & wxWhatScrollData::wxPageW) {
    units_per_page_x = scrollData->GetValue(wxWhatScrollData::wxPageW);
  }

  if ((long)whatScrollData & wxWhatScrollData::wxPageH) {
    units_per_page_y = scrollData->GetValue(wxWhatScrollData::wxPageH);
  }

  theDC = GetDC();

  if (!scrollAutomanaged) {
    if (theDC) {
      theDC->device_origin_x -= theDC->auto_device_origin_x;
      theDC->device_origin_y -= theDC->auto_device_origin_y;
      theDC->auto_device_origin_x = 0;
      theDC->auto_device_origin_y = 0;
    }
    if (evnt) {
      /* OnScroll must queue the callback: */
      OnScroll(evnt);
    }
    return;
  }

  if (theDC) {
    int dH = 0;
    int dV = 0;

    {
      int newH;
      newH = (scrollData->GetValue(wxWhatScrollData::wxPositionH)
	      * scrollData->GetValue(wxWhatScrollData::wxUnitW));
      dH = (int)(newH - (-theDC->auto_device_origin_x));
    }
    
    {
      int newV;
      newV = (scrollData->GetValue(wxWhatScrollData::wxPositionV)
	      * scrollData->GetValue(wxWhatScrollData::wxUnitH));
      dV = (int)(newV - (-theDC->auto_device_origin_y));
    }
    
    if (dH != 0 || dV != 0) {
      int need_repaint = 0;

      if (!IsHidden()) {
	wxArea* clientArea;
	RgnHandle theUpdateRgn;
	Rect scrollRect;
	clientArea = ClientArea();
	theUpdateRgn = ::NewRgn();
	CheckMemOK(theUpdateRgn);
	SetCurrentDC();
	scrollRect.top = 0;
	scrollRect.left = 0;
	scrollRect.bottom = clientArea->Height();
	scrollRect.right = clientArea->Width();
	OffsetRect(&scrollRect,SetOriginX,SetOriginY);
	::ScrollRect(&scrollRect, -dH, -dV, theUpdateRgn);
	if (!EmptyRgn(theUpdateRgn)) {
          AddPaintRegion(theUpdateRgn);
	  need_repaint = 1;
        }
	::DisposeRgn(theUpdateRgn);
      }
      theDC->auto_device_origin_x += -dH;
      theDC->auto_device_origin_y += -dV;
      theDC->device_origin_x += -dH;
      theDC->device_origin_y += -dV;

      if (need_repaint) {
	if (evnt)
	  MrEdQueuePaint(this);
	else
	  DoPaint();
      }
    }
  }
}

//-----------------------------------------------------------------------------
void wxCanvas::GetScrollUnitsPerPage(int* x_page, int* y_page)
{
  *x_page = units_per_page_x;
  *y_page = units_per_page_y;
}

//-----------------------------------------------------------------------------
// Scroll to given position (scroll position, not pixel position)
//-----------------------------------------------------------------------------
void wxCanvas::Scroll(int xPos, int yPos)
{
  wxWhatScrollData whatScrollData; // track what scrolldata changes
  wxScrollData* oldScrollData;
  wxScrollData *scrollData;

  if (!cScroll) return;

  scrollData = new WXGC_ATOMIC wxScrollData;

  oldScrollData = cScroll->GetScrollData();
  if (oldScrollData) 
    *scrollData = *oldScrollData;

  if (xPos != -1)
    {
      if (xPos != scrollData->GetValue(wxWhatScrollData::wxPositionH))
	{
	  scrollData->SetValue(xPos, wxWhatScrollData::wxPositionH);
	  whatScrollData |= wxWhatScrollData::wxPositionH;
	}
    }

  if (yPos != -1)
    {
      if (yPos != scrollData->GetValue(wxWhatScrollData::wxPositionV))
	{
	  scrollData->SetValue(yPos, wxWhatScrollData::wxPositionV);
	  whatScrollData |= wxWhatScrollData::wxPositionV;
	}
    }

  if ((long)whatScrollData != 0)
    cScroll->SetScrollData(scrollData, whatScrollData, NULL);
}

void wxCanvas::ScrollPercent(double x, double y)
{
  if (!scrollAutomanaged) {
    /* Not managing  - do nothing */
  } else {
    /* Managing */
    int xp, yp, vw, vh, cw, ch;
    GetVirtualSize(&vw, &vh);
    GetClientSize(&cw, &ch);

    if (vw > cw)
      vw -= cw;
    else
      vw = 0;
    if (vh > ch)
      vh -= ch;
    else
      vh = 0;

    if (x >= 0)
      xp = (int)floor(x * vw);
    else
      xp = -1;
    
    if (y >= 0)
      yp = (int)floor(y * vh);
    else
      yp = -1;
    
    Scroll(xp, yp);
  }

}

//-----------------------------------------------------------------------------
void wxCanvas::EnableScrolling(Bool x_scroll, Bool y_scroll)
{
  if (cScrollArea) {
    if ((hScrollingEnabled != x_scroll)
	|| (vScrollingEnabled != y_scroll)) {
      hScrollingEnabled = x_scroll;
      vScrollingEnabled = y_scroll;
      cScrollArea->ShowScrolls(x_scroll, y_scroll);
      
      if (canvas_border && !(cStyle & wxCONTROL_BORDER)) {
	int direction = wxAll;
	if (vScrollingEnabled && (cStyle & wxVSCROLL))
	  direction -= wxRight;
	if (hScrollingEnabled && (cStyle & wxHSCROLL))
	  direction -= wxBottom;
	canvas_border->SetMargin(1, direction);
      }
    }
  }
}

void wxCanvas::SetResizeCorner(Bool on)
{
  if (cScrollArea) {
    cScrollArea->SetResizeCorner(on, hScrollingEnabled, vScrollingEnabled);
  }
}

//-----------------------------------------------------------------------------
int wxCanvas::GetScrollsPerPage(int orientation) // mac platform only
{
  return (orientation == wxHSCROLL ? units_per_page_x : units_per_page_y);
}

//-----------------------------------------------------------------------------
void wxCanvas::GetVirtualSize(int* x, int* y)
{
  int x1, y1;
  GetClientSize(&x1, &y1);
  if (hExtent == 0)
    *x = x1;
  else
    *x = hExtent;

  if (vExtent == 0)
    *y = y1;
  else
    *y = vExtent;
}

//-----------------------------------------------------------------------------
// Where the current view starts from
//-----------------------------------------------------------------------------
void wxCanvas::ViewStart(int* x, int* y)
{
  wxDC* theDC;
  theDC = GetDC();
  if (theDC) {
    *x = (int)(-(theDC->device_origin_x));
    *y = (int)(-(theDC->device_origin_y));
  } else
    *x = *y = 0;
}

//-----------------------------------------------------------------------------
void wxCanvas::WarpPointer(int x_pos, int y_pos)
{
  // Move the pointer to (x_pos,y_pos) coordinates. They are expressed in
  // pixel coordinates, relatives to the canvas -- So, we only need to
  // substract origin of the window.

  if (GetDC())
    {
      x_pos += (int)(GetDC()->device_origin_x) ;
      y_pos += (int)(GetDC()->device_origin_y) ;
    }
}

//-----------------------------------------------------------------------------
void wxCanvas::DoShow(Bool show)
{
  wxChildNode* node;
  wxWindow* theChildWindow;
  wxChildList *cl;

  if (!CanShow(show)) return;

  if (show)
    wxWindow::DoShow(show);

  cl = GetChildren();
  node = cl->First();
  while (node) {
    theChildWindow = (wxWindow*)node->Data();
    theChildWindow->DoShow(show);
    node = node->Next();
  }

  if (!show)
    wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------
void wxCanvas::ShowAsActive(Bool flag)
{
  /* Do nothing. */
}

void wxCanvas::ChangeToGray(Bool gray)
{
  if (cStyle & wxAS_CONTROL)
    Refresh();
  wxbCanvas::ChangeToGray(gray);
}

//-----------------------------------------------------------------------------
void wxCanvas::ClientToLogical(int* x, int* y) // mac platform only; testing
{ // Transform point from client c.s. to logical c.s. (virtual canvas, scrolling)
  // trying without all this gunk:
  wxDC* theDC;
  theDC = GetDC();
  if (theDC) {
    double fX, fY;
    fX = theDC->DeviceToLogicalX(*x);
    fY = theDC->DeviceToLogicalY(*y);
    *x = (int)fX;
    *y = (int)fY;
  }
}

Bool wxCanvas::WantsFocus(void)
{
  if (cStyle & (wxAS_CONTROL | wxNEVER_FOCUS))
    return FALSE;
  else
    return !cHidden;
}

Bool wxCanvas::AcceptsExplicitFocus(void)
{
  if (cStyle & wxNEVER_FOCUS)
    return FALSE;
  else if (cStyle & wxAS_CONTROL)
    return wxAllControlsWantFocus();
  else
    return wxbCanvas::AcceptsExplicitFocus();
}

Bool wxCanvas::SetAsControl()
{
  cStyle |= wxAS_CONTROL;
  return wxAllControlsWantFocus();
}

void wxCanvas::OnSetFocus(void)
{
  if (canvas_border)
    canvas_border->cBorder->PaintFocus(TRUE);
  wxWindow::OnSetFocus();
}

void wxCanvas::OnKillFocus(void)
{
  if (canvas_border)
    canvas_border->cBorder->PaintFocus(FALSE);
  wxWindow::OnSetFocus();
}

void wxCanvas::SetScrollPage(int dir, int val)
{
  if (scrollAutomanaged) return;

  wxCanvas::SetScrollbars(horiz_units, vert_units, 
			  units_x, units_y,
			  (dir == wxHORIZONTAL) ? val : units_per_page_x,
			  (dir == wxVERTICAL) ? val : units_per_page_y,
			  GetScrollPos(wxHORIZONTAL), GetScrollPos(wxVERTICAL), scrollAutomanaged);
}

void wxCanvas::SetScrollRange(int dir, int val)
{
  if (scrollAutomanaged) return;

  wxCanvas::SetScrollbars((dir == wxHORIZONTAL) ? (val > 0) : horiz_units,
			  (dir == wxVERTICAL) ? (val > 0) : vert_units,
			  (dir == wxHORIZONTAL) ? val : units_x,
			  (dir == wxVERTICAL) ? val : units_y,
			  units_per_page_x, units_per_page_y, 
			  GetScrollPos(wxHORIZONTAL), GetScrollPos(wxVERTICAL), scrollAutomanaged);
}

void wxCanvas::SetScrollPos(int dir, int val)
{
  int hv, vv;

  if (scrollAutomanaged) return;

  if (dir == wxHORIZONTAL)
    hv = val;
  else
    hv = GetScrollPos(wxHORIZONTAL);
      
  if (dir == wxVERTICAL)
    vv = val;
  else
    vv = GetScrollPos(wxVERTICAL);

  wxCanvas::SetScrollbars(horiz_units, vert_units, 
			  units_x, units_y,
			  units_per_page_x, units_per_page_y,
			  hv,
			  vv,
			  scrollAutomanaged);
}

int wxCanvas::GetScrollPos(int dir)
{
  if (scrollAutomanaged) return 0;

  if (!cScroll) return 0;
  
  return cScroll->GetScrollData()->GetValue((dir == wxHORIZONTAL) ? wxWhatScrollData::wxPositionH
					    : wxWhatScrollData::wxPositionV);
}

int wxCanvas::GetScrollPage(int dir)
{
  wxScrollData *sdata;

  if (scrollAutomanaged) return 0;

  if (!cScroll) return 1;
  
  if (!GetScrollRange(dir)) return 0;

  sdata = cScroll->GetScrollData();
  return sdata->GetValue((dir == wxHORIZONTAL) ? wxWhatScrollData::wxPageW
					    : wxWhatScrollData::wxPageH);
}
int wxCanvas::GetScrollRange(int dir)
{
  wxScrollData *sdata;

  if (scrollAutomanaged) return 0;

  if (!cScroll) return 0;
  
  sdata = cScroll->GetScrollData();

  return sdata->GetValue((dir == wxHORIZONTAL) ? wxWhatScrollData::wxSizeW
			 : wxWhatScrollData::wxSizeH);
}

void wxCanvas::DoPaint(void)
{
  if (!cHidden) {
    RgnHandle rgn;

    if (needs_update) {
      if (EmptyRgn(needs_update))
        return;

      if (cStyle & wxTRANSPARENT_WIN)
        rgn = NewRgn();
      else
        rgn = NULL;
      if (rgn) {
        CopyRgn(needs_update, rgn);
        SetRectRgn(needs_update, 0, 0, 0, 0);
        if (wx_dc->clip_reg) {
          /* FIXME: nested on-paint... */
        }
        wx_dc->clip_reg = rgn;
        wx_dc->SetCanvasClipping();
      } else {
        SetRectRgn(needs_update, 0, 0, 0, 0);
      }
    } else
      rgn = NULL;

    if (!(cStyle & wxTRANSPARENT_WIN)
	&& !(cStyle & wxNO_AUTOCLEAR)) {
      Rect itemRect;
      ThemeDrawingState s;
      RGBColor pixel;
      
      pixel = bgcol->pixel;
      
      SetCurrentDC();
      if (rgn)
        SetClip(rgn);
      GetThemeDrawingState(&s);
      GetControlBounds(cPaintControl, &itemRect);
      RGBBackColor(&pixel);
      BackPat(GetWhitePattern());
      EraseRect(&itemRect);
      SetThemeDrawingState(s, TRUE);
      if (rgn)
        cMacDC->setCurrentUser(NULL);
    }

    OnPaint();

    if (rgn) {
      wx_dc->clip_reg = NULL;
      wx_dc->SetCanvasClipping();
      DisposeRgn(rgn);
    }
  }
}

void wxCanvas::AddPaintRegion(RgnHandle rgn)
{
  if (!needs_update) {
    needs_update = NewRgn();
  }

  if (rgn) {
    UnionRgn(rgn, needs_update, needs_update);
  } else {
    SetRectRgn(needs_update, -32000, -32000, 32000, 32000);
  }
}

void wxCanvas::PaintRgn(RgnHandle rgn)
{
  if (!cHidden) {
    if (cStyle & wxAS_CONTROL) {
      /* Run on-paint atomically */
      RgnHandle old;

      old = wx_dc->clip_reg;

      wx_dc->clip_reg = rgn;
      wx_dc->SetCanvasClipping();

      MrEdAtomicallyPaint(this);

      wx_dc->clip_reg = old;
      wx_dc->SetCanvasClipping();
    } else {
      AddPaintRegion(rgn);
      /* In wx_frame.cc: */
      wxCallDoPaintOrQueue(this);
    }
  }
}

void wxCanvas::Paint()
{
  PaintRgn(NULL);
}

void wxCanvas::OnPaint(void)
{
  /* Do nothing */
}

void wxCanvas::ResetGLView()
{
  if (wx_dc->gl) {
    wxArea* clientArea;
    wxMacDC *macdc;
    wxFrame *fr;
    CGrafPtr graf;
    WindowRef win;
    int h, w;
    Rect cr, sr;

    SetCurrentMacDC();

    clientArea = ClientArea();
    w = clientArea->Width();
    h = clientArea->Height();

    fr = GetRootFrame();
    macdc = fr->MacDC();
    graf = macdc->macGrafPort();
    win = GetWindowFromPort(graf);

    GetWindowBounds(win, kWindowContentRgn, &cr);
    GetWindowBounds(win, kWindowStructureRgn, &sr);
    
    wx_dc->gl->ResetGLView(SetOriginX + (cr.left - sr.left),
			   (cr.bottom - cr.top) - (SetOriginY + h) + (sr.bottom - cr.bottom),
			   w,
			   h);
  }
}

void wxCanvas::SetCanvasBackground(wxColor *c)
{
  if (!bgcol || !c)
    return;
  
  if (c && c->IsMutable()) {
    c = new WXGC_PTRS wxColour(c);
    c->Lock(1);
  }
   
  bgcol = c;
}

wxColour *wxCanvas::GetCanvasBackground()
{
  return bgcol;
}
