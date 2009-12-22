/*								-*- C++ -*-
 *
 * Purpose: gauge panel item
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 2004-2010 PLT Scheme Inc.
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */

#ifdef __GNUG__
#pragma implementation "Gauge.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxGauge
#include "wx.h"
#define  Uses_TraversingEnforcerWidget
#define  Uses_SliderWidget
#include "widgets.h"

//-----------------------------------------------------------------------------
// create and destroy gauge
//-----------------------------------------------------------------------------

wxGauge::wxGauge(wxPanel *panel, char *label, int _range,
		 int x, int y, int width, int height, long style, 
		 wxFont *_font, char *name) : wxItem(_font)
{
    __type = wxTYPE_GAUGE;

    range = value = 0;

    Create(panel, label, _range, x, y, width, height, style, name);
}

#define wxDEFAULT_GAUGE_WIDTH		100
#define wxDEFAULT_GAUGE_HEIGHT		 24

Bool wxGauge::Create(wxPanel *panel, char *label, int _range,
		     int x, int y, int width, int height, long style, char *name)
{
    Widget wgt;
    wxWindow_Xintern *ph;
    Bool vert;
    double lw, lh, lvh, lhw;

    ChainToPanel(panel, style, name);

    if (style & wxVERTICAL_LABEL)
      vert = 1;
    else if (style & wxHORIZONTAL_LABEL)
      vert = 0;
    else
      vert = (panel->GetLabelPosition() == wxVERTICAL);
    range     = _range;

    label = wxGetCtlLabel(label);

    ph = parent->GetHandle();

    // create frame
    wgt = XtVaCreateWidget
	(name, xfwfTraversingEnforcerWidgetClass, ph->handle,
	 XtNlabel,       label,
	 XtNalignment,   vert ? XfwfTop : XfwfLeft,
	 XtNbackground,  wxGREY_PIXEL,
	 XtNforeground,  wxBLACK_PIXEL,
	 XtNfont,        font->GetInternalFont(),
#ifdef WX_USE_XFT
	 XtNxfont,       font->GetInternalAAFont(),
#endif
	 XtNtraversalOn, FALSE,
	 XtNframeType,   XfwfSunken,
	 XtNframeWidth,  1,
	 XtNhighlightThickness, 0, XtNtraversalOn, FALSE,
	 NULL);
    if (!(style & wxINVISIBLE))
      XtManageChild(wgt);
    else
      XtRealizeWidget(wgt);
    X->frame = wgt;
    // create the slider widget
    wgt = XtVaCreateManagedWidget
	("gauge", xfwfSlider2WidgetClass, X->frame,
	 XtNbackground,    wxDARK_GREY_PIXEL,
	 XtNforeground,    wxBLACK_PIXEL,
	 XtNthumbColor,    wxCTL_HIGHLIGHT_PIXEL,
	 XtNminsize,	   0,
	 XtNframeType,     XfwfRaised,
	 XtNframeWidth,    0,
	 XtNhighlightThickness, 0,
	 NULL);
    X->handle = wgt;
    XtUninstallTranslations(X->handle);
    // set data declared in wxItem
    // panel positioning

    if (label)
      GetTextExtent(label, &lw, &lh, NULL, NULL, font);
    else
      lw = lh = 0;
    if (vert) {
      lhw = 0;
      lvh = lh;
    } else {
      lhw = lw;
      lvh = 0;
    }

    panel->PositionItem(this, x, y, 
			(width  > -1 
			 ? (int)width  
			 : ((style & wxVERTICAL) 
			    ? (int)lhw + wxDEFAULT_GAUGE_HEIGHT
			    : (int)lhw + wxDEFAULT_GAUGE_WIDTH)),
			(height > -1 
			 ? (int)height 
			 : ((style & wxVERTICAL) 
			    ? (int)lvh + wxDEFAULT_GAUGE_WIDTH
			    : (int)lvh + wxDEFAULT_GAUGE_HEIGHT)));

    SetValue(0);

    AddEventHandlers();

    if (style & wxINVISIBLE)
      Show(FALSE);

    return TRUE;
}

//-----------------------------------------------------------------------------
// methods to access internal data
//-----------------------------------------------------------------------------

void wxGauge::SetRange(int new_range)
{
    if (0 <= new_range) {
	range = new_range;
	SetValue(value);
    }
}

void wxGauge::SetValue(int new_value)
{
  if (0 <= new_value && new_value <= range) {
    value = new_value;
    if (style & wxVERTICAL) {
      XfwfMoveThumb (X->handle, 0.0, 1.0);
      XfwfResizeThumb(X->handle, 1.0, ((double)value)/((double)range));
    } else {
      XfwfMoveThumb(X->handle, 0.0, 0.0);
      XfwfResizeThumb(X->handle, ((double)value)/((double)range), 1.0);
    }
  }
}
