/*								-*- C++ -*-
 *
 * Purpose: radio box panel item
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
#pragma implementation "RadioBox.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxRadioBox
#define  Uses_wxTypeTree
#include "wx.h"
#define  Uses_EnforcerWidget
#define  Uses_GroupWidget
#define  Uses_ToggleWidget
#include "widgets.h"

#define TOGGLES ((Widget*)toggles)

//-----------------------------------------------------------------------------
// create and destroy button
//-----------------------------------------------------------------------------

wxRadioBox::wxRadioBox(wxPanel *panel, wxFunction func, char *label,
		       int x, int y, int width, int height, int n, char **choices,
		       int num_rows, long style, wxFont *_font, char *name) : wxItem(_font)
{
    __type = wxTYPE_RADIO_BOX;

    toggles     = NULL;
    num_toggles = 0;

    Create(panel, func, label, x, y, width, height, n, choices,
	   num_rows, style, name);
}

wxRadioBox::wxRadioBox(wxPanel *panel, wxFunction func, char *label,
		       int x, int y, int width, int height, int n, wxBitmap **choices,
		       int num_rows, long style, wxFont *_font, char *name) : wxItem(_font)
{
    __type = wxTYPE_RADIO_BOX;

    toggles     = NULL;
    num_toggles = 0;

    Create(panel, func, label, x, y, width, height, n, choices,
	   num_rows, style, name);
}

Bool wxRadioBox::Create(wxPanel *panel, wxFunction func, char *label,
		   int x, int y, int width, int height, int n, char **choices,
		   int num_rows, long style, char *name)
{
    int i;
    wxWindow_Xintern *ph;
    Bool vert;
    Dimension ww, hh;
    double lw, lh;
    Widget wgt;

    if ( (num_toggles = n) <= 0 ) {
	wxDebugMsg("%s created without items (n=0)!\n", name);
	return TRUE;
    }

    bm_labels = NULL;
    bm_label_masks = NULL;

    ChainToPanel(panel, style, name);

    if (style & wxVERTICAL_LABEL)
      vert = 1;
    else if (style & wxHORIZONTAL_LABEL)
      vert = 0;
    else
      vert = (panel->GetLabelPosition() == wxVERTICAL);

    label = wxGetCtlLabel(label);

    if ((style & wxVERTICAL) == wxVERTICAL) {
      if (num_rows <= 0)
	num_rows = num_toggles;
      else
	num_rows = 1;
    } else {
      if (num_rows <= 0)
	num_rows = 1;
      else
	num_rows = num_toggles / num_rows;
    }

    ph = parent->GetHandle();
    
    // create frame
    wgt = XtVaCreateWidget(name, xfwfEnforcerWidgetClass, ph->handle,
			   XtNlabel,       label,
			   XtNalignment,   vert ? XfwfTop : XfwfLeft,
			   XtNbackground,  wxGREY_PIXEL,
			   XtNforeground,  wxBLACK_PIXEL,
			   XtNfont,        font->GetInternalFont(),
#ifdef WX_USE_XFT
			   XtNxfont,       font->GetInternalAAFont(),
#endif
			   XtNframeType,   (style & wxFLAT) ? XfwfChiseled : XfwfSunken,
			   XtNframeWidth,  0,
			   XtNshrinkToFit, (width < 0 || height < 0),
			   NULL);
    if (!(style & wxINVISIBLE))
      XtManageChild(wgt);
    else
      XtRealizeWidget(wgt);
    X->frame = wgt;
    // create group widget, which holds the toggles
    wgt = XtVaCreateManagedWidget("radiobox", xfwfGroupWidgetClass, X->frame,
				  XtNselectionStyle, (style & wxAT_MOST_ONE) ?
				  XfwfSingleSelection : XfwfOneSelection,
				  XtNstoreByRow,     FALSE,
				  XtNlabel,          NULL,
				  XtNframeWidth,     0,
				  XtNbackground,     wxGREY_PIXEL,
				  XtNrows,           num_rows,
				  XtNshrinkToFit,    (width < 0 || height < 0),
				  NULL);
    X->handle = wgt;
    // create the toggles
    toggles = new WXGC_ATOMIC long[num_toggles];
    enabled = new WXGC_ATOMIC Bool[num_toggles];
    for (i=0; i < num_toggles; ++i) {
	char num_name[10]; 
	char *tlabel;
        enabled[i] = 1;
	sprintf(num_name, "%d", i);
	tlabel = wxGetCtlLabel(choices[i]);
	wgt = XtVaCreateManagedWidget(num_name, xfwfToggleWidgetClass, X->handle,
				      XtNlabel,         tlabel,
				      XtNbackground,    wxGREY_PIXEL,
				      XtNforeground,    wxBLACK_PIXEL,
				      XtNhighlightColor, wxCTL_HIGHLIGHT_PIXEL,
				      XtNfont,          font->GetInternalFont(),
#ifdef WX_USE_XFT
				      XtNxfont,         font->GetInternalAAFont(),
#endif
				      XtNshrinkToFit,   TRUE,
				      NULL);
	((Widget*)toggles)[i] = wgt;
    }
    // set data declared in wxItem
    callback = func;
    XtAddCallback(X->handle, XtNactivate, wxRadioBox::EventCallback,
		  (XtPointer)saferef);
    // resize enforcer
    XtVaGetValues(X->handle, XtNwidth, &ww, XtNheight, &hh, NULL);
    if (label)
      GetTextExtent(label, &lw, &lh, NULL, NULL, font);
    else {
      lw = lh = 0;
    }
    if (vert)	hh += int(lh);
    else	ww += int(lw);
    XtVaSetValues(X->frame, XtNwidth, ww+4, XtNheight, hh+4, NULL);
    // panel positioning
    panel->PositionItem(this, x, y, width, height);
    AddEventHandlers();

    for (i = 0; i < num_toggles; i++) {
      XtInsertEventHandler(((Widget*)toggles)[i],
			   KeyPressMask |	// for PreOnChar
			   ButtonPressMask |	// for PreOnEvent
			   ButtonReleaseMask |
			   ButtonMotionMask |
			   PointerMotionMask | PointerMotionHintMask,
			   FALSE,
			   (XtEventHandler)wxWindow::WindowEventHandler,
			   (XtPointer)saferef,
			   XtListHead);
    }

    if (style & wxINVISIBLE)
      Show(FALSE);

    return TRUE;
}

Bool wxRadioBox::Create(wxPanel *panel, wxFunction func, char *label,
		   int x, int y, int width, int height, int n, wxBitmap **choices,
		   int num_rows, long style, char *name)
{
    int i;
    Bool vert;
    wxWindow_Xintern *ph;
    Dimension ww, hh; double lw, lh;
    Widget wgt;

    if ( (num_toggles = n) <= 0 ) {
	wxDebugMsg("%s created without items (n=0)!\n", name);
	return TRUE;
    }

    ChainToPanel(panel, style, name);

    label = wxGetCtlLabel(label);

    if (style & wxVERTICAL_LABEL)
      vert = 1;
    else if (style & wxHORIZONTAL_LABEL)
      vert = 0;
    else
      vert = (panel->GetLabelPosition() == wxVERTICAL);
    
    if ((style & wxVERTICAL) == wxVERTICAL) {
      if (num_rows <= 0)
	num_rows = num_toggles;
      else
	num_rows = 1;
    } else {
      if (num_rows <= 0)
	num_rows = 1;
      else
	num_rows = num_toggles / num_rows;
    }

    ph = parent->GetHandle();

    // create frame
    wgt = XtVaCreateWidget(name, xfwfEnforcerWidgetClass, ph->handle,
			   XtNlabel,       label,
			   XtNalignment,   vert ? XfwfTop : XfwfLeft,
			   XtNbackground,  wxGREY_PIXEL,
			   XtNforeground,  wxBLACK_PIXEL,
			   XtNfont,        font->GetInternalFont(),
#ifdef WX_USE_XFT
			   XtNxfont,       font->GetInternalAAFont(),
#endif
			   XtNframeType,   (style & wxFLAT) ? XfwfChiseled : XfwfSunken,
			   XtNframeWidth,  0,
			   XtNshrinkToFit, TRUE,
			   NULL);
    if (!(style & wxINVISIBLE))
      XtManageChild(wgt);
    else
      XtRealizeWidget(wgt);
    X->frame = wgt;

    // create group widget, which holds the toggles
    wgt = XtVaCreateManagedWidget("radiobox", xfwfGroupWidgetClass, X->frame,
				  XtNselectionStyle, (style & wxAT_MOST_ONE) ?
				  XfwfSingleSelection : XfwfOneSelection,
				  XtNstoreByRow,     FALSE,
				  XtNlabel,          NULL,
				  XtNframeWidth,     0,
				  XtNbackground,     wxGREY_PIXEL,
				  XtNrows,           num_rows,
				  XtNshrinkToFit,    TRUE,
				  NULL);
    X->handle = wgt;
    // create the toggles
    toggles = new WXGC_ATOMIC long[num_toggles];
    enabled = new WXGC_ATOMIC Bool[num_toggles];
#ifdef MZ_PRECISE_GC
    {
      wxBitmap **ba;
      ba = (wxBitmap **)GC_malloc(num_toggles * sizeof(wxBitmap *));
      bm_labels = ba;
      ba = (wxBitmap **)GC_malloc(num_toggles * sizeof(wxBitmap *));
      bm_label_masks = ba;
    }
#else
    bm_labels = new WXGC_PTRS wxBitmap*[num_toggles];
    bm_label_masks = new WXGC_PTRS wxBitmap*[num_toggles];
#endif
    for (i=0; i < num_toggles; ++i) {
	char num_name[10];
	char *kind;
	void *label;
	wxBitmap *achoice;
	void *mpm;

	sprintf(num_name, "%d", i);

	enabled[i] = 1;

	achoice = choices[i];
	if (achoice->Ok() && (achoice->selectedIntoDC >= 0)) {
	  kind = XtNpixmap;
	  label = achoice->GetLabelPixmap();
	  bm_labels[i] = achoice;
	  achoice->selectedIntoDC++;
	  achoice = CheckMask(bm_labels[i]);
	  bm_label_masks[i] = achoice;
	  if (achoice)
	    mpm = (void *)GETPIXMAP(achoice);
	  else
	    mpm = NULL;
	} else {
	  kind = XtNlabel;
	  label = (char *)"<bad-image>";
	  bm_labels[i] = NULL;
	  bm_label_masks[i] = NULL;
	  mpm = NULL;
	}

	wgt = XtVaCreateManagedWidget(num_name, xfwfToggleWidgetClass, X->handle,
				      kind,             label,
				      XtNmaskmap,       mpm,
				      XtNbackground,    wxGREY_PIXEL,
				      XtNforeground,    wxBLACK_PIXEL,
				      XtNhighlightColor, wxCTL_HIGHLIGHT_PIXEL,
				      XtNfont,          font->GetInternalFont(),
#ifdef WX_USE_XFT
				      XtNxfont,         font->GetInternalAAFont(),
#endif
				      XtNshrinkToFit,   TRUE,
				      NULL);
	((Widget*)toggles)[i] = wgt;
    }
    // set data declared in wxItem
    callback = func;
    XtAddCallback(X->handle, XtNactivate, wxRadioBox::EventCallback,
		  (XtPointer)saferef);
    // resize enforcer
    XtVaGetValues(X->handle, XtNwidth, &ww, XtNheight, &hh, NULL);
    if (label)
      GetTextExtent(label, &lw, &lh, NULL, NULL, font);
    else {
      lw = lh = 0;
    }
    if (vert)	hh += int(lh);
    else	ww += int(lw);
    XtVaSetValues(X->frame, XtNwidth, ww+4, XtNheight, hh+4, NULL);
    // panel positioning
    panel->PositionItem(this, x, y, width, height);
    AddEventHandlers();

    for (i = 0; i < num_toggles; i++) {
      XtInsertEventHandler(((Widget*)toggles)[i],
			   KeyPressMask |	// for PreOnChar
			   ButtonPressMask |	// for PreOnEvent
			   ButtonReleaseMask |
			   ButtonMotionMask |
			   PointerMotionMask | PointerMotionHintMask,
			   FALSE,
			   (XtEventHandler)wxWindow::WindowEventHandler,
			   (XtPointer)saferef,
			   XtListHead);
    }

    if (style & wxINVISIBLE)
      Show(FALSE);

    return TRUE;
}

wxRadioBox::~wxRadioBox(void)
{
  if (bm_labels) {
    int i;
    for (i = 0; i < num_toggles; i++) {
      if (bm_labels[i]) {
	wxBitmap *bm = bm_labels[i];
	--bm->selectedIntoDC;
	bm->ReleaseLabel();
	XtVaSetValues(((Widget*)toggles)[i], 
		      XtNpixmap, NULL, 
		      XtNmaskmap, NULL, 
		      NULL);
      }
      if (bm_label_masks[i]) {
	wxBitmap *bm = bm_label_masks[i];
	--bm->selectedIntoDC;
      }
    }
    bm_labels = NULL;
    bm_label_masks = NULL;
  }
}

//-----------------------------------------------------------------------------
// methods to access internal data
//-----------------------------------------------------------------------------

void wxRadioBox::Enable(int item, Bool enable)
{
  if (0 <= item && item < num_toggles) {
    enabled[item] = enable;
    if (!IsGray())
      XtSetSensitive(TOGGLES[item], enable);
  }
}

void wxRadioBox::ChangeToGray(Bool gray)
{
  int i;

  wxWindow::ChangeToGray(gray);
  for (i = 0; i < num_toggles; i++) {
    XtSetSensitive(TOGGLES[i], gray ? FALSE : enabled[i]);
  }
}

int wxRadioBox::FindString(char *s)
{
  int i;
  for (i = 0; i < num_toggles; i++) {
    char *l;
    l = GetLabel(i);
    if (l && !strcmp(l, s))
      return i;
  }
  return -1;
}

char *wxRadioBox::GetLabel(int item)
{
    char *label = NULL;

    if (0 <= item && item < num_toggles)
	XtVaGetValues(TOGGLES[item], XtNlabel, &label, NULL);

    return label;
}

int wxRadioBox::GetSelection(void)
{
    long selection;

    if (!num_toggles)
      return -1;

    XtVaGetValues(X->handle, XtNselection, &selection, NULL);

    return int(selection);
}

char *wxRadioBox::GetStringSelection(void)
{
    char *label = NULL;
    int  item;
    item = GetSelection();

    if (0 <= item && item < num_toggles)
	XtVaGetValues(TOGGLES[item], XtNlabel, &label, NULL);
    return label;
}

char *wxRadioBox::GetString(int which)
{
    char *label = NULL;

    if (0 <= which && which < num_toggles)
	XtVaGetValues(TOGGLES[which], XtNlabel, &label, NULL);
    return label;
}

void wxRadioBox::SetLabel(int item, char *label)
{
  label = wxGetCtlLabel(label);

  if (0 <= item && item < num_toggles
      && (!bm_labels || !bm_labels[item]))
    XtVaSetValues(TOGGLES[item], XtNlabel, label, NULL);
}

void wxRadioBox::SetLabel(int item, wxBitmap *bitmap)
{
  if (0 <= item && item < num_toggles
      && bm_labels && bm_labels[item]) {
    Pixmap pm, mpm;
    wxBitmap *obm;

    obm = bm_labels[item];
    --obm->selectedIntoDC;
    obm->ReleaseLabel();
    obm = bm_label_masks[item];
    if (obm)
      --obm->selectedIntoDC;

    bm_labels[item] = bitmap;
    bitmap->selectedIntoDC++;

    obm = CheckMask(bitmap);
    bm_label_masks[item] = obm;

    pm = (Pixmap)bitmap->GetLabelPixmap();
    if (obm)
      mpm = GETPIXMAP(obm);
    else
      mpm = 0;

    XtVaSetValues(TOGGLES[item], 
		  XtNlabel, NULL,
		  XtNpixmap, pm, 
		  XtNmaskmap, mpm, 
		  NULL);
  }
}

void wxRadioBox::SetSelection(int item)
{
  if (0 <= item && item < num_toggles)
    XtVaSetValues(X->handle, XtNselection, (long)item, NULL);
}

void wxRadioBox::SetStringSelection(char *s)
{
  SetSelection(FindString(s));
}

Bool wxRadioBox::Show(int item, Bool show)
{
  if (0 <= item && item < num_toggles) {
    if (show) XtMapWidget(TOGGLES[item]);
    else      XtUnmapWidget(TOGGLES[item]);
  }
  return FALSE;
}

void wxRadioBox::Command(wxCommandEvent *event)
{
  ProcessCommand(event);
}

//-----------------------------------------------------------------------------
// callbacks for xfwfGroupWidgetClass
//-----------------------------------------------------------------------------

void wxRadioBox::SetSelectedButtonFocus()
{
  ButtonFocus(GetSelection());
}

void wxRadioBox::EventCallback(Widget WXUNUSED(w), XtPointer dclient, XtPointer WXUNUSED(dcall))
{
    wxRadioBox     *radioBox = (wxRadioBox *)GET_SAFEREF(dclient);
    wxCommandEvent *event;

    event = new wxCommandEvent(wxEVENT_TYPE_RADIOBOX_COMMAND);

    radioBox->SetSelectedButtonFocus();

    radioBox->ProcessCommand(event);

#ifdef MZ_PRECISE_GC
    XFORM_RESET_VAR_STACK;
#endif
}

extern "C" Boolean has_focus_now(Widget w);

int wxRadioBox::ButtonFocus(int which)
{
  if (which > num_toggles) return -1;

  if (which > -1) {
    // Set the focus on the just-clicked button
    // find frame of this widget
    wxWindow *win = this;
    for (/*wxWindow *win = this*/; win; win = win->GetParent()) {
      if (wxSubType(win->__type, wxTYPE_FRAME))
	break;
    }
    
    if (win) {
      wxWindow_Xintern *h;
      h = win->GetHandle();
      XtSetKeyboardFocus(h->frame, (Widget)TOGGLES[which]);
    }

    return -1;
  } else {
    int i;
    for (i = num_toggles; i--; ) {
      Widget w = (Widget)TOGGLES[i];
      if (has_focus_now(w))
	return i;
    }
    return -1;
  }
}
