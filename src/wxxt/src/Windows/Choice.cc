/*								-*- C++ -*-
 *
 * Purpose: choice panel item
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
#pragma implementation "Choice.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxChoice
#define  Uses_wxMenu
#include "wx.h"
#define  Uses_ArrowWidget
#define  Uses_TraversingEnforcerWidget
#define  Uses_LabelWidget
#include "widgets.h"

char *wxchoice_unprotect_amp(char *s);

//-----------------------------------------------------------------------------
// create and destroy button
//-----------------------------------------------------------------------------

wxChoice::wxChoice(wxPanel *panel, wxFunction function, char *label,
		   int x, int y, int width, int height,
		   int n, char *choices[], long style, wxFont *_font, char *name) : wxItem(_font)
{
  wxMenu *naya;

  __type = wxTYPE_CHOICE;
  
  naya = DEBUG_NEW wxMenu(NULL, (wxFunction)&(wxChoice::MenuEventCallback), font);
  choice_menu = naya;
  num_choices = 0;
  selection   = -1;
  
  Create(panel, function, label, x, y, width, height, n, choices, style, name);
}

Bool wxChoice::Create(wxPanel *panel, wxFunction function, char *label,
		      int x, int y, int width, int height,
		      int n, char *choices[], long style, char *name)
{
    wxWindow_Xintern *ph;
    Bool vert;
    Widget button, wgt;

    ChainToPanel(panel, style, name);

    if (style & wxVERTICAL_LABEL)
      vert = 1;
    else if (style & wxHORIZONTAL_LABEL)
      vert = 0;
    else
      vert = (panel->GetLabelPosition() == wxVERTICAL);

    label = wxGetCtlLabel(label);

    ph = parent->GetHandle();

    // create frame
    wgt = XtVaCreateWidget
	(name, xfwfTraversingEnforcerWidgetClass, ph->handle,
	 XtNlabel,       label,
	 XtNalignment,   vert ? XfwfTop : XfwfLeft,
	 XtNbackground,  wxGREY_PIXEL,
	 XtNforeground,  wxBLACK_PIXEL,
	 XtNhighlightColor, wxCTL_HIGHLIGHT_PIXEL,
	 XtNfont,        font->GetInternalFont(),
#ifdef WX_USE_XFT
	 XtNxfont,       font->GetInternalAAFont(),
#endif
	 XtNshrinkToFit, TRUE,
	 NULL);
    if (!(style & wxINVISIBLE))
      XtManageChild(wgt);
    else
      XtRealizeWidget(wgt);
    X->frame = wgt;
    // create widget
    wgt = XtVaCreateManagedWidget
	("choice", xfwfLabelWidgetClass, X->frame,
	 XtNlabel,       n > 0 ? choices[0] : "",
	 XtNbackground,  wxBUTTON_PIXEL,
	 XtNforeground,  wxBLACK_PIXEL,
	 XtNfont,        font->GetInternalFont(),
#ifdef WX_USE_XFT
	 XtNxfont,       font->GetInternalAAFont(),
#endif
	 XtNframeWidth,  2,
	 XtNhighlightThickness, 0,
	 XtNalignment,   XfwfLeft,
	 XtNrightMargin,  16,
	 XtNshrinkToFit, (width < 0 || height < 0),
	 // XtNtraversalOn, TRUE, /* MATTHEW */
	 NULL);
    X->handle = wgt;
    // arrow widget which pops up a menu
    button = XtVaCreateManagedWidget
	("choice_button", xfwfArrowWidgetClass, X->handle,
	 XtNbackground,  wxBUTTON_PIXEL,
	 XtNforeground,  wxBLACK_PIXEL,
	 XtNdirection,   XfwfBottom,
	 XtNrepeat,      FALSE,
	 XtNarrowShadow, 0,
	 XtNlocation,    "1.0 - 16 0.5 - 5 16 10",
	 NULL);
    // set data declared in wxItem
    callback = function;
    XtAddCallback(button, XtNcallback, wxChoice::EventCallback, (XtPointer)saferef);
    X->extra = button;

    selection = n > 0 ? 0 : -1;
    for (int i = 0; i < n; ++i) {
      Append(choices[i]);
    }

    if (width < 0) {
      double maxw = 0, labelw = 0;
      for (int i = 0; i < n; i++) {
	double w, h;
	GetTextExtent(choices[i], &w, &h, NULL, NULL, font);
	if (w > maxw)
	  maxw = w;
      }
      
      if (label && !vert) {
	double w, h;
	char *label_stripped;
	label_stripped = wxchoice_unprotect_amp(label);
	GetTextExtent(label_stripped, &w, &h, NULL, NULL, font);
	labelw = w + 2; /* 2 for separation btw label and ctl */
      }

      width = (int)(maxw + labelw + 32); /* 32 = space for arrow */
    }

    panel->PositionItem(this, x, y, width, height);
    AddEventHandlers();

    XtInsertEventHandler(button,
			 KeyPressMask |	// for PreOnChar
			 ButtonPressMask |	// for PreOnEvent
			 ButtonReleaseMask |
			 ButtonMotionMask |
			 PointerMotionMask | PointerMotionHintMask,
			 FALSE,
			 (XtEventHandler)wxWindow::WindowEventHandler,
			 (XtPointer)saferef,
			 XtListHead);

    XtInsertEventHandler(X->frame,
			 KeyPressMask, // for PreOnChar
			 FALSE,
			 (XtEventHandler)wxWindow::WindowEventHandler,
			 (XtPointer)saferef,
			 XtListHead);

    if (style & wxINVISIBLE)
      Show(FALSE);

    return TRUE;
}

wxChoice::~wxChoice (void)
{
    if (choice_menu)
	DELETE_OBJ choice_menu;
    choice_menu = NULL;
    num_choices = 0;
}

//-----------------------------------------------------------------------------
// size
//-----------------------------------------------------------------------------

void  wxChoice::GetSize(int *width, int *height)
{
  wxWindow::GetSize(width, height);
}


//-----------------------------------------------------------------------------
// methods to access internal data
//-----------------------------------------------------------------------------

static char *protect_amp(char *s)
{
  if (strchr(s, '&')) {
    /* protect "&" */
    int i, amp = 0;
    char *s2;
    for (i = 0; s[i]; i++) {
      if (s[i] == '&')
	amp++;
    }

    s2 = new WXGC_ATOMIC char[i + amp + 1];
    for (i = 0, amp = 0; s[i]; i++, amp++) {
      s2[amp] = s[i];
      if (s[i] == '&') {
	s2[++amp] = '&';
      }
    }
    s2[amp] = 0;
    return s2;
  } else
    return s;
}

char *wxchoice_unprotect_amp(char *s)
{
  if (strchr(s, '&')) {
    /* strip "&&" */
    int i, amp = 0;
    char *s2;
    for (i = 0; s[i]; i++) {
      if (s[i] == '&') {
	amp++;
	i++;
      }
    }

    s2 = new WXGC_ATOMIC char[i - amp + 1];
    amp = 0;
    for (i = 0; s[i]; i++, amp++) {
      if (s[i] == '&')
	i++;
      s2[amp] = s[i];
    }
    s2[amp] = 0;
    return s2;
  } else
    return s;
}

void wxChoice::Append(char *s)
{
  s = protect_amp(s);

  choice_menu->Append(num_choices++, s, (char *)(-1));
  if (num_choices == 1) {
    XtVaSetValues(X->handle, XtNshrinkToFit, False, XtNlabel, s, NULL);
    selection = 0;
  }
}

void wxChoice::Clear(void)
{
  wxMenu *naya;
  
  DELETE_OBJ choice_menu;
  naya = DEBUG_NEW wxMenu(NULL, (wxFunction)&(wxChoice::MenuEventCallback));
  choice_menu = naya;
  num_choices = 0;
  selection = 0;
  XtVaSetValues(X->handle, XtNshrinkToFit, False, XtNlabel, "", NULL);
}

int wxChoice::FindString(char *s)
{
  s = protect_amp(s);

  return choice_menu->FindItem(s, 0);
}

char *wxChoice::GetString(int n)
{
  char *s;
  s = choice_menu->GetLabel(n);

  return s ? wxchoice_unprotect_amp(s) : (char *)NULL;
}

char *wxChoice::GetStringSelection(void)
{
  char *s;
  s = choice_menu->GetLabel(selection);
  
  return s ? wxchoice_unprotect_amp(s) : (char *)NULL;
}

void wxChoice::SetSelection(int n)
{
  if (0 <= n && n < num_choices) {
    char *label;
    selection = n;
    label = choice_menu->GetLabel(n);
    XtVaSetValues(X->handle, XtNshrinkToFit, False, XtNlabel, label, NULL);
  }
}

Bool wxChoice::SetStringSelection(char *s)
{
  int i;
  i = FindString(s);
  if (i > -1) {
    SetSelection(i);
    return TRUE;
  } else
    return FALSE;
}

void wxChoice::Command(wxCommandEvent *event)
{
  ProcessCommand(event);
}

void wxChoice::ChangeToGray(Bool gray)
{
  XtVaSetValues(X->extra, XtNdrawgrayArrow, (Boolean)gray, NULL);
  wxItem::ChangeToGray(gray);
}

//-----------------------------------------------------------------------------
// callback for commandWidgetClass
//-----------------------------------------------------------------------------

void wxChoice::EventCallback(Widget WXUNUSED(w),
			     XtPointer clientData, XtPointer WXUNUSED(ptr))
{
    wxChoice *choice = (wxChoice *)GET_SAFEREF(clientData);
    Dimension hh, ww;

    choice->SetFocus();

    choice->choice_menu->SetClientData(choice);

    // popup menu below "button"
    XtVaGetValues(choice->X->handle, XtNheight, &hh, XtNwidth, &ww, NULL);

    choice->choice_menu->SetWidth(ww);

    choice->PopupMenu(choice->choice_menu, 0, (int)hh - 2, TRUE, (int)hh);

#ifdef MZ_PRECISE_GC
    XFORM_RESET_VAR_STACK;
#endif
}

void wxChoice::OnEvent(wxMouseEvent *e)
{
  if (e->ButtonDown())
    EventCallback(0, (XtPointer)saferef, 0);
}

void wxChoice::MenuEventCallback(wxObject* obj, wxCommandEvent* ev)
{
    wxChoice       *choice;
    wxPopupEvent *pu = (wxPopupEvent *)ev;
    wxCommandEvent *event;

    choice = (wxChoice*)((wxMenu *)obj)->GetClientData();

    if (!choice->Number())
      return;

    event = new wxCommandEvent(wxEVENT_TYPE_CHOICE_COMMAND);

    choice->SetSelection(pu->menuId);
    choice->ProcessCommand(event);
}

void wxChoice::OnChar(wxKeyEvent *e)
{
  int delta = 0;

  switch (e->keyCode) {
  case WXK_UP:
    delta = -1;
    break;
  case WXK_DOWN:
    delta = 1;
    break;
  }

  if (delta) {
    int s;
    s = GetSelection();
    SetSelection(s + delta);
    if (s != GetSelection()) {
      wxCommandEvent *event;
      event = new wxCommandEvent(wxEVENT_TYPE_CHOICE_COMMAND);
      ProcessCommand(event);
    }
  }
}
