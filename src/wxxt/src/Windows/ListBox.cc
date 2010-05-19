/*								-*- C++ -*-
 *
 * Purpose: list box panel item
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
#pragma implementation "ListBox.h"
#pragma GCC diagnostic ignored "-Wwrite-strings"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxListBox
#define  Uses_wxStringList
#include "wx.h"
#define  Uses_TraversingEnforcerWidget
#define  Uses_MultiListWidget
#define  Uses_ScrollWinWidget
#include "widgets.h"

#include <ctype.h>

// don't allocate or free for every append or del
#define LIST_CHUNK_SIZE	20
#define MULTILIST	((XfwfMultiListWidget)(X->handle))

#define wxLIST_BOX_WIDTH	70
#define wxLIST_BOX_HEIGHT	50

char *wxchoice_unprotect_amp(char *s);

//-----------------------------------------------------------------------------
// create and destroy wxListBox
//-----------------------------------------------------------------------------

wxListBox::wxListBox(wxPanel *panel, wxFunction func, char *title,
		     Bool multiple, int x, int y, int width, int height,
		     int n, char **_choices, long style, 
		     wxFont *_font, wxFont *_label_font, 
		     char *name) : wxItem(_font)
{
    __type = wxTYPE_LIST_BOX;

    AllowDoubleClick(TRUE);

    choices = client_data = NULL;
    num_choices = 0;
    num_free = 0;
    typepos = 0;
    typetime = 0;

    label_font = (_label_font ? _label_font : wxSYSTEM_FONT);

    Create(panel, func, title, multiple, x, y, width, height,
	   n, _choices, style, name);
}

Bool wxListBox::Create(wxPanel *panel, wxFunction func, char *title,
		       Bool multiple, int x, int y, int width, int height,
		       int n, char **choices, long style, char *name)
{
    wxWindow_Xintern *ph;
    Widget wgt;
    Bool vert;
    long labelw = 0, labelh = 0;

    ChainToPanel(panel, style | ((long)multiple), name);
    
    if (style & wxVERTICAL_LABEL)
      vert = 1;
    else if (style & wxHORIZONTAL_LABEL)
      vert = 0;
    else
      vert = (panel->GetLabelPosition() == wxVERTICAL);

    title = wxGetCtlLabel(title);

    ph = parent->GetHandle();

    // create frame
    wgt = XtVaCreateWidget
	(name, xfwfTraversingEnforcerWidgetClass, ph->handle,
	 XtNlabel,       title,
	 XtNalignment,   vert ? XfwfTop : XfwfTopLeft,
	 XtNbackground,  wxGREY_PIXEL,
	 XtNforeground,  wxBLACK_PIXEL,
	 XtNhighlightColor, wxCTL_HIGHLIGHT_PIXEL,
	 XtNhighlightThickness, 2,
	 XtNfont,        label_font->GetInternalFont(),
#ifdef WX_USE_XFT
	 XtNxfont,       label_font->GetInternalAAFont(),
#endif
	 NULL);
    if (!(style & wxINVISIBLE))
      XtManageChild(wgt);
    else
      XtRealizeWidget(wgt);
    X->frame = wgt;
    // create viewport
    wgt = XtVaCreateManagedWidget
	("viewport", xfwfScrolledWindowWidgetClass, X->frame,
	 XtNhideHScrollbar, TRUE,
	 XtNbackground, wxGREY_PIXEL,
	 XtNdoScroll, FALSE,
	 XtNhighlightThickness, 0,
	 XtNhighlightColor, wxCTL_HIGHLIGHT_PIXEL,
	 XtNtraversalOn, FALSE,
	 NULL);
    X->scroll = wgt;
    // create multi list
    wgt = XtVaCreateManagedWidget
	("list", xfwfMultiListWidgetClass, X->scroll,
	 XtNbackground,     wxWHITE_PIXEL,
	 XtNforeground,     wxBLACK_PIXEL,
	 XtNhighlightBackground,  wxCTL_HIGHLIGHT_PIXEL,
	 XtNhighlightForeground,  wxGREY_PIXEL,
	 XtNfont,           font->GetInternalFont(),
#ifdef WX_USE_XFT
	 XtNmlXftFont,          font->GetInternalAAFont(),
#endif
	 XtNborderWidth,    0,
	 XtNshadeSurplus,   FALSE,
	 XtNdefaultColumns, 1,
	 XtNforceColumns,   TRUE,
	 XtNcursor,         NULL,
	 XtNmaxSelectable,  (multiple & (wxMULTIPLE | wxEXTENDED)) ? 10000 : 1,
	 XtNclickExtends,   (Boolean)(multiple & wxEXTENDED),
	 NULL);
    X->handle = wgt;

    XtVaSetValues(X->scroll, XtNautoAdjustScrollbars, 0, NULL);
    misc_flags |= 8; /* Indicates no auto-scroll. */

    Set(n, choices);
    // propagate key events from frame to scrollwin widget
    XtVaSetValues(X->frame, XtNpropagateTarget, X->handle, NULL);
    // callback
    callback = func;
    XtAddCallback(X->handle, XtNcallback,
		  wxListBox::EventCallback,  (XtPointer)saferef);

    if (title) {
      double w, h;
      char *label_stripped;
      label_stripped = wxchoice_unprotect_amp(title);
      GetTextExtent(label_stripped, &w, &h, NULL, NULL, label_font);
      if (vert)
	labelh = (long)h;
      else
	labelw = (long)w;
    }

    panel->PositionItem(this, x, y,
			(width  > -1 ? width  : (wxLIST_BOX_WIDTH + labelw)),
			(height > -1 ? height : (wxLIST_BOX_HEIGHT + labelh)));
    AddEventHandlers();

    if (style & wxINVISIBLE)
      Show(FALSE);

    return TRUE;
}

wxListBox::~wxListBox(void)
{
    Clear();
}

//-----------------------------------------------------------------------------
// override parent methods
//-----------------------------------------------------------------------------

void wxListBox::SetSize(int x, int y, int width, int height, int flags)
{
    wxItem::SetSize(x, y, width, height, flags);
    OnListSize(width, height);
}

//-----------------------------------------------------------------------------
// change contents of wxListBox
//-----------------------------------------------------------------------------

void wxListBox::Append(char *item)
{
  int i, count, *selections;

  count = GetSelections(&selections);

  if (num_free == 0) {
    char **new_choices, **new_client_data;

    num_free = LIST_CHUNK_SIZE;
    new_choices     = new WXGC_PTRS char *[num_choices+LIST_CHUNK_SIZE];
    new_client_data = new WXGC_PTRS char *[num_choices+LIST_CHUNK_SIZE];
    // copy current choices
    for (i=0; i<num_choices; ++i) {
      new_choices[i] = choices[i];
      new_client_data[i] = client_data[i];
    }
    choices = new_choices;
    client_data = new_client_data;
  }
  // set new item
  {
    char *s;
    s = copystring(item);
    choices[num_choices]     = s;
  }
  client_data[num_choices] = NULL;
  // one choice more, one free space less
  ++num_choices; --num_free;
  SetInternalData();

  while (count--) {
    SetSelection(selections[count], TRUE);
  }
}

void wxListBox::Append(char *item, char *_client_data)
{
    Append(item);
    client_data[num_choices-1] = _client_data;
}

void wxListBox::Clear(void)
{
    if (choices)
      choices = NULL;
    if (client_data)
      client_data = NULL;
    num_choices = num_free = 0;
    SetInternalData();
}

void wxListBox::Delete(int n)
{
    if (0 <= n && n < num_choices) {
      int i, count, *selections;
      
      count = GetSelections(&selections);


      for (i=n+1; i<num_choices; ++i) { // shrink arrays
	choices[i-1] = choices[i];
	client_data[i-1] = client_data[i];
      }
      --num_choices; ++num_free;
      SetInternalData();

      while (count--) {
	if (selections[count] < n)
	  SetSelection(selections[count], TRUE);
	else if (selections[count] > n)
	  SetSelection(selections[count] - 1, TRUE);
      }
    }
}

void wxListBox::InsertItems(int n_items, char **items, int pos)
{
    int     i, j;
    char **new_choices, **new_client_data;

    pos = pos < num_choices ? pos : num_choices;

    new_choices     = new WXGC_PTRS char *[num_choices+n_items];
    new_client_data = new WXGC_PTRS char *[num_choices+n_items];

    for (i = 0; i < pos; ++i) {			// copy choices previous to pos
	new_choices[i] = choices[i];
	new_client_data[i] = client_data[i];
    }
    for (j = 0; j < n_items; ++i, ++j) {		    // copy new choices
	new_choices[i] = items[j];
	new_client_data[i] = NULL;
    }
    for (j = pos; j < num_choices; ++i, ++j) { 	       // copy left old choices
	new_choices[i] = choices[j];
	new_client_data[i] = client_data[j];
    }
    num_choices+=n_items;
    choices = new_choices;
    client_data = new_client_data;

    SetInternalData();
}

void wxListBox::Set(int n, char *_choices[])
{
  int i;
  char **sa;

  // clear ListBox
  Clear();

  // copy choices and initialize client_data
  num_choices = n;
  num_free = LIST_CHUNK_SIZE;
  sa = new WXGC_PTRS char*[n+num_free];
  choices = sa;
  sa = new WXGC_PTRS char*[n+num_free];
  client_data = sa;
  for (i = 0; i < n; i++) {
    char *s;
    s = copystring(_choices[i]);
    choices[i] = s;
    client_data[i] = NULL;
  }
  SetInternalData();
}

void wxListBox::SetInternalData(void)
{
    int ww, hh, p;

    GetSize(&ww, &hh);
    XfwfMultiListSetNewData(
	MULTILIST, num_choices ? choices : (String*)NULL, num_choices,
	ww, TRUE, (Boolean*)NULL);

    OnListSize(0, 0);

    p = GetScrollPos(wxVERTICAL);
    XtVaSetValues(X->handle, XtNoffset, p, NULL);
}

void wxListBox::OnScroll(wxScrollEvent* event)
{
  int p;

  wxItem::OnScroll(event);

  p = GetScrollPos(wxVERTICAL);
  XtVaSetValues(X->handle, XtNoffset, p, NULL);
}

void wxListBox::OnSize(int width, int height)
{
  OnListSize(width, height);
  wxItem::OnSize(width, height);
}

void wxListBox::OnListSize(int, int)
{
  int v, s, n;
  v = NumberOfVisibleItems();
  s = num_choices - v;
  if (s < 0)
    s = 0;
  SetScrollRange(wxVERTICAL, s);
  if (!v)
    v = 1;
  SetScrollPage(wxVERTICAL, v);

  n = GetScrollPos(wxVERTICAL);
  XtVaSetValues(X->handle, XtNoffset, n, NULL);
}

void wxListBox::SetFirstItem(int n)
{
  SetScrollPos(wxVERTICAL, n);
    
  n = GetScrollPos(wxVERTICAL);
  XtVaSetValues(X->handle, XtNoffset, n, NULL);
}

int wxListBox::GetFirstItem()
{
  return GetScrollPos(wxVERTICAL);
}

void wxListBox::SetFirstItem(char *s)
{
  int n;
  if ((n = FindString(s)) > -1) {
    SetFirstItem(n);
  }
}

int wxListBox::NumberOfVisibleItems()
{
  Dimension row_height;
  int cw, ch;

  XtVaGetValues(X->handle, XtNrowHeight, &row_height, NULL);

  GetClientSize(&cw, &ch);
  
  ch = ch / row_height;

  return max(1, ch);
}

//-----------------------------------------------------------------------------
// change state of wxListBox
//-----------------------------------------------------------------------------

void wxListBox::Deselect(int n)
{
    XfwfMultiListUnhighlightItem(MULTILIST, n);
}

int wxListBox::FindString(char *s)
{
  for (int i=0; i<num_choices; ++i) {
    if (!strcmp(s, choices[i]))
      return i;
  }
  return -1;
}

char *wxListBox::GetClientData(int n)
{
    if (0 <= n && n < num_choices)
	return client_data[n];
    return NULL;
}

int wxListBox::GetSelection(void)
{
  XfwfMultiListReturnStruct *rs;
  rs = XfwfMultiListGetHighlighted(MULTILIST);
  if (rs->num_selected >= 1)
    return rs->selected_items[0];
  return -1;
}

static int int_le(const void *a, const void *b)
{
  return (*(int *)a - *(int *)b);
}

int wxListBox::GetSelections(int **list_selections)
{
    XfwfMultiListReturnStruct *rs;
    int *selections, i;

    rs = XfwfMultiListGetHighlighted(MULTILIST);

    selections = new WXGC_ATOMIC int[rs->num_selected];
    for (i = 0; i < rs->num_selected; i++) {
      selections[i] = rs->selected_items[i];
    }
    
    qsort(selections, rs->num_selected, sizeof(int), int_le);

    *list_selections = selections;

    return (rs->num_selected);
}

char *wxListBox::GetString(int n)
{
    if (0 <= n && n < num_choices)
	return choices[n];
    return NULL;
}

char *wxListBox::GetStringSelection(void)
{
    int n;
    if ((n = GetSelection()) > -1)
	return choices[n];
    return NULL;
}

int wxListBox::Number(void)
{
    return num_choices;
}

Bool wxListBox::Selected(int n)
{
    if (0 <= n && n < num_choices)
	return XfwfMultiListIsHighlighted(MULTILIST, n);
    return FALSE;
}

void wxListBox::SetClientData(int n, char *_client_data)
{
    if (0 <= n && n < num_choices)
	client_data[n] = _client_data;
}

void wxListBox::SetSelection(int n, Bool select)
{
	if (0 <= n && n < num_choices) {
		if (select) {
			XfwfMultiListHighlightItem(MULTILIST, n);
		} else {
			XfwfMultiListUnhighlightItem(MULTILIST, n);
		}
	}
}

void wxListBox::SetOneSelection(int n)
{
  if (0 <= n && n < num_choices) {
    if (style & (wxMULTIPLE | wxEXTENDED))
      XfwfMultiListUnhighlightAll(MULTILIST);
    XfwfMultiListHighlightItem(MULTILIST, n);
  }
}

Bool wxListBox::SetStringSelection(char *s)
{
    int n;
    if ((n = FindString(s)) > -1) {
	SetOneSelection(n);
	return TRUE;
    }
    return FALSE;
}

void wxListBox::SetString(int n, char *s)
{
  if (0 <= n && n < num_choices) {
    s = copystring(s);
    choices[n] = s;
    SetInternalData();    
  }
}

void wxListBox::Command(wxCommandEvent *event)
{
  ProcessCommand (event);
}

//-----------------------------------------------------------------------------
// callback for xfwfMultiListWidgetClass
//-----------------------------------------------------------------------------

void wxListBox::EventCallback(Widget WXUNUSED(w),
			     XtPointer dclient, XtPointer dcall)
{
    wxListBox                 *lbox   = (wxListBox *)GET_SAFEREF(dclient);
    XfwfMultiListReturnStruct *rs     = (XfwfMultiListReturnStruct*)dcall;
    wxCommandEvent            *event;

    event = new wxCommandEvent(wxEVENT_TYPE_LISTBOX_COMMAND);

    if (rs->action == XfwfMultiListActionDClick 
	&& lbox->allow_dclicks)
      event->eventType = wxEVENT_TYPE_LISTBOX_DCLICK_COMMAND;

    lbox->ProcessCommand(event);

#ifdef MZ_PRECISE_GC
    XFORM_RESET_VAR_STACK;
#endif
}

extern void wxBell(void);

void wxListBox::OnChar(wxKeyEvent *e)
{
  int delta = 0;

  switch (e->keyCode) {
  case WXK_UP:
    delta = -1;
    break;
  case WXK_PRIOR:
    delta = - NumberOfVisibleItems();
    break;
  case WXK_HOME:
    delta = - num_choices;
    break;
  case WXK_DOWN:
    delta = 1;
    break;
  case WXK_NEXT:
    delta = NumberOfVisibleItems();
    break;
  case WXK_END:
    delta = num_choices;
    break;
  default:
    if ((e->keyCode < 0)
	|| (e->keyCode > 255)
	|| !isprint(e->keyCode))
      return;

    if (e->timeStamp && typetime 
	&& (e->timeStamp - typetime < 500))
      typepos++;
    else
      typepos = 0;
    if (typepos == 16) {
      wxBell();
      typepos = 15;
      return;
    }
    typetime = e->timeStamp;
    typing[typepos] = e->keyCode;
    /* Try to find it */
    {
      int *sels;
      int n;
      n = GetSelections(&sels);
      if (n <= 1) {
	int i, start;
	if (n)
	  start = sels[0];
	else
	  start = 0;
	for (i = 0; i < num_choices; i++) {
	  char *s;
	  int j;
	  s = GetString((start + i) % num_choices);
	  for (j = 0; j <= typepos; j++) {
	    if (toupper(typing[j]) != toupper(s[j]))
	      break;
	  }
	  if (j > typepos) {
	    if (n)
	      delta = ((start + i) % num_choices) - start;
	    else
	      delta = i + 1;
	    break;
	  }
	}
	
	if (i == num_choices) {
	  wxBell();
	  return;
	}
      }
    }
    break;
  }

  if (delta && num_choices) {
    int *sels;
    int n;
    n = GetSelections(&sels);
    if (n <= 1) {
      int s, s2;
      if (n == 1)
	s = sels[0];
      else if (delta < 0)
	s = 2;
      else
	s = -1;

      s2 = s + delta;
      if (s2 < 0)
	s2 = 0;
      else if (s2 >= num_choices)
	s2 = num_choices - 1;
      SetSelection(s2);

      if (s != GetSelection()) {
	wxCommandEvent *event;
	int first, count;

	// Is is visible?
	first = GetFirstItem();
	count = NumberOfVisibleItems() - 1;
	s = GetSelection();
	if (s < first)
	  SetFirstItem(s);
	else if (s > first + count) {
	  SetFirstItem(s - count);
	}

	event = new wxCommandEvent(wxEVENT_TYPE_LISTBOX_COMMAND);
	ProcessCommand(event);
      }
    }
  }
}
