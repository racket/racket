/*								-*- C++ -*-
 *
 * Purpose: busy cursor
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 2004-2005 PLT Scheme, Inc.
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#define  Uses_XLib
#define  Uses_XtIntrinsic
#define  Uses_wxCursor
#define  Uses_wxWindow
#define  Uses_wxTypeTree
#include "wx.h"
#define  Uses_SimpleWidget
#include "widgets.h"

extern int wxGetBusyState();
extern void wxSetBusyState(int);

void wxXSetNoCursor(wxWindow *win, wxCursor *cursor)
{
  Cursor c;
  Cursor cc;
  wxChildList *cl;
  wxChildNode *node;

  if (cursor || !win->cursor)
    c = None;
  else
    c = GETCURSOR(win->cursor);

  win->user_edit_mode = !!cursor;

  XtVaGetValues(win->X->handle, XtNcursor, &cc, NULL);
  if (cc != c) {
    XtVaSetValues(win->X->handle, XtNcursor, c, NULL);
    if (win->__type == wxTYPE_LIST_BOX) {
      XtVaSetValues(XtParent(win->X->handle), XtNcursor, c, NULL);
    }
  }
  
#ifdef MZ_PRECISE_GC
  if (win->__type == wxTYPE_MENU_BAR)
    return;
#endif

  cl = win->GetChildren();
  for (node = cl->First(); node; node = node->Next()) {
    wxWindow *child;
    child = (wxWindow *) node->Data ();
    wxXSetNoCursor(child, cursor);
  }
}

void wxXSetBusyCursor(wxWindow *win, wxCursor *cursor)
{
  Cursor c;
  wxChildNode *node;
  wxChildList *cl;

  if (cursor)
    c = GETCURSOR(cursor);
  else if (win->cursor)
    c = GETCURSOR(win->cursor);
  else
    c = GETCURSOR(wxSTANDARD_CURSOR);
  
  win->user_edit_mode = !!cursor;

  XtVaSetValues(win->X->handle, XtNcursor, c, NULL);
  if (win->__type == wxTYPE_LIST_BOX) {
    XtVaSetValues(XtParent(win->X->handle), XtNcursor, c, NULL);
  }

  cl = win->GetChildren();
  for (node = cl->First(); node; node = node->Next()) {
    wxWindow *child;
    child = (wxWindow *) node->Data ();
    if (wxSubType(child->__type, wxTYPE_FRAME))
      wxXSetBusyCursor(child, cursor);
    else
      wxXSetNoCursor(child, cursor);
  }
}

static void set_all_cursors(wxObject *o, wxCursor *cursor)
{
  wxChildList *cl;
  wxWindow *win;
  wxChildNode *node;

  cl = wxTopLevelFrames(o);
  for (node = cl->First(); node; node = node->Next()) {
    win = (wxWindow *)node->Data();
    if (win)
      wxXSetBusyCursor(win, cursor);
  }

  XFlush(wxAPP_DISPLAY);
}

/* Busy state valeus:
    0    => not busy, not blanked
    -1   => not busy, blanked
    < -1 => busy, blanked
    > 0  => busy, not blanked
*/

void wxBeginBusyCursor(wxCursor * cursor)
{
  int busy;

  busy = wxGetBusyState();
  if (busy >= 0) {
    busy++;
    wxSetBusyState(busy);
  } else {
    /* Blanked, so "increment", but don't change cursor */
    --busy;
    wxSetBusyState(busy);
    return;
  }

  if (busy == 1) {
    set_all_cursors(NULL, cursor);
  }
}

// Restore cursor to normal
void 
wxEndBusyCursor (void)
{
  int busy;

  busy = wxGetBusyState();
  if ((busy == 0) || (busy == -1))
    return;
  if (busy > 0) {
    busy--;
    wxSetBusyState(busy);
  } else {
    /* Blanked, so "decrement", but don't change cursor */
    busy++;
    wxSetBusyState(busy);
    return;
  }

  if (busy == 0) {
    set_all_cursors(NULL, NULL);
  }
}

Bool wxIsBusy (void)
{
  int busy;
  busy = wxGetBusyState();
  return ((busy > 0) || (busy < -1));
}

static int some_hidden = 0;

void 
wxHideCursor (void)
{
  int busy;
  busy = wxGetBusyState();

  if (busy >= 0) {
    some_hidden++;
    busy = -(busy + 1);
    wxSetBusyState(busy);
    set_all_cursors(NULL, wxBLANK_CURSOR);
  }
}

void
wxUnhideCursor(void)
     /* Not currently used. MrEd calls wxUnhideCursorInFrame, instead */
{
  int busy;
  busy = wxGetBusyState();

  if (busy < 0) {
    if (some_hidden)
      --some_hidden;
    busy = -(busy + 1);
    wxSetBusyState(busy);
    set_all_cursors(NULL, (busy > 0) ? wxHOURGLASS_CURSOR : NULL);
  }  
}

int wxCheckHiddenCursors()
     /* Called by MrEd to check whether cursors need to be unhidden in
	some context. */
{
  if (some_hidden) {
    some_hidden = 0;
    return 1;
  }
  return 0;
}

int wxUnhideCursorInFrame(wxObject *o, int busy)
     /* Called by MrEd to try to unhide cursors in each context. */
{
  if (busy < 0) {
    busy = -(busy + 1);
    set_all_cursors(o, (busy > 0) ? wxHOURGLASS_CURSOR : NULL);
  }
  return busy;
}
