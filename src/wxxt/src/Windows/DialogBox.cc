/*								-*- C++ -*-
 *
 * Purpose: dialog box
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

#ifdef __GNUG__
#pragma implementation "DialogBox.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxDialogBox
#define  Uses_wxList
#define  Uses_wxTypeTree
#include "wx.h"

wxDialogBox::wxDialogBox(void) : wxFrame()
{
  __type = wxTYPE_DIALOG_BOX;
}

wxDialogBox::wxDialogBox(wxWindow *parent, char *title, Bool _modal, int x,
			 int y, int width, int height, long style, char *name)
    : wxFrame()
{
    __type = wxTYPE_DIALOG_BOX;

    style |= wxTRANSIENT;

    Create((wxFrame *)parent, title, _modal, x, y, width, height, style, name);
}

Bool wxDialogBox::Create(wxFrame *parent, char *title, Bool _modal, int x,
			 int y, int width, int height, long style, char *name)
{
    style |= wxTRANSIENT;

    (void)wxFrame::Create(parent, title, x, y, width, height, style, name);

    return TRUE;
}

extern void wxDispatchEventsUntil(int (*)(void *), void *);

static int CheckDialogShowing(void *data)
{
  return !((wxDialogBox *)data)->ModalShowing();
}

Bool wxDialogBox::Show(Bool show)
{
  // handle modality
  if (show) {
    wxChildNode *cnode;
    wxChildList *tlf;

    // popup/popdown frame
    wxFrame::Show(show);
    SetShown(show);
      
    if (!restore_disabled_windows) {
      wxList *disabled_windows;

      wxPushModalWindow(this, this);
      
      disabled_windows = new wxList;
      
      tlf = wxTopLevelFrames(this);
      for (cnode = tlf->First(); cnode; cnode = cnode->Next()) {
	wxWindow *w;
	w = (wxWindow *)cnode->Data();
	if (w && w != this && cnode->IsShown()) {
	  disabled_windows->Append(w);
	  w->InternalEnable(FALSE);
	}
      }
      
      restore_disabled_windows = disabled_windows;
    }

    wxDispatchEventsUntil(CheckDialogShowing, (void *)this);
  } else {
    if (restore_disabled_windows) {
      wxList *disabled_windows = restore_disabled_windows;
      wxNode *node;

      restore_disabled_windows = NULL;
      
      for (node = disabled_windows->First(); node; node = node->Next()) {
	wxWindow *w;
	w = (wxWindow *)node->Data();
	w->InternalEnable(TRUE);
      }

      wxPopModalWindow(this, this);

      wxFrame::Show(FALSE);
      SetShown(FALSE);
      
      XFlush(XtDisplay(wxAPP_TOPLEVEL));
      XSync(XtDisplay(wxAPP_TOPLEVEL), FALSE);
    }
  } 
  
  return TRUE;
}

