/*								-*- C++ -*-
 *
 * Purpose: base class for all panel items
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
#pragma implementation "Item.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxItem
#define  Uses_wxPanel
#define  Uses_wxBitmap
#include "wx.h"
#include "widgets.h" // for <X11/StringDefs.h>

//-----------------------------------------------------------------------------
// wxItem constructor
//-----------------------------------------------------------------------------

wxItem::wxItem(wxFont *_font) : wxWindow()
{ 
    __type = wxTYPE_ITEM;
    font = (_font ? _font : wxSYSTEM_FONT);
    callback = NULL;
};

//-----------------------------------------------------------------------------
// get and set colours
//-----------------------------------------------------------------------------

void wxItem::ChainToPanel(wxPanel *panel, long _style, char *name)
{
    if (!panel)
	wxFatalError("created without a panel!", name ? name : "item");
    parent = panel;
    parent->AddChild(this);

    style       = _style;
}

//-----------------------------------------------------------------------------
// functions to execute item
//-----------------------------------------------------------------------------

void wxItem::Command(wxCommandEvent *WXUNUSED(event))
{
}

void wxItem::ProcessCommand(wxCommandEvent *event)
{
  if (callback) {
    callback(this, event);
  } else {
    if (parent)
      parent->OnCommand(this, event);
  }
}

char *wxGetCtlLabel(char *label)
{
  if (!label)
    return NULL;

#if 0
  wxGetLabelAndKey(label, &label, NULL);
#endif

  return label;
}

wxBitmap *wxItem::CheckMask(wxBitmap *bm)
{
  int can_x_render;
  wxBitmap *mbm;

  if (!bm)
    return NULL;

  mbm = bm->GetMask();
  if (!mbm)
    return NULL;

#ifdef WX_USE_XRENDER
  can_x_render = wxXRenderHere();
#else
  can_x_render = 0;
#endif

  /* If no X render and non-mono mask, then we have to manually apply
     the alpha mask. See wxBitmap::GetLabelPixmap(). */
  if (!can_x_render && (mbm->GetDepth() != 1))
    return NULL;

  if ((mbm->GetWidth() == bm->GetWidth())
      && (mbm->GetHeight() == bm->GetHeight())) {
    if (mbm->selectedIntoDC >= 0) {
      if (mbm->GetDepth() > 1) {
	/* Need mask */
	mbm = mbm->GetMaskBit();
	if (!mbm)
	  return NULL;
      }
      mbm->selectedIntoDC++;
      return mbm;
    }
  }

  return NULL;
}
