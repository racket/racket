/*								-*- C++ -*-
 *
 * Purpose: type tree
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
#pragma implementation "TypeTree.h"
#endif

#define  Uses_wxTypeTree
#include "wx.h"

wxTypeDef::wxTypeDef()
: wxObject(FALSE) 
{ 
  name = NULL; 
  __type = wxTYPE_TYPEDEF;
}

typedef struct {
    WXTYPE my_type;
    WXTYPE parent_type;
    char   *my_name;
} wxInitType;

static wxInitType init_types[] = {

    { wxTYPE_WINDOW,		wxTYPE_ANY,		"window"},

    { wxTYPE_PANEL,		wxTYPE_WINDOW,		"panel"},
    { wxTYPE_FRAME,		wxTYPE_PANEL,		"frame"},
    { wxTYPE_DIALOG_BOX,	wxTYPE_FRAME,		"dialog-box"},

    { wxTYPE_BUTTON,		wxTYPE_ITEM,		"button"},
    { wxTYPE_CHECK_BOX,		wxTYPE_ITEM,		"check-box"},
    { wxTYPE_CHOICE,		wxTYPE_ITEM,		"choice"},
    { wxTYPE_CANVAS,		wxTYPE_ITEM,		"canvas"},
    { wxTYPE_GAUGE,		wxTYPE_ITEM,		"gauge"},
    { wxTYPE_GROUP_BOX,		wxTYPE_ITEM,		"group-box"},
    { wxTYPE_ITEM,		wxTYPE_WINDOW,		"item"},
    { wxTYPE_LIST_BOX,		wxTYPE_ITEM,		"list-box"},
    { wxTYPE_MENU,		wxTYPE_ANY,		"menu"},
    { wxTYPE_MENU_BAR,		wxTYPE_ITEM,		"menu-bar"},
    { wxTYPE_MESSAGE,		wxTYPE_ITEM,		"message"},
    { wxTYPE_RADIO_BOX,		wxTYPE_ITEM,		"radio-box"},
    { wxTYPE_SCROLL_BAR,	wxTYPE_ITEM,		"scrollbar"},
    { wxTYPE_SLIDER,		wxTYPE_ITEM,		"slider"},

    { wxTYPE_EVENT,		wxTYPE_ANY,		"event"},
    { wxTYPE_COMMAND_EVENT,	wxTYPE_EVENT,		"command-event"},
    { wxTYPE_KEY_EVENT,		wxTYPE_EVENT,		"key-event"},
    { wxTYPE_MOUSE_EVENT,	wxTYPE_EVENT,		"mouse-event"},

    { wxTYPE_DC,		wxTYPE_ANY,		"device-context"},
    { wxTYPE_DC_WINDOW,		wxTYPE_DC,		"window-device-context"},
    { wxTYPE_DC_MEMORY,		wxTYPE_DC_CANVAS,	"memory-device-context"},
    { wxTYPE_DC_METAFILE,	wxTYPE_DC,		"metafile-device-context"},
    { wxTYPE_DC_POSTSCRIPT,	wxTYPE_DC,		"PostScript-device-context"},
    { wxTYPE_DC_PRINTER,	wxTYPE_DC,		"printer-device-context"},

    { wxTYPE_BITMAP,		wxTYPE_ANY,		"bitmap"},
    { wxTYPE_BRUSH,		wxTYPE_ANY,		"brush"},
    { wxTYPE_COLOUR,		wxTYPE_ANY,		"colour"},
    { wxTYPE_COLOURMAP,         wxTYPE_ANY,             "colourmap"},
    { wxTYPE_CURSOR,		wxTYPE_BITMAP,		"cursor"},
    { wxTYPE_PEN,		wxTYPE_ANY,		"pen"},
    { wxTYPE_FONT,		wxTYPE_ANY,		"font"},
    { wxTYPE_ICON,		wxTYPE_BITMAP,		"icon"},
    { wxTYPE_METAFILE,		wxTYPE_ANY,		"metafile"},
    { wxTYPE_TIMER,		wxTYPE_ANY,		"timer"},

    { wxTYPE_APP,		wxTYPE_ANY,		"application"},

    { wxTYPE_HASH_TABLE,	wxTYPE_ANY,		"hash-table"},
    { wxTYPE_LIST,		wxTYPE_ANY,		"list"},
    { wxTYPE_NODE,		wxTYPE_ANY,		"node"},
    { wxTYPE_STRING_LIST,	wxTYPE_LIST,		"string-list"},

    { wxTYPE_CONSTRAINTS,	wxTYPE_ANY,		"constraint-set"},
    { wxTYPE_TYPEDEF,		wxTYPE_ANY,		"type-record"},

};

//-----------------------------------------------------------------------------
// wxTypeTree implementation
//-----------------------------------------------------------------------------

wxTypeTree::wxTypeTree(void) : wxHashTable(wxKEY_INTEGER)
{
  unsigned int i;

  // Define explicit type hierarchy
  for (i = 0; i < wxNumberOf(init_types); i++) {
    AddType(init_types[i].my_type,
	    init_types[i].parent_type,
	    init_types[i].my_name);
  }
}

wxTypeTree::~wxTypeTree(void)
{
}

void wxTypeTree::AddType(WXTYPE type, WXTYPE parent, char *name)
{
    wxTypeDef *typ;
    typ = DEBUG_NEW wxTypeDef;
    typ->type   = type;
    typ->parent = parent;
    typ->name   = copystring(name);
    Put((long)type, (wxObject*)typ);
}

//-----------------------------------------------------------------------------
// global functions to get type infos
//-----------------------------------------------------------------------------

Bool wxSubType(WXTYPE type1, WXTYPE type2)
{
  WXTYPE t;

  if (type1 == type2)
    return TRUE;
  
  t = type1;
  while (TRUE) {
    wxTypeDef *typ;
    typ = (wxTypeDef*)wxAllTypes->Get((long)t);
    if (!typ)
      return FALSE;
    if (type2 == typ->parent)
      return TRUE;
    t = typ->parent;
  }
}

char *wxGetTypeName(WXTYPE type)
{
  wxTypeDef *typ;

  if (type == wxTYPE_ANY)
    return "any";
  typ = (wxTypeDef *)wxAllTypes->Get((long)type);
  if (!typ)
    return NULL;
  return typ->name;
}
