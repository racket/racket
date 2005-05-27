/*
 * File:	wb_types.cc
 * Purpose:	Explicit type implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

wxTypeTree *wxAllTypes;

void wxInitStandardTypes(void)
{
  wxREGGLOB(wxAllTypes);
  wxAllTypes = new wxTypeTree;

  // Define explicit type hierarchy
  wxAllTypes->AddType(wxTYPE_WINDOW,      wxTYPE_ANY,                        "window");

  wxAllTypes->AddType(wxTYPE_CANVAS,      wxTYPE_WINDOW,                     "canvas");
  wxAllTypes->AddType(wxTYPE_PANEL,       wxTYPE_CANVAS,                     "panel");
  wxAllTypes->AddType(wxTYPE_FRAME,       wxTYPE_WINDOW,                     "frame");
  wxAllTypes->AddType(wxTYPE_ITEM,        wxTYPE_WINDOW,                     "item");

  wxAllTypes->AddType(wxTYPE_DIALOG_BOX,  wxTYPE_PANEL,                      "dialog box");

  wxAllTypes->AddType(wxTYPE_BUTTON,      wxTYPE_ITEM,                       "button");
  wxAllTypes->AddType(wxTYPE_MESSAGE,     wxTYPE_ITEM,                       "message");
  wxAllTypes->AddType(wxTYPE_CHOICE,      wxTYPE_ITEM,                       "choice");
  wxAllTypes->AddType(wxTYPE_LIST_BOX,    wxTYPE_ITEM,                       "list box");
  wxAllTypes->AddType(wxTYPE_CHECK_BOX,   wxTYPE_ITEM,                       "check box");
  wxAllTypes->AddType(wxTYPE_SLIDER,      wxTYPE_ITEM,                       "slider");
  wxAllTypes->AddType(wxTYPE_MENU,        wxTYPE_ITEM,                       "menu");
  wxAllTypes->AddType(wxTYPE_MENU_BAR,    wxTYPE_ITEM,                       "menu bar");
  wxAllTypes->AddType(wxTYPE_RADIO_BOX,   wxTYPE_ITEM,                       "radio box");
  wxAllTypes->AddType(wxTYPE_GROUP_BOX,   wxTYPE_ITEM,                       "group box");
  wxAllTypes->AddType(wxTYPE_GAUGE,       wxTYPE_ITEM,                       "gauge");
  wxAllTypes->AddType(wxTYPE_TAB_CHOICE,  wxTYPE_ITEM,                       "tab-choice");

  wxAllTypes->AddType(wxTYPE_EVENT,       wxTYPE_ANY,                        "event");
  wxAllTypes->AddType(wxTYPE_MOUSE_EVENT, wxTYPE_EVENT,                      "mouse event");
  wxAllTypes->AddType(wxTYPE_KEY_EVENT,   wxTYPE_EVENT,                      "key event");
  wxAllTypes->AddType(wxTYPE_COMMAND_EVENT,wxTYPE_EVENT,                     "command event");

  wxAllTypes->AddType(wxTYPE_DC,          wxTYPE_ANY,                        "device context");
  wxAllTypes->AddType(wxTYPE_DC_CANVAS,   wxTYPE_DC,                         "canvas device context");
  wxAllTypes->AddType(wxTYPE_DC_PANEL,    wxTYPE_CANVAS,                     "panel device context");
  wxAllTypes->AddType(wxTYPE_DC_POSTSCRIPT,wxTYPE_DC,                        "PostScript device context");
  wxAllTypes->AddType(wxTYPE_DC_PRINTER,  wxTYPE_DC,                         "printer device context");
  wxAllTypes->AddType(wxTYPE_DC_METAFILE, wxTYPE_DC,                         "metafile device context");
  wxAllTypes->AddType(wxTYPE_DC_MEMORY,   wxTYPE_DC,                         "memory device context");

  wxAllTypes->AddType(wxTYPE_PEN,         wxTYPE_ANY,                        "pen");
  wxAllTypes->AddType(wxTYPE_BRUSH,       wxTYPE_ANY,                        "brush");
  wxAllTypes->AddType(wxTYPE_FONT,        wxTYPE_ANY,                        "font");
  wxAllTypes->AddType(wxTYPE_BITMAP,      wxTYPE_ANY,                        "bitmap");
  wxAllTypes->AddType(wxTYPE_ICON,        wxTYPE_BITMAP,                     "icon");
  wxAllTypes->AddType(wxTYPE_CURSOR,      wxTYPE_BITMAP,                     "cursor");
  wxAllTypes->AddType(wxTYPE_METAFILE,    wxTYPE_ANY,                        "metafile");
  wxAllTypes->AddType(wxTYPE_TIMER,       wxTYPE_ANY,                        "timer");
  wxAllTypes->AddType(wxTYPE_COLOUR,      wxTYPE_ANY,                        "colour");

  wxAllTypes->AddType(wxTYPE_LIST,        wxTYPE_ANY,                        "list");
  wxAllTypes->AddType(wxTYPE_STRING_LIST, wxTYPE_LIST,                       "string list");
  wxAllTypes->AddType(wxTYPE_NODE,        wxTYPE_ANY,                        "node");
  wxAllTypes->AddType(wxTYPE_HASH_TABLE,  wxTYPE_ANY,                        "hash table");
  wxAllTypes->AddType(wxTYPE_APP,         wxTYPE_ANY,                        "application");
}

// Explicit type hierarchy required
wxTypeTree::wxTypeTree(void):wxHashTable(wxKEY_INTEGER)
{
}

wxTypeTree::~wxTypeTree(void)
{
}

void wxTypeTree::AddType(WXTYPE type, WXTYPE parent, char *name)
{
  wxTypeDef  *typ;
  typ = new wxTypeDef;
  typ->type = type;
  typ->parent = parent;
  typ->name = copystring(name);
  Put((long)type, (wxObject *)typ);
}

Bool wxSubType(WXTYPE type1, WXTYPE type2)
{
  WXTYPE t;

  if (type1 == type2)
    return TRUE;

  t = type1;
  while (1) {
    wxTypeDef *typ;
    typ = (wxTypeDef *)wxAllTypes->Get((long)t);
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

wxTypeDef::wxTypeDef(void) { name = NULL ; }
wxTypeDef::~wxTypeDef(void) { if (name) delete name ; }
