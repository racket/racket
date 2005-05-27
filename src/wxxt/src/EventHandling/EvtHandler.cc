/*								-*- C++ -*-
 *
 * Purpose: base event handler of windows etc.
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
#pragma implementation "EvtHandler.h"
#endif

#define  Uses_XLib
#define  Uses_wxEvtHandler
#include "wx.h"

//-----------------------------------------------------------------------------
// create/destroy wxEvtHandler
//-----------------------------------------------------------------------------

wxEvtHandler::wxEvtHandler(void)
{
}

wxEvtHandler::~wxEvtHandler(void)
{
}

//-----------------------------------------------------------------------------
// translate between WX and X key symbols
//-----------------------------------------------------------------------------

#include <X11/keysym.h>

typedef struct { KeySym x; int wx; } key_equiv;

#ifndef XK_ISO_Left_Tab
# define     XK_ISO_Left_Tab                                 0xFE20
#endif

static key_equiv key_translation[] = {
    { XK_Shift_L,	WXK_SHIFT },
    { XK_Shift_R,	WXK_SHIFT },
    { XK_Control_L,	WXK_CONTROL },
    { XK_Control_R,	WXK_CONTROL },
#ifdef old_linux_keys
    { XK_Delete,	WXK_BACK },   // results from my private
    { XK_Find,		WXK_DELETE }, // keyboard mapping
#else
    { XK_BackSpace,	WXK_BACK },
    { XK_Delete,	WXK_DELETE },
#endif
    { XK_Clear,		WXK_CLEAR },
    { XK_Tab,		WXK_TAB },
    { XK_ISO_Left_Tab,  WXK_TAB },
    { XK_numbersign,	'#' },
    { XK_Return,	WXK_RETURN },
    { ' ',		WXK_SPACE }, // necessary because of >XK_KP_Space = ' '<
    { XK_Escape,	WXK_ESCAPE },
    { XK_Pause,		WXK_PAUSE },
    { XK_Break,		WXK_PAUSE },
    { XK_Num_Lock,	WXK_NUMLOCK },
    { XK_Scroll_Lock,	WXK_SCROLL },
    { XK_Home,		WXK_HOME },
    { XK_End,		WXK_END },
    { XK_Left,		WXK_LEFT },
    { XK_Right,		WXK_RIGHT },
    { XK_Up,		WXK_UP },
    { XK_Down,		WXK_DOWN },
    { XK_Next,		WXK_NEXT },
    { XK_Prior,		WXK_PRIOR },
    { XK_Menu,		WXK_MENU },
    { XK_Select,	WXK_SELECT },
    { XK_Cancel,	WXK_CANCEL },
    { XK_Print,		WXK_PRINT },
    { XK_Execute,	WXK_EXECUTE },
    { XK_Insert,	WXK_INSERT },
    { XK_Help,		WXK_HELP },
    { XK_KP_Multiply,	WXK_MULTIPLY },
    { XK_KP_Add,	WXK_ADD },
    { XK_KP_Subtract,	WXK_SUBTRACT },
    { XK_KP_Divide,	WXK_DIVIDE },
    { XK_KP_Decimal,	WXK_DECIMAL },
    { XK_KP_Equal,	'=' },
    { XK_KP_Space,	' ' },
    { XK_KP_Tab,	WXK_TAB },
    { XK_KP_Enter,	3 },
    { XK_KP_0,		WXK_NUMPAD0 },
    { XK_KP_1,		WXK_NUMPAD1 },
    { XK_KP_2,		WXK_NUMPAD2 },
    { XK_KP_3,		WXK_NUMPAD3 },
    { XK_KP_4,		WXK_NUMPAD4 },
    { XK_KP_5,		WXK_NUMPAD5 },
    { XK_KP_6,		WXK_NUMPAD6 },
    { XK_KP_7,		WXK_NUMPAD7 },
    { XK_KP_8,		WXK_NUMPAD8 },
    { XK_KP_9,		WXK_NUMPAD9 },
    { XK_F1,		WXK_F1 },
    { XK_F2,		WXK_F2 },
    { XK_F3,		WXK_F3 },
    { XK_F4,		WXK_F4 },
    { XK_F5,		WXK_F5 },
    { XK_F6,		WXK_F6 },
    { XK_F7,		WXK_F7 },
    { XK_F8,		WXK_F8 },
    { XK_F9,		WXK_F9 },
    { XK_F10,		WXK_F10 },
    { XK_F11,		WXK_F11 },
    { XK_F12,		WXK_F12 },
    { XK_F13,		WXK_F13 },
    { XK_F14,		WXK_F14 },
    { XK_F15,		WXK_F15 },
    { XK_F16,		WXK_F16 },
    { XK_F17,		WXK_F17 },
    { XK_F18,		WXK_F18 },
    { XK_F19,		WXK_F19 },
    { XK_F20,		WXK_F20 },
    { XK_F21,		WXK_F21 },
    { XK_F22,		WXK_F22 },
    { XK_F23,		WXK_F23 },
    { XK_F24,		WXK_F24 }
};

int CharCodeXToWX(KeySym key_sym)
{
  // first try translation
  int i;

  for (i=0; i < wxNumberOf(key_translation); ++i) {
    if (key_sym == key_translation[i].x)
      return key_translation[i].wx;
  }

  // try displayable keysyms
  if (key_sym <= 255)
    return (int)key_sym;

  // keysym not handled by wx
  return 0;
}

KeySym CharCodeWXToX(int id)
{
  int i;
  
  if (!id)
    return 0;

  // first try translation
  for (i=0; i < wxNumberOf(key_translation); ++i) {
    if (id == key_translation[i].wx)
      return key_translation[i].x;
  }

  // try displayable keysyms
  if (id <= 255)
    return KeySym(id);

  // keysym not handled by wx
  return 0;
}

Bool wxIsAlt(KeySym key_sym)
{
  if ((key_sym == XK_Alt_L) || (key_sym == XK_Alt_R)
      || (key_sym == XK_Meta_L) || (key_sym == XK_Meta_R)) {
    return TRUE;
  }

  return FALSE;
}
