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

#ifndef EvtHandler_h
#define EvtHandler_h

#ifdef __GNUG__
#pragma interface
#endif

class wxCommandEvent;
class wxItem;
class wxKeyEvent;
class wxMouseEvent;
class wxScrollEvent;
class wxWindow;

class wxEvtHandler : public wxObject {
public:
    wxEvtHandler(void);
    ~wxEvtHandler(void);

    // virtual event functions
    inline virtual void OnActivate(Bool WXUNUSED(active)) {};
    inline virtual void OnChangeFocus(wxItem *WXUNUSED(from),
				      wxItem *WXUNUSED(to)) {};
    inline virtual void OnChar(wxKeyEvent* WXUNUSED(event)) {};
    inline virtual Bool OnCharHook(wxKeyEvent* WXUNUSED(event)) { return FALSE; }
    inline virtual Bool OnClose(void) { return TRUE; };
    inline virtual void OnCommand(wxWindow* WXUNUSED(win),
				  wxCommandEvent* WXUNUSED(event)) {};
    inline virtual void OnDefaultAction(wxItem *WXUNUSED(initiatingItem)) {};
    inline virtual void OnDropFile(char *WXUNUSED(file)) {};
    inline virtual void OnEvent(wxMouseEvent* WXUNUSED(event)) {};
    inline virtual Bool OnFunctionKey(wxKeyEvent* WXUNUSED(event)) { return FALSE; };
    inline virtual void OnKillFocus(void) {};
    inline virtual void OnLeftClick(int WXUNUSED(x), int WXUNUSED(y),
				    int WXUNUSED(keys)) {};
    inline virtual void OnMenuCommand(long WXUNUSED(cmd)) {};
    inline virtual void OnMenuSelect(long WXUNUSED(cmd)) {};
    inline virtual void OnMove(int WXUNUSED(x), int WXUNUSED(y)) {};
    inline virtual void OnPaint(void) {};
    inline virtual void OnRightClick(int WXUNUSED(x), int WXUNUSED(y),
				     int WXUNUSED(keys)) {};
    inline virtual void OnScroll(wxScrollEvent* WXUNUSED(event)) {};
    inline virtual void OnSelect(Bool WXUNUSED(select)) {};
    inline virtual void OnSetFocus(void) {};
    inline virtual void OnSize(int WXUNUSED(width), int WXUNUSED(height)) {};
};

#if defined (Uses_XtIntrinsic) || defined (Uses_XtIntrinsicP) || defined (Uses_XLib)
KeySym	CharCodeWXToX(int id);
int	CharCodeXToWX(KeySym key_sym);
#endif

#endif // EvtHandler_h
