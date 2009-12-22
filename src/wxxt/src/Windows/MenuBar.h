/*								-*- C++ -*-
 *
 * Purpose: menu bar class
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

#ifndef MenuBar_h
#define MenuBar_h

#ifdef __GNUG__
#pragma interface
#endif

typedef void *wxMenuItem;

class wxPanel;
class wxMenu;

class wxMenuBar : public wxItem {
public:
    wxMenuBar(void);
    ~wxMenuBar(void);

    Bool Create(wxPanel *panel);
    void Destroy(void);

    int Number(void);

    // add menu to menubar
    void  Append(wxMenu *menu, char *title);
    Bool  Delete(wxMenu *menu, int pos = 0);
    // modify items
    void  Check(long id, Bool flag);
    Bool  Checked(long id);
    void  Enable(long id, Bool flag);
    void  EnableTop(int pos, Bool flag);
    char  *GetHelpString(long id);
    char  *GetLabel(long id);
    char  *GetLabelTop(int pos);
    void  SetHelpString(long id, char *help);
    void  SetLabel(long id, char *label);
    void  SetLabelTop(int pos, char *label);
    // search for item by label
    int   FindMenuItem(char *menu, char *label);
    void Stop();
    int InProgress();
    void SelectAMenu(wxMenu *m = NULL);
private:
    // search for internal data by id
    wxMenuItem  *FindItemForId(long id, wxMenu **menu=NULL);
    // callback functions
#   ifdef Have_Xt_Types
    static void SelectEventCallback(Widget, XtPointer, XtPointer);
    static void CommandEventCallback(Widget, XtPointer, XtPointer);
#   endif
    // internal representation
    wxMenuItem	*top;		// first menu item
    wxMenuItem	*last;		// last menu item for wxMenu::Append
    wxMenuItem  *help;		// help menu pushed right
    wxMenuItem  *topdummy;
};

#endif // MenuBar_h
