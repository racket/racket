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

#ifndef Choice_h
#define Choice_h

#ifdef __GNUG__
#pragma interface
#endif

class wxMenu;
class wxPanel;

// Choice item
class wxChoice : public wxItem {
public:
    wxChoice(wxPanel *panel, wxFunction func, char *label,
	     int x = -1, int y = -1, int width = -1, int height = -1,
	     int n = 0, char **choices = NULL,
	     long style = 0, wxFont *_font = NULL, char *name = "choice");
    ~wxChoice(void);

    Bool Create(wxPanel *panel, wxFunction func, char *label,
		int x = -1, int y = -1, int width = -1, int height = -1,
		int n = 0, char **choices = NULL,
		long style = 0, char *name = "choice");

    void  Append(char *item);
    void  Clear(void);
    int   FindString(char *s);
    int   GetSelection(void) { return selection; }
    char  *GetString(int n);
    char  *GetStringSelection(void);
    void  SetSelection(int n);
    Bool  SetStringSelection(char *s);
    inline int Number() { return num_choices; }

    /* MATTHEW: [6] */
    virtual void  GetSize(int *width, int *height);

    // can't handle them
    int   GetColumns(void) { return 1; }
    void  SetColoumns(int WXUNUSED(n=1)) {};

    void Command(wxCommandEvent *event);

    void OnEvent(wxMouseEvent*);
    void OnChar(wxKeyEvent*);

    virtual void ChangeToGray(Bool gray);

private:
#   ifdef Have_Xt_Types
    static void EventCallback(Widget, XtPointer, XtPointer);
    static void MenuEventCallback(wxObject*, wxCommandEvent*);
#endif
    wxMenu *choice_menu;
    int    num_choices;
    int    selection;
};

#endif // Choice_h
