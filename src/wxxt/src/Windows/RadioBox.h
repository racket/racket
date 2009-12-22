/*								-*- C++ -*-
 *
 * Purpose: radio box panel item
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

#ifndef RadioBox_h
#define RadioBox_h

#ifdef __GNUG__
#pragma interface
#endif

class wxBitmap;
class wxPanel;

class wxRadioBox : public wxItem {
public:
    wxRadioBox(wxPanel *panel, wxFunction func, char *label,
	       int x = -1, int y = -1, int width = -1, int height = -1,
	       int n = 0, char **choices = NULL,
	       int num_rows=0, long style = 0, 
	       wxFont *_font = NULL, char *name = "radioBox");
    wxRadioBox(wxPanel *panel, wxFunction func, char *label,
	       int x = -1, int y = -1, int width = -1, int height = -1,
	       int n = 0, wxBitmap **choices = NULL,
	       int num_rows=0, long style = 0, 
	       wxFont *_font = NULL, char *name = "radioBox");
    ~wxRadioBox(void);

    Bool Create(wxPanel *panel, wxFunction func, char *label,
		int x = -1, int y = -1, int width = -1, int height = -1,
		int n = 0, char **choices = NULL,
		int num_rows=0, long style = 0, char *name = "radioBox");
    Bool Create(wxPanel *panel, wxFunction func, char *label,
		int x = -1, int y = -1, int width = -1, int height = -1,
		int n = 0, wxBitmap **choices = NULL,
		int nom_rows=0, long style = 0, char *name = "radioBox");

    void  Enable(int item, Bool enable);
    void  ChangeToGray(Bool enable);

    int   FindString(char *s);
    char  *GetLabel(int item);
    int   GetSelection(void);
    char  *GetStringSelection(void);
    int   Number()				{ return num_toggles; }
    void  SetLabel(int item, char *label);
    void  SetLabel(int item, wxBitmap *bitmap);
    void  SetSelection(int n);
    void  SetStringSelection(char *s);
    Bool  Show(int item, Bool show);
    char  *GetString(int which);
    // root methods to parent class
    virtual void  Enable(Bool enable)	{ wxItem::Enable(enable); }
    virtual char  *GetLabel(void)	{ return wxItem::GetLabel(); }
    virtual void  SetLabel(char *label)	{ wxItem::SetLabel(label); }
    virtual Bool  Show(Bool show)	{ return wxItem::Show(show); }
    void Command(wxCommandEvent *event);
    void SetSelectedButtonFocus();

    int ButtonFocus(int which);

private:
#   ifdef Have_Xt_Types
    static void EventCallback(Widget w, XtPointer clientData, XtPointer ptr);
#   endif

    void* toggles; // of type Widget*
    Bool* enabled;
    wxBitmap **bm_labels, **bm_label_masks;
    int   num_toggles;
};

#endif // RadioBox_h
