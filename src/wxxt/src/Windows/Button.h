/*								-*- C++ -*-
 *
 * Purpose: button panel item
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

#ifndef Button_h
#define Button_h

#ifdef __GNUG__
#pragma interface
#endif

class wxBitmap;
class wxPanel;

class wxButton : public wxItem {
public:
    ~wxButton(void);

    wxButton(wxPanel *panel, wxFunction func, char *label,
	     int x=-1, int y=-1, int width=-1, int height=-1,
	     long style=0, wxFont *_font = NULL, char *name="button");
    wxButton(wxPanel *panel, wxFunction func, wxBitmap *bitmap,
	     int x=-1, int y=-1, int width=-1, int height=-1,
	     long style=0, wxFont *_font = NULL, char *name = "button");
    
    Bool Create(wxPanel *panel, wxFunction func, char *label,
		int x=-1, int y=-1, int width=-1, int height=-1,
		long style=0, char *name="button");
    Bool Create(wxPanel *panel, wxFunction func, wxBitmap *bitmap,
		int x=-1, int y=-1, int width=-1, int height=-1,
		long style=0, char *name="button");

    void AllowResize(Bool allow);
    void Command(wxCommandEvent* event);
    void SetAlignment(long alignment);
    void SetDefault(void);
    void SetLabel(char *label);
    void SetLabel(wxBitmap *bitmap);

    virtual char  *GetLabel(void);

    virtual void ChangeToGray(Bool gray);

    void SetBorder(Bool on);

private:
#   ifdef Have_Xt_Types
    static void EventCallback(Widget w, XtPointer clientData, XtPointer ptr);
#   endif
    wxBitmap *bm_label, *bm_label_mask;
};

#endif // Button_h
