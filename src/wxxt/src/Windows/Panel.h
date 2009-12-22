/*								-*- C++ -*-
 *
 * Purpose: base class for all panels
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

#ifndef Panel_h
#define Panel_h

#ifdef __GNUG__
#pragma interface
#endif

class wxButton;
class wxColour;
class wxCommandEvent;
class wxItem;

class wxPanel : public wxWindow {
public:
    wxPanel(void); 
    wxPanel(wxWindow *parent, int x=-1, int y=-1, int width=-1, int height=-1,
	    int style=0, char *name="panel");
    // panel creation
    Bool  Create(wxPanel *parent, 
		 int x=-1, int y=-1, int width=-1, int height=-1,
		 int style=0, char *name="panel");
    // resize/layout panel
    virtual void  GetClientSize(int *width, int *height);
    virtual void  Fit(void);
    virtual void  Layout(void);
    // data retrieved from wxItem and it's children
    // position of labels
    int   GetLabelPosition(void)           { return label_pos; }
    void  SetLabelPosition(int position)   { label_pos = position; }
    // positioning of items
    void  GetCursor(int *x, int *y);
    void  SetItemCursor(int x, int y);
    int   GetHorizontalSpacing(void)  { return h_space; }
    int   GetVerticalSpacing(void)    { return v_space; }
    void  NewLine(int pixels = 0);
    void  PositionItem(wxWindow *win, int x, int y, int width, int height);
    void  SetHorizontalSpacing(int sp)  { h_space = sp; }
    void  SetVerticalSpacing(int sp)    { v_space = sp; }
    void  Tab(int pixels = 0);
    // default item
    wxButton  *GetDefaultItem(void)  { return default_item; }
    // virtual event functions
    virtual void  OnDefaultAction(wxItem *item);
    // drawing
    wxPanelDC* GetPanelDC(void) { return dc; }
    virtual void ChangeToGray(Bool gray);
    virtual void ReleaseAllFocus();
    virtual Bool  WantsFocus(void);
protected:
    friend class wxButton;	// allow access to default_item

    wxButton  *default_item;	// executed on default action
    int       label_pos;	// where to put the label
    int       cursor_x, cursor_y,
	      h_space, v_space,
              v_line_extent; // for positioning of items
};

#endif // Panel_h
