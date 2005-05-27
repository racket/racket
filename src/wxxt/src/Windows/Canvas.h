/*								-*- C++ -*-
 *
 * Purpose: canvas panel item
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

#ifndef Canvas_h
#define Canvas_h

#ifdef __GNUG__
#pragma interface
#endif

class wxColourMap;
class wxCommandEvent;
class wxCursor;
class wxKeyEvent;
class wxMouseEvent;
class wxPanel;
class wxFrame;
class wxGLConfig;

class wxCanvas : public wxItem {
public:
    wxCanvas(wxWindow *parent,
	    int x=-1, int y=-1, int width=-1, int height=-1,
	    int style=0, char *name="canvas", wxGLConfig *gl = NULL);
    ~wxCanvas(void);

    Bool Create(wxPanel *parent,
		int x=-1, int y=-1, int width=-1, int height=-1,
		int style=0, char *name="canvas", wxGLConfig *gl = NULL);

    virtual void GetRefreshSize(int *w, int *h);

    void GetScrollUnitsPerPage(int *x, int *y)
	{ *x = h_units; *y = v_units; };
    void GetVirtualSize(int *x, int *y);
    void Scroll(int x_pos, int y_pos);
    void ScrollPercent(double x_pos, double y_pos);
    virtual void SetScrollbars(int h_pixels, int v_pixels, int x_len, int y_len,
			       int x_page, int y_page, int x_pos=0, int y_pos=0,
			       Bool setVirtualSize = TRUE);
    void SetBackgroundToGray(void);
    void ViewStart(int *x, int *y);
    void WarpPointer(int x, int y);
    // overriden callback methods
    virtual void OnChar(wxKeyEvent *event);
    virtual void Paint(void);

    virtual void SetCanvasBackground(wxColor *c);
    wxColor *GetCanvasBackground();

    virtual void ChangeToGray(Bool gray);

    virtual void Layout(void);

private:
    int	h_size, h_units, h_units_per_page,
	v_size, v_units, v_units_per_page;

    wxColour *bgcol;
};

#endif // Canvas_h
