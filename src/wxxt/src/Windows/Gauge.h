/*								-*- C++ -*-
 *
 * Purpose: gauge panel item
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

#ifndef Gauge_h
#define Gauge_h

#ifdef __GNUG__
#pragma interface
#endif

class wxPanel;

class wxGauge: public wxItem {
public:
    wxGauge(wxPanel *panel, char *label, int range,
	    int x = -1, int y = -1, int width = -1, int height = -1,
	    long style = wxHORIZONTAL, wxFont *_font = NULL, char *name = "gauge");

    Bool Create(wxPanel *panel, char *label, int range,
		int x = -1, int y = -1, int width = -1, int height = -1,
		long style = wxHORIZONTAL, char *name = "gauge");

    void  SetBezelFace(int WXUNUSED(w)) {}
    void  SetRange(int r);
    void  SetShadowWidth(int WXUNUSED(w)) {}
    void  SetValue(int pos);
private:
    int range, value;
};

#endif // Gauge_h
