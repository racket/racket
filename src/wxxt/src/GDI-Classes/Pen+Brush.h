/*								-*- C++ -*-
 *
 * Purpose: pen and brush classes needed for drawing
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

#ifndef PenBrush_h
#define PenBrush_h

#ifdef __GNUG__
#pragma interface
#endif

class wxBitmap;

typedef unsigned char wxDash;

class wxBrush : public wxObject { // brush representation
public:
    wxBrush(void);
    wxBrush(wxColour* col, int style);
    wxBrush(const char *col, int style);
    ~wxBrush(void);

    wxColour*  GetColour(void)       { return colour; }
    wxBitmap   *GetStipple(void)     { return stipple; }
    int        GetStyle(void)        { return style; }

    void SetColour(wxColour* col)          { colour->CopyFrom(col); }
    void SetColour(const char *col)        { colour->CopyFrom(col); }
    void SetColour(char r, char g, char b) { colour->Set(r, g, b); }
    void SetStipple(wxBitmap *s);
    void SetStyle(int s)                   { style = s; }

    inline Bool  IsMutable(void)          { return !locked; }
    inline void  Lock(int d)              { locked += d; colour->Lock(d); }

private:
    wxColour  *colour;
    short     locked;
    short     style;
    wxBitmap  *stipple;
};

class wxPen : public wxObject { // pen representation
public:
    wxPen(void);
    wxPen(wxColour* col, double width, int style);
    wxPen(const char *col, double width, int style);
    ~wxPen(void);

    int       GetCap(void)          { return cap; }
    wxColour* GetColour(void)       { return colour; }
    int       GetDashes(wxDash **p) { *p = dash; return nb_dash; }
    int       GetJoin(void)         { return join; }
    wxBitmap  *GetStipple(void)     { return stipple; }
    int       GetStyle(void)        { return style; }
    int       GetWidth(void)        { return (int)width; }
    double     GetWidthF(void)       { return width; }

    void SetCap(int c)                     { cap = c; }
    void SetColour(wxColour* col)          { colour->CopyFrom(col); }
    void SetColour(const char *col)        { colour->CopyFrom(col); }
    void SetColour(char r, char g, char b) { colour->Set(r, g, b); }
    void SetDashes(int n, wxDash *d)       { nb_dash = n; dash = d; }
    void SetJoin(int j)                    { join = j; }
    void SetStipple(wxBitmap *s);
    void SetStyle(int s)                   { style = s; }
    void SetWidth(double w)                 { width = w; }

    inline Bool  IsMutable(void)          { return !locked; }
    inline void  Lock(int d)              { locked += d; colour->Lock(d); }

private:
    int       nb_dash;
    wxDash    *dash;
    wxColour  *colour;
    double     width;
    int       locked;
    short      style;
    int       join;
    int       cap;
    wxBitmap  *stipple;
};

class wxBrushList : public wxObject {
    wxChildList *list;
public:
    wxBrushList(void);
    ~wxBrushList(void);

    void  AddBrush(wxBrush *Brush);
    wxBrush *FindOrCreateBrush(wxColour *colour, int style);
    wxBrush *FindOrCreateBrush(char *colour, int style);
};

class wxPenList : public wxObject {
    wxChildList *list;
public:
    wxPenList(void);
    ~wxPenList(void);

    void  AddPen(wxPen *pen);
    wxPen *FindOrCreatePen(wxColour *colour, double width, int style);
    wxPen *FindOrCreatePen(char *colour, double width, int style);
};

#endif // PenBrush_h
