/*								-*- C++ -*-
 *
 * Purpose: wxWindows font handling
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

#ifndef Font_h
#define Font_h

#ifdef __GNUG__
#pragma interface
#endif

extern char *wx_font_spec[];

#ifdef WX_USE_XFT
# define wxFontStruct XftFont
#else
# define wxFontStruct XFontStruct
#endif

class wxFont : public wxObject {
public:
    wxFont(void);
    wxFont(int PointSize, int FontIdOrFamily, int Style, int Weight,
	   Bool underlined = FALSE, int smoothing = wxSMOOTHING_DEFAULT, 
	   Bool sip = FALSE, double Rotation = 0.0);
    wxFont(int PointSize, const char *Face, int Family, int Style, int Weight, 
	   Bool underlined = FALSE, int smoothing = wxSMOOTHING_DEFAULT, 
	   Bool sip = FALSE);
    ~wxFont(void);

    void InitFont(void);

    int   GetPointSize(void)     { return point_size; }
    int   GetFamily(void)        { return family; }
    char  *GetFamilyString(void) { return wx_font_spec[family]; }
    int   GetFontId(void)        { return font_id; }
    char  *GetFaceString(void);
    int   GetStyle(void)         { return style; }
    char  *GetStyleString(void)  { return wx_font_spec[style]; }
    int   GetWeight(void)	 { return weight==wxNORMAL_WEIGHT ? wxNORMAL : weight; }
    char  *GetWeightString(void) { return wx_font_spec[weight]; }
    Bool  GetUnderlined(void)    { return underlined; }
    int   GetSmoothing(void)     { return smoothing; }
    int   GetSizeInPixels(void)  { return size_in_pixels; }

    Bool  ScreenGlyphAvailable(int c, Bool for_label = FALSE);

    wxFont *GetRotated(double angle);
    int CanRotate();

    void  *GetInternalFont(double scale_x = 1.0, double scale_y = 1.0, double angle = 0.0); // return type XFontStruct*
    void  *GetInternalAAFont(double scale_x = 1.0, double scale_y = 1.0, double angle = 0.0); // return type wxFontStruct*

#ifdef WX_USE_XFT
    int HasAASubstitutions(void);
    void *GetNextAASubstitution(int index, int cval, double scale_x, double scale_y, double angle);
#endif

private:
    wxList *scaled_xfonts;
#ifdef WX_USE_XFT
    wxList *scaled_xft_fonts;
    wxList *substitute_xft_fonts;
#endif
    wxList *rotated_fonts;
    short  point_size;
    short  family, style, weight;
    Bool   underlined, size_in_pixels;
    int    font_id;
    int    smoothing;
    double  rotation;
    char   *main_screen_name;
};

class wxFontList : public wxObject {
   wxChildList *list;
public:
    wxFontList(void);
    ~wxFontList(void);

    void AddFont(wxFont *font);

    wxFont *FindOrCreateFont(int PointSize, int FontIdOrFamily, int Style, 
			     int Weight, Bool underline = FALSE,
			     int smoothing = wxSMOOTHING_DEFAULT, 
			     Bool sip = FALSE);
    wxFont *FindOrCreateFont(int PointSize, const char *Face, int Family, 
			     int Style, int Weight,
			     Bool underline = FALSE,
			     int smoothing = wxSMOOTHING_DEFAULT, 
			     Bool sip = FALSE);
};

extern int wxGetControlFontSize();

#endif // Font_h
