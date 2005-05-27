/*								-*- C++ -*-
 *
 * Purpose: classes to cover colours and colourmaps
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

#ifndef Colour_h
#define Colour_h

#ifndef MZ_PRECISE_GC
# ifdef __GNUG__
# pragma interface
# endif
#endif

#ifdef Have_X_Types
// easier access to X-Colormap
#define GETCOLORMAP(obj) (*((Colormap*)((obj)->GetHandle())))
#endif

class wxColour_Xintern;
class wxColourMap_Xintern;

class wxColourMap;

class wxColour : public wxObject { // colour representation
public:
    wxColour(void);
    wxColour(unsigned char r, unsigned char g, unsigned char b);
    wxColour(wxColour* col);
    wxColour(const char *col);
    ~wxColour(void);

    wxColour* CopyFrom(wxColour*);
    wxColour* CopyFrom(const char*);

#ifndef MZ_PRECISE_GC
    wxColour& operator =(wxColour& x) { 
      printf("Error: shouldn't use = on color objects anymore\n");
      CopyFrom(&x);
      return *this;
    }
#endif

    Bool Ok(void) { return (X!=NULL); }

    void Get(unsigned char *r, unsigned char *b, unsigned char *g);
    void Set(unsigned char r, unsigned char b, unsigned char g);

    unsigned char Red(void);
    unsigned char Green(void);
    unsigned char Blue(void);

    // alloc and free X pixel value
    unsigned long GetPixel(wxColourMap* cmap=wxAPP_COLOURMAP, Bool is_color=1, Bool fg=1);
    void FreePixel(Bool del);

    inline Bool  IsMutable(void) { return !locked; } 
    inline void  Lock(int d) { locked += d; }

private:
    friend class wxColourMap;

    wxColour_Xintern* X; // the encapsulated X representation
    int locked;
};

class wxColourMap : public wxObject { // colourmap representation
public:
    wxColourMap(Bool priv=TRUE);
    ~wxColourMap(void);

    void *GetHandle(void); // return Colormap*

    Bool Ok(void) { return (X!=NULL); }
private:
    friend class wxColour;
    friend class wxColourDatabase;

    wxColourMap_Xintern* X; // the encapsulated X representation
};

class wxColourDatabase : public wxList { // colour database representation
public:
    wxColourDatabase(void);
    ~wxColourDatabase(void);

    wxColour *FindColour(const char *colour);
    char *FindName(wxColour* colour);
};

#endif // Colour_h
