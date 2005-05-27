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

#ifdef __GNUG__
#pragma implementation "Colour.h"
#endif

#define  Uses_XLib
#define  Uses_wxColour
#define  Uses_wxHashTable
#include "wx.h"

#include <ctype.h>

// shift between wxWindows RGB- and XColor RGB-values
// (necessary because the values specify an intensity)
#define SHIFT (8*(sizeof(short int)-sizeof(char)))

extern "C" { 
#include "XWidgets/wxAllocColor.h"
};

extern Colormap wx_default_colormap;

//-----------------------------------------------------------------------------
// private data of wxColour and wxColourMap
//-----------------------------------------------------------------------------

class wxColour_Xintern {
public:
    XColor   xcolor;
    Bool     have_pixel;
    Colormap xcolormap;
};

class wxColourMap_Xintern {
public:
    Colormap xcolormap;
    Bool     priv;
};

//-----------------------------------------------------------------------------
// wxColour
//-----------------------------------------------------------------------------

/* Since destructor doesn't do anything: */
#define COLOR_CLEANUP WXGC_NO_CLEANUP

wxColour::wxColour(void)
: wxObject(COLOR_CLEANUP)
{
    __type = wxTYPE_COLOUR;

    X = NULL; // not Ok
    locked = 0;
}

wxColour::wxColour(wxColour *col)
: wxObject(COLOR_CLEANUP)
{
  __type = wxTYPE_COLOUR;
  
  locked = 0;
  
  CopyFrom(col);
}

wxColour::wxColour(const char *col)
: wxObject(COLOR_CLEANUP)
{
    __type = wxTYPE_COLOUR;

    locked = 0;

    CopyFrom(col);
}

wxColour::wxColour(unsigned char r, unsigned char g, unsigned char b)
: wxObject(COLOR_CLEANUP)
{
    __type = wxTYPE_COLOUR;

    locked = 0;
    X = NULL; Set(r, g, b); // set RGB-values
}

wxColour::~wxColour(void)
{
  /* If you do anything important here, be sure to change
     COLOR_CLEANUP */

  /* This doesn't count as important, because the MrEd
     color manage never frees colors: */
  FreePixel(TRUE);
}

//--- assignment -------------------------------------------------------------

wxColour* wxColour::CopyFrom(wxColour *col)
{
  if (col->Ok()) {
    FreePixel(FALSE);
    if (!X) {
      X  = new wxColour_Xintern; // create new X representation;
    }
    *X = *(col->X);	       // assign data
    X->have_pixel = FALSE;
  } else
    FreePixel(TRUE);

  return this;
}

wxColour* wxColour::CopyFrom(const char *col)
{
  wxColour *the_colour;

  the_colour = wxTheColourDatabase->FindColour(col); // find colour by name
  
  if (the_colour) {
    FreePixel(FALSE);
    if (!X) {
       X = new wxColour_Xintern; // create new X representation
     }
    *X = *(the_colour->X);	   // assign data
    X->have_pixel = FALSE;
  } else
    FreePixel(TRUE); // free pixel before assignment

  return this;
}

//--- get and set RGB values --------------------------------------------------

void wxColour::Set(unsigned char r, unsigned char g, unsigned char b)
{
    FreePixel(FALSE);

    if (!X) {
       X = new wxColour_Xintern; // create new X representation
     }

    X->xcolor.red   = ((unsigned short)r) << SHIFT; // init XColor structure
    X->xcolor.green = ((unsigned short)g) << SHIFT;
    X->xcolor.blue  = ((unsigned short)b) << SHIFT;
    X->xcolor.flags = DoRed | DoGreen | DoBlue;
    X->have_pixel   = FALSE; // no pixel value assigned
}

void wxColour::Get(unsigned char *r, unsigned char *g, unsigned char *b)
{
  if (X) {
    *r = (unsigned char)(X->xcolor.red   >> SHIFT);
    *g = (unsigned char)(X->xcolor.green >> SHIFT);
    *b = (unsigned char)(X->xcolor.blue  >> SHIFT);
  } else {
    *r = *g = *b = 0;
  }
}

unsigned char wxColour::Red(void)
{
  return ( X ? (unsigned char)(X->xcolor.red >> SHIFT) : 0 );
}

unsigned char wxColour::Green(void)
{
  return ( X ? (unsigned char)(X->xcolor.green >> SHIFT) : 0 );
}

unsigned char wxColour::Blue(void)
{
  return ( X ? (unsigned char)(X->xcolor.blue >> SHIFT) : 0 );
}

//--- allocate and free X pixel values ----------------------------------------

static int alloc_close_color(Display *display, Colormap cmap, XColor *xc)
{
  XColor ctab[256];
  int ncells, j;
  int d, mdist, close;
  

  ncells = DisplayCells(display, DefaultScreen(display));

  ncells = (ncells < 256) ? ncells : 256;
  
  for (j = 0; j < ncells; j++) {
    ctab[j].pixel = j;
  }

  XQueryColors(display, cmap, ctab, ncells);

  mdist = 0;   close = -1;
  
  for (j = 0; j < ncells; j++) {
    d = (abs((int)(xc->red - ctab[j].red)) +
	 abs((int)(xc->green - ctab[j].green)) +
	 abs((int)(xc->blue - ctab[j].blue)));
    if (!mdist || (d < mdist)) { 
      mdist = d; 
      close = j;
    }
  }

  if (wxAllocColor(display, cmap, &ctab[close])) { 
    static int approxmsg = 1;
    if (approxmsg) {
      wxError("Cannot allocate color, using approximate match.\n"
	      "(Future allocations may be approximate without report.)",
	      "MrEd Warning");
      
      approxmsg = 0;
    }

    xc->pixel = ctab[close].pixel;
    return 1;
  } else
    return 0;
}

unsigned long wxColour::GetPixel(wxColourMap *cmap, Bool is_color, Bool fg)
{
  if (!is_color) {
    int white;
    if (!X) {
      white = 1;
    } else if (fg) {
      /* foreground: white = white, all else = black */
      white = (((X->xcolor.red >> SHIFT) == 255)
	       && ((X->xcolor.green >> SHIFT) == 255)
	       && ((X->xcolor.blue >> SHIFT) == 255));
    } else {
      /* background: black = black, all else = white */
      white = (X->xcolor.red || X->xcolor.green || X->xcolor.blue);
    }

    if (white)
      return 0; /* WhitePixelOfScreen(wxAPP_SCREEN); */
    else
      return 1; /* BlackPixelOfScreen(wxAPP_SCREEN); */
  }

    if (X) {
	if (!X->have_pixel) {
	  XColor xcol;
	  Colormap cm;

	    // no pixel value or wrong colourmap
	    FreePixel(FALSE); // free pixel value if any
	    cm = GETCOLORMAP(cmap); // colourmap to use
	    X->xcolormap = cm;

	    // allocate pixel
	    /* Copy color b/c XAllocColour sets RGB values */
	    xcol.red = X->xcolor.red;
	    xcol.green = X->xcolor.green;
	    xcol.blue = X->xcolor.blue;
	    xcol.flags = DoRed | DoBlue | DoGreen;

	    if (!wxAllocColor(wxAPP_DISPLAY, X->xcolormap, &xcol)
		&& !alloc_close_color(wxAPP_DISPLAY, X->xcolormap, &xcol)) {
	      // failed => used default
	      static int message_printed = FALSE;
	      if (!message_printed) {
		wxError("Colour allocation failed, using black.\n(Future allocations may fail without reports.)", 
			"wxColour");
		message_printed = TRUE;
	      } 
	      return BlackPixelOfScreen(wxAPP_SCREEN);
	    } else {
	      X->xcolor.pixel = xcol.pixel;
	      X->have_pixel = TRUE; // allocation successful
	    }
	}
    } else {
	// use something as a default value
	wxDebugMsg("wxColour: no colour specified, using white\n");
	return(WhitePixelOfScreen(wxAPP_SCREEN));
    }
    return (X->xcolor.pixel);
}

void wxColour::FreePixel(Bool del)
{
    if (X) {
	if (X->have_pixel) {
	    // free allocated colour
	    // -- currently don't free colours, because the ownership of
	    // -- the pixel-value is not specified!
	    // XFreeColors(wxAPP_DISPLAY, X->xcolormap, &(X->xcolor.pixel), 1, 0);
	    X->have_pixel = FALSE;
	}
	if (del) {
	    DELETE_OBJ X; // destroy X representation;
	    X = NULL; // not Ok
	}
    }
}

//-----------------------------------------------------------------------------
// wxColourDatabase
//-----------------------------------------------------------------------------

wxColourDatabase::wxColourDatabase()
: wxList(wxKEY_STRING)
{
  
}

wxColourDatabase::~wxColourDatabase (void)
{
  wxNode *node;
  node = First();
  while (node) {
    wxColour *col;
    wxNode *next;
    col  = (wxColour*)node->Data();
    next = node->Next();
    DELETE_OBJ col;
    node = next;
  }
}

wxColour *wxColourDatabase::FindColour(const char *colour)
{
  wxNode *node;
  wxColour *col;
#if 0
  XColor xcolor;
  Colormap cm;
#endif

  // Force capital so lc matches as in X
  char uc_colour[256];
  int i;

  for (i = 0; colour[i] && i < 255; i++) {
    uc_colour[i] = toupper(colour[i]);
  }
  uc_colour[i] = 0;
  colour = uc_colour;

  if ((node = Find(colour)))
    return (wxColour*)node->Data(); // colour already defined

  /* Define the standard set: */
  static wxHashTable *aux = NULL;
  if (!aux) {
    wxColour *tmpc;
    wxREGGLOB(aux);
    aux = new wxHashTable(wxKEY_STRING, 20);
#define APPEND_C(name, c) tmpc = c; tmpc->Lock(1); aux->Put(name, tmpc);
#include "../../../wxcommon/DBColors.inc"
  }

#if 0
  // use wxAPP_DISPLAY and wxAPP_COLOURMAP as default
  cm = GETCOLORMAP(wxAPP_COLOURMAP);
  if (XParseColor(wxAPP_DISPLAY, cm, colour, &xcolor)) {
    // new colour found: add to list as found, but only if it's in the standard set
    col = (wxColour *)aux->Get(colour);
    if (col) {
      col = DEBUG_NEW wxColour((unsigned char)(xcolor.red >> SHIFT),
			       (unsigned char)(xcolor.green >> SHIFT),
			       (unsigned char)(xcolor.blue >> SHIFT));
      col->Lock(1);
    }
  } else 
#endif
    {
      col = (wxColour *)aux->Get(colour);
    }

  if (col)
    Append(colour, col);

  return col;
}

char *wxColourDatabase::FindName(wxColour *colour)
{
  if (colour->Ok()) {
    wxNode *node;
    unsigned char red, green, blue;

    red   = colour->Red();
    green = colour->Green();
    blue  = colour->Blue();
    
    for (node = First(); node; node = node->Next()) {
      wxColour *col;
      col = (wxColour*)node->Data ();
      if (col->Red()==red && col->Green()==green && col->Blue()==blue) {
	char *found = node->string_key;
	if (found)
	  return found;
      }
    }
  }
  return NULL;
}

//-----------------------------------------------------------------------------
// wxColourMap
//-----------------------------------------------------------------------------

wxColourMap::wxColourMap(Bool priv)
{
    __type = wxTYPE_COLOURMAP;

    X  = new wxColourMap_Xintern; // create new X representation
    X->xcolormap = wx_default_colormap;
    X->priv      = priv;
    // if (X->priv) {
    //	    X = NULL; // create colourmap;
    // } else
    //      X->xcolormap = DefaultColormapOfScreen(wxAPP_SCREEN);
}

wxColourMap::~wxColourMap(void)
{
    if (X) {
	if (X->priv) {
	    // free colourmap
	}
	DELETE_OBJ X;
    }
}

void *wxColourMap::GetHandle(void)
{
    if (X)
	return (&(X->xcolormap));
    return &(wxAPP_COLOURMAP->X->xcolormap); // just to return somthing
}

