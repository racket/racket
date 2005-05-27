/*
 * Global Data
 *
 */

#include "wx.h"

#define _MAXPATHLEN 500

// Useful buffer, initialized in wxCommonInit
char *wxBuffer = NULL;

// GDI Object Lists
wxBrushList *wxTheBrushList = NULL;
wxPenList   *wxThePenList = NULL;
wxFontList   *wxTheFontList = NULL;

wxColourDatabase *wxTheColourDatabase = NULL;

// Stock objects
wxFont *wxNORMAL_FONT;

wxPen *wxBLACK_PEN;

wxBrush *wxWHITE_BRUSH;
wxBrush *wxBLACK_BRUSH;

wxColour *wxBLACK;
wxColour *wxWHITE;

wxCursor *wxSTANDARD_CURSOR = NULL;
wxCursor *wxHOURGLASS_CURSOR = NULL;
wxCursor *wxCROSS_CURSOR = NULL;
wxCursor *wxIBEAM_CURSOR = NULL;

