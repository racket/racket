/*
 * Global Data
 *
 */

#ifdef __GNUG__
#ifdef __GNUG__
#pragma implementation
#endif
#endif

#include "common.h"
#include "wx_setup.h"
#include "wx_list.h"
#include "wx_gdi.h"

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
wxFont *wxSMALL_FONT;
wxFont *wxITALIC_FONT;
wxFont *wxSWISS_FONT;
wxPen *wxRED_PEN;

wxPen *wxCYAN_PEN;
wxPen *wxGREEN_PEN;
wxPen *wxBLACK_PEN;
wxPen *wxWHITE_PEN;
wxPen *wxTRANSPARENT_PEN;
wxPen *wxBLACK_DASHED_PEN;
wxPen *wxGREY_PEN;
wxPen *wxMEDIUM_GREY_PEN;
wxPen *wxLIGHT_GREY_PEN;

wxBrush *wxBLUE_BRUSH;
wxBrush *wxGREEN_BRUSH;
wxBrush *wxWHITE_BRUSH;
wxBrush *wxBLACK_BRUSH;
wxBrush *wxTRANSPARENT_BRUSH;
wxBrush *wxCYAN_BRUSH;
wxBrush *wxRED_BRUSH;
wxBrush *wxGREY_BRUSH;
wxBrush *wxMEDIUM_GREY_BRUSH;
wxBrush *wxLIGHT_GREY_BRUSH;

wxBrush *wxCONTROL_BACKGROUND_BRUSH;

wxColour *wxBLACK;
wxColour *wxWHITE;
wxColour *wxRED;
wxColour *wxBLUE;
wxColour *wxGREEN;
wxColour *wxCYAN;
wxColour *wxLIGHT_GREY;

wxCursor *wxSTANDARD_CURSOR = NULL;
wxCursor *wxHOURGLASS_CURSOR = NULL;
wxCursor *wxCROSS_CURSOR = NULL;
wxCursor *wxIBEAM_CURSOR = NULL;
