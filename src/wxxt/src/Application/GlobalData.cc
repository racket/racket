/*								-*- C++ -*-
 *
 * Purpose: global data for an application (UNSHARED)
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
#pragma implementation "GlobalData.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxList
#define  Uses_wxTypeTree
#include "wx.h"

//-- Application data wxWindows -----------------------------------------------

class wxApp;
wxApp* wxTheApp = 0;

char*  wxAPP_CLASS = 0;
char*  wxAPP_NAME = 0;
Bool   wxAPP_DEBUGOUTPUT = FALSE;

wxColourMap* wxAPP_COLOURMAP = 0;

//-- Application data wxWindows (Xt) ------------------------------------------

XtAppContext wxAPP_CONTEXT;

//-- Application data wxWindows (XLib) ----------------------------------------

Display* wxAPP_DISPLAY = 0;
Screen*	 wxAPP_SCREEN = 0;
Window   wxAPP_ROOT = 0;

//-- GDI collections ----------------------------------------------------------

wxColourDatabase* wxTheColourDatabase = 0;
wxPenList*        wxThePenList = 0;
wxBrushList*	  wxTheBrushList = 0;
wxFontList*	  wxTheFontList = 0;

//-- misc ---------------------------------------------------------------------

char* wxBuffer = 0;

//-- Printing -----------------------------------------------------------------

wxPrintPaperDatabase* wxThePrintPaperDatabase = 0;
wxPrintSetupData*     wxThePrintSetupData = 0;

//-- Resources ----------------------------------------------------------------

XrmDatabase wxResourceDatabase;
wxList *wxResourceCache;

//-- simple language support---------------------------------------------------

char **wx_msg_str = NULL;

//-- stock objects ------------------------------------------------------------

wxBrush* wxBLUE_BRUSH = 0;
wxBrush* wxGREEN_BRUSH = 0;
wxBrush* wxWHITE_BRUSH = 0;
wxBrush* wxBLACK_BRUSH = 0;
wxBrush* wxGREY_BRUSH = 0;
wxBrush* wxMEDIUM_GREY_BRUSH = 0;
wxBrush* wxLIGHT_GREY_BRUSH = 0;
wxBrush* wxTRANSPARENT_BRUSH = 0;
wxBrush* wxCYAN_BRUSH = 0;
wxBrush* wxRED_BRUSH = 0;

wxColour* wxBLACK = 0;
wxColour* wxWHITE = 0;
wxColour* wxGREY = 0;
wxColour* wxBUTTON_COLOR = 0;
wxColour* wxRED = 0;
wxColour* wxBLUE = 0;
wxColour* wxGREEN = 0;
wxColour* wxCYAN = 0;
wxColour* wxLIGHT_GREY = 0;
wxColour* wxCTL_HILITE = 0;

wxCursor* wxSTANDARD_CURSOR = 0;
wxCursor* wxHOURGLASS_CURSOR = 0;
wxCursor* wxCROSS_CURSOR = 0;
wxCursor* wxIBEAM_CURSOR = 0;
wxCursor* wxBLANK_CURSOR = 0;

wxFont* wxNORMAL_FONT = 0;
wxFont* wxSMALL_FONT = 0;
wxFont* wxITALIC_FONT = 0;
wxFont* wxSWISS_FONT = 0;
wxFont* wxSYSTEM_FONT = 0;

wxPen* wxRED_PEN = 0;
wxPen* wxCYAN_PEN = 0;
wxPen* wxGREEN_PEN = 0;
wxPen* wxBLACK_PEN = 0;
wxPen* wxWHITE_PEN = 0;
wxPen* wxTRANSPARENT_PEN = 0;
wxPen* wxBLACK_DASHED_PEN = 0;
wxPen* wxGREY_PEN = 0;
wxPen* wxMEDIUM_GREY_PEN = 0;
wxPen* wxLIGHT_GREY_PEN = 0;

unsigned long wxWHITE_PIXEL;
unsigned long wxBLACK_PIXEL;
unsigned long wxGREY_PIXEL;
unsigned long wxBUTTON_PIXEL;
unsigned long wxDARK_GREY_PIXEL;
unsigned long wxCTL_HIGHLIGHT_PIXEL;

//-- Types --------------------------------------------------------------------

wxTypeTree *wxAllTypes;
