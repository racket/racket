/*								-*- C++ -*-
 *
 * Purpose: wxWindows application and main loop
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

#ifdef __GNUG__
#pragma implementation "AppMain.h"
#pragma GCC diagnostic ignored "-Wwrite-strings"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxApp
#define  Uses_wxGDI
#define  Uses_wxPrintSetup
#define  Uses_wxTypeTree
#define  Uses_wxMenu
#define  Uses_wxWindowDC
#include "wx.h"
#include "widgets.h" // for X11/StringDefs.h
#include "wxAllocColor.h"

#ifdef WX_USE_XFT
# include <X11/Xft/Xft.h>
#endif

#ifndef NO_XKB_LIB_PRESENT
# include <X11/XKBlib.h>
#endif

extern void wxsRememberDisplay(char *str);

Visual *wxAPP_VISUAL;
int wx_visual_depth;
Colormap wx_default_colormap;
unsigned long wx_white_pixel, wx_black_pixel;

int wx_single_instance = 0;

//-----------------------------------------------------------------------------
// wxApp implementation
//-----------------------------------------------------------------------------

wxApp::wxApp()
{
  __type = wxTYPE_APP;
  
  // no application and/or application not initialized
  initialized = FALSE;
  wxREGGLOB(wxTheApp);
  wxTheApp = this;
}

void wxApp::Dispatch(void)
{
  wxDoNextEvent();
}

int wxApp::MainLoop(void)
{
    keep_going = TRUE;
    while (keep_going) {
      wxDoEvents();
    }
    return 0;
}

Bool wxApp::Pending(void)
{
    XFlush(wxAPP_DISPLAY);
    return (wxEventReady());
}

//-----------------------------------------------------------------------------
// The main function 
//-----------------------------------------------------------------------------

#include <X11/Shell.h>

static void wxCommonInit(void);

void wxInitNewToplevel(void)
{
  Widget tl;

  tl = XtVaAppCreateShell(wxAPP_NAME, wxAPP_CLASS,
			  applicationShellWidgetClass,
			  wxAPP_DISPLAY,
			  XtNvisual, wxAPP_VISUAL,
			  XtNdepth, wx_visual_depth,
			  XtNcolormap, wx_default_colormap,
			  NULL);

  wxPutAppToplevel(tl);
}

typedef struct {
  char *flag;
  int arg_count;
} X_flag_entry;

#define SINGLE_INSTANCE "-singleInstance"

X_flag_entry X_flags[] = {
  { "-display", 1 },
  { "-geometry", 1 },
  { "-bg", 1 },
  { "-background", 1 },
  { "-fg", 1 },
  { "-foreground", 1 },
  { "-fn", 1 },
  { "-font", 1 },
  { "-iconic", 0 },
  { "-name", 1 },
  { "-rv", 0 },
  { "-reverse", 0 },
  { "+rv", 0 },
  { "-selectionTimeout", 1 },
  { "-synchronous", 0 },
  { "-title", 1 },
  { "-xnllanguage", 1 },
  { "-xrm", 1 },
  { SINGLE_INSTANCE, 0},
  { NULL, 0 }
};

static int filter_x_readable(char **argv, int argc, char **x_display_str)
{
  int pos = 1, i;

  while (pos < argc) {
    for (i = 0; X_flags[i].flag; i++) {
      if (!strcmp(X_flags[i].flag, argv[pos]))
	break;
    }

    if (!X_flags[i].flag)
      return pos;
    else {
      int newpos = pos + X_flags[i].arg_count + 1;
      if (newpos > argc) {
	printf("%s: X Window System flag \"%s\" expects %d arguments, %d provided\n",
	       argv[0], argv[pos], X_flags[i].arg_count, argc - pos - 1);
	exit(-1);
      }
      if (!strcmp(argv[pos], "-display")) {
	*x_display_str = argv[pos + 1];
      }
      pos = newpos;
    }
  }

  return pos;
}

int wxEntry(int argc, char *argv[])
{
  int xargc, ate;
  char *x_display_str = NULL;

  if (!wxTheApp) {
    wxFatalError("You have to define an instance of wxApp!");
  }

  // init private and public data
  /* Set if not set... */
  if (!wxAPP_CLASS) {
    wxREGGLOB(wxAPP_CLASS);
    wxAPP_CLASS = wxFileNameFromPath(argv[0]);
  }
  if (!wxAPP_NAME) {
    wxREGGLOB(wxAPP_NAME);
    wxAPP_NAME  = wxFileNameFromPath(argv[0]);
  }

  xargc = filter_x_readable(argv, argc, &x_display_str);
  ate = xargc - 1;

  if (!x_display_str)
    x_display_str = getenv("DISPLAY");
  
  /* Remember -display or DISPLAY, in case someone needs it: */
  wxsRememberDisplay(x_display_str);

  XtToolkitInitialize();
  wxAPP_CONTEXT = XtCreateApplicationContext();
  wxAPP_DISPLAY = XtOpenDisplay(wxAPP_CONTEXT, NULL, NULL,NULL,
				NULL, 0,
				&xargc, argv); // command line arguments

  if (!wxAPP_DISPLAY) {
    if (!x_display_str) {
      printf("DISPLAY environment variable not set and no -display argument\n");
    } else {
      printf("Cannot open display: %s\n", x_display_str);
    }
    exit(1);
  }


  if ((xargc > 1) && !strcmp("-singleInstance", argv[1])) {
    wx_single_instance = 1;
    --xargc;
    if (xargc > 1) {
      argv[1] = argv[2];
    }
  }

  if (xargc != 1) {
    printf("%s: standard X Window System flag \"%s\" was rejected\n",
	   argv[0], argv[1]);
    exit(1);
  }
  
  wxAPP_SCREEN = DefaultScreenOfDisplay(wxAPP_DISPLAY);
  wxAPP_VISUAL = DefaultVisualOfScreen(wxAPP_SCREEN);
  wx_default_colormap = DefaultColormapOfScreen(wxAPP_SCREEN);
  wx_black_pixel = BlackPixel(wxAPP_DISPLAY, DefaultScreen(wxAPP_DISPLAY));
  wx_white_pixel = WhitePixel(wxAPP_DISPLAY, DefaultScreen(wxAPP_DISPLAY));

  /* Use 24-bit TrueColor visual, if possible */
  {
    XVisualInfo *vi, vi_tmpl, vi2;
    int n;
    
    vi_tmpl.visualid = XVisualIDFromVisual(wxAPP_VISUAL);
    vi = XGetVisualInfo(wxAPP_DISPLAY, VisualIDMask, &vi_tmpl, &n);
    wx_visual_depth = vi->depth;

    if ((vi->c_class != TrueColor) || (vi->depth < 24)) {
      if (XMatchVisualInfo(wxAPP_DISPLAY, DefaultScreen(wxAPP_DISPLAY), 
			   24, TrueColor, &vi2)) {
	wxAPP_VISUAL = vi2.visual;
	wx_visual_depth = 24;
	wx_default_colormap = XCreateColormap(wxAPP_DISPLAY, 
					      RootWindow(wxAPP_DISPLAY,
							 DefaultScreen(wxAPP_DISPLAY)),
					      wxAPP_VISUAL, 
					      AllocNone);

	{
	  XColor xcol;
	  xcol.red = xcol.blue = xcol.green = 0;
	  wxAllocColor(wxAPP_DISPLAY, wx_default_colormap, &xcol);
	  wx_black_pixel = xcol.pixel;
	  xcol.red = xcol.blue = xcol.green = 0xFFFF;
	  wxAllocColor(wxAPP_DISPLAY, wx_default_colormap, &xcol);
	  wx_white_pixel = xcol.pixel;
	}
      }
    }

    XFree(vi);
  }

  wxInitNewToplevel();

  for (int i = ate + 1; i < argc; i++) {
    argv[i - ate] = argv[i];
  }
  argc -= ate;
  
  wxTheApp->argc = argc;
  wxTheApp->argv = argv;

  // initialize global data
  wxCommonInit();
  
  wxTheApp->OnInit();
  
  return 0;
}

//-----------------------------------------------------------------------------
// initialize and destroy global data
//-----------------------------------------------------------------------------

extern int wxGetPreference(const char *name, int *len);
extern int wxGetPreference(const char *name, char *res, long len);

void wxCommonInit(void)
{
    Bool supported;
    int fsize;
  
    wxAPP_ROOT	    = RootWindow(wxAPP_DISPLAY, DefaultScreen(wxAPP_DISPLAY));

#ifdef WX_USE_XFT
    if (wxXRenderHere())
      XftInit(NULL);
#endif
  
    wxREGGLOB(wxAPP_COLOURMAP);
    wxAPP_COLOURMAP = DEBUG_NEW wxColourMap(FALSE); // default colourmap

    wxREGGLOB(wxBuffer);
    wxBuffer = new WXGC_ATOMIC char[BUFSIZ+512];

    wxREGGLOB(wxResourceCache);
    wxResourceCache = new wxList(wxKEY_STRING);

#ifndef NO_XKB_LIB_PRESENT
    XkbSetDetectableAutoRepeat(wxAPP_DISPLAY, 1, &supported);
#endif

    wxInitPopupMgr();

    wxREGGLOB(wxAllTypes);
    wxAllTypes = new wxTypeTree;

    wxInitializeFontNameDirectory();

    wxInitializePrintSetupData();
    wxREGGLOB(wxThePrintPaperDatabase);
    wxThePrintPaperDatabase = DEBUG_NEW wxPrintPaperDatabase;
    wxThePrintPaperDatabase->CreateDatabase();

    wxREGGLOB(wxTheColourDatabase);
    wxTheColourDatabase = DEBUG_NEW wxColourDatabase();
    wxREGGLOB(wxThePenList);
    wxThePenList = DEBUG_NEW wxPenList();
    wxREGGLOB(wxTheBrushList);
    wxTheBrushList = DEBUG_NEW wxBrushList();
    wxREGGLOB(wxTheFontList);
    wxTheFontList = DEBUG_NEW wxFontList();

#ifdef WX_USE_XFT
    if (wxXRenderHere())
      fsize = 10;
    else
      fsize = 12;
#else
    fsize = 12;
#endif
    wxGetPreference("controlFontSize", &fsize);

    wxREGGLOB(wxNORMAL_FONT);
    wxNORMAL_FONT = DEBUG_NEW wxFont (fsize, wxMODERN, wxNORMAL, wxNORMAL);
    wxREGGLOB(wxSMALL_FONT);
    wxSMALL_FONT = DEBUG_NEW wxFont (10, wxSWISS, wxNORMAL, wxNORMAL);
    wxREGGLOB(wxITALIC_FONT);
    wxITALIC_FONT = DEBUG_NEW wxFont (fsize, wxROMAN, wxITALIC, wxNORMAL);
    wxREGGLOB(wxSWISS_FONT);
    wxSWISS_FONT = DEBUG_NEW wxFont (fsize, wxSWISS, wxNORMAL, wxNORMAL);
    wxREGGLOB(wxSYSTEM_FONT);
    wxSYSTEM_FONT = DEBUG_NEW wxFont (fsize, wxSYSTEM, wxNORMAL, wxNORMAL);

    wxREGGLOB(wxRED_PEN);
    wxRED_PEN = DEBUG_NEW wxPen ("RED", 0, wxSOLID);
    wxREGGLOB(wxCYAN_PEN);
    wxCYAN_PEN = DEBUG_NEW wxPen ("CYAN", 0, wxSOLID);
    wxREGGLOB(wxGREEN_PEN);
    wxGREEN_PEN = DEBUG_NEW wxPen ("GREEN", 0, wxSOLID);
    wxREGGLOB(wxBLACK_PEN);
    wxBLACK_PEN = DEBUG_NEW wxPen ("BLACK", 0, wxSOLID);
    wxREGGLOB(wxWHITE_PEN);
    wxWHITE_PEN = DEBUG_NEW wxPen ("WHITE", 0, wxSOLID);
    wxREGGLOB(wxTRANSPARENT_PEN);
    wxTRANSPARENT_PEN = DEBUG_NEW wxPen ("BLACK", 0, wxTRANSPARENT);
    wxREGGLOB(wxBLACK_DASHED_PEN);
    wxBLACK_DASHED_PEN = DEBUG_NEW wxPen ("BLACK", 0, wxSHORT_DASH);
    wxREGGLOB(wxGREY_PEN);
    wxGREY_PEN = DEBUG_NEW wxPen ("GRAY", 0, wxSOLID);
    wxREGGLOB(wxMEDIUM_GREY_PEN);
    wxMEDIUM_GREY_PEN = DEBUG_NEW wxPen ("MEDIUM GRAY", 0, wxSOLID);
    wxREGGLOB(wxLIGHT_GREY_PEN);
    wxLIGHT_GREY_PEN = DEBUG_NEW wxPen ("LIGHT GRAY", 0, wxSOLID);

    wxBLACK_PEN->Lock(1);
    wxWHITE_PEN->Lock(1);

    wxREGGLOB(wxBLUE_BRUSH);
    wxBLUE_BRUSH = DEBUG_NEW wxBrush ("BLUE", wxSOLID);
    wxREGGLOB(wxGREEN_BRUSH);
    wxGREEN_BRUSH = DEBUG_NEW wxBrush ("GREEN", wxSOLID);
    wxREGGLOB(wxWHITE_BRUSH);
    wxWHITE_BRUSH = DEBUG_NEW wxBrush ("WHITE", wxSOLID);
    wxREGGLOB(wxBLACK_BRUSH);
    wxBLACK_BRUSH = DEBUG_NEW wxBrush ("BLACK", wxSOLID);
    wxREGGLOB(wxTRANSPARENT_BRUSH);
    wxTRANSPARENT_BRUSH = DEBUG_NEW wxBrush ("BLACK", wxTRANSPARENT);
    wxREGGLOB(wxCYAN_BRUSH);
    wxCYAN_BRUSH = DEBUG_NEW wxBrush ("CYAN", wxSOLID);
    wxREGGLOB(wxRED_BRUSH);
    wxRED_BRUSH = DEBUG_NEW wxBrush ("RED", wxSOLID);
    wxREGGLOB(wxGREY_BRUSH);
    wxGREY_BRUSH = DEBUG_NEW wxBrush ("GRAY", wxSOLID);
    wxREGGLOB(wxMEDIUM_GREY_BRUSH);
    wxMEDIUM_GREY_BRUSH = DEBUG_NEW wxBrush ("MEDIUM GRAY", wxSOLID);
    wxREGGLOB(wxLIGHT_GREY_BRUSH);
    wxLIGHT_GREY_BRUSH = DEBUG_NEW wxBrush ("LIGHT GRAY", wxSOLID);

    wxBLACK_BRUSH->Lock(1);
    wxWHITE_BRUSH->Lock(1);

    wxREGGLOB(wxBLACK);
    wxBLACK = DEBUG_NEW wxColour ("BLACK");
    wxREGGLOB(wxWHITE);
    wxWHITE = DEBUG_NEW wxColour ("WHITE");
    wxREGGLOB(wxGREY);
    // wxGREY = DEBUG_NEW wxColour (214, 214, 214); /* aka "gray 84" */
    wxGREY = DEBUG_NEW wxColour (220, 218, 213); /* GTK2 background */
    wxREGGLOB(wxBUTTON_COLOR);
    // wxBUTTON_COLOR = DEBUG_NEW wxColour (224, 224, 224);
    wxBUTTON_COLOR = wxGREY;
    wxREGGLOB(wxRED);
    wxRED = DEBUG_NEW wxColour ("RED");
    wxREGGLOB(wxBLUE);
    wxBLUE = DEBUG_NEW wxColour ("BLUE");
    wxREGGLOB(wxGREEN);
    wxGREEN = DEBUG_NEW wxColour ("GREEN");
    wxREGGLOB(wxCYAN);
    wxCYAN = DEBUG_NEW wxColour ("CYAN");
    wxREGGLOB(wxLIGHT_GREY);
    wxLIGHT_GREY = DEBUG_NEW wxColour ("LIGHT GRAY");

    wxWHITE_PIXEL = wxWHITE->GetPixel(wxAPP_COLOURMAP);
    wxBLACK_PIXEL = wxBLACK->GetPixel(wxAPP_COLOURMAP);;
    wxGREY_PIXEL = wxGREY->GetPixel(wxAPP_COLOURMAP);;
    wxBUTTON_PIXEL = wxBUTTON_COLOR->GetPixel(wxAPP_COLOURMAP);;
    {
      wxColour *c;
      int r, g, b;
      r = wxGREY->Red();
      g = wxGREY->Green();
      b = wxGREY->Blue();
      c = new wxColour((int)(r * 0.85),
		       (int)(g * 0.85),
		       (int)(b * 0.85));
      wxDARK_GREY_PIXEL = c->GetPixel(wxAPP_COLOURMAP);
    }
    {
      int r, g, b, i, c;

      r = 75;
      g = 105;
      b = 131;
      if (wxGetPreference("hiliteColor", wxBuffer, 50)) {
	wxBuffer[50]= 0;
	if (strlen(wxBuffer) == 6) {
	  for (i = 0; wxBuffer[i]; i++) {
	    c = wxBuffer[i];
	    if ((c >= '0') && (c <= '9'))
	      wxBuffer[i] = c - '0';
	    else if ((c >= 'a') && (c <= 'f'))
	      wxBuffer[i] = c - 'a' + 10;
	    else if ((c >= 'A') && (c <= 'F'))
	      wxBuffer[i] = c - 'A' + 10;
	    else
	      break;
	  }
	  if (i == 6) {
	    r = (wxBuffer[0] << 4) + wxBuffer[1];
	    g = (wxBuffer[2] << 4) + wxBuffer[3];
	    b = (wxBuffer[4] << 4) + wxBuffer[5];
	  }
	}
      }
      wxREGGLOB(wxCTL_HILITE);
      wxCTL_HILITE = new wxColour(r, g, b);
      wxCTL_HIGHLIGHT_PIXEL = wxCTL_HILITE->GetPixel(wxAPP_COLOURMAP);
    }

    wxREGGLOB(wxSTANDARD_CURSOR);
    wxSTANDARD_CURSOR = DEBUG_NEW wxCursor (wxCURSOR_ARROW);
    wxREGGLOB(wxHOURGLASS_CURSOR);
    wxHOURGLASS_CURSOR = DEBUG_NEW wxCursor (wxCURSOR_WAIT);
    wxREGGLOB(wxCROSS_CURSOR);
    wxCROSS_CURSOR = DEBUG_NEW wxCursor (wxCURSOR_CROSS);
    wxREGGLOB(wxIBEAM_CURSOR);
    wxIBEAM_CURSOR = DEBUG_NEW wxCursor (wxCURSOR_IBEAM);
    wxREGGLOB(wxBLANK_CURSOR);
    wxBLANK_CURSOR = DEBUG_NEW wxCursor (wxCURSOR_BLANK);
}

static int hilite_border = -1;

extern "C" int wxUseMenuHiliteBorder()
{
  if (hilite_border < 0) {
    int on;
    hilite_border = 0;
    if (wxGetBoolPreference("hiliteMenuBorder", &on)) {
      if (on)
	hilite_border = 1;
    }
  }
  return hilite_border;
}
