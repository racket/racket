
@INCLUDE prefix.xci

#include "wx_canvs.h"
#include "wx_panel.h"
#include "wx_frame.h"

@INCLUDE wxs.xci

@HEADER

#ifdef wx_xt
# include "wx_types.h"
# define CHECK_FOR_PANEL(x) !wxSubType((x)->__type, wxTYPE_CANVAS)
#else
# define CHECK_FOR_PANEL(x) 0
#endif

#ifndef wxCONTROL_BORDER
# define wxCONTROL_BORDER wxBORDER
#endif

static void FillZero(int *a, int *b) {
  *a = *b = 0;
}

static Bool wxSetBackgroundToGray(wxCanvas *c)
{
#ifdef wx_xt
  c->SetBackgroundToGray();
  return TRUE;
#endif
#ifdef wx_mac
  return c->SetAsControl();
#endif
#ifdef wx_msw
  return TRUE;
#endif
}

static void wxSetResizeCorner(wxCanvas *c, Bool v)
{
#ifdef wx_mac
  c->SetResizeCorner(v);
#endif
}

#ifndef wx_mac
# define wxRESIZE_CORNER 0
#endif

@BEGINSYMBOLS canvasStyle > > PRED BUNDLE
@SYM "border" : wxBORDER
@SYM "control-border" : wxCONTROL_BORDER
@SYM "combo" : wxCOMBO_SIDE
@SYM "vscroll" : wxVSCROLL
@SYM "hscroll" : wxHSCROLL
@SYM "gl" : 0
@SYM "no-autoclear" : wxNO_AUTOCLEAR
@SYM "deleted" : wxINVISIBLE
@SYM "transparent" : wxTRANSPARENT_WIN
@SYM "resize-corner" : wxRESIZE_CORNER
@SYM "no-focus" : wxNEVER_FOCUS
@ENDSYMBOLS

@INCLUDE wxs_ornt.xci

/* Handle cases in Xt that are a problem because a wxPanel isn't really a wxCanvas */
@MACRO PANELREDIRECT[x] = if (CHECK_FOR_PANEL((wxObject *)((Scheme_Class_Object *)THEOBJ)->primdata)) { <x>; }

@CLASSBASE wxCanvas "canvas":"window"

// @CREATOR (wxFrame!,int=-1,int=-1,int=-1,int=-1,SYM[canvasStyle]=0,string="canvas") : : /NOZERO[3]|NOZERO[4]/ <> frame
@CREATOR (wxPanel!,int=-1,int=-1,int=-1,int=-1,SYM[canvasStyle]=0,string="canvas",wxGLConfig^=NULL) : : /NOZERO[3]|NOZERO[4]/ <> panel

@SETMARK c = d
@INCLUDE wxs_cnvs.xci

@ "get-dc" : wxDC! GetDC();

// @ "get-scroll-units" : void GetScrollUnitsPerPage(int*,int*); : : / PANELREDIRECT[ FillZero(x0,x1); READY_TO_RETURN; return scheme_void]
@ "get-virtual-size" : void GetVirtualSize(int*,int*); : : / PANELREDIRECT[FillZero(x0,x1); READY_TO_RETURN; return scheme_void]
@ "set-scrollbars" : void SetScrollbars(rint[0|1000000000],rint[0|1000000000],rint[0|1000000000],rint[0|1000000000],rint[1|1000000000],rint[1|1000000000],rint[0|1000000000]=0,rint[0|1000000000]=0,bool=TRUE);  : : / PANELREDIRECT[READY_TO_RETURN; return scheme_void]
@ "show-scrollbars" : void EnableScrolling(bool,bool)
@ m "set-resize-corner" : void wxSetResizeCorner(bool)
@ "view-start" : void ViewStart(int*,int*); : : / PANELREDIRECT[FillZero(x0,x1); READY_TO_RETURN; return scheme_void]
@ "warp-pointer" : void WarpPointer(rint[0|10000],rint[0|10000]);  : : / PANELREDIRECT[READY_TO_RETURN; return scheme_void]

@ "scroll" : void ScrollPercent(double,double);
@ "get-scroll-pos" : int GetScrollPos(SYM[orientation]);
@ "get-scroll-range" : int GetScrollRange(SYM[orientation]);
@ "get-scroll-page" : int GetScrollPage(SYM[orientation]);

@ "set-scroll-pos" : void SetScrollPos(SYM[orientation], rint[0|1000000000]);
@ "set-scroll-range" : void SetScrollRange(SYM[orientation], rint[0|1000000000]);
@ "set-scroll-page" : void SetScrollPage(SYM[orientation], rint[1|1000000000]);

@ v "on-scroll" : void OnScroll(wxScrollEvent!); : JMPDECL/SETJMP/RESETJMP : / PANELREDIRECT[READY_TO_RETURN; return scheme_void]

@ m "set-background-to-gray" : bool wxSetBackgroundToGray()

@ "set-canvas-background" : void SetCanvasBackground(wxColour^);
@ "get-canvas-background" : wxColour^ GetCanvasBackground();

@SETMARK w = d
@INCLUDE wxs_win.xci

@END
