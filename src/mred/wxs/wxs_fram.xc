
@INCLUDE prefix.xci

#include "wx_frame.h"
#include "wx_gdi.h"

@INCLUDE wxs.xci

@HEADER

#ifdef wx_xt
#define GET_THE_MENU_BAR(f) (f)->GetMenuBar()
#else
#define GET_THE_MENU_BAR(f) (f)->wx_menu_bar
#endif

#ifdef wx_motif
#define wxALLOW_AUTO_RESIZE wxPUSH_PIN
#else
#define wxALLOW_AUTO_RESIZE 0
#endif

@MACRO CHECKHASMENU[log] = if (<log>GET_THE_MENU_BAR(((wxFrame *)((Scheme_Class_Object *)THEOBJ)->primdata))) return scheme_void;

@INCLUDE wxs_espc.xci

static wxMenuBar *GetTheMenuBar(wxFrame *f)
{
  return GET_THE_MENU_BAR(f);
}

#ifndef wxTOOLBAR_BUTTON
# define wxTOOLBAR_BUTTON 0
#endif
#ifndef wxHIDE_MENUBAR
# define wxHIDE_MENUBAR 0
#endif
#ifndef wxMETAL
# define wxMETAL 0
#endif

@BEGINSYMBOLS frameStyle > > PRED BUNDLE
@SYM "no-caption" : wxNO_CAPTION
@SYM "mdi-parent" : wxMDI_PARENT
@SYM "mdi-child" : wxMDI_CHILD
@SYM "no-system-menu" : wxNO_SYSTEM_MENU
@SYM "no-resize-border" : wxNO_RESIZE_BORDER
@SYM "toolbar-button" : wxTOOLBAR_BUTTON
@SYM "hide-menu-bar" : wxHIDE_MENUBAR
@SYM "metal" : wxMETAL
@SYM "float" : wxFLOAT_FRAME
@ENDSYMBOLS

@BEGINSYMBOLS iconKind > ONE > PRED BUNDLE
@SYM "both" : 0
@SYM "small" : 1
@SYM "large" : 2
@ENDSYMBOLS

#ifdef wx_msw
# define XTMAC_UNUSED(x) x
#else
# define XTMAC_UNUSED(x) /**/
#endif

static void frameMenu(wxFrame *XTMAC_UNUSED(f))
{
#ifdef wx_msw
  f->SystemMenu();
#endif
}

static void DesignateRootFrame(wxFrame *f)
{
#ifdef wx_mac
  f->DesignateRootFrame();
#endif
}

@CLASSBASE wxFrame "frame":"window"

@CLASSID wxTYPE_FRAME

@CREATOR (wxFrame^, string, int = -1, int = -1, int = -1, int = -1, SYM[frameStyle]=0, string = "frame") : : /CHECKEVENTSPACE[METHODNAME("frame%","initialization")]|NOZERO[4]|NOZERO[5]/

@MACRO CHECKICONOK[p] = if (x<p> && !x<p>->Ok()) scheme_arg_mismatch(METHODNAME("frame%","set-icon"), "bad bitmap: ", p[POFFSET+<p>]);
@MACRO CHECKICONBW[p] = if (x<p> && (x<p>->GetDepth() != 1)) scheme_arg_mismatch(METHODNAME("frame%","set-icon"), "mask bitmap is not monochrome: ", p[POFFSET+<p>]);

@ "set-title" : void SetTitle(string);
@ "iconize" : void Iconize(bool);
@ "set-icon" : void SetIcon(wxBitmap!,wxBitmap^ = NULL,SYM[iconKind] = 0); : : /CHECKICONOK[0]|CHECKICONOK[1]|CHECKICONBW[1]
@ "set-menu-bar" : void SetMenuBar(wxMenuBar!) : : /CHECKHASMENU[ ]
@ m "get-menu-bar" : wxMenuBar^ GetTheMenuBar()
@ "set-status-text" : void SetStatusText(string)
@ "iconized?" : bool Iconized();
@ "status-line-exists?" : bool StatusLineExists();
@ "maximize" : void Maximize(bool)
@ "create-status-line" : void CreateStatusLine(int = 1, string = "status_line")
@ "set-modified" : void SetFrameModified(bool)

@ m "system-menu" : void frameMenu();
@ m "designate-root-frame" : void DesignateRootFrame()

@SETMARK f = d
@INCLUDE wxs_fram.xci

@ v "on-menu-command" : void OnMenuCommand(ExactLong)
@ v "on-menu-click" : void OnMenuClick()
@ v "on-toolbar-click" : void OnToolbarButton()

@SETMARK w = d
@INCLUDE wxs_win.xci

@END
