
@INCLUDE prefix.xci

#include "wx_panel.h"
#include "wx_dialg.h"
#include "wx_types.h"

@INCLUDE wxs.xci

@HEADER

#if !defined(wx_mac)
#define INTERACT_METHODS 1
#else
#define INTERACT_METHODS 0
#endif

@BEGINSYMBOLS panelStyle > > PRED BUNDLE
@SYM "border" : wxBORDER
@SYM "deleted" : wxINVISIBLE
@ENDSYMBOLS

@INCLUDE wxs_ornt.xci

/* The derivation panel -> canvas is a lie for Xt */
@CLASSBASE wxPanel "panel":"window"

@CLASSID wxTYPE_PANEL

@CREATOR (wxFrame!,int=-1,int=-1,int=-1,int=-1,SYM[panelStyle]=0,string="panel") : : /NOZERO[3]|NOZERO[4] <> frame
@CREATOR (wxDialogBox!,int=-1,int=-1,int=-1,int=-1,SYM[panelStyle]=0,string="panel") : : /NOZERO[3]|NOZERO[4] <> dialog
@CREATOR (wxPanel!,int=-1,int=-1,int=-1,int=-1,SYM[panelStyle]=0,string="panel") : : /NOZERO[3]|NOZERO[4] <> panel parent

@ "get-item-cursor" : void GetCursor(int*,int*);
@ "set-item-cursor" : void SetItemCursor(int,int);

@SETMARK p = v
@INCLUDE wxs_panl.xci

@INCLUDE wxs_cnvs.xci

@ "set-label-position" : void SetLabelPosition(SYM[orientation]);
@ "get-label-position" : SYM[orientation] GetLabelPosition();

@INCLUDE wxs_ifnt.xci
// @INCLUDE wxs_icol.xci

@END

#ifdef wx_msw
# define XTMAC_UNUSED(x) x
#else
# define XTMAC_UNUSED(x) /**/
#endif

static void dialogMenu(wxDialogBox *XTMAC_UNUSED(d))
{
#ifdef wx_msw
  d->SystemMenu();
#endif
}

@BEGINSYMBOLS dialogStyle >  > PRED BUNDLE
@SYM "no-caption" : wxNO_CAPTION
@SYM "resize-border" : wxMAXIMIZE
@ENDSYMBOLS

@INCLUDE wxs_espc.xci

@CLASSBASE wxDialogBox "dialog" : "window"

@CLASSID wxTYPE_DIALOG_BOX

@INCLUDE wxs_dorf.xci

@CREATOR (wxWindow^,nstring,bool=FALSE,int=300,int=300,int=500,int=500,SYM[dialogStyle]=0,string="dialogBox"); : : /DLGORFRAME[0.METHODNAME("dialog%","initialization")]|CHECKEVENTSPACE[METHODNAME("dialog%","initialization")]|NOZERO[5]|NOZERO[6]

@SETMARK f = d
@INCLUDE wxs_fram.xci

@SETMARK p = d
@INCLUDE wxs_panl.xci

@ "set-title" : void SetTitle(string);

@ m "system-menu" : void dialogMenu();

@END
