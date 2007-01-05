
@INCLUDE prefix.xci 

#include "wx_win.h"
#include "wx_gdi.h"
#include "wx_types.h"

@INCLUDE wxs.xci

@HEADER

#ifdef wx_mac
#define Move(x, y) SetSize(x, y, -1, -1, wxPOS_USE_MINUS_ONE)
#endif

static int wxSchemeWindowGetWidth(wxWindow *w)
{
  int x, y;

  w->GetSize(&x, &y);
  
  return x;
}

static int wxSchemeWindowGetHeight(wxWindow *w)
{
  int x, y;

  w->GetSize(&x, &y);
  
  return y;
}

static int wxSchemeWindowGetX(wxWindow *w)
{
  int x, y;

  w->GetPosition(&x, &y);
  
  return x;
}

static int wxSchemeWindowGetY(wxWindow *w)
{
  int x, y;

  w->GetPosition(&x, &y);
  
  return y;
}

static void wxSetPhantomSize(wxWindow *w, int wd, int ht)
{
#ifdef wx_mac
   w->SetPhantomSize(wd, ht);
#endif
}

static Bool wxIsEnabledToRoot(wxWindow *w)
{
   return !w->IsGray();
}

static Bool wxIsShownToRoot(wxWindow *w)
{
  while (1) {
    if (!w->IsShown())
      return 0;
    if (wxSubType(w->__type, wxTYPE_FRAME))
      return 1;
#ifdef wx_msw
    if (wxSubType(w->__type, wxTYPE_DIALOG_BOX))
      return 1;
#endif
    w = w->GetParent();
    if (!w)
      return 1;
  }
}

static long wxWindowGetHandle(wxWindow *w)
{
#ifdef wx_msw
  return (long)w->GetHWND();
#else
  return w->GetWindowHandle();
#endif
}

static void CenterWParent(wxWindow *w, int direction, wxWindow *parent)
{
#ifdef wx_mac
  w->Centre(direction, parent);
#else
  w->Centre(direction);
#endif
}

@BEGINSYMBOLS sizeMode > ONE > PRED BUNDLE
@SYM "auto" : wxSIZE_AUTO
@SYM "use-exsiting" : wxSIZE_USE_EXISTING
@SYM "use-minus-one" : wxPOS_USE_MINUS_ONE
@ENDSYMBOLS

@BEGINSYMBOLS direction > ONE > PRED BUNDLE
@SYM "both" : wxBOTH
@SYM "vertical" : wxVERTICAL
@SYM "horizontal" : wxHORIZONTAL
@ENDSYMBOLS

@CLASSBASE wxWindow "window":"object"

@MACRO CHECKCURSOROK[p] = if (x<p> && !x<p>->Ok()) x<p> = wxSTANDARD_CURSOR;

@ "centre" : void Centre(SYM[direction]=wxBOTH);

@ "gets-focus?" : bool GetsFocus();

@ "set-focus" : void SetFocus();
@ "set-size" : void SetSize(int,int,int,int,SYM[sizeMode]=wxSIZE_AUTO);
@ "move" : void Move(int,int); 
@ "set-cursor" : wxCursor^ SetCursor(wxCursor^); : : /CHECKCURSOROK[0]
@ "show" : void Show(bool);
@ "is-shown?" : bool IsShown();
@ "fit" : void Fit();
@ "get-size" : void GetSize(int*,int*);
@ "get-client-size" : void GetClientSize(int*,int*);
@ "get-position" : void GetPosition(int*,int*);
@ "enable" : void Enable(bool);

@ "drag-accept-files" : void DragAcceptFiles(bool);

// @ "get-char-height" : double GetCharHeight();
// @ "get-char-width" : double GetCharWidth();
@ "client-to-screen" : void ClientToScreen(int*, int*);
@ "screen-to-client" : void ScreenToClient(int*,int*);
@ "refresh" : void Refresh();
@ "get-parent" : wxWindow^ GetParent();
@ "get-text-extent" : void GetTextExtent(string,double*,double*,double?=NULL,double?=NULL,wxFont^=NULL,bool=FALSE);
@ m "center" : void CenterWParent(SYM[direction]=wxBOTH, wxWindow^=NULL);

@ "popup-menu" : void PopupMenu(wxMenu!, rint[0|10000], rint[0|10000]);

@ m "get-height" : int wxSchemeWindowGetHeight();
@ m "get-width" : int wxSchemeWindowGetWidth();
@ m "get-x" : int wxSchemeWindowGetX();
@ m "get-y" : int wxSchemeWindowGetY();

@ m "set-phantom-size" : void wxSetPhantomSize(int, int);

@ m "is-shown-to-root?" : bool wxIsShownToRoot();
@ m "is-enabled-to-root?" : bool wxIsEnabledToRoot();

@ m "get-handle" : ExactLong wxWindowGetHandle();

@SETMARK w = V
@INCLUDE wxs_win.xci

@END
