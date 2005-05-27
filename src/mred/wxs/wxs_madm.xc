
@INCLUDE prefix.xci

#include "wx_media.h"
#include "wx_frame.h"
#include "wx_panel.h"

@INCLUDE wxs.xci

@HEADER

#include "wxs_obj.h"
#include "wxs_evnt.h"

#ifndef wxCONTROL_BORDER
# define wxCONTROL_BORDER wxBORDER
#endif

@MACRO rNULL = return NULL;
@MACRO rFALSE = return FALSE;
@MACRO rZERO = return 0;

@MACRO bAnythingFromVoid = ((Scheme_Object *){x})
@MACRO ubAnythingToVoid = ((void *){x})
@MACRO cAnything = 1

extern Bool wxsCheckIsPopupMenu(void *m);
@MACRO CHECKMENU[n.p] = if (!wxsCheckIsPopupMenu(p[POFFSET+<p>])) scheme_wrong_type(<n>, "popup-menu% object", <p>+POFFSET, n, p);

#ifndef wx_mac
# define wxRESIZE_CORNER 0
#endif

@BEGINSYMBOLS style > > PRED BUNDLE
@SYM "no-hscroll" : wxMCANVAS_NO_H_SCROLL
@SYM "no-vscroll" : wxMCANVAS_NO_V_SCROLL
@SYM "hide-hscroll" : wxMCANVAS_HIDE_H_SCROLL
@SYM "hide-vscroll" : wxMCANVAS_HIDE_V_SCROLL
@SYM "auto-hscroll" : wxMCANVAS_AUTO_H_SCROLL
@SYM "auto-vscroll" : wxMCANVAS_AUTO_V_SCROLL
@SYM "deleted" : wxINVISIBLE
@SYM "control-border" : wxCONTROL_BORDER
@SYM "combo" : wxCOMBO_SIDE
@SYM "transparent" : wxTRANSPARENT_WIN
@SYM "border" : wxBORDER
@SYM "resize-corner" : wxRESIZE_CORNER
@ENDSYMBOLS

@INCLUDE wxs_fcs.xci

static void *DoCAPOCallback(void *data)
{
  return (void *)scheme_apply_multi((Scheme_Object *)data, 0, NULL);
}

typedef void *(*CAPOFunc)(void*);

@CLASSBASE wxMediaCanvas "editor-canvas" : "canvas"

// @CREATOR (wxFrame!,int=-1,int=-1,int=-1,int=-1, string="",SYM[style]=0,int=100,wxMediaBuffer^=NULL); : : /NOZERO[3]|NOZERO[4] <> frame
@CREATOR (wxPanel!,int=-1,int=-1,int=-1,int=-1, string="",SYM[style]=0,int=100,wxMediaBuffer^=NULL,wxGLConfig^=NULL); : : /NOZERO[3]|NOZERO[4] <> panel

@CLASSID wxTYPE_MEDIA_CANVAS

@IVAR "wheel-step" : nnint wheel_amt

@ "set-editor" : void SetMedia(wxMediaBuffer^,bool=TRUE);
@ "get-editor" : wxMediaBuffer^ GetMedia();

@ v "on-set-focus" : void OnSetFocus();
@ v "on-kill-focus" : void OnKillFocus();

@ v "on-scroll-on-change" : void OnScrollOnChange();

@ "is-focus-on?" : bool IsFocusOn();

@ "force-display-focus" : void ForceDisplayFocus(bool);

@ "allow-scroll-to-last" : void AllowScrollToLast(bool);
@ "scroll-with-bottom-base" : void ScrollWithBottomBase(bool);

@ "get-lazy-refresh" : bool GetLazyRefresh();
@ "set-lazy-refresh" : void SetLazyRefresh(bool);

@ "scroll-to" : bool ScrollTo(double,double,nndouble,nndouble, bool,SYM[bias]=0); : : : rFALSE

@ "get-x-margin" : nnint GetXMargin();
@ "get-y-margin" : nnint GetYMargin();
@ "set-x-margin" : void SetXMargin(nnint);
@ "set-y-margin" : void SetYMargin(nnint);

@ "set-canvas-background" : void SetCanvasBackground(wxColour^);
@ "get-canvas-background" : wxColour^ GetCanvasBackground();

@MACRO CastToSO = (Scheme_Object*){x}
@MACRO ubTestFunc = DoCAPOCallback
@MACRO ubData = p[POFFSET]
@MACRO spAnything = _
@MACRO spCAPOProc = (-> _)

@ "call-as-primary-owner" : void[]/CastToSO//spAnything CallAsPrimaryOwner(CAPOFunc//ubTestFunc///spCAPOProc/nopush, -void[]//ubData////push);

@ v "popup-for-editor" : wxMenu^ PopupForMedia(wxMediaBuffer^,void[]/bAnythingFromVoid/ubAnythingToVoid/cAnything///push); : : : rNULL

@SETMARK w = d
@INCLUDE wxs_win.xci

@SETMARK c = d
@INCLUDE wxs_cnvs.xci

@END


@CLASSBASE wxMediaAdmin "editor-admin":"object" / nofnl

@CREATOR ();

@CLASSID wxTYPE_MEDIA_ADMIN

@SETMARK A = V
@INCLUDE wxs_madm.xci

@END

@CLASSBASE wxMediaSnipMediaAdmin "editor-snip-editor-admin":"editor-admin" / nofnl
@INTERFACE "editor-snip-editor-admin"

@CLASSID wxTYPE_MEDIA_SNIP_MEDIA_ADMIN

@ "get-snip" : wxMediaSnip! GetSnip()

@END

@CLASSBASE wxSnipAdmin "snip-admin":"object" / nofnl

@CREATOR ();

@CLASSID wxTYPE_MEDIA_SNIP_ADMIN

@ V "get-editor" : wxMediaBuffer^ GetMedia(); : : : rNULL
@ V "get-dc" : wxDC^ GetDC(); : : : rNULL
@ V "get-view-size" : void GetViewSize(nndouble?, nndouble?);
@ V "get-view" : void GetView(double?, double?, nndouble?, nndouble?, wxSnip^=NULL);
@ V "scroll-to" : bool ScrollTo(wxSnip!, double,double,nndouble,nndouble, bool,SYM[bias]=0); : : : rFALSE
@ V "set-caret-owner" : void SetCaretOwner(wxSnip!,SYM[focus]);
@ V "resized" : void Resized(wxSnip!, bool);
@ V "recounted" : bool Recounted(wxSnip!, bool); : : : rFALSE
@ V "needs-update" : void NeedsUpdate(wxSnip!, double,double,nndouble,nndouble);
@ V "release-snip" : bool ReleaseSnip(wxSnip!); : : : rFALSE
@ V "update-cursor" : void UpdateCursor();
@ V "popup-menu" : bool PopupMenu(void[]/bAnythingFromVoid/ubAnythingToVoid/cAnything///push,wxSnip!,double,double); : : CHECKMENU[METHODNAME("snip-admin%","popup-menu").0] : rFALSE
@ V "modified" : void Modified(wxSnip!, bool);

@END


@CLASSBASE wxSnipClass "snip-class" : "object"

@CREATOR ();

@CLASSID wxTYPE_SNIP_CLASS

@IVAR "classname" : string classname
@IVAR "version" : int version

@ V "read" : wxSnip^ Read(wxMediaStreamIn!); : : : rNULL
@ v "read-header" : bool ReadHeader(wxMediaStreamIn!);
@ v "write-header" : bool WriteHeader(wxMediaStreamOut!);
@ "reading-version" : int ReadingVersion(wxMediaStreamIn!);

@END


@CLASSBASE wxSnipClassList "snip-class-list" : "object"
@INTERFACE "snip-class-list"

@CLASSID wxTYPE_SNIP_CLASS_LIST

@ "find" : wxSnipClass^ Find(string);
@ "find-position" : short FindPosition(wxSnipClass!);
@ "add" : void Add(wxSnipClass!);
@ "number" : int Number();
@ "nth" : wxSnipClass^ Nth(nnint);

@END


@CLASSBASE wxKeymap "keymap":"object" / nofnl

typedef Scheme_Object KeymapCallbackToSchemeRec;
#define kctsr(o) o

static Bool KMCallbackToScheme(UNKNOWN_OBJ, wxEvent *, KeymapCallbackToSchemeRec *data);
static Bool GrabKeyCallbackToScheme(char *s, wxKeymap *km, UNKNOWN_OBJ, wxKeyEvent *, KeymapCallbackToSchemeRec *data);
static Bool GrabMouseCallbackToScheme(char *s, wxKeymap *km, UNKNOWN_OBJ, wxMouseEvent *, KeymapCallbackToSchemeRec *data);
static void BreakSequenceCallbackToScheme(KeymapCallbackToSchemeRec *data);

@MACRO bCallback =
@MACRO ubSetup = KeymapCallbackToSchemeRec *cb;

@MACRO ubCallbackKM = (wxKMFunction)KMCallbackToScheme
@MACRO ubCallbackGrabKey = (wxGrabKeyFunction)GrabKeyCallbackToScheme
@MACRO ubCallbackGrabMouse = (wxGrabMouseFunction)GrabMouseCallbackToScheme
@MACRO ubCallbackBreak = (wxBreakSequenceFunction)BreakSequenceCallbackToScheme

@MACRO spCallbackKM = (wxObject-object wxEvent-object -> bool)
@MACRO spCallbackGrabKey = (str wxKeymap-object wxObject-object wxKeyEvent-object -> bool)
@MACRO spCallbackGrabMouse = (str wxKeymap-object wxObject-object wxMouseEvent-object -> bool)
@MACRO spCallbackBreak = (-> void)

@MACRO ubData = NULL

@MACRO ubSetData[n.m] = kctsr(cb) = p[POFFSET+<n>]; x<m> = (void *)cb;

@CREATOR ();

@CLASSID wxTYPE_KEYMAP

@ "get-double-click-interval" : int GetDoubleClickInterval();
@ "set-double-click-interval" : void SetDoubleClickInterval(rint[0|1000000]);

@ v "handle-key-event" : bool HandleKeyEvent(UNKNOWN_OBJ/bAnythingFromVoid/ubAnythingToVoid/cAnything///push,wxKeyEvent!);
@ v "handle-mouse-event" : bool HandleMouseEvent(UNKNOWN_OBJ/bAnythingFromVoid/ubAnythingToVoid/cAnything///push,wxMouseEvent!);
@ "break-sequence" : void BreakSequence();
@ "map-function" : void MapFunction(string,string);
@ "add-function" : void AddFunction(string,wxKMFunction/bCallback/ubCallbackKM/cCallback//spCallbackKM/nopush,-unknown#void*=NULL); : : ubSetup / ubSetData[1.2]
@ "set-grab-key-function" : void SetGrabKeyFunction(wxGrabKeyFunction/bCallback/ubCallbackGrabKey/cCallback//spCallbackGrabKey/nopush,-unknown#void*=NULL); : : ubSetup / ubSetData[0.1]
@ "remove-grab-key-function" : void RemoveGrabKeyFunction()
@ "set-grab-mouse-function" : void SetGrabMouseFunction(wxGrabMouseFunction/bCallback/ubCallbackGrabMouse/cCallback//spCallbackGrabMouse/nopush,-unknown#void*=NULL); : : ubSetup / ubSetData[0.1]
@ "remove-grab-mouse-function" : void RemoveGrabMouseFunction()
@ "call-function" : bool CallFunction(string,UNKNOWN_OBJ/bAnythingFromVoid/ubAnythingToVoid/cAnything///push,wxEvent!,bool=FALSE);
@ "set-break-sequence-callback" : void SetBreakSequenceCallback(wxBreakSequenceFunction/bCallback/ubCallbackBreak/cCallback//spCallbackBreak/nopush,-unknown#void*=NULL); : : ubSetup / ubSetData[0.1]
@ "chain-to-keymap" : void ChainToKeymap(wxKeymap!,bool);
@ "remove-chained-keymap" : void RemoveChainedKeymap(wxKeymap!);

@END

static Bool KMCallbackToScheme(UNKNOWN_OBJ media, wxEvent *event, 
			       KeymapCallbackToSchemeRec *data)
{
  Scheme_Object *p[2], *obj = NULL;
  Bool r;
  SETUP_VAR_STACK(5);
  VAR_STACK_PUSH(0, p[0]);
  VAR_STACK_PUSH(1, p[1]);
  VAR_STACK_PUSH(2, obj);
  VAR_STACK_PUSH(3, event);
  VAR_STACK_PUSH(4, data);

  p[0] = (Scheme_Object *)media;
  p[1] = NULL;
  p[1] = WITH_VAR_STACK(objscheme_bundle_wxEvent(event));

  obj = WITH_VAR_STACK(scheme_apply(kctsr(data), 2, p));
  r = WITH_VAR_STACK(objscheme_unbundle_bool(obj, "Scheme key callback"));
  READY_TO_RETURN;
  return r;
}

static Bool GrabKeyCallbackToScheme(char *s, wxKeymap *km,
				    UNKNOWN_OBJ media, wxKeyEvent *event, 
				    KeymapCallbackToSchemeRec *data)
{
  Scheme_Object *p[4], *obj = NULL;
  Bool r;
  SETUP_VAR_STACK(6);
  VAR_STACK_PUSH_ARRAY(0, p, 4);
  VAR_STACK_PUSH(3, obj);
  VAR_STACK_PUSH(4, event);
  VAR_STACK_PUSH(5, data);
  VAR_STACK_PUSH(6, km);

  p[0] = NULL;
  p[1] = NULL;
  p[3] = NULL;

  p[2] = (Scheme_Object *)media;

  p[0] = WITH_VAR_STACK(objscheme_bundle_string(s));
  p[1] = WITH_VAR_STACK(objscheme_bundle_wxKeymap(km));
  p[3] = WITH_VAR_STACK(objscheme_bundle_wxKeyEvent(event));

  obj = WITH_VAR_STACK(scheme_apply(kctsr(data), 4, p));
  r = WITH_VAR_STACK(objscheme_unbundle_bool(obj, "Scheme grab-key callback"));
  READY_TO_RETURN;
  return r;
}

static Bool GrabMouseCallbackToScheme(char *s, wxKeymap *km,
				      UNKNOWN_OBJ media, wxMouseEvent *event, 
				      KeymapCallbackToSchemeRec *data)
{
  Scheme_Object *p[4], *obj = NULL;
  Bool r;
  SETUP_VAR_STACK(6);
  VAR_STACK_PUSH_ARRAY(0, p, 4);
  VAR_STACK_PUSH(3, obj);
  VAR_STACK_PUSH(4, event);
  VAR_STACK_PUSH(5, data);
  VAR_STACK_PUSH(6, km);

  p[0] = NULL;
  p[1] = NULL;
  p[3] = NULL;

  p[2] = (Scheme_Object *)media;

  p[0] = WITH_VAR_STACK(objscheme_bundle_string(s));
  p[1] = WITH_VAR_STACK(objscheme_bundle_wxKeymap(km));
  p[3] = WITH_VAR_STACK(objscheme_bundle_wxMouseEvent(event));

  obj = WITH_VAR_STACK(scheme_apply(kctsr(data), 4, p));
  r = WITH_VAR_STACK(objscheme_unbundle_bool(obj, "Scheme grab-mouse callback"));
  READY_TO_RETURN;
  return r;
}

static void BreakSequenceCallbackToScheme(KeymapCallbackToSchemeRec *data)
{
  scheme_apply_multi(kctsr(data), 0, NULL);
}

@INCLUDE wxs_bkt.xci

@CLASSBASE wxMediaWordbreakMap "editor-wordbreak-map" : "object"

@CREATOR ()

@CLASSID wxTYPE_WORDBREAK_MAP

@ "set-map" : void SetMap(uchar,SYM[breakType]);
@ "get-map" : SYM[breakType] GetMap(uchar);

@END

static wxMediaWordbreakMap* wxGetTheMediaWordbreakMap()
{
  return wxTheMediaWordbreakMap;
}

@GLOBAL wxGlobalMediaWordbreakMap
@ "get-the-editor-wordbreak-map" : wxMediaWordbreakMap! wxGetTheMediaWordbreakMap()
@END

