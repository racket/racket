
@INCLUDE prefix.xci

#include "wx_media.h"

@INCLUDE wxs.xci

@HEADER

static void *wxbBufferToDC(wxMediaBuffer *b, double x, double y)
{
  Scheme_Object *a[2];
  void *r;
  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH_ARRAY(0, a, 2);

  a[0] = a[1] = NULL;

  WITH_VAR_STACK(b->LocalToGlobal(&x, &y));

  a[0] = WITH_VAR_STACK(objscheme_bundle_double(x));
  a[1] = WITH_VAR_STACK(objscheme_bundle_double(y));

  r = WITH_VAR_STACK(scheme_values(2, a));
  READY_TO_RETURN;
  return r;
}

static void *wxbDCToBuffer(wxMediaBuffer *b, double x, double y)
{
  Scheme_Object *a[2];
  void *r;
  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH_ARRAY(0, a, 2);

  a[0] = a[1] = NULL;

  WITH_VAR_STACK(b->GlobalToLocal(&x, &y));

  a[0] = WITH_VAR_STACK(objscheme_bundle_double(x));
  a[1] = WITH_VAR_STACK(objscheme_bundle_double(y));

  r = WITH_VAR_STACK(scheme_values(2, a));
  READY_TO_RETURN;
  return r;
}

@MACRO rNULL = return NULL;
@MACRO rFALSE = return FALSE;
@MACRO rZERO = return 0;

@MACRO ubPort = (SCHEME_INPORTP({x}) ? {x} : (scheme_wrong_type(METHODNAME("editor<%>","insert-file"), "input port", -1, 1, &{x}), (Scheme_Object *)NULL))
@MACRO cPort = SCHEME_INPORTP({x})

@INCLUDE wxs_eds.xci

@INCLUDE wxs_eop.xci

@BEGINSYMBOLS printMethod > ONE > PRED BUNDLE
@SYM "standard" : 0
@SYM "postscript" : 1
@ENDSYMBOLS

static void NoLoadFile(wxMediaBuffer *)
{
}

static void NoInsertFile(wxMediaBuffer *)
{
}

@INCLUDE wxs_bmt.xci

@CLASSBASE wxMediaBuffer "editor" : "object" / nofnl
@INTERFACE "editor"

@CLASSID wxTYPE_MEDIA_BUFFER

@SETMARK X = p
@SETMARK Y = p
@SETMARK Z = p
@INCLUDE wxs_mbuf.xci

// W are Methods not intended to be overriden by the user,
// but acutally are implemented with virtual
@SETMARK W = D

@ W "save-file" : bool SaveFile(nxpathname=NULL,SYM[fileType]=wxMEDIA_FF_SAME,bool=TRUE);
@ W "insert-port" : SYM[fileType] InsertPort(Scheme_Object[]//ubPort/cPort///push,SYM[fileType]=wxMEDIA_FF_GUESS,bool=TRUE); <> port

// No longer actually in C, but we want them in the editor<%> interface:
@ m "load-file" : void NoLoadFile()
@ m "insert-file" : void NoInsertFile()

@ W "get-max-width" : nnfs[none] GetMaxWidth(); : : : : XrZERO
@ W "get-min-width" : nnfs[none] GetMinWidth(); : : : : XrZERO
@ W "set-max-width" : void SetMaxWidth(nnfs[none]);
@ W "set-min-width" : void SetMinWidth(nnfs[none]);
@ W "get-max-height" : nnfs[none] GetMaxHeight(); : : : : XrZERO
@ W "get-min-height" : nnfs[none] GetMinHeight(); : : : : XrZERO
@ W "set-max-height" : void SetMaxHeight(nnfs[none]);
@ W "set-min-height" : void SetMinHeight(nnfs[none]);

@ W "style-has-changed" : void StyleHasChanged(wxStyle^);

@ W "begin-edit-sequence" : void BeginEditSequence(bool=TRUE,bool=TRUE);
@ W "end-edit-sequence" : void EndEditSequence();
@ W "refresh-delayed?" : bool RefreshDelayed();
@ W "in-edit-sequence?" : bool InEditSequence();
@ W "locations-computed?" : bool LocationsUpToDate();

@ W "get-snip-location" : bool GetSnipLocation(wxSnip!,double?=NULL,double?=NULL,bool=FALSE); : : : : XrZERO

@ W "scroll-line-location" : double ScrollLineLocation(long); : : : : XrZERO
@ W "num-scroll-lines" : long NumScrollLines(); : : : : XrZERO
@ W "find-scroll-line" : long FindScrollLine(double); : : : : XrZERO

@ W "print-to-dc" : void PrintToDC(wxDC!); : : /CHECKDCOK[0.METHODNAME("editor<%>","print-to-dc")]

@ W "get-admin" : wxMediaAdmin^ GetAdmin(); : : : rNULL
@ W "set-admin" : void SetAdmin(wxMediaAdmin^);

@ W "locked-for-read?" : bool IsLockedForRead();
@ W "locked-for-write?" : bool IsLockedForWrite();
@ W "locked-for-flow?" : bool IsLockedForFlow();

@ "global-to-local" : void GlobalToLocal(double?,double?);
@ "local-to-global" : void LocalToGlobal(double?,double?);

@ "get-dc" : wxDC^ GetDC();
@ "get-view-size" : void GetViewSize(nndouble?,nndouble?);

@ "clear" : void Clear();
@ "select-all" : void SelectAll();

@ "undo" :  void Undo();
@ "redo": void Redo()
@ "clear-undos" : void ClearUndos();

@MACRO ubUndoer = ((void *){x})
@MACRO CHECKUNDOER = WITH_VAR_STACK(scheme_check_proc_arity(METHODNAME("editor<%>","add-undo"), 0, POFFSET, n, p));

@ "add-undo" : void AddSchemeUndo(UNKNOWN_OBJ//ubUndoer////push); : : /CHECKUNDOER

@ "set-max-undo-history" : void SetMaxUndoHistory(rint[0|100000]);
@ "get-max-undo-history" : int GetMaxUndoHistory();

@ "do-edit-operation" : void DoEdit(SYM[editOp],bool=TRUE,ExactLong=0);
@ "can-do-edit-operation?" : bool CanEdit(SYM[editOp],bool=TRUE);

@ "set-keymap" : void SetKeymap(wxKeymap^=NULL);
@ "get-keymap" : wxKeymap^ GetKeymap();

@ "get-style-list" : wxStyleList! GetStyleList();
@ "set-style-list" : void SetStyleList(wxStyleList!);

@ "set-load-overwrites-styles" : void SetLoadOverwritesStyles(bool)
@ "get-load-overwrites-styles" : bool GetLoadOverwritesStyles();

@ "set-paste-text-only" : void SetPasteTextOnly(bool)
@ "get-paste-text-only" : bool GetPasteTextOnly();

@ "set-cursor" : void SetCursor(wxCursor^,bool=TRUE); : : /CHECKVOIDABLEOK[0]

@ "lock" : void Lock(bool);
@ "is-locked?" : bool IsLocked();
@ "is-modified?" : bool Modified();

@ "get-filename" : npathname GetFilename(bool?=NULL);

@ "insert-box" : void InsertBox(SYM[bufferType]=wxEDIT_BUFFER);
@ "insert-image" : void InsertImage(nxpathname=NULL,SYM[bitmapType]=0,bool=FALSE,bool=TRUE);

@ "print" : void Print(bool=TRUE,bool=TRUE,SYM[printMethod]=0,wxWindow^=NULL,bool=TRUE,bool=FALSE);
/* : : /DLGORFRAME[3.METHODNAME("editor<%>","print")] */

@ "begin-write-header-footer-to-file" : bool BeginWriteHeaderFooterToFile(wxMediaStreamOut!,string,long*);
@ "end-write-header-footer-to-file" : bool EndWriteHeaderFooterToFile(wxMediaStreamOut!,long);

@ "get-focus-snip" : wxSnip^ GetFocusSnip();

@ "get-inactive-caret-threshold" : SYM[caret] GetInactiveCaretThreshold();
@ "set-inactive-caret-threshold" : void SetInactiveCaretThreshold(SYM[caret]);

@MACRO bundleAny = ((Scheme_Object *){x})
 
@ m "editor-location-to-dc-location" : void*/bundleAny wxbBufferToDC(double, double);
@ m "dc-location-to-editor-location" : void*/bundleAny wxbDCToBuffer(double, double);

@END

@GLOBAL wxMediaGlobal

@ "get-editor-print-margin" : void wxGetMediaPrintMargin(nnlong?,nnlong?);
@ "set-editor-print-margin" : void wxSetMediaPrintMargin(nnlong,nnlong);

@ "write-editor-version" : bool wxWriteMediaVersion(wxMediaStreamOut!, wxMediaStreamOutBase!);
@ "read-editor-version" : bool wxReadMediaVersion(wxMediaStreamIn!, wxMediaStreamInBase!, bool, bool=TRUE);

@ "read-editor-global-header" : bool wxReadMediaGlobalHeader(wxMediaStreamIn!);
@ "read-editor-global-footer" : bool wxReadMediaGlobalFooter(wxMediaStreamIn!);
@ "write-editor-global-header" : bool wxWriteMediaGlobalHeader(wxMediaStreamOut!);
@ "write-editor-global-footer" : bool wxWriteMediaGlobalFooter(wxMediaStreamOut!);

@ "add-editor-keymap-functions" : void wxAddMediaBufferFunctions(wxKeymap!);
@ "add-text-keymap-functions" : void wxAddMediaEditorFunctions(wxKeymap!);
@ "add-pasteboard-keymap-functions" : void wxAddMediaPasteboardFunctions(wxKeymap!);

@ "editor-set-x-selection-mode" : void wxMediaSetXSelectionMode(bool);

@ "get-the-snip-class-list" : wxSnipClassList! wxGetTheSnipClassList()
@ "get-the-editor-data-class-list" : wxBufferDataClassList! wxGetTheBufferDataClassList()

@END

/* Called from plt/src/mred/wxme/wx_cgrec.cxx */
int wxsSchemeUndo(void *f)
{
  Scheme_Object *v = scheme_apply((Scheme_Object *)f, 0, NULL);
  return SCHEME_TRUEP(v);
}
