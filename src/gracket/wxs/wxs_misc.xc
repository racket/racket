
@INCLUDE prefix.xci

#include "wx_obj.h"
#include "wx_win.h"
#include "wx_timer.h"
#include "wx_types.h"
#include "wx_stdev.h"
#include "wx_dc.h"
#include "wx_dcps.h"
#include "wx_clipb.h"

@INCLUDE wxs.xci

@HEADER

#if !defined(wx_mac)
#define NEWEST_TYPES 1
#else
#define NEWEST_TYPES 0
#endif

@MACRO rFALSE = return FALSE;

@INCLUDE wxs_espc.xci

@CLASSBASE wxTimer "timer" : "object"

@CREATOR (); : : /CHECKEVENTSPACE[METHODNAME("timer%","initialization")]
@ARGNAMES

@ "interval" : int Interval();
@ v "notify" : void Notify();
@ "start" : void Start(rint[0|1000000000],bool=FALSE); : : : : rFALSE
@ "stop" : void Stop();

@END


void AddType(wxClipboardClient *c, char *s) 
{ 
  c->formats->Add(s); 
}

Scheme_Object *GetTypes(wxClipboardClient *c)
{
  wxNode *n = NULL;
  Scheme_Object *first = scheme_null, *last = NULL;
  Scheme_Object *p = NULL, *s = NULL;
    
  SETUP_VAR_STACK(5);
  VAR_STACK_PUSH(0, n);
  VAR_STACK_PUSH(1, first);
  VAR_STACK_PUSH(2, last);
  VAR_STACK_PUSH(3, p);
  VAR_STACK_PUSH(4, s);

  n = WITH_VAR_STACK(c->formats->First());
  for (; n; n = WITH_VAR_STACK(n->Next())) {
    s = WITH_VAR_STACK(scheme_make_utf8_string((char *)n->Data()));
    p = WITH_VAR_STACK(scheme_make_pair(s, scheme_null));
    if (last)
      SCHEME_CDR(last) = p;
    else
      first = p;
    last = p;
  }
  
  READY_TO_RETURN;

  return first;
}

typedef Scheme_Object *Scheme_Object_Ptr;
static int SameClipboardClientEventspace(wxClipboardClient *c, Scheme_Object *es)
{
  return (c->context == es);
}

static int SameClipboardClient(wxClipboard *cb, wxClipboardClient *cc)
{
  return (cb->GetClipboardClient() == cc);
}


@MACRO makeSizedString[i] = (r ? scheme_make_sized_byte_string(r, _x<i>, 1) : XC_SCHEME_NULL)

@CLASSBASE wxClipboard "clipboard" : "object"
@INTERFACE "clipboard"

@ "set-clipboard-client" : void SetClipboardClient(wxClipboardClient!,ExactLong);
@ "set-clipboard-string" : void SetClipboardString(string,ExactLong); <> string
// @ "get-clipboard-client" : wxClipboardClient^ GetClipboardClient();
@ "get-clipboard-string" : nstring GetClipboardString(ExactLong);
@ "get-clipboard-data" : nbstring/makeSizedString[1] GetClipboardData(string,-long*,ExactLong);

@ "set-clipboard-bitmap" : void SetClipboardBitmap(wxBitmap!,ExactLong);
@ "get-clipboard-bitmap" : wxBitmap^ GetClipboardBitmap(ExactLong);

@ m "same-clipboard-client?" : bool SameClipboardClient(wxClipboardClient!)

@END

static wxClipboard* wxGetTheClipboard()
{
  return wxTheClipboard;
}

static wxClipboard* wxGetTheXSelection()
{
#ifdef wx_xt
  return wxTheSelection;
#else
  return wxTheClipboard;
#endif
}

@GLOBAL wxClipboardGlobal
@ "get-the-clipboard" : wxClipboard^ wxGetTheClipboard()
@ "get-the-x-selection" : wxClipboard^ wxGetTheXSelection()
@END

@MACRO setStringSize[cn] = if (SCHEME_BYTE_STRINGP(v)) (*x<cn>) = SCHEME_BYTE_STRTAG_VAL(v);
@MACRO identity = {x}
@MACRO XrNULL = return NULL;

@MACRO sbString = str

@CLASSBASE wxClipboardClient "clipboard-client" : "object"

@CREATOR ()
@ARGNAMES

@ V "on-replaced" : void BeingReplaced();
@ V "get-data" : nbstring GetData(string,-long*); : //setStringSize[1] : : : XrNULL

@ m "add-type" : void AddType(string);
@ m "get-types" : Scheme_Object*/identity//sbString GetTypes();

@ m "same-eventspace?" : bool SameClipboardClientEventspace(Scheme_Object_Ptr/identity/identity////push)

@END


@BEGINSYMBOLS psMode > ONE > PRED
@SYM "preview" : PS_PREVIEW
@SYM "file" : PS_FILE
@SYM "printer" : PS_PRINTER
@ENDSYMBOLS

@BEGINSYMBOLS psOrientation > ONE > PRED
@SYM "portrait" : PS_PORTRAIT
@SYM "landscape" : PS_LANDSCAPE
@ENDSYMBOLS

#ifdef wx_xt
void check_ps_mode(int, Scheme_Object *) {}
#else
void check_ps_mode(int v, Scheme_Object *p)
{
  if ((v == PS_PREVIEW) || (v == PS_PRINTER)) {
    scheme_arg_mismatch(METHODNAME("ps-setup%","set-mode"), 
	"only file mode is allowed for this platform, given: ",
	p);
  }
}
#endif

@MACRO checkPSMode[cn] = WITH_VAR_STACK(check_ps_mode(x<cn>, p[POFFSET+<cn>]));

@INCLUDE wxs_dorf.xci

@CLASSBASE wxPrintSetupData "ps-setup" : "object"

@CREATOR ()
@ARGNAMES

@ "get-command" : string GetPrinterCommand();
@ "get-file" : npathname GetPrinterFile();
@ "get-preview-command" : string GetPrintPreviewCommand();
@ "get-mode" : SYM[psMode] GetPrinterMode();
@ "get-orientation" : SYM[psOrientation] GetPrinterOrientation();
// @ "get-options" : string GetPrinterOptions();
@ "get-scaling" : void GetPrinterScaling(nndouble*,nndouble*);
@ "get-translation" : void GetPrinterTranslation(double*,double*);
@ "get-paper-name" : nstring GetPaperName();
@ "get-level-2" : bool GetLevel2();
@ "get-editor-margin" : void GetEditorMargin(nnlong*,nnlong*);
@ "get-margin" : void GetMargin(nndouble*,nndouble*);

@ "set-command" : void SetPrinterCommand(string);
@ "set-file" : void SetPrinterFile(npathname);
@ "set-preview-command" : void SetPrintPreviewCommand(string); 
@ "set-mode" : void SetPrinterMode(SYM[psMode]); : : /checkPSMode[0]
@ "set-orientation" : void SetPrinterOrientation(SYM[psOrientation]);
// @ "set-options" : void SetPrinterOptions(pstring);
@ "set-scaling" : void SetPrinterScaling(nndouble,nndouble);
@ "set-translation" : void SetPrinterTranslation(double,double);
@ "set-paper-name" : void SetPaperName(nstring);
@ "set-level-2" : void SetLevel2(bool);
@ "set-editor-margin" : void SetEditorMargin(nnlong,nnlong);
@ "set-margin" : void SetMargin(nndouble,nndouble);

@ "copy-from" : void copy(wxPrintSetupData!);

@END

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif
static Bool wxCanShowNative()
{
  wxPrintSetupData *pss;
  pss = wxGetThePrintSetupData();
  return pss->CanShowNative();
}
static Bool wxShowNative(wxWindow *w)
{
  wxPrintSetupData *pss;
  pss = wxGetThePrintSetupData();
  return pss->ShowNative(w);
}
#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

@GLOBAL wxPrintSetupGlobal

@ "can-show-print-setup?" : bool wxCanShowNative();
@ "show-print-setup" : bool wxShowNative(wxWindow^=NULL); : : /DLGORFRAME[0.METHODNAME("ps-setup","show-native")]

@END

#if 0

#ifdef wx_msw

#include "wx_mf.h"

class baseMetaFile : public wxMetaFile
{
};

#else

class baseMetaFile : public wxObject
{
public:
  Bool Ok() { return FALSE; }
  void Play(wxDC*) { }
  Bool SetClipboard(int, int) { return FALSE; }

};

#endif

@CLASSBASE baseMetaFile "meta-file" : "object"
@INTERFACE "meta-file"

// @CREATOR (string=NULL);

@ "ok?" : bool Ok();
@ "play" : void Play(wxDC!);
@ "set-clipboard" : bool SetClipboard(int=0,int=0);

@END

#endif
