
@INCLUDE prefix.xci

#include "wx_utils.h"
#include "wx_dialg.h"
#include "wx_cmdlg.h"
#include "wx_timer.h"
#include "wx_dcps.h"
#include "wx_main.h"

#if USE_METAFILE
#include "wx_mf.h"
#endif

@INCLUDE wxs.xci

@HEADER

static void wxsFillPrivateColor(wxDC *dc, wxColour *c)
{
#ifdef wx_x
 ((wxWindowDC *)dc)->FillPrivateColor(c);
#endif
}

#ifdef wx_msw
extern void wxNotifyCancelEndSession();
#endif
static void wxCancelQuit()
{
#ifdef wx_msw
  wxNotifyCancelEndSession();
#endif
}

static void wxsDisplayOrigin(int *x, int *y, Bool flags = FALSE)
{
#ifdef wx_xt
  wxDisplayOrigin(x, y);
#else
  wxDisplayOrigin(x, y, flags);
#endif	
}

#ifndef wxGETDIR
# define wxGETDIR 0
#endif
#ifndef wxBUNDLES_OK
# define wxBUNDLES_OK 0
#endif
#ifndef wxBUNDLES_ENTER
# define wxBUNDLES_ENTER 0
#endif

@BEGINSYMBOLS fileSelMode > PRED BUNDLE
@SYM "get" : wxOPEN
@SYM "put" : wxSAVE
@SYM "dir" : wxGETDIR
@SYM "multi" : wxMULTIOPEN
@SYM "overwrite-prompt" : wxOVERWRITE_PROMPT
@SYM "hide-readonly" : wxHIDE_READONLY
@SYM "packages" : wxBUNDLES_OK
@SYM "enter-packages" : wxBUNDLES_ENTER
@ENDSYMBOLS

#define USE_PRINTER 1

extern void *wxSchemeYield(void *sema);

extern void wxFlushDisplay(void);

#ifdef wx_x
# define FILE_SEL_DEF_PATTERN "*"
#else
# define FILE_SEL_DEF_PATTERN "*.*"
#endif

static char *wxStripMenuCodes_Scheme(char *in)
{
  static char *buffer = NULL;
  static long buflen = 0;
  long len;
  SETUP_VAR_STACK(1);
  VAR_STACK_PUSH(0, in);

  len = strlen(in);
  if (buflen <= len) {
    if (!buffer)
      wxREGGLOB(buffer);
    buflen = 2 * len + 1;
    buffer = (char *)WITH_VAR_STACK(GC_malloc_atomic(buflen));
  }

  WITH_VAR_STACK(wxStripMenuCodes(in, buffer));
  READY_TO_RETURN;
  return buffer;
}

#ifdef wx_xt
extern void wxBell(void);
#endif

@GLOBAL wxsGlobal

extern int objscheme_istype_wxFrame(Scheme_Object *obj, const char *stop, int nullOK);
extern class wxFrame *objscheme_unbundle_wxFrame(Scheme_Object *obj, const char *where, int nullOK);
extern int objscheme_istype_wxDialogBox(Scheme_Object *obj, const char *stop, int nullOK);
extern class wxDialogBox *objscheme_unbundle_wxDialogBox(Scheme_Object *obj, const char *where, int nullOK);

@MACRO ubFrameDialog[who] = (((n <= {s}) || XC_SCHEME_NULLP({x})) ? (wxWindow *)NULL : (WITH_VAR_STACK(objscheme_istype_wxFrame({x}, NULL, 1)) ? (wxWindow *)WITH_VAR_STACK(objscheme_unbundle_wxFrame({x}, NULL, 0)) : (WITH_VAR_STACK(objscheme_istype_wxDialogBox({x}, NULL, 1)) ? (wxWindow *)WITH_VAR_STACK(objscheme_unbundle_wxDialogBox({x}, NULL, 0)) : (WITH_VAR_STACK(scheme_wrong_type(<who>, "frame% or dialog%", -1, 0, &{x})), (wxWindow *)NULL))))
@MACRO cFrameDialog = (WITH_REMEMBERED_STACK(objscheme_istype_wxFrame({x}, NULL, 1)) || WITH_REMEMBERED_STACK(objscheme_istype_wxDialogBox({x}, NULL, 1)))

@ "file-selector" : npathname wxFileSelector(nstring,npathname=NULL,nxpathname=NULL,nstring=NULL,nstring=FILE_SEL_DEF_PATTERN,SYM[fileSelMode]=wxOPEN,wxWindow^//ubFrameDialog["file-selector"]/cFrameDialog=NULL,int=-1,int=-1);

@ "is-color-display?" : bool wxColourDisplay();
@ "get-display-depth" : int wxDisplayDepth();

@ "begin-busy-cursor" : void wxBeginBusyCursor()
@ "is-busy?" : bool wxIsBusy();
@ "end-busy-cursor" : void wxEndBusyCursor();
@ "hide-cursor" : void wxHideCursor()
@ "bell" : void wxBell();
@ "display-size" : void wxDisplaySize(int*,int*,int);
@ "display-origin" : void wxsDisplayOrigin(int*,int*,bool=FALSE);

@ "label->plain-label" : string wxStripMenuCodes_Scheme(string);

@ "get-resource" : bool wxGetResource(string,string,string*,npathname=NULL); <> string
@ "get-resource" : bool wxGetResource(string,string,long*,npathname=NULL); <> number
@ "write-resource" : bool wxWriteResource(string,string,string,wnpathname=NULL); <> string
@ "write-resource" : bool wxWriteResource(string,string,ExactLong,wnpathname=NULL); <> number

@MACRO BundleVoidStar = (void *){x}
@MACRO UnbundleVoidStar = (Scheme_Object *){x}
@MACRO spSema = semaphore

@ "yield" : void[]/UnbundleVoidStar wxSchemeYield(void[]=NULL//BundleVoidStar///spSema/push);
@ "flush-display" : void wxFlushDisplay();

@ "fill-private-color" : void wxsFillPrivateColor(wxDC!, wxColour!);

@ "cancel-quit" : void wxCancelQuit();

@END
