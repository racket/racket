
@INCLUDE prefix.xci

#include "wx_media.h"

@INCLUDE wxs.xci

@HEADER

@INCLUDE wxs_cret.xci

@BEGINSYMBOLS flags > > PRED
@SYM "can-append" : wxSNIP_CAN_APPEND
@SYM "newline" : wxSNIP_NEWLINE
@SYM "hard-newline" : wxSNIP_HARD_NEWLINE
@SYM "is-text" : wxSNIP_IS_TEXT
@SYM "invisible" : wxSNIP_INVISIBLE
@SYM "handles-events" : wxSNIP_HANDLES_EVENTS
@SYM "width-depends-on-x" : wxSNIP_WIDTH_DEPENDS_ON_X
@SYM "height-depends-on-x" : wxSNIP_HEIGHT_DEPENDS_ON_X
@SYM "width-depends-on-y" : wxSNIP_WIDTH_DEPENDS_ON_Y
@SYM "height-depends-on-y" : wxSNIP_HEIGHT_DEPENDS_ON_Y
@SYM "uses-buffer-path" : wxSNIP_USES_BUFFER_PATH
@ENDSYMBOLS

@INCLUDE wxs_eop.xci

@CLASSBASE wxSnip "snip":"object" / nofnl

@CREATOR ();

@CLASSID wxTYPE_SNIP

@IVAR r "count" : long count
@IVAR r "flags" : SYM[flags] flags
@IVAR r "style" : wxStyle! style
@IVAR "snipclass" : wxSnipClass^ snipclass

@ "get-admin" : wxSnipAdmin! GetAdmin();

@ "set-count" : void SetCount(rint[1|100000]);
@ "set-flags" : void SetFlags(SYM[flags]);

@ "set-style" : void SetStyle(wxStyle!)

@ "is-owned?" : bool IsOwned();
@ "release-from-owner" : bool ReleaseFromOwner();

@SETMARK s = v
@INCLUDE wxs_snip.xci

@ "next" : wxSnip^ Next();
@ "previous" : wxSnip^ Previous();

@END

@MACRO SetLength = x1 = SCHEME_CHAR_STRLEN_VAL(p[POFFSET]);

@CLASSBASE wxTextSnip "string-snip":"snip" / nofnl

@CREATOR (nnlong=0); <> initial size
@CREATOR (mzstring,-long); : : /SetLength <> initial string

@CLASSID wxTYPE_TEXT_SNIP

@SETMARK s = d
@INCLUDE wxs_snip.xci

@ "insert" : void Insert(mzstring,nnlong,nnlong=0);
@ "read" : void Read(nnlong,wxMediaStreamIn!);

@END


@CLASSBASE wxTabSnip "tab-snip":"string-snip" / nofnl

@CREATOR ();

@CLASSID wxTYPE_TAB_SNIP

@SETMARK s = d
@INCLUDE wxs_snip.xci

@END

@MACRO CheckBW[p.who] = if (x<p> && (x<p>->GetDepth() != 1)) WITH_VAR_STACK(scheme_arg_mismatch(<who>, "mask bitmap is not monochrome: ", p[POFFSET+<p>]));
@MACRO CheckSizes[p.m.who] = if (x<m> && ((x<p>->GetWidth() != x<m>->GetWidth()) || (x<p>->GetHeight() != x<m>->GetHeight()))) WITH_VAR_STACK(scheme_arg_mismatch(<who>, "mask bitmap size does not match bitmap to draw: ", p[POFFSET+<p>]));

@INCLUDE wxs_bmt.xci

@CLASSBASE wxImageSnip "image-snip":"snip" / nofnl

@CREATOR (nxpathname=NULL,SYM[bitmapType]=0,bool=FALSE,bool=TRUE); : : //USEALLFUEL[x0] <> filename
@CREATOR (wxBitmap!,wxBitmap^=NULL) : : /CheckBW[1.METHODNAME("image-snip%","initialization")]|CHECKOK[0.METHODNAME("image-snip%","initialization")]|CHECKOK[1.METHODNAME("image-snip%","initialization")]|CheckSizes[0.1.METHODNAME("image-snip%","initialization")] <> bitmap

@CLASSID wxTYPE_IMAGE_SNIP

@SETMARK s = d
@INCLUDE wxs_snip.xci

// This isn't `pathname' because it expands internally
@ "load-file" : void LoadFile(nxpathname,SYM[bitmapType]=0,bool=FALSE,bool=TRUE);  : : //USEALLFUEL[x0]

@ "get-filename" : npathname GetFilename(bool?=NULL);
@ "get-filetype" : SYM[bitmapType] GetFiletype();

@ "set-bitmap" : void SetBitmap(wxBitmap!,wxBitmap^=NULL); : : /CheckBW[1.METHODNAME("image-snip%","set-bitmap")]|CHECKOK[0.METHODNAME("image-snip%","set-bitmap")]|CHECKOK[1.METHODNAME("image-snip%","set-bitmap")]|CheckSizes[0.1.METHODNAME("image-snip%","set-bitmap")]

@ "get-bitmap" : wxBitmap^ GetSnipBitmap();
@ "get-bitmap-mask" : wxBitmap^ GetSnipBitmapMask();

@ "set-offset" : void SetOffset(double, double);

@END

@CLASSBASE wxMediaSnip "editor-snip" : "snip" / nofnl

@CREATOR (wxMediaBuffer^=NULL,bool=TRUE,nnint=wxMSNIPBOX_XMARGIN,nnint=wxMSNIPBOX_YMARGIN,nnint=wxMSNIPBOX_XMARGIN,nnint=wxMSNIPBOX_YMARGIN,nnint=wxMSNIPBOX_XINSET,nnint=wxMSNIPBOX_YINSET,nnint=wxMSNIPBOX_XINSET,nnint=wxMSNIPBOX_YINSET,nnfs[none]=-1,nnfs[none]=-1,nnfs[none]=-1,nnfs[none]=-1);

@CLASSID wxTYPE_MEDIA_SNIP

@ "get-editor" : wxMediaBuffer^ GetThisMedia();
@ "set-editor" : void SetMedia(wxMediaBuffer^);

@SETMARK s = d
@INCLUDE wxs_snip.xci

@ "set-max-width" : void SetMaxWidth(nnfs[none]);
@ "set-max-height" : void SetMaxHeight(nnfs[none]);
@ "get-max-width" : nnfs[none] GetMaxWidth();
@ "get-max-height" : nnfs[none] GetMaxHeight();
@ "set-min-width" : void SetMinWidth(nnfs[none]);
@ "set-min-height" : void SetMinHeight(nnfs[none]);
@ "get-min-width" : nnfs[none] GetMinWidth();
@ "get-min-height" : nnfs[none] GetMinHeight();

@ "get-tight-text-fit" : bool GetTightTextFit();
@ "set-tight-text-fit" : void SetTightTextFit(bool);
@ "get-align-top-line" : bool GetAlignTopLine();
@ "set-align-top-line" : void SetAlignTopLine(bool);

@ "show-border" : void ShowBorder(bool);
@ "border-visible?" : bool BorderVisible();

@ "set-margin" : void SetMargin(nnint,nnint,nnint,nnint);
@ "get-margin" :void GetMargin(nnint*,nnint*,nnint*,nnint*);
@ "set-inset" :void SetInset(nnint,nnint,nnint,nnint);
@ "get-inset" :void GetInset(nnint*,nnint*,nnint*,nnint*);

@END

@MACRO rZERO = return 0;
@MACRO rNULL = return NULL;

@CLASSBASE wxBufferDataClass "editor-data-class" : "object"

@CREATOR ()

@CLASSID wxTYPE_BUFFER_DATA_CLASS

@IVAR "classname" : string classname

@ V "read" : wxBufferData^ Read(wxMediaStreamIn!); : : : : rNULL

@END

@CLASSBASE wxBufferDataClassList "editor-data-class-list" : "object"
@INTERFACE "editor-data-class-list"

@CLASSID wxTYPE_BUFFER_DATA_CLASS_LIST

@ "find" : wxBufferDataClass^ Find(string);
@ "find-position" : short FindPosition(wxBufferDataClass!);
@ "add" : void Add(wxBufferDataClass!);
@ "number" : int Number();
@ "nth" : wxBufferDataClass^ Nth(nnint);

@END

static void SetNextNoCycle(wxBufferData *dest, wxBufferData *naya)
{
  wxBufferData *d;
  for (d = naya; d; d = d->next) {
    if (d == dest) {
      /* Don't allow it because it would create a cycle. */
      return;
    }
  }

  dest->next = naya;
}

@CLASSBASE wxBufferData "editor-data" : "object"

@CREATOR ()

@CLASSID wxTYPE_BUFFER_DATA

@IVAR "dataclass" : wxBufferDataClass^ dataclass
@IVAR r "next" : wxBufferData^ next

@ V "write" : bool Write(wxMediaStreamOut!); : : : : rZERO
@ m "set-next" : void SetNextNoCycle(wxBufferData^)

@END

