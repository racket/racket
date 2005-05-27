
@INCLUDE prefix.xci

#include "wx_gdi.h"

@INCLUDE wxs.xci

@HEADER

@INCLUDE wxs_bmt.xci

@BEGINSYMBOLS saveBitmapType > ONE > PRED BUNDLE
@SYM "bmp" : wxBITMAP_TYPE_BMP
@SYM "xbm" : wxBITMAP_TYPE_XBM
@SYM "xpm" : wxBITMAP_TYPE_XPM
@SYM "jpeg" : wxBITMAP_TYPE_JPEG
@SYM "png" : wxBITMAP_TYPE_PNG
@ENDSYMBOLS

static Bool IsColor(wxBitmap *bm)
{
  return (bm->GetDepth() != 1);
}

@CLASSBASE wxBitmap "bitmap" : "object"

@MACRO STRINGENOUGH = if (SCHEME_BYTE_STRTAG_VAL(p[POFFSET]) < (((x1 * x2) + 7) >> 3)) WITH_VAR_STACK(scheme_arg_mismatch(METHODNAME("bitmap%","initialization"), "byte string too short: ", p[POFFSET]));

@MACRO USEALLFUEL[ok] = if (<ok>) WITH_VAR_STACK(scheme_thread_block(0.0));

@CREATOR (bstring////bstring,rint[1|10000],rint[1|10000]); : : /STRINGENOUGH// <> datastring
@CREATOR (rint[1|10000],rint[1|10000],bool=0); : : <> width/height
@CREATOR (pathname////string,SYM[bitmapType]=0,wxColour^=NULL); : : //USEALLFUEL[realobj->Ok()] <> pathname

@ "get-depth" : int GetDepth();
@ "get-height" : int GetHeight();
@ "get-width" : int GetWidth();
@ "ok?" : bool Ok();
@ m "is-color?" : bool IsColor();

@ "load-file" : bool LoadFile(pathname,SYM[bitmapType]=0,wxColour^=NULL);  : : //USEALLFUEL[r]
@ "save-file" : bool SaveFile(wpathname,SYM[saveBitmapType],rint[0|100]=75);  : : //USEALLFUEL[1]

@ "get-loaded-mask" : wxBitmap! GetMask()
@ "set-loaded-mask" : void SetMask(wxBitmap!)

@ "set-gl-config" : void SetGLConfig(wxGLConfig^)
@ "get-gl-config" : wxGLConfig^ GetGLConfig()

@END
