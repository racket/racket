
@INCLUDE prefix.xci

#include "wx_style.h"
#include "wx_mtype.h"

@INCLUDE wxs.xci

@HEADER

@CLASSBASE wxMultColour "mult-color" : "object" / nofnl
@INTERFACE "mult-color"

@IVAR "r" : double r
@IVAR "g" : double g
@IVAR "b" : double b

@ "get" : void Get(double*,double*,double*);
@ "set" : void Set(double,double,double);

@END

@CLASSBASE wxAddColour "add-color" : "object" / nofnl
@INTERFACE "add-color"

@IVAR "r" : rint[-1000|1000] r
@IVAR "g" : rint[-1000|1000] g
@IVAR "b" : rint[-1000|1000] b

// short* should really be rshort[-1000|1000]*:
@ "get" : void Get(short*,short*,short*);
@ "set" : void Set(rint[-1000|1000],rint[-1000|1000],rint[-1000|1000]);

@END

@BEGINSYMBOLS family > ONE > PRED
@SYM "base" : wxBASE
@SYM "default" : wxDEFAULT
@SYM "decorative" : wxDECORATIVE
@SYM "roman" : wxROMAN
@SYM "script" : wxSCRIPT
@SYM "swiss" : wxSWISS
@SYM "modern" : wxMODERN
@SYM "system" : wxSYSTEM
@SYM "symbol" : wxSYMBOL
@ENDSYMBOLS

@BEGINSYMBOLS weight > ONE > PRED
@SYM "base" : wxBASE
@SYM "normal" : wxNORMAL
@SYM "light" : wxLIGHT
@SYM "bold" : wxBOLD
@ENDSYMBOLS

@BEGINSYMBOLS style > ONE > PRED
@SYM "base" : wxBASE
@SYM "normal" : wxNORMAL
@SYM "italic" : wxITALIC
@SYM "slant" : wxSLANT
@ENDSYMBOLS

@BEGINSYMBOLS smoothing > ONE > PRED
@SYM "base" : wxBASE
@SYM "default" : wxSMOOTHING_DEFAULT
@SYM "partly-smoothed" : wxSMOOTHING_PARTIAL
@SYM "smoothed" : wxSMOOTHING_ON
@SYM "unsmoothed" : wxSMOOTHING_OFF
@ENDSYMBOLS

@BEGINSYMBOLS align > ONE > PRED
@SYM "base" : wxBASE
@SYM "top" :  wxALIGN_TOP
@SYM "bottom" : wxALIGN_BOTTOM
@SYM "center" : wxALIGN_CENTER
@ENDSYMBOLS

@BEGINSYMBOLS changeNoArg > ONE > PRED BUNDLE
@SYM "change-nothing" : wxCHANGE_NOTHING
@SYM "change-normal" : wxCHANGE_NORMAL
@SYM "change-bold" : wxCHANGE_BOLD
@SYM "change-italic" : wxCHANGE_ITALIC
@SYM "change-toggle-underline" : wxCHANGE_TOGGLE_UNDERLINE
@SYM "change-toggle-size-in-pixels" : wxCHANGE_TOGGLE_SIP
@SYM "change-normal-color" : wxCHANGE_NORMAL_COLOUR
@ENDSYMBOLS
@BEGINSYMBOLS changeFam > ONE > BUNDLE
@SYM "change-family" : wxCHANGE_FAMILY
@ENDSYMBOLS
@BEGINSYMBOLS changeStyle > ONE > BUNDLE
@SYM "change-style" : wxCHANGE_STYLE
@SYM "change-toggle-style" : wxCHANGE_TOGGLE_STYLE
@ENDSYMBOLS
@BEGINSYMBOLS changeWeight > ONE > BUNDLE
@SYM "change-weight" : wxCHANGE_WEIGHT
@SYM "change-toggle-weight" : wxCHANGE_TOGGLE_WEIGHT
@ENDSYMBOLS
@BEGINSYMBOLS changeSmoothing > ONE > BUNDLE
@SYM "change-smoothing" : wxCHANGE_SMOOTHING
@SYM "change-toggle-smoothing" : wxCHANGE_TOGGLE_SMOOTHING
@ENDSYMBOLS
@BEGINSYMBOLS changeUnderline > ONE > BUNDLE
@SYM "change-underline" : wxCHANGE_UNDERLINE
@ENDSYMBOLS
@BEGINSYMBOLS changeSizeInPixels > ONE > BUNDLE
@SYM "change-size-in-pixels" : wxCHANGE_SIP
@ENDSYMBOLS
@BEGINSYMBOLS changeSize > ONE > BUNDLE
@SYM "change-size" : wxCHANGE_SIZE
@SYM "change-bigger" : wxCHANGE_BIGGER
@SYM "change-smaller" : wxCHANGE_SMALLER
@ENDSYMBOLS
@BEGINSYMBOLS changeAlign > ONE > BUNDLE
@SYM "change-alignment" : wxCHANGE_ALIGNMENT
@ENDSYMBOLS

@CLASSBASE wxStyleDelta "style-delta" : "object" / nofnl

@IVAR "family" : SYM[family] family
@IVAR "face" : nstring face
@IVAR "size-mult" : double sizeMult
@IVAR "size-add" : rint[0|255] sizeAdd
@IVAR "weight-on" : SYM[weight] weightOn
@IVAR "weight-off" : SYM[weight] weightOff
@IVAR "smoothing-on" : SYM[smoothing] smoothingOn
@IVAR "smoothing-off" : SYM[smoothing] smoothingOff
@IVAR "style-on" : SYM[style] styleOn
@IVAR "style-off" : SYM[style] styleOff
@IVAR "underlined-on" : bool underlinedOn
@IVAR "underlined-off" : bool underlinedOff
@IVAR "size-in-pixels-on" : bool sipOn
@IVAR "size-in-pixels-off" : bool sipOff
@IVAR "transparent-text-backing-on" : bool transparentTextBackingOn
@IVAR "transparent-text-backing-off" : bool transparentTextBackingOff
@IVAR r "foreground-mult" : wxMultColour! foregroundMult
@IVAR r "background-mult" : wxMultColour! backgroundMult
@IVAR r "foreground-add" : wxAddColour! foregroundAdd
@IVAR r "background-add" : wxAddColour! backgroundAdd
@IVAR "alignment-on" : SYM[align] alignmentOn
@IVAR "alignment-off" : SYM[align] alignmentOff
  
@CREATOR (SYM[changeNoArg]=wxCHANGE_NOTHING,-int=0); : : /setX1Zero <> no change argument
@CREATORX (SYM[changeFam],SYM[family]); <> family
@CREATORX (SYM[changeStyle],SYM[style]); <> style
@CREATORX (SYM[changeWeight],SYM[weight]); <> weight
@CREATORX (SYM[changeSmoothing],SYM[smoothing]); <> smoothing
@CREATORX (SYM[changeUnderline],bool); <> underline
@CREATORX (SYM[changeSizeInPixels],bool); <> size in pixels
@CREATORX (SYM[changeSize],rint[0|255]); <> size
@CREATORX (SYM[changeAlign],SYM[align]); <> size

@CLASSID wxTYPE_STYLE_DELTA

@MACRO setX1Zero = x1 = 0;

@ "set-delta" : wxStyleDelta! SetDelta(SYM[changeNoArg]=wxCHANGE_NOTHING,-int=0); : : /setX1Zero <> no change argument
@ "set-delta" : wxStyleDelta! SetDelta(SYM[changeFam],SYM[family]); <> family
@ "set-delta" : wxStyleDelta! SetDelta(SYM[changeStyle],SYM[style]); <> style
@ "set-delta" : wxStyleDelta! SetDelta(SYM[changeWeight],SYM[weight]); <> weight
@ "set-delta" : wxStyleDelta! SetDelta(SYM[changeSmoothing],SYM[smoothing]); <> smoothing
@ "set-delta" : wxStyleDelta! SetDelta(SYM[changeUnderline],bool); <> underline
@ "set-delta" : wxStyleDelta! SetDelta(SYM[changeSizeInPixels],bool); <> size in pixels
@ "set-delta" : wxStyleDelta! SetDelta(SYM[changeSize],rint[0|255]); <> size
@ "set-delta" : wxStyleDelta! SetDelta(SYM[changeAlign],SYM[align]); <> size

@ "set-delta-face" : wxStyleDelta! SetDeltaFace(string,SYM[family]=wxDEFAULT);
@ "set-delta-background" : wxStyleDelta! SetDeltaBackground(string); <> color name
@ "set-delta-background" : wxStyleDelta! SetDeltaBackground(wxColour!); <> colour%
@ "set-delta-foreground" : wxStyleDelta! SetDeltaForeground(string); <> color name
@ "set-delta-foreground" : wxStyleDelta! SetDeltaForeground(wxColour!); <> colour%

@ "equal?" : bool Equal(wxStyleDelta!);
@ "collapse" : bool Collapse(wxStyleDelta!);
@ "copy" : void Copy(wxStyleDelta!);

@END


@CLASSBASE wxStyle "style" : "object" / nofnl
@INTERFACE "style"

@CLASSID wxTYPE_STYLE

@ "get-name" : string GetName();
@ "get-family" : SYM[family] GetFamily();
@ "get-face" : nstring GetFace();
@ "get-size" : int GetSize();
@ "get-weight" : SYM[weight] GetWeight();
@ "get-style" : SYM[style] GetStyle();
@ "get-smoothing" : SYM[style] GetSmoothing();
@ "get-underlined" : bool GetUnderlined();
@ "get-size-in-pixels" : bool GetSizeInPixels();
@ "get-font" : wxFont! GetFont();
@ "get-foreground" : wxColour! GetForeground();
@ "get-background" : wxColour! GetBackground();
@ "get-alignment" : SYM[align] GetAlignment();
@ "get-transparent-text-backing" : bool GetTransparentTextBacking();

@ "get-text-height" : double GetTextHeight(wxDC!);
@ "get-text-descent" : double GetTextDescent(wxDC!);
@ "get-text-space" : double GetTextSpace(wxDC!);
@ "get-text-width" : double GetTextWidth(wxDC!);

@ "get-base-style" : wxStyle^ GetBaseStyle();
@ "set-base-style" : void SetBaseStyle(wxStyle!);

@ "get-delta" : void GetDelta(wxStyleDelta!);
@ "set-delta" : void SetDelta(wxStyleDelta!);

@ "is-join?" : bool IsJoin();

@ "get-shift-style" :  wxStyle^ GetShiftStyle();
@ "set-shift-style" : void SetShiftStyle(wxStyle!);

@ "switch-to" : void SwitchTo(wxDC!, wxStyle^); : : /CHECKDCOK[0.METHODNAME("style%","switch-to")]

@END


@CLASSBASE wxStyleList "style-list" : "object" / nofnl

@CREATOR ();

@CLASSID wxTYPE_STYLE_LIST

// @ "clear" : void Clear();
// @ "copy" : void Copy(wxStyleList!);

@ "basic-style" : wxStyle! BasicStyle();

@ "number" : int Number();

@ "find-or-create-style" : wxStyle! FindOrCreateStyle(wxStyle^,wxStyleDelta!);
@ "find-or-create-join-style" : wxStyle! FindOrCreateJoinStyle(wxStyle^,wxStyle!);
@ "find-named-style" : wxStyle^ FindNamedStyle(string);
@ "new-named-style" : wxStyle! NewNamedStyle(string,wxStyle!);
@ "replace-named-style" : wxStyle! ReplaceNamedStyle(string,wxStyle!);

@ "convert" : wxStyle! Convert(wxStyle!);

@MACRO bNegAsFalse = (({x} < 0) ? scheme_false : scheme_make_integer({x}))

@ "index-to-style" : wxStyle^ IndexToStyle(nnint);
@ "style-to-index" : int/bNegAsFalse StyleToIndex(wxStyle!);

static void NotifyCallbackToScheme(wxStyle *, Scheme_Object *f);

@MACRO ubCallback = (wxStyleNotifyFunc)NotifyCallbackToScheme
@MACRO ubData = (scheme_check_proc_arity(METHODNAME("style-list%","notify-on-change"), 1, POFFSET, 0, p), p[POFFSET])
@MACRO spCallback = (wxStyle-object-or-#f -> void)

@MACRO bAnythingFromLong = ((Scheme_Object *){x})
@MACRO ubAnythingToLong = ((void *){x})
@MACRO cAnything = 1

@ "notify-on-change" : void[]/bAnythingFromLong NotifyOnChange(wxStyleNotifyFunc//ubCallback///spCallback/nopush,-unknown#void*//ubData)
@ "forget-notification" : void ForgetNotification(void[]//ubAnythingToLong/cAnything///push)

@END

static void NotifyCallbackToScheme(wxStyle *s, Scheme_Object *f)
{
  Scheme_Object *p[1];
  SETUP_VAR_STACK(1);
  VAR_STACK_PUSH(0, f);

  p[0] = NULL;

  p[0] = s ? WITH_VAR_STACK(objscheme_bundle_wxStyle(s)) : scheme_false;

  WITH_VAR_STACK(scheme_apply_multi(f, 1, p));

  READY_TO_RETURN;
}

static wxStyleList* wxGetTheStyleList()
{
  return wxTheStyleList;
}

@GLOBAL wxGlobalStyleList
@ "get-the-style-list" : wxStyleList! wxGetTheStyleList()
@END
