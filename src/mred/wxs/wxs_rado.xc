
@INCLUDE prefix.xci

#include "wx_rbox.h"

@INCLUDE wxs.xci

@HEADER

@BEGINSYMBOLS radioboxStyle >  > PRED BUNDLE
@SYM "vertical" : wxVERTICAL
@SYM "horizontal" : wxHORIZONTAL
@SYM "deleted" : wxINVISIBLE
@SYM "vertical-label" : wxVERTICAL_LABEL
@SYM "horizontal-label" : wxHORIZONTAL_LABEL
@ENDSYMBOLS

@CLASSBASE wxRadioBox "radio-box":"item"

@SET CALLBACK_CLASS = wxRadioBox
@SET CALLBACK_CLASS_USER = METHODNAME("radio-box%", "initialization")
@INCLUDE cb_start.xci

#include "wxs_bmap.h"

@SET TYPE = string
@SET NOTEST = 1
@INCLUDE list.xci

@SET TYPE = wxBitmap
@SET POINTERS = 1
@SET DOOKTEST = 1
@DEFINE OKTESTWHERE METHODNAME("radio-box%", "initialization")
@INCLUDE list.xci

@MACRO cStringList = (SCHEME_LISTP({x}) && (XC_SCHEME_NULLP({x}) || SCHEME_CHAR_STRINGP(SCHEME_CAR({x}))))
@MACRO cBitmapList = (SCHEME_LISTP({x}) && (XC_SCHEME_NULLP({x}) || WITH_REMEMBERED_STACK(objscheme_istype_wxBitmap((SCHEME_CAR({x})), NULL, 0))))

@MACRO spBitmapList = (listof wxBitmap-object)

@CREATOR (wxPanel!,wxFunction/bCallback/ubCallback/cCallback//spCallback/nopush,nstring,int=-1,int=-1,int=-1,int=-1,-int=0,string[]=NULL/bList/ubList/cStringList///push,int=0,SYM[radioboxStyle]=wxVERTICAL,wxFont^=NULL,string="radioBox"); : : ubCallbackSetup/NOZERO[5]|NOZERO[6]|glueListSet[string.7.8.7.METHODNAME("radio-box%","initialization")]/glueCleanup[8]/ubCallbackCreatorFinish <> string list
@CREATOR (wxPanel!,wxFunction/bCallback/ubCallback/cCallback//spCallback/nopush,nstring,int,int,int,int,-int,wxBitmap*[]/bList/ubList/cBitmapList//spBitmapList/push,int=0,SYM[radioboxStyle]=wxVERTICAL,wxFont^=NULL,string="radioBox"); : : ubCallbackSetup/NOZERO[5]|NOZERO[6]|glueListSet[wxBitmap.7.8.7.METHODNAME("radio-box%","initialization")]/glueCleanup[8]/ubCallbackCreatorFinish <> bitmap list

@INCLUDE wxs_item.xci

#define RANGECLASS wxRadioBox
@INCLUDE range.xci

@ "get-selection" : int GetSelection();
@ "number" : int Number()
@ "set-selection" : void SetSelection(int); : : /RANGEX[0]

@ "enable" : void Enable(int,bool); : : /RANGE[0] <> single-button
@ "enable" : void Enable(bool); <> all-buttons

@ "button-focus" : int ButtonFocus(int);

@END

@INCLUDE cb_end.xci
