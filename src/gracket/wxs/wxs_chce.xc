
@INCLUDE prefix.xci

#include "wx_choic.h"

#ifndef wx_msw
void wxSetComboBoxFont(wxFont *f)
{
}
#endif

@INCLUDE wxs.xci

@HEADER

@BEGINSYMBOLS choiceStyle > > BUNDLE
@SYM "vertical-label" : wxVERTICAL_LABEL
@SYM "horizontal-label" : wxHORIZONTAL_LABEL
@SYM "deleted" : wxINVISIBLE
@ENDSYMBOLS


@CLASSBASE wxChoice "choice":"item"

@SET CALLBACK_CLASS = wxChoice
@SET CALLBACK_CLASS_USER = METHODNAME("choice%","initialization")
@INCLUDE cb_start.xci

@SET TYPE = string
@SET NOTEST = 1
@INCLUDE list.xci

#define RANGECLASS wxChoice
@INCLUDE range.xci

@CREATOR (wxPanel!,wxFunction/bCallback/ubCallback/cCallback//spCallback/nopush,nstring,int=-1,int=-1,int=-1,int=-1,-int=0,string[]=NULL/bList/ubList/cList///push,SYM[choiceStyle]=0,wxFont^=NULL,string="checkBox"); : : ubCallbackSetup/glueListSet[string.7.8.7.METHODNAME("choice%","initialization")]|NOZERO[5]|NOZERO[6]/glueCleanup[8]/ubCallbackCreatorFinish

@INCLUDE wxs_item.xci

@ "append" : void Append(string);
@ "clear" : void Clear();
@ "number" : int Number()
@ "get-selection" : int GetSelection();
@ "set-selection" : void SetSelection(int); : : /RANGE[0]

@END

@GLOBAL wxChoiceGlobal 
@ "set-combo-box-font" : void wxSetComboBoxFont(wxFont^)
@END

@INCLUDE cb_end.xci
