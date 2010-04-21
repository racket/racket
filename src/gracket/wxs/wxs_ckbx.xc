
@INCLUDE prefix.xci

#include "wx_check.h"

@INCLUDE wxs.xci

@HEADER

@BEGINSYMBOLS checkboxStyle > > BUNDLE
@SYM "deleted" : wxINVISIBLE
@ENDSYMBOLS

@CLASSBASE wxCheckBox "check-box":"item"

@SET CALLBACK_CLASS = wxCheckBox
@SET CALLBACK_CLASS_USER = METHODNAME("check-box%","initialization")
@INCLUDE cb_start.xci

@CREATOR (wxPanel!,wxFunction/bCallback/ubCallback/cCallback//spCallback/nopush,string,int=-1,int=-1,int=-1,int=-1,SYM[checkboxStyle]=0,wxFont^=NULL,string="checkBox"); : : ubCallbackSetup/NOZERO[5]|NOZERO[6]//ubCallbackCreatorFinish <> string label
@CREATOR (wxPanel!,wxFunction/bCallback/ubCallback/cCallback//spCallback/nopush,wxBitmap!,int=-1,int=-1,int=-1,int=-1,SYM[checkboxStyle]=0,wxFont^=NULL,string="checkBox"); : : ubCallbackSetup/CHECKOK[2.METHODNAME("check-box%","initialization")]|NOZERO[5]|NOZERO[6]//ubCallbackCreatorFinish <> bitmap label

@INCLUDE wxs_item.xci

@ "get-value" : bool GetValue()
@ "set-value" : void SetValue(bool);
@ "set-label" : void SetLabel(wxBitmap!); : : /CHECKOK[0.METHODNAME("check-box%","set-label")] <> bitmap label
@ "set-label" : void SetLabel(string); <> string label

@END

@INCLUDE cb_end.xci
