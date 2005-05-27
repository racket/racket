
@INCLUDE prefix.xci

#include "wx_item.h"
#include "wx_messg.h"

@INCLUDE wxs.xci

@HEADER

@CLASSBASE wxItem "item":"window"

@CLASSID wxTYPE_ITEM

@ "command" : void Command(wxCommandEvent!);

// @INCLUDE wxs_icol.xci

@ "get-label" : nstring GetLabel();
@ "set-label" : void SetLabel(string);

// @ "set-background-colour" : void SetBackgroundColour(wxColour!);
// @ "set-label-colour" : void SetLabelColour(wxColour!);
// @ "set-button-colour" : void SetButtonColour(wxColour!);

@END

@BEGINSYMBOLS messageStyle > > BUNDLE
@SYM "deleted" : wxINVISIBLE
@ENDSYMBOLS

@BEGINSYMBOLS iconID > ONE > BUNDLE
@SYM "app" : wxMSGICON_APP
@SYM "caution" : wxMSGICON_WARNING
@SYM "stop" : wxMSGICON_ERROR
@ENDSYMBOLS

@CLASSBASE wxMessage "message" : "item"

@CREATOR (wxPanel!,string,int=-1,int=-1,SYM[messageStyle]=0,wxFont^=NULL,string="message"); <> string label
@CREATOR (wxPanel!,wxBitmap!,int=-1,int=-1,SYM[messageStyle]=0,wxFont^=NULL,string="message"); : : /CHECKOK[1.METHODNAME("message%","initialization")] <> bitmap label
@CREATOR (wxPanel!,SYM[iconID],int=-1,int=-1,SYM[messageStyle]=0,wxFont^=NULL,string="message"); <> icon label

@INCLUDE wxs_item.xci

@ "set-label" : void SetLabel(wxBitmap!) : : /CHECKOK[0.METHODNAME("message%","set-label")] <> bitmap label
@ "set-label" : void SetLabel(string); <> string label

@ "get-font" : wxFont! GetFont();

@END
