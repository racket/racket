
@INCLUDE prefix.xci

#ifndef wx_xt
# include "wx_tabc.h"
# include "wx_gbox.h"
#endif

@INCLUDE wxs.xci

#ifdef wx_xt
/* This calls won't be instantiated, but it must compile. */
#include "wx_item.h"

class wxTabChoice : public wxItem {
public:
    wxTabChoice(wxPanel *panel, wxFunction func, char *label,
  	        int n, char **choices, int style, wxFont *fnt);

    int   GetSelection(void);
    int   Number(void);
    void  SetSelection(int n);
    void  Enable(Bool enable);
    void  Append(char *);
    void  Delete(int);
    void  SetLabel(int, char *);
    void  Set(int n, char **choices);
    int   ButtonFocus(int);
};

wxTabChoice::wxTabChoice(wxPanel *panel, wxFunction func, char *label,
			 int n, char **choices, int style, wxFont *fnt)
{
}

int wxTabChoice::GetSelection(void) { return 0; }
int wxTabChoice::Number(void) { return 0; }
void wxTabChoice::SetSelection(int n) { }
void wxTabChoice::Enable(Bool enable) { }
void wxTabChoice::Append(char *name) { }
void wxTabChoice::Delete(int which) { }
void wxTabChoice::SetLabel(int which, char *lbl) { }
void wxTabChoice::Set(int n, char **choices) { }
int wxTabChoice::ButtonFocus(int n) { return 0; }

class wxGroupBox : public wxItem {
public:
    wxGroupBox(wxPanel *panel, char *label, int style, wxFont *fnt);
};

wxGroupBox::wxGroupBox(wxPanel *panel, char *label, int style, wxFont *fnt)
{
}

#endif

@HEADER

@CLASSBASE wxTabChoice "tab-group":"item"

@SET CALLBACK_CLASS = wxTabChoice
@SET CALLBACK_CLASS_USER = METHODNAME("tab-group", "initialization")
@INCLUDE cb_start.xci

@SET TYPE = string
@SET NOTEST = 1
@INCLUDE list.xci

@MACRO cStringList = (SCHEME_LISTP({x}) && (XC_SCHEME_NULLP({x}) || SCHEME_STRINGP(SCHEME_CAR({x}))))

@BEGINSYMBOLS tabStyle > > PRED BUNDLE
@SYM "deleted" : wxINVISIBLE
@SYM "border" : wxBORDER
@ENDSYMBOLS

#define RANGECLASS wxTabChoice
@INCLUDE range.xci

@CREATOR (wxPanel!,wxFunction/bCallback/ubCallback/cCallback//spCallback/nopush,nstring,-int=0,string[]=NULL/bList/ubList/cStringList///push,SYM[tabStyle]=0,wxFont^=NULL); : : ubCallbackSetup/glueListSet[string.3.4.3.METHODNAME("tab-group","initialization")]/glueCleanup[4]/ubCallbackCreatorFinish

@INCLUDE wxs_item.xci

@ "get-selection" : int GetSelection();
@ "number" : int Number()
@ "set-selection" : void SetSelection(int); : : /RANGE[0]

@ "enable" : void Enable(bool);

@ "append" : void Append(string);
@ "delete" : void Delete(int);

@ "set-label" : void SetLabel(int,string); : : /RANGE[0]

@ "set" : void Set(-int=0,string[]=NULL/bList/ubList/cStringList///push) : : /glueListSet[string.0.1.0.METHODNAME("tab-group","set")]/glueCleanup[1]

@ "button-focus" : int ButtonFocus(int);

@END

@INCLUDE cb_end.xci



@CLASSBASE wxGroupBox "group-box":"item"

@BEGINSYMBOLS groupBoxStyle > > PRED BUNDLE
@SYM "deleted" : wxINVISIBLE
@ENDSYMBOLS

@CREATOR (wxPanel!,nstring,SYM[tabStyle]=0,wxFont^=NULL);
@INCLUDE wxs_item.xci

@END
