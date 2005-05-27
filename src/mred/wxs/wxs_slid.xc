
@INCLUDE prefix.xci

#include "wx_slidr.h"

@INCLUDE wxs.xci

@HEADER

#define wxPLAIN_SLIDER (wxHORIZONTAL << 2)

@BEGINSYMBOLS sliderStyle > > PRED BUNDLE
@SYM "vertical" : wxVERTICAL
@SYM "horizontal" : wxHORIZONTAL
@SYM "plain" : wxPLAIN_SLIDER
@SYM "vertical-label" : wxVERTICAL_LABEL
@SYM "horizontal-label" : wxHORIZONTAL_LABEL
@SYM "deleted" : wxINVISIBLE
@ENDSYMBOLS

@CLASSBASE wxSlider "slider" : "item"

@SET CALLBACK_CLASS = wxSlider
@SET CALLBACK_CLASS_USER = METHODNAME("slider%","initialization")
@INCLUDE cb_start.xci

@MACRO PROGRESS = if (x3 < x4 || x5 < x3) WITH_VAR_STACK(scheme_arg_mismatch(METHODNAME("slider%","initialization"), "minimum, value, and maximum must be increasing; given minimum: ", p[POFFSET+4]));
@MACRO NOZEROX[p] = if (x<p> <= 0) x<p> = 1;

@CREATOR (wxPanel!,wxFunction/bCallback/ubCallback/cCallback//spCallback/nopush,nstring,int,int,int,int,int=-1,int=-1,SYM[sliderStyle]=wxHORIZONTAL,wxFont^=NULL,string="slider"); : : ubCallbackSetup/PROGRESS|NOZEROX[6]//ubCallbackCreatorFinish

@INCLUDE wxs_item.xci

@ "get-value" : int GetValue()
@ "set-value" : void SetValue(int);

@END

@INCLUDE cb_end.xci
