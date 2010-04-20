
@INCLUDE prefix.xci

#include "wx_gauge.h"

@INCLUDE wxs.xci

@HEADER

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

class wxsGauge : public wxGauge
{
 public:
   int range, pos;

   wxsGauge(wxPanel *panel, char *label, int rng,
	    int x, int y, int width, int height,
	    long style, wxFont *fnt, char *name);

   void SetRange(int r);

   void SetValue(int v);

   int GetValue(void) { return pos; }
   int GetRange(void) { return range; }
};

wxsGauge::wxsGauge(wxPanel *panel, char *label, int rng,
		   int x = -1, int y = -1, int width = -1, int height = -1,
		   long style = wxHORIZONTAL, wxFont *fnt = NULL, char *name = "gauge")
: wxGauge(panel, label, rng, x, y, width, height,
	  style, fnt, name)
{
  range = rng; pos = 0;
}

void wxsGauge::SetRange(int r)
{
  if (r > 0) {
    range = r;
    wxGauge::SetRange(r);
    if (pos > r) {
      pos = r;
      wxGauge::SetValue(r);
    }
  }
}

void wxsGauge::SetValue(int v) {
  if (v >= 0 && v <= range) {
    pos = v;
    wxGauge::SetValue(v);
  }
}

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

@BEGINSYMBOLS gaugeStyle > > PRED BUNDLE
@SYM "vertical" : wxVERTICAL
@SYM "horizontal" : wxHORIZONTAL
@SYM "vertical-label" : wxVERTICAL_LABEL
@SYM "horizontal-label" : wxHORIZONTAL_LABEL
@SYM "deleted" : wxINVISIBLE
@ENDSYMBOLS

@CLASSBASE wxsGauge "gauge" : "item"

@CREATOR (wxPanel!,nstring,int,int=-1,int=-1,int=-1,int=-1,SYM[gaugeStyle]=wxHORIZONTAL,wxFont^=NULL,string="gauge"); : : /NOZERO[5]|NOZERO[6]//

@INCLUDE wxs_item.xci

@ "set-range" : void SetRange(int);
@ "get-range" : int GetRange();
@ "set-value" : void SetValue(int);
@ "get-value" : int GetValue();

@END
