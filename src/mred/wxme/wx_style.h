
#ifndef __WXME_STYLE__
#define __WXME_STYLE__

#include "wx_list.h"
#include "wx_gdi.h"
#include "wx_dc.h"
#include "wx_utils.h"

#ifndef WXGC_NO_CLEANUP
#define WXGC_NO_CLEANUP /* empty */
#endif

#define wxBASE (-1)

#ifndef wxALIGN_TOP
enum {
  wxALIGN_TOP,
  wxALIGN_BOTTOM
# ifndef wxALIGN_CENTER
  , wxALIGN_CENTER
# endif
};
#endif

/* Simple style change commands */
enum {
  wxCHANGE_NOTHING,

  wxCHANGE_STYLE,
  wxCHANGE_WEIGHT,
  wxCHANGE_UNDERLINE,
  wxCHANGE_SIZE,
  wxCHANGE_FAMILY,
  wxCHANGE_ALIGNMENT,

  wxCHANGE_BOLD,
  wxCHANGE_ITALIC,

  wxCHANGE_SMOOTHING,

  wxCHANGE_TOGGLE_STYLE,
  wxCHANGE_TOGGLE_WEIGHT,
  wxCHANGE_TOGGLE_SMOOTHING,
  wxCHANGE_TOGGLE_UNDERLINE,

  wxCHANGE_BIGGER,
  wxCHANGE_SMALLER,

  wxCHANGE_NORMAL,

  wxCHANGE_NORMAL_COLOUR,

  wxCHANGE_SIP,
  wxCHANGE_TOGGLE_SIP,
};

class wxMultColour : public wxObject
{
 public:
  double r, g, b;
  inline wxMultColour();
  void Get(double *r, double *g, double *b);
  void Set(double r, double g, double b);
};

inline wxMultColour::wxMultColour()
: wxObject(WXGC_NO_CLEANUP)
{
}

class wxAddColour : public wxObject
{
 public:
  short r, g, b;
  inline wxAddColour();
  void Get(short *r, short *g, short *b);
  void Set(short r, short g, short b);
};

inline wxAddColour::wxAddColour()
     : wxObject(WXGC_NO_CLEANUP)
{
}

class wxStyleDelta : public wxObject
{
 public:
  int family;
  char *face;
  double sizeMult;
  int sizeAdd;
  int weightOn; /* On == Off => Toggle */ 
  int weightOff; /* On & Off, but On != Off => Converting toggle */
  int styleOn;
  int styleOff;
  int smoothingOn;
  int smoothingOff;
  Bool underlinedOn;
  Bool underlinedOff;
  Bool sipOn;
  Bool sipOff;
  Bool transparentTextBackingOn;
  Bool transparentTextBackingOff;
  wxMultColour *foregroundMult, *backgroundMult;
  wxAddColour *foregroundAdd, *backgroundAdd;
  int alignmentOn;
  int alignmentOff;
  
  wxStyleDelta(int changeCommand = wxCHANGE_NOTHING, int param = 0);
  ~wxStyleDelta();

  wxStyleDelta *SetDelta(int changeCommand, int param = 0);
  wxStyleDelta *SetDeltaFace(char *name, int family = wxDEFAULT);
  wxStyleDelta *SetDeltaBackground(char *name);
  wxStyleDelta *SetDeltaBackground(wxColour *colour);
  wxStyleDelta *SetDeltaForeground(char *name);
  wxStyleDelta *SetDeltaForeground(wxColour *colour);

  Bool Collapse(wxStyleDelta *delta);

  Bool Equal(wxStyleDelta *delta);

  void Copy(wxStyleDelta *delta);
};

class wxStyleList;

class wxStyle : public wxObject
{
  friend class wxStyleList;

 private:
  wxStyleList *styleList; /* Points back to the list owning the style */

  char *name;

  wxStyle *baseStyle;

  wxStyle *join_shiftStyle;
  wxStyleDelta *nonjoin_delta;

  /* cache computation: */
  Bool transText;
  wxColour *foreground, *background;
  wxFont *font;
  wxPen *pen;
  wxBrush *brush;
  int alignment;

  wxDC *textMetricDC;
  double textWidth, textHeight, textDescent, textSpace;

  wxList *children;

  void Update(wxStyle *basic = NULL, wxStyle *target = NULL, 
	      Bool propogate = TRUE, Bool topLevel = TRUE);

  void ResetTextMetrics(wxDC *dc);

 public:
  wxStyle();
  ~wxStyle();

  char *GetName();
  int GetFamily();
  char *GetFace();
  int GetSize();
  int GetWeight();
  int GetStyle();
  int GetSmoothing();
  Bool GetUnderlined();
  Bool GetSizeInPixels();
  wxFont *GetFont();
  wxColour *GetForeground();
  wxColour *GetBackground();
  int GetAlignment();
  Bool GetTransparentTextBacking();

  wxStyle *GetBaseStyle(void);
  void SetBaseStyle(wxStyle *baseStyle);

  Bool IsJoin(void);

  void GetDelta(wxStyleDelta *delta);
  void SetDelta(wxStyleDelta *delta);

  wxStyle *GetShiftStyle();
  void SetShiftStyle(wxStyle *);

  void SwitchTo(wxDC *dc, wxStyle *oldStyle);

  double GetTextWidth(wxDC *dc);
  double GetTextHeight(wxDC *dc);
  double GetTextDescent(wxDC *dc);
  double GetTextSpace(wxDC *dc);
};

typedef void (*wxStyleNotifyFunc)(wxStyle *which, void *data);
class wxMediaStream;

class wxStyleList : public wxList /* should be private */
{
  wxStyle *basic;
  wxList *notifications;

  wxStyle *DoNamedStyle(char *name, wxStyle *plainStyle, Bool replace);

 public:
  wxStyleList();
  ~wxStyleList();

  void Copy(wxStyleList *other);

  wxStyle *BasicStyle(void);

  int Number(void);

  /* Called automatically by style: */
  void StyleWasChanged(wxStyle *which);
  void StyleHasNewChild(wxStyle *s, wxStyle *c);
  Bool CheckForLoop(wxStyle *s, wxStyle *p);

  wxStyle *FindOrCreateStyle(wxStyle *baseStyle, wxStyleDelta *delta);

  wxStyle *FindOrCreateJoinStyle(wxStyle *baseStyle, wxStyle *shiftStyle);

  wxStyle *FindNamedStyle(char *name);
  wxStyle *NewNamedStyle(char *name, wxStyle *plainStyle);
  wxStyle *ReplaceNamedStyle(char *name, wxStyle *plainStyle);

  wxStyle *Convert(wxStyle *, Bool overwrite = FALSE);

  void *NotifyOnChange(wxStyleNotifyFunc f, void *data, int weak = 0);
  void ForgetNotification(void *id);

  wxStyle *IndexToStyle(int i);
  int StyleToIndex(wxStyle *);

  Bool WriteToFile(class wxMediaStreamOut *f);
  wxStyle *MapIndexToStyle(wxMediaStream *f, int i, long listId);
};

extern wxStyleList *wxTheStyleList;

void wxInitStyles(void);

wxStyleList *wxReadStyleList(class wxMediaStreamIn *f);

class wxMediaStream;

/* Internal use only */
Bool wxmbWriteStylesToFile(wxStyleList *styleList, class wxMediaStreamOut *f);
wxStyleList *wxmbReadStylesFromFile(wxStyleList *, class wxMediaStreamIn *f, Bool overwritename, long *listId);
void wxmbSetupStyleReadsWrites(wxMediaStream *);
void wxmbDoneStyleReadsWrites(wxMediaStream *);

#endif
