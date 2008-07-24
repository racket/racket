/* Main header file for wxMedia */
/* Include this file for anything using wxMedia */

#ifndef __WX_MEDIA__
#define __WX_MEDIA__

#include "scheme.h"

#include "wx_panel.h"
#include "wx_canvs.h"
#include "wx_dcmem.h"
#include "wx_keym.h"
#include "wx_medio.h"
#include "wx_style.h"
#include "wx_mtype.h"

class wxMediaEdit;
class wxClickback;
class wxClipboard;

extern wxchar wx_empty_wxstr[1];

#define wxFOCUS_IMMEDIATE 0
#define wxFOCUS_DISPLAY 1
#define wxFOCUS_GLOBAL 2

/* File formats */
enum {
  wxMEDIA_FF_GUESS,
  wxMEDIA_FF_STD,
  wxMEDIA_FF_TEXT,
  wxMEDIA_FF_TEXT_FORCE_CR,
  wxMEDIA_FF_SAME,
  wxMEDIA_FF_COPY
};

#include "wx_snip.h"
#include "wx_cgrec.h"
#include "wx_medad.h"

class wxSnipLocation;
class wxMediaFlashTimer;

class wxBitmap;

/* Edit commands */
enum {
  wxEDIT_UNDO = 1,
  wxEDIT_REDO,
  wxEDIT_CLEAR,
  wxEDIT_CUT,
  wxEDIT_COPY,
  wxEDIT_PASTE,
  wxEDIT_KILL,
  wxEDIT_INSERT_TEXT_BOX,
  wxEDIT_INSERT_GRAPHIC_BOX,
  wxEDIT_INSERT_IMAGE,
  wxEDIT_SELECT_ALL,
  _wx_EDIT_counter
};

/* Movement kinds */
enum {
  wxMOVE_SIMPLE = 1,
  wxMOVE_LINE,
  wxMOVE_PAGE,
  wxMOVE_WORD
};

/* For Finding Wordbreaks: */
enum {
  wxBREAK_FOR_CARET = 1,
  wxBREAK_FOR_LINE = 2,
  wxBREAK_FOR_SELECTION = 4,
  wxBREAK_FOR_USER_1 = 32,
  wxBREAK_FOR_USER_2 = 64
};

/* Selection: */
enum {
  wxDEFAULT_SELECT = 0,
  wxX_SELECT = 1,
  wxLOCAL_SELECT = 2
};

/* Drawing: */
enum {
  wxSNIP_DRAW_NO_CARET = 0,
  wxSNIP_DRAW_SHOW_INACTIVE_CARET,
  wxSNIP_DRAW_SHOW_CARET
};


/* For FindSnip: */
#define wxSNIP_BEFORE_OR_NULL (-2)
#define wxSNIP_BEFORE (-1)
#define wxSNIP_AFTER 1
#define wxSNIP_AFTER_OR_NULL 2

#define wxTAB_WIDTH 20

typedef void (*wxClickbackFunc)(wxMediaEdit *, long start, long end, void *);
typedef void (*wxWordbreakFunc)(wxMediaEdit *, long *start, long *end, int reason, void *);

class wxMediaAdmin;


class wxMediaWordbreakMap : public wxObject
{
  int usage;
 public:
  char map[256];

  wxMediaWordbreakMap();

  void SetMap(int ch, int mask);
  int GetMap(int ch);
};

extern wxMediaWordbreakMap *wxTheMediaWordbreakMap;

class wxMediaLine;

class wxMediaEdit : public wxMediaBuffer
{
  friend class wxMediaLine;
  friend class wxMediaSnipMediaAdmin;
 public:
  wxMediaEdit(double lineSpacing = 1.0, 
	      double *tabstops = NULL, int numtabs = 0);
  ~wxMediaEdit();

  wxMediaBuffer *CopySelf(void);
  void CopySelfTo(wxMediaBuffer *b);
  
  /* Usually called by wxMediaAdmin */
  virtual void OnEvent(wxMouseEvent *event);
  virtual void OnChar(wxKeyEvent *event);
  virtual wxCursor *AdjustCursor(wxMouseEvent *event);
  virtual void Refresh(double localx, double localy, double w, double h, 
		       int show_caret, wxColor *bgColor);
  virtual void OwnCaret(Bool ownit);
  virtual void BlinkCaret();
  virtual void SizeCacheInvalid(void);

  virtual void OnDefaultEvent(wxMouseEvent *event);
  virtual void OnDefaultChar(wxKeyEvent *event);

  /* Callbacks for the wxSnipAdmin: */
  virtual Bool ScrollTo(wxSnip *, double localx, double localy, 
			double w, double h, Bool refresh, int bias = 0);
  virtual void Resized(wxSnip *, Bool redraw_now);
  virtual Bool Recounted(wxSnip *, Bool redraw_now);
  virtual void NeedsUpdate(wxSnip *, double localx, double localy, 
			   double w, double h);
  virtual void SetCaretOwner(wxSnip *, int = wxFOCUS_IMMEDIATE);
  virtual Bool ReleaseSnip(wxSnip *);

  /* Methods for you to override: */
  virtual void OnChange(void);
  virtual Bool CanInsert(long start, long len);
  virtual void OnInsert(long start, long len);
  virtual void AfterInsert(long start, long len);
  virtual Bool CanDelete(long start, long len);
  virtual void OnDelete(long start, long len);
  virtual void AfterDelete(long start, long len);
  virtual Bool CanChangeStyle(long start, long len);
  virtual void OnChangeStyle(long start, long len);
  virtual void AfterChangeStyle(long start, long len);
  virtual void AfterSetPosition(void);
  virtual Bool CanSetSizeConstraint(void);
  virtual void OnSetSizeConstraint(void);
  virtual void AfterSetSizeConstraint(void);
  virtual void OnSplitSnip(long pos);
  virtual void OnMergeSnips(long pos);

  /* Set the caret position: */
  void GetPosition(long *start, long *end = NULL);
  long GetStartPosition(void);
  long GetEndPosition(void);
  void SetPosition(long start, long end = -1, 
		   Bool eol = FALSE, Bool scroll = TRUE,
		   int seltype = wxDEFAULT_SELECT);
  void MovePosition(long code, Bool extend = FALSE,
		    int kind = wxMOVE_SIMPLE);
  void SetPositionBiasScroll(int bias, long start, long end = -1, 
			     Bool eol = FALSE, Bool scroll = TRUE,
			     int seltype = wxDEFAULT_SELECT);

  virtual void SetAnchor(Bool);
  Bool GetAnchor(void);

  Bool ScrollToPosition(long start, Bool ateol = FALSE, 
			long end = -1, int bias = 0);
  void GetVisiblePositionRange(long *start, long *end, Bool all = TRUE);
  void GetVisibleLineRange(long *start, long *end, Bool all = TRUE);

  /* Hilite a region without changing the selection position: */
  void FlashOn(long start, long end, Bool ateol = FALSE, 
	       Bool scroll = TRUE, long timeout = 500);
  void FlashOff();

  /* Change the text */
  void Insert(char *str, long start, long end = -1, Bool scrollOk = TRUE);
  void Insert(char *str);
  void Insert(long len, char *str, long start, long end = -1, Bool scrollOk=TRUE);
  void Insert(long len, char *str);
  void Insert(wxchar *str, long start, long end = -1, Bool scrollOk = TRUE);
  void Insert(wxchar *str);
  void Insert(long len, wxchar *str, long start, long end = -1, Bool scrollOk=TRUE);
  void Insert(long len, wxchar *str);
  void Insert(wxSnip *snip, long start, long end = -1, Bool scrollOk=TRUE);
  void Insert(wxSnip *snip);
  void Insert(wxList *snips);
  void Insert(wxList *snips, long start, long end = -1);
  void Insert(wxchar code_point);
  void Insert(wxchar code_point, long start, long end = -1);
  void Delete(long start, long end = -1, Bool scrollOk = TRUE);
  void Delete();
  void Erase();

  void Cut(Bool extend, long time, long start, long end = -1);
  void Cut(Bool extend = FALSE, long time = 0);
  void Copy(Bool extend, long time, long start, long end = -1);
  void Copy(Bool extend = FALSE, long time = 0);
  void Paste(long time, long start, long end = -1);
  void Paste(long time = 0);
  void PasteSelection(long time, long start, long end = -1);
  void PasteSelection(long time = 0);
  void PasteNext(void);
  void Kill(long time = 0);
  void Kill(long time, long start, long end);
  void Clear(void);
  void SelectAll(void);

  virtual void DoCopy(long start, long end, long time, Bool extend);
  virtual void DoPaste(long start, long time);
  virtual void DoPasteSelection(long start, long time);
  void DoGenericPaste(wxClipboard *cb, long start, long time);

  void GenericPaste(Bool x_sel, long time, long start, long end);

  /* For making a lot of changes to be displayed at once: */
  void BeginEditSequence(Bool undoable = TRUE, Bool interruptSeqs = TRUE);
  void EndEditSequence(void);
  Bool RefreshDelayed(void);
  Bool InEditSequence(void);
  Bool LocationsUpToDate(void);

  void ChangeStyle(wxStyleDelta *);
  void ChangeStyle(wxStyle *, long start = -1, long end = -1, Bool counts_as_mod = TRUE);
  void ChangeStyle(wxStyleDelta *, long start, long end = -1, Bool counts_as_mod = TRUE);
  /* Called automatically when a style is changed; no need to call this */
  void StyleHasChanged(wxStyle *style);

  void SetStyleList(wxStyleList *styles);

  Bool ReallyCanEdit(int op);

  /* Convert canvas co-ordinates to caret position */
  long FindPosition(double x, double y, Bool 
		    *ateol = NULL, Bool *onit = NULL,
		    double *how_close = NULL);
  long FindLine(double y, Bool *onit = NULL);
  long FindPositionInLine(long i, double x, 
			  Bool *ateol = NULL, Bool *onit =NULL,
			  double *how_close = NULL);

  /* Find the line or canvas co-ordinates for a caret position */
  long PositionLine(long start, Bool eol=FALSE);
  void PositionLocation(long start, 
			double *x = NULL, double *y = NULL, 
			Bool front = TRUE, Bool eol = FALSE,
			Bool wholeLine = FALSE);
  void PositionLocations(long start, 
                         double *tx, double *ty, 
                         double *bx, double *by, 
                         Bool eol = FALSE, Bool wholeLine = FALSE);
  double LineLocation(long line, Bool top = TRUE);

  /* Get first/last caret position in a line: */
  long LineStartPosition(long i, Bool visibleOnly = TRUE);
  long LineEndPosition(long i, Bool visibleOnly = TRUE);
  long LineLength(long i);

  /* Paragraphs */
  long PositionParagraph(long start, Bool eol=FALSE);
  long ParagraphStartPosition(long i, Bool visibleOnly = TRUE);
  long ParagraphEndPosition(long i, Bool visibleOnly = TRUE);
  long LineParagraph(long line);
  long ParagraphStartLine(long i);
  long ParagraphEndLine(long i);

  long LastPosition(void);
  long LastLine(void);
  long LastParagraph(void);
  
  void SetParagraghMargins(long i, double firstLeft, double left, double right);
  void SetParagraghAlignment(long i, int align);

  void GetExtent(double *w, double *h);
  double GetDescent(void);
  double GetSpace(void);
  double GetTopLineBase(void);

  double ScrollLineLocation(long line);
  long NumScrollLines();
  long FindScrollLine(double y);

  /* Searching */
  long FindString(wxchar *str, int direction = 1, long start =-1, long end =-1,
		  Bool bos = TRUE, Bool caseSens = TRUE);
  long *FindStringAll(wxchar *str, long *cnt, int direction = 1,
		      long start =-1, long end =-1, Bool bos = TRUE,
		      Bool caseSens = TRUE);
  long FindStringUTF8(char *str, int direction = 1, long start =-1, long end =-1,
		  Bool bos = TRUE, Bool caseSens = TRUE);
  long *FindStringAllUTF8(char *str, long *cnt, int direction = 1,
		      long start =-1, long end =-1, Bool bos = TRUE,
		      Bool caseSens = TRUE);
  long FindNewline(int direction = 1, long start =-1, long end =-1);

  /* Create clickable ranges: */
  void SetClickback(long start, long end, wxClickbackFunc, void *,
		    wxStyleDelta *hiliteDelta = NULL, Bool callOnDown = FALSE);
  void RemoveClickback(long start, long end);
  void CallClickback(long start, long end);

  void SetClickback(wxClickback *); /* Used by undo record only */

  wxSnip *FindFirstSnip(void);
  wxSnip *FindNextNonTextSnip(wxSnip *snip);
  wxSnip *FindSnip(long p, int direction, long *sPos = NULL);
  Bool GetSnipPositionAndLocation(wxSnip *thesnip, long *pos, 
				  double *x = NULL, double *y = NULL);
  Bool GetSnipLocation(wxSnip *thesnip, double *x = NULL, double *y = NULL,
		       Bool bottomRight=FALSE);
  long GetSnipPosition(wxSnip *thesnip);

  wxchar *GetFlattenedText(long *len = NULL);
  wxchar *GetText(long start = -1, long end = -1, 
		  Bool flattened = FALSE, Bool forceCR = FALSE,
		  long *got = NULL);
  char *GetTextUTF8(long start = -1, long end = -1, 
		    Bool flattened = FALSE, Bool forceCR = FALSE,
		    long *got = NULL);
  wxchar GetCharacter(long start);
  char GetTruncatedCharacter(long start);

  Bool SavePort(Scheme_Object *port, int format = wxMEDIA_FF_SAME, Bool showErrors = TRUE);
  int InsertPort(Scheme_Object *port, int format = wxMEDIA_FF_GUESS, Bool replaceStyles = TRUE);

  Bool ReadFromFile(wxMediaStreamIn *, long start, Bool overwritestyle = FALSE);
  Bool ReadFromFile(wxMediaStreamIn *, Bool overwritestyle = FALSE);
  Bool WriteToFile(wxMediaStreamOut *, long start, long end = -1);
  Bool WriteToFile(wxMediaStreamOut *);

  void SetFilename(char *, Bool temp = FALSE);
  int GetFileFormat(void);
  void SetFileFormat(int);

  void SplitSnip(long pos);

  double *GetTabs(int *length = NULL, 
		 double *tabInc = NULL, Bool *inUnits = NULL);
  void SetTabs(double *tabs, int count, 
	       double tabIncrement = wxTAB_WIDTH, Bool inUnits = TRUE);

  void AddEditorFunctions(wxKeymap *keymap);

  void SetWordbreakFunc(wxWordbreakFunc f, void *data);
  void FindWordbreak(long *start, long *end, int reason);

  wxMediaWordbreakMap *GetWordbreakMap(void);
  void SetWordbreakMap(wxMediaWordbreakMap *map);

  virtual void PrintToDC(wxDC *dc, int page = -1);
  virtual void *BeginPrint(wxDC *, Bool);
  virtual void EndPrint(wxDC *, void *);
  virtual Bool HasPrintPage(wxDC *dc, int page);

  void SetMaxWidth(double w);
  void SetMinWidth(double w);
  double GetMaxWidth();
  double GetMinWidth();
  void SetMinHeight(double h);
  void SetMaxHeight(double w);
  double GetMinHeight();
  double GetMaxHeight();

  virtual void InvalidateBitmapCache(double x=0.0, double y=0.0,
				     double w=-1.0, double h=-1.0);

  /* You might need to call this if you're doing text changes within
     BeginEditSequence() and EndEditSequence(). Hopefully, it will
     get called automatically. */
  void Recalculate(void);

  void SettingAdmin(wxMediaAdmin *);
  void InitNewAdmin(void);

  virtual wxBufferData *GetRegionData(long start, long end);
  virtual void SetRegionData(long start, long end, wxBufferData *);

  void PasteRegionData(wxBufferData *);

  virtual wxTextSnip *OnNewTextSnip();
  virtual wxTabSnip *OnNewTabSnip();

  wxBitmap *SetAutowrapBitmap(wxBitmap *bm);
  virtual void OnReflow(void);

  void HideCaret(Bool hide);
  Bool CaretHidden(void);

  double GetBetweenThreshold();
  void SetBetweenThreshold(double thresh);

  inline Bool GetOverwriteMode(void) { return overwriteMode; }
  inline void SetOverwriteMode(Bool m) { overwriteMode = !!m; }

  inline double GetLineSpacing() { return lineSpacing; }
  void SetLineSpacing(double);

  inline Bool GetStickyStyles() { return stickyStyles; }
  inline void SetStickyStyles(Bool s) { stickyStyles = s; if (s) caretStyle = NULL; }

#if ALLOW_X_STYLE_SELECTION
  virtual Bool OwnXSelection(Bool on, Bool update, Bool force);
#endif

#ifdef MEMORY_USE_METHOD
  virtual long MemoryUse(void);
#endif

  Bool IsLockedForRead();
  Bool IsLockedForFlow();
  Bool IsLockedForWrite();

  double GetRevisionNumber();

 private:
#define TF_Flag(var) unsigned var : 1

  TF_Flag( readLocked );
  TF_Flag( flowLocked );
  TF_Flag( writeLocked );

  TF_Flag( hiliteOn );

  TF_Flag( changed ); /* Set if OnChange() needs to be called */

  TF_Flag( flash );
  TF_Flag( flashautoreset );
  TF_Flag( flashdirectoff );

  TF_Flag( posateol ); /* display the caret at the end of a line? */
  TF_Flag( flashposateol );
  TF_Flag( flashscroll ); /* Scroll back after unflashing? */

  TF_Flag( graphicsInvalid );
  TF_Flag( flowInvalid );
  TF_Flag( snipCacheInvalid );
  TF_Flag( graphicMaybeInvalid );
  TF_Flag( graphicMaybeInvalidForce );

  TF_Flag( typingStreak );
  TF_Flag( deletionStreak );
  TF_Flag( delayedStreak );
  TF_Flag( vcursorStreak );
  TF_Flag( killStreak );
  TF_Flag( anchorStreak );
  TF_Flag( extendStreak );
  TF_Flag( insertForceStreak );
  TF_Flag( deleteForceStreak );

  TF_Flag( keepAnchorStreak );

  TF_Flag( streaksPushed );
  TF_Flag( saveTypingStreak );
  TF_Flag( saveDeletionStreak );
  TF_Flag( saveDelayedStreak );
  TF_Flag( saveVcursorStreak );
  TF_Flag( saveKillStreak );
  TF_Flag( saveAnchorStreak );
  TF_Flag( saveExtendStreak );

  TF_Flag( dragging );
  TF_Flag( tracking );
  TF_Flag( extraLine ); /* Empty line at end of file with no representative */

  TF_Flag( delayedscrollateol );
  TF_Flag( delayedscrollbox );
  TF_Flag( drawCachedInBitmap );
  TF_Flag( refreshUnset );
  TF_Flag( refreshBoxUnset );
  TF_Flag( refreshAll );

  TF_Flag( tabSpaceInUnits );
  TF_Flag( overwriteMode );
  TF_Flag( stickyStyles );

#if ALLOW_X_STYLE_SELECTION
  TF_Flag( needXCopy );
#endif

  TF_Flag( caretBlinked ); /* Whether we want to hide an active caret or not */

  TF_Flag( initialStyleNeeded );

#undef TF_Flag

  int lastDrawCaret;
  int lastDrawXSel;

  double lineSpacing;
  double maxWidth, minWidth, minHeight, maxHeight;
  double wrapBitmapWidth;

  wxBitmap *autoWrapBitmap;

  int delayRefresh;

  long len; /* Total length in "characters" == number of positions - 1 */

  long startpos, endpos;
  long extendstartpos, extendendpos; /* for extendstreak */
  double vcursorloc; /* for vcursorStreak */

  wxMediaFlashTimer *flashTimer;
  long flashstartpos, flashendpos;

  wxSnip *snips, *lastSnip; /* The contents of this edit session */
  long snipCount;

  wxStandardSnipAdmin *snipAdmin;
  
  wxMediaLine *lineRoot, *firstLine, *lastLine; /* Line information */
  long numValidLines;

  double extraLineH;

  double totalHeight, totalWidth; /* Total height/width in canvas units */
  double finalDescent; /* Descent of last line */
  double initialSpace; /* Space from first line */
  double initialLineBase; /* Inverse descent from first line */

  wxStyle *caretStyle;

  long dragstart;

  wxClickback *trackClickback;

  long refreshStart, refreshEnd;
  double refreshL, refreshT, refreshR, refreshB;

  double lastDrawL, lastDrawT, lastDrawR, lastDrawB;
  unsigned char lastDrawRed, lastDrawGreen, lastDrawBlue;

  long delayedscroll, delayedscrollend;
  int delayedscrollbias;
  wxSnip *delayedscrollsnip;
  double delayedscrollX, delayedscrollY, delayedscrollW, delayedscrollH;

  wxList *clickbacks;

  int fileFormat;

  double betweenThreshold;

  double *tabs;
  int tabcount;
  double tabSpace;
  
  long readInsert, readInsertStart;

  long prevPasteStart, prevPasteEnd;
  long savePrevPasteStart, savePrevPasteEnd;

  double revision_count;

  wxWordbreakFunc wordBreak;
  void *wordBreakData;

  wxMediaWordbreakMap *wordBreakMap;

  void _Insert(wxSnip *snip, long len, wxchar *str, wxList *snips,
	       long start, long end = -1, 
	       Bool scrollOk = TRUE);
  void _Delete(long start, long end, Bool undo, Bool scrollOk);

  void _SetPosition(Bool setflash, int bias, long start, long end, 
		    Bool ateol, Bool scroll, int seltype);

  void _ChangeStyle(long start, long end, wxStyle *, wxStyleDelta *, Bool restoreSel = 1, Bool counts_as_mod = TRUE);

  void MakeOnlySnip(void);
  void SpliceSnip(wxSnip *snip, wxSnip *prev, wxSnip *next);
  void InsertSnip(wxSnip *before, wxSnip *snip);
  void AppendSnip(wxSnip *snip);
  void DeleteSnip(wxSnip *snip);
  wxSnip *SnipSetAdmin(wxSnip *snip, wxSnipAdmin *a);
  void SnipSplit(wxSnip *snip, long pos, wxSnip **a, wxSnip **b);

  void MakeSnipset(long start, long end);
  wxTextSnip *InsertTextSnip(long start, wxStyle *style = NULL);
  void CheckMergeSnips(long start);

  void AdjustClickbacks(long start, long end, long d, wxDeleteRecord *rec);
  wxClickback *FindClickback(long start, double y);
  void SetClickbackHilited(wxClickback *, Bool);

  Bool ScrollToPosition(long start, Bool ateol, Bool refresh, long end,
			int bias);

  long FindFirstVisiblePosition(wxMediaLine *line, wxSnip *snip = NULL);
  void FindLastVisiblePosition(wxMediaLine *line, long *p,
			       wxSnip **snipP = NULL);

  long _FindPositionLine(long pos);
  void _CalcValidPositionLine(void);

  long _FindPositionInLine(Bool internal, long i, double x, 
			   Bool *ateol = NULL, Bool *onit =NULL,
			   double *how_close = NULL);
  long _FindPositionInSnip(wxDC *dc, double X, double Y,
			   wxSnip *snip, double x, double *how_close = NULL);

  void LinesInvalidAfter(long);
  void OneLineInvalid(long);
  void SnipChangedAt(long);

  long _FindStringAll(wxchar *str, int direction,
		      long start, long end, long **positions, 
		      Bool, Bool, Bool);
  
  Bool InsertFile(const char *who, Scheme_Object *f, char *filename, int *format, Bool clearStyles, Bool showErrors);

  void RecalcLines(wxDC *dc, Bool calcGraphic = TRUE);
  Bool CheckFlow(double maxw, wxDC *dc, double Y, long startp, wxSnip *start);
  Bool CheckRecalc(Bool need_graphic = TRUE, Bool need_write = TRUE, Bool no_display_ok = FALSE);
  void Redraw(wxDC *, double, double, double, double, double, double, int, int, wxColour*);
  void Redraw();

  void NeedRefresh(long start, long end = -1);
  void NeedRefresh(double, double, double, double);
  void RefreshByLineDemand(void);
  void ContinueRefresh(void);
  void RefreshBox(double x, double y, double w, double h);

  void NeedCaretRefresh(void);
  
  void EndStreaks(int exception = 0);

  void PushStreaks(void);
  void PopStreaks(void);

 public:
  Bool ReadInsert(wxList *snipl);
 protected:
  Bool ReadInsert(wxSnip *snip);
  void InsertPasteSnip(wxSnip *snip, wxBufferData *);
  void InsertPasteString(wxchar *str);
};

#include "wx_medpb.h"

void wxInitMedia(void);
void wxAddMediaEditorFunctions(wxKeymap *tab);
void wxAddMediaPasteboardFunctions(wxKeymap *tab);

extern const char *(*wxmeExpandFilename)(const char *, const char *, int);

extern void wxmeError(const char *e);

extern wxMediaSnip *wxsMakeMediaSnip(wxMediaBuffer *useme,
				     Bool border,
				     int lm, int tm, int rm, int bm,
				     int li, int ti, int ri, int bi,
				     double w, double W, double h, double H);
extern wxMediaEdit *wxsMakeMediaEdit();
extern wxMediaPasteboard *wxsMakeMediaPasteboard();

extern int wxGetPreference(const char *name, int *res);
extern int wxGetBoolPreference(const char *name, int *res);

#endif /* __WX_MEDIA__ */
