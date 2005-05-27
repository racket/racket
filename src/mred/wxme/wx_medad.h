
#ifndef __WX_MEDIA_BUFFER__
#define __WX_MEDIA_BUFFER__

class wxStyleCanvas;
class wxStyleScrollbar;

const long
wxMCANVAS_NO_H_SCROLL = 0x10,
wxMCANVAS_NO_V_SCROLL = 0x2,
wxMCANVAS_HIDE_H_SCROLL = 0x4,
wxMCANVAS_HIDE_V_SCROLL = 0x8,
wxMCANVAS_AUTO_H_SCROLL = 0x01000000,
wxMCANVAS_AUTO_V_SCROLL = 0x02000000;

enum {
  wxEDIT_BUFFER =1,
  wxPASTEBOARD_BUFFER
};

#define STD_STYLE "Standard"

extern int wxmeCheckFormatAndVersion(wxMediaStreamIn *s, wxMediaStreamInBase *b, Bool showErrors);

class wxMediaAdmin;
class wxKeymap;
class wxImageSnip;

#ifdef OLD_WXWINDOWS
#ifndef EACH_BUFFER_OWN_OFFSCREEN
#define EACH_BUFFER_OWN_OFFSCREEN
#endif
#endif

#ifndef ALLOW_X_STYLE_SELECTION
#ifdef wx_x
#define ALLOW_X_STYLE_SELECTION 1
#else
#define ALLOW_X_STYLE_SELECTION 0
#endif
#endif

class wxMediaBuffer : public wxObject
{
 protected:
#ifdef EACH_BUFFER_OWN_OFFSCREEN
  wxBitmap *bitmap;
  wxMemoryDC *offscreen;
  long bmHeight, bmWidth;
  const Bool offscreenInUse = FALSE;
#else
  static wxBitmap *bitmap;
  static wxMemoryDC *offscreen;
  static long bmHeight, bmWidth;
  static Bool offscreenInUse;
  static wxMediaBuffer *lastUsedOffscreen;
#endif

#if ALLOW_X_STYLE_SELECTION
  virtual Bool OwnXSelection(Bool on, Bool update, Bool force) = 0;
  Bool DoOwnXSelection(Bool on, Bool force);
  void CopyOutXSelection(void);

  friend class wxMediaXClipboardClient;
#endif
  friend class wxStandardSnipAdmin;

  wxMediaAdmin *admin;

  wxKeymap *map;

#define TF_Flag(var) unsigned var : 1

  TF_Flag( ownCaret );
  TF_Flag( tempFilename ); /* TRUE if filename is just a temporary name */
  TF_Flag( userLocked );
  TF_Flag( modified );
  TF_Flag( undomode );
  TF_Flag( redomode );
  TF_Flag( interceptmode );
  TF_Flag( loadoverwritesstyles );
  TF_Flag( customCursorOverrides );
  TF_Flag( pasteTextOnly );
  TF_Flag( needOnDisplaySize );
  
  int num_parts_modified;

  int noundomode;

  wxSnip *caretSnip;

  wxStyleList *styleList;
  void *notifyId;
  
  char *filename;  /* last loaded file, or NULL */

  int maxUndos;

  wxChangeRecord **changes;
  int changes_start, changes_end;
  wxChangeRecord **redochanges;
  int redochanges_start, redochanges_end;
  wxList *intercepted;

  wxCursor *customCursor;

  int inactiveCaretThreshold;

  Bool DoWriteHeadersFooters(wxMediaStreamOut *f, Bool headers);
  Bool ReadHeadersFooters(wxMediaStreamIn *f, Bool headers);

  Bool ReadSnipsFromFile(wxMediaStreamIn *f, int overstyle);

  virtual Bool ReadInsert(wxSnip *snip) = 0;

  void AddUndo(wxChangeRecord *);

  void CopyRingNext(void);
  void BeginCopyBuffer(void);
  void EndCopyBuffer(void);
  void FreeOldCopies(void);
  void InstallCopyBuffer(long time, wxStyleList *sl);
  void DoBufferPaste(wxClipboard *cb, long time, Bool local = FALSE);
  virtual void InsertPasteSnip(wxSnip *snip, wxBufferData *) = 0;
  virtual void InsertPasteString(wxchar *str) = 0;

  void PerformUndos(wxChangeRecord **, Bool redos);
  void PerformUndoList(wxList *);
  void AppendUndo(wxChangeRecord *, wxChangeRecord **, Bool redo);

  Bool DoOwnCaret(Bool ownit);
  Bool DoSetCaretOwner(wxSnip *, int);

  Bool ReadyOffscreen(double w, double h);

  virtual void SettingAdmin(wxMediaAdmin *);
  virtual void InitNewAdmin(void); 

 public:
  wxMediaBuffer();
  ~wxMediaBuffer();

  int bufferType;
  wxDC *printing;

  void SetAdmin(wxMediaAdmin *admin);
  wxMediaAdmin *GetAdmin(void);

  virtual void OnEvent(wxMouseEvent *event) = 0;
  virtual void OnChar(wxKeyEvent *event) = 0;
  virtual wxCursor *AdjustCursor(wxMouseEvent *event) = 0;
  virtual void Refresh(double localx, double localy, double w, double h, 
		       Bool show_caret, wxColour *bgColor) = 0;
  virtual void OwnCaret(Bool ownit) = 0;
  virtual void BlinkCaret() = 0;
  virtual void SizeCacheInvalid(void) = 0;
  virtual void GetExtent(double *w, double *h) = 0;

  virtual void OnDefaultEvent(wxMouseEvent *event) = 0;
  virtual void OnLocalEvent(wxMouseEvent *event);

  virtual void OnDefaultChar(wxKeyEvent *event) = 0;
  virtual void OnLocalChar(wxKeyEvent *event);
  
  /* Callbacks for the wxSnipAdmin: */
  virtual wxDC *GetDC();
  virtual void GetViewSize(double *h, double *w);
  virtual Bool ScrollTo(wxSnip *, double localx, double localy, 
			double w, double h, Bool refresh, int bias = 0) = 0;
  virtual void SetCaretOwner(wxSnip *, int = wxFOCUS_IMMEDIATE) = 0;
  virtual void Resized(wxSnip *, Bool redraw_now) = 0;
  virtual Bool Recounted(wxSnip *, Bool redraw_now) = 0;
  virtual void NeedsUpdate(wxSnip *, double localx, double localy, 
			   double w, double h) = 0;
  virtual Bool ReleaseSnip(wxSnip *) = 0;

  virtual double ScrollLineLocation(long line) = 0;
  virtual long NumScrollLines() = 0;
  virtual long FindScrollLine(double y) = 0;

  virtual wxMediaBuffer *CopySelf(void) = 0;
  virtual void CopySelfTo(wxMediaBuffer *b);
  virtual double GetDescent(void) = 0;
  virtual double GetSpace(void) = 0;
  virtual wxchar *GetFlattenedText(long *got = NULL) = 0;
  virtual char *GetFlattenedTextUTF8(long *got = NULL);

  void GlobalToLocal(double *x, double *y);
  void LocalToGlobal(double *x, double *y);

  /* Edit & Font menus: */
  void DoEdit(int op, Bool recursive = TRUE, long time = 0);
  Bool CanEdit(int op, Bool recursive = TRUE);
  void DoFont(int op, Bool recursive = TRUE);
  virtual void ChangeStyle(wxStyleDelta *) = 0;

  virtual Bool ReallyCanEdit(int op) = 0;

  virtual void Clear() = 0;
  virtual void Cut(Bool extend, long time) = 0;
  virtual void Copy(Bool extend, long time) = 0;
  virtual void Paste(long time) = 0;
  virtual void PasteSelection(long time) = 0;
  virtual void Kill(long time) = 0;
  virtual void SelectAll(void) = 0;

  virtual void Insert(wxSnip *) = 0;

  inline wxSnip *GetFocusSnip(void) { return caretSnip; }

  void InsertBox(int type = wxEDIT_BUFFER);
  void InsertImage(char *filename = NULL, long type = 0, 
		   Bool relative = FALSE, Bool inlineImg = TRUE);

  virtual wxSnip *OnNewBox(int type);

  void Undo(void);
  void Redo(void);
  void ClearUndos(void);
  void AddSchemeUndo(void *proc);

  void SetMaxUndoHistory(int);
  int GetMaxUndoHistory();

  int AppendEditItems(wxMenu *edit, int idOffset = 0);
  int AppendFontItems(wxMenu *font, int idOffset = 0);

  virtual Bool GetSnipLocation(wxSnip *thesnip, 
			       double *x = NULL, double *y = NULL,
			       Bool bottomRight=FALSE) = 0;

  virtual Bool WriteToFile(wxMediaStreamOut *) = 0;
  virtual Bool ReadFromFile(wxMediaStreamIn *, Bool owrs = FALSE) = 0;

  /* Override this to put more information in the file. Make
     sure that you follow the rules for extra header data. */
  virtual Bool ReadHeaderFromFile(wxMediaStreamIn *, char *headerName);
  virtual Bool ReadFooterFromFile(wxMediaStreamIn *, char *headerName);
  virtual Bool WriteHeadersToFile(wxMediaStreamOut *);
  virtual Bool WriteFootersToFile(wxMediaStreamOut *);
  /* Use these functions for adding custom header data: */
  Bool BeginWriteHeaderFooterToFile(wxMediaStreamOut *, char *headerName,
				    long *dataBuffer);
  Bool EndWriteHeaderFooterToFile(wxMediaStreamOut *, long dataBuffer);
  
  void SetKeymap(wxKeymap *keymap = NULL);
  wxKeymap *GetKeymap(void);
  wxStyleList *GetStyleList(void);
  virtual void SetStyleList(wxStyleList *styles);
  virtual void StyleHasChanged(wxStyle *style) = 0;

  /* For making a lot of changes to be displayed at once: */
  virtual void BeginEditSequence(Bool undoable = TRUE, Bool interruptSeqs = TRUE) = 0;
  virtual void EndEditSequence(void) = 0;
  virtual Bool RefreshDelayed(void) = 0;
  virtual Bool InEditSequence(void) = 0;
  virtual Bool LocationsUpToDate(void) = 0;

  virtual wxSnip *FindFirstSnip(void) = 0;

  virtual void SetMaxWidth(double w) = 0;
  virtual void SetMinWidth(double w) = 0;
  virtual double GetMaxWidth() = 0;
  virtual double GetMinWidth() = 0;
  virtual void SetMinHeight(double w) = 0;
  virtual void SetMaxHeight(double w) = 0;
  virtual double GetMinHeight() = 0;
  virtual double GetMaxHeight() = 0;

  virtual void OnPaint(Bool pre, wxDC *dc,
		       double, double, double, double, 
		       double dx, double dy,
		       int show_caret);
  virtual void InvalidateBitmapCache(double x=0.0, double y=0.0,
				     double w=-1.0, double h=-1.0) = 0;

  void Print(Bool interactive=TRUE, Bool fit=FALSE, int output_mode = 0, wxWindow *parent = NULL, 
	     Bool forcePageBBox = TRUE, Bool asEPS = FALSE);
  virtual void *BeginPrint(wxDC *dc, Bool fit) = 0;
  virtual void EndPrint(wxDC*, void*) = 0;
  virtual void PrintToDC(wxDC *dc, int page = -1) = 0;
  virtual Bool HasPrintPage(wxDC *dc, int page) = 0;

  virtual Bool SaveFile(char *filename = NULL, int format = wxMEDIA_FF_SAME, Bool showErrors = TRUE) = 0;
  virtual int InsertPort(Scheme_Object *port, int format = wxMEDIA_FF_GUESS, Bool replaceStyles = TRUE) = 0;

  char *GetFilename(Bool *temp = NULL);
  virtual void SetFilename(char *, Bool temp = FALSE) = 0;
  
  virtual wxBufferData *GetSnipData(wxSnip *);
  virtual void SetSnipData(wxSnip *, wxBufferData *);

  virtual void OnChange(void) = 0;

  virtual void OnFocus(Bool on);

  virtual Bool CanSaveFile(char *filename, int format);
  virtual void OnSaveFile(char *filename, int format);
  virtual void AfterSaveFile(Bool success);
  virtual Bool CanLoadFile(char *filename, int format);
  virtual void OnLoadFile(char *filename, int format);
  virtual void AfterLoadFile(Bool success);

  virtual void OnEditSequence(void);
  virtual void AfterEditSequence(void);

  virtual void OnDisplaySize(void);
  virtual void OnDisplaySizeWhenReady(void);

  virtual wxImageSnip *OnNewImageSnip(char *filename, long type, 
				      Bool relative, Bool inlineImg);

  virtual char *GetFile(char *path);
  virtual char *PutFile(char *path, char *suggested_name);

  wxWindow *ExtractParent(void);

  int GetInactiveCaretThreshold(void);
  void SetInactiveCaretThreshold(int);

  void SetCursor(wxCursor *cursor, Bool override = TRUE);

  Bool GetPasteTextOnly(void);
  void SetPasteTextOnly(Bool pto);

  /* State */
  void Lock(Bool);
  Bool IsLocked();
  Bool Modified(void);
  virtual void SetModified(Bool);
  virtual void OnSnipModified(wxSnip *, Bool);

  void SetLoadOverwritesStyles(Bool);
  Bool GetLoadOverwritesStyles();

  void AddBufferFunctions(wxKeymap *tab);

  virtual Bool IsLockedForRead() { return 0; }
  virtual Bool IsLockedForFlow() { return 0; }
  virtual Bool IsLockedForWrite() { return 0; }

  void BeginSequenceLock();
  void EndSequenceLock();
  void WaitSequenceLock();

#ifdef MEMORY_USE_METHOD
  virtual long MemoryUse(void);
#endif

 private:
  int numExtraHeaders;
  void *seq_lock;
};

Bool wxWriteMediaVersion(wxMediaStreamOut *mf, wxMediaStreamOutBase *f);
Bool wxReadMediaVersion(wxMediaStreamIn *f, wxMediaStreamInBase *b, Bool parseFormat, Bool showErrors = TRUE);

Bool wxReadMediaGlobalHeader(wxMediaStreamIn *f);
Bool wxReadMediaGlobalFooter(wxMediaStreamIn *f);
Bool wxWriteMediaGlobalHeader(wxMediaStreamOut *f);
Bool wxWriteMediaGlobalFooter(wxMediaStreamOut *f);
wxStyle *wxmbStyleByIndex(int index);

Bool wxmbWriteSnipsToFile(wxMediaStreamOut *, wxStyleList *, 
			  wxList *, wxSnip *, wxSnip *, wxList *,
			  wxMediaBuffer *);

class wxCanvasMediaAdmin;
class SimpleScroll;
class wxTimer;
class wxAutoDragTimer;
class wxGLConfig;

class wxMediaCanvas : public wxCanvas
{
  friend class wxCanvasMediaAdmin;
  friend class wxUpdateCursorTimer;

  wxCanvasMediaAdmin *admin;
  wxMediaBuffer *media;
  wxTimer *blinkTimer;
  Bool noloop;
  long hpixelsPerScroll;
  int givenHScrollsPerPage;
  int hscrollsPerPage, vscrollsPerPage;
  int scrollHeight, scrollWidth;
  char xscroll_on, yscroll_on, auto_x, auto_y;
  Bool focuson, focusforcedon;
  Bool lazy_refresh, need_refresh;

  wxAutoDragTimer *autoDragger;

  wxCursor *customCursor;
  Bool customCursorOn;

  Bool scrollToLast, scrollBottomBased;
  int scrollOffset;

  int lastwidth, lastheight;

  int last_x, last_y;

  wxColour *bgColor;

  Bool allowXScroll, allowYScroll;
  Bool fakeXScroll, fakeYScroll;
  SimpleScroll *hscroll, *vscroll;
  void PaintScrolls(void);

  void GetScroll(int *x, int *y);
  Bool ResetVisual(Bool reset_scroll = FALSE);

  void GetView(double *fx, double *fy, double *fh, double *fw, Bool full = FALSE);
  wxDC *GetDCAndOffset(double *fx, double *fy);
  void Redraw(double, double, double, double);

  void NoCustomCursor(void);

  void UpdateCursorNow(void);

 public:
  int wheel_amt;
  int xmargin, ymargin;

  wxMediaCanvas(wxWindow *parent, int x = -1, int y = -1,
		int width = -1, int height = -1, char *name = "",
		long scrollStyle = 0,
		int scrollsPerPage = 100, wxMediaBuffer *m = NULL,
		wxGLConfig *gl_cfg = NULL);
  ~wxMediaCanvas();

  Bool IsFocusOn();

  Bool ScrollTo(double, double, double, double, Bool refresh = TRUE, int bias = 0);

  /* Override some wxCanvas methods: */
  virtual void OnSize(int, int);
  virtual void OnEvent(wxMouseEvent *event);
  virtual void OnChar(wxKeyEvent *event);
  virtual void OnPaint(void);
  virtual void OnScroll(wxScrollEvent *event);
  virtual void OnSetFocus();
  virtual void OnKillFocus();
  virtual void OnFocus(Bool focus);
  virtual void OnScrollOnChange();

  virtual void Scroll(int x, int y, Bool refresh);

  /* To block bad uses: */
  virtual void Scroll(int x, int y);
  virtual void SetScrollbars(int h_pixels, int v_pixels, int x_len, int y_len,
			     int x_page, int y_page, int x_pos=0, int y_pos=0,
			     Bool setVirtualSize = TRUE);

  void BlinkCaret();

  void ForceDisplayFocus(Bool on);

  void SetLazyRefresh(Bool on);
  Bool GetLazyRefresh(void);
  void Repaint();

  wxMediaBuffer *GetMedia(void);
  void SetMedia(wxMediaBuffer *, Bool redisplay = TRUE);

  void SetCustomCursor(wxCursor *cursor);

  void AllowScrollToLast(Bool toLast);
  void ScrollWithBottomBase(Bool bottom);

  void *CallAsPrimaryOwner(void *(*f)(void *), void *);

  virtual wxMenu *PopupForMedia(wxMediaBuffer *b, void *m);

  void ResetSize(void);
  void SetXMargin(int);
  void SetYMargin(int);
  int GetXMargin(void);
  int GetYMargin(void);

  virtual void SetCanvasBackground(wxColour *);
};

class wxCursor;

#include "wx_madm.h"

class wxStandardSnipAdmin : public wxSnipAdmin
{
  wxMediaBuffer *media;
 public:
  wxStandardSnipAdmin(wxMediaBuffer *m);

  wxMediaBuffer *GetMedia(void);

  wxDC *GetDC();
  void GetViewSize(double *h, double *w);
  void GetView(double *x, double *y, double *h, double *w, wxSnip *snip = NULL);
  Bool ScrollTo(wxSnip *, double localx, double localy, 
		double w, double h, Bool refresh, int bias = 0);
  void SetCaretOwner(wxSnip *, int = wxFOCUS_IMMEDIATE);
  void Resized(wxSnip *, Bool redraw_now);
  Bool Recounted(wxSnip *, Bool redraw_now);
  void NeedsUpdate(wxSnip *, double localx, double localy, 
		   double w, double h);
  Bool ReleaseSnip(wxSnip *);

  void UpdateCursor();
  Bool PopupMenu(void *m, wxSnip *s, double x, double y);

  void Modified(wxSnip *, Bool);
};

class wxBufferData;
class wxStyleList;

extern wxList *wxmb_commonCopyBuffer;
extern wxList *wxmb_commonCopyBuffer2;
extern wxBufferData *wxmb_commonCopyRegionData;
extern wxStyleList *wxmb_copyStyleList;

void wxGetMediaPrintMargin(long *hm=NULL, long *vm=NULL);
void wxSetMediaPrintMargin(long hm=-1, long vm=-1);

void wxAddMediaBufferFunctions(wxKeymap *tab);

#if ALLOW_X_STYLE_SELECTION
extern Bool wxMediaXSelectionMode;
extern wxMediaBuffer *wxMediaXSelectionOwner;
extern wxMediaBuffer *wxMediaXSelectionAllowed;
#endif

void wxMediaSetXSelectionMode(Bool on);

#endif /* __WX_MEDIA_BUFFER__ */
