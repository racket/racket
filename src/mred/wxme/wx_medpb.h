
/* Included by wx_media.h */

class wxMediaPasteboard : public wxMediaBuffer
{
 protected:
  Bool ReadInsert(wxSnip *snip);
  void InsertPasteSnip(wxSnip *snip, wxBufferData *);
  void InsertPasteString(wxchar *str);
 public:
  wxMediaPasteboard();
  ~wxMediaPasteboard();

  virtual void OnEvent(wxMouseEvent *event);
  virtual void OnChar(wxKeyEvent *event);
  virtual wxCursor *AdjustCursor(wxMouseEvent *event);
  virtual void Refresh(double localx, double localy, double w, double h, 
		       int show_caret, wxColour *c);
  virtual void OwnCaret(Bool ownit);
  virtual void BlinkCaret();
  virtual void SizeCacheInvalid(void);
  void GetExtent(double *w, double *h);

  virtual void OnDefaultEvent(wxMouseEvent *event);
  virtual void OnDefaultChar(wxKeyEvent *event);

  virtual void OnDoubleClick(wxSnip *, wxMouseEvent *event);

  /* Callbacks for the wxSnipAdmin: */
  virtual Bool ScrollTo(wxSnip *, double localx, double localy, 
			double w, double h, Bool refresh, int bias = 0);
  virtual void SetCaretOwner(wxSnip *, int = wxFOCUS_IMMEDIATE);
  virtual void Resized(wxSnip *, Bool redraw_now);
  virtual Bool Recounted(wxSnip *, Bool redraw_now);
  virtual void NeedsUpdate(wxSnip *, double localx, double localy, 
			   double w, double h);
  virtual Bool ReleaseSnip(wxSnip *);

  double ScrollLineLocation(long line);
  long NumScrollLines();
  long FindScrollLine(double y);

  wxMediaBuffer *CopySelf(void);
  void CopySelfTo(wxMediaBuffer *b);
  double GetDescent(void);
  double GetSpace(void);
  wxchar *GetFlattenedText(long *got=NULL);
  void GetCenter(double *x, double *y);

  void Clear();
  void Cut(Bool extend=FALSE, long time=0);
  void Copy(Bool extend=FALSE, long time=0);
  void Paste(long time=0);
  void PasteSelection(long time=0);
  void Kill(long time=0);
  void SelectAll(void);

  virtual void DoCopy(long time, Bool extend);
  virtual void DoPaste(long time);
  virtual void DoPasteSelection(long time);

  void DoGenericPaste(wxClipboard *cb, long time);
  void GenericPaste(Bool x_sel, long time);

  void SetSelected(wxSnip *);
  void AddSelected(wxSnip *);
  void RemoveSelected(wxSnip *);
  void NoSelected();
  void AddSelected(double x, double y, double w, double h);

  void Insert(wxSnip *snip, wxSnip *before, double x, double y);
  void Insert(wxSnip *snip, double x, double y);
  void Insert(wxSnip *snip);
  void Insert(wxSnip *snip, wxSnip *before);

  void Delete();
  void Delete(wxSnip *snip);
  void Remove(wxSnip *snip);

  void Erase(void);

  void ChangeStyle(wxStyleDelta *delta);
  void ChangeStyle(wxStyle *style, wxSnip *snip = NULL);
  void ChangeStyle(wxStyleDelta *delta, wxSnip *snip);

  Bool ReallyCanEdit(int op);

  Bool GetSnipLocation(wxSnip *thesnip, double *x = NULL, double *y = NULL, Bool bottomRight=FALSE);

  void SetFilename(char *, Bool temp = FALSE);

  Bool WriteToFile(wxMediaStreamOut *);
  Bool ReadFromFile(wxMediaStreamIn *, Bool overwritestyle = FALSE);
  Bool SaveFile(char *filename = NULL, int format = wxMEDIA_FF_STD, Bool showErrors = TRUE);
  int InsertPort(Scheme_Object *port, int format = wxMEDIA_FF_GUESS, Bool replaceStyles = TRUE);

  void StyleHasChanged(wxStyle *style);

  /* For making a lot of changes to be displayed at once: */
  void BeginEditSequence(Bool undoable = TRUE, Bool interruptSeqs = TRUE);
  void EndEditSequence(void);
  Bool RefreshDelayed(void);
  Bool InEditSequence(void);
  Bool LocationsUpToDate(void);

  void SetMaxWidth(double w);
  void SetMinWidth(double w);
  double GetMaxWidth();
  double GetMinWidth();
  void SetMinHeight(double h);
  void SetMaxHeight(double w);
  double GetMinHeight();
  double GetMaxHeight();

  virtual void PrintToDC(wxDC *print, int page = -1);
  virtual void *BeginPrint(wxDC *, Bool);
  virtual void EndPrint(wxDC *, void *);
  virtual Bool HasPrintPage(wxDC *dc, int page);

  void AddPasteboardFunctions(wxKeymap *tab);

  wxSnip *FindSnip(double x, double y, wxSnip *after=NULL);
  wxSnip *FindFirstSnip(void);

  Bool IsSelected(wxSnip *asnip);
  wxSnip *FindNextSelectedSnip(wxSnip *start);
  
  void MoveTo(wxSnip *, double x, double y);
  void Move(wxSnip *, double x, double y);
  void Move(double x, double y);

  Bool Resize(wxSnip *, double w, double h);

  void Raise(wxSnip *snip);
  void Lower(wxSnip *snip);
  void SetBefore(wxSnip *snip, wxSnip *before);
  void SetAfter(wxSnip *snip, wxSnip *after);

  Bool GetDragable();
  void SetDragable(Bool);

  Bool GetSelectionVisible();
  void SetSelectionVisible(Bool);

  virtual void SetSnipData(wxSnip *, wxBufferData *);
  virtual wxBufferData *GetSnipData(wxSnip *snip);

  virtual void InvalidateBitmapCache(double x=0.0, double y=0.0,
				     double w=-1.0, double h=-1.0);

#if ALLOW_X_STYLE_SELECTION
  virtual Bool OwnXSelection(Bool on, Bool update, Bool force);
#endif

  /* Override these for your own use: */
  virtual void OnChange(void);
  virtual Bool CanInsert(wxSnip *, wxSnip *, double x, double y);
  virtual void OnInsert(wxSnip *, wxSnip *, double x, double y);
  virtual void AfterInsert(wxSnip *, wxSnip *, double x, double y);
  virtual Bool CanDelete(wxSnip *);
  virtual void OnDelete(wxSnip *);
  virtual void AfterDelete(wxSnip *);
  virtual Bool CanMoveTo(wxSnip *, double x, double y, Bool dragging);
  virtual void OnMoveTo(wxSnip *, double x, double y, Bool dragging);
  virtual void AfterMoveTo(wxSnip *, double x, double y, Bool dragging);
  virtual Bool CanResize(wxSnip *, double w, double h);
  virtual void OnResize(wxSnip *, double w, double h);
  virtual void AfterResize(wxSnip *, double w, double h, Bool did);
  virtual Bool CanReorder(wxSnip *, wxSnip *, Bool);
  virtual void OnReorder(wxSnip *, wxSnip *, Bool);
  virtual void AfterReorder(wxSnip *, wxSnip *, Bool);

  virtual Bool CanSelect(wxSnip *, Bool on);
  virtual void OnSelect(wxSnip *, Bool on);
  virtual void AfterSelect(wxSnip *, Bool on);

  virtual Bool CanInteractiveMove(wxMouseEvent *);
  virtual void OnInteractiveMove(wxMouseEvent *);
  virtual void AfterInteractiveMove(wxMouseEvent *);
  virtual Bool CanInteractiveResize(wxSnip *snip);
  virtual void OnInteractiveResize(wxSnip *snip);
  virtual void AfterInteractiveResize(wxSnip *snip);

  virtual void InteractiveAdjustMouse(double *x, double *y);
  virtual void InteractiveAdjustResize(wxSnip *s, double *x, double *y);
  virtual void InteractiveAdjustMove(wxSnip *s, double *x, double *y);

  void SetScrollStep(double s);
  double GetScrollStep(void);

  Bool IsLockedForWrite() { return writeLocked; }
  Bool IsLockedForFlow() { return flowLocked; }

 private:
  Bool dragable, selectionVisible;

  wxSnip *snips, *lastSnip;
  Scheme_Hash_Table *snipLocationList;

  wxStandardSnipAdmin *snipAdmin;
  
  long lastTime;
  double startX, startY;
  double lastX, lastY;

  double origX, origY, origW, origH;
  
  double maxWidth, minWidth, minHeight, maxHeight;

  Bool keepSize, dragging, rubberband;

  int needResize;

  wxSnip *resizing;
  double sizedxm, sizedym;

  double scrollStep;

  double totalWidth, totalHeight, realWidth, realHeight;

  double updateLeft, updateRight, updateTop, updateBottom;
  Bool updateNonempty, noImplicitUpdate;

  Bool sizeCacheInvalid;
  int writeLocked;
  Bool flowLocked;

  int sequence;

  int delayedscrollbias;
  wxSnip *delayedscrollsnip;
  double delayedscrollX, delayedscrollY, delayedscrollW, delayedscrollH;

  Bool sequenceStreak;

  Bool changed;

  void InitDragging(wxMouseEvent *);
  void FinishDragging(wxMouseEvent *);

  void DoSelect(wxSnip *, Bool on);

  void DoEventResize(double eventX, double eventY);
  void DoEventMove(double eventX, double eventY);

  Bool _Delete(wxSnip *, wxDeleteSnipRecord *del);

  Bool InsertFile(const char *who, Scheme_Object *f, const char *filename, Bool clearStyles, Bool showErrors);

  void Draw(wxDC *dc, double dx, double dy, 
	    double cx, double cy, double cw, double ch, 
	    int show_caret, wxColour *bg);

  Bool FindDot(wxSnipLocation *loc, double x, double y,
	       double *dxm, double *dym);

  void Update(double x, double y, double w, double h);
  void UpdateSnip(wxSnip *);
  void UpdateLocation(wxSnipLocation *);
  void UpdateSelected();
  void UpdateAll();
  void UpdateNeeded();

  void CheckRecalc();

  void RubberBand(double x, double y, double w, double h);

  void _ChangeStyle(wxStyle *style, wxStyleDelta *delta, wxSnip *snip);

  wxSnip *SnipSetAdmin(wxSnip *snip, wxSnipAdmin *a);
};
