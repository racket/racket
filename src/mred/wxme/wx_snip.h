
#ifndef __WX_SNIP__
#define __WX_SNIP__

class wxSnipAdmin;

/* The default is that none of these flags are on: */
enum {
  wxSNIP_IS_TEXT = 0x1,
  wxSNIP_CAN_APPEND = 0x2,
  wxSNIP_INVISIBLE = 0x4,
  wxSNIP_NEWLINE = 0x8, /* Soft newline, typically inserted by wxMediaEdit */
  wxSNIP_HARD_NEWLINE = 0x10, /* => Snip must be follwed by newline */
  wxSNIP_HANDLES_EVENTS = 0x20,
  wxSNIP_WIDTH_DEPENDS_ON_X = 0x40,
  wxSNIP_HEIGHT_DEPENDS_ON_Y = 0x80,
  wxSNIP_WIDTH_DEPENDS_ON_Y = 0x100,
  wxSNIP_HEIGHT_DEPENDS_ON_X = 0x200,
  wxSNIP_ANCHORED = 0x400,
  wxSNIP_USES_BUFFER_PATH = 0x800,
  wxSNIP_CAN_SPLIT = 0x1000, /* safety feature */
  wxSNIP_OWNED = 0x2000,
  wxSNIP_CAN_DISOWN = 0x4000
};

extern void wxInitSnips(void);

#define WRITE_FUNC \
   Bool wxmbWriteSnipsToFile(class wxMediaStreamOut *, \
			     class wxStyleList *, \
			     class wxList *, class wxSnip *, \
			     class wxSnip *, wxList *, \
			     class wxMediaBuffer *)

class wxSnip;
extern WRITE_FUNC;

class wxSnipClass : public wxObject
{
 public:
  char *classname;
  int version;

  /* If this flag is TRUE, then files saved to disk will be written
     assuming that, when the file is read from disk, the class will
     always be present. */
  Bool required;
  
  wxSnipClass();

  virtual wxSnip *Read(wxMediaStreamIn *) = 0;

  virtual Bool ReadHeader(wxMediaStreamIn *);

  virtual Bool WriteHeader(wxMediaStreamOut *);

  int ReadingVersion(wxMediaStreamIn *);
};

class wxSnipClassList : public /* should be private */ wxList
{
 public:
  wxSnipClassList(void);
  ~wxSnipClassList();

  wxSnipClass *Find(char *name);
  short FindPosition(wxSnipClass *);
  int ReadingVersion(wxSnipClass *);
  void Add(wxSnipClass *snipclass); /* Checks for duplicates */
  int Number(void);
  wxSnipClass *Nth(int);
};

enum {
  wxRESET_NO_MSG = 0,
  wxRESET_DONE_READ,
  wxRESET_DONE_WRITE
};

class wxStandardSnipClassList : public wxSnipClassList
{
 private:
  wxList *unknowns;

 public:
  wxStandardSnipClassList(void);

  void ResetHeaderFlags(wxMediaStream *s);
  Bool Write(wxMediaStreamOut *f);
  Bool Read(wxMediaStreamIn *f);
  wxSnipClass *FindByMapPosition(wxMediaStream *f, short n);
};

extern wxStandardSnipClassList *wxMakeTheSnipClassList();
extern wxStandardSnipClassList *wxGetTheSnipClassList();
#define wxTheSnipClassList (*wxGetTheSnipClassList())

extern wxSnipClass *wxGetSnipClass(const char *name);

/******************************************************************/

class wxMediaLine;

class wxSnip : public wxObject
{
 private:
  friend class wxMediaEdit;
  friend class wxMediaPasteboard;
  friend class wxMediaBuffer;
  friend class wxMediaLine;
  friend WRITE_FUNC;

  /* For use only by the owning wxMediaBuffer */
  wxSnip *prev, *next;
  wxMediaLine *line;

  void Init(void);

 protected:
  wxSnipAdmin *admin;

 public:
  long count;
  long flags;
  wxSnipClass *snipclass;
  wxStyle *style;

  wxSnip();
  wxSnip(Bool cleanup);
  ~wxSnip();

  wxSnip *Next(void);
  wxSnip *Previous(void);

  wxSnipAdmin *GetAdmin(void);
  virtual void SetAdmin(wxSnipAdmin *);

  Bool IsOwned(void);
  Bool ReleaseFromOwner(void);

  void SetCount(long count);
  void SetFlags(long flags);

  virtual void OnEvent(wxDC *dc, double x, double y, 
		       double mediax, double mediay, 
		       wxMouseEvent *event);
  virtual void OnChar(wxDC *dc, double x, double y, 
		      double mediax, double mediay, 
		      wxKeyEvent *event);
  virtual wxCursor *AdjustCursor(wxDC *dc, double x, double y, 
				 double mediax, double mediay, 
				 wxMouseEvent *event);
  virtual void OwnCaret(Bool);  
  virtual void BlinkCaret(wxDC *dc, double x, double y);  
  
  virtual void DoEdit(int op, Bool recursive = TRUE, long time = 0);
  virtual Bool CanEdit(int op, Bool recursive = TRUE);
  virtual void DoFont(int op, Bool recursive = TRUE);

  virtual int Match(wxSnip *other); 

  virtual void SizeCacheInvalid(void);

  virtual void GetExtent(wxDC *dc, 
			 double x, double y,
			 double *w = NULL, double *h = NULL, 
			 double *descent = NULL, double *space = NULL,
			 double *lspace = NULL, double *rspace = NULL);
  virtual double PartialOffset(wxDC *, double x, double y, long);

  virtual void Draw(wxDC *dc, double x, double y, 
		    double,double,double,double, 
		    double dx, double xy, 
		    int show_caret);
  virtual void Split(long position, wxSnip **first, wxSnip **second);
  virtual wxSnip *MergeWith(wxSnip *pred);
  virtual void GetTextBang(wxchar *s, long offset, long num, long dt);
  virtual wxchar *GetText(long offset, long num, Bool flattened = FALSE, long *got=NULL);
  virtual char *GetTextUTF8(long offset, long num, Bool flattened = FALSE, long *got=NULL);
  virtual wxSnip *Copy();
  virtual void Write(wxMediaStreamOut *f);

  virtual Bool Resize(double w, double h);

  virtual long GetNumScrollSteps();
  virtual long FindScrollStep(double y);
  virtual double GetScrollStepOffset(long i);

  virtual void SetUnmodified();

  void SetStyle(wxStyle *s);

 protected:
  void Copy(wxSnip *); /* Copy basic values into given snip */
};

class wxInternalSnip : public wxSnip
{
 public:
  wxInternalSnip();
  wxInternalSnip(Bool cleanup);
  void SetCount(long count);
};

class wxTextSnip : public wxInternalSnip
{
 protected:
  double w; /* < 0 => need to recalc size */

  void GetTextExtent(wxDC *dc, int count, double *wo);

  void Init(long allocsize);

 public:
  long dtext;
  wxchar *buffer;

  long allocated;

  wxTextSnip(long allocsize = 0); 
  wxTextSnip(wxchar *initstring, long len); 
  wxTextSnip(char *initstring, long len); 
  ~wxTextSnip(); 

  virtual void SizeCacheInvalid(void);

  virtual void GetExtent(wxDC *dc,
			 double x, double y,
			 double *w = NULL, double *h = NULL, 
			 double *descent = NULL, double *space = NULL,
			 double *lspace = NULL, double *rspace = NULL);
  virtual double PartialOffset(wxDC *, double x, double y, long);
  virtual void Draw(wxDC *dc, double x, double y, 
		    double,double,double,double, 
		    double dx, double dy, int);
  virtual void Split(long position, wxSnip **first, wxSnip **second);
  virtual wxSnip *MergeWith(wxSnip *pred);

  virtual void Insert(wxchar *str, long len, long pos = 0);
  virtual void InsertWithOffset(wxchar *str, long len, long delta, long pos = 0);
  virtual void InsertUTF8(char *str, long len, long pos = 0);
  virtual void Read(long len, wxMediaStreamIn *f);
  virtual void GetTextBang(wxchar *s, long offset, long num, long dt);
  virtual wxchar *GetText(long offset, long num, Bool flattened = FALSE, long *got=NULL);
  virtual wxSnip *Copy(void);
  virtual void Write(wxMediaStreamOut *f);


#ifdef MEMORY_USE_METHOD
  long MemoryUse(void);
#endif

 protected:
  void Copy(wxTextSnip *); /* Copy text values into given snip */
};

class wxTabSnip : public wxTextSnip
{
 public:
  wxTabSnip();
  
  virtual void GetExtent(wxDC *dc, 
			 double x, double y,
			 double *w = NULL, double *h = NULL, 
			 double *descent = NULL, double *space = NULL,
			 double *lspace = NULL, double *rspace = NULL);
  virtual double PartialOffset(wxDC *, double x, double y, long);
  virtual void Draw(wxDC *dc, double x, double y, 
		    double,double,double,double, 
		    double dx, double dy, int);
  virtual wxSnip *Copy();
};

class wxImageSnip : public wxInternalSnip
{
 private:
  char *filename;
  long filetype; /* file != NULL => type of file, otherwise loaded 1 => XBM and 2 => XPM */
  wxBitmap *bm, *mask;
  Bool relativePath;

  void Init(void);

 protected:
  double w, h, vieww, viewh, viewdx, viewdy;
  Bool contentsChanged;

 public:
  wxImageSnip(char *name = NULL, long type = 0, Bool relative = FALSE, Bool inlineImg = TRUE); 
  wxImageSnip(wxBitmap *bm, wxBitmap *mask = NULL);
  ~wxImageSnip(); 

  virtual void SizeCacheInvalid(void);

  virtual void GetExtent(wxDC *dc,
			 double x, double y,
			 double *w = NULL, double *h = NULL, 
			 double *descent = NULL, double *space = NULL,
			 double *lspace = NULL, double *rspace = NULL);

  virtual void Draw(wxDC *dc, double x, double y, 
		    double,double,double,double, 
		    double dx, double dy, int);

  virtual wxSnip *Copy(void);

  virtual void Write(wxMediaStreamOut *f);

  void LoadFile(char *name, long type, Bool relative = FALSE, Bool inlineImg = TRUE);
  char *GetFilename(Bool *relative);
  long GetFiletype();

  void SetBitmap(wxBitmap *, wxBitmap *mask = NULL, int refresh = TRUE);
  wxBitmap *GetSnipBitmap();
  wxBitmap *GetSnipBitmapMask();

  void SetOffset(double dx, double dy);
  virtual Bool Resize(double w, double h);

  virtual long GetNumScrollSteps();
  virtual long FindScrollStep(double y);
  virtual double GetScrollStepOffset(long i);

  virtual void SetAdmin(wxSnipAdmin *);

 protected:
  void Copy(wxImageSnip *);
};

class wxSnipAdmin : public wxObject
{
 public:
  inline wxSnipAdmin();

  virtual wxMediaBuffer *GetMedia(void) = 0;

  virtual wxDC *GetDC() = 0;
  virtual void GetViewSize(double *h, double *w) = 0;
  virtual void GetView(double *x, double *y, double *h, double *w, wxSnip *snip = NULL) = 0;
  virtual Bool ScrollTo(wxSnip *, double localx, double localy, 
			double w, double h, Bool refresh, int bias = 0) = 0;
  virtual void SetCaretOwner(wxSnip *, int = wxFOCUS_IMMEDIATE) = 0;
  virtual void Resized(wxSnip *, Bool redraw_now) = 0;
  virtual Bool Recounted(wxSnip *, Bool redraw_now) = 0;
  virtual void NeedsUpdate(wxSnip *, double localx, double localy, 
			   double w, double h) = 0;
  virtual Bool ReleaseSnip(wxSnip *) = 0;

  virtual void UpdateCursor() = 0;
  virtual Bool PopupMenu(void *m, wxSnip *s, double x, double y) = 0;

  virtual void Modified(wxSnip *s, Bool mod) = 0;
};

inline wxSnipAdmin::wxSnipAdmin()
     : wxObject(WXGC_NO_CLEANUP)
{
}

#define wxMSNIPBOX_XMARGIN 5
#define wxMSNIPBOX_YMARGIN 5
#define wxMSNIPBOX_XINSET 1
#define wxMSNIPBOX_YINSET 1

class wxMediaSnipMediaAdmin;

class wxMediaSnip : public wxInternalSnip
{
  friend class wxMediaSnipMediaAdmin;

  wxMediaBuffer *me;
  wxMediaSnipMediaAdmin *myAdmin;

#define TF_Flag(var) unsigned var : 1
  TF_Flag( withBorder );
  TF_Flag( tightFit );
  TF_Flag( alignTopLine );
#undef TF_Flag

  int leftMargin, topMargin, rightMargin, bottomMargin;
  int leftInset, topInset, rightInset, bottomInset;

  double minWidth, maxWidth, minHeight, maxHeight;

 public:
  wxMediaSnip(wxMediaBuffer *useme = NULL,
	      Bool withBorder = TRUE,
	      int leftMargin = wxMSNIPBOX_XMARGIN,
	      int topMargin = wxMSNIPBOX_YMARGIN,
	      int rightMargin = wxMSNIPBOX_XMARGIN,
	      int bottomMargin = wxMSNIPBOX_YMARGIN,
	      int leftInset = wxMSNIPBOX_XINSET,
	      int topInset = wxMSNIPBOX_YINSET,
	      int rightInset = wxMSNIPBOX_XINSET,
	      int bottomInset = wxMSNIPBOX_YINSET,
	      double w = -1, double W = -1, 
	      double h = -1, double H = -1);
  ~wxMediaSnip();

  virtual void SetAdmin(wxSnipAdmin *a);

  virtual void OnEvent(wxDC *, double, double, double,double, wxMouseEvent *event);
  virtual void OnChar(wxDC *, double, double, double, double, wxKeyEvent *event);
  virtual wxCursor *AdjustCursor(wxDC *, double, double, double,double, wxMouseEvent *event);
  virtual void OwnCaret(Bool);  
  virtual void BlinkCaret(wxDC *dc, double x, double y);

  virtual void DoEdit(int op, Bool recursive = TRUE, long time = 0);
  virtual Bool CanEdit(int op, Bool recursive = TRUE);
  virtual void DoFont(int op, Bool recursive = TRUE);

  virtual Bool Match(wxSnip *other); 

  virtual void SizeCacheInvalid(void);

  virtual wxchar *GetText(long offset, long num, Bool flattened = FALSE, long *got = NULL);

  virtual void GetExtent(wxDC *dc, 
			 double x, double y,
			 double *w = NULL, double *h = NULL, 
			 double *descent = NULL, double *space = NULL,
			 double *lspace = NULL, double *rspace = NULL);
  virtual void Draw(wxDC *dc, double x, double y, 
		    double, double, double, double, double dx, double dy, 
		    int show_caret);
  virtual wxSnip *Copy(void);

  virtual void Write(wxMediaStreamOut *f);

  virtual long GetNumScrollSteps();
  virtual long FindScrollStep(double y);
  virtual double GetScrollStepOffset(long i);

  void SetMaxWidth(double);
  void SetMaxHeight(double);
  double GetMaxWidth(void);
  double GetMaxHeight(void);
  void SetMinWidth(double);
  void SetMinHeight(double);
  double GetMinWidth(void);
  double GetMinHeight(void);

  Bool GetTightTextFit(void);
  void SetTightTextFit(Bool);
  Bool GetAlignTopLine(void);
  void SetAlignTopLine(Bool);

  void ShowBorder(Bool show);
  Bool BorderVisible();

  void SetMargin(int lm, int tm, int rm, int bm);
  void GetMargin(int *lm, int *tm, int *rm, int *bm);
  void SetInset(int lm, int tm, int rm, int bm);
  void GetInset(int *li, int *ti, int *ri, int *bi);

  virtual Bool Resize(double w, double h);

  void SetUnmodified();

  wxMediaBuffer *GetThisMedia(void);
  void SetMedia(wxMediaBuffer *b);
};

/**********************************************************************/

class wxMediaStreamOut;
class wxBufferData;

class wxBufferDataClass : public wxObject
{
 private:
  friend class wxBufferDataClassList;
  friend class wxMediaBuffer;
  friend WRITE_FUNC;
  friend Bool wxmbWriteBufferData(wxMediaStreamOut *, wxBufferData *data);

 public:
  char *classname;
  Bool required;

  wxBufferDataClass();

  virtual wxBufferData *Read(wxMediaStreamIn *) = 0;
};

class wxBufferData : public wxObject
{
 public:
  wxBufferDataClass *dataclass;
  wxBufferData *next; /* Used to chain them in a list */

  wxBufferData();
  ~wxBufferData();

  virtual Bool Write(wxMediaStreamOut *) = 0;
};

class wxBufferDataClassList : public /* should be private */ wxList
{
 private:
  wxList *unknowns;

 public:
  wxBufferDataClassList();
  ~wxBufferDataClassList();

  wxBufferDataClass *Find(char *name);
  short FindPosition(wxBufferDataClass *);
  void Add(wxBufferDataClass *dataclass); /* Checks for duplicates */
  int Number(void);
  wxBufferDataClass *Nth(int);

  Bool Write(wxMediaStreamOut *f);
  Bool Read(wxMediaStreamIn *f);
  wxBufferDataClass *FindByMapPosition(wxMediaStream *f, short n);
};

extern wxBufferDataClassList *wxMakeTheBufferDataClassList();
extern wxBufferDataClassList *wxGetTheBufferDataClassList();
#define wxTheBufferDataClassList (*wxGetTheBufferDataClassList())

extern wxBufferDataClass *wxGetEditorDataClass(const char *name);

/**********************************************************************/

class wxLocationBufferData : public wxBufferData
{
 public:
  double x, y;

  wxLocationBufferData();

  Bool Write(wxMediaStreamOut *);
};

#endif /* __WX_SNIP__ */
