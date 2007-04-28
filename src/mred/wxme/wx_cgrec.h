
class wxcgList;

#define CGREC_DEBUG 0

class wxChangeRecordId;

class wxChangeRecord 
#ifndef MZ_PRECISE_GC
: public gc
#endif
{
 public:
  wxChangeRecord(void);
  virtual ~wxChangeRecord();
  virtual Bool Undo(wxMediaBuffer *media);
  virtual void DropSetUnmodified(void);
  virtual Bool IsComposite();
  virtual wxChangeRecordId *GetId();
  virtual int GetParity();
  virtual wxChangeRecord *Inverse();
#if CGREC_DEBUG
  virtual char *GetName();
#endif
};

class wxSchemeModifyRecord : public wxChangeRecord
{
  void *p;
 public:
  wxSchemeModifyRecord(void *p);
  Bool Undo(wxMediaBuffer *media);
#if CGREC_DEBUG
  char *GetName();
#endif
};

class wxUnmodifyRecord : public wxChangeRecord
{
  Bool ok, cont;
 public:
  wxUnmodifyRecord(Bool cont);
  Bool Undo(wxMediaBuffer *media);
  void DropSetUnmodified(void);
#if CGREC_DEBUG
  char *GetName();
#endif
};

class wxInsertRecord : public wxChangeRecord
{
 private:
  long start, end;
  long startsel, endsel;
  Bool continued;
 public:
  wxInsertRecord(long position, long length, Bool cont, long startsel, long endsel);
  Bool Undo(wxMediaBuffer *media);
#if CGREC_DEBUG
  char *GetName();
#endif
};

class wxInsertSnipRecord : public wxChangeRecord
{
 private:
  wxSnip *snip;
  Bool continued;
 public:
  wxInsertSnipRecord(wxSnip *s, Bool cont);
  Bool Undo(wxMediaBuffer *media);
#if CGREC_DEBUG
  char *GetName();
#endif
};

class wxDeleteRecord : public wxChangeRecord
{
 private:
  Bool continued;
  long start, end;
  long startsel, endsel;
  wxcgList *deletions;
  wxcgList *clickbacks;
  Bool undid;

 public:
  wxDeleteRecord(long start, long end, Bool cont, long startsel, long endsel);
  ~wxDeleteRecord();

  void InsertSnip(wxSnip *);
  void AddClickback(wxClickback *);

  Bool Undo(wxMediaBuffer *media);

#if CGREC_DEBUG
  char *GetName();
#endif
};

class wxDeleteSnipRecord : public wxChangeRecord
{
 private:
  Bool continued;
  wxcgList *deletions;

 public:
  Bool undid;

  wxDeleteSnipRecord(Bool cont);
  ~wxDeleteSnipRecord();

  void InsertSnip(wxSnip *, wxSnip *, double, double);

  Bool Undo(wxMediaBuffer *media);

#if CGREC_DEBUG
  char *GetName();
#endif
};

class wxStyleChangeRecord : public wxChangeRecord
{
 private:
  Bool continued, restoreSelection;
  long start, end;
  long startsel, endsel;
  wxcgList *changes;

 public:
  wxStyleChangeRecord(long start, long end, Bool cont, long startsel, long endsel, Bool restoreSel = 1);
  ~wxStyleChangeRecord();

  void AddStyleChange(long start, long end, wxStyle *style);
  Bool Undo(wxMediaBuffer *media);

#if CGREC_DEBUG
  char *GetName();
#endif
};

class wxStyleChangeSnipRecord : public wxChangeRecord
{
 private:
  Bool continued;
  wxcgList *changes;

 public:
  wxStyleChangeSnipRecord(Bool cont);
  ~wxStyleChangeSnipRecord();

  void AddStyleChange(wxSnip *, wxStyle *style);
  Bool Undo(wxMediaBuffer *media);

#if CGREC_DEBUG
  char *GetName();
#endif
};

class wxMoveSnipRecord : public wxChangeRecord
{
 private:
  Bool continued;
  wxSnip *snip;
  double x, y;
  Bool delta;
 public:
  wxMoveSnipRecord(wxSnip *snip, double x, double y, Bool delta, Bool cont);
  Bool Undo(wxMediaBuffer *media);
#if CGREC_DEBUG
  char *GetName();
#endif
};

class wxResizeSnipRecord : public wxChangeRecord
{
 private:
  Bool continued;
  wxSnip *snip;
  double x, y;
 public:
  wxResizeSnipRecord(wxSnip *snip, double x, double y, Bool cont);
  Bool Undo(wxMediaBuffer *media);
#if CGREC_DEBUG
  char *GetName();
#endif
};

class wxChangeRecordId {
 public:
  wxChangeRecord *positive, *negative;
};

class wxCompositeRecord : public wxChangeRecord
{
  int cnt;
  int parity;
  wxChangeRecordId *id;
  wxChangeRecord **seq;
 public:
  wxCompositeRecord(int cnt, wxChangeRecordId *, int parity);
  ~wxCompositeRecord();
  Bool Undo(wxMediaBuffer *media);
  void DropSetUnmodified(void);
  void AddUndo(int pos, wxChangeRecord *c);
  Bool IsComposite();
  wxChangeRecordId *GetId();
  int GetParity();
  wxChangeRecord *Inverse();
#if CGREC_DEBUG
  char *GetName();
#endif
};

class wxInverseRecord : public wxChangeRecord
{
  int parity;
  wxChangeRecordId *id;
  wxChangeRecord *Get();
 public:
  wxInverseRecord(wxChangeRecordId *, int parity);
  ~wxInverseRecord();
  Bool Undo(wxMediaBuffer *media);
  void DropSetUnmodified(void);
  wxChangeRecordId *GetId();
  int GetParity();
  wxChangeRecord *Inverse();
#if CGREC_DEBUG
  char *GetName();
#endif
};
