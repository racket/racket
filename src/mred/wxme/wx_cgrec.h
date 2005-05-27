
class wxcgList;

class wxChangeRecord 
{
 public:
  wxChangeRecord(void);
  virtual ~wxChangeRecord();
  virtual Bool Undo(wxMediaBuffer *media);
  virtual void DropSetUnmodified(void);
};

class wxSchemeModifyRecord : public wxChangeRecord
{
  void *p;
 public:
  wxSchemeModifyRecord(void *p);
  Bool Undo(wxMediaBuffer *media);
};

class wxUnmodifyRecord : public wxChangeRecord
{
  int ok;
 public:
  wxUnmodifyRecord(void);
  Bool Undo(wxMediaBuffer *media);
  void DropSetUnmodified(void);
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
};

class wxInsertSnipRecord : public wxChangeRecord
{
 private:
  wxSnip *snip;
  Bool continued;

 public:
  wxInsertSnipRecord(wxSnip *s, Bool cont);

  Bool Undo(wxMediaBuffer *media);
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
};

