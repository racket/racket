
#ifndef wb_rgnh
#define wb_rgnh

#include "wx_dc.h"

/************************************************************/

/* We start with wxPSStream, because we need it to output region
   paths. */

class wxPSStream : public wxObject {
 public:
  void *f;
  int int_width;
  wxPSStream(char *file);
  ~wxPSStream(void);

  int good(void);
  void Out(char s);
  void Out(const char *s);
  void Out(double n);
  void Out(long l);
  void Out(int i);

  long tellp(void);
  void seekp(long pos);

  void width(int w);
};

class wxPostScriptDC;

/************************************************************/

#ifdef UseXtRegions
typedef Region XtRegion;
#else
typedef void *XtRegion;
#endif

class wxPathRgn;
class wxPath;

class wxRegion : public wxObject 
{
 public:
#ifdef wx_msw
  HRGN rgn;
#endif
#ifdef wx_x
  XtRegion rgn;
#endif
#ifdef wx_mac
  RgnHandle rgn;
#endif
  wxPathRgn *prgn;
  wxDC *dc;
  char is_ps, locked, no_prgn;

  wxRegion(wxDC *dc, wxRegion *r = NULL, Bool no_prgn = FALSE);
  ~wxRegion();

  inline wxDC *GetDC() { return dc; }
  
  void SetRectangle(double x, double y, double width, double height);
  void SetRoundedRectangle(double x, double y, double width, double height, double radius = 20.0);
  void SetEllipse(double x, double y, double width, double height);
  void SetPolygon(int n, wxPoint points[], double xoffset = 0, double yoffset = 0, 
		  int fillStyle=wxODDEVEN_RULE, int delta = 0);
  void SetPath(wxPath *p, double xoffset = 0, double yoffset = 0, 
	       int fillStyle=wxODDEVEN_RULE);
  void SetArc(double x, double y, double w, double h, double start, double end);

  void Union(wxRegion *);
  void Intersect(wxRegion *);
  void Subtract(wxRegion *);
  void Xor(wxRegion *r);

  void BoundingBox(double *x, double *y, double *w, double *h);

  Bool Empty();
  Bool ReallyEmpty();

  Bool IsInRegion(double x, double y);
  
  void Cleanup();

  void Install(long target, Bool align);
  void InstallPS(wxPostScriptDC *dc, wxPSStream *s);
};

/************************************************************/

class wxPathRgn : public wxObject
{
 public:
  double ox, oy, sx, sy;

  wxPathRgn(wxDC *dc_for_scale);
  ~wxPathRgn();
  virtual Bool Install(long target, Bool reverse, Bool align) = 0;

  long PrepareScale(long target, Bool oe, Bool align);
  void RestoreScale(long target, long v, Bool align);

  virtual Bool InstallPS(wxPostScriptDC *dc, wxPSStream *s) = 0;

  double XFormX(double x, Bool align);
  double XFormY(double x, Bool align);
  double XFormXB(double x, Bool align);
  double XFormYB(double x, Bool align);
  double XFormW(double w, double x, Bool align);
  double XFormH(double h, double y, Bool align);
};

class wxRectanglePathRgn : public wxPathRgn
{
 public:
  double x;
  double y;
  double width;
  double height;
  wxRectanglePathRgn(wxDC *dc_for_scale, double x, double y, double width, double height);
  virtual Bool Install(long target, Bool reverse, Bool align);
  virtual Bool InstallPS(wxPostScriptDC *dc, wxPSStream *s);
};

class wxRoundedRectanglePathRgn : public wxPathRgn
{
 public:
  double x;
  double y;
  double width;
  double height;
  double radius;
  wxRoundedRectanglePathRgn(wxDC *dc_for_scale, double x, double y, double width, double height, double radius);
  virtual Bool Install(long target, Bool reverse, Bool align);
  virtual Bool InstallPS(wxPostScriptDC *dc, wxPSStream *s);
};

class wxPolygonPathRgn : public wxPathRgn
{
 public:
  int n;
  wxPoint *points;
  double xoffset;
  double yoffset;
  int fillStyle;
  wxPolygonPathRgn(wxDC *dc_for_scale, int n, wxPoint points[], double xoffset, double yoffset, int fillStyle);
  virtual Bool Install(long target, Bool reverse, Bool align);
  virtual Bool InstallPS(wxPostScriptDC *dc, wxPSStream *s);
};

class wxArcPathRgn : public wxPathRgn
{
 public:
  double x;
  double y;
  double w;
  double h;
  double start;
  double end;
  wxArcPathRgn(wxDC *dc_for_scale, double x, double y, double w, double h, double start, double end);
  virtual Bool Install(long target, Bool reverse, Bool align);
  virtual Bool InstallPS(wxPostScriptDC *dc, wxPSStream *s);
};

class wxPathPathRgn : public wxPathRgn
{
 public:
  wxPath *p;
  double xoffset;
  double yoffset;
  int fillStyle;
  wxPathPathRgn(wxDC *dc_for_scale, wxPath *p, double xoffset, double yoffset, int fillStyle);
  virtual Bool Install(long target, Bool reverse, Bool align);
  virtual Bool InstallPS(wxPostScriptDC *dc, wxPSStream *s);
};

class wxUnionPathRgn : public wxPathRgn
{
 public:
  wxPathRgn *a, *b;
  wxUnionPathRgn(wxPathRgn *f, wxPathRgn *s);
  virtual Bool Install(long target, Bool reverse, Bool align);
  virtual Bool InstallPS(wxPostScriptDC *dc, wxPSStream *s);
};

class wxIntersectPathRgn : public wxPathRgn
{
 public:
  wxPathRgn *a, *b;
  wxIntersectPathRgn(wxPathRgn *f, wxPathRgn *s);
  virtual Bool Install(long target, Bool reverse, Bool align);
  virtual Bool InstallPS(wxPostScriptDC *dc, wxPSStream *s);
};

class wxDiffPathRgn : public wxPathRgn
{
 public:
  wxPathRgn *a, *b;
  wxDiffPathRgn(wxPathRgn *f, wxPathRgn *s);
  virtual Bool Install(long target, Bool reverse, Bool align);
  virtual Bool InstallPS(wxPostScriptDC *dc, wxPSStream *s);
};

/************************************************************/

class wxPath : public wxObject
{
 public:
  long cmd_size, alloc_cmd_size, last_cmd;
  double *cmds;

  int num_polys;
  double **poly_pts;

  wxPath();
  ~wxPath();

  void Reset();

  Bool IsOpen();

  void Close();
  void MoveTo(double x, double y);
  void LineTo(double x, double y);
  void Arc(double x, double y, double w, double h, double start, double end, Bool ccw);
  void CurveTo(double x1, double y1, double x2, double y2, double x3, double y3);

  void Rectangle(double x, double y, double width, double height);
  void RoundedRectangle(double x, double y, double width, double height, double radius = 20.0);
  void Ellipse(double x, double y, double width, double height);
  void Lines(int n, wxPoint points[], double xoffset = 0, double yoffset = 0);

  void Translate(double x, double y);
  void Scale(double x, double y);
  void Rotate(double a);

  void Reverse(int start_cmd = 0, Bool start_with_line = 0);

  void AddPath(wxPath *p);
  
  void Install(long target, double dx, double dy,
	       double ox, double oy, double sx, double sy, 
	       Bool align, double pox, double poy);
  void InstallPS(wxPostScriptDC *dc, wxPSStream *s, double dx, double dy);
  int ToPolygons(int **_lens, double ***_pts, double sx, double sy);

  void BoundingBox(double *x1, double *y1, double *x2, double *y2);

 private:
  void MakeRoom(int n);
  void ClearCache();
};

#endif
