/*
 * File:	wb_gdi.h
 * Purpose:	Declaration of various graphics objects - fonts, pens, icons etc.
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wxb_gdih
#define wxb_gdih

#include "wx_obj.h"
#include "wx_list.h"
#include "wx_setup.h"

// Standard cursors
typedef enum {
 wxCURSOR_ARROW =  1,
 wxCURSOR_BULLSEYE,
 wxCURSOR_CHAR,
 wxCURSOR_CROSS,
 wxCURSOR_HAND,
 wxCURSOR_IBEAM,
 wxCURSOR_LEFT_BUTTON,
 wxCURSOR_MAGNIFIER,
 wxCURSOR_MIDDLE_BUTTON,
 wxCURSOR_NO_ENTRY,
 wxCURSOR_PAINT_BRUSH,
 wxCURSOR_PENCIL,
 wxCURSOR_POINT_LEFT,
 wxCURSOR_POINT_RIGHT,
 wxCURSOR_QUESTION_ARROW,
 wxCURSOR_RIGHT_BUTTON,
 wxCURSOR_SIZENESW,
 wxCURSOR_SIZENS,
 wxCURSOR_SIZENWSE,
 wxCURSOR_SIZEWE,
 wxCURSOR_SIZING,
 wxCURSOR_SPRAYCAN,
 wxCURSOR_WAIT,
 wxCURSOR_WATCH,
 wxCURSOR_BLANK
} _standard_cursors_t;

typedef    DWORD  wxDash ;

// Font
class wxFont;
class wxbFont: public wxObject
{
 protected:
  Bool temporary;   // If TRUE, the pointer to the actual font
                    // is temporary and SHOULD NOT BE DELETED by
                    // destructor
  int point_size;
  int family;
  int fontid;
  int style;
  int weight;
  int smoothing;
  Bool underlined;
  Bool size_in_pixels;
  double rotation;
 public:
  wxbFont(void);
  wxbFont(int PointSize, int FamilyOrFontId, int Style, int Weight, 
	  Bool underline = FALSE, int smoothing = wxSMOOTHING_DEFAULT,
	  Bool sip = FALSE, double Rotation = 0.0);
  ~wxbFont();

  inline int GetPointSize(void) { return point_size; }
  inline int GetFamily(void) { return family; }
  char *GetFaceString(void);
  inline int GetFontId(void) { return fontid; }
  inline int GetStyle(void) { return style; }
  inline int GetWeight(void) { return weight; }
  inline Bool GetUnderlined(void) { return underlined; }
  inline int GetSmoothing(void) { return smoothing; }
  inline int GetSizeInPixels(void) { return size_in_pixels; }
  inline int CanRotate(void) { return TRUE; }
  inline double GetRotation(void) { return rotation; }
};

#include "../../../wxcommon/FontDirectory.h"

// Colour
class wxColour: public wxObject
{
 private:
  short isInit;
  short locked;
  unsigned char red;
  unsigned char blue;
  unsigned char green;
 public:
  COLORREF pixel ;

  wxColour(void);
  wxColour(const unsigned char r, const unsigned char b, const unsigned char g);
  wxColour(const wxColour* col);
  wxColour(const char *col);
  ~wxColour(void) ;
  wxColour* CopyFrom(const wxColour* src) ;
  wxColour* CopyFrom(const char *src) ;
#ifndef MZ_PRECISE_GC
  wxColour& operator=(const wxColour & src) ;
#endif
  inline int Ok(void) { return (isInit) ; }

  void Set(unsigned char r, unsigned char b, unsigned char g);
  void Get(unsigned char *r, unsigned char *b, unsigned char *g);

  inline unsigned char Red(void) { return red; }
  inline unsigned char Green(void) { return green; }
  inline unsigned char Blue(void) { return blue; }

  inline Bool IsMutable(void) { return !locked; }
  inline void Lock(int d) { locked += d; }
};

#define wxColor wxColour

class wxColourMap;


// Point
class wxPoint: public wxObject
{
 public:
  double x;
  double y;
  wxPoint(void);
  wxPoint(double the_x, double the_y);
  ~wxPoint(void);
};

class wxIntPoint: public wxObject
{
 public:
  int x;
  int y;
  wxIntPoint(void);
  wxIntPoint(int the_x, int the_y);
  ~wxIntPoint(void);
};

// Pen
class wxPen;
class wxBitmap;
class wxbPen: public wxObject
{
 protected:
  double width;
  short locked;
  short style;
  int join ;
  int cap ;
  wxBitmap *stipple ;
 public:
  int nb_dash ;
  wxDash *dash ;

  wxColour *colour;

  wxbPen(void);
  wxbPen(wxColour *col, double width, int style);
  wxbPen(const char *col, double width, int style);
  ~wxbPen(void);

  void SetColour(wxColour *col) ;
  void SetColour(const char *col)  ;
  void SetColour(char r, char g, char b)  ;

  void SetWidth(double width)  ;
  void SetStyle(int style)  ;
  void SetStipple(wxBitmap *stipple)  ;
  void SetDashes(int nb_dashes, wxDash *dash)  ;
  void SetJoin(int join)  ;
  void SetCap(int cap)  ;

  wxColour* GetColour(void);
  int GetWidth(void);
  double GetWidthF(void);
  int GetStyle(void);
  int GetJoin(void);
  int GetCap(void);
  int GetDashes(wxDash **dash);
  wxBitmap *GetStipple(void);

  inline Bool IsMutable(void) { return !locked; }
  inline void Lock(int d) { locked += d; }
};

// Brush
class wxBrush;
class wxbBrush: public wxObject
{
 protected:
  short locked;
  short style;
  wxBitmap *stipple ;
 public:
  wxColour *colour;
  wxbBrush(void);
  wxbBrush(wxColour *col, int style);
  wxbBrush(char *col, int style);
  ~wxbBrush(void);

  void SetColour(wxColour *col)  ;
  void SetColour(const char *col)  ;
  void SetColour(char r, char g, char b)  ;
  void SetStyle(int style)  ;
  void SetStipple(wxBitmap* stipple=NULL)  ;

  wxColour *GetColour(void);
  int GetStyle(void);
  wxBitmap *GetStipple(void);

  inline Bool IsMutable(void) { return !locked; }
  inline void Lock(int d) { locked += d; }
};

/*
 * Bitmap flags
 */

// Hint to discard colourmap if one is loaded
#define wxBITMAP_DISCARD_COLOURMAP      1

// Hint to indicate filetype
#define wxBITMAP_TYPE_BMP               0x2
#define wxBITMAP_TYPE_BMP_RESOURCE      0x4
#define wxBITMAP_TYPE_ICO               0x8
#define wxBITMAP_TYPE_ICO_RESOURCE      0x10
#define wxBITMAP_TYPE_CUR               0x20
#define wxBITMAP_TYPE_CUR_RESOURCE      0x40
#define wxBITMAP_TYPE_XBM               0x80
#define wxBITMAP_TYPE_XBM_DATA          0x100
#define wxBITMAP_TYPE_XPM               0x200
#define wxBITMAP_TYPE_XPM_DATA          0x400
#define wxBITMAP_TYPE_TIF               0x800
#define wxBITMAP_TYPE_GIF               0x1000
#define wxBITMAP_TYPE_JPEG              0x4000
#define wxBITMAP_TYPE_PNG               0x8000
#define wxBITMAP_TYPE_ANY               0x2000
#define wxBITMAP_TYPE_MASK              0x10000

#define wxBITMAP_TYPE_RESOURCE wxBITMAP_TYPE_BMP_RESOURCE

class wxBitmap;
class wxCursor;

// Management of pens, brushes and fonts
class wxPenList: public wxObject
{
  wxChildList *list;
 public:
  wxPenList(void);
  ~wxPenList(void);
  void AddPen(wxPen *pen);
  wxPen *FindOrCreatePen(wxColour *colour, double width, int style, int cap = wxCAP_ROUND, int join = wxJOIN_ROUND);
  wxPen *FindOrCreatePen(char *colour, double width, int style, int cap = wxCAP_ROUND, int join = wxJOIN_ROUND);
};

class wxBrushList: public wxObject
{
  wxChildList *list;
 public:
  wxBrushList(void);
  ~wxBrushList(void);
  void AddBrush(wxBrush *brush);
  wxBrush *FindOrCreateBrush(wxColour *colour, int style);
  wxBrush *FindOrCreateBrush(char *colour, int style);
};

class wxFontList: public wxObject
{
  wxChildList *list;
 public:
  wxFontList(void);
  ~wxFontList(void);
  void AddFont(wxFont *font);
  wxFont *FindOrCreateFont(int PointSize, int FamilyOrFontId, int Style, int Weight, 
			   Bool underline = FALSE, int smoothing = wxSMOOTHING_DEFAULT,
			   Bool sip = FALSE, double rotation = 0.0);
  wxFont *FindOrCreateFont(int PointSize, const char *Face, int Family, int Style, 
			   int Weight, Bool underline = FALSE, int smoothing = wxSMOOTHING_DEFAULT,
			   Bool sip = FALSE, double rotation = 0.0); 
};

class wxColourDatabase: public wxList
{
 public:
  wxColourDatabase(int type);
  ~wxColourDatabase(void) ;
  wxColour *FindColour(const char *colour);
  char *FindName(wxColour *colour);
  void Initialize(void);
};

// Lists of GDI objects
extern wxPenList   *wxThePenList;
extern wxBrushList *wxTheBrushList;
extern wxFontList   *wxTheFontList;

// Stock objects
extern wxFont *wxNORMAL_FONT;

extern wxPen *wxBLACK_PEN;

extern wxBrush *wxWHITE_BRUSH;
extern wxBrush *wxBLACK_BRUSH;

extern wxColour *wxBLACK;
extern wxColour *wxWHITE;

// Stock cursors types
extern wxCursor *wxSTANDARD_CURSOR;
extern wxCursor *wxHOURGLASS_CURSOR;
extern wxCursor *wxCROSS_CURSOR;
extern wxCursor *wxIBEAM_CURSOR;

extern wxColourDatabase *wxTheColourDatabase;
extern void wxInitializeStockObjects(void);
extern void wxDeleteStockObjects(void);

extern Bool wxColourDisplay(void);

// Returns depth of screen
extern int wxDisplayDepth(void);

extern void wxDisplaySize(int *width, int *height, int flags = 0);
extern void wxDisplayOrigin(int *x, int *y, int flags = 0);

extern void wxSetCursor(wxCursor *cursor);

#endif // wxb_gdih
