/*
 * File:	wx_dcps.h
 * Purpose:	PostScript device context
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wx_dcpsh
#define wx_dcpsh

#ifdef __GNUG__
# ifndef wx_mac
#  pragma interface
# endif
#endif

#ifdef wx_xt
class wxBitmap;
class wxBrush;
class wxColour;
class wxColourMap;
class wxFont;
class wxList;
class wxPen;
class ofstream;
#else
#include "wx_dc.h"
#endif

class wxMemoryDC;
class wxPath;
#ifdef wx_mac
class wxPrintData;
#endif

#if USE_POSTSCRIPT

#ifdef IN_CPROTO
typedef       void    *wxPostScriptDC ;
#else

#ifdef wx_xt
# define DRAW_TEXT_CONST /* empty */
#else
# define DRAW_TEXT_CONST const
#endif

class wxPSStream;

class wxPostScriptDC: public wxDC
{
 public:
#ifdef wx_xt
  char *title;
#endif
  int page_number;
  wxPSStream *pstream;    // PostScript output stream
  char *filename;
  long boundingboxpos, fontlistpos;
  unsigned char currentRed;
  unsigned char currentGreen;
  unsigned char currentBlue;
  double clipx, clipy, clipw, cliph;

  double max_x, max_y, min_x, min_y;

  char *current_font_name, *next_font_name;
  double current_font_size, next_font_size;

  double paper_x, paper_y, paper_w, paper_h, paper_x_scale, paper_y_scale;
  double paper_margin_x, paper_margin_y;
  Bool landscape, resetFont, level2ok;
  char *afm_path;

  int mode, use_paper_bbox, as_eps;
  char *preview_cmd, *print_cmd, *print_opts;

  void *used_fonts;

  // Create a printer DC
  wxPostScriptDC(Bool interactive = TRUE, wxWindow *parent = NULL, Bool usePaperBBox = FALSE, Bool asEPS = TRUE);

  ~wxPostScriptDC(void);

  Bool Create(Bool interactive = TRUE, wxWindow *parent = NULL, Bool usePaperBBox = FALSE, Bool asEPS = TRUE);

  Bool PrinterDialog(Bool interactive, wxWindow *parent, Bool usePaperBBox);

  inline virtual void BeginDrawing(void) {} ;
  inline virtual void EndDrawing(void) {} ;

  Bool GetPixel(double x1, double y1, wxColour *col) ;

  void DrawLine(double x1, double y1, double x2, double y2);
  void DrawArc(double x1,double y1,double w,double h,double start,double end);
  void DrawPoint(double x, double y);
  void DrawPoint(wxPoint* point) { DrawPoint(point->x, point->y); }
  void DrawLines(int n, wxPoint points[], double xoffset = 0, double yoffset = 0);
  void DrawPolygon(int n, wxPoint points[], double xoffset = 0, double yoffset = 0, int fillStyle=wxODDEVEN_RULE);
  void  DrawPath(wxPath *p, double dx, double dy, int fillStyle=wxODDEVEN_RULE);

  void DrawSpline(double x1, double y1, double x2, double y2, double x3, double y3);

  void DrawRectangle(double x, double y, double width, double height);
  void DrawRoundedRectangle(double x, double y, double width, double height, double radius = 20);
  void DrawEllipse(double x, double y, double width, double height);
  void DrawText(DRAW_TEXT_CONST char *text, double x, double y, 
		Bool combine = FALSE, Bool use16 = FALSE, 
		int dt = 0, double angle = 0.0);

  void Clear(void);
  void SetFont(wxFont *font);
  void SetPen(wxPen *pen);
  void SetBrush(wxBrush *brush);
  void SetBackground(wxColour *c);
  void SetClippingRect(double x, double y, double width, double height);
  wxRegion *GetClippingRegion();
  void SetClippingRegion(wxRegion *r);
  void DestroyClippingRegion(void);

  Bool StartDoc(char *message);
  void EndDoc(void);
  void StartPage(void);
  void EndPage(void);

  double GetCharHeight(void);
  double GetCharWidth(void);
  void GetTextExtent(const char *string, double *x, double *y,
                     double *descent = NULL, double *externalLeading = NULL, 
		     wxFont *theFont = NULL, 
		     Bool combine = FALSE, Bool use16 = FALSE, int dt = 0, int slen = -1);
  void SetMapMode(int mode);
  void SetUserScale(double x, double y);
  double DeviceToLogicalX(int x);
  double DeviceToLogicalY(int y);
  double DeviceToLogicalXRel(int x);
  double DeviceToLogicalYRel(int y);
  double UnscrolledDeviceToLogicalX(int x);
  double UnscrolledDeviceToLogicalY(int y);
  int LogicalToDeviceX(double x);
  int LogicalToDeviceY(double y);
  int LogicalToDeviceXRel(double x);
  int LogicalToDeviceYRel(double y);
  int LogicalToUnscrolledDeviceX(double x);
  int LogicalToUnscrolledDeviceY(double y);
  double FLogicalToDeviceX(double x);
  double FLogicalToDeviceY(double y);
  double FLogicalToDeviceXRel(double x);
  double FLogicalToDeviceYRel(double y);
  double FLogicalToUnscrolledDeviceX(double x);
  double FLogicalToUnscrolledDeviceY(double y);

  double FsLogicalToDeviceX(double x, double o, double s);
  double FsLogicalToDeviceY(double y, double o, double s);
  double FsLogicalToDeviceXRel(double x, double o, double s);
  double FsLogicalToDeviceYRel(double y, double o, double s);

  Bool Blit(double xdest, double ydest, double width, double height,
            wxBitmap *source, double xsrc, double ysrc, int rop = wxSOLID, wxColour *c = NULL, wxBitmap *mask=NULL);
  Bool Blit(double xdest, double ydest, double width, double height,
            wxMemoryDC *source, double xsrc, double ysrc, int rop = wxSOLID, wxColour *c = NULL, wxMemoryDC *mask=NULL);
  inline Bool CanGetTextExtent(void) { return USE_AFM_FOR_POSTSCRIPT; }
  inline Bool CanDrawBitmap(void) { return TRUE; }

  void GetSize(double *width, double *height);
  void GetSizeMM(double *width, double *height);

  inline void SetColourMap(wxColourMap *WXUNUSED(cmap)) {}

  void SetBackgroundMode(int mode);
  void SetTextBackground(wxColour *col);
  void SetTextForeground(wxColour *col);
  void TryColour(wxColour *src, wxColour *dest);

  Bool GlyphAvailable(int c, wxFont *f = NULL);

  void CalcBoundingBoxClip(double x, double y);

  void SetAntiAlias(int mode);

  virtual int CacheFontMetricsKey();

  virtual Bool Ok() { return ok; }
};

#ifndef wx_xt

// Print Orientation (Should also add Left, Right)
enum {
  PS_PORTRAIT,
  PS_LANDSCAPE
};// ps_orientation = PS_PORTRAIT;

// Print Actions
enum {
  PS_PRINTER,
  PS_FILE,
  PS_PREVIEW
};// ps_action = PS_PREVIEW;

#endif

extern void wxInitializePrintSetupData(Bool init = TRUE);

class wxPrintSetupData : public wxObject {
public:
    wxPrintSetupData(void);
    ~wxPrintSetupData(void);

    void copy (wxPrintSetupData* data);

    void  SetPrinterCommand(char *cmd);
    void  SetPaperName(char *paper);
    void  SetPrintPreviewCommand(char *cmd);
    void  SetPrinterOptions(char *flags);
    void  SetPrinterFile(char *f);
    void  SetAFMPath(char *f);
    void  SetPrinterMode(int mode);
    void  SetPrinterOrientation(int orient);
    void  SetPrinterScaling(double x, double y)
	{ printer_scale_x = x; printer_scale_y = y; }
    void  SetPrinterTranslation(double x, double y)
	{ printer_translate_x = x; printer_translate_y = y; }
    void  SetColour(Bool col)
	{ print_colour = col; }
    void  SetLevel2(Bool l2)
	{ print_level_2 = l2; }
    void SetEditorMargin(long x, long y)
        { emargin_h = x; emargin_v = y; }
    void SetMargin(double x, double y)
        { ps_margin_h = x; ps_margin_v = y; }

    inline char *GetPrinterCommand(void)
	{ return printer_command; }
    inline char *GetPrintPreviewCommand(void)
	{ return preview_command; }
    inline char *GetPrinterOptions(void)
	{ return printer_flags; }
    inline char *GetPrinterFile(void)
	{ return printer_file; }
    inline char *GetPaperName(void)
	{ return paper_name; }
    inline int GetPrinterOrientation(void)
	{  return printer_orient; }
    inline void GetPrinterScaling(double *x, double *y)
	{ *x=printer_scale_x; *y=printer_scale_y; }
    inline void GetPrinterTranslation(double *x, double *y)
	{ *x=printer_translate_x; *y=printer_translate_y; }
    inline int GetPrinterMode(void)
	{ return printer_mode; }
    inline char *GetAFMPath(void)
	{ return afm_path; }
    inline Bool GetColour(void)
	{ return print_colour; }
    inline Bool GetLevel2()
	{ return print_level_2; }
    void GetEditorMargin(long *x, long *y)
        { *x = emargin_h; *y = emargin_v; }
    void GetMargin(double *x, double *y)
        { *x = ps_margin_h; *y = ps_margin_v; }

    Bool CanShowNative();
    Bool ShowNative(wxWindow *parent);

private:
    friend class wxPostScriptDC;

    char   *printer_command;
    char   *preview_command;
    char   *printer_flags;
    char   *printer_file;
    int    printer_orient;
    double  printer_scale_x;
    double  printer_scale_y;
    double  printer_translate_x;
    double  printer_translate_y;
    int    printer_mode;
    char   *afm_path;
    char   *paper_name;
    Bool   print_colour;
    Bool   print_level_2;
    long   emargin_h, emargin_v;
    double  ps_margin_h, ps_margin_v;
#ifdef wx_mac
 public:
    wxPrintData *native;
#endif
};

extern wxPrintSetupData *wxGetThePrintSetupData();
extern void wxSetThePrintSetupData(wxPrintSetupData *);

class wxPrintPaperType : public wxObject {
public:
    wxPrintPaperType(char *name=NULL, int wmm=0, int hmm=0, int wp=0, int hp=0);
    ~wxPrintPaperType(void);
public:
    int   widthMM;
    int   heightMM;
    int   widthPixels;
    int   heightPixels;
    char  *pageName;
};

class wxPrintPaperDatabase : public wxList {
public:
    wxPrintPaperDatabase(void);
    ~wxPrintPaperDatabase(void);

    void CreateDatabase(void);
    void ClearDatabase(void);

    void AddPaperType(char *name, int wmm, int hmm, int wp, int hp);
    wxPrintPaperType *FindPaperType(char *name);
};

extern wxPrintPaperDatabase *wxThePrintPaperDatabase;

#endif // IN_CPROTO
#endif // USE_POSTSCRIPT
#endif // wx_dcpsh
