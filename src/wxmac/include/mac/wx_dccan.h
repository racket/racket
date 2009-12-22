///////////////////////////////////////////////////////////////////////////////
// File:	wx_dccan.h
// Purpose:	Canvas device context declaration (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_dccanh
#define wx_dccanh

#include "wx_gdi.h"
#include "wb_dccan.h"

#ifdef IN_CPROTO
typedef       void* wxCanvasDC ;
#else

class wxCanvas;
class wxGLConfig;

class wxCanvasDC: public wxbCanvasDC
{
 protected:
  Bool cMacDoingDrawing;  // mac platform only; internal use only; used by Begin/EndDrawing
 public:
  wxCanvas* canvas;
  wxGL *gl;
  wxGLConfig *gl_cfg;
  // Every time a callback happens, these are set to point to the right values
  // for drawing calls to work

  int dc_set_depth;
  int paint_brush_with_erase; // used for wxPANEL_PATTERN brushes

  int pixmapWidth;
  int pixmapHeight;

  RgnHandle current_reg, onpaint_reg, clip_reg;

  int current_pen_join;
  int current_pen_cap;
  int current_pen_nb_dash;
  char* current_pen_dash;
  wxBitmap* current_stipple;

  wxRegion *clippingCached;

  wxBitmap* selected_pixmap;

  void *fast_pb;
  long fast_rb;

  int gdx, gdy;
  wxCanvasDC *chain_next, *chain_prev;

  wxCanvasDC(void);
  wxCanvasDC(wxCanvas* canvas); // Create a DC corresponding to a canvas

  void Init(wxCanvas*);

  ~wxCanvasDC(void);

  void SetCanvasClipping() ;
  void GetClippingBox(double* x,double* y,double* w,double* h) ;

  virtual void BeginDrawing(void);
  virtual void EndDrawing(void);
  
  void SetCurrentDC(Bool cgok = FALSE);
  void ReleaseCurrentDC(void);

  CGContextRef GetCG();

  Bool GetPixel(double x1, double y1, wxColour* col) ;

  void SetPixel(double x1, double y1, wxColour* col) ;
  inline void BeginSetPixel() {}
  inline void EndSetPixel() {}

  Bool BeginSetPixelFast(int x, int y, int w, int h);
  void EndSetPixelFast();
  void SetPixelFast(int i, int j, int r, int g, int b);
  void SetPixelCore(int i, int j, int r, int g, int b);

  Bool BeginGetPixelFast(int x, int y, int w, int h);
  void EndGetPixelFast();
  void GetPixelFast(int x, int y, int *r, int *g, int *b);

  void DrawLine(double x1, double y1, double x2, double y2);
  void DrawArc(double x1,double y1,double x2,double y2,double xc,double yc);
  void DrawPoint(double x, double y);
  void DrawLines(int n, wxPoint points[], double xoffset = 0, double yoffset = 0);
  void DrawPolygon(int n, wxPoint points[], double xoffset = 0, double yoffset = 0,
  					int fillStyle=wxODDEVEN_RULE);
  void DrawPath(wxPath *p, double xoffset = 0, double yoffset = 0, int fillStyle=wxODDEVEN_RULE);
  void DrawRectangle(double x, double y, double width, double height);
  void DrawRoundedRectangle(double x, double y, double width, double height, double radius = 20);
  void DrawEllipse(double x, double y, double width, double height);
  void DrawText(const char* text, double x, double y, 
		Bool combine = FALSE, Bool use16 = FALSE, 
		int d = 0, double angle = 0.0);

  void GetSize(double *width, double *height);

  void Clear(void);
  void SetFont(wxFont* font);
  void SetPen(wxPen* pen);
  void SetBrush(wxBrush* brush);
  void SetLogicalFunction(int function);
  void SetBackground(wxColour* c);
  virtual void SetPaintRegion(Rect* paintRect); // mac platform only
  void SetClippingRect(double x, double y, double width, double height);
  wxRegion* GetClippingRegion();
  void SetClippingRegion(wxRegion*);
  void SetGrafPtrOffsets(int dx, int dy);

  double GetCharHeight(void);
  double GetCharWidth(void);
  virtual void GetTextExtent(const char* string, double* x, double* y, double* descent = NULL,
			     double* externalLeading = NULL, wxFont* the_font = NULL, 
			     Bool combine = FALSE, Bool use16 = FALSE,
			     int d = 0, int len = -1);
  Bool StartDoc(char* message);
  void EndDoc(void);
  void StartPage(void);
  void EndPage(void);
  void SetMapMode(int mode);
  void SetUserScale(double x, double y);
  void SetDeviceOrigin(double x, double y);
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

  Bool Blit(double xdest, double ydest, double width, double height,
            wxBitmap* source, double xsrc, double ysrc, int rop = wxSOLID, wxColour *c = NULL,
            wxBitmap* mask = NULL);
  Bool GCBlit(double xdest, double ydest, double width, double height,
            wxBitmap* source, double xsrc, double ysrc);

  void wxMacSetClip(void); // Internal only
  void wxMacSetCurrentTool(wxMacToolType whichTool); // Internal only

  void TryColour(wxColour *src, wxColour *dest);

  Bool GlyphAvailable(int c, wxFont *f = NULL);
  
  void InstallColor(wxColour *c, int fg);
  void InstallLogicalFunction(int func);
  
  wxRegion *BrushStipple();
  void PaintStipple(wxRegion *);

  wxGL *GetGL();

  void ResetBackground();

  void SetAntiAlias(int v);

  Bool AlignSmoothing();
  double GetPenSmoothingOffset();
  double SmoothingXFormX(double x);
  double SmoothingXFormY(double y);
  double SmoothingXFormW(double w, double x);
  double SmoothingXFormH(double h, double y);
  double SmoothingXFormXB(double x);
  double SmoothingXFormYB(double y);
  double SmoothingXFormWL(double w, double x);
  double SmoothingXFormHL(double h, double y);

  void DrawTabBase(double x, double y, double w, double h, int state);
  void DrawTab(char *str, double x, double y, double w, double h, int state);

  virtual void SetAlpha(double d);

  virtual int CacheFontMetricsKey();
};

long wxTextFontInfo(int font, int size, int face, FontInfo *finfo, char *str, int d = 0, int len = -1);
double wxDrawUnicodeText(const char *text, int d, int len = -1, int ucs4 = FALSE, Bool qd_spacing = FALSE, 
			 int smoothing = wxSMOOTHING_DEFAULT, double angle = 0.0,
			 double scale_x = 1.0, double scale_y = 1.0,
			 int use_start = 0, double start_x = 0.0, double start_y = 0.0,
			 double device_dx = 0.0, double device_dy = 0.0,
			 int is_sym = 0, double current_alpha = 1.0);
void wxGetUnicodeTextWidth(const char *text, int d, int theStrlen, 
			   short txFont, short txSize, short txFace,
			   int ucs4, double scale_y,
			   double* x, double* y,
			   double* descent, double* externalLeading,
			   Bool qd_spacing, double scale_x = 1.0,
			   int is_sym = 0);
Bool wxGetUnicodeGlyphAvailable(int c, 
				short txFont, short txSize, short txFace,
				int is_sym);

extern Pattern wx_white_pat, wx_black_pat, wx_light_gray_pat, wx_dark_gray_pat;
#define GetWhitePattern() &wx_white_pat
#define GetBlackPattern() &wx_black_pat
#define GetLightGrayPattern() &wx_light_gray_pat
#define GetDarkGrayPattern() &wx_dark_gray_pat
extern void wx_init_patterns();
extern void wxResetCanvasBackgrounds();

class wxGL : public wxObject {
public:
  wxGL();

  long gl_ctx; /* really an AGLContext */

  int Ok();
  
  void Reset(wxGLConfig *cfg, CGrafPtr gp, int offscreen, int w, int h);
  
  void SwapBuffers(void);
  void ThisContextCurrent(void);
  
  void ResetGLView(int x, int y, int w, int h);
};

void wxInitGL();

#endif // IN_CPROTO
#endif // wx_dccanh

