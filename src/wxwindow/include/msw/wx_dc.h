/*
 * File:	wx_dc.h
 * Purpose:	wxDC device context declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */


#ifndef wx_dch
#define wx_dch

#include "wx_gdi.h"
#include "wb_dc.h"

#include "wx_pltgdi.h"

class wxRegion;
class wxGL;
class wxGLConfig;
class wxPath;

// Since Windows handles DCs quite uniformly, we can have
// a general wxWindowsDC, and derive canvas and printer DCs from this
// with minimum extra functionality.
class wxDC: public wxbDC
{
 public:
  Bool dont_delete;
  Bool screen_font;
  int combine_status;
  int window_ext_x;
  int window_ext_y;

  int canvas_scroll_dx, canvas_scroll_dy;

  wxCanvas *canvas;
  wxBitmap *selected_bitmap;
  char *filename;

  HDC cdc;

  HDC      cur_dc;
  COLORREF cur_bk;
  int cur_rop;

  Graphics *g;

  // Store all old GDI objects when do a SelectObject,
  // so we can select them back in (thus unselecting user
  // objects) so we can safely delete the DC.
  HBITMAP old_bitmap;
  HPEN    old_pen;
  HBRUSH  old_brush;
  HFONT   old_font;
  HPALETTE old_palette;

  wxGL *wx_gl;
  wxGLConfig *wx_gl_cfg;

  wxDC(void);
  ~wxDC(void);

  virtual void BeginDrawing(void);
  virtual void EndDrawing(void);

  void FloodFill(double x1, double y1, wxColour *col, int style=wxFLOOD_SURFACE);
  Bool GetPixel(double x1, double y1, wxColour *col);
  void SetPixel(double x1, double y1, wxColour *col);

  inline void BeginSetPixel() {}
  inline void EndSetPixel() {}

  Bool BeginSetPixelFast(int x, int y, int w, int h);
  void EndSetPixelFast();
  void SetPixelFast(int x1, int y1, int r, int g, int b);

  Bool BeginGetPixelFast(int x, int y, int w, int h);
  void EndGetPixelFast();
  void GetPixelFast(int x, int y, int *r, int *g, int *b);

  void DrawLine(double x1, double y1, double x2, double y2);
  void DrawArc(double x1,double y1,double x2,double y2,double xc,double yc);
  void DrawPoint(double x, double y);

  void DrawLines(int n, wxPoint points[], double xoffset = 0, double yoffset = 0);
  // MS C7 complains if this overloaded function isn't explicitly mentioned
  inline void DrawLines(wxList *list, double xoffset = 0, double yoffset = 0)
  { wxbDC::DrawLines(list, xoffset, yoffset); }

  void DrawPolygon(int n, wxPoint points[], double xoffset = 0, double yoffset = 0, int fillStyle=wxODDEVEN_RULE);
  // See MS C7 comment above
  inline void DrawPolygon(wxList *list, double xoffset = 0, double yoffset = 0, int fillStyle=wxODDEVEN_RULE)
  { wxbDC::DrawPolygon(list, xoffset, yoffset,fillStyle); }

  void DrawPath(wxPath *p, double xoffset = 0, double yoffset = 0, int fillStyle=wxODDEVEN_RULE);

  void DrawRectangle(double x, double y, double width, double height);
  void DrawRoundedRectangle(double x, double y, double width, double height, double radius = 20.0);
  void DrawEllipse(double x, double y, double width, double height);

  void Clear(void);
  void SetFont(wxFont *font);
  void SetPen(wxPen *pen);
  void SetBrush(wxBrush *brush);
  void SetBackground(wxColour *c);
  void SetBackgroundMode(int mode);
  void SetClippingRect(double x, double y, double width, double height);
  wxRegion* GetClippingRegion();
  void SetClippingRegion(wxRegion*);
  void SetColourMap(wxColourMap *cmap);
  void DrawText(const char *text, double x, double y, 
		Bool combine = FALSE, Bool use16bit = FALSE, 
		int d = 0, double angle = 0.0);

  double GetCharHeight(void);
  double GetCharWidth(void);
  void GetTextExtent(const char *string, double *x, double *y,
                     double *descent = NULL, double *externalLeading = NULL, 
		     wxFont *theFont = NULL, 
		     Bool combine = FALSE, Bool use16bit = FALSE, int d = 0);
  void GetSize(double *width, double *height);
  void GetSizeMM(double *width, double *height);
  Bool StartDoc(char *message);
  void EndDoc(void);
  void StartPage(void);
  void EndPage(void);
  void SetMapMode(int mode, HDC dc = 0);
  void SetScaleMode(int mode, HDC dc = 0);
  void ResetMapMode(HDC dc = 0);
  void SetUserScale(double x, double y);
  void SetDeviceOrigin(double x, double y);
  double DeviceToLogicalX(int x);
  double DeviceToLogicalY(int y);
  double DeviceToLogicalXRel(int x);
  double DeviceToLogicalYRel(int y);
  int LogicalToDeviceX(double x);
  int LogicalToDeviceY(double y);
  int LogicalToDeviceXRel(double x);
  int LogicalToDeviceYRel(double y);
  double FLogicalToDeviceX(double x);
  double FLogicalToDeviceY(double y);
  double FLogicalToDeviceXRel(double x);
  double FLogicalToDeviceYRel(double y);

  Bool GlyphAvailable(int c, wxFont *f = NULL);

  Bool Blit(double xdest, double ydest, double width, double height,
            wxBitmap *source, double xsrc, double ysrc, int rop = wxSOLID,
	    wxColour *c=NULL, wxBitmap *mask=NULL);
            
  Bool CanDrawBitmap(void);
  Bool CanGetTextExtent(void);

  void SetRop(HDC cdc, int mode);
  void DoClipping(HDC cdc);
  void SelectOldObjects(HDC dc);
   HDC ThisDC();
   void DoneDC(HDC dc);
   void ShiftXY(double x, double y, int *ix, int *iy);

  Bool StartBrush(HDC dc, Bool no_stipple = FALSE);
  Bool StartPen(HDC dc);
  void DoneBrush(HDC dc);
  void DonePen(HDC dc);

  wxBitmap *StippleBrush();				   

  virtual wxGL *GetGL();

  void OnCalcScroll(void);

  void InitGraphics(HDC dc);
  void ReleaseGraphics(HDC given_dc = 0);

  void SetAntiAlias(Bool v);

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
};

// This class specific to Windows 3.1
class wxPrinterDC: public wxDC
{
 public:
  // Create a printer DC
  wxPrinterDC(wxWindow *parent = NULL, 
	      char *driver = NULL, char *device = NULL, char *output = NULL, Bool interactive = TRUE);
  wxPrinterDC(HDC theDC);

  ~wxPrinterDC(void);
};

// Gets an HDC for the default printer configuration
HDC wxGetPrinterDC(void);

/*
 * Have the same macros as for XView but not for every operation:
 * just for calculating window/viewport extent (a better way of scaling).
 */

// Logical to device
// Absolute
#define MS_XLOG2DEV(x) ((int)floor((x)*logical_scale_x*user_scale_x + device_origin_x + canvas_scroll_dx))
#define MS_YLOG2DEV(y) ((int)floor((y)*logical_scale_y*user_scale_y + device_origin_y + canvas_scroll_dy))

// Logical to device
#define XLOG2DEV(x) MS_XLOG2DEV(x)
#define YLOG2DEV(y) MS_YLOG2DEV(y)

// Relative
#define MS_XLOG2DEVREL(x) ((int)floor((x)*logical_scale_x*user_scale_x))
#define MS_YLOG2DEVREL(y) ((int)floor((y)*logical_scale_y*user_scale_y))

// Device to logical
// Absolute
#define MS_XDEV2LOG(x) (((x) - device_origin_x - canvas_scroll_dx)/(logical_scale_x*user_scale_x))
#define MS_YDEV2LOG(y) (((y) - device_origin_y - canvas_scroll_dy)/(logical_scale_y*user_scale_y))

// Relative
#define MS_XDEV2LOGREL(x) ((x)/(logical_scale_x*user_scale_x))
#define MS_YDEV2LOGREL(y) ((y)/(logical_scale_y*user_scale_y))


class wxGL : public wxObject {
public:
  wxGL();
  virtual ~wxGL();

  virtual int Ok() = 0;

  virtual void Reset(wxGLConfig *cfg, HDC dc, int offscreen) = 0;

  virtual void SwapBuffers(void) = 0;
  virtual void ThisContextCurrent(void) = 0;
};

#endif // wx_dc.h
