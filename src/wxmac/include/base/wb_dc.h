/*
 * File:	wb_dc.h
 * Purpose:	wxDC device context declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wxb_dch
#define wxb_dch

#include "common.h"
#include "wx_frame.h"
#include "wx_gdi.h"

#define wxDEVICE_CANVAS  1
                            // X canvas
#define wxDEVICE_EPS     2
                            // Encapsulated PostScript on any platform
#define wxDEVICE_WINDOWS 3
                            // MS Windows device (canvas, printer)
#define wxDEVICE_PIXMAP  4
                            // X pixmap

#define MM_TEXT        1
#define MM_ISOTROPIC   2
#define MM_ANISOTROPIC 3
#define MM_LOMETRIC    4
#define MM_HIMETRIC    5
#define MM_TWIPS       6

#define MM_POINTS      7
#define MM_METRIC      8

#ifdef IN_CPROTO
typedef       void    *wxbDC ;
#else

class wxRegion;
class wxCanvas;
class wxCanvasDC;
class wxDC;
class wxPath;

class wxbDC: public wxObject
{
 public:
  int device;
  Bool ok;
  wxRegion *clipping;
  Bool wx_interactive;

  // Coordinate system variables
  double logical_origin_x;
  double logical_origin_y;

  double device_origin_x;
  double device_origin_y;
  double auto_device_origin_x;
  double auto_device_origin_y;

  double logical_scale_x;
  double logical_scale_y;

  double user_scale_x;
  double user_scale_y;

  double current_alpha;

  int mapping_mode;

  double min_x;          // bounding box
  double min_y;
  double max_x;
  double max_y;
  char *title;

  Bool Colour;
  int anti_alias;

  int current_bk_mode;

  wxPen *current_pen;
  wxBrush *current_brush;
  wxColour *current_background_color;
  wxColour *current_text_foreground;
  wxColour *current_text_background;
  wxFont *font;
  Bool autoSetting ;

  wxbDC(void);

  ~wxbDC(void);

  //
  // This function is intended to improves drawing, by avoiding to
  // repeatly call ::SetPen/::SetBrush. If set to FALSE, these functions
  // aren't called when calling ::DrawLine(),...
  // Please note that this is YOUR responsability to use it, and do it
  // only when you KNOWN that pen/brush isn't changed between 2 calls to
  // DrawLine,... !!!
  // Note also that in X, we don't test autoSetting on brushes, because they
  // modify Foreground, as pens. So, convention is:
  //   - call your SetBrush(), THEN your SetPen, THEN AutoSetTools(FALSE)
  //   - call DrawLine,...
  // [mainly coded for Windows]
  inline virtual void AutoSetTools(Bool auto_setting) { autoSetting = auto_setting ; }
 
  inline virtual void BeginDrawing(void) {} ;
  inline virtual void EndDrawing(void) {} ;

  virtual Bool GetPixel(double x1, double y1, wxColour *col) = 0;

  virtual void DrawLine(double x1, double y1, double x2, double y2) = 0;
  virtual void DrawArc(double x1,double y1,double x2,double y2,double xc,double yc)=0;
  virtual void DrawPoint(double x, double y) = 0;
  virtual void DrawLines(int n, wxPoint points[], double xoffset = 0, double yoffset = 0) = 0;
  virtual void DrawLines(wxList *list, double xoffset = 0, double yoffset = 0);
  virtual void DrawPolygon(int n, wxPoint points[], double xoffset = 0, double yoffset = 0, int fillStyle=wxODDEVEN_RULE) = 0;
  virtual void DrawPolygon(wxList *list, double xoffset = 0, double yoffset = 0, int fillStyle=wxODDEVEN_RULE);
  virtual void DrawRectangle(double x, double y, double width, double height) = 0;
  virtual void DrawRoundedRectangle(double x, double y, double width, double height, double radius = 20) = 0;
  virtual void DrawEllipse(double x, double y, double width, double height) = 0;
#if USE_SPLINES
  // Splines
  // 3-point spline
  virtual void DrawSpline(double x1, double y1, double x2, double y2, double x3, double y3);
  // Any number of control points - a list of pointers to wxPoints
  virtual void DrawSpline(wxList *points);
  virtual void DrawSpline(int n, wxPoint points[]);
#endif
  virtual void DrawPath(wxPath *p, double xoffset = 0, double yoffset = 0, int fillStyle=wxODDEVEN_RULE) = 0;
  virtual void DrawText(const char *text, double x, double y, Bool combine = FALSE, 
			Bool use16 = FALSE, int d = 0, double angle = 0.0) = 0;
  virtual void Clear(void) = 0;

  virtual Bool StartDoc(char *message) = 0;
  virtual void EndDoc(void) = 0;
  virtual void StartPage(void) = 0;
  virtual void EndPage(void) = 0;

  virtual void SetFont(wxFont *font) = 0;
  virtual void SetPen(wxPen *pen) = 0;
  virtual void SetBrush(wxBrush *brush) = 0;
  virtual void SetBackground(wxColour *c) = 0;
  virtual void SetTextForeground(wxColour *colour);
  virtual void SetTextBackground(wxColour *colour);
  virtual void SetBackgroundMode(int mode); // wxSOLID or wxTRANSPARENT
                                            // for drawing background colour
  inline int GetBackgroundMode(void) { return current_bk_mode; }

  virtual void SetClippingRect(double x, double y, double width, double height)= 0;
  virtual wxRegion* GetClippingRegion()= 0;
  virtual void SetClippingRegion(wxRegion*)= 0;
  inline virtual void SetColourMap(wxColourMap *cmap) {};

  virtual double GetCharHeight(void) = 0;
  virtual double GetCharWidth(void) = 0;
  virtual void GetTextExtent(const char* string, double* x, double* y, double* descent = NULL,
  			     double* externalLeading = NULL, wxFont* the_font = NULL, 
			     Bool combine=FALSE, Bool use16=FALSE,
                             int d = 0, int len = -1) = 0;
  inline virtual Bool Ok(void) {return ok;};
  virtual void SetMapMode(int mode) = 0;
  inline virtual int  GetMapMode(void) {return mapping_mode;};

  // The following methods provide a cleaner interface
  virtual wxColor *GetBackground(void);
  inline virtual wxBrush *GetBrush(void)           { return current_brush ;}
  inline virtual wxFont  *GetFont(void)            { return font ;}
  inline virtual wxPen   *GetPen(void)             { return current_pen ;}
  inline virtual wxColour *GetTextBackground(void)  { return current_text_background ;}
  inline virtual wxColour *GetTextForeground(void)  { return current_text_foreground ;}
 
  virtual void SetLogicalOrigin(double x, double y);
  virtual void SetDeviceOrigin(double x, double y);
  virtual void SetLogicalScale(double x, double y);
  virtual void SetUserScale(double x, double y) = 0;
  virtual double DeviceToLogicalX(int x) = 0;
  virtual double DeviceToLogicalY(int y) = 0;
  virtual double DeviceToLogicalXRel(int x) = 0;
  virtual double DeviceToLogicalYRel(int y) = 0;
  virtual double UnscrolledDeviceToLogicalX(int x) = 0;
  virtual double UnscrolledDeviceToLogicalY(int y) = 0;
  virtual int LogicalToDeviceX(double x) = 0;
  virtual int LogicalToDeviceY(double y) = 0;
  virtual int LogicalToDeviceXRel(double x) = 0;
  virtual int LogicalToDeviceYRel(double y) = 0;
  virtual int LogicalToUnscrolledDeviceX(double x) = 0;
  virtual int LogicalToUnscrolledDeviceY(double y) = 0;
  virtual double FLogicalToDeviceX(double x) = 0;
  virtual double FLogicalToDeviceY(double y) = 0;
  virtual double FLogicalToDeviceXRel(double x) = 0;
  virtual double FLogicalToDeviceYRel(double y) = 0;
  virtual double FLogicalToUnscrolledDeviceX(double x) = 0;
  virtual double FLogicalToUnscrolledDeviceY(double y) = 0;
  // Only works for PostScript *after* you've printed an image.
  // Gives width and height of image.
  virtual void GetSize(double *width, double *height);
  virtual Bool Blit(double xdest, double ydest, double width, double height,
            wxBitmap *source, double xsrc, double ysrc, int rop = wxSOLID, 
            wxColour *c = NULL, wxBitmap *mask = NULL) = 0;
            
  virtual void TryColour(wxColour *src, wxColour *dest);

  virtual Bool GlyphAvailable(int c, wxFont *f = NULL) = 0;
    
  void GetUserScale(double *xs, double *ys)
      { *xs = user_scale_x; *ys = user_scale_y; }
  void GetDeviceOrigin(double *x, double *y) 
      { *x = device_origin_x; *y = device_origin_y; }
 
  int GetAntiAlias();
  virtual void SetAntiAlias(int v);

  virtual void SetAlpha(double d);
  double GetAlpha();

  virtual int CacheFontMetricsKey();
};

// Conversion
#define METRIC_CONVERSION_CONSTANT  0.0393700787

// Scaling factors for various unit conversions
#define mm2inches (METRIC_CONVERSION_CONSTANT)
#define inches2mm (1/METRIC_CONVERSION_CONSTANT)

#define mm2twips (METRIC_CONVERSION_CONSTANT*1440)
#define twips2mm (1/(METRIC_CONVERSION_CONSTANT*1440))

#define mm2pt (METRIC_CONVERSION_CONSTANT*72)
#define pt2mm (1/(METRIC_CONVERSION_CONSTANT*72))

#define     wx_round(a)    (int)((a)+.5)


#endif // IN_CPROTO
#endif // wxb_dch
