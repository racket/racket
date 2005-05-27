/*
 * File:	wb_dc.h
 * Purpose:	wxDC device context declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
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

#define MM_POINTS      7
#define MM_METRIC      8

#define wxPIXELS_MAP    0
#define wxPOINTS_MAP    1

#define wxWINDOWS_SCALE 0
#define wxWX_SCALE      1

class wxRegion;
class wxCanvas;
class wxCanvasDC;
class wxDC;
class wxbDC: public wxObject
{
 protected:
  Bool dcOptimize;
 public:
  int device;
  Bool ok;
  Bool wx_interactive;
  wxRegion *clipping;

  // Coordinate system variables
  double device_origin_x;
  double device_origin_y;

  double user_scale_x;
  double user_scale_y;
  double logical_scale_x;
  double logical_scale_y;

  int mapping_mode;
  int scaling_mode;

  char *title;

  Bool Colour;

  int current_bk_mode;

  wxPen *current_pen;
  wxBrush *current_brush;
  wxColour *current_background_color;
  wxColour *current_text_foreground;
  wxColour *current_text_background;
  wxFont *font;
  Bool autoSetting;
  Bool anti_alias;

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
  inline virtual void DrawPoint(wxPoint *point) { DrawPoint(point->x, point->y); }
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
#endif
  virtual void DrawText(const char *text, double x, double y,
                        Bool combine = FALSE, Bool use16bit = FALSE, 
			int d = 0, double angle = 0.0) = 0;
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
  int GetBackgroundMode(void);
  virtual void SetClippingRect(double x, double y, double width, double height)= 0;
  virtual wxRegion *GetClippingRegion() = 0;
  virtual void SetClippingRegion(wxRegion *r) = 0;
  inline virtual void SetColourMap(wxColourMap *WXUNUSED(cmap)) {};

  virtual double GetCharHeight(void) = 0;
  virtual double GetCharWidth(void) = 0;
  virtual void GetTextExtent(const char *string, double *x, double *y,
                             double *descent = NULL, 
                             double *externalLeading = NULL, 
                             wxFont *theFont = NULL,
                             Bool combine = FALSE, Bool use16bit = FALSE,
			     int d = 0) = 0;

  inline virtual Bool Ok(void) {return ok;};
  virtual Bool CanGetTextExtent(void) = 0;
  virtual Bool CanDrawBitmap(void) = 0;
  inline virtual int  GetMapMode(void) {return mapping_mode;};

  // The following methods provide a cleaner interface
  virtual wxColour *GetBackground(void);
  inline virtual wxBrush *GetBrush(void)           { return current_brush;}
  inline virtual wxFont  *GetFont(void)            { return font;}
  inline virtual wxPen   *GetPen(void)             { return current_pen;}
  inline virtual wxColour* GetTextBackground(void)  { return current_text_background;}
  inline virtual wxColour* GetTextForeground(void)  { return current_text_foreground;}

  virtual Bool GlyphAvailable(int c, wxFont *f = NULL) = 0;
 
  virtual void SetDeviceOrigin(double x, double y);
  void GetDeviceOrigin(double *x, double *y) {*x = device_origin_x; *y = device_origin_y; }
  virtual void SetUserScale(double x, double y) = 0;
  void GetUserScale(double *x, double *y) {*x = user_scale_x; *y = user_scale_y; }
  virtual double DeviceToLogicalX(int x) = 0;
  virtual double DeviceToLogicalY(int y) = 0;
  virtual double DeviceToLogicalXRel(int x) = 0;
  virtual double DeviceToLogicalYRel(int y) = 0;
  virtual int LogicalToDeviceX(double x) = 0;
  virtual int LogicalToDeviceY(double y) = 0;
  virtual int LogicalToDeviceXRel(double x) = 0;
  virtual int LogicalToDeviceYRel(double y) = 0;
  virtual double FLogicalToDeviceX(double x) = 0;
  virtual double FLogicalToDeviceY(double y) = 0;
  virtual double FLogicalToDeviceXRel(double x) = 0;
  virtual double FLogicalToDeviceYRel(double y) = 0;
  // Only works for PostScript *after* you've printed an image.
  // Gives width and height of image.
  virtual void GetSize(double *width, double *height);
  virtual inline void GetSizeMM(double *width, double *height) { *width = 0.0; *height = 0.0; };
  virtual Bool Blit(double xdest, double ydest, double width, double height,
                    wxBitmap *source, double xsrc, double ysrc, int rop = wxSOLID, 
		    wxColour* c = NULL, wxBitmap *mask = NULL) = 0;

  // Sometimes we need to override optimization, e.g.
  // if other software is drawing onto our surface and we
  // can't be sure of who's done what.
  inline virtual void SetOptimization(Bool opt) { dcOptimize = opt; }
  inline virtual Bool GetOptimization(void) { return dcOptimize; }

  virtual void TryColour(wxColour *src, wxColour *dest);
 
  Bool GetAntiAlias();
  virtual void SetAntiAlias(Bool v);
};

/*
extern char wx_printer_file[];
extern double wx_printer_scale_x;
extern double wx_printer_scale_y;
extern double wx_printer_translate_x;
extern double wx_printer_translate_y;
*/
extern int wxPageNumber;

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

#endif // wxb_dch
