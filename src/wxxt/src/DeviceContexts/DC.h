/*								-*- C++ -*-
 *
 * Purpose: basic device context
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 2004-2005 PLT Scheme, Inc.
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef DC_h
#define DC_h

#ifndef MZ_PRECISE_GC
# ifdef __GNUG__
# pragma interface
# endif
#endif

/* for floor(): */
#include <math.h>

#ifdef USE_GL
class wxGL;
class wxGLConfig;
#endif

// wxPoint
class wxPoint : public wxObject {
public:
  inline wxPoint(void);
  inline wxPoint(double a, double b);
  double x, y;
};

inline wxPoint::wxPoint(void) 
: wxObject(WXGC_NO_CLEANUP)
{ 
  x = y = 0.0;
}

inline wxPoint::wxPoint(double a, double b) 
: wxObject(WXGC_NO_CLEANUP)
{
  x = a;
  y = b;
}

class wxBitmap;
class wxBrush;
class wxCanvas;
class wxColour;
class wxColourMap;
class wxFont;
class wxList;
class wxPen;
class wxRegion;
class wxPath;

class wxDC : public wxObject {
public:
    wxDC(void);

    // pure virtual methods, must be implemented for all DCs
    virtual Bool  Blit(double xdest, double ydest, double w, double h, wxBitmap *src,
		       double xsrc, double ysrc, int rop=wxSOLID, wxColour *c=NULL, wxBitmap *mask=NULL) = 0;
    virtual Bool  CanGetTextExtent(void) = 0;
    virtual Bool  CanDrawBitmap(void) = 0;
    virtual void  Clear(void) = 0;
    virtual void  DrawArc(double x, double y, double w, double h, double start, double end) = 0;
    virtual void  DrawEllipse(double x, double y, double w, double h) = 0;
    virtual void  DrawLine(double x1, double y1, double x2, double y2) = 0;
    virtual void  DrawLines(int n, wxPoint pts[],
			    double xoff=0, double yoff=0) = 0;
    virtual void  DrawLines(wxList *pts, double xoff=0, double yoff=0);
    virtual void  DrawPoint(double x, double y) = 0;
            void  DrawPoint(wxPoint *pt)  { DrawPoint(pt->x, pt->y); }
    virtual void  DrawPolygon(int n, wxPoint pts[], double xoff=0, double yoff=0,
			      int fill=wxODDEVEN_RULE) = 0;
    virtual void  DrawPolygon(wxList *pts, double xoff=0, double yoff=0,
			      int fill=wxODDEVEN_RULE);
    virtual void  DrawRectangle(double x, double y, double w, double h) = 0;
    virtual void  DrawRoundedRectangle(double x, double y, double w, double h,
				       double radius=20) = 0;
    virtual void  DrawPath(wxPath *p, double dx, double dy, int fill=wxODDEVEN_RULE) = 0;

    virtual void  DrawText(char *text, double x, double y, Bool combine=FALSE,
			   Bool use16 = FALSE, int dt = 0, double angle = 0.0) = 0;
    virtual double GetCharHeight(void) = 0;
    virtual double GetCharWidth(void) = 0;
    virtual void  GetTextExtent(const char *s, double *w, double *h,
				double *descent = 0, double *ext_leading = 0,
				wxFont *font=NULL, 
				Bool combine=FALSE, Bool use16bit=FALSE, int dt=0) = 0;
    virtual void  SetBackground(wxColour *c) = 0;
    virtual void  SetBrush(wxBrush *brush) = 0;
    virtual void  SetClippingRect(double x, double y, double w, double h) = 0;
    virtual void  SetClippingRegion(wxRegion *r) = 0;
    virtual wxRegion *GetClippingRegion() = 0;
    virtual void  SetColourMap(wxColourMap *cmap) = 0;
    virtual void  SetFont(wxFont *font) = 0;
    virtual void  SetPen(wxPen *pen) = 0;
    virtual void  SetTextBackground(wxColour *col) = 0;
    virtual void  SetTextForeground(wxColour *col) = 0;

    virtual void TryColour(wxColour *src, wxColour *dest) = 0;

    // only necessary for printing
    virtual Bool  StartDoc(char *WXUNUSED(message)) { return TRUE; }
    virtual void  EndDoc(void) {}
    virtual void  StartPage(void) {}
    virtual void  EndPage(void) {}

    // non virtual methods, same in all subclasses
    void AutoSetTools(Bool set_auto)
	{ auto_setting = set_auto; }
    void BeginDrawing(void)
	{}
    virtual double DeviceToLogicalX(int x)
	{ return XDEV2LOG(x); }
    virtual double DeviceToLogicalXRel(int x)
	{ return XDEV2LOGREL(x); }
    virtual double DeviceToLogicalY(int y)
	{ return YDEV2LOG(y); }
    virtual double DeviceToLogicalYRel(int y)
	{ return YDEV2LOGREL(y); }
    void  DrawSpline(int n, wxPoint pts[]);
    void  DrawSpline(wxList *pts);
    virtual void DrawSpline(double x1,double y1, double x2,double y2, double x3,double y3);
    void  EndDrawing(void)
	{}
    wxColour *GetBackground(void);
    wxBrush *GetBrush(void)
	{ return current_brush; }
    wxFont *GetFont(void)
	{ return current_font; }
    int GetMapMode(void)
	{ return current_map_mode; }
    Bool GetOptimization(void)
	{ return optimize; }
    wxPen *GetPen(void)
	{ return current_pen; }
    Bool GetPixel(double WXUNUSED(x), double WXUNUSED(y),
		  wxColour *WXUNUSED(col))
	{ return FALSE; }
    virtual void GetSize(double *w, double *h)
	{ *w = (double)10; *h = (double)10; }
    void GetSizeMM(double *w, double *h)
	{ GetSize(w, h); *w/=(scale_x*mm_to_pix_x); *h/=(scale_y*mm_to_pix_y); }
    int GetTextAlignment(void)
	{ return current_text_alignment; }
    wxColour* GetTextBackground(void)
	{ return current_text_bg; }
    wxColour* GetTextForeground(void)
	{ return current_text_fg; }
    virtual int LogicalToDeviceX(double x)
	{ return XLOG2DEV(x); }
    virtual int LogicalToDeviceXRel(double x)
	{ return XLOG2DEVREL(x); }
    virtual int LogicalToDeviceY(double y)
	{ return YLOG2DEV(y); }
    virtual int LogicalToDeviceYRel(double y)
	{ return YLOG2DEVREL(y); }
    virtual double FLogicalToDeviceX(double x)
	{ return XLOG2DEV(x); }
    virtual double FLogicalToDeviceXRel(double x)
	{ return XLOG2DEVREL(x); }
    virtual double FLogicalToDeviceY(double y)
	{ return YLOG2DEV(y); }
    virtual double FLogicalToDeviceYRel(double y)
	{ return YLOG2DEVREL(y); }
    virtual Bool Ok(void)
	{ return ok; }
    void SetBackgroundMode(int mode)
	{ current_text_bgmode = mode; }
    int GetBackgroundMode()
	{ return current_text_bgmode; }
    void SetOptimization(Bool opt)
	{ optimize = opt; }
    void SetTextAlignment(int new_alignment)
	{ current_text_alignment = new_alignment; }
    // scale and origin methods
    void  SetDeviceOrigin(double x, double y);
    void  SetLogicalScale(double xs, double ys);
    void  SetMapMode(int mode);
    virtual void  SetUserScale(double xs, double ys);

    void GetUserScale(double *xs, double *ys)
      { *xs = user_scale_x; *ys = user_scale_y; }
    void GetDeviceOrigin(double *x, double *y) 
      { *x = device_origin_x; *y = device_origin_y; }

    virtual Bool GlyphAvailable(int c, wxFont *f = NULL) = 0;

#ifdef USE_GL
    virtual wxGL *GetGL();
#endif

    int GetAntiAlias();
    virtual void SetAntiAlias(int v);
  
    // public data members
    Bool  Colour;
    int   device;
protected:
    Bool  auto_setting, optimize, ok;
    // everything needed for sizing
    double mm_to_pix_x, mm_to_pix_y;
    double scale_x, scale_y;
    double device_origin_x, device_origin_y;
    double logical_scale_x, logical_scale_y, user_scale_x, user_scale_y;
    // Tools for drawing
    wxColour*    current_background_color;
    wxBrush*     current_brush;
    wxColourMap* current_cmap;
    wxFont*      current_font;
    int          current_map_mode;
    wxPen*       current_pen;
    int		 current_text_alignment;
    wxColour*    current_text_bg;
    int		 current_text_bgmode;
    wxColour*    current_text_fg;
    wxRegion     *clipping;
    int          anti_alias;
    // utilities for internal use
    void  CalcBoundingBox(double x, double y);
    void  ComputeScaleAndOrigin(void);
    // abbreviations
    double XDEV2LOG(int x)
      { return (double(x) / scale_x) - device_origin_x; }
    double XDEV2LOGREL(int x)
      { return double(double(x) / scale_x); }
    double YDEV2LOG(int y)
      { return (double(y) / scale_y) - device_origin_y; }
    double YDEV2LOGREL(int y)
      { return double(double(y) / scale_y); }
    int XLOG2DEV(double x)
      { double a = (x * scale_x) + device_origin_x;
	return (int)floor(a); }
    int XLOG2DEVREL(double x)
      { double a = x * scale_x;
	return (int)floor(a); }
    int YLOG2DEV(double y)
      { double a = (y * scale_y) + device_origin_y;
	return (int)floor(a); }
    int YLOG2DEVREL(double y)
      { double a = y * scale_y;
	return (int)floor(a); }
    // virtual function for spline drawing
    virtual void DrawOpenSpline(wxList *pts);
};

#endif // DC_h
