/*
 * File:	wx_dccan.h
 * Purpose:	Canvas device context declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_dccanh
#define wx_dccanh

#include "wx_gdi.h"
#include "wb_dccan.h"

class wxCanvasDC: public wxbCanvasDC
{
 public:
  wxCanvasDC(void);

  void TryColour(wxColour *src, wxColour *dest);

  // Create a DC corresponding to a canvas
  wxCanvasDC(wxCanvas *canvas);
  void GetClippingBox(double *x,double *y,double *w,double *h) ;
  void GetClippingRegion(double *x, double *y, double *width, double *height);

  void GetSize(double *width, double *height);

  Bool GCBlit(double xdest, double ydest, double width, double height,
	      wxBitmap *source, double xsrc, double ysrc);

  ~wxCanvasDC(void);
};

#endif // wx_dccan

