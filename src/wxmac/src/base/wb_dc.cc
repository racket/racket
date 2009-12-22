/*
 * File:        wb_dc.cc
 * Purpose:     Device context implementation
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	August 1994
 * Copyright:   (c) 2004-2010 PLT Scheme Inc.
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#ifdef __GNUG__
#pragma implementation "wb_dc.h"
#endif
#ifdef __GNUG__
#pragma implementation "wb_dccan.h"
#endif
#ifdef __GNUG__
#pragma implementation "wb_dcmem.h"
#endif
#endif

#include "common.h"
#include "wx_frame.h"
#include "wx_dc.h"
#include "wx_dcps.h"
// wx_dcmem.h not strictly necessary but required
// for GNU GCC when using pragmas.
#include "wx_dcmem.h"
#include "wx_stdev.h"
#include "wx_utils.h"
#include "wx_canvs.h"
#include "wx_dialg.h"
#include "wx_main.h"

// Default constructor
wxbDC::wxbDC(void)
{
  __type = wxTYPE_DC;
  min_x = 0; min_y = 0; max_x = 0; max_y = 0;
  auto_device_origin_x = auto_device_origin_y = 0;
  title = NULL;
  clipping = FALSE;
  autoSetting = TRUE ;
  current_bk_mode = wxTRANSPARENT;
  current_alpha = 1.0;
}

wxbDC::~wxbDC(void)
{
  title = NULL;
}

void wxbDC::DrawPolygon(wxList *list, double xoffset, double yoffset, int fillStyle)
{
  int i = 0;
  int n;
  wxPoint *points, *point;
  wxNode *node;

  n = list->Number();
  points  = new WXGC_ATOMIC wxPoint[n];

  for (node = list->First(); node; node = node->Next()) {
    point = (wxPoint *)(node->Data());
    points[i].x = point->x;
    points[i++].y = point->y;
  }
  DrawPolygon(n, points, xoffset, yoffset, fillStyle);
}

void wxbDC::DrawLines(wxList *list, double xoffset, double yoffset)
{
  int i = 0;
  int n;
  wxPoint *points;
  wxPoint *point;
  wxNode *node;

  n = list->Number();
  points = new WXGC_ATOMIC wxPoint[n];

  for (node = list->First(); node; node = node->Next()) {
    point = (wxPoint *)(node->Data());
    points[i].x = point->x;
    points[i++].y = point->y;
  }
  DrawLines(n, points, xoffset, yoffset);
}

void wxbDC::SetTextForeground(wxColour *colour)
{
  if (colour)
    current_text_foreground->CopyFrom(colour);
}

void wxbDC::SetTextBackground(wxColour *colour)
{
  if (colour)
    current_text_background->CopyFrom(colour);
}

void wxbDC::SetBackgroundMode(int mode)
{
  current_bk_mode = mode;
}

void wxbDC::GetSize(double *width, double *height)
{
  if (!(min_x == 1000.0 && min_y == 1000.0 && max_x == -1000.0 && max_y == -1000.0)) {
    *width = (double)(max_x - min_x);
    *height = (double)(max_y - min_y);
  } else {
    *width = 0.0;
    *height = 0.0;
  }
}

#if USE_SPLINES
// Make a 3-point spline
void wxbDC::DrawSpline(double x1, double y1, double x2, double y2, double x3, double y3)
{
  wxList *point_list;
  wxPoint *point1;
  wxPoint *point2;
  wxPoint *point3;

  point_list = new WXGC_PTRS wxList;
  point1 = new WXGC_ATOMIC wxPoint;
  point2 = new WXGC_ATOMIC wxPoint;
  point3 = new WXGC_ATOMIC wxPoint;

  point1->x = x1; point1->y = y1;
  point_list->Append((wxObject*)point1);

  point2->x = x2; point2->y = y2;
  point_list->Append((wxObject*)point2);

  point3->x = x3; point3->y = y3;
  point_list->Append((wxObject*)point3);

  DrawSpline(point_list);
}
#endif

wxColor *wxbDC::GetBackground(void)
{ 
  return new WXGC_PTRS wxColour(current_background_color);
}

void wxbDC::SetLogicalOrigin(double x, double y)
{
  logical_origin_x = x;
  logical_origin_y = y;
}

void wxbDC::SetDeviceOrigin(double x, double y)
{
  device_origin_x = x + auto_device_origin_x;
  device_origin_y = y + auto_device_origin_y;
}

// For use by wxWindows only, unless custom units are required.
void wxbDC::SetLogicalScale(double x, double y)
{
  logical_scale_x = x;
  logical_scale_y = y;
}

void wxbDC::TryColour(wxColour *src, wxColour *dest)
{
  dest->CopyFrom(src);
}

Bool wxbDC::GetAntiAlias()
{
  return anti_alias;
}

void wxbDC::SetAntiAlias(Bool v)
{
  anti_alias = v;
}

void wxbDC::SetAlpha(double a)
{
  current_alpha = a;
}

double wxbDC::GetAlpha()
{
  return current_alpha;
}

int wxbDC::CacheFontMetricsKey()
{
  return 0;
}

wxbCanvasDC::wxbCanvasDC(void)
{
}

wxbCanvasDC::wxbCanvasDC(wxCanvas *the_canvas)
{
}

wxbMemoryDC::wxbMemoryDC(void)
{
}

