///////////////////////////////////////////////////////////////////////////////
// File:	wx_dccan1.cc
// Purpose:	Canvas device context implementation (Macintosh version) (part 1)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2010 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "common.h"
#include <math.h>
#include "wx_dccan.h"
#include "wx_canvs.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wx_area.h"
#include "wx_rgn.h"
#ifdef OS_X
# include <AGL/agl.h> 
#endif

extern CGrafPtr wxMainColormap;

static int patterns_inited;
Pattern wx_white_pat, wx_black_pat, wx_light_gray_pat, wx_dark_gray_pat;

static wxCanvasDC *reset_chain;

void wx_init_patterns(void)
{
  wxREGGLOB(reset_chain);
  GetQDGlobalsWhite(&wx_white_pat);
  GetQDGlobalsBlack(&wx_black_pat);
  GetQDGlobalsLightGray(&wx_light_gray_pat);
  GetQDGlobalsDarkGray(&wx_dark_gray_pat);
  patterns_inited = 1;
}

//-----------------------------------------------------------------------------
// Default constructor
//-----------------------------------------------------------------------------
wxCanvasDC::wxCanvasDC(void)
{
  Init(NULL);
}


//-----------------------------------------------------------------------------
wxCanvasDC::wxCanvasDC(wxCanvas* the_canvas): wxbCanvasDC(the_canvas)
{
  Init(the_canvas);
}

void wxCanvasDC::Init(wxCanvas* the_canvas)
{
  __type = wxTYPE_DC_CANVAS;

  canvas = the_canvas;
  if (canvas) {
    WXGC_IGNORE(this, canvas);
    cMacDC = canvas->MacDC();
  }

  cMacDoingDrawing = FALSE;

  clipping = NULL;
  selected_pixmap = NULL;

  pixmapWidth = 0;
  pixmapHeight = 0;

  current_reg = NULL;
  onpaint_reg = NULL;
  clip_reg = NULL;

  min_x = 0; min_y = 0; max_x = 0; max_y = 0;

  device = wxDEVICE_CANVAS;
  font = wxNORMAL_FONT;
  logical_origin_x = 0;
  logical_origin_y = 0;
  device_origin_x = 0;
  device_origin_y = 0;

  logical_scale_x = 1.0;
  logical_scale_y = 1.0;

  user_scale_x = 1.0;
  user_scale_y = 1.0;

  mapping_mode = MM_TEXT;

  title = NULL;

  ok = TRUE;
  current_pen_join = -1 ;
  current_pen_cap = -1 ;
  current_pen_nb_dash = -1 ;
  current_pen_dash = NULL ;
  current_stipple = NULL ;

  Colour = wxColourDisplay();

  current_pen = NULL;
  current_brush = NULL;
  SetBrush(wxWHITE_BRUSH);
  SetPen(wxBLACK_PEN);

  if (the_canvas) {
    int clientWidth, clientHeight;
    Rect paintRect;
    the_canvas->GetClientSize(&clientWidth, &clientHeight);
    ::SetRect(&paintRect, 0, 0, clientWidth, clientHeight);
    SetPaintRegion(&paintRect);
  }
}

//-----------------------------------------------------------------------------
wxCanvasDC::~wxCanvasDC(void)
{
  if (gl) {
    gl->Reset(NULL, NULL, 0, 0, 0);
    gl = NULL;
  }

  if (current_pen) current_pen->Lock(-1);
  if (current_brush) current_brush->Lock(-1);
  if (clipping) --clipping->locked;
  
  if (current_reg) {
    ::DisposeRgn(current_reg);
    current_reg = NULL;
  }
  if (onpaint_reg) {
    ::DisposeRgn(onpaint_reg);
    onpaint_reg = NULL;
  }
  if (clip_reg) {
    ::DisposeRgn(clip_reg);
    clip_reg = NULL;
  }
  canvas = NULL;

  if (reset_chain == this) {
    reset_chain = chain_next;
    if (chain_next)
      chain_next->chain_prev = NULL;
    chain_next = NULL;
  } else {
    if (chain_prev)
      chain_prev->chain_next = chain_next;
    if (chain_next)
      chain_next->chain_prev = chain_prev;
    chain_prev = chain_next = NULL;
  }
}

void wxCanvasDC::BeginDrawing(void)
{
}

void wxCanvasDC::EndDrawing(void)
{
}


static GDHandle def_dev_handle = 0;
static CGrafPtr def_grafptr = NULL;

//-----------------------------------------------------------------------------
void wxCanvasDC::SetCurrentDC(Bool cgok)
//-----------------------------------------------------------------------------
{
  CGrafPtr theMacGrafPort;

  if (!Ok() || !cMacDC) return;
  
  dc_set_depth++;

  if ((dc_set_depth != 1)
      || def_grafptr)
    printf("Nested SetDCs\n");

  if (!cgok)
    cMacDC->EndCG();
  else if (cMacDC->currentUser() != this)
    cMacDC->EndCG();

  if (!cMacDC->GetCG(TRUE)) {
    theMacGrafPort = cMacDC->macGrafPort();

    if (!canvas)
      GetGWorld(&def_grafptr, &def_dev_handle);

    if (!canvas /* IsPortOffscreen(theMacGrafPort) */) {
      ::SetGWorld(theMacGrafPort, NULL);
    } else {
      ::SetPort(theMacGrafPort);
    }

    SetOriginX = gdx;
    SetOriginY = gdy;
#if 0
    if (canvas) {
      wxArea *area;
      area = canvas->ClientArea();
      area->FrameContentAreaOffset(&SetOriginX, &SetOriginY);
    }
#endif

    if (cMacDC->currentUser() != this) { 
      // must setup platform
      cMacDC->setCurrentUser(this);
      wxMacSetClip();
      ToolChanged(kNoTool);
    }
  }
}

void wxCanvasDC::ReleaseCurrentDC(void)
{
  if (!--dc_set_depth) {
    if (canvas) {
      /* We have to go back to the canvas's drawings settings --- at
	 least the bg color --- otherwise controls might get drawn
	 wrong. That's because the DC GrafPtr is the same as the Window GrafPtr. */
      if (!chain_next && !chain_prev) {
	chain_next = reset_chain;
	if (reset_chain)
	  reset_chain->chain_prev = this;
	reset_chain = this;
      }
    } else {
      ::SetGWorld(def_grafptr, def_dev_handle);
      def_grafptr = NULL;
    }
  }
}

void wxCanvasDC::ResetBackground()
{
  CGrafPtr theMacGrafPort;

  if (cMacDC->currentUser() == this) {
    cMacDC->EndCG();
    
    theMacGrafPort = cMacDC->macGrafPort();
    ::SetPort(theMacGrafPort);
    cMacCurrentTool = kNoTool;
    canvas->MacSetBackground();
  }

  reset_chain = chain_next;
  chain_next = NULL;
  chain_prev = NULL;
}

void wxResetCanvasBackgrounds()
{
  if (reset_chain) {
    CGrafPtr savep;
    GDHandle savegd;
    
    GetGWorld(&savep, &savegd);  

    while (reset_chain) {
      reset_chain->ResetBackground();
    }
    
    SetGWorld(savep, savegd);
  }
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetCanvasClipping(void)
  //-----------------------------------------------------------------------------
{
  wxObject* theCurrentUser;

  if (!Ok() || !cMacDC)
    return;

  if (current_reg) {
    ::DisposeRgn(current_reg);
  }
  if (clipping || onpaint_reg || clip_reg) {
    current_reg = ::NewRgn();
    CheckMemOK(current_reg);
  } else
    current_reg = NULL;
  
  if (clipping && !clipping->rgn) {
    /* NULL rgn pointer means the empty region */
  } else if (clipping) {
    ::CopyRgn(clipping->rgn, current_reg);
    ::OffsetRgn(current_reg, auto_device_origin_x, auto_device_origin_y); 
    if (onpaint_reg) {
      ::SectRgn(current_reg, onpaint_reg, current_reg);
    }
    if (clip_reg) {
      ::SectRgn(current_reg, clip_reg, current_reg);
    }
  } else if (onpaint_reg) {
    ::CopyRgn(onpaint_reg, current_reg);
    if (clip_reg) {
      ::SectRgn(current_reg, clip_reg, current_reg);
    }
  } else if (clip_reg) {
    ::CopyRgn(clip_reg, current_reg);
  }

  theCurrentUser = cMacDC->currentUser();
  if (theCurrentUser == this) { 
    // must update
    CGrafPtr savep;
    GDHandle savegd;
    CGrafPtr theMacGrafPort;
    long oox, ooy;

    cMacDC->EndCG();

    theMacGrafPort = cMacDC->macGrafPort();
    
    ::GetGWorld(&savep, &savegd);  
    if (IsPortOffscreen(theMacGrafPort)) {
      ::SetGWorld(theMacGrafPort, NULL);
    } else {
      ::SetPort(theMacGrafPort);
    }
    
    oox = SetOriginX;
    ooy = SetOriginY;
    SetOriginX = SetOriginY = 0;
    if (canvas) {
      wxArea *area;
      area = canvas->ClientArea();
      area->FrameContentAreaOffset(&SetOriginX, &SetOriginY);
    }

    wxMacSetClip();

    SetOriginX = oox;
    SetOriginY = ooy;
    
    ::SetGWorld(savep, savegd);
  }
}

//-----------------------------------------------------------------------------
void wxCanvasDC::GetClippingBox(double *x,double *y,double *w,double *h)
  //-----------------------------------------------------------------------------
{
  if (current_reg && cMacDC) {
    CGrafPtr theMacGrafPort;
    RgnHandle clipRgn;
    Rect theClipRect;
    int theX, theY, theWidth, theHeight;

    theMacGrafPort = cMacDC->macGrafPort();
    clipRgn = NewRgn();
      
    GetPortClipRegion(theMacGrafPort,clipRgn);
    GetRegionBounds(clipRgn,&theClipRect);
    
    DisposeRgn(clipRgn);

    theX = theClipRect.left;
    theY = theClipRect.top;
    theWidth = theClipRect.right - theClipRect.left;
    theHeight = theClipRect.bottom - theClipRect.top;
    *x = XDEV2LOG(theX) ;
    *y = YDEV2LOG(theY) ;
    *w = XDEV2LOGREL(theWidth) ;
    *h = YDEV2LOGREL(theHeight) ;
  } else
    *x = *y = *w = *h = 0;
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetPaintRegion(Rect* paintRect)
  //-----------------------------------------------------------------------------
{
  if (onpaint_reg) { ::DisposeRgn(onpaint_reg); }
  onpaint_reg = ::NewRgn();
  CheckMemOK(onpaint_reg);
  ::RectRgn(onpaint_reg, paintRect);
  SetCanvasClipping();
}

void wxCanvasDC::SetGrafPtrOffsets(int ox, int oy)
{
  gdx = ox;
  gdy = oy;
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetClippingRect(double cx, double cy, double cw, double ch)
  //-----------------------------------------------------------------------------
{
  wxRegion *r;
  if (clippingCached)
    r = clippingCached;
  else {
    r = new WXGC_PTRS wxRegion(this);
    clippingCached = r;
  }
  r->SetRectangle(cx, cy, cw, ch);
  SetClippingRegion(r);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetClippingRegion(wxRegion *r)
  //-----------------------------------------------------------------------------
{
  if (clipping)
    --clipping->locked;

  clipping = r;

  if (clipping)
    clipping->locked++;

  SetCanvasClipping();
}

//-----------------------------------------------------------------------------
wxRegion* wxCanvasDC::GetClippingRegion()
  //-----------------------------------------------------------------------------
{
  if (clipping == clippingCached)
    clippingCached = NULL;
  return clipping;
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetFont(wxFont *the_font)
{
  font = the_font;
  ToolChanged(kTextTool);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetPen(wxPen *pen)
{
  if (pen != current_pen) {
    if (current_pen) current_pen->Lock(-1);
    current_pen = pen;
    if (current_pen) current_pen->Lock(1);
    
    ToolChanged(kPenTool);
  }
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetBrush(wxBrush *brush)
{
  if (brush != current_brush) {
    if (current_brush) current_brush->Lock(-1);
    current_brush = brush;
    if (current_brush) current_brush->Lock(1);
    
    ToolChanged(kBrushTool);
  }
}

//-----------------------------------------------------------------------------
void wxCanvasDC::InstallColor(wxColour *c, int fg)
{
  if (Colour) {
    RGBColor pixel;
    pixel = c->pixel;
    if (fg)
      RGBForeColor(&pixel);
    else
      RGBBackColor(&pixel);
  } else {
    unsigned char red, blue, green;
    red = c->Red();
    blue = c->Blue();
    green = c->Green();
    if (fg) {
      Bool isWhiteColour =
	(red == (unsigned char )255 &&
	 blue == (unsigned char)255 &&
	 green == (unsigned char)255);
      ForeColor(isWhiteColour ? whiteColor : blackColor);
    } else {
      Bool isBlackColour =
	(red == (unsigned char )0 &&
	 blue == (unsigned char)0 &&
	 green == (unsigned char)0);
      BackColor(isBlackColour ? blackColor : whiteColor);
    }
  }
}

void wxCanvasDC::InstallLogicalFunction(int function)
{
  int theMacPenMode;

  switch (function)
    {
    case wxCLEAR:  theMacPenMode = patBic; break;
    case wxXOR:  theMacPenMode = patXor; break;
    case wxINVERT: theMacPenMode = notPatCopy /* GXinvert */; break;
    case wxOR_REVERSE: theMacPenMode = patXor /* GXorReverse */; break;
    case wxAND_REVERSE: theMacPenMode = patCopy /* GXandReverse */; break;
    case wxAND: theMacPenMode = adMin; break;
    case wxOR: theMacPenMode = adMax; break;
    case wxAND_INVERT: theMacPenMode = patBic; break;
    case wxNO_OP: theMacPenMode = patCopy /* GXnoop */; break;
    case wxNOR: theMacPenMode = notPatOr /* GXnor */; break;
    case wxEQUIV: theMacPenMode = notPatXor; break;
    case wxSRC_INVERT: theMacPenMode = notPatCopy; break;
    case wxOR_INVERT: theMacPenMode = notPatOr; break;
    case wxNAND: theMacPenMode = notPatCopy /* GXnand */; break;
    case wxSET: theMacPenMode = patCopy /* GXset */; break;
    case wxCOPY:
    default:
      theMacPenMode = patCopy; break;
    }
  
  PenMode(theMacPenMode);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetBackground(wxColour* color)
{
  current_background_color = color;

  ToolChanged(kNoTool);
}

//-----------------------------------------------------------------------------
Bool wxCanvasDC::StartDoc(char *message) { return TRUE; }

//-----------------------------------------------------------------------------
void wxCanvasDC::EndDoc(void) { }

//-----------------------------------------------------------------------------
void wxCanvasDC::StartPage(void) { }

//-----------------------------------------------------------------------------
void wxCanvasDC::EndPage(void){ }

//-----------------------------------------------------------------------------
void wxCanvasDC::SetMapMode(int mode)
{
  /* not used */
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetUserScale(double x, double y)
  //-----------------------------------------------------------------------------
{
  cMacDC->EndCG();

  user_scale_x = x;
  user_scale_y = y;

  ToolChanged(kNoTool);
}

void wxCanvasDC::SetDeviceOrigin(double x, double y)
{
  cMacDC->EndCG();

  wxbCanvasDC::SetDeviceOrigin(x, y);
}

//-----------------------------------------------------------------------------
double wxCanvasDC::DeviceToLogicalX(int x) { return XDEV2LOG(x); }

//-----------------------------------------------------------------------------
double wxCanvasDC::DeviceToLogicalXRel(int x) { return XDEV2LOGREL(x); }

//-----------------------------------------------------------------------------
double wxCanvasDC::UnscrolledDeviceToLogicalX(int x) { return XUDEV2LOG(x); }

//-----------------------------------------------------------------------------
double wxCanvasDC::DeviceToLogicalY(int y) { return YDEV2LOG(y); }

//-----------------------------------------------------------------------------
double wxCanvasDC::DeviceToLogicalYRel(int y) { return YDEV2LOGREL(y); }

//-----------------------------------------------------------------------------
double wxCanvasDC::UnscrolledDeviceToLogicalY(int y) { return YUDEV2LOG(y); }

//-----------------------------------------------------------------------------
int wxCanvasDC::LogicalToDeviceX(double x) { return XLOG2DEV(x); }

//-----------------------------------------------------------------------------
int wxCanvasDC::LogicalToDeviceXRel(double x) { return XLOG2DEVREL(x); }

//-----------------------------------------------------------------------------
int wxCanvasDC::LogicalToUnscrolledDeviceX(double x) { return XLOG2UDEV(x); }

//-----------------------------------------------------------------------------
int wxCanvasDC::LogicalToDeviceY(double y) { return YLOG2DEV(y); }

//-----------------------------------------------------------------------------
int wxCanvasDC::LogicalToDeviceYRel(double y) { return YLOG2DEVREL(y); }

//-----------------------------------------------------------------------------
int wxCanvasDC::LogicalToUnscrolledDeviceY(double y) { return YLOG2UDEV(y); }

//-----------------------------------------------------------------------------
double wxCanvasDC::FLogicalToDeviceX(double x) { return XLOG2DEV(x); }

//-----------------------------------------------------------------------------
double wxCanvasDC::FLogicalToDeviceXRel(double x) { return XLOG2DEVREL(x); }

//-----------------------------------------------------------------------------
double wxCanvasDC::FLogicalToUnscrolledDeviceX(double x) { return XLOG2UDEV(x); }

//-----------------------------------------------------------------------------
double wxCanvasDC::FLogicalToDeviceY(double y) { return YLOG2DEV(y); }

//-----------------------------------------------------------------------------
double wxCanvasDC::FLogicalToDeviceYRel(double y) { return YLOG2DEVREL(y); }

//-----------------------------------------------------------------------------
double wxCanvasDC::FLogicalToUnscrolledDeviceY(double y) { return YLOG2UDEV(y); }

//-----------------------------------------------------------------------------
void wxCanvasDC::wxMacSetClip(void)
{
  /* current GrafPtr is set when this function is called */

  if (canvas && canvas->IsHidden()) {
    Rect zeroClipRect = {0, 0, 0, 0};
    ::ClipRect(&zeroClipRect);
  } else {
    if (current_reg) {
      if (SetOriginX || SetOriginY) {
	RgnHandle rgn;
	rgn = ::NewRgn();
	::CopyRgn(current_reg,rgn);
	::OffsetRgn(rgn,SetOriginX,SetOriginY);
	::SetClip(rgn);
	DisposeRgn(rgn);
      } else {
	::SetClip(current_reg);
      }
    } else {
      Rect largestClipRect = {-32767, -32767, 32767, 32767};
      ::ClipRect(&largestClipRect);
    }
  }
}

static void HiliteMode()
{
  RGBColor col;
  LMGetHiliteRGB(&col);
  col.red = 0xFFFF - col.red;
  col.green = 0xFFFF - col.green;
  col.blue = 0xFFFF - col.blue;
  PenMode(subPin);
  RGBForeColor(&col);
}

//-----------------------------------------------------------------------------

static CGLineJoin join_style[] = { kCGLineJoinBevel, kCGLineJoinMiter, kCGLineJoinRound };
static CGLineCap cap_style[]  = { kCGLineCapRound, kCGLineCapSquare, kCGLineCapButt };

static float dotted[] = {2, 5, /* offset */ 2};
static float short_dashed[] = {4, 4, /* offset */ 2};
static float long_dashed[] = {4, 8, /* offset */ 2};
static float dotted_dashed[] = {6, 6, 2, 6, /* offset */ 4};

void wxCanvasDC::wxMacSetCurrentTool(wxMacToolType whichTool)
  /* assumes that SetCurrentDC() has been called, already */  
{
  CGContextRef cg;

  if (!Ok() || !cMacDC) return;

  if (whichTool == cMacCurrentTool)
    return;

  cg = cMacDC->GetCG(TRUE);

  switch (whichTool) {
  case kNoTool:
    break;
  case kBGTool:
    InstallColor(current_background_color, FALSE);
    BackPat(GetWhitePattern());
    PenMode(patCopy);
    break;
  case kBrushTool:
    if (cg) {
      wxColor *c;
      int r, g, b;

      c = current_brush->GetColour();
      r = c->Red();
      g = c->Green();
      b = c->Blue();
      
      CGContextSetRGBFillColor(cg, r / 255.0, g / 255.0, b / 255.0, 1.0);
    } else {
      int theBrushStyle;
      theBrushStyle = current_brush->GetStyle();
      if (theBrushStyle == wxPANEL_PATTERN) {
	int depth;
	depth = wxDisplayDepth();
	SetThemeBackground(kThemeBrushDialogBackgroundActive, depth, depth > 1);
	paint_brush_with_erase = 1;
      } else {
	int log = (theBrushStyle == wxXOR ? patXor : patCopy);
	if ((theBrushStyle == wxSOLID) || (theBrushStyle == wxXOR) || (theBrushStyle == wxCOLOR))
	  PenPat(GetBlackPattern());
	else if (theBrushStyle == wxTRANSPARENT)
	  PenPat(GetWhitePattern());
	else if (IS_HATCH(theBrushStyle)) {
	  macGetHatchPattern(theBrushStyle, &cMacPattern);
	  PenPat(&cMacPattern);
	  log = patOr;
	} else {
	  PenPat(GetBlackPattern());
	}
	
	InstallColor(current_background_color, FALSE);
	BackPat(GetWhitePattern());
	if (Colour && (theBrushStyle == wxCOLOR))
	  HiliteMode();
	else {
	  InstallColor(current_brush->GetColour(), TRUE);
	  PenMode(log);
	}
	paint_brush_with_erase = 0;
      }
    }
    break;
  case kPenTool:
    if (cg) {
      wxColor *c;
      int r, g, b, cap, join;
      double pw;
      float *dashes;
      int ndash;
      float offset;

      c = current_pen->GetColour();
      r = c->Red();
      g = c->Green();
      b = c->Blue();
      
      CGContextSetRGBStrokeColor(cg, r / 255.0, g / 255.0, b / 255.0, 1.0);

      pw = current_pen->GetWidthF();
      if (AlignSmoothing()) {
	pw = (int)pw;
	pw = (int)(pw * user_scale_x);
	if (!pw)
	  pw = 1;
      } else {
	if (!pw) {
	  if (user_scale_x > user_scale_y)
	    pw = 1 / user_scale_y;
	  else
	    pw = 1 / user_scale_x;
	}
      }
      CGContextSetLineWidth(cg, pw);

      join = current_pen->GetJoin();
      CGContextSetLineJoin(cg, join_style[join - wxJOIN_BEVEL]);
      cap = current_pen->GetCap();
      CGContextSetLineCap(cg, cap_style[cap - wxCAP_ROUND]);

      switch (current_pen->GetStyle ()) {
      case wxDOT:
	dashes = dotted;
	ndash = (sizeof(dotted) / sizeof(float)) - 1;
	break;
      case wxSHORT_DASH:
	dashes = short_dashed;
	ndash = (sizeof(short_dashed) / sizeof(float)) - 1;
	break;
      case wxLONG_DASH:
	dashes = long_dashed;
	ndash = (sizeof(long_dashed) / sizeof(float)) - 1;
	break;
      case wxDOT_DASH:
	dashes = dotted_dashed;
	ndash = (sizeof(dotted_dashed) / sizeof(float)) - 1;
	break;
      case wxSOLID:
      case wxTRANSPARENT:
      default:
	dashes = NULL;
	ndash = 0;
	break;
      }
      if (ndash)
	offset = dashes[ndash];
      else
	offset = 0;
      CGContextSetLineDash(cg, offset, dashes, ndash);
    } else {
      int pensize;
      int thePenWidth;
      int thePenStyle, origPenStyle;
      int log;
      wxBitmap *bm;

      pensize = current_pen->GetWidth();
      thePenWidth = (pensize ? pensize : 1);
      if (pensize) {
	int sx, sy;
	sx = XLOG2DEVREL(thePenWidth);
	sy = YLOG2DEVREL(thePenWidth);
	PenSize(sx ? sx : 1, sy ? sy : 1);
      } else
	PenSize(1, 1);
      
      thePenStyle = current_pen->GetStyle();
      origPenStyle = thePenStyle;
      log = patCopy;
      switch (thePenStyle) {
      case wxXOR:
      case wxCOLOR:
	thePenStyle = wxSOLID;
	log = patXor;
	break;
      case wxXOR_DOT:
	thePenStyle = wxDOT;
	log = patXor;
	break;
      case wxXOR_LONG_DASH:
	thePenStyle = wxLONG_DASH;
	log = patXor;
	break;
      case wxXOR_SHORT_DASH:
	thePenStyle = wxSHORT_DASH;
	log = patXor;
	break;
      case wxXOR_DOT_DASH:
	thePenStyle = wxDOT_DASH;
	log = patXor;
	break;
      }
      bm = current_pen->GetStipple();
      if (bm && bm->Ok() && (bm->GetDepth() == 1)
	  && (bm->GetWidth() == 8) && (bm->GetHeight() == 8)) {
	GDHandle savegd;
	CGrafPtr saveport;
	char p[8]; int i, k;
	GetGWorld(&saveport, &savegd);
	SetGWorld(bm->x_pixmap, 0);
	for (i = 0; i < 8; i++) {
	  p[i] = 0;
	  for (k = 0; k < 8; k++) {
	    RGBColor cpix;
	    ::GetCPixel(k, i, &cpix);
	    p[i] = p[i] << 1;
	    if (!cpix.red) {
	      p[i] |= 1;
	    }
	  }
	}
	SetGWorld(saveport, savegd);
	PenPat((Pattern *)p);
	if (origPenStyle == wxCOLOR)
	  origPenStyle = wxXOR;
      } else if (thePenStyle == wxSOLID)
	PenPat(GetBlackPattern());
      else if (thePenStyle == wxTRANSPARENT)
	PenPat(GetWhitePattern());
      else if ((thePenStyle == wxDOT)
	       || (thePenStyle == wxSHORT_DASH)) {
	PenPat(GetLightGrayPattern());
	if (log == patCopy) log = patOr;
      } else if ((thePenStyle == wxLONG_DASH)
		 || (thePenStyle == wxDOT_DASH)) {
	PenPat(GetDarkGrayPattern());
	if (log == patCopy) log = patOr;
      } else if (IS_HATCH(thePenStyle)) {
	macGetHatchPattern(thePenStyle, &cMacPattern);
	PenPat(&cMacPattern);
      } else {
	PenPat(GetBlackPattern());
      }

      BackPat(GetWhitePattern());
      InstallColor(current_background_color, FALSE);
      if (Colour && (origPenStyle == wxCOLOR))
	HiliteMode();
      else {
	InstallColor(current_pen->GetColour(), TRUE);
	PenMode(log);
      }
    }
    break;
  case kTextTool:
    InstallColor(current_text_foreground, TRUE);
    if (current_bk_mode != wxTRANSPARENT)
      InstallColor(current_text_background, FALSE);
    else
      BackColor(whiteColor);
    BackPat(GetWhitePattern());
    ::TextFont(font->GetMacFontNum());
    ::TextSize(font->GetPointSize());
    ::TextFace(font->GetMacFontStyle());
    ::TextMode((current_bk_mode == wxTRANSPARENT) ? srcOr : srcCopy);
    InstallLogicalFunction(wxCOPY);
    break;
  case kBlitTool:
    ForeColor(blackColor);
    BackColor(whiteColor);
    BackPat(GetWhitePattern());
    InstallLogicalFunction(wxCOPY);
    break;
  case kColorBlitTool:
    InstallColor(current_background_color, FALSE);
    BackPat(GetWhitePattern());
    InstallColor(current_pen->GetColour(), TRUE);
    InstallLogicalFunction(wxCOPY);
    break;
  case kQuillTool:
    break;
  }

  cMacCurrentTool = whichTool;
}

Bool wxCanvasDC::GlyphAvailable(int c, wxFont *f)
{
  if (!f)
    f = font;

  return f->ScreenGlyphAvailable(c);
}

void wxCanvasDC::SetAntiAlias(Bool v)
{
  if (anti_alias != v) {
    if (cMacDC)
      cMacDC->EndCG();
  }
  wxbCanvasDC::SetAntiAlias(v);
}

CGContextRef wxCanvasDC::GetCG()
{
  CGContextRef cg;
  CGrafPtr qdp;
  Rect portRect;
  RgnHandle clipRgn;

  cg = cMacDC->GetCG(TRUE);
  if (cg)
    return cg; /* Must be up-to-date */
  
  qdp = cMacDC->macGrafPort();
  GetPortBounds(qdp, &portRect);
   
  /* Make clipping regions match (including BeginUpdate effect) */
  clipRgn = NewRgn();
  if (canvas && canvas->IsHidden()) {
    /* Leave the region empty */
  } else {
    if (clipRgn) {
      if (onpaint_reg || clip_reg) {
	RgnHandle visRgn;
	visRgn = NewRgn();
	if (visRgn) {
          if (onpaint_reg) {
            ::CopyRgn(onpaint_reg, clipRgn);
            if (clip_reg)
              ::SectRgn(clipRgn, clip_reg, clipRgn);
          } else {
            ::CopyRgn(clip_reg, clipRgn);
          }
	  ::OffsetRgn(clipRgn, gdx, gdy);
	  GetPortVisibleRegion(qdp, visRgn);
	  SectRgn(clipRgn, visRgn, clipRgn);
	  DisposeRgn(visRgn);
	}
      } else {
	GetPortVisibleRegion(qdp, clipRgn);
      }
    }
  }

  cg = cMacDC->GetCG();

  SyncCGContextOriginWithPort(cg, qdp);
  if (clipRgn) {
    ClipCGContextToRegion(cg, &portRect, clipRgn);
    DisposeRgn(clipRgn);
  }
  CGContextTranslateCTM(cg, gdx, (float)(portRect.bottom - portRect.top - gdy));
  CGContextScaleCTM(cg, 1.0, -1.0 );
 
  if (clipping) {
    CGContextTranslateCTM(cg, auto_device_origin_x, auto_device_origin_y);
    clipping->Install((long)cg, AlignSmoothing());
    CGContextTranslateCTM(cg, -auto_device_origin_x, -auto_device_origin_y);
  }
  
  if (!AlignSmoothing()) {
    CGContextTranslateCTM(cg, device_origin_x, device_origin_y);
    CGContextScaleCTM(cg, user_scale_x, user_scale_y);
  }

  CGContextSetAlpha(cg, current_alpha);

  return cg;
}

Bool wxCanvasDC::AlignSmoothing()
{
  return (anti_alias == 2);
}

void wxCanvasDC::SetAlpha(double a)
{
  CGContextRef cg;

  wxbDC::SetAlpha(a);
  
  cg = cMacDC->GetCG(TRUE);
  
  if (cg)
    CGContextSetAlpha(cg, current_alpha);
}

double wxCanvasDC::GetPenSmoothingOffset()
{
  int pw;
  pw = current_pen->GetWidth();
  pw = (int)(user_scale_x * pw);
  if (!pw)
    pw = 1;
  return ((pw & 1) * 0.5);
}

double wxCanvasDC::SmoothingXFormX(double x)
{
  if (AlignSmoothing())
    return floor((x * user_scale_x) + device_origin_x) + GetPenSmoothingOffset();
  else
    return x;
}

double wxCanvasDC::SmoothingXFormY(double y)
{
  if (AlignSmoothing())
    return floor((y * user_scale_y) + device_origin_y) + GetPenSmoothingOffset();
  else
    return y;
}

double wxCanvasDC::SmoothingXFormW(double w, double x)
{
  if (AlignSmoothing())
    return SmoothingXFormX(x + w) - SmoothingXFormX(x);
  else
    return w;
}

double wxCanvasDC::SmoothingXFormH(double h, double y)
{
  if (AlignSmoothing())
    return SmoothingXFormY(y + h) - SmoothingXFormY(y);
  else
    return h;
}

double wxCanvasDC::SmoothingXFormXB(double x)
{
  if (AlignSmoothing())
    return floor((x * user_scale_x) + device_origin_x);
  else
    return x;
}

double wxCanvasDC::SmoothingXFormYB(double y)
{
  if (AlignSmoothing())
    return floor((y * user_scale_y) + device_origin_y);
  else
    return y;
}

double wxCanvasDC::SmoothingXFormWL(double w, double x)
{
  if (AlignSmoothing()) {
    w = SmoothingXFormW(w, x);
    if (w >= 1.0)
      return w - 1.0;
    else
      return w;
  } else
    return w;
}

double wxCanvasDC::SmoothingXFormHL(double h, double y)
{
  if (AlignSmoothing()) {
    h = SmoothingXFormH(h, y);
    if (h >= 1.0)
      return h - 1.0;
    else
      return h;
  } else
    return h;
}

/************************************************************************/
/*                                GL                                    */
/************************************************************************/

#include "../../../wxcommon/wxGLConfig.h"
#include "../../../wxcommon/wxGLConfig.cxx"

wxGL *wxCanvasDC::GetGL()
{
  if (!gl) {
    CGrafPtr cp;
    gl = new WXGC_PTRS wxGL();
    cp = cMacDC->macGrafPort();
    gl->Reset(gl_cfg, cp, 0, 0, 0);
    canvas->ResetGLView();
  }

  return gl;
}

static wxGL *current_gl_context = NULL;
#ifdef OS_X
static AGLContext dummy;
#endif

static AGLPixelFormat FindFormat(wxGLConfig *cfg, Bool offscreen)
{
  AGLPixelFormat fmt;
  GLint attrib[30];
  int a = 0, pre_samp;

  attrib[a++] = AGL_RGBA;

  if (offscreen)
    attrib[a++] = AGL_OFFSCREEN;
  else if (!cfg || cfg->doubleBuffered)
    attrib[a++] = AGL_DOUBLEBUFFER;
  
  if (offscreen) {
    attrib[a++] = AGL_PIXEL_SIZE;
    attrib[a++] = 32;
  }

  if (cfg && cfg->depth) {
    attrib[a++] = AGL_DEPTH_SIZE;
    attrib[a++] = cfg->depth;
  } else if (!cfg) {
    attrib[a++] = AGL_DEPTH_SIZE;
    attrib[a++] = 1;
  }

  if (cfg && cfg->stencil) {
    attrib[a++] = AGL_STENCIL_SIZE;
    attrib[a++] = cfg->stencil;
  }

  pre_samp = a;

  if (cfg && cfg->multisample) {
    attrib[a++] = AGL_SAMPLES_ARB;
    attrib[a++] = cfg->multisample;
    attrib[a++] = AGL_SAMPLE_BUFFERS_ARB;
    attrib[a++] = 1;
  }

  attrib[a] = AGL_NONE;

  fmt = aglChoosePixelFormat(NULL, 0, attrib);
  if (!fmt && (pre_samp < a)) {
    /* try without multisampling */
    attrib[pre_samp] = AGL_NONE;
    fmt = aglChoosePixelFormat(NULL, 0, attrib);
  }

  return fmt;
}

void wxInitGL()
{
  AGLPixelFormat fmt;

  fmt = FindFormat(NULL, 0);
  if (fmt) {
    dummy = aglCreateContext(fmt, NULL);
    aglSetCurrentContext(dummy);
  }

  wxREGGLOB(current_gl_context); 
}

wxGL::wxGL()
  : wxObject(WXGC_NO_CLEANUP)
{
}

void wxGL::Reset(wxGLConfig *cfg, CGrafPtr gp, int offscreen, int w, int h)
{
#ifdef OS_X
  AGLContext ctx; 

  ctx = (AGLContext)gl_ctx;
  if (gl_ctx) {
    if (this == current_gl_context) {
      aglSetCurrentContext(dummy);
    }
    
    aglSetDrawable(ctx, NULL);
    aglDestroyContext(ctx); 

    gl_ctx = 0;
  }

  if (gp) {
    if (offscreen) {
      /* Note: gp has been locked by LockPixels already */
      PixMapHandle pm;
      AGLPixelFormat fmt;
      fmt = FindFormat(cfg, 1);
      if (fmt) {
	pm = GetGWorldPixMap(gp);
	ctx = aglCreateContext(fmt, NULL);	
	if (ctx)
	  aglSetOffScreen(ctx, w, h, GetPixRowBytes(pm), GetPixBaseAddr(pm));
      }
    } else {
      AGLPixelFormat fmt;
      fmt = FindFormat(cfg, 0);
      if (fmt) {
	ctx = aglCreateContext(fmt, NULL);	
	if (ctx)
	  aglSetDrawable(ctx, gp);
      }
    }

    gl_ctx = (long)ctx;
    if (ctx && (current_gl_context == this)) {
      aglSetCurrentContext(ctx);
    }
  }
#endif
}

int wxGL::Ok()
{
  return !!gl_ctx;
}

void wxGL::SwapBuffers(void)
{
#ifdef OS_X
  if (gl_ctx) {
    aglSwapBuffers((AGLContext)gl_ctx);
  }
#endif
}

void wxGL::ThisContextCurrent(void)
{
#ifdef OS_X
  if (current_gl_context != this) {
    current_gl_context = this;
    if (gl_ctx) {
      aglSetCurrentContext((AGLContext)gl_ctx);
    }
  }
#endif
}

void wxGL::ResetGLView(int x, int y, int w, int h)
{
#ifdef OS_X
  GLint bufferRect[4];
  AGLContext ctx = (AGLContext)gl_ctx;
  
  if (ctx) {
    bufferRect[0] = x;
    bufferRect[1] = y;
    bufferRect[2] = w;
    bufferRect[3] = h;
  
    aglSetInteger(ctx, AGL_BUFFER_RECT, bufferRect);
    aglEnable(ctx, AGL_BUFFER_RECT);
    
    if (current_gl_context != this) {
      aglSetCurrentContext(ctx);
    }
    glViewport(0, 0, (GLsizei)w, (GLsizei)h);
    if (current_gl_context != this) {
      if (current_gl_context) {
	aglSetCurrentContext((AGLContext)current_gl_context->gl_ctx);
      } else {
	aglSetCurrentContext(dummy);
      }
    }
  }
#endif
}

void wxGLNoContext()
{
#ifdef OS_X
  current_gl_context = NULL;
  aglSetCurrentContext(dummy);
#endif
}

/************************************************************************/
/*                        Tab-drawing hack                              */
/************************************************************************/

void wxCanvasDC::DrawTabBase(double x, double y, double w, double h, int state)
{
  Rect r;

  SetCurrentDC();

  x += SetOriginX;
  y += SetOriginY;

  r.left = (int)x;
  r.right = (int)(x + w);
  r.top = (int)y;
  r.bottom = (int)(y + h);

  DrawThemeTabPane(&r, (ThemeDrawState)state);

  ReleaseCurrentDC();
}

typedef struct {
  wxFont *font;
  char *str;
} tab_info;

static void draw_tab_label(const Rect *bounds,
			   ThemeTabStyle style,
			   ThemeTabDirection direction,
			   SInt16 depth,
			   Boolean isColorDev,
			   UInt32 ti)
{
  wxFont *font = ((tab_info *)ti)->font;
  char *str = ((tab_info *)ti)->str;
  double w, h, d, dy, dx;

  font->GetTextExtent(str, 0, -1, &w, &h, &d, NULL, FALSE);

  dy = ((bounds->bottom - bounds->top - 3) - (h - d)) / 2;
  dx = ((bounds->right - bounds->left) - w) / 2;

  MoveTo((short)floor(bounds->left + dx), (short)floor(bounds->top + dy + (h - d)));
  wxDrawUnicodeText(str, 0, -1, 0);
}

void wxCanvasDC::DrawTab(char *str, double x, double y, double w, double h, int state)
{
  Rect rb;
  Bool focus_ring = 0;
  tab_info ti;

  if (state >= 100) {
    focus_ring = 1;
    state -= 100;
  }

  SetCurrentDC();

  wxMacSetCurrentTool(kTextTool);

  x += SetOriginX;
  y += SetOriginY;

  rb.left = (int)x;
  rb.right = (int)(x + w);
  rb.top = (int)y;
  rb.bottom = (int)(y + h);

  ti.font = font;
  ti.str = str;

  DrawThemeTab(&rb, state, kThemeTabNorth, draw_tab_label, (UInt32)&ti);

  if (focus_ring) {
    RgnHandle rgn;
    rgn = NewRgn();
    if (rgn) {
      GetThemeTabRegion(&rb, state, kThemeTabNorth, rgn);
      InsetRgn(rgn, 1, 1);
      DrawThemeFocusRegion(rgn, TRUE);
      DisposeRgn(rgn);
    }
  }

  ReleaseCurrentDC();
}

int wxCanvasDC::CacheFontMetricsKey()
{
  if ((user_scale_x == 1.0)
      && (user_scale_y == 1.0))
    return 1;
  return 0;
}
