
@INCLUDE prefix.xci

#include "wx_dccan.h"
#include "wx_dcmem.h"
#include "wx_dcps.h"
#include "wx_gdi.h"
#ifdef wx_msw
#include "wx_mf.h"
#endif
#include "wx_types.h"
#ifdef wx_mac
#include "wx_dcpr.h"
#endif
#include "wx_rgn.h"
#include "../../wxcommon/wxGLConfig.h"
#include <math.h>

#ifdef wx_msw
# define USE_GL
#endif
#ifdef wx_mac
# define USE_GL
#endif

#ifndef USE_GL
class wxGL : public wxObject {
public:
  wxGL();

  int Ok();

  void Reset(long d);
  void SwapBuffers(void);
  void ThisContextCurrent(void);
};

wxGL::wxGL()
: wxObject(WXGC_NO_CLEANUP)
{
}
int wxGL::Ok() { return 0; }
void wxGL::SwapBuffers(void) { }
void wxGL::ThisContextCurrent(void) { }
#endif

@INCLUDE wxs.xci

@HEADER

@BEGINSYMBOLS textMode > ONE > PRED
@SYM "transparent" : wxTRANSPARENT
@SYM "solid" : wxSOLID
@ENDSYMBOLS

@BEGINSYMBOLS bitmapDrawStyle > ONE > PRED BUNDLE
@SYM "solid" : wxSOLID
@SYM "opaque" : wxSTIPPLE
@SYM "xor" : wxXOR
@ENDSYMBOLS

@INCLUDE wxs_drws.xci

@MACRO CastToSO = (Scheme_Object*){x}
@MACRO CastFromSO = (void*){x}
@MACRO spAnything = _

static wxColour* dcGetTextBackground(wxDC *dc)
{
  wxColour *c = NULL, *bg = NULL;
  SETUP_VAR_STACK(2);
  VAR_STACK_PUSH(0, dc);
  VAR_STACK_PUSH(1, c);

  c = WITH_VAR_STACK(new wxColour());
#ifdef MZ_PRECISE_GC
  WITH_VAR_STACK(c->gcInit_wxColour());
#endif
  bg = WITH_VAR_STACK(dc->GetTextBackground());
  WITH_VAR_STACK(c->CopyFrom(bg));
  READY_TO_RETURN;
  return c;
}

static wxColour* dcGetTextForeground(wxDC *dc)
{
  wxColour *c = NULL, *fg = NULL;
  SETUP_VAR_STACK(2);
  VAR_STACK_PUSH(0, dc);
  VAR_STACK_PUSH(1, c);

  c = WITH_VAR_STACK(new wxColour());
#ifdef MZ_PRECISE_GC
  WITH_VAR_STACK(c->gcInit_wxColour());
#endif
  fg = WITH_VAR_STACK(dc->GetTextForeground());
  WITH_VAR_STACK(c->CopyFrom(fg));
  READY_TO_RETURN;
  return c;
}

static Bool DrawBitmap(wxDC *dc, wxBitmap *bm, double x, double y, int mode, wxColour *c, wxBitmap* mask)
{
  REMEMBER_VAR_STACK();
  if (bm->Ok()) {
    return WITH_REMEMBERED_STACK(dc->Blit(x, y, bm->GetWidth(), bm->GetHeight(), bm, 0, 0, mode, c, mask));
  } else
    return FALSE;
}

static Bool DrawBitmapRegion(wxDC *dc, wxBitmap *bm, double x, double y, double dx, double dy, double dw, double dh, int mode, wxColour *c, wxBitmap* mask)
{
  REMEMBER_VAR_STACK();
  if (bm->Ok()) {
    return WITH_REMEMBERED_STACK(dc->Blit(x, y, dw, dh, bm, dx, dy, mode, c, mask));
  } else
    return FALSE;
}

static void* MyTextExtent(wxDC *dc, mzchar *s, wxFont *f, Bool combine, int offset)
{
  double w, h, d, asc;
  Scheme_Object *a[4];
  void *r;
  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH_ARRAY(0, a, 4);

  a[0] = a[1] = a[2] = a[3] = NULL;

  WITH_VAR_STACK(dc->GetTextExtent((char *)s, &w, &h, &d, &asc, f, combine, TRUE, offset));
    
  a[0] = WITH_VAR_STACK(scheme_make_double(w));
  a[1] = WITH_VAR_STACK(scheme_make_double(h));
  a[2] = WITH_VAR_STACK(scheme_make_double(d));
  a[3] = WITH_VAR_STACK(scheme_make_double(asc));

  r = WITH_VAR_STACK(scheme_values(4, a));

  READY_TO_RETURN;

  return r;
}

static void* MyGetSize(wxDC *dc)
{
  double w, h;
  Scheme_Object *a[2];
  void *r;
  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH_ARRAY(0, a, 2);

  a[0] = a[1] = NULL;

  dc->GetSize(&w, &h); /* no GC possible */

  a[0] = WITH_VAR_STACK(scheme_make_double(w));
  a[1] = WITH_VAR_STACK(scheme_make_double(h));

  r = WITH_VAR_STACK(scheme_values(2, a));

  READY_TO_RETURN;

  return r;
}

static void* MyGetScale(wxDC *dc)
{
  double w, h;
  Scheme_Object *a[2];
  void *r;
  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH_ARRAY(0, a, 2);

  a[0] = a[1] = NULL;

  dc->GetUserScale(&w, &h); /* no GC possible */

  a[0] = WITH_VAR_STACK(scheme_make_double(w));
  a[1] = WITH_VAR_STACK(scheme_make_double(h));

  r = WITH_VAR_STACK(scheme_values(2, a));

  READY_TO_RETURN;

  return r;
}

static void* MyGetOrigin(wxDC *dc)
{
  double w, h;
  Scheme_Object *a[2];
  void *r;
  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH_ARRAY(0, a, 2);

  a[0] = a[1] = NULL;

  dc->GetDeviceOrigin(&w, &h); /* no GC possible */

  a[0] = WITH_VAR_STACK(scheme_make_double(w));
  a[1] = WITH_VAR_STACK(scheme_make_double(h));

  r = WITH_VAR_STACK(scheme_values(2, a));

  READY_TO_RETURN;

  return r;
}

inline static wxGL *_GetGL(wxDC *dc)
{
#ifdef USE_GL
  return dc->GetGL();
#else
  return NULL;
#endif
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif
static wxMemoryDC *make_memdc(void)
{
  return new wxMemoryDC(1);
}
#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

static wxMemoryDC *temp_mdc;

static wxMemoryDC *MakeDC(wxBitmap *src)
{
  wxMemoryDC *srcdc = NULL;
  SETUP_VAR_STACK(2);
  VAR_STACK_PUSH(0, src);
  VAR_STACK_PUSH(1, srcdc);

#ifdef wx_msw
  srcdc = (wxMemoryDC *)src->selectedInto;
  if (!srcdc) {
#endif
    if (!temp_mdc) {
      wxREGGLOB(temp_mdc);
      temp_mdc = WITH_VAR_STACK(make_memdc());
    }
    WITH_VAR_STACK(temp_mdc->SelectObject(src));
    srcdc = temp_mdc;
#ifdef wx_msw
  }
#endif

  READY_TO_RETURN;

  return srcdc;
}

static void UnmakeDC(wxMemoryDC *srcdc)
{
#ifdef wx_msw
  if (srcdc == temp_mdc)
#endif
    WITH_VAR_STACK(temp_mdc->SelectObject(NULL));
}

static void dcGetARGBPixels(wxMemoryDC *dc, double x, double y, int w, int h, char *s, Bool get_alpha)
{
  int i, j, p;
  unsigned char *ss = (unsigned char *)s;
  wxColour *c = NULL;
  double xs, ys, xo, yo;
  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH(0, ss);
  VAR_STACK_PUSH(1, c);
  VAR_STACK_PUSH(2, dc);

  dc->GetUserScale(&xs, &ys);
  dc->GetDeviceOrigin(&xo, &yo);
  p = 0;

  if (xs == 1 && ys == 1 && xo == 0 && yo == 0
      && WITH_VAR_STACK(dc->BeginGetPixelFast((int)x, (int)y, w, h))) {
    int xi = (int)x;
    int yi = (int)y;
    int r, g, b;
    if (!get_alpha) {
      for (j = 0; j < h; j++) {
	for (i = 0; i < w; i++) {
	  WITH_VAR_STACK(dc->GetPixelFast(xi + i, yi + j, &r, &g, &b));
	  ss[p++] = 255; /* alpha */
	  ss[p++] = r;
	  ss[p++] = g;
	  ss[p++] = b;
	}
      }
    } else {
      for (j = 0; j < h; j++) {
	for (i = 0; i < w; i++) {
	  WITH_VAR_STACK(dc->GetPixelFast(xi + i, yi + j, &r, &g, &b));
	  ss[p] = 255 - ((r + g + b) / 3);
	  p += 4;
	}
      }
    }
    WITH_VAR_STACK(dc->EndGetPixelFast());
  } else {
    c = new wxColour();

    if (!get_alpha) {
      for (j = 0; j < h; j++) {
	for (i = 0; i < w; i++) {
	  WITH_VAR_STACK(dc->GetPixel(x + i, y + j, c));
	  ss[p++] = 255; /* alpha */
	  ss[p++] = c->Red();
	  ss[p++] = c->Green();
	  ss[p++] = c->Blue();
	}
      }
    } else {
      for (j = 0; j < h; j++) {
	for (i = 0; i < w; i++) {
	  WITH_VAR_STACK(dc->GetPixel(x + i, y + j, c));
	  ss[p] = 255 - ((c->Red() + c->Green() + c->Blue()) / 3);
	  p += 4;
	}
      }
    }
  }

  READY_TO_RETURN;
}

void wxGetARGBPixels(wxBitmap *src, double x, double y, int w, int h, char *s, Bool get_alpha)
{
  wxMemoryDC *srcdc;
  SETUP_VAR_STACK(1);
  VAR_STACK_PUSH(0, srcdc);

  srcdc = WITH_VAR_STACK(MakeDC(src));
  WITH_VAR_STACK(dcGetARGBPixels(srcdc, x, y, w, h, (char *)s, get_alpha));
  WITH_VAR_STACK(UnmakeDC(srcdc));

  READY_TO_RETURN;
}

static void dcSetARGBPixels(wxMemoryDC *dc, double x, double y, int w, int h, char *s, Bool set_alpha)
{
  int i, j, p;
  unsigned char *ss = (unsigned char *)s;
  wxColour *c = NULL;
  double xs, ys, xo, yo;
  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH(0, ss);
  VAR_STACK_PUSH(1, c);
  VAR_STACK_PUSH(2, dc);

  dc->GetUserScale(&xs, &ys);
  dc->GetDeviceOrigin(&xo, &yo);
  p = 0;    

  if (xs == 1 && ys == 1 && xo == 0 && yo == 0
      && WITH_VAR_STACK(dc->BeginSetPixelFast((int)x, (int)y, w, h))) {
    int xi = (int)x;
    int yi = (int)y;
    if (!set_alpha) {
      for (j = 0; j < h; j++) {
	for (i = 0; i < w; i++) {
	  WITH_VAR_STACK(dc->SetPixelFast(xi + i, yi + j, ss[p+1], ss[p+2], ss[p+3]));
	  p += 4;
	}
      }
    } else {
      for (j = 0; j < h; j++) {
	for (i = 0; i < w; i++) {
	  WITH_VAR_STACK(dc->SetPixelFast(xi + i, yi + j, 255-ss[p], 255-ss[p], 255-ss[p]));
	  p += 4;
	}
      }
    }
    WITH_VAR_STACK(dc->EndSetPixelFast());
  } else {
    c = new wxColour();
  
    if (!set_alpha) {
      for (j = 0; j < h; j++) {
	for (i = 0; i < w; i++) {
	  WITH_VAR_STACK(c->Set(ss[p+1], ss[p+2], ss[p+3]));
	  WITH_VAR_STACK(dc->SetPixel(x + i, y + j, c));
	  p += 4;
	}
      }
    } else {
      for (j = 0; j < h; j++) {
	for (i = 0; i < w; i++) {
	  WITH_VAR_STACK(c->Set(255-ss[p], 255-ss[p], 255-ss[p]));
	  WITH_VAR_STACK(dc->SetPixel(x + i, y + j, c));
	  p += 4;
	}
      }
    }
  }

  READY_TO_RETURN;
}

static wxBitmap *dc_target(Scheme_Object *obj)
{
  wxDC *dc;
  dc = (wxDC *)((Scheme_Class_Object *)obj)->primdata;
  if (dc->__type == wxTYPE_DC_MEMORY) {
    wxBitmap *bm;
    bm = ((wxMemoryDC *)dc)->GetObject();
    if (bm)
      return bm;
  }
  return (wxBitmap *)0x1; /* dont't return NULL because that matches unspecified mask */
}

static inline double approx_dist(double x, double y) 
{
  x = fabs(x);
  y = fabs(y);
  return ((x < y) ? y : x);
}

static double my_round(double f)
/* doesn't have to deal with negtive numbers */
{
  double d, frac;
  
  frac = modf(f, &d);

  if (frac >= 0.5)  
    d += 1.0;

  return d;
}

static void ScaleSection(wxMemoryDC *dest, wxBitmap *src, 
			 double tx, double ty, double ww2, double hh2,
			 double fx, double fy, double ww, double hh,
			 wxBitmap *mask)
{
  double xs, ys, r, g, b, t, dx, dy, wt, si, sj, a, span;
  int i, j, starti, endi, startj, endj, p, xi, xj, sji, sii;
  int sbmw, sbmh, w, h, w2, h2, ispan, jspan;
  unsigned char *s = NULL, *s2 = NULL;
  wxMemoryDC *srcdc = NULL;
  SETUP_VAR_STACK(6);
  VAR_STACK_PUSH(0, s);
  VAR_STACK_PUSH(1, s2);
  VAR_STACK_PUSH(2, dest);
  VAR_STACK_PUSH(3, src);
  VAR_STACK_PUSH(4, srcdc);
  VAR_STACK_PUSH(5, mask);

  if (!dest->Ok())
    WITH_VAR_STACK(scheme_arg_mismatch(METHODNAME("bitmap-dc%","draw-bitmap-section-smooth"), 
				       "dc is not ok: ",
				       WITH_VAR_STACK(objscheme_bundle_wxMemoryDC(dest))));
  if (!src->Ok())
    WITH_VAR_STACK(scheme_arg_mismatch(METHODNAME("bitmap-dc%","draw-bitmap-section-smooth"), 
				       "source bitmap is not ok: ", 
				       WITH_VAR_STACK(objscheme_bundle_wxBitmap(src))));
  sbmw = WITH_VAR_STACK(src->GetWidth());
  sbmh = WITH_VAR_STACK(src->GetHeight());
  if (fx > sbmw)
    WITH_VAR_STACK(scheme_arg_mismatch(METHODNAME("bitmap-dc%","draw-bitmap-section-smooth"), 
				       "x offset too large for source bitmap: ", 
				       WITH_VAR_STACK(scheme_make_double(fx))));
  if (fy > sbmh)
    WITH_VAR_STACK(scheme_arg_mismatch(METHODNAME("bitmap-dc%","draw-bitmap-section-smooth"), 
				       "y offset too large for source bitmap: ", 
				       WITH_VAR_STACK(scheme_make_double(fy))));
  if ((fx + ww) > sbmw)
    WITH_VAR_STACK(scheme_arg_mismatch(METHODNAME("bitmap-dc%","draw-bitmap-section-smooth"), 
				       "x offset plus width too large for source bitmap: ", 
				       WITH_VAR_STACK(scheme_make_double(fx))));
  if ((fy + hh) > sbmh)
    WITH_VAR_STACK(scheme_arg_mismatch(METHODNAME("bitmap-dc%","draw-bitmap-section-smooth"), 
				       "y offset plus height too large for source bitmap: ", 
				       WITH_VAR_STACK(scheme_make_double(fy))));
  
  if (mask) {
    if (!mask->Ok()) {
      WITH_VAR_STACK(scheme_arg_mismatch(METHODNAME("bitmap-dc%","draw-bitmap-section-smooth"), 
					 "mask bitmap is not ok: ", 
					 WITH_VAR_STACK(objscheme_bundle_wxBitmap(mask))));
    }
    if ((WITH_VAR_STACK(mask->GetWidth()) != sbmw)
	|| (WITH_VAR_STACK(mask->GetHeight()) != sbmh))
      WITH_VAR_STACK(scheme_arg_mismatch(METHODNAME("bitmap-dc%","draw-bitmap-section-smooth"), 
					 "mask bitmap does not match source bitmap dimensions: ", 
					 WITH_VAR_STACK(objscheme_bundle_wxBitmap(mask))));
  }

  w = (int)(floor(ww + fx) - floor(fx));
  h = (int)(floor(hh + fy) - floor(fy));

  w2 = (int)(floor(ww2 + tx) - floor(tx));
  h2 = (int)(floor(hh2 + ty) - floor(ty));

  xs = (double)w2 / (double)w;
  ys = (double)h2 / (double)h;

  s = (unsigned char *)WITH_VAR_STACK(scheme_malloc_atomic(w * h * 4));
  s2 = (unsigned char *)WITH_VAR_STACK(scheme_malloc_atomic(w2 * h2 * 4));

  srcdc = WITH_VAR_STACK(MakeDC(src));
  WITH_VAR_STACK(dcGetARGBPixels(srcdc, fx, fy, w, h, (char *)s, 0));
  WITH_VAR_STACK(UnmakeDC(srcdc));

  if (mask) {
    srcdc = WITH_VAR_STACK(MakeDC(mask));
    WITH_VAR_STACK(dcGetARGBPixels(srcdc, fx, fy, w, h, (char *)s, 1));
    WITH_VAR_STACK(UnmakeDC(srcdc));

    WITH_VAR_STACK(dcGetARGBPixels(dest, tx, ty, w2, h2, (char *)s2, 0));
  }

  if (w <= w2)
    ispan = 0;
  else
    ispan = (w / w2) - 1;
  if (h <= h2)
    jspan = 0;
  else
    jspan = (h / h2) - 1;
  span = (((double)ispan + (double)jspan) / 2.0) + 0.001;

  for (j = 0; j < h2; j++) {
    sj = (double)j / ys;
    sji = (int)sj;
    startj = sji - (jspan >> 1);
    if (startj < 0)
      startj = 0;
    endj = sji + (jspan - (jspan >> 1)) + (((double)sji == sj) ? 0 : 1);
    if (endj >= h)
      endj = h - 1;
    
    for (i = 0; i < w2; i++) {
      si = (double)i / xs;
      sii = (int)si;
      starti = sii - (ispan >> 1);
      if (starti < 0)
	starti = 0;
      endi = sii + (ispan - (ispan >> 1)) + (((double)sii == si) ? 0 : 1);
      if (endi >= w)
	endi = w - 1;

      r = g = b = t = a = 0.0;

      for (xj = startj; xj <= endj; xj++) {
	dy = ((xj * ys) - j);
	for (xi = starti; xi <= endi; xi++) {
	  dx = ((xi * xs) - i);
	  wt = 1 / (span + approx_dist(dx, dy));
	  p = ((xj * w) + xi) * 4;
	  a += (wt * s[p]);
	  r += (wt * s[p+1]);
	  g += (wt * s[p+2]);
	  b += (wt * s[p+3]);
	  t += wt;
	}
      }

      r /= t;
      g /= t;
      b /= t;

      p = ((j * w2) + i) * 4;

      if (mask) {
	a /= (3.0 * 255.0 * t);
	r = (r * (1 - a)) + ((double)s2[p+1] * a);
	g = (g * (1 - a)) + ((double)s2[p+2] * a);
	b = (b * (1 - a)) + ((double)s2[p+3] * a);
      }

      s2[p+1] = (int)my_round(r);
      s2[p+2] = (int)my_round(g);
      s2[p+3] = (int)my_round(b);
    }
  }

  WITH_VAR_STACK(dcSetARGBPixels(dest, tx, ty, w2, h2, (char *)s2, 0));

#ifndef SENORA_GC_NO_FREE
  GC_free(s);
  GC_free(s2);
#endif

  READY_TO_RETURN;
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

static void SetPen(wxDC *dc, wxColour *c, double pw, int style)
{
  wxPenList *pl;
  wxPen *p;
  pl = wxThePenList;
  p = pl->FindOrCreatePen(c, pw, style);
  dc->SetPen(p);
}

static void SetPen(wxDC *dc, char *cname, double pw, int style)
{
  wxPenList *pl;
  wxPen *p;
  pl = wxThePenList;
  p = pl->FindOrCreatePen(cname, pw, style);
  if (p)
    dc->SetPen(p);
  else {
    scheme_arg_mismatch(METHODNAME("dc<%>", "set-pen"), "unknown color: ", scheme_make_utf8_string(cname));
  }
}

static void SetBrush(wxDC *dc, wxColour *c, int style)
{
  wxBrushList *bl;
  wxBrush *b;
  bl = wxTheBrushList;
  b = bl->FindOrCreateBrush(c, style);
  dc->SetBrush(b);
}

static void SetBrush(wxDC *dc, char *cname, int style)
{
  wxBrushList *bl;
  wxBrush *b;
  bl = wxTheBrushList;
  b = bl->FindOrCreateBrush(cname, style);
  if (b)
    dc->SetBrush(b);
  else {
    scheme_arg_mismatch(METHODNAME("dc<%>", "set-brush"), "unknown color: ", scheme_make_utf8_string(cname));
  }
}

void wxDrawTabBase(wxDC *dc, double x, double y, double w, double h, int state)
{
#ifdef wx_mac
  dc->DrawTabBase(x, y, w, h, state);
#endif
}

void wxDrawTab(wxDC *dc, char *s, double x, double y, double w, double h, int state)
{
#ifdef wx_mac
  dc->DrawTab(s, x, y, w, h, state);
#endif
}

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

@INCLUDE wxs_gdistyle.xci

@MACRO CheckStringIndex[n.s.i] = if (x<i> > SCHEME_CHAR_STRLEN_VAL(p[POFFSET+<s>])) WITH_VAR_STACK(scheme_arg_mismatch(METHODNAME("dc<%>",<n>), "string index too large: ", p[POFFSET+<i>]));

@CLASSBASE wxDC "dc":"object"
@INTERFACE "dc"

@CLASSID wxTYPE_DC

@SETMARK Q = U
@INCLUDE wxs_draw.xci

// Also in wxWindow:
@ m "get-text-extent" : void[]/CastToSO//spAnything MyTextExtent(mzstring,wxFont^=NULL,bool=FALSE,nnint=0); : : /CheckStringIndex["get-text-extent".0.3]|CheckOk[METHODNAME("dc<%>","get-text-extent")]
@ Q "get-char-height" : double GetCharHeight(); : : /CheckOk[METHODNAME("dc<%>","get-char-height")]
@ Q "get-char-width" : double GetCharWidth(); : : /CheckOk[METHODNAME("dc<%>","get-char-width")]

@MACRO rZERO = return 0;
@MACRO rFALSE = return FALSE;

#ifndef wx_mac
#define HIDETHISSTATEMENT(x) x
#else
#define HIDETHISSTATEMENT(x) 
#endif

@MACRO IFNOTMAC = HIDETHISSTATEMENT(
@MACRO ENDIF = )

#ifndef wx_mac
#define CHECKTHISONE(x) x
#else
#define CHECKTHISONE(x) 1
#endif

@MACRO CheckBMOk[p.who] = if (x<p> && !(x<p>->Ok())) WITH_VAR_STACK(scheme_arg_mismatch(<who>, "mask bitmap is not ok: ", p[POFFSET+<p>]));
@MACRO old_CheckBW[p.who] = if (x<p> && (x<p>->GetDepth() != 1)) WITH_VAR_STACK(scheme_arg_mismatch(<who>, "mask bitmap is not monochrome: ", p[POFFSET+<p>]));
@MACRO CheckBW[p.who] = 
@MACRO CheckSizes[p.m.who] = if (x<m> && ((x<p>->GetWidth() != x<m>->GetWidth()) || (x<p>->GetHeight() != x<m>->GetHeight()))) WITH_VAR_STACK(scheme_arg_mismatch(<who>, "mask bitmap size does not match bitmap to draw: ", p[POFFSET+<p>]));
@MACRO CheckNotSame[p.m.who] = if (WITH_VAR_STACK(dc_target(THEOBJ)) == x<p>) WITH_VAR_STACK(scheme_arg_mismatch(<who>, "source bitmap is the same as the destination: ", p[POFFSET+<p>])); if (WITH_VAR_STACK(dc_target(THEOBJ)) == x<m>) WITH_VAR_STACK(scheme_arg_mismatch(<who>, "mask bitmap is the same as the destination: ", p[POFFSET+<m>]));

@ m "draw-bitmap-section" : bool DrawBitmapRegion(wxBitmap!,double,double,double,double,nndouble,nndouble,SYM[bitmapDrawStyle]=wxSOLID,wxColour!=NULL,wxBitmap^=NULL); : : /CheckBMOk[9.METHODNAME("dc<%>","draw-bitmap-section")]|CheckBW[9.METHODNAME("dc<%>","draw-bitmap-section")]|CheckSizes[0.9.METHODNAME("dc<%>","draw-bitmap-section")]|CheckOk[METHODNAME("dc<%>","draw-bitmap-section")]|CheckNotSame[0.9.METHODNAME("dc<%>","draw-bitmap-section")] : : rFALSE <> with size
@ m "draw-bitmap" : bool DrawBitmap(wxBitmap!,double,double,SYM[bitmapDrawStyle]=wxSOLID,wxColour!=NULL,wxBitmap^=NULL); : : /CheckBMOk[5.METHODNAME("dc<%>","draw-bitmap")]|CheckBW[5.METHODNAME("dc<%>","draw-bitmap")]|CheckSizes[0.5.METHODNAME("dc<%>","draw-bitmap")]|CheckOk[METHODNAME("dc<%>","draw-bitmap")]|CheckNotSame[0.5.METHODNAME("dc<%>","draw-bitmap")]

@ Q "try-color" : void TryColour(wxColour!,wxColour!); : : /CheckOk[METHODNAME("dc<%>","try-color")]

@ Q "set-text-mode" : void SetBackgroundMode(SYM[textMode]); :  : /CheckOk[METHODNAME("dc<%>","set-text-mode")]
@ Q "set-scale" : void SetUserScale(nndouble,nndouble); : : /CheckOk[METHODNAME("dc<%>","set-scale")]
@ Q "set-origin" : void SetDeviceOrigin(double,double); : : /CheckOk[METHODNAME("dc<%>","set-origin")]

@ m "get-scale" : void[]/CastToSO//spAnything MyGetScale(); : : /CheckOk[METHODNAME("dc<%>","get-scale")]
@ m "get-origin" : void[]/CastToSO//spAnything MyGetOrigin(); : : /CheckOk[METHODNAME("dc<%>","get-origin")]

@ q "get-background" : wxColour! GetBackground(); : : /CheckOk[METHODNAME("dc<%>","get-background")]
@ q "get-text-mode" : SYM[textMode] GetBackgroundMode(); : : /CheckOk[METHODNAME("dc<%>","get-text-mode")]
@ q "get-brush" : wxBrush! GetBrush(); : : /CheckOk[METHODNAME("dc<%>","get-brush")]
@ q "get-font" : wxFont! GetFont(); : : /CheckOk[METHODNAME("dc<%>","get-font")]
@ q "get-pen" : wxPen! GetPen(); : : /CheckOk[METHODNAME("dc<%>","get-pen")]
@ m "get-text-background" : wxColour! dcGetTextBackground(); : : /CheckOk[METHODNAME("dc<%>","get-text-background")]
@ m "get-text-foreground" : wxColour! dcGetTextForeground(); : : /CheckOk[METHODNAME("dc<%>","get-text-foreground")]

@ m "get-size" : void[]/CastToSO//spAnything MyGetSize(); : : /CheckOk[METHODNAME("dc<%>","get-size")]

@ m "get-gl-context" : wxGL^ _GetGL();

@ q "ok?" : bool Ok();

@ Q "start-doc" : bool StartDoc(string); : : /CheckOk[METHODNAME("dc<%>","start-doc")] : rFALSE
@ Q "start-page" : void StartPage(); : : /CheckOk[METHODNAME("dc<%>","start-page")]
@ Q "end-doc" : void EndDoc(); : : /CheckOk[METHODNAME("dc<%>","end-doc-line")]
@ Q "end-page" : void EndPage(); : : /CheckOk[METHODNAME("dc<%>","end-page")]

@ "glyph-exists?" : bool GlyphAvailable(mzchar,wxFont^=NULL) : : /CheckOk[METHODNAME("dc<%>","glyph-exists?")]

@ "set-alpha" : void SetAlpha(rdouble[0|1]);
@ "get-alpha" : double GetAlpha();

@ "cache-font-metrics-key" : int CacheFontMetricsKey();

@END

@GLOBAL wxDCGlobal

@ "draw-tab-base" : void wxDrawTabBase(wxDC!, double, double, double, double, int);
@ "draw-tab" : void wxDrawTab(wxDC!, string, double, double, double, double, int);

@END

@MACRO STRINGENOUGH[who] = if (SCHEME_BYTE_STRTAG_VAL(p[4+POFFSET]) < (x2 * x3 * 4)) WITH_VAR_STACK(scheme_arg_mismatch(METHODNAME("bitmap-dc%",<who>), "byte string too short: ", p[4+POFFSET]));

@CLASSBASE wxMemoryDC "bitmap-dc":"dc"

@CLASSID wxTYPE_DC_MEMORY

@CREATOR ()
@ARGNAMES

@ "get-pixel" : bool GetPixel(double,double,wxColour!) : : /CheckOk[METHODNAME("bitmap-dc%","get-pixel")]
@ "set-pixel" : void SetPixel(double,double,wxColour!) : : /CheckOk[METHODNAME("bitmap-dc%","set-pixel")]

@ m "get-argb-pixels" : void dcGetARGBPixels(double,double,rint[0|10000],rint[0|10000],wbstring,bool=FALSE) : : /CheckOk[METHODNAME("bitmap-dc%","get-argb-pixels")]|STRINGENOUGH["get-argb-pixels"]
@ m "set-argb-pixels" : void dcSetARGBPixels(double,double,rint[0|10000],rint[0|10000],bstring,bool=FALSE) : : /CheckOk[METHODNAME("bitmap-dc%","set-argb-pixels")]|STRINGENOUGH["set-argb-pixels"]

@ m "draw-bitmap-section-smooth" : void ScaleSection(wxBitmap!,double,double,nndouble,nndouble,double,double,nndouble,nndouble,wxBitmap^=NULL)

@ "set-bitmap" : void SelectObject(wxBitmap^);  : : /CHECKOKFORDC[0.METHODNAME("bitmap-dc%","set-bitmap")]
@ "get-bitmap" : wxBitmap^ GetObject();

@END

@CLASSBASE wxPostScriptDC "post-script-dc":"dc"

@CLASSID wxTYPE_DC_POSTSCRIPT

@INCLUDE wxs_dorf.xci

@CREATOR (bool=TRUE,wxWindow^=NULL,bool=FALSE,bool=TRUE) : : /DLGORFRAME[1.METHODNAME("post-script-dc%","initialization")]
@ARGNAMES [interactive #t] [parent #f] [use-paper-bbox #f] [eps #t]

@END

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

#ifdef wx_x

class basePrinterDC : public wxObject
{
public:
  basePrinterDC(wxWindow *w);
};

basePrinterDC::basePrinterDC(wxWindow *)
{
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		   "%s", 
		   METHODNAME("printer-dc%","initialization")": not supported for X Windows");
}

#else

class basePrinterDC : public wxPrinterDC
{
public:
  basePrinterDC(wxWindow *w);
};

basePrinterDC::basePrinterDC(wxWindow *w) 
: wxPrinterDC( )
{
}

#endif

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

@CLASSBASE basePrinterDC "printer-dc":"dc"

@CLASSID wxTYPE_DC_PRINTER

@CREATOR (wxWindow^=NULL); : : /DLGORFRAME[0.METHODNAME("printer-dc%","initialization")]
@ARGNAMES [parent #f]

@END


#ifdef USE_GL
extern void *wxWithGLContext(wxGL *gl, void *thunk, void *alt_waitable, int eb);
#endif

static void *WithContext(wxGL *gl, void *thunk, void *alt_waitable, int eb)
{
#ifdef USE_GL
  return wxWithGLContext(gl, thunk, alt_waitable, eb);
#else
  return (void *)scheme_false;
#endif
}

@MACRO CheckGLOk[name] =  if (!((wxGL *)((Scheme_Class_Object *)THEOBJ)->primdata)->Ok()) WITH_VAR_STACK(scheme_arg_mismatch(<name>, "GL context is not ok: ", THEOBJ));


@CLASSBASE wxGL "gl-context" : "object"
@INTERFACE "gl-context"

@ "ok?" : bool Ok()
@ "swap-buffers" : void SwapBuffers() : : /CheckGLOk[METHODNAME("gl-context<%>","swap-buffers")]
@ m "call-as-current" : void[]/CastToSO//spAnything WithContext(void[]/CastToSO/CastFromSO/spAnything///push,void[]=NULL/CastToSO/CastFromSO/spAnything///push,bool=0) : : /CheckGLOk[METHODNAME("gl-context<%>","swap-buffers")]

@END


@CLASSBASE wxGLConfig "gl-config" : "object"

@CREATOR ()

@IVAR "double-buffered" : bool doubleBuffered
@IVAR "stereo" : bool stereo
@IVAR "stencil-size" : rint[0|256] stencil
@IVAR "accum-size" : rint[0|256] accum
@IVAR "depth-size" : rint[0|256] depth
@IVAR "multisample-size" : rint[0|256] multisample

@END


#if 0

#ifdef wx_msw

class baseMetaFileDC : public wxMetaFileDC {
public:
  baseMetaFileDC(char *s = NULL);

  baseMetaFile* baseClose() { return (baseMetaFile *)Close(); }
};

baseMetaFileDC::baseMetaFileDC(char *s)
    : wxMetaFileDC(s)
{
}

#else

class baseMetaFileDC : public wxObject 
{
public:
  baseMetaFileDC(char * = NULL) {
    scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		     "%s", 
		     METHODNAME("meta-file-dc%","initialization")": only supported for Windows");
  }

  baseMetaFile* baseClose() { return NULL; }
};

#endif

@CLASSBASE baseMetaFileDC "meta-file-dc":"dc"

@CLASSID wxTYPE_DC_METAFILE

@CREATOR (string=NULL);
@ARGNAMES [filename #f]

@ "close" : baseMetaFile! baseClose()

@END

#endif
