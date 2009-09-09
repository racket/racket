///////////////////////////////////////////////////////////////////////////////
// File:	wx_dccan3.cc
// Purpose:	Canvas device context implementation (Macintosh version) (part 3)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2009 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "common.h"
#include "wx_dccan.h"
#include "wx_utils.h"
#include "scheme.h"

extern CGrafPtr wxMainColormap;

extern "C" {
  int scheme_utf8_decode(const unsigned char *s, int start, int len, 
			 unsigned int *us, int dstart, int dlen,
			 long *ipos, char utf16, int permissive);
};

static ATSUStyle theATSUstyle, theATSUqdstyle;

#define MAX_WIDTH_MAPPINGS 1024
static Scheme_Hash_Table *style_table, *width_table, *old_width_table;
static Scheme_Object *table_key;

typedef struct {
  double scale_x, scale_y, angle;
  int code;
  short txFont, txSize, txFace;
  char use_cgctx, smoothing;
} wxKey;

typedef void (*atomic_timeout_t)(void);

static atomic_timeout_t pre_scheme()
{
  atomic_timeout_t old;

  old = scheme_on_atomic_timeout;
  scheme_on_atomic_timeout = NULL;
  scheme_start_atomic();
  scheme_current_thread->suspend_break++;
  return old;
}

static void post_scheme(atomic_timeout_t old)
{
  --scheme_current_thread->suspend_break;
  scheme_end_atomic_no_swap();
  scheme_on_atomic_timeout = old;
}

Scheme_Object *lookup_width(Scheme_Object *table_key)
{
  Scheme_Object *val;
  val = scheme_hash_get(width_table, table_key);
  if (!val) {
    val = scheme_hash_get(old_width_table, table_key);
    if (val) {
      /* Move it to the new table, so we find it faster, and
	 so it's kept on the next rotation: */
      Scheme_Object *new_key;
      new_key = scheme_make_sized_byte_string(SCHEME_BYTE_STR_VAL(table_key), sizeof(wxKey), 1);
      scheme_hash_set(width_table, new_key, val);
      scheme_hash_set(old_width_table, table_key, NULL);
    }
  }
  return val;
}

static void init_ATSU_style(void);
static OSStatus atsuSetStyleFromGrafPtrParams( ATSUStyle iStyle, short txFont, short txSize, SInt16 txFace, int smoothing, 
					       double angle, double scale_y, int qd_spacing);
static double DrawMeasUnicodeText(const char *text, int d, int theStrlen, int ucs4,
				  int just_meas, int given_font, 
				  short txFont, short txSize, short txFace,
				  int again, int qd_spacing, int smoothing,
				  double angle, int sym_map,
				  double scale_x, double scale_y,
				  double pen_delta_x, int with_delta,
				  double pen_start_x, double pen_start_y, double ddx, double ddy, int with_start,
                                  double current_alpha);

#ifndef DoubleToFixed
# define DoubleToFixed(a) ((Fixed)((double) (a) * fixed1)) 
#endif

static int symbol_map[] = { 0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 8704, 0, 8707, 0, 0, 8717,
			    0, 0, 8727, 0, 0, 8722, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    8773, 913, 914, 935, 916, 917, 934, 915,
			    919, 921, 977, 922, 923, 924, 925, 927,
			    928, 920, 929, 931, 932, 933, 962, 937,
			    926, 936, 918, 0, 8756, 0, 8869, 0,
			    0, 945, 946, 967, 948, 949, 966, 947,
			    951, 953, 981, 954, 955, 956, 957, 959,
			    960, 952, 961, 963, 964, 965, 982, 969,
			    958, 968, 950, 0, 0, 0, 8764, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 978, 8242, 8804, 8260, 8734, 402, 9827,
			    9830, 9829, 9824, 8596, 8592, 8593, 8594, 8595,
			    0, 177, 8243, 8805, 215, 8733, 8706, 8729,
			    247, 8800, 8801, 8776, 8230, 9168, 9135, 8629,
			    8501, 8465, 8476, 8472, 8855, 8853, 8709, 8745,
			    8746, 8835, 8839, 8836, 8834, 8838, 8712, 8713,
			    8736, 8711, 174, 169, 8482, 8719, 8730, 8901,
			    172, 8743, 8744, 8660, 8656, 8657, 8658, 8659,
			    9674, 9001, 174, 169, 8482, 8721, 9115, 9116,
			    9117, 9121, 9122, 9123, 9127, 9128, 9129, 9130,
			    8364, 9002, 8747, 8992, 9134, 8993, 9118, 9119,
			    9120, 9124, 9125, 9126, 9131, 9132, 9133, 0 };

static int always_use_atsu = 1;
# define ALWAYS_USE_ATSU always_use_atsu

#define QUICK_UBUF_SIZE 512
static UniChar u_buf[QUICK_UBUF_SIZE];
static double widths_buf[QUICK_UBUF_SIZE];
static ATSUTextLayout layout_buf[QUICK_UBUF_SIZE];
static int glyphs_buf[QUICK_UBUF_SIZE];
static CGGlyph cgglyphs_buf[QUICK_UBUF_SIZE];
static CGSize sizes_buf[QUICK_UBUF_SIZE];

static CGFontRef prev_cgf;
static short cgf_txFont, cgf_txFace;

#if 0
/* This undocumented Quartz function used to control how fonts are 
   anti-aliased. (I discovered it by running `nm' on the "QD" framework.)
   Mode 0 was normal anti-aliasing, mode 1 was no anti-aliasing, and mode 2 was
   4-bit pixel-aligned anti-aliasing (the old QuickDraw standard). 
   But with 10.5, mode 2 stopped working; with 10.6, this function was
   replaced by CGContextSetFontRenderingStyle --- which is also undocumented,
   and I didn't manage to guess how it works, and it's probably not in older
   versions anyway. */
extern "C" void CGContextSetFontRenderingMode(CGContextRef cg, int v);
#endif

//-----------------------------------------------------------------------------

static RgnHandle GetCurrentClipRgn(CGrafPtr qdp)
{
  RgnHandle clipRgn;

  clipRgn = NewRgn();
  if (clipRgn) {
    RgnHandle visRgn;
    visRgn = NewRgn();
    if (visRgn) {
      GetPortClipRegion(qdp, clipRgn);
      GetPortVisibleRegion(qdp, visRgn);
      SectRgn(clipRgn, visRgn, clipRgn);
      DisposeRgn(visRgn);
    }
  }

  return clipRgn;
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawText(const char* text, double x, double y, Bool combine, Bool ucs4, int d, double angle)
{
  FontInfo fontInfo;
  double w;

  if (!Ok()) return;

  /* If characters should not be combined (i.e., they should be drawn
     separately), then try fast mode, using size and glyph information
     gleaned from ATSUI on a previous round. We can save a factor of
     10 for all but the smallest strings by avoiding ATSUI this
     time. */
  if (!combine
      && ucs4 
      && (angle == 0.0) 
      && table_key
      && (font->GetFamily() != wxSYMBOL)
      && (current_bk_mode == wxTRANSPARENT)
      && Colour
      && scheme_current_thread) {
    int i;
    unsigned int *s = (unsigned int *)text;
    wxKey *k = (wxKey *)SCHEME_BYTE_STR_VAL(table_key);
    int ulen;
    atomic_timeout_t old;
    double one_res;
    CGSize *sizes;
    CGGlyph *cgglyphs;
    int glyph;
    Scheme_Object *val;
    int smoothing, use_cgctx;
    
    smoothing = font->GetEffectiveSmoothing(user_scale_y);
    use_cgctx = (always_use_atsu 
		 && ((smoothing != wxSMOOTHING_PARTIAL) || (user_scale_x != user_scale_y)));

    k->scale_x = user_scale_x;
    k->scale_y = user_scale_y;
    k->angle = angle;
    i = font->GetMacFontNum();
    k->txFont = i;
    i = font->GetPointSize();
    k->txSize = i;
    i = font->GetMacFontStyle();
    k->txFace = i;
    k->use_cgctx = (char)use_cgctx;
    k->smoothing = (char)smoothing;

    for (i = d; s[i]; i++) { }
    ulen = i - d;
    if (ulen > QUICK_UBUF_SIZE) {
      sizes = (CGSize *)(new WXGC_ATOMIC char[ulen * sizeof(CGSize)]);
      cgglyphs = (CGGlyph *)(new WXGC_ATOMIC char[ulen * sizeof(CGGlyph)]);
    } else {
      sizes = sizes_buf;
      cgglyphs = cgglyphs_buf;
    }

    old = pre_scheme();

    /* Check whether all of the characters have known widths and known
       glyphs: */ 
    for (i = d; s[i]; i++) {
      k->code = s[i];
      val = lookup_width(table_key);
      if (!val)
	break;
      glyph = SCHEME_INT_VAL(SCHEME_VEC_ELS(val)[2]);
      if (glyph < 0)
	break;
      one_res = SCHEME_DBL_VAL(SCHEME_VEC_ELS(val)[0]);
      if (!use_cgctx)
	one_res = one_res / user_scale_y;
      sizes[i-d].width = one_res;
      sizes[i-d].height = 0;
      cgglyphs[i-d] = (CGGlyph)glyph;
    }

    post_scheme(old);

    if (!s[i]) {
      /* Yes, all widths and glyphs are known. Use Quartz directly. */
      FMFont fnt;
      FMFontStyle intrinsic;
      int done = 0;
      FontInfo fontInfo;
      CGFontRef cgf;

      wxTextFontInfo(k->txFont, k->txSize, k->txFace, &fontInfo, NULL, 0, 0);

      if (prev_cgf
	  && (cgf_txFont == k->txFont)
	  && (cgf_txFace == k->txFace)) {
	cgf = prev_cgf;
      } else {
	if (FMGetFontFromFontFamilyInstance(k->txFont,
					    k->txFace,
					    &fnt,
					    &intrinsic)
	    == noErr) {
	  short face = k->txFace;

	  face &= ~intrinsic;
	  if (!face) {
	    ATSFontRef ats;
	    ats = FMGetATSFontRefFromFont(fnt);
	    cgf = CGFontCreateWithPlatformFont(&ats);
	  } else {
	    /* After all this, we give up and let the OS take care of bolding
	       or italicing the font. */
	    cgf = NULL;
	  }
	} else
	  cgf = NULL;
      }
	  
      if (cgf) {
	CGContextRef cg;
	CGrafPtr qdp;
	Rect portRect;

	SetCurrentDC(TRUE);
	cg = GetCG();
	
	CGContextSaveGState(cg);
      
	CGContextSetFont(cg, cgf);
	CGContextSetFontSize(cg, k->txSize);

	{
	  int red, green, blue;

	  red = current_text_foreground->Red();
	  green = current_text_foreground->Green();
	  blue = current_text_foreground->Blue();

	  CGContextSetRGBFillColor(cg, 
				   (double)red / 255.0,
				   (double)green / 255.0,
				   (double)blue / 255.0,
				   1.0);
	}

	if (smoothing == wxSMOOTHING_OFF)
	  CGContextSetShouldAntialias(cg, FALSE);
#if 0
	else if (smoothing == wxSMOOTHING_PARTIAL)
	  CGContextSetFontRenderingMode(cg, 2);
#endif

	qdp = cMacDC->macGrafPort();
	SyncCGContextOriginWithPort(cg, qdp);
	GetPortBounds(qdp, &portRect);
	CGContextTranslateCTM(cg, 
			      gdx + (x * user_scale_x) + device_origin_x, 
			      (portRect.bottom - portRect.top) 
			      - (gdy + ((y  + fontInfo.ascent) * user_scale_y) + device_origin_y));
	CGContextScaleCTM(cg, user_scale_x, user_scale_y);
	  
	CGContextSetTextPosition(cg, 0, 0);
	CGContextShowGlyphsWithAdvances(cg, cgglyphs, sizes, ulen);

	if (prev_cgf && (prev_cgf != cgf))
	  CGFontRelease(prev_cgf);
	prev_cgf = cgf;
	cgf_txFont = k->txFont;
	cgf_txFace = k->txFace;

	CGContextRestoreGState(cg);
	
	ReleaseCurrentDC();

	done = 1;
      }

      if (done)
	return;
    }
  }

  /* Fall back to default mode. */

  SetCurrentDC();

  wxMacSetCurrentTool(kTextTool);

  ::GetFontInfo(&fontInfo);

  {
    int smoothing;
    smoothing = font->GetEffectiveSmoothing(user_scale_y);
    if (!Colour)
      smoothing = wxSMOOTHING_OFF;
    w = wxDrawUnicodeText(text, d, -1, ucs4, 
			  !combine, smoothing, angle,
			  user_scale_x, user_scale_y,
			  1,
			  x + (fontInfo.ascent * sin(angle)) - logical_origin_x,
			  y + (fontInfo.ascent * cos(angle)) - logical_origin_y, 
			  device_origin_x + SetOriginX,
			  device_origin_y + SetOriginY,
			  font->GetFamily(),
                          current_alpha);
  }
  
  ReleaseCurrentDC();
}

//-----------------------------------------------------------------------------
double wxCanvasDC::GetCharHeight(void)
     //-----------------------------------------------------------------------------
{
  int theCharHeight;
  if (font) {
    /* We provide the scale only for font selection. 
       The result is unscaled (as we need it). */
    theCharHeight = (int)font->GetCharHeight(user_scale_x, user_scale_y);
  } else
    theCharHeight = 12;

  return theCharHeight;
}

//-----------------------------------------------------------------------------
double wxCanvasDC::GetCharWidth(void)
     //-----------------------------------------------------------------------------
{
  int theCharWidth;
  if (font) {
    /* We provide the scale only for font selection.
       The result is unscaled (as we need it). */
    theCharWidth = (int)font->GetCharWidth(user_scale_x, user_scale_y);
  } else
    theCharWidth = 12;

  return theCharWidth;
}

//-----------------------------------------------------------------------------
void wxCanvasDC::GetTextExtent(const char* string, double* x, double* y, double* descent,
			       double* internalLeading, wxFont* the_font, 
			       Bool combine, Bool ucs4, int d, int slen)
{
  double x2, y2, descent2, externalLeading2;

  /* Note: extent result is unscaled. We provide scales only in case it matters
     in font selection. */

  if (the_font)
    the_font->GetTextExtent((char *)string, d, slen, &x2, &y2, &descent2, &externalLeading2, 
			    !combine, ucs4, 
			    user_scale_x, user_scale_y);
  else if (font)
    font->GetTextExtent((char *)string, d, slen, &x2, &y2, &descent2, &externalLeading2, 
			!combine, ucs4,
			user_scale_x, user_scale_y);
  else {
    *x = -1;
    *y = -1;
    if (descent) *descent = 0.0;
    if (internalLeading) *internalLeading = 0.0;
    return;
  }

  *x = x2;
  *y = y2;
  if (descent) *descent = descent2;
  if (internalLeading) *internalLeading = 0.0;
}

/*****************************************************************************/
/*                              ATSU-based text                              */
/*****************************************************************************/

//----------------------------------------------------------------------

void wxCheckATSUCapability()
{
  /* Disable always_use_atsu if the part we need isn't there */
  SInt32 res;
  Gestalt(gestaltATSUVersion, &res);
  if (res <  (7 << 16) /* gestaltATSUUpdate6 */)
    always_use_atsu = 0;
}

double wxDrawUnicodeText(const char *text, int d, int theStrlen, int ucs4, Bool qd_spacing, int smoothing, double angle,
			 double scale_x, double scale_y, int use_start, double start_x, double start_y, double ddx, double ddy,
			 int is_sym, double current_alpha)
{
  int i;
  int again = 0;
  double pen_delta = 0.0;
  int move_pen_at_end;

  if (theStrlen < 0) {
    if (ucs4) {
      int *t2 = (int *)text;
      for (theStrlen = d; t2[theStrlen]; theStrlen++) {
      }
      theStrlen -= d;
    } else
      theStrlen = strlen(text XFORM_OK_PLUS d);
  }

  move_pen_at_end = qd_spacing && ALWAYS_USE_ATSU && !use_start;

  while (theStrlen) {
    /* Check whether we need to go into Unicode mode: */
    if (!qd_spacing || ALWAYS_USE_ATSU || ucs4) {
      i = 0;
    } else if (is_sym == wxSYMBOL)
      /* Symbol font hack: don't convert */
      i = theStrlen;
    else {
      for (i = 0; i < theStrlen; i++) {
	if (((unsigned char *)text)[i + d] > 127)
	  break;
      }
    }

    if (i) {
      /* Up to i, it's all ASCII, where MacRoman == UTF-8 */
      int reset_size = 0;
      Point pen_start, pen_end;

      if (scale_y != 1.0) {
	GrafPtr iGrafPtr;
	int ssize;

	GetPort( &iGrafPtr );
	reset_size = GetPortTextSize(iGrafPtr);
	ssize = (int)floor(scale_y * reset_size);
	if (!ssize)
	  ssize = 1;
	::TextSize(ssize);
      }

      if (use_start) {
	MoveTo((short)floor((start_x * scale_x) + ddx), 
	       (short)floor((start_y * scale_y) + ddy));
	use_start = 0;
      }
      ::GetPen(&pen_start);

      ::DrawText(text XFORM_OK_PLUS d, 0, i);

      if (reset_size)
	::TextSize(reset_size);

      ::GetPen(&pen_end);
      pen_delta += (pen_end.h - pen_start.h);

      d += i;
      theStrlen -= i;
    }

    if (theStrlen) {
      int amt;

      if (!qd_spacing || ALWAYS_USE_ATSU || ucs4)
	amt = theStrlen;
      else
	amt = 1;

      pen_delta += DrawMeasUnicodeText(text, d, amt, ucs4, 0, 0, 0, 0, 0, again, 
				       qd_spacing, smoothing, angle, is_sym,
				       scale_x, scale_y,
				       pen_delta, move_pen_at_end || use_start,
				       start_x, start_y, ddx, ddy, use_start,
                                       current_alpha);
	  
      d += amt;
      theStrlen -= amt;
      again = 1;
    }
  }

  if (move_pen_at_end) {
    Point start;
    GetPen(&start);
    MoveTo(start.h + (int)floor(pen_delta * scale_x), start.v);
  }

  return pen_delta;
}

void wxGetUnicodeTextWidth(const char *text, int d, int theStrlen, 
			   short txFont, short txSize, short txFace,
			   int ucs4, double scale_y,
			   double* x, double* y,
			   double* descent, double* externalLeading,
			   Bool qd_spacing, double scale_x,
			   int is_sym)
{
  FontInfo fontInfo;
  const char *meas = NULL;
  int i;

  if (text) {
    if (theStrlen < 0) {
      if (ucs4) {
	int *t2 = (int *)text;
	for (theStrlen = d; t2[theStrlen]; theStrlen++) {
	}
	theStrlen -= d;
      } else
	theStrlen = strlen(text XFORM_OK_PLUS d);
    }

    if (!qd_spacing || ALWAYS_USE_ATSU || ucs4) {
      i = 0;
    } else {
      if (is_sym != wxSYMBOL) {
	/* Check whether we need to go into Unicode mode to get UTF-8 output: */
	for (i = 0; i < theStrlen; i++) {
	  if (((unsigned char *)text)[i + d] > 127)
	    break;
	}
      } else
	/* Symbol font hack: don't convert */
	i = theStrlen;
    }
    
    if (i >= theStrlen) {
      meas = text;
    }
  } else
    theStrlen = 0;

  /* gets ascent, etc., and gets width if meas is non-NULL: */
  {
    double dx;
    dx = wxTextFontInfo(txFont, txSize, txFace,
			&fontInfo, (char *)meas, 
			d, theStrlen);
    *x = dx;
  }

  if (meas) {
    /* it's all ASCII, where MacRoman == UTF-8 */
    /* so *x is right */
  } else if (text) {
    if (!qd_spacing || ALWAYS_USE_ATSU || ucs4) {
      double dx;
      dx = DrawMeasUnicodeText(text, d, theStrlen, ucs4,
			       1, 1, 
			       txFont, txSize, txFace,
			       0, qd_spacing, wxSMOOTHING_DEFAULT, 0.0, is_sym,
			       scale_x, scale_y,
			       0.0, 0, 0.0, 0.0, 0.0, 0.0, 0, 0.0);
      *x = dx;
    } else {
      /* Need to split the string into parts */
      int again = 0;
      *x = 0;
      while (theStrlen) {
	if (ALWAYS_USE_ATSU) {
	  i = 0;
	} else {
	  for (i = 0; i < theStrlen; i++) {
	    if (((unsigned char *)text)[i + d] > 127)
	      break;
	  }
	}

	/* Measure the leading ASCII part, if any: */
	if (i) {
	  (*x) += wxTextFontInfo(txFont, txSize, txFace,
				 &fontInfo, 
				 (char *)text, d, i);
	  d += i;
	  theStrlen -= i;
	}

	/* Measure one Latin-1 part: */
	if (theStrlen) {
	  int amt;

	  amt = 1;
      
	  (*x) += DrawMeasUnicodeText(text, d, amt, ucs4,
				      1, 1, 
				      txFont, txSize, txFace,
				      again, qd_spacing,
				      wxSMOOTHING_DEFAULT, 0.0, is_sym,
				      scale_x, scale_y, 
				      0.0, 0, 0.0, 0.0, 0.0, 0.0, 0, 0.0);
	  d += amt;
	  theStrlen -= amt;
	  again = 1;
	}
      }
    }
  }

  *y = fontInfo.ascent + fontInfo.descent; // height
  if (descent) *descent = fontInfo.descent;
  if (externalLeading) *externalLeading = fontInfo.leading;
}

Bool wxGetUnicodeGlyphAvailable(int c, 
				short txFont, short txSize, short txFace,
				int is_sym)
{
  ATSUTextLayout layout;
  UniChar uc[1];
  UniCharArrayOffset ulen = 1, changed;
  ATSUFontID fontid;
  UniCharCount changedLen;
  OSStatus r;

  if (!theATSUstyle)
    init_ATSU_style();

  if (c > 0xFFFF)
    return FALSE;

  atsuSetStyleFromGrafPtrParams(theATSUstyle, txFont, txSize, txFace, 1, 0.0, 1.0, 1);

  uc[0] = c;
  ATSUCreateTextLayoutWithTextPtr((UniCharArrayPtr)uc,
				  kATSUFromTextBeginning,
				  kATSUToTextEnd,
				  ulen,
				  1,
				  &ulen,
				  &theATSUstyle,
				  &layout);

  
  r = ATSUMatchFontsToText (layout,
			    kATSUFromTextBeginning,
			    kATSUToTextEnd,
			    &fontid,
			    &changed,
			    &changedLen);

  ATSUDisposeTextLayout(layout);
  
  return (r != kATSUFontsNotMatched);
}

#if 0
static long time_preprocess, time_ctx, time_style, time_layout, time_measure, time_draw;
static long time_counter, time_start;
#define START_TIME time_start = scheme_get_process_milliseconds()
#define END_TIME(x) time_ ## x += (scheme_get_process_milliseconds() - time_start)
#else
#define START_TIME /* empty */
#define END_TIME(x) /* empty */
#endif

static double DrawMeasUnicodeText(const char *text, int d, int theStrlen, int ucs4,
				  int just_meas, int given_font, 
				  short txFont, short txSize, short txFace,
				  int again, int qd_spacing, int smoothing,
				  double angle, int is_sym,
				  double scale_x, double scale_y,
				  double pen_delta, int use_pen_delta,
				  double start_x, double start_y, double ddx, double ddy, int with_start,
                                  double current_alpha)
{
  ATSUTextLayout layout = NULL, *layouts;
  UniCharCount ulen, one_ulen, delta;
  UniChar *unicode;
  double result = 0, one_res = 0;
  int need_convert, need_layout, need_size, textMode = 0;
  Scheme_Object *val;
#define JUSTDELTA(v, s, d) (need_convert ? v : ((v - d) / s))
#define COORDCONV(v, s, d) (need_convert ? ((v * s) + d) : v)
  CGrafPtr qdp;
  CGContextRef cgctx;
  Rect portRect;
  RGBColor eraseColor, textColor;
  ATSUStyle style;
  Point start;
  RgnHandle clipRgn;
  double *widths;
  int use_cgctx = (always_use_atsu 
		   && ((smoothing != wxSMOOTHING_PARTIAL) || (scale_x != scale_y)));
  int use_cache = (qd_spacing && scheme_current_thread);
  int *glyphs;
  FontInfo fontInfo;
	
  if (!theATSUstyle)
    init_ATSU_style();

  START_TIME;

  /****************************************/
  /* Unicode conversion                  */

  if (ucs4) {
    int i, extra;
    unsigned int v;
    UniCharCount alloc_ulen;

    /* Count characters that fall outside UCS-2: */
    for (i = 0, extra = 0; i < theStrlen; i++) {
      if (((unsigned int *)text)[d+i] > 0xFFFF)
	extra++;
    }

    ulen = theStrlen + extra;
    alloc_ulen = ulen;
    if (alloc_ulen > QUICK_UBUF_SIZE)
      unicode = new WXGC_ATOMIC UniChar[alloc_ulen];
    else
      unicode = u_buf;
    
    /* UCS-4 -> UTF-16 conversion */
    for (i = 0, extra = 0; i < theStrlen; i++) {
      v = ((unsigned int *)text)[d+i];
      if (v > 0xFFFF) {
	v -= 0x10000;
	unicode[i+extra] = 0xD800 | ((v >> 10) & 0x3FF);
	extra++;
	unicode[i+extra] = 0xDC00 | (v & 0x3FF);
      } else
	unicode[i+extra] = v;
    }
  } else {
    UniCharCount alloc_ulen;

    /* UTF-8 -> UTF-16 conversion */
    ulen = scheme_utf8_decode((unsigned char *)text, d, 
			      theStrlen, NULL, 0, -1, 
			      NULL, 1 /*UTF-16*/, '?');
    alloc_ulen = ulen;
    if (alloc_ulen > QUICK_UBUF_SIZE)
      unicode = new WXGC_ATOMIC UniChar[alloc_ulen];
    else
      unicode = u_buf;
    ulen = scheme_utf8_decode((unsigned char *)text, d, theStrlen, 
			      (unsigned int *)unicode, 0, -1, 
			      NULL, 1 /*UTF-16*/, '?');
  }

  if (is_sym == wxSYMBOL) {
    unsigned int i;
    int v, m;
    for (i = 0; i < ulen; i++) {
      v = unicode[d+i];
      if (v < 256) {
	m = symbol_map[v];
	unicode[i] = (m ? m : v);
      }
    }
  }

  END_TIME(preprocess);
  START_TIME;

  GetPort(&qdp);

  /****************************************/
  /* Set up measure cache                 */

  if (!width_table) {
    char *s;

    wxREGGLOB(style_table);
    wxREGGLOB(width_table);
    wxREGGLOB(old_width_table);
    wxREGGLOB(table_key);

    style_table = scheme_make_hash_table_equal();
    width_table = scheme_make_hash_table_equal();
    old_width_table = scheme_make_hash_table_equal();
    s = new WXGC_ATOMIC char[sizeof(wxKey)];
    memset(s, 0, sizeof(wxKey));
    table_key = scheme_make_sized_byte_string(s, sizeof(wxKey), 0);
  }
  if (!given_font) {
    txFont = GetPortTextFont(qdp);
    txSize = GetPortTextSize(qdp);
    txFace = GetPortTextFace(qdp);
  }
  {
    wxKey *k = (wxKey *)SCHEME_BYTE_STR_VAL(table_key);
    k->scale_x = scale_x;
    k->scale_y = scale_y;
    k->angle = angle;
    k->txFont = txFont;
    k->txSize = txSize;
    k->txFace = txFace;
    k->use_cgctx = (char)use_cgctx;
    k->smoothing = (char)smoothing;
  }

  if (use_cache) {
    /* Get all cached sizes */
    double r = 0;
    int i, all = 1, wc, di;
    atomic_timeout_t old;

    if (ulen > QUICK_UBUF_SIZE) {
      widths = new WXGC_ATOMIC double[ulen];
      layouts = (ATSUTextLayout *)(new WXGC_ATOMIC char[sizeof(ATSUTextLayout) * ulen]);
      glyphs = new WXGC_ATOMIC int[ulen];
    } else {
      widths = widths_buf;
      layouts = layout_buf;
      glyphs = glyphs_buf;
    }

    old = pre_scheme();

    for (i = 0; i < (int)ulen; i += di) {
      wc = unicode[i];
      if ((wc & 0xF800) == 0xD800) {
	/* Yuck. Re-un-parse UTF-16... */
	wc = ((wc & 0x3FF) << 10) + (unicode[i+1] & 0x3FF);
	di = 2;
      } else
	di = 1;
      
      ((wxKey *)SCHEME_BYTE_STR_VAL(table_key))->code = wc;
      val = lookup_width(table_key);
      if (!val) {
	all = 0;
	widths[i] = -1;
      } else {
	widths[i] = SCHEME_DBL_VAL(SCHEME_VEC_ELS(val)[0]);
	layouts[i] = *(ATSUTextLayout *)SCHEME_VEC_ELS(val)[1];
	glyphs[i] = SCHEME_INT_VAL(SCHEME_VEC_ELS(val)[2]);
	r += widths[i];
      }
    }

    post_scheme(old);

    if (all && just_meas)
      return r;
  } else {
    widths = NULL;
    layouts = NULL;
    glyphs = NULL;
  }

  END_TIME(cache);
  START_TIME;
  
  /****************************************/
  /* Set up style                         */

  /* We cache ATSUStyles using a hash table, and currently, we never
     release them. (In case some adds relasing code later: check
     whether it's ok to release a style that was provided to a
     text-layout object. */

  if (use_cache) {
    atomic_timeout_t old;

    old = pre_scheme();

    ((wxKey *)SCHEME_BYTE_STR_VAL(table_key))->angle = 0.0;
    ((wxKey *)SCHEME_BYTE_STR_VAL(table_key))->code = 0;
    val = scheme_hash_get(style_table, table_key);
    if (val) {
      style = *(ATSUStyle *)val;
    } else {
      Scheme_Object *new_key;
      ATSUCreateAndCopyStyle(theATSUqdstyle, &style);
      atsuSetStyleFromGrafPtrParams(style, txFont, txSize, txFace, smoothing, 
				    angle, use_cgctx ? 1.0 : scale_y,
				    qd_spacing);
      val = (Scheme_Object *)(new WXGC_ATOMIC char[sizeof(ATSUStyle)]);
      *(ATSUStyle *)val = style;
      new_key = scheme_make_sized_byte_string(SCHEME_BYTE_STR_VAL(table_key), sizeof(wxKey), 1);
      scheme_hash_set(style_table, new_key, val);
    }
    ((wxKey *)SCHEME_BYTE_STR_VAL(table_key))->angle = angle;

    post_scheme(old);
  } else {
    style = theATSUstyle;
    atsuSetStyleFromGrafPtrParams(style, txFont, txSize, txFace, smoothing, 
				  angle, use_cgctx ? 1.0 : scale_y,
				  qd_spacing);
  }

  END_TIME(style);
  START_TIME;
  
  /****************************************/
  /* Set up port                          */

  if (!just_meas) {
    GetPortBounds(qdp, &portRect); 
    GetBackColor(&eraseColor);
    GetForeColor(&textColor);
    textMode = GetPortTextMode(qdp);

    if (!with_start) {
      GetPen(&start);
      start_x = start.h;
      start_y = start.v;
    }

    if ((angle == 0.0) && (textMode == srcCopy)) {
      ::GetFontInfo(&fontInfo);
    }

    if (use_cgctx) {
      /* Make clipping regions match (including BeginUpdate effect) */
      clipRgn = GetCurrentClipRgn(qdp);
    } else
      clipRgn = NULL;
  } else
    clipRgn = NULL;

  if (use_cgctx && QDBeginCGContext(qdp, &cgctx))
    use_cgctx = 0;

  if (use_cgctx && !just_meas) {
    SyncCGContextOriginWithPort(cgctx, qdp);
    if (clipRgn) {
      ClipCGContextToRegion(cgctx, &portRect, clipRgn);
      DisposeRgn(clipRgn);
    }
  }

  if (use_cgctx) {
    /* Set scale */
    if (!just_meas) {
      long h;
      h = portRect.bottom - portRect.top;
      CGContextTranslateCTM(cgctx, 
			    (with_start ? ddx : 0), 
			    h - (with_start ? ddy : 0));
      ddx = 0;
      ddy = 0;
    }
    CGContextScaleCTM(cgctx, scale_x, scale_y);

    /* set color */
    CGContextSetRGBFillColor(cgctx, 
			     (double)textColor.red / 65535.0,
			     (double)textColor.green / 65535.0,
			     (double)textColor.blue / 65535.0,
			     1.0);

    /* set alpha */
    CGContextSetAlpha(cgctx, current_alpha);
  }

  END_TIME(ctx);
  START_TIME;

  /****************************************/
  /* Draw/measure loop                    */
  
  /* Beware of GCing without adjusting the text layout, because the  */
  /* unicode string could move, and because GCing might attempt to   */
  /* draw a bitmap into the same port. */

  delta = 0;
	
  while (1) {
    if (delta >= ulen)
      break;

    if (qd_spacing) {
      if ((unicode[delta] & 0xF800) == 0xD800) {
	/* Surrogate pair; need to handle 2 at a time */
	one_ulen = 2;
      } else
	one_ulen = 1;
    } else
      one_ulen = ulen;

    if (use_cache) {
      if (widths[delta] >= 0) {
	one_res = widths[delta];
	layout = layouts[delta];
	need_size = 0;

	/* Adjust context in layout: */
	if (use_cgctx) {
	  GC_CAN_IGNORE ATSUAttributeTag ll_theTags[1];
	  GC_CAN_IGNORE ByteCount ll_theSizes[1];
	  ATSUAttributeValuePtr ll_theValues[1];
	  ll_theTags[0] = kATSUCGContextTag;
	  ll_theSizes[0] = sizeof(CGContextRef);
	  ll_theValues[0] =  &cgctx;
	  ATSUSetLayoutControls(layout, 1, ll_theTags, ll_theSizes, ll_theValues);
	}
      } else {
	one_res = 0;
	need_size = 1;
      }
    } else {
      need_size = 1;
      one_res = 0;
    }

    need_layout = (!just_meas || need_size);
    if (need_layout) {
      if (!layout) {
	UniCharArrayPtr uca;
	ATSUStyle *style_array;
	UniCharCount *ulen_array;

	if (use_cache) {
	  uca = (UniCharArrayPtr)malloc(one_ulen * sizeof(UniChar) + sizeof(ATSUStyle) + sizeof(UniCharCount));
	  memcpy(uca, unicode XFORM_OK_PLUS delta, one_ulen * sizeof(UniChar));
	  style_array = (ATSUStyle *)((char *)uca XFORM_OK_PLUS (one_ulen * sizeof(UniChar)));
	  ulen_array = (UniCharCount *)(style_array XFORM_OK_PLUS 1);
	  *style_array = style;
	  *ulen_array = one_ulen;
	} else {
	  uca = (UniCharArrayPtr)(unicode XFORM_OK_PLUS delta);
	  style_array = &style;
	  ulen_array = &one_ulen;
	}

	ATSUCreateTextLayoutWithTextPtr(uca,
					kATSUFromTextBeginning,
					kATSUToTextEnd,
					one_ulen,
					1,
					ulen_array,
					style_array,
					&layout);

	if (qd_spacing || use_cgctx) {
	  int cnt = 0;
	  GC_CAN_IGNORE ATSUAttributeTag ll_theTags[2];
	  GC_CAN_IGNORE ByteCount ll_theSizes[2];
	  ATSUAttributeValuePtr ll_theValues[2];
	  ATSLineLayoutOptions ll_attribs;

	  ll_attribs = (kATSLineHasNoHangers
			| kATSLineHasNoOpticalAlignment);

	  if (qd_spacing) {
	    ll_attribs |= (kATSLineFractDisable 
			   | kATSLineDisableAutoAdjustDisplayPos
			   | kATSLineDisableAllLayoutOperations
			   | kATSLineUseDeviceMetrics
			   | (use_cgctx ? 0 : kATSLineUseQDRendering));
	  }
	  ll_theTags[cnt] = kATSULineLayoutOptionsTag;
	  ll_theSizes[cnt] = sizeof(ATSLineLayoutOptions);
	  ll_theValues[cnt] = &ll_attribs;
	  cnt++;

	  if (use_cgctx) {
	    ll_theTags[cnt] = kATSUCGContextTag;
	    ll_theSizes[cnt] = sizeof(CGContextRef);
	    ll_theValues[cnt] =  &cgctx;
	    cnt++;
	  }
    
	  ATSUSetLayoutControls(layout, cnt, ll_theTags, ll_theSizes, ll_theValues);
	}

	ATSUSetTransientFontMatching(layout, TRUE);
      
	if (angle != 0.0) {
	  GC_CAN_IGNORE ATSUAttributeTag  r_theTags[] = { kATSULineRotationTag };
	  GC_CAN_IGNORE ByteCount    r_theSizes[] = { sizeof(Fixed) };
	  ATSUAttributeValuePtr r_theValues[1];
	  Fixed deg_angle;
	
	  deg_angle = DoubleToFixed(angle * 180 / 3.14159);
	  r_theValues[0] = &deg_angle;
	  ATSUSetLayoutControls(layout, 1, r_theTags, r_theSizes, r_theValues); 
	}
      }
    }
      
    END_TIME(layout);
    START_TIME;

    if (need_size) {
      ATSTrapezoid bounds;
      ItemCount actual;
      ATSUTextLayout meas_layout;

      if (angle != 0.0) {
	ATSUAttributeTag tag = kATSULineRotationTag;
	ATSUCreateAndCopyTextLayout(layout, &meas_layout);
	ATSUClearLayoutControls(meas_layout, 1, &tag);
      } else
	meas_layout = layout;

      ATSUGetGlyphBounds(meas_layout,
			 0, 0,
			 kATSUFromTextBeginning,
			 kATSUToTextEnd,
			 kATSUseDeviceOrigins,
			 1,
			 &bounds,
			 &actual);

      if (angle != 0.0)
	ATSUDisposeTextLayout(meas_layout);
      
      one_res = (Fix2X(bounds.upperRight.x) - Fix2X(bounds.upperLeft.x));
      if (one_res < 0)
	one_res = 0;

      if (use_cache)
	widths[delta] = one_res;
    } else if (use_cache)
      widths[delta] = -1.0; /* inidicates that we don't need to re-hash */
    if (!use_cgctx && !just_meas) {
      one_res = one_res / scale_y;
    }
    result += one_res;

    END_TIME(measure);
    START_TIME;

    if (!just_meas) {
      if (!with_start) {
	ddx = 0;
	ddy = 0;
	need_convert = 0;
      } else {
	need_convert = 1;
      }
    
      if ((angle == 0.0) && (textMode == srcCopy)) {
	if (use_cgctx) {
	  CGRect cgr;
	  double rt, rl, rr, rb;

	  rl = JUSTDELTA(start_x, scale_x, ddx) + (use_pen_delta ? pen_delta : 0.0);
	  rt = JUSTDELTA(start_y, scale_y, ddy) - fontInfo.ascent;
	  rb = JUSTDELTA(start_y, scale_y, ddy) + fontInfo.descent;
	  rr = rl + one_res;

	  cgr.origin.x = rl;
	  cgr.origin.y = -rb;
	  cgr.size.width = rr - rl;
	  cgr.size.height = rb - rt;

	  CGContextSetRGBFillColor(cgctx, 
				   (double)eraseColor.red / 65535.0,
				   (double)eraseColor.green / 65535.0,
				   (double)eraseColor.blue / 65535.0,
				   1.0);
	  CGContextFillRect(cgctx, cgr);
	  CGContextSetRGBFillColor(cgctx, 
				   (double)textColor.red / 65535.0,
				   (double)textColor.green / 65535.0,
				   (double)textColor.blue / 65535.0,
				   1.0);
	} else {
	  Rect theRect;
	  double rt, rl, rr, rb;
	  
	  rl = COORDCONV(start_x, scale_x, ddx) + (use_pen_delta ? (pen_delta * scale_x) : 0.0);
	  rt = COORDCONV(start_y, scale_y, ddy) - (fontInfo.ascent * scale_y);
	  rb = COORDCONV(start_y, scale_y, ddy) + (fontInfo.descent * scale_y);
	  rr = rl + (one_res * scale_x);

	  theRect.left = (int)floor(rl);
	  theRect.top = (int)floor(rt);
	  theRect.right = (int)floor(rr);
	  theRect.bottom = (int)floor(rb);
	  EraseRect(&theRect);
	}
      }
    
      {
	Fixed sx, sy;

	if (use_cgctx) {
	  double isx;
	  isx = JUSTDELTA(start_x, scale_x, ddx) + (use_pen_delta ? pen_delta : 0.0);
	  sx = DoubleToFixed(isx);
	} else if (with_start) {
	  double isx;
	  isx = COORDCONV(start_x, scale_x, ddx) + (use_pen_delta ? (pen_delta * scale_x) : 0.0);
	  sx = DoubleToFixed(isx);
	} else {
	  sx = kATSUUseGrafPortPenLoc;
	}

	if (use_cgctx) {
	  double isy;
	  isy = -JUSTDELTA(start_y, scale_y, ddy);
	  sy = DoubleToFixed(isy);
	} else if (with_start) {
	  double isy;
	  isy = COORDCONV(start_y, scale_y, ddy);
	  sy = DoubleToFixed(isy);
	} else {
	  sy = kATSUUseGrafPortPenLoc;
	}

	ATSUDrawText(layout, 
		     kATSUFromTextBeginning,
		     kATSUToTextEnd,
		     sx, sy);
      }
    }

    if (!with_start) {
      /* Make sure start is scaled for further iterations */
      start_x = scale_x * start_x;
      start_y = scale_y * start_y;
      with_start = 1;
    }
    if (angle == 0.0) {
      start_x += one_res;
    } else {
      start_x += one_res * cos(angle);
      start_y -= one_res * sin(angle);
    }

    if (use_cache) {
      if (need_size) {
	ItemCount n;
	ATSLayoutRecord *info;

	if (ATSUDirectGetLayoutDataArrayPtrFromTextLayout(layout, 
							  0, 
							  kATSUDirectDataLayoutRecordATSLayoutRecordCurrent,
							  (void **)&info, &n)
	    == noErr) {
	  if (n == 2) {
	    /* Make sure that the glyph was not substituted from another font: */
	    ATSUFontID fontid;
	    UniCharArrayOffset changed;
	    UniCharCount changedLen;
	
	    if (ATSUMatchFontsToText(layout,
				     kATSUFromTextBeginning,
				     kATSUToTextEnd,
				     &fontid,
				     &changed,
				     &changedLen)
		== noErr) {
	      glyphs[delta] = info->glyphID;
	    } else {
	      glyphs[delta] = -1;
	    }
	  } else {
	    glyphs[delta] = -1;
	  }
	  ATSUDirectReleaseLayoutDataArrayPtr(NULL, kATSUDirectDataLayoutRecordATSLayoutRecordCurrent, (void **)&info);
	} else {
	  glyphs[delta] = -1;
	}

	layouts[delta] = layout;
      }
      layout = NULL;
    } else if (qd_spacing) {
      if (layout)
	ATSUDisposeTextLayout(layout);
      layout = NULL;
    }

    delta += one_ulen;
  }

  if (layout)
    ATSUDisposeTextLayout(layout);

  if (!just_meas) {
    if (use_cgctx) {
      CGContextSynchronize(cgctx);
      QDEndCGContext(qdp, &cgctx);
    }

    /* QuickDraw is back again: */
    if (!just_meas && !use_pen_delta && !with_start)
      MoveTo(start.h + (int)floor(result * scale_x), start.v);
  } else {
    if (use_cgctx) {
      QDEndCGContext(qdp, &cgctx);
    }
  }

  if (use_cache) {
    /* Record collected widths. (We can't record these during the
       drawing loop because it might trigger a GC, which might try to
       draw a GC bitmap, etc. */
    int j, wc, dj;
    atomic_timeout_t old;

    old = pre_scheme();

    for (j = 0; j < (int)ulen; j += dj) {
      wc = unicode[j];
      if ((wc & 0xF800) == 0xD800) {
	/* Yuck. Re-un-parse UTF-16... */
	wc = ((wc & 0x3FF) << 10) + (unicode[j+1] & 0x3FF);
	dj = 2;
      } else
	dj = 1;

      if (widths[j] >= 0) {
	char *lp;
	Scheme_Object *vec, *wd;

	val = scheme_make_sized_byte_string(SCHEME_BYTE_STR_VAL(table_key), sizeof(wxKey), 1);
	((wxKey *)SCHEME_BYTE_STR_VAL(val))->code = wc;

	vec = scheme_make_vector(3, NULL);

	wd = scheme_make_double(widths[j]);

	lp = new WXGC_ATOMIC char[sizeof(ATSUTextLayout)];
	*(ATSUTextLayout*)lp = layouts[j];

	SCHEME_VEC_ELS(vec)[0] = wd;
	SCHEME_VEC_ELS(vec)[1] = (Scheme_Object *)lp;
	SCHEME_VEC_ELS(vec)[2] = scheme_make_integer(glyphs[j]);

	scheme_hash_set(width_table, val, vec);
	
	if (width_table->mcount >= MAX_WIDTH_MAPPINGS) {
	  /* rotate tables, so width_table doesn't grow indefinitely,
	     but we also don't throw away recent information completely.
	     the old old_widths_table is going away, so dispose of
	     information there. */
	  {
	    int i;
	    void *ptr;
	    Boolean is_hand;
	    UniCharArrayOffset poffset;
	    UniCharCount plen, total_plen;
	    for (i = old_width_table->size; i--; ) {
	      if (old_width_table->vals[i]) {
		layout = *(ATSUTextLayout *)(SCHEME_VEC_ELS(old_width_table->vals[i])[1]);
		ATSUGetTextLocation(layout,
				    &ptr,
				    &is_hand,
				    &poffset,
				    &plen,
				    &total_plen);
		ATSUDisposeTextLayout(layout);
		free(ptr);
	      }
	    }
	  }

	  old_width_table = width_table;
	  width_table = scheme_make_hash_table_equal();
	}
      }
    }

    post_scheme(old);
  }

  END_TIME(draw);
  
#if 0
  if (!((time_counter++) & 0xFF)) {
    printf("---%ld\npre %ld\nctx %ld\nstyle %ld\nlayout %ld\nmeasure %ld\ndraw %ld\n",
	   time_counter,
	   time_preprocess, time_ctx, time_style, time_layout, time_measure, time_draw);
  }
#endif

  return result;
}

/************************************************************************/
/************************************************************************/

static void init_ATSU_style(void) 
{
  /* For some reason, toggling kAllTypographicFeaturesType makes
     text drawing slower and slower. So we have separate styles,
     one with typographic features and one without. */
  ATSUFontFeatureType types[1];
  ATSUFontFeatureSelector sels[1];
  
  ATSUCreateStyle(&theATSUstyle);
  ATSUCreateStyle(&theATSUqdstyle);
  
  types[0] = kAllTypographicFeaturesType;
  sels[0] = 0;
  ATSUSetFontFeatures(theATSUstyle, 1, types, sels);
  sels[0] = 1;
  ATSUSetFontFeatures(theATSUqdstyle, 1, types, sels); 
}


/* The following code comes from an Apple example: */


/*
 This is just like ATSUFONDtoFontID except that it also returns the intrinsic
 style of the selected font. This information is needed to correctly adjust the
 QD style bits. See the implementation of atsuSetStyleFromGrafPtr for an example.
 
 NB: On Mac OS 9 or later, just call through to FMGetFontFromFontFamilyInstance,
 which is the preferred function.
*/

static OSStatus
atsuFONDtoFontID( short    iFONDNumber,
      StyleParameter iFONDStyle,
      ATSUFontID *  oFontID,
       StyleParameter * oIntrinsicStyle )
{
  return FMGetFontFromFontFamilyInstance( iFONDNumber, iFONDStyle, oFontID, oIntrinsicStyle );
}

#define apple_require(x, y) if (!(x)) return status;

static OSStatus
atsuSetStyleFromGrafPtrParams( ATSUStyle iStyle, short txFont, short txSize, SInt16 txFace, int smoothing, 
			       double angle, double scale_y, int qd_spacing)
{
 OSStatus status = noErr;

#define xNUM_TAGS 8
 
 GC_CAN_IGNORE ATSUAttributeTag  theTags[] = { kATSUFontTag,
					       kATSUSizeTag,
					       kATSUQDBoldfaceTag,
					       kATSUQDItalicTag,
					       kATSUQDUnderlineTag,
					       kATSUStyleRenderingOptionsTag,
					       kATSUQDCondensedTag,
					       kATSUQDExtendedTag,
                                               };
 GC_CAN_IGNORE ByteCount    theSizes[] = { sizeof(ATSUFontID),
					   sizeof(Fixed),
					   sizeof(Boolean),
					   sizeof(Boolean),
					   sizeof(Boolean),
					   sizeof(ATSStyleRenderingOptions),
					   sizeof(Boolean),
					   sizeof(Boolean),
                                           };
 ATSUAttributeValuePtr theValues[ xNUM_TAGS /* = sizeof(theTags) / sizeof(ATSUAttributeTag) */ ];
 int tag_count;

 ATSUFontID   atsuFont;
 Fixed    atsuSize;
 Boolean    isBold, isItalic, isUnderline, isCondensed, isExtended;
 SInt16    intrinsicStyle;
 ATSStyleRenderingOptions options = kATSStyleNoOptions;
 
 status = atsuFONDtoFontID( txFont, txFace, &atsuFont, &intrinsicStyle );
 apple_require( status == noErr, EXIT );
 
 // Need to adjust the QD style bits based on the intrinsic style of the font.
 // Otherwise, you can end up doing things like artifically bolding an already-bold font.
 txFace &= ~intrinsicStyle;
 
 isBold = ( txFace & bold ) != 0;
 isItalic = ( txFace & italic ) != 0;
 isUnderline = ( txFace & underline ) != 0;
 isCondensed = ( txFace & condense ) != 0;
 isExtended = ( txFace & extend ) != 0;

 if (scale_y != 1.0)
   txSize = (short)floor(txSize * scale_y);
 if ( txSize == 0 ) {
   // this would already be set correctly in a brand-new style
   txSize = (short) ( GetScriptVariable( FontToScript( txFont ), smScriptPrefFondSize ) & 0xFFFFU );
 }
 atsuSize = Long2Fix( txSize );
 
 if (smoothing == wxSMOOTHING_OFF)
   options = kATSStyleNoAntiAliasing;
 else if (smoothing == wxSMOOTHING_ON)
   options = kATSStyleApplyAntiAliasing;

 // C doesn't allow this to be done in an initializer, so we have to fill in the pointers here.
 theValues[0] = &atsuFont;
 theValues[1] = &atsuSize;
 theValues[2] = &isBold;
 theValues[3] = &isItalic;
 theValues[4] = &isUnderline;
 theValues[5] = &options;
 theValues[6] = &isCondensed;
 theValues[7] = &isExtended;

 tag_count = xNUM_TAGS;

 status = ATSUSetAttributes( iStyle, tag_count, theTags, theSizes, theValues );

 return status;
}
