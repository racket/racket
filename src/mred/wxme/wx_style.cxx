/*
 * File:        wx_style.cc
 * Purpose:     wxStyle and wxStyleList implementation
 * Author:      Matthew Flatt
 * Created:     1995
 * Copyright:   (c) 2004-2005 PLT Scheme, Inc.
 * Copyright:   (c) 1995, Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 */

#if defined(_MSC_VER) && defined(MZ_PRECISE_GC)
# include "wx.h"
#endif
#include "common.h"
#include "wx_gdi.h"
#include "wx_main.h"
#include "wx_dialg.h"
#ifndef OLD_WXWINDOWS
# include "wx_cmdlg.h"
#endif
#include "wx_style.h"
#include "wx_mtype.h"
#include "wx_medio.h"
#include "wx_ptreq.h"
#include <string.h>
#include "scheme.h"

#ifdef wx_x
static int defaultSize = 12;
#endif
#ifdef wx_msw
static int defaultSize = 10;
#endif
#ifdef wx_mac
static int defaultSize = 12;
#endif

# define FONT_DIRECTORY wxTheFontNameDirectory

extern void wxmeError(const char *e);
extern int wxGetPreference(const char *, int *);

wxStyleList *wxTheStyleList;

static wxStyleDelta *quick_delta;

#ifdef WX_USE_XFT
extern int wxXRenderHere(void);
#endif

void wxInitStyles(void)
{
  if (wxTheStyleList)
    return;

#ifdef WX_USE_XFT
  if (wxXRenderHere())
    defaultSize = 11;
#endif

  wxGetPreference("default-font-size", &defaultSize);

  wxREGGLOB(wxTheStyleList);
  wxTheStyleList = new wxStyleList;

  wxREGGLOB(quick_delta);
}

void wxMultColour::Get(double *rf, double *gf, double *bf)
{
  *rf = r;
  *gf = g;
  *bf = b;
}

void wxMultColour::Set(double rf, double gf, double bf)
{
  r = rf;
  g = gf;
  b = bf;
}

void wxAddColour::Get(short *rf, short *gf, short *bf)
{
  *rf = r;
  *gf = g;
  *bf = b;
}

void wxAddColour::Set(short rf, short gf, short bf)
{
  r = rf;
  g = gf;
  b = bf;
}

wxStyleDelta::wxStyleDelta(int changeCommand, int param) 
: wxObject(WXGC_NO_CLEANUP)
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_STYLE_DELTA;
#endif

  face = NULL;
    
  SetDelta(wxCHANGE_NOTHING);
  SetDelta(changeCommand, param);
}

wxStyleDelta::~wxStyleDelta()
{
}

wxStyleDelta *wxStyleDelta::SetDelta(int changeCommand, int param)
{
  switch (changeCommand) {
  case wxCHANGE_NOTHING:
    family = wxBASE;
    face = NULL;
    sizeMult = 1;
    sizeAdd = 0;
    weightOn = wxBASE;
    weightOff = wxBASE;
    styleOn = wxBASE;
    styleOff = wxBASE;
    smoothingOn = wxBASE;
    smoothingOff = wxBASE;
    underlinedOn = underlinedOff = FALSE;
    sipOn = sipOff = FALSE;
    transparentTextBackingOn = transparentTextBackingOff = FALSE;
    foregroundMult = new wxMultColour;
    foregroundMult->Set(1, 1, 1);
    foregroundAdd = new wxAddColour;
    foregroundAdd->Set(0, 0, 0);
    backgroundMult = new wxMultColour;
    backgroundMult->Set(1, 1, 1);
    backgroundAdd = new wxAddColour;
    backgroundAdd->Set(0, 0, 0);
    alignmentOn = wxBASE;
    alignmentOff = wxBASE;
    break;
  case wxCHANGE_STYLE:
    styleOn = param;
    styleOff = wxBASE;
    break;
  case wxCHANGE_WEIGHT:
    weightOn = param;
    weightOff = wxBASE;
    break;
  case wxCHANGE_SMOOTHING:
    smoothingOn = param;
    smoothingOff = wxBASE;
    break;
  case wxCHANGE_UNDERLINE:
    underlinedOn = param;
    underlinedOff = !param;
    break;
  case wxCHANGE_SIP:
    sipOn = param;
    sipOff = !param;
    break;
  case wxCHANGE_SIZE:
    sizeMult = 0;
    sizeAdd = param;
    break;
  case wxCHANGE_FAMILY:
    family = param;
    face = NULL;
    break;
  case wxCHANGE_ALIGNMENT:
    alignmentOn = param;
    alignmentOff = wxBASE;
    break;
  case wxCHANGE_BOLD:
    weightOn = wxBOLD;
    weightOff = wxBASE;
    break;
  case wxCHANGE_ITALIC:
    styleOn = wxITALIC;
    styleOff = wxBASE;
    break;
  case wxCHANGE_TOGGLE_STYLE:
    styleOn = param;
    styleOff = param;
    break;
  case wxCHANGE_TOGGLE_WEIGHT:
    weightOn = param;
    weightOff = param;
    break;
  case wxCHANGE_TOGGLE_SMOOTHING:
    smoothingOn = param;
    smoothingOff = param;
    break;
  case wxCHANGE_TOGGLE_UNDERLINE:
    underlinedOn = TRUE;
    underlinedOff = TRUE;
    break;
  case wxCHANGE_TOGGLE_SIP:
    sipOn = TRUE;
    sipOff = TRUE;
    break;
  case wxCHANGE_BIGGER:
    sizeMult = 1;
    sizeAdd = param;
    break;
  case wxCHANGE_SMALLER:
    sizeMult = 1;
    sizeAdd = -param;
    break;
  case wxCHANGE_NORMAL:
    family = wxDEFAULT;
    face = NULL;
    sizeMult = 0;
    sizeAdd = defaultSize;
    weightOn = wxNORMAL;
    weightOff = wxBASE;
    styleOn = wxNORMAL;
    styleOff = wxBASE;
    smoothingOn = wxSMOOTHING_DEFAULT;
    smoothingOff = wxBASE;
    underlinedOn = FALSE;
    underlinedOff = TRUE;
    sipOn = FALSE;
    sipOff = TRUE;
    alignmentOn = wxALIGN_BOTTOM;
    alignmentOff = wxBASE;
    /* fall through ... */
  case wxCHANGE_NORMAL_COLOUR: /* ^^ falls through */
    foregroundMult->Set(0, 0, 0);
    foregroundAdd->Set(0, 0, 0);
    backgroundMult->Set(0, 0, 0);
    backgroundAdd->Set(255, 255, 255);
    break;
  }

  return this;
}

wxStyleDelta *wxStyleDelta::SetDeltaFace(char *name, int fam)
{
  face = copystring(name);
  family = fam;

  return this;
}

wxStyleDelta *wxStyleDelta::SetDeltaBackground(char *name)
{
  wxColour *c;

  transparentTextBackingOn = FALSE;
  transparentTextBackingOff = TRUE;

  if ((c = wxTheColourDatabase->FindColour(name)))
    SetDeltaBackground(c);

  return this;
}

wxStyleDelta *wxStyleDelta::SetDeltaBackground(wxColour *colour)
{
  unsigned char r, g, b;

  transparentTextBackingOn = FALSE;
  transparentTextBackingOff = TRUE;

  backgroundMult->Set(0, 0, 0);
  colour->Get(&r, &g, &b);
  backgroundAdd->Set(r, g, b);

  return this;
}

wxStyleDelta *wxStyleDelta::SetDeltaForeground(char *name)
{
  wxColour *c;

  if ((c = wxTheColourDatabase->FindColour(name)))
    SetDeltaForeground(c);
  return this;
}

wxStyleDelta *wxStyleDelta::SetDeltaForeground(wxColour *colour)
{
  unsigned char r, g, b;

  foregroundMult->Set(0, 0, 0);
  colour->Get(&r, &g, &b);
  foregroundAdd->Set(r, g, b);
  return this;
}

Bool wxStyleDelta::Collapse(wxStyleDelta *deltaIn)
{
  double ambr, ambb, ambg, amfr, amfb, amfg;
  double bmbr, bmbb, bmbg, bmfr, bmfb, bmfg;
  short aabr, aabb, aabg, aafr, aafb, aafg;
  short babr, babb, babg, bafr, bafb, bafg;

  /* If collapsing possible? */
  /* It may not be if add & multiply sequence occurs, */
  /* or certain toggling settings conflict or */
  if (sizeMult && sizeMult != 1.0 && deltaIn->sizeAdd != 0)
    return FALSE;

  foregroundMult->Get(&amfr, &amfb, &amfg);
  backgroundMult->Get(&ambr, &ambb, &ambg);
  deltaIn->foregroundAdd->Get(&bafr, &bafb, &bafg);
  deltaIn->backgroundAdd->Get(&babr, &babb, &babg);
  if ((amfr && amfr != 1.0 && bafr != 0)
      || (amfg && amfg != 1.0 && bafg != 0)
      || (amfb && amfb != 1.0 && bafb != 0)
      || (ambr && ambr != 1.0 && babr != 0)
      || (ambg && ambg != 1.0 && babg != 0)
      || (ambb && ambb != 1.0 && babb != 0))
    return FALSE;

  // Cases: simple or double toggle
  //        no further change
  //        formerly no change
  //        style definitely on
  //        style definitely off
  if (!((styleOn == deltaIn->styleOn && styleOff == deltaIn->styleOff)
	|| (styleOn == wxBASE && styleOff == wxBASE)
	|| (deltaIn->styleOn == wxBASE && deltaIn->styleOff == wxBASE)
	|| (styleOn == wxBASE && styleOff != wxBASE)
	|| (styleOff == wxBASE && styleOn != wxBASE)))
    return FALSE;
  if (!((weightOn == deltaIn->weightOn && weightOff == deltaIn->weightOff)
	|| (weightOn == wxBASE && weightOff == wxBASE)
	|| (weightOn == wxBASE && weightOff != wxBASE)
	|| (weightOff == wxBASE && weightOn != wxBASE)))
    return FALSE;
  if (!((smoothingOn == deltaIn->smoothingOn && smoothingOff == deltaIn->smoothingOff)
	|| (smoothingOn == wxBASE && smoothingOff == wxBASE)
	|| (smoothingOn == wxBASE && smoothingOff != wxBASE)
	|| (smoothingOff == wxBASE && smoothingOn != wxBASE)))
    return FALSE;
  if (!((alignmentOn == deltaIn->alignmentOn 
	 && alignmentOff == deltaIn->alignmentOff)
	|| (alignmentOn == wxBASE && alignmentOff == wxBASE)
	|| (alignmentOn == wxBASE && alignmentOff != wxBASE)
	|| (alignmentOff == wxBASE && alignmentOn != wxBASE)))
    return FALSE;

  if (!((underlinedOn == deltaIn->underlinedOn
	 && underlinedOff == deltaIn->underlinedOff)
	|| (!underlinedOn && !underlinedOff)
	|| (!deltaIn->underlinedOn && !deltaIn->underlinedOff)
	|| (!underlinedOn && underlinedOff)
	|| (!underlinedOff && underlinedOn)))
    return FALSE;

  if (!((sipOn == deltaIn->sipOn
	 && sipOff == deltaIn->sipOff)
	|| (!sipOn && !sipOff)
	|| (!deltaIn->sipOn && !deltaIn->sipOff)
	|| (!sipOn && sipOff)
	|| (!sipOff && sipOn)))
    return FALSE;

  if (!((transparentTextBackingOn == deltaIn->transparentTextBackingOn
	 && transparentTextBackingOff == deltaIn->transparentTextBackingOff)
	|| (!transparentTextBackingOn && !transparentTextBackingOff)
	|| (!deltaIn->transparentTextBackingOn && !deltaIn->transparentTextBackingOff)
	|| (!transparentTextBackingOn && transparentTextBackingOff)
	|| (!transparentTextBackingOff && transparentTextBackingOn)))
    return FALSE;

  /* Collapsing is possible. */

  deltaIn->foregroundMult->Get(&bmfr, &bmfb, &bmfg);
  deltaIn->backgroundMult->Get(&bmbr, &bmbb, &bmbg);
  foregroundAdd->Get(&aafr, &aafb, &aafg);
  backgroundAdd->Get(&aabr, &aabb, &aabg);
  
  sizeAdd += (int)(sizeMult * deltaIn->sizeAdd);
  sizeMult *= deltaIn->sizeMult;

  foregroundMult->Set(amfr * bmfr, amfb * bmfb, amfg * bmfg);
  backgroundMult->Set(ambr * bmbr, ambb * bmbb, ambg * bmbg);
  foregroundAdd->Set(aafr + (int)(amfr * bafr), 
		     aafb + (int)(amfb * bafb), 
		     aafg + (int)(amfg * bafg));
  backgroundAdd->Set(aabr + (int)(ambr * babr), 
		     aabb + (int)(ambb * babb), 
		     aabg + (int)(ambg * babg));

  if (family == wxBASE) {
    family = deltaIn->family;
    if (!face) {
      face = deltaIn->face;
    }
  }

  if (styleOn == wxBASE && styleOff == wxBASE) {
    styleOff = deltaIn->styleOff;
    styleOn = deltaIn->styleOn;
  } else if (styleOn != wxBASE && styleOff != wxBASE) {
    if (deltaIn->styleOn != wxBASE || deltaIn->styleOff != wxBASE
	&& styleOn == styleOff)
      styleOn = styleOff = wxBASE; // Double toggle
  }
  if (weightOn == wxBASE && weightOff == wxBASE) {
    weightOff = deltaIn->weightOff;
    weightOn = deltaIn->weightOn;
  } else if (weightOn != wxBASE && weightOff != wxBASE) {
    if (deltaIn->weightOn != wxBASE || deltaIn->weightOff != wxBASE
	&& weightOn == weightOff)
      weightOn = weightOff = wxBASE; // Double toggle
  }
  if (smoothingOn == wxBASE && smoothingOff == wxBASE) {
    smoothingOff = deltaIn->smoothingOff;
    smoothingOn = deltaIn->smoothingOn;
  } else if (smoothingOn != wxBASE && smoothingOff != wxBASE) {
    if (deltaIn->smoothingOn != wxBASE || deltaIn->smoothingOff != wxBASE
	&& smoothingOn == smoothingOff)
      smoothingOn = smoothingOff = wxBASE; // Double toggle
  }
  if (alignmentOn == wxBASE && alignmentOff == wxBASE) {
    alignmentOff = deltaIn->alignmentOff;
    alignmentOn = deltaIn->alignmentOn;
  } else if (alignmentOn != wxBASE && alignmentOff != wxBASE) {
    if (deltaIn->alignmentOn != wxBASE || deltaIn->alignmentOff != wxBASE
	&& alignmentOn == alignmentOff)
      alignmentOn = alignmentOff = wxBASE; // Double toggle
  }
  if (!underlinedOn && !underlinedOff) {
    underlinedOn = deltaIn->underlinedOn;
    underlinedOff = deltaIn->underlinedOff;
  } else if (underlinedOn && underlinedOff) {
    if (deltaIn->underlinedOn && deltaIn->underlinedOff)
      underlinedOn = underlinedOff = FALSE;
  }
  if (!sipOn && !sipOff) {
    sipOn = deltaIn->sipOn;
    sipOff = deltaIn->sipOff;
  } else if (sipOn && sipOff) {
    if (deltaIn->sipOn && deltaIn->sipOff)
      sipOn = sipOff = FALSE;
  }
  if (!transparentTextBackingOn && !transparentTextBackingOff) {
    transparentTextBackingOn = deltaIn->transparentTextBackingOn;
    transparentTextBackingOff = deltaIn->transparentTextBackingOff;
  } else if (transparentTextBackingOn && transparentTextBackingOff) {
    if (deltaIn->transparentTextBackingOn && deltaIn->transparentTextBackingOff)
      transparentTextBackingOn = transparentTextBackingOff = FALSE;
  }

  return TRUE;
}

Bool wxStyleDelta::Equal(wxStyleDelta *deltaIn)
{
#define SAME_C(fld) ((fld->r == deltaIn->fld->r) \
                     && (fld->g == deltaIn->fld->g) \
                     && (fld->b == deltaIn->fld->b))

  return ((family == deltaIn->family)
	  && ((face && deltaIn->face && !strcmp(face, deltaIn->face))
	      || (!face && !deltaIn->face))
	  && sizeMult == deltaIn->sizeMult
	  && sizeAdd == deltaIn->sizeAdd
	  && weightOn == deltaIn->weightOn
	  && weightOff == deltaIn->weightOff
	  && smoothingOn == deltaIn->smoothingOn
	  && smoothingOff == deltaIn->smoothingOff
	  && styleOn == deltaIn->styleOn
	  && styleOff == deltaIn->styleOff
	  && underlinedOn == deltaIn->underlinedOn
	  && underlinedOff == deltaIn->underlinedOff
	  && sipOn == deltaIn->sipOn
	  && sipOff == deltaIn->sipOff
	  && transparentTextBackingOn == deltaIn->transparentTextBackingOn
	  && transparentTextBackingOff == deltaIn->transparentTextBackingOff
	  && SAME_C(foregroundMult)
	  && SAME_C(backgroundMult)
	  && SAME_C(foregroundAdd)
	  && SAME_C(backgroundAdd)
	  && alignmentOn == deltaIn->alignmentOn
	  && alignmentOff == deltaIn->alignmentOff);
#undef SAME_C
}

void wxStyleDelta::Copy(wxStyleDelta *in)
{
#define DCOPY(x) x = in->x
  DCOPY(family);
  DCOPY(face);
  DCOPY(sizeMult);
  DCOPY(sizeAdd);
  DCOPY(weightOn);
  DCOPY(weightOff);
  DCOPY(smoothingOn);
  DCOPY(smoothingOff);
  DCOPY(styleOn);
  DCOPY(styleOff);
  DCOPY(underlinedOn);
  DCOPY(underlinedOff);
  DCOPY(sipOn);
  DCOPY(sipOff);
  DCOPY(transparentTextBackingOn);
  DCOPY(transparentTextBackingOff);
  DCOPY(foregroundMult->r);
  DCOPY(foregroundMult->g);
  DCOPY(foregroundMult->b);
  DCOPY(foregroundAdd->r);
  DCOPY(foregroundAdd->g);
  DCOPY(foregroundAdd->b);
  DCOPY(backgroundMult->r);
  DCOPY(backgroundMult->g);
  DCOPY(backgroundMult->b);
  DCOPY(backgroundAdd->r);
  DCOPY(backgroundAdd->g);
  DCOPY(backgroundAdd->b);
  DCOPY(alignmentOn);
  DCOPY(alignmentOff);
}

/***************************************************************/

static unsigned char ColourNum(double v)
{
  if (v < 0)
    return 0;
  else if (v > 255)
    return 255;
  else
    return (unsigned char)v;
}

wxStyle::wxStyle()
: wxObject(WXGC_NO_CLEANUP)
{
  wxList *cl;

  __type = wxTYPE_STYLE;

  textMetricDC = NULL;

  foreground = new wxColour;
  background = new wxColour;

  cl = new wxList(wxKEY_NONE, FALSE);
  children = cl;
}

wxStyle::~wxStyle()
{
  DELETE_OBJ children;
  styleList = NULL;
  nonjoin_delta = NULL;
  join_shiftStyle = NULL;
}

void wxStyle::Update(wxStyle *basic, wxStyle *target, 
		     Bool propogate, Bool topLevel)
{
  int size;
  int fontid;
  int style, weight, smoothing;
  unsigned char r, g, b;
  double rm, gm, bm;
  short rp, gp, bp; 
  Bool match;
  wxNode *node;
  Bool underlined, sip;
  wxStyle *base;

  base = baseStyle;
  if (basic) {
    if (!styleList || PTREQ(base, styleList->BasicStyle())) {
      base = basic;
    } else {
      base->Update(basic, target, FALSE, FALSE);
      base = target;
    }
  }

  if (!target)
    target = this;

  if (join_shiftStyle) {
    if (styleList) {
      if (!PTREQ(join_shiftStyle, styleList->BasicStyle())) {
	join_shiftStyle->Update(base, target, FALSE, topLevel);
      } else {
	target->alignment = base->alignment;
	target->font = base->font;
	target->pen = base->pen;
	target->brush = base->brush;
	target->textMetricDC = NULL;
	target->foreground->CopyFrom(base->foreground);
	target->background->CopyFrom(base->background);

	if (styleList) {
	  styleList->StyleWasChanged(target);
	  if (topLevel)
	    styleList->StyleWasChanged(NULL);
	}
      }
    }
    return;
  }

  size = (int)(nonjoin_delta->sizeMult * base->font->GetPointSize());
  size += nonjoin_delta->sizeAdd;
  if (size <= 0)
    size = 1;

  if (!nonjoin_delta->face && nonjoin_delta->family == wxBASE) {
    fontid = base->font->GetFontId();
  } else {
    int fam;
    fam = nonjoin_delta->family;
    if (fam == wxBASE)
      fam = base->font->GetFamily();

    if (nonjoin_delta->face)
      fontid = FONT_DIRECTORY->FindOrCreateFontId(nonjoin_delta->face, fam);
    else
      fontid = fam;
  }

  style = base->font->GetStyle();
  match = (style == nonjoin_delta->styleOff);
  if (match)
    style = wxNORMAL;
  if (!match || (match && nonjoin_delta->styleOn != nonjoin_delta->styleOff))
    if (nonjoin_delta->styleOn != wxBASE)
      style = nonjoin_delta->styleOn;

  weight = base->font->GetWeight();
  match = (weight == nonjoin_delta->weightOff);
  if (match)
    weight = wxNORMAL;
  if (!match || (match && nonjoin_delta->weightOn != nonjoin_delta->weightOff))
    if (nonjoin_delta->weightOn != wxBASE)
      weight = nonjoin_delta->weightOn;

  smoothing = base->font->GetSmoothing();
  match = (smoothing == nonjoin_delta->smoothingOff);
  if (match)
    smoothing = wxSMOOTHING_DEFAULT;
  if (!match || (match && nonjoin_delta->smoothingOn != nonjoin_delta->smoothingOff))
    if (nonjoin_delta->smoothingOn != wxBASE)
      smoothing = nonjoin_delta->smoothingOn;

  target->alignment = base->alignment;
  match = (target->alignment == nonjoin_delta->alignmentOff);
  if (match)
    target->alignment = wxALIGN_BOTTOM;
  if (!match || (match && nonjoin_delta->alignmentOn != nonjoin_delta->alignmentOff))
    if (nonjoin_delta->alignmentOn != wxBASE)
      target->alignment = nonjoin_delta->alignmentOn;

  if (nonjoin_delta->underlinedOff && nonjoin_delta->underlinedOn)
    underlined = !base->font->GetUnderlined();
  else if (nonjoin_delta->underlinedOff)
    underlined = FALSE;
  else if (nonjoin_delta->underlinedOn)
    underlined = TRUE;
  else
    underlined = base->font->GetUnderlined();
  
  if (nonjoin_delta->sipOff && nonjoin_delta->sipOn)
    sip = !base->font->GetSizeInPixels();
  else if (nonjoin_delta->sipOff)
    sip = FALSE;
  else if (nonjoin_delta->sipOn)
    sip = TRUE;
  else
    sip = base->font->GetSizeInPixels();
  
  target->font = wxTheFontList->FindOrCreateFont(size, fontid,
						 style, weight, 
						 underlined, smoothing,
						 sip);

  target->textMetricDC = NULL;

  if (nonjoin_delta->transparentTextBackingOff && nonjoin_delta->transparentTextBackingOn)
    transText = !base->transText;
  else if (nonjoin_delta->transparentTextBackingOff)
    transText = FALSE;
  else if (nonjoin_delta->transparentTextBackingOn)
    transText = TRUE;
  else
    transText = base->transText;
  
  base->foreground->Get(&r, &g, &b);
  nonjoin_delta->foregroundMult->Get(&rm, &gm, &bm);
  nonjoin_delta->foregroundAdd->Get(&rp, &gp, &bp);
  r = ColourNum(r * rm + rp);
  g = ColourNum(g * gm + gp);
  b = ColourNum(b * bm + bp);
  target->foreground->Set(r, g, b);

  base->background->Get(&r, &g, &b);
  nonjoin_delta->backgroundMult->Get(&rm, &gm, &bm);
  nonjoin_delta->backgroundAdd->Get(&rp, &gp, &bp);
  r = ColourNum(r * rm + rp);
  g = ColourNum(g * gm + gp);
  b = ColourNum(b * bm + bp);
  target->background->Set(r, g, b);

  target->pen = wxThePenList->FindOrCreatePen(foreground, 0, wxSOLID);
  target->brush = wxTheBrushList->FindOrCreateBrush(background, wxSOLID);

  if (propogate)
    for (node = children->First(); node; node = node->Next()) {
      wxStyle *stl;
      stl = (wxStyle *)node->Data(); 
      stl->Update(NULL, NULL, TRUE, FALSE);
    }

  if (styleList) {
    styleList->StyleWasChanged(target);
    if (topLevel)
      styleList->StyleWasChanged(NULL);
  }
}

char *wxStyle::GetName()
{
  return name;
}

int wxStyle::GetFamily()
{
  return font->GetFamily();
}

char *wxStyle::GetFace()
{
  return font->GetFaceString();
}

wxFont *wxStyle::GetFont()
{
  return font;
}

int wxStyle::GetSize()
{
  return font->GetPointSize();
}

int wxStyle::GetWeight()
{
  return font->GetWeight();
}

int wxStyle::GetStyle()
{
  return font->GetStyle();
}

int wxStyle::GetSmoothing()
{
  return font->GetSmoothing();
}

Bool wxStyle::GetUnderlined()
{
  return font->GetUnderlined();
}

Bool wxStyle::GetSizeInPixels()
{
  return font->GetSizeInPixels();
}

Bool wxStyle::GetTransparentTextBacking()
{
  return transText;
}

wxColour *wxStyle::GetForeground()
{
  return foreground;
}

wxColour *wxStyle::GetBackground()
{
  return background;
}

int wxStyle::GetAlignment()
{
  return alignment;
}

Bool wxStyle::IsJoin()
{
  return !!join_shiftStyle;
}

void wxStyle::GetDelta(wxStyleDelta *d)
{
  if (join_shiftStyle)
    d->SetDelta(wxCHANGE_NOTHING);
  else
    d->Copy(nonjoin_delta);
}

void wxStyle::SetDelta(wxStyleDelta *d)
{
  if (join_shiftStyle || (styleList && PTREQ(this, styleList->BasicStyle())))
    return;

  if (!nonjoin_delta->Equal(d)) {
    nonjoin_delta->Copy(d);
    Update();
  }
}

wxStyle *wxStyle::GetShiftStyle()
{
  if (join_shiftStyle)
    return join_shiftStyle;
  else if (styleList)
    return styleList->BasicStyle();
  else
    return wxTheStyleList->BasicStyle();
}

void wxStyle::SetShiftStyle(wxStyle *style)
{
  if (!join_shiftStyle || !styleList || (styleList->StyleToIndex(style) < 0))
    return;

  if (styleList->CheckForLoop(this, style))
    return;

  if (join_shiftStyle)
    join_shiftStyle->children->DeleteObject(this);
  style->children->Append(this);

  join_shiftStyle = style;
  styleList->StyleHasNewChild(style, this);

  Update();

  join_shiftStyle = style;
  Update();
}

wxStyle *wxStyle::GetBaseStyle(void)
{
  return baseStyle;
}

void wxStyle::SetBaseStyle(wxStyle *style)
{
  if (!styleList || PTREQ(this, styleList->BasicStyle()))
    return;

  if (!style)
    style = styleList->BasicStyle();
  else
    if (styleList->StyleToIndex(style) < 0)
      return;

  if (styleList->CheckForLoop(this, style))
    return;

  if (baseStyle)
    baseStyle->children->DeleteObject(this);

  baseStyle = style;
  style->children->Append(this);

  styleList->StyleHasNewChild(style, this);

  Update();
}

void wxStyle::SwitchTo(wxDC *dc, wxStyle *oldStyle)
{
  unsigned char afr, afg, afb, bfr, bfg, bfb;
  unsigned char abr, abg, abb, bbr, bbg, bbb;

  if (oldStyle) {
    oldStyle->foreground->Get(&afr, &afg, &afb);
    foreground->Get(&bfr, &bfg, &bfb);
    oldStyle->background->Get(&abr, &abg, &abb);
    background->Get(&bbr, &bbg, &bbb);
  }

  if (!oldStyle || oldStyle->font != font)
    dc->SetFont(font);
  if (!oldStyle || afr != bfr || afb != bfb || afg != bfg)
    dc->SetTextForeground(foreground);
  if (!oldStyle || abr != bbr || abb != bbb || abg != bbg)
    dc->SetTextBackground(background);
  if (!oldStyle || oldStyle->pen != pen)
    dc->SetPen(pen);
  if (!oldStyle || oldStyle->transText != transText)
    dc->SetBackgroundMode(transText ? wxTRANSPARENT : wxSOLID);
}

void wxStyle::ResetTextMetrics(wxDC *dc)
{
  double w, h, d, s;

  textMetricDC = dc;
#ifdef BROKEN_GET_TEXT_EXTENT 
  dc->SetFont(style->GetFont());
#endif
  dc->GetTextExtent(" ", &w, &h, &d, &s, font);
  textWidth = w;
  textHeight = h;
  textDescent = d;
  textSpace = s;
}

double wxStyle::GetTextWidth(wxDC *dc)
{
  if (dc != textMetricDC)
    ResetTextMetrics(dc);

  return textWidth;
}

double wxStyle::GetTextHeight(wxDC *dc)
{
  if (dc != textMetricDC)
    ResetTextMetrics(dc);

  return textHeight;
}

double wxStyle::GetTextDescent(wxDC *dc)
{
  if (dc != textMetricDC)
    ResetTextMetrics(dc);

  return textDescent;
}

double wxStyle::GetTextSpace(wxDC *dc)
{
  if (dc != textMetricDC)
    ResetTextMetrics(dc);

  return textSpace;
}

/***************************************************************/

class NotificationRec {
 public:
  wxStyleNotifyFunc f;
  void *data;
#ifdef MZ_PRECISE_GC
# define GET_WEAK_DATA(data) SCHEME_BOX_VAL(data)
#else
# define GET_WEAK_DATA(data) data
#endif
  void *id;
};

wxStyleList::wxStyleList() : wxList(wxKEY_NONE, WXGC_NO_CLEANUP)
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_STYLE_LIST;
#endif

  basic = new wxStyle;
  /* Note: The file-reader relies on having a new `basic' when the
     list is cleared: */

  basic->styleList = this;

  basic->name = "Basic";
  basic->baseStyle = NULL;

  basic->nonjoin_delta = new wxStyleDelta;
  basic->nonjoin_delta->SetDelta(wxCHANGE_NORMAL);

  basic->font = wxTheFontList->FindOrCreateFont(defaultSize, wxDEFAULT,
						wxNORMAL, wxNORMAL);
  basic->foreground->CopyFrom(wxBLACK);
  basic->background->CopyFrom(wxWHITE);
  basic->pen = wxThePenList->FindOrCreatePen(basic->foreground, 0, wxSOLID);
  basic->brush = wxTheBrushList->FindOrCreateBrush(basic->background, wxSOLID);
  basic->alignment = wxALIGN_BOTTOM;
  basic->transText = TRUE;

  Append(basic);

  notifications = new wxList(wxKEY_NONE, FALSE);
}

void wxStyleList::Copy(wxStyleList *other)
{
  wxNode *node;

  for (node = other->First(); node; node = node->Next()) {
    wxStyle *s;
    s = (wxStyle *)node->Data();
    Convert(s, 1);
  }
}

wxStyleList::~wxStyleList()
{
  DeleteContents(TRUE);
}

wxStyle *wxStyleList::BasicStyle(void)
{
  return basic;
}

wxStyle *wxStyleList::FindOrCreateStyle(wxStyle *baseStyle, 
					wxStyleDelta *deltain)
{
  wxNode *node;
  wxStyle *style;
  wxStyleDelta *delta;

  if (!baseStyle || (StyleToIndex(baseStyle) < 0))
    baseStyle = basic;

  /* Collapse the delta: */
  if (quick_delta) {
    delta = quick_delta;
    quick_delta = NULL;
  } else
    delta = new wxStyleDelta;
  delta->Copy(deltain);
  while (!baseStyle->name && !baseStyle->join_shiftStyle) {
    if (!delta->Collapse(baseStyle->nonjoin_delta))
      break;
    baseStyle = baseStyle->baseStyle;
  }

  for (node = First(); node; node = node->Next()) {
    style = (wxStyle *)node->Data();
    if (!style->name
	&& !style->join_shiftStyle
	&& PTREQ(style->baseStyle, baseStyle)
	&& delta->Equal(style->nonjoin_delta)) {
      quick_delta = delta;
      return style;
    }
  }

  style = new wxStyle;

  style->styleList = this;

  style->name = NULL;

  style->nonjoin_delta = delta;

  style->baseStyle = baseStyle;
  baseStyle->children->Append(style);

  style->Update();

  Append(style);

  return style;
}

wxStyle *wxStyleList::FindOrCreateJoinStyle(wxStyle *baseStyle, 
					    wxStyle *shiftStyle)
{
  wxNode *node;
  wxStyle *style;

  if (!baseStyle || (StyleToIndex(baseStyle) < 0))
    baseStyle = basic;
  if (!shiftStyle || (StyleToIndex(shiftStyle) < 0))
    return baseStyle;

  for (node = First(); node; node = node->Next()) {
    style = (wxStyle *)node->Data();
    if (!style->name
	&& style->join_shiftStyle
	&& PTREQ(style->baseStyle, baseStyle)
	&& PTREQ(style->join_shiftStyle, shiftStyle))
      return style;
  }

  style = new wxStyle;

  style->styleList = this;

  style->name = NULL;

  style->join_shiftStyle = shiftStyle;
  shiftStyle->children->Append(style);

  style->baseStyle = baseStyle;
  baseStyle->children->Append(style);

  style->Update();

  Append(style);

  return style;
}

wxStyle *wxStyleList::FindNamedStyle(char *name)
{
  wxNode *node;
  wxStyle *style;

  for (node = First(); node; node = node->Next()) {
    style = (wxStyle *)node->Data();
    if (style->name && !strcmp(name, style->name))
      return style;
  }

  return NULL;
}

wxStyle *wxStyleList::DoNamedStyle(char *name, wxStyle *plainStyle, Bool replac)
{
  wxNode *node;
  wxStyle *style;

  if (!plainStyle || (StyleToIndex(plainStyle) < 0))
    plainStyle = basic;

  style = NULL;
  for (node = First(); node; node = node->Next()) {
    style = (wxStyle *)node->Data();
    if (style->name && !strcmp(name, style->name)) {
      if (!replac)
	return style;
      break;
    }
  }

  if (!node) {
    style = new wxStyle;
    style->name = copystring(name);
    style->styleList = this;
  } else {
    /* Can't replace basic style: */
    if (style == basic)
      return basic;

    /* plainStyle must not depend on this style
       (otherwise, we'd create a dependency cycle): */
    if (CheckForLoop(style, plainStyle))
      return style;

    style->baseStyle->children->DeleteObject(style);
    if (style->join_shiftStyle)
      style->join_shiftStyle->children->DeleteObject(style);
  }

  if (plainStyle->join_shiftStyle) {
    style->join_shiftStyle = plainStyle->join_shiftStyle;
    style->join_shiftStyle->children->Append(style);
  } else {
    style->nonjoin_delta = new wxStyleDelta;
    if (PTRNE(plainStyle, basic))
      style->nonjoin_delta->Copy(plainStyle->nonjoin_delta);
  }

  if (PTREQ(plainStyle, basic))
    style->baseStyle = basic;
  else
    style->baseStyle = plainStyle->baseStyle;
  style->baseStyle->children->Append(style);

  style->Update();

  if (!node)
    Append(style);

  return style;
}

wxStyle *wxStyleList::NewNamedStyle(char *name, wxStyle *plainStyle)
{
  return DoNamedStyle(name, plainStyle, FALSE);
}

wxStyle *wxStyleList::ReplaceNamedStyle(char *name, wxStyle *plainStyle)
{
  return DoNamedStyle(name, plainStyle, TRUE);
}

wxStyle *wxStyleList::Convert(wxStyle *style, Bool overwrite)
{
  wxStyle *base, *newstyle;

  if (StyleToIndex(style) >= 0)
    return style;

  if (style->name && !overwrite) {
    newstyle = FindNamedStyle(style->name);
    if (newstyle)
      return newstyle;
  }

  if (!style->baseStyle)
    base = BasicStyle();
  else
    base = Convert(style->baseStyle);

  if (style->join_shiftStyle) {
    wxStyle *shift;

    shift = Convert(style->join_shiftStyle);
    newstyle = FindOrCreateJoinStyle(base, shift);
  } else
    newstyle = FindOrCreateStyle(base, style->nonjoin_delta);

  if (style->name) {
    if (overwrite)
      return ReplaceNamedStyle(style->name, newstyle);
    else
      return NewNamedStyle(style->name, newstyle);
  } else
    return newstyle;
}

void wxStyleList::StyleWasChanged(wxStyle *which)
{
  NotificationRec *rec;
  wxNode *node;

  for (node = notifications->First(); node; node = node->Next()) {
    rec = (NotificationRec *)node->Data();
    rec->f(which, GET_WEAK_DATA(rec->data));
  }
}

void *wxStyleList::NotifyOnChange(wxStyleNotifyFunc f, void *data, int weak)
{
  NotificationRec *rec, *orec;
  wxNode *node;

#ifdef MZ_PRECISE_GC
  rec = new NotificationRec;
  if (weak) {
    rec->data = GC_malloc_weak_box(data, NULL, 0);
  } else {
    void *weak_data;
    weak_data = (void *)scheme_box((Scheme_Object *)data);
    GC_finalization_weak_ptr((void **)weak_data, 
			     ((void **)&SCHEME_BOX_VAL(weak_data)
			      - (void **)weak_data));
    rec->data = weak_data;
  }
#else
  if (weak)
    rec = new WXGC_ATOMIC NotificationRec;
  else
    rec = new NotificationRec;
  rec->data = data;
  if (weak)
    scheme_weak_reference((void **)&rec->data);
  else
    WXGC_IGNORE(rec, rec->data);
#endif
  
  rec->f = f;
  rec->id = scheme_make_symbol("notify-change-key");
    
  /* Look for dropped weak entries to replace: */
  for (node = notifications->First(); node; node = node->Next()) {
    orec = (NotificationRec *)node->Data();
    if (!GET_WEAK_DATA(orec->data)) {
      node->SetData((wxObject *)rec);
      return rec->id;
    }
  }

  notifications->Append((wxObject *)rec);

  return rec->id;
}

void wxStyleList::ForgetNotification(void *id)
{
  NotificationRec *rec;
  wxNode *node;
  
  for (node = notifications->First(); node; node = node->Next()) {
    rec = (NotificationRec *)node->Data();
    if (rec->id == id) {
      notifications->DeleteNode(node);
      DELETE_OBJ rec;
      return;
    }
  }
}

Bool wxStyleList::CheckForLoop(wxStyle *s, wxStyle *p)
{
  if (PTREQ(p, s))
    return TRUE;

  if (!p->baseStyle)
    return FALSE;

  if (p->join_shiftStyle) {
    if (CheckForLoop(s, p->baseStyle))
      return TRUE;
    return CheckForLoop(s, p->join_shiftStyle);
  } else
    return CheckForLoop(s, p->baseStyle);
}

void wxStyleList::StyleHasNewChild(wxStyle *s, wxStyle *c)
{
  wxNode *cnode, *snode, *node;

  /* Need to maintain the invariant that parents are in the list
     before children... */
  cnode = Member(c);
  snode = Member(s);
  
  for (node = cnode; node; node = node->Next()) {
    if (PTREQ(node, snode)) {
      /* Move base style to before this style */
      DeleteNode(snode);
      Insert(cnode, s);
      break;
    }
  }
}

int wxStyleList::Number(void)
{
  return wxList::Number();
}

wxStyle *wxStyleList::IndexToStyle(int i)
{
  wxNode *node;

  for (node = First(); i && node; i--, node = node->Next()) {
  }

  if (node)
    return (wxStyle *)node->Data();
  else
    return NULL;
}
 
int wxStyleList::StyleToIndex(wxStyle *s)
{
  wxNode *node;
  int i = 0;

  for (node = First(); 
       node && PTRNE((wxStyle *)node->Data(), s); 
       i++, node = node->Next()) {
  }

  if (node)
    return i;
  else
    return -1;
}

Bool wxStyleList::WriteToFile(class wxMediaStreamOut *f)
{
  return wxmbWriteStylesToFile(this, f);
}

wxStyle *wxStyleList::MapIndexToStyle(wxMediaStream *s, int i, long listId)
{
  wxStyleListLink *ssl;

  for (ssl = s->ssl; ssl; ssl = ssl->next) {
    if (ssl->listId == listId) {
      if (ssl->basic == basic) {
	// If basic changes, that means list was cleared
	if (ssl->styleMap && i < ssl->numMappedStyles)
	  return ssl->styleMap[i];
	else {
	  wxmeError("map-index-to-style: bad style index for snip");
	  return basic;
	}
      } else {
	wxmeError("map-index-to-style: cannot resolve style index; style list has been cleared");
	return basic;
      }
    }
  }

  wxmeError("map-index-to-style: bad style list index for snip");

  return basic;
}

wxStyleList *wxReadStyleList(class wxMediaStreamIn *f)
{
  wxStyleList *l;
  long listId;

  l = new wxStyleList;

  return wxmbReadStylesFromFile(l, f, 0, &listId);
}

void wxmbSetupStyleReadsWrites(wxMediaStream *s)
{
  s->ssl = NULL;
}

void wxmbDoneStyleReadsWrites(wxMediaStream *s)
{
  s->ssl = NULL;
}

static int FamilyStandardToThis(int v)
{
  switch (v) {
  case wxBASE:
    return wxBASE;
  case 71:
    return wxDECORATIVE;
  case 72:
    return wxROMAN;
  case 73:
    return wxSCRIPT;
  case 74:
    return wxSWISS;
  case 75:
    return wxMODERN;
  case 76:
    return wxTELETYPE;
  case 77:
    return wxSYSTEM;
  case 78:
    return wxSYMBOL;
  case 70:
  default:
    return wxDEFAULT;
  }
}

static int FamilyThisToStandard(int v)
{
  switch (v) {
  case wxBASE:
    return wxBASE;
  case wxDECORATIVE:
    return 71;
  case wxROMAN:
    return 72;
  case wxSCRIPT:
    return 73;
  case wxSWISS:
    return 74;
  case wxMODERN:
    return 75;
  case wxTELETYPE:
    return 76;
  case wxSYSTEM:
    return 77;
  case wxSYMBOL:
    return 78;
  case wxDEFAULT:
  default:
    return 70;
  }
}

static int WeightStandardToThis(int v)
{
  switch (v) {
  case wxBASE:
    return wxBASE;
  case 91:
    return wxLIGHT;
  case 92:
    return wxBOLD;
  case 90:
  default:
    return wxNORMAL;
  }
}

static int WeightThisToStandard(int v)
{
  switch (v) {
  case wxBASE:
    return wxBASE;
  case wxLIGHT:
    return 91;
  case wxBOLD:
    return 92;
  case wxNORMAL:
  default:
    return 90;
  }
}

static int StyleStandardToThis(int v)
{
  switch (v) {
  case wxBASE:
    return wxBASE;
  case 93:
    return wxITALIC;
  case 94:
    return wxSLANT;
  case 90:
  default:
    return wxNORMAL;
  }
}

static int StyleThisToStandard(int v)
{
  switch (v) {
  case wxBASE:
    return wxBASE;
  case wxITALIC:
    return 93;
  case wxSLANT:
    return 94;
  case wxNORMAL:
  default:
    return 90;
  }
}

static int SmoothingStandardToThis(int v)
{
  switch (v) {
  case wxBASE:
    return wxBASE;
  case 0:
    return wxSMOOTHING_PARTIAL;
  case 1:
    return wxSMOOTHING_ON;
  case 2:
    return wxSMOOTHING_OFF;
  case 3:
  default:
    return wxSMOOTHING_DEFAULT;
  }
}

static int SmoothingThisToStandard(int v)
{
  switch (v) {
  case wxBASE:
    return wxBASE;
  case wxSMOOTHING_PARTIAL:
    return 0;
  case wxSMOOTHING_ON:
    return 1;
  case wxSMOOTHING_OFF:
    return 2;
  case wxNORMAL:
  default:
    return 3;
  }
}


static int AlignStandardToThis(int v)
{
  switch (v) {
  case wxBASE:
    return wxBASE;
  case 0:
    return wxALIGN_TOP;
  case 2:
    return wxALIGN_CENTER;
  case 1:
  default:
    return wxALIGN_BOTTOM;
  }
}

static int AlignThisToStandard(int v)
{
  switch (v) {
  case wxBASE:
    return wxBASE;
  case wxALIGN_TOP:
    return 0;
  case wxALIGN_CENTER:
    return 2;
  case wxALIGN_BOTTOM:
  default:
    return 1;
  }
}

wxStyleList *wxmbReadStylesFromFile(wxStyleList *styleList, 
				    wxMediaStreamIn *f, 
				    Bool overwritename,
				    long *_listId)
{
#define MAX_STYLE_NAME 256
  int baseIndex, shiftIndex;
  long nameSize;
  char name[MAX_STYLE_NAME];
  char face[MAX_STYLE_NAME];
  short r, g, b;
  int i, isJoin, listId, nms, num;
  double flt;
  wxStyleDelta *delta;
  wxStyle *bs;
  wxStyleListLink *ssl;

  f->Get(&listId);
  
  *_listId = listId;

  for (ssl = f->ssl; ssl; ssl = ssl->next) {
    if (ssl->listId == listId)
      return ssl->styleList;
  }

  ssl = new wxStyleListLink;
  ssl->styleList = styleList;
  ssl->listId = listId;
  ssl->basic = styleList->BasicStyle();
  ssl->next = f->ssl;
  f->ssl = ssl;

  f->Get(&nms);
  ssl->numMappedStyles = nms;
  ssl->styleMap = new wxStyle*[ssl->numMappedStyles];

  bs = styleList->BasicStyle();
  ssl->styleMap[0] = bs;
  for (i = 1; i < ssl->numMappedStyles; i++) {
    f->Get(&baseIndex);

    if (baseIndex >= i) {
      wxmeError("map-index-to-style: bad style index");
      return FALSE;
    }

    nameSize = MAX_STYLE_NAME;
    f->Get((long *)&nameSize, (char *)name);

    f->Get(&isJoin);

    if (isJoin) {
      wxStyle *js;

      f->Get(&shiftIndex);

      js = styleList->FindOrCreateJoinStyle(ssl->styleMap[baseIndex], 
					    ssl->styleMap[shiftIndex]);
      ssl->styleMap[i] = js;
    } else {
      int fam;

      delta = new wxStyleDelta;
      
      f->Get(&fam);
      delta->family = FamilyStandardToThis(fam);

      nameSize = MAX_STYLE_NAME;
      f->Get((long *)&nameSize, (char *)face);
      
      if (*face) {
	char *str;
	str = copystring(face);
	delta->face = str;
      } else
	delta->face = NULL;

      // printf("%d %s\n", delta->family, delta->face ? delta->face : "NULL");
      
      f->Get(&flt); delta->sizeMult = flt;
      f->Get(&num); delta->sizeAdd = num;
      f->Get(&num);
      delta->weightOn = WeightStandardToThis(num);
      f->Get(&num);
      delta->weightOff = WeightStandardToThis(num);
      f->Get(&num);
      delta->styleOn = StyleStandardToThis(num);
      f->Get(&num);
      delta->styleOff = StyleStandardToThis(num);
      if (WXME_VERSION_ONE(f) || WXME_VERSION_TWO(f)
	  || WXME_VERSION_THREE(f) || WXME_VERSION_FOUR(f)) {
	delta->smoothingOn = wxSMOOTHING_DEFAULT;
	delta->smoothingOff = wxSMOOTHING_DEFAULT;
      } else {
	f->Get(&num);
	delta->smoothingOn = SmoothingStandardToThis(num);
	f->Get(&num);
	delta->smoothingOff = SmoothingStandardToThis(num);
      }
      f->Get(&num); delta->underlinedOn = num;
      f->Get(&num); delta->underlinedOff = num;
      if (WXME_VERSION_ONE(f) || WXME_VERSION_TWO(f)
	  || WXME_VERSION_THREE(f) || WXME_VERSION_FOUR(f)
	  || WXME_VERSION_FIVE(f)) {
	delta->sipOn = FALSE;
	delta->sipOff = FALSE;
      } else {
	f->Get(&num); delta->sipOn = num;
	f->Get(&num); delta->sipOff = num;
      }
      if (WXME_VERSION_ONE(f) || WXME_VERSION_TWO(f)) {
	delta->transparentTextBackingOn = FALSE;
	delta->transparentTextBackingOff = FALSE;
      } else {
	f->Get(&num); delta->transparentTextBackingOn = num;
	f->Get(&num); delta->transparentTextBackingOff = num;
      }
      
      f->Get(&flt); delta->foregroundMult->r = flt;
      f->Get(&flt); delta->foregroundMult->g = flt;
      f->Get(&flt); delta->foregroundMult->b = flt;
      f->Get(&flt); delta->backgroundMult->r = flt;
      f->Get(&flt); delta->backgroundMult->g = flt;
      f->Get(&flt); delta->backgroundMult->b = flt;
      f->Get(&r);
      f->Get(&g);
      f->Get(&b);
      delta->foregroundAdd->Set(r, g, b);
      f->Get(&r);
      f->Get(&g);
      f->Get(&b);
      delta->backgroundAdd->Set(r, g, b);
      if (WXME_VERSION_ONE(f) || WXME_VERSION_TWO(f)) {
	if (r || g || b)
	  delta->transparentTextBackingOff = TRUE;
      }

      f->Get(&num);
      delta->alignmentOn = AlignStandardToThis(num);
      f->Get(&num);
      delta->alignmentOff = AlignStandardToThis(num);

      {
	wxStyle *cs;
	cs = styleList->FindOrCreateStyle(ssl->styleMap[baseIndex], delta);
	ssl->styleMap[i] = cs;
      }
    }

    if (*name) {
      wxStyle *ns;
      ns = (overwritename 
	    ? styleList->ReplaceNamedStyle(name, ssl->styleMap[i])
	    : styleList->NewNamedStyle(name, ssl->styleMap[i]));
      ssl->styleMap[i] = ns;
    }
  }

  return styleList;
}

Bool wxmbWriteStylesToFile(wxStyleList *styleList, wxMediaStreamOut *f)
{
  int i, count, lid;
  wxStyle *style;
  short r, g, b;
  char *name;
  wxStyleDelta *delta;
  wxStyleListLink *ssl;

  for (ssl = f->ssl; ssl; ssl = ssl->next) {
    if (ssl->styleList == styleList) {
      f->Put(ssl->listId);
      return TRUE;
    }
  }

  lid = f->styleCount + 1;
  f->styleCount++;

  ssl = new wxStyleListLink;
  ssl->styleList = styleList;
  ssl->listId = lid;
  ssl->next = f->ssl;
  f->ssl = ssl;
  
  f->Put(lid);

  count = styleList->Number();

  f->Put(count);

  // Basic style is implied

  for (i = 1; i < count; i++) {
    int idx;
    wxStyle *bs;
    
    style = styleList->IndexToStyle(i);

    bs = style->GetBaseStyle();
    idx = styleList->StyleToIndex(bs);
    f->Put(idx);
    name = style->GetName();
    if (name)
      f->Put(name);
    else
      f->Put("");

    if (style->IsJoin()) {
      int idx;
      wxStyle *ss;

      f->Put(1);

      ss = style->GetShiftStyle();
      idx = styleList->StyleToIndex(ss);
      f->Put(idx);
    } else {
      delta = new wxStyleDelta;

      style->GetDelta(delta);
      
      f->Put(0);

      f->Put(FamilyThisToStandard(delta->family));
      if (delta->face)
	f->Put(delta->face);
      else
	f->Put("");

      f->Put(delta->sizeMult);
      f->Put(delta->sizeAdd);
      f->Put(WeightThisToStandard(delta->weightOn)); 
      f->Put(WeightThisToStandard(delta->weightOff));
      f->Put(StyleThisToStandard(delta->styleOn));
      f->Put(StyleThisToStandard(delta->styleOff));
      f->Put(SmoothingThisToStandard(delta->smoothingOn));
      f->Put(SmoothingThisToStandard(delta->smoothingOff));
      f->Put(delta->underlinedOn);
      f->Put(delta->underlinedOff);
      f->Put(delta->sipOn);
      f->Put(delta->sipOff);
      f->Put(delta->transparentTextBackingOn);
      f->Put(delta->transparentTextBackingOff);

      f->Put(delta->foregroundMult->r);
      f->Put(delta->foregroundMult->g);
      f->Put(delta->foregroundMult->b);
      f->Put(delta->backgroundMult->r);
      f->Put(delta->backgroundMult->g);
      f->Put(delta->backgroundMult->b);
      delta->foregroundAdd->Get(&r, &g, &b);
      f->Put(r);
      f->Put(g);
      f->Put(b);
      delta->backgroundAdd->Get(&r, &g, &b);
      f->Put(r);
      f->Put(g);
      f->Put(b);

      f->Put(AlignThisToStandard(delta->alignmentOn));
      f->Put(AlignThisToStandard(delta->alignmentOff));
    }
  }

  return TRUE;
}
