/*
 * File:        wx_msnip.cc
 * Purpose:     wxMediaSnip implementation
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
#include "wx_media.h"

#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <locale.h>

#include "wx_mpriv.h"
#include "wx_gcrct.h"

wxMediaSnip::wxMediaSnip(wxMediaBuffer *useme,
			 Bool border,
			 int lm, int tm, int rm, int bm,
			 int li, int ti, int ri, int bi,
			 double w, double W, double h, double H)
{
  Bool istemp;
  wxSnipClassList *scl;

#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_MEDIA_SNIP;
#endif

  flags |= wxSNIP_HANDLES_EVENTS;

  scl = &wxTheSnipClassList;
  snipclass = scl->Find("wxmedia");

  withBorder = border;
  leftMargin = lm;
  topMargin = tm;
  rightMargin = rm;
  bottomMargin = bm;
  leftInset = li;
  topInset = ti;
  rightInset = ri;
  bottomInset = bi;

  minWidth = w;
  maxWidth = W;
  minHeight = h;
  maxHeight = H;

  if (useme && !useme->GetAdmin()) {
    me = useme;
  } else {
    me = wxsMakeMediaEdit();
  }
  {
    wxMediaSnipMediaAdmin *msma;
    msma = new wxMediaSnipMediaAdmin(this);
    myAdmin = msma;
  }

  if (!me->GetFilename(&istemp) || istemp)
    /* Turn on flag to mirror filename: */
    flags |= wxSNIP_USES_BUFFER_PATH;

  me->OwnCaret(FALSE);
}

wxMediaSnip::~wxMediaSnip()
{
#if 1
  /* The only way a snip should be deleted if by GC, and it wouldn't
     get GC'd if the editor isn't also going to be GCd. It's important
     *not* to call SetAdmin(), because that can trigger Scheme
     code. */
  DELETE_OBJ me;
  me = NULL;
#else
  if (me) {
    if (me->GetAdmin() == myAdmin)
      me->SetAdmin(NULL);
  }
#endif

  DELETE_OBJ myAdmin;
}

void wxMediaSnip::SetAdmin(wxSnipAdmin *a)
{
  if (PTRNE(a, admin)) {
    wxSnip::SetAdmin(a);
    if (me) {
      if (a) {
	if (me->GetAdmin())
	  me = NULL; /* traitor! - get rid of it */
	else
	  me->SetAdmin(myAdmin);
      } else
	me->SetAdmin(NULL);
    }
  } 

  if (admin && (flags & wxSNIP_USES_BUFFER_PATH)) {
    /* Propogate a filename change: */
    Bool istemp;
    if (me && (!me->GetFilename(&istemp) || istemp)) {
      wxMediaBuffer *b;
      b = admin->GetMedia();
      if (b) {
	char *filename;
	filename = b->GetFilename();
	if (filename)
	  me->SetFilename(filename, TRUE);
      }
    } else
      flags -= wxSNIP_USES_BUFFER_PATH; /* Turn off the flag; not needed */
  }
}

void wxMediaSnip::SetMedia(wxMediaBuffer *b)
{
  if (me == b)
    return;

  if (me && admin)
    me->SetAdmin(NULL);
  me = b;
  if (b) {
    if (b->GetAdmin()) {
      me = NULL;
      return;
    } else if (admin) {
      me->SetAdmin(myAdmin);
    }
  }

  if (admin)
    admin->Resized(this, TRUE);
}

wxMediaBuffer *wxMediaSnip::GetThisMedia(void)
{
  return me;
}

wxCursor *wxMediaSnip::AdjustCursor(wxDC *dc, double x, double y, 
				    double, double, wxMouseEvent *event)
{
  wxMSMA_SnipDrawState *save;
  wxCursor *c;

  if (!me) 
    return NULL;

  save = new wxMSMA_SnipDrawState;
  myAdmin->SaveState(save, dc, x, y);
  c = me->AdjustCursor(event);
  myAdmin->RestoreState(save);

  return c;
}

void wxMediaSnip::OnEvent(wxDC *dc, double x, double y, 
			  double, double, wxMouseEvent *event)
{
  wxMSMA_SnipDrawState *save;

  if (!me) return;

  save = new wxMSMA_SnipDrawState;
  myAdmin->SaveState(save, dc, x, y);
  me->OnEvent(event);
  myAdmin->RestoreState(save);
}

void wxMediaSnip::OnChar(wxDC *dc, double x, double y, 
			  double, double, wxKeyEvent *event)
{
  wxMSMA_SnipDrawState *save;

  if (!me) return;

  save = new wxMSMA_SnipDrawState;
  myAdmin->SaveState(save, dc, x, y);
  me->OnChar(event);
  myAdmin->RestoreState(save);
}

void wxMediaSnip::OwnCaret(Bool ownit)
{
  if (me)
    me->OwnCaret(ownit);
}

void wxMediaSnip::BlinkCaret(wxDC *dc, double x, double y)
{
  if (me) {
    wxMSMA_SnipDrawState *save;

    save = new wxMSMA_SnipDrawState;
    myAdmin->SaveState(save, dc, x, y);
    me->BlinkCaret();
    myAdmin->RestoreState(save);
  }
}

void wxMediaSnip::DoEdit(int op, Bool recursive, long time)
{
  if (me)
    me->DoEdit(op, recursive, time);
}

Bool wxMediaSnip::CanEdit(int op, Bool recursive)
{
  if (me)
    return me->CanEdit(op, recursive);
  else
    return FALSE;
}

void wxMediaSnip::DoFont(int op, Bool recursive)
{
  if (me)
    me->DoFont(op, recursive);
}

Bool wxMediaSnip::Match(wxSnip *)
{
  return FALSE;
}

void wxMediaSnip::SizeCacheInvalid(void)
{
  if (me)
    me->SizeCacheInvalid();
}

wxchar *wxMediaSnip::GetText(long offset, long num, Bool flat, long *got)
{
  if (offset >= 1 || !num) {
    if (got) *got = 0;
    return wx_empty_wxstr;
  }

  if (!flat) {
    wxchar *s;
    s = new wxchar[2];
    s[0] = '.';
    s[1] = 0;
    if (got) *got = 1;
    return s;
  } else if (me)
    return me->GetFlattenedText(got);
  else
    return wx_empty_wxstr;
}


void wxMediaSnip::GetExtent(wxDC *dc, 
			    double x, double y,
			    double *w, double *h,
			    double *_descent, double *_space,
			    double *lspace, double *rspace)
{
  wxMSMA_SnipDrawState *save;
  double descent, space, origH, dummyH;

  save = new wxMSMA_SnipDrawState;
  myAdmin->SaveState(save, dc, x, y);
  
  if (!h && alignTopLine)
    h = &dummyH;

  if (me)
    me->GetExtent(w, h);
  else {
    if (w)
      *w = 0;
    if (h)
      *h = 0;
  }

  origH = alignTopLine ? *h : 0.0;

  if (w) {
    if (me && (me->bufferType == wxEDIT_BUFFER)) {
      if (tightFit)
	(*w) -= CURSOR_WIDTH;
      else
	--(*w); /* It still looks better to subtract 1 */
      if (*w < 0)
	*w = 0;
    }
    if (*w < minWidth)
      *w = minWidth;
    else if (maxWidth > 0 && *w > maxWidth)
      *w = maxWidth;

    (*w) += rightMargin + leftMargin;
  }
  if (h) {
    if (me && (me->bufferType == wxEDIT_BUFFER)) {
      if (tightFit) {
	(*h) -= ((wxMediaEdit *)me)->GetLineSpacing();
	if (*h < 0)
	  *h = 0;
      }
    }
    if (*h < minHeight)
      *h = minHeight;
    else if (maxHeight > 0 && *h > maxHeight)
      *h = maxHeight;

    (*h) += topMargin + bottomMargin;
  }

  {
    descent = (me ? me->GetDescent() : 0.0) + bottomMargin;
    if (me && (me->bufferType == wxEDIT_BUFFER)) {
      if (alignTopLine)
	descent = origH - ((wxMediaEdit *)me)->GetTopLineBase() + bottomMargin;
      if (tightFit) {
	descent -= ((wxMediaEdit *)me)->GetLineSpacing();
	if (descent < 0)
	  descent = 0;
      }
    }
  }
  space = (me ? me->GetSpace() : 0.0) + topMargin;

  if (maxHeight > 0 && (descent + space >= maxHeight + topMargin + bottomMargin)) {
    /* Just give up on spaces in this case: */
    space = topMargin;
    descent = bottomMargin;
  }

  if (_descent)
    *_descent = descent;
  if (_space)
    *_space = space;
  if (lspace)
    *lspace = leftMargin;
  if (rspace)
    *rspace = rightMargin;

  myAdmin->RestoreState(save);
}
	       
void wxMediaSnip::Draw(wxDC *dc, double x, double y, 
		       double left, double top, double right, double bottom,
		       double WXUNUSED(dx), double WXUNUSED(dy), 
		       int show_caret)
{
  double w, h, r, b, orig_x, orig_y;
  double t, l;
  wxColour *bgColor = wxWHITE;

  wxMSMA_SnipDrawState *save;

  save = new wxMSMA_SnipDrawState;
  myAdmin->SaveState(save, dc, x, y);
  
  if (me) {
    w = h = 0.0;
    me->GetExtent(&w, &h);
    if (me && (me->bufferType == wxEDIT_BUFFER)) {
      if (tightFit)
	w -= CURSOR_WIDTH;
      else
	--w; /* It still looks better to subtract 1 */
      if (w < 0)
	w = 0;
    }
    if (me && (me->bufferType == wxEDIT_BUFFER)) {
      if (tightFit) {
	h -= ((wxMediaEdit *)me)->GetLineSpacing();
	if (h < 0)
	  h = 0;
      }
    }
  } else 
    w = h = 0.0;

  if (w < minWidth)
    w = minWidth;
  else if (maxWidth > 0 && w > maxWidth)
    w = maxWidth;

  if (h < minHeight)
    h = minHeight;
  else if (maxHeight > 0 && h > maxHeight)
    h = maxHeight;

  orig_x = x;
  orig_y = y;

  x += leftMargin;
  y += topMargin;
  r = x + w;
  b = y + h;
  
  l = ((x > left) ? x : left);
  t = ((y > top) ? y : top);
  r = ((r < right) ? r : right);
  b = ((b < bottom) ? b : bottom);

  if (me)
    me->Refresh(l - x, t - y, r - l, b - t, show_caret, bgColor);

  if (withBorder) {
    double mt, ml, mb, mr;
    
    l = orig_x + leftInset;
    t = orig_y + topInset;
    r = l + (w + leftMargin + rightMargin - (leftInset + rightInset)) - 1;
    b = t + (h + topMargin + bottomMargin - (topInset + bottomInset)) - 1;
    
    ml = ((l > left) ? ((l < right) ? l : right) : left);
    mr = ((r > left) ? ((r < right) ? r : right) : left);
    mt = ((t > top) ? ((t < bottom) ? t : bottom) : top);
    mb = ((b > top) ? ((b < bottom) ? b : bottom) : top);

    if (l >= left && l < right  && mt < mb + GC_LINE_EXTEND)
      dc->DrawLine(l, mt, l, mb + GC_LINE_EXTEND);
    if (r >= left && r < right && mt < mb + GC_LINE_EXTEND)
      dc->DrawLine(r, mt, r, mb + GC_LINE_EXTEND);

    if (t >= top && t < bottom && ml < mr + GC_LINE_EXTEND)
      dc->DrawLine(ml, t, mr + GC_LINE_EXTEND, t);
    if (b >= top && b < bottom && ml < mr + GC_LINE_EXTEND)
      dc->DrawLine(ml, b, mr + GC_LINE_EXTEND, b);
  }

  myAdmin->RestoreState(save);
}

wxSnip *wxMediaSnip::Copy(void)
{
  wxMediaSnip *ms;
  wxMediaBuffer *mb;

  mb = (me ? me->CopySelf() : (wxMediaBuffer *)NULL);
  ms = wxsMakeMediaSnip(mb, 
			withBorder,
			leftMargin, topMargin,
			rightMargin, bottomMargin,
			leftInset, topInset,
			rightInset, bottomInset,
			minWidth, maxWidth,
			minHeight, maxHeight);

  /* Copy core snip info: */
  wxSnip::Copy(ms);

  ms->tightFit = tightFit;
  ms->alignTopLine = alignTopLine;

  if (!me)
    ms->SetMedia(NULL);

  return ms;
}

void wxMediaSnip::Write(wxMediaStreamOut *f)
{
  Bool wb = withBorder, tf = tightFit, ta = alignTopLine;

  f->Put((me ? me->bufferType : 0));
  f->Put(wb);
  f->Put(leftMargin);
  f->Put(topMargin);
  f->Put(rightMargin);
  f->Put(bottomMargin);
  f->Put(leftInset);
  f->Put(topInset);
  f->Put(rightInset);
  f->Put(bottomInset);
  f->Put(minWidth);
  f->Put(maxWidth);
  f->Put(minHeight);
  f->Put(maxHeight);
  f->Put(tf);
  f->Put(ta);

  if (me)
    me->WriteToFile(f);
}

void wxMediaSnip::SetMaxWidth(double w)
{
  maxWidth = w;
  if (admin)
    admin->Resized(this, TRUE);
}

void wxMediaSnip::SetMinWidth(double w)
{
  minWidth = w;
  if (admin)
    admin->Resized(this, TRUE);
}

void wxMediaSnip::SetMaxHeight(double h)
{
  maxHeight = h;
  if (admin)
    admin->Resized(this, TRUE);
}

void wxMediaSnip::SetMinHeight(double h)
{
  minHeight = h;
  if (admin)
    admin->Resized(this, TRUE);
}

double wxMediaSnip::GetMaxWidth(void) { return maxWidth; }
double wxMediaSnip::GetMaxHeight(void) { return maxHeight; }
double wxMediaSnip::GetMinWidth(void) { return minWidth; }
double wxMediaSnip::GetMinHeight(void) { return minHeight; }

Bool wxMediaSnip::GetTightTextFit(void)
{
  return tightFit;
}

void wxMediaSnip::SetTightTextFit(Bool t)
{
  tightFit = (t ? TRUE : FALSE);
  if (admin)
    admin->Resized(this, TRUE);
}

Bool wxMediaSnip::GetAlignTopLine(void)
{
  return alignTopLine;
}

void wxMediaSnip::SetAlignTopLine(Bool t)
{
  alignTopLine = (t ? TRUE : FALSE);
  if (admin)
    admin->Resized(this, TRUE);
}

Bool wxMediaSnip::Resize(double w, double h)
{
  w -= leftMargin + rightMargin;
  h -= topMargin + bottomMargin;
  if (w < 0)
    w = 0;
  if (h < 0)
    h = 0;
  minWidth = maxWidth = w;
  minHeight = maxHeight = h;

  if (me) {
    me->SetMaxWidth(w);
    me->SetMinWidth(w);
  }

  if (admin)
    admin->Resized(this, TRUE);

  return TRUE;
}

void wxMediaSnip::ShowBorder(Bool show)
{
  if ((withBorder ?  1 : 0) != (show ? 1 : 0)) {
    withBorder = (show ? TRUE : FALSE);
    if (admin) {
      wxDC *dc;
      double w, h;

      dc = admin->GetDC();
      if (dc) {
	w = h = 0.0;
	GetExtent(dc, 0, 0, &w, &h);
	admin->NeedsUpdate(this, leftInset, topInset, 
				  w + rightMargin - rightInset, 
				  h + bottomMargin - bottomInset);
      }
    }
  }
}

Bool wxMediaSnip::BorderVisible()
{
  return withBorder;
}

void wxMediaSnip::SetMargin(int lm, int tm, int rm, int bm)
{
  leftMargin = lm;
  topMargin = tm;
  rightMargin = rm;
  bottomMargin = bm;
  
  if (admin)
    admin->Resized(this, TRUE);
}

void wxMediaSnip::GetMargin(int *lm, int *tm, int *rm, int *bm)
{
  *lm = leftMargin;
  *tm = topMargin;
  *rm = rightMargin;
  *bm = bottomMargin;
}

void wxMediaSnip::SetInset(int lm, int tm, int rm, int bm)
{
  leftMargin = lm;
  topMargin = tm;
  rightMargin = rm;
  bottomMargin = bm;

  if (admin) {
    wxDC *dc;
    double w, h;

    dc = admin->GetDC();
    if (dc) {
      w = h = 0.0;
      GetExtent(dc, 0, 0, &w, &h);
      admin->NeedsUpdate(this, 0, 0, 
			 w + rightMargin + leftMargin,
			 h + bottomMargin + topMargin);
    }
  }
}

void wxMediaSnip::GetInset(int *li, int *ti, int *ri, int *bi)
{
  *li = leftInset;
  *ti = topInset;
  *ri = rightInset;
  *bi = bottomInset;
}

long wxMediaSnip::GetNumScrollSteps()
{
  return (me ? me->NumScrollLines() : 1);
}

long wxMediaSnip::FindScrollStep(double y)
{
  return (me ? me->FindScrollLine(y - topMargin) : 0);
}

double wxMediaSnip::GetScrollStepOffset(long i)
{
  return (me ? me->ScrollLineLocation(i) + topMargin : 0);
}

void wxMediaSnip::SetUnmodified()
{
  if (me)
    me->SetModified(FALSE);
}

/****************************************************************/

wxMediaSnipMediaAdmin::wxMediaSnipMediaAdmin(wxMediaSnip *s)
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_MEDIA_SNIP_MEDIA_ADMIN;
#endif

  snip = s;
  state = new wxMSMA_SnipDrawState;
  state->drawing = 0;

  WXGC_IGNORE(state, state->dc);
}

wxMediaSnipMediaAdmin::~wxMediaSnipMediaAdmin()
{
  state->dc = NULL;
}

wxDC *wxMediaSnipMediaAdmin::GetDC(double *xp, double *yp)
{
  if (state->drawing) {
    if (xp)
      *xp = -state->x;
    if (yp)
      *yp = -state->y;
  } else {
    if (xp)
      *xp = 0;
    if (yp)
      *yp = 0;
  }
  
  if (state->drawing)
    return state->dc;
  else {
    wxSnipAdmin *sadmin;
    sadmin = snip->GetAdmin();
    if (sadmin)
      return sadmin->GetDC();
    else
      return NULL;
  }
}

void wxMediaSnipMediaAdmin::SaveState(wxMSMA_SnipDrawState *save, wxDC *dc, 
				      double x, double y)
{
  save->drawing = state->drawing;
  save->dc = state->dc;
  save->x = state->x;
  save->y = state->y;

  state->drawing = TRUE;
  state->x = x + snip->leftMargin;
  state->y = y + snip->topMargin;
  state->dc = dc;
}

void wxMediaSnipMediaAdmin::RestoreState(wxMSMA_SnipDrawState *saved)
{
  state->drawing = saved->drawing;
  state->dc = saved->dc;
  state->x = saved->x;
  state->y = saved->y;
}

void wxMediaSnipMediaAdmin::GetView(double *x, double *y, double *w, double *h, 
				    Bool full)
{
  wxSnipAdmin *sadmin;
  sadmin = snip->GetAdmin();

  if (!sadmin) {
    if (x) *x = 0;
    if (y) *y = 0;
    if (w) *w = 0;
    if (h) *h = 0;
    return;  
  }

  if (full) {
    sadmin->GetView(x, y, w, h, NULL);
  } else {
    double sx, sy, sw, sh;
    sadmin->GetView(&sx, &sy, &sw, &sh, snip);
    if (x) {
      *x = sx - snip->leftMargin;
      if (*x < 0)
	*x = 0;
    }
    if (y) {
      *y = sy - snip->topMargin;
      if (*y < 0)
	*y = 0;
    }

    if (w || h) {
      if (sw || sh) {
	/* w and h might be too big due to margins - but
	   they might be small enough already because 
	   part of the snip itself is not viewed. */
	double rw, rh;
	
	/* We want the internal, non-overridden method: */
	snip->wxMediaSnip::GetExtent(state->dc, 0, 0, &rw, &rh);

	/* remember: sx and sy are in snip coordinates */

	if (w) {
	  double leftMargin, rightMargin;

	  leftMargin = snip->leftMargin - sx;
	  if (leftMargin < 0)
	    leftMargin = 0;
	  sw -= leftMargin;
	  rw -= snip->leftMargin;
	  
	  rightMargin = snip->rightMargin - (rw - sw);
	  if (rightMargin < 0)
	    rightMargin = 0;
	  sw -= rightMargin;

	  if (sw < 0) sw = 0;
	  *w = sw;
	}
	if (h) {
	  double topMargin, bottomMargin;

	  topMargin = snip->topMargin - sy;
	  if (topMargin < 0)
	    topMargin = 0;
	  sh -= topMargin;
	  rh -= snip->topMargin;

	  bottomMargin = snip->bottomMargin - (rh - sh);
	  if (bottomMargin < 0)
	    bottomMargin = 0;
	  sh -= bottomMargin;

	  if (sh < 0) sh = 0;
	  *h = sh;
	}
      } else {
	if (w) *w = 0;
	if (h) *h = 0;
      }
    }
  }
}

Bool wxMediaSnipMediaAdmin::ScrollTo(double localx, double localy, double w, double h,
				     Bool refresh, int bias)
{
  wxSnipAdmin *sadmin;
  sadmin = snip->GetAdmin();
  if (sadmin)
    return sadmin->ScrollTo(snip, localx + snip->leftMargin,
			    localy + snip->topMargin,
			    w, h, refresh, bias);
  else
    return FALSE;
}
	
void wxMediaSnipMediaAdmin::GrabCaret(int dist)
{
  wxSnipAdmin *sadmin;
  sadmin = snip->GetAdmin();
  if (sadmin)
    sadmin->SetCaretOwner(snip, dist);
}

void wxMediaSnipMediaAdmin::Resized(Bool redraw_now)
{
  wxSnipAdmin *sadmin;
  sadmin = snip->GetAdmin();
  if (sadmin)
    sadmin->Resized(snip, redraw_now);
}

void wxMediaSnipMediaAdmin::NeedsUpdate(double localx, double localy, 
					double w, double h)
{
  wxSnipAdmin *sadmin;
  sadmin = snip->GetAdmin();
  if (sadmin)
    sadmin->NeedsUpdate(snip, 
			localx + snip->leftMargin,
			localy + snip->topMargin,
			w, h);
}

void wxMediaSnipMediaAdmin::UpdateCursor()
{
  wxSnipAdmin *sadmin;
  sadmin = snip->GetAdmin();
  if (sadmin)
    sadmin->UpdateCursor();
}

Bool wxMediaSnipMediaAdmin::PopupMenu(void *m, double x, double y)
{
  wxSnipAdmin *sadmin;
  sadmin = snip->GetAdmin();
  if (sadmin)
    return sadmin->PopupMenu(m, snip, 
			     x + snip->leftMargin, 
			     y + snip->topMargin);  
  else
    return FALSE;
}

Bool wxMediaSnipMediaAdmin::DelayRefresh()
{
  wxSnipAdmin *sadmin;
  sadmin = snip->GetAdmin();
  if (!sadmin)
    return 1;

  if (sadmin->__type == wxTYPE_MEDIA_SNIP_ADMIN) {
    wxMediaBuffer *b;
    b = ((wxStandardSnipAdmin *)sadmin)->GetMedia();

    return b->RefreshDelayed();
  } else
    return 0;
}

void wxMediaSnipMediaAdmin::Modified(Bool modified)
{
  wxSnipAdmin *sadmin;
  sadmin = snip->GetAdmin();
  if (sadmin)
    sadmin->Modified(snip, modified);
}

/************************************************************************/

extern wxMediaBuffer *objscheme_unbundle_wxMediaBuffer(Scheme_Object *, const char*, int);

#define GET_EDIT(buf) ((buf->bufferType == wxEDIT_BUFFER) ? (wxMediaEdit *)buf : NULL)
#define GET_BUF(bv) objscheme_unbundle_wxMediaBuffer((Scheme_Object *)vb, NULL, 0)

#define edf(name, action) \
     static Bool ed_##name(void *vb, wxEvent *, void *) \
     { wxMediaBuffer *buf; wxMediaEdit *b; \
       buf = GET_BUF(vb); \
       b = GET_EDIT(buf); \
       if (!b) return FALSE; \
       b->action; return TRUE; } \

edf(right, MovePosition(WXK_RIGHT))
edf(left, MovePosition(WXK_LEFT))
edf(up, MovePosition(WXK_UP))
edf(down, MovePosition(WXK_DOWN))
edf(pageup, MovePosition(WXK_UP, FALSE, wxMOVE_PAGE))
edf(pagedown, MovePosition(WXK_DOWN, FALSE, wxMOVE_PAGE))
edf(rightword, MovePosition(WXK_RIGHT, FALSE, wxMOVE_WORD))
edf(leftword, MovePosition(WXK_LEFT, FALSE, wxMOVE_WORD))

edf(selectright, MovePosition(WXK_RIGHT, TRUE))
edf(selectleft, MovePosition(WXK_LEFT, TRUE))
edf(selectup, MovePosition(WXK_UP, TRUE))
edf(selectdown, MovePosition(WXK_DOWN, TRUE))
edf(selectpageup, MovePosition(WXK_UP, TRUE, wxMOVE_PAGE))
edf(selectpagedown, MovePosition(WXK_DOWN, TRUE, wxMOVE_PAGE))
edf(selectrightword, MovePosition(WXK_RIGHT, TRUE, wxMOVE_WORD))
edf(selectleftword, MovePosition(WXK_LEFT, TRUE, wxMOVE_WORD))

edf(startofline, MovePosition(WXK_LEFT, FALSE, wxMOVE_LINE))
edf(endofline, MovePosition(WXK_RIGHT, FALSE, wxMOVE_LINE))
edf(home, MovePosition(WXK_HOME))
edf(end,  MovePosition(WXK_END))

edf(selecttostartofline, MovePosition(WXK_LEFT, TRUE, wxMOVE_LINE))
edf(selecttoendofline, MovePosition(WXK_RIGHT, TRUE, wxMOVE_LINE))
edf(selecttohome, MovePosition(WXK_HOME, TRUE))
edf(selecttoend,  MovePosition(WXK_END, TRUE))

edf(clear, Erase())
edf(delete, Delete())

edf(pastenext, PasteNext())

static Bool ed_deletenext(void *vb, wxEvent *, void *)
{
  long s, e;
  wxMediaEdit *edit;
  wxMediaBuffer *buf;
  buf = GET_BUF(vb);
  edit = GET_EDIT(buf);
  if (!edit) return FALSE;

  edit->GetPosition(&s, &e);
  if (s != e)
    edit->Delete();
  else
    edit->Delete(s, s + 1);
  return TRUE;
}

static Bool ed_deletenextword(void *vb, wxEvent *event, void *)
{
  wxMediaEdit *edit;
  wxMediaBuffer *buf;
  buf = GET_BUF(vb);
  edit = GET_EDIT(buf);
  if (!edit) return FALSE;

  edit->BeginEditSequence();
  ed_selectrightword(edit, event, NULL);
  ed_delete(edit, event, NULL);
  edit->EndEditSequence();
  return TRUE;
}

static Bool ed_deleteprevword(void *vb, wxEvent *event, void *)
{
  wxMediaEdit *edit;
  wxMediaBuffer *buf;
  buf = GET_BUF(vb);
  edit = GET_EDIT(buf);
  if (!edit) return FALSE;

  edit->BeginEditSequence();
  ed_selectleftword(edit, event, NULL);
  ed_delete(edit, event, NULL);
  edit->EndEditSequence();
  return TRUE;
}

static Bool ed_deleteline(void *vb, wxEvent *event, void *)
{
  wxMediaEdit *edit;
  wxMediaBuffer *buf;
  buf = GET_BUF(vb);
  edit = GET_EDIT(buf);
  if (!edit) return FALSE;

  edit->BeginEditSequence();
  ed_startofline(edit, event, NULL);
  ed_selecttoendofline(edit, event, NULL);
  ed_delete(edit, event, NULL);
  edit->EndEditSequence();
  return TRUE;
}

void wxMediaEdit::AddEditorFunctions(wxKeymap *tab)
{
  wxAddMediaEditorFunctions(tab);
}

void wxAddMediaEditorFunctions(wxKeymap *tab)
{
#define setf(name, func) tab->AddFunction(name, ed_##func, NULL)

  setf("forward-character", right);
  setf("backward-character", left);
  setf("previous-line", up);
  setf("next-line", down);
  setf("previous-page", pageup);
  setf("next-page", pagedown);
  setf("forward-word", rightword);
  setf("backward-word", leftword);

  setf("forward-select", selectright);
  setf("backward-select", selectleft);
  setf("select-down", selectdown);
  setf("select-up", selectup);
  setf("select-page-up", selectpageup);
  setf("select-page-down", selectpagedown);
  setf("forward-select-word", selectrightword);
  setf("backward-select-word", selectleftword);

  setf("beginning-of-file", home);
  setf("end-of-file", end);
  setf("beginning-of-line", startofline);
  setf("end-of-line", endofline);

  setf("select-to-beginning-of-file", selecttohome);
  setf("select-to-end-of-file", selecttoend);
  setf("select-to-beginning-of-line", selecttostartofline);
  setf("select-to-end-of-line", selecttoendofline);

  setf("delete-previous-character", delete);
  setf("delete-next-character", deletenext);
  setf("clear-buffer", clear);
  setf("delete-next-word", deletenextword);
  setf("delete-previous-word", deleteprevword);

  setf("delete-line", deleteline);
  setf("paste-next", pastenext);

  wxAddMediaBufferFunctions(tab);
}

/*************************************************************/

wxMediaWordbreakMap::wxMediaWordbreakMap()
{
  int i;
  char *old;

  usage = 0;
  memset(map, 0, sizeof(map));

  /* Use default locale... */
  old = setlocale(LC_CTYPE, NULL);
  old = copystring(old);
  setlocale(LC_CTYPE, "");

  for (i = 0; i < 256; i++) {
    if (isalnum(i))
      map[i] = wxBREAK_FOR_CARET | wxBREAK_FOR_LINE | wxBREAK_FOR_SELECTION;
    else if ((i >= 128) || !isspace(i))
      map[i] = wxBREAK_FOR_LINE;
  }

  setlocale(LC_CTYPE, old);

  map[(int)'-'] -= wxBREAK_FOR_LINE;
}

void wxMediaWordbreakMap::SetMap(int ch, int mask)
{
  if ((ch >= 0) && (ch <= 255))
    map[ch] = mask;
}

int wxMediaWordbreakMap::GetMap(int ch)
{
  if ((ch >= 0) && (ch <= 255))
    return map[ch];
  else
    return 0;
}
