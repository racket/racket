/*
 * File:        wx_mline.cc
 * Purpose:     wxMediaLine (internal class for wxMediaEdit) implementation
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
#include "wx_mline.h"
#include "wx_ptreq.h"

#define MAX_W_MASK (WXLINE_MAX_W_HERE | WXLINE_MAX_W_LEFT | WXLINE_MAX_W_RIGHT)
#define COLOR_MASK (WXLINE_RED | WXLINE_BLACK)
#define CALC_MASK (WXLINE_CALC_HERE | WXLINE_CALC_LEFT | WXLINE_CALC_RIGHT)
#define FLOW_MASK (WXLINE_FLOW_HERE | WXLINE_FLOW_LEFT | WXLINE_FLOW_RIGHT)

#define SET_RED(n) n->flags = ((n->flags & ~COLOR_MASK) | WXLINE_RED)
#define SET_BLACK(n) n->flags = ((n->flags & ~COLOR_MASK) | WXLINE_BLACK)

#define REDP(node) (node->flags & WXLINE_RED)
#define BLACKP(node) (node->flags & WXLINE_BLACK)

wxMediaLine *NIL;

static wxMediaParagraph *plain_paragraph = NULL;

/*****************************************************************/

wxMediaLine::wxMediaLine()
{
  if (!NIL) {
    wxREGGLOB(NIL);
    NIL = this;
  }

  pos = line = scroll = 0;
  y = 0;

  prev = next = NULL;
  left = right = parent = NIL;

  flags = WXLINE_BLACK | WXLINE_MAX_W_HERE | WXLINE_CALC_HERE;
  w = maxWidth = 0;

  snip = lastSnip = scrollSnip = NULL;

  numscrolls = 1;
  len = 0;
  h = w = 0;
  lastH = lastW = 0;
  bottombase = topbase = 0;
  parno = 0;
}

wxMediaLine::~wxMediaLine()
{
  if (PTRNE(left, NIL))
    DELETE_OBJ left;
  if (PTRNE(right, NIL))
    DELETE_OBJ right;
}

/*****************************************************************/

void wxMediaLine::AdjustOffsets(wxMediaLine *newchild)
{
  if (PTREQ(newchild, NIL))
    return;

  newchild->line -= line + 1;
  newchild->pos -= pos + len;
  newchild->scroll -= scroll + numscrolls;
  newchild->y -= y + h;
  newchild->parno -= parno + StartsParagraph();
}

void wxMediaLine::DeadjustOffsets(wxMediaLine *oldchild)
{
  if (PTREQ(oldchild, NIL))
    return;

  oldchild->line += line + 1;
  oldchild->pos += pos + len;
  oldchild->scroll += scroll + numscrolls;
  oldchild->y += y + h;
  oldchild->parno += parno + StartsParagraph();
}

void wxMediaLine::RotateLeft(wxMediaLine **root)
{
  wxMediaLine *oldright;

  oldright = right;
  
  DeadjustOffsets(oldright);
  
  right = oldright->left;
  if (PTRNE(right, NIL))
    right->parent = this;

  oldright->parent = parent;
  if (PTREQ(parent, NIL))
    *root = oldright;
  else if (PTREQ(this, parent->left))
    parent->left = oldright;
  else      
    parent->right = oldright;

  oldright->left = this;
  parent = oldright;

  AdjustMaxWidth();
  AdjustNeedCalc();
  AdjustNeedFlow();
  oldright->AdjustMaxWidth();
  oldright->AdjustNeedCalc();
  oldright->AdjustNeedFlow();
}

void wxMediaLine::RotateRight(wxMediaLine **root)
{
  wxMediaLine *oldleft;

  oldleft = left;

  oldleft->AdjustOffsets(this);

  left = oldleft->right;
  if (PTRNE(left, NIL))
    left->parent = this;

  oldleft->parent = parent;
  if (PTREQ(parent, NIL))
    *root = oldleft;
  else if (PTREQ(this, parent->left))
    parent->left = oldleft;
  else      
    parent->right = oldleft;

  oldleft->right = this;
  parent = oldleft;

  AdjustMaxWidth();
  AdjustNeedCalc();
  AdjustNeedFlow();
  oldleft->AdjustMaxWidth();
  oldleft->AdjustNeedCalc();
  oldleft->AdjustNeedFlow();
}

wxMediaLine *wxMediaLine::Insert(wxMediaLine **root, Bool before)
{
  wxMediaLine *newline, *node;

  newline = new wxMediaLine;

  if (PTREQ(*root, NIL)) {
    *root = newline;
    return newline;
  }

  SET_RED(newline);

  if (before) {
    newline->prev = prev;
    if (prev)
      prev->next = newline;
    newline->next = this;
    prev = newline;
  } else {
    newline->prev = this;
    newline->next = next;
    if (next)
      next->prev = newline;
    next = newline;
  }

  if (before) {
    if (PTREQ(left, NIL)) {
      left = newline;
      node = this;
    } else {
      node = left;
      while (PTRNE(node->right, NIL)) {
	node = node->right;
      }
      node->right = newline;
    }
  } else {
    if (PTREQ(right, NIL)) {
      right = newline;
      node = this;
    } else {
      node = right;
      while (PTRNE(node->left, NIL)) {
	node = node->left;
      }
      node->left = newline;
    }
  }
 
  newline->parent = node;
  node->AdjustNeedCalc(TRUE);

  node = newline;
  while (PTRNE(node->parent, NIL)) {
    if (PTREQ(node, node->parent->left)) {
      node = node->parent;
      newline->DeadjustOffsets(node);
    } else
      node = node->parent;
  }
	
  node = newline;
  while (PTRNE(node, *root) && REDP(node->parent)) {
    if (PTREQ(node->parent, node->parent->parent->left)) {
      wxMediaLine *v = node->parent->parent->right;
      if (REDP(v)) {
	SET_BLACK(node->parent);
	SET_BLACK(v);
	node = node->parent->parent;
	SET_RED(node);
      } else {
	if (PTREQ(node, node->parent->right)) {
	  node = node->parent;
	  node->RotateLeft(root);
	}
	SET_BLACK(node->parent);
	node = node->parent->parent;
	SET_RED(node);
	node->RotateRight(root);
      }
    } else {
      wxMediaLine *v = node->parent->parent->left;
      if (REDP(v)) {
	SET_BLACK(node->parent);
	SET_BLACK(v);
	node = node->parent->parent;
	SET_RED(node);
      } else {
	if (PTREQ(node, node->parent->left)) {
	  node = node->parent;
	  node->RotateRight(root);
	}
	SET_BLACK(node->parent);
	node = node->parent->parent;
	SET_RED(node);
	node->RotateLeft(root);
      }
    }
  }
  
  SET_BLACK((*root));

  return newline;
}

void wxMediaLine::Delete(wxMediaLine **root)
{
  wxMediaLine *v, *x;
  Bool wasBlack;

  /* Adjust ancestor offsets */
  v = this;
  while (PTRNE(v->parent, NIL)) {
    if (PTREQ(v->parent->right, v))
      v = v->parent;
    else {
      v = v->parent;
      v->line -= 1;
      v->pos -= len;
      v->scroll -= numscrolls;
      v->y -= h;
      v->parno -= StartsParagraph();
    }
  }

  if (PTREQ(left, NIL) || PTREQ(right, NIL)) {
    v = this;
  } else {
    v = this->next;
    x = v;
    while (PTRNE(x->parent, this)) {
      if (PTREQ(x->parent->right, x))
	x = x->parent;
      else {
	x = x->parent;
	x->line -= 1;
	x->pos -= v->len;
	x->scroll -= v->numscrolls;
	x->y -= v->h;
	x->parno -= v->StartsParagraph();
      }
    }
  }

  if (PTRNE(v->left, NIL))
    x = v->left;
  else
    x = v->right;

  x->parent = v->parent;

  if (PTREQ(v->parent, NIL))
    *root = x;
  else if (PTREQ(v, v->parent->left))
    v->parent->left = x;
  else
    v->parent->right = x;

  wasBlack = BLACKP(v);

  if (PTRNE(v, this)) {
    wxMediaLine *oldparent;

    oldparent = v->parent;

    if (BLACKP(this))
      SET_BLACK(v);
    else
      SET_RED(v);

    v->left = left;
    if (PTRNE(left, NIL))
      left->parent = v;
    v->right = right;
    if (PTRNE(right, NIL))
      right->parent = v;
    v->parent = parent;
    if (PTREQ(*root, this))
      *root = v;
    else if (PTREQ(parent->right, this))
      parent->right = v;
    else
      parent->left = v;

    v->prev = prev;
    if (v->prev)
      v->prev->next = v;
    
    v->line = line;
    v->pos = pos;
    v->scroll = scroll;
    v->y = this->y;
    v->parno = parno;

    oldparent->AdjustMaxWidth(TRUE);
    oldparent->AdjustNeedCalc(TRUE);
    oldparent->AdjustNeedFlow(TRUE);
    
    v->AdjustMaxWidth(TRUE);
    v->AdjustNeedCalc(TRUE);
    v->AdjustNeedFlow(TRUE);

    if (PTREQ(x->parent, this))
      x->parent = v;
  } else {
    if (prev)
      prev->next = next;
    if (next)
      next->prev = prev;
  }

  if (wasBlack) {
    /* Fixup */
    wxMediaLine *z;

    while (PTRNE(x, *root) && BLACKP(x)) {
      if (PTREQ(x, x->parent->left)) {
	z = x->parent->right;
	if (REDP(z)) {
	  SET_BLACK(z);
	  SET_RED(x->parent);
	  x->parent->RotateLeft(root);
	  z = x->parent->right;
	}
	if (BLACKP(z->left) && BLACKP(z->right)) {
	  SET_RED(z);
	  x = x->parent;
	} else {
	  if (BLACKP(z->right)) {
	    SET_BLACK(z->left);
	    SET_RED(z);
	    z->RotateRight(root);
	    z = x->parent->right;
	  }
	  if (REDP(x->parent))
	    SET_RED(z);
	  else
	    SET_BLACK(z);
	  SET_BLACK(x->parent);
	  SET_BLACK(z->right);
	  x->parent->RotateLeft(root);
	  x = *root;
	}
      } else {
	z = x->parent->left;
	if (REDP(z)) {
	  SET_BLACK(z);
	  SET_RED(x->parent);
	  x->parent->RotateRight(root);
	  z = x->parent->left;
	}
	if (BLACKP(z->right) && BLACKP(z->left)) {
	  SET_RED(z);
	  x = x->parent;
	} else {
	  if (BLACKP(z->left)) {
	    SET_BLACK(z->right);
	    SET_RED(z);
	    z->RotateLeft(root);
	    z = x->parent->left;
	  }
	  if (REDP(x->parent))
	    SET_RED(z);
	  else
	    SET_BLACK(z);
	  SET_BLACK(x->parent);
	  SET_BLACK(z->left);
	  x->parent->RotateRight(root);
	  x = *root;
	}
      }
    }
    
    SET_BLACK(x);
  }

  right = left = NIL;
  DELETE_OBJ this;
}

/***************************************************************/

#define SEARCH(what, size)                   \
  wxMediaLine *node, *_prev;                 \
                                             \
  node = this;                               \
                                             \
  do {                                       \
    _prev = node;                            \
    if (what < node->what)                   \
      node = node->left;                     \
    else if (what >= node->what + size) {    \
      what -= node->what + size;             \
      node = node->right;                    \
    } else                                   \
      return node;                           \
  } while (PTRNE(node, NIL));                \
                                             \
  return _prev;                              \

wxMediaLine *wxMediaLine::FindLine(long line)
{
  SEARCH(line, 1);
}

wxMediaLine *wxMediaLine::FindPosition(long pos)
{
  SEARCH(pos, node->len);
}

wxMediaLine *wxMediaLine::FindScroll(long scroll)
{
  SEARCH(scroll, node->numscrolls);
}

wxMediaLine *wxMediaLine::FindLocation(double y)
{
  SEARCH(y, node->h);
}

wxMediaLine *wxMediaLine::FindParagraph(long parno)
{
  wxMediaLine *node;

  node = this;

  do {
    if (parno < node->parno)
      node = node->left;
    else if (parno > node->parno
	     || (parno == node->parno
		 && !node->StartsParagraph())) {
      parno -= node->parno + node->StartsParagraph();
      node = node->right;
    } else
      return node;
  } while (PTRNE(node, NIL));

  return NULL;
}

/***************************************************************/

#define SUM(what, lwhat, size, adjust)       \
  wxMediaLine *node;                         \
                                             \
  node = this;                               \
                                             \
  lwhat = node->what;                        \
  while (PTRNE(node->parent, NIL)) {         \
    if (PTREQ(node, node->parent->left))     \
      node = node->parent;                   \
    else {                                   \
      node = node->parent;                   \
      lwhat += node->what + size;            \
    }                                        \
  }                                          \
                                             \
  return lwhat + adjust;                     \

long wxMediaLine::GetLine()
{
  long _line;
  SUM(line, _line, 1, 0);
}

long wxMediaLine::GetPosition()
{
  long _pos;
  SUM(pos, _pos, node->len, 0);
}

long wxMediaLine::GetScroll()
{
  long _scroll;
  SUM(scroll, _scroll, node->numscrolls, 0);
}

double wxMediaLine::GetLocation()
{
  double _y;
  SUM(y, _y, node->h, 0);
}

long wxMediaLine::GetParagraph()
{
  long _parno;
  SUM(parno, _parno, node->StartsParagraph(), (StartsParagraph() ? 0 : -1));
}

wxMediaParagraph *wxMediaLine::GetParagraphStyle(Bool *first)
{
  if (flags & WXLINE_STARTS_PARA) {
    if (first) *first = 1;
    return paragraph;
  } else {
    wxMediaLine *root, *pstart;
    int p;
    if (first) *first = 0;
    root = GetRoot();
    p = GetParagraph();
    pstart = root->FindParagraph(p);
    return pstart->paragraph;
  }
}

/***************************************************************/

#define ADJUST(what, adj)                    \
  wxMediaLine *node;                         \
                                             \
  node = this;                               \
                                             \
  delta = what - node->what;                 \
  node->what = what;                         \
  while (PTRNE(node->parent, NIL)) {         \
    if (PTREQ(node, node->parent->left)) {   \
      node = node->parent;                   \
      node->adj += delta;                    \
    } else {                                 \
      node = node->parent;                   \
    }                                        \
  }                                          \

void wxMediaLine::SetLength(long len)
{
  long delta;
  ADJUST(len, pos);
}

void wxMediaLine::CalcLineLength()
{
  wxSnip *asnip, *nexts;
  long l = 0;

  nexts = lastSnip->next;
  for(asnip = snip; PTRNE(asnip, nexts); asnip = asnip->next) {
    l += asnip->count;
    if (asnip->flags & wxSNIP_WIDTH_DEPENDS_ON_X)
      asnip->SizeCacheInvalid();
  }

  if (l != len)
    SetLength(l);

  if (next && (lastSnip->flags & wxSNIP_HARD_NEWLINE)) {
    if (!next->StartsParagraph())
      next->SetStartsParagraph(TRUE);
  } else if (next)
    if (next->StartsParagraph())
      next->SetStartsParagraph(FALSE);

  if (!prev || (prev->lastSnip->flags & wxSNIP_HARD_NEWLINE)) {
    if (!StartsParagraph())
      SetStartsParagraph(TRUE);
  } else
    if (StartsParagraph())
      SetStartsParagraph(FALSE);
}

void wxMediaLine::SetScrollLength(long numscrolls)
{
  long delta;
  ADJUST(numscrolls, scroll);
}

void wxMediaLine::SetHeight(double h)
{
  double delta;
  ADJUST(h, y);
}

void wxMediaLine::SetStartsParagraph(Bool starts)
{
  wxMediaLine *node;

  if ((starts ? 1 : 0) == StartsParagraph())
    return;

  if (starts) {
    flags |= WXLINE_STARTS_PARA;
    if (!paragraph) {
      if (!plain_paragraph) {
	wxREGGLOB(plain_paragraph);
	paragraph = new wxMediaParagraph();
	paragraph->leftMarginFirst = 0;
	paragraph->leftMargin = 0;
	paragraph->rightMargin = 0;
	paragraph->alignment = WXPARA_LEFT;
	plain_paragraph = paragraph;
      } else
	paragraph = plain_paragraph;
    }
  } else {
    flags -= WXLINE_STARTS_PARA;
    paragraph = NULL;
  }

  node = this;

  while (PTRNE(node->parent, NIL)) {
    if (PTREQ(node, node->parent->left)) {
      node = node->parent;
      if (starts)
	node->parno += 1;
      else
	node->parno -= 1;
    } else {
      node = node->parent;
    }
  }
}

/***************************************************************/

void wxMediaLine::AdjustMaxWidth(Bool recur)
{
  wxMediaLine *node = this;
  long which, old;

  if (PTREQ(this, NIL))
    return;

  while (1) {
    old = node->flags & MAX_W_MASK;

    if (PTRNE(node->right, NIL)
	&& node->right->maxWidth > node->w 
	&& (PTREQ(node->left, NIL)
	    || node->right->maxWidth > node->left->maxWidth)) {
      node->maxWidth = node->right->maxWidth;
      which = WXLINE_MAX_W_RIGHT;
    } else if (PTRNE(node->left, NIL)
	       && node->left->maxWidth > node->w) {
      node->maxWidth = node->left->maxWidth;
      which = WXLINE_MAX_W_LEFT;
    } else {
      node->maxWidth = node->w;
      which = WXLINE_MAX_W_HERE;
    }

    if (old != which) {
      node->flags &= ~MAX_W_MASK;
      node->flags |= which;
    }
    
    node = node->parent;
    if (!recur || PTREQ(node, NIL))
      return;
  }
}

void wxMediaLine::SetWidth(double w)
{
  this->w = w;
  AdjustMaxWidth(TRUE);
}

/***************************************************************/

double wxMediaLine::ScrollOffset(long p)
{
  if (!scrollSnip)
    return 0;

  if (p >= numscrolls)
    return h;

  return scrollSnip->GetScrollStepOffset(p);
}

long wxMediaLine::FindExtraScroll(double y)
{
  if (y >= h)
    return numscrolls;
  
  if (!scrollSnip)
    return 0;

  return scrollSnip->FindScrollStep(y);
}

/***************************************************************/

void wxMediaLine::MarkRecalculate()
{
  if (!(flags & WXLINE_CALC_HERE)) {
    flags |= WXLINE_CALC_HERE;
    if (PTRNE(parent, NIL))
      parent->AdjustNeedCalc(TRUE);
  }
}

void wxMediaLine::AdjustNeedCalc(Bool recur)
{
  wxMediaLine *node = this;
  long which, old;

  while (1) {
    old = node->flags & CALC_MASK;

    which = old & WXLINE_CALC_HERE;

    if (PTRNE(node->right, NIL) && node->right->flags & CALC_MASK)
      which |= WXLINE_CALC_RIGHT;
    if (PTRNE(node->left, NIL) && node->left->flags & CALC_MASK)
      which |= WXLINE_CALC_LEFT;

    if (old != which) {
      node->flags &= ~CALC_MASK;
      node->flags |= which;
      if (!recur || PTREQ(node->parent, NIL))
	return;
      node = node->parent;
    } else
      return;
  }
}

/***************************************************************/

void wxMediaLine::MarkCheckFlow()
{
  if (!(flags & WXLINE_FLOW_HERE)) {
    flags |= WXLINE_FLOW_HERE;
    if (PTRNE(parent, NIL))
      parent->AdjustNeedFlow(TRUE);
  }
}

void wxMediaLine::AdjustNeedFlow(Bool recur)
{
  wxMediaLine *node = this;
  long which, old;

  while (1) {
    old = node->flags & FLOW_MASK;

    which = old & WXLINE_FLOW_HERE;

    if (PTRNE(node->right, NIL) && node->right->flags & FLOW_MASK)
      which |= WXLINE_FLOW_RIGHT;
    if (PTRNE(node->left, NIL) && node->left->flags & FLOW_MASK)
      which |= WXLINE_FLOW_LEFT;

    if (old != which) {
      node->flags &= ~FLOW_MASK;
      node->flags |= which;
      if (!recur || PTREQ(node->parent, NIL))
	return;
      node = node->parent;
    } else
      return;
  }
}

/***************************************************************/

wxMediaLine *wxMediaLine::GetRoot()
{
  wxMediaLine *node;

  node = this;
  while (PTRNE(node->parent, NIL)) {
    node = node->parent;
  }

  return node;
}

/***************************************************************/

Bool wxMediaLine::UpdateFlow(wxMediaLine **root,
			     wxMediaEdit *media, double maxWidth, wxDC *dc)
{
  if (flags & WXLINE_FLOW_LEFT) {
    if (PTRNE(left, NIL) && left->UpdateFlow(root, media, maxWidth, dc))
      return TRUE;
    flags -= WXLINE_FLOW_LEFT;
  }

  if (flags & WXLINE_FLOW_HERE) {
    Bool firstLine;
    wxMediaParagraph *para;
    double lineMaxWidth;

    flags -= WXLINE_FLOW_HERE;

    para = GetParagraphStyle(&firstLine);
    lineMaxWidth = para->GetLineMaxWidth(maxWidth, firstLine);

    if (media->CheckFlow(lineMaxWidth, dc, GetLocation(), GetPosition(), snip)) {
      wxSnip *asnip;

      for (asnip = snip; PTRNE(asnip, lastSnip); asnip = asnip->next) {
	if (asnip->flags & wxSNIP_NEWLINE) {
	  /* Items pushed to next line or new line was inserted */
	  wxSnip *nextsnip = NULL;

	  if (next) {
	    nextsnip = asnip->next;
	    while (nextsnip && PTRNE(nextsnip, next->lastSnip)
		   && !(nextsnip->flags & wxSNIP_NEWLINE)) {
	      nextsnip = nextsnip->next;
	    }
	  }
	  
	  if (!next || PTRNE(nextsnip, next->lastSnip)) {
	    /* It was a new line */

	    wxMediaLine *newline;

	    newline = Insert(root, FALSE);
	    newline->snip = asnip->next;
	    newline->lastSnip = lastSnip;
	    lastSnip = asnip;
	    
	    nextsnip = newline->lastSnip->next;
	    
	    for (asnip = newline->snip; PTRNE(asnip, nextsnip); 
		 asnip = asnip->next) {
	      asnip->line = newline;
	    }
	    
	    newline->MarkCheckFlow();
	    newline->MarkRecalculate();
	    newline->CalcLineLength();
	  } else {
	    /* Just pushed to next line */
	    lastSnip = asnip;
	    asnip->line = this;

	    next->snip = asnip->next;

	    nextsnip = next->lastSnip->next;
	    for (asnip = next->snip; PTRNE(asnip, nextsnip);
		 asnip = asnip->next) {
	      asnip->line = next;
	    }

	    next->MarkCheckFlow();
	    next->MarkRecalculate();
	    next->CalcLineLength();
	  }

	  CalcLineLength();
	  MarkRecalculate();

	  return TRUE;
	}
	asnip->line = this;
      }
      /* This line was extended */

      if (asnip) {
	while (asnip->next && !(asnip->flags & wxSNIP_NEWLINE)) {
	  asnip->line = this;
	  if (next && PTREQ(asnip, next->lastSnip)) {
	    /* a line was deleted */
	    next->Delete(root);
	  }
	  asnip = asnip->next;
	}

	if (next && PTREQ(asnip, next->lastSnip)) {
	  /* a line was deleted */
	  next->Delete(root);
	}

	lastSnip = asnip;
      } else {
	lastSnip = media->lastSnip;
	while (next) {
	  next->Delete(root);
	}
      }

      lastSnip->line = this;

      if (next) {
	asnip = asnip->next;
	if (PTRNE(next->snip, asnip) || !(next->lastSnip->flags & wxSNIP_NEWLINE)) {
	  /* Effect can propogate to more lines, merging the
             next several. (Handle prefixing the remains of the source of
             the extension to this line onto the next line. Implemented
             as the next line eating the next->next line.) */
	  next->snip = asnip;
	  while (asnip->next && !(asnip->flags & wxSNIP_NEWLINE)) {
	    if (next->next && PTREQ(asnip, next->next->lastSnip)) {
	      /* a line was deleted */
	      next->next->Delete(root);
	    }
	    asnip->line = next;
	    asnip = asnip->next;
	  }
	  asnip->line = next;
	  next->lastSnip = asnip;
	  if (next->next) {
	    if (PTREQ(asnip, next->next->lastSnip)) {
	      /* a line was deleted */
	      next->next->Delete(root);
	    } else
	      next->next->snip = asnip->next;
	  }

	  next->CalcLineLength();
	  next->MarkRecalculate();
	  next->MarkCheckFlow();
	}
      }

      CalcLineLength();
      MarkRecalculate();
      
      return TRUE;
    }
  }

  if (flags & WXLINE_FLOW_RIGHT) {
    if (PTRNE(right, NIL) && right->UpdateFlow(root, media, maxWidth, dc))
      return TRUE;
    flags -= WXLINE_FLOW_RIGHT;
  }

  return FALSE;
}

Bool wxMediaLine::UpdateGraphics(wxMediaEdit *media, wxDC *dc)
{
  Bool changed = FALSE;
  Bool isfirst;
  wxMediaParagraph *para;

  if (flags & WXLINE_CALC_LEFT)
    if (PTRNE(left, NIL) && left->UpdateGraphics(media, dc))
      changed = TRUE;
  
  if (flags & WXLINE_CALC_HERE) {
    wxSnip *asnip, *_next;
    long maxscroll, _scroll;
    double _y, bigwidth;
    double _h, _w, descent, space, totalwidth;
    double maxh, maxbase, maxdescent, maxspace, maxantidescent, maxantispace;
    int align;

    _y = GetLocation();

    maxbase = maxdescent = maxspace = maxantidescent = maxantispace = 0;
    totalwidth = 0;
	
    maxscroll = 1;
    scrollSnip = NULL;

    _next = lastSnip->next;
    for (asnip = snip; PTRNE(asnip, _next); asnip = asnip->next) {
      _w = _h = descent = space = 0.0;
      asnip->GetExtent(dc, totalwidth, _y, &_w, &_h, &descent, &space);

      align = asnip->style->GetAlignment();
	  
      _scroll = asnip->GetNumScrollSteps();

      if (_h - descent - space > maxbase)
	maxbase = _h - descent - space;
	 
      if (align == wxALIGN_BOTTOM) {
	if (descent > maxdescent)
	  maxdescent = descent;
      } else {
	if (_h - space > maxantispace)
	  maxantispace = _h - space;
      }

      if (align == wxALIGN_TOP) {
	if (space > maxspace)
	  maxspace = space;
      } else {
	if (_h - descent > maxantidescent)
	  maxantidescent = _h - descent;
      }

      if (_scroll > maxscroll) {
	scrollSnip = asnip;
	maxscroll = _scroll;
      }
	  
      totalwidth += _w;
    }
    
    if (maxantidescent - maxbase > maxspace)
      maxspace = maxantidescent - maxbase;

    if (maxantispace - maxbase > maxdescent)
      maxdescent = maxantispace - maxbase;

    lastH = _h;
    lastW = _w;
    topbase = maxspace;
    bottombase = maxspace + maxbase;

    maxh = maxbase + maxdescent + maxspace;
	
    maxh += media->lineSpacing;

    if (this->w > totalwidth)
      bigwidth = this->w;
    else
      bigwidth = totalwidth;
    bigwidth += CURSOR_WIDTH;
    
    para = GetParagraphStyle(&isfirst);
    if (isfirst)
      bigwidth += para->leftMarginFirst;
    else
      bigwidth += para->leftMargin;

    SetWidth(totalwidth);

    if (maxscroll != this->numscrolls)
      SetScrollLength(maxscroll);

    if (maxh == this->h)
      media->RefreshBox(0, _y, bigwidth, maxh);
    else {
      double bigheight;

      SetHeight(maxh);

      bigwidth = 1e5; /* Really want viewable width, but > ok */
      if (bigwidth < media->totalWidth)
	bigwidth = media->totalWidth;

      bigheight = maxh + media->totalHeight;
      
      media->RefreshBox(0, _y, bigwidth, bigheight);      
    }

    changed = TRUE;
  }

  if (flags & WXLINE_CALC_RIGHT)
    if (PTRNE(right, NIL) && right->UpdateGraphics(media, dc))
      changed = TRUE;

  flags &= ~CALC_MASK;

  return changed;
}

/***************************************************************/

long wxMediaLine::Number()
{
  wxMediaLine *last;
  last = Last();
  return last->GetLine() + 1;
}

wxMediaLine *wxMediaLine::First()
{
  wxMediaLine *node;
  
  node = this;
  while (PTRNE(node->left, NIL)) {
    node = node->left;
  }

  return node;
}

wxMediaLine *wxMediaLine::Last()
{
  wxMediaLine *node;
  
  node = this;
  while (PTRNE(node->right, NIL)) {
    node = node->right;
  }

  return node;  
}

/***************************************************************/

double wxMediaLine::GetLeftLocation(double maxWidth)
{
  double _left;
  wxMediaParagraph *para;

  if (flags & WXLINE_STARTS_PARA) {
    para = paragraph;
    _left = para->leftMarginFirst;
  } else {
    para = GetParagraphStyle();
    _left = para->leftMargin;
  }

  if (para->alignment != (unsigned)WXPARA_LEFT) {
    if (maxWidth > 0) {
      double delta = maxWidth - w;
      if (delta < 0)
	delta = 0;
      if (para->alignment == (unsigned)WXPARA_RIGHT)
	_left += delta;
      else
	_left += (delta / 2);
    }
  }

  return _left;
}

double wxMediaLine::GetRightLocation(double maxWidth)
{
  return GetLeftLocation(maxWidth) + w;
}

/***************************************************************/

wxMediaParagraph *wxMediaParagraph::Clone()
{
  wxMediaParagraph *paragraph;
  paragraph = new wxMediaParagraph();

  paragraph->leftMarginFirst = leftMarginFirst;
  paragraph->leftMargin = leftMargin;
  paragraph->rightMargin = rightMargin;
  paragraph->alignment = alignment;

  return paragraph;
}

double wxMediaParagraph::GetLineMaxWidth(double maxWidth, Bool first)
{
  if (maxWidth <= 0)
    return maxWidth;

  if (first)
    maxWidth -= leftMarginFirst;
  else
    maxWidth -= leftMargin;
  maxWidth -= rightMargin;

  if (maxWidth <= 0)
    maxWidth = 1;

  return maxWidth;
}

/***************************************************************/

#ifdef TEST

#include <iostream.h>

wxMgediaLine *root = NIL;
int numLines = 0;

main()
{
  long i;
  wxMediaLine *line;
  char command[256];

  line->Insert(&root);
  numLines++;

  while (1) {
    cout << "Lines:\n";
    for (i = 0; i < numLines; i++) {
      line = root->FindLine(i);
      cout << "  " << i 
        << " (" << line->GetLine() << ")"
	<< ": [" << ((void *)line->prev) << ","
	         << ((void *)line) << ","
	         << ((void *)line->next) << "]"
	<< " pos=" << line->GetPosition()
	<< " len=" << line->len
	<< " scr=" << line->GetScroll()
	<< " slen=" << line->numscrolls
	<< " loc=" << line->GetLocation()
	<< " ht=" << line->h
	<< "\n";
    }
    
    cout << "Line? ";
    cin >> i;
    if (i >= numLines)
      return 0;
    
    line = root->FindLine(i);
    cout << "[" << ((void *)line) << "]\n";    

    cout << "Command? [i,d,l,h,s,q] ";
    cin >> command;
    if (command[0] == 'i') {
      line->Insert(&root);
      numLines++;
    } else if (command[0] == 'd') {
      line->Delete(&root);
      --numLines;
    } else if (command[0] == 'l') {
      cout << "How many? ";
      cin >> i;
      if (i >= 0)
	line->SetLength(i);
    } else if (command[0] == 's') {
      cout << "How many? ";
      cin >> i;
      if (i >= 0)
	line->SetScrollLength(i);
    } else if (command[0] == 'h') {
      double h;
      cout << "How tall? ";
      cin >> h;
      if (h >= 0.0)
	line->SetHeight(h);
    } else if (command[0] == 'q') {
      return 0;
    }
  }
}

#endif
