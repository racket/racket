/*
 * File:        wx_snip.cc
 * Purpose:     wxSnip implementations
 * Author:      Matthew Flatt
 * Created:     1995
 * Copyright:   (c) 2004-2008 PLT Scheme Inc.
 * Copyright:   (c) 1995-2002, Matthew Flatt

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
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301 USA.

 */

#if defined(_MSC_VER) && defined(MZ_PRECISE_GC)
# include "wx.h"
#endif
#include "common.h"
#include "wx_dialg.h"
#ifndef OLD_WXWINDOWS
#include "wx_cmdlg.h"
#endif
#include "wx_utils.h"
#include "wx_media.h"
#include "wx_ptreq.h"
#include <string.h>
#include "wx_gcrct.h"

#define SETWD_NOT_PRESENT 1

#ifdef wx_x
#define CHECK_SPLIT 1
#else
#define CHECK_SPLIT 0
#endif

#define CHECK_CS_FLAG 1

#ifdef MEMMOVE_NOT_PRESENT
static void memmove(char *dest, char *src, long size)
{
  if (dest < src) {
    while (size--)
      *(dest++) = *(src++);
  } else {
    dest += size;
    src += size;
    while (size--)
      *(--dest) = *(--src);
  }
}
#endif

# define WXGC_CLEANUP_ARG(a) a

/* MSW version needs this for just a little while longer... */
#ifndef WXGC_ATOMIC
#define WXGC_ATOMIC /* empty */
#endif

#define MAX_WASTE 3

#define IMAGE_PIXELS_PER_SCROLL 20
#define IMAGE_VOID_SIZE 20

#define ALWAYSZERO(x) if (x) *x = 0;

extern void *wxMallocAtomicIfPossible(size_t s);

#define STRALLOC(n) new WXGC_ATOMIC wxchar[n]
#define TRY_STRALLOC(n) (wxchar *)wxMallocAtomicIfPossible((n) * sizeof(wxchar))
#define STRFREE(s) /* empty */

wxchar wx_empty_wxstr[1] = {0};

/***************************************************************/

wxSnipClass::wxSnipClass()
: wxObject(WXGC_NO_CLEANUP)
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_SNIP_CLASS;
#endif

  classname = "wxbad";
  version = 0;
  required = 0;
}

Bool wxSnipClass::ReadHeader(wxMediaStreamIn *)
{
  return TRUE;
}

Bool wxSnipClass::WriteHeader(wxMediaStreamOut *)
{
  return TRUE;
}

int wxSnipClass::ReadingVersion(wxMediaStreamIn *f)
{
  return f->ReadingVersion(this);
}

/***************************************************************/

wxSnip::wxSnip() : wxObject(WXGC_CLEANUP_ARG(WXGC_NO_CLEANUP))
{
  Init();
}

wxSnip::wxSnip(Bool cleanup) : wxObject(WXGC_CLEANUP_ARG(cleanup))
{
  Init();
}

wxInternalSnip::wxInternalSnip() : wxSnip()
{
}

wxInternalSnip::wxInternalSnip(Bool cleanup) : wxSnip(cleanup)
{
}

void wxSnip::Init(void)
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_SNIP;
#endif

  count = 1;

  flags = 0;

  snipclass = NULL;


  next = prev = NULL;
  line = NULL;

  admin = NULL;

  style = wxTheStyleList->BasicStyle();
}

wxSnip::~wxSnip()
{
  next = prev = NULL;
  line = NULL;
}

wxSnip *wxSnip::Next(void)
{
  return next;
}

wxSnip *wxSnip::Previous(void)
{
  return prev;
}

wxSnipAdmin *wxSnip::GetAdmin(void)
{
  return admin;
}

void wxSnip::SetAdmin(wxSnipAdmin *a)
{
  if (PTRNE(a, admin) && (flags & wxSNIP_OWNED)
      && (a || !(flags & wxSNIP_CAN_DISOWN)))
    return;

  admin = a;
  SizeCacheInvalid();
  if (!a) {
    prev = next = NULL;
    line = NULL;
  } else
    flags |= wxSNIP_OWNED;
}

void wxSnip::SetCount(long new_count)
{
  long old_count = count;

  if (new_count <= 0)
    new_count = 1;
  count = new_count;
  if (admin) {
    if (!admin->Recounted(this, TRUE))
      count = old_count;
  }
}

void wxInternalSnip::SetCount(long WXUNUSED(new_count))
{
  /* reject change */
}

void wxSnip::SetFlags(long new_flags)
{
  /* Make sure that wxSNIP_HARD_NEWLINE implies a wxSNIP_NEWLINE */
  if (new_flags & wxSNIP_NEWLINE)
    new_flags -= wxSNIP_NEWLINE;    
  if (new_flags & wxSNIP_HARD_NEWLINE)
    new_flags |= wxSNIP_NEWLINE;

  /* Make sure ownership and splitness flags don't change */
  if (new_flags & wxSNIP_OWNED)
    new_flags -= wxSNIP_OWNED;
  if (new_flags & wxSNIP_CAN_DISOWN)
    new_flags -= wxSNIP_CAN_DISOWN;
  if (new_flags & wxSNIP_CAN_SPLIT)
    new_flags -= wxSNIP_CAN_SPLIT; 

  if (flags & wxSNIP_OWNED)
    new_flags |= wxSNIP_OWNED;
  if (flags & wxSNIP_CAN_DISOWN)
    new_flags |= wxSNIP_CAN_DISOWN;
  if (flags & wxSNIP_CAN_SPLIT)
    new_flags |= wxSNIP_CAN_SPLIT; 

  flags = new_flags;
  if (admin)
    admin->Resized(this, TRUE);
}

void wxSnip::OnEvent(wxDC *, double, double, double, double, wxMouseEvent *)
{
}

wxCursor *wxSnip::AdjustCursor(wxDC *, double, double, double, double, wxMouseEvent *)
{
  return NULL;
}

void wxSnip::OnChar(wxDC *, double, double, double, double, wxKeyEvent *)
{
}

void wxSnip::DoEdit(int, Bool, long)
{
}

Bool wxSnip::CanEdit(int, Bool)
{
  return FALSE;
}

void wxSnip::DoFont(int, Bool)
{
}

Bool wxSnip::Match(wxSnip *other)
{
  if (PTRNE(other->snipclass, snipclass))
    return FALSE;
  if (other->count != count)
    return FALSE;
  return TRUE;
}
 
void wxSnip::OwnCaret(Bool)
{
}

void wxSnip::BlinkCaret(wxDC *, double, double)
{
}

void wxSnip::SizeCacheInvalid(void)
{
}

void wxSnip::GetExtent(wxDC *, 
		       double, double, 
		       double *w, double *h, 
		       double *descent, double *space,
		       double *lspace, double *rspace)
{
  if (w)
    *w = 0;
  if (h)
    *h = 0;
  if (descent)
    *descent = 0;
  if (space)
    *space = 0;
  if (lspace)
    *lspace = 0;
  if (rspace)
    *rspace = 0;
}

double wxSnip::PartialOffset(wxDC *dc, 
			    double x, double y, long offset)
{
  double w;

  if (!offset)
    return 0.0;

  w = 0.0;
  GetExtent(dc, x, y, &w);
  return w;
}

void wxSnip::Draw(wxDC *, double, double,
		  double, double, double, double, 
		  double, double, int)
{
}

void wxSnip::Split(long position, wxSnip **first, wxSnip **second)
{
  wxSnip *snip;

  snip = new WXGC_PTRS wxSnip();
  snip->count = position;
  count -= position;

  *first = snip;
  *second = this;

#if CHECK_CS_FLAG
  if (!(flags & wxSNIP_CAN_SPLIT) && admin)
    admin->Resized(this, TRUE);
#endif
}

wxSnip *wxSnip::MergeWith(wxSnip *)
{
  return NULL;
}

void wxSnip::GetTextBang(wxchar *s, long offset, long num, long dt)
{
  wxchar *str;

  if (num <= 0)
    return;
  str = GetText(offset + dt, num, FALSE);
  if (!str) {
    int i;
    for (i = 0; i < num; i++) {
      s[i] = '.';
    }
  } else
    memcpy(s, str, num * sizeof(wxchar));
}

wxchar *wxSnip::GetText(long offset, long num, 
			Bool WXUNUSED(flattened), long *got)
{
  wxchar *s;
  int i;

  if (num <= 0)
    return wx_empty_wxstr;
  if (offset < 0)
    offset = 0;
  if (offset > count)
    return wx_empty_wxstr;
  if (num > count - offset)
    num = count - offset;

  s = new WXGC_ATOMIC wxchar[num + 1];
  for (i = 0; i < num; i++) {
    s[i] = '.';
  }
  s[num] = 0;

  if (got)
    *got = num;

  return s;
}

char *wxSnip::GetTextUTF8(long offset, long num, Bool flattened, long *_got)
{
  wxchar *s;
  long got, len;
  char *r = NULL;

  s = GetText(offset, num, flattened, &got);
  wxme_utf8_encode(s, got, &r, &len);

  if (_got)
    *_got = len;

  return r;
}

wxSnip *wxSnip::Copy()
{
  wxSnip *snip;

  snip = new WXGC_PTRS wxSnip();
  Copy(snip);

  return snip;
}

void wxSnip::SetStyle(wxStyle *s)
{
  if (flags & wxSNIP_OWNED)
    return;

  style = s;
}

void wxSnip::Copy(wxSnip *snip)
{
  snip->count = count;
  snip->flags = flags;
  if (snip->flags & wxSNIP_OWNED)
    snip->flags -= wxSNIP_OWNED;
  if (snip->flags & wxSNIP_CAN_DISOWN)
    snip->flags -= wxSNIP_CAN_DISOWN;
  if (snip->flags & wxSNIP_CAN_SPLIT)
    snip->flags -= wxSNIP_CAN_SPLIT;
  snip->snipclass = snipclass;
  snip->style = style;
}

void wxSnip::Write(wxMediaStreamOut *)
{
}

Bool wxSnip::Resize(double, double)
{
  return FALSE;
}

long wxSnip::GetNumScrollSteps()
{
  return 1;
}

long wxSnip::FindScrollStep(double)
{
  return 0;
}

double wxSnip::GetScrollStepOffset(long)
{
  return 0;
}

Bool wxSnip::IsOwned(void)
{
  return !!(flags & wxSNIP_OWNED);
}

Bool wxSnip::ReleaseFromOwner(void)
{
  if (!IsOwned())
    return TRUE;

  if (!admin)
    return FALSE;

  if (admin->ReleaseSnip(this))
    return !(flags & wxSNIP_OWNED);
  else
    return FALSE;
}

void wxSnip::SetUnmodified()
{
  /* do nothing */
}

/***************************************************************/

class TextSnipClass : public wxSnipClass
{
 public:
  TextSnipClass(void);

  virtual wxSnip *Read(wxMediaStreamIn *);
  wxSnip *Read(wxTextSnip *, wxMediaStreamIn *);
};

static TextSnipClass *TheTextSnipClass;

TextSnipClass::TextSnipClass(void)
{
  classname = "wxtext";
  version = 3;
  required = TRUE;
}

wxSnip *TextSnipClass::Read(wxMediaStreamIn *f)
{
  wxTextSnip *s;
  s = new WXGC_PTRS wxTextSnip(0);
  return Read(s, f);
}

wxSnip *TextSnipClass::Read(wxTextSnip *snip, wxMediaStreamIn *f)
{
  long flags;
  long count, pos;

  f->Get(&flags);

  pos = f->Tell();
  f->Get(&count);
  f->JumpTo(pos);

  if (count < 0)
    count = 10; /* This is a failure. We make up something. */

  snip->Read(count, f);

  snip->flags = flags;

  return snip;
}

/***************************************************************/

#ifdef wx_mac
# define NON_BREAKING_SPACE 0xCA
#else
# define NON_BREAKING_SPACE 0xA0
#endif

static int dont_shrink_alloc_size;

wxTextSnip::wxTextSnip(long allocsize) 
{
  Init(allocsize);
}

wxTextSnip::wxTextSnip(wxchar *initstring, long len) 
{
  Init(len + 2);
  Insert(initstring, len, 0);
}

wxTextSnip::wxTextSnip(char *initstring, long len) 
{
  Init(len + 2);
  InsertUTF8(initstring, len, 0);
}

void wxTextSnip::Init(long allocsize) 
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_TEXT_SNIP;
#endif

  flags |= wxSNIP_IS_TEXT | wxSNIP_CAN_APPEND;

  w = -1.0;

  if (!dont_shrink_alloc_size)
    if (allocsize > 5000)
      allocsize = 5000;

  allocated = (allocsize > 0) ? 2 * allocsize : 20;
  buffer = STRALLOC(allocated + 1);
  dtext = 0;

  snipclass = TheTextSnipClass;
  
  count = 0;
}

wxTextSnip::~wxTextSnip()
{
  STRFREE(buffer);
  buffer = NULL;
}

void wxTextSnip::SizeCacheInvalid(void)
{
  w = -1.0;
}

void wxTextSnip::GetTextExtent(wxDC *dc, int count, double *wo)
{
  double _w, h;
  wxFont *font;
  int i;

  for (i = count; i--; ) {
    wxchar c = buffer[dtext + i]; 
    if (!c || (c == NON_BREAKING_SPACE))
      break;
  }
  
  font = style->GetFont();
#ifdef BROKEN_GET_TEXT_EXTENT 
  dc->SetFont(font);
#endif

  if (i < 0) {
    dc->GetTextExtent((char *)buffer, &_w, &h, NULL, NULL, font, FALSE, TRUE, dtext, count);
  } else {
    /* text includes null chars */
    double ex_w;
    int start = 0, i;
    
#ifndef BROKEN_GET_TEXT_EXTENT 
    dc->SetFont(font);
#endif
    dc->GetTextExtent(" ", &ex_w, &h, NULL, NULL, font);
    
    _w = 0;
    for (i = 0; i <= count; i++) {
      if (!buffer[dtext + i] || (buffer[dtext + i] == NON_BREAKING_SPACE) || (i == count)) {
	if (i > start) {
	  double piece_w, h;
	  dc->GetTextExtent((char *)buffer, &piece_w, &h, NULL, NULL, NULL, FALSE, TRUE, dtext + start, i - start);
	  _w += piece_w;
	}
	if (i < count) {
	  start = i + 1;
	  _w += ex_w;
	}
      }
    }
  }

  *wo = _w;
}

void wxTextSnip::GetExtent(wxDC *dc, 
			   double WXUNUSED(x), double WXUNUSED(y), 
			   double *wo, double *ho, double *dso, double *so,
			   double *ls, double *rs)
{

  if (w < 0) {
    if ((flags & wxSNIP_INVISIBLE) || !count 
	|| (count == 1 && buffer[dtext] == '\n')
	|| (count == 1 && buffer[dtext] == '\t')) {
      if (count == 1 && buffer[dtext] == '\t') {
	double tw;
	tw = style->GetTextWidth(dc);
	w = tw;
      } else
	w = 0;
    } else {
      double ww;
      GetTextExtent(dc, count, &ww);
      w = ww;
    }
  }

  if (wo)
    *wo = w;
  if (ho) {
    double th;
    th = style->GetTextHeight(dc);
    *ho = th;
  }
  if (dso) {
    double td;
    td = style->GetTextDescent(dc);
    *dso = td;
  }
  if (so) {
    double ts;
    ts = style->GetTextSpace(dc);
    *so = ts;
  }
  if (ls)
    *ls = 0.0;
  if (rs)
    *rs = 0.0;
}

double wxTextSnip::PartialOffset(wxDC *dc, double, double, long offset)
{
  double _w;
  
  if (offset > count)
    offset = count;

  GetTextExtent(dc, offset, &_w);

  return _w;
}

void wxTextSnip::Draw(wxDC *dc, double x, double y, 
		      double, double, double, double, 
		      double WXUNUSED(dx), double WXUNUSED(dy), 
		      int)
{
  wxchar save;
  int i;

  if (flags & wxSNIP_INVISIBLE)
    return;

  save = buffer[dtext + count];
  buffer[dtext + count] = 0;

  for (i = count; i--; ) {
    wxchar c = buffer[dtext + i]; 
    if (!c || (c == NON_BREAKING_SPACE))
      break;
  }
  
  if (i < 0)
    dc->DrawText((char *)buffer, x, y, FALSE, TRUE, dtext);
  else {
    /* text includes null chars */
    double px, h, ex_w;
    int start = 0, i;

    dc->GetTextExtent(" ", &ex_w, &h, NULL, NULL);
    
    px = x;
    for (i = 0; i <= count; i++) {
      if (!buffer[dtext + i] || (buffer[dtext + i] == NON_BREAKING_SPACE) || (i == count)) {
	if (i > start) {
	  double piece_w, h;
	  wxchar save = buffer[dtext + i];
	  buffer[dtext + i] = 0;
	  dc->GetTextExtent((char *)buffer, &piece_w, &h, NULL, NULL, NULL, FALSE, TRUE, dtext + start);
	  dc->DrawText((char *)buffer, px, y, FALSE, TRUE, dtext + start);
	  buffer[dtext + i] = save;
	  px += piece_w;
	}
	if (i < count) {
	  /* In case there's a background, draw a space: */
	  dc->DrawText(" ", px, y);

	  /* Draw box for nul: */
	  if (!buffer[dtext + i])
	    if (h > 2 && ex_w > 2)
	      dc->DrawRectangle(px + 1, y + 1, ex_w - 2, h - 2);

	  start = i + 1;
	  px += ex_w;
	}
      }
    }
  }

#ifdef wx_x
  if (style->GetUnderlined()) {
    double descent, h;
    
    descent = style->GetTextDescent(dc);
    h = style->GetTextHeight(dc);
    
    if (descent >= 2)
      y += h - (descent / 2);
    else
      y += h - descent;
    dc->DrawLine(x, y, x + w + GC_LINE_EXTEND, y);
  }
#endif

  buffer[dtext + count] = save;
}

void wxTextSnip::Split(long position, wxSnip **first, wxSnip **second)
{
  wxTextSnip *snip;

  if (position < 0 || position > count)
    return;

  dont_shrink_alloc_size = 1;
  snip = new WXGC_PTRS wxTextSnip(position);
  dont_shrink_alloc_size = 0;

  w = -1.0;

  memcpy(snip->buffer + snip->dtext, buffer + dtext, position * sizeof(wxchar));
  dtext += position;

  snip->count = position;
  count -= position;

  if (count && ((allocated / count) > MAX_WASTE)) {
    wxchar *naya;
    allocated = count;
    naya = STRALLOC(allocated + 1);
    memcpy(naya, buffer + dtext, (count + 1) * sizeof(wxchar));
    buffer = naya;
    dtext = 0;
  }

  *first = snip;
  *second = this;

#if CHECK_CS_FLAG
  if (!(flags & wxSNIP_CAN_SPLIT) && admin)
    admin->Resized(this, TRUE);
#endif
}

wxSnip *wxTextSnip::MergeWith(wxSnip *pred)
{
#if USE_OLD_TYPE_SYSTEM
  if (pred->__type != wxTYPE_TEXT_SNIP)
    return this;
#endif

  w = -1.0;

  InsertWithOffset(((wxTextSnip *)pred)->buffer, pred->count, ((wxTextSnip *)pred)->dtext, 0);

#if CHECK_CS_FLAG
  if (!(flags & wxSNIP_CAN_SPLIT) && admin)
    admin->Resized(this, TRUE);
#endif

  return this;
}

void wxTextSnip::InsertWithOffset(wxchar *str, long len, long delta, long pos)
{
  if (len <= 0)
    return;
  if (pos < 0)
    pos = 0;

  if (allocated < count + len) {
    wxchar *naya;

    allocated = 2 * (count + len);
    naya = STRALLOC(allocated + 1);
    
    memcpy(naya, buffer + dtext, count * sizeof(wxchar));

    buffer = naya;
    dtext = 0;
  } else if (dtext && (dtext + count + len > allocated)) {
    memmove(buffer, buffer + dtext, count * sizeof(wxchar));
    dtext = 0;
  }
   
  if (pos < count)
    memmove(buffer + dtext + pos + len, 
	    buffer + dtext + pos, 
	    (count - pos) * sizeof(wxchar));
  memcpy(buffer + dtext + pos, str + delta, len * sizeof(wxchar));
  
  count += len;

  w = -1.0;
  
#if CHECK_CS_FLAG
  if (!(flags & wxSNIP_CAN_SPLIT) && admin)
    if (!admin->Recounted(this, TRUE))
      count -= len;
#endif
}

void wxTextSnip::Insert(wxchar *str, long len, long pos)
{
  InsertWithOffset(str, len, 0, pos);
}

void wxTextSnip::InsertUTF8(char *str, long len, long pos)
{
  long ulen;
  wxchar *us = NULL;
  wxme_utf8_decode(str, len, &us, &ulen);
  Insert(us, ulen, pos);
}

void wxTextSnip::GetTextBang(wxchar *s, long offset, long num, long dt)
{
  if (num <= 0)
    return;

  memcpy(s + dt, buffer + dtext + offset, num * sizeof(wxchar));
}

wxchar *wxTextSnip::GetText(long offset, long num, Bool flat, long *got)
{
  if (offset < 0) offset = 0;
  if ((num <= 0) || (offset >= count)) {
    if (got)
      *got = 0;
    return wx_empty_wxstr;
  }
  if (num + offset > count)
    num = count - offset;

  {
    wxchar *s;
    s = new WXGC_ATOMIC wxchar[num + 1];
    memcpy(s, buffer + dtext + offset, num * sizeof(wxchar));
    s[num] = 0;
    if (got)
      *got = num;
    return s;
  }
}

wxSnip *wxTextSnip::Copy()
{
  wxTextSnip *snip;

  snip = new WXGC_PTRS wxTextSnip(count);
  Copy(snip);
  return snip;
}

void wxTextSnip::Copy(wxTextSnip *snip) 
{
  wxSnip::Copy(snip);

  if (snip->allocated < count) {
    int a;
    wxchar *s;
    a = count + 10;
    s = STRALLOC(a + 1);
    snip->allocated = a;
    snip->buffer = s;
  }
  
  memcpy(snip->buffer + snip->dtext, buffer + dtext, count * sizeof(wxchar));
  snip->count = count;
  snip->dtext = 0;

  snip->w = -1.0;
}

void wxTextSnip::Write(wxMediaStreamOut *f)
{
  long writeFlags, ul;
  char *ub, buf[128];

  writeFlags = flags;
  if (writeFlags & wxSNIP_OWNED)
    writeFlags -= wxSNIP_OWNED;
  if (writeFlags & wxSNIP_CAN_DISOWN)
    writeFlags -= wxSNIP_CAN_DISOWN;
  if (writeFlags & wxSNIP_CAN_SPLIT)
    writeFlags -= wxSNIP_CAN_SPLIT;

  f->Put(writeFlags);

  ul = scheme_utf8_encode(buffer, dtext, dtext + count, NULL, 0, 0);
  if (ul <= 128)
    ub = buf;
  else
    ub = new WXGC_ATOMIC char[ul];
  scheme_utf8_encode(buffer, dtext, dtext + count, (unsigned char *)ub, 0, 0);
  f->Put(ul, ub, 0);
}

void wxTextSnip::Read(long len, wxMediaStreamIn *f)
{
  int rv;

  if (len <= 0)
    return;

  if (allocated < len) {
    long l = 2 * len;
    if (l < 0) {
      Read(100, f);
      return;
    }
    STRFREE(buffer);
    if (l > 500) {
      wxchar *ts;
      ts = TRY_STRALLOC(l + 1);
      buffer = ts;
      if (!buffer) {
	Read(100, f);
	return;
      }
    } else {
      buffer = STRALLOC(l + 1);
    }

    allocated = l;
    if (!buffer)
      Read(10, f);
  }

  dtext = 0;
  rv = f->ReadingVersion(TheTextSnipClass);
  if (rv < 2) {
    int i;
    /* Read Latin-1: */
    f->Get((long *)&len, (char *)buffer);
    /* Expand out Latin-1: */
    for (i = len; i--; ) {
      buffer[i] = ((unsigned char *)buffer)[i];
    }
    count = len;
  } else if (rv > 2) {
    /* Read UTF-8: */
    char *ub, buf[128];
    long bl;
    if (len > 128)
      ub = new WXGC_ATOMIC char[len];
    else
      ub = buf;
    bl = len;
    f->Get(&bl, ub);
    len = scheme_utf8_decode((unsigned char *)ub, 0, bl,
			     buffer, 0, len,
			     NULL, 0, 1);
    count = len;
  } else {
    /* Version 2 wrote out UTF-32 directly -- bad idea!
       because it uses the machine's endianness. */
    len *= sizeof(wxchar);
    f->Get((long *)&len, (char *)buffer);
    count = len / sizeof(wxchar);
  }
  w = -1.0;
}

#ifdef MEMORY_USE_METHOD
long wxTextSnip::MemoryUse(void)
{
  return allocated + wxObject::MemoryUse();
}
#endif

/***************************************************************/

class TabSnipClass : public TextSnipClass
{
 public:
  TabSnipClass(void);

  virtual wxSnip *Read(wxMediaStreamIn *);
};

static TabSnipClass *TheTabSnipClass;

TabSnipClass::TabSnipClass(void)
{
  classname = "wxtab";
  version = 1;
  required = TRUE;
}

wxSnip *TabSnipClass::Read(wxMediaStreamIn *f)
{
  wxTabSnip *ts;
  ts = new WXGC_PTRS wxTabSnip();
  return TextSnipClass::Read(ts, f);
}

/***************************************************************/

wxTabSnip::wxTabSnip() : wxTextSnip(1)
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_TAB_SNIP;
#endif

  snipclass = TheTabSnipClass;

  flags |= wxSNIP_WIDTH_DEPENDS_ON_X;
  flags -= (flags & wxSNIP_CAN_APPEND);
}

void wxTabSnip::GetExtent(wxDC *dc, 
			  double x, double y, 
			  double *wi, double *h, 
			  double *descent, double *space,
			  double *lspace, double *rspace)
{
  double *tabs, oldw;
  double tabspace;
  int n, i;
  Bool changed;

  changed = (w < 0);
  oldw = w;

  wxTextSnip::GetExtent(dc, x, y, wi, h, descent, space, lspace, rspace);

  if (changed) {
    /* w is now width of a space */
    double mult;
    wxMediaBuffer *media = NULL;

    if (admin && (media = admin->GetMedia()) && (media->bufferType == wxEDIT_BUFFER)) {
      double space;
      Bool units;
      wxMediaEdit *edt;

      edt = (wxMediaEdit *)admin->GetMedia();
      tabs = edt->GetTabs(&n, &space, &units);
      tabspace = space;
      mult = units ? 1 : w;
    } else {
      n = 0;
      tabs = NULL;
      tabspace = wxTAB_WIDTH;
      mult = 1;
    }
    
    for (i = 0; i < n; i++) {
      if (tabs[i] * mult > x) {
	w = tabs[i] * mult - x;
	break;
      }
    }

    if (i >= n) {
      double base;

      base = tabs ? (tabs[n - 1] * mult) : 0;
      x -= base;

      tabspace *= mult;
      w = base + ((long)tabspace - ((long)x % (long)tabspace));
    }
    
  } else
    w = oldw;

  if (wi)
    *wi = w;
}

double wxTabSnip::PartialOffset(wxDC *dc, double x, double y, long offset)
{
  double _w;

  if (!offset)
    return 0;
  else {
    _w = 0.0;
    GetExtent(dc, x, y, &_w);
    return _w;
  }
}

void wxTabSnip::Draw(wxDC *, double, double, 
		     double, double, double, double, 
		     double, double, int)
{
  /* Do nothing! */
}

wxSnip *wxTabSnip::Copy()
{
  wxTabSnip *snip;

  snip = new WXGC_PTRS wxTabSnip();
  wxTextSnip::Copy(snip);
  return snip;
}

/***************************************************************/

#define IMG_MOVE_BUF_SIZE 500

class ImageSnipClass : public wxSnipClass
{
 public:
  ImageSnipClass(void);

  virtual wxSnip *Read(wxMediaStreamIn *);
};

static ImageSnipClass *TheImageSnipClass;

ImageSnipClass::ImageSnipClass(void)
{
  classname = "wximage";
  version = 2;
  required = FALSE;
}

wxSnip *ImageSnipClass::Read(wxMediaStreamIn *f)
{
  wxImageSnip *snip;
  char *filename, *delfile = NULL, *loadfile;
  long type;
  Bool relative, inlined = FALSE;
  double w, h, dx, dy;
  wxStandardSnipClassList *scl;
  Bool canInline;

  scl = wxGetTheSnipClassList();
  canInline = (f->ReadingVersion(this) > 1);

  filename = f->GetString(NULL);
  f->Get(&type);
  f->Get(&w);
  f->Get(&h);
  f->Get(&dx);
  f->Get(&dy);
  f->Get(&relative);

  loadfile = filename;

  if (filename && !*filename && canInline && type) {
    /* read inlined image */

    long len;
    f->GetFixed(&len);

    if ((len > 0) && f->Ok()) {
      char *fname;
      FILE *fi;
      char buffer[IMG_MOVE_BUF_SIZE + 1];
    
      fname = wxGetTempFileName("img", NULL);

      fi = fopen(fname, "wb");
      if (fi) {
	long c;
	
	while (len--) {
	  c = IMG_MOVE_BUF_SIZE + 1;
	  f->Get(&c, buffer);

	  if (!f->Ok())
	    break;
	  
	  c = fwrite(buffer, 1, c, fi);
	}
	fclose(fi);

	loadfile = fname;
	type = wxBITMAP_TYPE_MASK; /* use a mask if available (PNG) */
	inlined = TRUE;
      }

      delfile = fname;      
    }
  }
  
  snip = new WXGC_PTRS wxImageSnip(loadfile, type, relative, inlined);

  if (delfile) {
    wxRemoveFile(delfile);
  }

  snip->Resize(w, h);
  snip->SetOffset(dx, dy);

  return (wxSnip *)snip;
}

/***************************************************************/

wxImageSnip::wxImageSnip(char *name, long type, Bool relative, Bool inlineImg)
{
  Init();

  if (name && *name)
    LoadFile(name, type, relative, inlineImg);
}

wxImageSnip::wxImageSnip(wxBitmap *bm, wxBitmap *mask)
{
  Init();

  SetBitmap(bm, mask);
}

void wxImageSnip::Init(void)
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_IMAGE_SNIP;
#endif

  snipclass = TheImageSnipClass;

  contentsChanged = TRUE;
  filename = NULL;
  filetype = 0;
  relativePath = FALSE;
  bm = NULL;
  vieww = viewh = -1.0;
  viewdx = viewdy = 0.0;
}

wxImageSnip::~wxImageSnip()
{
}

void wxImageSnip::SizeCacheInvalid(void)
{
  contentsChanged = TRUE;  
}


void wxImageSnip::GetExtent(wxDC *,
			    double WXUNUSED(x), 
			    double WXUNUSED(y),
			    double *wi, double *hi, 
			    double *descent, double *space,
			    double *lspace, double *rspace)
{
  if (contentsChanged) {
    if (bm && bm->Ok()) {
      if (viewh < 0) {
	int bmh;
	bmh = bm->GetHeight();
	h = bmh;
      } else
	h = viewh;
      if (vieww < 0) {
	int bmw;
	bmw = bm->GetWidth();
	w = bmw;
      } else
	w = vieww;
    } else {
      h = w = 0;
    }
    if (!h)
      h = IMAGE_VOID_SIZE;
    if (!w)
      w = IMAGE_VOID_SIZE;
  }

  if (wi)
    *wi = w;
  if (hi)
    *hi = h;
  if (descent) {
    if (!bm || !bm->Ok())
      *descent = 1;
    else
      *descent = 0;
  }
  ALWAYSZERO(space);
  ALWAYSZERO(lspace);
  ALWAYSZERO(rspace);
}

void wxImageSnip::Draw(wxDC *dc, double x, double y, 
		       double WXUNUSED(l), double WXUNUSED(t), 
		       double WXUNUSED(r), double WXUNUSED(b),
		       double WXUNUSED(dx), double WXUNUSED(dy), 
		       int)
{
  wxBitmap *msk;

  if (!bm || !bm->Ok()) {
    dc->DrawRectangle(x + 1, y + 1, 
		      w - 2 + GC_RECT_FRAME_EXTEND, 
		      h - 2 + GC_RECT_FRAME_EXTEND);
    dc->DrawLine(x + 1, y + 1, 
		 x + w - 2 + GC_LINE_EXTEND, 
		 y + h - 2 + GC_LINE_EXTEND);
    dc->DrawLine(x + 1, y + h - 2, 
		 x + w - 2 + GC_LINE_EXTEND, 
		 y + 1 - GC_LINE_EXTEND);
    return;
  }

  if (mask)
    msk = mask;
  else {
    msk = bm->GetMask();
    if (msk && (!msk->Ok() 
		|| (msk->GetWidth() != w)
		|| (msk->GetHeight() != h)))
      msk = NULL;
  }

  dc->Blit(x, y, w, h, bm, 0, 0, wxCOPY, NULL, msk);
  return;
}

wxSnip *wxImageSnip::Copy(void)
{
  wxImageSnip *snip;

  snip = new WXGC_PTRS wxImageSnip();
  Copy(snip);

  return (wxSnip *)snip;
}

void wxImageSnip::Write(wxMediaStreamOut *f)
{
  int writeBm = 0, writePm = 0;

  f->Put((filename ? filename : (char *)""));
  if (filename)
    f->Put(filetype);
  else {
    if (!bm)
      f->Put(0);
    else if (bm->GetDepth() == 1) {
      f->Put(1);
      writeBm = 1;
    } else {
      f->Put(2);
      writePm = 1;
    }
  }
  f->Put(vieww);
  f->Put(viewh);
  f->Put(viewdx);
  f->Put(viewdy);
  f->Put(relativePath);

  /* inline the image */
  if (writeBm || writePm) {
    FILE *fi;
    char buffer[IMG_MOVE_BUF_SIZE];
    long lenpos, numlines = 0;
    char *fname;
    long end;

    lenpos = f->Tell();
    f->PutFixed(0);

    fname = wxGetTempFileName("img", NULL);

    bm->SaveFile(fname, wxBITMAP_TYPE_PNG);
    
    fi = fopen(fname, "rb");
    if (fi) {
      while (1) {
	int c;
	c = fread(buffer, 1, IMG_MOVE_BUF_SIZE, fi);
	if (c) {
	  numlines++;
	  f->Put(c, buffer);
	} else 
	  break;
      }
      fclose(fi);
    }

    wxRemoveFile(fname);

    end = f->Tell();
    f->JumpTo(lenpos);
    f->PutFixed(numlines);
    f->JumpTo(end);
  }
}

void wxImageSnip::LoadFile(char *name, long type, Bool relative, Bool inlineImg)
{
  if (name && !*name)
    name = NULL;

  SetBitmap(NULL, NULL, FALSE);

  if (relative && name) {
#ifdef wx_mac
    if (name[0] != ':') {
      int i;
      for (i = 0; name[i]; i++) {
	if (name[i] == ':') {
	  relative = FALSE;
	  break;
	}
      }
    }
#else
    if (name[0] == '/')
      relative = FALSE;
#ifdef wx_msw
    if (name[0] == '\\')
      relative = FALSE;
    if (name[0] && name[1] == ':')
      relative = FALSE;
#endif
#ifdef wx_x
    if (name[0] == '~')
      relative = FALSE;
#endif
#endif
  }

  relativePath = relative && name;

  if (relativePath)
    flags |= wxSNIP_USES_BUFFER_PATH;
  else if (flags & wxSNIP_USES_BUFFER_PATH)
    flags -= wxSNIP_USES_BUFFER_PATH;

  if (name) {
    char *loadname, *fn;
    wxBitmap *nbm = NULL;

    loadname = name;

    if (!relativePath || admin) {
      if (relativePath) {
	wxMediaBuffer *b;
	char *path;
	
	b = admin ? admin->GetMedia() : (wxMediaBuffer *)NULL;
	fn = b ? b->GetFilename() : (char *)NULL;
	if (fn) {
	  path = wxPathOnly(fn);
	  if (path) {
	    loadname = new WXGC_ATOMIC char[strlen(path) + strlen(name) + 2];
	    strcpy(loadname, path);
#ifdef wx_x
	    strcat(loadname, "/");
#else
#ifdef wx_mac
	    strcat(loadname, ":");
#else
	    strcat(loadname, "\\");
#endif	    
#endif
	    strcat(loadname, name);
	  }
	}
      }
      
      fn = (char *)wxmeExpandFilename(loadname, "load-file in image-snip%", 0);

      wxBeginBusyCursor();

      nbm = new WXGC_PTRS wxBitmap(fn, type);

      wxEndBusyCursor();

      if (!nbm->Ok()) {
	DELETE_OBJ nbm;
	nbm = NULL;
      }
    }

    if (!inlineImg) {
      filename = copystring(name);
      filetype = type;
    } else
      filename = NULL;

    if (nbm)
      SetBitmap(nbm, NULL, FALSE);
  } else {
    filename = NULL;
  }

  /* For refresh: */
  SetBitmap(bm, mask);
}

void wxImageSnip::Copy(wxImageSnip *newSnip)
{
  wxSnip::Copy(newSnip);
  
  if (filename) {
    newSnip->filename = copystring(filename);
  } else
    newSnip->filename = NULL;
  newSnip->filetype = filetype;
  newSnip->relativePath = relativePath;

  newSnip->vieww = vieww;
  newSnip->viewh = viewh;
  newSnip->viewdx = viewdx;
  newSnip->viewdy = viewdy;

  newSnip->bm = bm;
  newSnip->mask = mask;

  if (bm)
    bm->selectedIntoDC++;
  if (mask)
    mask->selectedIntoDC++;
}

char *wxImageSnip::GetFilename(Bool *rel)
{
  if (rel)
    *rel = filename && relativePath;

  return filename;
}

long wxImageSnip::GetFiletype()
{
  return filename ? 0 : filetype;
}

void wxImageSnip::SetBitmap(wxBitmap *map, wxBitmap *msk, int refresh)
{
  if ((map && (map->selectedIntoDC < 0))
      || (msk && (msk->selectedIntoDC < 0)))
    return;

  if (bm)
    --bm->selectedIntoDC;
  if (mask)
    --mask->selectedIntoDC;

  bm = NULL;
  mask = NULL;

  if ((!map || map->Ok())
      && (!msk || msk->Ok())) {
    if (map)
      map->selectedIntoDC++;
    if (msk)
      msk->selectedIntoDC++;
    
    bm = map;
    mask = msk;
  }

  if (refresh) {
    contentsChanged = TRUE;
    
    if (admin)
      admin->Resized(this, TRUE);
  }
}

wxBitmap *wxImageSnip::GetSnipBitmap()
{
  return bm;
}

wxBitmap *wxImageSnip::GetSnipBitmapMask()
{
  return mask;
}

void wxImageSnip::SetOffset(double x, double y)
{
  viewdx = x;
  viewdy = y;

  contentsChanged = TRUE;

  if (admin)
    admin->NeedsUpdate(this, 0, 0, w, h);
}

Bool wxImageSnip::Resize(double w, double h)
{
  vieww = w;
  viewh = h;

  contentsChanged = TRUE;

  if (admin)
    admin->Resized(this, TRUE);

  return TRUE;
}

long wxImageSnip::GetNumScrollSteps()
{
  long ss;

  ss = (long)(h / IMAGE_PIXELS_PER_SCROLL);

  return ss ? ss : 1;
}

long wxImageSnip::FindScrollStep(double y)
{
  return (long)(y / IMAGE_PIXELS_PER_SCROLL);
}

double wxImageSnip::GetScrollStepOffset(long i)
{
  return ((double )i) * IMAGE_PIXELS_PER_SCROLL;
}

void wxImageSnip::SetAdmin(wxSnipAdmin *a)
{
  if (PTRNE(admin, a))
    wxSnip::SetAdmin(a);
  if (admin && relativePath && filename)
    LoadFile(filename, filetype, TRUE);
}

/***************************************************************/

class MediaSnipClass : public wxSnipClass
{
 public:
  MediaSnipClass(void);

  virtual wxSnip *Read(wxMediaStreamIn *);
};

static MediaSnipClass *TheMediaSnipClass;

MediaSnipClass::MediaSnipClass(void)
{
  classname = "wxmedia";
  version = 4;
  required = TRUE;
}

wxSnip *MediaSnipClass::Read(wxMediaStreamIn *f)
{
  wxMediaBuffer *media;
  wxMediaSnip *snip;
  Bool border, tightFit = 0, alignTopLine = 0, useStyleBG = 0;
  int lm, tm, rm, bm, li, ti, ri, bi, type;
  double w, W, h, H;
  wxStandardSnipClassList *scl;

  f->Get(&type);
  f->Get(&border);
  f->Get(&lm);
  f->Get(&tm);
  f->Get(&rm);
  f->Get(&bm);
  f->Get(&li);
  f->Get(&ti);
  f->Get(&ri);
  f->Get(&bi);
  f->Get(&w);
  f->Get(&W);
  f->Get(&h);
  f->Get(&H);
  
  scl = wxGetTheSnipClassList();
  if (f->ReadingVersion(this) > 1)
    f->Get(&tightFit);
  if (f->ReadingVersion(this) > 2)
    f->Get(&alignTopLine);
  if (f->ReadingVersion(this) > 3)
    f->Get(&useStyleBG);
  
  if (!type)
    media = NULL;
  else if (type == wxEDIT_BUFFER)
    media = wxsMakeMediaEdit();
  else
    media = wxsMakeMediaPasteboard();

  if (lm < 0) lm = 0;
  if (tm < 0) tm = 0;
  if (rm < 0) rm = 0;
  if (bm < 0) bm = 0;
  if (li < 0) li = 0;
  if (ti < 0) ti = 0;
  if (ri < 0) ri = 0;
  if (bi < 0) bi = 0;

  snip = wxsMakeMediaSnip(media, border, lm, tm, rm, bm, li, ti, ri, bi,
			  w, W, h, H);
  if (tightFit)
    snip->SetTightTextFit(1);
  if (alignTopLine)
    snip->SetAlignTopLine(1);
  if (useStyleBG)
    snip->UseStyleBG(1);
  
  if (media) {
    wxStyleList *sl;
    sl = media->GetStyleList();
    media->ReadFromFile(f, TRUE);
  } else
    snip->SetMedia(NULL);

  return snip;
}

/***************************************************************/

wxSnipClassList::wxSnipClassList(void)
: wxList((KeyType)wxKEY_STRING, FALSE)
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_SNIP_CLASS_LIST;
#endif
}

wxSnipClassList::~wxSnipClassList()
{
}

wxSnipClass *wxSnipClassList::Find(char *name)
{
  wxNode *node;

  node = wxList::Find(name);
  if (!node) {
    /* invoke getter and try again */
    wxSnipClass *sc;
    sc = wxGetSnipClass(name);
    if (sc)
      Add(sc);
    node = wxList::Find(name);
  }

  return node ? (wxSnipClass *)node->Data() : (wxSnipClass *)NULL;
}

short wxSnipClassList::FindPosition(wxSnipClass *sclass)
{
  wxNode *node;
  short i;
  
  for (i = 0, node = First(); node; node = node->Next(), i++) {
    if (PTREQ(sclass, (wxSnipClass *)node->Data()))
      return i;
  }
  
  return -1;
}

void wxSnipClassList::Add(wxSnipClass *snipclass)
{
  Append(snipclass->classname, snipclass);
}

int wxSnipClassList::Number(void)
{
  return wxList::Number();
}

wxSnipClass *wxSnipClassList::Nth(int n)
{
  wxNode *node;
  node = wxList::Nth(n);

  if (node)
    return (wxSnipClass *)node->Data();
  else
    return NULL;
}

/***************************************************************/

wxStandardSnipClassList::wxStandardSnipClassList(void)
{
  wxList *ul;

  ul = new WXGC_PTRS wxList((KeyType)wxKEY_INTEGER);
  unknowns = ul;

  Add(TheTextSnipClass);
  Add(TheTabSnipClass);
  Add(TheMediaSnipClass);
  Add(TheImageSnipClass);
}

void wxStandardSnipClassList::ResetHeaderFlags(wxMediaStream *s)
{
  s->sl = NULL;
  s->dl = NULL;
}

Bool wxStandardSnipClassList::Write(wxMediaStreamOut *f)
{
  wxNode *node;
  wxSnipClass *sclass;
  short i;

  f->Put(Number());

  for (i = 0, node = First(); node; node = node->Next(), i++) {
    wxSnipClassLink *sl;

    sclass = (wxSnipClass *)node->Data();
    f->Put(sclass->classname);
    f->Put(sclass->version);
    f->Put(sclass->required);

    sl = new WXGC_PTRS wxSnipClassLink;
    sl->c= sclass;
    sl->mapPosition = i;
    sl->headerFlag = 0;
    sl->next = f->sl;
    f->sl = sl;
  }

  return TRUE;
}

Bool wxStandardSnipClassList::Read(wxMediaStreamIn *f)
{
  int count, i;
  long _n;
  char buffer[256];
  int version;
  Bool required;
  wxNode *node, *next;
  wxSnipClassLink *sl;

  f->Get(&count);

  buffer[255] = 0;

  for (node = unknowns->First(); node; node = next) {
    next = node->Next();
    DELETE_OBJ node;
  }

  for (i = 0; i < count; i++) {
    _n = 255;
    f->Get((long *)&_n, (char *)buffer);
    f->Get(&version);
    f->Get(&required);
    if (!f->Ok())
      return FALSE;

    sl = new WXGC_PTRS wxSnipClassLink;
    sl->c = NULL;
    sl->mapPosition = i;
    sl->next = f->sl;
    f->sl = sl;
    sl->name = copystring(buffer);
    sl->readingVersion = version;
  }

  return TRUE;
}

wxSnipClass *wxStandardSnipClassList::FindByMapPosition(wxMediaStream *f, short n)
{
  wxSnipClassLink *sl;
  wxSnipClass *sclass;
  
  if (n < 0)
    return NULL;

  for (sl = f->sl; sl; sl = sl->next) {
    if (sl->mapPosition == n) {
      if (sl->name) {
	sclass = Find(sl->name);
	if (!sclass || (sclass->version < sl->readingVersion)) {
	  /* unknown class/version */
	  /* since we zero out sl->name, error is only shown once */
	  char buffer2[256];
	  sprintf(buffer2, "Unknown snip class or version: \"%.100s\" version %d.", sl->name, sl->readingVersion);
	  wxmeError(buffer2);
	} else {
	  sl->c = sclass;
	}
	sl->name = NULL;
      }

      return sl->c;
    }
  }

  return NULL;
}

wxStandardSnipClassList *wxMakeTheSnipClassList(void)
{
  return new WXGC_PTRS wxStandardSnipClassList;
}

/***************************************************************/

wxBufferDataClass::wxBufferDataClass()
: wxObject(WXGC_NO_CLEANUP)
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_BUFFER_DATA_CLASS;
#endif

  classname = "wxbad";
  required = 0;
}

wxBufferData::wxBufferData()
: wxObject(WXGC_NO_CLEANUP)
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_BUFFER_DATA;
#endif

  next = NULL;
}

wxBufferData::~wxBufferData()
{
  if (next) {
    DELETE_OBJ next;
    next = NULL;
  }
}

class LocationBufferDataClass : public wxBufferDataClass
{
 public:
  LocationBufferDataClass();
  wxBufferData *Read(wxMediaStreamIn *);
};

LocationBufferDataClass::LocationBufferDataClass()
{
  classname = "wxloc";
  required = 1;
}

wxBufferData *LocationBufferDataClass::Read(wxMediaStreamIn *f)
{
  wxLocationBufferData *data;

  data = new WXGC_PTRS wxLocationBufferData;
  f->Get(&data->x);
  f->Get(&data->y);

  return data;
}

static LocationBufferDataClass *TheLocationBufferDataClass;

wxLocationBufferData::wxLocationBufferData()
{
  x = y = 0;
  dataclass = TheLocationBufferDataClass;
}

Bool wxLocationBufferData::Write(wxMediaStreamOut *f)
{
  f->Put(x);
  f->Put(y);
  return TRUE;
}

/**************************************************************/

wxBufferDataClassList::wxBufferDataClassList(void)
: wxList((KeyType)wxKEY_STRING, FALSE)
{
  wxList *ul;

#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_BUFFER_DATA_CLASS_LIST;
#endif
  
  ul = new WXGC_PTRS wxList((KeyType)wxKEY_INTEGER);
  unknowns = ul;

  Add(TheLocationBufferDataClass);
}

wxBufferDataClassList::~wxBufferDataClassList()
{
}

wxBufferDataClass *wxBufferDataClassList::Find(char *name)
{
  wxNode *node;

  node = wxList::Find(name);

  if (!node) {
    /* invoke getter and try again */
    wxBufferDataClass *bdc;
    bdc = wxGetEditorDataClass(name);
    if (bdc)
      Add(bdc);
    node = wxList::Find(name);
  }
  
  return node ? (wxBufferDataClass *)node->Data() : (wxBufferDataClass *)NULL;
}

short wxBufferDataClassList::FindPosition(wxBufferDataClass *sclass)
{
  wxNode *node;
  short i;
  
  for (i = 0, node = First(); node; node = node->Next(), i++) {
    if (PTREQ(sclass, (wxBufferDataClass *)node->Data()))
      return i + 1;
  }

  return 0;
}

void wxBufferDataClassList::Add(wxBufferDataClass *dataclass)
{
  Append(dataclass->classname, dataclass);
}

int wxBufferDataClassList::Number(void)
{
  return wxList::Number();
}

wxBufferDataClass *wxBufferDataClassList::Nth(int n)
{
  wxNode *o;

  o = wxList::Nth(n);

  if (!o)
    return NULL;
  else
    return (wxBufferDataClass *)o->Data();
}

Bool wxBufferDataClassList::Write(wxMediaStreamOut *f)
{
  wxNode *node;
  wxBufferDataClass *sclass;
  wxDataClassLink *dl;
  short i;

  f->Put(Number());

  for (i = 0, node = First(); node; node = node->Next(), i++) {
    sclass = (wxBufferDataClass *)node->Data();
    f->Put(sclass->classname);

    dl = new WXGC_PTRS wxDataClassLink;
    dl->d = sclass;
    dl->mapPosition = i + 1;
    dl->next = f->dl;
    f->dl = dl;
  }

  return TRUE;
}

Bool wxBufferDataClassList::Read(wxMediaStreamIn *f)
{
  int _count, i;
  long _n;
  wxDataClassLink *dl;
  char buffer[256];
  
  f->Get(&_count);

  buffer[255] = 0;

  for (i = 0; i < _count; i++) {
    _n = 255;
    f->Get((long *)&_n, (char *)buffer);
    if (!f->Ok())
      return FALSE;

    dl = new WXGC_PTRS wxDataClassLink;
    dl->d = NULL;
    dl->mapPosition = i + 1;
    dl->next = f->dl;
    f->dl = dl;
    dl->name = copystring(buffer);
  }

  return TRUE;
}

wxBufferDataClass *wxBufferDataClassList::FindByMapPosition(wxMediaStream *f, short n)
{
  wxDataClassLink *dl;
  wxBufferDataClass *sclass;
  
  if (n <= 0)
    return NULL;

  for (dl = f->dl; dl; dl = dl->next) {
    if (dl->mapPosition == n) {
      if (dl->name) {
	sclass = Find(dl->name);
	if (!sclass) {
	  char buffer2[256];
	  sprintf(buffer2, "Unknown snip data class or version: \"%.100s\".", dl->name);
	  wxmeError(buffer2);
	} else
	  dl->d = sclass;
	dl->name = NULL;
      }

      return dl->d;
    }
  }

  return NULL;
}

wxBufferDataClassList *wxMakeTheBufferDataClassList()
{
  return new WXGC_PTRS wxBufferDataClassList;
}

/**************************************************/

void wxInitSnips(void)
{
  wxREGGLOB(TheTextSnipClass);
  wxREGGLOB(TheTabSnipClass);
  wxREGGLOB(TheMediaSnipClass);
  wxREGGLOB(TheImageSnipClass);
  wxREGGLOB(TheLocationBufferDataClass);


  TheTextSnipClass = new WXGC_PTRS TextSnipClass;
  TheTabSnipClass = new WXGC_PTRS TabSnipClass;
  TheMediaSnipClass = new WXGC_PTRS MediaSnipClass;
  TheImageSnipClass = new WXGC_PTRS ImageSnipClass;
  TheLocationBufferDataClass = new WXGC_PTRS LocationBufferDataClass;
}
