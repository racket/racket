/*
 * File:        wx_cgrec.cc
 * Purpose:     wxChangeRecord implementations
 * Author:      Matthew Flatt
 * Created:     1995
 * Copyright:   (c) 2004-2007 PLT Scheme Inc.
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
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301 USA.

 */

#if defined(_MSC_VER) && defined(MZ_PRECISE_GC)
# include "wx.h"
#endif
#include "common.h"
#include <stdio.h>
#include <string.h>
#include "wx_media.h"
#include "wx_ptreq.h"

class wxcgList 
#ifndef MZ_PRECISE_GC
: public gc
#endif
{
public:
  long count, size;
  wxObject **array;

  wxcgList(void) { count = size = 0; }
  
  int Count() { return count; }
  wxObject *Get(int i) { return array[i]; }

  void Append(wxObject *o);
  void DeleteAll(void);
};

void wxcgList::Append(wxObject *o)
{
  if (count >= size) {
    wxObject **naya;
    if (size)
      size *= 2;
    else
      size = 2;
    naya = new WXGC_PTRS wxObject*[size];
    memcpy(naya, array, count * sizeof(wxObject*));
    array = naya;
  }
  array[count++] = o;
}

void wxcgList::DeleteAll(void)
{
  wxObject *o;
  while (count--) {
    o = array[count];
    DELETE_OBJ o;
  }
}

wxChangeRecord::wxChangeRecord(void)
{
}

wxChangeRecord::~wxChangeRecord(void)
{
}

Bool wxChangeRecord::Undo(wxMediaBuffer *)
{
  return FALSE;
}

void wxChangeRecord::DropSetUnmodified(void)
{
}

Bool wxChangeRecord::IsComposite()
{
  return FALSE;
}

wxChangeRecordId *wxChangeRecord::GetId()
{
  return NULL;
}

int wxChangeRecord::GetParity()
{
  return 0;
}

wxChangeRecord *wxChangeRecord::Inverse()
{
  return NULL;
}

#if CGREC_DEBUG
char *wxChangeRecord::GetName()
{
  return "???";
}
#endif


wxSchemeModifyRecord::wxSchemeModifyRecord(void *proc)
{
  p = proc;
}

extern int wxsSchemeUndo(void *);

Bool wxSchemeModifyRecord::Undo(wxMediaBuffer *)
{
  return wxsSchemeUndo(p);
}

#if CGREC_DEBUG
char *wxSchemeModifyRecord::GetName()
{
  return "scheme-modify";
}
#endif


wxUnmodifyRecord::wxUnmodifyRecord(Bool _cont)
{
  ok = 1;
  cont = _cont;
}

Bool wxUnmodifyRecord::Undo(wxMediaBuffer *media)
{
  if (ok)
    media->SetModified(FALSE);
  return cont;
}

void wxUnmodifyRecord::DropSetUnmodified(void)
{
  ok = 0;
}

#if CGREC_DEBUG
char *wxUnmodifyRecord::GetName()
{
  return "unmodify";
}
#endif


wxInsertRecord::wxInsertRecord(long position, long length, Bool cont, long ss, long es)
{
  start = position;
  end = position + length;
  continued = cont;
  startsel = ss;
  endsel = es;
}

Bool wxInsertRecord::Undo(wxMediaBuffer *buffer)
{
  wxMediaEdit *media;
  media = (wxMediaEdit *)buffer;

  media->Delete(start, end);

  media->SetPosition(startsel, endsel);

  return continued;
}

#if CGREC_DEBUG
char *wxInsertRecord::GetName()
{
  return "insert";
}
#endif

wxInsertSnipRecord::wxInsertSnipRecord(wxSnip *thesnip, Bool cont)
{
  snip = thesnip;
  continued = cont;
}

Bool wxInsertSnipRecord::Undo(wxMediaBuffer *buffer)
{
  wxMediaPasteboard *media;
  media = (wxMediaPasteboard *)buffer;

  media->Delete(snip);
  if (!continued)
   media->SetSelected(snip);

  return continued;
}

#if CGREC_DEBUG
char *wxInsertSnipRecord::GetName()
{
  return "insert-snip";
}
#endif

class DeleteSnipItem /* : public wxObject --- uncomment to GC */
{
 public:
  ~DeleteSnipItem();

  wxDeleteSnipRecord *parent;
  wxSnip *snip, *before;
  double x, y;
};

DeleteSnipItem::~DeleteSnipItem()
{
  if (!parent->undid) {
    if (snip->flags & wxSNIP_OWNED)
      snip->flags -= wxSNIP_OWNED;
    snip->SetAdmin(NULL);
  }
}

wxDeleteSnipRecord::wxDeleteSnipRecord(Bool cont)
{
  continued = cont;
  deletions = new WXGC_PTRS wxcgList();
}

wxDeleteSnipRecord::~wxDeleteSnipRecord()
{
  int i;
  
  for (i = deletions->Count(); i--; ) {
    DeleteSnipItem *ds;
    ds = (DeleteSnipItem *)deletions->Get(i);
  }

  DELETE_OBJ deletions;
}

void wxDeleteSnipRecord::InsertSnip(wxSnip *snip, wxSnip *before, 
				    double x, double y)
{
  DeleteSnipItem *item;

  item = new WXGC_PTRS DeleteSnipItem();
  item->parent = this;
  item->snip = snip;
  item->before = before;
  item->x = x;
  item->y = y;

  deletions->Append((wxObject *)item);
}

Bool wxDeleteSnipRecord::Undo(wxMediaBuffer *buffer)
{
  DeleteSnipItem *item;
  wxMediaPasteboard *media;
  int i, count;

  media = (wxMediaPasteboard *)buffer;
  
  if (!continued)
    media->NoSelected();

  count = deletions->Count();
  for (i = 0; i < count; i++) {
    item = (DeleteSnipItem *)deletions->Get(i);

    /* Have to turn off the owned flag; we know that it's really ours */
    if (item->snip->flags & wxSNIP_OWNED)
      item->snip->flags -= wxSNIP_OWNED;

    media->Insert(item->snip, item->before, item->x, item->y);
    if (!continued)
      media->AddSelected(item->snip);
  }

  undid = TRUE;

  return continued;
}

#if CGREC_DEBUG
char *wxDeleteSnipRecord::GetName()
{
  return "delete-snip";
}
#endif

wxDeleteRecord::wxDeleteRecord(long startpos, long endpos, Bool cont, long ss, long es)
{
  continued = cont;
  start = startpos;
  end = endpos;
  startsel = ss;
  endsel = es;
  undid = FALSE;
  deletions = new WXGC_PTRS wxcgList();
  clickbacks = NULL;
}

wxDeleteRecord::~wxDeleteRecord()
{
  if (!undid) {
    wxSnip *snip;
    int i;
    for (i = deletions->Count(); i--; ) {
      snip = (wxSnip *)deletions->Get(i);
      if (snip->flags & wxSNIP_OWNED)
	snip->flags -= wxSNIP_OWNED;
      snip->SetAdmin(NULL);
    }
    if (clickbacks)
      clickbacks->DeleteAll();
  }

  DELETE_OBJ deletions;
  if (clickbacks)
    DELETE_OBJ clickbacks;
}

void wxDeleteRecord::InsertSnip(wxSnip *snip)
{
  deletions->Append(snip);
}

void wxDeleteRecord::AddClickback(wxClickback *click)
{
  if (!clickbacks) {
    clickbacks = new WXGC_PTRS wxcgList();
  }
  clickbacks->Append((wxObject *)click);
}

Bool wxDeleteRecord::Undo(wxMediaBuffer *buffer)
{
  wxSnip *snip;
  wxMediaEdit *media;
  int i, count;
  wxList *toAdd;

  media = (wxMediaEdit *)buffer;

  toAdd = new WXGC_PTRS wxList(wxKEY_NONE, FALSE);
  
  count = deletions->Count();
  for (i = count; i--; ) {
    snip = (wxSnip *)deletions->Get(i);

    /* Have to turn off the owned flag, though we know that it's really ours */
    if (snip->flags & wxSNIP_OWNED)
      snip->flags -= wxSNIP_OWNED;

    toAdd->Append(snip);
  }
  media->Insert(toAdd, start);
  DELETE_OBJ toAdd;

  if (clickbacks) {
    count = clickbacks->Count();
    for (i = 0; i < count; i++) {
      wxClickback *cb;
      cb = (wxClickback *)clickbacks->Get(i);
      media->SetClickback(cb);
    }
  }

  media->SetPosition(startsel, endsel);

  undid = TRUE;

  return continued;
}

#if CGREC_DEBUG
char *wxDeleteRecord::GetName()
{
  return "delete";
}
#endif

class StyleChange /* : public wxObject  */
{
 public:
  long start, end;
  wxStyle *style;
};

wxStyleChangeRecord::wxStyleChangeRecord(long startpos, long endpos, Bool cont, long ss, long es, Bool restoreSel)
{
  continued = cont;
  start = startpos;
  end = endpos;
  startsel = ss;
  endsel = es;
  restoreSelection = restoreSel;

  changes = new WXGC_PTRS wxcgList();
}

wxStyleChangeRecord::~wxStyleChangeRecord()
{
  int i;

  for (i = changes->Count(); i--; ) {
    StyleChange *sc;
    sc = (StyleChange *)changes->Get(i);
  }
}

void wxStyleChangeRecord::AddStyleChange(long start, long end, wxStyle *style)
{
  StyleChange *change;

  change = new WXGC_PTRS StyleChange;

  change->start = start;
  change->end = end;
  change->style = style;

  changes->Append((wxObject *)change);
}

Bool wxStyleChangeRecord::Undo(wxMediaBuffer *buffer)
{
  StyleChange *change;
  long p;
  wxMediaEdit *media;
  int i, count;

  media = (wxMediaEdit *)buffer;
  
  p = start;

  count = changes->Count();
  for (i = 0; i < count; i++) {
    change = (StyleChange *)changes->Get(i);
    media->ChangeStyle(change->style, change->start, change->end);
  }

  if (restoreSelection)
    media->SetPosition(startsel, endsel);

  return continued;
}

#if CGREC_DEBUG
char *wxStyleChangeRecord::GetName()
{
  return "style-change";
}
#endif

class StyleChangeSnip /* : public wxObject  */
{
 public:
  wxSnip *snip;
  wxStyle *style;
};

wxStyleChangeSnipRecord::wxStyleChangeSnipRecord(Bool cont)
{
  continued = cont;

  changes = new WXGC_PTRS wxcgList();
}

wxStyleChangeSnipRecord::~wxStyleChangeSnipRecord()
{
  int i;

  for (i = changes->Count(); i--; ) {
    StyleChange *sc;
    sc = (StyleChange *)changes->Get(i);
  }
  
  DELETE_OBJ changes;
}

void wxStyleChangeSnipRecord::AddStyleChange(wxSnip *snip, wxStyle *style)
{
  StyleChangeSnip *change;

  change = new WXGC_PTRS StyleChangeSnip;

  change->snip = snip;
  change->style = style;

  changes->Append((wxObject *)change);
}

Bool wxStyleChangeSnipRecord::Undo(wxMediaBuffer *buffer)
{
  StyleChangeSnip *change;
  wxMediaPasteboard *media;
  int i, count;

  media = (wxMediaPasteboard *)buffer;
  
  if (!continued)
    media->NoSelected();

  count = changes->Count();
  for (i = 0; i < count; i++) {
    change = (StyleChangeSnip *)changes->Get(i);
    media->ChangeStyle(change->style, change->snip);
    if (!continued)
      media->AddSelected(change->snip);
  }

  return continued;
}

#if CGREC_DEBUG
char *wxStyleChangeSnipRecord::GetName()
{
  return "style-change-snip";
}
#endif

wxMoveSnipRecord::wxMoveSnipRecord(wxSnip *s, double fx, double fy, 
				   Bool d, Bool cont)
{
  snip = s;
  continued = cont;
  x = fx;
  y = fy;
  delta = d;
}

Bool wxMoveSnipRecord::Undo(wxMediaBuffer *buffer)
{
  wxMediaPasteboard *media;

  media = (wxMediaPasteboard *)buffer;
  
  if (delta)
    media->Move(snip, x, y);
  else
    media->MoveTo(snip, x, y);

  return continued;
}

#if CGREC_DEBUG
char *wxMoveSnipRecord::GetName()
{
  return "move-snip";
}
#endif

wxResizeSnipRecord::wxResizeSnipRecord(wxSnip *s, double fx, double fy, 
				       Bool cont)
{
  snip = s;
  continued = cont;
  x = fx;
  y = fy;
}

Bool wxResizeSnipRecord::Undo(wxMediaBuffer *buffer)
{
  wxMediaPasteboard *media;

  media = (wxMediaPasteboard *)buffer;

  media->Resize(snip, x, y);

  return continued;
}

#if CGREC_DEBUG
char *wxResizeSnipRecord::GetName()
{
  return "resize-snip";
}
#endif


wxCompositeRecord::wxCompositeRecord(int _cnt, wxChangeRecordId *_id, int _parity)
{
  cnt = _cnt;
  seq = new WXGC_PTRS wxChangeRecord*[cnt];
  id = _id;
  parity = _parity;
  if (!id) {
    id = new WXGC_PTRS wxChangeRecordId;
  }
  if (parity)
    id->positive = this;
  else
    id->negative = this;
}

wxCompositeRecord::~wxCompositeRecord()
{
  int i = cnt;
  wxChangeRecord *cr;
  while (i--) {
    cr = seq[i];
    DELETE_OBJ cr;
  }
  cnt = 0;
  seq = NULL;
  if (id) {
    if (parity)
      id->positive = NULL;
    else
      id->negative = NULL;
    id = NULL;
  }
}

Bool wxCompositeRecord::Undo(wxMediaBuffer *media)
{
  int i = cnt;
  wxChangeRecord *cr;
  while (i--) {
    cr = seq[i];
    cr->Undo(media);
  }
  return FALSE;
}

void wxCompositeRecord::DropSetUnmodified()
{
  int i = cnt;
  wxChangeRecord *cr;
  while (i--) {
    cr = seq[i];
    cr->DropSetUnmodified();
  }
}

void wxCompositeRecord::AddUndo(int pos, wxChangeRecord *c)
{
  seq[pos] = c;
}

Bool wxCompositeRecord::IsComposite()
{
  return TRUE;
}

wxChangeRecordId *wxCompositeRecord::GetId()
{
  return id;
}

int wxCompositeRecord::GetParity()
{
  return parity;
}

wxChangeRecord *wxCompositeRecord::Inverse()
{
  return new WXGC_PTRS wxInverseRecord(id, !parity);
}

#if CGREC_DEBUG
char *wxCompositeRecord::GetName()
{
  if (cnt == 1) {
    wxChangeRecord *cr;
    cr = seq[0];
    return cr->GetName();
  } else
    return "composite";
}
#endif

wxInverseRecord::wxInverseRecord(wxChangeRecordId *_id, int _parity)
{
  id = _id;
  parity = _parity;
}

wxInverseRecord::~wxInverseRecord()
{
  /* Avoid double-frees by not doing anything */
}

wxChangeRecord *wxInverseRecord::Get()
{
  if (parity)
    return id->positive;
  else
    return id->negative;
}

Bool wxInverseRecord::Undo(wxMediaBuffer *media)
{
  wxChangeRecord *c;
  c = Get();
  return c->Undo(media);
}

void wxInverseRecord::DropSetUnmodified()
{
  wxChangeRecord *c;
  c = Get();
  if (c)
    c->DropSetUnmodified();
}

wxChangeRecordId *wxInverseRecord::GetId()
{
  return id;
}

int wxInverseRecord::GetParity()
{
  return parity;
}

wxChangeRecord *wxInverseRecord::Inverse()
{
  wxChangeRecord *c;
  c = Get();
  return c->Inverse();
}

#if CGREC_DEBUG
char *wxInverseRecord::GetName()
{
  wxChangeRecord *c;
  c = Get();
  if (c)
    return c->GetName();
  else
    return "inverse";
}
#endif

