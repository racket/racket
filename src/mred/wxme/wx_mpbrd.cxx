/*
 * File:        wx_mpbd.cc
 * Purpose:     wxMediaPasteboard implementation
 * Author:      Matthew Flatt
 * Created:     1995
 * Copyright:   (c) 2004-2006 PLT Scheme Inc.
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
#include "wx_dialg.h"
#ifndef OLD_WXWINDOWS
# include "wx_cmdlg.h"
#endif
#include "wx_clipb.h"
#include "wx_utils.h"
#include "wx_media.h"
#include "wx_gcrct.h"
#include "wx_ptreq.h"

#include <string.h>
#include <math.h>

#define LINE_HEIGHT 16

#define DOT_WIDTH 5
#define HALF_DOT_WIDTH 2

static wxCursor *arrow = NULL;

int wxmeGetDoubleClickThreshold();

extern Scheme_Object *objscheme_bundle_wxSnip(class wxSnip *);
  
static wxSnipLocation *DoXSnipLoc(Scheme_Hash_Table *snipLocationList, wxSnip *s)
{
  Scheme_Object *key, *v;

  key = objscheme_bundle_wxSnip(s);
  v = scheme_hash_get(snipLocationList, key);
  return (wxSnipLocation *)v;
}

static void SetSnipLoc(Scheme_Hash_Table *snipLocationList, wxSnip *s, wxSnipLocation *loc)
{
  Scheme_Object *key;

  key = objscheme_bundle_wxSnip(s);
  scheme_hash_set(snipLocationList, key, (Scheme_Object *)loc);
}

#define XSnipLoc(snip) DoXSnipLoc(snipLocationList, snip)
#define SnipLoc(snip) XSnipLoc(snip)

inline Bool Inbox(double lx, double x)
{ 
  return ((lx - HALF_DOT_WIDTH <= x)
	  && (lx - HALF_DOT_WIDTH + DOT_WIDTH >= x));
}

class wxSnipLocation : public wxObject
{
 public:
  double x, y, w, h, r, b, hm, vm;
  double startx, starty;
  Bool selected, needResize;
  wxSnip *snip;

  wxSnipLocation();
  void Resize(wxDC *dc);
};

static wxBrush *blackBrush = NULL, *whiteBrush = NULL, *rbBrush = NULL;
static wxPen *invisiPen = NULL, *rbPen = NULL;

#ifdef wx_mac
extern void wxMediaSetFileCreatorType(char *file, Bool is_binary);
#endif

/**********************************************************************/

wxMediaPasteboard::wxMediaPasteboard()
{
  Scheme_Hash_Table *sll;

  sizeCacheInvalid = TRUE;
  updateNonempty = FALSE;
  noImplicitUpdate = FALSE;
  writeLocked = 0;

  snips = lastSnip = NULL;
  sll = scheme_make_hash_table(SCHEME_hash_ptr);
  snipLocationList = sll;

  sequence = 0;

#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_MEDIA_PASTEBOARD;
#endif
  bufferType = wxPASTEBOARD_BUFFER;

  totalWidth = totalHeight = realWidth = realHeight = 0;

  dragable = TRUE;
  selectionVisible = TRUE;

  sequenceStreak = FALSE;

  dragging = rubberband = FALSE;

  if (!blackBrush) {
    wxREGGLOB(blackBrush);
    wxREGGLOB(whiteBrush);
    wxREGGLOB(invisiPen);
    wxREGGLOB(rbBrush);
    wxREGGLOB(rbPen);
    blackBrush = wxTheBrushList->FindOrCreateBrush("BLACK", wxXOR);
    whiteBrush = wxTheBrushList->FindOrCreateBrush("WHITE", wxSOLID);
    invisiPen = wxThePenList->FindOrCreatePen("BLACK", 1, wxTRANSPARENT);
    rbBrush = wxTheBrushList->FindOrCreateBrush("BLACK", wxTRANSPARENT);
    rbPen = wxThePenList->FindOrCreatePen("BLACK", 1, wxXOR_DOT);
  }

  {
    wxStandardSnipAdmin *ssa;
    ssa = new WXGC_PTRS wxStandardSnipAdmin(this);
    snipAdmin = ssa;
  }

  needResize = FALSE;

  keepSize = FALSE;

  scrollStep = LINE_HEIGHT;

  maxWidth = minWidth = minHeight = maxHeight = 0.0;
}

wxMediaPasteboard::~wxMediaPasteboard()
{
  wxSnip *snip, *next;
  for (snip = snips; snip; snip = next) {
    next = snip->next;
    DELETE_OBJ snip;
  }

  DELETE_OBJ snipAdmin;
}

void wxMediaPasteboard::RubberBand(double x, double y, double w, double h)
{
  wxPen *oldPen;
  wxBrush *oldBrush;
  wxDC *dc;
  double vx, vy, vw, vh, b, r, dx, dy;

  if (!admin)
    return;

  if (!w & !h)
    return;

  if (w < 0) {
    x += w;
    w = -w;
  }
  r = x + w;
  if (h < 0) {
    y += h;
    h = -h;
  }
  b = y + h;

  admin->GetView(&vx, &vy, &vw, &vh);

  if (x < vx)
    x = vx;
  if (y < vy)
    y = vy;
  if (r > vx + vw)
    r = vx + vw;
  if (b > vy + vh)
    b = vy + vh;

  if (x >= r || y >= b)
    return;

  dc = admin->GetDC(&dx, &dy);
  
  oldPen = dc->GetPen();
  oldBrush = dc->GetBrush();
  dc->SetPen(rbPen);
  dc->SetBrush(rbBrush);
  
  dc->DrawRectangle(x - dx, y - dy, 
		    r - x + GC_RECT_BRUSH_EXTEND, 
		    b - y + GC_RECT_BRUSH_EXTEND);
  
  dc->SetPen(oldPen);
  dc->SetBrush(oldBrush);;
}

wxCursor *wxMediaPasteboard::AdjustCursor(wxMouseEvent *event)
{
  double scrollx, scrolly;
  double x, y;
  wxSnip *snip;
  wxDC *dc;
  wxCursor *c;

  if (!admin)
    return NULL;

  dc = admin->GetDC(&scrollx, &scrolly);
  if (!dc)
    return NULL;

  x = event->x + scrollx;
  y = event->y + scrolly;

  if (!customCursorOverrides) {

    if (caretSnip && event->Dragging()) {
      double x, y;
      GetSnipLocation(caretSnip, &x, &y);
      c = caretSnip->AdjustCursor(dc, x - scrollx, y - scrolly, x, y, event);
      if (c)
	return c;
    }
    
    snip = FindSnip(x, y);
    
    if (snip && (snip == caretSnip)) {
      double x, y;
      GetSnipLocation(caretSnip, &x, &y);
      c = snip->AdjustCursor(dc, x - scrollx, y - scrolly, x, y, event);
      if (c)
	return c;
    }
  }
  
  if (customCursor)
    return customCursor;

  if (!arrow) {
    wxREGGLOB(arrow);
    arrow = new WXGC_PTRS wxCursor(wxCURSOR_ARROW);
  }

  return arrow;
}

void wxMediaPasteboard::OnEvent(wxMouseEvent *event)
{
  double x, y, scrollx, scrolly;
  wxSnip *snip;
  wxSnipLocation *loc;
  wxDC *dc;

  if (!admin)
    return;

  if (event->ButtonDown() || caretSnip) {
    /* First, find clicked-on snip: */
    x = event->x;
    y = event->y;
    
    dc = admin->GetDC(&scrollx, &scrolly);
    y += scrolly;
    x += scrollx;
  } else {
    x = y = 0;
    dc = NULL;
  }

  if (event->ButtonDown())
    snip = FindSnip(x, y);
  else
    snip = caretSnip;

  if (caretSnip && PTREQ(snip, caretSnip)) {
    loc = SnipLoc(caretSnip);
    caretSnip->OnEvent(dc, loc->x - scrollx, loc->y - scrolly, loc->x, loc->y, event);
    return;
  }

  OnLocalEvent(event);
}

void wxMediaPasteboard::OnDefaultEvent(wxMouseEvent *event)
{
  double x, y, scrollx, scrolly;
  wxSnip *snip;
  wxSnipLocation *loc;
  wxDC *dc;
  Bool click;

  if (!admin)
    return;

  /* First, find clicked-on snip: */
  x = event->x;
  y = event->y;
  
  dc = admin->GetDC(&scrollx, &scrolly);
  y += scrolly;
  x += scrollx;

  InteractiveAdjustMouse(&x, &y);

  if (event->ButtonDown() 
      || (event->Moving() && !event->Dragging())
      || event->ButtonUp()) {
    Bool update = FALSE;

    keepSize = FALSE;
    if (dragging) {
      if (resizing) {
	BeginEditSequence();
	/* Move & resize back without Undo */
	if (sizedxm < 0 || sizedym < 0)
	  MoveTo(resizing, origX, origY);
	Resize(resizing, origW, origH);
	dragging = FALSE;
	/* Re-move and re-size with undo: */
	DoEventResize(lastX, lastY);
	AfterInteractiveResize(resizing);
	EndEditSequence();
	resizing = NULL;
      } else {
	FinishDragging(event);
      }
    }
    if (rubberband) {
      rubberband = FALSE;
      RubberBand(startX, startY, lastX - startX, lastY - startY);
      AddSelected(startX, startY, lastX - startX, lastY - startY);
      update = TRUE;
    }

    if (update) {
      UpdateAll();
    }
  }

  click = FALSE;
  if (event->ButtonDown())
    click = TRUE;
  if (event->Dragging() && !dragging && !rubberband)
    click = TRUE;

  if (click) {
    snip = FindSnip(x, y);

    if (dragable) {
      if (snip) {
	loc = SnipLoc(snip);
	origX = loc->x;
	origY = loc->y;
	origW = loc->w;
	origH = loc->h;
	if (!loc->selected) {
	  if (!event->shiftDown)
	    NoSelected();
	  SetCaretOwner(NULL);
	  AddSelected(snip);
	  InitDragging(event);
	} else {
	  long interval;

	  interval = event->timeStamp - lastTime;
	  if (interval < 0)
	    interval = -interval;
	  if (event->ButtonDown() && (interval < (map ? map->GetDoubleClickInterval() : wxmeGetDoubleClickThreshold())))
	    OnDoubleClick(snip, event);
	  else {
	    if (FindDot(loc, x, y, &sizedxm, &sizedym))
	      resizing = snip;
	    InitDragging(event);
	  }
	}
        if (event->ButtonDown())
	  lastTime = event->timeStamp;
      } else {
	if (!event->shiftDown)
	  NoSelected();
	SetCaretOwner(NULL);
	rubberband = TRUE;
      }
      startX = lastX = x;
      startY = lastY = y;
    } else
      SetCaretOwner(snip);

    return;
  }

  if (dragable) {
    if (event->Dragging()) {
      if (rubberband) {
	/* Erase old */
	RubberBand(startX, startY, lastX - startX, lastY - startY);
	/* Draw new: */
	RubberBand(startX, startY, x - startX, y - startY);
      } else {
	if (resizing)
	  DoEventResize(x, y);
	else {
	  DoEventMove(x, y);
	}
      }
      lastX = x;
      lastY = y;
    }
  }
}

void wxMediaPasteboard::OnDoubleClick(wxSnip *snip, wxMouseEvent *)
{
  if (snip->flags & wxSNIP_HANDLES_EVENTS) {
    NoSelected();
    SetCaretOwner(snip);
  }
}

void wxMediaPasteboard::OnChar(wxKeyEvent *event)
{
  double x, y, scrollx, scrolly;
  wxSnipLocation *loc;
  wxDC *dc;

  if (!admin)
    return;
  
  x = event->x;
  y = event->y;

  dc = admin->GetDC(&scrollx, &scrolly);
  y += scrolly;
  x += scrollx;

  if (caretSnip) {
    loc = SnipLoc(caretSnip);
    caretSnip->OnChar(dc, loc->x, loc->y, x - scrollx, y - scrolly, event);
    return;
  }

  OnLocalChar(event);
}

void wxMediaPasteboard::OnDefaultChar(wxKeyEvent *event)
{
  long code;

  if (!admin)
    return;

  code = event->KeyCode();

  switch(code) {
    case WXK_BACK:
    case WXK_DELETE:
      Delete();
      break;
    case WXK_RIGHT:
      Move(1, 0);
      break;
    case WXK_LEFT:
      Move(-1, 0);
      break;
    case WXK_UP:
      Move(0, -1);
      break;
    case WXK_DOWN:
      Move(0, 1);
      break;
    }
}

void wxMediaPasteboard::InitDragging(wxMouseEvent *e)
{
  wxSnip *s = NULL;

  if (resizing) {
    if (!CanInteractiveResize(resizing)) {
      resizing = NULL;
      return;
    }
    OnInteractiveResize(resizing);
  } else {
    if (!CanInteractiveMove(e))
      return;
    OnInteractiveMove(e);
  }

  dragging = TRUE;
  keepSize = TRUE;

  while ((s = FindNextSelectedSnip(s))) {
    wxSnipLocation *loc;
    loc = SnipLoc(s);
    loc->startx = loc->x;
    loc->starty = loc->y;
  }
}

void wxMediaPasteboard::FinishDragging(wxMouseEvent *e)
{
  wxSnip *s = NULL;

  BeginEditSequence();
  /* Move back without Undo and remember final */
  while ((s = FindNextSelectedSnip(s))) {
    double x, y;
    wxSnipLocation *loc;
    loc = SnipLoc(s);
    x = loc->startx;
    y = loc->starty;
    loc->startx = loc->x;
    loc->starty = loc->y;
    MoveTo(s, x, y);
  }
  dragging = FALSE;
  /* Move to final position with undo: */
  s = NULL;
  while ((s = FindNextSelectedSnip(s))) {
    wxSnipLocation *loc;
    loc = SnipLoc(s);
    MoveTo(s, loc->startx, loc->starty);
  }

  AfterInteractiveMove(e);

  EndEditSequence();
}

void wxMediaPasteboard::DoEventMove(double eventX, double eventY)
{
  wxSnip *s = NULL;
  double dx, dy;
  
  dx = eventX - startX;
  dy = eventY - startY;

  BeginEditSequence();

  while ((s = FindNextSelectedSnip(s))) {
    wxSnipLocation *loc;
    double x, y;

    loc = SnipLoc(s);
    x = loc->startx + dx;
    y = loc->starty + dy;
    InteractiveAdjustMove(s, &x, &y);
    MoveTo(s, x, y);
  }

  EndEditSequence();
}

void wxMediaPasteboard::DoEventResize(double eventX, double eventY)
{
  double Dx, Dy, w, h, x, y;
  
  Dx = eventX - startX;
  Dy = eventY - startY;

  w = origW + Dx * sizedxm;
  h = origH + Dy * sizedym;
  
  if (w < 0)
    w = 0;
  if (h < 0)
    h = 0;

  InteractiveAdjustResize(resizing, &w, &h);
  
  if (w < 0)
    w = 0;
  if (h < 0)
    h = 0;
  
  x = origX;
  if (sizedxm < 0) {
    x += (origW - w);
  }
  y = origY;
  if (sizedym < 0) {
    y += (origH - h);
  }
  
  BeginEditSequence();
  
  if (Resize(resizing, w, h)) {
    if ((sizedxm < 0 || sizedym < 0))
      MoveTo(resizing, x, y);
  }
  
  EndEditSequence();
}

void wxMediaPasteboard::InteractiveAdjustMouse(double *x, double *y)
{
  if (*x < 0)
    *x = 0;
  if (*y < 0)
    *y = 0;
}

void wxMediaPasteboard::InteractiveAdjustResize(wxSnip *, double *, double *)
{
  /* Do nothing */
}

void wxMediaPasteboard::InteractiveAdjustMove(wxSnip *, double *x, double *y)
{
  if (*x < 0)
    *x = 0;
  if (*y < 0)
    *y = 0;
}


/***************************************************************************/

void wxMediaPasteboard::SetSelected(wxSnip *snip)
{
  BeginEditSequence();
  NoSelected();
  AddSelected(snip);
  EndEditSequence();
}

void wxMediaPasteboard::DoSelect(wxSnip *snip, Bool on)
{
  wxSnipLocation *loc;

  if ((loc = XSnipLoc(snip))) {
    if (loc->selected != on) {
      writeLocked++;
      if (CanSelect(snip, on)) {
	OnSelect(snip, on);
	--writeLocked;
	loc->selected = on;
	AfterSelect(snip, on);
	UpdateLocation(loc);
      } else
	--writeLocked;
    }
  }
}

void wxMediaPasteboard::AddSelected(wxSnip *snip)
{
  DoSelect(snip, TRUE);
}

void wxMediaPasteboard::RemoveSelected(wxSnip *snip)
{
  DoSelect(snip, FALSE);
}

void wxMediaPasteboard::AddSelected(double x, double y, double w, double h)
{
  wxSnip *s;
  double r, b;

  if (w < 0) {
    x += w;
    w = -w;
  }
  if (h < 0) {
    y += h;
    h = -h;
  }

  r = x + w;
  b = y + h;

  BeginEditSequence();

  for (s = snips; s; s = s->next) {
    wxSnipLocation *loc;
    loc = SnipLoc(s);
    if (loc
	&& !loc->selected
	&& (loc->x <= r)
	&& (loc->y <= b)
	&& (loc->r >= x)
	&& (loc->b >= y)) {
      AddSelected(s);
    }
  }
  
  EndEditSequence();
}
				  
void wxMediaPasteboard::SelectAll(void)
{
  wxSnip *s;

  BeginEditSequence();

  for (s = snips; s; s = s->next) {
    AddSelected(s);
  }
  
  EndEditSequence();
}
				  
void wxMediaPasteboard::NoSelected()
{
  wxSnip *s;

  BeginEditSequence();

  for (s = snips; s; s = s->next) {
    RemoveSelected(s);
  }
  
  EndEditSequence();
}

void wxMediaPasteboard::Insert(wxSnip *snip, wxSnip *before, double x, double y)
{
  wxSnipLocation *loc;
  wxSnip *search;

  if (userLocked || writeLocked)
    return;

  if (snip->IsOwned())
    return;

  if (!snip->snipclass)
    wxmeError("insert in pasteboard%: cannot insert a snip without a snipclass");

  writeLocked++;
  BeginEditSequence();
  if (!CanInsert(snip, before, x, y)) {
    EndEditSequence();
    --writeLocked;
    return;
  }
  OnInsert(snip, before, x, y);
  --writeLocked;

  if (snip->IsOwned()) {
    /* Disaster: Can/OnInsert made the snip owned. */
    snip = new WXGC_PTRS wxImageSnip();
  }

  for (search = snips; search && (search != before); search = search->next) {
  }
  
  snip->next = search;
  if (snip->next) {
    snip->prev = search->prev;
    snip->next->prev = snip;
  } else {
    snip->prev = lastSnip;
    lastSnip = snip;
  }
  if (snip->prev)
    snip->prev->next = snip;
  else
    snips = snip;

  loc = new WXGC_PTRS wxSnipLocation;
  loc->x = x;
  loc->y = y;
  loc->snip = snip;
  loc->needResize = TRUE;
  loc->selected = FALSE;
  SetSnipLoc(snipLocationList, snip, loc);

  snip->style = styleList->Convert(snip->style);
  if (PTREQ(snip->style, styleList->BasicStyle())) {
    wxStyle *s;
    s = GetDefaultStyle();
    if (s)
      snip->style = s;
  }

  snip->SizeCacheInvalid();

  SnipSetAdmin(snip, snipAdmin);

  if (!noundomode) {
    wxInsertSnipRecord *is;
    is = new WXGC_PTRS wxInsertSnipRecord(snip, sequenceStreak);
    AddUndo(is);
  }
  if (sequence)
    sequenceStreak = TRUE;

  changed = TRUE;

  if (!modified)
    SetModified(TRUE);

  needResize = TRUE;
  UpdateLocation(loc);

  writeLocked++;
  EndEditSequence();
  --writeLocked;

  if (!sequence)
    UpdateNeeded();

  AfterInsert(snip, before, x, y);
}

void wxMediaPasteboard::Insert(wxSnip *snip, double x, double y)
{
  Insert(snip, snips, x, y);
}

void wxMediaPasteboard::Insert(wxSnip *snip)
{
  double x, y;

  GetCenter(&x, &y);
  Insert(snip, x, y);
}

void wxMediaPasteboard::Insert(wxSnip *snip, wxSnip *before)
{
  double x, y;

  GetCenter(&x, &y);
  Insert(snip, before, x, y);
}

void wxMediaPasteboard::Delete()
{
  int i;
  wxSnipLocation *loc;
  wxDeleteSnipRecord *del;

  if (userLocked || writeLocked)
    return;

  del = new WXGC_PTRS wxDeleteSnipRecord(sequenceStreak);
  if (sequence)
    sequenceStreak = TRUE;

  BeginEditSequence();

  for (i = 0; i < snipLocationList->size; i++) {
    loc = (wxSnipLocation *)snipLocationList->vals[i];
    if (loc) {
      if (loc->selected) 
	_Delete(loc->snip, del);
    }
  }

  if (!noundomode)
    AddUndo(del);

  EndEditSequence();
}

void wxMediaPasteboard::Erase()
{
  wxSnip *snip, *next;
  wxDeleteSnipRecord *del;

  if (userLocked || writeLocked)
    return;

  del = new WXGC_PTRS wxDeleteSnipRecord(sequenceStreak);
  if (sequence)
    sequenceStreak = TRUE;

  BeginEditSequence();
  for (snip = snips; snip; snip = next) {
    next = snip->next;
    _Delete(snip, del);
  }

  if (!noundomode)
    AddUndo(del);

  EndEditSequence();
}

Bool wxMediaPasteboard::_Delete(wxSnip *del_snip,
				wxDeleteSnipRecord *del)
{
  wxSnip *snip;
  wxSnipLocation *loc;
  Bool updateCursor = FALSE;
  Bool result = FALSE;

  for (snip = snips; snip; snip = snip->next) {
    if (PTREQ(snip, del_snip)) {
      writeLocked++;
      BeginEditSequence();
      if (!CanDelete(del_snip)) {
	EndEditSequence();
	--writeLocked;
	return FALSE;
      }
      OnDelete(del_snip);
      --writeLocked;

      if (del_snip == caretSnip) {
	caretSnip->OwnCaret(FALSE);
	caretSnip = NULL;
	updateCursor = TRUE;
      }

      UpdateSnip(del_snip);

      if (!snip->prev)
	snips = snip->next;
      else
	snip->prev->next = snip->next;
      if (!snip->next)
	lastSnip = snip->prev;
      else
	snip->next->prev = snip->prev;

      loc = DoXSnipLoc(snipLocationList, snip);
      SetSnipLoc(snipLocationList, snip, NULL);
      if (del)
	del->InsertSnip(snip, snip->next, loc->x, loc->y);
      snip->next = snip->prev = NULL;

      snip->flags += wxSNIP_CAN_DISOWN;
      SnipSetAdmin(snip, NULL);
      snip->flags -= wxSNIP_CAN_DISOWN;

      if (!modified)
	SetModified(TRUE);

      AfterDelete(del_snip);
      changed = TRUE;

      needResize = TRUE;

      writeLocked++;
      EndEditSequence();
      --writeLocked;

      if (!sequence)
	UpdateNeeded();

      result = TRUE;
    }
  }  

  if (updateCursor)
    if (admin)
      admin->UpdateCursor();

  return result;
}

void wxMediaPasteboard::Delete(wxSnip *del_snip)
{
  wxDeleteSnipRecord *del;

  if (userLocked || writeLocked)
    return;

  del = new WXGC_PTRS wxDeleteSnipRecord(sequenceStreak);
  if (sequence)
    sequenceStreak = TRUE;

  _Delete(del_snip, del);

  if (!noundomode)
    AddUndo(del);
}

void wxMediaPasteboard::Remove(wxSnip *del_snip)
{
  if (userLocked || writeLocked)
    return;

  _Delete(del_snip, NULL);
}

void wxMediaPasteboard::MoveTo(wxSnip *snip, double x, double y)
{
  wxSnipLocation *loc;
  wxMoveSnipRecord *rec;

  if (userLocked || writeLocked)
    return;

  loc = DoXSnipLoc(snipLocationList, snip);

  if (loc) {
    if ((loc->x == x) && (loc->y == y))
      return;

    writeLocked++;
    BeginEditSequence();
    if (!CanMoveTo(snip, x, y, dragging)) {
      EndEditSequence();
      --writeLocked;
      return;
    }
    OnMoveTo(snip, x, y, dragging);
    --writeLocked;

    UpdateLocation(loc);

    if (!dragging) {
      rec = new WXGC_PTRS wxMoveSnipRecord(loc->snip, loc->x, loc->y, 
				 FALSE, sequenceStreak);
      if (sequence)
	sequenceStreak = TRUE;
      if (!noundomode)
	AddUndo(rec);
    }

    loc->x = x;
    loc->y = y;
    loc->r = x + loc->w;
    loc->b = y + loc->h;
    loc->hm = x + loc->w/2;
    loc->vm = y + loc->h/2;
    UpdateLocation(loc);

    if (!dragging && !modified)
	SetModified(TRUE);

    AfterMoveTo(snip, x, y, dragging);

    needResize = TRUE;

    writeLocked++;
    EndEditSequence();
    --writeLocked;

    changed = TRUE;

    if (!sequence)
      UpdateNeeded();
  }
}

void wxMediaPasteboard::Move(wxSnip *snip, double dx, double dy)
{
  wxSnipLocation *loc;

  if (userLocked || writeLocked)
    return;

  loc = DoXSnipLoc(snipLocationList, snip);
  if (loc) {
    MoveTo(snip, loc->x + dx, loc->y + dy);
  }
}

void wxMediaPasteboard::Move(double dx, double dy)
{
  int i;
  wxSnipLocation *loc;

  if (userLocked || writeLocked)
    return;

  BeginEditSequence();

  for (i = 0; i < snipLocationList->size; i++) {
    loc = (wxSnipLocation *)snipLocationList->vals[i];
    if (loc) {
      if (loc->selected)
	Move(loc->snip, dx, dy);
    }
  }

  EndEditSequence();
}

Bool wxMediaPasteboard::Resize(wxSnip *snip, double w, double h)
{
  wxSnipLocation *loc;
  double oldw, oldh;
  Bool rv;
  
  if (!admin)
    return FALSE;

  loc = DoXSnipLoc(snipLocationList, snip);
  if (!loc)
    return FALSE;

  oldw = loc->w;
  oldh = loc->h;

  writeLocked++;
  BeginEditSequence();
  if (!CanResize(snip, w, h)) {
    EndEditSequence();
    --writeLocked;
    return FALSE;
  }
  OnResize(snip, w, h);
  --writeLocked;

  if (!snip->Resize(w, h))
    rv = FALSE;
  else {
    if (!dragging) {
      if (!noundomode) {
	wxResizeSnipRecord *rs;
	rs = new WXGC_PTRS wxResizeSnipRecord(snip, oldw, oldh, sequenceStreak);
	AddUndo(rs);
      }
      if (sequence)
	sequenceStreak = TRUE;
    }
    rv = TRUE;
  }

  if (rv && !dragging && !modified)
    SetModified(TRUE);

  AfterResize(snip, w, h, rv);

  writeLocked++;
  EndEditSequence();
  --writeLocked;

  changed = TRUE;

  if (!sequence)
    UpdateNeeded();

  return rv;
}

void wxMediaPasteboard::ChangeStyle(wxStyleDelta *delta)
{ 
  ChangeStyle(delta, NULL);
}

void wxMediaPasteboard::ChangeStyle(wxStyle *style, wxSnip *snip)
{
  _ChangeStyle(style, NULL, snip);
}

void wxMediaPasteboard::ChangeStyle(wxStyleDelta *delta, wxSnip *snip)
{
  _ChangeStyle(NULL, delta, snip);
}
 
void wxMediaPasteboard::_ChangeStyle(wxStyle *style, wxStyleDelta *delta, 
				     wxSnip *snip)
{
  wxSnipLocation *loc;
  wxStyleChangeSnipRecord *rec;
  Bool didit = FALSE;

  if (userLocked || writeLocked)
    return;

  rec = new WXGC_PTRS wxStyleChangeSnipRecord(sequenceStreak);
  if (sequence)
    sequenceStreak = TRUE;

  if (!style && !delta) {
    style = GetDefaultStyle();
    if (!style)
      style = styleList->BasicStyle();
  }

  BeginEditSequence();

  if (snip) {
    rec->AddStyleChange(snip, snip->style);
    if (style)
      snip->style = style;
    else {
      snip->style = styleList->FindOrCreateStyle(snip->style, delta);
    }
    snip->SizeCacheInvalid();
    UpdateSnip(snip);
    didit = TRUE;
  } else {
    int i;
    for (i = 0; i < snipLocationList->size; i++) {
      loc = (wxSnipLocation *)snipLocationList->vals[i];
      if (loc) {
	if (loc->selected) {
	  rec->AddStyleChange(loc->snip, loc->snip->style);
	  if (style)
	    loc->snip->style = style;
	  else {
	    loc->snip->style = styleList->FindOrCreateStyle(loc->snip->style, 
							    delta);
	  }
	  loc->snip->SizeCacheInvalid();
	  loc->needResize = TRUE;
	  needResize = TRUE;
	  UpdateLocation(loc);
	  didit = TRUE;
	}
      }
    }
  }
  
  if (didit) {
    if (!noundomode)
      AddUndo(rec);

    changed = TRUE;
    if (!modified)
      SetModified(TRUE);
  }

  EndEditSequence();
}

void wxMediaPasteboard::Raise(wxSnip *snip)
{
  SetBefore(snip, snip->prev);
}

void wxMediaPasteboard::Lower(wxSnip *snip)
{
  SetAfter(snip, snip->next);
}

void wxMediaPasteboard::SetBefore(wxSnip *snip, wxSnip *before)
{
  if (userLocked || writeLocked)
    return;

  if (!before)
    before = snips;

  if (!DoXSnipLoc(snipLocationList, snip)
      || !DoXSnipLoc(snipLocationList, before))
    return;

  if (snip == before)
    return;

  writeLocked++;
  if (!CanReorder(snip, before, TRUE)) {
    --writeLocked;
    return;
  }
  OnReorder(snip, before, TRUE);
  --writeLocked;

  /* Remove snip from current pos: */
  if (snip->prev)
    snip->prev->next = snip->next;
  else
    snips = snip->next;
  if (snip->next)
    snip->next->prev = snip->prev;
  else
    lastSnip = snip->prev;

  /* Insert before `before': */
  snip->prev = before->prev;
  snip->next = before;
  before->prev = snip;
  if (snip->prev)
    snip->prev->next = snip;
  else
    snips = snip;

  changed = TRUE;
  if (!modified)
    SetModified(TRUE);

  UpdateSnip(snip);

  AfterReorder(snip, before, TRUE);
}

void wxMediaPasteboard::SetAfter(wxSnip *snip, wxSnip *after)
{
  if (userLocked || writeLocked)
    return;

  if (!after)
    after = lastSnip;

  if (!DoXSnipLoc(snipLocationList, snip)
      || !DoXSnipLoc(snipLocationList, after))
    return;

  if (snip == after)
    return;

  writeLocked++;
  if (!CanReorder(snip, after, FALSE)) {
    --writeLocked;
    return;
  }    
  OnReorder(snip, after, FALSE);
  --writeLocked;
 
  /* Remove snip from current pos: */
  if (snip->prev)
    snip->prev->next = snip->next;
  else
    snips = snip->next;
  if (snip->next)
    snip->next->prev = snip->prev;
  else
    lastSnip = snip->prev;

  /* Insert after `after': */
  snip->next = after->next;
  snip->prev = after;
  after->next = snip;
  if (snip->next)
    snip->next->prev = snip;
  else
    lastSnip = snip;

  changed = TRUE;
  if (!modified)
    SetModified(TRUE);

  UpdateSnip(snip);

  AfterReorder(snip, after, FALSE);
}

wxSnip *wxMediaPasteboard::SnipSetAdmin(wxSnip *snip, wxSnipAdmin *a)
{
  wxSnipAdmin *orig_admin;
  orig_admin = snip->GetAdmin();

  /* Lock during SetAdmin! */
  snip->SetAdmin(a);

  if (snip->GetAdmin() != a) {
    /* Something went wrong. */
    if (!a && (snip->GetAdmin() == orig_admin)) {
      /* Force admin to NULL. */
      snip->wxSnip::SetAdmin(NULL);
    } else if (a) {
      /* Snip didn't accept membership into this buffer. Give up on it. */
      wxSnip *naya;
      naya = new WXGC_PTRS wxSnip();
      naya->prev = snip->prev;
      naya->next = snip->next;
      if (naya->prev)
	naya->prev->next = naya;
      else
	snips = naya;
      if (naya->next)
	naya->next->prev = naya;
      else
	lastSnip = naya;

      snip->wxSnip::SetAdmin(NULL);

      naya->SetAdmin(a);
      snip = naya;
    }
  }

  return snip;
}

Bool wxMediaPasteboard::ReallyCanEdit(int op)
{
  if (op != wxEDIT_COPY) {
    if (writeLocked)
      return FALSE;
  }

  switch(op) {
  case wxEDIT_CLEAR:
  case wxEDIT_CUT:
  case wxEDIT_COPY:
  case wxEDIT_KILL:
    if (!FindNextSelectedSnip(NULL))
      return FALSE;
    break;
  case wxEDIT_SELECT_ALL:
    if (!snips)
      return FALSE;
  }

  return TRUE;
}

/***************************************************************************/

Bool wxMediaPasteboard::FindDot(wxSnipLocation *loc, double x, double y,
				double *dxm, double *dym)
{
  if (Inbox(loc->x, x)) {
    *dxm = -1;
    if (Inbox(loc->y, y))
      *dym = -1;
    else if (Inbox(loc->vm, y))
      *dym = 0;
    else if (Inbox(loc->b, y))
      *dym = 1;
    else
      return FALSE;
  } else if (Inbox(loc->hm, x)) {
    *dxm = 0;
    if (Inbox(loc->y, y))
      *dym = -1;
    else if (Inbox(loc->b, y))
      *dym = 1;
    else
      return FALSE;
  } else if (Inbox(loc->r, x)) {
    *dxm = 1;
    if (Inbox(loc->y, y))
      *dym = -1;
    else if (Inbox(loc->vm, y))
      *dym = 0;
    else if (Inbox(loc->b, y))
      *dym = 1;
    else
      return FALSE;
  } else
    return FALSE;

  return TRUE;
}

wxSnip *wxMediaPasteboard::FindSnip(double x, double y, wxSnip *after)
{
  wxSnip *snip;
  wxSnipLocation *loc;
  double dym, dxm;

  for (snip = snips; snip; snip = snip->next) {
    if (after) {
      if (after == snip)
	after = NULL;
    } else {
      loc = SnipLoc(snip);
      if (loc->x <= x && loc->y <= y
	  && loc->r >= x && loc->b >= y)
	return snip;
      else if (loc->selected && FindDot(loc, x, y, &dxm, &dym))
	return snip;
    }
  }

  return NULL;
}

wxSnip *wxMediaPasteboard::FindFirstSnip(void)
{
  return snips;
}

Bool wxMediaPasteboard::IsSelected(wxSnip *asnip)
{
  wxSnip *snip;
  wxSnipLocation *loc;

  for (snip = snips; snip; snip = snip->next) {
    if (PTREQ(asnip, snip)) {
      loc = SnipLoc(snip);
      return loc->selected;
    }
  }

  return FALSE;
}

wxSnip *wxMediaPasteboard::FindNextSelectedSnip(wxSnip *start)
{
  wxSnip *snip;
  wxSnipLocation *loc;

  if (!start)
    snip = snips;
  else {
    loc = XSnipLoc(start);
    if (!loc) return NULL; /* Not in this pasteboard */
    snip = start->next;
  }

  for (; snip; snip = snip->next) {
    loc = SnipLoc(snip);
    if (loc->selected)
      return snip;
  }

  return NULL;
}

/***************************************************************************/

void wxMediaPasteboard::Draw(wxDC *dc, double dx, double dy, 
			     double cx, double cy, double cw, double ch, 
			     int show_caret, wxColour *bgColor)
{
  wxSnip *snip;
  wxStyle *oldstyle = NULL;
  wxSnipLocation *loc;
  double cr, cb, x, y, r, b, hm, vm, dcx, dcy, dcr, dcb;

  if (!admin)
    return;

  writeLocked++;
  flowLocked = TRUE;

  dcx = cx + dx;
  dcy = cy + dy;

  cr = cx + cw;
  cb = cy + ch;

  dcr = dcx + cw;
  dcb = dcy + ch;

  if (bgColor) {
    wxPen *savePen;
    wxBrush *saveBrush, *wb;

    savePen = dc->GetPen();
    saveBrush = dc->GetBrush();

    if (bgColor == wxWHITE)
      wb = whiteBrush;
    else
      wb = wxTheBrushList->FindOrCreateBrush("WHITE", wxSOLID);
    dc->SetBrush(wb);
    dc->SetPen(invisiPen);
    dc->DrawRectangle(dcx, dcy,
		      cw + GC_RECT_BRUSH_EXTEND,
		      ch + GC_RECT_BRUSH_EXTEND);

    dc->SetBrush(saveBrush);
    dc->SetPen(savePen);
  }

  OnPaint(TRUE, dc, cx, cy, cr, cb, dx, dy, 
	  (show_caret && !caretSnip)
	  ? show_caret
	  : (int)wxSNIP_DRAW_NO_CARET);

  for (snip = lastSnip; snip; snip = snip->prev) {
    loc = SnipLoc(snip);

    if ((loc->x <= cr)
	&& (loc->y <= cb)
	&& (loc->r >= cx)
	&& (loc->b >= cy)) {
      snip->style->SwitchTo(dc, oldstyle);
      oldstyle = snip->style;

      x = loc->x + dx;
      y = loc->y + dy;
      
      snip->Draw(dc, x, y, dcx, dcy, dcr, dcb, dx, dy, 
		 PTREQ(snip, caretSnip) 
		 ? (show_caret ? show_caret : (int)wxSNIP_DRAW_NO_CARET)
		 : (int)wxSNIP_DRAW_NO_CARET);

      if ((show_caret == wxSNIP_DRAW_SHOW_CARET)
	  && ownCaret 
	  && selectionVisible
	  && loc->selected) {
	wxBrush *oldbrush;
	wxPen *oldpen;

	oldbrush = dc->GetBrush();
	oldpen = dc->GetPen();
	dc->SetBrush(blackBrush);
	dc->SetPen(invisiPen);

	r = loc->r + dx;
	b = loc->b + dy;
	hm = loc->hm + dx;
	vm = loc->vm + dy;

	dc->DrawRectangle(x - HALF_DOT_WIDTH, 
			  y - HALF_DOT_WIDTH, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND);
	dc->DrawRectangle(hm - HALF_DOT_WIDTH, 
			  y - HALF_DOT_WIDTH, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND);
	dc->DrawRectangle(r - HALF_DOT_WIDTH, 
			  y - HALF_DOT_WIDTH, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND);
	dc->DrawRectangle(r - HALF_DOT_WIDTH, 
			  vm - HALF_DOT_WIDTH, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND);
	dc->DrawRectangle(r - HALF_DOT_WIDTH, 
			  b - HALF_DOT_WIDTH, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND,
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND);
	dc->DrawRectangle(hm - HALF_DOT_WIDTH, 
			  b - HALF_DOT_WIDTH, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND);
	dc->DrawRectangle(x - HALF_DOT_WIDTH, 
			  b - HALF_DOT_WIDTH, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND);
	dc->DrawRectangle(x - HALF_DOT_WIDTH, 
			  vm - HALF_DOT_WIDTH, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND);

	dc->SetPen(oldpen);
	dc->SetBrush(oldbrush);
      }
    }
  }

  {
    wxStyle *bs;
    bs = styleList->BasicStyle();
    bs->SwitchTo(dc, oldstyle);
  }

  OnPaint(FALSE, dc, cx, cy, cr, cb, dx, dy, 
	  (show_caret && !caretSnip)
	  ? show_caret
	  : (int)wxSNIP_DRAW_NO_CARET);

  flowLocked = FALSE;
  --writeLocked;
}

void wxMediaPasteboard::Refresh(double localx, double localy, double w, double h, 
				int show_caret, wxColour *bgColor)
{
  double dx, dy, right, bottom;
  wxDC *dc;

  if (!admin)
    return;

  if ((h <= 0) || (w <= 0))
    return;

  if (flowLocked || sequence) {
    /* We're busy. Invalidate so that everything is refreshed later. */
    Update(localx, localy, w, h);
    return;
  }

  BeginSequenceLock();

  ReadyOffscreen(w, h);

  dc = admin->GetDC(&dx, &dy);

  /* Make sure all location information is integral,
     so we can shift the coordinate system and generally
     update on pixel boundaries. */
  dx = floor(dx);
  dy = floor(dy);
  bottom = ceil(localy + h);
  right = ceil(localx + w);
  localy = floor(localy);
  localx = floor(localx);
  w = right - localx;
  h = bottom - localy;

  if (!offscreenInUse && bitmap && bitmap->Ok() && offscreen->Ok()
      && bgColor) {
#ifndef EACH_BUFFER_OWN_OFFSCREEN
    offscreenInUse = TRUE;
#endif

    Draw(offscreen, -localx, -localy, localx, localy, w, h, show_caret, bgColor);
    {
      wxBitmap *bm;
      bm = offscreen->GetObject();
      dc->Blit(localx - dx, localy - dy, w, h, bm, 0, 0, wxCOPY);
    }

#ifndef EACH_BUFFER_OWN_OFFSCREEN
    offscreenInUse = FALSE;
    lastUsedOffscreen = this;
#endif
  } else {
    wxPen *pen;
    wxBrush *brush;
    wxFont *font;
    wxColour *fg, *bg;
#ifndef NO_GET_CLIPPING_REGION
    wxRegion *rgn;
#endif

    pen = dc->GetPen();
    brush = dc->GetBrush();
    font = dc->GetFont();
    {
      wxColour *clr;
      clr = dc->GetTextForeground();
      fg = new WXGC_PTRS wxColour(clr);
      clr = dc->GetTextBackground();
      bg = new WXGC_PTRS wxColour(clr);
    }
    
#ifndef NO_GET_CLIPPING_REGION
    rgn = dc->GetClippingRegion();
    dc->SetClippingRect(localx - dx, localy - dy, w, h);
#endif

    Draw(dc, -dx, -dy, localx, localy, w, h, show_caret, bgColor);

#ifndef NO_GET_CLIPPING_REGION
    dc->SetClippingRegion(rgn);
#endif

    dc->SetBrush(brush);
    dc->SetPen(pen);
    dc->SetFont(font);
    dc->SetTextForeground(fg);
    dc->SetTextBackground(bg);
  }

  EndSequenceLock();
}

void wxMediaPasteboard::CheckRecalc()
{
  double r, b;
  wxDC *dc;
  wxSnipLocation *loc;
  
  if (!admin)
    return;

  dc = admin->GetDC();

  if (!dc)
    return;

  if (needResize) {
    /* Find right & bottom */
    int i;
    r = b = 0;
    for (i = 0; i < snipLocationList->size; i++) {
      loc = (wxSnipLocation *)snipLocationList->vals[i];
      if (loc) {
	if (sizeCacheInvalid) {
	  loc->snip->SizeCacheInvalid();
	  loc->needResize = TRUE;
	}
	if (loc->needResize)
	  loc->Resize(dc);
	if (loc->r + HALF_DOT_WIDTH > r)
	  r = loc->r + HALF_DOT_WIDTH;
	if (loc->b + HALF_DOT_WIDTH > b)
	  b = loc->b + HALF_DOT_WIDTH;
      }
    }
  
    realWidth = r;
    realHeight = b;

    if (minWidth && (realWidth < minWidth))
      realWidth = minWidth;
    if (maxWidth && (realWidth > maxWidth))
      realWidth = maxWidth;

    if (minHeight && (realHeight < minHeight))
      realHeight = minHeight;
    if (maxHeight && (realHeight > maxHeight))
      realHeight = maxHeight;

    needResize = FALSE;
  }

  sizeCacheInvalid = FALSE;

  if (!keepSize) {
    if (realWidth != totalWidth || realHeight != totalHeight) {
      totalWidth = realWidth;
      totalHeight = realHeight;
      admin->Resized(FALSE);
    }
  }
}

void wxMediaPasteboard::Update(double x, double y, double w, double h)
{
  double r, b;

  if (delayedscrollsnip && !sequence && !flowLocked) {
    wxSnip *s = delayedscrollsnip;
    delayedscrollsnip = NULL;
    if (ScrollTo(s, 
		 delayedscrollX, delayedscrollY, 
		 delayedscrollW, delayedscrollH, 
		 TRUE, delayedscrollbias))
      return;
  }

  r = x + w;
  b = y + h;

  if (x < 0)
    x = 0;
  if (y < 0)
    y = 0;
  if (r < 0)
    r = 0;
  if (b < 0)
    b = 0;

  noImplicitUpdate = FALSE;

  if (!updateNonempty) {
    updateTop = y;
    updateLeft = x;
    if (h < 0)
      updateBottom = h;
    else
      updateBottom = b;
    if (w < 0)
      updateRight = w;
    else
      updateRight = r;
    updateNonempty = TRUE;
  } else {
    if (y < updateTop)
      updateTop = y;

    if (x < updateLeft)
      updateLeft = x;

    if ((h < 0) && (updateBottom > 0))
      updateBottom = -updateBottom;
    if (updateBottom < 0) {
      if (h < 0 && h < updateBottom)
	updateBottom = h;
      else if (h > 0 && (-b) < updateBottom)
	updateBottom = -b;
    } else if (b > updateBottom)
      updateBottom = b;

    if ((w < 0) && (updateRight > 0))
      updateRight = -updateRight;
    if (updateRight < 0) {
      if (w < 0 && w < updateRight)
	updateRight = w;
      else if (h > 0 && (-r) < updateRight)
	updateRight = -r;
    } else if (r > updateRight)
      updateRight = r;
  }

  if (sequence || !admin || flowLocked)
    return;

  CheckRecalc();

  if (updateBottom < 0) {
    updateBottom = -updateBottom;
    if (updateBottom < realHeight)
      updateBottom = realHeight;
  }
  if (updateRight < 0) {
    updateRight = -updateRight;
    if (updateRight < realWidth)
      updateRight = realWidth;
  }

  updateNonempty = FALSE;

  if (changed) {
    changed = FALSE;
    writeLocked++;
    OnChange();
    --writeLocked;
  }

  if (updateTop != updateBottom || updateLeft != updateRight) {
    /* Bizarre MSVC bug: if we inline w & h and skip the > 0 test, 
       h is wrong */
    double w = updateRight - updateLeft + 1;
    double h = updateBottom - updateTop + 1;

    if ((w > 0) && (h > 0))
      admin->NeedsUpdate(updateLeft, updateTop, w, h);
  }
}

void wxMediaPasteboard::UpdateLocation(wxSnipLocation *loc)
{
  if (admin) {
    if (loc->needResize) {
      wxDC *dc;
      dc = admin->GetDC();
      if (dc)
	loc->Resize(dc);
      /* otherwise, still need resize... */
    }
    Update(loc->x - HALF_DOT_WIDTH, loc->y - HALF_DOT_WIDTH, 
	   loc->w + DOT_WIDTH, loc->h + DOT_WIDTH);
  }
}

void wxMediaPasteboard::UpdateSnip(wxSnip *snip)
{
  wxSnipLocation *loc;

  loc = DoXSnipLoc(snipLocationList, snip);
  if (loc)
    UpdateLocation(loc);
}

void wxMediaPasteboard::UpdateSelected()
{
  int i;
  wxSnipLocation *loc;

  BeginEditSequence();

  for (i = 0; i < snipLocationList->size; i++) {
    loc = (wxSnipLocation *)snipLocationList->vals[i];
    if (loc && loc->selected)
	UpdateLocation(loc);
  }
  
  EndEditSequence();
}

void wxMediaPasteboard::UpdateAll()
{
  Update(0, 0, -1, -1);
}

void wxMediaPasteboard::UpdateNeeded()
{
  if ((updateNonempty && !noImplicitUpdate) || delayedscrollsnip)
    Update(updateLeft, updateTop, 0, 0);
}

void wxMediaPasteboard::InvalidateBitmapCache(double x, double y, double w, double h)
{
  Update(x, y, w, h);
}

/***************************************************************************/

void wxMediaPasteboard::OwnCaret(Bool ownit)
{
  if (DoOwnCaret(ownit)) {
    UpdateSelected();
    OnFocus(ownit);
  }
}

void wxMediaPasteboard::BlinkCaret()
{
  if (caretSnip) {
    wxDC *dc;
    double dx, dy;
    if ((dc = admin->GetDC(&dx, &dy))) {
      double x, y;
      if (GetSnipLocation(caretSnip, &x, &y))
	caretSnip->BlinkCaret(dc, x - dx, y - dy);
    }
  }
}

void wxMediaPasteboard::SizeCacheInvalid(void)
{
  sizeCacheInvalid = TRUE;
  needResize = TRUE;
}


void wxMediaPasteboard::GetExtent(double *w, double *h)
{
  CheckRecalc();

  if (w)
    *w = totalWidth;
  if (h)
    *h = totalHeight;
}

Bool wxMediaPasteboard::ScrollTo(wxSnip *snip, 
				 double localx, double localy, 
				 double w, double h, 
				 Bool refresh, int bias)
{
  if (sequence) {
    delayedscrollsnip = snip;
    delayedscrollX = localx;
    delayedscrollY = localy;
    delayedscrollW = w;
    delayedscrollH = h;
    return FALSE;
  } else if (admin) {
    double x, y;

    GetSnipLocation(snip, &x, &y);

    if (admin->ScrollTo(x + localx, y + localy, w, h, refresh, bias)) {
      if (!refresh) {
	updateTop = 0;
	updateLeft = 0;
	updateBottom = -1;
	updateRight = -1;
	updateNonempty = TRUE;
      }
      return TRUE;
    } else
      return FALSE;
  } else
    return FALSE;
}

void wxMediaPasteboard::SetCaretOwner(wxSnip *snip, int dist)
{
  if (DoSetCaretOwner(snip, dist)) {
    UpdateAll();
    OnFocus(!snip);
  }
}


void wxMediaPasteboard::Resized(wxSnip *snip, Bool redraw_now)
{
  wxSnipLocation *loc;
  Bool no_implicit_update;

  loc = DoXSnipLoc(snipLocationList, snip);

  if (!loc)
    return;

  if (loc->needResize)
    return;

  changed = TRUE;

  no_implicit_update = (!updateNonempty || noImplicitUpdate);

  if (!redraw_now)
    sequence++;
  BeginEditSequence();
  
  UpdateLocation(loc);

  loc->needResize = TRUE;
  needResize = TRUE;

  UpdateLocation(loc);

  EndEditSequence();
  if (!redraw_now) {
    --sequence;
    if (no_implicit_update)
      noImplicitUpdate = TRUE;
  }
}

Bool wxMediaPasteboard::Recounted(wxSnip *snip, Bool redraw_now)
{
  Resized(snip, redraw_now);
  return TRUE;
}

void wxMediaPasteboard::NeedsUpdate(wxSnip *snip, double localx, double localy, 
				    double w, double h)
{
  double x, y;

  GetSnipLocation(snip, &x, &y);
  Update(x + localx, y + localy, w, h);
}

Bool wxMediaPasteboard::ReleaseSnip(wxSnip *snip)
{
  if (_Delete(snip, NULL)) {
    if (!(snip->admin) && (snip->flags & wxSNIP_OWNED))
      snip->flags  -= wxSNIP_OWNED;

    return TRUE;
  }

  return FALSE;
}

/************************************************************************/

double wxMediaPasteboard::ScrollLineLocation(long line)
{
  return line * scrollStep;
}


long wxMediaPasteboard::NumScrollLines()
{
  return (long)((totalHeight + scrollStep - 1) / scrollStep);
}

long wxMediaPasteboard::FindScrollLine(double y)
{
  return (long)(y / scrollStep);
}

void wxMediaPasteboard::SetScrollStep(double s)
{
  if (scrollStep != s) {
    scrollStep = s;
    if (admin)
      admin->Resized(TRUE);
  }
}
 
double wxMediaPasteboard::GetScrollStep(void)
{
  return scrollStep;
}

/************************************************************************/

void wxMediaPasteboard::SetMinWidth(double w)
{
  if (w <= 0)
    minWidth = 0.0;
  else
    minWidth = w;

  needResize = TRUE;
  UpdateAll();
}

void wxMediaPasteboard::SetMaxWidth(double w)
{
  if (w <= 0)
    maxWidth = 0.0;
  else
    maxWidth = w;

  needResize = TRUE;
  UpdateAll();
}

double wxMediaPasteboard::GetMinWidth()
{
  return minWidth;
}

double wxMediaPasteboard::GetMaxWidth()
{
  return maxWidth;
}

void wxMediaPasteboard::SetMinHeight(double h)
{
  if (h <= 0)
    minHeight = 0.0;
  else
    minHeight = h;

  needResize = TRUE;
  UpdateAll();
}

void wxMediaPasteboard::SetMaxHeight(double h)
{
  if (h <= 0)
    maxHeight = 0.0;
  else
    maxHeight = h;

  needResize = TRUE;
  UpdateAll();
}

double wxMediaPasteboard::GetMinHeight()
{
  return minHeight;
}

double wxMediaPasteboard::GetMaxHeight()
{
  return maxHeight;
}

/************************************************************************/


wxMediaBuffer *wxMediaPasteboard::CopySelf(void)
{
  wxMediaPasteboard *pb;

  pb = new WXGC_PTRS wxMediaPasteboard();

  CopySelfTo(pb);

  return pb;
}

void wxMediaPasteboard::CopySelfTo(wxMediaBuffer *b)
{
  wxMediaPasteboard *pb;

  if (b->bufferType != wxPASTEBOARD_BUFFER)
	return;
  pb = (wxMediaPasteboard *)b;

  wxMediaBuffer::CopySelfTo(pb);

  pb->SetDragable(GetDragable());
  pb->SetSelectionVisible(GetSelectionVisible());
  pb->SetScrollStep(GetScrollStep());
}

double wxMediaPasteboard::GetDescent(void)
{
  return 0;
}


double wxMediaPasteboard::GetSpace(void)
{
  return 0;
}

void wxMediaPasteboard::GetCenter(double *fx, double *fy)
{
  double x, y, w, h;

  if (!admin) {
    w = totalWidth;
    h = totalHeight;
    x = y = 0;
  } else
    admin->GetView(&x, &y, &w, &h, TRUE);

  if (w > 1000)
    // Don't belive it
    w = 500;
  if (h > 1000)
    // Don't belive it
    h = 500;

  if (fx)
    *fx = w / 2;
  if (fy)
    *fy = h / 2;

}

wxchar *wxMediaPasteboard::GetFlattenedText(long *got)
{
  wxSnip *snip;
  wxchar *t, *s, *old;
  long p, alloc, offset;

  alloc = 100;
  s = new WXGC_ATOMIC wxchar[alloc];

  snip = snips;

  p = 0;

  while (snip) {
    t = snip->GetText(0, snip->count, TRUE);

    offset = wxstrlen(t);
    if (p + offset >= alloc) {
      alloc = 2 * (p + offset);
      old = s;
      s = new WXGC_ATOMIC wxchar[alloc];
      memcpy(s, old, p * sizeof(wxchar));
    }
    memcpy(s + p, t, offset * sizeof(wxchar));
    p += offset;

    snip = snip->next;
  }

  s[p] = 0;
  if (got)
    *got = p;

  return s;
}

/************************************************************************/

void wxMediaPasteboard::Clear()
{
  Delete();
}

void wxMediaPasteboard::Cut(Bool extend, long time)
{
  Copy(extend, time);
  Clear();
}

void wxMediaPasteboard::DoCopy(long time, Bool extend)
{
  wxSnip *snip, *asnip;
  wxSnipLocation *loc;
  wxStyleList *sl;

  wxmb_commonCopyRegionData = NULL;

  sl = (extend && wxmb_copyStyleList) ? wxmb_copyStyleList : styleList;
  
  for (snip = snips; snip; snip = snip->Next()) {
    loc = SnipLoc(snip);
    if (loc->selected) {
      asnip = snip->Copy();
      asnip->SetAdmin(NULL);
      asnip->style = sl->Convert(asnip->style);  
      wxmb_commonCopyBuffer->Append(asnip);
      wxmb_commonCopyBuffer2->Append(GetSnipData(loc->snip));
    }
  }

  InstallCopyBuffer(time, sl);
}

void wxMediaPasteboard::Copy(Bool extend, long time)
{
  BeginCopyBuffer();

  if (!extend)
    FreeOldCopies();

  DoCopy(time, extend);

  EndCopyBuffer();
}

void wxMediaPasteboard::DoGenericPaste(wxClipboard *cb, long time)
{
  wxSnip *start, *snip;
  double cx, cy, left, right, top, bottom, dx, dy;
  wxSnipLocation *loc;
  wxDC *dc;

  if (userLocked || writeLocked)
    return;

  start = snips;
  GetCenter(&cx, &cy);

  DoBufferPaste(cb, time);

  // Quiet the compiler:
  left = right = top = bottom = 0;

  if (admin && PTRNE(snips, start)) {
    dc = GetDC();

    if (dc) {
      /* Get top/left/bottom/right of pasted group: */
      for (snip = snips; PTRNE(snip, start); snip = snip->next) {
	loc = SnipLoc(snip);      
	if (loc->needResize)
	  loc->Resize(dc);
	if (PTREQ(snip, snips)) {
	  left = loc->x;
	  top = loc->y;
	  right = loc->r;
	  bottom = loc->b;
	} else {
	  if (loc->x < left)
	    left = loc->x;
	  if (loc->y < top)
	    top = loc->y;
	  if (loc->r > right)
	    right = loc->r;
	  if (loc->b > bottom)
	    bottom = loc->b;
	}
	AddSelected(snip);
      }

      dx = cx - (left + right) / 2;
      dy = cy - (top + bottom) / 2;
    
      /* Shift the pasted group to center: */
      Move(dx, dy);
    }
  } else {
    /* Just select them: */
    for (snip = snips; PTRNE(snip, start); snip = snip->next) {
      AddSelected(snip);
    }
  }
}

void wxMediaPasteboard::DoPaste(long time)
{
  DoGenericPaste(wxTheClipboard, time);
}

void wxMediaPasteboard::DoPasteSelection(long time)
{
#ifdef wx_xt
  DoGenericPaste(wxTheSelection, time);
#else
  DoGenericPaste(wxTheClipboard, time);
#endif
}

void wxMediaPasteboard::GenericPaste(Bool x_sel, long time)
{
  if (userLocked || writeLocked)
    return;

  BeginEditSequence();

  NoSelected();

  if (x_sel)
    DoPasteSelection(time);
  else
    DoPaste(time);

  EndEditSequence();
}

void wxMediaPasteboard::Paste(long time)
{
  GenericPaste(0, time);
}

void wxMediaPasteboard::PasteSelection(long time)
{
#ifdef wx_xt
  GenericPaste(1, time);
#endif
}

void wxMediaPasteboard::InsertPasteSnip(wxSnip *snip, wxBufferData *data)
{
  Insert(snip, snip);
  SetSnipData(snip, data);
}

void wxMediaPasteboard::InsertPasteString(wxchar *str)
{
  wxTextSnip *snip;

  snip = new WXGC_PTRS wxTextSnip();
  snip->style = GetDefaultStyle();
  if (!snip->style) {
    snip->style = styleList->BasicStyle();
  }
  snip->Insert(str, wxstrlen(str));
  
  InsertPasteSnip(snip, NULL);
}

void wxMediaPasteboard::Kill(long time)
{
  Cut(time);
}

/************************************************************************/

Bool wxMediaPasteboard::GetSnipLocation(wxSnip *thesnip, double *x, double *y, 
					Bool bottomRight)
{
  wxSnipLocation *loc;

  if (bottomRight) {
    if (!admin)
      return FALSE;
    CheckRecalc();
  }

  loc = DoXSnipLoc(snipLocationList, thesnip);
  if (!loc)
    return FALSE;
  
  if (x)
    *x = loc->x;
  if (y)
    *y = loc->y;
  if (bottomRight) {
    if (x)
      *x += loc->w;
    if (y)
      *y += loc->h;
  }

  return TRUE;
}

/************************************************************************/

wxBufferData *wxMediaPasteboard::GetSnipData(wxSnip *snip)
{
  wxSnipLocation *loc;
  wxLocationBufferData *data;

  loc = DoXSnipLoc(snipLocationList, snip);

  if (!loc)
    return wxMediaBuffer::GetSnipData(snip);

  data = new WXGC_PTRS wxLocationBufferData;
  data->x = loc->x;
  data->y = loc->y;

  data->next = wxMediaBuffer::GetSnipData(snip);

  return data;
}

void wxMediaPasteboard::SetSnipData(wxSnip *snip, wxBufferData *data)
{
  while (data) {
    if (data->dataclass && !strcmp(data->dataclass->classname, "wxloc")) {
      wxLocationBufferData *ldata;
      ldata = (wxLocationBufferData *)data;
      MoveTo(snip, ldata->x, ldata->y);
    }
    data = data->next;
  }
}

int wxMediaPasteboard::InsertPort(Scheme_Object *f, int WXUNUSED(format), Bool replaceStyles)
{
  if (userLocked || writeLocked)
    return FALSE;

  InsertFile("insert-file in pasteboard%", f, NULL, replaceStyles, TRUE);

  return wxMEDIA_FF_STD;
}

Bool wxMediaPasteboard::InsertFile(const char *who, Scheme_Object *f, const char *filename, 
				   Bool clearStyles, Bool showErrors)
{
  int n;
  char buffer[MRED_START_STR_LEN + 1];
  Bool fileerr;

  if (userLocked || writeLocked)
    return FALSE;

  showErrors = TRUE;

  n = scheme_get_byte_string(who, f, buffer, 0, MRED_START_STR_LEN, 0, 0, NULL);
  buffer[MRED_START_STR_LEN] = 0;
  if ((n != MRED_START_STR_LEN) || strcmp(buffer, MRED_START_STR)) {
    if (showErrors)
      wxmeError("insert-file in pasteboard%: not a MrEd editor<%> file");
    fileerr = TRUE;
  } else {
    wxMediaStreamInFileBase *b;
    wxMediaStreamIn *mf;
    
    b = new WXGC_PTRS wxMediaStreamInFileBase(f);
    mf = new WXGC_PTRS wxMediaStreamIn(b);
    
    if (wxReadMediaVersion(mf, b, FALSE, showErrors)) {
      if (wxReadMediaGlobalHeader(mf)) {
	if (mf->Ok())
	  fileerr = !ReadFromFile(mf, clearStyles);
	else
	  fileerr = TRUE;
      } else
	fileerr = TRUE;
      fileerr = !wxReadMediaGlobalFooter(mf) || fileerr;
    
      styleList->NewNamedStyle(STD_STYLE, NULL);
      
      fileerr = fileerr || !mf->Ok();
    } else
      fileerr = TRUE;
  }

  if (fileerr && showErrors)
    wxmeError("insert-file in pasteboard%: error loading the file");

  return !fileerr;
}

Bool wxMediaPasteboard::SavePort(Scheme_Object *f, int format, Bool showErrors)
{
  Bool fileerr;
  wxMediaStreamOutFileBase *b;
  wxMediaStreamOut *mf;

  showErrors = TRUE;
  
  b = new WXGC_PTRS wxMediaStreamOutFileBase(f);
  mf = new WXGC_PTRS wxMediaStreamOut(b);
  
  wxWriteMediaVersion(mf, b);
  
  wxWriteMediaGlobalHeader(mf);
  if (mf->Ok())
    fileerr = !WriteToFile(mf);
  else
    fileerr = TRUE;
  wxWriteMediaGlobalFooter(mf);
  
  fileerr = fileerr || !mf->Ok();

  if (fileerr && showErrors)
    wxmeError("save-file in pasteboard%: error writing the file");
  
  return !fileerr;
}

Bool wxMediaPasteboard::WriteToFile(wxMediaStreamOut *f)
{
  if (!DoWriteHeadersFooters(f, TRUE))
    return FALSE;

  wxmbWriteSnipsToFile(f, styleList, NULL, snips, NULL, NULL, this);
  
  if (!DoWriteHeadersFooters(f, FALSE))
    return FALSE;

  return TRUE;
}


Bool wxMediaPasteboard::ReadFromFile(wxMediaStreamIn *f, Bool overwritestyle)
{
  if (userLocked || writeLocked)
    return FALSE;

  return ReadSnipsFromFile(f, overwritestyle);
}


Bool wxMediaPasteboard::ReadInsert(wxSnip *snip)
{
  Insert(snip, (wxSnip *)NULL);
  return TRUE;
}

void wxMediaPasteboard::SetFilename(char *name, Bool temp)
{
  wxSnip *snip;

  filename = copystring(name);
  tempFilename = temp;

  for (snip = snips; snip; snip = snip->next) {
    if (snip->flags & wxSNIP_USES_BUFFER_PATH)
      /* Just a notification */
      snip->SetAdmin(snipAdmin);
  }
}

/************************************************************************/

void wxMediaPasteboard::StyleHasChanged(wxStyle *style)
{
  if (!style) {
    changed = TRUE;
    UpdateAll();
    return;
  }
}

/************************************************************************/

void wxMediaPasteboard::BeginEditSequence(Bool undoable, Bool interruptSeqs)
{
  WaitSequenceLock();

  if (noundomode || !undoable)
    noundomode++;

  if (!sequence && !writeLocked)
    OnEditSequence();

  sequence++;
}


void wxMediaPasteboard::EndEditSequence(void)
{
  if (!(--sequence) && !writeLocked) {
    sequenceStreak = FALSE;
    UpdateNeeded();
    AfterEditSequence();
  }

  if (noundomode)
    --noundomode;

  if (!sequence && needOnDisplaySize) {
    needOnDisplaySize = 0;
    OnDisplaySize();
  }
}

Bool wxMediaPasteboard::RefreshDelayed(void)
{
  if (sequence)
    return 1;
  
  if (!admin)
    return 1;
  else
    return admin->DelayRefresh();
}

Bool wxMediaPasteboard::InEditSequence(void)
{
  return !!sequence;
}

Bool wxMediaPasteboard::LocationsUpToDate(void)
{
  return !needResize;
}

/************************************************************************/

void wxMediaPasteboard::AddPasteboardFunctions(wxKeymap *tab)
{
  wxAddMediaPasteboardFunctions(tab);
}

void wxAddMediaPasteboardFunctions(wxKeymap *)
{
}

Bool wxMediaPasteboard::GetDragable()
{
  return dragable;
}

void wxMediaPasteboard::SetDragable(Bool d)
{
  dragable = d;
}

Bool wxMediaPasteboard::GetSelectionVisible()
{
  return selectionVisible;
}

void wxMediaPasteboard::SetSelectionVisible(Bool v)
{
  selectionVisible = v;
}


/************************************************************************/

void wxMediaPasteboard::OnChange(void)
{
}

Bool wxMediaPasteboard::CanInsert(wxSnip *, wxSnip *, double, double)
{
  return TRUE;
}

void wxMediaPasteboard::OnInsert(wxSnip *, wxSnip *, double, double)
{
}

void wxMediaPasteboard::AfterInsert(wxSnip *, wxSnip *, double, double)
{
}

Bool wxMediaPasteboard::CanDelete(wxSnip *)
{
  return TRUE;
}

void wxMediaPasteboard::OnDelete(wxSnip *)
{
}

void wxMediaPasteboard::AfterDelete(wxSnip *)
{
}

Bool wxMediaPasteboard::CanMoveTo(wxSnip *, double, double, Bool WXUNUSED(dragging))
{
  return TRUE;
}

void wxMediaPasteboard::OnMoveTo(wxSnip *, double, double, Bool WXUNUSED(dragging))
{
}

void wxMediaPasteboard::AfterMoveTo(wxSnip *, double, double, Bool WXUNUSED(dragging))
{
}

Bool wxMediaPasteboard::CanResize(wxSnip *, double, double)
{
  return TRUE;
}

void wxMediaPasteboard::OnResize(wxSnip *, double, double)
{
}

void wxMediaPasteboard::AfterResize(wxSnip *, double, double, Bool WXUNUSED(did))
{
}

Bool wxMediaPasteboard::CanSelect(wxSnip *, Bool)
{
  return TRUE;
}

void wxMediaPasteboard::OnSelect(wxSnip *, Bool)
{
}

void wxMediaPasteboard::AfterSelect(wxSnip *, Bool)
{
}

Bool wxMediaPasteboard::CanReorder(wxSnip *, wxSnip *, Bool)
{
  return TRUE;
}

void wxMediaPasteboard::OnReorder(wxSnip *, wxSnip *, Bool)
{
}

void wxMediaPasteboard::AfterReorder(wxSnip *, wxSnip *, Bool)
{
  
}

#if ALLOW_X_STYLE_SELECTION
Bool wxMediaPasteboard::OwnXSelection(Bool on, Bool WXUNUSED(update), Bool force)
{
  return DoOwnXSelection(on, force);
}
#endif

Bool wxMediaPasteboard::CanInteractiveMove(wxMouseEvent *)
{
  return TRUE;
}

void wxMediaPasteboard::OnInteractiveMove(wxMouseEvent *)
{
}

void wxMediaPasteboard::AfterInteractiveMove(wxMouseEvent *)
{
}

Bool wxMediaPasteboard::CanInteractiveResize(wxSnip *)
{
  return TRUE;
}

void wxMediaPasteboard::OnInteractiveResize(wxSnip *)
{
}

void wxMediaPasteboard::AfterInteractiveResize(wxSnip *)
{
}

/************************************************************************/

extern void wxmeGetDefaultSize(double *w, double *h);

void *wxMediaPasteboard::BeginPrint(wxDC *, Bool)
{
  SizeCacheInvalid();  

  writeLocked++;
  OnChange();
  --writeLocked;

  return NULL;
}

void wxMediaPasteboard::EndPrint(wxDC *, void *)
{
  SizeCacheInvalid();

  writeLocked++;
  OnChange();
  --writeLocked;
}

Bool wxMediaPasteboard::HasPrintPage(wxDC *dc, int p)
{
  double H, W, h, w;
  long hm, vm, hcount, vcount;

  CheckRecalc();

  dc->GetSize(&W, &H);
  if (!W || !H)
    wxmeGetDefaultSize(&W, &H);
  wxGetMediaPrintMargin(&hm, &vm);
  W -= 2 * hm;
  H -= 2 * vm;

  w = h = 0.0;
  GetExtent(&w, &h);

  hcount = (long)(w / W);
  if (hcount * W < w)
    hcount++;

  vcount = (long)(h / H);
  if (vcount * H < h)
    vcount++;

  return (p <= (hcount * vcount));
}

void wxMediaPasteboard::PrintToDC(wxDC *dc, int page)
{
  double H, W, FH, FW, h, w;
  long hm, vm, hcount, vcount, hpos, vpos, startpage, endpage, p;

  CheckRecalc();

  dc->GetSize(&W, &H);
  if (!W || !H)
    wxmeGetDefaultSize(&W, &H);
  FH = H;
  FW = W;
  wxGetMediaPrintMargin(&hm, &vm);
  W -= 2 * hm;
  H -= 2 * vm;

  w = h = 0.0;
  GetExtent(&w, &h);

  hcount = (long)(w / W);
  if (hcount * W < w)
    hcount++;

  vcount = (long)(h / H);
  if (vcount * H < h)
    vcount++;

  if (page < 0) {
    startpage = 1;
    endpage = hcount * vcount;
  } else {
    startpage = endpage = page;
  }

  for (p = startpage; p <= endpage; p++) {
    double x, y;

    vpos = (p - 1) / hcount;
    hpos = (p - 1) % hcount;

    x = hpos * W;
    y = vpos * H;

    if (page < 0)
      dc->StartPage();
    
    Draw(dc, -x + hm, -y + vm,
	 x, y, x + W, y + H,
	 FALSE,
	 NULL);

    if (page < 0)
      dc->EndPage();    
  }
}

/************************************************************************/

wxSnipLocation::wxSnipLocation()
: wxObject(WXGC_NO_CLEANUP)
{
}

void wxSnipLocation::Resize(wxDC *dc)
{
  double ww, hh;

  ww = hh = 0.0;
  snip->GetExtent(dc, x, y, &ww, &hh);
  w = ww;
  h = hh;
  r = x + w;
  b = y + h;
  hm = x + w/2;
  vm = y + h/2;
  
  needResize = FALSE;
}
