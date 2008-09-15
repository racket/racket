/*
 * File:        wx_media.cc
 * Purpose:     wxMediaEdit implementation
 * Author:      Matthew Flatt
 * Created:     1995
 * Copyright:   (c) 2004-2008 PLT Scheme Inc.
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
#include "wx_dialg.h"
#include "wx_cmdlg.h"
#include "wx_utils.h"
#include "wx_media.h"
#include "wx_types.h"

#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "wx_mpriv.h"

static void StandardWordbreak(wxMediaEdit *win, long *start, long *end, int, void*);

wxMediaWordbreakMap *wxTheMediaWordbreakMap;

#ifdef wx_mac
extern void wxMediaSetFileCreatorType(char *file, Bool is_binary);
#endif

static wxCursor *iBeam = NULL, *arrow = NULL;

/************************************************************************/

void wxme_utf8_decode(char *str, long len, wxchar **_us, long *_ulen)
{
  long ulen;
  wxchar *us;

  ulen = scheme_utf8_decode_all((unsigned char *)str, len, NULL, '?');
  us = new WXGC_ATOMIC wxchar[ulen+1];
  ulen = scheme_utf8_decode_all((unsigned char *)str, len, us, '?');
  us[ulen] = 0;
  *_us = us;
  *_ulen = ulen;
}

void wxme_utf8_encode(wxchar *us, long ulen, char **_s, long *_len)
{
  char *s;
  long len;
  len = scheme_utf8_encode_all(us, ulen, NULL);
  s = new WXGC_ATOMIC char[len + 1];
  len = scheme_utf8_encode_all(us, ulen, (unsigned char *)s);
  s[len] = 0;
  *_s = s;
  *_len = len;
}

int wxstrlen(wxchar *s)
{
  int i;
  for (i = 0; s[i]; i++) {
  }
  return i;
}

/************************************************************************/

extern void wxMediaIOCheckLSB(void);

const char *(*wxmeExpandFilename)(const char *, const char *, int) = NULL;

static const char *wxCallExpandPath(const char *f, const char *who, int to_write)
{
  return f; // wxExpandPath(NULL, f);
}

void wxInitMedia(void)
{
  wxREGGLOB(iBeam);
  wxREGGLOB(arrow);

  /* Creates NIL: */
  (void)(new WXGC_PTRS wxMediaLine);

  wxInitClipboard();
  wxInitStyles();

  wxREGGLOB(wxTheMediaWordbreakMap);
  wxTheMediaWordbreakMap = new WXGC_PTRS wxMediaWordbreakMap;

#if USE_OLD_TYPE_SYSTEM
  wxAllTypes->AddType(wxTYPE_MEDIA_CANVAS, wxTYPE_CANVAS, "media-canvas");

  wxAllTypes->AddType(wxTYPE_MEDIA_BUFFER, wxTYPE_OBJECT, "media-buffer");
  wxAllTypes->AddType(wxTYPE_MEDIA_EDIT, wxTYPE_MEDIA_BUFFER, "media-edit");
  wxAllTypes->AddType(wxTYPE_MEDIA_PASTEBOARD, wxTYPE_MEDIA_BUFFER, "media-pasteboard");

  wxAllTypes->AddType(wxTYPE_SNIP, wxTYPE_OBJECT, "snip");
  wxAllTypes->AddType(wxTYPE_TEXT_SNIP, wxTYPE_SNIP, "text-snip");
  wxAllTypes->AddType(wxTYPE_TAB_SNIP, wxTYPE_SNIP, "tab-snip");
  wxAllTypes->AddType(wxTYPE_IMAGE_SNIP, wxTYPE_SNIP, "image-snip");
  wxAllTypes->AddType(wxTYPE_MEDIA_SNIP, wxTYPE_SNIP, "media-snip");

  wxAllTypes->AddType(wxTYPE_MEDIA_ADMIN, wxTYPE_OBJECT, "media-admin");
  wxAllTypes->AddType(wxTYPE_CANVAS_MEDIA_ADMIN, wxTYPE_MEDIA_ADMIN, "canvas-media-admin");
  wxAllTypes->AddType(wxTYPE_MEDIA_SNIP_MEDIA_ADMIN, wxTYPE_MEDIA_ADMIN, "media-snip-media-admin");

  wxAllTypes->AddType(wxTYPE_MEDIA_SNIP_ADMIN, wxTYPE_OBJECT, "media-snip-admin");

  wxAllTypes->AddType(wxTYPE_SNIP_CLASS, wxTYPE_OBJECT, "snip-class");
  wxAllTypes->AddType(wxTYPE_BUFFER_DATA, wxTYPE_OBJECT, "buffer-data");
  wxAllTypes->AddType(wxTYPE_BUFFER_DATA_CLASS, wxTYPE_OBJECT, "buffer-data-class");

  wxAllTypes->AddType(wxTYPE_KEYMAP, wxTYPE_OBJECT, "keymap");

  wxAllTypes->AddType(wxTYPE_STYLE, wxTYPE_OBJECT, "style");
  wxAllTypes->AddType(wxTYPE_STYLE_DELTA, wxTYPE_OBJECT, "style-delta");
  wxAllTypes->AddType(wxTYPE_STYLE_LIST, wxTYPE_OBJECT, "style-list");

  wxAllTypes->AddType(wxTYPE_WORDBREAK_MAP, wxTYPE_OBJECT, "wordbreak-map");

  wxAllTypes->AddType(wxTYPE_SNIP_CLASS_LIST, wxTYPE_LIST, "snip-class-list");
  wxAllTypes->AddType(wxTYPE_BUFFER_DATA_CLASS_LIST, wxTYPE_LIST, "buffer-data-class-list");
#endif

  wxMediaIOCheckLSB();

  if (!wxmeExpandFilename)
    wxmeExpandFilename = wxCallExpandPath;
}

wxMediaEdit::wxMediaEdit(double spacing, double *tabstops, int numtabs) 
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_MEDIA_EDIT;
#endif
  bufferType = wxEDIT_BUFFER;

  readLocked = FALSE;
  flowLocked = FALSE;
  writeLocked = FALSE;
  userLocked = modified = FALSE;
  
  lineSpacing = spacing;
  maxWidth = minWidth = minHeight = maxHeight = -1;

  hiliteOn = TRUE;

  len = 0;

  // AddEditorFunctions(map);

  startpos = endpos = 0;
  posateol = FALSE;

  flash = FALSE;
  flashTimer = NULL;

  delayRefresh = 0;

  refreshUnset = refreshBoxUnset = TRUE;
  refreshAll = FALSE;
  delayedscroll = -1;
  delayedscrollbox = FALSE;

  {
    wxStandardSnipAdmin *ssa;
    ssa = new WXGC_PTRS wxStandardSnipAdmin(this);
    snipAdmin = ssa;
  }

  caretStyle = NULL;

  MakeOnlySnip();

  snipCacheInvalid = FALSE;

  graphicsInvalid = flowInvalid = FALSE;
  graphicMaybeInvalid = TRUE;
  graphicMaybeInvalidForce = FALSE;

  totalHeight = totalWidth = finalDescent = initialSpace = 0;

  drawCachedInBitmap = FALSE;

  overwriteMode = FALSE;

  clickbacks = NULL;

  typingStreak = FALSE;
  deletionStreak = FALSE;
  delayedStreak = FALSE;
  anchorStreak = FALSE;
  vcursorStreak = FALSE;
  killStreak = FALSE;
  insertForceStreak = FALSE;
  deleteForceStreak = FALSE;

  keepAnchorStreak = FALSE;

  caretBlinked = FALSE;

  changed = FALSE;

#if ALLOW_X_STYLE_SELECTION
  needXCopy = FALSE;
#endif

  tracking = dragging = FALSE;

  fileFormat = wxMEDIA_FF_STD;

  prevPasteStart = -1;

  stickyStyles = 1;
  initialStyleNeeded = 1;

  tabcount = numtabs;
  tabs = tabstops;
  tabSpace = wxTAB_WIDTH;
  tabSpaceInUnits = TRUE;

  wordBreak = StandardWordbreak;
  wordBreakData = NULL;
  wordBreakMap = wxTheMediaWordbreakMap;

  autoWrapBitmap = NULL;
  wrapBitmapWidth = 0;

  betweenThreshold = 2.0;
}

wxMediaEdit::~wxMediaEdit()
{
  wxSnip *snip, *next;

  SetWordbreakMap(NULL);

  for (snip = snips; snip; snip = next) {
    next = snip->next;
    DELETE_OBJ snip;
  }

  if (clickbacks)
    clickbacks->DeleteContents(TRUE);
}

wxMediaBuffer *wxMediaEdit::CopySelf(void)
{
  wxMediaEdit *m;

  m = new WXGC_PTRS wxMediaEdit(lineSpacing);

  CopySelfTo(m);

  /* All done! */
  return m;
}

void wxMediaEdit::CopySelfTo(wxMediaBuffer *b)
{
  wxMediaEdit *m;

  if (b->bufferType != wxEDIT_BUFFER)
    return;
  m = (wxMediaEdit *)b;

  /* Copy parameters, such as tab settings: */
  if (tabs) {
    double *t;

    t = new WXGC_ATOMIC double[tabcount];
    memcpy(t, tabs, sizeof(double) * tabcount);
    m->SetTabs(t, tabcount, tabSpace, tabSpaceInUnits);
  }

  wxMediaBuffer::CopySelfTo(m);

  if (!m->LastPosition()) {
    wxStyle *bs;
    char *sname;
    /* Make sure only snip in m has a good style (since we called
       m->styleList->Copy() in wxMediaBuffer::CopySelfTo). */
    sname = GetDefaultStyleName();
    bs = m->styleList->FindNamedStyle(sname);
    m->snips->style = bs;
    if (!m->snips->style) {
      bs = m->styleList->BasicStyle();
      m->snips->style = bs;
    }
  }

  m->SetFileFormat(GetFileFormat());

  m->SetWordbreakFunc(wordBreak, wordBreakData);
  m->SetWordbreakMap(GetWordbreakMap());
  m->SetBetweenThreshold(GetBetweenThreshold());
  m->HideCaret(CaretHidden());
  m->SetOverwriteMode(GetOverwriteMode());

  m->SetAutowrapBitmap(autoWrapBitmap);

  m->SetStickyStyles(stickyStyles);
}

/******************************************************************/

wxCursor *wxMediaEdit::AdjustCursor(wxMouseEvent *event)
{
  double scrollx, scrolly;
  double x, y;
  wxSnip *snip;
  wxDC *dc;
  Bool onit;
  long pos;
  wxCursor *c;
  double how_close;

  if (!iBeam) {
    arrow = new WXGC_PTRS wxCursor(wxCURSOR_ARROW);
    iBeam = new WXGC_PTRS wxCursor(wxCURSOR_IBEAM);
  }

  if (!admin)
    return NULL;

  dc = admin->GetDC(&scrollx, &scrolly);
  if (!dc)
    return NULL;

  x = event->x + scrollx;
  y = event->y + scrolly;

  if (tracking)
    return customCursor ? customCursor : arrow;
  
  /* This test needs to be the same as in Refresh(): */
  if (graphicMaybeInvalid || flowLocked || delayRefresh) {
    /* We're too busy. Ask again later. */
    if (customCursorOverrides  &&customCursor)
      return customCursor;
    return iBeam;
  }
  
  BeginSequenceLock();
    
  if (!customCursorOverrides) {
    if (caretSnip && event->Dragging()) {
      double x, y;
      GetSnipPositionAndLocation(caretSnip, NULL, &x, &y);
      c = caretSnip->AdjustCursor(dc, x - scrollx, y - scrolly, x, y, event);
      if (c) {
	EndSequenceLock();
	return c;
      }
    }
    
    /* Find snip: */
    
    pos = FindPosition(x, y, NULL, &onit, &how_close);
    if ((how_close > 0  && how_close <= betweenThreshold)
	|| (how_close < 0  && -how_close <= betweenThreshold))
      onit = FALSE;
    
    if (onit)
      snip = FindSnip(pos, +1);
    else
      snip = NULL;
    
    if (snip) {
      double x, y;
      GetSnipPositionAndLocation(snip, NULL, &x, &y);
      c = snip->AdjustCursor(dc, x - scrollx, y - scrolly, x, y, event);
      if (c) {
	EndSequenceLock();
	return c;
      }
    }
  }
  
  if (customCursor) {
    EndSequenceLock();
    return customCursor;
  }

  if (x >= 0) {
    int cb;
    pos = FindPosition(x, y, NULL);
    cb = !!FindClickback(pos, y);
    EndSequenceLock();
    return cb ? arrow : iBeam;
  } else {
    EndSequenceLock();
    return iBeam;
  }
}

void wxMediaEdit::OnEvent(wxMouseEvent *event)
{
  double scrollx, scrolly;
  double x, y;
  wxSnip *snip;
  wxDC *dc;
  Bool onit;
  Bool sequenced = FALSE;
  long now;

  if (!admin)
    return;

  if (!event->Moving()
      && !event->Entering()
      && !event->Leaving())
    EndStreaks(wxSTREAK_EXCEPT_KEY_SEQUENCE | wxSTREAK_EXCEPT_CURSOR | wxSTREAK_EXCEPT_DELAYED);

  if (event->ButtonDown() || caretSnip) {
    /* First, find clicked-on snip: */
    x = event->x;
    y = event->y;
    
    dc = admin->GetDC(&scrollx, &scrolly);
    y += scrolly;
    x += scrollx;

    if (!dc)
      return;
  } else
    dc = NULL;

  if (event->ButtonDown()) {
    double how_close;

    now = FindPosition(x, y, NULL, &onit, &how_close);
    if ((how_close > 0  && how_close <= betweenThreshold)
	|| (how_close < 0  && -how_close <= betweenThreshold))
      onit = FALSE;

    if (onit) {
      /* We're in the snip's horizontal region... */
      double top, bottom, dummy;

      snip = FindSnip(now, +1);
      
      /* ... but maybe the mouse is above or below it. */
      GetSnipLocation(snip, &dummy, &top, FALSE);
      GetSnipLocation(snip, &dummy, &bottom, TRUE);
      if ((top > y) || (y > bottom))
	snip = NULL;
    } else
      snip = NULL;
    sequenced = 0 && (PTRNE(snip, caretSnip));
    if (sequenced)
      BeginEditSequence();
    SetCaretOwner(snip);
  }

  if (caretSnip && (caretSnip->flags & wxSNIP_HANDLES_EVENTS)) {
    GetSnipPositionAndLocation(caretSnip, NULL, &x, &y);
    caretSnip->OnEvent(dc, x - scrollx, y - scrolly, x, y, event);
    if (sequenced)
      EndEditSequence();
    return;
  }

  OnLocalEvent(event);

  if (sequenced)
    EndEditSequence();
}

void wxMediaEdit::OnDefaultEvent(wxMouseEvent *event)
{
  long now;
  double scrollx, scrolly;
  double x, y;
  Bool ateol;
  wxClickback *click;
  wxDC *dc;
  double how_close;

  if (!admin)
    return;

  x = event->x;
  y = event->y;
  dc = admin->GetDC(&scrollx, &scrolly);
  y += scrolly;
  x += scrollx;

  if (!dc)
    return;

  now = FindPosition(x, y, &ateol, NULL, &how_close);
  if (how_close > 0  && how_close <= betweenThreshold)
    now++;

  if (event->ButtonDown()) {
    tracking = FALSE;
    if ((x >= 0) && (click = FindClickback(now, y))) {
      if (click->callOnDown) {
	click->f(this, click->start, click->end, click->data);
      } else {
	tracking = TRUE;
	trackClickback = click;
	if (admin)
	  admin->UpdateCursor();
	SetClickbackHilited(trackClickback, TRUE);
      }
    } else {
      dragstart = now;
      dragging = TRUE;
      if (event->ShiftDown()) {
	if (dragstart > startpos)
	  dragstart = startpos;
	else
	  dragstart = endpos;
      }
      if (now < dragstart)
	SetPositionBiasScroll(-2, now, dragstart, ateol);
      else
	SetPositionBiasScroll(2, dragstart, now, ateol);
    }
  } else if (event->Dragging()) {
    now = FindPosition(x, y, &ateol);
    if (dragging) {
      if (now < dragstart) {
	if (now != startpos || dragstart != endpos)
	  SetPositionBiasScroll(-2, now, dragstart, ateol);
      } else {
	if (now != endpos || dragstart != startpos)
	  SetPositionBiasScroll(2, dragstart, now, ateol);
      }
    } else if (tracking) {
      wxClickback *cb;
      if (x >= 0)
	cb = FindClickback(now, y);
      else
	cb = NULL;
      SetClickbackHilited(trackClickback, cb == trackClickback);
    }
  } else if (event->ButtonUp()) {
    if (dragging)
      dragging = FALSE;
    else if (tracking) {
      tracking = FALSE;
      if (trackClickback->hilited) {
	SetClickbackHilited(trackClickback, FALSE);
	trackClickback->f(this, trackClickback->start, 
			  trackClickback->end, trackClickback->data);
      }
      if (admin)
	admin->UpdateCursor();
    }
  } else if (event->Moving()) {
    dragging = FALSE;
    if (tracking) {
      tracking = FALSE;
      if (trackClickback->hilited) {
	SetClickbackHilited(trackClickback, FALSE);
	trackClickback->f(this, trackClickback->start, 
			  trackClickback->end, trackClickback->data);
      }
      if (admin)
	admin->UpdateCursor();
    }
  }
}

void wxMediaEdit::OnChar(wxKeyEvent *event)
{
  if (!admin)
    return;

  if (caretSnip && (caretSnip->flags & wxSNIP_HANDLES_EVENTS)) {
    wxDC *dc;
    double scrollx, scrolly, x, y;
    
    dc = admin->GetDC(&scrollx, &scrolly);
    GetSnipPositionAndLocation(caretSnip, NULL, &x, &y);

    caretSnip->OnChar(dc, x - scrollx, y - scrolly, x, y, event);
    return;
  }

  {
    int code;
    code = event->keyCode;
    if ((code != WXK_RELEASE)
	&& (code != WXK_SHIFT)
	&& (code != WXK_CONTROL)
	&& (code != WXK_MENU)
	&& (code != 0))
      wxHideCursor();
  }

  OnLocalChar(event);
}

void wxMediaEdit::OnDefaultChar(wxKeyEvent *event)
{
  long code, ins = -1;
  int ok = 0;

  if (!admin)
    return;

  code = event->KeyCode();

  switch(code) {
  case WXK_BACK:
    Delete();
    return;
  case WXK_DELETE:
    if (startpos == endpos) {
      if (endpos < len) {
	Delete(endpos, endpos + 1);
      }
    } else
      Delete();
    return;
  case WXK_RIGHT:
  case WXK_LEFT:
  case WXK_UP:
  case WXK_DOWN:
  case WXK_HOME:
  case WXK_END:
  case WXK_PRIOR:
  case WXK_NEXT:
    {
      int isshift;
      isshift = event->ShiftDown();
      MovePosition(code, isshift);
    }
    return;
  case WXK_NUMPAD0:
    ins = '0';
    break;
  case WXK_NUMPAD1:
    ins = '1';
    break;
  case WXK_NUMPAD2:
    ins = '2';
    break;
  case WXK_NUMPAD3:
    ins = '3';
    break;
  case WXK_NUMPAD4:
    ins = '4';
    break;
  case WXK_NUMPAD5:
    ins = '5';
    break;
  case WXK_NUMPAD6:
    ins = '6';
    break;
  case WXK_NUMPAD7:
    ins = '7';
    break;
  case WXK_NUMPAD8:
    ins = '8';
    break;
  case WXK_NUMPAD9:
    ins = '9';
    break;
  case WXK_MULTIPLY:
    ins = '*';
    break;
  case WXK_DIVIDE:
    ins = '/';
    break;
  case WXK_ADD:
    ins = '+';
    break;
  case WXK_SUBTRACT:
    ins = '-';
    break;
  case WXK_DECIMAL:
    ins = '.';
    break;
  case 3: /* NUMPAD_ENTER */
    ins = '\r';
    break;
  case WXK_RETURN:
  case WXK_TAB:
    ok = 1;
  default: /* ^^^ fallthrough ^^^ */
    if (ok || ((code >= 32)
	       && ((code <= 0xD800)
		   || (code > 0xDF00)))) {
      ins = code;
    }
  }

  if (ins > -1) {
    if (overwriteMode && (startpos == endpos))
      Insert(ins, startpos, startpos + 1);
    else
      Insert(ins);
  }
}

void wxMediaEdit::OwnCaret(Bool ownit)
{
  if (DoOwnCaret(ownit)) {
    NeedCaretRefresh();
    OnFocus(ownit);
  }
}

void wxMediaEdit::BlinkCaret()
{
  if (caretSnip) {
    wxDC *dc;
    double dx, dy;
    if ((dc = admin->GetDC(&dx, &dy))) {
      double x, y;
      if (GetSnipLocation(caretSnip, &x, &y))
	caretSnip->BlinkCaret(dc, x - dx, y - dy);
    }
  } else {
    /* This test needs to be the same as in Refresh(): */
    if (graphicMaybeInvalid || flowLocked || delayRefresh) {
      /* We're busy. Go away. */
      return;
    }

    if ((startpos == endpos) 
	&& !flash 
	&& hiliteOn) {
      caretBlinked  = !caretBlinked;
      NeedCaretRefresh();
    }
  }
}

void wxMediaEdit::SizeCacheInvalid(void)
{
  if (!graphicMaybeInvalid)
    graphicMaybeInvalid = TRUE;
  graphicsInvalid = TRUE;
  if (maxWidth > 0)
    flowInvalid = TRUE;
  snipCacheInvalid = TRUE;
}

Bool wxMediaEdit::IsLockedForRead() { 
  return readLocked;
}

Bool wxMediaEdit::IsLockedForFlow() {
  return flowLocked;
}

Bool wxMediaEdit::IsLockedForWrite() {
  return writeLocked;
}

/****************************************************************/

void wxMediaEdit::OnChange(void)
{
  /* Do nothing */
}

Bool wxMediaEdit::CanInsert(long WXUNUSED(start), long WXUNUSED(len))
{
  /* Do nothing */
  return TRUE;
}

void wxMediaEdit::OnInsert(long WXUNUSED(start), long WXUNUSED(len))
{
  /* Do nothing */
}

void wxMediaEdit::AfterInsert(long WXUNUSED(start), long WXUNUSED(len))
{
  /* Do nothing */
}

Bool wxMediaEdit::CanDelete(long WXUNUSED(start), long WXUNUSED(len))
{
  /* Do nothing */
  return TRUE;
}

void wxMediaEdit::OnDelete(long WXUNUSED(start), long WXUNUSED(len))
{
  /* Do nothing */
}

void wxMediaEdit::AfterDelete(long WXUNUSED(start), long WXUNUSED(len))
{
  /* Do nothing */
}

Bool wxMediaEdit::CanChangeStyle(long WXUNUSED(start), long WXUNUSED(len))
{
  /* Do nothing */
  return TRUE;
}

void wxMediaEdit::OnChangeStyle(long WXUNUSED(start), long WXUNUSED(len))
{
  /* Do nothing */
}

void wxMediaEdit::AfterChangeStyle(long WXUNUSED(start), long WXUNUSED(len))
{
  /* Do nothing */
}


void wxMediaEdit::AfterSetPosition(void)
{
  /* Do nothing */
}

Bool wxMediaEdit::CanSetSizeConstraint(void)
{
  return TRUE;
}

void wxMediaEdit::OnSetSizeConstraint(void)
{
  /* Do nothing */
}

void wxMediaEdit::AfterSetSizeConstraint(void)
{
  /* Do nothing */
}

void wxMediaEdit::OnSplitSnip(long pos)
{
  /* Do nothing */
}

void wxMediaEdit::OnMergeSnips(long pos)
{
  /* Do nothing */
}

/****************************************************************/

void wxMediaEdit::BeginEditSequence(Bool undoable, Bool interruptSeqs)
{
  WaitSequenceLock();

  if (!delayRefresh && !interruptSeqs)
    PushStreaks();

  EndStreaks(wxSTREAK_EXCEPT_DELAYED);

  if (noundomode || !undoable)
    noundomode++;

  if (!delayRefresh) {
#if ALLOW_X_STYLE_SELECTION
    needXCopy = TRUE;
#endif
    delayRefresh++;
    OnEditSequence();
  } else
    delayRefresh++;
}

void wxMediaEdit::EndEditSequence(void)
{
  if (!delayRefresh) {
#ifdef wx_x
    fprintf(stderr, "EndEditSequence without BeginEditSequence\n");
    return;
#endif
  }

  if (!(--delayRefresh)) {
    EndStreaks();
    PopStreaks();
    Redraw();
#if ALLOW_X_STYLE_SELECTION
    needXCopy = FALSE;
#endif
    AfterEditSequence();
  } else if (delayRefresh < 0)
    delayRefresh = 0;

  if (noundomode)
    --noundomode;

  if (!delayRefresh && needOnDisplaySize) {
    needOnDisplaySize = 0;
    OnDisplaySize();
  }
}

Bool wxMediaEdit::RefreshDelayed(void)
{
  if (delayRefresh > 0)
    return 1;
    
  if (!admin)
    return 1;
  else
    return admin->DelayRefresh();
}

Bool wxMediaEdit::InEditSequence(void)
{
  return (delayRefresh > 0);
}

Bool wxMediaEdit::LocationsUpToDate(void)
{
  return !graphicMaybeInvalid;
}

void wxMediaEdit::Recalculate(void)
{
}

void wxMediaEdit::GetPosition(long *start, long *end)
{
  if (start)
    *start = startpos;
  if (end)
    *end = endpos;
}

long wxMediaEdit::GetStartPosition(void)
{
  return startpos;
}

long wxMediaEdit::GetEndPosition(void)
{
  return endpos;
}
  
void wxMediaEdit::SetPosition(long start, long end, Bool ateol, Bool scroll,
			      int seltype)
{
  _SetPosition(FALSE, 0, start, end, ateol, scroll, seltype);
}

void wxMediaEdit::SetPositionBiasScroll(int bias, long start, long end, 
					Bool ateol, Bool scroll, int seltype)
{
  _SetPosition(FALSE, bias, start, end, ateol, scroll, seltype);
}

Bool wxMediaEdit::ScrollToPosition(long start, Bool ateol, Bool refresh,
				   long end, int bias)
{
  double topx, botx, topy, boty, w, h;

  if (flowLocked)
    return FALSE;

  if (end < start)
    end = start;

  if (delayRefresh) {
    if (admin) {
      delayedscrollbox = FALSE;
      delayedscroll = start;
      delayedscrollend = end;
      delayedscrollateol = ateol;
      delayedscrollbias = bias;
    }
    return FALSE;
  }

  if (!CheckRecalc(TRUE, FALSE))
    return FALSE;

  delayedscroll = -1;
  
  PositionLocation(start, &topx, &topy, TRUE, ateol, TRUE);
  PositionLocation(end, &botx, &boty, FALSE, ateol, TRUE);

  if (botx < topx) {
    /* when the end position is to the left of the start position */
    topx = 0;
    botx = totalWidth;
  }

  w = botx - topx;
  h = boty - topy;

  return AdminScrollTo(topx, topy, w, h, refresh, bias);
}

Bool wxMediaEdit::ScrollToPosition(long start, Bool ateol, long end, int bias)
{
  return ScrollToPosition(start, ateol, TRUE, end, bias);
}

void wxMediaEdit::GetVisiblePositionRange(long *start, long *end, Bool all)
{
  double x, y, h, w;

  if (!CheckRecalc(TRUE, FALSE))
    return;

  if (all)
    admin->GetMaxView(&x, &y, &w, &h);
  else
    admin->GetView(&x, &y, &w, &h);
  if (start) {
    long s;
    s = FindPosition(x, y);
    *start = s;
  }
  if (end) {
    long e;
    e = FindPosition(x + w, y + h);
    *end = e;
  }
}

void wxMediaEdit::GetVisibleLineRange(long *start, long *end, Bool all)
{  
  double x, y, h, w;

  if (!CheckRecalc(TRUE, FALSE))
    return;

  if (all)
    admin->GetMaxView(&x, &y, &w, &h);
  else
    admin->GetView(&x, &y, &w, &h);
  if (start) {
    long s;
    s = FindLine(y);
    *start = s;
  }
  if (end) {
    long e;
    e = FindLine(y + h);
    *end = e;
  }
}

void wxMediaEdit::MovePosition(long code, Bool extendSelection,
			       int kind)
{
  long i, start, end, extendstart, extendend;
  Bool ateol, vcursor, anchor, extend, kas;
  double y;
  int leftshrink, rightshrink;

  if (flowLocked)
    return;

  if (!CheckRecalc(maxWidth > 0, FALSE, TRUE))
    return;

  anchor = anchorStreak;
  vcursor = vcursorStreak;

  if (extendStreak || anchorStreak) {
    extendend = extendendpos;
    extendstart = extendstartpos;
  } else {
    extendend = endpos;
    extendstart = startpos;
  }

  kas = keepAnchorStreak;
  keepAnchorStreak = anchorStreak;

  EndStreaks(wxSTREAK_EXCEPT_DELAYED);

  extend = anchor || extendSelection;

  /* rightshrink: motion to right shrinks the selected region */
  rightshrink = (extend && (startpos < extendstart));
  leftshrink = (extend && (endpos > extendend));

  if (code == WXK_PRIOR) {
    code = WXK_UP;
    kind = wxMOVE_PAGE;
  } else if (code == WXK_NEXT) {
    code = WXK_DOWN;
    kind = wxMOVE_PAGE;
  }

  if (code == WXK_HOME) {
    if (leftshrink)
      SetPositionBiasScroll(-2, extendstart, extendend);
    else
      SetPositionBiasScroll(-2, 0, extend ? extendend : 0);
  } else if (code == WXK_END) {
    if (rightshrink)
      SetPositionBiasScroll(2, extendstart, extendend);
    else
      SetPositionBiasScroll(2, extend ? extendstart : len, len);
  } else if (code == WXK_LEFT) {
    if ((kind != wxMOVE_LINE)
	&& (kind != wxMOVE_WORD)
	&& !extend
	&& (startpos != endpos)) {
      SetPosition(startpos);
    } else {
      /* Pick a starting place */
      if (leftshrink)
	start = endpos;
      else
	start = startpos;

      if (kind == wxMOVE_WORD)
	FindWordbreak(&start, NULL, wxBREAK_FOR_CARET);
      else if (kind == wxMOVE_LINE)
	start = LineStartPosition(PositionLine(start, posateol));
      else
	--start;

      if (extend) {
	if (leftshrink) {
	  if (start < extendend)
	    start = extendend; /* collapse to original */
	  end = start;
	  start = startpos;
	} else {
	  end = endpos;
	}
      } else
	end = start;

      SetPositionBiasScroll(-2, start, end);
    }
  } else if (code == WXK_RIGHT) {
    if ((kind != wxMOVE_LINE)
	&& (kind != wxMOVE_WORD)
	&& !extend
	&& (startpos != endpos)) {
      SetPosition(endpos, endpos, TRUE);
    } else {
      /* Pick a starting place */
      if (rightshrink)
	end = startpos;
      else
	end = endpos;

      if (kind == wxMOVE_WORD)
	FindWordbreak(NULL, &end, wxBREAK_FOR_CARET);
      else if (kind == wxMOVE_LINE)
	end = LineEndPosition(PositionLine(end, posateol));
      else
	end++;

      if (extend) {
	if (rightshrink) {
	  if (end > extendstart)
	    end = extendstart; /* collapse to original */
	  start = end;
	  end = endpos;
	} else {
	  start = startpos;
	}
      } else
	start = end;
      
      SetPositionBiasScroll(2, start, end, TRUE);
    }
  } else if ((code == WXK_UP) || (code == WXK_DOWN)) {
    int cline, bias;
    /* Used when paging: */
    Bool specialScroll = (kind == wxMOVE_PAGE);
    double scrollLeft = 0.0, scrollWidth = 0.0;
    double scrollTop = 0.0, scrollHeight = 0.0;

    if (code == WXK_UP) {
      if (leftshrink)
	start = endpos;
      else
	start = startpos;

      if (!vcursor)
	PositionLocation(start, &vcursorloc, NULL, TRUE, posateol, TRUE);

      cline = PositionLine(start, posateol);
      if (kind == wxMOVE_PAGE) {
	/* The current top line should become the next-to bottom line.
	   The caret should go to line above current top line, but
	   watch out for:
	      - especially tall lines
	      - already at top */
	double vy, ty;
	long newtop, top;

	admin->GetView(&scrollLeft, &vy, &scrollWidth, &scrollHeight);

	/* Top line should be completely visible as bottom line after
	   scrolling. */
	top = FindScrollLine(vy);
	ty = ScrollLineLocation(top + 1);
	newtop = FindScrollLine(ty - scrollHeight);
	y = ScrollLineLocation(newtop);
	if (y < ty  - scrollHeight) {
	  newtop++;
	}
	y = ScrollLineLocation(newtop);
	/* y is the new top location */

	if (y >= vy) {
	  /* No or backward progess. */
	  y = ScrollLineLocation(top - 1);
	}

	if (y == vy) {
	  /* Must be at the top: */
	  i = FindLine(y);
	} else {
	  i = FindLine(y + scrollHeight);
	  if (LineLocation(i - 1) > y)
	    i--;
	}
	scrollTop = y;
      } else
	i = cline - 1;
      if (i >= 0)
	start = FindPositionInLine(i, vcursorloc, &ateol);
      else {
	start = 0;
	ateol = FALSE;
      }

      if (extend) {
	if (leftshrink) {
	  if (start < extendend) {
	    if ((kind != wxMOVE_PAGE) && (start < extendstart)) {
	      /* Inversion! */
	      end = extendend;
	    } else {
	      /* collapse to original */
	      end = extendend;
	      start = startpos;
	      specialScroll = 0;
	    }
	  } else {
	    end = start;
	    start = startpos;
	  }
	} else {
	  end = endpos;
	} 
      } else
	end = start;

      bias = leftshrink ? 2 : -2;
    } else {
      if (rightshrink)
	end = startpos;
      else
	end = endpos;

      if (!vcursor)
	PositionLocation(end, &vcursorloc, NULL, TRUE, posateol, TRUE);

      cline = PositionLine(end, posateol);
      if (kind == wxMOVE_PAGE) {
	double vy;
	long newtop;

	admin->GetView(&scrollLeft, &vy, &scrollWidth, &scrollHeight);

	/* Last fully-visible line is the new top line */
	newtop = FindScrollLine(vy + scrollHeight);
	y = ScrollLineLocation(newtop + 1);
	if (y > vy + scrollHeight) {
	  newtop--;
	}
	y = ScrollLineLocation(newtop);
	/* y is the new top location */

	if (y <= vy) {
	  /* No or backwards movement. Scroll back one. */
	  newtop = FindScrollLine(vy) + 1;
	  y = ScrollLineLocation(newtop);
	}

	/* Compute top line, for caret */
	i = FindLine(y);
	if (LineLocation(i, TRUE) < y)
	  i++;

	/* Now, suppose we're scrolling down while extending the
	   selection.  We want to be able to see that we're
	   selecting. So try moving the line `i' down one more, if
	   there's room: */
	if (LineLocation(i + 1, FALSE) < y + scrollHeight)
	  i++;

	scrollTop = y - 1;
      } else
	i = cline + 1;
      if (i <= numValidLines - 1)
	end = FindPositionInLine(i, vcursorloc, &ateol);
      else
	end = len;

      if (extend) {
	if (rightshrink) {
	  if (end > extendstart) {
	    if ((kind != wxMOVE_PAGE) && (end > extendend)) {
	      /* Inversion! */
	      start = extendstart;
	    } else {
	      /* collapse to original */
	      start = extendstart;
	      end = endpos;
	      specialScroll = 0;
	    }
	  } else {
	    start = end;
	    end = endpos;
	  }
	} else {
	  start = startpos;
	}
      } else
	start = end;

      bias = rightshrink ? -2 : 2;
    }

    if (specialScroll)
      BeginEditSequence();

    /* Scroll only if !specialScroll */
    SetPositionBiasScroll(bias, start, end, ateol, 
			  !specialScroll);
    if (specialScroll)
      /* Special scrolling intructions: */
      ScrollTo(NULL, scrollLeft, scrollTop,
	       scrollWidth, scrollHeight,
	       FALSE, 0);
    
    if (specialScroll)
      EndEditSequence();

    vcursorStreak = TRUE;
  }

  keepAnchorStreak = kas;
  if (extend)
    extendStreak = TRUE;

  if (extendStreak || anchorStreak) {
    extendendpos = extendend;
    extendstartpos = extendstart;
  }
}

void wxMediaEdit::SetAnchor(Bool on)
{
  int wason = anchorStreak;

  anchorStreak = on;

  if (on && !wason) {
    extendendpos = endpos;
    extendstartpos = startpos;
  }
}

Bool wxMediaEdit::GetAnchor(void)
{
  return anchorStreak;
}

void wxMediaEdit::_Insert(wxSnip *isnip, long strlen, wxchar *str, wxList *snipsl,
			  long start, long end, Bool scrollOk)
{
  long addlen, i, sPos, s, snipStartPos;
  wxSnip *gsnip, *cSnip, *beforeSnip;
  wxTextSnip *snip;
  wxTabSnip *tabsnip;
  Bool deleted = FALSE, insertedLine = FALSE, scroll;
  wxNode *node;

  if (writeLocked || userLocked)
    return;

  if (start < 0)
    return;
  if (start > len)
    start = len;

  if (caretStyle)
    if (start != end || start != startpos)
      caretStyle = NULL;

  if (end > -1)
    if (start < end) {
#if ALLOW_X_STYLE_SELECTION
      if (!delayRefresh)
	needXCopy = TRUE;
#endif	
      if (isnip || strlen || snipsl)
	BeginEditSequence();
      Delete(start, end, scrollOk);
      deleted = TRUE;
#if ALLOW_X_STYLE_SELECTION
      if (!delayRefresh)
	needXCopy = FALSE;
#endif	
    }

  if (!isnip && !strlen && !snipsl)
    return;

  /* If deleted, must end edit sequence after this point... */

  writeLocked = TRUE;

  if (isnip || snipsl) {
    int did_one;

    addlen = 0;

    if (snipsl) {
      for (node = snipsl->First(); node; node = node->Next()) {
	isnip = (wxSnip *)node->Data();
	if (!isnip->count)
	  goto give_up;
	addlen += isnip->count;
	if (isnip->IsOwned())
	  goto give_up;
      }
    } else {
      if (!isnip->count)
	goto give_up;
      
      addlen = isnip->count;
      
      if (isnip->IsOwned())
	goto give_up;
    }

    if (!CanInsert(start, addlen))
      goto give_up;
    OnInsert(start, addlen);

    flowLocked = TRUE;

    /* Make sure OnInsert didn't do something bad to the snip(s): */
    if (snipsl) {
      for (node = snipsl->First(); node; node = node->Next()) {
	isnip = (wxSnip *)node->Data();
	if (!isnip->count)
	  goto give_up;
	if (isnip->IsOwned())
	  goto give_up;
      }
    } else {
      if (!isnip->count)
	goto give_up;
      if (isnip->IsOwned())
	goto give_up;
    }

    did_one = 0;
    beforeSnip = NULL;

    for (node = snipsl ? snipsl->First() : NULL; 
	 !snipsl || node;
	 node = node ? node->Next() : node) {

      if (node)
	isnip = (wxSnip *)node->Data();

      if ((isnip->flags & wxSNIP_NEWLINE)&&!(isnip->flags & wxSNIP_HARD_NEWLINE))
	isnip->flags -= wxSNIP_NEWLINE;
    
      if (!len && !did_one) {
	/* Special case: ignore the empty snip */
	snips = lastSnip = isnip;
	lineRoot = new WXGC_PTRS wxMediaLine;
	isnip->line = lineRoot;
	lineRoot->snip = lineRoot->lastSnip = isnip;
	if (maxWidth > 0)
	  lineRoot->MarkCheckFlow();
      } else {
	if (!did_one) {
	  MakeSnipset(start, start);
	  gsnip = FindSnip(start, +2);
	  beforeSnip = gsnip;
	} else
	  gsnip = beforeSnip;
      
	if (!gsnip) {
	  AppendSnip(isnip);
	  gsnip = lastLine->lastSnip;
	  if (gsnip && (gsnip->flags & wxSNIP_HARD_NEWLINE)) {
	    wxMediaLine *lr;
	    lr = lineRoot;
	    isnip->line = lastLine->Insert(&lr, FALSE);
	    lineRoot = lr;
	    isnip->line->snip = isnip->line->lastSnip = isnip;
	    numValidLines++;
	    insertedLine = TRUE;
	  } else {
	    isnip->line = lastLine;
	    if (!lastLine->snip)
	      lastLine->snip = isnip;
	    lastLine->lastSnip = isnip;
	    if (isnip->flags & wxSNIP_HARD_NEWLINE)
	      insertedLine = TRUE; /* b/c added extra ghost line */
	  }
	} else {
	  InsertSnip(gsnip, isnip);
	  if (isnip->flags & wxSNIP_HARD_NEWLINE) {
	    wxMediaLine *lr;
	    lr = lineRoot;
	    isnip->line = gsnip->line->Insert(&lr, TRUE);
	    lineRoot = lr;
	    insertedLine = TRUE;
	    numValidLines++;
	    if (PTREQ(gsnip->line->snip, gsnip))
	      isnip->line->snip = isnip;
	    else
	      isnip->line->snip = gsnip->line->snip;
	    isnip->line->lastSnip = isnip;
	    gsnip->line->snip = gsnip;
	  
	    for (cSnip = isnip->line->snip;
		 PTRNE(cSnip, isnip); 
		 cSnip = cSnip->next) {
	      cSnip->line = isnip->line;
	    }
	  
	    gsnip->line->CalcLineLength();
	    gsnip->line->MarkRecalculate();
	  } else {
	    isnip->line = gsnip->line;
	    if (PTREQ(isnip->line->snip, gsnip))
	      isnip->line->snip = isnip;
	  }
	}

	if (maxWidth > 0) {
	  isnip->line->MarkCheckFlow();
	  if (isnip->line->prev
	      && !(isnip->line->prev->lastSnip->flags & wxSNIP_HARD_NEWLINE))
	    isnip->line->prev->MarkCheckFlow();
	  if ((isnip->flags & wxSNIP_HARD_NEWLINE) && isnip->line->next)
	    isnip->line->next->MarkCheckFlow();
	}
      }


      isnip->style = styleList->Convert(isnip->style);
      isnip->SizeCacheInvalid();

      isnip->line->CalcLineLength();
      isnip->line->MarkRecalculate();

      len += isnip->count;
      did_one = 1;
    
      SnipSetAdmin(isnip, snipAdmin);

      firstLine = lineRoot->First();
      lastLine = lineRoot->Last();

      if (!snipsl)
	break;
    }
  } else {
    int sp, cnt;

    addlen = strlen;
    
    if (!CanInsert(start, addlen))
      goto give_up;
    OnInsert(start, addlen);

    flowLocked = TRUE;

    if (!len) {
      wxStyle *style;
      style = ((stickyStyles  & !initialStyleNeeded)
	       ? snips->style 
	       : GetDefaultStyle());
      snip = InsertTextSnip(start, style);
      sPos = 0;
      caretStyle = NULL;
      lineRoot->snip = lineRoot->lastSnip = snip;
    } else {
      wxStyle *style;

      if (start)
	gsnip = FindSnip(start, -1, &sPos);
      else
	gsnip = NULL;

      if (!gsnip || (caretStyle && (gsnip->style != caretStyle))
	  || !(gsnip->flags & wxSNIP_IS_TEXT)
	  || (gsnip->count + addlen > MAX_COUNT_FOR_SNIP)
	  || (!stickyStyles
	      && (gsnip->style != GetDefaultStyle()))) {
	style = (caretStyle
		 ? caretStyle
		 : (stickyStyles
		    ? (gsnip
		       ? gsnip->style
		       : snips->style) // No style: use forward
		    : GetDefaultStyle()));
	snip = InsertTextSnip(start, style);
	caretStyle = NULL;
	sPos = start;
      } else {
	snip = (wxTextSnip *)gsnip;
	if (!(snip->flags & wxSNIP_CAN_APPEND)) {
	  style = (stickyStyles
		   ? snip->style
		   : GetDefaultStyle());
	  snip = InsertTextSnip(start, style);
	  sPos = start;
	}
      }

      if (gsnip && (gsnip->flags & wxSNIP_HARD_NEWLINE) && (gsnip->next == snip)) {
	/* Preceeding snip was a newline, so the new slip belongs on the next line: */
	wxMediaLine *oldline = gsnip->line, *newline;
	
	if (!oldline->next) {
	  wxMediaLine *lr;
	  lr = lineRoot;
	  oldline->Insert(&lr, FALSE);
	  lineRoot = lr;
	  insertedLine = TRUE;
	  numValidLines++;
	  
	  oldline->next->lastSnip = snip;
	}
      
	newline = oldline->next;
	
	snip->line = newline;
	
	oldline->lastSnip = gsnip;
	newline->snip = snip;
	
	oldline->CalcLineLength();
	oldline->MarkRecalculate();
      }
    }

    s = start - sPos;

    snip->flags |= wxSNIP_CAN_SPLIT;
    snip->Insert(str, addlen, s);
    if (snip->flags & wxSNIP_CAN_SPLIT)
      snip->flags -= wxSNIP_CAN_SPLIT;

    snip->line->CalcLineLength();
    snip->line->MarkRecalculate();

    if (maxWidth > 0) {
      snip->line->MarkCheckFlow();
      if (snip->line->prev 
	  && !(snip->line->prev->lastSnip->flags & wxSNIP_HARD_NEWLINE))
	snip->line->prev->MarkCheckFlow();
    }

    snipStartPos = start;
    str = snip->buffer;
    sp = s + snip->dtext;
    cnt = 0;
    for (i = 0; i < addlen; i++) {
      if (str[sp] == '\r')
	str[sp] = '\n';
      if (str[sp] == '\n' || str[sp] == '\t') {
	Bool newline = (str[sp] == '\n');

	MakeSnipset(i + start, i + start + 1);
	snip = (wxTextSnip *)FindSnip(i + start, +1);

	if (newline) {
	  /* Forced return - split the snip */
	  wxMediaLine *oldLine;

	  snip->flags |= wxSNIP_NEWLINE | wxSNIP_HARD_NEWLINE 
	    | wxSNIP_INVISIBLE;
	  snip->flags -= (snip->flags & wxSNIP_CAN_APPEND);

	  insertedLine = TRUE;

	  if (PTRNE(snip, snip->line->lastSnip)) {
	    wxMediaLine *lr;
	    oldLine = snip->line;
	    lr = lineRoot;
	    snip->line = oldLine->Insert(&lr, TRUE);
	    lineRoot = lr;
	    numValidLines++;
	    snip->line->lastSnip = snip;
	    snip->line->snip = oldLine->snip;
	    
	    /* Retarget snips moved to new line: */
	    for (cSnip = snip->line->snip; 
		 PTRNE(cSnip, snip);
		 cSnip = cSnip->next) {
	      cSnip->line = snip->line;
	    }

	    oldLine->snip = snip->next;

	    oldLine->CalcLineLength();
	    oldLine->MarkRecalculate();
	    if (maxWidth > 0)
	      oldLine->MarkCheckFlow();

	    snip->line->CalcLineLength();
	    snip->line->MarkRecalculate();
	    if (maxWidth > 0)
	      snip->line->MarkCheckFlow();
	  } else {
	    /* Carriage-return inserted at the end of a auto-wrapped line.
	       Line lengths stay the same, but next line now starts
	       a paragraph. */
	    if (snip->line->next)
	      if (!snip->line->next->StartsParagraph())
		snip->line->next->SetStartsParagraph(TRUE);
	  }
	} else {
	  wxSnip *rsnip;
	  tabsnip = OnNewTabSnip();
	  if (tabsnip->IsOwned() || tabsnip->count) {
	    /* Uh-oh. */
	    tabsnip = new WXGC_PTRS wxTabSnip();
	  }
	  tabsnip->style = snip->style;
	  rsnip = SnipSetAdmin(tabsnip, snipAdmin);
	  if (rsnip!= tabsnip) {
	    /* Uh-oh. */
	    tabsnip = new WXGC_PTRS wxTabSnip();
	    tabsnip->style = snip->style;
	    tabsnip->SetAdmin(snipAdmin);
	  }
	  
	  tabsnip->flags |= wxSNIP_CAN_SPLIT;
	  tabsnip->InsertUTF8("\t", 1, 0);
	  if (tabsnip->flags & wxSNIP_CAN_SPLIT)
	    tabsnip->flags -= wxSNIP_CAN_SPLIT;

	  SpliceSnip(tabsnip, snip->prev, snip->next);
	  tabsnip->line = snip->line;
	  if (PTREQ(snip->line->snip, snip))
	    tabsnip->line->snip = tabsnip;
	  if (PTREQ(snip->line->lastSnip, snip))
	    tabsnip->line->lastSnip = tabsnip;
	  DELETE_OBJ snip;
	}

	snip = (wxTextSnip *)FindSnip(i + start + 1, +1);
	snipStartPos = i + start + 1;
	str = snip->buffer;
	sp = snip->dtext;
	cnt = 0;
      } else if (cnt > MAX_COUNT_FOR_SNIP) {
	/* Divide up snip because it's too large: */
	MakeSnipset(i + start, i + start);
	snip = (wxTextSnip *)FindSnip(i + start, +1);
	snipStartPos = i + start;
	str = snip->buffer;
	sp = snip->dtext + 1;
	cnt = 1;
      } else {
	sp++;
	cnt++;
      }
    }

    firstLine = lineRoot->First();
    lastLine = lineRoot->Last();

    len += addlen;
  }

  initialStyleNeeded = 0;

  revision_count++;
  
  AdjustClickbacks(start, start, addlen, NULL);

  if (!modified) {
    wxUnmodifyRecord *ur;
    ur = new WXGC_PTRS wxUnmodifyRecord(delayedStreak);
    AddUndo(ur);
  }
  if (!noundomode) {
    wxInsertRecord *ir;
    ir = new WXGC_PTRS wxInsertRecord(start, addlen, 
				      deleted || typingStreak || delayedStreak
				      || insertForceStreak
				      || !modified,
				      startpos, endpos);
    AddUndo(ir);
  }

  if (delayRefresh)
    delayedStreak = TRUE;

  scroll = (startpos == start);

  if (startpos >= start)
    startpos += addlen;
  if (endpos >= start)
    endpos += addlen;

  if (!refreshUnset) {
    if (refreshStart >= start)
      refreshStart += addlen;
    if (refreshEnd >= start)
      refreshEnd += addlen;
  }

  extraLine = !!(lastSnip->flags & wxSNIP_NEWLINE);

  writeLocked = FALSE;
  flowLocked = FALSE;

  if (scroll)
    caretBlinked = FALSE;

  if (scroll && scrollOk) {
    delayRefresh++;
    ScrollToPosition(startpos);
    --delayRefresh;
  }

  changed = TRUE;

  caretStyle = NULL;

  if (insertedLine) {
    if (!graphicMaybeInvalid)
      graphicMaybeInvalid = TRUE;
    NeedRefresh(start);
  } else
    RefreshByLineDemand();

  if (deleted)
    EndEditSequence();

  if (!modified)
    SetModified(TRUE);
  
  AfterInsert(start, addlen);

  return;

 give_up:
  writeLocked = FALSE;
  flowLocked = FALSE;
  if (deleted)
    EndEditSequence();

  return;
}

void wxMediaEdit::Insert(wxchar *str, long start, long end, Bool scrollOk)
{
  _Insert(NULL, wxstrlen(str), str, NULL, start, end, scrollOk);
}

void wxMediaEdit::Insert(wxchar *str)
{
  Insert(str, startpos, endpos);
}

void wxMediaEdit::Insert(char *str, long start, long end, Bool scrollOk)
{
  int l;
  l = strlen(str);
  Insert(l, str, start, end, scrollOk);
}

void wxMediaEdit::Insert(char *str)
{
  Insert(str, startpos, endpos);
}

void wxMediaEdit::Insert(long len, wxchar *str, long start, long end, Bool scrollOk)
{
  _Insert(NULL, len, str, NULL, start, end, scrollOk);
}

void wxMediaEdit::Insert(long len, wxchar *str)
{
  _Insert(NULL, len, str, NULL, startpos, endpos);
}

void wxMediaEdit::Insert(long len, char *str, long start, long end, Bool scrollOk)
{
  long ulen;
  wxchar *us = NULL;
  wxme_utf8_decode(str, len, &us, &ulen);
  _Insert(NULL, ulen, us, NULL, start, end, scrollOk);
}

void wxMediaEdit::Insert(long len, char *str)
{
  Insert(len, str, startpos, endpos);
}

void wxMediaEdit::Insert(wxchar a_char, long start, long end)
{
  Bool streak, ifs;
  wxchar buffer[2];

  buffer[0] = a_char;
  buffer[1] = 0;
  
  streak = typingStreak;
  ifs = insertForceStreak;

  EndStreaks(wxSTREAK_EXCEPT_DELAYED);

  insertForceStreak = streak;
  Insert(buffer, start, end);

  insertForceStreak = ifs;
  typingStreak = TRUE;
}

void wxMediaEdit::Insert(wxchar a_char)
{
  Insert(a_char, startpos, endpos);
}

void wxMediaEdit::Insert(wxSnip *snip, long start, long end, Bool scrollOk)
{
  _Insert(snip, 0, NULL, NULL, start, end, scrollOk);
}

void wxMediaEdit::Insert(wxSnip *snip)
{
  Insert(snip, startpos, endpos);
}

void wxMediaEdit::Insert(wxList *snipsl)
{
  _Insert(NULL, 0, NULL, snipsl, startpos);
}

void wxMediaEdit::Insert(wxList *snipsl, long start, long end)
{
  _Insert(NULL, 0, NULL, snipsl, start, end);
}

void wxMediaEdit::_Delete(long start, long end, Bool withUndo, Bool scrollOk)
{
  wxSnip *prev, *next, *startSnip, *endSnip;
  long dellen;
  wxSnip *snip;
  wxDeleteRecord *rec;
  wxMediaLine *line;
  Bool deletedLine = FALSE, setCaretStyle = FALSE;
  Bool updateCursor = FALSE, movedToNext;

  if (writeLocked || userLocked)
    return;

  if (end <= -1) {
    if (!start)
      return;
    end = start;
    --start;
    setCaretStyle = TRUE;
  } else if (start == startpos && end == endpos)
    setCaretStyle = TRUE;

  if (start >= end)
    return;
  if ((start < 0) || (start >= len))
    return;

  if (end > len)
    end = len;
  
#if ALLOW_X_STYLE_SELECTION
  if (start <= startpos && end >= endpos)
    if (!delayRefresh || needXCopy) {
      needXCopy = FALSE;
      CopyOutXSelection();
    }
#endif

  writeLocked = TRUE;

  if (!CanDelete(start, end - start))
    goto give_up;
  OnDelete(start, end - start);

  flowLocked = TRUE;

  MakeSnipset(start, end);
  revision_count++;
  
  startSnip = FindSnip(start, -2);
  endSnip = FindSnip(end, -1);

  if (noundomode)
    withUndo = FALSE;

  if (withUndo) {
    if (!modified) {
      wxUnmodifyRecord *ur;
      ur = new WXGC_PTRS wxUnmodifyRecord(delayedStreak);
      AddUndo(ur);
    }
    rec = new WXGC_PTRS wxDeleteRecord(start, end, deletionStreak || delayedStreak
				       || deleteForceStreak || !modified,
				       startpos, endpos);
  } else
    rec = NULL;

  if (setCaretStyle && stickyStyles)
    caretStyle = startSnip ? startSnip->next->style : snips->style;

  for (snip = endSnip; PTRNE(snip, startSnip); snip = prev) {
    if (PTREQ(snip, caretSnip)) {
      caretSnip->OwnCaret(FALSE);
      caretSnip = NULL;
      updateCursor = TRUE;
    }

    if (withUndo)
      rec->InsertSnip(snip);

    prev = snip->prev;

    if (PTREQ(snip->line->snip, snip)) {
      if (PTREQ(snip->line->lastSnip, snip)) {
	snip->line->Delete(&lineRoot);
	deletedLine = TRUE;
	numValidLines--;
      } else
	snip->line->snip = snip->next;
    } else if (PTREQ(snip->line->lastSnip, snip)) {
      if (snip->line->next) {
	snip->line->lastSnip = snip->line->next->lastSnip;
	snip->line->next->Delete(&lineRoot);
	deletedLine = TRUE;
	numValidLines--;
      } else {
	snip->line->lastSnip = prev;
	if (!snip->line->next && extraLine)
	  deletedLine = TRUE; /* b/c maybe deleted extra ghost line */
      }
    }

    DeleteSnip(snip);
  }

  if (!snipCount) {
    MakeOnlySnip();
    if (caretStyle) {
      snips->style = caretStyle;
      caretStyle = NULL;
    }
  }

  firstLine = lineRoot->First();
  lastLine = lineRoot->Last();

  movedToNext = FALSE;

  if (startSnip) {
    if (startSnip->flags & wxSNIP_NEWLINE) {
      if (startSnip->line->next) {
	line = startSnip->line->next;
	movedToNext = TRUE;
      } else {
	startSnip->line->MarkCheckFlow();
	line = NULL;
      }
    } else
      line = startSnip->line;
  } else
    line = firstLine;

  if (line) {
    /* Fix line references from possibly moved snips: */
    next = line->lastSnip->next;
    for (snip = line->snip; PTRNE(snip, next); snip = snip->next) {
      snip->line = line;
    }
    
    line->CalcLineLength();
    line->MarkRecalculate();

    if (maxWidth >= 0) {
      line->MarkCheckFlow();
      if (line->prev && !(line->prev->lastSnip->flags & wxSNIP_HARD_NEWLINE)) {
	line->prev->MarkCheckFlow();
	if (movedToNext && deletedLine && line->prev->prev 
	    && !(line->prev->prev->lastSnip->flags & wxSNIP_HARD_NEWLINE)) {
	  /* Maybe the deleted object was in the middle of a long word,
	     and maybe now the long word can be folded into the previous
	     line */
	  line->prev->prev->MarkCheckFlow();
	}
      }
    }
  }

  AdjustClickbacks(start, end, start - end, rec);

  if (withUndo) {
    AddUndo(rec);
    if (delayRefresh)
      delayedStreak = TRUE;
  }

  dellen = end - start;
  
  len -= dellen;

  CheckMergeSnips(start);

  flowLocked = FALSE;
  writeLocked = FALSE;

  if ((startpos >= start) && (startpos <= end)) {
    caretBlinked = FALSE;
    startpos = start;
  } else if (startpos > end) {
    caretBlinked = FALSE;
    startpos -= dellen;
  }

  if ((endpos >= start) && (endpos <= end))
    endpos = start;
  else if (endpos > end)
    endpos -= dellen;

  if (!refreshUnset) {
    if ((refreshStart >= start) && (refreshStart <= end))
      refreshStart = start;
    else if (refreshStart >= end)
      refreshStart -= dellen;
    if ((refreshEnd >= start) && (refreshEnd <= end))
      refreshEnd = start;
    else if (refreshEnd >= end)
      refreshEnd -= dellen;
  }
  
  extraLine = !!(lastSnip->flags & wxSNIP_NEWLINE);

  if (scrollOk && startpos == start) {
    delayRefresh++;
    ScrollToPosition(startpos);
    --delayRefresh;
  }

  changed = TRUE;

  if (!setCaretStyle)
    caretStyle = NULL;

  if (start == len) {
    /* force recheck extra line state: */
    graphicMaybeInvalid = TRUE;
    graphicMaybeInvalidForce = TRUE;
  }

  if (deletedLine) {
    if (!graphicMaybeInvalid)
      graphicMaybeInvalid = TRUE;
    NeedRefresh(start);
  } else
    RefreshByLineDemand();

  if (!modified)
    SetModified(TRUE);

  AfterDelete(start, dellen);

  if (updateCursor)
    if (admin)
      admin->UpdateCursor();

  return;

 give_up:
  writeLocked = FALSE;
  flowLocked = FALSE;

  return;
}

void wxMediaEdit::Delete(long start, long end, Bool scrollOk)
{
  _Delete(start, end, TRUE, scrollOk);
}

void wxMediaEdit::Delete()
{
  Bool dstreak, streak = (startpos == endpos), dfs;

  dstreak = deletionStreak;
  dfs = deleteForceStreak;

  EndStreaks(wxSTREAK_EXCEPT_DELAYED);
  deleteForceStreak = dstreak;
  Delete(startpos, (endpos == startpos) ? -1 : endpos);

  deleteForceStreak = dfs;
  deletionStreak = streak;
}

void wxMediaEdit::Erase(void)
{
  Delete(0, len);
}

void wxMediaEdit::Clear()
{
  Delete(startpos, endpos);
}

void wxMediaEdit::Cut(Bool extend, long time, long start, long end)
{
  if (start < 0)
    start = startpos;
  if (end < 0)
    end = endpos;
  if (end > len)
    end = len;
  if (start >= end)
    return;
  Copy(extend, time, start, end);
  Delete(start, end);
}

void wxMediaEdit::Cut(Bool extend, long time)
{
  Cut(extend, time, -1);
}

void wxMediaEdit::DoCopy(long startp, long endp, long time, Bool extend)
{
  wxSnip *start, *end;
  wxSnip *asnip, *snip;
  wxStyleList *sl;
  Bool wl, fl;

  if (startp < 0)
    startp = 0;
  if (endp > len)
    endp = len;
  if (endp <= startp)
    return;

  MakeSnipset(startp, endp);
  
  sl = (extend && wxmb_copyStyleList) ? wxmb_copyStyleList : styleList;

  wxmb_commonCopyRegionData = GetRegionData(startp, endp);

  start = FindSnip(startp, +1);
  end = FindSnip(endp, +2);

  wl = writeLocked;
  fl = flowLocked;
  writeLocked = TRUE;
  flowLocked = TRUE;

  for (snip = start; PTRNE(snip, end); snip = snip->next) {
    asnip = snip->Copy();
    SnipSetAdmin(asnip, NULL);
    asnip->style = sl->Convert(asnip->style);  
    wxmb_commonCopyBuffer->Append(asnip);
    wxmb_commonCopyBuffer2->Append(GetSnipData(snip));
  }

  writeLocked = wl;
  flowLocked = fl;

  InstallCopyBuffer(time, sl);
}

void wxMediaEdit::Copy(Bool extend, long time, long startp, long endp)
{
  if (startp < 0)
    startp = startpos;
  if (endp < 0)
    endp = endpos;
  if (endp > len)
    endp = len;
  if (startp >= endp)
    return;

  BeginCopyBuffer();

  if (!extend)
    FreeOldCopies(); 
  
  DoCopy(startp, endp, time, extend);

  EndCopyBuffer();
}

void wxMediaEdit::Copy(Bool extend, long time)
{
  Copy(extend, time, -1);
}

void wxMediaEdit::DoGenericPaste(wxClipboard *cb, long start, long time)
{
  long delta;

  readInsert = readInsertStart = start;

  delta = len;

  DoBufferPaste(cb, time, FALSE);

  delta = len - delta;

  prevPasteStart = start;
  prevPasteEnd = start + delta;
}

void wxMediaEdit::DoPaste(long start, long time)
{
  DoGenericPaste(wxTheClipboard, start, time);
}

void wxMediaEdit::DoPasteSelection(long start, long time)
{
#ifdef wx_xt
  DoGenericPaste(wxTheSelection, start, time);
#else
  DoGenericPaste(wxTheClipboard, start, time);
#endif
}

void wxMediaEdit::GenericPaste(Bool x_sel, long time, long start, long end)
{
  int savePrevPaste;

  if (end < 0)
    end = (start < 0) ? endpos : start;
  if (start < 0)
    start = endpos;
  if (end > len)
    end = len;
  if (start > end)
    return;

  BeginEditSequence();

  if (start < end)
    Delete(start, end);

  if (x_sel)
    DoPasteSelection(start, time);
  else
    DoPaste(start, time);

  savePrevPaste = prevPasteStart;

  EndEditSequence();

  prevPasteStart = savePrevPaste;
}

void wxMediaEdit::Paste(long time, long start, long end)
{
  GenericPaste(0, time, start, end);
}

void wxMediaEdit::PasteSelection(long time, long start, long end)
{
  GenericPaste(1, time, start, end);
}

void wxMediaEdit::InsertPasteSnip(wxSnip *snip, wxBufferData *data)
{
  int addpos = snip->count;

  Insert(snip, readInsert);
  if (data) {
    wxSnip *fsnip;
    fsnip = FindSnip(readInsert, +1);
    SetSnipData(fsnip, data);
  }
  readInsert += addpos;
}

void wxMediaEdit::PasteRegionData(wxBufferData *data)
{
  SetRegionData(readInsertStart, readInsert, data);
}

void wxMediaEdit::InsertPasteString(wxchar *str)
{
#ifdef wx_msw
  /* Change cr/lf to just lf: */
  int i, offset = 0;
  for (i = 0; str[i]; i++) {
    if (str[i] == '\r' && str[i + 1] == '\n') {
      str[i + offset] = '\n';
      i++;
      --offset;
    } else if (offset)
      str[i + offset] = str[i];
  }
  str[i + offset] = 0;
#endif

  /* Change non-breaking space to space: */
  {
    int i;
    for (i = 0; str[i]; i++) {
      if (str[i] == 160)
	str[i] = 32;
    }
  }

  Insert(str, readInsert);
  readInsert += wxstrlen(str);
}

void wxMediaEdit::Paste(long time)
{
  Paste(time, startpos, endpos);
}

void wxMediaEdit::PasteSelection(long time)
{
  PasteSelection(time, startpos, endpos);
}

void wxMediaEdit::PasteNext(void)
{
  long start, end, delta;

  if (prevPasteStart < 0)
    return;

  start = prevPasteStart;
  end = prevPasteEnd;

  CopyRingNext();

  BeginEditSequence();

  Delete(start, end);

  readInsert = readInsertStart = start;

  delta = len;

  DoBufferPaste(wxTheClipboard, 0, TRUE);

  EndEditSequence();

  delta = len - delta;

  prevPasteStart = start;
  prevPasteEnd = start + delta;
}

void wxMediaEdit::Kill(long time, long start, long end)
{
  int streak;
  wxchar *text;

  if ((start < 0) != (end < 0))
    return;

  streak = killStreak;

  BeginEditSequence();
  if (start < 0) {
    int newend;
    newend = ParagraphEndPosition(PositionParagraph(endpos, posateol));

    if (startpos == newend)
      SetPosition(startpos, startpos + 1, FALSE, TRUE, wxLOCAL_SELECT);
    else {
      long i;
      
      SetPosition(startpos, newend, FALSE, TRUE, wxLOCAL_SELECT);
      text = GetText(startpos, endpos);
      for (i = endpos - startpos; i--; ) {
	if (!isspace(text[i]))
	  break;
      }
      
      if (i < 0) {
	/* Line has all spaces: move one more */
	SetPosition(startpos, endpos + 1, FALSE, TRUE, wxLOCAL_SELECT);
      }
    }
    start = startpos;
    end = endpos;
  }
  Cut(streak, time, start, end);
  EndEditSequence();

  killStreak = TRUE;
}

void wxMediaEdit::Kill(long time)
{
  Kill(time, -1, -1);
}

void wxMediaEdit::SelectAll(void)
{
  SetPosition(0, len);
}


void wxMediaEdit::SplitSnip(long pos)
{
  Bool wl;

  if (flowLocked)
    return;

  if (pos <= 0)
    return;
  if (pos >= len)
    return;

  wl = writeLocked;

  writeLocked = TRUE;
  flowLocked = TRUE;
  MakeSnipset(pos, pos);
  writeLocked = wl;
  flowLocked = FALSE;
}

Bool wxMediaEdit::ReallyCanEdit(int op)
{
  if (readLocked)
    return FALSE;

  if (op != wxEDIT_COPY) {
    if (flowLocked || writeLocked)
      return FALSE;
  }

  switch(op) {
  case wxEDIT_CLEAR:
  case wxEDIT_CUT:
  case wxEDIT_COPY:
    if (startpos == endpos)
      return FALSE;
    break;
  case wxEDIT_PASTE:
    break;
  case wxEDIT_KILL:
    if (endpos == len)
      return FALSE;
    break;
  case wxEDIT_SELECT_ALL:
    if (!len)
      return 0;
  }

  return TRUE;
}

double wxMediaEdit::GetRevisionNumber()
{
  return revision_count;
}

/****************************************************************/

wxchar *wxMediaEdit::GetFlattenedText(long *got)
{
  return GetText(-1, -1, TRUE, FALSE, got);
}

wxchar *wxMediaEdit::GetText(long start, long end, Bool flatt, Bool forceCR, long *got)
{
  wxSnip *snip;
  long count, sPos, p, num, offset, total;
  long alloc;
  wxchar *s, *t, *old;
  Bool wl, fl;

  if (readLocked) {
    if (got)
      *got = 0;
    return wx_empty_wxstr;
  }

  if (start < 0)
    start = 0;
  if (end < 0)
    end = len;

  if (start > len)
    start = len;
  if (end < start)
    end = start;

  if (end > len)
    end = len;

  count = end - start;

  if (!flatt) {
    s = new WXGC_ATOMIC wxchar[count + 1];
    s[count] = 0;
    alloc = count + 1;
  } else {
    alloc = 2 * count;
    if (!alloc)
      alloc = 2;
    s = new WXGC_ATOMIC wxchar[alloc];
    s[0] = 0;
  }

  if (!count) {
    if (got)
      *got = 0;
    return s;
  }

  wl = writeLocked;
  fl = flowLocked;
  writeLocked = TRUE;
  flowLocked = TRUE;

  snip = FindSnip(start, +1, &sPos);
  offset = start - sPos;
  num = ((snip->count - offset <= count) ? snip->count - offset : count);
  if (!flatt) {
    snip->GetTextBang(s, offset, num, 0);
    p = num;
  } else {
    int add_newline;

    t = snip->GetText(offset, num, TRUE);
    p = wxstrlen(t);
    if (forceCR && (snip->flags & wxSNIP_NEWLINE) 
	  && !(snip->flags & wxSNIP_HARD_NEWLINE)) {
      p++;
      add_newline = 1;
    } else
      add_newline = 0;
    if (p >= alloc) {
      alloc = 2 * p;
      s = new WXGC_ATOMIC wxchar[alloc];
    }
    memcpy(s, t, (p - add_newline) * sizeof(wxchar));
    if (add_newline)
      s[p - 1] = '\n';
  }
  total = num;
  snip = snip->next;
  while (snip && (count > total)) {
    num = ((total + snip->count <= count) ? snip->count : count - total);
    if (!flatt) {
      /* Precise GC: can't write directly to s + p. (And we don't
	 want to chage the interface to take a string offset.) */
      if (num < 256) {
	wxchar buffer[256];
	snip->GetTextBang(buffer, 0, num, 0);
	memcpy(s + p, buffer, num * sizeof(wxchar));
      } else {
	wxchar *ss;
	ss = new WXGC_ATOMIC wxchar[num];
	snip->GetTextBang(ss, 0, num, 0);
	memcpy(s + p, ss, num * sizeof(wxchar));
      }
      p += num;
    } else {
      int add_newline;

      t = snip->GetText(0, num, TRUE);
      offset = wxstrlen(t);

      if (forceCR && (snip->flags & wxSNIP_NEWLINE) 
	  && !(snip->flags & wxSNIP_HARD_NEWLINE)) {
	offset++;
	add_newline = 1;
      } else
	add_newline = 0;

      if (p + offset >= alloc) {
	alloc = 2 * (p + offset);
	old = s;
	s = new WXGC_ATOMIC wxchar[alloc];
	memcpy(s, old, p * sizeof(wxchar));
      }

      memcpy(s + p, t, offset * sizeof(wxchar));
      if (add_newline)
	s[p + offset - 1] = '\n';
      p += offset;
    }
    total += num;
    snip = snip->next;
  }

  writeLocked = wl;
  flowLocked = fl;

  if (flatt)
    s[p] = 0;

  if (got)
    *got = p;

  return s;
}

char *wxMediaEdit::GetTextUTF8(long start, long end, Bool flatt, Bool forceCR, long *_got)
{
  wxchar *s;
  long got, l;
  char *r = NULL;

  s = GetText(start, end, flatt, forceCR, &got);
  wxme_utf8_encode(s, got, &r, &l);

  if (_got)
    *_got = l;

  return r;
}

wxchar wxMediaEdit::GetCharacter(long start)
{
  wxSnip *snip;
  long sPos;
  wxchar buffer[2];

  if (readLocked)
    return 0;

  if (start < 0)
    start = 0;
  else if (start >= len)
    return 0;

  snip = FindSnip(start, +1, &sPos);
  snip->GetTextBang(buffer, start - sPos, 1, 0);

  return buffer[0];
}

char wxMediaEdit::GetTruncatedCharacter(long start)
{
  wxchar c;
  c = GetCharacter(start);
  if (c < 256)
    return c;
  else
    return 255;
}

/****************************************************************/

wxClickback::wxClickback() : wxObject(WXGC_NO_CLEANUP)
{
}

void wxMediaEdit::SetClickback(long start, long end, 
			       wxClickbackFunc f, void *d,
			       wxStyleDelta *delta,
			       Bool callOnDown)
{
  wxClickback *click;

  click = new WXGC_PTRS wxClickback;
  click->start = start;
  click->end = end;
  click->f = f;
  click->data = d;
  click->callOnDown = callOnDown;

  click->delta = new WXGC_PTRS wxStyleDelta;
  if (delta)
    click->delta->Copy(delta);

  SetClickback(click);
} 

void wxMediaEdit::SetClickback(wxClickback *click)
{
  if (!clickbacks) {
    clickbacks = new WXGC_PTRS wxList(wxKEY_NONE, FALSE);
  }

  clickbacks->Append(click);
}

void wxMediaEdit::RemoveClickback(long start, long end)
{
  wxNode *node, *next;
  wxClickback *click;

  if (!clickbacks) return;

  for (node = clickbacks->First(); node; node = next) {
    next = node->Next();
    click = (wxClickback *)node->Data();
    if (click->start == start && click->end == end) {
      DELETE_OBJ click;
      clickbacks->DeleteNode(node);
    }
  }
}

void wxMediaEdit::CallClickback(long start, long end)
{
  wxNode *node;
  wxClickback *click;

  if ((start > end) || !clickbacks)
    return;

  for (node = clickbacks->First(); node; node = node->Next()) {
    click = (wxClickback *)node->Data();
    if (click->start <= start && click->end >= end) {
      click->f(this, click->start, click->end, click->data);
      return;
    }
  }
}

/****************************************************************/

void wxMediaEdit::FlashOn(long start, long end, Bool ateol, Bool scroll, 
			  long timeout)
{
  _SetPosition(TRUE, 0, start, end, ateol, scroll, wxDEFAULT_SELECT);
  if (timeout > 0) {
    flashautoreset = TRUE;
    if (flashTimer) {
      flashTimer->Stop();
      DELETE_OBJ flashTimer;
    }
    flashTimer = new WXGC_PTRS wxMediaFlashTimer();
    flashTimer->media = this;
    flashTimer->Start(timeout);
  }
  flashscroll = scroll;
}

void wxMediaEdit::FlashOff(void)
{
  if (!flash)
    return;
  flashautoreset = TRUE;
  flashdirectoff = TRUE;
  _SetPosition(FALSE, 0, startpos, endpos, posateol, flashscroll, wxDEFAULT_SELECT);
}

/****************************************************************/

void StandardWordbreak(wxMediaEdit *win, long *startp, long *endp, 
		       int reason, void *)
{
  long pstart, start, lstart, tstart, end, lend, tend;
  wxchar *text;
  wxMediaWordbreakMap *wordBreakMap;

  wordBreakMap = win->GetWordbreakMap();

#define nonbreak(x) (((x > 255) ? 1 : (wordBreakMap ? wordBreakMap->map : wxTheMediaWordbreakMap->map)[x] & reason))
  /* Try looking at only MAX_DIST_TRY chars. If that fails, then
     look until a newline. */
#define MAX_DIST_TRY 30

  if (startp) {
    int phase1_complete = 0, phase2_complete = 0;

    pstart = start = *startp;

    lstart = win->FindNewline(-1, start);
    if (lstart < 0)
      lstart = 0;
    else if (reason == wxBREAK_FOR_CARET) {
      lstart = win->FindNewline(-1, lstart - 1);
      if (lstart < 0)
	lstart = 0;
    }
    lend = win->LastPosition();
    if (start + 1 < lend)
      lend = start + 1;

    if (start - lstart > MAX_DIST_TRY)
      tstart = start - MAX_DIST_TRY;
    else
      tstart = lstart;

    text = win->GetText(tstart, lend);
    
    start -= tstart;
    pstart -= tstart;

  try_start_again:

    if (!phase1_complete) {
      if (start && nonbreak(text[start]))
	--start;
      if (!nonbreak(text[start]))
	phase1_complete = 1;
    }
    if (reason != wxBREAK_FOR_SELECTION) {
      if (!phase2_complete) {
	while (start && !nonbreak(text[start])) {
	  --start;
	}
	if (nonbreak(text[start]))
	  phase2_complete = 1;
      }
    }
    while (start && nonbreak(text[start])) {
      --start;
    }
    if ((start < pstart) && !nonbreak(text[start]))
      start++;

    if (!start && (tstart != lstart)) {
      start += (tstart - lstart);
      pstart += (tstart - lstart);
      text = win->GetText(lstart, lend);
      tstart = lstart;
      goto try_start_again;
    }

    *startp = start + tstart;
  }

  if (endp) {
    int phase1_complete = 0;

    end = *endp;

    lstart = end;
    lend = win->FindNewline(1, end);
    if (lend < 0)
      lend = win->LastPosition();
    else if (reason == wxBREAK_FOR_CARET) {
      lend = win->FindNewline(1, lend + 1);
      if (lend < 0)
	lend = win->LastPosition();
    }

    if (lend - end > MAX_DIST_TRY)
      tend = end + MAX_DIST_TRY;
    else
      tend = lend;

    text = win->GetText(lstart, tend);
    
    end -= lstart;
    lend -= lstart;
    tend -= lstart;

  try_end_again:

    if (!phase1_complete) {
      while ((end < tend) && !nonbreak(text[end])) {
	end++;
      }
      if (end < tend)
	phase1_complete = 1;
    }
    while ((end < tend) && nonbreak(text[end])) {
      end++;
    }

    if ((end == tend) && (tend != lend)) {
      text = win->GetText(lstart, lstart + lend);
      tend = lend;
      goto try_end_again;
    }

    *endp = end + lstart;
  }
}

/****************************************************************/

void wxMediaEdit::SetWordbreakFunc(wxWordbreakFunc f, void *data)
{
  wordBreak = f;
  wordBreakData = data;
}

void wxMediaEdit::FindWordbreak(long *start, long *end, int reason)
{
  long oldstart, oldend;

  if (readLocked)
    return;

  oldstart = start ? *start : 0;
  oldend = end ? *end : 0;

  wordBreak(this, start, end, reason, wordBreakData);

  if (start && *start > oldstart)
    *start = oldstart;
  if (end && *end < oldend)
    *end = oldend;
}

wxMediaWordbreakMap *wxMediaEdit::GetWordbreakMap(void)
{
  return wordBreakMap;
}

void wxMediaEdit::SetWordbreakMap(wxMediaWordbreakMap *map)
{
  wordBreakMap = map;

  if (!map)
    return;
}

/****************************************************************/

void wxMediaEdit::SetLineSpacing(double s)
{
  if (flowLocked)
    return;

  if (s != lineSpacing) {
    lineSpacing = s;
    SizeCacheInvalid();
    changed = TRUE;
    NeedRefresh(-1, -1);
  }
}

double wxMediaEdit::GetMaxWidth()
{
  if (maxWidth <= 0)
    return 0.0;
  else
    return maxWidth + wrapBitmapWidth;
}

double wxMediaEdit::GetMinWidth()
{
  return minWidth;
}

void wxMediaEdit::SetMaxWidth(double w)
{
  if (flowLocked)
    return;

  if (wrapBitmapWidth && (w > 0)) {
    w -= wrapBitmapWidth;
    if (w <= 0.0)
      w = CURSOR_WIDTH + 1;
  }

  if ((w == maxWidth) || ((w <= 0) && (maxWidth <= 0)))
    return;

  if (!CanSetSizeConstraint())
    return;
  OnSetSizeConstraint();

  if (w > 0 && w < (CURSOR_WIDTH + 1))
    w = CURSOR_WIDTH + 1;
  maxWidth = w;
  flowInvalid = TRUE;
  if (!graphicMaybeInvalid)
    graphicMaybeInvalid = TRUE;
  changed = TRUE;
  NeedRefresh(-1, -1);

  AfterSetSizeConstraint();
}

void wxMediaEdit::SetMinWidth(double w)
{
  if (flowLocked)
    return;

  if (w == minWidth || ((w <= 0) && (minWidth <= 0)))
    return;

  if (!CanSetSizeConstraint())
    return;
  OnSetSizeConstraint();

  graphicMaybeInvalid = TRUE;
  graphicMaybeInvalidForce = TRUE;
  minWidth = w;
  changed = TRUE;
  NeedRefresh(-1,-1);

  AfterSetSizeConstraint();
}

void wxMediaEdit::SetMinHeight(double h)
{
  if (flowLocked)
    return;

  if (h == minHeight || ((h <= 0) && (minHeight <= 0)))
    return;

  if (!CanSetSizeConstraint())
    return;
  OnSetSizeConstraint();

  graphicMaybeInvalid = TRUE;
  graphicMaybeInvalidForce = TRUE;
  minHeight = h;
  changed = TRUE;
  NeedRefresh(-1,-1);

  AfterSetSizeConstraint();
}

void wxMediaEdit::SetMaxHeight(double h)
{
  if (flowLocked)
    return;

  if (h == maxHeight || ((h <= 0) && (maxHeight <= 0)))
    return;

  if (!CanSetSizeConstraint())
    return;
  OnSetSizeConstraint();

  graphicMaybeInvalid = TRUE;
  graphicMaybeInvalidForce = TRUE;
  maxHeight = h;
  changed = TRUE;
  NeedRefresh(-1,-1);

  AfterSetSizeConstraint();
}

double wxMediaEdit::GetMinHeight()
{
  return minHeight;
}

double wxMediaEdit::GetMaxHeight()
{
  return maxHeight;
}

/****************************************************************/

int wxMediaEdit::InsertPort(Scheme_Object *f, int format, Bool replaceStyles)
{
  if (writeLocked || userLocked)
    return FALSE;
  
  InsertFile("insert-file in text%", f, NULL, &format, replaceStyles, TRUE);

  return format;
}

Bool wxMediaEdit::InsertFile(const char *who, Scheme_Object *f, char *WXUNUSED(file), 
			     int *format, Bool clearStyles, Bool showErrors)
{
  long n;
  const int BUF_SIZE = 1000;
  wxchar buffer[BUF_SIZE];
  Bool fileerr;

  if (*format == wxMEDIA_FF_GUESS) {
    if (!wxDetectWXMEFile(who, f, 1)) {
      *format = wxMEDIA_FF_TEXT;
    } else {
      *format = wxMEDIA_FF_STD;
    }
  }

  fileerr = FALSE;

  showErrors = TRUE;

  if (*format == wxMEDIA_FF_STD) {
    if (!wxDetectWXMEFile(who, f, 1)) {
      if (showErrors) {
	char ebuf[256];
	sprintf(ebuf, "%s: not a MrEd editor<%%> file", who);
	wxmeError(ebuf);
      }
      *format = wxMEDIA_FF_TEXT;
    } else {
      wxMediaStreamInFileBase *b;
      wxMediaStreamIn *mf;

      wxDetectWXMEFile(who, f, 0);
      
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

	/* If STD_STYLE wasn't loaded, re-create it: */
	styleList->NewNamedStyle(STD_STYLE, NULL);
	
	fileerr = fileerr || !mf->Ok();
      } else
	fileerr = TRUE;
    }
  }

  if (*format == wxMEDIA_FF_TEXT || *format == wxMEDIA_FF_TEXT_FORCE_CR) {
    int savecr = 0;
    while (1) {
      buffer[0] = '\r';

      n = scheme_get_char_string(who, f, buffer+ savecr, 0, BUF_SIZE - savecr, 0, NULL);

      if ((n == EOF) || !n)
	break;
      else {
	n += savecr;
	if ((n > 1) && (buffer[n - 1] == '\r')) {
	  savecr = 1;
	  --n;
	} else
	  savecr = 0;

	{
	  int i;
	  for (i = 0; i < n - 1; i++) {
	    if ((buffer[i] == '\r') && (buffer[i + 1] == '\n')) {
	      memmove(buffer + i + 1, buffer + i + 2, (n - i - 2) * sizeof(wxchar));
	      --n;
	    }
	  }
	}
	Insert(n, buffer);
      }
    }
    if (savecr)
      Insert(1, "\r");
  } 

  if (fileerr && showErrors) {
    char ebuf[256];
    sprintf(ebuf, "%s: error loading the file", who);
    wxmeError(ebuf);
  }

  return !fileerr;
}

Bool wxMediaEdit::SavePort(Scheme_Object *f, int format, Bool showErrors)
{
  Bool fileerr;

  showErrors = TRUE;

  if (readLocked) {
    if (showErrors)
      wxmeError("save-file in text%: editor locked for reading");
    return FALSE;
  }

  if ((format == wxMEDIA_FF_SAME) || (format == wxMEDIA_FF_GUESS)
      || (format == wxMEDIA_FF_COPY))
    format = fileFormat;

  fileerr = FALSE;

  if (format == wxMEDIA_FF_TEXT || format == wxMEDIA_FF_TEXT_FORCE_CR) {
    wxchar *us;
    us = GetText(-1, -1, TRUE, format == wxMEDIA_FF_TEXT_FORCE_CR);
    scheme_put_char_string("save-file", f, us, 0, wxstrlen(us));
  } else {
    wxMediaStreamOutFileBase *b;
    wxMediaStreamOut *mf;

    b = new WXGC_PTRS wxMediaStreamOutFileBase(f);
    mf = new WXGC_PTRS wxMediaStreamOut(b);

    wxWriteMediaVersion(mf, b);

    wxWriteMediaGlobalHeader(mf);
    if (mf->Ok())
      fileerr = !WriteToFile(mf);
    wxWriteMediaGlobalFooter(mf);

    fileerr = fileerr || !mf->Ok();
  }

  if (fileerr && showErrors)
    wxmeError("save-file in text%: error writing the file");

  return !fileerr;
}

Bool wxMediaEdit::ReadFromFile(wxMediaStreamIn *f, long start, Bool overwritestyle)
{
  Bool result;

  if (writeLocked)
    return FALSE;

  if (start < 0)
    start = startpos;

  readInsert = start;

  result = ReadSnipsFromFile(f, overwritestyle);

  if (!LastPosition()) {
    /* We probably destructively changed the style list. Reset the dummy snip. */
    snips->style = GetDefaultStyle();
    if (!snips->style) {
      snips->style = styleList->BasicStyle();
    }
  }

  return result;
}

Bool wxMediaEdit::ReadFromFile(wxMediaStreamIn *f, Bool owrs)
{
  return ReadFromFile(f, -1, owrs);
}


Bool wxMediaEdit::ReadInsert(wxSnip *snip)
{
  int addpos = snip->count;

  Insert(snip, readInsert);
  readInsert += addpos;

  return TRUE;
}

Bool wxMediaEdit::ReadInsert(wxList *snipsl)
{
  int oldlen = len;

  Insert(snipsl, readInsert);
  readInsert += (len - oldlen);

  return TRUE;
}

Bool wxMediaEdit::WriteToFile(wxMediaStreamOut *f, long start, long end)
{
  wxSnip *startSnip, *endSnip;

  if (readLocked)
    return FALSE;

  if (start < 0)
    start = 0;
  if (end < 0)
    end = len;
  if (end < start)
    end = start;
    
  startSnip = FindSnip(start, +1);
  endSnip = FindSnip(end, +2);
  if (!snips->count)
    startSnip = endSnip = NULL;

  if (!DoWriteHeadersFooters(f, TRUE))
    return FALSE;

  wxmbWriteSnipsToFile(f, styleList, NULL, startSnip, endSnip, NULL, this);
  
  if (!DoWriteHeadersFooters(f, FALSE))
    return FALSE;

  return TRUE;
}

Bool wxMediaEdit::WriteToFile(wxMediaStreamOut *f)
{
  return WriteToFile(f, -1);
}

int wxMediaEdit::GetFileFormat(void)
{
  return fileFormat;
}

void wxMediaEdit::SetFileFormat(int format)
{
  if (format == wxMEDIA_FF_STD
      || format == wxMEDIA_FF_TEXT
      || format == wxMEDIA_FF_TEXT_FORCE_CR)
    fileFormat = format;
}

void wxMediaEdit::SetFilename(char *name, Bool temp)
{
  wxSnip *snip;
  Bool wl, fl;
  char *fn;

  fn = (name ? copystring(name) : (char *)NULL);
  filename = fn;
  tempFilename = temp;

  wl = writeLocked;
  fl = flowLocked;
  writeLocked = TRUE;
  flowLocked = TRUE;

  for (snip = snips; snip; snip = snip->next) {
    if (snip->flags & wxSNIP_USES_BUFFER_PATH)
      snip->SetAdmin(snipAdmin);
  }

  writeLocked = wl;
  flowLocked = fl;
}

wxBufferData *wxMediaEdit::GetRegionData(long WXUNUSED(start), long WXUNUSED(end))
{
  return NULL;
}

void wxMediaEdit::SetRegionData(long WXUNUSED(start), long WXUNUSED(end), 
				wxBufferData *)
{
}

/****************************************************************/

double *wxMediaEdit::GetTabs(int *count, double *space, Bool *inUnits)
{
  if (count)
    *count = tabcount;

  if (space)
    *space = tabSpace;

  if (inUnits)
    *inUnits = tabSpaceInUnits;

  return tabs;
}

void wxMediaEdit::SetTabs(double *newtabs, int count, 
			  double tabWidth, Bool inUnits)
{
  if (flowLocked)
    return;

  tabs = newtabs;
  tabcount = count;

  if (tabWidth >= 1)
    tabSpace = tabWidth;
  else
    tabSpace = wxTAB_WIDTH;

  tabSpaceInUnits = inUnits;

  SizeCacheInvalid();
  changed = TRUE;
  NeedRefresh(-1, -1);
}

/****************************************************************/

long wxMediaEdit::FindPositionInLine(long i, double x, Bool *ateol, Bool *onit,
				     double *how_close)
{
  return _FindPositionInLine(FALSE, i, x, ateol, onit, how_close);
}

long wxMediaEdit::_FindPositionInSnip(wxDC *dc, double X, double Y,
				      wxSnip *snip, double x,
				      double *how_close)
{
  long offset, range, i;
  Bool wl, fl;

  if (readLocked)
    return 0;

  if (x < 0) {
    if (how_close)
      *how_close = -100;
    return 0;
  }

  wl = writeLocked;
  fl = flowLocked;
  writeLocked = TRUE;
  flowLocked = TRUE;

  if (snip->PartialOffset(dc, X, Y, snip->count) <= x) {
    if (how_close)
      *how_close = 100;
    writeLocked = wl;
    flowLocked = fl;
    return snip->count;
  }

  /* Binary search for position within snip: */
  range = snip->count;
  i = range / 2;
  offset = 0;

  while (1) {
    double dl, dr;

    dl = snip->PartialOffset(dc, X, Y, offset + i);
    if (dl > x)
      range = i;
    else {
      dr = snip->PartialOffset(dc, X, Y, offset + i + 1);
      if (dr <= x) {
        offset += i;
        range -= i;
      } else {
        if (how_close) {
          if (dr - x < x - dl)
            *how_close = dr - x;
          else
            *how_close = dl - x;
        }
        break;
      }
    }
    
    i = range / 2;
  }

  writeLocked = wl;
  flowLocked = fl;

  return i + offset;
}

long wxMediaEdit::FindLine(double y, Bool *onit)
{
  if (onit)
    *onit = FALSE;
    
  if (!CheckRecalc(TRUE, FALSE))
    return 0;

  if (y <= 0)
    return 0;
  if ((y >= totalHeight) || (extraLine && y >= totalHeight - extraLineH))
    return numValidLines - (extraLine ? 0 : 1);

  if (onit)
    *onit = TRUE;

  {
    wxMediaLine *line;
    line = lineRoot->FindLocation(y);
    return line->GetLine();
  }
}

long wxMediaEdit::FindPosition(double x, double y, Bool *ateol, Bool *onit,
			       double *how_close)
{
  long i, p;
  Bool online;

  if (readLocked)
    return 0;

  if (ateol)
    *ateol = FALSE;

  i = FindLine(y, &online);
  if ((i >= numValidLines - 1) && !online && (y > 0)) {
    if (onit)
      *onit = FALSE;
    if (how_close)
      *how_close = 100;
    return len;
  }

  p = FindPositionInLine(i, x, ateol, onit, how_close);
  if (onit)
    *onit = online && *onit;

  return p;
}

long wxMediaEdit::PositionLine(long start, Bool eol)
{
  wxMediaLine *line;

  if (!CheckRecalc(maxWidth > 0, FALSE, TRUE))
    return 0;

  if (start <= 0)
    return 0;
  if (start >= len) {
    if (extraLine && !eol)
      return numValidLines;
    else
      return numValidLines - 1;
  }

  line = lineRoot->FindPosition(start);
  if (eol && (line->GetPosition() == start))
    line = line->prev;
  
  return line->GetLine();
}

void wxMediaEdit::PositionLocations(long start, 
                                    double *tx, double *ty, 
                                    double *bx, double *by, 
                                    Bool eol, Bool wholeLine)
{
  double horiz, h, descent, space, topy;
  int align;
  wxMediaLine *line;
  wxSnip *snip;
  wxDC *dc;
  Bool wl, fl;

  if (!CheckRecalc(TRUE, FALSE))
    return;

  /* Handle boundary cases first: */

  if (start <= 0) {
    if (wholeLine) {
      if (tx || bx) {
	double xl;
	xl = firstLine->GetLeftLocation(maxWidth);
	if (tx) *tx = xl;
        if (bx) *bx = xl;
      } 
      if (ty || by) {
	double yl;
	yl = firstLine->GetLocation();
	if (ty) *ty = yl;
	if (by) *by = yl + firstLine->h;
      }
      return;
    }
    line = firstLine;
  } else if (start >= len) {
    if (extraLine && !eol) {
      if (ty) *ty = totalHeight - extraLineH;
      if (by) *by = totalHeight;
      if (tx) *tx = 0;
      if (bx) *bx = 0;
      return;
    } 

    line = lastLine;

    if (wholeLine || !len) {
      if (tx || bx) {
	double xl;
	xl = line->GetRightLocation(maxWidth);
	if (tx) *tx = xl;
	if (bx) *bx = xl;
      }
      if (ty || by) {
	double yl;
	yl = lastLine->GetLocation();
        if (ty) *ty = yl;
        if (by) *by = yl + lastLine->h;
      }
      return;
    }
  } else {
    line = lineRoot->FindLine(PositionLine(start, eol));

    if (wholeLine) {
      if (by || ty) {
	double yl;
	yl = line->GetLocation();
        if (ty) *ty = yl;
        if (by) *by = yl + line->h;
      }
      if (!tx && !bx)
	return;
    }
  }

  dc = NULL;

  wl = writeLocked;
  fl = flowLocked;
  writeLocked = TRUE;
  flowLocked = TRUE;

  horiz = line->GetLeftLocation(maxWidth);
  topy = line->GetLocation();

  start -= line->GetPosition();
  if (!start) {
    snip = line->snip;
  } else if (start >= line->len) {
    horiz += (line->w - line->lastW);
    snip = line->lastSnip;
  } else {
    /* linear seach for snip */
    snip = NULL;
    
    while(1) {
      snip = snip ? snip->next : line->snip;
      
      if ((start > snip->count)
	  || ((wholeLine || start) && start == snip->count)) {
	double v;

	start -= snip->count;
	if (!dc) {
	  dc = admin->GetDC();
	  if (!dc) {
	    writeLocked = wl;
	    flowLocked = fl;
	    return;
	  }
	}
	
	v = 0.0;
	snip->GetExtent(dc, horiz, topy, &v);
	horiz += v;
      } else
	break;
    }
  }


  if (tx || bx) {
    double xv;

    if (start && !dc) {
      dc = admin->GetDC();
      if (!dc) {
	writeLocked = wl;
	flowLocked = fl;
	return;
      }
    }
      
    xv = horiz + (start ? snip->PartialOffset(dc, horiz, topy, start) : 0);
    if (tx) *tx = xv;
    if (bx) *bx = xv;
  }

  if (!wholeLine && (ty || by)) {
    if (!dc) {
      dc = admin->GetDC();
      if (!dc) {
	writeLocked = wl;
	flowLocked = fl;
	return;
      }
    }
    h = descent = space = 0.0;
    snip->GetExtent(dc, horiz, topy, NULL, &h, &descent, &space);
    align = snip->style->GetAlignment();
    if (align == wxALIGN_BOTTOM) {
      double yl;
      yl = topy + line->bottombase + descent;
      if (ty) *ty = yl - h;
      if (by) *by = yl;
    } else if (align == wxALIGN_TOP) {
      double yl;
      yl = topy + line->topbase - space;
      if (ty) *ty = yl;
      if (by) *by = yl + h;
    } else {
      double yl;
      h = (h - descent - space) / 2;
      yl = topy + ((line->topbase + line->bottombase) / 2);
      if (ty) *ty = yl - h - space;
      if (by) *by = yl + h + descent;
    }
  }

  writeLocked = wl;
  flowLocked = fl;
}

void wxMediaEdit::PositionLocation(long start, double *x, double *y, 
				   Bool top, Bool eol, Bool wholeLine)
{
  PositionLocations(start, 
                    top ? x : NULL, top ? y : NULL,
                    top ? NULL : x, top ? NULL : y,
                    eol, wholeLine);
}

double wxMediaEdit::LineLocation(long i, Bool top)
{
  wxMediaLine *line;
  double y;

  if (!CheckRecalc(TRUE, FALSE))
    return 0.0;

  if (i < 0)
    return 0;
  else if (i > numValidLines)
    return totalHeight;
  else if (i == numValidLines) {
    if (extraLine)
      return totalHeight - extraLineH;
    return totalHeight;
  }

  line = lineRoot->FindLine(i);
  y = line->GetLocation();

  if (top)
    return y;
  else
    return y + line->h;
}

long wxMediaEdit::LineStartPosition(long i, Bool visibleOnly)
{
  if (!CheckRecalc(maxWidth > 0, FALSE, TRUE))
    return 0;

  if (i < 0)
    i = 0;
  else if (i >= numValidLines) {
    if (extraLine)
      return len;
    i = numValidLines - 1;
  }

  if (visibleOnly) {
    wxMediaLine *line;
    line = lineRoot->FindLine(i);
    return FindFirstVisiblePosition(line);
  } else {
    wxMediaLine *line;
    line = lineRoot->FindLine(i);
    return line->GetPosition();
  }
}

long wxMediaEdit::LineEndPosition(long i, Bool visibleOnly)
{
  wxMediaLine *line;
  long p;

  if (!CheckRecalc(maxWidth > 0, FALSE, TRUE))
    return 0;

  if (i < 0)
    i = 0;
  else if (i >= numValidLines) {
    if (extraLine)
      return len;
    i = numValidLines - 1;
  }

  line = lineRoot->FindLine(i);

  p = line->GetPosition() + line->len;
  if (visibleOnly)
    FindLastVisiblePosition(line, &p);

  return p;
}

long wxMediaEdit::LineLength(long i)
{
  wxMediaLine *line;
  
  if (!CheckRecalc(maxWidth > 0, FALSE, TRUE))
    return 0;

  if (i < 0)
    return 0;
  else if (i >= numValidLines)
    return 0;

  line = lineRoot->FindLine(i);
  return line->len;
}

long wxMediaEdit::PositionParagraph(long i, Bool WXUNUSED(eol))
{
  wxMediaLine *l;
  int delta = 0;

  if (!CheckRecalc(FALSE, FALSE, TRUE))
    return 0;

  if (i < 0)
    i = 0;
  else if (i >= len) {
    i = len;
    if (extraLine)
      delta = 1;
  }

  l = lineRoot->FindPosition(i);

  return l->GetParagraph() + delta;
}

long wxMediaEdit::ParagraphStartPosition(long i, Bool visibleOnly)
{
  wxMediaLine *l;

  if (!CheckRecalc(FALSE, FALSE, TRUE))
    return 0;

  if (i < 0)
    i = 0;

  l = lineRoot->FindParagraph(i);
  if (!l) {
    if (extraLine)
      return len;
    else {
      l = lastLine;
      while (l->prev && !l->StartsParagraph()) {
	l = l->prev;
      }
    }
  }
  
  if (visibleOnly)
    return FindFirstVisiblePosition(l);
  else
    return l->GetPosition();
}

long wxMediaEdit::ParagraphEndPosition(long i, Bool visibleOnly)
{
  wxMediaLine *l;
  long p;

  if (!CheckRecalc(FALSE, FALSE, TRUE))
    return 0;

  if (i < 0)
    i = 0;

  l = lineRoot->FindParagraph(i);
  if (l) {
    while (l->next && !l->next->StartsParagraph()) {
      l = l->next;
    }
  } else {
    if (extraLine)
      return len;
    else
      l = lastLine;
  }

  p = l->GetPosition() + l->len;
  if (visibleOnly)
    FindLastVisiblePosition(l, &p);

  return p;
}

long wxMediaEdit::LineParagraph(long i)
{
  wxMediaLine *l;

  if (!CheckRecalc(maxWidth > 0, FALSE, TRUE))
    return 0;

  if (i < 0)
    return 0;
  else if (i >= numValidLines)
    return lastLine->GetParagraph() + (extraLine ? 1 : 0);
  
  l = lineRoot->FindLine(i);
  
  return l->GetParagraph();
}

long wxMediaEdit::ParagraphStartLine(long i)
{
  wxMediaLine *l;

  if (!CheckRecalc(maxWidth > 0, FALSE, TRUE))
    return 0;

  if (i < 0)
    i = 0;

  l = lineRoot->FindParagraph(i);
  if (!l)
    return LastLine();
  
  return l->GetLine();
}

long wxMediaEdit::ParagraphEndLine(long i)
{
  wxMediaLine *l;

  if (!CheckRecalc(maxWidth > 0, FALSE, TRUE))
    return 0;

  if (i < 0)
    i = 0;

  l = lineRoot->FindParagraph(i);
  if (l) {
    while (l->next && !l->next->StartsParagraph()) {
      l = l->next;
    }
  } else
    return LastLine();

  return l->GetLine();
}

long wxMediaEdit::LastPosition(void)
{
  return len;
}

long wxMediaEdit::LastLine(void)
{
  if (!CheckRecalc(maxWidth > 0, FALSE, TRUE))
    return 0;

  return numValidLines - (extraLine ? 0 : 1);
}

long wxMediaEdit::LastParagraph(void)
{
  if (!CheckRecalc(maxWidth > 0, FALSE, TRUE))
    return 0;

  return lastLine->GetParagraph() + (extraLine ? 1 : 0);
}

void wxMediaEdit::GetExtent(double *w, double *h)
{
  CheckRecalc(TRUE, FALSE);

  if (w)
    *w = totalWidth;
  if (h)
    *h = totalHeight;
}

double wxMediaEdit::GetDescent(void)
{
  CheckRecalc(TRUE, FALSE);

  return finalDescent;
}

double wxMediaEdit::GetSpace(void)
{
  CheckRecalc(TRUE, FALSE);

  return initialSpace;
}

double wxMediaEdit::GetTopLineBase(void)
{
  CheckRecalc(TRUE, FALSE);

  return initialLineBase;
}

double wxMediaEdit::ScrollLineLocation(long scroll)
{
  wxMediaLine *line;
  long p;
  double y;
  long total;

  if (readLocked)
    return 0;

  CheckRecalc(TRUE, FALSE);
  
  total = lastLine->GetScroll() + lastLine->numscrolls;

  if (scroll == total) {
    if (extraLine)
      return totalHeight - extraLineH;
    else
      return totalHeight;
  } else if (scroll > total)
    return totalHeight;

  line = lineRoot->FindScroll(scroll);
  p = line->GetScroll();
  y = line->GetLocation();

  if (p < scroll)
    y += line->ScrollOffset(scroll - p);

  return y;
}

long wxMediaEdit::NumScrollLines()
{
  if (readLocked)
    return 0;

  CheckRecalc(maxWidth > 0, FALSE, TRUE);

  return lastLine->GetScroll() + lastLine->numscrolls + (extraLine ? 1 : 0);
}

long wxMediaEdit::FindScrollLine(double p)
{
  wxMediaLine *line;
  double y;
  long s;

  if (readLocked)
    return 0;

  CheckRecalc(TRUE, FALSE);

  if (extraLine && (p >= totalHeight - extraLineH))
    return NumScrollLines() - 1;

  line = lineRoot->FindLocation(p);
  s = line->GetScroll();

  if (line->numscrolls > 1) {
    y = line->GetLocation();
    s += line->FindExtraScroll(p - y);
  }

  return s;
}

/****************************************************************/

long wxMediaEdit::FindString(wxchar *str, int direction, long start, long end,
			     Bool bos, Bool caseSens)
{
  if (!CheckRecalc(FALSE, FALSE))
    return -1;

  return _FindStringAll(str, direction, start, end, NULL, TRUE, bos, caseSens);
}

long wxMediaEdit::FindStringUTF8(char *str, int direction, long start, long end,
				 Bool bos, Bool caseSens)
{
  long ulen;
  wxchar *us = NULL;
  wxme_utf8_decode(str, strlen(str), &us, &ulen);

  return FindString(us, direction, start, end, bos, caseSens);
}

long *wxMediaEdit::FindStringAll(wxchar *str, long *cnt, int direction,
				 long start, long end, Bool bos, Bool caseSens)
{
  long *positions, cntv;

  if (!CheckRecalc(FALSE, FALSE)) {
    *cnt = 0;
    return NULL;
  }

  cntv = _FindStringAll(str, direction, start, end, &positions, FALSE,
			bos, caseSens);
  *cnt = cntv;

  if (*cnt < 0) {
    *cnt = 0;
    positions = NULL;
  }
  return positions;
}

long *wxMediaEdit::FindStringAllUTF8(char *str, long *cnt, int direction,
				     long start, long end, Bool bos, Bool caseSens)
{
  long ulen;
  wxchar *us = NULL;
  wxme_utf8_decode(str, strlen(str), &us, &ulen);

  return FindStringAll(us, cnt, direction, start, end, bos, caseSens);
}

long wxMediaEdit::FindNewline(int direction, long start, long end)
{
  long para, pos;

  para = PositionParagraph(start, direction < 0 ? TRUE : FALSE);

  if (direction > 0)
    para++;

  pos = ParagraphStartPosition(para);

  if (direction > 0) {
    if (pos > end)
      return -1;
  } else {
    if (pos < end)
      return -1;
  }

  return pos;
}

/****************************************************************/

void wxMediaEdit::ChangeStyle(wxStyleDelta *delta)
{
  ChangeStyle(delta, -1);
}

void wxMediaEdit::ChangeStyle(wxStyleDelta *delta, long start, long end, Bool counts_as_mod)
{
  _ChangeStyle(start > -1 ? start : startpos, 
	       end > -1 ? end : (start > -1 ? len : endpos), 
	       NULL, delta, 1, counts_as_mod);
}

void wxMediaEdit::ChangeStyle(wxStyle *style, long start, long end, Bool counts_as_mod)
{
  _ChangeStyle(start > -1 ? start : startpos, 
	       end > -1 ? end : (start > -1 ? len : endpos), 
	       style, NULL, 1, counts_as_mod);
}

void wxMediaEdit::SetStyleList(wxStyleList *newList)
{
  wxSnip *snip;
  wxStyle *style, *baseStyle, *newStyle = NULL;
  int count, index, baseIndex;
  wxStyle **smap;
  wxStyleDelta *delta;
  char *name;

  if (writeLocked)
    return;

  delta = new WXGC_PTRS wxStyleDelta;

  count = styleList->Number();
  if (count) {
    wxStyle *i2s;
    smap = new WXGC_PTRS wxStyle*[count];
    i2s = newList->IndexToStyle(0); /* base style maps to base style */
    smap[0] = i2s;
    for (index = 1; index < count; index++) {
      style = styleList->IndexToStyle(index);
      name = style->GetName();

      if (!name || !(newStyle = newList->FindNamedStyle(name))) {
	baseStyle = style->GetBaseStyle();
	baseIndex = styleList->StyleToIndex(baseStyle);
	
	if (style->IsJoin()) {
	  int shiftIndex;
	  wxStyle *ss;
	  ss = style->GetShiftStyle();
	  shiftIndex = styleList->StyleToIndex(ss);
	  
	  newStyle = newList->FindOrCreateJoinStyle(smap[baseIndex], smap[shiftIndex]);
	} else {
	  style->GetDelta(delta);
	  
	  newStyle = newList->FindOrCreateStyle(smap[baseIndex], delta);
	}
	if (name)
	  newStyle = newList->NewNamedStyle(name, newStyle);
      }
      
      smap[index] = newStyle;
    }
    
    for (snip = snips; snip; snip = snip->next) {
      style = snip->style;
      index = styleList->StyleToIndex(style);
      if (index < 0) {
	/* Bad! Snip had style not from this buffer's style list */
	snip->style = smap[0];
      } else
	snip->style = smap[index];
    }
  }

  wxMediaBuffer::SetStyleList(newList);

  SizeCacheInvalid();
  changed = TRUE;
  NeedRefresh(-1, -1);
}

void wxMediaEdit::StyleHasChanged(wxStyle *style)
{
  wxSnip *snip;
  Bool wl, fl;

  if (readLocked)
    return;

  if (!style) {
    /* Our queue to repaint */
    changed = TRUE;
    NeedRefresh(-1, -1);
    return;
  }
  
  wl = writeLocked;
  fl = flowLocked;
  writeLocked = TRUE;
  flowLocked = TRUE;

  for (snip = snips; snip; snip = snip->next) {
    if (PTREQ(snip->style, style)) {
      snip->style = style;
      snip->SizeCacheInvalid();
      snip->line->MarkRecalculate();
      if (maxWidth >= 0) {
	snip->line->MarkCheckFlow();
	if (snip->line->prev
	    && !(snip->line->prev->lastSnip->flags & wxSNIP_HARD_NEWLINE))
	  snip->line->prev->MarkCheckFlow();
      }
    }
  }

  writeLocked = wl;
  flowLocked = fl;
}

/****************************************************************/

Bool wxMediaEdit::ScrollTo(wxSnip *snip, double localx, double localy, 
			   double w, double h, Bool refresh, int bias)
{
  double x, y;

  if (flowLocked)
    return FALSE;

  if (delayRefresh) {
    if (admin) {
      delayedscroll = -1;
      delayedscrollbox = TRUE;
      delayedscrollsnip = snip;
      delayedscrollX = localx;
      delayedscrollY = localy;
      delayedscrollW = w;
      delayedscrollH = h;
      delayedscrollbias = bias;
    }
    return FALSE;
  } else {
    if (snip) {
      if (!GetSnipPositionAndLocation(snip, NULL, &x, &y))
	return FALSE;
    } else
      x = y = 0;
    if (AdminScrollTo(x + localx, y + localy, w, h, refresh, bias)) {
      if (!refresh)
	refreshAll = TRUE;
      return TRUE;
    } else
      return FALSE;
  }
}

void wxMediaEdit::Resized(wxSnip *snip, Bool redraw_now)
{
  if (!GetSnipPositionAndLocation(snip, NULL, NULL, NULL))
    return;

  snip->line->MarkRecalculate();
  if (maxWidth >= 0) {
    snip->line->MarkCheckFlow();
    /* Maybe something can now move to the previous line. */
    if (snip->line->prev
	&& !(snip->line->prev->lastSnip->flags & wxSNIP_HARD_NEWLINE))
      snip->line->prev->MarkCheckFlow();
  }
  if (!graphicMaybeInvalid)
    graphicMaybeInvalid = TRUE;

  if (flowLocked)
    redraw_now = FALSE;

  changed = TRUE;

  if (!redraw_now)
    delayRefresh++;
  RefreshByLineDemand();
  if (!redraw_now)
    --delayRefresh;
}

Bool wxMediaEdit::Recounted(wxSnip *snip, Bool redraw_now)
{
  if (writeLocked)
    return FALSE;

  revision_count++;

  Resized(snip, redraw_now);
  
  return TRUE;
}

void wxMediaEdit::SetCaretOwner(wxSnip *snip, int dist)
{
  if (DoSetCaretOwner(snip, dist)) {
    NeedRefresh(startpos, endpos); /* NeedCaretRefresh(); <- doesn't work; local caret ownership weirdness*/
    OnFocus(!snip);
  }
}

Bool wxMediaEdit::ReleaseSnip(wxSnip *snip)
{
  long pos;

  if ((pos = GetSnipPosition(snip)) < 0)
    return FALSE;

  _Delete(pos, pos + snip->count, FALSE, FALSE);

  if (!(snip->admin) && (snip->flags & wxSNIP_OWNED))
    snip->flags  -= wxSNIP_OWNED;

  return TRUE;
}

void wxMediaEdit::RefreshBox(double L, double T, double w, double h)
{
  double B, R;

  B = T + h;
  R = L + w;

  if (refreshBoxUnset) {
    refreshL = L;
    refreshR = R;
    refreshT = T;
    refreshB = B;
    refreshBoxUnset = FALSE;
  } else {
    if (L < refreshL)
      refreshL = L;
    if (R > refreshR)
      refreshR = R;
    if (T < refreshT)
      refreshT = T;
    if (B > refreshB)
      refreshB = B;
  }

  drawCachedInBitmap = FALSE;
}

void wxMediaEdit::NeedsUpdate(wxSnip *snip, double localx, double localy, 
			      double w, double h)
{
  double x, y;

  if (!GetSnipLocation(snip, &x, &y))
    return;

  RefreshBox(x + localx, y + localy, w, h);

  if (!delayRefresh)
    Redraw();
}

void wxMediaEdit::InvalidateBitmapCache(double x, double y, double w, double h)
{
  if (w < 0)
    w = totalWidth - x;
  if (h < 0)
    h = totalHeight - y;

  RefreshBox(x, y, w, h);
  if (!delayRefresh)
    Redraw();
}

void wxMediaEdit::HideCaret(Bool hide)
{
  if (!!hiliteOn == !!hide) {
    hiliteOn = !hide;
    if (ownCaret || (startpos != endpos))
      NeedCaretRefresh();
  }
}

Bool wxMediaEdit::CaretHidden(void)
{
  return !hiliteOn;
}

double wxMediaEdit::GetBetweenThreshold()
{
  return betweenThreshold;
}

void wxMediaEdit::SetBetweenThreshold(double t)
{
  if (t > 99.0)
    t = 99.0;
  betweenThreshold = t;
}

/****************************************************************/

#ifdef MEMORY_USE_METHOD
long wxMediaEdit::MemoryUse(void)
{
  return ((numValidLines * sizeof(wxMediaLine))
	  + (tabcount * sizeof(double))
	  + wxMediaBuffer::MemoryUse());
}
#endif

