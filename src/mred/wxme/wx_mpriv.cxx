/*
 * File:        wx_mpriv.cc
 * Purpose:     wxMediaEdit private methods implementation
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
#define Uses_wxPrintSetup /* for wx_xt */
#define Uses_XLib /* for getting black pixel value */
#include "common.h"
#include "wx_dialg.h"
#ifndef OLD_WXWINDOWS
# include "wx_cmdlg.h"
#endif
#include "wx_utils.h"
#include "wx_dcps.h"
#include "wx_media.h"
#ifdef wx_xt
# include "wx_types.h"
#else
# include "wx_main.h"
#endif

#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>

#include "wx_mpriv.h"

#include "wx_gcrct.h"

/* Debugging: */
#define CHECK_CONSISTENCY 0
#define LOOK_FOR_ZEROED 0
#define PLOT_SNIP_DOT 0

#define A_VERY_BIG_NUMBER 1e50

/****************************************************************/
/*                             PRIVATE                          */
/****************************************************************/

void wxMediaEdit::_SetPosition(Bool setflash, int bias, long start, long end, 
			       Bool ateol, Bool scroll, int seltype)
{
  long oldstart, oldend, sPos;
  Bool oldateol;
  wxSnip *snip;
  Bool needRefresh, needFullRefresh, changedPos;

  if (flowLocked)
    return;
  
  if (!setflash && (!flash || !flashautoreset || !flashdirectoff))
    EndStreaks(wxSTREAK_EXCEPT_DELAYED);
  
  if ((start < 0) 
      || ((end != -1) && (start > end)))
    return;

  if (end == -1)
    end = start;
  else if (end > len)
    end = len;

  if (start > len)
    start = len;

  if (ateol) {
    if (start != end)
      ateol = FALSE;
    else {
      snip = FindSnip(start, -1, &sPos);
      if (!(snip->flags & wxSNIP_NEWLINE) 
	  || (snip->flags & wxSNIP_INVISIBLE) 
	  || start != sPos + snip->count)
	ateol = FALSE;
    }
  }

  if (flash) {
    oldstart = flashstartpos;
    oldend = flashendpos;
    oldateol = flashposateol;
  } else {
    oldstart = startpos;
    oldend = endpos;
    oldateol = posateol;
  }
  
  if (!setflash && flash && flashautoreset) {
    flash = FALSE;
    if (flashTimer) {
      flashTimer->Stop();
      DELETE_OBJ flashTimer;
      flashTimer = NULL;
    }
  }

  if (start == oldstart && end == oldend && ateol == oldateol)
    needRefresh = changedPos = FALSE;
  else {
    needRefresh = changedPos = TRUE;

    if (setflash) {
      flashstartpos = start;
      flashendpos = end;
      flashposateol = ateol;
    } else {
#if ALLOW_X_STYLE_SELECTION
      if (start == end || wxMediaXSelectionAllowed != this 
	  || (seltype == wxLOCAL_SELECT)) {
	if (!delayRefresh || needXCopy) {
	  needXCopy = FALSE;
	  CopyOutXSelection();
	}
      }
#endif

      CheckMergeSnips(startpos);
      CheckMergeSnips(endpos);
    
      caretStyle = NULL;
      startpos = start;
      endpos = end;
      posateol = ateol;
    }
  }

  needFullRefresh = FALSE;
#if ALLOW_X_STYLE_SELECTION
  if (!setflash && wxMediaXSelectionMode) {
    if ((seltype != wxLOCAL_SELECT) 
	&& start != end 
	&& this != wxMediaXSelectionOwner) {
      if (OwnXSelection(TRUE, FALSE, seltype == wxX_SELECT)) {
	needFullRefresh = TRUE;
	needRefresh = TRUE;
      }
    } else if ((start == end 
		|| wxMediaXSelectionAllowed != this 
		|| (seltype == wxLOCAL_SELECT))
	       && this == wxMediaXSelectionOwner) {
      if (OwnXSelection(FALSE, FALSE, FALSE)) {
	needFullRefresh = TRUE;
	needRefresh = TRUE;
      }
    }
  }
#endif

  if (setflash)
    flash = TRUE;

  if (scroll) {
    long scrollStart, scrollEnd;

    if (bias < -1) {
      scrollStart = scrollEnd = start;
      bias = 0;
    } else if (bias > 1) {
      scrollStart = scrollEnd = end;
      bias = 0;
    } else {
      scrollStart = start;
      scrollEnd = end;
    }
    
    {
      int wasBlinked = caretBlinked;
      caretBlinked = FALSE;
      if (ScrollToPosition(scrollStart, posateol, TRUE, scrollEnd, bias))
	needRefresh = FALSE;
      else
	caretBlinked = wasBlinked;
    }
  }

  if (needRefresh) {
    if (needRefresh) {
      caretBlinked = FALSE;
      if (start >= oldend || end <= oldstart || needFullRefresh) {
	/* No overlap: */
	NeedRefresh(oldstart, oldend);
	NeedRefresh(start, end);
      } else {
	if (start < oldstart)
	  NeedRefresh(start, oldstart);
	if (oldstart < start)
	  NeedRefresh(oldstart, start);
	if (end < oldend)
	  NeedRefresh(end, oldend);
	if (oldend < end)
	  NeedRefresh(oldend, end);
      }
    }
  }

  if (changedPos && !setflash)
    AfterSetPosition();
}

void wxMediaFlashTimer::Notify(void)
{
  media->FlashOff();
}

/**********************************************************************/

void wxMediaEdit::_ChangeStyle(long start, long end, 
			       wxStyle *newStyle, wxStyleDelta *delta,
			       Bool restoreSel, Bool counts_as_mod)
{
  wxSnip *gsnip, *startSnip, *endSnip;
  wxStyleChangeRecord *rec;
  wxStyle *style, *style2, *prev_style;
  long p, prev_style_pos, extra_check_pos = 0;
  int something;

  if (writeLocked || userLocked)
    return;

  if (newStyle && (styleList->StyleToIndex(newStyle) < 0))
    return;

  if (start < 0)
    start = 0;
  if (start > len)
    start = len;
  if (end > len)
    end = len;
  if (start > end)
    return;

  if (!newStyle && !delta) {
    newStyle = GetDefaultStyle();
    if (!newStyle)
      newStyle = styleList->BasicStyle();
  }

  if ((startpos == start) && (endpos == end) && (start == end) && len) {
    if (stickyStyles) {
      if (newStyle) 
	caretStyle = newStyle;
      else {
	/* handle delta */
	if (caretStyle) {
	  caretStyle = styleList->FindOrCreateStyle(caretStyle, delta);
	} else {
	  gsnip = FindSnip(start, -1);
	  caretStyle = styleList->FindOrCreateStyle(gsnip->style, delta);
	}
      }
    }
    return;
  }

  writeLocked = TRUE;

  if (!CanChangeStyle(start, end - start))
    goto give_up;
  OnChangeStyle(start, end - start);
  
  flowLocked = TRUE;

  MakeSnipset(start, end);
  
  if (!len) {
    startSnip = snips;
    endSnip = NULL;
    initialStyleNeeded = 0;
  } else {
    startSnip = FindSnip(start, +1);
    endSnip = FindSnip(end, +2);
  }

  if (!noundomode)
    rec = new WXGC_PTRS wxStyleChangeRecord(start, end, delayedStreak || !modified, startpos, endpos, restoreSel);
  else
    rec = NULL;

  something = FALSE;

  prev_style = NULL;
  prev_style_pos = start;
  p = start;
  for (gsnip = startSnip; PTRNE(gsnip, endSnip); gsnip = gsnip->next) {
    style = gsnip->style;

    if (newStyle)
      style2 = newStyle;
    else
      style2 = styleList->FindOrCreateStyle(style, delta);

    if (PTRNE(style, style2)) {
      gsnip->style = style2;

      if (rec && (style != prev_style)) {
        if (prev_style) {
          rec->AddStyleChange(prev_style_pos, p, prev_style);
        }
	prev_style_pos = p;
	prev_style = style;
      }

      gsnip->SizeCacheInvalid();
      gsnip->line->MarkRecalculate();
      if (maxWidth > 0)
	gsnip->line->MarkCheckFlow();

      extra_check_pos = p;

      something = TRUE;
    } else if (rec && prev_style) {
      rec->AddStyleChange(prev_style_pos, p, prev_style);
      prev_style = NULL;
    }

    p += gsnip->count;
  }

  if (rec && prev_style) {
    rec->AddStyleChange(prev_style_pos, p, prev_style);
  }

  if (something) {
    if (startSnip->line->prev
	&& !(startSnip->line->prev->lastSnip->flags & wxSNIP_HARD_NEWLINE))
      startSnip->line->prev->MarkCheckFlow();
    
    if (!modified) {
      wxUnmodifyRecord *ur;
      ur = new WXGC_PTRS wxUnmodifyRecord(delayedStreak);
      AddUndo(ur);
    }
    if (rec)
      AddUndo(rec);
    if (delayRefresh)
      delayedStreak = TRUE;
    
    CheckMergeSnips(start);
    if (extra_check_pos)
      CheckMergeSnips(extra_check_pos);
    if (extra_check_pos != end)
      CheckMergeSnips(end);
    
    if (!modified && counts_as_mod)
      SetModified(TRUE);

    writeLocked = FALSE;
    flowLocked = FALSE;

    RefreshByLineDemand();
  } else {
    if (rec)
      DELETE_OBJ rec;
    writeLocked = FALSE;
    flowLocked = FALSE;

    CheckMergeSnips(start);
    CheckMergeSnips(end);
  }

  AfterChangeStyle(start, end - start);

  return;

 give_up:
  writeLocked = FALSE;
  flowLocked = FALSE;

  return;
}

/****************************************************************/

void wxMediaEdit::SettingAdmin(wxMediaAdmin * /* newadmin */)
{
}

void wxMediaEdit::InitNewAdmin(void)
{
  if (!delayRefresh && (!admin || !admin->DelayRefresh()))
    Redraw();
}

void wxMediaEdit::EndStreaks(int exception)
{
  if (map && !(exception & wxSTREAK_EXCEPT_KEY_SEQUENCE) && !streaksPushed)
    map->BreakSequence();
  if (flash && flashautoreset && !flashdirectoff)
    FlashOff();

  typingStreak = FALSE;
  deletionStreak = FALSE;
  if (!(exception & wxSTREAK_EXCEPT_CURSOR)) {
    vcursorStreak = FALSE;
    extendStreak = FALSE;
  }
  if (anchorStreak && !keepAnchorStreak)
    SetAnchor(FALSE);

  if (!(exception & wxSTREAK_EXCEPT_DELAYED))
    delayedStreak = FALSE;

  killStreak = FALSE;

  prevPasteStart = -1;
}

void wxMediaEdit::PushStreaks(void)
{
  streaksPushed = TRUE;
  saveTypingStreak = typingStreak;
  saveDeletionStreak = deletionStreak;
  saveDelayedStreak = delayedStreak;
  saveVcursorStreak = vcursorStreak;
  saveKillStreak = killStreak;
  saveAnchorStreak = anchorStreak;
  saveExtendStreak = extendStreak;
  savePrevPasteStart = prevPasteStart;
  savePrevPasteEnd = prevPasteEnd;
}

void wxMediaEdit::PopStreaks(void)
{
  if (streaksPushed) {
    streaksPushed = FALSE;
    typingStreak = saveTypingStreak;
    deletionStreak = saveDeletionStreak;
    delayedStreak = saveDelayedStreak;
    vcursorStreak = saveVcursorStreak;
    killStreak = saveKillStreak;
    anchorStreak = saveAnchorStreak;
    extendStreak = saveExtendStreak;
    prevPasteStart = savePrevPasteStart;
    prevPasteEnd = savePrevPasteEnd;
  }
}

/****************************************************************/

long wxMediaEdit::_FindPositionInLine(Bool internal, long i, double x, 
				      Bool *ateol, Bool *onit,
				      double *how_close)
{
  double w, X, topy;
  wxSnip *snip;
  wxDC *dc;
  long p, sPos;
  wxMediaLine *line;
  Bool atsnipend;
  Bool wl, fl;

  if (onit)
    *onit = FALSE;
  if (how_close)
    *how_close = 100;

  if (!internal && !CheckRecalc(TRUE, FALSE))
    return 0;

  if (i < 0)
    return 0;
  if (i >= numValidLines)
    return len;

  line = lineRoot->FindLine(i);

  x -= line->GetLeftLocation(maxWidth);

  if (ateol)
    *ateol = FALSE;

  if (x <= 0)
    return FindFirstVisiblePosition(line);

  p = line->GetPosition();

  if (x >= line->w) {
    /* snip == the last one */
    snip = line->lastSnip;
    sPos = p + line->len - snip->count;
    p += line->len;
  } else {
    if (onit)
      *onit = TRUE;

    dc = admin->GetDC();
    if (!dc)
      return 0;
    
    X = 0;

    wl = writeLocked;
    fl = flowLocked;
    writeLocked = TRUE;
    flowLocked = TRUE;

    /* linear seach for snip */
    snip = NULL;
    topy = line->GetLocation();
    while(1) {
      snip = snip ? snip->next : line->snip;

      w = 0.0;
      snip->GetExtent(dc, X, topy, &w);

      if (x > w && snip->next) {
	x -= w;
	X += w;
	p += snip->count;
      } else {
	/* Found the right snip */
	sPos = p;
	p += _FindPositionInSnip(dc, X, topy, snip, x, how_close);
	break;
      }
    }

    writeLocked = wl;
    flowLocked = fl;
  }

  /* Back up over invisibles */
  atsnipend = (p - sPos == snip->count);
  if (atsnipend)
    FindLastVisiblePosition(line, &p, &snip);

  if (ateol && atsnipend && snip && PTREQ(snip, line->lastSnip))
    *ateol = TRUE;

  return p;
}

long wxMediaEdit::FindFirstVisiblePosition(wxMediaLine *line, wxSnip *snip)
{
  wxSnip *nextSnip;
  long p, startp;

  if (readLocked)
    return 0;

  if (!snip)
    snip = line->snip;

  startp = line->GetPosition();
  p = startp;
  nextSnip = line->lastSnip->next;

  while (PTRNE(snip, nextSnip)) {
    if (snip->flags & wxSNIP_INVISIBLE)
      p += snip->count;
    else
      break;
    snip = snip->next;
  }

  if (PTREQ(snip, nextSnip)) {
    /* If everything is invisible, then presumably the CR is forced,
       so go to the beginning of the line anyway */
    p = startp;
  }

  return p;
}

void wxMediaEdit::FindLastVisiblePosition(wxMediaLine *line, long *p, 
					  wxSnip **snipP)
{
  wxSnip *snip;
  
  if (readLocked)
    return;
  
  snip = snipP ? *snipP : (wxSnip *)NULL;

  if (!snip)
    snip = line->lastSnip;

  do {
    if (snip->flags & wxSNIP_INVISIBLE) {
      *p -= snip->count;
      if (PTRNE(snip, line->snip))
	snip = snip->prev;
    }
  } while ((snip->flags & wxSNIP_INVISIBLE) && PTRNE(snip, line->snip));

  if (snipP)
    *snipP = snip;
}

/****************************************************************/

long wxMediaEdit::_FindStringAll(wxchar *str, int direction, 
				 long start, long end,
				 long **positions, Bool justOne,
				 Bool bos, Bool caseSens)
{
  wxSnip *snip;
  wxchar text[256], *oldStr, c;
  long *smap;
  long sPos, p, n, thistime, thisoffset, need, checked, offset, shorten, i;
  long slen, s, sbase, beyond, sgoal, totalCount, allocFound, foundCount;

  if (!direction)
    direction = 1;
  if (direction < -1)
    direction = -1;
  else if (direction > 1)
    direction = 1;

  if (start < 0)
    start = startpos;
  if (end < 0) {
    if (direction < 0)
      end = 0;
    else
      end = len;
  }
  if (start > len)
    start = len;
  if (end > len)
    end = len;

  if (direction < 0)
    totalCount = start - end;
  else
    totalCount = end - start;
  if (totalCount < 0)
    return -1;

  slen = wxstrlen(str);
  if (!slen)
    return -1;

  if (!caseSens) {
    oldStr = str;
    str = new WXGC_ATOMIC wxchar[slen + 1];
    for (i = 0; i < slen; i++) {
      c = oldStr[i];
      str[i] = scheme_tofold(c);
    }
    str[i] = 0;
  }
  
  snip = FindSnip(start, direction, &sPos);
  if (!snip)
    return -1;

  if (direction > 0) {
    offset = start - sPos;
    shorten = 0;
    sbase = 0;
    beyond = -1;
    sgoal = slen;
  } else {
    shorten = (sPos + snip->count) - start;
    offset = 0;
    sbase = slen - 1;
    beyond = slen;
    sgoal = -1;
  }

  smap = new WXGC_ATOMIC long[slen];

  smap[sbase] = beyond;
  s = beyond;
  for (i = sbase + direction; i != sgoal; i += direction) {
    while ((s != beyond) && (str[s + direction] != str[i])) {
      s = smap[s];
    }
    if (str[s + direction] == str[i])
      s += direction;
    smap[i] = s;
  }

  s = beyond;

  if (!justOne) {
    long *naya;
    allocFound = 10;
    naya = new WXGC_ATOMIC long[allocFound];
    *positions = naya;
    foundCount = 0;
  } else
    allocFound = foundCount = 0;

  while (snip && totalCount) {
    need = snip->count - shorten - offset;
    if (need > totalCount) {
      if (direction < 0)
	offset += (need - totalCount);
      need = totalCount;
    }

    checked = 0;

    totalCount -= need;

    do {
      Bool wl, fl;

      thistime = need;
      if (thistime > 255)
	thistime = 255;
      need -= thistime;

      thisoffset = offset + ((direction < 0) ? need : checked);

      wl = writeLocked, fl = flowLocked;
      writeLocked = TRUE;
      flowLocked = TRUE;
      
      snip->GetTextBang(text, thisoffset, thistime, 0);

      writeLocked = wl;
      flowLocked = fl;

      text[thistime] = 0;
      
      i = (direction > 0) ? 0 : thistime - 1;
      n = thistime;
      while(n--) {
	c = text[i];
	if (!caseSens)
	  c = scheme_tofold(c);
	while ((s != beyond) && (str[s + direction] != c)) {
	  s = smap[s];
	}
	if (str[s + direction] == c) {
	  s += direction;
	  if (s + direction == sgoal) {
	    p = sPos + i + thisoffset;
	    if (bos) {
	      if (direction < 0)
		p += slen;
	      else
		p -= (slen - 1);
	    } else if (direction > 0)
	      p++;
	    if (justOne)
	      goto search_done;
	    else {
	      if (foundCount == allocFound) {
		long *old = *positions, *naya, oldCount = allocFound;

		allocFound *= 2;
		naya = new WXGC_ATOMIC long[allocFound];
		*positions = naya;
		
		memcpy(*positions, old, oldCount * sizeof(long));
	      }
	      (*positions)[foundCount++] = p;
	      s = beyond;
	    }
	  }
	}
	i += direction;
      }

      checked += thistime;
    } while(need);

    if (direction > 0)
      sPos += snip->count;
    snip = (direction > 0) ? snip->next : snip->prev;
    if (snip) {
      if (direction < 0)
	sPos -= snip->count;
    }
    offset = shorten = 0;
  }

  p = -1;

 search_done:
  return justOne ? p : foundCount;
}

/****************************************************************/

void wxMediaEdit::MakeOnlySnip(void)
{
  wxMediaLine *line;

  snips = new WXGC_PTRS wxTextSnip();
  snips->style = GetDefaultStyle();
  if (!snips->style) {
    snips->style = styleList->BasicStyle();
  }
#if CHECK_CONSISTENCY
  if (!snips->style)
    fprintf(stderr, "NULL style for basic style!\n");
#endif
  snips->count = 0;
  snips->SetAdmin(snipAdmin);
  snips->prev = NULL;
  snips->next = NULL;

  line = new WXGC_PTRS wxMediaLine;
  snips->line = lineRoot = firstLine = lastLine = line;
  lineRoot->SetStartsParagraph(TRUE);

  lineRoot->snip = lineRoot->lastSnip = snips;

  lastSnip = snips;
  snipCount = 1;

  numValidLines = 1;
}

void wxMediaEdit::SpliceSnip(wxSnip *snip, wxSnip *prev, wxSnip *next)
{
  if (prev)
    prev->next = snip;
  else
    snips = snip;
  snip->prev = prev;
  snip->next = next;
  if (next)
    next->prev = snip;
  else
    lastSnip = snip;
}

void wxMediaEdit::InsertSnip(wxSnip *before, wxSnip *snip)
{
  if (PTREQ(snips, lastSnip) && !snips->count) {
    AppendSnip(snip);
  } else {
    SpliceSnip(snip, before->prev, before);
    snipCount++;
  }
}

void wxMediaEdit::AppendSnip(wxSnip *snip)
{
  if (PTREQ(snips, lastSnip) && !snips->count) {
    /* Get rid of empty snip */
    DELETE_OBJ snips;
    snips = lastSnip = snip;
  } else {
    SpliceSnip(snip, lastSnip, NULL);
    snipCount++;
  }
}

void wxMediaEdit::DeleteSnip(wxSnip *snip)
{
  if (snip->next)
    SpliceSnip(snip->next, snip->prev, snip->next->next);
  else if (snip->prev)
    SpliceSnip(snip->prev, snip->prev->prev, snip->next);
  else {
    lastSnip = snips = NULL;
  }
  --snipCount;
  snip->flags += wxSNIP_CAN_DISOWN;
  SnipSetAdmin(snip, NULL);
  snip->line = NULL;
  snip->prev = snip->next = NULL;
  snip->flags -= wxSNIP_CAN_DISOWN;
}

wxSnip *wxMediaEdit::SnipSetAdmin(wxSnip *snip, wxSnipAdmin *a)
{
  wxSnipAdmin *orig_admin;
  long orig_count = snip->count;
  wxMediaLine *line = snip->line;
  Bool wl = writeLocked, fl = flowLocked;

  orig_admin = snip->GetAdmin();

  readLocked = writeLocked = flowLocked = TRUE;
  snip->SetAdmin(a);
  readLocked = FALSE; writeLocked = wl; flowLocked = fl;

  if (snip->GetAdmin() != a) {
    /* Something went wrong. */
    if (!a && (snip->GetAdmin() == orig_admin)) {
      /* Force admin to NULL. */
      snip->wxSnip::SetAdmin(NULL);
    } else if (a) {
      /* Snip didn't accept membership into this buffer. Give up on it. */
      wxSnip *naya;
      naya = new WXGC_PTRS wxSnip();

      naya->count = orig_count;
      SpliceSnip(naya, snip->prev, snip->next);
      naya->line = line;

      if (line) {
	if (line->snip == snip)
	  line->snip = naya;
	if (line->lastSnip == snip)
	  line->lastSnip = naya;
      }

      snip->wxSnip::SetAdmin(NULL);

      naya->SetAdmin(a);
      snip = naya;
    }
  }

  /* Force count to be consistent: */
  if (a && (snip->count != orig_count))
    snip->count = orig_count;

  return snip;
}

void wxMediaEdit::SnipSplit(wxSnip *snip, long pos, wxSnip **a_ptr, wxSnip **b_ptr)
{
  int c = snip->count, nl = (snip->flags & wxSNIP_NEWLINE), hnl = (snip->flags & wxSNIP_HARD_NEWLINE);
  wxSnip *a, *b;
  Bool wl, fl;
  wxSnip *orig;

  snip->flags |= wxSNIP_CAN_SPLIT;
  orig = snip;

  DeleteSnip(snip);
  orig->flags -= wxSNIP_OWNED;

  revision_count++;

  wl = writeLocked, fl = flowLocked;
  readLocked = writeLocked = flowLocked = TRUE;
  *a_ptr = NULL;
  *b_ptr = NULL;
  snip->Split(pos, a_ptr, b_ptr);
  readLocked = FALSE; writeLocked = wl; flowLocked = fl;

  a = *a_ptr;
  b = *b_ptr;

  if (!a)
    a = new WXGC_PTRS wxSnip();
  if (!b)
    b = new WXGC_PTRS wxSnip();

  if (a->IsOwned()) {
    /* uh-oh: make up a dummy */
     a = new WXGC_PTRS wxSnip();
  }
  if (b->IsOwned()) {
    /* uh-oh: make up a dummy */
     b = new WXGC_PTRS wxSnip();
  }

  *a_ptr = a;
  *b_ptr = b;

  if (a->flags & wxSNIP_CAN_SPLIT)
    a->flags -= wxSNIP_CAN_SPLIT;
  if (b->flags & wxSNIP_CAN_SPLIT)
    b->flags -= wxSNIP_CAN_SPLIT;
  if (orig->flags & wxSNIP_CAN_SPLIT)
    orig->flags -= wxSNIP_CAN_SPLIT;

  /* Make *sure* that count is right */
  a->count = pos;
  b->count = c - pos;

  /* Make sure that NEWLINE & HARD_NEWLINE is consistent: */
  if (nl)
    b->flags |= wxSNIP_NEWLINE;
  if (hnl)
    b->flags |= wxSNIP_HARD_NEWLINE;
  if (a->flags & wxSNIP_NEWLINE)
    a->flags -= wxSNIP_NEWLINE;  
  if (a->flags & wxSNIP_HARD_NEWLINE)
    a->flags -= wxSNIP_HARD_NEWLINE;
}

/****************************************************************/

wxSnip *wxMediaEdit::FindFirstSnip(void)
{
  if (!len)
    return NULL;
  else
    return snips;
}

wxSnip *wxMediaEdit::FindSnip(long p, int direction, long *sPos)
{
  wxSnip *snip;
  wxMediaLine *line;
  long pos;

  if ((direction < -1) && !p)
    return NULL;

  line = lineRoot->FindPosition(p);
  pos = line->GetPosition();
  p -= pos;
  if (sPos)
    *sPos = pos;

  snip = line->snip;
  if (!p && snip->prev) {
    /* Back up one: */
    snip = snip->prev;
    p += snip->count;
    if (sPos)
      *sPos -= snip->count;
  }

  for (; snip; snip = snip->next) {
    p -= snip->count;
    if ((!direction && !p)
	|| ((direction < 0) && (p <= 0))
	|| ((direction > 0) && (p < 0)))
      return snip;

    if (!direction && (p < 0))
      return NULL;

    if (sPos)
      *sPos += snip->count;
  }

  if (direction < 2)
    return lastSnip;
  else
    return NULL;
}

wxSnip *wxMediaEdit::FindNextNonTextSnip(wxSnip *snip)
{
  if (snip) {
    if (snip->GetAdmin() != snipAdmin)
      return NULL;
    snip = snip->next;
  } else if (!len)
    return NULL;
  else
    snip = snips;
  
  while (snip && ((snip->__type == wxTYPE_TEXT_SNIP)
		  || (snip->__type == wxTYPE_TAB_SNIP))) {
    snip = snip->next;
  }

  return snip;
}

void wxMediaEdit::MakeSnipset(long start, long end) 
{
  long sPos;
  wxSnip *prev, *next, *snip, *insSnip;
  wxStyle *style;
  wxMediaLine *line;
  Bool atStart, atEnd;

  if (start) {
    snip = FindSnip(start, +1, &sPos);
    if (start != sPos) {
      line = snip->line;
      prev = snip->prev; next = snip->next;
      style = snip->style;
      
      atStart = PTREQ(line->snip, snip);
      atEnd = PTREQ(line->lastSnip, snip);
      
      SnipSplit(snip, start - sPos, &insSnip, &snip);
      
      snip->style = insSnip->style = style;
      
      snip->line = insSnip->line = line;
      if (atStart)
	line->snip = insSnip;
      if (atEnd)
	line->lastSnip = snip;
      
      SpliceSnip(snip, prev, next);
      snipCount++;
      InsertSnip(snip, insSnip);
      
      SnipSetAdmin(snip, snipAdmin);
      SnipSetAdmin(insSnip, snipAdmin);

      OnSplitSnip(start - sPos);
    }
  }

  if (end) {
    snip = FindSnip(end, -1, &sPos);
    if (end != sPos + snip->count) {
      line = snip->line;
      prev = snip->prev; next = snip->next;
      style = snip->style;
      
      atStart = PTREQ(line->snip, snip);
      atEnd = PTREQ(line->lastSnip, snip);
      
      SnipSplit(snip, end - sPos, &insSnip, &snip);
      
      snip->style = insSnip->style = style;
      
      snip->line = insSnip->line = line;
      if (atStart)
	line->snip = insSnip;
      if (atEnd)
	line->lastSnip = snip;
      
      SpliceSnip(snip, prev, next);
      snipCount++;
      InsertSnip(snip, insSnip);
      
      SnipSetAdmin(snip, snipAdmin);
      SnipSetAdmin(insSnip, snipAdmin);

      OnSplitSnip(end - sPos);
    }
  }
}

wxTextSnip *wxMediaEdit::InsertTextSnip(long start, wxStyle *style)
{
  long sPos;
  wxSnip *gsnip, *insGsnip, *prev, *next;
  wxTextSnip *snip;
  wxStyle *gstyle;
  wxMediaLine *line;
  Bool atStart, atEnd;
  wxSnip *rsnip;
 
  snip = OnNewTextSnip();
  if (snip->IsOwned() || snip->count) {
    /* Uh-oh. Resort to wxTextSnip() */
    snip = new WXGC_PTRS wxTextSnip();
  }
  {
    wxStyle *styl;
    styl = (style ? style : GetDefaultStyle());
    snip->style = styl;
  }
  if (!snip->style) {
    snip->style = styleList->BasicStyle();
  }
  rsnip = SnipSetAdmin(snip, snipAdmin);
  if (rsnip != snip) {
    /* Uh-oh. Resort to wxTextSnip() */
    wxStyle *styl;
    snip = new WXGC_PTRS wxTextSnip();
    styl = (style ? style : GetDefaultStyle());
    snip->style = styl;
    if (!snip->style) {
      snip->style = styleList->BasicStyle();
    }
    snip->SetAdmin(snipAdmin);
  }
  snip->count = 0;

  gsnip = FindSnip(start, -2, &sPos);
  if (gsnip
      && (gsnip->count + sPos == start)
      && (gsnip->flags & wxSNIP_NEWLINE)
      && !(gsnip->flags & wxSNIP_HARD_NEWLINE)) {
    /* We want the snip on the same line as the preceeding snip: */
    if (gsnip->next)
      InsertSnip(gsnip->next, snip);
    else
      AppendSnip(snip);
    gsnip->flags -= wxSNIP_NEWLINE;
    snip->flags |= wxSNIP_NEWLINE;
    snip->line = gsnip->line;
    snip->line->lastSnip = snip;
  } else {
    gsnip = FindSnip(start, +2, &sPos);
    if (!gsnip) {
      AppendSnip(snip);
      snip->line = lastLine;
      if (PTREQ(lastLine->snip, lastSnip))
	lastLine->snip = lastLine->lastSnip = snip;
      else
	lastLine->lastSnip = snip;
    } else if (start == sPos) {
      InsertSnip(gsnip, snip);
      snip->line = gsnip->line;
      if (PTREQ(snip->line->snip, gsnip))
	snip->line->snip = snip;
    } else {
      prev = gsnip->prev; next = gsnip->next;
      gstyle = gsnip->style;
      line = gsnip->line;
      
      atStart = PTREQ(line->snip, gsnip);
      atEnd = PTREQ(line->lastSnip, gsnip);
      
      SnipSplit(gsnip, start - sPos, &insGsnip, &gsnip);
      
      gsnip->style = insGsnip->style = gstyle;
      
      gsnip->line = insGsnip->line = snip->line = line;
      if (atStart)
	line->snip = insGsnip;
      if (atEnd)
	line->lastSnip = gsnip;
      
      SpliceSnip(gsnip, prev, next);
      snipCount++;
      InsertSnip(gsnip, snip);
      InsertSnip(snip, insGsnip);
      
      SnipSetAdmin(gsnip, snipAdmin);
      SnipSetAdmin(insGsnip,snipAdmin);

      OnSplitSnip(start - sPos);
    }
  }

  return snip;
}

void wxMediaEdit::CheckMergeSnips(long start)
{
  wxSnip *snip1, *snip2, *prev, *next;
  long sPos1, sPos2, c;
  int did_something = 0;
  wxMediaLine *line;
  Bool atStart, atEnd;

 restart:

  snip1 = FindSnip(start, -1, &sPos1);
  snip2 = FindSnip(start, +1, &sPos2);

  if (PTRNE(snip1, snip2)) {
    if (snip1->snipclass 
#if USE_OLD_TYPE_SYSTEM
	&& (snip1->__type == snip2->__type)
#endif
	&& PTREQ(snip1->snipclass, snip2->snipclass)
	&& PTREQ(snip1->style, snip2->style)) {
      if (!(snip1->flags & wxSNIP_NEWLINE)
	  && (snip1->flags & wxSNIP_CAN_APPEND) 
	  && (snip2->flags & wxSNIP_CAN_APPEND)
	  && (snip1->count + snip2->count < MAX_COUNT_FOR_SNIP)
	  && PTREQ(snip1->line, snip2->line)) {
	did_something = 1;
	if (!snip1->count) {
	  if (PTREQ(snip1->line->snip, snip1))
	    snip1->line->snip = snip2;
	  DeleteSnip(snip1);
	  snip1->flags -= wxSNIP_OWNED;
	  goto restart;
	} else if (!snip2->count) {
	  if (PTREQ(snip1->line->lastSnip, snip2)) {
	    snip1->line->lastSnip = snip1;
	    snip1->line->MarkRecalculate(); // need lastW updated
	    graphicMaybeInvalid = TRUE;
	  }
	  DeleteSnip(snip2);
	  snip2->flags -= wxSNIP_OWNED;
	  goto restart;
	} else {
	  wxSnip *naya;
	  Bool wl, fl;

	  c = snip1->count + snip2->count;
	  prev = snip1->prev;
	  next = snip2->next;
	  line = snip1->line;
	  atStart = PTREQ(line->snip, snip1);
	  atEnd = PTREQ(line->lastSnip, snip2);
	  snip2->flags |= wxSNIP_CAN_SPLIT;

	  wl = writeLocked, fl = flowLocked;
	  readLocked = writeLocked = flowLocked = TRUE;
	  naya = snip2->MergeWith(snip1);
	  readLocked = FALSE; writeLocked = wl; flowLocked = fl;

	  if (naya) {
	    if (snip1->flags & wxSNIP_CAN_SPLIT)
	      snip1->flags -= wxSNIP_CAN_SPLIT;
	    if (snip2->flags & wxSNIP_CAN_SPLIT)
	      snip2->flags -= wxSNIP_CAN_SPLIT;

	    /* Claim snip1 & snip2 unowned for naya test: */
	    snip1->flags -= wxSNIP_OWNED;
	    snip2->flags -= wxSNIP_OWNED;
	    if (naya->IsOwned()) {
	      /* Uh-oh. Make dummy */
	      naya = new WXGC_PTRS wxSnip();
	    }
	    if (naya->flags & wxSNIP_CAN_SPLIT)
	      naya->flags -= wxSNIP_CAN_SPLIT;
	    snip1->flags += wxSNIP_OWNED;
	    snip2->flags += wxSNIP_OWNED;

	    DeleteSnip(snip1);
	    snip1->flags -= wxSNIP_OWNED;
	    DeleteSnip(snip2);
	    snip2->flags -= wxSNIP_OWNED;

	    SpliceSnip(naya, prev, next);
	    snipCount++;
	    /* Make *sure* that count is right */
	    naya->count = c;

	    revision_count++;

	    naya = SnipSetAdmin(naya, snipAdmin);

	    naya->line = line;
	    if (atStart)
	      line->snip = naya;
	    if (atEnd) {
	      line->lastSnip = naya;
	      line->MarkRecalculate(); // need lastW updated
	      graphicMaybeInvalid = TRUE;
	    }
	  } else if (snip2->flags & wxSNIP_CAN_SPLIT)
	    snip2->flags -= wxSNIP_CAN_SPLIT;
	}
      }
    }
  }

  if (did_something)
    OnMergeSnips(start);
}

wxTextSnip *wxMediaEdit::OnNewTextSnip()
{
  return new WXGC_PTRS wxTextSnip();
}

wxTabSnip *wxMediaEdit::OnNewTabSnip()
{
  return new WXGC_PTRS wxTabSnip();
}

/****************************************************************/

Bool wxMediaEdit::GetSnipPositionAndLocation(wxSnip *thesnip, long *pos, 
					     double *x, double *y)
{
  wxSnip *snip;
  long p;

  if (!CheckRecalc(x || y, FALSE))
    return FALSE;

  if (!thesnip->line || PTRNE(thesnip->line->GetRoot(), lineRoot))
    return FALSE;

  if (pos || x || y) {
    p = thesnip->line->GetPosition();
    
    for (snip = thesnip->line->snip; PTRNE(snip, thesnip); snip = snip->next) {
      p += snip->count;
    }
    
    if (pos)
      *pos = p;
    
    if (x || y) 
      PositionLocation(p, x, y);
  }

  return TRUE;
}

Bool wxMediaEdit::GetSnipLocation(wxSnip *thesnip, double *x, double *y, Bool bottomRight)
{
  double lx, ly;

  if (bottomRight) {
    if (!x)
      x = &lx;
    if (!y)
      y = &ly;
  }

  if (GetSnipPositionAndLocation(thesnip, NULL, x, y)) {
    if (bottomRight) {
      wxDC *dc;
      double w, h;
      
      Bool wl = writeLocked, fl = flowLocked;
      writeLocked = TRUE;
      flowLocked = TRUE;
      
      dc = admin->GetDC();

      w = h = 0.0;
      thesnip->GetExtent(dc, *x, *y, &w, &h);

      writeLocked = wl;
      flowLocked = fl;

      *x += w;
      *y += h;
    }
     
    return TRUE;
  } else
    return FALSE;
}

long wxMediaEdit::GetSnipPosition(wxSnip *thesnip)
{
  long pos;

  if (GetSnipPositionAndLocation(thesnip, &pos))
    return pos;
  else
    return -1;
}

/****************************************************************/

void wxMediaEdit::AdjustClickbacks(long start, long end, 
				   long d, wxDeleteRecord *rec)
{
  wxNode *node, *next;
  wxClickback *click;
  Bool deleteit;

  if (!clickbacks)
    return;

  for (node = clickbacks->First(); node; node = next) {
    next = node->Next();
    click = (wxClickback *)node->Data();
    deleteit = FALSE;
    if (click->start >= start && click->end <= end) {
      deleteit = TRUE;
    } else if (click->start >= end) {
      click->start += d;
      click->end += d;
    } else if (click->start <= start && click->end >= end) {
      if ((d < 0) || (click->end > end))
	click->end += d;
    } else if (click->start > start && click->end > end) {
      click->start = start;
      click->end += d;
    }

    if (click->end == click->start)
      deleteit = TRUE;
    if (deleteit) {
      clickbacks->DeleteNode(node);
      if (rec)
	rec->AddClickback(click);
      else
	DELETE_OBJ click;
    }
  }
}

wxClickback *wxMediaEdit::FindClickback(long start, double y)
{
  wxNode *node;
  wxClickback *click;

  if (!clickbacks)
    return NULL;

  for (node = clickbacks->Last(); node; node = node->Previous()) {
    click = (wxClickback *)node->Data();
    if (click->start <= start && click->end > start) {
      /* We're in the right horizontal region, but maybe the mouse
	 is above or below the clickback. */
      wxSnip *start, *end;
      double top, bottom, dummy;

      start = FindSnip(click->start, 1);
      end = FindSnip(click->end, -1);

      if (start && end) {
	GetSnipLocation(start, &dummy, &top, FALSE);
	GetSnipLocation(start, &dummy, &bottom, TRUE);
	while (start != end) {
	  double ntop, nbottom;
	  start = start->Next();
	  GetSnipLocation(start, &dummy, &ntop, FALSE);
	  GetSnipLocation(start, &dummy, &nbottom, TRUE);
	  if (ntop < top)
	    top = ntop;
	  if (nbottom > bottom)
	    bottom = nbottom;
	}
      
	if ((y >= top) && (y <= bottom))
	  return click;
      }
    }
  }

  return NULL;
}

void wxMediaEdit::SetClickbackHilited(wxClickback *click, Bool on)
{
  if (on != click->hilited) {
    if (on) {
      interceptmode = TRUE;
      intercepted = new WXGC_PTRS wxList();
      
      BeginEditSequence();
      FlashOn(click->start, click->end, FALSE, FALSE, -1);
      _ChangeStyle(click->start, click->end, NULL, click->delta, 0);
      EndEditSequence();

      click->unhilite = intercepted;
      interceptmode = FALSE;
    } else {
      wxNode *node;

      PerformUndoList(click->unhilite);

      for (node = click->unhilite->First(); node; node = node->Next()) {
	wxChangeRecord *cr;
	cr = (wxChangeRecord *)node->Data();
	/* DELETE_OBJ cr; */
      }
  
      /* DELETE_OBJ click->unhilite; */
      FlashOff();
    }
    click->hilited = on;
  }
}

/****************************************************************/

Bool wxMediaEdit::CheckRecalc(Bool need_graphic, Bool need_write, Bool no_display_ok)
{
  if (readLocked)
    return FALSE;
  if (writeLocked && need_write)
    return FALSE;

  if (need_graphic) {
    if (!admin) {
      if (no_display_ok)
	return TRUE;
      return FALSE;
    }

    if (graphicMaybeInvalid) {
      wxDC *dc;

      if (flowLocked)
	return FALSE;

      dc = admin->GetDC();
      if (!dc) {
	if (no_display_ok)
	  return TRUE;
	return FALSE;
      }
      
      RecalcLines(dc, need_graphic);
    }
  }

  return TRUE;
}

Bool wxMediaEdit::CheckFlow(double maxw, wxDC *dc, double Y, 
			    long startp, wxSnip *start)
  /* This method is called with writeLocked and flowLocked already TRUE */
{
  long p, c, origc, b;
  double _totalWidth, w;
  wxSnip *snip;
  Bool checkingUnderflow; // no overflow => check move up from next line
  Bool checkingUnderflowAtNext;
  Bool noChangeIfEndOfSnip, noChangeIfStartOfSnip;
  Bool theFirstSnip, firstUnderflow, hadNewline;
  Bool deletedANewline;

  _totalWidth = 0;
  p = startp;
  checkingUnderflow = FALSE; // start by ensuring no overflow
  checkingUnderflowAtNext = FALSE;
  noChangeIfEndOfSnip = TRUE; // Because an immediate overflow can't be helped
  noChangeIfStartOfSnip = FALSE;
  theFirstSnip = TRUE;
  firstUnderflow = FALSE;
  deletedANewline = FALSE;

  for (snip = start; 
       snip && !(snip->flags & wxSNIP_HARD_NEWLINE); 
       snip = snip->next) {

    if (!checkingUnderflow) {
      checkingUnderflow = checkingUnderflowAtNext;
      if (checkingUnderflow)
	firstUnderflow = TRUE;
    }
    noChangeIfStartOfSnip = noChangeIfEndOfSnip;

    if (snip->flags & wxSNIP_NEWLINE) {
      noChangeIfEndOfSnip = !checkingUnderflow;
      snip->flags -= wxSNIP_NEWLINE;
      checkingUnderflowAtNext = TRUE;
      hadNewline = TRUE;
      deletedANewline = TRUE;
      /* Note: if the newline is restored, then the
	 return jumps directly out of the `for' loop. */
    } else {
      noChangeIfEndOfSnip = FALSE;
      checkingUnderflowAtNext = FALSE;
      hadNewline = FALSE;
    }

    {
      Scheme_Thread *thread;
      thread = scheme_get_current_thread();
      if (thread) SCHEME_USE_FUEL(1);
      thread = NULL;
    }

    w = 0.0;
    snip->GetExtent(dc, _totalWidth, Y, &w);
    _totalWidth += w;
    if (_totalWidth > maxw) {
      _totalWidth -= w;
      /* Get best breaking position: (0.1 is hopefully a positive value smaller than any character) */
      origc = _FindPositionInSnip(dc, _totalWidth, Y, snip, maxw - _totalWidth - 0.1);

      /* get legal breaking position before optimal: */
      b = p + origc + 1;
      FindWordbreak(&b, NULL, wxBREAK_FOR_LINE);
      c = b - p;
      if (c > origc)
	c = origc;

      if (c <= 0) {
	if ((b <= startp) && checkingUnderflow && origc) {
	  /* The word was currently force-broken; shift some part to here */
	  p = p + origc;
	} else if ((checkingUnderflow && firstUnderflow
		    && (b <= startp || c>= 0))
		   || (!theFirstSnip 
		       && (!c || (!origc && (c < 0) && (b <= startp))))) {
	  /* Can't fit this snip in the line */
	  if (snip->prev)
	    snip->prev->flags |= wxSNIP_NEWLINE;
	  if (hadNewline && snip->next)
	    snip->flags |= wxSNIP_NEWLINE;
	  if (noChangeIfStartOfSnip && (!hadNewline || snip->next))
	    return FALSE;
	  refreshAll = TRUE;
	  return TRUE;
	} else if ((c < 0) && (b > startp)) {
	  /* Overflow, but previous wordbreak was before this snip */
	  p = b;
	} else {
	  /* Overflow: we have to break the word anyway */
	  if (!origc) {
	    if ((snip->count == 1) 
		&& snip->next
		&& (snip->next->flags & wxSNIP_HARD_NEWLINE)) {
	      /* don't insert a break before a real newline */
	      break;
	    } else
	      p++;
	  } else
	    p += origc;
	}
      } else {
	p = p + c;
      }
      MakeSnipset(p, p);
      snip = FindSnip(p, -1);
      if (snip->next)
	snip->flags |= wxSNIP_NEWLINE;
      refreshAll = TRUE;
      return TRUE;
    }
    
    p += snip->count;

    theFirstSnip = FALSE;
    firstUnderflow = FALSE;
  }

  if (!snip 
      && (lastSnip->flags & wxSNIP_NEWLINE)
      && !(lastSnip->flags & wxSNIP_HARD_NEWLINE)) {
    lastSnip->flags -= wxSNIP_NEWLINE;
    refreshAll = TRUE;
    return TRUE;
  }

  if (!checkingUnderflow || noChangeIfEndOfSnip)
    return deletedANewline;
  else {
    refreshAll = TRUE;
    return TRUE;
  }
}

#if CHECK_CONSISTENCY
static int CheckRBConsistent(wxMediaLine *line)
{
  Bool red;
  int l, r;

  if (line == NIL)
    return 0;

  red = (line->flags & 1);

  if (red) {
    if (line->left->flags & 1)
      fprintf(stderr, "Red left inconsistency\n");
    if (line->right->flags & 1)
      fprintf(stderr, "Red right inconsistency\n");
  }

  l = CheckRBConsistent(line->left);
  r = CheckRBConsistent(line->right);

  if (l != r)
    fprintf(stderr, "Black count inconsistency\n");
  
  return l + (red ? 0 : 1);
}
#endif

void wxMediaEdit::RecalcLines(wxDC *dc, Bool calcGraphics)
{
  wxMediaLine *line;
  wxSnip *snip;
  double X, Y, descent, space, lineBase, old_max_width;
  Bool _changed, resized;

  if (!calcGraphics)
    return;

  _changed = FALSE;

#if CHECK_CONSISTENCY
  long p, p2;

  snip = snips;

  if (firstLine != lineRoot->First())
    fprintf(stderr, "First line inconsistency\n");
  if (lastLine != lineRoot->Last())
    fprintf(stderr, "Last line inconsistency\n");

  if (firstLine->prev)
    fprintf(stderr, "First line prev inconsistency\n");
  if (lastLine->next)
    fprintf(stderr, "First line next inconsistency\n");

  if (firstLine->snip != snips)
    fprintf(stderr, "First line first snip inconsistency\n");
  if (lastLine->lastSnip != lastSnip)
    fprintf(stderr, "Last line last snip inconsistency\n");

  CheckRBConsistent(lineRoot);

  Y = 0;
  p = 0;
  for (line = firstLine; line; line = line->next) {
    if (line->snip != snip)
      fprintf(stderr, "Line start inconsistency\n");
    
    if (line->GetPosition() != p)
      fprintf(stderr, "Line position inconsistency\n");
    if (line->GetLocation() != Y)
      fprintf(stderr, "Line location inconsistency\n");

    if (line->prev && (line->snip != line->prev->lastSnip->next))
      fprintf(stderr, "Line snip continuity inconsistency\n");

    p2 = p;
    for (snip = line->snip; snip && snip != line->lastSnip; snip = snip->next){
      if (snip->line != line)
	fprintf(stderr, "Snip's line inconsistency\n");
      if (!snip->style)
	fprintf(stderr, "Snip's style missing\n");
      if (snip->admin != snipAdmin)
	fprintf(stderr, "Snip's admin inconsistency\n");
      if (!(snip->flags & wxSNIP_OWNED))
	fprintf(stderr, "Snip's is-owned inconsistency\n");
      p2 += snip->count;
    }
    if (snip->line != line)
	fprintf(stderr, "Snip's line inconsistency\n");

    if (!snip)
      fprintf(stderr, "Line end inconsistency\n");

    p2 += snip->count;
    if ((p2 - p) != line->len)
      fprintf(stderr, "Line count inconsistency\n");

    snip = line->lastSnip->next;

    if (!line->next && line != lastLine)
      fprintf(stderr, "Last line link inconsistency\n");

    wxMediaLine *other;
    if (line->left != NIL) {
      for (other = line->prev; other && other != line->left; other=other->prev) {
      }
      if (!other)
	fprintf(stderr, "Left link inconsistency\n");
    }
    if (line->right != NIL) {
      for (other=line->next; other && other!=line->right; other=other->next) {
      }
      if (!other)
	fprintf(stderr, "Right link inconsistency\n");
    }
    if (line->parent == NIL) {
      if (line != lineRoot)
	fprintf(stderr, "Root inconsistency\n");
    } else if (line != line->parent->right && line != line->parent->left)
      fprintf(stderr, "Parent inconsistency\n");

    Y += line->h;
    p += line->len;
  }
  if (p != len)
    fprintf(stderr, "Total count inconsistency\n");

#endif

  if (snipCacheInvalid)
    for (snip = snips; snip; snip = snip->next) {
      snip->SizeCacheInvalid();
    }

  old_max_width = maxWidth;

  if (flowInvalid && (maxWidth <= 0))
    maxWidth = A_VERY_BIG_NUMBER;

  if (graphicsInvalid || flowInvalid || snipCacheInvalid) {
    /* Set all lines invalid. */
    for (line = firstLine; line; line = line->next) {
      line->MarkRecalculate();
      if (flowInvalid)
	line->MarkCheckFlow();
    }
  }

#if LOOK_FOR_ZEROED
  if (len)
    for (snip = snips, i = 0; snip; snip = snip->next) {
      if (!snip->count)
	fprintf(stderr, "zero snip found at %d\n", i);
      else
	i += snip->count;
    }
#endif

  if (maxWidth > 0) {
    double w;
    Bool fl = flowLocked, wl = writeLocked;
    wxMediaLine *lr;

    /* If any flow is update, snip sizing methods will be called. */
    flowLocked = TRUE;
    writeLocked = TRUE;

    w = maxWidth - CURSOR_WIDTH;
    lr = lineRoot;
    while (lineRoot->UpdateFlow(&lr, this, w, dc)) {
      lineRoot = lr;
      _changed = TRUE;
    }
    lineRoot = lr;

    flowLocked = fl;
    writeLocked = wl;    
  }

  if (old_max_width != maxWidth)
    maxWidth = old_max_width;

  if (_changed) {
    refreshAll = TRUE;
    firstLine = lineRoot->First();
    lastLine = lineRoot->Last();
    numValidLines = lineRoot->Number();
  }

  if (lineRoot->UpdateGraphics(this, dc))
    _changed = TRUE;

  if (!_changed && !graphicMaybeInvalidForce) {
    graphicMaybeInvalid = FALSE;
    return;
  }

  graphicMaybeInvalid = FALSE;
  graphicMaybeInvalidForce = FALSE;

  Y = lastLine->GetLocation() + lastLine->h;

  if (lastSnip->flags & wxSNIP_NEWLINE) {
    extraLine = TRUE;
    extraLineH = lastLine->lastH + lineSpacing;
    Y += extraLineH;
  } else {
    extraLine = FALSE;
    extraLineH = 0;
  }

  X = lineRoot->maxWidth + CURSOR_WIDTH;
  if (minWidth > 0 && X < minWidth)
    X = minWidth;

  if (minHeight > 0 && Y < minHeight)
    Y = minHeight;
  if (maxHeight > 0 && Y > maxHeight)
    Y = maxHeight;

  descent = lastLine->h - lastLine->bottombase;
  space = firstLine->topbase;
  lineBase = firstLine->bottombase;
  
  if (totalHeight != Y || totalWidth != X 
      || finalDescent != descent 
      || initialSpace != space
      || initialLineBase != lineBase) {
    totalHeight = Y;
    totalWidth = X;
    finalDescent = descent;
    initialSpace = space;
    initialLineBase = lineBase;
    resized = TRUE;
  } else
    resized = FALSE;

  graphicsInvalid = FALSE;  
  flowInvalid = FALSE;
  snipCacheInvalid = FALSE;

  drawCachedInBitmap = FALSE;

  if (resized && admin)
    admin->Resized(FALSE);

  OnReflow();
}

void wxMediaEdit::OnReflow(void)
{
}

wxBitmap *wxMediaEdit::SetAutowrapBitmap(wxBitmap *bm)
{
  wxBitmap *old;
  double oldWidth;

  if (flowLocked)
    return NULL;

  old = autoWrapBitmap;

  autoWrapBitmap = bm;
  oldWidth = wrapBitmapWidth;
  if (autoWrapBitmap) {
    int bw;
    bw = autoWrapBitmap->GetWidth();
    wrapBitmapWidth = bw;
  } else
    wrapBitmapWidth = 0;

  if (maxWidth > 0)
    SetMaxWidth(maxWidth + oldWidth);

  return old;
}

static int show_outline_for_inactive = 0;

static wxPen *caretPen = NULL;
static wxPen *outlinePen = NULL;
static wxPen *outlineInactivePen = NULL;
static wxBrush *outlineBrush = NULL;
#if ALLOW_X_STYLE_SELECTION
static wxBrush *outlineNonownerBrush = NULL;
static char xpattern[32] = {0x88, 0x88,
			    0,    0,
			    0x22, 0x22,
			    0,    0,
			    0x88, 0x88,
			    0,    0,
			    0x22, 0x22,
			    0,    0,
			    0x88, 0x88,
			    0,    0,
			    0x22, 0x22,
			    0,    0,
			    0x88, 0x88,
			    0,    0,
			    0x22, 0x22,
			    0,    0};
#endif
static wxBrush *clearBrush = NULL;

/* This does the actual drawing */
void wxMediaEdit::Redraw(wxDC *dc, double starty, double endy, 
			 double leftx, double rightx,
			 double dy, double dx, 
			 int show_caret, int show_xsel,
			 wxColour *bgColor)
{
  wxMediaLine *line;
  wxSnip *snip, *first, *last;
  double x, topbase, bottombase, hxs, hxe, hsxs, hsxe, hsys, hsye, down, bottom;
  double tleftx, tstarty, trightx, tendy;
  double h, w, descent, space, ycounter, prevwasfirst = 0.0;
  long p, pcounter;
  long _startpos, _endpos;
  Bool posAtEol;
  Bool hilite, hiliteSome;
  int align;
  wxStyle *oldStyle;
  wxPen *savePen;
  wxBrush *saveBrush;
  Bool wl;

  if (!show_outline_for_inactive) {
    if (!wxGetBoolPreference("outlineInactiveSelection", &show_outline_for_inactive))
      show_outline_for_inactive = 0;
    show_outline_for_inactive = !show_outline_for_inactive ? -1 : 1;
  }

  wl = writeLocked;

  flowLocked = TRUE;
  writeLocked = TRUE;

  if (flash) {
    _startpos = flashstartpos;
    _endpos = flashendpos;
    posAtEol = flashposateol;
  } else {
    _startpos = startpos;
    _endpos = endpos;
    posAtEol = posateol;
  }

  if (!outlinePen) {
    wxREGGLOB(outlinePen);
    wxREGGLOB(outlineInactivePen);
    wxREGGLOB(outlineBrush);
#if ALLOW_X_STYLE_SELECTION
    wxREGGLOB(outlineNonownerBrush);
#endif
    wxREGGLOB(clearBrush);

    outlinePen = wxThePenList->FindOrCreatePen("BLACK", 0, wxTRANSPARENT);
    if (!caretPen) {
      wxREGGLOB(caretPen);
      caretPen = wxThePenList->FindOrCreatePen("BLACK", 1, wxXOR);
    }
    outlineBrush = wxTheBrushList->FindOrCreateBrush("BLACK", wxCOLOR);
    outlineInactivePen = wxThePenList->FindOrCreatePen("BLACK", 1, wxCOLOR);
#if ALLOW_X_STYLE_SELECTION
    outlineNonownerBrush = new WXGC_PTRS wxBrush();
    outlineNonownerBrush->SetColour("BLACK");
    {
      wxBitmap *bm;
      bm = new WXGC_PTRS wxBitmap(xpattern, 16, 16);
      outlineNonownerBrush->SetStipple(bm);
    }
    outlineNonownerBrush->SetStyle(wxXOR);
#endif
    clearBrush = wxTheBrushList->FindOrCreateBrush("WHITE", wxSOLID);
  }
  
  dc->SetBackgroundMode(wxSOLID);

  line = lineRoot->FindLocation(starty);

  if (bgColor) {
    wxPen *lsavePen;
    wxBrush *lsaveBrush, *wb;

    lsavePen = dc->GetPen();
    lsaveBrush = dc->GetBrush();

    if (bgColor == wxWHITE)
      wb = clearBrush;
    else
      wb = wxTheBrushList->FindOrCreateBrush(bgColor, wxSOLID);
    dc->SetBrush(wb);
    dc->SetPen(outlinePen);

    dc->DrawRectangle(leftx + dx, starty + dy, 
		      rightx - leftx + GC_RECT_BRUSH_EXTEND,
		      endy - starty + GC_RECT_BRUSH_EXTEND);

    dc->SetBrush(lsaveBrush);
    dc->SetPen(lsavePen);
  }

  OnPaint(TRUE, dc, leftx, starty, rightx, endy, dx, dy, 
	  (show_caret && !caretSnip) 
	  ? show_caret
	  : (int)wxSNIP_DRAW_NO_CARET);

  if (!line)
    goto paint_done;

  tleftx = leftx + dx;
  tstarty = starty + dy;
  trightx = rightx + dx;
  tendy = endy + dy;

  oldStyle = NULL;

  ycounter = line->GetLocation();
  pcounter = line->GetPosition();

  /* Quiet the compiler: */
  hsxs = hsxe = hsys = hsye = 0;

  for (; line; line = line->next) {
    if (ycounter >= endy)
      goto paint_done;

    first = line->snip;
    last = line->lastSnip->next;

    x = line->GetLeftLocation(maxWidth);
    
    bottombase = ycounter + line->bottombase;
    topbase = ycounter + line->topbase;
    p = pcounter;

    hiliteSome = FALSE;

    for (snip = first; PTRNE(snip, last); snip = snip->next) {
      snip->style->SwitchTo(dc, oldStyle);
      oldStyle = snip->style;

      w = h = descent = space = 0.0;
      snip->GetExtent(dc, x, ycounter, &w, &h, &descent, &space);
      
      align = snip->style->GetAlignment();
      
      if (align == wxALIGN_BOTTOM)
	down = bottombase - h + descent;
      else if (align == wxALIGN_TOP)
	down = topbase - space;
      else {
	down = (h - descent - space) / 2;
	down = (topbase + bottombase) / 2 - down - space;
      }

      if ((x <= rightx) && (x + w >= leftx)) {
	snip->Draw(dc, x + dx, down + dy, 
		   tleftx, tstarty, trightx, tendy, 
		   dx, dy, 
		   (PTREQ(snip, caretSnip) && show_caret)
		   ? show_caret
		   : (int)wxSNIP_DRAW_NO_CARET);

#if PLOT_SNIP_DOT
	dc->DrawLine(x + dx, down + dy, x + dx + 2, down + dy + 2);
#endif
      }

      /* The rules for hiliting are surprisingly complicated: */

      if (hiliteOn
	  && (show_xsel
	      || (!caretSnip
		  && ((show_caret == wxSNIP_DRAW_SHOW_CARET)
		      || ((show_caret >= inactiveCaretThreshold)
			  && (_startpos != _endpos)))))) {
	if (posAtEol)
	  hilite = (_startpos == p + snip->count);
	else
	  hilite = (((_startpos < p + snip->count) && (_endpos >= p)
		     && (_startpos == _endpos || _endpos > p))
		    || (p + snip->count == len && _startpos == len));
	
	if (hilite && (snip->flags & wxSNIP_NEWLINE))
	  /* End of line: */
	  hilite = ((_startpos != p + snip->count)
		    || (_startpos == _endpos && posAtEol)
		    || (_startpos != _endpos && _startpos < p + snip->count));
	if (hilite && PTREQ(snip, first))
	  /* Beginning of line: */
	  hilite = ((_endpos != p)
		    || (_startpos == _endpos && !posAtEol)
		    || (_startpos != _endpos && _endpos > p));
      } else
	hilite = FALSE;

      if (hilite) {
	bottom = down + h;

	if (_startpos <= p) {
	  if (_startpos < p)
	    hxs = 0;
	  else
	    hxs = x;
	} else
	  hxs = x + snip->PartialOffset(dc, x, ycounter, _startpos - p);
	
	if (_endpos >= p + snip->count) {
	  if (snip->flags & wxSNIP_NEWLINE) {
	    if (_endpos == _startpos)
	      hxe = hxs;
	    else {
	      hxe = rightx;
	      bottom = ycounter + line->h;
	    }
	  } else
	    hxe = x + w;
	} else
	  hxe = x + snip->PartialOffset(dc, x, ycounter, _endpos - p);
	
	if (!hiliteSome) {
	  hsxs = hxs;
	  hsxe = hxe;
	  hsys = down;
	  hsye = bottom;
	  hiliteSome = TRUE;
	} else {
	  hsxe = hxe;
	  if (down < hsys)
	    hsys = down;
	  if (bottom > hsye)
	    hsye = bottom;
	}
      }

      x += w;
      
      p += snip->count;
    }

    if (wrapBitmapWidth 
	&& !(line->lastSnip->flags & wxSNIP_HARD_NEWLINE)
	&& last
	&& (rightx >= maxWidth) && autoWrapBitmap->Ok()) {
      int h;

      if (autoWrapBitmap->Ok()) {
	wxColour *osfg;

	h = (int)autoWrapBitmap->GetHeight();
	if (h > line->bottombase)
	  h = (int)line->bottombase;

	osfg = oldStyle->GetForeground();
	dc->Blit(maxWidth + dx - 1, bottombase - h + dy, 
		 wrapBitmapWidth, h,
		 autoWrapBitmap, 0, 0,
		 wxSOLID, osfg);
      }
    }

    if (hiliteSome) {
      if (hsxe != hsxs) {
	if ((hsxs <= rightx) && (hsxe >= leftx)) {
	  savePen = dc->GetPen();
	  
	  if (hsxs < leftx)
	    hsxs = leftx;
	  if (hsxe > rightx)
	    hsxe = rightx;

	  if (!show_xsel && (show_caret < wxSNIP_DRAW_SHOW_CARET)) {
	    if (show_outline_for_inactive > 0) {
	      int lastHilite, firstHilite;
	      
	      firstHilite = (_startpos >= pcounter);
	      lastHilite = (_endpos <= pcounter + line->len);
	      
	      dc->SetPen(outlineInactivePen);
	      
	      if (firstHilite) {
		dc->DrawLine(hsxs + dx, hsys + dy, hsxe + dx - 1, hsys + dy);
		prevwasfirst = hsxs;
	      } else if (prevwasfirst) {
		dc->DrawLine(0 + dx, hsys + dy, prevwasfirst + dx, hsys + dy);
		prevwasfirst = 0.0;
	      }
	      dc->DrawLine(hsxs + dx, hsys + dy, hsxs + dx, hsye + dy - 1);
	      dc->DrawLine(hsxe + dx - 1, hsys + dy, hsxe + dx - 1, hsye + dy - 1);
	      if (lastHilite) {
		dc->DrawLine(hsxs + dx, hsye + dy, hsxe + dx - 1, hsye + dy);
		if (!firstHilite)
		  dc->DrawLine(hsxe + dx, hsys + dy, rightx + dx, hsys + dy);
	      }
	    }
	  } else {
	    saveBrush = dc->GetBrush();
	    dc->SetPen(outlinePen);
	    dc->SetBrush(outlineBrush);
	    
	    dc->DrawRectangle(hsxs + dx, hsys + dy, 
			      hsxe - hsxs + GC_RECT_BRUSH_EXTEND, 
			      hsye - hsys + GC_RECT_BRUSH_EXTEND);
#if ALLOW_X_STYLE_SELECTION
	    if (show_xsel) {
	      dc->SetBrush(outlineNonownerBrush);
	      dc->DrawRectangle(hsxs + dx, hsys + dy, 
				hsxe - hsxs + GC_RECT_BRUSH_EXTEND, 
				hsye - hsys + GC_RECT_BRUSH_EXTEND);
	    }
#endif
	    
	    dc->SetBrush(saveBrush);
	  }
	  dc->SetPen(savePen);
	}
      } else {
	if (show_caret == wxSNIP_DRAW_SHOW_CARET) {
	  if ((hsxs <= rightx) && (hsxs >= leftx)) {
	    savePen = dc->GetPen();
	    dc->SetPen(caretPen);
	    dc->DrawLine(hsxs + dx, hsys + dy, 
			 hsxs + dx, 
			 hsye + dy - 1 + GC_LINE_EXTEND);
	    dc->SetPen(savePen);
	  }
	}
      }
    }

    pcounter += line->len;
    ycounter += line->h;
  }

  {
    wxStyle *bs;
    bs = styleList->BasicStyle();
    bs->SwitchTo(dc, oldStyle);
  }

  if ((show_caret == wxSNIP_DRAW_SHOW_CARET) && !caretSnip)
    if (!line && extraLine)
      if (!posAtEol && _startpos == len && _endpos == _startpos
	  && hiliteOn) {
	double y;
	y = ycounter;

	savePen = dc->GetPen();
	dc->SetPen(caretPen);
	dc->DrawLine(dx, y + dy, dx, 
		     y + extraLineH + dy - 1 + GC_LINE_EXTEND);
	dc->SetPen(savePen);
      }
 
paint_done:

  OnPaint(FALSE, dc, leftx, starty, rightx, endy, dx, dy, 
	  (show_caret && !caretSnip)
	  ? show_caret
	  : (int)wxSNIP_DRAW_NO_CARET);

  writeLocked = wl;
  flowLocked = FALSE;
}

/* This one notifies the administrator that we need to be updated. */
void wxMediaEdit::Redraw()
{
  double w, h;
  double top, bottom, height, width, left, right;
  double x, y, origx, origy;
  double fy, fx;
  Bool oneline;
  wxDC *dc;
  Bool needs_update = TRUE;

  if (flowLocked || !admin)
    return;

  if (admin->DelayRefresh()) {
    /* Does the admin know the refresh box already? */
    if ((delayedscroll != -1) && !delayedscrollbox && (refreshAll || refreshUnset)) {
      /* Yes ... */
      if (!refreshAll && refreshBoxUnset)
	return; /* Nothing to do */
      admin->GetMaxView(&x, &y, &w, &h);
      top = y;
      bottom = y + h;
      left = x;
      right = left + w;
      if (!refreshAll) {
	if (refreshL > left)
	  left = refreshL;
	if (refreshR < right)
	  right = refreshR;
	if (refreshT > top)
	  top = refreshT;
	if (refreshB < bottom)
	  bottom = refreshB;
      }
      refreshUnset = refreshBoxUnset = TRUE;
      refreshAll = FALSE;
      height = bottom - top;
      width = right - left;
      if ((width > 0) && (height > 0))
	admin->NeedsUpdate(left, top, width, height);
    }
  }

  dc = admin->GetDC(&x, &y);

  if (!dc) {
    delayedscroll = -1;
    delayedscrollbox = FALSE;
    return;
  }

  origx = x;
  origy = y;

  RecalcLines(dc);

  if (delayedscroll != -1) {
    if (ScrollToPosition(delayedscroll, delayedscrollateol, FALSE,
			 delayedscrollend, delayedscrollbias))
      refreshAll = TRUE;
  } else if (delayedscrollbox) {
    delayedscrollbox = FALSE;
    if (ScrollTo(delayedscrollsnip, delayedscrollX, delayedscrollY,
		 delayedscrollW, delayedscrollH, FALSE, delayedscrollbias))
      refreshAll = TRUE;
  }

  admin->GetDC(&x, &y);
  
  if (x != origx || y != origy)
    refreshAll = TRUE;

  admin->GetMaxView(&x, &y, &w, &h);

  top = y;
  bottom = y + h;
  left = x;
  right = left + w;

  /* Figure out the minimal refresh area. The refresh area may be
     determined by character position ranges, box coordinates, or
     both. If neither is specified, we have to assume that everything
     needs to be refreshed. */
  
  if (!refreshAll && (!refreshUnset || !refreshBoxUnset)) {
    if (!refreshUnset) {
      top = y;
      if (refreshStart > -1 && refreshEnd > -1) {
	// oneline = PositionLine(refreshStart) == PositionLine(refreshEnd);
	oneline = FALSE; // b/c it doesn't seem to help
      } else
	oneline = FALSE;
      if (refreshStart > -1) {
	PositionLocation(refreshStart, 
			 oneline ? (double *)&fx : (double *)NULL, (double *)&fy, 
			 TRUE, TRUE, TRUE);
	if (fy > top)
	  top = (long)fy;
	if (oneline && fx > left)
	  left = fx;
      }
      bottom = y + h;
      if (refreshEnd > -1) {
	PositionLocation(refreshEnd, 
			 oneline ? (double *)&fx : (double *)NULL, (double *)&fy, 
			 FALSE, FALSE, TRUE);
	if (fy < bottom)
	  bottom = (long)fy;
	if (oneline && fx + CURSOR_WIDTH < right)
	  right = fx + CURSOR_WIDTH;
      }

      if (!refreshBoxUnset) {
	if (refreshT < top)
	  top = refreshT;
	if (refreshB > bottom)
	  bottom = refreshB;
      }
    } else {
      if (refreshL > left)
	left = refreshL;
      if (refreshR < right)
	right = refreshR;
      if (refreshT > top)
	top = refreshT;
      if (refreshB < bottom)
	bottom = refreshB;
    }
  } else if (!refreshAll) {
    /* Nothing needs to be updated! */
    needs_update = FALSE;
  }

  refreshUnset = refreshBoxUnset = TRUE;
  refreshAll = FALSE;

  height = bottom - top;
  width = right - left;

  if (changed) {
    Bool wl, fl;

    changed = FALSE;
    wl = writeLocked;
    fl = flowLocked;
    writeLocked = flowLocked = TRUE;
    OnChange();
    writeLocked = wl;
    flowLocked = fl;
  }

  if (needs_update && (width > 0) && (height > 0))
    admin->NeedsUpdate(left, top, width, height);
}

/* This one is called by the administrator: */
void wxMediaEdit::Refresh(double left, double top, double width, double height,
			  int show_caret, wxColour *bgColor)
{
  double x, y, bottom, right;
  Bool ps;
  wxDC *dc;
  int show_xsel = 0;

  if ((width <= 0) || (height <= 0))
    return;

  /* BEWARE - this same test is in BlinkCaret() and AdjustCursor(): */
  if (graphicMaybeInvalid || flowLocked || delayRefresh) {
    /* This Refresh command was not requested by us and we're busy. 
       (Probably in the middle of a begin-/end-edit-sequnce.)
       Add the given region to our own invalid-region tracking and
       we'll get back to it when we're done with whatever. */
    RefreshBox(left, top, width, height);
    return;
  }

  if (!admin)
    return;
  dc = admin->GetDC(&x, &y);
  if (!dc)
    return;

  BeginSequenceLock();

  if (caretBlinked && show_caret && !caretSnip) {
    /* Maintain caretBlinked invariant */
    show_caret = 0;
  }

  if (ReadyOffscreen(width, height))
    drawCachedInBitmap = FALSE;

  /* Make sure all location information is integral,
     so we can shift the coordinate system and generally
     update on pixel boundaries. */
  x = floor(x);
  y = floor(y);
  bottom = ceil(top + height);
  right = ceil(left + width);
  top = floor(top);
  left = floor(left);
  width = right - left;
  height = bottom - top;

  ps = (wxSubType(dc->__type, wxTYPE_DC_POSTSCRIPT)
	|| wxSubType(dc->__type, wxTYPE_DC_PRINTER));

#if ALLOW_X_STYLE_SELECTION
  if (((show_caret != wxSNIP_DRAW_SHOW_CARET) || caretSnip)
      && (wxMediaXSelectionOwner == this)
      && !flash 
      && (startpos != endpos))
    show_xsel = 1;
#endif

  if (bgColor && !offscreenInUse && bitmap && bitmap->Ok() && offscreen->Ok() && !ps) {
    unsigned char red, green, blue;

    red = (unsigned char)bgColor->Red();
    green = (unsigned char)bgColor->Green();
    blue = (unsigned char)bgColor->Blue();

#ifndef EACH_BUFFER_OWN_OFFSCREEN
    offscreenInUse = TRUE;
#endif
    if (!drawCachedInBitmap 
#ifndef EACH_BUFFER_OWN_OFFSCREEN
	|| (lastUsedOffscreen != this)
#endif
	|| (top != lastDrawT) || (bottom != lastDrawB)
	|| (left != lastDrawL) || (right != lastDrawR)
	|| (lastDrawCaret != show_caret)
	|| (lastDrawXSel != show_xsel)
	|| (red != lastDrawRed)
	|| (green != lastDrawGreen)
	|| (blue != lastDrawBlue)) {
      offscreen->BeginDrawing();
      Redraw(offscreen, top, bottom, left, right, -top, -left, 
	     show_caret, show_xsel, bgColor);
      offscreen->EndDrawing();
      lastDrawL = left;
      lastDrawT = top;
      lastDrawR = right;
      lastDrawB = bottom;
      lastDrawCaret = show_caret;
      lastDrawXSel = show_xsel;
      lastDrawRed = red;
      lastDrawGreen = green;
      lastDrawBlue = blue;
      drawCachedInBitmap = TRUE;
    }

    {
      wxBitmap *bm;
      bm = offscreen->GetObject();
      dc->Blit(left - x, top - y, width, height, bm, 0, 0, wxCOPY);
    }
#ifndef EACH_BUFFER_OWN_OFFSCREEN
    offscreenInUse = FALSE;
    lastUsedOffscreen = this;
#endif
  } else {
    wxPen *pen;
    wxBrush *brush;
    wxFont *font;
    wxColour *fg, *bg, *col;
    wxRegion *rgn;
    int bgmode;

    pen = dc->GetPen();
    brush = dc->GetBrush();
    font = dc->GetFont();
    col = dc->GetTextForeground();
    fg = new WXGC_PTRS wxColour(col);
    col = dc->GetTextBackground();
    bg = new WXGC_PTRS wxColour(col);
    bgmode = dc->GetBackgroundMode();
 
    rgn = dc->GetClippingRegion();
    dc->SetClippingRect(left - x, top - y, width, height);

    Redraw(dc, top, bottom, left, right, -y, -x, show_caret, show_xsel, bgColor);

    dc->SetClippingRegion(rgn);

    dc->SetBrush(brush);
    dc->SetPen(pen);
    dc->SetFont(font);
    dc->SetTextForeground(fg);
    dc->SetTextBackground(bg);
    dc->SetBackgroundMode(bgmode);
  }

  EndSequenceLock();
}

/* This one is used internally to delay refreshes: */
void wxMediaEdit::NeedRefresh(long start, long end)
{
  if (refreshUnset) {
    refreshStart = start;
    refreshEnd = end;
    refreshUnset = FALSE;
  } else {
    if (start < refreshStart)
      refreshStart = start;
    if (end == -1)
      refreshEnd = -1;
    else if (refreshEnd != -1 && end > refreshEnd)
      refreshEnd = end;
  }

  drawCachedInBitmap = FALSE;

  ContinueRefresh();
}

void wxMediaEdit::RefreshByLineDemand(void)
{
  if (!graphicMaybeInvalid)
    graphicMaybeInvalid = TRUE;

  ContinueRefresh();
}

void wxMediaEdit::ContinueRefresh(void)
{
  if (!delayRefresh && !printing && (!admin || !admin->DelayRefresh()))
    Redraw();
  else {
    int rs = 1;
    if (!delayRefresh && ((delayedscroll != -1)
                          || delayedscrollbox)) {
      if (!printing && admin) {
        /* Although the administrator says to delay,
           we can't just drop scroll requests. */
        Redraw();
        rs =  0;
      } else {
        delayedscroll = -1;
        delayedscrollbox = 0;
      }
    }
    if (admin && !admin->standard)
      admin->Resized(FALSE);
  }
}

void wxMediaEdit::NeedCaretRefresh(void)
{
  NeedRefresh(startpos, endpos);
}

/* 8.5" x 11" Paper, 0.5" Margin; usually not used */
static long page_width = 612, page_height = 792;

#define wxGetPrinterOrientation(local) (local = wxGetThePrintSetupData(), local->GetPrinterOrientation())

void wxmeGetDefaultSize(double *w, double *h)
{
  wxPrintSetupData *psd;

  *w = page_width;
  *h = page_height;

  if (wxGetPrinterOrientation(psd) != PS_PORTRAIT)  {
    double tmp;
    
    tmp = *h;
    *h = *w;
    *w = tmp;
  }
}

void wxGetMediaPrintMargin(long *hm, long *vm)
{
  wxPrintSetupData *psd;
  psd = wxGetThePrintSetupData();
  psd->GetEditorMargin(hm, vm);
}

void wxSetMediaPrintMargin(long hm, long vm)
{
  wxPrintSetupData *psd;
  psd = wxGetThePrintSetupData();
  psd->SetEditorMargin(hm, vm);
}

class SaveSizeInfo {
public:
  double maxw;
  wxBitmap *bm;
};

void *wxMediaEdit::BeginPrint(wxDC *dc, Bool fit)
{
  SaveSizeInfo *savedInfo;

  if (flowLocked)
    return NULL;

  CheckRecalc();

  SizeCacheInvalid();

  if (fit) {
    double w, h;
    long hm, vm;

    savedInfo = new WXGC_PTRS SaveSizeInfo;
    
    savedInfo->maxw = GetMaxWidth();
    savedInfo->bm = SetAutowrapBitmap(NULL);

    wxGetMediaPrintMargin(&hm, &vm);

    dc->GetSize(&w, &h);
    w -= 2 * hm;
    SetMaxWidth(w);
  } else
    savedInfo = NULL;

  RecalcLines(dc, TRUE);

  {
    Bool wl, fl;

    wl = writeLocked;
    fl = flowLocked;
    writeLocked = flowLocked = TRUE;
    OnChange();
    writeLocked = wl;
    flowLocked = fl;
  }


  return savedInfo;
}

void wxMediaEdit::EndPrint(wxDC *, void *data)
{
  if (flowLocked)
    return;

  SizeCacheInvalid();

  if (data) {
    SaveSizeInfo *savedInfo = (SaveSizeInfo *)data;
    
    SetMaxWidth(savedInfo->maxw);
    SetAutowrapBitmap(savedInfo->bm);

    /* DELETE_OBJ savedInfo; */
  }

  {
    Bool wl, fl;

    wl = writeLocked;
    fl = flowLocked;
    writeLocked = flowLocked = TRUE;
    OnChange();
    writeLocked = wl;
    flowLocked = fl;
  }
}

Bool wxMediaEdit::HasPrintPage(wxDC *dc, int page)
{
  double H, W, h;
  long vm, hm;
  int i, this_page = 1;
  wxMediaLine *line;

  if (flowLocked)
    return FALSE;

  RecalcLines(dc, TRUE);

  dc->GetSize(&W, &H);

  if (!W || !H)
    wxmeGetDefaultSize(&W, &H);

  wxGetMediaPrintMargin(&hm, &vm);

  H -= (2 * vm);
  W -= (2 * hm);

  line = firstLine;
  for (i = 0; i < numValidLines; this_page++) {
    h = 0;
    while (!h || ((i < numValidLines) && (line->h < H - h))) {
      h += line->h;
      i++;
      line = line->next;
    }

    if (this_page >= page)
      return TRUE;
  }

  return FALSE;
}

void wxMediaEdit::PrintToDC(wxDC *dc, int page)
{
  double H, W, FH, FW, y, h, next_h;
  long vm, hm;
  int i, this_page = 1;
  wxMediaLine *line;
  wxPrintSetupData *psd;

  if (flowLocked)
    return;

  RecalcLines(dc, TRUE);

  dc->GetSize(&W, &H);

  if (!W || !H) {
    W = page_width;
    H = page_height;

    if (wxGetPrinterOrientation(psd) != PS_PORTRAIT)  {
      double tmp;
      
      tmp = H;
      H = W;
      W = tmp;
    }
  }

  FH = H;
  FW = W;

  wxGetMediaPrintMargin(&hm, &vm);

  H -= (2 * vm);
  W -= (2 * hm);

  y = 0;
  next_h = 0;
  line = firstLine;
  for (i = 0; (i < numValidLines) || next_h; this_page++) {
    /* line is the line that we haven't finished printing.
       H is the total page height.
       y is the starting location to print for this page.
       h is the height that we're hoping to fit into the page. */

    h = next_h; /* for part of a line leftover last time */
    next_h = 0;

    while (!h || ((i < numValidLines) && (line->h < H - h))) {
      h += line->h;
      i++;
      line = line->next;
    }

    if ((h < H) && (i < numValidLines) && (line->h > H)) {
      /* We'll have to break it up anyway. Start now? */
      int pos;
      double py;
      pos = FindScrollLine(y + H);
      py = ScrollLineLocation(pos);
      if (py > y + h) {
	/* Yes, at least one line will fit */
	h += line->h;
	i++;
	line = line->next;
      }
    }

    if (h > H) {
      /* Only happens if we have something that's too big to fit on a page. */
      /* Look for internal scroll positions */
      int pos;
      double py;
      pos = FindScrollLine(y + H);
      py = ScrollLineLocation(pos);
      if (py > y) {
	double new_h = py - y;
	next_h = h - new_h;
	h = new_h;
      }
    }

    if (page < 0 || (page == this_page)) {
      if (page < 0)
	dc->StartPage();
      
      Redraw(dc, y + (i ? 1 : 0), y + h - 1, 0, W, -y + vm, hm, 
	     wxSNIP_DRAW_NO_CARET, 0, NULL);

      if (page < 0)
	dc->EndPage();

      if (page >= 0)
	break;
    }

    y += h;
  }
}

#if ALLOW_X_STYLE_SELECTION

Bool wxMediaEdit::OwnXSelection(Bool on, Bool update, Bool force)
{
  if (DoOwnXSelection(on, force)) {
    if (update)
      NeedCaretRefresh();
    return TRUE;
  } else
    return FALSE;
}

#endif

/****************************************************************/
/****************************************************************/

void wxMediaEdit::SetParagraghMargins(long i, double firstLeft, double left, double right)
{
  wxMediaLine *l;
  wxMediaParagraph *p;

  if (i < 0)
    i = 0;
  
  l = lineRoot->FindParagraph(i);
  if (l) {

    p = l->paragraph->Clone();
    l->paragraph = p;

    p->leftMarginFirst = firstLeft;
    p->leftMargin = left;
    p->rightMargin = right;

    if (maxWidth > 0) {
      l->MarkCheckFlow();
      l = l->next;
      while (l && !(l->flags & WXLINE_STARTS_PARA)) {
	l->MarkCheckFlow();
	l = l->next;
      }
    } else {
      int start, end;
      start = ParagraphStartPosition(i);
      end = ParagraphEndPosition(i);
      NeedRefresh(start, end);
    }

    RefreshByLineDemand();
  }
}

void wxMediaEdit::SetParagraghAlignment(long i, int align)
{
  wxMediaLine *l;
  wxMediaParagraph *p;

  switch(align) {
  case 1:
    align = WXPARA_RIGHT;
    break;
  case 0:
    align = WXPARA_CENTER;
    break;
  case -1:
  default:
    align = WXPARA_LEFT;
    break;
  }
  
  if (i < 0)
    i = 0;
  
  l = lineRoot->FindParagraph(i);
  if (l) {
    int start, end;

    p = l->paragraph->Clone();
    l->paragraph = p;

    p->alignment = align;

    start = ParagraphStartPosition(i);
    end = ParagraphEndPosition(i);
    NeedRefresh(start, end);

    RefreshByLineDemand();
  }
}
