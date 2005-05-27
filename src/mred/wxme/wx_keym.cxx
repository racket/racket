/*
 * File:        wx_keym.cc
 * Purpose:     wxKeymap implementation
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
#include "wx_main.h"
#include "wx_keym.h"
#include "wx_mtype.h"
#include "wx_utils.h"
#include <string.h>
#include <ctype.h>
#include "wx_ptreq.h"

extern void wxsKeymapError(char *s);

class wxKMFunc
{
 public:
  char *name;
  wxKMFunction f;
  void *data;

  wxKMFunc(char *name, wxKMFunction f, void *data);
  Bool Call(UNKNOWN_OBJ , wxEvent *);
};

class wxKeycode
{
 public:
  long code;
  int score;

#define TF_Flag(var) unsigned var : 1
  TF_Flag( shiftOn );
  TF_Flag( shiftOff );
  TF_Flag( ctrlOn );
  TF_Flag( ctrlOff );
  TF_Flag( altOn );
  TF_Flag( altOff );
  TF_Flag( metaOn );
  TF_Flag( metaOff );

  TF_Flag( fullset );
#undef TF_Flag

  char *fname;

  Bool isprefix;
  wxKeycode *seqprefix;

  wxKeycode *next;
};

/***************************************************************/

extern int wxMrEdGetDoubleTime(void);

int wxmeGetDoubleClickThreshold()
{
  return wxMrEdGetDoubleTime();
}

wxKeymap::wxKeymap()
 : wxObject(WXGC_NO_CLEANUP)
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_KEYMAP;
#endif

  functions = NULL;
  keys = NULL;

  prefix = NULL;

  active_mouse_function = NULL;

  grabKeyFunction = NULL;
  grabMouseFunction = NULL;
  onBreak = NULL;

  chainCount = 0;
  chainTo = NULL;

  lastButton = 0;

  doubleInterval = wxmeGetDoubleClickThreshold();
}

wxKeymap::~wxKeymap()
{
}

void wxKeymap::Reset(void)
{
  int i;

  prefix = NULL;

  for (i = 0; i < chainCount; i++) {
    chainTo[i]->Reset();  
  }
}

void wxKeymap::BreakSequence(void)
{
  int i;

  prefix = NULL;

  if (onBreak) {
    wxBreakSequenceFunction f;
    void *data;

    f = onBreak;
    data = onBreakData;

    onBreak = NULL;
    onBreakData = NULL;

    f(data);
  }

  for (i = 0; i < chainCount; i++) {
    chainTo[i]->BreakSequence();
  }
}

void wxKeymap::SetBreakSequenceCallback(wxBreakSequenceFunction f, 
					void *data)
{
  wxBreakSequenceFunction fold;
  void *dataold;
  
  fold = onBreak;
  dataold = onBreakData;

  onBreak = f;
  onBreakData = data;

  if (fold)
    fold(dataold);
}

wxKeycode *wxKeymap::FindKey(long code, 
			     Bool shift, Bool ctrl, 
			     Bool alt, Bool meta,
			     wxKeycode *prefix)
{
  wxKeycode *key;
  wxKeycode *bestKey = NULL;
  int bestScore = -1;

  if (!keys)
    return NULL;

  key = (wxKeycode *)keys->Get(code);
  while (key) {
    if (key->code == code
	&& ((key->shiftOn && shift)
	    || (key->shiftOff && !shift)
	    || (!key->shiftOn && !key->shiftOff))
	&& ((key->ctrlOn && ctrl)
	    || (key->ctrlOff && !ctrl)
	    || (!key->ctrlOn && !key->ctrlOff))
	&& ((key->altOn && alt)
	    || (key->altOff && !alt)
	    || (!key->altOn && !key->altOff))
	&& ((key->metaOn && meta)
	    || (key->metaOff && !meta)
	    || (!key->metaOn && !key->metaOff))
	&& key->seqprefix == prefix) {
      int score = key->score;
      if (score > bestScore) {
	bestKey = key;
	bestScore = score;
      }
    }
    key = key->next;
  }

  return bestKey;
}


typedef struct {
  char *str;
  long code;
} Keybind;

static Keybind keylist[] 
            = { { "leftbutton" , WXK_MOUSE_LEFT },
		{ "rightbutton" , WXK_MOUSE_RIGHT },
		{ "middlebutton" , WXK_MOUSE_MIDDLE },
		{ "leftbuttondouble" , WXK_MOUSE_LEFT_DOUBLE },
		{ "rightbuttondouble" , WXK_MOUSE_RIGHT_DOUBLE },
		{ "middlebuttondouble" , WXK_MOUSE_MIDDLE_DOUBLE },
		{ "leftbuttontriple" , WXK_MOUSE_LEFT_TRIPLE },
		{ "rightbuttontriple" , WXK_MOUSE_RIGHT_TRIPLE },
		{ "middlebuttontriple" , WXK_MOUSE_MIDDLE_TRIPLE },
		{ "leftbuttonseq" , WXK_MOUSE_LEFT },
		{ "rightbuttonseq" , WXK_MOUSE_RIGHT },
		{ "middlebuttonseq" , WXK_MOUSE_MIDDLE },
		{ "wheelup" , WXK_WHEEL_UP },
		{ "wheeldown" , WXK_WHEEL_DOWN },
		{ "esc", WXK_ESCAPE }, 
		{ "delete", WXK_DELETE },
		{ "del", WXK_DELETE },
		{ "insert", WXK_INSERT },
		{ "ins", WXK_INSERT },
		{ "add", WXK_ADD },
		{ "subtract", WXK_SUBTRACT },
		{ "multiply", WXK_MULTIPLY },
		{ "divide", WXK_DIVIDE },
		{ "backspace", WXK_BACK },
		{ "back", WXK_BACK },
		{ "return", WXK_RETURN },
		{ "enter", WXK_RETURN },
		{ "tab", WXK_TAB },
		{ "space", WXK_SPACE },
		{ "right", WXK_RIGHT },
		{ "left", WXK_LEFT },
		{ "up", WXK_UP },
		{ "down", WXK_DOWN },
		{ "home", WXK_HOME },
		{ "end", WXK_END },
		{ "pageup", WXK_PRIOR },
		{ "pagedown", WXK_NEXT },
		{ "semicolon", ';' },
		{ "colon", ':' },
		{ "numpad0", WXK_NUMPAD0 },
		{ "numpad1", WXK_NUMPAD1 },
		{ "numpad2", WXK_NUMPAD2 },
		{ "numpad3", WXK_NUMPAD3 },
		{ "numpad4", WXK_NUMPAD4 },
		{ "numpad5", WXK_NUMPAD5 },
		{ "numpad6", WXK_NUMPAD6 },
		{ "numpad7", WXK_NUMPAD7 },
		{ "numpad8", WXK_NUMPAD8 },
		{ "numpad9", WXK_NUMPAD9 },
		{ "numpadenter", 3 },
		{ "f1", WXK_F1 },
		{ "f2", WXK_F2 },
		{ "f3", WXK_F3 },
		{ "f4", WXK_F4 },
		{ "f5", WXK_F5 },
		{ "f6", WXK_F6 },
		{ "f7", WXK_F7 },
		{ "f8", WXK_F8 },
		{ "f9", WXK_F9 },
		{ "f10", WXK_F10 },
		{ "f11", WXK_F11 },
		{ "f12", WXK_F12 },
		{ "f13", WXK_F13 },
		{ "f14", WXK_F14 },
		{ "f15", WXK_F15 },
		{ "f16", WXK_F16 },
		{ "f17", WXK_F17 },
		{ "f18", WXK_F18 },
		{ "f19", WXK_F19 },
		{ "f20", WXK_F20 },
		{ "f21", WXK_F21 },
		{ "f22", WXK_F22 },
		{ "f23", WXK_F23 },
		{ "f24", WXK_F24 },
		{ NULL, 0 }};

wxKeycode *wxKeymap::MapFunction(long code, int shift, int ctrl, 
				 int alt, int meta,
				 char *fname, wxKeycode *prev, int type)
{
  wxKeycode *key, *newkey;

  /* Look for exact key: */
  key = keys ? ((wxKeycode *)keys->Get(code)) : (wxKeycode *)NULL;
  while (key) {
    if (key->code == code
	&& (key->shiftOn == (shift > 0))
	&& (key->shiftOff == (shift < 0))
	&& (key->ctrlOn == (ctrl > 0))
	&& (key->ctrlOff == (ctrl < 0))
	&& (key->altOn == (alt > 0))
	&& (key->altOff == (alt < 0))
	&& (key->metaOn == (meta > 0))
	&& (key->metaOff == (meta < 0))
	&& key->seqprefix == prev)
      break;
    key = key->next;
  }

  if (key) {
    if ((type == wxKEY_PREFIX) != key->isprefix) {
      char buffer[256], modbuf[256], *keystr = NULL;
      int i;
	
      modbuf[0] = 0;
      if (meta > 0)
	strcat(modbuf, "m:");
      if (meta < 0)
	strcat(modbuf, "~m:");

      if (alt > 0)
	strcat(modbuf, "a:");
      if (alt < 0)
	strcat(modbuf, "~a:");

      if (ctrl > 0)
	strcat(modbuf, "c:");
      if (ctrl < 0)
	strcat(modbuf, "~c:");

      if (shift > 0)
	strcat(modbuf, "s:");
      if (shift < 0)
	strcat(modbuf, "~s:");

      for (i = 0; keylist[i].str; i++) {
	if (keylist[i].code == code)
	  keystr = keylist[i].str;
      }

      if (keystr)
	sprintf(buffer, "keymap: \"%s%s\" ", modbuf, keystr);
      else
	sprintf(buffer, "keymap: \"%s%c\" ", modbuf, (char)code);

      strcat(buffer, "is already mapped as a ");
      if (!key->isprefix)
	strcat(buffer, "non-");
      strcat(buffer, "prefix key");
      
      wxsKeymapError(buffer);

      return NULL;
    }  else {
      if (strcmp(key->fname, fname)) {
	key->fname = copystring(fname);
      }
      return key;
    }
  }
  
  newkey = new wxKeycode;

  newkey->code = code;
  newkey->shiftOn = (shift > 0);
  newkey->shiftOff = (shift < 0);
  newkey->ctrlOn = (ctrl > 0);
  newkey->ctrlOff = (ctrl < 0);
  newkey->altOn = (alt > 0);
  newkey->altOff = (alt < 0);
  newkey->metaOn = (meta > 0);
  newkey->metaOff = (meta < 0);
  newkey->score = ((newkey->shiftOn ? 1 : 0)
		   + (newkey->shiftOff ? 5 : 0)
		   + (newkey->ctrlOn ? 1 : 0)
		   + (newkey->ctrlOff ? 5 : 0)
		   + (newkey->altOn ? 1 : 0)
		   + (newkey->altOff ? 5 : 0)
		   + (newkey->metaOn ? 1 : 0)
		   + (newkey->metaOn ? 5 : 0));
  newkey->fullset = 0;
  newkey->fname = copystring(fname);
  newkey->next = NULL;

  newkey->seqprefix = prev;

  newkey->isprefix = (type == wxKEY_PREFIX);

  if (!keys) {
    wxHashTable *ht;
    ht = new wxHashTable(wxKEY_INTEGER, 25);
    keys = ht;
  }

  key = (wxKeycode *)keys->Get(code);
  if (!key)
    keys->Put(code, (wxObject *)newkey);
  else {
    while (key->next) {
      key = key->next;
    }
    key->next = newkey;
  }

  return newkey;
}

static long GetCode(unsigned char *keyseq, int *_kp, int *fullset)
{
  long i, code, kp;
#define MAX_BUF 256
  unsigned char buffer[MAX_BUF], first;

  kp = *_kp;

  first = buffer[0] = keyseq[kp++];
  for (i = 1; keyseq[kp] && (keyseq[kp] != ';'); i++, kp++) {
    if (i >= MAX_BUF - 1)
      return 0;
    buffer[i] = tolower(keyseq[kp]);
  }
  buffer[i] = 0;
  code = 0;
  if (buffer[1]) {
    buffer[0] = tolower(buffer[0]);
    for (i = 0; keylist[i].str; i++) {
      if (!strcmp((char *)buffer, keylist[i].str)) {
	code = keylist[i].code;
	if (!strcmp((char *)buffer, "leftbuttonseq")
	    || !strcmp((char *)buffer, "middlebuttonseq")
	    || !strcmp((char *)buffer, "rightbuttonseq"))
	  *fullset = 1;
	break;
      }
    }
  } else
    code = first;

  *_kp = kp;

  return code;
}

void wxKeymap::MapFunction(char *keys, char *fname)
{
  char *keyseq = keys;
  int num_keys, num_new_keys, kp, start_keys;
  wxKeycode **key, **new_key;
  int shift, ctrl, alt, meta, mod;
  int part = 1, i, j;
  long code;
  int fullset;
  char *errstr;
  char buffer[256];

  num_keys = 1;
  key = new wxKeycode*[1];
  key[0] = NULL;

  start_keys = kp = 0;

  while (keyseq[kp]) {
    shift = ctrl = alt = meta = 0;
    code = 0;
    fullset = 0;

    while (keyseq[kp] && (keyseq[kp] != ';')) {
      mod = 1;
      if ((kp == start_keys) && (keyseq[kp] == ':') && keyseq[kp + 1]) {
	shift = ctrl = alt = meta = -1;
	kp++;
      } else if (keyseq[kp] == '~') {
	if (!keyseq[kp + 1] || (keyseq[kp + 2] != ':')) {
	  goto do_char;
	} else {
	  mod = -1;
	  kp++;
	  goto do_mod;
	}
      } else if (isspace(keyseq[kp])) {
	kp++;
      } else if (keyseq[kp + 1] == ':') {
      do_mod:
	switch (tolower(keyseq[kp])) {
	case 's':
	  shift = mod;
	  break;
	case 'c':
	  ctrl = mod;
	  break;
	case 'm':
#ifdef wx_mac
	  if (mod > 0)
	    return; // impossible
#else
	  meta = mod;
#endif
	  break;
	case 'd':
#ifndef wx_mac
	  if (mod > 0)
	    return; // impossible
#else
	  meta = mod;
#endif
	  break;
	case 'a':
	  alt = mod;
	  break;
	default:
	  errstr = "bad modifier";
	  goto key_error;
	}
	mod = 1;
	kp += 2;
      } else {
      do_char:
	code = GetCode((unsigned char *)keyseq, &kp, &fullset);
	if (!code) {
	  errstr = "bad keyname";
	  goto key_error;
	}
      }
    }

    if (code) {
      if ((code > 0) && (code < 256) && isalpha(code)) {
	if (shift > 0)
	  code = toupper(code);
	else if (isupper(code))
	  shift = TRUE;
      } 

      num_new_keys = num_keys;
      new_key = new wxKeycode*[num_new_keys];

      for (i = 0, j = 0; i < num_keys; i++) {
	wxKeycode *mf;
	mf = MapFunction(code, shift, ctrl, alt, meta, fname, key[i], 
			 keyseq[kp] ? wxKEY_PREFIX : wxKEY_FINAL);
	mf->fullset = fullset;
	new_key[j++] = mf;
      }

      
      num_keys = num_new_keys;
      key = new_key;

      part++;
      if (keyseq[kp])
	kp++;
      start_keys = kp;
    } else {
      errstr = "no non-modifier key";
      goto key_error;
    }

    if (!key)
      return;
  }

  return;

 key_error:
  sprintf(buffer, "keymap: %s in keystring: \"%.100s\", part %d", 
	  errstr, keys, part);
  wxsKeymapError(buffer);
}

int wxKeymap::HandleEvent(long code, Bool shift, Bool ctrl, 
			  Bool alt, Bool meta, int score,
			  char **fname, int *fullset)
{
  wxKeycode *key;

  key = FindKey(code, shift, ctrl, alt, meta, prefix);
  
  prefix = NULL;

  if (key && (key->score >= score)) {
    if (key->isprefix) {
      prefix = key;
      *fname = NULL;
      return 1;
    }
    *fname = key->fname;
    if (fullset)
      *fullset = key->fullset;
    return 1;
  }

  return 0;
}

int wxKeymap::GetBestScore(long code, Bool shift, Bool ctrl, 
			   Bool alt, Bool meta)
{
  wxKeycode *key;
  int s, i;

  key = FindKey(code, shift, ctrl, alt, meta, prefix);

  if (key)
    s = key->score;
  else
    s = -1;

  for (i = 0; i < chainCount; i++) {
    int r;
    r = chainTo[i]->GetBestScore(code, shift, ctrl, alt, meta);
    if (r > s)
      s = r;
  }

  return s;
}

void wxKeymap::SetGrabKeyFunction(wxGrabKeyFunction grab, void *grabData)
{
  grabKeyFunction = grab;
  grabKeyData = grabData;
}

void wxKeymap::RemoveGrabKeyFunction(void)
{
  grabKeyFunction = NULL;
  grabKeyData = NULL;
}

Bool wxKeymap::HandleKeyEvent(UNKNOWN_OBJ media, wxKeyEvent *event)
{
  int score;

  if (event->keyCode == WXK_SHIFT
      || event->keyCode == WXK_CONTROL
      || event->keyCode == WXK_RELEASE
      || !event->keyCode)
    return TRUE;

  score = GetBestScore(event);

  return ChainHandleKeyEvent(media, event, NULL, NULL, 0, score) ? TRUE : FALSE;
}

int wxKeymap::GetBestScore(wxKeyEvent *event)
{
  return GetBestScore(event->keyCode,
		      event->shiftDown,
		      event->controlDown,
		      event->altDown,
		      event->metaDown);
}

int wxKeymap::OtherHandleKeyEvent(UNKNOWN_OBJ media, wxKeyEvent *event,
				  wxGrabKeyFunction grab, void *grabData,
				  int try_state, int score)
{
  int i, result = 0;
  
  for (i = 0; i < chainCount; i++) {
    int r;
    r = chainTo[i]->ChainHandleKeyEvent(media, event, grab, grabData, try_state, score);
    if (r > 0) {
      Reset();
      return r;
    } else if (r)
      result = r;
  }
  
  return result;
}

int wxKeymap::ChainHandleKeyEvent(UNKNOWN_OBJ media, wxKeyEvent *event,
				  wxGrabKeyFunction grab, void *grabData,
				  int try_state, int score)
{
  char *fname;
  int result;

  lastTime = event->timeStamp;
  lastButton = 0;

  if (grabKeyFunction) {
    grab = grabKeyFunction;
    grabData = grabKeyData;
  }

  if (!prefix && (try_state >= 0)) {
    int r;
    r = OtherHandleKeyEvent(media, event, grab, grabData, 1, score);
    
    if (r > 0)
      return r;

    if (try_state > 0)
      return r;
    else
      try_state = -1;
  } else if (prefix && (try_state < 0))
    return OtherHandleKeyEvent(media, event, grab, grabData, -1, score);

  if (HandleEvent(event->keyCode,
		  event->shiftDown,
		  event->controlDown,
		  event->altDown,
		  event->metaDown,
		  score,
		  &fname,
		  NULL)) {
    if (fname) {
      Reset();
      if (grab && grab(fname, this, media, event, grabData))
	return 1;
      return CallFunction(fname, media, event) ? 1 : 0;
    } else {
      if (prefix) {
	/* Just found prefix; try others */
	int r;
	r = OtherHandleKeyEvent(media, event, grab, grabData, try_state, score);
	if (r > 0)
	  return r;
	return -1;
      }
    }
  }

  result = OtherHandleKeyEvent(media, event, grab, grabData, try_state, score);

  if (!result && grabKeyFunction)
    if (grabKeyFunction(NULL, this, media, event, grabKeyData))
      return 1;

  return result;
}

static inline long Abs(long x)
{
  if (x < 0)
    return -x;
  else
    return x;
}

void wxKeymap::SetGrabMouseFunction(wxGrabMouseFunction grab, void *grabData)
{
  grabMouseFunction = grab;
  grabMouseData = grabData;
}

void wxKeymap::RemoveGrabMouseFunction(void)
{
  grabMouseFunction = NULL;
  grabMouseData = NULL;
}

Bool wxKeymap::HandleMouseEvent(UNKNOWN_OBJ media, wxMouseEvent *event)
{
  int score;
  score = GetBestScore(event);

  return ChainHandleMouseEvent(media, event, NULL, NULL, 0, score) ? TRUE : FALSE;
}

int wxKeymap::GetBestScore(wxMouseEvent *event)
{
  long code;

  if (!event->ButtonDown()) {
    int i;
    if (active_mouse_function)
      return 100;
    for (i = 0; i < chainCount; i++) {
      if (chainTo[i]->GetBestScore(event))
	return 100;
    }
    return -1;
  } else {
    if (event->RightDown())
      code = WXK_MOUSE_RIGHT;
    else if (event->LeftDown())
      code = WXK_MOUSE_LEFT;
    else if (event->MiddleDown())
      code = WXK_MOUSE_MIDDLE;
    else
      return -1;
  
    if (code == lastButton && event->x == lastX && event->y == lastY) {
      if (Abs(event->timeStamp - lastTime) < doubleInterval) {
	code += WXK_CLICK_ADDER * clickCount;
      }
    }
  }
  
  return GetBestScore(code,
		      event->shiftDown,
		      event->controlDown,
		      event->altDown,
		      event->metaDown);
}

int wxKeymap::OtherHandleMouseEvent(UNKNOWN_OBJ media, wxMouseEvent *event,
				    wxGrabMouseFunction grab, void *grabData,
				    int try_state, int score)
{
  int i, result = 0;
  
  for (i = 0; i < chainCount; i++) {
    int r;
    r = chainTo[i]->ChainHandleMouseEvent(media, event, grab, grabData, try_state, score);
    if (r > 0) {
      Reset();
      return r;
    } else if (r)
      result = r;
  }
  
  return result;
}

int wxKeymap::ChainHandleMouseEvent(UNKNOWN_OBJ media, wxMouseEvent *event,
				    wxGrabMouseFunction grab, void *grabData,
				    int try_state, int score)
{
  long code, origCode, lastCode;
  char *fname;
  int result, fullset;

  if (grabMouseFunction) {
    grab = grabMouseFunction;
    grabData = grabMouseData;
  }

  if (!prefix && (try_state >= 0)) {
    int r;
    r = OtherHandleMouseEvent(media, event, grab, grabData, 1, score);
    
    if (r > 0)
      return r;

    if (try_state > 0)
      return r;
    else
      try_state = -1;
  } else if (prefix && (try_state < 0))
    return OtherHandleMouseEvent(media, event, grab, grabData, -1, score);

  if (!event->ButtonDown()) {
    Bool v;

    if (!event->Dragging() && !event->ButtonUp()) {
      /* We must have missed the button-up */
      active_mouse_function = NULL;
    }

    if (!active_mouse_function) {
      return OtherHandleMouseEvent(media, event, grab, grabData, -1, score);
    }

    if (grab && grab(active_mouse_function, this, media, event, grabData))
      v = 1;
    else
      v = CallFunction(active_mouse_function, media, event);
    if (event->ButtonUp())
      active_mouse_function = NULL;
    return v;
  }

  if (event->RightDown())
    code = WXK_MOUSE_RIGHT;
  else if (event->LeftDown())
    code = WXK_MOUSE_LEFT;
  else if (event->MiddleDown())
    code = WXK_MOUSE_MIDDLE;
  else
    return 0;

  origCode = code;

  if (code == lastButton && event->x == lastX && event->y == lastY) {
    if (Abs(event->timeStamp - lastTime) < doubleInterval) {
      code += WXK_CLICK_ADDER * clickCount;
      clickCount++;
    } else
      clickCount = 1;
  } else {
    lastButton = code;
    clickCount = 1;
  }
  lastTime = event->timeStamp;
  lastX = event->x;
  lastY = event->y;

  do {
    if (HandleEvent(code,
		    event->shiftDown,
		    event->controlDown,
		    event->altDown,
		    event->metaDown,
		    score,
		    &fname,
		    &fullset)) {
      if (fname) {
	Reset();
	if (fullset)
	  active_mouse_function = fname;
	if (grab && grab(fname, this, media, event, grabData))
	  return 1;
	return CallFunction(fname, media, event) 
	  ? 1 : 0;
      } else {
	int r;
	r = OtherHandleMouseEvent(media, event, grab, grabData, try_state, score);

	if (r > 0)
	  return r;
	return -1;
      }
    }
    lastCode = code;
    code = origCode;
  } while (lastCode != origCode);
  
  result = OtherHandleMouseEvent(media, event, grab, grabData, try_state, score);

  if (!result && grabMouseFunction)
    if (grabMouseFunction(NULL, this, media, event, grabMouseData))
      return 1;
  
  return result;
}

void wxKeymap::AddFunction(char *name, wxKMFunction func, void *data)
{
  wxKMFunc *f;

  if (!functions) {
    wxHashTable *ht;
    ht = new wxHashTable(wxKEY_STRING, 50);
    functions = ht;
  }

  f = new wxKMFunc(name, func, data);
  if (functions->Get(f->name))
    functions->Delete(f->name);
  functions->Put(f->name, (wxObject *)f);
}
  
Bool wxKeymap::CallFunction(char *name, UNKNOWN_OBJ media, wxEvent *event,
			    Bool try_chained)
{
  wxKMFunc *f;

  if (functions) {
    f = (wxKMFunc *)functions->Get(name);
    if (f) {
      f->Call(media, event);
      return TRUE;
    }
  }

  if (try_chained) {
    int i;

    for (i = 0; i < chainCount; i++) {
      if (chainTo[i]->CallFunction(name, media, event, TRUE))
	return TRUE;
    }
  } else {
    char buffer[256];
    sprintf(buffer, "keymap: no function \"%.150s\"", name);
    wxsKeymapError(buffer);
  }

  return 0;
}

long wxKeymap::GetDoubleClickInterval()
{
  return doubleInterval;
}

void wxKeymap::SetDoubleClickInterval(long d)
{
  doubleInterval = d;
}

Bool wxKeymap::CycleCheck(wxKeymap *km)
{
  int i;

  for (i = 0; i < chainCount; i++) {
    if (PTREQ(km, chainTo[i]) || chainTo[i]->CycleCheck(km))
      return TRUE;
  }

  return FALSE;
}

void wxKeymap::ChainToKeymap(wxKeymap *km, Bool prefix)
{
  wxKeymap **old;
  
  if ((km == this) || CycleCheck(km) || km->CycleCheck(this))
    return;

  old = chainTo;
  chainTo = new wxKeymap*[chainCount + 1];

  memcpy(chainTo + (prefix ? 1 : 0), old, chainCount * sizeof(wxKeymap *));
  chainTo[prefix ? 0 : chainCount] = km;

  chainCount++;
}

void wxKeymap::RemoveChainedKeymap(wxKeymap *km)
{
  int i;

  for (i = 0; i < chainCount; i++) {
    if (PTREQ(km, chainTo[i]))
      break;
  }
  
  if (i >= chainCount)
    return;

  memcpy(chainTo + i, chainTo + i + 1, 
	 sizeof(wxKeymap *) * (chainCount - i - 1));

  chainCount--;
}

/***************************************************************/

wxKMFunc::wxKMFunc(char *fname, wxKMFunction func, void *d)
{
  name = copystring(fname);
  f = func;
  data = d;
}

Bool wxKMFunc::Call(UNKNOWN_OBJ media, wxEvent *event)
{
  return f(media, event, data);
}
