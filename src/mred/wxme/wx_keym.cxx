/*
 * File:        wx_keym.cc
 * Purpose:     wxKeymap implementation
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
#include "wx_main.h"
#include "wx_keym.h"
#include "wx_mtype.h"
#include "wx_utils.h"
#include <string.h>
#include <ctype.h>
#include "wx_ptreq.h"

extern void wxsKeymapError(char *s);

#ifdef wx_mac
int wxMapCommandAsMeta;
# define AS_META_KEY(k) (wxMapCommandAsMeta ? (k) : 0)
# define AS_CMD_KEY(k) (k)
#else
# define AS_META_KEY(k) (k)
# define AS_CMD_KEY(k) 0
#endif

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
  TF_Flag( cmdOn );
  TF_Flag( cmdOff );
  TF_Flag( capsOn );
  TF_Flag( capsOff );

  TF_Flag( checkOther );

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
  prefixed = 0;

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

wxKeycode *wxKeymap::FindKey(long code, long other_code, long alt_code, long other_alt_code, long caps_code,
			     Bool shift, Bool ctrl, 
			     Bool alt, Bool meta, Bool cmd, Bool caps,
			     wxKeycode *prefix, int *_score)
{
  wxKeycode *key;
  wxKeycode *bestKey = NULL;
  int bestScore = -1;
  int iter;
  long findk;

  if (!keys)
    return NULL;

  for (iter = 0; iter < 5; iter++) {
    switch (iter) {
    case 0:
      findk = code;
      break;
    case 1:
      findk = other_code;
      break;
    case 2:
      findk = alt_code;
      break;
    case 3:
      findk = other_alt_code;
      break;
    case 4:
    default:
      findk = caps_code;
      break;
    }
    key = (wxKeycode *)keys->Get(findk);
    while (key) {
      if (((key->code == code)
	   || (key->checkOther
	       && ((key->code == other_code)
                   || (key->code == alt_code)
                   || (key->code == other_alt_code)
                   || (key->code == caps_code))))
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
	  && ((key->cmdOn && cmd)
	      || (key->cmdOff && !cmd)
	      || (!key->cmdOn && !key->cmdOff))
	  && ((key->capsOn && caps)
	      || (key->capsOff && !caps)
	      || (!key->capsOn && !key->capsOff))
	  && key->seqprefix == prefix) {
	int score = key->score;
        if (key->code != code) {
          if (key->code == other_alt_code)
            score -= 4;
          else
            score -= 2;
        }
	if (score > bestScore) {
	  bestKey = key;
	  bestScore = score;
	}
      }
      key = key->next;
    }
  }

  if (_score)
    *_score = bestScore;

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
				 int alt, int meta, int cmd, int caps, int checkOther,
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
	&& (key->cmdOn == (cmd > 0))
	&& (key->cmdOff == (cmd < 0))
	&& (key->capsOn == (caps > 0))
	&& (key->capsOff == (caps < 0))
	&& (key->checkOther == (checkOther ? 1 : 0))
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

      if (cmd > 0)
	strcat(modbuf, "d:");
      if (cmd < 0)
	strcat(modbuf, "~d:");

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
	sprintf(buffer, "keymap: \"%s%s%s\" ", modbuf, checkOther ? "?" : "", keystr);
      else
	sprintf(buffer, "keymap: \"%s%s%c\" ", modbuf, checkOther ? "?" : "", (char)code);

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
  
  newkey = new WXGC_PTRS wxKeycode;

  newkey->code = code;
  newkey->shiftOn = (shift > 0);
  newkey->shiftOff = (shift < 0);
  newkey->ctrlOn = (ctrl > 0);
  newkey->ctrlOff = (ctrl < 0);
  newkey->altOn = (alt > 0);
  newkey->altOff = (alt < 0);
  newkey->metaOn = (meta > 0);
  newkey->metaOff = (meta < 0);
  newkey->cmdOn = (cmd > 0);
  newkey->cmdOff = (cmd < 0);
  newkey->capsOn = (caps > 0);
  newkey->capsOff = (caps < 0);
  newkey->checkOther = (checkOther ? 1 : 0);
  newkey->score = ((newkey->shiftOn ? 1 : 0)
		   + (newkey->shiftOff ? 5 : 0)
		   + (newkey->ctrlOn ? 1 : 0)
		   + (newkey->ctrlOff ? 5 : 0)
		   + (newkey->altOn ? 1 : 0)
		   + (newkey->altOff ? 5 : 0)
		   + (newkey->metaOn ? 1 : 0)
		   + (newkey->metaOff ? 5 : 0)
		   + (newkey->cmdOn ? 1 : 0)
		   + (newkey->cmdOff ? 5 : 0)
		   + (newkey->capsOn ? 1 : 0)
		   + (newkey->capsOff ? 5 : 0)
		   + (newkey->checkOther ? 6 : 30));
  newkey->fullset = 0;
  newkey->fname = copystring(fname);
  newkey->next = NULL;

  newkey->seqprefix = prev;

  newkey->isprefix = (type == wxKEY_PREFIX);

  if (!keys) {
    wxHashTable *ht;
    ht = new WXGC_PTRS wxHashTable(wxKEY_INTEGER, 25);
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

static int wx_c_strcmp(wxchar *keyseq, char *s)
{
  int i = 0;

  while (keyseq[i] && s[i]) {
    if ((int)keyseq[i] != (int)s[i])
      return 1;
    i++;
  }
  return keyseq[i] || s[i];
}

static long GetCode(wxchar *keyseq, int *_kp, int *fullset)
{
  long i, code, kp;
#define MAX_BUF 256
  wxchar buffer[MAX_BUF], first;

  kp = *_kp;

  first = buffer[0] = keyseq[kp++];
  for (i = 1; keyseq[kp] && (keyseq[kp] != ';'); i++, kp++) {
    if (i >= MAX_BUF - 1)
      return 0;
    buffer[i] = keyseq[kp];
    if (buffer[i] < 128)
      buffer[i] = tolower(buffer[i]);
  }
  buffer[i] = 0;
  code = 0;
  if (buffer[1]) {
    if (buffer[0] < 128)
      buffer[0] = tolower(buffer[0]);
    for (i = 0; keylist[i].str; i++) {
      if (!wx_c_strcmp(buffer, keylist[i].str)) {
	code = keylist[i].code;
	if (!wx_c_strcmp(buffer, "leftbuttonseq")
	    || !wx_c_strcmp(buffer, "middlebuttonseq")
	    || !wx_c_strcmp(buffer, "rightbuttonseq"))
	  *fullset = 1;
	break;
      }
    }
  } else
    code = first;

  *_kp = kp;

  return code;
}

void wxKeymap::MapFunction(wxchar *keys, char *fname)
{
  wxchar *keyseq = keys;
  int num_keys, num_new_keys, kp, start_keys;
  wxKeycode **key, **new_key;
  int shift, ctrl, alt, meta, cmd, caps, mod, checkOther;
  int part = 1, i, j;
  long code;
  int fullset;
  char *errstr;
  char buffer[256];

  num_keys = 1;
  key = new WXGC_PTRS wxKeycode*[1];
  key[0] = NULL;

  start_keys = kp = 0;

  while (keyseq[kp]) {
    shift = ctrl = alt = meta = cmd = caps = 0;
    code = 0;
    fullset = 0;
    checkOther = 0;

    while (keyseq[kp] && (keyseq[kp] != ';')) {
      mod = 1;
      if ((kp == start_keys) && (keyseq[kp] == ':') && keyseq[kp + 1]) {
	shift = ctrl = alt = cmd = meta = -1;
        caps = 0;
	kp++;
      } else if (keyseq[kp] == '~') {
	if (!keyseq[kp + 1] || (keyseq[kp + 2] != ':')) {
	  goto do_char;
	} else {
	  mod = -1;
	  kp++;
	  goto do_mod;
	}
      } else if (keyseq[kp] < 128 && isspace(keyseq[kp])) {
	kp++;
      } else if (keyseq[kp + 1] == ':') {
      do_mod:
	wxchar mch;
	mch = keyseq[kp];
	if (mch < 128)
	  mch = tolower(mch);
	switch (mch) {
	case 's':
	  shift = mod;
	  break;
	case 'c':
	  ctrl = mod;
	  break;
	case 'l':
	  caps = mod;
	  break;
	case 'm':
	  meta = mod;
	  break;
	case 'd':
	  cmd = mod;
	  break;
	case 'a':
	  alt = mod;
	  break;
	case '?':
	  if (mod == 1) {
	    checkOther = 1;
	  } else {
	    errstr = "cannot negate ? modifier";
	    goto key_error;
	  }
	  break;
	default:
	  errstr = "bad modifier";
	  goto key_error;
	}
	mod = 1;
	kp += 2;
      } else {
      do_char:
	code = GetCode(keyseq, &kp, &fullset);
	if (!code) {
	  errstr = "bad keyname";
	  goto key_error;
	}
      }
    }

    if (code) {
      if ((code > 0) && (code < 127) && isalpha(code)) {
	if (shift > 0) {
#ifdef wx_mac
	  if ((meta < 1) && (cmd < 1))
#endif
#if defined(wx_msw)
	    if ((ctrl < 1) || (meta > 0))
#endif
	      code = toupper(code);
	} else if (isupper(code))
	  shift = 1;
      } 

      num_new_keys = num_keys;
      new_key = new WXGC_PTRS wxKeycode*[num_new_keys];

      for (i = 0, j = 0; i < num_keys; i++) {
	wxKeycode *mf;
	mf = MapFunction(code, shift, ctrl, alt, meta, cmd, caps, checkOther, fname, key[i], 
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
  {
    long l;
    char *r = NULL;
    
    wxme_utf8_encode(keys, wxstrlen(keys), &r, &l);
    sprintf(buffer, "keymap: %s in keystring: \"%.100s\", part %d", 
            errstr, r, part);
    wxsKeymapError(buffer);
  }
}

void wxKeymap::MapFunction(char *keys, char *fname)
{
  wxchar *us;
  long ulen;
  
  wxme_utf8_decode(keys, strlen(keys), &us,  &ulen);
  MapFunction(us, fname);
}

int wxKeymap::HandleEvent(long code, long other_code,  long alt_code,  long other_alt_code, long caps_code,
                          Bool shift, Bool ctrl, 
			  Bool alt, Bool meta, Bool cmd, Bool caps, int score,
			  char **fname, int *fullset)
{
  wxKeycode *key;
  int found_score;

  key = FindKey(code, other_code, alt_code, other_alt_code, caps_code,
                shift, ctrl, alt, meta, cmd, caps, prefix, &found_score);
  
  prefix = NULL;

  if (key && (found_score >= score)) {
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

int wxKeymap::GetBestScore(long code, long other_code, long alt_code, long other_alt_code, long caps_code, 
                           Bool shift, Bool ctrl, 
			   Bool alt, Bool meta, Bool cmd, Bool caps)
{
  wxKeycode *key;
  int s, i;
  int score;

  key = FindKey(code, other_code, alt_code, other_alt_code, caps_code,
                shift, ctrl, alt, meta, cmd, caps, prefix, &score);

  if (key)
    s = score;
  else
    s = -1;

  for (i = 0; i < chainCount; i++) {
    int r;
    r = chainTo[i]->GetBestScore(code, other_code, alt_code, other_alt_code, caps_code,
                                 shift, ctrl, alt, meta, cmd, caps);
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
  int score, result, was_prefixed;

  if (event->keyCode == WXK_SHIFT
      || event->keyCode == WXK_CONTROL
      || event->keyCode == WXK_RELEASE
      || !event->keyCode)
    return TRUE;

  score = GetBestScore(event);

  was_prefixed = prefixed;

  result = ChainHandleKeyEvent(media, event, NULL, NULL, prefixed, score);

  if (!result && was_prefixed) {
    Reset();
    /* Try again without prefix: */
    result = ChainHandleKeyEvent(media, event, NULL, NULL, 0, score);
  }

  if (result >= 0)
    Reset();

  return result ? TRUE : FALSE;
}

int wxKeymap::GetBestScore(wxKeyEvent *event)
{
  return GetBestScore(event->keyCode,
		      event->otherKeyCode,
		      event->altKeyCode,
		      event->otherAltKeyCode,
		      event->capsKeyCode,
		      event->shiftDown,
		      event->controlDown,
		      event->altDown,
		      AS_META_KEY(event->metaDown),
		      AS_CMD_KEY(event->metaDown),
		      event->capsDown);
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
				  int only_prefixed, int score)
/* Results: 0 = no match, 1 = match, -1 = matched prefix */
{
  char *fname;
  int result, sub_result;

  lastTime = event->timeStamp;
  lastButton = 0;

  if (grabKeyFunction) {
    grab = grabKeyFunction;
    grabData = grabKeyData;
  }

  if (only_prefixed && !prefixed)
    return 0;

  sub_result = OtherHandleKeyEvent(media, event, grab, grabData, only_prefixed, score);

  if (sub_result > 0)
    return sub_result;

  if (HandleEvent(event->keyCode,
		  event->otherKeyCode,
		  event->altKeyCode,
		  event->otherAltKeyCode,
		  event->capsKeyCode,
		  event->shiftDown,
		  event->controlDown,
		  event->altDown,
		  AS_META_KEY(event->metaDown),
		  AS_CMD_KEY(event->metaDown),
		  event->capsDown,
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
        prefixed = 1;
        return -1;
      }
      /* Huh? */
      result = 0;
    }
  } else
    result = 0;

  if (sub_result < 0) {
    prefixed = 1;
    result = -1;
  }

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
		      -1, -1, -1, -1,
		      event->shiftDown,
		      event->controlDown,
		      event->altDown,
		      AS_META_KEY(event->metaDown),
		      AS_CMD_KEY(event->metaDown),
		      event->capsDown);
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
		    -1, -1, -1, -1,
		    event->shiftDown,
		    event->controlDown,
		    event->altDown,
		    AS_META_KEY(event->metaDown),
		    AS_CMD_KEY(event->metaDown),
		    event->capsDown,
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
    ht = new WXGC_PTRS wxHashTable(wxKEY_STRING, 50);
    functions = ht;
  }

  f = new WXGC_PTRS wxKMFunc(name, func, data);
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
  chainTo = new WXGC_PTRS wxKeymap*[chainCount + 1];

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
