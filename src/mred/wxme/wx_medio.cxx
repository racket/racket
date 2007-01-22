
#if defined(_MSC_VER) && defined(MZ_PRECISE_GC)
# include "wx.h"
#endif
#include "common.h"

#include <stdio.h>

#include "wx_dialg.h"
#ifndef OLD_WXWINDOWS
// wxWindows 1.62(f): new include for common dialogs
#include "wx_cmdlg.h"
#endif

#include "wx_media.h"
#include <string.h>

static int lsb_first;

/* For testing and debugging: */
#define TYPESAFE 0

extern void wxmeError(const char *e);

extern "C" Scheme_Object *scheme_read_byte_string(Scheme_Object *port);

enum {
  st_STRING,
  st_NUMBER,
  st_FLOAT,
  st_FIXED
};

/* 
   Integer format specified by first byte:
     bit 8: 0 - read 7-bit (positive) number
     bit 8: 1 - ...
        bit 7: 0 - read abother byte for 15-bit (positive) number
	bit 7: 1 - negative and long numbers...
	 bit 1: 1 - read another 8-bit (signed) number
	 bit 1: 0 - ...
	   bit 2: 1 - read another 16-bit (signed) number
	   bit 2: 0 - read another 32-bit (signed) number
*/

void wxMediaIOCheckLSB(void)
{
  long v = 1;

  lsb_first = *(char *)&v;
}

/****************************************************************/

wxMediaStream::wxMediaStream()
{
  wxStandardSnipClassList *_scl;
  wxBufferDataClassList *_bdl;

  _scl = &wxTheSnipClassList;
  scl = _scl;
  _bdl = &wxTheBufferDataClassList;
  bdl = _bdl;
}

wxMediaStream::~wxMediaStream()
{
}

int wxMediaStream::ReadingVersion(wxSnipClass *sclass)
{
  wxSnipClassLink *asl;
  
  for (asl = sl; asl; asl = asl->next) {
    if (asl->c == sclass)
      return asl->readingVersion;
  }

  /* Class didn't show up in the header?
     Assume we're reading the current version. */
  return sclass->version;
}

int wxMediaStream::MapPosition(wxSnipClass *c)
{
  wxSnipClassLink *asl;
  
  for (asl = sl; asl; asl = asl->next) {
    if (asl->c == c)
      return asl->mapPosition;
  }

  return -1;
}

int wxMediaStream::MapPosition(wxBufferDataClass *d)
{
  wxDataClassLink *adl;
  
  for (adl = dl; adl; adl = adl->next) {
    if (adl->d == d)
      return adl->mapPosition;
  }

  return -1;
}

int wxMediaStream::GetHeaderFlag(wxSnipClass *c)
{
  wxSnipClassLink *asl;
  
  for (asl = sl; asl; asl = asl->next) {
    if (asl->c == c)
      return asl->headerFlag;
  }

  return 0;
}

void wxMediaStream::SetHeaderFlag(wxSnipClass *c)
{
  wxSnipClassLink *asl;
  
  for (asl = sl; asl; asl = asl->next) {
    if (asl->c == c) {
      asl->headerFlag = 1;
      return;
    }
  }  
}


/****************************************************************/

wxMediaStreamInFileBase::wxMediaStreamInFileBase(Scheme_Object *s)
{
  f = s;
}

wxMediaStreamInFileBase::~wxMediaStreamInFileBase()
{
}

long wxMediaStreamInFileBase::Tell(void)
{
  return scheme_set_file_position(f, -1);
}

void wxMediaStreamInFileBase::Seek(long p)
{
  scheme_set_file_position(f, p);
}

void wxMediaStreamInFileBase::Skip(long n)
{
  scheme_set_file_position(f, n + scheme_set_file_position(f, -1));
}

Bool wxMediaStreamInFileBase::Bad(void)
{
  return FALSE;
}

long wxMediaStreamInFileBase::Read(char *data, long len, long delta)
{
  if (len <= 0)
    return 0;

  return scheme_get_byte_string("read in editor-stream-in%", f, data, delta, len, 0, 0, NULL);
}

/****************************************************************/

void wxMediaStreamOutBase::Write(char *data, long len, int delta)
{
#ifdef MZ_PRECISE_GC
  if (delta) {
    char *d;
    d = new WXGC_ATOMIC char[len];
    memcpy(d, data + delta, len);
    delta = 0;
    data = d;
  }
#endif
  Write(data + delta, len);
}

/****************************************************************/

wxMediaStreamOutFileBase::wxMediaStreamOutFileBase(Scheme_Object *s)
{
  f = s;
}

wxMediaStreamOutFileBase::~wxMediaStreamOutFileBase()
{
}

long wxMediaStreamOutFileBase::Tell(void)
{
  return scheme_set_file_position(f, -1);
}

void wxMediaStreamOutFileBase::Seek(long p)
{
  scheme_set_file_position(f, p);
}

Bool wxMediaStreamOutFileBase::Bad(void)
{
  return FALSE;
}

void wxMediaStreamOutFileBase::Write(char *data, long len)
{
  Write(data, len, 0);
}

void wxMediaStreamOutFileBase::Write(char *data, long len, int delta)
{
  if (len <= 0)
    return;

  scheme_put_byte_string("write in editor-stream-out%", f, data, delta, len, 0);
}

/****************************************************************/

wxMediaStreamInStringBase::wxMediaStreamInStringBase(char *s, long l)
{
  a_string = s;
  len = l;
  pos = 0;
  bad = FALSE;
}

wxMediaStreamInStringBase::~wxMediaStreamInStringBase()
{
}

long wxMediaStreamInStringBase::Tell(void)
{
  return pos;
}

void wxMediaStreamInStringBase::Seek(long p)
{
  if (p < 0)
    p = 0;
  else if (p < len)
    pos = p;
  else
    pos = len;
}

void wxMediaStreamInStringBase::Skip(long n)
{
  pos += n;
  if (pos > len)
    pos = len;
  else if (pos < 0)
    pos = 0;
}

Bool wxMediaStreamInStringBase::Bad(void)
{
  return bad;
}

long wxMediaStreamInStringBase::Read(char *data, long l, long delta)
{
  if (l + pos > len) {
    bad = TRUE;
    l = len - pos;
  }

  memcpy(data + delta, a_string + pos, l);
  pos += l;

  return l;
}

/****************************************************************/

wxMediaStreamOutStringBase::wxMediaStreamOutStringBase()
{
  alloc = 50;
  len = pos = 0;
  a_string = new WXGC_ATOMIC char[alloc];
  bad = FALSE;
}

wxMediaStreamOutStringBase::~wxMediaStreamOutStringBase()
{
}

char *wxMediaStreamOutStringBase::GetString(long *l)
{
  *l = len;
  return a_string;
}

long wxMediaStreamOutStringBase::Tell(void)
{
  return pos;
}

void wxMediaStreamOutStringBase::Seek(long p)
{
  if (p < 0)
    pos = 0;
  else if (p > len)
    pos = len;
  else
    pos = p;
}

Bool wxMediaStreamOutStringBase::Bad(void)
{
  return bad;
}

void wxMediaStreamOutStringBase::Write(char *data, long l, int delta)
{
  if (l + pos > alloc) {
    char *old = a_string;

    alloc = (alloc * 2) + l;
    a_string = new WXGC_ATOMIC char[alloc];
    memcpy(a_string, old, len);
  }

  memcpy(a_string + pos, data + delta, l);
  pos += l;
  if (len < pos)
    len = pos;
}

void wxMediaStreamOutStringBase::Write(char *data, long l)
{
  Write(data, l, 0);
}

/****************************************************************/

wxMediaStreamIn::wxMediaStreamIn(wxMediaStreamInBase *s)
{
  f = s;
  boundalloc = 10;
  boundcount = 0;
  boundaries = new WXGC_ATOMIC long[boundalloc];
  bad = FALSE;
}

wxMediaStreamIn::~wxMediaStreamIn()
{
}

#ifdef TYPESAFE
# define WX_TYPESAFE_USED(x) x
#else
# define WX_TYPESAFE_USED(x) WXUNUSED(x)
#endif

void wxMediaStreamIn::Typecheck(char WX_TYPESAFE_USED(v))
{
  if (bad)
    return;

  if (boundcount && (Tell() >= boundaries[boundcount - 1])) {
    bad = TRUE;
    wxmeError("editor-stream-in%: overread (caused by file corruption?)");
    return;
  }

  bad = f->Bad();

  if (bad) {
    wxmeError("editor-stream-in%: stream error");
    return;
  }

#if TYPESAFE
  char t;

  f->Read(&t, 1);

  if (bad = f->Bad()) {
    wxmeError("editor-stream-in%: stream error");
    return;
  }

  bad = (t != v);

  if (bad)
    wxmeError("editor-stream-in%: type safety error");
#endif
}

#if 0
# define BAD_PRINTF(x) printf x
#else
# define BAD_PRINTF(x) /* disabled */
#endif

char wxMediaStreamIn::SkipWhitespace(char *_buf)
{
  char buf[1];

  if (bad)
    return 0;

  do {
    if (f->Read(buf, 1) != 1) {
      BAD_PRINTF(("bad 1\n"));
      bad = 1;
      break;
    }
    if (buf[0] == '#') {
      int pos;
      pos = f->Tell();
      if ((f->Read(buf, 1) == 1) && (buf[0] == '|')) {
	/* Skip to end of comment */
	int saw_bar = 0, saw_hash = 0, nesting = 0;
	while (1) {
	  if (f->Read(buf, 1) != 1) {
	    BAD_PRINTF(("bad 1.1\n"));
	    bad = 1;
	    break;
	  }
	  if (saw_bar && (buf[0] == '#')) {
	    buf[0] = ' ';
	    if (nesting) {
	      --nesting;
	    } else
	      break;
	  } else if (saw_hash && (buf[0] == '|')) {
	    nesting++;
	    buf[0] = 0; /* So the bar doesn't count for closing */
	  }
	  saw_bar = (buf[0] == '|');
	  saw_hash = (buf[0] == '#');
	}
	if (bad)
	  break;
      } else {
	f->Seek(pos);
	buf[0] = '#';
      }
    } else if (buf[0] == ';') {
      /* Skip to end of comment */
      while (1) {
	if (f->Read(buf, 1) != 1) {
	  BAD_PRINTF(("bad 1.1\n"));
	  bad = 1;
	  break;
	}
	if ((buf[0] == '\n') || (buf[0] == '\r'))
	  break;
      }
      buf[0] = ' ';
    }
  } while (scheme_isspace(((unsigned char *)buf)[0]));

  if (_buf)
    _buf[0] = buf[0];

  return buf[0];
}

int wxMediaStreamIn::IsDelim(char c)
{
  if (scheme_isspace((unsigned char)c))
    return 1;
  else if (c == '#') {
    long pos;
    char next[1];
    pos = f->Tell();
    f->Read(next, 1);
    if (next[0] == '|') {
      f->Seek(pos - 1);
      return 1;
    } else {
      f->Seek(pos);
      return 0;
    }
  } else if (c == ';') {
    long pos;
    pos = f->Tell();
    f->Seek(pos - 1);
    return 1;
  } else
    return 0;
}

void wxMediaStreamIn::GetNumber(long *_v, double *_fv)
{
  char buf[50];
  int cnt = 1;

  SkipWhitespace(buf);
  if (bad)
    cnt = 50;
  
  while (cnt < 50) {
    if (f->Read(buf, 1, cnt) != 1) {
      /* Assuming EOF */
      break;
    }
    if (IsDelim(buf[cnt]))
      break;
    cnt++;
  }

  if (cnt == 50) {
    BAD_PRINTF(("bad 3\n"));
    bad = 1;
    if (_v)
      *_v = 0;
    if (_fv)
      *_fv = 0.0;
  } else {
    buf[cnt] = 0;
    if (_fv) {
      double fv;
      GC_CAN_IGNORE char *p;
      fv = strtod(buf, &p);
      *_fv = fv;
    } else {
      long v = 0;
      int i = 0, negate;

      if (buf[i] == '-') {
	negate = 1;
	i = 1;
      } else {
	negate = 0;
      }
      if (cnt > 11) {
	BAD_PRINTF(("bad 4 %d %s\n", cnt, buf));
	bad = 1;
      }

      for (; buf[i]; i++) {
	if ((buf[i] >= '0' && (buf[i] <= '9'))) {
	  v = (v * 10) + (buf[i] - '0');
	} else {
	  bad = 1;
	  BAD_PRINTF(("bad 6 %c\n", buf[i]));
	}
      }
      if (negate)
	v = -v;

      *_v = v;
    }
  }

  IncItemCount();
}

char *wxMediaStreamIn::GetAString(long *n, long limit, char *target, int extra, int recur)
{
  char *s, buf[32];
  int alloc = 32, size = 0;
  Scheme_Object *port, *str;
  long len, orig_len, get_amt, got_amt;

  if (recur) {
    if (limit < 16)
      orig_len = limit;
    else
      orig_len = 16;
  } else {
    Get(&orig_len);
  }
  get_amt = orig_len + 1;

  if (recur) {
    buf[0] = '#';
  } else {
    SkipWhitespace(buf);
    if (bad)
      buf[0] = 0;
  }

  if (buf[0] == '#') {
    if (f->Read(buf, 1, 1) == 1) {
      if (buf[1] == '"') {
	size = 0;
	s = buf;
	while (1) {
	  if ((size + get_amt + 1) >= alloc) {
	    char *naya;
	    do {
	      alloc *= 2;
	    } while (alloc <= (size + get_amt + 1));
	    naya = new WXGC_ATOMIC char[alloc];
	    memcpy(naya, s, size);
	    s = naya;
	  }
	  got_amt = f->Read(s, get_amt, size);
	  if (got_amt == get_amt) {
	    int i, eos = 0, orig_size = size;
	    for (i = 0; i < get_amt; ) {
	      if (s[orig_size + i] == '"') {
		size++;
		i++;
		eos = 1;
		break;
	      } else if (s[orig_size + i] == '\\') {
		if (i + 1 >= get_amt) {
		  if (f->Read(s, 1, orig_size + i + 1) != 1) {
		    bad = 1;
		    BAD_PRINTF(("bad 8\n"));
		    break;
		  }
		  i++;
		} else
		  i += 2;
		size += 2;
	      } else {
		size++;
		i++;
	      }
	    }
	    if (i < get_amt) {
	      bad = 1;
	      BAD_PRINTF(("bad 8.5 %d %ld %ld %s\n", i, get_amt, limit, s));
	    }
	    get_amt = 1;
	    if (eos || bad)
	      break;
	  } else {
	    bad = 1;
	    BAD_PRINTF(("bad 9 %ld %ld\n", get_amt, got_amt));
	    break;
	  }
	}

	if (!bad) {
	  if (!recur)
	    IncItemCount();

	  port = scheme_make_sized_byte_string_input_port(s, size);
	  str = scheme_read_byte_string(port);
      
	  if (str) {
	    if (recur) {
	      return (char *)str;
	    } else {
	      len = SCHEME_BYTE_STRLEN_VAL(str);

	      if (len == orig_len) {
		if (target) {
		  long amt;
		  amt = ((len > limit) ? limit : len);
		  memcpy(target, SCHEME_BYTE_STR_VAL(str), amt);
		  *n = amt;
		  return target;
		} else {
		  /* extra is either 1 or 0 */
		  if (n)
		    *n = len + extra;
		  return SCHEME_BYTE_STR_VAL(str);
		}
	      } else {
		BAD_PRINTF(("bad 9.9 %ld %ld %s\n", orig_len, len, SCHEME_BYTE_STR_VAL(str)));
	      }
	    }
	  }
	  BAD_PRINTF(("bad 10--1\n"));
	}
	BAD_PRINTF(("bad 10-0\n"));
      }
      BAD_PRINTF(("bad 10-1\n"));
    }
    BAD_PRINTF(("bad 10-2\n"));
  } else if (!recur && (buf[0] == '(')) {
    /* Read a sequence of strings */
    Scheme_Object *accum = scheme_null;
    long left_to_get = orig_len;
    while (1) {
      SkipWhitespace(buf);

      if (bad)
	break;

      if (buf[0] == ')') {
	/* Got all byte strings */
	break;
      } else if (buf[0] == '#') {
	str = (Scheme_Object *)GetAString(NULL, left_to_get, NULL, 0, 1);
	if (bad)
	  break;
	accum = scheme_make_pair(str, accum);
	left_to_get -= SCHEME_BYTE_STRLEN_VAL(str);

	if (left_to_get < 0) {
	  BAD_PRINTF(("bad 10.2\n"));
	  bad = 1;
	  break;
	}
      } else {
	BAD_PRINTF(("bad 10.7\n"));
	bad = 1;
	break;
      }
    }

    if (left_to_get) {
      BAD_PRINTF(("bad 10.3\n"));
      bad = 1;
    }

    if (!bad) {
      long amt, i;

      /* Reverse list */
      str = scheme_null;
      while (SCHEME_PAIRP(accum)) {
	str = scheme_make_pair(SCHEME_CAR(accum), str);
	accum = SCHEME_CDR(accum);
      }

      /* Prepare target: */
      if (target) {
	amt = ((orig_len > limit) ? limit : orig_len);
	*n = amt;
      } else {
	/* extra is either 1 or 0 */
	amt = orig_len;
	if (n)
	  *n = orig_len + extra;
	target = new WXGC_ATOMIC char[orig_len + extra];
	if (extra)
	  target[orig_len] = 0;
      }

      /* Copy strings to target: */
      i = 0;
      accum = str;
      while (amt) {
	str = SCHEME_CAR(accum);
	accum = SCHEME_CDR(accum);
	get_amt = SCHEME_BYTE_STRLEN_VAL(str);
	if (get_amt > amt)
	  get_amt = amt;
	memcpy(target + i, SCHEME_BYTE_STR_VAL(str), get_amt);
	i += get_amt;
	amt -= get_amt;
      }

      IncItemCount();

      return target;
    }
    BAD_PRINTF(("bad 10-3\n"));
  } else {
    BAD_PRINTF(("bad 10-4 %d\n", buf[0]));
  }
   
  bad = 1;
  BAD_PRINTF(("bad 10\n"));
  if (n)
    *n = 0;
  return "";
}

void wxMediaStreamIn::IncItemCount()
{
  items++;
  Tell(); /* Adds mapping for items */
}

void wxMediaStreamIn::SkipOne(int recur)
{
  char buf[1];

  if (recur) {
    buf[0] = '#';
  } else {
    SkipWhitespace(buf);
  }

  if (!bad) {
    if (buf[0] == '#') {
      /* Byte string */
      if (f->Read(buf, 1) == 1) {
	if (buf[0] != '"') {
	  bad = 1;
	  BAD_PRINTF(("bad 12\n"));
	} else {
	  while (1) {
	    if (f->Read(buf, 1) != 1) {
	      bad = 1;
	      BAD_PRINTF(("bad 13\n"));
	      break;
	    }
	    if (buf[0] == '"') {
	      break;
	    } else if (buf[0] == '\\') {
	      if (f->Read(buf, 1) != 1) {
		bad = 1;
		BAD_PRINTF(("bad 14\n"));
		break;
	      }
	    }
	  }
	}
      } else {
	bad = 1;
	BAD_PRINTF(("bad 15\n"));
      }
    } else if (buf[0] == '(') {
      /* List of byte strings */
      while (!bad) {
	do {
	  if (f->Read(buf, 1) != 1) {
	    bad = 1;
	    BAD_PRINTF(("bad 16\n"));
	    break;
	  }
	} while (!IsDelim(buf[0]));
	if (buf[0] == ')')
	  break;
	else if (buf[0] == '#') {
	  SkipOne(TRUE);
	} else {
	  bad = 1;
	  break;
	}
      }
    } else {
      /* Number */
      do {
	if (f->Read(buf, 1) != 1) {
	  bad = 1;
	  BAD_PRINTF(("bad 16\n"));
	  break;
	}
      } while (!IsDelim(buf[0]));
    }

    if (!bad && !recur)
      IncItemCount();
  }
}

wxMediaStreamIn *wxMediaStreamIn::GetFixed(long *v)
{
  Typecheck(st_FIXED);

  if (bad) {
    *v = 0;
    return this;
  }

  if (WXME_VERSION_BEFORE_EIGHT(this)) {
    if (!lsb_first) {
      if (f->Read((char *)v, sizeof(long)) != sizeof(long)) {
	*v = 0;
	bad = 1;
	BAD_PRINTF(("bad 17\n"));
      }
    } else {
      if (WXME_VERSION_ONE(this)) {
	if (f->Read((char *)v, sizeof(long)) != sizeof(long)) {
	  bad = 1;
	  BAD_PRINTF(("bad 18\n"));
	  *v = 0;
	}
      } else {
	unsigned char bl[4];
      
	if (f->Read((char *)bl, 4) != 4) {
	  bad = 1;
	  BAD_PRINTF(("bad 19\n"));
	  *v = 0;
	} else {
	  *v = ((((long)bl[0]) << 24) + (((long)bl[1]) << 16)
		+ (((long)bl[2]) << 8) + bl[3]);
	}
      }
    }
  } else {
    GetNumber(v, NULL);
  }

  return this;
}

extern void *wxMallocAtomicIfPossible(size_t s);

char *wxMediaStreamIn::GetString(long *n, int extra)
{
  long m;
  char *r;

  if (bad) {
    if (n)
      *n = 0;
    return NULL;
  }

  if (WXME_VERSION_BEFORE_EIGHT(this)) {
    Get(&m);

    Typecheck(st_STRING);

    r = (char *)wxMallocAtomicIfPossible(m + extra);
    if (!r) {
      wxmeError("editor-stream-in%: string too large (out of memory) while reading stream");
      bad = 1;
      BAD_PRINTF(("bad 20\n"));
      if (n)
	*n = 0;
      return NULL;
    }
    if (extra)
      r[m] = 0;

    if (f->Read(r, m) != m) {
      bad = 1;
      BAD_PRINTF(("bad 21\n"));
      m = 0;
    }
    if (n)
      *n = m;
  } else {
    r = GetAString(n, -1, NULL, extra, 0);
  }

  return r;
}

char *wxMediaStreamIn::GetStringPlusOne(long *n)
{
  char *s;
  s = GetString(n, 1);
  if (n && *n)
    *n = (*n - 1);
  return s;
}

wxMediaStreamIn *wxMediaStreamIn::Get(long *n, char *str)
{
  long m;

  if (bad) {
    *n = 0;
    return this;
  }

  if (WXME_VERSION_BEFORE_EIGHT(this)) {
    Get(&m);

    Typecheck(st_STRING);

    if (m <= *n) {
      if (f->Read(str, m) != m) {
	bad = 1;
	BAD_PRINTF(("bad 22\n"));
	m = 0;
      }
    } else {
      int d;
      d = f->Read(str, *n);
      if (d != *n) {
	bad = 1;
	BAD_PRINTF(("bad 23\n"));
	m = 0;
      } else {
	f->Skip(m - *n);
      }
    }
    *n = m;
  } else {
    GetAString(n, *n, str, 0, 0);
  }

  return this;
}

wxMediaStreamIn* wxMediaStreamIn::Get(long *v)
{
  char b;
  
  Typecheck(st_NUMBER);

  if (bad) {
    *v = 0;
    return this;
  }

  if (WXME_VERSION_BEFORE_EIGHT(this)) {
    if (f->Read((char *)&b, sizeof(char)) != sizeof(char)) {
      bad = 1;
      BAD_PRINTF(("bad 24\n"));
      b = 0;
    }

    if (b & 0x80) {
      if (b & 0x40) {
	if (b & 0x1) {
	  signed char bv;
	  if (f->Read((char *)&bv, 1) != 1) {
	    bad = 1;
	    BAD_PRINTF(("25\n"));
	    *v = 0;
	  } else
	    *v = bv;
	} else if (b & 0x2) {
	  unsigned char bl[2];
	  if (f->Read((char *)bl, 2) != 2) {
	    bad = 1;
	    BAD_PRINTF(("bad 26\n"));
	    *v = 0;
	  } else
	    *v = (((int)((signed char *)bl)[0]) << 8) + bl[1];
	} else {
	  unsigned char bl[4];
	  if (f->Read((char *)bl, 4) != 4) {
	    bad = 1;
	    BAD_PRINTF(("27\n"));
	    *v = 0;
	  } else
	    *v = (((long)((signed char *)bl)[0]) << 24) 
	      + (((long)bl[1]) << 16)
	      + (((long)bl[2]) << 8) + bl[3];
	}
      } else {
	unsigned char b2;
	if (f->Read((char *)&b2, sizeof(char)) != sizeof(char)) {
	  bad = 1;
	  BAD_PRINTF(("28\n"));
	  *v = 0;
	} else
	  *v = (((int)(b & 0x3F)) << 8) | b2;
      }
    } else
      *v = b;
  } else {
    GetNumber(v, NULL);
  }

  return this;
}

wxMediaStreamIn* wxMediaStreamIn::Get(short *v)
{
  long lv;

  Get(&lv);
  *v = lv;

  return this;
}

wxMediaStreamIn* wxMediaStreamIn::Get(int *v)
{
  long lv;

  Get(&lv);
  *v = lv;

  return this;
}

wxMediaStreamIn* wxMediaStreamIn::Get(char *v)
{
  long lv;

  Get(&lv);
  *v = lv;

  return this;
}

wxMediaStreamIn *wxMediaStreamIn::Get(double *v)
{
  Typecheck(st_FLOAT);

  if (bad) {
    *v = 0.0;
    return this;
  }

  if (WXME_VERSION_BEFORE_EIGHT(this)) {
    if (!lsb_first) {
      if (f->Read((char *)v, sizeof(double)) != sizeof(double)) {
	bad = 1;
	BAD_PRINTF(("bad 29\n"));
	*v = 0.0;
      }
    } else {
      if (WXME_VERSION_ONE(this)) {
	if (f->Read((char *)v, sizeof(double)) != sizeof(double)) {
	  bad = 1;
	  BAD_PRINTF(("bad 30\n"));
	  *v = 0.0;
	}
      } else {
	char num[sizeof(double)], num2[sizeof(double)];
	int i, j;
      
	if (f->Read((char *)num, sizeof(double))  != sizeof(double)) {
	  bad = 1;
	  BAD_PRINTF(("bad 31\n"));
	  *v = 0.0;
	} else {
	  for (i = 0, j = sizeof(double); i < (int)sizeof(double); ) {
	    num2[i++] = num[--j];
	  }
	
	  memcpy((char *)v, num2, sizeof(double));
	}
      }
    }
  } else {
    GetNumber(NULL, v);
  }

  return this;
}

wxMediaStreamIn* wxMediaStreamIn::Get(float *v)
{
  double lv;

  Get(&lv);
  *v = lv;

  return this;
}

void wxMediaStreamIn::SetBoundary(long n)
{
  if (boundcount == boundalloc) {
    long *old = boundaries;
    boundalloc *= 2;
    boundaries = new WXGC_ATOMIC long[boundalloc];
    memcpy(boundaries, old, boundcount * sizeof(long));
  }

  {
    long m;
    m = Tell() + n;
    boundaries[boundcount++] = m;
  }
}

void wxMediaStreamIn::RemoveBoundary()
{
  --boundcount;
}

void wxMediaStreamIn::Skip(long n)
{
  if (WXME_VERSION_BEFORE_EIGHT(this)) {
    f->Skip(n);
  } else {
    JumpTo(n + items);
  }
}

long wxMediaStreamIn::Tell(void)
{
  if (WXME_VERSION_BEFORE_EIGHT(this)) {
    return f->Tell();
  } else {
    long pos;
    Scheme_Hash_Table *ht;
    
    pos = f->Tell();
    
    ht = (Scheme_Hash_Table *)pos_map;
    if (!ht) {
      ht = scheme_make_hash_table(SCHEME_hash_ptr);
      pos_map = (void *)ht;
    }
    
    scheme_hash_set(ht, scheme_make_integer(items),
		    scheme_make_integer_value(pos));
    
    
    return items;
  }
}

void wxMediaStreamIn::JumpTo(long pos)
{
  if (WXME_VERSION_BEFORE_EIGHT(this)) {
    f->Seek(pos);
  } else {
    Scheme_Hash_Table *ht;
    Scheme_Object *p;
    
    ht = (Scheme_Hash_Table *)pos_map;
    if (ht) {
      p = scheme_hash_get(ht, scheme_make_integer(pos));
    } else
      p = NULL;

    if (!p) {
      while ((items < pos) && !bad) {
	SkipOne(FALSE);
      }
      if (items != pos) {
	bad = 1;
	BAD_PRINTF(("bad 32\n"));
      }
      return;
    } else {
      items = (int)pos;
    }
    
    scheme_get_int_val(p, &pos);
    f->Seek(pos);
  }
}

Bool wxMediaStreamIn::Ok(void)
{
  return !bad;
}

/*********************************************************************/

wxMediaStreamOut::wxMediaStreamOut(wxMediaStreamOutBase *s)
{
  f = s;
  bad = FALSE;
  col = 72;
}

void wxMediaStreamOut::Typeset(char WX_TYPESAFE_USED(v))
{
  if (bad)
    return;

  bad = f->Bad();

  if (bad) {
    wxmeError("editor-stream-out%: stream error");
    return;
  }

#if TYPESAFE
  f->Write(&v, 1);
#endif
}

wxMediaStreamOut *wxMediaStreamOut::PutFixed(long v)
{
  char buf[13];
  int spc;

  Typeset(st_FIXED);

  if (col + 12 > 72) {
    col = 11;
    spc = '\n';
  } else {
    spc = ' ';
    col += 12;
  }

  if (v < 0)
    sprintf(buf, " %10.10ld", v);
  else
    sprintf(buf, " %11.11ld", v);
  buf[0] = spc;
  f->Write(buf, 12);

  items++;

  return this;
}

static int estimate_size(char *s, int ds, int n)
{
  int i, c, len = 3;

  for (i = 0; i < n; i++) {
    c = ((unsigned char *)s)[i + ds];
    if (!c)
      len += 2;
    else if (((c >= '_') && (c <= '~'))
	     || ((c >= '#') && (c <= 'Z'))
	     || (c == ' '))
      len++;
    else
      len += 4; /* worst case */
  }

  return len;
}

wxMediaStreamOut* wxMediaStreamOut::Put(long n, char *str, int ds)
{
  long len;
  char *s;
  
  Put(n);

  Typeset(st_STRING);

  len = estimate_size(str, ds, n);

  if (len > 72) {
    /* Single byte string doesn't fit on a line */
    int amt;
    f->Write("\n(", 2);
    while (n) {
      if (n > 32)
	amt = 32;
      else
	amt = n;
      len = estimate_size(str, ds, amt);
      if (len < 71) {
	while (amt < n) {
	  if (estimate_size(str, ds, amt + 1) < 71)
	    amt++;
	  else
	    break;
	}
      } else {
	while (1) {
	  if (estimate_size(str, ds, amt) < 71)
	    break;
	  --amt;
	}
      }

      s = scheme_write_to_string(scheme_make_sized_offset_byte_string(str, ds, amt, 0), &len);
      f->Write("\n ", 2);
      f->Write(s, len);
      ds += amt;
      n -= amt;
    }
    f->Write("\n)", 2);
    col = 1;
  } else {
    s = scheme_write_to_string(scheme_make_sized_offset_byte_string(str, ds, n, 0), &len);
    if (col + len + 1 > 72) {
      f->Write("\n", 1);
      col = 0;
    } else {
      f->Write(" ", 1);
      col++;
    }
    f->Write(s, len);
    // col += len;
    col = 72; /* forcing a newline after every string makes the file more readable */
  }

  items++;

  return this;
}

wxMediaStreamOut *wxMediaStreamOut::Put(char *v)
{
  return Put(strlen(v) + 1, v);
}

wxMediaStreamOut *wxMediaStreamOut::Put(long v)
{
  char buf[13];
  int len;

  Typeset(st_NUMBER);

  sprintf(buf, " %ld", v);

  len = strlen(buf);
  
  if (col + len > 72) {
    col = len - 1;
    buf[0] = '\n';
  } else {
    col += len;
  }

  f->Write(buf, len);

  items++;

  return this;
}

wxMediaStreamOut* wxMediaStreamOut::Put(short v)
{
  return Put((long)v);
}

wxMediaStreamOut* wxMediaStreamOut::Put(int v)
{
  return Put((long)v);
}

wxMediaStreamOut* wxMediaStreamOut::Put(char v)
{
  return Put((long)v);
}

wxMediaStreamOut* wxMediaStreamOut::Put(double v)
{
  int digits, len;
  char buffer[50];

  Typeset(st_FLOAT);

  digits = 14;
  while (digits < 30) {
    double check;
    GC_CAN_IGNORE char *ptr;

    sprintf(buffer, "%.*g", digits, v);
    
    /* Did we get read-write invariance, yet? */
    check = strtod(buffer, &ptr);
    if (check == v)
      break;
    
    digits++;
  }

  len = strlen(buffer);

  if (col + len + 1 > 72) {
    col = len;
    f->Write("\n", 1);
  } else {
    f->Write(" ", 1);
    col += len + 1;
  }
  
  f->Write(buffer, len);

  items++;

  return this;
}

wxMediaStreamOut* wxMediaStreamOut::Put(float v)
{
  return Put((double)v);
}

long wxMediaStreamOut::Tell(void)
{
  long pos;
  Scheme_Hash_Table *ht;

  pos = f->Tell();

  ht = (Scheme_Hash_Table *)pos_map;
  if (!ht) {
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    pos_map = (void *)ht;
  }

  scheme_hash_set(ht, scheme_make_integer(items),
		  scheme_make_pair(scheme_make_integer_value(pos),
				   scheme_make_integer(col)));

  return items;
}

void wxMediaStreamOut::JumpTo(long icount)
{
  long pos;
  Scheme_Hash_Table *ht;
  Scheme_Object *p;

  if (pos_map && !bad) {
    ht = (Scheme_Hash_Table *)pos_map;
    p = scheme_hash_get(ht, scheme_make_integer(icount));

    if (p) {
      scheme_get_int_val(SCHEME_CAR(p), &pos);
      f->Seek(pos);

      p = SCHEME_CDR(p);
      col = SCHEME_INT_VAL(p);

      items = icount;
    }
  }
}

Bool wxMediaStreamOut::Ok(void)
{
  return !bad;
}

void wxMediaStreamOut::PrettyFinish()
{
  if (!bad && col) {
    f->Write("\n", 1);
    col = 0;
  }
}

void wxMediaStreamOut::PrettyStart()
{
  if (!bad) {
    char *s;
    if (col) {
      f->Write("\n", 1);
    }
    s = "#|\n   This file is in PLT Scheme editor format.\n";
    f->Write(s, strlen(s));
    s = "   Open this file in DrScheme version " MZSCHEME_WRITER_VERSION " or later to read it.\n";
    f->Write(s, strlen(s));
    s = "\n";
    f->Write(s, strlen(s));
    s = "   Most likely, it was created by saving a program in DrScheme,\n";
    f->Write(s, strlen(s));
    s = "   and it probably contains a program with non-text elements\n";
    f->Write(s, strlen(s));
    s = "   (such as images or comment boxes).\n";
    f->Write(s, strlen(s));
    s = "\n";
    f->Write(s, strlen(s));
    s = "            http://www.plt-scheme.org\n|#\n";
    f->Write(s, strlen(s));
    col = 0;
  }
}
