
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

long wxMediaStreamInFileBase::Read(char *data, long len)
{
  if (len <= 0)
    return 0;

  return scheme_get_byte_string("read in editor-stream-in%", f, data, 0, len, 0, 0, NULL);
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

long wxMediaStreamInStringBase::Read(char *data, long l)
{
  if (l + pos > len) {
    bad = TRUE;
    l = len - pos;
  }

  memcpy(data, a_string + pos, l);
  pos += l;

  return l;
}

/****************************************************************/

wxMediaStreamOutStringBase::wxMediaStreamOutStringBase()
{
  alloc = 50;
  len = pos = 0;
  a_string = new char[alloc];
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
    a_string = new char[alloc];
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
  boundaries = new long[boundalloc];
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

  if (boundcount && (f->Tell() >= boundaries[boundcount - 1])) {
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

wxMediaStreamIn *wxMediaStreamIn::GetFixed(long *v)
{
  Typecheck(st_FIXED);

  if (bad) {
    *v = 0;
    return this;
  }

  if (!lsb_first) {
    if (f->Read((char *)v, sizeof(long)) != sizeof(long)) {
      *v = 0;
      bad = 1;
    }
  } else {
    if (WXME_VERSION_ONE(this)) {
      if (f->Read((char *)v, sizeof(long)) != sizeof(long)) {
	bad = 1;
	*v = 0;
      }
    } else {
      unsigned char bl[4];
      
      if (f->Read((char *)bl, 4) != 4) {
	bad = 1;
	*v = 0;
      } else {
	*v = ((((long)bl[0]) << 24) + (((long)bl[1]) << 16)
	      + (((long)bl[2]) << 8) + bl[3]);
      }
    }
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

  Get(&m);

  Typecheck(st_STRING);

  r = (char *)wxMallocAtomicIfPossible(m + extra);
  if (!r) {
    wxmeError("editor-stream-in%: string too large (out of memory) while reading stream");
    bad = 1;
    if (n)
      *n = 0;
    return NULL;
  }
  if (extra)
    r[m] = 0;

  if (f->Read(r, m) != m) {
    bad = 1;
    m = 0;
  }
  if (n)
    *n = m;

  return r;
}

char *wxMediaStreamIn::GetStringPlusOne(long *n)
{
  return GetString(n, 1);
}

wxMediaStreamIn *wxMediaStreamIn::Get(long *n, char *str)
{
  long m;

  if (bad) {
    *n = 0;
    return this;
  }

  Get(&m);

  Typecheck(st_STRING);

  if (m <= *n) {
    if (f->Read(str, m) != m) {
      bad = 1;
      m = 0;
    }
  } else {
    int d;
    d = f->Read(str, *n);
    if (d != *n) {
      bad = 1;
      m = 0;
    } else {
      f->Skip(m - *n);
    }
  }
  *n = m;

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

  if (f->Read((char *)&b, sizeof(char)) != sizeof(char)) {
    bad = 1;
    b = 0;
  }

  if (b & 0x80) {
    if (b & 0x40) {
      if (b & 0x1) {
	signed char bv;
	if (f->Read((char *)&bv, 1) != 1) {
	  bad = 1;
	  *v = 0;
	} else
	  *v = bv;
      } else if (b & 0x2) {
	unsigned char bl[2];
	if (f->Read((char *)bl, 2) != 2) {
	  bad = 1;
	  *v = 0;
	} else
	  *v = (((int)((signed char *)bl)[0]) << 8) + bl[1];
      } else {
	unsigned char bl[4];
	if (f->Read((char *)bl, 4) != 4) {
	  bad = 1;
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
	*v = 0;
      } else
	*v = (((int)(b & 0x3F)) << 8) | b2;
    }
  } else
    *v = b;

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

  if (!lsb_first) {
    if (f->Read((char *)v, sizeof(double)) != sizeof(double)) {
      bad = 1;
      *v = 0.0;
    }
  } else {
    if (WXME_VERSION_ONE(this)) {
      if (f->Read((char *)v, sizeof(double)) != sizeof(double)) {
	bad = 1;
	*v = 0.0;
      }
    } else {
      char num[sizeof(double)], num2[sizeof(double)];
      int i, j;
      
      if (f->Read((char *)num, sizeof(double))  != sizeof(double)) {
	bad = 1;
	*v = 0.0;
      } else {
	for (i = 0, j = sizeof(double); i < (int)sizeof(double); ) {
	  num2[i++] = num[--j];
	}
	
	memcpy((char *)v, num2, sizeof(double));
      }
    }
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
    boundaries = new long[boundalloc];
    memcpy(boundaries, old, boundcount * sizeof(long));
  }

  {
    long m;
    m = f->Tell() + n;
    boundaries[boundcount++] = m;
  }
}

void wxMediaStreamIn::RemoveBoundary()
{
  --boundcount;
}

void wxMediaStreamIn::Skip(long n)
{
  f->Skip(n);
}

long wxMediaStreamIn::Tell(void)
{
  return f->Tell();
}

void wxMediaStreamIn::JumpTo(long pos)
{
  f->Seek(pos);
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
  Typeset(st_FIXED);

  if (!lsb_first) {
    f->Write((char *)&v, sizeof(long));
  } else {
    char lb[4];
    
    lb[0] = (v >> 24) & 0xFF;
    lb[1] = (v >> 16) & 0xFF;
    lb[2] = (v >> 8) & 0xFF;
    lb[3] = v & 0xFF;
    f->Write(lb, 4);
  }

  return this;
}

wxMediaStreamOut* wxMediaStreamOut::Put(long n, char *str, int ds)
{
  Put(n);

  Typeset(st_STRING);

  f->Write(str, n, ds);

  return this;
}

wxMediaStreamOut *wxMediaStreamOut::Put(char *v)
{
  return Put(strlen(v) + 1, v);
}

wxMediaStreamOut *wxMediaStreamOut::Put(long v)
{
  Typeset(st_NUMBER);

  if (v >= 0) {
    if (v <= 0x7F) {
      char b = v;
      f->Write(&b, 1);
    } else if (v <= 0x1FFF) {
      unsigned char b[2];
      b[0] = (v >> 8) | 0x80;
      b[1] = v & 0xFF;
      f->Write((char *)b, 2);
    } else {
      char markb = 0xC0;
      unsigned char lb[4];
      lb[0] = (v >> 24) & 0xFF;
      lb[1] = (v >> 16) & 0xFF;
      lb[2] = (v >> 8) & 0xFF;
      lb[3] = v & 0xFF;
      f->Write(&markb, 1);
      f->Write((char *)lb, 4);
    }
  } else {
    char b = 0xC0;
    if (v > ((signed char)0x80)) {
      signed char b2 = v;
      b |= 0x1;
      f->Write(&b, 1);
      f->Write((char *)&b2, 1);
    } else {
      unsigned char lb[4];
      f->Write(&b, sizeof(char));
      ((signed char *)lb)[0] = (v >> 24) & 0xFF;
      lb[1] = (v >> 16) & 0xFF;
      lb[2] = (v >> 8) & 0xFF;
      lb[3] = v & 0xFF;
      f->Write((char *)lb, 4);
    }
  }

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
  Typeset(st_FLOAT);

  if (!lsb_first) {
    f->Write((char *)&v, sizeof(double));
  } else {
    char num[sizeof(double)], num2[sizeof(double)];
    int i, j;
    
    memcpy(num2, (char *)&v, sizeof(double));
    for (i = 0, j = sizeof(double); i < (int)sizeof(double); ) {
      num[i++] = num2[--j];
    }
    
    f->Write((char *)num, sizeof(double));
  }

  return this;
}

wxMediaStreamOut* wxMediaStreamOut::Put(float v)
{
  return Put((double)v);
}

long wxMediaStreamOut::Tell(void)
{
  return f->Tell();
}

void wxMediaStreamOut::JumpTo(long pos)
{
  f->Seek(pos);
}

Bool wxMediaStreamOut::Ok(void)
{
  return !bad;
}
