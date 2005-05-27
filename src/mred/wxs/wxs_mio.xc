
@INCLUDE prefix.xci

#include "wx_obj.h"
#include "wx_medio.h"

@INCLUDE wxs.xci

@HEADER

@MACRO rZERO = return 0;

@SET TYPE = char
@SET NOTEST = 1
@SET SIZEISLONG = 1
@INCLUDE list.xci

@CLASSBASE wxMediaStreamInBase "editor-stream-in-base" : "object"

static char *VectorToArray(char *r, Scheme_Object *vec, long *len)
{
  long c, i;
  Scheme_Object **a = NULL;
  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH(0, r);
  VAR_STACK_PUSH(1, vec);
  VAR_STACK_PUSH(2, a);

  if (!SCHEME_VECTORP(vec))
    WITH_VAR_STACK(scheme_wrong_type(METHODNAME("editor-stream-in-base%","read"), 
		                     "character vector", -1, 0, &vec));

  c = *len = SCHEME_VEC_SIZE(vec);

  if (!r)
    r = WITH_VAR_STACK((char *)scheme_malloc_atomic(c));

  for (a = SCHEME_VEC_ELS(vec), i = 0; i < c; i++) {
    if (!SCHEME_CHARP(a[i]))
      WITH_VAR_STACK(scheme_wrong_type(METHODNAME("editor-stream-in-base%","read"), 
				       "character vector", -1, 0, &vec));
    r[i] = SCHEME_CHAR_VAL(a[i]);
  }

  READY_TO_RETURN;
  return r;
}

static Scheme_Object *ArrayToVector(char *r, Scheme_Object *vec, long len)
{
  long i;
  Scheme_Object **a = NULL;
  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH(0, r);
  VAR_STACK_PUSH(1, vec);
  VAR_STACK_PUSH(2, a);

  if (!vec)
    vec = WITH_VAR_STACK(scheme_make_vector(len, WITH_VAR_STACK(scheme_make_char(0))));
  else if (!SCHEME_VECTORP(vec))
    WITH_VAR_STACK(scheme_wrong_type(METHODNAME("editor-stream-in-base%","read"), 
		                     "character vector", -1, 0, &vec));
  
  for (a = SCHEME_VEC_ELS(vec), i = 0; i < len; i++)
    a[i] = WITH_VAR_STACK(scheme_make_char(r[i]));

  READY_TO_RETURN;

  return vec;
}

@MACRO setNULL = NULL
@MACRO arrayToVector = p[POFFSET] = ArrayToVector(x0, NULL, x1);
@MACRO copyArrayToVector = ArrayToVector(x0, p[POFFSET], x1);
@MACRO vectorToArray = x0 = VectorToArray(NULL, p[POFFSET], &x1);
@MACRO copyVectorToArray = VectorToArray(x0, p[POFFSET], &x1);

@CREATOR ();

@ V "tell" : long Tell(); : : : rZERO
@ V "seek" : void Seek(nnlong);
@ V "skip" : void Skip(nnlong);
@ V "bad?" : bool Bad(); : : : rZERO
@ V "read" : long Read(char[]/setNULL/setNULL////push,-long); : /arrayToVector/copyVectorToArray : /vectorToArray/copyArrayToVector : rZERO

@END

@CLASSBASE wxMediaStreamOutBase "editor-stream-out-base" : "object"

@CREATOR ();

@ V "tell" : long Tell(); : : : rZERO
@ V "seek" : void Seek(nnlong);
@ V "bad?" : bool Bad(); : : : rZERO
@ V "write" : void Write(char[]/bList/ubList/cList///push,-long); : /methListSet[char.0.0.1] : /glueListSet[char.0.0.1.METHODNAME("editor-stream-out-base%","write")]

@END


@CLASSBASE wxMediaStreamInStringBase "editor-stream-in-bytes-base" : "editor-stream-in-base"

@MACRO setStringSize[ss.cn] = x<cn> = SCHEME_BYTE_STRTAG_VAL(p[POFFSET+<ss>]);

@CREATOR (bstring,-long); : : /setStringSize[0.1]

@END

@CLASSBASE wxMediaStreamOutStringBase "editor-stream-out-bytes-base" : "editor-stream-out-base"

@CREATOR ()

@MACRO makeSizedString = (r ? scheme_make_sized_byte_string(r, _x0, 0) : XC_SCHEME_NULL)

@ "get-bytes" : nbstring/makeSizedString GetString(-long*);

@END

static long GetExact(wxMediaStreamIn *s)
{
  long l;
  s->Get(&l);
  return l;
}
static double GetInexact(wxMediaStreamIn *s)
{
  double d;
  s->Get(&d);
  return d;
}

#define GET Get

@CLASSBASE wxMediaStreamIn "editor-stream-in" : "object"

@CREATOR (wxMediaStreamInBase!);
  
@ "get" : wxMediaStreamIn! Get(Long*////long); <> exact number
@ "get" : wxMediaStreamIn! Get(Double*); <> inexact number

@MACRO alwaysPassPtr = x0 = &_x0;

/* Subtract 1 here because the nul terminator is already included */
@MACRO makeSizedStringX = (r ? scheme_make_sized_byte_string(r, _x0 ? _x0 - 1 : 0, 0) : XC_SCHEME_NULL)

@ "get-bytes" : nbstring/makeSizedStringX GetString(nnlong?=NULL); : : /alwaysPassPtr/
@ "get-unterminated-bytes" : nbstring/makeSizedString GetStringPlusOne(nnlong?=NULL); : : /alwaysPassPtr/
@ "get-fixed" : wxMediaStreamIn! GetFixed(long*);

@ m "get-exact" : long GetExact();
@ m "get-inexact" : double GetInexact();

@ "set-boundary" : void SetBoundary(nnlong);
@ "remove-boundary" : void RemoveBoundary();

@ "skip" : void Skip(nnlong);
@ "tell" : long Tell();
@ "jump-to" : void JumpTo(nnlong);

@ "ok?" : bool Ok();

@END

#define PUT Put

@MACRO CheckBytesLength = if ((x0 - 1) > SCHEME_BYTE_STRTAG_VAL(p[POFFSET+1])) WITH_VAR_STACK(scheme_arg_mismatch(METHODNAME("editor-stream-out","put"), "byte length too large: ", p[POFFSET]));
@MACRO SetBytesLength = x0 = (SCHEME_BYTE_STRTAG_VAL(p[POFFSET+0]) + 1);
@CLASSBASE wxMediaStreamOut "editor-stream-out" : "object"

@CREATOR (wxMediaStreamOutBase!);

@ "put" : wxMediaStreamOut! Put(nnint////long,bstring); : : /CheckBytesLength/ <> length and byte string 
@ "put" : wxMediaStreamOut! Put(-long,bstring); : : /SetBytesLength/ <> byte string without length
@ "put" : wxMediaStreamOut! Put(Long////long); <> exact number
@ "put" : wxMediaStreamOut! Put(Double); <> inexact number

@ "put-fixed" : wxMediaStreamOut! PutFixed(long);

@ "tell" : long Tell();
@ "jump-to" : void JumpTo(nnlong);

@ "ok?" : bool Ok();

@END

