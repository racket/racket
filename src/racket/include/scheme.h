/*
  Racket
  Copyright (c) 2004-2012 PLT Scheme Inc.
  Copyright (c) 1995-2001 Matthew Flatt
  All rights reserved.

  Please see the full copyright in the documentation.

  Originally based on:
  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

#ifndef SCHEME_H
#define SCHEME_H

/* The next line is used and set during installation: */
/*III*/

/*========================================================================*/
/*                           configuration                                */
/*========================================================================*/

/* The configuration is not intended to be adjusted here. Instead,
   modify sconfig.h. The code below simply draws a few more
   configuration conclusions and a few extra macros based on those
   settings. */

#ifdef INCLUDE_WITHOUT_PATHS
# include "sconfig.h"
#else
# include "../sconfig.h"
#endif

#if defined(__MWERKS__)
# ifdef MZSCHEME_USES_NEAR_GLOBALS
#  pragma far_data off
# endif
#endif

#if SGC_STD_DEBUGGING
# ifndef USE_SENORA_GC
#  define USE_SENORA_GC
# endif
# define USE_MEMORY_TRACING
#endif

#ifdef MZ_PRECISE_GC
# define MUST_REGISTER_GLOBALS
# define MZTAG_REQUIRED
# undef UNIX_IMAGE_DUMPS
/* In case SGC is used to build PRECISE_GC: */
# undef USE_SENORA_GC
#endif

#ifdef USE_SENORA_GC
# define MUST_REGISTER_GLOBALS
# undef UNIX_IMAGE_DUMPS
#endif

#ifdef USE_SINGLE_FLOATS
# define MZ_USE_SINGLE_FLOATS
#endif

#ifdef DONT_ITIMER
# undef USE_ITIMER
#endif

#if defined(USE_ITIMER) || defined(USE_WIN32_THREAD_TIMER) || defined(USE_PTHREAD_THREAD_TIMER)
# define FUEL_AUTODECEREMENTS
#endif

#ifdef SIZEOF_VOID_P
# if SIZEOF_VOID_P == 8
#  define SIXTY_FOUR_BIT_INTEGERS
#  ifdef USE_LONG_LONG_FOR_BIGDIG
     Do not specify USE_LONG_LONG_FOR_BIGDIG on a platform with
     64-bit integers
#  endif
# endif
#endif

#ifdef SIZEOF_LONG
# if SIZEOF_LONG == 8
#  define SIXTY_FOUR_BIT_LONGS
# endif
#endif

#ifdef MZ_PRECISE_GC
# define MZ_HASH_KEY_EX  short keyex;
# define MZ_OPT_HASH_KEY_EX /**/
# define MZ_OPT_HASH_KEY(obj) (obj)->so.keyex
#else
# define MZ_HASH_KEY_EX /**/
# define MZ_OPT_HASH_KEY_EX  short keyex;
# define MZ_OPT_HASH_KEY(obj) (obj)->keyex
#endif

#ifdef PALMOS_STUFF
# include <PalmOS.h>
typedef long FILE;
# define _LINUX_TYPES_H  /* Blocks types.h */
#endif

#ifndef SCHEME_DIRECT_EMBEDDED
# define SCHEME_DIRECT_EMBEDDED 1
#endif

#ifndef MSC_IZE
# define MSC_IZE(x) x
#endif
#ifndef MSCBOR_IZE
# define MSCBOR_IZE(x) MSC_IZE(x)
#endif

#ifdef SIGSET_IS_SIGNAL
# define MZ_SIGSET(s, f) signal(s, f)
#else
# define MZ_SIGSET(s, f) sigset(s, f)
#endif

/* C99 allows an array in a struct to be declared
   with [] to indicate that its actual size can be
   any number. The old way was to declare the array
   of size 1. For now, we support going back to the 
   old way. */
#ifdef MZ_USE_OLD_ARRAY_STYLE
# define mzFLEX_ARRAY_DECL 1
# define mzFLEX_DELTA 1
#else
# define mzFLEX_ARRAY_DECL /* empty */
# define mzFLEX_DELTA 0
#endif

#ifdef MZ_XFORM
# define XFORM_NONGCING __xform_nongcing__
#else
# define XFORM_NONGCING /* empty */
#endif

#ifdef MZ_XFORM
START_XFORM_SUSPEND;
#endif

#include <stdio.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>

#ifdef MZ_XFORM
END_XFORM_SUSPEND;
#endif

#ifdef PALMOS_STUFF
typedef jmpbuf jmp_buf[1];
#endif

#define GC_MIGHT_USE_REGISTERED_STATICS

#ifdef MACINTOSH_EVENTS
/* We avoid #including the Carbon headers because we only
   need a few abstract struct types: */
typedef struct FSSpec mzFSSpec;
#endif

#ifndef MZ_DONT_USE_JIT
# if defined(MZ_USE_JIT_PPC) || defined(MZ_USE_JIT_I386) || defined(MZ_USE_JIT_X86_64)
#  define MZ_USE_JIT
# endif
#endif

/* Define _W64 for MSC if needed. */
#if defined(_MSC_VER) && !defined(_W64)
# if !defined(__midl) && (defined(_X86_) || defined(_M_IX86)) && _MSC_VER >= 1300
# define _W64 __w64
# else
# define _W64
# endif
#endif

#ifdef MZ_PRECISE_GC
# ifndef MZ_XFORM
#  define XFORM_SKIP_PROC /* empty */
#  define XFORM_CAN_IGNORE /**/
# endif
#else
# define XFORM_HIDE_EXPR(x) x
# define XFORM_START_SKIP /**/
# define XFORM_END_SKIP /**/
# define XFORM_START_SUSPEND /**/
# define XFORM_END_SUSPEND /**/
# define XFORM_SKIP_PROC /**/
# define XFORM_START_TRUST_ARITH /**/
# define XFORM_END_TRUST_ARITH /**/
# define XFORM_CAN_IGNORE /**/
# define XFORM_TRUST_PLUS +
# define XFORM_TRUST_MINUS -
#endif

/* PPC Linux plays a slimy trick: it defines strcpy() as a macro that
   uses __extension__. This breaks the 3m xform. */
#if defined(MZ_XFORM) && defined(strcpy)
START_XFORM_SKIP;
static inline void _mzstrcpy(char *a, const char *b)
{
  strcpy(a, b);
}
END_XFORM_SKIP;
# undef strcpy
# define strcpy _mzstrcpy
#endif

#ifdef __cplusplus
extern "C"
{
#endif

/* Allowed by all configurations, currently: */
#define MZ_CAN_ACCESS_THREAD_LOCAL_DIRECTLY

/*========================================================================*/
/*                        basic Scheme values                             */
/*========================================================================*/

typedef short Scheme_Type;

typedef int mzshort;

typedef unsigned int mzchar;
typedef int mzchar_int; /* includes EOF */

#ifdef INT64_AS_LONG_LONG
typedef _int64 mzlonglong;
typedef unsigned _int64 umzlonglong;
#else
# if defined(NO_LONG_LONG_TYPE) || defined(SIXTY_FOUR_BIT_INTEGERS)
typedef long mzlonglong;
typedef unsigned long umzlonglong;
# else
typedef long long mzlonglong;
typedef unsigned long long umzlonglong;
# endif
#endif

/* Racket values have the type `Scheme_Object *'. The Scheme_Object
   structure declares just the header: a type tag and space for
   hashing or extra flags; actual object types will extend this
   structure.

   For example, Scheme_Simple_Object defines a few variants. The
   important thing is that it starts with a nested Scheme_Object
   record.

   The Scheme_Simple_Object struct is defined here, instead of in a
   private header, so that macros can provide quick access. Of course,
   don't access the fields of these structures directly; use the
   macros instead. */

typedef struct Scheme_Object
{
  Scheme_Type type; /* Anything that starts with a type field
		       can be a Scheme_Object */

  /* For precise GC, the keyex field is used for all object types to
     store a hash key extension. The low bit is not used for this
     purpose, though. For string, pair, vector, and box values in all
     variants of Racket, the low bit is set to 1 to indicate that
     the object is immutable. Thus, the keyex field is needed even in
     non-precise GC mode, so such structures embed
     Scheme_Inclhash_Object */

  MZ_HASH_KEY_EX
} Scheme_Object;

  /* See note above on MZ_HASH_KEY_EX. To get the keyex field,
     use MZ_OPT_HASH_KEY(iso), where iso is a pointer to a
     Scheme_Inclhash_Object */
typedef struct Scheme_Inclhash_Object
{
  Scheme_Object so;
  MZ_OPT_HASH_KEY_EX
} Scheme_Inclhash_Object;

typedef struct Scheme_Simple_Object
{
  Scheme_Inclhash_Object iso;

  union
    {
      struct { mzchar *string_val; intptr_t tag_val; } char_str_val;
      struct { char *string_val; intptr_t tag_val; } byte_str_val;
      struct { void *ptr1, *ptr2; } two_ptr_val;
      struct { int int1; int int2; } two_int_val;
      struct { void *ptr; int pint; } ptr_int_val;
      struct { void *ptr; intptr_t pint; } ptr_long_val;
      struct { struct Scheme_Object *car, *cdr; } pair_val;
      struct { mzshort len; mzshort *vec; } svector_val;
      struct { void *val; Scheme_Object *type; } cptr_val;
    } u;
} Scheme_Simple_Object;

typedef struct Scheme_Object *(*Scheme_Closure_Func)(struct Scheme_Object *);

/* Scheme_Small_Object is used for several types of Racket values: */
typedef struct {
  Scheme_Inclhash_Object iso;
  union {
    mzchar char_val;
    Scheme_Object *ptr_value;
    intptr_t int_val;
    Scheme_Object *ptr_val;
  } u;
} Scheme_Small_Object;

/* A floating-point number: */
typedef struct {
  Scheme_Object so;
  double double_val;
} Scheme_Double;

#ifdef MZ_USE_SINGLE_FLOATS
typedef struct {
  Scheme_Object so;
  float float_val;
} Scheme_Float;
#endif

typedef struct Scheme_Symbol {
  Scheme_Inclhash_Object iso; /* 1 in low bit of keyex indicates uninterned */
  intptr_t len;
  char s[4]; /* Really, a number of chars to match `len' */
} Scheme_Symbol;

typedef struct Scheme_Vector {
  Scheme_Inclhash_Object iso; /* 1 in low bit of keyex indicates immutable */
  intptr_t size;
  Scheme_Object *els[mzFLEX_ARRAY_DECL];
} Scheme_Vector;

# define SHARED_ALLOCATED 0x2
# define SHARED_ALLOCATEDP(so) (MZ_OPT_HASH_KEY((Scheme_Inclhash_Object *)(so)) & SHARED_ALLOCATED)
# define SHARED_ALLOCATED_SET(so) (MZ_OPT_HASH_KEY((Scheme_Inclhash_Object *)(so)) |= SHARED_ALLOCATED)

typedef struct Scheme_Double_Vector {
  Scheme_Inclhash_Object iso; /* & 0x2 indicates allocated in the MASTERGC */
  intptr_t size;
  double els[mzFLEX_ARRAY_DECL];
} Scheme_Double_Vector;

typedef struct Scheme_Print_Params Scheme_Print_Params;
typedef void (*Scheme_Type_Printer)(Scheme_Object *v, int for_display, Scheme_Print_Params *pp);

typedef int (*Scheme_Equal_Proc)(Scheme_Object *obj1, Scheme_Object *obj2, void *cycle_data);
typedef intptr_t (*Scheme_Primary_Hash_Proc)(Scheme_Object *obj, intptr_t base, void *cycle_data);
typedef intptr_t (*Scheme_Secondary_Hash_Proc)(Scheme_Object *obj, void *cycle_data);

/* This file defines all the built-in types */
#ifdef INCLUDE_WITHOUT_PATHS
# include "stypes.h"
#else
# include "../src/stypes.h"
#endif

#define OBJ_TO_LONG(ptr) ((intptr_t)(ptr))
#define LONG_TO_OBJ(l) ((Scheme_Object *)(void *)(intptr_t)(l))

/* Scheme Objects are always aligned on 2-byte boundaries, so  */
/* words of type Scheme_Object * will always have zero in the  */
/* least significant bit.  Therefore, we can use this bit as a */
/* tag to indicate that the `pointer' isn't really a pointer   */
/* but a 31-bit signed immediate integer. */

#define SCHEME_INTP(obj)     (OBJ_TO_LONG(obj) & 0x1)

#define SAME_PTR(a, b) ((a) == (b))
#define NOT_SAME_PTR(a, b) ((a) != (b))

#define SAME_OBJ(a, b) SAME_PTR(a, b)
#define NOT_SAME_OBJ(a, b) NOT_SAME_PTR(a, b)

#define SAME_TYPE(a, b) ((Scheme_Type)(a) == (Scheme_Type)(b))
#define NOT_SAME_TYPE(a, b) ((Scheme_Type)(a) != (Scheme_Type)(b))

# define SCHEME_TYPE(obj)     (SCHEME_INTP(obj)?(Scheme_Type)scheme_integer_type:((Scheme_Object *)(obj))->type)
# define _SCHEME_TYPE(obj) ((obj)->type) /* unsafe version */

/*========================================================================*/
/*                        basic Scheme predicates                         */
/*========================================================================*/

#define SCHEME_CHARP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_char_type)
/* SCHEME_INTP defined above */
#define SCHEME_DBLP(obj)     SAME_TYPE(SCHEME_TYPE(obj), scheme_double_type)
#ifdef MZ_USE_SINGLE_FLOATS
# define SCHEME_FLTP(obj)     SAME_TYPE(SCHEME_TYPE(obj), scheme_float_type)
# define SCHEME_FLOATP(obj)     (SCHEME_FLTP(obj) || SCHEME_DBLP(obj))
#else
# define SCHEME_FLTP SCHEME_DBLP
# define SCHEME_FLOATP SCHEME_DBLP
#endif
#define SCHEME_BIGNUMP(obj)     SAME_TYPE(SCHEME_TYPE(obj), scheme_bignum_type)
#define SCHEME_RATIONALP(obj)     SAME_TYPE(SCHEME_TYPE(obj), scheme_rational_type)
#define SCHEME_COMPLEXP(obj)     (!SCHEME_INTP(obj) && ((_SCHEME_TYPE(obj) == scheme_complex_type)))
#define SCHEME_EXACT_INTEGERP(obj)  (SCHEME_INTP(obj) || (_SCHEME_TYPE(obj) == scheme_bignum_type))
#define SCHEME_EXACT_REALP(obj)  (SCHEME_INTP(obj) || (_SCHEME_TYPE(obj) == scheme_bignum_type) || (_SCHEME_TYPE(obj) == scheme_rational_type))
#define SCHEME_REALP(obj)  (SCHEME_INTP(obj) || ((_SCHEME_TYPE(obj) >= scheme_bignum_type) && (_SCHEME_TYPE(obj) < scheme_complex_type)))
#define SCHEME_NUMBERP(obj)  (SCHEME_INTP(obj) || ((_SCHEME_TYPE(obj) >= scheme_bignum_type) && (_SCHEME_TYPE(obj) <= scheme_complex_type)))

#define SCHEME_CHAR_STRINGP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_char_string_type)
#define SCHEME_MUTABLE_CHAR_STRINGP(obj)  (SCHEME_CHAR_STRINGP(obj) && SCHEME_MUTABLEP(obj))
#define SCHEME_IMMUTABLE_CHAR_STRINGP(obj)  (SCHEME_CHAR_STRINGP(obj) && SCHEME_IMMUTABLEP(obj))

#define SCHEME_BYTE_STRINGP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_byte_string_type)
#define SCHEME_MUTABLE_BYTE_STRINGP(obj)  (SCHEME_BYTE_STRINGP(obj) && SCHEME_MUTABLEP(obj))
#define SCHEME_IMMUTABLE_BYTE_STRINGP(obj)  (SCHEME_BYTE_STRINGP(obj) && SCHEME_IMMUTABLEP(obj))

#define SCHEME_PATHP(obj)  SAME_TYPE(SCHEME_TYPE(obj), SCHEME_PLATFORM_PATH_KIND)
#define SCHEME_GENERAL_PATHP(obj)  ((SCHEME_TYPE(obj) >= scheme_unix_path_type) && (SCHEME_TYPE(obj) <= scheme_windows_path_type))
  /* A path is guaranteed to have the same shape as a byte string */

#define SCHEME_PATH_STRINGP(x) (SCHEME_CHAR_STRINGP(x) || SCHEME_PATHP(x))
#define SCHEME_PATH_STRING_STR "path or string"

#define SCHEME_GENERAL_PATH_STRINGP(x) (SCHEME_CHAR_STRINGP(x) || SCHEME_GENERAL_PATHP(x))
#define SCHEME_GENERAL_PATH_STRING_STR "path (for any platform) or string"

#define SCHEME_SYMBOLP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_symbol_type)
#define SCHEME_KEYWORDP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_keyword_type)

#define SCHEME_STRSYMP(obj) (SCHEME_CHAR_STRINGP(obj) || SCHEME_SYMBOLP(obj))

#define SCHEME_BOOLP(obj)    (SAME_OBJ(obj, scheme_true) || SAME_OBJ(obj, scheme_false))
#define SCHEME_FALSEP(obj)     SAME_OBJ((obj), scheme_false)
#define SCHEME_TRUEP(obj)     (!SCHEME_FALSEP(obj))
#define SCHEME_EOFP(obj)     SAME_OBJ((obj), scheme_eof)
#define SCHEME_VOIDP(obj)     SAME_OBJ((obj), scheme_void)

#define SCHEME_NULLP(obj)    SAME_OBJ(obj, scheme_null)
#define SCHEME_PAIRP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_pair_type)
#define SCHEME_MPAIRP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_mutable_pair_type)
#define SCHEME_MUTABLE_PAIRP(obj)    SCHEME_MPAIRP(obj)
#define SCHEME_LISTP(obj)    (SCHEME_NULLP(obj) || SCHEME_PAIRP(obj))

#define SCHEME_RPAIRP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_raw_pair_type)

#define SCHEME_BOXP(obj)     SAME_TYPE(SCHEME_TYPE(obj), scheme_box_type)
#define SCHEME_MUTABLE_BOXP(obj)  (SCHEME_BOXP(obj) && SCHEME_MUTABLEP(obj))
#define SCHEME_IMMUTABLE_BOXP(obj)  (SCHEME_BOXP(obj) && SCHEME_IMMUTABLEP(obj))

#define SCHEME_PROMPT_TAGP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_prompt_tag_type)

#define SCHEME_BUCKTP(obj) SAME_TYPE(SCHEME_TYPE(obj),scheme_bucket_table_type)
#define SCHEME_HASHTP(obj) SAME_TYPE(SCHEME_TYPE(obj),scheme_hash_table_type)
#define SCHEME_HASHTRP(obj) SAME_TYPE(SCHEME_TYPE(obj),scheme_hash_tree_type)

#define SCHEME_VECTORP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_vector_type)
#define SCHEME_MUTABLE_VECTORP(obj)  (SCHEME_VECTORP(obj) && SCHEME_MUTABLEP(obj))
#define SCHEME_IMMUTABLE_VECTORP(obj)  (SCHEME_VECTORP(obj) && SCHEME_IMMUTABLEP(obj))

#define SCHEME_FLVECTORP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_flvector_type)
#define SCHEME_FXVECTORP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_fxvector_type)

#define SCHEME_STRUCTP(obj) (SAME_TYPE(SCHEME_TYPE(obj), scheme_structure_type) || SAME_TYPE(SCHEME_TYPE(obj), scheme_proc_struct_type))
#define SCHEME_STRUCT_TYPEP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_struct_type_type)

#define SCHEME_INPORTP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_input_port_type)
#define SCHEME_OUTPORTP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_output_port_type)

#define SCHEME_INPUT_PORTP(obj)  scheme_is_input_port(obj)
#define SCHEME_OUTPUT_PORTP(obj) scheme_is_output_port(obj)

#define SCHEME_THREADP(obj)   SAME_TYPE(SCHEME_TYPE(obj), scheme_thread_type)
#define SCHEME_CUSTODIANP(obj)   SAME_TYPE(SCHEME_TYPE(obj), scheme_custodian_type)
#define SCHEME_SEMAP(obj)   SAME_TYPE(SCHEME_TYPE(obj), scheme_sema_type)
#define SCHEME_CHANNELP(obj)   SAME_TYPE(SCHEME_TYPE(obj), scheme_channel_type)
#define SCHEME_CHANNEL_PUTP(obj)   SAME_TYPE(SCHEME_TYPE(obj), scheme_channel_put_type)

#define SCHEME_CONFIGP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_config_type)
#define SCHEME_NAMESPACEP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_namespace_type)
#define SCHEME_WEAKP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_weak_box_type)

#define SCHEME_STXP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_stx_type)

#define SCHEME_CHAPERONEP(obj) (SAME_TYPE(SCHEME_TYPE(obj), scheme_chaperone_type) \
                                || SAME_TYPE(SCHEME_TYPE(obj), scheme_proc_chaperone_type))

#define SCHEME_UDPP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_udp_type)
#define SCHEME_UDP_EVTP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_udp_evt_type)

#define SCHEME_CPTRP(obj) (SAME_TYPE(SCHEME_TYPE(obj), scheme_cpointer_type))

#define SCHEME_MUTABLEP(obj) (!(MZ_OPT_HASH_KEY((Scheme_Inclhash_Object *)(obj)) & 0x1))
#define SCHEME_IMMUTABLEP(obj) (MZ_OPT_HASH_KEY((Scheme_Inclhash_Object *)(obj)) & 0x1)

#define GUARANTEE_TYPE(fname, argnum, typepred, typenam)                                \
   (typepred (argv [argnum])                                                            \
        ? argv [argnum]                                                                 \
        : (scheme_wrong_type (fname, typenam, argnum, argc, argv), argv [argnum]))

#define GUARANTEE_BOOL(fname, argnum)        GUARANTEE_TYPE (fname, argnum, SCHEME_BOOLP, "boolean")
#define GUARANTEE_CHAR(fname, argnum)        GUARANTEE_TYPE (fname, argnum, SCHEME_CHARP, "character")
#define GUARANTEE_INTEGER(fname, argnum)     GUARANTEE_TYPE (fname, argnum, SCHEME_INTP, "integer")
#define GUARANTEE_PAIR(fname, argnum)        GUARANTEE_TYPE (fname, argnum, SCHEME_PAIRP, "pair")
#define GUARANTEE_PROCEDURE(fname, argnum)   GUARANTEE_TYPE (fname, argnum, SCHEME_PROCP, "procedure")
#define GUARANTEE_CHAR_STRING(fname, argnum) GUARANTEE_TYPE (fname, argnum, SCHEME_CHAR_STRINGP, "string")
#define GUARANTEE_STRSYM(fname, argnum)      GUARANTEE_TYPE (fname, argnum, SCHEME_STRSYMP, "string or symbol")
#define GUARANTEE_SYMBOL(fname, argnum)      GUARANTEE_TYPE (fname, argnum, SCHEME_SYMBOLP, "symbol")

#define SCHEME_UNIX_PATH_KIND scheme_unix_path_type
#define SCHEME_WINDOWS_PATH_KIND scheme_windows_path_type

#ifdef DOS_FILE_SYSTEM
# define SCHEME_PLATFORM_PATH_KIND SCHEME_WINDOWS_PATH_KIND
#else
# define SCHEME_PLATFORM_PATH_KIND SCHEME_UNIX_PATH_KIND
#endif

#define SCHEME_PATH_KIND(p) SCHEME_TYPE(p)

/*========================================================================*/
/*                        basic Scheme accessors                          */
/*========================================================================*/

#define SCHEME_CHAR_VAL(obj) (((Scheme_Small_Object *)(obj))->u.char_val)
#define SCHEME_INT_VAL(obj)  (OBJ_TO_LONG(obj)>>1)
#define SCHEME_DBL_VAL(obj)  (((Scheme_Double *)(obj))->double_val)
#ifdef MZ_USE_SINGLE_FLOATS
# define SCHEME_FLT_VAL(obj)  (((Scheme_Float *)(obj))->float_val)
# define SCHEME_FLOAT_VAL(obj) (SCHEME_DBLP(obj) ? SCHEME_DBL_VAL(obj) : SCHEME_FLT_VAL(obj))
#else
# define SCHEME_FLT_VAL(x) ((float)(SCHEME_DBL_VAL(x)))
# define SCHEME_FLOAT_VAL SCHEME_DBL_VAL
# define scheme_make_float(x) scheme_make_double((double)x)
#endif

#define SCHEME_CHAR_STR_VAL(obj)  (((Scheme_Simple_Object *)(obj))->u.char_str_val.string_val)
#define SCHEME_CHAR_STRTAG_VAL(obj)  (((Scheme_Simple_Object *)(obj))->u.char_str_val.tag_val)
#define SCHEME_CHAR_STRLEN_VAL(obj)  (((Scheme_Simple_Object *)(obj))->u.char_str_val.tag_val)
#define SCHEME_BYTE_STR_VAL(obj)  (((Scheme_Simple_Object *)(obj))->u.byte_str_val.string_val)
#define SCHEME_BYTE_STRTAG_VAL(obj)  (((Scheme_Simple_Object *)(obj))->u.byte_str_val.tag_val)
#define SCHEME_BYTE_STRLEN_VAL(obj)  (((Scheme_Simple_Object *)(obj))->u.byte_str_val.tag_val)
#define SCHEME_PATH_VAL(obj)  (((Scheme_Simple_Object *)(obj))->u.byte_str_val.string_val)
#define SCHEME_PATH_LEN(obj)  (((Scheme_Simple_Object *)(obj))->u.byte_str_val.tag_val)
#define SCHEME_SYM_VAL(obj)  (((Scheme_Symbol *)((Scheme_Simple_Object *)(obj)))->s)
#define SCHEME_SYM_LEN(obj)  (((Scheme_Symbol *)((Scheme_Simple_Object *)(obj)))->len)
#define SCHEME_KEYWORD_VAL(obj) SCHEME_SYM_VAL(obj)
#define SCHEME_KEYWORD_LEN(obj) SCHEME_SYM_LEN(obj)

#define SCHEME_SYMSTR_OFFSET(obj) ((uintptr_t)SCHEME_SYM_VAL(obj)-(uintptr_t)(obj))

/* return a `char *' pointing to the string or the symbol name */
#define SCHEME_STRSYM_VAL(obj) (SCHEME_SYMBOLP(obj) ? SCHEME_SYM_VAL(obj) : SCHEME_CHAR_STR_VAL(obj))

#define SCHEME_BOX_VAL(obj)  (((Scheme_Small_Object *)(obj))->u.ptr_val)

#define SCHEME_CAR(obj)      (((Scheme_Simple_Object *)(obj))->u.pair_val.car)
#define SCHEME_CDR(obj)      (((Scheme_Simple_Object *)(obj))->u.pair_val.cdr)

#define SCHEME_CADR(obj)     (SCHEME_CAR (SCHEME_CDR (obj)))
#define SCHEME_CAAR(obj)     (SCHEME_CAR (SCHEME_CAR (obj)))
#define SCHEME_CDDR(obj)     (SCHEME_CDR (SCHEME_CDR (obj)))

#define SCHEME_MCAR(obj)      (((Scheme_Simple_Object *)(obj))->u.pair_val.car)
#define SCHEME_MCDR(obj)      (((Scheme_Simple_Object *)(obj))->u.pair_val.cdr)

#define SCHEME_VEC_SIZE(obj) (((Scheme_Vector *)(obj))->size)
#define SCHEME_VEC_ELS(obj)  (((Scheme_Vector *)(obj))->els)
#define SCHEME_VEC_BASE(obj) SCHEME_VEC_ELS(obj)

#define SCHEME_FLVEC_SIZE(obj) (((Scheme_Double_Vector *)(obj))->size)
#define SCHEME_FLVEC_ELS(obj)  (((Scheme_Double_Vector *)(obj))->els)

#define SCHEME_FXVEC_SIZE(obj) SCHEME_VEC_SIZE(obj)
#define SCHEME_FXVEC_ELS(obj) SCHEME_VEC_ELS(obj)

#define SCHEME_ENVBOX_VAL(obj)  (*((Scheme_Object **)(obj)))
#define SCHEME_WEAK_BOX_VAL(obj) SCHEME_BOX_VAL(obj)

#define SCHEME_PTR_VAL(obj)  (((Scheme_Small_Object *)(obj))->u.ptr_val)
#define SCHEME_PTR1_VAL(obj) (((Scheme_Simple_Object *)(obj))->u.two_ptr_val.ptr1)
#define SCHEME_PTR2_VAL(obj) (((Scheme_Simple_Object *)(obj))->u.two_ptr_val.ptr2)
#define SCHEME_IPTR_VAL(obj) (((Scheme_Simple_Object *)(obj))->u.ptr_int_val.ptr)
#define SCHEME_LPTR_VAL(obj) (((Scheme_Simple_Object *)(obj))->u.ptr_long_val.ptr)
#define SCHEME_INT1_VAL(obj) (((Scheme_Simple_Object *)(obj))->u.two_int_val.int1)
#define SCHEME_INT2_VAL(obj) (((Scheme_Simple_Object *)(obj))->u.two_int_val.int2)
#define SCHEME_PINT_VAL(obj) (((Scheme_Simple_Object *)(obj))->u.ptr_int_val.pint)
#define SCHEME_PLONG_VAL(obj) (((Scheme_Simple_Object *)(obj))->u.ptr_long_val.pint)

typedef struct Scheme_Cptr
{
  Scheme_Inclhash_Object so; /* 0x1 => an external pointer (not GCable); 0x2 => has offset */
  void *val;
  Scheme_Object *type;
} Scheme_Cptr;
typedef struct Scheme_Offset_Cptr
{
  Scheme_Cptr cptr; 
  intptr_t offset;
} Scheme_Offset_Cptr;

#define SCHEME_CPTR_VAL(obj) (((Scheme_Cptr *)(obj))->val)
#define SCHEME_CPTR_TYPE(obj) (((Scheme_Cptr *)(obj))->type)
#define SCHEME_CPTR_OFFSET(obj) (SCHEME_CPTR_HAS_OFFSET(obj) ? ((Scheme_Offset_Cptr *)obj)->offset : 0)
#define SCHEME_CPTR_FLAGS(obj) MZ_OPT_HASH_KEY(&((Scheme_Cptr *)(obj))->so)
#define SCHEME_CPTR_HAS_OFFSET(obj) (SCHEME_CPTR_FLAGS(obj) & 0x2)

#define SCHEME_SET_IMMUTABLE(obj)  ((MZ_OPT_HASH_KEY((Scheme_Inclhash_Object *)(obj)) |= 0x1))
#define SCHEME_SET_CHAR_STRING_IMMUTABLE(obj) SCHEME_SET_IMMUTABLE(obj)
#define SCHEME_SET_BYTE_STRING_IMMUTABLE(obj) SCHEME_SET_IMMUTABLE(obj)
#define SCHEME_SET_VECTOR_IMMUTABLE(obj) SCHEME_SET_IMMUTABLE(obj)
#define SCHEME_SET_BOX_IMMUTABLE(obj) SCHEME_SET_IMMUTABLE(obj)

/*========================================================================*/
/*               fast basic Scheme constructor macros                     */
/*========================================================================*/

#define scheme_make_integer(i)    LONG_TO_OBJ ((OBJ_TO_LONG(i) << 1) | 0x1)
#define scheme_make_character(ch) ((((mzchar)ch) < 256) ? scheme_char_constants[(unsigned char)(ch)] : scheme_make_char(ch))
#define scheme_make_ascii_character(ch) scheme_char_constants[(unsigned char)(ch)]

#define scheme_uchar_find(table, x) (table[(x >> 8) & 0x1FFF][x & 0xFF])

#define scheme_isblank(x) ((scheme_uchar_find(scheme_uchar_table, x)) & 0x1)
#define scheme_issymbol(x) ((scheme_uchar_find(scheme_uchar_table, x)) & 0x2)
#define scheme_ispunc(x) ((scheme_uchar_find(scheme_uchar_table, x)) & 0x4)
#define scheme_iscontrol(x) ((scheme_uchar_find(scheme_uchar_table, x)) & 0x8)
#define scheme_isspace(x) ((scheme_uchar_find(scheme_uchar_table, x)) & 0x10)
/* #define scheme_isSOMETHING(x) ((scheme_uchar_find(scheme_uchar_table, x)) & 0x20) - not yet used */
#define scheme_isdigit(x) ((scheme_uchar_find(scheme_uchar_table, x)) & 0x40)
#define scheme_isalpha(x) ((scheme_uchar_find(scheme_uchar_table, x)) & 0x80)
#define scheme_istitle(x) ((scheme_uchar_find(scheme_uchar_table, x)) & 0x100)
#define scheme_isupper(x) ((scheme_uchar_find(scheme_uchar_table, x)) & 0x200)
#define scheme_islower(x) ((scheme_uchar_find(scheme_uchar_table, x)) & 0x400)
#define scheme_isgraphic(x) ((scheme_uchar_find(scheme_uchar_table, x)) & 0x800)
#define scheme_iscaseignorable(x) ((scheme_uchar_find(scheme_uchar_table, x)) & 0x1000)
#define scheme_isspecialcasing(x) ((scheme_uchar_find(scheme_uchar_table, x)) & 0x2000)
#define scheme_needs_decompose(x) ((scheme_uchar_find(scheme_uchar_table, x)) & 0x4000)
#define scheme_needs_maybe_compose(x) ((scheme_uchar_find(scheme_uchar_table, x)) & 0x8000)

#define scheme_iscased(x) ((scheme_uchar_find(scheme_uchar_table, x)) & 0x700)

#define scheme_toupper(x) (x + scheme_uchar_ups[scheme_uchar_find(scheme_uchar_cases_table, x)])
#define scheme_tolower(x) (x + scheme_uchar_downs[scheme_uchar_find(scheme_uchar_cases_table, x)])
#define scheme_totitle(x) (x + scheme_uchar_titles[scheme_uchar_find(scheme_uchar_cases_table, x)])
#define scheme_tofold(x) (x + scheme_uchar_folds[scheme_uchar_find(scheme_uchar_cases_table, x)])
#define scheme_combining_class(x) (scheme_uchar_combining_classes[scheme_uchar_find(scheme_uchar_cases_table, x)])

#define scheme_general_category(x) ((scheme_uchar_find(scheme_uchar_cats_table, x)) & 0x1F)
/* Note: 3 bits available in the cats table */

/*========================================================================*/
/*                          procedure values                              */
/*========================================================================*/

/* Constants for flags in Scheme_Primitive_[Closed]_Proc.
   Do not use them directly. */
#define SCHEME_PRIM_OPT_MASK (1 | 2)
#define SCHEME_PRIM_IS_PRIMITIVE 4
#define SCHEME_PRIM_IS_UNSAFE_OMITABLE 8
#define SCHEME_PRIM_IS_OMITABLE 16
#define SCHEME_PRIM_OTHER_TYPE_MASK (32 | 64 | 128 | 256)
#define SCHEME_PRIM_IS_MULTI_RESULT 512
#define SCHEME_PRIM_IS_BINARY_INLINED 1024
#define SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL 2048
#define SCHEME_PRIM_IS_METHOD 4096
#define SCHEME_PRIM_IS_CLOSURE 8192
#define SCHEME_PRIM_IS_UNARY_INLINED 16384
#define SCHEME_PRIM_IS_NARY_INLINED 32768

/* Values with SCHEME_PRIM_OPT_MASK, earlier implies later: */
#define SCHEME_PRIM_OPT_FOLDING    3
#define SCHEME_PRIM_OPT_IMMEDIATE  2
#define SCHEME_PRIM_OPT_NONCM      1

/* Values with SCHEME_PRIM_OTHER_TYPE_MASK */
#define SCHEME_PRIM_STRUCT_TYPE_INDEXLESS_GETTER (32 | 256)
#define SCHEME_PRIM_STRUCT_TYPE_CONSTR           128
#define SCHEME_PRIM_STRUCT_TYPE_INDEXLESS_SETTER 256
#define SCHEME_PRIM_STRUCT_TYPE_INDEXED_SETTER   (128 | 256)
#define SCHEME_PRIM_TYPE_PARAMETER               64
#define SCHEME_PRIM_TYPE_STRUCT_PROP_GETTER      (64 | 128)
#define SCHEME_PRIM_SOMETIMES_INLINED            (64 | 256)
#define SCHEME_PRIM_STRUCT_TYPE_STRUCT_PROP_PRED        (64 | 128 | 256)
#define SCHEME_PRIM_STRUCT_TYPE_INDEXED_GETTER   32
#define SCHEME_PRIM_STRUCT_TYPE_PRED             (32 | 64)

#define SCHEME_PRIM_PROC_FLAGS(x) (((Scheme_Prim_Proc_Header *)x)->flags)

#define SCHEME_PRIM_IS_SOMETIMES_INLINED(rator) \
  (((SCHEME_PRIM_PROC_FLAGS(rator) & SCHEME_PRIM_OTHER_TYPE_MASK) == SCHEME_PRIM_SOMETIMES_INLINED) \
   || (SCHEME_PRIM_PROC_FLAGS(rator) & (SCHEME_PRIM_IS_UNARY_INLINED | SCHEME_PRIM_IS_BINARY_INLINED)))

typedef struct Scheme_Object *(Scheme_Prim)(int argc, Scheme_Object *argv[]);

typedef struct Scheme_Object *(Scheme_Primitive_Closure_Proc)(int argc, struct Scheme_Object *argv[], Scheme_Object *p);

#define SCHEME_MAX_ARGS 0x3FFFFFFE

typedef struct {
  Scheme_Object so;
  unsigned short flags;
} Scheme_Prim_Proc_Header;

typedef struct {
  Scheme_Prim_Proc_Header pp;
  Scheme_Primitive_Closure_Proc *prim_val;
  const char *name;
  mzshort mina;
  /* If mina < 0; mina is negated case count minus one for a case-lambda
     generated by mzc, where the primitive checks argument arity
     itself, and mu.cases is available instead of mu.maxa. */
  union {
    mzshort *cases;
    mzshort maxa;   /* > SCHEME_MAX_ARGS => any number of arguments */
  } mu;
} Scheme_Primitive_Proc;

typedef struct {
  Scheme_Primitive_Proc pp;
  mzshort minr, maxr;
  /* Never combined with a closure */
} Scheme_Prim_W_Result_Arity;

typedef struct Scheme_Primitive_Closure {
  Scheme_Primitive_Proc p;
  /* The rest is here only if SCHEME_PRIM_IS_CLOSURE
     is set in p.pp.flags. */
#ifdef MZ_PRECISE_GC
  mzshort count;
#endif
  Scheme_Object *val[mzFLEX_ARRAY_DECL];
} Scheme_Primitive_Closure;

#define SCHEME_PRIM_CLOSURE_ELS(p) ((Scheme_Primitive_Closure *)p)->val

/* ------ Old-style primitive closures ------- */

typedef struct Scheme_Object *(Scheme_Closed_Prim)(void *d, int argc, struct Scheme_Object *argv[]);

typedef struct {
  Scheme_Prim_Proc_Header pp;
  Scheme_Closed_Prim *prim_val;
  void *data;
  const char *name;
  mzshort mina, maxa; /* mina == -2 => maxa is negated case count and
		       record is a Scheme_Closed_Case_Primitive_Proc */
} Scheme_Closed_Primitive_Proc;

typedef struct {
  Scheme_Closed_Primitive_Proc p;
  mzshort *cases;
} Scheme_Closed_Case_Primitive_Proc;

typedef struct {
  Scheme_Closed_Primitive_Proc p;
  mzshort minr, maxr;
} Scheme_Closed_Prim_W_Result_Arity;

/* ------------------------------------------------- */
/*                 mzc closure glue
    The following are used by mzc to implement closures.
*/

#define _scheme_fill_prim_closure(rec, cfunc, nm, amin, amax, flgs) \
  ((rec)->pp.so.type = scheme_prim_type, \
   (rec)->prim_val = cfunc, \
   (rec)->name = nm, \
   (rec)->mina = amin,	  \
   (rec)->mu.maxa = (amax == -1 ? SCHEME_MAX_ARGS + 1 : amax), \
   (rec)->pp.flags = flgs, \
   rec)

#ifdef MZ_PRECISE_GC
# define _scheme_fill_prim_closure_post(rec, cfunc, nm, amin, amax, flgs, ln) \
  ((rec)->count = ln,							\
   _scheme_fill_prim_closure(&(rec)->p, cfunc, nm, amin, amax,	\
			     flgs | SCHEME_PRIM_IS_CLOSURE))
#else
# define _scheme_fill_prim_closure_post(rec, cfunc, nm, amin, amax, flgs, ln) \
  _scheme_fill_prim_closure(&(rec)->p, cfunc, nm, amin, amax, flgs)
#endif

#define _scheme_fill_prim_case_closure(rec, cfunc, nm, ccount, cses, flgs) \
  ((rec)->pp.so.type = scheme_prim_type, \
   (rec)->prim_val = cfunc, \
   (rec)->name = nm, \
   (rec)->mina = -(ccount+1), \
   (rec)->pp.flags = flgs, \
   (rec)->mu.cases = cses, \
   rec)

#ifdef MZ_PRECISE_GC
# define _scheme_fill_prim_case_closure_post(rec, cfunc, nm, ccount, cses, flgs, ln) \
  ((rec)->count = ln,							\
   _scheme_fill_prim_case_closure(&((rec)->p), cfunc, nm, ccount, cses,	\
				  flgs | SCHEME_PRIM_IS_CLOSURE))
#else
# define _scheme_fill_prim_case_closure_post(rec, cfunc, nm, ccount, cses, flgs, ln) \
  _scheme_fill_prim_case_closure(&((rec)->p), cfunc, nm, ccount, cses, flgs)
#endif

/* ------------------------------------------------- */

#define SCHEME_PROCP(obj)  (!SCHEME_INTP(obj) && ((_SCHEME_TYPE(obj) >= scheme_prim_type) && (_SCHEME_TYPE(obj) <= scheme_proc_chaperone_type)))
#define SCHEME_SYNTAXP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_syntax_compiler_type)
#define SCHEME_PRIMP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_prim_type)
#define SCHEME_CLSD_PRIMP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_closed_prim_type)
#define SCHEME_CONTP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_cont_type)
#define SCHEME_ECONTP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_escaping_cont_type)
#define SCHEME_CONT_MARK_SETP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_cont_mark_set_type)
#define SCHEME_PROC_STRUCTP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_proc_struct_type)
#define SCHEME_CLOSUREP(obj) (SAME_TYPE(SCHEME_TYPE(obj), scheme_closure_type) || SAME_TYPE(SCHEME_TYPE(obj), scheme_case_closure_type))

#define SCHEME_PRIM(obj)     (((Scheme_Primitive_Proc *)(obj))->prim_val)
#define SCHEME_CLSD_PRIM(obj) (((Scheme_Closed_Primitive_Proc *)(obj))->prim_val)
#define SCHEME_CLSD_PRIM_DATA(obj) (((Scheme_Closed_Primitive_Proc *)(obj))->data)
#define SCHEME_CLOS_FUNC(obj) ((Scheme_Closure_Func)SCHEME_CAR(obj))
#define SCHEME_CLOS_DATA(obj) SCHEME_CDR(obj)

/*========================================================================*/
/*                      hash tables and environments                      */
/*========================================================================*/

typedef struct Scheme_Hash_Table
{
  Scheme_Inclhash_Object iso; /* 0x1 flag => marshal as #t (hack for stxobj bytecode) */
  intptr_t size; /* power of 2 */
  intptr_t count;
  Scheme_Object **keys;
  Scheme_Object **vals;
  void (*make_hash_indices)(void *v, intptr_t *h1, intptr_t *h2);
  int (*compare)(void *v1, void *v2);
  Scheme_Object *mutex;
  intptr_t mcount; /* number of non-NULL keys, >= count (which is non-NULL vals) */
} Scheme_Hash_Table;

typedef struct Scheme_Hash_Tree Scheme_Hash_Tree;

typedef struct Scheme_Bucket
{
  Scheme_Object so;
  void *val;
  char *key;
} Scheme_Bucket;

typedef struct Scheme_Bucket_Table
{
  Scheme_Object so;
  intptr_t size; /* power of 2 */
  intptr_t count;
  Scheme_Bucket **buckets;
  char weak; /* 1 => normal weak, 2 => late weak */
  char with_home;
  void (*make_hash_indices)(void *v, intptr_t *h1, intptr_t *h2);
  int (*compare)(void *v1, void *v2);
  Scheme_Object *mutex;
} Scheme_Bucket_Table;

/* Hash tablekey types, used with scheme_hash_table */
enum {
  SCHEME_hash_string,
  SCHEME_hash_ptr,
  SCHEME_hash_bound_id,
  SCHEME_hash_weak_ptr,
  SCHEME_hash_late_weak_ptr
};

typedef struct Scheme_Env Scheme_Env;

#define SCHEME_VAR_BUCKET(obj) ((Scheme_Bucket *)(obj))

/*========================================================================*/
/*                    setjmpup (continuation) support                     */
/*========================================================================*/

#ifdef USE_MZ_SETJMP
typedef intptr_t mz_pre_jmp_buf[8];
#else
# define mz_pre_jmp_buf jmp_buf
#endif

#ifdef MZ_USE_JIT
typedef struct { 
  mz_pre_jmp_buf jb; 
  uintptr_t stack_frame; /* declared as `uintptr_t' to hide pointer from 3m xform */
} mz_one_jit_jmp_buf;
typedef mz_one_jit_jmp_buf mz_jit_jmp_buf[1];
#else
# define mz_jit_jmp_buf mz_pre_jmp_buf
#endif

#ifdef MZ_PRECISE_GC
typedef struct {
  XFORM_CAN_IGNORE mz_jit_jmp_buf jb;
  intptr_t gcvs; /* declared as `intptr_t' to hide pointer from 3m xform */
  intptr_t gcvs_cnt;
} mz_jmp_buf;
#else
# define mz_jmp_buf mz_jit_jmp_buf
#endif

/* Like setjmp & longjmp, but you can jmp to a deeper stack position */
/* Initialize a Scheme_Jumpup_Buf record before using it */
typedef struct Scheme_Jumpup_Buf {
  void *stack_from, *stack_copy;
  intptr_t stack_size, stack_max_size;
  struct Scheme_Cont *cont; /* for sharing continuation tails */
  mz_jmp_buf buf;
#ifdef MZ_PRECISE_GC
  void *gc_var_stack;
  void *external_stack;
#endif
} Scheme_Jumpup_Buf;

typedef struct Scheme_Jumpup_Buf_Holder {
  Scheme_Type type; /* for precise GC only */
  Scheme_Jumpup_Buf buf;
} Scheme_Jumpup_Buf_Holder;

typedef struct Scheme_Continuation_Jump_State {
  struct Scheme_Object *jumping_to_continuation;
  struct Scheme_Object *alt_full_continuation;
  Scheme_Object *val; /* or **vals */
  mzshort num_vals;
  char is_kill, is_escape, skip_dws;
} Scheme_Continuation_Jump_State;

/* A mark position is in odd number, so that it can be
   viewed as a pointer (i.e., a fixnum): */
#define MZ_MARK_POS_TYPE intptr_t
/* A mark "pointer" is an offset into the stack: */
#define MZ_MARK_STACK_TYPE intptr_t

typedef struct Scheme_Cont_Frame_Data {
  MZ_MARK_POS_TYPE cont_mark_pos;
  MZ_MARK_STACK_TYPE cont_mark_stack;
  void *cache;
} Scheme_Cont_Frame_Data;

/*========================================================================*/
/*                              threads                                   */
/*========================================================================*/

#ifdef MZ_PRECISE_GC
# ifdef INCLUDE_WITHOUT_PATHS
#  include "schgc2obj.h"
# else
#  include "../gc2/gc2_obj.h"
# endif
#endif

typedef void (Scheme_Close_Custodian_Client)(Scheme_Object *o, void *data);
typedef void (*Scheme_Exit_Closer_Func)(Scheme_Object *, Scheme_Close_Custodian_Client *, void *);
typedef Scheme_Object *(*Scheme_Custodian_Extractor)(Scheme_Object *o);

#ifdef MZ_PRECISE_GC
typedef struct Scheme_Object Scheme_Custodian_Reference;
#else
typedef struct Scheme_Custodian *Scheme_Custodian_Reference;
#endif

typedef struct Scheme_Custodian Scheme_Custodian;
typedef Scheme_Bucket_Table Scheme_Thread_Cell_Table;
typedef struct Scheme_Config Scheme_Config;

typedef int (*Scheme_Ready_Fun)(Scheme_Object *o);
typedef void (*Scheme_Needs_Wakeup_Fun)(Scheme_Object *, void *);
typedef Scheme_Object *(*Scheme_Sync_Sema_Fun)(Scheme_Object *, int *repost);
typedef int (*Scheme_Sync_Filter_Fun)(Scheme_Object *);

/* The Scheme_Thread structure represents a Racket thread. */

typedef struct Scheme_Thread {
  Scheme_Object so;

  struct Scheme_Thread *next;
  struct Scheme_Thread *prev;

  struct Scheme_Thread_Set *t_set_parent;
  Scheme_Object *t_set_next;
  Scheme_Object *t_set_prev;

  mz_jmp_buf *error_buf;
  Scheme_Continuation_Jump_State cjs;
  struct Scheme_Meta_Continuation *decompose_mc; /* set during a jump */

  Scheme_Thread_Cell_Table *cell_values;
  Scheme_Config *init_config;

  Scheme_Object *init_break_cell;
  int can_break_at_swap;

  Scheme_Object **runstack;
  Scheme_Object **runstack_start;
  intptr_t runstack_size;
  struct Scheme_Saved_Stack *runstack_saved;
  Scheme_Object **runstack_tmp_keep;

  Scheme_Object **spare_runstack;   /* in case of bouncing, we keep a recently
                                       released runstack; it's dropped on GC, though */
  intptr_t spare_runstack_size;

  struct Scheme_Thread **runstack_owner;
  struct Scheme_Saved_Stack *runstack_swapped;

  MZ_MARK_POS_TYPE cont_mark_pos;     /* depth of the continuation chain */
  MZ_MARK_STACK_TYPE cont_mark_stack; /* current mark stack position */
  struct Scheme_Cont_Mark **cont_mark_stack_segments;
  intptr_t cont_mark_seg_count;
  intptr_t cont_mark_stack_bottom; /* for restored delimited continuations */
  intptr_t cont_mark_pos_bottom;   /* for splicing cont marks in meta continuations */

  struct Scheme_Thread **cont_mark_stack_owner;
  struct Scheme_Cont_Mark *cont_mark_stack_swapped;

  struct Scheme_Prompt *meta_prompt; /* a pseudo-prompt */
  
  struct Scheme_Meta_Continuation *meta_continuation;
  struct Scheme_Prompt *acting_barrier_prompt;

  intptr_t engine_weight;

  void *stack_start; /* This is the C stack base of the thread, which 
                        corresponds to the starting stack address for
                        paging out the thread, and in 3m corresponds to
                        the starting stack address for GC marking. In non-3m,
                        it can be 0, which means that the deepest (non-main)
                        thread starting address should be used. This value will
                        change when a continuation is applied under a prompt, 
                        and it will be changed on stack overflow. */
  void *stack_end; /* The end of the C stack, for determine stack overflow.
                      Currently, this is the same for all threads. */

  Scheme_Jumpup_Buf jmpup_buf; /* For jumping back to this thread */

  struct Scheme_Dynamic_Wind *dw;
  int next_meta;  /* amount to move forward in the meta-continuaiton chain, starting with dw */

  int running;
  Scheme_Object *suspended_box; /* contains pointer to thread when it's suspended */
  Scheme_Object *resumed_box;   /* contains pointer to thread when it's resumed */
  Scheme_Object *dead_box;      /* contains non-zero when the thread is dead */
  Scheme_Object *running_box;   /* contains pointer to thread when it's running */

  struct Scheme_Thread *nester, *nestee;

  struct future_t *current_ft;

  double sleep_end; /* blocker has starting sleep time */
  int block_descriptor;
  Scheme_Object *blocker; /* semaphore or port */
  Scheme_Ready_Fun block_check;
  Scheme_Needs_Wakeup_Fun block_needs_wakeup;
  char ran_some;
  char suspend_to_kill;

  struct Scheme_Thread *return_marks_to;
  Scheme_Object *returned_marks;

  struct Scheme_Overflow *overflow;

  struct Scheme_Comp_Env *current_local_env;
  Scheme_Object *current_local_mark;
  Scheme_Object *current_local_name;
  Scheme_Object *current_local_modidx;
  Scheme_Env *current_local_menv;
  Scheme_Object *current_local_bindings;
  intptr_t current_phase_shift;

  struct Scheme_Marshal_Tables *current_mt;

  Scheme_Object *constant_folding; /* compiler hack */
  Scheme_Object *reading_delayed; /* reader hack */

  Scheme_Object *(*overflow_k)(void);
  Scheme_Object *overflow_reply;

   /* content of tail_buffer is zeroed on GC, unless
      runstack_tmp_keep is set to tail_buffer */
  Scheme_Object **tail_buffer;
  int tail_buffer_size;

  /* values_buffer is used to avoid allocating for `values'
     calls. When ku.multiple.array is not the same as
     values_buffer, then it can be zeroed at GC points. */
  Scheme_Object **values_buffer;
  int values_buffer_size;

  struct { /* used to be a union, but that confuses MZ_PRECISE_GC */
    struct {
      Scheme_Object *wait_expr;
    } eval;
    struct {
      Scheme_Object *tail_rator;
      Scheme_Object **tail_rands;
      intptr_t tail_num_rands;
    } apply;
    struct {
      Scheme_Object **array;
      intptr_t count;
    } multiple;
    struct {
      void *p1, *p2, *p3, *p4, *p5;
      intptr_t i1, i2, i3, i4;
    } k;
  } ku;

  short suspend_break;
  short external_break;

  Scheme_Simple_Object *list_stack;
  int list_stack_pos;

  /* Racket client can use: */
  void (*on_kill)(struct Scheme_Thread *p);
  void *kill_data;

  /* Racket use only: */
  void (*private_on_kill)(void *);
  void *private_kill_data;
  void **private_kill_next; /* array of three pointers */

  void **user_tls;
  int user_tls_size;

  /* save thread-specific GMP state: */
  intptr_t gmp_tls[6];
  void *gmp_tls_data;

  intptr_t accum_process_msec;
  intptr_t current_start_process_msec;

  struct Scheme_Thread_Custodian_Hop *mr_hop;
  Scheme_Custodian_Reference *mref;
  Scheme_Object *extra_mrefs; /* More owning custodians */
  Scheme_Object *transitive_resumes; /* A hash table of running-boxes */

  Scheme_Object *name;

  Scheme_Object *mbox_first;
  Scheme_Object *mbox_last;
  Scheme_Object *mbox_sema;

  long saved_errno;

  int futures_slow_path_tracing;

#ifdef MZ_PRECISE_GC
  struct GC_Thread_Info *gc_info; /* managed by the GC */
  void *place_channel_msg_in_flight;
#endif

} Scheme_Thread;

#include "schthread.h"

#if !SCHEME_DIRECT_EMBEDDED
# ifdef LINK_EXTENSIONS_BY_TABLE
#  define scheme_current_thread (*scheme_current_thread_ptr)
# endif
#endif

typedef void (*Scheme_Kill_Action_Func)(void *);

#define ESCAPE_BLOCK(return_code) \
    thread = scheme_get_current_thread(); \
    savebuf = thread->error_buf; \
    thread->error_buf = &newbuf; \
    thread = NULL; \
    if (scheme_setjmp(newbuf)) \
    { \
      thread = scheme_get_current_thread(); \
      thread->error_buf = savebuf; \
      scheme_clear_escape(); \
      return return_code; \
    }

# define BEGIN_ESCAPEABLE(func, data) \
    { mz_jmp_buf * volatile savebuf, newbuf; \
      Scheme_Thread *thread; \
      thread = scheme_get_current_thread(); \
      scheme_push_kill_action((Scheme_Kill_Action_Func)func, (void *)data); \
      savebuf = thread->error_buf; \
      thread->error_buf = &newbuf; \
      thread = NULL; \
      if (scheme_setjmp(newbuf)) { \
        scheme_pop_kill_action(); \
        thread = scheme_get_current_thread(); \
        if (!thread->cjs.skip_dws) { \
          func(data); \
        } \
        scheme_longjmp(*savebuf, 1); \
      } else {
# define END_ESCAPEABLE() \
      thread = scheme_get_current_thread(); \
      scheme_pop_kill_action(); \
      thread->error_buf = savebuf; \
      thread = NULL; } }

typedef int (*Scheme_Frozen_Stack_Proc)(void *);

/*========================================================================*/
/*                             parameters                                 */
/*========================================================================*/

enum {
  MZCONFIG_ENV,
  MZCONFIG_INPUT_PORT,
  MZCONFIG_OUTPUT_PORT,
  MZCONFIG_ERROR_PORT,

  MZCONFIG_ERROR_DISPLAY_HANDLER,
  MZCONFIG_ERROR_PRINT_VALUE_HANDLER,

  MZCONFIG_EXIT_HANDLER,

  MZCONFIG_INIT_EXN_HANDLER,

  MZCONFIG_EVAL_HANDLER,
  MZCONFIG_COMPILE_HANDLER,
  MZCONFIG_LOAD_HANDLER,
  MZCONFIG_LOAD_COMPILED_HANDLER,

  MZCONFIG_PRINT_HANDLER,
  MZCONFIG_PROMPT_READ_HANDLER,
  MZCONFIG_READ_HANDLER,
  MZCONFIG_READ_INPUT_PORT_HANDLER,

  MZCONFIG_READTABLE,
  MZCONFIG_READER_GUARD,

  MZCONFIG_CAN_READ_GRAPH,
  MZCONFIG_CAN_READ_COMPILED,
  MZCONFIG_CAN_READ_BOX,
  MZCONFIG_CAN_READ_PIPE_QUOTE,
  MZCONFIG_CAN_READ_DOT,
  MZCONFIG_CAN_READ_INFIX_DOT,
  MZCONFIG_CAN_READ_QUASI,
  MZCONFIG_CAN_READ_READER,
  MZCONFIG_CAN_READ_LANG,
  MZCONFIG_READ_DECIMAL_INEXACT,
  
  MZCONFIG_PRINT_GRAPH,
  MZCONFIG_PRINT_STRUCT,
  MZCONFIG_PRINT_BOX,
  MZCONFIG_PRINT_VEC_SHORTHAND,
  MZCONFIG_PRINT_HASH_TABLE,
  MZCONFIG_PRINT_UNREADABLE,
  MZCONFIG_PRINT_PAIR_CURLY,
  MZCONFIG_PRINT_MPAIR_CURLY,
  MZCONFIG_PRINT_SYNTAX_WIDTH,
  MZCONFIG_PRINT_READER,
  MZCONFIG_PRINT_LONG_BOOLEAN,
  MZCONFIG_PRINT_AS_QQ,

  MZCONFIG_CASE_SENS,
  MZCONFIG_SQUARE_BRACKETS_ARE_PARENS,
  MZCONFIG_CURLY_BRACES_ARE_PARENS,

  MZCONFIG_ERROR_PRINT_WIDTH,
  MZCONFIG_ERROR_PRINT_CONTEXT_LENGTH,

  MZCONFIG_ERROR_ESCAPE_HANDLER,

  MZCONFIG_EXE_YIELD_HANDLER,

  MZCONFIG_ALLOW_SET_UNDEFINED,
  MZCONFIG_COMPILE_MODULE_CONSTS,
  MZCONFIG_USE_JIT,
  MZCONFIG_DISALLOW_INLINE,

  MZCONFIG_CUSTODIAN,
  MZCONFIG_INSPECTOR,
  MZCONFIG_CODE_INSPECTOR,

  MZCONFIG_USE_COMPILED_KIND,
  MZCONFIG_USE_USER_PATHS,
  MZCONFIG_USE_LINK_PATHS,

  MZCONFIG_LOAD_DIRECTORY,
  MZCONFIG_WRITE_DIRECTORY,

  MZCONFIG_COLLECTION_PATHS,

  MZCONFIG_PORT_PRINT_HANDLER,

  MZCONFIG_LOAD_EXTENSION_HANDLER,

  MZCONFIG_CURRENT_DIRECTORY,

  MZCONFIG_RANDOM_STATE,

  MZCONFIG_CURRENT_MODULE_RESOLVER,
  MZCONFIG_CURRENT_MODULE_NAME,
  MZCONFIG_CURRENT_MODULE_SRC,

  MZCONFIG_ERROR_PRINT_SRCLOC,

  MZCONFIG_CMDLINE_ARGS,

  MZCONFIG_LOCALE,

  MZCONFIG_SECURITY_GUARD,

  MZCONFIG_PORT_COUNT_LINES,

  MZCONFIG_SCHEDULER_RANDOM_STATE,

  MZCONFIG_THREAD_SET,
  MZCONFIG_THREAD_INIT_STACK_SIZE,

  MZCONFIG_SUBPROC_CUSTODIAN_MODE,
  MZCONFIG_SUBPROC_GROUP_ENABLED,

  MZCONFIG_LOAD_DELAY_ENABLED,
  MZCONFIG_DELAY_LOAD_INFO,

  MZCONFIG_EXPAND_OBSERVE,

  MZCONFIG_LOGGER,

  __MZCONFIG_BUILTIN_COUNT__
};

/*========================================================================*/
/*                                  ports                                 */
/*========================================================================*/

typedef struct Scheme_Input_Port Scheme_Input_Port;
typedef struct Scheme_Output_Port Scheme_Output_Port;
typedef struct Scheme_Port Scheme_Port;

typedef intptr_t (*Scheme_Get_String_Fun)(Scheme_Input_Port *port,
				      char *buffer, intptr_t offset, intptr_t size,
				      int nonblock, Scheme_Object *unless);
typedef intptr_t (*Scheme_Peek_String_Fun)(Scheme_Input_Port *port,
				       char *buffer, intptr_t offset, intptr_t size,
				       Scheme_Object *skip,
				       int nonblock, Scheme_Object *unless);
typedef Scheme_Object *(*Scheme_Progress_Evt_Fun)(Scheme_Input_Port *port);
typedef int (*Scheme_Peeked_Read_Fun)(Scheme_Input_Port *port,
				      intptr_t amount,
				      Scheme_Object *unless_evt,
				      Scheme_Object *target_ch);
typedef int (*Scheme_In_Ready_Fun)(Scheme_Input_Port *port);
typedef void (*Scheme_Close_Input_Fun)(Scheme_Input_Port *port);
typedef void (*Scheme_Need_Wakeup_Input_Fun)(Scheme_Input_Port *, void *);

typedef Scheme_Object *(*Scheme_Location_Fun)(Scheme_Port *);
typedef void (*Scheme_Count_Lines_Fun)(Scheme_Port *);
typedef int (*Scheme_Buffer_Mode_Fun)(Scheme_Port *, int m);

typedef Scheme_Object *(*Scheme_Write_String_Evt_Fun)(Scheme_Output_Port *,
						      const char *str, intptr_t offset, intptr_t size);
typedef intptr_t (*Scheme_Write_String_Fun)(Scheme_Output_Port *,
					const char *str, intptr_t offset, intptr_t size,
					int rarely_block, int enable_break);
typedef int (*Scheme_Out_Ready_Fun)(Scheme_Output_Port *port);
typedef void (*Scheme_Close_Output_Fun)(Scheme_Output_Port *port);
typedef void (*Scheme_Need_Wakeup_Output_Fun)(Scheme_Output_Port *, void *);
typedef Scheme_Object *(*Scheme_Write_Special_Evt_Fun)(Scheme_Output_Port *, Scheme_Object *);
typedef int (*Scheme_Write_Special_Fun)(Scheme_Output_Port *, Scheme_Object *,
					int nonblock);

struct Scheme_Port
{
  Scheme_Object so;
  char count_lines, was_cr;
  intptr_t position, readpos, lineNumber, charsSinceNewline;
  intptr_t column, oldColumn; /* column tracking with one tab/newline ungetc */
  int utf8state;
  Scheme_Location_Fun location_fun;
  Scheme_Count_Lines_Fun count_lines_fun;
  Scheme_Buffer_Mode_Fun buffer_mode_fun;
};

struct Scheme_Input_Port
{
  struct Scheme_Port p;
  char slow; /* 0 => no line count, no ungotten, etc.: can call get_string_fun directly */
  char closed, pending_eof;
  Scheme_Object *sub_type;
  Scheme_Object *closed_evt;
  Scheme_Custodian_Reference *mref;
  void *port_data;
  Scheme_Get_String_Fun get_string_fun;
  Scheme_Peek_String_Fun peek_string_fun;
  Scheme_Progress_Evt_Fun progress_evt_fun;
  Scheme_Peeked_Read_Fun peeked_read_fun;
  Scheme_In_Ready_Fun byte_ready_fun;
  Scheme_Close_Input_Fun close_fun;
  Scheme_Need_Wakeup_Input_Fun need_wakeup_fun;
  Scheme_Object *read_handler;
  Scheme_Object *name;
  Scheme_Object *peeked_read, *peeked_write;
  Scheme_Object *progress_evt, *input_lock, *input_giveup, *input_extras, *input_extras_ready;
  unsigned char ungotten[24];
  int ungotten_count;
  Scheme_Object *special, *ungotten_special;
  Scheme_Object *unless, *unless_cache;
  struct Scheme_Output_Port *output_half;
};

struct Scheme_Output_Port
{
  struct Scheme_Port p;
  short closed;
  Scheme_Object *sub_type;
  Scheme_Object *closed_evt;
  Scheme_Custodian_Reference *mref;
  void *port_data;
  Scheme_Write_String_Evt_Fun write_string_evt_fun;
  Scheme_Write_String_Fun write_string_fun;
  Scheme_Close_Output_Fun close_fun;
  Scheme_Out_Ready_Fun ready_fun;
  Scheme_Need_Wakeup_Output_Fun need_wakeup_fun;
  Scheme_Write_Special_Evt_Fun write_special_evt_fun;
  Scheme_Write_Special_Fun write_special_fun;
  intptr_t pos;
  Scheme_Object *name;
  Scheme_Object *display_handler;
  Scheme_Object *write_handler;
  Scheme_Object *print_handler;
  struct Scheme_Input_Port *input_half;
};

#define SCHEME_INPORT_VAL(obj) (((Scheme_Input_Port *)(obj))->port_data)
#define SCHEME_OUTPORT_VAL(obj) (((Scheme_Output_Port *)(obj))->port_data)
#define SCHEME_IPORT_NAME(obj) (((Scheme_Input_Port *)obj)->name)

#define SCHEME_SPECIAL (-2)
#define SCHEME_UNLESS_READY (-3)

/*========================================================================*/
/*                              exceptions                                */
/*========================================================================*/

/* This file includes the MZEXN constants */
#ifdef INCLUDE_WITHOUT_PATHS
# include "schexn.h"
#else
# include "../src/schexn.h"
#endif

#define SCHEME_LOG_FATAL   1
#define SCHEME_LOG_ERROR   2
#define SCHEME_LOG_WARNING 3
#define SCHEME_LOG_INFO    4
#define SCHEME_LOG_DEBUG   5

typedef struct Scheme_Logger Scheme_Logger;

/*========================================================================*/
/*                               security                                 */
/*========================================================================*/

#define SCHEME_GUARD_FILE_READ    0x1
#define SCHEME_GUARD_FILE_WRITE   0x2
#define SCHEME_GUARD_FILE_EXECUTE 0x4
#define SCHEME_GUARD_FILE_DELETE  0x8
#define SCHEME_GUARD_FILE_EXISTS  0x10

/*========================================================================*/
/*                               modules                                  */
/*========================================================================*/

typedef void (*Scheme_Invoke_Proc)(Scheme_Env *env, intptr_t phase_shift,
				   Scheme_Object *self_modidx, void *data);

/*========================================================================*/
/*                               evaluation                               */
/*========================================================================*/

/* Exploit the fact that these should never be dereferenced: */
#ifndef FIRST_TWO_BYTES_ARE_LEGAL_ADDRESSES
# define MZ_EVAL_WAITING_CONSTANT ((Scheme_Object *)0x2)
# define MZ_APPLY_WAITING_CONSTANT ((Scheme_Object *)0x4)
# define MZ_MULTIPLE_VALUES_CONSTANT ((Scheme_Object *)0x6)
#endif

#ifdef MZ_EVAL_WAITING_CONSTANT
# define SCHEME_EVAL_WAITING MZ_EVAL_WAITING_CONSTANT
# define SCHEME_TAIL_CALL_WAITING MZ_APPLY_WAITING_CONSTANT
# define SCHEME_MULTIPLE_VALUES MZ_MULTIPLE_VALUES_CONSTANT
#else
# define SCHEME_TAIL_CALL_WAITING scheme_tail_call_waiting
# define SCHEME_EVAL_WAITING scheme_eval_waiting
# define SCHEME_MULTIPLE_VALUES scheme_multiple_values
#endif

#define SCHEME_ASSERT(expr,msg) ((expr) ? 1 : (scheme_signal_error(msg), 0))

#ifdef MZ_CAN_ACCESS_THREAD_LOCAL_DIRECTLY
# define mzSCHEME_CURRENT_THREAD scheme_current_thread
#else
# define mzSCHEME_CURRENT_THREAD scheme_get_current_thread()
#endif

#define scheme_eval_wait_expr (mzSCHEME_CURRENT_THREAD->ku.eval.wait_expr)
#define scheme_tail_rator (mzSCHEME_CURRENT_THREAD->ku.apply.tail_rator)
#define scheme_tail_num_rands (mzSCHEME_CURRENT_THREAD->ku.apply.tail_num_rands)
#define scheme_tail_rands (mzSCHEME_CURRENT_THREAD->ku.apply.tail_rands)
#define scheme_overflow_reply (mzSCHEME_CURRENT_THREAD->overflow_reply)

#define scheme_error_buf *(mzSCHEME_CURRENT_THREAD->error_buf)
#define scheme_jumping_to_continuation (mzSCHEME_CURRENT_THREAD->cjs.jumping_to_continuation)

#define scheme_multiple_count (mzSCHEME_CURRENT_THREAD->ku.multiple.count)
#define scheme_multiple_array (mzSCHEME_CURRENT_THREAD->ku.multiple.array)

#define scheme_setjmpup(b, base, s) scheme_setjmpup_relative(b, base, s, NULL)

#define scheme_do_eval_w_thread(r,n,e,f,p) scheme_do_eval(r,n,e,f)
#define scheme_apply_wp(r,n,a,p) scheme_apply(r,n,a)
#define scheme_apply_multi_wp(r,n,a,p) scheme_apply_multi(r,n,a)
#define scheme_apply_eb_wp(r,n,a,p) scheme_apply_eb(r,n,a)
#define scheme_apply_multi_eb_wp(r,n,a,p) scheme_apply_multi_eb(r,n,a)

#define _scheme_apply(r,n,rs) scheme_do_eval(r,n,rs,1)
#define _scheme_apply_multi(r,n,rs) scheme_do_eval(r,n,rs,-1)
#define _scheme_apply_wp(r,n,rs,p) scheme_do_eval_w_thread(r,n,rs,1,p)
#define _scheme_apply_multi_wp(r,n,rs,p) scheme_do_eval_w_thread(r,n,rs,-1,p)
#define _scheme_tail_apply scheme_tail_apply
#define _scheme_tail_apply_wp scheme_tail_apply_wp

#define _scheme_tail_eval scheme_tail_eval
#define _scheme_tail_eval_wp scheme_tail_eval_wp

#define _scheme_direct_apply_primitive_multi(prim, argc, argv) \
  (((Scheme_Primitive_Proc *)prim)->prim_val(argc, argv, prim))
#define _scheme_direct_apply_primitive(prim, argc, argv) \
  scheme_check_one_value(_scheme_direct_apply_primitive_multi(prim, argc, argv))
#define _scheme_direct_apply_primitive_closure_multi(prim, argc, argv) \
  _scheme_direct_apply_primitive_multi(prim, argc, argv)
#define _scheme_direct_apply_primitive_closure(prim, argc, argv) \
  _scheme_direct_apply_primitive(prim, argc, argv)
#define _scheme_direct_apply_closed_primitive_multi(prim, argc, argv) \
    (((Scheme_Closed_Primitive_Proc *)prim)->prim_val(((Scheme_Closed_Primitive_Proc *)prim)->data, argc, argv))
#define _scheme_direct_apply_closed_primitive(prim, argc, argv) \
    scheme_check_one_value(_scheme_direct_apply_closed_primitive_multi(prim, argc, argv))

#define _scheme_force_value(v) ((v == SCHEME_TAIL_CALL_WAITING) ? scheme_force_value(v) : v)

#define scheme_tail_apply_buffer_wp(n, p) ((p)->tail_buffer)
#define scheme_tail_apply_buffer(n) \
{ \
  Scheme_Thread *thread; \
  thread = scheme_get_current_thread(); \
  scheme_tail_apply_buffer_wp(n, thread);\
}

#define _scheme_tail_apply_no_copy_wp_tcw(f, n, args, p, tcw) (p->ku.apply.tail_rator = f, p->ku.apply.tail_rands = args, p->ku.apply.tail_num_rands = n, tcw)
#define _scheme_tail_apply_no_copy_wp(f, n, args, p) _scheme_tail_apply_no_copy_wp_tcw(f, n, args, p, SCHEME_TAIL_CALL_WAITING)
#define _scheme_tail_apply_no_copy(f, n, args) \
{ \
  Scheme_Thread *thread; \
  thread = scheme_get_current_thread(); \
  _scheme_tail_apply_no_copy_wp(f, n, args, thread) \
}

#define scheme_thread_block_w_thread(t,p) scheme_thread_block(t)

#if !SCHEME_DIRECT_EMBEDDED
# ifdef LINK_EXTENSIONS_BY_TABLE
#  define scheme_fuel_counter (*scheme_fuel_counter_ptr)
# endif
#else
THREAD_LOCAL_DECL(MZ_EXTERN volatile int scheme_fuel_counter);
#endif

#ifdef FUEL_AUTODECEREMENTS
# define DECREMENT_FUEL(f, p) (f)
#else
# define DECREMENT_FUEL(f, p) (f -= (p))
#endif

#define SCHEME_USE_FUEL(n) \
  { if (DECREMENT_FUEL(scheme_fuel_counter, n) <= 0) { scheme_out_of_fuel(); }}

#if SCHEME_DIRECT_EMBEDDED
MZ_EXTERN Scheme_Object *scheme_eval_waiting;
#define scheme_tail_eval(obj) \
 (scheme_eval_wait_expr = obj, SCHEME_EVAL_WAITING)
#endif

#define scheme_break_waiting(p) (p->external_break)

#ifndef USE_MZ_SETJMP
# ifdef USE_UNDERSCORE_SETJMP
#  define scheme_mz_longjmp(b, v) _longjmp(b, v)
#  define scheme_mz_setjmp(b) _setjmp(b)
# else
#  define scheme_mz_longjmp(b, v) longjmp(b, v)
#  define scheme_mz_setjmp(b) setjmp(b)
# endif
#endif

#ifdef MZ_USE_JIT
MZ_EXTERN void scheme_jit_longjmp(mz_jit_jmp_buf b, int v);
MZ_EXTERN void scheme_jit_setjmp_prepare(mz_jit_jmp_buf b);
# define scheme_jit_setjmp(b) (scheme_jit_setjmp_prepare(b), scheme_mz_setjmp((b)->jb))
#else
# define scheme_jit_longjmp(b, v) scheme_mz_longjmp(b, v) 
# define scheme_jit_setjmp(b) scheme_mz_setjmp(b) 
#endif

#ifdef MZ_PRECISE_GC
/* Need to make sure that a __gc_var_stack__ is always available where
   setjmp & longjmp are used. */
# define scheme_longjmp(b, v) (((intptr_t *)(void*)((b).gcvs))[1] = (b).gcvs_cnt, \
                               GC_variable_stack = (void **)(void*)(b).gcvs, \
                               scheme_jit_longjmp((b).jb, v))
# define scheme_setjmp(b)     ((b).gcvs = (intptr_t)__gc_var_stack__, \
                               (b).gcvs_cnt = (intptr_t)(__gc_var_stack__[1]), \
                               scheme_jit_setjmp((b).jb))
#else
# define scheme_longjmp(b, v) scheme_jit_longjmp(b, v)
# define scheme_setjmp(b) scheme_jit_setjmp(b)
#endif

/*========================================================================*/
/*                      memory management macros                          */
/*========================================================================*/

/* Allocation */
#define scheme_alloc_object() \
   ((Scheme_Object *) scheme_malloc_small_tagged(sizeof(Scheme_Simple_Object)))
#define scheme_alloc_small_object() \
   ((Scheme_Object *) scheme_malloc_small_tagged(sizeof(Scheme_Small_Object)))
#define scheme_alloc_stubborn_object() \
   ((Scheme_Object *) scheme_malloc_stubborn_tagged(sizeof(Scheme_Simple_Object)))
#define scheme_alloc_stubborn_small_object() \
   ((Scheme_Object *) scheme_malloc_stubborn_tagged(sizeof(Scheme_Small_Object)))
#define scheme_alloc_eternal_object() \
   ((Scheme_Object *) scheme_malloc_eternal_tagged(sizeof(Scheme_Simple_Object)))
#define scheme_alloc_eternal_small_object() \
   ((Scheme_Object *) scheme_malloc_eternal_tagged(sizeof(Scheme_Small_Object)))

#ifdef SCHEME_NO_GC
void *scheme_malloc(size_t size);
# define scheme_malloc_atomic scheme_malloc
# define scheme_malloc_stubborn scheme_malloc
# define scheme_malloc_uncollectable scheme_malloc
#else
# define scheme_malloc GC_malloc
# define scheme_malloc_atomic GC_malloc_atomic
# ifdef MZ_PRECISE_GC
#  define scheme_malloc_stubborn scheme_malloc
# else
#  define scheme_malloc_stubborn GC_malloc_stubborn
#  define scheme_malloc_uncollectable GC_malloc_uncollectable
# endif
#endif

#ifdef USE_MEMORY_TRACING
# define USE_TAGGED_ALLOCATION
# define MEMORY_COUNTING_ON
#endif

#ifdef MZ_PRECISE_GC
# ifndef GC2_EXTERN
#  define GC2_EXTERN MZ_EXTERN
# endif
# ifdef INCLUDE_WITHOUT_PATHS
#  if !SCHEME_DIRECT_EMBEDDED
#   define GC2_JUST_MACROS_AND_TYPEDEFS
#  endif
#  include "schemegc2.h"
# else
#  include "../gc2/gc2.h"
# endif
# define scheme_malloc_tagged GC_malloc_one_tagged
# define scheme_malloc_small_tagged(s) GC_malloc_one_small_tagged(gcWORDS_TO_BYTES(gcBYTES_TO_WORDS(s)))
# define scheme_malloc_small_dirty_tagged(s) GC_malloc_one_small_dirty_tagged(gcWORDS_TO_BYTES(gcBYTES_TO_WORDS(s)))
# define scheme_malloc_small_atomic_tagged(s) GC_malloc_small_atomic_tagged(gcWORDS_TO_BYTES(gcBYTES_TO_WORDS(s)))
# define scheme_malloc_array_tagged GC_malloc_array_tagged
# define scheme_malloc_atomic_tagged GC_malloc_atomic_tagged
# define scheme_malloc_stubborn_tagged GC_malloc_one_tagged
# define scheme_malloc_eternal_tagged GC_malloc_atomic_uncollectable
# define scheme_malloc_uncollectable_tagged >> error <<
# define scheme_malloc_envunbox GC_malloc
# define scheme_malloc_weak GC_malloc_weak
# define scheme_malloc_weak_tagged GC_malloc_one_weak_tagged
# define scheme_malloc_allow_interior GC_malloc_allow_interior
# define scheme_malloc_atomic_allow_interior GC_malloc_atomic_allow_interior
#else
# ifdef USE_TAGGED_ALLOCATION
extern void *scheme_malloc_tagged(size_t);
#  define scheme_malloc_array_tagged scheme_malloc
#  define scheme_malloc_small_tagged scheme_malloc
extern void *scheme_malloc_atomic_tagged(size_t);
extern void *scheme_malloc_stubborn_tagged(size_t);
extern void *scheme_malloc_eternal_tagged(size_t);
extern void *scheme_malloc_uncollectable_tagged(size_t);
extern void *scheme_malloc_envunbox(size_t);
# else
#  define scheme_malloc_tagged scheme_malloc
#  define scheme_malloc_small_tagged scheme_malloc
#  define scheme_malloc_array_tagged scheme_malloc
#  define scheme_malloc_atomic_tagged scheme_malloc_atomic
#  define scheme_malloc_stubborn_tagged scheme_malloc_stubborn
#  define scheme_malloc_eternal_tagged scheme_malloc_eternal
#  define scheme_malloc_uncollectable_tagged scheme_malloc_uncollectable
#  define scheme_malloc_envunbox scheme_malloc
# endif
# define scheme_malloc_small_dirty_tagged scheme_malloc_small_tagged
# define scheme_malloc_allow_interior scheme_malloc
# define scheme_malloc_atomic_allow_interior scheme_malloc_atomic
# define scheme_malloc_small_atomic_tagged scheme_malloc_atomic_tagged
#endif

#ifdef MZ_PRECISE_GC
# define MZ_GC_DECL_REG(size) void *__gc_var_stack__[size+2] = { (void *)0, (void *)size };
# define MZ_GC_VAR_IN_REG(x, v) (__gc_var_stack__[x+2] = (void *)&(v))
# define MZ_GC_ARRAY_VAR_IN_REG(x, v, l) (__gc_var_stack__[x+2] = (void *)0, \
                                          __gc_var_stack__[x+3] = (void *)&(v), \
                                          __gc_var_stack__[x+4] = (void *)l)
# define MZ_GC_NO_VAR_IN_REG(x) (__gc_var_stack__[x+2] = NULL)
# define MZ_GC_REG()  (__gc_var_stack__[0] = GC_variable_stack, \
                       GC_variable_stack = __gc_var_stack__)
# define MZ_GC_UNREG() (GC_variable_stack = (void **)__gc_var_stack__[0])
#else
# define MZ_GC_DECL_REG(size)            /* empty */
# define MZ_GC_VAR_IN_REG(x, v)          /* empty */
# define MZ_GC_ARRAY_VAR_IN_REG(x, v, l) /* empty */
# define MZ_GC_NO_VAR_IN_REG(x)          /* empty */
# define MZ_GC_REG()                     /* empty */
# define MZ_GC_UNREG()                   /* empty */
#endif

/*========================================================================*/
/*                   embedding configuration and hooks                    */
/*========================================================================*/

#if SCHEME_DIRECT_EMBEDDED

#if defined(_IBMR2)
MZ_EXTERN intptr_t scheme_stackbottom;
#endif

MZ_EXTERN int scheme_defining_primitives;

/* These flags must be set before Racket is started: */
MZ_EXTERN int scheme_case_sensitive; /* Defaults to 0 */
MZ_EXTERN int scheme_no_keywords; /* Defaults to 0 */
MZ_EXTERN int scheme_allow_set_undefined; /* Defaults to 0 */
MZ_EXTERN int scheme_square_brackets_are_parens; /* Defaults to 1 */
MZ_EXTERN int scheme_curly_braces_are_parens; /* Defaults to 1 */
MZ_EXTERN int scheme_hash_percent_syntax_only; /* Defaults to 0 */
MZ_EXTERN int scheme_hash_percent_globals_only; /* Defaults to 0 */
MZ_EXTERN int scheme_binary_mode_stdio; /* Windows-specific; Defaults to 0 */
MZ_EXTERN int scheme_startup_use_jit; /* Defaults to 1 */
MZ_EXTERN int scheme_ignore_user_paths; /* Defaults to 0 */
MZ_EXTERN int scheme_ignore_link_paths; /* Defaults to 0 */

MZ_EXTERN void scheme_set_case_sensitive(int);
MZ_EXTERN void scheme_set_allow_set_undefined(int);
MZ_EXTERN void scheme_set_binary_mode_stdio(int);
MZ_EXTERN void scheme_set_startup_use_jit(int);
MZ_EXTERN void scheme_set_startup_load_on_demand(int);
MZ_EXTERN void scheme_set_ignore_user_paths(int);
MZ_EXTERN void scheme_set_ignore_link_paths(int);
MZ_EXTERN void scheme_set_logging(int syslog_level, int stderr_level);

MZ_EXTERN int scheme_get_allow_set_undefined();

#ifdef MZ_CAN_ACCESS_THREAD_LOCAL_DIRECTLY
THREAD_LOCAL_DECL(MZ_EXTERN Scheme_Thread *scheme_current_thread);
THREAD_LOCAL_DECL(MZ_EXTERN Scheme_Thread *scheme_first_thread);
#endif
XFORM_NONGCING MZ_EXTERN Scheme_Thread *scheme_get_current_thread();
XFORM_NONGCING MZ_EXTERN intptr_t scheme_get_multiple_count();
XFORM_NONGCING MZ_EXTERN Scheme_Object **scheme_get_multiple_array();
XFORM_NONGCING MZ_EXTERN void scheme_set_current_thread_ran_some();

MZ_EXTERN void scheme_embedded_load(intptr_t len, const char *s, int predefined);
MZ_EXTERN void scheme_register_embedded_load(intptr_t len, const char *s);

/* Set these global hooks (optionally): */
typedef void (*Scheme_Exit_Proc)(int v);
MZ_EXTERN Scheme_Exit_Proc scheme_exit;
MZ_EXTERN void scheme_set_exit(Scheme_Exit_Proc p);
typedef void (*Scheme_At_Exit_Callback_Proc)();
typedef void (*Scheme_At_Exit_Proc)(Scheme_At_Exit_Callback_Proc);
MZ_EXTERN void scheme_set_atexit(Scheme_At_Exit_Proc p);
typedef void (*scheme_console_printf_t)(char *str, ...);
MZ_EXTERN scheme_console_printf_t scheme_console_printf;
MZ_EXTERN scheme_console_printf_t scheme_get_console_printf();
MZ_EXTERN void scheme_set_console_printf(scheme_console_printf_t p);
typedef void (*scheme_console_output_t)(char *str, intptr_t len);
MZ_EXTERN scheme_console_output_t scheme_console_output;
MZ_EXTERN void scheme_set_console_output(scheme_console_output_t p);
MZ_EXTERN void (*scheme_sleep)(float seconds, void *fds);
MZ_EXTERN void (*scheme_notify_multithread)(int on);
MZ_EXTERN void (*scheme_wakeup_on_input)(void *fds);
MZ_EXTERN int (*scheme_check_for_break)(void);
MZ_EXTERN Scheme_Object *(*scheme_module_demand_hook)(int c, Scheme_Object **a);
#ifdef MZ_PRECISE_GC
MZ_EXTERN void *(*scheme_get_external_stack_val)(void);
MZ_EXTERN void (*scheme_set_external_stack_val)(void *);
#endif
#ifdef USE_WIN32_THREADS
MZ_EXTERN void (*scheme_suspend_main_thread)(void);
int scheme_set_in_main_thread(void);
void scheme_restore_nonmain_thread(void);
#endif
#ifdef MAC_FILE_SYSTEM
extern long scheme_creator_id;
#endif

typedef Scheme_Object *(*Scheme_Stdio_Maker_Proc)(void);
MZ_EXTERN Scheme_Object *(*scheme_make_stdin)(void);
MZ_EXTERN Scheme_Object *(*scheme_make_stdout)(void);
MZ_EXTERN Scheme_Object *(*scheme_make_stderr)(void);

MZ_EXTERN void scheme_set_stdio_makers(Scheme_Stdio_Maker_Proc in,
				       Scheme_Stdio_Maker_Proc out,
				       Scheme_Stdio_Maker_Proc err);


MZ_EXTERN void scheme_set_banner(char *s);
MZ_EXTERN Scheme_Object *scheme_set_exec_cmd(char *s);
MZ_EXTERN Scheme_Object *scheme_set_run_cmd(char *s);
MZ_EXTERN void scheme_set_collects_path(Scheme_Object *p);
MZ_EXTERN void scheme_set_original_dir(Scheme_Object *d);
MZ_EXTERN void scheme_set_addon_dir(Scheme_Object *p);
MZ_EXTERN void scheme_set_links_file(Scheme_Object *p);
MZ_EXTERN void scheme_set_command_line_arguments(Scheme_Object *vec);
MZ_EXTERN void scheme_set_compiled_file_paths(Scheme_Object *list);

MZ_EXTERN void scheme_init_collection_paths(Scheme_Env *global_env, Scheme_Object *extra_dirs);
MZ_EXTERN void scheme_init_collection_paths_post(Scheme_Env *global_env, Scheme_Object *extra_dirs, Scheme_Object *extra_post_dirs);

MZ_EXTERN void scheme_seal_parameters();

/* Initialization */
MZ_EXTERN Scheme_Env *scheme_basic_env(void);
MZ_EXTERN void scheme_reset_overflow(void);
MZ_EXTERN void scheme_free_all(void);

#ifdef USE_MSVC_MD_LIBRARY
MZ_EXTERN void GC_pre_init(void);
#endif

MZ_EXTERN void scheme_check_threads(void);
MZ_EXTERN void scheme_wake_up(void);
MZ_EXTERN int scheme_get_external_event_fd(void);

/* GC registration: */
MZ_EXTERN void scheme_set_stack_base(void *base, int no_auto_statics);
MZ_EXTERN void scheme_set_stack_bounds(void *base, void *deepest, int no_auto_statics);

typedef void (*Scheme_Report_Out_Of_Memory_Proc)(void);
MZ_EXTERN void scheme_set_report_out_of_memory(Scheme_Report_Out_Of_Memory_Proc p);

/* Stack-preparation start-up: */
typedef int (*Scheme_Nested_Main)(void *data);
MZ_EXTERN int scheme_main_stack_setup(int no_auto_statics, Scheme_Nested_Main _main, void *data);

/* More automatic start-up: */
typedef int (*Scheme_Env_Main)(Scheme_Env *env, int argc, char **argv);
MZ_EXTERN int scheme_main_setup(int no_auto_statics, Scheme_Env_Main _main, int argc, char **argv);

#ifdef IMPLEMENT_THREAD_LOCAL_VIA_WIN_TLS
MZ_EXTERN void scheme_register_tls_space(void *tls_space, int _tls_index);
#endif

MZ_EXTERN void scheme_register_static(void *ptr, intptr_t size);
#if defined(MUST_REGISTER_GLOBALS) || defined(GC_MIGHT_USE_REGISTERED_STATICS)
# define MZ_REGISTER_STATIC(x)  scheme_register_static((void *)&x, sizeof(x))
#else
# define MZ_REGISTER_STATIC(x) /* empty */
#endif

MZ_EXTERN void scheme_immediate_exit(int status);

MZ_EXTERN int scheme_new_param(void);
MZ_EXTERN Scheme_Object *scheme_param_config(char *name, Scheme_Object *pos,
					     int argc, Scheme_Object **argv,
					     int arity,
					     Scheme_Prim *check, char *expected,
					     int isbool);
MZ_EXTERN Scheme_Object *scheme_register_parameter(Scheme_Prim *function, char *name, int which);

#endif /* SCHEME_DIRECT_EMBEDDED */

/*========================================================================*/
/*                              addrinfo                                  */
/*========================================================================*/

#ifdef HAVE_GETADDRINFO
# define mz_addrinfo addrinfo
#else
struct mz_addrinfo {
  int ai_flags;
  int ai_family;
  int ai_socktype;
  int ai_protocol;
  size_t  ai_addrlen;
  struct sockaddr *ai_addr;
  struct mz_addrinfo *ai_next;
};
#endif

/*========================================================================*/
/*                              FFI functions                             */
/*========================================================================*/

/* If Racket is being empbedded, then we just include the
   prototypes. Otherwise, we may include a function-table definition
   instead, plus macros that map the usual name to table lookups. */

#if SCHEME_DIRECT_EMBEDDED

/* All functions & global constants prototyped here */
#ifdef INCLUDE_WITHOUT_PATHS
# include "schemef.h"
#else
# include "../src/schemef.h"
#endif

#else

#ifdef LINK_EXTENSIONS_BY_TABLE
/* Constants and function prototypes as function pointers in a struct: */
# ifdef INCLUDE_WITHOUT_PATHS
#  include "schemex.h"
# else
#  include "../src/schemex.h"
# endif

extern Scheme_Extension_Table *scheme_extension_table;

/* Macro mapping names to record access */
# ifdef INCLUDE_WITHOUT_PATHS
#  include "schemexm.h"
# else
#  include "../src/schemexm.h"
# endif

#else

/* Not LINK_EXTENSIONS_BY_TABLE */
# ifdef INCLUDE_WITHOUT_PATHS
#  include "schemef.h"
# else
#  include "../src/schemef.h"
# endif

#endif

#endif

/*========================================================================*/
/*                              misc flags                                */
/*========================================================================*/

/* For use with scheme_symbol_name_and_size: */
#define SCHEME_SNF_FOR_TS 0x1
#define SCHEME_SNF_PIPE_QUOTE 0x2
#define SCHEME_SNF_NO_PIPE_QUOTE 0x4
#define SCHEME_SNF_NEED_CASE 0x8
#define SCHEME_SNF_KEYWORD 0x10
#define SCHEME_SNF_NO_KEYWORDS 0x20

/* For use with scheme_make_struct_values et al.: */
#define SCHEME_STRUCT_NO_TYPE 0x01
#define SCHEME_STRUCT_NO_CONSTR 0x02
#define SCHEME_STRUCT_NO_PRED 0x04
#define SCHEME_STRUCT_NO_GET 0x08
#define SCHEME_STRUCT_NO_SET 0x10
#define SCHEME_STRUCT_GEN_GET 0x20
#define SCHEME_STRUCT_GEN_SET 0x40
#define SCHEME_STRUCT_EXPTIME 0x80
#define SCHEME_STRUCT_NO_MAKE_PREFIX 0x100

/*========================================================================*/
/*                           file descriptors                             */
/*========================================================================*/

#if defined(DETECT_WIN32_CONSOLE_STDIN) || defined(WINDOWS_PROCESSES)
# ifndef NO_STDIO_THREADS
#  define USE_FAR_MZ_FDCALLS
# endif
#endif
#ifdef USE_DYNAMIC_FDSET_SIZE
# define USE_FAR_MZ_FDCALLS
#endif
#ifdef USE_BEOS_PORT_THREADS
# define USE_FAR_MZ_FDCALLS
#endif
#ifdef HAVE_POLL_SYSCALL
# define USE_FAR_MZ_FDCALLS
#endif

#ifdef USE_FAR_MZ_FDCALLS
# define MZ_GET_FDSET(p, n) scheme_get_fdset(p, n)
# define MZ_FD_ZERO(p) scheme_fdzero(p)
# define MZ_FD_SET(n, p) scheme_fdset(p, n)
# define MZ_FD_CLR(n, p) scheme_fdclr(p, n)
# define MZ_FD_ISSET(n, p) scheme_fdisset(p, n)
#else
# define MZ_GET_FDSET(p, n) ((void *)(((fd_set *)p) + n))
# define MZ_FD_ZERO(p) FD_ZERO((fd_set *)(p))
# define MZ_FD_SET(n, p) FD_SET(n, (fd_set *)(p))
# define MZ_FD_CLR(n, p) FD_CLR(n, (fd_set *)(p))
# define MZ_FD_ISSET(n, p) FD_ISSET(n, (fd_set *)(p))
#endif

/* For scheme_fd_to_semaphore(): */
#define MZFD_CREATE_READ  1
#define MZFD_CREATE_WRITE 2
#define MZFD_CHECK_READ   3
#define MZFD_CHECK_WRITE  4
#define MZFD_REMOVE       5

/*========================================================================*/

#ifdef __cplusplus
}
#endif

#if defined(__MWERKS__)
# ifdef MZSCHEME_USES_NEAR_GLOBALS
#  pragma far_data reset
# endif
#endif

#endif /* ! SCHEME_H */

