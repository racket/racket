/*
  Racket
  Copyright (c) 2004-2013 PLT Design Inc.
  Copyright (c) 1995-2001 Matthew Flatt

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

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

/* Some copilers don't like re-def of GC_malloc in schemef.h: */
#ifndef MZ_PRECISE_GC
# define SCHEME_NO_GC_PROTO
#endif

#include "schpriv.h"
#include <string.h>
#include <ctype.h>
#include "schgc.h"

# define HASH_TABLE_INIT_SIZE 256
#ifdef SMALL_HASH_TABLES
# define FILL_FACTOR 1.30
#else
# define FILL_FACTOR 2
#endif

#ifndef MZ_PRECISE_GC
extern MZ_DLLIMPORT void (*GC_custom_finalize)(void);
#endif
#ifndef USE_SENORA_GC
extern int GC_is_marked(void *);
#endif

#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
THREAD_LOCAL_DECL(static Scheme_Hash_Table *place_local_symbol_table = NULL;)
THREAD_LOCAL_DECL(static Scheme_Hash_Table *place_local_keyword_table = NULL;)
THREAD_LOCAL_DECL(static Scheme_Hash_Table *place_local_parallel_symbol_table = NULL;)
#endif

SHARED_OK static Scheme_Hash_Table *symbol_table = NULL;
SHARED_OK static Scheme_Hash_Table *keyword_table = NULL;
SHARED_OK static Scheme_Hash_Table *parallel_symbol_table = NULL;

SHARED_OK static uintptr_t scheme_max_symbol_length;

/* globals */
SHARED_OK int scheme_case_sensitive = 1;
THREAD_LOCAL_DECL(static int gensym_counter);

void scheme_set_case_sensitive(int v) { scheme_case_sensitive =  v; }

/* locals */
static Scheme_Object *symbol_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *symbol_p_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *symbol_unreadable_p_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *symbol_interned_p_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_to_symbol_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_to_uninterned_symbol_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_to_unreadable_symbol_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *symbol_to_string_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *keyword_p_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *keyword_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_to_keyword_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *keyword_to_string_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *gensym(int argc, Scheme_Object *argv[]);


/**************************************************************************/

typedef uintptr_t hash_v_t;
#define HASH_SEED  0xF0E1D2C3

#define SYMTAB_LOST_CELL scheme_false

#ifdef MZ_PRECISE_GC
# define WEAK_ARRAY_HEADSIZE 4
#else
# define WEAK_ARRAY_HEADSIZE 0
#endif

static Scheme_Object *rehash_symbol_bucket(Scheme_Hash_Table *table,
                                           GC_CAN_IGNORE const char *key, uintptr_t length,
                                           Scheme_Object *naya);

/* Special hashing for symbols: */
static Scheme_Object *symbol_bucket(Scheme_Hash_Table *table,
				    GC_CAN_IGNORE const char *key, uintptr_t length,
				    Scheme_Object *naya)
{
  hash_v_t h, h2;
  uintptr_t mask;
  Scheme_Object *bucket;

  /* WARNING: key may be GC-misaligned... */
  /* This function is designed to need no MZ_PRECISE_GC instrumentation.
     To handle re-hashing, it tail-calls rehash_symbol_bucket. */

  mask = table->size - 1;

  {
    uintptr_t i;
    i = 0;
    h = HASH_SEED;
    h2 = 0;

    while (i < length) {
      int c = key[i++];
       h ^= (h << 5) + (h >> 2) + c;
       h2 += c;
    }
    /* post hash mixing helps for short symbols */
    h ^= (h << 5) + (h >> 2) + 0xA0A0;
    h ^= (h << 5) + (h >> 2) + 0x0505;

    h = h & mask;
    h2 = h2 & mask;
  }

  h2 |= 0x1;

  while ((bucket = table->keys[WEAK_ARRAY_HEADSIZE + h])) {
    if (SAME_OBJ(bucket, SYMTAB_LOST_CELL)) {
      if (naya) {
	/* We're re-using, so decrement count and it will be
	   re-incremented. */
	--table->count;
	break;
      }
    } else if (((int)length == SCHEME_SYM_LEN(bucket))
	       && !memcmp(key, SCHEME_SYM_VAL(bucket), length))
      return bucket;
    h = (h + h2) & mask;
  }

  /* In case it's GC-misaligned: */
  key = NULL;

  if (!naya)
    return NULL;

  if (table->count * FILL_FACTOR >= table->size) {
    return rehash_symbol_bucket(table, key, length, naya);
  }

  table->keys[WEAK_ARRAY_HEADSIZE + h] = naya;

  table->count++;

  return naya;
}

static Scheme_Object *rehash_symbol_bucket(Scheme_Hash_Table *table,
                                           GC_CAN_IGNORE const char *key, uintptr_t length,
                                           Scheme_Object *naya)
{
  int i, oldsize = table->size, newsize, lostc;
  size_t asize;
  Scheme_Object *cb;
  Scheme_Object **old = table->keys;

  /* WARNING: key may be GC-misaligned... */

  /* Don't grow table if it's mostly lost cells (due to lots of
     temporary symbols). */
  lostc = 0;
  for (i = 0; i < oldsize; i++) {
    cb = old[WEAK_ARRAY_HEADSIZE + i];
    if (cb == SYMTAB_LOST_CELL)
      lostc++;
  }
  if ((lostc * 2) < table->count)
    newsize = oldsize << 1;
  else
    newsize = oldsize;

  asize = (size_t)newsize * sizeof(Scheme_Object *);
  {
    Scheme_Object **ba;
#ifdef MZ_PRECISE_GC
    ba = (Scheme_Object **)GC_malloc_weak_array(asize, SYMTAB_LOST_CELL);
#else
    ba = MALLOC_N_ATOMIC(Scheme_Object *, newsize);
    memset((char *)ba, 0, asize);
#endif
    table->keys = ba;
  }
  table->size = newsize;

  table->count = 0;

  for (i = 0; i < oldsize; i++) {
    cb = old[WEAK_ARRAY_HEADSIZE + i] ;
    if (cb && (cb != SYMTAB_LOST_CELL))
      symbol_bucket(table, SCHEME_SYM_VAL(cb), SCHEME_SYM_LEN(cb), cb);
  }

  /* Restore GC-misaligned key: */
  key = SCHEME_SYM_VAL(naya);

  return symbol_bucket(table, key, length, naya);
}

#ifndef MZ_PRECISE_GC
static void clean_one_symbol_table(Scheme_Hash_Table *table)
{
  /* Clean the symbol table by removing pointers to collected
     symbols. The correct way to do this is to install a GC
     finalizer on symbol pointers, but that would be expensive. */

  if (table) {
    Scheme_Object **buckets = (Scheme_Object **)table->keys;
    int i = table->size;
    void *b;

    while (i--) {
      if (buckets[WEAK_ARRAY_HEADSIZE + i] && !SAME_OBJ(buckets[WEAK_ARRAY_HEADSIZE + i], SYMTAB_LOST_CELL)
          && (!(b = GC_base(buckets[WEAK_ARRAY_HEADSIZE + i]))
#ifndef USE_SENORA_GC
	      || !GC_is_marked(b)
#endif
	      )) {
	buckets[WEAK_ARRAY_HEADSIZE + i] = SYMTAB_LOST_CELL;
      }
    }
  }
}

static void clean_symbol_table(void)
{
  clean_one_symbol_table(symbol_table);
  clean_one_symbol_table(keyword_table);
  clean_one_symbol_table(parallel_symbol_table);

  scheme_clear_ephemerons();
# ifdef MZ_USE_JIT
  scheme_clean_native_symtab();
# endif
# ifndef MZ_PRECISE_GC
  scheme_clean_cust_box_list();
# endif
# ifndef MZ_PRECISE_GC
  scheme_notify_code_gc();
# endif
}
#endif

/**************************************************************************/

static Scheme_Hash_Table *init_one_symbol_table()
{
  Scheme_Hash_Table *table;
  int size;
  Scheme_Object **ba;

  table = scheme_make_hash_table(SCHEME_hash_ptr);

  table->size = HASH_TABLE_INIT_SIZE;
  
  size = table->size * sizeof(Scheme_Object *);
#ifdef MZ_PRECISE_GC
  ba = (Scheme_Object **)GC_malloc_weak_array(size, SYMTAB_LOST_CELL);
#else
  ba = MALLOC_N_ATOMIC(Scheme_Object *, size);
  memset((char *)ba, 0, size);
#endif
  table->keys = ba;

  return table;
}

void
scheme_init_symbol_table ()
{
  REGISTER_SO(symbol_table);
  REGISTER_SO(keyword_table);
  REGISTER_SO(parallel_symbol_table);

  symbol_table = init_one_symbol_table();
  keyword_table = init_one_symbol_table();
  parallel_symbol_table = init_one_symbol_table();

#ifndef MZ_PRECISE_GC
  GC_custom_finalize = clean_symbol_table;
#endif
}

#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
void
scheme_init_place_local_symbol_table ()
{
  REGISTER_SO(place_local_symbol_table);
  REGISTER_SO(place_local_keyword_table);
  REGISTER_SO(place_local_parallel_symbol_table);

  place_local_symbol_table = init_one_symbol_table();
  place_local_keyword_table = init_one_symbol_table();
  place_local_parallel_symbol_table = init_one_symbol_table();
}
#endif

void
scheme_init_symbol_type (Scheme_Env *env)
{
}

void
scheme_init_symbol (Scheme_Env *env)
{
  Scheme_Object *p;

  p = scheme_make_folding_prim(symbol_p_prim, "symbol?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE);
  scheme_add_global_constant("symbol?", p, env);

  p = scheme_make_folding_prim(symbol_unreadable_p_prim, "symbol-unreadable?", 1, 1, 1);
  scheme_add_global_constant("symbol-unreadable?", p, env);
  
  p = scheme_make_folding_prim(symbol_interned_p_prim, "symbol-interned?", 1, 1, 1);
  scheme_add_global_constant("symbol-interned?", p, env);

  GLOBAL_FOLDING_PRIM("symbol<?",                 symbol_lt,                       2, -1, 1, env);  
  GLOBAL_IMMED_PRIM("string->symbol",             string_to_symbol_prim,            1, 1, env);
  GLOBAL_IMMED_PRIM("string->uninterned-symbol",  string_to_uninterned_symbol_prim, 1, 1, env);
  GLOBAL_IMMED_PRIM("string->unreadable-symbol",  string_to_unreadable_symbol_prim, 1, 1, env);
  GLOBAL_IMMED_PRIM("symbol->string",             symbol_to_string_prim,            1, 1, env);
  GLOBAL_FOLDING_PRIM("keyword?",                 keyword_p_prim,                   1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("keyword<?",                keyword_lt,                       2, -1, 1, env);
  GLOBAL_IMMED_PRIM("string->keyword",            string_to_keyword_prim,           1, 1, env);
  GLOBAL_IMMED_PRIM("keyword->string",            keyword_to_string_prim,           1, 1, env);
  GLOBAL_IMMED_PRIM("gensym",                     gensym,                           0, 1, env);
}

uintptr_t scheme_get_max_symbol_length() {
  /* x86, x86_64, and powerpc support aligned_atomic_loads_and_stores */
  return scheme_max_symbol_length;
}


static Scheme_Object *
make_a_symbol(const char *name, uintptr_t len, int kind)
{
  Scheme_Symbol *sym;

  sym = (Scheme_Symbol *)scheme_malloc_atomic_tagged(sizeof(Scheme_Symbol) + len + 1 - mzFLEX4_DELTA);

  sym->iso.so.type = scheme_symbol_type;
  MZ_OPT_HASH_KEY(&sym->iso) = kind;
  sym->len = len;
  memcpy(sym->s, name, len);
  sym->s[len] = 0;

#ifdef MZ_USE_PLACES
  mzrt_ensure_max_cas(&scheme_max_symbol_length, len);
#else
  if ( len > scheme_max_symbol_length ) {
    scheme_max_symbol_length = len;
  }
#endif

  return (Scheme_Object *) sym;
}

Scheme_Object *
scheme_make_symbol(const char *name)
{
  return make_a_symbol(name, strlen(name), 0x1);
}

Scheme_Object *
scheme_make_exact_symbol(const char *name, uintptr_t len)
{
  return make_a_symbol(name, len, 0x1);
}

Scheme_Object *
scheme_make_exact_char_symbol(const mzchar *name, uintptr_t len)
{
  char buf[64], *bs;
  intptr_t blen;
  bs = scheme_utf8_encode_to_buffer_len(name, len, buf, 64, &blen);
  return make_a_symbol(bs, blen, 0x1);
}

typedef enum {
  enum_symbol,
  enum_keyword,
  enum_parallel_symbol,
} enum_symbol_table_type;

static Scheme_Object *
intern_exact_symbol_in_table_worker(enum_symbol_table_type type, int kind, const char *name, uintptr_t len)
{
  Scheme_Object *sym;
  Scheme_Hash_Table *table;
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  Scheme_Hash_Table *place_local_table;
#endif

  sym = NULL;

  switch(type) {
    case enum_symbol:
      table = symbol_table;
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
      place_local_table = place_local_symbol_table;
#endif
      break;
    case enum_keyword:
      table = keyword_table;
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
      place_local_table = place_local_keyword_table;
#endif
      break;
    case enum_parallel_symbol:
      table = parallel_symbol_table;
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
      place_local_table = place_local_parallel_symbol_table;
#endif
      break;
    default:
      printf("Invalid enum_symbol_table_type %i\n", type);
      abort();
  }

#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  if (place_local_table) {
    sym = symbol_bucket(place_local_table, name, len, NULL);
  }
#endif
  if (!sym && table) {
    sym = symbol_bucket(table, name, len, NULL);
  }
  if (!sym) {
    /* create symbol in symbol table unless a place local symbol table has been created */
    /* once the first place has been create the symbol_table becomes read-only and
       shouldn't be modified */

    Scheme_Object *newsymbol;
    Scheme_Hash_Table *create_table;
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
    create_table = place_local_table ? place_local_table : table;
#else
    create_table = table;
#endif
    newsymbol = make_a_symbol(name, len, kind);
    
    /* we must return the result of this symbol bucket call because another
     * thread could have inserted the same symbol between the first
     * symbol_bucket call above and this one */
    sym = symbol_bucket(create_table, name, len, newsymbol);
  }

  return sym;
}

static Scheme_Object *
intern_exact_symbol_in_table(enum_symbol_table_type type, int kind, const char *name, uintptr_t len)
{
  return intern_exact_symbol_in_table_worker(type, kind, name, len);
}

Scheme_Object *
scheme_intern_exact_symbol(const char *name, uintptr_t len)
{
  return intern_exact_symbol_in_table(enum_symbol, 0, name, len);
}

Scheme_Object *
scheme_intern_exact_parallel_symbol(const char *name, uintptr_t len)
{
  return intern_exact_symbol_in_table(enum_parallel_symbol, 0x2, name, len);
}

Scheme_Object *
scheme_intern_exact_char_symbol(const mzchar *name, uintptr_t len)
{
  char buf[64], *bs;
  intptr_t blen;
  bs = scheme_utf8_encode_to_buffer_len(name, len, buf, 64, &blen);
  return intern_exact_symbol_in_table(enum_symbol, 0, bs, blen);
}

Scheme_Object *
scheme_intern_exact_keyword(const char *name, uintptr_t len)
{
  Scheme_Object *s;
  s = intern_exact_symbol_in_table(enum_keyword, 0, name, len);
  if (s->type == scheme_symbol_type)
    s->type = scheme_keyword_type;
  return s;
}

Scheme_Object *scheme_intern_exact_char_keyword(const mzchar *name, uintptr_t len)
{
  char buf[64], *bs;
  intptr_t blen;
  Scheme_Object *s;
  bs = scheme_utf8_encode_to_buffer_len(name, len, buf, 64, &blen);
  s = intern_exact_symbol_in_table(enum_keyword, 0, bs, blen);
  if (s->type == scheme_symbol_type)
    s->type = scheme_keyword_type;
  return s;
}

#define MAX_SYMBOL_SIZE 256

Scheme_Object *
scheme_intern_symbol(const char *name)
  /* `name' must be ASCII; this function is not suitable for non-ASCII
     conversion, necause it assumes that downcasing each C char
     is good enough to normalize the case. */
{
  if (!scheme_case_sensitive) {
    uintptr_t i, len;
    char *naya;
    char on_stack[MAX_SYMBOL_SIZE];

    len = strlen(name);
    if (len >= MAX_SYMBOL_SIZE)
      naya = (char *)scheme_malloc_atomic(len + 1);
    else
      naya = on_stack;

    for (i = 0; i < len; i++) {
      int c = ((unsigned char *)name)[i];

      c = scheme_tolower(c);

      naya[i] = c;
    }

    naya[len] = 0;

    return scheme_intern_exact_symbol(naya, len);
  }

  return scheme_intern_exact_symbol(name, strlen(name));
}

const char *scheme_symbol_name_and_size(Scheme_Object *sym, uintptr_t *length, int flags)
{
  int has_space = 0, has_special = 0, has_pipe = 0, has_upper = 0, digit_start;
  int dz;
  uintptr_t i, len = SCHEME_SYM_LEN(sym), total_length;
  int pipe_quote;
  char buf[100];
  char *s, *result;

  if ((flags & SCHEME_SNF_PIPE_QUOTE) || (flags & SCHEME_SNF_FOR_TS))
    pipe_quote = 1;
  else if (flags & SCHEME_SNF_NO_PIPE_QUOTE)
    pipe_quote = 0;
  else {
    pipe_quote = SCHEME_TRUEP(scheme_get_param(scheme_current_config(), MZCONFIG_CAN_READ_PIPE_QUOTE));
  }

  if (len < 100) {
    s = buf;
    memcpy(buf, SCHEME_SYM_VAL(sym), len + 1);
  } else
    s = scheme_symbol_val(sym);
  

#define isSpecial(ch) ((ch == '(') || (ch == '[') || (ch == '{')       \
		       || (ch == ')') || (ch == ']') || (ch == '}')    \
		       || (ch == ')') || (ch == '\\')   \
		       || (ch == '"') || (ch == '\'')   \
		       || (ch == '`') || (ch == ',')    \
                       || (ch == ';')                   \
                       || (((ch == '>') || (ch == '<')) \
			   && (flags & SCHEME_SNF_FOR_TS)))

  if (len) {
    if (flags & SCHEME_SNF_KEYWORD) {
      digit_start = 0;
    } else {
      int ch = ((unsigned char *)s)[0];
      digit_start = (((ch < 128) && isdigit(ch)) || (ch == '.')
		     || (ch == '+') || (ch == '-'));
      if (ch == '#' && (len == 1 || s[1] != '%'))
	has_special = 1;
      if (ch == '.' && len == 1)
	has_special = 1;
    }
  } else {
    digit_start = 0;
    if (!(flags & SCHEME_SNF_KEYWORD))
      has_space = 1;
  }

  for (i = 0; i < len; i++) {
    int ch = ((unsigned char *)s)[i];

    if (ch > 127) {
      /* Decode UTF-8. */
      mzchar buf[2];
      int ul = 2;
      while (1) {
        if (scheme_utf8_decode((unsigned char *)s, i, i + ul,
                               buf, 0, 1,
                               NULL, 0, 0) > 0)
          break;
        ul++;
      }
      ch = buf[0];
      if ((flags & SCHEME_SNF_NEED_CASE) && scheme_isspecialcasing(ch)) {
        mzchar *rc;
        buf[1] = 0;
        rc = scheme_string_recase(buf, 0, 1, 3, 1, NULL);
        if ((rc != buf) || (rc[0] != ch))
          has_upper = 1;
        ch = 'a';
      }
      i += (ul - 1);
    }

    if (scheme_isspace(ch)) { /* used to have || !isprint(ch) */
      if ((flags & SCHEME_SNF_FOR_TS) && (ch == ' ')) {
	/* space is OK in type symbols */
      } else
	has_space = 1;
    } else if (isSpecial(ch))
      has_special = 1;
    else if (ch == '|')
      has_pipe = 1;
    else if (flags & SCHEME_SNF_NEED_CASE) {
      if (scheme_tofold(ch) != ch)
	has_upper = 1;
    }
  }

  result = NULL;
  total_length = 0;

  if (!has_space && !has_special && (!pipe_quote || !has_pipe) && !has_upper) {
    mzchar cbuf[100], *cs;
    intptr_t clen;
    dz = 0;
    cs = scheme_utf8_decode_to_buffer_len((unsigned char *)s, len, cbuf, 100, &clen);
    if (cs
	&& digit_start
	&& !(flags & SCHEME_SNF_FOR_TS)
	&& (SCHEME_TRUEP(scheme_read_number(cs, clen, 0, 0, 1, 10, 0, NULL, &dz, 1, NULL, 0, 0, 0, 0, NULL))
	    || dz)) {
      /* Need quoting: */
      if (pipe_quote)
	has_space = 1; /* Use normal */
      else {
	/* Just need a leading backslash: */
	result = (char *)scheme_malloc_atomic(len + 2);
	total_length = len + 1;
	memcpy(result + 1, s, len);
	result[0] = '\\';
	result[len + 1] = 0;
      }
    } else {
      total_length = len;
      result = s;
    }
  }

  if (!result) {
    if (!has_pipe && pipe_quote) {
      result = (char *)scheme_malloc_atomic(len + 3);
      total_length = len + 2;
      memcpy(result + 1, s, len);
      result[0] = '|';
      result[len + 1] = '|';
      result[len + 2] = 0;
    } else {
      mzchar cbuf[100], *cs, *cresult;
      intptr_t clen;
      int p = 0;
      uintptr_t i = 0;
      intptr_t rlen;

      dz = 0;
      cs = scheme_utf8_decode_to_buffer_len((unsigned char *)s, len, cbuf, 100, &clen);

      cresult = (mzchar *)scheme_malloc_atomic(((2 * len) + 1) * sizeof(mzchar));

      for (i = 0; i < clen; i++) {
        mzchar ch = cs[i];
        if (scheme_isspace(ch)
	    || isSpecial(ch)
	    || ((ch == '|') && pipe_quote)
	    || (!i && s[0] == '#')
	    || (has_upper && (ch >= 'A') && (ch <= 'Z')))
	  cresult[p++] = '\\';
	cresult[p++] = ch;
      }

      result = scheme_utf8_encode_to_buffer_len(cresult, p, NULL, 0, &rlen);
      total_length = rlen;
    }
  }

  if (length)
    *length = total_length;

  return (result == buf) ? scheme_symbol_val (sym) : result;
}

const char *scheme_symbol_name(Scheme_Object *sym)
{
  return scheme_symbol_name_and_size(sym, NULL, 0);
}

char *scheme_symbol_val(Scheme_Object *sym)
{
  char *s;
  s = scheme_malloc_atomic(SCHEME_SYM_LEN(sym) + 1);
  memcpy(s, SCHEME_SYM_VAL(sym), SCHEME_SYM_LEN(sym) + 1);
  return s;
}

/* locals */

static Scheme_Object *
symbol_p_prim (int argc, Scheme_Object *argv[])
{
  return SCHEME_SYMBOLP(argv[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *
symbol_interned_p_prim (int argc, Scheme_Object *argv[])
{
  if (SCHEME_SYMBOLP(argv[0]))
    return (SCHEME_SYM_WEIRDP(argv[0]) ? scheme_false : scheme_true);
  
  scheme_wrong_contract("symbol-interned?", "symbol?", 0, argc, argv);
  return NULL;
}

static Scheme_Object *
symbol_unreadable_p_prim (int argc, Scheme_Object *argv[])
{
  if (SCHEME_SYMBOLP(argv[0]))
    return (SCHEME_SYM_PARALLELP(argv[0]) ? scheme_true : scheme_false);
  
  scheme_wrong_contract("symbol-unreadable?", "symbol?", 0, argc, argv);
  return NULL;
}

static Scheme_Object *
string_to_symbol_prim (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract("string->symbol", "string?", 0, argc, argv);
  return scheme_intern_exact_char_symbol(SCHEME_CHAR_STR_VAL(argv[0]),
					 SCHEME_CHAR_STRTAG_VAL(argv[0]));
}

static Scheme_Object *
string_to_uninterned_symbol_prim (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract("string->uninterned-symbol", "string?", 0, argc, argv);
  return scheme_make_exact_char_symbol(SCHEME_CHAR_STR_VAL(argv[0]),
				       SCHEME_CHAR_STRTAG_VAL(argv[0]));
}

static Scheme_Object *
string_to_unreadable_symbol_prim (int argc, Scheme_Object *argv[])
{
  char buf[64], *bs;
  intptr_t blen;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract("string->unreadable-symbol", "string?", 0, argc, argv);

  bs = scheme_utf8_encode_to_buffer_len(SCHEME_CHAR_STR_VAL(argv[0]),
                                        SCHEME_CHAR_STRTAG_VAL(argv[0]), 
                                        buf, 64, &blen);

  return scheme_intern_exact_parallel_symbol(bs, blen);
}

Scheme_Object *scheme_symbol_to_string(Scheme_Object *sym)
{
  Scheme_Object *str;
  GC_CAN_IGNORE unsigned char *s;
  GC_CAN_IGNORE mzchar *s2;
  intptr_t len, i;

  s = (unsigned char *)SCHEME_SYM_VAL(sym);
  len = SCHEME_SYM_LEN(sym);
  for (i = 0; i < len; i++) {
    if (s[i] >= 128)
      break;
  }
  s = NULL;

  if (i == len) {
    /* ASCII */
    str = scheme_alloc_char_string(len, 0);
    s = (unsigned char *)SCHEME_SYM_VAL(sym);
    s2 = SCHEME_CHAR_STR_VAL(str);
    for (i = 0; i < len; i++) {
      s2[i] = s[i];
    }
    return str;
  } else {
    return scheme_make_sized_offset_utf8_string((char *)sym,
                                                SCHEME_SYMSTR_OFFSET(sym),
                                                SCHEME_SYM_LEN(sym));
  }
}

static Scheme_Object *
symbol_to_string_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *sym;

  sym = argv[0];

  if (!SCHEME_SYMBOLP(sym))
    scheme_wrong_contract("symbol->string", "symbol?", 0, argc, argv);

  return scheme_symbol_to_string(sym);
}


static Scheme_Object *
keyword_p_prim (int argc, Scheme_Object *argv[])
{
  return SCHEME_KEYWORDP(argv[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *symkey_lt (const char *who, Scheme_Type ty, const char *contract,
                                  int argc, Scheme_Object *argv[])
{
  Scheme_Object *prev = argv[0], *kw;
  GC_CAN_IGNORE unsigned char *a, *b;
  int i, al, bl, t;

  if (!SAME_TYPE(SCHEME_TYPE(prev), ty))
    scheme_wrong_contract(who, contract, 0, argc, argv);

  for (i = 1; i < argc; i++) {
    kw = argv[i];
    if (!SAME_TYPE(SCHEME_TYPE(kw), ty))
      scheme_wrong_contract(who, contract, i, argc, argv);

    a = (unsigned char *)SCHEME_SYM_VAL(prev);
    al = SCHEME_SYM_LEN(prev);
    b = (unsigned char *)SCHEME_SYM_VAL(kw);
    bl = SCHEME_SYM_LEN(kw);
    t = ((al < bl) ? al : bl);
    while (t--) {
      if (*a < *b) {
        al = 0;
        bl = 1;
        break;
      } else if (*a > *b) {
        al = bl = 0;
        break;
      } else {
        a++;
        b++;
      }
    }
    a = b = NULL;

    if (al >= bl) {
      /* Check remaining types */
      for (i++; i < argc; i++) {
        if (!SAME_TYPE(SCHEME_TYPE(argv[i]), ty))
          scheme_wrong_contract(who, contract, i, argc, argv);
      }
      return scheme_false;
    }

    prev = kw;
  }

  return scheme_true;
}

static Scheme_Object *keyword_lt (int argc, Scheme_Object *argv[])
{
  return symkey_lt("keyword<?", scheme_keyword_type, "keyword?", argc, argv);
}

static Scheme_Object *symbol_lt (int argc, Scheme_Object *argv[])
{
  return symkey_lt("symbol<?", scheme_symbol_type, "symbol?", argc, argv);
}

static Scheme_Object *
string_to_keyword_prim (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract("string->keyword", "string?", 0, argc, argv);
  return scheme_intern_exact_char_keyword(SCHEME_CHAR_STR_VAL(argv[0]),
					  SCHEME_CHAR_STRTAG_VAL(argv[0]));
}

static Scheme_Object *
keyword_to_string_prim (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_KEYWORDP(argv[0]))
    scheme_wrong_contract("keyword->string", "keyword?", 0, argc, argv);
  
  return scheme_make_sized_offset_utf8_string((char *)(argv[0]),
					      SCHEME_SYMSTR_OFFSET(argv[0]),
					      SCHEME_SYM_LEN(argv[0]));
}

static Scheme_Object *gensym(int argc, Scheme_Object *argv[])
{
  char buffer[100], *str;
  Scheme_Object *r;

  if (argc)
    r = argv[0];
  else
    r = NULL;

  if (r && !SCHEME_SYMBOLP(r) && !SCHEME_CHAR_STRINGP(r))
    scheme_wrong_contract("gensym", "(or/c symbol? string?)", 0, argc, argv);

  if (r) {
    char buf[64];
    if (SCHEME_CHAR_STRINGP(r)) {
      str = scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(r),
					 SCHEME_CHAR_STRTAG_VAL(r),
					 buf, 64);
    } else
      str = SCHEME_SYM_VAL(r);
    sprintf(buffer, "%.80s%d", str, gensym_counter++);
    str = NULL; /* because it might be GC-misaligned */
  } else
    sprintf(buffer, "g%d", gensym_counter++);

  r = scheme_make_symbol(buffer);

  return r;
}

Scheme_Object *scheme_symbol_append(Scheme_Object *s1, Scheme_Object *s2)
{
  char *s;
  s = MALLOC_N_ATOMIC(char, SCHEME_SYM_LEN(s1) + SCHEME_SYM_LEN(s2) + 1);
  memcpy(s, SCHEME_SYM_VAL(s1), SCHEME_SYM_LEN(s1));
  memcpy(s + SCHEME_SYM_LEN(s1), SCHEME_SYM_VAL(s2), SCHEME_SYM_LEN(s2) + 1);
  if (SCHEME_SYM_UNINTERNEDP(s1) || SCHEME_SYM_UNINTERNEDP(s2))
    return scheme_make_exact_symbol(s, SCHEME_SYM_LEN(s1) + SCHEME_SYM_LEN(s2));
  else if (SCHEME_SYM_PARALLELP(s1) || SCHEME_SYM_PARALLELP(s2))
    return scheme_intern_exact_parallel_symbol(s, SCHEME_SYM_LEN(s1) + SCHEME_SYM_LEN(s2));
  else
    return scheme_intern_exact_symbol(s, SCHEME_SYM_LEN(s1) + SCHEME_SYM_LEN(s2));
}
