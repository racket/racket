/*
  MzScheme
  Copyright (c) 2004-2005 PLT Scheme, Inc.
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
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

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

# define HASH_TABLE_SIZE_STEP 4
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

Scheme_Hash_Table *scheme_symbol_table = NULL;
Scheme_Hash_Table *scheme_parallel_symbol_table = NULL;

unsigned long scheme_max_found_symbol_name;

/* globals */
int scheme_case_sensitive = 1;

/* locals */
static Scheme_Object *symbol_p_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_to_symbol_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_to_uninterned_symbol_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *symbol_to_string_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *gensym(int argc, Scheme_Object *argv[]);

static int gensym_counter;

/**************************************************************************/

typedef unsigned long hash_v_t;
#define HASH_SEED  0xF0E1D2C3

extern long scheme_hash_primes[];

#define SYMTAB_LOST_CELL scheme_false

#ifdef MZ_PRECISE_GC
# define WEAK_ARRAY_HEADSIZE 4
#else
# define WEAK_ARRAY_HEADSIZE 0
#endif

/* Special hashing for symbols: */
static Scheme_Object *symbol_bucket(Scheme_Hash_Table *table,
				    GC_CAN_IGNORE const char *key, unsigned int length,
				    Scheme_Object *naya)
{
  hash_v_t h, h2;
  Scheme_Object *bucket;

  /* WARNING: key may be GC-misaligned... */

 rehash_key:
  {
    unsigned int i;
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

    h = h % table->size;
    h2 = h2 % table->size;
  }

  if (!h2)
    h2 = 2;
  else if (h2 & 0x1)
    h2++; /* note: table size is never even, so no % needed */

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
    h = (h + h2) % table->size;
  }

  /* In case it's GC-misaligned: */
  key = NULL;

  if (!naya)
    return NULL;

  if (table->count * FILL_FACTOR >= table->size) {
    /* Rehash */
    int i, oldsize = table->size, newsize, lostc;
    size_t asize;
    Scheme_Object *cb;
    Scheme_Object **old = table->keys;

    /* Don't grow table if it's mostly lost cells (due to lots of
       temporary symbols). */
    lostc = 0;
    for (i = 0; i < oldsize; i++) {
      cb = old[WEAK_ARRAY_HEADSIZE + i];
      if (cb == SYMTAB_LOST_CELL)
	lostc++;
    }
    if ((lostc * 2) < table->count)
      table->step++;

    newsize = scheme_hash_primes[table->step];

    asize = (size_t)newsize * sizeof(Scheme_Object *);
    {
      Scheme_Object **ba;
#ifdef MZ_PRECISE_GC
      ba = (Scheme_Object **)GC_malloc_weak_array(sizeof(Scheme_Object *) * newsize,
						  SYMTAB_LOST_CELL);
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

    goto rehash_key;
  }

  table->keys[WEAK_ARRAY_HEADSIZE + h] = naya;

  table->count++;

  return naya;
}

#ifndef MZ_PRECISE_GC
static void clean_one_symbol_table(Scheme_Hash_Table *symbol_table)
{
  /* Clean the symbol table by removing pointers to collected
     symbols. The correct way to do this is to install a GC
     finalizer on symbol pointers, but that would be expensive. */

  if (symbol_table) {
    Scheme_Object **buckets = (Scheme_Object **)symbol_table->keys;
    int i = symbol_table->size;
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
  clean_one_symbol_table(scheme_symbol_table);
  clean_one_symbol_table(scheme_parallel_symbol_table);
}
#endif

/**************************************************************************/

static Scheme_Hash_Table *init_one_symbol_table()
{
  Scheme_Hash_Table *symbol_table;
  int size;
  Scheme_Object **ba;

  symbol_table = scheme_make_hash_table(SCHEME_hash_ptr);

  symbol_table->step = HASH_TABLE_SIZE_STEP;
  symbol_table->size = scheme_hash_primes[symbol_table->step];
  
  size = symbol_table->size * sizeof(Scheme_Object *);
#ifdef MZ_PRECISE_GC
  ba = (Scheme_Object **)GC_malloc_weak_array(size, SYMTAB_LOST_CELL);
#else
  ba = MALLOC_N_ATOMIC(Scheme_Object *, size);
  memset((char *)ba, 0, size);
#endif
  symbol_table->keys = ba;

  return symbol_table;
}

void
scheme_init_symbol_table ()
{
  REGISTER_SO(scheme_symbol_table);
  REGISTER_SO(scheme_parallel_symbol_table);

  scheme_symbol_table = init_one_symbol_table();
  scheme_parallel_symbol_table = init_one_symbol_table();

#ifndef MZ_PRECISE_GC
  GC_custom_finalize = clean_symbol_table;
#endif
}

void
scheme_init_symbol_type (Scheme_Env *env)
{
}

void
scheme_init_symbol (Scheme_Env *env)
{
  scheme_add_global_constant("symbol?",
			     scheme_make_folding_prim(symbol_p_prim,
						      "symbol?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("string->symbol",
			     scheme_make_prim_w_arity(string_to_symbol_prim,
						      "string->symbol",
						      1, 1), env);
  scheme_add_global_constant("string->uninterned-symbol",
			     scheme_make_prim_w_arity(string_to_uninterned_symbol_prim,
						      "string->uninterned-symbol",
						      1, 1),
			     env);
  scheme_add_global_constant("symbol->string",
			     scheme_make_prim_w_arity(symbol_to_string_prim,
						      "symbol->string",
						      1, 1),
			     env);

  scheme_add_global_constant("gensym",
			     scheme_make_prim_w_arity(gensym,
						      "gensym",
						      0, 1),
			     env);
}

static Scheme_Object *
make_a_symbol(const char *name, unsigned int len, int kind)
{
  Scheme_Symbol *sym;

  sym = (Scheme_Symbol *)scheme_malloc_atomic_tagged(sizeof(Scheme_Symbol) + len - 3);

  sym->iso.so.type = scheme_symbol_type;
  MZ_OPT_HASH_KEY(&sym->iso) = kind;
  sym->len = len;
  memcpy(sym->s, name, len);
  sym->s[len] = 0;

  if (len > scheme_max_found_symbol_name) {
    scheme_max_found_symbol_name = len;
    scheme_reset_prepared_error_buffer();
  }

  return (Scheme_Object *) sym;
}

Scheme_Object *
scheme_make_symbol(const char *name)
{
  return make_a_symbol(name, strlen(name), 0x1);
}

Scheme_Object *
scheme_make_exact_symbol(const char *name, unsigned int len)
{
  return make_a_symbol(name, len, 0x1);
}

Scheme_Object *
scheme_make_exact_char_symbol(const mzchar *name, unsigned int len)
{
  char buf[64], *bs;
  long blen;
  bs = scheme_utf8_encode_to_buffer_len(name, len, buf, 64, &blen);
  return make_a_symbol(bs, blen, 0x1);
}

Scheme_Object *
scheme_intern_exact_symbol_in_table(Scheme_Hash_Table *symbol_table, int kind, const char *name, unsigned int len)
{
  Scheme_Object *sym;

  sym = symbol_bucket(symbol_table, name, len, NULL);

  if (!sym) {
    sym = make_a_symbol(name, len, kind);
    symbol_bucket(symbol_table, name, len, sym);
  }

  return sym;
}

Scheme_Object *
scheme_intern_exact_symbol(const char *name, unsigned int len)
{
  return scheme_intern_exact_symbol_in_table(scheme_symbol_table, 0, name, len);
}

Scheme_Object *
scheme_intern_exact_parallel_symbol(const char *name, unsigned int len)
{
  return scheme_intern_exact_symbol_in_table(scheme_parallel_symbol_table, 0x2, name, len);
}

Scheme_Object *
scheme_intern_exact_char_symbol(const mzchar *name, unsigned int len)
{
  char buf[64], *bs;
  long blen;
  bs = scheme_utf8_encode_to_buffer_len(name, len, buf, 64, &blen);
  return scheme_intern_exact_symbol_in_table(scheme_symbol_table, 0, bs, blen);
}

#define MAX_SYMBOL_SIZE 256

Scheme_Object *
scheme_intern_symbol(const char *name)
{
  if (!scheme_case_sensitive) {
      unsigned long i, len;
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

const char *scheme_symbol_name_and_size(Scheme_Object *sym, unsigned int *length, int flags)
{
  int has_space = 0, has_special = 0, has_pipe = 0, has_upper = 0, digit_start;
  int dz;
  unsigned int i, len = SCHEME_SYM_LEN(sym), total_length;
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
    digit_start = (isdigit((unsigned char)s[0]) || (s[0] == '.')
		   || (s[0] == '+') || (s[0] == '-'));
    if (s[0] == '#' && (len == 1 || s[1] != '%'))
      has_special = 1;
    if (s[0] == '.' && len == 1)
      has_special = 1;
  } else {
    digit_start = 0;
    has_space = 1;
  }

  for (i = 0; i < len; i++) {
    if (isspace((unsigned char)s[i])) { /* used to have || !isprint((unsigned char)s[i]) */
      if ((flags & SCHEME_SNF_FOR_TS) && (s[i] == ' ')) {
	/* space is OK in type symbols */
      } else
	has_space = 1;
    } else if (isSpecial(s[i]))
      has_special = 1;
    else if (s[i] == '|')
      has_pipe = 1;
    else if ((((unsigned char)s[i]) >= 'A')
	     && (((unsigned char)s[i]) <= 'Z'))
      has_upper = 1;
  }

  if (!(flags & SCHEME_SNF_NEED_CASE))
    has_upper = 0;

  result = NULL;
  total_length = 0;

  if (!has_space && !has_special && (!pipe_quote || !has_pipe) && !has_upper) {
    mzchar cbuf[100], *cs;
    long clen;
    dz = 0;
    cs = scheme_utf8_decode_to_buffer_len(s, len, cbuf, 100, &clen);
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
      int p = 0;
      unsigned int i = 0;

      result = (char *)scheme_malloc_atomic((2 * len) + 1);

      for (i = 0; i < len; i++) {
	if (isspace((unsigned char)s[i])
	    || isSpecial(s[i])
	    || ((s[i] == '|') && pipe_quote)
	    || (!i && s[0] == '#')
	    || (has_upper && ((((unsigned char)s[i]) >= 'A')
			      && (((unsigned char)s[i]) <= 'Z'))))
	  result[p++] = '\\';
	result[p++] = s[i];
      }

      result[p] = 0;
      total_length = p;
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
string_to_symbol_prim (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("string->symbol", "string", 0, argc, argv);
  return scheme_intern_exact_char_symbol(SCHEME_CHAR_STR_VAL(argv[0]),
					 SCHEME_CHAR_STRTAG_VAL(argv[0]));
}

static Scheme_Object *
string_to_uninterned_symbol_prim (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("string->uninterned-symbol", "string", 0, argc, argv);
  return scheme_make_exact_char_symbol(SCHEME_CHAR_STR_VAL(argv[0]),
				       SCHEME_CHAR_STRTAG_VAL(argv[0]));
}

static Scheme_Object *
symbol_to_string_prim (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("symbol->string", "symbol", 0, argc, argv);

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
    scheme_wrong_type("gensym", "symbol or string", 0, argc, argv);

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
