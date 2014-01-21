/*
  Racket
  Copyright (c) 2004-2014 PLT Design Inc.
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

#include "schpriv.h"
#include <ctype.h>

/* globals */
#include "schuchar.inc"
READ_ONLY Scheme_Object **scheme_char_constants;
READ_ONLY static Scheme_Object *general_category_symbols[NUM_GENERAL_CATEGORIES];

/* locals */
static Scheme_Object *char_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_gt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_eq_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lt_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_gt_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lt_eq_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_gt_eq_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_alphabetic (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_numeric (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_whitespace (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_symbolic (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_graphic (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_blank (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_control (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_punctuation (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_upper_case (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lower_case (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_title_case (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_upcase (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_downcase (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_titlecase (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_foldcase (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_general_category (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_utf8_length (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_map_list (int argc, Scheme_Object *argv[]);


void scheme_init_portable_case(void)
{
  init_uchar_table();
}

void scheme_init_char (Scheme_Env *env)
{
  Scheme_Object *p;
  int i;

  REGISTER_SO(scheme_char_constants);
  REGISTER_SO(general_category_symbols);

  scheme_char_constants = 
    (Scheme_Object **)scheme_malloc_eternal(256 * sizeof(Scheme_Object*));
    
  for (i = 0; i < 256; i++) {
    Scheme_Object *sc;
    sc = scheme_alloc_eternal_small_object();
    sc->type = scheme_char_type;
    SCHEME_CHAR_VAL(sc) = i;
    
    scheme_char_constants[i] = sc;
  }
  
  for (i = 0; i < NUM_GENERAL_CATEGORIES; i++) {
    Scheme_Object *s;
    s = scheme_intern_symbol(general_category_names[i]);
    general_category_symbols[i] = s;
  }

  p = scheme_make_folding_prim(char_p, "char?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE);
  scheme_add_global_constant("char?", p, env);

  p = scheme_make_folding_prim(char_eq, "char=?", 2, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED);
  scheme_add_global_constant("char=?", p, env);

  GLOBAL_FOLDING_PRIM("char<?",                char_lt,               2, -1, 1, env);
  GLOBAL_FOLDING_PRIM("char>?",                char_gt,               2, -1, 1, env);
  GLOBAL_FOLDING_PRIM("char<=?",               char_lt_eq,            2, -1, 1, env);
  GLOBAL_FOLDING_PRIM("char>=?",               char_gt_eq,            2, -1, 1, env);
  GLOBAL_FOLDING_PRIM("char-ci=?",             char_eq_ci,            2, -1, 1, env);
  GLOBAL_FOLDING_PRIM("char-ci<?",             char_lt_ci,            2, -1, 1, env);
  GLOBAL_FOLDING_PRIM("char-ci>?",             char_gt_ci,            2, -1, 1, env);
  GLOBAL_FOLDING_PRIM("char-ci<=?",            char_lt_eq_ci,         2, -1, 1, env);
  GLOBAL_FOLDING_PRIM("char-ci>=?",            char_gt_eq_ci,         2, -1, 1, env);
  GLOBAL_FOLDING_PRIM("char-alphabetic?",      char_alphabetic,       1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("char-numeric?",         char_numeric,          1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("char-symbolic?",        char_symbolic,         1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("char-graphic?",         char_graphic,          1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("char-whitespace?",      char_whitespace,       1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("char-blank?",           char_blank,            1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("char-iso-control?",     char_control,          1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("char-punctuation?",     char_punctuation,      1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("char-upper-case?",      char_upper_case,       1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("char-title-case?",      char_title_case,       1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("char-lower-case?",      char_lower_case,       1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("char-title-case?",      char_title_case,       1, 1, 1, env);

  p = scheme_make_folding_prim(scheme_checked_char_to_integer, "char->integer", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_add_global_constant("char->integer", p, env);
  p = scheme_make_folding_prim(scheme_checked_integer_to_char, "integer->char", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_add_global_constant("integer->char", p, env);

  GLOBAL_FOLDING_PRIM("char-upcase",           char_upcase,           1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("char-downcase",         char_downcase,         1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("char-titlecase",        char_titlecase,        1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("char-foldcase",         char_foldcase,         1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("char-general-category", char_general_category, 1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("char-utf-8-length",     char_utf8_length,      1, 1, 1, env);
  GLOBAL_IMMED_PRIM("make-known-char-range-list", char_map_list, 0, 0, env);
}

Scheme_Object *scheme_make_char(mzchar ch)
{
  Scheme_Object *o;

  if (ch < 256)
    return scheme_char_constants[ch];
  
  o = scheme_malloc_small_atomic_tagged(sizeof(Scheme_Small_Object));
  CLEAR_KEY_FIELD(o);
  o->type = scheme_char_type;
  SCHEME_CHAR_VAL(o) = ch;

  return o;
}

Scheme_Object *scheme_make_char_or_nul(mzchar v)
{
  if ((v <= 0x10FFFF)
      && ((v < 0xD800) || (v > 0xDFFF)))
    return scheme_make_char(v);

  return scheme_char_constants[0];
}

/* locals */

static Scheme_Object *
char_p (int argc, Scheme_Object *argv[])
{
  return (SCHEME_CHARP(argv[0]) ? scheme_true : scheme_false);
}

#define charSTD_FOLDCASE(nl) nl;
#define charNO_FOLDCASE(nl) /* empty */

#define GEN_CHAR_COMP(func_name, scheme_name, comp, FOLDCASE) \
 static Scheme_Object *func_name(int argc, Scheme_Object *argv[])     \
 { int c, prev, i; Scheme_Object *rv = scheme_true; \
   if (!SCHEME_CHARP(argv[0]))      \
     scheme_wrong_contract(#scheme_name, "char?", 0, argc, argv);     \
   prev = SCHEME_CHAR_VAL(argv[0]);     \
   FOLDCASE(prev = scheme_tofold(prev)) \
   for (i = 1; i < argc; i++) {     \
     if (!SCHEME_CHARP(argv[i]))      \
       scheme_wrong_contract(#scheme_name, "char?", i, argc, argv);     \
     c = SCHEME_CHAR_VAL(argv[i]);     \
     FOLDCASE(c = scheme_tofold(c)) \
     if (!(prev comp c)) rv = scheme_false;   \
     prev = c;     \
   }     \
   return rv;     \
 }

GEN_CHAR_COMP(char_eq, char=?, ==, charNO_FOLDCASE)
GEN_CHAR_COMP(char_lt, char<?, <, charNO_FOLDCASE)
GEN_CHAR_COMP(char_gt, char>?, >, charNO_FOLDCASE)
GEN_CHAR_COMP(char_lt_eq, char<=?, <=, charNO_FOLDCASE)
GEN_CHAR_COMP(char_gt_eq, char>=?, >=, charNO_FOLDCASE)

GEN_CHAR_COMP(char_eq_ci, char-ci=?, ==, charSTD_FOLDCASE)
GEN_CHAR_COMP(char_lt_ci, char-ci<?, <, charSTD_FOLDCASE)
GEN_CHAR_COMP(char_gt_ci, char-ci>?, >, charSTD_FOLDCASE)
GEN_CHAR_COMP(char_lt_eq_ci, char-ci<=?, <=, charSTD_FOLDCASE)
GEN_CHAR_COMP(char_gt_eq_ci, char-ci>=?, >=, charSTD_FOLDCASE)

#define GEN_CHAR_TEST(func_name, scheme_name, pred) \
static Scheme_Object *func_name (int argc, Scheme_Object *argv[]) \
{ \
  mzchar c;    \
  if (!SCHEME_CHARP(argv[0]))  \
    scheme_wrong_contract(scheme_name, "char?", 0, argc, argv); \
  c = SCHEME_CHAR_VAL(argv[0]);                    \
  return (pred(c) ? scheme_true : scheme_false);   \
}
     
GEN_CHAR_TEST(char_numeric, "char-numeric?", scheme_isdigit)
GEN_CHAR_TEST(char_alphabetic, "char-alphabetic?", scheme_isalpha)
GEN_CHAR_TEST(char_whitespace, "char-whitespace?", scheme_isspace)
GEN_CHAR_TEST(char_blank, "char-blank?", scheme_isblank)
GEN_CHAR_TEST(char_control, "char-iso-control?", scheme_iscontrol)
GEN_CHAR_TEST(char_punctuation, "char-punctuation?", scheme_ispunc)
GEN_CHAR_TEST(char_symbolic, "char-symbolic?", scheme_issymbol)
GEN_CHAR_TEST(char_graphic, "char-graphic?", scheme_isgraphic)
GEN_CHAR_TEST(char_upper_case, "char-upper-case?", scheme_isupper)
GEN_CHAR_TEST(char_lower_case, "char-lower-case?", scheme_islower)
GEN_CHAR_TEST(char_title_case, "char-title-case?", scheme_istitle)

Scheme_Object *
scheme_checked_char_to_integer (int argc, Scheme_Object *argv[])
{
  mzchar c;

  if (!SCHEME_CHARP(argv[0]))
    scheme_wrong_contract("char->integer", "char?", 0, argc, argv);

  c = SCHEME_CHAR_VAL(argv[0]);

  return scheme_make_integer_value(c);
}

Scheme_Object *
scheme_checked_integer_to_char (int argc, Scheme_Object *argv[])
{
  if (SCHEME_INTP(argv[0])) {
    intptr_t v;
    v = SCHEME_INT_VAL(argv[0]);
    if ((v >= 0) 
	&& (v <= 0x10FFFF)
	&& ((v < 0xD800) || (v > 0xDFFF)))
      return _scheme_make_char((int)v);
  } else if (SCHEME_BIGNUMP(argv[0])
	     && SCHEME_BIGPOS(argv[0])) {
    /* On 32-bit machines, there's still a chance... */
    intptr_t y;
    if (scheme_get_int_val(argv[0], &y)) {
      if (y <= 0x10FFFF)
	return _scheme_make_char((int)y);
    }
  }

  scheme_wrong_contract("integer->char", 
                        "(and/c (integer-in 0 #x10FFFF) (not/c (integer-in #xD800 #xDFFF)))", 
                        0, argc, argv);
  return NULL;
}

#define GEN_RECASE(func_name, scheme_name, cvt) \
static Scheme_Object *func_name (int argc, Scheme_Object *argv[]) \
{ \
  mzchar c, nc;    \
  if (!SCHEME_CHARP(argv[0]))  \
    scheme_wrong_contract(scheme_name, "char?", 0, argc, argv); \
  c = SCHEME_CHAR_VAL(argv[0]);                    \
  nc = cvt(c);                                      \
  if (nc == c) return argv[0];       \
  return scheme_make_character(nc);  \
}

GEN_RECASE(char_upcase, "char-upcase", scheme_toupper)
GEN_RECASE(char_downcase, "char-downcase", scheme_tolower)
GEN_RECASE(char_titlecase, "char-titlecase", scheme_totitle)
GEN_RECASE(char_foldcase, "char-foldcase", scheme_tofold)

static Scheme_Object *char_general_category (int argc, Scheme_Object *argv[])
{
  mzchar c;
  int cat;

  if (!SCHEME_CHARP(argv[0]))
    scheme_wrong_contract("char-general-category", "char?", 0, argc, argv);

  c = SCHEME_CHAR_VAL(argv[0]);
  cat = scheme_general_category(c);

  return general_category_symbols[cat];
}

static Scheme_Object *char_utf8_length (int argc, Scheme_Object *argv[])
{
  mzchar wc;
  if (!SCHEME_CHARP(argv[0]))
    scheme_wrong_contract("char-utf-8-length", "char?", 0, argc, argv);

  wc = SCHEME_CHAR_VAL(argv[0]);
  if (wc < 0x80) {
    return scheme_make_integer(1);
  } else if (wc < 0x800) {
    return scheme_make_integer(2);
  } else if (wc < 0x10000) {
    return scheme_make_integer(3);
  } else if (wc < 0x200000) {
    return scheme_make_integer(4);
  } else if (wc < 0x4000000) {
    return scheme_make_integer(5);
  } else {
    return scheme_make_integer(6);
  }
}

static Scheme_Object *char_map_list (int argc, Scheme_Object *argv[])
{
  int i, bottom, top, uniform;
  Scheme_Object *l = scheme_null;

# define cons scheme_make_pair

  for (i = 2 * (NUM_UCHAR_RANGES - 1); i >= 0; i -= 2) {
    bottom = mapped_uchar_ranges[i];
    top = mapped_uchar_ranges[i + 1];
    if (top & URANGE_VARIES) {
      top -= URANGE_VARIES;
      uniform = 0;
    } else
      uniform = 1;
    l = cons(cons(scheme_make_integer_value(bottom),
		    cons(scheme_make_integer_value(top),
			  cons((uniform ? scheme_true : scheme_false),
				scheme_null))),
	      l);
  }

  return l;
}
