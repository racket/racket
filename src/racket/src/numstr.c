/*
  Racket
  Copyright (c) 2004-2012 PLT Scheme Inc.
  Copyright (c) 2000-2001 Matthew Flatt

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

/* The bulk of this file is the number parser, an insane bit of code
   that would probably be better off implemented via lex+yacc, except
   the error messages are better this way.

   Also, for no particularly good reason, random-number support is
   here, though the real work is in newrandom.inc. */

#include "schpriv.h"
#include <math.h>
#include <string.h>
#include <ctype.h>

static Scheme_Object *number_to_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_to_number (int argc, Scheme_Object *argv[]);

static Scheme_Object *bytes_to_integer (int argc, Scheme_Object *argv[]);
static Scheme_Object *integer_to_bytes (int argc, Scheme_Object *argv[]);
static Scheme_Object *bytes_to_real (int argc, Scheme_Object *argv[]);
static Scheme_Object *real_to_bytes (int argc, Scheme_Object *argv[]);
static Scheme_Object *system_big_endian_p (int argc, Scheme_Object *argv[]);

static Scheme_Object *random_seed(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_random(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_pseudo_random_generator(int argc, Scheme_Object **argv);
static Scheme_Object *current_pseudo_random_generator(int argc, Scheme_Object **argv);
static Scheme_Object *current_sched_pseudo_random_generator(int argc, Scheme_Object **argv);
static Scheme_Object *pseudo_random_generator_p(int argc, Scheme_Object **argv);
static Scheme_Object *sch_unpack(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_pack(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_pack_bang(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_check_pack(int argc, Scheme_Object *argv[]);

static char *number_to_allocated_string(int radix, Scheme_Object *obj, int alloc);

READ_ONLY static char *infinity_str = "+inf.0";
READ_ONLY static char *minus_infinity_str = "-inf.0";
READ_ONLY static char *not_a_number_str = "+nan.0";
READ_ONLY static char *other_not_a_number_str = "-nan.0";
/* Single-precision float literals.
   Due to the structure of the reader, they have to be exactly 6
   characters long. */
READ_ONLY static char *single_infinity_str = "+inf.f";
READ_ONLY static char *single_minus_infinity_str = "-inf.f";
READ_ONLY static char *single_not_a_number_str = "+nan.f";
READ_ONLY static char *single_other_not_a_number_str = "-nan.f";

#if !defined(SIXTY_FOUR_BIT_INTEGERS) && defined(NO_LONG_LONG_TYPE)
SHARED_OK static Scheme_Object *num_limits[3];
#endif

#ifdef SCHEME_BIG_ENDIAN
# define MZ_IS_BIG_ENDIAN 1
#else
# define MZ_IS_BIG_ENDIAN 0
#endif

#define TO_DOUBLE scheme_TO_DOUBLE

#define zeroi scheme_exact_zero

void scheme_init_numstr(Scheme_Env *env)
{
  scheme_add_global_constant("number->string", 
			     scheme_make_prim_w_arity(number_to_string,
						      "number->string",
						      1, 2),
			     env);
  scheme_add_global_constant("string->number", 
			     scheme_make_folding_prim(string_to_number,
						      "string->number", 
						      1, 2, 1),
			     env);

  scheme_add_global_constant("integer-bytes->integer", 
			     scheme_make_prim_w_arity(bytes_to_integer,
						      "integer-bytes->integer", 
						      2, 5),
			     env);
  scheme_add_global_constant("integer->integer-bytes", 
			     scheme_make_prim_w_arity(integer_to_bytes,
						      "integer->integer-bytes", 
						      3, 6),
			     env);
  scheme_add_global_constant("floating-point-bytes->real", 
			     scheme_make_prim_w_arity(bytes_to_real,
						      "floating-point-bytes->real",
						      1, 4),
			     env);
  scheme_add_global_constant("real->floating-point-bytes",
			     scheme_make_prim_w_arity(real_to_bytes,
						      "real->floating-point-bytes",
						      2, 5),
			     env);
  scheme_add_global_constant("system-big-endian?",
			     scheme_make_prim_w_arity(system_big_endian_p,
						      "system-big-endian?",
						      0, 0),
			     env);

  scheme_add_global_constant("random", 
			     scheme_make_prim_w_arity(sch_random,
						      "random",
						      0, 2),
			     env);
  scheme_add_global_constant("random-seed", 
			     scheme_make_prim_w_arity(random_seed,
						      "random-seed",
						      1, 1),
			     env);
  scheme_add_global_constant("make-pseudo-random-generator", 
			     scheme_make_prim_w_arity(make_pseudo_random_generator,
						      "make-pseudo-random-generator", 
						      0, 0), 
			     env);
  scheme_add_global_constant("vector->pseudo-random-generator",
			     scheme_make_prim_w_arity(sch_pack,
						      "vector->pseudo-random-generator", 
						      1, 1), 
			     env);
  scheme_add_global_constant("vector->pseudo-random-generator!",
			     scheme_make_prim_w_arity(sch_pack_bang,
						      "vector->pseudo-random-generator!", 
						      2, 2), 
			     env);
  scheme_add_global_constant("pseudo-random-generator->vector",
			     scheme_make_prim_w_arity(sch_unpack,
						      "pseudo-random-generator->vector", 
						      1, 1), 
			     env);
  scheme_add_global_constant("pseudo-random-generator-vector?",
                             scheme_make_prim_w_arity(sch_check_pack,
						      "pseudo-random-generator-vector?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("pseudo-random-generator?", 
			     scheme_make_prim_w_arity(pseudo_random_generator_p,
						      "pseudo-random-generator?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("current-pseudo-random-generator", 
			     scheme_register_parameter(current_pseudo_random_generator,
						       "current-pseudo-random-generator",
						       MZCONFIG_RANDOM_STATE),
			     env);
  scheme_add_global_constant("current-evt-pseudo-random-generator", 
			     scheme_register_parameter(current_sched_pseudo_random_generator,
						       "current-evt-pseudo-random-generator",
						       MZCONFIG_SCHEDULER_RANDOM_STATE),
			     env);

#if !defined(SIXTY_FOUR_BIT_INTEGERS) && defined(NO_LONG_LONG_TYPE)
  REGISTER_SO(num_limits);
  {
    Scheme_Object *a[2], *v;

    a[0] = scheme_make_integer(1);
    a[1] = scheme_make_integer(64);
    a[0] = scheme_bitwise_shift(2, a);
    v = scheme_sub1(1, a);
    num_limits[MZ_U8HI] = v;
    a[0] = v;
    a[1] = scheme_make_integer(-1);
    v = scheme_bitwise_shift(2, a);
    num_limits[MZ_S8HI] = v;
    a[0] = v;
    v = scheme_bin_minus(scheme_make_integer(0), scheme_add1(1, a));
    num_limits[MZ_S8LO] = v;
  }
#endif
}

# ifdef SIN_COS_NEED_DEOPTIMIZE
#  pragma optimize("g", off)
#  define MK_SCH_TRIG(SCH_TRIG, c_trig) static double SCH_TRIG(double d) { return c_trig(d); }
MK_SCH_TRIG(SCH_SIN, sin)
MK_SCH_TRIG(SCH_COS, cos)
#  pragma optimize("g", on)
# else
#  define SCH_SIN sin
#  define SCH_COS cos
# endif

/*========================================================================*/
/*                           number parsing                               */
/*========================================================================*/

static int u_strcmp(mzchar *s, const char *t)
{
  int i;

  for (i = 0; s[i] && (s[i] == ((unsigned char *)t)[i]); i++) {
  }
  if (s[i] || t[i])
    return 1;
  return 0;
}

static Scheme_Object *read_special_number(const mzchar *str, int pos)
{
  if ((str[pos] == '-' || str[pos] == '+') && scheme_isalpha(str[pos + 1])) {
    mzchar s[7];
    int i;

    for (i = 0; i < 6; i++) {
      s[i] = scheme_tolower(str[i + pos]);
    }
    s[i] = 0;

    if (!u_strcmp(s, infinity_str)) {
#ifdef USE_SINGLE_FLOATS_AS_DEFAULT
      return scheme_single_inf_object;
#else
      return scheme_inf_object;
#endif
    }
    else if (!u_strcmp(s, minus_infinity_str)) {
#ifdef USE_SINGLE_FLOATS_AS_DEFAULT
      return scheme_single_minus_inf_object;
#else
      return scheme_minus_inf_object;
#endif
    }
    else if (!u_strcmp(s, not_a_number_str)
	     || !u_strcmp(s, other_not_a_number_str)) {
#ifdef USE_SINGLE_FLOATS_AS_DEFAULT
      return scheme_single_nan_object;
#else      
      return scheme_nan_object;
#endif
    }
    /* Single-precision specials
       If single-precision float support is disabled, promote. */
    else if (!u_strcmp(s, single_infinity_str)) {
#ifdef MZ_USE_SINGLE_FLOATS
      return scheme_single_inf_object;
#else
      return scheme_inf_object;
#endif
    }
    else if (!u_strcmp(s, single_minus_infinity_str)) {
#ifdef MZ_USE_SINGLE_FLOATS
      return scheme_single_minus_inf_object;
#else
      return scheme_minus_inf_object;
#endif
    }
    else if (!u_strcmp(s, single_not_a_number_str)
	     || !u_strcmp(s, single_other_not_a_number_str)) {
#ifdef MZ_USE_SINGLE_FLOATS
      return scheme_single_nan_object;
#else
      return scheme_nan_object;
#endif
    }
  }

  return NULL;
}

/* Exponent threshold for obvious infinity. Must be at least
   max(MAX_FAST_FLOATREAD_LEN, MAX_FLOATREAD_PRECISION_DIGITS) more
   than the larget possible FP exponent. */
#define CHECK_INF_EXP_THRESHOLD 400

/* Don't bother reading more than the following number of digits in a
   floating-point mantissa: */
#define MAX_FLOATREAD_PRECISION_DIGITS CHECK_INF_EXP_THRESHOLD

#ifdef USE_EXPLICT_FP_FORM_CHECK

/* Fixes Linux problem of 0e...  => non-number (0 with ptr at e...) */
/* Fixes SunOS problem with numbers like .3e2666666666666 => 0.0 */
/* Fixes HP/UX problem with numbers like .3e2666666666666 => non-number */

# ifdef MZ_XFORM
END_XFORM_ARITH;
# endif

static double STRTOD(const char *orig_c, char **f)
{
  int neg = 0;
  int found_dot = 0, is_infinity = 0, is_zero = 0;
  const char *c = orig_c;

  *f = (char *)c;

  if (*c == '-') {
    c++;
    neg = 1;
  } else if (*c == '+') {
    c++;
  }

  if (!isdigit((unsigned char)*c)) {
    if (*c == '.') {
      if (!isdigit((unsigned char)c[1]))
	return 0; /* no digits - bad! */
    } else
      return 0; /* no digits - bad! */
  }

  for (; *c; c++) {
    int ch = *c;

    if (isdigit(ch)) {
      /* ok */
    } else if ((ch == 'e') || (ch == 'E')) {
      int e = 0, neg_exp = 0;

      c++;
      if (*c == '-') {
	c++;
	neg_exp = 1;
      } else if (*c == '+') {
	c++;
      }
      if (!isdigit((unsigned char)*c))
	return 0; /* no digits - bad! */

      for (; *c; c++) {
	int ch = *c;
	if (!isdigit(ch))
	  return 0; /* not a digit - bad! */
	else {
	  e = (e * 10) + (ch - '0');
	  if (e > CHECK_INF_EXP_THRESHOLD) {
	    if (neg_exp)
	      is_zero  = 1;
	    else
	      is_infinity  = 1;
	  }
	}
      }

      break;
    } else if (ch == '.') {
      if (found_dot)
	return 0; /* two dots - shouldn't happen */
      found_dot = 1;
    } else
      return 0; /* unknown non-digit - shouldn't happen */
  }
  
  *f = (char *)c;

  if (is_infinity) {
    if (neg)
      return scheme_minus_infinity_val;
    else
      return scheme_infinity_val;
  }

  if (is_zero) {
    if (neg)
      return scheme_floating_point_nzero;
    else
      return scheme_floating_point_zero;
  }

  /* It's OK if c is ok: */
  return strtod(orig_c, NULL);
}
# ifdef MZ_XFORM_GC
START_XFORM_ARITH;
# endif
#else
#define STRTOD(x, y) strtod(x, y)
#endif

#ifdef MZ_USE_SINGLE_FLOATS
static Scheme_Object *CHECK_SINGLE(Scheme_Object *v, int s)
{
  if (s && SCHEME_DBLP(v))
    return scheme_make_float((float)SCHEME_DBL_VAL(v));
  else
    return v;
}
#else
# define CHECK_SINGLE(v, s) v
#endif

Scheme_Object *scheme_read_number(const mzchar *str, intptr_t len,
				  int is_float, 
				  int is_not_float,
				  int decimal_means_float,
				  int radix, int radix_set, 
				  Scheme_Object *complain,
				  int *div_by_zero,
				  int test_only,
				  Scheme_Object *stxsrc, intptr_t line, intptr_t col, intptr_t pos, intptr_t span,
				  Scheme_Object *indentation)
{
  int i, has_decimal, must_parse, has_slash;
  int report, delta;
  Scheme_Object *next_complain;
  int has_hash, has_expt, has_i, has_sign, has_at, has_hash_since_slash;
  int saw_digit_since_slash, saw_nonzero_digit;
  Scheme_Object *o;
#ifdef MZ_USE_SINGLE_FLOATS
  int sgl;
#endif

  if (len < 0)
    len = scheme_char_strlen(str);

  delta = 0;

  while (str[delta] == '#') {
    if (str[delta+1] != 'E' && str[delta+1] != 'e' && str[delta+1] != 'I' && str[delta+1] != 'i') {
      if (radix_set) {
	if (complain)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: bad radix specification: %u",
			  str, len);
	else
	  return scheme_false;
      }
      radix_set = 1;
    } else {
      if (is_float || is_not_float) {
	if (complain)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: bad exactness specification: %u", 
			  str, len);
	else
	  return scheme_false;
      }
    }

    switch (str[delta+1]) {
    case 'B':
    case 'b':
      radix = 2;
      break;
    case 'O':
    case 'o':
      radix = 8;
      break;
    case 'D':
    case 'd':
      radix = 10;
      break;
    case 'X':
    case 'x':
      radix = 16;
      break;
    case 'I':
    case 'i':
      is_float = 1;
      break;
    case 'E':
    case 'e':
      is_not_float = 1;
      break;
    default:
      if (complain)
	scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			"read: bad `#' indicator `%c': %u",
			str[delta+1], str, len);
      return scheme_false;
    }
    delta += 2;
  }

  must_parse = (radix_set || is_float || is_not_float);

  report = complain && must_parse;
  next_complain = must_parse ? complain : NULL;

  if (!(len - delta)) {
    if (report)
      scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
		      "read: no digits");
    return scheme_false;
  }

  /* look for +inf.0, etc: */
  if (len -delta == 6) {
    Scheme_Object *special;
    special = read_special_number(str, delta);
    if (special) {
      if (!is_not_float)
	return special;
      if (report)
	scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			"read: no exact representation for %V",
			special);
      return scheme_false;
    }
  }

  /* Look for <special>+...i and ...<special>i */
  if ((len-delta > 7) && str[len-1] == 'i') {
    Scheme_Object *special;
    mzchar *s2;
    
    /* Try <special>+...i */
    special = read_special_number(str, delta);
    if (special) {
      s2 = (mzchar *)scheme_malloc_atomic((len - delta - 6 + 4 + 1) * sizeof(mzchar));
      s2[0] = '+';
      s2[1] = '0';
      s2[2] = '.';
      s2[3] = '0';
      memcpy(s2 + 4, str + delta + 6, (len - delta - 5) * sizeof(mzchar));
    } else {
      /* Try ...<special>i: */
      special = read_special_number(str, len - 7);
      if (special) {
	s2 = (mzchar *)scheme_malloc_atomic((len - delta - 6 + 4 + 1) * sizeof(mzchar));
	memcpy(s2, str + delta, (len - delta - 7) * sizeof(mzchar));
	s2[len - delta - 7] = '+';
	s2[len - delta - 7 + 1] = '0';
	s2[len - delta - 7 + 2] = '.';
	s2[len - delta - 7 + 3] = '0';
	s2[len - delta - 7 + 4] = 'i';
	s2[len - delta - 7 + 5] = 0;
	special = scheme_bin_mult(special, scheme_plus_i);
      } else
	s2 = NULL;
    }

    if (special) {
      Scheme_Object *other;
      int dbz = 0;

      if (is_not_float) {
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: no exact representation for %V",
			  special);
	return scheme_false;
      }

      other = scheme_read_number(s2, len - delta - 6 + 4,
				 is_float, is_not_float, 1,
				 radix, 1, 0,
				 &dbz, test_only,
				 stxsrc, line, col, pos, span,
				 indentation);

      if (dbz) {
	if (div_by_zero)
	  *div_by_zero = 1;
	if (complain)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: division by zero: %u",
			  str, len);
	return scheme_false;
      }

      if (!SCHEME_FALSEP(other))
	return scheme_bin_plus(special, other);
      
      if (!complain)
	return scheme_false;
    }
  } else if ((len-delta == 7) && str[len-1] == 'i') {
    /* Try <special>i */
    Scheme_Object *special;
    special = read_special_number(str, delta);
    if (special) {
      special = scheme_make_complex(scheme_make_integer(0), special);

      if (is_not_float) {
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: no exact representation for %V",
			  special);
	return scheme_false;
      }

      return special;
    }
  }

  /* Look for <special>@... and ...@<special> */
  if ((len - delta > 7) && ((str[delta+6] == '@') || (str[len - 7] == '@'))) {
    Scheme_Object *special;
    mzchar *s2;
    int spec_mag = 0;

    /* Try <special>@... */
    if (str[delta+6] == '@')
      special = read_special_number(str, delta);
    else
      special = NULL;
    if (special) {
      s2 = (mzchar *)scheme_malloc_atomic((len - delta - 6) * sizeof(mzchar));
      memcpy(s2, str + delta + 7, (len - delta - 6) * sizeof(mzchar));
      spec_mag = 1;
    } else {
      if (str[len - 7] == '@')
	special = read_special_number(str, len - 6);
      else
	special = NULL;
      
      if (special) {
	s2 = (mzchar *)scheme_malloc_atomic((len - delta - 6) * sizeof(mzchar));
	memcpy(s2, str + delta, (len - delta - 7) * sizeof(mzchar));
	s2[len - delta - 7] = 0;
      } else
	s2 = NULL;
    }

    if (special) {
      Scheme_Object *other;
      int dbz = 0;

      /* s2 can't contain @: */
      for (i = 0; s2[i]; i++) {
	if (s2[i] == '@')
	  break;
      }

      if (s2[i])
	other = scheme_false;
      else
	other = scheme_read_number(s2, len - delta - 7,
				   is_float, is_not_float, 1,
				   radix, 1, 0,
				   &dbz, test_only,
				   stxsrc, line, col, pos, span,
				   indentation);

      if (dbz) {
	if (div_by_zero)
	  *div_by_zero = 1;
	if (complain)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: division by zero: %u", 
			  str, len);
	return scheme_false;
      }

      if (!SCHEME_FALSEP(other)) {
	/* If string is complex, not well-formed: */
	if (!SCHEME_COMPLEXP(other)) {
	  Scheme_Object *a[2];
	  if (spec_mag) {
	    a[0] = special;
	    a[1] = other;
	  } else {
	    a[0] = other;
	    a[1] = special;
	  }

	  return scheme_make_polar(2, a);
	}
      }

      if (!complain)
	return scheme_false;
    }
  }
      
#define isinexactmark(ch) ((ch == 'e') || (ch == 'E') \
			   || (ch == 's') || (ch == 'S') \
			   || (ch == 'f') || (ch == 'F') \
			   || (ch == 'd') || (ch == 'D') \
			   || (ch == 'l') || (ch == 'L'))

#define isAdigit(ch) ((ch >= '0') && (ch <= '9'))


#define isbaseNdigit(N, ch) (((ch >= 'a') && (ch <= (mzchar)('a' + N - 11))) \
                             || ((ch >= 'A') && (ch <= (mzchar)('A' + N - 11))))

  has_i = 0;
  has_at = 0;
  has_sign = delta-1;
  for (i = delta; i < len; i++) {
    mzchar ch = str[i];
    if (!ch) {
      if (report)
	scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			"read: embedded null character: %u",
			str, len);
      return scheme_false;
    } else if (isinexactmark(ch) && ((radix <= 10) || !isbaseNdigit(radix, ch))) {
      /* If a sign follows, don't count it */
      if (str[i+1] == '+' || str[i+1] == '-')
	i++;
    } else if ((ch == '+') || (ch == '-')) {
      if ((has_sign > delta) || ((has_sign == delta) && (i == delta+1))) {
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			   "read: too many signs: %u", 
			   str, len);
	return scheme_false;
      }
      has_sign = i;
    } else if (((ch == 'I') || (ch == 'i')) && (has_sign >= delta)) {
      if (has_at) {
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: cannot mix `@' and `i': %u", 
			  str, len);
	return scheme_false;
      }
      if (i + 1 < len) {
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: `i' must be at the end: %u", 
			  str, len);
	return scheme_false;
      }
      has_i = i;
    } else if (ch == '@') {
      if (has_at) {
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: too many `@'s: %u", 
			  str, len);
	return scheme_false;
      }
      if (i == delta) {
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: `@' cannot be at start: %u", 
			  str, len);
	return scheme_false;
      }
      has_at = i;
      if (has_sign >= delta)
	has_sign = delta-1;
    }
  }

  if (has_i) {
    Scheme_Object *n1, *n2;
    mzchar *first, *second;
    int fdbz = 0, sdbz = 0;

    if (has_sign != delta) {
      first = (mzchar *)scheme_malloc_atomic((has_sign - delta + 1) * sizeof(mzchar));
      memcpy(first, str + delta, (has_sign - delta) * sizeof(mzchar));
      first[has_sign - delta] = 0;
    } else
      first = NULL;

    if (has_i - has_sign > 1) {
      second = (mzchar *)scheme_malloc_atomic((has_i - has_sign + 1) * sizeof(mzchar));
      memcpy(second, str + has_sign, (has_i - has_sign) * sizeof(mzchar));
      second[has_i - has_sign] = 0;
    } else
      second = NULL;

    if (first)
      n1 = scheme_read_number(first, has_sign - delta,
			      is_float, is_not_float, decimal_means_float,
			      radix, 1, next_complain,
			      &fdbz, test_only,
			      stxsrc, line, col, pos, span,
			      indentation);
    else
      n1 = zeroi;

    if (SAME_OBJ(n1, scheme_false) && !fdbz)
      return scheme_false;
    /* This +nan.0 test looks unnecessary  -- Matthew, 08/14/01 */
    else if (SCHEME_FLOATP(n1)) {
      double d = SCHEME_FLOAT_VAL(n1);
      if (MZ_IS_NAN(d))
	return scheme_false;
    }
    
    if (second)
      n2 = scheme_read_number(second, has_i - has_sign,
			      is_float, is_not_float, decimal_means_float,
			      radix, 1, next_complain,
			      &sdbz, test_only,
			      stxsrc, line, col, pos, span,
			      indentation);
    else if (str[has_sign] == '-')
      n2 = scheme_make_integer(-1);
    else
      n2 = scheme_make_integer(1);
    
    if (SAME_OBJ(n2, scheme_false) && !sdbz)
      return scheme_false;
    /* This +nan.0 test looks unnecessary  -- Matthew, 08/14/01 */
    else if (SCHEME_FLOATP(n2)) {
      double d = SCHEME_FLOAT_VAL(n2);
      if (MZ_IS_NAN(d))
	return scheme_false;
    }

    if (fdbz || sdbz) {
      if (div_by_zero)
	*div_by_zero = 1;
      if (complain)
	scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			 "read: division by zero: %u", 
			 str, len);
      return scheme_false;
    }

    if (!is_not_float && ((SCHEME_FLOATP(n1) && (n2 != zeroi)) || is_float))
      n2 = scheme_exact_to_inexact(1, &n2);  /* uses default conversion: float or double */
    else if (is_not_float)
      n2 = scheme_inexact_to_exact(1, &n2);

    if (!is_not_float && ((SCHEME_FLOATP(n2) && (n1 != zeroi)) || is_float))
      n1 = scheme_exact_to_inexact(1, &n1); /* uses default conversion: float or double */
    else if (is_not_float)
      n1 = scheme_inexact_to_exact(1, &n1);

    return scheme_make_complex(n1, n2);
  }

  if (has_at) {
    Scheme_Object *n1, *n2;
    double d1, d2, r1, r2;
    mzchar *first;
    const mzchar *second;
    int fdbz = 0, sdbz = 0;

    first = (mzchar *)scheme_malloc_atomic((has_at - delta + 1) * sizeof(mzchar));
    memcpy(first, str + delta, (has_at - delta) * sizeof(mzchar));
    first[has_at - delta] = 0;

#ifdef MZ_PRECISE_GC
    {
      /* Can't pass mis-aligned pointer to scheme_read_number. */
      int slen = len - (has_at + 1) + 1;
      second = (mzchar *)scheme_malloc_atomic(slen * sizeof(mzchar));
      memcpy((mzchar *)second, str + has_at + 1, slen * sizeof(mzchar));
    }
#else
    second = str + has_at + 1;
#endif

    n2 = scheme_read_number(second, len - has_at - 1,
			    is_float, is_not_float, decimal_means_float,
			    radix, 1, next_complain,
			    &fdbz, test_only,
			    stxsrc, line, col, pos, span,
			    indentation);

    if (!fdbz) {
      if (SCHEME_FALSEP(n2))
	return scheme_false;

      /* Special case: angle is zero => real number */
      if (n2 == zeroi)
	return scheme_read_number(first, has_at - delta,
				  is_float, is_not_float, decimal_means_float,
				  radix, 1, complain,
				  div_by_zero,
				  test_only,
				  stxsrc, line, col, pos, span,
				  indentation);

      n2 = scheme_exact_to_inexact(1, &n2); /* uses default conversion: float or double */

      d2 = SCHEME_FLOAT_VAL(n2);
      
      /* This +nan.0 test looks unnecessary  -- Matthew, 08/14/01 */
      if (MZ_IS_NAN(d2))
	return scheme_false;

      n1 = scheme_read_number(first, has_at - delta, 
			      is_float, is_not_float, decimal_means_float,
			      radix, 1, next_complain,
			      &sdbz,
			      test_only,
			      stxsrc, line, col, pos, span,
			      indentation);

      /* Special case: magnitude is zero => zero */
      if (n1 == zeroi)
	return zeroi;

      if (!SCHEME_FALSEP(n1))
	n1 = scheme_exact_to_inexact(1, &n1); /* uses default conversion: float or double */
    } else {
      n1 = NULL;
      d2 = 0;
    }

    if (fdbz || sdbz) {
      if (div_by_zero)
	*div_by_zero = 1;
      if (complain)
	scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			"read: division by zero in %u", 
			str, len);
      return scheme_false;
    }

    if (SCHEME_FALSEP(n1))
      return scheme_false;

    d1 = SCHEME_FLOAT_VAL(n1);

    /* This +nan.0 test looks unnecessary  -- Matthew, 08/14/01 */
    if (MZ_IS_NAN(d1))
      return scheme_false;

    r1 = d1 * SCH_COS(d2);
    r2 = d1 * SCH_SIN(d2);

#ifdef MZ_USE_SINGLE_FLOATS
    if (SCHEME_FLTP(n1) && SCHEME_FLTP(n2))
      n1 = scheme_make_complex(scheme_make_float((float)r1),
                               scheme_make_float((float)r2));
    else
#endif
      n1 = scheme_make_complex(scheme_make_double(r1),
                               scheme_make_double(r2));

    if (is_not_float)
      n1 = scheme_inexact_to_exact(1, &n1);

    return n1;
  }

  has_decimal = has_slash = has_hash = has_hash_since_slash = has_expt = 0;
  saw_digit_since_slash = saw_nonzero_digit = 0;
  for (i = delta; i < len; i++) {
    mzchar ch = str[i];
    if (ch == '.') {
      if (has_decimal) {
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: multiple decimal points: %u", 
			  str, len);
	return scheme_false;
      }
      if (has_slash) {
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: decimal points and fractions "
			  "cannot be mixed: %u", 
			  str, len);
	return scheme_false;
      }
      has_decimal = 1;
    } else if (isinexactmark(ch)
	       && ((radix <= 10) || !isbaseNdigit(radix, ch))) {
      if (i == delta) {
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: cannot begin with `%c' in %u", 
			  ch, str, len);
	return scheme_false;
      }
      has_expt = i;
      break;
    } else if (ch == '/') {
      if (i == delta) {
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: cannot have slash at start: %u", 
			  str, len);
	return scheme_false;
      }
      if (has_slash) {
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: multiple slashes: %u", 
			  str, len);
	return scheme_false;
      }
      if (has_decimal) {
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: decimal points and fractions "
			  "cannot be mixed: %u", 
			  str, len);
	return scheme_false;
      }
      has_slash = i;
      saw_digit_since_slash = 0;
      has_hash_since_slash = 0;
    } else if ((ch == '-') || (ch == '+')) {
      if (has_slash || has_decimal || has_hash) {
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: misplaced sign: %u", 
			  str, len);
	return scheme_false;
      }
    } else if (ch == '#') {
      if (!saw_digit_since_slash) {
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: misplaced hash: %u", 
			  str, len);
	return scheme_false;
      }
      has_hash = 1;
      has_hash_since_slash = 1;
    } else if (!isAdigit(ch) && !((radix > 10) && isbaseNdigit(radix, ch))) {
      if (has_decimal) {
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: bad decimal number: %u", 
			  str, len);
	return scheme_false;
      }
      if (has_hash) {
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: misplaced hash: %u", 
			  str, len);
	return scheme_false;
      }
      break;
    } else {
      saw_digit_since_slash = 1;
      if (ch != '0')
	saw_nonzero_digit = 1;
      if (has_hash_since_slash) {
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: misplaced hash: %u", 
			  str, len);
	return scheme_false;
      }
    }
  }

#ifdef MZ_USE_SINGLE_FLOATS
  if (has_expt && str[has_expt]) {
    sgl = str[has_expt];
    sgl = ((sgl == 'f') || (sgl == 'F')
# ifdef USE_SINGLE_FLOATS_AS_DEFAULT
	      || (sgl == 'e') || (sgl == 'E')
#endif
	      || (sgl == 's') || (sgl == 'S'));
  } else {
# ifdef USE_SINGLE_FLOATS_AS_DEFAULT
    sgl = 1;
# else
    sgl = 0;
# endif
  }
#endif


#define MAX_FAST_FLOATREAD_LEN 50
  /* When possible, use the standard floating-point parser */
  if (!is_not_float && (is_float || decimal_means_float) 
      && !has_slash && !has_hash && (radix == 10) 
      && (has_decimal || has_expt)
      && (len <= MAX_FAST_FLOATREAD_LEN)) {
    double d;
    GC_CAN_IGNORE char *ptr;

    if (has_expt && !(str[has_expt + 1])) {
      if (report)
	scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			"read: no digits after \"%c\": %u",
			str[has_expt], str, len);
      return scheme_false;
    }

    {
      /* We'd like to use strtod() for the common case, but we don't trust it entirely. */
      char ffl_buf[MAX_FAST_FLOATREAD_LEN + 1];
      GC_CAN_IGNORE char *loc;

      {
        int k;
        for (k = delta; k < len; k++) {
          if (str[k] > 127)
            ffl_buf[k - delta] = '?';
          else
            ffl_buf[k - delta] = str[k];
        }
        ffl_buf[len - delta] = 0;
      }

      if (has_expt && (str[has_expt] != 'e' && str[has_expt] != 'E')) {
        ffl_buf[has_expt - delta] = 'e';
      }

      loc = scheme_push_c_numeric_locale();

      d = STRTOD(ffl_buf, &ptr);

      scheme_pop_c_numeric_locale(loc);

      if ((ptr XFORM_OK_MINUS ffl_buf) < (len - delta)) {
        if (report)
          scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
              "read: bad decimal number %u",
              str, len);
        return scheme_false;
      } 
    }

    if (!saw_nonzero_digit) {
      /* Assert: d = 0.0 or -0.0 */
      if (str[delta] == '-') {
	/* Make sure it's -0.0 */
#ifdef MZ_USE_SINGLE_FLOATS
	if (sgl) return scheme_nzerof;
#endif
	return scheme_nzerod;
      }
    }

    if (!d) {
      if (str[delta] == '-') {
	/* Make sure it's -0.0 */
#ifdef MZ_USE_SINGLE_FLOATS
	if (sgl) return scheme_nzerof;
#endif
	return scheme_nzerod;
      }
    }

#ifdef MZ_USE_SINGLE_FLOATS
    if (sgl)
      return scheme_make_float((float)d);
#endif
    return scheme_make_double(d);
  }

  if (has_decimal || has_expt || (has_hash && !has_slash)) {
    Scheme_Object *mantissa, *exponent, *power, *n;
    Scheme_Object *args[2];
    int result_is_float= (is_float || (!is_not_float && decimal_means_float));

    if (has_expt) {
      mzchar *substr;

      if (!str[has_expt + 1]) {
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: no digits after \"%c\": %u",
			  str[has_expt], str, len);
	return scheme_false;
      }

#ifdef MZ_PRECISE_GC
      {
	/* Can't pass misaligned pointer to scheme_read_bignum: */
	int slen = len - (has_expt + 1) + 1;
	substr = (mzchar *)scheme_malloc_atomic(slen * sizeof(mzchar));
	memcpy(substr, str + has_expt + 1, slen * sizeof(mzchar));
      }
#else
      substr = (mzchar *)str + has_expt + 1;
#endif

      exponent = scheme_read_bignum(substr, 0, radix);
      if (SCHEME_FALSEP(exponent)) {
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: bad exponent: %u", 
			  str, len);
	return scheme_false;
      }
    } else
      exponent = zeroi;

    if (!has_expt)
      has_expt = len;

    if (has_slash) {
      /* Mantissa is a fraction. */
      mzchar *s;
      int dbz;
      
      s = (mzchar *)scheme_malloc_atomic((has_expt - delta + 1) * sizeof(mzchar));
      memcpy(s, str + delta, (has_expt - delta) * sizeof(mzchar));
      s[has_expt - delta] = 0;
      
      mantissa = scheme_read_number(s, has_expt - delta, 
				    is_float, is_not_float, 1,
				    radix, 1, next_complain,
				    &dbz,
				    test_only,
				    stxsrc, line, col, pos, span,
				    indentation);

      if (SCHEME_FALSEP(mantissa)) {
	if (dbz) {
	  if (div_by_zero)
	    *div_by_zero = 1;
	  if (complain)
	    scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			    "read: division by zero: %u", 
			    str, len);
	}
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: bad number: %u", 
			  str, len);
	return scheme_false;
      }
    } else {
      /* Mantissa is not a fraction. */
      mzchar *digits;
      int extra_power = 0, dcp = 0, num_ok;

      digits = (mzchar *)scheme_malloc_atomic((has_expt - delta + 1) * sizeof(mzchar));

      i = delta;
      if (str[i] == '+' || str[i] == '-')
	digits[dcp++] = str[i++];

      for (; isAdigit(str[i]) || ((radix > 10) && isbaseNdigit(radix, str[i])); i++) {
	digits[dcp++] = str[i];
      }

      if (str[i] == '#') {
	for (; str[i] == '#'; i++) {
	  digits[dcp++] = '0';
	}
	num_ok = 0;
      } else
	num_ok = 1;
	
      if (str[i] == '.') {
	i++;
	if (num_ok)
	  for (; isAdigit(str[i]) || ((radix > 10) && isbaseNdigit(radix, str[i])); i++) {
	    digits[dcp++] = str[i];
	    extra_power++;
	  }

	for (; str[i] == '#'; i++) {
	  digits[dcp++] = '0';  
	  extra_power++;
	}
      }

      if ((str[i] && (!has_expt || i != has_expt))
	  || !dcp || (dcp == 1 && !(isAdigit(digits[0])
				    || ((radix > 10) && isbaseNdigit(radix, digits[0]))))) {
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			  "read: bad decimal number %u", 
			  str, len);
	return scheme_false;
      }

      /* Reduce unnecessary mantissa-reading work for inexact results.
         This is also necessary to make the range check on `exponent'
         correct. */
      if (result_is_float && (dcp > MAX_FLOATREAD_PRECISION_DIGITS)) {
	extra_power -= (dcp - MAX_FLOATREAD_PRECISION_DIGITS);
	dcp = MAX_FLOATREAD_PRECISION_DIGITS;
      }

      digits[dcp] = 0;
      mantissa = scheme_read_bignum(digits, 0, radix);
      if (SCHEME_FALSEP(mantissa)) {
	/* can get here with bad radix */
	if (report)
	  scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			   "read: bad number: %u", 
			   str, len);
	return scheme_false;
      }

      if (extra_power)
	exponent = scheme_bin_minus(exponent, scheme_make_integer(extra_power));
    
      /* Don't calculate a huge exponential if we're returning a float: */
      if (result_is_float) {
	if (scheme_bin_gt(exponent, scheme_make_integer(CHECK_INF_EXP_THRESHOLD))) {
	  if (scheme_is_negative(mantissa))
	    return CHECK_SINGLE(scheme_minus_inf_object, sgl);
	  else
	    return CHECK_SINGLE(scheme_inf_object, sgl);
	} else if (scheme_bin_lt(exponent, scheme_make_integer(-CHECK_INF_EXP_THRESHOLD))) {
	  if (scheme_is_negative(mantissa))
	    return CHECK_SINGLE(scheme_nzerod, sgl);
	  else
	    return CHECK_SINGLE(scheme_zerod, sgl);
	}
      }
    }

    /* This is the important use of test_only, because it's the one
       place where the read calculation is not linear in the input. */
    if (test_only)
      return scheme_make_integer(1);

    args[0] = scheme_make_integer(radix);
    args[1] = exponent;
    power = scheme_expt(2, args);

    n = scheme_bin_mult(mantissa, power);

    if (result_is_float)
      n = CHECK_SINGLE(TO_DOUBLE(n), sgl);
    else
      n = CHECK_SINGLE(n, sgl);

    if (SCHEME_FLOATP(n) && str[delta] == '-') {
      if (SCHEME_FLOAT_VAL(n) == 0.0) {
	/* 0.0 => -0.0 */
#ifdef MZ_USE_SINGLE_FLOATS
	if (SCHEME_FLTP(n)) {
	  n = scheme_make_float(-SCHEME_FLT_VAL(n));
	} else
#endif
	  n = scheme_make_double(-SCHEME_DBL_VAL(n));
      }
    }

    return n;
  }
  
  if (has_slash) {
    Scheme_Object *n1, *n2;
    mzchar *first;

    first = (mzchar *)scheme_malloc_atomic((has_slash - delta + 1) * sizeof(mzchar));
    memcpy(first, str + delta, (has_slash - delta) * sizeof(mzchar));
    first[has_slash - delta] = 0;

    n1 = scheme_read_number(first, has_slash - delta,
			    is_float, is_not_float, 1,
			    radix, 1, next_complain,
			    div_by_zero,
			    test_only,
			    stxsrc, line, col, pos, span,
			    indentation);
    if (SAME_OBJ(n1, scheme_false))
      return scheme_false;

    {
      mzchar *substr;

#ifdef MZ_PRECISE_GC
      {
	/* Can't pass misaligned pointer to scheme_read_bignum: */
	int slen = len - (has_slash + 1) + 1;
	substr = (mzchar *)scheme_malloc_atomic(slen * sizeof(mzchar));
	memcpy(substr, str + has_slash + 1, slen * sizeof(mzchar));
      }
#else
      substr = (mzchar *)str + has_slash + 1;
#endif

      n2 = scheme_read_number(substr, len - has_slash - 1,
			      is_float, is_not_float, 1,
			      radix, 1, next_complain,
			      div_by_zero,
			      test_only,
			      stxsrc, line, col, pos, span,
			      indentation);
    }

    if (SAME_OBJ(n2, scheme_false))
      return scheme_false;

    if (SCHEME_EXACT_REALP(n2) && scheme_is_zero(n2)) {
      if (complain)
	scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			"read: division by zero: %u", 
			str, len);
      if (div_by_zero)
	*div_by_zero = 1;
      return scheme_false;
    }

    if (test_only)
      return scheme_make_integer(1);

    n1 = scheme_bin_div(n1, n2);

    if (is_not_float) {
      if (SCHEME_FLOATP(n1)) {
	if (!scheme_check_double(NULL, SCHEME_FLOAT_VAL(n1), NULL)) {
	  if (complain)
	    scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
			    "read: no exact representation for %V", 
			    n1);
	  return scheme_false;
	}
      }
      n1 = scheme_inexact_to_exact(1, &n1);
    } else if (is_float)
      n1 = TO_DOUBLE(n1);

    return CHECK_SINGLE(n1, sgl);
  }

  o = scheme_read_bignum(str, delta, radix);
  if (SAME_OBJ(o, scheme_false)) {
    if (report)
      scheme_read_err(complain, stxsrc, line, col, pos, span, 0, indentation,
		      "read: bad number: %u", 
		      str, len);
  } else if (is_float) {
    /* Special case: "#i-0" => -0. */
    if ((o == zeroi) && str[delta] == '-') {
#ifdef MZ_USE_SINGLE_FLOATS
      if (sgl) return scheme_nzerof;
#endif
      return scheme_nzerod;
    }

    return CHECK_SINGLE(TO_DOUBLE(o), sgl);
  }

  return o;
}

/*========================================================================*/
/*                           Racket functions                             */
/*========================================================================*/

static Scheme_Object *
number_to_string (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  intptr_t radix;

  if (!SCHEME_NUMBERP(o))
    scheme_wrong_contract("number->string", "number?", 0, argc, argv);
  
  if (argc == 2) {
    if (!SCHEME_INTP(argv[1]))
      radix = 0;
    else
      radix = SCHEME_INT_VAL(argv[1]);

    if ((radix != 2) && (radix != 8) && (radix != 10)  && (radix != 16)) {
      scheme_wrong_contract("number->string", "(or/c 2 8 10 16)", 1, argc, argv);
      ESCAPED_BEFORE_HERE;
    }
    
    radix = SCHEME_INT_VAL(argv[1]);
  } else
    radix = 10;

  if (SCHEME_INTP(o) && ((radix == 10) || (radix == 16))) {
    /* Fast path for common case. */
    mzchar num[32];
    int pos = 32;
    intptr_t v = SCHEME_INT_VAL(o);
    if (v) {
      int neg, digit;
      if (v < 0) {
	neg = 1;
	v = -v;
      } else
	neg = 0;
      while (v) {
	digit = (v % radix);
	if (digit < 10)
	  num[--pos] = digit + '0';
	else
	  num[--pos] = (digit - 10) + 'a';
	v = v / radix;
      }
      if (neg)
	num[--pos] = '-';
    } else {
      num[--pos] = '0';
    }
    return scheme_make_sized_offset_char_string(num, pos, 32 - pos, 1);
  }

  return scheme_make_utf8_string/*_without_copying*/(number_to_allocated_string(radix, o, 1));
}


static Scheme_Object *
string_to_number (int argc, Scheme_Object *argv[])
{
  intptr_t radix;
  intptr_t len;
  mzchar *mzstr;
  int decimal_inexact, div_by_zero = 0;
  Scheme_Object *v;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract("string->number", "string?", 0, argc, argv);

  if (argc > 1) {
    if (SCHEME_INTP(argv[1]))
      radix = SCHEME_INT_VAL(argv[1]);
    else
      radix = 0;
    
    if ((radix < 2) || (radix > 16)) {
      scheme_wrong_contract("string->number", "(integer-in 2 16)", 1, argc, argv);
      ESCAPED_BEFORE_HERE;
    }
  } else
    radix = 10;

  decimal_inexact = SCHEME_TRUEP(scheme_get_param(scheme_current_config(), 
						  MZCONFIG_READ_DECIMAL_INEXACT));

  mzstr = SCHEME_CHAR_STR_VAL(argv[0]);
  len = SCHEME_CHAR_STRTAG_VAL(argv[0]);

  v = scheme_read_number(mzstr, len, 
			 0, 0, decimal_inexact,
			 radix, 0, NULL, &div_by_zero,
			 0, NULL, 0, 0, 0, 0,
			 NULL);

  return v;
}


static char *double_to_string (double d, int alloc, int was_single)
{
  char buffer[100], *s;
  int l, i, digits;

  if (MZ_IS_NAN(d))
#ifdef MZ_USE_SINGLE_FLOATS
    if (was_single)
      s = single_not_a_number_str;
    else
#endif
      s = not_a_number_str;
  else if (MZ_IS_POS_INFINITY(d))
#ifdef MZ_USE_SINGLE_FLOATS
    if (was_single)
      s = single_infinity_str;
    else
#endif
      s = infinity_str;
  else if (MZ_IS_NEG_INFINITY(d))
#ifdef MZ_USE_SINGLE_FLOATS
    if (was_single)
      s = single_minus_infinity_str;
    else
#endif
      s = minus_infinity_str;
  else if (d == 0.0) {
    /* Check for -0.0, since some printers get it wrong. */
    if (scheme_minus_zero_p(d))
#ifdef MZ_USE_SINGLE_FLOATS
      if (was_single)
	/* The f0 suffix causes the string to be read as a single-
	   precision float. */
	s = "-0.0f0";
      else
#endif
	s = "-0.0";
    else
#ifdef MZ_USE_SINGLE_FLOATS
      if (was_single)
	s = "0.0f0";
      else
#endif
	s = "0.0";
  } else {
    /* Initial count for significant digits is 14 (double) or 6 digits
       (single). That's big enough to get most right, small enough to
       avoid nonsense digits. But we'll loop in case it's not precise
       enough to get read-write invariance: */
    GC_CAN_IGNORE char *loc;
    if (was_single)
      digits = 6;
    else
      digits = 14;
    loc = scheme_push_c_numeric_locale();
    while (digits < 30) {
      double check;
      GC_CAN_IGNORE char *ptr;

      sprintf(buffer, "%.*g", digits, d);

      /* Did we get read-write invariance, yet? */
      check = strtod(buffer, &ptr);
      if (was_single) {
        if ((float)check == (float)d)
          break;
      } else {
        if (check == d)
          break;
      }

      digits++;
    }
    scheme_pop_c_numeric_locale(loc);
    
    l = strlen(buffer);
    for (i = 0; i < l; i++) {
      if (buffer[i] == '.' || isalpha((unsigned char)buffer[i]))
	break;
    }
    if (i == l) {
      buffer[i] = '.';
      buffer[i + 1] = '0';
      buffer[i + 2] = 0;
      l += 2;
    }
#ifdef MZ_USE_SINGLE_FLOATS
    if (was_single) {
      /* In case of a single-precision float, add the f0 suffix (or
	 replace the existing e exponent separator) to cause the
	 string to be read back as a single-precision float. */
      for (i = 0; i < l; i++) {
	if (buffer[i] == 'e')
	  break;
      }
      if (i == l) {
	buffer[l] = 'f';
	buffer[l + 1] = '0';
	buffer[l + 2] = 0;
	l += 2;
      } else {
	buffer[i] = 'f';
      }
    }
#endif
    
    s = (char *)scheme_malloc_atomic(strlen(buffer) + 1);
    strcpy(s, buffer);
    alloc = 0;
  }

  if (alloc) {
    char *s2;
    l = strlen(s) + 1;
    s2 = (char *)scheme_malloc_atomic(l);
    memcpy(s2, s, l);
    s = s2;
  }

  return s;
}

static char *number_to_allocated_string(int radix, Scheme_Object *obj, int alloc)
{
  char *s;

  if (SCHEME_FLOATP(obj)) {
    if (radix != 10)
      scheme_contract_error("number->string",
                            "inexact numbers can only be printed in base 10",
                            "number", 1, obj,
                            "requested base", 1, scheme_make_integer(radix),
                            NULL);
    s = double_to_string(SCHEME_FLOAT_VAL(obj), alloc, SCHEME_FLTP(obj));
  } else if (SCHEME_RATIONALP(obj)) {
    Scheme_Object *n, *d;
    char *ns, *ds;
    int nlen, dlen;

    n = scheme_rational_numerator(obj);
    d = scheme_rational_denominator(obj);

    ns = number_to_allocated_string(radix, n, 0);
    ds = number_to_allocated_string(radix, d, 0);

    nlen = strlen(ns);
    dlen = strlen(ds);

    s = (char *)scheme_malloc_atomic(nlen + dlen + 2);
    memcpy(s, ns, nlen);
    s[nlen] = '/';
    strcpy(s + nlen + 1, ds);
  } else if (SCHEME_COMPLEXP(obj)) {
    Scheme_Object *r, *i;
    char *rs, *is;
    int rlen, ilen, offset = 0;

    r = _scheme_complex_real_part(obj);
    i = _scheme_complex_imaginary_part(obj);

    rs = number_to_allocated_string(radix, r, 0);
    is = number_to_allocated_string(radix, i, 0);

    rlen = strlen(rs);
    ilen = strlen(is);
    s = (char *)scheme_malloc_atomic(rlen + ilen + 3);
    memcpy(s, rs, rlen);
    if ((is[0] != '-') && (is[0] != '+')) {
      offset = 1;
      s[rlen] = '+';
    }
    memcpy(s + rlen + offset, is, ilen);
    s[rlen + offset + ilen] = 'i';
    s[rlen + offset + ilen + 1] = 0;
  } else {
    if (SCHEME_INTP(obj))
      obj = scheme_make_bignum(SCHEME_INT_VAL(obj));

    s = scheme_bignum_to_allocated_string(obj, radix, alloc);
  }

  return s;
}

char *scheme_number_to_string(int radix, Scheme_Object *obj)
{
  return number_to_allocated_string(radix, obj, 0);
}

int scheme_check_double(const char *where, double d, const char *dest)
{
  if (MZ_IS_INFINITY(d)
      || MZ_IS_NAN(d)) {
    if (where) {
      char buf[32];
      sprintf(buf, "no %s representation", dest);
      scheme_contract_error(where,
                            buf,
                            "number", 1, scheme_make_double(d),
                            NULL);
    }
    return 0;
  }

  return 1;
}

/*========================================================================*/
/*                      native representations                            */
/*========================================================================*/

static Scheme_Object *bytes_to_integer (int argc, Scheme_Object *argv[])
{
  intptr_t strlen, slen;
  int sgned;
  char *str;
  int buf[2], i;
  int bigend = MZ_IS_BIG_ENDIAN, offset = 0;

  if (!SCHEME_BYTE_STRINGP(argv[0]))
    scheme_wrong_contract("integer-bytes->integer", "bytes?", 0, argc, argv);
  strlen = SCHEME_BYTE_STRLEN_VAL(argv[0]);

  str = SCHEME_BYTE_STR_VAL(argv[0]);

  sgned = SCHEME_TRUEP(argv[1]);
  if (argc > 2)
    bigend = SCHEME_TRUEP(argv[2]);

  if (argc > 3) {
    intptr_t start, finish;

    scheme_get_substring_indices("integer-bytes->integer", argv[0],
                                 argc, argv,
                                 3, 4, &start, &finish);

    offset = start;
    slen = finish - start;
  } else {
    offset = 0;
    slen = strlen;
  }

  if ((slen != 2)  && (slen != 4) && (slen != 8)) {
    scheme_contract_error("integer-bytes->integer",
                          "length is not 2, 4, or 8 bytes",
                          "length", 1, scheme_make_integer(slen),
                          NULL);
    return NULL;
  }

  if (bigend != MZ_IS_BIG_ENDIAN) {
    for (i = 0; i < slen; i++) {
      ((char *)buf)[slen - i - 1] = str[i + offset];
    }
    str = (char *)buf;
  } else {
    memcpy(&buf, str + offset, slen);
    str = (char *)buf;
  }

  switch(slen) {
  case 2:
    if (sgned) {
      short val;
      memcpy(&val, str, sizeof(short));
      return scheme_make_integer(val);
      }
    else {
      unsigned short val;
      memcpy(&val, str, sizeof(unsigned short));
      return scheme_make_integer(val);
      }
    break;
  case 4:
    if (sgned) {
      int val;
      memcpy(&val, str, sizeof(int));
      return scheme_make_integer_value(val);
      }
    else {
      unsigned int val;
      memcpy(&val, str, sizeof(unsigned int));
      return scheme_make_integer_value_from_unsigned(val);
      }
    break;
  default:
#ifdef SIXTY_FOUR_BIT_INTEGERS
    if (sgned) {
      intptr_t val;
      memcpy(&val, str, sizeof(intptr_t));
      return scheme_make_integer_value(val);
      }
    else {
      uintptr_t val;
      memcpy(&val, str, sizeof(uintptr_t));
      return scheme_make_integer_value_from_unsigned(val);
      }
    break;
#else
# ifndef NO_LONG_LONG_TYPE
    {
      mzlonglong lv;
      memcpy(&lv, str, sizeof(mzlonglong));
      if (sgned)
	return scheme_make_integer_value_from_long_long(lv);
      else
	return scheme_make_integer_value_from_unsigned_long_long((umzlonglong)lv);
      break;
    }
# else
    {
      Scheme_Object *h, *l, *a[2];

#  if MZ_IS_BIG_ENDIAN
      /* make little-endian at int level: */
      {
	int v;
	v = ((int *)str)[0];
	buf[0] = ((int *)str)[1];
	buf[1] = v;
	str = (char *)buf;
      }
#  endif

      if (sgned)
	h = scheme_make_integer_value(((int *)str)[1]);
      else
	h = scheme_make_integer_value_from_unsigned(((unsigned int *)str)[1]);
      l = scheme_make_integer_value_from_unsigned(((unsigned int *)str)[0]);
      a[0] = h;
      a[1] = scheme_make_integer(32);
      h = scheme_bitwise_shift(2, a);
      return scheme_bin_plus(h, l);
    }
# endif
#endif
    break;
  }

  /* throw an error here */
}

#define MZ_U8HI 0
#define MZ_S8HI 1
#define MZ_S8LO 2

static Scheme_Object *integer_to_bytes(int argc, Scheme_Object *argv[])
{
  Scheme_Object *n, *s;
  char *str;
  int size, sgned;
  intptr_t val, offset, buf[2];
#if !defined(NO_LONG_LONG_TYPE) && !defined(SIXTY_FOUR_BIT_INTEGERS)
  mzlonglong llval;
#endif
  int bigend = MZ_IS_BIG_ENDIAN, bad;

  n = argv[0];
  if (!SCHEME_INTP(n) && !SCHEME_BIGNUMP(n))
    scheme_wrong_contract("integer->integer-bytes", "exact-integer?", 0, argc, argv);

  if (SCHEME_INTP(argv[1]))
    size = SCHEME_INT_VAL(argv[1]);
  else
    size = 0;
  if ((size != 2) && (size != 4) && (size != 8))
    scheme_wrong_contract("integer->integer-bytes", "(or/c 2 4 8)", 1, argc, argv);

  sgned = SCHEME_TRUEP(argv[2]);
  if (argc > 3)
    bigend = SCHEME_TRUEP(argv[3]);
  
  if (argc > 4)
    s = argv[4];
  else
    s = scheme_make_sized_byte_string("12345678", size, 1);
  
  if (!SCHEME_MUTABLE_BYTE_STRINGP(s))
    scheme_wrong_contract("integer->integer-bytes", "(and/c bytes? (not/c immutable?))", 4, argc, argv);

  if (argc > 5) {
    intptr_t start, finish;
    
    scheme_get_substring_indices("integer-bytes->integer", s,
                                 argc, argv,
                                 5, 6, &start, &finish);

    offset = start;
  } else
    offset = 0;
  
  if (offset + size > SCHEME_BYTE_STRLEN_VAL(s)) {
    scheme_contract_error("integer-bytes->integer",
                          "byte string length is shorter than starting position plus size",
                          "byte string length", 1, scheme_make_integer(SCHEME_BYTE_STRLEN_VAL(s)),
                          "starting position", 1, scheme_make_integer(offset),
                          "size", 1, scheme_make_integer(size),
                          NULL);
    return NULL;
  }

  /* Check for mismatch: number doesn't fit */
  if (size == 2) {
    if (SCHEME_BIGNUMP(n))
      bad = 1;
    else {
      val = SCHEME_INT_VAL(n);
      if (sgned) {
	bad = ((val < -32768) || (val > 32767));
      } else {
	bad = ((val < 0) || (val > 65535));
      }
    }
  } else if (size ==4) {
    if (sgned)
      bad = !scheme_get_int_val(n, &val);
    else
      bad = !scheme_get_unsigned_int_val(n, (uintptr_t *)&val);
#ifdef SIXTY_FOUR_BIT_INTEGERS
    if (!bad) {
      if (sgned)
	bad = ((val > (intptr_t)0x7fffffff) || (val < -(intptr_t)0x80000000));
      else
	bad = (val > (intptr_t)0xffffffff);
    }
#endif
  } else  {
#ifdef SIXTY_FOUR_BIT_INTEGERS
    if (sgned)
      bad = !scheme_get_int_val(n, &val);
    else
      bad = !scheme_get_unsigned_int_val(n, (uintptr_t *)&val);
#else
# ifndef NO_LONG_LONG_TYPE
    if (sgned)
      bad = !scheme_get_long_long_val(n, &llval);
    else
      bad = !scheme_get_unsigned_long_long_val(n, (umzlonglong *)&llval);
# else
    if (sgned)
      bad = (scheme_bin_lt(n, num_limits[MZ_S8LO])
	     || scheme_bin_lt(num_limits[MZ_S8HI], n));
    else
      bad = (!scheme_nonneg_exact_p(n)
	     || scheme_bin_lt(num_limits[MZ_U8HI], n));

    val = 0;
# endif
#endif
  }

  if (bad) {
    scheme_contract_error("integer->integer-bytes",
                          (sgned
                           ? "integer does not fit into requested signed bytes"
                           : "integer does not fit into requested unsigned bytes"),
                          "integer", 1, n,
                          "requested bytes", 1, scheme_make_integer(size),
                          NULL);
    return NULL;
  }

  /* Finally, do the work */
  str = (char *)buf;
  switch (size) {
  case 2:
    {
      if (sgned) {
        unsigned short value = val;
        memcpy(str, &value, sizeof(unsigned short));
      } else {
        short value = val;
        memcpy(str, &value, sizeof(short));
      }
    }
    break;
  case 4:
    if (sgned) {
        unsigned int value = val;
        memcpy(str, &value, sizeof(unsigned int));
    } else {
        int value = val;
        memcpy(str, &value, sizeof(int));
    }
    break;
  default:
#ifdef SIXTY_FOUR_BIT_INTEGERS
    *(intptr_t *)str = val;
#else
# ifndef NO_LONG_LONG_TYPE
    memcpy(str, &llval, sizeof(mzlonglong));
# else
    {
      Scheme_Object *hi, *lo, *a[2];
      uintptr_t ul;
      
      a[0] = n;
      a[1] = scheme_make_integer_value_from_unsigned((uintptr_t)-1);
      lo = scheme_bitwise_and(2, a);
      a[1] = scheme_make_integer(-32);
      hi = scheme_bitwise_shift(2, a);

      scheme_get_unsigned_int_val(lo, &ul);
      
      ((unsigned int *)str)[0] = ul;
      if (sgned) {
	scheme_get_int_val(hi, &val);
	((unsigned int *)str)[1] = val;
      } else {
	scheme_get_unsigned_int_val(hi, &ul);
	((unsigned int *)str)[1] = ul;
      }

#if MZ_IS_BIG_ENDIAN
      {
	/* We've assumed little endian so far */
	val = ((int *)str)[0];
	((int *)str)[0] = ((int *)str)[1];
	((int *)str)[1] = val;
      }
#endif

    }
# endif
#endif
    break;
  }

  str = SCHEME_BYTE_STR_VAL(s);
  if (bigend != MZ_IS_BIG_ENDIAN) {
    int i;
    for (i = 0; i < size; i++) {
      str[i + offset] = ((char *)buf)[size - i - 1];
    }
  } else {
    int i;
    for (i = 0; i < size; i++) {
      str[i + offset] = ((char *)buf)[i];
    }
  }

  return s;
}

static Scheme_Object *bytes_to_real (int argc, Scheme_Object *argv[])
{
  intptr_t offset = 0, slen;
  char *str, buf[8];
  int bigend = MZ_IS_BIG_ENDIAN;

  if (!SCHEME_BYTE_STRINGP(argv[0]))
    scheme_wrong_contract("integer-bytes->integer", "bytes?", 0, argc, argv);

  if (argc > 2) {
    intptr_t start, finish;

    scheme_get_substring_indices("integer-bytes->integer", argv[0],
                                 argc, argv,
                                 2, 3, &start, &finish);

    offset = start;
    slen = finish - start;
  } else {
    offset = 0;
    slen = SCHEME_BYTE_STRLEN_VAL(argv[0]);
  }

  if ((slen != 4) && (slen != 8))
    scheme_contract_error("floating-point-bytes->real",
                          "length is not 2, 4, or 8 bytes",
                          "length", 1, scheme_make_integer(slen),
                          NULL);

  str = SCHEME_BYTE_STR_VAL(argv[0]);

  if (argc > 1)
    bigend = SCHEME_TRUEP(argv[1]);

  if (bigend != MZ_IS_BIG_ENDIAN) {
    int i;
    for (i = 0; i < slen; i++) {
      buf[slen - i - 1] = str[offset + i];
    }
  } else {
    memcpy(buf, str + offset, slen);
  }
  str = buf;

  switch(slen) {
  case 4:
    {
      float f;
      memcpy(&f, buf, sizeof(float));
#ifdef MZ_USE_SINGLE_FLOATS_AS_DEFAULT
      return scheme_make_float(f);
#else
      return scheme_make_double(f);
#endif
    }
    break;
  default:
    {
      double d;
      memcpy(&d, str, sizeof(double));
      return scheme_make_double(d);
    }
    break;
  }
}

static Scheme_Object *real_to_bytes (int argc, Scheme_Object *argv[])
{
  Scheme_Object *n, *s;
  int size;
  int bigend = MZ_IS_BIG_ENDIAN;
  double d;
  intptr_t offset = 0;

  n = argv[0];
  if (!SCHEME_REALP(n))
    scheme_wrong_contract("real->floating-point-bytes", "real?", 0, argc, argv);

  if (SCHEME_INTP(argv[1]))
    size = SCHEME_INT_VAL(argv[1]);
  else
    size = 0;
  if ((size != 4) && (size != 8))
    scheme_wrong_contract("real->floating-point-bytes", "(or/c 4 8)", 1, argc, argv);

  if (argc > 2)
    bigend = SCHEME_TRUEP(argv[2]);

  if (argc > 3) {
    s = argv[3];

    if (!SCHEME_MUTABLE_BYTE_STRINGP(s))
      scheme_wrong_contract("real->floating-point-bytes", "(and/c bytes? (not/c immutable?))", 3, argc, argv);
    
    if (argc > 4) {
      intptr_t start, finish;
      
      scheme_get_substring_indices("real->floating-point-bytes", s,
                                   argc, argv,
                                   4, 5, &start, &finish);
      
      offset = start;
    } else
      offset = 0;
  } else
    s = scheme_make_sized_byte_string("12345678", size, 1);
  
  if (offset + size > SCHEME_BYTE_STRLEN_VAL(s)) {
    scheme_contract_error("real->floating-point-bytes",
                          "byte string length is shorter than starting position plus size",
                          "byte string length", 1, scheme_make_integer(SCHEME_BYTE_STRLEN_VAL(s)),
                          "starting position", 1, scheme_make_integer(offset),
                          "size", 1, scheme_make_integer(size),
                          NULL);
    return NULL;
  }

  d = scheme_get_val_as_double(n);
  
  if (size == 4) {
    float f = (float) d;
    memcpy(SCHEME_BYTE_STR_VAL(s) + offset, &f, sizeof(float));
  } else {
    memcpy(SCHEME_BYTE_STR_VAL(s) + offset, &d, sizeof(double));
  }

  if (bigend != MZ_IS_BIG_ENDIAN) {
    int i;
    char buf[8], *str;

    str = SCHEME_BYTE_STR_VAL(s);
    
    for (i = 0; i < size; i++) {
      buf[size - i - 1] = str[offset + i];
    }
    for (i = 0; i < size; i++) {
      str[offset + i] = buf[i];
    }
  }

  return s;
}

static Scheme_Object *system_big_endian_p (int argc, Scheme_Object *argv[])
{
#if MZ_IS_BIG_ENDIAN
  return scheme_true;
#else
  return scheme_false;
#endif
}

/*========================================================================*/
/*                       random number generator                          */
/*========================================================================*/

#ifdef MZ_BSD_RANDOM_GENERATOR
# include "random.inc"
#else
# include "newrandom.inc"
#endif

intptr_t scheme_rand(Scheme_Random_State *rs)
{
  return sch_int_rand(2147483647, rs);
}

static Scheme_Object *
random_seed(int argc, Scheme_Object *argv[])
{
  intptr_t i = -1;
  Scheme_Object *o = argv[0], *rand_state;

  if (scheme_get_int_val(o,  &i)) {
    if (i > 2147483647)
      i = -1;
  }

  if (i < 0)
    scheme_wrong_contract("random-seed", "(integer-in 0 2147483647)", 0, argc, argv);

  rand_state = scheme_get_param(scheme_current_config(), MZCONFIG_RANDOM_STATE);
  sch_srand(i, (Scheme_Random_State *)rand_state);

  return scheme_void;
}

static Scheme_Object *
sch_random(int argc, Scheme_Object *argv[])
{
  if (!argc) {
    double v;
    Scheme_Object *rand_state;
    
    rand_state = scheme_get_param(scheme_current_config(), MZCONFIG_RANDOM_STATE);
    v = sch_double_rand((Scheme_Random_State *)rand_state);
    return scheme_make_double(v);
  } else if ((argc == 1)
             && SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_random_state_type)) {
    double v;
    Scheme_Object *rand_state;
    
    rand_state = argv[0];
    v = sch_double_rand((Scheme_Random_State *)rand_state);
    return scheme_make_double(v);
  } else {
    uintptr_t i, v;
    Scheme_Object *o, *rand_state;

    o = argv[0];
    if (scheme_get_unsigned_int_val(o,  &i)) {
      if (i > 4294967087UL)
	i = 0;
    } else
      i = 0;
    
    if (!i) {
      scheme_wrong_contract("random", 
                            ((argc == 1)
                             ? "(or/c (integer-in 1 4294967087) pseudo-random-generator?)"
                             : "(integer-in 1 4294967087)"), 
                            0, argc, argv);
      return NULL;
    }

    if (argc == 2) {
      rand_state = argv[1];
      if (!SAME_TYPE(SCHEME_TYPE(rand_state), scheme_random_state_type)) {
        scheme_wrong_contract("random", "pseudo-random-generator?", 1, argc, argv);
        return NULL;
      }
    } else {
      rand_state = scheme_get_param(scheme_current_config(), MZCONFIG_RANDOM_STATE);
    }

    v = sch_int_rand(i, (Scheme_Random_State *)rand_state);
    
    return scheme_make_integer_value_from_unsigned(v);
  }
}

static Scheme_Object *
do_pack(const char *name, int argc, Scheme_Object *argv[], int set, int check)
{
  Scheme_Object *s;
  GC_CAN_IGNORE Scheme_Random_State rs;

  if (set) {
    s = argv[0];
    if (!SAME_TYPE(SCHEME_TYPE(s), scheme_random_state_type)) {
      scheme_wrong_contract(name, "pseudo-random-generator?", 0, argc, argv);
    }
  }

  if (SCHEME_VECTORP(argv[set]) && (SCHEME_VEC_SIZE(argv[set]) == 6))
    s = pack_rand_state(argv[set], ((set || check) ? &rs : NULL));
  else
    s = NULL;

  if (check)
    return (s ? scheme_true : scheme_false);

  if (!s)
    scheme_wrong_contract(name,
                          "pseudo-random-generator-vector?",
                          set, argc, argv);

  if (set) {
    s = argv[0];
    ((Scheme_Random_State *)s)->x10 = rs.x10;
    ((Scheme_Random_State *)s)->x11 = rs.x11;
    ((Scheme_Random_State *)s)->x12 = rs.x12;
    ((Scheme_Random_State *)s)->x20 = rs.x20;
    ((Scheme_Random_State *)s)->x21 = rs.x21;
    ((Scheme_Random_State *)s)->x22 = rs.x22;

    return scheme_void;
  } else {
    return s;
  }
}

static Scheme_Object *
sch_pack(int argc, Scheme_Object *argv[])
{
  return do_pack("vector->pseudo-random-generator", argc, argv, 0, 0);
}

static Scheme_Object *
sch_pack_bang(int argc, Scheme_Object *argv[])
{
  return do_pack("vector->pseudo-random-generator!", argc, argv, 1, 0);
}

static Scheme_Object *
sch_check_pack(int argc, Scheme_Object *argv[])
{
  return do_pack("pseudo-random-generator-vector?", argc, argv, 0, 1);
}

static Scheme_Object *
sch_unpack(int argc, Scheme_Object *argv[])
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_random_state_type))
    scheme_wrong_contract("pseudo-random-generator->vector", "pseudo-random-generator?",
                          0, argc, argv);

  return unpack_rand_state((Scheme_Random_State *)argv[0]);
}

static Scheme_Object *current_pseudo_random_generator(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-pseudo-random-generator", 
			     scheme_make_integer(MZCONFIG_RANDOM_STATE),
			     argc, argv,
			     -1, pseudo_random_generator_p, "pseudo-random-generator", 0);
}

static Scheme_Object *current_sched_pseudo_random_generator(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-evt-pseudo-random-generator", 
			     scheme_make_integer(MZCONFIG_SCHEDULER_RANDOM_STATE),
			     argc, argv,
			     -1, pseudo_random_generator_p, "pseudo-random-generator", 0);
}

static Scheme_Object *make_pseudo_random_generator(int argc, Scheme_Object **argv)
{
  return scheme_make_random_state(scheme_get_milliseconds());
}

static Scheme_Object *pseudo_random_generator_p(int argc, Scheme_Object **argv)
{
  return ((SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_random_state_type)) 
	  ? scheme_true 
	  : scheme_false);
}

