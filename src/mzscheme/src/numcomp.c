/*
  MzScheme
  Copyright (c) 2004-2007 PLT Scheme Inc.
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

#include "schpriv.h"
#include "nummacs.h"
#include <math.h>

static Scheme_Object *eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *lt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *gt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *zero_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *positive_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *negative_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_max (int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_min (int argc, Scheme_Object *argv[]);

#define zeroi scheme_exact_zero

void scheme_init_numcomp(Scheme_Env *env)
{
  Scheme_Object *p;

  p = scheme_make_folding_prim(eq, "=", 2, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  scheme_add_global_constant("=", p, env);

  p = scheme_make_folding_prim(lt, "<", 2, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  scheme_add_global_constant("<", p, env);

  p = scheme_make_folding_prim(gt, ">", 2, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  scheme_add_global_constant(">", p, env);

  p = scheme_make_folding_prim(lt_eq, "<=", 2, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  scheme_add_global_constant("<=", p, env);

  p = scheme_make_folding_prim(gt_eq, ">=", 2, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  scheme_add_global_constant(">=", p, env);

  p = scheme_make_folding_prim(zero_p, "zero?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("zero?", p, env);

  p = scheme_make_folding_prim(positive_p, "positive?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("positive?", p, env);

  p = scheme_make_folding_prim(negative_p, "negative?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("negative?", p, env);

  p = scheme_make_folding_prim(sch_max, "max", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  scheme_add_global_constant("max", p, env);

  p = scheme_make_folding_prim(sch_min, "min", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  scheme_add_global_constant("min", p, env);
}

/* Prototype needed for 3m conversion: */
static MZ_INLINE Scheme_Object *force_rat(Scheme_Object *n, Small_Rational *sr);

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif

static MZ_INLINE Scheme_Object *force_rat(Scheme_Object *n, Small_Rational *sr)
{
  Scheme_Type t = SCHEME_TYPE(n);
  if (t == scheme_rational_type)
    return n;
  else
    return scheme_make_small_bn_rational(n, sr);
}

#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

GEN_NARY_COMP(eq, "=", scheme_bin_eq, SCHEME_NUMBERP, "number")
GEN_NARY_COMP(lt, "<", scheme_bin_lt, SCHEME_REALP, REAL_NUMBER_STR)
GEN_NARY_COMP(gt, ">", scheme_bin_gt, SCHEME_REALP, REAL_NUMBER_STR)
GEN_NARY_COMP(lt_eq, "<=", scheme_bin_lt_eq, SCHEME_REALP, REAL_NUMBER_STR)
GEN_NARY_COMP(gt_eq, ">=", scheme_bin_gt_eq, SCHEME_REALP, REAL_NUMBER_STR)

#define EQUAL(x, y) (x == y)
#define LESS_THAN(x, y) (x < y)
#define GREATER_THAN(x, y) (x > y)
#define LESS_OR_EQUAL(x, y) (x <= y)
#define GREATER_OR_EQUAL(x, y) (x >= y)

#ifdef NAN_LT_COMPARISON_WRONG
# define fLESS_THAN(x, y) (!(x >= y) && (x == x) && (y == y))
# define fLESS_OR_EQUAL(x, y) (!(x > y) && (x == x) && (y == y))
#else
# define fLESS_THAN LESS_THAN
# define fLESS_OR_EQUAL LESS_OR_EQUAL
#endif

#define COMP_IZI_LT(a, b) scheme_bin_lt(IZI_REAL_PART(a), IZI_REAL_PART(b))
#define COMP_IZI_GT(a, b) scheme_bin_gt(IZI_REAL_PART(a), IZI_REAL_PART(b))
#define COMP_IZI_LT_EQ(a, b) scheme_bin_lt_eq(IZI_REAL_PART(a), IZI_REAL_PART(b))
#define COMP_IZI_GT_EQ(a, b) scheme_bin_gt_eq(IZI_REAL_PART(a), IZI_REAL_PART(b))

#define GEN_IDENT_FOR_IZI GEN_IDENT

GEN_BIN_COMP(scheme_bin_eq, "=", EQUAL, EQUAL, scheme_bignum_eq, scheme_rational_eq, scheme_complex_eq, 0, 0, scheme_is_inexact, scheme_is_inexact, GEN_IDENT, GEN_IDENT, "number")
GEN_BIN_COMP(scheme_bin_lt, "<", LESS_THAN, fLESS_THAN, scheme_bignum_lt, scheme_rational_lt, COMP_IZI_LT, 0, 1, scheme_is_positive, scheme_is_negative, GEN_IDENT_FOR_IZI, GEN_OMIT, REAL_NUMBER_STR)
GEN_BIN_COMP(scheme_bin_gt, ">", GREATER_THAN, GREATER_THAN, scheme_bignum_gt, scheme_rational_gt, COMP_IZI_GT, 1, 0, scheme_is_negative, scheme_is_positive, GEN_IDENT_FOR_IZI, GEN_OMIT, REAL_NUMBER_STR)
GEN_BIN_COMP(scheme_bin_lt_eq, "<=", LESS_OR_EQUAL, fLESS_OR_EQUAL, scheme_bignum_le, scheme_rational_le, COMP_IZI_LT_EQ, 0, 1, scheme_is_positive, scheme_is_negative, GEN_IDENT_FOR_IZI, GEN_OMIT, REAL_NUMBER_STR)
GEN_BIN_COMP(scheme_bin_gt_eq, ">=", GREATER_OR_EQUAL, GREATER_OR_EQUAL, scheme_bignum_ge, scheme_rational_ge, COMP_IZI_GT_EQ, 1, 0, scheme_is_negative, scheme_is_positive, GEN_IDENT_FOR_IZI, GEN_OMIT, REAL_NUMBER_STR)

int
scheme_is_zero(const Scheme_Object *o)
{
  Scheme_Type t;

 top:

  if (SCHEME_INTP(o))
    return o == zeroi;
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type) {
# ifdef NAN_EQUALS_ANYTHING
    if (MZ_IS_NAN(SCHEME_FLT_VAL(o)))
      return 0;
# endif
    return SCHEME_FLT_VAL(o) == 0.0f;
  }
#endif
  if (t == scheme_double_type) {
#ifdef NAN_EQUALS_ANYTHING
    if (MZ_IS_NAN(SCHEME_DBL_VAL(o)))
      return 0;
#endif
    return SCHEME_DBL_VAL(o) == 0.0;
  }

  if (t == scheme_complex_izi_type) {
    o = IZI_REAL_PART(o);
    goto top;
  }

  if ((t >= scheme_bignum_type) && (t <= scheme_complex_type))
    return 0;
 
  return -1;
}

Scheme_Object *
zero_p (int argc, Scheme_Object *argv[])
{
  int v;
  v = scheme_is_zero(argv[0]);
  if (v < 0) {
    NEED_REAL(zero?);
    ESCAPED_BEFORE_HERE;
  }
  return (v ? scheme_true : scheme_false);
}

int
scheme_is_positive(const Scheme_Object *o)
{
  Scheme_Type t;

 top:

  if (SCHEME_INTP(o))
    return SCHEME_INT_VAL(o) > 0;
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type) {
    float d = SCHEME_FLT_VAL(o);
# ifdef NAN_EQUALS_ANYTHING
    if (MZ_IS_NAN(d))
      return 0;
# endif
    return d > 0;
  }
#endif
  if (t == scheme_double_type) {
    double d = SCHEME_DBL_VAL(o);
#ifdef NAN_EQUALS_ANYTHING
    if (MZ_IS_NAN(d))
      return 0;
#endif
    return d > 0;
  }
  if (t == scheme_bignum_type)
    return SCHEME_BIGPOS(o);
  if (t == scheme_rational_type)
    return scheme_is_rational_positive(o);
  if (t == scheme_complex_izi_type) {
    o = IZI_REAL_PART(o);
    goto top;
  }

  return -1;
}

Scheme_Object *
positive_p (int argc, Scheme_Object *argv[])
{
  int v;
  v = scheme_is_positive(argv[0]);
  if (v < 0) {
    NEED_REAL(positive?);
    ESCAPED_BEFORE_HERE;
  }
  return (v ? scheme_true : scheme_false);
}

int
scheme_is_negative(const Scheme_Object *o)
{
  Scheme_Type t;

 top:

  if (SCHEME_INTP(o))
    return SCHEME_INT_VAL(o) < 0;
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type) {
    float d = SCHEME_FLT_VAL(o);
# if defined(NAN_EQUALS_ANYTHING) || defined(NAN_LT_COMPARISON_WRONG)
    if (MZ_IS_NAN(d))
      return 0;
# endif
    return d < 0;
  }
#endif
  if (t == scheme_double_type) {
    double d = SCHEME_DBL_VAL(o);
# if defined(NAN_EQUALS_ANYTHING) || defined(NAN_LT_COMPARISON_WRONG)
    if (MZ_IS_NAN(d))
      return 0;
#endif
    return d < 0;
  }
  if (t == scheme_bignum_type)
    return !SCHEME_BIGPOS(o);
  if (t == scheme_rational_type)
    return !scheme_is_rational_positive(o);
  if (t == scheme_complex_izi_type) {
    o = IZI_REAL_PART(o);
    goto top;
  }

  return -1;
}

Scheme_Object *
negative_p (int argc, Scheme_Object *argv[])
{
  int v;
  v = scheme_is_negative(argv[0]);
  if (v < 0) {
    NEED_REAL(negative?);
    ESCAPED_BEFORE_HERE;
  }
  return (v ? scheme_true : scheme_false);
}

#define MAX(n1,n2) scheme_make_integer((n1>n2) ? n1 : n2)
#define MIN(n1,n2) scheme_make_integer((n1<n2) ? n1 : n2)
#define F_MAX(n1,n2) scheme_make_double((n1>n2) ? n1 : n2)
#define F_MIN(n1,n2) scheme_make_double((n1<n2) ? n1 : n2)

#define FS_MAX(n1,n2) scheme_make_float((n1>n2) ? n1 : n2)
#define FS_MIN(n1,n2) scheme_make_float((n1<n2) ? n1 : n2)

#define MAX_IZI(a, b) bin_max(IZI_REAL_PART(a), IZI_REAL_PART(b))
#define MIN_IZI(a, b) bin_min(IZI_REAL_PART(a), IZI_REAL_PART(b))

static GEN_BIN_OP(bin_max, "max", MAX, F_MAX, FS_MAX, scheme_bignum_max, scheme_rational_max, MAX_IZI, GEN_OMIT, GEN_OMIT, NAN_RETURNS_NAN, NAN_RETURNS_SNAN)
static GEN_BIN_OP(bin_min, "min", MIN, F_MIN, FS_MIN, scheme_bignum_min, scheme_rational_min, MIN_IZI, GEN_OMIT, GEN_OMIT, NAN_RETURNS_NAN, NAN_RETURNS_SNAN)

GEN_TWOARY_OP(static, sch_max, "max", bin_max, SCHEME_REALP, REAL_NUMBER_STR)
GEN_TWOARY_OP(static, sch_min, "min", bin_min, SCHEME_REALP, REAL_NUMBER_STR)
