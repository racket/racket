/*
  MzScheme
  Copyright (c) 2004-2005 PLT Scheme, Inc.
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
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

#include "schpriv.h"
#include "nummacs.h"
#include <math.h>

static Scheme_Object *plus (int argc, Scheme_Object *argv[]);
static Scheme_Object *minus (int argc, Scheme_Object *argv[]);
static Scheme_Object *mult (int argc, Scheme_Object *argv[]);
static Scheme_Object *div_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *quotient (int argc, Scheme_Object *argv[]);
static Scheme_Object *rem_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *quotient_remainder (int argc, Scheme_Object *argv[]);

#define zeroi scheme_exact_zero

void scheme_init_numarith(Scheme_Env *env)
{
  scheme_add_global_constant("add1", 
			     scheme_make_folding_prim(scheme_add1,
						      "add1",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("sub1", 
			     scheme_make_folding_prim(scheme_sub1,
						      "sub1",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("+", 
			     scheme_make_folding_prim(plus,
						      "+", 
						      0, -1, 1),
			     env);
  scheme_add_global_constant("-", 
			     scheme_make_folding_prim(minus,
						      "-",
						      1, -1, 1),
			     env);
  scheme_add_global_constant("*", 
			     scheme_make_folding_prim(mult,
						      "*", 
						      0, -1, 1),
			     env);
  scheme_add_global_constant("/", 
			     scheme_make_folding_prim(div_prim,
						      "/",
						      1, -1, 1),
			     env);
  scheme_add_global_constant("abs", 
			     scheme_make_folding_prim(scheme_abs,
						      "abs",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("quotient", 
			     scheme_make_folding_prim(quotient,
						      "quotient", 
						      2, 2, 1),
			     env);
  scheme_add_global_constant("remainder", 
			     scheme_make_folding_prim(rem_prim,
						      "remainder", 
						      2, 2, 1),
			     env);
  scheme_add_global_constant("quotient/remainder", 
			     scheme_make_prim_w_arity2(quotient_remainder,
						       "quotient/remainder", 
						       2, 2,
						       2, 2),
			     env);
  scheme_add_global_constant("modulo", 
			     scheme_make_folding_prim(scheme_modulo,
						      "modulo", 
						      2, 2, 1),
			     env);
}

Scheme_Object *
scheme_add1 (int argc, Scheme_Object *argv[])
{
  Scheme_Type t;
  Scheme_Object *o = argv[0];

  if (SCHEME_INTP(o)) {
    long v;
    v = SCHEME_INT_VAL(o);
    if (v < 0x3FFFFFFF)
      return scheme_make_integer(v + 1);
    else {
      Small_Bignum b;
      return scheme_bignum_add1(scheme_make_small_bignum(v, &b));
    }
  }
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type)
    return scheme_make_float(SCHEME_FLT_VAL(o) + 1.0f);
#endif
  if (t == scheme_double_type)
    return scheme_make_double(SCHEME_DBL_VAL(o) + 1.0);
  if (t == scheme_bignum_type)
    return scheme_bignum_add1(o);
  if (t == scheme_rational_type)
    return scheme_rational_add1(o);
  if ((t == scheme_complex_type) || (t == scheme_complex_izi_type))
    return scheme_complex_add1(o);

  NEED_NUMBER(add1);

  ESCAPED_BEFORE_HERE;
}

Scheme_Object *
scheme_sub1 (int argc, Scheme_Object *argv[])
{
  Scheme_Type t;
  Scheme_Object *o = argv[0];

  if (SCHEME_INTP(o)) {
    long v;
    v = SCHEME_INT_VAL(o);
    if (v > -(0x3FFFFFFF))
      return scheme_make_integer(SCHEME_INT_VAL(o) - 1);
    else {
      Small_Bignum b;
      return scheme_bignum_sub1(scheme_make_small_bignum(v, &b));
    }
  }
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type)
    return scheme_make_float(SCHEME_FLT_VAL(o) - 1.0f);
#endif
  if (t == scheme_double_type)
    return scheme_make_double(SCHEME_DBL_VAL(o) - 1.0);
  if (t == scheme_bignum_type)
    return scheme_bignum_sub1(o);
  if (t == scheme_rational_type)
    return scheme_rational_sub1(o);
  if ((t == scheme_complex_type) || (t == scheme_complex_izi_type))
    return scheme_complex_sub1(o);
  
  NEED_NUMBER(sub1);

  ESCAPED_BEFORE_HERE;
}

#define F_ADD(x,y) scheme_make_double(x + y)
#define F_SUBTRACT(x,y) scheme_make_double(x - y)
#define F_MULTIPLY(x,y) scheme_make_double(x * y)
#define DIVIDE(x,y) scheme_make_fixnum_rational(x, y)
#define F_DIVIDE(x,y) scheme_make_double((double)x / (double)y)

#define FS_ADD(x,y) scheme_make_float(x + y)
#define FS_SUBTRACT(x,y) scheme_make_float(x - y)
#define FS_MULTIPLY(x,y) scheme_make_float(x * y)
#define FS_DIVIDE(x,y) scheme_make_float((float)x / (float)y)

static Scheme_Object *ADD(long a, long b)
{
  long r;
  Scheme_Object *o;

  r = a + b;

  o = scheme_make_integer(r);
  r = SCHEME_INT_VAL(o);

  if (b == r - a)
    return o;
  else {
    Small_Bignum sa, sb;
    return scheme_bignum_add(scheme_make_small_bignum(a, &sa),
			     scheme_make_small_bignum(b, &sb));
  }
}

static Scheme_Object *SUBTRACT(long a, long b)
{
  long r;
  Scheme_Object *o;

  r = a - b;

  o = scheme_make_integer(r);
  r = SCHEME_INT_VAL(o);

  if (a == r + b)
    return o;
  else {
    Small_Bignum sa, sb;
    return scheme_bignum_subtract(scheme_make_small_bignum(a, &sa),
				  scheme_make_small_bignum(b, &sb));
  }
}

static Scheme_Object *MULTIPLY(long a, long b)
{
  long r;
  Scheme_Object *o;

  if (!b)
    return zeroi;

  r = a * b;

  o = scheme_make_integer(r);
  r = SCHEME_INT_VAL(o);

  if (a == r / b)
    return o;
  else {
    Small_Bignum sa, sb;
    return scheme_bignum_multiply(scheme_make_small_bignum(a, &sa),
				  scheme_make_small_bignum(b, &sb));
  }
}

GEN_BIN_OP(scheme_bin_plus, "+", ADD, F_ADD, FS_ADD, scheme_bignum_add, scheme_rational_add, scheme_complex_add, GEN_RETURN_N2, GEN_RETURN_N1, NO_NAN_CHECK, NO_NAN_CHECK)
GEN_BIN_OP(scheme_bin_minus, "-", SUBTRACT, F_SUBTRACT, FS_SUBTRACT, scheme_bignum_subtract, scheme_rational_subtract, scheme_complex_subtract, GEN_SINGLE_SUBTRACT_N2, GEN_RETURN_N1, NO_NAN_CHECK, NO_NAN_CHECK)
GEN_BIN_OP(scheme_bin_mult, "*", MULTIPLY, F_MULTIPLY, FS_MULTIPLY, scheme_bignum_multiply, scheme_rational_multiply, scheme_complex_multiply, GEN_RETURN_0, GEN_RETURN_0, NO_NAN_CHECK, NO_NAN_CHECK)
GEN_BIN_DIV_OP(scheme_bin_div, "/", DIVIDE, F_DIVIDE, FS_DIVIDE, scheme_make_rational, scheme_rational_divide, scheme_complex_divide)

GEN_NARY_OP(plus, "+", scheme_bin_plus, 0, SCHEME_NUMBERP, "number")
GEN_NARY_OP(mult, "*", scheme_bin_mult, 1, SCHEME_NUMBERP, "number")

static Scheme_Object *
minus (int argc, Scheme_Object *argv[])
{
  Scheme_Object *ret;
  int i;

  ret = argv[0];
  if (!SCHEME_NUMBERP(ret)) {
    scheme_wrong_type("-", "number", 0, argc, argv);
    ESCAPED_BEFORE_HERE;
  }
  if (argc == 1) {
    if (SCHEME_FLOATP(ret)) {
#ifdef MZ_USE_SINGLE_FLOATS
      if (SCHEME_FLTP(ret))
	return scheme_make_float(-SCHEME_FLT_VAL(ret));
#endif
      return scheme_make_double(-SCHEME_DBL_VAL(ret));
    }
    return scheme_bin_minus(zeroi, ret);
  }
  for (i = 1; i < argc; i++) {
    Scheme_Object *o = argv[i];
    if (!SCHEME_NUMBERP(o)) {
      scheme_wrong_type("-", "number", i, argc, argv);
      ESCAPED_BEFORE_HERE;
    }
    ret = scheme_bin_minus(ret, o);
  }
  return ret;
}

static Scheme_Object *
div_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *ret;
  int i;

  ret = argv[0];
  if (!SCHEME_NUMBERP(ret)) {
    scheme_wrong_type("/", "number", 0, argc, argv);
    ESCAPED_BEFORE_HERE;
  }
  if (argc == 1) {
    if (ret != zeroi)
      return scheme_bin_div(scheme_make_integer(1), ret);
    else {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT_DIVIDE_BY_ZERO,
		       "/: division by zero");
      ESCAPED_BEFORE_HERE;
    }
  }
  for (i = 1; i < argc; i++) {
    Scheme_Object *o = argv[i];

    if (!SCHEME_NUMBERP(o)) {
      scheme_wrong_type("/", "number", i, argc, argv);
      ESCAPED_BEFORE_HERE;
    }

    if (o != zeroi)
      ret = scheme_bin_div(ret, o);
    else {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT_DIVIDE_BY_ZERO,
		       "/: division by zero");
      ESCAPED_BEFORE_HERE;
    }
  }
  return ret;
}

#define ABS(n)  ((n>0) ? n : -n)

Scheme_Object *
scheme_abs(int argc, Scheme_Object *argv[])
{
  Scheme_Type t;
  Scheme_Object *o;

  o = argv[0];

  if (SCHEME_INTP(o)) {
    int n = SCHEME_INT_VAL(o);
    return scheme_make_integer_value(ABS(n));
  } 
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type)
    return scheme_make_float(fabs(SCHEME_FLT_VAL(o)));
#endif
  if (t == scheme_double_type)
    return scheme_make_double(fabs(SCHEME_DBL_VAL(o)));
  if (t == scheme_bignum_type) {
    if (SCHEME_BIGPOS(o))
      return o;
    return scheme_bignum_negate(o);
  }
  if (t == scheme_rational_type) {
    if (scheme_is_rational_positive(o))
      return o;
    else
      return scheme_rational_negate(o);
  }
  if (t == scheme_complex_izi_type) {
    Scheme_Object *r = IZI_REAL_PART(o);
    return scheme_abs(1, &r);
  }

  NEED_REAL(abs);

  ESCAPED_BEFORE_HERE;
}

Scheme_Object *
do_bin_quotient(const char *name, const Scheme_Object *n1, const Scheme_Object *n2, Scheme_Object **bn_rem)
{
  Scheme_Object *q;

  if (!scheme_is_integer(n1)) {
    Scheme_Object *a[2];
    a[0] = (Scheme_Object *)n1;
    a[1] = (Scheme_Object *)n2;
    scheme_wrong_type(name, "integer", 0, 2, a);
  }
  if (!scheme_is_integer(n2)) {
    Scheme_Object *a[2];
    a[0] = (Scheme_Object *)n1;
    a[1] = (Scheme_Object *)n2;
    scheme_wrong_type(name, "integer", 1, 2, a);
  }

  if (SCHEME_COMPLEX_IZIP(n1)) n1 = IZI_REAL_PART(n1);
  if (SCHEME_COMPLEX_IZIP(n2)) n2 = IZI_REAL_PART(n2);

  if (SCHEME_INTP(n2) && !SCHEME_INT_VAL(n2))
    scheme_raise_exn(MZEXN_FAIL_CONTRACT_DIVIDE_BY_ZERO,
		     "%s: undefined for 0", name);
  if (
#ifdef MZ_USE_SINGLE_FLOATS
      (SCHEME_FLTP(n2) && (SCHEME_FLT_VAL(n2) == 0.0f)) ||
#endif
      (SCHEME_DBLP(n2) && (SCHEME_DBL_VAL(n2) == 0.0)))
    scheme_raise_exn(MZEXN_FAIL_CONTRACT_DIVIDE_BY_ZERO,
		     "%s: undefined for 0.0", name);

  if (SCHEME_INTP(n1) && SCHEME_INTP(n2)) {
    return (scheme_make_integer (SCHEME_INT_VAL(n1) / SCHEME_INT_VAL(n2)));
  }
  if (SCHEME_DBLP(n1) || SCHEME_DBLP(n2)) {
    Scheme_Object *r;
    double d, d2;

    r = scheme_bin_div(n1, n2); /* could be exact 0 ... */
    if (SCHEME_DBLP(r)) {
      d = SCHEME_DBL_VAL(r);
      
      if (d > 0)
	d2 = floor(d);
      else
	d2 = ceil(d);
      
      if (d2 == d)
	return r;
      else
	return scheme_make_double(d2);
    } else
      return r;
  }
#ifdef MZ_USE_SINGLE_FLOATS
  if (SCHEME_FLTP(n1) || SCHEME_FLTP(n2)) {
    Scheme_Object *r;
    float d, d2;

    r = scheme_bin_div(n1, n2); /* could be exact 0 ... */
    if (SCHEME_FLTP(r)) {
      d = SCHEME_FLT_VAL(r);
      
      if (d > 0)
	d2 = floor(d);
      else
	d2 = ceil(d);
      
      if (d2 == d)
	return r;
      else
	return scheme_make_float(d2);
    } else
      return r;
  }
#endif

#if 0
  /* I'm pretty sure this isn't needed, but I'm keeping the code just
     in case... 03/19/2000 */
  if (SCHEME_RATIONALP(n1))
    WRONG_TYPE(name, "integer", n1);
  if (SCHEME_RATIONALP(n2))
    WRONG_TYPE(name, "integer", n2);
#endif
  
  n1 = scheme_to_bignum(n1);
  n2 = scheme_to_bignum(n2);

  scheme_bignum_divide(n1, n2, &q, bn_rem, 1);
  return q;
}

Scheme_Object *
scheme_bin_quotient (const Scheme_Object *n1, const Scheme_Object *n2)
{
  return do_bin_quotient("quotient", n1, n2, NULL);
}

static Scheme_Object *
quotient (int argc, Scheme_Object *argv[])
{
  return do_bin_quotient("quotient", argv[0], argv[1], NULL);
}

/* Declaration is for FARPROC: */
static Scheme_Object *
rem_mod (int argc, Scheme_Object *argv[], char *name, int first_sign);

static Scheme_Object *
rem_mod (int argc, Scheme_Object *argv[], char *name, int first_sign)
{
  Scheme_Object *n1, *n2, *r;
  int negate;

  n1 = argv[0];
  n2 = argv[1];

  if (!scheme_is_integer(n1))
    scheme_wrong_type(name, "integer", 0, argc, argv);
  if (!scheme_is_integer(n2))
    scheme_wrong_type(name, "integer", 1, argc, argv);

  if (SCHEME_COMPLEX_IZIP(n1)) n1 = IZI_REAL_PART(n1);
  if (SCHEME_COMPLEX_IZIP(n2)) n2 = IZI_REAL_PART(n2);

  if (SCHEME_INTP(n2) && !SCHEME_INT_VAL(n2))
    scheme_raise_exn(MZEXN_FAIL_CONTRACT_DIVIDE_BY_ZERO,
		     "%s: undefined for 0", name);
  if (
#ifdef MZ_USE_SINGLE_FLOATS
      (SCHEME_FLTP(n2) && (SCHEME_FLT_VAL(n2) == 0.0f)) ||
#endif
      (SCHEME_DBLP(n2) && (SCHEME_DBL_VAL(n2) == 0.0))) {
    int neg;
    neg = scheme_minus_zero_p(SCHEME_FLOAT_VAL(n2));
    scheme_raise_exn(MZEXN_FAIL_CONTRACT_DIVIDE_BY_ZERO,
		     "%s: undefined for %s0.0",
		     name,
		     neg ? "-" : "");
  }

  if (SCHEME_INTP(n1) && !SCHEME_INT_VAL(n1))
    return zeroi;

  if (SCHEME_INTP(n1) && SCHEME_INTP(n2)) {
    long a, b, na, nb, v;
    int neg1, neg2;

    a = SCHEME_INT_VAL(n1);
    b = SCHEME_INT_VAL(n2);
    na =  (a < 0) ? -a : a;
    nb =  (b < 0) ? -b : b;

    v = na % nb;

    if (v) {
      if (first_sign) {
	if (a < 0)
	  v = -v;
      } else {
	neg1 = (a < 0);
	neg2 = (b < 0);
	
	if (neg1 != neg2)
	  v = nb - v;
	
	if (neg2)
	  v = -v;
      }
    }

    return scheme_make_integer(v);
  }

  if (SCHEME_FLOATP(n1) || SCHEME_FLOATP(n2)) {
    double a, b, na, nb, v;
#ifdef MZ_USE_SINGLE_FLOATS
    int was_single = !(SCHEME_DBLP(n1) || SCHEME_DBLP(n2));
#endif

    if (SCHEME_INTP(n1))
      a = SCHEME_INT_VAL(n1);
#ifdef MZ_USE_SINGLE_FLOATS
    else if (SCHEME_FLTP(n1))
      a = SCHEME_FLT_VAL(n1);
#endif
    else if (SCHEME_DBLP(n1))
      a = SCHEME_DBL_VAL(n1);
    else
      a = scheme_bignum_to_double(n1);

    if (SCHEME_INTP(n2))
      b = SCHEME_INT_VAL(n2);
#ifdef MZ_USE_SINGLE_FLOATS
    else if (SCHEME_FLTP(n2))
      b = SCHEME_FLT_VAL(n2);
#endif
    else if (SCHEME_DBLP(n2))
      b = SCHEME_DBL_VAL(n2);
    else
      b = scheme_bignum_to_double(n2);

    if (a == 0.0) {
      /* Avoid sign problems. */
#ifdef MZ_USE_SINGLE_FLOATS
      if (was_single)
	return scheme_zerof;
#endif
      return scheme_zerod;
    }

    na =  (a < 0) ? -a : a;
    nb =  (b < 0) ? -b : b;

    if (MZ_IS_POS_INFINITY(nb))
      v = na;
    else if (MZ_IS_POS_INFINITY(na)) {
#ifdef MZ_USE_SINGLE_FLOATS
      if (was_single)
	return scheme_zerof;
#endif
      return scheme_zerod;
    } else {
      v = fmod(na, nb);

#ifdef FMOD_CAN_RETURN_NEG_ZERO
      if (v == 0.0)
	v = 0.0;
#endif
    }

    if (v) {
      if (first_sign) {
	if (a < 0)
	  v = -v;
      } else {
	int neg1, neg2;
	
	neg1 = (a < 0);
	neg2 = (b < 0);
	
	if (neg1 != neg2)
	  v = nb - v;
	
	if (neg2)
	  v = -v;
      }
    }

#ifdef MZ_USE_SINGLE_FLOATS
    if (was_single)
      return scheme_make_float((float)v);
#endif

    return scheme_make_double(v);
  }

  n1 = scheme_to_bignum(n1);
  n2 = scheme_to_bignum(n2);

  scheme_bignum_divide(n1, n2, NULL, &r, 1);

  negate = 0;

  if (!SCHEME_INTP(r) || SCHEME_INT_VAL(r)) {
    /* Easier if we can assume 'r' is positive: */
    if (SCHEME_INTP(r)) {
      if (SCHEME_INT_VAL(r) < 0)
	r = scheme_make_integer(-SCHEME_INT_VAL(r));
    } else if (!SCHEME_BIGPOS(r))
      r = scheme_bignum_negate(r);

    if (first_sign) {
      if (!SCHEME_BIGPOS(n1))
	negate = 1;
    } else {
      int neg1, neg2;
      
      neg1 = !SCHEME_BIGPOS(n1);
      neg2 = !SCHEME_BIGPOS(n2);
      
      if (neg1 != neg2) {
	if (neg2)
	  r = scheme_bin_plus(n2, r);
	else
	  r = scheme_bin_minus(n2, r);
      } else if (neg2)
	negate = 1;
    }
    
    if (negate) {
      if (SCHEME_INTP(r))
	r = scheme_make_integer(-SCHEME_INT_VAL(r));
      else
	r = scheme_bignum_negate(r);
    }
  }

  return r;
}

static Scheme_Object *
rem_prim (int argc, Scheme_Object *argv[])
{
  return rem_mod(argc, argv, "remainder", 1);
}

Scheme_Object *
scheme_modulo(int argc, Scheme_Object *argv[])
{
  return rem_mod(argc, argv, "modulo", 0);
}


Scheme_Object *
quotient_remainder(int argc, Scheme_Object *argv[])
{
  Scheme_Object *rem = NULL, *quot, *a[2];

  quot = do_bin_quotient("quotient/remainder", argv[0], argv[1], &rem);
  if (!rem) {
    rem = rem_mod(argc, argv, "remainder", 1);
  }
  a[0] = quot;
  a[1] = rem;
  return scheme_values(2, a);
}
