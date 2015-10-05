/*
  Racket
  Copyright (c) 2004-2015 PLT Design Inc.
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
#include <math.h>

READ_ONLY static Scheme_Object *one = scheme_make_integer(1);

static Scheme_Object *make_rational(const Scheme_Object *n, const Scheme_Object *d,
				    int normalize)
{
  Scheme_Rational *r;

  r = (Scheme_Rational *)scheme_malloc_small_dirty_tagged(sizeof(Scheme_Rational));
  r->so.type = scheme_rational_type;
  CLEAR_KEY_FIELD(&r->so);
  r->num = (Scheme_Object *)n;
  r->denom = (Scheme_Object *)d;
  
  return (normalize 
	  ? scheme_rational_normalize((Scheme_Object *)r) 
	  : (Scheme_Object *)r);
}

Scheme_Object *scheme_make_rational(const Scheme_Object *n, const Scheme_Object *d)
{
  return make_rational(scheme_bignum_normalize(n), 
		       scheme_bignum_normalize(d), 1);
}

Scheme_Object *scheme_integer_to_rational(const Scheme_Object *n)
{
  return make_rational(n, one, 0);
}

Scheme_Object *scheme_make_small_rational(intptr_t n, Small_Rational *s)
 XFORM_SKIP_PROC
{
  s->so.type = scheme_rational_type;
  s->num = scheme_make_integer(n);
  s->denom = one;

  return (Scheme_Object *)s;
}

Scheme_Object *scheme_make_small_bn_rational(Scheme_Object *n, Small_Rational *s)
  XFORM_SKIP_PROC
{
  s->so.type = scheme_rational_type;
  s->num = n;
  s->denom = one;

  return (Scheme_Object *)s;
}

int scheme_is_rational_positive(const Scheme_Object *o)
{
  Scheme_Rational *r = (Scheme_Rational *)o;

  if (SCHEME_INTP(r->num))
    return (SCHEME_INT_VAL(r->num) > 0);
  else 
    return SCHEME_BIGPOS(r->num);
}

Scheme_Object *scheme_rational_normalize(const Scheme_Object *o)
{
  Scheme_Rational *r = (Scheme_Rational *)o;
  Scheme_Object *gcd, *tmpn;
  int negate = 0;

  if (r->num == scheme_exact_zero)
    return scheme_make_integer(0);

  if (SCHEME_INTP(r->denom)) {
    if (SCHEME_INT_VAL(r->denom) < 0) {
      tmpn = scheme_make_integer_value(-SCHEME_INT_VAL(r->denom));
      r->denom = tmpn;
      negate = 1;
    }
  } else if (!SCHEME_BIGPOS(r->denom)) {
    tmpn = scheme_bignum_negate(r->denom);
    r->denom = tmpn;
    negate = 1;
  }

  if (negate) {
    if (SCHEME_INTP(r->num)) {
      tmpn = scheme_make_integer_value(-SCHEME_INT_VAL(r->num));
      r->num = tmpn;
    } else {
      tmpn = scheme_bignum_negate(r->num);
      r->num = tmpn;
    }
  }
  
  if (r->denom == one)
    return r->num;

  gcd = scheme_bin_gcd(r->num, r->denom);

  if (gcd == one)
    return (Scheme_Object *)o;

  tmpn = scheme_bin_quotient(r->num, gcd);
  r->num = tmpn;
  tmpn = scheme_bin_quotient(r->denom, gcd);
  r->denom = tmpn;

  if (r->denom == one)
    return r->num;

  return (Scheme_Object *)r;
}

Scheme_Object *scheme_rational_numerator(const Scheme_Object *n)
{
  return ((Scheme_Rational *)n)->num;
}

Scheme_Object *scheme_rational_denominator(const Scheme_Object *n)
{
  return ((Scheme_Rational *)n)->denom;
}

Scheme_Object *scheme_make_fixnum_rational(intptr_t n, intptr_t d)
{
  /* This function is called to implement division on small integers,
     so don't allocate unless necessary. */
  Small_Rational s;
  Scheme_Object *o;
  
  s.so.type = scheme_rational_type;
  s.num = scheme_make_integer(n);
  s.denom = scheme_make_integer(d);

  o = scheme_rational_normalize((Scheme_Object *)&s);
  if (o == (Scheme_Object *)&s)
    return make_rational(s.num, s.denom, 0);
  else
    return o;
}

int scheme_rational_eq(const Scheme_Object *a, const Scheme_Object *b)
{
  Scheme_Rational *ra = (Scheme_Rational *)a;
  Scheme_Rational *rb = (Scheme_Rational *)b;

  if (SCHEME_INTP(ra->num) && SCHEME_INTP(rb->num)) {
    if (ra->num != rb->num)
      return 0;
  } else if (SCHEME_BIGNUMP(ra->num) && SCHEME_BIGNUMP(rb->num)) {
    if (!scheme_bignum_eq(ra->num, rb->num))
      return 0;
  } else
    return 0;

  if (SCHEME_INTP(ra->denom) && SCHEME_INTP(rb->denom)) {
    if (ra->denom != rb->denom)
      return 0;
  } else if (SCHEME_BIGNUMP(ra->denom) && SCHEME_BIGNUMP(rb->denom)) {
    if (!scheme_bignum_eq(ra->denom, rb->denom))
      return 0;
  } else
    return 0;

  return 1;
}

static int rational_lt(const Scheme_Object *a, const Scheme_Object *b, int or_eq)
{
  Scheme_Rational *ra = (Scheme_Rational *)a;
  Scheme_Rational *rb = (Scheme_Rational *)b;
  Scheme_Object *ma, *mb;

  ma = scheme_bin_mult(ra->num, rb->denom);
  mb = scheme_bin_mult(rb->num, ra->denom);

  if (SCHEME_INTP(ma) && SCHEME_INTP(mb)) {
    if (or_eq)
      return (SCHEME_INT_VAL(ma) <= SCHEME_INT_VAL(mb));
    else
      return (SCHEME_INT_VAL(ma) < SCHEME_INT_VAL(mb));
  } else if (SCHEME_BIGNUMP(ma) && SCHEME_BIGNUMP(mb)) {
    if (or_eq)
      return scheme_bignum_le(ma, mb);
    else
      return scheme_bignum_lt(ma, mb);
  } else if (SCHEME_BIGNUMP(mb)) {
    return SCHEME_BIGPOS(mb);
  } else
    return !SCHEME_BIGPOS(ma);
}

int scheme_rational_lt(const Scheme_Object *a, const Scheme_Object *b)
{
  return rational_lt(a, b, 0);
}

int scheme_rational_gt(const Scheme_Object *a, const Scheme_Object *b)
{
  return !rational_lt(a, b, 1);
}

int scheme_rational_le(const Scheme_Object *a, const Scheme_Object *b)
{
  return rational_lt(a, b, 1);
}

int scheme_rational_ge(const Scheme_Object *a, const Scheme_Object *b)
{
  return !rational_lt(a, b, 0);
}

Scheme_Object *scheme_rational_negate(const Scheme_Object *o)
{
  Scheme_Rational *r = (Scheme_Rational *)o;

  return make_rational(scheme_bin_minus(scheme_make_integer(0),
					r->num), 
		       r->denom, 0);
}

Scheme_Object *scheme_rational_add(const Scheme_Object *a, const Scheme_Object *b)
{
  Scheme_Rational *ra = (Scheme_Rational *)a;
  Scheme_Rational *rb = (Scheme_Rational *)b;
  Scheme_Object *ac, *bd, *sum, *cd;
  int no_normalize = 0;

  if (SCHEME_INTP(ra->denom) && (SCHEME_INT_VAL(ra->denom) == 1)) {
    /* Swap, to take advantage of the next optimization */
    Scheme_Rational *rx = ra;
    ra = rb;
    rb = rx;
  }
  if (SCHEME_INTP(rb->denom) && (SCHEME_INT_VAL(rb->denom) == 1)) {
    /* From Brad Lucier: */
    /*    (+ p/q n) = (make-rational (+ p (* n q)) q), no normalize */
    ac = ra->num;
    cd = ra->denom;
    no_normalize = 1;
  } else {
    ac = scheme_bin_mult(ra->num, rb->denom);
    cd = scheme_bin_mult(ra->denom, rb->denom);
  }

  bd = scheme_bin_mult(ra->denom, rb->num);
  sum = scheme_bin_plus(ac, bd);

  if (no_normalize)
    return make_rational(sum, cd, 0);
  else
    return scheme_make_rational(sum, cd);
}

Scheme_Object *scheme_rational_subtract(const Scheme_Object *a, const Scheme_Object *b)
{
  return scheme_rational_add(a, scheme_rational_negate(b));
}

Scheme_Object *scheme_rational_add1(const Scheme_Object *n)
{
  Small_Rational s;

  return scheme_rational_add(scheme_make_small_rational(1, &s), n);
}

Scheme_Object *scheme_rational_sub1(const Scheme_Object *n)
{
  Small_Rational s;

  return scheme_rational_add(n, scheme_make_small_rational(-1, &s));
}

Scheme_Object *scheme_rational_multiply(const Scheme_Object *a, const Scheme_Object *b)
{
  Scheme_Rational *ra = (Scheme_Rational *)a;
  Scheme_Rational *rb = (Scheme_Rational *)b;
  Scheme_Object *gcd_ps, *gcd_rq, *p_, *r_, *q_, *s_;

  /* From Brad Lucier: */
  /* (* p/q r/s) => (make-rational (* (quotient p (gcd p s))
                                      (quotient r (gcd r q)))
                                   (* (quotient q (gcd r q))
                                      (quotient s (gcd p s)))) */
  
  gcd_ps = scheme_bin_gcd(ra->num, rb->denom);
  gcd_rq = scheme_bin_gcd(rb->num, ra->denom);

  p_ = scheme_bin_quotient(ra->num, gcd_ps);
  r_ = scheme_bin_quotient(rb->num, gcd_rq);

  q_ = scheme_bin_quotient(ra->denom, gcd_rq);
  s_ = scheme_bin_quotient(rb->denom, gcd_ps);

  p_ = scheme_bin_mult(p_, r_);
  q_ = scheme_bin_mult(q_, s_);

  return scheme_make_rational(p_, q_);
}

Scheme_Object *scheme_rational_max(const Scheme_Object *a, const Scheme_Object *b)
{
  int lt;
  lt = scheme_rational_lt(a, b);
  return scheme_rational_normalize(lt ? b : a);
}

Scheme_Object *scheme_rational_min(const Scheme_Object *a, const Scheme_Object *b)
{
  int lt;
  lt = scheme_rational_lt(a, b);
  return scheme_rational_normalize(lt ? a : b);
}

static Scheme_Object *negate_simple(Scheme_Object *v)
{
  if (SCHEME_INTP(v))
    return scheme_make_integer_value(-SCHEME_INT_VAL(v));
  else
    return scheme_bignum_negate(v);
}

Scheme_Object *scheme_rational_divide(const Scheme_Object *n, const Scheme_Object *d)
{ 
  Scheme_Rational *rd = (Scheme_Rational *)d, *rn = (Scheme_Rational *)n;
  Scheme_Rational d_inv;

  /* Check for [negative] inverse, which is easy */
  if ((SCHEME_INTP(rn->num) && ((SCHEME_INT_VAL(rn->num) == 1)
				|| (SCHEME_INT_VAL(rn->num) == -1)))
      && (SCHEME_INTP(rn->denom) && SCHEME_INT_VAL(rn->denom) == 1)) {
    int negate = (SCHEME_INT_VAL(rn->num) == -1);
    if (SCHEME_INTP(rd->num)) {
      if ((SCHEME_INT_VAL(rd->num) == 1)) {
	if (negate)
	  return negate_simple(rd->denom);
	else
	  return rd->denom;
      }
      if (SCHEME_INT_VAL(rd->num) == -1) {
	if (negate)
	  return rd->denom;
	else
	  return negate_simple(rd->denom);
      }
    }
    if (((SCHEME_INTP(rd->num))
	 && (SCHEME_INT_VAL(rd->num) < 0))
	|| (!SCHEME_INTP(rd->num)
	    && !SCHEME_BIGPOS(rd->num))) {
      Scheme_Object *v;
      v = negate ? rd->denom : negate_simple(rd->denom);
      return make_rational(v, negate_simple(rd->num), 0);
    } else {
      Scheme_Object *v;
      v = negate ? negate_simple(rd->denom) : rd->denom;
      return make_rational(v, rd->num, 0);
    }
  }
  
  d_inv.so.type = scheme_rational_type;
  d_inv.denom = rd->num;
  d_inv.num = rd->denom;

  return scheme_rational_multiply(n, (Scheme_Object *)&d_inv);
}

Scheme_Object *scheme_rational_power(const Scheme_Object *o, const Scheme_Object *p)
{
  double b, e, v;

  if (((Scheme_Rational *)p)->denom == one) {
    Scheme_Object *a[2], *n;
    a[0] = ((Scheme_Rational *)o)->num;
    a[1] = ((Scheme_Rational *)p)->num;
    n = scheme_expt(2, a);
    a[0] = ((Scheme_Rational *)o)->denom;
    return make_rational(n, scheme_expt(2, a), 0);
  }

  if (scheme_is_rational_positive(o)) {
    b = scheme_rational_to_double(o);
    e = scheme_rational_to_double(p);

    v = pow(b, e);

#ifdef USE_SINGLE_FLOATS_AS_DEFAULT
    return scheme_make_float(v);
#else
    return scheme_make_double(v);
#endif
  } else {
    return scheme_complex_power(scheme_real_to_complex(o),
				scheme_real_to_complex(p));
  }
}

Scheme_Object *scheme_rational_truncate(const Scheme_Object *o)
{
  Scheme_Rational *r = (Scheme_Rational *)o;

  return scheme_bin_quotient(r->num, r->denom);
}

Scheme_Object *scheme_rational_floor(const Scheme_Object *o)
{
  if (scheme_is_rational_positive(o))
    return scheme_rational_truncate(o);
  else {
    Scheme_Object *r;
    r = scheme_rational_truncate(o);
    return scheme_sub1(1, &r);
  }
}

Scheme_Object *scheme_rational_ceiling(const Scheme_Object *o)
{
  if (!scheme_is_rational_positive(o))
    return scheme_rational_truncate(o);
  else {
    Scheme_Object *r;
    r = scheme_rational_truncate(o);
    return scheme_add1(1, &r);
  }  
}

Scheme_Object *scheme_rational_round(const Scheme_Object *o)
{
  Scheme_Rational *r = (Scheme_Rational *)o;
  Scheme_Object *q, *qd, *delta, *half;
  int more = 0, can_eq_half, negative;

  negative = !scheme_is_rational_positive(o);
  
  q = scheme_bin_quotient(r->num, r->denom);

  /* Get remainder absolute value: */
  qd = scheme_bin_mult(q, r->denom);
  if (negative)
    delta = scheme_bin_minus(qd, r->num);
  else
    delta = scheme_bin_minus(r->num, qd);

  half = scheme_bin_quotient(r->denom, scheme_make_integer(2));
  can_eq_half = SCHEME_FALSEP(scheme_odd_p(1, &r->denom));

  if (SCHEME_INTP(half) && SCHEME_INTP(delta)) {
    if (can_eq_half && (SCHEME_INT_VAL(delta) == SCHEME_INT_VAL(half)))
      more = SCHEME_TRUEP(scheme_odd_p(1, &q));
    else
      more = (SCHEME_INT_VAL(delta) > SCHEME_INT_VAL(half));
  } else if (SCHEME_BIGNUMP(delta) && SCHEME_BIGNUMP(half)) {
    if (can_eq_half && (scheme_bignum_eq(delta, half)))
      more = SCHEME_TRUEP(scheme_odd_p(1, &q));      
    else
      more = !scheme_bignum_lt(delta, half);
  } else
    more = SCHEME_BIGNUMP(delta);

  if (more) {
    if (negative)
      q = scheme_sub1(1, &q);
    else
      q = scheme_add1(1, &q);      
  }

  return q;
}


Scheme_Object *scheme_rational_sqrt(const Scheme_Object *o)
{
  Scheme_Rational *r = (Scheme_Rational *)o;
  Scheme_Object *n, *d;
  double v;

  n = scheme_integer_sqrt(r->num);
  if (!SCHEME_DBLP(n)) {
    d = scheme_integer_sqrt(r->denom);
    if (!SCHEME_DBLP(d))
      return make_rational(n, d, 0);
  }

  v = sqrt(scheme_rational_to_double(o));

#ifdef USE_SINGLE_FLOATS_AS_DEFAULT
  return scheme_make_float(v);
#else
  return scheme_make_double(v);
#endif
}

#ifndef FLOATING_POINT_IS_NOT_IEEE
# define DECODE_IEEE_FLOATING_POINT
#endif

#define FP_TYPE double
#define FP_MULT(x, y) x*y
#define FP_DIV(x, y) x/y
#define FP_NEG(x) -x
#define FP_EQV(x,y) x==y
#define FP_LESS(x, y) x<y
#define FP_IS_ZERO(x) x==0.0
#define FP_TYPE_FROM_INT(x) ((FP_TYPE)(x))
#define FP_TYPE_FROM_INTPTR(x) ((FP_TYPE)(x))
#ifdef SIXTY_FOUR_BIT_INTEGERS
# define FIXNUM_FITS_FP(x) (!(SCHEME_INT_VAL(x) & ~(((intptr_t)1 << (FLOAT_M_BITS-1)) - 1)))
# define BIGNUM_FITS_FP(x) 0
# define FP_FITS_INT_TYPE uintptr_t
#else
# define FIXNUM_FITS_FP(x) 1
# define BIGNUM_FITS_FP(x) (scheme_integer_length(x) <= (FLOAT_M_BITS-1))
#endif
#define SCHEME_RATIONAL_TO_FLOAT scheme_rational_to_double
#define SCHEME_RATIONAL_FROM_FLOAT scheme_rational_from_double
#define SCHEME_BIGNUM_TO_FLOAT_INF_INFO scheme_bignum_to_double_inf_info
#define SCHEME_CHECK_FLOAT scheme_check_double
#define SCHEME_BIGNUM_FROM_FLOAT scheme_bignum_from_double
#define DO_FLOAT_DIV scheme__do_double_div
#define FLOAT_E_MIN (-1074)
#define FLOAT_M_BITS 52
#define FLOAT_E_BITS 11
#include "ratfloat.inc"

#ifdef MZ_USE_SINGLE_FLOATS
#define FP_TYPE float
#define FP_MULT(x, y) x*y
#define FP_DIV(x, y) x/y
#define FP_NEG(x) -x
#define FP_EQV(x,y) x==y
#define FP_LESS(x, y) x<y
#define FP_TYPE_FROM_INT(x) ((FP_TYPE)(x))
#define FP_TYPE_FROM_INTPTR(x) ((FP_TYPE)(x))
#define FIXNUM_FITS_FP(x) (!(SCHEME_INT_VAL(x) & ~(((intptr_t)1 << (FLOAT_M_BITS-1)) - 1)))
#define FP_IS_ZERO(x) x==0.0
#define BIGNUM_FITS_FP(x) 0
#define FP_FITS_INT_TYPE unsigned int
#define SCHEME_RATIONAL_TO_FLOAT scheme_rational_to_float
#define SCHEME_RATIONAL_FROM_FLOAT scheme_rational_from_float
#define SCHEME_BIGNUM_TO_FLOAT_INF_INFO scheme_bignum_to_float_inf_info
#define SCHEME_CHECK_FLOAT scheme_check_float
#define SCHEME_BIGNUM_FROM_FLOAT scheme_bignum_from_float
#define DO_FLOAT_DIV scheme__do_float_div
#define FLOAT_E_MIN (-127)
#define FLOAT_M_BITS 23
#define FLOAT_E_BITS 8
#include "ratfloat.inc"
#endif

/* scheme_bytes_to_integer() can't real with sizeof(long_double): */
#undef DECODE_IEEE_FLOATING_POINT

#ifdef MZ_LONG_DOUBLE
# define FP_TYPE long_double
# define FP_MULT(x, y) long_double_mult(x,y)
# define FP_DIV(x, y) long_double_div(x,y)
# define FP_NEG(x) long_double_neg(x)
# define FP_EQV(x,y) long_double_eqv(x,y)
# define FP_LESS(x, y) long_double_less(x,y)
# define FP_TYPE_FROM_INT(x) long_double_from_int(x)
# define FP_TYPE_FROM_INTPTR(x) long_double_from_intptr(x)
# define FIXNUM_FITS_FP(x) 1
# define BIGNUM_FITS_FP(x) (scheme_integer_length(x) <= (FLOAT_M_BITS-1))
# define FP_IS_ZERO(x) long_double_is_zero(x)
# define SCHEME_RATIONAL_TO_FLOAT scheme_rational_to_long_double
# define SCHEME_RATIONAL_FROM_FLOAT scheme_rational_from_long_double
# define SCHEME_BIGNUM_TO_FLOAT_INF_INFO scheme_bignum_to_long_double_inf_info
# define SCHEME_CHECK_FLOAT scheme_check_long_double
# define SCHEME_BIGNUM_FROM_FLOAT scheme_bignum_from_long_double
# define FLOAT_E_MIN (-16383)
# define FLOAT_M_BITS 64
# define FLOAT_E_BITS 15
# define FP_ZEROx get_long_double_zero()
# define FP_POWx long_double_pow
# define FP_MODFx long_double_modf
# define FP_FREXPx long_double_frexp
# define FP_LDEXP long_double_ldexp
# define FP_DOUBLE_TYPE FP_TYPE
#include "ratfloat.inc"
#endif
