/*
  Racket
  Copyright (c) 2004-2016 PLT Design Inc.
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

#define zero scheme_exact_zero

static Scheme_Object *make_complex(const Scheme_Object *r, const Scheme_Object *i,
				   int normalize)
{
  Scheme_Complex *c;

  c = (Scheme_Complex *)scheme_malloc_small_dirty_tagged(sizeof(Scheme_Complex));
  CLEAR_KEY_FIELD(&c->so);
  c->so.type = scheme_complex_type;
  c->r = (Scheme_Object *)r;
  c->i = (Scheme_Object *)i;

  if (normalize)
    return scheme_complex_normalize((Scheme_Object *)c);
  else
    return (Scheme_Object *)c;
}

Scheme_Object *scheme_make_complex(const Scheme_Object *r, const Scheme_Object *i)
{
  return make_complex(r, i, 1);
}

Scheme_Object *scheme_real_to_complex(const Scheme_Object *n)
{
  return make_complex(n, zero, 0);
}

Scheme_Object *scheme_make_small_complex(const Scheme_Object *n, Small_Complex *s)
  XFORM_SKIP_PROC
{
  s->so.type = scheme_complex_type;
  s->r = (Scheme_Object *)n;
  s->i = zero;

  return (Scheme_Object *)s;
}

int scheme_is_complex_exact(const Scheme_Object *o)
{
  Scheme_Complex *c = (Scheme_Complex *)o;

  return !SCHEME_FLOATP(c->r) && !SCHEME_FLOATP(c->i);
}

Scheme_Object *scheme_complex_normalize(const Scheme_Object *o)
{
  Scheme_Complex *c = (Scheme_Complex *)o;

  if (c->i == zero)
    return c->r;
  if (c->r == zero) {
    /* No coercions */
    return (Scheme_Object *)c; 
  }

  /* Coercions: Exact -> float -> double
     If the complex contains a float and an exact, we coerce the exact
     to a float, etc. */

#ifdef MZ_USE_SINGLE_FLOATS
  if (SCHEME_FLTP(c->i)) {
    if (!SCHEME_FLTP(c->r)) {
      Scheme_Object *v;
      if (SCHEME_DBLP(c->r)) {
        v = scheme_make_double(SCHEME_FLT_VAL(c->i));
        c->i = v;
      } else {
        v = scheme_make_float(scheme_get_val_as_float(c->r));
	c->r = v;
      }
    }
  } else if (SCHEME_FLTP(c->r)) {
    Scheme_Object *v;
    /* Imag part can't be a float, or we'd be in the previous case */
    if (SCHEME_DBLP(c->i)) {
      v = scheme_make_double(SCHEME_FLT_VAL(c->r));
      c->r = v;
    } else {
      v = scheme_make_float(scheme_get_val_as_float(c->i));
      c->i = v;
    }
  } else
#endif

  if (SCHEME_DBLP(c->i)) {
    if (!SCHEME_DBLP(c->r)) {
      Scheme_Object *r;
      r = scheme_make_double(scheme_get_val_as_double(c->r));
      c->r = r;
    }
  } else if (SCHEME_DBLP(c->r)) {
    Scheme_Object *i;
    i = scheme_make_double(scheme_get_val_as_double(c->i));
    c->i = i;
  }

  return (Scheme_Object *)c;
}

Scheme_Object *scheme_complex_real_part(const Scheme_Object *n)
{
  return ((Scheme_Complex *)n)->r;
}

Scheme_Object *scheme_complex_imaginary_part(const Scheme_Object *n)
{
  return ((Scheme_Complex *)n)->i;
}

int scheme_complex_eq(const Scheme_Object *a, const Scheme_Object *b)
{
  Scheme_Complex *ca = (Scheme_Complex *)a;
  Scheme_Complex *cb = (Scheme_Complex *)b;
  return scheme_bin_eq(ca->r, cb->r) && scheme_bin_eq(ca->i, cb->i);
}

Scheme_Object *scheme_complex_negate(const Scheme_Object *o)
{
  Scheme_Complex *c = (Scheme_Complex *)o;

  return make_complex(scheme_bin_minus(scheme_make_integer(0),
				       c->r), 
		      scheme_bin_minus(scheme_make_integer(0),
				       c->i),
		      0);
}

Scheme_Object *scheme_complex_add(const Scheme_Object *a, const Scheme_Object *b)
{
  Scheme_Complex *ca = (Scheme_Complex *)a;
  Scheme_Complex *cb = (Scheme_Complex *)b;

  return scheme_make_complex(scheme_bin_plus(ca->r, cb->r),
			     scheme_bin_plus(ca->i, cb->i));
}

Scheme_Object *scheme_complex_subtract(const Scheme_Object *a, const Scheme_Object *b)
{
  Scheme_Complex *ca = (Scheme_Complex *)a;
  Scheme_Complex *cb = (Scheme_Complex *)b;

  return scheme_make_complex(scheme_bin_minus(ca->r, cb->r),
			     scheme_bin_minus(ca->i, cb->i));
}

Scheme_Object *scheme_complex_add1(const Scheme_Object *n)
{
  Small_Complex s;

  return scheme_complex_add(scheme_make_small_complex(scheme_make_integer(1), &s), 
			    n);
}

Scheme_Object *scheme_complex_sub1(const Scheme_Object *n)
{
  Small_Complex s;

  return scheme_complex_add(n, scheme_make_small_complex(scheme_make_integer(-1), 
							 &s));
}

Scheme_Object *scheme_complex_multiply(const Scheme_Object *a, const Scheme_Object *b)
{
  Scheme_Complex *ca = (Scheme_Complex *)a;
  Scheme_Complex *cb = (Scheme_Complex *)b;

  return scheme_make_complex(scheme_bin_minus(scheme_bin_mult(ca->r, cb->r),
					      scheme_bin_mult(ca->i, cb->i)),
			     scheme_bin_plus(scheme_bin_mult(ca->r, cb->i),
					     scheme_bin_mult(ca->i, cb->r)));
  
}

Scheme_Object *scheme_complex_divide(const Scheme_Object *_n, const Scheme_Object *_d)
{ 
  Scheme_Complex *cn = (Scheme_Complex *)_n;
  Scheme_Complex *cd = (Scheme_Complex *)_d;
  Scheme_Object *den, *r, *i, *a, *b, *c, *d, *cm, *dm, *aa[1];
  int swap;
  
  if ((cn->r == zero) && (cn->i == zero))
    return zero;

  a = cn->r;
  b = cn->i;
  c = cd->r;
  d = cd->i;

  /* Check for exact-zero simplifications in d: */
  if (c == zero) {
    i = scheme_bin_minus(zero, scheme_bin_div(a, d));
    r = scheme_bin_div(b, d);
    return scheme_make_complex(r, i);
  } else if (d == zero) {
    r = scheme_bin_div(a, c);
    i = scheme_bin_div(b, c);
    return scheme_make_complex(r, i);
  }

  if (!SCHEME_FLOATP(c) && !SCHEME_FLOATP(d)) {
    /* The simple way: */
    cm = scheme_bin_plus(scheme_bin_mult(c, c), 
                         scheme_bin_mult(d, d));
    
    r = scheme_bin_div(scheme_bin_plus(scheme_bin_mult(c, a),
                                       scheme_bin_mult(d, b)),
                       cm);
    i = scheme_bin_div(scheme_bin_minus(scheme_bin_mult(c, b),
                                        scheme_bin_mult(d, a)),
                       cm);
    
    return scheme_make_complex(r, i);
  }

  if (scheme_is_zero(d)) {
    /* This is like dividing by a real number, except that
       the inexact 0 imaginary part can interact with +inf.0 and +nan.0 */
    r = scheme_bin_plus(scheme_bin_div(a, c),
			/* Either 0.0 or +nan.0: */
			scheme_bin_mult(d, b));
    i = scheme_bin_minus(scheme_bin_div(b, c),
			 /* Either 0.0 or +nan.0: */
			 scheme_bin_mult(d, a));
    
    return scheme_make_complex(r, i);
  }
  if (scheme_is_zero(c)) {
    r = scheme_bin_plus(scheme_bin_div(b, d),
			/* Either 0.0 or +nan.0: */
			scheme_bin_mult(c, a));
    i = scheme_bin_minus(scheme_bin_mult(c, b),  /* either 0.0 or +nan.0 */
			 scheme_bin_div(a, d));

    return scheme_make_complex(r, i);
  }

  aa[0] = c;
  cm = scheme_abs(1, aa);
  aa[0] = d;
  dm = scheme_abs(1, aa);

  if (scheme_bin_lt(cm, dm)) {
    cm = a;
    a = b;
    b = cm;
    cm = c;
    c = d;
    d = cm;
    swap = 1;
  } else
    swap = 0;

  r = scheme_bin_div(c, d);

  den = scheme_bin_plus(d, scheme_bin_mult(c, r));

  if (swap)
    i = scheme_bin_div(scheme_bin_minus(a, scheme_bin_mult(b, r)), den);
  else
    i = scheme_bin_div(scheme_bin_minus(scheme_bin_mult(b, r), a), den);

  r = scheme_bin_div(scheme_bin_plus(b, scheme_bin_mult(a, r)), den);

  return scheme_make_complex(r, i);
}

Scheme_Object *scheme_complex_power(const Scheme_Object *base, const Scheme_Object *exponent)
{
  Scheme_Complex *cb = (Scheme_Complex *)base;
  Scheme_Complex *ce = (Scheme_Complex *)exponent;
  double a, b, c, d, bm, ba, nm, na, r1, r2;
  int d_is_zero;

  if ((ce->i == zero) && !SCHEME_FLOATP(ce->r)) {
    if (SCHEME_INTP(ce->r) || SCHEME_BIGNUMP(ce->r))
      return scheme_generic_integer_power(base, ce->r);
  }

  a = scheme_get_val_as_double(cb->r);
  b = scheme_get_val_as_double(cb->i);
  c = scheme_get_val_as_double(ce->r);
  d = scheme_get_val_as_double(ce->i);
  d_is_zero = (ce->i == zero);

  bm = sqrt(a * a + b * b);
  ba = atan2(b, a);

  /* New mag & angle */
  nm = scheme_double_expt(bm, c) * exp(-(ba * d));
  if (d_is_zero) /* precision here can avoid NaNs */
    na = ba * c;
  else
    na = log(bm) * d + ba * c;

  r1 = nm * cos(na);
  r2 = nm * sin(na);

#ifdef MZ_USE_SINGLE_FLOATS
  /* Coerce to double or float? */
  if (!SCHEME_DBLP(cb->r) && !SCHEME_DBLP(cb->i)
      && !SCHEME_DBLP(ce->r) && !SCHEME_DBLP(ce->i))
#ifndef USE_SINGLE_FLOATS_AS_DEFAULT
    if (SCHEME_FLTP(cb->r) || SCHEME_FLTP(cb->i)
        || SCHEME_FLTP(ce->r) || SCHEME_FLTP(ce->i))
#endif
      return scheme_make_complex(scheme_make_float((float)r1), 
                                 scheme_make_float((float)r2));
#endif

  return scheme_make_complex(scheme_make_double(r1), 
			     scheme_make_double(r2));
}

Scheme_Object *scheme_complex_sqrt(const Scheme_Object *o)
{
  Scheme_Complex *c = (Scheme_Complex *)o;
  Scheme_Object *r, *i, *ssq, *srssq, *nrsq, *prsq, *nr, *ni;

  r = c->r;
  i = c->i;

  if (scheme_is_zero(i)) {
    /* Special case for x+0.0i: */
    r = scheme_sqrt(1, &r);
    if (!SCHEME_COMPLEXP(r))
      return scheme_make_complex(r, i);
    else {
      c = (Scheme_Complex *)r;
      if (SAME_OBJ(c->r, zero)) {
        /* need an inexact-zero real part: */
#ifdef MZ_USE_SINGLE_FLOATS
        if (SCHEME_FLTP(c->i))
          r = scheme_make_float(0.0);
        else
#endif
          r = scheme_make_double(0.0);
        return scheme_make_complex(r, c->i);
      } else
        return r;
    }
  }

  ssq = scheme_bin_plus(scheme_bin_mult(r, r),
			scheme_bin_mult(i, i));

  srssq = scheme_sqrt(1, &ssq);

  if (SCHEME_FLOATP(srssq)) {
    /* We may have lost too much precision, if i << r.  The result is
       going to be inexact, anyway, so switch to using expt. */
    Scheme_Object *a[2], *p;
    a[0] = (Scheme_Object *)o;
#ifdef MZ_USE_SINGLE_FLOATS
    if (SCHEME_FLTP(c->i))
      p = scheme_make_float(0.5);
    else
#endif
      p = scheme_make_double(0.5);
    a[1] = p;
    return scheme_expt(2, a);
  }

  nrsq = scheme_bin_div(scheme_bin_minus(srssq, r),
			scheme_make_integer(2));

  nr = scheme_sqrt(1, &nrsq);
  if (scheme_is_negative(i))
    nr = scheme_bin_minus(zero, nr);
    
  prsq = scheme_bin_div(scheme_bin_plus(srssq, r),
			scheme_make_integer(2));

  ni = scheme_sqrt(1, &prsq);

  return scheme_make_complex(ni, nr);
}
