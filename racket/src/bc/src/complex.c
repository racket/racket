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

static Scheme_Object *simple_complex_divide(Scheme_Object *a, Scheme_Object *b,
                                            Scheme_Object *c, Scheme_Object *d,
                                            int swap)
{
  Scheme_Object *r, *i, *cm, *cb, *da, *ci;

  cm = scheme_bin_plus(scheme_bin_mult(c, c),
                       scheme_bin_mult(d, d));

  r = scheme_bin_div(scheme_bin_plus(scheme_bin_mult(c, a),
                                     scheme_bin_mult(d, b)),
                     cm);

  cb = scheme_bin_mult(c, b);
  da = scheme_bin_mult(d, a);
  if (swap)
    ci = scheme_bin_minus(da, cb);
  else
    ci = scheme_bin_minus(cb, da);
  i = scheme_bin_div(ci, cm);

  return scheme_make_complex(r, i);
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

  if (b == zero) {
    /* As in Chez Scheme: a / c+di => c(a/(cc+dd)) + (-d(a/cc+dd))i */
    cm = scheme_bin_div(a, scheme_bin_plus(scheme_bin_mult(c, c), scheme_bin_mult(d, d)));
    return scheme_make_complex(scheme_bin_mult(c, cm),
                               scheme_bin_minus(zero, scheme_bin_mult(d, cm)));
  }

  if (!SCHEME_FLOATP(a) && !SCHEME_FLOATP(b) && !SCHEME_FLOATP(c) && !SCHEME_FLOATP(d))
    return simple_complex_divide(a, b, c, d, 0);

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

  if (!SCHEME_FLOATP(r) && (SCHEME_FLOATP(a) || SCHEME_FLOATP(b))) {
    aa[0] = r;
    r = scheme_exact_to_inexact(1, aa);
  }

  /* If r goes to infinity, try computing a different way to avoid overflow: */
  if (SCHEME_FLOATP(r)) {
    double v = SCHEME_FLOAT_VAL(r);
    if (MZ_IS_POS_INFINITY(v) || MZ_IS_NEG_INFINITY(v)) {
      /* This calculuation does not work as well for complex numbers with
         large parts, such as `(/ 1e+300+1e+300i 4e+300+4e+300i)`, but it
         works better for small parts, as in `(/ 0.0+0.0i 1+1e-320i)`. */
      return simple_complex_divide(a, b, c, d, swap);
    }
  }

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
    /* Special case for x+0.0i and x-0.0i: */
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
	if (scheme_minus_zero_p(scheme_real_to_double(i))) {
	  /* we started with x-0.0i */
	  return scheme_make_complex(r, scheme_bin_minus(scheme_make_integer(0), c->i));
	}
	else
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

Scheme_Object *scheme_complex_atan(const Scheme_Object *c)
{
  /* From Chez Scheme, which implements the principal expression from Kahan:
     (log(1+z) - log(1-z))/2
  */
# define OMEGA   1.7976931348623157e308
# define THETA   3.351951982485649e+153  /* = sqrt(OMEGA)/4 */
# define RHO     2.9833362924800834e-154 /* = 1/THETA */
# define HALF_PI 1.5707963267948966
# define IS_NEG(x) (((x) < 0.0) || ((x == 0.0) && scheme_minus_zero_p(x)))
  double x, y, ay, r, i;
  int negate;

  /* Get components after multiplication by i */
  x = -scheme_real_to_double(_scheme_complex_imaginary_part(c));
  y = scheme_real_to_double(_scheme_complex_real_part(c));

  /* Compute atanh */

  if (x < 0.0) {
    x = -x;
    negate = 1;
  } else {
    y = -y;
    negate = 0;
  }

  ay = fabs(y);

  if ((x > THETA) || (y > THETA)) {
    /*  RP(1/z) +/- (pi/2)i */
    if (x > ay)
      r = 1/(x + ((y/x) * y));
    else if (x < ay) {
      r = y/x;
      r = r/((x * r)+ y);
    } else
      r = 1/(x+ay);

    i = (IS_NEG(y) ? HALF_PI : (-HALF_PI));
  } else if (x == 1.0) {
    double k = ay + RHO;
    r = scheme_double_log(sqrt(sqrt((y * y) + 4.0)) / sqrt(k));
    i = (HALF_PI + scheme_double_atan(k/2.0)) / (IS_NEG(y) ? 2.0 : -2.0);
  } else {
    double mx = 1.0 - x;
    double k = ay + RHO;
    k = k * k;

    r = scheme_double_log(((4.0 * x) / ((mx * mx) + k)) + 1.0) / 4.0;
    i = scheme_double_atan2(2.0 * y, (mx * (1.0 + x)) - k) / -2.0;
  }

  if (negate) {
    i = -i;
    r = -r;
  }

  /* Multiply by -i to get atan */
  x = i;
  y = -r;
  
#ifdef MZ_USE_SINGLE_FLOATS
  if (SCHEME_FLTP(_scheme_complex_real_part(c))
      || SCHEME_FLTP(_scheme_complex_imaginary_part(c))) {
    return scheme_make_complex(scheme_make_float(x), scheme_make_float(y));
  }
#endif

  return scheme_make_complex(scheme_make_double(x), scheme_make_double(y));
}

Scheme_Object *scheme_complex_asin_or_acos(const Scheme_Object *z, int get_asin)
{
  /* From Chez Scheme, which implements the principal expression from Kahan */
  Scheme_Object *zp, *zm, *aa[1];
  double a, b, c, d, r, i;

  aa[0] = scheme_bin_minus(scheme_make_integer(1), z);
  zm = scheme_sqrt(1, aa);
  
  aa[0] = scheme_bin_plus(scheme_make_integer(1), z);
  zp = scheme_sqrt(1, aa);

  if (SCHEME_COMPLEXP(zm)) {
    a = scheme_real_to_double(_scheme_complex_real_part(zm));
    b = scheme_real_to_double(_scheme_complex_imaginary_part(zm));
  } else {
    a = scheme_real_to_double(zm);
    b = 0.0;
  }

  if (SCHEME_COMPLEXP(zp)) {
    c = scheme_real_to_double(_scheme_complex_real_part(zp));
    d = scheme_real_to_double(_scheme_complex_imaginary_part(zp));
  } else {
    c = scheme_real_to_double(zp);
    d = 0.0;
  }

  if (get_asin) {
    if (SCHEME_COMPLEXP(z)) {
      r = scheme_real_to_double(_scheme_complex_real_part(z));
      r = scheme_double_atan2(r, (a*c)-(b*d));
    } else {
      r = scheme_real_to_double((Scheme_Object *)z);
      r = scheme_double_atan2(r, 0.0); /* void +nan.0 from (a*c)-(b*d) */
    }

    i = asinh((a*d)-(b*c));
  } else {
    r = 2.0 * scheme_double_atan2(a, c);
    i = asinh((b*c) - (a*d));
  }

#ifdef MZ_USE_SINGLE_FLOATS
  if (SCHEME_FLTP(_scheme_complex_real_part(z))
      || SCHEME_FLTP(_scheme_complex_imaginary_part(z))) {
    return scheme_make_complex(scheme_make_float(r), scheme_make_float(i));
  }
#endif

  return scheme_make_complex(scheme_make_double(r), scheme_make_double(i));
}

Scheme_Object *scheme_complex_asin(const Scheme_Object *c)
{
  return scheme_complex_asin_or_acos(c, 1);
}

Scheme_Object *scheme_complex_acos(const Scheme_Object *c)
{
  return scheme_complex_asin_or_acos(c, 0);
}
