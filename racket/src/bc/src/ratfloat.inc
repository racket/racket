
/* Back when we didn't set force the FP mode to double-precision on
   x87, compiler optimization would make n or d is represented in an
   extended format instead of a `double'. We didn't want to turn off
   floating-point optimizations in the rest of the program, so we used
   a little function to defeat the optimization. This is almost
   certainly not necessary anymore. */
   
FP_TYPE DO_FLOAT_DIV(FP_TYPE n, FP_TYPE d)
{
  return FP_DIV(n, d);
}

#ifndef FP_ZEROx
# define FP_ZEROx 0
# define FP_POWx pow
# define FP_MODFx modf
# define FP_FREXPx frexp
# define FP_DOUBLE_TYPE double
# define FP_LDEXP ldexp
#endif

FP_TYPE SCHEME_RATIONAL_TO_FLOAT(const Scheme_Object *o)
{
  Scheme_Rational *r = (Scheme_Rational *)o;
  FP_TYPE n, d;
  intptr_t ns, ds;

  if (SCHEME_INTP(r->num)) {
    if (FIXNUM_FITS_FP(r->num)) {
#ifdef CONVERT_INT_TO_FLOAT
      n = CONVERT_INT_TO_FLOAT(SCHEME_INT_VAL(r->num));
#else
      n = FP_TYPE_FROM_INTPTR(SCHEME_INT_VAL(r->num));
#endif
      ns = 0;
    } else {
      n = FP_ZEROx;
      ns = 1;
    }
  } else {
    if (BIGNUM_FITS_FP(r->num)) {
      n = SCHEME_BIGNUM_TO_FLOAT_INF_INFO(r->num, 0, &ns);
    } else {
      n = FP_ZEROx;
      ns = 1;
    }
  }

  if (SCHEME_INTP(r->denom)) {
    if (FIXNUM_FITS_FP(r->denom)) {
      d = FP_TYPE_FROM_INTPTR(SCHEME_INT_VAL(r->denom));
      ds = 0;
    } else {
      d = FP_ZEROx;
      ds = 1;
    }
  } else {
    if (BIGNUM_FITS_FP(r->denom)) {
      d = SCHEME_BIGNUM_TO_FLOAT_INF_INFO(r->denom, 0, &ds);
    } else {
      d = FP_ZEROx;
      ds = 1;
    }
  }

  if (ns || ds) {
    /* Quick path doesn't necessarily work. The more general
       way is adpated from Gambit-C 4.1. */
    intptr_t nl, dl, p, shift;
    Scheme_Object *a[2], *n, *d, *rem;
    FP_TYPE res;

    a[0] = r->num;
    n = scheme_abs(1, a);

    d = r->denom;

    nl = scheme_integer_length(n);
    dl = scheme_integer_length(d);

    p = nl - dl;
    if (p < 0) {
      a[0] = n;
      a[1] = scheme_make_integer(-p);
      
      n = scheme_bitwise_shift(2, a);
    } else {
      a[0] = d;
      a[1] = scheme_make_integer(p);
      
      d = scheme_bitwise_shift(2, a);
    }

    if (scheme_bin_lt(n, d)) {
      a[0] = n;
      a[1] = scheme_make_integer(1);
      
      n = scheme_bitwise_shift(2, a);
      --p;
    }

    shift = p - FLOAT_E_MIN;
    if (shift > FLOAT_M_BITS) {
      shift = FLOAT_M_BITS;
    }

    a[0] = n;
    a[1] = scheme_make_integer(shift);
    n = scheme_bitwise_shift(2, a);

    /* Rounded divide: */
    n = scheme_bin_quotient_remainder(n, d, &rem);
    a[0] = d;
    a[1] = scheme_make_integer(-1);
    d = scheme_bitwise_shift(2, a);
    if (!scheme_bin_lt(rem, d)) {
      if (scheme_bin_gt(rem, d)) {
        n = scheme_bin_plus(n, scheme_make_integer(1));
      } else {
        /* Round to even: */
        a[0] = d;
        if (SCHEME_FALSEP(scheme_odd_p(1, a))) {
          a[0] = n;
          if (SCHEME_FALSEP(scheme_even_p(1, a))) {
            n = scheme_bin_plus(n, scheme_make_integer(1));
          }
        }
      }
    }

    if (SCHEME_INTP(n))
      res = FP_TYPE_FROM_INTPTR(SCHEME_INT_VAL(n));
    else
      res = SCHEME_BIGNUM_TO_FLOAT_INF_INFO(n, 0, NULL);

    res = FP_MULT(res, FP_POWx(FP_TYPE_FROM_INT(2), FP_TYPE_FROM_INTPTR(p - shift)));

    if (SCHEME_INTP(r->num)) {
      if (SCHEME_INT_VAL(r->num) < 0)
        res = FP_NEG(res);
    } else if (!SCHEME_BIGPOS(r->num)) {
      res = FP_NEG(res);
    }

    return res;
  }

  return DO_FLOAT_DIV(n, d);
}

Scheme_Object *SCHEME_RATIONAL_FROM_FLOAT(FP_TYPE d)
{
#ifdef DECODE_IEEE_FLOATING_POINT
  Scheme_Object *a[2], *r, *m;
  intptr_t s, e;

  SCHEME_CHECK_FLOAT("inexact->exact", d, "exact");

# ifdef FP_FITS_INT_TYPE
  {
    FP_FITS_INT_TYPE k;
    memcpy(&k, &d, sizeof(FP_TYPE));
    s = (k >> (FLOAT_M_BITS + FLOAT_E_BITS)) & 0x1;
    e = (k >> FLOAT_M_BITS) & (((FP_FITS_INT_TYPE)1 << FLOAT_E_BITS) - 1);
    m = scheme_make_integer(k & (((FP_FITS_INT_TYPE)1 << FLOAT_M_BITS) - 1));
  }
# else
  {
    char b[sizeof(FP_TYPE)];  
    memcpy(b, &d, sizeof(FP_TYPE));
    m = scheme_bytes_to_integer(b, sizeof(FP_TYPE), 0, (FLOAT_M_BITS + FLOAT_E_BITS), 1);
    s = SCHEME_INT_VAL(m);
    m = scheme_bytes_to_integer(b, sizeof(FP_TYPE), 0, FLOAT_M_BITS, FLOAT_E_BITS);
    e = SCHEME_INT_VAL(m);
    m = scheme_bytes_to_integer(b, sizeof(FP_TYPE), 0, 0, FLOAT_M_BITS);
  }
# endif

  if (e == 0) {
    /* Subnormal numbers all have the same exponent, 2^FLOAT_E_MIN */
    a[0] = scheme_make_integer(1);
    a[1] = scheme_make_integer(-FLOAT_E_MIN);
    r = scheme_bin_div(m, scheme_bitwise_shift(2, a));
  } else {
    /* Adjust exponent for bias and mantissa size; add implicit one bit to mantissa */
    e -= (((1 << (FLOAT_E_BITS-1))-1) + FLOAT_M_BITS);
# ifdef FP_FITS_INT_TYPE
    m = scheme_make_integer(SCHEME_INT_VAL(m) | ((FP_FITS_INT_TYPE)1 << FLOAT_M_BITS));
# else
    a[0] = scheme_make_integer(1);
    a[1] = scheme_make_integer(FLOAT_M_BITS);
    m = scheme_bin_plus(m, scheme_bitwise_shift(2, a));
#endif
    /* Compute m * 2^e */
    if (e < 0) {
      a[0] = scheme_make_integer(1);
      a[1] = scheme_make_integer(-e);
      r = scheme_bin_div(m, scheme_bitwise_shift(2, a));
    } else {
      a[0] = m;
      a[1] = scheme_make_integer(e);
      r = scheme_bitwise_shift(2, a);
    }
  }

  if (s)
    return scheme_bin_minus(scheme_make_integer(0), r);
  else
    return r;
#else
  FP_DOUBLE_TYPE frac, i;
  int count, exponent, is_neg;
  Scheme_Object *int_part, *frac_part, *frac_num, *frac_denom, *two, *result;
# ifdef COMPUTE_NEG_INEXACT_TO_EXACT_AS_POS
  int negate;
  if (d < FP_ZEROx) {
    d = -d;
    negate = 1;
  } else
    negate = 0;
# endif

  SCHEME_CHECK_FLOAT("inexact->exact", d, "exact");

  is_neg = FP_LESS(d, FP_ZEROx);

  frac = FP_MODFx(d, &i);
  (void)FP_FREXPx(d, &exponent);

  int_part = SCHEME_BIGNUM_FROM_FLOAT(i);

  if (FP_EQV(frac, FP_ZEROx)) {
# ifdef COMPUTE_NEG_INEXACT_TO_EXACT_AS_POS
   if (negate)
     return scheme_bin_minus(scheme_make_integer(0), int_part);
# endif  
    return int_part;
  }

  frac_num = scheme_make_integer(0);
  frac_denom = one;
  two = scheme_make_integer(2);

  count = 0;
  while (!FP_EQV(frac, FP_ZEROx)) {
    count++;
    frac_num = scheme_bin_mult(frac_num, two);
    frac_denom = scheme_bin_mult(frac_denom, two);    
    frac = FP_MODFx(FP_LDEXP(frac, 1), &i);
    if (!FP_IS_ZERO(i)) {
      if (is_neg)	
	frac_num = scheme_bin_minus(frac_num, one);
      else
	frac_num = scheme_bin_plus(frac_num, one);
    }
  }

  frac_part = scheme_bin_div(frac_num, frac_denom);

  result = scheme_bin_plus(int_part, frac_part);

# ifdef COMPUTE_NEG_INEXACT_TO_EXACT_AS_POS
  if (negate)
    return scheme_bin_minus(scheme_make_integer(0), result);
# endif

  return result;
#endif
}

#undef FP_TYPE
#undef SCHEME_RATIONAL_TO_FLOAT
#undef SCHEME_RATIONAL_FROM_FLOAT
#undef SCHEME_BIGNUM_TO_FLOAT_INF_INFO
#undef SCHEME_BIGNUM_FROM_FLOAT
#undef SCHEME_CHECK_FLOAT
#undef FIXNUM_FITS_FP
#undef BIGNUM_FITS_FP
#undef DO_FLOAT_DIV 
#undef FLOAT_E_MIN
#undef FLOAT_M_BITS
#undef FLOAT_E_BITS
#undef FP_ZEROx
#undef FP_POWx
#undef FP_MODFx
#undef FP_FREXPx
#undef FP_DOUBLE_TYPE

#undef FP_MULT
#undef FP_DIV
#undef FP_NEG
#undef FP_LESS
#undef FP_TYPE_FROM_INTPTR
#undef FP_TYPE_FROM_INT
#undef FP_LDEXP
#undef FP_EQV
#undef FP_IS_ZERO
#undef FP_FITS_INT_TYPE
