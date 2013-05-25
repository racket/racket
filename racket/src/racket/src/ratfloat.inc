
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
  #ifdef CONVERT_INT_TO_FLOAT
    n = CONVERT_INT_TO_FLOAT(SCHEME_INT_VAL(r->num));
  #else
    n = FP_TYPE_FROM_INT(SCHEME_INT_VAL(r->num));
  #endif
    ns = 0;
  } else
    n = SCHEME_BIGNUM_TO_FLOAT_INF_INFO(r->num, 0, &ns);

  if (SCHEME_INTP(r->denom)) {
    d = FP_TYPE_FROM_INT(SCHEME_INT_VAL(r->denom));
    ds = 0;
  } else
    d = SCHEME_BIGNUM_TO_FLOAT_INF_INFO(r->denom, 0, &ds);

  if (ns || ds) {
    /* Quick path doesn't necessarily work. The more general
       way is adpated from Gambit-C 4.1. */
    intptr_t nl, dl, p, shift;
    Scheme_Object *a[2], *n, *d;
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

    n = scheme_bin_div(n, d);
    if (SCHEME_RATIONALP(n))
      n = scheme_rational_round(n);
    
    if (SCHEME_INTP(n))
      res = FP_TYPE_FROM_INT(SCHEME_INT_VAL(n));
    else
      res = SCHEME_BIGNUM_TO_FLOAT_INF_INFO(n, 0, NULL);

    res = FP_MULT(res, FP_TYPE_FROM_INT(FP_POWx(2, p - shift)));

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
  FP_DOUBLE_TYPE frac, i;
  int count, exponent, is_neg;
  Scheme_Object *int_part, *frac_part, *frac_num, *frac_denom, *two, *result;
#ifdef COMPUTE_NEG_INEXACT_TO_EXACT_AS_POS
  int negate;
  if (d < FP_ZEROx) {
    d = -d;
    negate = 1;
  } else
    negate = 0;
#endif

  SCHEME_CHECK_FLOAT("inexact->exact", d, "exact");

  is_neg = FP_LESS(d, FP_ZEROx);

  frac = FP_MODFx(d, &i);
  (void)FP_FREXPx(d, &exponent);

  int_part = SCHEME_BIGNUM_FROM_FLOAT(i);

  if (FP_EQV(frac, FP_ZEROx)) {
#ifdef COMPUTE_NEG_INEXACT_TO_EXACT_AS_POS
   if (negate)
     return scheme_bin_minus(scheme_make_integer(0), int_part);
#endif  
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

#ifdef COMPUTE_NEG_INEXACT_TO_EXACT_AS_POS
  if (negate)
    return scheme_bin_minus(scheme_make_integer(0), result);
#endif

  return result;
}

#undef FP_TYPE
#undef SCHEME_RATIONAL_TO_FLOAT
#undef SCHEME_RATIONAL_FROM_FLOAT
#undef SCHEME_BIGNUM_TO_FLOAT_INF_INFO
#undef SCHEME_BIGNUM_FROM_FLOAT
#undef SCHEME_CHECK_FLOAT
#undef DO_FLOAT_DIV 
#undef FLOAT_E_MIN
#undef FLOAT_M_BITS
#undef FP_ZEROx
#undef FP_POWx
#undef FP_MODFx
#undef FP_FREXPx
#undef FP_DOUBLE_TYPE

#undef FP_MULT
#undef FP_DIV
#undef FP_NEG
#undef FP_LESS
#undef FP_TYPE_FROM_INT
#undef FP_LDEXP
#undef FP_EQV
#undef FP_IS_ZERO
