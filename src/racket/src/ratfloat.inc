
/* Optimization sometimes causes a problem: n or d is represented in an
   extended format instead of a `double'. We don't want to turn off
   floatng-point optimizations in the rest of the program, so we use a
   little function to defeat the optimization: */

FP_TYPE DO_FLOAT_DIV(FP_TYPE n, FP_TYPE d)
{
  return n / d;
}


FP_TYPE SCHEME_RATIONAL_TO_FLOAT(const Scheme_Object *o)
{
  Scheme_Rational *r = (Scheme_Rational *)o;
  FP_TYPE n, d;
  intptr_t ns, ds;

  if (SCHEME_INTP(r->num)) {
    n = (FP_TYPE)SCHEME_INT_VAL(r->num);
    ns = 0;
  } else
    n = SCHEME_BIGNUM_TO_FLOAT_INF_INFO(r->num, 0, &ns);

  if (SCHEME_INTP(r->denom)) {
    d = (FP_TYPE)SCHEME_INT_VAL(r->denom);
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
      res = (FP_TYPE)SCHEME_INT_VAL(n);
    else
      res = SCHEME_BIGNUM_TO_FLOAT_INF_INFO(n, 0, NULL);

    res = res * pow(2, p - shift);

    if (SCHEME_INTP(r->num)) {
      if (SCHEME_INT_VAL(r->num) < 0)
        res = -res;
    } else if (!SCHEME_BIGPOS(r->num)) {
      res = -res;
    }

    return res;
  }

  return DO_FLOAT_DIV(n, d);
}

Scheme_Object *SCHEME_RATIONAL_FROM_FLOAT(FP_TYPE d)
{
  double frac, i;
  int count, exponent, is_neg;
  Scheme_Object *int_part, *frac_part, *frac_num, *frac_denom, *two, *result;
#ifdef COMPUTE_NEG_INEXACT_TO_EXACT_AS_POS
  int negate;
  if (d < 0) {
    d = -d;
    negate = 1;
  } else
    negate = 0;
#endif

  SCHEME_CHECK_FLOAT("inexact->exact", d, "exact");

  is_neg = (d < 0);

  frac = modf((double)d, &i);
  (void)frexp(d, &exponent);

  int_part = SCHEME_BIGNUM_FROM_FLOAT(i);

  if (!frac) {
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
  while (frac) {
    count++;
    frac_num = scheme_bin_mult(frac_num, two);
    frac_denom = scheme_bin_mult(frac_denom, two);    
    frac = modf(ldexp(frac, 1), &i);
    if (i) {
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
