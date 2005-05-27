
/* Optimization sometimes causes a problem: n or d is represented in an
   extended format instead of a `double'. We don't want to turn off
   floatng-point optimizations in the rest of the program, so we use a
   little function to defeat the optimization: */

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

static FP_TYPE DO_FLOAT_DIV(FP_TYPE n, FP_TYPE d)
{
  return n / d;
}


/* Must not trigger GC! (Required by xform in number.c) */
FP_TYPE SCHEME_RATIONAL_TO_FLOAT(const Scheme_Object *o)
{
  Scheme_Rational *r = (Scheme_Rational *)o;
  FP_TYPE n, d;
  int ns, ds;

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

  if (ns && ds) {
    /* Dividing now would give NaN. Use ns & ds (number of bignum
       digits not needed before encountering infinity) to scale num & denom
       and get a useful value. */
    int m;

    m = (ns < ds) ? ds : ns;

    n = SCHEME_BIGNUM_TO_FLOAT_INF_INFO(r->num, m, NULL);
    d = SCHEME_BIGNUM_TO_FLOAT_INF_INFO(r->denom, m, NULL);
  }

  return DO_FLOAT_DIV(n, d);
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

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
