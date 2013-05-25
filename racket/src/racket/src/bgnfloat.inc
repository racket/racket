
#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

#ifndef FP_ZEROx
# define FP_ZEROx 0.0
# define FP_ONEx 1.0
# define FP_TWOx 2.0
# define FP_POWx pow
# define FP_MZ_IS_POS_INFINITY(x) MZ_IS_POS_INFINITY(x)
# define FP_scheme_floating_point_nzero scheme_floating_point_nzero
#endif

/* Optimization sometimes causes a problem?
   See note in "ratfloat.inc". */
int IS_FLOAT_INF(FP_TYPE d)
{
  return FP_MZ_IS_POS_INFINITY(d);
}

/* Must not trigger GC! (Required by xform in number.c) */
FP_TYPE SCHEME_BIGNUM_TO_FLOAT_INFO(const Scheme_Object *n, intptr_t skip, intptr_t *_skipped)
{
  intptr_t nl, skipped;
  bigdig *na;
  FP_TYPE d;

  nl = SCHEME_BIGLEN(n);
  na = SCHEME_BIGDIG(n) + nl;

  skipped = nl;

  if (skip >= nl) {
    if (SCHEME_BIGPOS(n))
      return FP_ZEROx;
    else
      return FP_scheme_floating_point_nzero;
  } else
    nl -= skip;
	
  d = FP_ZEROx;
  while (nl--) {
    d = FP_TYPE_MULT(d, FP_TYPE_FROM_DOUBLE(BIG_RADIX));
    d = FP_TYPE_PLUS(d, FP_TYPE_FROM_UINTPTR(*(--na)));
    if (IS_FLOAT_INF(d))
      break;
    --skipped;
  }

  if (_skipped)
    *_skipped = skipped;
  
  if (!SCHEME_BIGPOS(n))
    d = FP_TYPE_NEG(d);

  return d;
}

FP_TYPE SCHEME_BIGNUM_TO_FLOAT(const Scheme_Object *n)
{
  return SCHEME_BIGNUM_TO_FLOAT_INFO(n, 0, NULL);
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

Scheme_Object *SCHEME_BIGNUM_FROM_FLOAT(FP_TYPE d)
{
  Small_Bignum s1;
  int negate, log, times, i;
  FP_TYPE r;
  Scheme_Object *n, *m;

  r = FP_ONEx;

  SCHEME_CHECK_FLOAT("inexact->exact", d, "integer");

  if (FP_TYPE_LESS(d, FP_ZEROx)) {
    negate = 1;
    d = FP_TYPE_NEG(d);
  } else
    negate = 0;

  if (FP_TYPE_LESS(d, FP_ONEx))
    return scheme_make_integer(0);

  log = 0;
  while (FP_TYPE_LESS(r, d)) {
    log++;
    r = FP_TYPE_MULT(r, FP_TWOx);
  }

  if (log > USE_FLOAT_BITS) {
    times = log - USE_FLOAT_BITS;
    log = USE_FLOAT_BITS;
    for (i = 0; i < times; i++) {
      d = FP_TYPE_DIV(d, FP_TWOx);
    }
  } else
    times = 0;

  r = FP_POWx(FP_TWOx, FP_TYPE_FROM_INT(log));

  n = scheme_make_small_bignum(0, &s1);

  log++;
  while (log--) {
    bignum_double_inplace(&n);
    if (FP_TYPE_GREATER_OR_EQV(d, r)) {
      d = FP_TYPE_MINUS(d, r);
      bignum_add1_inplace(&n);
    }
    r = FP_TYPE_DIV(r, FP_TWOx);
  }

  if (times) {
    m = scheme_make_bignum(1);
    while (times--) {
      bignum_double_inplace(&m);      
    }
    n = bignum_multiply(n, m, 0);
  }

  if (negate)
    SCHEME_SET_BIGPOS(n, !SCHEME_BIGPOS(n));

  n = scheme_bignum_normalize(n);

  return n;
}

#undef USE_FLOAT_BITS
#undef FP_TYPE
#undef IS_FLOAT_INF
#undef SCHEME_BIGNUM_TO_FLOAT_INFO
#undef SCHEME_BIGNUM_TO_FLOAT
#undef SCHEME_CHECK_FLOAT
#undef SCHEME_BIGNUM_FROM_FLOAT
#undef FP_ZEROx
#undef FP_ONEx
#undef FP_TWOx
#undef FP_POWx
#undef FP_MZ_IS_POS_INFINITY
#undef FP_scheme_floating_point_nzero

#undef FP_TYPE_FROM_DOUBLE
#undef FP_TYPE_NEG
#undef FP_TYPE_LESS
#undef FP_TYPE_MULT
#undef FP_TYPE_PLUS
#undef FP_TYPE_DIV
#undef FP_TYPE_FROM_INT
#undef FP_TYPE_GREATER_OR_EQV
#undef FP_TYPE_MINUS
#undef FP_TYPE_FROM_UINTPTR
