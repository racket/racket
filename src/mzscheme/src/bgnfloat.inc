
#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

/* Optimization someimtes causes a problem: d is represented in an
   extended format instead of a `double'. We don't want to turn off
   floatng-point optimizations in the rest of the program, so we use a
   little function to defeat the optimization: */
static int IS_FLOAT_INF(FP_TYPE d)
{
  return MZ_IS_POS_INFINITY(d);
}

/* Must not trigger GC! (Required by xform in number.c) */
FP_TYPE SCHEME_BIGNUM_TO_FLOAT_INFO(const Scheme_Object *n, int skip, int *_skipped)
{
  int nl, skipped;
  bigdig *na;
  FP_TYPE d;

  nl = SCHEME_BIGLEN(n);
  na = SCHEME_BIGDIG(n) + nl;

  skipped = nl;

  if (skip >= nl)
    return 0.0;
  else
    nl -= skip;
	
  d = 0;
  while (nl--) {
    d *= (FP_TYPE)BIG_RADIX;
    d += *(--na);
    if (IS_FLOAT_INF(d))
      break;
    --skipped;
  }

  if (_skipped)
    *_skipped = skipped;
  
  if (!SCHEME_BIGPOS(n))
    d = -d;

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

  r = 1;

  SCHEME_CHECK_FLOAT("inexact->exact", d, "integer");

  if (d < 0) {
    negate = 1;
    d = -d;
  } else
    negate = 0;

  if (d < 1.0)
    return scheme_make_integer(0);

  log = 0;
  while (r < d) {
    log++;
    r *= 2.0;
  }

  if (log > USE_FLOAT_BITS) {
    times = log - USE_FLOAT_BITS;
    log = USE_FLOAT_BITS;
    for (i = 0; i < times; i++) {
      d /= 2;
    }
  } else
    times = 0;

  r = pow(2.0, (FP_TYPE)log);

  n = scheme_make_small_bignum(0, &s1);

  log++;
  while (log--) {
    bignum_double_inplace(&n);
    if (d >= r) {
      d -= r;
      bignum_add1_inplace(&n);
    }
    r /= 2;
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
