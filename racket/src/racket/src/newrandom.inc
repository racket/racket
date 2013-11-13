/*
  Based on

   Implementation of SRFI-27 core generator in C for Racket.
   dvanhorn@cs.uvm.edu

  and

   54-BIT (double) IMPLEMENTATION IN C OF THE "MRG32K3A" GENERATOR
   ===============================================================

   Sebastian.Egner@philips.com, Mar-2002, in ANSI-C and Scheme 48 0.57

   This code is a C-implementation of Pierre L'Ecuyer's MRG32k3a generator.
   The code uses (double)-arithmetics, assuming that it covers the range
   {-2^53..2^53-1} exactly (!). The code of the generator is based on the
   L'Ecuyer's own implementation of the generator. Please refer to the
   file 'mrg32k3a.scm' for more information about the method.
*/

/* maximum value for random_integer: min(S48_MAX_FIXNUM_VALUE, m1) */
#define m_max (((intptr_t)1 << 31) - 1)

/* The Generator
   =============
*/

/* moduli of the components */
#define Im1 0xffffff2f
#define Im2 0xffffa6bb
#define m1 4294967087.0
#define m2 4294944443.0

/* recursion coefficients of the components */
#define a12  1403580.0
#define a13n  810728.0
#define a21   527612.0
#define a23n 1370589.0

/* normalization factor 1/(m1 + 1) */
#define norm 2.328306549295728e-10

/* the actual generator */

XFORM_NONGCING static double mrg32k3a(Scheme_Random_State *s) { /* (double), in {0..m1-1} */
  double x10, x20, y;
  intptr_t   k10, k20;

  /* component 1 */
  x10  = a12*(s->x11) - a13n*(s->x12);
  k10  = (intptr_t)(x10 / m1);
  x10 -= k10 * m1;
  if (x10 < 0.0)
    x10 += m1;
  s->x12 = s->x11;
  s->x11 = s->x10;
  s->x10 = x10;

  /* component 2 */
  x20  = a21*(s->x20) - a23n*(s->x22);
  k20  = (intptr_t)(x20 / m2);
  x20 -= k20 * m2;
  if (x20 < 0.0)
    x20 += m2;
  s->x22 = s->x21;
  s->x21 = s->x20;
  s->x20 = x20;

  /* combination of component */
  y = x10 - x20;
  if (y < 0.0)
    y += m1;
  return y;
}

/**************************************************/

static Scheme_Object *pack_rand_state(Scheme_Object *vec, Scheme_Random_State *s)
{
  if (!s) {
    s = (Scheme_Random_State *)scheme_malloc_atomic_tagged(sizeof(Scheme_Random_State));
    s->so.type = scheme_random_state_type;

  }
#define REF(r, i, top) \
  { \
    uintptr_t l; \
    if (!scheme_get_unsigned_int_val(SCHEME_VEC_ELS(vec)[i],  &l)) \
      return NULL; \
    if (l > top - 1) \
      return NULL; \
    r = (double)l; \
  }

  /* copy the numbers from state into s */
  REF(s->x10, 0, Im1)
  REF(s->x11, 1, Im1)
  REF(s->x12, 2, Im1)
  REF(s->x20, 3, Im2)
  REF(s->x21, 4, Im2)
  REF(s->x22, 5, Im2)

#undef REF

  if (!s->x10 && !s->x11 && !s->x12)
    return NULL;
  if (!s->x20 && !s->x21 && !s->x22)
    return NULL;

  return (Scheme_Object *)s;
}

static Scheme_Object *unpack_rand_state(Scheme_Random_State *s)
{
  Scheme_Object *result;

  /* make and fill a Scheme vector with the numbers */
  result = scheme_make_vector((intptr_t)6, NULL);

#define SET(i, x) \
  { \
    Scheme_Object *o; \
    o = scheme_make_integer_value_from_unsigned((uintptr_t)x); \
    SCHEME_VEC_ELS(result)[i] = o; \
  }

  SET(0, s->x10)
  SET(1, s->x11)
  SET(2, s->x12)
  SET(3, s->x20)
  SET(4, s->x21)
  SET(5, s->x22)

#undef SET

  return result;
}

/**************************************************/

static unsigned int _random_m(unsigned int *_x)
{
  unsigned int x, y;
  x = *_x;
  y = x & 0xFFFF;
  x = (30903 * y) + (x >> 16);
  *_x = x;
  return y;
}

static int _random_n(unsigned int *_x, int n)
{
  return ((_random_m(_x) << 16) + _random_m(_x)) % n;
}

static void sch_srand_half(unsigned int x, Scheme_Random_State *s)
{
  /* Due to integer overflow, this doesn't match the Scheme implementation!
     We use "int" instead of "long" to make the overflow consistent
     across platforms (since "long" is sometimes 64 bits). */
  unsigned int z;
  z = _random_n(&x, Im1-1);
  s->x10 = (double)(1 + (((unsigned int)s->x10 + z) % (Im1 - 1)));
  z = _random_n(&x, Im1);
  s->x11 = (double)(((unsigned int)s->x11 + z) % Im1);
  z = _random_n(&x, Im1);
  s->x12 = (double)(((unsigned int)s->x12 + z) % Im1);
  z = _random_n(&x, Im2-1);
  s->x20 = (double)(1 + (((unsigned int)s->x20 + z) % (Im2 - 1)));
  z = _random_n(&x, Im2);
  s->x21 = (double)(((unsigned int)s->x21 + z) % Im2);
  z = _random_n(&x, Im2);
  s->x22 = (double)(((unsigned int)s->x22 + z) % Im2);

  /* Due to the mismatch, maybe it's possible that we can hit a degeneracy?
     Double-check, just in case... */
  if (!s->x10 && !s->x11 && !s->x12)
    s->x10 = 1;
  if (!s->x20 && !s->x21 && !s->x22)
    s->x20 = 1;
}

static void sch_srand(unsigned int x, Scheme_Random_State *s)
{
  /* Initial values are from Sebastian Egner's implementation: */
  s->x10 = 1062452522.0;
  s->x11 = 2961816100.0;
  s->x12 = 342112271.0;
  s->x20 = 2854655037.0;
  s->x21 = 3321940838.0;
  s->x22 = 3542344109.0;

  sch_srand_half(x & 0xFFFF, s);
  sch_srand_half((x >> 16) & 0xFFFF, s);
}

/**************************************************/

static uintptr_t sch_int_rand(uintptr_t n, Scheme_Random_State *rs)
{
  double  x, q, qn, xq;

  /* generate result in {0..n-1} using the rejection method */
  q  = (double)( (uintptr_t)(m1 / (double)n) );
  qn = q * n;
  do {
    x = mrg32k3a(rs);
  } while (x >= qn);
  xq = x / q;

  /* return result */
  return (uintptr_t)xq;
}

XFORM_NONGCING static double sch_double_rand(Scheme_Random_State *rs)
{
  double  x;
  x = mrg32k3a(rs);
  return (x + 1.0) * norm;
}

Scheme_Object *scheme_make_random_state(intptr_t seed)
{
  Scheme_Random_State *s;

  s = (Scheme_Random_State *)scheme_malloc_atomic_tagged(sizeof(Scheme_Random_State));
  s->so.type = scheme_random_state_type;

  sch_srand(seed, s);

  return (Scheme_Object *)s;
}
