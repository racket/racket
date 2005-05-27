/* Copyright (C) 1992, 1993, 1994, 1996 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA. */


#define _FORCE_INLINES
#define _EXTERN_INLINE /* empty */

/* We use malloc for now; this will have to change. */
/* The allocation function should not create collectable
   memory, though it can safely GC when allocating. */
extern void *malloc(unsigned long);
extern void free(void *);
#define MALLOC malloc
#define FREE(p, s) free(p)

#include "../../sconfig.h"
#include "gmp.h"
#include "gmp-impl.h"
#include "gmplonglong.h"

#define GMP_NAIL_BITS 0
#define GMP_LIMB_BITS BITS_PER_MP_LIMB
#define GMP_NUMB_BITS BITS_PER_MP_LIMB
#define GMP_LIMB_HIGHBIT (1 << (BITS_PER_MP_LIMB - 1))
#define GMP_NUMB_HIGHBIT GMP_LIMB_HIGHBIT

#ifndef NULL
# define NULL 0L
#endif

#if GMP_NUMB_BITS == 32
# define MP_BASES_CHARS_PER_LIMB_10      9
# define MP_BASES_BIG_BASE_10            CNST_LIMB(0x3b9aca00)
# define MP_BASES_BIG_BASE_INVERTED_10   CNST_LIMB(0x12e0be82)
# define MP_BASES_NORMALIZATION_STEPS_10 2
# define GMP_NUMB_MASK 0xFFFFFFFF
#else
# define MP_BASES_CHARS_PER_LIMB_10      19
# define MP_BASES_BIG_BASE_10            CNST_LIMB(0x8ac7230489e80000)
# define MP_BASES_BIG_BASE_INVERTED_10   CNST_LIMB(0xd83c94fb6d2ac34a)
# define MP_BASES_NORMALIZATION_STEPS_10 0
# define GMP_NUMB_MASK 0xFFFFFFFFFFFFFFFF
#endif

#define MPN_DIVREM_OR_PREINV_DIVREM_1(qp,xsize,ap,size,d,dinv,shift)    \
  mpn_divrem_1 (qp, xsize, ap, size, d)

#define ABOVE_THRESHOLD(size,thresh)    \
  ((thresh) == 0                        \
   || ((thresh) != MP_SIZE_T_MAX        \
       && (size) >= (thresh)))
#define BELOW_THRESHOLD(size,thresh)  (! ABOVE_THRESHOLD (size, thresh))

/* n-1 inverts any low zeros and the lowest one bit.  If n&(n-1) leaves zero
   then that lowest one bit must have been the only bit set.  n==0 will
   return true though, so avoid that.  */
#define POW2_P(n)  (((n) & ((n) - 1)) == 0)

# define __GMP_ALLOCATE_FUNC_LIMBS(n) TMP_ALLOC(n * sizeof(mp_limb_t))
# define __GMP_FREE_FUNC_LIMBS(p, n) /* */

/* static const int mp_bits_per_limb = BITS_PER_MP_LIMB; */
/* static const int __gmp_0 = 0; */
/* static int __gmp_junk; */
/* static int gmp_errno = 0; */

#define SCHEME_BIGNUM_USE_FUEL(n) scheme_bignum_use_fuel(n)
extern void scheme_bignum_use_fuel(long n);

/* Compare OP1_PTR/OP1_SIZE with OP2_PTR/OP2_SIZE.
   There are no restrictions on the relative sizes of
   the two arguments.
   Return 1 if OP1 > OP2, 0 if they are equal, and -1 if OP1 < OP2.  */

int
#if __STDC__
mpn_cmp (mp_srcptr op1_ptr, mp_srcptr op2_ptr, mp_size_t size)
#else
mpn_cmp (op1_ptr, op2_ptr, size)
     mp_srcptr op1_ptr;
     mp_srcptr op2_ptr;
     mp_size_t size;
#endif
{
  mp_size_t i;
  mp_limb_t op1_word, op2_word;

  for (i = size - 1; i >= 0; i--)
    {
      op1_word = op1_ptr[i];
      op2_word = op2_ptr[i];
      if (op1_word != op2_word)
	goto diff;
    }
  return 0;
 diff:
  /* This can *not* be simplified to
	op2_word - op2_word
     since that expression might give signed overflow.  */
  return (op1_word > op2_word) ? 1 : -1;
}


/* mpn_sub_n -- Subtract two limb vectors of equal, non-zero length. */

mp_limb_t
#if __STDC__
mpn_sub_n (mp_ptr res_ptr, mp_srcptr s1_ptr, mp_srcptr s2_ptr, mp_size_t size)
#else
mpn_sub_n (res_ptr, s1_ptr, s2_ptr, size)
     register mp_ptr res_ptr;
     register mp_srcptr s1_ptr;
     register mp_srcptr s2_ptr;
     mp_size_t size;
#endif
{
  register mp_limb_t x, y, cy;
  register mp_size_t j;

  /* The loop counter and index J goes from -SIZE to -1.  This way
     the loop becomes faster.  */
  j = -size;

  /* Offset the base pointers to compensate for the negative indices.  */
  s1_ptr -= j;
  s2_ptr -= j;
  res_ptr -= j;

  cy = 0;
  do
    {
      y = s2_ptr[j];
      x = s1_ptr[j];
      y += cy;			/* add previous carry to subtrahend */
      cy = (y < cy);		/* get out carry from that addition */
      y = x - y;		/* main subtract */
      cy = (y > x) + cy;	/* get out carry from the subtract, combine */
      res_ptr[j] = y;
    }
  while (++j != 0);

  return cy;
}

/* mpn_add_n -- Add two limb vectors of equal, non-zero length. */

mp_limb_t
#if __STDC__
mpn_add_n (mp_ptr res_ptr, mp_srcptr s1_ptr, mp_srcptr s2_ptr, mp_size_t size)
#else
mpn_add_n (res_ptr, s1_ptr, s2_ptr, size)
     register mp_ptr res_ptr;
     register mp_srcptr s1_ptr;
     register mp_srcptr s2_ptr;
     mp_size_t size;
#endif
{
  register mp_limb_t x, y, cy;
  register mp_size_t j;

  /* The loop counter and index J goes from -SIZE to -1.  This way
     the loop becomes faster.  */
  j = -size;

  /* Offset the base pointers to compensate for the negative indices.  */
  s1_ptr -= j;
  s2_ptr -= j;
  res_ptr -= j;

  cy = 0;
  do
    {
      y = s2_ptr[j];
      x = s1_ptr[j];
      y += cy;			/* add previous carry to one addend */
      cy = (y < cy);		/* get out carry from that addition */
      y = x + y;		/* add other addend */
      cy = (y < x) + cy;	/* get out carry from that add, combine */
      res_ptr[j] = y;
    }
  while (++j != 0);

  return cy;
}


/* mpn_mul_n and helper function -- Multiply/square natural numbers. */

/* Multiplicative inverse of 3, modulo 2^BITS_PER_MP_LIMB.
   0xAAAAAAAB for 32 bits, 0xAAAAAAAAAAAAAAAB for 64 bits. */
#define INVERSE_3      ((MP_LIMB_T_MAX / 3) * 2 + 1)

#if !defined (__alpha) && !defined (__mips)
/* For all other machines, we want to call mpn functions for the compund
   operations instead of open-coding them.  */
#define USE_MORE_MPN
#endif

/*== Function declarations =================================================*/

static void evaluate3 _PROTO ((mp_ptr, mp_ptr, mp_ptr,
                               mp_ptr, mp_ptr, mp_ptr,
                               mp_srcptr, mp_srcptr, mp_srcptr,
                               mp_size_t, mp_size_t));
static void interpolate3 _PROTO ((mp_srcptr,
                                  mp_ptr, mp_ptr, mp_ptr,
                                  mp_srcptr,
                                  mp_ptr, mp_ptr, mp_ptr,
                                  mp_size_t, mp_size_t));
static mp_limb_t add2Times _PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t));


/*-- mpn_kara_mul_n ---------------------------------------------------------------*/

/* Multiplies using 3 half-sized mults and so on recursively.
 * p[0..2*n-1] := product of a[0..n-1] and b[0..n-1].
 * No overlap of p[...] with a[...] or b[...].
 * ws is workspace.
 */

void
#if __STDC__
mpn_kara_mul_n (mp_ptr p, mp_srcptr a, mp_srcptr b, mp_size_t n, mp_ptr ws)
#else
mpn_kara_mul_n(p, a, b, n, ws)
     mp_ptr    p;
     mp_srcptr a;
     mp_srcptr b;
     mp_size_t n;
     mp_ptr    ws;
#endif
{
  mp_limb_t i, sign, w, w0, w1;
  mp_size_t n2;
  mp_srcptr x, y;

  n2 = n >> 1;
  ASSERT (n2 > 0);

  SCHEME_BIGNUM_USE_FUEL(n);

  if (n & 1)
    {
      /* Odd length. */
      mp_size_t n1, n3, nm1;

      n3 = n - n2;

      sign = 0;
      w = a[n2];
      if (w != 0)
	w -= mpn_sub_n (p, a, a + n3, n2);
      else
	{
	  i = n2;
	  do
	    {
	      --i;
	      w0 = a[i];
	      w1 = a[n3+i];
	    }
	  while (w0 == w1 && i != 0);
	  if (w0 < w1)
	    {
	      x = a + n3;
	      y = a;
	      sign = 1;
	    }
	  else
	    {
	      x = a;
	      y = a + n3;
	    }
	  mpn_sub_n (p, x, y, n2);
	}
      p[n2] = w;

      w = b[n2];
      if (w != 0)
	w -= mpn_sub_n (p + n3, b, b + n3, n2);
      else
	{
	  i = n2;
	  do 
	    {
	      --i;
	      w0 = b[i]; 
	      w1 = b[n3+i];
	    }
	  while (w0 == w1 && i != 0);
	  if (w0 < w1)
	    {
	      x = b + n3;
	      y = b;
	      sign ^= 1;
	    }
	  else
	    {
	      x = b;
	      y = b + n3;
	    }
	  mpn_sub_n (p + n3, x, y, n2);
	}
      p[n] = w;

      n1 = n + 1;
      if (n2 < KARATSUBA_MUL_THRESHOLD)
	{
	  if (n3 < KARATSUBA_MUL_THRESHOLD)
	    {
	      mpn_mul_basecase (ws, p, n3, p + n3, n3);
	      mpn_mul_basecase (p, a, n3, b, n3);
	    }
	  else
	    {
	      mpn_kara_mul_n (ws, p, p + n3, n3, ws + n1);
	      mpn_kara_mul_n (p, a, b, n3, ws + n1);
	    }
	  mpn_mul_basecase (p + n1, a + n3, n2, b + n3, n2);
	}
      else
	{
	  mpn_kara_mul_n (ws, p, p + n3, n3, ws + n1);
	  mpn_kara_mul_n (p, a, b, n3, ws + n1);
	  mpn_kara_mul_n (p + n1, a + n3, b + n3, n2, ws + n1);
	}

      if (sign)
	mpn_add_n (ws, p, ws, n1);
      else
	mpn_sub_n (ws, p, ws, n1);

      nm1 = n - 1;
      if (mpn_add_n (ws, p + n1, ws, nm1))
	{
	  mp_limb_t x = ws[nm1] + 1;
	  ws[nm1] = x;
	  if (x == 0)
	    ++ws[n];
	}
      if (mpn_add_n (p + n3, p + n3, ws, n1))
	{
	  mp_limb_t x;
	  i = n1 + n3;
	  do
	    {
	      x = p[i] + 1;
	      p[i] = x;
	      ++i;
	    } while (x == 0);
	}
    }
  else
    {
      /* Even length. */
      mp_limb_t t;

      i = n2;
      do
	{
	  --i;
	  w0 = a[i];
	  w1 = a[n2+i];
	}
      while (w0 == w1 && i != 0);
      sign = 0;
      if (w0 < w1)
	{
	  x = a + n2;
	  y = a;
	  sign = 1;
	}
      else
	{
	  x = a;
	  y = a + n2;
	}
      mpn_sub_n (p, x, y, n2);

      i = n2;
      do 
	{
	  --i;
	  w0 = b[i];
	  w1 = b[n2+i];
	}
      while (w0 == w1 && i != 0);
      if (w0 < w1)
	{
	  x = b + n2;
	  y = b;
	  sign ^= 1;
	}
      else
	{
	  x = b;
	  y = b + n2;
	}
      mpn_sub_n (p + n2, x, y, n2);

      /* Pointwise products. */
      if (n2 < KARATSUBA_MUL_THRESHOLD)
	{
	  mpn_mul_basecase (ws, p, n2, p + n2, n2);
	  mpn_mul_basecase (p, a, n2, b, n2);
	  mpn_mul_basecase (p + n, a + n2, n2, b + n2, n2);
	}
      else
	{
	  mpn_kara_mul_n (ws, p, p + n2, n2, ws + n);
	  mpn_kara_mul_n (p, a, b, n2, ws + n);
	  mpn_kara_mul_n (p + n, a + n2, b + n2, n2, ws + n);
	}

      /* Interpolate. */
      if (sign)
	w = mpn_add_n (ws, p, ws, n);
      else
	w = -mpn_sub_n (ws, p, ws, n);
      w += mpn_add_n (ws, p + n, ws, n);
      w += mpn_add_n (p + n2, p + n2, ws, n);
      /* TO DO: could put "if (w) { ... }" here.
       * Less work but badly predicted branch.
       * No measurable difference in speed on Alpha.
       */
      i = n + n2;
      t = p[i] + w;
      p[i] = t;
      if (t < w)
	{
	  do
	    {
	      ++i;
	      w = p[i] + 1;
	      p[i] = w;
	    }
	  while (w == 0);
	}
    }
}

void
#if __STDC__
mpn_kara_sqr_n (mp_ptr p, mp_srcptr a, mp_size_t n, mp_ptr ws)
#else
mpn_kara_sqr_n (p, a, n, ws)
     mp_ptr    p;
     mp_srcptr a;
     mp_size_t n;
     mp_ptr    ws;
#endif
{
  mp_limb_t i, sign, w, w0, w1;
  mp_size_t n2;
  mp_srcptr x, y;

  n2 = n >> 1;
  ASSERT (n2 > 0);

  SCHEME_BIGNUM_USE_FUEL(n);

  if (n & 1)
    {
      /* Odd length. */
      mp_size_t n1, n3, nm1;

      n3 = n - n2;

      sign = 0;
      w = a[n2];
      if (w != 0)
	w -= mpn_sub_n (p, a, a + n3, n2);
      else
	{
	  i = n2;
	  do
	    {
	      --i;
	      w0 = a[i];
	      w1 = a[n3+i];
	    }
	  while (w0 == w1 && i != 0);
	  if (w0 < w1)
	    {
	      x = a + n3;
	      y = a;
	      sign = 1;
	    }
	  else
	    {
	      x = a;
	      y = a + n3;
	    }
	  mpn_sub_n (p, x, y, n2);
	}
      p[n2] = w;

      w = a[n2];
      if (w != 0)
	w -= mpn_sub_n (p + n3, a, a + n3, n2);
      else
	{
	  i = n2;
	  do 
	    {
	      --i;
	      w0 = a[i]; 
	      w1 = a[n3+i];
	    }
	  while (w0 == w1 && i != 0);
	  if (w0 < w1)
	    {
	      x = a + n3;
	      y = a;
	      sign ^= 1;
	    }
	  else
	    {
	      x = a;
	      y = a + n3;
	    }
	  mpn_sub_n (p + n3, x, y, n2);
	}
      p[n] = w;

      n1 = n + 1;
      if (n2 < KARATSUBA_SQR_THRESHOLD)
	{
	  if (n3 < KARATSUBA_SQR_THRESHOLD)
	    {
	      mpn_sqr_basecase (ws, p, n3);
	      mpn_sqr_basecase (p, a, n3);
	    }
	  else
	    {
	      mpn_kara_sqr_n (ws, p, n3, ws + n1);
	      mpn_kara_sqr_n (p, a, n3, ws + n1);
	    }
	  mpn_sqr_basecase (p + n1, a + n3, n2);
	}
      else
	{
	  mpn_kara_sqr_n (ws, p, n3, ws + n1);
	  mpn_kara_sqr_n (p, a, n3, ws + n1);
	  mpn_kara_sqr_n (p + n1, a + n3, n2, ws + n1);
	}

      if (sign)
	mpn_add_n (ws, p, ws, n1);
      else
	mpn_sub_n (ws, p, ws, n1);

      nm1 = n - 1;
      if (mpn_add_n (ws, p + n1, ws, nm1))
	{
	  mp_limb_t x = ws[nm1] + 1;
	  ws[nm1] = x;
	  if (x == 0)
	    ++ws[n];
	}
      if (mpn_add_n (p + n3, p + n3, ws, n1))
	{
	  mp_limb_t x;
	  i = n1 + n3;
	  do
	    {
	      x = p[i] + 1;
	      p[i] = x;
	      ++i;
	    } while (x == 0);
	}
    }
  else
    {
      /* Even length. */
      mp_limb_t t;

      i = n2;
      do
	{
	  --i;
	  w0 = a[i];
	  w1 = a[n2+i];
	}
      while (w0 == w1 && i != 0);
      sign = 0;
      if (w0 < w1)
	{
	  x = a + n2;
	  y = a;
	  sign = 1;
	}
      else
	{
	  x = a;
	  y = a + n2;
	}
      mpn_sub_n (p, x, y, n2);

      i = n2;
      do 
	{
	  --i;
	  w0 = a[i];
	  w1 = a[n2+i];
	}
      while (w0 == w1 && i != 0);
      if (w0 < w1)
	{
	  x = a + n2;
	  y = a;
	  sign ^= 1;
	}
      else
	{
	  x = a;
	  y = a + n2;
	}
      mpn_sub_n (p + n2, x, y, n2);

      /* Pointwise products. */
      if (n2 < KARATSUBA_SQR_THRESHOLD)
	{
	  mpn_sqr_basecase (ws, p, n2);
	  mpn_sqr_basecase (p, a, n2);
	  mpn_sqr_basecase (p + n, a + n2, n2);
	}
      else
	{
	  mpn_kara_sqr_n (ws, p, n2, ws + n);
	  mpn_kara_sqr_n (p, a, n2, ws + n);
	  mpn_kara_sqr_n (p + n, a + n2, n2, ws + n);
	}

      /* Interpolate. */
      if (sign)
	w = mpn_add_n (ws, p, ws, n);
      else
	w = -mpn_sub_n (ws, p, ws, n);
      w += mpn_add_n (ws, p + n, ws, n);
      w += mpn_add_n (p + n2, p + n2, ws, n);
      /* TO DO: could put "if (w) { ... }" here.
       * Less work but badly predicted branch.
       * No measurable difference in speed on Alpha.
       */
      i = n + n2;
      t = p[i] + w;
      p[i] = t;
      if (t < w)
	{
	  do
	    {
	      ++i;
	      w = p[i] + 1;
	      p[i] = w;
	    }
	  while (w == 0);
	}
    }
}

/*-- add2Times -------------------------------------------------------------*/

/* z[] = x[] + 2 * y[]
   Note that z and x might point to the same vectors. */
#ifdef USE_MORE_MPN
static inline mp_limb_t
#if __STDC__
add2Times (mp_ptr z, mp_srcptr x, mp_srcptr y, mp_size_t n)
#else
add2Times (z, x, y, n)
     mp_ptr    z;
     mp_srcptr x;
     mp_srcptr y;
     mp_size_t n;
#endif
{
  mp_ptr t;
  mp_limb_t c;
  TMP_DECL (marker);
  TMP_MARK (marker);
  t = (mp_ptr) TMP_ALLOC (n * BYTES_PER_MP_LIMB);
  c = mpn_lshift (t, y, n, 1);
  c += mpn_add_n (z, x, t, n);
  TMP_FREE (marker);
  return c;
}
#else

static mp_limb_t
#if __STDC__
add2Times (mp_ptr z, mp_srcptr x, mp_srcptr y, mp_size_t n)
#else
add2Times (z, x, y, n)
     mp_ptr    z;
     mp_srcptr x;
     mp_srcptr y;
     mp_size_t n;
#endif
{
  mp_limb_t c, v, w;

  ASSERT (n > 0);
  v = *x; w = *y;
  c = w >> (BITS_PER_MP_LIMB - 1);
  w <<= 1;
  v += w;
  c += v < w;
  *z = v;
  ++x; ++y; ++z;
  while (--n)
    {
      v = *x;
      w = *y;
      v += c;
      c = v < c;
      c += w >> (BITS_PER_MP_LIMB - 1);
      w <<= 1;
      v += w;
      c += v < w;
      *z = v;
      ++x; ++y; ++z;
    }

  return c;
}
#endif

/*-- evaluate3 -------------------------------------------------------------*/

/* Evaluates:
 *   ph := 4*A+2*B+C
 *   p1 := A+B+C
 *   p2 := A+2*B+4*C
 * where:
 *   ph[], p1[], p2[], A[] and B[] all have length len,
 *   C[] has length len2 with len-len2 = 0, 1 or 2.
 * Returns top words (overflow) at pth, pt1 and pt2 respectively.
 */
#ifdef USE_MORE_MPN
static void
#if __STDC__
evaluate3 (mp_ptr ph, mp_ptr p1, mp_ptr p2, mp_ptr pth, mp_ptr pt1, mp_ptr pt2,
	   mp_srcptr A, mp_srcptr B, mp_srcptr C, mp_size_t len, mp_size_t len2)
#else
evaluate3 (ph, p1, p2, pth, pt1, pt2,
           A, B, C, len, len2)
     mp_ptr    ph;
     mp_ptr    p1;
     mp_ptr    p2;
     mp_ptr    pth;
     mp_ptr    pt1;
     mp_ptr    pt2;
     mp_srcptr A;
     mp_srcptr B;
     mp_srcptr C;
     mp_size_t len;
     mp_size_t len2;
#endif
{
  mp_limb_t c, d, e;
  
  ASSERT (len - len2 <= 2);

  e = mpn_lshift (p1, B, len, 1);

  c = mpn_lshift (ph, A, len, 2);
  c += e + mpn_add_n (ph, ph, p1, len);
  d = mpn_add_n (ph, ph, C, len2);
  if (len2 == len) c += d; else c += mpn_add_1 (ph + len2, ph + len2, len-len2, d);
  ASSERT (c < 7);
  *pth = c;

  c = mpn_lshift (p2, C, len2, 2);
#if 1
  if (len2 != len) { p2[len-1] = 0; p2[len2] = c; c = 0; }
  c += e + mpn_add_n (p2, p2, p1, len);
#else
  d = mpn_add_n (p2, p2, p1, len2);
  c += d;
  if (len2 != len) c = mpn_add_1 (p2+len2, p1+len2, len-len2, c);
  c += e;
#endif
  c += mpn_add_n (p2, p2, A, len);
  ASSERT (c < 7);
  *pt2 = c;

  c = mpn_add_n (p1, A, B, len);
  d = mpn_add_n (p1, p1, C, len2);
  if (len2 == len) c += d;
  else c += mpn_add_1 (p1+len2, p1+len2, len-len2, d);
  ASSERT (c < 3);
  *pt1 = c;

}

#else

static void
#if __STDC__
evaluate3 (mp_ptr ph, mp_ptr p1, mp_ptr p2, mp_ptr pth, mp_ptr pt1, mp_ptr pt2,
	   mp_srcptr A, mp_srcptr B, mp_srcptr C, mp_size_t l, mp_size_t ls)
#else
evaluate3 (ph, p1, p2, pth, pt1, pt2,
           A, B, C, l, ls)
     mp_ptr    ph;
     mp_ptr    p1;
     mp_ptr    p2;
     mp_ptr    pth;
     mp_ptr    pt1;
     mp_ptr    pt2;
     mp_srcptr A;
     mp_srcptr B;
     mp_srcptr C;
     mp_size_t l;
     mp_size_t ls;
#endif
{
  mp_limb_t a,b,c, i, t, th,t1,t2, vh,v1,v2;

  ASSERT (l - ls <= 2);

  th = t1 = t2 = 0;
  for (i = 0; i < l; ++i)
    {
      a = *A;
      b = *B;
      c = i < ls ? *C : 0;

      /* TO DO: choose one of the following alternatives. */
#if 0
      t = a << 2;
      vh = th + t;
      th = vh < t;
      th += a >> (BITS_PER_MP_LIMB - 2);
      t = b << 1;
      vh += t;
      th += vh < t;
      th += b >> (BITS_PER_MP_LIMB - 1);
      vh += c;
      th += vh < c;
#else
      vh = th + c;
      th = vh < c;
      t = b << 1;
      vh += t;
      th += vh < t;
      th += b >> (BITS_PER_MP_LIMB - 1);
      t = a << 2;
      vh += t;
      th += vh < t;
      th += a >> (BITS_PER_MP_LIMB - 2);
#endif

      v1 = t1 + a;
      t1 = v1 < a;
      v1 += b;
      t1 += v1 < b;
      v1 += c;
      t1 += v1 < c;

      v2 = t2 + a;
      t2 = v2 < a;
      t = b << 1;
      v2 += t;
      t2 += v2 < t;
      t2 += b >> (BITS_PER_MP_LIMB - 1);
      t = c << 2;
      v2 += t;
      t2 += v2 < t;
      t2 += c >> (BITS_PER_MP_LIMB - 2);

      *ph = vh;
      *p1 = v1;
      *p2 = v2;

      ++A; ++B; ++C;
      ++ph; ++p1; ++p2;
    }

  ASSERT (th < 7);
  ASSERT (t1 < 3);
  ASSERT (t2 < 7);

  *pth = th;
  *pt1 = t1;
  *pt2 = t2;
}
#endif


/*-- interpolate3 ----------------------------------------------------------*/

/* Interpolates B, C, D (in-place) from:
 *   16*A+8*B+4*C+2*D+E
 *   A+B+C+D+E
 *   A+2*B+4*C+8*D+16*E
 * where:
 *   A[], B[], C[] and D[] all have length l,
 *   E[] has length ls with l-ls = 0, 2 or 4.
 *
 * Reads top words (from earlier overflow) from ptb, ptc and ptd,
 * and returns new top words there.
 */

#ifdef USE_MORE_MPN
static void
#if __STDC__
interpolate3 (mp_srcptr A, mp_ptr B, mp_ptr C, mp_ptr D, mp_srcptr E,
              mp_ptr ptb, mp_ptr ptc, mp_ptr ptd, mp_size_t len, mp_size_t len2)
#else
interpolate3 (A, B, C, D, E,
              ptb, ptc, ptd, len, len2)
     mp_srcptr A;
     mp_ptr    B;
     mp_ptr    C;
     mp_ptr    D;
     mp_srcptr E;
     mp_ptr    ptb;
     mp_ptr    ptc;
     mp_ptr    ptd;
     mp_size_t len;
     mp_size_t len2;
#endif
{
  mp_ptr ws;
  mp_limb_t t, tb,tc,td;
  TMP_DECL (marker);
  TMP_MARK (marker);

  ASSERT (len - len2 == 0 || len - len2 == 2 || len - len2 == 4);

  /* Let x1, x2, x3 be the values to interpolate.  We have:
   *         b = 16*a + 8*x1 + 4*x2 + 2*x3 +    e
   *         c =    a +   x1 +   x2 +   x3 +    e
   *         d =    a + 2*x1 + 4*x2 + 8*x3 + 16*e
   */

  ws = (mp_ptr) TMP_ALLOC (len * BYTES_PER_MP_LIMB);

  tb = *ptb; tc = *ptc; td = *ptd;


  /* b := b - 16*a -    e
   * c := c -    a -    e
   * d := d -    a - 16*e
   */

  t = mpn_lshift (ws, A, len, 4);
  tb -= t + mpn_sub_n (B, B, ws, len);
  t = mpn_sub_n (B, B, E, len2);
  if (len2 == len) tb -= t;
  else tb -= mpn_sub_1 (B+len2, B+len2, len-len2, t);

  tc -= mpn_sub_n (C, C, A, len);
  t = mpn_sub_n (C, C, E, len2);
  if (len2 == len) tc -= t;
  else tc -= mpn_sub_1 (C+len2, C+len2, len-len2, t);

  t = mpn_lshift (ws, E, len2, 4);
  t += mpn_add_n (ws, ws, A, len2);
#if 1
  if (len2 != len) t = mpn_add_1 (ws+len2, A+len2, len-len2, t);
  td -= t + mpn_sub_n (D, D, ws, len);
#else
  t += mpn_sub_n (D, D, ws, len2);
  if (len2 != len) {
    t = mpn_sub_1 (D+len2, D+len2, len-len2, t);
    t += mpn_sub_n (D+len2, D+len2, A+len2, len-len2);
  } /* end if/else */
  td -= t;
#endif


  /* b, d := b + d, b - d */

#ifdef HAVE_MPN_ADD_SUB_N
  /* #error TO DO ... */
#else
  t = tb + td + mpn_add_n (ws, B, D, len);  
  td = tb - td - mpn_sub_n (D, B, D, len);
  tb = t;
  MPN_COPY (B, ws, len);
#endif
  
  /* b := b-8*c */
  t = 8 * tc + mpn_lshift (ws, C, len, 3);
  tb -= t + mpn_sub_n (B, B, ws, len);

  /* c := 2*c - b */
  tc = 2 * tc + mpn_lshift (C, C, len, 1);
  tc -= tb + mpn_sub_n (C, C, B, len);

  /* d := d/3 */
  td = (td - mpn_divexact_by3 (D, D, len)) * INVERSE_3;

  /* b, d := b + d, b - d */
#ifdef HAVE_MPN_ADD_SUB_N
  /* #error TO DO ... */
#else
  t = tb + td + mpn_add_n (ws, B, D, len);  
  td = tb - td - mpn_sub_n (D, B, D, len);
  tb = t;
  MPN_COPY (B, ws, len);
#endif

      /* Now:
       *	 b = 4*x1
       *	 c = 2*x2
       *	 d = 4*x3
       */

  ASSERT(!(*B & 3));
  mpn_rshift (B, B, len, 2);
  B[len-1] |= tb<<(BITS_PER_MP_LIMB-2);
  ASSERT((long)tb >= 0);
  tb >>= 2;

  ASSERT(!(*C & 1));
  mpn_rshift (C, C, len, 1);
  C[len-1] |= tc<<(BITS_PER_MP_LIMB-1);
  ASSERT((long)tc >= 0);
  tc >>= 1;

  ASSERT(!(*D & 3));
  mpn_rshift (D, D, len, 2);
  D[len-1] |= td<<(BITS_PER_MP_LIMB-2);
  ASSERT((long)td >= 0);
  td >>= 2;

#if WANT_ASSERT
  ASSERT (tb < 2);
  if (len == len2)
    {
      ASSERT (tc < 3);
      ASSERT (td < 2);
    }
  else
    {
      ASSERT (tc < 2);
      ASSERT (!td);
    }
#endif

  *ptb = tb;
  *ptc = tc;
  *ptd = td;

  TMP_FREE (marker);
}

#else

static void
#if __STDC__
interpolate3 (mp_srcptr A, mp_ptr B, mp_ptr C, mp_ptr D, mp_srcptr E,
	      mp_ptr ptb, mp_ptr ptc, mp_ptr ptd, mp_size_t l, mp_size_t ls)
#else
interpolate3 (A, B, C, D, E,
              ptb, ptc, ptd, l, ls)
     mp_srcptr A;
     mp_ptr    B;
     mp_ptr    C;
     mp_ptr    D;
     mp_srcptr E;
     mp_ptr    ptb;
     mp_ptr    ptc;
     mp_ptr    ptd;
     mp_size_t l;
     mp_size_t ls;
#endif
{
  mp_limb_t a,b,c,d,e,t, i, sb,sc,sd, ob,oc,od;
  const mp_limb_t maskOffHalf = (~(mp_limb_t) 0) << (BITS_PER_MP_LIMB >> 1);

#if WANT_ASSERT
  t = l - ls;
  ASSERT (t == 0 || t == 2 || t == 4);
#endif

  sb = sc = sd = 0;
  for (i = 0; i < l; ++i)
    {
      mp_limb_t tb, tc, td, tt;

      a = *A;
      b = *B;
      c = *C;
      d = *D;
      e = i < ls ? *E : 0;

      /* Let x1, x2, x3 be the values to interpolate.  We have:
       *	 b = 16*a + 8*x1 + 4*x2 + 2*x3 +    e
       *	 c =	a +   x1 +   x2 +   x3 +    e
       *	 d =	a + 2*x1 + 4*x2 + 8*x3 + 16*e
       */

      /* b := b - 16*a -    e
       * c := c -    a -    e
       * d := d -    a - 16*e
       */
      t = a << 4;
      tb = -(a >> (BITS_PER_MP_LIMB - 4)) - (b < t);
      b -= t;
      tb -= b < e;
      b -= e;
      tc = -(c < a);
      c -= a;
      tc -= c < e;
      c -= e;
      td = -(d < a);
      d -= a;
      t = e << 4;
      td = td - (e >> (BITS_PER_MP_LIMB - 4)) - (d < t);
      d -= t;

      /* b, d := b + d, b - d */
      t = b + d;
      tt = tb + td + (t < b);
      td = tb - td - (b < d);
      d = b - d;
      b = t;
      tb = tt;

      /* b := b-8*c */
      t = c << 3;
      tb = tb - (tc << 3) - (c >> (BITS_PER_MP_LIMB - 3)) - (b < t);
      b -= t;

      /* c := 2*c - b */
      t = c << 1;
      tc = (tc << 1) + (c >> (BITS_PER_MP_LIMB - 1)) - tb - (t < b);
      c = t - b;

      /* d := d/3 */
      d *= INVERSE_3;
      td = td - (d >> (BITS_PER_MP_LIMB - 1)) - (d*3 < d);
      td *= INVERSE_3;

      /* b, d := b + d, b - d */
      t = b + d;
      tt = tb + td + (t < b);
      td = tb - td - (b < d);
      d = b - d;
      b = t;
      tb = tt;

      /* Now:
       *	 b = 4*x1
       *	 c = 2*x2
       *	 d = 4*x3
       */

      /* sb has period 2. */
      b += sb;
      tb += b < sb;
      sb &= maskOffHalf;
      sb |= sb >> (BITS_PER_MP_LIMB >> 1);
      sb += tb;

      /* sc has period 1. */
      c += sc;
      tc += c < sc;
      /* TO DO: choose one of the following alternatives. */
#if 1
      sc = (mp_limb_t)((mp_limb_signed_t)sc >> (BITS_PER_MP_LIMB - 1));
      sc += tc;
#else
      sc = tc - ((mp_limb_signed_t)sc < 0L);
#endif

      /* sd has period 2. */
      d += sd;
      td += d < sd;
      sd &= maskOffHalf;
      sd |= sd >> (BITS_PER_MP_LIMB >> 1);
      sd += td;

      if (i != 0)
	{
	  B[-1] = ob | b << (BITS_PER_MP_LIMB - 2);
	  C[-1] = oc | c << (BITS_PER_MP_LIMB - 1);
	  D[-1] = od | d << (BITS_PER_MP_LIMB - 2);
	}
      ob = b >> 2;
      oc = c >> 1;
      od = d >> 2;

      ++A; ++B; ++C; ++D; ++E;
    }

  /* Handle top words. */
  b = *ptb;
  c = *ptc;
  d = *ptd;

  t = b + d;
  d = b - d;
  b = t;
  b -= c << 3;
  c = (c << 1) - b;
  d *= INVERSE_3;
  t = b + d;
  d = b - d;
  b = t;

  b += sb;
  c += sc;
  d += sd;

  B[-1] = ob | b << (BITS_PER_MP_LIMB - 2);
  C[-1] = oc | c << (BITS_PER_MP_LIMB - 1);
  D[-1] = od | d << (BITS_PER_MP_LIMB - 2);

  b >>= 2;
  c >>= 1;
  d >>= 2;

#if WANT_ASSERT
  ASSERT (b < 2);
  if (l == ls)
    {
      ASSERT (c < 3);
      ASSERT (d < 2);
    }
  else
    {
      ASSERT (c < 2);
      ASSERT (!d);
    }
#endif

  *ptb = b;
  *ptc = c;
  *ptd = d;
}
#endif


/*-- mpn_toom3_mul_n --------------------------------------------------------------*/

/* Multiplies using 5 mults of one third size and so on recursively.
 * p[0..2*n-1] := product of a[0..n-1] and b[0..n-1].
 * No overlap of p[...] with a[...] or b[...].
 * ws is workspace.
 */

/* TO DO: If TOOM3_MUL_THRESHOLD is much bigger than KARATSUBA_MUL_THRESHOLD then the
 *        recursion in mpn_toom3_mul_n() will always bottom out with mpn_kara_mul_n()
 *        because the "n < KARATSUBA_MUL_THRESHOLD" test here will always be false.
 */

#define TOOM3_MUL_REC(p, a, b, n, ws) \
  do {								\
    if (n < KARATSUBA_MUL_THRESHOLD)				\
      mpn_mul_basecase (p, a, n, b, n);				\
    else if (n < TOOM3_MUL_THRESHOLD)				\
      mpn_kara_mul_n (p, a, b, n, ws);				\
    else							\
      mpn_toom3_mul_n (p, a, b, n, ws);				\
  } while (0)

void
#if __STDC__
mpn_toom3_mul_n (mp_ptr p, mp_srcptr a, mp_srcptr b, mp_size_t n, mp_ptr ws)
#else
mpn_toom3_mul_n (p, a, b, n, ws)
     mp_ptr    p;
     mp_srcptr a;
     mp_srcptr b;
     mp_size_t n;
     mp_ptr    ws;
#endif
{
  mp_limb_t cB,cC,cD, dB,dC,dD, tB,tC,tD;
  mp_limb_t *A,*B,*C,*D,*E, *W;
  mp_size_t l,l2,l3,l4,l5,ls;

  SCHEME_BIGNUM_USE_FUEL(n);

  /* Break n words into chunks of size l, l and ls.
   * n = 3*k   => l = k,   ls = k
   * n = 3*k+1 => l = k+1, ls = k-1
   * n = 3*k+2 => l = k+1, ls = k
   */
  {
    mp_limb_t m;

    ASSERT (n >= TOOM3_MUL_THRESHOLD);
    l = ls = n / 3;
    m = n - l * 3;
    if (m != 0)
      ++l;
    if (m == 1)
      --ls;

    l2 = l * 2;
    l3 = l * 3;
    l4 = l * 4;
    l5 = l * 5;
    A = p;
    B = ws;
    C = p + l2;
    D = ws + l2;
    E = p + l4;
    W = ws + l4;
  }

  /** First stage: evaluation at points 0, 1/2, 1, 2, oo. **/
  evaluate3 (A, B, C, &cB, &cC, &cD, a, a + l, a + l2, l, ls);
  evaluate3 (A + l, B + l, C + l, &dB, &dC, &dD, b, b + l, b + l2, l, ls);

  /** Second stage: pointwise multiplies. **/
  TOOM3_MUL_REC(D, C, C + l, l, W);
  tD = cD*dD;
  if (cD) tD += mpn_addmul_1 (D + l, C + l, l, cD);
  if (dD) tD += mpn_addmul_1 (D + l, C, l, dD);
  ASSERT (tD < 49);
  TOOM3_MUL_REC(C, B, B + l, l, W);
  tC = cC*dC;
  /* TO DO: choose one of the following alternatives. */
#if 0
  if (cC) tC += mpn_addmul_1 (C + l, B + l, l, cC);
  if (dC) tC += mpn_addmul_1 (C + l, B, l, dC);
#else
  if (cC)
    {
      if (cC == 1) tC += mpn_add_n (C + l, C + l, B + l, l);
      else tC += add2Times (C + l, C + l, B + l, l);
    }
  if (dC)
    {
      if (dC == 1) tC += mpn_add_n (C + l, C + l, B, l);
      else tC += add2Times (C + l, C + l, B, l);
    }
#endif
  ASSERT (tC < 9);
  TOOM3_MUL_REC(B, A, A + l, l, W);
  tB = cB*dB;
  if (cB) tB += mpn_addmul_1 (B + l, A + l, l, cB);
  if (dB) tB += mpn_addmul_1 (B + l, A, l, dB);
  ASSERT (tB < 49);
  TOOM3_MUL_REC(A, a, b, l, W);
  TOOM3_MUL_REC(E, a + l2, b + l2, ls, W);

  /** Third stage: interpolation. **/
  interpolate3 (A, B, C, D, E, &tB, &tC, &tD, l2, ls << 1);

  /** Final stage: add up the coefficients. **/
  {
    /* mp_limb_t i, x, y; */
    tB += mpn_add_n (p + l, p + l, B, l2);
    tD += mpn_add_n (p + l3, p + l3, D, l2);
    mpn_incr_u (p + l3, tB);
    mpn_incr_u (p + l4, tC);
    mpn_incr_u (p + l5, tD);
  }
}

/*-- mpn_toom3_sqr_n --------------------------------------------------------------*/

/* Like previous function but for squaring */

#define TOOM3_SQR_REC(p, a, n, ws) \
  do {								\
    if (n < KARATSUBA_SQR_THRESHOLD)				\
      mpn_sqr_basecase (p, a, n);				\
    else if (n < TOOM3_SQR_THRESHOLD)				\
      mpn_kara_sqr_n (p, a, n, ws);				\
    else							\
      mpn_toom3_sqr_n (p, a, n, ws);				\
  } while (0)

void
#if __STDC__
mpn_toom3_sqr_n (mp_ptr p, mp_srcptr a, mp_size_t n, mp_ptr ws)
#else
mpn_toom3_sqr_n (p, a, n, ws)
     mp_ptr    p;
     mp_srcptr a;
     mp_size_t n;
     mp_ptr    ws;
#endif
{
  mp_limb_t cB,cC,cD, tB,tC,tD;
  mp_limb_t *A,*B,*C,*D,*E, *W;
  mp_size_t l,l2,l3,l4,l5,ls;

  SCHEME_BIGNUM_USE_FUEL(n);

  /* Break n words into chunks of size l, l and ls.
   * n = 3*k   => l = k,   ls = k
   * n = 3*k+1 => l = k+1, ls = k-1
   * n = 3*k+2 => l = k+1, ls = k
   */
  {
    mp_limb_t m;

    ASSERT (n >= TOOM3_MUL_THRESHOLD);
    l = ls = n / 3;
    m = n - l * 3;
    if (m != 0)
      ++l;
    if (m == 1)
      --ls;

    l2 = l * 2;
    l3 = l * 3;
    l4 = l * 4;
    l5 = l * 5;
    A = p;
    B = ws;
    C = p + l2;
    D = ws + l2;
    E = p + l4;
    W = ws + l4;
  }

  /** First stage: evaluation at points 0, 1/2, 1, 2, oo. **/
  evaluate3 (A, B, C, &cB, &cC, &cD, a, a + l, a + l2, l, ls);

  /** Second stage: pointwise multiplies. **/
  TOOM3_SQR_REC(D, C, l, W);
  tD = cD*cD;
  if (cD) tD += mpn_addmul_1 (D + l, C, l, 2*cD);
  ASSERT (tD < 49);
  TOOM3_SQR_REC(C, B, l, W);
  tC = cC*cC;
  /* TO DO: choose one of the following alternatives. */
#if 0
  if (cC) tC += mpn_addmul_1 (C + l, B, l, 2*cC);
#else
  if (cC >= 1)
    {
      tC += add2Times (C + l, C + l, B, l);
      if (cC == 2)
        tC += add2Times (C + l, C + l, B, l);
    }
#endif
  ASSERT (tC < 9);
  TOOM3_SQR_REC(B, A, l, W);
  tB = cB*cB;
  if (cB) tB += mpn_addmul_1 (B + l, A, l, 2*cB);
  ASSERT (tB < 49);
  TOOM3_SQR_REC(A, a, l, W);
  TOOM3_SQR_REC(E, a + l2, ls, W);

  /** Third stage: interpolation. **/
  interpolate3 (A, B, C, D, E, &tB, &tC, &tD, l2, ls << 1);

  /** Final stage: add up the coefficients. **/
  {
    /* mp_limb_t i, x, y; */
    tB += mpn_add_n (p + l, p + l, B, l2);
    tD += mpn_add_n (p + l3, p + l3, D, l2);
    mpn_incr_u (p + l3, tB);
    mpn_incr_u (p + l4, tC);
    mpn_incr_u (p + l5, tD);
  }
}

void
#if __STDC__
mpn_mul_n (mp_ptr p, mp_srcptr a, mp_srcptr b, mp_size_t n)
#else
mpn_mul_n (p, a, b, n)
     mp_ptr    p;
     mp_srcptr a;
     mp_srcptr b;
     mp_size_t n;
#endif
{
  if (n < KARATSUBA_MUL_THRESHOLD)
    mpn_mul_basecase (p, a, n, b, n);
  else if (n < TOOM3_MUL_THRESHOLD)
    {
      /* Allocate workspace of fixed size on stack: fast! */
#if TUNE_PROGRAM_BUILD
      mp_limb_t ws[2 * (TOOM3_MUL_THRESHOLD_LIMIT-1) + 2 * BITS_PER_MP_LIMB];
#else
      mp_limb_t ws[2 * (TOOM3_MUL_THRESHOLD-1) + 2 * BITS_PER_MP_LIMB];
#endif
      mpn_kara_mul_n (p, a, b, n, ws);
    }
#if WANT_FFT || TUNE_PROGRAM_BUILD
  else if (n < FFT_MUL_THRESHOLD)
#else
  else
#endif
    {
      /* Use workspace of unknown size in heap, as stack space may
       * be limited.  Since n is at least TOOM3_MUL_THRESHOLD, the
       * multiplication will take much longer than malloc()/free().  */
      mp_limb_t wsLen, *ws;
      TMP_DECL (marker);
      TMP_MARK (marker);

      wsLen = 2 * n + 3 * BITS_PER_MP_LIMB;
      ws = (mp_ptr) TMP_ALLOC ((size_t) wsLen * sizeof (mp_limb_t));
      mpn_toom3_mul_n (p, a, b, n, ws);
      TMP_FREE (marker);
    }
#if WANT_FFT || TUNE_PROGRAM_BUILD
  else
    {
      mpn_mul_fft_full (p, a, n, b, n);      
    }
#endif
}


/* mpn_rshift -- Shift right a low-level natural-number integer. */

/* Shift U (pointed to by UP and USIZE limbs long) CNT bits to the right
   and store the USIZE least significant limbs of the result at WP.
   The bits shifted out to the right are returned.

   Argument constraints:
   1. 0 < CNT < BITS_PER_MP_LIMB
   2. If the result is to be written over the input, WP must be <= UP.
*/

mp_limb_t
#if __STDC__
mpn_rshift (register mp_ptr wp,
	    register mp_srcptr up, mp_size_t usize,
	    register unsigned int cnt)
#else
mpn_rshift (wp, up, usize, cnt)
     register mp_ptr wp;
     register mp_srcptr up;
     mp_size_t usize;
     register unsigned int cnt;
#endif
{
  register mp_limb_t high_limb, low_limb;
  register unsigned sh_1, sh_2;
  register mp_size_t i;
  mp_limb_t retval;

#ifdef DEBUG
  if (usize == 0 || cnt == 0)
    abort ();
#endif

  sh_1 = cnt;

#if 0
  if (sh_1 == 0)
    {
      if (wp != up)
	{
	  /* Copy from low end to high end, to allow specified input/output
	     overlapping.  */
	  for (i = 0; i < usize; i++)
	    wp[i] = up[i];
	}
      return usize;
    }
#endif

  wp -= 1;
  sh_2 = BITS_PER_MP_LIMB - sh_1;
  high_limb = up[0];
  retval = high_limb << sh_2;
  low_limb = high_limb;

  for (i = 1; i < usize; i++)
    {
      high_limb = up[i];
      wp[i] = (low_limb >> sh_1) | (high_limb << sh_2);
      low_limb = high_limb;
    }
  wp[i] = low_limb >> sh_1;

  return retval;
}

/* mpn_lshift -- Shift left low level. */

/* Shift U (pointed to by UP and USIZE digits long) CNT bits to the left
   and store the USIZE least significant digits of the result at WP.
   Return the bits shifted out from the most significant digit.

   Argument constraints:
   1. 0 < CNT < BITS_PER_MP_LIMB
   2. If the result is to be written over the input, WP must be >= UP.
*/

mp_limb_t
#if __STDC__
mpn_lshift (register mp_ptr wp,
	    register mp_srcptr up, mp_size_t usize,
	    register unsigned int cnt)
#else
mpn_lshift (wp, up, usize, cnt)
     register mp_ptr wp;
     register mp_srcptr up;
     mp_size_t usize;
     register unsigned int cnt;
#endif
{
  register mp_limb_t high_limb, low_limb;
  register unsigned sh_1, sh_2;
  register mp_size_t i;
  mp_limb_t retval;

#ifdef DEBUG
  if (usize == 0 || cnt == 0)
    abort ();
#endif

  sh_1 = cnt;
#if 0
  if (sh_1 == 0)
    {
      if (wp != up)
	{
	  /* Copy from high end to low end, to allow specified input/output
	     overlapping.  */
	  for (i = usize - 1; i >= 0; i--)
	    wp[i] = up[i];
	}
      return 0;
    }
#endif

  wp += 1;
  sh_2 = BITS_PER_MP_LIMB - sh_1;
  i = usize - 1;
  low_limb = up[i];
  retval = low_limb >> sh_2;
  high_limb = low_limb;
  while (--i >= 0)
    {
      low_limb = up[i];
      wp[i] = (high_limb << sh_1) | (low_limb >> sh_2);
      high_limb = low_limb;
    }
  wp[i] = high_limb << sh_1;

  return retval;
}



/* Conversion of U {up,un} to a string in base b.  Internally, we convert to
     base B = b^m, the largest power of b that fits a limb.  Basic algorithms:

  A) Divide U repeatedly by B, generating a quotient and remainder, until the
     quotient becomes zero.  The remainders hold the converted digits.  Digits
     come out from right to left.  (Used in mpn_sb_get_str.)

  B) Divide U by b^g, for g such that 1/b <= U/b^g < 1, generating a fraction.
     Then develop digits by multiplying the fraction repeatedly by b.  Digits
     come out from left to right.  (Currently not used herein, except for in
     code for converting single limbs to individual digits.)

  C) Compute B^1, B^2, B^4, ..., B^(2^s), for s such that B^(2^s) > sqrt(U).
     Then divide U by B^(2^k), generating an integer quotient and remainder.
     Recursively convert the quotient, then the remainder, using the
     precomputed powers.  Digits come out from left to right.  (Used in
     mpn_dc_get_str.)

  When using algorithm C, algorithm B might be suitable for basecase code,
  since the required b^g power will be readily accessible.

  Optimization ideas:
  1. The recursive function of (C) could avoid TMP allocation:
     a) Overwrite dividend with quotient and remainder, just as permitted by
        mpn_sb_divrem_mn.
     b) If TMP memory is anyway needed, pass it as a parameter, similarly to
        how we do it in Karatsuba multiplication.
  2. Store the powers of (C) in normalized form, with the normalization count.
     Quotients will usually need to be left-shifted before each divide, and
     remainders will either need to be left-shifted of right-shifted.
  3. When b is even, the powers will end up with lots of low zero limbs.  Could
     save significant time in the mpn_tdiv_qr call by stripping these zeros.
  4. In the code for developing digits from a single limb, we could avoid using
     a full umul_ppmm except for the first (or first few) digits, provided base
     is even.  Subsequent digits can be developed using plain multiplication.
     (This saves on register-starved machines (read x86) and on all machines
     that generate the upper product half using a separate instruction (alpha,
     powerpc, IA-64) or lacks such support altogether (sparc64, hppa64).
  5. Separate mpn_dc_get_str basecase code from code for small conversions. The
     former code will have the exact right power readily available in the
     powtab parameter for dividing the current number into a fraction.  Convert
     that using algorithm B.
  6. Completely avoid division.  Compute the inverses of the powers now in
     powtab instead of the actual powers.

  Basic structure of (C):
    mpn_get_str:
      if POW2_P (n)
	...
      else
	if (un < GET_STR_PRECOMPUTE_THRESHOLD)
	  mpn_sb_get_str (str, base, up, un);
	else
	  precompute_power_tables
	  mpn_dc_get_str

    mpn_dc_get_str:
	mpn_tdiv_qr
	if (qn < GET_STR_DC_THRESHOLD)
	  mpn_sb_get_str
	else
	  mpn_dc_get_str
	if (rn < GET_STR_DC_THRESHOLD)
	  mpn_sb_get_str
	else
	  mpn_dc_get_str


  The reason for the two threshold values is the cost of
  precompute_power_tables.  GET_STR_PRECOMPUTE_THRESHOLD will be considerably
  larger than GET_STR_PRECOMPUTE_THRESHOLD.  Do you think I should change
  mpn_dc_get_str to instead look like the following?  */


/* The x86s and m68020 have a quotient and remainder "div" instruction and
   gcc recognises an adjacent "/" and "%" can be combined using that.
   Elsewhere "/" and "%" are either separate instructions, or separate
   libgcc calls (which unfortunately gcc as of version 3.0 doesn't combine).
   A multiply and subtract should be faster than a "%" in those cases.  */
#if HAVE_HOST_CPU_FAMILY_x86            \
  || HAVE_HOST_CPU_m68020               \
  || HAVE_HOST_CPU_m68030               \
  || HAVE_HOST_CPU_m68040               \
  || HAVE_HOST_CPU_m68060               \
  || HAVE_HOST_CPU_m68360 /* CPU32 */
#define udiv_qrnd_unnorm(q,r,n,d)       \
  do {                                  \
    mp_limb_t  __q = (n) / (d);         \
    mp_limb_t  __r = (n) % (d);         \
    (q) = __q;                          \
    (r) = __r;                          \
  } while (0)
#else
#define udiv_qrnd_unnorm(q,r,n,d)       \
  do {                                  \
    mp_limb_t  __q = (n) / (d);         \
    mp_limb_t  __r = (n) - __q*(d);     \
    (q) = __q;                          \
    (r) = __r;                          \
  } while (0)
#endif

/* When to stop divide-and-conquer and call the basecase mpn_get_str.  */
#ifndef GET_STR_DC_THRESHOLD
#define GET_STR_DC_THRESHOLD 15
#endif
/* Whether to bother at all with precomputing powers of the base, or go
   to the basecase mpn_get_str directly.  */
#ifndef GET_STR_PRECOMPUTE_THRESHOLD
#define GET_STR_PRECOMPUTE_THRESHOLD 30
#endif

struct powers
{
  size_t digits_in_base;
  mp_ptr p;
  mp_size_t n;		/* mpz_struct uses int for sizes, but not mpn! */
  int base;
};
typedef struct powers powers_t;

/* Convert {UP,UN} to a string with a base as represented in POWTAB, and put
   the string in STR.  Generate LEN characters, possibly padding with zeros to
   the left.  If LEN is zero, generate as many characters as required.
   Return a pointer immediately after the last digit of the result string.
   Complexity is O(UN^2); intended for small conversions.  */
static unsigned char *
mpn_sb_get_str (unsigned char *str, size_t len,
		mp_ptr up, mp_size_t un,
		const powers_t *powtab)
{
  mp_limb_t rl, ul;
  unsigned char *s;
  int base;
  size_t l;
  /* Allocate memory for largest possible string, given that we only get here
     for operands with un < GET_STR_PRECOMPUTE_THRESHOLD and that the smallest
     base is 3.  7/11 is an approximation to 1/log2(3).  */
#if TUNE_PROGRAM_BUILD
#define BUF_ALLOC (GET_STR_THRESHOLD_LIMIT * BITS_PER_MP_LIMB * 7 / 11)
#else
#define BUF_ALLOC (GET_STR_PRECOMPUTE_THRESHOLD * BITS_PER_MP_LIMB * 7 / 11)
#endif
  unsigned char buf[BUF_ALLOC];
#if TUNE_PROGRAM_BUILD
  mp_limb_t rp[GET_STR_THRESHOLD_LIMIT];
#else
  mp_limb_t rp[GET_STR_PRECOMPUTE_THRESHOLD];
#endif

  base = powtab->base;
  if (base == 10)
    {
      /* Special case code for base==10 so that the compiler has a chance to
	 optimize things.  */

      MPN_COPY (rp + 1, up, un);

      s = buf + BUF_ALLOC;
      while (un > 1)
	{
	  int i;
	  mp_limb_t frac, digit;
	  MPN_DIVREM_OR_PREINV_DIVREM_1 (rp, (mp_size_t) 1, rp + 1, un,
					 MP_BASES_BIG_BASE_10,
					 MP_BASES_BIG_BASE_INVERTED_10,
					 MP_BASES_NORMALIZATION_STEPS_10);
	  un -= rp[un] == 0;
	  frac = (rp[0] + 1) << GMP_NAIL_BITS;
	  s -= MP_BASES_CHARS_PER_LIMB_10;
#if HAVE_HOST_CPU_FAMILY_x86
	  /* The code below turns out to be a bit slower for x86 using gcc.
	     Use plain code.  */
	  i = MP_BASES_CHARS_PER_LIMB_10;
	  do
	    {
	      umul_ppmm (digit, frac, frac, 10);
	      *s++ = digit;
	    }
	  while (--i);
#else
	  /* Use the fact that 10 in binary is 1010, with the lowest bit 0.
	     After a few umul_ppmm, we will have accumulated enough low zeros
	     to use a plain multiply.  */
	  if (MP_BASES_NORMALIZATION_STEPS_10 == 0)
	    {
	      umul_ppmm (digit, frac, frac, 10);
	      *s++ = digit;
	    }
	  if (MP_BASES_NORMALIZATION_STEPS_10 <= 1)
	    {
	      umul_ppmm (digit, frac, frac, 10);
	      *s++ = digit;
	    }
	  if (MP_BASES_NORMALIZATION_STEPS_10 <= 2)
	    {
	      umul_ppmm (digit, frac, frac, 10);
	      *s++ = digit;
	    }
	  if (MP_BASES_NORMALIZATION_STEPS_10 <= 3)
	    {
	      umul_ppmm (digit, frac, frac, 10);
	      *s++ = digit;
	    }
	  i = MP_BASES_CHARS_PER_LIMB_10 - (4-MP_BASES_NORMALIZATION_STEPS_10);
	  frac = (frac + 0xf) >> 4;
	  do
	    {
	      frac *= 10;
	      digit = frac >> (BITS_PER_MP_LIMB - 4);
	      *s++ = digit;
	      frac &= (~(mp_limb_t) 0) >> 4;
	    }
	  while (--i);
#endif
	  s -= MP_BASES_CHARS_PER_LIMB_10;
	}

      ul = rp[1];
      while (ul != 0)
	{
	  udiv_qrnd_unnorm (ul, rl, ul, 10);
	  *--s = rl;
	}
    }
  else /* not base 10 */
    {
      unsigned chars_per_limb;
      mp_limb_t big_base, big_base_inverted;
      unsigned normalization_steps;

      chars_per_limb = __mp_bases[base].chars_per_limb;
      big_base = __mp_bases[base].big_base;
      big_base_inverted = __mp_bases[base].big_base_inverted;
      count_leading_zeros (normalization_steps, big_base);

      MPN_COPY (rp + 1, up, un);

      s = buf + BUF_ALLOC;
      while (un > 1)
	{
	  int i;
	  mp_limb_t frac;
	  MPN_DIVREM_OR_PREINV_DIVREM_1 (rp, (mp_size_t) 1, rp + 1, un,
					 big_base, big_base_inverted,
					 normalization_steps);
	  un -= rp[un] == 0;
	  frac = (rp[0] + 1) << GMP_NAIL_BITS;
	  s -= chars_per_limb;
	  i = chars_per_limb;
	  do
	    {
	      mp_limb_t digit;
	      umul_ppmm (digit, frac, frac, base);
	      *s++ = digit;
	    }
	  while (--i);
	  s -= chars_per_limb;
	}

      ul = rp[1];
      while (ul != 0)
	{
	  udiv_qrnd_unnorm (ul, rl, ul, base);
	  *--s = rl;
	}
    }

  l = buf + BUF_ALLOC - s;
  while (l < len)
    {
      *str++ = 0;
      len--;
    }
  while (l != 0)
    {
      *str++ = *s++;
      l--;
    }
  return str;
}

/* Convert {UP,UN} to a string with a base as represented in POWTAB, and put
   the string in STR.  Generate LEN characters, possibly padding with zeros to
   the left.  If LEN is zero, generate as many characters as required.
   Return a pointer immediately after the last digit of the result string.
   This uses divide-and-conquer and is intended for large conversions.  */
static unsigned char *
mpn_dc_get_str (unsigned char *str, size_t len,
		mp_ptr up, mp_size_t un,
		const powers_t *powtab)
{
  if (un < GET_STR_DC_THRESHOLD)
    {
      if (un != 0)
	str = mpn_sb_get_str (str, len, up, un, powtab);
      else
	{
	  while (len != 0)
	    {
	      *str++ = 0;
	      len--;
	    }
	}
    }
  else
    {
      mp_ptr pwp, qp, rp;
      mp_size_t pwn, qn;

      pwp = powtab->p;
      pwn = powtab->n;

      if (un < pwn || (un == pwn && mpn_cmp (up, pwp, un) < 0))
	{
	  str = mpn_dc_get_str (str, len, up, un, powtab - 1);
	}
      else
	{
	  TMP_DECL (marker);
	  TMP_MARK (marker);
	  qp = TMP_ALLOC_LIMBS (un - pwn + 1);
	  rp = TMP_ALLOC_LIMBS (pwn);

	  mpn_tdiv_qr (qp, rp, 0L, up, un, pwp, pwn);
	  qn = un - pwn; qn += qp[qn] != 0;		/* quotient size */
	  if (len != 0)
	    len = len - powtab->digits_in_base;
	  str = mpn_dc_get_str (str, len, qp, qn, powtab - 1);
	  str = mpn_dc_get_str (str, powtab->digits_in_base, rp, pwn, powtab - 1);
	  TMP_FREE (marker);
	}
    }
  return str;
}

/* There are no leading zeros on the digits generated at str, but that's not
   currently a documented feature.  */

size_t
mpn_get_str (unsigned char *str, int base, mp_ptr up, mp_size_t un)
{
  mp_ptr powtab_mem, powtab_mem_ptr;
  mp_limb_t big_base;
  size_t digits_in_base;
  powers_t powtab[30];
  int pi;
  mp_size_t n;
  mp_ptr p, t;
  size_t out_len;

  /* Special case zero, as the code below doesn't handle it.  */
  if (un == 0)
    {
      str[0] = 0;
      return 1;
    }

  if (POW2_P (base))
    {
      /* The base is a power of 2.  Convert from most significant end.  */
      mp_limb_t n1, n0;
      int bits_per_digit = __mp_bases[base].big_base;
      int cnt;
      int bit_pos;
      mp_size_t i;
      unsigned char *s = str;

      n1 = up[un - 1];
      count_leading_zeros (cnt, n1);

      /* BIT_POS should be R when input ends in least significant nibble,
	 R + bits_per_digit * n when input ends in nth least significant
	 nibble. */

      {
	unsigned long bits;

	bits = GMP_NUMB_BITS * un - cnt + GMP_NAIL_BITS;
	cnt = bits % bits_per_digit;
	if (cnt != 0)
	  bits += bits_per_digit - cnt;
	bit_pos = bits - (un - 1) * GMP_NUMB_BITS;
      }

      /* Fast loop for bit output.  */
      i = un - 1;
      for (;;)
	{
	  bit_pos -= bits_per_digit;
	  while (bit_pos >= 0)
	    {
	      *s++ = (n1 >> bit_pos) & ((1 << bits_per_digit) - 1);
	      bit_pos -= bits_per_digit;
	    }
	  i--;
	  if (i < 0)
	    break;
	  n0 = (n1 << -bit_pos) & ((1 << bits_per_digit) - 1);
	  n1 = up[i];
	  bit_pos += GMP_NUMB_BITS;
	  *s++ = n0 | (n1 >> bit_pos);

	  if (!(i & 0xFF)) SCHEME_BIGNUM_USE_FUEL(1);
	}

      *s = 0;

      return s - str;
    }

  /* General case.  The base is not a power of 2.  */

  if (un < GET_STR_PRECOMPUTE_THRESHOLD)
    {
      struct powers ptab[1];
      ptab[0].base = base;
      return mpn_sb_get_str (str, (size_t) 0, up, un, ptab) - str;
    }

  /* Allocate one large block for the powers of big_base.  With the current
     scheme, we need to allocate twice as much as would be possible if a
     minimal set of powers were generated.  */
#define ALLOC_SIZE (2 * un + 30)
 {
   TMP_DECL (marker);
   TMP_MARK (marker);

   powtab_mem = __GMP_ALLOCATE_FUNC_LIMBS (ALLOC_SIZE);
   powtab_mem_ptr = powtab_mem;

  /* Compute a table of powers: big_base^1, big_base^2, big_base^4, ...,
     big_base^(2^k), for k such that the biggest power is between U and
     sqrt(U).  */

  big_base = __mp_bases[base].big_base;
  digits_in_base = __mp_bases[base].chars_per_limb;

  powtab[0].base = base; /* FIXME: hack for getting base to mpn_sb_get_str */
  powtab[1].p = &big_base;
  powtab[1].n = 1;
  powtab[1].digits_in_base = digits_in_base;
  powtab[1].base = base;
  powtab[2].p = &big_base;
  powtab[2].n = 1;
  powtab[2].digits_in_base = digits_in_base;
  powtab[2].base = base;
  n = 1;
  pi = 2;
  p = &big_base;
  for (;;)
    {
      ++pi;
      t = powtab_mem_ptr;
      powtab_mem_ptr += 2 * n;
      mpn_sqr_n (t, p, n);
      n *= 2; n -= t[n - 1] == 0;
      digits_in_base *= 2;
      p = t;
      powtab[pi].p = p;
      powtab[pi].n = n;
      powtab[pi].digits_in_base = digits_in_base;
      powtab[pi].base = base;

      if (2 * n > un)
	break;
    }
  ASSERT_ALWAYS (ALLOC_SIZE > powtab_mem_ptr - powtab_mem);

  /* Using our precomputed powers, now in powtab[], convert our number.  */
  out_len = mpn_dc_get_str (str, 0, up, un, powtab + pi) - str;

  __GMP_FREE_FUNC_LIMBS (powtab_mem, ALLOC_SIZE);
  
  TMP_FREE(marker);
 }

  return out_len;
}


/* When to switch to sub-quadratic code.  This counts characters/digits in
   the input string, not limbs as most other *_THRESHOLD.  */
#ifndef SET_STR_THRESHOLD
#define SET_STR_THRESHOLD 4000
#endif

/* Don't define this to anything but 1 for now.  In order to make other values
   work well, either the convert_blocks function should be generazed to handle
   larger blocks than chars_per_limb, or the basecase code should be broken out
   of the main function.  Also note that this must be a power of 2.  */
#ifndef SET_STR_BLOCK_SIZE
#define SET_STR_BLOCK_SIZE 1	/* Must be a power of 2. */
#endif


/* This check interferes with expression based values of SET_STR_THRESHOLD
   used for tuning and measuring.
#if SET_STR_BLOCK_SIZE >= SET_STR_THRESHOLD
These values are silly.
The sub-quadratic code would recurse to itself.
#endif
*/

static mp_size_t
convert_blocks (mp_ptr dp, const unsigned char *str, size_t str_len, int base);

mp_size_t
mpn_set_str (mp_ptr rp, const unsigned char *str, size_t str_len, int base)
{
  mp_size_t size;
  mp_limb_t big_base;
  int chars_per_limb;
  mp_limb_t res_digit;

  ASSERT (base >= 2);
  ASSERT (base < numberof (__mp_bases));
  ASSERT (str_len >= 1);

  big_base = __mp_bases[base].big_base;
  chars_per_limb = __mp_bases[base].chars_per_limb;

  size = 0;

  if (POW2_P (base))
    {
      /* The base is a power of 2.  Read the input string from least to most
	 significant character/digit.  */

      const unsigned char *s;
      int next_bitpos;
      int bits_per_indigit = big_base;

      res_digit = 0;
      next_bitpos = 0;

      for (s = str + str_len - 1; s >= str; s--)
	{
	  int inp_digit = *s;

	  res_digit |= ((mp_limb_t) inp_digit << next_bitpos) & GMP_NUMB_MASK;
	  next_bitpos += bits_per_indigit;
	  if (next_bitpos >= GMP_NUMB_BITS)
	    {
	      rp[size++] = res_digit;
	      next_bitpos -= GMP_NUMB_BITS;
	      res_digit = inp_digit >> (bits_per_indigit - next_bitpos);
	    }

	  if (!((long)s & 0xFF)) SCHEME_BIGNUM_USE_FUEL(1);
	}

      if (res_digit != 0)
	rp[size++] = res_digit;
      return size;
    }
  else
    {
      /* General case.  The base is not a power of 2.  */

      if (str_len < SET_STR_THRESHOLD)
	{
	  size_t i;
	  int j;
	  mp_limb_t cy_limb;

	  for (i = chars_per_limb; i < str_len; i += chars_per_limb)
	    {
	      res_digit = *str++;
	      if (base == 10)
		{ /* This is a common case.
		     Help the compiler to avoid multiplication.  */
		  for (j = MP_BASES_CHARS_PER_LIMB_10 - 1; j != 0; j--)
		    res_digit = res_digit * 10 + *str++;
		}
	      else
		{
		  for (j = chars_per_limb - 1; j != 0; j--)
		    res_digit = res_digit * base + *str++;
		}

	      if (size == 0)
		{
		  if (res_digit != 0)
		    {
		      rp[0] = res_digit;
		      size = 1;
		    }
		}
	      else
		{
#if HAVE_NATIVE_mpn_mul_1c
		  cy_limb = mpn_mul_1c (rp, rp, size, big_base, res_digit);
#else
		  cy_limb = mpn_mul_1 (rp, rp, size, big_base);
		  cy_limb += mpn_add_1 (rp, rp, size, res_digit);
#endif
		  if (cy_limb != 0)
		    rp[size++] = cy_limb;
		}
	    }

	  big_base = base;
	  res_digit = *str++;
	  if (base == 10)
	    { /* This is a common case.
		 Help the compiler to avoid multiplication.  */
	      for (j = str_len - (i - MP_BASES_CHARS_PER_LIMB_10) - 1; j > 0; j--)
		{
		  res_digit = res_digit * 10 + *str++;
		  big_base *= 10;
		}
	    }
	  else
	    {
	      for (j = str_len - (i - chars_per_limb) - 1; j > 0; j--)
		{
		  res_digit = res_digit * base + *str++;
		  big_base *= base;
		}
	    }

	  if (size == 0)
	    {
	      if (res_digit != 0)
		{
		  rp[0] = res_digit;
		  size = 1;
		}
	    }
	  else
	    {
#if HAVE_NATIVE_mpn_mul_1c
	      cy_limb = mpn_mul_1c (rp, rp, size, big_base, res_digit);
#else
	      cy_limb = mpn_mul_1 (rp, rp, size, big_base);
	      cy_limb += mpn_add_1 (rp, rp, size, res_digit);
#endif
	      if (cy_limb != 0)
		rp[size++] = cy_limb;
	    }
	  return size;
	}
      else
	{
	  /* Sub-quadratic code.  */

	  mp_ptr dp;
	  mp_size_t dsize;
	  mp_ptr xp, tp;
	  mp_size_t step;
	  mp_size_t i;
	  size_t alloc;
	  mp_size_t n;
	  mp_ptr pow_mem;
	  TMP_DECL (marker);
	  TMP_MARK (marker);

	  alloc = (str_len + chars_per_limb - 1) / chars_per_limb;
	  alloc = 2 * alloc;
	  dp = __GMP_ALLOCATE_FUNC_LIMBS (alloc);

#if SET_STR_BLOCK_SIZE == 1
	  dsize = convert_blocks (dp, str, str_len, base);
#else
	  {
	    const unsigned char *s;
	    mp_ptr ddp = dp;

	    s = str + str_len;
	    while (s - str >  SET_STR_BLOCK_SIZE * chars_per_limb)
	      {
		s -= SET_STR_BLOCK_SIZE * chars_per_limb;
		mpn_set_str (ddp, s, SET_STR_BLOCK_SIZE * chars_per_limb, base);
		ddp += SET_STR_BLOCK_SIZE;
	      }
	    ddp += mpn_set_str (ddp, str, s - str, base);
	    dsize = ddp - dp;
	  }
#endif

	  /* Allocate space for powers of big_base.  Could trim this in two
	     ways:
	     1. Only really need 2^ceil(log2(dsize)) bits for the largest
		power.
	     2. Only the variable to get the largest power need that much
		memory.  The other variable needs half as much.  Need just
		figure out which of xp and tp will hold the last one.
	     Net space savings would be in the range 1/4 to 5/8 of current
	     allocation, depending on how close to the next power of 2 that
	     dsize is.  */
	  pow_mem = __GMP_ALLOCATE_FUNC_LIMBS (2 * alloc);
	  xp = pow_mem;
	  tp = pow_mem + alloc;

	  xp[0] = big_base;
	  n = 1;
	  step = 1;
#if SET_STR_BLOCK_SIZE != 1
	  for (i = SET_STR_BLOCK_SIZE; i > 1; i >>= 1)
	    {
	      mpn_sqr_n (tp, xp, n);
	      n = 2 * n;
	      n -= tp[n - 1] == 0;

	      step = 2 * step;
	      MP_PTR_SWAP (tp, xp);
	    }
#endif

	  /* Multiply every second limb block, each `step' limbs large by the
	     base power currently in xp[], then add this to the adjacent block.
	     We thereby convert from dsize blocks in base big_base, to dsize/2
	     blocks in base big_base^2, then to dsize/4 blocks in base
	     big_base^4, etc, etc.  */

	  if (step < dsize)
	    {
	      for (;;)
		{
		  for (i = 0; i < dsize - step; i += 2 * step)
		    {
		      mp_ptr bp = dp + i;
		      mp_size_t m = dsize - i - step;
		      mp_size_t hi;
		      if (n >= m)
			{
			  mpn_mul (tp, xp, n, bp + step, m);
			  mpn_add (bp, tp, n + m, bp, n);
			  hi = i + n + m;
			  dsize = hi;
			  dsize -= dp[dsize - 1] == 0;
			}
		      else
			{
			  mpn_mul_n (tp, xp, bp + step, n);
			  mpn_add (bp, tp, n + n, bp, n);
			}
		    }

		  step = 2 * step;
		  if (! (step < dsize))
		    break;

		  mpn_sqr_n (tp, xp, n);
		  n = 2 * n;
		  n -= tp[n - 1] == 0;
		  MP_PTR_SWAP (tp, xp);
		}
	    }

	  MPN_NORMALIZE (dp, dsize);
	  MPN_COPY (rp, dp, dsize);
	  __GMP_FREE_FUNC_LIMBS (pow_mem, 2 * alloc);
	  __GMP_FREE_FUNC_LIMBS (dp, alloc);
	  TMP_FREE (marker);
	  return dsize;
	}
    }
}

static mp_size_t
convert_blocks (mp_ptr dp, const unsigned char *str, size_t str_len, int base)
{
  int chars_per_limb;
  mp_size_t i;
  int j;
  int ds;
  mp_size_t dsize;
  mp_limb_t res_digit;

  chars_per_limb = __mp_bases[base].chars_per_limb;

  dsize = str_len / chars_per_limb;
  ds = str_len % chars_per_limb;

  if (ds != 0)
    {
      res_digit = *str++;
      for (j = ds - 1; j != 0; j--)
	res_digit = res_digit * base + *str++;
      dp[dsize] = res_digit;
    }

  if (base == 10)
    {
      for (i = dsize - 1; i >= 0; i--)
	{
	  res_digit = *str++;
	  for (j = MP_BASES_CHARS_PER_LIMB_10 - 1; j != 0; j--)
	    res_digit = res_digit * 10 + *str++;
	  dp[i] = res_digit;
	}
    }
  else
    {
      for (i = dsize - 1; i >= 0; i--)
	{
	  res_digit = *str++;
	  for (j = chars_per_limb - 1; j != 0; j--)
	    res_digit = res_digit * base + *str++;
	  dp[i] = res_digit;
	}
    }

  return dsize + (ds != 0);
}


/* mpn_tdiv_qr -- Divide the numerator (np,nn) by the denominator (dp,dn) and
   write the nn-dn+1 quotient limbs at qp and the dn remainder limbs at rp.  If
   qxn is non-zero, generate that many fraction limbs and append them after the
   other quotient limbs, and update the remainder accordningly.  The input
   operands are unaffected.

   Preconditions:
   1. The most significant limb of of the divisor must be non-zero.
   2. No argument overlap is permitted.  (??? relax this ???)
   3. nn >= dn, even if qxn is non-zero.  (??? relax this ???)

   The time complexity of this is O(qn*qn+M(dn,qn)), where M(m,n) is the time
   complexity of multiplication. */

#ifndef BZ_THRESHOLD
#define BZ_THRESHOLD (7 * KARATSUBA_MUL_THRESHOLD)
#endif

/* Extract the middle limb from ((h,,l) << cnt) */
#define SHL(h,l,cnt) \
  ((h << cnt) | ((l >> 1) >> ((~cnt) & (BITS_PER_MP_LIMB - 1))))

void
#if __STDC__
mpn_tdiv_qr (mp_ptr qp, mp_ptr rp, mp_size_t qxn,
	     mp_srcptr np, mp_size_t nn, mp_srcptr dp, mp_size_t dn)
#else
mpn_tdiv_qr (qp, rp, qxn, np, nn, dp, dn)
     mp_ptr qp;
     mp_ptr rp;
     mp_size_t qxn;
     mp_srcptr np;
     mp_size_t nn;
     mp_srcptr dp;
     mp_size_t dn;
#endif
{
  /* FIXME:
     1. qxn
     2. pass allocated storage in additional parameter?
  */
#ifdef DEBUG
  if (qxn != 0)
    abort ();
#endif

  switch (dn)
    {
    case 0:
#if 0
      DIVIDE_BY_ZERO;
#endif
      return;

    case 1:
      {
	rp[0] = mpn_divmod_1 (qp, np, nn, dp[0]);
	return;
      }

    case 2:
      {
	int cnt;
	mp_ptr n2p, d2p;
	mp_limb_t qhl, cy;
	TMP_DECL (marker);
	TMP_MARK (marker);
	count_leading_zeros (cnt, dp[dn - 1]);
	if (cnt != 0)
	  {
	    d2p = (mp_ptr) TMP_ALLOC (dn * BYTES_PER_MP_LIMB);
	    mpn_lshift (d2p, dp, dn, cnt);
	    n2p = (mp_ptr) TMP_ALLOC ((nn + 1) * BYTES_PER_MP_LIMB);
	    cy = mpn_lshift (n2p, np, nn, cnt);
	    n2p[nn] = cy;
	    qhl = mpn_divrem_2 (qp, 0L, n2p, nn + (cy != 0), d2p);
	    if (cy == 0)
	      qp[nn - 2] = qhl;	/* always store nn-dn+1 quotient limbs */
	  }
	else
	  {
	    d2p = (mp_ptr) dp;
	    n2p = (mp_ptr) TMP_ALLOC (nn * BYTES_PER_MP_LIMB);
	    MPN_COPY (n2p, np, nn);
	    qhl = mpn_divrem_2 (qp, 0L, n2p, nn, d2p);
	    qp[nn - 2] = qhl;	/* always store nn-dn+1 quotient limbs */
	  }

	if (cnt != 0)
	  mpn_rshift (rp, n2p, dn, cnt);
	else
	  MPN_COPY (rp, n2p, dn);
	TMP_FREE (marker);
	return;
      }

    default:
      {
	int adjust;
	TMP_DECL (marker);
	TMP_MARK (marker);
	adjust = np[nn - 1] >= dp[dn - 1];	/* conservative tests for quotient size */
	if (nn + adjust >= 2 * dn)
	  {
	    mp_ptr n2p, d2p;
	    mp_limb_t cy;
	    int cnt;
	    count_leading_zeros (cnt, dp[dn - 1]);

	    qp[nn - dn] = 0;			/* zero high quotient limb */
	    if (cnt != 0)			/* normalize divisor if needed */
	      {
		d2p = (mp_ptr) TMP_ALLOC (dn * BYTES_PER_MP_LIMB);
		mpn_lshift (d2p, dp, dn, cnt);
		n2p = (mp_ptr) TMP_ALLOC ((nn + 1) * BYTES_PER_MP_LIMB);
		cy = mpn_lshift (n2p, np, nn, cnt);
		n2p[nn] = cy;
		nn += adjust;
	      }
	    else
	      {
		d2p = (mp_ptr) dp;
		n2p = (mp_ptr) TMP_ALLOC ((nn + 1) * BYTES_PER_MP_LIMB);
		MPN_COPY (n2p, np, nn);
		n2p[nn] = 0;
		nn += adjust;
	      }

	    if (dn == 2)
	      mpn_divrem_2 (qp, 0L, n2p, nn, d2p);
	    else if (dn < BZ_THRESHOLD)
	      mpn_sb_divrem_mn (qp, n2p, nn, d2p, dn);
	    else
	      {
		/* Perform 2*dn / dn limb divisions as long as the limbs
		   in np last.  */
		mp_ptr q2p = qp + nn - 2 * dn;
		n2p += nn - 2 * dn;
		mpn_bz_divrem_n (q2p, n2p, d2p, dn);
		nn -= dn;
		while (nn >= 2 * dn)
		  {
		    mp_limb_t c;
		    q2p -= dn;  n2p -= dn;
		    c = mpn_bz_divrem_n (q2p, n2p, d2p, dn);
		    ASSERT_ALWAYS (c == 0);
		    nn -= dn;
		  }

		if (nn != dn)
		  {
		    n2p -= nn - dn;
		    /* In theory, we could fall out to the cute code below
		       since we now have exactly the situation that code
		       is designed to handle.  We botch this badly and call
		       the basic mpn_sb_divrem_mn!  */
		    if (dn == 2)
		      mpn_divrem_2 (qp, 0L, n2p, nn, d2p);
		    else
		      mpn_sb_divrem_mn (qp, n2p, nn, d2p, dn);
		  }
	      }


	    if (cnt != 0)
	      mpn_rshift (rp, n2p, dn, cnt);
	    else
	      MPN_COPY (rp, n2p, dn);
	    TMP_FREE (marker);
	    return;
	  }

	/* When we come here, the numerator/partial remainder is less
	   than twice the size of the denominator.  */

	  {
	    /* Problem:

	       Divide a numerator N with nn limbs by a denominator D with dn
	       limbs forming a quotient of nn-dn+1 limbs.  When qn is small
	       compared to dn, conventional division algorithms perform poorly.
	       We want an algorithm that has an expected running time that is
	       dependent only on qn.  It is assumed that the most significant
	       limb of the numerator is smaller than the most significant limb
	       of the denominator.

	       Algorithm (very informally stated):

	       1) Divide the 2 x qn most significant limbs from the numerator
		  by the qn most significant limbs from the denominator.  Call
		  the result qest.  This is either the correct quotient, but
		  might be 1 or 2 too large.  Compute the remainder from the
		  division.  (This step is implemented by a mpn_divrem call.)

	       2) Is the most significant limb from the remainder < p, where p
		  is the product of the most significant limb from the quotient
		  and the next(d).  (Next(d) denotes the next ignored limb from
		  the denominator.)  If it is, decrement qest, and adjust the
		  remainder accordingly.

	       3) Is the remainder >= qest?  If it is, qest is the desired
		  quotient.  The algorithm terminates.

	       4) Subtract qest x next(d) from the remainder.  If there is
		  borrow out, decrement qest, and adjust the remainder
		  accordingly.

	       5) Skip one word from the denominator (i.e., let next(d) denote
		  the next less significant limb.  */

	    mp_size_t qn;
	    mp_ptr n2p, d2p;
	    mp_ptr tp;
	    mp_limb_t cy;
	    mp_size_t in, rn;
	    mp_limb_t quotient_too_large;
	    int cnt;

	    qn = nn - dn;
	    qp[qn] = 0;				/* zero high quotient limb */
	    qn += adjust;			/* qn cannot become bigger */

	    if (qn == 0)
	      {
		MPN_COPY (rp, np, dn);
		TMP_FREE (marker);
		return;
	      }

	    in = dn - qn;		/* (at least partially) ignored # of limbs in ops */
	    /* Normalize denominator by shifting it to the left such that its
	       most significant bit is set.  Then shift the numerator the same
	       amount, to mathematically preserve quotient.  */
	    count_leading_zeros (cnt, dp[dn - 1]);
	    if (cnt != 0)
	      {
		d2p = (mp_ptr) TMP_ALLOC (qn * BYTES_PER_MP_LIMB);

		mpn_lshift (d2p, dp + in, qn, cnt);
		d2p[0] |= dp[in - 1] >> (BITS_PER_MP_LIMB - cnt);

		n2p = (mp_ptr) TMP_ALLOC ((2 * qn + 1) * BYTES_PER_MP_LIMB);
		cy = mpn_lshift (n2p, np + nn - 2 * qn, 2 * qn, cnt);
		if (adjust)
		  {
		    n2p[2 * qn] = cy;
		    n2p++;
		  }
		else
		  {
		    n2p[0] |= np[nn - 2 * qn - 1] >> (BITS_PER_MP_LIMB - cnt);
		  }
	      }
	    else
	      {
		d2p = (mp_ptr) dp + in;

		n2p = (mp_ptr) TMP_ALLOC ((2 * qn + 1) * BYTES_PER_MP_LIMB);
		MPN_COPY (n2p, np + nn - 2 * qn, 2 * qn);
		if (adjust)
		  {
		    n2p[2 * qn] = 0;
		    n2p++;
		  }
	      }

	    /* Get an approximate quotient using the extracted operands.  */
	    if (qn == 1)
	      {
		mp_limb_t q0, r0;
		mp_limb_t gcc272bug_n1, gcc272bug_n0, gcc272bug_d0;
		/* Due to a gcc 2.7.2.3 reload pass bug, we have to use some
		   temps here.  This doesn't hurt code quality on any machines
		   so we do it unconditionally.  */
		gcc272bug_n1 = n2p[1];
		gcc272bug_n0 = n2p[0];
		gcc272bug_d0 = d2p[0];
		udiv_qrnnd (q0, r0, gcc272bug_n1, gcc272bug_n0, gcc272bug_d0);
		n2p[0] = r0;
		qp[0] = q0;
	      }
	    else if (qn == 2)
	      mpn_divrem_2 (qp, 0L, n2p, 4L, d2p);
	    else if (qn < BZ_THRESHOLD)
	      mpn_sb_divrem_mn (qp, n2p, qn * 2, d2p, qn);
	    else
	      mpn_bz_divrem_n (qp, n2p, d2p, qn);

	    rn = qn;
	    /* Multiply the first ignored divisor limb by the most significant
	       quotient limb.  If that product is > the partial remainder's
	       most significant limb, we know the quotient is too large.  This
	       test quickly catches most cases where the quotient is too large;
	       it catches all cases where the quotient is 2 too large.  */
	    {
	      mp_limb_t dl, x;
	      mp_limb_t h, l;

	      if (in - 2 < 0)
		dl = 0;
	      else
		dl = dp[in - 2];

	      x = SHL (dp[in - 1], dl, cnt);
	      umul_ppmm (h, l, x, qp[qn - 1]);

	      if (n2p[qn - 1] < h)
		{
		  mp_limb_t cy;

		  mpn_decr_u (qp, (mp_limb_t) 1);
		  cy = mpn_add_n (n2p, n2p, d2p, qn);
		  if (cy)
		    {
		      /* The partial remainder is safely large.  */
		      n2p[qn] = cy;
		      ++rn;
		    }
		}
	    }

	    quotient_too_large = 0;
	    if (cnt != 0)
	      {
		mp_limb_t cy1, cy2;

		/* Append partially used numerator limb to partial remainder.  */
		cy1 = mpn_lshift (n2p, n2p, rn, BITS_PER_MP_LIMB - cnt);
		n2p[0] |= np[in - 1] & (~(mp_limb_t) 0 >> cnt);

		/* Update partial remainder with partially used divisor limb.  */
		cy2 = mpn_submul_1 (n2p, qp, qn, dp[in - 1] & (~(mp_limb_t) 0 >> cnt));
		if (qn != rn)
		  {
#ifdef DEBUG
		    if (n2p[qn] < cy2)
		      abort ();
#endif
		    n2p[qn] -= cy2;
		  }
		else
		  {
		    n2p[qn] = cy1 - cy2;

		    quotient_too_large = (cy1 < cy2);
		    ++rn;
		  }
		--in;
	      }
	    /* True: partial remainder now is neutral, i.e., it is not shifted up.  */

	    tp = (mp_ptr) TMP_ALLOC (dn * BYTES_PER_MP_LIMB);

	    if (in < qn)
	      {
		if (in == 0)
		  {
		    MPN_COPY (rp, n2p, rn);
#ifdef DEBUG
		    if (rn != dn)
		      abort ();
#endif
		    goto foo;
		  }
		mpn_mul (tp, qp, qn, dp, in);
	      }
	    else
	      mpn_mul (tp, dp, in, qp, qn);

	    cy = mpn_sub (n2p, n2p, rn, tp + in, qn);
	    MPN_COPY (rp + in, n2p, dn - in);
	    quotient_too_large |= cy;
	    cy = mpn_sub_n (rp, np, tp, in);
	    cy = mpn_sub_1 (rp + in, rp + in, rn, cy);
	    quotient_too_large |= cy;
	  foo:
	    if (quotient_too_large)
	      {
		mpn_decr_u (qp, (mp_limb_t) 1);
		mpn_add_n (rp, rp, dp, dn);
	      }
	  }
	TMP_FREE (marker);
	return;
      }
    }
}


/* Square roots table.  Generated by the following program:
#include "gmp.h"
main(){mpz_t x;int i;mpz_init(x);for(i=64;i<256;i++){mpz_set_ui(x,256*i);
mpz_sqrt(x,x);mpz_out_str(0,10,x);printf(",");if(i%16==15)printf("\n");}}
*/
static const unsigned char approx_tab[192] =
  {
    128,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,
    143,144,144,145,146,147,148,149,150,150,151,152,153,154,155,155,
    156,157,158,159,160,160,161,162,163,163,164,165,166,167,167,168,
    169,170,170,171,172,173,173,174,175,176,176,177,178,178,179,180,
    181,181,182,183,183,184,185,185,186,187,187,188,189,189,190,191,
    192,192,193,193,194,195,195,196,197,197,198,199,199,200,201,201,
    202,203,203,204,204,205,206,206,207,208,208,209,209,210,211,211,
    212,212,213,214,214,215,215,216,217,217,218,218,219,219,220,221,
    221,222,222,223,224,224,225,225,226,226,227,227,228,229,229,230,
    230,231,231,232,232,233,234,234,235,235,236,236,237,237,238,238,
    239,240,240,241,241,242,242,243,243,244,244,245,245,246,246,247,
    247,248,248,249,249,250,250,251,251,252,252,253,253,254,254,255
  };

#define HALF_NAIL (GMP_NAIL_BITS / 2)

/* same as mpn_sqrtrem, but for size=1 and {np, 1} normalized */
static mp_size_t
mpn_sqrtrem1 (mp_ptr sp, mp_ptr rp, mp_srcptr np)
{
  mp_limb_t np0, s, r, q, u;
  int prec;

  ASSERT (np[0] >= GMP_NUMB_HIGHBIT / 2);
  ASSERT (GMP_LIMB_BITS >= 16);

  /* first compute a 8-bit approximation from the high 8-bits of np[0] */
  np0 = np[0] << GMP_NAIL_BITS;
  q = np0 >> (GMP_LIMB_BITS - 8);
  /* 2^6 = 64 <= q < 256 = 2^8 */
  s = approx_tab[q - 64];				/* 128 <= s < 255 */
  r = (np0 >> (GMP_LIMB_BITS - 16)) - s * s;		/* r <= 2*s */
  if (r > 2 * s)
    {
      r -= 2 * s + 1;
      s++;
    }

  prec = 8;
  np0 <<= 2 * prec;
  while (2 * prec < GMP_LIMB_BITS)
    {
      /* invariant: s has prec bits, and r <= 2*s */
      r = (r << prec) + (np0 >> (GMP_LIMB_BITS - prec));
      np0 <<= prec;
      u = 2 * s;
      q = r / u;
      u = r - q * u;
      s = (s << prec) + q;
      u = (u << prec) + (np0 >> (GMP_LIMB_BITS - prec));
      q = q * q;
      r = u - q;
      if (u < q)
	{
	  r += 2 * s - 1;
	  s --;
	}
      np0 <<= prec;
      prec = 2 * prec;
    }

  ASSERT (2 * prec == GMP_LIMB_BITS); /* GMP_LIMB_BITS must be a power of 2 */

  /* normalize back, assuming GMP_NAIL_BITS is even */
  ASSERT (GMP_NAIL_BITS % 2 == 0);
  sp[0] = s >> HALF_NAIL;
  u = s - (sp[0] << HALF_NAIL); /* s mod 2^HALF_NAIL */
  r += u * ((sp[0] << (HALF_NAIL + 1)) + u);
  r = r >> GMP_NAIL_BITS;

  if (rp != NULL)
    rp[0] = r;
  return r != 0 ? 1 : 0;
}


#define Prec (GMP_NUMB_BITS >> 1)

/* same as mpn_sqrtrem, but for size=2 and {np, 2} normalized
   return cc such that {np, 2} = sp[0]^2 + cc*2^GMP_NUMB_BITS + rp[0] */
static mp_limb_t
mpn_sqrtrem2 (mp_ptr sp, mp_ptr rp, mp_srcptr np)
{
  mp_limb_t qhl, q, u, np0;
  int cc;

  ASSERT (np[1] >= GMP_NUMB_HIGHBIT / 2);

  np0 = np[0];
  mpn_sqrtrem1 (sp, rp, np + 1);
  qhl = 0;
  while (rp[0] >= sp[0])
    {
      qhl++;
      rp[0] -= sp[0];
    }
  /* now rp[0] < sp[0] < 2^Prec */
  rp[0] = (rp[0] << Prec) + (np0 >> Prec);
  u = 2 * sp[0];
  q = rp[0] / u;
  u = rp[0] - q * u;
  q += (qhl & 1) << (Prec - 1);
  qhl >>= 1; /* if qhl=1, necessary q=0 as qhl*2^Prec + q <= 2^Prec */
  /* now we have (initial rp[0])<<Prec + np0>>Prec = (qhl<<Prec + q) * (2sp[0]) + u */
  sp[0] = ((sp[0] + qhl) << Prec) + q;
  cc = u >> Prec;
  rp[0] = ((u << Prec) & GMP_NUMB_MASK) + (np0 & (((mp_limb_t) 1 << Prec) - 1));
  /* subtract q * q or qhl*2^(2*Prec) from rp */
  cc -= mpn_sub_1 (rp, rp, 1, q * q) + qhl;
  /* now subtract 2*q*2^Prec + 2^(2*Prec) if qhl is set */
  if (cc < 0)
    {
      cc += sp[0] != 0 ? mpn_add_1 (rp, rp, 1, sp[0]) : 1;
      cc += mpn_add_1 (rp, rp, 1, --sp[0]);
    }

  return cc;
}

/* writes in {sp, n} the square root (rounded towards zero) of {np, 2n},
   and in {np, n} the low n limbs of the remainder, returns the high
   limb of the remainder (which is 0 or 1).
   Assumes {np, 2n} is normalized, i.e. np[2n-1] >= B/4
   where B=2^GMP_NUMB_BITS.  */
static mp_limb_t
mpn_dc_sqrtrem (mp_ptr sp, mp_ptr np, mp_size_t n)
{
  mp_limb_t q;			/* carry out of {sp, n} */
  int c, b;			/* carry out of remainder */
  mp_size_t l, h;

  ASSERT (np[2 * n - 1] >= GMP_NUMB_HIGHBIT / 2);

  if (n == 1)
    c = mpn_sqrtrem2 (sp, np, np);
  else
    {
      l = n / 2;
      h = n - l;
      q = mpn_dc_sqrtrem (sp + l, np + 2 * l, h);
      if (q != 0)
        mpn_sub_n (np + 2 * l, np + 2 * l, sp + l, h);
      q += mpn_divrem (sp, 0, np + l, n, sp + l, h);
      c = sp[0] & 1;
      mpn_rshift (sp, sp, l, 1);
      sp[l - 1] |= (q << (GMP_NUMB_BITS - 1)) & GMP_NUMB_MASK;
      q >>= 1;
      if (c != 0)
        c = mpn_add_n (np + l, np + l, sp + l, h);
      mpn_sqr_n (np + n, sp, l);
      b = q + mpn_sub_n (np, np, np + n, 2 * l);
      c -= (l == h) ? b : mpn_sub_1 (np + 2 * l, np + 2 * l, 1, b);
      q = mpn_add_1 (sp + l, sp + l, h, q);

      if (c < 0)
        {
          c += mpn_addmul_1 (np, sp, n, 2) + 2 * q;
          c -= mpn_sub_1 (np, np, n, 1);
          q -= mpn_sub_1 (sp, sp, n, 1);
        }
    }

  return c;
}


mp_size_t
mpn_sqrtrem (mp_ptr sp, mp_ptr rp, mp_srcptr np, mp_size_t nn)
{
  mp_limb_t *tp, s0[1], cc, high, rl;
  int c;
  mp_size_t rn, tn;
  TMP_DECL (marker);

  ASSERT (nn >= 0);

  /* If OP is zero, both results are zero.  */
  if (nn == 0)
    return 0;

  ASSERT (np[nn - 1] != 0);
  ASSERT (rp == NULL || MPN_SAME_OR_SEPARATE_P (np, rp, nn));
  ASSERT (rp == NULL || ! MPN_OVERLAP_P (sp, (nn + 1) / 2, rp, nn));
  ASSERT (! MPN_OVERLAP_P (sp, (nn + 1) / 2, np, nn));

  high = np[nn - 1];
  if (nn == 1 && (high & GMP_NUMB_HIGHBIT))
    return mpn_sqrtrem1 (sp, rp, np);
  count_leading_zeros (c, high);
  c -= GMP_NAIL_BITS;

  c = c / 2; /* we have to shift left by 2c bits to normalize {np, nn} */
  tn = (nn + 1) / 2; /* 2*tn is the smallest even integer >= nn */

  TMP_MARK (marker);
  if (nn % 2 != 0 || c > 0)
    {
      tp = TMP_ALLOC_LIMBS (2 * tn);
      tp[0] = 0;	     /* needed only when 2*tn > nn, but saves a test */
      if (c != 0)
	mpn_lshift (tp + 2 * tn - nn, np, nn, 2 * c);
      else
	MPN_COPY (tp + 2 * tn - nn, np, nn);
      rl = mpn_dc_sqrtrem (sp, tp, tn);
      /* We have 2^(2k)*N = S^2 + R where k = c + (2tn-nn)*GMP_NUMB_BITS/2,
	 thus 2^(2k)*N = (S-s0)^2 + 2*S*s0 - s0^2 + R where s0=S mod 2^k */
      c += (nn % 2) * GMP_NUMB_BITS / 2;		/* c now represents k */
      s0[0] = sp[0] & (((mp_limb_t) 1 << c) - 1);	/* S mod 2^k */
      rl += mpn_addmul_1 (tp, sp, tn, 2 * s0[0]);	/* R = R + 2*s0*S */
      cc = mpn_submul_1 (tp, s0, 1, s0[0]);
      rl -= (tn > 1) ? mpn_sub_1 (tp + 1, tp + 1, tn - 1, cc) : cc;
      mpn_rshift (sp, sp, tn, c);
      tp[tn] = rl;
      if (rp == NULL)
	rp = tp;
      c = c << 1;
      if (c < GMP_NUMB_BITS)
	tn++;
      else
	{
	  tp++;
	  c -= GMP_NUMB_BITS;
	}
      if (c != 0)
	mpn_rshift (rp, tp, tn, c);
      else
	MPN_COPY_INCR (rp, tp, tn);
      rn = tn;
    }
  else
    {
      if (rp == NULL)
	rp = TMP_ALLOC_LIMBS (nn);
      if (rp != np)
	MPN_COPY (rp, np, nn);
      rn = tn + (rp[tn] = mpn_dc_sqrtrem (sp, rp, tn));
    }

  MPN_NORMALIZE (rp, rn);

  TMP_FREE (marker);
  return rn;
}

/* mpn_bz_divrem_n and auxilliary routines. */

/*
[1] Fast Recursive Division, by Christoph Burnikel and Joachim Ziegler,
    Technical report MPI-I-98-1-022, october 1998.
    http://www.mpi-sb.mpg.de/~ziegler/TechRep.ps.gz
*/

static mp_limb_t mpn_bz_div_3_halves_by_2
  _PROTO ((mp_ptr qp, mp_ptr np, mp_srcptr dp, mp_size_t n));


/* mpn_bz_divrem_n(n) calls 2*mul(n/2)+2*div(n/2), thus to be faster than
   div(n) = 4*div(n/2), we need mul(n/2) to be faster than the classic way,
   i.e. n/2 >= KARATSUBA_MUL_THRESHOLD */
#ifndef BZ_THRESHOLD
#define BZ_THRESHOLD (7 * KARATSUBA_MUL_THRESHOLD)
#endif

/* mpn_bz_divrem_n - Implements algorithm of page 8 in [1]: divides (np,2n)
   by (dp,n) and puts the quotient in (qp,n), the remainder in (np,n).
   Returns most significant limb of the quotient, which is 0 or 1.
   Requires that the most significant bit of the divisor is set.  */

mp_limb_t
#if __STDC__
mpn_bz_divrem_n (mp_ptr qp, mp_ptr np, mp_srcptr dp, mp_size_t n)
#else
mpn_bz_divrem_n (qp, np, dp, n)
     mp_ptr qp;
     mp_ptr np;
     mp_srcptr dp;
     mp_size_t n;
#endif
{
  mp_limb_t qhl, cc;

  if (n % 2 != 0)
    {
      qhl = mpn_bz_divrem_n (qp + 1, np + 2, dp + 1, n - 1);
      cc = mpn_submul_1 (np + 1, qp + 1, n - 1, dp[0]);
      cc = mpn_sub_1 (np + n, np + n, 1, cc);
      if (qhl) cc += mpn_sub_1 (np + n, np + n, 1, dp[0]);
      while (cc)
        {
          qhl -= mpn_sub_1 (qp + 1, qp + 1, n - 1, (mp_limb_t) 1);
          cc -= mpn_add_n (np + 1, np + 1, dp, n);
        }
      qhl += mpn_add_1 (qp + 1, qp + 1, n - 1,
                        mpn_sb_divrem_mn (qp, np, n + 1, dp, n));
    }
  else
    {
      mp_size_t n2 = n/2;
      qhl = mpn_bz_div_3_halves_by_2 (qp + n2, np + n2, dp, n2);
      qhl += mpn_add_1 (qp + n2, qp + n2, n2,
                        mpn_bz_div_3_halves_by_2 (qp, np, dp, n2));
    }
  return qhl;
}


/* divides (np, 3n) by (dp, 2n) and puts the quotient in (qp, n),
   the remainder in (np, 2n) */

static mp_limb_t
#if __STDC__
mpn_bz_div_3_halves_by_2 (mp_ptr qp, mp_ptr np, mp_srcptr dp, mp_size_t n)
#else
mpn_bz_div_3_halves_by_2 (qp, np, dp, n)
     mp_ptr qp;
     mp_ptr np;
     mp_srcptr dp;
     mp_size_t n;
#endif
{
  mp_size_t twon = n + n; 
  mp_limb_t qhl, cc;
  mp_ptr tmp;
  TMP_DECL (marker);

  TMP_MARK (marker);
  if (n < BZ_THRESHOLD)
    qhl = mpn_sb_divrem_mn (qp, np + n, twon, dp + n, n);
  else
    qhl = mpn_bz_divrem_n (qp, np + n, dp + n, n);
  tmp = (mp_ptr) TMP_ALLOC (twon * BYTES_PER_MP_LIMB);
  mpn_mul_n (tmp, qp, dp, n);
  cc = mpn_sub_n (np, np, tmp, twon);
  TMP_FREE (marker);
  if (qhl) cc += mpn_sub_n (np + n, np + n, dp, n);
  while (cc)
    {
      qhl -= mpn_sub_1 (qp, qp, n, (mp_limb_t) 1);
      cc -= mpn_add_n (np, np, dp, twon);
    }
  return qhl;
}

/* mpn_sb_divrem_mn -- Divide natural numbers, producing both remainder and
   quotient. */

/* Divide num (NP/NSIZE) by den (DP/DSIZE) and write
   the NSIZE-DSIZE least significant quotient limbs at QP
   and the DSIZE long remainder at NP.  If QEXTRA_LIMBS is
   non-zero, generate that many fraction bits and append them after the
   other quotient limbs.
   Return the most significant limb of the quotient, this is always 0 or 1.

   Preconditions:
   0. NSIZE >= DSIZE.
   1. The most significant bit of the divisor must be set.
   2. QP must either not overlap with the input operands at all, or
      QP + DSIZE >= NP must hold true.  (This means that it's
      possible to put the quotient in the high part of NUM, right after the
      remainder in NUM.
   3. NSIZE >= DSIZE, even if QEXTRA_LIMBS is non-zero.
   4. DSIZE >= 2.  */


#define PREINVERT_VIABLE \
  (UDIV_TIME > 2 * UMUL_TIME + 6 /* && ! TARGET_REGISTER_STARVED */)

mp_limb_t
#if __STDC__
mpn_sb_divrem_mn (mp_ptr qp,
	       mp_ptr np, mp_size_t nsize,
	       mp_srcptr dp, mp_size_t dsize)
#else
mpn_sb_divrem_mn (qp, np, nsize, dp, dsize)
     mp_ptr qp;
     mp_ptr np;
     mp_size_t nsize;
     mp_srcptr dp;
     mp_size_t dsize;
#endif
{
  mp_limb_t most_significant_q_limb = 0;
  mp_size_t i;
  mp_limb_t dx, d1, n0;
  mp_limb_t dxinv = 0;
  int have_preinv;

  ASSERT_ALWAYS (dsize > 2);

  np += nsize - dsize;
  dx = dp[dsize - 1];
  d1 = dp[dsize - 2];
  n0 = np[dsize - 1];

  if (n0 >= dx)
    {
      if (n0 > dx || mpn_cmp (np, dp, dsize - 1) >= 0)
	{
	  mpn_sub_n (np, np, dp, dsize);
	  most_significant_q_limb = 1;
	}
    }

  /* If multiplication is much faster than division, preinvert the
     most significant divisor limb before entering the loop.  */
  if (PREINVERT_VIABLE)
    {
      have_preinv = 0;
      if ((UDIV_TIME - (2 * UMUL_TIME + 6)) * (nsize - dsize) > UDIV_TIME)
	{
	  invert_limb (dxinv, dx);
	  have_preinv = 1;
	}
    }

  for (i = nsize - dsize - 1; i >= 0; i--)
    {
      mp_limb_t q;
      mp_limb_t nx;
      mp_limb_t cy_limb;

      nx = np[dsize - 1];
      np--;

      if (nx == dx)
	{
	  /* This might over-estimate q, but it's probably not worth
	     the extra code here to find out.  */
	  q = ~(mp_limb_t) 0;

#if 1
	  cy_limb = mpn_submul_1 (np, dp, dsize, q);
#else
	  /* This should be faster on many machines */
	  cy_limb = mpn_sub_n (np + 1, np + 1, dp, dsize);
	  cy = mpn_add_n (np, np, dp, dsize);
	  np[dsize] += cy;
#endif

	  if (nx != cy_limb)
	    {
	      mpn_add_n (np, np, dp, dsize);
	      q--;
	    }

	  qp[i] = q;
	}
      else
	{
	  mp_limb_t rx, r1, r0, p1, p0;

          /* "workaround" avoids a problem with gcc 2.7.2.3 i386 register
             usage when np[dsize-1] is used in an asm statement like
             umul_ppmm in udiv_qrnnd_preinv.  The symptom is seg faults due
             to registers being clobbered.  gcc 2.95 i386 doesn't have the
             problem. */
          {
            mp_limb_t  workaround = np[dsize - 1];
            if (PREINVERT_VIABLE && have_preinv)
              udiv_qrnnd_preinv (q, r1, nx, workaround, dx, dxinv);
            else
              udiv_qrnnd (q, r1, nx, workaround, dx);
          }
	  umul_ppmm (p1, p0, d1, q);

	  r0 = np[dsize - 2];
	  rx = 0;
	  if (r1 < p1 || (r1 == p1 && r0 < p0))
	    {
	      p1 -= p0 < d1;
	      p0 -= d1;
	      q--;
	      r1 += dx;
	      rx = r1 < dx;
	    }

	  p1 += r0 < p0;	/* cannot carry! */
	  rx -= r1 < p1;	/* may become 11..1 if q is still too large */
	  r1 -= p1;
	  r0 -= p0;

	  cy_limb = mpn_submul_1 (np, dp, dsize - 2, q);

	  {
	    mp_limb_t cy1, cy2;
	    cy1 = r0 < cy_limb;
	    r0 -= cy_limb;
	    cy2 = r1 < cy1;
	    r1 -= cy1;
	    np[dsize - 1] = r1;
	    np[dsize - 2] = r0;
	    if (cy2 != rx)
	      {
		mpn_add_n (np, np, dp, dsize);
		q--;
	      }
	  }
	  qp[i] = q;
	}
    }

  /* ______ ______ ______
    |__rx__|__r1__|__r0__|		partial remainder
	    ______ ______
	 - |__p1__|__p0__|		partial product to subtract
	    ______ ______
	 - |______|cylimb|		

     rx is -1, 0 or 1.  If rx=1, then q is correct (it should match
     carry out).  If rx=-1 then q is too large.  If rx=0, then q might
     be too large, but it is most likely correct.
  */

  return most_significant_q_limb;
}

/* mpn_divrem_1(quot_ptr, qsize, dividend_ptr, dividend_size, divisor_limb) --
   Divide (DIVIDEND_PTR,,DIVIDEND_SIZE) by DIVISOR_LIMB.
   Write DIVIDEND_SIZE limbs of quotient at QUOT_PTR.
   Return the single-limb remainder.
   There are no constraints on the value of the divisor.

   QUOT_PTR and DIVIDEND_PTR might point to the same limb.
 */

/* __gmpn_divmod_1_internal(quot_ptr,dividend_ptr,dividend_size,divisor_limb)
   Divide (DIVIDEND_PTR,,DIVIDEND_SIZE) by DIVISOR_LIMB.
   Write DIVIDEND_SIZE limbs of quotient at QUOT_PTR.
   Return the single-limb remainder.
   There are no constraints on the value of the divisor.

   QUOT_PTR and DIVIDEND_PTR might point to the same limb. */

#ifndef UMUL_TIME
#define UMUL_TIME 1
#endif

#ifndef UDIV_TIME
#define UDIV_TIME UMUL_TIME
#endif

static mp_limb_t
#if __STDC__
__gmpn_divmod_1_internal (mp_ptr quot_ptr,
	      mp_srcptr dividend_ptr, mp_size_t dividend_size,
	      mp_limb_t divisor_limb)
#else
__gmpn_divmod_1_internal (quot_ptr, dividend_ptr, dividend_size, divisor_limb)
     mp_ptr quot_ptr;
     mp_srcptr dividend_ptr;
     mp_size_t dividend_size;
     mp_limb_t divisor_limb;
#endif
{
  mp_size_t i;
  mp_limb_t n1, n0, r;

  /* ??? Should this be handled at all?  Rely on callers?  */
  if (dividend_size == 0)
    return 0;

  /* If multiplication is much faster than division, and the
     dividend is large, pre-invert the divisor, and use
     only multiplications in the inner loop.  */

  /* This test should be read:
       Does it ever help to use udiv_qrnnd_preinv?
	 && Does what we save compensate for the inversion overhead?  */
  if (UDIV_TIME > (2 * UMUL_TIME + 6)
      && (UDIV_TIME - (2 * UMUL_TIME + 6)) * dividend_size > UDIV_TIME)
    {
      int normalization_steps;

      count_leading_zeros (normalization_steps, divisor_limb);
      if (normalization_steps != 0)
	{
	  mp_limb_t divisor_limb_inverted;

	  divisor_limb <<= normalization_steps;
	  invert_limb (divisor_limb_inverted, divisor_limb);

	  n1 = dividend_ptr[dividend_size - 1];
	  r = n1 >> (BITS_PER_MP_LIMB - normalization_steps);

	  /* Possible optimization:
	     if (r == 0
	     && divisor_limb > ((n1 << normalization_steps)
			     | (dividend_ptr[dividend_size - 2] >> ...)))
	     ...one division less... */

	  for (i = dividend_size - 2; i >= 0; i--)
	    {
	      n0 = dividend_ptr[i];
	      udiv_qrnnd_preinv (quot_ptr[i + 1], r, r,
				 ((n1 << normalization_steps)
				  | (n0 >> (BITS_PER_MP_LIMB - normalization_steps))),
				 divisor_limb, divisor_limb_inverted);
	      n1 = n0;
	    }
	  udiv_qrnnd_preinv (quot_ptr[0], r, r,
			     n1 << normalization_steps,
			     divisor_limb, divisor_limb_inverted);
	  return r >> normalization_steps;
	}
      else
	{
	  mp_limb_t divisor_limb_inverted;

	  invert_limb (divisor_limb_inverted, divisor_limb);

	  i = dividend_size - 1;
	  r = dividend_ptr[i];

	  if (r >= divisor_limb)
	    r = 0;
	  else
	    {
	      quot_ptr[i] = 0;
	      i--;
	    }

	  for (; i >= 0; i--)
	    {
	      n0 = dividend_ptr[i];
	      udiv_qrnnd_preinv (quot_ptr[i], r, r,
				 n0, divisor_limb, divisor_limb_inverted);
	    }
	  return r;
	}
    }
  else
    {
      if (UDIV_NEEDS_NORMALIZATION)
	{
	  int normalization_steps;

	  count_leading_zeros (normalization_steps, divisor_limb);
	  if (normalization_steps != 0)
	    {
	      divisor_limb <<= normalization_steps;

	      n1 = dividend_ptr[dividend_size - 1];
	      r = n1 >> (BITS_PER_MP_LIMB - normalization_steps);

	      /* Possible optimization:
		 if (r == 0
		 && divisor_limb > ((n1 << normalization_steps)
				 | (dividend_ptr[dividend_size - 2] >> ...)))
		 ...one division less... */

	      for (i = dividend_size - 2; i >= 0; i--)
		{
		  n0 = dividend_ptr[i];
		  udiv_qrnnd (quot_ptr[i + 1], r, r,
			      ((n1 << normalization_steps)
			       | (n0 >> (BITS_PER_MP_LIMB - normalization_steps))),
			      divisor_limb);
		  n1 = n0;
		}
	      udiv_qrnnd (quot_ptr[0], r, r,
			  n1 << normalization_steps,
			  divisor_limb);
	      return r >> normalization_steps;
	    }
	}
      /* No normalization needed, either because udiv_qrnnd doesn't require
	 it, or because DIVISOR_LIMB is already normalized.  */

      i = dividend_size - 1;
      r = dividend_ptr[i];

      if (r >= divisor_limb)
	r = 0;
      else
	{
	  quot_ptr[i] = 0;
	  i--;
	}

      for (; i >= 0; i--)
	{
	  n0 = dividend_ptr[i];
	  udiv_qrnnd (quot_ptr[i], r, r, n0, divisor_limb);
	}
      return r;
    }
}



mp_limb_t
#if __STDC__
mpn_divrem_1 (mp_ptr qp, mp_size_t qxn,
	      mp_srcptr np, mp_size_t nn,
	      mp_limb_t d)
#else
mpn_divrem_1 (qp, qxn, np, nn, d)
     mp_ptr qp;
     mp_size_t qxn;
     mp_srcptr np;
     mp_size_t nn;
     mp_limb_t d;
#endif
{
  mp_limb_t rlimb;
  mp_size_t i;

  /* Develop integer part of quotient.  */
  rlimb = __gmpn_divmod_1_internal (qp + qxn, np, nn, d);

  /* Develop fraction part of quotient.  This is not as fast as it should;
     the preinvert stuff from __gmpn_divmod_1_internal ought to be used here
     too.  */
  if (UDIV_NEEDS_NORMALIZATION)
    {
      int normalization_steps;

      count_leading_zeros (normalization_steps, d);
      if (normalization_steps != 0)
	{
	  d <<= normalization_steps;
	  rlimb <<= normalization_steps;

	  for (i = qxn - 1; i >= 0; i--)
	    udiv_qrnnd (qp[i], rlimb, rlimb, 0, d);

	  return rlimb >> normalization_steps;
	}
      else
	/* fall out */
	;
    }

  for (i = qxn - 1; i >= 0; i--)
    udiv_qrnnd (qp[i], rlimb, rlimb, 0, d);

  return rlimb;
}

/* mpn_divrem_2 -- Divide natural numbers, producing both remainder and
   quotient.  The divisor is two limbs. */

/* Divide num (NP/NSIZE) by den (DP/2) and write
   the NSIZE-2 least significant quotient limbs at QP
   and the 2 long remainder at NP.  If QEXTRA_LIMBS is
   non-zero, generate that many fraction bits and append them after the
   other quotient limbs.
   Return the most significant limb of the quotient, this is always 0 or 1.

   Preconditions:
   0. NSIZE >= 2.
   1. The most significant bit of the divisor must be set.
   2. QP must either not overlap with the input operands at all, or
      QP + 2 >= NP must hold true.  (This means that it's
      possible to put the quotient in the high part of NUM, right after the
      remainder in NUM.
   3. NSIZE >= 2, even if QEXTRA_LIMBS is non-zero.  */

mp_limb_t
#if __STDC__
mpn_divrem_2 (mp_ptr qp, mp_size_t qxn,
	      mp_ptr np, mp_size_t nsize,
	      mp_srcptr dp)
#else
mpn_divrem_2 (qp, qxn, np, nsize, dp)
     mp_ptr qp;
     mp_size_t qxn;
     mp_ptr np;
     mp_size_t nsize;
     mp_srcptr dp;
#endif
{
  mp_limb_t most_significant_q_limb = 0;
  mp_size_t i;
  mp_limb_t n1, n0, n2;
  mp_limb_t d1, d0;
  mp_limb_t d1inv = 0;
  int have_preinv;

  np += nsize - 2;
  d1 = dp[1];
  d0 = dp[0];
  n1 = np[1];
  n0 = np[0];

  if (n1 >= d1 && (n1 > d1 || n0 >= d0))
    {
      sub_ddmmss (n1, n0, n1, n0, d1, d0);
      most_significant_q_limb = 1;
    }

  /* If multiplication is much faster than division, preinvert the most 
     significant divisor limb before entering the loop.  */
  if (UDIV_TIME > 2 * UMUL_TIME + 6)
    {
      have_preinv = 0;
      if ((UDIV_TIME - (2 * UMUL_TIME + 6)) * (nsize - 2) > UDIV_TIME)
	{
	  invert_limb (d1inv, d1);
	  have_preinv = 1;
	}
    }

  for (i = qxn + nsize - 2 - 1; i >= 0; i--)
    {
      mp_limb_t q;
      mp_limb_t r;

      if (i >= qxn)
	np--;
      else
	np[0] = 0;

      if (n1 == d1)
	{
	  /* Q should be either 111..111 or 111..110.  Need special treatment
	     of this rare case as normal division would give overflow.  */
	  q = ~(mp_limb_t) 0;

	  r = n0 + d1;
	  if (r < d1)	/* Carry in the addition? */
	    {
	      add_ssaaaa (n1, n0, r - d0, np[0], 0, d0);
	      qp[i] = q;
	      continue;
	    }
	  n1 = d0 - (d0 != 0);
	  n0 = -d0;
	}
      else
	{
	  if (UDIV_TIME > 2 * UMUL_TIME + 6 && have_preinv)
	    udiv_qrnnd_preinv (q, r, n1, n0, d1, d1inv);
	  else
	    udiv_qrnnd (q, r, n1, n0, d1);
	  umul_ppmm (n1, n0, d0, q);
	}

      n2 = np[0];

    q_test:
      if (n1 > r || (n1 == r && n0 > n2))
	{
	  /* The estimated Q was too large.  */
	  q--;

	  sub_ddmmss (n1, n0, n1, n0, 0, d0);
	  r += d1;
	  if (r >= d1)	/* If not carry, test Q again.  */
	    goto q_test;
	}

      qp[i] = q;
      sub_ddmmss (n1, n0, r, n2, n1, n0);
    }
  np[1] = n1;
  np[0] = n0;

  return most_significant_q_limb;
}

/* mpn_mul_basecase -- Internal routine to multiply two natural numbers
   of length m and n. */

/* Handle simple cases with traditional multiplication.

   This is the most critical code of multiplication.  All multiplies rely on
   this, both small and huge.  Small ones arrive here immediately, huge ones
   arrive here as this is the base case for Karatsuba's recursive algorithm. */

void
#if __STDC__
mpn_mul_basecase (mp_ptr prodp,
		     mp_srcptr up, mp_size_t usize,
		     mp_srcptr vp, mp_size_t vsize)
#else
mpn_mul_basecase (prodp, up, usize, vp, vsize)
     mp_ptr prodp;
     mp_srcptr up;
     mp_size_t usize;
     mp_srcptr vp;
     mp_size_t vsize;
#endif
{
  /* We first multiply by the low order one or two limbs, as the result can
     be stored, not added, to PROD.  We also avoid a loop for zeroing this
     way.  */
#if HAVE_NATIVE_mpn_mul_2
  if (vsize >= 2)
    {
      prodp[usize + 1] = mpn_mul_2 (prodp, up, usize, vp[0], vp[1]);
      prodp += 2, vp += 2, vsize -= 2;
    }
  else
    {
      prodp[usize] = mpn_mul_1 (prodp, up, usize, vp[0]);
      return;
    }
#else
  prodp[usize] = mpn_mul_1 (prodp, up, usize, vp[0]);
  prodp += 1, vp += 1, vsize -= 1;
#endif

#if HAVE_NATIVE_mpn_addmul_2
  while (vsize >= 2)
    {
      prodp[usize + 1] = mpn_addmul_2 (prodp, up, usize, vp[0], vp[1]);
      prodp += 2, vp += 2, vsize -= 2;
    }
  if (vsize != 0)
    prodp[usize] = mpn_addmul_1 (prodp, up, usize, vp[0]);
#else
  /* For each iteration in the loop, multiply U with one limb from V, and
     add the result to PROD.  */
  while (vsize != 0)
    {
      prodp[usize] = mpn_addmul_1 (prodp, up, usize, vp[0]);
      prodp += 1, vp += 1, vsize -= 1;
    }
#endif
}


/* mpn_sqr_basecase -- Internal routine to square two natural numbers
   of length m and n. */


#define SQR_KARATSUBA_THRESHOLD KARATSUBA_SQR_THRESHOLD 

void
mpn_sqr_basecase (mp_ptr prodp, mp_srcptr up, mp_size_t n)
{
  mp_size_t i;

  ASSERT (n >= 1);
  ASSERT (! MPN_OVERLAP_P (prodp, 2*n, up, n));

  {
    /* N.B.!  We need the superfluous indirection through argh to work around
       a reloader bug in GCC 2.7.*.  */
#if GMP_NAIL_BITS == 0
    mp_limb_t ul, argh;
    ul = up[0];
    umul_ppmm (argh, prodp[0], ul, ul);
    prodp[1] = argh;
#else
    mp_limb_t ul, lpl;
    ul = up[0];
    umul_ppmm (prodp[1], lpl, ul, ul << GMP_NAIL_BITS);
    prodp[0] = lpl >> GMP_NAIL_BITS;
#endif
  }
  if (n > 1)
    {
      mp_limb_t tarr[2 * SQR_KARATSUBA_THRESHOLD];
      mp_ptr tp = tarr;
      mp_limb_t cy;

      /* must fit 2*n limbs in tarr */
      ASSERT (n <= SQR_KARATSUBA_THRESHOLD);

      cy = mpn_mul_1 (tp, up + 1, n - 1, up[0]);
      tp[n - 1] = cy;
      for (i = 2; i < n; i++)
	{
	  mp_limb_t cy;
	  cy = mpn_addmul_1 (tp + 2 * i - 2, up + i, n - i, up[i - 1]);
	  tp[n + i - 2] = cy;
	}
#if HAVE_NATIVE_mpn_sqr_diagonal
      mpn_sqr_diagonal (prodp + 2, up + 1, n - 1);
#else
      for (i = 1; i < n; i++)
	{
#if GMP_NAIL_BITS == 0
	  mp_limb_t ul;
	  ul = up[i];
	  umul_ppmm (prodp[2 * i + 1], prodp[2 * i], ul, ul);
#else
	  mp_limb_t ul, lpl;
	  ul = up[i];
	  umul_ppmm (prodp[2 * i + 1], lpl, ul, ul << GMP_NAIL_BITS);
	  prodp[2 * i] = lpl >> GMP_NAIL_BITS;
#endif
	}
#endif
      {
	mp_limb_t cy;
	cy = mpn_lshift (tp, tp, 2 * n - 2, 1);
	cy += mpn_add_n (prodp + 1, prodp + 1, tp, 2 * n - 2);
	prodp[2 * n - 1] += cy;
      }
    }
}

#if 0

void
#if __STDC__
mpn_sqr_basecase (mp_ptr prodp, mp_srcptr up, mp_size_t n)
#else
mpn_sqr_basecase (prodp, up, n)
     mp_ptr prodp;
     mp_srcptr up;
     mp_size_t n;
#endif
{
  mp_size_t i;

  {
    /* N.B.!  We need the superfluous indirection through argh to work around
       a reloader bug in GCC 2.7.*.  */
    mp_limb_t x;
    mp_limb_t argh;
    x = up[0];
    umul_ppmm (argh, prodp[0], x, x);
    prodp[1] = argh;
  }
  if (n > 1)
    {
      mp_limb_t tarr[2 * KARATSUBA_SQR_THRESHOLD];
      mp_ptr tp = tarr;
      mp_limb_t cy;

      /* must fit 2*n limbs in tarr */
      ASSERT (n <= KARATSUBA_SQR_THRESHOLD);

      cy = mpn_mul_1 (tp, up + 1, n - 1, up[0]);
      tp[n - 1] = cy;
      for (i = 2; i < n; i++)
	{
	  mp_limb_t cy;
	  cy = mpn_addmul_1 (tp + 2 * i - 2, up + i, n - i, up[i - 1]);
	  tp[n + i - 2] = cy;
	}
      for (i = 1; i < n; i++)
	{
	  mp_limb_t x;
	  x = up[i];
	  umul_ppmm (prodp[2 * i + 1], prodp[2 * i], x, x);
	}
      {
	mp_limb_t cy;
	cy = mpn_lshift (tp, tp, 2 * n - 2, 1);
	cy += mpn_add_n (prodp + 1, prodp + 1, tp, 2 * n - 2);
	prodp[2 * n - 1] += cy;
      }
    }
}

#endif

/* mpn_mul_1 -- Multiply a limb vector with a single limb and
   store the product in a second limb vector. */

mp_limb_t
mpn_mul_1 (mp_ptr res_ptr, mp_srcptr s1_ptr, mp_size_t s1_size, mp_limb_t s2_limb)
{
  register mp_limb_t cy_limb;
  register mp_size_t j;
  register mp_limb_t prod_high, prod_low;

  SCHEME_BIGNUM_USE_FUEL(s1_size);

  /* The loop counter and index J goes from -S1_SIZE to -1.  This way
     the loop becomes faster.  */
  j = -s1_size;

  /* Offset the base pointers to compensate for the negative indices.  */
  s1_ptr -= j;
  res_ptr -= j;

  cy_limb = 0;
  do
    {
      umul_ppmm (prod_high, prod_low, s1_ptr[j], s2_limb);

      prod_low += cy_limb;
      cy_limb = (prod_low < cy_limb) + prod_high;

      res_ptr[j] = prod_low;
    }
  while (++j != 0);

  return cy_limb;
}

/* mpn_addmul_1 -- multiply the S1_SIZE long limb vector pointed to by S1_PTR
   by S2_LIMB, add the S1_SIZE least significant limbs of the product to the
   limb vector pointed to by RES_PTR.  Return the most significant limb of
   the product, adjusted for carry-out from the addition. */

mp_limb_t
mpn_addmul_1 (mp_ptr rp, mp_srcptr up, mp_size_t n, mp_limb_t vl)
{
  mp_limb_t ul, cl, hpl, lpl, rl;
 
  SCHEME_BIGNUM_USE_FUEL(n);
 
  cl = 0;
  do
    {
      ul = *up++;
      umul_ppmm (hpl, lpl, ul, vl);
                                                                                
      lpl += cl;
      cl = (lpl < cl) + hpl;
                                                                                
      rl = *rp;
      lpl = rl + lpl;
      cl += lpl < rl;
      *rp++ = lpl;
    }
  while (--n != 0);
                                                                                
  return cl;
}

/* mpn_submul_1 -- multiply the S1_SIZE long limb vector pointed to by S1_PTR
   by S2_LIMB, subtract the S1_SIZE least significant limbs of the product
   from the limb vector pointed to by RES_PTR.  Return the most significant
   limb of the product, adjusted for carry-out from the subtraction.
 */

mp_limb_t
mpn_submul_1 (mp_ptr res_ptr, mp_srcptr s1_ptr, mp_size_t s1_size, mp_limb_t s2_limb)
{
  register mp_limb_t cy_limb;
  register mp_size_t j;
  register mp_limb_t prod_high, prod_low;
  register mp_limb_t x;

  SCHEME_BIGNUM_USE_FUEL(s1_size);

  /* The loop counter and index J goes from -SIZE to -1.  This way
     the loop becomes faster.  */
  j = -s1_size;

  /* Offset the base pointers to compensate for the negative indices.  */
  res_ptr -= j;
  s1_ptr -= j;

  cy_limb = 0;
  do
    {
      umul_ppmm (prod_high, prod_low, s1_ptr[j], s2_limb);

      prod_low += cy_limb;
      cy_limb = (prod_low < cy_limb) + prod_high;

      x = res_ptr[j];
      prod_low = x - prod_low;
      cy_limb += (prod_low > x);
      res_ptr[j] = prod_low;
    }
  while (++j != 0);

  return cy_limb;
}

/* mpn_divexact_by3 -- mpn division by 3, expecting no remainder. */


/* Multiplicative inverse of 3, modulo 2^BITS_PER_MP_LIMB.
   0xAAAAAAAB for 32 bits, 0xAAAAAAAAAAAAAAAB for 64 bits. */
#define INVERSE_3      ((MP_LIMB_T_MAX / 3) * 2 + 1)


/* The "c += ..."s are adding the high limb of 3*l to c.  That high limb
   will be 0, 1 or 2.  Doing two separate "+="s seems to turn out better
   code on gcc (as of 2.95.2 at least).

   When a subtraction of a 0,1,2 carry value causes a borrow, that leaves a
   limb value of either 0xFF...FF or 0xFF...FE and the multiply by INVERSE_3
   gives 0x55...55 or 0xAA...AA respectively, producing a further borrow of
   only 0 or 1 respectively.  Hence the carry out of each stage and for the
   return value is always only 0, 1 or 2.  */

mp_limb_t
#if __STDC__
mpn_divexact_by3c (mp_ptr dst, mp_srcptr src, mp_size_t size, mp_limb_t c)
#else
mpn_divexact_by3c (dst, src, size, c)
     mp_ptr    dst;
     mp_srcptr src;
     mp_size_t size;
     mp_limb_t c;
#endif
{
  mp_size_t  i;

  SCHEME_BIGNUM_USE_FUEL(size);

  ASSERT (size >= 1);

  i = 0;
  do
    {
      mp_limb_t  l, s;

      s = src[i];
      l = s - c;
      c = (l > s);

      l *= INVERSE_3;
      dst[i] = l;

      c += (l > MP_LIMB_T_MAX/3);
      c += (l > (MP_LIMB_T_MAX/3)*2);
    }
  while (++i < size);

  return c;
}

/* mpn_mul -- Multiply two natural numbers. */

/* Multiply the natural numbers u (pointed to by UP, with UN limbs) and v
   (pointed to by VP, with VN limbs), and store the result at PRODP.  The
   result is UN + VN limbs.  Return the most significant limb of the result.

   NOTE: The space pointed to by PRODP is overwritten before finished with U
   and V, so overlap is an error.

   Argument constraints:
   1. UN >= VN.
   2. PRODP != UP and PRODP != VP, i.e. the destination must be distinct from
      the multiplier and the multiplicand.  */

void
#if __STDC__
mpn_sqr_n (mp_ptr prodp,
         mp_srcptr up, mp_size_t un)
#else
mpn_sqr_n (prodp, up, un)
     mp_ptr prodp;
     mp_srcptr up;
     mp_size_t un;
#endif
{
  if (un < KARATSUBA_SQR_THRESHOLD)
    { /* plain schoolbook multiplication */
      if (un == 0)
	return;
      mpn_sqr_basecase (prodp, up, un);
    }
  else if (un < TOOM3_SQR_THRESHOLD)
    { /* karatsuba multiplication */
      mp_ptr tspace;
      TMP_DECL (marker);
      TMP_MARK (marker);
      tspace = (mp_ptr) TMP_ALLOC (2 * (un + BITS_PER_MP_LIMB) * BYTES_PER_MP_LIMB);
      mpn_kara_sqr_n (prodp, up, un, tspace);
      TMP_FREE (marker);
    }
#if WANT_FFT || TUNE_PROGRAM_BUILD
  else if (un < FFT_SQR_THRESHOLD)
#else
  else
#endif
    { /* toom3 multiplication */
      mp_ptr tspace;
      TMP_DECL (marker);
      TMP_MARK (marker);
      tspace = (mp_ptr) TMP_ALLOC (2 * (un + BITS_PER_MP_LIMB) * BYTES_PER_MP_LIMB);
      mpn_toom3_sqr_n (prodp, up, un, tspace);
      TMP_FREE (marker);
    }
#if WANT_FFT || TUNE_PROGRAM_BUILD
  else
    {
      /* schoenhage multiplication */
      mpn_mul_fft_full (prodp, up, un, up, un);
    }
#endif
}

mp_limb_t
#if __STDC__
mpn_mul (mp_ptr prodp,
	 mp_srcptr up, mp_size_t un,
	 mp_srcptr vp, mp_size_t vn)
#else
mpn_mul (prodp, up, un, vp, vn)
     mp_ptr prodp;
     mp_srcptr up;
     mp_size_t un;
     mp_srcptr vp;
     mp_size_t vn;
#endif
{
  mp_size_t l;
  mp_limb_t c;

  if (up == vp && un == vn)
    {
      mpn_sqr_n (prodp, up, un);
      return prodp[2 * un - 1];
    }

  if (vn < KARATSUBA_MUL_THRESHOLD)
    { /* long multiplication */
      mpn_mul_basecase (prodp, up, un, vp, vn);
      return prodp[un + vn - 1];
    }

  mpn_mul_n (prodp, up, vp, vn);
  if (un != vn)
    { mp_limb_t t;
      mp_ptr ws;
      TMP_DECL (marker);
      TMP_MARK (marker);

      prodp += vn;
      l = vn;
      up += vn;
      un -= vn;

      if (un < vn) 
	{
	  /* Swap u's and v's. */
          MPN_SRCPTR_SWAP (up,un, vp,vn);
	}

      ws = (mp_ptr) TMP_ALLOC (((vn >= KARATSUBA_MUL_THRESHOLD ? vn : un) + vn)
			       * BYTES_PER_MP_LIMB);

      t = 0;
      while (vn >= KARATSUBA_MUL_THRESHOLD)
	{
	  mpn_mul_n (ws, up, vp, vn);
	  if (l <= 2*vn) 
	    {
	      t += mpn_add_n (prodp, prodp, ws, l);
	      if (l != 2*vn)
		{
		  t = mpn_add_1 (prodp + l, ws + l, 2*vn - l, t);
		  l = 2*vn;
		}
	    }
	  else
	    {
	      c = mpn_add_n (prodp, prodp, ws, 2*vn);
	      t += mpn_add_1 (prodp + 2*vn, prodp + 2*vn, l - 2*vn, c);
	    }
	  prodp += vn;
	  l -= vn;
	  up += vn;
	  un -= vn;
	  if (un < vn) 
	    {
	      /* Swap u's and v's. */
              MPN_SRCPTR_SWAP (up,un, vp,vn);
	    }
	}

      if (vn)
	{
	  mpn_mul_basecase (ws, up, un, vp, vn);
	  if (l <= un + vn) 
	    {
	      t += mpn_add_n (prodp, prodp, ws, l);
	      if (l != un + vn)
		t = mpn_add_1 (prodp + l, ws + l, un + vn - l, t);
	    } 
	  else
	    {
	      c = mpn_add_n (prodp, prodp, ws, un + vn);
	      t += mpn_add_1 (prodp + un + vn, prodp + un + vn, l - un - vn, c);
	    }
	}

    TMP_FREE (marker);
  }
  return prodp[un + vn - 1];
}

/* mpn_divrem -- Divide natural numbers, producing both remainder and
   quotient.  This is now just a middle layer for calling the new
   internal mpn_tdiv_qr. */

mp_limb_t
#if __STDC__
mpn_divrem (mp_ptr qp, mp_size_t qxn,
	    mp_ptr np, mp_size_t nn,
	    mp_srcptr dp, mp_size_t dn)
#else
mpn_divrem (qp, qxn, np, nn, dp, dn)
     mp_ptr qp;
     mp_size_t qxn;
     mp_ptr np;
     mp_size_t nn;
     mp_srcptr dp;
     mp_size_t dn;
#endif
{
  SCHEME_BIGNUM_USE_FUEL(dn + nn);

  if (dn == 1)
    {
      mp_limb_t ret;
      mp_ptr q2p;
      mp_size_t qn;
      TMP_DECL (marker);

      TMP_MARK (marker);
      q2p = (mp_ptr) TMP_ALLOC ((nn + qxn) * BYTES_PER_MP_LIMB);

      np[0] = mpn_divrem_1 (q2p, qxn, np, nn, dp[0]);
      qn = nn + qxn - 1;
      MPN_COPY (qp, q2p, qn);
      ret = q2p[qn];

      TMP_FREE (marker);
      return ret;
    }
  else if (dn == 2)
    {
      return mpn_divrem_2 (qp, qxn, np, nn, dp);
    }
  else
    {
      mp_ptr rp, q2p;
      mp_limb_t qhl;
      mp_size_t qn;
      TMP_DECL (marker);

      TMP_MARK (marker);
      if (qxn != 0)
	{
	  mp_ptr n2p;
	  n2p = (mp_ptr) TMP_ALLOC ((nn + qxn) * BYTES_PER_MP_LIMB);
	  MPN_ZERO (n2p, qxn);
	  MPN_COPY (n2p + qxn, np, nn);
	  q2p = (mp_ptr) TMP_ALLOC ((nn - dn + qxn + 1) * BYTES_PER_MP_LIMB);
	  rp = (mp_ptr) TMP_ALLOC (dn * BYTES_PER_MP_LIMB);
	  mpn_tdiv_qr (q2p, rp, 0L, n2p, nn + qxn, dp, dn);
	  MPN_COPY (np, rp, dn);
	  qn = nn - dn + qxn;
	  MPN_COPY (qp, q2p, qn);
	  qhl = q2p[qn];
	}
      else
	{
	  q2p = (mp_ptr) TMP_ALLOC ((nn - dn + 1) * BYTES_PER_MP_LIMB);
	  rp = (mp_ptr) TMP_ALLOC (dn * BYTES_PER_MP_LIMB);
	  mpn_tdiv_qr (q2p, rp, 0L, np, nn, dp, dn);
	  MPN_COPY (np, rp, dn);	/* overwrite np area with remainder */
	  qn = nn - dn;
	  MPN_COPY (qp, q2p, qn);
	  qhl = q2p[qn];
	}
      TMP_FREE (marker);
      return qhl;
    }
}

/************************************************************************/
/* GCD: */

#define MPN_MOD_OR_MODEXACT_1_ODD(src,size,divisor)     \
  mpn_mod_1 (src, size, divisor)



/* The size where udiv_qrnnd_preinv should be used rather than udiv_qrnnd,
   meaning the quotient size where that should happen, the quotient size
   being how many udiv divisions will be done.

   The default is to use preinv always, CPUs where this doesn't suit have
   tuned thresholds.  Note in particular that preinv should certainly be
   used if that's the only division available (USE_PREINV_ALWAYS).  */

#ifndef MOD_1_NORM_THRESHOLD
#define MOD_1_NORM_THRESHOLD  0
#endif
#ifndef MOD_1_UNNORM_THRESHOLD
#define MOD_1_UNNORM_THRESHOLD  0
#endif


/* The comments in mpn/generic/divrem_1.c apply here too.

   As noted in the algorithms section of the manual, the shifts in the loop
   for the unnorm case can be avoided by calculating r = a%(d*2^n), followed
   by a final (r*2^n)%(d*2^n).  In fact if it happens that a%(d*2^n) can
   skip a division where (a*2^n)%(d*2^n) can't then there's the same number
   of divide steps, though how often that happens depends on the assumed
   distributions of dividend and divisor.  In any case this idea is left to
   CPU specific implementations to consider.  */

mp_limb_t
mpn_mod_1 (mp_srcptr up, mp_size_t un, mp_limb_t d)
{
  mp_size_t  i;
  mp_limb_t  n1, n0, r;
  mp_limb_t  dummy;

  ASSERT (un >= 0);
  ASSERT (d != 0);

  /* Botch: Should this be handled at all?  Rely on callers?
     But note un==0 is currently required by mpz/fdiv_r_ui.c and possibly
     other places.  */
  if (un == 0)
    return 0;

  d <<= GMP_NAIL_BITS;

  if ((d & GMP_LIMB_HIGHBIT) != 0)
    {
      /* High limb is initial remainder, possibly with one subtract of
	 d to get r<d.  */
      r = up[un - 1] << GMP_NAIL_BITS;
      if (r >= d)
	r -= d;
      r >>= GMP_NAIL_BITS;
      un--;
      if (un == 0)
	return r;

      if (BELOW_THRESHOLD (un, MOD_1_NORM_THRESHOLD))
	{
	plain:
	  for (i = un - 1; i >= 0; i--)
	    {
	      n0 = up[i] << GMP_NAIL_BITS;
	      udiv_qrnnd (dummy, r, r, n0, d);
	      r >>= GMP_NAIL_BITS;
	    }
	  return r;
	}
      else
	{
	  mp_limb_t  inv;
	  invert_limb (inv, d);
	  for (i = un - 1; i >= 0; i--)
	    {
	      n0 = up[i] << GMP_NAIL_BITS;
	      udiv_qrnnd_preinv (dummy, r, r, n0, d, inv);
	      r >>= GMP_NAIL_BITS;
	    }
	  return r;
	}
    }
  else
    {
      int norm;

      /* Skip a division if high < divisor.  Having the test here before
	 normalizing will still skip as often as possible.  */
      r = up[un - 1] << GMP_NAIL_BITS;
      if (r < d)
	{
	  r >>= GMP_NAIL_BITS;
	  un--;
	  if (un == 0)
	    return r;
	}
      else
	r = 0;

      /* If udiv_qrnnd doesn't need a normalized divisor, can use the simple
	 code above. */
      if (! UDIV_NEEDS_NORMALIZATION
	  && BELOW_THRESHOLD (un, MOD_1_UNNORM_THRESHOLD))
	goto plain;

      count_leading_zeros (norm, d);
      d <<= norm;

      n1 = up[un - 1] << GMP_NAIL_BITS;
      r = (r << norm) | (n1 >> (GMP_LIMB_BITS - norm));

      if (UDIV_NEEDS_NORMALIZATION
	  && BELOW_THRESHOLD (un, MOD_1_UNNORM_THRESHOLD))
	{
	  for (i = un - 2; i >= 0; i--)
	    {
	      n0 = up[i] << GMP_NAIL_BITS;
	      udiv_qrnnd (dummy, r, r,
			  (n1 << norm) | (n0 >> (GMP_NUMB_BITS - norm)),
			  d);
	      r >>= GMP_NAIL_BITS;
	      n1 = n0;
	    }
	  udiv_qrnnd (dummy, r, r, n1 << norm, d);
	  r >>= GMP_NAIL_BITS;
	  return r >> norm;
	}
      else
	{
	  mp_limb_t inv;
	  invert_limb (inv, d);

	  for (i = un - 2; i >= 0; i--)
	    {
	      n0 = up[i] << GMP_NAIL_BITS;
	      udiv_qrnnd_preinv (dummy, r, r,
				 (n1 << norm) | (n0 >> (GMP_NUMB_BITS - norm)),
				 d, inv);
	      r >>= GMP_NAIL_BITS;
	      n1 = n0;
	    }
	  udiv_qrnnd_preinv (dummy, r, r, n1 << norm, d, inv);
	  r >>= GMP_NAIL_BITS;
	  return r >> norm;
	}
    }
}

/* Does not work for U == 0 or V == 0.  It would be tough to make it work for
   V == 0 since gcd(x,0) = x, and U does not generally fit in an mp_limb_t.

   The threshold for doing u%v when size==1 will vary by CPU according to
   the speed of a division and the code generated for the main loop.  Any
   tuning for this is left to a CPU specific implementation.  */

mp_limb_t
mpn_gcd_1 (mp_srcptr up, mp_size_t size, mp_limb_t vlimb)
{
  mp_limb_t      ulimb;
  unsigned long  zero_bits, u_low_zero_bits;

  ASSERT (size >= 1);
  ASSERT (vlimb != 0);
  /* ASSERT_MPN_NONZERO_P (up, size); */

  ulimb = up[0];

  /* Need vlimb odd for modexact, want it odd to get common zeros. */
  count_trailing_zeros (zero_bits, vlimb);
  vlimb >>= zero_bits;

  if (size > 1)
    {
      /* Must get common zeros before the mod reduction.  If ulimb==0 then
	 vlimb already gives the common zeros.  */
      if (ulimb != 0)
	{
	  count_trailing_zeros (u_low_zero_bits, ulimb);
	  zero_bits = MIN (zero_bits, u_low_zero_bits);
	}

      ulimb = MPN_MOD_OR_MODEXACT_1_ODD (up, size, vlimb);
      if (ulimb == 0)
	goto done;

      goto strip_u_maybe;
    }

  /* size==1, so up[0]!=0 */
  count_trailing_zeros (u_low_zero_bits, ulimb);
  ulimb >>= u_low_zero_bits;
  zero_bits = MIN (zero_bits, u_low_zero_bits);

  /* make u bigger */
  if (vlimb > ulimb)
    MP_LIMB_T_SWAP (ulimb, vlimb);

  /* if u is much bigger than v, reduce using a division rather than
     chipping away at it bit-by-bit */
  if ((ulimb >> 16) > vlimb)
    {
      ulimb %= vlimb;
      if (ulimb == 0)
	goto done;
      goto strip_u_maybe;
    }

  while (ulimb != vlimb)
    {
      ASSERT (ulimb & 1);
      ASSERT (vlimb & 1);

      if (ulimb > vlimb)
	{
	  ulimb -= vlimb;
	  do
	    {
	      ulimb >>= 1;
	      ASSERT (ulimb != 0);
	    strip_u_maybe:
	      ;
	    }
	  while ((ulimb & 1) == 0);
	}
      else /*  vlimb > ulimb.  */
	{
	  vlimb -= ulimb;
	  do
	    {
	      vlimb >>= 1;
	      ASSERT (vlimb != 0);
	    }
	  while ((vlimb & 1) == 0);
	}
    }

 done:
  return vlimb << zero_bits;
}


/* Integer greatest common divisor of two unsigned integers, using
   the accelerated algorithm (see reference below).

   mp_size_t mpn_gcd (up, usize, vp, vsize).

   Preconditions [U = (up, usize) and V = (vp, vsize)]:

   1.  V is odd.
   2.  numbits(U) >= numbits(V).

   Both U and V are destroyed by the operation.  The result is left at vp,
   and its size is returned.

   Ken Weber (kweber@mat.ufrgs.br, kweber@mcs.kent.edu)

   Funding for this work has been partially provided by Conselho Nacional
   de Desenvolvimento Cienti'fico e Tecnolo'gico (CNPq) do Brazil, Grant
   301314194-2, and was done while I was a visiting reseacher in the Instituto
   de Matema'tica at Universidade Federal do Rio Grande do Sul (UFRGS).

   Refer to
	K. Weber, The accelerated integer GCD algorithm, ACM Transactions on
	Mathematical Software, v. 21 (March), 1995, pp. 111-122.  */

/* If MIN (usize, vsize) >= GCD_ACCEL_THRESHOLD, then the accelerated
   algorithm is used, otherwise the binary algorithm is used.  This may be
   adjusted for different architectures.  */
#ifndef GCD_ACCEL_THRESHOLD
#define GCD_ACCEL_THRESHOLD 5
#endif

/* When U and V differ in size by more than BMOD_THRESHOLD, the accelerated
   algorithm reduces using the bmod operation.  Otherwise, the k-ary reduction
   is used.  0 <= BMOD_THRESHOLD < GMP_NUMB_BITS.  */
enum
  {
    BMOD_THRESHOLD = GMP_NUMB_BITS/2
  };


/* Use binary algorithm to compute V <-- GCD (V, U) for usize, vsize == 2.
   Both U and V must be odd.  */
static inline mp_size_t
gcd_2 (mp_ptr vp, mp_srcptr up)
{
  mp_limb_t u0, u1, v0, v1;
  mp_size_t vsize;

  u0 = up[0];
  u1 = up[1];
  v0 = vp[0];
  v1 = vp[1];

  while (u1 != v1 && u0 != v0)
    {
      unsigned long int r;
      if (u1 > v1)
	{
	  u1 -= v1 + (u0 < v0);
	  u0 = (u0 - v0) & GMP_NUMB_MASK;
	  count_trailing_zeros (r, u0);
	  u0 = ((u1 << (GMP_NUMB_BITS - r)) & GMP_NUMB_MASK) | (u0 >> r);
	  u1 >>= r;
	}
      else  /* u1 < v1.  */
	{
	  v1 -= u1 + (v0 < u0);
	  v0 = (v0 - u0) & GMP_NUMB_MASK;
	  count_trailing_zeros (r, v0);
	  v0 = ((v1 << (GMP_NUMB_BITS - r)) & GMP_NUMB_MASK) | (v0 >> r);
	  v1 >>= r;
	}
    }

  vp[0] = v0, vp[1] = v1, vsize = 1 + (v1 != 0);

  /* If U == V == GCD, done.  Otherwise, compute GCD (V, |U - V|).  */
  if (u1 == v1 && u0 == v0)
    return vsize;

  v0 = (u0 == v0) ? (u1 > v1) ? u1-v1 : v1-u1 : (u0 > v0) ? u0-v0 : v0-u0;
  vp[0] = mpn_gcd_1 (vp, vsize, v0);

  return 1;
}

/* The function find_a finds 0 < N < 2^GMP_NUMB_BITS such that there exists
   0 < |D| < 2^GMP_NUMB_BITS, and N == D * C mod 2^(2*GMP_NUMB_BITS).
   In the reference article, D was computed along with N, but it is better to
   compute D separately as D <-- N / C mod 2^(GMP_NUMB_BITS + 1), treating
   the result as a twos' complement signed integer.

   Initialize N1 to C mod 2^(2*GMP_NUMB_BITS).  According to the reference
   article, N2 should be initialized to 2^(2*GMP_NUMB_BITS), but we use
   2^(2*GMP_NUMB_BITS) - N1 to start the calculations within double
   precision.  If N2 > N1 initially, the first iteration of the while loop
   will swap them.  In all other situations, N1 >= N2 is maintained.  */

#if HAVE_NATIVE_mpn_gcd_finda
#define find_a(cp)  mpn_gcd_finda (cp)

#else
static
#if ! defined (__i386__)
inline				/* don't inline this for the x86 */
#endif
mp_limb_t
find_a (mp_srcptr cp)
{
  unsigned long int leading_zero_bits = 0;

  mp_limb_t n1_l = cp[0];	/* N1 == n1_h * 2^GMP_NUMB_BITS + n1_l.  */
  mp_limb_t n1_h = cp[1];

  mp_limb_t n2_l = (-n1_l & GMP_NUMB_MASK);	/* N2 == n2_h * 2^GMP_NUMB_BITS + n2_l.  */
  mp_limb_t n2_h = (~n1_h & GMP_NUMB_MASK);

  /* Main loop.  */
  while (n2_h != 0)		/* While N2 >= 2^GMP_NUMB_BITS.  */
    {
      /* N1 <-- N1 % N2.  */
      if (((GMP_NUMB_HIGHBIT >> leading_zero_bits) & n2_h) == 0)
	{
	  unsigned long int i;
	  count_leading_zeros (i, n2_h);
	  i -= GMP_NAIL_BITS;
	  i -= leading_zero_bits;
	  leading_zero_bits += i;
	  n2_h = ((n2_h << i) & GMP_NUMB_MASK) | (n2_l >> (GMP_NUMB_BITS - i));
	  n2_l = (n2_l << i) & GMP_NUMB_MASK;
	  do
	    {
	      if (n1_h > n2_h || (n1_h == n2_h && n1_l >= n2_l))
		{
		  n1_h -= n2_h + (n1_l < n2_l);
		  n1_l = (n1_l - n2_l) & GMP_NUMB_MASK;
		}
	      n2_l = (n2_l >> 1) | ((n2_h << (GMP_NUMB_BITS - 1)) & GMP_NUMB_MASK);
	      n2_h >>= 1;
	      i -= 1;
	    }
	  while (i != 0);
	}
      if (n1_h > n2_h || (n1_h == n2_h && n1_l >= n2_l))
	{
	  n1_h -= n2_h + (n1_l < n2_l);
	  n1_l = (n1_l - n2_l) & GMP_NUMB_MASK;
	}

      MP_LIMB_T_SWAP (n1_h, n2_h);
      MP_LIMB_T_SWAP (n1_l, n2_l);
    }

  return n2_l;
}
#endif


mp_size_t
mpn_gcd (mp_ptr gp, mp_ptr up, mp_size_t usize, mp_ptr vp, mp_size_t vsize)
{
  mp_ptr orig_vp = vp;
  mp_size_t orig_vsize = vsize;
  int binary_gcd_ctr;		/* Number of times binary gcd will execute.  */
  TMP_DECL (marker);

  ASSERT (usize >= 1);
  ASSERT (vsize >= 1);
  ASSERT (usize >= vsize);
  ASSERT (vp[0] & 1);
  ASSERT (up[usize - 1] != 0);
  ASSERT (vp[vsize - 1] != 0);
#if WANT_ASSERT
  if (usize == vsize)
    {
      int  uzeros, vzeros;
      count_leading_zeros (uzeros, up[usize - 1]);
      count_leading_zeros (vzeros, vp[vsize - 1]);
      ASSERT (uzeros <= vzeros);
    }
#endif
  ASSERT (! MPN_OVERLAP_P (up, usize, vp, vsize));
  ASSERT (MPN_SAME_OR_SEPARATE2_P (gp, vsize, up, usize));
  ASSERT (MPN_SAME_OR_SEPARATE2_P (gp, vsize, vp, vsize));

  TMP_MARK (marker);

  /* Use accelerated algorithm if vsize is over GCD_ACCEL_THRESHOLD.
     Two EXTRA limbs for U and V are required for kary reduction.  */
  if (vsize >= GCD_ACCEL_THRESHOLD)
    {
      unsigned long int vbitsize, d;
      mp_ptr orig_up = up;
      mp_size_t orig_usize = usize;
      mp_ptr anchor_up = (mp_ptr) TMP_ALLOC ((usize + 2) * BYTES_PER_MP_LIMB);

      MPN_COPY (anchor_up, orig_up, usize);
      up = anchor_up;

      count_leading_zeros (d, up[usize - 1]);
      d -= GMP_NAIL_BITS;
      d = usize * GMP_NUMB_BITS - d;
      count_leading_zeros (vbitsize, vp[vsize - 1]);
      vbitsize -= GMP_NAIL_BITS;
      vbitsize = vsize * GMP_NUMB_BITS - vbitsize;
      ASSERT (d >= vbitsize);
      d = d - vbitsize + 1;

      /* Use bmod reduction to quickly discover whether V divides U.  */
      up[usize++] = 0;				/* Insert leading zero.  */
      mpn_bdivmod (up, up, usize, vp, vsize, d);

      /* Now skip U/V mod 2^d and any low zero limbs.  */
      d /= GMP_NUMB_BITS, up += d, usize -= d;
      while (usize != 0 && up[0] == 0)
	up++, usize--;

      if (usize == 0)				/* GCD == ORIG_V.  */
	goto done;

      vp = (mp_ptr) TMP_ALLOC ((vsize + 2) * BYTES_PER_MP_LIMB);
      MPN_COPY (vp, orig_vp, vsize);

      do					/* Main loop.  */
	{
	  /* mpn_com_n can't be used here because anchor_up and up may
	     partially overlap */
	  if ((up[usize - 1] & GMP_NUMB_HIGHBIT) != 0)  /* U < 0; take twos' compl. */
	    {
	      mp_size_t i;
	      anchor_up[0] = -up[0] & GMP_NUMB_MASK;
	      for (i = 1; i < usize; i++)
		anchor_up[i] = (~up[i] & GMP_NUMB_MASK);
	      up = anchor_up;
	    }

	  MPN_NORMALIZE_NOT_ZERO (up, usize);

	  if ((up[0] & 1) == 0)			/* Result even; remove twos. */
	    {
	      unsigned int r;
	      count_trailing_zeros (r, up[0]);
	      mpn_rshift (anchor_up, up, usize, r);
	      usize -= (anchor_up[usize - 1] == 0);
	    }
	  else if (anchor_up != up)
	    MPN_COPY_INCR (anchor_up, up, usize);

	  MPN_PTR_SWAP (anchor_up,usize, vp,vsize);
	  up = anchor_up;

	  if (vsize <= 2)		/* Kary can't handle < 2 limbs and  */
	    break;			/* isn't efficient for == 2 limbs.  */

	  d = vbitsize;
	  count_leading_zeros (vbitsize, vp[vsize - 1]);
	  vbitsize -= GMP_NAIL_BITS;
	  vbitsize = vsize * GMP_NUMB_BITS - vbitsize;
	  d = d - vbitsize + 1;

	  if (d > BMOD_THRESHOLD)	/* Bmod reduction.  */
	    {
	      up[usize++] = 0;
	      mpn_bdivmod (up, up, usize, vp, vsize, d);
	      d /= GMP_NUMB_BITS, up += d, usize -= d;
	    }
	  else				/* Kary reduction.  */
	    {
	      mp_limb_t bp[2], cp[2];

	      /* C <-- V/U mod 2^(2*GMP_NUMB_BITS).  */
	      {
		mp_limb_t u_inv, hi, lo;
		modlimb_invert (u_inv, up[0]);
		cp[0] = (vp[0] * u_inv) & GMP_NUMB_MASK;
		umul_ppmm (hi, lo, cp[0], up[0] << GMP_NAIL_BITS);
		lo >>= GMP_NAIL_BITS;
		cp[1] = (vp[1] - hi - cp[0] * up[1]) * u_inv & GMP_NUMB_MASK;
	      }

	      /* U <-- find_a (C)  *  U.  */
	      up[usize] = mpn_mul_1 (up, up, usize, find_a (cp));
	      usize++;

	      /* B <-- A/C == U/V mod 2^(GMP_NUMB_BITS + 1).
		  bp[0] <-- U/V mod 2^GMP_NUMB_BITS and
		  bp[1] <-- ( (U - bp[0] * V)/2^GMP_NUMB_BITS ) / V mod 2

		  Like V/U above, but simplified because only the low bit of
		  bp[1] is wanted. */
	      {
		mp_limb_t  v_inv, hi, lo;
		modlimb_invert (v_inv, vp[0]);
		bp[0] = (up[0] * v_inv) & GMP_NUMB_MASK;
		umul_ppmm (hi, lo, bp[0], vp[0] << GMP_NAIL_BITS);
		lo >>= GMP_NAIL_BITS;
		bp[1] = (up[1] + hi + (bp[0] & vp[1])) & 1;
	      }

	      up[usize++] = 0;
	      if (bp[1] != 0)	/* B < 0: U <-- U + (-B)  * V.  */
		{
		   mp_limb_t c = mpn_addmul_1 (up, vp, vsize, -bp[0] & GMP_NUMB_MASK);
		   mpn_add_1 (up + vsize, up + vsize, usize - vsize, c);
		}
	      else		/* B >= 0:  U <-- U - B * V.  */
		{
		  mp_limb_t b = mpn_submul_1 (up, vp, vsize, bp[0]);
		  mpn_sub_1 (up + vsize, up + vsize, usize - vsize, b);
		}

	      up += 2, usize -= 2;  /* At least two low limbs are zero.  */
	    }

	  /* Must remove low zero limbs before complementing.  */
	  while (usize != 0 && up[0] == 0)
	    up++, usize--;
	}
      while (usize != 0);

      /* Compute GCD (ORIG_V, GCD (ORIG_U, V)).  Binary will execute twice.  */
      up = orig_up, usize = orig_usize;
      binary_gcd_ctr = 2;
    }
  else
    binary_gcd_ctr = 1;

  /* Finish up with the binary algorithm.  Executes once or twice.  */
  for ( ; binary_gcd_ctr--; up = orig_vp, usize = orig_vsize)
    {
      if (usize > 2)		/* First make U close to V in size.  */
	{
	  unsigned long int vbitsize, d;
	  count_leading_zeros (d, up[usize - 1]);
	  d -= GMP_NAIL_BITS;
	  d = usize * GMP_NUMB_BITS - d;
	  count_leading_zeros (vbitsize, vp[vsize - 1]);
	  vbitsize -= GMP_NAIL_BITS;
	  vbitsize = vsize * GMP_NUMB_BITS - vbitsize;
	  d = d - vbitsize - 1;
	  if (d != -(unsigned long int)1 && d > 2)
	    {
	      mpn_bdivmod (up, up, usize, vp, vsize, d);  /* Result > 0.  */
	      d /= (unsigned long int)GMP_NUMB_BITS, up += d, usize -= d;
	    }
	}

      /* Start binary GCD.  */
      do
	{
	  mp_size_t zeros;

	  /* Make sure U is odd.  */
	  MPN_NORMALIZE (up, usize);
	  while (up[0] == 0)
	    up += 1, usize -= 1;
	  if ((up[0] & 1) == 0)
	    {
	      unsigned int r;
	      count_trailing_zeros (r, up[0]);
	      mpn_rshift (up, up, usize, r);
	      usize -= (up[usize - 1] == 0);
	    }

	  /* Keep usize >= vsize.  */
	  if (usize < vsize)
	    MPN_PTR_SWAP (up, usize, vp, vsize);

	  if (usize <= 2)				/* Double precision. */
	    {
	      if (vsize == 1)
		vp[0] = mpn_gcd_1 (up, usize, vp[0]);
	      else
		vsize = gcd_2 (vp, up);
	      break;					/* Binary GCD done.  */
	    }

	  /* Count number of low zero limbs of U - V.  */
	  for (zeros = 0; up[zeros] == vp[zeros] && ++zeros != vsize; )
	    continue;

	  /* If U < V, swap U and V; in any case, subtract V from U.  */
	  if (zeros == vsize)				/* Subtract done.  */
	    up += zeros, usize -= zeros;
	  else if (usize == vsize)
	    {
	      mp_size_t size = vsize;
	      do
		size--;
	      while (up[size] == vp[size]);
	      if (up[size] < vp[size])			/* usize == vsize.  */
		MP_PTR_SWAP (up, vp);
	      up += zeros, usize = size + 1 - zeros;
	      mpn_sub_n (up, up, vp + zeros, usize);
	    }
	  else
	    {
	      mp_size_t size = vsize - zeros;
	      up += zeros, usize -= zeros;
	      if (mpn_sub_n (up, up, vp + zeros, size))
		{
		  while (up[size] == 0)			/* Propagate borrow. */
		    up[size++] = -(mp_limb_t)1;
		  up[size] -= 1;
		}
	    }
	}
      while (usize);					/* End binary GCD.  */
    }

done:
  if (vp != gp)
    MPN_COPY_INCR (gp, vp, vsize);
  TMP_FREE (marker);
  return vsize;
}


/* q_high = mpn_bdivmod (qp, up, usize, vp, vsize, d).

   Puts the low d/BITS_PER_MP_LIMB limbs of Q = U / V mod 2^d at qp, and
   returns the high d%BITS_PER_MP_LIMB bits of Q as the result.

   Also, U - Q * V mod 2^(usize*BITS_PER_MP_LIMB) is placed at up.  Since the
   low d/BITS_PER_MP_LIMB limbs of this difference are zero, the code allows
   the limb vectors at qp to overwrite the low limbs at up, provided qp <= up.

   Preconditions:
   1.  V is odd.
   2.  usize * BITS_PER_MP_LIMB >= d.
   3.  If Q and U overlap, qp <= up.

   Ken Weber (kweber@mat.ufrgs.br, kweber@mcs.kent.edu)

   Funding for this work has been partially provided by Conselho Nacional
   de Desenvolvimento Cienti'fico e Tecnolo'gico (CNPq) do Brazil, Grant
   301314194-2, and was done while I was a visiting reseacher in the Instituto
   de Matema'tica at Universidade Federal do Rio Grande do Sul (UFRGS).

   References:
       T. Jebelean, An algorithm for exact division, Journal of Symbolic
       Computation, v. 15, 1993, pp. 169-180.

       K. Weber, The accelerated integer GCD algorithm, ACM Transactions on
       Mathematical Software, v. 21 (March), 1995, pp. 111-122.  */

mp_limb_t
mpn_bdivmod (mp_ptr qp, mp_ptr up, mp_size_t usize,
	     mp_srcptr vp, mp_size_t vsize, unsigned long int d)
{
  mp_limb_t v_inv;

  ASSERT (usize >= 1);
  ASSERT (vsize >= 1);
  ASSERT (usize * GMP_NUMB_BITS >= d);
  ASSERT (! MPN_OVERLAP_P (up, usize, vp, vsize));
  ASSERT (! MPN_OVERLAP_P (qp, d/GMP_NUMB_BITS, vp, vsize));
  ASSERT (MPN_SAME_OR_INCR2_P (qp, d/GMP_NUMB_BITS, up, usize));
  /* ASSERT_MPN (up, usize); */
  /* ASSERT_MPN (vp, vsize); */

  /* 1/V mod 2^GMP_NUMB_BITS. */
  modlimb_invert (v_inv, vp[0]);

  /* Fast code for two cases previously used by the accel part of mpn_gcd.
     (Could probably remove this now it's inlined there.) */
  if (usize == 2 && vsize == 2 &&
      (d == GMP_NUMB_BITS || d == 2*GMP_NUMB_BITS))
    {
      mp_limb_t hi, lo;
      mp_limb_t q = (up[0] * v_inv) & GMP_NUMB_MASK;
      umul_ppmm (hi, lo, q, vp[0] << GMP_NAIL_BITS);
      up[0] = 0;
      up[1] -= hi + q*vp[1];
      qp[0] = q;
      if (d == 2*GMP_NUMB_BITS)
        {
          q = (up[1] * v_inv) & GMP_NUMB_MASK;
          up[1] = 0;
          qp[1] = q;
        }
      return 0;
    }

  /* Main loop.  */
  while (d >= GMP_NUMB_BITS)
    {
      mp_limb_t q = (up[0] * v_inv) & GMP_NUMB_MASK;
      mp_limb_t b = mpn_submul_1 (up, vp, MIN (usize, vsize), q);
      if (usize > vsize)
	mpn_sub_1 (up + vsize, up + vsize, usize - vsize, b);
      d -= GMP_NUMB_BITS;
      up += 1, usize -= 1;
      *qp++ = q;
    }

  if (d)
    {
      mp_limb_t b;
      mp_limb_t q = (up[0] * v_inv) & (((mp_limb_t)1<<d) - 1);
      if (q <= 1)
	{
	  if (q == 0)
	    return 0;
	  else
	    b = mpn_sub_n (up, up, vp, MIN (usize, vsize));
	}
      else
	b = mpn_submul_1 (up, vp, MIN (usize, vsize), q);

      if (usize > vsize)
	mpn_sub_1 (up + vsize, up + vsize, usize - vsize, b);
      return q;
    }

  return 0;
}


/* modlimb_invert_table[i] is the multiplicative inverse of 2*i+1 mod 256,
   ie. (modlimb_invert_table[i] * (2*i+1)) % 256 == 1 */

const unsigned char  modlimb_invert_table[128] = {
  0x01, 0xAB, 0xCD, 0xB7, 0x39, 0xA3, 0xC5, 0xEF,
  0xF1, 0x1B, 0x3D, 0xA7, 0x29, 0x13, 0x35, 0xDF,
  0xE1, 0x8B, 0xAD, 0x97, 0x19, 0x83, 0xA5, 0xCF,
  0xD1, 0xFB, 0x1D, 0x87, 0x09, 0xF3, 0x15, 0xBF,
  0xC1, 0x6B, 0x8D, 0x77, 0xF9, 0x63, 0x85, 0xAF,
  0xB1, 0xDB, 0xFD, 0x67, 0xE9, 0xD3, 0xF5, 0x9F,
  0xA1, 0x4B, 0x6D, 0x57, 0xD9, 0x43, 0x65, 0x8F,
  0x91, 0xBB, 0xDD, 0x47, 0xC9, 0xB3, 0xD5, 0x7F,
  0x81, 0x2B, 0x4D, 0x37, 0xB9, 0x23, 0x45, 0x6F,
  0x71, 0x9B, 0xBD, 0x27, 0xA9, 0x93, 0xB5, 0x5F,
  0x61, 0x0B, 0x2D, 0x17, 0x99, 0x03, 0x25, 0x4F,
  0x51, 0x7B, 0x9D, 0x07, 0x89, 0x73, 0x95, 0x3F,
  0x41, 0xEB, 0x0D, 0xF7, 0x79, 0xE3, 0x05, 0x2F,
  0x31, 0x5B, 0x7D, 0xE7, 0x69, 0x53, 0x75, 0x1F,
  0x21, 0xCB, 0xED, 0xD7, 0x59, 0xC3, 0xE5, 0x0F,
  0x11, 0x3B, 0x5D, 0xC7, 0x49, 0x33, 0x55, 0xFF
};


/************************************************************************/

/* __mp_bases -- Structure for conversion between internal binary
   format and strings in base 2..255.  The fields are explained in
   gmp-impl.h. */

#if BITS_PER_MP_LIMB == 32
const struct bases __mp_bases[256] =
{
  /*  0 */ {0, 0.0, 0, 0},
  /*  1 */ {0, 1e38, 0, 0},
  /*  2 */ {32, 1.0000000000000000, 0x1, 0x0},
  /*  3 */ {20, 0.6309297535714575, 0xcfd41b91, 0x3b563c24},
  /*  4 */ {16, 0.5000000000000000, 0x2, 0x0},
  /*  5 */ {13, 0.4306765580733931, 0x48c27395, 0xc25c2684},
  /*  6 */ {12, 0.3868528072345416, 0x81bf1000, 0xf91bd1b6},
  /*  7 */ {11, 0.3562071871080222, 0x75db9c97, 0x1607a2cb},
  /*  8 */ {10, 0.3333333333333334, 0x3, 0x0},
  /*  9 */ {10, 0.3154648767857287, 0xcfd41b91, 0x3b563c24},
  /* 10 */ {9, 0.3010299956639811, 0x3b9aca00, 0x12e0be82},
  /* 11 */ {9, 0.2890648263178878, 0x8c8b6d2b, 0xd24cde04},
  /* 12 */ {8, 0.2789429456511298, 0x19a10000, 0x3fa39ab5},
  /* 13 */ {8, 0.2702381544273197, 0x309f1021, 0x50f8ac5f},
  /* 14 */ {8, 0.2626495350371936, 0x57f6c100, 0x74843b1e},
  /* 15 */ {8, 0.2559580248098155, 0x98c29b81, 0xad0326c2},
  /* 16 */ {8, 0.2500000000000000, 0x4, 0x0},
  /* 17 */ {7, 0.2446505421182260, 0x18754571, 0x4ef0b6bd},
  /* 18 */ {7, 0.2398124665681315, 0x247dbc80, 0xc0fc48a1},
  /* 19 */ {7, 0.2354089133666382, 0x3547667b, 0x33838942},
  /* 20 */ {7, 0.2313782131597592, 0x4c4b4000, 0xad7f29ab},
  /* 21 */ {7, 0.2276702486969530, 0x6b5a6e1d, 0x313c3d15},
  /* 22 */ {7, 0.2242438242175754, 0x94ace180, 0xb8cca9e0},
  /* 23 */ {7, 0.2210647294575037, 0xcaf18367, 0x42ed6de9},
  /* 24 */ {6, 0.2181042919855316, 0xb640000, 0x67980e0b},
  /* 25 */ {6, 0.2153382790366965, 0xe8d4a51, 0x19799812},
  /* 26 */ {6, 0.2127460535533632, 0x1269ae40, 0xbce85396},
  /* 27 */ {6, 0.2103099178571525, 0x17179149, 0x62c103a9},
  /* 28 */ {6, 0.2080145976765095, 0x1cb91000, 0x1d353d43},
  /* 29 */ {6, 0.2058468324604344, 0x23744899, 0xce1decea},
  /* 30 */ {6, 0.2037950470905062, 0x2b73a840, 0x790fc511},
  /* 31 */ {6, 0.2018490865820999, 0x34e63b41, 0x35b865a0},
  /* 32 */ {6, 0.2000000000000000, 0x5, 0x0},
  /* 33 */ {6, 0.1982398631705605, 0x4cfa3cc1, 0xa9aed1b3},
  /* 34 */ {6, 0.1965616322328226, 0x5c13d840, 0x63dfc229},
  /* 35 */ {6, 0.1949590218937863, 0x6d91b519, 0x2b0fee30},
  /* 36 */ {6, 0.1934264036172708, 0x81bf1000, 0xf91bd1b6},
  /* 37 */ {6, 0.1919587200065601, 0x98ede0c9, 0xac89c3a9},
  /* 38 */ {6, 0.1905514124267734, 0xb3773e40, 0x6d2c32fe},
  /* 39 */ {6, 0.1892003595168700, 0xd1bbc4d1, 0x387907c9},
  /* 40 */ {6, 0.1879018247091076, 0xf4240000, 0xc6f7a0b},
  /* 41 */ {5, 0.1866524112389434, 0x6e7d349, 0x28928154},
  /* 42 */ {5, 0.1854490234153689, 0x7ca30a0, 0x6e8629d},
  /* 43 */ {5, 0.1842888331487062, 0x8c32bbb, 0xd373dca0},
  /* 44 */ {5, 0.1831692509136336, 0x9d46c00, 0xa0b17895},
  /* 45 */ {5, 0.1820879004699383, 0xaffacfd, 0x746811a5},
  /* 46 */ {5, 0.1810425967800402, 0xc46bee0, 0x4da6500f},
  /* 47 */ {5, 0.1800313266566926, 0xdab86ef, 0x2ba23582},
  /* 48 */ {5, 0.1790522317510414, 0xf300000, 0xdb20a88},
  /* 49 */ {5, 0.1781035935540111, 0x10d63af1, 0xe68d5ce4},
  /* 50 */ {5, 0.1771838201355579, 0x12a05f20, 0xb7cdfd9d},
  /* 51 */ {5, 0.1762914343888821, 0x1490aae3, 0x8e583933},
  /* 52 */ {5, 0.1754250635819545, 0x16a97400, 0x697cc3ea},
  /* 53 */ {5, 0.1745834300480449, 0x18ed2825, 0x48a5ca6c},
  /* 54 */ {5, 0.1737653428714400, 0x1b5e4d60, 0x2b52db16},
  /* 55 */ {5, 0.1729696904450771, 0x1dff8297, 0x111586a6},
  /* 56 */ {5, 0.1721954337940981, 0x20d38000, 0xf31d2b36},
  /* 57 */ {5, 0.1714416005739134, 0x23dd1799, 0xc8d76d19},
  /* 58 */ {5, 0.1707072796637201, 0x271f35a0, 0xa2cb1eb4},
  /* 59 */ {5, 0.1699916162869140, 0x2a9ce10b, 0x807c3ec3},
  /* 60 */ {5, 0.1692938075987814, 0x2e593c00, 0x617ec8bf},
  /* 61 */ {5, 0.1686130986895011, 0x3257844d, 0x45746cbe},
  /* 62 */ {5, 0.1679487789570419, 0x369b13e0, 0x2c0aa273},
  /* 63 */ {5, 0.1673001788101741, 0x3b27613f, 0x14f90805},
  /* 64 */ {5, 0.1666666666666667, 0x6, 0x0},
  /* 65 */ {5, 0.1660476462159378, 0x4528a141, 0xd9cf0829},
  /* 66 */ {5, 0.1654425539190583, 0x4aa51420, 0xb6fc4841},
  /* 67 */ {5, 0.1648508567221604, 0x50794633, 0x973054cb},
  /* 68 */ {5, 0.1642720499620502, 0x56a94400, 0x7a1dbe4b},
  /* 69 */ {5, 0.1637056554452156, 0x5d393975, 0x5f7fcd7f},
  /* 70 */ {5, 0.1631512196835108, 0x642d7260, 0x47196c84},
  /* 71 */ {5, 0.1626083122716341, 0x6b8a5ae7, 0x30b43635},
  /* 72 */ {5, 0.1620765243931223, 0x73548000, 0x1c1fa5f6},
  /* 73 */ {5, 0.1615554674429964, 0x7b908fe9, 0x930634a},
  /* 74 */ {5, 0.1610447717564445, 0x84435aa0, 0xef7f4a3c},
  /* 75 */ {5, 0.1605440854340214, 0x8d71d25b, 0xcf5552d2},
  /* 76 */ {5, 0.1600530732548213, 0x97210c00, 0xb1a47c8e},
  /* 77 */ {5, 0.1595714156699382, 0xa1563f9d, 0x9634b43e},
  /* 78 */ {5, 0.1590988078692941, 0xac16c8e0, 0x7cd3817d},
  /* 79 */ {5, 0.1586349589155960, 0xb768278f, 0x65536761},
  /* 80 */ {5, 0.1581795909397823, 0xc3500000, 0x4f8b588e},
  /* 81 */ {5, 0.1577324383928644, 0xcfd41b91, 0x3b563c24},
  /* 82 */ {5, 0.1572932473495469, 0xdcfa6920, 0x28928154},
  /* 83 */ {5, 0.1568617748594410, 0xeac8fd83, 0x1721bfb0},
  /* 84 */ {5, 0.1564377883420716, 0xf9461400, 0x6e8629d},
  /* 85 */ {4, 0.1560210650222250, 0x31c84b1, 0x491cc17c},
  /* 86 */ {4, 0.1556113914024940, 0x342ab10, 0x3a11d83b},
  /* 87 */ {4, 0.1552085627701551, 0x36a2c21, 0x2be074cd},
  /* 88 */ {4, 0.1548123827357682, 0x3931000, 0x1e7a02e7},
  /* 89 */ {4, 0.1544226628011101, 0x3bd5ee1, 0x11d10edd},
  /* 90 */ {4, 0.1540392219542636, 0x3e92110, 0x5d92c68},
  /* 91 */ {4, 0.1536618862898642, 0x4165ef1, 0xf50dbfb2},
  /* 92 */ {4, 0.1532904886526781, 0x4452100, 0xdf9f1316},
  /* 93 */ {4, 0.1529248683028321, 0x4756fd1, 0xcb52a684},
  /* 94 */ {4, 0.1525648706011593, 0x4a75410, 0xb8163e97},
  /* 95 */ {4, 0.1522103467132434, 0x4dad681, 0xa5d8f269},
  /* 96 */ {4, 0.1518611533308632, 0x5100000, 0x948b0fcd},
  /* 97 */ {4, 0.1515171524096389, 0x546d981, 0x841e0215},
  /* 98 */ {4, 0.1511782109217764, 0x57f6c10, 0x74843b1e},
  /* 99 */ {4, 0.1508442006228941, 0x5b9c0d1, 0x65b11e6e},
  /* 100 */ {4, 0.1505149978319906, 0x5f5e100, 0x5798ee23},
  /* 101 */ {4, 0.1501904832236879, 0x633d5f1, 0x4a30b99b},
  /* 102 */ {4, 0.1498705416319474, 0x673a910, 0x3d6e4d94},
  /* 103 */ {4, 0.1495550618645152, 0x6b563e1, 0x314825b0},
  /* 104 */ {4, 0.1492439365274121, 0x6f91000, 0x25b55f2e},
  /* 105 */ {4, 0.1489370618588283, 0x73eb721, 0x1aadaccb},
  /* 106 */ {4, 0.1486343375718350, 0x7866310, 0x10294ba2},
  /* 107 */ {4, 0.1483356667053617, 0x7d01db1, 0x620f8f6},
  /* 108 */ {4, 0.1480409554829326, 0x81bf100, 0xf91bd1b6},
  /* 109 */ {4, 0.1477501131786861, 0x869e711, 0xe6d37b2a},
  /* 110 */ {4, 0.1474630519902391, 0x8ba0a10, 0xd55cff6e},
  /* 111 */ {4, 0.1471796869179852, 0x90c6441, 0xc4ad2db2},
  /* 112 */ {4, 0.1468999356504447, 0x9610000, 0xb4b985cf},
  /* 113 */ {4, 0.1466237184553111, 0x9b7e7c1, 0xa5782bef},
  /* 114 */ {4, 0.1463509580758620, 0xa112610, 0x96dfdd2a},
  /* 115 */ {4, 0.1460815796324244, 0xa6cc591, 0x88e7e509},
  /* 116 */ {4, 0.1458155105286054, 0xacad100, 0x7b8813d3},
  /* 117 */ {4, 0.1455526803620167, 0xb2b5331, 0x6eb8b595},
  /* 118 */ {4, 0.1452930208392428, 0xb8e5710, 0x627289db},
  /* 119 */ {4, 0.1450364656948130, 0xbf3e7a1, 0x56aebc07},
  /* 120 */ {4, 0.1447829506139581, 0xc5c1000, 0x4b66dc33},
  /* 121 */ {4, 0.1445324131589439, 0xcc6db61, 0x4094d8a3},
  /* 122 */ {4, 0.1442847926987864, 0xd345510, 0x3632f7a5},
  /* 123 */ {4, 0.1440400303421672, 0xda48871, 0x2c3bd1f0},
  /* 124 */ {4, 0.1437980688733775, 0xe178100, 0x22aa4d5f},
  /* 125 */ {4, 0.1435588526911310, 0xe8d4a51, 0x19799812},
  /* 126 */ {4, 0.1433223277500932, 0xf05f010, 0x10a523e5},
  /* 127 */ {4, 0.1430884415049874, 0xf817e01, 0x828a237},
  /* 128 */ {4, 0.1428571428571428, 0x7, 0x0},
  /* 129 */ {4, 0.1426283821033600, 0x10818201, 0xf04ec452},
  /* 130 */ {4, 0.1424021108869747, 0x11061010, 0xe136444a},
  /* 131 */ {4, 0.1421782821510107, 0x118db651, 0xd2af9589},
  /* 132 */ {4, 0.1419568500933153, 0x12188100, 0xc4b42a83},
  /* 133 */ {4, 0.1417377701235801, 0x12a67c71, 0xb73dccf5},
  /* 134 */ {4, 0.1415209988221527, 0x1337b510, 0xaa4698c5},
  /* 135 */ {4, 0.1413064939005528, 0x13cc3761, 0x9dc8f729},
  /* 136 */ {4, 0.1410942141636095, 0x14641000, 0x91bf9a30},
  /* 137 */ {4, 0.1408841194731412, 0x14ff4ba1, 0x86257887},
  /* 138 */ {4, 0.1406761707131039, 0x159df710, 0x7af5c98c},
  /* 139 */ {4, 0.1404703297561400, 0x16401f31, 0x702c01a0},
  /* 140 */ {4, 0.1402665594314587, 0x16e5d100, 0x65c3ceb1},
  /* 141 */ {4, 0.1400648234939879, 0x178f1991, 0x5bb91502},
  /* 142 */ {4, 0.1398650865947379, 0x183c0610, 0x5207ec23},
  /* 143 */ {4, 0.1396673142523192, 0x18eca3c1, 0x48ac9c19},
  /* 144 */ {4, 0.1394714728255649, 0x19a10000, 0x3fa39ab5},
  /* 145 */ {4, 0.1392775294872041, 0x1a592841, 0x36e98912},
  /* 146 */ {4, 0.1390854521985406, 0x1b152a10, 0x2e7b3140},
  /* 147 */ {4, 0.1388952096850913, 0x1bd51311, 0x2655840b},
  /* 148 */ {4, 0.1387067714131417, 0x1c98f100, 0x1e7596ea},
  /* 149 */ {4, 0.1385201075671774, 0x1d60d1b1, 0x16d8a20d},
  /* 150 */ {4, 0.1383351890281539, 0x1e2cc310, 0xf7bfe87},
  /* 151 */ {4, 0.1381519873525671, 0x1efcd321, 0x85d2492},
  /* 152 */ {4, 0.1379704747522905, 0x1fd11000, 0x179a9f4},
  /* 153 */ {4, 0.1377906240751463, 0x20a987e1, 0xf59e80eb},
  /* 154 */ {4, 0.1376124087861776, 0x21864910, 0xe8b768db},
  /* 155 */ {4, 0.1374358029495937, 0x226761f1, 0xdc39d6d5},
  /* 156 */ {4, 0.1372607812113589, 0x234ce100, 0xd021c5d1},
  /* 157 */ {4, 0.1370873187823978, 0x2436d4d1, 0xc46b5e37},
  /* 158 */ {4, 0.1369153914223921, 0x25254c10, 0xb912f39c},
  /* 159 */ {4, 0.1367449754241439, 0x26185581, 0xae150294},
  /* 160 */ {4, 0.1365760475984821, 0x27100000, 0xa36e2eb1},
  /* 161 */ {4, 0.1364085852596902, 0x280c5a81, 0x991b4094},
  /* 162 */ {4, 0.1362425662114337, 0x290d7410, 0x8f19241e},
  /* 163 */ {4, 0.1360779687331669, 0x2a135bd1, 0x8564e6b7},
  /* 164 */ {4, 0.1359147715670014, 0x2b1e2100, 0x7bfbb5b4},
  /* 165 */ {4, 0.1357529539050150, 0x2c2dd2f1, 0x72dadcc8},
  /* 166 */ {4, 0.1355924953769863, 0x2d428110, 0x69ffc498},
  /* 167 */ {4, 0.1354333760385373, 0x2e5c3ae1, 0x6167f154},
  /* 168 */ {4, 0.1352755763596663, 0x2f7b1000, 0x5911016e},
  /* 169 */ {4, 0.1351190772136599, 0x309f1021, 0x50f8ac5f},
  /* 170 */ {4, 0.1349638598663645, 0x31c84b10, 0x491cc17c},
  /* 171 */ {4, 0.1348099059658079, 0x32f6d0b1, 0x417b26d8},
  /* 172 */ {4, 0.1346571975321549, 0x342ab100, 0x3a11d83b},
  /* 173 */ {4, 0.1345057169479844, 0x3563fc11, 0x32dee622},
  /* 174 */ {4, 0.1343554469488779, 0x36a2c210, 0x2be074cd},
  /* 175 */ {4, 0.1342063706143054, 0x37e71341, 0x2514bb58},
  /* 176 */ {4, 0.1340584713587980, 0x39310000, 0x1e7a02e7},
  /* 177 */ {4, 0.1339117329233981, 0x3a8098c1, 0x180ea5d0},
  /* 178 */ {4, 0.1337661393673756, 0x3bd5ee10, 0x11d10edd},
  /* 179 */ {4, 0.1336216750601996, 0x3d311091, 0xbbfb88e},
  /* 180 */ {4, 0.1334783246737591, 0x3e921100, 0x5d92c68},
  /* 181 */ {4, 0.1333360731748201, 0x3ff90031, 0x1c024c},
  /* 182 */ {4, 0.1331949058177136, 0x4165ef10, 0xf50dbfb2},
  /* 183 */ {4, 0.1330548081372441, 0x42d8eea1, 0xea30efa3},
  /* 184 */ {4, 0.1329157659418126, 0x44521000, 0xdf9f1316},
  /* 185 */ {4, 0.1327777653067443, 0x45d16461, 0xd555c0c9},
  /* 186 */ {4, 0.1326407925678156, 0x4756fd10, 0xcb52a684},
  /* 187 */ {4, 0.1325048343149731, 0x48e2eb71, 0xc193881f},
  /* 188 */ {4, 0.1323698773862368, 0x4a754100, 0xb8163e97},
  /* 189 */ {4, 0.1322359088617821, 0x4c0e0f51, 0xaed8b724},
  /* 190 */ {4, 0.1321029160581950, 0x4dad6810, 0xa5d8f269},
  /* 191 */ {4, 0.1319708865228925, 0x4f535d01, 0x9d15039d},
  /* 192 */ {4, 0.1318398080287045, 0x51000000, 0x948b0fcd},
  /* 193 */ {4, 0.1317096685686114, 0x52b36301, 0x8c394d1d},
  /* 194 */ {4, 0.1315804563506306, 0x546d9810, 0x841e0215},
  /* 195 */ {4, 0.1314521597928493, 0x562eb151, 0x7c3784f8},
  /* 196 */ {4, 0.1313247675185968, 0x57f6c100, 0x74843b1e},
  /* 197 */ {4, 0.1311982683517524, 0x59c5d971, 0x6d02985d},
  /* 198 */ {4, 0.1310726513121843, 0x5b9c0d10, 0x65b11e6e},
  /* 199 */ {4, 0.1309479056113158, 0x5d796e61, 0x5e8e5c64},
  /* 200 */ {4, 0.1308240206478128, 0x5f5e1000, 0x5798ee23},
  /* 201 */ {4, 0.1307009860033912, 0x614a04a1, 0x50cf7bde},
  /* 202 */ {4, 0.1305787914387386, 0x633d5f10, 0x4a30b99b},
  /* 203 */ {4, 0.1304574268895465, 0x65383231, 0x43bb66bd},
  /* 204 */ {4, 0.1303368824626505, 0x673a9100, 0x3d6e4d94},
  /* 205 */ {4, 0.1302171484322746, 0x69448e91, 0x374842ee},
  /* 206 */ {4, 0.1300982152363760, 0x6b563e10, 0x314825b0},
  /* 207 */ {4, 0.1299800734730872, 0x6d6fb2c1, 0x2b6cde75},
  /* 208 */ {4, 0.1298627138972530, 0x6f910000, 0x25b55f2e},
  /* 209 */ {4, 0.1297461274170591, 0x71ba3941, 0x2020a2c5},
  /* 210 */ {4, 0.1296303050907487, 0x73eb7210, 0x1aadaccb},
  /* 211 */ {4, 0.1295152381234257, 0x7624be11, 0x155b891f},
  /* 212 */ {4, 0.1294009178639407, 0x78663100, 0x10294ba2},
  /* 213 */ {4, 0.1292873358018581, 0x7aafdeb1, 0xb160fe9},
  /* 214 */ {4, 0.1291744835645007, 0x7d01db10, 0x620f8f6},
  /* 215 */ {4, 0.1290623529140715, 0x7f5c3a21, 0x14930ef},
  /* 216 */ {4, 0.1289509357448472, 0x81bf1000, 0xf91bd1b6},
  /* 217 */ {4, 0.1288402240804449, 0x842a70e1, 0xefdcb0c7},
  /* 218 */ {4, 0.1287302100711567, 0x869e7110, 0xe6d37b2a},
  /* 219 */ {4, 0.1286208859913518, 0x891b24f1, 0xddfeb94a},
  /* 220 */ {4, 0.1285122442369443, 0x8ba0a100, 0xd55cff6e},
  /* 221 */ {4, 0.1284042773229231, 0x8e2ef9d1, 0xcceced50},
  /* 222 */ {4, 0.1282969778809442, 0x90c64410, 0xc4ad2db2},
  /* 223 */ {4, 0.1281903386569819, 0x93669481, 0xbc9c75f9},
  /* 224 */ {4, 0.1280843525090381, 0x96100000, 0xb4b985cf},
  /* 225 */ {4, 0.1279790124049077, 0x98c29b81, 0xad0326c2},
  /* 226 */ {4, 0.1278743114199984, 0x9b7e7c10, 0xa5782bef},
  /* 227 */ {4, 0.1277702427352035, 0x9e43b6d1, 0x9e1771a9},
  /* 228 */ {4, 0.1276667996348261, 0xa1126100, 0x96dfdd2a},
  /* 229 */ {4, 0.1275639755045533, 0xa3ea8ff1, 0x8fd05c41},
  /* 230 */ {4, 0.1274617638294791, 0xa6cc5910, 0x88e7e509},
  /* 231 */ {4, 0.1273601581921741, 0xa9b7d1e1, 0x8225759d},
  /* 232 */ {4, 0.1272591522708010, 0xacad1000, 0x7b8813d3},
  /* 233 */ {4, 0.1271587398372755, 0xafac2921, 0x750eccf9},
  /* 234 */ {4, 0.1270589147554692, 0xb2b53310, 0x6eb8b595},
  /* 235 */ {4, 0.1269596709794558, 0xb5c843b1, 0x6884e923},
  /* 236 */ {4, 0.1268610025517973, 0xb8e57100, 0x627289db},
  /* 237 */ {4, 0.1267629036018709, 0xbc0cd111, 0x5c80c07b},
  /* 238 */ {4, 0.1266653683442337, 0xbf3e7a10, 0x56aebc07},
  /* 239 */ {4, 0.1265683910770258, 0xc27a8241, 0x50fbb19b},
  /* 240 */ {4, 0.1264719661804097, 0xc5c10000, 0x4b66dc33},
  /* 241 */ {4, 0.1263760881150453, 0xc91209c1, 0x45ef7c7c},
  /* 242 */ {4, 0.1262807514205999, 0xcc6db610, 0x4094d8a3},
  /* 243 */ {4, 0.1261859507142915, 0xcfd41b91, 0x3b563c24},
  /* 244 */ {4, 0.1260916806894653, 0xd3455100, 0x3632f7a5},
  /* 245 */ {4, 0.1259979361142023, 0xd6c16d31, 0x312a60c3},
  /* 246 */ {4, 0.1259047118299582, 0xda488710, 0x2c3bd1f0},
  /* 247 */ {4, 0.1258120027502338, 0xdddab5a1, 0x2766aa45},
  /* 248 */ {4, 0.1257198038592741, 0xe1781000, 0x22aa4d5f},
  /* 249 */ {4, 0.1256281102107963, 0xe520ad61, 0x1e06233c},
  /* 250 */ {4, 0.1255369169267456, 0xe8d4a510, 0x19799812},
  /* 251 */ {4, 0.1254462191960791, 0xec940e71, 0x15041c33},
  /* 252 */ {4, 0.1253560122735751, 0xf05f0100, 0x10a523e5},
  /* 253 */ {4, 0.1252662914786691, 0xf4359451, 0xc5c2749},
  /* 254 */ {4, 0.1251770521943144, 0xf817e010, 0x828a237},
  /* 255 */ {4, 0.1250882898658681, 0xfc05fc01, 0x40a1423},
};
#endif
#if BITS_PER_MP_LIMB == 64
const struct bases __mp_bases[256] =
{
  /*  0 */ {0, 0.0, 0, 0},
  /*  1 */ {0, 1e38, 0, 0},
  /*  2 */ {64, 1.0000000000000000, CNST_LIMB(0x1), CNST_LIMB(0x0)},
  /*  3 */ {40, 0.6309297535714574, CNST_LIMB(0xa8b8b452291fe821), CNST_LIMB(0x846d550e37b5063d)},
  /*  4 */ {32, 0.5000000000000000, CNST_LIMB(0x2), CNST_LIMB(0x0)},
  /*  5 */ {27, 0.4306765580733931, CNST_LIMB(0x6765c793fa10079d), CNST_LIMB(0x3ce9a36f23c0fc90)},
  /*  6 */ {24, 0.3868528072345416, CNST_LIMB(0x41c21cb8e1000000), CNST_LIMB(0xf24f62335024a295)},
  /*  7 */ {22, 0.3562071871080222, CNST_LIMB(0x3642798750226111), CNST_LIMB(0x2df495ccaa57147b)},
  /*  8 */ {21, 0.3333333333333334, CNST_LIMB(0x3), CNST_LIMB(0x0)},
  /*  9 */ {20, 0.3154648767857287, CNST_LIMB(0xa8b8b452291fe821), CNST_LIMB(0x846d550e37b5063d)},
  /* 10 */ {19, 0.3010299956639811, CNST_LIMB(0x8ac7230489e80000), CNST_LIMB(0xd83c94fb6d2ac34a)},
  /* 11 */ {18, 0.2890648263178878, CNST_LIMB(0x4d28cb56c33fa539), CNST_LIMB(0xa8adf7ae45e7577b)},
  /* 12 */ {17, 0.2789429456511298, CNST_LIMB(0x1eca170c00000000), CNST_LIMB(0xa10c2bec5da8f8f)},
  /* 13 */ {17, 0.2702381544273197, CNST_LIMB(0x780c7372621bd74d), CNST_LIMB(0x10f4becafe412ec3)},
  /* 14 */ {16, 0.2626495350371936, CNST_LIMB(0x1e39a5057d810000), CNST_LIMB(0xf08480f672b4e86)},
  /* 15 */ {16, 0.2559580248098155, CNST_LIMB(0x5b27ac993df97701), CNST_LIMB(0x6779c7f90dc42f48)},
  /* 16 */ {16, 0.2500000000000000, CNST_LIMB(0x4), CNST_LIMB(0x0)},
  /* 17 */ {15, 0.2446505421182260, CNST_LIMB(0x27b95e997e21d9f1), CNST_LIMB(0x9c71e11bab279323)},
  /* 18 */ {15, 0.2398124665681315, CNST_LIMB(0x5da0e1e53c5c8000), CNST_LIMB(0x5dfaa697ec6f6a1c)},
  /* 19 */ {15, 0.2354089133666382, CNST_LIMB(0xd2ae3299c1c4aedb), CNST_LIMB(0x3711783f6be7e9ec)},
  /* 20 */ {14, 0.2313782131597592, CNST_LIMB(0x16bcc41e90000000), CNST_LIMB(0x6849b86a12b9b01e)},
  /* 21 */ {14, 0.2276702486969530, CNST_LIMB(0x2d04b7fdd9c0ef49), CNST_LIMB(0x6bf097ba5ca5e239)},
  /* 22 */ {14, 0.2242438242175754, CNST_LIMB(0x5658597bcaa24000), CNST_LIMB(0x7b8015c8d7af8f08)},
  /* 23 */ {14, 0.2210647294575037, CNST_LIMB(0xa0e2073737609371), CNST_LIMB(0x975a24b3a3151b38)},
  /* 24 */ {13, 0.2181042919855316, CNST_LIMB(0xc29e98000000000), CNST_LIMB(0x50bd367972689db1)},
  /* 25 */ {13, 0.2153382790366965, CNST_LIMB(0x14adf4b7320334b9), CNST_LIMB(0x8c240c4aecb13bb5)},
  /* 26 */ {13, 0.2127460535533632, CNST_LIMB(0x226ed36478bfa000), CNST_LIMB(0xdbd2e56854e118c9)},
  /* 27 */ {13, 0.2103099178571525, CNST_LIMB(0x383d9170b85ff80b), CNST_LIMB(0x2351ffcaa9c7c4ae)},
  /* 28 */ {13, 0.2080145976765095, CNST_LIMB(0x5a3c23e39c000000), CNST_LIMB(0x6b24188ca33b0636)},
  /* 29 */ {13, 0.2058468324604344, CNST_LIMB(0x8e65137388122bcd), CNST_LIMB(0xcc3dceaf2b8ba99d)},
  /* 30 */ {13, 0.2037950470905062, CNST_LIMB(0xdd41bb36d259e000), CNST_LIMB(0x2832e835c6c7d6b6)},
  /* 31 */ {12, 0.2018490865820999, CNST_LIMB(0xaee5720ee830681), CNST_LIMB(0x76b6aa272e1873c5)},
  /* 32 */ {12, 0.2000000000000000, CNST_LIMB(0x5), CNST_LIMB(0x0)},
  /* 33 */ {12, 0.1982398631705605, CNST_LIMB(0x172588ad4f5f0981), CNST_LIMB(0x61eaf5d402c7bf4f)},
  /* 34 */ {12, 0.1965616322328226, CNST_LIMB(0x211e44f7d02c1000), CNST_LIMB(0xeeb658123ffb27ec)},
  /* 35 */ {12, 0.1949590218937863, CNST_LIMB(0x2ee56725f06e5c71), CNST_LIMB(0x5d5e3762e6fdf509)},
  /* 36 */ {12, 0.1934264036172708, CNST_LIMB(0x41c21cb8e1000000), CNST_LIMB(0xf24f62335024a295)},
  /* 37 */ {12, 0.1919587200065601, CNST_LIMB(0x5b5b57f8a98a5dd1), CNST_LIMB(0x66ae7831762efb6f)},
  /* 38 */ {12, 0.1905514124267734, CNST_LIMB(0x7dcff8986ea31000), CNST_LIMB(0x47388865a00f544)},
  /* 39 */ {12, 0.1892003595168700, CNST_LIMB(0xabd4211662a6b2a1), CNST_LIMB(0x7d673c33a123b54c)},
  /* 40 */ {12, 0.1879018247091076, CNST_LIMB(0xe8d4a51000000000), CNST_LIMB(0x19799812dea11197)},
  /* 41 */ {11, 0.1866524112389434, CNST_LIMB(0x7a32956ad081b79), CNST_LIMB(0xc27e62e0686feae)},
  /* 42 */ {11, 0.1854490234153689, CNST_LIMB(0x9f49aaff0e86800), CNST_LIMB(0x9b6e7507064ce7c7)},
  /* 43 */ {11, 0.1842888331487062, CNST_LIMB(0xce583bb812d37b3), CNST_LIMB(0x3d9ac2bf66cfed94)},
  /* 44 */ {11, 0.1831692509136336, CNST_LIMB(0x109b79a654c00000), CNST_LIMB(0xed46bc50ce59712a)},
  /* 45 */ {11, 0.1820879004699383, CNST_LIMB(0x1543beff214c8b95), CNST_LIMB(0x813d97e2c89b8d46)},
  /* 46 */ {11, 0.1810425967800402, CNST_LIMB(0x1b149a79459a3800), CNST_LIMB(0x2e81751956af8083)},
  /* 47 */ {11, 0.1800313266566926, CNST_LIMB(0x224edfb5434a830f), CNST_LIMB(0xdd8e0a95e30c0988)},
  /* 48 */ {11, 0.1790522317510413, CNST_LIMB(0x2b3fb00000000000), CNST_LIMB(0x7ad4dd48a0b5b167)},
  /* 49 */ {11, 0.1781035935540111, CNST_LIMB(0x3642798750226111), CNST_LIMB(0x2df495ccaa57147b)},
  /* 50 */ {11, 0.1771838201355579, CNST_LIMB(0x43c33c1937564800), CNST_LIMB(0xe392010175ee5962)},
  /* 51 */ {11, 0.1762914343888821, CNST_LIMB(0x54411b2441c3cd8b), CNST_LIMB(0x84eaf11b2fe7738e)},
  /* 52 */ {11, 0.1754250635819545, CNST_LIMB(0x6851455acd400000), CNST_LIMB(0x3a1e3971e008995d)},
  /* 53 */ {11, 0.1745834300480449, CNST_LIMB(0x80a23b117c8feb6d), CNST_LIMB(0xfd7a462344ffce25)},
  /* 54 */ {11, 0.1737653428714400, CNST_LIMB(0x9dff7d32d5dc1800), CNST_LIMB(0x9eca40b40ebcef8a)},
  /* 55 */ {11, 0.1729696904450771, CNST_LIMB(0xc155af6faeffe6a7), CNST_LIMB(0x52fa161a4a48e43d)},
  /* 56 */ {11, 0.1721954337940981, CNST_LIMB(0xebb7392e00000000), CNST_LIMB(0x1607a2cbacf930c1)},
  /* 57 */ {10, 0.1714416005739134, CNST_LIMB(0x50633659656d971), CNST_LIMB(0x97a014f8e3be55f1)},
  /* 58 */ {10, 0.1707072796637201, CNST_LIMB(0x5fa8624c7fba400), CNST_LIMB(0x568df8b76cbf212c)},
  /* 59 */ {10, 0.1699916162869140, CNST_LIMB(0x717d9faa73c5679), CNST_LIMB(0x20ba7c4b4e6ef492)},
  /* 60 */ {10, 0.1692938075987814, CNST_LIMB(0x86430aac6100000), CNST_LIMB(0xe81ee46b9ef492f5)},
  /* 61 */ {10, 0.1686130986895011, CNST_LIMB(0x9e64d9944b57f29), CNST_LIMB(0x9dc0d10d51940416)},
  /* 62 */ {10, 0.1679487789570419, CNST_LIMB(0xba5ca5392cb0400), CNST_LIMB(0x5fa8ed2f450272a5)},
  /* 63 */ {10, 0.1673001788101741, CNST_LIMB(0xdab2ce1d022cd81), CNST_LIMB(0x2ba9eb8c5e04e641)},
  /* 64 */ {10, 0.1666666666666667, CNST_LIMB(0x6), CNST_LIMB(0x0)},
  /* 65 */ {10, 0.1660476462159378, CNST_LIMB(0x12aeed5fd3e2d281), CNST_LIMB(0xb67759cc00287bf1)},
  /* 66 */ {10, 0.1654425539190583, CNST_LIMB(0x15c3da1572d50400), CNST_LIMB(0x78621feeb7f4ed33)},
  /* 67 */ {10, 0.1648508567221604, CNST_LIMB(0x194c05534f75ee29), CNST_LIMB(0x43d55b5f72943bc0)},
  /* 68 */ {10, 0.1642720499620502, CNST_LIMB(0x1d56299ada100000), CNST_LIMB(0x173decb64d1d4409)},
  /* 69 */ {10, 0.1637056554452156, CNST_LIMB(0x21f2a089a4ff4f79), CNST_LIMB(0xe29fb54fd6b6074f)},
  /* 70 */ {10, 0.1631512196835108, CNST_LIMB(0x2733896c68d9a400), CNST_LIMB(0xa1f1f5c210d54e62)},
  /* 71 */ {10, 0.1626083122716341, CNST_LIMB(0x2d2cf2c33b533c71), CNST_LIMB(0x6aac7f9bfafd57b2)},
  /* 72 */ {10, 0.1620765243931223, CNST_LIMB(0x33f506e440000000), CNST_LIMB(0x3b563c2478b72ee2)},
  /* 73 */ {10, 0.1615554674429964, CNST_LIMB(0x3ba43bec1d062211), CNST_LIMB(0x12b536b574e92d1b)},
  /* 74 */ {10, 0.1610447717564444, CNST_LIMB(0x4455872d8fd4e400), CNST_LIMB(0xdf86c03020404fa5)},
  /* 75 */ {10, 0.1605440854340214, CNST_LIMB(0x4e2694539f2f6c59), CNST_LIMB(0xa34adf02234eea8e)},
  /* 76 */ {10, 0.1600530732548213, CNST_LIMB(0x5938006c18900000), CNST_LIMB(0x6f46eb8574eb59dd)},
  /* 77 */ {10, 0.1595714156699382, CNST_LIMB(0x65ad9912474aa649), CNST_LIMB(0x42459b481df47cec)},
  /* 78 */ {10, 0.1590988078692941, CNST_LIMB(0x73ae9ff4241ec400), CNST_LIMB(0x1b424b95d80ca505)},
  /* 79 */ {10, 0.1586349589155960, CNST_LIMB(0x836612ee9c4ce1e1), CNST_LIMB(0xf2c1b982203a0dac)},
  /* 80 */ {10, 0.1581795909397823, CNST_LIMB(0x9502f90000000000), CNST_LIMB(0xb7cdfd9d7bdbab7d)},
  /* 81 */ {10, 0.1577324383928644, CNST_LIMB(0xa8b8b452291fe821), CNST_LIMB(0x846d550e37b5063d)},
  /* 82 */ {10, 0.1572932473495469, CNST_LIMB(0xbebf59a07dab4400), CNST_LIMB(0x57931eeaf85cf64f)},
  /* 83 */ {10, 0.1568617748594410, CNST_LIMB(0xd7540d4093bc3109), CNST_LIMB(0x305a944507c82f47)},
  /* 84 */ {10, 0.1564377883420716, CNST_LIMB(0xf2b96616f1900000), CNST_LIMB(0xe007ccc9c22781a)},
  /* 85 */ {9, 0.1560210650222250, CNST_LIMB(0x336de62af2bca35), CNST_LIMB(0x3e92c42e000eeed4)},
  /* 86 */ {9, 0.1556113914024940, CNST_LIMB(0x39235ec33d49600), CNST_LIMB(0x1ebe59130db2795e)},
  /* 87 */ {9, 0.1552085627701551, CNST_LIMB(0x3f674e539585a17), CNST_LIMB(0x268859e90f51b89)},
  /* 88 */ {9, 0.1548123827357682, CNST_LIMB(0x4645b6958000000), CNST_LIMB(0xd24cde0463108cfa)},
  /* 89 */ {9, 0.1544226628011101, CNST_LIMB(0x4dcb74afbc49c19), CNST_LIMB(0xa536009f37adc383)},
  /* 90 */ {9, 0.1540392219542636, CNST_LIMB(0x56064e1d18d9a00), CNST_LIMB(0x7cea06ce1c9ace10)},
  /* 91 */ {9, 0.1536618862898642, CNST_LIMB(0x5f04fe2cd8a39fb), CNST_LIMB(0x58db032e72e8ba43)},
  /* 92 */ {9, 0.1532904886526781, CNST_LIMB(0x68d74421f5c0000), CNST_LIMB(0x388cc17cae105447)},
  /* 93 */ {9, 0.1529248683028321, CNST_LIMB(0x738df1f6ab4827d), CNST_LIMB(0x1b92672857620ce0)},
  /* 94 */ {9, 0.1525648706011593, CNST_LIMB(0x7f3afbc9cfb5e00), CNST_LIMB(0x18c6a9575c2ade4)},
  /* 95 */ {9, 0.1522103467132434, CNST_LIMB(0x8bf187fba88f35f), CNST_LIMB(0xd44da7da8e44b24f)},
  /* 96 */ {9, 0.1518611533308632, CNST_LIMB(0x99c600000000000), CNST_LIMB(0xaa2f78f1b4cc6794)},
  /* 97 */ {9, 0.1515171524096389, CNST_LIMB(0xa8ce21eb6531361), CNST_LIMB(0x843c067d091ee4cc)},
  /* 98 */ {9, 0.1511782109217764, CNST_LIMB(0xb92112c1a0b6200), CNST_LIMB(0x62005e1e913356e3)},
  /* 99 */ {9, 0.1508442006228941, CNST_LIMB(0xcad7718b8747c43), CNST_LIMB(0x4316eed01dedd518)},
  /* 100 */ {9, 0.1505149978319906, CNST_LIMB(0xde0b6b3a7640000), CNST_LIMB(0x2725dd1d243aba0e)},
  /* 101 */ {9, 0.1501904832236879, CNST_LIMB(0xf2d8cf5fe6d74c5), CNST_LIMB(0xddd9057c24cb54f)},
  /* 102 */ {9, 0.1498705416319474, CNST_LIMB(0x1095d25bfa712600), CNST_LIMB(0xedeee175a736d2a1)},
  /* 103 */ {9, 0.1495550618645152, CNST_LIMB(0x121b7c4c3698faa7), CNST_LIMB(0xc4699f3df8b6b328)},
  /* 104 */ {9, 0.1492439365274121, CNST_LIMB(0x13c09e8d68000000), CNST_LIMB(0x9ebbe7d859cb5a7c)},
  /* 105 */ {9, 0.1489370618588283, CNST_LIMB(0x15876ccb0b709ca9), CNST_LIMB(0x7c828b9887eb2179)},
  /* 106 */ {9, 0.1486343375718350, CNST_LIMB(0x17723c2976da2a00), CNST_LIMB(0x5d652ab99001adcf)},
  /* 107 */ {9, 0.1483356667053617, CNST_LIMB(0x198384e9c259048b), CNST_LIMB(0x4114f1754e5d7b32)},
  /* 108 */ {9, 0.1480409554829326, CNST_LIMB(0x1bbde41dfeec0000), CNST_LIMB(0x274b7c902f7e0188)},
  /* 109 */ {9, 0.1477501131786861, CNST_LIMB(0x1e241d6e3337910d), CNST_LIMB(0xfc9e0fbb32e210c)},
  /* 110 */ {9, 0.1474630519902391, CNST_LIMB(0x20b91cee9901ee00), CNST_LIMB(0xf4afa3e594f8ea1f)},
  /* 111 */ {9, 0.1471796869179852, CNST_LIMB(0x237ff9079863dfef), CNST_LIMB(0xcd85c32e9e4437b0)},
  /* 112 */ {9, 0.1468999356504447, CNST_LIMB(0x267bf47000000000), CNST_LIMB(0xa9bbb147e0dd92a8)},
  /* 113 */ {9, 0.1466237184553111, CNST_LIMB(0x29b08039fbeda7f1), CNST_LIMB(0x8900447b70e8eb82)},
  /* 114 */ {9, 0.1463509580758620, CNST_LIMB(0x2d213df34f65f200), CNST_LIMB(0x6b0a92adaad5848a)},
  /* 115 */ {9, 0.1460815796324244, CNST_LIMB(0x30d201d957a7c2d3), CNST_LIMB(0x4f990ad8740f0ee5)},
  /* 116 */ {9, 0.1458155105286054, CNST_LIMB(0x34c6d52160f40000), CNST_LIMB(0x3670a9663a8d3610)},
  /* 117 */ {9, 0.1455526803620167, CNST_LIMB(0x3903f855d8f4c755), CNST_LIMB(0x1f5c44188057be3c)},
  /* 118 */ {9, 0.1452930208392428, CNST_LIMB(0x3d8de5c8ec59b600), CNST_LIMB(0xa2bea956c4e4977)},
  /* 119 */ {9, 0.1450364656948130, CNST_LIMB(0x4269541d1ff01337), CNST_LIMB(0xed68b23033c3637e)},
  /* 120 */ {9, 0.1447829506139581, CNST_LIMB(0x479b38e478000000), CNST_LIMB(0xc99cf624e50549c5)},
  /* 121 */ {9, 0.1445324131589439, CNST_LIMB(0x4d28cb56c33fa539), CNST_LIMB(0xa8adf7ae45e7577b)},
  /* 122 */ {9, 0.1442847926987864, CNST_LIMB(0x5317871fa13aba00), CNST_LIMB(0x8a5bc740b1c113e5)},
  /* 123 */ {9, 0.1440400303421672, CNST_LIMB(0x596d2f44de9fa71b), CNST_LIMB(0x6e6c7efb81cfbb9b)},
  /* 124 */ {9, 0.1437980688733775, CNST_LIMB(0x602fd125c47c0000), CNST_LIMB(0x54aba5c5cada5f10)},
  /* 125 */ {9, 0.1435588526911310, CNST_LIMB(0x6765c793fa10079d), CNST_LIMB(0x3ce9a36f23c0fc90)},
  /* 126 */ {9, 0.1433223277500932, CNST_LIMB(0x6f15be069b847e00), CNST_LIMB(0x26fb43de2c8cd2a8)},
  /* 127 */ {9, 0.1430884415049874, CNST_LIMB(0x7746b3e82a77047f), CNST_LIMB(0x12b94793db8486a1)},
  /* 128 */ {9, 0.1428571428571428, CNST_LIMB(0x7), CNST_LIMB(0x0)},
  /* 129 */ {9, 0.1426283821033600, CNST_LIMB(0x894953f7ea890481), CNST_LIMB(0xdd5deca404c0156d)},
  /* 130 */ {9, 0.1424021108869747, CNST_LIMB(0x932abffea4848200), CNST_LIMB(0xbd51373330291de0)},
  /* 131 */ {9, 0.1421782821510107, CNST_LIMB(0x9dacb687d3d6a163), CNST_LIMB(0x9fa4025d66f23085)},
  /* 132 */ {9, 0.1419568500933153, CNST_LIMB(0xa8d8102a44840000), CNST_LIMB(0x842530ee2db4949d)},
  /* 133 */ {9, 0.1417377701235801, CNST_LIMB(0xb4b60f9d140541e5), CNST_LIMB(0x6aa7f2766b03dc25)},
  /* 134 */ {9, 0.1415209988221527, CNST_LIMB(0xc15065d4856e4600), CNST_LIMB(0x53035ba7ebf32e8d)},
  /* 135 */ {9, 0.1413064939005528, CNST_LIMB(0xceb1363f396d23c7), CNST_LIMB(0x3d12091fc9fb4914)},
  /* 136 */ {9, 0.1410942141636095, CNST_LIMB(0xdce31b2488000000), CNST_LIMB(0x28b1cb81b1ef1849)},
  /* 137 */ {9, 0.1408841194731412, CNST_LIMB(0xebf12a24bca135c9), CNST_LIMB(0x15c35be67ae3e2c9)},
  /* 138 */ {9, 0.1406761707131039, CNST_LIMB(0xfbe6f8dbf88f4a00), CNST_LIMB(0x42a17bd09be1ff0)},
  /* 139 */ {8, 0.1404703297561400, CNST_LIMB(0x1ef156c084ce761), CNST_LIMB(0x8bf461f03cf0bbf)},
  /* 140 */ {8, 0.1402665594314587, CNST_LIMB(0x20c4e3b94a10000), CNST_LIMB(0xf3fbb43f68a32d05)},
  /* 141 */ {8, 0.1400648234939879, CNST_LIMB(0x22b0695a08ba421), CNST_LIMB(0xd84f44c48564dc19)},
  /* 142 */ {8, 0.1398650865947379, CNST_LIMB(0x24b4f35d7a4c100), CNST_LIMB(0xbe58ebcce7956abe)},
  /* 143 */ {8, 0.1396673142523192, CNST_LIMB(0x26d397284975781), CNST_LIMB(0xa5fac463c7c134b7)},
  /* 144 */ {8, 0.1394714728255649, CNST_LIMB(0x290d74100000000), CNST_LIMB(0x8f19241e28c7d757)},
  /* 145 */ {8, 0.1392775294872041, CNST_LIMB(0x2b63b3a37866081), CNST_LIMB(0x799a6d046c0ae1ae)},
  /* 146 */ {8, 0.1390854521985406, CNST_LIMB(0x2dd789f4d894100), CNST_LIMB(0x6566e37d746a9e40)},
  /* 147 */ {8, 0.1388952096850913, CNST_LIMB(0x306a35e51b58721), CNST_LIMB(0x526887dbfb5f788f)},
  /* 148 */ {8, 0.1387067714131417, CNST_LIMB(0x331d01712e10000), CNST_LIMB(0x408af3382b8efd3d)},
  /* 149 */ {8, 0.1385201075671774, CNST_LIMB(0x35f14200a827c61), CNST_LIMB(0x2fbb374806ec05f1)},
  /* 150 */ {8, 0.1383351890281539, CNST_LIMB(0x38e858b62216100), CNST_LIMB(0x1fe7c0f0afce87fe)},
  /* 151 */ {8, 0.1381519873525671, CNST_LIMB(0x3c03b2c13176a41), CNST_LIMB(0x11003d517540d32e)},
  /* 152 */ {8, 0.1379704747522905, CNST_LIMB(0x3f44c9b21000000), CNST_LIMB(0x2f5810f98eff0dc)},
  /* 153 */ {8, 0.1377906240751463, CNST_LIMB(0x42ad23cef3113c1), CNST_LIMB(0xeb72e35e7840d910)},
  /* 154 */ {8, 0.1376124087861776, CNST_LIMB(0x463e546b19a2100), CNST_LIMB(0xd27de19593dc3614)},
  /* 155 */ {8, 0.1374358029495937, CNST_LIMB(0x49f9fc3f96684e1), CNST_LIMB(0xbaf391fd3e5e6fc2)},
  /* 156 */ {8, 0.1372607812113589, CNST_LIMB(0x4de1c9c5dc10000), CNST_LIMB(0xa4bd38c55228c81d)},
  /* 157 */ {8, 0.1370873187823978, CNST_LIMB(0x51f77994116d2a1), CNST_LIMB(0x8fc5a8de8e1de782)},
  /* 158 */ {8, 0.1369153914223921, CNST_LIMB(0x563cd6bb3398100), CNST_LIMB(0x7bf9265bea9d3a3b)},
  /* 159 */ {8, 0.1367449754241439, CNST_LIMB(0x5ab3bb270beeb01), CNST_LIMB(0x69454b325983dccd)},
  /* 160 */ {8, 0.1365760475984821, CNST_LIMB(0x5f5e10000000000), CNST_LIMB(0x5798ee2308c39df9)},
  /* 161 */ {8, 0.1364085852596902, CNST_LIMB(0x643dce0ec16f501), CNST_LIMB(0x46e40ba0fa66a753)},
  /* 162 */ {8, 0.1362425662114337, CNST_LIMB(0x6954fe21e3e8100), CNST_LIMB(0x3717b0870b0db3a7)},
  /* 163 */ {8, 0.1360779687331669, CNST_LIMB(0x6ea5b9755f440a1), CNST_LIMB(0x2825e6775d11cdeb)},
  /* 164 */ {8, 0.1359147715670014, CNST_LIMB(0x74322a1c0410000), CNST_LIMB(0x1a01a1c09d1b4dac)},
  /* 165 */ {8, 0.1357529539050150, CNST_LIMB(0x79fc8b6ae8a46e1), CNST_LIMB(0xc9eb0a8bebc8f3e)},
  /* 166 */ {8, 0.1355924953769863, CNST_LIMB(0x80072a66d512100), CNST_LIMB(0xffe357ff59e6a004)},
  /* 167 */ {8, 0.1354333760385373, CNST_LIMB(0x86546633b42b9c1), CNST_LIMB(0xe7dfd1be05fa61a8)},
  /* 168 */ {8, 0.1352755763596663, CNST_LIMB(0x8ce6b0861000000), CNST_LIMB(0xd11ed6fc78f760e5)},
  /* 169 */ {8, 0.1351190772136599, CNST_LIMB(0x93c08e16a022441), CNST_LIMB(0xbb8db609dd29ebfe)},
  /* 170 */ {8, 0.1349638598663645, CNST_LIMB(0x9ae49717f026100), CNST_LIMB(0xa71aec8d1813d532)},
  /* 171 */ {8, 0.1348099059658079, CNST_LIMB(0xa25577ae24c1a61), CNST_LIMB(0x93b612a9f20fbc02)},
  /* 172 */ {8, 0.1346571975321549, CNST_LIMB(0xaa15f068e610000), CNST_LIMB(0x814fc7b19a67d317)},
  /* 173 */ {8, 0.1345057169479844, CNST_LIMB(0xb228d6bf7577921), CNST_LIMB(0x6fd9a03f2e0a4b7c)},
  /* 174 */ {8, 0.1343554469488779, CNST_LIMB(0xba91158ef5c4100), CNST_LIMB(0x5f4615a38d0d316e)},
  /* 175 */ {8, 0.1342063706143054, CNST_LIMB(0xc351ad9aec0b681), CNST_LIMB(0x4f8876863479a286)},
  /* 176 */ {8, 0.1340584713587980, CNST_LIMB(0xcc6db6100000000), CNST_LIMB(0x4094d8a3041b60eb)},
  /* 177 */ {8, 0.1339117329233981, CNST_LIMB(0xd5e85d09025c181), CNST_LIMB(0x32600b8ed883a09b)},
  /* 178 */ {8, 0.1337661393673756, CNST_LIMB(0xdfc4e816401c100), CNST_LIMB(0x24df8c6eb4b6d1f1)},
  /* 179 */ {8, 0.1336216750601996, CNST_LIMB(0xea06b4c72947221), CNST_LIMB(0x18097a8ee151acef)},
  /* 180 */ {8, 0.1334783246737591, CNST_LIMB(0xf4b139365210000), CNST_LIMB(0xbd48cc8ec1cd8e3)},
  /* 181 */ {8, 0.1333360731748201, CNST_LIMB(0xffc80497d520961), CNST_LIMB(0x3807a8d67485fb)},
  /* 182 */ {8, 0.1331949058177136, CNST_LIMB(0x10b4ebfca1dee100), CNST_LIMB(0xea5768860b62e8d8)},
  /* 183 */ {8, 0.1330548081372441, CNST_LIMB(0x117492de921fc141), CNST_LIMB(0xd54faf5b635c5005)},
  /* 184 */ {8, 0.1329157659418126, CNST_LIMB(0x123bb2ce41000000), CNST_LIMB(0xc14a56233a377926)},
  /* 185 */ {8, 0.1327777653067443, CNST_LIMB(0x130a8b6157bdecc1), CNST_LIMB(0xae39a88db7cd329f)},
  /* 186 */ {8, 0.1326407925678156, CNST_LIMB(0x13e15dede0e8a100), CNST_LIMB(0x9c10bde69efa7ab6)},
  /* 187 */ {8, 0.1325048343149731, CNST_LIMB(0x14c06d941c0ca7e1), CNST_LIMB(0x8ac36c42a2836497)},
  /* 188 */ {8, 0.1323698773862368, CNST_LIMB(0x15a7ff487a810000), CNST_LIMB(0x7a463c8b84f5ef67)},
  /* 189 */ {8, 0.1322359088617821, CNST_LIMB(0x169859ddc5c697a1), CNST_LIMB(0x6a8e5f5ad090fd4b)},
  /* 190 */ {8, 0.1321029160581950, CNST_LIMB(0x1791c60f6fed0100), CNST_LIMB(0x5b91a2943596fc56)},
  /* 191 */ {8, 0.1319708865228925, CNST_LIMB(0x18948e8c0e6fba01), CNST_LIMB(0x4d4667b1c468e8f0)},
  /* 192 */ {8, 0.1318398080287045, CNST_LIMB(0x19a1000000000000), CNST_LIMB(0x3fa39ab547994daf)},
  /* 193 */ {8, 0.1317096685686114, CNST_LIMB(0x1ab769203dafc601), CNST_LIMB(0x32a0a9b2faee1e2a)},
  /* 194 */ {8, 0.1315804563506306, CNST_LIMB(0x1bd81ab557f30100), CNST_LIMB(0x26357ceac0e96962)},
  /* 195 */ {8, 0.1314521597928493, CNST_LIMB(0x1d0367a69fed1ba1), CNST_LIMB(0x1a5a6f65caa5859e)},
  /* 196 */ {8, 0.1313247675185968, CNST_LIMB(0x1e39a5057d810000), CNST_LIMB(0xf08480f672b4e86)},
  /* 197 */ {8, 0.1311982683517524, CNST_LIMB(0x1f7b2a18f29ac3e1), CNST_LIMB(0x4383340615612ca)},
  /* 198 */ {8, 0.1310726513121843, CNST_LIMB(0x20c850694c2aa100), CNST_LIMB(0xf3c77969ee4be5a2)},
  /* 199 */ {8, 0.1309479056113158, CNST_LIMB(0x222173cc014980c1), CNST_LIMB(0xe00993cc187c5ec9)},
  /* 200 */ {8, 0.1308240206478128, CNST_LIMB(0x2386f26fc1000000), CNST_LIMB(0xcd2b297d889bc2b6)},
  /* 201 */ {8, 0.1307009860033912, CNST_LIMB(0x24f92ce8af296d41), CNST_LIMB(0xbb214d5064862b22)},
  /* 202 */ {8, 0.1305787914387386, CNST_LIMB(0x2678863cd0ece100), CNST_LIMB(0xa9e1a7ca7ea10e20)},
  /* 203 */ {8, 0.1304574268895465, CNST_LIMB(0x280563f0a9472d61), CNST_LIMB(0x99626e72b39ea0cf)},
  /* 204 */ {8, 0.1303368824626505, CNST_LIMB(0x29a02e1406210000), CNST_LIMB(0x899a5ba9c13fafd9)},
  /* 205 */ {8, 0.1302171484322746, CNST_LIMB(0x2b494f4efe6d2e21), CNST_LIMB(0x7a80a705391e96ff)},
  /* 206 */ {8, 0.1300982152363760, CNST_LIMB(0x2d0134ef21cbc100), CNST_LIMB(0x6c0cfe23de23042a)},
  /* 207 */ {8, 0.1299800734730872, CNST_LIMB(0x2ec84ef4da2ef581), CNST_LIMB(0x5e377df359c944dd)},
  /* 208 */ {8, 0.1298627138972530, CNST_LIMB(0x309f102100000000), CNST_LIMB(0x50f8ac5fc8f53985)},
  /* 209 */ {8, 0.1297461274170591, CNST_LIMB(0x3285ee02a1420281), CNST_LIMB(0x44497266278e35b7)},
  /* 210 */ {8, 0.1296303050907487, CNST_LIMB(0x347d6104fc324100), CNST_LIMB(0x382316831f7ee175)},
  /* 211 */ {8, 0.1295152381234257, CNST_LIMB(0x3685e47dade53d21), CNST_LIMB(0x2c7f377833b8946e)},
  /* 212 */ {8, 0.1294009178639407, CNST_LIMB(0x389ff6bb15610000), CNST_LIMB(0x2157c761ab4163ef)},
  /* 213 */ {8, 0.1292873358018581, CNST_LIMB(0x3acc1912ebb57661), CNST_LIMB(0x16a7071803cc49a9)},
  /* 214 */ {8, 0.1291744835645007, CNST_LIMB(0x3d0acff111946100), CNST_LIMB(0xc6781d80f8224fc)},
  /* 215 */ {8, 0.1290623529140715, CNST_LIMB(0x3f5ca2e692eaf841), CNST_LIMB(0x294092d370a900b)},
  /* 216 */ {8, 0.1289509357448472, CNST_LIMB(0x41c21cb8e1000000), CNST_LIMB(0xf24f62335024a295)},
  /* 217 */ {8, 0.1288402240804449, CNST_LIMB(0x443bcb714399a5c1), CNST_LIMB(0xe03b98f103fad6d2)},
  /* 218 */ {8, 0.1287302100711567, CNST_LIMB(0x46ca406c81af2100), CNST_LIMB(0xcee3d32cad2a9049)},
  /* 219 */ {8, 0.1286208859913518, CNST_LIMB(0x496e106ac22aaae1), CNST_LIMB(0xbe3f9df9277fdada)},
  /* 220 */ {8, 0.1285122442369443, CNST_LIMB(0x4c27d39fa5410000), CNST_LIMB(0xae46f0d94c05e933)},
  /* 221 */ {8, 0.1284042773229231, CNST_LIMB(0x4ef825c296e43ca1), CNST_LIMB(0x9ef2280fb437a33d)},
  /* 222 */ {8, 0.1282969778809442, CNST_LIMB(0x51dfa61f5ad88100), CNST_LIMB(0x9039ff426d3f284b)},
  /* 223 */ {8, 0.1281903386569819, CNST_LIMB(0x54def7a6d2f16901), CNST_LIMB(0x82178c6d6b51f8f4)},
  /* 224 */ {8, 0.1280843525090381, CNST_LIMB(0x57f6c10000000000), CNST_LIMB(0x74843b1ee4c1e053)},
  /* 225 */ {8, 0.1279790124049077, CNST_LIMB(0x5b27ac993df97701), CNST_LIMB(0x6779c7f90dc42f48)},
  /* 226 */ {8, 0.1278743114199984, CNST_LIMB(0x5e7268b9bbdf8100), CNST_LIMB(0x5af23c74f9ad9fe9)},
  /* 227 */ {8, 0.1277702427352035, CNST_LIMB(0x61d7a7932ff3d6a1), CNST_LIMB(0x4ee7eae2acdc617e)},
  /* 228 */ {8, 0.1276667996348261, CNST_LIMB(0x65581f53c8c10000), CNST_LIMB(0x43556aa2ac262a0b)},
  /* 229 */ {8, 0.1275639755045533, CNST_LIMB(0x68f48a385b8320e1), CNST_LIMB(0x3835949593b8ddd1)},
  /* 230 */ {8, 0.1274617638294791, CNST_LIMB(0x6cada69ed07c2100), CNST_LIMB(0x2d837fbe78458762)},
  /* 231 */ {8, 0.1273601581921741, CNST_LIMB(0x70843718cdbf27c1), CNST_LIMB(0x233a7e150a54a555)},
  /* 232 */ {8, 0.1272591522708010, CNST_LIMB(0x7479027ea1000000), CNST_LIMB(0x19561984a50ff8fe)},
  /* 233 */ {8, 0.1271587398372755, CNST_LIMB(0x788cd40268f39641), CNST_LIMB(0xfd211159fe3490f)},
  /* 234 */ {8, 0.1270589147554692, CNST_LIMB(0x7cc07b437ecf6100), CNST_LIMB(0x6aa563e655033e3)},
  /* 235 */ {8, 0.1269596709794558, CNST_LIMB(0x8114cc6220762061), CNST_LIMB(0xfbb614b3f2d3b14c)},
  /* 236 */ {8, 0.1268610025517973, CNST_LIMB(0x858aa0135be10000), CNST_LIMB(0xeac0f8837fb05773)},
  /* 237 */ {8, 0.1267629036018709, CNST_LIMB(0x8a22d3b53c54c321), CNST_LIMB(0xda6e4c10e8615ca5)},
  /* 238 */ {8, 0.1266653683442337, CNST_LIMB(0x8ede496339f34100), CNST_LIMB(0xcab755a8d01fa67f)},
  /* 239 */ {8, 0.1265683910770258, CNST_LIMB(0x93bde80aec3a1481), CNST_LIMB(0xbb95a9ae71aa3e0c)},
  /* 240 */ {8, 0.1264719661804097, CNST_LIMB(0x98c29b8100000000), CNST_LIMB(0xad0326c296b4f529)},
  /* 241 */ {8, 0.1263760881150453, CNST_LIMB(0x9ded549671832381), CNST_LIMB(0x9ef9f21eed31b7c1)},
  /* 242 */ {8, 0.1262807514205999, CNST_LIMB(0xa33f092e0b1ac100), CNST_LIMB(0x91747422be14b0b2)},
  /* 243 */ {8, 0.1261859507142915, CNST_LIMB(0xa8b8b452291fe821), CNST_LIMB(0x846d550e37b5063d)},
  /* 244 */ {8, 0.1260916806894653, CNST_LIMB(0xae5b564ac3a10000), CNST_LIMB(0x77df79e9a96c06f6)},
  /* 245 */ {8, 0.1259979361142023, CNST_LIMB(0xb427f4b3be74c361), CNST_LIMB(0x6bc6019636c7d0c2)},
  /* 246 */ {8, 0.1259047118299582, CNST_LIMB(0xba1f9a938041e100), CNST_LIMB(0x601c4205aebd9e47)},
  /* 247 */ {8, 0.1258120027502338, CNST_LIMB(0xc0435871d1110f41), CNST_LIMB(0x54ddc59756f05016)},
  /* 248 */ {8, 0.1257198038592741, CNST_LIMB(0xc694446f01000000), CNST_LIMB(0x4a0648979c838c18)},
  /* 249 */ {8, 0.1256281102107963, CNST_LIMB(0xcd137a5b57ac3ec1), CNST_LIMB(0x3f91b6e0bb3a053d)},
  /* 250 */ {8, 0.1255369169267456, CNST_LIMB(0xd3c21bcecceda100), CNST_LIMB(0x357c299a88ea76a5)},
  /* 251 */ {8, 0.1254462191960791, CNST_LIMB(0xdaa150410b788de1), CNST_LIMB(0x2bc1e517aecc56e3)},
  /* 252 */ {8, 0.1253560122735751, CNST_LIMB(0xe1b24521be010000), CNST_LIMB(0x225f56ceb3da9f5d)},
  /* 253 */ {8, 0.1252662914786691, CNST_LIMB(0xe8f62df12777c1a1), CNST_LIMB(0x1951136d53ad63ac)},
  /* 254 */ {8, 0.1251770521943144, CNST_LIMB(0xf06e445906fc0100), CNST_LIMB(0x1093d504b3cd7d93)},
  /* 255 */ {8, 0.1250882898658681, CNST_LIMB(0xf81bc845c81bf801), CNST_LIMB(0x824794d1ec1814f)},
};
#endif

static int
#if __STDC__
__gmp_assert_fail (const char *filename, int linenum,
                   const char *expr)
#else
__gmp_assert_fail (filename, linenum, expr)
char *filename;
int  linenum;
char *expr;
#endif
{
#if 0
  if (filename != NULL && filename[0] != '\0')
    {
      fprintf (stderr, "%s:", filename);
      if (linenum != -1)
        fprintf (stderr, "%d: ", linenum);
    }

  fprintf (stderr, "GNU MP assertion failed: %s\n", expr);
  abort();
#endif

  /*NOTREACHED*/
  return 0;
}

/* __clz_tab -- support for longlong.h */

const
unsigned char __clz_tab[] =
{
  0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
  6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
};


/****************************************/

#ifndef HAVE_ALLOCA

typedef struct tmp_stack tmp_stack;

static unsigned long max_total_allocation = 0;
static unsigned long current_total_allocation = 0;

static tmp_stack xxx = {&xxx, &xxx, 0};
static tmp_stack *current = &xxx;

/* The rounded size of the header of each allocation block.  */
#define HSIZ ((sizeof (tmp_stack) + __TMP_ALIGN - 1) & -__TMP_ALIGN)

/* Allocate a block of exactly <size> bytes.  This should only be called
   through the TMP_ALLOC macro, which takes care of rounding/alignment.  */
void *
#if __STDC__
__gmp_tmp_alloc (unsigned long size)
#else
__gmp_tmp_alloc (size)
     unsigned long size;
#endif
{
  void *that;

  if (size > (char *) current->end - (char *) current->alloc_point)
    {
      void *chunk;
      tmp_stack *header;
      unsigned long chunk_size;
      unsigned long now;

      /* Allocate a chunk that makes the total current allocation somewhat
	 larger than the maximum allocation ever.  If size is very large, we
	 allocate that much.  */

      now = current_total_allocation + size;
      if (now > max_total_allocation)
	{
	  /* We need more temporary memory than ever before.  Increase
	     for future needs.  */
	  now = now * 3 / 2;
	  chunk_size = now - current_total_allocation + HSIZ;
	  current_total_allocation = now;
	  max_total_allocation = current_total_allocation;
	}
      else
	{
	  chunk_size = max_total_allocation - current_total_allocation + HSIZ;
	  current_total_allocation = max_total_allocation;
	}

      chunk = MALLOC (chunk_size);
      header = (tmp_stack *) chunk;
      header->end = (char *) chunk + chunk_size;
      header->alloc_point = (char *) chunk + HSIZ;
      header->prev = current;
      current = header;
    }

  that = current->alloc_point;
  current->alloc_point = (char *) that + size;
  return that;
}

/* Typically called at function entry.  <mark> is assigned so that
   __gmp_tmp_free can later be used to reclaim all subsequently allocated
   storage.  */
void
#if __STDC__
__gmp_tmp_mark (tmp_marker *mark)
#else
__gmp_tmp_mark (mark)
     tmp_marker *mark;
#endif
{
  mark->which_chunk = current;
  mark->alloc_point = current->alloc_point;
}

/* Free everything allocated since <mark> was assigned by __gmp_tmp_mark */
void
#if __STDC__
__gmp_tmp_free (tmp_marker *mark)
#else
__gmp_tmp_free (mark)
     tmp_marker *mark;
#endif
{
  while (mark->which_chunk != current)
    {
      tmp_stack *tmp;

      tmp = current;
      current = tmp->prev;
      current_total_allocation -= (((char *) (tmp->end) - (char *) tmp) - HSIZ);
      FREE (tmp, (char *) tmp->end - (char *) tmp);
    }
  current->alloc_point = mark->alloc_point;
}

void scheme_gmp_tls_init(long *s) 
{
  s[0] = 0;
  s[1] = 0;
  s[2] = (long)&xxx;
  ((tmp_marker *)(s + 3))->which_chunk = &xxx;
  ((tmp_marker *)(s + 3))->alloc_point = &xxx;
}

void scheme_gmp_tls_load(long *s) 
{
  s[0] = (long)current_total_allocation;
  s[1] = (long)max_total_allocation;
  s[2] = (long)current;
}

void scheme_gmp_tls_unload(long *s)
{
  current_total_allocation = (unsigned long)s[0];
  max_total_allocation = (unsigned long)s[1];
  current = (tmp_stack *)s[2];
}

void scheme_gmp_tls_snapshot(long *s, long *save)
{
  save[0] = s[3];
  save[1] = s[4];
  __gmp_tmp_mark((tmp_marker *)(s + 3));
}

void scheme_gmp_tls_restore_snapshot(long *s, long *save, int do_free)
{
  long other[6];

  if (do_free == 2) {
    scheme_gmp_tls_load(other);
    scheme_gmp_tls_unload(s);
  }

  if (do_free)
    __gmp_tmp_free((tmp_marker *)(s + 3));  

  if (save) {
    s[3] = save[0];
    s[4] = save[1];
  }

  if (do_free == 2) {
    scheme_gmp_tls_load(s);
    scheme_gmp_tls_unload(other);
  }
}

#else

void scheme_gmp_tls_init(long *s) 
{
}

void scheme_gmp_tls_load(long *s) 
{
}

void scheme_gmp_tls_unload(long *s)
{
}

#endif
