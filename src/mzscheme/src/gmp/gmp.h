/* gmp.h -- Definitions for GNU multiple precision functions.

Copyright (C) 1991, 1993, 1994, 1995, 1996, 1997, 1999, 2000 Free Software
Foundation, Inc.

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

#ifndef __GMP_H__

#ifndef __GNU_MP__		/* to allow inclusion of both gmp.h and mp.h */
#define __GNU_MP__ 2
#define __need_size_t
#include <stddef.h>
#undef __need_size_t

#if defined(_MSC_VER)
# ifndef __STDC__
#  define __STDC__ 1
# endif
#endif

#if defined(__BORLANDC__)
# ifndef __STDC__
#  define __STDC__ 1
# endif
#endif

#if defined (__mips) && defined (_ABIN32)
/* Force the use of 64-bit limbs for all 64-bit MIPS CPUs if ABI permits.  */
#define _LONG_LONG_LIMB
#endif

#if (__STDC__-0) || defined (__cplusplus)
#define __gmp_const const
#define __gmp_signed signed
#else
#define __gmp_const
#define __gmp_signed
#endif

#if defined (__GNUC__)
#define __gmp_inline __inline__
#else
#define __gmp_inline
#endif

#ifndef _EXTERN_INLINE
#ifdef __GNUC__
#define _EXTERN_INLINE extern __inline__
#else
#define _EXTERN_INLINE static
#endif
#endif

#ifdef _SHORT_LIMB
typedef unsigned int		mp_limb_t;
typedef int			mp_limb_signed_t;
#else
#ifdef _LONG_LONG_LIMB
typedef unsigned long long int	mp_limb_t;
typedef long long int		mp_limb_signed_t;
#else
typedef unsigned long int	mp_limb_t;
typedef long int		mp_limb_signed_t;
#endif
#endif

typedef mp_limb_t *		mp_ptr;
typedef __gmp_const mp_limb_t *	mp_srcptr;
#if defined (_CRAY) && ! defined (_CRAYMPP)
/* plain `int' is much faster (48 bits) */
typedef int			mp_size_t;
typedef int			mp_exp_t;
#else
typedef long int		mp_size_t;
typedef long int		mp_exp_t;
#endif

typedef struct
{
  int _mp_alloc;		/* Number of *limbs* allocated and pointed
				   to by the _mp_d field.  */
  int _mp_size;			/* abs(_mp_size) is the number of limbs the
				   last field points to.  If _mp_size is
				   negative this is a negative number.  */
  mp_limb_t *_mp_d;		/* Pointer to the limbs.  */
} __mpz_struct;
#endif /* __GNU_MP__ */

typedef __mpz_struct MP_INT;
typedef __mpz_struct mpz_t[1];

typedef struct
{
  __mpz_struct _mp_num;
  __mpz_struct _mp_den;
} __mpq_struct;

typedef __mpq_struct MP_RAT;
typedef __mpq_struct mpq_t[1];

typedef struct
{
  int _mp_prec;			/* Max precision, in number of `mp_limb_t's.
				   Set by mpf_init and modified by
				   mpf_set_prec.  The area pointed to by the
				   _mp_d field contains `prec' + 1 limbs.  */
  int _mp_size;			/* abs(_mp_size) is the number of limbs the
				   last field points to.  If _mp_size is
				   negative this is a negative number.  */
  mp_exp_t _mp_exp;		/* Exponent, in the base of `mp_limb_t'.  */
  mp_limb_t *_mp_d;		/* Pointer to the limbs.  */
} __mpf_struct;

/* typedef __mpf_struct MP_FLOAT; */
typedef __mpf_struct mpf_t[1];

/* Available random number generation algorithms.  */
typedef enum
{
  GMP_RAND_ALG_DEFAULT = 0,
  GMP_RAND_ALG_LC = GMP_RAND_ALG_DEFAULT /* Linear congruential.  */
} gmp_randalg_t;

/* Linear congruential data struct.  */
typedef struct {
  mpz_t a;			/* Multiplier. */
  unsigned long int c;		/* Adder. */
  mpz_t m;			/* Modulus (valid only if m2exp == 0).  */
  unsigned long int m2exp;	/* If != 0, modulus is 2 ^ m2exp.  */
} __gmp_randata_lc;

/* Random state struct.  */
typedef struct
{
  mpz_t seed;			/* Current seed.  */
  gmp_randalg_t alg;		/* Algorithm used.  */
  union {			/* Algorithm specific data.  */
    __gmp_randata_lc *lc;	/* Linear congruential.  */
  } algdata;
} __gmp_randstate_struct;
typedef __gmp_randstate_struct gmp_randstate_t[1];

/* Types for function declarations in gmp files.  */
/* ??? Should not pollute user name space with these ??? */
typedef __gmp_const __mpz_struct *mpz_srcptr;
typedef __mpz_struct *mpz_ptr;
typedef __gmp_const __mpf_struct *mpf_srcptr;
typedef __mpf_struct *mpf_ptr;
typedef __gmp_const __mpq_struct *mpq_srcptr;
typedef __mpq_struct *mpq_ptr;

#ifndef _PROTO
#if (__STDC__-0) || defined (__cplusplus) || defined(__BORLANDC__)
#define _PROTO(x) x
#else
#define _PROTO(x) ()
#endif
#endif

#ifndef __MPN
/* Really use `defined (__STDC__)' here; we want it to be true for Sun C */
#if defined (__STDC__) || defined (__cplusplus) || defined(__BORLANDC__)
#define __MPN(x) scheme_gmpn_##x
#else
#define __MPN(x) scheme_gmpn_/**/x
#endif
#endif

#if defined (FILE) || defined (H_STDIO) || defined (_H_STDIO) \
 || defined (_STDIO_H) || defined (_STDIO_H_) || defined (__STDIO_H__) \
 || defined (_STDIO_INCLUDED) || defined (__dj_include_stdio_h_)
#define _GMP_H_HAVE_FILE 1
#endif

#if defined (__cplusplus)
extern "C" {
#endif

#define mp_set_memory_functions __gmp_set_memory_functions
void mp_set_memory_functions _PROTO ((void *(*) (size_t),
				      void *(*) (void *, size_t, size_t),
				      void (*) (void *, size_t)));

#define mp_bits_per_limb __gmp_bits_per_limb
extern __gmp_const int mp_bits_per_limb;

#if defined (__cplusplus)
}
#endif


/************ Low level positive-integer (i.e. N) routines.  ************/

/* This is ugly, but we need to make user calls reach the prefixed function. */
#define mpn_add			__MPN(add)
#define mpn_add_1		__MPN(add_1)
#define mpn_add_n		__MPN(add_n)
#define mpn_add_nc		__MPN(add_nc)
#define mpn_addmul_1		__MPN(addmul_1)
#define mpn_addsub_n		__MPN(addsub_n)
#define mpn_addsub_nc		__MPN(addsub_nc)
/* #define mpn_and_n		__MPN(and_n) */
/* #define mpn_andn_n		__MPN(andn_n) */
#define mpn_bdivmod		__MPN(bdivmod)
#define mpn_cmp			__MPN(cmp)
/* #define mpn_com_n		__MPN(com_n) */
#define mpn_copyd		__MPN(copyd)
#define mpn_copyi		__MPN(copyi)
#define mpn_divrem		__MPN(divrem)
#define mpn_divrem_1		__MPN(divrem_1)
#define mpn_divrem_2		__MPN(divrem_2)
#define mpn_dump		__MPN(dump)
#define mpn_gcd			__MPN(gcd)
#define mpn_gcd_1		__MPN(gcd_1)
#define mpn_gcdext		__MPN(gcdext)
#define mpn_get_str		__MPN(get_str)
#define mpn_hamdist		__MPN(hamdist)
#define mpn_invert_limb 	__MPN(invert_limb)
/* #define mpn_ior_n		__MPN(ior_n) */
/* #define mpn_iorn_n		__MPN(iorn_n) */
/* #define mpn_kara_mul_n	__MPN(kara_mul_n)  internal */
/* #define mpn_kara_sqr_n	__MPN(kara_sqr_n)  internal */
#define mpn_lshift		__MPN(lshift)
#define mpn_lshiftc		__MPN(lshiftc)
#define mpn_mod_1		__MPN(mod_1)
#define mpn_mul			__MPN(mul)
#define mpn_mul_1		__MPN(mul_1)
#define mpn_mul_basecase	__MPN(mul_basecase)
#define mpn_mul_n		__MPN(mul_n)
#define mpn_perfect_square_p	__MPN(perfect_square_p)
#define mpn_popcount		__MPN(popcount)
#define mpn_preinv_mod_1	__MPN(preinv_mod_1)
/* #define mpn_nand_n		__MPN(nand_n) */
/* #define mpn_nior_n		__MPN(nior_n) */
#define mpn_random		__MPN(random)
#define mpn_random2		__MPN(random2)
#define mpn_rshift		__MPN(rshift)
#define mpn_rshiftc		__MPN(rshiftc)
#define mpn_scan0		__MPN(scan0)
#define mpn_scan1		__MPN(scan1)
#define mpn_set_str		__MPN(set_str)
#define mpn_sqr_basecase	__MPN(sqr_basecase)
#define mpn_sqr_n		__MPN(sqr_n)
#define mpn_sqrtrem		__MPN(sqrtrem)
#define mpn_sub			__MPN(sub)
#define mpn_sub_1		__MPN(sub_1)
#define mpn_sub_n		__MPN(sub_n)
#define mpn_sub_nc		__MPN(sub_nc)
#define mpn_submul_1		__MPN(submul_1)
/* #define mpn_toom3_mul_n		__MPN(toom3_mul_n)  internal */
/* #define mpn_toom3_sqr_n		__MPN(toom3_sqr_n)  internal */
/* #define mpn_xnor_n		__MPN(xnor_n) */
/* #define mpn_xor_n		__MPN(xor_n) */

#if defined (__cplusplus)
extern "C" {
#endif
mp_limb_t mpn_add _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_srcptr,mp_size_t));
mp_limb_t mpn_add_1 _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t));
mp_limb_t mpn_add_n _PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t));
mp_limb_t mpn_add_nc _PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t, mp_limb_t));

mp_limb_t mpn_addmul_1 _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t));

#define mpn_addmul_1c  __MPN(addmul_1c)
mp_limb_t mpn_addmul_1c _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t));

mp_limb_t mpn_addsub_n _PROTO ((mp_ptr, mp_ptr, mp_srcptr, mp_srcptr, mp_size_t));
mp_limb_t mpn_bdivmod _PROTO ((mp_ptr, mp_ptr, mp_size_t, mp_srcptr, mp_size_t, unsigned long int));
int mpn_cmp _PROTO ((mp_srcptr, mp_srcptr, mp_size_t));

#define mpn_divexact_by3(dst, src, size)  mpn_divexact_by3c (dst, src, size, 0)

#define mpn_divexact_by3c  __MPN(divexact_by3c)
mp_limb_t mpn_divexact_by3c _PROTO ((mp_ptr dst, mp_srcptr src,
                                     mp_size_t size, mp_limb_t carry));

#define mpn_divmod_1(qp,np,nsize,dlimb) mpn_divrem_1 (qp,0,np,nsize,dlimb)

mp_limb_t mpn_divrem _PROTO((mp_ptr, mp_size_t, mp_ptr, mp_size_t, mp_srcptr, mp_size_t));

mp_limb_t mpn_divrem_1 _PROTO ((mp_ptr, mp_size_t, mp_srcptr, mp_size_t, mp_limb_t));

#define mpn_divrem_1c  __MPN(divrem_1c)
mp_limb_t mpn_divrem_1c _PROTO ((mp_ptr, mp_size_t, mp_srcptr, mp_size_t,
                                 mp_limb_t, mp_limb_t));

mp_limb_t mpn_divrem_2 _PROTO ((mp_ptr, mp_size_t, mp_ptr, mp_size_t, mp_srcptr));
void mpn_dump _PROTO ((mp_srcptr, mp_size_t));
mp_size_t mpn_gcd _PROTO ((mp_ptr, mp_ptr, mp_size_t, mp_ptr, mp_size_t));
mp_limb_t mpn_gcd_1 _PROTO ((mp_srcptr, mp_size_t, mp_limb_t));
mp_size_t mpn_gcdext _PROTO ((mp_ptr, mp_ptr, mp_size_t *, mp_ptr, mp_size_t, mp_ptr, mp_size_t));
size_t mpn_get_str _PROTO ((unsigned char *, int, mp_ptr, mp_size_t));
unsigned long int mpn_hamdist _PROTO ((mp_srcptr, mp_srcptr, mp_size_t));

#define mpn_jacobi_base __MPN(jacobi_base)
int mpn_jacobi_base _PROTO ((mp_limb_t a, mp_limb_t b, int result_bit1));

mp_limb_t mpn_lshift _PROTO ((mp_ptr, mp_srcptr, mp_size_t, unsigned int));
mp_limb_t mpn_mod_1 _PROTO ((mp_srcptr, mp_size_t, mp_limb_t));

#define mpn_mod_1c  __MPN(mod_1c)
mp_limb_t mpn_mod_1c _PROTO ((mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t));

#define mpn_mod_1_rshift __MPN(mod_1_rshift)
mp_limb_t mpn_mod_1_rshift _PROTO ((mp_srcptr, mp_size_t, unsigned,mp_limb_t));

mp_limb_t mpn_mul _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t));
mp_limb_t mpn_mul_1 _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t));

#define mpn_mul_1c  __MPN(mul_1c)
mp_limb_t mpn_mul_1c _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t));

void mpn_mul_basecase _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t));
void mpn_mul_n _PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t));
int mpn_perfect_square_p _PROTO ((mp_srcptr, mp_size_t));
unsigned long int mpn_popcount _PROTO ((mp_srcptr, mp_size_t));
mp_limb_t mpn_preinv_mod_1 _PROTO ((mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t));
void mpn_random _PROTO ((mp_ptr, mp_size_t));
void mpn_random2 _PROTO ((mp_ptr, mp_size_t));
mp_limb_t mpn_rshift _PROTO ((mp_ptr, mp_srcptr, mp_size_t, unsigned int));
unsigned long int mpn_scan0 _PROTO ((mp_srcptr, unsigned long int));
unsigned long int mpn_scan1 _PROTO ((mp_srcptr, unsigned long int));
mp_size_t mpn_set_str _PROTO ((mp_ptr, __gmp_const unsigned char *, size_t, int));
void mpn_sqr_n _PROTO ((mp_ptr, mp_srcptr, mp_size_t));
void mpn_sqr_basecase _PROTO ((mp_ptr, mp_srcptr, mp_size_t));
mp_size_t mpn_sqrtrem _PROTO ((mp_ptr, mp_ptr, mp_srcptr, mp_size_t));
mp_limb_t mpn_sub _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_srcptr,mp_size_t));
mp_limb_t mpn_sub_1 _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t));
mp_limb_t mpn_sub_n _PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t));
mp_limb_t mpn_sub_nc _PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t, mp_limb_t));
mp_limb_t mpn_submul_1 _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t));

#define mpn_submul_1c  __MPN(submul_1c)
mp_limb_t mpn_submul_1c _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t));

#define mpn_tdiv_qr  __MPN(tdiv_qr)
void mpn_tdiv_qr _PROTO ((mp_ptr, mp_ptr, mp_size_t, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t));

#if defined (__cplusplus)
}
#endif

#define mpn_incr_u(p,incr) \
  do { mp_limb_t __x; mp_ptr __p = p;			\
    __x = *__p + incr;					\
    *__p = __x;						\
    if (__x < incr)					\
      while (++(*(++__p)) == 0) {}		      	\
  } while (0)

#define mpn_decr_u(p,incr) \
  do { mp_limb_t __x; mp_ptr __p = p;			\
    __x = *__p;						\
    *__p = __x - incr;					\
    if (__x < incr)					\
      while ((*(++__p))-- == 0)	{}			\
  } while (0)

#if (defined (__GNUC__) || defined (_FORCE_INLINES)) && !defined(PALMOS_STUFF)
_EXTERN_INLINE mp_limb_t
#if (__STDC__-0) || defined (__cplusplus) || defined(__BORLANDC__)
mpn_add_1 (register mp_ptr res_ptr,
	   register mp_srcptr s1_ptr,
	   register mp_size_t s1_size,
	   register mp_limb_t s2_limb)
#else
mpn_add_1 (res_ptr, s1_ptr, s1_size, s2_limb)
     register mp_ptr res_ptr;
     register mp_srcptr s1_ptr;
     register mp_size_t s1_size;
     register mp_limb_t s2_limb;
#endif
{
  register mp_limb_t x;

  x = *s1_ptr++;
  s2_limb = x + s2_limb;
  *res_ptr++ = s2_limb;
  if (s2_limb < x)
    {
      while (--s1_size != 0)
	{
	  x = *s1_ptr++ + 1;
	  *res_ptr++ = x;
	  if (x != 0)
	    goto fin;
	}

      return 1;
    }

 fin:
  if (res_ptr != s1_ptr)
    {
      mp_size_t i;
      for (i = 0; i < s1_size - 1; i++) {
	res_ptr[i] = s1_ptr[i];
      }
    }
  return 0;
}

_EXTERN_INLINE mp_limb_t
#if (__STDC__-0) || defined (__cplusplus) || defined(__BORLANDC__)
mpn_add (register mp_ptr res_ptr,
	 register mp_srcptr s1_ptr,
	 register mp_size_t s1_size,
	 register mp_srcptr s2_ptr,
	 register mp_size_t s2_size)
#else
mpn_add (res_ptr, s1_ptr, s1_size, s2_ptr, s2_size)
     register mp_ptr res_ptr;
     register mp_srcptr s1_ptr;
     register mp_size_t s1_size;
     register mp_srcptr s2_ptr;
     register mp_size_t s2_size;
#endif
{
  mp_limb_t cy_limb = 0;

  if (s2_size != 0)
    cy_limb = mpn_add_n (res_ptr, s1_ptr, s2_ptr, s2_size);

  if (s1_size - s2_size != 0)
    cy_limb = mpn_add_1 (res_ptr + s2_size,
			 s1_ptr + s2_size,
			 s1_size - s2_size,
			 cy_limb);
  return cy_limb;
}

_EXTERN_INLINE mp_limb_t
#if (__STDC__-0) || defined (__cplusplus) || defined(__BORLANDC__)
mpn_sub_1 (register mp_ptr res_ptr,
	   register mp_srcptr s1_ptr,
	   register mp_size_t s1_size,
	   register mp_limb_t s2_limb)
#else
mpn_sub_1 (res_ptr, s1_ptr, s1_size, s2_limb)
     register mp_ptr res_ptr;
     register mp_srcptr s1_ptr;
     register mp_size_t s1_size;
     register mp_limb_t s2_limb;
#endif
{
  register mp_limb_t x;

  x = *s1_ptr++;
  s2_limb = x - s2_limb;
  *res_ptr++ = s2_limb;
  if (s2_limb > x)
    {
      while (--s1_size != 0)
	{
	  x = *s1_ptr++;
	  *res_ptr++ = x - 1;
	  if (x != 0)
	    goto fin;
	}

      return 1;
    }

 fin:
  if (res_ptr != s1_ptr)
    {
      mp_size_t i;
      for (i = 0; i < s1_size - 1; i++) {
	res_ptr[i] = s1_ptr[i];
      }
    }
  return 0;
}

_EXTERN_INLINE mp_limb_t
#if (__STDC__-0) || defined (__cplusplus) || defined(__BORLANDC__)
mpn_sub (register mp_ptr res_ptr,
	 register mp_srcptr s1_ptr,
	 register mp_size_t s1_size,
	 register mp_srcptr s2_ptr,
	 register mp_size_t s2_size)
#else
mpn_sub (res_ptr, s1_ptr, s1_size, s2_ptr, s2_size)
     register mp_ptr res_ptr;
     register mp_srcptr s1_ptr;
     register mp_size_t s1_size;
     register mp_srcptr s2_ptr;
     register mp_size_t s2_size;
#endif
{
  mp_limb_t cy_limb = 0;

  if (s2_size != 0)
    cy_limb = mpn_sub_n (res_ptr, s1_ptr, s2_ptr, s2_size);

  if (s1_size - s2_size != 0)
    cy_limb = mpn_sub_1 (res_ptr + s2_size,
			 s1_ptr + s2_size,
			 s1_size - s2_size,
			 cy_limb);
  return cy_limb;
}
#endif /* __GNUC__ */

/* Allow faster testing for negative, zero, and positive.  */
#define mpz_sgn(Z) ((Z)->_mp_size < 0 ? -1 : (Z)->_mp_size > 0)
#define mpf_sgn(F) ((F)->_mp_size < 0 ? -1 : (F)->_mp_size > 0)
#define mpq_sgn(Q) ((Q)->_mp_num._mp_size < 0 ? -1 : (Q)->_mp_num._mp_size > 0)

/* When using GCC, optimize certain common comparisons.  */
#if defined (__GNUC__)
#define mpz_cmp_ui(Z,UI) \
  (__builtin_constant_p (UI) && (UI) == 0				\
   ? mpz_sgn (Z) : _mpz_cmp_ui (Z,UI))
#define mpz_cmp_si(Z,SI) \
  (__builtin_constant_p (SI) && (SI) == 0 ? mpz_sgn (Z)			\
   : __builtin_constant_p (SI) && (SI) > 0				\
    ? _mpz_cmp_ui (Z, (unsigned long int) SI)				\
   : _mpz_cmp_si (Z,SI))
#define mpq_cmp_ui(Q,NUI,DUI) \
  (__builtin_constant_p (NUI) && (NUI) == 0				\
   ? mpq_sgn (Q) : _mpq_cmp_ui (Q,NUI,DUI))
#else
#define mpz_cmp_ui(Z,UI) _mpz_cmp_ui (Z,UI)
#define mpz_cmp_si(Z,UI) _mpz_cmp_si (Z,UI)
#define mpq_cmp_ui(Q,NUI,DUI) _mpq_cmp_ui (Q,NUI,DUI)
#endif


/* Using "&" rather than "&&" means these can come out branch-free.  Every
   mpz_t has at least one limb allocated, so fetching the low limb is always
   allowed.  */
#define mpz_odd_p(z)   ((int) ((z)->_mp_size != 0) & (int) (z)->_mp_d[0])
#define mpz_even_p(z)  (! mpz_odd_p (z))


/* Allow direct user access to numerator and denominator of a mpq_t object.  */
#define mpq_numref(Q) (&((Q)->_mp_num))
#define mpq_denref(Q) (&((Q)->_mp_den))


/* Compatibility with GMP 2 and earlier. */
#define mpn_divmod(qp,np,nsize,dp,dsize) mpn_divrem (qp,0,np,nsize,dp,dsize)

/* Compatibility with GMP 1.  */
#define mpz_mdiv	mpz_fdiv_q
#define mpz_mdivmod	mpz_fdiv_qr
#define mpz_mmod	mpz_fdiv_r
#define mpz_mdiv_ui	mpz_fdiv_q_ui
#define mpz_mdivmod_ui(q,r,n,d) \
  ((r == 0) ? mpz_fdiv_q_ui (q,n,d) : mpz_fdiv_qr_ui (q,r,n,d))
#define mpz_mmod_ui(r,n,d) \
  ((r == 0) ? mpz_fdiv_ui (n,d) : mpz_fdiv_r_ui (r,n,d))

/* Useful synonyms, but not quite compatible with GMP 1.  */
#define mpz_div		mpz_fdiv_q
#define mpz_divmod	mpz_fdiv_qr
#define mpz_div_ui	mpz_fdiv_q_ui
#define mpz_divmod_ui	mpz_fdiv_qr_ui
#define mpz_mod_ui	mpz_fdiv_r_ui
#define mpz_div_2exp	mpz_fdiv_q_2exp
#define mpz_mod_2exp	mpz_fdiv_r_2exp

#define gmp_errno __gmp_errno
extern int gmp_errno;

enum
{
  GMP_ERROR_NONE = 0,
  GMP_ERROR_UNSUPPORTED_ARGUMENT = 1,
  GMP_ERROR_DIVISION_BY_ZERO = 2,
  GMP_ERROR_SQRT_OF_NEGATIVE = 4,
  GMP_ERROR_INVALID_ARGUMENT = 8,
  GMP_ERROR_ALLOCATE = 16,
  GMP_ERROR_BAD_STRING = 32,
  GMP_ERROR_UNUSED_ERROR
};

/* Note: major version number is in mp.h too */
#define __GNU_MP_VERSION 3
#define __GNU_MP_VERSION_MINOR 1
#define __GNU_MP_VERSION_PATCHLEVEL 1

#define gmp_version __gmp_version
extern __gmp_const char *gmp_version;

#define __GMP_H__
#endif /* __GMP_H__ */
