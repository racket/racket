/* longlong.h -- definitions for mixed size 32/64 bit arithmetic.

Copyright 1991, 1992, 1993, 1994, 1996, 1997, 1999, 2000, 2001, 2002, 2003,
2004, 2005, 2007, 2008, 2009, 2011, 2012 Free Software Foundation, Inc.

This file is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this file; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA. */

/* You have to define the following before including this file:

   UWtype -- An unsigned type, default type for operations (typically a "word")
   UHWtype -- An unsigned type, at least half the size of UWtype.
   UDWtype -- An unsigned type, at least twice as large a UWtype
   W_TYPE_SIZE -- size in bits of UWtype

   SItype, USItype -- Signed and unsigned 32 bit types.
   DItype, UDItype -- Signed and unsigned 64 bit types.

   On a 32 bit machine UWtype should typically be USItype;
   on a 64 bit machine, UWtype should typically be UDItype.
*/

#define __BITS4 (W_TYPE_SIZE / 4)
#define __ll_B ((UWtype) 1 << (W_TYPE_SIZE / 2))
#define __ll_lowpart(t) ((UWtype) (t) & (__ll_B - 1))
#define __ll_highpart(t) ((UWtype) (t) >> (W_TYPE_SIZE / 2))

/* This is used to make sure no undesirable sharing between different libraries
   that use this file takes place.  */
#ifndef __MPN
#define __MPN(x) __##x
#endif

#ifndef _PROTO
#if (__STDC__-0) || defined (__cplusplus)
#define _PROTO(x) x
#else
#define _PROTO(x) ()
#endif
#endif

/* Define auxiliary asm macros.

   1) umul_ppmm(high_prod, low_prod, multipler, multiplicand) multiplies two
   UWtype integers MULTIPLER and MULTIPLICAND, and generates a two UWtype
   word product in HIGH_PROD and LOW_PROD.

   2) __umulsidi3(a,b) multiplies two UWtype integers A and B, and returns a
   UDWtype product.  This is just a variant of umul_ppmm.

   3) udiv_qrnnd(quotient, remainder, high_numerator, low_numerator,
   denominator) divides a UDWtype, composed by the UWtype integers
   HIGH_NUMERATOR and LOW_NUMERATOR, by DENOMINATOR and places the quotient
   in QUOTIENT and the remainder in REMAINDER.  HIGH_NUMERATOR must be less
   than DENOMINATOR for correct operation.  If, in addition, the most
   significant bit of DENOMINATOR must be 1, then the pre-processor symbol
   UDIV_NEEDS_NORMALIZATION is defined to 1.

   4) sdiv_qrnnd(quotient, remainder, high_numerator, low_numerator,
   denominator).  Like udiv_qrnnd but the numbers are signed.  The quotient
   is rounded towards 0.

   5) count_leading_zeros(count, x) counts the number of zero-bits from the
   msb to the first non-zero bit in the UWtype X.  This is the number of
   steps X needs to be shifted left to set the msb.  Undefined for X == 0,
   unless the symbol COUNT_LEADING_ZEROS_0 is defined to some value.

   6) count_trailing_zeros(count, x) like count_leading_zeros, but counts
   from the least significant end.

   7) add_ssaaaa(high_sum, low_sum, high_addend_1, low_addend_1,
   high_addend_2, low_addend_2) adds two UWtype integers, composed by
   HIGH_ADDEND_1 and LOW_ADDEND_1, and HIGH_ADDEND_2 and LOW_ADDEND_2
   respectively.  The result is placed in HIGH_SUM and LOW_SUM.  Overflow
   (i.e. carry out) is not stored anywhere, and is lost.

   8) sub_ddmmss(high_difference, low_difference, high_minuend, low_minuend,
   high_subtrahend, low_subtrahend) subtracts two two-word UWtype integers,
   composed by HIGH_MINUEND_1 and LOW_MINUEND_1, and HIGH_SUBTRAHEND_2 and
   LOW_SUBTRAHEND_2 respectively.  The result is placed in HIGH_DIFFERENCE
   and LOW_DIFFERENCE.  Overflow (i.e. carry out) is not stored anywhere,
   and is lost.

   If any of these macros are left undefined for a particular CPU,
   C macros are used.  */

#ifndef MZ_GMP_NO_ASM

/* The CPUs come in alphabetical order below.

   Please add support for more CPUs here, or improve the current support
   for the CPUs below!  */

#if defined (__alpha) && W_TYPE_SIZE == 64
#if defined (__GNUC__)
#define umul_ppmm(ph, pl, m0, m1) \
  do {									\
    UDItype __m0 = (m0), __m1 = (m1);					\
    __asm__ ("umulh %r1,%2,%0"						\
	     : "=r" (ph)						\
	     : "%rJ" (m0), "rI" (m1));					\
    (pl) = __m0 * __m1;							\
  } while (0)
#define UMUL_TIME 18
#ifndef LONGLONG_STANDALONE
#define udiv_qrnnd(q, r, n1, n0, d) \
  do { UDItype __di;							\
    __di = __MPN(invert_limb) (d);					\
    udiv_qrnnd_preinv (q, r, n1, n0, d, __di);				\
  } while (0)
#define UDIV_NEEDS_NORMALIZATION 1
#define UDIV_TIME 220
long __MPN(count_leading_zeros) ();
#define count_leading_zeros(count, x) \
  ((count) = __MPN(count_leading_zeros) (x))
#endif /* LONGLONG_STANDALONE */
#else /* ! __GNUC__ */
#include <machine/builtins.h>
#define umul_ppmm(ph, pl, m0, m1) \
  do {									\
    UDItype __m0 = (m0), __m1 = (m1);					\
    (ph) = __UMULH (m0, m1);						\
    (pl) = __m0 * __m1;							\
  } while (0)
#endif
#endif /* __alpha */

#if defined (__hppa) && W_TYPE_SIZE == 64
/* We put the result pointer parameter last here, since it makes passing
   of the other parameters more efficient.  */
#define LONGLONG_STANDALONE /* <----------- PLTSCHEME: Avoid extern */
#ifndef LONGLONG_STANDALONE
#define umul_ppmm(wh, wl, u, v) \
  do {									\
    UDItype __p0;							\
    (wh) = __MPN(umul_ppmm) (u, v, &__p0);				\
    (wl) = __p0;							\
  } while (0)
extern UDItype __MPN(umul_ppmm) _PROTO ((UDItype, UDItype, UDItype *));
#define udiv_qrnnd(q, r, n1, n0, d) \
  do { UDItype __r;							\
    (q) = __MPN(udiv_qrnnd) (n1, n0, d, &__r);				\
    (r) = __r;								\
  } while (0)
extern UDItype __MPN(udiv_qrnnd) _PROTO ((UDItype, UDItype, UDItype, UDItype *));
#define UMUL_TIME 8
#define UDIV_TIME 60
#endif /* LONGLONG_STANDALONE */
#endif /* hppa */

/* PLTSCHEME: doesn't seem to work, so disabled: */
#if defined (__ia64) && W_TYPE_SIZE == 64 && 0
#if defined (__GNUC__)
#define umul_ppmm(ph, pl, m0, m1) \
  do {									\
    UDItype __m0 = (m0), __m1 = (m1);					\
    __asm__ ("xma.hu %0 = %1, %2, f0"					\
	     : "=e" (ph)						\
	     : "e" (m0), "e" (m1));					\
    (pl) = __m0 * __m1;							\
  } while (0)
#endif
#endif


#if defined (__GNUC__) && !defined (NO_ASM)

/* We sometimes need to clobber "cc" with gcc2, but that would not be
   understood by gcc1.  Use cpp to avoid major code duplication.  */
#if __GNUC__ < 2
#define __CLOBBER_CC
#define __AND_CLOBBER_CC
#else /* __GNUC__ >= 2 */
#define __CLOBBER_CC : "cc"
#define __AND_CLOBBER_CC , "cc"
#endif /* __GNUC__ < 2 */

#if (defined (__a29k__) || defined (_AM29K)) && W_TYPE_SIZE == 32
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("add %1,%4,%5\n\taddc %0,%2,%3"				\
	   : "=r" (sh), "=&r" (sl)					\
	   : "%r" (ah), "rI" (bh), "%r" (al), "rI" (bl))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("sub %1,%4,%5\n\tsubc %0,%2,%3"				\
	   : "=r" (sh),	"=&r" (sl)					\
	   : "r" (ah), "rI" (bh), "r" (al), "rI" (bl))
#define umul_ppmm(xh, xl, m0, m1) \
  do {									\
    USItype __m0 = (m0), __m1 = (m1);					\
    __asm__ ("multiplu %0,%1,%2"					\
	     : "=r" (xl)						\
	     : "r" (__m0), "r" (__m1));					\
    __asm__ ("multmu %0,%1,%2"						\
	     : "=r" (xh)						\
	     : "r" (__m0), "r" (__m1));					\
  } while (0)
#define udiv_qrnnd(q, r, n1, n0, d) \
  __asm__ ("dividu %0,%3,%4"						\
	   : "=r" (q), "=q" (r)						\
	   : "1" (n1), "r" (n0), "r" (d))
#define count_leading_zeros(count, x) \
    __asm__ ("clz %0,%1"						\
	     : "=r" (count)						\
	     : "r" (x))
#define COUNT_LEADING_ZEROS_0 32
#endif /* __a29k__ */

#if defined (__arm__) && W_TYPE_SIZE == 32
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("adds\t%1, %4, %5\n\tadc\t%0, %2, %3"			\
	   : "=r" (sh), "=&r" (sl)					\
	   : "%r" (ah), "rI" (bh), "%r" (al), "rI" (bl))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("subs\t%1, %4, %5\n\tsbc\t%0, %2, %3"			\
	   : "=r" (sh),	"=&r" (sl)					\
	   : "r" (ah), "rI" (bh), "r" (al), "rI" (bl))
#if 1 || defined (__arm_m__)		/* `M' series has widening multiply support */
#define umul_ppmm(xh, xl, a, b) \
  __asm__ ("umull %0,%1,%2,%3" : "=&r" (xl), "=&r" (xh) : "r" (a), "r" (b))
#define smul_ppmm(xh, xl, a, b) \
  __asm__ ("smull %0,%1,%2,%3" : "=&r" (xl), "=&r" (xh) : "r" (a), "r" (b))
#define UMUL_TIME 5
#else
#define umul_ppmm(xh, xl, a, b) \
  __asm__ ("%@ Inlined umul_ppmm\n"					\
"	mov	%|r0, %2, lsr #16\n"					\
"	mov	%|r2, %3, lsr #16\n"					\
"	bic	%|r1, %2, %|r0, lsl #16\n"				\
"	bic	%|r2, %3, %|r2, lsl #16\n"				\
"	mul	%1, %|r1, %|r2\n"					\
"	mul	%|r2, %|r0, %|r2\n"					\
"	mul	%|r1, %0, %|r1\n"					\
"	mul	%0, %|r0, %0\n"						\
"	adds	%|r1, %|r2, %|r1\n"					\
"	addcs	%0, %0, #65536\n"					\
"	adds	%1, %1, %|r1, lsl #16\n"				\
"	adc	%0, %0, %|r1, lsr #16"					\
	   : "=&r" (xh), "=r" (xl)					\
	   : "r" (a), "r" (b)						\
	   : "r0", "r1", "r2")
#define UMUL_TIME 20
#endif
#define UDIV_TIME 100
#endif /* __arm__ */

#if defined (__clipper__) && W_TYPE_SIZE == 32
#define umul_ppmm(w1, w0, u, v) \
  ({union {UDItype __ll;						\
	   struct {USItype __l, __h;} __i;				\
	  } __x;							\
  __asm__ ("mulwux %2,%0"						\
	   : "=r" (__x.__ll)						\
	   : "%0" ((USItype)(u)), "r" ((USItype)(v)));			\
  (w1) = __x.__i.__h; (w0) = __x.__i.__l;})
#define smul_ppmm(w1, w0, u, v) \
  ({union {DItype __ll;							\
	   struct {SItype __l, __h;} __i;				\
	  } __x;							\
  __asm__ ("mulwx %2,%0"						\
	   : "=r" (__x.__ll)						\
	   : "%0" ((SItype)(u)), "r" ((SItype)(v)));			\
  (w1) = __x.__i.__h; (w0) = __x.__i.__l;})
#define __umulsidi3(u, v) \
  ({UDItype __w;							\
    __asm__ ("mulwux %2,%0"						\
	     : "=r" (__w) : "%0" ((USItype)(u)), "r" ((USItype)(v)));	\
    __w; })
#endif /* __clipper__ */

/* Fujitsu vector computers.  */
#if defined (__uxp__) && W_TYPE_SIZE == 32
#define umul_ppmm(ph, pl, u, v) \
  do {									\
    union {UDItype __ll;						\
	   struct {USItype __h, __l;} __i;				\
	  } __x;							\
    __asm__ ("mult.lu %1,%2,%0"	: "=r" (__x.__ll) : "%r" (u), "rK" (v));\
    (ph) = __x.__i.__h;							\
    (pl) = __x.__i.__l;							\
  } while (0)
#define smul_ppmm(ph, pl, u, v) \
  do {									\
    union {UDItype __ll;						\
	   struct {USItype __h, __l;} __i;				\
	  } __x;							\
    __asm__ ("mult.l %1,%2,%0" : "=r" (__x.__ll) : "%r" (u), "rK" (v));	\
    (ph) = __x.__i.__h;							\
    (pl) = __x.__i.__l;							\
  } while (0)
#endif

#if defined (__gmicro__) && W_TYPE_SIZE == 32
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("add.w %5,%1\n\taddx %3,%0"					\
	   : "=g" ((USItype)(sh)), "=&g" ((USItype)(sl))		\
	   : "%0" ((USItype)(ah)), "g" ((USItype)(bh)),			\
	     "%1" ((USItype)(al)), "g" ((USItype)(bl)))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("sub.w %5,%1\n\tsubx %3,%0"					\
	   : "=g" ((USItype)(sh)), "=&g" ((USItype)(sl))		\
	   : "0" ((USItype)(ah)), "g" ((USItype)(bh)),			\
	     "1" ((USItype)(al)), "g" ((USItype)(bl)))
#define umul_ppmm(ph, pl, m0, m1) \
  __asm__ ("mulx %3,%0,%1"						\
	   : "=g" ((USItype)(ph)), "=r" ((USItype)(pl))			\
	   : "%0" ((USItype)(m0)), "g" ((USItype)(m1)))
#define udiv_qrnnd(q, r, nh, nl, d) \
  __asm__ ("divx %4,%0,%1"						\
	   : "=g" ((USItype)(q)), "=r" ((USItype)(r))			\
	   : "1" ((USItype)(nh)), "0" ((USItype)(nl)), "g" ((USItype)(d)))
#define count_leading_zeros(count, x) \
  __asm__ ("bsch/1 %1,%0"						\
	   : "=g" (count) : "g" ((USItype)(x)), "0" ((USItype)0))
#endif

#if defined (__hppa) && W_TYPE_SIZE == 32
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("add %4,%5,%1\n\taddc %2,%3,%0"				\
	   : "=r" (sh), "=&r" (sl)					\
	   : "%rM" (ah), "rM" (bh), "%rM" (al), "rM" (bl))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("sub %4,%5,%1\n\tsubb %2,%3,%0"				\
	   : "=r" (sh), "=&r" (sl)					\
	   : "rM" (ah), "rM" (bh), "rM" (al), "rM" (bl))
#if defined (_PA_RISC1_1)
#define umul_ppmm(wh, wl, u, v) \
  do {									\
    union {UDItype __ll;						\
	   struct {USItype __h, __l;} __i;				\
	  } __x;							\
    __asm__ ("xmpyu %1,%2,%0" : "=*f" (__x.__ll) : "*f" (u), "*f" (v));	\
    (wh) = __x.__i.__h;							\
    (wl) = __x.__i.__l;							\
  } while (0)
#define UMUL_TIME 8
#define UDIV_TIME 60
#else
#define UMUL_TIME 40
#define UDIV_TIME 80
#endif
#define LONGLONG_STANDALONE /* <----------- PLTSCHEME: Avoid extern */
#ifndef LONGLONG_STANDALONE
#define udiv_qrnnd(q, r, n1, n0, d) \
  do { USItype __r;							\
    (q) = __MPN(udiv_qrnnd) (&__r, (n1), (n0), (d));			\
    (r) = __r;								\
  } while (0)
extern USItype __MPN(udiv_qrnnd) _PROTO ((USItype *, USItype, USItype, USItype));
#endif /* LONGLONG_STANDALONE */
#define count_leading_zeros(count, x) \
  do {									\
    USItype __tmp;							\
    __asm__ (								\
       "ldi		1,%0\n"						\
"	extru,=		%1,15,16,%%r0	; Bits 31..16 zero?\n"		\
"	extru,tr	%1,15,16,%1	; No.  Shift down, skip add.\n"	\
"	ldo		16(%0),%0	; Yes.  Perform add.\n"		\
"	extru,=		%1,23,8,%%r0	; Bits 15..8 zero?\n"		\
"	extru,tr	%1,23,8,%1	; No.  Shift down, skip add.\n"	\
"	ldo		8(%0),%0	; Yes.  Perform add.\n"		\
"	extru,=		%1,27,4,%%r0	; Bits 7..4 zero?\n"		\
"	extru,tr	%1,27,4,%1	; No.  Shift down, skip add.\n"	\
"	ldo		4(%0),%0	; Yes.  Perform add.\n"		\
"	extru,=		%1,29,2,%%r0	; Bits 3..2 zero?\n"		\
"	extru,tr	%1,29,2,%1	; No.  Shift down, skip add.\n"	\
"	ldo		2(%0),%0	; Yes.  Perform add.\n"		\
"	extru		%1,30,1,%1	; Extract bit 1.\n"		\
"	sub		%0,%1,%0	; Subtract it.\n"		\
	: "=r" (count), "=r" (__tmp) : "1" (x));			\
  } while (0)
#endif /* hppa */

#if (defined (__i370__) || defined (__mvs__)) && W_TYPE_SIZE == 32
#define smul_ppmm(xh, xl, m0, m1) \
  do {									\
    union {DItype __ll;							\
	   struct {USItype __h, __l;} __i;				\
	  } __x;							\
    __asm__ ("mr %0,%3"							\
	     : "=r" (__x.__i.__h), "=r" (__x.__i.__l)			\
	     : "%1" (m0), "r" (m1));					\
    (xh) = __x.__i.__h; (xl) = __x.__i.__l;				\
  } while (0)
#define sdiv_qrnnd(q, r, n1, n0, d) \
  do {									\
    union {DItype __ll;							\
	   struct {USItype __h, __l;} __i;				\
	  } __x;							\
    __x.__i.__h = n1; __x.__i.__l = n0;					\
    __asm__ ("dr %0,%2"							\
	     : "=r" (__x.__ll)						\
	     : "0" (__x.__ll), "r" (d));				\
    (q) = __x.__i.__l; (r) = __x.__i.__h;				\
  } while (0)
#endif

#if (defined (__i386__) || defined (__i486__)) && W_TYPE_SIZE == 32
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("addl %5,%k1\n\tadcl %3,%k0"					\
	   : "=r" (sh), "=&r" (sl)					\
	   : "0"  ((USItype)(ah)), "g" ((USItype)(bh)),			\
	     "%1" ((USItype)(al)), "g" ((USItype)(bl)))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("subl %5,%k1\n\tsbbl %3,%k0"					\
	   : "=r" (sh), "=&r" (sl)					\
	   : "0" ((USItype)(ah)), "g" ((USItype)(bh)),			\
	     "1" ((USItype)(al)), "g" ((USItype)(bl)))
#define umul_ppmm(w1, w0, u, v) \
  __asm__ ("mull %3"							\
	   : "=a" (w0), "=d" (w1)					\
	   : "%0" ((USItype)(u)), "rm" ((USItype)(v)))
#define udiv_qrnnd(q, r, n1, n0, dx) /* d renamed to dx avoiding "=d" */\
  __asm__ ("divl %4"		     /* stringification in K&R C */	\
	   : "=a" (q), "=d" (r)						\
	   : "0" ((USItype)(n0)), "1" ((USItype)(n1)), "rm" ((USItype)(dx)))
#define count_leading_zeros(count, x) \
  do {									\
    USItype __cbtmp;							\
    __asm__ ("bsrl %1,%0" : "=r" (__cbtmp) : "rm" ((USItype)(x)));	\
    (count) = __cbtmp ^ 31;						\
  } while (0)
#define count_trailing_zeros(count, x) \
  __asm__ ("bsfl %1,%0" : "=r" (count) : "rm" ((USItype)(x)))
#ifndef UMUL_TIME
#define UMUL_TIME 10
#endif
#ifndef UDIV_TIME
#define UDIV_TIME 40
#endif
#endif /* 80x86 */

#if defined (__amd64__) && W_TYPE_SIZE == 64
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("addq %5,%q1\n\tadcq %3,%q0"					\
	   : "=r" (sh), "=&r" (sl)					\
	   : "0"  ((UDItype)(ah)), "rme" ((UDItype)(bh)),		\
	     "%1" ((UDItype)(al)), "rme" ((UDItype)(bl)))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("subq %5,%q1\n\tsbbq %3,%q0"					\
	   : "=r" (sh), "=&r" (sl)					\
	   : "0" ((UDItype)(ah)), "rme" ((UDItype)(bh)),		\
	     "1" ((UDItype)(al)), "rme" ((UDItype)(bl)))
#define umul_ppmm(w1, w0, u, v) \
  __asm__ ("mulq %3"							\
	   : "=a" (w0), "=d" (w1)					\
	   : "%0" ((UDItype)(u)), "rm" ((UDItype)(v)))
#define udiv_qrnnd(q, r, n1, n0, dx) /* d renamed to dx avoiding "=d" */\
  __asm__ ("divq %4"		     /* stringification in K&R C */	\
	   : "=a" (q), "=d" (r)						\
	   : "0" ((UDItype)(n0)), "1" ((UDItype)(n1)), "rm" ((UDItype)(dx)))
/* bsrq destination must be a 64-bit register, hence UDItype for __cbtmp. */
#define count_leading_zeros(count, x)					\
  do {									\
    UDItype __cbtmp;							\
    ASSERT ((x) != 0);							\
    __asm__ ("bsrq %1,%0" : "=r" (__cbtmp) : "rm" ((UDItype)(x)));	\
    (count) = __cbtmp ^ 63;						\
  } while (0)
/* bsfq destination must be a 64-bit register, "%q0" forces this in case
   count is only an int. */
#define count_trailing_zeros(count, x)					\
  do {									\
    ASSERT ((x) != 0);							\
    __asm__ ("bsfq %1,%q0" : "=r" (count) : "rm" ((UDItype)(x)));	\
  } while (0)
#endif /* x86_64 */

#if defined (__i860__) && W_TYPE_SIZE == 32
#define rshift_rhlc(r,h,l,c) \
  __asm__ ("shr %3,r0,r0\;shrd %1,%2,%0"				\
	   "=r" (r) : "r" (h), "r" (l), "rn" (c))
#endif /* i860 */

#if defined (__i960__) && W_TYPE_SIZE == 32
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("cmpo 1,0\;addc %5,%4,%1\;addc %3,%2,%0"			\
	   : "=r" (sh), "=&r" (sl)					\
	   : "%dI" (ah), "dI" (bh), "%dI" (al), "dI" (bl))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("cmpo 0,0\;subc %5,%4,%1\;subc %3,%2,%0"			\
	   : "=r" (sh), "=&r" (sl)					\
	   : "dI" (ah), "dI" (bh), "dI" (al), "dI" (bl))
#define umul_ppmm(w1, w0, u, v) \
  ({union {UDItype __ll;						\
	   struct {USItype __l, __h;} __i;				\
	  } __x;							\
  __asm__ ("emul %2,%1,%0"						\
	   : "=d" (__x.__ll) : "%dI" (u), "dI" (v));			\
  (w1) = __x.__i.__h; (w0) = __x.__i.__l;})
#define __umulsidi3(u, v) \
  ({UDItype __w;							\
    __asm__ ("emul %2,%1,%0" : "=d" (__w) : "%dI" (u), "dI" (v));	\
    __w; })
#define udiv_qrnnd(q, r, nh, nl, d) \
  do {									\
    union {UDItype __ll;						\
	   struct {USItype __l, __h;} __i;				\
	  } __nn;							\
    __nn.__i.__h = (nh); __nn.__i.__l = (nl);				\
    __asm__ ("ediv %d,%n,%0"						\
	   : "=d" (__rq.__ll) : "dI" (__nn.__ll), "dI" (d));		\
    (r) = __rq.__i.__l; (q) = __rq.__i.__h;				\
  } while (0)
#define count_leading_zeros(count, x) \
  do {									\
    USItype __cbtmp;							\
    __asm__ ("scanbit %1,%0" : "=r" (__cbtmp) : "r" (x));		\
    (count) = __cbtmp ^ 31;						\
  } while (0)
#define COUNT_LEADING_ZEROS_0 (-32) /* sic */
#if defined (__i960mx)		/* what is the proper symbol to test??? */
#define rshift_rhlc(r,h,l,c) \
  do {									\
    union {UDItype __ll;						\
	   struct {USItype __l, __h;} __i;				\
	  } __nn;							\
    __nn.__i.__h = (h); __nn.__i.__l = (l);				\
    __asm__ ("shre %2,%1,%0" : "=d" (r) : "dI" (__nn.__ll), "dI" (c));	\
  }
#endif /* i960mx */
#endif /* i960 */

#if (defined (__mc68000__) || defined (__mc68020__) || defined(mc68020) \
     || defined (__m68k__) || defined (__mc5200__) || defined (__mc5206e__) \
     || defined (__mc5307__)) && W_TYPE_SIZE == 32
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("add%.l %5,%1\n\taddx%.l %3,%0"				\
	   : "=d" ((USItype)(sh)), "=&d" ((USItype)(sl))		\
	   : "%0" ((USItype)(ah)), "d" ((USItype)(bh)),			\
	     "%1" ((USItype)(al)), "g" ((USItype)(bl)))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("sub%.l %5,%1\n\tsubx%.l %3,%0"				\
	   : "=d" ((USItype)(sh)), "=&d" ((USItype)(sl))		\
	   : "0" ((USItype)(ah)), "d" ((USItype)(bh)),			\
	     "1" ((USItype)(al)), "g" ((USItype)(bl)))
/* The '020, '030, '040 and CPU32 have 32x32->64 and 64/32->32q-32r.  */
#if defined (__mc68020__) || defined(mc68020) \
     || defined (__mc68030__) || defined (mc68030) \
     || defined (__mc68040__) || defined (mc68040) \
     || defined (__mc68332__) || defined (mc68332) \
     || defined (__NeXT__)
#define umul_ppmm(w1, w0, u, v) \
  __asm__ ("mulu%.l %3,%1:%0"						\
	   : "=d" ((USItype)(w0)), "=d" ((USItype)(w1))			\
	   : "%0" ((USItype)(u)), "dmi" ((USItype)(v)))
#define UMUL_TIME 45
#define udiv_qrnnd(q, r, n1, n0, d) \
  __asm__ ("divu%.l %4,%1:%0"						\
	   : "=d" ((USItype)(q)), "=d" ((USItype)(r))			\
	   : "0" ((USItype)(n0)), "1" ((USItype)(n1)), "dmi" ((USItype)(d)))
#define UDIV_TIME 90
#define sdiv_qrnnd(q, r, n1, n0, d) \
  __asm__ ("divs%.l %4,%1:%0"						\
	   : "=d" ((USItype)(q)), "=d" ((USItype)(r))			\
	   : "0" ((USItype)(n0)), "1" ((USItype)(n1)), "dmi" ((USItype)(d)))
#else /* for other 68k family members use 16x16->32 multiplication */
#define umul_ppmm(xh, xl, a, b) \
  do { USItype __umul_tmp1, __umul_tmp2;				\
	__asm__ ("| Inlined umul_ppmm\n"				\
"	move%.l	%5,%3\n"						\
"	move%.l	%2,%0\n"						\
"	move%.w	%3,%1\n"						\
"	swap	%3\n"							\
"	swap	%0\n"							\
"	mulu%.w	%2,%1\n"						\
"	mulu%.w	%3,%0\n"						\
"	mulu%.w	%2,%3\n"						\
"	swap	%2\n"							\
"	mulu%.w	%5,%2\n"						\
"	add%.l	%3,%2\n"						\
"	jcc	1f\n"							\
"	add%.l	%#0x10000,%0\n"						\
"1:	move%.l	%2,%3\n"						\
"	clr%.w	%2\n"							\
"	swap	%2\n"							\
"	swap	%3\n"							\
"	clr%.w	%3\n"							\
"	add%.l	%3,%1\n"						\
"	addx%.l	%2,%0\n"						\
"	| End inlined umul_ppmm"					\
	      : "=&d" ((USItype)(xh)), "=&d" ((USItype)(xl)),		\
		"=d" (__umul_tmp1), "=&d" (__umul_tmp2)			\
	      : "%2" ((USItype)(a)), "d" ((USItype)(b)));		\
  } while (0)
#define UMUL_TIME 100
#define UDIV_TIME 400
#endif /* not mc68020 */
/* The '020, '030, '040 and '060 have bitfield insns.  */
#if defined (__mc68020__) || defined (mc68020) \
     || defined (__mc68030__) || defined (mc68030) \
     || defined (__mc68040__) || defined (mc68040) \
     || defined (__mc68060__) || defined (mc68060) \
     || defined (__NeXT__)
#define count_leading_zeros(count, x) \
  __asm__ ("bfffo %1{%b2:%b2},%0"					\
	   : "=d" ((USItype) (count))					\
	   : "od" ((USItype) (x)), "n" (0))
#define COUNT_LEADING_ZEROS_0 32
#endif
#endif /* mc68000 */

#if defined (__m88000__) && W_TYPE_SIZE == 32
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("addu.co %1,%r4,%r5\n\taddu.ci %0,%r2,%r3"			\
	   : "=r" (sh), "=&r" (sl)					\
	   : "%rJ" (ah), "rJ" (bh), "%rJ" (al), "rJ" (bl))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("subu.co %1,%r4,%r5\n\tsubu.ci %0,%r2,%r3"			\
	   : "=r" (sh), "=&r" (sl)					\
	   : "rJ" (ah), "rJ" (bh), "rJ" (al), "rJ" (bl))
#define count_leading_zeros(count, x) \
  do {									\
    USItype __cbtmp;							\
    __asm__ ("ff1 %0,%1" : "=r" (__cbtmp) : "r" (x));			\
    (count) = __cbtmp ^ 31;						\
  } while (0)
#define COUNT_LEADING_ZEROS_0 63 /* sic */
#if defined (__m88110__)
#define umul_ppmm(wh, wl, u, v) \
  do {									\
    union {UDItype __ll;						\
	   struct {USItype __h, __l;} __i;				\
	  } __x;							\
    __asm__ ("mulu.d %0,%1,%2" : "=r" (__x.__ll) : "r" (u), "r" (v));	\
    (wh) = __x.__i.__h;							\
    (wl) = __x.__i.__l;							\
  } while (0)
#define udiv_qrnnd(q, r, n1, n0, d) \
  ({union {UDItype __ll;						\
	   struct {USItype __h, __l;} __i;				\
	  } __x, __q;							\
  __x.__i.__h = (n1); __x.__i.__l = (n0);				\
  __asm__ ("divu.d %0,%1,%2"						\
	   : "=r" (__q.__ll) : "r" (__x.__ll), "r" (d));		\
  (r) = (n0) - __q.__l * (d); (q) = __q.__l; })
#define UMUL_TIME 5
#define UDIV_TIME 25
#else
#define UMUL_TIME 17
#define UDIV_TIME 150
#endif /* __m88110__ */
#endif /* __m88000__ */

#if defined (__mips) && W_TYPE_SIZE == 32
#if __GMP_GNUC_PREREQ (4,4)
#define umul_ppmm(w1, w0, u, v) \
  do {                                                                  \
    UDItype __ll = (UDItype)(u) * (v);                                  \
    w1 = __ll >> 32;                                                    \
    w0 = __ll;                                                          \
  } while (0)
#endif
#if !defined (umul_ppmm) && __GMP_GNUC_PREREQ (2,7)
#define umul_ppmm(w1, w0, u, v) \
  __asm__ ("multu %2,%3" : "=l" (w0), "=h" (w1) : "d" (u), "d" (v))
#endif
#if !defined (umul_ppmm)
#define umul_ppmm(w1, w0, u, v) \
  __asm__ ("multu %2,%3\n\tmflo %0\n\tmfhi %1"                          \
           : "=d" (w0), "=d" (w1) : "d" (u), "d" (v))
#endif
#define UMUL_TIME 10
#define UDIV_TIME 100
#endif /* __mips */

#if (defined (__mips) && __mips >= 3) && W_TYPE_SIZE == 64
#if __GMP_GNUC_PREREQ (4,4)
#define umul_ppmm(w1, w0, u, v) \
  do {                                                                  \
    typedef unsigned int __ll_UTItype __attribute__((mode(TI)));        \
    __ll_UTItype __ll = (__ll_UTItype)(u) * (v);                        \
    w1 = __ll >> 64;                                                    \
    w0 = __ll;                                                          \
  } while (0)
#endif
#if !defined (umul_ppmm) && __GMP_GNUC_PREREQ (2,7)
#define umul_ppmm(w1, w0, u, v) \
  __asm__ ("dmultu %2,%3" : "=l" (w0), "=h" (w1) : "d" (u), "d" (v))
#endif
#if !defined (umul_ppmm)
#define umul_ppmm(w1, w0, u, v) \
  __asm__ ("dmultu %2,%3\n\tmflo %0\n\tmfhi %1"                         \
           : "=d" (w0), "=d" (w1) : "d" (u), "d" (v))
#endif
#define UMUL_TIME 20
#define UDIV_TIME 140
#endif /* __mips */

#if defined (__ns32000__) && W_TYPE_SIZE == 32
#define umul_ppmm(w1, w0, u, v) \
  ({union {UDItype __ll;						\
	   struct {USItype __l, __h;} __i;				\
	  } __x;							\
  __asm__ ("meid %2,%0"							\
	   : "=g" (__x.__ll)						\
	   : "%0" ((USItype)(u)), "g" ((USItype)(v)));			\
  (w1) = __x.__i.__h; (w0) = __x.__i.__l;})
#define __umulsidi3(u, v) \
  ({UDItype __w;							\
    __asm__ ("meid %2,%0"						\
	     : "=g" (__w)						\
	     : "%0" ((USItype)(u)), "g" ((USItype)(v)));		\
    __w; })
#define udiv_qrnnd(q, r, n1, n0, d) \
  ({union {UDItype __ll;						\
	   struct {USItype __l, __h;} __i;				\
	  } __x;							\
  __x.__i.__h = (n1); __x.__i.__l = (n0);				\
  __asm__ ("deid %2,%0"							\
	   : "=g" (__x.__ll)						\
	   : "0" (__x.__ll), "g" ((USItype)(d)));			\
  (r) = __x.__i.__l; (q) = __x.__i.__h; })
#define count_trailing_zeros(count,x) \
  do {									\
    __asm__ ("ffsd	%2,%0"						\
	     : "=r" ((USItype) (count))					\
	     : "0" ((USItype) 0), "r" ((USItype) (x)));			\
  } while (0)
#endif /* __ns32000__ */

/* We should test _IBMR2 here when we add assembly support for the system
   vendor compilers.  */
#if (defined (_ARCH_PPC) || defined (_ARCH_PWR) || defined (__powerpc__)) && W_TYPE_SIZE == 32
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  do {									\
    if (__builtin_constant_p (bh) && (bh) == 0)				\
      __asm__ ("{a%I4|add%I4c} %1,%3,%4\n\t{aze|addze} %0,%2"		\
	     : "=r" (sh), "=&r" (sl) : "%r" (ah), "%r" (al), "rI" (bl));\
    else if (__builtin_constant_p (bh) && (bh) == ~(USItype) 0)		\
      __asm__ ("{a%I4|add%I4c} %1,%3,%4\n\t{ame|addme} %0,%2"		\
	     : "=r" (sh), "=&r" (sl) : "%r" (ah), "%r" (al), "rI" (bl));\
    else								\
      __asm__ ("{a%I5|add%I5c} %1,%4,%5\n\t{ae|adde} %0,%2,%3"		\
	     : "=r" (sh), "=&r" (sl)					\
	     : "%r" (ah), "r" (bh), "%r" (al), "rI" (bl));		\
  } while (0)
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  do {									\
    if (__builtin_constant_p (ah) && (ah) == 0)				\
      __asm__ ("{sf%I3|subf%I3c} %1,%4,%3\n\t{sfze|subfze} %0,%2"	\
	       : "=r" (sh), "=&r" (sl) : "r" (bh), "rI" (al), "r" (bl));\
    else if (__builtin_constant_p (ah) && (ah) == ~(USItype) 0)		\
      __asm__ ("{sf%I3|subf%I3c} %1,%4,%3\n\t{sfme|subfme} %0,%2"	\
	       : "=r" (sh), "=&r" (sl) : "r" (bh), "rI" (al), "r" (bl));\
    else if (__builtin_constant_p (bh) && (bh) == 0)			\
      __asm__ ("{sf%I3|subf%I3c} %1,%4,%3\n\t{ame|addme} %0,%2"		\
	       : "=r" (sh), "=&r" (sl) : "r" (ah), "rI" (al), "r" (bl));\
    else if (__builtin_constant_p (bh) && (bh) == ~(USItype) 0)		\
      __asm__ ("{sf%I3|subf%I3c} %1,%4,%3\n\t{aze|addze} %0,%2"		\
	       : "=r" (sh), "=&r" (sl) : "r" (ah), "rI" (al), "r" (bl));\
    else								\
      __asm__ ("{sf%I4|subf%I4c} %1,%5,%4\n\t{sfe|subfe} %0,%3,%2"	\
	       : "=r" (sh), "=&r" (sl)					\
	       : "r" (ah), "r" (bh), "rI" (al), "r" (bl));		\
  } while (0)
#define count_leading_zeros(count, x) \
  __asm__ ("{cntlz|cntlzw} %0,%1" : "=r" (count) : "r" (x))
#define COUNT_LEADING_ZEROS_0 32
#if defined (_ARCH_PPC) || defined (__powerpc__)
#define umul_ppmm(ph, pl, m0, m1) \
  do {									\
    USItype __m0 = (m0), __m1 = (m1);					\
    __asm__ ("mulhwu %0,%1,%2" : "=r" (ph) : "%r" (m0), "r" (m1));	\
    (pl) = __m0 * __m1;							\
  } while (0)
#define UMUL_TIME 15
#define smul_ppmm(ph, pl, m0, m1) \
  do {									\
    SItype __m0 = (m0), __m1 = (m1);					\
    __asm__ ("mulhw %0,%1,%2" : "=r" (ph) : "%r" (m0), "r" (m1));	\
    (pl) = __m0 * __m1;							\
  } while (0)
#define SMUL_TIME 14
#define UDIV_TIME 120
#else
#define UMUL_TIME 8
#define smul_ppmm(xh, xl, m0, m1) \
  __asm__ ("mul %0,%2,%3" : "=r" (xh), "=q" (xl) : "r" (m0), "r" (m1))
#define SMUL_TIME 4
#define sdiv_qrnnd(q, r, nh, nl, d) \
  __asm__ ("div %0,%2,%4" : "=r" (q), "=q" (r) : "r" (nh), "1" (nl), "r" (d))
#define UDIV_TIME 100
#endif
#endif /* 32-bit POWER architecture variants.  */

/* We should test _IBMR2 here when we add assembly support for the system
   vendor compilers.  */
#if (defined (_ARCH_PPC) || defined (__powerpc__)) && W_TYPE_SIZE == 64
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  do {									\
    if (__builtin_constant_p (bh) && (bh) == 0)				\
      __asm__ ("{a%I4|add%I4c} %1,%3,%4\n\t{aze|addze} %0,%2"		\
	     : "=r" (sh), "=&r" (sl) : "%r" (ah), "%r" (al), "rI" (bl));\
    else if (__builtin_constant_p (bh) && (bh) == ~(UDItype) 0)		\
      __asm__ ("{a%I4|add%I4c} %1,%3,%4\n\t{ame|addme} %0,%2"		\
	     : "=r" (sh), "=&r" (sl) : "%r" (ah), "%r" (al), "rI" (bl));\
    else								\
      __asm__ ("{a%I5|add%I5c} %1,%4,%5\n\t{ae|adde} %0,%2,%3"		\
	     : "=r" (sh), "=&r" (sl)					\
	     : "%r" (ah), "r" (bh), "%r" (al), "rI" (bl));		\
  } while (0)
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  do {									\
    if (__builtin_constant_p (ah) && (ah) == 0)				\
      __asm__ ("{sf%I3|subf%I3c} %1,%4,%3\n\t{sfze|subfze} %0,%2"	\
	       : "=r" (sh), "=&r" (sl) : "r" (bh), "rI" (al), "r" (bl));\
    else if (__builtin_constant_p (ah) && (ah) == ~(UDItype) 0)		\
      __asm__ ("{sf%I3|subf%I3c} %1,%4,%3\n\t{sfme|subfme} %0,%2"	\
	       : "=r" (sh), "=&r" (sl) : "r" (bh), "rI" (al), "r" (bl));\
    else if (__builtin_constant_p (bh) && (bh) == 0)			\
      __asm__ ("{sf%I3|subf%I3c} %1,%4,%3\n\t{ame|addme} %0,%2"		\
	       : "=r" (sh), "=&r" (sl) : "r" (ah), "rI" (al), "r" (bl));\
    else if (__builtin_constant_p (bh) && (bh) == ~(UDItype) 0)		\
      __asm__ ("{sf%I3|subf%I3c} %1,%4,%3\n\t{aze|addze} %0,%2"		\
	       : "=r" (sh), "=&r" (sl) : "r" (ah), "rI" (al), "r" (bl));\
    else								\
      __asm__ ("{sf%I4|subf%I4c} %1,%5,%4\n\t{sfe|subfe} %0,%3,%2"	\
	       : "=r" (sh), "=&r" (sl)					\
	       : "r" (ah), "r" (bh), "rI" (al), "r" (bl));		\
  } while (0)
#define count_leading_zeros(count, x) \
  __asm__ ("cntlzd %0,%1" : "=r" (count) : "r" (x))
#define COUNT_LEADING_ZEROS_0 64
#define umul_ppmm(ph, pl, m0, m1) \
  do {									\
    UDItype __m0 = (m0), __m1 = (m1);					\
    __asm__ ("mulhdu %0,%1,%2" : "=r" (ph) : "%r" (m0), "r" (m1));	\
    (pl) = __m0 * __m1;							\
  } while (0)
#define UMUL_TIME 15
#define smul_ppmm(ph, pl, m0, m1) \
  do {									\
    DItype __m0 = (m0), __m1 = (m1);					\
    __asm__ ("mulhd %0,%1,%2" : "=r" (ph) : "%r" (m0), "r" (m1));	\
    (pl) = __m0 * __m1;							\
  } while (0)
#define SMUL_TIME 14  /* ??? */
#define UDIV_TIME 120 /* ??? */
#endif /* 64-bit PowerPC.  */

#if defined (__pyr__) && W_TYPE_SIZE == 32
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("addw %5,%1\n\taddwc %3,%0"					\
	   : "=r" ((USItype)(sh)), "=&r" ((USItype)(sl))		\
	   : "%0" ((USItype)(ah)), "g" ((USItype)(bh)),			\
	     "%1" ((USItype)(al)), "g" ((USItype)(bl)))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("subw %5,%1\n\tsubwb %3,%0"					\
	   : "=r" ((USItype)(sh)), "=&r" ((USItype)(sl))		\
	   : "0" ((USItype)(ah)), "g" ((USItype)(bh)),			\
	     "1" ((USItype)(al)), "g" ((USItype)(bl)))
/* This insn works on Pyramids with AP, XP, or MI CPUs, but not with SP.  */
#define umul_ppmm(w1, w0, u, v) \
  ({union {UDItype __ll;						\
	   struct {USItype __h, __l;} __i;				\
	  } __x;							\
  __asm__ ("movw %1,%R0\n\tuemul %2,%0"					\
	   : "=&r" (__x.__ll)						\
	   : "g" ((USItype) (u)), "g" ((USItype)(v)));			\
  (w1) = __x.__i.__h; (w0) = __x.__i.__l;})
#endif /* __pyr__ */

#if defined (__ibm032__) /* RT/ROMP */  && W_TYPE_SIZE == 32
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("a %1,%5\n\tae %0,%3"					\
	   : "=r" ((USItype)(sh)), "=&r" ((USItype)(sl))		\
	   : "%0" ((USItype)(ah)), "r" ((USItype)(bh)),			\
	     "%1" ((USItype)(al)), "r" ((USItype)(bl)))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("s %1,%5\n\tse %0,%3"					\
	   : "=r" ((USItype)(sh)), "=&r" ((USItype)(sl))		\
	   : "0" ((USItype)(ah)), "r" ((USItype)(bh)),			\
	     "1" ((USItype)(al)), "r" ((USItype)(bl)))
#define smul_ppmm(ph, pl, m0, m1) \
  __asm__ (								\
       "s	r2,r2\n"						\
"	mts r10,%2\n"							\
"	m	r2,%3\n"						\
"	m	r2,%3\n"						\
"	m	r2,%3\n"						\
"	m	r2,%3\n"						\
"	m	r2,%3\n"						\
"	m	r2,%3\n"						\
"	m	r2,%3\n"						\
"	m	r2,%3\n"						\
"	m	r2,%3\n"						\
"	m	r2,%3\n"						\
"	m	r2,%3\n"						\
"	m	r2,%3\n"						\
"	m	r2,%3\n"						\
"	m	r2,%3\n"						\
"	m	r2,%3\n"						\
"	m	r2,%3\n"						\
"	cas	%0,r2,r0\n"						\
"	mfs	r10,%1"							\
	   : "=r" ((USItype)(ph)), "=r" ((USItype)(pl))			\
	   : "%r" ((USItype)(m0)), "r" ((USItype)(m1))			\
	   : "r2")
#define UMUL_TIME 20
#define UDIV_TIME 200
#define count_leading_zeros(count, x) \
  do {									\
    if ((x) >= 0x10000)							\
      __asm__ ("clz	%0,%1"						\
	       : "=r" ((USItype)(count)) : "r" ((USItype)(x) >> 16));	\
    else								\
      {									\
	__asm__ ("clz	%0,%1"						\
		 : "=r" ((USItype)(count)) : "r" ((USItype)(x)));	\
	(count) += 16;							\
      }									\
  } while (0)
#endif /* RT/ROMP */

#if defined (__sh2__) && W_TYPE_SIZE == 32
#define umul_ppmm(w1, w0, u, v) \
  __asm__ ("dmulu.l %2,%3\n\tsts macl,%1\n\tsts mach,%0"		\
	   : "=r" (w1), "=r" (w0) : "r" (u), "r" (v) : "macl", "mach")
#define UMUL_TIME 5
#endif

#if defined (__sparc__) && W_TYPE_SIZE == 32
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("addcc %r4,%5,%1\n\taddx %r2,%3,%0"				\
	   : "=r" (sh), "=&r" (sl)					\
	   : "%rJ" (ah), "rI" (bh),"%rJ" (al), "rI" (bl)		\
	   __CLOBBER_CC)
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("subcc %r4,%5,%1\n\tsubx %r2,%3,%0"				\
	   : "=r" (sh), "=&r" (sl)					\
	   : "rJ" (ah), "rI" (bh), "rJ" (al), "rI" (bl)	\
	   __CLOBBER_CC)
#if defined (__sparc_v9__) || defined (__sparcv9)
/* Perhaps we should use floating-point operations here?  */
#if 0
/* Triggers a bug making mpz/tests/t-gcd.c fail.
   Perhaps we simply need explicitly zero-extend the inputs?  */
#define umul_ppmm(w1, w0, u, v) \
  __asm__ ("mulx %2,%3,%%g1; srl %%g1,0,%1; srlx %%g1,32,%0" :		\
	   "=r" (w1), "=r" (w0) : "r" (u), "r" (v) : "g1")
#else
/* Use v8 umul until above bug is fixed.  */
#define umul_ppmm(w1, w0, u, v) \
  __asm__ ("umul %2,%3,%1;rd %%y,%0" : "=r" (w1), "=r" (w0) : "r" (u), "r" (v))
#endif
/* Use a plain v8 divide for v9.  */
#define udiv_qrnnd(q, r, n1, n0, d) \
  do {									\
    USItype __q;							\
    __asm__ ("mov %1,%%y;nop;nop;nop;udiv %2,%3,%0"			\
	     : "=r" (__q) : "r" (n1), "r" (n0), "r" (d));		\
    (r) = (n0) - __q * (d);						\
    (q) = __q;								\
  } while (0)
#else
#if defined (__sparc_v8__)
/* Don't match immediate range because, 1) it is not often useful,
   2) the 'I' flag thinks of the range as a 13 bit signed interval,
   while we want to match a 13 bit interval, sign extended to 32 bits,
   but INTERPRETED AS UNSIGNED.  */
#define umul_ppmm(w1, w0, u, v) \
  __asm__ ("umul %2,%3,%1;rd %%y,%0" : "=r" (w1), "=r" (w0) : "r" (u), "r" (v))
#define UMUL_TIME 5
#ifndef SUPERSPARC	/* SuperSPARC's udiv only handles 53 bit dividends */
#define udiv_qrnnd(q, r, n1, n0, d) \
  do {									\
    USItype __q;							\
    __asm__ ("mov %1,%%y;nop;nop;nop;udiv %2,%3,%0"			\
	     : "=r" (__q) : "r" (n1), "r" (n0), "r" (d));		\
    (r) = (n0) - __q * (d);						\
    (q) = __q;								\
  } while (0)
#define UDIV_TIME 25
#else
#define UDIV_TIME 60		/* SuperSPARC timing */
#endif /* SUPERSPARC */
#else /* ! __sparc_v8__ */
#if defined (__sparclite__)
/* This has hardware multiply but not divide.  It also has two additional
   instructions scan (ffs from high bit) and divscc.  */
#define umul_ppmm(w1, w0, u, v) \
  __asm__ ("umul %2,%3,%1;rd %%y,%0" : "=r" (w1), "=r" (w0) : "r" (u), "r" (v))
#define UMUL_TIME 5
#define udiv_qrnnd(q, r, n1, n0, d) \
  __asm__ ("! Inlined udiv_qrnnd\n"					\
"	wr	%%g0,%2,%%y	! Not a delayed write for sparclite\n"	\
"	tst	%%g0\n"							\
"	divscc	%3,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%%g1\n"						\
"	divscc	%%g1,%4,%0\n"						\
"	rd	%%y,%1\n"						\
"	bl,a 1f\n"							\
"	add	%1,%4,%1\n"						\
"1:	! End of inline udiv_qrnnd"					\
	   : "=r" (q), "=r" (r) : "r" (n1), "r" (n0), "rI" (d)		\
	   : "%g1" __AND_CLOBBER_CC)
#define UDIV_TIME 37
#define count_leading_zeros(count, x) \
  __asm__ ("scan %1,0,%0" : "=r" (x) : "r" (count))
/* Early sparclites return 63 for an argument of 0, but they warn that future
   implementations might change this.  Therefore, leave COUNT_LEADING_ZEROS_0
   undefined.  */
#endif /* __sparclite__ */
#endif /* __sparc_v8__ */
#endif /* __sparc_v9__ */
/* Default to sparc v7 versions of umul_ppmm and udiv_qrnnd.  */
#ifndef umul_ppmm
#define umul_ppmm(w1, w0, u, v) \
  __asm__ ("! Inlined umul_ppmm\n"					\
"	wr	%%g0,%2,%%y	! SPARC has 0-3 delay insn after a wr\n" \
"	sra	%3,31,%%g2	! Don't move this insn\n"		\
"	and	%2,%%g2,%%g2	! Don't move this insn\n"		\
"	andcc	%%g0,0,%%g1	! Don't move this insn\n"		\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,%3,%%g1\n"						\
"	mulscc	%%g1,0,%%g1\n"						\
"	add	%%g1,%%g2,%0\n"						\
"	rd	%%y,%1"							\
	   : "=r" (w1), "=r" (w0) : "%rI" (u), "r" (v)			\
	   : "%g1", "%g2" __AND_CLOBBER_CC)
#define UMUL_TIME 39		/* 39 instructions */
#endif
#ifndef udiv_qrnnd
#define LONGLONG_STANDALONE /* <----------- PLTSCHEME: Avoid extern */
#ifndef LONGLONG_STANDALONE
#define XXX_udiv_qrnnd(q, r, n1, n0, d) \
  do { USItype __r;							\
    (q) = __MPN(udiv_qrnnd) (&__r, (n1), (n0), (d));			\
    (r) = __r;								\
  } while (0)
extern USItype __MPN(udiv_qrnnd) _PROTO ((USItype *, USItype, USItype, USItype));
#ifndef UDIV_TIME
#define UDIV_TIME 140
#endif
#endif /* LONGLONG_STANDALONE */
#endif /* udiv_qrnnd */
#endif /* __sparc__ */

#if defined (__vax__) && W_TYPE_SIZE == 32
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("addl2 %5,%1\n\tadwc %3,%0"					\
	   : "=g" ((USItype)(sh)), "=&g" ((USItype)(sl))		\
	   : "%0" ((USItype)(ah)), "g" ((USItype)(bh)),			\
	     "%1" ((USItype)(al)), "g" ((USItype)(bl)))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("subl2 %5,%1\n\tsbwc %3,%0"					\
	   : "=g" ((USItype)(sh)), "=&g" ((USItype)(sl))		\
	   : "0" ((USItype)(ah)), "g" ((USItype)(bh)),			\
	     "1" ((USItype)(al)), "g" ((USItype)(bl)))
#define smul_ppmm(xh, xl, m0, m1) \
  do {									\
    union {UDItype __ll;						\
	   struct {USItype __l, __h;} __i;				\
	  } __x;							\
    USItype __m0 = (m0), __m1 = (m1);					\
    __asm__ ("emul %1,%2,$0,%0"						\
	     : "=g" (__x.__ll) : "g" (__m0), "g" (__m1));		\
    (xh) = __x.__i.__h; (xl) = __x.__i.__l;				\
  } while (0)
#define sdiv_qrnnd(q, r, n1, n0, d) \
  do {									\
    union {DItype __ll;							\
	   struct {SItype __l, __h;} __i;				\
	  } __x;							\
    __x.__i.__h = n1; __x.__i.__l = n0;					\
    __asm__ ("ediv %3,%2,%0,%1"						\
	     : "=g" (q), "=g" (r) : "g" (__x.__ll), "g" (d));		\
  } while (0)
#endif /* __vax__ */

#if defined (__z8000__) && W_TYPE_SIZE == 16
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("add	%H1,%H5\n\tadc	%H0,%H3"				\
	   : "=r" ((unsigned int)(sh)), "=&r" ((unsigned int)(sl))	\
	   : "%0" ((unsigned int)(ah)), "r" ((unsigned int)(bh)),	\
	     "%1" ((unsigned int)(al)), "rQR" ((unsigned int)(bl)))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("sub	%H1,%H5\n\tsbc	%H0,%H3"				\
	   : "=r" ((unsigned int)(sh)), "=&r" ((unsigned int)(sl))	\
	   : "0" ((unsigned int)(ah)), "r" ((unsigned int)(bh)),	\
	     "1" ((unsigned int)(al)), "rQR" ((unsigned int)(bl)))
#define umul_ppmm(xh, xl, m0, m1) \
  do {									\
    union {long int __ll;						\
	   struct {unsigned int __h, __l;} __i;				\
	  } __x;							\
    unsigned int __m0 = (m0), __m1 = (m1);				\
    __asm__ ("mult	%S0,%H3"					\
	     : "=r" (__x.__i.__h), "=r" (__x.__i.__l)			\
	     : "%1" (m0), "rQR" (m1));					\
    (xh) = __x.__i.__h; (xl) = __x.__i.__l;				\
    (xh) += ((((signed int) __m0 >> 15) & __m1)				\
	     + (((signed int) __m1 >> 15) & __m0));			\
  } while (0)
#endif /* __z8000__ */

#endif /* __GNUC__ */

#if !defined (umul_ppmm) && defined (__umulsidi3)
#define umul_ppmm(ph, pl, m0, m1) \
  {									\
    UDWtype __ll = __umulsidi3 (m0, m1);				\
    ph = (UWtype) (__ll >> W_TYPE_SIZE);				\
    pl = (UWtype) __ll;							\
  }
#endif

#if !defined (__umulsidi3)
#define __umulsidi3(u, v) \
  ({UWtype __hi, __lo;							\
    umul_ppmm (__hi, __lo, u, v);					\
    ((UDWtype) __hi << W_TYPE_SIZE) | __lo; })
#endif


/* Note the prototypes are under !define(umul_ppmm) etc too, since the HPPA
   versions above are different and we don't want to conflict.  */

#if ! defined (umul_ppmm) && HAVE_NATIVE_mpn_umul_ppmm
#define mpn_umul_ppmm  __MPN(umul_ppmm)
extern mp_limb_t mpn_umul_ppmm _PROTO ((mp_limb_t *, mp_limb_t, mp_limb_t));
#define umul_ppmm(wh, wl, u, v)                                 \
  do {                                                          \
    mp_limb_t __umul_ppmm__p0;                                  \
    (wh) = __MPN(umul_ppmm) (&__umul_ppmm__p0,                  \
                             (mp_limb_t) (u), (mp_limb_t) (v)); \
    (wl) = __umul_ppmm__p0;                                     \
  } while (0)
#endif

#if ! defined (udiv_qrnnd) && HAVE_NATIVE_mpn_udiv_qrnnd
#define mpn_udiv_qrnnd  __MPN(udiv_qrnnd)
extern mp_limb_t mpn_udiv_qrnnd _PROTO ((mp_limb_t *,
                                         mp_limb_t, mp_limb_t, mp_limb_t));
#define udiv_qrnnd(q, r, n1, n0, d)                                           \
  do {                                                                        \
    mp_limb_t __udiv_qrnnd__r;                                                \
    (q) = mpn_udiv_qrnnd (&__udiv_qrnnd__r,                                   \
                          (mp_limb_t) (n1), (mp_limb_t) (n0), (mp_limb_t) d); \
    (r) = __udiv_qrnnd__r;                                                    \
  } while (0)
#endif

#endif /* !MZ_GMP_NO_ASM */


/* If this machine has no inline assembler, use C macros.  */

#if !defined (add_ssaaaa)
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  do {									\
    UWtype __x;								\
    __x = (al) + (bl);							\
    (sh) = (ah) + (bh) + (__x < (al));					\
    (sl) = __x;								\
  } while (0)
#endif

#if !defined (sub_ddmmss)
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  do {									\
    UWtype __x;								\
    __x = (al) - (bl);							\
    (sh) = (ah) - (bh) - (__x > (al));					\
    (sl) = __x;								\
  } while (0)
#endif

/* If we lack umul_ppmm but have smul_ppmm, define umul_ppmm in terms of
   smul_ppmm.  */
#if !defined (umul_ppmm) && defined (smul_ppmm)
#define umul_ppmm(w1, w0, u, v)						\
  do {									\
    UWtype __w1;							\
    UWtype __xm0 = (u), __xm1 = (v);					\
    smul_ppmm (__w1, w0, __xm0, __xm1);					\
    (w1) = __w1 + (-(__xm0 >> (W_TYPE_SIZE - 1)) & __xm1)		\
		+ (-(__xm1 >> (W_TYPE_SIZE - 1)) & __xm0);		\
  } while (0)
#endif

/* If we still don't have umul_ppmm, define it using plain C.  */
#if !defined (umul_ppmm)
#define umul_ppmm(w1, w0, u, v)						\
  do {									\
    UWtype __x0, __x1, __x2, __x3;					\
    UHWtype __ul, __vl, __uh, __vh;					\
    UWtype __u = (u), __v = (v);					\
									\
    __ul = __ll_lowpart (__u);						\
    __uh = __ll_highpart (__u);						\
    __vl = __ll_lowpart (__v);						\
    __vh = __ll_highpart (__v);						\
									\
    __x0 = (UWtype) __ul * __vl;					\
    __x1 = (UWtype) __ul * __vh;					\
    __x2 = (UWtype) __uh * __vl;					\
    __x3 = (UWtype) __uh * __vh;					\
									\
    __x1 += __ll_highpart (__x0);/* this can't give carry */		\
    __x1 += __x2;		/* but this indeed can */		\
    if (__x1 < __x2)		/* did we get it? */			\
      __x3 += __ll_B;		/* yes, add it in the proper pos. */	\
									\
    (w1) = __x3 + __ll_highpart (__x1);					\
    (w0) = (__x1 << W_TYPE_SIZE/2) + __ll_lowpart (__x0);		\
  } while (0)
#endif

/* If we don't have smul_ppmm, define it using umul_ppmm (which surely will
   exist in one form or another.  */
#if !defined (smul_ppmm)
#define smul_ppmm(w1, w0, u, v)						\
  do {									\
    UWtype __w1;							\
    UWtype __xm0 = (u), __xm1 = (v);					\
    umul_ppmm (__w1, w0, __xm0, __xm1);					\
    (w1) = __w1 - (-(__xm0 >> (W_TYPE_SIZE - 1)) & __xm1)		\
		- (-(__xm1 >> (W_TYPE_SIZE - 1)) & __xm0);		\
  } while (0)
#endif

/* Define this unconditionally, so it can be used for debugging.  */
#define __udiv_qrnnd_c(q, r, n1, n0, d) \
  do {									\
    UWtype __d1, __d0, __q1, __q0, __r1, __r0, __m;			\
    __d1 = __ll_highpart (d);						\
    __d0 = __ll_lowpart (d);						\
									\
    __q1 = (n1) / __d1;							\
    __r1 = (n1) - __q1 * __d1;						\
    __m = (UWtype) __q1 * __d0;						\
    __r1 = __r1 * __ll_B | __ll_highpart (n0);				\
    if (__r1 < __m)							\
      {									\
	__q1--, __r1 += (d);						\
	if (__r1 >= (d)) /* i.e. we didn't get carry when adding to __r1 */\
	  if (__r1 < __m)						\
	    __q1--, __r1 += (d);					\
      }									\
    __r1 -= __m;							\
									\
    __q0 = __r1 / __d1;							\
    __r0 = __r1  - __q0 * __d1;						\
    __m = (UWtype) __q0 * __d0;						\
    __r0 = __r0 * __ll_B | __ll_lowpart (n0);				\
    if (__r0 < __m)							\
      {									\
	__q0--, __r0 += (d);						\
	if (__r0 >= (d))						\
	  if (__r0 < __m)						\
	    __q0--, __r0 += (d);					\
      }									\
    __r0 -= __m;							\
									\
    (q) = (UWtype) __q1 * __ll_B | __q0;				\
    (r) = __r0;								\
  } while (0)

/* If the processor has no udiv_qrnnd but sdiv_qrnnd, go through
   __udiv_w_sdiv (defined in libgcc or elsewhere).  */
#if !defined (udiv_qrnnd) && defined (sdiv_qrnnd)
#define udiv_qrnnd(q, r, nh, nl, d) \
  do {									\
    UWtype __r;								\
    (q) = __MPN(udiv_w_sdiv) (&__r, nh, nl, d);				\
    (r) = __r;								\
  } while (0)
#endif

/* If udiv_qrnnd was not defined for this processor, use __udiv_qrnnd_c.  */
#if !defined (udiv_qrnnd)
#define UDIV_NEEDS_NORMALIZATION 1
#define udiv_qrnnd __udiv_qrnnd_c
#endif

#if !defined (count_leading_zeros)
extern
#if __STDC__
const
#endif
unsigned char __clz_tab[];
#define count_leading_zeros(count, x) \
  do {									\
    UWtype __xr = (x);							\
    UWtype __a;								\
									\
    if (W_TYPE_SIZE <= 32)						\
      {									\
	__a = __xr < ((UWtype) 1 << 2*__BITS4)				\
	  ? (__xr < ((UWtype) 1 << __BITS4) ? 0 : __BITS4)		\
	  : (__xr < ((UWtype) 1 << 3*__BITS4) ?  2*__BITS4 : 3*__BITS4);\
      }									\
    else								\
      {									\
	for (__a = W_TYPE_SIZE - 8; __a > 0; __a -= 8)			\
	  if (((__xr >> __a) & 0xff) != 0)				\
	    break;							\
      }									\
									\
    (count) = W_TYPE_SIZE - (__clz_tab[__xr >> __a] + __a);		\
  } while (0)
/* This version gives a well-defined value for zero. */
#define COUNT_LEADING_ZEROS_0 W_TYPE_SIZE
#define COUNT_LEADING_ZEROS_NEED_CLZ_TAB
#endif

#if !defined (count_trailing_zeros)
/* Define count_trailing_zeros using count_leading_zeros.  The latter might be
   defined in asm, but if it is not, the C version above is good enough.  */
#define count_trailing_zeros(count, x) \
  do {									\
    UWtype __ctz_x = (x);						\
    UWtype __ctz_c;							\
    count_leading_zeros (__ctz_c, __ctz_x & -__ctz_x);			\
    (count) = W_TYPE_SIZE - 1 - __ctz_c;				\
  } while (0)
#endif

#ifndef UDIV_NEEDS_NORMALIZATION
#define UDIV_NEEDS_NORMALIZATION 0
#endif

/* Give defaults for UMUL_TIME and UDIV_TIME.  */
#ifndef UMUL_TIME
#define UMUL_TIME 1
#endif

#ifndef UDIV_TIME
#define UDIV_TIME UMUL_TIME
#endif

/* count_trailing_zeros is often on the slow side, so make that the default */
#ifndef COUNT_TRAILING_ZEROS_TIME
#define COUNT_TRAILING_ZEROS_TIME  15  /* cycles */
#endif


