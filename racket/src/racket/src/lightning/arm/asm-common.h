/******************************** -*- C -*- ****************************
 *
 *	Dynamic assembler support
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright 2000, 2001, 2002 Free Software Foundation, Inc.
 * Written by Paolo Bonzini.
 *
 * This file is part of GNU lightning.
 *
 * GNU lightning is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 * 
 * GNU lightning is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with GNU lightning; see the file COPYING.LESSER; if not, write to the
 * Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 ***********************************************************************/


#ifndef __lightning_asm_common_h_
#define __lightning_asm_common_h_

#if 1
# include <assert.h>
# define jit_assert(x) assert(x)
#else
# define jit_assert(x) /* empty */
#endif

#define __WORDSIZE 32

#define __jit_constructor /* empty */
#define __jit_inline XFORM_NONGCING MZ_INLINE static

typedef signed char		_sc;
typedef unsigned char		_uc, jit_insn;
typedef unsigned short		_us;
typedef unsigned int		_ui;
typedef long			_sl;
typedef unsigned long		_ul;
typedef struct jit_local_state	jit_local_state;

typedef struct jit_state jit_state;

struct jit_local_state {
    int          long_jumps;
    int		 nextarg_get;
    int		 nextarg_put;
    int		 nextarg_getf;
    jit_insn	*thumb;
};

#ifdef JIT_ARM_DYNAMIC_CPU
struct {
    _ui		version		: 4;
    _ui		extend		: 1;
    /* only generate thumb instructions for thumb2 */
    _ui		thumb		: 1;
    _ui		vfp		: 3;
    _ui		neon		: 1;
    _ui		abi		: 2;
} jit_cpu;
#endif

struct {
    /* prevent using thumb instructions that set flags? */
    _ui		no_set_flags	: 1;
} jit_flags;

typedef struct jit_state {
    union {
	jit_insn	*pc;
	_uc		*uc_pc;
	_us		*us_pc;
	_ui		*ui_pc;
	_ul		*ul_pc;
    } x;
    struct jit_fp	*fp;
    jit_local_state	 jitl;
} jit_state_t[1];

#ifndef _ASM_SAFETY
#define JITFAIL(MSG) 0
#else
#if (defined __STDC_VERSION__ && __STDC_VERSION__ >= 199901L) || (defined __GNUC__ && (__GNUC__ == 3 ? __GNUC_MINOR__ >= 2 : __GNUC__ > 3))
#define JITFAIL(MSG) jit_fail(MSG, __FILE__, __LINE__, __func__)
#elif defined __GNUC__
#define JITFAIL(MSG) jit_fail(MSG, __FILE__, __LINE__, __FUNCTION__)
#else
#define JITFAIL(MSG) jit_fail(MSG, __FILE__, __LINE__, "(unknown)")
#endif
#endif

#if (defined __STDC_VERSION__ && __STDC_VERSION__ >= 199901L) || (defined __GNUC__ && (__GNUC__ == 3 ? __GNUC_MINOR__ >= 2 : __GNUC__ > 3))
#define JITSORRY(MSG) jit_fail("sorry, unimplemented: " MSG, __FILE__, __LINE__, __func__)
#elif defined __GNUC__
#define JITSORRY(MSG) jit_fail("sorry, unimplemented: " MSG, __FILE__, __LINE__, __FUNCTION__)
#else
#define JITSORRY(MSG) jit_fail("sorry, unimplemented: " MSG, __FILE__, __LINE__, "(unknown)")
#endif

#ifdef __GNUC__
#define JIT_UNUSED		__attribute__((__unused__))
#else
#define JIT_UNUSED
#endif

static int jit_fail(const char *, const char*, int, const char *) JIT_UNUSED;


/* NextStep 2.0 cc is really gcc 1.93 but it defines __GNUC__ = 2 and
   does not implement __extension__.  But that compiler doesn't define
   __GNUC_MINOR__.  */
#ifdef __GNUC__
#if __GNUC__ < 2 || (defined(__NeXT__) && !__GNUC_MINOR__)
#define __extension__
#endif

#define _TEMPD(type, var) 	

#define _TEMP(type, var, val, body) __extension__ ({	\
  register struct { type var } _jitl; _jitl.var = val;	\
  body;							\
})

#else

/* Between loading a global and calling a subroutine, we choose the lesser
 * evil. */
#define _TEMPD(type, var) 	static type var;
#define _TEMP(type, var, val, body) ((var = val), body)

#endif

#define _jit_UC(X)	((_uc  )(X))
#define _jit_US(X)	((_us  )(X))
#define _jit_UI(X)	((_ui  )(X))
#define _jit_SI(X)	((int  )(X))
#define _jit_SL(X)	((_sl  )(X))
#define _jit_UL(X)	((_ul  )(X))
# define _PUC(X)	((_uc *)(X))
# define _PUS(X)	((_us *)(X))
# define _PUI(X)	((_ui *)(X))
# define _PSI(X)	((int *)(X))
# define _PSL(X)	((_sl *)(X))
# define _PUL(X)	((_ul *)(X))

#define _jit_B(B)         _jit_UL(((*_jitp->x.uc_pc++)= _jit_UC((B)&  0xff)))
#define _jit_W(W)         _jit_UL(((*_jitp->x.us_pc++)= _jit_US((W)&0xffff)))
#define _jit_I(I)         _jit_UL(((*_jitp->x.ui_pc++)= _jit_UI((I)       )))
#define _jit_L(L)         _jit_UL(((*_jitp->x.ul_pc++)= _jit_UL((L)       )))
#define _jit_I_noinc(I)   _jit_UL(((*_jitp->x.ui_pc)=   _jit_UI((I)       )))

#define _MASK(N)	((unsigned long)((1L<<(N)))-1L)
#define _siP(N,I)	(!((((unsigned long)(I))^(((unsigned long)(I))<<1))&~_MASK(N)))
#define _uiP(N,I)	(!(((unsigned long)(I))&~_MASK(N)))
#define _suiP(N,I)	(_siP(N,I) | _uiP(N,I))

#ifndef _ASM_SAFETY
#define _ck_s(W,I)	(_jit_UL(I) & _MASK(W))
#define _ck_u(W,I)    	(_jit_UL(I) & _MASK(W))
#define _ck_su(W,I)    	(_jit_UL(I) & _MASK(W))
#define _ck_d(W,I)    	(_jit_UL(I) & _MASK(W))
#else
#define _ck_s(W,I)	(_siP(W,I) ? (_jit_UL(I) & _MASK(W)) : JITFAIL(  "signed integer `"#I"' too large for "#W"-bit field"))
#define _ck_u(W,I)    	(_uiP(W,I) ? (_jit_UL(I) & _MASK(W)) : JITFAIL("unsigned integer `"#I"' too large for "#W"-bit field"))
#define _ck_su(W,I)    	(_suiP(W,I) ? (_jit_UL(I) & _MASK(W)) : JITFAIL(        "integer `"#I"' too large for "#W"-bit field"))
#define _ck_d(W,I)    	(_siP(W,I) ? (_jit_UL(I) & _MASK(W)) : JITFAIL(    "displacement `"#I"' too large for "#W"-bit field"))
#endif

#define _s0P(I)		((I)==0)
#define _s8P(I)		_siP(8,I)
#define _s16P(I)	_siP(16,I)
#define _s20P(I)	_siP(20,I)
#define _s24P(I)	_siP(24,I)
#define _s32P(I)	_siP(32,I)
#define _u8P(I)		_uiP(8,I)
#define _u16P(I)	_uiP(16,I)
#define _u32P(I)	_uiP(32,I)

#define _su8(I)		_ck_su(8,I)
#define _su16(I)	_ck_su(16,I)

#define _s1(I)          _ck_s( 1,I)
#define _s2(I)          _ck_s( 2,I)
#define _s3(I)          _ck_s( 3,I)
#define _s4(I)          _ck_s( 4,I)
#define _s5(I)          _ck_s( 5,I)
#define _s6(I)          _ck_s( 6,I)
#define _s7(I)          _ck_s( 7,I)
#define _s8(I)          _ck_s( 8,I)
#define _s9(I)          _ck_s( 9,I)
#define _s10(I)         _ck_s(10,I)
#define _s11(I)         _ck_s(11,I)
#define _s12(I)         _ck_s(12,I)
#define _s13(I)         _ck_s(13,I)
#define _s14(I)         _ck_s(14,I)
#define _s15(I)         _ck_s(15,I)
#define _s16(I)         _ck_s(16,I)
#define _s17(I)         _ck_s(17,I)
#define _s18(I)         _ck_s(18,I)
#define _s19(I)         _ck_s(19,I)
#define _s20(I)         _ck_s(20,I)
#define _s21(I)         _ck_s(21,I)
#define _s22(I)         _ck_s(22,I)
#define _s23(I)         _ck_s(23,I)
#define _s24(I)         _ck_s(24,I)
#define _s25(I)         _ck_s(25,I)
#define _s26(I)         _ck_s(26,I)
#define _s27(I)         _ck_s(27,I)
#define _s28(I)         _ck_s(28,I)
#define _s29(I)         _ck_s(29,I)
#define _s30(I)         _ck_s(30,I)
#define _s31(I)         _ck_s(31,I)
#if __WORDSIZE == 32
#  define _s32(I)	(I)
#else
#  define _s32(I)	_ck_s(32,I)
#endif
#define _u1(I)          _ck_u( 1,I)
#define _u2(I)          _ck_u( 2,I)
#define _u3(I)          _ck_u( 3,I)
#define _u4(I)          _ck_u( 4,I)
#define _u5(I)          _ck_u( 5,I)
#define _u6(I)          _ck_u( 6,I)
#define _u7(I)          _ck_u( 7,I)
#define _u8(I)          _ck_u( 8,I)
#define _u9(I)          _ck_u( 9,I)
#define _u10(I)         _ck_u(10,I)
#define _u11(I)         _ck_u(11,I)
#define _u12(I)         _ck_u(12,I)
#define _u13(I)         _ck_u(13,I)
#define _u14(I)         _ck_u(14,I)
#define _u15(I)         _ck_u(15,I)
#define _u16(I)         _ck_u(16,I)
#define _u17(I)         _ck_u(17,I)
#define _u18(I)         _ck_u(18,I)
#define _u19(I)         _ck_u(19,I)
#define _u20(I)         _ck_u(20,I)
#define _u21(I)         _ck_u(21,I)
#define _u22(I)         _ck_u(22,I)
#define _u23(I)         _ck_u(23,I)
#define _u24(I)         _ck_u(24,I)
#define _u25(I)         _ck_u(25,I)
#define _u26(I)         _ck_u(26,I)
#define _u27(I)         _ck_u(27,I)
#define _u28(I)         _ck_u(28,I)
#define _u29(I)         _ck_u(29,I)
#define _u30(I)         _ck_u(30,I)
#define _u31(I)         _ck_u(31,I)
#if __WORDSIZE == 32
#  define _u32(I)	(I)
#else
#  define _u32(I)	_ck_u(32,I)
#endif

#endif /* __lightning_asm_common_h */
