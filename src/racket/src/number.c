/*
  Racket
  Copyright (c) 2004-2010 PLT Scheme Inc.
  Copyright (c) 1995-2001 Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301 USA.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

#include "schpriv.h"
#include "nummacs.h"
#include <math.h>
#include <string.h>
#include <ctype.h>
#ifndef DONT_IGNORE_FPE_SIGNAL
#include <signal.h>
#endif
#ifdef IGNORE_BY_BORLAND_CONTROL_87
#include <float.h>
#endif
#ifdef IGNORE_BY_MS_CONTROL_87
#include <float.h>
#endif

#ifdef USE_SINGLE_FLOATS_AS_DEFAULT
# ifndef MZ_USE_SINGLE_FLOATS
#  undef USE_SINGLE_FLOATS_AS_DEFAULT
# endif
#endif

#ifdef SIXTY_FOUR_BIT_INTEGERS
# define MAX_SHIFT_TRY 61
# define MAX_SHIFT_EVER 64
#else
# define MAX_SHIFT_TRY 29
# define MAX_SHIFT_EVER 32
#endif

/* locals */
static Scheme_Object *number_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *complex_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *real_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *rational_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *integer_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *exact_integer_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *exact_nonnegative_integer_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *exact_positive_integer_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *fixnum_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *inexact_real_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *exact_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *even_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *bitwise_or (int argc, Scheme_Object *argv[]);
static Scheme_Object *bitwise_xor (int argc, Scheme_Object *argv[]);
static Scheme_Object *bitwise_not (int argc, Scheme_Object *argv[]);
static Scheme_Object *bitwise_bit_set_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *bitwise_bit_field (int argc, Scheme_Object *argv[]);
static Scheme_Object *integer_length (int argc, Scheme_Object *argv[]);
static Scheme_Object *gcd (int argc, Scheme_Object *argv[]);
static Scheme_Object *lcm (int argc, Scheme_Object *argv[]);
static Scheme_Object *floor_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *ceiling (int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_truncate (int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_round (int argc, Scheme_Object *argv[]);
static Scheme_Object *numerator (int argc, Scheme_Object *argv[]);
static Scheme_Object *denominator (int argc, Scheme_Object *argv[]);
static Scheme_Object *exp_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *log_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *sin_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cos_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *tan_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *asin_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *acos_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *atan_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *magnitude (int argc, Scheme_Object *argv[]);
static Scheme_Object *angle (int argc, Scheme_Object *argv[]);
static Scheme_Object *int_sqrt (int argc, Scheme_Object *argv[]);
static Scheme_Object *int_sqrt_rem (int argc, Scheme_Object *argv[]);

static Scheme_Object *flvector (int argc, Scheme_Object *argv[]);
static Scheme_Object *flvector_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *flvector_length (int argc, Scheme_Object *argv[]);
static Scheme_Object *make_flvector (int argc, Scheme_Object *argv[]);
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
static Scheme_Object *shared_flvector (int argc, Scheme_Object *argv[]);
static Scheme_Object *make_shared_flvector (int argc, Scheme_Object *argv[]);
#endif

static Scheme_Object *fxvector (int argc, Scheme_Object *argv[]);
static Scheme_Object *fxvector_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *fxvector_length (int argc, Scheme_Object *argv[]);
static Scheme_Object *make_fxvector (int argc, Scheme_Object *argv[]);
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
static Scheme_Object *shared_fxvector (int argc, Scheme_Object *argv[]);
static Scheme_Object *make_shared_fxvector (int argc, Scheme_Object *argv[]);
#endif

static Scheme_Object *integer_to_fl (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_to_integer (int argc, Scheme_Object *argv[]);

static Scheme_Object *fx_and (int argc, Scheme_Object *argv[]);
static Scheme_Object *fx_or (int argc, Scheme_Object *argv[]);
static Scheme_Object *fx_xor (int argc, Scheme_Object *argv[]);
static Scheme_Object *fx_not (int argc, Scheme_Object *argv[]);
static Scheme_Object *fx_lshift (int argc, Scheme_Object *argv[]);
static Scheme_Object *fx_rshift (int argc, Scheme_Object *argv[]);
static Scheme_Object *fx_to_fl (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_to_fx (int argc, Scheme_Object *argv[]);

static Scheme_Object *fl_floor (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_ceiling (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_truncate (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_round (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_sin (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_cos (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_tan (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_asin (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_acos (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_atan (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_exp (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_log (int argc, Scheme_Object *argv[]);

static Scheme_Object *unsafe_fx_and (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fx_or (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fx_xor (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fx_not (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fx_lshift (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fx_rshift (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fx_to_fl (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fl_to_fx (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_set (int argc, Scheme_Object *argv[]);

static Scheme_Object *unsafe_flvector_length (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_flvector_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_flvector_set (int argc, Scheme_Object *argv[]);

static Scheme_Object *unsafe_fxvector_length (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fxvector_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fxvector_set (int argc, Scheme_Object *argv[]);
static Scheme_Object *s16_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *s16_set (int argc, Scheme_Object *argv[]);
static Scheme_Object *u16_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *u16_set (int argc, Scheme_Object *argv[]);

static Scheme_Object *unsafe_make_flrectangular (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_flreal_part (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_flimag_part (int argc, Scheme_Object *argv[]);

/* globals */
READ_ONLY double scheme_infinity_val;
READ_ONLY double scheme_minus_infinity_val;
READ_ONLY double scheme_floating_point_zero = 0.0;
READ_ONLY double scheme_floating_point_nzero = 0.0; /* negated below; many compilers treat -0.0 as 0.0, 
					     but otherwise correctly implement fp negation */
READ_ONLY static double not_a_number_val;

READ_ONLY Scheme_Object *scheme_inf_object, *scheme_minus_inf_object, *scheme_nan_object;

#define zeroi scheme_exact_zero

READ_ONLY Scheme_Object *scheme_zerod, *scheme_nzerod, *scheme_pi, *scheme_half_pi, *scheme_plus_i, *scheme_minus_i;
#ifdef MZ_USE_SINGLE_FLOATS
READ_ONLY Scheme_Object *scheme_zerof, *scheme_nzerof, *scheme_single_pi;
READ_ONLY Scheme_Object *scheme_single_inf_object, *scheme_single_minus_inf_object, *scheme_single_nan_object;
#endif


#ifdef FREEBSD_CONTROL_387
#include <machine/floatingpoint.h>
#endif
#ifdef LINUX_CONTROL_387
#include <fpu_control.h>
#endif
#ifdef ALPHA_CONTROL_FP
#include <machine/fpu.h>
#endif

void
scheme_init_number (Scheme_Env *env)
{
  Scheme_Object *p;

  REGISTER_SO(scheme_pi);
  REGISTER_SO(scheme_half_pi);
  REGISTER_SO(scheme_zerod);
  REGISTER_SO(scheme_nzerod);
#ifdef MZ_USE_SINGLE_FLOATS
  REGISTER_SO(scheme_single_pi);
  REGISTER_SO(scheme_zerof);
  REGISTER_SO(scheme_nzerof);
#endif
  REGISTER_SO(scheme_plus_i);
  REGISTER_SO(scheme_minus_i);
  REGISTER_SO(scheme_inf_object);
  REGISTER_SO(scheme_minus_inf_object);
  REGISTER_SO(scheme_nan_object);
#ifdef MZ_USE_SINGLE_FLOATS
  REGISTER_SO(scheme_single_inf_object);
  REGISTER_SO(scheme_single_minus_inf_object);
  REGISTER_SO(scheme_single_nan_object);
#endif
    
  START_XFORM_SKIP;
#ifndef DONT_IGNORE_FPE_SIGNAL
  MZ_SIGSET(SIGFPE, SIG_IGN);
#endif
#ifdef FREEBSD_CONTROL_387
  fpsetmask(0);
#endif
#ifdef LINUX_CONTROL_387
  __setfpucw(_FPU_EXTENDED + _FPU_RC_NEAREST + 0x3F);
#endif
#ifdef IGNORE_BY_BORLAND_CONTROL_87
  {
    int bits = 0x3F + RC_NEAR + PC_64;
    _control87(bits, 0xFFFF);
  }
#endif
#ifdef IGNORE_BY_MS_CONTROL_87
  /* Shouldn't be necessary, because the C library
     should do this, but explictly masking exceptions
     makes Racket work under Bochs 2.1.1 with Win95 */
  _control87(_MCW_EM, _MCW_EM);
#endif
#ifdef ALPHA_CONTROL_FP
  {
    long flags = ieee_get_fp_control();
    flags |= IEEE_TRAP_ENABLE_MASK;
    ieee_set_fp_control(flags);
  }
#endif
#ifdef ASM_DBLPREC_CONTROL_87
  {
    /* Make x87 computations double-precision instead of 
       extended-precision, so that if/when the JIT generates
       x87 instructions, it's consistent with everything else. */
    int _dblprec = 0x27F;
    asm ("fldcw %0" : : "m" (_dblprec));
  }
#endif
  END_XFORM_SKIP;

#if defined(HUGE_VAL) && !defined(USE_DIVIDE_MAKE_INFINITY)
  scheme_infinity_val = HUGE_VAL;
#else
#ifndef USE_INFINITY_FUNC
  scheme_infinity_val = 1.0 / scheme_floating_point_zero;
#else
  scheme_infinity_val = infinity();
#endif
#endif

#ifdef ZERO_MINUS_ZERO_IS_POS_ZERO
  scheme_floating_point_nzero = -1.0 / scheme_infinity_val;
#else
  scheme_floating_point_nzero = - scheme_floating_point_nzero;
#endif

  scheme_minus_infinity_val = -scheme_infinity_val;
  not_a_number_val = scheme_infinity_val + scheme_minus_infinity_val;
  
  scheme_zerod = scheme_make_double(1.0);
  SCHEME_DBL_VAL(scheme_zerod) = 0.0;
  scheme_nzerod = scheme_make_double(-1.0);
  SCHEME_DBL_VAL(scheme_nzerod) = scheme_floating_point_nzero;
  
  scheme_pi = scheme_make_double(atan2(0.0, -1.0));
  scheme_half_pi = scheme_make_double(atan2(0.0, -1.0)/2);
#ifdef MZ_USE_SINGLE_FLOATS
  scheme_zerof = scheme_make_float(0.0f);
  scheme_nzerof = scheme_make_float(-0.0f);
  scheme_single_pi = scheme_make_float((float)atan2(0.0, -1.0));
#endif
  scheme_plus_i = scheme_make_complex(scheme_make_integer(0), scheme_make_integer(1));
  scheme_minus_i = scheme_make_complex(scheme_make_integer(0), scheme_make_integer(-1));
  
  scheme_inf_object = scheme_make_double(scheme_infinity_val);
  scheme_minus_inf_object = scheme_make_double(scheme_minus_infinity_val);
#ifdef NAN_EQUALS_ANYTHING
  scheme_nan_object = scheme_make_double(1);
  SCHEME_DBL_VAL(scheme_nan_object) = not_a_number_val;
#else
  scheme_nan_object = scheme_make_double(not_a_number_val);
#endif
#ifdef MZ_USE_SINGLE_FLOATS
  scheme_single_inf_object = scheme_make_float((float)scheme_infinity_val);
  scheme_single_minus_inf_object = scheme_make_float((float)scheme_minus_infinity_val);
  scheme_single_nan_object = scheme_make_float((float)not_a_number_val);
#endif

  p = scheme_make_folding_prim(number_p, "number?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("number?", p, env);

  scheme_add_global_constant("complex?", 
			     scheme_make_folding_prim(complex_p,
						      "complex?",
						      1, 1, 1),
			     env);

  p = scheme_make_folding_prim(real_p, "real?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("real?", p, env);

  scheme_add_global_constant("rational?", 
			     scheme_make_folding_prim(rational_p,
						      "rational?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("integer?", 
			     scheme_make_folding_prim(integer_p,
						      "integer?",
						      1, 1, 1),
			     env);

  p = scheme_make_folding_prim(exact_integer_p, "exact-integer?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("exact-integer?", p, env);

  p = scheme_make_folding_prim(exact_nonnegative_integer_p, "exact-nonnegative-integer?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("exact-nonnegative-integer?", p, env);

  p = scheme_make_folding_prim(exact_positive_integer_p, "exact-positive-integer?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("exact-positive-integer?", p, env);

  p = scheme_make_immed_prim(fixnum_p, "fixnum?", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("fixnum?", p, env);

  p = scheme_make_folding_prim(inexact_real_p, "inexact-real?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("inexact-real?", p, env);

  scheme_add_global_constant("exact?", 
			     scheme_make_folding_prim(exact_p,
						      "exact?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("inexact?", 
			     scheme_make_folding_prim(scheme_inexact_p,
						      "inexact?",
						      1, 1, 1),
			     env);

  p = scheme_make_folding_prim(scheme_odd_p, "odd?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("odd?", p, env);
  
  p = scheme_make_folding_prim(even_p, "even?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("even?", p, env);

  p = scheme_make_folding_prim(scheme_bitwise_and, "bitwise-and", 0, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_BINARY_INLINED
                                | SCHEME_PRIM_IS_NARY_INLINED);
  scheme_add_global_constant("bitwise-and", p, env);

  p = scheme_make_folding_prim(bitwise_or, "bitwise-ior", 0, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_BINARY_INLINED
                                | SCHEME_PRIM_IS_NARY_INLINED);
  scheme_add_global_constant("bitwise-ior", p, env);

  p = scheme_make_folding_prim(bitwise_xor, "bitwise-xor", 0, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_BINARY_INLINED
                                | SCHEME_PRIM_IS_NARY_INLINED);
  scheme_add_global_constant("bitwise-xor", p, env);

  p = scheme_make_folding_prim(bitwise_not, "bitwise-not", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("bitwise-not", p, env);

  p = scheme_make_folding_prim(bitwise_bit_set_p, "bitwise-bit-set?", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  scheme_add_global_constant("bitwise-bit-set?", p, env);

  scheme_add_global_constant("bitwise-bit-field",
                             scheme_make_folding_prim(bitwise_bit_field, 
                                                      "bitwise-bit-field",
                                                      3, 3, 1), 
                             env);

  p = scheme_make_folding_prim(scheme_bitwise_shift, "arithmetic-shift", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  scheme_add_global_constant("arithmetic-shift", p, env);

  scheme_add_global_constant("integer-length",
                             scheme_make_folding_prim(integer_length, 
                                                      "integer-length", 
                                                      1, 1, 1), 
                             env);

  scheme_add_global_constant("gcd", 
			     scheme_make_folding_prim(gcd,
						      "gcd", 
						      0, -1, 1),
			     env);
  scheme_add_global_constant("lcm", 
			     scheme_make_folding_prim(lcm,
						      "lcm", 
						      0, -1, 1),
			     env);
  scheme_add_global_constant("floor", 
			     scheme_make_folding_prim(floor_prim,
						      "floor",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("ceiling", 
			     scheme_make_folding_prim(ceiling,
						      "ceiling",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("truncate", 
			     scheme_make_folding_prim(sch_truncate,
						      "truncate",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("round", 
			     scheme_make_folding_prim(sch_round,
						      "round",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("numerator", 
			     scheme_make_folding_prim(numerator,
						      "numerator",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("denominator", 
			     scheme_make_folding_prim(denominator,
						      "denominator",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("exp", 
			     scheme_make_folding_prim(exp_prim,
						      "exp",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("log", 
			     scheme_make_folding_prim(log_prim,
						      "log",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("sin", 
			     scheme_make_folding_prim(sin_prim,
						      "sin",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("cos", 
			     scheme_make_folding_prim(cos_prim,
						      "cos",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("tan", 
			     scheme_make_folding_prim(tan_prim,
						      "tan",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("asin", 
			     scheme_make_folding_prim(asin_prim,
						      "asin",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("acos", 
			     scheme_make_folding_prim(acos_prim,
						      "acos",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("atan", 
			     scheme_make_folding_prim(atan_prim,
						      "atan",
						      1, 2, 1),
			     env);
  scheme_add_global_constant("sqrt", 
			     scheme_make_folding_prim(scheme_sqrt,
						      "sqrt",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("integer-sqrt", 
			     scheme_make_folding_prim(int_sqrt,
						      "integer-sqrt",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("integer-sqrt/remainder", 
			     scheme_make_prim_w_arity2(int_sqrt_rem,
						       "integer-sqrt/remainder",
						       1, 1,
						       2, 2),
			     env);
  scheme_add_global_constant("expt", 
			     scheme_make_folding_prim(scheme_expt,
						      "expt", 
						      2, 2, 1),
			     env);

  p = scheme_make_folding_prim(scheme_checked_make_rectangular, "make-rectangular", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  scheme_add_global_constant("make-rectangular", p, env);

  scheme_add_global_constant("make-polar", 
			     scheme_make_folding_prim(scheme_make_polar,
						      "make-polar", 
						      2, 2, 1),
			     env);

  p = scheme_make_folding_prim(scheme_checked_real_part, "real-part", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("real-part", p, env);

  p = scheme_make_folding_prim(scheme_checked_imag_part, "imag-part", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("imag-part", p, env);

  scheme_add_global_constant("angle", 
			     scheme_make_folding_prim(angle,
						      "angle",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("magnitude", 
			     scheme_make_folding_prim(magnitude,
						      "magnitude",
						      1, 1, 1),
			     env);
  
  p = scheme_make_folding_prim(scheme_exact_to_inexact, "exact->inexact", 1, 1, 1);
  if (scheme_can_inline_fp_op())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  scheme_add_global_constant("exact->inexact", p, env);

  p = scheme_make_folding_prim(scheme_inexact_to_exact, "inexact->exact", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("inexact->exact", p, env);
}

void scheme_init_flfxnum_number(Scheme_Env *env)
{
  Scheme_Object *p;

  scheme_add_global_constant("flvector",
                             scheme_make_prim_w_arity(flvector,
                                                      "flvector",
                                                      0, -1),
			     env);
  scheme_add_global_constant("flvector?",
                             scheme_make_folding_prim(flvector_p,
                                                      "flvector?",
                                                      1, 1, 1),
			     env);
  scheme_add_global_constant("make-flvector",
                             scheme_make_immed_prim(make_flvector,
                                                    "make-flvector",
                                                    1, 2),
			     env);

#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  GLOBAL_PRIM_W_ARITY("shared-flvector", shared_flvector, 0, -1, env);
  GLOBAL_PRIM_W_ARITY("make-shared-flvector", make_shared_flvector, 1, 2, env);
#else
  GLOBAL_PRIM_W_ARITY("shared-flvector", flvector, 0, -1, env); 
  GLOBAL_PRIM_W_ARITY("make-shared-flvector", make_flvector, 1, 2, env);
#endif

  p = scheme_make_immed_prim(flvector_length, "flvector-length", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("flvector-length", p, env);

  p = scheme_make_immed_prim(scheme_checked_flvector_ref,
                             "flvector-ref",
                             2, 2);
  if (scheme_can_inline_fp_op())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  scheme_add_global_constant("flvector-ref", p, env);

  p = scheme_make_immed_prim(scheme_checked_flvector_set,
                             "flvector-set!",
                             3, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_NARY_INLINED;
  scheme_add_global_constant("flvector-set!", p, env);

  scheme_add_global_constant("fxvector",
                             scheme_make_prim_w_arity(fxvector,
                                                      "fxvector",
                                                      0, -1),
			     env);
  scheme_add_global_constant("fxvector?",
                             scheme_make_folding_prim(fxvector_p,
                                                      "fxvector?",
                                                      1, 1, 1),
			     env);
  scheme_add_global_constant("make-fxvector",
                             scheme_make_immed_prim(make_fxvector,
                                                    "make-fxvector",
                                                    1, 2),
			     env);

#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  GLOBAL_PRIM_W_ARITY("shared-fxvector", shared_fxvector, 0, -1, env);
  GLOBAL_PRIM_W_ARITY("make-shared-fxvector", make_shared_fxvector, 1, 2, env);
#else
  GLOBAL_PRIM_W_ARITY("shared-fxvector", fxvector, 0, -1, env); 
  GLOBAL_PRIM_W_ARITY("make-shared-fxvector", make_fxvector, 1, 2, env);
#endif

  p = scheme_make_immed_prim(fxvector_length, "fxvector-length", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("fxvector-length", p, env);

  p = scheme_make_immed_prim(scheme_checked_fxvector_ref,
                             "fxvector-ref",
                             2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  scheme_add_global_constant("fxvector-ref", p, env);

  p = scheme_make_immed_prim(scheme_checked_fxvector_set,
                             "fxvector-set!",
                             3, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_NARY_INLINED;
  scheme_add_global_constant("fxvector-set!", p, env);

  p = scheme_make_folding_prim(integer_to_fl, "->fl", 1, 1, 1);
  if (scheme_can_inline_fp_op())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  scheme_add_global_constant("->fl", p, env);

  p = scheme_make_folding_prim(fl_to_integer, "fl->exact-integer", 1, 1, 1);
  if (scheme_can_inline_fp_comp())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  scheme_add_global_constant("fl->exact-integer", p, env);


  p = scheme_make_folding_prim(fx_and, "fxand", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  scheme_add_global_constant("fxand", p, env);

  p = scheme_make_folding_prim(fx_or, "fxior", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  scheme_add_global_constant("fxior", p, env);

  p = scheme_make_folding_prim(fx_xor, "fxxor", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  scheme_add_global_constant("fxxor", p, env);

  p = scheme_make_folding_prim(fx_not, "fxnot", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("fxnot", p, env);

  p = scheme_make_folding_prim(fx_lshift, "fxlshift", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  scheme_add_global_constant("fxlshift", p, env);

  p = scheme_make_folding_prim(fx_rshift, "fxrshift", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  scheme_add_global_constant("fxrshift", p, env);

  p = scheme_make_folding_prim(fx_to_fl, "fx->fl", 1, 1, 1);
  if (scheme_can_inline_fp_op())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  scheme_add_global_constant("fx->fl", p, env);

  p = scheme_make_folding_prim(fl_to_fx, "fl->fx", 1, 1, 1);
  if (scheme_can_inline_fp_comp())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  scheme_add_global_constant("fl->fx", p, env);


  p = scheme_make_folding_prim(fl_truncate, "fltruncate", 1, 1, 1);
  if (scheme_can_inline_fp_op())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  scheme_add_global_constant("fltruncate", p, env);

  p = scheme_make_folding_prim(fl_round, "flround", 1, 1, 1);
  if (scheme_can_inline_fp_op())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  scheme_add_global_constant("flround", p, env);

  p = scheme_make_folding_prim(fl_ceiling, "flceiling", 1, 1, 1);
  if (scheme_can_inline_fp_op())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  scheme_add_global_constant("flceiling", p, env);

  p = scheme_make_folding_prim(fl_floor, "flfloor", 1, 1, 1);
  if (scheme_can_inline_fp_op())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  scheme_add_global_constant("flfloor", p, env);

  p = scheme_make_folding_prim(fl_sin, "flsin", 1, 1, 1);
  if (scheme_can_inline_fp_op())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  scheme_add_global_constant("flsin", p, env);

  p = scheme_make_folding_prim(fl_cos, "flcos", 1, 1, 1);
  if (scheme_can_inline_fp_op())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  scheme_add_global_constant("flcos", p, env);

  p = scheme_make_folding_prim(fl_tan, "fltan", 1, 1, 1);
  if (scheme_can_inline_fp_op())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  scheme_add_global_constant("fltan", p, env);

  p = scheme_make_folding_prim(fl_asin, "flasin", 1, 1, 1);
  if (scheme_can_inline_fp_op())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  scheme_add_global_constant("flasin", p, env);

  p = scheme_make_folding_prim(fl_acos, "flacos", 1, 1, 1);
  if (scheme_can_inline_fp_op())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  scheme_add_global_constant("flacos", p, env);

  p = scheme_make_folding_prim(fl_atan, "flatan", 1, 1, 1);
  if (scheme_can_inline_fp_op())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  scheme_add_global_constant("flatan", p, env);

  p = scheme_make_folding_prim(fl_log, "fllog", 1, 1, 1);
  if (scheme_can_inline_fp_op())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  scheme_add_global_constant("fllog", p, env);

  p = scheme_make_folding_prim(fl_exp, "flexp", 1, 1, 1);
  if (scheme_can_inline_fp_op())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  scheme_add_global_constant("flexp", p, env);

  p = scheme_make_folding_prim(scheme_checked_make_rectangular, "make-flrectangular", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  scheme_add_global_constant("make-flrectangular", p, env);

  p = scheme_make_folding_prim(scheme_checked_real_part, "flreal-part", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("flreal-part", p, env);

  p = scheme_make_folding_prim(scheme_checked_imag_part, "flimag-part", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("flimag-part", p, env);
}

void scheme_init_unsafe_number(Scheme_Env *env)
{
  Scheme_Object *p;

  p = scheme_make_folding_prim(unsafe_fx_and, "unsafe-fxand", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_BINARY_INLINED
                                | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_add_global_constant("unsafe-fxand", p, env);

  p = scheme_make_folding_prim(unsafe_fx_or, "unsafe-fxior", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_BINARY_INLINED
                                | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_add_global_constant("unsafe-fxior", p, env);

  p = scheme_make_folding_prim(unsafe_fx_xor, "unsafe-fxxor", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_BINARY_INLINED
                                | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_add_global_constant("unsafe-fxxor", p, env);

  p = scheme_make_folding_prim(unsafe_fx_not, "unsafe-fxnot", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_UNARY_INLINED
                                | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_add_global_constant("unsafe-fxnot", p, env);

  p = scheme_make_folding_prim(unsafe_fx_lshift, "unsafe-fxlshift", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_BINARY_INLINED
                                | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_add_global_constant("unsafe-fxlshift", p, env);

  p = scheme_make_folding_prim(unsafe_fx_rshift, "unsafe-fxrshift", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_BINARY_INLINED
                                | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_add_global_constant("unsafe-fxrshift", p, env);

  p = scheme_make_folding_prim(unsafe_fx_to_fl, "unsafe-fx->fl", 1, 1, 1);
  if (scheme_can_inline_fp_op())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL;
  scheme_add_global_constant("unsafe-fx->fl", p, env);

  p = scheme_make_folding_prim(unsafe_fl_to_fx, "unsafe-fl->fx", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNARY_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL;
  scheme_add_global_constant("unsafe-fl->fx", p, env);

  p = scheme_make_immed_prim(fl_ref, "unsafe-f64vector-ref",
                             2, 2);
  if (scheme_can_inline_fp_op())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL;
  scheme_add_global_constant("unsafe-f64vector-ref", p, env);
  
  p = scheme_make_immed_prim(fl_set, "unsafe-f64vector-set!",
                             3, 3);
  if (scheme_can_inline_fp_op())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_NARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  scheme_add_global_constant("unsafe-f64vector-set!", p, env);  

  p = scheme_make_immed_prim(unsafe_flvector_length, "unsafe-flvector-length",
                             1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_UNARY_INLINED
                                | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_add_global_constant("unsafe-flvector-length", p, env);

  p = scheme_make_immed_prim(unsafe_flvector_ref, "unsafe-flvector-ref",
                             2, 2);
  if (scheme_can_inline_fp_op())
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  else
    SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL;
  scheme_add_global_constant("unsafe-flvector-ref", p, env);

  p = scheme_make_immed_prim(unsafe_flvector_set, "unsafe-flvector-set!",
                             3, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_NARY_INLINED;
  scheme_add_global_constant("unsafe-flvector-set!", p, env);

  p = scheme_make_immed_prim(unsafe_fxvector_length, "unsafe-fxvector-length",
                             1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_UNARY_INLINED
                                | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_add_global_constant("unsafe-fxvector-length", p, env);

  p = scheme_make_immed_prim(unsafe_fxvector_ref, "unsafe-fxvector-ref",
                             2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_BINARY_INLINED
                                | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_add_global_constant("unsafe-fxvector-ref", p, env);

  p = scheme_make_immed_prim(unsafe_fxvector_set, "unsafe-fxvector-set!",
                             3, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_NARY_INLINED;
  scheme_add_global_constant("unsafe-fxvector-set!", p, env);

  p = scheme_make_immed_prim(s16_ref, "unsafe-s16vector-ref",
                             2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL;
  scheme_add_global_constant("unsafe-s16vector-ref", p, env);
  
  p = scheme_make_immed_prim(s16_set, "unsafe-s16vector-set!",
                             3, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_NARY_INLINED;
  scheme_add_global_constant("unsafe-s16vector-set!", p, env);  

  p = scheme_make_immed_prim(u16_ref, "unsafe-u16vector-ref",
                             2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_BINARY_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL;
  scheme_add_global_constant("unsafe-u16vector-ref", p, env);
  
  p = scheme_make_immed_prim(u16_set, "unsafe-u16vector-set!",
                             3, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |= SCHEME_PRIM_IS_NARY_INLINED;
  scheme_add_global_constant("unsafe-u16vector-set!", p, env);  

  p = scheme_make_folding_prim(unsafe_make_flrectangular, "unsafe-make-flrectangular", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_BINARY_INLINED
                                | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_add_global_constant("unsafe-make-flrectangular", p, env);

  p = scheme_make_folding_prim(unsafe_flreal_part, "unsafe-flreal-part", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_UNARY_INLINED
                                | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_add_global_constant("unsafe-flreal-part", p, env);

  p = scheme_make_folding_prim(unsafe_flimag_part, "unsafe-flimag-part", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= (SCHEME_PRIM_IS_UNARY_INLINED
                                | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_add_global_constant("unsafe-flimag-part", p, env);
}


Scheme_Object *
scheme_make_integer_value(long i)
{
  Scheme_Object *o = scheme_make_integer(i);
  
  if (SCHEME_INT_VAL(o) == i)
    return o;
  else
    return scheme_make_bignum(i);
}

Scheme_Object *
scheme_make_integer_value_from_unsigned(unsigned long i)
{
  Scheme_Object *o = scheme_make_integer(i);
  
  if ((SCHEME_INT_VAL(o) >= 0)
      && ((unsigned long)SCHEME_INT_VAL(o)) == i)
    return o;
  else
    return scheme_make_bignum_from_unsigned(i);
}

Scheme_Object *scheme_make_integer_value_from_long_long(mzlonglong i)
{
#if defined(SIXTY_FOUR_BIT_INTEGERS)
  return scheme_make_integer_value(i);
#else
  if (i < 0) {
    if (!(((i >> 32) & 0xFFFFFFFF) ^ 0xFFFFFFFF)
	&& (i & 0x80000000)) {
      return scheme_make_integer_value((long)i);
    } else
      return scheme_make_bignum_from_long_long(i);
  } else {
    return scheme_make_integer_value_from_unsigned_long_long(i);
  }
#endif
}

Scheme_Object *scheme_make_integer_value_from_unsigned_long_long(umzlonglong i)
{
#if defined(SIXTY_FOUR_BIT_INTEGERS)
  return scheme_make_integer_value_from_unsigned(i);
#else
  if (!((i >> 32) & 0xFFFFFFFF))
    return scheme_make_integer_value_from_unsigned((long)i);
  else
    return scheme_make_bignum_from_unsigned_long_long(i);
#endif
}

static Scheme_Object * fixnum_expt (long x, long y);

Scheme_Object *
scheme_make_integer_value_from_unsigned_long_halves(unsigned long lowhalf,
						    unsigned long hihalf)
{
#ifdef NO_LONG_LONG_TYPE
  /*  Paste the two halves together by 
      hihalf * (2 ** 32) + lowhalf
      
      There may be a more efficient way to do this, but this way
      does not depend upon the representation of bignums.
  */
  
  return
    scheme_bin_plus
    (scheme_make_integer_value_from_unsigned (lowhalf),
     scheme_bin_mult (scheme_make_integer_value_from_unsigned (hihalf),
		      fixnum_expt (2, 32)));
#else
  umzlonglong v;

  v = ((umzlonglong)lowhalf) | ((umzlonglong)hihalf << 32);

  return scheme_make_integer_value_from_unsigned_long_long(v);
#endif
}

Scheme_Object *
scheme_make_integer_value_from_long_halves(unsigned long lowhalf,
					   unsigned long hihalf)
{
#ifdef NO_LONG_LONG_TYPE
  /* hihalf and lowhalf form the two halves of a 64bit 
     number in 2's complement form.  This means that if the 
     topmost bit in hihalf is set, the number is actually 
     the negative version of the complement plus one.
  */
  
  return (hihalf < 0x80000000L
	  ? scheme_make_integer_value_from_unsigned_long_long (lowhalf, hihalf)
	  : scheme_bin_minus
	  (scheme_make_integer (0),
	   scheme_make_integer_value_from_unsigned_long_long
	   ((lowhalf ^ 0xFFFFFFFFL) + 1,
	    (hihalf  ^ 0xFFFFFFFFL) + (lowhalf == 0))));
#else
  mzlonglong v;

  v = (mzlonglong)lowhalf | ((mzlonglong)hihalf << 32);

  return scheme_make_integer_value_from_long_long(v);
#endif
}


int scheme_get_int_val(Scheme_Object *o, long *v)
{
  if (SCHEME_INTP(o)) {
    *v = SCHEME_INT_VAL(o);
    return 1;
  } else if (SCHEME_BIGNUMP(o))
    return scheme_bignum_get_int_val(o, v);
  else
    return 0;
}

int scheme_get_unsigned_int_val(Scheme_Object *o, unsigned long *v)
{
  if (SCHEME_INTP(o)) {
    long i = SCHEME_INT_VAL(o);
    if (i < 0)
      return 0;
    *v = i;
    return 1;
  } else if (SCHEME_BIGNUMP(o))
    return scheme_bignum_get_unsigned_int_val(o, v);
  else
    return 0;
}

int scheme_get_long_long_val(Scheme_Object *o, mzlonglong *v)
{
  if (SCHEME_INTP(o)) {
    *v = SCHEME_INT_VAL(o);
    return 1;
  } else if (SCHEME_BIGNUMP(o))
    return scheme_bignum_get_long_long_val(o, v);
  else
    return 0;
}

int scheme_get_unsigned_long_long_val(Scheme_Object *o, umzlonglong *v)
{
  if (SCHEME_INTP(o)) {
    long i = SCHEME_INT_VAL(o);
    if (i < 0)
      return 0;
    *v = i;
    return 1;
  } else if (SCHEME_BIGNUMP(o))
    return scheme_bignum_get_unsigned_long_long_val(o, v);
  else
    return 0;
}

int scheme_nonneg_exact_p(Scheme_Object *n)
{
  return ((SCHEME_INTP(n) && (SCHEME_INT_VAL(n) >= 0))
	  || (SCHEME_BIGNUMP(n) && SCHEME_BIGPOS(n)));
}

double scheme_real_to_double(Scheme_Object *r)
{
  if (SCHEME_INTP(r))
    return (double)SCHEME_INT_VAL(r);
  else if (SCHEME_DBLP(r))
    return SCHEME_DBL_VAL(r);
#ifdef MZ_USE_SINGLE_FLOATS
  else if (SCHEME_FLTP(r))
    return SCHEME_FLT_VAL(r);
#endif
  else if (SCHEME_BIGNUMP(r))
    return scheme_bignum_to_double(r);
  else if (SCHEME_RATIONALP(r))
    return scheme_rational_to_double(r);
  else
    return 0.0;
}

XFORM_NONGCING static MZ_INLINE int minus_zero_p(double d)
{
  return (1 / d) < 0;
}

int scheme_minus_zero_p(double d)
{
  return minus_zero_p(d);
}

#ifdef MZ_USE_SINGLE_FLOATS
static int rational_flt_p(float f) {
  return !(MZ_IS_NAN(f)
           || MZ_IS_INFINITY(f));
}
#endif

static int rational_dbl_p(double f) {
  return !(MZ_IS_NAN(f)
           || MZ_IS_INFINITY(f));
}

#ifdef DEFEAT_FP_COMP_OPTIMIZATION
int scheme_both_nan(double a, double b)
{
  /* Called by the MZ_IS_NAN() macro for certain compilers.
     A and B are actually the same FP number, but the compiler
     optimizes (A == A) to TRUE, so we use a function call to
     hide the fact that A and B are the same. */
  return a != b;
}
#endif

#ifdef USE_PALM_INF_TESTS
int scheme_is_pos_inf(double d)
{
  return (d == scheme_infinity_val);
}

int scheme_is_neg_inf(double d)
{
  return (d == scheme_minus_infinity_val);
}

int scheme_is_nan(double d)
{
  return (!(d == d));
}
#endif

Scheme_Object *scheme_make_double(double d)
{
  GC_CAN_IGNORE Scheme_Double *sd;

  if (d == 0.0) {
    if (minus_zero_p(d))
      return scheme_nzerod;
#ifdef NAN_EQUALS_ANYTHING
    else if (MZ_IS_NAN(d))
      return scheme_nan_object;
#endif
    else
      return scheme_zerod;
  }

  sd = (Scheme_Double *)scheme_malloc_small_atomic_tagged(sizeof(Scheme_Double));
  CLEAR_KEY_FIELD(&sd->so);
  sd->so.type = scheme_double_type;
  SCHEME_DBL_VAL(sd) = d;
  return (Scheme_Object *)sd;
}

#ifdef MZ_USE_SINGLE_FLOATS
Scheme_Object *scheme_make_float(float f)
{
  Scheme_Float *sf;

  sf = (Scheme_Float *)scheme_malloc_small_atomic_tagged(sizeof(Scheme_Float));
  CLEAR_KEY_FIELD(&sf->so);
  sf->so.type = scheme_float_type;
  SCHEME_FLT_VAL(sf) = f;
  return (Scheme_Object *)sf;
}
#endif

/* locals */

static Scheme_Object *
number_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  return (SCHEME_NUMBERP(o) ? scheme_true : scheme_false);
}

static Scheme_Object *
complex_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  return (SCHEME_NUMBERP(o) ? scheme_true : scheme_false);
}

static Scheme_Object *
real_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  return (SCHEME_REALP(o) ? scheme_true : scheme_false);
}

static Scheme_Object *
rational_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (SCHEME_FLOATP(o))
    return (rational_dbl_p(SCHEME_FLOAT_VAL(o)) ? scheme_true : scheme_false);
  else
    return (SCHEME_REALP(o) ? scheme_true : scheme_false);
}

int scheme_is_integer(const Scheme_Object *o)
{
  if (SCHEME_INTP(o) || SCHEME_BIGNUMP(o))
    return 1;

  if (SCHEME_FLOATP(o)) {
    double d;
    d = SCHEME_FLOAT_VAL(o);
# ifdef NAN_EQUALS_ANYTHING
    if (MZ_IS_NAN(d))
      return 0;
# endif
    if (MZ_IS_INFINITY(d))
      return 0;
    if (floor(d) == d)
      return 1;
  }

  return 0;
}


static Scheme_Object *
integer_p (int argc, Scheme_Object *argv[])
{
  return scheme_is_integer(argv[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *
exact_integer_p (int argc, Scheme_Object *argv[])
{
  Scheme_Object *n = argv[0];
  if (SCHEME_INTP(n))
    return scheme_true;
  else if (SCHEME_BIGNUMP(n))
    return scheme_true;
  else
    return scheme_false;
}

static Scheme_Object *
exact_nonnegative_integer_p (int argc, Scheme_Object *argv[])
{
  Scheme_Object *n = argv[0];
  if (SCHEME_INTP(n))
    return ((SCHEME_INT_VAL(n) >= 0) ? scheme_true : scheme_false);
  else if (SCHEME_BIGNUMP(n))
    return (SCHEME_BIGPOS(n) ? scheme_true : scheme_false);
  else
    return scheme_false;
}

static Scheme_Object *
exact_positive_integer_p (int argc, Scheme_Object *argv[])
{
  Scheme_Object *n = argv[0];
  if (SCHEME_INTP(n))
    return ((SCHEME_INT_VAL(n) > 0) ? scheme_true : scheme_false);
  else if (SCHEME_BIGNUMP(n))
    return (SCHEME_BIGPOS(n) ? scheme_true : scheme_false);
  else
    return scheme_false;
}

static Scheme_Object *
fixnum_p (int argc, Scheme_Object *argv[])
{
  Scheme_Object *n = argv[0];
  if (SCHEME_INTP(n))
    return scheme_true;
  else
    return scheme_false;
}

static Scheme_Object *
inexact_real_p (int argc, Scheme_Object *argv[])
{
  Scheme_Object *n = argv[0];
  if (SCHEME_FLOATP(n))
    return scheme_true;
  else
    return scheme_false;
}

int scheme_is_exact(const Scheme_Object *n)
{
  if (SCHEME_INTP(n)) {
    return 1;
  } else {
    Scheme_Type type = _SCHEME_TYPE(n);
    if ((type == scheme_bignum_type)
	|| (type == scheme_rational_type))
      return 1;
    else if (type == scheme_complex_type) {
      return scheme_is_complex_exact(n);
    } else if (type == scheme_double_type)
      return 0;
#ifdef MZ_USE_SINGLE_FLOATS
    else if (type == scheme_float_type)
      return 0;
#endif
    else {
      return -1;
    }
  }
}

Scheme_Object *
exact_p (int argc, Scheme_Object *argv[])
{
  int v;
  v = scheme_is_exact(argv[0]);
  if (v < 0) {
    scheme_wrong_type("exact?", "number", 0, argc, argv);
    ESCAPED_BEFORE_HERE;
  }
  return (v ? scheme_true : scheme_false);
}

int scheme_is_inexact(const Scheme_Object *n)
{
  if (SCHEME_INTP(n)) {
    return 0;
  } else {
    Scheme_Type type = _SCHEME_TYPE(n);
    if ((type == scheme_bignum_type)
	|| (type == scheme_rational_type))
      return 0;
    else if (type == scheme_complex_type) {
      return !scheme_is_complex_exact(n);
    } else if (type == scheme_double_type)
      return 1;
#ifdef MZ_USE_SINGLE_FLOATS
    else if (type == scheme_float_type)
      return 1;
#endif
    else {
      return -1;
    }
  }
}

Scheme_Object *
scheme_inexact_p (int argc, Scheme_Object *argv[])
{
  int v;
  v = scheme_is_inexact(argv[0]);
  if (v < 0) {
    scheme_wrong_type("inexact?", "number", 0, argc, argv);
    ESCAPED_BEFORE_HERE;
  }
  return (v ? scheme_true : scheme_false);
}


Scheme_Object *
scheme_odd_p (int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];

  if (SCHEME_INTP(v))
    return (SCHEME_INT_VAL(v) & 0x1) ? scheme_true : scheme_false;
  if (SCHEME_BIGNUMP(v))
    return (SCHEME_BIGDIG(v)[0] & 0x1) ? scheme_true : scheme_false;
  
  if (scheme_is_integer(v)) {
    double d = SCHEME_FLOAT_VAL(v);
    if (MZ_IS_INFINITY(d))
      return scheme_true;
    return (fmod(d, 2.0) == 0.0) ? scheme_false : scheme_true;
  }

  NEED_INTEGER(odd?);

  ESCAPED_BEFORE_HERE;
}

static Scheme_Object *
even_p (int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];

  if (SCHEME_INTP(v))
    return (SCHEME_INT_VAL(v) & 0x1) ? scheme_false : scheme_true;
  if (SCHEME_BIGNUMP(v))
    return (SCHEME_BIGDIG(v)[0] & 0x1) ? scheme_false : scheme_true;

  if (scheme_is_integer(v)) {
    double d = SCHEME_FLOAT_VAL(v);
    if (MZ_IS_INFINITY(d))
      return scheme_true;
    return (fmod(d, 2.0) == 0.0) ? scheme_true : scheme_false;
  }

  NEED_INTEGER(even?);

  ESCAPED_BEFORE_HERE;
}

static Scheme_Object *bin_lcm (Scheme_Object *n1, Scheme_Object *n2);

static Scheme_Object *int_abs(Scheme_Object *v)
{
  if (scheme_is_negative(v))
    return scheme_bin_minus(scheme_make_integer(0), v);
  else
    return v;
}

GEN_NARY_OP(static, gcd, "gcd", scheme_bin_gcd, 0, scheme_is_integer, "integer", int_abs)
GEN_NARY_OP(static, lcm, "lcm", bin_lcm, 1, scheme_is_integer, "integer", int_abs)

Scheme_Object *
scheme_bin_gcd (const Scheme_Object *n1, const Scheme_Object *n2)
{
  if (SCHEME_INTP(n1) && SCHEME_INTP(n2)) {
    long i1, i2, a, b, r;

    i1 = SCHEME_INT_VAL(n1);
    i2 = SCHEME_INT_VAL(n2);
    if (i1 < 0)
      i1 = -i1;
    if (i2 < 0)
      i2 = -i2;
    if (i1 > i2) {
      a = i1;
      b = i2;
    } else {
      a = i2;
      b = i1;
    }
    
    while (b > 0) {
      r = a % b;
      a = b;
      b = r;
    }
    return (scheme_make_integer(a));
  } else if (SCHEME_FLOATP(n1) || SCHEME_FLOATP(n2)) {
    double i1, i2, a, b, r;
#ifdef MZ_USE_SINGLE_FLOATS
# ifdef USE_SINGLE_FLOATS_AS_DEFAULT
    int was_single = !(SCHEME_DBLP(n1) || SCHEME_DBLP(n2));
# else
    int was_single = (SCHEME_FLTP(n1) || SCHEME_FLTP(n2));
# endif
#endif

    if (SCHEME_INTP(n1))
      i1 = SCHEME_INT_VAL(n1);
    else if (SCHEME_FLOATP(n1))
      i1 = SCHEME_FLOAT_VAL(n1);
    else
      i1 = scheme_bignum_to_double(n1);

    if (SCHEME_INTP(n2))
      i2 = SCHEME_INT_VAL(n2);
    else if (SCHEME_FLOATP(n2))
      i2 = SCHEME_FLOAT_VAL(n2);
    else
      i2 = scheme_bignum_to_double(n2);

    if (i1 < 0)
      i1 = -i1;
    if (i2 < 0)
      i2 = -i2;
    if (i1 > i2) {
      a = i1;
      b = i2;
    } else {
      a = i2;
      b = i1;
    }

#if 0
    /* Shouldn't happen, since +nan.0 isn't an integer */
    if (MZ_IS_NAN(a) || MZ_IS_NAN(b))
      return nan_object;
#endif
    if (MZ_IS_POS_INFINITY(a)) {
#ifdef MZ_USE_SINGLE_FLOATS
      if (was_single)
	return scheme_make_float((float)b);
#endif
      return scheme_make_double(b);
    }
    
    while (b > 0) {
      r = fmod(a, b);
      a = b;
      b = r;
    }

#ifdef MZ_USE_SINGLE_FLOATS
    if (was_single)
      return scheme_make_float((float)a);
#endif

    return scheme_make_double(a);
  } else {
    n1 = scheme_to_bignum(n1);
    n2 = scheme_to_bignum(n2);

    if (!SCHEME_BIGPOS(n1))
      n1 = scheme_bignum_negate(n1);
    if (!SCHEME_BIGPOS(n2))
      n2 = scheme_bignum_negate(n2);

    return scheme_bignum_gcd(n1, n2);
  }
}

static Scheme_Object *
bin_lcm (Scheme_Object *n1, Scheme_Object *n2)
{
  Scheme_Object *d, *ret;

  d = scheme_bin_gcd(n1, n2);

  if (scheme_is_zero(d))
    return d;
  
  ret = scheme_bin_mult(n1, scheme_bin_quotient(n2, d));

  return scheme_abs(1, &ret);
}

static Scheme_Object *
floor_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  Scheme_Type t;

  if (SCHEME_INTP(o))
    return o;
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type) {
    float d = SCHEME_FLT_VAL(o);
    return scheme_make_float(floor(d));
  }
#endif
  if (t == scheme_double_type) {
    double d = SCHEME_DBL_VAL(o);
    return scheme_make_double(floor(d));
  }
  if (t == scheme_bignum_type)
    return o;
  if (t == scheme_rational_type)
    return scheme_rational_floor(o);

  NEED_REAL(floor);

  ESCAPED_BEFORE_HERE;
}

static Scheme_Object *
ceiling (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  Scheme_Type t;

  if (SCHEME_INTP(o))
    return o;
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type) {
    float d = SCHEME_FLT_VAL(o);
    return scheme_make_float(ceil(d));
  }
#endif
  if (t == scheme_double_type) {
    double d = SCHEME_DBL_VAL(o);
    return scheme_make_double(ceil(d));
  }
  if (t == scheme_bignum_type)
    return o;
  if (t == scheme_rational_type)
    return scheme_rational_ceiling(o);

  NEED_REAL(ceiling);

  ESCAPED_BEFORE_HERE;
}

XFORM_NONGCING static double SCH_TRUNC(double v)
{
  if (v > 0)
    v = floor(v);
  else
    v = ceil(v);  
  return v;
}

static Scheme_Object *
sch_truncate (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  Scheme_Type t;

  if (SCHEME_INTP(o))
    return o;
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type) {
    float v = SCHEME_FLT_VAL(o);
    if (v > 0)
      v = floor(v);
    else
      v = ceil(v);
    return scheme_make_float(v);
  }
#endif
  if (t == scheme_double_type) {
    double v;
    v = SCH_TRUNC(SCHEME_DBL_VAL(o));
    return scheme_make_double(v);
  }
  if (t == scheme_bignum_type)
    return o;
  if (t == scheme_rational_type)
    return scheme_rational_truncate(o);

  NEED_REAL(truncate);

  ESCAPED_BEFORE_HERE;
}

XFORM_NONGCING static double SCH_ROUND(double d)
{
  double i, frac;
  int invert;

#ifdef FMOD_CAN_RETURN_POS_ZERO
  if ((d == 0.0) && minus_zero_p(d))
    return d;
#endif

  if (d < 0) {
    d = -d;
    invert = 1;
  } else
    invert = 0;

  frac = modf(d, &i);
  if (frac < 0.5)
    d = i;
  else if (frac > 0.5)
    d = i + 1;
  else if (fmod(i, 2.0) != 0.0)
    d = i + 1;
  else
    d = i;

  if (invert)
    d = -d;

  return d;
}

static Scheme_Object *
sch_round (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  Scheme_Type t;

  if (SCHEME_INTP(o))
    return o;
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type) {
    double d = SCHEME_FLT_VAL(o);
    double i, frac;
    int invert;

    if (d < 0) {
      d = -d;
      invert = 1;
    } else
      invert = 0;

    frac = modf(d, &i);
    if (frac < 0.5)
      d = i;
    else if (frac > 0.5)
      d = i + 1;
    else if (fmod(i, 2.0) != 0.0)
      d = i + 1;
    else
      d = i;

    if (invert)
      d = -d;

    return scheme_make_float((float)d);
  }
#endif
  if (t == scheme_double_type) {
    double d;
    d = SCH_ROUND(SCHEME_DBL_VAL(o));
    return scheme_make_double(d);
  }
  if (t == scheme_bignum_type)
    return o;
  if (t == scheme_rational_type)
    return scheme_rational_round(o);

  NEED_REAL(round);

  ESCAPED_BEFORE_HERE;
}

double scheme_double_truncate(double x) { return SCH_TRUNC(x); }
double scheme_double_round(double x) { return SCH_ROUND(x); }
double scheme_double_floor(double x) { return floor(x); }
double scheme_double_ceiling(double x) { return ceil(x); }

#ifdef MZ_USE_SINGLE_FLOATS

#define TO_FLOAT_VAL scheme_get_val_as_float

float TO_FLOAT_VAL(const Scheme_Object *n)
{
  Scheme_Type t;

  if (SCHEME_INTP(n))
    return (float)SCHEME_INT_VAL(n);
  t = _SCHEME_TYPE(n);
  if (t == scheme_float_type)
    return SCHEME_FLT_VAL(n);
  if (t == scheme_double_type)
    return SCHEME_DBL_VAL(n);
  if (t == scheme_bignum_type)
    return scheme_bignum_to_float(n);
  if (t == scheme_rational_type)
    return scheme_rational_to_float(n);
  return 0.0f;
}

static Scheme_Object *TO_FLOAT(const Scheme_Object *n)
{
  if (SCHEME_FLTP(n))
    return (Scheme_Object *)n;
  else
    return scheme_make_float(TO_FLOAT_VAL(n));
}
#endif

#define TO_DOUBLE_VAL scheme_get_val_as_double

#ifdef USE_SINGLE_FLOATS_AS_DEFAULT

double TO_DOUBLE_VAL(const Scheme_Object *n)
{
  Scheme_Type t;

  if (SCHEME_INTP(n))
    return (double)SCHEME_INT_VAL(n);
  t = _SCHEME_TYPE(n);
  if (t == scheme_float_type)
    return SCHEME_FLT_VAL(n);
  if (t == scheme_double_type)
    return SCHEME_DBL_VAL(n);
  if (t == scheme_bignum_type)
    return scheme_bignum_to_double(n);
  if (t == scheme_rational_type)
    return scheme_rational_to_double(n);
  return 0.0;
}

Scheme_Object *scheme_TO_DOUBLE(const Scheme_Object *n)
{
  if (SCHEME_DBLP(n))
    return (Scheme_Object *)n;
  else
    return scheme_make_double(TO_DOUBLE_VAL(n));
}

#else

Scheme_Object *scheme_TO_DOUBLE(const Scheme_Object *n)
{
  return scheme_exact_to_inexact(1, (Scheme_Object **)&n);
}

double TO_DOUBLE_VAL(const Scheme_Object *n)
{
  return SCHEME_DBL_VAL(scheme_TO_DOUBLE(n));
}

#endif

#define TO_DOUBLE scheme_TO_DOUBLE

Scheme_Object *scheme_to_bignum(const Scheme_Object *o)
{
  if (SCHEME_INTP(o))
    return scheme_make_bignum(SCHEME_INT_VAL(o));
  else
    return (Scheme_Object *)o;
}

static Scheme_Object *get_frac(char *name, int low_p, 
			       int argc, Scheme_Object *argv[])
{
  Scheme_Object *n = argv[0], *orig;

  orig = n;

  if (SCHEME_FLOATP(n)) {
    double d = SCHEME_FLOAT_VAL(n);
    
    if (MZ_IS_NAN(d)
        || MZ_IS_INFINITY(d)) {
      scheme_wrong_type(name, "rational number", 0, argc, argv);
      ESCAPED_BEFORE_HERE;
    }
    
#ifdef MZ_USE_SINGLE_FLOATS
    if (SCHEME_FLTP(n))
      n = scheme_rational_from_float((float)d);
    else
#endif
      n = scheme_rational_from_double(d);
  }
  
  if (SCHEME_INTP(n) || SCHEME_BIGNUMP(n))
    n = low_p ? scheme_make_integer(1) : n;
  else if (SCHEME_RATIONALP(n)) {
    if (low_p)
      n = scheme_rational_denominator(n);
    else
      n = scheme_rational_numerator(n);
  } else {
    scheme_wrong_type(name, "rational number", 0, argc, argv);
    ESCAPED_BEFORE_HERE;   
  }
  
  if (SCHEME_DBLP(orig))
    return TO_DOUBLE(n);
#ifdef MZ_USE_SINGLE_FLOATS
  if (SCHEME_FLTP(orig))
    return TO_FLOAT(n);
#endif
  else
    return n;
}

static Scheme_Object *un_exp(Scheme_Object *o);
static Scheme_Object *un_log(Scheme_Object *o);

static Scheme_Object *un_exp(Scheme_Object *o)
{
  return exp_prim(1, &o);
}

static Scheme_Object *un_log(Scheme_Object *o)
{
  return log_prim(1, &o);
}

static Scheme_Object *numerator(int argc, Scheme_Object *argv[])
{
  return get_frac("numerator", 0, argc, argv);
}

static Scheme_Object *denominator(int argc, Scheme_Object *argv[])
{
  return get_frac("denominator", 1, argc, argv);
}

static Scheme_Object *complex_exp(Scheme_Object *c);

static Scheme_Object *complex_exp(Scheme_Object *c)
{
  Scheme_Object *r = _scheme_complex_real_part(c);
  Scheme_Object *i = _scheme_complex_imaginary_part(c);
  Scheme_Object *cos_a, *sin_a;

  r = exp_prim(1, &r);
  cos_a = cos_prim(1, &i);
  sin_a = sin_prim(1, &i);

  return scheme_bin_mult(r, scheme_bin_plus(cos_a, scheme_bin_mult(sin_a, scheme_plus_i)));
}

static Scheme_Object *complex_log(Scheme_Object *c);

static Scheme_Object *complex_log(Scheme_Object *c)
{
  Scheme_Object *m, *theta;

  m = magnitude(1, &c);
  theta = angle(1, &c);

  return scheme_bin_plus(log_prim(1, &m), scheme_bin_mult(scheme_plus_i, theta));
}

static Scheme_Object *bignum_log(Scheme_Object *b)
{
  Scheme_Object *rem;
  int d_count = 0;
  double d;

  if (!SCHEME_BIGPOS(b))
    return complex_log(b);

  /* Assume that each digit is no bigger than 64 bits: */
  while (SCHEME_BIGLEN(b) >= 15) {
    b = scheme_integer_sqrt_rem(b, &rem);
    d_count++;
  }

  if (SCHEME_BIGNUMP(b))
    d = scheme_bignum_to_double(b);
  else
    d = SCHEME_INT_VAL(b);
  d = log(d);

  while (d_count--) {
    d = d * 2;
  }

  return scheme_make_double(d);
}

static Scheme_Object *complex_sin(Scheme_Object *c);

static Scheme_Object *complex_sin(Scheme_Object *c)
{
  Scheme_Object *i_c;

  i_c = scheme_bin_mult(c, scheme_plus_i);
  
  return scheme_bin_div(scheme_bin_minus(un_exp(i_c),
					 un_exp(scheme_bin_minus(zeroi, i_c))),
			scheme_bin_mult(scheme_make_integer(2), scheme_plus_i));
}

static Scheme_Object *complex_cos(Scheme_Object *c);

static Scheme_Object *complex_cos(Scheme_Object *c)
{
  Scheme_Object *i_c;

  i_c = scheme_bin_mult(c, scheme_plus_i);
  
  return scheme_bin_div(scheme_bin_plus(un_exp(i_c),
					un_exp(scheme_bin_minus(zeroi, i_c))),
			scheme_make_integer(2));
}

static Scheme_Object *complex_tan(Scheme_Object *c);

static Scheme_Object *complex_tan(Scheme_Object *c)
{
  return scheme_bin_div(complex_sin(c), complex_cos(c));
}

static Scheme_Object *complex_asin(Scheme_Object *c);
static Scheme_Object *complex_atan(Scheme_Object *c);

static Scheme_Object *complex_asin(Scheme_Object *c)
{
  Scheme_Object *one_minus_c_sq, *sqrt_1_minus_c_sq;

  one_minus_c_sq = scheme_bin_minus(scheme_make_integer(1),
				    scheme_bin_mult(c, c));
  sqrt_1_minus_c_sq = scheme_sqrt(1, &one_minus_c_sq);
  return scheme_bin_mult(scheme_make_integer(2),
                         complex_atan(scheme_bin_div(c,
                                                     scheme_bin_plus(scheme_make_integer(1),
                                                                     sqrt_1_minus_c_sq))));
}

static Scheme_Object *complex_acos(Scheme_Object *c);

static Scheme_Object *complex_acos(Scheme_Object *c)
{
  Scheme_Object *a, *r;
  a = complex_asin(c);
  if (scheme_is_zero(_scheme_complex_imaginary_part(c))
      && (scheme_bin_gt(_scheme_complex_real_part(c), scheme_make_integer(1))
          || scheme_bin_lt(_scheme_complex_real_part(c), scheme_make_integer(-1)))) {
    /* Make sure real part is 0 or pi */
    if (scheme_is_negative(_scheme_complex_real_part(c)))
      r = scheme_pi;
    else
      r = scheme_make_integer(0);
    return scheme_make_complex(r, scheme_bin_minus(scheme_make_integer(0),
                                                   _scheme_complex_imaginary_part(a)));
  } else {
    return scheme_bin_minus(scheme_half_pi, a);
  }
}

static Scheme_Object *complex_atan(Scheme_Object *c)
{
  if (scheme_complex_eq(c, scheme_plus_i) || scheme_complex_eq(c, scheme_minus_i))
    return scheme_minus_inf_object;

  return scheme_bin_mult(scheme_plus_i,
			 scheme_bin_mult(
#ifdef USE_SINGLE_FLOATS_AS_DEFAULT
					 scheme_make_float(0.5)
#else
					 scheme_make_double(0.5)
#endif
					 ,
					 un_log(scheme_bin_div(scheme_bin_plus(scheme_plus_i, c),
							       scheme_bin_plus(scheme_plus_i, 
									       scheme_bin_minus(zeroi, c))))));
}

#define GEN_ZERO_IS_ZERO() if (o == zeroi) return zeroi;
#define GEN_ZERO_IS_ONE() if (o == zeroi) return scheme_make_integer(1);
#define GEN_ONE_IS_ZERO() if (o == scheme_exact_one) return zeroi;
#define GEN_ONE_IS_ZERO_AND_ZERO_IS_ERR() if (o == scheme_exact_one) return zeroi; else if (o == zeroi) scheme_raise_exn(MZEXN_FAIL_CONTRACT_DIVIDE_BY_ZERO, "log: undefined for 0");
#define GEN_ZERO_IS_HALF_PI() if (o == zeroi) return scheme_half_pi;

#define NEVER_RESORT_TO_COMPLEX(d) 0
#define NEGATIVE_USES_COMPLEX(d) d < 0.0
#define OVER_ONE_MAG_USES_COMPLEX(d) (d > 1.0) || (d < -1.0)

#ifdef TRIG_ZERO_NEEDS_SIGN_CHECK
#define MK_SCH_TRIG(SCH_TRIG, c_trig) static double SCH_TRIG(double d) { if (d == 0.0) return d; else return c_trig(d); }
MK_SCH_TRIG(SCH_TAN, tan)
MK_SCH_TRIG(SCH_SIN, sin)
MK_SCH_TRIG(SCH_ASIN, asin)
# define SCH_COS cos
#else
# ifdef SIN_COS_NEED_DEOPTIMIZE
#  pragma optimize("g", off)
#  define MK_SCH_TRIG(SCH_TRIG, c_trig) static double SCH_TRIG(double d) { return c_trig(d); }
MK_SCH_TRIG(SCH_SIN, sin)
MK_SCH_TRIG(SCH_COS, cos)
MK_SCH_TRIG(SCH_TAN, tan)
#  pragma optimize("g", on)
# else
#  define SCH_SIN sin
#  define SCH_COS cos
#  define SCH_TAN tan
# endif
# define SCH_ASIN asin
#endif

static double SCH_ATAN(double v)
{
#ifdef TRIG_ZERO_NEEDS_SIGN_CHECK
  if (v == 0.0) {
    /* keep v the same */
  } else
#endif
    v = atan(v);
  return v;
}

#ifdef LOG_ZERO_ISNT_NEG_INF
static double SCH_LOG(double d) { if (d == 0.0) return scheme_minus_infinity_val; else return log(d); }
#else
# define SCH_LOG log
#endif
#define BIGNUM_LOG(o) return bignum_log(o);

double scheme_double_sin(double x) { return SCH_SIN(x); }
double scheme_double_cos(double x) { return SCH_COS(x); }
double scheme_double_tan(double x) { return SCH_TAN(x); }
double scheme_double_asin(double x) { return SCH_ASIN(x); }
double scheme_double_acos(double x) { return acos(x); }
double scheme_double_atan(double x) { return SCH_ATAN(x); }
double scheme_double_log(double x) { return SCH_LOG(x); }
double scheme_double_exp(double x) { return exp(x); }

static Scheme_Object *scheme_inf_plus_pi()
{
  return scheme_make_complex(scheme_inf_object, scheme_pi);
}

#ifdef MZ_USE_SINGLE_FLOATS
static Scheme_Object *scheme_single_inf_plus_pi()
{
  return scheme_make_complex(scheme_single_inf_object, scheme_single_pi);
}
#endif

GEN_UNARY_OP(exp_prim, exp, exp, scheme_inf_object, scheme_single_inf_object, scheme_zerod, scheme_zerof, scheme_nan_object, scheme_single_nan_object, complex_exp, GEN_ZERO_IS_ONE, NEVER_RESORT_TO_COMPLEX, BIGNUMS_AS_DOUBLES)
GEN_UNARY_OP(log_prim, log, SCH_LOG, scheme_inf_object, scheme_single_inf_object, scheme_inf_plus_pi(), scheme_single_inf_plus_pi(), scheme_nan_object, scheme_single_nan_object, complex_log, GEN_ONE_IS_ZERO_AND_ZERO_IS_ERR, NEGATIVE_USES_COMPLEX, BIGNUM_LOG)
GEN_UNARY_OP(sin_prim, sin, SCH_SIN, scheme_nan_object, scheme_single_nan_object, scheme_nan_object, scheme_single_nan_object, scheme_nan_object, scheme_single_nan_object, complex_sin, GEN_ZERO_IS_ZERO, NEVER_RESORT_TO_COMPLEX, BIGNUMS_AS_DOUBLES)
GEN_UNARY_OP(cos_prim, cos, SCH_COS, scheme_nan_object, scheme_single_nan_object, scheme_nan_object, scheme_single_nan_object, scheme_nan_object, scheme_single_nan_object, complex_cos, GEN_ZERO_IS_ONE, NEVER_RESORT_TO_COMPLEX, BIGNUMS_AS_DOUBLES)
GEN_UNARY_OP(tan_prim, tan, SCH_TAN, scheme_nan_object, scheme_single_nan_object, scheme_nan_object, scheme_single_nan_object, scheme_nan_object, scheme_single_nan_object, complex_tan, GEN_ZERO_IS_ZERO, NEVER_RESORT_TO_COMPLEX, BIGNUMS_AS_DOUBLES)
GEN_UNARY_OP(asin_prim, asin, SCH_ASIN, scheme_nan_object, scheme_single_nan_object, scheme_nan_object, scheme_single_nan_object, scheme_nan_object, scheme_single_nan_object, complex_asin, GEN_ZERO_IS_ZERO, OVER_ONE_MAG_USES_COMPLEX, BIGNUMS_AS_DOUBLES)
GEN_UNARY_OP(acos_prim, acos, acos, scheme_nan_object, scheme_single_nan_object, scheme_nan_object, scheme_single_nan_object, scheme_nan_object, scheme_single_nan_object, complex_acos, GEN_ONE_IS_ZERO, OVER_ONE_MAG_USES_COMPLEX, BIGNUMS_AS_DOUBLES)

static Scheme_Object *
atan_prim (int argc, Scheme_Object *argv[])
{
  double v;
  Scheme_Object *n1;
#ifdef MZ_USE_SINGLE_FLOATS
# ifdef USE_SINGLE_FLOATS_AS_DEFAULT
  int dbl = 0;
# define MZ_USE_SINGLE !dbl
# else
  int single = 0;
# define MZ_USE_SINGLE single == 2
#endif
#endif

  n1 = argv[0];

  if (SCHEME_INTP(n1))
    v = SCHEME_INT_VAL(n1);
#ifdef MZ_USE_SINGLE_FLOATS
  else if (SCHEME_FLTP(n1)) {
    v = SCHEME_FLT_VAL(n1);
# ifndef USE_SINGLE_FLOATS_AS_DEFAULT
    single++;
# endif
  }
#endif
  else if (SCHEME_DBLP(n1)) {
# ifdef USE_SINGLE_FLOATS_AS_DEFAULT
    dbl++;
# endif
    v = SCHEME_DBL_VAL(n1);
  } else if (SCHEME_BIGNUMP(n1))
    v = scheme_bignum_to_double(n1);
  else if (SCHEME_RATIONALP(n1))
    v = scheme_rational_to_double(n1);
  else if (SCHEME_COMPLEXP(n1)) {
    if (argc > 1) {
      scheme_wrong_type("atan (with two arguments)", REAL_NUMBER_STR, 0, argc, argv);
      ESCAPED_BEFORE_HERE;
    } else
      return complex_atan(n1);
  } else {
    NEED_NUMBER(atan);
    ESCAPED_BEFORE_HERE;
  }

  if (argc == 2) {
    double v2;
    Scheme_Object *n2;
    
    n2 = argv[1];

    if (n1 == zeroi) {
      if (n2 == zeroi) {
        scheme_raise_exn(MZEXN_FAIL_CONTRACT_DIVIDE_BY_ZERO,
                         "atan: undefined for 0 and 0");
        ESCAPED_BEFORE_HERE;
      }
      if ((SCHEME_INTP(n2) && (SCHEME_INT_VAL(n2) > 0))
          || (SCHEME_BIGNUMP(n2) && (SCHEME_BIGPOS(n2))))
        return zeroi;
    }

    if (SCHEME_INTP(n2))
      v2 = SCHEME_INT_VAL(n2);
#ifdef MZ_USE_SINGLE_FLOATS
    else if (SCHEME_FLTP(n2)) {
      v2 = SCHEME_FLT_VAL(n2);
# ifndef USE_SINGLE_FLOATS_AS_DEFAULT
      single++;
# endif
    }
#endif
    else if (SCHEME_DBLP(n2)) {
# ifdef USE_SINGLE_FLOATS_AS_DEFAULT
      dbl++;
# endif
      v2 = SCHEME_DBL_VAL(n2);
    } else if (SCHEME_BIGNUMP(n2))
      v2 = scheme_bignum_to_double(n2);
    else if (SCHEME_RATIONALP(n2))
      v2 = scheme_rational_to_double(n2);
    else {
      scheme_wrong_type("atan", REAL_NUMBER_STR, 1, argc, argv);
      ESCAPED_BEFORE_HERE;
    }

    if ((v == 0.0) && (v2 == 0.0)) {
#ifdef MZ_USE_SINGLE_FLOATS
      if (MZ_USE_SINGLE)
	return scheme_zerof;
#endif      
      return scheme_zerod;
    }

#ifdef ATAN2_DOESNT_WORK_WITH_INFINITIES
    if (MZ_IS_INFINITY(v) && MZ_IS_INFINITY(v2)) {
      v = MZ_IS_POS_INFINITY(v) ? 1.0 : -1.0;
      v2 = MZ_IS_POS_INFINITY(v2) ? 1.0 : -1.0;
    }
#endif
#ifdef ATAN2_DOESNT_WORK_WITH_NAN
    if (MZ_IS_NAN(v) || MZ_IS_NAN(v2))
      return scheme_nan_object;
#endif

    v = atan2(v, v2);
  } else {
    if (argv[0] == zeroi)
      return zeroi;

    v = SCH_ATAN(v);

#ifdef MZ_USE_SINGLE_FLOATS
# ifndef USE_SINGLE_FLOATS_AS_DEFAULT
    single++;
# endif
#endif    
  }

#ifdef MZ_USE_SINGLE_FLOATS
  if (MZ_USE_SINGLE)
    return scheme_make_float((float)v);
#endif

  return scheme_make_double(v);

#undef MZ_USE_SINGLE
}

#ifdef NEED_TO_DEFINE_MATHERR
int _RTLENTRY _EXPFUNC _matherr(struct exception* e)
{
   e->retval=1.0;
   return 1;
}
#endif

Scheme_Object *scheme_sqrt (int argc, Scheme_Object *argv[])
{
  int imaginary = 0;
  Scheme_Object *n;
  
  n = argv[0];

  if (SCHEME_COMPLEXP(n))
    return scheme_complex_sqrt(n);

  if (!SCHEME_REALP(n))
    scheme_wrong_type("sqrt", "number", 0, argc, argv);

  if (scheme_is_negative(n)) {
    n = scheme_bin_minus(zeroi, n);
    imaginary = 1;
  }

  if (SCHEME_INTP(n) || SCHEME_BIGNUMP(n))
    n = scheme_integer_sqrt(n);
#ifdef MZ_USE_SINGLE_FLOATS
  else if (SCHEME_FLTP(n))
    n = scheme_make_float((float)sqrt(SCHEME_FLT_VAL(n)));
#endif
  else if (SCHEME_DBLP(n)) {
    double d = SCHEME_DBL_VAL(n);
#ifdef SQRT_NAN_IS_WRONG
    if (MZ_IS_NAN(d))
      return scheme_nan_object;
#endif
    n = scheme_make_double(sqrt(d));
  } else if (SCHEME_RATIONALP(n))
    n = scheme_rational_sqrt(n);

  if (imaginary)
    return scheme_make_complex(zeroi, n);
  else
    return n;
}

Scheme_Object *do_int_sqrt (const char *name, int argc, Scheme_Object *argv[], int w_rem)
{
  Scheme_Object *v = argv[0], *rem = NULL;

  if (!scheme_is_integer(v)) {
    scheme_wrong_type(name, "integer", 0, argc, argv);
    return NULL;
  }

  if (SCHEME_INTP(v) || SCHEME_BIGNUMP(v)) {
    int imaginary = 0;
    
    if (scheme_is_negative(v)) {
      v = scheme_bin_minus(zeroi, v);
      imaginary = 1;
    }

    v = scheme_integer_sqrt_rem(v, &rem);

    if (imaginary) {
      v = scheme_make_complex(zeroi, v);
      rem = scheme_bin_minus(zeroi, rem);
    }
  } else {
    /* Must be inexact. Compose normal sqrt and floor, which should
       handle infinities and NAN just fine. */
    rem = v;
    v = scheme_sqrt(1, &v);
    if (SCHEME_COMPLEXP(v)) {
      v = scheme_complex_imaginary_part(v);
      v = floor_prim(1, &v);
      v = scheme_make_complex(scheme_make_integer(0), v);
    } else
      v = floor_prim(1, &v);
    
    if (w_rem) {
      rem = scheme_bin_minus(rem, scheme_bin_mult(v, v));
    }
  }

  if (w_rem) {
    Scheme_Object *a[2];
    a[0] = v;
    a[1] = rem;
    return scheme_values(2, a);
  } else
    return v;
}

Scheme_Object *int_sqrt (int argc, Scheme_Object *argv[])
{
  return do_int_sqrt("integer-sqrt", argc, argv, 0);
}

Scheme_Object *int_sqrt_rem (int argc, Scheme_Object *argv[])
{
  return do_int_sqrt("integer-sqrt/remainder", argc, argv, 1);
}

static Scheme_Object *fixnum_expt(long x, long y)
{
  long orig_x = x;
  long orig_y = y;

  if ((x == 2) && (y <= MAX_SHIFT_TRY))
    return scheme_make_integer((long)1 << y);
  else
  {
    long result = 1;
    int odd_result = (x < 0) && (y & 0x1);

    if (x < 0)
      x = -x;
    while (y > 0)
    {
      /* x^y*result is invariant and result <= x */
      if (x > 46339 && y > 1) /* x * x won't fit in 31 bits */
        return scheme_generic_integer_power(scheme_make_integer_value(orig_x), scheme_make_integer_value(orig_y));

      if (y & 0x1) /* if (odd?) */
      {
        long next_result = x * result;
        if (y == 1 && x > 46339 && !(next_result / x == result))
          return scheme_generic_integer_power(scheme_make_integer_value(orig_x), scheme_make_integer_value(orig_y));
        else
          result = next_result;
      }
      y = y >> 1;
      x = x * x;
    }
    return scheme_make_integer_value(odd_result ? -result : result);
  }
}

#ifdef POW_HANDLES_INF_CORRECTLY
# define sch_pow pow
#else
static double sch_pow(double x, double y)
{
  if (MZ_IS_POS_INFINITY(y)) {
    if ((x == 1.0) || (x == -1.0))
      return not_a_number_val;
    else if ((x < 1.0) && (x > -1.0))
      return 0.0;
    else
      return scheme_infinity_val;
  } else if (MZ_IS_NEG_INFINITY(y)) {
    if ((x == 1.0) || (x == -1.0))
      return not_a_number_val;
    else if ((x < 1.0) && (x > -1.0))
      return scheme_infinity_val;
    else
      return 0.0;
  } else if (MZ_IS_POS_INFINITY(x)) {
    if (y == 0.0)
      return 1.0;
    else if (y < 0)
      return 0.0;
    else
      return scheme_infinity_val;
  } else if (MZ_IS_NEG_INFINITY(x)) {
    if (y == 0.0)
      return 1.0;
    else {
      int neg = 0;
      if (y < 0) {
	neg = 1;
	y = -y;
      }
      if (fmod(y, 2.0) == 1.0) {
	if (neg)
	  return scheme_floating_point_nzero;
	else
	  return scheme_minus_infinity_val;
      } else {
	if (neg)
	  return 0.0;
	else
	  return scheme_infinity_val;
      }
    }
  } else
    return pow(x, y);
}
#endif

GEN_BIN_PROT(bin_expt);

# define F_EXPT(x, y) (((x < 0.0) && (y != floor(y))) \
                       ? scheme_complex_power(scheme_real_to_complex(scheme_make_double(x)), \
				              scheme_real_to_complex(scheme_make_double(y))) \
                       : scheme_make_double(sch_pow((double)x, (double)y)))
# define FS_EXPT(x, y) (((x < 0.0) && (y != floor(y))) \
                       ? scheme_complex_power(scheme_real_to_complex(scheme_make_float(x)), \
				              scheme_real_to_complex(scheme_make_float(y))) \
                        : scheme_make_float(sch_pow((double)x, (double)y)))

static GEN_BIN_OP(bin_expt, "expt", fixnum_expt, F_EXPT, FS_EXPT, scheme_generic_integer_power, scheme_rational_power, scheme_complex_power, GEN_RETURN_0_USUALLY, GEN_RETURN_1, NAN_RETURNS_NAN, NAN_RETURNS_SNAN, cx_NO_CHECK, cx_NO_CHECK, cx_NO_CHECK, cx_NO_CHECK)

Scheme_Object *
scheme_expt(int argc, Scheme_Object *argv[])
{
  int invert = 0;
  Scheme_Object *e, *r, *n;

  n = argv[0];
  e = argv[1];

  if (!SCHEME_NUMBERP(n))
    scheme_wrong_type("expt", "number", 0, argc, argv);

  if (e == zeroi)
    return scheme_make_integer(1);
  if (e == scheme_exact_one)
    return n;
  if (n == scheme_exact_one) {
    /* Power of one: */
    if (SCHEME_NUMBERP(e))
      return n;
  }
  if (SCHEME_RATIONALP(e)
      && (((Scheme_Rational *)e)->num == scheme_exact_one)
      && (((Scheme_Rational *)e)->denom == scheme_make_integer(2))) {
    return scheme_sqrt(1, argv);
  }

  if (n == zeroi) {
    /* Power of exact zero */
    int neg;

    if (SCHEME_FLOATP(e)) {
      double d = SCHEME_FLOAT_VAL(e);
      if (MZ_IS_NAN(d)) {
#ifdef MZ_USE_SINGLE_FLOATS
	if (SCHEME_FLTP(e))
	  return scheme_single_nan_object;
#endif
	return scheme_nan_object;
      }
    }

    if (!SCHEME_COMPLEXP(e)) {
      neg = scheme_is_negative(e);
    } else {
      neg = !scheme_is_positive(scheme_complex_real_part(e));
    }
    
    if (neg) {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT_DIVIDE_BY_ZERO,
		       "expt: undefined for 0 and %s",
		       scheme_make_provided_string(e, 0, NULL));
      ESCAPED_BEFORE_HERE;
    }
  }

  if (!SCHEME_FLOATP(n)) {
    /* negative integer power of exact: compute positive power and invert */
    if (SCHEME_INTP(e) || SCHEME_BIGNUMP(e)) {
      if (!scheme_is_positive(e)) {
	e = scheme_bin_minus(zeroi, e);
	invert = 1;
      }
    }
  } else {
    /* real power of inexact zero? */
    /* (Shouldn't have to do this, but pow() is especially unreliable.) */
    double d = SCHEME_FLOAT_VAL(n);
    if ((d == 0.0)
#ifdef NAN_EQUALS_ANYTHING
	&& !MZ_IS_NAN(d)
#endif
	) {
      if (SCHEME_REALP(e)) {
	int norm = 0;

	if (SCHEME_FLOATP(e)) {
	  double d2;
	  d2 = SCHEME_FLOAT_VAL(e);
	  
	  if ((d2 == 0.0)
	      || MZ_IS_INFINITY(d2)
	      || MZ_IS_NAN(d2))
	    norm = 1;
	}

	if (!norm) {
	  int isnonneg, iseven, negz;
#ifdef MZ_USE_SINGLE_FLOATS
	  int single = !SCHEME_DBLP(n) && !SCHEME_DBLP(e);
#endif

	  if (scheme_is_integer(e)) {
	    iseven = SCHEME_FALSEP(scheme_odd_p(1, &e));
	  } else {
	    /* Treat it as even for sign purposes: */
	    iseven = 1;
	  }
	  isnonneg = !scheme_is_negative(e);
	  negz = scheme_minus_zero_p(d);

	  if (isnonneg) {
	    if (iseven || !negz) {
#ifdef MZ_USE_SINGLE_FLOATS
	      if (single)
		return scheme_zerof;
#endif
	      return scheme_zerod;
	    } else {
#ifdef MZ_USE_SINGLE_FLOATS
	      if (single)
		return scheme_nzerof;
#endif
	      return scheme_nzerod;
	    }
	  } else {
	    if (iseven || !negz) {
#ifdef MZ_USE_SINGLE_FLOATS
	      if (single)
		return scheme_single_inf_object;
#endif
	      return scheme_inf_object;
	    } else {
#ifdef MZ_USE_SINGLE_FLOATS
	      if (single)
		return scheme_single_minus_inf_object;
#endif
	      return scheme_minus_inf_object;
	    }
	  }
	}
      }
    }
  }

  r = bin_expt(argv[0], e);
  if (invert)
    r = scheme_bin_div(scheme_make_integer(1), r);

  return r;
}


Scheme_Object *scheme_checked_make_rectangular (int argc, Scheme_Object *argv[])
{
  Scheme_Object *a, *b;
  int af, bf;

  a = argv[0];
  b = argv[1];
  if (!SCHEME_REALP(a))
    scheme_wrong_type("make-rectangular", REAL_NUMBER_STR, 0, argc, argv);
  if (!SCHEME_REALP(b))
    scheme_wrong_type("make-rectangular", REAL_NUMBER_STR, 1, argc, argv);

  af = SCHEME_FLOATP(a);
  bf = SCHEME_FLOATP(b);

  if (af && !bf) {
    if (b != zeroi)
      b = scheme_exact_to_inexact(1, &b);
  }
  if (bf && !af) {
    if (a != zeroi)
      a = scheme_exact_to_inexact(1, &a);
  }

  return scheme_make_complex(a, b);
}

Scheme_Object *scheme_checked_make_flrectangular (int argc, Scheme_Object *argv[])
{
  Scheme_Object *a, *b;

  a = argv[0];
  b = argv[1];
  if (!SCHEME_FLOATP(a))
    scheme_wrong_type("make-rectangular", "inexact-real", 0, argc, argv);
  if (!SCHEME_FLOATP(b))
    scheme_wrong_type("make-rectangular", "inexact-real", 1, argc, argv);

  return scheme_make_complex(a, b);
}

Scheme_Object *scheme_make_polar (int argc, Scheme_Object *argv[])
{
  Scheme_Object *a, *b, *r, *i, *v;

  a = argv[0];
  b = argv[1];
  if (!SCHEME_REALP(a))
    scheme_wrong_type("make-polar", REAL_NUMBER_STR, 0, argc, argv);
  if (!SCHEME_REALP(b))
    scheme_wrong_type("make-polar", REAL_NUMBER_STR, 1, argc, argv);

  if (b == zeroi)
    return a;

  v = b;

  r = scheme_bin_mult(a, cos_prim(1, &v));
  i = scheme_bin_mult(a, sin_prim(1, &v));

  return scheme_make_complex(r, i);
}

Scheme_Object *scheme_checked_real_part (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (!SCHEME_NUMBERP(o))
    scheme_wrong_type("real-part", "number", 0, argc, argv);

  if (SCHEME_COMPLEXP(o))
    return _scheme_complex_real_part(o);
  else
    return argv[0];
}

Scheme_Object *scheme_checked_imag_part (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (!SCHEME_NUMBERP(o))
    scheme_wrong_type("imag-part", "number", 0, argc, argv);

  if (SCHEME_COMPLEXP(o))
    return scheme_complex_imaginary_part(o);

  return zeroi;
}

Scheme_Object *scheme_checked_flreal_part (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (!SCHEME_COMPLEXP(o)
      || !SCHEME_FLOATP(((Scheme_Complex *)o)->r))
    scheme_wrong_type("flreal-part", "complex number with inexact parts", 0, argc, argv);

  return _scheme_complex_real_part(o);
}

Scheme_Object *scheme_checked_flimag_part (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (!SCHEME_COMPLEXP(o)
      || !SCHEME_FLOATP(((Scheme_Complex *)o)->r))
    scheme_wrong_type("flimag-part", "complex number with inexact parts", 0, argc, argv);

  return scheme_complex_imaginary_part(o);
}

static Scheme_Object *magnitude(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (!SCHEME_NUMBERP(o))
    scheme_wrong_type("magnitude", "number", 0, argc, argv);

  if (SCHEME_COMPLEXP(o)) {
    Scheme_Object *r = _scheme_complex_real_part(o);
    Scheme_Object *i = _scheme_complex_imaginary_part(o);
    Scheme_Object *a[1], *q;
    a[0] = r;
    r = scheme_abs(1, a);
    a[0] = i;
    i = scheme_abs(1, a);
    
    if (SAME_OBJ(r, scheme_make_integer(0)))
      return i;

    if (scheme_bin_lt(i, r)) {
      Scheme_Object *tmp;
      tmp = i;
      i = r;
      r = tmp;
    }
    if (scheme_is_zero(r)) {
      a[0] = i;
      return scheme_exact_to_inexact(1, a);
    }
    if (SCHEME_FLOATP(i)) {
      double d;
      d = SCHEME_FLOAT_VAL(i);
      if (MZ_IS_POS_INFINITY(d)) {
        if (SCHEME_FLOATP(r)) {
          d = SCHEME_FLOAT_VAL(r);
          if (MZ_IS_NAN(d))
            return scheme_nan_object;
        }
        return scheme_inf_object;
      }
    }
    q = scheme_bin_div(r, i);
    q = scheme_bin_plus(scheme_make_integer(1),
			scheme_bin_mult(q, q));
    a[0] = q;
    return scheme_bin_mult(i, scheme_sqrt(1, a));
  } else
    return scheme_abs(1, argv);
}

static Scheme_Object *angle(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (!SCHEME_NUMBERP(o))
    scheme_wrong_type("angle", "number", 0, argc, argv);

  if (SCHEME_COMPLEXP(o)) {
    Scheme_Object *r = (Scheme_Object *)_scheme_complex_real_part(o);
    Scheme_Object *i = (Scheme_Object *)_scheme_complex_imaginary_part(o);
    double rd, id, v;
#ifdef MZ_USE_SINGLE_FLOATS
# ifdef USE_SINGLE_FLOATS_AS_DEFAULT
    int was_single = !(SCHEME_DBLP(r) || SCHEME_DBLP(i));
# else
    int was_single = (SCHEME_FLTP(r) || SCHEME_FLTP(i));
# endif
#endif

    id = TO_DOUBLE_VAL(i);
    rd = TO_DOUBLE_VAL(r);

    v = atan2(id, rd);

#ifdef MZ_USE_SINGLE_FLOATS
    if (was_single)
      return scheme_make_float((float)v);
#endif

    return scheme_make_double(v);
  } else {
#ifdef MZ_USE_SINGLE_FLOATS
    if (SCHEME_FLTP(o)) {
      float v = SCHEME_FLT_VAL(o);
      if (MZ_IS_NAN(v))
	return scheme_single_nan_object;
      else if (v == 0.0f) {
	int neg;
	neg = minus_zero_p(v);
	v = (neg ? -1.0f : 1.0f);
      }
      if (v > 0)
	return zeroi;
      else
	return scheme_single_pi;
    }
#endif
    if (SCHEME_DBLP(o)) {
      double v = SCHEME_DBL_VAL(o);
      if (MZ_IS_NAN(v))
	return scheme_nan_object;
      else if (v == 0.0) {
	int neg;
	neg = minus_zero_p(v);
	v = (neg ? -1.0 : 1.0);
      }
      if (v > 0)
	return zeroi;
      else
	return scheme_pi;
    } else if (o == zeroi) {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT_DIVIDE_BY_ZERO,
		       "angle: undefined for 0");
      ESCAPED_BEFORE_HERE;
    } else if (scheme_is_positive(o))
      return zeroi;
    else {
# ifdef USE_SINGLE_FLOATS_AS_DEFAULT
      return scheme_single_pi;
# endif
      return scheme_pi;
    }
  }
}

Scheme_Object *
scheme_exact_to_inexact (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  Scheme_Type t;

  if (SCHEME_INTP(o)) {
#ifdef USE_SINGLE_FLOATS_AS_DEFAULT
    return scheme_make_float(SCHEME_INT_VAL(o));
#else
    return scheme_make_double(SCHEME_INT_VAL(o));
#endif
  }
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type)
    return o;
#endif
  if (t == scheme_double_type)
    return o;
  if (t == scheme_bignum_type) {
#ifdef USE_SINGLE_FLOATS_AS_DEFAULT
    return scheme_make_float(scheme_bignum_to_float(o));
#else
    return scheme_make_double(scheme_bignum_to_double(o));
#endif
  }
  if (t == scheme_rational_type) {
#ifdef USE_SINGLE_FLOATS_AS_DEFAULT
    return scheme_make_float(scheme_rational_to_float(o));
#else
    return scheme_make_double(scheme_rational_to_double(o));
#endif
  }
  if (t == scheme_complex_type) {
    Scheme_Object *realpart, *imaginarypart;

    realpart = _scheme_complex_real_part(o);
    imaginarypart = _scheme_complex_imaginary_part(o);

    realpart = scheme_exact_to_inexact(1, &realpart);
    imaginarypart = scheme_exact_to_inexact(1, &imaginarypart);

    return scheme_make_complex(realpart, imaginarypart);
  }

  NEED_NUMBER(exact->inexact);

  ESCAPED_BEFORE_HERE;
}

Scheme_Object *
scheme_inexact_to_exact (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  Scheme_Type t;

  if (SCHEME_INTP(o))
    return o;
  t = _SCHEME_TYPE(o);
  if (t == scheme_double_type
#ifdef MZ_USE_SINGLE_FLOATS
      || t == scheme_float_type
#endif
      ) {
    double d = SCHEME_FLOAT_VAL(o);

    /* Try simple case: */
    Scheme_Object *i = scheme_make_integer((long)d);
    if ((double)SCHEME_INT_VAL(i) == d) {
# ifdef NAN_EQUALS_ANYTHING
      if (!MZ_IS_NAN(d))
#endif
	return i;
    }

    return scheme_rational_from_double(d);
  }
  if (t == scheme_bignum_type)
    return o;
  if (t == scheme_rational_type)
    return o;
  if (t == scheme_complex_type) {
    Scheme_Object *realpart, *imaginarypart;

    realpart = _scheme_complex_real_part(o);
    imaginarypart = _scheme_complex_imaginary_part(o);

    realpart = scheme_inexact_to_exact(1, &realpart);
    imaginarypart = scheme_inexact_to_exact(1, &imaginarypart);

    return scheme_make_complex(realpart, imaginarypart);
  }

  NEED_NUMBER(inexact->exact);

  ESCAPED_BEFORE_HERE;
}

#ifdef MZ_USE_SINGLE_FLOATS
int scheme_check_float(const char *where, float f, const char *dest)
{
  return scheme_check_double(where, f, dest);
}
#endif

GEN_BIN_PROT(bin_bitwise_and);
GEN_BIN_PROT(bin_bitwise_or);
GEN_BIN_PROT(bin_bitwise_xor);

GEN_BIN_INT_OP(bin_bitwise_and, "bitwise-and", &, scheme_bignum_and)
GEN_BIN_INT_OP(bin_bitwise_or, "bitwise-ior", |, scheme_bignum_or)
GEN_BIN_INT_OP(bin_bitwise_xor, "bitwise-xor", ^, scheme_bignum_xor)

#define MZ_PUBLIC /**/

GEN_NARY_OP(MZ_PUBLIC, scheme_bitwise_and, "bitwise-and", bin_bitwise_and, -1, SCHEME_EXACT_INTEGERP, "exact integer", GEN_IDENT)
GEN_NARY_OP(static, bitwise_or, "bitwise-ior", bin_bitwise_or, 0, SCHEME_EXACT_INTEGERP, "exact integer", GEN_IDENT)
GEN_NARY_OP(static, bitwise_xor, "bitwise-xor", bin_bitwise_xor, 0, SCHEME_EXACT_INTEGERP, "exact integer", GEN_IDENT)

static Scheme_Object *
bitwise_not(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (SCHEME_INTP(o)) {
    long a = SCHEME_INT_VAL(o);

    a = ~a;
    return scheme_make_integer(a);
  } else if (_SCHEME_TYPE(o) == scheme_bignum_type)
    return scheme_bignum_not(o);
   
  scheme_wrong_type("bitwise-not", "exact integer", 0, argc, argv);
  ESCAPED_BEFORE_HERE;
}

Scheme_Object *
scheme_bitwise_shift(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v, *so;
  long shift;

  v = argv[0];
  
  if (!SCHEME_EXACT_INTEGERP(v)) {
    scheme_wrong_type("arithmetic-shift", "exact integer", 0, argc, argv);
    ESCAPED_BEFORE_HERE;
  }
  so = argv[1];
  if (!SCHEME_INTP(so)) {
    if (SCHEME_BIGNUMP(so)) {
      if (!SCHEME_BIGPOS(so)) {
	if (scheme_is_negative(v))
	  return scheme_make_integer(-1);
	else
	  return scheme_make_integer(0);
      } else
	scheme_raise_out_of_memory("arithmetic-shift", NULL);
    } else
      scheme_wrong_type("arithmetic-shift", "exact integer", 1, argc, argv);
    ESCAPED_BEFORE_HERE;
  }
  
  shift = SCHEME_INT_VAL(so);
  if (!shift)
    return v;

  if (SCHEME_INTP(v)) {
    long i = SCHEME_INT_VAL(v);

    if (!i)
      return v;

    if (i > 0) {
      if (shift < 0) {
	int shft = -shift;
	if (shft < MAX_SHIFT_EVER) {
	  i = i >> shft;
	  return scheme_make_integer(i);
	} else
	  return scheme_make_integer(0);
      } else if (shift <= MAX_SHIFT_TRY) {
	long n;
	
	n = i << shift;
	if ((n > 0) && (SCHEME_INT_VAL(scheme_make_integer(n)) >> shift == i))
	  return scheme_make_integer(n);
      }
    }

    v = scheme_make_bignum(i);
  }

  return scheme_bignum_shift(v, shift);
}

static Scheme_Object *bitwise_bit_set_p (int argc, Scheme_Object *argv[])
{
  Scheme_Object *so, *sb;

  so = argv[0];
  if (!SCHEME_EXACT_INTEGERP(so)) {
    scheme_wrong_type("bitwise-bit-set?", "exact integer", 0, argc, argv);
    ESCAPED_BEFORE_HERE;
  }
  sb = argv[1];
  if (SCHEME_INTP(sb)) {
    long v;
    v = SCHEME_INT_VAL(sb);
    if (v < 0) {
      scheme_wrong_type("bitwise-bit-set?", "nonnegative exact integer", 1, argc, argv);
      ESCAPED_BEFORE_HERE;
    }
    if (SCHEME_INTP(so)) {
      if (v < (sizeof(long) * 8))
        return ((((long)1 << v) & SCHEME_INT_VAL(so)) ? scheme_true : scheme_false);
      else
        return ((SCHEME_INT_VAL(so) < 0) ? scheme_true : scheme_false);
    } else {
      bigdig d;
      long vd, vb;
      vd = v / (sizeof(bigdig) * 8);
      vb = v & ((sizeof(bigdig) * 8) - 1);
      if (vd >= ((Scheme_Bignum *)so)->len)
        return (SCHEME_BIGPOS(so) ? scheme_false : scheme_true);
      if (SCHEME_BIGPOS(so)) {
        d = ((Scheme_Bignum *)so)->digits[vd];
        return ((((bigdig)1 << vb) & d) ? scheme_true : scheme_false);
      } else {
        /* Testing a bit in a negative bignum. Just use the slow way for now. */
        Scheme_Object *bit;
        bit = scheme_bignum_shift(scheme_make_bignum(1), v);
        if (SCHEME_INTP(bit))
          bit = scheme_make_bignum(SCHEME_INT_VAL(bit));
        bit = scheme_bignum_and(bit, so);
        return (SAME_OBJ(bit, scheme_make_integer(0)) ? scheme_false : scheme_true);
      }
    }
  } else if (SCHEME_BIGNUMP(sb) && SCHEME_BIGPOS(sb)) {
    if (SCHEME_INTP(so))
      return ((SCHEME_INT_VAL(so) < 0) ? scheme_true : scheme_false);
    else
      return (SCHEME_BIGPOS(so) ? scheme_false : scheme_true);
  } else {
    scheme_wrong_type("bitwise-bit-set?", "nonnegative exact integer", 1, argc, argv);
    ESCAPED_BEFORE_HERE;
  }
}

static Scheme_Object *slow_bitwise_bit_field (int argc, Scheme_Object *argv[],
                                              Scheme_Object *so, Scheme_Object *sb1, Scheme_Object *sb2)
{
  Scheme_Object *a[2];

  if (!SCHEME_EXACT_INTEGERP(so))
    scheme_wrong_type("bitwise-bit-field", "exact integer", 0, argc, argv);

  if (!((SCHEME_INTP(sb1) && (SCHEME_INT_VAL(sb1) >= 0))
        || (SCHEME_BIGNUMP(sb1) && SCHEME_BIGPOS(sb1))))
    scheme_wrong_type("bitwise-bit-field", "nonnegative exact integer", 1, argc, argv);
  if (!((SCHEME_INTP(sb2) && (SCHEME_INT_VAL(sb2) >= 0))
        || (SCHEME_BIGNUMP(sb2) && SCHEME_BIGPOS(sb2))))
    scheme_wrong_type("bitwise-bit-field", "nonnegative exact integer", 2, argc, argv);

  if (!scheme_bin_lt_eq(sb1, sb2))
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                     "bitwise-bit-field: first index: %V is more than second index: %V",
                     sb1, sb2);
  
  sb2 = scheme_bin_minus(sb2, sb1);
  sb1 = scheme_bin_minus(scheme_make_integer(0), sb1);
  
  a[0] = so;
  a[1] = sb1;
  so = scheme_bitwise_shift(2, a);
  
  a[0] = scheme_make_integer(1);
  a[1] = sb2;
  sb2 = scheme_bitwise_shift(2, a);
  
  sb2 = scheme_bin_minus(sb2, scheme_make_integer(1));
  
  a[0] = so;
  a[1] = sb2;
  return scheme_bitwise_and(2, a);    
}

static Scheme_Object *bitwise_bit_field (int argc, Scheme_Object *argv[])
{
  Scheme_Object *so, *sb1, *sb2;

  so = argv[0];
  sb1 = argv[1];
  sb2 = argv[2];
  if (SCHEME_EXACT_INTEGERP(so)) {
    /* Fast path is when sb1 < sizeof(long), sb2 - sb1 < sizeof(long),
       and argument is positive (though the fixnum negative case is also
       handled here). */
    if (SCHEME_INTP(sb1)) {
      long v1;
      v1 = SCHEME_INT_VAL(sb1);
      if (v1 >= 0) {
        if (SCHEME_INTP(sb2)) {
          long v2;
          v2 = SCHEME_INT_VAL(sb2);
          if (v2 >= v1) {
            v2 -= v1;
            if (v2 < (sizeof(long) * 8)) {
              if (SCHEME_INTP(so)) {
                if (v1 < (sizeof(long) * 8)) {
                  long res;
                  res = ((SCHEME_INT_VAL(so) >> v1) & (((long)1 << v2) - 1));
                  return scheme_make_integer(res);
                } else if (SCHEME_INT_VAL(so) > 0) 
                  return scheme_make_integer(0);
              } else if (SCHEME_BIGPOS(so)) {
                bigdig d;
                long vd, vb, avail;
                vd = v1 / (sizeof(bigdig) * 8);
                vb = v1 & ((sizeof(bigdig) * 8) - 1);
                if (vd >= ((Scheme_Bignum *)so)->len)
                  return scheme_make_integer(0);
                d = ((Scheme_Bignum *)so)->digits[vd];
                d >>= vb;
                avail = (sizeof(bigdig) * 8) - vb;
                if ((avail < v2)
                    && ((vd + 1) < ((Scheme_Bignum *)so)->len)) {
                  /* Pull in more bits from next digit: */
                  d |= (((Scheme_Bignum *)so)->digits[vd + 1] << avail);
                }
                d = (d & (((bigdig)1 << v2) - 1));
                return scheme_make_integer(d);
              }
            }
          }
        }
      }
    }
  }

  return slow_bitwise_bit_field(argc, argv, so, sb1, sb2);
}

static Scheme_Object *
integer_length(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  unsigned long n;
  int base;

  if (SCHEME_INTP(o)) {
    long a = SCHEME_INT_VAL(o);

    if (a < 0)
      a = ~a;
    
    n = a;
    base = 0;
  } else if (_SCHEME_TYPE(o) == scheme_bignum_type) {
    bigdig d;

    if (!SCHEME_BIGPOS(o)) {
      /* Maybe we could do better... */
      o = scheme_bignum_not(o);
    }

    base = ((Scheme_Bignum *)o)->len;
    d = ((Scheme_Bignum *)o)->digits[base - 1];
    base = (base - 1) * (sizeof(bigdig) * 8);

#ifdef USE_LONG_LONG_FOR_BIGDIG
    n = (unsigned long)d;
    if ((bigdig)n != d) {
      /* Must have been overflow */
      d >>= (sizeof(unsigned long) * 8);
      base += (sizeof(unsigned long) * 8);
      n = (unsigned long)d;
    }
#else
    n = d;
#endif
  } else {
    scheme_wrong_type("integer-length", "exact integer", 0, argc, argv);
    ESCAPED_BEFORE_HERE;
  }

  while (n) {
    n >>= 1;
    base++;
  }

  return scheme_make_integer(base);
}

long scheme_integer_length(Scheme_Object *n)
{
  Scheme_Object *a[1], *r;
  a[0] = n;
  r = integer_length(1, a);
  return SCHEME_INT_VAL(r);
}


/************************************************************************/
/*                             flvectors                               */
/************************************************************************/

Scheme_Double_Vector *scheme_alloc_flvector(long size)
{
  Scheme_Double_Vector *vec;

  vec = (Scheme_Double_Vector *)scheme_malloc_fail_ok(scheme_malloc_atomic_tagged, 
                                                      sizeof(Scheme_Double_Vector) 
                                                      + ((size - 1) * sizeof(double)));
  vec->iso.so.type = scheme_flvector_type;
  vec->size = size;

  return vec;
}

#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
static Scheme_Double_Vector *alloc_shared_flvector(long size)
{
  Scheme_Double_Vector *vec;
  void *original_gc;

  original_gc = GC_switch_to_master_gc();
  vec = scheme_alloc_flvector(size);
  GC_switch_back_from_master(original_gc);

  return vec;
}
#endif

static Scheme_Object *do_flvector (const char *name, Scheme_Double_Vector *vec, int argc, Scheme_Object *argv[])
{
  int i;

  for (i = 0; i < argc; i++) {
    if (!SCHEME_FLOATP(argv[i])) {
      scheme_wrong_type(name, "inexact real", i, argc, argv);
      return NULL;
    }
    vec->els[i] = SCHEME_FLOAT_VAL(argv[i]);
  }

  return (Scheme_Object *)vec;
}

static Scheme_Object *flvector (int argc, Scheme_Object *argv[])
{
  return do_flvector("flvector", scheme_alloc_flvector(argc), argc, argv);
}

#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
static Scheme_Object *flvector (int argc, Scheme_Object *argv[])
{
  return do_flvector("shared-flvector", scheme_alloc_shared_flvector(argc), argc, argv);
}
#endif

static Scheme_Object *flvector_p (int argc, Scheme_Object *argv[])
{
  if (SCHEME_FLVECTORP(argv[0]))
    return scheme_true;
  else
    return scheme_false;
}

static Scheme_Object *do_make_flvector (const char *name, int as_shared, int argc, Scheme_Object *argv[])
{
  Scheme_Double_Vector *vec;
  long size;

  if (SCHEME_INTP(argv[0]))
    size = SCHEME_INT_VAL(argv[0]);
  else if (SCHEME_BIGNUMP(argv[0])) {
    if (SCHEME_BIGPOS(argv[0])) {
      scheme_raise_out_of_memory(name, NULL);
      return NULL;
    } else
      size = -1;
  } else
    size = -1;

  if (size < 0)
    scheme_wrong_type(name, "exact non-negative integer", 0, argc, argv);

  if (argc > 1) {
    if (!SCHEME_FLOATP(argv[1]))
      scheme_wrong_type(name, "inexact real", 1, argc, argv);
  }
  
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  if (as_shared)
    vec = scheme_alloc_shared_flvector(size);
  else
#else
    vec = scheme_alloc_flvector(size);
#endif

  if (argc > 1) {
    int i;
    double d = SCHEME_FLOAT_VAL(argv[1]);
    for (i = 0; i < size; i++) {
      vec->els[i] = d;
    }
  }

  return (Scheme_Object *)vec;
}

static Scheme_Object *make_flvector (int argc, Scheme_Object *argv[])
{
  return do_make_flvector("make-flvector", 0, argc, argv);
}

#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
static Scheme_Object *make_shared_flvector (int argc, Scheme_Object *argv[])
{
  return do_make_flvector("make-shared-flvector", 1, argc, argv);
}
#endif

Scheme_Object *scheme_flvector_length(Scheme_Object *vec)
{
  if (!SCHEME_FLVECTORP(vec))
    scheme_wrong_type("flvector-length", "flvector", 0, 1, &vec);

  return scheme_make_integer(SCHEME_FLVEC_SIZE(vec));
}

static Scheme_Object *flvector_length (int argc, Scheme_Object *argv[])
{
  return scheme_flvector_length(argv[0]);
}

Scheme_Object *scheme_checked_flvector_ref (int argc, Scheme_Object *argv[])
{
  double d;
  Scheme_Object *vec;
  long len, pos;

  vec = argv[0];
  if (!SCHEME_FLVECTORP(vec))
    scheme_wrong_type("flvector-ref", "flvector", 0, argc, argv);
  
  len = SCHEME_FLVEC_SIZE(vec);
  pos = scheme_extract_index("flvector-ref", 1, argc, argv, len, 0);

  if (pos >= len) {
    scheme_bad_vec_index("flvector-ref", argv[1], 
                         "flvector", vec,
                         0, len);
    return NULL;
  }

  d = SCHEME_FLVEC_ELS(vec)[pos];

  return scheme_make_double(d);
}

Scheme_Object *scheme_checked_flvector_set (int argc, Scheme_Object *argv[])
{
  Scheme_Object *vec;
  long len, pos;

  vec = argv[0];
  if (!SCHEME_FLVECTORP(vec))
    scheme_wrong_type("flvector-set!", "flvector", 0, argc, argv);
  
  len = SCHEME_FLVEC_SIZE(vec);
  pos = scheme_extract_index("flvector-set!", 1, argc, argv, len, 0);
  
  if (!SCHEME_FLOATP(argv[2]))
    scheme_wrong_type("flvector-set!", "inexact real", 2, argc, argv);

  if (pos >= len) {
    scheme_bad_vec_index("flvector-set!", argv[1], 
                         "flvector", vec,
                         0, len);
    return NULL;
  }

  SCHEME_FLVEC_ELS(vec)[pos] = SCHEME_FLOAT_VAL(argv[2]);

  return scheme_void;
}

/************************************************************************/
/*                             fxvectors                               */
/************************************************************************/

Scheme_Vector *scheme_alloc_fxvector(long size)
{
  Scheme_Vector *vec;

  vec = (Scheme_Vector *)scheme_malloc_fail_ok(scheme_malloc_atomic_tagged, 
                                               sizeof(Scheme_Vector) 
                                               + ((size - 1) * sizeof(Scheme_Object*)));
  vec->iso.so.type = scheme_fxvector_type;
  vec->size = size;

  return vec;
}

#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
static Scheme_Vector *alloc_shared_fxvector(long size)
{
  Scheme_Vector *vec;
  void *original_gc;

  original_gc = GC_switch_to_master_gc();
  vec = scheme_alloc_fxvector(size);
  GC_switch_back_from_master(original_gc);

  return vec;
}
#endif

static Scheme_Object *do_fxvector (const char *name, Scheme_Vector *vec, int argc, Scheme_Object *argv[])
{
  int i;

  for (i = 0; i < argc; i++) {
    if (!SCHEME_INTP(argv[i])) {
      scheme_wrong_type(name, "fixnum", i, argc, argv);
      return NULL;
    }
    vec->els[i] = argv[i];
  }

  return (Scheme_Object *)vec;
}

static Scheme_Object *fxvector (int argc, Scheme_Object *argv[])
{
  return do_fxvector("fxvector", scheme_alloc_fxvector(argc), argc, argv);
}

#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
static Scheme_Object *fxvector (int argc, Scheme_Object *argv[])
{
  return do_fxvector("shared-fxvector", scheme_alloc_shared_fxvector(argc), argc, argv);
}
#endif

static Scheme_Object *fxvector_p (int argc, Scheme_Object *argv[])
{
  if (SCHEME_FXVECTORP(argv[0]))
    return scheme_true;
  else
    return scheme_false;
}

static Scheme_Object *do_make_fxvector (const char *name, int as_shared, int argc, Scheme_Object *argv[])
{
  Scheme_Vector *vec;
  long size;

  if (SCHEME_INTP(argv[0]))
    size = SCHEME_INT_VAL(argv[0]);
  else if (SCHEME_BIGNUMP(argv[0])) {
    if (SCHEME_BIGPOS(argv[0])) {
      scheme_raise_out_of_memory(name, NULL);
      return NULL;
    } else
      size = -1;
  } else
    size = -1;

  if (size < 0)
    scheme_wrong_type(name, "exact non-negative integer", 0, argc, argv);

  if (argc > 1) {
    if (!SCHEME_INTP(argv[1]))
      scheme_wrong_type(name, "fixnum", 1, argc, argv);
  }
  
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  if (as_shared)
    vec = scheme_alloc_shared_fxvector(size);
  else
#else
    vec = scheme_alloc_fxvector(size);
#endif

  {
    int i;
    Scheme_Object *val = ((argc > 1) ? argv[1] : scheme_make_integer(0));
    for (i = 0; i < size; i++) {
      vec->els[i] = val;
    }
  }

  return (Scheme_Object *)vec;
}

static Scheme_Object *make_fxvector (int argc, Scheme_Object *argv[])
{
  return do_make_fxvector("make-fxvector", 0, argc, argv);
}

#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
static Scheme_Object *make_shared_fxvector (int argc, Scheme_Object *argv[])
{
  return do_make_fxvector("make-shared-fxvector", 1, argc, argv);
}
#endif

Scheme_Object *scheme_fxvector_length(Scheme_Object *vec)
{
  if (!SCHEME_FXVECTORP(vec))
    scheme_wrong_type("fxvector-length", "fxvector", 0, 1, &vec);

  return scheme_make_integer(SCHEME_FXVEC_SIZE(vec));
}

static Scheme_Object *fxvector_length (int argc, Scheme_Object *argv[])
{
  return scheme_fxvector_length(argv[0]);
}

Scheme_Object *scheme_checked_fxvector_ref (int argc, Scheme_Object *argv[])
{
  Scheme_Object *vec;
  long len, pos;

  vec = argv[0];
  if (!SCHEME_FXVECTORP(vec))
    scheme_wrong_type("fxvector-ref", "fxvector", 0, argc, argv);
  
  len = SCHEME_FXVEC_SIZE(vec);
  pos = scheme_extract_index("fxvector-ref", 1, argc, argv, len, 0);

  if (pos >= len) {
    scheme_bad_vec_index("fxvector-ref", argv[1], 
                         "fxvector", vec,
                         0, len);
    return NULL;
  }

  return SCHEME_FXVEC_ELS(vec)[pos];
}

Scheme_Object *scheme_checked_fxvector_set (int argc, Scheme_Object *argv[])
{
  Scheme_Object *vec;
  long len, pos;

  vec = argv[0];
  if (!SCHEME_FXVECTORP(vec))
    scheme_wrong_type("fxvector-set!", "fxvector", 0, argc, argv);
  
  len = SCHEME_FXVEC_SIZE(vec);
  pos = scheme_extract_index("fxvector-set!", 1, argc, argv, len, 0);
  
  if (!SCHEME_INTP(argv[2]))
    scheme_wrong_type("fxvector-set!", "fixnum", 2, argc, argv);

  if (pos >= len) {
    scheme_bad_vec_index("fxvector-set!", argv[1], 
                         "fxvector", vec,
                         0, len);
    return NULL;
  }

  SCHEME_FXVEC_ELS(vec)[pos] = argv[2];

  return scheme_void;
}

/************************************************************************/
/*                               Unsafe                                 */
/************************************************************************/

static Scheme_Object *neg_bitwise_shift(int argc, Scheme_Object *argv[])
{
  Scheme_Object *a[2];
  a[0] = argv[0];
  a[1] = scheme_bin_minus(scheme_make_integer(0), argv[1]);
  return scheme_bitwise_shift(argc, a);
}

#define SAFE_FX(name, s_name, scheme_op, sec_p, sec_t)      \
 static Scheme_Object *name(int argc, Scheme_Object *argv[]) \
 {                                                           \
   Scheme_Object *o;                                         \
   if (!SCHEME_INTP(argv[0])) scheme_wrong_type(s_name, "fixnum", 0, argc, argv); \
   if (!sec_p(argv[1])) scheme_wrong_type(s_name, sec_t, 1, argc, argv); \
   o = scheme_op(argc, argv);                                \
   if (!SCHEME_INTP(o)) scheme_non_fixnum_result(s_name, o); \
   return o;                            \
 }

SAFE_FX(fx_and, "fxand", scheme_bitwise_and, SCHEME_INTP, "fixnum")
SAFE_FX(fx_or, "fxior", bitwise_or, SCHEME_INTP, "fixnum")
SAFE_FX(fx_xor, "fxxor", bitwise_xor, SCHEME_INTP, "fixnum")

#ifdef SIXTY_FOUR_BIT_INTEGERS
# define FIXNUM_WIDTH_P(v) (SCHEME_INTP(v) && (SCHEME_INT_VAL(v) >= 0) && (SCHEME_INT_VAL(v) <= 64))
# define FIXNUM_WIDTH_TYPE "exact integer in [0,63]"
#else
# define FIXNUM_WIDTH_P(v) (SCHEME_INTP(v) && (SCHEME_INT_VAL(v) >= 0) && (SCHEME_INT_VAL(v) <= 31))
# define FIXNUM_WIDTH_TYPE "exact integer in [0,31]"
#endif

SAFE_FX(fx_lshift, "fxlshift", scheme_bitwise_shift, FIXNUM_WIDTH_P, FIXNUM_WIDTH_TYPE)
SAFE_FX(fx_rshift, "fxrshift", neg_bitwise_shift, FIXNUM_WIDTH_P, FIXNUM_WIDTH_TYPE)

static Scheme_Object *fx_not (int argc, Scheme_Object *argv[])
{
  long v;
  if (!SCHEME_INTP(argv[0])) scheme_wrong_type("fxnot", "fixnum", 0, argc, argv);
  v = SCHEME_INT_VAL(argv[0]);
  v = ~v;
  return scheme_make_integer(v);
}

static Scheme_Object *fx_to_fl (int argc, Scheme_Object *argv[])
{
  long v;
  if (!SCHEME_INTP(argv[0])) scheme_wrong_type("fx->fl", "fixnum", 0, argc, argv);
  v = SCHEME_INT_VAL(argv[0]);
  return scheme_make_double(v);
}

static Scheme_Object *fl_to_fx (int argc, Scheme_Object *argv[])
{
  double d;
  long v;
  Scheme_Object *o;

  if (!SCHEME_DBLP(argv[0])
      || !scheme_is_integer(argv[0]))
    scheme_wrong_type("fl->fx", "inexact-real integer", 0, argc, argv);

  d = SCHEME_DBL_VAL(argv[0]);
  v = (long)d;
  if ((double)v == d) {
    o = scheme_make_integer_value(v);
    if (SCHEME_INTP(o))
      return o;
  }

  scheme_arg_mismatch("fl->fx", "no fixnum representation: ", argv[0]);
  return NULL;
}

#define SAFE_FL(op) \
  static Scheme_Object * fl_ ## op (int argc, Scheme_Object *argv[])    \
  {                                                                     \
    double v;                                                           \
    if (!SCHEME_DBLP(argv[0])) scheme_wrong_type("fl" #op, "inexact-real", 0, argc, argv); \
    v = scheme_double_ ## op (SCHEME_DBL_VAL(argv[0]));                  \
    return scheme_make_double(v);                                        \
  }

SAFE_FL(floor)
SAFE_FL(ceiling)
SAFE_FL(truncate)
SAFE_FL(round)
SAFE_FL(sin)
SAFE_FL(cos)
SAFE_FL(tan)
SAFE_FL(asin)
SAFE_FL(acos)
SAFE_FL(atan)
SAFE_FL(exp)
SAFE_FL(log)

#define UNSAFE_FX(name, op, fold)                            \
 static Scheme_Object *name(int argc, Scheme_Object *argv[]) \
 {                                                           \
   long v;                                                   \
   if (scheme_current_thread->constant_folding) return fold(argc, argv);     \
   v = SCHEME_INT_VAL(argv[0]) op SCHEME_INT_VAL(argv[1]);   \
   return scheme_make_integer(v);                            \
 }

UNSAFE_FX(unsafe_fx_and, &, scheme_bitwise_and)
UNSAFE_FX(unsafe_fx_or, |, bitwise_or)
UNSAFE_FX(unsafe_fx_xor, ^, bitwise_xor)
UNSAFE_FX(unsafe_fx_lshift, <<, scheme_bitwise_shift)

UNSAFE_FX(unsafe_fx_rshift, >>, neg_bitwise_shift)

static Scheme_Object *unsafe_fx_not (int argc, Scheme_Object *argv[])
{
  long v;
  if (scheme_current_thread->constant_folding) return bitwise_not(argc, argv);
  v = SCHEME_INT_VAL(argv[0]);
  v = ~v;
  return scheme_make_integer(v);
}

static Scheme_Object *unsafe_fx_to_fl (int argc, Scheme_Object *argv[])
{
  long v;
  if (scheme_current_thread->constant_folding) return scheme_exact_to_inexact(argc, argv);
  v = SCHEME_INT_VAL(argv[0]);
  return scheme_make_double(v);
}

static Scheme_Object *unsafe_fl_to_fx (int argc, Scheme_Object *argv[])
{
  long v;
  if (scheme_current_thread->constant_folding) return scheme_inexact_to_exact(argc, argv);
  v = (long)(SCHEME_DBL_VAL(argv[0]));
  return scheme_make_integer(v);
}

static Scheme_Object *fl_ref (int argc, Scheme_Object *argv[])
{
  double v;
  Scheme_Object *p;
  p = ((Scheme_Structure *)argv[0])->slots[0];
  v = ((double *)SCHEME_CPTR_VAL(p))[SCHEME_INT_VAL(argv[1])];
  return scheme_make_double(v);
}

static Scheme_Object *fl_set (int argc, Scheme_Object *argv[])
{
  Scheme_Object *p;
  p = ((Scheme_Structure *)argv[0])->slots[0];
  ((double *)SCHEME_CPTR_VAL(p))[SCHEME_INT_VAL(argv[1])] = SCHEME_DBL_VAL(argv[2]);
  return scheme_void;
}

static Scheme_Object *unsafe_flvector_length (int argc, Scheme_Object *argv[])
{
  return scheme_make_integer(SCHEME_FLVEC_SIZE(argv[0]));
}

static Scheme_Object *unsafe_flvector_ref (int argc, Scheme_Object *argv[])
{
  long pos;
  double d;

  pos = SCHEME_INT_VAL(argv[1]);
  d = SCHEME_FLVEC_ELS(argv[0])[pos];

  return scheme_make_double(d);
}

static Scheme_Object *unsafe_flvector_set (int argc, Scheme_Object *argv[])
{
  long pos;

  pos = SCHEME_INT_VAL(argv[1]);
  SCHEME_FLVEC_ELS(argv[0])[pos] = SCHEME_FLOAT_VAL(argv[2]);

  return scheme_void;
}

static Scheme_Object *unsafe_fxvector_length (int argc, Scheme_Object *argv[])
{
  return scheme_make_integer(SCHEME_FXVEC_SIZE(argv[0]));
}

static Scheme_Object *unsafe_fxvector_ref (int argc, Scheme_Object *argv[])
{
  long pos;

  pos = SCHEME_INT_VAL(argv[1]);
  return SCHEME_FXVEC_ELS(argv[0])[pos];
}

static Scheme_Object *unsafe_fxvector_set (int argc, Scheme_Object *argv[])
{
  long pos;

  pos = SCHEME_INT_VAL(argv[1]);
  SCHEME_FXVEC_ELS(argv[0])[pos] = argv[2];

  return scheme_void;
}

static Scheme_Object *s16_ref (int argc, Scheme_Object *argv[])
{
  long v;
  Scheme_Object *p;
  p = ((Scheme_Structure *)argv[0])->slots[0];
  v = ((short *)SCHEME_CPTR_VAL(p))[SCHEME_INT_VAL(argv[1])];
  return scheme_make_integer(v);
}

static Scheme_Object *s16_set (int argc, Scheme_Object *argv[])
{
  Scheme_Object *p;
  p = ((Scheme_Structure *)argv[0])->slots[0];
  ((short *)SCHEME_CPTR_VAL(p))[SCHEME_INT_VAL(argv[1])] = (short)SCHEME_INT_VAL(argv[2]);
  return scheme_void;
}

static Scheme_Object *u16_ref (int argc, Scheme_Object *argv[])
{
  long v;
  Scheme_Object *p;
  p = ((Scheme_Structure *)argv[0])->slots[0];
  v = ((unsigned short *)SCHEME_CPTR_VAL(p))[SCHEME_INT_VAL(argv[1])];
  return scheme_make_integer(v);
}

static Scheme_Object *u16_set (int argc, Scheme_Object *argv[])
{
  Scheme_Object *p;
  p = ((Scheme_Structure *)argv[0])->slots[0];
  ((unsigned short *)SCHEME_CPTR_VAL(p))[SCHEME_INT_VAL(argv[1])] = (unsigned short)SCHEME_INT_VAL(argv[2]);
  return scheme_void;
}

static Scheme_Object *integer_to_fl (int argc, Scheme_Object *argv[])
{
  if (SCHEME_INTP(argv[0])
      || SCHEME_BIGNUMP(argv[0])) {
    return scheme_exact_to_inexact(argc, argv);
  } else {
    scheme_wrong_type("->fl", "exact integer", 0, argc, argv);
    return NULL;
  }
}

static Scheme_Object *fl_to_integer (int argc, Scheme_Object *argv[])
{
  if (SCHEME_DBLP(argv[0])) {
    Scheme_Object *o;
    o = scheme_inexact_to_exact(argc, argv);
    if (SCHEME_INTP(o) || SCHEME_BIGNUMP(o))
      return o;
  }
   
  scheme_wrong_type("fl->exact-integer", "inexact-real integer", 0, argc, argv);
  return NULL;
}

static Scheme_Object *unsafe_make_flrectangular (int argc, Scheme_Object *argv[])
{
  return scheme_make_complex(argv[0], argv[1]);
}

static Scheme_Object *unsafe_flreal_part (int argc, Scheme_Object *argv[])
{
  return ((Scheme_Complex *)argv[0])->r;
}

static Scheme_Object *unsafe_flimag_part (int argc, Scheme_Object *argv[])
{
  return ((Scheme_Complex *)argv[0])->i;
}
