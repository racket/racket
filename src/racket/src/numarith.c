/*
  Racket
  Copyright (c) 2004-2013 PLT Design Inc.
  Copyright (c) 2000-2001 Matthew Flatt

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

static Scheme_Object *plus (int argc, Scheme_Object *argv[]);
static Scheme_Object *minus (int argc, Scheme_Object *argv[]);
static Scheme_Object *mult (int argc, Scheme_Object *argv[]);
static Scheme_Object *div_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *quotient (int argc, Scheme_Object *argv[]);
static Scheme_Object *rem_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *quotient_remainder (int argc, Scheme_Object *argv[]);

static Scheme_Object *fx_plus (int argc, Scheme_Object *argv[]);
static Scheme_Object *fx_minus (int argc, Scheme_Object *argv[]);
static Scheme_Object *fx_mult (int argc, Scheme_Object *argv[]);
static Scheme_Object *fx_div (int argc, Scheme_Object *argv[]);
static Scheme_Object *fx_rem (int argc, Scheme_Object *argv[]);
static Scheme_Object *fx_mod (int argc, Scheme_Object *argv[]);
static Scheme_Object *fx_abs (int argc, Scheme_Object *argv[]);

static Scheme_Object *unsafe_fx_plus (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fx_minus (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fx_mult (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fx_div (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fx_rem (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fx_mod (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fx_abs (int argc, Scheme_Object *argv[]);

static Scheme_Object *fl_plus (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_minus (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_mult (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_div (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_abs (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_sqrt (int argc, Scheme_Object *argv[]);

static Scheme_Object *unsafe_fl_plus (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fl_minus (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fl_mult (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fl_div (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fl_abs (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fl_sqrt (int argc, Scheme_Object *argv[]);

static Scheme_Object *extfl_plus (int argc, Scheme_Object *argv[]);
static Scheme_Object *extfl_minus (int argc, Scheme_Object *argv[]);
static Scheme_Object *extfl_mult (int argc, Scheme_Object *argv[]);
static Scheme_Object *extfl_div (int argc, Scheme_Object *argv[]);
static Scheme_Object *extfl_abs (int argc, Scheme_Object *argv[]);
static Scheme_Object *extfl_sqrt (int argc, Scheme_Object *argv[]);

static Scheme_Object *unsafe_extfl_plus (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_extfl_minus (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_extfl_mult (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_extfl_div (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_extfl_abs (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_extfl_sqrt (int argc, Scheme_Object *argv[]);

#define zeroi scheme_exact_zero

#if defined(__POWERPC__) || defined(powerpc)
# define SQRT_MACHINE_CODE_AVAILABLE 0
#else
# define SQRT_MACHINE_CODE_AVAILABLE 1
#endif

void scheme_init_numarith(Scheme_Env *env)
{
  Scheme_Object *p;

  p = scheme_make_folding_prim(scheme_add1, "add1", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_add_global_constant("add1", p, env);

  p = scheme_make_folding_prim(scheme_sub1, "sub1", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_add_global_constant("sub1", p, env);

  p = scheme_make_folding_prim(plus, "+", 0, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED);
  scheme_add_global_constant("+", p, env);

  p = scheme_make_folding_prim(minus, "-", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED);
  scheme_add_global_constant("-", p, env);

  p = scheme_make_folding_prim(mult, "*", 0, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED);
  scheme_add_global_constant("*", p, env);

  p = scheme_make_folding_prim(div_prim, "/", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED);
  scheme_add_global_constant("/", p, env);

  p = scheme_make_folding_prim(scheme_abs, "abs", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_add_global_constant("abs", p, env);
  
  p = scheme_make_folding_prim(quotient, "quotient", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED);
  scheme_add_global_constant("quotient", p, env);

  p = scheme_make_folding_prim(rem_prim, "remainder", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED);
  scheme_add_global_constant("remainder", p, env);

  scheme_add_global_constant("quotient/remainder", 
			     scheme_make_prim_w_arity2(quotient_remainder,
						       "quotient/remainder", 
						       2, 2,
						       2, 2),
			     env);

  p = scheme_make_folding_prim(scheme_modulo, "modulo", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED);
  scheme_add_global_constant("modulo", p, env);
}

void scheme_init_flfxnum_numarith(Scheme_Env *env)
{
  Scheme_Object *p;
  int flags;

  p = scheme_make_folding_prim(fx_plus, "fx+", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_add_global_constant("fx+", p, env);

  p = scheme_make_folding_prim(fx_minus, "fx-", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_add_global_constant("fx-", p, env);

  p = scheme_make_folding_prim(fx_mult, "fx*", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_add_global_constant("fx*", p, env);

  p = scheme_make_folding_prim(fx_div, "fxquotient", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_add_global_constant("fxquotient", p, env);

  p = scheme_make_folding_prim(fx_rem, "fxremainder", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_add_global_constant("fxremainder", p, env);

  p = scheme_make_folding_prim(fx_mod, "fxmodulo", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_add_global_constant("fxmodulo", p, env);

  p = scheme_make_folding_prim(fx_abs, "fxabs", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED)
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM;
  scheme_add_global_constant("fxabs", p, env);

  p = scheme_make_folding_prim(fl_plus, "fl+", 2, 2, 1);
  if (scheme_can_inline_fp_op())
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_PRODUCES_FLONUM
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_add_global_constant("fl+", p, env);

  p = scheme_make_folding_prim(fl_minus, "fl-", 2, 2, 1);
  if (scheme_can_inline_fp_op())
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_PRODUCES_FLONUM
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_add_global_constant("fl-", p, env);

  p = scheme_make_folding_prim(fl_mult, "fl*", 2, 2, 1);
  if (scheme_can_inline_fp_op())
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_PRODUCES_FLONUM
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_add_global_constant("fl*", p, env);

  p = scheme_make_folding_prim(fl_div, "fl/", 2, 2, 1);
  if (scheme_can_inline_fp_op())
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_PRODUCES_FLONUM
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_add_global_constant("fl/", p, env);

  p = scheme_make_folding_prim(fl_abs, "flabs", 1, 1, 1);
  if (scheme_can_inline_fp_op())
    flags = SCHEME_PRIM_IS_UNARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_PRODUCES_FLONUM
                                                            | SCHEME_PRIM_WANTS_FLONUM_FIRST);
  scheme_add_global_constant("flabs", p, env);

  p = scheme_make_folding_prim(fl_sqrt, "flsqrt", 1, 1, 1);
  if (scheme_can_inline_fp_op() && SQRT_MACHINE_CODE_AVAILABLE)
    flags = SCHEME_PRIM_IS_UNARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_PRODUCES_FLONUM
                                                            | SCHEME_PRIM_WANTS_FLONUM_FIRST);
  scheme_add_global_constant("flsqrt", p, env);

}

void scheme_init_extfl_numarith(Scheme_Env *env)
{
  Scheme_Object *p;
  int flags;

  p = scheme_make_folding_prim(extfl_plus, "extfl+", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_op()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_PRODUCES_EXTFLONUM
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_add_global_constant("extfl+", p, env);

  p = scheme_make_folding_prim(extfl_minus, "extfl-", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_op()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_PRODUCES_EXTFLONUM
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_add_global_constant("extfl-", p, env);

  p = scheme_make_folding_prim(extfl_mult, "extfl*", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_op()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_PRODUCES_EXTFLONUM
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_add_global_constant("extfl*", p, env);

  p = scheme_make_folding_prim(extfl_div, "extfl/", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_op()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_PRODUCES_EXTFLONUM
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_add_global_constant("extfl/", p, env);

  p = scheme_make_folding_prim(extfl_abs, "extflabs", 1, 1, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_op()))
    flags = SCHEME_PRIM_IS_UNARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_PRODUCES_EXTFLONUM
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_FIRST);
  scheme_add_global_constant("extflabs", p, env);

  p = scheme_make_folding_prim(extfl_sqrt, "extflsqrt", 1, 1, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_op() && SQRT_MACHINE_CODE_AVAILABLE))
    flags = SCHEME_PRIM_IS_UNARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_PRODUCES_EXTFLONUM
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_FIRST);
  scheme_add_global_constant("extflsqrt", p, env);
}

void scheme_init_unsafe_numarith(Scheme_Env *env)
{
  Scheme_Object *p;
  int flags;

  p = scheme_make_folding_prim(unsafe_fx_plus, "unsafe-fx+", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_add_global_constant("unsafe-fx+", p, env);

  p = scheme_make_folding_prim(unsafe_fx_minus, "unsafe-fx-", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_add_global_constant("unsafe-fx-", p, env);

  p = scheme_make_folding_prim(unsafe_fx_mult, "unsafe-fx*", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_add_global_constant("unsafe-fx*", p, env);

  p = scheme_make_folding_prim(unsafe_fx_div, "unsafe-fxquotient", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_add_global_constant("unsafe-fxquotient", p, env);

  p = scheme_make_folding_prim(unsafe_fx_rem, "unsafe-fxremainder", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_add_global_constant("unsafe-fxremainder", p, env);

  p = scheme_make_folding_prim(unsafe_fx_mod, "unsafe-fxmodulo", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_add_global_constant("unsafe-fxmodulo", p, env);

  p = scheme_make_folding_prim(unsafe_fx_abs, "unsafe-fxabs", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_add_global_constant("unsafe-fxabs", p, env);


  p = scheme_make_folding_prim(unsafe_fl_plus, "unsafe-fl+", 2, 2, 1);
  if (scheme_can_inline_fp_op())
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_FLONUM
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_add_global_constant("unsafe-fl+", p, env);

  p = scheme_make_folding_prim(unsafe_fl_minus, "unsafe-fl-", 2, 2, 1);
  if (scheme_can_inline_fp_op())
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_FLONUM
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_add_global_constant("unsafe-fl-", p, env);

  p = scheme_make_folding_prim(unsafe_fl_mult, "unsafe-fl*", 2, 2, 1);
  if (scheme_can_inline_fp_op())
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_FLONUM
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_add_global_constant("unsafe-fl*", p, env);

  p = scheme_make_folding_prim(unsafe_fl_div, "unsafe-fl/", 2, 2, 1);
  if (scheme_can_inline_fp_op())
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_FLONUM
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_add_global_constant("unsafe-fl/", p, env);

  p = scheme_make_folding_prim(unsafe_fl_abs, "unsafe-flabs", 1, 1, 1);
  if (scheme_can_inline_fp_op())
    flags = SCHEME_PRIM_IS_UNARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_FLONUM
                                                            | SCHEME_PRIM_WANTS_FLONUM_FIRST);
  scheme_add_global_constant("unsafe-flabs", p, env);

  p = scheme_make_folding_prim(unsafe_fl_sqrt, "unsafe-flsqrt", 1, 1, 1);
  if (scheme_can_inline_fp_op() && SQRT_MACHINE_CODE_AVAILABLE)
    flags = SCHEME_PRIM_IS_UNARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_FLONUM
                                                            | SCHEME_PRIM_WANTS_FLONUM_FIRST);
  scheme_add_global_constant("unsafe-flsqrt", p, env);
}

void scheme_init_extfl_unsafe_numarith(Scheme_Env *env)
{
  Scheme_Object *p;
  int flags;

  p = scheme_make_folding_prim(unsafe_extfl_plus, "unsafe-extfl+", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_op()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_EXTFLONUM
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_add_global_constant("unsafe-extfl+", p, env);

  p = scheme_make_folding_prim(unsafe_extfl_minus, "unsafe-extfl-", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_op()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_EXTFLONUM
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_add_global_constant("unsafe-extfl-", p, env);

  p = scheme_make_folding_prim(unsafe_extfl_mult, "unsafe-extfl*", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_op()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_EXTFLONUM
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_add_global_constant("unsafe-extfl*", p, env);

  p = scheme_make_folding_prim(unsafe_extfl_div, "unsafe-extfl/", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_op()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_EXTFLONUM
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_add_global_constant("unsafe-extfl/", p, env);

  p = scheme_make_folding_prim(unsafe_extfl_abs, "unsafe-extflabs", 1, 1, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_op()))
    flags = SCHEME_PRIM_IS_UNARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_EXTFLONUM
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_FIRST);
  scheme_add_global_constant("unsafe-extflabs", p, env);

  p = scheme_make_folding_prim(unsafe_extfl_sqrt, "unsafe-extflsqrt", 1, 1, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_op() && SQRT_MACHINE_CODE_AVAILABLE))
    flags = SCHEME_PRIM_IS_UNARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_EXTFLONUM
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_FIRST);
  scheme_add_global_constant("unsafe-extflsqrt", p, env);
}

Scheme_Object *
scheme_add1 (int argc, Scheme_Object *argv[])
{
  Scheme_Type t;
  Scheme_Object *o = argv[0];

  if (SCHEME_INTP(o)) {
    intptr_t v;
    v = SCHEME_INT_VAL(o);
    if (v < 0x3FFFFFFF)
      return scheme_make_integer(v + 1);
    else {
      Small_Bignum b;
      return scheme_bignum_add1(scheme_make_small_bignum(v, &b));
    }
  }
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type)
    return scheme_make_float(SCHEME_FLT_VAL(o) + 1.0f);
#endif
  if (t == scheme_double_type)
    return scheme_make_double(SCHEME_DBL_VAL(o) + 1.0);
  if (t == scheme_bignum_type)
    return scheme_bignum_add1(o);
  if (t == scheme_rational_type)
    return scheme_rational_add1(o);
  if (t == scheme_complex_type)
    return scheme_complex_add1(o);

  NEED_NUMBER(add1);

  ESCAPED_BEFORE_HERE;
}

Scheme_Object *
scheme_sub1 (int argc, Scheme_Object *argv[])
{
  Scheme_Type t;
  Scheme_Object *o = argv[0];

  if (SCHEME_INTP(o)) {
    intptr_t v;
    v = SCHEME_INT_VAL(o);
    if (v > -(0x3FFFFFFF))
      return scheme_make_integer(SCHEME_INT_VAL(o) - 1);
    else {
      Small_Bignum b;
      return scheme_bignum_sub1(scheme_make_small_bignum(v, &b));
    }
  }
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type)
    return scheme_make_float(SCHEME_FLT_VAL(o) - 1.0f);
#endif
  if (t == scheme_double_type)
    return scheme_make_double(SCHEME_DBL_VAL(o) - 1.0);
  if (t == scheme_bignum_type)
    return scheme_bignum_sub1(o);
  if (t == scheme_rational_type)
    return scheme_rational_sub1(o);
  if (t == scheme_complex_type)
    return scheme_complex_sub1(o);
  
  NEED_NUMBER(sub1);

  ESCAPED_BEFORE_HERE;
}

#define F_ADD(x,y) scheme_make_double(x + y)
#define F_SUBTRACT(x,y) scheme_make_double(x - y)
#define F_MULTIPLY(x,y) scheme_make_double(x * y)
#define DIVIDE(x,y) scheme_make_fixnum_rational(x, y)
#define F_DIVIDE(x,y) scheme_make_double((double)x / (double)y)

#define FS_ADD(x,y) scheme_make_float(x + y)
#define FS_SUBTRACT(x,y) scheme_make_float(x - y)
#define FS_MULTIPLY(x,y) scheme_make_float(x * y)
#define FS_DIVIDE(x,y) scheme_make_float((float)x / (float)y)

static Scheme_Object *ADD_slow(intptr_t a, intptr_t b)
{
  Small_Bignum sa, sb;
  return scheme_bignum_add(scheme_make_small_bignum(a, &sa),
                           scheme_make_small_bignum(b, &sb));
}

static Scheme_Object *ADD(intptr_t a, intptr_t b)
{
  intptr_t r;
  Scheme_Object *o;

  r = a + b;

  o = scheme_make_integer(r);
  r = SCHEME_INT_VAL(o);

  if (b == r - a)
    return o;
  else
    return ADD_slow(a, b);
}

static Scheme_Object *SUBTRACT_slow(intptr_t a, intptr_t b)
{
  Small_Bignum sa, sb;
  return scheme_bignum_subtract(scheme_make_small_bignum(a, &sa),
                                scheme_make_small_bignum(b, &sb));  
}

static Scheme_Object *SUBTRACT(intptr_t a, intptr_t b)
{
  intptr_t r;
  Scheme_Object *o;

  r = a - b;

  o = scheme_make_integer(r);
  r = SCHEME_INT_VAL(o);

  if (a == r + b)
    return o;
  else
    return SUBTRACT_slow(a, b);
}

static Scheme_Object *MULTIPLY(intptr_t a, intptr_t b)
{
  intptr_t r;
  Scheme_Object *o;

  if (!b)
    return zeroi;

  r = a * b;

  o = scheme_make_integer(r);
  r = SCHEME_INT_VAL(o);

  if (a == r / b)
    return o;
  else {
    Small_Bignum sa, sb;
    return scheme_bignum_multiply(scheme_make_small_bignum(a, &sa),
				  scheme_make_small_bignum(b, &sb));
  }
}

static Scheme_Object *unary_minus(const Scheme_Object *n)
{
  Scheme_Object *a[1];
  a[0] = (Scheme_Object *)n;
  return minus(1, a);
}

#define ret_other(n1, n2) if (SAME_OBJ(n1, scheme_make_integer(0))) return (Scheme_Object *)n2
#define ret_1other(n1, n2) if (SAME_OBJ(n1, scheme_make_integer(1))) return (Scheme_Object *)n2
#define ret_zero(n1, n2) if (SAME_OBJ(n1, scheme_make_integer(0))) return scheme_make_integer(0)

GEN_BIN_OP(scheme_bin_plus, "+", ADD, F_ADD, FS_ADD, scheme_bignum_add, scheme_rational_add, scheme_complex_add, GEN_RETURN_N2, GEN_RETURN_N1, NO_NAN_CHECK, NO_NAN_CHECK, NO_NAN_CHECK, NO_NAN_CHECK, ret_other, cx_NO_CHECK, ret_other, cx_NO_CHECK)
GEN_BIN_OP(scheme_bin_minus, "-", SUBTRACT, F_SUBTRACT, FS_SUBTRACT, scheme_bignum_subtract, scheme_rational_subtract, scheme_complex_subtract, GEN_SINGLE_SUBTRACT_N2, GEN_RETURN_N1, NO_NAN_CHECK, NO_NAN_CHECK, NO_NAN_CHECK, NO_NAN_CHECK, cx_NO_CHECK, cx_NO_CHECK, ret_other, cx_NO_CHECK)
GEN_BIN_OP(scheme_bin_mult, "*", MULTIPLY, F_MULTIPLY, FS_MULTIPLY, scheme_bignum_multiply, scheme_rational_multiply, scheme_complex_multiply, GEN_RETURN_0, GEN_RETURN_0, NO_NAN_CHECK, NO_NAN_CHECK, NO_NAN_CHECK, NO_NAN_CHECK, ret_zero, ret_1other, ret_zero, ret_1other)
GEN_BIN_DIV_OP(scheme_bin_div, "/", DIVIDE, F_DIVIDE, FS_DIVIDE, scheme_make_rational, scheme_rational_divide, scheme_complex_divide, ret_zero, cx_NO_CHECK, cx_NO_CHECK, ret_1other)

GEN_NARY_OP(static, plus, "+", scheme_bin_plus, 0, SCHEME_NUMBERP, "number?", GEN_IDENT)
GEN_NARY_OP(static, mult, "*", scheme_bin_mult, 1, SCHEME_NUMBERP, "number?", GEN_IDENT)

static MZ_INLINE Scheme_Object *
minus_slow (Scheme_Object *ret, int argc, Scheme_Object *argv[])
{
  int i;
  for (i = 1; i < argc; i++) {
    Scheme_Object *o = argv[i];
    if (!SCHEME_NUMBERP(o)) {
      scheme_wrong_contract("-", "number?", i, argc, argv);
      ESCAPED_BEFORE_HERE;
    }
    ret = scheme_bin_minus(ret, o);
  }
  return ret;
}

static Scheme_Object *
minus (int argc, Scheme_Object *argv[])
{
  Scheme_Object *ret, *v;

  ret = argv[0];
  if (!SCHEME_NUMBERP(ret)) {
    scheme_wrong_contract("-", "number?", 0, argc, argv);
    ESCAPED_BEFORE_HERE;
  }
  if (argc == 1) {
    if (SCHEME_FLOATP(ret)) {
#ifdef MZ_USE_SINGLE_FLOATS
      if (SCHEME_FLTP(ret))
	return scheme_make_float(-SCHEME_FLT_VAL(ret));
#endif
      return scheme_make_double(-SCHEME_DBL_VAL(ret));
    }
    return scheme_bin_minus(zeroi, ret);
  }
  if (argc == 2) {
    v = argv[1];
    if (!SCHEME_NUMBERP(v)) {
      scheme_wrong_contract("-", "number?", 1, argc, argv);
      ESCAPED_BEFORE_HERE;
    } 
    return scheme_bin_minus(ret, v);
  }
  return minus_slow(ret, argc, argv);
}

static Scheme_Object *
div_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *ret;
  int i;

  ret = argv[0];
  if (!SCHEME_NUMBERP(ret)) {
    scheme_wrong_contract("/", "number?", 0, argc, argv);
    ESCAPED_BEFORE_HERE;
  }
  if (argc == 1) {
    if (ret != zeroi)
      return scheme_bin_div(scheme_make_integer(1), ret);
    else {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT_DIVIDE_BY_ZERO,
		       "/: division by zero");
      ESCAPED_BEFORE_HERE;
    }
  }
  for (i = 1; i < argc; i++) {
    Scheme_Object *o = argv[i];

    if (!SCHEME_NUMBERP(o)) {
      scheme_wrong_contract("/", "number?", i, argc, argv);
      ESCAPED_BEFORE_HERE;
    }

    if (o != zeroi)
      ret = scheme_bin_div(ret, o);
    else {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT_DIVIDE_BY_ZERO,
		       "/: division by zero");
      ESCAPED_BEFORE_HERE;
    }
  }
  return ret;
}

#define ABS(n)  ((n>0) ? n : -n)

Scheme_Object *
scheme_abs(int argc, Scheme_Object *argv[])
{
  Scheme_Type t;
  Scheme_Object *o;

  o = argv[0];

  if (SCHEME_INTP(o)) {
    intptr_t n = SCHEME_INT_VAL(o);
    return scheme_make_integer_value(ABS(n));
  } 
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type)
    return scheme_make_float(fabs(SCHEME_FLT_VAL(o)));
#endif
  if (t == scheme_double_type)
    return scheme_make_double(fabs(SCHEME_DBL_VAL(o)));
  if (t == scheme_bignum_type) {
    if (SCHEME_BIGPOS(o))
      return o;
    return scheme_bignum_negate(o);
  }
  if (t == scheme_rational_type) {
    if (scheme_is_rational_positive(o))
      return o;
    else
      return scheme_rational_negate(o);
  }

  NEED_REAL(abs);

  ESCAPED_BEFORE_HERE;
}

Scheme_Object *
do_bin_quotient(const char *name, const Scheme_Object *n1, const Scheme_Object *n2, Scheme_Object **bn_rem)
{
  Scheme_Object *q;

  if (!scheme_is_integer(n1)) {
    Scheme_Object *a[2];
    a[0] = (Scheme_Object *)n1;
    a[1] = (Scheme_Object *)n2;
    scheme_wrong_contract(name, "integer?", 0, 2, a);
  }
  if (!scheme_is_integer(n2)) {
    Scheme_Object *a[2];
    a[0] = (Scheme_Object *)n1;
    a[1] = (Scheme_Object *)n2;
    scheme_wrong_contract(name, "integer?", 1, 2, a);
  }

  if (SCHEME_INTP(n2) && !SCHEME_INT_VAL(n2))
    scheme_raise_exn(MZEXN_FAIL_CONTRACT_DIVIDE_BY_ZERO,
		     "%s: undefined for 0", name);
  if (
#ifdef MZ_USE_SINGLE_FLOATS
      (SCHEME_FLTP(n2) && (SCHEME_FLT_VAL(n2) == 0.0f)) ||
#endif
      (SCHEME_DBLP(n2) && (SCHEME_DBL_VAL(n2) == 0.0)))
    scheme_raise_exn(MZEXN_FAIL_CONTRACT_DIVIDE_BY_ZERO,
		     "%s: undefined for 0.0", name);

  if (SCHEME_INTP(n1) && SCHEME_INTP(n2)) {
    /* Beware that most negative fixnum divided by -1
       isn't a fixnum: */
    return (scheme_make_integer_value(SCHEME_INT_VAL(n1) / SCHEME_INT_VAL(n2)));
  }
  if (SCHEME_DBLP(n1) || SCHEME_DBLP(n2)) {
    Scheme_Object *r;
    double d, d2;

    r = scheme_bin_div(n1, n2); /* could be exact 0 ... */
    if (SCHEME_DBLP(r)) {
      d = SCHEME_DBL_VAL(r);
      
      if (d > 0)
	d2 = floor(d);
      else
	d2 = ceil(d);
      
      if (d2 == d)
	return r;
      else
	return scheme_make_double(d2);
    } else
      return r;
  }
#ifdef MZ_USE_SINGLE_FLOATS
  if (SCHEME_FLTP(n1) || SCHEME_FLTP(n2)) {
    Scheme_Object *r;
    float d, d2;

    r = scheme_bin_div(n1, n2); /* could be exact 0 ... */
    if (SCHEME_FLTP(r)) {
      d = SCHEME_FLT_VAL(r);
      
      if (d > 0)
	d2 = floor(d);
      else
	d2 = ceil(d);
      
      if (d2 == d)
	return r;
      else
	return scheme_make_float(d2);
    } else
      return r;
  }
#endif

#if 0
  /* I'm pretty sure this isn't needed, but I'm keeping the code just
     in case... 03/19/2000 */
  if (SCHEME_RATIONALP(n1))
    wrong_contract(name, "integer?", n1);
  if (SCHEME_RATIONALP(n2))
    wrong_contract(name, "integer?", n2);
#endif
  
  n1 = scheme_to_bignum(n1);
  n2 = scheme_to_bignum(n2);

  scheme_bignum_divide(n1, n2, &q, bn_rem, 1);
  return q;
}

Scheme_Object *
scheme_bin_quotient (const Scheme_Object *n1, const Scheme_Object *n2)
{
  return do_bin_quotient("quotient", n1, n2, NULL);
}

static Scheme_Object *
quotient (int argc, Scheme_Object *argv[])
{
  return do_bin_quotient("quotient", argv[0], argv[1], NULL);
}

/* Declaration is for FARPROC: */
static Scheme_Object *
rem_mod (int argc, Scheme_Object *argv[], char *name, int first_sign);

static Scheme_Object *
rem_mod (int argc, Scheme_Object *argv[], char *name, int first_sign)
{
  Scheme_Object *n1, *n2, *r;
  int negate;

  n1 = argv[0];
  n2 = argv[1];

  if (!scheme_is_integer(n1))
    scheme_wrong_contract(name, "integer?", 0, argc, argv);
  if (!scheme_is_integer(n2))
    scheme_wrong_contract(name, "integer?", 1, argc, argv);

  if (SCHEME_INTP(n2) && !SCHEME_INT_VAL(n2))
    scheme_raise_exn(MZEXN_FAIL_CONTRACT_DIVIDE_BY_ZERO,
		     "%s: undefined for 0", name);
  if (
#ifdef MZ_USE_SINGLE_FLOATS
      (SCHEME_FLTP(n2) && (SCHEME_FLT_VAL(n2) == 0.0f)) ||
#endif
      (SCHEME_DBLP(n2) && (SCHEME_DBL_VAL(n2) == 0.0))) {
    int neg;
    neg = scheme_minus_zero_p(SCHEME_FLOAT_VAL(n2));
    scheme_raise_exn(MZEXN_FAIL_CONTRACT_DIVIDE_BY_ZERO,
		     "%s: undefined for %s0.0",
		     name,
		     neg ? "-" : "");
  }

  if (SCHEME_INTP(n1) && !SCHEME_INT_VAL(n1))
    return zeroi;

  if (SCHEME_INTP(n1) && SCHEME_INTP(n2)) {
    intptr_t a, b, na, nb, v;
    int neg1, neg2;

    a = SCHEME_INT_VAL(n1);
    b = SCHEME_INT_VAL(n2);
    na =  (a < 0) ? -a : a;
    nb =  (b < 0) ? -b : b;

    v = na % nb;

    if (v) {
      if (first_sign) {
	if (a < 0)
	  v = -v;
      } else {
	neg1 = (a < 0);
	neg2 = (b < 0);
	
	if (neg1 != neg2)
	  v = nb - v;
	
	if (neg2)
	  v = -v;
      }
    }

    return scheme_make_integer(v);
  }

  if (SCHEME_FLOATP(n1) || SCHEME_FLOATP(n2)) {
    double a, b, na, nb, v;
#ifdef MZ_USE_SINGLE_FLOATS
    int was_single = !(SCHEME_DBLP(n1) || SCHEME_DBLP(n2));
#endif

    if (SCHEME_INTP(n1))
      a = SCHEME_INT_VAL(n1);
#ifdef MZ_USE_SINGLE_FLOATS
    else if (SCHEME_FLTP(n1))
      a = SCHEME_FLT_VAL(n1);
#endif
    else if (SCHEME_DBLP(n1))
      a = SCHEME_DBL_VAL(n1);
    else
      a = scheme_bignum_to_double(n1);

    if (SCHEME_INTP(n2))
      b = SCHEME_INT_VAL(n2);
#ifdef MZ_USE_SINGLE_FLOATS
    else if (SCHEME_FLTP(n2))
      b = SCHEME_FLT_VAL(n2);
#endif
    else if (SCHEME_DBLP(n2))
      b = SCHEME_DBL_VAL(n2);
    else
      b = scheme_bignum_to_double(n2);

    if (a == 0.0) {
      /* Avoid sign problems. */
#ifdef MZ_USE_SINGLE_FLOATS
      if (was_single)
	return scheme_zerof;
#endif
      return scheme_zerod;
    }

    na =  (a < 0) ? -a : a;
    nb =  (b < 0) ? -b : b;

    if (MZ_IS_POS_INFINITY(nb))
      v = na;
    else if (MZ_IS_POS_INFINITY(na)) {
#ifdef MZ_USE_SINGLE_FLOATS
      if (was_single)
	return scheme_zerof;
#endif
      return scheme_zerod;
    } else {
      v = fmod(na, nb);

#ifdef FMOD_CAN_RETURN_NEG_ZERO
      if (v == 0.0)
	v = 0.0;
#endif
    }

    if (v) {
      if (first_sign) {
        /* remainder */
	if (a < 0)
	  v = -v;
      } else {
        /* modulo */
	int neg1, neg2;
	
	neg1 = (a < 0);
	neg2 = (b < 0);
	
	if (neg1 != neg2)
	  v = nb - v;
	
	if (neg2)
	  v = -v;
      }
    }

#ifdef MZ_USE_SINGLE_FLOATS
    if (was_single)
      return scheme_make_float((float)v);
#endif

    return scheme_make_double(v);
  }

  n1 = scheme_to_bignum(n1);
  n2 = scheme_to_bignum(n2);

  scheme_bignum_divide(n1, n2, NULL, &r, 1);

  negate = 0;

  if (!SCHEME_INTP(r) || SCHEME_INT_VAL(r)) {
    /* Easier if we can assume 'r' is positive: */
    if (SCHEME_INTP(r)) {
      if (SCHEME_INT_VAL(r) < 0)
	r = scheme_make_integer(-SCHEME_INT_VAL(r));
    } else if (!SCHEME_BIGPOS(r))
      r = scheme_bignum_negate(r);

    if (first_sign) {
      if (!SCHEME_BIGPOS(n1))
	negate = 1;
    } else {
      int neg1, neg2;
      
      neg1 = !SCHEME_BIGPOS(n1);
      neg2 = !SCHEME_BIGPOS(n2);
      
      if (neg1 != neg2) {
	if (neg2)
	  r = scheme_bin_plus(n2, r);
	else
	  r = scheme_bin_minus(n2, r);
      } else if (neg2)
	negate = 1;
    }
    
    if (negate) {
      if (SCHEME_INTP(r))
	r = scheme_make_integer(-SCHEME_INT_VAL(r));
      else
	r = scheme_bignum_negate(r);
    }
  }

  return r;
}

static Scheme_Object *
rem_prim (int argc, Scheme_Object *argv[])
{
  return rem_mod(argc, argv, "remainder", 1);
}

Scheme_Object *
scheme_modulo(int argc, Scheme_Object *argv[])
{
  return rem_mod(argc, argv, "modulo", 0);
}


Scheme_Object *
quotient_remainder(int argc, Scheme_Object *argv[])
{
  Scheme_Object *rem = NULL, *quot, *a[2];

  quot = do_bin_quotient("quotient/remainder", argv[0], argv[1], &rem);
  if (!rem) {
    rem = rem_mod(argc, argv, "remainder", 1);
  }
  a[0] = quot;
  a[1] = rem;
  return scheme_values(2, a);
}

/************************************************************************/
/*                                Flfx                                  */
/************************************************************************/

#define CHECK_SECOND_ZERO(name) \
  if (!SCHEME_INT_VAL(argv[1])) \
    scheme_raise_exn(MZEXN_FAIL_CONTRACT_DIVIDE_BY_ZERO, \
		     name ": undefined for 0");

#ifdef SIXTY_FOUR_BIT_INTEGERS
static void check_always_fixnum(const char *name, Scheme_Object *o)
{
  if (SCHEME_INTP(o)) {
    intptr_t v = SCHEME_INT_VAL(o);
    if ((v < -1073741824) || (v > 1073741823)) {
      scheme_contract_error(name, 
                            "cannot fold to result that is not a fixnum on some platforms",
                            "result", 1, o,
                            NULL);
    }
  }
}
# define mzWHEN_64_BITS(e) e
#else
# define mzWHEN_64_BITS(e) /* empty */
#endif

#define SAFE_FX(name, s_name, scheme_op, EXTRA_CHECK)        \
 static Scheme_Object *name(int argc, Scheme_Object *argv[]) \
 {                                                           \
   Scheme_Object *o;                                         \
   if (!SCHEME_INTP(argv[0])) scheme_wrong_contract(s_name, "fixnum?", 0, argc, argv); \
   if (!SCHEME_INTP(argv[1])) scheme_wrong_contract(s_name, "fixnum?", 1, argc, argv); \
   EXTRA_CHECK                                               \
   o = scheme_op(argc, argv);                                \
   mzWHEN_64_BITS(if (scheme_current_thread->constant_folding) check_always_fixnum(s_name, o);) \
   if (!SCHEME_INTP(o)) scheme_non_fixnum_result(s_name, o); \
   return o;                                                 \
 }

SAFE_FX(fx_plus, "fx+", plus, )
SAFE_FX(fx_minus, "fx-", minus, )
SAFE_FX(fx_mult, "fx*", mult, )
SAFE_FX(fx_div, "fxquotient", quotient, CHECK_SECOND_ZERO("fxquotient"))
SAFE_FX(fx_rem, "fxremainder", rem_prim, CHECK_SECOND_ZERO("fxremainder"))
SAFE_FX(fx_mod, "fxmodulo", scheme_modulo, CHECK_SECOND_ZERO("fxmodulo"))

static Scheme_Object *fx_abs(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o;
  if (!SCHEME_INTP(argv[0])) scheme_wrong_contract("fxabs", "fixnum?", 0, argc, argv);
  o = scheme_abs(argc, argv);
  if (!SCHEME_INTP(o)) scheme_non_fixnum_result("fxabs", o);
  return o;
}

#define UNSAFE_FX(name, op, fold)                            \
 static Scheme_Object *name(int argc, Scheme_Object *argv[]) \
 {                                                           \
   intptr_t v;                                                   \
   if (scheme_current_thread->constant_folding) return fold(argc, argv);     \
   v = SCHEME_INT_VAL(argv[0]) op SCHEME_INT_VAL(argv[1]);   \
   return scheme_make_integer(v);                            \
 }

UNSAFE_FX(unsafe_fx_plus, +, fx_plus)
UNSAFE_FX(unsafe_fx_minus, -, fx_minus)
UNSAFE_FX(unsafe_fx_mult, *, fx_mult)
UNSAFE_FX(unsafe_fx_div, /, fx_div)
UNSAFE_FX(unsafe_fx_rem, %, fx_rem)

static Scheme_Object *unsafe_fx_mod(int argc, Scheme_Object *argv[])
{
  int neg1, neg2;
  intptr_t v, v1, av1, v2, av2;
  if (scheme_current_thread->constant_folding) return scheme_modulo(argc, argv);

  v1 = SCHEME_INT_VAL(argv[0]);
  v2 = SCHEME_INT_VAL(argv[1]);

  av1 = (v1 < 0) ? -v1 : v1;
  av2 = (v2 < 0) ? -v2 : v2;

  v = av1 % av2;
	
  if (v) {
    neg1 = (v1 < 0);
    neg2 = (v2 < 0);
  
    if (neg1 != neg2)
      v = av2 - v;
  
    if (neg2)
      v = -v;
  }

  return scheme_make_integer(v); 
}

static Scheme_Object *unsafe_fx_abs(int argc, Scheme_Object *argv[])
{
  intptr_t v;
  if (scheme_current_thread->constant_folding) return scheme_abs(argc, argv);
  v = SCHEME_INT_VAL(argv[0]);
  if (v < 0) v = -v;
  return scheme_make_integer(v);
}

#define UNSAFE_FL(name, op, fold)                            \
 static Scheme_Object *name(int argc, Scheme_Object *argv[]) \
 {                                                           \
   double v;                                                 \
   if (scheme_current_thread->constant_folding) return fold(argc, argv);     \
   v = SCHEME_DBL_VAL(argv[0]) op SCHEME_DBL_VAL(argv[1]);   \
   return scheme_make_double(v);                             \
 }

UNSAFE_FL(unsafe_fl_plus, +, plus)
UNSAFE_FL(unsafe_fl_minus, -, minus)
UNSAFE_FL(unsafe_fl_mult, *, mult)
UNSAFE_FL(unsafe_fl_div, /, div_prim)

#define UNSAFE_FL1(name, op, fold) \
 static Scheme_Object *name(int argc, Scheme_Object *argv[])  \
 {                                                            \
   double v;                                                              \
   if (scheme_current_thread->constant_folding) return fold(argc, argv);  \
   v = SCHEME_DBL_VAL(argv[0]);                                           \
   v = op(v);                                                             \
   return scheme_make_double(v);                                          \
 }

UNSAFE_FL1(unsafe_fl_abs, fabs, scheme_abs)

static Scheme_Object *pos_sqrt(int argc, Scheme_Object **argv)
{
  if (SCHEME_DBLP(argv[0]) && (SCHEME_DBL_VAL(argv[0]) < 0.0))
    return scheme_nan_object;
  return scheme_sqrt(argc, argv);
}

UNSAFE_FL1(unsafe_fl_sqrt, sqrt, pos_sqrt)

#define SAFE_FL(name, sname, op)                              \
 static Scheme_Object *name(int argc, Scheme_Object *argv[]) \
 {                                                           \
   double v;                                                 \
   if (!SCHEME_DBLP(argv[0])) scheme_wrong_contract(sname, "flonum?", 0, argc, argv); \
   if (!SCHEME_DBLP(argv[1])) scheme_wrong_contract(sname, "flonum?", 1, argc, argv); \
   v = SCHEME_DBL_VAL(argv[0]) op SCHEME_DBL_VAL(argv[1]);   \
   return scheme_make_double(v);                             \
 }

SAFE_FL(fl_plus, "fl+", +)
SAFE_FL(fl_minus, "fl-", -)
SAFE_FL(fl_mult, "fl*", *)
SAFE_FL(fl_div, "fl/", /)

#define SAFE_FL1(name, sname, op)                      \
 static Scheme_Object *name(int argc, Scheme_Object *argv[])  \
 {                                                            \
   double v;                                                              \
   if (!SCHEME_DBLP(argv[0])) scheme_wrong_contract(sname, "flonum?", 0, argc, argv); \
   v = SCHEME_DBL_VAL(argv[0]);                                           \
   v = op(v);                                                             \
   return scheme_make_double(v);                                          \
 }

SAFE_FL1(fl_abs, "flabs", fabs)
SAFE_FL1(fl_sqrt, "flsqrt", sqrt)

#ifdef MZ_LONG_DOUBLE
# define UNSAFE_EXTFL(name, op)                                          \
  static Scheme_Object *name(int argc, Scheme_Object *argv[])           \
  {                                                                     \
    long_double v;                                                      \
    CHECK_MZ_LONG_DOUBLE_UNSUPPORTED("unsafe-extfl" #op);               \
    v = op(SCHEME_LONG_DBL_VAL(argv[0]), SCHEME_LONG_DBL_VAL(argv[1]));   \
    return scheme_make_long_double(v);                                  \
  }
#else
# define UNSAFE_EXTFL(name, op)                                         \
  static Scheme_Object * name (int argc, Scheme_Object *argv[])         \
  {                                                                     \
    scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,                            \
                     "unsafe-extfl" #op ": " NOT_SUPPORTED_STR);        \
    return NULL;                                                        \
  }
#endif

UNSAFE_EXTFL(unsafe_extfl_plus, long_double_plus)
UNSAFE_EXTFL(unsafe_extfl_minus, long_double_minus)
UNSAFE_EXTFL(unsafe_extfl_mult, long_double_mult)
UNSAFE_EXTFL(unsafe_extfl_div, long_double_div)

#ifdef MZ_LONG_DOUBLE
# define UNSAFE_EXTFL1(name, op)                                        \
  static Scheme_Object *name(int argc, Scheme_Object *argv[])           \
  {                                                                     \
    long_double v;                                                      \
    v = SCHEME_LONG_DBL_VAL(argv[0]);                                   \
    v = op(v);                                                          \
    return scheme_make_long_double(v);                                  \
  }
#else
# define UNSAFE_EXTFL1(name, op) UNSAFE_EXTFL(name, op)
#endif

UNSAFE_EXTFL1(unsafe_extfl_abs, long_double_fabs)
UNSAFE_EXTFL1(unsafe_extfl_sqrt, long_double_sqrt)

#ifdef MZ_LONG_DOUBLE
# define SAFE_EXTFL(name, sname, op)                                     \
  static Scheme_Object *name(int argc, Scheme_Object *argv[])           \
  {                                                                     \
    long_double v;                                                      \
    CHECK_MZ_LONG_DOUBLE_UNSUPPORTED(sname);                            \
    if (!SCHEME_LONG_DBLP(argv[0])) scheme_wrong_contract(sname, "extflonum?", 0, argc, argv); \
    if (!SCHEME_LONG_DBLP(argv[1])) scheme_wrong_contract(sname, "extflonum?", 1, argc, argv); \
    v = op(SCHEME_LONG_DBL_VAL(argv[0]), SCHEME_LONG_DBL_VAL(argv[1]));   \
    return scheme_make_long_double(v);                                  \
  }
#else
# define SAFE_EXTFL(name, sname, op)                                   \
  static Scheme_Object * name (int argc, Scheme_Object *argv[])         \
  {                                                                     \
    scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,                            \
                     sname ": " NOT_SUPPORTED_STR);                     \
    return NULL;                                                        \
  }
#endif

SAFE_EXTFL(extfl_plus, "extfl+", long_double_plus)
SAFE_EXTFL(extfl_minus, "extfl-", long_double_minus)
SAFE_EXTFL(extfl_mult, "extfl*", long_double_mult)
SAFE_EXTFL(extfl_div, "extfl/", long_double_div)

#ifdef MZ_LONG_DOUBLE
# define SAFE_EXTFL1(name, sname, op)                                    \
  static Scheme_Object *name(int argc, Scheme_Object *argv[])           \
  {                                                                     \
   long_double v;                                                       \
   if (!SCHEME_LONG_DBLP(argv[0])) scheme_wrong_contract(sname, "extflonum?", 0, argc, argv); \
   v = SCHEME_LONG_DBL_VAL(argv[0]);                                    \
   v = op(v);                                                           \
   return scheme_make_long_double(v);                                   \
   }
#else
# define SAFE_EXTFL1(name, sname, op) SAFE_EXTFL(name, sname, op)
#endif

SAFE_EXTFL1(extfl_abs, "extflabs", long_double_fabs)
SAFE_EXTFL1(extfl_sqrt, "extflsqrt", long_double_sqrt)
