#include "schpriv.h"
#include "nummacs.h"
#include <math.h>

/* read only globals */
READ_ONLY Scheme_Object *scheme_unsafe_fx_eq_proc;
READ_ONLY Scheme_Object *scheme_unsafe_fx_lt_proc;
READ_ONLY Scheme_Object *scheme_unsafe_fx_gt_proc;
READ_ONLY Scheme_Object *scheme_unsafe_fx_lt_eq_proc;
READ_ONLY Scheme_Object *scheme_unsafe_fx_gt_eq_proc;
READ_ONLY Scheme_Object *scheme_unsafe_fx_min_proc;
READ_ONLY Scheme_Object *scheme_unsafe_fx_max_proc;

/* locals */
static Scheme_Object *eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *lt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *gt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *zero_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *positive_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *negative_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_max (int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_min (int argc, Scheme_Object *argv[]);

static Scheme_Object *fx_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *fx_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *fx_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *fx_lt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *fx_gt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *fx_min (int argc, Scheme_Object *argv[]);
static Scheme_Object *fx_max (int argc, Scheme_Object *argv[]);

static Scheme_Object *unsafe_fx_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fx_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fx_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fx_lt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fx_gt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fx_min (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fx_max (int argc, Scheme_Object *argv[]);

static Scheme_Object *fl_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_lt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_gt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_min (int argc, Scheme_Object *argv[]);
static Scheme_Object *fl_max (int argc, Scheme_Object *argv[]);

static Scheme_Object *unsafe_fl_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fl_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fl_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fl_lt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fl_gt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fl_min (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_fl_max (int argc, Scheme_Object *argv[]);

static Scheme_Object *extfl_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *extfl_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *extfl_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *extfl_lt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *extfl_gt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *extfl_min (int argc, Scheme_Object *argv[]);
static Scheme_Object *extfl_max (int argc, Scheme_Object *argv[]);

static Scheme_Object *unsafe_extfl_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_extfl_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_extfl_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_extfl_lt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_extfl_gt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_extfl_min (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_extfl_max (int argc, Scheme_Object *argv[]);

#define zeroi scheme_exact_zero

void scheme_init_numcomp(Scheme_Startup_Env *env)
{
  Scheme_Object *p;

  p = scheme_make_folding_prim(eq, "=", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_WANTS_NUMBER
                                                            | SCHEME_PRIM_OMITTABLE_ON_GOOD_ARGS
                                                            | SCHEME_PRIM_AD_HOC_OPT
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance("=", p, env);

  p = scheme_make_folding_prim(lt, "<", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_WANTS_REAL
                                                            | SCHEME_PRIM_OMITTABLE_ON_GOOD_ARGS
                                                            | SCHEME_PRIM_AD_HOC_OPT
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance("<", p, env);

  p = scheme_make_folding_prim(gt, ">", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_WANTS_REAL
                                                            | SCHEME_PRIM_OMITTABLE_ON_GOOD_ARGS
                                                            | SCHEME_PRIM_AD_HOC_OPT
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance(">", p, env);

  p = scheme_make_folding_prim(lt_eq, "<=", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_WANTS_REAL
                                                            | SCHEME_PRIM_OMITTABLE_ON_GOOD_ARGS
                                                            | SCHEME_PRIM_AD_HOC_OPT
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance("<=", p, env);

  p = scheme_make_folding_prim(gt_eq, ">=", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_WANTS_REAL
                                                            | SCHEME_PRIM_OMITTABLE_ON_GOOD_ARGS
                                                            | SCHEME_PRIM_AD_HOC_OPT
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance(">=", p, env);

  p = scheme_make_folding_prim(zero_p, "zero?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_WANTS_NUMBER
                                                            | SCHEME_PRIM_OMITTABLE_ON_GOOD_ARGS
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance("zero?", p, env);

  p = scheme_make_folding_prim(positive_p, "positive?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_WANTS_REAL
                                                            | SCHEME_PRIM_OMITTABLE_ON_GOOD_ARGS
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance("positive?", p, env);

  p = scheme_make_folding_prim(negative_p, "negative?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_WANTS_REAL
                                                            | SCHEME_PRIM_OMITTABLE_ON_GOOD_ARGS
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance("negative?", p, env);

  p = scheme_make_folding_prim(sch_max, "max", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_WANTS_REAL
                                                            | SCHEME_PRIM_OMITTABLE_ON_GOOD_ARGS
                                                            | SCHEME_PRIM_PRODUCES_REAL
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("max", p, env);

  p = scheme_make_folding_prim(sch_min, "min", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_WANTS_REAL
                                                            | SCHEME_PRIM_OMITTABLE_ON_GOOD_ARGS
                                                            | SCHEME_PRIM_PRODUCES_REAL
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("min", p, env);
}

void scheme_init_flfxnum_numcomp(Scheme_Startup_Env *env)
{
  Scheme_Object *p;
  int flags;

  p = scheme_make_folding_prim(fx_eq, "fx=", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance("fx=", p, env);

  p = scheme_make_folding_prim(fx_lt, "fx<", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance("fx<", p, env);

  p = scheme_make_folding_prim(fx_gt, "fx>", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance("fx>", p, env);

  p = scheme_make_folding_prim(fx_lt_eq, "fx<=", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance("fx<=", p, env);

  p = scheme_make_folding_prim(fx_gt_eq, "fx>=", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance("fx>=", p, env);

  p = scheme_make_folding_prim(fx_min, "fxmin", 1, -1, 1);
  if (scheme_can_inline_fp_comp())
    flags = (SCHEME_PRIM_IS_BINARY_INLINED | SCHEME_PRIM_IS_NARY_INLINED);
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("fxmin", p, env);
  
  p = scheme_make_folding_prim(fx_max, "fxmax", 1, -1, 1);
  if (scheme_can_inline_fp_comp())
    flags = (SCHEME_PRIM_IS_BINARY_INLINED | SCHEME_PRIM_IS_NARY_INLINED);
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("fxmax", p, env);


  p = scheme_make_folding_prim(fl_eq, "fl=", 1, -1, 1);
  if (scheme_can_inline_fp_comp())
    flags = (SCHEME_PRIM_IS_BINARY_INLINED | SCHEME_PRIM_IS_NARY_INLINED);
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_addto_prim_instance("fl=", p, env);

  p = scheme_make_folding_prim(fl_lt, "fl<", 1, -1, 1);
  if (scheme_can_inline_fp_comp())
    flags = (SCHEME_PRIM_IS_BINARY_INLINED | SCHEME_PRIM_IS_NARY_INLINED);
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_addto_prim_instance("fl<", p, env);

  p = scheme_make_folding_prim(fl_gt, "fl>", 1, -1, 1);
  if (scheme_can_inline_fp_comp())
    flags = (SCHEME_PRIM_IS_BINARY_INLINED | SCHEME_PRIM_IS_NARY_INLINED);
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_addto_prim_instance("fl>", p, env);

  p = scheme_make_folding_prim(fl_lt_eq, "fl<=", 1, -1, 1);
  if (scheme_can_inline_fp_comp())
    flags = (SCHEME_PRIM_IS_BINARY_INLINED | SCHEME_PRIM_IS_NARY_INLINED);
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_addto_prim_instance("fl<=", p, env);

  p = scheme_make_folding_prim(fl_gt_eq, "fl>=", 1, -1, 1);
  if (scheme_can_inline_fp_comp())
    flags = (SCHEME_PRIM_IS_BINARY_INLINED | SCHEME_PRIM_IS_NARY_INLINED);
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_addto_prim_instance("fl>=", p, env);

  p = scheme_make_folding_prim(fl_min, "flmin", 1, -1, 1);
  if (scheme_can_inline_fp_op())
    flags = (SCHEME_PRIM_IS_BINARY_INLINED | SCHEME_PRIM_IS_NARY_INLINED);
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_PRODUCES_FLONUM
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_addto_prim_instance("flmin", p, env);
  
  p = scheme_make_folding_prim(fl_max, "flmax", 1, -1, 1);
  if (scheme_can_inline_fp_op())
    flags = (SCHEME_PRIM_IS_BINARY_INLINED | SCHEME_PRIM_IS_NARY_INLINED);
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_PRODUCES_FLONUM
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_addto_prim_instance("flmax", p, env);
}

void scheme_init_extfl_numcomp(Scheme_Startup_Env *env)
{
  Scheme_Object *p;
  int flags;

  p = scheme_make_folding_prim(extfl_eq, "extfl=", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_comp()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_addto_prim_instance("extfl=", p, env);

  p = scheme_make_folding_prim(extfl_lt, "extfl<", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_comp()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_addto_prim_instance("extfl<", p, env);

  p = scheme_make_folding_prim(extfl_gt, "extfl>", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_comp()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_addto_prim_instance("extfl>", p, env);

  p = scheme_make_folding_prim(extfl_lt_eq, "extfl<=", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_comp()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_addto_prim_instance("extfl<=", p, env);

  p = scheme_make_folding_prim(extfl_gt_eq, "extfl>=", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_comp()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_addto_prim_instance("extfl>=", p, env);

  p = scheme_make_folding_prim(extfl_min, "extflmin", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_op()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_PRODUCES_EXTFLONUM
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_addto_prim_instance("extflmin", p, env);
  
  p = scheme_make_folding_prim(extfl_max, "extflmax", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_op()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_PRODUCES_EXTFLONUM
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_addto_prim_instance("extflmax", p, env);
}

void scheme_init_unsafe_numcomp(Scheme_Startup_Env *env)
{
  Scheme_Object *p;
  int flags;

  REGISTER_SO(scheme_unsafe_fx_eq_proc);
  p = scheme_make_folding_prim(unsafe_fx_eq, "unsafe-fx=", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_addto_prim_instance("unsafe-fx=", p, env);
  scheme_unsafe_fx_eq_proc = p;

  REGISTER_SO(scheme_unsafe_fx_lt_proc);
  p = scheme_make_folding_prim(unsafe_fx_lt, "unsafe-fx<", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_addto_prim_instance("unsafe-fx<", p, env);
  scheme_unsafe_fx_lt_proc = p;

  REGISTER_SO(scheme_unsafe_fx_gt_proc);
  p = scheme_make_folding_prim(unsafe_fx_gt, "unsafe-fx>", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_addto_prim_instance("unsafe-fx>", p, env);
  scheme_unsafe_fx_gt_proc = p;

  REGISTER_SO(scheme_unsafe_fx_lt_eq_proc);
  p = scheme_make_folding_prim(unsafe_fx_lt_eq, "unsafe-fx<=", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_addto_prim_instance("unsafe-fx<=", p, env);
  scheme_unsafe_fx_lt_eq_proc = p;

  REGISTER_SO(scheme_unsafe_fx_gt_eq_proc);
  p = scheme_make_folding_prim(unsafe_fx_gt_eq, "unsafe-fx>=", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_addto_prim_instance("unsafe-fx>=", p, env);
  scheme_unsafe_fx_gt_eq_proc = p;

  REGISTER_SO(scheme_unsafe_fx_min_proc);
  p = scheme_make_folding_prim(unsafe_fx_min, "unsafe-fxmin", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_addto_prim_instance("unsafe-fxmin", p, env);
  scheme_unsafe_fx_min_proc = p;

  REGISTER_SO(scheme_unsafe_fx_max_proc);
  p = scheme_make_folding_prim(unsafe_fx_max, "unsafe-fxmax", 1, -1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_addto_prim_instance("unsafe-fxmax", p, env);
  scheme_unsafe_fx_max_proc = p;

  p = scheme_make_folding_prim(unsafe_fl_eq, "unsafe-fl=", 1, -1, 1);
  if (scheme_can_inline_fp_comp())
    flags = (SCHEME_PRIM_IS_BINARY_INLINED | SCHEME_PRIM_IS_NARY_INLINED);
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_addto_prim_instance("unsafe-fl=", p, env);

  p = scheme_make_folding_prim(unsafe_fl_lt, "unsafe-fl<", 1, -1, 1);
  if (scheme_can_inline_fp_comp())
    flags = (SCHEME_PRIM_IS_BINARY_INLINED | SCHEME_PRIM_IS_NARY_INLINED);
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_addto_prim_instance("unsafe-fl<", p, env);

  p = scheme_make_folding_prim(unsafe_fl_gt, "unsafe-fl>", 1, -1, 1);
  if (scheme_can_inline_fp_comp())
    flags = (SCHEME_PRIM_IS_BINARY_INLINED | SCHEME_PRIM_IS_NARY_INLINED);
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_addto_prim_instance("unsafe-fl>", p, env);

  p = scheme_make_folding_prim(unsafe_fl_lt_eq, "unsafe-fl<=", 1, -1, 1);
  if (scheme_can_inline_fp_comp())
    flags = (SCHEME_PRIM_IS_BINARY_INLINED | SCHEME_PRIM_IS_NARY_INLINED);
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_addto_prim_instance("unsafe-fl<=", p, env);

  p = scheme_make_folding_prim(unsafe_fl_gt_eq, "unsafe-fl>=", 1, -1, 1);
  if (scheme_can_inline_fp_comp())
    flags = (SCHEME_PRIM_IS_BINARY_INLINED | SCHEME_PRIM_IS_NARY_INLINED);
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_addto_prim_instance("unsafe-fl>=", p, env);

  p = scheme_make_folding_prim(unsafe_fl_min, "unsafe-flmin", 1, -1, 1);
  if (scheme_can_inline_fp_op())
    flags = (SCHEME_PRIM_IS_BINARY_INLINED | SCHEME_PRIM_IS_NARY_INLINED);
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_FLONUM
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_addto_prim_instance("unsafe-flmin", p, env);
  
  p = scheme_make_folding_prim(unsafe_fl_max, "unsafe-flmax", 1, -1, 1);
  if (scheme_can_inline_fp_op())
    flags = (SCHEME_PRIM_IS_BINARY_INLINED | SCHEME_PRIM_IS_NARY_INLINED);
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_FLONUM
                                                            | SCHEME_PRIM_WANTS_FLONUM_BOTH);
  scheme_addto_prim_instance("unsafe-flmax", p, env);
}

void scheme_init_extfl_unsafe_numcomp(Scheme_Startup_Env *env)
{
  Scheme_Object *p;
  int flags;

  p = scheme_make_folding_prim(unsafe_extfl_eq, "unsafe-extfl=", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_comp()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_addto_prim_instance("unsafe-extfl=", p, env);

  p = scheme_make_folding_prim(unsafe_extfl_lt, "unsafe-extfl<", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_comp()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_addto_prim_instance("unsafe-extfl<", p, env);

  p = scheme_make_folding_prim(unsafe_extfl_gt, "unsafe-extfl>", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_comp()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_addto_prim_instance("unsafe-extfl>", p, env);

  p = scheme_make_folding_prim(unsafe_extfl_lt_eq, "unsafe-extfl<=", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_comp()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_addto_prim_instance("unsafe-extfl<=", p, env);

  p = scheme_make_folding_prim(unsafe_extfl_gt_eq, "unsafe-extfl>=", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_comp()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_addto_prim_instance("unsafe-extfl>=", p, env);

  p = scheme_make_folding_prim(unsafe_extfl_min, "unsafe-extflmin", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_op()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_EXTFLONUM
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_addto_prim_instance("unsafe-extflmin", p, env);
  
  p = scheme_make_folding_prim(unsafe_extfl_max, "unsafe-extflmax", 2, 2, 1);
  if (MZ_LONG_DOUBLE_AVAIL_AND(scheme_can_inline_fp_op()))
    flags = SCHEME_PRIM_IS_BINARY_INLINED;
  else
    flags = SCHEME_PRIM_SOMETIMES_INLINED;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(flags
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_EXTFLONUM
                                                            | SCHEME_PRIM_WANTS_EXTFLONUM_BOTH);
  scheme_addto_prim_instance("unsafe-extflmax", p, env);
}

/* Prototype needed for 3m conversion: */
static MZ_INLINE Scheme_Object *force_rat(Scheme_Object *n, Small_Rational *sr);

static MZ_INLINE Scheme_Object *force_rat(Scheme_Object *n, Small_Rational *sr)
  XFORM_SKIP_PROC
{
  Scheme_Type t = SCHEME_TYPE(n);
  if (t == scheme_rational_type)
    return n;
  else
    return scheme_make_small_bn_rational(n, sr);
}

GEN_NARY_COMP(eq, "=", scheme_bin_eq, SCHEME_NUMBERP, "number?")
GEN_NARY_COMP(lt, "<", scheme_bin_lt, SCHEME_REALP, "real?")
GEN_NARY_COMP(gt, ">", scheme_bin_gt, SCHEME_REALP, "real?")
GEN_NARY_COMP(lt_eq, "<=", scheme_bin_lt_eq, SCHEME_REALP, "real?")
GEN_NARY_COMP(gt_eq, ">=", scheme_bin_gt_eq, SCHEME_REALP, "real?")

#define EQUAL(x, y) (x == y)
#define LESS_THAN(x, y) (x < y)
#define GREATER_THAN(x, y) (x > y)
#define LESS_OR_EQUAL(x, y) (x <= y)
#define GREATER_OR_EQUAL(x, y) (x >= y)

#ifdef NAN_LT_COMPARISON_WRONG
# define fLESS_THAN(x, y) (!(x >= y) && (x == x) && (y == y))
# define fLESS_OR_EQUAL(x, y) (!(x > y) && (x == x) && (y == y))
#else
# define fLESS_THAN LESS_THAN
# define fLESS_OR_EQUAL LESS_OR_EQUAL
#endif

#define COMP_IZI_LT(a, b) scheme_bin_lt(IZI_REAL_PART(a), IZI_REAL_PART(b))
#define COMP_IZI_GT(a, b) scheme_bin_gt(IZI_REAL_PART(a), IZI_REAL_PART(b))
#define COMP_IZI_LT_EQ(a, b) scheme_bin_lt_eq(IZI_REAL_PART(a), IZI_REAL_PART(b))
#define COMP_IZI_GT_EQ(a, b) scheme_bin_gt_eq(IZI_REAL_PART(a), IZI_REAL_PART(b))

#define GEN_IDENT_FOR_IZI GEN_OMIT

GEN_BIN_COMP(scheme_bin_eq, "=", EQUAL, EQUAL, scheme_bignum_eq, scheme_rational_eq, scheme_complex_eq, 0, 0, scheme_is_inexact, scheme_is_inexact, GEN_IDENT, GEN_IDENT, "number?")
GEN_BIN_COMP(scheme_bin_lt, "<", LESS_THAN, fLESS_THAN, scheme_bignum_lt, scheme_rational_lt, COMP_IZI_LT, 0, 1, scheme_is_positive, scheme_is_negative, GEN_IDENT_FOR_IZI, GEN_OMIT, "real?")
GEN_BIN_COMP(scheme_bin_gt, ">", GREATER_THAN, GREATER_THAN, scheme_bignum_gt, scheme_rational_gt, COMP_IZI_GT, 1, 0, scheme_is_negative, scheme_is_positive, GEN_IDENT_FOR_IZI, GEN_OMIT, "real?")
GEN_BIN_COMP(scheme_bin_lt_eq, "<=", LESS_OR_EQUAL, fLESS_OR_EQUAL, scheme_bignum_le, scheme_rational_le, COMP_IZI_LT_EQ, 0, 1, scheme_is_positive, scheme_is_negative, GEN_IDENT_FOR_IZI, GEN_OMIT, "real?")
GEN_BIN_COMP(scheme_bin_gt_eq, ">=", GREATER_OR_EQUAL, GREATER_OR_EQUAL, scheme_bignum_ge, scheme_rational_ge, COMP_IZI_GT_EQ, 1, 0, scheme_is_negative, scheme_is_positive, GEN_IDENT_FOR_IZI, GEN_OMIT, "real?")

int
scheme_is_zero(const Scheme_Object *o)
{
  Scheme_Type t;

  if (SCHEME_INTP(o))
    return o == zeroi;
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type) {
# ifdef NAN_EQUALS_ANYTHING
    if (MZ_IS_NAN(SCHEME_FLT_VAL(o)))
      return 0;
# endif
    return SCHEME_FLT_VAL(o) == 0.0f;
  }
#endif
  if (t == scheme_double_type) {
#ifdef NAN_EQUALS_ANYTHING
    if (MZ_IS_NAN(SCHEME_DBL_VAL(o)))
      return 0;
#endif
    return SCHEME_DBL_VAL(o) == 0.0;
  }
  if (t == scheme_complex_type) {
    if (scheme_is_zero(scheme_complex_imaginary_part(o)))
      return scheme_is_zero(scheme_complex_real_part(o));
    return 0;
  }
  
  if ((t >= scheme_bignum_type) && (t <= scheme_complex_type))
    return 0;
 
  return -1;
}

Scheme_Object *
zero_p (int argc, Scheme_Object *argv[])
{
  int v;
  v = scheme_is_zero(argv[0]);
  if (v < 0) {
    NEED_NUMBER(zero?);
    ESCAPED_BEFORE_HERE;
  }
  return (v ? scheme_true : scheme_false);
}

int
scheme_is_positive(const Scheme_Object *o)
{
  Scheme_Type t;

  if (SCHEME_INTP(o))
    return SCHEME_INT_VAL(o) > 0;
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type) {
    float d = SCHEME_FLT_VAL(o);
# ifdef NAN_EQUALS_ANYTHING
    if (MZ_IS_NAN(d))
      return 0;
# endif
    return d > 0;
  }
#endif
  if (t == scheme_double_type) {
    double d = SCHEME_DBL_VAL(o);
#ifdef NAN_EQUALS_ANYTHING
    if (MZ_IS_NAN(d))
      return 0;
#endif
    return d > 0;
  }
  if (t == scheme_bignum_type)
    return SCHEME_BIGPOS(o);
  if (t == scheme_rational_type)
    return scheme_is_rational_positive(o);

  return -1;
}

Scheme_Object *
positive_p (int argc, Scheme_Object *argv[])
{
  int v;
  v = scheme_is_positive(argv[0]);
  if (v < 0) {
    NEED_REAL(positive?);
    ESCAPED_BEFORE_HERE;
  }
  return (v ? scheme_true : scheme_false);
}

int
scheme_is_negative(const Scheme_Object *o)
{
  Scheme_Type t;

  if (SCHEME_INTP(o))
    return SCHEME_INT_VAL(o) < 0;
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type) {
    float d = SCHEME_FLT_VAL(o);
# if defined(NAN_EQUALS_ANYTHING) || defined(NAN_LT_COMPARISON_WRONG)
    if (MZ_IS_NAN(d))
      return 0;
# endif
    return d < 0;
  }
#endif
  if (t == scheme_double_type) {
    double d = SCHEME_DBL_VAL(o);
# if defined(NAN_EQUALS_ANYTHING) || defined(NAN_LT_COMPARISON_WRONG)
    if (MZ_IS_NAN(d))
      return 0;
#endif
    return d < 0;
  }
  if (t == scheme_bignum_type)
    return !SCHEME_BIGPOS(o);
  if (t == scheme_rational_type)
    return !scheme_is_rational_positive(o);

  return -1;
}

Scheme_Object *
negative_p (int argc, Scheme_Object *argv[])
{
  int v;
  v = scheme_is_negative(argv[0]);
  if (v < 0) {
    NEED_REAL(negative?);
    ESCAPED_BEFORE_HERE;
  }
  return (v ? scheme_true : scheme_false);
}

#define MAX(n1,n2) scheme_make_integer((n1>n2) ? n1 : n2)
#define MIN(n1,n2) scheme_make_integer((n1<n2) ? n1 : n2)
#define F_MAX(n1,n2) scheme_make_double((n1>n2) ? n1 : n2)
#define F_MIN(n1,n2) scheme_make_double((n1<n2) ? n1 : n2)

#define FS_MAX(n1,n2) scheme_make_float((n1>n2) ? n1 : n2)
#define FS_MIN(n1,n2) scheme_make_float((n1<n2) ? n1 : n2)

#define MAX_IZI(a, b) bin_max(IZI_REAL_PART(a), IZI_REAL_PART(b))
#define MIN_IZI(a, b) bin_min(IZI_REAL_PART(a), IZI_REAL_PART(b))

static GEN_BIN_OP(bin_max, "max", MAX, F_MAX, FS_MAX, scheme_bignum_max, scheme_rational_max, MAX_IZI, GEN_OMIT, GEN_OMIT, NAN_RETURNS_NAN, NAN_RETURNS_SNAN, NAN_RETURNS_NAN, NAN_RETURNS_SNAN, cx_NO_CHECK, cx_NO_CHECK, cx_NO_CHECK, cx_NO_CHECK)
static GEN_BIN_OP(bin_min, "min", MIN, F_MIN, FS_MIN, scheme_bignum_min, scheme_rational_min, MIN_IZI, GEN_OMIT, GEN_OMIT, NAN_RETURNS_NAN, NAN_RETURNS_SNAN, NAN_RETURNS_NAN, NAN_RETURNS_SNAN, cx_NO_CHECK, cx_NO_CHECK, cx_NO_CHECK, cx_NO_CHECK)

GEN_TWOARY_OP(static, sch_max, "max", bin_max, SCHEME_REALP, "real?")
GEN_TWOARY_OP(static, sch_min, "min", bin_min, SCHEME_REALP, "real?")

/************************************************************************/
/*                                Flfx                                  */
/************************************************************************/

#define SAFE_FX_X(name, s_name, op, T, F, result_init, loop_arg, loop_F) \
 static Scheme_Object *name(int argc, Scheme_Object *argv[]) \
 {                                                           \
   if (!SCHEME_INTP(argv[0])) scheme_wrong_contract(s_name, "fixnum?", 0, argc, argv); \
   if (argc == 2) {                                                     \
     if (!SCHEME_INTP(argv[1])) scheme_wrong_contract(s_name, "fixnum?", 1, argc, argv); \
     if (SCHEME_INT_VAL(argv[0]) op SCHEME_INT_VAL(argv[1]))            \
       return T;                                                        \
     else                                                               \
       return F;                                                        \
   } else {                                                             \
     int i;                                                             \
     Scheme_Object *result = result_init;                               \
     for (i = 1; i < argc; i++) {                                       \
       if (!SCHEME_INTP(argv[i])) scheme_wrong_contract(s_name, "fixnum?", i, argc, argv); \
       if (!(SCHEME_INT_VAL(loop_arg) op SCHEME_INT_VAL(argv[i])))      \
         result = loop_F;                                               \
     }                                                                  \
     return result;                                                     \
   }                                                                    \
 }

#define SAFE_FX(name, s_name, op) SAFE_FX_X(name, s_name, op, scheme_true, scheme_false, scheme_true, argv[i-1], scheme_false)

SAFE_FX(fx_eq, "fx=", ==)
SAFE_FX(fx_lt, "fx<", <)
SAFE_FX(fx_gt, "fx>", >)
SAFE_FX(fx_lt_eq, "fx<=", <=)
SAFE_FX(fx_gt_eq, "fx>=", >=)
SAFE_FX_X(fx_min, "fxmin", <, argv[0], argv[1], argv[0], result, argv[i])
SAFE_FX_X(fx_max, "fxmax", >, argv[0], argv[1], argv[0], result, argv[i])

#define UNSAFE_FX_X(name, op, fold, T, F, result_init, loop_arg, loop_F) \
 static Scheme_Object *name(int argc, Scheme_Object *argv[]) \
 {                                                           \
   if (scheme_current_thread->constant_folding) return fold(argc, argv); \
   if (argc == 2) { \
     if (SCHEME_INT_VAL(argv[0]) op SCHEME_INT_VAL(argv[1])) \
       return T;                                             \
     else                                                    \
       return F;                                             \
   } else {                                                  \
     int i;                                                             \
     Scheme_Object *result = result_init;                               \
     for (i = 1; i < argc; i++) {                                       \
       if (!(SCHEME_INT_VAL(loop_arg) op SCHEME_INT_VAL(argv[i])))      \
         result = loop_F;                                               \
     }                                                                  \
     return result;                                                     \
   }                                                                    \
 }

#define UNSAFE_FX(name, op, fold) UNSAFE_FX_X(name, op, fold, scheme_true, scheme_false, scheme_true, argv[i-1], scheme_false)

UNSAFE_FX(unsafe_fx_eq, ==, eq)
UNSAFE_FX(unsafe_fx_lt, <, lt)
UNSAFE_FX(unsafe_fx_gt, >, gt)
UNSAFE_FX(unsafe_fx_lt_eq, <=, lt_eq)
UNSAFE_FX(unsafe_fx_gt_eq, >=, gt_eq)

UNSAFE_FX_X(unsafe_fx_min, <, sch_min, argv[0], argv[1], argv[0], result, argv[i])
UNSAFE_FX_X(unsafe_fx_max, >, sch_max, argv[0], argv[1], argv[0], result, argv[i])

#define SAFE_FL_X(name, s_name, op, T, F, PRE_CHECK, result_init, loop_arg, loop_F, LOOP_PRE_CHECK) \
 static Scheme_Object *name(int argc, Scheme_Object *argv[]) \
 {                                                           \
   if (!SCHEME_DBLP(argv[0])) scheme_wrong_contract(s_name, "flonum?", 0, argc, argv); \
   if (argc == 2) {                                                     \
     if (!SCHEME_DBLP(argv[1])) scheme_wrong_contract(s_name, "flonum?", 1, argc, argv); \
     PRE_CHECK                                                          \
     if (SCHEME_DBL_VAL(argv[0]) op SCHEME_DBL_VAL(argv[1]))            \
       return T;                                                        \
     else                                                               \
       return F;                                                        \
   } else {                                                             \
     int i;                                                             \
     Scheme_Object *result = result_init;                               \
     for (i = 1; i < argc; i++) {                                       \
       if (!SCHEME_DBLP(argv[i])) scheme_wrong_contract(s_name, "flonum?", i, argc, argv); \
       LOOP_PRE_CHECK                                                   \
       if (!(SCHEME_DBL_VAL(loop_arg) op SCHEME_DBL_VAL(argv[i])))      \
         result = loop_F;                                               \
     }                                                                  \
     return result;                                                     \
   }                                                                    \
 }

#define SAFE_FL(name, sname, op) SAFE_FL_X(name, sname, op, scheme_true, scheme_false, ;, scheme_true, argv[i-1], scheme_false, ;)

SAFE_FL(fl_eq, "fl=", ==)
SAFE_FL(fl_lt, "fl<", <)
SAFE_FL(fl_gt, "fl>", >)
SAFE_FL(fl_lt_eq, "fl<=", <=)
SAFE_FL(fl_gt_eq, "fl>=", >=)

#define CHECK_ARGV0_NAN { if (MZ_IS_NAN(SCHEME_DBL_VAL(argv[0])) || MZ_IS_NAN(SCHEME_DBL_VAL(argv[1]))) return scheme_nan_object; }
#define CHECK_ARGVi_NAN if (MZ_IS_NAN(SCHEME_DBL_VAL(result)) || MZ_IS_NAN(SCHEME_DBL_VAL(argv[i]))) { result = scheme_nan_object; } else

SAFE_FL_X(fl_min, "flmin", <, argv[0], argv[1], CHECK_ARGV0_NAN, argv[0], result, argv[i], CHECK_ARGVi_NAN)
SAFE_FL_X(fl_max, "flmax", >, argv[0], argv[1], CHECK_ARGV0_NAN, argv[0], result, argv[i], CHECK_ARGVi_NAN)

#define UNSAFE_FL_X(name, op, fold, T, F, PRE_CHECK, result_init, loop_arg, loop_F, LOOP_PRE_CHECK) \
 static Scheme_Object *name(int argc, Scheme_Object *argv[]) \
 {                                                           \
   if (scheme_current_thread->constant_folding) return fold(argc, argv); \
   if (argc == 2) { \
     PRE_CHECK                                               \
     if (SCHEME_DBL_VAL(argv[0]) op SCHEME_DBL_VAL(argv[1])) \
       return T;                                             \
     else                                                    \
       return F;                                             \
   } else {                                                  \
     int i;                                                             \
     Scheme_Object *result = result_init;                               \
     for (i = 1; i < argc; i++) {                                       \
       LOOP_PRE_CHECK                                                   \
       if (!(SCHEME_DBL_VAL(loop_arg) op SCHEME_DBL_VAL(argv[i])))      \
         result = loop_F;                                               \
     }                                                                  \
     return result;                                                     \
   }                                                                    \
 }

#define UNSAFE_FL_COMP(name, op, fold) UNSAFE_FL_X(name, op, fold, scheme_true, scheme_false, ;, scheme_true, argv[i-1], scheme_false, ;)

UNSAFE_FL_COMP(unsafe_fl_eq, ==, eq)
UNSAFE_FL_COMP(unsafe_fl_lt, <, lt)
UNSAFE_FL_COMP(unsafe_fl_gt, >, gt)
UNSAFE_FL_COMP(unsafe_fl_lt_eq, <=, lt_eq)
UNSAFE_FL_COMP(unsafe_fl_gt_eq, >=, gt_eq)

UNSAFE_FL_X(unsafe_fl_min, <, sch_min, argv[0], argv[1], CHECK_ARGV0_NAN, argv[0], result, argv[i], CHECK_ARGVi_NAN)
UNSAFE_FL_X(unsafe_fl_max, >, sch_max, argv[0], argv[1], CHECK_ARGV0_NAN, argv[0], result, argv[i], CHECK_ARGVi_NAN)

#ifdef MZ_LONG_DOUBLE
# define SAFE_EXTFL_X(name, sname, op, T, F, PRE_CHECK)	     \
 static Scheme_Object *name(int argc, Scheme_Object *argv[]) \
 {                                                           \
   CHECK_MZ_LONG_DOUBLE_UNSUPPORTED(sname);                             \
   if (!SCHEME_LONG_DBLP(argv[0])) scheme_wrong_contract(sname, "extflonum?", 0, argc, argv); \
   if (!SCHEME_LONG_DBLP(argv[1])) scheme_wrong_contract(sname, "extflonum?", 1, argc, argv); \
   PRE_CHECK                                                 \
   if (op(SCHEME_LONG_DBL_VAL(argv[0]), SCHEME_LONG_DBL_VAL(argv[1])))   \
     return T;                                               \
   else                                                      \
     return F;                                               \
 }
#else
# define SAFE_EXTFL_X(name, sname, op, T, F, PRE_CHECK)                 \
  static Scheme_Object * name(int argc, Scheme_Object *argv[])          \
  {                                                                     \
    scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,                            \
                     sname ": " NOT_SUPPORTED_STR);                     \
    return NULL;                                                        \
  }
#endif

#define SAFE_EXTFL(name, sname, op) SAFE_EXTFL_X(name, sname, op, scheme_true, scheme_false, ;)

SAFE_EXTFL(extfl_eq, "extfl=", long_double_eqv)
SAFE_EXTFL(extfl_lt, "extfl<", long_double_less)
SAFE_EXTFL(extfl_gt, "extfl>", long_double_greater)
SAFE_EXTFL(extfl_lt_eq, "extfl<=", long_double_less_or_eqv)
SAFE_EXTFL(extfl_gt_eq, "extfl>=", long_double_greater_or_eqv)

#define CHECK_ARGV0_LONG_NAN { if (MZ_IS_LONG_NAN(SCHEME_LONG_DBL_VAL(argv[0]))) return argv[0]; }

SAFE_EXTFL_X(extfl_min, "extflmin", long_double_less, argv[0], argv[1], CHECK_ARGV0_LONG_NAN)
SAFE_EXTFL_X(extfl_max, "extflmax", long_double_greater, argv[0], argv[1], CHECK_ARGV0_LONG_NAN)

#ifdef MZ_LONG_DOUBLE
/* Unsafe EXTFL comparisons. Return boolean */
/* removed if (scheme_current_thread->constant_folding) return (fold(argv[0], argv[1]) ? scheme_true : scheme_false); \ */
# define UNSAFE_EXTFL_COMP(name, op)         \
 static Scheme_Object *name(int argc, Scheme_Object *argv[]) \
 {                                                           \
   if (op(SCHEME_LONG_DBL_VAL(argv[0]), SCHEME_LONG_DBL_VAL(argv[1])))   \
     return scheme_true;                                     \
   else                                                      \
     return scheme_false;                                    \
 }

/* Unsafe EXTFL binary operators. Return extflonum */
/* removed if (scheme_current_thread->constant_folding) return (fold(argv[0], argv[1])); \ */
# define UNSAFE_EXTFL_BINOP(name, op, T, F, PRE_CHECK)         \
 static Scheme_Object *name(int argc, Scheme_Object *argv[]) \
 {                                                           \
   PRE_CHECK                                                 \
   CHECK_MZ_LONG_DOUBLE_UNSUPPORTED("extfl" #op);                             \
   if (op(SCHEME_LONG_DBL_VAL(argv[0]), SCHEME_LONG_DBL_VAL(argv[1])))   \
     return T;                                               \
   else                                                      \
     return F;                                               \
 }
#else
# define UNSAFE_EXTFL_COMP(name, op)                                    \
  static Scheme_Object *name(int argc, Scheme_Object *argv[])           \
  {                                                                     \
    scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,                            \
                     "unsafe-extfl" #op ": " NOT_SUPPORTED_STR);               \
    return NULL;                                                        \
  }
# define UNSAFE_EXTFL_BINOP(name, op, T, F, PRE_CHECK) UNSAFE_EXTFL_COMP(name, op)
#endif

UNSAFE_EXTFL_COMP(unsafe_extfl_eq, long_double_eqv)
UNSAFE_EXTFL_COMP(unsafe_extfl_lt, long_double_less)
UNSAFE_EXTFL_COMP(unsafe_extfl_gt, long_double_greater)
UNSAFE_EXTFL_COMP(unsafe_extfl_lt_eq, long_double_less_or_eqv)
UNSAFE_EXTFL_COMP(unsafe_extfl_gt_eq, long_double_greater_or_eqv)

UNSAFE_EXTFL_BINOP(unsafe_extfl_min, long_double_less, argv[0], argv[1], CHECK_ARGV0_LONG_NAN)
UNSAFE_EXTFL_BINOP(unsafe_extfl_max, long_double_greater, argv[0], argv[1], CHECK_ARGV0_LONG_NAN)

