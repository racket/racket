/*
  Racket
  Copyright (c) 2006-2012 PLT Scheme Inc.

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
*/


#include "schpriv.h"
#include "schmach.h"
#include "future.h"

#ifdef MZ_USE_JIT

#include "jit.h"

#define JITARITH_TS_PROCS
#include "jit_ts.c"

static int can_reorder_unboxing(Scheme_Object *rand, Scheme_Object *rand2)
{
  /* Can we reorder `rand' and `rand2', given that we want floating-point
     results (so it's ok for `rand' to be a floating-point local)? */
  return scheme_is_relatively_constant_and_avoids_r1_maybe_fp(rand, rand2, 1);
}

static int is_inline_unboxable_op(Scheme_Object *obj, int flag, int unsafely, int just_checking_result)
/* If unsafely, a result f 2 means that arguments should be checked safely. */
{
  if (!SCHEME_PRIMP(obj))
    return 0;
  if (!(SCHEME_PRIM_PROC_FLAGS(obj) & flag))
    return 0;

  if (IS_NAMED_PRIM(obj, "unsafe-fl+")) return 1;
  if (IS_NAMED_PRIM(obj, "unsafe-fl-")) return 1;
  if (IS_NAMED_PRIM(obj, "unsafe-fl*")) return 1;
  if (IS_NAMED_PRIM(obj, "unsafe-fl/")) return 1;
  if (IS_NAMED_PRIM(obj, "unsafe-flabs")) return 1;
  if (IS_NAMED_PRIM(obj, "unsafe-flsqrt")) return 1;
  if (IS_NAMED_PRIM(obj, "unsafe-flmin")) return 1;
  if (IS_NAMED_PRIM(obj, "unsafe-flmax")) return 1;
  if (IS_NAMED_PRIM(obj, "unsafe-fx->fl")) return 1;
  if (IS_NAMED_PRIM(obj, "unsafe-f64vector-ref")) return 1;
  if (IS_NAMED_PRIM(obj, "unsafe-flvector-ref")) return 1;
  if (IS_NAMED_PRIM(obj, "unsafe-flimag-part")) return 1;
  if (IS_NAMED_PRIM(obj, "unsafe-flreal-part")) return 1;

  if (unsafely) {
    /* These are inline-unboxable when their args are
       safely inline-unboxable: */
    if (IS_NAMED_PRIM(obj, "fl+")) return 2;
    if (IS_NAMED_PRIM(obj, "fl-")) return 2;
    if (IS_NAMED_PRIM(obj, "fl*")) return 2;
    if (IS_NAMED_PRIM(obj, "fl/")) return 2;
    if (IS_NAMED_PRIM(obj, "flabs")) return 2;
    if (IS_NAMED_PRIM(obj, "flsqrt")) return 2;
    if (IS_NAMED_PRIM(obj, "flmin")) return 2;
    if (IS_NAMED_PRIM(obj, "flmax")) return 2;
    if (IS_NAMED_PRIM(obj, "flimag-part")) return 2;
    if (IS_NAMED_PRIM(obj, "flreal-part")) return 2;

    if (just_checking_result) {
      if (IS_NAMED_PRIM(obj, "flfloor")) return 1;
      if (IS_NAMED_PRIM(obj, "flceiling")) return 1;
      if (IS_NAMED_PRIM(obj, "fltruncate")) return 1;
      if (IS_NAMED_PRIM(obj, "flround")) return 1;
      if (IS_NAMED_PRIM(obj, "flsin")) return 1;
      if (IS_NAMED_PRIM(obj, "flcos")) return 1;
      if (IS_NAMED_PRIM(obj, "fltan")) return 1;
      if (IS_NAMED_PRIM(obj, "flasin")) return 1;
      if (IS_NAMED_PRIM(obj, "flacos")) return 1;
      if (IS_NAMED_PRIM(obj, "flatan")) return 1;
      if (IS_NAMED_PRIM(obj, "fllog")) return 1;
      if (IS_NAMED_PRIM(obj, "flexp")) return 1;
    }
  }

  return 0;
}

int scheme_generate_pop_unboxed(mz_jit_state *jitter)
{
#if defined(MZ_USE_JIT_I386)
# if 1
  if (jitter->unbox_depth) {
    scheme_signal_error("internal error: scheme_generate_pop_unboxed() isn't right");
  }
  /* The code below doesn't work right because it's emitted *before*
     the test for failure. Adding it after the failure test means
     moving it to (something like) sjc.unbound_global_code. Meanwhile,
     the JIT doesn't currently actually try to reference globals when it has
     values on the FP stack. */
# else
  /* If we have some arguments pushed on the FP stack, we need
     to pop them off before escaping. */
  int i;
  for (i = jitter->unbox_depth; i--; ) {
    FSTPr(0);
  }
  CHECK_LIMIT();
# endif
#endif
  return 1;
}

static int is_unboxing_immediate(Scheme_Object *obj, int unsafely)
{
  Scheme_Type t;

  t = SCHEME_TYPE(obj);
  switch (t) {
  case scheme_local_type:
    if (SCHEME_LOCAL_FLAGS(obj) == SCHEME_LOCAL_FLONUM)
      return 1;
    return unsafely;
  case scheme_toplevel_type:
    /* Can generalize to allow any toplevel if scheme_generate_pop_unboxed() is fixed */
    if ((SCHEME_TOPLEVEL_FLAGS(obj) & SCHEME_TOPLEVEL_FLAGS_MASK) < SCHEME_TOPLEVEL_READY)
      return 0;
    return unsafely;
    break;
  case scheme_local_unbox_type:
    return unsafely;
    break;
  default:
    if (!unsafely)
      return SCHEME_FLOATP(obj);
    return (t > _scheme_values_types_);
  }
}

int scheme_can_unbox_inline(Scheme_Object *obj, int fuel, int regs, int unsafely)
/* Assuming that `arg' is [unsafely] assumed to produce a flonum, can we
   just unbox it without using more than `regs' registers? There
   cannot be any errors or function calls, unless we've specifically
   instrumented them to save/pop floating-point values before
   jumping. If the result is true, then arguments must be evaluated in
   order. */
{
  Scheme_Type t;

  if (!fuel) return 0;
  if (!regs) return 0;

  t = SCHEME_TYPE(obj);
  switch (t) {
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)obj;
      int ok_op;
      ok_op = is_inline_unboxable_op(app->rator, SCHEME_PRIM_IS_UNARY_INLINED, unsafely, 0);
      if (!ok_op)
        return 0;
      else if (ok_op == 2)
        unsafely = 0;
      return scheme_can_unbox_inline(app->rand, fuel - 1, regs, unsafely);
    }
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)obj;
      int ok_op;
      ok_op = is_inline_unboxable_op(app->rator, SCHEME_PRIM_IS_BINARY_INLINED, unsafely, 0);
      if (!ok_op)
        return 0;
      else if (ok_op == 2)
        unsafely = 0;
      if ((SCHEME_PRIM_PROC_FLAGS(app->rator) & SCHEME_PRIM_IS_BINARY_INLINED)
          && (IS_NAMED_PRIM(app->rator, "unsafe-f64vector-ref")
              || IS_NAMED_PRIM(app->rator, "unsafe-flvector-ref"))) {
        if (is_unboxing_immediate(app->rand1, 1)
            && is_unboxing_immediate(app->rand2, 1)) {
          return 1;
        }
      }
      if (!scheme_can_unbox_inline(app->rand1, fuel - 1, regs, unsafely))
        return 0;
      return scheme_can_unbox_inline(app->rand2, fuel - 1, regs - 1, unsafely);
    }    
  default:
    return is_unboxing_immediate(obj, unsafely);
  }
}

int scheme_can_unbox_directly(Scheme_Object *obj)
/* Used only when !can_unbox_inline(). Detects safe operations that
   produce flonums when they don't raise an exception. */
{
  Scheme_Type t;

  while (1) {
    t = SCHEME_TYPE(obj);
    switch (t) {
    case scheme_application2_type:
      {
        Scheme_App2_Rec *app = (Scheme_App2_Rec *)obj;
        if (is_inline_unboxable_op(app->rator, SCHEME_PRIM_IS_UNARY_INLINED, 1, 1))
          return 1;
        if (SCHEME_PRIMP(app->rator)
            && (SCHEME_PRIM_PROC_FLAGS(app->rator) & SCHEME_PRIM_IS_UNARY_INLINED)) {
          if (IS_NAMED_PRIM(app->rator, "->fl")
              || IS_NAMED_PRIM(app->rator, "fx->fl"))
            return 1;
        }
        return 0;
      }
      break;
    case scheme_application3_type:
      {
        Scheme_App3_Rec *app = (Scheme_App3_Rec *)obj;
        if (is_inline_unboxable_op(app->rator, SCHEME_PRIM_IS_BINARY_INLINED, 1, 1))
          return 1;
        if (SCHEME_PRIMP(app->rator)
            && (SCHEME_PRIM_PROC_FLAGS(app->rator) & SCHEME_PRIM_IS_BINARY_INLINED)) {
          if (IS_NAMED_PRIM(app->rator, "flvector-ref")) return 1;
        }
        return 0;
      }    
      break;
    case scheme_let_value_type:
      obj = ((Scheme_Let_Value *)obj)->body;
      break;
    case scheme_let_one_type:
      obj = ((Scheme_Let_One *)obj)->body;
      break;
    case scheme_let_void_type:
      obj = ((Scheme_Let_Void *)obj)->body;
      break;
    case scheme_letrec_type:
      obj = ((Scheme_Letrec *)obj)->body;
      break;
    default:
      return 0;
    }
  }
}

static jit_insn *generate_arith_slow_path(mz_jit_state *jitter, Scheme_Object *rator, 
					  jit_insn **_ref, jit_insn **_ref4,
                                          Branch_Info *for_branch, 
					  int orig_args, int reversed, int arith, int use_v, int v)
/* *_ref4 is place to set for where to jump (for true case, if for_branch) after completing;
   *_ref is place to set for where to jump for false if for_branch, result if !for_branch;
   result is place to jump to start slow path if fixnum attempt fails */
{
  GC_CAN_IGNORE jit_insn *ref, *ref4, *refslow;

  refslow = _jit.x.pc;

  (void)jit_movi_p(JIT_R2, ((Scheme_Primitive_Proc *)rator)->prim_val);
  if (for_branch) {
    scheme_prepare_branch_jump(jitter, for_branch);
    CHECK_LIMIT();
    ref4 = jit_patchable_movi_p(JIT_V1, jit_forward());
    mz_set_local_p(JIT_V1, JIT_LOCAL2);
  } else
    ref4 = NULL;
  ref = jit_patchable_movi_p(JIT_V1, jit_forward());

  if (orig_args == 1) {
    if (for_branch) {
      (void)jit_jmpi(sjc.call_original_unary_arith_for_branch_code);
    } else {
      (void)jit_jmpi(sjc.call_original_unary_arith_code);
    }
  } else {
    if (use_v) {
      (void)jit_movi_p(JIT_R1, scheme_make_integer(v));
      reversed = !reversed;
    }

    if (for_branch) {
      if (reversed) {
	(void)jit_jmpi(sjc.call_original_binary_rev_arith_for_branch_code);
      } else {
	(void)jit_jmpi(sjc.call_original_binary_arith_for_branch_code);
      }
    } else {
      if (reversed) {
	(void)jit_jmpi(sjc.call_original_binary_rev_arith_code);
      } else {
	(void)jit_jmpi(sjc.call_original_binary_arith_code);
      }
    }
  }

  *_ref = ref;
  *_ref4 = ref4;

  if (arith == ARITH_LSH) {
    /* Add tag back to first arg, just in case. See arithmetic-shift branch to refslow. */
    ref = _jit.x.pc;

    if (reversed || use_v) {
      jit_ori_l(JIT_R0, JIT_R0, 0x1);
    } else {
      jit_ori_l(JIT_R1, JIT_R1, 0x1);
    }

    __START_TINY_JUMPS__(1);
    (void)jit_jmpi(refslow);
    __END_TINY_JUMPS__(1);

    return ref;
  } else {
    return refslow;
  }
}

#ifdef SIXTY_FOUR_BIT_INTEGERS
# define SCHEME_INT_SMALL_ENOUGH(rand2) ((((intptr_t)rand2 & 0x7FFFFFFF) == (intptr_t)rand2) || (((intptr_t)rand2 & 0xFFFFFFFFF8000000) == 0xFFFFFFFFF8000000))
#else
# define SCHEME_INT_SMALL_ENOUGH(rand2) 1
#endif

static int can_fast_double(int arith, int cmp, int two_args)
{
#ifdef INLINE_FP_OPS
  if ((arith == ARITH_ADD)
      || (arith == ARITH_SUB)
      || (arith == ARITH_MUL)
      || (arith == ARITH_DIV)
      || (arith == ARITH_ABS)
      || (arith == ARITH_EX_INEX)
      || (arith == ARITH_SQRT)
      || (arith == ARITH_FLUNOP)
      || (arith == ARITH_INEX_EX))
    return 1;
#endif
#ifdef INLINE_FP_COMP
  if ((!arith && (cmp != CMP_EVENP) && (cmp != CMP_ODDP))
      || ((arith == ARITH_MIN) && two_args)
      || ((arith == ARITH_MAX) && two_args))
    return 1;
#endif

  return 0;
}

#ifdef CAN_INLINE_ALLOC
# ifdef JIT_USE_FP_OPS
#define DECL_FP_GLUE(op) static void call_ ## op(void) XFORM_SKIP_PROC {  \
    scheme_jit_save_fp = scheme_double_ ## op(scheme_jit_save_fp); }
DECL_FP_GLUE(sin)
DECL_FP_GLUE(cos)
DECL_FP_GLUE(tan)
DECL_FP_GLUE(asin)
DECL_FP_GLUE(acos)
DECL_FP_GLUE(atan)
DECL_FP_GLUE(exp)
DECL_FP_GLUE(log)
DECL_FP_GLUE(floor)
DECL_FP_GLUE(ceiling)
DECL_FP_GLUE(truncate)
DECL_FP_GLUE(round)
typedef void (*call_fp_proc)(void);

#define DECL_BIN_FP_GLUE(op) static void call_ ## op(void) XFORM_SKIP_PROC {  \
    scheme_jit_save_fp = scheme_double_ ## op(scheme_jit_save_fp, scheme_jit_save_fp2); }
DECL_BIN_FP_GLUE(expt)
typedef void (*call_fp_bin_proc)(void);
# endif
#endif

int scheme_generate_unboxing(mz_jit_state *jitter, int target)
{
  int fpr0;

  fpr0 = JIT_FPR_0(jitter->unbox_depth);
  jit_ldxi_d_fppush(fpr0, target, &((Scheme_Double *)0x0)->double_val);
  jitter->unbox_depth++;

  return 1;
}

int scheme_generate_alloc_double(mz_jit_state *jitter, int inline_retry)
/* value should be in JIT_FPR0; R0-R2 not saved; V1 used */
{
#ifdef INLINE_FP_OPS
# ifdef CAN_INLINE_ALLOC
  scheme_inline_alloc(jitter, sizeof(Scheme_Double), scheme_double_type, 0, 0, 1, inline_retry);
  CHECK_LIMIT();
  jit_addi_p(JIT_R0, JIT_V1, OBJHEAD_SIZE);
  (void)jit_stxi_d_fppop(&((Scheme_Double *)0x0)->double_val, JIT_R0, JIT_FPR0);
# else
  (void)mz_tl_sti_d_fppop(tl_scheme_jit_save_fp, JIT_FPR0, JIT_R0);
  JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
  mz_prepare(0);
  {
    GC_CAN_IGNORE jit_insn *refr;
    (void)mz_finish_lwe(ts_malloc_double, refr);
  }
  jit_retval(JIT_R0);
# endif
#endif
  return 1;
}

static int generate_double_arith(mz_jit_state *jitter, Scheme_Object *rator,
                                 int arith, int cmp, int reversed, int two_args, int second_const,
                                 jit_insn **_refd, jit_insn **_refdt, Branch_Info *for_branch,
                                 int branch_short, int unsafe_fl, int unboxed, int unboxed_result)
/* Unless unboxed, first arg is in JIT_R1, second in JIT_R0.
   If unboxed in push/pop mode, first arg is pushed before second.
   If unboxed in direct mode, first arg is in JIT_FPR0+depth
    and second is in JIT_FPR1+depth (which is backward). 
   Unboxed implies unsafe unless arith == ARITH_INEX_EX. */
{
#if defined(INLINE_FP_OPS) || defined(INLINE_FP_COMP)
  GC_CAN_IGNORE jit_insn *ref8, *ref9, *ref10, *refd, *refdt, *refs = NULL, *refs2 = NULL;
  int no_alloc = unboxed_result, need_post_pop = 0;

  if (!unsafe_fl && !unboxed) {
    /* Maybe they're doubles */
    __START_TINY_JUMPS__(1);
    if (two_args) {
      jit_orr_ul(JIT_R2, JIT_R0, JIT_R1);
      ref8 = jit_bmsi_ul(jit_forward(), JIT_R2, 0x1);
    } else
      ref8 = NULL;
    jit_ldxi_s(JIT_R2, JIT_R0, &((Scheme_Object *)0x0)->type);
    ref9 = jit_bnei_i(jit_forward(), JIT_R2, scheme_double_type);
    if (two_args) {
      jit_ldxi_s(JIT_R2, JIT_R1, &((Scheme_Object *)0x0)->type);
      ref10 = jit_bnei_i(jit_forward(), JIT_R2, scheme_double_type);
    } else
      ref10 = NULL;
    CHECK_LIMIT();
    __END_TINY_JUMPS__(1);
  } else {
    ref8 = ref9 = ref10 = NULL;
  }

  if (!two_args && !second_const && ((arith == ARITH_MUL) || ((arith == ARITH_DIV) && reversed))) {
    /* Special case: multiplication by exact 0 */
    (void)jit_movi_p(JIT_R0, scheme_make_integer(0));
  } else {
    /* Yes, they're doubles. First arg is in JIT_R1, second is in JIT_R0. 
       Put the first arg in fpr0 and second (if any) into fpr1. To work
       right with stacks, that means pushing the second argument first. */
    int fpr1, fpr0;

    fpr0 = JIT_FPR_0(jitter->unbox_depth);
    fpr1 = JIT_FPR_1(1+jitter->unbox_depth);

    if (two_args) {
      if (!unboxed)
        jit_ldxi_d_fppush(fpr1, JIT_R1, &((Scheme_Double *)0x0)->double_val);
    } else if ((arith == ARITH_SUB) && !second_const && reversed) {
      reversed = 0;
    } else if (arith == ARITH_ABS) {
      /* abs needs no extra number */
    } else if (arith == ARITH_SQRT) {
      /* sqrt needs no extra number */
    } else if (arith == ARITH_FLUNOP) {
      /* flround, flsin, etc. needs no extra number */
    } else if (arith == ARITH_EX_INEX) {
      /* exact->inexact needs no extra number */
    } else if (arith == ARITH_INEX_EX) {
      /* inexact->exact needs no extra number */
    } else {
      double d = second_const;
      mz_movi_d_fppush(fpr1, d, JIT_R2);
      reversed = !reversed;
      cmp = -cmp;
    }

    if (!unboxed) {
      if (arith != ARITH_EX_INEX) {
        jit_ldxi_d_fppush(fpr0, JIT_R0, &((Scheme_Double *)0x0)->double_val);
      }
    }

#ifdef DIRECT_FPR_ACCESS
    if (unboxed) {
      /* arguments are backward */
      reversed = !reversed;
      cmp = -cmp;
    }
#endif

    if (arith) {
      switch (arith) {
      case ARITH_ADD:
        jit_addr_d_fppop(fpr0, fpr0, fpr1);
        break;
      case ARITH_MUL:
        jit_mulr_d_fppop(fpr0, fpr0, fpr1);
        break;
      case ARITH_DIV:
        if (!reversed)
          jit_divrr_d_fppop(fpr0, fpr0, fpr1);
        else
          jit_divr_d_fppop(fpr0, fpr0, fpr1);
        break;
      case ARITH_SUB:
        {
          if (!two_args && !second_const && !reversed) {
            /* Need a special case to make sure that (- 0.0) => -0.0 */
            jit_negr_d_fppop(fpr0, fpr0);
          } else if (reversed)
            jit_subr_d_fppop(fpr0, fpr0, fpr1);
          else
            jit_subrr_d_fppop(fpr0, fpr0, fpr1);
        }
        break;
      case ARITH_MIN:
      case ARITH_MAX:
        {
          GC_CAN_IGNORE jit_insn *refc, *refn;
          __START_TINY_JUMPS__(1);
	  /* If R0 is nan, then copy to R1, ensuring nan result */
	  refn = jit_beqr_d(jit_forward(), fpr0, fpr0);
          if (unboxed)
            jit_movr_d_rel(fpr1, fpr0);
          else
            jit_movr_p(JIT_R1, JIT_R0);
	  mz_patch_branch(refn);
          if (arith == ARITH_MIN) {
            if (unboxed) {
              refc = jit_bltr_d(jit_forward(), fpr0, fpr1);
            } else {
              refc = jit_bltr_d_fppop(jit_forward(), fpr0, fpr1);
            }
          } else {
            if (unboxed) {
              refc = jit_bger_d(jit_forward(), fpr0, fpr1);
            } else {
              refc = jit_bger_d_fppop(jit_forward(), fpr0, fpr1);
            }
          }
          if (unboxed) {
            jit_movr_d_rel(fpr0, fpr1);
            need_post_pop = 1;
          } else
            jit_movr_p(JIT_R0, JIT_R1);
          mz_patch_branch(refc);
          __END_TINY_JUMPS__(1);
          if (!unboxed) {
            /* we've already set JIT_R0 */
            no_alloc = 1;
          }
        }
        break;
      case ARITH_ABS:
        jit_abs_d_fppop(fpr0, fpr0);
        break;
      case ARITH_EX_INEX: /* exact->inexact */
        /* no work to do, because argument is already inexact;
           no need to allocate, because argument is never unboxed,
           and it therefore already resides in R0 */
        no_alloc = 1;
        break;
      case ARITH_INEX_EX: /* inexact->exact */
        if (!unsafe_fl) {
          jit_movr_d_fppush(fpr1, fpr0);
        }
        jit_roundr_d_l_fppop(JIT_R1, fpr0);
        if (!unsafe_fl) {
          /* to check whether it fits in a fixnum, we
             need to convert back and check whether it
             is the same */
          if (unboxed)
            jit_movr_d_fppush(fpr1+1, fpr1); /* for slow path */
          jit_extr_l_d_fppush(fpr0, JIT_R1);
          __START_TINY_JUMPS__(1);
          refs = jit_bantieqr_d_fppop(jit_forward(), fpr0, fpr1);
          __END_TINY_JUMPS__(1);
          /* result still may not fit in a fixnum */
          jit_lshi_l(JIT_R2, JIT_R1, 1);
          jit_rshi_l(JIT_R2, JIT_R2, 1);
          __START_TINY_JUMPS__(1);
          refs2 = jit_bner_l(jit_forward(), JIT_R1, JIT_R2);
          __END_TINY_JUMPS__(1);
#ifndef DIRECT_FPR_ACCESS
          if (unboxed)
            jit_roundr_d_l_fppop(JIT_R1, fpr2); /* slow path won't be needed */
#endif
        }
        jit_lshi_l(JIT_R0, JIT_R1, 1);
        jit_ori_l(JIT_R0, JIT_R0, 0x1);
        no_alloc = 1;
        break;
      case ARITH_SQRT:
        jit_sqrt_d_fppop(fpr0, fpr0);
        break;
#ifdef CAN_INLINE_ALLOC
# ifdef JIT_USE_FP_OPS
      case ARITH_FLUNOP: /* flfloor, flsin, etc. */
        {
          call_fp_proc f;

          if (IS_NAMED_PRIM(rator, "flsin"))
            f = call_sin;
          else if (IS_NAMED_PRIM(rator, "flcos"))
            f = call_cos;
          else if (IS_NAMED_PRIM(rator, "fltan"))
            f = call_tan;
          else if (IS_NAMED_PRIM(rator, "flasin"))
            f = call_asin;
          else if (IS_NAMED_PRIM(rator, "flacos"))
            f = call_acos;
          else if (IS_NAMED_PRIM(rator, "flatan"))
            f = call_atan;
          else if (IS_NAMED_PRIM(rator, "flexp"))
            f = call_exp;
          else if (IS_NAMED_PRIM(rator, "fllog"))
            f = call_log;
          else if (IS_NAMED_PRIM(rator, "flfloor"))
            f = call_floor;
          else if (IS_NAMED_PRIM(rator, "flceiling"))
            f = call_ceiling;
          else if (IS_NAMED_PRIM(rator, "fltruncate"))
            f = call_truncate;
          else if (IS_NAMED_PRIM(rator, "flround"))
            f = call_round;
          else {
            scheme_signal_error("internal error: unknown flonum function");
            f = NULL;
          }
          (void)mz_tl_sti_d_fppop(tl_scheme_jit_save_fp, JIT_FPR0, JIT_R2);
          mz_prepare(0);
          (void)mz_finish(f);
          (void)mz_tl_ldi_d_fppush(JIT_FPR0, tl_scheme_jit_save_fp, JIT_R2);
        }
        break;
      case ARITH_EXPT: /* flexpt */
        {
          if (!reversed) {
            (void)mz_tl_sti_d_fppop(tl_scheme_jit_save_fp2, JIT_FPR0, JIT_R2);
            (void)mz_tl_sti_d_fppop(tl_scheme_jit_save_fp, JIT_FPR1, JIT_R2);
          } else {
            (void)mz_tl_sti_d_fppop(tl_scheme_jit_save_fp, JIT_FPR0, JIT_R2);
            (void)mz_tl_sti_d_fppop(tl_scheme_jit_save_fp2, JIT_FPR1, JIT_R2);
          }
          mz_prepare(0);
          (void)mz_finish(call_expt);
          (void)mz_tl_ldi_d_fppush(JIT_FPR0, tl_scheme_jit_save_fp, JIT_R2);          
        }
        break;
# endif
#endif
      default:
        break;
      }
      CHECK_LIMIT();

      if (!no_alloc) {
        mz_rs_sync(); /* needed if arguments were unboxed */
        scheme_generate_alloc_double(jitter, 0);
        CHECK_LIMIT();
#if defined(MZ_USE_JIT_I386)
        if (need_post_pop)
          FSTPr(0);
#endif
      } else if (unboxed_result) {
        jitter->unbox_depth++;
#if defined(MZ_USE_JIT_I386)
        if (need_post_pop) {
          FXCHr(1);
          FSTPr(0);
        }
#endif
      }
    } else {
      /* The "anti" variants below invert the branch. Unlike the "un" 
         variants, the "anti" variants invert the comparison result
         after the layer where +nan.0 always generates false. */
      __START_SHORT_JUMPS__(branch_short);
      if (for_branch) {
        scheme_prepare_branch_jump(jitter, for_branch);
        CHECK_LIMIT();
      }
      R0_FP_ADJUST(_jitl.r0_can_be_tmp++);
      switch (cmp) {
      case CMP_LT:
        refd = jit_bantigtr_d_fppop(jit_forward(), fpr0, fpr1);
        break;
      case CMP_LEQ:
        refd = jit_bantiger_d_fppop(jit_forward(), fpr0, fpr1);
        break;
      case CMP_EQUAL:
        refd = jit_bantieqr_d_fppop(jit_forward(), fpr0, fpr1);
        break;
      case CMP_GEQ:
        refd = jit_bantiler_d_fppop(jit_forward(), fpr0, fpr1);
        break;
      case CMP_GT:
        refd = jit_bantiltr_d_fppop(jit_forward(), fpr0, fpr1);
        break;
      default:
        refd = NULL;
        break;
      }
      R0_FP_ADJUST(_jitl.r0_can_be_tmp--);
      __END_SHORT_JUMPS__(branch_short);
      *_refd = refd;
    }
  }

  if (!unsafe_fl) {
    /* Jump to return result or true branch: */
    __START_SHORT_JUMPS__(branch_short);
    refdt = jit_jmpi(jit_forward());
    *_refdt = refdt;
    __END_SHORT_JUMPS__(branch_short);
  }

  if (!unsafe_fl) {
    /* No, they're not both doubles, or slow path is needed
       for some other reason. */
    __START_TINY_JUMPS__(1);
    if (!unboxed) {
      if (two_args) {
        mz_patch_branch(ref8);
        mz_patch_branch(ref10);
      }
      mz_patch_branch(ref9);
    }
    if (refs)
      mz_patch_branch(refs);
    if (refs2)
      mz_patch_branch(refs2);
    __END_TINY_JUMPS__(1);
  }
#endif
  
  return 1;
}

static int check_flonum_result(mz_jit_state *jitter, int reg, void *fail_code, Scheme_Object *rator)
/* Doesn't use R0 or R1, except for `reg' */
{
  /* Check for flonum result */
  GC_CAN_IGNORE jit_insn *ref, *reffail;

  mz_rs_sync();

  __START_TINY_JUMPS__(1);
  ref = jit_bmci_l(jit_forward(), reg, 0x1);
  __END_TINY_JUMPS__(1);

  reffail = _jit.x.pc;
  (void)jit_movi_p(JIT_V1, ((Scheme_Primitive_Proc *)rator)->prim_val);
  (void)jit_calli(fail_code);

  __START_TINY_JUMPS__(1);
  mz_patch_branch(ref);
  __END_TINY_JUMPS__(1);

  jit_ldxi_s(JIT_R2, JIT_R0, &((Scheme_Object *)0x0)->type);
  __START_SHORT_JUMPS__(1);
  (void)jit_bnei_i(reffail, JIT_R2, scheme_double_type);
  __END_SHORT_JUMPS__(1);
  CHECK_LIMIT();

  scheme_generate_unboxing(jitter, reg);

  return 1;
}

static void generate_modulo_setup(mz_jit_state *jitter, int branch_short, int a1, int a2)
/* r1 has two flags: bit 0 means two args have different sign; bit 1 means second arg is negative */
{
  GC_CAN_IGNORE jit_insn *refx;

  jit_movi_l(JIT_R1, 0x0);
  __START_INNER_TINY__(branch_short);
  refx = jit_bgei_l(jit_forward(), a1, 0);
  jit_negr_l(a1, a1);
  jit_movi_l(JIT_R1, 0x1);
  mz_patch_branch(refx);
  refx = jit_bgei_l(jit_forward(), a2, 0);
  jit_xori_l(JIT_R1, JIT_R1, 0x3);
  jit_negr_l(a2, a2);
  mz_patch_branch(refx);
  __END_INNER_TINY__(branch_short);
}

int scheme_generate_arith(mz_jit_state *jitter, Scheme_Object *rator, Scheme_Object *rand, Scheme_Object *rand2, 
			  int orig_args, int arith, int cmp, int v, 
                          Branch_Info *for_branch, int branch_short,
                          int unsafe_fx, int unsafe_fl, GC_CAN_IGNORE jit_insn *overflow_refslow)
/* needs de-sync */
/* Operation codes are defined in jit.h.
   Either arith is non-zero or it's a cmp; the value of each determines the operation:
        arith = 1 -> + or add1 (if !rand2)
        arith = -1 -> - or sub1
        arith = 2 -> *
        arith = -2 -> /
        arith = -3 -> quotient
        arith = -4 -> remainder
        arith = -5 -> modulo
        arith = 3 -> bitwise-and
        arith = 4 -> bitwise-ior
        arith = 5 -> bitwise-xor
        arith = 6 -> arithmetic-shift, fxlshift
        arith = -6 -> fxrshift
        arith = 7 -> bitwise-not
        arith = 9 -> min
        arith = 10 -> max
        arith = 11 -> abs
        arith = 12 -> exact->inexact
        arith = 13 -> sqrt
        arith = 14 -> unary floating-point op (consult `rator')
        arith = 15 -> inexact->exact
        cmp = 0 -> = or zero?
        cmp = +/-1 -> >=/<=
        cmp = +/-2 -> >/< or positive/negative?
        cmp = 3 -> bitwise-bit-test?
        cmp = +/-4 -> even?/odd?
   If rand is NULL, then we're generating part of the fast path for an
   nary arithmatic over a binary operator; the first argument is
   already in R0 (fixnum or min/max) or a floating-point register
   (flonum) and the second argument is in R1 (fixnum or min/max) or a
   floating-point register (flonum).
   For unsafe_fx or unsafe_fl, -1 means safe but specific to the type.
*/
{
  GC_CAN_IGNORE jit_insn *ref, *ref2, *ref3, *ref4, *refd = NULL, *refdt = NULL;
  GC_CAN_IGNORE jit_insn *refslow;
  int skipped, simple_rand, simple_rand2, reversed = 0;
  int has_fixnum_fast = 1, has_flonum_fast = 1;
  int inlined_flonum1, inlined_flonum2;

  LOG_IT(("inlined %s\n", rator ? ((Scheme_Primitive_Proc *)rator)->name : "???"));

  if (unsafe_fx < 0) {
    unsafe_fx = 0;
    has_flonum_fast = 0;
  }

  if (unsafe_fl) {
    if (!rand) {
      inlined_flonum1 = inlined_flonum2 = 1;
    } else {
      if (scheme_can_unbox_inline(rand, 5, JIT_FPR_NUM-2, unsafe_fl > 0))
        inlined_flonum1 = 1;
      else
        inlined_flonum1 = 0;
      if (!rand2 || scheme_can_unbox_inline(rand2, 5, JIT_FPR_NUM-3, unsafe_fl > 0))
        inlined_flonum2 = 1;
      else
        inlined_flonum2 = 0;
    }
  } else
    inlined_flonum1 = inlined_flonum2 = 0;

  if (unsafe_fl
#ifndef USE_FLONUM_UNBOXING
      && inlined_flonum1 && inlined_flonum2
#endif
      ) {
    /* Unboxed (and maybe unsafe) floating-point ops. */
    int args_unboxed = (((arith != ARITH_MIN) && (arith != ARITH_MAX)) || rand);
    int flonum_depth, fl_reversed = 0, can_direct1, can_direct2;

    if (inlined_flonum1 && inlined_flonum2 && (arith != ARITH_INEX_EX))
      /* safe can be implemented as unsafe */
      unsafe_fl = 1;
    
    if (!args_unboxed && rand)
      scheme_signal_error("internal error: invalid mode");

    if (inlined_flonum1 && !inlined_flonum2 && can_reorder_unboxing(rand, rand2)) {
      GC_CAN_IGNORE Scheme_Object *tmp;
      reversed = !reversed;
      cmp = -cmp;
      fl_reversed = 1;
      tmp = rand;
      rand = rand2;
      rand2 = tmp;
      inlined_flonum1 = 0;
      inlined_flonum2 = 1;
    }

    if (inlined_flonum1)
      can_direct1 = 2;
    else
      can_direct1 = scheme_can_unbox_directly(rand);
    if (inlined_flonum2)
      can_direct2 = 2;
    else 
      can_direct2 = scheme_can_unbox_directly(rand2);

    if (args_unboxed)
      jitter->unbox++;
    if (!rand) {
      CHECK_LIMIT();
      if (args_unboxed)
        flonum_depth = 2;
      else
        flonum_depth = 0;
    } else if (!rand2) {
      mz_runstack_skipped(jitter, 1);
      scheme_generate_unboxed(rand, jitter, can_direct1, (unsafe_fl > 0));
      CHECK_LIMIT();
      mz_runstack_unskipped(jitter, 1);
      if (!can_direct1 && (unsafe_fl <= 0)) {
        check_flonum_result(jitter, JIT_R0, sjc.fl1_fail_code, rator);
        CHECK_LIMIT();
      }
      flonum_depth = 1;
    } else {
#ifdef USE_FLONUM_UNBOXING
      int flostack = 0, flopos = 0;
#endif
      mz_runstack_skipped(jitter, 2);
      scheme_generate_unboxed(rand, jitter, can_direct1, (unsafe_fl > 0));
      CHECK_LIMIT();
      if (!(inlined_flonum1 && inlined_flonum2)) {
        if (!can_direct1 && (unsafe_fl <= 0)) {
          mz_pushr_p(JIT_R0);
        } else if (!inlined_flonum2) {
#ifdef USE_FLONUM_UNBOXING
          flostack = scheme_mz_flostack_save(jitter, &flopos);
          --jitter->unbox_depth;
          scheme_generate_flonum_local_unboxing(jitter, 0);
          CHECK_LIMIT();
#endif        
        }
      }
      scheme_generate_unboxed(rand2, jitter, can_direct2, (unsafe_fl > 0));
      CHECK_LIMIT();
      if (!(inlined_flonum1 && inlined_flonum2)) {
        if ((can_direct1 || (unsafe_fl > 0)) && !inlined_flonum2) {
#ifdef USE_FLONUM_UNBOXING
          int fpr0;
          fpr0 = JIT_FPR_0(jitter->unbox_depth);
          mz_ld_fppush(fpr0, jitter->flostack_offset);
          scheme_mz_flostack_restore(jitter, flostack, flopos, 1, 1);
          CHECK_LIMIT();
          jitter->unbox_depth++;
#endif
        }
        if (!can_direct2 && (unsafe_fl <= 0)) {
          jit_movr_p(JIT_R1, JIT_R0);
          if (!can_direct1) {
            mz_popr_p(JIT_R0);
            check_flonum_result(jitter, JIT_R0, sjc.fl2rr_fail_code[fl_reversed], rator);
            CHECK_LIMIT();
          }
          check_flonum_result(jitter, JIT_R1, sjc.fl2fr_fail_code[fl_reversed], rator);
          CHECK_LIMIT();
        } else {
          if (!can_direct1 && (unsafe_fl <= 0)) {
            mz_popr_p(JIT_R0);
            check_flonum_result(jitter, JIT_R0, sjc.fl2rf_fail_code[fl_reversed], rator);
            CHECK_LIMIT();
          }
          if (!(can_direct1 || (unsafe_fl > 0)) || !inlined_flonum2) {
            cmp = -cmp;
            reversed = !reversed;
          }
        }
      }
      mz_runstack_unskipped(jitter, 2);
      flonum_depth = 2;
    }
    if (args_unboxed)
      --jitter->unbox;
    jitter->unbox_depth -= flonum_depth;
    if (!jitter->unbox && jitter->unbox_depth && rand)
      scheme_signal_error("internal error: broken unbox depth");
    if (for_branch
        || (arith == ARITH_INEX_EX)) /* has slow path */
      mz_rs_sync(); /* needed if arguments were unboxed */

    generate_double_arith(jitter, rator, arith, cmp, reversed, !!rand2, 0,
                          &refd, &refdt, for_branch, branch_short, 
                          (arith == ARITH_INEX_EX) ? (unsafe_fl > 0) : 1, 
                          args_unboxed, jitter->unbox);
    CHECK_LIMIT();
    ref3 = NULL;
    ref = NULL;
    ref4 = NULL;

    if ((arith == ARITH_INEX_EX) && (unsafe_fl < 1)) {
      /* need a slow path */
      if (args_unboxed) {
        (void)jit_calli(sjc.box_flonum_from_reg_code);
      }
      generate_arith_slow_path(jitter, rator, &ref, &ref4, for_branch, orig_args, reversed, arith, 0, 0);
      /* assert: !ref4, since not for_branch */
      jit_patch_movi(ref, (_jit.x.pc));
      __START_SHORT_JUMPS__(branch_short);
      mz_patch_ucbranch(refdt);
      __END_SHORT_JUMPS__(branch_short);
    }

    __START_SHORT_JUMPS__(branch_short);
  } else {
    int unbox = jitter->unbox;

    if (unsafe_fl < 0) {
      has_fixnum_fast = 0;
      unsafe_fl = 0;
    }

    /* While generating a fixnum op, don't unbox! */
    jitter->unbox = 0;

    if (!rand) {
      /* generating for an nary operation; first arg in R0, 
         second in R1 */
      reversed = 1;
      cmp = -cmp;
      refslow = overflow_refslow;
      refd = NULL;
      refdt = NULL;
      ref3 = NULL;
      ref = NULL;
      ref4 = NULL;
    } else {
      if (rand2) {
        if (SCHEME_INTP(rand2)
            && SCHEME_INT_SMALL_ENOUGH(rand2)
            && ((arith != ARITH_LSH)
                || ((SCHEME_INT_VAL(rand2) <= MAX_TRY_SHIFT)
                    && (SCHEME_INT_VAL(rand2) >= -MAX_TRY_SHIFT)))
            && ((cmp != 3)
                || ((SCHEME_INT_VAL(rand2) <= MAX_TRY_SHIFT)
                    && (SCHEME_INT_VAL(rand2) >= 0)))) {
          /* Second is constant, so use constant mode.
             For arithmetic shift, only do this if the constant
             is in range. */
          v = SCHEME_INT_VAL(rand2);
          rand2 = NULL;
        } else if (SCHEME_INTP(rand)
                   && SCHEME_INT_SMALL_ENOUGH(rand)
                   && (arith != ARITH_LSH) && (arith != ARITH_RSH)
                   && (cmp != CMP_BIT)) {
          /* First is constant; swap argument order and use constant mode. */
          v = SCHEME_INT_VAL(rand);
          cmp = -cmp;
          rand = rand2;
          rand2 = NULL;
          reversed = 1;
        } else if ((scheme_ok_to_move_local(rand2)
                    || SCHEME_INTP(rand2))
                   && !(scheme_ok_to_move_local(rand)
                        || SCHEME_INTP(rand))) {
          /* Second expression is side-effect-free, unlike the first; 
             swap order and use the fast path for when the first arg is
             side-effect free. */
          Scheme_Object *t = rand2;
          rand2 = rand;
          rand = t;
          cmp = -cmp;
          reversed = 1;
        }
      }

      if ((arith == ARITH_SUB) && (orig_args == 1) && !v) {
        /* Unary subtract */
        reversed = 1;
      }
      
      if (rand2) {
        simple_rand = (scheme_ok_to_move_local(rand)
                       || SCHEME_INTP(rand));
        simple_rand2 = (SAME_TYPE(SCHEME_TYPE(rand2), scheme_local_type)
                        && (SCHEME_GET_LOCAL_FLAGS(rand2) != SCHEME_LOCAL_FLONUM));
        if (simple_rand && simple_rand2) {
          if (mz_CURRENT_REG_STATUS_VALID()
              && (jitter->r0_status >= 0)
              && !(SAME_TYPE(SCHEME_TYPE(rand), scheme_local_type)
                   && SCHEME_LOCAL_POS(rand) == SCHEME_LOCAL_POS(rand2))) {
            /* prefer to evaluate the rand2 second, so that we can use R0 if 
               it's helpful to set up R1 as rand */
            simple_rand = 0;
          } else
            simple_rand2 = 0;
        }
      } else {
        simple_rand = 0;
        simple_rand2 = 0;
      }

      if (rand2 && !simple_rand && !simple_rand2)
        skipped = orig_args - 1;    
      else
        skipped = orig_args;

      mz_runstack_skipped(jitter, skipped);

      if (rand2 && !simple_rand && !simple_rand2) {
        mz_runstack_skipped(jitter, 1);
        scheme_generate_non_tail(rand, jitter, 0, 1, 0); /* sync'd later */
        CHECK_LIMIT();
        mz_runstack_unskipped(jitter, 1);
        mz_rs_dec(1);
        CHECK_RUNSTACK_OVERFLOW();
        mz_runstack_pushed(jitter, 1);
        mz_rs_str(JIT_R0);
      }
      /* not sync'd... */

      if (simple_rand2) {
        if (SAME_TYPE(SCHEME_TYPE(rand), scheme_local_type))
          scheme_generate(rand, jitter, 0, 0, 0, JIT_R1, NULL); /* sync'd below */
        else {
          scheme_generate_non_tail(rand, jitter, 0, 1, 0); /* sync'd below */
          CHECK_LIMIT();
          jit_movr_p(JIT_R1, JIT_R0);
        }
        CHECK_LIMIT();
        scheme_generate(rand2, jitter, 0, 0, 0, JIT_R0, NULL); /* sync'd below */
      } else {
        scheme_generate_non_tail(rand2 ? rand2 : rand, jitter, 0, 1, 0); /* sync'd below */
      }
      CHECK_LIMIT();
      /* sync'd in three branches below */

      /* rand2 in R0, and rand in R1 unless it's simple */

      if (simple_rand || simple_rand2) {
        int va;

        if (simple_rand && SCHEME_INTP(rand)) {
          (void)jit_movi_p(JIT_R1, rand);
          va = JIT_R0;
        } else {
          if (simple_rand) {
            scheme_generate(rand, jitter, 0, 0, 0, JIT_R1, NULL);
          }
          if (!unsafe_fx && !unsafe_fl) {
            /* check both fixnum bits at once by ANDing into R2: */
            jit_andr_ul(JIT_R2, JIT_R0, JIT_R1);
            va = JIT_R2;
          }
        }

        if (!unsafe_fx && !unsafe_fl) {
          mz_rs_sync();

          __START_TINY_JUMPS_IF_COMPACT__(1);
          ref2 = jit_bmsi_ul(jit_forward(), va, 0x1);
          __END_TINY_JUMPS_IF_COMPACT__(1);
        } else {
          ref2 = NULL;
          if (for_branch) mz_rs_sync();
        }

        if (unsafe_fl || (!unsafe_fx && !SCHEME_INTP(rand) 
                          && has_flonum_fast 
                          && can_fast_double(arith, cmp, 1))) {
          /* Maybe they're both doubles... */
          if (unsafe_fl) mz_rs_sync();
          generate_double_arith(jitter, rator, arith, cmp, reversed, 1, 0, &refd, &refdt, 
                                for_branch, branch_short, unsafe_fl, 0, unbox);
          CHECK_LIMIT();
        }

        if (!unsafe_fx && !unsafe_fl) {
          if (!has_fixnum_fast) {
            __START_TINY_JUMPS_IF_COMPACT__(1);
            mz_patch_branch(ref2);
            __END_TINY_JUMPS_IF_COMPACT__(1);
          }

          /* Slow path */
          refslow = generate_arith_slow_path(jitter, rator, &ref, &ref4, for_branch, orig_args, reversed, arith, 0, 0);

          if (has_fixnum_fast) {
            __START_TINY_JUMPS_IF_COMPACT__(1);
            mz_patch_branch(ref2);
            __END_TINY_JUMPS_IF_COMPACT__(1);
          }
        } else {
          refslow = overflow_refslow;
          ref = NULL;
          ref4 = NULL;
        }
        CHECK_LIMIT();
      } else if (rand2) {
        /* Move rand result back into R1 */
        mz_rs_ldr(JIT_R1);
        mz_rs_inc(1);
        mz_runstack_popped(jitter, 1);

        if (!unsafe_fx && !unsafe_fl) {
          mz_rs_sync();

          /* check both fixnum bits at once by ANDing into R2: */
          jit_andr_ul(JIT_R2, JIT_R0, JIT_R1);
          __START_TINY_JUMPS_IF_COMPACT__(1);
          ref2 = jit_bmsi_ul(jit_forward(), JIT_R2, 0x1);
          __END_TINY_JUMPS_IF_COMPACT__(1);
          CHECK_LIMIT();
        } else {
          if (for_branch) mz_rs_sync();
          ref2 = NULL;
          CHECK_LIMIT();
        }

        if (unsafe_fl || (!unsafe_fx && has_flonum_fast && can_fast_double(arith, cmp, 1))) {
          /* Maybe they're both doubles... */
          if (unsafe_fl) mz_rs_sync();
          generate_double_arith(jitter, rator, arith, cmp, reversed, 1, 0, &refd, &refdt, 
                                for_branch, branch_short, unsafe_fl, 0, unbox);
          CHECK_LIMIT();
        }

        if (!unsafe_fx && !unsafe_fl) {
          if (!has_fixnum_fast) {
            __START_TINY_JUMPS_IF_COMPACT__(1);
            mz_patch_branch(ref2);
            __END_TINY_JUMPS_IF_COMPACT__(1);
          }

          /* Slow path */
          refslow = generate_arith_slow_path(jitter, rator, &ref, &ref4, for_branch, orig_args, reversed, arith, 0, 0);
      
          if (has_fixnum_fast) {
            /* Fixnum branch: */
            __START_TINY_JUMPS_IF_COMPACT__(1);
            mz_patch_branch(ref2);
            __END_TINY_JUMPS_IF_COMPACT__(1);
          }
          CHECK_LIMIT();
        } else {
          refslow = overflow_refslow;
          ref = NULL;
          ref4 = NULL;
        }
      } else {
        /* Only one argument: */
        if (!unsafe_fx && !unsafe_fl) {
          mz_rs_sync();
          __START_TINY_JUMPS_IF_COMPACT__(1);
          ref2 = jit_bmsi_ul(jit_forward(), JIT_R0, 0x1);
          __END_TINY_JUMPS_IF_COMPACT__(1);
        } else {
          if (for_branch) mz_rs_sync();
          ref2 = NULL;
        }

        if (unsafe_fl
            || ((orig_args != 2) /* <- heuristic: we could generate code when an exact argument is
                                    given, but the extra FP code is probably not worthwhile. */
                && !unsafe_fx
                && has_flonum_fast
                && can_fast_double(arith, cmp, 0)
                /* watch out: divide by 0 is special: */
                && ((arith != ARITH_DIV) || v || reversed))) {
          /* Maybe it's a double... */
          generate_double_arith(jitter, rator, arith, cmp, reversed, 0, v, &refd, &refdt, 
                                for_branch, branch_short, unsafe_fl, 0, unbox);
          CHECK_LIMIT();
        }

        if (!unsafe_fx && !unsafe_fl) {
          if (!has_fixnum_fast) {
            __START_TINY_JUMPS_IF_COMPACT__(1);
            mz_patch_branch(ref2);
            __END_TINY_JUMPS_IF_COMPACT__(1);
          }

          /* Slow path */
          refslow = generate_arith_slow_path(jitter, rator, &ref, &ref4, for_branch, orig_args, reversed, arith, 1, v);

          if (has_fixnum_fast) {
            __START_TINY_JUMPS_IF_COMPACT__(1);
            mz_patch_branch(ref2);
            __END_TINY_JUMPS_IF_COMPACT__(1);
          }
        } else {
          refslow = overflow_refslow;
          ref = NULL;
          ref4 = NULL;
        }
      }

      CHECK_LIMIT();

      mz_runstack_unskipped(jitter, skipped);
    }

    __START_SHORT_JUMPS__(branch_short);

    if (!unsafe_fl) {
      if (arith) {
        if (((arith == ARITH_DIV) || (arith == ARITH_QUOT) || (arith == ARITH_REM) || (arith == ARITH_MOD)) && !rand2) {
          (void)jit_movi_p(JIT_R1, scheme_make_integer(v));
          rand2 = scheme_true;
          reversed = !reversed;
        }

        if (rand2) {
          /* First arg is in JIT_R1, second is in JIT_R0 */
          if (arith == ARITH_ADD) {
            jit_andi_ul(JIT_R2, JIT_R1, (~0x1));
            if (unsafe_fx && !overflow_refslow)
              jit_addr_l(JIT_R0, JIT_R2, JIT_R0);
            else {
              (void)jit_boaddr_l(refslow, JIT_R2, JIT_R0);
              jit_movr_p(JIT_R0, JIT_R2);
            }
          } else if (arith == ARITH_SUB) {
            if (reversed) {
              jit_movr_p(JIT_R2, JIT_R0);
              if (unsafe_fx && !overflow_refslow)
                jit_subr_l(JIT_R2, JIT_R2, JIT_R1);
              else
                (void)jit_bosubr_l(refslow, JIT_R2, JIT_R1);
            } else {
              jit_movr_p(JIT_R2, JIT_R1);
              if (unsafe_fx && !overflow_refslow)
                (void)jit_subr_l(JIT_R2, JIT_R2, JIT_R0);
              else
                (void)jit_bosubr_l(refslow, JIT_R2, JIT_R0);
            }
            jit_ori_ul(JIT_R0, JIT_R2, 0x1);
          } else if (arith == ARITH_MUL) {
            jit_andi_ul(JIT_R2, JIT_R1, (~0x1));
            jit_rshi_l(JIT_V1, JIT_R0, 0x1);
            if (unsafe_fx && !overflow_refslow)
              jit_mulr_l(JIT_V1, JIT_V1, JIT_R2);
            else
              (void)jit_bomulr_l(refslow, JIT_V1, JIT_R2);
            jit_ori_ul(JIT_R0, JIT_V1, 0x1);
          } else if ((arith == ARITH_DIV) || (arith == ARITH_QUOT) || (arith == ARITH_REM) || (arith == ARITH_MOD)) {
            if (reversed) {
              jit_rshi_l(JIT_V1, JIT_R0, 0x1);
              jit_rshi_l(JIT_R2, JIT_R1, 0x1);
            } else {
              jit_rshi_l(JIT_R2, JIT_R0, 0x1);
              jit_rshi_l(JIT_V1, JIT_R1, 0x1);
            }
            if (!unsafe_fx || overflow_refslow)
              (void)jit_beqi_l(refslow, JIT_R2, 0);

            if (arith == ARITH_MOD) {
              generate_modulo_setup(jitter, branch_short, JIT_V1, JIT_R2);
              CHECK_LIMIT();
            }
            if ((arith == ARITH_DIV) || (arith == ARITH_QUOT))
              jit_divr_l(JIT_R0, JIT_V1, JIT_R2);
            else
              jit_modr_l(JIT_R0, JIT_V1, JIT_R2);

            if (arith == ARITH_DIV) {
              GC_CAN_IGNORE jit_insn *refx;
              if (reversed)
                jit_mulr_l(JIT_R2, JIT_R0, JIT_R2);
              else
                jit_mulr_l(JIT_V1, JIT_R0, JIT_V1);
              __START_INNER_TINY__(branch_short);
              refx = jit_beqr_l(jit_forward(), JIT_R2, JIT_V1);
              __END_INNER_TINY__(branch_short);
              /* restore R0 argument: */
              if (reversed)
                jit_lshi_l(JIT_R0, JIT_V1, 1);
              else
                jit_lshi_l(JIT_R0, JIT_R2, 1);
              jit_ori_l(JIT_R0, JIT_R0, 0x1);
              (void)jit_jmpi(refslow);
              __START_INNER_TINY__(branch_short);
              mz_patch_branch(refx);
              __END_INNER_TINY__(branch_short);
            } else if (arith == ARITH_MOD) {
              GC_CAN_IGNORE jit_insn *refx, *refy;
              __START_INNER_TINY__(branch_short);
              refy = jit_beqi_l(jit_forward(), JIT_R0, 0);
              refx = jit_bmci_l(jit_forward(), JIT_R1, 0x1);
              jit_subr_l(JIT_R0, JIT_R2, JIT_R0);
              mz_patch_branch(refx);
              refx = jit_bmci_l(jit_forward(), JIT_R1, 0x2);
              jit_negr_l(JIT_R0, JIT_R0);
              mz_patch_branch(refx);
              mz_patch_branch(refy);
              __END_INNER_TINY__(branch_short);
            } else if (arith == ARITH_QUOT) {
              /* watch out for negation of most negative fixnum,
                 which is a positive number too big for a fixnum */
              if (!unsafe_fx || overflow_refslow) {
                GC_CAN_IGNORE jit_insn *refx;
                __START_INNER_TINY__(branch_short);
                refx = jit_bnei_l(jit_forward(), JIT_R0, (void *)(((intptr_t)1 << ((8 * JIT_WORD_SIZE) - 2))));
                __END_INNER_TINY__(branch_short);
                /* first argument must have been most negative fixnum, 
                   second argument must have been -1: */
                if (reversed)
                  (void)jit_movi_p(JIT_R0, (void *)(((intptr_t)1 << ((8 * JIT_WORD_SIZE) - 1)) | 0x1));
                else
                  (void)jit_movi_p(JIT_R0, scheme_make_integer(-1));
                (void)jit_jmpi(refslow);
                __START_INNER_TINY__(branch_short);
                mz_patch_branch(refx);
                __END_INNER_TINY__(branch_short);
              }
            }
            jit_lshi_l(JIT_R0, JIT_R0, 1);
            jit_ori_l(JIT_R0, JIT_R0, 0x1);
          } else if (arith == ARITH_AND) {
            /* and */
            jit_andr_ul(JIT_R0, JIT_R1, JIT_R0);
          } else if (arith == ARITH_IOR) {
            /* ior */
            jit_orr_ul(JIT_R0, JIT_R1, JIT_R0);
          } else if (arith == ARITH_XOR) {
            /* xor */
            jit_andi_ul(JIT_R0, JIT_R0, (~0x1));
            jit_xorr_ul(JIT_R0, JIT_R1, JIT_R0);
          } else if ((arith == ARITH_LSH) || (arith == ARITH_RSH)) {
            /* arithmetic-shift 
               This is a lot of code, but if you're using
               arithmetic-shift, then you probably want it. */
            int v1 = (reversed ? JIT_R0 : JIT_R1);
            int v2 = (reversed ? JIT_R1 : JIT_R0);
            GC_CAN_IGNORE jit_insn *refi, *refc;

            if ((arith != ARITH_RSH) && (!unsafe_fx || overflow_refslow))
              refi = jit_bgei_l(jit_forward(), v2, (intptr_t)scheme_make_integer(0));
            else
              refi = NULL;

            if (!unsafe_fx || overflow_refslow || (arith == ARITH_RSH)) {
              /* Right shift */
              if (!unsafe_fx || overflow_refslow) {
                /* check for a small enough shift */
                if (arith == ARITH_RSH) {
                  (void)jit_blti_l(refslow, v2, scheme_make_integer(0));
                  (void)jit_bgti_l(refslow, v2, scheme_make_integer(MAX_TRY_SHIFT));
                  jit_rshi_l(JIT_V1, v2, 0x1);
                } else {
                  (void)jit_blti_l(refslow, v2, scheme_make_integer(-MAX_TRY_SHIFT));
                  jit_notr_l(JIT_V1, v2);
                  jit_rshi_l(JIT_V1, JIT_V1, 0x1);
                  jit_addi_l(JIT_V1, JIT_V1, 0x1);
                }
              } else {
                jit_rshi_l(JIT_V1, v2, 0x1);
              }
              CHECK_LIMIT();
#ifdef MZ_USE_JIT_I386
              /* Can't shift from _ECX */
              jit_movr_l(JIT_R2, v1);
              jit_rshr_l(JIT_R2, JIT_R2, JIT_V1);
#else
              jit_rshr_l(JIT_R2, v1, JIT_V1);
#endif
              jit_ori_l(JIT_R0, JIT_R2, 0x1);
              if (!unsafe_fx || overflow_refslow)
                refc = jit_jmpi(jit_forward());
              else
                refc = NULL;
              CHECK_LIMIT();
            } else
              refc = NULL;

            /* Left shift */
            if (!unsafe_fx || overflow_refslow || (arith == ARITH_LSH)) {
              if (refi)
                mz_patch_branch(refi);
              if (!unsafe_fx || overflow_refslow)
                (void)jit_bgti_l(refslow, v2, (intptr_t)scheme_make_integer(MAX_TRY_SHIFT));
              jit_rshi_l(JIT_V1, v2, 0x1);
              jit_andi_l(v1, v1, (~0x1));
#ifdef MZ_USE_JIT_I386
              /* Can't shift from _ECX */
              jit_movr_l(JIT_R2, v1);
              jit_lshr_l(JIT_R2, JIT_R2, JIT_V1);
#else
              jit_lshr_l(JIT_R2, v1, JIT_V1);
#endif
              CHECK_LIMIT();
              /* If shifting back right produces a different result, that's overflow... */
              jit_rshr_l(JIT_V1, JIT_R2, JIT_V1);
              /* !! In case we go refslow, it needs to add back tag to v1 !! */
              if (!unsafe_fx || overflow_refslow)
                (void)jit_bner_p(refslow, JIT_V1, v1);
              /* No overflow. */
              jit_ori_l(JIT_R0, JIT_R2, 0x1);
            }

            if (refc)
              mz_patch_ucbranch(refc);
          } else if (arith == ARITH_MIN) {
            /* min */
            GC_CAN_IGNORE jit_insn *refc;
            __START_INNER_TINY__(branch_short);
            refc = jit_bltr_l(jit_forward(), JIT_R0, JIT_R1);
            jit_movr_l(JIT_R0, JIT_R1);
            mz_patch_branch(refc);
            __END_INNER_TINY__(branch_short);
          } else if (arith == ARITH_MAX) {
            /* max */
            GC_CAN_IGNORE jit_insn *refc;
            __START_INNER_TINY__(branch_short);
            refc = jit_bgtr_l(jit_forward(), JIT_R0, JIT_R1);
            jit_movr_l(JIT_R0, JIT_R1);
            mz_patch_branch(refc);
            __END_INNER_TINY__(branch_short);
          }
        } else {
          /* Non-constant arg is in JIT_R0 */
          if (arith == ARITH_ADD) {
            if (unsafe_fx && !overflow_refslow)
              jit_addi_l(JIT_R0, JIT_R0, v << 1);
            else {
              jit_movr_p(JIT_R2, JIT_R0);
              (void)jit_boaddi_l(refslow, JIT_R2, v << 1);
              jit_movr_p(JIT_R0, JIT_R2);
            }
          } else if (arith == ARITH_SUB) {
            if (reversed) {
              (void)jit_movi_p(JIT_R2, scheme_make_integer(v));
              if (unsafe_fx && !overflow_refslow)
                jit_subr_l(JIT_R2, JIT_R2, JIT_R0);
              else
                (void)jit_bosubr_l(refslow, JIT_R2, JIT_R0);
              jit_addi_ul(JIT_R0, JIT_R2, 0x1);
            } else {
              if (unsafe_fx && !overflow_refslow)
                jit_subi_l(JIT_R0, JIT_R0, v << 1);
              else {
                jit_movr_p(JIT_R2, JIT_R0);
                (void)jit_bosubi_l(refslow, JIT_R2, v << 1);
                jit_movr_p(JIT_R0, JIT_R2);
              }
            }
          } else if (arith == ARITH_MUL) {
            if (v == 1) {
              /* R0 already is the answer */
            } else if (v == 0) {
              (void)jit_movi_p(JIT_R0, scheme_make_integer(0));
            } else {
              (void)jit_movi_l(JIT_R2, ((intptr_t)scheme_make_integer(v) & (~0x1)));
              jit_rshi_l(JIT_V1, JIT_R0, 0x1);
              if (unsafe_fx && !overflow_refslow)
                jit_mulr_l(JIT_V1, JIT_V1, JIT_R2);
              else {
                (void)jit_movi_p(JIT_R1, scheme_make_integer(v)); /* for slow path */
                (void)jit_bomulr_l(refslow, JIT_V1, JIT_R2);
              }
              jit_ori_ul(JIT_R0, JIT_V1, 0x1);
            }
          } else {
            if (arith == ARITH_AND) {
              /* and */
              intptr_t l = (intptr_t)scheme_make_integer(v);
              jit_andi_ul(JIT_R0, JIT_R0, l);
            } else if (arith == ARITH_IOR) {
              /* ior */
              intptr_t l = (intptr_t)scheme_make_integer(v);
              jit_ori_ul(JIT_R0, JIT_R0, l);
            } else if (arith == ARITH_XOR) {
              /* xor */
              jit_xori_ul(JIT_R0, JIT_R0, v << 1);
            } else if ((arith == ARITH_LSH) || (arith == ARITH_RSH)) {
              /* arithmetic-shift */
              /* We only get here when v is between -MAX_TRY_SHIFT and MAX_TRY_SHIFT, inclusive */
              if ((v <= 0) || (arith == ARITH_RSH)) {
                int amt = v;
                if (arith != ARITH_RSH) 
                  amt = -amt;
                jit_rshi_l(JIT_R0, JIT_R0, amt);
                jit_ori_l(JIT_R0, JIT_R0, 0x1);
              } else {
                jit_andi_l(JIT_R0, JIT_R0, (~0x1));
                jit_lshi_l(JIT_R2, JIT_R0, v);
                if (!unsafe_fx && !overflow_refslow) {
                  /* If shifting back right produces a different result, that's overflow... */
                  jit_rshi_l(JIT_V1, JIT_R2, v);
                  /* !! In case we go refslow, it nseed to add back tag to JIT_R0 !! */
                  (void)jit_bner_p(refslow, JIT_V1, JIT_R0);
                }
                /* No overflow. */
                jit_ori_l(JIT_R0, JIT_R2, 0x1);
              }
            } else if (arith == ARITH_NOT) {
              jit_notr_ul(JIT_R0, JIT_R0);
              jit_ori_ul(JIT_R0, JIT_R0, 0x1);
            } else if (arith == ARITH_MIN) {
              /* min */
              GC_CAN_IGNORE jit_insn *refc;
              __START_INNER_TINY__(branch_short);
              refc = jit_blti_l(jit_forward(), JIT_R0, (intptr_t)scheme_make_integer(v));
              jit_movi_l(JIT_R0, (intptr_t)scheme_make_integer(v));
              mz_patch_branch(refc);
              __END_INNER_TINY__(branch_short);
            } else if (arith == ARITH_MAX) {
              /* max */
              GC_CAN_IGNORE jit_insn *refc;
              __START_INNER_TINY__(branch_short);
              refc = jit_bgti_l(jit_forward(), JIT_R0, (intptr_t)scheme_make_integer(v));
              jit_movi_l(JIT_R0, (intptr_t)scheme_make_integer(v));
              mz_patch_branch(refc);
              __END_INNER_TINY__(branch_short);
            } else if (arith == ARITH_ABS) {
              /* abs */
              GC_CAN_IGNORE jit_insn *refc;
              __START_INNER_TINY__(branch_short);
              refc = jit_bgei_l(jit_forward(), JIT_R0, (intptr_t)scheme_make_integer(0));
              __END_INNER_TINY__(branch_short);
              /* watch out for most negative fixnum! */
              if (!unsafe_fx || overflow_refslow)
                (void)jit_beqi_p(refslow, JIT_R0, (void *)(((intptr_t)1 << ((8 * JIT_WORD_SIZE) - 1)) | 0x1));
              (void)jit_movi_p(JIT_R1, scheme_make_integer(0));
              jit_subr_l(JIT_R0, JIT_R1, JIT_R0);
              jit_ori_l(JIT_R0, JIT_R0, 0x1);
              __START_INNER_TINY__(branch_short);
              mz_patch_branch(refc);
              __END_INNER_TINY__(branch_short);
              CHECK_LIMIT();
            } else if (arith == ARITH_EX_INEX) {
              /* exact->inexact */
              int fpr0;
              fpr0 = JIT_FPR_0(jitter->unbox_depth);
              jit_rshi_l(JIT_R0, JIT_R0, 1);
              jit_extr_l_d_fppush(fpr0, JIT_R0);
              CHECK_LIMIT();
              if (!unbox) {
                mz_rs_sync(); /* needed for unsafe op before allocation */
                __END_SHORT_JUMPS__(branch_short);
                scheme_generate_alloc_double(jitter, 0);
                __START_SHORT_JUMPS__(branch_short);
              } else {
                jitter->unbox_depth++;
              }
              CHECK_LIMIT();
            } else if (arith == ARITH_INEX_EX) {
              /* inexact->exact */
              /* no work to do, since fixnum is already exact */
            }
          }
        }
        if (refdt)
          mz_patch_ucbranch(refdt);
        if (!unsafe_fx && !unsafe_fl)
          jit_patch_movi(ref, (_jit.x.pc));
        ref3 = NULL;
      } else {
        /* If second is constant, first arg is in JIT_R0. */
        /* Otherwise, first arg is in JIT_R1, second is in JIT_R0 */
        /* Jump to ref3 to produce false */
        int rs_valid, rs_can_keep = 0;

        if (for_branch) {
          scheme_prepare_branch_jump(jitter, for_branch);
          CHECK_LIMIT();
        }

        rs_valid = mz_CURRENT_REG_STATUS_VALID();

        switch (cmp) {
        case CMP_ODDP:
          ref3 = jit_bmci_l(jit_forward(), JIT_R0, 0x2);
          rs_can_keep = 1;
          break;
        case -CMP_BIT:
          if (rand2) {
            if (!unsafe_fx || overflow_refslow) {
              (void)jit_blti_l(refslow, JIT_R1, 0);
              (void)jit_bgti_l(refslow, JIT_R1, (intptr_t)scheme_make_integer(MAX_TRY_SHIFT));
            }
            jit_rshi_l(JIT_R1, JIT_R1, 1);
            jit_addi_l(JIT_V1, JIT_R1, 1);
            jit_movi_l(JIT_R2, 1);
            jit_lshr_l(JIT_R2, JIT_R2, JIT_V1);
            ref3 = jit_bmcr_l(jit_forward(), JIT_R0, JIT_R2);
          } else {
            /* shouldn't get here */
            scheme_signal_error("internal error: bitwise-bit-test? constant in wrong position");
            ref3 = NULL;
          }
          break;
        case CMP_LT:
          if (rand2) {
            ref3 = jit_bger_l(jit_forward(), JIT_R1, JIT_R0);
          } else {
            ref3 = jit_bgei_l(jit_forward(), JIT_R0, (intptr_t)scheme_make_integer(v));
          }
          rs_can_keep = 1;
          break;
        case CMP_LEQ:
          if (rand2) {
            ref3 = jit_bgtr_l(jit_forward(), JIT_R1, JIT_R0);
          } else {
            ref3 = jit_bgti_l(jit_forward(), JIT_R0, (intptr_t)scheme_make_integer(v));
          }
          rs_can_keep = 1;
          break;
        case CMP_EQUAL:
          if (rand2) {
            ref3 = jit_bner_l(jit_forward(), JIT_R1, JIT_R0);
          } else {
            ref3 = jit_bnei_l(jit_forward(), JIT_R0, (intptr_t)scheme_make_integer(v));
          }
          rs_can_keep = 1;
          break;
        case CMP_GEQ:
          if (rand2) {
            ref3 = jit_bltr_l(jit_forward(), JIT_R1, JIT_R0);
          } else {
            ref3 = jit_blti_l(jit_forward(), JIT_R0, (intptr_t)scheme_make_integer(v));
          }
          rs_can_keep = 1;
          break;
        case CMP_GT:
          if (rand2) {
            ref3 = jit_bler_l(jit_forward(), JIT_R1, JIT_R0);
          } else {
            ref3 = jit_blei_l(jit_forward(), JIT_R0, (intptr_t)scheme_make_integer(v));
          }
          rs_can_keep = 1;
          break;
        default:
        case CMP_BIT:
          if (rand2) {
            if (!unsafe_fx || overflow_refslow) {
              (void)jit_blti_l(refslow, JIT_R0, 0);
              (void)jit_bgti_l(refslow, JIT_R0, (intptr_t)scheme_make_integer(MAX_TRY_SHIFT));
            }
            jit_rshi_l(JIT_R0, JIT_R0, 1);
            jit_addi_l(JIT_R0, JIT_R0, 1);
            jit_movi_l(JIT_V1, 1);
            jit_lshr_l(JIT_R0, JIT_V1, JIT_R0);
            ref3 = jit_bmcr_l(jit_forward(), JIT_R1, JIT_R0);
          } else {
            ref3 = jit_bmci_l(jit_forward(), JIT_R0, 1 << (v+1));
            rs_can_keep = 1;
          }
          break;
        case CMP_EVENP:
          ref3 = jit_bmsi_l(jit_forward(), JIT_R0, 0x2);
          rs_can_keep = 1;
          break;
        }

        mz_SET_REG_STATUS_VALID(rs_valid && rs_can_keep);
      }
    } else {
      ref3 = NULL;
    }

    jitter->unbox = unbox;
  }

  if (!arith) {
    if (for_branch) {
      if (refdt) {
        scheme_add_or_patch_branch_true_uc(jitter, for_branch, refdt);
        CHECK_LIMIT();
      }
      if (ref4) {
        scheme_add_or_patch_branch_true_movi(jitter, for_branch, ref4);
        CHECK_LIMIT();
      }
      scheme_add_branch_false(for_branch, ref3);
      scheme_add_branch_false(for_branch, refd);
      scheme_add_branch_false_movi(for_branch, ref);
      scheme_branch_for_true(jitter, for_branch);
      CHECK_LIMIT();
    } else {
      if (refdt)
        mz_patch_ucbranch(refdt);

      (void)jit_movi_p(JIT_R0, scheme_true);
      __START_INNER_TINY__(branch_short);
      ref2 = jit_jmpi(jit_forward());
      __END_INNER_TINY__(branch_short);
      if (ref3)
        mz_patch_branch(ref3);
      if (refd)
        mz_patch_branch(refd);
      (void)jit_movi_p(JIT_R0, scheme_false);
      __START_INNER_TINY__(branch_short);
      mz_patch_ucbranch(ref2);
      __END_INNER_TINY__(branch_short);
      if (!unsafe_fx && !unsafe_fl)
        jit_patch_movi(ref, (_jit.x.pc));
    }
  }

  __END_SHORT_JUMPS__(branch_short);

  return 1;  
}

#define MAX_NON_SIMPLE_ARGS 5

static int extract_nary_arg(int reg, int n, mz_jit_state *jitter, Scheme_App_Rec *app, 
                            Scheme_Object **alt_args, int old_short_jumps)
{
  if (!alt_args) {
    jit_ldxi_p(reg, JIT_RUNSTACK, WORDS_TO_BYTES(n));
    if (jitter->unbox)
      scheme_generate_unboxing(jitter, JIT_R0);
  } else if (scheme_is_constant_and_avoids_r1(app->args[n+1])) {
    __END_SHORT_JUMPS__(old_short_jumps);
    scheme_generate(app->args[n+1], jitter, 0, 0, 0, reg, NULL);
    CHECK_LIMIT();
    __START_SHORT_JUMPS__(old_short_jumps);
  } else {
    int i, j = 0;
    for (i = 0; i < n; i++) {
      if (!scheme_is_constant_and_avoids_r1(app->args[i+1]))
        j++;
    }
    jit_ldxi_p(reg, JIT_RUNSTACK, WORDS_TO_BYTES(j));
    if (jitter->unbox)
      scheme_generate_unboxing(jitter, JIT_R0);
  }
  CHECK_LIMIT();
  return 1;
}

static void init_nary_branches(Branch_Info *for_nary_branch, Branch_Info_Addr *addrs)
{
  memset(for_nary_branch, 0, sizeof(Branch_Info));
  for_nary_branch->addrs_size = 3;
  for_nary_branch->addrs = addrs;
}

static void patch_nary_branches(mz_jit_state *jitter, Branch_Info *for_nary_branch, GC_CAN_IGNORE jit_insn *reffalse)
{
  int i;

  for (i = for_nary_branch->addrs_count; i--; ) {
    if (for_nary_branch->addrs[i].mode == BRANCH_ADDR_FALSE) {
      if (for_nary_branch->addrs[i].kind == BRANCH_ADDR_BRANCH)
        mz_patch_branch_at(for_nary_branch->addrs[i].addr, reffalse);
      else if (for_nary_branch->addrs[i].kind == BRANCH_ADDR_MOVI)
        jit_patch_movi(for_nary_branch->addrs[i].addr, reffalse);
      else
        break;
    } else
      break;
  }

  if (i != -1)
    scheme_signal_error("internal error: unexpected branch addresses");
}

int scheme_generate_nary_arith(mz_jit_state *jitter, Scheme_App_Rec *app,
                               int arith, int cmp, Branch_Info *for_branch, int branch_short)
{
  int c, i, non_simple_c = 0, stack_c, use_fx = 1, trigger_arg = 0;
  Scheme_Object *non_simples[1+MAX_NON_SIMPLE_ARGS], **alt_args, *v;
  Branch_Info for_nary_branch;
  Branch_Info_Addr nary_addrs[3];
  GC_CAN_IGNORE jit_insn *refslow, *reffx, *refdone;
  GC_CAN_IGNORE jit_insn *reffalse = NULL, *refdone3 = NULL;
#ifdef INLINE_FP_OPS
  int args_unboxed;
  GC_CAN_IGNORE jit_insn *reffl, *refdone2;
  int use_fl = 1;
# define mzSET_USE_FL(x) x
#else
# define mzSET_USE_FL(x) /* empty */
#endif

  if ((arith == ARITH_AND)
      || (arith == ARITH_IOR)
      || (arith == ARITH_XOR)) {
    /* bitwise operators are fixnum, only */
    mzSET_USE_FL(use_fl = 0);
  }

  c = app->num_args;
  for (i = 0; i < c; i++) {
    v = app->args[i+1];
    if (!scheme_is_constant_and_avoids_r1(v)) {
      if (non_simple_c < MAX_NON_SIMPLE_ARGS)
        non_simples[1+non_simple_c] = v;
      non_simple_c++;
    }
    if (SCHEME_INTP(v)) {
      mzSET_USE_FL(use_fl = 0);
      if (trigger_arg == i)
        trigger_arg++;
    } else if (SCHEME_FLOATP(v)) {
      use_fx = 0;
      if (trigger_arg == i)
        trigger_arg++;
    } else if (SCHEME_TYPE(v) >= _scheme_compiled_values_types_) {
      use_fx = 0;
      mzSET_USE_FL(use_fl = 0);
    }
  }

  if ((non_simple_c <= MAX_NON_SIMPLE_ARGS) && (non_simple_c < c)) {
    stack_c = non_simple_c;
    alt_args = non_simples;
    non_simples[0] = app->args[0];
    mz_runstack_skipped(jitter, c - stack_c);
  } else {
    stack_c = c;
    alt_args = NULL;
  }

  if (stack_c)
    scheme_generate_app(app, alt_args, stack_c, jitter, 0, 0, 0, 2);
  CHECK_LIMIT();
  mz_rs_sync();

  __START_SHORT_JUMPS__(c < 100);

  if (trigger_arg >= c) {
    /* we don't expect this to happen, since constant-folding normally
       would have collapsed it --- but a division by zero, for example,
       might block constant folding */
    trigger_arg = 0;
  }

  extract_nary_arg(JIT_R0, trigger_arg, jitter, app, alt_args, c < 100);
  CHECK_LIMIT();
  /* trigger argument a fixnum? */
  reffx = jit_bmsi_ul(jit_forward(), JIT_R0, 0x1);

#ifdef INLINE_FP_OPS
  if (use_fl) {
    /* First argument a flonum? */
    jit_ldxi_s(JIT_R0, JIT_R0, &((Scheme_Object *)0x0)->type);
    reffl = jit_beqi_i(jit_forward(), JIT_R0, scheme_double_type);
    CHECK_LIMIT();
  } else {
    reffl = NULL;
  }
#endif
  
  if (!use_fx) {
    mz_patch_branch(reffx);
  }

  refslow = _jit.x.pc;
  /* slow path */
  if (alt_args) {
    /* get all args on runstack */
    int delta = stack_c - c;
    for (i = 0; i < c; i++) {
      if (delta) {
        extract_nary_arg(JIT_R0, i, jitter, app, alt_args, c < 100);
        CHECK_LIMIT();
        jit_stxi_p(WORDS_TO_BYTES(i+delta), JIT_RUNSTACK, JIT_R0);
      } else
        break;
    }
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(c - stack_c));
  }
  (void)jit_movi_p(JIT_V1, ((Scheme_Primitive_Proc *)app->args[0])->prim_val);
  (void)jit_movi_i(JIT_R1, c);
  (void)jit_calli(sjc.call_original_nary_arith_code);
  if (alt_args) {
    jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(c - stack_c));
  }
  refdone = jit_jmpi(jit_forward());
  if (!arith) {
    reffalse = _jit.x.pc;
    (void)jit_movi_p(JIT_R0, scheme_false);
    refdone3 = jit_jmpi(jit_forward());
  } else {
    reffalse = NULL;
  }

#ifdef INLINE_FP_OPS
  if (use_fl) {
    /* Flonum branch: */
    mz_patch_branch(reffl);
    for (i = 0; i < c; i++) {
      if (i != trigger_arg) {
        v = app->args[i+1];
        if (!SCHEME_FLOATP(v)) {
          extract_nary_arg(JIT_R0, i, jitter, app, alt_args, c < 100);
          (void)jit_bmsi_ul(refslow, JIT_R0, 0x1);
          jit_ldxi_s(JIT_R0, JIT_R0, &((Scheme_Object *)0x0)->type);
          (void)jit_bnei_i(refslow, JIT_R0, scheme_double_type);
          CHECK_LIMIT();
        }
      }
    }
    /* All flonums, so inline fast flonum combination */
    args_unboxed = ((arith != ARITH_MIN) && (arith != ARITH_MAX)); /* no unboxing for min & max */
    if (args_unboxed)
      jitter->unbox++;
    extract_nary_arg(JIT_R0, 0, jitter, app, alt_args, c < 100);
    CHECK_LIMIT();
    for (i = 1; i < c; i++) {
      if (!arith && (i > 1))
        extract_nary_arg(JIT_R0, i - 1, jitter, app, alt_args, c < 100);
      extract_nary_arg((args_unboxed ? JIT_R0 : JIT_R1), i, jitter, app, alt_args, c < 100);
      if ((i == c - 1) && args_unboxed) --jitter->unbox; /* box last result */
      if (!arith) init_nary_branches(&for_nary_branch, nary_addrs);
      __END_SHORT_JUMPS__(c < 100);
      scheme_generate_arith(jitter, NULL, NULL, scheme_void, 2, arith, cmp, 0,
                            !arith ? &for_nary_branch : NULL, c < 100, 0, 1, NULL);
      __START_SHORT_JUMPS__(c < 100);
      if (!arith) patch_nary_branches(jitter, &for_nary_branch, reffalse);
      CHECK_LIMIT();
    }
    if (use_fx) {
      refdone2 = jit_jmpi(jit_forward());
    } else {
      refdone2 = NULL;
    }
  } else {
    refdone2 = NULL;
  }
#endif

  if (use_fx) {
    /* Fixnum branch */
    mz_patch_branch(reffx);
    for (i = 0; i < c; i++) {
      if (i != trigger_arg) {
        v = app->args[i+1];
        if (!SCHEME_INTP(v)) {
          extract_nary_arg(JIT_R0, i, jitter, app, alt_args, c < 100);
          CHECK_LIMIT();
          (void)jit_bmci_ul(refslow, JIT_R0, 0x1);
          CHECK_LIMIT();
        }
      }
    }
    /* All fixnums, so inline fast fixnum combination;
       on overflow, bail out to refslow. */
    extract_nary_arg(JIT_R0, 0, jitter, app, alt_args, c < 100);
    for (i = 1; i < c; i++) {
      if (!arith && (i > 1))
        extract_nary_arg(JIT_R0, i - 1, jitter, app, alt_args, c < 100);
      extract_nary_arg(JIT_R1, i, jitter, app, alt_args, c < 100);
      CHECK_LIMIT();
      if (!arith) init_nary_branches(&for_nary_branch, nary_addrs);
      __END_SHORT_JUMPS__(c < 100);
      scheme_generate_arith(jitter, NULL, NULL, scheme_void, 2, arith, cmp, 0,
                            !arith ? &for_nary_branch : NULL, c < 100, 1, 0, refslow);
      __START_SHORT_JUMPS__(c < 100);
      if (!arith) patch_nary_branches(jitter, &for_nary_branch, reffalse);
      CHECK_LIMIT();
    }
  }

#ifdef INLINE_FP_OPS
  if (use_fl && use_fx) {
    mz_patch_ucbranch(refdone2);
  }
#endif
  if (!arith) {
    (void)jit_movi_p(JIT_R0, scheme_true);
  }
  mz_patch_ucbranch(refdone);
  if (refdone3)
    mz_patch_ucbranch(refdone3);

  __END_SHORT_JUMPS__(c < 100);

  if (stack_c) {
    mz_rs_inc(stack_c); /* no sync */
    mz_runstack_popped(jitter, stack_c);
  }
  if (c > stack_c)
    mz_runstack_unskipped(jitter, c - stack_c);

  if (!arith && for_branch) {
    GC_CAN_IGNORE jit_insn *refx;
    scheme_prepare_branch_jump(jitter, for_branch);
    CHECK_LIMIT();
    __START_SHORT_JUMPS__(branch_short);
    refx = jit_beqi_p(jit_forward(), JIT_R0, scheme_false);
    scheme_add_branch_false(for_branch, refx);
    scheme_branch_for_true(jitter, for_branch);
    __END_SHORT_JUMPS__(branch_short);
    CHECK_LIMIT();
  }

  return 1;
}

#endif
