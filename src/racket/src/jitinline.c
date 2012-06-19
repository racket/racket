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

static Scheme_Object *extract_one_cc_mark_to_tag(Scheme_Object *, Scheme_Object *, Scheme_Object *);

#define JITINLINE_TS_PROCS
#ifndef CAN_INLINE_ALLOC
# define JIT_BOX_TS_PROCS
#endif
#include "jit_ts.c"

#ifdef MZ_USE_FUTURES
static Scheme_Object *ts_scheme_make_fsemaphore(int argc, Scheme_Object **argv) 
  XFORM_SKIP_PROC
{
  if (scheme_use_rtcall) { 
    return scheme_rtcall_make_fsemaphore(argv[0]);
  } 
  
  return scheme_make_fsemaphore_inl(argv[0]);
}
#else
# define ts_scheme_make_fsemaphore scheme_make_fsemaphore
#endif

static Scheme_Object *extract_one_cc_mark_to_tag(Scheme_Object *mark_set, 
                                                 Scheme_Object *key,
                                                 Scheme_Object *prompt_tag)
  XFORM_SKIP_PROC
{
  /* wrapper on scheme_extract_one_cc_mark_to_tag() to convert NULL to false */
  Scheme_Object *r;
  r = scheme_extract_one_cc_mark_to_tag(mark_set, key, prompt_tag);
  if (!r) return scheme_false;
  return r;
}

static Scheme_Object *cont_mark_set_first_try_fast(Scheme_Object *cms, Scheme_Object *key)
  XFORM_SKIP_PROC
{
  Scheme_Object *nullableCms;
  Scheme_Object *prompt_tag; 
 
  prompt_tag = SCHEME_PTR_VAL(scheme_default_prompt_tag);
  if (key == scheme_parameterization_key || key == scheme_break_enabled_key) 
    prompt_tag = NULL;

  nullableCms = SCHEME_FALSEP(cms) ? NULL : cms;
  
  /*Fast path here */
  
  if (!nullableCms) { 
    intptr_t findpos, bottom, startpos, minbottom; 
    intptr_t pos; 
    Scheme_Object *val = NULL;
    Scheme_Cont_Mark *seg; 
    Scheme_Thread *p = scheme_current_thread;
  
    startpos = (intptr_t)MZ_CONT_MARK_STACK;
    if (!p->cont_mark_stack_segments) 
      startpos = 0;

    bottom = p->cont_mark_stack_bottom; 
    findpos = startpos;
    minbottom = findpos - 16;
    if (bottom < minbottom) 
      bottom = minbottom;

    while (findpos-- > bottom) { 
      seg = p->cont_mark_stack_segments[findpos >> SCHEME_LOG_MARK_SEGMENT_SIZE];
      pos = findpos & SCHEME_MARK_SEGMENT_MASK;

      if (SAME_OBJ(seg[pos].key, key)) { 
        val = seg[pos].val;
        break;
      } else if (SAME_OBJ(seg[pos].key, prompt_tag)) { 
        break;
      }
    }

    if (val) { 
      return val;
    }
  }

  
  /* Otherwise, slow path. This must be a "tail call", because the
     calling context may be captured as a lightweight continuation. */
  return ts_extract_one_cc_mark_to_tag(nullableCms, key, prompt_tag);
}

static int generate_two_args(Scheme_Object *rand1, Scheme_Object *rand2, mz_jit_state *jitter, 
                             int order_matters, int skipped);

static int check_val_struct_prim(Scheme_Object *p, int arity)
{
  if (p && SCHEME_PRIMP(p)) {
    if (arity == 1) {
      int t = (((Scheme_Primitive_Proc *)p)->pp.flags & SCHEME_PRIM_OTHER_TYPE_MASK);
      if (t == SCHEME_PRIM_STRUCT_TYPE_PRED)
        return 1;
      if (t == SCHEME_PRIM_STRUCT_TYPE_INDEXED_GETTER)
        return 2;
      else if (t == SCHEME_PRIM_TYPE_STRUCT_PROP_GETTER)
        return 4;
      else if (t == SCHEME_PRIM_STRUCT_TYPE_STRUCT_PROP_PRED)
        return 6;
    } else if (arity == 2) {
      int t = (((Scheme_Primitive_Proc *)p)->pp.flags & SCHEME_PRIM_OTHER_TYPE_MASK);
      if (t == SCHEME_PRIM_STRUCT_TYPE_INDEXED_SETTER)
        return 3;
      else if (t == SCHEME_PRIM_TYPE_STRUCT_PROP_GETTER)
        return 5;
    }
  }
  return 0;
}

static int inlineable_struct_prim(Scheme_Object *o, mz_jit_state *jitter, int extra_push, int arity)
{
  if (jitter->nc) {
    if (SAME_TYPE(SCHEME_TYPE(o), scheme_toplevel_type)) {
      Scheme_Object *p;
      p = scheme_extract_global(o, jitter->nc);
      p = ((Scheme_Bucket *)p)->val;
      return check_val_struct_prim(p, arity);
    } else if (SAME_TYPE(SCHEME_TYPE(o), scheme_local_type)) {
      Scheme_Object *p;
      p = scheme_extract_closure_local(o, jitter, extra_push);
      return check_val_struct_prim(p, arity);
    }
  }
  return 0;
}

int scheme_inlined_unary_prim(Scheme_Object *o, Scheme_Object *_app, mz_jit_state *jitter)
{
  if (SCHEME_PRIMP(o)
      && (SCHEME_PRIM_PROC_FLAGS(o) & SCHEME_PRIM_IS_UNARY_INLINED))
    return 1;

  if (inlineable_struct_prim(o, jitter, 1, 1))
    return 1;

  return 0;
}

int scheme_inlined_binary_prim(Scheme_Object *o, Scheme_Object *_app, mz_jit_state *jitter)
{
  return ((SCHEME_PRIMP(o)
           && (SCHEME_PRIM_PROC_FLAGS(o) & SCHEME_PRIM_IS_BINARY_INLINED))
          || inlineable_struct_prim(o, jitter, 2, 2));
}

int scheme_inlined_nary_prim(Scheme_Object *o, Scheme_Object *_app)
{
  return (SCHEME_PRIMP(o)
          && (SCHEME_PRIM_PROC_FLAGS(o) & SCHEME_PRIM_IS_NARY_INLINED)
          && (((Scheme_App_Rec *)_app)->num_args >= ((Scheme_Primitive_Proc *)o)->mina)
          && (((Scheme_App_Rec *)_app)->num_args <= ((Scheme_Primitive_Proc *)o)->mu.maxa));
}

static int generate_inlined_constant_test(mz_jit_state *jitter, Scheme_App2_Rec *app,
					  Scheme_Object *cnst, Scheme_Object *cnst2, 
					  Branch_Info *for_branch, int branch_short, int need_sync)
/* de-sync'd ok */
{
  GC_CAN_IGNORE jit_insn *ref, *ref2;

  LOG_IT(("inlined %s\n", ((Scheme_Primitive_Proc *)app->rator)->name));

  mz_runstack_skipped(jitter, 1);

  scheme_generate_non_tail(app->rand, jitter, 0, 1, 0);
  CHECK_LIMIT();

  mz_runstack_unskipped(jitter, 1);

  if (need_sync) mz_rs_sync();

  __START_SHORT_JUMPS__(branch_short);

  if (for_branch) {
    scheme_prepare_branch_jump(jitter, for_branch);
    CHECK_LIMIT();
  }

  if (cnst2) {
    ref2 = mz_beqi_p(jit_forward(), JIT_R0, cnst);
    ref = mz_bnei_p(jit_forward(), JIT_R0, cnst2);
    mz_patch_branch(ref2);
  } else {
    ref = mz_bnei_p(jit_forward(), JIT_R0, cnst);
  }

  if (for_branch) {
    scheme_add_branch_false(for_branch, ref);
    scheme_branch_for_true(jitter, for_branch);
    CHECK_LIMIT();
  } else {
    (void)jit_movi_p(JIT_R0, scheme_true);
    ref2 = jit_jmpi(jit_forward());
    mz_patch_branch(ref);
    (void)jit_movi_p(JIT_R0, scheme_false);
    mz_patch_ucbranch(ref2);
  }

  __END_SHORT_JUMPS__(branch_short);

  return 1;
}

static int generate_inlined_type_test(mz_jit_state *jitter, Scheme_App2_Rec *app,
				      Scheme_Type lo_ty, Scheme_Type hi_ty, int can_chaperone,
				      Branch_Info *for_branch, int branch_short, int need_sync)
{
  GC_CAN_IGNORE jit_insn *ref, *ref2, *ref3, *ref4, *ref5;
  int int_ok, reg_valid;

  int_ok = ((lo_ty <= scheme_integer_type) && (scheme_integer_type <= hi_ty));

  LOG_IT(("inlined %s\n", ((Scheme_Primitive_Proc *)app->rator)->name));

  mz_runstack_skipped(jitter, 1);

  scheme_generate_non_tail(app->rand, jitter, 0, 1, 0);
  CHECK_LIMIT();

  mz_runstack_unskipped(jitter, 1);

  if (need_sync) mz_rs_sync();

  __START_SHORT_JUMPS__(branch_short);

  reg_valid = 0;
  if (for_branch) {
    reg_valid = mz_CURRENT_REG_STATUS_VALID();
    scheme_prepare_branch_jump(jitter, for_branch);
    CHECK_LIMIT();
  }

  if ((lo_ty == scheme_integer_type) && (scheme_integer_type == hi_ty)) {
    ref3 = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);
    ref4 = NULL;
    ref = NULL;
    ref5 = NULL;
#ifdef jit_bxnei_s
  } else if (!can_chaperone && (lo_ty == hi_ty)) {
    ref = jit_bmsi_ul(jit_forward(), JIT_R0, 0x1);
    ref3 = jit_bxnei_s(jit_forward(), JIT_R0, lo_ty);
    ref4 = NULL;
    ref5 = NULL;
#endif
  } else {
    ref = jit_bmsi_ul(jit_forward(), JIT_R0, 0x1);
    jit_ldxi_s(JIT_R1, JIT_R0, &((Scheme_Object *)0x0)->type);
    if (can_chaperone > 0) {
      __START_INNER_TINY__(branch_short);
      ref3 = jit_bnei_i(jit_forward(), JIT_R1, scheme_chaperone_type);
      jit_ldxi_p(JIT_R1, JIT_R0, (intptr_t)&((Scheme_Chaperone *)0x0)->val);
      jit_ldxi_s(JIT_R1, JIT_R1, &((Scheme_Object *)0x0)->type);
      mz_patch_branch(ref3);
      CHECK_LIMIT();
      __END_INNER_TINY__(branch_short);
    }
    if (lo_ty == hi_ty) {
      ref3 = jit_bnei_p(jit_forward(), JIT_R1, lo_ty);
      ref4 = NULL;
    } else {
      ref3 = jit_blti_p(jit_forward(), JIT_R1, lo_ty);
      ref4 = jit_bgti_p(jit_forward(), JIT_R1, hi_ty);
    }
    if (can_chaperone < 0) {
      /* Make sure it's not a impersonator */
      jit_ldxi_s(JIT_R1, JIT_R0, (intptr_t)&SCHEME_CHAPERONE_FLAGS((Scheme_Chaperone *)0x0));
      ref5 = jit_bmsi_i(jit_forward(), JIT_R1, SCHEME_CHAPERONE_IS_IMPERSONATOR);
    } else
      ref5 = NULL;
    if (int_ok) {
      mz_patch_branch(ref);
    }
  }
  if (for_branch) {
    if (!int_ok) {
      scheme_add_branch_false(for_branch, ref);
    }
    scheme_add_branch_false(for_branch, ref3);
    scheme_add_branch_false(for_branch, ref4);
    scheme_add_branch_false(for_branch, ref5);

    /* In case true is a fall-through, note that the test 
       didn't disturb R0: */
    mz_SET_R0_STATUS_VALID(reg_valid);

    scheme_branch_for_true(jitter, for_branch);
    CHECK_LIMIT();
  } else {
    (void)jit_movi_p(JIT_R0, scheme_true);
    ref2 = jit_jmpi(jit_forward());
    if (!int_ok) {
      mz_patch_branch(ref);
    }
    mz_patch_branch(ref3);
    if (ref4) {
      mz_patch_branch(ref4);
    }
    if (ref5) {
      mz_patch_branch(ref5);
    }
    (void)jit_movi_p(JIT_R0, scheme_false);
    mz_patch_ucbranch(ref2);
  }

  __END_SHORT_JUMPS__(branch_short);

  return 1;
}

static int generate_inlined_struct_op(int kind, mz_jit_state *jitter, 
				      Scheme_Object *rator, Scheme_Object *rand, Scheme_Object *rand2,
				      Branch_Info *for_branch, int branch_short, 
                                      int multi_ok)
/* de-sync'd ok; for branch, sync'd before */
{
  LOG_IT(("inlined struct op\n"));

  if (!rand2) {
    generate_two_args(rator, rand, jitter, 1, 1); /* sync'd below */
    CHECK_LIMIT();
  } else {
    Scheme_Object *args[3];
    args[0] = rator;
    args[1] = rand;
    args[2] = rand2;
    scheme_generate_app(NULL, args, 2, jitter, 0, 0, 1); /* sync'd below */
    CHECK_LIMIT();
    jit_movr_p(JIT_R0, JIT_V1);
    mz_rs_ldr(JIT_R1);
    mz_rs_ldxi(JIT_V1, 1);
    mz_rs_inc(2); /* sync'd below */
    mz_runstack_popped(jitter, 2);
  }
  mz_rs_sync();

  /* R0 is [potential] predicate/getter/setting, R1 is struct. 
     V1 is value for setting. */

  if (for_branch) {
    scheme_prepare_branch_jump(jitter, for_branch);
    CHECK_LIMIT();
    __START_SHORT_JUMPS__(for_branch->branch_short);
    scheme_add_branch_false_movi(for_branch, jit_patchable_movi_p(JIT_V1, jit_forward()));
    __END_SHORT_JUMPS__(for_branch->branch_short);
    (void)jit_calli(sjc.struct_pred_branch_code);
    __START_SHORT_JUMPS__(for_branch->branch_short);
    scheme_branch_for_true(jitter, for_branch);
    __END_SHORT_JUMPS__(for_branch->branch_short);
    CHECK_LIMIT();
  } else if (kind == 1) {
    if (multi_ok) {
      (void)jit_calli(sjc.struct_pred_multi_code);
    } else {
      (void)jit_calli(sjc.struct_pred_code);
    }
  } else if (kind == 2) {
    if (multi_ok) {
      (void)jit_calli(sjc.struct_get_multi_code);
    } else {
      (void)jit_calli(sjc.struct_get_code);
    }
  } else if (kind == 3) {
    if (multi_ok) {
      (void)jit_calli(sjc.struct_set_multi_code);
    } else {
      (void)jit_calli(sjc.struct_set_code);
    }
  } else if (kind == 4) {
    if (multi_ok) {
      (void)jit_calli(sjc.struct_prop_get_multi_code);
    } else {
      (void)jit_calli(sjc.struct_prop_get_code);
    }
  } else if (kind == 5) {
    if (multi_ok) {
      (void)jit_calli(sjc.struct_prop_get_defl_multi_code);
    } else {
      (void)jit_calli(sjc.struct_prop_get_defl_code);
    }
  } else if (kind == 6) {
    if (multi_ok) {
      (void)jit_calli(sjc.struct_prop_pred_multi_code);
    } else {
      (void)jit_calli(sjc.struct_prop_pred_code);
    }
  } else {
    scheme_signal_error("internal error: unknown struct-op mode");
  }

  return 1;
}

static int is_cXr_prim(const char *name) 
{
  int i;
  if (name[0] != 'c') return 0;
  for (i = 1; (name[i] == 'a') || (name[i] == 'd'); i++) { }
  if (name[i] != 'r') return 0;
  return !name[i+1];
}

static int generate_inlined_constant_varref_test(mz_jit_state *jitter, Scheme_Object *obj,
                                                 Branch_Info *for_branch, int branch_short, int need_sync)
{
  GC_CAN_IGNORE jit_insn *ref1, *ref2;
  int pos;

  if (SCHEME_VARREF_FLAGS(obj) & 0x1) {
    jit_movi_p(JIT_R0, scheme_true);
    return 1;
  }

  mz_runstack_skipped(jitter, 1);

  obj = SCHEME_PTR1_VAL(obj);

  /* Load global array: */
  pos = mz_remap(SCHEME_TOPLEVEL_DEPTH(obj));
  mz_rs_ldxi(JIT_R2, pos);
  /* Load bucket: */
  pos = SCHEME_TOPLEVEL_POS(obj);
  jit_ldxi_p(JIT_R1, JIT_R2, &(((Scheme_Prefix *)0x0)->a[pos]));
  CHECK_LIMIT();
  
  mz_runstack_unskipped(jitter, 1);

  if (need_sync) mz_rs_sync();

  __START_SHORT_JUMPS__(branch_short);
  
  if (for_branch) {
    scheme_prepare_branch_jump(jitter, for_branch);
    CHECK_LIMIT();
  }

  jit_ldxi_s(JIT_R1, JIT_R1, &((Scheme_Bucket_With_Flags *)0x0)->flags);
  ref1 = jit_bmci_ul(jit_forward(), JIT_R1, GLOB_IS_IMMUTATED);
  CHECK_LIMIT();

  if (for_branch) {
    scheme_add_branch_false(for_branch, ref1);
    scheme_branch_for_true(jitter, for_branch);
  } else {
    (void)jit_movi_p(JIT_R0, scheme_true);
    ref2 = jit_jmpi(jit_forward());

    mz_patch_branch(ref1);
    (void)jit_movi_p(JIT_R0, scheme_false);
      
    mz_patch_ucbranch(ref2);
  }
  CHECK_LIMIT();

  __END_SHORT_JUMPS__(branch_short);
  
  return 1;
}

static int generate_vector_alloc(mz_jit_state *jitter, Scheme_Object *rator,
                                 Scheme_App_Rec *app, Scheme_App2_Rec *app2, Scheme_App3_Rec *app3);

int scheme_generate_inlined_unary(mz_jit_state *jitter, Scheme_App2_Rec *app, int is_tail, int multi_ok, 
				  Branch_Info *for_branch, int branch_short, int need_sync, int result_ignored)
/* de-sync's, unless branch */
{
  Scheme_Object *rator = app->rator;

  {
    int k;
    k = inlineable_struct_prim(rator, jitter, 1, 1);
    if (k == 1) {
      generate_inlined_struct_op(1, jitter, rator, app->rand, NULL, for_branch, branch_short, multi_ok);
      scheme_direct_call_count++;
      return 1;
    } else if (((k == 2) || (k == 4) || (k == 6)) && !for_branch) {
      generate_inlined_struct_op(k, jitter, rator, app->rand, NULL, for_branch, branch_short, multi_ok);
      scheme_direct_call_count++;
      return 1;
    }
  }

  if (SAME_OBJ(rator, scheme_varref_const_p_proc)
      && SAME_TYPE(SCHEME_TYPE(app->rand), scheme_varref_form_type)) {
    generate_inlined_constant_varref_test(jitter, app->rand, for_branch, branch_short, need_sync);
    return 1;
  }

  if (!SCHEME_PRIMP(rator))
    return 0;

  if (!(SCHEME_PRIM_PROC_FLAGS(rator) & SCHEME_PRIM_IS_UNARY_INLINED))
    return 0;

  scheme_direct_call_count++;

  if (IS_NAMED_PRIM(rator, "not")) {
    generate_inlined_constant_test(jitter, app, scheme_false, NULL, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "null?")) {
    generate_inlined_constant_test(jitter, app, scheme_null, NULL, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "void?")) {
    generate_inlined_constant_test(jitter, app, scheme_void, NULL, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "pair?")) {
    generate_inlined_type_test(jitter, app, scheme_pair_type, scheme_pair_type, 0, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "mpair?")) {
    generate_inlined_type_test(jitter, app, scheme_mutable_pair_type, scheme_mutable_pair_type, 0, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "symbol?")) {
    generate_inlined_type_test(jitter, app, scheme_symbol_type, scheme_symbol_type, 0, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "syntax?")) {
    generate_inlined_type_test(jitter, app, scheme_stx_type, scheme_stx_type, 0, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "char?")) {
    generate_inlined_type_test(jitter, app, scheme_char_type, scheme_char_type, 0, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "boolean?")) {
    generate_inlined_constant_test(jitter, app, scheme_false, scheme_true, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "number?")) {
    generate_inlined_type_test(jitter, app, scheme_integer_type, scheme_complex_type, 0, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "real?")) {
    generate_inlined_type_test(jitter, app, scheme_integer_type, scheme_double_type, 0, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "exact-integer?")) {
    generate_inlined_type_test(jitter, app, scheme_integer_type, scheme_bignum_type, 0, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "fixnum?")) {
    generate_inlined_type_test(jitter, app, scheme_integer_type, scheme_integer_type, 0, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "inexact-real?")) {
    generate_inlined_type_test(jitter, app, SCHEME_FLOAT_TYPE, scheme_double_type, 0, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "flonum?")) {
    generate_inlined_type_test(jitter, app, scheme_double_type, scheme_double_type, 0, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "single-flonum?")) {
    generate_inlined_type_test(jitter, app, SCHEME_FLOAT_TYPE, SCHEME_FLOAT_TYPE, 0, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "procedure?")) {
    generate_inlined_type_test(jitter, app, scheme_prim_type, scheme_proc_chaperone_type, 1, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "chaperone?")) {
    generate_inlined_type_test(jitter, app, scheme_proc_chaperone_type, scheme_chaperone_type, -1, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "impersonator?")) {
    generate_inlined_type_test(jitter, app, scheme_proc_chaperone_type, scheme_chaperone_type, 0, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "vector?")) {
    generate_inlined_type_test(jitter, app, scheme_vector_type, scheme_vector_type, 1, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "box?")) {
    generate_inlined_type_test(jitter, app, scheme_box_type, scheme_box_type, 1, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "string?")) {
    generate_inlined_type_test(jitter, app, scheme_char_string_type, scheme_char_string_type, 0, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "bytes?")) {
    generate_inlined_type_test(jitter, app, scheme_byte_string_type, scheme_byte_string_type, 0, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "path?")) {
    generate_inlined_type_test(jitter, app, SCHEME_PLATFORM_PATH_KIND, SCHEME_PLATFORM_PATH_KIND, 0, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "eof-object?")) {
    generate_inlined_constant_test(jitter, app, scheme_eof, NULL, for_branch, branch_short, need_sync);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "zero?")) {
    scheme_generate_arith(jitter, rator, app->rand, NULL, 1, 0, CMP_EQUAL, 0, for_branch, branch_short, 0, 0, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "negative?")) {
    scheme_generate_arith(jitter, rator, app->rand, NULL, 1, 0, CMP_LT, 0, for_branch, branch_short, 0, 0, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "positive?")) {
    scheme_generate_arith(jitter, rator, app->rand, NULL, 1, 0, CMP_GT, 0, for_branch, branch_short, 0, 0, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "even?")) {
    scheme_generate_arith(jitter, rator, app->rand, NULL, 1, 0, CMP_EVENP, 0, for_branch, branch_short, 0, 0, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "odd?")) {
    scheme_generate_arith(jitter, rator, app->rand, NULL, 1, 0, CMP_ODDP, 0, for_branch, branch_short, 0, 0, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "list?")) {
    GC_CAN_IGNORE jit_insn *ref0, *ref1, *ref3, *ref4;

    mz_runstack_skipped(jitter, 1);
    
    scheme_generate_non_tail(app->rand, jitter, 0, 1, 0);
    CHECK_LIMIT();

    mz_runstack_unskipped(jitter, 1);

    if (need_sync) mz_rs_sync();

    __START_SHORT_JUMPS__(branch_short);

    if (for_branch) {
      scheme_prepare_branch_jump(jitter, for_branch);
      CHECK_LIMIT();
    }

    ref1 = jit_bmsi_ul(jit_forward(), JIT_R0, 0x1);
    jit_ldxi_s(JIT_R1, JIT_R0, &((Scheme_Object *)0x0)->type);
    ref3 = jit_beqi_p(jit_forward(), JIT_R1, scheme_null_type);
    ref4 = jit_bnei_p(jit_forward(), JIT_R1, scheme_pair_type);
    CHECK_LIMIT();

    if (for_branch) {
      ref0 = jit_patchable_movi_p(JIT_V1, jit_forward());
      (void)jit_calli(sjc.list_p_branch_code);

      mz_patch_branch(ref3);

      scheme_add_branch_false_movi(for_branch, ref0);
      scheme_add_branch_false(for_branch, ref1);
      scheme_add_branch_false(for_branch, ref4);
      scheme_branch_for_true(jitter, for_branch);
    } else {
      GC_CAN_IGNORE jit_insn *ref5;

      (void)jit_calli(sjc.list_p_code);
      ref5 = jit_jmpi(jit_forward());

      mz_patch_branch(ref1);
      mz_patch_branch(ref4);
      (void)jit_movi_p(JIT_R0, scheme_false);
      ref1 = jit_jmpi(jit_forward());
      
      mz_patch_branch(ref3);
      (void)jit_movi_p(JIT_R0, scheme_true);

      mz_patch_ucbranch(ref5);
      mz_patch_ucbranch(ref1);
    }
    CHECK_LIMIT();

    __END_SHORT_JUMPS__(branch_short);

    return 1;
  } else if (IS_NAMED_PRIM(rator, "exact-nonnegative-integer?")
             || IS_NAMED_PRIM(rator, "exact-positive-integer?")) {
    GC_CAN_IGNORE jit_insn *ref, *ref2, *ref3, *ref4;
    
    LOG_IT(("inlined exact-nonnegative-integer?\n"));

    mz_runstack_skipped(jitter, 1);
    
    scheme_generate_non_tail(app->rand, jitter, 0, 1, 0);
    CHECK_LIMIT();

    mz_runstack_unskipped(jitter, 1);

    if (need_sync) mz_rs_sync();

    if (for_branch) {
      scheme_prepare_branch_jump(jitter, for_branch);
      CHECK_LIMIT();
    }

    /* Jump ahead if it's a fixnum: */
    __START_TINY_JUMPS__(1);
    ref = jit_bmsi_ul(jit_forward(), JIT_R0, 0x1);
    __END_TINY_JUMPS__(1);

    /* Check for positive bignum: */
    __START_SHORT_JUMPS__(branch_short);
    ref2 = mz_bnei_t(jit_forward(), JIT_R0, scheme_bignum_type, JIT_R2);
    jit_ldxi_s(JIT_R2, JIT_R0, &MZ_OPT_HASH_KEY(&((Scheme_Stx *)0x0)->iso));
    ref3 = jit_bmci_ul(jit_forward(), JIT_R2, 0x1);
    __END_SHORT_JUMPS__(branch_short);
    /* Ok bignum. Instead of jumping, install the fixnum 1: */
    (void)jit_movi_p(JIT_R0, scheme_make_integer(1));

    __START_TINY_JUMPS__(1);
    mz_patch_branch(ref);
    __END_TINY_JUMPS__(1);
    
    /* Check whether the fixnum is in range: */
    __START_SHORT_JUMPS__(branch_short);
    jit_rshi_l(JIT_R0, JIT_R0, 0x1);
    if (IS_NAMED_PRIM(rator, "exact-nonnegative-integer?")) {
      ref4 = jit_blti_l(jit_forward(), JIT_R0, 0);
    } else {
      ref4 = jit_blei_l(jit_forward(), JIT_R0, 0);
    }

    /* Ok --- it's in range */
    
    if (for_branch) {
      scheme_add_branch_false(for_branch, ref2);
      scheme_add_branch_false(for_branch, ref3);
      scheme_add_branch_false(for_branch, ref4);
      scheme_branch_for_true(jitter, for_branch);
      CHECK_LIMIT();
    } else {
      (void)jit_movi_p(JIT_R0, scheme_true);
      ref = jit_jmpi(jit_forward());
      mz_patch_branch(ref2);
      mz_patch_branch(ref3);
      mz_patch_branch(ref4);
      (void)jit_movi_p(JIT_R0, scheme_false);
      mz_patch_ucbranch(ref);
    }
    
    __END_SHORT_JUMPS__(branch_short);

    return 1;
  } else if (!for_branch) {
    if (is_cXr_prim(((Scheme_Primitive_Proc *)rator)->name)) {
#     define MAX_LEVELS 2
      GC_CAN_IGNORE jit_insn *reffail = NULL, *ref;
      int steps, i;
      const char *name = ((Scheme_Primitive_Proc *)rator)->name;

      LOG_IT(("inlined %s\n", ((Scheme_Primitive_Proc *)rator)->name));

      for (steps = 0; name[steps+1] != 'r'; steps++) {
      }

      mz_runstack_skipped(jitter, 1);

      scheme_generate_non_tail(app->rand, jitter, 0, 1, 0);
      CHECK_LIMIT();

      mz_runstack_unskipped(jitter, 1);

      mz_rs_sync_fail_branch();

      __START_TINY_JUMPS__(1);

      if (steps > 1) {
        jit_movr_p(JIT_R2, JIT_R0); /* save original argument */
      }
      for (i = 0; i < steps; i++) {
        if (!sjc.skip_checks) {
          if (!i) {
            ref = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);
            reffail = _jit.x.pc;
            __END_TINY_JUMPS__(1);
            if (steps == 1) {
              if (name[1] == 'a') {
                (void)jit_calli(sjc.bad_car_code);
              } else {
                (void)jit_calli(sjc.bad_cdr_code);
              }
            } else if (steps == 2) {
              if (name[1] == 'a') {
                if (name[2] == 'a') {
                  (void)jit_calli(sjc.bad_caar_code);
                } else {
                  (void)jit_calli(sjc.bad_cadr_code);
                }
              } else {
                if (name[2] == 'a') {
                  (void)jit_calli(sjc.bad_cdar_code);
                } else {
                  (void)jit_calli(sjc.bad_cddr_code);
                }
              }
            } else {
              (void)jit_movi_p(JIT_R0, ((Scheme_Primitive_Proc *)rator)->prim_val);
              (void)jit_calli(sjc.bad_cXr_code);
            }
            __START_TINY_JUMPS__(1);
            mz_patch_branch(ref);
          } else {
            (void)jit_bmsi_ul(reffail, JIT_R0, 0x1);
          }
          (void)mz_bnei_t(reffail, JIT_R0, scheme_pair_type, JIT_R1);
        } else {
          reffail = NULL;
        }
        if (name[steps - i] == 'a') {
          (void)jit_ldxi_p(JIT_R0, JIT_R0, &((Scheme_Simple_Object *)0x0)->u.pair_val.car);
        } else {
          (void)jit_ldxi_p(JIT_R0, JIT_R0, &((Scheme_Simple_Object *)0x0)->u.pair_val.cdr);
        }
        VALIDATE_RESULT(JIT_R0);
        CHECK_LIMIT();
      }
      __END_TINY_JUMPS__(1);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "mcar")
               || IS_NAMED_PRIM(rator, "mcdr")) {
      GC_CAN_IGNORE jit_insn *reffail = NULL, *ref;
      const char *name = ((Scheme_Primitive_Proc *)rator)->name;

      LOG_IT(("inlined %s\n", ((Scheme_Primitive_Proc *)rator)->name));

      mz_runstack_skipped(jitter, 1);

      scheme_generate_non_tail(app->rand, jitter, 0, 1, 0);
      CHECK_LIMIT();

      mz_runstack_unskipped(jitter, 1);

      mz_rs_sync_fail_branch();

      __START_TINY_JUMPS__(1);

      ref = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);
      reffail = _jit.x.pc;
      __END_TINY_JUMPS__(1);
      if (name[2] == 'a') {
        (void)jit_calli(sjc.bad_mcar_code);
      } else {
        (void)jit_calli(sjc.bad_mcdr_code);
      }
      __START_TINY_JUMPS__(1);
      mz_patch_branch(ref);
      (void)mz_bnei_t(reffail, JIT_R0, scheme_mutable_pair_type, JIT_R1);
      if (name[2] == 'a') {
        (void)jit_ldxi_p(JIT_R0, JIT_R0, &((Scheme_Simple_Object *)0x0)->u.pair_val.car);
      } else {
        (void)jit_ldxi_p(JIT_R0, JIT_R0, &((Scheme_Simple_Object *)0x0)->u.pair_val.cdr);
      }
      VALIDATE_RESULT(JIT_R0);
      CHECK_LIMIT();
      __END_TINY_JUMPS__(1);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-car")
               || IS_NAMED_PRIM(rator, "unsafe-mcar")
               || IS_NAMED_PRIM(rator, "unsafe-cdr")
               || IS_NAMED_PRIM(rator, "unsafe-mcdr")) {
      const char *name = ((Scheme_Primitive_Proc *)rator)->name;

      LOG_IT(("inlined %s\n", ((Scheme_Primitive_Proc *)rator)->name));

      mz_runstack_skipped(jitter, 1);

      scheme_generate_non_tail(app->rand, jitter, 0, 1, 0);
      CHECK_LIMIT();

      mz_runstack_unskipped(jitter, 1);

      if (!strcmp(name, "unsafe-car") || !strcmp(name, "unsafe-mcar")) {
        (void)jit_ldxi_p(JIT_R0, JIT_R0, &((Scheme_Simple_Object *)0x0)->u.pair_val.car);
      } else {
        (void)jit_ldxi_p(JIT_R0, JIT_R0, &((Scheme_Simple_Object *)0x0)->u.pair_val.cdr);
      }
      CHECK_LIMIT();

      return 1;
    } else if (IS_NAMED_PRIM(rator, "length")) {
      mz_runstack_skipped(jitter, 1);

      scheme_generate_non_tail(app->rand, jitter, 0, 1, 0);
      CHECK_LIMIT();

      mz_runstack_unskipped(jitter, 1);

      mz_rs_sync();
      (void)jit_calli(sjc.list_length_code);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "vector-length")
               || IS_NAMED_PRIM(rator, "fxvector-length")
               || IS_NAMED_PRIM(rator, "unsafe-vector-length")
               || IS_NAMED_PRIM(rator, "unsafe-fxvector-length")
               || IS_NAMED_PRIM(rator, "unsafe-vector*-length")
               || IS_NAMED_PRIM(rator, "flvector-length")
               || IS_NAMED_PRIM(rator, "unsafe-flvector-length")) {
      GC_CAN_IGNORE jit_insn *reffail, *ref;
      int unsafe = 0, for_fl = 0, for_fx = 0, can_chaperone = 0;

      if (IS_NAMED_PRIM(rator, "unsafe-vector*-length")
          || IS_NAMED_PRIM(rator, "unsafe-fxvector-length")) {
        unsafe = 1;
      } else if (IS_NAMED_PRIM(rator, "unsafe-vector-length")) {
        unsafe = 1;
        can_chaperone = 1;
      } else if (IS_NAMED_PRIM(rator, "flvector-length")) {
        for_fl = 1;
      } else if (IS_NAMED_PRIM(rator, "unsafe-flvector-length")) {
        unsafe = 1;
        for_fl = 1;
      } else if (IS_NAMED_PRIM(rator, "fxvector-length")) {
        for_fx = 1;
      } else {
        can_chaperone = 1;
      }

      LOG_IT(("inlined vector-length\n"));

      mz_runstack_skipped(jitter, 1);

      scheme_generate_non_tail(app->rand, jitter, 0, 1, 0);
      CHECK_LIMIT();

      mz_runstack_unskipped(jitter, 1);

      if (!unsafe) {
        mz_rs_sync_fail_branch();

        __START_TINY_JUMPS__(1);
        ref = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);
        __END_TINY_JUMPS__(1);
        
        reffail = _jit.x.pc;
        if (for_fl)
          (void)jit_calli(sjc.bad_flvector_length_code);
        else if (for_fx)
          (void)jit_calli(sjc.bad_fxvector_length_code);
        else {
          (void)jit_calli(sjc.bad_vector_length_code);
          /* can return with updated R0 */
        }
        /* bad_vector_length_code may unpack a proxied object */

        __START_TINY_JUMPS__(1);
        mz_patch_branch(ref);
        if (for_fl)
          (void)mz_bnei_t(reffail, JIT_R0, scheme_flvector_type, JIT_R1);
        else if (for_fx)
          (void)mz_bnei_t(reffail, JIT_R0, scheme_fxvector_type, JIT_R1);
        else
          (void)mz_bnei_t(reffail, JIT_R0, scheme_vector_type, JIT_R1);
        __END_TINY_JUMPS__(1);
      } else if (can_chaperone) {
        __START_TINY_JUMPS__(1);
        ref = mz_bnei_t(jit_forward(), JIT_R0, scheme_chaperone_type, JIT_R1);
        jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&((Scheme_Chaperone *)0x0)->val);
        mz_patch_branch(ref);
        __END_TINY_JUMPS__(1);
      }
      CHECK_LIMIT();

      if (!for_fl)
        (void)jit_ldxi_l(JIT_R0, JIT_R0, &SCHEME_VEC_SIZE(0x0));
      else
        (void)jit_ldxi_l(JIT_R0, JIT_R0, &SCHEME_FLVEC_SIZE(0x0));
      jit_lshi_l(JIT_R0, JIT_R0, 1);
      jit_ori_l(JIT_R0, JIT_R0, 0x1);
            
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-string-length")
               || IS_NAMED_PRIM(rator, "unsafe-bytes-length")) {
      LOG_IT(("inlined string-length\n"));

      mz_runstack_skipped(jitter, 1);

      scheme_generate_non_tail(app->rand, jitter, 0, 1, 0);
      CHECK_LIMIT();

      mz_runstack_unskipped(jitter, 1);

      if (IS_NAMED_PRIM(rator, "unsafe-string-length"))
        (void)jit_ldxi_l(JIT_R0, JIT_R0, &SCHEME_CHAR_STRLEN_VAL(0x0));
      else
        (void)jit_ldxi_l(JIT_R0, JIT_R0, &SCHEME_BYTE_STRLEN_VAL(0x0));
      jit_lshi_l(JIT_R0, JIT_R0, 1);
      jit_ori_l(JIT_R0, JIT_R0, 0x1);
      
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unbox")) {
      GC_CAN_IGNORE jit_insn *reffail, *ref, *refdone;

      LOG_IT(("inlined unbox\n"));

      mz_runstack_skipped(jitter, 1);

      scheme_generate_non_tail(app->rand, jitter, 0, 1, 0);
      CHECK_LIMIT();

      mz_runstack_unskipped(jitter, 1);

      mz_rs_sync();

      __START_TINY_JUMPS__(1);
      ref = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);
      __END_TINY_JUMPS__(1);

      reffail = _jit.x.pc;
      (void)jit_calli(sjc.unbox_code);

      __START_TINY_JUMPS__(1);
      refdone = jit_jmpi(jit_forward());
      mz_patch_branch(ref);
      (void)mz_bnei_t(reffail, JIT_R0, scheme_box_type, JIT_R1);
      __END_TINY_JUMPS__(1);

      (void)jit_ldxi_p(JIT_R0, JIT_R0, &SCHEME_BOX_VAL(0x0));
      
      __START_TINY_JUMPS__(1);
      mz_patch_ucbranch(refdone);
      __END_TINY_JUMPS__(1);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-unbox*")) {
      LOG_IT(("inlined unbox\n"));

      mz_runstack_skipped(jitter, 1);

      scheme_generate_non_tail(app->rand, jitter, 0, 1, 0);
      CHECK_LIMIT();

      mz_runstack_unskipped(jitter, 1);

      (void)jit_ldxi_p(JIT_R0, JIT_R0, &SCHEME_BOX_VAL(0x0));
      
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-unbox")) {
      GC_CAN_IGNORE jit_insn *ref, *ref2;

      LOG_IT(("inlined unbox\n"));

      mz_runstack_skipped(jitter, 1);

      scheme_generate_non_tail(app->rand, jitter, 0, 1, 0);
      CHECK_LIMIT();

      mz_runstack_unskipped(jitter, 1);

      mz_rs_sync();

      /* check for chaperone: */
      __START_TINY_JUMPS__(1);
      ref = mz_bnei_t(jit_forward(), JIT_R0, scheme_chaperone_type, JIT_R1);
      (void)jit_calli(sjc.unbox_code);
      jit_retval(JIT_R0);
      ref2 = jit_jmpi(jit_forward());
      jit_retval(JIT_R0);
      mz_patch_branch(ref);
      CHECK_LIMIT();
      __END_TINY_JUMPS__(1);

      (void)jit_ldxi_p(JIT_R0, JIT_R0, &SCHEME_BOX_VAL(0x0));

      __START_TINY_JUMPS__(1);
      mz_patch_ucbranch(ref2);
      __END_TINY_JUMPS__(1);
      
      return 1;
    } else if (IS_NAMED_PRIM(rator, "syntax-e")) {
      LOG_IT(("inlined syntax-e\n"));

      mz_runstack_skipped(jitter, 1);

      scheme_generate_non_tail(app->rand, jitter, 0, 1, 0);
      CHECK_LIMIT();

      mz_runstack_unskipped(jitter, 1);

      mz_rs_sync();

      (void)jit_calli(sjc.syntax_e_code);
      
      return 1;
    } else if (IS_NAMED_PRIM(rator, "imag-part")
               || IS_NAMED_PRIM(rator, "real-part")
               || IS_NAMED_PRIM(rator, "flimag-part")
               || IS_NAMED_PRIM(rator, "flreal-part")) {
      GC_CAN_IGNORE jit_insn *reffail = NULL, *ref, *refdone;
      const char *name = ((Scheme_Primitive_Proc *)rator)->name;
      int unbox;

      LOG_IT(("inlined %s\n", name));

      unbox = jitter->unbox;
      jitter->unbox = 0;

      mz_runstack_skipped(jitter, 1);

      scheme_generate_non_tail(app->rand, jitter, 0, 1, 0);
      CHECK_LIMIT();

      mz_runstack_unskipped(jitter, 1);

      jitter->unbox = unbox;

      mz_rs_sync();

      __START_TINY_JUMPS__(1);

      ref = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);
      reffail = _jit.x.pc;
      __END_TINY_JUMPS__(1);
      if (name[0] == 'i') {
        (void)jit_calli(sjc.imag_part_code);
      } else if (name[2] == 'i') {
        (void)jit_calli(sjc.bad_flimag_part_code);
      } else if (name[0] == 'r') {
        (void)jit_calli(sjc.real_part_code);
      } else {
        (void)jit_calli(sjc.bad_flreal_part_code);
      }
      if (name[0] != 'f') {
        /* can return */
        CHECK_LIMIT();
        __START_TINY_JUMPS__(1);
        refdone = jit_jmpi(jit_forward());
        __END_TINY_JUMPS__(1);
      } else {
        refdone = NULL;
      }
      __START_TINY_JUMPS__(1);
      mz_patch_branch(ref);
      (void)mz_bnei_t(reffail, JIT_R0, scheme_complex_type, JIT_R1);
      if (name[0] == 'i') {
        (void)jit_ldxi_p(JIT_R0, JIT_R0, &((Scheme_Complex *)0x0)->i);
      } else if (name[0] == 'r') {
        (void)jit_ldxi_p(JIT_R0, JIT_R0, &((Scheme_Complex *)0x0)->r);
      } else {
        /* real part must always be inexact */
        (void)jit_ldxi_p(JIT_R1, JIT_R0, &((Scheme_Complex *)0x0)->r);
        CHECK_LIMIT();
        (void)mz_bnei_t(reffail, JIT_R1, scheme_double_type, JIT_R2);
        if (name[2] == 'i') {
          (void)jit_ldxi_p(JIT_R0, JIT_R0, &((Scheme_Complex *)0x0)->i);
        } else {
          jit_movr_p(JIT_R0, JIT_R1);
        }
      }
      VALIDATE_RESULT(JIT_R0);
      if (refdone)
        mz_patch_ucbranch(refdone);
      CHECK_LIMIT();
      __END_TINY_JUMPS__(1);

      if (jitter->unbox) /* for fl....-part: */
        scheme_generate_unboxing(jitter, JIT_R0);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-flimag-part")
               || IS_NAMED_PRIM(rator, "unsafe-flreal-part")) {
      const char *name = ((Scheme_Primitive_Proc *)rator)->name;
      int unbox;

      LOG_IT(("inlined %s\n", name));
      
      mz_runstack_skipped(jitter, 1);

      unbox = jitter->unbox;
      jitter->unbox = 0;

      scheme_generate_non_tail(app->rand, jitter, 0, 1, 0);
      CHECK_LIMIT();

      jitter->unbox = unbox;

      mz_runstack_unskipped(jitter, 1);

      if (name[9] == 'i') {
        (void)jit_ldxi_p(JIT_R0, JIT_R0, &((Scheme_Complex *)0x0)->i);
      } else {
        (void)jit_ldxi_p(JIT_R0, JIT_R0, &((Scheme_Complex *)0x0)->r);
      }
      CHECK_LIMIT();

      if (jitter->unbox)
        scheme_generate_unboxing(jitter, JIT_R0);
      
      return 1;
    } else if (IS_NAMED_PRIM(rator, "add1")) {
      scheme_generate_arith(jitter, rator, app->rand, NULL, 1, ARITH_ADD, 0, 1, NULL, 1, 0, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "sub1")) {
      scheme_generate_arith(jitter, rator, app->rand, NULL, 1, ARITH_SUB, 0, 1, NULL, 1, 0, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "-")) {
      scheme_generate_arith(jitter, rator, app->rand, NULL, 1, ARITH_SUB, 0, 0, NULL, 1, 0, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "abs")) {
      scheme_generate_arith(jitter, rator, app->rand, NULL, 1, ARITH_ABS, 0, 0, NULL, 1, 0, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-fxabs")) {
      scheme_generate_arith(jitter, rator, app->rand, NULL, 1, ARITH_ABS, 0, 0, NULL, 1, 1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "fxabs")) {
      scheme_generate_arith(jitter, rator, app->rand, NULL, 1, ARITH_ABS, 0, 0, NULL, 1, -1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-flabs")) {
      scheme_generate_arith(jitter, rator, app->rand, NULL, 1, ARITH_ABS, 0, 0, NULL, 1, 0, 1, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "flabs")) {
      scheme_generate_arith(jitter, rator, app->rand, NULL, 1, ARITH_ABS, 0, 0, NULL, 1, 0, -1, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-flsqrt")) {
      scheme_generate_arith(jitter, rator, app->rand, NULL, 1, ARITH_SQRT, 0, 0, NULL, 1, 0, 1, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "flsqrt")) {
      scheme_generate_arith(jitter, rator, app->rand, NULL, 1, ARITH_SQRT, 0, 0, NULL, 1, 0, -1, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "flfloor")
               || IS_NAMED_PRIM(rator, "flceiling")
               || IS_NAMED_PRIM(rator, "flround")
               || IS_NAMED_PRIM(rator, "fltruncate")
               || IS_NAMED_PRIM(rator, "flsin")
               || IS_NAMED_PRIM(rator, "flcos")
               || IS_NAMED_PRIM(rator, "fltan")
               || IS_NAMED_PRIM(rator, "flasin")
               || IS_NAMED_PRIM(rator, "flacos")
               || IS_NAMED_PRIM(rator, "flatan")
               || IS_NAMED_PRIM(rator, "flexp")
               || IS_NAMED_PRIM(rator, "fllog")) {
      scheme_generate_arith(jitter, rator, app->rand, NULL, 1, ARITH_FLUNOP, 0, 0, NULL, 1, 0, -1, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "exact->inexact")
	       || IS_NAMED_PRIM(rator, "real->double-flonum")) {
      scheme_generate_arith(jitter, rator, app->rand, NULL, 1, ARITH_EX_INEX, 0, 0, NULL, 1, 0, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-fx->fl")) {
      scheme_generate_arith(jitter, rator, app->rand, NULL, 1, ARITH_EX_INEX, 0, 0, NULL, 1, 1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "->fl")
               || IS_NAMED_PRIM(rator, "fx->fl")) {
      scheme_generate_arith(jitter, rator, app->rand, NULL, 1, ARITH_EX_INEX, 0, 0, NULL, 1, -1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "inexact->exact")) {
      scheme_generate_arith(jitter, rator, app->rand, NULL, 1, ARITH_INEX_EX, 0, 0, NULL, 1, 0, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-fl->fx")) {
      scheme_generate_arith(jitter, rator, app->rand, NULL, 1, ARITH_INEX_EX, 0, 0, NULL, 1, 0, 1, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "fl->exact-integer")
               || IS_NAMED_PRIM(rator, "fl->fx")) {
      scheme_generate_arith(jitter, rator, app->rand, NULL, 1, ARITH_INEX_EX, 0, 0, NULL, 1, 0, -1, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "bitwise-not")) {
      scheme_generate_arith(jitter, rator, app->rand, NULL, 1, ARITH_NOT, 0, 9, NULL, 1, 0, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-fxnot")) {
      scheme_generate_arith(jitter, rator, app->rand, NULL, 1, ARITH_NOT, 0, 9, NULL, 1, 1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "fxnot")) {
      scheme_generate_arith(jitter, rator, app->rand, NULL, 1, ARITH_NOT, 0, 9, NULL, 1, -1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "vector-immutable")
               || IS_NAMED_PRIM(rator, "vector")) {
      return generate_vector_alloc(jitter, rator, NULL, app, NULL);
    } else if (IS_NAMED_PRIM(rator, "list*")
               || IS_NAMED_PRIM(rator, "values")) {
      /* on a single argument, `list*' or `values' is identity */
      mz_runstack_skipped(jitter, 1);
      scheme_generate_non_tail(app->rand, jitter, 0, 1, 0);
      CHECK_LIMIT();
      mz_runstack_unskipped(jitter, 1);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "list")) {
      mz_runstack_skipped(jitter, 1);
      scheme_generate_non_tail(app->rand, jitter, 0, 1, 0);
      CHECK_LIMIT();
      mz_rs_sync();
      mz_runstack_unskipped(jitter, 1);
      (void)jit_movi_p(JIT_R1, &scheme_null);
      return scheme_generate_cons_alloc(jitter, 0, 0);
    } else if (IS_NAMED_PRIM(rator, "box")) {
      mz_runstack_skipped(jitter, 1);
      scheme_generate_non_tail(app->rand, jitter, 0, 1, 0);
      CHECK_LIMIT();
      mz_runstack_unskipped(jitter, 1);
      mz_rs_sync();
      
#ifdef CAN_INLINE_ALLOC
      /* Inlined alloc */
      (void)jit_movi_p(JIT_R1, NULL); /* needed because R1 is marked during a GC */
      scheme_inline_alloc(jitter, sizeof(Scheme_Small_Object), scheme_box_type, 0, 1, 0, 0);
      CHECK_LIMIT();
      
      jit_stxi_p((intptr_t)&SCHEME_BOX_VAL(0x0) + OBJHEAD_SIZE, JIT_V1, JIT_R0);
      jit_addi_p(JIT_R0, JIT_V1, OBJHEAD_SIZE);
#else
      /* Non-inlined */
      JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
      mz_prepare(1);
      jit_pusharg_p(JIT_R0);
      {
        GC_CAN_IGNORE jit_insn *refr;
        (void)mz_finish_lwe(ts_scheme_box, refr);
      }
      jit_retval(JIT_R0);
#endif

      return 1;
    } else if (IS_NAMED_PRIM(rator, "future?")) { 
      generate_inlined_type_test(jitter, app, scheme_future_type, scheme_future_type, 1, for_branch, branch_short, need_sync);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "fsemaphore?")) { 
      generate_inlined_type_test(jitter, app, scheme_fsemaphore_type, scheme_fsemaphore_type, 1, for_branch, branch_short, need_sync);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "future")
               || IS_NAMED_PRIM(rator, "touch")
               || IS_NAMED_PRIM(rator, "fsemaphore-count")
               || IS_NAMED_PRIM(rator, "make-fsemaphore")
               || IS_NAMED_PRIM(rator, "fsemaphore-post")
               || IS_NAMED_PRIM(rator, "fsemaphore-wait")
               || IS_NAMED_PRIM(rator, "fsemaphore-try-wait?")) {
      /* Inline calls to future functions that specially support
         running in the future thread: */
      GC_CAN_IGNORE jit_insn *refr;

      mz_runstack_skipped(jitter, 1);
      scheme_generate_non_tail(app->rand, jitter, 0, 1, 0);
      CHECK_LIMIT();
      mz_runstack_unskipped(jitter, 1);
        
      /* Push the arg onto the runstack */ 
      mz_pushr_p(JIT_R0);
      mz_rs_sync();
      JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
      CHECK_LIMIT();

      mz_prepare(2);
      jit_pusharg_p(JIT_RUNSTACK);
      jit_movi_i(JIT_R0, 1);
      jit_pusharg_i(JIT_R0);

      if (IS_NAMED_PRIM(rator, "make-fsemaphore"))
        (void)mz_finish_lwe(ts_scheme_make_fsemaphore, refr);
      else
        (void)mz_finish_lwe(((Scheme_Primitive_Proc *)rator)->prim_val, refr);

      jit_retval(JIT_R0);

      mz_popr_x(); /* remove arg */

      return 1;
    }
  }

  if (!for_branch) {
    scheme_console_printf("Inlining expected.\n");
    abort();
  }

  --scheme_direct_call_count;

  return 0;
}

static int generate_two_args(Scheme_Object *rand1, Scheme_Object *rand2, mz_jit_state *jitter, 
                             int order_matters, int skipped)
/* de-sync's rs.
   Results go into R0 and R1. If !order_matters, and if only the
   second is simple, then the arguments will be in reverse order. */
{
  int simple1, simple2, direction = 1;
  
  simple1 = scheme_is_relatively_constant_and_avoids_r1(rand1, rand2);
  simple2 = scheme_is_relatively_constant_and_avoids_r1(rand2, rand1);

  if (!simple1) {
    if (simple2) {
      mz_runstack_skipped(jitter, skipped);

      scheme_generate_non_tail(rand1, jitter, 0, 1, 0); /* no sync... */
      CHECK_LIMIT();

      jit_movr_p(JIT_R1, JIT_R0);

      scheme_generate(rand2, jitter, 0, 0, 0, JIT_R0, NULL); /* no sync... */
      CHECK_LIMIT();

      if (order_matters) {
        /* Swap arguments: */
        int reg_status;
        reg_status = mz_CURRENT_REG_STATUS_VALID();
        jit_movr_p(JIT_R2, JIT_R0);
        jit_movr_p(JIT_R0, JIT_R1);
        jit_movr_p(JIT_R1, JIT_R2);
        if (reg_status) {
          int pos;
          pos = jitter->r0_status;
          jitter->r0_status = jitter->r1_status;
          jitter->r1_status = pos;
          mz_SET_REG_STATUS_VALID(1);
        }
      } else
        direction = -1;

      mz_runstack_unskipped(jitter, skipped);
    } else {
      mz_runstack_skipped(jitter, skipped);
      scheme_generate_non_tail(rand1, jitter, 0, 1, 0); /* no sync... */
      CHECK_LIMIT();
      mz_runstack_unskipped(jitter, skipped);

      mz_rs_dec(1);
      CHECK_RUNSTACK_OVERFLOW();
      mz_runstack_pushed(jitter, 1);
      mz_rs_str(JIT_R0);
      mz_runstack_skipped(jitter, skipped-1);

      scheme_generate_non_tail(rand2, jitter, 0, 1, 0); /* no sync... */
      CHECK_LIMIT();

      jit_movr_p(JIT_R1, JIT_R0);
      mz_rs_ldr(JIT_R0);

      mz_runstack_unskipped(jitter, skipped-1);
      mz_rs_inc(1);
      mz_runstack_popped(jitter, 1);
    }
  } else {
    mz_runstack_skipped(jitter, skipped);

    if (simple2) {
      scheme_generate(rand2, jitter, 0, 0, 0, JIT_R1, NULL); /* no sync... */
      CHECK_LIMIT();
    } else {
      scheme_generate_non_tail(rand2, jitter, 0, 1, 0); /* no sync... */
      CHECK_LIMIT();
      jit_movr_p(JIT_R1, JIT_R0);
    }

    scheme_generate(rand1, jitter, 0, 0, 0, JIT_R0, NULL); /* no sync... */
    CHECK_LIMIT();

    mz_runstack_unskipped(jitter, skipped);
  }

  return direction;
}

static int generate_binary_char(mz_jit_state *jitter, Scheme_App3_Rec *app,
                                Branch_Info *for_branch, int branch_short)
/* de-sync'd ok */
{
  Scheme_Object *r1, *r2, *rator = app->rator;
  GC_CAN_IGNORE jit_insn *reffail = NULL, *ref;
  int direct = 0, direction;

  LOG_IT(("inlined %s\n", ((Scheme_Primitive_Proc *)rator)->name));

  r1 = app->rand1;
  r2 = app->rand2;
  direction = generate_two_args(r1, r2, jitter, 1, 2);
  CHECK_LIMIT();

  mz_rs_sync();

  __START_SHORT_JUMPS__(branch_short);
  
  if (!SCHEME_CHARP(r1)) {
    GC_CAN_IGNORE jit_insn *pref;
    pref = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);
    reffail = _jit.x.pc;
    (void)jit_movi_p(JIT_R2, ((Scheme_Primitive_Proc *)rator)->prim_val);
    __END_SHORT_JUMPS__(branch_short);
    if (direction > 0) {
      (void)jit_jmpi(sjc.call_original_binary_rev_arith_code);
    } else {
      (void)jit_jmpi(sjc.call_original_binary_arith_code);
    }
    __START_SHORT_JUMPS__(branch_short);
    mz_patch_branch(pref);
    (void)mz_bnei_t(reffail, JIT_R0, scheme_char_type, JIT_R2);
    CHECK_LIMIT();
  } else {
    if (!direct)
      direct = (SCHEME_CHAR_VAL(r1) < 256);
  }
  if (!SCHEME_CHARP(r2)) {
    if (!reffail) {
      GC_CAN_IGNORE jit_insn *pref;
      pref = jit_bmci_ul(jit_forward(), JIT_R1, 0x1);
      reffail = _jit.x.pc;
      (void)jit_movi_p(JIT_R2, ((Scheme_Primitive_Proc *)rator)->prim_val);
      __END_SHORT_JUMPS__(branch_short);
      if (direction > 0) {
        (void)jit_jmpi(sjc.call_original_binary_rev_arith_code);
      } else {
        (void)jit_jmpi(sjc.call_original_binary_arith_code);
      }
      __START_SHORT_JUMPS__(branch_short);
      mz_patch_branch(pref);
    } else {
      (void)jit_bmsi_ul(reffail, JIT_R1, 0x1);
    }
    (void)mz_bnei_t(reffail, JIT_R1, scheme_char_type, JIT_R2);
    CHECK_LIMIT();
  } else {
    if (!direct)
      direct = (SCHEME_CHAR_VAL(r2) < 256);
  }

  if (for_branch) {
    scheme_prepare_branch_jump(jitter, for_branch);
    CHECK_LIMIT();
  }

  if (!direct) {
    /* Extract character value */
    jit_ldxi_i(JIT_R0, JIT_R0, (int)&SCHEME_CHAR_VAL((Scheme_Object *)0x0));
    jit_ldxi_i(JIT_R1, JIT_R1, (int)&SCHEME_CHAR_VAL((Scheme_Object *)0x0));
    ref = jit_bner_i(jit_forward(), JIT_R0, JIT_R1);
  } else {
    ref = jit_bner_p(jit_forward(), JIT_R0, JIT_R1);
  }
  CHECK_LIMIT();
  if (for_branch) {
    scheme_add_branch_false(for_branch, ref);
    scheme_branch_for_true(jitter, for_branch);
    CHECK_LIMIT();
  } else {
    GC_CAN_IGNORE jit_insn *ref2;
    (void)jit_movi_p(JIT_R0, scheme_true);
    ref2 = jit_jmpi(jit_forward());
    mz_patch_branch(ref);
    (void)jit_movi_p(JIT_R0, scheme_false);
    mz_patch_ucbranch(ref2);
  }
    
  __END_SHORT_JUMPS__(branch_short);

  return 1;
}

static int generate_vector_op(mz_jit_state *jitter, int set, int int_ready, int base_offset, 
                              int for_fl, int unsafe, 
                              int unbox_flonum, int result_ignored, int can_chaperone, 
                              int for_struct, int for_fx, int check_mutable)
/* R0 has vector. In set mode, R2 has value; if not unboxed, not unsafe, or can chaperone,
   RUNSTACK has space for a temporary (intended for R2).
   If int_ready, R1 has num index (for safe or can-chaperone mode) and V1 has pre-computed
   offset, otherwise (when not int_ready) R1 has fixnum index */
{
  GC_CAN_IGNORE jit_insn *ref, *reffail, *pref;

  if (!sjc.skip_checks && (!unsafe || can_chaperone)) {
    if (set && !unbox_flonum)
      mz_rs_str(JIT_R2);
    __START_TINY_JUMPS__(1);
    if (!unsafe) {
      ref = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);
    } else {
      /* assert: can_chaperone */
      ref = mz_bnei_t(jit_forward(), JIT_R0, scheme_chaperone_type, JIT_R2);
    }
    __END_TINY_JUMPS__(1);

    reffail = _jit.x.pc;
    if (int_ready) {
      jit_lshi_ul(JIT_R1, JIT_R1, 1);
      jit_ori_l(JIT_R1, JIT_R1, 0x1);
    }
    if (set) {
      if (for_struct)
        (void)jit_calli(sjc.struct_raw_set_code);
      else if (for_fx)
        (void)jit_calli(sjc.fxvector_set_check_index_code);
      else if (!for_fl)
        (void)jit_calli(sjc.vector_set_check_index_code);
      else if (unbox_flonum)
        (void)jit_calli(sjc.flvector_set_flonum_check_index_code);
      else
        (void)jit_calli(sjc.flvector_set_check_index_code);
    } else {
      if (for_struct)
        (void)jit_calli(sjc.struct_raw_ref_code);
      else if (for_fx)
        (void)jit_calli(sjc.fxvector_ref_check_index_code);
      else if (!for_fl)
        (void)jit_calli(sjc.vector_ref_check_index_code);
      else
        (void)jit_calli(sjc.flvector_ref_check_index_code);
    }
    CHECK_LIMIT();
    if (can_chaperone) {
      pref = jit_jmpi(jit_forward());
    } else {
      /* doesn't return */
      pref = NULL;
    }

    __START_TINY_JUMPS__(1);
    mz_patch_branch(ref);
    if (for_struct && unsafe && can_chaperone) 
      (void)mz_beqi_t(reffail, JIT_R0, scheme_proc_chaperone_type, JIT_R2);
    if (!unsafe) {
      if (!int_ready)
        (void)jit_bmci_ul(reffail, JIT_R1, 0x1);
      if (set && for_fx)
        (void)jit_bmci_ul(reffail, JIT_R2, 0x1);
      if (for_fx) {
        (void)mz_bnei_t(reffail, JIT_R0, scheme_fxvector_type, JIT_R2);
        jit_ldxi_l(JIT_R2, JIT_R0, (int)&SCHEME_FXVEC_SIZE(0x0));
      } else if (!for_fl) {
        (void)mz_bnei_t(reffail, JIT_R0, scheme_vector_type, JIT_R2);
        if (check_mutable) {
          jit_ldxi_s(JIT_R2, JIT_R0, &MZ_OPT_HASH_KEY((Scheme_Inclhash_Object *)0x0));
          (void)jit_bmsi_ul(reffail, JIT_R2, 0x1);
        }
        jit_ldxi_l(JIT_R2, JIT_R0, (int)&SCHEME_VEC_SIZE(0x0));
      } else {
        (void)mz_bnei_t(reffail, JIT_R0, scheme_flvector_type, JIT_R2);
        jit_ldxi_l(JIT_R2, JIT_R0, (int)&SCHEME_FLVEC_SIZE(0x0));
      }
      if (!int_ready) {
        jit_rshi_ul(JIT_V1, JIT_R1, 1);
        (void)jit_bler_ul(reffail, JIT_R2, JIT_V1);
      } else {
        (void)jit_bler_ul(reffail, JIT_R2, JIT_R1);
      }
      CHECK_LIMIT();

      if (for_fl && set && !unbox_flonum) {
        jit_ldr_p(JIT_R2, JIT_RUNSTACK);
        (void)jit_bmsi_ul(reffail, JIT_R2, 0x1);
        (void)mz_bnei_t(reffail, JIT_R2, scheme_double_type, JIT_R2);
        CHECK_LIMIT();
      }
    } else if (!int_ready) {
      jit_rshi_ul(JIT_V1, JIT_R1, 1);
    }

    __END_TINY_JUMPS__(1);
  } else {
    if (!int_ready)
      jit_rshi_ul(JIT_V1, JIT_R1, 1);
    pref = NULL;
  }

  if (!int_ready) {
    if (!for_fl)
      jit_lshi_ul(JIT_V1, JIT_V1, JIT_LOG_WORD_SIZE);
    else
      jit_lshi_ul(JIT_V1, JIT_V1, JIT_LOG_DOUBLE_SIZE);
    jit_addi_p(JIT_V1, JIT_V1, base_offset);
  }
  if (set) {
    if (!unbox_flonum && (!unsafe || can_chaperone))
      jit_ldr_p(JIT_R2, JIT_RUNSTACK);
    if (!for_fl) {
      jit_stxr_p(JIT_V1, JIT_R0, JIT_R2);
    } else {
      if (!unbox_flonum)
        jit_ldxi_d_fppush(JIT_FPR0, JIT_R2, &((Scheme_Double *)0x0)->double_val);  
      jit_stxr_d_fppop(JIT_V1, JIT_R0, JIT_FPR0);
      if (unbox_flonum) {
        --jitter->unbox_depth;
      }
    }
    if (can_chaperone)
      mz_patch_ucbranch(pref);
    if (!result_ignored)
      (void)jit_movi_p(JIT_R0, scheme_void);
  } else {
    if (!for_fl) {
      jit_ldxr_p(JIT_R0, JIT_R0, JIT_V1);
    } else {
      int fpr0;
      fpr0 = JIT_FPR_0(jitter->unbox_depth);
      jit_ldxr_d_fppush(fpr0, JIT_R0, JIT_V1);
      if (unbox_flonum)
        jitter->unbox_depth++;
      else
        scheme_generate_alloc_double(jitter, 0);
    }
    if (can_chaperone)
      mz_patch_ucbranch(pref);
  }

  return 1;
}

static int allocate_rectangular(mz_jit_state *jitter)
{
#ifdef CAN_INLINE_ALLOC
  /* Inlined alloc */
  scheme_inline_alloc(jitter, sizeof(Scheme_Complex), scheme_complex_type, 0, 1, 0, 0);
  CHECK_LIMIT();
  
  jit_stxi_p((intptr_t)&(((Scheme_Complex *)0x0)->r) + OBJHEAD_SIZE, JIT_V1, JIT_R0);
  jit_stxi_p((intptr_t)&(((Scheme_Complex *)0x0)->i) + OBJHEAD_SIZE, JIT_V1, JIT_R1);
  jit_addi_p(JIT_R0, JIT_V1, OBJHEAD_SIZE);
#else
  /* Non-inlined alloc */
  JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
  mz_prepare(2);
  jit_pusharg_p(JIT_R1);
  jit_pusharg_p(JIT_R0);
  {
    GC_CAN_IGNORE jit_insn *refr;
    (void)mz_finish_lwe(ts_scheme_make_complex, refr);
  }
  jit_retval(JIT_R0);
#endif

  return 1;
}

int scheme_generate_inlined_binary(mz_jit_state *jitter, Scheme_App3_Rec *app, int is_tail, int multi_ok, 
				   Branch_Info *for_branch, int branch_short, int need_sync, int result_ignored)
/* de-sync's; for branch, sync'd before  */
{
  Scheme_Object *rator = app->rator;

  if (!for_branch) {
    int k;
    k = inlineable_struct_prim(rator, jitter, 2, 2);
    if (k) {
      generate_inlined_struct_op(k, jitter, rator, app->rand1, app->rand2, for_branch, branch_short, multi_ok);
      scheme_direct_call_count++;
      return 1;
    }
  }

  if (!SCHEME_PRIMP(rator))
    return 0;

  if (!(SCHEME_PRIM_PROC_FLAGS(rator) & SCHEME_PRIM_IS_BINARY_INLINED))
    return 0;

  scheme_direct_call_count++;

  if (IS_NAMED_PRIM(rator, "eq?")) {
    Scheme_Object *a1, *a2;
    GC_CAN_IGNORE jit_insn *ref, *ref2;

    LOG_IT(("inlined eq?\n"));
    
    a1 = app->rand1;
    if (SCHEME_TYPE(a1) > _scheme_values_types_) {
      a2 = app->rand2;
    } else {
      a1 = app->rand2;
      a2 = app->rand1;
    }

    if (SCHEME_TYPE(a1) > _scheme_values_types_) {
      /* Compare to constant: */
      int reg_status;

      mz_runstack_skipped(jitter, 2);

      scheme_generate_non_tail(a2, jitter, 0, 1, 0);
      CHECK_LIMIT();
      if (need_sync) mz_rs_sync();
      
      mz_runstack_unskipped(jitter, 2);

      __START_SHORT_JUMPS__(branch_short);

      if (for_branch) {
        scheme_prepare_branch_jump(jitter, for_branch);
        CHECK_LIMIT();
      }

      reg_status = mz_CURRENT_REG_STATUS_VALID();
      
      if (!SCHEME_INTP(a1)
	  && !SCHEME_FALSEP(a1)
	  && !SCHEME_VOIDP(a1)
	  && !SAME_OBJ(a1, scheme_true)) {
	scheme_mz_load_retained(jitter, JIT_R1, a1);
	ref = jit_bner_p(jit_forward(), JIT_R0, JIT_R1);
        /* In case true is a fall-through, note that the test 
           didn't disturb R0: */
        if (for_branch) mz_SET_R0_STATUS_VALID(reg_status);
      } else {
	ref = mz_bnei_p(jit_forward(), JIT_R0, a1);
        /* In case true is a fall-through, note that the test 
           didn't disturb R0 or R1: */
        if (for_branch) mz_SET_REG_STATUS_VALID(reg_status);
      }

      if (for_branch) {
        scheme_add_branch_false(for_branch, ref);
        scheme_branch_for_true(jitter, for_branch);
        CHECK_LIMIT();
      } else {
	(void)jit_movi_p(JIT_R0, scheme_true);
	ref2 = jit_jmpi(jit_forward());
	mz_patch_branch(ref);
	(void)jit_movi_p(JIT_R0, scheme_false);
	mz_patch_ucbranch(ref2);
      }
      
      __END_SHORT_JUMPS__(branch_short);
    } else {
      /* Two complex expressions: */
      generate_two_args(a2, a1, jitter, 0, 2);
      CHECK_LIMIT();

      if (need_sync) mz_rs_sync();

      __START_SHORT_JUMPS__(branch_short);

      if (for_branch) {
        scheme_prepare_branch_jump(jitter, for_branch);
        CHECK_LIMIT();
      }

      ref = jit_bner_p(jit_forward(), JIT_R0, JIT_R1);
      if (for_branch) {
        scheme_add_branch_false(for_branch, ref);
        scheme_branch_for_true(jitter, for_branch);
        CHECK_LIMIT();
      } else {
	(void)jit_movi_p(JIT_R0, scheme_true);
	ref2 = jit_jmpi(jit_forward());
	mz_patch_branch(ref);
	(void)jit_movi_p(JIT_R0, scheme_false);
	mz_patch_ucbranch(ref2);
      }
      
      __END_SHORT_JUMPS__(branch_short);
    }
    
    return 1;
  }  else if (IS_NAMED_PRIM(rator, "equal?")) {
    GC_CAN_IGNORE jit_insn *ref_f, *ref_d, *refr;

    generate_two_args(app->rand1, app->rand2, jitter, 0, 2);
    CHECK_LIMIT();

    mz_rs_sync();
    JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();

    jit_prepare(2);
    jit_pusharg_p(JIT_R0);
    jit_pusharg_p(JIT_R1);
    mz_finish_prim_lwe(ts_scheme_equal, refr);
    jit_retval(JIT_R0);
    CHECK_LIMIT();

    __START_SHORT_JUMPS__(branch_short);
    
    if (for_branch) {
      scheme_prepare_branch_jump(jitter, for_branch);
      CHECK_LIMIT();
    }
    
    ref_f = jit_beqi_p(jit_forward(), JIT_R0, 0);

    if (for_branch) {
      scheme_add_branch_false(for_branch, ref_f);
      scheme_branch_for_true(jitter, for_branch);
    } else {
      (void)jit_movi_p(JIT_R0, scheme_true);
      ref_d = jit_jmpi(jit_forward());
      
      mz_patch_branch(ref_f);
      (void)jit_movi_p(JIT_R0, scheme_false);

      mz_patch_ucbranch(ref_d);
    }

    __END_SHORT_JUMPS__(branch_short);

    return 1;
  } else if (IS_NAMED_PRIM(rator, "eqv?")) {
    GC_CAN_IGNORE jit_insn *ref_f1, *ref_f2, *ref_f3, *ref_f4, *ref_f5;
    GC_CAN_IGNORE jit_insn *ref_d1, *ref_d2, *ref_t1;

    generate_two_args(app->rand1, app->rand2, jitter, 0, 2);
    CHECK_LIMIT();

    if (need_sync) mz_rs_sync();

    __START_SHORT_JUMPS__(branch_short);
    
    if (for_branch) {
      scheme_prepare_branch_jump(jitter, for_branch);
      CHECK_LIMIT();
    }

    /* eq? */
    ref_t1 = jit_beqr_p(jit_forward(), JIT_R0, JIT_R1);

    /* if either is fixnum, result is false */
    ref_f1 = jit_bmsi_ul(jit_forward(), JIT_R0, 0x1);
    ref_f2 = jit_bmsi_ul(jit_forward(), JIT_R1, 0x1);
    CHECK_LIMIT();

    /* Both have a tag */
    jit_ldxi_s(JIT_R2, JIT_R0, &((Scheme_Object *)0x0)->type);
    jit_ldxi_s(JIT_V1, JIT_R1, &((Scheme_Object *)0x0)->type);
    ref_f3 = jit_bner_i(jit_forward(), JIT_R2, JIT_V1);

    /* check in range of type treated by eqv: */
    ref_f4 = jit_blti_i(jit_forward(), JIT_R2, scheme_char_type);
    ref_f5 = jit_bgti_i(jit_forward(), JIT_R2, scheme_complex_type);
    CHECK_LIMIT();
    
    /* in range of interesting types, so break out the generic comparison */
    if (for_branch) {
      scheme_add_branch_false_movi(for_branch, jit_patchable_movi_p(JIT_V1, jit_forward()));    
      (void)jit_calli(sjc.eqv_branch_code);

      scheme_add_branch_false(for_branch, ref_f1);
      scheme_add_branch_false(for_branch, ref_f2);
      scheme_add_branch_false(for_branch, ref_f3);
      scheme_add_branch_false(for_branch, ref_f4);
      scheme_add_branch_false(for_branch, ref_f5);

      mz_patch_branch(ref_t1);
      scheme_branch_for_true(jitter, for_branch);
    } else {
      (void)jit_calli(sjc.eqv_code);
      ref_d1 = jit_jmpi(jit_forward());

      mz_patch_branch(ref_t1);
      (void)jit_movi_p(JIT_R0, scheme_true);
      ref_d2 = jit_jmpi(jit_forward());
      
      mz_patch_branch(ref_f1);
      mz_patch_branch(ref_f2);
      mz_patch_branch(ref_f3);
      mz_patch_branch(ref_f4);
      mz_patch_branch(ref_f5);

      (void)jit_movi_p(JIT_R0, scheme_false);

      mz_patch_ucbranch(ref_d1);
      mz_patch_ucbranch(ref_d2);
    }

    __END_SHORT_JUMPS__(branch_short);

    return 1;
  } else if (IS_NAMED_PRIM(rator, "=")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_EQUAL, 0, for_branch, branch_short, 0, 0, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "unsafe-fx=")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_EQUAL, 0, for_branch, branch_short, 1, 0, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "fx=")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_EQUAL, 0, for_branch, branch_short, -1, 0, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "unsafe-fl=")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_EQUAL, 0, for_branch, branch_short, 0, 1, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "fl=")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_EQUAL, 0, for_branch, branch_short, 0, -1, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "<=")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_LEQ, 0, for_branch, branch_short, 0, 0, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "unsafe-fx<=")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_LEQ, 0, for_branch, branch_short, 1, 0, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "fx<=")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_LEQ, 0, for_branch, branch_short, -1, 0, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "unsafe-fl<=")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_LEQ, 0, for_branch, branch_short, 0, 1, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "fl<=")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_LEQ, 0, for_branch, branch_short, 0, -1, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "<")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_LT, 0, for_branch, branch_short, 0, 0, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "unsafe-fx<")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_LT, 0, for_branch, branch_short, 1, 0, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "fx<")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_LT, 0, for_branch, branch_short, -1, 0, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "unsafe-fl<")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_LT, 0, for_branch, branch_short, 0, 1, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "fl<")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_LT, 0, for_branch, branch_short, 0, -1, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, ">=")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_GEQ, 0, for_branch, branch_short, 0, 0, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "unsafe-fx>=")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_GEQ, 0, for_branch, branch_short, 1, 0, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "fx>=")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_GEQ, 0, for_branch, branch_short, -1, 0, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "unsafe-fl>=")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_GEQ, 0, for_branch, branch_short, 0, 1, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "fl>=")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_GEQ, 0, for_branch, branch_short, 0, -1, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, ">")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_GT, 0, for_branch, branch_short, 0, 0, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "unsafe-fx>")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_GT, 0, for_branch, branch_short, 1, 0, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "fx>")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_GT, 0, for_branch, branch_short, -1, 0, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "unsafe-fl>")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_GT, 0, for_branch, branch_short, 0, 1, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "fl>")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_GT, 0, for_branch, branch_short, 0, -1, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "bitwise-bit-set?")) {
    scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, 0, CMP_BIT, 0, for_branch, branch_short, 0, 0, NULL);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "char=?")) {
    generate_binary_char(jitter, app, for_branch, branch_short);
    return 1;
  } else if (!for_branch) {
    if (IS_NAMED_PRIM(rator, "+")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_ADD, 0, 0, NULL, 1, 0, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-fx+")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_ADD, 0, 0, NULL, 1, 1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "fx+")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_ADD, 0, 0, NULL, 1, -1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-fl+")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_ADD, 0, 0, NULL, 1, 0, 1, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "fl+")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_ADD, 0, 0, NULL, 1, 0, -1, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "-")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_SUB, 0, 0, NULL, 1, 0, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-fx-")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_SUB, 0, 0, NULL, 1, 1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "fx-")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_SUB, 0, 0, NULL, 1, -1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-fl-")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_SUB, 0, 0, NULL, 1, 0, 1, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "fl-")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_SUB, 0, 0, NULL, 1, 0, -1, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "*")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_MUL, 0, 0, NULL, 1, 0, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-fx*")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_MUL, 0, 0, NULL, 1, 1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "fx*")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_MUL, 0, 0, NULL, 1, -1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-fl*")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_MUL, 0, 0, NULL, 1, 0, 1, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "fl*")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_MUL, 0, 0, NULL, 1, 0, -1, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "/")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_DIV, 0, 0, NULL, 1, 0, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-fl/")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_DIV, 0, 0, NULL, 1, 0, 1, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "fl/")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_DIV, 0, 0, NULL, 1, 0, -1, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "quotient")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_QUOT, 0, 0, NULL, 1, 0, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-fxquotient")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_QUOT, 0, 0, NULL, 1, 1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "fxquotient")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_QUOT, 0, 0, NULL, 1, -1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "remainder")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_REM, 0, 0, NULL, 1, 0, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "modulo")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_MOD, 0, 0, NULL, 1, 0, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-fxremainder")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_REM, 0, 0, NULL, 1, 1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-fxmodulo")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_MOD, 0, 0, NULL, 1, 1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "fxremainder")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_REM, 0, 0, NULL, 1, -1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "fxmodulo")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_MOD, 0, 0, NULL, 1, -1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "min")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_MIN, 0, 0, NULL, 1, 0, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "max")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_MAX, 0, 0, NULL, 1, 0, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-flmin")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_MIN, 0, 0, NULL, 1, 0, 1, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-flmax")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_MAX, 0, 0, NULL, 1, 0, 1, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "flmin")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_MIN, 0, 0, NULL, 1, 0, -1, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "flmax")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_MAX, 0, 0, NULL, 1, 0, -1, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-fxmin")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_MIN, 0, 0, NULL, 1, 1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-fxmax")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_MAX, 0, 0, NULL, 1, 1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "fxmin")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_MIN, 0, 0, NULL, 1, -1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "fxmax")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_MAX, 0, 0, NULL, 1, -1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "bitwise-and")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_AND, 0, 0, NULL, 1, 0, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-fxand")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_AND, 0, 0, NULL, 1, 1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "fxand")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_AND, 0, 0, NULL, 1, -1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "bitwise-ior")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_IOR, 0, 0, NULL, 1, 0, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-fxior")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_IOR, 0, 0, NULL, 1, 1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "fxior")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_IOR, 0, 0, NULL, 1, -1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "bitwise-xor")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_XOR, 0, 0, NULL, 1, 0, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-fxxor")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_XOR, 0, 0, NULL, 1, 1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "fxxor")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_XOR, 0, 0, NULL, 1, -1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "arithmetic-shift")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_LSH, 0, 0, NULL, 1, 0, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-fxlshift")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_LSH, 0, 0, NULL, 1, 1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "fxlshift")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_LSH, 0, 0, NULL, 1, -1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-fxrshift")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_RSH, 0, 0, NULL, 1, 1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "fxrshift")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_RSH, 0, 0, NULL, 1, -1, 0, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "flexpt")) {
      scheme_generate_arith(jitter, rator, app->rand1, app->rand2, 2, ARITH_EXPT, 0, 0, NULL, 1, 0, -1, NULL);
      return 1;
    } else if (IS_NAMED_PRIM(rator, "vector-ref")
               || IS_NAMED_PRIM(rator, "unsafe-vector-ref")
               || IS_NAMED_PRIM(rator, "unsafe-vector*-ref")
               || IS_NAMED_PRIM(rator, "unsafe-struct-ref")
               || IS_NAMED_PRIM(rator, "unsafe-struct*-ref")
	       || IS_NAMED_PRIM(rator, "string-ref")
               || IS_NAMED_PRIM(rator, "unsafe-string-ref")
	       || IS_NAMED_PRIM(rator, "bytes-ref")
	       || IS_NAMED_PRIM(rator, "unsafe-bytes-ref")
	       || IS_NAMED_PRIM(rator, "flvector-ref")
	       || IS_NAMED_PRIM(rator, "fxvector-ref")
	       || IS_NAMED_PRIM(rator, "unsafe-fxvector-ref")) {
      int simple;
      int which, unsafe = 0, base_offset = ((int)&SCHEME_VEC_ELS(0x0));
      int unbox = jitter->unbox;
      int can_chaperone = 1, for_struct = 0, for_fx = 0;

      if (IS_NAMED_PRIM(rator, "vector-ref")) {
        which = 0;
      } else if (IS_NAMED_PRIM(rator, "fxvector-ref")) {
	which = 0;
        for_fx = 1;
        can_chaperone = 0;
      } else if (IS_NAMED_PRIM(rator, "unsafe-vector*-ref")) {
	which = 0;
        unsafe = 1;
        can_chaperone = 0;
      } else if (IS_NAMED_PRIM(rator, "unsafe-fxvector-ref")) {
	which = 0;
        unsafe = 1;
        can_chaperone = 0;
        for_fx = 1;
      } else if (IS_NAMED_PRIM(rator, "unsafe-vector-ref")) {
	which = 0;
        unsafe = 1;
      } else if (IS_NAMED_PRIM(rator, "flvector-ref")) {
	which = 3;
        base_offset = ((int)&SCHEME_FLVEC_ELS(0x0));
        if (unbox) {
          if (jitter->unbox_depth) 
            scheme_signal_error("internal error: bad depth for flvector-ref");
          jitter->unbox = 0;
        }
        can_chaperone = 0;
      } else if (IS_NAMED_PRIM(rator, "unsafe-struct*-ref")) {
	which = 0;
        unsafe = 1;
        base_offset = ((int)&((Scheme_Structure *)0x0)->slots);
        can_chaperone = 0;
        for_struct = 1;
      } else if (IS_NAMED_PRIM(rator, "unsafe-struct-ref")) {
	which = 0;
        unsafe = 1;
        base_offset = ((int)&((Scheme_Structure *)0x0)->slots);
        for_struct = 1;
      } else if (IS_NAMED_PRIM(rator, "string-ref"))
	which = 1;
      else if (IS_NAMED_PRIM(rator, "unsafe-string-ref")) {
        which = 1;
        unsafe = 1;
      } else if (IS_NAMED_PRIM(rator, "unsafe-bytes-ref")) {
        which = 2;
        unsafe = 1;
      } else
	which = 2;

      LOG_IT(("inlined vector-/string-/bytes-ref\n"));

      simple = (SCHEME_INTP(app->rand2)
		&& (SCHEME_INT_VAL(app->rand2) >= 0));

      if (!simple) {
        generate_two_args(app->rand1, app->rand2, jitter, 1, 2);
        CHECK_LIMIT();

        if (!unsafe || can_chaperone)
          mz_rs_sync();

        if (!which) {
          /* vector-ref is relatively simple and worth inlining */
          if (can_chaperone) scheme_mz_need_space(jitter, 3);
          generate_vector_op(jitter, 0, 0, base_offset, 0, unsafe, 
                             0, 0, can_chaperone, for_struct, for_fx, 0);
          CHECK_LIMIT();
	} else if (which == 3) {
          /* flvector-ref is relatively simple and worth inlining */
          generate_vector_op(jitter, 0, 0, base_offset, 1, unsafe, 
                             unbox, 0, can_chaperone, for_struct, for_fx, 0);
          CHECK_LIMIT();
	} else if (which == 1) {
          if (unsafe) {
            jit_rshi_ul(JIT_R1, JIT_R1, 1);
            jit_lshi_ul(JIT_R1, JIT_R1, LOG_MZCHAR_SIZE);
            jit_ldxi_p(JIT_R0, JIT_R0, &SCHEME_CHAR_STR_VAL((Scheme_Object *)0x0));
            jit_ldxr_i(JIT_R0, JIT_R0, JIT_R1);
            (void)jit_movi_p(JIT_R1, scheme_char_constants);
            jit_lshi_ul(JIT_R0, JIT_R0, JIT_LOG_WORD_SIZE);
            jit_ldxr_p(JIT_R0, JIT_R1, JIT_R0);
            CHECK_LIMIT();
          } else {
            (void)jit_calli(sjc.string_ref_check_index_code);
          }
	} else {
          if (unsafe) {
            jit_rshi_ul(JIT_R1, JIT_R1, 1);
            jit_ldxi_p(JIT_R0, JIT_R0, &SCHEME_CHAR_STR_VAL((Scheme_Object *)0x0));
            jit_ldxr_c(JIT_R0, JIT_R0, JIT_R1);
            jit_extr_uc_ul(JIT_R0, JIT_R0);
	    jit_lshi_l(JIT_R0, JIT_R0, 0x1);
	    jit_ori_l(JIT_R0, JIT_R0, 0x1);
            CHECK_LIMIT();
          } else {
            (void)jit_calli(sjc.bytes_ref_check_index_code);
          }
	}
      } else {
	intptr_t offset;

        mz_runstack_skipped(jitter, 2);
      
        scheme_generate_non_tail(app->rand1, jitter, 0, 1, 0);
        CHECK_LIMIT();

        if (!unsafe || can_chaperone)
          mz_rs_sync();
      
	offset = SCHEME_INT_VAL(app->rand2);
        if (!unsafe || can_chaperone)
          (void)jit_movi_p(JIT_R1, offset);
	if (!which)
	  offset = base_offset + WORDS_TO_BYTES(offset);
	else if (which == 3)
	  offset = base_offset + (offset * sizeof(double));
	else if (which == 1)
	  offset = offset << LOG_MZCHAR_SIZE;
	jit_movi_l(JIT_V1, offset);
	if (!which) {
          /* vector-ref is relatively simple and worth inlining */
          if (can_chaperone) scheme_mz_need_space(jitter, 3);
          generate_vector_op(jitter, 0, 1, base_offset, 0, unsafe, 
                             0, 0, can_chaperone, for_struct, for_fx, 0);
          CHECK_LIMIT();
	} else if (which == 3) {
          /* flvector-ref is relatively simple and worth inlining */
          generate_vector_op(jitter, 0, 1, base_offset, 1, unsafe, 
                             unbox, 0, can_chaperone, for_struct, for_fx, 0);
          CHECK_LIMIT();
	} else if (which == 1) {
          if (unsafe) {
            jit_ldxi_p(JIT_R0, JIT_R0, &SCHEME_CHAR_STR_VAL((Scheme_Object *)0x0));
            jit_ldxr_i(JIT_R0, JIT_R0, JIT_V1);
            (void)jit_movi_p(JIT_R1, scheme_char_constants);
            jit_lshi_ul(JIT_R0, JIT_R0, JIT_LOG_WORD_SIZE);
            jit_ldxr_p(JIT_R0, JIT_R1, JIT_R0);
            CHECK_LIMIT();
          } else {
            (void)jit_calli(sjc.string_ref_code);
          }
	} else {
          if (unsafe) {
            jit_ldxi_p(JIT_R0, JIT_R0, &SCHEME_CHAR_STR_VAL((Scheme_Object *)0x0));
            jit_ldxr_c(JIT_R0, JIT_R0, JIT_V1);
            jit_extr_uc_ul(JIT_R0, JIT_R0);
	    jit_lshi_l(JIT_R0, JIT_R0, 0x1);
	    jit_ori_l(JIT_R0, JIT_R0, 0x1);
          } else {
            (void)jit_calli(sjc.bytes_ref_code);
          }
	}

        mz_runstack_unskipped(jitter, 2);
      }

      if (unbox) jitter->unbox = unbox;

      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-f64vector-ref")
               || IS_NAMED_PRIM(rator, "unsafe-flvector-ref")) {
      int fpr0, unbox = jitter->unbox;
      int is_f64;

      is_f64 = IS_NAMED_PRIM(rator, "unsafe-f64vector-ref");
      
      jitter->unbox = 0; /* no unboxing of vector and index arguments */
      generate_two_args(app->rand1, app->rand2, jitter, 1, 2);
      jitter->unbox = unbox;
      CHECK_LIMIT();

      if (is_f64) {
        jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&(((Scheme_Structure *)0x0)->slots[0]));
        jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&SCHEME_CPTR_VAL(0x0));
      }
      jit_rshi_ul(JIT_R1, JIT_R1, 1);
      jit_lshi_ul(JIT_R1, JIT_R1, JIT_LOG_DOUBLE_SIZE);
      if (!is_f64) {
        jit_addi_ul(JIT_R1, JIT_R1, (int)(&SCHEME_FLVEC_ELS(0x0)));
      }

      if (jitter->unbox)
        fpr0 = JIT_FPR_0(jitter->unbox_depth);
      else
        fpr0 = JIT_FPR0;

      jit_ldxr_d_fppush(fpr0, JIT_R0, JIT_R1);
      CHECK_LIMIT();

      if (jitter->unbox)
        jitter->unbox_depth++;
      else {
        mz_rs_sync();
        scheme_generate_alloc_double(jitter, 0);
      }

      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-s16vector-ref")
               || IS_NAMED_PRIM(rator, "unsafe-u16vector-ref")) {
      int is_u;

      is_u = IS_NAMED_PRIM(rator, "unsafe-u16vector-ref");
      
      generate_two_args(app->rand1, app->rand2, jitter, 1, 2);

      jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&(((Scheme_Structure *)0x0)->slots[0]));
      jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&SCHEME_CPTR_VAL(0x0));
      jit_subi_l(JIT_R1, JIT_R1, 1);

      if (is_u)
        jit_ldxr_us(JIT_R0, JIT_R0, JIT_R1);
      else
        jit_ldxr_s(JIT_R0, JIT_R0, JIT_R1);

      jit_lshi_l(JIT_R0, JIT_R0, 0x1);
      jit_ori_l(JIT_R0, JIT_R0, 0x1);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "list-ref")
               || IS_NAMED_PRIM(rator, "list-tail")) {
      generate_two_args(app->rand1, app->rand2, jitter, 1, 2);

      mz_rs_sync();
      if (IS_NAMED_PRIM(rator, "list-ref"))
        (void)jit_calli(sjc.list_ref_code);
      else
        (void)jit_calli(sjc.list_tail_code);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-list-ref")
               || IS_NAMED_PRIM(rator, "unsafe-list-tail")) {
      if (SCHEME_INTP(app->rand2)) {
        intptr_t v = SCHEME_INT_VAL(app->rand2);
        if ((v >= 0) && (v <= 10)) {
          mz_runstack_skipped(jitter, 2);
          scheme_generate_non_tail(app->rand1, jitter, 0, 1, 0);
          CHECK_LIMIT();
          mz_runstack_unskipped(jitter, 2);

          while (v--) {
            jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&SCHEME_CDR(0x0));
          }
          if (IS_NAMED_PRIM(rator, "unsafe-list-ref"))
            jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&SCHEME_CAR(0x0));

          return 1;
        }
      }

      generate_two_args(app->rand1, app->rand2, jitter, 1, 2);

      if (IS_NAMED_PRIM(rator, "unsafe-list-ref"))
        (void)jit_calli(sjc.list_ref_code);
      else
        (void)jit_calli(sjc.list_tail_code);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "set-mcar!")
               || IS_NAMED_PRIM(rator, "set-mcdr!")) {
      GC_CAN_IGNORE jit_insn *reffail, *ref;
      int set_mcar;

      set_mcar = IS_NAMED_PRIM(rator, "set-mcar!");

      LOG_IT(("inlined set-mcar!\n"));

      generate_two_args(app->rand1, app->rand2, jitter, 1, 2);
      CHECK_LIMIT();
      mz_rs_sync_fail_branch();

      __START_TINY_JUMPS__(1);
      ref = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);
      reffail = _jit.x.pc;
      __END_TINY_JUMPS__(1);
      if (set_mcar)
        (void)jit_calli(sjc.bad_set_mcar_code);
      else
        (void)jit_calli(sjc.bad_set_mcdr_code);
      __START_TINY_JUMPS__(1);
      mz_patch_branch(ref);
      (void)mz_bnei_t(reffail, JIT_R0, scheme_mutable_pair_type, JIT_R2);
      __END_TINY_JUMPS__(1);
      CHECK_LIMIT();

      if (set_mcar)
        (void)jit_stxi_p(&((Scheme_Simple_Object *)0x0)->u.pair_val.car, JIT_R0, JIT_R1);
      else
        (void)jit_stxi_p(&((Scheme_Simple_Object *)0x0)->u.pair_val.cdr, JIT_R0, JIT_R1);
      
      if (!result_ignored)
        (void)jit_movi_p(JIT_R0, scheme_void);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-set-mcar!")
               || IS_NAMED_PRIM(rator, "unsafe-set-mcdr!")) {
      int set_mcar;

      set_mcar = IS_NAMED_PRIM(rator, "unsafe-set-mcar!");

      LOG_IT(("inlined unsafe-set-mcar!\n"));

      generate_two_args(app->rand1, app->rand2, jitter, 1, 2);
      CHECK_LIMIT();
      if (set_mcar)
        (void)jit_stxi_p(&((Scheme_Simple_Object *)0x0)->u.pair_val.car, JIT_R0, JIT_R1);
      else
        (void)jit_stxi_p(&((Scheme_Simple_Object *)0x0)->u.pair_val.cdr, JIT_R0, JIT_R1);
      
      if (!result_ignored)
        (void)jit_movi_p(JIT_R0, scheme_void);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "set-box!")
               || IS_NAMED_PRIM(rator, "unsafe-set-box!")) {
      GC_CAN_IGNORE jit_insn *ref, *ref2, *ref3, *reffail;
      int unsafe;

      LOG_IT(("inlined set-box!\n"));

      unsafe = IS_NAMED_PRIM(rator, "unsafe-set-box!");

      generate_two_args(app->rand1, app->rand2, jitter, 1, 2);
      CHECK_LIMIT();
      mz_rs_sync();
      __START_TINY_JUMPS__(1);
      if (!unsafe)
        ref3 = jit_bmsi_ul(jit_forward(), JIT_R0, 0x1);
      else
        ref3 = NULL;
      ref = mz_beqi_t(jit_forward(), JIT_R0, scheme_box_type, JIT_R2);
      if (ref3)
        mz_patch_branch(ref3);
      reffail = _jit.x.pc;
      (void)jit_calli(sjc.set_box_code);
      ref2 = jit_jmpi(jit_forward());
      mz_patch_branch(ref);
      if (!unsafe) {
        jit_ldxi_s(JIT_R2, JIT_R0, &MZ_OPT_HASH_KEY((Scheme_Inclhash_Object *)0x0));
        (void)jit_bmsi_ul(reffail, JIT_R2, 0x1);
      }
      __END_TINY_JUMPS__(1);

      (void)jit_stxi_p(&SCHEME_BOX_VAL(0x0), JIT_R0, JIT_R1);

      __START_TINY_JUMPS__(1);
      mz_patch_ucbranch(ref2);
      __END_TINY_JUMPS__(1);
      
      if (!result_ignored)
        (void)jit_movi_p(JIT_R0, scheme_void);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-set-box*!")) {
      LOG_IT(("inlined unsafe-set-box*!\n"));

      generate_two_args(app->rand1, app->rand2, jitter, 1, 2);
      CHECK_LIMIT();
      (void)jit_stxi_p(&SCHEME_BOX_VAL(0x0), JIT_R0, JIT_R1);
      
      if (!result_ignored)
        (void)jit_movi_p(JIT_R0, scheme_void);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "cons")
               || IS_NAMED_PRIM(rator, "list*")) {
      LOG_IT(("inlined cons\n"));

      generate_two_args(app->rand1, app->rand2, jitter, 1, 2);
      CHECK_LIMIT();
      mz_rs_sync();

      return scheme_generate_cons_alloc(jitter, 0, 0);
    } else if (IS_NAMED_PRIM(rator, "mcons")) {
      LOG_IT(("inlined mcons\n"));

      generate_two_args(app->rand1, app->rand2, jitter, 1, 2);
      CHECK_LIMIT();
      mz_rs_sync();

#ifdef CAN_INLINE_ALLOC
      /* Inlined alloc */
      scheme_inline_alloc(jitter, sizeof(Scheme_Simple_Object), scheme_mutable_pair_type, 0, 1, 0, 0);
      CHECK_LIMIT();

      jit_stxi_p((intptr_t)&SCHEME_MCAR(0x0) + OBJHEAD_SIZE, JIT_V1, JIT_R0);
      jit_stxi_p((intptr_t)&SCHEME_MCDR(0x0) + OBJHEAD_SIZE, JIT_V1, JIT_R1);
      jit_addi_p(JIT_R0, JIT_V1, OBJHEAD_SIZE);
#else
      /* Non-inlined alloc */
      JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
      mz_prepare(2);
      jit_pusharg_p(JIT_R1);
      jit_pusharg_p(JIT_R0);
      {
        GC_CAN_IGNORE jit_insn *refr;
        (void)mz_finish_lwe(ts_scheme_make_mutable_pair, refr);
      }
      jit_retval(JIT_R0);
#endif

      return 1;
    } else if (IS_NAMED_PRIM(rator, "list")) {
      LOG_IT(("inlined list\n"));

      generate_two_args(app->rand1, app->rand2, jitter, 1, 2);
      CHECK_LIMIT();

      mz_rs_dec(1);
      CHECK_RUNSTACK_OVERFLOW();
      mz_runstack_pushed(jitter, 1);
      mz_rs_str(JIT_R0);
      (void)jit_movi_p(JIT_R0, &scheme_null);
      CHECK_LIMIT();
      mz_rs_sync();

      scheme_generate_cons_alloc(jitter, 1, 0);
      CHECK_LIMIT();

      jit_ldr_p(JIT_R1, JIT_RUNSTACK);
      jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
      mz_runstack_popped(jitter, 1);
      CHECK_LIMIT();
      
      return scheme_generate_cons_alloc(jitter, 1, 0);
    } else if (IS_NAMED_PRIM(rator, "vector-immutable")
               || IS_NAMED_PRIM(rator, "vector")) {
      return generate_vector_alloc(jitter, rator, NULL, NULL, app);
    } else if (IS_NAMED_PRIM(rator, "make-rectangular")) {
      GC_CAN_IGNORE jit_insn *ref, *ref2, *ref3, *ref4, *refslow, *refdone;

      LOG_IT(("inlined make-rectangular\n"));

      generate_two_args(app->rand1, app->rand2, jitter, 1, 2);
      CHECK_LIMIT();
      mz_rs_sync();

      jit_movi_i(JIT_V1, 0); /* V1 as 0 => exact first argument */

      __START_SHORT_JUMPS__(1);
      /* Check first arg: */
      ref = jit_bmsi_ul(jit_forward(), JIT_R0, 0x1);
      jit_ldxi_s(JIT_R2, JIT_R0, &((Scheme_Object *)0x0)->type);
      ref2 = jit_bgei_i(jit_forward(), JIT_R2, scheme_bignum_type);
      /* (slow path) */
      refslow = _jit.x.pc;
      (void)jit_calli(sjc.make_rectangular_code);
      jit_retval(JIT_R0);
      CHECK_LIMIT();
      refdone = jit_jmpi(jit_forward());
      /* (end of slow path) */
      mz_patch_branch(ref2);
      (void)jit_bgei_i(refslow, JIT_R2, scheme_complex_type);
      /* set V1 if inexact */
      ref3 = jit_blti_i(jit_forward(), JIT_R2, scheme_float_type);
      jit_movi_i(JIT_V1, 1);
      mz_patch_branch(ref3);
      mz_patch_branch(ref);
      CHECK_LIMIT();

      /* Check second arg: */
      ref = jit_bmsi_ul(jit_forward(), JIT_R1, 0x1);
      jit_ldxi_s(JIT_R2, JIT_R1, &((Scheme_Object *)0x0)->type);
      (void)jit_blti_i(refslow, JIT_R2, scheme_bignum_type);
      (void)jit_bgei_i(refslow, JIT_R2, scheme_complex_type);
      ref3 = jit_blti_i(jit_forward(), JIT_R2, scheme_float_type);
      (void)jit_bnei_i(refslow, JIT_V1, 1); /* need to coerce other to inexact */
      ref4 = jit_jmpi(jit_forward());
      mz_patch_branch(ref3);
      mz_patch_branch(ref);
      (void)jit_bnei_i(refslow, JIT_V1, 0); /* need to coerce to inexact */
      /* exact zero => result is real */
      (void)jit_beqi_p(refslow, JIT_R1, scheme_make_integer(0));
      CHECK_LIMIT();
      mz_patch_ucbranch(ref4);

      __END_SHORT_JUMPS__(1);

      allocate_rectangular(jitter);

      __START_SHORT_JUMPS__(1);
      mz_patch_ucbranch(refdone);
      __END_SHORT_JUMPS__(1);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "make-flrectangular")) {
      GC_CAN_IGNORE jit_insn *ref, *refslow;
      
      LOG_IT(("inlined make-rectangular\n"));

      generate_two_args(app->rand1, app->rand2, jitter, 1, 2);
      CHECK_LIMIT();
      mz_rs_sync();

      __START_TINY_JUMPS__(1);
      ref = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);
      refslow = _jit.x.pc;
      (void)jit_calli(sjc.bad_make_flrectangular_code);
      mz_patch_branch(ref);
      (void)mz_bnei_t(refslow, JIT_R0, scheme_double_type, JIT_R2);
      (void)jit_bmsi_ul(refslow, JIT_R1, 0x1);
      (void)mz_bnei_t(refslow, JIT_R1, scheme_double_type, JIT_R2);
      __END_TINY_JUMPS__(1);
      CHECK_LIMIT();

      allocate_rectangular(jitter);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-make-flrectangular")) {
      LOG_IT(("inlined make-rectangular\n"));

      generate_two_args(app->rand1, app->rand2, jitter, 1, 2);
      CHECK_LIMIT();
      
      allocate_rectangular(jitter);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "procedure-arity-includes?")) {
      LOG_IT(("inlined procedure-arity-includes?\n"));

      generate_two_args(app->rand1, app->rand2, jitter, 1, 2);
      CHECK_LIMIT();

      mz_rs_sync();
      (void)jit_calli(sjc.proc_arity_includes_code);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "values")) {
      Scheme_Object *args[3];

      if (!multi_ok) return 0;

      args[0] = rator;
      args[1] = app->rand1;
      args[2] = app->rand2;

      scheme_generate_app(NULL, args, 2, jitter, 0, 0, 2);

      CHECK_LIMIT();
      mz_rs_sync();

      jit_movi_l(JIT_V1, 2);
      (void)jit_calli(sjc.values_code);

      mz_rs_inc(2); /* no sync */
      mz_runstack_popped(jitter, 2);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "continuation-mark-set-first")) {
      GC_CAN_IGNORE jit_insn *refr;

      LOG_IT(("inlined continuation-mark-set-first\n"));

      generate_two_args(app->rand1, app->rand2, jitter, 1, 2);
      CHECK_LIMIT();
      /* R0 has the first argument, R1 has the second argument */

      mz_rs_sync();
      JIT_UPDATE_THREAD_RSPTR();

      jit_prepare(2);
      jit_pusharg_p(JIT_R1);
      jit_pusharg_p(JIT_R0);
      mz_finish_prim_lwe(cont_mark_set_first_try_fast, refr);
      jit_retval(JIT_R0);
      CHECK_LIMIT();

      return 1;
    }
  }

  if (!for_branch) {
    scheme_console_printf("Inlining expected.\n");
    abort();
  }

  --scheme_direct_call_count;

  return 0;
}

int scheme_generate_inlined_nary(mz_jit_state *jitter, Scheme_App_Rec *app, int is_tail, int multi_ok, 
                                 Branch_Info *for_branch, int branch_short, int result_ignored)
/* de-sync's; for branch, sync'd before */
{
  Scheme_Object *rator = app->args[0];
  
  if (!SCHEME_PRIMP(rator))
    return 0;

  if (!(SCHEME_PRIM_PROC_FLAGS(rator) & SCHEME_PRIM_IS_NARY_INLINED))
    return 0;

  if (app->num_args < ((Scheme_Primitive_Proc *)rator)->mina)
    return 0;
  if (app->num_args > ((Scheme_Primitive_Proc *)rator)->mu.maxa)
    return 0;

  scheme_direct_call_count++;

  if (IS_NAMED_PRIM(rator, "=")) {
    scheme_generate_nary_arith(jitter, app, 0, CMP_EQUAL, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "<")) {
    scheme_generate_nary_arith(jitter, app, 0, CMP_LT, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, ">")) {
    scheme_generate_nary_arith(jitter, app, 0, CMP_GT, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "<=")) {
    scheme_generate_nary_arith(jitter, app, 0, CMP_LEQ, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, ">=")) {
    scheme_generate_nary_arith(jitter, app, 0, CMP_GEQ, for_branch, branch_short);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "current-future")) { 
    mz_rs_sync();
    JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
    mz_prepare(0);
    (void)mz_finish(scheme_current_future);
    jit_retval(JIT_R0);
    return 1;
  } else if (IS_NAMED_PRIM(rator, "box-cas!") || (IS_NAMED_PRIM(rator, "unsafe-box*-cas!"))) { 

    GC_CAN_IGNORE jit_insn *ref, *reffail, *reffalse, *reftrue;
    int unsafe = 0;

    if (IS_NAMED_PRIM(rator, "unsafe-box*-cas!")) {
      unsafe = 1;
    }

    /* generate code to evaluate the arguments */
    scheme_generate_app(app, NULL, 3, jitter, 0, 0, 2);
    CHECK_LIMIT();
    mz_rs_sync();

    mz_rs_ldr(JIT_R1);

    if (!unsafe) {
      __START_TINY_JUMPS__(1);
      /* Fail if this isn't a pointer (0x1 is the integer tag) */
      ref = jit_bmci_ul(jit_forward(), JIT_R1, 0x1);
      reffail = _jit.x.pc;
      __END_TINY_JUMPS__(1);

      (void)jit_calli(sjc.box_cas_fail_code);

      __START_TINY_JUMPS__(1);
      /* jump to here if the type tag tests succeed */
      mz_patch_branch(ref);

      /* Get the type tag, fail if it isn't a box */
      (void)mz_bnei_t(reffail, JIT_R1, scheme_box_type, JIT_R2);
      /* fail if immutable: */
      jit_ldxi_s(JIT_R2, JIT_R1, &MZ_OPT_HASH_KEY((Scheme_Inclhash_Object *)0x0));
      (void)jit_bmsi_ul(reffail, JIT_R2, 0x1);
      __END_TINY_JUMPS__(1);
    }
    CHECK_LIMIT();

    /* box is in JIT_R1 */
    jit_addi_l(JIT_R1, JIT_R1, (intptr_t)&SCHEME_BOX_VAL(0x0));
    mz_rs_ldxi(JIT_R0, 1); /* old val */
    mz_rs_ldxi(JIT_V1, 2); /* new val */

    /* pop off 3 arguments */
    mz_rs_inc(3);
    mz_runstack_popped(jitter, 3);

    if (for_branch) {
      __START_SHORT_JUMPS__(branch_short);
      scheme_prepare_branch_jump(jitter, for_branch);
      CHECK_LIMIT();
    } else {
      __START_TINY_JUMPS__(1);
    }

    /* This is the actual CAS: */
#ifdef MZ_USE_FUTURES
    if (scheme_is_multiprocessor(0)) {
      jit_lock_cmpxchgr_l(JIT_R1, JIT_V1); /* implicitly uses JIT_R0 */
      reffalse = (JNEm(jit_forward(), 0,0,0), _jit.x.pc);
    } else
#endif
      {
        jit_ldr_p(JIT_R2, JIT_R1);
        reffalse = jit_bner_p(jit_forward(), JIT_R2, JIT_R0);
        jit_str_p(JIT_R1, JIT_V1);
      }

    /* Branch or set true/false: */
    if (for_branch) {
      scheme_branch_for_true(jitter, for_branch);
      scheme_add_branch_false(for_branch, reffalse);
      __END_SHORT_JUMPS__(branch_short);
    } else {
      jit_movi_p(JIT_R0, scheme_true);
      reftrue = jit_jmpi(jit_forward());
        
      mz_patch_branch(reffalse);
      jit_movi_p(JIT_R0, scheme_false);
        
      mz_patch_branch(reftrue);
      __END_TINY_JUMPS__(1);
    }

    return 1;
  } else if (!for_branch) {
    if (IS_NAMED_PRIM(rator, "vector-set!")
        || IS_NAMED_PRIM(rator, "unsafe-vector-set!")
        || IS_NAMED_PRIM(rator, "unsafe-vector*-set!")
        || IS_NAMED_PRIM(rator, "flvector-set!")
        || IS_NAMED_PRIM(rator, "fxvector-set!")
        || IS_NAMED_PRIM(rator, "unsafe-fxvector-set!")
        || IS_NAMED_PRIM(rator, "unsafe-struct-set!")
        || IS_NAMED_PRIM(rator, "unsafe-struct*-set!")
	|| IS_NAMED_PRIM(rator, "string-set!")
	|| IS_NAMED_PRIM(rator, "unsafe-string-set!")
	|| IS_NAMED_PRIM(rator, "bytes-set!")
	|| IS_NAMED_PRIM(rator, "unsafe-bytes-set!")
	|| IS_NAMED_PRIM(rator, "unsafe-s16vector-set!")
	|| IS_NAMED_PRIM(rator, "unsafe-u16vector-set!")) {
      int simple, constval, can_delay_vec, can_delay_index;
      int which, unsafe = 0, base_offset = ((int)&SCHEME_VEC_ELS(0x0));
      int pushed, flonum_arg;
      int can_chaperone = 1, for_struct = 0, for_fx = 0, check_mutable = 0;

      if (IS_NAMED_PRIM(rator, "vector-set!")) {
	which = 0;
        check_mutable = 1;
      } else if (IS_NAMED_PRIM(rator, "fxvector-set!")) {
	which = 0;
        for_fx = 1;
      } else if (IS_NAMED_PRIM(rator, "unsafe-vector*-set!")) {
        which = 0;
        unsafe = 1;
        can_chaperone = 0;
      } else if (IS_NAMED_PRIM(rator, "unsafe-fxvector-set!")) {
        which = 0;
        unsafe = 1;
        can_chaperone = 0;
        for_fx = 1;
      } else if (IS_NAMED_PRIM(rator, "unsafe-vector-set!")) {
        which = 0;
        unsafe = 1;
      } else if (IS_NAMED_PRIM(rator, "flvector-set!")) {
	which = 3;
        base_offset = ((int)&SCHEME_FLVEC_ELS(0x0));
      } else if (IS_NAMED_PRIM(rator, "unsafe-struct*-set!")) {
        which = 0;
        unsafe = 1;
        base_offset = ((int)&((Scheme_Structure *)0x0)->slots);
        can_chaperone = 0;
        for_struct = 1;
      } else if (IS_NAMED_PRIM(rator, "unsafe-struct-set!")) {
        which = 0;
        unsafe = 1;
        base_offset = ((int)&((Scheme_Structure *)0x0)->slots);
        for_struct = 1;
      } else if (IS_NAMED_PRIM(rator, "string-set!"))
	which = 1;
      else if (IS_NAMED_PRIM(rator, "unsafe-string-set!")) {
	which = 1;
        unsafe = 1;
        can_chaperone = 0;
      } else if (IS_NAMED_PRIM(rator, "unsafe-bytes-set!")) {
        which = 2;
        unsafe = 1;
        can_chaperone = 0;
      } else if (IS_NAMED_PRIM(rator, "unsafe-s16vector-set!")) {
        which = 4;
        unsafe = 1;
        can_chaperone = 0;
      } else if (IS_NAMED_PRIM(rator, "unsafe-u16vector-set!")) {
        which = 5;
        unsafe = 1;
        can_chaperone = 0;
      } else
	which = 2;

      LOG_IT(("inlined vector-set!\n"));

      if (scheme_can_delay_and_avoids_r1_r2(app->args[1]))
        can_delay_vec = 1;
      else
        can_delay_vec = 0;

      simple = (SCHEME_INTP(app->args[2])
		&& (SCHEME_INT_VAL(app->args[2]) >= 0));
      if (simple || scheme_can_delay_and_avoids_r1(app->args[2]))
        can_delay_index = 1;
      else
        can_delay_index = 0;

      constval = scheme_can_delay_and_avoids_r1(app->args[3]);

      if (which == 3) {
        if (scheme_can_unbox_inline(app->args[3], 5, JIT_FPR_NUM-3, 0))
          flonum_arg = 2;
        else if (scheme_can_unbox_directly(app->args[3]))
          flonum_arg = 1;
        else
          flonum_arg = 0;
      } else
        flonum_arg = 0;
# if !defined(INLINE_FP_OPS) || !defined(CAN_INLINE_ALLOC)
      /* Error handling will have to box flonum, so don't unbox if
         that cannot be done inline: */
      if (flonum_arg && !unsafe)
        flonum_arg = 0;
# endif
      
      if (can_delay_vec && can_delay_index)
        pushed = 0;
      else if (constval && can_delay_index)
	pushed = 0;
      else if (constval && can_delay_vec)
	pushed = 0;
      else if (!can_delay_vec && !can_delay_index && !constval)
	pushed = 2;
      else
        pushed = 1;

      if (!pushed && !flonum_arg && (!unsafe || can_chaperone))
        pushed = 1; /* need temporary space */
      
      mz_runstack_skipped(jitter, 3 - pushed);

      if (pushed) {
        mz_rs_dec(pushed);
        CHECK_RUNSTACK_OVERFLOW();
	mz_runstack_pushed(jitter, pushed);
        scheme_stack_safety(jitter, pushed, 0);
        CHECK_LIMIT();
      }
      
      if (!can_delay_vec) {
        scheme_generate_non_tail(app->args[1], jitter, 0, 1, 0); /* sync'd below */
        CHECK_LIMIT();
        if (!constval || !can_delay_index) {
          mz_rs_str(JIT_R0);
        } else {
          jit_movr_p(JIT_V1, JIT_R0);
        }
      }

      if (!can_delay_index) {
	scheme_generate_non_tail(app->args[2], jitter, 0, 1, 0); /* sync'd below */
	CHECK_LIMIT();
	if (!constval) {
          if (can_delay_vec)
            mz_rs_str(JIT_R0);
          else
            mz_rs_stxi(1, JIT_R0);
	} else {
	  jit_movr_p(JIT_R1, JIT_R0);
	}
      }

      if (flonum_arg) {
        jitter->unbox++;
        scheme_generate_unboxed(app->args[3], jitter, flonum_arg, 0);
        --jitter->unbox;
      } else {
        if (constval)
          scheme_generate(app->args[3], jitter, 0, 0, 0, JIT_R2, NULL); /* sync'd below */
        else {
          scheme_generate_non_tail(app->args[3], jitter, 0, 1, 0); /* sync'd below */
          jit_movr_p(JIT_R2, JIT_R0);
        }
      }
      CHECK_LIMIT();

      /* At this point, value is in R2, vec is uncomputed or in V1,
         and index is uncomputed or in R1.
         Need to get vec into R0, non-simple index into R1, value into R2. */

      if (can_delay_vec) {
        scheme_generate(app->args[1], jitter, 0, 0, 0, JIT_R0, NULL); /* sync'd below */
        CHECK_LIMIT();
      } else if (can_delay_index && constval) {
        jit_movr_p(JIT_R0, JIT_V1);
      } else {
        mz_rs_ldr(JIT_R0);
      }

      if (!simple) {
        if (can_delay_index) {
          scheme_generate(app->args[2], jitter, 0, 0, 0, JIT_R1, NULL); /* sync'd below */
          CHECK_LIMIT();
        } else if (!constval) {
          if (can_delay_vec)
            mz_rs_ldr(JIT_R1);
          else
            mz_rs_ldxi(JIT_R1, 1);
        }
      }

      /* All pieces are in place */

      if (!unsafe || can_chaperone)
        mz_rs_sync();

      if (!simple) {
	if (!which) {
          /* vector-set! is relatively simple and worth inlining */
          if (can_chaperone) scheme_mz_need_space(jitter, 3);
          generate_vector_op(jitter, 1, 0, base_offset, 0, unsafe, 
                             flonum_arg, result_ignored, can_chaperone, 
                             for_struct, for_fx, check_mutable);
          CHECK_LIMIT();
	} else if (which == 3) {
          /* flvector-set! is relatively simple and worth inlining */
          generate_vector_op(jitter, 1, 0, base_offset, 1, unsafe, 
                             flonum_arg, result_ignored, can_chaperone, 
                             for_struct, for_fx, 0);
          CHECK_LIMIT();
        } else if ((which == 4) || (which == 5)) {
          /* unsafe-{s,u}16vector-set! */
          jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&(((Scheme_Structure *)0x0)->slots[0]));
          jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&SCHEME_CPTR_VAL(0x0));
          jit_subi_l(JIT_R1, JIT_R1, 1);
          jit_rshi_ul(JIT_R2, JIT_R2, 1);
          if (which == 5)
            jit_stxr_us(JIT_R1, JIT_R0, JIT_R2);
          else
            jit_stxr_s(JIT_R1, JIT_R0, JIT_R2);
          CHECK_LIMIT();
          if (!result_ignored)
            (void)jit_movi_p(JIT_R0, scheme_void);          
	} else if (which == 1) {
          if (unsafe) {
            jit_rshi_ul(JIT_R1, JIT_R1, 1);
            jit_lshi_ul(JIT_R1, JIT_R1, LOG_MZCHAR_SIZE);
            jit_ldxi_p(JIT_R0, JIT_R0, &SCHEME_CHAR_STR_VAL((Scheme_Object *)0x0));
            jit_ldxi_i(JIT_R2, JIT_R2, &((Scheme_Small_Object *)0x0)->u.char_val);
            jit_stxr_i(JIT_R1, JIT_R0, JIT_R2);
            if (!result_ignored)
              (void)jit_movi_p(JIT_R0, scheme_void);
          } else {
            mz_rs_str(JIT_R2);
            (void)jit_calli(sjc.string_set_check_index_code);
          }
	} else {
          if (unsafe) {
            jit_rshi_ul(JIT_R1, JIT_R1, 1);
            jit_ldxi_p(JIT_R0, JIT_R0, &SCHEME_BYTE_STR_VAL((Scheme_Object *)0x0));
            jit_rshi_ul(JIT_R2, JIT_R2, 1);
            jit_stxr_c(JIT_R1, JIT_R0, JIT_R2);
            if (!result_ignored)
              (void)jit_movi_p(JIT_R0, scheme_void);
          } else {
            mz_rs_str(JIT_R2);
            (void)jit_calli(sjc.bytes_set_check_index_code);
          }
	}
      } else {
	intptr_t offset;
	offset = SCHEME_INT_VAL(app->args[2]);
	(void)jit_movi_p(JIT_R1, offset);
	if (!which)
	  offset = base_offset + WORDS_TO_BYTES(offset);
	else if (which == 3)
	  offset = base_offset + (offset * sizeof(double));
	else if (which == 1)
	  offset = offset << LOG_MZCHAR_SIZE;
        else if ((which == 4) || (which == 5))
          offset *= 2;
	jit_movi_l(JIT_V1, offset);
	if (!which) {
          /* vector-set! is relatively simple and worth inlining */
          if (can_chaperone) scheme_mz_need_space(jitter, 3);
          generate_vector_op(jitter, 1, 1, base_offset, 0, unsafe, 
                             flonum_arg, result_ignored, can_chaperone, 
                             for_struct, for_fx, check_mutable);
          CHECK_LIMIT();
        } else if ((which == 4) || (which == 5)) {
          /* unsafe-{s,u}16vector-set! */
          jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&(((Scheme_Structure *)0x0)->slots[0]));
          jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&SCHEME_CPTR_VAL(0x0));
          jit_rshi_ul(JIT_R2, JIT_R2, 1);
          if (which == 5)
            jit_stxi_us(offset, JIT_R0, JIT_R2);
          else
            jit_stxi_s(offset, JIT_R0, JIT_R2);
          CHECK_LIMIT();
          if (!result_ignored)
            (void)jit_movi_p(JIT_R0, scheme_void);          
	} else if (which == 3) {
          /* flvector-set! is relatively simple and worth inlining */
          generate_vector_op(jitter, 1, 1, base_offset, 1, unsafe, 
                             flonum_arg, result_ignored, can_chaperone, 
                             for_struct, for_fx, 0);
          CHECK_LIMIT();
	} else if (which == 1) {
          if (unsafe) {
            jit_ldxi_p(JIT_R0, JIT_R0, &SCHEME_CHAR_STR_VAL((Scheme_Object *)0x0));
            jit_ldxi_i(JIT_R2, JIT_R2, &((Scheme_Small_Object *)0x0)->u.char_val);
            jit_stxr_i(JIT_V1, JIT_R0, JIT_R2);
            if (!result_ignored)
              (void)jit_movi_p(JIT_R0, scheme_void);
          } else {
            mz_rs_str(JIT_R2);
            (void)jit_calli(sjc.string_set_code);
          }
	} else {
          if (unsafe) {
            jit_ldxi_p(JIT_R0, JIT_R0, &SCHEME_CHAR_STR_VAL((Scheme_Object *)0x0));
            jit_rshi_ul(JIT_R2, JIT_R2, 1);
            jit_stxr_c(JIT_V1, JIT_R0, JIT_R2);
            if (!result_ignored)
              (void)jit_movi_p(JIT_R0, scheme_void);
          } else {
            mz_rs_str(JIT_R2);
            (void)jit_calli(sjc.bytes_set_code);
          }
	}
      }

      if (pushed) {
        mz_rs_inc(pushed); /* no sync */
        mz_runstack_popped(jitter, pushed);
      }

      mz_runstack_unskipped(jitter, 3 - pushed);

      return 1;
    } else if (IS_NAMED_PRIM(rator, "unsafe-f64vector-set!")
               || IS_NAMED_PRIM(rator, "unsafe-flvector-set!")) {
      int is_f64;
      int can_direct, got_two;

      is_f64 = IS_NAMED_PRIM(rator, "unsafe-f64vector-set!");

      if (scheme_is_constant_and_avoids_r1(app->args[1])
          && scheme_is_constant_and_avoids_r1(app->args[2])) {
        mz_runstack_skipped(jitter, 3);
        got_two = 0;
      } else {
        got_two = 1;
        mz_runstack_skipped(jitter, 1);
        scheme_generate_app(app, NULL, 2, jitter, 0, 0, 2);
      }

      if (scheme_can_unbox_inline(app->args[3], 5, JIT_FPR_NUM-1, 1))
        can_direct = 2;
      else if (scheme_can_unbox_directly(app->args[3]))
        can_direct = 1;
      else
        can_direct = 0;

      jitter->unbox++;
      scheme_generate_unboxed(app->args[3], jitter, can_direct, 1);
      --jitter->unbox;
      --jitter->unbox_depth;
      CHECK_LIMIT();
      
      if (!got_two) {
        scheme_generate(app->args[2], jitter, 0, 0, 0, JIT_R1, NULL);
        CHECK_LIMIT();
        scheme_generate(app->args[1], jitter, 0, 0, 0, JIT_R0, NULL);
        mz_runstack_unskipped(jitter, 3);
      } else {
        mz_rs_ldr(JIT_R0);
        mz_rs_ldxi(JIT_R1, 1);
        mz_rs_inc(2); /* no sync */
        mz_runstack_popped(jitter, 2);
        mz_runstack_unskipped(jitter, 1);
      }
      CHECK_LIMIT();

      if (is_f64) {
        jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&(((Scheme_Structure *)0x0)->slots[0]));
        jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&SCHEME_CPTR_VAL(0x0));
      }
      jit_rshi_ul(JIT_R1, JIT_R1, 1);
      jit_lshi_ul(JIT_R1, JIT_R1, JIT_LOG_DOUBLE_SIZE);
      if (!is_f64) {
        jit_addi_ul(JIT_R1, JIT_R1, (int)(&SCHEME_FLVEC_ELS(0x0)));
      }
      jit_stxr_d_fppop(JIT_R1, JIT_R0, JIT_FPR0);
      CHECK_LIMIT();
      
      if (!result_ignored)
        (void)jit_movi_p(JIT_R0, scheme_void);
      
      return 1;
    } else if (IS_NAMED_PRIM(rator, "vector-immutable")
               || IS_NAMED_PRIM(rator, "vector")) {
      return generate_vector_alloc(jitter, rator, app, NULL, NULL);
    } else if (IS_NAMED_PRIM(rator, "list")
               || IS_NAMED_PRIM(rator, "list*")) {
      int c = app->num_args;
      int star;

      star = IS_NAMED_PRIM(rator, "list*");

      if (c)
        scheme_generate_app(app, NULL, c, jitter, 0, 0, 2);
      CHECK_LIMIT();
      mz_rs_sync();

#ifdef CAN_INLINE_ALLOC
      jit_movi_l(JIT_R2, c);
      if (star)
        (void)jit_calli(sjc.make_list_star_code);
      else
        (void)jit_calli(sjc.make_list_code);
#else
      JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
      jit_movi_l(JIT_R0, c);
      mz_prepare(2);
      jit_pusharg_l(JIT_R0);
      jit_pusharg_p(JIT_RUNSTACK);
      {
        GC_CAN_IGNORE jit_insn *refr;
        if (star)
          (void)mz_finish_lwe(ts_scheme_jit_make_list_star, refr);
        else
          (void)mz_finish_lwe(ts_scheme_jit_make_list, refr);
      }
      jit_retval(JIT_R0);
#endif

      if (c) {
        mz_rs_inc(c); /* no sync */
        mz_runstack_popped(jitter, c);
      }

      return 1;
    } else if (IS_NAMED_PRIM(rator, "values")) {
      int c = app->num_args;

      if (!multi_ok) return 0;

      if (c) {
        scheme_generate_app(app, NULL, c, jitter, 0, 0, 2);
        CHECK_LIMIT();
        mz_rs_sync();

        jit_movi_l(JIT_V1, c);
        (void)jit_calli(sjc.values_code);

        mz_rs_inc(c); /* no sync */
        mz_runstack_popped(jitter, c);
      } else {
        mz_tl_ldi_p(JIT_R2, tl_scheme_current_thread);
        jit_movi_l(JIT_R0, 0);
        jit_stxi_l(((int)&((Scheme_Thread *)0x0)->ku.multiple.count), JIT_R2, JIT_R0);
        jit_stxi_p(((int)&((Scheme_Thread *)0x0)->ku.multiple.array), JIT_R2, JIT_R0);
        (void)jit_movi_p(JIT_R0, SCHEME_MULTIPLE_VALUES);
      }

      return 1;
    } else if (IS_NAMED_PRIM(rator, "+")) {
      return scheme_generate_nary_arith(jitter, app, ARITH_ADD, 0, NULL, 1);
    } else if (IS_NAMED_PRIM(rator, "-")) {
      return scheme_generate_nary_arith(jitter, app, ARITH_SUB, 0, NULL, 1);
    } else if (IS_NAMED_PRIM(rator, "*")) {
      return scheme_generate_nary_arith(jitter, app, ARITH_MUL, 0, NULL, 1);
    } else if (IS_NAMED_PRIM(rator, "/")) {
      return scheme_generate_nary_arith(jitter, app, ARITH_DIV, 0, NULL, 1);
    } else if (IS_NAMED_PRIM(rator, "bitwise-and")) {
      return scheme_generate_nary_arith(jitter, app, ARITH_AND, 0, NULL, 1);
    } else if (IS_NAMED_PRIM(rator, "bitwise-ior")) {
      return scheme_generate_nary_arith(jitter, app, ARITH_IOR, 0, NULL, 1);
    } else if (IS_NAMED_PRIM(rator, "bitwise-xor")) {
      return scheme_generate_nary_arith(jitter, app, ARITH_XOR, 0, NULL, 1);
    } else if (IS_NAMED_PRIM(rator, "min")) {
      return scheme_generate_nary_arith(jitter, app, ARITH_MIN, 0, NULL, 1);
    } else if (IS_NAMED_PRIM(rator, "max")) {
      return scheme_generate_nary_arith(jitter, app, ARITH_MAX, 0, NULL, 1);
    } else if (IS_NAMED_PRIM(rator, "checked-procedure-check-and-extract")) {
      scheme_generate_app(app, NULL, 5, jitter, 0, 0, 2);  /* sync'd below */
      CHECK_LIMIT();
      mz_rs_sync();

      (void)jit_calli(sjc.struct_proc_extract_code);
      CHECK_LIMIT();

      mz_rs_inc(5);
      mz_runstack_popped(jitter, 5);

      return 1;
    }
  }

  if (!for_branch) {
    scheme_console_printf("Inlining expected.\n");
    abort();
  }

  --scheme_direct_call_count;

  return 0;
}

int scheme_generate_cons_alloc(mz_jit_state *jitter, int rev, int inline_retry)
/* Args must be in R0 (car) and R1 (cdr); uses R2 and V1 as temporaries */
{
#ifdef CAN_INLINE_ALLOC
  /* Inlined alloc */
  scheme_inline_alloc(jitter, sizeof(Scheme_Simple_Object), scheme_pair_type, 0, 1, 0, inline_retry);
  CHECK_LIMIT();
  
  if (rev) {
    jit_stxi_p((intptr_t)&SCHEME_CAR(0x0) + OBJHEAD_SIZE, JIT_V1, JIT_R1);
    jit_stxi_p((intptr_t)&SCHEME_CDR(0x0) + OBJHEAD_SIZE, JIT_V1, JIT_R0);
  } else {
    jit_stxi_p((intptr_t)&SCHEME_CAR(0x0) + OBJHEAD_SIZE, JIT_V1, JIT_R0);
    jit_stxi_p((intptr_t)&SCHEME_CDR(0x0) + OBJHEAD_SIZE, JIT_V1, JIT_R1);
  }
  jit_addi_p(JIT_R0, JIT_V1, OBJHEAD_SIZE);
#else
  /* Non-inlined */
  JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
  mz_prepare(2);
  if (rev) {
    jit_pusharg_p(JIT_R0);
    jit_pusharg_p(JIT_R1);
  } else {
    jit_pusharg_p(JIT_R1);
    jit_pusharg_p(JIT_R0);
  }
  {
    GC_CAN_IGNORE jit_insn *refr;
    (void)mz_finish_lwe(ts_scheme_make_pair, refr);
  }
  jit_retval(JIT_R0);
#endif

  return 1;
}

static int generate_vector_alloc(mz_jit_state *jitter, Scheme_Object *rator,
                                 Scheme_App_Rec *app, Scheme_App2_Rec *app2, Scheme_App3_Rec *app3)
/* de-sync'd ok */
{
  int imm, i, c;

  imm = IS_NAMED_PRIM(rator, "vector-immutable");

  if (app2) {
    mz_runstack_skipped(jitter, 1);
    scheme_generate_non_tail(app2->rand, jitter, 0, 1, 0); /* sync'd below */
    CHECK_LIMIT();
    mz_runstack_unskipped(jitter, 1);
    c = 1;
  } else if (app3) {
    generate_two_args(app3->rand1, app3->rand2, jitter, 1, 2);  /* sync'd below */
    c = 2;
  } else {
    c = app->num_args;
    if (c)
      scheme_generate_app(app, NULL, c, jitter, 0, 0, 2);  /* sync'd below */
  }
  CHECK_LIMIT();

  mz_rs_sync();

#ifdef CAN_INLINE_ALLOC
  /* Inlined alloc */
  if (app2)
    (void)jit_movi_p(JIT_R1, NULL); /* needed because R1 is marked during a GC */
  scheme_inline_alloc(jitter, 
                      sizeof(Scheme_Vector) + ((c - mzFLEX_DELTA) * sizeof(Scheme_Object*)), 
                      scheme_vector_type, 
                      imm, app2 || app3, 0, 0);
  CHECK_LIMIT();

  if ((c == 2) || (c == 1)) {
    jit_stxi_p((intptr_t)&SCHEME_VEC_ELS(0x0)[0] + OBJHEAD_SIZE, JIT_V1, JIT_R0);
  }
  if (c == 2) {
    jit_stxi_p((intptr_t)&SCHEME_VEC_ELS(0x0)[1] + OBJHEAD_SIZE, JIT_V1, JIT_R1);
  }
  jit_movi_l(JIT_R1, c);
  jit_stxi_l((intptr_t)&SCHEME_VEC_SIZE(0x0) + OBJHEAD_SIZE, JIT_V1, JIT_R1);
  jit_addi_p(JIT_R0, JIT_V1, OBJHEAD_SIZE);
#else
  {
    /* Non-inlined */
    GC_CAN_IGNORE jit_insn *refr;
    JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
    if (c == 1) {
      mz_prepare(1);
      jit_pusharg_p(JIT_R0);
      if (imm)
        (void)mz_finish_lwe(ts_scheme_jit_make_one_element_ivector, refr);
      else
        (void)mz_finish_lwe(ts_scheme_jit_make_one_element_vector, refr);
    } else if (c == 2) {
      mz_prepare(2);
      jit_pusharg_p(JIT_R1);
      jit_pusharg_p(JIT_R0);
      if (imm)
        (void)mz_finish_lwe(ts_scheme_jit_make_two_element_ivector, refr);
      else
        (void)mz_finish_lwe(ts_scheme_jit_make_two_element_vector, refr);
    } else {
      jit_movi_l(JIT_R1, c);
      mz_prepare(1);
      jit_pusharg_l(JIT_R1);
      if (imm)
        (void)mz_finish_lwe(ts_scheme_jit_make_ivector, refr);
      else
        (void)mz_finish_lwe(ts_scheme_jit_make_vector, refr);
    }
  }
  jit_retval(JIT_R0);
#endif

  CHECK_LIMIT();

  if (app) {
    for (i = 0; i < c; i++) {
      jit_ldxi_p(JIT_R1, JIT_RUNSTACK, WORDS_TO_BYTES(i));
      jit_stxi_p((intptr_t)&SCHEME_VEC_ELS(0x0)[i], JIT_R0, JIT_R1);
      CHECK_LIMIT();
    }
    
    if (c) {
      /* could use mz_rs */
      jit_addi_l(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(c));
      mz_runstack_popped(jitter, c);
    }
  }

  return 1;
}

int scheme_generate_inlined_test(mz_jit_state *jitter, Scheme_Object *obj, int branch_short, 
                                 Branch_Info *for_branch, int need_sync)
/* de-sync'd ok; syncs before jump */
{
  switch (SCHEME_TYPE(obj)) {
  case scheme_application_type:
    return scheme_generate_inlined_nary(jitter, (Scheme_App_Rec *)obj, 0, 0, for_branch, branch_short, 0);
  case scheme_application2_type:
    return scheme_generate_inlined_unary(jitter, (Scheme_App2_Rec *)obj, 0, 0, for_branch, branch_short, need_sync, 0);
  case scheme_application3_type:
    return scheme_generate_inlined_binary(jitter, (Scheme_App3_Rec *)obj, 0, 0, for_branch, branch_short, need_sync, 0);
  }

  return 0;
}

#endif
