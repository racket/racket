/*
  Racket
  Copyright (c) 2006-2013 PLT Design Inc.

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

struct scheme_jit_common_record scheme_jit_common;
void *scheme_on_demand_jit_code;

static void call_wrong_return_arity(int expected, int got, Scheme_Object **argv)
  
{
  scheme_wrong_return_arity(NULL, expected, got, argv, NULL);
}

static void raise_bad_call_with_values(Scheme_Object *f)
{
  Scheme_Object *a[1];
  a[0] = f;
  scheme_wrong_contract("call-with-values", "procedure?", -1, 1, a);    
}

static Scheme_Object *call_with_values_from_multiple_result(Scheme_Object *f)
{
  Scheme_Thread *p = scheme_current_thread;
  if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
    p->values_buffer = NULL;
  return _scheme_apply(f, p->ku.multiple.count, p->ku.multiple.array);
}

static Scheme_Object *call_with_values_from_multiple_result_multi(Scheme_Object *f)
{
  Scheme_Thread *p = scheme_current_thread;
  if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
    p->values_buffer = NULL;
  return _scheme_apply_multi(f, p->ku.multiple.count, p->ku.multiple.array);
}

static Scheme_Object *tail_call_with_values_from_multiple_result(Scheme_Object *f)
{
  Scheme_Thread *p = scheme_current_thread;
  int num_rands = p->ku.multiple.count;
  
  if (num_rands > p->tail_buffer_size) {
    /* scheme_tail_apply will allocate */
    if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
      p->values_buffer = NULL;
  }
  return scheme_tail_apply(f, num_rands, p->ku.multiple.array);
}

static Scheme_Object *apply_checked_fail(Scheme_Object **args)
{
  Scheme_Object *a[3];

  a[0] = args[1];
  a[1] = args[3];
  a[2] = args[4];

  return _scheme_apply(args[2], 3, a);
}

static void apply_prim_to_fail(int argc, Scheme_Object **argv, void *_p)
{
  Scheme_Primitive_Closure_Proc *p = (Scheme_Primitive_Closure_Proc *)_p;
  (void)p(argc, argv, NULL);
}

static Scheme_Object *vector_check_chaperone_of(Scheme_Object *o, Scheme_Object *orig, int setter)
{
  if (!scheme_chaperone_of(o, orig))
    scheme_contract_error((setter ? "vector-set!" : "vector-ref"),
                          "chaperone produced a result that is not a chaperone of the original result",
                          "chaperone result", 1, o,
                          "original result", 1, o,
                          NULL);
  
  return o;
}

static int save_struct_temp(mz_jit_state *jitter, int reg)
{
#ifdef MZ_USE_JIT_PPC
  jit_movr_p(JIT_V(3), reg);
#endif
#ifdef MZ_USE_JIT_I386
# ifdef X86_ALIGN_STACK
  mz_set_local_p(reg, JIT_LOCAL3);
# else
  jit_pushr_p(reg);
# endif
#endif
  return 1;
}

int scheme_save_struct_temp(mz_jit_state *jitter, int reg) {
  return save_struct_temp(jitter, reg);
}

static int restore_struct_temp(mz_jit_state *jitter, int reg)
{
#ifdef MZ_USE_JIT_PPC
  jit_movr_p(reg, JIT_V(3));
#endif
#ifdef MZ_USE_JIT_I386
# ifdef X86_ALIGN_STACK
  mz_get_local_p(reg, JIT_LOCAL3);
# else
  jit_popr_p(reg);
# endif
#endif
  return 1;
}

int scheme_restore_struct_temp(mz_jit_state *jitter, int reg) {
  return restore_struct_temp(jitter, reg);
}

static void allocate_values(int count, Scheme_Thread *p)
{
  Scheme_Object **a;

  a = MALLOC_N(Scheme_Object *, count);

  p->values_buffer = a;
  p->values_buffer_size = count;
}

void scheme_jit_allocate_values(int count, Scheme_Thread *p)
{
  allocate_values(count, p);
}

#ifdef MZ_USE_FUTURES
static void ts_allocate_values(int count, Scheme_Thread *p) XFORM_SKIP_PROC
{
  if (scheme_use_rtcall) {
    /* try thread-local allocation: */
    Scheme_Object **a;    
    a = MALLOC_N(Scheme_Object *, count);
    if (a) {
      p->values_buffer = a;
      p->values_buffer_size = count;
    } else
      scheme_rtcall_allocate_values(count, p);
  } else
    allocate_values(count, p);
}
#else
# define ts_allocate_values allocate_values
#endif

static void chaperone_set_mark()
/* arguments are on runstack; result goes there, too */
{
  Scheme_Object *v;
  v = scheme_chaperone_do_continuation_mark("with-continuation-mark", 0, MZ_RUNSTACK[1], MZ_RUNSTACK[0]);
  MZ_RUNSTACK[0] = v;
  MZ_RUNSTACK[1] = SCHEME_CHAPERONE_VAL(MZ_RUNSTACK[1]);
}

#define JITCOMMON_TS_PROCS
#include "jit_ts.c"

#ifdef MZ_USE_FUTURES
static Scheme_Object **ts_scheme_on_demand(Scheme_Object **rs) XFORM_SKIP_PROC
{
  if (scheme_use_rtcall) {
    return scheme_rtcall_on_demand(rs);
  } else
    return scheme_on_demand(rs);
}
#endif

/* ************************************************************ */

static int common0(mz_jit_state *jitter, void *_data)
{
  int in;
  GC_CAN_IGNORE jit_insn *ref, *ref2;

  /* *** check_arity_code *** */
  /* Called as a function: */
  sjc.check_arity_code = (Native_Check_Arity_Proc)jit_get_ip().ptr;
  jit_prolog(NATIVE_ARG_COUNT); /* only need 2 arguments, but return path overlaps with proc conventions */
  mz_push_threadlocal_early();
  in = jit_arg_p();
  jit_getarg_p(JIT_R0, in); /* closure */
  in = jit_arg_p();
  jit_getarg_i(JIT_R2, in); /* argc */
  in = jit_arg_p();
  jit_getarg_i(JIT_R1, in); /* ignored */
  mz_push_locals();
  mz_push_threadlocal(in);
  jit_movi_i(JIT_R1, -1);
  jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure *)0x0)->code);
  jit_ldxi_p(JIT_V1, JIT_V1, &((Scheme_Native_Closure_Data *)0x0)->arity_code);
  jit_jmpr(JIT_V1); /* leads to a jit_ret() that assumes NATIVE_ARG_COUNT arguments */
  CHECK_LIMIT();

  /* *** get_arity_code *** */
  /* Called as a function: */
  sjc.get_arity_code = (Native_Get_Arity_Proc)jit_get_ip().ptr;
  jit_prolog(NATIVE_ARG_COUNT); /* only need 1 argument, but return path overlaps with proc conventions */
  mz_push_threadlocal_early();
  in = jit_arg_p();
  jit_getarg_p(JIT_R0, in); /* closure */
  in = jit_arg_p();
  jit_getarg_p(JIT_R1, in); /* ignored */
  in = jit_arg_p();
  jit_getarg_i(JIT_R1, in); /* ignored */
  mz_push_locals();
  mz_push_threadlocal(in);
  jit_movi_i(JIT_R1, -1);
  (void)jit_movi_p(JIT_R2, 0x0);
  jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure *)0x0)->code);
  jit_ldxi_p(JIT_V1, JIT_V1, &((Scheme_Native_Closure_Data *)0x0)->arity_code);
  jit_jmpr(JIT_V1); /* leads to a jit_ret() that assumes NATIVE_ARG_COUNT arguments */
  CHECK_LIMIT();

  /* *** bad_result_arity_code *** */
  /* Jumped-to from non-tail contexts  */
  sjc.bad_result_arity_code = (Native_Get_Arity_Proc)jit_get_ip().ptr;
  mz_tl_ldi_p(JIT_R2, tl_scheme_current_thread);
  jit_ldxi_l(JIT_R1, JIT_R2, &((Scheme_Thread *)0x0)->ku.multiple.count);
  jit_ldxi_p(JIT_R2, JIT_R2, &((Scheme_Thread *)0x0)->ku.multiple.array);
  CHECK_LIMIT();
  mz_prepare(3);
  jit_pusharg_p(JIT_R2);
  jit_pusharg_i(JIT_R1);
  CHECK_LIMIT();
  jit_movi_i(JIT_V1, 1);
  jit_pusharg_i(JIT_V1);
  (void)mz_finish_lwe(ts_call_wrong_return_arity, ref);
  CHECK_LIMIT();

  /* *** unbound_global_code *** */
  sjc.unbound_global_code = jit_get_ip().ptr;
  JIT_UPDATE_THREAD_RSPTR();
  mz_prepare(1);
  jit_pusharg_p(JIT_R2);
  (void)mz_finish_lwe(ts_scheme_unbound_global, ref);
  CHECK_LIMIT();

  /* *** quote_syntax_code *** */
  /* R0 is WORDS_TO_BYTES(c), R1 is &0->a[i+p+1], R2 is &0->a[p] */
  sjc.quote_syntax_code = jit_get_ip().ptr;
  mz_prolog(JIT_V1);
  __START_SHORT_JUMPS__(1);
  /* Load global array: */
  jit_ldxr_p(JIT_V1, JIT_RUNSTACK, JIT_R0);
#ifdef JIT_PRECISE_GC
  /* Save global-array index before we lose it: */
  mz_set_local_p(JIT_R0, JIT_LOCAL3);
#endif
  /* Load syntax object: */
  jit_ldxr_p(JIT_R0, JIT_V1, JIT_R1);
  /* Is it null? */
  ref = jit_bnei_p(jit_forward(), JIT_R0, 0x0);
  CHECK_LIMIT();
  /* Syntax object is NULL, so we need to create it. */
  jit_ldxr_p(JIT_R0, JIT_V1, JIT_R2); /* put element at p in R0 */
#ifndef JIT_PRECISE_GC
  /* Save global array: */
  mz_set_local_p(JIT_V1, JIT_LOCAL3);
#endif
  /* Move R1 to V1 to save it: */
  jit_movr_p(JIT_V1, JIT_R1);
  /* Compute i in JIT_R1: */
  jit_subr_p(JIT_R1, JIT_R1, JIT_R2);
  jit_subi_p(JIT_R1, JIT_R1, WORDS_TO_BYTES(1));
  jit_rshi_ul(JIT_R1, JIT_R1, JIT_LOG_WORD_SIZE);
  CHECK_LIMIT();
  /* Call scheme_delayed_rename: */
  JIT_UPDATE_THREAD_RSPTR();
  CHECK_LIMIT();
  mz_prepare(2);
  jit_pusharg_l(JIT_R1);
  jit_pusharg_p(JIT_R0);
  (void)mz_finish_lwe(ts_scheme_delayed_rename, ref2);
  CHECK_LIMIT();
  jit_retval(JIT_R0);
  /* Restore global array into JIT_R1, and put computed element at i+p+1: */
#ifdef JIT_PRECISE_GC
  mz_get_local_p(JIT_R1, JIT_LOCAL3);
  jit_ldxr_p(JIT_R1, JIT_RUNSTACK, JIT_R1);
#else
  mz_get_local_p(JIT_R1, JIT_LOCAL3);
#endif
  jit_stxr_p(JIT_V1, JIT_R1, JIT_R0);
  mz_patch_branch(ref);
  __END_SHORT_JUMPS__(1);
  mz_epilog(JIT_V1);

  return 1;
}

static int common1(mz_jit_state *jitter, void *_data)
{
  int i;
  GC_CAN_IGNORE jit_insn *ref;

  /* *** [bad_][m]{car,cdr,...,{imag,real}_part}_code *** */
  /* Argument is in R2 for cXX+r, R0 otherwise */
  for (i = 0; i < 13; i++) {
    void *code;
    
    code = jit_get_ip().ptr;
    switch (i) {
    case 0:
      sjc.bad_car_code = code;
      break;
    case 1:
      sjc.bad_cdr_code = code;
      break;
    case 2:
      sjc.bad_caar_code = code;
      break;
    case 3:
      sjc.bad_cadr_code = code;
      break;
    case 4:
      sjc.bad_cdar_code = code;
      break;
    case 5:
      sjc.bad_cddr_code = code;      
      break;
    case 6:
      sjc.bad_mcar_code = code;
      break;
    case 7:
      sjc.bad_mcdr_code = code;
      break;
    case 8:
      sjc.real_part_code = code;
      break;
    case 9:
      sjc.imag_part_code = code;
      break;
    case 10:
      sjc.bad_flreal_part_code = code;
      break;
    case 11:
      sjc.bad_flimag_part_code = code;
      break;
    case 12:
      sjc.bad_cXr_code = code;
      break;
    }
    mz_prolog(JIT_R1);
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
    CHECK_RUNSTACK_OVERFLOW();
    if ((i != 12) && ((i < 2) || (i > 5))) {
      jit_str_p(JIT_RUNSTACK, JIT_R0);
    } else {
      jit_str_p(JIT_RUNSTACK, JIT_R2);
    }
    JIT_UPDATE_THREAD_RSPTR();
    CHECK_LIMIT();
    jit_movi_i(JIT_R1, 1);
    if (i == 12) {
      jit_prepare(3);
      jit_pusharg_p(JIT_R0);
    } else {
      jit_prepare(2);
    }
    jit_pusharg_p(JIT_RUNSTACK);
    jit_pusharg_i(JIT_R1);
    switch (i) {
    case 0:
      (void)mz_finish_lwe(ts_scheme_checked_car, ref);
      break;
    case 1:
      (void)mz_finish_lwe(ts_scheme_checked_cdr, ref);
      break;
    case 2:
      (void)mz_finish_lwe(ts_scheme_checked_caar, ref);
      break;
    case 3:
      (void)mz_finish_lwe(ts_scheme_checked_cadr, ref);
      break;
    case 4:
      (void)mz_finish_lwe(ts_scheme_checked_cdar, ref);
      break;
    case 5:
      (void)mz_finish_lwe(ts_scheme_checked_cddr, ref);
      break;
    case 6:
      (void)mz_finish_lwe(ts_scheme_checked_mcar, ref);
      break;
    case 7:
      (void)mz_finish_lwe(ts_scheme_checked_mcdr, ref);
      break;
    case 8:
      (void)mz_finish_lwe(ts_scheme_checked_real_part, ref);
      break;
    case 9:
      (void)mz_finish_lwe(ts_scheme_checked_imag_part, ref);
      break;
    case 10:
      (void)mz_finish_lwe(ts_scheme_checked_flreal_part, ref);
      break;
    case 11:
      (void)mz_finish_lwe(ts_scheme_checked_flimag_part, ref);
      break;
    case 12:
      (void)mz_finish_lwe(ts_apply_prim_to_fail, ref);
      break;
    }
    CHECK_LIMIT();

    switch (i) {
    case 8:
    case 9:
      jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
      JIT_UPDATE_THREAD_RSPTR();
      jit_retval(JIT_R0);
      mz_epilog(JIT_R1);
      break;
    default:
      /* never returns */
      break;
    }

    scheme_jit_register_sub_func(jitter, code, scheme_false);
  }

  return 1;
}

static int common1b(mz_jit_state *jitter, void *_data)
{
  int i;
  GC_CAN_IGNORE jit_insn *ref, *ref2;

  /* *** bad_set_{car,cdr}_code and make_[fl]rectangular_code *** */
  /* Bad argument is in R0, other is in R1 */
  for (i = 0; i < 4; i++) {
    void *code;
    code = jit_get_ip().ptr;
    switch (i) {
    case 0:
      sjc.bad_set_mcar_code = code;
      break;
    case 1:
      sjc.bad_set_mcdr_code = code;
      break;
    case 2:
      sjc.make_rectangular_code = code;
      break;
    case 3:
      sjc.bad_make_flrectangular_code = code;
      break;
    }
    mz_prolog(JIT_R2);
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(2));
    CHECK_RUNSTACK_OVERFLOW();
    jit_str_p(JIT_RUNSTACK, JIT_R0);
    jit_stxi_p(WORDS_TO_BYTES(1), JIT_RUNSTACK, JIT_R1);
    JIT_UPDATE_THREAD_RSPTR();
    CHECK_LIMIT();
    jit_movi_i(JIT_R1, 2);
    jit_prepare(2);
    jit_pusharg_p(JIT_RUNSTACK);
    jit_pusharg_i(JIT_R1);
    switch (i) {
    case 0:
      (void)mz_finish_lwe(ts_scheme_checked_set_mcar, ref);
      break;
    case 1:
      (void)mz_finish_lwe(ts_scheme_checked_set_mcdr, ref);
      break;
    case 2:
      (void)mz_finish_lwe(ts_scheme_checked_make_rectangular, ref);
      jit_retval(JIT_R0);
      jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(2));
      mz_epilog(JIT_R2);
      break;
    case 3:
      (void)mz_finish_lwe(ts_scheme_checked_make_flrectangular, ref);
      break;
    }
    CHECK_LIMIT();
    scheme_jit_register_sub_func(jitter, code, scheme_false);
  }

  /* *** unbox_code *** */
  /* R0 is argument */
  sjc.unbox_code = jit_get_ip().ptr;
  mz_prolog(JIT_R1);
  JIT_UPDATE_THREAD_RSPTR();
  jit_prepare(1);
  jit_pusharg_p(JIT_R0);
  (void)mz_finish_lwe(ts_scheme_unbox, ref);
  CHECK_LIMIT();
  jit_retval(JIT_R0); /* returns if proxied */
  mz_epilog(JIT_R1);
  scheme_jit_register_sub_func(jitter, sjc.unbox_code, scheme_false);

  /* *** set_box_code *** */
  /* R0 is box, R1 is value */
  sjc.set_box_code = jit_get_ip().ptr;
  mz_prolog(JIT_R2);
  JIT_UPDATE_THREAD_RSPTR();
  jit_prepare(2);
  jit_pusharg_p(JIT_R1);
  jit_pusharg_p(JIT_R0);
  (void)mz_finish_lwe(ts_scheme_set_box, ref);
  CHECK_LIMIT();
  /* returns if proxied */
  mz_epilog(JIT_R2);
  scheme_jit_register_sub_func(jitter, sjc.set_box_code, scheme_false);

  /* *** box_cas_fail_code *** */
  /* Arguments are on runstack; */
  /* call scheme_box_cas to raise the exception,
     we use mz_finish_lwe because it will capture the stack,
     and the ts_ version because we may be in a future */
  sjc.box_cas_fail_code = jit_get_ip().ptr;
  mz_prolog(JIT_R2);
  JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
  jit_movi_l(JIT_R0, 3);
  mz_prepare(2);
  jit_pusharg_p(JIT_RUNSTACK);
  jit_pusharg_l(JIT_R0);
  CHECK_LIMIT();      
  (void)mz_finish_lwe(ts_scheme_box_cas, ref); /* doesn't return */
  scheme_jit_register_sub_func(jitter, sjc.box_cas_fail_code, scheme_false);

  /* *** bad_vector_length_code *** */
  /* R0 is argument */
  sjc.bad_vector_length_code = jit_get_ip().ptr;
  mz_prolog(JIT_R1);

  /* Check for chaperone: */
  ref2 = jit_bmsi_ul(jit_forward(), JIT_R0, 0x1);
  ref = mz_bnei_t(jit_forward(), JIT_R0, scheme_chaperone_type, JIT_R1);
  jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&((Scheme_Chaperone *)0x0)->val);
  mz_epilog(JIT_R1); /* return after unwrapping */
  CHECK_LIMIT();

  mz_patch_branch(ref);
  mz_patch_branch(ref2);
  jit_prepare(1);
  jit_pusharg_p(JIT_R0);
  (void)mz_finish_lwe(ts_scheme_vector_length, ref);
  CHECK_LIMIT();
  scheme_jit_register_sub_func(jitter, sjc.bad_vector_length_code, scheme_false);

  /* *** bad_flvector_length_code *** */
  /* R0 is argument */
  sjc.bad_flvector_length_code = jit_get_ip().ptr;
  mz_prolog(JIT_R1);
  jit_prepare(1);
  jit_pusharg_p(JIT_R0);
  (void)mz_finish_lwe(ts_scheme_flvector_length, ref);
  CHECK_LIMIT();
  scheme_jit_register_sub_func(jitter, sjc.bad_flvector_length_code, scheme_false);

#ifdef MZ_LONG_DOUBLE
    /* *** bad_extflvector_length_code *** */
  /* R0 is argument */
  sjc.bad_extflvector_length_code = jit_get_ip().ptr;
  mz_prolog(JIT_R1);
  jit_prepare(1);
  jit_pusharg_p(JIT_R0);
  (void)mz_finish_lwe(ts_scheme_extflvector_length, ref);
  CHECK_LIMIT();
  scheme_jit_register_sub_func(jitter, sjc.bad_extflvector_length_code, scheme_false);
#endif

  /* *** bad_fxvector_length_code *** */
  /* R0 is argument */
  sjc.bad_fxvector_length_code = jit_get_ip().ptr;
  mz_prolog(JIT_R1);
  jit_prepare(1);
  jit_pusharg_p(JIT_R0);
  (void)mz_finish_lwe(ts_scheme_fxvector_length, ref);
  CHECK_LIMIT();
  scheme_jit_register_sub_func(jitter, sjc.bad_fxvector_length_code, scheme_false);

  return 1;
}

static int common2(mz_jit_state *jitter, void *_data)
{
  int in, i;
  GC_CAN_IGNORE jit_insn *ref, *ref2;

  /* *** call_original_unary_arith_code *** */
  /* R0 is arg, R2 is code pointer;
     if for branch, V1 is return address for false,
     LOCAL2 is target address for true */
  for (i = 0; i < 3; i++) {
    int argc, j;
    void *code;
    for (j = 0; j < 2; j++) {
      CHECK_LIMIT();
      code = jit_get_ip().ptr;
      if (!i) {
	if (!j)
	  sjc.call_original_unary_arith_code = code;
	else
	  sjc.call_original_unary_arith_for_branch_code = code;
	argc = 1;
      } else if (i == 1) {
	if (!j)
	  sjc.call_original_binary_arith_code = code;
	else
	  sjc.call_original_binary_arith_for_branch_code = code;
	argc = 2;
      } else {
	if (!j)
	  sjc.call_original_binary_rev_arith_code = code;
	else
	  sjc.call_original_binary_rev_arith_for_branch_code = code;
	argc = 2;
      }
      if (!j) {
        mz_prolog(JIT_V1);
      }
      jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(argc));
      CHECK_RUNSTACK_OVERFLOW();
      if (i == 2) {
	jit_str_p(JIT_RUNSTACK, JIT_R0);
	jit_stxi_p(WORDS_TO_BYTES(1), JIT_RUNSTACK, JIT_R1);
      } else if (i == 1) {
	jit_str_p(JIT_RUNSTACK, JIT_R1);
	jit_stxi_p(WORDS_TO_BYTES(1), JIT_RUNSTACK, JIT_R0);
      } else {
	jit_str_p(JIT_RUNSTACK, JIT_R0);
      }
      jit_movi_i(JIT_R1, argc);
      JIT_UPDATE_THREAD_RSPTR();
      mz_prepare_direct_prim(2);
      {
        /* May use JIT_R0 and create local branch: */
        mz_generate_direct_prim(jit_pusharg_p(JIT_RUNSTACK),
                                jit_pusharg_i(JIT_R1),
                                JIT_R2, scheme_noncm_prim_indirect);
      }
      CHECK_LIMIT();
      jit_retval(JIT_R0);
      VALIDATE_RESULT(JIT_R0);
      jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(argc));
      JIT_UPDATE_THREAD_RSPTR();
      if (!j) {
        mz_epilog(JIT_V1);
      } else {
	/* In for_branch mode, V1 is target for false, LOCAL2 is target for true */
	mz_get_local_p(JIT_R1, JIT_LOCAL2);
	__START_TINY_JUMPS__(1);
	ref = jit_beqi_p(jit_forward(), JIT_R0, scheme_true);
	jit_jmpr(JIT_V1);
	mz_patch_branch(ref);
	jit_jmpr(JIT_R1);
	__END_TINY_JUMPS__(1);
      }
      CHECK_LIMIT();

      if (!j)
        scheme_jit_register_sub_func(jitter, code, scheme_false);
      else
        scheme_jit_register_sub_func(jitter, code, scheme_void);
    }
  }

  /* *** call_original_nary_arith_code *** */
  /* rator is in V1, count is in R1, args are on runstack */
  {
    void *code;

    code = jit_get_ip().ptr;
    sjc.call_original_nary_arith_code = code;

    mz_prolog(JIT_R2);
    JIT_UPDATE_THREAD_RSPTR();
    mz_prepare_direct_prim(2);
    {
      /* May use JIT_R0 and create local branch: */
      mz_generate_direct_prim(jit_pusharg_p(JIT_RUNSTACK),
                              jit_pusharg_i(JIT_R1),
                              JIT_V1, scheme_noncm_prim_indirect);
    }
    CHECK_LIMIT();
    jit_retval(JIT_R0);
    VALIDATE_RESULT(JIT_R0);
    mz_epilog(JIT_R2);
    CHECK_LIMIT();

    scheme_jit_register_sub_func(jitter, code, scheme_false);
  }

  /* *** on_demand_jit_[arity_]code *** */
  /* Used as the code stub for a closure whose
     code is not yet compiled. See generate_function_prolog
     for the state of registers on entry */
  scheme_on_demand_jit_code = jit_get_ip().ptr;
  jit_prolog(NATIVE_ARG_COUNT);
  mz_push_threadlocal_early();
  in = jit_arg_p();
  jit_getarg_p(JIT_R0, in); /* closure */
  in = jit_arg_i();
  jit_getarg_i(JIT_R1, in); /* argc */
  in = jit_arg_p();
  jit_getarg_p(JIT_R2, in); /* argv */
  CHECK_LIMIT();
  mz_push_locals();
  mz_push_threadlocal(in);
  mz_tl_ldi_p(JIT_RUNSTACK, tl_MZ_RUNSTACK);
  sjc.on_demand_jit_arity_code = jit_get_ip().ptr; /* <<<- arity variant starts here */
  jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(2));
  CHECK_RUNSTACK_OVERFLOW();
  jit_str_p(JIT_RUNSTACK, JIT_R0);
  jit_fixnum_l(JIT_R1, JIT_R1);
  CHECK_LIMIT();
  jit_stxi_p(WORDS_TO_BYTES(1), JIT_RUNSTACK, JIT_R1);
  JIT_UPDATE_THREAD_RSPTR();
  mz_prepare(1);
  jit_pusharg_p(JIT_R2); /* argv is threaded through as an argument (for lwc handling) */
  (void)mz_finish_lwe(ts_scheme_on_demand, ref);
  CHECK_LIMIT();
  /* Restore registers and runstack, and jump to arity checking
     of newly-created code when argv == runstack (i.e., a tail call): */
  jit_retval(JIT_R2);
  jit_ldr_p(JIT_R0, JIT_RUNSTACK);
  jit_ldxi_p(JIT_R1, JIT_RUNSTACK, WORDS_TO_BYTES(1));
  jit_rshi_ul(JIT_R1, JIT_R1, 0x1);
  jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(2));
  CHECK_LIMIT();
  ref = jit_bner_p(jit_forward(), JIT_RUNSTACK, JIT_R2);
  /* Also, check that the runstack is big enough with the revised
     max_let_depth. */
  jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure *)0x0)->code);
  jit_ldxi_i(JIT_V1, JIT_V1, &((Scheme_Native_Closure_Data *)0x0)->max_let_depth);
  mz_set_local_p(JIT_R2, JIT_LOCAL2);
  mz_tl_ldi_p(JIT_R2, tl_MZ_RUNSTACK_START);
  jit_subr_ul(JIT_R2, JIT_RUNSTACK, JIT_R2);
  jit_subr_ul(JIT_V1, JIT_R2, JIT_V1);
  mz_get_local_p(JIT_R2, JIT_LOCAL2);
  ref2 = jit_blti_l(jit_forward(), JIT_V1, 0);
  CHECK_LIMIT();
  /* This is the tail-call fast path: */
  /* Set runstack base to end of arguments on runstack: */
  jit_movr_p(JIT_RUNSTACK_BASE_OR_ALT(JIT_V1), JIT_R1);
  jit_lshi_ul(JIT_RUNSTACK_BASE_OR_ALT(JIT_V1), JIT_RUNSTACK_BASE_OR_ALT(JIT_V1), JIT_LOG_WORD_SIZE);
  jit_addr_p(JIT_RUNSTACK_BASE_OR_ALT(JIT_V1), JIT_RUNSTACK_BASE_OR_ALT(JIT_V1), JIT_RUNSTACK);
  mz_st_runstack_base_alt(JIT_V1);
  /* Extract function and jump: */
  jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure *)0x0)->code);
  jit_ldxi_p(JIT_V1, JIT_V1, &((Scheme_Native_Closure_Data *)0x0)->arity_code);
  jit_jmpr(JIT_V1);
  CHECK_LIMIT();
  /* Slower path (non-tail) when argv != runstack. */
  mz_patch_branch(ref);
  mz_patch_branch(ref2);
  CHECK_LIMIT();
  JIT_UPDATE_THREAD_RSPTR();
  mz_prepare(3);
  jit_pusharg_p(JIT_R2);
  jit_pusharg_i(JIT_R1);
  jit_pusharg_p(JIT_R0);
  scheme_generate_finish_multi_apply(jitter);
  CHECK_LIMIT();
  mz_pop_threadlocal();
  mz_pop_locals();
  jit_ret();
  CHECK_LIMIT();
  scheme_jit_register_helper_func(jitter, scheme_on_demand_jit_code);

  /* Used for the state of a function that is being JITted 
     (for a kind of cycle detection) without breaking concurrent 
     future threads that might try to call the function. */
  sjc.in_progress_on_demand_jit_arity_code = jit_get_ip().ptr;
  (void)jit_jmpi(sjc.on_demand_jit_arity_code);

  /* *** app_values_tail_slow_code *** */
  /* RELIES ON jit_prolog(NATIVE_ARG_COUNT) FROM ABOVE */
  /* Rator in V1, arguments are in thread's multiple-values cells. */
  sjc.app_values_tail_slow_code = jit_get_ip().ptr;
  JIT_UPDATE_THREAD_RSPTR();
  mz_prepare(1);
  jit_pusharg_p(JIT_V1);
  (void)mz_finish_lwe(ts_tail_call_with_values_from_multiple_result, ref);
  jit_retval(JIT_R0);
  VALIDATE_RESULT(JIT_R0);
  /* Return: */
  mz_pop_threadlocal();
  mz_pop_locals();
  jit_ret();  
  CHECK_LIMIT();

  /* *** finish_tail_call_[fixup_]code *** */
  /* RELIES ON jit_prolog(NATIVE_ARG_COUNT) FROM ABOVE */
  sjc.finish_tail_call_code = jit_get_ip().ptr;
  scheme_generate_finish_tail_call(jitter, 0);
  CHECK_LIMIT();
  scheme_jit_register_helper_func(jitter, sjc.finish_tail_call_code);
  sjc.finish_tail_call_fixup_code = jit_get_ip().ptr;
  scheme_generate_finish_tail_call(jitter, 2);
  CHECK_LIMIT();
  scheme_jit_register_helper_func(jitter, sjc.finish_tail_call_fixup_code);

  /* *** get_stack_pointer_code *** */
  sjc.get_stack_pointer_code = jit_get_ip().ptr;
  jit_leaf(0);
  jit_movr_p(JIT_R0, JIT_FP);
  /* Get frame pointer of caller... */
#ifdef MZ_USE_JIT_PPC
  jit_ldr_p(JIT_R0, JIT_R0);
#endif
#ifdef MZ_USE_JIT_I386
  jit_ldr_p(JIT_R0, JIT_R0);
#endif
  jit_movr_p(JIT_RET, JIT_R0);
  jit_ret();
  CHECK_LIMIT();

  /* *** stack_cache_pop_code *** */
  /* DANGER: this code must save and restore (or avoid)
     any registers that a function call would normally save 
     and restore. JIT_AUX, which is used by things like jit_ldi,
     is such a register for PPC. */
  sjc.stack_cache_pop_code = jit_get_ip().ptr;
  jit_movr_p(JIT_R0, JIT_RET);
#ifdef MZ_USE_JIT_PPC
  jit_subi_p(JIT_SP, JIT_SP, 48); /* includes space maybe used by callee */
  jit_stxi_p(44, JIT_SP, JIT_AUX);
#endif
  /* Decrement stack_cache_stack_pos (using a function,
     in case of thread-local vars) and get record pointer.
     Use jit_normal_finish(), because jit_finish() shuffles
     callee-saved registers to match the mz protocol 
     (on x86_64). */
  mz_prepare(1);
  jit_normal_pushonlyarg_p(JIT_R0);
  (void)jit_normal_finish(scheme_decrement_cache_stack_pos);
  jit_retval(JIT_R1); /* = pointer to a stack_cache_stack element */
  CHECK_LIMIT();
  /* Extract old return address and jump to it */
  jit_ldxi_l(JIT_R0, JIT_R1, (int)&((Stack_Cache_Elem *)0x0)->orig_result);
  (void)jit_movi_p(JIT_R2, NULL);
  jit_stxi_l((int)&((Stack_Cache_Elem *)0x0)->orig_result, JIT_R1, JIT_R2);
  jit_ldxi_l(JIT_R2, JIT_R1, (int)&((Stack_Cache_Elem *)0x0)->orig_return_address);
  jit_movr_p(JIT_RET, JIT_R0);
#ifdef MZ_USE_JIT_PPC
  jit_ldxi_p(JIT_AUX, JIT_SP, 44);
  jit_addi_p(JIT_SP, JIT_SP, 48);
#endif
  jit_jmpr(JIT_R2);
  CHECK_LIMIT();

  /* *** bad_app_vals_target *** */
  /* Non-proc is in R0 */
  sjc.bad_app_vals_target = jit_get_ip().ptr;
  JIT_UPDATE_THREAD_RSPTR();
  mz_prepare(1);
  jit_pusharg_p(JIT_R0);
  (void)mz_finish_lwe(ts_raise_bad_call_with_values, ref);
  /* Doesn't return */
  CHECK_LIMIT();

  /* *** app_values[_multi]_slow_code *** */
  /* Rator in V1, arguments are in thread's multiple-values cells. */
  for (i = 0; i < 2; i++) {
    if (i)
      sjc.app_values_multi_slow_code = jit_get_ip().ptr;
    else
      sjc.app_values_slow_code = jit_get_ip().ptr;
    mz_prolog(JIT_R1);
    JIT_UPDATE_THREAD_RSPTR();
    mz_prepare(1);
    jit_pusharg_p(JIT_V1);
    if (i) {
      (void)mz_finish_lwe(ts_call_with_values_from_multiple_result_multi, ref);
    } else {
      (void)mz_finish_lwe(ts_call_with_values_from_multiple_result, ref);
    }
    jit_retval(JIT_R0);
    VALIDATE_RESULT(JIT_R0);
    mz_epilog(JIT_R1);
    CHECK_LIMIT();
  }

  /*** values_code ***/
  /* Arguments on runstack, V1 has count */
  {
    GC_CAN_IGNORE jit_insn *refslow, *ref1, *refloop, *ref2;

    sjc.values_code = jit_get_ip().ptr;
    mz_prolog(JIT_R1);
    mz_tl_ldi_p(JIT_R2, tl_scheme_current_thread);
    jit_ldxi_p(JIT_R1, JIT_R2, &((Scheme_Thread *)0x0)->values_buffer);
    ref1 = jit_bnei_p(jit_forward(), JIT_R1, NULL);
    CHECK_LIMIT();

    /* Allocate new array: */
    refslow = _jit.x.pc;
    JIT_UPDATE_THREAD_RSPTR();
    mz_prepare(2);
    jit_pusharg_p(JIT_R2);
    jit_pusharg_i(JIT_V1);
    (void)mz_finish_lwe(ts_allocate_values, ref2);
    CHECK_LIMIT();

    /* Try again... */
    mz_tl_ldi_p(JIT_R2, tl_scheme_current_thread);
    jit_ldxi_p(JIT_R1, JIT_R2, &((Scheme_Thread *)0x0)->values_buffer);

    /* Buffer is non-NULL... big enough? */
    mz_patch_branch(ref1);
    jit_ldxi_i(JIT_R0, JIT_R2, &((Scheme_Thread *)0x0)->values_buffer_size);
    (void)jit_bltr_i(refslow, JIT_R0, JIT_V1);
    
    /* Buffer is ready */
    jit_stxi_p(&((Scheme_Thread *)0x0)->ku.multiple.array, JIT_R2, JIT_R1);
    jit_stxi_i(&((Scheme_Thread *)0x0)->ku.multiple.count, JIT_R2, JIT_V1);
    CHECK_LIMIT();
    
    /* Copy values over: */
    jit_movr_p(JIT_R0, JIT_RUNSTACK);
    refloop = _jit.x.pc;
    jit_ldr_p(JIT_R2, JIT_R0);
    jit_str_p(JIT_R1, JIT_R2);
    jit_subi_l(JIT_V1, JIT_V1, 1);
    jit_addi_p(JIT_R0, JIT_R0, JIT_WORD_SIZE);
    jit_addi_p(JIT_R1, JIT_R1, JIT_WORD_SIZE);
    (void)jit_bnei_l(refloop, JIT_V1, 0);
    CHECK_LIMIT();

    (void)jit_movi_p(JIT_R0, SCHEME_MULTIPLE_VALUES);
    
    mz_epilog(JIT_R1);
    CHECK_LIMIT();
  }

  return 1;
}

static int generate_apply_proxy(mz_jit_state *jitter, int setter)
/* current val in R0, chaperone-filtered val in R0;
   original chaperone and index on runstack;
   for setter, put back result in R2, vec in R0, and index in V1 */
{
  GC_CAN_IGNORE jit_insn *ref, *ref1, *ref2, *refrts;

  CHECK_LIMIT();
  jit_ldr_p(JIT_R2, JIT_RUNSTACK);
  jit_ldxi_p(JIT_R1, JIT_R2, &((Scheme_Chaperone *)0x0)->redirects);

  /* if chaperone was for properties, only, then we're done */
  ref = mz_beqi_t(jit_forward(), JIT_R1, scheme_vector_type, JIT_V1);

  if (setter)
    jit_ldxi_p(JIT_V1, JIT_R1, &SCHEME_CDR(0x0)); /* rator */
  else
    jit_ldxi_p(JIT_V1, JIT_R1, &SCHEME_CAR(0x0)); /* rator */
  jit_ldxi_p(JIT_R2, JIT_R2, &((Scheme_Chaperone *)0x0)->prev); /* vec */
  jit_ldxi_p(JIT_R1, JIT_RUNSTACK, WORDS_TO_BYTES(1)); /* index */
  if (setter) {
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(4));
    jit_stxi_p(WORDS_TO_BYTES(3), JIT_RUNSTACK, JIT_R0); /* save value */
  } else {
    jit_stxi_p(WORDS_TO_BYTES(1), JIT_RUNSTACK, JIT_R0); /* save value */
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(3));
  }
  jit_str_p(JIT_RUNSTACK, JIT_R2);
  jit_stxi_p(WORDS_TO_BYTES(1), JIT_RUNSTACK, JIT_R1);
  jit_stxi_p(WORDS_TO_BYTES(2), JIT_RUNSTACK, JIT_R0);
  CHECK_LIMIT();
  JIT_UPDATE_THREAD_RSPTR();
  __END_SHORT_JUMPS__(1);
  scheme_generate_non_tail_call(jitter, 3, 0, 0, 0, 0, 0, 0, 1, 0);
  __START_SHORT_JUMPS__(1);
  CHECK_LIMIT();
  if (setter) {
    jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(4));
  } else {
    jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(3));
  }
            
  jit_ldr_p(JIT_R1, JIT_RUNSTACK);
  jit_ldxi_s(JIT_R2, JIT_R1, &MZ_OPT_HASH_KEY(&((Scheme_Stx *)0x0)->iso));
  /* if impersonator, no chaperone-of check needed */
  ref1 = jit_bmsi_ul(jit_forward(), JIT_R2, SCHEME_CHAPERONE_IS_IMPERSONATOR);

  if (setter)
    jit_ldxi_p(JIT_R1, JIT_RUNSTACK, WORDS_TO_BYTES(-1)); /* saved value */
  else
    jit_ldxi_p(JIT_R1, JIT_RUNSTACK, WORDS_TO_BYTES(1)); /* saved value */
  ref2 = jit_beqr_p(jit_forward(), JIT_R1, JIT_R0);
  CHECK_LIMIT();
  jit_prepare(3);
  jit_movi_i(JIT_R2, setter);
  jit_pusharg_i(JIT_R2);
  jit_pusharg_p(JIT_R1);
  jit_pusharg_p(JIT_R0);
  JIT_UPDATE_THREAD_RSPTR();
  (void)mz_finish_lwe(ts_vector_check_chaperone_of, refrts);
  jit_retval(JIT_R0);
  CHECK_LIMIT();
            
  mz_patch_branch(ref);
  mz_patch_branch(ref1);
  mz_patch_branch(ref2);
  if (setter) {
    jit_movr_p(JIT_R2, JIT_R0); /* result needed in R2 for setter */
    jit_ldxi_p(JIT_V1, JIT_RUNSTACK, WORDS_TO_BYTES(1)); /* saved index */
    jit_ldr_p(JIT_R0, JIT_RUNSTACK); /* saved chaperone */
    jit_ldxi_p(JIT_R0, JIT_R0, &((Scheme_Chaperone *)0x0)->prev); /* vec */
  }
  jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(2)); /* don't need saved anymore */
  JIT_UPDATE_THREAD_RSPTR();

  return 1;
}

static int common3(mz_jit_state *jitter, void *_data)
{
  int i, ii, iii;

  /* *** {vector,string,bytes}_{ref,set}_[check_index_]code *** */
  /* R0 is vector/string/bytes, R1 is index (Scheme number in check-index mode), 
     V1 is vector/string/bytes offset in non-check-index mode (and for
     vector, it includes the offset to the start of the elements array).
     In set mode, value is on run stack. */
  for (iii = 0; iii < 2; iii++) { /* ref, set */
    for (ii = -1; ii < 4; ii++) { /* chap-vector, vector, string, bytes, fx */
      for (i = 0; i < 2; i++) { /* check index? */
	GC_CAN_IGNORE jit_insn *ref, *reffail, *refrts;
	Scheme_Type ty;
	int offset, count_offset, log_elem_size;
        void *code;

        code = jit_get_ip().ptr;

	switch (ii) {
	case -1:
	case 0:
	  ty = scheme_vector_type;
	  offset = (int)&SCHEME_VEC_ELS(0x0);
	  count_offset = (int)&SCHEME_VEC_SIZE(0x0);
	  log_elem_size = JIT_LOG_WORD_SIZE;
          if (ii == -1) {
            if (!iii) {
              if (!i) {
                sjc.chap_vector_ref_code = code;
              } else {
                sjc.chap_vector_ref_check_index_code = code;
              }
            } else {
              if (!i) {
                sjc.chap_vector_set_code = code;
              } else {
                sjc.chap_vector_set_check_index_code = code;
              }
            }
          } else if (!iii) {
	    if (!i) {
	      sjc.vector_ref_code = code;
	    } else {
	      sjc.vector_ref_check_index_code = code;
	    }
	  } else {
	    if (!i) {
	      sjc.vector_set_code = code;
	    } else {
	      sjc.vector_set_check_index_code = code;
	    }
	  }
	  break;
	case 1:
	  ty = scheme_char_string_type;
	  offset = (int)&SCHEME_CHAR_STR_VAL(0x0);
	  count_offset = (int)&SCHEME_CHAR_STRLEN_VAL(0x0);
	  log_elem_size = LOG_MZCHAR_SIZE;
	  if (!iii) {
	    if (!i) {
	      sjc.string_ref_code = code;
	    } else {
	      sjc.string_ref_check_index_code = code;
	    }
	  } else {
	    if (!i) {
	      sjc.string_set_code = code;
	    } else {
	      sjc.string_set_check_index_code = code;
	    }
	  }
	  break;
	case 2:
	  ty = scheme_byte_string_type;
	  offset = (int)&SCHEME_BYTE_STR_VAL(0x0);
	  count_offset = (int)&SCHEME_BYTE_STRLEN_VAL(0x0);
	  log_elem_size = 0;
	  if (!iii) {
	    if (!i) {
	      sjc.bytes_ref_code = code;
	    } else {
	      sjc.bytes_ref_check_index_code = code;
	    }
	  } else {
	    if (!i) {
	      sjc.bytes_set_code = code;
	    } else {
	      sjc.bytes_set_check_index_code = code;
	    }
	  }
	  break;
	default:
	case 3:
	  ty = scheme_fxvector_type;
	  offset = (int)&SCHEME_VEC_ELS(0x0);
	  count_offset = (int)&SCHEME_VEC_SIZE(0x0);
	  log_elem_size = JIT_LOG_WORD_SIZE;
	  if (!iii) {
	    if (!i) {
	      sjc.fxvector_ref_code = code;
	    } else {
	      sjc.fxvector_ref_check_index_code = code;
	    }
	  } else {
	    if (!i) {
	      sjc.fxvector_set_code = code;
	    } else {
	      sjc.fxvector_set_check_index_code = code;
	    }
	  }
	  break;
	}

	__START_SHORT_JUMPS__(1);

        if (ii != -1) {
          mz_prolog(JIT_R2);
        } else {
          /* skip prolog for chaperone handling, but save original R0 & R1: */
          jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(2));
          CHECK_RUNSTACK_OVERFLOW();
          jit_str_p(JIT_RUNSTACK, JIT_R0);
          if (i)
            jit_stxi_p(WORDS_TO_BYTES(1), JIT_RUNSTACK, JIT_R1);
          else {
            jit_fixnum_l(JIT_R2, JIT_R1);
            jit_stxi_p(WORDS_TO_BYTES(1), JIT_RUNSTACK, JIT_R2);
          }
        }

	ref = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);
	CHECK_LIMIT();

	/* Slow path: */
	reffail = _jit.x.pc;
        if (ii != -1) {
          /* in chaperone mode, we already saved original and index on runstack */
          if (!i) {
            jit_fixnum_l(JIT_R1, JIT_R1);
          }
          jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(2));
          CHECK_RUNSTACK_OVERFLOW();
          jit_str_p(JIT_RUNSTACK, JIT_R0);
          jit_stxi_p(WORDS_TO_BYTES(1), JIT_RUNSTACK, JIT_R1);
        }
	if (!iii) {
	  jit_movi_i(JIT_R1, 2);
	} else {
	  /* In set mode, value was already on run stack */
	  jit_movi_i(JIT_R1, 3);
	}
	JIT_UPDATE_THREAD_RSPTR();
	jit_prepare(2);
	jit_pusharg_p(JIT_RUNSTACK);
	jit_pusharg_i(JIT_R1);
	switch (ii) {
	case -1:
	case 0:
	  if (!iii) {
	    (void)mz_finish_lwe(ts_scheme_checked_vector_ref, refrts);
	  } else {
	    (void)mz_finish_lwe(ts_scheme_checked_vector_set, refrts);
	  }
          CHECK_LIMIT();
          /* Might return, if arg was chaperone */
          jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(2));
          JIT_UPDATE_THREAD_RSPTR();
          if (!iii)
            jit_retval(JIT_R0);
          mz_epilog(JIT_R2);
	  break;
	case 1:
	  if (!iii) {
	    (void)mz_finish_lwe(ts_scheme_checked_string_ref, refrts);
            CHECK_LIMIT();
	    /* might return, if char was outside Latin-1 */
	    jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(2));
	    JIT_UPDATE_THREAD_RSPTR();
	    jit_retval(JIT_R0);
	    mz_epilog(JIT_R2);
	  } else {
	    (void)mz_finish_lwe(ts_scheme_checked_string_set, refrts);
	  }
	  break;
	case 2:
	  if (!iii) {
	    (void)mz_finish_lwe(ts_scheme_checked_byte_string_ref, refrts);
	  } else {
	    (void)mz_finish_lwe(ts_scheme_checked_byte_string_set, refrts);
	  }
	  break;
	case 3:
	  if (!iii) {
	    (void)mz_finish_lwe(ts_scheme_checked_fxvector_ref, refrts);
	  } else {
	    (void)mz_finish_lwe(ts_scheme_checked_fxvector_set, refrts);
	  }
	  break;
	}
	/* doesn't return */
	CHECK_LIMIT();

        /* Continue fast path */
    
	mz_patch_branch(ref);
	if (i) {
	  (void)jit_bmci_ul(reffail, JIT_R1, 0x1);
	  (void)jit_blei_l(reffail, JIT_R1, 0x0);
	}
        if (!ii) {
          __END_SHORT_JUMPS__(1);
          if (iii == 0) {
            if (i)
              (void)mz_beqi_t(sjc.chap_vector_ref_check_index_code, JIT_R0, scheme_chaperone_type, JIT_R2);
            else
              (void)mz_beqi_t(sjc.chap_vector_ref_code, JIT_R0, scheme_chaperone_type, JIT_R2);
          } else {
            if (i)
              (void)mz_beqi_t(sjc.chap_vector_set_check_index_code, JIT_R0, scheme_chaperone_type, JIT_R2);
            else
              (void)mz_beqi_t(sjc.chap_vector_set_code, JIT_R0, scheme_chaperone_type, JIT_R2);
          }
          __START_SHORT_JUMPS__(1);
        } else if (ii == -1) {
          /* since we got here, we know that the wrapper is a proxy */
          jit_ldxi_p(JIT_R0, JIT_R0, &((Scheme_Chaperone *)0x0)->prev);
        }
	(void)mz_bnei_t(reffail, JIT_R0, ty, JIT_R2);
        if (iii) {
          jit_ldxi_s(JIT_R2, JIT_R0, &(MZ_OPT_HASH_KEY((Scheme_Inclhash_Object *)0x0)));
          (void)jit_bmsi_ul(reffail, JIT_R2, 0x1);
        }
	jit_ldxi_l(JIT_R2, JIT_R0, count_offset);
        CHECK_LIMIT();
	if (i) {
	  /* index from expression: */
	  jit_rshi_ul(JIT_V1, JIT_R1, 1);
	  (void)jit_bler_ul(reffail, JIT_R2, JIT_V1);
	  if (log_elem_size)
	    jit_lshi_ul(JIT_V1, JIT_V1, log_elem_size);
	  if (!ii || (ii == -1)) /* vector */
	    jit_addi_p(JIT_V1, JIT_V1, offset);
	} else {
	  /* constant index supplied: */
	  (void)jit_bler_ul(reffail, JIT_R2, JIT_R1);
	}
	if (!iii) {
	  /* ref mode: */
	  switch (ii) {
	  case -1: /* chap-vector */
	  case 0: /* vector */
	  case 3: /* fxvector */
	    jit_ldxr_p(JIT_R0, JIT_R0, JIT_V1);
	    break;
	  case 1: /* string */
	    jit_ldxi_p(JIT_R2, JIT_R0, offset);
	    jit_ldxr_i(JIT_R2, JIT_R2, JIT_V1);
	    /* Non-Latin-1 char: use slow path: */
	    jit_extr_i_l(JIT_R2, JIT_R2);
	    (void)jit_bgti_l(reffail, JIT_R2, 255);
	    /* Latin-1: extract from scheme_char_constants: */
	    jit_lshi_l(JIT_R2, JIT_R2, JIT_LOG_WORD_SIZE);
	    (void)jit_movi_p(JIT_R0, scheme_char_constants);
	    jit_ldxr_p(JIT_R0, JIT_R0, JIT_R2);
	    break;
	  case 2: /* bytes */
	    jit_ldxi_p(JIT_R0, JIT_R0, offset);
	    jit_ldxr_c(JIT_R0, JIT_R0, JIT_V1);
	    jit_extr_uc_ul(JIT_R0, JIT_R0);
            jit_fixnum_l(JIT_R0, JIT_R0);
	    break;
	  }

          if (ii == -1) {
            /* apply proxy */
            CHECK_LIMIT();
            generate_apply_proxy(jitter, 0);
            CHECK_LIMIT();
          }
	} else {
	  /* set mode: */
          if (ii == -1) {
            /* apply proxy */
            CHECK_LIMIT();
            jit_ldxi_p(JIT_R0, JIT_RUNSTACK, WORDS_TO_BYTES(2));
            generate_apply_proxy(jitter, 1);
            CHECK_LIMIT();

            /* recompute offset */
            jit_rshi_ul(JIT_V1, JIT_V1, 1);
            if (log_elem_size)
              jit_lshi_ul(JIT_V1, JIT_V1, log_elem_size);
            jit_addi_p(JIT_V1, JIT_V1, offset);            
          } else {
            jit_ldr_p(JIT_R2, JIT_RUNSTACK);
          }
	  switch (ii) {
	  case 3: /* fxvector */
            (void)jit_bmci_l(reffail, JIT_R2, 0x1);
	  case -1: /* chap-vector, fall-though from fxvector */
	  case 0: /* vector, fall-though from fxvector */
	    jit_stxr_p(JIT_V1, JIT_R0, JIT_R2);
	    break;
	  case 1: /* string */
            (void)jit_bmsi_l(reffail, JIT_R2, 0x1);
	    jit_ldxi_s(JIT_R2, JIT_R2, &((Scheme_Object *)0x0)->type);
	    (void)jit_bnei_i(reffail, JIT_R2, scheme_char_type);
	    jit_ldr_p(JIT_R2, JIT_RUNSTACK);
	    jit_ldxi_i(JIT_R2, JIT_R2, &((Scheme_Small_Object *)0x0)->u.char_val);
	    jit_ldxi_p(JIT_R0, JIT_R0, offset);
	    jit_stxr_i(JIT_V1, JIT_R0, JIT_R2);
	    break;
	  case 2: /* bytes */
	    (void)jit_bmci_l(reffail, JIT_R2, 0x1);
	    jit_rshi_ul(JIT_R2, JIT_R2, 1);
	    (void)jit_bmsi_l(reffail, JIT_R2, ~0xFF);
	    jit_ldxi_p(JIT_R0, JIT_R0, offset);
	    jit_stxr_c(JIT_V1, JIT_R0, JIT_R2);
	    break;
	  }
          (void)jit_movi_p(JIT_R0, scheme_void);
	}
	mz_epilog(JIT_R2);
	CHECK_LIMIT();

	__END_SHORT_JUMPS__(1);

        scheme_jit_register_sub_func(jitter, code, scheme_false);
      }
    }
  }

  return 1;
}

static int gen_struct_slow(mz_jit_state *jitter, int kind, int ok_proc, 
                           int for_branch, int is_tail, int multi_ok,
                           GC_CAN_IGNORE jit_insn **_bref5,
                           GC_CAN_IGNORE jit_insn **_bref6)
{
  GC_CAN_IGNORE jit_insn *bref5, *bref6, *refrts;

  jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES((kind == 3) ? 2 : 1));
  CHECK_RUNSTACK_OVERFLOW();
  JIT_UPDATE_THREAD_RSPTR();
  jit_str_p(JIT_RUNSTACK, JIT_R1);
  if (kind == 3) {
    restore_struct_temp(jitter, JIT_V1);        
    jit_stxi_p(WORDS_TO_BYTES(1), JIT_RUNSTACK, JIT_V1);
  }
  jit_movi_i(JIT_V1, ((kind == 3) ? 2 : 1));
  jit_prepare(3);
  if (!ok_proc) {
    jit_pusharg_p(JIT_RUNSTACK);
    jit_pusharg_i(JIT_V1);
    jit_pusharg_p(JIT_R0);
    if (is_tail) {
      scheme_generate_finish_tail_apply(jitter);
    } else if (multi_ok) {
      scheme_generate_finish_multi_apply(jitter);
    } else {
      scheme_generate_finish_apply(jitter);
    }
    CHECK_LIMIT();
  } else {
    /* The proc is a setter or getter, but the argument is
       bad or chaperoned. We can take a shortcut by using
       scheme_struct_getter() or scheme_struct_setter() instead
       of going through scheme_apply(). */
    jit_pusharg_p(JIT_R0);
    jit_pusharg_p(JIT_RUNSTACK);
    jit_pusharg_i(JIT_V1);
    if (kind == 2)
      (void)mz_finish_lwe(ts_scheme_struct_getter, refrts);
    else
      (void)mz_finish_lwe(ts_scheme_struct_setter, refrts);
  }
  jit_retval(JIT_R0);
  VALIDATE_RESULT(JIT_R0);
  jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES((kind == 3) ? 2 : 1));
  JIT_UPDATE_THREAD_RSPTR();
  if (!for_branch) {
    mz_epilog(JIT_V1);
    bref5 = NULL;
    bref6 = NULL;
  } else {
    /* Need to check for true or false. */
    bref5 = jit_beqi_p(jit_forward(), JIT_R0, scheme_false);
    bref6 = jit_jmpi(jit_forward());
  }

  if (_bref5) {
    *_bref5 = bref5;
    *_bref6 = bref6;
  }

  return 1;
}

int scheme_generate_struct_op(mz_jit_state *jitter, int kind, int for_branch,
                              Branch_Info *branch_info, int branch_short, 
                              int result_ignored,
                              int check_proc, int check_arg_fixnum,
                              int type_pos, int field_pos, 
                              int pop_and_jump,
                              GC_CAN_IGNORE jit_insn *refslow, GC_CAN_IGNORE jit_insn *refslow2,
                              GC_CAN_IGNORE jit_insn *bref_false, GC_CAN_IGNORE jit_insn *bref_true)
/* kind: pred (1), get (2), or set (3) 
   R0 is (potential) struct proc, R1 is (potential) struct.
   In set mode, value to install is saved as a temp. */
{
  GC_CAN_IGNORE jit_insn *ref2, *ref3, *bref1, *bref2, *refretry;
  GC_CAN_IGNORE jit_insn *bref3, *bref4, *bref8, *ref9, *refdone;

  __START_SHORT_JUMPS__(branch_short);

  if (check_proc) {
    (void)mz_bnei_t(refslow, JIT_R0, scheme_prim_type, JIT_R2);
    jit_ldxi_s(JIT_R2, JIT_R0, &((Scheme_Primitive_Proc *)0x0)->pp.flags);
    jit_andi_i(JIT_R2, JIT_R2, SCHEME_PRIM_OTHER_TYPE_MASK);
    (void)jit_bnei_i(refslow, JIT_R2, ((kind == 3)
                                       ? SCHEME_PRIM_STRUCT_TYPE_INDEXED_SETTER
                                       : ((kind == 1) 
                                          ? SCHEME_PRIM_STRUCT_TYPE_PRED
                                          : SCHEME_PRIM_STRUCT_TYPE_INDEXED_GETTER)));
  }

  CHECK_LIMIT();
  /* Check argument: */
  if (kind == 1) {
    bref1 = jit_bmsi_ul(jit_forward(), JIT_R1, 0x1);
    refretry = _jit.x.pc;
    jit_ldxi_s(JIT_R2, JIT_R1, &((Scheme_Object *)0x0)->type);
    __START_INNER_TINY__(1);
    ref2 = jit_beqi_i(jit_forward(), JIT_R2, scheme_structure_type);
    ref3 = jit_beqi_i(jit_forward(), JIT_R2, scheme_proc_struct_type);
    CHECK_LIMIT();
    ref9 = jit_beqi_i(jit_forward(), JIT_R2, scheme_chaperone_type);
    __END_INNER_TINY__(1);
    bref2 = jit_bnei_i(jit_forward(), JIT_R2, scheme_proc_chaperone_type);
    CHECK_LIMIT();
    __START_INNER_TINY__(1);
    mz_patch_branch(ref9);
    jit_ldxi_p(JIT_R1, JIT_R1, &SCHEME_CHAPERONE_VAL(0x0));
    (void)jit_jmpi(refretry);
    mz_patch_branch(ref3);
    __END_INNER_TINY__(1);
  } else {
    if (check_arg_fixnum) {
      (void)jit_bmsi_ul(refslow2, JIT_R1, 0x1);
    }
    jit_ldxi_s(JIT_R2, JIT_R1, &((Scheme_Object *)0x0)->type);
    __START_INNER_TINY__(1);
    ref2 = jit_beqi_i(jit_forward(), JIT_R2, scheme_structure_type);
    __END_INNER_TINY__(1);
    (void)jit_bnei_i(refslow2, JIT_R2, scheme_proc_struct_type);
    bref1 = bref2 = NULL;
  }
  __START_INNER_TINY__(1);
  mz_patch_branch(ref2);
  __END_INNER_TINY__(1);
  CHECK_LIMIT();

  if (type_pos != 0) {
    /* Put argument struct type in R2, target struct type in V1 */
    jit_ldxi_p(JIT_R2, JIT_R1, &((Scheme_Structure *)0x0)->stype);
    if (type_pos < 0) {
      jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Primitive_Closure *)0x0)->val);
    }
    CHECK_LIMIT();

    if (type_pos < 0) {
      /* common case: types are the same */
      if (kind >= 2) {
        __START_INNER_TINY__(1);
        bref8 = jit_beqr_p(jit_forward(), JIT_R2, JIT_V1);
        __END_INNER_TINY__(1);
      } else
        bref8 = NULL;
    } else
      bref8 = NULL;

    jit_ldxi_i(JIT_R2, JIT_R2, &((Scheme_Struct_Type *)0x0)->name_pos);
    if (type_pos < 0) {
      jit_ldxi_i(JIT_V1, JIT_V1, &((Scheme_Struct_Type *)0x0)->name_pos);
      /* Now R2 is argument depth, V1 is target depth */
      if (kind == 1) {
        bref3 = jit_bltr_i(jit_forward(), JIT_R2, JIT_V1);
      } else {
        (void)jit_bltr_i(refslow2, JIT_R2, JIT_V1);
        bref3 = NULL;
      }
    } else {
      if (type_pos != 0) {
        if (kind == 1) {
          bref3 = jit_blti_i(jit_forward(), JIT_R2, type_pos);
        } else {
          (void)jit_blti_i(refslow2, JIT_R2, type_pos);
          bref3 = NULL;
        }
      } else
        bref3 = NULL;
    }
    CHECK_LIMIT();
    /* Lookup argument type at target type depth, put it in R2: */
    if (type_pos < 0) {
      jit_lshi_ul(JIT_R2, JIT_V1, JIT_LOG_WORD_SIZE);
      jit_addi_p(JIT_R2, JIT_R2, &((Scheme_Struct_Type *)0x0)->parent_types);
    }
  } else {
    bref3 = NULL;
    bref8 = NULL;
  }
  jit_ldxi_p(JIT_V1, JIT_R1, &((Scheme_Structure *)0x0)->stype);
  if (type_pos < 0) {
    jit_ldxr_p(JIT_R2, JIT_V1, JIT_R2);
  } else {
    jit_ldxi_p(JIT_R2, JIT_V1, (type_pos << JIT_LOG_WORD_SIZE) + (intptr_t)&(((Scheme_Struct_Type *)0x0)->parent_types));
  }
  CHECK_LIMIT();

  /* (Re-)load target type into V1: */
  jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Primitive_Closure *)0x0)->val);

  if (kind == 1) {
    bref4 = jit_bner_p(jit_forward(), JIT_R2, JIT_V1);

    /* True branch: */
    if (!for_branch) {
      (void)jit_movi_p(JIT_R0, scheme_true);
    } else if (branch_info) {
      scheme_branch_for_true(jitter, branch_info);
    } else {
      mz_patch_ucbranch(bref_true);
#ifdef MZ_USE_JIT_I386
# ifndef X86_ALIGN_STACK
      jit_popr_p(JIT_V1);
# endif
#endif
    }
    if (pop_and_jump) {
      mz_epilog(JIT_V1);
      refdone = NULL;
    } else if (!for_branch) {
      __START_INNER_TINY__(1);
      refdone = jit_jmpi(jit_forward());
      __END_INNER_TINY__(1);
    } else {
      refdone = NULL;
    }
    CHECK_LIMIT();

    /* False branch: */
    if (branch_info) {
      scheme_add_branch_false(branch_info, bref1);
      scheme_add_branch_false(branch_info, bref2);
      if (bref3)
        scheme_add_branch_false(branch_info, bref3);
      scheme_add_branch_false(branch_info, bref4);
    } else {
      mz_patch_branch(bref1);
      mz_patch_branch(bref2);
      if (bref3)
        mz_patch_branch(bref3);
      mz_patch_branch(bref4);
      if (for_branch) {
        mz_patch_branch(bref_false);
        if (pop_and_jump) {
          restore_struct_temp(jitter, JIT_V1);
          mz_epilog_without_jmp();
        }
        jit_jmpr(JIT_V1);
      } else {
        (void)jit_movi_p(JIT_R0, scheme_false);
        if (pop_and_jump)
          mz_epilog(JIT_V1);
      }
      if (!pop_and_jump) {
        __START_INNER_TINY__(1);
        mz_patch_ucbranch(refdone);
        __END_INNER_TINY__(1);
      }
    }
  } else {
    (void)jit_bner_p(refslow2, JIT_R2, JIT_V1);
    bref4 = NULL;
    if (bref8) {
      __START_INNER_TINY__(1);
      mz_patch_branch(bref8);
      __END_INNER_TINY__(1);
    }
    /* Extract field */
    if (field_pos < 0) {
      jit_ldxi_p(JIT_V1, JIT_R0, &(((Scheme_Primitive_Closure *)0x0)->val[1]));
      jit_rshi_ul(JIT_V1, JIT_V1, 1);
      jit_lshi_ul(JIT_V1, JIT_V1, JIT_LOG_WORD_SIZE);
      jit_addi_p(JIT_V1, JIT_V1, &((Scheme_Structure *)0x0)->slots);
    } else {
      field_pos = (field_pos << JIT_LOG_WORD_SIZE) + (uintptr_t)&((Scheme_Structure *)0x0)->slots;
    }
    if (kind == 3) {
      restore_struct_temp(jitter, JIT_R0);
      if (field_pos < 0)
        jit_stxr_p(JIT_V1, JIT_R1, JIT_R0);
      else
        jit_stxi_p(field_pos, JIT_R1, JIT_R0);
      if (!result_ignored)
        (void)jit_movi_p(JIT_R0, scheme_void);
    } else {
      if (field_pos < 0)
        jit_ldxr_p(JIT_R0, JIT_R1, JIT_V1);
      else
        jit_ldxi_p(JIT_R0, JIT_R1, field_pos);
    }
    if (pop_and_jump)
      mz_epilog(JIT_V1);
  }
  CHECK_LIMIT();
      
  __END_SHORT_JUMPS__(branch_short);

  return 1;
}

static int common4(mz_jit_state *jitter, void *_data)
{
  int i, ii, iii;
  GC_CAN_IGNORE jit_insn *ref;

  /* *** {flvector}_{ref,set}_check_index_code *** */
  /* Same calling convention as for vector ops.    */
  for (iii = 0; iii < JIT_NUM_FL_KINDS; iii++) {
    for (i = 0; i < 3; i++) {
      void *code;

      code = jit_get_ip().ptr;

      if (!i) {
        sjc.flvector_ref_check_index_code[iii] = code;
      } else if (i == 1) {
        sjc.flvector_set_check_index_code[iii] = code;
      } else {
        sjc.flvector_set_flonum_check_index_code[iii] = code;
      }

      mz_prolog(JIT_R2);

      jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(2));
      CHECK_RUNSTACK_OVERFLOW();
      jit_str_p(JIT_RUNSTACK, JIT_R0);
      jit_stxi_p(WORDS_TO_BYTES(1), JIT_RUNSTACK, JIT_R1);
      if (!i) {
        jit_movi_i(JIT_R1, 2);
      } else {
        /* In set mode, value was already on run stack 
           or in FP register */
        jit_movi_i(JIT_R1, 3);
        if (i == 2) {
          /* need to box flonum */
          scheme_generate_alloc_double(jitter, 1, JIT_R0);
          jit_stxi_p(WORDS_TO_BYTES(2), JIT_RUNSTACK, JIT_R0);
        }
      }
      CHECK_LIMIT();
      JIT_UPDATE_THREAD_RSPTR();
      jit_prepare(2);
      jit_pusharg_p(JIT_RUNSTACK);
      jit_pusharg_i(JIT_R1);
      if (!i) {
        MZ_FPUSEL_STMT(iii,
                       (void)mz_finish_lwe(ts_scheme_checked_extflvector_ref, ref),
                       (void)mz_finish_lwe(ts_scheme_checked_flvector_ref, ref));
      } else {
        MZ_FPUSEL_STMT(iii,
                       (void)mz_finish_lwe(ts_scheme_checked_extflvector_set, ref),
                       (void)mz_finish_lwe(ts_scheme_checked_flvector_set, ref));
      }
      /* does not return */
      CHECK_LIMIT();

      scheme_jit_register_sub_func(jitter, code, scheme_false);
    }
  }

  /* *** struct_raw_{ref,set}_code *** */
  /* R0 is struct, R1 is index (Scheme number).
     In set mode, value is on run stack. */
  for (iii = 0; iii < 2; iii++) { /* ref, set */
    void *code;

    code = jit_get_ip().ptr;
    
    if (!iii) {
      sjc.struct_raw_ref_code = code;
    } else {
      sjc.struct_raw_set_code = code;
    }

    mz_prolog(JIT_R2);
    jit_rshi_ul(JIT_R1, JIT_R1, 1);
    JIT_UPDATE_THREAD_RSPTR();
    if (!iii)
      jit_prepare(2);
    else {
      jit_ldr_p(JIT_R2, JIT_RUNSTACK);
      jit_prepare(3);
      jit_pusharg_p(JIT_R2);
    }
    jit_pusharg_p(JIT_R1);
    jit_pusharg_i(JIT_R0);
    if (!iii) {
      (void)mz_finish_lwe(ts_scheme_struct_ref, ref);
      jit_retval(JIT_R0);
    } else
      (void)mz_finish_lwe(ts_scheme_struct_set, ref);
    CHECK_LIMIT();
    jit_retval(JIT_R0);
    mz_epilog(JIT_R2);

    scheme_jit_register_sub_func(jitter, code, scheme_false);
  }

  /* *** syntax_e_code *** */
  /* R0 is (potential) syntax object */
  {
    GC_CAN_IGNORE jit_insn *ref, *reffail, *refrts;
    sjc.syntax_e_code = jit_get_ip().ptr;
    __START_TINY_JUMPS__(1);
    mz_prolog(JIT_R2);

    ref = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);

    reffail = _jit.x.pc;
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
    CHECK_RUNSTACK_OVERFLOW();
    jit_str_p(JIT_RUNSTACK, JIT_R0);
    jit_movi_i(JIT_R1, 1);
    JIT_UPDATE_THREAD_RSPTR();
    CHECK_LIMIT();
    jit_prepare(2);
    jit_pusharg_p(JIT_RUNSTACK);
    jit_pusharg_i(JIT_R1);
    (void)mz_finish_lwe(ts_scheme_checked_syntax_e, refrts);
    jit_retval(JIT_R0);
    jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
    mz_epilog(JIT_R2);
    CHECK_LIMIT();
    
    /* It's not a fixnum... */
    mz_patch_branch(ref);
    (void)mz_bnei_t(reffail, JIT_R0, scheme_stx_type, JIT_R2);
    
    /* It's a syntax object... needs to propagate? */
    jit_ldxi_l(JIT_R2, JIT_R0, &((Scheme_Stx *)0x0)->u.lazy_prefix);
    ref = jit_beqi_l(jit_forward(), JIT_R2, 0x0);
    CHECK_LIMIT();

    /* Maybe needs to propagate; check STX_SUBSTX_FLAG flag */
    jit_ldxi_s(JIT_R2, JIT_R0, &MZ_OPT_HASH_KEY(&((Scheme_Stx *)0x0)->iso));
    (void)jit_bmsi_ul(reffail, JIT_R2, STX_SUBSTX_FLAG);
    
    /* Maybe needs taint handling; check STX_ARMED_FLAG flag */
    mz_patch_branch(ref);
    jit_ldxi_s(JIT_R2, JIT_R0, &MZ_OPT_HASH_KEY(&((Scheme_Stx *)0x0)->iso));
    (void)jit_bmsi_ul(reffail, JIT_R2, STX_ARMED_FLAG);
    
    /* No propagations or dye packs. Extract value. */
    jit_ldxi_p(JIT_R0, JIT_R0, &((Scheme_Stx *)0x0)->val);

    mz_epilog(JIT_R2);
    CHECK_LIMIT();
    __END_TINY_JUMPS__(1);
  }

  /* *** struct_{pred,get,set}[_branch,_multi,_tail]_code *** */
  /* R0 is (potential) struct proc, R1 is (potential) struct. */
  /* In branch mode, V1 is target address for false branch. */
  /* In set mode, V1 is value to install.                   */
  for (ii = 0; ii < 3; ii++) { /* single, multi, or tail */
    for (i = 0; i < 4; i++) { /* pred, pred_branch, get, or set */
      void *code;
      int kind, for_branch;
      GC_CAN_IGNORE jit_insn *ref, *refslow, *refslow2;
      GC_CAN_IGNORE jit_insn *bref5, *bref6;

      if ((ii == 1) && (i == 1)) continue; /* no multi variant of pred branch */
      if ((ii == 2) && (i == 1)) continue; /* no tail variant of pred branch */

      code = jit_get_ip().ptr;

      if (!i) {
	kind = 1;
	for_branch = 0;
        if (ii == 2) 
          sjc.struct_pred_tail_code = jit_get_ip().ptr;
        else if (ii == 1) 
          sjc.struct_pred_multi_code = jit_get_ip().ptr;
        else
          sjc.struct_pred_code = jit_get_ip().ptr;
      } else if (i == 1) {
	kind = 1;
	for_branch = 1;
        sjc.struct_pred_branch_code = jit_get_ip().ptr;
	/* Save target address for false branch: */
        save_struct_temp(jitter, JIT_V1);
      } else if (i == 2) {
	kind = 2;
	for_branch = 0;
        if (ii == 2) 
          sjc.struct_get_tail_code = jit_get_ip().ptr;
        else if (ii == 1) 
          sjc.struct_get_multi_code = jit_get_ip().ptr;
        else
          sjc.struct_get_code = jit_get_ip().ptr;
      } else {
	kind = 3;
	for_branch = 0;
        if (ii == 2) 
          sjc.struct_set_tail_code = jit_get_ip().ptr;
        else if (ii == 1) 
          sjc.struct_set_multi_code = jit_get_ip().ptr;
        else
          sjc.struct_set_code = jit_get_ip().ptr;
        /* Save value to install: */
        save_struct_temp(jitter, JIT_V1);
      }

      mz_prolog(JIT_V1);

      __START_SHORT_JUMPS__(1);

      ref = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);
      CHECK_LIMIT();

      /* Slow path: non-struct proc. */
      refslow = _jit.x.pc;
      gen_struct_slow(jitter, kind, 0, for_branch, ii == 2, ii == 1, &bref5, &bref6);
      CHECK_LIMIT();

      if ((kind == 2) || (kind == 3)) {
        /* Slow path: argument type is bad for a getter/setter. */
        refslow2 = _jit.x.pc;
        gen_struct_slow(jitter, kind, 1, 0, 0, 0, NULL, NULL);
        CHECK_LIMIT();
      } else
        refslow2 = refslow;

      /* Continue trying fast path: check proc */
      mz_patch_branch(ref);
      __END_SHORT_JUMPS__(1);

      scheme_generate_struct_op(jitter, kind, for_branch, NULL, 1, 0,
                                1, 1, -1, -1,
                                1, refslow, refslow2, bref5, bref6);
      CHECK_LIMIT();

      scheme_jit_register_sub_func(jitter, code, scheme_false);
    }
  }

  return 1;
}

static int common4b(mz_jit_state *jitter, void *_data)
{
  int i, ii;

  /* *** struct_prop_{pred,get[_defl]}_[multi_,tail_]code *** */
  /* R0 is (potential) struct-prop proc, R1 is (potential) struct.
     If defl_, V1 is second argument for default value. */
  for (i = 0; i < 3; i++) {
    for (ii = 0; ii < 3; ii++) { /* single, multi, or tail */
      void *code;
      GC_CAN_IGNORE jit_insn *ref, *ref2, *ref3, *refno, *refslow, *refloop;

      code = jit_get_ip().ptr;
      
      if (i == 0) {
        if (ii == 2) 
          sjc.struct_prop_get_tail_code = code;
        else if (ii == 1) 
          sjc.struct_prop_get_multi_code = code;
        else
          sjc.struct_prop_get_code = code;
      } else if (i == 1) {
        if (ii == 2) 
          sjc.struct_prop_get_defl_tail_code = code;
        else if (ii == 1) 
          sjc.struct_prop_get_defl_multi_code = code;
        else
          sjc.struct_prop_get_defl_code = code;
      } else if (i == 2) {
        if (ii == 2) 
          sjc.struct_prop_pred_tail_code = code;
        else if (ii == 1) 
          sjc.struct_prop_pred_multi_code = code;
        else
          sjc.struct_prop_pred_code = code;
      }
    
      mz_prolog(JIT_R2);

      if (i == 1) {
        /* push second argument now, since we don't have a better
           place to keep it */
        jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
        jit_str_p(JIT_RUNSTACK, JIT_V1);
      }
    
      __START_SHORT_JUMPS__(1);

      ref = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);
      CHECK_LIMIT();

      /* Slow path: non-struct-prop proc, or argument type is
         bad for a getter. */
      refslow = _jit.x.pc;
      jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
      CHECK_RUNSTACK_OVERFLOW();
      JIT_UPDATE_THREAD_RSPTR();
      jit_str_p(JIT_RUNSTACK, JIT_R1);
      jit_movi_i(JIT_V1, ((i == 1) ? 2 : 1));
      jit_prepare(3);
      jit_pusharg_p(JIT_RUNSTACK);
      jit_pusharg_i(JIT_V1);
      jit_pusharg_p(JIT_R0);
      if (ii == 2) {
        scheme_generate_finish_tail_apply(jitter);
      } else if (ii == 1) {
        scheme_generate_finish_multi_apply(jitter);
      } else {
        scheme_generate_finish_apply(jitter);
      }
      CHECK_LIMIT();
      jit_retval(JIT_R0);
      VALIDATE_RESULT(JIT_R0);
      if (i == 1) {
        /* second argument was pushed early */
        jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(2));
      } else {
        jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
      }
      JIT_UPDATE_THREAD_RSPTR();
      mz_epilog(JIT_V1);
      CHECK_LIMIT();
      if (i == 2) {
        refno = _jit.x.pc;
        (void)jit_movi_p(JIT_R0, scheme_false);
        mz_epilog(JIT_V1);
        CHECK_LIMIT();
      } else
        refno = refslow;

      /* Continue trying fast path: check proc */
      mz_patch_branch(ref);
      (void)mz_bnei_t(refslow, JIT_R0, scheme_prim_type, JIT_R2);
      jit_ldxi_s(JIT_R2, JIT_R0, &((Scheme_Primitive_Proc *)0x0)->pp.flags);
      (void)jit_bmci_i(refslow, JIT_R2, SCHEME_PRIM_TYPE_STRUCT_PROP_GETTER);
      CHECK_LIMIT();

      /* Check argument: */
      (void)jit_bmsi_ul(refno, JIT_R1, 0x1);
      jit_ldxi_s(JIT_R2, JIT_R1, &((Scheme_Object *)0x0)->type);
      __START_INNER_TINY__(1);
      ref2 = jit_beqi_i(jit_forward(), JIT_R2, scheme_structure_type);
      __END_INNER_TINY__(1);
      if (i == 2) {
        (void)jit_beqi_i(refslow, JIT_R2, scheme_proc_chaperone_type);
        (void)jit_beqi_i(refslow, JIT_R2, scheme_chaperone_type);
        (void)jit_beqi_i(refslow, JIT_R2, scheme_struct_type_type);
      }
      (void)jit_bnei_i(refno, JIT_R2, scheme_proc_struct_type);
      __START_INNER_TINY__(1);
      mz_patch_branch(ref2);
      __END_INNER_TINY__(1);
      CHECK_LIMIT();

      /* Put argument struct type in R2, array prop count in V1: */
      jit_ldxi_p(JIT_R2, JIT_R1, &((Scheme_Structure *)0x0)->stype);
      jit_ldxi_i(JIT_V1, JIT_R2, &((Scheme_Struct_Type *)0x0)->num_props);
      CHECK_LIMIT();

      /* negative count means use the hash table (in the slow path);
         zero count means we've run out */
      if (i == 2) {
        (void)jit_blei_i(refslow, JIT_V1, 0);
      }
      refloop = _jit.x.pc;
      (void)jit_blei_i(refno, JIT_V1, 0);
      jit_subi_i(JIT_V1, JIT_V1, 1);
      mz_set_local_p(JIT_V1, JIT_LOCAL3);

      jit_ldxi_p(JIT_R2, JIT_R1, &((Scheme_Structure *)0x0)->stype);
      jit_ldxi_p(JIT_R2, JIT_R2, &((Scheme_Struct_Type *)0x0)->props);
      jit_lshi_ul(JIT_V1, JIT_V1, JIT_LOG_WORD_SIZE);
      jit_ldxr_p(JIT_R2, JIT_R2, JIT_V1);
      /* extract car of table entry, which is the key: */
      (void)jit_ldxi_p(JIT_R2, JIT_R2, &((Scheme_Simple_Object *)0x0)->u.pair_val.car);
      CHECK_LIMIT();

      /* target struct-type property in V1 */
      jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Primitive_Closure *)0x0)->val);
    
      ref3 = jit_beqr_p(jit_forward(), JIT_R2, JIT_V1);
    
      mz_get_local_p(JIT_V1, JIT_LOCAL3);
      (void)jit_jmpi(refloop);

      /* Success! */
      mz_patch_branch(ref3);

      if (i == 2) {
        (void)jit_movi_p(JIT_R0, scheme_true);
      } else {
        if (i == 1) {
          /* pop second argument, which we don't need */
          jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
        }

        /* same as above, but get the cdr this time: */
        mz_get_local_p(JIT_V1, JIT_LOCAL3);
        jit_ldxi_p(JIT_R2, JIT_R1, &((Scheme_Structure *)0x0)->stype);
        jit_ldxi_p(JIT_R2, JIT_R2, &((Scheme_Struct_Type *)0x0)->props);
        jit_lshi_ul(JIT_V1, JIT_V1, JIT_LOG_WORD_SIZE);
        jit_ldxr_p(JIT_R2, JIT_R2, JIT_V1);
        (void)jit_ldxi_p(JIT_R0, JIT_R2, &((Scheme_Simple_Object *)0x0)->u.pair_val.cdr);
      }
      CHECK_LIMIT();

      mz_epilog(JIT_V1);

      __END_SHORT_JUMPS__(1);

      scheme_jit_register_sub_func(jitter, code, scheme_false);
    }
  }

  return 1;
}

static int common4c(mz_jit_state *jitter, void *_data)
{
  int i, ii;

  /* We can't use this code if inline alloc isn't supported, but make it
     compiled in that mode, anyway. */

  /* *** struct_constr_{unary,binary,nary}_[multi_,tail_]code *** */
  /* For unary case, rator is in R0, R1 is argument.
     For binary case, rator is in R0, R1 is first argument, V1 is second argument.
     For nary case, rator in R0, args on are on runstack, R1 has the count. */
  for (i = 0; i < 3; i++) { /* unary, binary, or nary */
    for (ii = 0; ii < 3; ii++) { /* single, multi, or tail */
      void *code;
      int num_args;

      code = jit_get_ip().ptr;
      
      if (i == 0) {
        if (ii == 2) 
          sjc.struct_constr_unary_tail_code = code;
        else if (ii == 1) 
          sjc.struct_constr_unary_multi_code = code;
        else
          sjc.struct_constr_unary_code = code;
        num_args = 1;
      } else if (i == 1) {
        if (ii == 2) 
          sjc.struct_constr_binary_tail_code = code;
        else if (ii == 1) 
          sjc.struct_constr_binary_multi_code = code;
        else
          sjc.struct_constr_binary_code = code;
        num_args = 2;
      } else if (i == 2) {
        if (ii == 2) 
          sjc.struct_constr_nary_tail_code = code;
        else if (ii == 1) 
          sjc.struct_constr_nary_multi_code = code;
        else
          sjc.struct_constr_nary_code = code;
        num_args =-1;
      } else
        num_args = 0;
    
      scheme_generate_struct_alloc(jitter, num_args, 1, 1, ii == 2, ii == 1, JIT_R0);

      CHECK_LIMIT();

      scheme_jit_register_sub_func(jitter, code, scheme_false);
    }
  }

  return 1;
}

static int common5(mz_jit_state *jitter, void *_data)
{
  int i, ii, iii;

#ifdef MZ_LONG_DOUBLE
# define END_OF_I 4
#else
# define END_OF_I 3
#endif

#ifdef CAN_INLINE_ALLOC
  /* *** retry_alloc_code[{_keep_r0_r1,_keep_fpr1}] *** */
  for (i = 0; i < END_OF_I; i++) { 
    if (!i)
      sjc.retry_alloc_code = jit_get_ip().ptr;
    else if (i == 1)
      sjc.retry_alloc_code_keep_r0_r1 = jit_get_ip().ptr;
    else if (i == 2) {
      sjc.retry_alloc_code_keep_fpr1 = jit_get_ip().ptr;
    }
#ifdef MZ_LONG_DOUBLE
    else if (i == 3) {
      sjc.retry_alloc_code_keep_extfpr1 = jit_get_ip().ptr;
    }
#endif

    mz_prolog(JIT_V1);
    scheme_generate_alloc_retry(jitter, i);
    CHECK_LIMIT();
    mz_epilog(JIT_V1);
    CHECK_LIMIT();
  }
#endif

#ifdef CAN_INLINE_ALLOC
  /* *** make_list_code *** */
  /* R2 has length, args are on runstack */
  for (i = 0; i < 2; i++) {
    GC_CAN_IGNORE jit_insn *ref, *refnext;

    if (i == 0)
      sjc.make_list_code = jit_get_ip().ptr;  
    else
      sjc.make_list_star_code = jit_get_ip().ptr;  
    mz_prolog(JIT_R1);
    jit_lshi_l(JIT_R2, JIT_R2, JIT_LOG_WORD_SIZE);
    if (i == 0)
      (void)jit_movi_p(JIT_R0, &scheme_null);
    else {
      jit_subi_l(JIT_R2, JIT_R2, JIT_WORD_SIZE);
      jit_ldxr_p(JIT_R0, JIT_RUNSTACK, JIT_R2);
    }

    __START_SHORT_JUMPS__(1);
    ref = jit_beqi_l(jit_forward(), JIT_R2, 0);
    refnext = _jit.x.pc;
    __END_SHORT_JUMPS__(1);
    CHECK_LIMIT();

    jit_subi_l(JIT_R2, JIT_R2, JIT_WORD_SIZE);
    jit_ldxr_p(JIT_R1, JIT_RUNSTACK, JIT_R2);
    mz_set_local_p(JIT_R2, JIT_LOCAL3);

    scheme_generate_cons_alloc(jitter, 1, 1, !i, JIT_R0);
    CHECK_LIMIT();

    mz_get_local_p(JIT_R2, JIT_LOCAL3);

    __START_SHORT_JUMPS__(1);
    (void)jit_bnei_l(refnext, JIT_R2, 0);
    mz_patch_branch(ref);
    __END_SHORT_JUMPS__(1);

    mz_epilog(JIT_R1);
  }
#endif

  /* *** box_flonum_from_stack_code *** */
  /* R0 has offset from frame pointer to double on stack */
  {
    sjc.box_flonum_from_stack_code = jit_get_ip().ptr;

    mz_prolog(JIT_R2);

    JIT_UPDATE_THREAD_RSPTR();

    jit_movr_p(JIT_R1, JIT_FP);
    jit_ldxr_d_fppush(JIT_FPR0, JIT_R1, JIT_R0);
    scheme_generate_alloc_double(jitter, 1, JIT_R0);
    CHECK_LIMIT();
    
    mz_epilog(JIT_R2);
  }

  /* *** box_flonum_from_reg_code *** */
  /* JIT_FPR2 (reg-based) or JIT_FPR0 (stack-based) has value */
  {
    sjc.box_flonum_from_reg_code = jit_get_ip().ptr;

    mz_prolog(JIT_R2);

    JIT_UPDATE_THREAD_RSPTR();

#ifdef DIRECT_FPR_ACCESS
    jit_movr_d(JIT_FPR0, JIT_FPR2);
#endif

    scheme_generate_alloc_double(jitter, 1, JIT_R0);
    CHECK_LIMIT();
    
    mz_epilog(JIT_R2);
  }

  /* *** box_extflonum_from_reg_code *** */
  /* JIT_FPU_FPR2 (reg-based) or JIT_FPU_FPR0 (stack-based) has value */
#ifdef MZ_LONG_DOUBLE
  {
    sjc.box_extflonum_from_reg_code = jit_get_ip().ptr;

    mz_prolog(JIT_R2);

    JIT_UPDATE_THREAD_RSPTR();

#ifdef DISABLED_DIRECT_FPR_ACCESS
    jit_fpu_movr_ld(JIT_FPU_FPR0, JIT_FPU_FPR2);
#endif

    scheme_generate_alloc_long_double(jitter, 1, JIT_R0);
    CHECK_LIMIT();
    
    mz_epilog(JIT_R2);
  }
#endif

  /* *** fl1_fail_code *** */
  /* R0 has argument, V1 has primitive proc */
  for (iii = 0; iii < JIT_NUM_FL_KINDS; iii++) {
    sjc.fl1_fail_code[iii] = jit_get_ip().ptr;

    mz_prolog(JIT_R2);

    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
    JIT_UPDATE_THREAD_RSPTR();
    jit_str_p(JIT_RUNSTACK, JIT_R0);
    
    jit_movi_i(JIT_R1, 1);
    CHECK_LIMIT();

    mz_prepare_direct_prim(2);
    {
      mz_generate_direct_prim(jit_pusharg_p(JIT_RUNSTACK),
                              jit_pusharg_i(JIT_R1),
                              JIT_V1, scheme_noncm_prim_indirect);
      CHECK_LIMIT();
    }

    scheme_jit_register_sub_func(jitter, sjc.fl1_fail_code[iii], scheme_false);
  }

  /* *** fl2{rf}{rf}_fail_code *** */
  /* R0 and/or R1 have arguments, V1 has primitive proc,
     non-register argument is in FPR0 */
  for (iii = 0; iii < JIT_NUM_FL_KINDS; iii++) {
    for (ii = 0; ii < 2; ii++) {
      for (i = 0; i < 3; i++) {
        void *code;
        int a0, a1;

        code = jit_get_ip().ptr;
        switch (i) {
        case 0:
          sjc.fl2rr_fail_code[ii][iii] = code;
          break;
        case 1:
          sjc.fl2fr_fail_code[ii][iii] = code;
          break;
        case 2:
          sjc.fl2rf_fail_code[ii][iii] = code;
          break;
        }

        if (!ii) {
          a0 = 0; a1 = 1;
        } else {
          a0 = 1; a1 = 0;
        }

        mz_prolog(JIT_R2);

        jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(2));
        JIT_UPDATE_THREAD_RSPTR();
        if ((i == 0) || (i == 2))
          jit_stxi_p(WORDS_TO_BYTES(a0), JIT_RUNSTACK, JIT_R0);
        else
          jit_stxi_p(WORDS_TO_BYTES(a0), JIT_RUNSTACK, JIT_V1);
        if ((i == 0) || (i == 1))
          jit_stxi_p(WORDS_TO_BYTES(a1), JIT_RUNSTACK, JIT_R1);
        else
          jit_stxi_p(WORDS_TO_BYTES(a1), JIT_RUNSTACK, JIT_V1);

        if (i != 0) {
          MZ_FPUSEL_STMT(iii,
                         scheme_generate_alloc_long_double(jitter, 1, JIT_R0),
                         scheme_generate_alloc_double(jitter, 1, JIT_R0));
          CHECK_LIMIT();
          if (i == 1) {
            jit_ldxi_p(JIT_V1, JIT_RUNSTACK, WORDS_TO_BYTES(a0));
            jit_stxi_p(WORDS_TO_BYTES(a0), JIT_RUNSTACK, JIT_R0);
          } else {
            jit_ldxi_p(JIT_V1, JIT_RUNSTACK, WORDS_TO_BYTES(a1));
            jit_stxi_p(WORDS_TO_BYTES(a1), JIT_RUNSTACK, JIT_R0);
          }
        }
    
        jit_movi_i(JIT_R1, 2);
        CHECK_LIMIT();
    
        mz_prepare_direct_prim(2);
        {
          mz_generate_direct_prim(jit_pusharg_p(JIT_RUNSTACK),
                                  jit_pusharg_i(JIT_R1),
                                  JIT_V1, scheme_noncm_prim_indirect);
          CHECK_LIMIT();
        }

        scheme_jit_register_sub_func(jitter, code, scheme_false);
      }
    }
  }

  return 1;
}

static int common6(mz_jit_state *jitter, void *_data)
{
  /* wcm_[nontail_]code */
  /* key and value are on runstack */
  {
    GC_CAN_IGNORE jit_insn *refloop, *ref, *ref2, *ref3, *ref4, *ref5, *ref7, *ref8;

    sjc.wcm_code = jit_get_ip().ptr;

    mz_prolog(JIT_R2);

    (void)mz_tl_ldi_p(JIT_R2, tl_scheme_current_cont_mark_stack);
    /* R2 has counter for search */

    refloop = _jit.x.pc;
    (void)mz_tl_ldi_p(JIT_R1, tl_scheme_current_thread);
    jit_ldxi_l(JIT_R0, JIT_R1, &((Scheme_Thread *)0x0)->cont_mark_stack_bottom);
    ref = jit_bler_i(jit_forward(), JIT_R2, JIT_R0); /* => double-check meta-continuation */
    CHECK_LIMIT();

    jit_subi_l(JIT_R2, JIT_R2, 1);

    jit_ldxi_p(JIT_R0, JIT_R1, &((Scheme_Thread *)0x0)->cont_mark_stack_segments);
    jit_rshi_l(JIT_V1, JIT_R2, SCHEME_LOG_MARK_SEGMENT_SIZE);
    jit_lshi_l(JIT_V1, JIT_V1, JIT_LOG_WORD_SIZE);
    jit_ldxr_p(JIT_R0, JIT_R0, JIT_V1); /* R0 now points to the right array */
    CHECK_LIMIT();
    
    jit_andi_l(JIT_V1, JIT_R2, SCHEME_MARK_SEGMENT_MASK);
    jit_movi_l(JIT_R1, sizeof(Scheme_Cont_Mark));
    jit_mulr_l(JIT_V1, JIT_V1, JIT_R1);
    jit_addr_l(JIT_R0, JIT_R0, JIT_V1);
    CHECK_LIMIT();
    /* R0 now points to the right record */
    
    (void)mz_tl_ldi_l(JIT_R1, tl_scheme_current_cont_mark_pos);
    jit_ldxi_l(JIT_V1, JIT_R0, &((Scheme_Cont_Mark *)0x0)->pos);
    ref2 = jit_bltr_l(jit_forward(), JIT_V1, JIT_R1); /* => try to allocate new slot */

    jit_ldxi_p(JIT_R1, JIT_RUNSTACK, WORDS_TO_BYTES(1));
    jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Cont_Mark *)0x0)->key);
    ref3 = jit_beqr_p(jit_forward(), JIT_V1, JIT_R1); /* => found right destination */

    /* Assume that we'll find a record and mutate it. (See scheme_set_cont_mark().) */
    (void)jit_movi_p(JIT_R1, NULL);
    jit_stxi_p(&((Scheme_Cont_Mark *)0x0)->cache, JIT_R0, JIT_R1);    

    CHECK_LIMIT();
    (void)jit_jmpi(refloop); 

    /* Double-check meta-continuation */
    /* R1 has thread pointer */
    mz_patch_branch(ref);
    jit_ldxi_l(JIT_R0, JIT_R1, &((Scheme_Thread *)0x0)->cont_mark_pos_bottom);
    (void)mz_tl_ldi_l(JIT_R2, tl_scheme_current_cont_mark_pos);
    jit_subi_l(JIT_R2, JIT_R2, 2);
    ref = jit_bner_i(jit_forward(), JIT_R2, JIT_R0); /* => try to allocate new slot */
    jit_ldxi_p(JIT_R1, JIT_R1, &((Scheme_Thread *)0x0)->meta_continuation);
    ref7 = jit_beqi_l(jit_forward(), JIT_R1, NULL); /* => try to allocate new slot */
    /* we need to check a meta-continuation... take the slow path. */
    ref8 = jit_jmpi(jit_forward());
    CHECK_LIMIT();

    /* Entry point when we know we're not in non-tail position with respect
       to any enclosing wcm: */
    sjc.wcm_nontail_code = jit_get_ip().ptr;
    mz_prolog(JIT_R2);

    /* Try to allocate new slot: */
    mz_patch_branch(ref);
    mz_patch_branch(ref2);
    mz_patch_branch(ref7);
    (void)mz_tl_ldi_p(JIT_R2, tl_scheme_current_cont_mark_stack);
    jit_rshi_l(JIT_V1, JIT_R2, SCHEME_LOG_MARK_SEGMENT_SIZE - JIT_LOG_WORD_SIZE);
    (void)mz_tl_ldi_p(JIT_R1, tl_scheme_current_thread);
    jit_ldxi_l(JIT_R0, JIT_R1, &((Scheme_Thread *)0x0)->cont_mark_seg_count);
    ref4 = jit_bger_i(jit_forward(), JIT_V1, JIT_R0); /* => take slow path */
    CHECK_LIMIT();
    
    jit_ldxi_p(JIT_R0, JIT_R1, &((Scheme_Thread *)0x0)->cont_mark_stack_segments);
    jit_rshi_l(JIT_V1, JIT_R2, SCHEME_LOG_MARK_SEGMENT_SIZE);
    jit_lshi_l(JIT_V1, JIT_V1, JIT_LOG_WORD_SIZE);
    jit_ldxr_p(JIT_R0, JIT_R0, JIT_V1);
    CHECK_LIMIT();
    /* R0 now points to the right array */
    
    jit_andi_l(JIT_V1, JIT_R2, SCHEME_MARK_SEGMENT_MASK);
    jit_movi_l(JIT_R1, sizeof(Scheme_Cont_Mark));
    jit_mulr_l(JIT_V1, JIT_V1, JIT_R1);
    jit_addr_l(JIT_R0, JIT_R0, JIT_V1);
    CHECK_LIMIT();
    /* R0 now points to the right record */

    /* Increment counter: */
    jit_addi_l(JIT_R2, JIT_R2, 1);
    mz_tl_sti_p(tl_scheme_current_cont_mark_stack, JIT_R2, JIT_R1);

    /* Fill in record at R0: */
    mz_patch_branch(ref3);
    (void)mz_tl_ldi_l(JIT_R1, tl_scheme_current_cont_mark_pos);
    jit_stxi_l(&((Scheme_Cont_Mark *)0x0)->pos, JIT_R0, JIT_R1);
    jit_ldxi_p(JIT_R1, JIT_RUNSTACK, WORDS_TO_BYTES(1));
    jit_stxi_p(&((Scheme_Cont_Mark *)0x0)->key, JIT_R0, JIT_R1);
    jit_ldxi_p(JIT_R1, JIT_RUNSTACK, WORDS_TO_BYTES(0));
    jit_stxi_p(&((Scheme_Cont_Mark *)0x0)->val, JIT_R0, JIT_R1);
    (void)jit_movi_p(JIT_R1, NULL);
    jit_stxi_p(&((Scheme_Cont_Mark *)0x0)->cache, JIT_R0, JIT_R1);
    CHECK_LIMIT();

    /* return: */
    ref5 = _jit.x.pc;
    mz_epilog(JIT_R2);
    
    /* slow path: */

    mz_patch_branch(ref4);
    mz_patch_ucbranch(ref8);
    JIT_UPDATE_THREAD_RSPTR();

    jit_ldxi_p(JIT_R0, JIT_RUNSTACK, WORDS_TO_BYTES(0));
    jit_ldxi_p(JIT_V1, JIT_RUNSTACK, WORDS_TO_BYTES(1));
    CHECK_LIMIT();
    
    mz_prepare(2);
    jit_pusharg_p(JIT_R0);
    jit_pusharg_p(JIT_V1);
    (void)mz_finish(scheme_set_cont_mark);
    CHECK_LIMIT();

    (void)jit_jmpi(ref5);

    scheme_jit_register_sub_func(jitter, sjc.wcm_code, scheme_false);
  }

  /* wcm_chaperone */
  /* key and value are on runstack and are updated there */
  {
    GC_CAN_IGNORE jit_insn *ref2;
    sjc.wcm_chaperone = jit_get_ip().ptr;

    mz_prolog(JIT_R2);
    JIT_UPDATE_THREAD_RSPTR();
    jit_prepare(0);
    (void)mz_finish_lwe(ts_chaperone_set_mark, ref2);
    mz_epilog(JIT_R2);

    scheme_jit_register_sub_func(jitter, sjc.wcm_chaperone, scheme_false);
  }

  return 1;
}

static int common7(mz_jit_state *jitter, void *_data)
{
  int i;

  /* list_p_[branch_]code */
  /* argument is in R0, and it's a pair */
  /* for branch, V1 holds return address for false */
  for (i = 0; i < 2; i++) {
    GC_CAN_IGNORE void *code;
    GC_CAN_IGNORE jit_insn *refloop, *ref1, *ref2, *ref3, *ref4;
    GC_CAN_IGNORE jit_insn *ref5, *ref6, *ref7, *ref8;

    code = jit_get_ip().ptr;
    if (!i)
      sjc.list_p_code = code;
    else
      sjc.list_p_branch_code = code;

    mz_prolog(JIT_R2);
    
    __START_SHORT_JUMPS__(1);

    /* R0 is hare, R1 is turtle */
    jit_movr_p(JIT_R1, JIT_R0);
    CHECK_LIMIT();
    
    /* Note: there's no fuel check in this loop, just like there isn't in
       scheme_is_list(). */

    refloop = _jit.x.pc;
    jit_ldxi_s(JIT_R2, JIT_R0, &MZ_OPT_HASH_KEY(&((Scheme_Stx *)0x0)->iso));
    ref1 = jit_bmsi_ul(jit_forward(), JIT_R2, PAIR_FLAG_MASK);
    
    jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&SCHEME_CDR(0x0));
    ref2 = jit_beqi_p(jit_forward(), JIT_R0, scheme_null);    
    ref8 = jit_bmsi_l(jit_forward(), JIT_R0, 0x1);

    ref3 = mz_bnei_t(jit_forward(), JIT_R0, scheme_pair_type, JIT_R2);
    CHECK_LIMIT();

    jit_ldxi_s(JIT_R2, JIT_R0, &MZ_OPT_HASH_KEY(&((Scheme_Stx *)0x0)->iso));
    ref4 = jit_bmsi_ul(jit_forward(), JIT_R2, PAIR_FLAG_MASK);
    
    jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&SCHEME_CDR(0x0));
    jit_ldxi_p(JIT_R1, JIT_R1, (intptr_t)&SCHEME_CDR(0x0));
    ref5 = jit_beqi_p(jit_forward(), JIT_R0, scheme_null);    
    ref7 = jit_bmsi_l(jit_forward(), JIT_R0, 0x1);

    (void)mz_beqi_t(refloop, JIT_R0, scheme_pair_type, JIT_R2);

    ref6 = jit_jmpi(jit_forward());
    CHECK_LIMIT();
    
    /* R2 has flags, and either list or non-list is set */
    mz_patch_branch(ref1);
    mz_patch_branch(ref4);
    ref1 = jit_bmci_ul(jit_forward(), JIT_R2, PAIR_IS_LIST);
    
    /* it's a list: */
    mz_patch_branch(ref2);
    mz_patch_branch(ref5);

    jit_ldxi_s(JIT_R2, JIT_R1, &MZ_OPT_HASH_KEY(&((Scheme_Stx *)0x0)->iso));
#ifdef MZ_USE_FUTURES
    if (scheme_is_multithreaded(0)) {
      /* Need an atomic update in case another thread is setting
         a hash code on the target pair. */
      ref5 = jit_bmsi_i(jit_forward(), JIT_R2, PAIR_IS_LIST);
      jit_movr_i(JIT_R0, JIT_R2);
      jit_ori_i(JIT_R2, JIT_R2, PAIR_IS_LIST);
      jit_addi_p(JIT_R1, JIT_R1, &MZ_OPT_HASH_KEY(&((Scheme_Stx *)0x0)->iso));
      /* In the unlikely case that the compare-and-swap fails, then it's ok to 
         lose the caching of the list bit: */
      jit_lock_cmpxchgr_s(JIT_R1, JIT_R2); /* implicitly uses JIT_R0 */
      mz_patch_branch(ref5);
    } else 
#endif
      {
        jit_ori_i(JIT_R2, JIT_R2, PAIR_IS_LIST);
        jit_stxi_s(&MZ_OPT_HASH_KEY(&((Scheme_Stx *)0x0)->iso), JIT_R1, JIT_R2);
      }

    __END_SHORT_JUMPS__(1);
    CHECK_LIMIT();

    if (!i)
      (void)jit_movi_p(JIT_R0, scheme_true);
    mz_epilog(JIT_R2);

    __START_SHORT_JUMPS__(1);

    /* it's a non-list: */
    mz_patch_branch(ref1);
    mz_patch_branch(ref3);
    mz_patch_branch(ref7);
    mz_patch_branch(ref8);
    mz_patch_ucbranch(ref6);

    jit_ldxi_s(JIT_R2, JIT_R1, &MZ_OPT_HASH_KEY(&((Scheme_Stx *)0x0)->iso));
#ifdef MZ_USE_FUTURES
    /* As above: */
    if (scheme_is_multithreaded(0)) {
      ref5 = jit_bmsi_i(jit_forward(), JIT_R2, PAIR_IS_NON_LIST);
      jit_movr_i(JIT_R0, JIT_R2);
      jit_ori_i(JIT_R2, JIT_R2, PAIR_IS_NON_LIST);
      jit_addi_p(JIT_R1, JIT_R1, &MZ_OPT_HASH_KEY(&((Scheme_Stx *)0x0)->iso));
      jit_lock_cmpxchgr_s(JIT_R1, JIT_R2); /* implicitly uses JIT_R0 */
      mz_patch_branch(ref5);
    } else
#endif
      {
        jit_ori_i(JIT_R2, JIT_R2, PAIR_IS_NON_LIST);
        jit_stxi_s(&MZ_OPT_HASH_KEY(&((Scheme_Stx *)0x0)->iso), JIT_R1, JIT_R2);
      }
    CHECK_LIMIT();

    __END_SHORT_JUMPS__(1);

    if (i) {
      mz_epilog_without_jmp();
      jit_jmpr(JIT_V1);
    } else {
      (void)jit_movi_p(JIT_R0, scheme_false);
      mz_epilog(JIT_R2);
    }

    scheme_jit_register_sub_func(jitter, code, scheme_false);
  }

  return 1;
}

static int common8(mz_jit_state *jitter, void *_data)
{
  /* list_length_code */
  /* argument is in R0 */
  {
    void *code;
    GC_CAN_IGNORE jit_insn *refloop, *ref1, *ref2, *ref3, *ref4, *ref5;

    code = jit_get_ip().ptr;
    sjc.list_length_code = code;

    mz_prolog(JIT_R2);
    
    __START_SHORT_JUMPS__(1);
    
    /* Save original argument: */
    jit_movr_p(JIT_V1, JIT_R0);

    /* Note: there's no fuel check in this loop, just like there isn't in
       scheme_list_length(). Maybe there should be. */

    /* R0 has argument, R1 has counter */
    jit_movi_l(JIT_R1, 0);

    refloop = _jit.x.pc;

    ref2 = jit_beqi_p(jit_forward(), JIT_R0, scheme_null);    
    ref3 = jit_bmsi_l(jit_forward(), JIT_R0, 0x1);

    ref4 = mz_bnei_t(jit_forward(), JIT_R0, scheme_pair_type, JIT_R2);
    CHECK_LIMIT();

    jit_ldxi_s(JIT_R2, JIT_R0, &MZ_OPT_HASH_KEY(&((Scheme_Stx *)0x0)->iso));
    ref5 = jit_bmsi_ul(jit_forward(), JIT_R2, PAIR_IS_NON_LIST);

    jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&SCHEME_CDR(0x0));
    jit_addi_l(JIT_R1, JIT_R1, 1);

    (void)jit_jmpi(refloop);
    CHECK_LIMIT();

    /* Return result: */
    mz_patch_branch(ref2);
    __END_SHORT_JUMPS__(1);
    jit_fixnum_l(JIT_R0, JIT_R1);
    ref1 = _jit.x.pc;
    mz_epilog(JIT_R2);

    __START_SHORT_JUMPS__(1);
    mz_patch_branch(ref3);
    mz_patch_branch(ref4);
    mz_patch_branch(ref5);
    __END_SHORT_JUMPS__(1);

    JIT_UPDATE_THREAD_RSPTR();
    jit_prepare(1);
    jit_pusharg_p(JIT_V1);
    (void)mz_finish_lwe(ts_scheme_checked_length, ref2);
    CHECK_LIMIT();
    jit_retval(JIT_R0);

    __START_SHORT_JUMPS__(1);
    (void)jit_jmpi(ref1);
    __END_SHORT_JUMPS__(1);

    scheme_jit_register_sub_func(jitter, code, scheme_false);
  }

  return 1;
}

static int common8_5(mz_jit_state *jitter, void *_data)
{
  int i;

  /* list_{ref,tail}_code */
  /* first argument is in R0, second in R1 */
  for (i = 0; i < 2; i++) {
    void *code;
    GC_CAN_IGNORE jit_insn *refslow, *refloop, *refdone, *ref, *refr;
    GC_CAN_IGNORE jit_insn *refmaybedone, *refresume;

    code = jit_get_ip().ptr;
    if (i == 0)
      sjc.list_tail_code = code;
    else
      sjc.list_ref_code = code;

    mz_prolog(JIT_R2);
    
    __START_SHORT_JUMPS__(1);
    
    /* Save original arguments: */
    jit_movr_p(JIT_V1, JIT_R0);
    mz_set_local_p(JIT_R1, JIT_LOCAL3);

    ref = jit_bmsi_l(jit_forward(), JIT_R1, 0x1);

    refslow = _jit.x.pc;
    
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(2));
    JIT_UPDATE_THREAD_RSPTR();
    mz_get_local_p(JIT_R1, JIT_LOCAL3);
    jit_stxi_p(WORDS_TO_BYTES(1), JIT_RUNSTACK, JIT_R1);
    jit_str_p(JIT_RUNSTACK, JIT_V1);
    CHECK_LIMIT();
    jit_movi_i(JIT_R0, 2);
    mz_prepare(2);
    jit_pusharg_p(JIT_RUNSTACK);
    jit_pusharg_i(JIT_R0);
    __END_SHORT_JUMPS__(1);
    if (i == 0) {
      mz_finish_prim_lwe(ts_scheme_checked_list_tail, refr);
    } else {
      mz_finish_prim_lwe(ts_scheme_checked_list_ref, refr);
    }
    __START_SHORT_JUMPS__(1);
    jit_retval(JIT_R0);
    jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(2));
    JIT_UPDATE_THREAD_RSPTR();
    CHECK_LIMIT();
    mz_epilog(JIT_R2);

    mz_patch_branch(ref);

    /* Handle simple cases: non-negative fixnum index */
    jit_rshi_l(JIT_R1, JIT_R1, 1);
    (void)jit_blti_l(refslow, JIT_R1, 0);
    
    refloop = _jit.x.pc;
    if (i == 0) {
      refmaybedone = jit_bmci_l(jit_forward(), JIT_R1, 0xFFF);
      refresume = _jit.x.pc;
    } else {
      refmaybedone = NULL;
      refresume = NULL;
    }
    (void)jit_bmsi_l(refslow, JIT_R0, 0x1);
    (void)mz_bnei_t(refslow, JIT_R0, scheme_pair_type, JIT_R2);
    if (i == 1) {
      refmaybedone = jit_bmci_l(jit_forward(), JIT_R1, 0xFFF);
      refresume = _jit.x.pc;
    }
    jit_subi_l(JIT_R1, JIT_R1, 1);
    jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&SCHEME_CDR(0x0));
    (void)jit_jmpi(refloop);

    mz_patch_branch(refmaybedone);
    refdone = jit_beqi_l(jit_forward(), JIT_R1, 0);
#ifdef FUEL_AUTODECEREMENTS
    /* not zero yet, so check for thread swap or other pause */ 
    (void)mz_tl_ldi_i(JIT_R2, tl_scheme_fuel_counter);
    (void)jit_blei_i(refslow, JIT_R2, 0);
    (void)jit_jmpi(refresume); /* return to fast path */
#else
    (void)jit_jmpi(refslow); /* no free registers to decrement fuel, so give up */
#endif

    mz_patch_branch(refdone);
    if (i == 1) {
      jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&SCHEME_CAR(0x0));
    }
    mz_epilog(JIT_R2);
    CHECK_LIMIT();

    __END_SHORT_JUMPS__(1);

    scheme_jit_register_sub_func(jitter, code, scheme_false);
  }

  return 1;
}

static int common9(mz_jit_state *jitter, void *_data)
{
  int i;

  /* eqv[_branch]_code */
  /* arguments in R0 and R1; for branch, false return address in V1 */
  for (i = 0; i < 2; i++) {
    void *code;
    GC_CAN_IGNORE jit_insn *ref;

    code = jit_get_ip().ptr;
    if (i == 0)
      sjc.eqv_code = code;
    else
      sjc.eqv_branch_code = code;

    mz_prolog(JIT_R2);
    CHECK_LIMIT();
    
    jit_prepare(2);
    jit_pusharg_p(JIT_R0);
    jit_pusharg_p(JIT_R1);
    (void)jit_finish(scheme_eqv); /* NONGCING, so ok from a future thread */
    jit_retval(JIT_R0);
    CHECK_LIMIT();

    __START_TINY_JUMPS__(1);
    ref = jit_beqi_i(jit_forward(), JIT_R0, 0);
    __END_TINY_JUMPS__(1);

    if (i) {
      mz_epilog(JIT_R2);
    } else {
      (void)jit_movi_p(JIT_R0, scheme_true);
      mz_epilog(JIT_R2);
    }

    __START_TINY_JUMPS__(1);
    mz_patch_branch(ref);
    __END_TINY_JUMPS__(1);
    
    if (i) {
      mz_epilog_without_jmp();
      jit_jmpr(JIT_V1);
    } else {
      (void)jit_movi_p(JIT_R0, scheme_false);
      mz_epilog(JIT_R2);
    }

    scheme_jit_register_sub_func(jitter, code, scheme_false);    

    CHECK_LIMIT();
  }

  return 1;
}

static int common10(mz_jit_state *jitter, void *_data)
{
  /* proc_arity_includes_code */
  /* R0 has proc, R1 has arity */
  {
    GC_CAN_IGNORE jit_insn *ref, *refslow, *refr ,*ref_nc, *ref_prim, *refno;
    
    sjc.proc_arity_includes_code = jit_get_ip().ptr;

    mz_prolog(JIT_R2);

    __START_SHORT_JUMPS__(1);

    ref = jit_bmsi_l(jit_forward(), JIT_R1, 0x1);

    refslow = _jit.x.pc;
    
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(2));
    JIT_UPDATE_THREAD_RSPTR();
    jit_stxi_p(WORDS_TO_BYTES(1), JIT_RUNSTACK, JIT_R1);
    jit_str_p(JIT_RUNSTACK, JIT_R0);
    CHECK_LIMIT();
    jit_movi_i(JIT_R0, 2);
    mz_prepare(2);
    jit_pusharg_p(JIT_RUNSTACK);
    jit_pusharg_i(JIT_R0);
    __END_SHORT_JUMPS__(1);
    mz_finish_prim_lwe(ts_scheme_procedure_arity_includes, refr);
    __START_SHORT_JUMPS__(1);
    jit_retval(JIT_R0);
    jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(2));
    JIT_UPDATE_THREAD_RSPTR();
    CHECK_LIMIT();

    mz_epilog(JIT_R2);

    refno = _jit.x.pc;
    (void)jit_movi_p(JIT_R0, scheme_false);
    mz_epilog(JIT_R2);

    /* R1 has fixnum ... check non-negative and them proc type */
    mz_patch_branch(ref);
    (void)jit_blti_l(refslow, JIT_R1, 0);

    jit_ldxi_s(JIT_R2, JIT_R0, &((Scheme_Object *)0x0)->type);
    ref_nc = jit_beqi_i(jit_forward(), JIT_R2, scheme_native_closure_type);
    ref_prim = jit_beqi_i(jit_forward(), JIT_R2, scheme_prim_type);

    (void)jit_jmpi(refslow);
    CHECK_LIMIT();

    /* native: */
    mz_patch_branch(ref_nc);
    jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure *)0x0)->code);
    jit_ldxi_i(JIT_R2, JIT_V1, &((Scheme_Native_Closure_Data *)0x0)->closure_size);
    (void)jit_blti_i(refslow, JIT_R2, 0); /* case lambda */
    jit_ldxi_p(JIT_R2, JIT_V1, &((Scheme_Native_Closure_Data *)0x0)->start_code);
    /* patchable_movi_p doesn't depend on actual address, which might change size: */
    (void)jit_patchable_movi_p(JIT_V1, scheme_on_demand_jit_code);
    ref_nc = jit_beqr_p(jit_forward(), JIT_R2, JIT_V1); /* not yet JITted? */
    jit_rshi_l(JIT_V1, JIT_R1, 1);
    jit_addi_l(JIT_V1, JIT_V1, 1);
    CHECK_LIMIT();
    mz_prepare(3);
    jit_pusharg_i(JIT_V1); /* anything */
    jit_pusharg_i(JIT_V1);
    jit_pusharg_p(JIT_R0);
    (void)jit_finish(sjc.check_arity_code);
    jit_retval(JIT_R0);
    (void)jit_beqi_i(refno, JIT_R0, 0);
    (void)jit_movi_p(JIT_R0, scheme_true);
    mz_epilog(JIT_R2);
    CHECK_LIMIT();

    /* not-yet-JITted native: */
    mz_patch_branch(ref_nc);
    jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure *)0x0)->code);
    jit_ldxi_p(JIT_R0, JIT_V1, &((Scheme_Native_Closure_Data *)0x0)->u2.orig_code);
    jit_rshi_l(JIT_V1, JIT_R1, 1);
    jit_ldxi_i(JIT_R2, JIT_R0, &((Scheme_Closure_Data *)0x0)->num_params);
    jit_ldxi_s(JIT_R0, JIT_R0, &SCHEME_CLOSURE_DATA_FLAGS(((Scheme_Closure_Data *)0x0)));
    ref_nc = jit_bmsi_i(jit_forward(), JIT_R0, CLOS_HAS_REST);
    (void)jit_bner_i(refno, JIT_V1, JIT_R2);
    (void)jit_movi_p(JIT_R0, scheme_true);
    mz_epilog(JIT_R2);
    CHECK_LIMIT();
    /* has rest arg: */
    mz_patch_branch(ref_nc);
    jit_subi_i(JIT_R2, JIT_R2, 1);
    (void)jit_bltr_i(refno, JIT_V1, JIT_R2);
    (void)jit_movi_p(JIT_R0, scheme_true);
    mz_epilog(JIT_R2);
    CHECK_LIMIT();

    /* primitive: */
    mz_patch_branch(ref_prim);
    jit_ldxi_i(JIT_R2, JIT_R0, &((Scheme_Primitive_Proc *)0x0)->mina);
    (void)jit_blti_i(refslow, JIT_R2, 0); /* case lambda */
    jit_rshi_l(JIT_V1, JIT_R1, 1);
    (void)jit_bltr_i(refno, JIT_V1, JIT_R2);
    jit_ldxi_i(JIT_R2, JIT_R0, &((Scheme_Primitive_Proc *)0x0)->mu.maxa);
    (void)jit_bgtr_i(refno, JIT_V1, JIT_R2);
    CHECK_LIMIT();

    (void)jit_movi_p(JIT_R0, scheme_true);
    mz_epilog(JIT_R2);

    __END_SHORT_JUMPS__(1);

    scheme_jit_register_sub_func(jitter, sjc.proc_arity_includes_code, scheme_false);
    CHECK_LIMIT();
  }

  return 1;
}

int scheme_do_generate_common(mz_jit_state *jitter, void *_data)
{
  if (!common0(jitter, _data)) return 0;
  if (!common1(jitter, _data)) return 0;
  if (!common1b(jitter, _data)) return 0;
  if (!common2(jitter, _data)) return 0;
  if (!common3(jitter, _data)) return 0;
  if (!common4(jitter, _data)) return 0;
  if (!common4b(jitter, _data)) return 0;
  if (!common4c(jitter, _data)) return 0;
  if (!common5(jitter, _data)) return 0;
  if (!common6(jitter, _data)) return 0;
  if (!common7(jitter, _data)) return 0;
  if (!common8(jitter, _data)) return 0;
  if (!common8_5(jitter, _data)) return 0;
  if (!common9(jitter, _data)) return 0;
  if (!common10(jitter, _data)) return 0;
  return 1;
}

static int more_common0(mz_jit_state *jitter, void *_data)
{
  /* *** check_proc_extract_code *** */
  /* arguments are on the Scheme stack */
  {
    GC_CAN_IGNORE jit_insn *ref, *ref2, *ref3, *refslow, *refrts;
    
    sjc.struct_proc_extract_code = jit_get_ip().ptr;
    mz_prolog(JIT_V1);
      
    __START_SHORT_JUMPS__(1);

    mz_rs_ldr(JIT_R0);
    ref = jit_bmci_ul(jit_forward(), JIT_R0, 0x1);
    CHECK_LIMIT();

    /* Slow path: call C implementation */
    refslow = _jit.x.pc;
    JIT_UPDATE_THREAD_RSPTR();
    jit_movi_i(JIT_V1, 5);
    jit_prepare(2);
    jit_pusharg_p(JIT_RUNSTACK);
    jit_pusharg_i(JIT_V1);
    (void)mz_finish_lwe(ts_scheme_extract_checked_procedure, refrts);
    jit_retval(JIT_R0);
    VALIDATE_RESULT(JIT_R0);
    mz_epilog(JIT_V1);

    /* Continue trying fast path: check proc */
    mz_patch_branch(ref);
    (void)mz_bnei_t(refslow, JIT_R0, scheme_struct_type_type, JIT_R2);
    jit_ldxi_s(JIT_R2, JIT_R0, &MZ_OPT_HASH_KEY(&((Scheme_Struct_Type *)0x0)->iso));
    (void)jit_bmci_ul(refslow, JIT_R2, STRUCT_TYPE_CHECKED_PROC);
    CHECK_LIMIT();

    mz_rs_ldxi(JIT_R1, 1);
    (void)jit_bmsi_ul(refslow, JIT_R1, 0x1);
    jit_ldxi_s(JIT_R2, JIT_R1, &((Scheme_Object *)0x0)->type);
    __START_INNER_TINY__(1);
    ref2 = jit_beqi_i(jit_forward(), JIT_R2, scheme_structure_type);
    __END_INNER_TINY__(1);
    (void)jit_bnei_i(refslow, JIT_R2, scheme_proc_struct_type);
    __START_INNER_TINY__(1);
    mz_patch_branch(ref2);
    __END_INNER_TINY__(1);
    CHECK_LIMIT();

    /* Put argument struct type in R2, target struct type is in R0 */
    jit_ldxi_p(JIT_R2, JIT_R1, &((Scheme_Structure *)0x0)->stype);
    jit_ldxi_i(JIT_R2, JIT_R2, &((Scheme_Struct_Type *)0x0)->name_pos);
    jit_ldxi_i(JIT_V1, JIT_R0, &((Scheme_Struct_Type *)0x0)->name_pos);

    /* Now R2 is argument depth, V1 is target depth */
    (void)jit_bltr_i(refslow, JIT_R2, JIT_V1);
    CHECK_LIMIT();
    /* Lookup argument type at target type depth, put it in R2: */
    jit_lshi_ul(JIT_R2, JIT_V1, JIT_LOG_WORD_SIZE);
    jit_addi_p(JIT_R2, JIT_R2, &((Scheme_Struct_Type *)0x0)->parent_types);
    jit_ldxi_p(JIT_V1, JIT_R1, &((Scheme_Structure *)0x0)->stype);
    jit_ldxr_p(JIT_R2, JIT_V1, JIT_R2);
    CHECK_LIMIT();
    (void)jit_bner_p(refslow, JIT_R2, JIT_R0);

    /* Type matches. Extract checker. */
    jit_ldxi_p(JIT_V1, JIT_R1, &(((Scheme_Structure *)0x0)->slots[0]));

    /* Checker is in V1. Set up args on runstack, then apply it. */
    mz_rs_dec(2);
    mz_rs_ldxi(JIT_R2, 5);
    mz_rs_str(JIT_R2);
    mz_rs_ldxi(JIT_R2, 6);
    mz_rs_stxi(1, JIT_R2);
    CHECK_LIMIT();
    mz_rs_sync();

    __END_SHORT_JUMPS__(1);
    scheme_generate_non_tail_call(jitter, 2, 0, 1, 0, 0, 0, 0, 0, 0);
    CHECK_LIMIT();
    __START_SHORT_JUMPS__(1);

    mz_rs_inc(2);
    mz_rs_sync();
    ref3 = jit_bnei_p(refslow, JIT_R0, scheme_false);
    CHECK_LIMIT();

    /* Check failed. Apply the failure procedure. */
    JIT_UPDATE_THREAD_RSPTR();
    jit_prepare(1);
    jit_pusharg_p(JIT_RUNSTACK);
    (void)mz_finish_lwe(ts_apply_checked_fail, refrts);
    CHECK_LIMIT();
    jit_retval(JIT_R0);
    VALIDATE_RESULT(JIT_R0);
    mz_epilog(JIT_V1);
    CHECK_LIMIT();

    /* Check passed. Extract the procedure. */
    mz_patch_branch(ref3);
    mz_rs_ldxi(JIT_R1, 1);
    jit_ldxi_p(JIT_R0, JIT_R1, &(((Scheme_Structure *)0x0)->slots[1]));

    mz_epilog(JIT_V1);
    CHECK_LIMIT();
      
    __END_SHORT_JUMPS__(1);

    scheme_jit_register_sub_func(jitter, sjc.struct_proc_extract_code, scheme_false);
  }

  /* *** module_run_start_code *** */
  /* Pushes a module name onto the stack for stack traces. */
  {
    int in;
    
    sjc.module_run_start_code = jit_get_ip().ptr;
    jit_prolog(3);
    in = jit_arg_p();
    jit_getarg_p(JIT_R0, in); /* menv */
    in = jit_arg_p();
    jit_getarg_p(JIT_R1, in); /* env */
    in = jit_arg_p();
    jit_getarg_p(JIT_R2, in); /* &name */
    CHECK_LIMIT();

    /* Store the name where we can find it */
    mz_push_locals();
    mz_set_local_p(JIT_R2, JIT_LOCAL2);

    jit_prepare(2);
    jit_pusharg_p(JIT_R1);
    jit_pusharg_p(JIT_R0);
    (void)mz_finish(scheme_module_run_finish);
    CHECK_LIMIT();
    mz_pop_locals();
    jit_ret();
    CHECK_LIMIT();

    scheme_jit_register_sub_func(jitter, sjc.module_run_start_code, scheme_eof);
  }

  /* *** module_exprun_start_code *** */
  /* Pushes a module name onto the stack for stack traces. */
  {
    int in;
    
    sjc.module_exprun_start_code = jit_get_ip().ptr;
    jit_prolog(3);
    in = jit_arg_p();
    jit_getarg_p(JIT_R0, in); /* menv */
    in = jit_arg_p();
    jit_getarg_i(JIT_R1, in); /* set_ns */
    in = jit_arg_p();
    jit_getarg_p(JIT_R2, in); /* &name */
    CHECK_LIMIT();

    /* Store the name where we can find it */
    mz_push_locals();
    mz_set_local_p(JIT_R2, JIT_LOCAL2);

    jit_prepare(2);
    jit_pusharg_i(JIT_R1);
    jit_pusharg_p(JIT_R0);
    (void)mz_finish(scheme_module_exprun_finish);
    CHECK_LIMIT();
    mz_pop_locals();
    jit_ret();
    CHECK_LIMIT();

    scheme_jit_register_sub_func(jitter, sjc.module_exprun_start_code, scheme_eof);
  }

  /* *** module_start_start_code *** */
  /* Pushes a module name onto the stack for stack traces. */
  {
    int in;
    
    sjc.module_start_start_code = jit_get_ip().ptr;
    jit_prolog(2);
    in = jit_arg_p();
    jit_getarg_p(JIT_R0, in); /* a */
    in = jit_arg_p();
    jit_getarg_p(JIT_R1, in); /* &name */
    CHECK_LIMIT();

    /* Store the name where we can find it */
    mz_push_locals();
    mz_set_local_p(JIT_R1, JIT_LOCAL2);

    jit_prepare(1);
    jit_pusharg_p(JIT_R0);
    (void)mz_finish(scheme_module_start_finish);
    CHECK_LIMIT();
    mz_pop_locals();
    jit_ret();
    CHECK_LIMIT();

    scheme_jit_register_sub_func(jitter, sjc.module_start_start_code, scheme_eof);
  }

  return 1;
}

static int more_common1(mz_jit_state *jitter, void *_data)
{
  /* apply_to_list_tail_code */
  /* argc is in V1 */
  {
    GC_CAN_IGNORE jit_insn *ref1, *ref2, *ref3, *ref4, *ref5, *ref6, *refloop;
    
    sjc.apply_to_list_tail_code = jit_get_ip().ptr;

    __START_SHORT_JUMPS__(1);

    /* extract list argument */
    jit_subi_l(JIT_R0, JIT_V1, 1);
    jit_lshi_ul(JIT_R0, JIT_R0, JIT_LOG_WORD_SIZE);
    jit_ldxr_p(JIT_R0, JIT_RUNSTACK, JIT_R0);
    jit_movi_l(JIT_R1, 0);
    CHECK_LIMIT();

    /* check that it's a list and get the length */
    refloop = _jit.x.pc;
    __START_INNER_TINY__(1);
    ref2 = jit_beqi_p(jit_forward(), JIT_R0, scheme_null);    
    __END_INNER_TINY__(1);
    ref1 = jit_bmsi_ul(jit_forward(), JIT_R0, 0x1);
    ref3 = mz_bnei_t(jit_forward(), JIT_R0, scheme_pair_type, JIT_R2);
    jit_addi_l(JIT_R1, JIT_R1, 1);
    jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&SCHEME_CDR(0x0));
    __START_INNER_TINY__(1);
    (void)jit_jmpi(refloop);
    __END_INNER_TINY__(1);
    CHECK_LIMIT();

    /* JIT_R1 is now the length of the argument list */
    __START_INNER_TINY__(1);
    mz_patch_branch(ref2);
    __END_INNER_TINY__(1);

    /* Check whether we have enough space on the stack: */
    mz_ld_runstack_base_alt(JIT_R2);
    mz_tl_ldi_p(JIT_R0, tl_MZ_RUNSTACK_START);
    jit_subr_ul(JIT_R0, JIT_RUNSTACK_BASE_OR_ALT(JIT_R2), JIT_R0);
    jit_addr_l(JIT_R2, JIT_R1, JIT_V1);
    jit_lshi_ul(JIT_R2, JIT_R2, JIT_LOG_WORD_SIZE);    
    ref4 = jit_bltr_ul(jit_forward(), JIT_R0, JIT_R2);
    CHECK_LIMIT();

    /* We have enough space, so copy args into place. Save the list in
       local2, then move the other arguments into their final place,
       which may be shifting up or shifting down. */
    jit_subi_l(JIT_R0, JIT_V1, 1);
    jit_lshi_ul(JIT_R0, JIT_R0, JIT_LOG_WORD_SIZE);
    jit_ldxr_p(JIT_R0, JIT_RUNSTACK, JIT_R0);
    mz_set_local_p(JIT_R0, JIT_LOCAL2); /* list in now in local2 */
    CHECK_LIMIT();

    jit_subi_l(JIT_R0, JIT_V1, 1); /* drop last arg */
    jit_addr_l(JIT_R0, JIT_R0, JIT_R1); /* orig + new argc */
    jit_lshi_ul(JIT_R0, JIT_R0, JIT_LOG_WORD_SIZE);
    mz_ld_runstack_base_alt(JIT_R2);
    jit_subr_p(JIT_R2, JIT_RUNSTACK_BASE_OR_ALT(JIT_R2), JIT_R0);
    CHECK_LIMIT();
    /* JIT_R2 is destination argv. We'll put the eventual rator
       as the first value there, and then move it into V1 later. */
    
    ref6 = jit_bltr_ul(jit_forward(), JIT_RUNSTACK, JIT_R2);
    
    /* runstack > new dest, so shift down */
    mz_patch_branch(ref6);
    jit_subi_l(JIT_R0, JIT_V1, 1); /* drop last arg */
    jit_lshi_ul(JIT_R0, JIT_R0, JIT_LOG_WORD_SIZE);
    jit_addr_l(JIT_R2, JIT_R2, JIT_R0); /* move R2 and RUNSTACK pointers to end instead of start */
    jit_addr_l(JIT_RUNSTACK, JIT_RUNSTACK, JIT_R0);
    jit_negr_l(JIT_R0, JIT_R0); /* negate counter */
    refloop = _jit.x.pc;
    jit_ldxr_p(JIT_R1, JIT_RUNSTACK, JIT_R0);
    jit_stxr_p(JIT_R0, JIT_R2, JIT_R1);
    jit_addi_l(JIT_R0, JIT_R0, JIT_WORD_SIZE);
    CHECK_LIMIT();
    __START_INNER_TINY__(1);
    (void)jit_blti_l(refloop, JIT_R0, 0);
    __END_INNER_TINY__(1);
    jit_subi_l(JIT_R0, JIT_V1, 1); /* drop last arg */
    jit_lshi_ul(JIT_R0, JIT_R0, JIT_LOG_WORD_SIZE);
    jit_subr_l(JIT_R2, JIT_R2, JIT_R0); /* move R2 and RUNSTACK pointers back */
    jit_subr_l(JIT_RUNSTACK, JIT_RUNSTACK, JIT_R0);
    ref5 = jit_jmpi(jit_forward());

    /* runstack < new dest, so shift up */
    mz_patch_branch(ref6);
    jit_subi_l(JIT_R0, JIT_V1, 1); /* drop last arg */
    jit_lshi_ul(JIT_R0, JIT_R0, JIT_LOG_WORD_SIZE);
    refloop = _jit.x.pc;
    jit_subi_l(JIT_R0, JIT_R0, JIT_WORD_SIZE);
    jit_ldxr_p(JIT_R1, JIT_RUNSTACK, JIT_R0);
    jit_stxr_p(JIT_R0, JIT_R2, JIT_R1);
    CHECK_LIMIT();
    __START_INNER_TINY__(1);
    (void)jit_bgti_l(refloop, JIT_R0, 0);
    __END_INNER_TINY__(1);

    /* original args are in new place; now unpack list arguments; R2
       is still argv (with leading rator), but R1 doesn't have the
       count any more; we re-compute R1 as we traverse the list
       again. */
    mz_patch_ucbranch(ref5);
    mz_get_local_p(JIT_R0, JIT_LOCAL2); /* list in R0 */
    jit_subi_l(JIT_R1, JIT_V1, 1); /* drop last original arg */
    jit_lshi_ul(JIT_R1, JIT_R1, JIT_LOG_WORD_SIZE);
    refloop = _jit.x.pc;
    __START_INNER_TINY__(1);
    ref6 = jit_beqi_p(jit_forward(), JIT_R0, scheme_null);
    __END_INNER_TINY__(1);
    CHECK_LIMIT();
    jit_ldxi_p(JIT_V1, JIT_R0, (intptr_t)&SCHEME_CAR(0x0));
    jit_stxr_p(JIT_R1, JIT_R2, JIT_V1);
    jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&SCHEME_CDR(0x0));
    jit_addi_l(JIT_R1, JIT_R1, JIT_WORD_SIZE);
    __START_INNER_TINY__(1);
    (void)jit_jmpi(refloop);
    __END_INNER_TINY__(1);
    CHECK_LIMIT();

    __START_INNER_TINY__(1);
    mz_patch_branch(ref6);
    __END_INNER_TINY__(1);
    jit_rshi_ul(JIT_R1, JIT_R1, JIT_LOG_WORD_SIZE);
    jit_subi_l(JIT_R1, JIT_R1, 1);
    
    /* Set V1 and local2 for arguments to generic tail-call handler: */
    mz_set_local_p(JIT_R1, JIT_LOCAL2);
    jit_ldr_p(JIT_V1, JIT_R2);
    jit_addi_p(JIT_RUNSTACK, JIT_R2, JIT_WORD_SIZE);
    ref6 = jit_jmpi(jit_forward());
    CHECK_LIMIT();

    /***********************************/
    /* slow path: */
    mz_patch_branch(ref1);
    mz_patch_branch(ref3);
    mz_patch_branch(ref4);

    /* Move args to below RUNSTACK_BASE: */
    mz_ld_runstack_base_alt(JIT_R2);
    jit_lshi_ul(JIT_R0, JIT_V1, JIT_LOG_WORD_SIZE);
    jit_subr_p(JIT_R2, JIT_RUNSTACK_BASE_OR_ALT(JIT_R2), JIT_R0);
    refloop = _jit.x.pc;
    jit_subi_l(JIT_R0, JIT_R0, JIT_WORD_SIZE);
    jit_ldxr_p(JIT_R1, JIT_RUNSTACK, JIT_R0);
    jit_stxr_p(JIT_R0, JIT_R2, JIT_R1);
    CHECK_LIMIT();
    __START_INNER_TINY__(1);
    (void)jit_bnei_l(refloop, JIT_R0, 0);
    __END_INNER_TINY__(1);

    jit_movr_p(JIT_RUNSTACK, JIT_R2);

    /* Set V1 and local2 for arguments to generic tail-call handler: */
    mz_set_local_p(JIT_V1, JIT_LOCAL2);
    (void)jit_movi_p(JIT_V1, scheme_apply_proc);

    mz_patch_ucbranch(ref6);

    __END_SHORT_JUMPS__(1);
    
    scheme_generate_tail_call(jitter, -1, 0, 1, 0, NULL, NULL);
    CHECK_LIMIT();
  }

  /* apply_to_list_code */
  /* argc is in V1 */
  {
    int multi_ok;
    GC_CAN_IGNORE jit_insn *ref1, *ref2, *ref3, *ref4, *ref6, *ref7, *refloop;
    void *code;

    for (multi_ok = 0; multi_ok < 2; multi_ok++) {
      code = jit_get_ip().ptr;
      if (multi_ok)
        sjc.apply_to_list_multi_ok_code = code;
      else
        sjc.apply_to_list_code = code;

      mz_prolog(JIT_R1);

      __START_SHORT_JUMPS__(1);

      /* extract list argument */
      jit_subi_l(JIT_R0, JIT_V1, 1);
      jit_lshi_ul(JIT_R0, JIT_R0, JIT_LOG_WORD_SIZE);
      jit_ldxr_p(JIT_R0, JIT_RUNSTACK, JIT_R0);
      jit_movi_l(JIT_R1, 0);
      CHECK_LIMIT();

      /* check that it's a list and get the length */
      
      refloop = _jit.x.pc;
      __START_INNER_TINY__(1);
      ref2 = jit_beqi_p(jit_forward(), JIT_R0, scheme_null);
      __END_INNER_TINY__(1);
      ref1 = jit_bmsi_ul(jit_forward(), JIT_R0, 0x1);
      ref3 = mz_bnei_t(jit_forward(), JIT_R0, scheme_pair_type, JIT_R2);
      jit_addi_l(JIT_R1, JIT_R1, 1);
      jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&SCHEME_CDR(0x0));
      __START_INNER_TINY__(1);
      (void)jit_jmpi(refloop);
      __END_INNER_TINY__(1);
      CHECK_LIMIT();

      /* JIT_R1 is now the length of the argument list */
      __START_INNER_TINY__(1);
      mz_patch_branch(ref2);
      __END_INNER_TINY__(1);

      /* Check whether we have enough space on the stack: */
      mz_tl_ldi_p(JIT_R0, tl_MZ_RUNSTACK_START);
      jit_subr_ul(JIT_R0, JIT_RUNSTACK, JIT_R0);
      jit_addr_l(JIT_R2, JIT_R1, JIT_V1);
      jit_subi_l(JIT_R2, JIT_R2, 2); /* don't need first or last arg */
      jit_lshi_ul(JIT_R2, JIT_R2, JIT_LOG_WORD_SIZE);    
      ref4 = jit_bltr_ul(jit_forward(), JIT_R0, JIT_R2);
      CHECK_LIMIT();

      /* We have enough space, so copy args into place. */
      jit_subr_p(JIT_R2, JIT_RUNSTACK, JIT_R2);
      /* R2 is now destination */

      ref7 = jit_beqi_l(jit_forward(), JIT_V1, 2); /* 2 args => no non-list args to install */

      jit_subi_l(JIT_R0, JIT_V1, 2); /* drop first and last arg */
      jit_lshi_ul(JIT_R0, JIT_R0, JIT_LOG_WORD_SIZE);
      jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, JIT_WORD_SIZE); /* skip first arg */
      refloop = _jit.x.pc;
      jit_subi_l(JIT_R0, JIT_R0, JIT_WORD_SIZE);
      jit_ldxr_p(JIT_R1, JIT_RUNSTACK, JIT_R0);
      jit_stxr_p(JIT_R0, JIT_R2, JIT_R1);
      CHECK_LIMIT();
      __START_INNER_TINY__(1);
      (void)jit_bgti_l(refloop, JIT_R0, 0);
      __END_INNER_TINY__(1);
      jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, JIT_WORD_SIZE); /* restore RUNSTACK */

      mz_patch_branch(ref7);

      /* original args are in new place; now unpack list arguments; R2
         is still argv, but R1 doesn't have the count any more; 
         we re-compute R1 as we traverse the list again. */

      jit_subi_l(JIT_R0, JIT_V1, 1);
      jit_lshi_ul(JIT_R0, JIT_R0, JIT_LOG_WORD_SIZE);
      jit_ldxr_p(JIT_R0, JIT_RUNSTACK, JIT_R0);
      CHECK_LIMIT();
    
      jit_subi_l(JIT_R1, JIT_V1, 2); /* drop first and last original arg */
      jit_lshi_ul(JIT_R1, JIT_R1, JIT_LOG_WORD_SIZE);
      refloop = _jit.x.pc;
      __START_INNER_TINY__(1);
      ref6 = jit_beqi_p(jit_forward(), JIT_R0, scheme_null);
      __END_INNER_TINY__(1);
      CHECK_LIMIT();
      jit_ldxi_p(JIT_V1, JIT_R0, (intptr_t)&SCHEME_CAR(0x0));
      jit_stxr_p(JIT_R1, JIT_R2, JIT_V1);
      jit_ldxi_p(JIT_R0, JIT_R0, (intptr_t)&SCHEME_CDR(0x0));
      jit_addi_l(JIT_R1, JIT_R1, JIT_WORD_SIZE);
      __START_INNER_TINY__(1);
      (void)jit_jmpi(refloop);
      __END_INNER_TINY__(1);
      CHECK_LIMIT();

      __START_INNER_TINY__(1);
      mz_patch_branch(ref6);
      __END_INNER_TINY__(1);
    
      /* Set V1 and local2 for arguments to generic call handler: */
      jit_ldr_p(JIT_V1, JIT_RUNSTACK);
      jit_movr_p(JIT_RUNSTACK, JIT_R2);
      jit_rshi_ul(JIT_R1, JIT_R1, JIT_LOG_WORD_SIZE);
      jit_movr_i(JIT_R0, JIT_R1);
      ref6 = jit_jmpi(jit_forward());
      CHECK_LIMIT();

      /***********************************/
      /* slow path: */
      mz_patch_branch(ref1);
      mz_patch_branch(ref3);
      mz_patch_branch(ref4);

      /* We have to copy the args, because the generic apply
         wants to pop N arguments. */
      jit_lshi_ul(JIT_R0, JIT_V1, JIT_LOG_WORD_SIZE);
      jit_subr_p(JIT_R2, JIT_RUNSTACK, JIT_R0);
      refloop = _jit.x.pc;
      jit_subi_l(JIT_R0, JIT_R0, JIT_WORD_SIZE);
      jit_ldxr_p(JIT_R1, JIT_RUNSTACK, JIT_R0);
      jit_stxr_p(JIT_R0, JIT_R2, JIT_R1);
      CHECK_LIMIT();
      __START_INNER_TINY__(1);
      (void)jit_bnei_l(refloop, JIT_R0, 0);
      __END_INNER_TINY__(1);

      jit_movr_p(JIT_RUNSTACK, JIT_R2);

      /* Set V1 and local2 for arguments to generic tail-call handler: */
      jit_movr_p(JIT_R0, JIT_V1);
      (void)jit_movi_p(JIT_V1, scheme_apply_proc);

      mz_patch_ucbranch(ref6);

      __END_SHORT_JUMPS__(1);
    
      scheme_generate_non_tail_call(jitter, -1, 0, 1, multi_ok, 0, 0, 1, 0, 0);

      scheme_jit_register_sub_func(jitter, code, scheme_false);
    }
  }

#ifdef MZ_USE_LWC
  /* native_starter_code */
  {
    sjc.native_starter_code = (LWC_Native_Starter)jit_get_ip().ptr;
    
    /* store stack pointer in address given by 5th argument, then jump to
       the address given by the 4th argument */
    jit_getprearg_pippp_p(JIT_PREARG);
    jit_str_p(JIT_PREARG, JIT_SP);
    jit_getprearg_pipp_p(JIT_PREARG);
    jit_jmpr(JIT_PREARG);

    CHECK_LIMIT();
  }

  /* continuation_apply_indirect_code */
  {
    int in;

    sjc.continuation_apply_indirect_code = (Continuation_Apply_Indirect)jit_get_ip().ptr;

    /* install stack pointer into first argument before doing anything */
    jit_getprearg__p(JIT_PREARG);
    jit_str_p(JIT_PREARG, JIT_SP);

    /* accept the two arguments */
    jit_prolog(2);
    in = jit_arg_p();
    jit_getarg_p(JIT_R0, in);
    in = jit_arg_p();
    jit_getarg_l(JIT_R1, in);

    /* make room on the stack to copy a continuation in */
    jit_subr_p(JIT_SP, JIT_SP, JIT_R1);

    /* get preserved registers that we otherwise don't use in JIT-generated 
       code; put them back in place just before we get to the 
       continuation */
#ifdef JIT_X86_64
    jit_stxi_p((int)&((Apply_LWC_Args *)0x0)->saved_r14, JIT_R0, JIT_R(14));
    jit_stxi_p((int)&((Apply_LWC_Args *)0x0)->saved_r15, JIT_R0, JIT_R(15));
# ifdef _WIN64
    jit_stxi_p((int)&((Apply_LWC_Args *)0x0)->saved_r12, JIT_R0, JIT_R(12));
    jit_stxi_p((int)&((Apply_LWC_Args *)0x0)->saved_r13, JIT_R0, JIT_R(13));
# endif
#endif

    jit_prepare(1);
    jit_pusharg_p(JIT_R0);
    (void)mz_finish(scheme_jit_continuation_apply_install);
    
    CHECK_LIMIT();
  }
#endif
      
#ifdef MZ_USE_LWC
  /* continuation_apply_finish_code */
  {
    int in;

    sjc.continuation_apply_finish_code = (Continuation_Apply_Finish)jit_get_ip().ptr;

    jit_prolog(2);
    in = jit_arg_p();
    jit_getarg_p(JIT_R0, in); /* Apply_LWC_Args */
    in = jit_arg_p();
    jit_getarg_p(JIT_R1, in); /* new stack position */
    in = jit_arg_p();
    jit_getarg_p(JIT_R2, in); /* new frame position */
    CHECK_LIMIT();

    /* Restore old stack and frame pointers: */
    jit_movr_p(JIT_SP, JIT_R1);
    jit_movr_p(JIT_FP, JIT_R2);

    /* Restore saved V1: */
    jit_ldxi_p(JIT_R1, JIT_R0, (int)&((Apply_LWC_Args *)0x0)->lwc);
    jit_ldxi_l(JIT_V1, JIT_R1, (int)&((Scheme_Current_LWC *)0x0)->saved_v1);
    CHECK_LIMIT();

    /* Restore runstack, runstack_start, and thread-local pointer */
    jit_ldxi_p(JIT_RUNSTACK, JIT_R0, (int)&((Apply_LWC_Args *)0x0)->new_runstack);
# ifdef THREAD_LOCAL_USES_JIT_V2
    jit_ldxi_p(JIT_V2, JIT_R0, (int)&((Apply_LWC_Args *)0x0)->new_threadlocal);
# else
    jit_ldxi_p(JIT_RUNSTACK_BASE, JIT_R0, (int)&((Apply_LWC_Args *)0x0)->new_runstack_base);
# endif
# ifdef JIT_X86_64
    jit_ldxi_p(JIT_R14, JIT_R0, (int)&((Apply_LWC_Args *)0x0)->new_threadlocal);
# endif

    /* restore preserved registers that we otherwise don't use */
# ifdef JIT_X86_64
    /* saved_r14 is installed in the topmost frame already */
    jit_ldxi_p(JIT_R(15), JIT_R0, (int)&((Apply_LWC_Args *)0x0)->saved_r15);
# ifdef _WIN64
    jit_ldxi_p(JIT_R(12), JIT_R0, (int)&((Apply_LWC_Args *)0x0)->saved_r12);
    jit_ldxi_p(JIT_R(13), JIT_R0, (int)&((Apply_LWC_Args *)0x0)->saved_r13);
# endif
# endif
    CHECK_LIMIT();

    /* Prepare to jump to original return: */
    jit_ldxi_p(JIT_R1, JIT_R0, (int)&((Apply_LWC_Args *)0x0)->lwc);
    jit_ldxi_l(JIT_R2, JIT_R1, (int)&((Scheme_Current_LWC *)0x0)->original_dest);

    /* install result value: */
    jit_ldxi_p(JIT_R0, JIT_R0, (int)&((Apply_LWC_Args *)0x0)->result);

    jit_jmpr(JIT_R2);

    CHECK_LIMIT();
  }
#endif
      
  return 1;
}

int scheme_do_generate_more_common(mz_jit_state *jitter, void *_data)
{
  if (!more_common0(jitter, _data)) return 0;
  if (!more_common1(jitter, _data)) return 0;
  return 1;
}

#endif
