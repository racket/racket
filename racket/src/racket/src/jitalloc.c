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
#include "jit_ts.c"

/*========================================================================*/
/*                         inlined allocation                             */
/*========================================================================*/

#ifdef CAN_INLINE_ALLOC
THREAD_LOCAL_DECL(extern uintptr_t GC_gen0_alloc_page_ptr);
intptr_t GC_initial_word(int sizeb);
intptr_t GC_pair_initial_word(int sizeb);
intptr_t GC_array_initial_word(int sizeb);
intptr_t GC_compute_alloc_size(intptr_t sizeb);

THREAD_LOCAL_DECL(static void *retry_alloc_r1); /* set by prepare_retry_alloc() */

#ifdef JIT_USE_FP_OPS
THREAD_LOCAL_DECL(double scheme_jit_save_fp);
THREAD_LOCAL_DECL(double scheme_jit_save_fp2);
# ifdef MZ_LONG_DOUBLE
THREAD_LOCAL_DECL(long_double scheme_jit_save_extfp);
THREAD_LOCAL_DECL(long_double scheme_jit_save_extfp2);
# endif
#endif

static void *prepare_retry_alloc(void *p, void *p2)
{
  /* Allocate enough to trigger a new page */
  intptr_t avail, algn;

  algn = GC_alloc_alignment();
  avail = algn - (GC_gen0_alloc_page_ptr & (algn - 1));
  
  if (!avail)
    avail = 1;
  else if (avail == algn)
    avail = 1;

  if (avail > sizeof(intptr_t))
    avail -= sizeof(intptr_t);

  /* We assume that atomic memory and tagged go to the same nursery: */
  scheme_malloc_atomic(avail);

  retry_alloc_r1 = p2;

  return p;
}

#ifdef MZ_USE_FUTURES
static void *ts_prepare_retry_alloc(void *p, void *p2) XFORM_SKIP_PROC
{
  if (scheme_use_rtcall) {
    uintptr_t ret;
  
    jit_future_storage[0] = p;
    jit_future_storage[1] = p2;
    ret = scheme_rtcall_alloc();
    GC_gen0_alloc_page_ptr = ret;
    retry_alloc_r1 = jit_future_storage[1];
    p = jit_future_storage[0];
    jit_future_storage[0] = NULL;
    jit_future_storage[1] = NULL;
    return p;
  }

  return prepare_retry_alloc(p, p2);
}
#else
# define ts_prepare_retry_alloc prepare_retry_alloc
#endif

static intptr_t read_first_word(void *sp)
{
  intptr_t foo;
  memcpy(&foo, sp, sizeof(intptr_t));
  return foo;
}

static intptr_t initial_tag_word(Scheme_Type tag, int flags)
{
  GC_CAN_IGNORE Scheme_Simple_Object sp;
  memset(&sp, 0, sizeof(Scheme_Simple_Object));
  sp.iso.so.type = tag;
  if (flags) {
    if (tag == scheme_pair_type)
      SCHEME_PAIR_FLAGS(&sp) |= flags;
    else
      SCHEME_SET_IMMUTABLE(&sp);
  }
  return read_first_word((void *)&sp);
}

int scheme_inline_alloc(mz_jit_state *jitter, int amt, Scheme_Type ty, int flags,
			int keep_r0_r1, int keep_fpr1, int inline_retry,
                        int keep_extfpr1)
/* Puts allocated result at JIT_V1; first word is GC tag.
   Uses JIT_R2 as temporary. The allocated memory is "dirty" (i.e., not 0ed).
   Save FP0 when FP ops are enabled. */
{
  GC_CAN_IGNORE jit_insn *ref, *reffail;
#ifdef MZ_GC_STRESS_TESTING
  GC_CAN_IGNORE jit_insn *refstress;
#endif
  intptr_t a_word, sz, algn;

  sz = GC_compute_alloc_size(amt);
  algn = GC_alloc_alignment();

  __START_TINY_JUMPS__(1);
#ifdef MZ_GC_STRESS_TESTING
  refstress = jit_jmpi(jit_forward());
#endif
  reffail = jit_get_ip();
  mz_tl_ldi_p(JIT_V1, tl_GC_gen0_alloc_page_ptr);
  jit_subi_l(JIT_R2, JIT_V1, 1);
  jit_andi_l(JIT_R2, JIT_R2, (algn - 1));
  ref = jit_blti_l(jit_forward(), JIT_R2, (algn - sz));
#ifdef MZ_GC_STRESS_TESTING
  mz_patch_ucbranch(refstress);
#endif
  CHECK_LIMIT();
  __END_TINY_JUMPS__(1);

  /* Failure handling */
  if (inline_retry) {
    int mode;
    if (keep_r0_r1)
      mode = 1;
    else if (keep_fpr1)
      mode = 2;
    else if (keep_extfpr1)
      mode = 3;
    else
      mode = 0;
    scheme_generate_alloc_retry(jitter, mode);
    CHECK_LIMIT();
  } else {
    if (keep_r0_r1) {
      (void)jit_calli(sjc.retry_alloc_code_keep_r0_r1);
    } else if (keep_fpr1) {
      (void)jit_calli(sjc.retry_alloc_code_keep_fpr1);
#ifdef MZ_LONG_DOUBLE
    } else if (keep_extfpr1) {
      (void)jit_calli(sjc.retry_alloc_code_keep_extfpr1);
#endif
    } else {
      (void)jit_calli(sjc.retry_alloc_code);
    }
  }
  __START_TINY_JUMPS__(1);
  (void)jit_jmpi(reffail);
  __END_SHORT_JUMPS__(1);
  
  __START_TINY_JUMPS__(1);
  mz_patch_branch(ref);
  jit_addi_ul(JIT_R2, JIT_V1, sz);
  (void)mz_tl_sti_l(tl_GC_gen0_alloc_page_ptr, JIT_R2, JIT_R0);

  /* GC header: */
  if (ty >= 0) {
    if ((ty == scheme_pair_type)
        || (ty == scheme_mutable_pair_type)
        || (ty == scheme_raw_pair_type))
      a_word = GC_pair_initial_word(amt);
    else
      a_word = GC_initial_word(amt);
    jit_stir_l(JIT_V1, a_word);
    
    /* Scheme_Object header: */
    a_word = initial_tag_word(ty, flags);
    jit_stixi_l(sizeof(intptr_t), JIT_V1, a_word);
  } else {
    /* an array of pointers */
    a_word = GC_array_initial_word(amt);
    jit_stir_l(JIT_V1, a_word);
  }

  CHECK_LIMIT();
  __END_TINY_JUMPS__(1);

  return 1;
}
#endif

int scheme_can_inline_fp_op() 
{
#ifdef INLINE_FP_OPS
  return 1;
#else
  return 0;
#endif
}

int scheme_can_inline_fp_comp()
{
#ifdef INLINE_FP_COMP
  return 1;
#else
  return 0;
#endif
}

#if defined(INLINE_FP_OPS) && !defined(CAN_INLINE_ALLOC)
static void *malloc_double(void)
{
  return scheme_make_double(scheme_jit_save_fp);
}
#ifdef MZ_LONG_DOUBLE
static void *malloc_long_double(void)
{
  return scheme_make_long_double(scheme_jit_save_extfp);
}
#endif
#endif

#ifdef MZ_PRECISE_GC
# define cons GC_malloc_pair
#else
# define cons scheme_make_pair
#endif

#ifndef CAN_INLINE_ALLOC
Scheme_Object *scheme_jit_make_list(GC_CAN_IGNORE Scheme_Object **rs, intptr_t n)
{
  GC_CAN_IGNORE Scheme_Object *l = scheme_null;
  
  while (n--) {
    l = cons(rs[n], l);
  }

  return l;
}
Scheme_Object *scheme_jit_make_list_star(GC_CAN_IGNORE Scheme_Object **rs, intptr_t n)
{
  GC_CAN_IGNORE Scheme_Object *l = rs[--n];
  
  while (n--) {
    l = cons(rs[n], l);
  }

  return l;
}
#endif

#if !defined(CAN_INLINE_ALLOC)
Scheme_Object *scheme_jit_make_vector(intptr_t n)
{
  Scheme_Object *vec;
  vec = scheme_make_vector(n, NULL);
  return vec;
}
Scheme_Object *scheme_jit_make_ivector(intptr_t n)
{
  Scheme_Object *vec;
  vec = scheme_jit_make_vector(n);
  SCHEME_SET_IMMUTABLE(vec);
  return vec;
}
Scheme_Object *scheme_jit_make_one_element_vector(Scheme_Object *a)
{
  Scheme_Object *vec;
  vec = scheme_make_vector(1, a);
  return vec;
}
Scheme_Object *scheme_jit_make_one_element_ivector(Scheme_Object *a)
{
  Scheme_Object *vec;
  vec = scheme_jit_make_one_element_vector(a);
  SCHEME_SET_IMMUTABLE(vec);
  return vec;
}
Scheme_Object *scheme_jit_make_two_element_vector(Scheme_Object *a, Scheme_Object *b)
{
  Scheme_Object *vec;
  vec = scheme_make_vector(2, a);
  SCHEME_VEC_ELS(vec)[1] = b;
  return vec;
}
Scheme_Object *scheme_jit_make_two_element_ivector(Scheme_Object *a, Scheme_Object *b)
{
  Scheme_Object *vec;
  vec = scheme_jit_make_two_element_vector(a, b);
  SCHEME_SET_IMMUTABLE(vec);
  return vec;
}
#endif

#ifdef CAN_INLINE_ALLOC
long_double ld1;

int scheme_generate_alloc_retry(mz_jit_state *jitter, int i)
{
  GC_CAN_IGNORE jit_insn *refr USED_ONLY_FOR_FUTURES;

#ifdef JIT_USE_FP_OPS
  if (i == 2) {
    (void)mz_tl_sti_d_fppop(tl_scheme_jit_save_fp, JIT_FPR0, JIT_R2);
  }
# ifdef MZ_LONG_DOUBLE
  if (i == 3) {
    (void)mz_fpu_tl_sti_ld_fppop(tl_scheme_jit_save_extfp, JIT_FPU_FPR0, JIT_R2);
  }
# endif
#endif
  JIT_UPDATE_THREAD_RSPTR();
  jit_prepare(2);
  CHECK_LIMIT();
  if (i == 1) {
    jit_pusharg_p(JIT_R1);
    jit_pusharg_p(JIT_R0);
  } else {
    (void)jit_movi_p(JIT_R0, NULL);
    jit_pusharg_p(JIT_R0);
    jit_pusharg_p(JIT_R0);
  }
  (void)mz_finish_lwe(ts_prepare_retry_alloc, refr);
  jit_retval(JIT_R0);
  if (i == 1) {
    mz_tl_ldi_l(JIT_R1, tl_retry_alloc_r1);
  }
#ifdef JIT_USE_FP_OPS
  if (i == 2) {
    (void)mz_tl_ldi_d_fppush(JIT_FPR0, tl_scheme_jit_save_fp, JIT_R2);
  }
# ifdef MZ_LONG_DOUBLE
  if (i == 3) {
    (void)mz_fpu_tl_ldi_ld_fppush(JIT_FPU_FPR0, tl_scheme_jit_save_extfp, JIT_R2);
  }
# endif
#endif
  return 1;
}
#endif

/*========================================================================*/

#endif
