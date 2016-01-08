/*
  Racket
  Copyright (c) 2004-2016 PLT Design Inc.
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
#include "schmach.h"
#include <string.h>
#ifdef USE_STACKAVAIL
# include <malloc.c>
#endif

/* global_constants */
READ_ONLY Scheme_Object scheme_true[1];
READ_ONLY Scheme_Object scheme_false[1];

READ_ONLY Scheme_Object *scheme_not_prim;
READ_ONLY Scheme_Object *scheme_eq_prim;
READ_ONLY Scheme_Object *scheme_eqv_prim;
READ_ONLY Scheme_Object *scheme_equal_prim;

/* locals */
static Scheme_Object *not_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *boolean_p_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *eq_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *eqv_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *equal_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *equalish_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *chaperone_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *impersonator_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *chaperone_of (int argc, Scheme_Object *argv[]);
static Scheme_Object *impersonator_of (int argc, Scheme_Object *argv[]);

typedef struct Equal_Info {
  /* All pointers, 0, or odd numbers, because it's allocated with scheme_malloc(): */
  intptr_t depth; /* always odd, so it looks like a fixnum */
  intptr_t car_depth; /* always odd => fixnum */
  Scheme_Hash_Table *ht;
  Scheme_Object *recur;
  Scheme_Object *next, *next_next;
  Scheme_Object *insp;
  intptr_t for_chaperone; /* 3 => for impersonator */
  intptr_t eq_for_modidx;
} Equal_Info;

static int is_equal (Scheme_Object *obj1, Scheme_Object *obj2, Equal_Info *eql);
static int vector_equal (Scheme_Object *vec1, Scheme_Object *orig_vec1,
                         Scheme_Object *vec2, Scheme_Object *orig_vec2,
                         Equal_Info *eql);
static int struct_equal (Scheme_Object *s1, Scheme_Object *orig_s1, 
                         Scheme_Object *s2, Scheme_Object *orig_s2, 
                         Equal_Info *eql);

void scheme_init_true_false(void)
{
  scheme_true->type = scheme_true_type;
  scheme_false->type = scheme_false_type;
  scheme_void->type = scheme_void_type;
}

void scheme_init_bool (Scheme_Env *env)
{
  Scheme_Object *p;

  REGISTER_SO(scheme_not_prim);
  REGISTER_SO(scheme_eq_prim);
  REGISTER_SO(scheme_eqv_prim);
  REGISTER_SO(scheme_equal_prim);

  p = scheme_make_folding_prim(not_prim, "not", 1, 1, 1);
  scheme_not_prim = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE);
  scheme_add_global_constant("not", p, env);

  p = scheme_make_folding_prim(boolean_p_prim, "boolean?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE);
  scheme_add_global_constant("boolean?", p, env);

  p = scheme_make_folding_prim(eq_prim, "eq?", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE);
  scheme_eq_prim = p;
  scheme_add_global_constant("eq?", p, env);

  p = scheme_make_folding_prim(eqv_prim, "eqv?", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE);
  scheme_eqv_prim = p;
  scheme_add_global_constant("eqv?", scheme_eqv_prim, env);
  
  p = scheme_make_prim_w_arity(equal_prim, "equal?", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED);
  scheme_equal_prim = p;
  scheme_add_global_constant("equal?", scheme_equal_prim, env);

  scheme_add_global_constant("equal?/recur", 
                             scheme_make_prim_w_arity(equalish_prim, "equal?/recur", 3, 3), 
                             env);

  p = scheme_make_immed_prim(chaperone_p, "chaperone?", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE);
  scheme_add_global_constant("chaperone?", p, env);

  p = scheme_make_immed_prim(impersonator_p, "impersonator?", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE);
  scheme_add_global_constant("impersonator?", p, env);

  scheme_add_global_constant("chaperone-of?",
                             scheme_make_prim_w_arity(chaperone_of, "chaperone-of?", 2, 2),
                             env);
  scheme_add_global_constant("impersonator-of?",
                             scheme_make_prim_w_arity(impersonator_of, "impersonator-of?", 2, 2),
                             env);
}

static Scheme_Object *
not_prim (int argc, Scheme_Object *argv[])
{
  return (SAME_OBJ(argv[0], scheme_false) ? scheme_true : scheme_false);
}

static Scheme_Object *
boolean_p_prim (int argc, Scheme_Object *argv[])
{
  return (SCHEME_BOOLP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *
eq_prim (int argc, Scheme_Object *argv[])
{
  return (SAME_OBJ(argv[0], argv[1]) ? scheme_true : scheme_false);
}

static Scheme_Object *
eqv_prim (int argc, Scheme_Object *argv[])
{
  return (scheme_eqv(argv[0], argv[1]) ? scheme_true : scheme_false);
}

XFORM_NONGCING static void init_equal_info(Equal_Info *eql)
{
  eql->depth = 1;
  eql->car_depth = 1;
  eql->ht = NULL;
  eql->recur = NULL;
  eql->next = NULL;
  eql->next_next = NULL;
  eql->insp = NULL;
  eql->for_chaperone = 0;
  eql->eq_for_modidx = 0;
}

static Scheme_Object *
equal_prim (int argc, Scheme_Object *argv[])
{
  Equal_Info eql;

  init_equal_info(&eql);

  return (is_equal(argv[0], argv[1], &eql) ? scheme_true : scheme_false);
}

static Scheme_Object *
equalish_prim (int argc, Scheme_Object *argv[])
{
  Equal_Info eql;

  scheme_check_proc_arity("equal?/recur", 2, 2, argc, argv);

  init_equal_info(&eql);
  eql.next_next = argv[2];

  return (is_equal(argv[0], argv[1], &eql) ? scheme_true : scheme_false);
}

int scheme_eq (Scheme_Object *obj1, Scheme_Object *obj2)
{
  return SAME_OBJ(obj1, obj2);
}

#ifdef MZ_LONG_DOUBLE
XFORM_NONGCING static MZ_INLINE int mz_long_double_eqv(long_double a, long_double b)
{
# ifndef NAN_EQUALS_ANYTHING
  if (!long_double_eqv(a, b)) {
# endif
    /* Double-check for NANs: */
    if (MZ_IS_LONG_NAN(a)) {
      if (MZ_IS_LONG_NAN(b))
        return 1;
# ifdef NAN_EQUALS_ANYTHING
      return 0;
# endif
    }
# ifdef NAN_EQUALS_ANYTHING
    if (MZ_IS_LONG_NAN(b))
      return 0;
    else {
      if (long_double_eqv(a, get_long_double_zero())) {
        if (long_double_eqv(b, get_long_double_zero())) {
          return scheme_long_minus_zero_p(a) == scheme_long_minus_zero_p(b);
        }
      }
      return long_double_eqv(a, b);
    }
# else
    return 0;
  }
  if (long_double_eqv(a, get_long_double_zero())) {
    if (long_double_eqv(b, get_long_double_zero())) {
      return scheme_long_minus_zero_p(a) == scheme_long_minus_zero_p(b);
    }
  }
  return 1;
# endif
}
#endif
XFORM_NONGCING static MZ_INLINE int double_eqv(double a, double b)
{
# ifndef NAN_EQUALS_ANYTHING
  if (a != b) {
# endif
    /* Double-check for NANs: */
    if (MZ_IS_NAN(a)) {
      if (MZ_IS_NAN(b))
        return 1;
# ifdef NAN_EQUALS_ANYTHING
      return 0;
# endif
    }
# ifdef NAN_EQUALS_ANYTHING
    if (MZ_IS_NAN(b))
      return 0;
    else {
      if (a == 0.0) {
        if (b == 0.0) {
          return scheme_minus_zero_p(a) == scheme_minus_zero_p(b);
        }
      }
      return (a == b);
    }
# else
    return 0;
  }
  if (a == 0.0) {
    if (b == 0.0) {
      return scheme_minus_zero_p(a) == scheme_minus_zero_p(b);
    }
  }
  return 1;
# endif
}

XFORM_NONGCING static int is_eqv(Scheme_Object *obj1, Scheme_Object *obj2)
{
  Scheme_Type t1, t2;

  if (SAME_OBJ(obj1, obj2))
    return 1;

  t1 = SCHEME_TYPE(obj1);
  t2 = SCHEME_TYPE(obj2);

  if (NOT_SAME_TYPE(t1, t2)) {
#ifdef EQUATE_FLOATS_OF_DIFFERENT_PRECISIONS
    /* If one is a float and the other is a double, coerce to double */
    if ((t1 == scheme_float_type) && (t2 == scheme_double_type))
      return double_eqv(SCHEME_FLT_VAL(obj1), SCHEME_DBL_VAL(obj2));
    else if ((t2 == scheme_float_type) && (t1 == scheme_double_type))
      return double_eqv(SCHEME_DBL_VAL(obj1), SCHEME_FLT_VAL(obj2));
#endif
    return -1;
  } else {
    switch (t1) {
#ifdef MZ_LONG_DOUBLE
    case scheme_long_double_type:
      return mz_long_double_eqv(SCHEME_LONG_DBL_VAL(obj1), SCHEME_LONG_DBL_VAL(obj2));
#endif
#ifdef MZ_USE_SINGLE_FLOATS
    case scheme_float_type:
      return double_eqv(SCHEME_FLT_VAL(obj1), SCHEME_FLT_VAL(obj2));
#endif
    case scheme_double_type:
      return double_eqv(SCHEME_DBL_VAL(obj1), SCHEME_DBL_VAL(obj2));
    case scheme_bignum_type:
      return scheme_bignum_eq(obj1, obj2);
    case scheme_rational_type:
      return scheme_rational_eq(obj1, obj2);
    case scheme_complex_type:
      {
        Scheme_Complex *c1 = (Scheme_Complex *)obj1;
        Scheme_Complex *c2 = (Scheme_Complex *)obj2;
        return scheme_eqv(c1->r, c2->r) && scheme_eqv(c1->i, c2->i);
      }
    case scheme_char_type:
      return SCHEME_CHAR_VAL(obj1) == SCHEME_CHAR_VAL(obj2);
    case scheme_symbol_type:
    case scheme_keyword_type:
    case scheme_scope_type:
      /* `eqv?` requires `eq?` */
      return 0;
    default:
      return -1;
    }
  }
}

int scheme_eqv (Scheme_Object *obj1, Scheme_Object *obj2)
{
  return (is_eqv(obj1, obj2) > 0);
}

XFORM_NONGCING int is_fast_equal (Scheme_Object *obj1, Scheme_Object *obj2, int for_chaperone)
{
  Scheme_Type t1, t2;
  int cmp;

  cmp = is_eqv(obj1, obj2);
  if (cmp > -1)
    return cmp;

  t1 = SCHEME_TYPE(obj1);
  t2 = SCHEME_TYPE(obj2);

  if (NOT_SAME_TYPE(t1, t2))
    return -1;

 switch (t1) {
 case scheme_flvector_type:
   {
     intptr_t l1, l2, i;
     l1 = SCHEME_FLVEC_SIZE(obj1);
     l2 = SCHEME_FLVEC_SIZE(obj2);
     if (l1 == l2) {
       for (i = 0; i < l1; i++) {
         if (!double_eqv(SCHEME_FLVEC_ELS(obj1)[i],
                         SCHEME_FLVEC_ELS(obj2)[i]))
           return 0;
       }
       return 1;
     }
     return 0;
   }
#ifdef MZ_LONG_DOUBLE
 case scheme_extflvector_type:
   {
     intptr_t l1, l2, i;
     l1 = SCHEME_EXTFLVEC_SIZE(obj1);
     l2 = SCHEME_EXTFLVEC_SIZE(obj2);
     if (l1 == l2) {
       for (i = 0; i < l1; i++) {
         if (!mz_long_double_eqv(SCHEME_EXTFLVEC_ELS(obj1)[i],
                                 SCHEME_EXTFLVEC_ELS(obj2)[i]))
           return 0;
       }
       return 1;
     }
     return 0;
   }
#endif
 case scheme_byte_string_type:
 case scheme_unix_path_type:
 case scheme_windows_path_type:
   {
     intptr_t l1, l2;
     if (for_chaperone) return -1;
     l1 = SCHEME_BYTE_STRTAG_VAL(obj1);
     l2 = SCHEME_BYTE_STRTAG_VAL(obj2);
     return ((l1 == l2)
             && !memcmp(SCHEME_BYTE_STR_VAL(obj1), SCHEME_BYTE_STR_VAL(obj2), l1));
   }
 case scheme_char_string_type:
   {
     intptr_t l1, l2;
     if (for_chaperone) return -1;
     l1 = SCHEME_CHAR_STRTAG_VAL(obj1);
     l2 = SCHEME_CHAR_STRTAG_VAL(obj2);
     return ((l1 == l2)
             && !memcmp(SCHEME_CHAR_STR_VAL(obj1), SCHEME_CHAR_STR_VAL(obj2), l1 * sizeof(mzchar)));
   }
 case scheme_cpointer_type:
   {
     return (((char *)SCHEME_CPTR_VAL(obj1) + SCHEME_CPTR_OFFSET(obj1))
             == ((char *)SCHEME_CPTR_VAL(obj2) + SCHEME_CPTR_OFFSET(obj2)));
   }
 case scheme_place_bi_channel_type:
   {
     Scheme_Place_Bi_Channel *bc1, *bc2;
     bc1 = (Scheme_Place_Bi_Channel *)obj1;
     bc2 = (Scheme_Place_Bi_Channel *)obj2;
     return (SAME_OBJ(bc1->link->recvch, bc2->link->recvch)
             && SAME_OBJ(bc1->link->sendch, bc2->link->sendch));
   }
 }

 return -1;
}

int is_slow_equal (Scheme_Object *obj1, Scheme_Object *obj2)
{
  Equal_Info eql;

  init_equal_info(&eql);

  return is_equal(obj1, obj2, &eql);
}

int scheme_equal (Scheme_Object *obj1, Scheme_Object *obj2)
{
  int v;

  v = is_fast_equal(obj1, obj2, 0);
  if (v > -1)
    return v;

  return is_slow_equal(obj1, obj2);
}

int scheme_equal_modix_eq (Scheme_Object *obj1, Scheme_Object *obj2)
{
  Equal_Info eql;

  init_equal_info(&eql);
  eql.eq_for_modidx = 1;

  return is_equal(obj1, obj2, &eql);
}

static Scheme_Object *union_find(Scheme_Object *obj1, Scheme_Hash_Table *ht)
{
  Scheme_Object *v, *prev = obj1, *prev_prev = obj1;

  while (1) {
    v = scheme_hash_get(ht, prev);
    if (v) {
      prev_prev = prev;
      prev = v;
    } else 
      break;
  }

  /* Point all items to prev */
  while (obj1 != prev_prev) {
    v = scheme_hash_get(ht, obj1);
    scheme_hash_set(ht, obj1, prev);
    obj1 = v;
  }

  return prev;
}

static int union_check(Scheme_Object *obj1, Scheme_Object *obj2, Equal_Info *eql) 
{
  if (eql->depth < 50) {
    if (!eql->next_next)
      eql->depth += 2;
    return 0;
  } else {
    Scheme_Hash_Table *ht = eql->ht;
    if (!ht) {
      ht = scheme_make_hash_table(SCHEME_hash_ptr);
      eql->ht = ht;
    }
    obj1 = union_find(obj1, ht);
    obj2 = union_find(obj2, ht);

    if (SAME_OBJ(obj1, obj2))
      return 1;

    scheme_hash_set(ht, obj2, obj1);

    return 0;
  }
}

static Scheme_Object *equal_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *v1 = (Scheme_Object *)p->ku.k.p1;
  Scheme_Object *v2 = (Scheme_Object *)p->ku.k.p2;
  Equal_Info *eql = (Equal_Info *)p->ku.k.p3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  return is_equal(v1, v2, eql) ? scheme_true : scheme_false;
}

static Scheme_Object *equal_recur(int argc, Scheme_Object **argv, Scheme_Object *prim)
{
  Equal_Info *eql = (Equal_Info *)SCHEME_PRIM_CLOSURE_ELS(prim)[0];
  int is_eq;

  eql->insp = NULL; /* in case the inspector is changed by context */

  is_eq = is_equal(argv[0], argv[1], eql);

  eql->insp = NULL;

  return (is_eq
          ? scheme_true
          : scheme_false);
}

static int is_equal_overflow(Scheme_Object *obj1, Scheme_Object *obj2, Equal_Info *eql)
{
  Scheme_Thread *p = scheme_current_thread;
  Equal_Info *eql2;
  Scheme_Object *v;

  eql2 = (Equal_Info *)scheme_malloc(sizeof(Equal_Info));
  memcpy(eql2, eql, sizeof(Equal_Info));

  p->ku.k.p1 = (void *)obj1;
  p->ku.k.p2 = (void *)obj2;
  p->ku.k.p3 = (void *)eql2;

  v = scheme_handle_stack_overflow(equal_k);

  memcpy(eql, eql2, sizeof(Equal_Info));
  
  return SCHEME_TRUEP(v);
}

int is_equal (Scheme_Object *obj1, Scheme_Object *obj2, Equal_Info *eql)
{
  Scheme_Type t1, t2;
  int cmp;
  Scheme_Object *orig_obj1, *orig_obj2;

 top:
  orig_obj1 = obj1;
  orig_obj2 = obj2;

  if (eql->next_next) {
    if (eql->next) {
      Scheme_Object *a[2];
      a[0] = obj1;
      a[1] = obj2;
      obj1 = _scheme_apply(eql->next, 2, a);
      return SCHEME_TRUEP(obj1);
    }
    eql->next = eql->next_next;
  }

 top_after_next:
  cmp = is_fast_equal(obj1, obj2, eql->for_chaperone == 1);
  if (cmp > -1)
    return cmp;

  if (eql->for_chaperone 
      && SCHEME_CHAPERONEP(obj2)
      && scheme_is_noninterposing_chaperone(obj2)) {
    obj2 = ((Scheme_Chaperone *)obj2)->prev;
    goto top_after_next;
  }

  if (eql->for_chaperone 
      && SCHEME_CHAPERONEP(obj1)
      && (!(SCHEME_CHAPERONE_FLAGS((Scheme_Chaperone *)obj1) & SCHEME_CHAPERONE_IS_IMPERSONATOR)
          || (eql->for_chaperone > 1))) {
    /* `obj1` and `obj2` are not eq, otherwise is_fast_equal()
       would have returned true */
    if (SCHEME_CHAPERONEP(obj2)) {
      /* for immutable hashes, it's ok for the two objects to not be eq,
         as long as the interpositions are the same and the underlying
         values are `{impersonator,chaperone}-of?`: */
      if (SCHEME_HASHTRP(((Scheme_Chaperone *)obj1)->val)
          && SCHEME_HASHTRP(((Scheme_Chaperone *)obj2)->val)
          /* eq redirects means redirects were propagated: */
          && SAME_OBJ(((Scheme_Chaperone *)obj1)->redirects,
                      ((Scheme_Chaperone *)obj2)->redirects))
        obj2 = ((Scheme_Chaperone *)obj2)->prev;
    }
    obj1 = ((Scheme_Chaperone *)obj1)->prev;
    goto top_after_next;
  }

  t1 = SCHEME_TYPE(obj1);
  t2 = SCHEME_TYPE(obj2);

  if (NOT_SAME_TYPE(t1, t2)) {
    if (!eql->for_chaperone) {
      if (SCHEME_CHAPERONEP(obj1)) {
        obj1 = ((Scheme_Chaperone *)obj1)->val;
        goto top_after_next;
      } else if (t1 == scheme_hash_tree_indirection_type) {
        obj1 = (Scheme_Object *)scheme_hash_tree_resolve_placeholder((Scheme_Hash_Tree *)obj1);
        goto top_after_next;
      }
      if (SCHEME_CHAPERONEP(obj2)) {
        obj2 = ((Scheme_Chaperone *)obj2)->val;
        goto top_after_next;
      } else if (t2 == scheme_hash_tree_indirection_type) {
        obj2 = (Scheme_Object *)scheme_hash_tree_resolve_placeholder((Scheme_Hash_Tree *)obj2);
        goto top_after_next;
      }
    }
    return 0;
  } else {
    switch (t1) {
    case scheme_pair_type:
      {
#   include "mzeqchk.inc"
        if ((eql->car_depth > 2) || !scheme_is_list(obj1)) {
          if (union_check(obj1, obj2, eql))
            return 1;
        }
        eql->car_depth += 2;
        if (is_equal(SCHEME_CAR(obj1), SCHEME_CAR(obj2), eql)) {
          eql->car_depth -= 2;
          obj1 = SCHEME_CDR(obj1);
          obj2 = SCHEME_CDR(obj2);
          goto top;
        } else
          return 0;
      }
    case scheme_mutable_pair_type:
      {
#   include "mzeqchk.inc"
        if (eql->for_chaperone == 1)
          return 0;
        if (union_check(obj1, obj2, eql))
          return 1;
        if (is_equal(SCHEME_CAR(obj1), SCHEME_CAR(obj2), eql)) {
          obj1 = SCHEME_CDR(obj1);
          obj2 = SCHEME_CDR(obj2);
          goto top;
        } else
          return 0;
      }
    case scheme_vector_type:
    case scheme_fxvector_type:
      {
#   include "mzeqchk.inc"
        if ((eql->for_chaperone == 1) && (!SCHEME_IMMUTABLEP(obj1)
                                          || !SCHEME_IMMUTABLEP(obj2)))
          return 0;
        if (union_check(obj1, obj2, eql))
          return 1;
        return vector_equal(obj1, orig_obj1, obj2, orig_obj2, eql);
      }
    case scheme_byte_string_type:
    case scheme_unix_path_type:
    case scheme_windows_path_type:
      {
        intptr_t l1, l2;
        if ((eql->for_chaperone == 1) && (!SCHEME_IMMUTABLEP(obj1)
                                          || !SCHEME_IMMUTABLEP(obj2)))
          return 0;
        l1 = SCHEME_BYTE_STRTAG_VAL(obj1);
        l2 = SCHEME_BYTE_STRTAG_VAL(obj2);
        return ((l1 == l2)
                && !memcmp(SCHEME_BYTE_STR_VAL(obj1), SCHEME_BYTE_STR_VAL(obj2), l1));
      }
    case scheme_char_string_type:
      {
        intptr_t l1, l2;
        if ((eql->for_chaperone == 1) && (!SCHEME_IMMUTABLEP(obj1)
                                          || !SCHEME_IMMUTABLEP(obj2)))
          return 0;
        l1 = SCHEME_CHAR_STRTAG_VAL(obj1);
        l2 = SCHEME_CHAR_STRTAG_VAL(obj2);
        return ((l1 == l2)
                && !memcmp(SCHEME_CHAR_STR_VAL(obj1), SCHEME_CHAR_STR_VAL(obj2), l1 * sizeof(mzchar)));
      }
    case scheme_regexp_type:
      {
        if (scheme_regexp_is_byte(obj1) != scheme_regexp_is_byte(obj2))
          return 0;
        if (scheme_regexp_is_pregexp(obj1) != scheme_regexp_is_pregexp(obj2))
          return 0;
        obj1 = scheme_regexp_source(obj1);
        obj2 = scheme_regexp_source(obj2);
        goto top;
      }
    case scheme_structure_type:
    case scheme_proc_struct_type:
      {
        Scheme_Struct_Type *st1, *st2;
        Scheme_Object *procs1, *procs2;

        st1 = SCHEME_STRUCT_TYPE(obj1);
        st2 = SCHEME_STRUCT_TYPE(obj2);

        if (eql->for_chaperone == 1)
          procs1 = NULL;
        else
          procs1 = scheme_struct_type_property_ref(scheme_impersonator_of_property, (Scheme_Object *)st1);
        if (procs1)
          procs1 = scheme_apply_impersonator_of(eql->for_chaperone, procs1, obj1);
        if (eql->for_chaperone)
          procs2 = NULL;
        else {
          procs2 = scheme_struct_type_property_ref(scheme_impersonator_of_property, (Scheme_Object *)st2);
          if (procs2)
            procs2 = scheme_apply_impersonator_of(eql->for_chaperone, procs2, obj2);
        }

        if (procs1 || procs2) {
          /* impersonator-of property trumps other forms of checking */
          if (procs1) { obj1 = procs1; orig_obj1 = obj1; }
          if (procs2) { obj2 = procs2; orig_obj2 = obj2; }
          goto top_after_next;
        } else {
          procs1 = scheme_struct_type_property_ref(scheme_equal_property, (Scheme_Object *)st1);
          if (procs1 && (st1 != st2)) {
            procs2 = scheme_struct_type_property_ref(scheme_equal_property, (Scheme_Object *)st2);
            if (!procs2
                || !SAME_OBJ(SCHEME_VEC_ELS(procs1)[0], SCHEME_VEC_ELS(procs2)[0]))
              procs1 = NULL;
          }

          if (procs1) {
            /* Has an equality property: */
            Scheme_Object *a[3], *recur;
            Equal_Info *eql2;
#     include "mzeqchk.inc"

            if (union_check(obj1, obj2, eql))
              return 1;

            /* Create/cache closure to use for recursive equality checks: */
            if (eql->recur) {
              recur = eql->recur;
              eql2 = (Equal_Info *)SCHEME_PRIM_CLOSURE_ELS(recur)[0];
            } else {
              eql2 = (Equal_Info *)scheme_malloc(sizeof(Equal_Info));
              a[0] = (Scheme_Object *)eql2;
              recur = scheme_make_prim_closure_w_arity(equal_recur,
                                                       1, a,
                                                       "equal?/recur",
                                                       2, 2);
              eql->recur = recur;
            }
            memcpy(eql2, eql, sizeof(Equal_Info));

            a[0] = orig_obj1;
            a[1] = orig_obj2;
            a[2] = recur;

            procs1 = SCHEME_VEC_ELS(procs1)[1];

            recur = _scheme_apply(procs1, 3, a);

            memcpy(eql, eql2, sizeof(Equal_Info));

            return SCHEME_TRUEP(recur);
          } else if (st1 != st2) {
            return 0;
          } else if ((eql->for_chaperone == 1)
                     && !(MZ_OPT_HASH_KEY(&st1->iso) & STRUCT_TYPE_ALL_IMMUTABLE)) {
            return 0;
          } else {
            /* Same types, but doesn't have an equality property
               (or checking for chaperone), so check transparency: */
            Scheme_Object *insp;
            if (scheme_struct_is_transparent(obj1))
              insp = NULL;
            else {
              insp = scheme_get_param(scheme_current_config(), MZCONFIG_INSPECTOR);
            }
            if (!insp || scheme_inspector_sees_part(obj1, insp, -2)) {
#       include "mzeqchk.inc"
              if (union_check(obj1, obj2, eql))
                return 1;
              return struct_equal(obj1, orig_obj1, obj2, orig_obj2, eql);
            } else
              return 0;
          }
        }
      }
    case scheme_box_type:
      {
        SCHEME_USE_FUEL(1);
        if ((eql->for_chaperone == 1) && (!SCHEME_IMMUTABLEP(obj1)
                                          || !SCHEME_IMMUTABLEP(obj2)))
          return 0;
        if (union_check(obj1, obj2, eql))
          return 1;
        if (SAME_OBJ(obj1, orig_obj1))
          obj1 = SCHEME_BOX_VAL(obj1);
        else
          obj1 = scheme_unbox(orig_obj1);
        if (SAME_OBJ(obj2, orig_obj2))
          obj2 = SCHEME_BOX_VAL(obj2);
        else
          obj2 = scheme_unbox(orig_obj2);
        goto top;
      }
    case scheme_hash_table_type:
      {
#   include "mzeqchk.inc"
        if (eql->for_chaperone == 1) 
          return 0;
        if (union_check(obj1, obj2, eql))
          return 1;
        return scheme_hash_table_equal_rec((Scheme_Hash_Table *)obj1, orig_obj1, 
                                           (Scheme_Hash_Table *)obj2, orig_obj2,
                                           eql);
      }
    case scheme_hash_tree_type:
    case scheme_eq_hash_tree_type:
    case scheme_eqv_hash_tree_type:
    case scheme_hash_tree_indirection_type:
      {
#   include "mzeqchk.inc"
        if (union_check(obj1, obj2, eql))
          return 1;
        return scheme_hash_tree_equal_rec((Scheme_Hash_Tree *)obj1, orig_obj1,
                                          (Scheme_Hash_Tree *)obj2, orig_obj2,
                                          eql);
      } 
    case scheme_bucket_table_type:
      {
#   include "mzeqchk.inc"
        if (eql->for_chaperone == 1) 
          return 0;
        if (union_check(obj1, obj2, eql))
          return 1;
        return scheme_bucket_table_equal_rec((Scheme_Bucket_Table *)obj1, orig_obj1,
                                             (Scheme_Bucket_Table *)obj2, orig_obj2,
                                             eql);
      }
    case scheme_wrap_chunk_type: {
      return vector_equal(obj1, obj1, obj2, obj2, eql);
    }
    case scheme_resolved_module_path_type:
      {
        obj1 = SCHEME_PTR_VAL(obj1);
        obj2 = SCHEME_PTR_VAL(obj2);
        goto top;
      }
    case scheme_module_index_type:
      {
        Scheme_Modidx *midx1, *midx2;
#   include "mzeqchk.inc"
        midx1 = (Scheme_Modidx *)obj1;
        midx2 = (Scheme_Modidx *)obj2;
        if (eql->eq_for_modidx
            && (SCHEME_FALSEP(midx1->path)
                || SCHEME_FALSEP(midx2->path)))
          return 0;
        else if (is_equal(midx1->path, midx2->path, eql)) {
          obj1 = midx1->base;
          obj2 = midx2->base;
          goto top;
        }
      }
    case scheme_scope_table_type:
      {
        Scheme_Scope_Table *mt1 = (Scheme_Scope_Table *)obj1;
        Scheme_Scope_Table *mt2 = (Scheme_Scope_Table *)obj2;
        if (!is_equal((Scheme_Object *)mt1->simple_scopes, (Scheme_Object *)mt2->simple_scopes, eql))
          return 0;
        obj1 = mt1->multi_scopes;
        obj2 = mt2->multi_scopes;
        goto top;
      }
    default:
      if (!eql->for_chaperone && ((t1 == scheme_chaperone_type)
                                  || (t1 == scheme_proc_chaperone_type))) {
        /* both chaperones */
        obj1 = ((Scheme_Chaperone *)obj1)->val;
        obj2 = ((Scheme_Chaperone *)obj2)->val;
        goto top_after_next;
      } else {
        Scheme_Equal_Proc eqlp = scheme_type_equals[t1];
        if (eqlp) {
          if (union_check(obj1, obj2, eql))
            return 1;
          return eqlp(obj1, obj2, eql);
        } else
          return 0;
      }
    }
  }
}

static int vector_equal(Scheme_Object *vec1, Scheme_Object *orig_vec1,
                        Scheme_Object *vec2, Scheme_Object *orig_vec2,
                        Equal_Info *eql)
{
  intptr_t i, len;
  Scheme_Object *v1, *v2;

  len = SCHEME_VEC_SIZE(vec1);
  if (len != SCHEME_VEC_SIZE(vec2))
    return 0;

  SCHEME_USE_FUEL(len);

  for (i = 0; i < len; i++) {
    if (SAME_OBJ(vec1, orig_vec1))
      v1 = SCHEME_VEC_ELS(vec1)[i];
    else
      v1 = scheme_chaperone_vector_ref(orig_vec1, i);
    if (SAME_OBJ(vec2, orig_vec2))
      v2 = SCHEME_VEC_ELS(vec2)[i];
    else
      v2 = scheme_chaperone_vector_ref(orig_vec2, i);

    if (!is_equal(v1, v2, eql))
      return 0;
  }

  return 1;
}

int struct_equal (Scheme_Object *s1, Scheme_Object *orig_s1, 
                  Scheme_Object *s2, Scheme_Object *orig_s2, 
                  Equal_Info *eql)
{
  Scheme_Object *v1, *v2;
  int i;

  for (i = SCHEME_STRUCT_NUM_SLOTS(((Scheme_Structure *)s1)); i--; ) {
    if (SAME_OBJ(s1, orig_s1))
      v1 = ((Scheme_Structure *)s1)->slots[i];
    else
      v1 = scheme_struct_ref(orig_s1, i);
    if (SAME_OBJ(s2, orig_s2))
      v2 = ((Scheme_Structure *)s2)->slots[i];
    else
      v2 = scheme_struct_ref(orig_s2, i);

    if (!is_equal(v1, v2, eql))
      return 0;
  }

  return 1;
}

int scheme_recur_equal(Scheme_Object *obj1, Scheme_Object *obj2, void *cycle_info)
{
  return is_equal(obj1, obj2, (Equal_Info *)cycle_info);
}

/* used by external programs that cannot link to variables */
Scheme_Object * scheme_make_true (void)
{
  return scheme_true;
}

Scheme_Object * scheme_make_false (void)
{
  return scheme_false;
}

static Scheme_Object *chaperone_p(int argc, Scheme_Object *argv[])
{
  return ((SCHEME_CHAPERONEP(argv[0]) 
           && !(SCHEME_CHAPERONE_FLAGS(((Scheme_Chaperone *)argv[0])) & SCHEME_CHAPERONE_IS_IMPERSONATOR))
          ? scheme_true 
          : scheme_false);
}

static Scheme_Object *impersonator_p(int argc, Scheme_Object *argv[])
{
  return (SCHEME_CHAPERONEP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *chaperone_of(int argc, Scheme_Object *argv[])
{
  return (scheme_chaperone_of(argv[0], argv[1]) ? scheme_true : scheme_false);
}

static Scheme_Object *impersonator_of(int argc, Scheme_Object *argv[])
{
  return (scheme_impersonator_of(argv[0], argv[1]) ? scheme_true : scheme_false);
}

int scheme_chaperone_of(Scheme_Object *obj1, Scheme_Object *obj2)
{
  Equal_Info eql;

  init_equal_info(&eql);
  eql.for_chaperone = 1;

  return is_equal(obj1, obj2, &eql);
}

int scheme_impersonator_of(Scheme_Object *obj1, Scheme_Object *obj2)
{
  Equal_Info eql;

  init_equal_info(&eql);
  eql.for_chaperone = 3;

  return is_equal(obj1, obj2, &eql);
}

Scheme_Object *scheme_apply_impersonator_of(int for_chaperone, Scheme_Object *procs, Scheme_Object *obj)
{
  Scheme_Object *a[1], *v, *oprocs;

  a[0] = obj;
  v = _scheme_apply(SCHEME_CDR(procs), 1, a);
  
  if (SCHEME_FALSEP(v))
    return NULL;
  
  oprocs = scheme_struct_type_property_ref(scheme_impersonator_of_property, v);  
  if (!oprocs || !SAME_OBJ(SCHEME_CAR(oprocs), SCHEME_CAR(procs)))
    scheme_contract_error((for_chaperone ? "impersonator-of?" : "equal?"),
                          "impersonator-of property procedure returned a value with a different prop:impersonator-of source",
                          "original value", 1, obj,
                          "returned value", 1, v,
                          NULL);

  procs = scheme_struct_type_property_ref(scheme_equal_property, obj);
  oprocs = scheme_struct_type_property_ref(scheme_equal_property, v);  
  if (procs || oprocs)
    if (!procs || !oprocs || !SAME_OBJ(SCHEME_VEC_ELS(oprocs)[0], 
                                       SCHEME_VEC_ELS(procs)[0]))
      scheme_contract_error((for_chaperone ? "impersonator-of?" : "equal?"),
                            "impersonator-of property procedure returned a value with a different prop:equal+hash source",
                            "original value", 1, obj,
                            "returned value", 1, v,
                            NULL);

  return v;
}
