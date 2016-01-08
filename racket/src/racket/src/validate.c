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
#include "schrunst.h"
#include "schmach.h"

/* Bytecode validation is an abstract interpretation on the stack,
   where the abstract values are "not available", "value", "boxed
   value", "syntax object", or "global array". */

/* FIXME: validation doesn't check CLOS_SINGLE_RESULT or
   CLOS_PRESERVES_MARKS. (Maybe check them in the JIT pass?) */

static int validate_expr(Mz_CPort *port, Scheme_Object *expr, 
                         char *stack, Validate_TLS tls,
                         int depth, int letlimit, int delta,
                         int num_toplevels, int num_stxes, int num_lifts, void *tl_use_map,
                         mzshort *tl_state, mzshort tl_timestamp,
                         Scheme_Object *app_rator, int proc_with_refs_ok, 
                         int result_ignored, struct Validate_Clearing *vc, 
                         int tailpos, int need_local_type, Scheme_Hash_Tree *procs,
                         int expected_results,
                         Scheme_Hash_Table **_st_ht);
static int validate_rator_wants_box(Scheme_Object *app_rator, int pos,
                                    int hope,
                                    Validate_TLS tls,
                                    int num_toplevels, int num_stxes, int num_lifts, void *tl_use_map);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

void scheme_init_validate()
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif
}

#define VALID_NOT 0
#define VALID_UNINIT 1
#define VALID_VAL 2
#define VALID_BOX 3
#define VALID_TOPLEVELS 4
#define VALID_VAL_NOCLEAR 5
#define VALID_BOX_NOCLEAR 6
#define VALID_TYPED 7

typedef struct Validate_Clearing {
  MZTAG_IF_REQUIRED
  int stackpos, stacksize;
  int *stack;
  int ncstackpos, ncstacksize;
  int *ncstack;
  int self_pos, self_count, self_start;
} Validate_Clearing;

static struct Validate_Clearing *make_clearing_stack()
{
  Validate_Clearing *vc;
  vc = MALLOC_ONE_RT(Validate_Clearing);
  SET_REQUIRED_TAG(vc->type = scheme_rt_validate_clearing);
  vc->self_pos = -1;
  return vc;
}

static void reset_clearing(struct Validate_Clearing *vc)
{
  vc->stackpos = 0;
  vc->ncstackpos = 0;
}

static void clearing_stack_push(struct Validate_Clearing *vc, int pos, int val)
{
  if (vc->stackpos + 2 > vc->stacksize) {
    int *a, sz;
    sz = (vc->stacksize ? 2 * vc->stacksize : 32);
    a = (int *)scheme_malloc_atomic(sizeof(int) * sz);
    memcpy(a, vc->stack, vc->stacksize * sizeof(int));
    vc->stacksize = sz;
    vc->stack = a;
  }
  vc->stack[vc->stackpos] = pos;
  vc->stack[vc->stackpos + 1] = val;
  vc->stackpos += 2;
}

static void noclear_stack_push(struct Validate_Clearing *vc, int pos)
{
  if (vc->ncstackpos + 1 > vc->ncstacksize) {
    int *a, sz;
    sz = (vc->ncstacksize ? 2 * vc->ncstacksize : 32);
    a = (int *)scheme_malloc_atomic(sizeof(int) * sz);
    memcpy(a, vc->ncstack, vc->ncstacksize * sizeof(int));
    vc->ncstacksize = sz;
    vc->ncstack = a;
  }
  vc->ncstack[vc->ncstackpos] = pos;
  vc->ncstackpos += 1;
}


static void add_struct_mapping(Scheme_Hash_Table **_st_ht, int pos, int shape)
{
  if (!*_st_ht) {
    Scheme_Hash_Table *ht;
    ht = scheme_make_hash_table_eqv();
    *_st_ht = ht;
  }
  scheme_hash_set(*_st_ht, 
                  scheme_make_integer(pos),
                  scheme_make_integer(shape));
}

static int phaseless_expr(Scheme_Object *expr)
{
  /* A precise check is a little tricky, since compiler optimizations
     might change the original program beyond easily recognition of
     the syntactic pattern that defines "phaseless". For now, let
     anything through; the result can be weird if state somehow leakes
     through a "phaseless" module, but I don't think it can be unsafe
     from the run-time system's perspective. */
  return 1;
}

void scheme_validate_code(Mz_CPort *port, Scheme_Object *code,
                          int depth, 
                          int num_toplevels, int num_stxes, int num_lifts, void *tl_use_map,
                          Scheme_Object **toplevels,
                          int code_vec)
/* code_vec == 2 => check that phasesless is ok */
{
  char *stack;
  int delta;
  struct Validate_Clearing *vc;
  Validate_TLS tls;
  mzshort *tl_state;
  Scheme_Hash_Table *st_ht = NULL;
  Scheme_Object *form;

  depth += ((num_toplevels || num_stxes || num_lifts) ? 1 : 0);

  stack = scheme_malloc_atomic(depth);
  memset(stack, VALID_NOT, depth);
  
  if (num_toplevels || num_stxes || num_lifts) {
    stack[depth - 1] = VALID_TOPLEVELS;
  }

  delta = depth - ((num_toplevels || num_stxes || num_lifts) ? 1 : 0);

  tls = MALLOC_N(mzshort*, num_lifts);
  
  if (code_vec) {
    int i;
    tl_state = MALLOC_N_ATOMIC(mzshort, num_toplevels);
    memset(tl_state, 0, sizeof(mzshort) * num_toplevels);
    for (i = 0; i < num_toplevels; i++) {
      if (SAME_TYPE(SCHEME_TYPE(toplevels[i]), scheme_module_variable_type)) {
        int mv_flags = SCHEME_MODVAR_FLAGS(toplevels[i]);
        if (mv_flags & SCHEME_MODVAR_CONST) {
          intptr_t k;
          tl_state[i] = SCHEME_TOPLEVEL_CONST;
          if (scheme_decode_struct_shape(((Module_Variable *)toplevels[i])->shape, &k))
            add_struct_mapping(&st_ht, i, k);
        } else if (mv_flags & SCHEME_MODVAR_FIXED)
          tl_state[i] = SCHEME_TOPLEVEL_FIXED;
        else
          tl_state[i] = SCHEME_TOPLEVEL_READY;
      }
      if (0) {
        if (i < num_toplevels) {
          if (SCHEME_SYMBOLP(toplevels[i]))
            printf("%d is %s\n", i, SCHEME_SYM_VAL(toplevels[i]));
          if (SAME_TYPE(SCHEME_TYPE(toplevels[i]), scheme_module_variable_type))
            printf("%d is imported %s (%d)\n", i, 
                   SCHEME_SYM_VAL(((Module_Variable *)toplevels[i])->sym),
                   SCHEME_MODVAR_FLAGS(toplevels[i]) & 0x3);
        }
      }
    }
  } else {
    tl_state = NULL;
  }

  vc = make_clearing_stack();

  if (code_vec) {
    int i, cnt, tl_timestamp = 1;
    cnt = SCHEME_VEC_SIZE(code);
    for (i = 0; i < cnt; i++) {
      form = SCHEME_VEC_ELS(code)[i];
      if (code_vec == 2) {
        if (SAME_TYPE(SCHEME_TYPE(form), scheme_define_values_type)) {
          if (!phaseless_expr(SCHEME_VEC_ELS(form)[0]))
            scheme_ill_formed_code(port);
        } else
          scheme_ill_formed_code(port);
      }
      reset_clearing(vc);
      if (!validate_expr(port, form, 
                         stack, tls,
                         depth, delta, delta, 
                         num_toplevels, num_stxes, num_lifts, tl_use_map,
                         tl_state, tl_timestamp,
                         NULL, 0, 0,
                         vc, 1, 0, NULL, -1, &st_ht)) {
        tl_timestamp++;
        if (0) {
          printf("increment to %d for %d %p\n", tl_timestamp, 
                 SCHEME_TYPE(SCHEME_VEC_ELS(code)[i]), 
                 SCHEME_VEC_ELS(code)[i]);
        }
      }
    }
  } else {
    validate_expr(port, code, 
                  stack, tls,
                  depth, delta, delta, 
                  num_toplevels, num_stxes, num_lifts, tl_use_map,
                  tl_state, 0,
                  NULL, 0, 0,
                  vc, 1, 0, NULL, -1, NULL);
  }
}

static int validate_join(int r1, int r2)
/* both r1 and r2 is result */
{
  if (!r1 || !r2) return 0;
  if ((r1 == 2) && (r2 == 2)) return 2;
  return 1;
}

static int validate_join_seq(int r1, int r2)
/* only r2 is result */
{
  if (!r1 || !r2) return 0;
  return r2;
}

/*========================================================================*/
/*                            other syntax                                */
/*========================================================================*/

static int validate_toplevel(Scheme_Object *expr, Mz_CPort *port,
                             char *stack, Validate_TLS tls,
                             int depth, int delta, 
                             int num_toplevels, int num_stxes, int num_lifts, void *tl_use_map,
                             mzshort *tl_state, mzshort tl_timestamp,
                             int skip_refs_check)
{
  if (!SAME_TYPE(scheme_toplevel_type, SCHEME_TYPE(expr)))
    scheme_ill_formed_code(port);

  return validate_expr(port, expr, stack, tls, 
                       depth, delta, delta, 
                       num_toplevels, num_stxes, num_lifts, tl_use_map,
                       tl_state, tl_timestamp,
                       NULL, skip_refs_check ? 1 : 0, 0,
                       make_clearing_stack(), 0, 0, NULL, 1, NULL);
}

static int define_values_validate(Scheme_Object *data, Mz_CPort *port, 
                                  char *stack,  Validate_TLS tls,
                                  int depth, int letlimit, int delta, 
                                  int num_toplevels, int num_stxes, int num_lifts,
                                  void *tl_use_map, 
                                  mzshort *tl_state, mzshort tl_timestamp,
                                  int result_ignored,
                                  struct Validate_Clearing *vc, int tailpos,
                                  Scheme_Hash_Tree *procs,
                                  Scheme_Hash_Table **_st_ht)
{
  int i, size, flags, result, is_struct;
  Simple_Stuct_Type_Info stinfo;
  Scheme_Object *val, *only_var;

  val = SCHEME_VEC_ELS(data)[0];
  size = SCHEME_VEC_SIZE(data);

  if (size == 2)
    only_var = SCHEME_VEC_ELS(data)[1];
  else
    only_var = NULL;
  
  for (i = 1; i < size; i++) {
    validate_toplevel(SCHEME_VEC_ELS(data)[i], port, stack, tls, depth, delta, 
                      num_toplevels, num_stxes, num_lifts, tl_use_map,
                      NULL, tl_timestamp,
                      1);
  }

  if (only_var) {
    int pos;
    pos = SCHEME_TOPLEVEL_POS(only_var);
    if (pos >= (num_toplevels + num_stxes + (num_stxes ? 1 : 0))) {
      /* It's a lift. Check whether it needs to take reference arguments
         and/or install reference info. */
      Scheme_Object *app_rator;
      Scheme_Closure_Data *data = NULL;
      int tp = pos - (num_toplevels + num_stxes + (num_stxes ? 1 : 0));
      mzshort *a, *new_a = NULL;

      /* Make sure that no one has tried to register information. */
      a = tls[tp];
      if (a && (a != (mzshort *)0x1) && (a[0] < 1))
        scheme_ill_formed_code(port);

      /* Convert rator to ref-arg info: */
      app_rator = val;
      while (1) {
        if (SAME_TYPE(SCHEME_TYPE(app_rator), scheme_closure_type)) {
          data = SCHEME_COMPILED_CLOS_CODE(app_rator);
          break;
        } else if (SAME_TYPE(SCHEME_TYPE(app_rator), scheme_unclosed_procedure_type)) {
          data = (Scheme_Closure_Data *)app_rator;
          break;
        } else if (SAME_TYPE(SCHEME_TYPE(app_rator), scheme_toplevel_type)) {
          /* Record an indirection */
          data = NULL;
          new_a = MALLOC_N_ATOMIC(mzshort, 2);
          new_a[0] = 0;
          new_a[1] = SCHEME_TOPLEVEL_POS(app_rator);
          break;
        } else {
          /* Not a procedure */
          data = NULL;
          new_a = (mzshort *)0x1;
          break;
        }
      }
      if (data) {
        if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_TYPED_ARGS) {
          int sz;
          sz = data->num_params;
          new_a = MALLOC_N_ATOMIC(mzshort, (sz + 2));
          new_a[0] = -sz;
          new_a[sz+1] = !!(SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST);
          for (i = 0; i < sz; i++) {
            int ct;
            ct = scheme_boxmap_get(data->closure_map, i, data->closure_size);
            if (ct == CLOS_TYPE_BOXED)
              new_a[i + 1] = 1;
            else
              new_a[i + 1] = 0;
          }
        } else {
          new_a = (mzshort *)0x1;
        }
      }

      /* Install info: */
      tls[tp] = new_a;

      /* Check old hopes against actual */
      if (a == (mzshort *)0x1) {
        if (new_a != (mzshort *)0x1)
          scheme_ill_formed_code(port);
      } else if (a) {
        int cnt = a[0], i;

        for (i = 0; i < cnt; i++) {
          if (a[i + 1]) {
            int is;
            is = validate_rator_wants_box(val, i, 
                                          a[i + 1] == 2,
                                          tls, num_toplevels, num_stxes, num_lifts, tl_use_map);
            if ((is && (a[i + 1] == 1))
                || (!is && (a[i + 1] == 2)))
              scheme_ill_formed_code(port);
          }
        }
      }
    } else
      only_var = NULL;
  }

  if (scheme_is_simple_make_struct_type(val, size-1, 1, 1, NULL,
                                        &stinfo,
                                        NULL, (_st_ht ? *_st_ht : NULL), 
                                        NULL, 0, NULL, NULL, 5)) {
    /* This set of bindings is constant across invocations, but
       if `uses_super', we need to increment tl_timestamp for
       subtype-defining `struct' sequences. */
    is_struct = 1;
  } else {
    is_struct = 0;
  }

  result = validate_expr(port, val, stack, tls, 
                         depth, letlimit, delta, 
                         num_toplevels, num_stxes, num_lifts, tl_use_map,
                         tl_state, tl_timestamp + ((is_struct && stinfo.uses_super) ? 1 : 0),
                         NULL, !!only_var, 0, vc, 0, 0, NULL,
                         size-1, _st_ht);

  if (is_struct) {
    if (_st_ht) {
      /* Record `struct:' binding as constant across invocations,
         so that it can be recognized for sub-struct declarations,
         and so on: */
      for (i = 1; i < size; i++) {
        /* For the struct:, we need the init and field counts to be the
           same, otherwise anything is fine: */
        if ((i > 1)
            || (stinfo.field_count == stinfo.init_field_count))
          add_struct_mapping(_st_ht, 
                             SCHEME_TOPLEVEL_POS(SCHEME_VEC_ELS(data)[i]),
                             scheme_get_struct_proc_shape(i-1, &stinfo));
      }
    }
    /* In any case, treat the bindings as constant */
    result = 2;
  }

  flags = SCHEME_TOPLEVEL_READY;
  if (result == 2) {
    /* We may treat more things as constant (more than fixed)
       than the compiler would in terms of copyable literals, but
       that's good enough for ensuring safety. */
    flags = SCHEME_TOPLEVEL_CONST;
  }

  for (i = 1; i < size; i++) {
    int ts = (tl_timestamp + (result ? 0 : 1));
    if (tl_state) {
      int p = SCHEME_TOPLEVEL_POS(SCHEME_VEC_ELS(data)[i]);
      if (p < num_toplevels) {
        int s = -tl_state[p];
        int expected_flags = s & SCHEME_TOPLEVEL_FLAGS_MASK;
        int this_flags = flags;
        if ((this_flags == SCHEME_TOPLEVEL_READY)
            && (SCHEME_TOPLEVEL_FLAGS(SCHEME_VEC_ELS(data)[i]) & SCHEME_TOPLEVEL_SEAL))
          this_flags = SCHEME_TOPLEVEL_FIXED;
        if (0) {
          printf("%d is %d for %d %p; at %d\n", p, this_flags, SCHEME_TYPE(val), val, ts);
        }
        if (tl_state[p] > 0)
          scheme_ill_formed_code(port);
        if ((expected_flags > this_flags)
            || (expected_flags 
                /* Use "<=" instead of "<" to prevent things like
                   `(define x x)' with `x' claimed as constant. The
                   `tl_timestamp++' before checking a closure body
                   allows things like `(define x (lambda () x))'. */
                && ((s >> 2) <= ts)))
          scheme_ill_formed_code(port);
        tl_state[p] = (ts << 2) | this_flags;
      }
    }
  }

  return result;
}

static int set_validate(Scheme_Object *data, Mz_CPort *port, 
                        char *stack, Validate_TLS tls,
                        int depth, int letlimit, int delta, 
                        int num_toplevels, int num_stxes, int num_lifts, 
                        void *tl_use_map, 
                        mzshort *tl_state, mzshort tl_timestamp,
                        int result_ignored,
                        struct Validate_Clearing *vc, int tailpos,
                        Scheme_Hash_Tree *procs)
{
  Scheme_Set_Bang *sb = (Scheme_Set_Bang *)data;
  int r1, r2;
  
  r1 = validate_expr(port, sb->val, stack, tls, depth, letlimit, delta, 
                     num_toplevels, num_stxes, num_lifts, tl_use_map,
                     tl_state, tl_timestamp,
                     NULL, 0, 0, vc, 0, 0, procs, 1, NULL);
  r2 = validate_toplevel(sb->var, port, stack, tls, depth, delta, 
                         num_toplevels, num_stxes, num_lifts, tl_use_map,
                         tl_state, tl_timestamp,
                         0);

  return validate_join(validate_join_seq(r1, r2), 2);
}

static void ref_validate(Scheme_Object *data, Mz_CPort *port, 
			 char *stack, Validate_TLS tls,
                         int depth, int letlimit, int delta, 
                         int num_toplevels, int num_stxes, int num_lifts, 
                         void *tl_use_map, 
                         mzshort *tl_state, mzshort tl_timestamp,
                         int result_ignored,
                         struct Validate_Clearing *vc, int tailpos,
                         Scheme_Hash_Tree *procs)
{
  validate_toplevel(SCHEME_PTR1_VAL(data), port, stack, tls, depth, delta, 
                    num_toplevels, num_stxes, num_lifts, tl_use_map,
                    tl_state, tl_timestamp,
                    0);
  if (!SCHEME_FALSEP(SCHEME_PTR2_VAL(data)))
    validate_toplevel(SCHEME_PTR2_VAL(data), port, stack, tls, depth, delta, 
                      num_toplevels, num_stxes, num_lifts, tl_use_map,
                      tl_state, tl_timestamp,
                      0);
}

static int apply_values_validate(Scheme_Object *data, Mz_CPort *port, 
                                 char *stack, Validate_TLS tls,
                                 int depth, int letlimit, int delta, 
                                 int num_toplevels, int num_stxes, int num_lifts,
                                 void *tl_use_map, 
                                 mzshort *tl_state, mzshort tl_timestamp,
                                 int result_ignored,
                                 struct Validate_Clearing *vc, int tailpos,
                                 Scheme_Hash_Tree *procs)
{
  Scheme_Object *f, *e;
  int r1, r2;

  f = SCHEME_PTR1_VAL(data);
  e = SCHEME_PTR2_VAL(data);

  r1 = validate_expr(port, f, stack, tls,
                     depth, letlimit, delta, 
                     num_toplevels, num_stxes, num_lifts, tl_use_map,
                     tl_state, tl_timestamp,
                     NULL, 0, 0, vc, 0, 0, procs, 1, NULL);
  r2 = validate_expr(port, e, stack, tls,
                     depth, letlimit, delta, 
                     num_toplevels, num_stxes, num_lifts, tl_use_map,
                     tl_state, tl_timestamp,
                     NULL, 0, 0, vc, 0, 0, procs, -1, NULL);

  return validate_join(r1, r2);
}

static void inline_variant_validate(Scheme_Object *data, Mz_CPort *port, 
                                    char *stack, Validate_TLS tls,
                                    int depth, int letlimit, int delta, 
                                    int num_toplevels, int num_stxes, int num_lifts,
                                    void *tl_use_map, 
                                    mzshort *tl_state, mzshort tl_timestamp,
                                    int result_ignored,
                                    struct Validate_Clearing *vc, int tailpos,
                                    Scheme_Hash_Tree *procs)
{
  Scheme_Object *f1, *f2;

  f1 = SCHEME_VEC_ELS(data)[0];
  f2 = SCHEME_VEC_ELS(data)[1];
  
  validate_expr(port, f1, stack, tls,
                depth, letlimit, delta, 
                num_toplevels, num_stxes, num_lifts, tl_use_map,
                tl_state, tl_timestamp,
                NULL, 0, 0, vc, 0, 0, procs, 1, NULL);
  validate_expr(port, f2, stack, tls,
                depth, letlimit, delta, 
                num_toplevels, num_stxes, num_lifts, tl_use_map,
                tl_state, tl_timestamp,
                NULL, 0, 0, vc, 0, 0, procs, 1, NULL);
}

static void case_lambda_validate(Scheme_Object *data, Mz_CPort *port, char *stack, Validate_TLS tls,
				 int depth, int letlimit, int delta, 
                                 int num_toplevels, int num_stxes, int num_lifts, 
                                 void *tl_use_map, 
                                 mzshort *tl_state, mzshort tl_timestamp,
                                 int result_ignored,
                                 struct Validate_Clearing *vc, int tailpos,
                                 Scheme_Hash_Tree *procs)
{
  Scheme_Case_Lambda *seq = (Scheme_Case_Lambda *)data;
  Scheme_Object *e;
  int i;

  if (!SAME_TYPE(SCHEME_TYPE(data), scheme_case_lambda_sequence_type))
    scheme_ill_formed_code(port);

  for (i = 0; i < seq->count; i++) { 
    e = seq->array[i];
    if (!SAME_TYPE(SCHEME_TYPE(e), scheme_unclosed_procedure_type)
        && !SAME_TYPE(SCHEME_TYPE(e), scheme_closure_type))
      scheme_ill_formed_code(port);
    validate_expr(port, e, stack, tls, depth, letlimit, delta, 
                  num_toplevels, num_stxes, num_lifts, tl_use_map,
                  tl_state, tl_timestamp,
                  NULL, 0, 0, vc, 0, 0, procs, 1, NULL);
  }
}

static void validate_boxenv(int p, Mz_CPort *port, char *stack, int depth, int delta, int letlimit)
{
  if (p >= 0)
    p += delta;

  if ((p < 0) || (p >= letlimit) || (stack[p] != VALID_VAL))
    scheme_ill_formed_code(port);

  stack[p] = VALID_BOX;
}

static int bangboxenv_validate(Scheme_Object *data, Mz_CPort *port, 
                               char *stack, Validate_TLS tls,
                               int depth, int letlimit, int delta, 
                               int num_toplevels, int num_stxes, int num_lifts, 
                               void *tl_use_map, 
                               mzshort *tl_state, mzshort tl_timestamp,
                               int result_ignored,
                               struct Validate_Clearing *vc, int tailpos,
                               Scheme_Hash_Tree *procs,
                               int expected_results)
{
  validate_boxenv(SCHEME_INT_VAL(SCHEME_PTR1_VAL(data)), port, stack, depth, delta, letlimit);

  return validate_expr(port, SCHEME_PTR2_VAL(data), stack, tls, depth, letlimit, delta, 
                       num_toplevels, num_stxes, num_lifts, tl_use_map,
                       tl_state, tl_timestamp,
                       NULL, 0, result_ignored, vc, tailpos, 0, procs, expected_results, NULL);
}

static int begin0_validate(Scheme_Object *data, Mz_CPort *port, 
                           char *stack, Validate_TLS tls,
                           int depth, int letlimit, int delta, 
                           int num_toplevels, int num_stxes, int num_lifts,
                           void *tl_use_map, 
                           mzshort *tl_state, mzshort tl_timestamp,
                           int result_ignored,
                           struct Validate_Clearing *vc, int tailpos,
                           Scheme_Hash_Tree *procs,
                           int expected_results)
{
  Scheme_Sequence *seq = (Scheme_Sequence *)data;
  int i, r, result = 2;

  if (!SAME_TYPE(SCHEME_TYPE(seq), scheme_begin0_sequence_type)
      && !SAME_TYPE(SCHEME_TYPE(seq), scheme_sequence_type))
    scheme_ill_formed_code(port);

  for (i = 0; i < seq->count; i++) { 
    r = validate_expr(port, seq->array[i], stack, tls,
                      depth, letlimit, delta, 
                      num_toplevels, num_stxes, num_lifts, tl_use_map,
                      tl_state, tl_timestamp,
                      NULL, 0, i > 0, vc, 0, 0, procs, 
                      (i > 0) ? -1 : expected_results, NULL);
    result = validate_join_seq(r, result);
  }

  return result;
}

static void do_define_syntaxes_validate(Scheme_Object *data, Mz_CPort *port, 
					char *stack, Validate_TLS tls,
                                        int depth, int letlimit, int delta, 
					int num_toplevels, int num_stxes, int num_lifts, void *tl_use_map,
                                        mzshort *tl_state, mzshort tl_timestamp,
					int for_stx)
{
  Resolve_Prefix *rp;
  Scheme_Object *name, *val, *base_stack_depth, *dummy;
  int sdepth;

  if (!SAME_TYPE(SCHEME_TYPE(data), (for_stx ? scheme_begin_for_syntax_type : scheme_define_syntaxes_type))
      || (SCHEME_VEC_SIZE(data) < 4))
    scheme_ill_formed_code(port);

  rp = (Resolve_Prefix *)SCHEME_VEC_ELS(data)[1];
  base_stack_depth = SCHEME_VEC_ELS(data)[2];
  sdepth = SCHEME_INT_VAL(base_stack_depth);

  if (!SAME_TYPE(rp->so.type, scheme_resolve_prefix_type)
      || (sdepth < 0))
    scheme_ill_formed_code(port);

  dummy = SCHEME_VEC_ELS(data)[3];

  if (!for_stx) {
    int i, size;
    size = SCHEME_VEC_SIZE(data);
    for (i = 4; i < size; i++) {
      name = SCHEME_VEC_ELS(data)[i];
      if (!SCHEME_SYMBOLP(name)) {
	scheme_ill_formed_code(port);
      }
    }
  }

  validate_toplevel(dummy, port, stack, tls, depth, delta, 
                    num_toplevels, num_stxes, num_lifts, tl_use_map,
                    tl_state, tl_timestamp,
                    0);
  
  if (!for_stx) {
    scheme_validate_code(port, SCHEME_VEC_ELS(data)[0], sdepth, rp->num_toplevels, rp->num_stxes, rp->num_lifts, 
                         NULL, NULL, 0);
  } else {
    val = SCHEME_VEC_ELS(data)[0];
    while (SCHEME_PAIRP(val)) {
      scheme_validate_code(port, SCHEME_CAR(val), sdepth, rp->num_toplevels, rp->num_stxes, rp->num_lifts, 
                           NULL, NULL, 0);
      val = SCHEME_CDR(val);
    }
    if (!SCHEME_NULLP(val))
      scheme_ill_formed_code(port);
  }
}

static void define_syntaxes_validate(Scheme_Object *data, Mz_CPort *port, 
				     char *stack, Validate_TLS tls,
                                     int depth, int letlimit, int delta, 
				     int num_toplevels, int num_stxes, int num_lifts, 
                                     void *tl_use_map, 
                                     mzshort *tl_state, mzshort tl_timestamp,
                                     int result_ignored,
                                     struct Validate_Clearing *vc, int tailpos,
                                     Scheme_Hash_Tree *procs)
{
  do_define_syntaxes_validate(data, port, stack, tls, depth, letlimit, delta, 
                              num_toplevels, num_stxes, num_lifts, tl_use_map, 
                              tl_state, tl_timestamp, 0);
}

static void begin_for_syntaxes_validate(Scheme_Object *data, Mz_CPort *port, 
                                        char *stack, Validate_TLS tls,
                                        int depth, int letlimit, int delta, 
                                        int num_toplevels, int num_stxes, int num_lifts, 
                                        void *tl_use_map, 
                                        mzshort *tl_state, mzshort tl_timestamp,
                                        int result_ignored,
                                        struct Validate_Clearing *vc, int tailpos,
                                        Scheme_Hash_Tree *procs)
{
  do_define_syntaxes_validate(data, port, stack, tls, depth, letlimit, delta, 
                              num_toplevels, num_stxes, num_lifts, tl_use_map, 
                              tl_state, tl_timestamp,1);
}

/*========================================================================*/
/*                            expressions                                 */
/*========================================================================*/

static Scheme_Object *validate_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Mz_CPort *port = (Mz_CPort *)p->ku.k.p1;
  Scheme_Object *expr = (Scheme_Object *)p->ku.k.p2;
  char *stack = (char *)p->ku.k.p3;
  int *args = (int *)(((void **)p->ku.k.p5)[0]);
  Scheme_Object *app_rator = (Scheme_Object *)(((void **)p->ku.k.p5)[1]);
  Validate_TLS tls = (Validate_TLS)(((void **)p->ku.k.p5)[2]);
  Scheme_Hash_Tree *procs = (Scheme_Hash_Tree *)(((void **)p->ku.k.p5)[3]);
  struct Validate_Clearing *vc = (struct Validate_Clearing *)p->ku.k.p4;
  void *tl_use_map = (((void **)p->ku.k.p5)[4]);
  mzshort *tl_state = (((void **)p->ku.k.p5)[5]);
  Scheme_Hash_Table **_st_ht = (((void **)p->ku.k.p5)[6]);
  int r;
  
  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;
  p->ku.k.p5 = NULL;

  r = validate_expr(port, expr, stack, tls,
                    args[0], args[1], args[2],
                    args[3], args[4], args[5], tl_use_map,
                    tl_state, args[10],
                    app_rator, args[6], args[7], vc, args[8],
                    args[9], procs, args[11],
                    _st_ht);
  
  return scheme_make_integer(r);
}

/* FIXME: need to validate that a flonum is provided when a
   procedure expects a flonum */

int validate_rator_wants_box(Scheme_Object *app_rator, int pos,
                             int hope,
                             Validate_TLS tls,
                             int num_toplevels, int num_stxes, int num_lifts, void *tl_use_map)
{
  Scheme_Closure_Data *data = NULL;
  Scheme_Type ty;

  while (1) {
    ty = SCHEME_TYPE(app_rator);
    if (SAME_TYPE(ty, scheme_closure_type)) {
      data = SCHEME_COMPILED_CLOS_CODE(app_rator);
      break;
    } else if (SAME_TYPE(ty, scheme_unclosed_procedure_type)) {
      data = (Scheme_Closure_Data *)app_rator;
      break;
    } else if (SAME_TYPE(ty, scheme_toplevel_type)) {
      int p;
      p = SCHEME_TOPLEVEL_POS(app_rator);
      while (1) {
        if (p >= (num_toplevels + num_stxes + (num_stxes ? 1 : 0))) {
          /* It's a lift. Check that the lift is defined, and that it
             doesn't want reference arguments. */
          mzshort *a; /* 0x1 => no ref args, 
                         ptr with pos length => expected (0 => don't care, 1 => want not, 2 => want is), 
                         ptr with neg length => actual
                         ptr with 0 => another top-level */
          int tp;

          tp = (p - (num_toplevels + num_stxes + (num_stxes ? 1 : 0)));
          if (tp >= num_lifts)
            return 0;
            
          a = tls[tp];
          if (a == (mzshort *)0x1) {
            return 0;
          } else if (!a || (a[0] > 0)) {
            /* The lift isn't ready. 
               Record what we expect to find when it is ready. */
            if (!a || (a[0] < (pos + 1))) {
              mzshort *naya;
              int sz;
              if (a)
                sz = a[0];
              else
                sz = 3;
              sz *= 2;
              if (sz <= pos)
                sz = pos + 1;
              naya = scheme_malloc_atomic((sz + 1) * sizeof(mzshort));
              memset(naya, 0, (sz + 1) * sizeof(mzshort));
              if (a)
                memcpy(naya, a, (a[0] + 1) * sizeof(mzshort));
              naya[0] = sz;
              a = naya;
              tls[tp] = a;
            }

            if (!a[pos + 1]) {
              a[pos + 1] = hope ? 2 : 1;
              return hope;
            } else if (a[pos + 1] == 2)
              return 1;
            else
              return 0;
          } else if (!a[0]) {
            /* try again */
            p = a[1];
          } else {
            if (pos >= -a[0]) {
              /* last slot indicates whether rest args are allowed */
              return (a[-a[0]+1] ? hope : !hope);
            } else
              return a[pos + 1];
          }
        } else
          return 0;
      }
    } else
      return 0;
  }

  if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_TYPED_ARGS) {
    if (pos < data->num_params) {
      int ct;
      ct = scheme_boxmap_get(data->closure_map, pos, data->closure_size);
      if (ct == CLOS_TYPE_BOXED)
        return 1;
    }
  }

  return 0;
}

static int argument_to_arity_error(Scheme_Object *app_rator, int proc_with_refs_ok)
{
  /* Since `raise-arity-error' doesn't actually apply its argument,
     it's ok to pass any procedure. In particular, the compiler generates
     calls to converted procedures. */
  return ((proc_with_refs_ok == 2)
          && SAME_OBJ(app_rator, scheme_raise_arity_error_proc));
}

void scheme_validate_closure(Mz_CPort *port, Scheme_Object *expr, 
                             char *closure_stack, Validate_TLS tls,
                             int num_toplevels, int num_stxes, int num_lifts, void *tl_use_map,
                             mzshort *tl_state, mzshort tl_timestamp,
                             int self_pos_in_closure, Scheme_Hash_Tree *procs)
{
  Scheme_Closure_Data *data = (Scheme_Closure_Data *)expr;
  int i, sz, cnt, base, base2;
  char *new_stack;
  struct Validate_Clearing *vc;

  if (data->max_let_depth < (data->num_params + data->closure_size))
    scheme_ill_formed_code(port);

  sz = data->max_let_depth;
  new_stack = scheme_malloc_atomic(sz);
  memset(new_stack, VALID_NOT, sz - data->num_params - data->closure_size);

  cnt = data->num_params;
  base = sz - cnt;

  if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_TYPED_ARGS) {
    base2 = data->closure_size;
    for (i = 0; i < cnt; i++) {
      new_stack[base + i] = closure_stack[base2 + i];
    }
  } else {
    for (i = 0; i < cnt; i++) {
      new_stack[i + base] = VALID_VAL;
    }
  }

  cnt = data->closure_size;
  base = base - cnt;
  for (i = 0; i < cnt; i++) {
    new_stack[i + base] = closure_stack[i];
  }

  vc = make_clearing_stack();
  if (self_pos_in_closure >= 0) {
    vc->self_pos = base + self_pos_in_closure;
    vc->self_count = data->closure_size;
    vc->self_start = base;
  }

  if (data->tl_map) {
    if (tl_use_map) {
      /* check that data->tl_use_map => tl_use_map */
      int *a, a_buf[2], len;

      if ((uintptr_t)tl_use_map & 0x1) {
        len = 1;
        a_buf[1] = (((uintptr_t)data->tl_map) >> 1) & 0x7FFFFFFF;
        a = a_buf;
      } else {
        len = ((int *)tl_use_map)[0];
        a = (int *)tl_use_map;
      }

      if (tl_use_map) {
        if ((uintptr_t)data->tl_map & 0x1) {
          int map = (((uintptr_t)data->tl_map) >> 1) & 0x7FFFFFFF;
          if ((len < 1) || ((a[1] & map) != map))
            scheme_ill_formed_code(port);
        } else {
          int *b = ((int *)data->tl_map);
          for (i = b[0]; i--; ) {
            if ((len <= i) || ((a[i+1] & b[i+1]) != b[i+1]))
              scheme_ill_formed_code(port);
          }
        }
      }
    }
    tl_use_map = data->tl_map;
  }

  validate_expr(port, data->code, new_stack, tls, sz, sz, base, 
                num_toplevels, num_stxes, num_lifts, tl_use_map,
                tl_state, tl_timestamp,
                NULL, 0, 0, vc, 1, 0, procs, -1, NULL);
}

static Scheme_Hash_Tree *as_nonempty_procs(Scheme_Hash_Tree *procs)
{
  if (!procs)
    procs = scheme_make_hash_tree(0);
  return procs;
}

static void validate_unclosed_procedure(Mz_CPort *port, Scheme_Object *expr, 
                                        char *stack, Validate_TLS tls,
                                        int depth, int delta, 
                                        int num_toplevels, int num_stxes, int num_lifts, void *tl_use_map,
                                        mzshort *tl_state, mzshort tl_timestamp,
                                        Scheme_Object *app_rator, int proc_with_refs_ok,
                                        int self_pos, Scheme_Hash_Tree *procs)
{
  Scheme_Closure_Data *data = (Scheme_Closure_Data *)expr;
  int i, cnt, q, p, sz, base, stack_delta, vld, self_pos_in_closure = -1, typed_arg = 0;
  mzshort *map;
  char *closure_stack;
  Scheme_Object *proc;
  Scheme_Hash_Tree *new_procs = NULL;
      
  if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_TYPED_ARGS) {
    sz = data->closure_size + data->num_params;
  } else {
    sz = data->closure_size;
  }
  map = data->closure_map;
  
  if (sz)
    closure_stack = scheme_malloc_atomic(sz);
  else
    closure_stack = NULL;

  if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_TYPED_ARGS) {
    cnt = data->num_params;
    base = sz - cnt;
    for (i = 0; i < cnt; i++) {
      int ct;
      ct = scheme_boxmap_get(map, i, data->closure_size);
      if (ct == CLOS_TYPE_BOXED) {
        vld = VALID_BOX;
        typed_arg = 1;
      } else if (ct) {
        vld = (VALID_TYPED + (ct - CLOS_TYPE_TYPE_OFFSET));
        typed_arg = 1;
      } else
        vld = VALID_VAL;
      closure_stack[i + base] = vld;
    }
  } else {
    base = sz;
  }

  cnt = data->closure_size;
  base = base - cnt;
  stack_delta = data->max_let_depth - sz;

  for (i = 0; i < cnt; i++) {
    q = map[i];
    if (q == self_pos)
      self_pos_in_closure = i;
    p = q + delta;
    if ((q < 0) || (p < 0) || (p >= depth) || (stack[p] <= VALID_UNINIT))
      scheme_ill_formed_code(port);
    vld = stack[p];
    if (vld == VALID_VAL_NOCLEAR)
      vld = VALID_VAL;
    else if (vld == VALID_BOX_NOCLEAR)
      vld = VALID_BOX;

    if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_TYPED_ARGS) {
      int pos = data->num_params + i;
      int ct;
      ct = scheme_boxmap_get(map, pos, data->closure_size);
      if (ct == CLOS_TYPE_BOXED)
        scheme_ill_formed_code(port);
      if (ct > CLOS_TYPE_TYPE_OFFSET) {
        if (vld != (VALID_TYPED + (ct - CLOS_TYPE_TYPE_OFFSET)))
          vld = VALID_NOT;
      } else if (vld > VALID_TYPED)
        vld = VALID_NOT;
    } else if (vld > VALID_TYPED)
      vld = VALID_NOT;

    closure_stack[i + base] = vld;

    if (procs) {
      proc = scheme_hash_tree_get(procs, scheme_make_integer(p));
      if (proc)
        new_procs = scheme_hash_tree_set(as_nonempty_procs(new_procs), 
                                         scheme_make_integer(i + base + stack_delta),
                                         proc);
    }
  }

  if (typed_arg) {
    if ((proc_with_refs_ok != 1)
        && !argument_to_arity_error(app_rator, proc_with_refs_ok))
      scheme_ill_formed_code(port);
  }

  tl_timestamp++; /* closure delays use; needed for self-use <= check */
      
  if (SCHEME_RPAIRP(data->code)) {
    /* Delay validation */
    Scheme_Object *vec;
    vec = scheme_make_vector(11, NULL);
    SCHEME_VEC_ELS(vec)[0] = SCHEME_CAR(data->code);
    SCHEME_VEC_ELS(vec)[1] = (Scheme_Object *)closure_stack;
    SCHEME_VEC_ELS(vec)[2] = (Scheme_Object *)tls;
    SCHEME_VEC_ELS(vec)[3] = scheme_make_integer(num_toplevels);
    SCHEME_VEC_ELS(vec)[4] = scheme_make_integer(num_stxes);
    SCHEME_VEC_ELS(vec)[5] = scheme_make_integer(num_lifts);
    SCHEME_VEC_ELS(vec)[6] = scheme_make_integer(self_pos_in_closure);
    SCHEME_VEC_ELS(vec)[7] = new_procs ? (Scheme_Object *)new_procs : scheme_false;
    SCHEME_VEC_ELS(vec)[8] = tl_use_map ? tl_use_map : scheme_false;
    SCHEME_VEC_ELS(vec)[9] = tl_state ? (Scheme_Object *)tl_state : scheme_false;
    SCHEME_VEC_ELS(vec)[10] = scheme_make_integer(tl_timestamp);
    SCHEME_CAR(data->code) = vec;
  } else
    scheme_validate_closure(port, expr, closure_stack, tls, 
                            num_toplevels, num_stxes, num_lifts, tl_use_map,
                            tl_state, tl_timestamp,
                            self_pos_in_closure, new_procs);
}

static void check_self_call_valid(Scheme_Object *rator, Mz_CPort *port, struct Validate_Clearing *vc, 
                                  int delta, char *stack)
{
  if ((vc->self_pos >= 0)
      && SAME_TYPE(SCHEME_TYPE(rator), scheme_local_type)
      && !SCHEME_GET_LOCAL_FLAGS(rator)
      && ((SCHEME_LOCAL_POS(rator) + delta) == vc->self_pos)) {
    /* For a self call, the JIT needs the closure data to be intact. */
    int i, pos;
    for (i = vc->self_count; i--; ) {
      pos = i + vc->self_start;
      if (stack[pos] <= VALID_UNINIT)
        scheme_ill_formed_code(port);
    }
  }
}

static void module_validate(Scheme_Object *data, Mz_CPort *port, 
                            char *stack, Validate_TLS tls,
			    int depth, int letlimit, int delta, 
			    int num_toplevels, int num_stxes, int num_lifts, 
                            void *tl_use_map, 
                            mzshort *tl_state, mzshort tl_timestamp,
                            int result_ignored,
                            struct Validate_Clearing *vc, int tailpos,
                            Scheme_Hash_Tree *procs)
{
  Scheme_Module *m;
  int i, j, cnt, let_depth;
  Resolve_Prefix *rp;
  Scheme_Object *e;

  m = (Scheme_Module *)data;

  if (!SCHEME_MODNAMEP(m->modname))
    scheme_ill_formed_code(port);

  if (m->phaseless && m->prefix->num_stxes)
    scheme_ill_formed_code(port);

  if (m->max_let_depth < 0)
    scheme_ill_formed_code(port);

  validate_toplevel(m->dummy, port, stack, tls, depth, delta, 
                    num_toplevels, num_stxes, num_lifts, tl_use_map,
                    tl_state, tl_timestamp,
                    0);

  scheme_validate_code(port, m->bodies[0], m->max_let_depth,
                       m->prefix->num_toplevels, m->prefix->num_stxes, m->prefix->num_lifts,
                       NULL, m->prefix->toplevels,
                       (m->phaseless ? 2 : 1));

  /* validate exp-time code */
  for (j = m->num_phases; j-- > 1; ) {
    cnt = SCHEME_VEC_SIZE(m->bodies[j]);
    for (i = 0; i < cnt; i++) {
      if (m->phaseless) scheme_ill_formed_code(port);

      e = SCHEME_VEC_ELS(m->bodies[j])[i];
      
      let_depth = SCHEME_INT_VAL(SCHEME_VEC_ELS(e)[2]);
      rp = (Resolve_Prefix *)SCHEME_VEC_ELS(e)[3];
      e = SCHEME_VEC_ELS(e)[1];
      
      scheme_validate_code(port, e, let_depth,
                           rp->num_toplevels, rp->num_stxes, rp->num_lifts, NULL, NULL,
                           0);
    }
  }
}

static void top_level_require_validate(Scheme_Object *data, Mz_CPort *port, 
                                       char *stack, Validate_TLS tls,
				       int depth, int letlimit, int delta, 
				       int num_toplevels, int num_stxes, int num_lifts, 
                                       void *tl_use_map, 
                                       mzshort *tl_state, mzshort tl_timestamp,
                                       int result_ignored,
                                       struct Validate_Clearing *vc, int tailpos,
                                       Scheme_Hash_Tree *procs)
{
}

static void no_typed(int need_local_type, Mz_CPort *port)
{
  if (need_local_type) scheme_ill_formed_code(port);
}

static void check_typed(Scheme_Object *expr, int need_local_type, Mz_CPort *port)
{
  if (need_local_type) {
    if (scheme_expr_produces_local_type(expr) != need_local_type)
      scheme_ill_formed_code(port);
  }
}

static int validate_join_const(int result, int expected_results)
{
  return validate_join_seq(result,
                           (((expected_results == 1) || (expected_results == -1))
                            ? 2
                            : 0));
}

static int is_functional_nonfailing_rator(Scheme_Object *rator, int num_args, int expected_results,
                                          Scheme_Hash_Table **_st_ht)
{
  if (_st_ht && *_st_ht && SAME_TYPE(SCHEME_TYPE(rator), scheme_toplevel_type)) {
    int flags = (SCHEME_TOPLEVEL_FLAGS(rator) & SCHEME_TOPLEVEL_FLAGS_MASK);
    if (flags == SCHEME_TOPLEVEL_CONST) {
      /* could be a struct operation... */
      int pos = SCHEME_TOPLEVEL_POS(rator);
      Scheme_Object *v;
      v = scheme_hash_get(*_st_ht, scheme_make_integer(pos));
      if (v) {
        int k = SCHEME_INT_VAL(v);
        if ((k & STRUCT_PROC_SHAPE_MASK) == STRUCT_PROC_SHAPE_CONSTR) {
          if (num_args == (k >> STRUCT_PROC_SHAPE_SHIFT))
            return 1;
        } else if ((k & STRUCT_PROC_SHAPE_MASK) == STRUCT_PROC_SHAPE_PRED) {
          if (num_args == 1)
            return 1;
        }
      }
    }
  }

  return scheme_is_functional_nonfailing_primitive(rator, num_args, expected_results);
}

#define CAN_RESET_STACK_SLOT 0
#if !CAN_RESET_STACK_SLOT
# define WHEN_CAN_RESET_STACK_SLOT(x) 0
#else
# define WHEN_CAN_RESET_STACK_SLOT(x) (x)
#endif

static int validate_expr(Mz_CPort *port, Scheme_Object *expr, 
                         char *stack, Validate_TLS tls,
                         int depth, int letlimit, int delta, 
                         int num_toplevels, int num_stxes, int num_lifts, void *tl_use_map,
                         mzshort *tl_state, mzshort tl_timestamp,
                         Scheme_Object *app_rator, int proc_with_refs_ok,
                         int result_ignored,
                         struct Validate_Clearing *vc, int tailpos,
                         int need_local_type, Scheme_Hash_Tree *procs,
                         int expected_results,
                         Scheme_Hash_Table **_st_ht)
/* result is 1 if result is `expected_results' values with no
   exceptions and no use of any non-ready binding; it's 2 if the
   result is furthermore a "constant" (i.e., the same shape result for
   every instantiation) */
{
  Scheme_Type type;
  int did_one = 0, vc_merge = 0, vc_merge_start = 0, result = 2;

#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Thread *p = scheme_current_thread;
    Scheme_Object *r;
    void **pr;
    int *args;
    Scheme_Hash_Table **_2st_ht = NULL;

    if (_st_ht) {
      _2st_ht = MALLOC_N(Scheme_Hash_Table*, 1);
      *_2st_ht = *_st_ht;
    }
    
    args = MALLOC_N_ATOMIC(int, 12);

    p->ku.k.p1 = (void *)port;
    p->ku.k.p2 = (void *)expr;
    p->ku.k.p3 = (void *)stack;
    p->ku.k.p4 = (void *)vc;

    args[0] = depth;
    args[1] = letlimit;
    args[2] = delta;
    args[3] = num_toplevels;
    args[4] = num_stxes;
    args[5] = num_lifts;
    args[6] = proc_with_refs_ok;
    args[7] = result_ignored;
    args[8] = tailpos;
    args[9] = need_local_type;
    args[10] = tl_timestamp;
    args[11] = expected_results;

    pr = MALLOC_N(void*, 7);
    pr[0] = (void *)args;
    pr[1] = (void *)app_rator;
    pr[2] = (void *)tls;
    pr[3] = (void *)procs;
    pr[4] = tl_use_map;
    pr[5] = tl_state;
    pr[6] = _2st_ht;

    p->ku.k.p5 = (void *)pr;

    r = scheme_handle_stack_overflow(validate_k);

    if (_st_ht) {
      *_st_ht = *_2st_ht;
    }

    return SCHEME_INT_VAL(r);
  }
#endif

 top:
  if (did_one) {
    if (app_rator) {
      if (validate_rator_wants_box(app_rator, proc_with_refs_ok - 2, 0,
                                   tls, num_toplevels, num_stxes, num_lifts,
                                   tl_use_map))
        scheme_ill_formed_code(port);
      app_rator = NULL;
    }
    proc_with_refs_ok = 0;
  } else
    did_one = 1;

  type = SCHEME_TYPE(expr);

  switch (type) {
  case scheme_toplevel_type:
    {
      int c = SCHEME_TOPLEVEL_DEPTH(expr);
      int d = c + delta;
      int p = SCHEME_TOPLEVEL_POS(expr);
      int flags = (SCHEME_TOPLEVEL_FLAGS(expr) & SCHEME_TOPLEVEL_FLAGS_MASK);

      no_typed(need_local_type, port);

      if ((c < 0) || (p < 0) || (d < 0) || (d >= depth)
	  || (stack[d] != VALID_TOPLEVELS) 
	  || (p >= (num_toplevels + num_lifts + num_stxes + (num_stxes ? 1 : 0)))
	  || ((p >= num_toplevels) && (p < num_toplevels + num_stxes + (num_stxes ? 1 : 0))))
	scheme_ill_formed_code(port);
      
      if (tl_use_map) {
        int p2 = ((p < num_toplevels) 
                  ? p
                  : (p - num_stxes));
        if (num_stxes && (p >= num_toplevels) && (p < (num_toplevels + num_stxes + 1)))
          scheme_ill_formed_code(port);
        if ((uintptr_t)tl_use_map & 0x1) {
          if (p2 > 31)
            scheme_ill_formed_code(port);
          if (!((uintptr_t)tl_use_map & (1 << (p2 + 1))))
            scheme_ill_formed_code(port);
        } else {
          if (p2 >= (*(int *)tl_use_map * 32))
            scheme_ill_formed_code(port);
          if (!(((int *)tl_use_map)[1 + (p2 / 32)] & (1 << (p2 & 31))))
            scheme_ill_formed_code(port);
        }
      }

      if ((flags > SCHEME_TOPLEVEL_UNKNOWN) && tl_state && (p < num_toplevels)) {
        if (tl_state[p] <= 0) {
          /* record expectation */
          int s = -tl_state[p];
          int new_flags;
          new_flags = ((flags > (s & SCHEME_TOPLEVEL_FLAGS_MASK))
                       ? flags
                       : (s & SCHEME_TOPLEVEL_FLAGS_MASK));
          s >>= 2;
          if (!s || (tl_timestamp < s))
            s = tl_timestamp;
          tl_state[p] = -((s << 2) | new_flags);
        } else {
          /* check expectation */
          if (((tl_state[p] & SCHEME_TOPLEVEL_FLAGS_MASK) < flags)
              || ((tl_state[p] >> 2) > tl_timestamp))
            scheme_ill_formed_code(port);
        }
      }

      if ((proc_with_refs_ok != 1) 
          && !argument_to_arity_error(app_rator, proc_with_refs_ok)) {
        if (p >= (num_toplevels + num_stxes + (num_stxes ? 1 : 0))) {
          /* It's a lift. Check that the lift is defined, and that it
             doesn't want reference arguments. */
          int tp;
          mzshort *a;
          tp = p - (num_toplevels + num_stxes + (num_stxes ? 1 : 0));
          a = tls[tp];
          if (a) {
            if (a == (mzshort *)0x1) {
              /* Ok */
            } else if (a[0] > 0) {
              int i, cnt;
              cnt = a[0];
              for (i = 0; i < cnt; i++) {
                if (a[i] == 2) 
                  scheme_ill_formed_code(port);
              }
              tls[tp] = (mzshort *)0x1;
            } else {
              /* a[0] is either 0 (top-level ref; shouldn't happen) or < 0 (wants some ref args) */
              scheme_ill_formed_code(port);
            }
          } else {
            tls[tp] = (mzshort *)0x1; /* means "no ref args anywhere" */
          }
        }
      }

      if (flags == SCHEME_TOPLEVEL_UNKNOWN)
        result = validate_join_seq(result, 0);
      else {
        result = validate_join_const(result, expected_results);
        if (flags < SCHEME_TOPLEVEL_CONST)
          result = validate_join_seq(result, 1);
      }
    }
    break;
  case scheme_local_type:
    {
      int q = SCHEME_LOCAL_POS(expr);
      int p = q + delta;
      int ct;

      if ((q < 0) || (p >= depth) || (p < 0))
	scheme_ill_formed_code(port);

      ct = SCHEME_GET_LOCAL_TYPE(expr);
      if (!ct)
        no_typed(need_local_type, port);
      
      if (ct) {
        if (stack[p] != (VALID_TYPED + ct))
          scheme_ill_formed_code(port);
      } else if ((stack[p] != VALID_VAL) && (stack[p] != VALID_VAL_NOCLEAR)) {
        if (result_ignored && ((stack[p] == VALID_BOX) 
                               || (stack[p] == VALID_BOX_NOCLEAR)
                               || (stack[p] >= VALID_TYPED))) {
          /* ok to look up and ignore box or typed */
        } else if ((proc_with_refs_ok >= 2) 
                   && ((stack[p] == VALID_BOX) || (stack[p] == VALID_BOX_NOCLEAR))
                   && validate_rator_wants_box(app_rator, proc_with_refs_ok - 2, 1,
                                               tls, num_toplevels, num_stxes, num_lifts,
                                               tl_use_map)) {
          /* It's ok - the function wants us to pass it a box, and
             we did. */
          app_rator = NULL;
        } else
          scheme_ill_formed_code(port);
      }

      if (SCHEME_GET_LOCAL_FLAGS(expr) == SCHEME_LOCAL_CLEAR_ON_READ) {
        if ((stack[p] == VALID_VAL_NOCLEAR)
            || (stack[p] == VALID_BOX_NOCLEAR)
            || (stack[p] >= VALID_TYPED))
          scheme_ill_formed_code(port);
        if (p >= letlimit)
          clearing_stack_push(vc, p, stack[p]);
        stack[p] = VALID_NOT;
      } else if (!(SCHEME_GET_LOCAL_FLAGS(expr) == SCHEME_LOCAL_OTHER_CLEARS)) {
        if (stack[p] == VALID_BOX) {
          if (p >= letlimit)
            noclear_stack_push(vc, p);
          stack[p] = VALID_BOX_NOCLEAR;
        } else if (stack[p] == VALID_VAL) {
          if (p >= letlimit)
            noclear_stack_push(vc, p);
          stack[p] = VALID_VAL_NOCLEAR;
        }
      }

      if (procs && !proc_with_refs_ok && !result_ignored) {
        if (scheme_hash_tree_get(procs, scheme_make_integer(p)))
          scheme_ill_formed_code(port);
      }

      result = validate_join_const(result, expected_results);
    }
    break;
  case scheme_local_unbox_type:
    {
      int q = SCHEME_LOCAL_POS(expr);
      int p = q + delta;

      no_typed(need_local_type, port);

      if ((q < 0) || (p >= depth) || (p < 0)
          || ((stack[p] != VALID_BOX)
              && (stack[p] != VALID_BOX_NOCLEAR)))
	scheme_ill_formed_code(port);

      if (SCHEME_GET_LOCAL_FLAGS(expr) == SCHEME_LOCAL_CLEAR_ON_READ) {
        if (stack[p] == VALID_BOX_NOCLEAR)
          scheme_ill_formed_code(port);
        if (p >= letlimit)
          clearing_stack_push(vc, p, stack[p]);
        stack[p] = VALID_NOT;
      } else if (!(SCHEME_GET_LOCAL_FLAGS(expr) == SCHEME_LOCAL_OTHER_CLEARS)) {
        if (stack[p] == VALID_BOX) {
          if (p >= letlimit)
            noclear_stack_push(vc, p);
          stack[p] = VALID_BOX_NOCLEAR;
        }
      }

      result = validate_join_const(result, expected_results);
    }
    break;
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)expr;
      int i, n, r;

      check_typed(expr, need_local_type, port);

      n = app->num_args + 1;

      delta -= (n - 1);
      if (delta < 0)
	scheme_ill_formed_code(port);
      memset(stack + delta, VALID_NOT, n - 1);

      for (i = 0; i < n; i++) {
	r = validate_expr(port, app->args[i], stack, tls, depth, letlimit, delta, 
                          num_toplevels, num_stxes, num_lifts, tl_use_map,
                          tl_state, tl_timestamp,
                          i ? app->args[0] : NULL, i + 1, 0, vc, 0, 0, procs, 1, NULL);
        result = validate_join(result, r);
      }

      if (tailpos)
        check_self_call_valid(app->args[0], port, vc, delta, stack);

      if (result) {
        r = is_functional_nonfailing_rator(app->args[0], app->num_args, expected_results, _st_ht);
        result = validate_join(result, r);
      }
    }
    break;
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)expr;
      int r;

      check_typed(expr, need_local_type, port);
      
      delta -= 1;
      if (delta < 0)
	scheme_ill_formed_code(port);
      stack[delta] = VALID_NOT;

      r = validate_expr(port, app->rator, stack, tls, depth, letlimit, delta, 
                        num_toplevels, num_stxes, num_lifts, tl_use_map,
                        tl_state, tl_timestamp,
                        NULL, 1, 0, vc, 0, 0, procs, 1, NULL);
      result = validate_join(r, result);
      r = validate_expr(port, app->rand, stack, tls, depth, letlimit, delta, 
                        num_toplevels, num_stxes, num_lifts, tl_use_map,
                        tl_state, tl_timestamp,
                        app->rator, 2, 0, vc, 0, 0, procs, 1, NULL);
      result = validate_join(r, result);

      if (tailpos)
        check_self_call_valid(app->rator, port, vc, delta, stack);

      if (result) {
        r = is_functional_nonfailing_rator(app->rator, 1, expected_results, _st_ht);
        if (!r
            && SAME_OBJ(app->rator, scheme_make_vector_proc)
            && (expected_results == 1 || expected_results == -1)
            && (SCHEME_INTP(app->rand) 
                && (SCHEME_INT_VAL(app->rand) >= 0)
                && IN_FIXNUM_RANGE_ON_ALL_PLATFORMS(SCHEME_INT_VAL(app->rand)))) {
          r = 1;
        }
        result = validate_join(result, r);
      }
    }
    break;
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)expr;
      int r;

      check_typed(expr, need_local_type, port);
      
      delta -= 2;
      if (delta < 0)
	scheme_ill_formed_code(port);
      stack[delta] = VALID_NOT;
      stack[delta+1] = VALID_NOT;

      r = validate_expr(port, app->rator, stack, tls, depth, letlimit, delta, 
                        num_toplevels, num_stxes, num_lifts, tl_use_map,
                        tl_state, tl_timestamp,
                        NULL, 1, 0, vc, 0, 0, procs, 1, NULL);
      result = validate_join(r, result);
      r = validate_expr(port, app->rand1, stack, tls, depth, letlimit, delta, 
                        num_toplevels, num_stxes, num_lifts, tl_use_map,
                        tl_state, tl_timestamp,
                        app->rator, 2, 0, vc, 0, 0, procs, 1, NULL);
      result = validate_join(r, result);
      r = validate_expr(port, app->rand2, stack, tls, depth, letlimit, delta, 
                        num_toplevels, num_stxes, num_lifts, tl_use_map,
                        tl_state, tl_timestamp,
                        app->rator, 3, 0, vc, 0, 0, procs, 1, NULL);
      result = validate_join(r, result);

      if (tailpos)
        check_self_call_valid(app->rator, port, vc, delta, stack);

      if (result) {
        r = is_functional_nonfailing_rator(app->rator, 2, expected_results, _st_ht);
        if (!r
            && SAME_OBJ(app->rator, scheme_make_vector_proc)
            && (expected_results == 1 || expected_results == -1)
            && (SCHEME_INTP(app->rand1)
                && (SCHEME_INT_VAL(app->rand1) >= 0)
                && IN_FIXNUM_RANGE_ON_ALL_PLATFORMS(SCHEME_INT_VAL(app->rand1)))) {
          r = 1;
        }
         result = validate_join(r, result);
      }
    }
    break;
  case scheme_sequence_type:
  case scheme_splice_sequence_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)expr;
      int cnt;
      int i, r;

      if (type != scheme_sequence_type)
        no_typed(need_local_type, port);
      
      cnt = seq->count;
	  
      for (i = 0; i < cnt - 1; i++) {
	r = validate_expr(port, seq->array[i], stack, tls, depth, letlimit, delta, 
                          num_toplevels, num_stxes, num_lifts, tl_use_map,
                          tl_state, tl_timestamp,
                          NULL, 0, 1, vc, 0, 0, procs, -1, NULL);
        result = validate_join_seq(result, r);
      }

      expr = seq->array[cnt - 1];
      goto top;
    }
    break;
  case scheme_branch_type:
    {
      Scheme_Branch_Rec *b;
      int vc_pos, vc_ncpos, r;

      b = (Scheme_Branch_Rec *)expr;
      r = validate_expr(port, b->test, stack, tls, depth, letlimit, delta, 
                        num_toplevels, num_stxes, num_lifts, tl_use_map,
                        tl_state, tl_timestamp,
                        NULL, 0, 0, vc, 0, 0, procs, 1, NULL);
      result = validate_join(r, result);

      /* This is where letlimit is useful. It prevents let-assignment in the
	 "then" branch that could permit bad code in the "else" branch (or the
	 same thing with either branch affecting later code in a sequence). */
      letlimit = delta;
      vc_pos = vc->stackpos;
      vc_ncpos = vc->ncstackpos;
      r = validate_expr(port, b->tbranch, stack, tls, depth, letlimit, delta, 
                        num_toplevels, num_stxes, num_lifts, tl_use_map,
                        tl_state, tl_timestamp,
                        NULL, 0, result_ignored, vc, tailpos, need_local_type, procs,
                        expected_results, NULL);
      result = validate_join_seq(result, r);
      
      /* since we're branchig, the result isn't constant: */
      result = validate_join(1, result);

      /* Rewind clears and noclears, but also save the clears,
         so that the branches' effects can be merged. */
      {
        int i, j;

        if (!vc_merge) {
          vc_merge = 1;
          vc_merge_start = vc_pos;
        }
        
        for (i = vc->stackpos - 2; i >= vc_pos; i -= 2) {
          stack[vc->stack[i]] = vc->stack[i + 1];
        }

        for (i = vc->ncstackpos - 1; i >= vc_ncpos; i--) {
          j = vc->ncstack[i];
          if (stack[j] == VALID_VAL_NOCLEAR)
            stack[j] = VALID_VAL;
          else if (stack[j] == VALID_BOX_NOCLEAR)
            stack[j] = VALID_BOX;
        }
        vc->ncstackpos = vc_ncpos;
      }

      expr = b->fbranch;
      goto top;
    }
    break;
  case scheme_with_cont_mark_type:
    {
      Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)expr;
      int r;

      no_typed(need_local_type, port);
      
      r = validate_expr(port, wcm->key, stack, tls, depth, letlimit, delta, 
                        num_toplevels, num_stxes, num_lifts, tl_use_map,
                        tl_state, tl_timestamp,
                        NULL, 0, 0, vc, 0, 0, procs, 1, NULL);
      result = validate_join_seq(result, r);
      r = validate_expr(port, wcm->val, stack, tls, depth, letlimit, delta, 
                        num_toplevels, num_stxes, num_lifts, tl_use_map,
                        tl_state, tl_timestamp,
                        NULL, 0, 0, vc, 0, 0, procs, 1, NULL);
      result = validate_join_seq(result, r);

      expr = wcm->body;
      goto top;
    }
    break;
  case scheme_quote_syntax_type:
    {
      Scheme_Quote_Syntax *qs = (Scheme_Quote_Syntax *)expr;
      int c = qs->depth;
      int i = qs->position;
      int p = qs->midpoint;
      int d = c + delta;

      no_typed(need_local_type, port);

      if ((c < 0) || (p < 0) || (d < 0) || (d >= depth)
	  || (stack[d] != VALID_TOPLEVELS) 
	  || (p != num_toplevels)
	  || (i >= num_stxes))
	scheme_ill_formed_code(port);

      if (tl_use_map) {
        if ((uintptr_t)tl_use_map & 0x1) {
          if (p > 31)
            scheme_ill_formed_code(port);
          if (!((uintptr_t)tl_use_map & (1 << (p + 1))))
            scheme_ill_formed_code(port);
        } else {
          if (p >= (*(int *)tl_use_map * 32))
            scheme_ill_formed_code(port);
          if (!(((int *)tl_use_map)[1 + (p / 32)] & (1 << (p & 31))))
            scheme_ill_formed_code(port);
        }
      }

      result = validate_join_const(result, expected_results);
    }
    break;
  case scheme_unclosed_procedure_type:
    {
      no_typed(need_local_type, port);
      validate_unclosed_procedure(port, expr, stack, tls, depth, delta, 
                                  num_toplevels, num_stxes, num_lifts, tl_use_map,
                                  tl_state, tl_timestamp,
                                  app_rator, proc_with_refs_ok, -1, procs);

      result = validate_join_const(result, expected_results);
    }
    break;
  case scheme_let_value_type:
    {
      Scheme_Let_Value *lv = (Scheme_Let_Value *)expr;
      int q, p, c, i, r;
      
      r = validate_expr(port, lv->value, stack, tls, depth, letlimit, delta, 
                        num_toplevels, num_stxes, num_lifts, tl_use_map,
                        tl_state, tl_timestamp,
                        NULL, 0, 0, vc, 0, 0, procs, lv->count, NULL);
      result = validate_join_seq(r, result);

      /* memset(stack, VALID_NOT, delta);  <-- seems unnecessary (and slow) */

      c = lv->count;
      q = lv->position;
      p = q + delta;

      for (i = 0; i < c; i++, p++) {
	if ((q < 0) 
            || (p < 0)
	    || (SCHEME_LET_VALUE_AUTOBOX(lv) && ((p >= depth)
					   || ((stack[p] != VALID_BOX)
                                               && (stack[p] != VALID_BOX_NOCLEAR))))
	    || (!SCHEME_LET_VALUE_AUTOBOX(lv) && ((p >= letlimit)
					    || !(WHEN_CAN_RESET_STACK_SLOT(stack[p] == VALID_VAL) 
                                                 || WHEN_CAN_RESET_STACK_SLOT(stack[p] == VALID_VAL_NOCLEAR) 
                                                 || (stack[p] == VALID_UNINIT)))))
	  scheme_ill_formed_code(port);

	if (!SCHEME_LET_VALUE_AUTOBOX(lv)) {
          if (stack[p] != VALID_VAL_NOCLEAR)
            stack[p] = VALID_VAL;
	}
      }

      expr = lv->body;
      goto top;
    }
    break;
  case scheme_let_void_type:
    {
      Scheme_Let_Void *lv = (Scheme_Let_Void *)expr;
      int c, i;

      c = lv->count;

      if ((c < 0) || (c > delta))
	scheme_ill_formed_code(port);

      if (SCHEME_LET_VOID_AUTOBOX(lv)) {
	for (i = 0; i < c; i++) {
	  stack[--delta] = VALID_BOX;
	}
      } else {
	delta -= c;
	memset(stack + delta, VALID_UNINIT, c);
      }

      expr = lv->body;
      goto top;
    }
    break;
  case scheme_letrec_type:
    {
      Scheme_Letrec *l = (Scheme_Letrec *)expr;
      Scheme_Closure_Data *data;
      int i, c;

      c = l->count;
      
      if ((c < 0) || (c + delta > depth))
	scheme_ill_formed_code(port);

      for (i = 0; i < c; i++) {
	if (!SAME_TYPE(SCHEME_TYPE(l->procs[i]), scheme_unclosed_procedure_type))
	  scheme_ill_formed_code(port);
      }

      for (i = 0; i < c; i++) {
#if !CAN_RESET_STACK_SLOT
        if (stack[delta + i] != VALID_UNINIT)
          scheme_ill_formed_code(port);
#endif
	stack[delta + i] = VALID_VAL;
        data = (Scheme_Closure_Data *)l->procs[i];
        if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_TYPED_ARGS) {
          /* If any arguments (as opposed to closure slots) are typed, then
             add the procedure to `procs': */
          int j;
          for (j = data->num_params; j--; ) {
            if (scheme_boxmap_get(data->closure_map, j, data->closure_size))
              break;
          }
          if (j >= 0) {
            procs = scheme_hash_tree_set(as_nonempty_procs(procs),
                                         scheme_make_integer(delta + i),
                                         l->procs[i]);
          }
        }
      }

      for (i = 0; i < c; i++) {
	validate_unclosed_procedure(port, l->procs[i], stack, tls, depth, delta, 
                                    num_toplevels, num_stxes, num_lifts, tl_use_map,
                                    tl_state, tl_timestamp,
                                    NULL, 1, i, procs);
      }

      expr = l->body;
      goto top;
    }
    break;
  case scheme_let_one_type:
    {
      Scheme_Let_One *lo = (Scheme_Let_One *)expr;
      int r;

      --delta;
      if (delta < 0)
	scheme_ill_formed_code(port);
      stack[delta] = VALID_UNINIT;
      
      r = validate_expr(port, lo->value, stack, tls, depth, letlimit, delta, 
                        num_toplevels, num_stxes, num_lifts, tl_use_map,
                        tl_state, tl_timestamp,
                        NULL, 0, 0, vc, 0, SCHEME_LET_ONE_TYPE(lo), procs,
                        1, _st_ht);
      result = validate_join_seq(r, result);

#if !CAN_RESET_STACK_SLOT
      if (stack[delta] != VALID_UNINIT)
        scheme_ill_formed_code(port);
#endif
      
      if (SCHEME_LET_EVAL_TYPE(lo) & LET_ONE_UNUSED) {
        stack[delta] = VALID_NOT;
      } else if (SCHEME_LET_ONE_TYPE(lo)) {
        stack[delta] = (VALID_TYPED + SCHEME_LET_ONE_TYPE(lo));
        /* FIXME: need to check that lo->value produces a flonum */
      } else
        stack[delta] = VALID_VAL;

      expr = lo->body;
      goto top;
    }
    break;

  case scheme_define_values_type:
    no_typed(need_local_type, port);
    result = validate_join_seq(result,
                               define_values_validate(expr, port, stack, tls, depth, letlimit, delta, 
                                                      num_toplevels, num_stxes, num_lifts, tl_use_map, 
                                                      tl_state, tl_timestamp,
                                                      result_ignored, vc, tailpos, procs,
                                                      _st_ht));
    break;
  case scheme_define_syntaxes_type:
    no_typed(need_local_type, port);
    define_syntaxes_validate(expr, port, stack, tls, depth, letlimit, delta, 
                             num_toplevels, num_stxes, num_lifts, tl_use_map, 
                             tl_state, tl_timestamp,
                             result_ignored, vc, tailpos, procs);
    break;
  case scheme_begin_for_syntax_type:
    no_typed(need_local_type, port);
    begin_for_syntaxes_validate(expr, port, stack, tls, depth, letlimit, delta, 
                                num_toplevels, num_stxes, num_lifts, tl_use_map, 
                                tl_state, tl_timestamp,
                                result_ignored, vc, tailpos, procs);
    break;
  case scheme_set_bang_type:
    no_typed(need_local_type, port);
    result = validate_join_seq(result,
                               set_validate(expr, port, stack, tls, depth, letlimit, delta, 
                                            num_toplevels, num_stxes, num_lifts, tl_use_map, 
                                            tl_state, tl_timestamp,
                                            result_ignored, vc, tailpos, procs));
    break;
  case scheme_boxenv_type:
    no_typed(need_local_type, port);
    result = validate_join_seq(result,
                               bangboxenv_validate(expr, port, stack, tls, depth, letlimit, delta, 
                                                   num_toplevels, num_stxes, num_lifts, tl_use_map, 
                                                   tl_state, tl_timestamp,
                                                   result_ignored, vc, tailpos, procs, expected_results));
    break;
  case scheme_begin0_sequence_type:
    no_typed(need_local_type, port);
    result = validate_join_seq(result,
                               begin0_validate(expr, port, stack, tls, depth, letlimit, delta, 
                                               num_toplevels, num_stxes, num_lifts, tl_use_map, 
                                               tl_state, tl_timestamp,
                                               result_ignored, vc, tailpos, procs, expected_results));
    break;
  case scheme_require_form_type:
    no_typed(need_local_type, port);
    top_level_require_validate(expr, port, stack, tls, depth, letlimit, delta, 
                               num_toplevels, num_stxes, num_lifts, tl_use_map, 
                               tl_state, tl_timestamp,
                               result_ignored, vc, tailpos, procs);
    break;
  case scheme_varref_form_type:
    no_typed(need_local_type, port);
    ref_validate(expr, port, stack, tls, depth, letlimit, delta, 
                 num_toplevels, num_stxes, num_lifts, tl_use_map, 
                 tl_state, tl_timestamp,
                 result_ignored, vc, tailpos, procs);
    result = validate_join_const(result, expected_results);
    break;
  case scheme_apply_values_type:
    no_typed(need_local_type, port);
    apply_values_validate(expr, port, stack, tls, depth, letlimit, delta, 
                          num_toplevels, num_stxes, num_lifts, tl_use_map, 
                          tl_state, tl_timestamp,
                          result_ignored, vc, tailpos, procs);
    result = validate_join(0, result);
    break;
  case scheme_with_immed_mark_type:
    {
      Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)expr;
      int r;

      no_typed(need_local_type, port);
      
      r = validate_expr(port, wcm->key, stack, tls, depth, letlimit, delta, 
                        num_toplevels, num_stxes, num_lifts, tl_use_map,
                        tl_state, tl_timestamp,
                        NULL, 0, 0, vc, 0, 0, procs,
                        1, _st_ht);
      result = validate_join_seq(r, result);
      
      r = validate_expr(port, wcm->val, stack, tls, depth, letlimit, delta, 
                        num_toplevels, num_stxes, num_lifts, tl_use_map,
                        tl_state, tl_timestamp,
                        NULL, 0, 0, vc, 0, 0, procs,
                        1, _st_ht);
      result = validate_join_seq(r, result);

      --delta;
      if (delta < 0)
	scheme_ill_formed_code(port);
      stack[delta] = VALID_VAL;

      expr = wcm->body;
      goto top;
    }
    break;
  case scheme_case_lambda_sequence_type:
    no_typed(need_local_type, port);
    case_lambda_validate(expr, port, stack, tls, depth, letlimit, delta, 
                         num_toplevels, num_stxes, num_lifts, tl_use_map, 
                         tl_state, tl_timestamp,
                         result_ignored, vc, tailpos, procs);
    result = validate_join_const(result, expected_results);
    break;
  case scheme_module_type:
    no_typed(need_local_type, port);
    module_validate(expr, port, stack, tls, depth, letlimit, delta, 
                    num_toplevels, num_stxes, num_lifts, tl_use_map, 
                    tl_state, tl_timestamp,
                    result_ignored, vc, tailpos, procs);
    result = validate_join(0, result);
    break;
  case scheme_inline_variant_type:
    no_typed(need_local_type, port);
    inline_variant_validate(expr, port, stack, tls, depth, letlimit, delta, 
                            num_toplevels, num_stxes, num_lifts, tl_use_map, 
                            tl_state, tl_timestamp,
                            result_ignored, vc, tailpos, procs);
    result = validate_join_const(result, expected_results);
    break;
  default:
    /* All values are definitely ok, except pre-closed closures. 
       Such a closure can refer back to itself, so we use a flag
       to track cycles. Also check need_local_type. */
    result = validate_join_const(result, expected_results);
    if (SAME_TYPE(type, scheme_closure_type)
        /* If the closure is not empty, then it must be from 3-D code
           (where PLT_VALIDATE_COMPILE is set), and validation is not
           our responsibility here: */
        && (SCHEME_COMPILED_CLOS_CODE(expr)->closure_size == 0)) {
      Scheme_Closure_Data *data;
      no_typed(need_local_type, port);
      expr = (Scheme_Object *)SCHEME_COMPILED_CLOS_CODE(expr);
      data = (Scheme_Closure_Data *)expr;
      if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_VALIDATED) {
        /* Done with this one. */
      } else {
        SCHEME_CLOSURE_DATA_FLAGS(data) |= CLOS_VALIDATED;
        did_one = 0;
        goto top;
      }
    } else if (SAME_TYPE(type, scheme_case_closure_type)) {
      Scheme_Case_Lambda *seq;
      int i;
      seq = (Scheme_Case_Lambda *)expr;
      for (i = 0; i < seq->count; i++) {
        validate_expr(port, seq->array[i], stack, tls, depth, letlimit, delta, 
                      num_toplevels, num_stxes, num_lifts, tl_use_map,
                      tl_state, tl_timestamp,
                      NULL, 0, 0, vc, 0, 0, procs, 1, NULL);
      }
    } else if (need_local_type) {
      if (SCHEME_DBLP(expr) && (need_local_type == SCHEME_LOCAL_TYPE_FLONUM))
        need_local_type = 0;
#ifdef MZ_LONG_DOUBLE
      if (SCHEME_LONG_DBLP(expr) && (need_local_type == SCHEME_LOCAL_TYPE_EXTFLONUM))
        need_local_type = 0;
#endif
      if (SCHEME_INTP(expr) && (need_local_type == SCHEME_LOCAL_TYPE_FIXNUM))
        need_local_type = 0;
      no_typed(need_local_type, port);
    }
    break;
  }

  if (app_rator)
    if (validate_rator_wants_box(app_rator, proc_with_refs_ok - 2, 0,
                                 tls, num_toplevels, num_stxes, num_lifts, tl_use_map))
      scheme_ill_formed_code(port);

  if (vc_merge) {
    /* Re-clear to merge effects from branches */
    int i, p;
    for (i = vc_merge_start; i < vc->stackpos; i += 2) {
      p = vc->stack[i];
      stack[p] = VALID_NOT;
    }
  }

  return result;
}

/*========================================================================*/
/*                         precise GC traversers                          */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_validate.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_validate_clearing, mark_validate_clearing);
}

END_XFORM_SKIP;

#endif
