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

/* This file implements the bytecode "resolve" pass, which converts
   the optimization IR to the evaluation bytecode --- where the main
   difference between the representations is to use stack addresses.
   This pass is also responsible for closure conversion: lifting
   functions that are used only in application positions, where all
   variables captured by the closure can be converted to arguments at
   every call site.

   The "unresolve" functions convert run-time bytecode back into the
   optimizer's IR, which is used for cross-module inlining and for
   `compiled-expression-recompile`.

   See "eval.c" for an overview of compilation passes. */

#include "schpriv.h"
#include "schrunst.h"
#include "schmach.h"

struct Resolve_Info
{
  MZTAG_IF_REQUIRED
  char use_jit, in_module, in_proc, enforce_const, no_lift;
  int current_depth; /* tracks the stack depth, so variables can be
                        resolved relative to it; this depth is reset
                        on entry to `lambda` forms */
  int current_lex_depth; /* keeps track of the lexical depth, which isn't
                            reset on entry; this absolute depth is useful
                            for sorting */
  int max_let_depth; /* filled in by sub-expressions to track the maximum
                        stack depth experienced so far */
  Resolve_Prefix *prefix;
  Scheme_Hash_Table *stx_map; /* compile offset => resolve offset; prunes prefix-recored stxes */
  mzshort toplevel_pos; /* tracks where the run-time prefix will be, relative
                           to the current stack depth */
  void *tl_map; /* fixnum or bit array (as array of `int's) indicating which
                   globals+lifts in prefix are used */
  int stx_count; /* tracks the number of literal syntax objects used */
  Scheme_Hash_Tree *redirects; /* maps variables that will be from the closure
                                  to their stack depths for the enclosing `lambda` */
  Scheme_Object *lifts; /* tracks functions lifted by closure conversion */
  struct Resolve_Info *next;
};

#define cons(a,b) scheme_make_pair(a,b)

static Scheme_Object *
resolve_lambda(Scheme_Object *_lam, Resolve_Info *info, 
               int can_lift, int convert, int just_compute_lift,
               Scheme_Object *precomputed_lift);
static Resolve_Info *resolve_info_extend(Resolve_Info *info, int size, int lambda);
static void resolve_info_add_mapping(Resolve_Info *info, Scheme_IR_Local *var, Scheme_Object *v);
static int resolve_info_lookup(Resolve_Info *resolve, Scheme_IR_Local *var, Scheme_Object **lifted,
                               int convert_shift, int flags);
static Scheme_Object *resolve_info_lift_added(Resolve_Info *resolve, Scheme_Object *var, int convert_shift);
static void resolve_info_set_toplevel_pos(Resolve_Info *info, int pos);
static void merge_resolve(Resolve_Info *info, Resolve_Info *new_info);
static Scheme_Object *resolve_generate_stub_lift(void);
static int resolve_toplevel_pos(Resolve_Info *info);
static int resolve_quote_syntax_offset(int i, Resolve_Info *info);
static int resolve_quote_syntax_pos(Resolve_Info *info);
static Scheme_Object *resolve_toplevel(Resolve_Info *info, Scheme_Object *expr, int keep_ready);
static Scheme_Object *resolve_invent_toplevel(Resolve_Info *info);
static Scheme_Object *resolve_invented_toplevel_to_defn(Resolve_Info *info, Scheme_Object *tl);
static Scheme_Object *shift_lifted_reference(Scheme_Object *tl, Resolve_Info *info, int delta);
static Scheme_Object *shift_toplevel(Scheme_Object *expr, int delta);
static int is_nonconstant_procedure(Scheme_Object *lam, Resolve_Info *info, Scheme_Hash_Tree *exclude_vars);
static int resolve_is_inside_proc(Resolve_Info *info);
static int resolve_has_toplevel(Resolve_Info *info);
static void set_tl_pos_used(Resolve_Info *info, int pos);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

#define RESOLVE_UNUSED_OK    0x1
#define RESOLVE_IGNORE_LIFTS 0x2

void scheme_init_resolve()
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif
}

/*========================================================================*/
/*                            applications                                */
/*========================================================================*/

static Scheme_Object *check_converted_rator(Scheme_Object *rator, Resolve_Info *info, Scheme_Object **new_rator,
                                            int orig_arg_cnt, int *_rdelta)
/* Check whether `rator` refers to a function that has been lifted and
   changed to accept extra arguments, in which case the application
   needs to be adjusted with the extra arguments. */
{
  Scheme_Object *lifted;

  if (!SAME_TYPE(SCHEME_TYPE(rator), scheme_ir_local_type))
    return NULL;

  (void)resolve_info_lookup(info, SCHEME_VAR(rator), &lifted, 0, 0);

  if (lifted && SCHEME_RPAIRP(lifted)) {
    Scheme_Object *vec, *arity;

    *new_rator = SCHEME_CAR(lifted);
    vec = SCHEME_CDR(lifted);
    *_rdelta = 0;

    if (SAME_TYPE(SCHEME_TYPE(*new_rator), scheme_toplevel_type)) {
      Scheme_Object *tl;
      tl = shift_lifted_reference(*new_rator, info, orig_arg_cnt + SCHEME_VEC_SIZE(vec) - 1);
      *new_rator = tl;
    }

    if (SCHEME_VEC_SIZE(vec) > 1) {
      /* Check that actual argument count matches expected. If
         it doesn't, we need to generate explicit code to report
         the error, so that the conversion's arity change isn't
         visible. */
      arity = SCHEME_VEC_ELS(vec)[0];
      if (SCHEME_INTP(arity)) {
        if (orig_arg_cnt == SCHEME_INT_VAL(arity))
          arity = NULL;
      } else {
        arity = SCHEME_BOX_VAL(arity);
        if (orig_arg_cnt >= SCHEME_INT_VAL(arity))
          arity = NULL;
        else {
          Scheme_App2_Rec *app;
          app = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
          app->iso.so.type = scheme_application2_type;
          app->rator = scheme_make_arity_at_least;
          app->rand = arity;
          arity = (Scheme_Object *)app;
          *_rdelta = 1; /* so app gets resolved */
        }
      }
      /* If arity is non-NULL, there's a mismatch. */
      if (arity) {
        /* Generate a call to `raise-arity-error' instead of
           the current *new_rator: */
        Scheme_Object *old_rator = *new_rator;
        if (SAME_TYPE(SCHEME_TYPE(old_rator), scheme_toplevel_type)) {
          /* More coordinate trouble. old_rator was computed for an
             application with a potentially different number of arguments. */
          int delta;
          delta = 3 - SCHEME_VEC_SIZE(vec);
          if (delta)
            old_rator = shift_toplevel(old_rator, delta);
        }
        vec = scheme_make_vector(3, NULL);
        SCHEME_VEC_ELS(vec)[0] = scheme_make_integer(0);
        SCHEME_VEC_ELS(vec)[1] = old_rator;
        SCHEME_VEC_ELS(vec)[2] = arity;
        *new_rator = scheme_raise_arity_error_proc;
      }
    }

    return vec;
  } else
    return NULL;
}

static Scheme_Object *resolve_application(Scheme_Object *o, Resolve_Info *orig_info, int already_resolved_arg_count)
{
  Resolve_Info *info;
  Scheme_App_Rec *app;
  int i, n, devals;

  app = (Scheme_App_Rec *)o;

  n = app->num_args + 1;

  if (!already_resolved_arg_count) {
    /* Check whether this is an application of a converted closure: */
    Scheme_Object *additions = NULL, *rator;
    int rdelta;
    additions = check_converted_rator(app->args[0], orig_info, &rator, n - 1, &rdelta);
    if (additions) {
      /* Expand application with m arguments */
      Scheme_App_Rec *app2;
      Scheme_Object *arg;
      int m;
      m = SCHEME_VEC_SIZE(additions) - 1;
      app2 = scheme_malloc_application(n + m);
      for (i = 0; i < m; i++) {
        arg = resolve_info_lift_added(orig_info, SCHEME_VEC_ELS(additions)[i+1], n - 1 + m);
        app2->args[i + 1] = arg;
      }
      for (i = 1; i < n; i++) {
        app2->args[i + m] = app->args[i];
      }
      app2->args[0] = rator;
      n += m;
      app = app2;
      already_resolved_arg_count = m + 1 + rdelta;
      SCHEME_APPN_FLAGS(app) |= APPN_FLAG_SFS_TAIL;
    }
  }

  devals = sizeof(Scheme_App_Rec) + ((n - mzFLEX_DELTA) * sizeof(Scheme_Object *));
  
  info = resolve_info_extend(orig_info, n - 1, 0);
  
  for (i = 0; i < n; i++) {
    Scheme_Object *le;
    if (already_resolved_arg_count) {
      already_resolved_arg_count--;
    } else {
      le = scheme_resolve_expr(app->args[i], info);
      app->args[i] = le;
    }
  }

  merge_resolve(orig_info, info);

  for (i = 0; i < n; i++) {
    char et;
    et = scheme_get_eval_type(app->args[i]);
    ((char *)app XFORM_OK_PLUS devals)[i] = et;
  }

  return (Scheme_Object *)app;
}

static Scheme_Object *resolve_application3(Scheme_Object *o, Resolve_Info *orig_info, int already_resolved_arg_count);

static void set_app2_eval_type(Scheme_App2_Rec *app)
{
  short et;

  et = scheme_get_eval_type(app->rand);
  et = et << 3;
  et += scheme_get_eval_type(app->rator);
  
  SCHEME_APPN_FLAGS(app) = et | (SCHEME_APPN_FLAGS(app) & APPN_FLAG_MASK);
}

void scheme_reset_app2_eval_type(Scheme_App2_Rec *app)
{
  set_app2_eval_type(app);
}

static Scheme_Object *resolve_application2(Scheme_Object *o, Resolve_Info *orig_info, int already_resolved_arg_count)
{
  Resolve_Info *info;
  Scheme_App2_Rec *app;
  Scheme_Object *le, *arg;

  app = (Scheme_App2_Rec *)o;

  if (!already_resolved_arg_count) {
    /* Check whether this is an application of a converted closure: */
    Scheme_Object *additions = NULL, *rator;
    int rdelta;
    additions = check_converted_rator(app->rator, orig_info, &rator, 1, &rdelta);
    if (additions) {
      int m;
      m = SCHEME_VEC_SIZE(additions) - 1;
      if (!m) {
        app->rator = rator;
        already_resolved_arg_count = 1 + rdelta;
      } else if (m > 1) {
        /* Expand application with m arguments */
        Scheme_App_Rec *app2;
        int i;
        app2 = scheme_malloc_application(2 + m);
        for (i = 0; i < m; i++) {
          arg = resolve_info_lift_added(orig_info, SCHEME_VEC_ELS(additions)[i+1], 1 + m);
          app2->args[i + 1] = arg;
        }
        app2->args[0] = rator;
        app2->args[m+1] = app->rand;
        SCHEME_APPN_FLAGS(app2) |= APPN_FLAG_SFS_TAIL;
        return resolve_application((Scheme_Object *)app2, orig_info, m + 1 + rdelta);
      } else {
        Scheme_App3_Rec *app2;
        app2 = MALLOC_ONE_TAGGED(Scheme_App3_Rec);
        app2->iso.so.type = scheme_application3_type;
        app2->rator = rator;
        arg = resolve_info_lift_added(orig_info, SCHEME_VEC_ELS(additions)[1], 1 + 1);
        app2->rand1 = arg;
        app2->rand2 = app->rand;
        SCHEME_APPN_FLAGS(app2) |= APPN_FLAG_SFS_TAIL;
        return resolve_application3((Scheme_Object *)app2, orig_info, m + 1 + rdelta);
      }
    }
  }

  info = resolve_info_extend(orig_info, 1, 0);

  if (!already_resolved_arg_count) {
    le = scheme_resolve_expr(app->rator, info);
    app->rator = le;
  } else
    already_resolved_arg_count--;

  if (!already_resolved_arg_count) {
    le = scheme_resolve_expr(app->rand, info);
    app->rand = le;
  } else
    already_resolved_arg_count--;

  merge_resolve(orig_info, info);

  set_app2_eval_type(app);

  if (SAME_OBJ(app->rator, scheme_varref_const_p_proc)) {
    if (SAME_TYPE(SCHEME_TYPE(app->rand), scheme_varref_form_type)) {
      /* drop reference to namespace: */
      SCHEME_PTR2_VAL(app->rand) = scheme_false;
    }
  }
  
  return (Scheme_Object *)app;
}

static void set_app3_eval_type(Scheme_App3_Rec *app)
/* set flags used for a shortcut in the interpreter */
{
  short et;

  et = scheme_get_eval_type(app->rand2);
  et = et << 3;
  et += scheme_get_eval_type(app->rand1);
  et = et << 3;
  et += scheme_get_eval_type(app->rator);
  
  SCHEME_APPN_FLAGS(app) = et | (SCHEME_APPN_FLAGS(app) & APPN_FLAG_MASK);
}

void scheme_reset_app3_eval_type(Scheme_App3_Rec *app)
{
  set_app3_eval_type(app);
}

static Scheme_Object *resolve_application3(Scheme_Object *o, Resolve_Info *orig_info, int already_resolved_arg_count)
{
  Resolve_Info *info;
  Scheme_App3_Rec *app;
  Scheme_Object *le;

  app = (Scheme_App3_Rec *)o;

  if (!already_resolved_arg_count) {
    /* Check whether this is an application of a converted closure: */
    Scheme_Object *additions = NULL, *rator;
    int rdelta;
    additions = check_converted_rator(app->rator, orig_info, &rator, 2, &rdelta);
    if (additions) {
      int m, i;
      m = SCHEME_VEC_SIZE(additions) - 1;
      if (m) {
        /* Expand application with m arguments */
        Scheme_App_Rec *app2;
        Scheme_Object *arg;
        app2 = scheme_malloc_application(3 + m);
        for (i = 0; i < m; i++) {
          arg = resolve_info_lift_added(orig_info, SCHEME_VEC_ELS(additions)[i+1], 2 + m);
          app2->args[i + 1] = arg;
        }
        app2->args[0] = rator;
        app2->args[m+1] = app->rand1;
        app2->args[m+2] = app->rand2;
        SCHEME_APPN_FLAGS(app2) |= APPN_FLAG_SFS_TAIL;
        return resolve_application((Scheme_Object *)app2, orig_info, m + 1 + rdelta);
      } else {
        app->rator = rator;
        already_resolved_arg_count = 1 + rdelta;
      }
    }
  }

  info = resolve_info_extend(orig_info, 2, 0);

  if (already_resolved_arg_count) {
    already_resolved_arg_count--;
  } else {
    le = scheme_resolve_expr(app->rator, info);
    app->rator = le;
  }

  if (already_resolved_arg_count) {
    already_resolved_arg_count--;
  } else {
    le = scheme_resolve_expr(app->rand1, info);
    app->rand1 = le;
  }

  if (already_resolved_arg_count) {
    already_resolved_arg_count--;
  } else {
    le = scheme_resolve_expr(app->rand2, info);
    app->rand2 = le;
  }

  /* Optimize `equal?' or `eqv?' test on certain types
     to `eq?'. This is especially helpful for the JIT. 
     This transformation is also performed at the
     optimization layer, and we keep it just in case.*/
  if ((SAME_OBJ(app->rator, scheme_equal_proc)
       || SAME_OBJ(app->rator, scheme_eqv_proc))
      && (scheme_eq_testable_constant(app->rand1)
         || scheme_eq_testable_constant(app->rand2))) {
    app->rator = scheme_eq_proc;
  }

  set_app3_eval_type(app);

  merge_resolve(orig_info, info);

  return (Scheme_Object *)app;
}

/*========================================================================*/
/*                            branch, wcm                                 */
/*========================================================================*/

static Scheme_Object *resolve_branch(Scheme_Object *o, Resolve_Info *info)
{
  Scheme_Branch_Rec *b;
  Scheme_Object *t, *tb, *fb;

  b = (Scheme_Branch_Rec *)o;

  t = scheme_resolve_expr(b->test, info);
  tb = scheme_resolve_expr(b->tbranch, info);
  fb = scheme_resolve_expr(b->fbranch, info);

  b->test = t;
  b->tbranch = tb;
  b->fbranch = fb;

  return o;
}

static Scheme_Object *resolve_wcm(Scheme_Object *o, Resolve_Info *info)
{
  Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)o;
  Scheme_Object *k, *v, *b;

  k = scheme_resolve_expr(wcm->key, info);
  v = scheme_resolve_expr(wcm->val, info);
  b = scheme_resolve_expr(wcm->body, info);
  wcm->key = k;
  wcm->val = v;
  wcm->body = b;

  return (Scheme_Object *)wcm;
}

/*========================================================================*/
/*                              sequences                                 */
/*========================================================================*/

static Scheme_Object *look_for_letv_change(Scheme_Sequence *s)
{
  int i;

  /* Change (begin e1 ... (set!-for-let [x 10] (void)) e2 ...)
     to (begin e1 ... (set!-for-let [x 10] e2 ...)), which 
     avoids an unneeded recursive call in the evaluator */

  for (i = 0; i < s->count - 1; i++) {
    Scheme_Object *v;
    v = s->array[i];
    if (SAME_TYPE(SCHEME_TYPE(v), scheme_let_value_type)) {
      Scheme_Let_Value *lv = (Scheme_Let_Value *)v;
      if (scheme_omittable_expr(lv->body, 1, -1, 0, NULL, NULL)) {
	int esize = s->count - (i + 1);
	int nsize = i + 1;
	Scheme_Object *nv, *ev;

	if (nsize > 1) {
	  Scheme_Sequence *naya;

	  naya = scheme_malloc_sequence(nsize);
	  naya->so.type = s->so.type;
	  naya->count = nsize;
	  nv = (Scheme_Object *)naya;

	  for (i = 0; i < nsize; i++) {
	    naya->array[i] = s->array[i];
	  }
	} else
	  nv = (Scheme_Object *)lv;

	if (esize > 1) {
	  Scheme_Sequence *e;
	  e = scheme_malloc_sequence(esize);
	  e->so.type = s->so.type;
	  e->count = esize;

	  for (i = 0; i < esize; i++) {
	    e->array[i] = s->array[i + nsize];
	  }

	  ev = (Scheme_Object *)look_for_letv_change(e);
	} else
	  ev = s->array[nsize]; 

	lv->body = ev;

	return nv;
      }
    }
  }

  return (Scheme_Object *)s;
}

static Scheme_Object *resolve_sequence(Scheme_Object *o, Resolve_Info *info)
{
  Scheme_Sequence *s = (Scheme_Sequence *)o;
  int i;

  for (i = s->count; i--; ) {
    Scheme_Object *le;
    le = scheme_resolve_expr(s->array[i], info);
    s->array[i] = le;
  }
  
  return look_for_letv_change(s);
}

/*========================================================================*/
/*                             other syntax                               */
/*========================================================================*/

static Scheme_Object *
define_values_resolve(Scheme_Object *data, Resolve_Info *rslv)
{
  intptr_t cnt = 0;
  Scheme_Object *vars = SCHEME_VEC_ELS(data)[0], *l, *a;
  Scheme_Object *val = SCHEME_VEC_ELS(data)[1], *vec;

  /* If this is a module-level definition: for each variable, if the
     defined variable doesn't have SCHEME_TOPLEVEL_MUTATED, then
     resolve to a top-level reference with SCHEME_TOPLEVEL_SEAL, so
     that we know to set GLOS_IS_IMMUTATED at run time. */
  for (l = vars; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    a = SCHEME_CAR(l);
    if (rslv->in_module
	&& rslv->enforce_const
	&& (!(SCHEME_TOPLEVEL_FLAGS(a) & SCHEME_TOPLEVEL_MUTATED))) {
      a = scheme_toplevel_to_flagged_toplevel(a, SCHEME_TOPLEVEL_SEAL);
    }
    a = resolve_toplevel(rslv, a, 0);
    SCHEME_CAR(l) = a;
    cnt++;
  }

  vec = scheme_make_vector(cnt + 1, NULL);
  cnt = 1;
  for (l = vars; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    SCHEME_VEC_ELS(vec)[cnt++] = SCHEME_CAR(l);
  }

  val = scheme_resolve_expr(val, rslv);
  SCHEME_VEC_ELS(vec)[0] = val;

  vec->type = scheme_define_values_type;
  return vec;
}

static void resolve_lift_definition(Resolve_Info *info, Scheme_Object *var, Scheme_Object *rhs)
{
  Scheme_Object *decl, *vec, *pr;

  vec = scheme_make_vector(2, NULL);
  SCHEME_VEC_ELS(vec)[0] = rhs;
  SCHEME_VEC_ELS(vec)[1] = var;

  vec->type = scheme_define_values_type;

  decl = vec;

  vec = info->lifts;
  pr = cons(decl, SCHEME_VEC_ELS(vec)[0]);
  SCHEME_VEC_ELS(vec)[0] = pr;
}

static Scheme_Object *
inline_variant_resolve(Scheme_Object *data, Resolve_Info *rslv)
{
  Scheme_Object *a;
  char no_lift;

  a = SCHEME_VEC_ELS(data)[0];
  a = scheme_resolve_expr(a, rslv);
  SCHEME_VEC_ELS(data)[0] = a;

  /* Don't lift closures in the inline variant, since that
     just creates lifted bindings and closure cycles that we
     don't want to deal with when inlining. */
  a = SCHEME_VEC_ELS(data)[1];
  no_lift = rslv->no_lift;
  rslv->no_lift = 1;
  a = scheme_resolve_expr(a, rslv);
  rslv->no_lift = no_lift;
  SCHEME_VEC_ELS(data)[1] = a;
  
  return data;
}

static Scheme_Object *
set_resolve(Scheme_Object *data, Resolve_Info *rslv)
{
  Scheme_Set_Bang *sb = (Scheme_Set_Bang *)data;
  Scheme_Object *var, *val;

  var = sb->var;
  val = sb->val;
  
  val = scheme_resolve_expr(val, rslv);

  if (SAME_TYPE(SCHEME_TYPE(var), scheme_ir_local_type)) {
    Scheme_Let_Value *lv;
    Scheme_Object *cv;
    int li;

    MZ_ASSERT(SCHEME_VAR(var)->mutated);
    
    cv = scheme_compiled_void();

    lv = MALLOC_ONE_TAGGED(Scheme_Let_Value);
    lv->iso.so.type = scheme_let_value_type;
    lv->body = cv;
    lv->count = 1;
    li = resolve_info_lookup(rslv, SCHEME_VAR(var), NULL, 0, 0);
    lv->position = li;
    SCHEME_LET_VALUE_AUTOBOX(lv) = 1;
    lv->value = val;

    return (Scheme_Object *)lv;
  }

  var = scheme_resolve_expr(var, rslv);

  sb->var = var;
  sb->val = val;
  
  return (Scheme_Object *)sb;
}

static Scheme_Object *
ref_resolve(Scheme_Object *data, Resolve_Info *rslv)
{
  Scheme_Object *v;

  v = scheme_resolve_expr(SCHEME_PTR2_VAL(data), rslv);
  SCHEME_PTR2_VAL(data) = v;
  
  v = SCHEME_PTR1_VAL(data);
  if (SAME_OBJ(v, scheme_true)
      || SAME_OBJ(v, scheme_false)) {
    if (SCHEME_TRUEP(v))
      SCHEME_VARREF_FLAGS(data) |= 0x1; /* => constant */
    v = SCHEME_PTR2_VAL(data);
  } else if (SAME_TYPE(SCHEME_TYPE(v), scheme_ir_local_type)) {
    v = scheme_resolve_expr(v, rslv);
    if (SAME_TYPE(SCHEME_TYPE(v), scheme_local_type))
      SCHEME_VARREF_FLAGS(data) |= 0x1; /* because mutable would be unbox */
    v = SCHEME_PTR2_VAL(data);
  } else
    v = scheme_resolve_expr(v, rslv);
  SCHEME_PTR1_VAL(data) = v;

  return data;
}

static Scheme_Object *
apply_values_resolve(Scheme_Object *data, Resolve_Info *rslv)
{
  Scheme_Object *f, *e;

  f = SCHEME_PTR1_VAL(data);
  e = SCHEME_PTR2_VAL(data);

  f = scheme_resolve_expr(f, rslv);
  e = scheme_resolve_expr(e, rslv);

  SCHEME_PTR1_VAL(data) = f;
  SCHEME_PTR2_VAL(data) = e;
  
  return data;
}

static void set_resolve_mode(Scheme_IR_Local *var)
{
  MZ_ASSERT(SAME_TYPE(var->so.type, scheme_ir_local_type));
  memset(&var->resolve, 0, sizeof(var->resolve));
  var->mode = SCHEME_VAR_MODE_RESOLVE;
}

static Scheme_Object *
with_immed_mark_resolve(Scheme_Object *data, Resolve_Info *orig_rslv)
{
  Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)data;
  Scheme_Object *e;
  Scheme_IR_Local *var;
  Resolve_Info *rslv = orig_rslv;

  e = scheme_resolve_expr(wcm->key, rslv);
  wcm->key = e;

  e = scheme_resolve_expr(wcm->val, rslv);
  wcm->val = e;

  rslv = resolve_info_extend(rslv, 1, 0);

  var = SCHEME_VAR(SCHEME_CAR(wcm->body));
  set_resolve_mode(var);
  var->resolve.co_depth = rslv->current_depth;
  var->resolve.lex_depth = rslv->current_lex_depth;
  
  e = scheme_resolve_expr(SCHEME_CDR(wcm->body), rslv);
  wcm->body = e;

  merge_resolve(orig_rslv, rslv);
  
  return data;
}

static Scheme_Object *
case_lambda_resolve(Scheme_Object *expr, Resolve_Info *rslv)
{
  int i, all_closed = 1;
  Scheme_Case_Lambda *seq = (Scheme_Case_Lambda *)expr;

  for (i = 0; i < seq->count; i++) {
    Scheme_Object *le;
    le = seq->array[i];
    le = resolve_lambda(le, rslv, 0, 0, 0, NULL);
    seq->array[i] = le;
    if (!SCHEME_PROCP(le))
      all_closed = 0;
  }

  if (all_closed) {
    /* Produce closure directly */
    return scheme_case_lambda_execute(expr);
  }

  return expr;
}

static Scheme_Object *do_define_syntaxes_resolve(Scheme_Object *data, Resolve_Info *info)
{
  Comp_Prefix *cp;
  Resolve_Prefix *rp;
  Scheme_Object *names, *val, *base_stack_depth, *dummy, *vec;
  Resolve_Info *einfo;
  int len;

  cp = (Comp_Prefix *)SCHEME_VEC_ELS(data)[0];
  dummy = SCHEME_VEC_ELS(data)[1];
  names = SCHEME_VEC_ELS(data)[2];
  val = SCHEME_VEC_ELS(data)[3];

  rp = scheme_resolve_prefix(1, cp, info->prefix->src_insp_desc);

  dummy = scheme_resolve_expr(dummy, info);

  einfo = scheme_resolve_info_create(rp);

  val = scheme_resolve_expr(val, einfo);

  rp = scheme_remap_prefix(rp, einfo);

  base_stack_depth = scheme_make_integer(einfo->max_let_depth);

  len = scheme_list_length(names);
  
  vec = scheme_make_vector(len + 4, NULL);
  SCHEME_VEC_ELS(vec)[0] = val;
  SCHEME_VEC_ELS(vec)[1] = (Scheme_Object *)rp;
  SCHEME_VEC_ELS(vec)[2] = base_stack_depth;
  SCHEME_VEC_ELS(vec)[3] = dummy;

  len = 4;
  while (SCHEME_PAIRP(names)) {
    SCHEME_VEC_ELS(vec)[len++] = SCHEME_CAR(names);
    names = SCHEME_CDR(names);
  }

  vec->type = scheme_define_syntaxes_type;

  return vec;
}

static Scheme_Object *define_syntaxes_resolve(Scheme_Object *data, Resolve_Info *info)
{
  return do_define_syntaxes_resolve(data, info);
}

static Scheme_Object *begin_for_syntax_resolve(Scheme_Object *data, Resolve_Info *info)
{
  Comp_Prefix *cp;
  Resolve_Prefix *rp;
  Scheme_Object *l, *p, *a, *base_stack_depth, *dummy, *vec;
  Resolve_Info *einfo;

  cp = (Comp_Prefix *)SCHEME_VEC_ELS(data)[0];
  dummy = SCHEME_VEC_ELS(data)[1];
  l = SCHEME_VEC_ELS(data)[2];

  rp = scheme_resolve_prefix(1, cp, info->prefix->src_insp_desc);

  dummy = scheme_resolve_expr(dummy, info);

  einfo = scheme_resolve_info_create(rp);

  p = scheme_null;
  while (!SCHEME_NULLP(l)) {
    a = SCHEME_CAR(l);
    a = scheme_resolve_expr(a, einfo);
    p = scheme_make_pair(a, p);
    l = SCHEME_CDR(l);
  }
  l = scheme_reverse(p);
  
  rp = scheme_remap_prefix(rp, einfo);

  base_stack_depth = scheme_make_integer(einfo->max_let_depth);
  
  vec = scheme_make_vector(4, NULL);
  SCHEME_VEC_ELS(vec)[0] = l;
  SCHEME_VEC_ELS(vec)[1] = (Scheme_Object *)rp;
  SCHEME_VEC_ELS(vec)[2] = base_stack_depth;
  SCHEME_VEC_ELS(vec)[3] = dummy;
  vec->type = scheme_begin_for_syntax_type;

  return vec;
}

/*========================================================================*/
/*                    let, let-values, letrec, etc.                       */
/*========================================================================*/

static int is_lifted_reference(Scheme_Object *v)
/* check whether `v` is a reference to a lifted function */
{
  if (SCHEME_RPAIRP(v))
    return 1;

  return (SAME_TYPE(SCHEME_TYPE(v), scheme_toplevel_type)
          && ((SCHEME_TOPLEVEL_FLAGS(v) & SCHEME_TOPLEVEL_FLAGS_MASK)
              >= SCHEME_TOPLEVEL_CONST));
}

static int is_closed_reference(Scheme_Object *v)
{
  /* Look for a converted function (possibly with no new arguments)
     that is accessed directly as a closure, instead of through a
     top-level reference. */
  if (SCHEME_RPAIRP(v)) {
    v = SCHEME_CAR(v);
    return SCHEME_PROCP(v);
  }

  return 0;
}

static Scheme_Object *scheme_resolve_generate_stub_closure()
{
  Scheme_Closure *cl;
  Scheme_Object *ca;

  cl = scheme_malloc_empty_closure();

  ca = scheme_make_vector(1, scheme_make_integer(0));

  return scheme_make_raw_pair((Scheme_Object *)cl, ca);
}

static int get_convert_arg_count(Scheme_Object *lift)
{
  if (!lift)
    return 0;
  else if (SCHEME_RPAIRP(lift)) {
    lift = SCHEME_CDR(lift);
    MZ_ASSERT(SCHEME_VECTORP(lift));
    return SCHEME_VEC_SIZE(lift) - 1;
  } else
    return 0;
}

static Scheme_Object *get_convert_arg_map(Scheme_Object *lift)
{
  if (!lift)
    return NULL;
  else if (SCHEME_RPAIRP(lift)) {
    lift = SCHEME_CDR(lift);
    MZ_ASSERT(SCHEME_VECTORP(lift));
    return lift;
  } else
    return NULL;
}

static Scheme_Object *drop_zero_value_return(Scheme_Object *expr)
{
  if (SAME_TYPE(SCHEME_TYPE(expr), scheme_sequence_type)) {
    if (((Scheme_Sequence *)expr)->count == 2) {
      if (SAME_TYPE(SCHEME_TYPE(((Scheme_Sequence *)expr)->array[1]), scheme_application_type)) {
        if (((Scheme_App_Rec *)((Scheme_Sequence *)expr)->array[1])->num_args == 0) {
          if (SAME_OBJ(scheme_values_proc, ((Scheme_App_Rec *)((Scheme_Sequence *)expr)->array[1])->args[0])) {
            return ((Scheme_Sequence *)expr)->array[0];
          }
        }
      }
    }
  }

  return NULL;
}

#define HAS_UNBOXABLE_TYPE(var) ((var)->val_type && (!(var)->escapes_after_k_tick || ALWAYS_PREFER_UNBOX_TYPE((var)->val_type)))

static int check_need_boxed_letrec_rhs(Scheme_IR_Let_Header *head, Scheme_Hash_Tree *binding_vars, Resolve_Info *info,
                                       int *_num_rec_procs, int *_rec_proc_nonapply)
/* Check whether a `let`+`set!` is needed to implement a set of `letrec` bindings;
   the result is true if so, otherwise report the number of bindings that are
   functions for a function-only `letrec`. Set `_rec_proc_nonapply` if any binding
   is used in a non-application position, since that will disable lifting for
   closure conversion. */
{
  int recbox = 0;
  Scheme_IR_Let_Value *irlv;
  int i;

  irlv = (Scheme_IR_Let_Value *)head->body;
  for (i = head->num_clauses; i--; irlv = (Scheme_IR_Let_Value *)irlv->body) {
    int is_proc, is_lift;

    if ((irlv->count == 1)
        && !irlv->vars[0]->optimize_used
        && scheme_omittable_expr(irlv->value, irlv->count, -1, 0, NULL, NULL)) {
      /* record omittable, so we don't have to keep checking: */
      irlv->vars[0]->resolve_omittable = 1;
    } else {
      if (irlv->count == 1)
        is_proc = scheme_is_ir_lambda(irlv->value, 1, 1);
      else
        is_proc = 0;

      if (is_proc)
        is_lift = 0;
      else if (SCHEME_IRLV_FLAGS(irlv) & SCHEME_IRLV_NO_GROUP_USES)
        is_lift = 1;
      else
        is_lift = scheme_is_liftable(irlv->value, binding_vars, 5, 1, 0);

      if (!is_proc && !is_lift) {
        recbox = 1;
        break;
      } else {
        if (!is_lift) {
          /* is_proc must be true ... */
          int j;

          for (j = 0; j < irlv->count; j++) {
            if (irlv->vars[j]->mutated) {
              recbox = 1;
              break;
            }
          }
          if (recbox)
            break;

          if (is_nonconstant_procedure(irlv->value, info, binding_vars)) {
            (*_num_rec_procs)++;
            if (irlv->vars[0]->non_app_count)
              *_rec_proc_nonapply = 1;
          }
        }
      }
    }
  }

  if (recbox)
    *_num_rec_procs = 0;

  return recbox;
}

static Scheme_Object *build_let_one_chain(Scheme_IR_Let_Header *head, Scheme_Object *body, Resolve_Info *info)
/* Build a chain of Scheme_Let_One records for a simple binding set */
{
  Scheme_IR_Let_Value *irlv;
  Scheme_Let_Value *last = NULL;
  Scheme_Object *first = NULL;
  int i, j, num_frames;
  Resolve_Info *linfo;

  j = head->num_clauses;

  irlv = (Scheme_IR_Let_Value *)head->body;
  for (i = 0; i < j; i++, irlv = (Scheme_IR_Let_Value *)irlv->body) {
    if (irlv->vars[0]->optimize_used) {
      int aty, pty, involes_k_cross;
      aty = irlv->vars[0]->arg_type;
      pty = scheme_expr_produces_local_type(irlv->value, &involes_k_cross);
      if (pty && !involes_k_cross && ((pty == aty) || ALWAYS_PREFER_UNBOX_TYPE(pty)))
        irlv->vars[0]->val_type = pty;
      else
        irlv->vars[0]->val_type = 0;
    }
  }

  irlv = (Scheme_IR_Let_Value *)head->body;
  linfo = info;
  num_frames = 0;
  for (i = 0; i < head->num_clauses; i++, irlv = (Scheme_IR_Let_Value *)irlv->body) {
    Scheme_Object *le;

    if (!irlv->vars[0]->optimize_used
        && scheme_omittable_expr(irlv->value, irlv->count, -1, 0, NULL, NULL)) {
      /* unused and omittable; skip */
    } else {
      linfo = resolve_info_extend(linfo, 1, 0);
      num_frames++;
      set_resolve_mode(irlv->vars[0]);
      irlv->vars[0]->resolve.co_depth = linfo->current_depth;
      irlv->vars[0]->resolve.lex_depth = linfo->current_lex_depth;

      if (!info->no_lift
          && !irlv->vars[0]->non_app_count
          && SAME_TYPE(SCHEME_TYPE(irlv->value), scheme_ir_lambda_type))
        le = resolve_lambda(irlv->value, linfo, 1, 1, 0, NULL);
      else
        le = scheme_resolve_expr(irlv->value, linfo);

      if (is_lifted_reference(le)) {
        MZ_ASSERT(!info->no_lift);
        irlv->vars[0]->resolve.lifted = le;
        /* Use of binding will be replaced by lift, so drop binding. */
        linfo = linfo->next;
        --num_frames;
      } else {
        Scheme_Let_One *lo;
        int et;

        irlv->vars[0]->resolve.lifted = NULL;

        lo = MALLOC_ONE_TAGGED(Scheme_Let_One);
        lo->iso.so.type = scheme_let_one_type;
        MZ_ASSERT(!SCHEME_RPAIRP(le));
        lo->value = le;

        et = scheme_get_eval_type(lo->value);
        if (HAS_UNBOXABLE_TYPE(irlv->vars[0]))
          et |= (irlv->vars[0]->val_type << LET_ONE_TYPE_SHIFT);
        SCHEME_LET_EVAL_TYPE(lo) = et;

        if (last)
          ((Scheme_Let_One *)last)->body = (Scheme_Object *)lo;
        else
          first = (Scheme_Object *)lo;
        last = (Scheme_Let_Value *)lo;
      }
    }
  }

  body = scheme_resolve_expr(body, linfo);
  if (last)
    ((Scheme_Let_One *)last)->body = body;
  else
    first = body;

  for (i = 0; i < num_frames; i++) {
    merge_resolve(linfo->next, linfo);
    linfo = linfo->next;
  }

  return first;
}

static int all_unused_and_omittable(Scheme_IR_Let_Header *head)
{
  Scheme_IR_Let_Value *irlv;
  int i, j, any_used = 0;

  irlv = (Scheme_IR_Let_Value *)head->body;
  for (i = head->num_clauses; i--; irlv = (Scheme_IR_Let_Value *)irlv->body) {
    for (j = irlv->count; j--; ) {
      if (irlv->vars[j]->optimize_used) {
        any_used = 1;
        break;
      }
    }
    if (((irlv->count == 1) || !any_used)
        && scheme_omittable_expr(irlv->value, irlv->count, -1, 0, NULL, NULL)) {
      if ((irlv->count == 1) && !irlv->vars[0]->optimize_used)
        irlv->vars[0]->resolve_omittable = 1;
    } else
      any_used = 1;
  }

  return !any_used;
}

static Resolve_Info *compute_possible_lifts(Scheme_IR_Let_Header *head, Resolve_Info *info, Scheme_Hash_Tree *binding_vars,
                                            int recbox, int num_skips, int num_rec_procs, int rec_proc_nonapply,
                                            GC_CAN_IGNORE int *_lifted_recs)
/* First assume that all letrec-bound procedures can be lifted to empty closures.
   Then try assuming that all letrec-bound procedures can be at least lifted.
   Then fall back to assuming no lifts.
   Returns a resolve frame that is set up with lift decisions, and sets
   `_lifted_recs` to indicate the number of lifted functions. */
{
  int resolve_phase;
  Resolve_Info *linfo;
  int i, pos, rpos, lifted_recs = 0;
  Scheme_IR_Let_Value *irlv;
  
  linfo = NULL;
  for (resolve_phase = ((num_rec_procs && !rec_proc_nonapply && !info->no_lift) ? 0 : 2); 
       resolve_phase < 3; 
       resolve_phase++) {

    /* Don't try plain lifting if we're not inside a proc: */
    if ((resolve_phase == 1) && (!resolve_is_inside_proc(info)
                                 || !resolve_has_toplevel(info)))
      resolve_phase = 2;

    if (resolve_phase < 2) {
      linfo = resolve_info_extend(info, head->count - num_rec_procs - num_skips, 0);
      lifted_recs = 1;
    } else {
      linfo = resolve_info_extend(info, head->count - num_skips, 0);
      lifted_recs = 0;
    }

    /* Shuffle procedure letrecs to fall together in the shallowest part. Also determine
       and initialize lifts for recursive procedures. Generating lift information
       requires an iteration. */
    irlv = (Scheme_IR_Let_Value *)head->body;
    pos = ((resolve_phase < 2) ? 0 : num_rec_procs);
    rpos = 0;
    for (i = head->num_clauses; i--; irlv = (Scheme_IR_Let_Value *)irlv->body) {
      int j;

      if ((irlv->count == 1)
          && !irlv->vars[0]->optimize_used
          && irlv->vars[0]->resolve_omittable) {
        /* skipped */
      } else {
        for (j = 0; j < irlv->count; j++) {
          Scheme_Object *lift;

          set_resolve_mode(irlv->vars[j]);
          if (recbox)
            irlv->vars[j]->mutated = 1;

          if (num_rec_procs
              && (irlv->count == 1)
              && is_nonconstant_procedure(irlv->value, info, binding_vars)) {
            MZ_ASSERT(!recbox);
            if (resolve_phase == 0)
              lift = scheme_resolve_generate_stub_closure();
            else if (resolve_phase == 1)
              lift = resolve_generate_stub_lift();
            else
              lift = NULL;
            MZ_ASSERT(!info->no_lift || !lift);
            irlv->vars[0]->resolve.lifted = lift;
            irlv->vars[0]->resolve.co_depth = linfo->current_depth - rpos;
            irlv->vars[0]->resolve.lex_depth = linfo->current_lex_depth - rpos;
            rpos++;
          } else {
            irlv->vars[j]->resolve.lifted = NULL;
            irlv->vars[j]->resolve.co_depth = linfo->current_depth - pos;
            irlv->vars[j]->resolve.lex_depth = linfo->current_lex_depth - pos;
            /* Since Scheme_Let_Value doesn't record type info, we have
               to drop any unboxing type info recorded for the variable: */
            irlv->vars[j]->val_type = 0;
            pos++;
          }
        }
      }
    }

    if (resolve_phase < 2) {
      /* Given the assumption that all are closed/lifted, compute
         actual lift info. We have to iterate if there are
         conversions, because a conversion can trigger another 
         conversion. If the conversion changes for an item, it's
         always by adding more conversion arguments. */
      int converted;
      do {
        irlv = (Scheme_IR_Let_Value *)head->body;
        converted = 0;
        for (i = head->num_clauses; i--; irlv = (Scheme_IR_Let_Value *)irlv->body) {
          if ((irlv->count == 1)
              && !irlv->vars[0]->optimize_used
              && irlv->vars[0]->resolve_omittable) {
            /* skipped */
          } else if ((irlv->count == 1)
                     && is_nonconstant_procedure(irlv->value, info, binding_vars)) {
            Scheme_Object *lift, *old_lift;
            int old_convert_count;
            Scheme_Object *old_convert_map, *convert_map;

            old_lift = irlv->vars[0]->resolve.lifted;
            old_convert_count = get_convert_arg_count(old_lift);
            old_convert_map = get_convert_arg_map(old_lift);

            lift = resolve_lambda(irlv->value, linfo, 1, 1, 1,
                                  (resolve_phase ? NULL : old_lift));

            if (!info->no_lift
                && (is_closed_reference(lift)
                    || (is_lifted_reference(lift) && resolve_phase))) {
              if (!SAME_OBJ(old_lift, lift))
                irlv->vars[0]->resolve.lifted = lift;
              if (get_convert_arg_count(lift) != old_convert_count)
                converted = 1;
              else if (old_convert_map) {
                int z;
                convert_map = get_convert_arg_map(lift);
                for (z = 0; z < old_convert_count; z++) {
                  if (SCHEME_VEC_ELS(old_convert_map)[z+1] != SCHEME_VEC_ELS(convert_map)[z+1])
                    converted = 1;
                }
              }
            } else {
              lifted_recs = 0;
              converted = 0;
              break;
            }
          }
        }
      } while (converted);

      if (lifted_recs) {
        /* All can be closed or lifted --- and some may be converted.
           For the converted ones, the argument conversion is right. For
           lifted ones, we need to generate the actual offset. For fully
           closed ones, we need the actual closure. 

           If we succeeded with resolve_phase == 0, then all can be
           fully closed. We need to resolve again with the stub
           closures in place, and the mutate the stub closures with
           the actual closure info.

           If we succeeded with resolve_phase == 1, then we need
           actual lift offsets before resolving procedure bodies.
           Also, we need to fix up the stub closures. */
        irlv = (Scheme_IR_Let_Value *)head->body;
        for (i = head->num_clauses; i--; irlv = (Scheme_IR_Let_Value *)irlv->body) {
          if ((irlv->count == 1)
              && !irlv->vars[0]->optimize_used
              && irlv->vars[0]->resolve_omittable) {
            /* skipped */
          } else if ((irlv->count == 1) && is_nonconstant_procedure(irlv->value, info, binding_vars)) {
            Scheme_Object *lift;
            lift = irlv->vars[0]->resolve.lifted;
            if (is_closed_reference(lift)) {
              (void)resolve_lambda(irlv->value, linfo, 1, 1, 0, lift);
              /* lift is the final result; this result might be
                 referenced in the body of closures already, or in
                 not-yet-closed functions.  If no one uses the result
                 via linfo, then the code was dead and it will get
                 GCed. */
              irlv->value = NULL; /* indicates that there's nothing more to do with the expr */
            } else {
              lift = resolve_lambda(irlv->value, linfo, 1, 1, 2, NULL);
              /* need to resolve one more time for the body of the lifted function */
              irlv->vars[0]->resolve.lifted = lift;
            }
          }
        }

        break; /* don't need to iterate */
      }
    }
  }

  *_lifted_recs = lifted_recs;

  return linfo;
}

Scheme_Object *scheme_resolve_lets(Scheme_Object *form, Resolve_Info *info)
/* Convert a Scheme_IR_Let_Header plus Scheme_IR_Let_Value records
   into either a sequence of Scheme_Let_One records or Scheme_Let_Void
   plus either Scheme_Letrec or Scheme_Let_Value records. Also, check
   whether functions that are locally bound can be lifted through
   closure conversion. The closure-conversion step may require
   iteration to a fixpoint to determine whether a set of
   mutually-referential functions can be lifted together, and whether
   they must be lifted to the top level or module level (bacsue they
   refer to other top-level or module-level bindings) or whether they
   can be converted to constant empty closures. */
{
  Resolve_Info *linfo;
  Scheme_IR_Let_Header *head = (Scheme_IR_Let_Header *)form;
  Scheme_IR_Let_Value *irlv, *pre_body;
  Scheme_Let_Value *lv, *last = NULL;
  Scheme_Object *first = NULL, *body, *last_body = NULL, *last_seq = NULL;
  Scheme_Letrec *letrec;
  Scheme_Object *boxes;
  int i, j, rpos, recbox, num_rec_procs = 0, extra_alloc;
  int rec_proc_nonapply = 0;
  int num_skips, lifted_recs;
  Scheme_Hash_Tree *binding_vars;

  /* Find body and make a set of local bindings: */
  body = head->body;
  pre_body = NULL;
  binding_vars = scheme_make_hash_tree(0);
  for (i = head->num_clauses; i--; ) {
    pre_body = (Scheme_IR_Let_Value *)body;
    for (j = 0; j < pre_body->count; j++) {
      binding_vars = scheme_hash_tree_set(binding_vars, (Scheme_Object *)pre_body->vars[j], scheme_true);
    }
    body = pre_body->body;
  }

  recbox = 0;
  if (SCHEME_LET_FLAGS(head) & SCHEME_LET_RECURSIVE) {
    /* Do we need to box vars in a letrec? */
    recbox = check_need_boxed_letrec_rhs(head, binding_vars, info,
                                         &num_rec_procs, &rec_proc_nonapply);
  } else {
    /* Sequence of single-value, non-assigned lets? */

    irlv = (Scheme_IR_Let_Value *)head->body;
    for (i = head->num_clauses; i--; irlv = (Scheme_IR_Let_Value *)irlv->body) {
      if (irlv->count != 1)
	break;
      if (irlv->vars[0]->mutated)
	break;
    }

    if (i < 0) {
      /* Yes - build chain of Scheme_Let_Ones and we're done: */
      return build_let_one_chain(head, body, info);
    } else {
      /* Maybe some multi-binding lets, but all of them are unused and
         the RHSes are omittable? This can happen with auto-generated
         code. Checking has the side effect of setting
         `resolve_omittable` fields. */
      if (all_unused_and_omittable(head)) {
        /* All unused and omittable */
        return scheme_resolve_expr(body, info);
      }
    }
  }

  /* Count number of right-hand sides to be skipped entirely */
  num_skips = 0;
  irlv = (Scheme_IR_Let_Value *)head->body;
  for (i = head->num_clauses; i--; irlv = (Scheme_IR_Let_Value *)irlv->body) {
    if ((irlv->count == 1) && irlv->vars[0]->resolve_omittable) {
      num_skips++;
    }
  }

  /* Compute lifts */
  linfo = compute_possible_lifts(head, info, binding_vars,
                                 recbox, num_skips, num_rec_procs, rec_proc_nonapply,
                                 &lifted_recs);

  extra_alloc = 0;
  
  if (num_rec_procs) {
    if (!lifted_recs) {
      /* Since we didn't lift, prepare a frame for function-only
         `letrec`; non-function bindings will be put in additional
         Scheme_Let_Value steps. */
      Scheme_Object **sa;
      letrec = MALLOC_ONE_TAGGED(Scheme_Letrec);
      letrec->so.type = scheme_letrec_type;
      letrec->count = num_rec_procs;
      sa = MALLOC_N(Scheme_Object *, num_rec_procs);
      letrec->procs = sa;
    } else {
      extra_alloc = -num_rec_procs;
      letrec = NULL;
    }
  } else
    letrec = NULL;

  /* Resolve right-hand sides: */
  boxes = scheme_null;
  irlv = (Scheme_IR_Let_Value *)head->body;
  rpos = 0;
  for (i = head->num_clauses; i--; irlv = (Scheme_IR_Let_Value *)irlv->body) {
    if ((irlv->count == 1)
        && !irlv->vars[0]->optimize_used
        && irlv->vars[0]->resolve_omittable) {
      /* skipped */
    } else {
      int isproc;
      Scheme_Object *expr;
      if (!irlv->value)
        isproc = 1;
      else if (irlv->count == 1)
        isproc = is_nonconstant_procedure(irlv->value, info, binding_vars);
      else
        isproc = 0;
      if (num_rec_procs && isproc) {
        if (!lifted_recs) {
          expr = resolve_lambda(irlv->value, linfo, 0, 0, 0, NULL);
          if (!SAME_TYPE(SCHEME_TYPE(expr), scheme_lambda_type)) {
            scheme_signal_error("internal error: unexpected empty closure");
          }
          letrec->procs[rpos++] = expr;
        } else {
          if (!is_closed_reference(irlv->vars[0]->resolve.lifted)) {
            /* Side-effect is to install lifted function: */
            (void)resolve_lambda(irlv->value, linfo, 1, 1, 0, irlv->vars[0]->resolve.lifted);
          }
          rpos++;
        }
      } else {
        int j;

        if (!irlv->count)
          expr = drop_zero_value_return(irlv->value);
        else
          expr = NULL;

        if (expr) {
          /* Change a `[() (begin expr (values))]' clause,
             which can be generated by internal-definition expansion,
             into a `begin' */
          expr = scheme_resolve_expr(expr, linfo);
          expr = scheme_make_sequence_compilation(scheme_make_pair(expr,
                                                                   scheme_make_pair(scheme_false,
                                                                                    scheme_null)),
                                                  0,
                                                  0);
          
          if (last)
            last->body = expr;
          else if (last_body)
            SCHEME_PTR2_VAL(last_body) = expr;
          else if (last_seq)
            ((Scheme_Sequence *)last_seq)->array[1] = expr;
          else
            first = expr;
          last = NULL;
          last_body = NULL;
          last_seq = expr;
        } else {
          expr = scheme_resolve_expr(irlv->value, linfo);

          lv = MALLOC_ONE_TAGGED(Scheme_Let_Value);
          if (last)
            last->body = (Scheme_Object *)lv;
          else if (last_body)
            SCHEME_PTR2_VAL(last_body) = (Scheme_Object *)lv;
          else if (last_seq)
            ((Scheme_Sequence *)last_seq)->array[1] = (Scheme_Object *)lv;
          else
            first = (Scheme_Object *)lv;
          last = lv;
          last_body = NULL;
          last_seq = NULL;
      
          lv->iso.so.type = scheme_let_value_type;
          lv->value = expr;
          if (irlv->count) {
            int li;
            li = resolve_info_lookup(linfo, irlv->vars[0], NULL, 0, RESOLVE_UNUSED_OK);
            lv->position = li;
          } else
            lv->position = 0;
          lv->count = irlv->count;
          SCHEME_LET_VALUE_AUTOBOX(lv) = recbox;

          for (j = lv->count; j--; ) {
            if (!recbox && irlv->vars[j]->mutated) {
              GC_CAN_IGNORE Scheme_Object *pos;
              pos = scheme_make_integer(lv->position + j);
              if (SCHEME_LET_FLAGS(head) & SCHEME_LET_RECURSIVE) {
                /* For let* or a let*-like letrec, we need to insert the boxes after each evaluation. */
                Scheme_Object *boxenv;
                
                boxenv = scheme_alloc_object();
                boxenv->type = scheme_boxenv_type;
                SCHEME_PTR1_VAL(boxenv) = pos;
                SCHEME_PTR2_VAL(boxenv) = scheme_false;

                if (last)
                  last->body = boxenv;
                else if (last_seq)
                  ((Scheme_Sequence *)last_seq)->array[1] = boxenv;
                else
                  SCHEME_PTR2_VAL(last_body) = boxenv;
                last = NULL;
                last_body = boxenv;
                last_seq = NULL;
              } else {
                /* For regular let, delay the boxing until all RHSs are
                   evaluated. */
                boxes = scheme_make_pair(pos, boxes);
              }
            }
          }
        }
      }
    }
  }

  /* Resolve body: */
  body = scheme_resolve_expr((Scheme_Object *)irlv, linfo);

  while (SCHEME_PAIRP(boxes)) {
    /* See bangboxenv... */
    Scheme_Object *bcode;
    bcode = scheme_alloc_object();
    bcode->type = scheme_boxenv_type;
    SCHEME_PTR1_VAL(bcode) = SCHEME_CAR(boxes);
    SCHEME_PTR2_VAL(bcode) = body;
    body = bcode;
    boxes = SCHEME_CDR(boxes);
  }

  /* Link up function-only `letrec` and Scheme_Let_Values chain */
  if (letrec) {
    letrec->body = body;
    if (last)
      last->body = (Scheme_Object *)letrec;
    else if (last_body)
      SCHEME_PTR2_VAL(last_body) = (Scheme_Object *)letrec;
    else if (last_seq)
      ((Scheme_Sequence *)last_seq)->array[1] = (Scheme_Object *)letrec;
    else
      first = (Scheme_Object *)letrec;
  } else if (last)
    last->body = body;
  else if (last_body)
    SCHEME_PTR2_VAL(last_body) = body;
  else if (last_seq)
    ((Scheme_Sequence *)last_seq)->array[1] = (Scheme_Object *)body;
  else
    first = body;

  /* Check one last time for a simplification: */
  if (head->count + extra_alloc - num_skips) {
    int cnt;

    cnt = head->count + extra_alloc - num_skips;

    if (!recbox && (cnt == 1)
        && (SAME_TYPE(SCHEME_TYPE(first), scheme_let_value_type))
        && (((Scheme_Let_Value *)first)->count == 1)
        && (((Scheme_Let_Value *)first)->position == 0)) {
      /* Simplify to let-one after all */
      Scheme_Let_One *lo;
      int et;

      lo = MALLOC_ONE_TAGGED(Scheme_Let_One);
      lo->iso.so.type = scheme_let_one_type;
      lo->value = ((Scheme_Let_Value *)first)->value;
      lo->body = ((Scheme_Let_Value *)first)->body;
      
      et = scheme_get_eval_type(lo->value);
      SCHEME_LET_EVAL_TYPE(lo) = et;

      first = (Scheme_Object *)lo;
    } else {
      Scheme_Let_Void *lvd;
      
      lvd = MALLOC_ONE_TAGGED(Scheme_Let_Void);
      lvd->iso.so.type = scheme_let_void_type;
      lvd->body = first;
      lvd->count = cnt;
      SCHEME_LET_VOID_AUTOBOX(lvd) = recbox;
      
      first = (Scheme_Object *)lvd;
    }
  }

  merge_resolve(info, linfo);

  return first;
}

/*========================================================================*/
/*                               lambda                                   */
/*========================================================================*/

XFORM_NONGCING int scheme_boxmap_size(int n)
{
  return ((LAMBDA_TYPE_BITS_PER_ARG * n) + (BITS_PER_MZSHORT - 1)) / BITS_PER_MZSHORT;
}

void scheme_boxmap_set(mzshort *boxmap, int j, int bit, int delta)
/* assumes that existing bits are cleared */
{
  j *= LAMBDA_TYPE_BITS_PER_ARG;
  boxmap[delta + (j / BITS_PER_MZSHORT)] |= ((mzshort)bit << (j & (BITS_PER_MZSHORT - 1)));
}

int scheme_boxmap_get(mzshort *boxmap, int j, int delta)
{
  j *= LAMBDA_TYPE_BITS_PER_ARG;
  return (boxmap[delta + (j / BITS_PER_MZSHORT)] >> (j & (BITS_PER_MZSHORT - 1))
          & ((1 << LAMBDA_TYPE_BITS_PER_ARG) - 1));
}

static int is_nonconstant_procedure(Scheme_Object *_lam, Resolve_Info *info, Scheme_Hash_Tree *exclude_vars)
{
  /* check whether `_lam' --- which is in a `letrec' --- can be converted to
     a constant independent of other bindings in the `letrec' */
  Scheme_Lambda *lam;
  Scheme_IR_Lambda_Info *cl;
  Scheme_Object *lifted;
  int i;

  if (SAME_TYPE(SCHEME_TYPE(_lam), scheme_ir_lambda_type)) {
    lam = (Scheme_Lambda *)_lam;

    cl = lam->ir_info;
    if (cl->has_tl)
      return 1;

    for (i = 0; i < cl->base_closure->size; i++) {
      if (cl->base_closure->vals[i]) {
        Scheme_IR_Local *var = (Scheme_IR_Local *)cl->base_closure->keys[i];

        if (scheme_hash_tree_get(exclude_vars, (Scheme_Object *)var))
          return 1;

        if (var->optimize_used) {
          MZ_ASSERT(var->mode == SCHEME_VAR_MODE_RESOLVE);
          (void)resolve_info_lookup(info, var, &lifted, 0, 0);
          if (!lifted)
            return 1;
          if (SAME_TYPE(SCHEME_TYPE(lifted), scheme_toplevel_type)
              || SAME_TYPE(SCHEME_TYPE(SCHEME_CAR(lifted)), scheme_toplevel_type))
            return 1;
        }
      }
    }

    return 0;
  }

  return 0;
}

static Scheme_Object *
resolve_lambda(Scheme_Object *_lam, Resolve_Info *info, 
               int can_lift, int convert, int just_compute_lift,
               Scheme_Object *precomputed_lift)
{
  Scheme_Lambda *lam;
  int i, closure_size, new_params, num_params;
  int need_type_map = 0;
  int has_tl, need_lift, using_lifted = 0;
  mzshort *closure_map;
  Scheme_IR_Lambda_Info *cl;
  Resolve_Info *new_info;
  Scheme_Object *lifted, *result, *lifteds = NULL;
  Scheme_Hash_Table *captured = NULL;

  lam = (Scheme_Lambda *)_lam;
  cl = lam->ir_info;
  if (!just_compute_lift)
    lam->iso.so.type = scheme_lambda_type;

  if (convert || can_lift) {
    if (!convert && !resolve_is_inside_proc(info))
      can_lift = 0; /* no point in lifting when outside of a lambda or letrec */
    if (!info->lifts)
      can_lift = 0;
  }

  /* Check possibility of unboxing arguments: */
  closure_size = lam->closure_size;
  if (cl->arg_types) {
    int at_least_one = 0;
    for (i = lam->num_params; i--; ) {
      if (cl->arg_types[i]) {
        int ct;
        ct = scheme_predicate_to_local_type(cl->arg_types[i]);
        if (ct
            && (cl->vars[i]->arg_type == ct)
            && (!cl->vars[i]->escapes_after_k_tick
                || ALWAYS_PREFER_UNBOX_TYPE(cl->vars[i]->arg_type)))
          at_least_one = 1;
        else
          cl->arg_types[i] = NULL;
      }
    }
    if (at_least_one)
      need_type_map = 1;
    else
      cl->arg_types = NULL;
  }

  has_tl = cl->has_tl;
  
  /* Add original closure content to `captured`, pruning variables
     that are lifted (so the closure might get smaller). The
     `captured' table maps variables to new positions relative to the
     current stack. */
  closure_size = 0;
  captured = scheme_make_hash_table(SCHEME_hash_ptr);
  for (i = 0; i < cl->base_closure->size; i++) {
    if (cl->base_closure->vals[i]) {
      Scheme_IR_Local *var = SCHEME_VAR(cl->base_closure->keys[i]);

      if ((var->mode == SCHEME_VAR_MODE_OPTIMIZE)
          || !var->optimize_used) {
        /* reference must have been optimized away; drop it
           from the closure */
      } else {
        (void)resolve_info_lookup(info, var, &lifted, 0, 0);
        if (lifted) {
          /* Drop lifted binding from closure. */
          if (SAME_TYPE(SCHEME_TYPE(lifted), scheme_toplevel_type)
              || SAME_TYPE(SCHEME_TYPE(SCHEME_CAR(lifted)), scheme_toplevel_type)) {
            /* Former local variable is now a top-level variable. */
            has_tl = 1;
          }
          /* If the lifted binding is for a converted closure,
             we may need to add more bindings to this closure. */
          if (SCHEME_RPAIRP(lifted)) {
            lifteds = scheme_make_raw_pair(lifted, lifteds);
            using_lifted = 1;
          }
        } else {
          scheme_hash_set(captured, (Scheme_Object *)var, scheme_make_integer(closure_size));
          closure_size++;
          /* Currently, we only need type (not boxing) information for closure content: */
          if (HAS_UNBOXABLE_TYPE(var))
            need_type_map = 1;
        }
      }
    }
  }

  if (has_tl && !can_lift)
    convert = 0;

  /* Add variable references introduced by closure conversion. */
  while (lifteds) {
    int j, cnt;
    Scheme_Object *vec;

    lifted = SCHEME_CAR(lifteds);
    vec = SCHEME_CDR(lifted);
    cnt = SCHEME_VEC_SIZE(vec);
    --cnt;
    for (j = 0; j < cnt; j++) {
      Scheme_IR_Local *var = (Scheme_IR_Local *)SCHEME_VEC_ELS(vec)[j+1];
      if (!scheme_hash_get(captured, (Scheme_Object *)var)) {
        /* Need to capture an extra binding: */
        scheme_hash_set(captured, (Scheme_Object *)var, scheme_make_integer(captured->count));
        if (HAS_UNBOXABLE_TYPE(var))
          need_type_map = 1;
        closure_size++;
      }
    }

    lifteds = SCHEME_CDR(lifteds);
  }

  /* To make compilation deterministic, sort the captured variables */
  if (closure_size) {
    Scheme_IR_Local **c;
    int j = 0;
    c = MALLOC_N(Scheme_IR_Local*, closure_size);
    for (i = 0; i < captured->size; i++) {
      if (captured->vals[i]) {
        c[j++] = SCHEME_VAR(captured->keys[i]);
      }
    }
    scheme_sort_resolve_ir_local_array(c, closure_size);
    for (i = 0; i < closure_size; i++) {
      scheme_hash_set(captured, (Scheme_Object *)c[i], scheme_make_integer(i));
    }
  }

  if (convert && (closure_size || has_tl || using_lifted)) {
    new_params = closure_size;
    closure_size = 0;
  } else {
    new_params = 0;
    convert = 0;
  }

  /* Count the pointer to globals, if any: */
  if (has_tl) {
    /* GLOBAL ASSUMPTION: jit.c assumes that the array
       of globals is the last item in the closure; grep
       for "GLOBAL ASSUMPTION" in jit.c and mzmark.c */
    closure_size++;
  }

  /* New arguments due to closure conversion will be added before
     the original arguments: */
  num_params = lam->num_params + new_params;

  if ((num_params == 1)
      && !new_params
      && (SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_HAS_REST)
      && !cl->vars[0]->optimize_used) {
    /* We can claim 0 params plus LAMBDA_HAS_REST as an optimization */
    num_params = 0;
  }

  if (!just_compute_lift) {
    if (convert && !need_type_map && new_params) {
      /* As we turn closure content into arguments, we need mutation
         info, so double-check whether a type map is needed after all. */
      for (i = 0; i < captured->size; i++) {
        if (captured->vals[i]) {
          Scheme_IR_Local *var = SCHEME_VAR(captured->keys[i]);
          if (var->mutated) {
            need_type_map = 1;
            break;
          }
        }
      }
    }

    new_info = resolve_info_extend(info, num_params + closure_size, 1);
    
    lam->closure_size = closure_size;
    if (need_type_map)
      SCHEME_LAMBDA_FLAGS(lam) |= LAMBDA_HAS_TYPED_ARGS;

    MZ_ASSERT(need_type_map || !(SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_HAS_TYPED_ARGS));
    
    /* Create the closure map, if needed */
    if (closure_size || need_type_map) {
      int bmsz;
      if (need_type_map)
        bmsz = scheme_boxmap_size(closure_size + num_params);
      else
        bmsz = 0;
      bmsz += closure_size;
      closure_map = (mzshort *)scheme_malloc_atomic(sizeof(mzshort) * bmsz);
      memset(closure_map + closure_size, 0, sizeof(mzshort) * (bmsz - closure_size));
    } else
      closure_map = NULL;
    
    lam->closure_map = closure_map;
    lam->num_params = num_params;

    /* Register original argument names and types */
    for (i = 0; i < num_params - new_params; i++) {
      set_resolve_mode(cl->vars[i]);
      cl->vars[i]->resolve.co_depth = new_info->current_depth - (i + new_params + closure_size);
      cl->vars[i]->resolve.lex_depth = new_info->current_lex_depth - (i + new_params + closure_size);
      if (convert) {
        /* If we're lifting this function, then arguments can have unboxing
           types, because the valdiator will be able to check all the
           calls: */
        int lt;
        if (cl->arg_types) {
          lt = scheme_predicate_to_local_type(cl->arg_types[i]);
          cl->vars[i]->val_type = lt;
        } else
          lt = 0;
        if (need_type_map) {
          if (lt)
            scheme_boxmap_set(closure_map, i + new_params,
                              lt + LAMBDA_TYPE_TYPE_OFFSET,
                              closure_size);
        }
      }
    }

    /* Register closure content (possibly as new params) */
    for (i = 0; i < captured->size; i++) {
      if (captured->vals[i]) {
        int pos = SCHEME_INT_VAL(captured->vals[i]);
        Scheme_IR_Local *var = SCHEME_VAR(captured->keys[i]);
        resolve_info_add_mapping(new_info, var,
                                 scheme_make_integer(new_info->current_depth
                                                     - pos
                                                     - (convert
                                                        ? closure_size
                                                        : 0)));
        MZ_ASSERT(need_type_map || (!HAS_UNBOXABLE_TYPE(var) && (!var->mutated || !convert)));
        if (need_type_map) {
          scheme_boxmap_set(closure_map, (pos + (convert ? 0 : num_params)),
                            ((HAS_UNBOXABLE_TYPE(var)
                              ? (var->val_type + LAMBDA_TYPE_TYPE_OFFSET)
                              : 0)
                             | (convert
                                ? (var->mutated ? LAMBDA_TYPE_BOXED : 0)
                                : 0)),
                            closure_size);
        }
        if (!convert) {
          int li;
          li = resolve_info_lookup(info, var, NULL, 0, 0);
          closure_map[pos] = li;
        }
      }
    }

    if (has_tl) {
      /* array of globals is at the end: */
      resolve_info_set_toplevel_pos(new_info, closure_size - 1);
      if (closure_map) {
        int li;
        li = resolve_toplevel_pos(info);
        closure_map[closure_size-1] = li;
      }
    } else
      resolve_info_set_toplevel_pos(new_info, -1);

    /* Resolve the closure body: */
    {
      Scheme_Object *code;
      code = scheme_resolve_expr(lam->body, new_info);
      lam->body = code;
    }

    lam->max_let_depth = (new_info->max_let_depth
                           + SCHEME_TAIL_COPY_THRESHOLD);

    lam->tl_map = new_info->tl_map;
    if (!lam->tl_map && has_tl) {
      /* Our reason to refer to the top level has apparently gone away;
         record that we're not using anything */
      lam->tl_map = (void *)0x1;
    }

    /* Add code to box set!ed argument variables: */
    for (i = 0; i < num_params - new_params; i++) {
      if (cl->vars[i]->mutated) {
        int j = i + closure_size + new_params;
        Scheme_Object *bcode;
        
        bcode = scheme_alloc_object();
        bcode->type = scheme_boxenv_type;
        SCHEME_PTR1_VAL(bcode) = scheme_make_integer(j);
        SCHEME_PTR2_VAL(bcode) = lam->body;

        lam->body = bcode;
      }
    }
  } else {
    new_info = NULL;
    closure_map = NULL;
  }

  if ((closure_size == 1)
      && can_lift
      && has_tl
      && info->lifts) {
    need_lift = 1;
  } else
    need_lift = 0;

  /* If the closure is empty, create the closure now */
  if (!closure_size) {
    if (precomputed_lift) {
      result = SCHEME_CAR(precomputed_lift);
      if (!just_compute_lift)
        ((Scheme_Closure *)result)->code = lam;
    } else {
      if (just_compute_lift)
        result = (Scheme_Object *)scheme_malloc_empty_closure();
      else
        result = scheme_make_closure(NULL, (Scheme_Object *)lam, 0);
    }
  } else
    result = (Scheme_Object *)lam;
  
  if (need_lift) {
    if (just_compute_lift) {
      if (just_compute_lift > 1)
        result = resolve_invent_toplevel(info);
      else
        result = resolve_generate_stub_lift();
    } else {
      Scheme_Object *tl, *defn_tl;
      if (precomputed_lift) {
        tl = precomputed_lift;
        if (SCHEME_RPAIRP(tl))
          tl = SCHEME_CAR(tl);
      } else {
        tl = resolve_invent_toplevel(info);
      }
      defn_tl = resolve_invented_toplevel_to_defn(info, tl);
      resolve_lift_definition(info, defn_tl, result);
      if (has_tl)
        closure_map[0] = 0; /* globals for closure creation will be at 0 after lifting */
      result = tl;
    }
  } else if (!just_compute_lift) {
    merge_resolve(info, new_info);
  }
  
  if (convert) {
    /* Generate lift record, which is a vector containing
       the original arity and then each variable captured in the closure
       (or would be captured if there's no lift conversion). */
    Scheme_Object *ca, *arity;

    if ((SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_HAS_REST))
      arity = scheme_box(scheme_make_integer(num_params - new_params - 1));
    else
      arity = scheme_make_integer(num_params - new_params);

    ca = scheme_make_vector(1 + captured->count, scheme_false);
    SCHEME_VEC_ELS(ca)[0] = arity;

    for (i = 0; i < captured->size; i++) {
      if (captured->vals[i]) {
        MZ_ASSERT(SAME_TYPE(scheme_ir_local_type, SCHEME_TYPE(captured->keys[i])));
        SCHEME_VEC_ELS(ca)[1 + SCHEME_INT_VAL(captured->vals[i])] = captured->keys[i];
      }
    }

    if (precomputed_lift) {
      SCHEME_CAR(precomputed_lift) = result;
      SCHEME_CDR(precomputed_lift) = (Scheme_Object *)ca;
      result = precomputed_lift;
    } else
      result = scheme_make_raw_pair(result, (Scheme_Object *)ca);
  }

  return result;
}

/*========================================================================*/
/*                                module                                 */
/*========================================================================*/

static int has_syntax_constants(Scheme_Module *m)
{
  int i, j;
  Scheme_Object *e;
  Resolve_Prefix *rp;
  
  if (m->prefix->num_stxes)
    return 1;

  for (j = m->num_phases; j-- > 1; ) {
    for (i = SCHEME_VEC_SIZE(m->bodies[j]); i--; ) {
      e = SCHEME_VEC_ELS(m->bodies[j])[i];
      rp = (Resolve_Prefix *)SCHEME_VEC_ELS(e)[3];
      if (rp->num_stxes)
        return 1;
    }
  }

  return 0;
}

static Scheme_Object *
module_expr_resolve(Scheme_Object *data, Resolve_Info *old_rslv)
{
  Scheme_Module *m = (Scheme_Module *)data;
  Scheme_Object *b, *lift_vec, *body = scheme_null;
  Resolve_Prefix *rp;
  Resolve_Info *rslv;
  int i, cnt;

  if (!m->comp_prefix) {
    /* already resolved */
    return (Scheme_Object *)m;
  }

  rp = scheme_resolve_prefix(0, m->comp_prefix, m->insp);
  m->comp_prefix = NULL;

  b = scheme_resolve_expr(m->dummy, old_rslv);
  m->dummy = b;

  rslv = scheme_resolve_info_create(rp);
  rslv->enforce_const = old_rslv->enforce_const;
  rslv->in_module = 1;
  scheme_enable_expression_resolve_lifts(rslv);

  cnt = SCHEME_VEC_SIZE(m->bodies[0]);
  for (i = 0; i < cnt; i++) {
    Scheme_Object *e;
    e = scheme_resolve_expr(SCHEME_VEC_ELS(m->bodies[0])[i], rslv);
    
    /* add lift just before the expression that introduced it;
       this ordering is needed for bytecode validation of
       constantness for top-level references */
    lift_vec = rslv->lifts;
    if (!SCHEME_NULLP(SCHEME_VEC_ELS(lift_vec)[0])) {
      body = scheme_append(SCHEME_VEC_ELS(lift_vec)[0], body);
      SCHEME_VEC_ELS(lift_vec)[0] = scheme_null;
    }

    body = scheme_make_pair(e, body);
  }

  m->max_let_depth = rslv->max_let_depth;

  lift_vec = rslv->lifts;
  rp->num_lifts = SCHEME_INT_VAL(SCHEME_VEC_ELS(lift_vec)[1]);

  body = scheme_list_to_vector(scheme_reverse(body));
  m->bodies[0] = body;

  rp = scheme_remap_prefix(rp, rslv);

  m->prefix = rp;

  /* Exp-time body was resolved during compilation */

  /* If there are no syntax objects in the module, then there are no
     macros that can reach bindings in the bindings table whose marks
     are not a subset of the module context. */
  if (m->rn_stx && SCHEME_STXP(m->rn_stx) && !has_syntax_constants(m)) {
    if (m->binding_names) {
      b = scheme_prune_bindings_table(m->binding_names, m->rn_stx, scheme_make_integer(0));
      m->binding_names = b;
    }
    if (m->et_binding_names) {
      b = scheme_prune_bindings_table(m->et_binding_names, m->rn_stx, scheme_make_integer(1));
      m->et_binding_names = b;
    }
    if (m->other_binding_names) {
      intptr_t i;
      Scheme_Object *k, *val;
      Scheme_Hash_Tree *ht;

      ht = scheme_make_hash_tree(1);

      if (SCHEME_HASHTRP(m->other_binding_names)) {
        Scheme_Hash_Tree *t = (Scheme_Hash_Tree *)m->other_binding_names;
        for (i = scheme_hash_tree_next(t, -1); i != -1; i = scheme_hash_tree_next(t, i)) {
          scheme_hash_tree_index(t, i, &k, &val);
          val = scheme_prune_bindings_table(val, m->rn_stx, k);
          ht = scheme_hash_tree_set(ht, k, val);
        }
      } else {
        Scheme_Hash_Table *t = (Scheme_Hash_Table *)m->other_binding_names;
        for (i = t->size; i--; ) {
          if (t->vals[i]) {
            k = t->keys[i];
            val = t->vals[i];
            val = scheme_prune_bindings_table(val, m->rn_stx, k);
            ht = scheme_hash_tree_set(ht, k, val);
          }
        }
      }

      m->other_binding_names = (Scheme_Object *)ht;
    }
  }


  {
    /* resolve submodules */
    int k;
    Scheme_Object *p;
    for (k = 0; k < 2; k++) {
      p = (k ? m->post_submodules : m->pre_submodules);
      if (p) {
        while (!SCHEME_NULLP(p)) {
          scheme_resolve_expr(SCHEME_CAR(p), old_rslv);
          p = SCHEME_CDR(p);
        }
      }
    }
  }

  return data;
}

static Scheme_Object *
top_level_require_resolve(Scheme_Object *data, Resolve_Info *rslv)
{
  Scheme_Object *dummy = SCHEME_PTR1_VAL(data);

  dummy = scheme_resolve_expr(dummy, rslv);

  SCHEME_PTR1_VAL(data) = dummy;

  return data;
}

/*========================================================================*/
/*                              expressions                               */
/*========================================================================*/

static Scheme_Object *resolve_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *expr = (Scheme_Object *)p->ku.k.p1;
  Resolve_Info *info = (Resolve_Info *)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return scheme_resolve_expr(expr, info);
}

Scheme_Object *scheme_resolve_expr(Scheme_Object *expr, Resolve_Info *info)
{
  Scheme_Type type = SCHEME_TYPE(expr);

#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Thread *p = scheme_current_thread;

    p->ku.k.p1 = (void *)expr;
    p->ku.k.p2 = (void *)info;

    return scheme_handle_stack_overflow(resolve_k);
  }
#endif

  switch (type) {
  case scheme_ir_local_type:
    {
      int pos;
      Scheme_IR_Local *var = SCHEME_VAR(expr);
      Scheme_Object *lifted;
      
      pos = resolve_info_lookup(info, var, &lifted, 0, 0);
      if (lifted) {
        /* Lexical reference replaced with top-level reference for a lifted value: */
        return shift_lifted_reference(lifted, info, 0);
      } else {
        return scheme_make_local(var->mutated
                                 ? scheme_local_unbox_type
                                 : scheme_local_type,
                                 pos,
                                 (HAS_UNBOXABLE_TYPE(var)
                                  ? (SCHEME_LOCAL_TYPE_OFFSET + var->val_type)
                                  : 0));
      }
    }
  case scheme_application_type:
    return resolve_application(expr, info, 0);
  case scheme_application2_type:
    return resolve_application2(expr, info, 0);
  case scheme_application3_type:
    return resolve_application3(expr, info, 0);
  case scheme_sequence_type:
  case scheme_begin0_sequence_type:
  case scheme_splice_sequence_type:
    return resolve_sequence(expr, info);
  case scheme_branch_type:
    return resolve_branch(expr, info);
  case scheme_with_cont_mark_type:
    return resolve_wcm(expr, info);
  case scheme_ir_lambda_type:
    return resolve_lambda(expr, info, !info->no_lift, 0, 0, NULL);
  case scheme_ir_let_header_type:
    return scheme_resolve_lets(expr, info);
  case scheme_ir_toplevel_type:
    return resolve_toplevel(info, expr, 1);
  case scheme_ir_quote_syntax_type:
    {
      Scheme_Quote_Syntax *qs;
      int i, c, p;

      i = SCHEME_LOCAL_POS(expr);
      i = resolve_quote_syntax_offset(i, info);
      c = resolve_toplevel_pos(info);
      p = resolve_quote_syntax_pos(info);

      set_tl_pos_used(info, i+p+1);

      qs = MALLOC_ONE_TAGGED(Scheme_Quote_Syntax);
      qs->so.type = scheme_quote_syntax_type;
      qs->depth = c;
      qs->position = i;
      qs->midpoint = p;

      return (Scheme_Object *)qs;
    }
  case scheme_variable_type:
  case scheme_module_variable_type:
    scheme_signal_error("got top-level in wrong place");
    return 0;
  case scheme_define_values_type:
    return define_values_resolve(expr, info);
  case scheme_inline_variant_type:
    return inline_variant_resolve(expr, info);
  case scheme_define_syntaxes_type:
    return define_syntaxes_resolve(expr, info);
  case scheme_begin_for_syntax_type:
    return begin_for_syntax_resolve(expr, info);
  case scheme_set_bang_type:
    return set_resolve(expr, info);
  case scheme_require_form_type:
    return top_level_require_resolve(expr, info);
  case scheme_varref_form_type:
    return ref_resolve(expr, info);
  case scheme_apply_values_type:
    return apply_values_resolve(expr, info);
  case scheme_with_immed_mark_type:
    return with_immed_mark_resolve(expr, info);
  case scheme_case_lambda_sequence_type:
    return case_lambda_resolve(expr, info);
  case scheme_module_type:
    return module_expr_resolve(expr, info);
  case scheme_boxenv_type:
    scheme_signal_error("internal error: no boxenv resolve");
  default:
    return expr;
  }
}

Scheme_Object *scheme_resolve_list(Scheme_Object *expr, Resolve_Info *info)
{
  Scheme_Object *first = scheme_null, *last = NULL;

  while (SCHEME_PAIRP(expr)) {
    Scheme_Object *pr;

    pr = scheme_make_pair(scheme_resolve_expr(SCHEME_CAR(expr), info),
			  scheme_null);

    if (last)
      SCHEME_CDR(last) = pr;
    else
      first = pr;
    last = pr;

    expr = SCHEME_CDR(expr);
  }

  return first;
}

static Scheme_Object *resolve_info_lift_added(Resolve_Info *resolve, Scheme_Object *v, int convert_shift)
{
  /* If a variable added as an argument for closure conversion is mutable,
     we need to generate a non-unboxing reference to the variable: */
  Scheme_IR_Local *var;
  int pos;

  if (!SAME_TYPE(SCHEME_TYPE(v), scheme_ir_local_type)) {
    /* must be an argument to a generated "bad arity" call */
    return v;
  }

  var = SCHEME_VAR(v);
  
  pos = resolve_info_lookup(resolve, var, NULL, convert_shift, RESOLVE_IGNORE_LIFTS);
  
  return scheme_make_local(scheme_local_type,
                           pos,
                           ((!var->mutated && HAS_UNBOXABLE_TYPE(var))
                            ? (SCHEME_LOCAL_TYPE_OFFSET + var->val_type)
                            : 0));
}

static Scheme_Object *shift_lifted_reference(Scheme_Object *tl, Resolve_Info *info, int delta)
{
  int pos = SCHEME_TOPLEVEL_POS(tl);
  int depth;

  depth = resolve_toplevel_pos(info);
  tl = scheme_make_toplevel(depth + delta,
                            pos,
                            1,
                            SCHEME_TOPLEVEL_CONST);
  
  /* register if non-stub: */
  if (pos >= (info->prefix->num_toplevels
              + info->prefix->num_stxes
              + (info->prefix->num_stxes
                 ? 1
                 : 0)))
    set_tl_pos_used(info, pos);

  return tl;
}

/*========================================================================*/
/*                    compile-time env for resolve                        */
/*========================================================================*/

Resolve_Prefix *scheme_resolve_prefix(int phase, Comp_Prefix *cp, Scheme_Object *insp_desc)
{
  Resolve_Prefix *rp;
  Scheme_Object **tls, **stxes, *m;
  Scheme_Hash_Table *ht;
  int i;

  rp = MALLOC_ONE_TAGGED(Resolve_Prefix);
  rp->so.type = scheme_resolve_prefix_type;
  rp->num_toplevels = cp->num_toplevels;
  rp->num_stxes = cp->num_stxes;
  
  if (rp->num_toplevels)
    tls = MALLOC_N(Scheme_Object*, rp->num_toplevels);
  else
    tls = NULL;
  if (rp->num_stxes)
    stxes = MALLOC_N(Scheme_Object*, rp->num_stxes);
  else
    stxes = NULL;

  rp->toplevels = tls;
  rp->stxes = stxes;

  ht = cp->toplevels;
  if (ht) {
    for (i = 0; i < ht->size; i++) {
      if (ht->vals[i]) {
        m = ht->keys[i];
        if (SAME_TYPE(SCHEME_TYPE(m), scheme_module_variable_type)) {
          if (SCHEME_FALSEP(((Scheme_Modidx *)((Module_Variable *)m)->modidx)->base)
              && SCHEME_FALSEP(((Scheme_Modidx *)((Module_Variable *)m)->modidx)->path)) {
            /* Reduce self-referece to just a symbol: */
            m = ((Module_Variable *)m)->sym;
          }
        }
        tls[SCHEME_TOPLEVEL_POS(ht->vals[i])] = m;
      }
    }
  }

  ht = cp->stxes;
  if (ht) {
    for (i = 0; i < ht->size; i++) {
      if (ht->vals[i]) {
	stxes[SCHEME_LOCAL_POS(ht->vals[i])] = ht->keys[i];
      }
    }
  }

  rp->src_insp_desc = insp_desc;

  return rp;
}

Resolve_Prefix *scheme_remap_prefix(Resolve_Prefix *rp, Resolve_Info *ri)
{
  /* Rewrite stxes list based on actual uses at resolve pass.
     If we have no lifts, we can just drop unused stxes.
     Otherwise, if any stxes go unused, we just have to replace them
     with NULL. */
  int i, cnt;
  Scheme_Object **new_stxes, *v;

  if (!rp->num_stxes)
    return rp;

  if (rp->num_lifts)
    cnt = rp->num_stxes;
  else
    cnt = (int)ri->stx_map->count;

  new_stxes = MALLOC_N(Scheme_Object *, cnt);

  for (i = 0; i < rp->num_stxes; i++) {
    if (ri->stx_map)
      v = scheme_hash_get(ri->stx_map, scheme_make_integer(i));
    else
      v = NULL;
    if (v) {
      new_stxes[SCHEME_INT_VAL(v)]  = rp->stxes[i];
    }
  }

  rp->stxes = new_stxes;
  rp->num_stxes = cnt;

  return rp;
}

Resolve_Info *scheme_resolve_info_create(Resolve_Prefix *rp)
{
  Resolve_Info *naya;
  Scheme_Object *b;
  Scheme_Hash_Table *ht;

  naya = MALLOC_ONE_RT(Resolve_Info);
#ifdef MZTAG_REQUIRED
  naya->type = scheme_rt_resolve_info;
#endif
  naya->prefix = rp;
  naya->current_depth = 1; /* initial slot for prefix */
  naya->max_let_depth = naya->current_depth;
  naya->current_lex_depth = 0;
  naya->next = NULL;

  ht = scheme_make_hash_table(SCHEME_hash_ptr);
  naya->stx_map = ht;

  b = scheme_get_param(scheme_current_config(), MZCONFIG_USE_JIT);
  naya->use_jit = SCHEME_TRUEP(b);

  return naya;
}

void scheme_enable_expression_resolve_lifts(Resolve_Info *ri)
{
  Scheme_Object *lift_vec;

  lift_vec = scheme_make_vector(2, NULL);
  SCHEME_VEC_ELS(lift_vec)[0] = scheme_null;
  SCHEME_VEC_ELS(lift_vec)[1] = scheme_make_integer(0);
  ri->lifts = lift_vec;
}

Scheme_Object *scheme_merge_expression_resolve_lifts(Scheme_Object *expr, Resolve_Prefix *rp, Resolve_Info *ri)
{
  Scheme_Object *lift_vec, *lifts;
  Scheme_Sequence *s;
  int n, i;

  lift_vec = ri->lifts;
  n = SCHEME_INT_VAL(SCHEME_VEC_ELS(lift_vec)[1]);
  if (n) {
    rp->num_lifts = n;
    lifts = SCHEME_VEC_ELS(lift_vec)[0];

    s = scheme_malloc_sequence(n + 1);
    s->so.type = scheme_sequence_type;
    s->count = n + 1;
    for (i = 0; i < n; i++, lifts = SCHEME_CDR(lifts)) {
      s->array[i] = SCHEME_CAR(lifts);
    }
    s->array[i] = expr;

    return (Scheme_Object *)s;
  } else
    return expr;
}

void scheme_resolve_info_enforce_const(Resolve_Info *ri, int enforce_const)
{
  ri->enforce_const = enforce_const;
}

int scheme_resolve_info_use_jit(Resolve_Info *ri)
{
  return ri->use_jit;
}

int scheme_resolve_info_max_let_depth(Resolve_Info *ri)
{
  return ri->max_let_depth;
}

static Resolve_Info *resolve_info_extend(Resolve_Info *info, int size, int lambda)
/* size = number of appended items in run-time frame */
{
  Resolve_Info *naya;

  naya = MALLOC_ONE_RT(Resolve_Info);
#ifdef MZTAG_REQUIRED
  naya->type = scheme_rt_resolve_info;
#endif
  naya->prefix = info->prefix;
  naya->stx_map = info->stx_map;
  naya->next = (lambda ? NULL : info);
  naya->use_jit = info->use_jit;
  naya->enforce_const = info->enforce_const;
  naya->current_depth = (lambda ? 0 : info->current_depth) + size;
  naya->current_lex_depth = info->current_lex_depth + size;
  naya->toplevel_pos = (lambda
                        ? 0
                        : ((info->toplevel_pos < 0)
                           ? -1
                           : (info->toplevel_pos + size)));
  naya->no_lift = info->no_lift;
  naya->redirects = info->redirects;
  naya->max_let_depth = naya->current_depth;
  naya->in_proc = lambda || info->in_proc;
  naya->lifts = info->lifts;

  return naya;
}

static void *ensure_tl_map_len(void *old_tl_map, int new_len)
{
  int current_len;
  void *tl_map;

  if (!old_tl_map)
    current_len = 0;
  else if ((uintptr_t)old_tl_map & 0x1)
    current_len = 31;
  else
    current_len = (*(int *)old_tl_map) * 32;

  if (new_len > current_len) {
    /* allocate/grow tl_map */
    if (new_len <= 31)
      tl_map = (void *)0x1;
    else {
      int len = ((new_len + 31) / 32);
      tl_map = scheme_malloc_atomic((len + 1) * sizeof(int));
      memset(tl_map, 0, (len + 1) * sizeof(int));
      *(int *)tl_map = len;
    }

    if (old_tl_map) {
      if ((uintptr_t)old_tl_map & 0x1) {
        ((int *)tl_map)[1] = ((uintptr_t)old_tl_map >> 1) & 0x7FFFFFFF;
      } else {
        memcpy((int *)tl_map + 1,
               (int *)old_tl_map + 1,
               sizeof(int) * (current_len / 32));
      }
    }

    return tl_map;
  } else
    return old_tl_map;
}

static void set_tl_pos_used(Resolve_Info *info, int pos)
{
  int tl_pos;
  void *tl_map;

  /* Fixnum-like bit packing avoids allocation in the common case of a
     small prefix. We use 31 fixnum-like bits (even on a 64-bit
     platform, and even though fixnums are only 30 bits). There's one
     bit for each normal top-level, one bit for all syntax objects,
     and one bit for each lifted top-level. */

  if (pos > (info->prefix->num_toplevels + info->prefix->num_stxes))
    tl_pos = pos - info->prefix->num_stxes; /* lifted */
  else if (pos >= info->prefix->num_toplevels)
    tl_pos = info->prefix->num_toplevels; /* any syntax object */
  else
    tl_pos = pos; /* normal top level */

  tl_map = ensure_tl_map_len(info->tl_map, tl_pos + 1);
  info->tl_map = tl_map;

  if ((uintptr_t)info->tl_map & 0x1)
    info->tl_map = (void *)((uintptr_t)tl_map | ((uintptr_t)1 << (tl_pos + 1)));
  else
    ((int *)tl_map)[1 + (tl_pos / 32)] |= ((unsigned)1 << (tl_pos & 31));
}

static void *merge_tl_map(void *tl_map, void *new_tl_map)
{
  if (!tl_map)
    return new_tl_map;
  else if (!new_tl_map) 
    return tl_map;
  else if (((uintptr_t)new_tl_map) & 0x1) {
    if (((uintptr_t)tl_map) & 0x1) {
      return (void *)((uintptr_t)tl_map | (uintptr_t)new_tl_map);
    } else {
      ((int *)tl_map)[1] |= ((uintptr_t)new_tl_map >> 1) & 0x7FFFFFFF;
      return tl_map;
    }
  } else {
    int i, len = *(int *)new_tl_map;
    tl_map = ensure_tl_map_len(tl_map, len * 32);
    for (i = 0; i < len; i++) {
      ((int *)tl_map)[1+i] |= ((int *)new_tl_map)[1+i];
    }
    return tl_map;
  }
}

static void merge_resolve(Resolve_Info *info, Resolve_Info *new_info)
{
  if (new_info->next /* NULL => lambda */
      && (new_info->max_let_depth > info->max_let_depth))
    info->max_let_depth = new_info->max_let_depth;

  if (!new_info->tl_map) {
    /* nothing to do */
  } else {
    void *tl_map;
    tl_map = merge_tl_map(info->tl_map, new_info->tl_map);
    info->tl_map = tl_map;
  }
}

static void resolve_info_add_mapping(Resolve_Info *info, Scheme_IR_Local *var, Scheme_Object *v)
{
  Scheme_Hash_Tree *ht;

  if (!info->redirects) {
    ht = scheme_make_hash_tree(0);
    info->redirects = ht;
  }

  ht = scheme_hash_tree_set(info->redirects, (Scheme_Object *)var, v);
  info->redirects = ht;
}

static void resolve_info_set_toplevel_pos(Resolve_Info *info, int pos)
{
  info->toplevel_pos = pos;
}

static int resolve_info_lookup(Resolve_Info *info, Scheme_IR_Local *var, Scheme_Object **_lifted,
                               int convert_shift, int flags)
{
  Scheme_Object *v;
  int depth;
  
  MZ_ASSERT(var->mode == SCHEME_VAR_MODE_RESOLVE);
  MZ_ASSERT((flags & RESOLVE_UNUSED_OK) || (var->use_count > 0));
  MZ_ASSERT((flags & RESOLVE_UNUSED_OK) || var->optimize_used);

  if (var->resolve.lifted && !(flags & RESOLVE_IGNORE_LIFTS)) {
    MZ_ASSERT(_lifted);

    v = var->resolve.lifted;
    *_lifted = v;

    return -1;
  }

  depth = var->resolve.co_depth;
  if (info->redirects) {
    v = scheme_hash_tree_get(info->redirects, (Scheme_Object *)var);
    if (v) {
      depth = SCHEME_INT_VAL(v);
      MZ_ASSERT(var->val_type <= SCHEME_MAX_LOCAL_TYPE_MASK);
    }
  }

  if (_lifted)
    *_lifted = NULL;

  return info->current_depth - depth + convert_shift;
}

static Scheme_Object *resolve_generate_stub_lift()
{
  return scheme_make_toplevel(0, 0, 1, SCHEME_TOPLEVEL_CONST);
}

static int resolve_toplevel_pos(Resolve_Info *info)
{
  MZ_ASSERT(info->toplevel_pos >= 0);
  return info->toplevel_pos;
}

static int resolve_is_inside_proc(Resolve_Info *info)
{
  return info->in_proc;
}

static int resolve_has_toplevel(Resolve_Info *info)
{
  return info->toplevel_pos >= 0;
}
 
static int resolve_quote_syntax_offset(int i, Resolve_Info *info)
{
  Scheme_Hash_Table *ht;
  Scheme_Object *v;

  ht = info->stx_map;

  v = scheme_hash_get(ht, scheme_make_integer(i));
  if (!v) {
    v = scheme_make_integer(ht->count);
    scheme_hash_set(ht, scheme_make_integer(i), v);
  }

  return (int)SCHEME_INT_VAL(v);
}

static int resolve_quote_syntax_pos(Resolve_Info *info)
{
  return info->prefix->num_toplevels;
}

static Scheme_Object *resolve_toplevel(Resolve_Info *info, Scheme_Object *expr, int as_reference)
{
  int skip, pos;

  skip = resolve_toplevel_pos(info);

  pos = SCHEME_TOPLEVEL_POS(expr);

  set_tl_pos_used(info, pos);

  return scheme_make_toplevel(skip + SCHEME_TOPLEVEL_DEPTH(expr), /* depth is 0 (normal) or 1 (exp-time) */
                              pos,
                              1,
                              SCHEME_TOPLEVEL_FLAGS(expr) & SCHEME_TOPLEVEL_FLAGS_MASK);
}

static Scheme_Object *shift_toplevel(Scheme_Object *expr, int delta)
{
  return scheme_make_toplevel(SCHEME_TOPLEVEL_DEPTH(expr) + delta,
                              SCHEME_TOPLEVEL_POS(expr),
                              1,
                              SCHEME_TOPLEVEL_FLAGS(expr) & SCHEME_TOPLEVEL_FLAGS_MASK);
}

static Scheme_Object *resolve_invent_toplevel(Resolve_Info *info)
{
  int skip, pos;
  Scheme_Object *count;

  skip = resolve_toplevel_pos(info);

  count = SCHEME_VEC_ELS(info->lifts)[1];
  pos = (int)(SCHEME_INT_VAL(count)
              + info->prefix->num_toplevels 
              + info->prefix->num_stxes
              + (info->prefix->num_stxes ? 1 : 0));
  count = scheme_make_integer(SCHEME_INT_VAL(count) + 1);
  SCHEME_VEC_ELS(info->lifts)[1] = count;

  set_tl_pos_used(info, pos);

  return scheme_make_toplevel(skip,
                              pos,
                              1,
                              SCHEME_TOPLEVEL_CONST);
}

static Scheme_Object *resolve_invented_toplevel_to_defn(Resolve_Info *info, Scheme_Object *tl)
{
  return scheme_make_toplevel(0,
                              SCHEME_TOPLEVEL_POS(tl),
                              1,
                              SCHEME_TOPLEVEL_CONST);
}

/*========================================================================*/
/*                             unresolve                                  */
/*========================================================================*/

#if 0
# define return_NULL return (printf("%d\n", __LINE__), NULL)
#else
# define return_NULL return NULL
#endif

#if 0
# define LOG_UNRESOLVE(x) x
#else
# define LOG_UNRESOLVE(x) /* empty */
#endif

typedef struct Unresolve_Info {
  MZTAG_IF_REQUIRED
  int stack_pos; /* stack in resolved coordinates */
  int depth;     /* stack in unresolved coordinates */
  int stack_size;
  Scheme_IR_Local **vars;
  Resolve_Prefix *prefix;
  Scheme_Hash_Table *closures; /* handle cycles */
  int has_non_leaf, has_tl, body_size;
  int comp_flags;

  int inlining;
  Scheme_Module *module;

  Comp_Prefix *comp_prefix; /* Top-level and syntax-constant info for
                               top-level unresolved. This prefix is
                               the unresolved from of the original
                               resolved prefix.

                               When unresolving a single lambda for
                               inlining, this prefix is NULL, and
                               tenattive additions are added to
                               `new_toplevels`, instead. */

  Scheme_Hash_Table *new_toplevels; /* toplevels to add to an optimiation context */
  int new_toplevel_offset; /* the number of toplevels already registered in the
                              optimization context */
  Scheme_Object *from_modidx, *to_modidx; /* non-NULL => shift for adding to `new_toplevels` */
  intptr_t toplevel_ref_phase;
  Scheme_Env *opt_env;
  Scheme_Object *opt_insp;
  Scheme_Object *inline_variants;

  Scheme_Hash_Table *toplevels;
  Scheme_Object *definitions;
  int lift_offset, lift_to_local;
  Scheme_Hash_Table *ref_lifts;
} Unresolve_Info;

static Scheme_Object *unresolve_expr(Scheme_Object *e, Unresolve_Info *ui, int as_rator);
static Comp_Prefix *unresolve_prefix(Resolve_Prefix *rp, Unresolve_Info *ui);
static void locate_cyclic_closures(Scheme_Object *e, Unresolve_Info *ui);
static Scheme_IR_Let_Header *make_let_header(int count);
static Scheme_IR_Let_Value *make_ir_let_value(int count);

static Unresolve_Info *new_unresolve_info(Resolve_Prefix *prefix, int comp_flags)
{
  Unresolve_Info *ui;
  Scheme_IR_Local **vars;
  Scheme_Hash_Table *ht;

  ui = MALLOC_ONE_RT(Unresolve_Info);
  SET_REQUIRED_TAG(ui->type = scheme_rt_unresolve_info);

  ui->prefix = prefix;

  ui->stack_pos = 0;
  ui->stack_size = 10;
  vars = MALLOC_N(Scheme_IR_Local *, ui->stack_size);
  ui->vars = vars;

  ht = scheme_make_hash_table(SCHEME_hash_ptr);
  ui->toplevels = ht;
  ui->definitions = scheme_null;
  ht = scheme_make_hash_table(SCHEME_hash_ptr);
  ui->ref_lifts = ht;
  ht = scheme_make_hash_table(SCHEME_hash_ptr);
  ui->closures = ht;

  ui->comp_flags = comp_flags;

  return ui;
}

static int unresolve_stack_push(Unresolve_Info *ui, int n, int make_vars)
{
  int pos, i;
  Scheme_IR_Local **vars, *var;

  pos = ui->stack_pos;

  if (pos + n > ui->stack_size) {
    vars = MALLOC_N(Scheme_IR_Local *, ((2 * ui->stack_size) + n));
    memcpy(vars, ui->vars, sizeof(Scheme_IR_Local *) * pos);

    ui->vars = vars;

    ui->stack_size = (2 * ui->stack_size) + n;
  }
  if (make_vars) {
    for (i = 0; i < n; i++) {
      var = MALLOC_ONE_TAGGED(Scheme_IR_Local);
      var->so.type = scheme_ir_local_type;
      ui->vars[pos + i] = var;
    }
  } else
    memset(ui->vars + pos, 0, sizeof(Scheme_IR_Local *) * n);

  ui->stack_pos += n;
  
  LOG_UNRESOLVE(printf("push %d(%d), d=%d, sp=%d, [%d, %d, %d, %d, %d]\n", n, r_only, ui->depth, ui->stack_pos,
                       ui->depths[0], ui->depths[1], ui->depths[2], ui->depths[3], ui->depths[4]));

  return pos;
}

static Scheme_IR_Local **unresolve_stack_extract(Unresolve_Info *ui, int pos, int n)
{
  Scheme_IR_Local **vars;
  int i;

  if (!n)
    return NULL;

  vars = MALLOC_N(Scheme_IR_Local *, n);
  for (i = 0; i < n; i++) {
    vars[i] = ui->vars[ui->stack_pos - pos - 1 - i];
  }

  return vars;
}

static Scheme_IR_Local **unresolve_stack_pop(Unresolve_Info *ui, int pos, int n)
{
  Scheme_IR_Local **vars;

  MZ_ASSERT(!n || (ui->stack_pos == pos + n));

  vars = unresolve_stack_extract(ui, 0, n);
  
  ui->stack_pos = pos;

  return vars;
}

static Scheme_IR_Local *unresolve_lookup(Unresolve_Info *ui, int pos, int as_rator)
{
  Scheme_IR_Local *var = ui->vars[ui->stack_pos - pos - 1];

  if (var->use_count < SCHEME_USE_COUNT_INF)
    var->use_count++;
  if (!as_rator
      && !var->is_ref_arg
      && (var->non_app_count < SCHEME_USE_COUNT_INF))
    var->non_app_count++;

  return var;
}

static Scheme_Object *unresolve_lambda(Scheme_Lambda *rlam, Unresolve_Info *ui)
{
  Scheme_Lambda *lam;
  Scheme_Object *body;
  Scheme_IR_Lambda_Info *cl;
  int i, pos, lam_pos, init_size, has_non_leaf, has_tl;
  Scheme_IR_Local **vars;

  scheme_delay_load_closure(rlam);

  lam  = MALLOC_ONE_TAGGED(Scheme_Lambda);
  lam->iso.so.type = scheme_ir_lambda_type;

  SCHEME_LAMBDA_FLAGS(lam) = (SCHEME_LAMBDA_FLAGS(rlam) 
                              & (LAMBDA_HAS_REST | LAMBDA_IS_METHOD));


  lam->num_params = rlam->num_params;
  lam->name = rlam->name;

  pos = unresolve_stack_push(ui, lam->num_params, 1);
  vars = unresolve_stack_extract(ui, 0, lam->num_params);
    
  if (SCHEME_LAMBDA_FLAGS(rlam) & LAMBDA_HAS_TYPED_ARGS) {
    for (i = 0; i < lam->num_params; i++) {
      LOG_UNRESOLVE(printf("ref_args[%d] = %d\n", ui->stack_pos - i - 1,
                           scheme_boxmap_get(rlam->closure_map, i, rlam->closure_size)));
      if (scheme_boxmap_get(rlam->closure_map, i, rlam->closure_size) == LAMBDA_TYPE_BOXED) {
        vars[i]->is_ref_arg = 1;
      }
    }
  }

  if (rlam->closure_size) {
    lam_pos = unresolve_stack_push(ui, rlam->closure_size, 0);
    for (i = rlam->closure_size; i--; ) {
      Scheme_IR_Local *mp;
      mp = ui->vars[pos - rlam->closure_map[i] - 1];
      ui->vars[ui->stack_pos - i - 1] = mp;
    }
  } else
    lam_pos = 0;

  init_size = ui->body_size;
  has_non_leaf = ui->has_non_leaf;
  ui->has_non_leaf = 0;
  has_tl = ui->has_tl;
  ui->has_tl = 0;

  body = unresolve_expr(rlam->body, ui, 0);
  if (!body) return_NULL;

  lam->body = body;

  cl = MALLOC_ONE_RT(Scheme_IR_Lambda_Info);
  SET_REQUIRED_TAG(cl->type = scheme_rt_ir_lambda_info);
  lam->ir_info = cl;

  cl->body_size = (ui->body_size - init_size);

  cl->has_nonleaf = ui->has_non_leaf;
  ui->has_non_leaf = has_non_leaf;

  cl->has_tl = ui->has_tl;
  ui->has_tl = ui->has_tl || has_tl;

  if (rlam->closure_size)
    (void)unresolve_stack_pop(ui, lam_pos, 0);

  (void)unresolve_stack_pop(ui, pos, 0);
  cl->vars = vars;

  /* We don't need to set any more fields of cl, because
     optimize does that. */

  return (Scheme_Object *)lam;
}

static void check_nonleaf_rator(Scheme_Object *rator, Unresolve_Info *ui)
{
  if (!scheme_check_leaf_rator(rator, NULL))
    ui->has_non_leaf = 1;
}

static int unresolve_toplevel_pos(int pos, Unresolve_Info *ui)
{
  LOG_UNRESOLVE(printf("pos before = %d\n", pos));
  if (ui->prefix->num_stxes
      && (pos > (ui->prefix->num_toplevels + ui->prefix->num_stxes))) {
    /* shift lifted reference down to toplevel range */
    pos -= ui->prefix->num_stxes + 1; /* extra slot for lazy syntax */
  }
  LOG_UNRESOLVE(printf("pos = %d\n", pos));

  return pos;
}

static Scheme_Object *unresolve_toplevel(Scheme_Object *rdata, Unresolve_Info *ui)
{
  Scheme_Object *v;

  if (ui->inlining) {
    /* Create a reference that works for the optimization context. */
    int pos = SCHEME_TOPLEVEL_POS(rdata);
    if (ui->prefix->num_stxes
        && (pos > (ui->prefix->num_toplevels + ui->prefix->num_stxes))) {
      /* Cannot refer to a lift across a module boundary. */
      return_NULL;
    } else {
      Scheme_Object *hv, *modidx, *mod_constant, *sym, *npos, *shape;
      int flags, is_constant;
      int sym_pos;
      intptr_t mod_defn_phase;

      flags = SCHEME_TOPLEVEL_FLAGS(rdata) & SCHEME_TOPLEVEL_FLAGS_MASK;
      switch (flags) {
      case SCHEME_TOPLEVEL_CONST:
        is_constant = 2;
        break;
      case SCHEME_TOPLEVEL_FIXED:
        is_constant = 1;
        break;
      case SCHEME_TOPLEVEL_READY:
      default:
        /* Since we're referencing from an imported context, the
           variable is now at least ready: */
        flags = SCHEME_TOPLEVEL_READY;
        is_constant = 0;
      }

      v = ui->prefix->toplevels[pos];
      if (SCHEME_MPAIRP(v)) {
        /* Simplified version was installed by link_module_variable; original is in CDR */
        v = SCHEME_CDR(v);
      }

      if (SCHEME_SYMBOLP(v)) {
        mod_defn_phase = ui->toplevel_ref_phase;
        modidx = ui->to_modidx;
        sym_pos = -1;
        sym = v;
      } else {
        Module_Variable *mv = (Module_Variable *)v;
        MZ_ASSERT(SAME_TYPE(SCHEME_TYPE(v), scheme_module_variable_type));
        mod_defn_phase = mv->mod_phase;
        modidx = scheme_modidx_shift(mv->modidx, ui->from_modidx, ui->to_modidx);
        sym = mv->sym;
        sym_pos = mv->pos;
      }

      mod_constant = NULL;
      npos = scheme_check_accessible_in_module_name(modidx, mod_defn_phase, ui->opt_env,
                                                    sym, sym_pos,
                                                    ui->opt_insp, NULL,
                                                    &mod_constant);
      if (!npos)
        return_NULL;

      if (sym_pos < 0)
        sym_pos = SCHEME_INT_VAL(npos);

      shape = NULL;
      if (mod_constant) {
        if (SAME_TYPE(SCHEME_TYPE(mod_constant), scheme_struct_proc_shape_type))
          shape = scheme_intern_struct_proc_shape(SCHEME_PROC_SHAPE_MODE(mod_constant));
        else if (SAME_TYPE(SCHEME_TYPE(mod_constant), scheme_inline_variant_type))
          shape = scheme_get_or_check_procedure_shape(mod_constant, NULL);
      }
      
      hv = scheme_hash_module_variable(ui->opt_env, modidx,
                                       sym, ui->opt_insp,
                                       sym_pos, mod_defn_phase, is_constant,
                                       shape);

      /* Check whether this variable is already known in the optimzation context: */
      v = scheme_hash_get(ui->comp_prefix->toplevels, hv);
      if (!v) {
        /* Not already in optimization context; check/extend tentative additions */
        if (!ui->new_toplevels) {
          Scheme_Hash_Table *ht;
          ht = scheme_make_hash_table(SCHEME_hash_ptr);
          ui->new_toplevels = ht;
        }

        v = scheme_hash_get(ui->new_toplevels, hv);
        if (!v) {
          int new_pos = ui->new_toplevel_offset + ui->new_toplevels->count;
          v = scheme_make_toplevel(0, new_pos, 0, flags);
          scheme_hash_set(ui->new_toplevels, hv, v);

          if (mod_constant
              && ui->comp_prefix->inline_variants) {
            if (SAME_TYPE(SCHEME_TYPE(mod_constant), scheme_inline_variant_type)) {
              Scheme_Object *shiftable;
              shiftable = scheme_make_vector(4, scheme_false);
              SCHEME_VEC_ELS(shiftable)[0] = mod_constant;
              SCHEME_VEC_ELS(shiftable)[1] = ui->from_modidx;
              SCHEME_VEC_ELS(shiftable)[2] = ui->to_modidx;
              SCHEME_VEC_ELS(shiftable)[3] = scheme_make_integer(mod_defn_phase);
              mod_constant = shiftable;
            } else if (SAME_TYPE(SCHEME_TYPE(mod_constant), scheme_struct_proc_shape_type)) {
              /* keep it */
            } else
              mod_constant = NULL;

            if (mod_constant) {
              mod_constant = scheme_make_pair(scheme_make_pair(scheme_make_integer(new_pos),
                                                               mod_constant),
                                              ui->inline_variants);
              ui->inline_variants = mod_constant;
            }
          }
        }
      }
      MZ_ASSERT(SAME_TYPE(SCHEME_TYPE(v), scheme_ir_toplevel_type));
    }
  } else {
    /* If needed, shift top-level position to account for moving
       lifts to toplevels. */
    Scheme_Object *opos;
    int pos;

    pos = unresolve_toplevel_pos(SCHEME_TOPLEVEL_POS(rdata), ui);
    opos = scheme_make_integer(pos);
    v = scheme_hash_get(ui->toplevels, opos);
    if (!v) {
      v = scheme_make_toplevel(0,
                               pos,
                               0,
                               SCHEME_TOPLEVEL_FLAGS(rdata) & SCHEME_TOPLEVEL_FLAGS_MASK);
      scheme_hash_set(ui->toplevels, opos, v);
    }
    LOG_UNRESOLVE(printf("flags for %d: %d\n", pos, SCHEME_TOPLEVEL_FLAGS(rdata) & SCHEME_TOPLEVEL_FLAGS_MASK));
  }

  ui->has_tl = 1;
  
  return v;
}

static Scheme_Object *unresolve_apply_values(Scheme_Object *e, Unresolve_Info *ui)
{
  Scheme_Object *o, *a, *b;

  a = SCHEME_PTR1_VAL(e);
  a = unresolve_expr(a, ui, 0);
  if (!a) return_NULL;
  LOG_UNRESOLVE(printf("unresolve_apply_values: (a) %d %d\n", e->type, a->type));

  b = SCHEME_PTR2_VAL(e);
  b = unresolve_expr(b, ui, 0);
  if (!b) return_NULL;
  LOG_UNRESOLVE(printf(" (b) %d\n", b->type));

  o = scheme_alloc_object();
  o->type = SCHEME_TYPE(e);
  SCHEME_PTR1_VAL(o) = a;
  SCHEME_PTR2_VAL(o) = b;
  return o;
}

static Scheme_Object *unresolve_define_values(Scheme_Object *e, Unresolve_Info *ui)
{
  Scheme_Object *vars = scheme_null;
  Scheme_Object *vec, *val, *tl;
  int i;

  LOG_UNRESOLVE(printf("define-values-size!!!: %d\n", (int)SCHEME_VEC_SIZE(e)));
  for (i = SCHEME_VEC_SIZE(e); --i;) {
    LOG_UNRESOLVE(printf("define-values: %d\n", SCHEME_TYPE(SCHEME_VEC_ELS(e)[i])));
    tl = unresolve_toplevel(SCHEME_VEC_ELS(e)[i], ui);
    if (!tl) return_NULL; /* TODO: does this check need to be here? */
    vars = cons(tl, vars);
  }
  val = unresolve_expr(SCHEME_VEC_ELS(e)[0], ui, 0);
  if (!val) return_NULL;

  vec = scheme_make_vector(2, NULL);
  vec->type = scheme_define_values_type;
  SCHEME_VEC_ELS(vec)[0] = vars;
  SCHEME_VEC_ELS(vec)[1] = val;
  return vec;
}

static Scheme_Object *unresolve_define_or_begin_syntaxes(int def, Scheme_Object *e, Unresolve_Info *ui)
{
  Resolve_Prefix *prefix;
  Comp_Prefix *comp_prefix;
  Scheme_Object *names, *dummy, *val, *vec;
  Unresolve_Info *nui;
  int i, closures_count;

  prefix = (Resolve_Prefix *)SCHEME_VEC_ELS(e)[1];
  dummy = SCHEME_VEC_ELS(e)[3];
  val = SCHEME_VEC_ELS(e)[0];

  if (def) {
    names = scheme_null;
    for (i = SCHEME_VEC_SIZE(e); i-- > 4; ) {
      names = scheme_make_pair(SCHEME_VEC_ELS(e)[i], names);
    }
  } else
    names = NULL;

  nui = new_unresolve_info(prefix, ui->comp_flags);
  nui->lift_to_local = 1;

  dummy = unresolve_expr(dummy, ui, 0);
  comp_prefix = unresolve_prefix(prefix, nui);
  nui->comp_prefix = comp_prefix;
  
  if (def) {
    locate_cyclic_closures(val, nui);
    val = unresolve_expr(val, nui, 0);
  } else {
    for (e = val; !SCHEME_NULLP(e); e = SCHEME_CDR(e)) {
      locate_cyclic_closures(SCHEME_CAR(e), nui);
    }
    e = val;
    val = scheme_null;
    for (; !SCHEME_NULLP(e); e = SCHEME_CDR(e)) {
      val = scheme_make_pair(unresolve_expr(SCHEME_CAR(e), nui, 0),
                             val);
    }
    val = scheme_reverse(val);
  }

  vec = scheme_make_vector(4, NULL);
  vec->type = (def ? scheme_define_syntaxes_type : scheme_begin_for_syntax_type);
  SCHEME_VEC_ELS(vec)[0] = (Scheme_Object *)comp_prefix;
  SCHEME_VEC_ELS(vec)[1] = dummy;
  if (def) {
    SCHEME_VEC_ELS(vec)[2] = names;
    SCHEME_VEC_ELS(vec)[3] = val;
  } else {
    SCHEME_VEC_ELS(vec)[2] = val;
  }

  closures_count = 0;
  if (nui->closures && nui->closures->count) {
    for (i = 0; i < nui->closures->size; i++) {
      if (nui->closures->vals[i] && !SAME_OBJ(nui->closures->vals[i], scheme_true))
        closures_count++;
    }
  }

  if (closures_count) {
    Scheme_IR_Let_Header *head;
    Scheme_IR_Let_Value *irlv, *prev_irlv = NULL;
    Scheme_IR_Local **vars;

    head = make_let_header(closures_count);
    head->num_clauses = closures_count;
    SCHEME_LET_FLAGS(head) = SCHEME_LET_RECURSIVE;
    
    for (i = 0; i < nui->closures->size; i++) {
      if (nui->closures->vals[i] && !SAME_OBJ(nui->closures->vals[i], scheme_true)) {
        MZ_ASSERT(SAME_TYPE(SCHEME_TYPE(nui->closures->vals[i]), scheme_ir_local_type));
        irlv = make_ir_let_value(1);
        vars = MALLOC_N(Scheme_IR_Local *, 1);
        vars[0] = SCHEME_VAR(nui->closures->vals[i]);
        irlv->vars = vars;

        if (prev_irlv)
          prev_irlv->body = (Scheme_Object *)irlv;
        else
          head->body = (Scheme_Object *)irlv;
        prev_irlv = irlv;
      }
    }

    MZ_ASSERT(prev_irlv);
    prev_irlv->body = vec;

    return (Scheme_Object *)head;
  }


  return vec;
}

static Scheme_IR_Let_Header *make_let_header(int count)
{
  Scheme_IR_Let_Header *lh;
  lh = MALLOC_ONE_TAGGED(Scheme_IR_Let_Header);
  lh->iso.so.type = scheme_ir_let_header_type;
  lh->count = count;
  lh->num_clauses = 0;
  return lh;
}

static Scheme_IR_Let_Value *make_ir_let_value(int count)
{
  Scheme_IR_Let_Value *irlv;
  irlv = MALLOC_ONE_TAGGED(Scheme_IR_Let_Value);
  irlv->iso.so.type = scheme_ir_let_value_type;
  irlv->count = count;
  return irlv;
}

typedef struct Unresolve_Let_Void_State {
  /* All pointers so we can use scheme_malloc */
  Scheme_IR_Let_Header *prev_head;
  Scheme_IR_Let_Value *prev_let;
  Scheme_Sequence *prev_seq;
} Unresolve_Let_Void_State;

/* only one of lh, irlv, seq, or body should be non-NULL */
static void attach_lv(Scheme_IR_Let_Header *lh, 
                      Scheme_IR_Let_Value *irlv, 
                      Scheme_Sequence *seq,
                      Scheme_Object *body,
                      Unresolve_Let_Void_State *state)
{
  Scheme_Object *o;
  o = lh ? (Scheme_Object *)lh : 
    (irlv ? (Scheme_Object *)irlv :
    (seq ? (Scheme_Object *)seq : body));
  
  if (state->prev_head) {
    state->prev_head->body = o;
  } else if (state->prev_let) {
    state->prev_let->body = o;
  } else if (state->prev_seq) {
    state->prev_seq->array[state->prev_seq->count - 1] = o;
  }

  state->prev_head = lh;
  state->prev_let = irlv;
  state->prev_seq = seq; 
}

static Scheme_Object *push_to_rhs_sequence(Scheme_Object *push_rhs, Scheme_Object *val)
/* move accumulated forms to the next discovered right-hand side for a binding sequence */
{
  int len, i;
  Scheme_Sequence *seq;
  
  len = scheme_list_length(push_rhs);
  seq = scheme_malloc_sequence(len+1);
  seq->so.type = scheme_sequence_type;
  seq->count = len+1;
  seq->array[len] = val;

  for (i = len; i--; ) {
    seq->array[i] = SCHEME_CAR(push_rhs);
    push_rhs = SCHEME_CDR(push_rhs);
  }

  return (Scheme_Object *)seq;
}

static Scheme_Object *unresolve_let_void(Scheme_Object *e, Unresolve_Info *ui)
{
  Scheme_Let_Void *lv = (Scheme_Let_Void *)e;
  int i, pos, count;
  Scheme_IR_Local **vars;
  Scheme_IR_Let_Header *lh;
  Scheme_Object *o, *push_rhs = scheme_null;
  Unresolve_Let_Void_State *state;

  state = scheme_malloc(sizeof(Unresolve_Let_Void_State));

  count = lv->count;
  pos = unresolve_stack_push(ui, count, 1);
  lh = make_let_header(count);

  o = lv->body;
  attach_lv(lh, NULL, NULL, NULL, state);
  for (i = 0; i < count;) {
    switch (SCHEME_TYPE(o)) {
    case scheme_let_value_type: {   
      Scheme_Let_Value *lval = (Scheme_Let_Value *)o;
      Scheme_IR_Let_Value *irlv;
      Scheme_Object *val;
      irlv = make_ir_let_value(lval->count);
      lh->num_clauses++;
      
      vars = unresolve_stack_extract(ui, lval->position, lv->count);
      irlv->vars = vars;

      if (SCHEME_LET_VALUE_AUTOBOX(lval)) {
        SCHEME_LET_FLAGS(lh) = SCHEME_LET_RECURSIVE;
      }

      val = unresolve_expr(lval->value, ui, 0);
      if (!val) return_NULL;
      if (!SCHEME_NULLP(push_rhs)) {
        val = push_to_rhs_sequence(push_rhs, val);
        push_rhs = scheme_null;
      }
      irlv->value = val;

      o = lval->body;
      attach_lv(NULL, irlv, NULL, NULL, state);
      i += lval->count;
     
      break;
    }
    case scheme_boxenv_type: {
      o = SCHEME_PTR2_VAL(o);
      break;
    }
    case scheme_letrec_type: {
      Scheme_Letrec *lr = (Scheme_Letrec *)o;
      int j;
      SCHEME_LET_FLAGS(lh) = SCHEME_LET_RECURSIVE;
      for (j = 0; j < lr->count; j++) {
	Scheme_IR_Let_Value *irlv;
	Scheme_Object *val;
        Scheme_IR_Local **vars;
	irlv = make_ir_let_value(1);
	lh->num_clauses++;
        vars = unresolve_stack_extract(ui, j, 1);
	val = unresolve_expr(lr->procs[j], ui, 0);
	if (!val) return_NULL;
        if (!SCHEME_NULLP(push_rhs)) {
          val = push_to_rhs_sequence(push_rhs, val);
          push_rhs = scheme_null;
        }
	irlv->value = val;
        irlv->vars = vars;
        attach_lv(NULL, irlv, NULL, NULL, state);
	i++;
      }
      o = lr->body;
      break;
    }
    case scheme_sequence_type: {
      Scheme_Sequence *seq = (Scheme_Sequence *)o;
      int i;
      for (i = 0; i < seq->count - 1; i++) {
        if (!SAME_TYPE(SCHEME_TYPE(seq->array[i]), scheme_local_type)) {
          push_rhs = scheme_make_pair(unresolve_expr(seq->array[i], ui, 0), push_rhs);
        }
      }
      o = seq->array[seq->count - 1];
      break;
    }
    default: {
      scheme_signal_error("internal error: unexpected form in let-void: %d", SCHEME_TYPE(o));
    }
    }
  }
  
  o = unresolve_expr(o, ui, 0);
  if (!o) return_NULL;
  attach_lv(NULL, NULL, NULL, o, state);

  (void)unresolve_stack_pop(ui, pos, 0);

  return (Scheme_Object *)lh;
}

static Scheme_Object *unresolve_prefix_symbol(Scheme_Object *s, Unresolve_Info *ui)
{
  if (!ui->module) {
    return s;
  } else {
    Module_Variable *mv;

    mv = MALLOC_ONE_TAGGED(Module_Variable);
    mv->iso.so.type = scheme_module_variable_type;

    mv->modidx = ui->module->self_modidx;
    mv->sym = s;
    mv->insp = ui->module->insp;
    mv->pos = -1;
    mv->mod_phase = 0;
    SCHEME_MODVAR_FLAGS(mv) |= SCHEME_MODVAR_FIXED;
    return (Scheme_Object *)mv;
  }
}

static Scheme_Object *unresolve_closure(Scheme_Object *e, Unresolve_Info *ui)
{
  Scheme_Object *r, *c;

  if (ui->closures)
    c = scheme_hash_get(ui->closures, e);
  else
    c = NULL;
      
  if (ui->inlining) {
    /* can't handle cyclic closures */
    if (c) return_NULL;
    if (!ui->closures) {
      Scheme_Hash_Table *ht;
      ht = scheme_make_hash_table(SCHEME_hash_ptr);
      ui->closures = ht;
    }
    scheme_hash_set(ui->closures, e, scheme_true);
  } else  {  
    if (c && SAME_TYPE(SCHEME_TYPE(c), scheme_ir_toplevel_type))
      return c;
  }
  
  r = unresolve_lambda(SCHEME_CLOSURE_CODE(e), ui);

  if (ui->inlining)
    scheme_hash_set(ui->closures, e, NULL);

  return r;
}

static Comp_Prefix *unresolve_prefix(Resolve_Prefix *rp, Unresolve_Info *ui)
{
  Comp_Prefix *cp;
  Scheme_Object *o;
  int i;
  cp = MALLOC_ONE_TAGGED(Comp_Prefix);
  SET_REQUIRED_TAG(cp->type = scheme_rt_comp_prefix);
  cp->num_toplevels = 0;
  cp->toplevels = NULL;
  ui->lift_offset = rp->num_toplevels;
  for (i = 0; i < rp->num_toplevels; i++) {
    if (SCHEME_SYMBOLP(rp->toplevels[i])) {
      Scheme_Object *mv;
      mv = unresolve_prefix_symbol(rp->toplevels[i], ui);
      o = scheme_register_toplevel_in_comp_prefix(mv, cp, 0, NULL);
    } else {
      o = scheme_register_toplevel_in_comp_prefix(rp->toplevels[i], cp, ui->module ? 1 : 0, NULL); 
    }
    scheme_hash_set(ui->toplevels, scheme_make_integer(SCHEME_TOPLEVEL_POS(o)), o);
  }
  for (i = 0; i < rp->num_lifts; i++) {
    Scheme_Object *mv, *sym;
    sym = scheme_make_symbol("lift");
    sym = scheme_gensym(sym);
    mv = unresolve_prefix_symbol(sym, ui);
    o = scheme_register_toplevel_in_comp_prefix(mv, cp, 0, NULL);
    scheme_hash_set(ui->toplevels, scheme_make_integer(SCHEME_TOPLEVEL_POS(o)), o);
  }
  cp->stxes = NULL;
  for (i = 0; i < rp->num_stxes; i++) {
    if (rp->stxes[i]) {
      scheme_register_stx_in_comp_prefix(rp->stxes[i], cp);
    } else {
      cp->num_stxes++;
    }
  }
  cp->inline_variants = NULL;
  cp->unbound = NULL;
  return cp;
}

void locate_cyclic_closures(Scheme_Object *e, Unresolve_Info *ui)
{
  switch(SCHEME_TYPE(e)) {
    case scheme_sequence_type:
    case scheme_begin0_sequence_type:
    case scheme_splice_sequence_type:
      {
        Scheme_Sequence *seq = (Scheme_Sequence *)e;
		int i;
        for (i = 0; i < seq->count; i++) {
          locate_cyclic_closures(seq->array[i], ui);
        }
      }
      break;
    case scheme_application_type:
      {
        Scheme_App_Rec *app = (Scheme_App_Rec *)e;
		int i;
        for (i = 0; i < app->num_args + 1; i++) {
          locate_cyclic_closures(app->args[i], ui);
        }
      }
      break;
    case scheme_application2_type:
      {
        Scheme_App2_Rec *app = (Scheme_App2_Rec *)e;
        locate_cyclic_closures(app->rator, ui);
        locate_cyclic_closures(app->rand, ui);
      }
      break;
    case scheme_application3_type:
      {
        Scheme_App3_Rec *app = (Scheme_App3_Rec *)e;
        locate_cyclic_closures(app->rator, ui);
        locate_cyclic_closures(app->rand1, ui);
        locate_cyclic_closures(app->rand2, ui);
      }
      break;
    case scheme_branch_type:
      {
        Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)e;
        locate_cyclic_closures(b->test, ui);
        locate_cyclic_closures(b->tbranch, ui);
        locate_cyclic_closures(b->fbranch, ui);
      }
      break;
    case scheme_with_cont_mark_type:
    case scheme_with_immed_mark_type:
      {
        Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)e;
        locate_cyclic_closures(wcm->key, ui);
        locate_cyclic_closures(wcm->val, ui);
        locate_cyclic_closures(wcm->body, ui);
      }
      break;
    case scheme_let_void_type:
      {
        Scheme_Let_Void *lv = (Scheme_Let_Void *)e;
        locate_cyclic_closures(lv->body, ui);
      }
      break;
    case scheme_letrec_type:
      {
        Scheme_Letrec *lr = (Scheme_Letrec *)e;
		int i;
        for (i = 0; i < lr->count; i++) {
          locate_cyclic_closures(lr->procs[i], ui);
        }
        locate_cyclic_closures(lr->body, ui);
      }
      break;
    case scheme_let_one_type:
      {
        Scheme_Let_One *lo = (Scheme_Let_One *)e;
        locate_cyclic_closures(lo->value, ui);
        locate_cyclic_closures(lo->body, ui);
      }
      break;
    case scheme_closure_type:
      {
        Scheme_Object *c;
        c = scheme_hash_get(ui->closures, e);

        if (SAME_OBJ(c, scheme_true)) {
          Scheme_Object *s, *mv, *tl;
          s = scheme_make_symbol("cyclic");
          s = scheme_gensym(s);
          if (!ui->lift_to_local) {
            mv = unresolve_prefix_symbol(s, ui);
            tl = scheme_register_toplevel_in_comp_prefix(mv, ui->comp_prefix, 0, NULL);
          } else {
            Scheme_IR_Local *var;
            abort();
            var = MALLOC_ONE_TAGGED(Scheme_IR_Local);
            var->so.type = scheme_ir_local_type;
            var->name = s;
            tl = (Scheme_Object *)var;
          }
          scheme_hash_set(ui->closures, e, tl);
        } else if (c) {
          /* do nothing */
        } else {
          Scheme_Closure *cl = (Scheme_Closure *)e;
          scheme_hash_set(ui->closures, e, scheme_true);
          locate_cyclic_closures((Scheme_Object *)cl->code, ui);
        }
      }
      break;
    case scheme_lambda_type:
      {
        Scheme_Lambda *cd = (Scheme_Lambda *)e;
        locate_cyclic_closures(cd->body, ui);
      }
      break;
    case scheme_inline_variant_type:
      {
        Scheme_Object *a;
        a = SCHEME_VEC_ELS(e)[0];
        locate_cyclic_closures(a, ui);
      }
      break;
    case scheme_define_values_type:
      {
        if (SCHEME_VEC_SIZE(e) == 2) {
          int pos = SCHEME_TOPLEVEL_POS(SCHEME_VEC_ELS(e)[1]);
          if (pos >= ui->lift_offset) {
            Scheme_Lambda *lam = (Scheme_Lambda *)SCHEME_VEC_ELS(e)[0];
            if (SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_HAS_TYPED_ARGS) {
              scheme_hash_set(ui->ref_lifts, scheme_make_integer(pos), (Scheme_Object *)lam); 
            }
          }
        }

        locate_cyclic_closures(SCHEME_VEC_ELS(e)[0], ui);
      }
      break;
    case scheme_set_bang_type:
      {
        Scheme_Set_Bang *sb = (Scheme_Set_Bang *)e;
        locate_cyclic_closures(sb->var, ui);
        locate_cyclic_closures(sb->val, ui);
      }
      break;
    case scheme_varref_form_type:
    case scheme_apply_values_type:
      {
        Scheme_Object *a, *b;
        a = SCHEME_PTR1_VAL(e);
        locate_cyclic_closures(a, ui);
        b = SCHEME_PTR2_VAL(e);
        locate_cyclic_closures(b, ui);
      }
      break;
    case scheme_boxenv_type:
      {
        locate_cyclic_closures(SCHEME_PTR2_VAL(e), ui);
      }
      break;
    case scheme_case_lambda_sequence_type:
      {
        Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)e;
		int i;
        for (i = 0; i < cl->count; i++) {
          locate_cyclic_closures(cl->array[i], ui);
        }
      }
      break;
    case scheme_let_value_type:
      {
        Scheme_Let_Value *lv = (Scheme_Let_Value *)e;
        locate_cyclic_closures(lv->value, ui);
        locate_cyclic_closures(lv->body, ui);
      }
      break;
    default:
      break;
  }
}

static void convert_closures_to_definitions(Unresolve_Info *ui)
{
  Scheme_Object *d, *vars, *val;
  Scheme_Lambda *lam;
  int i;
  
  for (i = 0; i < ui->closures->size; i++) {
    if (ui->closures->vals[i] && !SAME_OBJ(ui->closures->vals[i], scheme_true)) {
      MZ_ASSERT(SAME_TYPE(SCHEME_TYPE(ui->closures->vals[i]), scheme_ir_toplevel_type));
      d = scheme_make_vector(2, NULL);
      d->type = scheme_define_values_type;
      vars = cons(ui->closures->vals[i], scheme_null);
      lam = SCHEME_CLOSURE_CODE(ui->closures->keys[i]);
      val = unresolve_lambda(lam, ui);
      SCHEME_VEC_ELS(d)[0] = vars;
      SCHEME_VEC_ELS(d)[1] = val;
      d = cons(d, ui->definitions);
      ui->definitions = d;
    }
  }
}

Scheme_Object *unresolve_module(Scheme_Object *e, Unresolve_Info *ui_in)
{
  Scheme_Module *m = (Scheme_Module *)e, *nm;
  Scheme_Object *dummy, *bs, *bs2, *ds, **bss;
  Comp_Prefix *cp;
  Unresolve_Info *ui;
  int i, cnt, len;

  ui = new_unresolve_info(m->prefix, ui_in->comp_flags);

  ui->module = m;
  cp = unresolve_prefix(m->prefix, ui);
  if (!cp) return_NULL;
  ui->comp_prefix = cp;

  cnt = SCHEME_VEC_SIZE(m->bodies[0]);
  bs = scheme_make_vector(cnt, NULL);

  for (i = 0; i < cnt; i++) {
    locate_cyclic_closures(SCHEME_VEC_ELS(m->bodies[0])[i], ui);
  }

  convert_closures_to_definitions(ui);

  for (i = 0; i < cnt; i++) {
    Scheme_Object *b;
    b = unresolve_expr(SCHEME_VEC_ELS(m->bodies[0])[i], ui, 0);
    if (!b) return_NULL;
    SCHEME_VEC_ELS(bs)[i] = b;
  }
  len = scheme_list_length(ui->definitions);
  ds = ui->definitions;
  bs2 = scheme_make_vector(cnt + len, NULL);
  for (i = 0; SCHEME_PAIRP(ds); ds = SCHEME_CDR(ds), i++) {
    SCHEME_VEC_ELS(bs2)[i] = SCHEME_CAR(ds);
  }
  for (i = 0; i < cnt; i++) {
    SCHEME_VEC_ELS(bs2)[i + len] = SCHEME_VEC_ELS(bs)[i];
  }

  dummy = unresolve_expr(m->dummy, ui_in, 0);

  nm = MALLOC_ONE_TAGGED(Scheme_Module);
  nm->so.type = scheme_module_type;
  nm->predefined = m->predefined;

  nm->modname = m->modname;
  nm->modsrc = m->modsrc;

  nm->et_requires = m->et_requires;
  nm->requires = m->requires;
  nm->tt_requires = m->tt_requires;
  nm->dt_requires = m->dt_requires;
  nm->other_requires = m->other_requires;

  bss = MALLOC_N(Scheme_Object*, m->num_phases); 
  nm->bodies = bss;
  nm->bodies[0] = bs2;
  /* Other phases are left as-is (and resolve doesn't traverse them): */
  for (i = 1; i < m->num_phases; i++) {
    nm->bodies[i] = m->bodies[i];
  }

  nm->me = m->me;

  nm->num_phases = m->num_phases;

  nm->exp_infos = m->exp_infos;

  nm->self_modidx = m->self_modidx;
  nm->insp = m->prefix->src_insp_desc;

  nm->lang_info = m->lang_info;

  nm->comp_prefix = cp;
  nm->max_let_depth = 0;
  nm->prefix = NULL;
  nm->dummy = dummy;
  nm->rn_stx = m->rn_stx;

  nm->phaseless = m->phaseless;

  nm->binding_names  = m->binding_names;
  nm->et_binding_names  = m->et_binding_names;
  nm->other_binding_names  = m->other_binding_names;

  /* leave submodules alone (and resolve doesn't traverse them): */
  nm->submodule_path = m->submodule_path;
  nm->pre_submodules = m->pre_submodules;
  nm->post_submodules = m->post_submodules;
  nm->pre_submodule_names = m->pre_submodule_names;
  nm->submodule_ancestry = m->submodule_ancestry;
  /* the `supermodule` field is only for instantiated modules */

  return (Scheme_Object *)nm;
}

static Scheme_Object *unresolve_let_value(Scheme_Let_Value *lv, Unresolve_Info *ui,
                                          Scheme_Object* val, Scheme_Object *body) {
  Scheme_Set_Bang *sb;
  Scheme_IR_Local *var;
  Scheme_Sequence *seq;
  
  LOG_UNRESOLVE(printf("set! position: %d (stack pos %d)\n", lv->position, ui->stack_pos));

  if (!lv->count) {
    /* Not a set! case; just make sure the expression produces 0 arguments */
    Scheme_IR_Let_Header *head;
    Scheme_IR_Let_Value *irlv;
 
    head = make_let_header(0);
    head->num_clauses = 1;
    irlv = make_ir_let_value(0);
    head->body = (Scheme_Object *)irlv;
    irlv->value = val;
    irlv->body = body;

    return (Scheme_Object *)head;
  }
  
  var = unresolve_lookup(ui, lv->position, 0);

  if (var->is_ref_arg) {
    Scheme_App2_Rec *app2;
    app2 = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
    app2->iso.so.type = scheme_application2_type;
    app2->rator = (Scheme_Object *)var;
    app2->rand = val;
    seq = scheme_malloc_sequence(2);
    seq->so.type = scheme_sequence_type;
    seq->count = 2;
    seq->array[0] = (Scheme_Object *)app2;
    seq->array[1] = body;
    return (Scheme_Object *)seq;
  }

  var->mutated = 1;

  sb = MALLOC_ONE_TAGGED(Scheme_Set_Bang);
  sb->so.type = scheme_set_bang_type;
  sb->var = (Scheme_Object *)var;
  sb->val = val;

  seq = scheme_malloc_sequence(2);
  seq->so.type = scheme_sequence_type;
  seq->count = 2;
  seq->array[0] = (Scheme_Object *)sb;
  seq->array[1] = body;

  return (Scheme_Object *)seq;
}

static Scheme_Object *maybe_unresolve_app_refs(Scheme_Object *rator,
                                               Scheme_App_Rec *app,
                                               Scheme_App2_Rec *app2,
                                               Scheme_App3_Rec *app3,
                                               Unresolve_Info *ui)
{
  Scheme_Lambda *lam = NULL;

  if (SAME_TYPE(SCHEME_TYPE(rator), scheme_closure_type)
      && (SCHEME_LAMBDA_FLAGS((SCHEME_CLOSURE_CODE(rator))) & LAMBDA_HAS_TYPED_ARGS)) {
    lam = SCHEME_CLOSURE_CODE(rator);
  } else if (SAME_TYPE(SCHEME_TYPE(rator), scheme_toplevel_type)) {
    lam = (Scheme_Lambda *)scheme_hash_get(ui->ref_lifts, scheme_make_integer(SCHEME_TOPLEVEL_POS(rator)));
  }

  if (lam) {
    Scheme_App_Rec *new_app = NULL;
    Scheme_App2_Rec *new_app2 = NULL;
    Scheme_App3_Rec *new_app3 = NULL;
    Scheme_Object *arg;
    Scheme_Object *new_rator;
    int i;

    if (app) {
      if (lam->num_params != app->num_args)
        return NULL;
      new_app = scheme_malloc_application(app->num_args + 1);
    } else if (app2) {
      if (lam->num_params != 1)
        return NULL;
      new_app2 = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
      new_app2->iso.so.type = scheme_application2_type;
    } else {
      if (lam->num_params != 2)
        return NULL;
      new_app3 = MALLOC_ONE_TAGGED(Scheme_App3_Rec);
      new_app3->iso.so.type = scheme_application3_type;
    }

    LOG_UNRESOLVE(printf("REF app\n"));
    for(i = 0; i < lam->num_params; i++) {
      if (app)
        arg = app->args[i + 1];
      else if (app2)
        arg = app2->rand;
      else if (i)
        arg = app3->rand2;
      else
        arg = app3->rand1;
      LOG_UNRESOLVE(printf("%d: %d\n", i, scheme_boxmap_get(lam->closure_map, i, lam->closure_size)));
      LOG_UNRESOLVE(printf("ui->stack_pos = %d, argpos = %d, i = %d\n", ui->stack_pos, SCHEME_LOCAL_POS(arg), i));
      if ((scheme_boxmap_get(lam->closure_map, i, lam->closure_size) == LAMBDA_TYPE_BOXED)
          && SAME_TYPE(SCHEME_TYPE(arg), scheme_local_type)
          && !ui->vars[ui->stack_pos - SCHEME_LOCAL_POS(arg) - 1]->is_ref_arg) {
        Scheme_Case_Lambda *cl;
        Scheme_Lambda *d0, *d1;
        Scheme_Set_Bang *sb;
        Scheme_Object *s;
        Scheme_IR_Local *arg_var;
        int pos;
        Scheme_IR_Local **vars;
        Scheme_IR_Lambda_Info *ci;
        LOG_UNRESOLVE(printf("This will be a case-lambda: %d\n", i));

        cl = (Scheme_Case_Lambda *)scheme_malloc_tagged(sizeof(Scheme_Case_Lambda) 
                                                        + ((2 - mzFLEX_DELTA) * sizeof(Scheme_Object *)));

        cl->so.type = scheme_case_lambda_sequence_type;
        cl->count = 2;
        s = scheme_make_symbol("cl");
        s = scheme_gensym(s);
        cl->name = s;

        arg_var = unresolve_lookup(ui, SCHEME_LOCAL_POS(arg), 0);
        arg_var->mutated = 1;
      
        d0 = MALLOC_ONE_TAGGED(Scheme_Lambda);
        d0->iso.so.type = scheme_ir_lambda_type;
        d0->num_params = 0;
        d0->body = (Scheme_Object *)arg_var;
        ci = MALLOC_ONE_RT(Scheme_IR_Lambda_Info);
        SET_REQUIRED_TAG(ci->type = scheme_rt_ir_lambda_info);
        d0->ir_info = ci;
        s = scheme_make_symbol("d0");
        s = scheme_gensym(s);
        d0->name = s;
        cl->array[0] = (Scheme_Object *)d0;

        pos = unresolve_stack_push(ui, 1, 1);
        vars = unresolve_stack_pop(ui, pos, 1);

        d1 = MALLOC_ONE_TAGGED(Scheme_Lambda);
        d1->iso.so.type = scheme_ir_lambda_type;
        d1->num_params = 1;

        sb = MALLOC_ONE_TAGGED(Scheme_Set_Bang);
        sb->so.type = scheme_set_bang_type;
        sb->var = (Scheme_Object *)arg_var;
        sb->val = (Scheme_Object *)vars[0];
        d1->body = (Scheme_Object *)sb;
        ci = MALLOC_ONE_RT(Scheme_IR_Lambda_Info);
        SET_REQUIRED_TAG(ci->type = scheme_rt_ir_lambda_info);
        ci->vars = vars;
        vars[0]->use_count = 1;
        vars[0]->non_app_count = 1;
        d1->ir_info = ci;
        

        s = scheme_make_symbol("d1");
        s = scheme_gensym(s);
        d1->name = s;
        cl->array[1] = (Scheme_Object *)d1;

        arg = (Scheme_Object *)cl;
      } else {
        arg = unresolve_expr(arg, ui, 0);
      }

      if (new_app)
        new_app->args[i + 1] = arg;
      else if (new_app2)
        new_app2->rand = arg;
      else if (i)
        new_app3->rand2 = arg;
      else
        new_app3->rand1 = arg;
    }
    new_rator = unresolve_expr(rator, ui, 0);

    if (new_app) {
      new_app->args[0] = new_rator;
      return (Scheme_Object *)new_app;
    } else if (new_app2) {
      new_app2->rator = new_rator;
      return (Scheme_Object *)new_app2;
    } else {
      new_app3->rator = new_rator;
      return (Scheme_Object *)new_app3;
    }
  }

  return NULL;
}

static Scheme_Object *unresolve_expr_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *e = (Scheme_Object *)p->ku.k.p1;
  Unresolve_Info *ui = (Unresolve_Info *)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return unresolve_expr(e, ui, p->ku.k.i1);
}

static Scheme_Object *unresolve_expr(Scheme_Object *e, Unresolve_Info *ui, int as_rator)
{
#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;

      p->ku.k.p1 = (void *)e;
      p->ku.k.p2 = (void *)ui;
      p->ku.k.i1 = as_rator;

      return scheme_handle_stack_overflow(unresolve_expr_k);
    }
  }
#endif

  ui->body_size++;

  switch (SCHEME_TYPE(e)) {
  case scheme_local_type:
    return (Scheme_Object *)unresolve_lookup(ui, SCHEME_LOCAL_POS(e), as_rator);
  case scheme_local_unbox_type:
    {
      Scheme_IR_Local *var;
      var = unresolve_lookup(ui, SCHEME_LOCAL_POS(e), as_rator);
      if (var->is_ref_arg) {
        Scheme_App_Rec *app;
        LOG_UNRESOLVE(printf("local unbox: %d (stack pos %d)\n", SCHEME_LOCAL_POS(e), ui->stack_pos));
        app = scheme_malloc_application(1);
        app->args[0] = (Scheme_Object *)var;
        return (Scheme_Object *)app;
      }
      return (Scheme_Object *)var;
    }
  case scheme_sequence_type:
  case scheme_begin0_sequence_type:
  case scheme_splice_sequence_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)e, *seq2;
      int i;

      seq2 = scheme_malloc_sequence(seq->count);
      seq2->so.type = seq->so.type;
      seq2->count = seq->count;
      for (i = seq->count; i--; ) {
        e = unresolve_expr(seq->array[i], ui, 0);
        if (!e) return_NULL;
        seq2->array[i] = e;
      }

      return (Scheme_Object *)seq2;
    }
    break;
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)e, *app2;
      Scheme_Object *a;
      int pos, i;

      ui->body_size += app->num_args;
      check_nonleaf_rator(app->args[0], ui);

      pos = unresolve_stack_push(ui, app->num_args, 0);

      e = maybe_unresolve_app_refs(app->args[0], app, NULL, NULL, ui);
      if (e) {
        (void)unresolve_stack_pop(ui, pos, 0);
        return e;
      }

      app2 = scheme_malloc_application(app->num_args+1);

      for (i = app->num_args + 1; i--; ) {
        a = unresolve_expr(app->args[i], ui, !i);
        if (!a) return_NULL;
        app2->args[i] = a;
      }

      (void)unresolve_stack_pop(ui, pos, 0);

      return (Scheme_Object *)app2;
    }
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)e, *app2;
      Scheme_Object *rator, *rand;
      int pos;

      ui->body_size += 1;
      check_nonleaf_rator(app->rator, ui);

      pos = unresolve_stack_push(ui, 1, 0);

      e = maybe_unresolve_app_refs(app->rator, NULL, app, NULL, ui);
      if (e) {
        (void)unresolve_stack_pop(ui, pos, 0);
        return e;
      }
      
      rator = unresolve_expr(app->rator, ui, 1);
      if (!rator) return_NULL;
      rand = unresolve_expr(app->rand, ui, 0);
      if (!rand) return_NULL;

      (void)unresolve_stack_pop(ui, pos, 0);

      app2 = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
      app2->iso.so.type = scheme_application2_type;
      app2->rator = rator;
      app2->rand = rand;

      return (Scheme_Object *)app2;
    }
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)e, *app2;
      Scheme_Object *rator, *rand1, *rand2;
      int pos;

      ui->body_size += 2;
      check_nonleaf_rator(app->rator, ui);

      pos = unresolve_stack_push(ui, 2, 0);

      e = maybe_unresolve_app_refs(app->rator, NULL, NULL, app, ui);
      if (e) {
        (void)unresolve_stack_pop(ui, pos, 0);
        return e;
      }

      rator = unresolve_expr(app->rator, ui, 1);
      if (!rator) return_NULL;
      rand1 = unresolve_expr(app->rand1, ui, 0);
      if (!rand1) return_NULL;
      rand2 = unresolve_expr(app->rand2, ui, 0);
      if (!rand2) return_NULL;

      (void)unresolve_stack_pop(ui, pos, 0);

      app2 = MALLOC_ONE_TAGGED(Scheme_App3_Rec);
      app2->iso.so.type = scheme_application3_type;
      app2->rator = rator;
      app2->rand1 = rand1;
      app2->rand2 = rand2;

      return (Scheme_Object *)app2;
    }
  case scheme_branch_type:
    {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)e, *b2;
      Scheme_Object *tst, *thn, *els;

      tst = unresolve_expr(b->test, ui, 0);
      if (!tst) return_NULL;
      thn = unresolve_expr(b->tbranch, ui, 0);
      if (!thn) return_NULL;
      els = unresolve_expr(b->fbranch, ui, 0);
      if (!els) return_NULL;
      
      b2 = MALLOC_ONE_TAGGED(Scheme_Branch_Rec);
      b2->so.type = scheme_branch_type;
      b2->test = tst;
      b2->tbranch = thn;
      b2->fbranch = els;

      return (Scheme_Object *)b2;
    }
  case scheme_with_cont_mark_type:
    {
      Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)e, *wcm2;
      Scheme_Object *k, *v, *b;

      k = unresolve_expr(wcm->key, ui, 0);
      if (!k) return_NULL;
      v = unresolve_expr(wcm->val, ui, 0);
      if (!v) return_NULL;
      b = unresolve_expr(wcm->body, ui, 0);
      if (!b) return_NULL;
      
      wcm2 = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
      wcm2->so.type = scheme_with_cont_mark_type;
      wcm2->key = k;
      wcm2->val = v;
      wcm2->body = b;

      return (Scheme_Object *)wcm2;
    }
  case scheme_with_immed_mark_type:
    {
      Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)e, *wcm2;
      Scheme_Object *k, *v, *b;
      Scheme_IR_Local **vars;
      int pos;

      k = unresolve_expr(wcm->key, ui, 0);
      if (!k) return_NULL;
      v = unresolve_expr(wcm->val, ui, 0);
      if (!v) return_NULL;

      pos = unresolve_stack_push(ui, 1, 1);
      vars = unresolve_stack_extract(ui, 0, 1);
      b = unresolve_expr(wcm->body, ui, 0);
      if (!b) return_NULL;
      (void)unresolve_stack_pop(ui, pos, 0);

      wcm2 = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
      wcm2->so.type = scheme_with_immed_mark_type;
      wcm2->key = k;
      wcm2->val = v;
      b = scheme_make_raw_pair((Scheme_Object *)vars[0], b);
      wcm2->body = b;

      return (Scheme_Object *)wcm2;
    }
  case scheme_let_void_type:
    {
      return unresolve_let_void(e, ui);
    }
  case scheme_let_one_type:
    {
      Scheme_Let_One *lo = (Scheme_Let_One *)e;
      Scheme_Object *rhs, *body;
      Scheme_IR_Let_Header *lh;
      Scheme_IR_Let_Value *irlv;
      Scheme_IR_Local **vars;
      int pos;

      pos = unresolve_stack_push(ui, 1, 1);
      rhs = unresolve_expr(lo->value, ui, 0);
      if (!rhs) return_NULL;

      body = unresolve_expr(lo->body, ui, 0);
      if (!body) return_NULL;

      vars = unresolve_stack_pop(ui, pos, 1);

      lh = MALLOC_ONE_TAGGED(Scheme_IR_Let_Header);
      lh->iso.so.type = scheme_ir_let_header_type;
      lh->count = 1;
      lh->num_clauses = 1;

      irlv = MALLOC_ONE_TAGGED(Scheme_IR_Let_Value);
      irlv->iso.so.type = scheme_ir_let_value_type;
      irlv->count = 1;
      irlv->value = rhs;
      irlv->vars = vars;
      irlv->body = body;

      lh->body = (Scheme_Object *)irlv;

      return (Scheme_Object *)lh;
    }
  case scheme_closure_type:
    {
      return unresolve_closure(e, ui);
    }
  case scheme_lambda_type:
    {
      return unresolve_lambda((Scheme_Lambda *)e, ui);
    }
  case scheme_inline_variant_type:
    {
      Scheme_Object *a;
      a = SCHEME_VEC_ELS(e)[0];
      a = unresolve_expr(a, ui, 0);
      if (!a) return_NULL;
      return a;
    }
  case scheme_module_type:
    {
      return unresolve_module(e, ui);
    }
  case scheme_define_values_type:
    {
      return unresolve_define_values(e, ui);
    }
  case scheme_define_syntaxes_type:
    {
      return unresolve_define_or_begin_syntaxes(1, e, ui);
    }
  case scheme_begin_for_syntax_type:
    {
      return unresolve_define_or_begin_syntaxes(0, e, ui);
    }
  case scheme_set_bang_type:
    {
      Scheme_Set_Bang *sb = (Scheme_Set_Bang *)e, *sb2;
      Scheme_Object *var, *val;
      var = unresolve_expr(sb->var, ui, 0);
      if (!var) return_NULL;
      if (SAME_TYPE(SCHEME_TYPE(var), scheme_ir_toplevel_type)) {
        if (ui->module)
          SCHEME_TOPLEVEL_FLAGS(var) |= SCHEME_TOPLEVEL_MUTATED;
      }
      val = unresolve_expr(sb->val, ui, 0);
      if (!val) return_NULL;
      
      LOG_UNRESOLVE(printf("SET BANG: %d, %d\n", SCHEME_TYPE(val), SCHEME_TYPE(var)));

      sb2 = MALLOC_ONE_TAGGED(Scheme_Set_Bang);
      sb2->so.type = scheme_set_bang_type;
      sb2->var = var;
      sb2->val = val;
      sb2->set_undef = (ui->comp_flags & COMP_ALLOW_SET_UNDEFINED);
      return (Scheme_Object *)sb2;
    }
  case scheme_varref_form_type:
    {
      Scheme_Object *a, *b, *o;
      a = SCHEME_PTR1_VAL(e);
      a = unresolve_expr(a, ui, 0);
      if (!a) return_NULL;
      LOG_UNRESOLVE(printf("unresolve_varref: (a) %d %d\n", e->type, a->type));

      if (SAME_TYPE(SCHEME_TYPE(a), scheme_ir_toplevel_type)) {
        SCHEME_TOPLEVEL_FLAGS(a) |= SCHEME_TOPLEVEL_MUTATED;
      }

      b = SCHEME_PTR2_VAL(e);
      b = unresolve_expr(b, ui, 0);
      if (!b) return_NULL;
      LOG_UNRESOLVE(printf(" (b) %d\n", b->type));

      o = scheme_alloc_object();
      o->type = scheme_varref_form_type;
      SCHEME_PTR1_VAL(o) = a;
      SCHEME_PTR2_VAL(o) = b;
      return o;
    }
  case scheme_apply_values_type:
    {
      return unresolve_apply_values(e, ui);
    }
  case scheme_boxenv_type:
    {
      return unresolve_expr(SCHEME_PTR2_VAL(e), ui, 0);
    }
  case scheme_toplevel_type:
    {
      return unresolve_toplevel(e, ui);
    }
  case scheme_case_lambda_sequence_type:
    {
      int i, cnt;
      Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)e, *cl2;

      cl2 = (Scheme_Case_Lambda *)scheme_malloc_tagged(sizeof(Scheme_Case_Lambda)
                                                       + ((cl->count - mzFLEX_DELTA) * sizeof(Scheme_Object*)));
      cl2->so.type = scheme_case_lambda_sequence_type;
      cl2->count = cl->count;
      cl2->name = cl->name; /* this may need more handling, see schpriv.c:1456 */

      cnt = cl->count; 

      for (i = 0; i < cnt; i++) {
        Scheme_Object *le;
        Scheme_Lambda *lam;
        if (SAME_TYPE(SCHEME_TYPE(cl->array[i]), scheme_closure_type)) {
          lam = ((Scheme_Closure *)cl->array[i])->code;
        } else {
          lam = (Scheme_Lambda *)cl->array[i];
        }

        le = unresolve_lambda(lam, ui);
        if (!le) return_NULL;
        
	cl2->array[i] = le;
      }

      return (Scheme_Object *)cl2;
    }
  case scheme_let_value_type:
    {
      Scheme_Let_Value *lv = (Scheme_Let_Value *)e;
      Scheme_Object *val, *body;
      val = unresolve_expr(lv->value, ui, 0);
      if (!val) return_NULL;
      
      body = unresolve_expr(lv->body, ui, 0);
      if (!body) return_NULL;
      
      return unresolve_let_value(lv, ui, val, body);
    }
  case scheme_quote_syntax_type:
    {
      Scheme_Quote_Syntax *qs = (Scheme_Quote_Syntax *)e;
      Scheme_Local *cqs;

      if (ui->inlining) return_NULL;

      cqs = (Scheme_Local *)scheme_malloc_atomic_tagged(sizeof(Scheme_Local));
      cqs->iso.so.type = scheme_ir_quote_syntax_type;
      cqs->position = qs->position;
      return (Scheme_Object *)cqs;
    }
  case scheme_require_form_type:
    {
      Scheme_Object *dummy = SCHEME_PTR1_VAL(e), *req;

      dummy = unresolve_expr(dummy, ui, 0);

      req = scheme_alloc_object();
      req->type = scheme_require_form_type;
      SCHEME_PTR1_VAL(req) = dummy;
      SCHEME_PTR2_VAL(req) = SCHEME_PTR2_VAL(e);
      
      return req;
    }
    break;
  default:
    if (SCHEME_TYPE(e) > _scheme_values_types_) {
      if (scheme_ir_duplicate_ok(e, 1) || !ui->inlining)
        return e;
      else if (ui->inlining)
        return_NULL;
    }

    scheme_signal_error("internal error: no unresolve for: %d", SCHEME_TYPE(e));
    return_NULL;
  }
}

Scheme_Object *scheme_unresolve_top(Scheme_Object* o, Comp_Prefix **cp, int comp_flags)
/* Convert from "resolved" form back to the intermediate representation used
   by the optimizer. Unresolving generates an intermediate-representation prefix
   (for top levels and syntax literals) in addition to the code. */
{
  Scheme_Compilation_Top *top = (Scheme_Compilation_Top *)o;
  Scheme_Object *code = top->code, *defns;
  Resolve_Prefix *rp = top->prefix;
  Comp_Prefix *c;
  Unresolve_Info *ui;
  int len, i;

  ui = new_unresolve_info(rp, comp_flags);

  c = unresolve_prefix(rp, ui);
  ui->comp_prefix = c;
  *cp = c;

  locate_cyclic_closures(code, ui);
  convert_closures_to_definitions(ui);

  code = unresolve_expr(code, ui, 0);
  if (!code) return_NULL;

  len = scheme_list_length(ui->definitions);
  if (len) {
    Scheme_Sequence *seq;
    seq = scheme_malloc_sequence(len+1);
    seq->so.type = scheme_sequence_type;
    seq->count = len+1;
    
    defns = ui->definitions;
    for (i = 0; i < len; i++) {
      seq->array[i] = SCHEME_CAR(defns);
      defns = SCHEME_CDR(defns);
    }
    seq->array[len] = code;
    code = (Scheme_Object *)seq;
  }

  return code;
}

Scheme_Object *scheme_unresolve(Scheme_Object *iv, int argc, int *_has_cases,
                                Comp_Prefix *cp, Scheme_Env *env, Scheme_Object *insp, intptr_t ref_phase,
                                Scheme_Object *from_modidx, Scheme_Object *to_modidx)
/* Convert a single function from "resolved" form back to the
   intermediate representation used by the optimizer. Unresolving can
   add new items to the intermediate-representation prefix for top levels. */
{
  Scheme_Object *o;
  Scheme_Lambda *lam = NULL;
  Unresolve_Info *ui;

  MZ_ASSERT(SAME_TYPE(SCHEME_TYPE(iv), scheme_inline_variant_type));

  o = SCHEME_VEC_ELS(iv)[1];

  if (SAME_TYPE(SCHEME_TYPE(o), scheme_closure_type))
    lam = ((Scheme_Closure *)o)->code;
  else if (SAME_TYPE(SCHEME_TYPE(o), scheme_lambda_type))
    lam = (Scheme_Lambda *)o;
  else if (SAME_TYPE(SCHEME_TYPE(o), scheme_case_lambda_sequence_type)
           || SAME_TYPE(SCHEME_TYPE(o), scheme_case_closure_type)) {
    Scheme_Case_Lambda *seqin = (Scheme_Case_Lambda *)o;
    int i, cnt;
    cnt = seqin->count;
    if (cnt > 1) *_has_cases = 1;
    for (i = 0; i < cnt; i++) {
      if (SAME_TYPE(SCHEME_TYPE(seqin->array[i]), scheme_closure_type)) {
        /* An empty closure, created at compile time */
        lam = ((Scheme_Closure *)seqin->array[i])->code;
      } else {
        lam = (Scheme_Lambda *)seqin->array[i];
      }
      if ((!(SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_HAS_REST) 
           && (lam->num_params == argc))
          || ((SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_HAS_REST)
              && (lam->num_params - 1 <= argc)))
        break;
      else
        lam = NULL;
    }
  } else
    lam = NULL;

  if (!lam)
    return_NULL;

  ui = new_unresolve_info((Resolve_Prefix *)SCHEME_VEC_ELS(iv)[2], 0);
  ui->inlining = 1;
  ui->from_modidx = from_modidx;
  ui->to_modidx = to_modidx;
  ui->new_toplevel_offset = cp->num_toplevels;
  ui->comp_prefix = cp;
  ui->opt_env = env;
  ui->opt_insp = insp;
  ui->toplevel_ref_phase = ref_phase;
  ui->inline_variants = scheme_null;

  /* convert an optimized & resolved closure back to compiled form: */
  o = unresolve_lambda(lam, ui);

  if (o) {
    /* Added any toplevels? */
    if (ui->new_toplevels) {
      int i;
      Scheme_Object *l;

      for (i = ui->new_toplevels->size; i--; ) {
        if (ui->new_toplevels->vals[i]) {
          scheme_hash_set(cp->toplevels,
                          ui->new_toplevels->keys[i],
                          ui->new_toplevels->vals[i]);
          cp->num_toplevels++;
        }
      }

      for (l = ui->inline_variants; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
        scheme_hash_set(ui->comp_prefix->inline_variants,
                        SCHEME_CAR(SCHEME_CAR(l)),
                        SCHEME_CDR(SCHEME_CAR(l)));
      }
    }
  }

  return o;
}

/*========================================================================*/
/*                         precise GC traversers                          */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_resolve.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_resolve_info, mark_resolve_info);
  GC_REG_TRAV(scheme_rt_unresolve_info, mark_unresolve_info);
}

END_XFORM_SKIP;

#endif
