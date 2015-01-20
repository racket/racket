/*
  Racket
  Copyright (c) 2004-2014 PLT Design Inc.
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
   the optimization IR to the evaluation IR --- where the main
   difference between the IRs is a change in stack addresses. This
   pass is also responsible for closure conversion (in the sense of
   lifting closures that are used only in application positions where
   all variables captured by the closure can be converted to arguments
   at all call sites).

   See "eval.c" for an overview of compilation passes. */

#include "schpriv.h"
#include "schrunst.h"
#include "schmach.h"

struct Resolve_Info
{
  MZTAG_IF_REQUIRED
  char use_jit, in_module, in_proc, enforce_const, no_lift;
  int size, oldsize, count, pos;
  int max_let_depth; /* filled in by sub-expressions */
  Resolve_Prefix *prefix;
  Scheme_Hash_Table *stx_map; /* compile offset => resolve offset; prunes prefix-recored stxes */
  mzshort toplevel_pos; /* -1 means consult `next' */
  void *tl_map; /* fixnum or bit array (as array of `int's) indicating which globals+lifts in prefix are used */
  mzshort *old_pos;
  mzshort *new_pos;
  int stx_count;
  mzshort *old_stx_pos; /* NULL => consult next; new pos is index in array */
  int *flags;
  Scheme_Object **lifted; /* maps bindings to lifts */
  Scheme_Object *lifts; /* accumulates lift info */
  struct Resolve_Info *next;
};

#define cons(a,b) scheme_make_pair(a,b)

static Scheme_Object *
resolve_closure_compilation(Scheme_Object *_data, Resolve_Info *info, 
                            int can_lift, int convert, int just_compute_lift,
                            Scheme_Object *precomputed_lift);
static Resolve_Info *resolve_info_extend(Resolve_Info *info, int size, int oldsize, int mapcount);
static void resolve_info_add_mapping(Resolve_Info *info, int oldp, int newp, int flags, Scheme_Object *lifted);
static void resolve_info_adjust_mapping(Resolve_Info *info, int oldp, int newp, int flags, Scheme_Object *lifted);
static int resolve_info_flags(Resolve_Info *info, int pos, Scheme_Object **lifted);
static int resolve_info_lookup(Resolve_Info *resolve, int pos, int *flags, Scheme_Object **lifted, int convert_shift);
static void resolve_info_set_toplevel_pos(Resolve_Info *info, int pos);
static void merge_resolve_tl_map(Resolve_Info *info, Resolve_Info *new_info);
static Scheme_Object *resolve_generate_stub_lift(void);
static int resolve_toplevel_pos(Resolve_Info *info);
static int resolve_quote_syntax_offset(int i, Resolve_Info *info);
static int resolve_quote_syntax_pos(Resolve_Info *info);
static Scheme_Object *resolve_toplevel(Resolve_Info *info, Scheme_Object *expr, int keep_ready);
static Scheme_Object *resolve_invent_toplevel(Resolve_Info *info);
static Scheme_Object *resolve_invented_toplevel_to_defn(Resolve_Info *info, Scheme_Object *tl);
static Scheme_Object *shift_toplevel(Scheme_Object *expr, int delta);
static int resolving_in_procedure(Resolve_Info *info);
static int is_nonconstant_procedure(Scheme_Object *data, Resolve_Info *info, int skip);
static int resolve_is_inside_proc(Resolve_Info *info);
static void set_tl_pos_used(Resolve_Info *info, int pos);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

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
{
  Scheme_Object *lifted;
  int flags;

  if (!SAME_TYPE(SCHEME_TYPE(rator), scheme_local_type))
    return NULL;

  (void)resolve_info_lookup(info, SCHEME_LOCAL_POS(rator), &flags, &lifted, orig_arg_cnt + 1);

  if (lifted && SCHEME_RPAIRP(lifted)) {
    Scheme_Object *vec, *arity;

    *new_rator = SCHEME_CAR(lifted);
    vec = SCHEME_CDR(lifted);
    *_rdelta = 0;

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
      Scheme_Object *loc;
      int m;
      m = SCHEME_VEC_SIZE(additions) - 1;
      app2 = scheme_malloc_application(n + m);
      for (i = 0; i < m; i++) {
        loc = SCHEME_VEC_ELS(additions)[i+1];
        if (SCHEME_BOXP(loc)) 
          loc = SCHEME_BOX_VAL(loc);
        else if (SCHEME_VECTORP(loc))
          loc = SCHEME_VEC_ELS(loc)[0];
        app2->args[i + 1] = loc;
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
  
  info = resolve_info_extend(orig_info, n - 1, 0, 0);
  
  for (i = 0; i < n; i++) {
    Scheme_Object *le;
    if (already_resolved_arg_count) {
      already_resolved_arg_count--;
    } else {
      le = scheme_resolve_expr(app->args[i], info);
      app->args[i] = le;
    }
  }

  info->max_let_depth += (n - 1);
  if (orig_info->max_let_depth < info->max_let_depth)
    orig_info->max_let_depth = info->max_let_depth;
  merge_resolve_tl_map(orig_info, info);

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
  Scheme_Object *le;

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
        Scheme_Object *loc;
        int i;
        app2 = scheme_malloc_application(2 + m);
        for (i = 0; i < m; i++) {
          loc = SCHEME_VEC_ELS(additions)[i+1];
          if (SCHEME_BOXP(loc))
            loc = SCHEME_BOX_VAL(loc);
          else if (SCHEME_VECTORP(loc))
            loc = SCHEME_VEC_ELS(loc)[0];
          app2->args[i + 1] = loc;
        }
        app2->args[0] = rator;
        app2->args[m+1] = app->rand;
        SCHEME_APPN_FLAGS(app2) |= APPN_FLAG_SFS_TAIL;
        return resolve_application((Scheme_Object *)app2, orig_info, m + 1 + rdelta);
      } else {
        Scheme_App3_Rec *app2;
        Scheme_Object *loc;
        app2 = MALLOC_ONE_TAGGED(Scheme_App3_Rec);
        app2->iso.so.type = scheme_application3_type;
        app2->rator = rator;
        loc = SCHEME_VEC_ELS(additions)[1];
        if (SCHEME_BOXP(loc))
          loc = SCHEME_BOX_VAL(loc);
        else if (SCHEME_VECTORP(loc))
          loc = SCHEME_VEC_ELS(loc)[0];
        app2->rand1 = loc;
        app2->rand2 = app->rand;
        SCHEME_APPN_FLAGS(app2) |= APPN_FLAG_SFS_TAIL;
        return resolve_application3((Scheme_Object *)app2, orig_info, 2 + rdelta);
      }
    }
  }

  info = resolve_info_extend(orig_info, 1, 0, 0);

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

  info->max_let_depth += 1;
  if (orig_info->max_let_depth < info->max_let_depth)
    orig_info->max_let_depth = info->max_let_depth;
  merge_resolve_tl_map(orig_info, info);

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
        Scheme_Object *loc;
        app2 = scheme_malloc_application(3 + m);
        for (i = 0; i < m; i++) {
          loc = SCHEME_VEC_ELS(additions)[i+1];
          if (SCHEME_BOXP(loc))
            loc = SCHEME_BOX_VAL(loc);
          else if (SCHEME_VECTORP(loc))
            loc = SCHEME_VEC_ELS(loc)[0];
          app2->args[i + 1] = loc;
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

  info = resolve_info_extend(orig_info, 2, 0, 0);

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
  if ((SAME_OBJ(app->rator, scheme_equal_prim)
       || SAME_OBJ(app->rator, scheme_eqv_prim))
      && (scheme_eq_testable_constant(app->rand1)
         || scheme_eq_testable_constant(app->rand2))) {
    app->rator = scheme_eq_prim;
  }

  set_app3_eval_type(app);

  info->max_let_depth += 2;
  if (orig_info->max_let_depth < info->max_let_depth)
    orig_info->max_let_depth = info->max_let_depth;
  merge_resolve_tl_map(orig_info, info);

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
      if (scheme_omittable_expr(lv->body, 1, -1, 0, NULL, NULL, 0, 0, 0)) {
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

  if (SAME_TYPE(SCHEME_TYPE(var), scheme_local_type)) {
    Scheme_Let_Value *lv;
    Scheme_Object *cv;
    int flags, li;

    cv = scheme_compiled_void();

    lv = MALLOC_ONE_TAGGED(Scheme_Let_Value);
    lv->iso.so.type = scheme_let_value_type;
    lv->body = cv;
    lv->count = 1;
    li = resolve_info_lookup(rslv, SCHEME_LOCAL_POS(var), &flags, NULL, 0);
    lv->position = li;
    SCHEME_LET_AUTOBOX(lv) = (flags & SCHEME_INFO_BOXED);
    lv->value = val;

    if (!(flags & SCHEME_INFO_BOXED))
      scheme_signal_error("internal error: set!: set!ed local variable is not boxed");

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
  } else if (SAME_TYPE(SCHEME_TYPE(v), scheme_local_type)) {
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

static Scheme_Object *
case_lambda_resolve(Scheme_Object *expr, Resolve_Info *rslv)
{
  int i, all_closed = 1;
  Scheme_Case_Lambda *seq = (Scheme_Case_Lambda *)expr;

  for (i = 0; i < seq->count; i++) {
    Scheme_Object *le;
    le = seq->array[i];
    le = resolve_closure_compilation(le, rslv, 0, 0, 0, NULL);
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

  rp = scheme_resolve_prefix(1, cp, 1);

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

  rp = scheme_resolve_prefix(1, cp, 1);

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
  Scheme_Object **ca;

  cl = scheme_malloc_empty_closure();

  ca = MALLOC_N(Scheme_Object*, 4);
  ca[0] = scheme_make_integer(0);
  ca[1] = NULL;
  ca[2] = scheme_make_integer(0);
  ca[3] = NULL;

  return scheme_make_raw_pair((Scheme_Object *)cl, (Scheme_Object *)ca);
}

static void shift_lift(Scheme_Object *lifted, int frame_size, int lifted_frame_size)
{
  int i, cnt, delta;
  Scheme_Object **ca;
  mzshort *map;

  if (!lifted) return;
  if (!SCHEME_RPAIRP(lifted)) return;

  ca = (Scheme_Object **)SCHEME_CDR(lifted);
  cnt = SCHEME_INT_VAL(ca[0]);
  map = (mzshort *)ca[1];

  delta = (frame_size - lifted_frame_size);

  for (i = 0; i < cnt; i++) {
    map[i] += delta;
  }
}

static int get_convert_arg_count(Scheme_Object *lift)
{
  if (!lift)
    return 0;
  else if (SCHEME_RPAIRP(lift)) {
    Scheme_Object **ca;
    ca = (Scheme_Object **)SCHEME_CDR(lift);
    return SCHEME_INT_VAL(ca[0]);
  } else
    return 0;
}

static mzshort* get_convert_arg_map(Scheme_Object *lift)
{
  if (!lift)
    return NULL;
  else if (SCHEME_RPAIRP(lift)) {
    Scheme_Object **ca;
    ca = (Scheme_Object **)SCHEME_CDR(lift);
    return (mzshort *)ca[1];
  } else
    return NULL;
}

static Scheme_Object *drop_zero_value_return(Scheme_Object *expr)
{
  if (SAME_TYPE(SCHEME_TYPE(expr), scheme_sequence_type)) {
    if (((Scheme_Sequence *)expr)->count == 2) {
      if (SAME_TYPE(SCHEME_TYPE(((Scheme_Sequence *)expr)->array[1]), scheme_application_type)) {
        if (((Scheme_App_Rec *)((Scheme_Sequence *)expr)->array[1])->num_args == 0) {
          if (SAME_OBJ(scheme_values_func, ((Scheme_App_Rec *)((Scheme_Sequence *)expr)->array[1])->args[0])) {
            return ((Scheme_Sequence *)expr)->array[0];
          }
        }
      }
    }
  }

  return NULL;
}

#define NUM_SKIPS_FAST 5

Scheme_Object *
scheme_resolve_lets(Scheme_Object *form, Resolve_Info *info)
{
  Resolve_Info *linfo, *val_linfo = NULL;
  Scheme_Let_Header *head = (Scheme_Let_Header *)form;
  Scheme_Compiled_Let_Value *clv, *pre_body;
  Scheme_Let_Value *lv, *last = NULL;
  Scheme_Object *first = NULL, *body, *last_body = NULL, *last_seq = NULL;
  Scheme_Letrec *letrec;
  mzshort *skips, skips_fast[NUM_SKIPS_FAST];
  char *local_types, local_types_fast[NUM_SKIPS_FAST];
  Scheme_Object **lifted, *lifted_fast[NUM_SKIPS_FAST], *boxes;
  int i, pos, opos, rpos, recbox, num_rec_procs = 0, extra_alloc;
  int rec_proc_nonapply = 0;
  int max_let_depth = 0;
  int resolve_phase, num_skips;
  Scheme_Object **lifted_recs;
  int post_bind = !(SCHEME_LET_FLAGS(head) & (SCHEME_LET_RECURSIVE | SCHEME_LET_STAR));

  /* Find body: */
  body = head->body;
  pre_body = NULL;
  for (i = head->num_clauses; i--; ) {
    pre_body = (Scheme_Compiled_Let_Value *)body;
    body = pre_body->body;
  }

  recbox = 0;
  if (SCHEME_LET_FLAGS(head) & SCHEME_LET_RECURSIVE) {
    /* Do we need to box vars in a letrec? */
    clv = (Scheme_Compiled_Let_Value *)head->body;
    for (i = head->num_clauses; i--; clv = (Scheme_Compiled_Let_Value *)clv->body) {
      int is_proc, is_lift;

      if ((clv->count == 1) 
          && !(clv->flags[0] & SCHEME_WAS_USED)) {
        /* skip */
      } else {
        if (clv->count == 1) 
          is_proc = scheme_is_compiled_procedure(clv->value, 1, 1);
        else
          is_proc = 0;

        if (is_proc)
          is_lift = 0;
        else if (SCHEME_CLV_FLAGS(clv) & SCHEME_CLV_NO_GROUP_USES)
          is_lift = 1;
        else
          is_lift = scheme_is_liftable(clv->value, head->count, 5, 1, 0);
      
        if (!is_proc && !is_lift) {
          recbox = 1;
          break;
        } else {
          if (!is_lift) {
            /* is_proc must be true ... */
            int j;

            for (j = 0; j < clv->count; j++) {
              if (clv->flags[j] & SCHEME_WAS_SET_BANGED) {
                recbox = 1;
                break;
              }
            }
            if (recbox)
              break;

            if (is_nonconstant_procedure(clv->value, info, head->count)) {
              num_rec_procs++;
              if (!(clv->flags[0] & SCHEME_WAS_ONLY_APPLIED))
                rec_proc_nonapply = 1;
            }
          }
        }
      }
    }

    if (recbox)
      num_rec_procs = 0;
  } else {
    /* Sequence of single-value, non-assigned lets? */

    clv = (Scheme_Compiled_Let_Value *)head->body;
    for (i = head->num_clauses; i--; clv = (Scheme_Compiled_Let_Value *)clv->body) {
      if (clv->count != 1)
	break;
      if (clv->flags[0] & SCHEME_WAS_SET_BANGED)
	break;
    }

    if (i < 0) {
      /* Yes - build chain of Scheme_Let_Ones and we're done: */
      int skip_count = 0, frame_size, lifts_frame_size = 0;
      int j, k, n, rev_bind_order = 0;

      if (head->num_clauses > 1) {
        clv = (Scheme_Compiled_Let_Value *)head->body;
        if (clv->position > ((Scheme_Compiled_Let_Value *)clv->body)->position)
          rev_bind_order = 1;
      }

      j = head->num_clauses;
      if (j <= NUM_SKIPS_FAST) {
	skips = skips_fast; 
        lifted = lifted_fast;
	local_types = local_types_fast; 
      } else {
	skips = MALLOC_N_ATOMIC(mzshort, j);
	lifted = MALLOC_N(Scheme_Object*, j);
	local_types = MALLOC_N_ATOMIC(char, j);
      }

      clv = (Scheme_Compiled_Let_Value *)head->body;
      for (i = 0; i < j; i++, clv = (Scheme_Compiled_Let_Value *)clv->body) {
        int aty, pty;

	if (!(clv->flags[0] & SCHEME_WAS_USED))
	  skips[i] = 1;
	else
	  skips[i] = 0;

        aty = SCHEME_WAS_TYPED_ARGUMENT(clv->flags[0]);
        pty = scheme_expr_produces_local_type(clv->value);
        if (pty && ((pty == aty) || ALWAYS_PREFER_UNBOX_TYPE(pty)))
          local_types[i] = pty;
        else
          local_types[i] = 0;
        lifted[i] = NULL;
      }

      clv = (Scheme_Compiled_Let_Value *)head->body;
      for (i = 0; i < head->num_clauses; i++, clv = (Scheme_Compiled_Let_Value *)clv->body) {
	Scheme_Object *le;

	if (!(clv->flags[0] & SCHEME_WAS_USED)) {
	  skip_count++;
	}

	/* First `i+1' bindings now exist "at runtime", except those skipped. */
	/* The mapping is complicated because we now push in the order of 
	   the variables, but it may have been compiled using the inverse order. */
        frame_size = i + 1 - skip_count;
        if (lifts_frame_size != frame_size) {
          /* We need to shift coordinates for any lifted[j] that is a
             converted procedure. */
          for (j = i, k = 0; j >= 0; j--) {
            shift_lift(lifted[j], frame_size, lifts_frame_size);
          }
        }
        if (post_bind) {
          linfo = resolve_info_extend(info, frame_size, 0, 0);
        } else {
          linfo = resolve_info_extend(info, frame_size, head->count, i + 1);
          for (j = i, k = 0; j >= 0; j--) {
            n = (rev_bind_order ? (head->count - j - 1) : j);
            if (skips[j])
              resolve_info_add_mapping(linfo, n, -1, local_types[j] << SCHEME_INFO_TYPED_VAL_SHIFT, lifted[j]);
            else
              resolve_info_add_mapping(linfo, n, k++, local_types[j] << SCHEME_INFO_TYPED_VAL_SHIFT, lifted[j]);
          }
        }
        lifts_frame_size = frame_size;

        if (skips[i]) {
          le = scheme_void;
        } else {
          if ((clv->flags[0] & SCHEME_WAS_ONLY_APPLIED)
              && SAME_TYPE(SCHEME_TYPE(clv->value), scheme_compiled_unclosed_procedure_type))
            le = resolve_closure_compilation(clv->value, linfo, 1, 1, 0, NULL);
          else
            le = scheme_resolve_expr(clv->value, linfo);
        }

        if (max_let_depth < linfo->max_let_depth + frame_size)
          max_let_depth = linfo->max_let_depth + frame_size;
        merge_resolve_tl_map(info, linfo);

        if (is_lifted_reference(le)) {
          lifted[i] = le;

          /* At this point, it's ok to change our mind
             about skipping, because compilation for previous
             RHSs did not look at this one. */
          if (!skips[i]) {
            skips[i] = 1;
            skip_count++;
          }
        }

	if (skips[i]) {
	  /* Unused binding, so drop it. */
	} else {
	  Scheme_Let_One *lo;
	  int et;

	  lo = MALLOC_ONE_TAGGED(Scheme_Let_One);
	  lo->iso.so.type = scheme_let_one_type;
	  lo->value = le;

	  et = scheme_get_eval_type(lo->value);
          if (local_types[i])
            et |= (local_types[i] << LET_ONE_TYPE_SHIFT);
	  SCHEME_LET_EVAL_TYPE(lo) = et;

	  if (last)
	    ((Scheme_Let_One *)last)->body = (Scheme_Object *)lo;
	  else
	    first = (Scheme_Object *)lo;
	  last = (Scheme_Let_Value *)lo;
	}
      }

      frame_size = head->count - skip_count;
      linfo = resolve_info_extend(info, frame_size, head->count, head->count);

      if (lifts_frame_size != frame_size) {
        for (i = head->count; i--; ) {
          /* We need to shift coordinates for any lifted[j] that is a
             converted procedure. */
          shift_lift(lifted[i], frame_size, lifts_frame_size);
        }
      }

      for (k = 0, i = head->count; i--; ) {
        n = (rev_bind_order ? (head->count - i - 1) : i);
        if ((skips[i] != 0) && (skips[i] != 1)) scheme_signal_error("trashed\n");
	if (skips[i])
	  resolve_info_add_mapping(linfo, n, -1, local_types[i] << SCHEME_INFO_TYPED_VAL_SHIFT, lifted[i]);
	else
	  resolve_info_add_mapping(linfo, n, k++, local_types[i] << SCHEME_INFO_TYPED_VAL_SHIFT, lifted[i]);
      }
      
      body = scheme_resolve_expr(body, linfo);
      if (last)
	((Scheme_Let_One *)last)->body = body;
      else {
	first = body;
      }

      if (max_let_depth < linfo->max_let_depth + frame_size)
        max_let_depth = linfo->max_let_depth + frame_size;

      if (info->max_let_depth < max_let_depth)
        info->max_let_depth = max_let_depth;

      merge_resolve_tl_map(info, linfo);

      /* Check for (let ([x <expr>]) (<simple> x)) at end, and change to
         (<simple> <expr>). This transformation is more generally performed
         at the optimization layer, the code here pre-dates the mode general
         optimzation, and we keep it just in case. The simple case is easy here,
         because the local-variable offsets in <expr> do not change (as long as 
         <simple> doesn't access the stack). */
      last_body = NULL;
      body = first;
      while (1) {
        if (!SAME_TYPE(SCHEME_TYPE(body), scheme_let_one_type))
          break;
        if (!SAME_TYPE(SCHEME_TYPE(((Scheme_Let_One *)body)->body), scheme_let_one_type))
          break;
        last_body = body;
        body = ((Scheme_Let_One *)body)->body;
      }
      if (SAME_TYPE(SCHEME_TYPE(body), scheme_let_one_type)) {
        if (SAME_TYPE(SCHEME_TYPE(((Scheme_Let_One *)body)->body), scheme_application2_type)) {
          Scheme_App2_Rec *app = (Scheme_App2_Rec *)((Scheme_Let_One *)body)->body;
          if (SAME_TYPE(SCHEME_TYPE(app->rand), scheme_local_type)
              && (SCHEME_LOCAL_POS(app->rand) == 1)) {
            if (SCHEME_TYPE(app->rator) > _scheme_values_types_) {
              /* Move <expr> to app, and drop let-one: */
              app->rand = ((Scheme_Let_One *)body)->value;
              scheme_reset_app2_eval_type(app);
              if (last_body)
                ((Scheme_Let_One *)last_body)->body = (Scheme_Object *)app;
              else
                first = (Scheme_Object *)app;
            }
          }
        }
      }

      return first;
    } else {
      /* Maybe some multi-binding lets, but all of them are unused
         and the RHSes are omittable? This can happen with auto-generated
         code. */
      int total = 0, j;

      clv = (Scheme_Compiled_Let_Value *)head->body;
      for (i = head->num_clauses; i--; clv = (Scheme_Compiled_Let_Value *)clv->body) {
        total += clv->count;
        for (j = clv->count; j--; ) {
          if (clv->flags[j] & SCHEME_WAS_USED)
            break;
        }
        if (j >= 0)
          break;
        if (!scheme_omittable_expr(clv->value, clv->count, -1, 0, NULL, NULL, 0, 0, 0))
          break;
      }
      if (i < 0) {
        /* All unused and omittable */
        linfo = resolve_info_extend(info, 0, total, 0);
        first = scheme_resolve_expr((Scheme_Object *)clv, linfo);
        if (info->max_let_depth < linfo->max_let_depth)
          info->max_let_depth = linfo->max_let_depth;
        merge_resolve_tl_map(info, linfo);
        return first;
      }
    }
  }

  num_skips = 0;
  clv = (Scheme_Compiled_Let_Value *)head->body;
  for (i = head->num_clauses; i--; clv = (Scheme_Compiled_Let_Value *)clv->body) {
    if ((clv->count == 1) && !(clv->flags[0] & SCHEME_WAS_USED))
      num_skips++;
  }

  /* First assume that all letrec-bound procedures can be lifted to empty closures.
     Then try assuming that all letrec-bound procedures can be at least lifted.
     Then fall back to assuming no lifts. */
  
  linfo = 0;
  for (resolve_phase = ((num_rec_procs && !rec_proc_nonapply && !info->no_lift) ? 0 : 2); 
       resolve_phase < 3; 
       resolve_phase++) {

    /* Don't try plain lifting if we're not inside a proc: */
    if ((resolve_phase == 1) && !resolve_is_inside_proc(info))
      resolve_phase = 2;

    if (resolve_phase < 2) {
      linfo = resolve_info_extend(info, head->count - num_rec_procs - num_skips, head->count, head->count);
      lifted_recs = MALLOC_N(Scheme_Object *, num_rec_procs);
    } else {
      linfo = resolve_info_extend(info, head->count - num_skips, head->count, head->count);
      lifted_recs = NULL;
    }
    
    if (post_bind)
      val_linfo = resolve_info_extend(info, head->count - num_skips, 0, 0);
    else
      val_linfo = linfo;

    /* Build mapping of compile-time indices to run-time indices, shuffling
       letrecs to fall together in the shallowest part. Also determine
       and initialize lifts for recursive procedures. Generating lift information
       requires an iteration. */
    clv = (Scheme_Compiled_Let_Value *)head->body;
    pos = ((resolve_phase < 2) ? 0 : num_rec_procs);
    rpos = 0;
    for (i = head->num_clauses; i--; clv = (Scheme_Compiled_Let_Value *)clv->body) {
      int j;

      opos = clv->position;

      if ((clv->count == 1) && !(clv->flags[0] & SCHEME_WAS_USED)) {
        /* skipped */
        resolve_info_add_mapping(linfo, opos, 0, 0, NULL);
      } else {
        for (j = 0; j < clv->count; j++) {
          int p;
          Scheme_Object *lift;

          if (num_rec_procs 
              && (clv->count == 1)
              && is_nonconstant_procedure(clv->value, info, head->count)) {
            if (resolve_phase == 0) {
              lift = scheme_resolve_generate_stub_closure();
              lifted_recs[rpos] = lift;
              p = 0;
            } else if (resolve_phase == 1) {
              lift = resolve_generate_stub_lift();
              lifted_recs[rpos] = lift;
              p = 0;
            } else {
              lift = NULL;
              p = rpos;
            }
            rpos++;
          } else {
            p = pos++;
            lift = NULL;
          }
      
          resolve_info_add_mapping(linfo, opos, p,
                                   ((recbox 
                                     || (clv->flags[j] & SCHEME_WAS_SET_BANGED))
                                    ? SCHEME_INFO_BOXED
                                    : 0),
                                   lift);

          opos++;
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
        clv = (Scheme_Compiled_Let_Value *)head->body;
        rpos = 0;
        converted = 0;
        for (i = head->num_clauses; i--; clv = (Scheme_Compiled_Let_Value *)clv->body) {
          opos = clv->position;
          if ((clv->count == 1) && !(clv->flags[0] & SCHEME_WAS_USED)) {
            /* skipped */
          } else if ((clv->count == 1)
                     && is_nonconstant_procedure(clv->value, info, head->count)) {
            Scheme_Object *lift, *old_lift;
            int old_convert_count;
            mzshort *old_convert_map, *convert_map;

            old_lift = lifted_recs[rpos];
            old_convert_count = get_convert_arg_count(old_lift);
            old_convert_map = get_convert_arg_map(old_lift);

            lift = resolve_closure_compilation(clv->value, val_linfo, 1, 1, 1,
                                               (resolve_phase ? NULL : old_lift));

            if (is_closed_reference(lift)
                || (is_lifted_reference(lift) && resolve_phase)) {
              if (!SAME_OBJ(old_lift, lift))
                resolve_info_adjust_mapping(linfo, opos, rpos, 0, lift);
              lifted_recs[rpos] = lift;
              if (get_convert_arg_count(lift) != old_convert_count)
                converted = 1;
              else if (old_convert_map) {
                int z;
                convert_map = get_convert_arg_map(lift);
                for (z = 0; z < old_convert_count; z++) {
                  if (old_convert_map[z] != convert_map[z])
                    converted = 1;
                }
              }
            } else {
              lifted_recs = NULL;
              converted = 0;
              break;
            }
            rpos++;
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
        clv = (Scheme_Compiled_Let_Value *)head->body;
        rpos = 0;
        for (i = head->num_clauses; i--; clv = (Scheme_Compiled_Let_Value *)clv->body) {
          opos = clv->position;
          if ((clv->count == 1) && !(clv->flags[0] & SCHEME_WAS_USED)) {
            /* skipped */
          } else if ((clv->count == 1) && is_nonconstant_procedure(clv->value, info, head->count)) {
            Scheme_Object *lift;
            lift = lifted_recs[rpos];
            if (is_closed_reference(lift)) {
              (void)resolve_closure_compilation(clv->value, val_linfo, 1, 1, 0, lift);
              /* lift is the final result; this result might be
                 referenced in the body of closures already, or in
                 not-yet-closed functions.  If no one uses the result
                 via linfo, then the code was dead and it will get
                 GCed. */
              clv->value = NULL; /* indicates that there's nothing more to do with the expr */
            } else {
              lift = resolve_closure_compilation(clv->value, val_linfo, 1, 1, 2, NULL);
              /* need to resolve one more time for the body of the lifted function */
            }
            resolve_info_adjust_mapping(linfo, opos, rpos, 0, lift);
            lifted_recs[rpos] = lift;
            rpos++;
          }
        }

        break; /* don't need to iterate */
      }
    }
  }

  extra_alloc = 0;

  if (num_rec_procs) {
    if (!lifted_recs) {
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

  /* Resolve values: */
  boxes = scheme_null;
  clv = (Scheme_Compiled_Let_Value *)head->body;
  rpos = 0;
  for (i = head->num_clauses; i--; clv = (Scheme_Compiled_Let_Value *)clv->body) {
    opos = clv->position;
    if ((clv->count == 1) && !(clv->flags[0] & SCHEME_WAS_USED)) {
      /* skipped */
    } else {
      int isproc;
      Scheme_Object *expr;
      if (!clv->value)
        isproc = 1;
      else if (clv->count == 1)
        isproc = is_nonconstant_procedure(clv->value, info, post_bind ? 0 : head->count);
      else
        isproc = 0;
      if (num_rec_procs && isproc) {
        if (!lifted_recs) {
          expr = resolve_closure_compilation(clv->value, val_linfo, 0, 0, 0, NULL);
          if (!SAME_TYPE(SCHEME_TYPE(expr), scheme_unclosed_procedure_type)) {
            scheme_signal_error("internal error: unexpected empty closure");
          }
          letrec->procs[rpos++] = expr;
        } else {
          if (!is_closed_reference(lifted_recs[rpos])) {
            /* Side-effect is to install lifted function: */
            (void)resolve_closure_compilation(clv->value, val_linfo, 1, 1, 0, lifted_recs[rpos]);
          }
          rpos++;
        }
      } else {
        int j;
        Scheme_Object *one_lifted;

        if (!clv->count)
          expr = drop_zero_value_return(clv->value);
        else
          expr = NULL;

        if (expr) {
          /* Change a `[() (begin expr (values))]' clause,
             which can be generated by internal-definition expansion,
             into a `begin' */
          expr = scheme_resolve_expr(expr, val_linfo);
          expr = scheme_make_sequence_compilation(scheme_make_pair(expr,
                                                                   scheme_make_pair(scheme_false,
                                                                                    scheme_null)),
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
          expr = scheme_resolve_expr(clv->value, val_linfo);

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
          if (clv->count) {
            int li;
            li = resolve_info_lookup(linfo, clv->position, NULL, NULL, 0);
            lv->position = li;
          } else
            lv->position = 0;
          lv->count = clv->count;
          SCHEME_LET_AUTOBOX(lv) = recbox;

          for (j = lv->count; j--; ) {
            if (!recbox
                && (resolve_info_flags(linfo, opos + j, &one_lifted) & SCHEME_INFO_BOXED)) {
              GC_CAN_IGNORE Scheme_Object *pos;
              pos = scheme_make_integer(lv->position + j);
              if (SCHEME_LET_FLAGS(head) & (SCHEME_LET_STAR | SCHEME_LET_RECURSIVE)) {
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
  body = scheme_resolve_expr(body, linfo);

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
      SCHEME_LET_AUTOBOX(lvd) = recbox;
      
      first = (Scheme_Object *)lvd;
    }
  }

  if (info->max_let_depth < linfo->max_let_depth + head->count - num_skips + extra_alloc)
    info->max_let_depth = linfo->max_let_depth + head->count - num_skips + extra_alloc;
  merge_resolve_tl_map(info, linfo);
  if (val_linfo) {
    if (info->max_let_depth < val_linfo->max_let_depth + head->count - num_skips + extra_alloc)
      info->max_let_depth = val_linfo->max_let_depth + head->count - num_skips + extra_alloc;
    merge_resolve_tl_map(info, val_linfo);
  }
  
  return first;
}

/*========================================================================*/
/*                             closures                                   */
/*========================================================================*/

XFORM_NONGCING static int boxmap_size(int n)
{
  return ((CLOS_TYPE_BITS_PER_ARG * n) + (BITS_PER_MZSHORT - 1)) / BITS_PER_MZSHORT;
}

static mzshort *allocate_boxmap(int n)
{
  mzshort *boxmap;
  int size;

  size = boxmap_size(n);
  boxmap = MALLOC_N_ATOMIC(mzshort, size);
  memset(boxmap, 0, size * sizeof(mzshort));

  return boxmap;
}

void scheme_boxmap_set(mzshort *boxmap, int j, int bit, int delta)
/* assumes that existing bits are cleared */
{
  j *= CLOS_TYPE_BITS_PER_ARG;
  boxmap[delta + (j / BITS_PER_MZSHORT)] |= ((mzshort)bit << (j & (BITS_PER_MZSHORT - 1)));
}

static void boxmap_clear(mzshort *boxmap, int j, int delta)
{
  mzshort v;
  j *= CLOS_TYPE_BITS_PER_ARG;
  v = boxmap[delta + (j / BITS_PER_MZSHORT)];
  v ^= (v & ((mzshort)(((1 << CLOS_TYPE_BITS_PER_ARG) - 1) << (j & (BITS_PER_MZSHORT - 1)))));
  boxmap[delta + (j / BITS_PER_MZSHORT)] = v;
}

int scheme_boxmap_get(mzshort *boxmap, int j, int delta)
{
  j *= CLOS_TYPE_BITS_PER_ARG;
  return (boxmap[delta + (j / BITS_PER_MZSHORT)] >> (j & (BITS_PER_MZSHORT - 1))
          & ((1 << CLOS_TYPE_BITS_PER_ARG) - 1));
}

static int is_nonconstant_procedure(Scheme_Object *_data, Resolve_Info *info, int skip)
{
  /* check whether `data' --- which is in a `letrec' --- can be converted to
     a constant independent of other bindings in the `letrec' */
  Scheme_Closure_Data *data;
  Closure_Info *cl;
  Scheme_Object *lifted;
  int i, sz;

  if (SAME_TYPE(SCHEME_TYPE(_data), scheme_compiled_unclosed_procedure_type)) {
    data = (Scheme_Closure_Data *)_data;
    sz = data->closure_size;

    cl = (Closure_Info *)data->closure_map;
    if (cl->has_tl)
      return 1;

    for (i = 0; i < sz; i++) {
      if (cl->base_closure_map[i] < skip)
        return 1;
      resolve_info_lookup(info, cl->base_closure_map[i] - skip, NULL, &lifted, 0);
      if (!lifted)
        return 1;
      if (SAME_TYPE(SCHEME_TYPE(lifted), scheme_toplevel_type)
          || SAME_TYPE(SCHEME_TYPE(SCHEME_CAR(lifted)), scheme_toplevel_type))
        return 1;
    }

    return 0;
  }

  return 0;
}

static Scheme_Object *
resolve_closure_compilation(Scheme_Object *_data, Resolve_Info *info, 
                            int can_lift, int convert, int just_compute_lift,
                            Scheme_Object *precomputed_lift)
{
  Scheme_Closure_Data *data;
  int i, closure_size, offset, np, num_params, expanded_already = 0, captured_typed;
  int no_map_shift_needed;
  int has_tl, convert_size, need_lift;
  mzshort *oldpos, *closure_map, *new_closure_map;
  Closure_Info *cl;
  Resolve_Info *new_info;
  Scheme_Object *lifted, *result, *lifteds = NULL;
  Scheme_Hash_Table *captured = NULL;
  mzshort *convert_boxes = NULL; /* local type for captured (i.e,. first captured is at 0) */
  mzshort *convert_map;  /* includes local type for args and captured (i.e,. first captured 
                            is at data->num_params) */

  data = (Scheme_Closure_Data *)_data;
  cl = (Closure_Info *)data->closure_map;
  if (!just_compute_lift)
    data->iso.so.type = scheme_unclosed_procedure_type;

  if (convert || can_lift) {
    if (!convert && !resolving_in_procedure(info))
      can_lift = 0; /* no point in lifting when outside of a lambda or letrec */
    if (!info->lifts)
      can_lift = 0;
  }

  /* We have to perform a small bit of constant propagation here.
     Procedures closed only over top-level bindings are lifted during
     this pass. Some of the captured bindings from this phase may
     refer to a lifted procedure. In that case, we can replace the
     lexical reference with a direct reference to the top-level
     binding, which means that we can drop the binding from the
     closure. */

  closure_size = data->closure_size;
  if (cl->local_type_map) {
    int at_least_one = 0;
    for (i = data->num_params; i--; ) {
      if (cl->local_type_map[i]) {
        if (SCHEME_WAS_TYPED_ARGUMENT(cl->local_flags[i]) == cl->local_type_map[i])
          at_least_one = 1;
        else
          cl->local_type_map[i] = 0;
      }
    }
    if (at_least_one) {
      closure_size += boxmap_size(data->num_params + closure_size);
      expanded_already = 1;
    } else
      cl->local_type_map = NULL;
  }
  closure_map = (mzshort *)scheme_malloc_atomic(sizeof(mzshort) * closure_size);
  if (cl->local_type_map)
    memset(closure_map, 0, sizeof(mzshort) * closure_size);

  has_tl = cl->has_tl;
  if (has_tl && !can_lift)
    convert = 0;

  /* Locals in closure are first: */
  oldpos = cl->base_closure_map;
  offset = 0;
  for (i = 0; i < cl->base_closure_size; i++) {
    int li, flags;
    li = resolve_info_lookup(info, oldpos[i], &flags, &lifted, 0);
    if (lifted) {
      /* Drop lifted binding from closure. */
      if (SAME_TYPE(SCHEME_TYPE(lifted), scheme_toplevel_type)
          || SAME_TYPE(SCHEME_TYPE(SCHEME_CAR(lifted)), scheme_toplevel_type)) {
        has_tl = 1;
        if (!can_lift)
          convert = 0;
      }
      /* If the lifted binding is for a converted closure,
         we may need to add more bindings to this closure. */
      if (SCHEME_RPAIRP(lifted)) {
        lifteds = scheme_make_raw_pair(lifted, lifteds);
      }
    } else {
      closure_map[offset] = li;
      if (convert && (flags & (SCHEME_INFO_BOXED | SCHEME_INFO_TYPED_VAL_MASK))) {
        /* The only problem with a boxed/local_type variable is that
           it's more difficult to validate. We have to track
           which arguments are boxes. And the resulting procedure
           must be used only in application positions. */
        if (!convert_boxes)
          convert_boxes = allocate_boxmap(cl->base_closure_size);
        scheme_boxmap_set(convert_boxes, offset, 
                          ((flags & SCHEME_INFO_BOXED) 
                           ? CLOS_TYPE_BOXED 
                           : CLOS_TYPE_TYPE_OFFSET + (flags >> SCHEME_INFO_TYPED_VAL_SHIFT)), 
                          0);
      } else {
        /* Currently, we only need local_type information as a closure type */
        if (flags & SCHEME_INFO_TYPED_VAL_MASK) {
          if (!expanded_already) {
            closure_size += boxmap_size(data->num_params + closure_size);
            new_closure_map = (mzshort *)scheme_malloc_atomic(sizeof(mzshort) * closure_size);
            memset(new_closure_map, 0, sizeof(mzshort) * closure_size);
            memcpy(new_closure_map, closure_map, sizeof(mzshort) * data->closure_size);
            closure_map = new_closure_map;
            expanded_already = 1;
          }
          scheme_boxmap_set(closure_map, data->num_params + offset, 
                            CLOS_TYPE_TYPE_OFFSET + (flags >> SCHEME_INFO_TYPED_VAL_SHIFT), 
                            data->closure_size);
        }
      }
      offset++;
    }
  }
  
  /* Add bindings introduced by closure conversion. The `captured'
     table maps old positions to new positions. */
  captured_typed = 0;
  while (lifteds) {
    int j, cnt, local_typed;
    Scheme_Object *vec, *loc;

    if (!captured) {
      captured = scheme_make_hash_table(SCHEME_hash_ptr);
      for (i = 0; i < offset; i++) {
        int cp, v;
        cp = i;
        if (convert_boxes) {
          v = scheme_boxmap_get(convert_boxes, i, 0);
        } else if (expanded_already) {
          v = scheme_boxmap_get(closure_map, data->num_params + i, data->closure_size);
        } else 
          v = 0;
        if (v)
          cp = -((cp << CLOS_TYPE_BITS_PER_ARG) + v);
        scheme_hash_set(captured, scheme_make_integer(closure_map[i]), scheme_make_integer(cp));
      }
    }

    lifted = SCHEME_CAR(lifteds);
    vec = SCHEME_CDR(lifted);
    cnt = SCHEME_VEC_SIZE(vec);
    --cnt;
    for (j = 0; j < cnt; j++) {
      loc = SCHEME_VEC_ELS(vec)[j+1];
      if (SCHEME_BOXP(loc)) {
        loc = SCHEME_BOX_VAL(loc);
        local_typed = CLOS_TYPE_BOXED;
      } else if (SCHEME_VECTORP(loc)) {
        local_typed = SCHEME_INT_VAL(SCHEME_VEC_ELS(loc)[1]);
        loc = SCHEME_VEC_ELS(loc)[0];
      } else {
        local_typed = 0;
      }
      i = SCHEME_LOCAL_POS(loc);
      if (!scheme_hash_get(captured, scheme_make_integer(i))) {
        /* Need to capture an extra binding: */
        int cp;
        cp = captured->count;
        if (local_typed) {
          cp = -((cp << CLOS_TYPE_BITS_PER_ARG) + local_typed);
          captured_typed = 1;
        }
        scheme_hash_set(captured, scheme_make_integer(i), scheme_make_integer(cp));
      }
    }

    lifteds = SCHEME_CDR(lifteds);
  }

  if (captured && (captured->count > offset)) {
    /* We need to extend the closure map.  All the info
       is in captured, so just build it from scratch. */
    int old_pos, j, new_size, need_flags;
    new_size = (captured->count + (has_tl ? 1 : 0));
    if (cl->local_type_map || expanded_already || convert_boxes || captured_typed) {
      need_flags = new_size;
      new_size += boxmap_size(data->num_params + new_size);
      expanded_already = 1;
    } else
      need_flags = 0;
    closure_map = (mzshort *)scheme_malloc_atomic(sizeof(mzshort) * new_size);
    if (need_flags)
      memset(closure_map, 0, sizeof(mzshort) * new_size);
    offset = captured->count;
    convert_boxes = NULL;
    for (j = captured->size; j--; ) {
      if (captured->vals[j]) {
        int cp;
        cp = SCHEME_INT_VAL(captured->vals[j]);
        old_pos = SCHEME_INT_VAL(captured->keys[j]);
        if (cp < 0) {
          /* Boxed or local_type */
          int bit;
          cp = -cp;
          bit = cp & ((1 << CLOS_TYPE_BITS_PER_ARG) - 1);
          cp >>= CLOS_TYPE_BITS_PER_ARG;
          if (!convert_boxes)
            convert_boxes = allocate_boxmap(offset);
          scheme_boxmap_set(convert_boxes, cp, bit, 0);
          if (need_flags && (bit > CLOS_TYPE_TYPE_OFFSET))
            scheme_boxmap_set(closure_map, cp + data->num_params, bit, need_flags);
        }
        closure_map[cp] = old_pos;
      }
    }
    no_map_shift_needed = 1;
  } else
    no_map_shift_needed = 0;

  if (convert
      && (offset || !has_tl) /* either need args, or treat as convert because it's fully closed */
      ) {
    /* Take over closure_map to be the convert map, instead. */ 
    convert_map = closure_map;
    convert_size = offset;

    if (has_tl || convert_boxes || cl->local_type_map) {
      int new_boxes_size;
      int sz;
      new_boxes_size = boxmap_size(convert_size + data->num_params + (has_tl ? 1 : 0));
      sz = ((has_tl ? sizeof(mzshort) : 0) + new_boxes_size * sizeof(mzshort));
      closure_map = (mzshort *)scheme_malloc_atomic(sz);
      memset(closure_map, 0, sz);
      if (convert_boxes) {
        int bsz;
        bsz = boxmap_size(convert_size);
        memcpy(closure_map XFORM_OK_PLUS (has_tl ? 1 : 0), 
               convert_boxes,
               bsz * sizeof(mzshort));
      }
    } else
      closure_map = NULL;
    offset = 0;
  } else {
    convert = 0;
    convert_map = NULL;
    convert_size = 0;
    convert_boxes = NULL;
  }

  /* Then the pointer to globals, if any: */
  if (has_tl) {
    /* GLOBAL ASSUMPTION: jit.c assumes that the array
       of globals is the last item in the closure; grep
       for "GLOBAL ASSUMPTION" in jit.c and mzmark.c */
    int li;
    li = resolve_toplevel_pos(info);
    closure_map[offset] = li;
    offset++;
  }

  if (!convert && !just_compute_lift && (offset < data->closure_size) 
      && expanded_already && !no_map_shift_needed) {
    /* shift boxmap down, since we're dropping closure elements */
    int bsz;
    bsz = boxmap_size(data->num_params + offset);
    memmove(closure_map + offset, closure_map + data->closure_size, sizeof(mzshort) * bsz);
  }

  /* Reset closure_size, in case a lifted variable was removed: */
  closure_size = offset;
  if (!just_compute_lift) {
    data->closure_size = closure_size;
    if (convert && convert_boxes)
      SCHEME_CLOSURE_DATA_FLAGS(data) |= CLOS_HAS_TYPED_ARGS;
  }

  /* Set up environment mapping, initialized for arguments: */
  
  np = num_params = data->num_params;
  if ((data->num_params == 1)
      && (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST)
      && !(cl->local_flags[0] & SCHEME_WAS_USED)
      && !convert) {
    /* (lambda args E) where args is not in E => drop the argument */
    new_info = resolve_info_extend(info, 0, 1, cl->base_closure_size);
    num_params = 0;
    if (!just_compute_lift) {
      data->num_params = 0;
      if (expanded_already) {
        /* shift type map down: */
        for (i = 0; i < closure_size; i++) {
          boxmap_clear(closure_map, i, closure_size);
          scheme_boxmap_set(closure_map, i, scheme_boxmap_get(closure_map, i + 1, closure_size), closure_size);
        }
        SCHEME_CLOSURE_DATA_FLAGS(data) |= CLOS_HAS_TYPED_ARGS;
      }
    }
  } else {
    new_info = resolve_info_extend(info, data->num_params, data->num_params,
                                   cl->base_closure_size + data->num_params);
    for (i = 0; i < data->num_params; i++) {
      resolve_info_add_mapping(new_info, i, i + closure_size + convert_size,
                               (((cl->local_flags[i] & SCHEME_WAS_SET_BANGED)
                                 ? SCHEME_INFO_BOXED
                                 : 0)
                                | ((convert && (cl->local_type_map && cl->local_type_map[i]))
                                   ? (cl->local_type_map[i] << SCHEME_INFO_TYPED_VAL_SHIFT)
                                   : 0)),
                               NULL);
      if (convert && cl->local_type_map && cl->local_type_map[i] && !just_compute_lift)
        scheme_boxmap_set(closure_map, i + convert_size, 
                          cl->local_type_map[i] + CLOS_TYPE_TYPE_OFFSET,
                          closure_size);
    }
    if (expanded_already && !just_compute_lift)
      SCHEME_CLOSURE_DATA_FLAGS(data) |= CLOS_HAS_TYPED_ARGS;
  }

  /* Extend mapping to go from old locations on the stack (as if bodies were
     evaluated immediately) to new locations (where closures
     effectively shift and compact values on the stack). 

     We don't have to include bindings added because an original
     binding was lifted (i.e., the extra bindings in `captured'),
     because they don't appear in the body. Instead, they are
     introduced directly in resolved form through the `lifted' info.
     That means, though, that we need to transform the `lifted'
     mapping. */
  if (has_tl && convert) {
    /* Skip handle for globals */
    offset = 1;
  } else {
    offset = 0;
  }
  for (i = 0; i < cl->base_closure_size; i++) {
    int p = oldpos[i], flags;

    if (p < 0)
      p -= np;
    else
      p += np;

    flags = resolve_info_flags(info, oldpos[i], &lifted);

    if (lifted && SCHEME_RPAIRP(lifted)) {
      /* Convert from a vector of local references to an array of
         positions. */
      Scheme_Object *vec, *loc, **ca;
      mzshort *cmap, *boxmap = NULL;
      int sz, j, cp;

      vec = SCHEME_CDR(lifted);
      sz = SCHEME_VEC_SIZE(vec);
      --sz;
      cmap = MALLOC_N_ATOMIC(mzshort, sz);
      for (j = 0; j < sz; j++) {
        int is_boxed = 0, is_local_type = 0;
        loc = SCHEME_VEC_ELS(vec)[j+1];
        if (SCHEME_BOXP(loc)) {
          if (!boxmap)
            boxmap = allocate_boxmap(sz);
          scheme_boxmap_set(boxmap, j, CLOS_TYPE_BOXED, 0);
          loc = SCHEME_BOX_VAL(loc);
          is_boxed = 1;
        } else if (SCHEME_VECTORP(loc)) {
          if (!boxmap)
            boxmap = allocate_boxmap(sz);
          scheme_boxmap_set(boxmap, j, SCHEME_INT_VAL(SCHEME_VEC_ELS(loc)[1]), 0);
          loc = SCHEME_VEC_ELS(loc)[0];
          is_local_type = 1;
        }
        loc = scheme_hash_get(captured, scheme_make_integer(SCHEME_LOCAL_POS(loc)));
        cp = SCHEME_INT_VAL(loc);
        if (cp < 0) {
          int v;
          cp = -cp;
          v = cp & ((1 << CLOS_TYPE_BITS_PER_ARG) - 1);
          cp >>= CLOS_TYPE_BITS_PER_ARG;
          if (v == CLOS_TYPE_BOXED) {
            if (convert && !is_boxed)
              scheme_signal_error("internal error: lift mismatch (boxed)");
          } else {
            if (convert && !is_local_type)
              scheme_signal_error("internal error: lift mismatch (local type) %d", v);
          }
        } else {
          if (convert && (is_boxed || is_local_type))
            scheme_signal_error("internal error: lift mismatch");
        }
        cmap[j] = cp + (has_tl && convert ? 1 : 0);
      }

      ca = MALLOC_N(Scheme_Object *, 4);
      ca[0] = scheme_make_integer(sz);
      ca[1] = (Scheme_Object *)cmap;
      ca[2] = SCHEME_VEC_ELS(vec)[0];
      ca[3] = (Scheme_Object *)boxmap;
      
      lifted = scheme_make_raw_pair(SCHEME_CAR(lifted), (Scheme_Object *)ca);
    }

    resolve_info_add_mapping(new_info, p, lifted ? 0 : offset++, flags, lifted);
  }
  if (has_tl) {
    if (convert)
      offset = 0; /* other closure elements converted to arguments */
    else
      offset = closure_size - 1;
    resolve_info_set_toplevel_pos(new_info, offset);
  }

  if (!just_compute_lift)
    data->closure_map = closure_map;

  new_info->in_proc = 1;

  if (!just_compute_lift) {
    Scheme_Object *code;
    code = scheme_resolve_expr(data->code, new_info);
    data->code = code;

    data->max_let_depth = (new_info->max_let_depth
                           + num_params
                           + closure_size
                           + convert_size
                           + SCHEME_TAIL_COPY_THRESHOLD);

    data->tl_map = new_info->tl_map;
    if (!data->tl_map && has_tl) {
      /* Our reason to refer to the top level has apparently gone away;
         record that we're not using anything */
      data->tl_map = (void *)0x1;
    }

    /* Add code to box set!ed argument variables: */
    for (i = 0; i < num_params; i++) {
      if (cl->local_flags[i] & SCHEME_WAS_SET_BANGED) {
        int j = i + closure_size + convert_size;
        Scheme_Object *bcode;
        
        bcode = scheme_alloc_object();
        bcode->type = scheme_boxenv_type;
        SCHEME_PTR1_VAL(bcode) = scheme_make_integer(j);
        SCHEME_PTR2_VAL(bcode) = data->code;

        data->code = bcode;
      }
    }
  }

  if ((closure_size == 1)
      && can_lift
      && has_tl
      && info->lifts) {
    need_lift = 1;
  } else
    need_lift = 0;

  if (convert) {
    num_params += convert_size;
    if (!just_compute_lift)
      data->num_params = num_params;
  }

  /* If the closure is empty, create the closure now */
  if (!closure_size) {
    if (precomputed_lift) {
      result = SCHEME_CAR(precomputed_lift);
      if (!just_compute_lift)
        ((Scheme_Closure *)result)->code = data;
    } else {
      if (just_compute_lift)
        result = (Scheme_Object *)scheme_malloc_empty_closure();
      else
        result = scheme_make_closure(NULL, (Scheme_Object *)data, 0);
    }
  } else
    result = (Scheme_Object *)data;
  
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
  } else {
    merge_resolve_tl_map(info, new_info);
  }
  
  if (convert) {
    Scheme_Object **ca, *arity;

    if ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST)) {
      arity = scheme_box(scheme_make_integer(num_params - convert_size - 1));
    } else {
      arity = scheme_make_integer(num_params - convert_size);
    }

    ca = MALLOC_N(Scheme_Object *, 4);
    ca[0] = scheme_make_integer(convert_size);
    ca[1] = (Scheme_Object *)convert_map;
    ca[2] = arity;
    ca[3] = (Scheme_Object *)convert_boxes;

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

  rp = scheme_resolve_prefix(0, m->comp_prefix, 1);
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
  case scheme_local_type:
    {
      int pos, flags;
      Scheme_Object *lifted;
      
      pos = resolve_info_lookup(info, SCHEME_LOCAL_POS(expr), &flags, &lifted, 0);
      if (lifted) {
        /* Lexical reference replaced with top-level reference for a lifted value: */
        return lifted;
      } else {
        return scheme_make_local((flags & SCHEME_INFO_BOXED) 
                                 ? scheme_local_unbox_type
                                 : scheme_local_type,
                                 pos,
                                 ((flags & SCHEME_INFO_TYPED_VAL_MASK) 
                                  ? (SCHEME_LOCAL_TYPE_OFFSET
                                     + ((flags & SCHEME_INFO_TYPED_VAL_MASK)
                                        >> SCHEME_INFO_TYPED_VAL_SHIFT))
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
  case scheme_compiled_unclosed_procedure_type:
    return resolve_closure_compilation(expr, info, 1, 0, 0, NULL);
  case scheme_compiled_let_void_type:
    return scheme_resolve_lets(expr, info);
  case scheme_compiled_toplevel_type:
    return resolve_toplevel(info, expr, 1);
  case scheme_compiled_quote_syntax_type:
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

/*========================================================================*/
/*                    compile-time env for resolve                        */
/*========================================================================*/

Resolve_Prefix *scheme_resolve_prefix(int phase, Comp_Prefix *cp, int simplify)
{
  Resolve_Prefix *rp;
  Scheme_Object **tls, **stxes, *simplify_cache, *m;
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

  if (simplify)
    simplify_cache = scheme_new_stx_simplify_cache();
  else
    simplify_cache = NULL;  

  ht = cp->stxes;
  if (ht) {
    for (i = 0; i < ht->size; i++) {
      if (ht->vals[i]) {
	scheme_simplify_stx(ht->keys[i], simplify_cache);
	stxes[SCHEME_LOCAL_POS(ht->vals[i])] = ht->keys[i];
      }
    }
  }

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
  naya->count = 0;
  naya->next = NULL;
  naya->toplevel_pos = -1;

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

static Resolve_Info *resolve_info_extend(Resolve_Info *info, int size, int oldsize, int mapc)
     /* size = number of appended items in run-time frame */
     /* oldisze = number of appended items in original compile-time frame */
     /* mapc = mappings that will be installed */
{
  Resolve_Info *naya;

  naya = MALLOC_ONE_RT(Resolve_Info);
#ifdef MZTAG_REQUIRED
  naya->type = scheme_rt_resolve_info;
#endif
  naya->prefix = info->prefix;
  naya->stx_map = info->stx_map;
  naya->next = info;
  naya->use_jit = info->use_jit;
  naya->enforce_const = info->enforce_const;
  naya->size = size;
  naya->oldsize = oldsize;
  naya->count = mapc;
  naya->pos = 0;
  naya->toplevel_pos = -1;
  naya->lifts = info->lifts;
  naya->no_lift = info->no_lift;

  if (mapc) {
    int i, *ia;
    mzshort *sa;

    sa = MALLOC_N_ATOMIC(mzshort, mapc);
    naya->old_pos = sa;
    sa = MALLOC_N_ATOMIC(mzshort, mapc);
    naya->new_pos = sa;
    ia = MALLOC_N_ATOMIC(int, mapc);
    naya->flags = ia;

    for (i = mapc; i--; ) {
      naya->old_pos[i] = 0;
      naya->new_pos[i] = 0;
      naya->flags[i] = 0;
    }
  }

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
    ((int *)tl_map)[1 + (tl_pos / 32)] |= (1 << (tl_pos & 31));
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

static void merge_resolve_tl_map(Resolve_Info *info, Resolve_Info *new_info)
{
  if (!new_info->tl_map) {
    /* nothing to do */
  } else {
    void *tl_map;
    tl_map = merge_tl_map(info->tl_map, new_info->tl_map);
    info->tl_map = tl_map;
  }
}

static void resolve_info_add_mapping(Resolve_Info *info, int oldp, int newp, int flags, Scheme_Object *lifted)
{
  if (info->pos == info->count) {
    scheme_signal_error("internal error: add_mapping: "
			"too many: %d", info->pos);
  }

  info->old_pos[info->pos] = oldp;
  info->new_pos[info->pos] = newp;
  info->flags[info->pos] = flags;
  if (lifted) {
    if (!info->lifted) {
      Scheme_Object **lifteds;
      lifteds = MALLOC_N(Scheme_Object*, info->count);
      info->lifted = lifteds;
    }
    info->lifted[info->pos] = lifted;
  }
  
  info->pos++;
}

static void resolve_info_adjust_mapping(Resolve_Info *info, int oldp, int newp, int flags, Scheme_Object *lifted)
{
  int i;

  for (i = info->pos; i--; ) {
    if (info->old_pos[i] == oldp) {
      info->new_pos[i] = newp;
      info->flags[i] = flags;
      if (lifted) {
        info->lifted[i] = lifted;
      }
      return;
    }
  }
      
  scheme_signal_error("internal error: adjust_mapping: "
                      "couldn't find: %d", oldp);
}

static void resolve_info_set_toplevel_pos(Resolve_Info *info, int pos)
{
  info->toplevel_pos = pos;
}

static int do_resolve_info_lookup(Resolve_Info *info, int pos, int *flags, Scheme_Object **_lifted, int convert_shift)
{
  Resolve_Info *orig_info = info;
  int i, offset = 0, orig = pos;

  if (_lifted)
    *_lifted = NULL;

  while (info) {
    for (i = info->pos; i--; ) {
      int oldp = info->old_pos[i];
      if (pos == oldp) {
	if (flags)
	  *flags = info->flags[i];
        if (info->lifted && (info->lifted[i])) {
          int skip, shifted;
          Scheme_Object *lifted, *tl, **ca;

          if (!_lifted)
            scheme_signal_error("unexpected lifted binding");

          lifted = info->lifted[i];

          if (SCHEME_RPAIRP(lifted)) {
            tl = SCHEME_CAR(lifted);
            ca = (Scheme_Object **)SCHEME_CDR(lifted);
            if (convert_shift)
              shifted = (int)SCHEME_INT_VAL(ca[0]) + convert_shift - 1;
            else
              shifted = 0;
          } else {
            tl = lifted;
            shifted = 0;
            ca = NULL;
          }

          if (SAME_TYPE(SCHEME_TYPE(tl), scheme_toplevel_type)) {
            skip = resolve_toplevel_pos(orig_info);
            tl = scheme_make_toplevel(skip + shifted, 
                                      SCHEME_TOPLEVEL_POS(tl),
                                      1,
                                      SCHEME_TOPLEVEL_CONST);

            /* register if non-stub: */
            if (SCHEME_TOPLEVEL_POS(tl) >= (info->prefix->num_toplevels
                                            + info->prefix->num_stxes
                                            + (info->prefix->num_stxes
                                               ? 1
                                               : 0)))
              set_tl_pos_used(orig_info, SCHEME_TOPLEVEL_POS(tl));
          }

          if (SCHEME_RPAIRP(lifted)) {
            int sz, i;
            mzshort *posmap, *boxmap;
            Scheme_Object *vec, *loc;
            sz = (int)SCHEME_INT_VAL(ca[0]);
            posmap = (mzshort *)ca[1];
            boxmap = (mzshort *)ca[3];
            vec = scheme_make_vector(sz + 1, NULL);
            for (i = 0; i < sz; i++) {
              int boxed = 0, local_typed = 0, flags = 0;

              if (boxmap) {
                int lt;
                lt = scheme_boxmap_get(boxmap, i, 0);
                if (lt == CLOS_TYPE_BOXED) {
                  boxed = 1;
                } else if (lt) {
                  local_typed = lt;
                  flags = ((lt - CLOS_TYPE_TYPE_OFFSET) + SCHEME_LOCAL_TYPE_OFFSET);
                }
              }
              
              loc = scheme_make_local(scheme_local_type,
                                      posmap[i] + offset + shifted,
                                      flags);
              
              if (boxed)
                loc = scheme_box(loc);
              else if (local_typed) {
                loc = scheme_make_vector(2, loc);
                SCHEME_VEC_ELS(loc)[1] = scheme_make_integer(local_typed);
              }
              
              SCHEME_VEC_ELS(vec)[i+1] = loc;
            }
            SCHEME_VEC_ELS(vec)[0] = ca[2];
            lifted = scheme_make_raw_pair(tl, vec);
          } else
            lifted = tl;
          
          *_lifted = lifted;
           
           return 0;
        } else {
          pos = info->new_pos[i];
          if (pos < 0)
            scheme_signal_error("internal error: skipped binding is used");
          return pos + offset;
        }
      }
    }

    if (info->in_proc) {
      scheme_signal_error("internal error: resolve_info_lookup: "
                          "searching past procedure");
    }

    pos -= info->oldsize;
    offset += info->size;
    info = info->next;
  }

  scheme_signal_error("internal error: resolve_info_lookup: "
		      "variable %d not found", orig);

  return 0;
}

static Scheme_Object *resolve_generate_stub_lift()
{
  return scheme_make_toplevel(0, 0, 1, SCHEME_TOPLEVEL_CONST);
}

static int resolve_info_flags(Resolve_Info *info, int pos, Scheme_Object **lifted)
{
  int flags;

  do_resolve_info_lookup(info, pos, &flags, lifted, 0);

  return flags;
}

static int resolve_info_lookup(Resolve_Info *info, int pos, int *flags, Scheme_Object **lifted, int convert_shift)
{
  return do_resolve_info_lookup(info, pos, flags, lifted, convert_shift);
}

static int resolve_toplevel_pos(Resolve_Info *info)
{
  int pos = 0;

  while (info && (info->toplevel_pos < 0)) {
    if (info->in_proc) {
      scheme_signal_error("internal error: resolve_toplevel_pos: "
                          "searching past procedure");
    }
    pos += info->size;
    info = info->next;
  }

  if (!info)
    return pos;
  else
    return info->toplevel_pos + pos;
}

static int resolve_is_inside_proc(Resolve_Info *info)
{
  while (info) {
    if (info->in_proc)
      return 1;
    info = info->next;
  }

  return 0;
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

static int resolving_in_procedure(Resolve_Info *info)
{
  while (info) {
    if (info->in_proc)
      return 1;
    info = info->next;
  }
  return 0;
}

/*========================================================================*/
/*                             uresolve                                   */
/*========================================================================*/

#if 0
# define return_NULL return (printf("%d\n", __LINE__), NULL)
#else
# define return_NULL return NULL
#endif

typedef struct Unresolve_Info {
  MZTAG_IF_REQUIRED
  int stack_pos; /* stack in resolved coordinates */
  int depth;     /* stack in unresolved coordinates */
  int stack_size;
  int *flags;
  mzshort *depths;
  Scheme_Prefix *prefix;
  Scheme_Hash_Table *closures; /* handle cycles */
  int has_non_leaf, body_size;
} Unresolve_Info;

static Scheme_Object *unresolve_expr(Scheme_Object *e, Unresolve_Info *ui, int as_rator);

static Unresolve_Info *new_unresolve_info(Scheme_Prefix *prefix)
{
  Unresolve_Info *ui;
  int *f, *d;

  ui = MALLOC_ONE_RT(Unresolve_Info);
  SET_REQUIRED_TAG(ui->type = scheme_rt_unresolve_info);

  ui->stack_pos = 0;
  ui->stack_size = 10;
  f = (int *)scheme_malloc_atomic(sizeof(int) * ui->stack_size);
  ui->flags = f;
  d = (mzshort *)scheme_malloc_atomic(sizeof(mzshort) * ui->stack_size);
  ui->depths = d;

  return ui;
}

static int unresolve_stack_push(Unresolve_Info *ui, int n, int r_only)
{
  int pos, *f, i;
  mzshort *d;

  pos = ui->stack_pos;

  if (pos + n > ui->stack_size) {
    f = (int *)scheme_malloc_atomic(sizeof(int) * ((2 * ui->stack_size) + n));
    memcpy(f, ui->flags, sizeof(int) * pos);
    
    d = (mzshort *)scheme_malloc_atomic(sizeof(mzshort) * ((2 * ui->stack_size) + n));
    memcpy(d, ui->depths, sizeof(mzshort) * pos);

    ui->flags = f;
    ui->depths = d;

    ui->stack_size = (2 * ui->stack_size) + n;
  }
  memset(ui->flags + pos, 0, sizeof(int) * n);
  if (!r_only) {
    for (i = 0; i < n; i++) {
      ui->depths[pos + i] = ui->depth++;
    }
  }

  ui->stack_pos += n;

  return pos;
}

static int *unresolve_stack_pop(Unresolve_Info *ui, int pos, int n)
{
  int *f, i;

  ui->stack_pos = pos;

  if (n) {
    f = (int *)scheme_malloc_atomic(sizeof(int) * n);
    for (i = 0; i < n; i++) {
      f[i] = ui->flags[pos + (n - i - 1)];
    }
    ui->depth -= n;
  } else
    f = NULL;

  return f;
}

XFORM_NONGCING static int combine_flags(int a, int b)
{
  int ac, bc;

  /* We don't currently try to support SCHEME_WAS_APPLIED_EXCEPT_ONCE,
     since that's to detect ((letrec ([f ....]) f) ....) patterns
     that would have been converted away already for code to inline
     across a module boundary. We do need to track SCHEME_WAS_ONLY_APPLIED,
     so that the resolver can ultimately lift expressions. */

  if ((b & SCHEME_WAS_ONLY_APPLIED) && !(a & SCHEME_WAS_ONLY_APPLIED)) {
    bc = b;
    b = a;
    a = bc;
  }

  if (a & SCHEME_WAS_ONLY_APPLIED) {
    if ((b & SCHEME_WAS_USED) && !(b & SCHEME_WAS_ONLY_APPLIED))
      a -= SCHEME_WAS_ONLY_APPLIED;
  }

  ac = (a & SCHEME_USE_COUNT_MASK) >> SCHEME_USE_COUNT_SHIFT;
  bc = (b & SCHEME_USE_COUNT_MASK) >> SCHEME_USE_COUNT_SHIFT;

  ac += bc;
  if (ac > SCHEME_USE_COUNT_INF)
    ac = SCHEME_USE_COUNT_INF;

  a |= b;
  a = (a - (a & SCHEME_USE_COUNT_MASK)) | (ac << SCHEME_USE_COUNT_SHIFT);

  return a;
}

static int unresolve_set_flag(Unresolve_Info *ui, int pos, int flag)
{
  int old_flag, i = ui->stack_pos - pos - 1;

  if ((pos < 0) || (pos >= ui->stack_pos))
    scheme_signal_error("internal error: unresolve out of bounds");

  old_flag = ui->flags[i];
  flag = combine_flags(flag | (1 << SCHEME_USE_COUNT_SHIFT), old_flag);
  ui->flags[i] = flag;

  return ui->depth - ui->depths[i] - 1;
}

Scheme_Object *unresolve_closure(Scheme_Closure_Data *rdata, Unresolve_Info *ui)
{
  Scheme_Closure_Data *data;
  Scheme_Object *body;
  Closure_Info *cl;
  int i, pos, data_pos, *flags, init_size, has_non_leaf;

  scheme_delay_load_closure(rdata);

  if (rdata->closure_size) {
    for (i = rdata->closure_size; i--; ) {
      if (rdata->closure_map[i] > ui->stack_pos)
        return_NULL; /* needs something (perhaps prefix) beyond known stack */
    }
  }

  data  = MALLOC_ONE_TAGGED(Scheme_Closure_Data);
  
  data->iso.so.type = scheme_compiled_unclosed_procedure_type;

  SCHEME_CLOSURE_DATA_FLAGS(data) = (SCHEME_CLOSURE_DATA_FLAGS(rdata) 
                                     & (CLOS_HAS_REST | CLOS_IS_METHOD));

  data->num_params = rdata->num_params;
  data->name = rdata->name;

  pos = unresolve_stack_push(ui, data->num_params, 0);

  if (rdata->closure_size) {
    data_pos = unresolve_stack_push(ui, rdata->closure_size, 1);
    /* remap closure slots: */
    for (i = rdata->closure_size; i--; ) {
      int mp;
      mp = ui->depths[pos - rdata->closure_map[i] - 1];
      ui->depths[ui->stack_pos - i - 1] = mp;
    }
  } else
    data_pos = 0;

  init_size = ui->body_size;
  has_non_leaf = ui->has_non_leaf;
  ui->has_non_leaf = 0;

  body = unresolve_expr(rdata->code, ui, 0);
  if (!body) return_NULL;

  data->code = body;

  cl = MALLOC_ONE_RT(Closure_Info);
  SET_REQUIRED_TAG(cl->type = scheme_rt_closure_info);
  data->closure_map = (mzshort *)cl;

  cl->body_size = (ui->body_size - init_size);
  cl->has_nonleaf = ui->has_non_leaf;

  ui->has_non_leaf = has_non_leaf;

  if (rdata->closure_size) {
    /* copy flags from unpacked closure to original slots */
    for (i = rdata->closure_size; i--; ) {
      int a, b;
      a = ui->flags[pos - rdata->closure_map[i] - 1];
      b = ui->flags[ui->stack_pos - i - 1];
      a = combine_flags(a, b);
      ui->flags[pos - rdata->closure_map[i] - 1] = a;
    }
    (void)unresolve_stack_pop(ui, data_pos, 0);
  }

  flags = unresolve_stack_pop(ui, pos, data->num_params);
  cl->local_flags = flags;

  return (Scheme_Object *)data;
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

static void check_nonleaf_rator(Scheme_Object *rator, Unresolve_Info *ui)
{
  if (!scheme_check_leaf_rator(rator, NULL))
    ui->has_non_leaf = 1;
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
    return scheme_make_local(scheme_local_type,
                             unresolve_set_flag(ui, 
                                                SCHEME_LOCAL_POS(e), 
                                                (SCHEME_WAS_USED
                                                 | (as_rator 
                                                    ? SCHEME_WAS_ONLY_APPLIED
                                                    : 0))),
                             0);
  case scheme_local_unbox_type:
    return scheme_make_local(scheme_local_type,
                             unresolve_set_flag(ui, SCHEME_LOCAL_POS(e), 
                                                (SCHEME_WAS_SET_BANGED | SCHEME_WAS_USED)),
                             0);
  case scheme_sequence_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)e, *seq2;
      int i;

      seq2 = scheme_malloc_sequence(seq->count);
      seq2->so.type = scheme_sequence_type;
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

      pos = unresolve_stack_push(ui, app->num_args, 1);

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

      pos = unresolve_stack_push(ui, 1, 1);

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

      pos = unresolve_stack_push(ui, 2, 1);

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
  case scheme_let_void_type:
    {
      Scheme_Let_Void *lv = (Scheme_Let_Void *)e;

      if (SAME_TYPE(SCHEME_TYPE(lv->body), scheme_letrec_type)) {
        Scheme_Letrec *lr = (Scheme_Letrec *)lv->body;

        if (lv->count == lr->count) {
          Scheme_Let_Header *lh;
          Scheme_Compiled_Let_Value *clv, *prev = NULL;
          Scheme_Object *rhs, *body;
          int i, pos, *all_flags, *flags;

          lh = MALLOC_ONE_TAGGED(Scheme_Let_Header);
          lh->iso.so.type = scheme_compiled_let_void_type;
          lh->count = lv->count;
          lh->num_clauses = lv->count;
          SCHEME_LET_FLAGS(lh) += SCHEME_LET_RECURSIVE;

          pos = unresolve_stack_push(ui, lv->count, 0);

          for (i = lv->count; i--; ) {
            rhs = unresolve_expr(lr->procs[i], ui, 0);
            if (!rhs) return_NULL;

            clv = MALLOC_ONE_TAGGED(Scheme_Compiled_Let_Value);
            clv->iso.so.type = scheme_compiled_let_value_type;
            clv->count = 1;
            clv->position = i;
            clv->value = rhs;

            if (prev)
              prev->body = (Scheme_Object *)clv;
            else
              lh->body = (Scheme_Object *)clv;
            prev = clv;
          }

          body = unresolve_expr(lr->body, ui, 0);
          if (!body) return_NULL;
          if (prev)
            prev->body = body;
          else
            lh->body = body;

          all_flags = unresolve_stack_pop(ui, pos, lv->count);
          
          clv = (Scheme_Compiled_Let_Value *)lh->body;
          for (i = lv->count; i--; ) {
            flags = (int *)scheme_malloc_atomic(sizeof(int));
            flags[0] = all_flags[i];
            clv->flags = flags;
            clv = (Scheme_Compiled_Let_Value *)clv->body;
          }
      
          return (Scheme_Object *)lh;
        }
      }

      return_NULL;
    }
  case scheme_let_one_type:
    {
      Scheme_Let_One *lo = (Scheme_Let_One *)e;
      Scheme_Object *rhs, *body;
      Scheme_Let_Header *lh;
      Scheme_Compiled_Let_Value *clv;
      int *flags, pos;

      pos = unresolve_stack_push(ui, 1, 1 /* => post-bind RHS */);
      rhs = unresolve_expr(lo->value, ui, 0);
      if (!rhs) return_NULL;
      (void)unresolve_stack_pop(ui, pos, 0);

      pos = unresolve_stack_push(ui, 1, 0);
      body = unresolve_expr(lo->body, ui, 0);
      if (!body) return_NULL;
      flags = unresolve_stack_pop(ui, pos, 1);

      lh = MALLOC_ONE_TAGGED(Scheme_Let_Header);
      lh->iso.so.type = scheme_compiled_let_void_type;
      lh->count = 1;
      lh->num_clauses = 1;

      clv = MALLOC_ONE_TAGGED(Scheme_Compiled_Let_Value);
      clv->iso.so.type = scheme_compiled_let_value_type;
      clv->count = 1;
      clv->position = 0;
      clv->value = rhs;
      clv->flags = flags;
      clv->body = body;

      lh->body = (Scheme_Object *)clv;

      return (Scheme_Object *)lh;
    }
  case scheme_closure_type:
    {
      Scheme_Object *r;

      if (!ui->closures) {
        Scheme_Hash_Table *ht;
        ht = scheme_make_hash_table(SCHEME_hash_ptr);
        ui->closures = ht;
      }
      if (scheme_hash_get(ui->closures, e))
        return_NULL; /* can't handle cyclic closures */

      scheme_hash_set(ui->closures, e, scheme_true);

      r = unresolve_closure(SCHEME_COMPILED_CLOS_CODE(e), ui);

      scheme_hash_set(ui->closures, e, NULL);

      return r;
    }
  case scheme_unclosed_procedure_type:
    {
      return unresolve_closure((Scheme_Closure_Data *)e, ui);
    }
  default:
    if (SCHEME_TYPE(e) > _scheme_values_types_) {
      if (scheme_compiled_duplicate_ok(e, 1))
        return e;
    }
    return_NULL;
  }
}

Scheme_Object *scheme_unresolve(Scheme_Object *iv, int argc, int *_has_cases)
{
  Scheme_Object *o;
  Scheme_Closure_Data *data = NULL;

  o = SCHEME_VEC_ELS(iv)[1];

  if (SAME_TYPE(SCHEME_TYPE(o), scheme_closure_type))
    data = ((Scheme_Closure *)o)->code;
  else if (SAME_TYPE(SCHEME_TYPE(o), scheme_unclosed_procedure_type))
    data = (Scheme_Closure_Data *)o;
  else if (SAME_TYPE(SCHEME_TYPE(o), scheme_case_lambda_sequence_type)
           || SAME_TYPE(SCHEME_TYPE(o), scheme_case_closure_type)) {
    Scheme_Case_Lambda *seqin = (Scheme_Case_Lambda *)o;
    int i, cnt;
    cnt = seqin->count;
    if (cnt > 1) *_has_cases = 1;
    for (i = 0; i < cnt; i++) {
      if (SAME_TYPE(SCHEME_TYPE(seqin->array[i]), scheme_closure_type)) {
        /* An empty closure, created at compile time */
        data = ((Scheme_Closure *)seqin->array[i])->code;
      } else {
        data = (Scheme_Closure_Data *)seqin->array[i];
      }
      if ((!(SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST) 
           && (data->num_params == argc))
          || ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST)
              && (data->num_params - 1 <= argc)))
        break;
      else
        data = NULL;
    }
  } else
    data = NULL;

  if (!data)
    return_NULL;

  if (data->closure_size)
    return_NULL;

  /* convert an optimized & resolved closure back to compiled form: */
  return unresolve_closure(data, 
                           new_unresolve_info((Scheme_Prefix *)SCHEME_VEC_ELS(iv)[2]));
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
