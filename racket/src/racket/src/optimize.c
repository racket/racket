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

/* This file implements bytecode optimzation.

   See "eval.c" for an overview of compilation passes. */

#include "schpriv.h"
#include "schrunst.h"
#include "schmach.h"

#define cons(a,b) scheme_make_pair(a,b)

/* Controls for inlining algorithm: */
#define OPT_ESTIMATE_FUTURE_SIZES   1
#define OPT_DISCOURAGE_EARLY_INLINE 1
#define OPT_LIMIT_FUNCTION_RESIZE   0
#define OPT_BRANCH_ADDS_NO_SIZE     1
#define OPT_DELAY_GROUP_PROPAGATE   0

#define MAX_PROC_INLINE_SIZE 256
#define CROSS_MODULE_INLINE_SIZE 8

#define SCHEME_PRIM_IS_UNSAFE_NONMUTATING (SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL | SCHEME_PRIM_IS_UNSAFE_OMITABLE)

struct Optimize_Info
{
  MZTAG_IF_REQUIRED
  short flags;
  struct Optimize_Info *next;
  int original_frame, new_frame;
  Scheme_Object *consts;
  Comp_Prefix *cp;
  int init_kclock;

  /* Propagated up and down the chain: */
  int size;
  int vclock; /* virtual clock that ticks for a side effect or branch;
                 the clock is only compared between binding sites and
                 uses, so we can rewind the clock at a join after an
                 increment that models a branch (if the branch is not
                 taken or doesn't increment the clock) */
  int kclock; /* virtual clock that ticks for a potential continuation capture */
  int sclock; /* virtual clock that ticks when space consumption is potentially observed */
  int psize;
  short inline_fuel, shift_fuel;
  char letrec_not_twice, enforce_const, use_psize, has_nonleaf;
  Scheme_Hash_Table *top_level_consts;

  /* Set by expression optimization: */
  int single_result, preserves_marks; /* negative means "tentative", due to fixpoint in progress */
  int escapes; /* flag to signal that the expression allways escapes. When escapes is 1, it's assumed
                  that single_result and preserves_marks are also 1, and that it's not necesary to
                  use optimize_ignored before including the expression. */

  char **stat_dists; /* (pos, depth) => used? */
  int *sd_depths;
  int used_toplevel;
  char *use;

  int transitive_use_pos; /* set to pos + 1 when optimizing a letrec-bound procedure */
  mzshort **transitive_use;
  int *transitive_use_len;

  Scheme_Object *context; /* for logging */
  Scheme_Logger *logger;
  Scheme_Hash_Tree *types; /* maps position (from this frame) to predicate */
  int no_types;
};

typedef struct Optimize_Info_Sequence {
  int init_shift_fuel, min_shift_fuel;
} Optimize_Info_Sequence;

#define OPT_IS_MUTATED           0x1
#define OPT_ESCAPES_AFTER_K_TICK 0x2
#define OPT_LOCAL_TYPE_ARG_SHIFT 2
#define OPT_LOCAL_TYPE_VAL_SHIFT (OPT_LOCAL_TYPE_ARG_SHIFT + SCHEME_MAX_LOCAL_TYPE_BITS)

static char *get_closure_local_type_map(Scheme_Closure_Data *data, int arg_n, int *ok);
static void set_closure_local_type_map(Scheme_Closure_Data *data, char *local_type_map);
static void merge_closure_local_type_map(Scheme_Closure_Data *data1, Scheme_Closure_Data *data2);
static int closure_body_size(Scheme_Closure_Data *data, int check_assign,
                             Optimize_Info *info, int *is_leaf);
static int closure_has_top_level(Scheme_Closure_Data *data);
static int closure_argument_flags(Scheme_Closure_Data *data, int i);

static int wants_local_type_arguments(Scheme_Object *rator, int argpos);

static int optimize_info_is_ready(Optimize_Info *info, int pos);

static void optimize_propagate(Optimize_Info *info, int pos, Scheme_Object *value, int single_use);
static Scheme_Object *optimize_info_lookup(Optimize_Info *info, int pos, int *closure_offset, int *single_use,
                                           int once_used_ok, int context, int *potential_size, int *_mutated);
static Scheme_Object *optimize_info_mutated_lookup(Optimize_Info *info, int pos, int *is_mutated);
static void optimize_info_used_top(Optimize_Info *info);
static Scheme_Object *optimize_get_predicate(int pos, Optimize_Info *info);
static void add_type(Optimize_Info *info, int pos, Scheme_Object *pred);
static void merge_types(Optimize_Info *src_info, Optimize_Info *info, int delta);
static Scheme_Object *lookup_constant_proc(Optimize_Info *info, Scheme_Object *rand, int delta);

static void optimize_mutated(Optimize_Info *info, int pos);
static void optimize_produces_local_type(Optimize_Info *info, int pos, int ct);
static int produces_local_type(Scheme_Object *rator, int argc);
static Scheme_Object *optimize_reverse(Optimize_Info *info, int pos, int unless_mutated, int disrupt_single_use);
static int optimize_is_used(Optimize_Info *info, int pos);
static int optimize_any_uses(Optimize_Info *info, int start_pos, int end_pos);
static int optimize_is_mutated(Optimize_Info *info, int pos);
static int optimize_escapes_after_k_tick(Optimize_Info *info, int pos);
static int optimize_is_local_type_arg(Optimize_Info *info, int pos, int depth);
static int optimize_is_local_type_valued(Optimize_Info *info, int pos);
static int env_uses_toplevel(Optimize_Info *frame);
static void env_make_closure_map(Optimize_Info *frame, mzshort *size, mzshort **map);

static Optimize_Info *optimize_info_add_frame(Optimize_Info *info, int orig, int current, int flags);
static int optimize_info_get_shift(Optimize_Info *info, int pos);
static void optimize_info_done(Optimize_Info *info, Optimize_Info *parent);

static void optimize_info_seq_init(Optimize_Info *info, Optimize_Info_Sequence *info_seq);
static void optimize_info_seq_step(Optimize_Info *info, Optimize_Info_Sequence *info_seq);
static void optimize_info_seq_done(Optimize_Info *info, Optimize_Info_Sequence *info_seq);

static Scheme_Object *estimate_closure_size(Scheme_Object *e);
static Scheme_Object *no_potential_size(Scheme_Object *value);

static Scheme_Object *optimize_clone(int dup_ok, Scheme_Object *obj, Optimize_Info *info, int delta, int closure_depth);
static Scheme_Object *optimize_shift(Scheme_Object *obj, int delta, int after_depth);

static int relevant_predicate(Scheme_Object *pred);
static int single_valued_noncm_expression(Scheme_Object *expr, int fuel);
static Scheme_Object *optimize_ignored(Scheme_Object *e, Optimize_Info *info, int id_offset,
                                       int expected_vals, int maybe_omittable,
                                       int fuel);
static int movable_expression(Scheme_Object *expr, Optimize_Info *info, int delta,
                              int cross_lambda, int cross_k, int cross_s,
                              int check_space, int fuel);

#define ID_OMIT            0
#define NO_ID_OMIT         1
#define NO_MUTABLE_ID_OMIT -1

#define IS_COMPILED_PROC(vals_expr) (SAME_TYPE(SCHEME_TYPE(vals_expr), scheme_compiled_unclosed_procedure_type) \
                                     || SAME_TYPE(SCHEME_TYPE(vals_expr), scheme_case_lambda_sequence_type))

static int compiled_proc_body_size(Scheme_Object *o, int less_args);

typedef struct Scheme_Once_Used {
  Scheme_Object so;
  Scheme_Object *expr;
  int pos;
  int vclock;
  int kclock;
  int sclock;

  int used;
  int delta;
  int cross_lambda;
  Optimize_Info *info;

  struct Scheme_Once_Used *next;
} Scheme_Once_Used;

static Scheme_Once_Used *make_once_used(Scheme_Object *val, int pos,
                                        int vclock, int kclock, int sclock,
                                        Scheme_Once_Used *prev);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

void scheme_init_optimize()
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif
}

/*========================================================================*/
/*                                  utils                                 */
/*========================================================================*/

int scheme_is_functional_nonfailing_primitive(Scheme_Object *rator, int num_args, int expected_vals)
/* return 2 => results are a constant when arguments are constants */
{
  if (SCHEME_PRIMP(rator)
      && (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & (SCHEME_PRIM_IS_OMITABLE | SCHEME_PRIM_IS_UNSAFE_NONMUTATING))
      && (num_args >= ((Scheme_Primitive_Proc *)rator)->mina)
      && (num_args <= ((Scheme_Primitive_Proc *)rator)->mu.maxa)
      && ((expected_vals < 0)
          || ((expected_vals == 1) && !(SCHEME_PRIM_PROC_FLAGS(rator) & SCHEME_PRIM_IS_MULTI_RESULT))
          || (SAME_OBJ(scheme_values_func, rator)
              && (expected_vals == num_args)))) {
    if (SAME_OBJ(scheme_values_func, rator))
      return 2;
    return 1;
  } else
    return 0;
}

static Scheme_Object *get_struct_proc_shape(Scheme_Object *rator, Optimize_Info *info)
{
  Scheme_Object *c;

  if (info
      && (info->top_level_consts || info->cp->inline_variants)
      && SAME_TYPE(SCHEME_TYPE(rator), scheme_compiled_toplevel_type)) {
    int pos;
    pos = SCHEME_TOPLEVEL_POS(rator);
    c = NULL;
    if (info->top_level_consts)
      c = scheme_hash_get(info->top_level_consts, scheme_make_integer(pos));
    if (!c && info->cp->inline_variants)
      c = scheme_hash_get(info->cp->inline_variants, scheme_make_integer(pos));
    if (c && SAME_TYPE(SCHEME_TYPE(c), scheme_struct_proc_shape_type)) {
      return c;
    }
  }

  return NULL;
}

int scheme_is_struct_functional(Scheme_Object *rator, int num_args, Optimize_Info *info, int vals)
{
  Scheme_Object *c;

  if ((vals == 1) || (vals == -1)) {
    c = get_struct_proc_shape(rator, info);
    if (c) {
      int mode = (SCHEME_PROC_SHAPE_MODE(c) & STRUCT_PROC_SHAPE_MASK);
      int field_count = (SCHEME_PROC_SHAPE_MODE(c) >> STRUCT_PROC_SHAPE_SHIFT);
      if (((num_args == 1) && (mode == STRUCT_PROC_SHAPE_PRED))
          || ((num_args == field_count) && (mode == STRUCT_PROC_SHAPE_CONSTR))) {
        return 1;
      }
    }
  }

  return 0;
}

static void note_match(int actual, int expected, Optimize_Info *warn_info)
{
  if (!warn_info || (expected == -1))
    return;

  if (actual != expected) {
    scheme_log(warn_info->logger,
               SCHEME_LOG_WARNING,
               0,
               "warning%s: %d values produced when %d expected",
               scheme_optimize_context_to_string(warn_info->context),
               actual, expected);
  }
}

int scheme_omittable_expr(Scheme_Object *o, int vals, int fuel, int resolved,
                          Optimize_Info *opt_info, Optimize_Info *warn_info,
                          int min_id_depth, int id_offset, int no_id)
     /* Checks whether the bytecode `o' returns `vals' values with no
        side-effects and without pushing and using continuation marks.
        -1 for vals means that any return count is ok.
        Also used with fully resolved expression by `module' to check
        for "functional" bodies.
        If warn_info is supplied, complain when a mismatch is detected.
        The min_id_depth argument (plus id_ofset) indicates the minimum
        depth allowed for local-variable reference; use this to disallow
        access to the first N variables that represent bindings being set up,
        for example.
        The id_offset value indincates an offset for local variables relative
        to opt_info; id_offset is also implicitly added to min_id_depth.
        If no_id is NO_ID_OMIT (= 1), then an identifier doesn't count as omittable,
        unless the identifier is a consistent top-level; the no_id mode
        is used by the "compile" phase before letrec checks are inserted
        (where referencing a variable might raise an exception) and to
        imply the absence of a continuation-mark impersonator.
        If no_id is NO_MUTABLE_ID_OMIT (= -1), then an identifier doesn't
        count as omittable if it's mutable, because the expression may be reordered
        instead of omitted; opt_info must be provided to check mutability. */
{
  Scheme_Type vtype;

  /* FIXME: can overflow the stack */

 try_again:

  vtype = SCHEME_TYPE(o);

  if ((vtype > _scheme_compiled_values_types_)
      || ((vtype == scheme_local_type)
          && (no_id <= 0)
          && !(SCHEME_GET_LOCAL_FLAGS(o) == SCHEME_LOCAL_CLEAR_ON_READ)
          && (SCHEME_LOCAL_POS(o) >= (min_id_depth + id_offset))
          && (!no_id || !optimize_is_mutated(opt_info, SCHEME_LOCAL_POS(o) - id_offset)))
      || ((vtype == scheme_local_unbox_type)
          && !no_id
          && !(SCHEME_GET_LOCAL_FLAGS(o) == SCHEME_LOCAL_CLEAR_ON_READ)
          && (SCHEME_LOCAL_POS(o) >= (min_id_depth + id_offset)))
      || (vtype == scheme_unclosed_procedure_type)
      || (vtype == scheme_compiled_unclosed_procedure_type)
      || (vtype == scheme_inline_variant_type)
      || (vtype == scheme_case_lambda_sequence_type)
      || (vtype == scheme_quote_syntax_type)
      || (vtype == scheme_varref_form_type)
      || (vtype == scheme_compiled_quote_syntax_type)) {
    note_match(1, vals, warn_info);
    return ((vals == 1) || (vals < 0));
  }

  if (vtype == scheme_toplevel_type) {
    note_match(1, vals, warn_info);
    if (!no_id && resolved && ((vals == 1) || (vals < 0))) {
      if (SCHEME_TOPLEVEL_FLAGS(o) & SCHEME_TOPLEVEL_FLAGS_MASK)
        return 1;
      else
        return 0;
    }
  }

  if (vtype == scheme_compiled_toplevel_type) {
    note_match(1, vals, warn_info);
    if ((vals == 1) || (vals < 0)) {
      if (!no_id && ((SCHEME_TOPLEVEL_FLAGS(o) & SCHEME_TOPLEVEL_FLAGS_MASK) >= SCHEME_TOPLEVEL_READY))
        return 1;
      else if ((SCHEME_TOPLEVEL_FLAGS(o) & SCHEME_TOPLEVEL_FLAGS_MASK) >= SCHEME_TOPLEVEL_FIXED)
        return 1;
      else
        return 0;
    }
  }

  if (vtype == scheme_case_lambda_sequence_type) {
    note_match(1, vals, warn_info);
    return 1;
  }

  if (vtype == scheme_compiled_quote_syntax_type) {
    note_match(1, vals, warn_info);
    return ((vals == 1) || (vals < 0));
  }

  if (vtype == scheme_branch_type) {
    Scheme_Branch_Rec *b;
    b = (Scheme_Branch_Rec *)o;
    return (scheme_omittable_expr(b->test, 1, fuel - 1, resolved, opt_info, warn_info, min_id_depth, id_offset, no_id)
	    && scheme_omittable_expr(b->tbranch, vals, fuel - 1, resolved, opt_info, warn_info, min_id_depth, id_offset, no_id)
	    && scheme_omittable_expr(b->fbranch, vals, fuel - 1, resolved, opt_info, warn_info, min_id_depth, id_offset, no_id));
  }

#if 0
  /* We can't do this because a set! to a lexical is turned into
     a let_value_type! */
  if (vtype == scheme_let_value_type) {
    Scheme_Let_Value *lv = (Scheme_Let_Value *)o;
    return (scheme_omittable_expr(lv->value, lv->count, fuel - 1, resolved, opt_info, warn_info, min_id_depth, id_offset, no_id)
	    && scheme_omittable_expr(lv->body, vals, fuel - 1, resolved, opt_info, warn_info, min_id_depth, id_offset, no_id));
  }
#endif

  if (vtype == scheme_let_one_type) {
    Scheme_Let_One *lo = (Scheme_Let_One *)o;
    return (scheme_omittable_expr(lo->value, 1, fuel - 1, resolved, opt_info, warn_info, min_id_depth, id_offset + 1, no_id)
	    && scheme_omittable_expr(lo->body, vals, fuel - 1, resolved, opt_info, warn_info, min_id_depth, id_offset + 1, no_id));
  }

  if (vtype == scheme_let_void_type) {
    Scheme_Let_Void *lv = (Scheme_Let_Void *)o;
    /* recognize (letrec ([x <omittable>]) ...): */
    if (SAME_TYPE(SCHEME_TYPE(lv->body), scheme_let_value_type)) {
      Scheme_Let_Value *lv2 = (Scheme_Let_Value *)lv->body;
      if ((lv2->count == 1)
          && (lv2->position == 0)
          && scheme_omittable_expr(lv2->value, 1, fuel - 1, resolved, opt_info, warn_info,
                                   min_id_depth, id_offset + 1 + lv->count,
                                   no_id)) {
        o = lv2->body;
        id_offset += 1;
      } else
        o = lv->body;
    } else
      o = lv->body;
    id_offset += lv->count;
    goto try_again;
  }

  if (vtype == scheme_compiled_let_void_type) {
    /* recognize another (let ([x <omittable>]) ...) pattern: */
    Scheme_Let_Header *lh = (Scheme_Let_Header *)o;
    if ((lh->count == 1) && (lh->num_clauses == 1)) {
      if (SAME_TYPE(SCHEME_TYPE(lh->body), scheme_compiled_let_value_type)) {
        Scheme_Compiled_Let_Value *lv = (Scheme_Compiled_Let_Value *)lh->body;
        if (scheme_omittable_expr(lv->value, 1, fuel - 1, resolved, opt_info, warn_info, min_id_depth, id_offset + 1, no_id)) {
          o = lv->body;
          id_offset++;
          goto try_again;
        }
      }
    }
  }

  if (vtype == scheme_letrec_type) {
    o = ((Scheme_Letrec *)o)->body;
    goto try_again;
  }

  if (vtype == scheme_application_type) {
    Scheme_App_Rec *app = (Scheme_App_Rec *)o;
    
    if ((app->num_args >= 4) && (app->num_args <= 11)
        && SAME_OBJ(scheme_make_struct_type_proc, app->args[0])) {
      note_match(5, vals, warn_info);
    }
    
    if (scheme_is_functional_nonfailing_primitive(app->args[0], app->num_args, vals)
        || scheme_is_struct_functional(app->args[0], app->num_args, opt_info, vals)) {
      int i;
      for (i = app->num_args; i--; ) {
        if (!scheme_omittable_expr(app->args[i + 1], 1, fuel - 1, resolved, opt_info, warn_info,
                                   min_id_depth, id_offset + (resolved ? app->num_args : 0), no_id))
          return 0;
      }
      return 1;
    } else if (SCHEME_PRIMP(app->args[0])) {
      if (!(SCHEME_PRIM_PROC_FLAGS(app->args[0]) & SCHEME_PRIM_IS_MULTI_RESULT)) {
        note_match(1, vals, warn_info);
      } else if (SAME_OBJ(scheme_values_func, app->args[0])) {
        note_match(app->num_args, vals, warn_info);
      }
    }
  
    return 0;
  }

  if (vtype == scheme_application2_type) {
    Scheme_App2_Rec *app = (Scheme_App2_Rec *)o;
    if (scheme_is_functional_nonfailing_primitive(app->rator, 1, vals)
        || scheme_is_struct_functional(app->rator, 1, opt_info, vals)) {
      if (scheme_omittable_expr(app->rand, 1, fuel - 1, resolved, opt_info, warn_info,
                                min_id_depth, id_offset + (resolved ? 1 : 0), no_id))
        return 1;
    } else if (SCHEME_PRIMP(app->rator)) {
      if (!(SCHEME_PRIM_PROC_FLAGS(app->rator) & SCHEME_PRIM_IS_MULTI_RESULT)
          || SAME_OBJ(scheme_values_func, app->rator)) {
        note_match(1, vals, warn_info);
      }
    }
    return 0;
  }

  if (vtype == scheme_application3_type) {
    Scheme_App3_Rec *app = (Scheme_App3_Rec *)o;
    if (scheme_is_functional_nonfailing_primitive(app->rator, 2, vals)
        || scheme_is_struct_functional(app->rator, 2, opt_info, vals)) {
      if (scheme_omittable_expr(app->rand1, 1, fuel - 1, resolved, opt_info, warn_info,
                                min_id_depth, id_offset + (resolved ? 2 : 0), no_id)
          && scheme_omittable_expr(app->rand2, 1, fuel - 1, resolved, opt_info, warn_info,
                                   min_id_depth, id_offset + (resolved ? 2 : 0), no_id))
        return 1;
    } else if (SCHEME_PRIMP(app->rator)) {
      if (!(SCHEME_PRIM_PROC_FLAGS(app->rator) & SCHEME_PRIM_IS_MULTI_RESULT)) {
        note_match(1, vals, warn_info);
      } else if (SAME_OBJ(scheme_values_func, app->rator)) {
        note_match(2, vals, warn_info);
      }
    }
    return 0;
  }

  /* check for (set! x x) */
  if (vtype == scheme_set_bang_type) {
    Scheme_Set_Bang *sb = (Scheme_Set_Bang *)o;
    if (SAME_TYPE(scheme_local_type, SCHEME_TYPE(sb->var))
        && SAME_TYPE(scheme_local_type, SCHEME_TYPE(sb->val))
        && (SCHEME_LOCAL_POS(sb->var) == SCHEME_LOCAL_POS(sb->val)))
      return 1;
  }

  /* check for struct-type declaration: */
  {
    Scheme_Object *auto_e;
    int auto_e_depth;
    auto_e = scheme_is_simple_make_struct_type(o, vals, resolved, 0, &auto_e_depth, 
                                               NULL,
                                               (opt_info ? opt_info->top_level_consts : NULL),
                                               NULL, NULL, 0, NULL, NULL,
                                               5);
    if (auto_e) {
      if (scheme_omittable_expr(auto_e, 1, fuel - 1, resolved, opt_info, warn_info,
                                min_id_depth, id_offset + auto_e_depth, no_id))
        return 1;
    }
  }

  return 0;
}

static Scheme_Object *ensure_single_value(Scheme_Object *e)
{
  Scheme_App2_Rec *app2;

  app2 = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
  app2->iso.so.type = scheme_application2_type;
  app2->rator = scheme_values_func;
  app2->rand = e;
  SCHEME_APPN_FLAGS(app2) |= (APPN_FLAG_IMMED | APPN_FLAG_SFS_TAIL);
  
  return (Scheme_Object *)app2;
}

static Scheme_Object *do_make_discarding_sequence(Scheme_Object *e1, Scheme_Object *e2,
                                                  Optimize_Info *info, int id_offset,
                                                  int ignored, int rev)
/* Evaluate `e1` then `e2` (or opposite order if rev), and each must
   produce a single value. The result of `e1` is ignored and the
   result is `e2` --- except that `e2` is ignored, too, if
   `ignored`. */
{
  int e2_omit;

  e2_omit = scheme_omittable_expr(e2, 1, 5, 0, info, NULL, 0, id_offset, ID_OMIT);

  if (!e2_omit && !single_valued_noncm_expression(e2, 5))
    e2 = ensure_single_value(e2);
  
  if (scheme_omittable_expr(e1, 1, 5, 0, info, NULL, 0, id_offset, ID_OMIT))
    return e2;
  else if (single_valued_noncm_expression(e1, 5))
    e1 = optimize_ignored(e1, info, id_offset, 1, 0, 5);
  else
    e1 = ensure_single_value(optimize_ignored(e1, info, id_offset, 1, 0, 5));

  if (e2_omit && ignored)
    return e1;

  /* use `begin` instead of `begin0` if we can swap the order: */
  if (rev && movable_expression(e2, info, -id_offset, 0, 1, 1, 0, 50))
    rev = 0;

  return scheme_make_sequence_compilation(scheme_make_pair((rev ? e2 : e1),
                                                           scheme_make_pair((rev ? e1 : e2), scheme_null)),
                                          rev ? -1 : 1);
}

static Scheme_Object *make_discarding_sequence(Scheme_Object *e1, Scheme_Object *e2,
                                               Optimize_Info *info, int id_offset)
{
  return do_make_discarding_sequence(e1, e2, info, id_offset, 0, 0);
}

static Scheme_Object *make_discarding_reverse_sequence(Scheme_Object *e1, Scheme_Object *e2,
                                                       Optimize_Info *info, int id_offset)
{
  return do_make_discarding_sequence(e1, e2, info, id_offset, 0, 1);
}

static Scheme_Object *make_discarding_app_sequence(Scheme_App_Rec *appr, int result_pos, Scheme_Object *result,
                                                   Optimize_Info *info, int id_offset)
/* Generalize do_make_discarding_sequence() to a sequence of argument
   expressions, where `result_pos` is the position of the returned
   argument. If `result_pos` is -1, then all argument results will be
   ignored. If `result`, then it is used as the result after all
   arguments are evaluated.*/
{
  int i;
  Scheme_Object *e, *l = scheme_null;

  result_pos = result_pos + 1;
  if (result)
    l = scheme_make_pair(result, l);

  for (i = appr->num_args; i; i--) {
    e = appr->args[i];
    if (scheme_omittable_expr(e, 1, 5, 0, info, NULL, 0, 0, ID_OMIT)) {
      /* drop if not result pos */
    } else if (single_valued_noncm_expression(e, 5)) {
      if (i != result_pos) {
        l = scheme_make_pair(optimize_ignored(e, info, id_offset, 1, 0, 5), l);
      }
    } else if (i == result_pos) {
      e = ensure_single_value(e);
    } else if (i != result_pos) {
      e = ensure_single_value(optimize_ignored(e, info, id_offset, 1, 0, 5));
      l = scheme_make_pair(e, l);
    }

    if (i == result_pos) {
      if (SCHEME_NULLP(l)) {
        l = scheme_make_pair(e, scheme_null);
      } else {
        l = scheme_make_sequence_compilation(scheme_make_pair(e, l), -1);
        l = scheme_make_pair(l, scheme_null);
      }
    }
  }

  if (SCHEME_NULLP(l))
    return scheme_void;

  if (SCHEME_NULLP(SCHEME_CDR(l)))
    return SCHEME_CAR(l);

  return scheme_make_sequence_compilation(l, 1);
}

static Scheme_Object *optimize_ignored(Scheme_Object *e, Optimize_Info *info, int id_offset,
                                       int expected_vals, int maybe_omittable,
                                       int fuel)
/* Simplify an expression whose result will be ignored.  The
   `expected_vals` is 1 or -1. If `maybe_omittable`, the result can be
   NULL to dincate that it can be omitted. */
{
  if (maybe_omittable) {
    if (scheme_omittable_expr(e, expected_vals, 5, 0, info, NULL, 0, id_offset, ID_OMIT))
      return NULL;
  }

  if (fuel) {
    /* We could do a lot more here, but for now, we just avoid purely
       functional, always successful operations --- especially allocating ones. */
    switch (SCHEME_TYPE(e)) {
    case scheme_application2_type:
      {
        Scheme_App2_Rec *app = (Scheme_App2_Rec *)e;

        if (!SAME_OBJ(app->rator, scheme_values_func)) /* `values` is probably here to ensure a single result */
          if (scheme_is_functional_nonfailing_primitive(app->rator, 1, expected_vals))
            return do_make_discarding_sequence(app->rand, scheme_void, info, id_offset, 1, 0);
      }
      break;
    case scheme_application3_type:
      {
        Scheme_App3_Rec *app = (Scheme_App3_Rec *)e;

        if (scheme_is_functional_nonfailing_primitive(app->rator, 2, expected_vals))
          return do_make_discarding_sequence(app->rand1,
                                             do_make_discarding_sequence(app->rand2,
                                                                         scheme_void,
                                                                         info, id_offset,
                                                                         1, 0),
                                             info, id_offset,
                                             1, 0);
      }
      break;
    case scheme_application_type:
      {
        Scheme_App_Rec *app = (Scheme_App_Rec *)e;

        if (scheme_is_functional_nonfailing_primitive(app->args[0], app->num_args, expected_vals))
          return make_discarding_app_sequence(app, -1, NULL, info, id_offset);
      }
      break;
    }
  }

  return e;
}

static Scheme_Object *make_sequence_2(Scheme_Object *a, Scheme_Object *b)
{
  return scheme_make_sequence_compilation(scheme_make_pair(a, scheme_make_pair(b, scheme_null)), 1);
}

static Scheme_Object *make_discarding_first_sequence(Scheme_Object *e1, Scheme_Object *e2,
                                                     Optimize_Info *info, int id_offset)
/* Like make_discarding_sequence(), but second expression is not constrained to
   a single result. */
{
  e1 = optimize_ignored(e1, info, id_offset, 1, 1, 5);
  if (!e1)
    return e2;
  if (!single_valued_noncm_expression(e1, 5))
    e1 = ensure_single_value(e1);
  return make_sequence_2(e1, e2);
}

static Scheme_Object *make_application_2(Scheme_Object *a, Scheme_Object *b, Optimize_Info *info)
{
  return scheme_make_application(scheme_make_pair(a, scheme_make_pair(b, scheme_null)), info);
}

static Scheme_Object *replace_tail_inside(Scheme_Object *alt, Scheme_Object *inside, Scheme_Object *orig) {
  if (inside) {
    switch (SCHEME_TYPE(inside)) {
    case scheme_sequence_type:
      if (((Scheme_Sequence *)inside)->count)
        ((Scheme_Sequence *)inside)->array[((Scheme_Sequence *)inside)->count-1] = alt;
      else
        scheme_signal_error("internal error: strange inside replacement");
      break;
    case scheme_compiled_let_void_type:
      ((Scheme_Let_Header *)inside)->body = alt;
      break;
    case scheme_compiled_let_value_type:
      ((Scheme_Compiled_Let_Value *)inside)->body = alt;
      break;
    default:
      scheme_signal_error("internal error: strange inside replacement");
    }
    return orig;
  }
  return alt;
}

static void extract_tail_inside(Scheme_Object **_t2, Scheme_Object **_inside, int *_id_offset)
{
  while (1) {
    if (SAME_TYPE(SCHEME_TYPE(*_t2), scheme_compiled_let_void_type)) {
      Scheme_Let_Header *head = (Scheme_Let_Header *)*_t2;
      int i;
      *_inside = *_t2;
      *_t2 = head->body;
      *_id_offset += head->count;
      for (i = head->num_clauses; i--; ) {
        *_inside = *_t2;
        *_t2 = ((Scheme_Compiled_Let_Value *)*_t2)->body;
      }
    } else if (SAME_TYPE(SCHEME_TYPE(*_t2), scheme_sequence_type)) {
      Scheme_Sequence *seq = (Scheme_Sequence *)*_t2;
      if (seq->count) {
        *_inside = *_t2;
        *_t2 = seq->array[seq->count-1];
      } else
        break;
    } else
      break;
  }
}


static int is_inspector_call(Scheme_Object *a)
{
  if (SAME_TYPE(SCHEME_TYPE(a), scheme_application_type)) {
    Scheme_App_Rec *app = (Scheme_App_Rec *)a;
    if (!app->num_args
        && (SAME_OBJ(app->args[0], scheme_current_inspector_proc)
            || SAME_OBJ(app->args[0], scheme_make_inspector_proc)))
      return 1;
  }
  return 0;
}

static int is_proc_spec_proc(Scheme_Object *p)
{
  Scheme_Type vtype;

  if (SCHEME_PROCP(p)) {
    p = scheme_get_or_check_arity(p, -1);
    if (SCHEME_INTP(p)) {
      return (SCHEME_INT_VAL(p) >= 1);
    } else if (SCHEME_STRUCTP(p)
               && scheme_is_struct_instance(scheme_arity_at_least, p)) {
      p = ((Scheme_Structure *)p)->slots[0];
      if (SCHEME_INTP(p))
        return (SCHEME_INT_VAL(p) >= 1);
    }
    return 0;
  }

  vtype = SCHEME_TYPE(p);

  if (vtype == scheme_unclosed_procedure_type) {
    if (((Scheme_Closure_Data *)p)->num_params >= 1)
      return 1;
  }

  return 0;
}

static int is_local_ref(Scheme_Object *e, int p, int r)
{
  return (SAME_TYPE(SCHEME_TYPE(e), scheme_local_type)
          && (SCHEME_LOCAL_POS(e) >= p)
          && (SCHEME_LOCAL_POS(e) < (p + r)));
}

static int is_int_list(Scheme_Object *o, int up_to)
{
  if (SCHEME_PAIRP(o)) {
    char *s, quick[8];
    Scheme_Object *e;
    if (up_to <= 8)
      s = quick;
    else
      s = (char *)scheme_malloc_atomic(up_to);
    memset(s, 0, up_to);
    while (SCHEME_PAIRP(o)) {
      e = SCHEME_CAR(o);
      o = SCHEME_CDR(o);
      if (!SCHEME_INTP(e)
          || (SCHEME_INT_VAL(e) < 0)
          || (SCHEME_INT_VAL(e) > up_to)
          || s[SCHEME_INT_VAL(e)])
        return 0;
      s[SCHEME_INT_VAL(e)] = 1;
    }
  }

  return SCHEME_NULLP(o);
}

static int ok_proc_creator_args(Scheme_Object *rator, Scheme_Object *rand1, Scheme_Object *rand2, Scheme_Object *rand3,
                                int delta2, int field_count)
{
  if ((SAME_OBJ(rator, scheme_make_struct_field_accessor_proc)
       && is_local_ref(rand1, delta2+3, 1))
      || (SAME_OBJ(rator, scheme_make_struct_field_mutator_proc)
          && is_local_ref(rand1, delta2+4, 1))) {
    if (SCHEME_INTP(rand2)
        && (SCHEME_INT_VAL(rand2) >= 0)
        && (SCHEME_INT_VAL(rand2) < field_count)
        && (!rand3 || SCHEME_SYMBOLP(rand3))) {
      return 1;
    }
  }

  return 0;
}

static int is_values_with_accessors_and_mutators(Scheme_Object *e, int vals, int resolved, 
                                                 Simple_Stuct_Type_Info *_stinfo)
{
  if (SAME_TYPE(SCHEME_TYPE(e), scheme_application_type)) {
    Scheme_App_Rec *app = (Scheme_App_Rec *)e;
    int delta = (resolved ? app->num_args : 0);
    if (SAME_OBJ(app->args[0], scheme_values_func)
        && (app->num_args == vals)
        && (app->num_args >= 3)
        && is_local_ref(app->args[1], delta, 1)
        && is_local_ref(app->args[2], delta+1, 1)
        && is_local_ref(app->args[3], delta+2, 1)) {
      int i, num_gets = 0, num_sets = 0, normal_ops = 1;
      for (i = app->num_args; i > 3; i--) {
        if (is_local_ref(app->args[i], delta, 5)) {
          normal_ops = 0;
        } else if (SAME_TYPE(SCHEME_TYPE(app->args[i]), scheme_application_type)
                   && _stinfo->normal_ops && !_stinfo->indexed_ops) {
          Scheme_App_Rec *app3 = (Scheme_App_Rec *)app->args[i];
          int delta2 = delta + (resolved ? app3->num_args : 0);
          if (app3->num_args == 3) {
            if (!ok_proc_creator_args(app3->args[0], app3->args[1], app3->args[2], app3->args[3],
                                      delta2, _stinfo->field_count))
              break;
            if (SAME_OBJ(app3->args[0], scheme_make_struct_field_mutator_proc)) {
              if (num_gets) normal_ops = 0;
              num_sets++;
            } else
              num_gets++;
          } else
            break;
        } else if (SAME_TYPE(SCHEME_TYPE(app->args[i]), scheme_application3_type)
                   && _stinfo->normal_ops && !_stinfo->indexed_ops) {
          Scheme_App3_Rec *app3 = (Scheme_App3_Rec *)app->args[i];
          int delta2 = delta + (resolved ? 2 : 0);
          if (!ok_proc_creator_args(app3->rator, app3->rand1, app3->rand2, NULL,
                                    delta2, _stinfo->field_count))
            break;
          if (SAME_OBJ(app3->rator, scheme_make_struct_field_mutator_proc)) {
            if (num_gets) normal_ops = 0;
            num_sets++;
          } else
            num_gets++;
        } else
          break;
      }
      if (i <= 3) {
        _stinfo->normal_ops = normal_ops;
        _stinfo->indexed_ops = 1;
        _stinfo->num_gets = num_gets;
        _stinfo->num_sets = num_sets;
        return 1;
      }
    }
  }

  return 0;
}

static Scheme_Object *skip_clears(Scheme_Object *body)
{
  if (SAME_TYPE(SCHEME_TYPE(body), scheme_sequence_type)) {
    Scheme_Sequence *seq = (Scheme_Sequence *)body;
    int i;
    for (i = seq->count - 1; i--; ) {
      if (!SAME_TYPE(SCHEME_TYPE(seq->array[i]), scheme_local_type))
        break;
    }
    if (i < 0)
      return seq->array[seq->count-1];
  }
  return body;
}

static int is_constant_super(Scheme_Object *arg, 
                             Scheme_Hash_Table *top_level_consts, 
                             Scheme_Hash_Table *top_level_table,
                             Scheme_Object **runstack, int rs_delta,
                             Scheme_Object **symbols, Scheme_Hash_Table *symbol_table)
{
  int pos;
  Scheme_Object *v;

  if (SAME_TYPE(SCHEME_TYPE(arg), scheme_compiled_toplevel_type)) {
    pos = SCHEME_TOPLEVEL_POS(arg);
    if (top_level_consts) {
      /* This is optimize mode */
      v = scheme_hash_get(top_level_consts, scheme_make_integer(pos));
      if (v && SAME_TYPE(SCHEME_TYPE(v), scheme_struct_proc_shape_type)) {
        int mode = (SCHEME_PROC_SHAPE_MODE(v) & STRUCT_PROC_SHAPE_MASK);
        int field_count = (SCHEME_PROC_SHAPE_MODE(v) >> STRUCT_PROC_SHAPE_SHIFT);
        if (mode == STRUCT_PROC_SHAPE_STRUCT)
          return field_count + 1;
      }
    }
  } else if (SAME_TYPE(SCHEME_TYPE(arg), scheme_toplevel_type)) {
    pos = SCHEME_TOPLEVEL_POS(arg);
    if (runstack) {
      /* This is eval mode; conceptually, this code belongs in 
         define_execute_with_dynamic_state() */
      Scheme_Bucket *b;
      Scheme_Prefix *toplevels;
      toplevels = (Scheme_Prefix *)runstack[SCHEME_TOPLEVEL_DEPTH(arg) - rs_delta];
      b = (Scheme_Bucket *)toplevels->a[pos];
      if (b->val) {
        if (SCHEME_STRUCT_TYPEP(b->val)
            && (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_IS_CONSISTENT)) {
          Scheme_Struct_Type *st = (Scheme_Struct_Type *)b->val;
          if (st->num_slots == st->num_islots)
            return st->num_slots + 1;
        }
      }
    }
    if (symbols) {
      /* This is module-export mode; conceptually, this code belongs in 
         setup_accessible_table() */
      Scheme_Object *name;
      name = symbols[pos];
      if (SCHEME_SYMBOLP(name)) {
        v = scheme_hash_get(symbol_table, name);
        if (v && SCHEME_VECTORP(v) && (SCHEME_VEC_SIZE(v) == 3)) {
          v = SCHEME_VEC_ELS(v)[1];
          if (v && SCHEME_INTP(v)) {
            int mode = (SCHEME_INT_VAL(v) & STRUCT_PROC_SHAPE_MASK);
            int field_count = (SCHEME_INT_VAL(v) >> STRUCT_PROC_SHAPE_SHIFT);
            if (mode == STRUCT_PROC_SHAPE_STRUCT)
              return field_count + 1;
          }
        }
      } else if (SAME_TYPE(SCHEME_TYPE(name), scheme_module_variable_type)) {
        intptr_t k;
        if (scheme_decode_struct_shape(((Module_Variable *)name)->shape, &k)) {
          if ((k & STRUCT_PROC_SHAPE_MASK) == STRUCT_PROC_SHAPE_STRUCT)
            return (k >> STRUCT_PROC_SHAPE_SHIFT) + 1;
        }
      }
    }
    if (top_level_table) {
      /* This is validate mode; conceptually, this code belongs in 
         define_values_validate() */
      v = scheme_hash_get(top_level_table, scheme_make_integer(pos));
      if (v) {
        int k = SCHEME_INT_VAL(v);
        if ((k & STRUCT_PROC_SHAPE_MASK) == STRUCT_PROC_SHAPE_STRUCT)
          return (k >> STRUCT_PROC_SHAPE_SHIFT) + 1;
      }
    }
  }

  return 0;
}

Scheme_Object *scheme_is_simple_make_struct_type(Scheme_Object *e, int vals, int resolved, 
                                                 int check_auto, 
                                                 GC_CAN_IGNORE int *_auto_e_depth, 
                                                 Simple_Stuct_Type_Info *_stinfo,
                                                 Scheme_Hash_Table *top_level_consts, 
                                                 Scheme_Hash_Table *top_level_table,
                                                 Scheme_Object **runstack, int rs_delta,
                                                 Scheme_Object **symbols, Scheme_Hash_Table *symbol_table,
                                                 int fuel)
/* Checks whether it's a `make-struct-type' call that certainly succeeds 
   (i.e., no exception) --- pending a check of the auto-value argument if !check_auto.
   The result is the auto-value argument or scheme_true if it's simple, NULL if not. 
   The first result is a struct type, the second a constructor, and the thrd a predicate;
   the rest are an unspecified mixture of selectors and mutators. */
{
  if (!fuel) return NULL;

  if (SAME_TYPE(SCHEME_TYPE(e), scheme_application_type)) {
    if ((vals == 5) || (vals < 0)) {
      Scheme_App_Rec *app = (Scheme_App_Rec *)e;

      if ((app->num_args >= 4) && (app->num_args <= 11)
          && SAME_OBJ(scheme_make_struct_type_proc, app->args[0])) {
        int super_count_plus_one;

        if (!SCHEME_FALSEP(app->args[2]))
          super_count_plus_one = is_constant_super(app->args[2], 
                                                   top_level_consts, top_level_table, runstack,
                                                   rs_delta + app->num_args,
                                                   symbols, symbol_table);
        else
          super_count_plus_one = 0;

        if (SCHEME_SYMBOLP(app->args[1])
            && (SCHEME_FALSEP(app->args[2]) /* super */
                || super_count_plus_one)
            && SCHEME_INTP(app->args[3])
            && (SCHEME_INT_VAL(app->args[3]) >= 0)
            && SCHEME_INTP(app->args[4])
            && (SCHEME_INT_VAL(app->args[4]) >= 0)
            && ((app->num_args < 5)
                /* auto-field value: */
                || !check_auto
                || scheme_omittable_expr(app->args[5], 1, 3, resolved, NULL, NULL, 0, 0, ID_OMIT))
            && ((app->num_args < 6)
                /* no properties: */
                || SCHEME_NULLP(app->args[6]))
            && ((app->num_args < 7)
                /* inspector: */
                || SCHEME_FALSEP(app->args[7])
                || (SCHEME_SYMBOLP(app->args[7])
                    && !strcmp("prefab", SCHEME_SYM_VAL(app->args[7]))
                    && !SCHEME_SYM_WEIRDP(app->args[7]))
                || is_inspector_call(app->args[7]))
            && ((app->num_args < 8)
                /* propcedure property: */
                || SCHEME_FALSEP(app->args[8])
                || is_proc_spec_proc(app->args[8]))
            && ((app->num_args < 9)
                /* immutables: */
                || is_int_list(app->args[9],
                               SCHEME_INT_VAL(app->args[3])))
            && ((app->num_args < 10)
                /* guard: */
                || SCHEME_FALSEP(app->args[10]))
            && ((app->num_args < 11)
                /* constructor name: */
                || SCHEME_FALSEP(app->args[11])
                || SCHEME_SYMBOLP(app->args[11]))) {
          if (_auto_e_depth)
            *_auto_e_depth = (resolved ? app->num_args : 0);
          if (_stinfo) {
            int super_count = (super_count_plus_one 
                               ? (super_count_plus_one - 1)
                               : 0);
            _stinfo->init_field_count = SCHEME_INT_VAL(app->args[3]) + super_count;
            _stinfo->field_count = (SCHEME_INT_VAL(app->args[3]) 
                                    + SCHEME_INT_VAL(app->args[4])
                                    + super_count);
            _stinfo->uses_super = (super_count_plus_one ? 1 : 0);
            _stinfo->normal_ops = 1;
            _stinfo->indexed_ops = 0;
            _stinfo->num_gets = 1;
            _stinfo->num_sets = 1;
          }
          return ((app->num_args < 5) ? scheme_true : app->args[5]);
        }
      }
    }
  }

  if (SAME_TYPE(SCHEME_TYPE(e), scheme_compiled_let_void_type)) {
    /* check for (let-values ([(: mk ? ref- set-!) (make-struct-type ...)]) (values ...))
       as generated by the expansion of `struct' */
    Scheme_Let_Header *lh = (Scheme_Let_Header *)e;
    if ((lh->count == 5) && (lh->num_clauses == 1)) {
      if (SAME_TYPE(SCHEME_TYPE(lh->body), scheme_compiled_let_value_type)) {
        Scheme_Compiled_Let_Value *lv = (Scheme_Compiled_Let_Value *)lh->body;
        if (SAME_TYPE(SCHEME_TYPE(lv->value), scheme_application_type)) {
          Scheme_Object *auto_e;
          Simple_Stuct_Type_Info stinfo;
          int lh_delta = ((SCHEME_LET_FLAGS(lh) & (SCHEME_LET_RECURSIVE | SCHEME_LET_STAR))
                          ? lh->count
                          : 0);
          if (!_stinfo) _stinfo = &stinfo;
          auto_e = scheme_is_simple_make_struct_type(lv->value, 5, resolved, check_auto, 
                                                     _auto_e_depth, _stinfo, 
                                                     top_level_consts, top_level_table, 
                                                     runstack, rs_delta + lh_delta,
                                                     symbols, symbol_table,
                                                     fuel-1);
          if (auto_e) {
            /* We have (let-values ([... (make-struct-type)]) ....), so make sure body
               just uses `make-struct-field-{accessor,mutator}'. */
            if (is_values_with_accessors_and_mutators(lv->body, vals, resolved, _stinfo)) {
              if (_auto_e_depth && lh_delta)
                *_auto_e_depth += lh_delta;
              return auto_e;
            }
          }
        }
      }
    }
  }

  if (SAME_TYPE(SCHEME_TYPE(e), scheme_let_void_type)) {
    /* same thing, but in resolved form */
    Scheme_Let_Void *lvd = (Scheme_Let_Void *)e;
    if (lvd->count == 5) {
      if (SAME_TYPE(SCHEME_TYPE(lvd->body), scheme_let_value_type)) {
        Scheme_Let_Value *lv = (Scheme_Let_Value *)lvd->body;
        if ((lv->position == 0) && (lv->count == 5)) {
          Scheme_Object *e2;
          e2 = skip_clears(lv->value);
          if (SAME_TYPE(SCHEME_TYPE(e2), scheme_application_type)) {
            Scheme_Object *auto_e;
            Simple_Stuct_Type_Info stinfo;
            if (!_stinfo) _stinfo = &stinfo;
            auto_e = scheme_is_simple_make_struct_type(e2, 5, resolved, check_auto,
                                                       _auto_e_depth, _stinfo,
                                                       top_level_consts, top_level_table,
                                                       runstack, rs_delta + lvd->count,
                                                       symbols, symbol_table,
                                                       fuel-1);
            if (auto_e) {
              /* We have (let-values ([... (make-struct-type)]) ....), so make sure body
                 just uses `make-struct-field-{accessor,mutator}'. */
              e2 = skip_clears(lv->body);
              if (is_values_with_accessors_and_mutators(e2, vals, resolved, _stinfo)) {
                if (_auto_e_depth) *_auto_e_depth += lvd->count;
                return auto_e;
              }
            }
          }
        }
      }
    }
  }

  return NULL;
}

intptr_t scheme_get_struct_proc_shape(int k, Simple_Stuct_Type_Info *stinfo)
{
  switch (k) {
  case 0:
    if (stinfo->field_count == stinfo->init_field_count)
      return STRUCT_PROC_SHAPE_STRUCT | (stinfo->field_count << STRUCT_PROC_SHAPE_SHIFT);
    else
      return STRUCT_PROC_SHAPE_OTHER;
    break;
  case 1:
    return STRUCT_PROC_SHAPE_CONSTR | (stinfo->init_field_count << STRUCT_PROC_SHAPE_SHIFT);
    break;
  case 2:
    return STRUCT_PROC_SHAPE_PRED;
    break;
  default:
    if (stinfo && stinfo->normal_ops && stinfo->indexed_ops) {
      if (k - 3 < stinfo->num_gets)
        return STRUCT_PROC_SHAPE_GETTER | (stinfo->field_count << STRUCT_PROC_SHAPE_SHIFT);
      else
        return STRUCT_PROC_SHAPE_SETTER | (stinfo->field_count << STRUCT_PROC_SHAPE_SHIFT);
    }
  }

  return STRUCT_PROC_SHAPE_OTHER;
}

Scheme_Object *scheme_make_struct_proc_shape(intptr_t k)
{
  Scheme_Object *ps;

  ps = scheme_malloc_small_atomic_tagged(sizeof(Scheme_Small_Object));
  ps->type = scheme_struct_proc_shape_type;
  SCHEME_PROC_SHAPE_MODE(ps) = k;

  return ps;
}

static int single_valued_noncm_expression(Scheme_Object *expr, int fuel)
/* Not necessarily omittable or copyable, but single-valued expresions that are not sensitive
   to being in tail position. */
{
  Scheme_Object *rator = NULL;

 switch (SCHEME_TYPE(expr)) {
 case scheme_local_type:
   return 1;
 case scheme_compiled_toplevel_type:
   return 1;
 case scheme_application_type:
   rator = ((Scheme_App_Rec *)expr)->args[0];
   break;
 case scheme_application2_type:
   rator = ((Scheme_App2_Rec *)expr)->rator;
   break;
 case scheme_application3_type:
   rator = ((Scheme_App2_Rec *)expr)->rator;
   break;
 case scheme_compiled_let_void_type:
   {
     Scheme_Let_Header *lh = (Scheme_Let_Header *)expr;
     Scheme_Compiled_Let_Value *clv;
     if ((lh->count == 1) && (lh->num_clauses == 1) && (fuel > 0)) {
       clv = (Scheme_Compiled_Let_Value *)lh->body;
       return single_valued_noncm_expression(clv->body, fuel - 1);
     }
   }
   break;
 case scheme_branch_type:
   if (fuel > 0) {
     Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)expr;
     return (single_valued_noncm_expression(b->test, fuel - 1)
             && single_valued_noncm_expression(b->tbranch, fuel - 1)
             && single_valued_noncm_expression(b->fbranch, fuel - 1));
   }
   break;
 case scheme_compiled_unclosed_procedure_type:
 case scheme_case_lambda_sequence_type:
 case scheme_set_bang_type:
   return 1;
 default:
   if (SCHEME_TYPE(expr) > _scheme_compiled_values_types_)
     return 1;
   break;
 }

 if (rator && SCHEME_PRIMP(rator)) {
   int opt;
   opt = ((Scheme_Prim_Proc_Header *)rator)->flags & SCHEME_PRIM_OPT_MASK;
   if (opt >= SCHEME_PRIM_OPT_NONCM)
     return 1;
 }

 return 0;
}

static int is_movable_prim(Scheme_Object *rator, int n, int cross_lambda, int cross_k, Optimize_Info *info)
/* A -1 return means that the arguments must be movable without
   changing space complexity. */
{
  if (rator && SCHEME_PRIMP(rator)) {
    if (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL) {
      /* Although it's semantically ok to return -1 even when cross_lambda,
         doing so risks duplicating a computation if the relevant `lambda'
         is later inlined. */
      if (cross_lambda) return 0;
      if (cross_k
          && !(SCHEME_PRIM_PROC_OPT_FLAGS(rator) & SCHEME_PRIM_IS_UNSAFE_NONALLOCATE)
          && (produces_local_type(rator, n) != SCHEME_LOCAL_TYPE_FIXNUM)) {
        return 0;
      }
      return -1;
    }
  }

  if (SAME_OBJ(scheme_void_proc, rator))
    return -1;

  if (!cross_lambda
      && !cross_k /* because all calls below allocate */
      /* Note that none of these have space-safety issues, since they
         return values that contain all arguments: */
      && (SAME_OBJ(scheme_list_proc, rator)
          || (SAME_OBJ(scheme_cons_proc, rator) && (n == 2))
          || (SAME_OBJ(scheme_mcons_proc, rator) && (n == 2))
          || (SAME_OBJ(scheme_unsafe_cons_list_proc, rator) && (n == 2))
          || SAME_OBJ(scheme_list_star_proc, rator)
          || SAME_OBJ(scheme_vector_proc, rator)
          || SAME_OBJ(scheme_vector_immutable_proc, rator)
          || (SAME_OBJ(scheme_box_proc, rator) && (n == 1))
          || (SAME_OBJ(scheme_box_immutable_proc, rator) && (n == 1))))
    return 1;

  return 0;
}

static int movable_expression(Scheme_Object *expr, Optimize_Info *info, int delta, 
                              int cross_lambda, int cross_k, int cross_s,
                              int check_space, int fuel)
/* An expression that can't necessarily be constant-folded,
   but can be delayed because it has no side-effects (or is unsafe),
   produces a single value,
   and is not sensitive to being in tail position */
{
  int can_move;

  if (fuel < 0) return 0;

  switch (SCHEME_TYPE(expr)) {
  case scheme_toplevel_type:
    return ((SCHEME_TOPLEVEL_FLAGS(expr) & SCHEME_TOPLEVEL_FLAGS_MASK) >= SCHEME_TOPLEVEL_FIXED);
  case scheme_compiled_quote_syntax_type:
    return 1;
  case scheme_local_type:
    {
      /* Ok if not mutable */
      int pos = SCHEME_LOCAL_POS(expr);
      if (pos + delta < 0)
        return 0; /* assume non-movable */
      else if (!optimize_is_mutated(info, pos + delta)) {
        if (check_space) {
          if (optimize_is_local_type_valued(info, pos + delta))
            return 1;
          /* the value of the identifier might be something that would
             retain significant memory, so we can't delay evaluation */
          return 0;
        }
        return 1;
      }
    }
    break;
  case scheme_application_type:
    can_move = is_movable_prim(((Scheme_App_Rec *)expr)->args[0], ((Scheme_App_Rec *)expr)->num_args, 
                               cross_lambda, cross_k, info);
    if (can_move) {
      int i;
      for (i = ((Scheme_App_Rec *)expr)->num_args; i--; ) {
        if (!movable_expression(((Scheme_App_Rec *)expr)->args[i+1], info, delta,
                                cross_lambda, cross_k, cross_s,
                                check_space || (cross_s && (can_move < 0)), fuel - 1))
          return 0;
      }
      return 1;
    }
    break;
  case scheme_application2_type:
    can_move = is_movable_prim(((Scheme_App2_Rec *)expr)->rator, 1, cross_lambda, cross_k, info);
    if (can_move) {
      if (movable_expression(((Scheme_App2_Rec *)expr)->rand, info, delta,
                             cross_lambda, cross_k, cross_s,
                             check_space || (cross_s && (can_move < 0)), fuel - 1))
        return 1;
    }
    break;
  case scheme_application3_type:
    can_move = is_movable_prim(((Scheme_App3_Rec *)expr)->rator, 2, cross_lambda, cross_k, info);
    if (can_move) {
      if (movable_expression(((Scheme_App3_Rec *)expr)->rand1, info, delta,
                             cross_lambda, cross_k, cross_s,
                             check_space || (cross_s && (can_move < 0)), fuel - 1)
          && movable_expression(((Scheme_App3_Rec *)expr)->rand2, info, delta,
                                cross_lambda, cross_k, cross_s,
                                check_space || (cross_s && (can_move < 0)), fuel - 1))
        return 1;
    }
    break;
  case scheme_branch_type:
    {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)expr;
      if (movable_expression(b->test, info, delta, cross_lambda, cross_k, cross_s, check_space, fuel-1)
          && movable_expression(b->tbranch, info, delta, cross_lambda, cross_k, cross_s, check_space, fuel-1)
          && movable_expression(b->fbranch, info, delta, cross_lambda, cross_k, cross_s, check_space, fuel-1))
        return 1;
    }
    break;
  case scheme_compiled_unclosed_procedure_type:
  case scheme_case_lambda_sequence_type:
    /* Can't move across lambda or continuation if not closed, since
       that changes allocation of a closure. */
    return !cross_lambda && !cross_k;
  default:
    if (SCHEME_TYPE(expr) > _scheme_compiled_values_types_)
      return 1;
  }

  return 0;
}

int scheme_is_compiled_procedure(Scheme_Object *o, int can_be_closed, int can_be_liftable)
{
  if (SAME_TYPE(SCHEME_TYPE(o), scheme_compiled_unclosed_procedure_type)) {
    if (!can_be_closed || !can_be_liftable) {
      Scheme_Closure_Data *data;
      data = (Scheme_Closure_Data *)o;
      /* Because == 0 is like a constant */
      if (!can_be_closed && !data->closure_size)
        return 0;
      /* Because procs that reference only globals are lifted: */
      if (!can_be_liftable && (data->closure_size == 1) && closure_has_top_level(data))
        return 0;
    }
    return 1;
  } else
    return 0;
}

/*========================================================================*/
/*                   applications, branches, sequences                    */
/*========================================================================*/

static Scheme_Object *finish_optimize_application(Scheme_App_Rec *app, Optimize_Info *info, int context, int rator_flags);
static Scheme_Object *finish_optimize_application2(Scheme_App2_Rec *app, Optimize_Info *info, int context, int rator_flags);
static Scheme_Object *finish_optimize_application3(Scheme_App3_Rec *app, Optimize_Info *info, int context, int rator_flags);

static Scheme_Object *try_optimize_fold(Scheme_Object *f, Scheme_Object *args, Scheme_Object *o, Optimize_Info *info)
/* If `args` is NULL, extract arguments from `o` */
{
  if (scheme_is_foldable_prim(f)) {

    if (!args) {
      switch (SCHEME_TYPE(o)) {
      case scheme_application_type:
        {
          Scheme_App_Rec *app = (Scheme_App_Rec *)o;
          int i;

          args = scheme_null;
          for (i = app->num_args; i--; ) {
            args = scheme_make_pair(app->args[i + 1], args);
          }
        }
        break;
      case scheme_application2_type:
        {
          Scheme_App2_Rec *app = (Scheme_App2_Rec *)o;
          args = scheme_make_pair(app->rand, scheme_null);
        }
        break;
      case scheme_application3_type:
      default:
        {
          Scheme_App3_Rec *app = (Scheme_App3_Rec *)o;
          args = scheme_make_pair(app->rand1,
                                  scheme_make_pair(app->rand2,
                                                   scheme_null));
        }
        break;
      }
    }

    return scheme_try_apply(f, args, info);
  }

  return NULL;
}

static int estimate_expr_size(Scheme_Object *expr, int sz, int fuel)
{
  Scheme_Type t;

  if (sz > 128)
    return sz;
  if (fuel < 0)
    return sz + 128;

  t = SCHEME_TYPE(expr);

  switch(t) {
  case scheme_local_type:
    {
      sz += 1;
      break;
    }
  case scheme_case_lambda_sequence_type:
    {
      int max_sz = sz + 1, a_sz;
      Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)expr;
      int i;
      for (i = cl->count; i--; ) {
        a_sz = estimate_expr_size(cl->array[i], sz, fuel);
        if (a_sz > max_sz) max_sz = a_sz;
      }
      sz = max_sz;
    }
    break;
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)expr;

      sz = estimate_expr_size(app->rator, sz, fuel - 1);
      sz = estimate_expr_size(app->rand, sz, fuel - 1);
      sz++;

      break;
    }
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)expr;
      int i;

      for (i = app->num_args + 1; i--; ) {
        sz = estimate_expr_size(app->args[i], sz, fuel - 1);
      }
      sz++;

      break;
    }
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)expr;

      sz = estimate_expr_size(app->rator, sz, fuel - 1);
      sz = estimate_expr_size(app->rand1, sz, fuel - 1);
      sz = estimate_expr_size(app->rand2, sz, fuel - 1);
      sz++;

      break;
    }
  case scheme_compiled_let_void_type:
    {
      Scheme_Let_Header *head = (Scheme_Let_Header *)expr;
      Scheme_Object *body;
      Scheme_Compiled_Let_Value *lv;
      int i;

      body = head->body;
      for (i = head->num_clauses; i--; ) {
	lv = (Scheme_Compiled_Let_Value *)body;
        sz = estimate_expr_size(lv->value, sz, fuel - 1);
	body = lv->body;
        sz++;
      }
      sz = estimate_expr_size(body, sz, fuel - 1);
      break;
    }
  case scheme_sequence_type:
  case scheme_begin0_sequence_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)expr;
      int i;

      for (i = seq->count; i--; ) {
	sz = estimate_expr_size(seq->array[i], sz, fuel - 1);
      }

      break;
    }
  case scheme_branch_type:
    {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)expr;

      sz = estimate_expr_size(b->test, sz, fuel - 1);
      sz = estimate_expr_size(b->tbranch, sz, fuel - 1);
      sz = estimate_expr_size(b->fbranch, sz, fuel - 1);
      break;
    }
  case scheme_compiled_unclosed_procedure_type:
    {
      sz = estimate_expr_size(((Scheme_Closure_Data *)expr)->code, sz, fuel - 1);
      sz++;
      break;
    }
  case scheme_compiled_toplevel_type:
  case scheme_compiled_quote_syntax_type:
    /* FIXME: other syntax types not covered */
  default:
    sz += 1;
    break;
  }

  return sz;
}

static Scheme_Object *estimate_closure_size(Scheme_Object *e)
{
  int sz;
  sz = estimate_expr_size(e, 0, 32);
  return scheme_box(scheme_make_integer(sz));
}

static Scheme_Object *no_potential_size(Scheme_Object *v)
{
  if (v && SCHEME_BOXP(v))
    return NULL;
  else
    return v;
}

static Scheme_Object *apply_inlined(Scheme_Object *p, Scheme_Closure_Data *data, Optimize_Info *info,
				    int argc, Scheme_App_Rec *app, Scheme_App2_Rec *app2, Scheme_App3_Rec *app3,
                                    int context,
                                    int nested_count, Scheme_Object *orig, Scheme_Object *le_prev)
{
  Scheme_Let_Header *lh;
  Scheme_Compiled_Let_Value *lv, *prev = NULL;
  Scheme_Object *val;
  int i, expected;
  int *flags, flag;
  Optimize_Info *sub_info;

  expected = data->num_params;

  if (!expected) {
    info = optimize_info_add_frame(info, 0, 0, 0);
    info->inline_fuel >>= 1;
    p = scheme_optimize_expr(p, info, context);
    info->next->single_result = info->single_result;
    info->next->preserves_marks = info->preserves_marks;
    optimize_info_done(info, NULL);

    return replace_tail_inside(p, le_prev, orig);
  }

  lh = MALLOC_ONE_TAGGED(Scheme_Let_Header);
  lh->iso.so.type = scheme_compiled_let_void_type;
  lh->count = expected;
  lh->num_clauses = expected;

  for (i = 0; i < expected; i++) {
    lv = MALLOC_ONE_TAGGED(Scheme_Compiled_Let_Value);
    lv->iso.so.type = scheme_compiled_let_value_type;
    lv->count = 1;
    lv->position = i;

    if ((i == expected - 1)
        && (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST)) {
      int j;
      Scheme_Object *l = scheme_null;

      for (j = argc; j-- > i; ) {
        if (app)
          val = app->args[j + 1];
        else if (app3)
          val = (j ? app3->rand2 : app3->rand1);
        else if (app2)
          val = app2->rand;
        else
          val = scheme_false;

        l = cons(val, l);
      }
      l = cons(scheme_list_proc, l);
      val = scheme_make_application(l, info);
    } else if (app)
      val = app->args[i + 1];
    else if (app3)
      val = (i ? app3->rand2 : app3->rand1);
    else
      val = app2->rand;

    if (nested_count)
      val = optimize_shift(val, nested_count, 0);
    lv->value = val;

    flag = closure_argument_flags(data, i);
    flags = (int *)scheme_malloc_atomic(sizeof(int));
    flags[0] = flag;
    lv->flags = flags;

    if (prev)
      prev->body = (Scheme_Object *)lv;
    else
      lh->body = (Scheme_Object *)lv;
    prev = lv;
  }

  if (prev)
    prev->body = p;
  else
    lh->body = p;

  sub_info = optimize_info_add_frame(info, 0, 0, 0);
  sub_info->inline_fuel >>= 1;

  p = scheme_optimize_lets((Scheme_Object *)lh, sub_info, 1, context);

  info->single_result = sub_info->single_result;
  info->preserves_marks = sub_info->preserves_marks;
  optimize_info_done(sub_info, NULL);

  return replace_tail_inside(p, le_prev, orig);
}

int scheme_check_leaf_rator(Scheme_Object *le, int *_flags)
{
  if (le && SCHEME_PRIMP(le)) {
    int opt;
    opt = ((Scheme_Prim_Proc_Header *)le)->flags & SCHEME_PRIM_OPT_MASK;
    if (opt >= SCHEME_PRIM_OPT_NONCM) {
      if (_flags)
        *_flags = (CLOS_PRESERVES_MARKS | CLOS_SINGLE_RESULT);
      if (opt >= SCHEME_PRIM_OPT_IMMEDIATE) {
        return 1;
      }
    }
  }

  return 0;
}

#if 0
# define LOG_INLINE(x) x
#else
# define LOG_INLINE(x) /*empty*/
#endif

Scheme_Object *optimize_for_inline(Optimize_Info *info, Scheme_Object *le, int argc,
                                   Scheme_App_Rec *app, Scheme_App2_Rec *app2, Scheme_App3_Rec *app3,
                                   int *_flags, int context, int optimized_rator, int id_offset)
/* Zero or one of app, app2 and app3 should be non-NULL.
   If app, we're inlining a general application. If app2, we're inlining an
   application with a single argument and if app3, we're inlining an
   application with two arguments.
   If not app, app2, or app3, just return a known procedure, if any,
   and do not check arity.
   The id_offset can be non 0 only when app, app2 and app3 are NULL and optimized_rator is 1. */
{
  int offset = 0, single_use = 0, psize = 0;
  Scheme_Object *bad_app = NULL, *prev = NULL, *orig_le = le;
  int outside_nested = 0, already_opt = optimized_rator, nonleaf, noapp;

  noapp = !app && !app2 && !app3;
  if (id_offset && !noapp)
    return NULL;
  if ((info->inline_fuel < 0) && info->has_nonleaf && !noapp)
    return NULL;

  /* Move inside `let' bindings, so we can convert ((let (....) proc) arg ...)
     to (let (....) (proc arg ...)) */
  if (already_opt)
    extract_tail_inside(&le, &prev, &id_offset);

  if (SAME_TYPE(SCHEME_TYPE(le), scheme_compiled_unclosed_procedure_type)) {
    /* Found a `((lambda' */
    single_use = 1;
  }

  if (SAME_TYPE(SCHEME_TYPE(le), scheme_local_type)) {
    /* Check for inlining: */
    int pos = SCHEME_LOCAL_POS(le);

    if (already_opt) {
      if (pos >= id_offset)
        le = optimize_reverse(info, pos - id_offset, 0, 0);
      else
        le = NULL;
      if (!le)
        return NULL;
      already_opt = 0;
      id_offset = 0;
      pos = SCHEME_LOCAL_POS(le);
    }

    le = optimize_info_lookup(info, pos - id_offset, &offset, &single_use, 0, 0, &psize, NULL);
    outside_nested = 1;
    already_opt = 1;
  }

  if (le) {
    while (SAME_TYPE(SCHEME_TYPE(le), scheme_compiled_toplevel_type)) {
      int pos;
      pos = SCHEME_TOPLEVEL_POS(le);
      single_use = 0;
      if (info->cp->inline_variants) {
        Scheme_Object *iv;
        iv = scheme_hash_get(info->cp->inline_variants, scheme_make_integer(pos));
        if (iv && SCHEME_TRUEP(iv)) {
          Scheme_Hash_Table *iv_ht = NULL;
          if (SCHEME_HASHTP(iv)) {
            iv_ht = (Scheme_Hash_Table *)iv;
            iv = scheme_hash_get(iv_ht, scheme_make_integer(argc));
            if (!iv)
              iv = scheme_hash_get(iv_ht, scheme_false);
          }
          if (SAME_TYPE(SCHEME_TYPE(iv), scheme_inline_variant_type)) {
            int has_cases = 0;
            Scheme_Object *orig_iv = iv;
            iv = scheme_unresolve(iv, argc, &has_cases);
            if (has_cases) {
              if (!iv_ht) {
                iv_ht = scheme_make_hash_table(SCHEME_hash_ptr);
                scheme_hash_set(iv_ht, scheme_false, orig_iv);
                scheme_hash_set(info->cp->inline_variants, scheme_make_integer(pos), (Scheme_Object *)iv_ht);
              }
              scheme_hash_set(iv_ht, scheme_make_integer(argc), iv ? iv : scheme_false);
            } else
              scheme_hash_set(info->cp->inline_variants, scheme_make_integer(pos), iv ? iv : scheme_false);
          }
          if (iv && SCHEME_TRUEP(iv)) {
            le = iv;
            break;
          }
        }
      }
      if (SAME_TYPE(SCHEME_TYPE(le), scheme_compiled_toplevel_type) && info->top_level_consts) {
        le = scheme_hash_get(info->top_level_consts, scheme_make_integer(pos));
        if (le && SCHEME_BOXP(le)) {
          psize = SCHEME_INT_VAL(SCHEME_BOX_VAL(le));
          le = NULL;
        }
        if (!le)
          break;
        outside_nested = 1;
        already_opt = 1;
      } else
        break;
    }
  }

  if (le && SAME_TYPE(SCHEME_TYPE(le), scheme_case_lambda_sequence_type)) {
    Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)le;
    Scheme_Object *cp;
    int i, count;

    if (noapp)
      return le;

    count = cl->count;
    for (i = 0; i < count; i++) {
      cp = cl->array[i];
      if (SAME_TYPE(SCHEME_TYPE(cp), scheme_compiled_unclosed_procedure_type)) {
        Scheme_Closure_Data *data = (Scheme_Closure_Data *)cp;
        if ((data->num_params == argc)
            || ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST)
                && (argc + 1 >= data->num_params))) {
          le = cp;
          break;
        }
      } else {
        scheme_signal_error("internal error: strange case-lambda");
      }
    }
    if (i >= count)
      bad_app = le;
  }

  nonleaf = 1;

  if (le && SAME_TYPE(SCHEME_TYPE(le), scheme_compiled_unclosed_procedure_type) && (info->inline_fuel >= 0)) {
    Scheme_Closure_Data *data = (Scheme_Closure_Data *)le;
    int sz;

    if (noapp)
      return le;

    *_flags = SCHEME_CLOSURE_DATA_FLAGS(data);

    if ((data->num_params == argc)
        || ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST)
            && (argc + 1 >= data->num_params))) {
      int threshold, is_leaf = 0;

      if (!already_opt) {
        /* We have an immediate `lambda' that wasn't optimized, yet.
           Go optimize it, first. */
        return NULL;
      }

      sz = closure_body_size(data, 1, info, &is_leaf);
      if (is_leaf) {
        /* encourage inlining of leaves: */
        sz >>= 2;
      }
      threshold = info->inline_fuel * (2 + argc);

      /* Do we have enough fuel? */
      if ((sz >= 0) && (single_use || (sz <= threshold))) {
        Optimize_Info *sub_info;
        if (id_offset) {
          sub_info = optimize_info_add_frame(info, id_offset, id_offset, 0);
          /* We only go into `let` and `begin` only for an optimized rator, so
             the virtual clock was already incremented as needed. */
          /* We could propagate bound values in sub_info, but relevant inlining
             and propagatation has probably already happened when the rator was
             optimized. */
        } else
          sub_info = info;

	/* If optimize_clone succeeds, inlining succeeds. */
        le = optimize_clone(single_use, data->code, sub_info,
                            offset + (outside_nested ? id_offset : 0),
                            data->num_params);

	if (le) {
	  LOG_INLINE(fprintf(stderr, "Inline %d[%d]<=%d@%d %d %s\n", sz, is_leaf, threshold, info->inline_fuel,
                             single_use, scheme_write_to_string(data->name ? data->name : scheme_false, NULL)));
	  scheme_log(info->logger,
		     SCHEME_LOG_DEBUG,
		     0,
		     "inlining %s size: %d threshold: %d#<separator>%s",
		     scheme_write_to_string(data->name ? data->name : scheme_false, NULL),
		     sz,
		     threshold,
		     scheme_optimize_context_to_string(info->context));
          le = apply_inlined(le, data, sub_info, argc, app, app2, app3, context,
                             id_offset, orig_le, prev);
          if (id_offset)
            optimize_info_done(sub_info, NULL);
          return le;
	} else {
          LOG_INLINE(fprintf(stderr, "No inline %s\n", scheme_write_to_string(data->name ? data->name : scheme_false, NULL)));
	  scheme_log(info->logger,
		     SCHEME_LOG_DEBUG,
		     0,
		     "no-inlining %s size: %d threshold: %d#<separator>%s",
		     scheme_write_to_string(data->name ? data->name : scheme_false, NULL),
		     sz,
		     threshold,
		     scheme_optimize_context_to_string(info->context));
        }
      } else {
        LOG_INLINE(fprintf(stderr, "No fuel %s %d[%d]>%d@%d %d\n", scheme_write_to_string(data->name ? data->name : scheme_false, NULL),
                           sz, is_leaf, threshold,
                           info->inline_fuel, info->use_psize));
	scheme_log(info->logger,
		   SCHEME_LOG_DEBUG,
		   0,
		   "out-of-fuel %s size: %d threshold: %d#<separator>%s",
		   scheme_write_to_string(data->name ? data->name : scheme_false, NULL),
		   sz,
		   threshold,
		   scheme_optimize_context_to_string(info->context));
      }
    } else {
      /* Issue warning below */
      bad_app = (Scheme_Object *)data;
      nonleaf = 0;
    }
  }

  if (scheme_check_leaf_rator(le, _flags))
    nonleaf = 0;

  if (le && SCHEME_PROCP(le)) {
    Scheme_Object *a[1];

    if (noapp)
      return le;

    a[0] = le;
    if (!scheme_check_proc_arity(NULL, argc, 0, 1, a))  {
      bad_app = le;
      nonleaf = 0;
    }
  }

  if (psize) {
    LOG_INLINE(fprintf(stderr, "Potential inline %d %d\n", psize, info->inline_fuel * (argc + 2)));
    /* If we inline, the enclosing function will get larger, so we increase
       its potential size. */
    if (psize <= (info->inline_fuel * (argc + 2)))
      info->psize += psize;
  }

  if (nonleaf)
    info->has_nonleaf = 1;

  if (bad_app) {
    int len;
    const char *pname, *context;
    info->escapes = 1;
    pname = scheme_get_proc_name(bad_app, &len, 0);
    context = scheme_optimize_context_to_string(info->context);
    scheme_log(info->logger,
               SCHEME_LOG_WARNING,
               0,
               "warning%s: optimizer detects procedure incorrectly applied to %d arguments%s%s",
               context,
               argc,
               pname ? ": " : "",
               pname ? pname : "");
  }

  return NULL;
}

static int is_local_type_expression(Scheme_Object *expr, Optimize_Info *info)
{
  int ty;

  ty = scheme_expr_produces_local_type(expr);
  if (ty) return ty;

  if (SAME_TYPE(SCHEME_TYPE(expr), scheme_local_type)) {
    ty = optimize_is_local_type_valued(info, SCHEME_LOCAL_POS(expr));
    if (ty) return ty;
  }

  return 0;
}

static void register_local_argument_types(Scheme_App_Rec *app, Scheme_App2_Rec *app2, Scheme_App3_Rec *app3,
                                          Optimize_Info *info)
{
  Scheme_Object *rator, *rand, *le;
  int n, i;

  if (app) {
    rator = app->args[0];
    n = app->num_args;
  } else if (app2) {
    rator = app2->rator;
    n = 1;
  } else {
    rator = app3->rator;
    n = 2;
  }

  if (SAME_TYPE(SCHEME_TYPE(rator), scheme_local_type)) {
    rator = optimize_reverse(info, SCHEME_LOCAL_POS(rator), 1, 0);
    if (rator) {
      int offset, single_use;
      le = optimize_info_lookup(info, SCHEME_LOCAL_POS(rator), &offset, &single_use, 0, 0, NULL, NULL);
      if (le && SAME_TYPE(SCHEME_TYPE(le), scheme_compiled_unclosed_procedure_type)) {
        Scheme_Closure_Data *data = (Scheme_Closure_Data *)le;
        char *map;
        int ok;

        map = get_closure_local_type_map(data, n, &ok);

        if (ok) {
          for (i = 0; i < n; i++) {
            int ct;

            if (app)
              rand = app->args[i+1];
            else if (app2)
              rand = app2->rand;
            else {
              if (!i)
                rand = app3->rand1;
              else
                rand = app3->rand2;
            }

            ct = is_local_type_expression(rand, info);
            if (ct) {
              if (!map) {
                map = MALLOC_N_ATOMIC(char, n);
                memset(map, ct, n);
                memset(map, 0, i);
              }
            }
            if (map)
              map[i] = ct;
          }

          set_closure_local_type_map(data, map);
        }
      }
    }
  }
}

char *scheme_optimize_context_to_string(Scheme_Object *context)
{
  if (context) {
    Scheme_Object *mod, *func;
    const char *ctx, *prefix, *mctx, *mprefix;
    char *all;
    int clen, plen, mclen, mplen, len;

    if (SCHEME_PAIRP(context)) {
      func = SCHEME_CAR(context);
      mod = SCHEME_CDR(context);
    } else if (SAME_TYPE(SCHEME_TYPE(context), scheme_module_type)) {
      func = scheme_false;
      mod = context;
    } else {
      func = context;
      mod = scheme_false;
    }

    if (SAME_TYPE(SCHEME_TYPE(func), scheme_compiled_unclosed_procedure_type)) {
      Scheme_Object *name;

      name = ((Scheme_Closure_Data *)func)->name;
      if (name) {
        if (SCHEME_VECTORP(name)) {
          Scheme_Object *port;
          int print_width = 1024;
          intptr_t plen;

          port = scheme_make_byte_string_output_port();

          scheme_write_proc_context(port, print_width,
                                    SCHEME_VEC_ELS(name)[0],
                                    SCHEME_VEC_ELS(name)[1], SCHEME_VEC_ELS(name)[2],
                                    SCHEME_VEC_ELS(name)[3], SCHEME_VEC_ELS(name)[4],
                                    SCHEME_TRUEP(SCHEME_VEC_ELS(name)[6]));

          ctx = scheme_get_sized_byte_string_output(port, &plen);
          prefix = " in: ";
        } else {
          ctx = scheme_get_proc_name(func, &len, 0);
          prefix = " in: ";
        }
      } else {
        ctx = "";
        prefix = "";
      }
    } else {
      ctx = "";
      prefix = "";
    }

    if (SAME_TYPE(SCHEME_TYPE(mod), scheme_module_type)) {
      mctx = scheme_display_to_string(((Scheme_Module *)mod)->modsrc, NULL);
      mprefix = " in module: ";
    } else {
      mctx = "";
      mprefix = "";
    }

    clen = strlen(ctx);
    plen = strlen(prefix);
    mclen = strlen(mctx);
    mplen = strlen(mprefix);

    if (!clen && !mclen)
      return "";

    all = scheme_malloc_atomic(clen + plen + mclen + mplen + 1);
    memcpy(all, prefix, plen);
    memcpy(all + plen, ctx, clen);
    memcpy(all + plen + clen, mprefix, mplen);
    memcpy(all + plen + clen + mplen, mctx, mclen);
    all[clen + plen + mclen + mplen] = 0;
    return all;
  } else
    return "";
}

char *scheme_optimize_info_context(Optimize_Info *info)
{
  return scheme_optimize_context_to_string(info->context);
}

Scheme_Logger *scheme_optimize_info_logger(Optimize_Info *info)
{
  return info->logger;
}

static void reset_rator(Scheme_Object *app, Scheme_Object *a)
{
  switch (SCHEME_TYPE(app)) {
  case scheme_application_type:
    ((Scheme_App_Rec *)app)->args[0] = a;
    break;
  case scheme_application2_type:
    ((Scheme_App2_Rec *)app)->rator = a;
    break;
  case scheme_application3_type:
    ((Scheme_App3_Rec *)app)->rator = a;
    break;
  }
}

static Scheme_Object *check_app_let_rator(Scheme_Object *app, Scheme_Object *rator, Optimize_Info *info,
                                          int argc, int context)
/* Convert ((let (....) E) arg ...) to (let (....) (E arg ...)) and
   ((begin .... E) arg ...) to (begin .... (E arg ...)), in case
   the `let' or `begin' is immediately apparent. We check for this 
   pattern again in optimize_for_inline() after optimizing a rator. */
{
  Scheme_Object *orig_rator = rator, *inside = NULL;
  int id_shift = 0;
    
  extract_tail_inside(&rator, &inside, &id_shift);

  if (!inside)
    return NULL;

  /* Handle ((let ([f ...]) f) arg ...) specially, so we can adjust the flags for `f': */
  if (SAME_TYPE(SCHEME_TYPE(inside), scheme_compiled_let_value_type)) {
    Scheme_Compiled_Let_Value *clv = (Scheme_Compiled_Let_Value *)inside;
    if ((clv->count == 1)
        && (clv->position == 0)
        && SAME_TYPE(SCHEME_TYPE(rator), scheme_local_type)
        && (SCHEME_LOCAL_POS(rator) == 0)
        && scheme_is_compiled_procedure(clv->value, 1, 1)) {

      /* get a new rator with flags = 0 */
      rator = scheme_make_local(scheme_local_type, 0, 0);

      if (clv->flags[0] & SCHEME_WAS_APPLIED_EXCEPT_ONCE) {
        clv->flags[0] -= SCHEME_WAS_APPLIED_EXCEPT_ONCE;
        clv->flags[0] |= SCHEME_WAS_ONLY_APPLIED;
      }
    }
  }

  if (id_shift) {
    reset_rator(app, scheme_false);
    app = optimize_shift(app, id_shift, 0);
  }
  reset_rator(app, rator);
  orig_rator = replace_tail_inside(app, inside, orig_rator);

  return scheme_optimize_expr(orig_rator, info, context);
}

static int is_nonmutating_primitive(Scheme_Object *rator, int n)
{
  if (SCHEME_PRIMP(rator)
      && (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & (SCHEME_PRIM_IS_OMITABLE))
      && (n >= ((Scheme_Primitive_Proc *)rator)->mina)
      && (n <= ((Scheme_Primitive_Proc *)rator)->mu.maxa))
    return 1;

  return 0;
}

static int is_noncapturing_primitive(Scheme_Object *rator, int n)
{
  if (SCHEME_PRIMP(rator)) {
    int opt;
    opt = ((Scheme_Prim_Proc_Header *)rator)->flags & SCHEME_PRIM_OPT_MASK;
    if (opt >= SCHEME_PRIM_OPT_IMMEDIATE)
      return 1;
    if (opt >= SCHEME_PRIM_OPT_NONCM) {
      if (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & SCHEME_PRIM_ALWAYS_ESCAPES) {
        /* even if a continuation is captured, it won't get back */
        return 1;
      }
    }
  }
  
  return 0;
}

static int is_nonsaving_primitive(Scheme_Object *rator, int n)
{
  if (SCHEME_PRIMP(rator)) {
    int opt;
    opt = ((Scheme_Prim_Proc_Header *)rator)->flags & SCHEME_PRIM_OPT_MASK;
    if (opt >= SCHEME_PRIM_OPT_IMMEDIATE)
      return 1;
  }

  return 0;
}

static int is_allways_escaping_primitive(Scheme_Object *rator)
{
  if (SCHEME_PRIMP(rator)
      && (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & SCHEME_PRIM_ALWAYS_ESCAPES)) {
        return 1;
  }
  return 0;
}

#define IS_NAMED_PRIM(p, nm) (!strcmp(((Scheme_Primitive_Proc *)p)->name, nm))

static int wants_local_type_arguments(Scheme_Object *rator, int argpos)
{
  if (SCHEME_PRIMP(rator)) {
    int flags;
    flags = SCHEME_PRIM_PROC_OPT_FLAGS(rator);

    if (argpos == 0) {
      if (flags & SCHEME_PRIM_WANTS_FLONUM_FIRST)
        return SCHEME_LOCAL_TYPE_FLONUM;
      if (flags & SCHEME_PRIM_WANTS_EXTFLONUM_FIRST)
        return SCHEME_LOCAL_TYPE_EXTFLONUM;
    } else if (argpos == 1) {
      if (flags & SCHEME_PRIM_WANTS_FLONUM_SECOND)
        return SCHEME_LOCAL_TYPE_FLONUM;
      if (flags & SCHEME_PRIM_WANTS_EXTFLONUM_SECOND)
        return SCHEME_LOCAL_TYPE_EXTFLONUM;
    } else if (argpos == 2) {
      if (flags & SCHEME_PRIM_WANTS_FLONUM_THIRD)
        return SCHEME_LOCAL_TYPE_FLONUM;
      if (flags & SCHEME_PRIM_WANTS_EXTFLONUM_THIRD)
        return SCHEME_LOCAL_TYPE_EXTFLONUM;
    }
  }

  return 0;
}

static int produces_local_type(Scheme_Object *rator, int argc)
{
  if (SCHEME_PRIMP(rator)
      && (argc >= ((Scheme_Primitive_Proc *)rator)->mina)
      && (argc <= ((Scheme_Primitive_Proc *)rator)->mu.maxa)) {
    int flags;
    flags = SCHEME_PRIM_PROC_OPT_FLAGS(rator);
    return SCHEME_PRIM_OPT_TYPE(flags);
  }

  return 0;
}

static int expr_produces_local_type(Scheme_Object *expr, int fuel)
/* can be called by the JIT; beware that the validator must be
   able to reconstruct the result in a shallow way, so don't
   make the result of a function call depend on its arguments */
{
  if (fuel <= 0) return 0;

  while (1) {
    switch (SCHEME_TYPE(expr)) {
    case scheme_application_type:
      {
        Scheme_App_Rec *app = (Scheme_App_Rec *)expr;
        return produces_local_type(app->args[0], app->num_args);
      }
      break;
    case scheme_application2_type:
      {
        Scheme_App2_Rec *app = (Scheme_App2_Rec *)expr;
        return produces_local_type(app->rator, 1);
      }
      break;
    case scheme_application3_type:
      {
        Scheme_App3_Rec *app = (Scheme_App3_Rec *)expr;

        if (SCHEME_PRIMP(app->rator)
            && (SCHEME_PRIM_PROC_OPT_FLAGS(app->rator) & SCHEME_PRIM_IS_BINARY_INLINED)
            && IS_NAMED_PRIM(app->rator, "bitwise-and")) {
          /* Assume that a fixnum argument to bitwise-and will never get lost,
             and so the validator will be able to confirm that a `bitwise-and`
             combination produces a fixnum. */
          if ((SCHEME_INTP(app->rand1)
               && IN_FIXNUM_RANGE_ON_ALL_PLATFORMS(SCHEME_INT_VAL(app->rand1)))
              || (SCHEME_INTP(app->rand2)
                  && IN_FIXNUM_RANGE_ON_ALL_PLATFORMS(SCHEME_INT_VAL(app->rand2))))
            return SCHEME_LOCAL_TYPE_FIXNUM;
        }
        
        return produces_local_type(app->rator, 2);
      }
      break;
    case scheme_branch_type:
      {
        Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)expr;
        int t1, t2;

        t1 = expr_produces_local_type(b->tbranch, fuel / 2);
        if (t1) {
          t2 = expr_produces_local_type(b->fbranch, fuel / 2);
          return ((t1 == t2) ? t1 : 0);
        } else
          return 0;
      }
      break;
    case scheme_sequence_type:
      {
        Scheme_Sequence *seq = (Scheme_Sequence *)expr;

        expr = seq->array[seq->count-1];
        break;
      }
    case scheme_compiled_let_void_type:
      {
        Scheme_Let_Header *lh = (Scheme_Let_Header *)expr;
        int i;
        expr = lh->body;
        for (i = 0; i < lh->num_clauses; i++) {
          expr = ((Scheme_Compiled_Let_Value *)expr)->body;
        }
        /* check expr again */
      }
      break;
    default:
      if (SCHEME_FLOATP(expr))
        return SCHEME_LOCAL_TYPE_FLONUM;
      if (SCHEME_LONG_DBLP(expr))
        return SCHEME_LOCAL_TYPE_EXTFLONUM;
      if (SCHEME_INTP(expr)
          && IN_FIXNUM_RANGE_ON_ALL_PLATFORMS(SCHEME_INT_VAL(expr)))
        return SCHEME_LOCAL_TYPE_FIXNUM;
      return 0;
    }
  }
}

int scheme_expr_produces_local_type(Scheme_Object *expr)
{
  return expr_produces_local_type(expr, 10);
}

static Scheme_Object *local_type_to_predicate(int t)
{
  switch (t) {
  case SCHEME_LOCAL_TYPE_FLONUM:
    return scheme_flonum_p_proc;
  case SCHEME_LOCAL_TYPE_FIXNUM:
    return scheme_fixnum_p_proc;
  case SCHEME_LOCAL_TYPE_EXTFLONUM:
    return scheme_extflonum_p_proc;
  }
  return NULL;
}

static Scheme_Object *rator_implies_predicate(Scheme_Object *rator, int argc)
{
  if (SCHEME_PRIMP(rator)) {
    if ((argc == 2)
        && (SAME_OBJ(rator, scheme_cons_proc)
            || SAME_OBJ(rator, scheme_unsafe_cons_list_proc)))
      return scheme_pair_p_proc;
    else if ((argc == 2) && SAME_OBJ(rator, scheme_mcons_proc))
      return scheme_mpair_p_proc;
    else if (SAME_OBJ(rator, scheme_list_proc)) {
      if (argc >= 1)
        return scheme_pair_p_proc;
      else
        return scheme_null_p_proc;
    } else if (SAME_OBJ(rator, scheme_list_star_proc)) {
      if (argc > 2)
        return scheme_pair_p_proc;
    } else if (SAME_OBJ(rator, scheme_vector_proc)
               || SAME_OBJ(rator, scheme_vector_immutable_proc))
      return scheme_vector_p_proc;
    else if ((argc == 1)
             && (SAME_OBJ(rator, scheme_box_proc)
                 || SAME_OBJ(rator, scheme_box_immutable_proc)))
      return scheme_box_p_proc;
    
    {
      Scheme_Object *p;
      p = local_type_to_predicate(produces_local_type(rator, argc));
      if (p)
        return p;
    }
  }

  return NULL;
}

static Scheme_Object *expr_implies_predicate(Scheme_Object *expr, Optimize_Info *info, int delta, int fuel)
{
  Scheme_Object *rator = NULL;
  int argc = 0;

  /* Any returned predicate must match only non-#f values, since
     that's assumed by optimize_branch(). */

  if (fuel <= 0)
    return NULL;

  switch (SCHEME_TYPE(expr)) {
  case scheme_local_type:
    {
      Scheme_Object *p;
      int pos = SCHEME_LOCAL_POS(expr);
      pos -= delta;
      if (pos < 0)
        return NULL;
      if (!optimize_is_mutated(info, pos)){
        p = optimize_get_predicate(pos, info);
        if (p)
          return p;

        p = local_type_to_predicate(optimize_is_local_type_valued(info, pos));
        if (p)
          return p;
      }
    }
    break;
  case scheme_application2_type:
    rator = ((Scheme_App2_Rec *)expr)->rator;
    argc = 1;
    break;
  case scheme_application3_type:
    rator = ((Scheme_App3_Rec *)expr)->rator;
    argc = 2;
    break;
  case scheme_application_type:
    argc = ((Scheme_App_Rec *)expr)->num_args;
    rator = ((Scheme_App_Rec *)expr)->args[0];
    break;
  case scheme_compiled_unclosed_procedure_type:
    return scheme_procedure_p_proc;
    break;
  case scheme_case_lambda_sequence_type:
    return scheme_procedure_p_proc;
    break;
  case scheme_compiled_quote_syntax_type:
    return scheme_syntax_p_proc;
    break;
  case scheme_branch_type:
    {
      Scheme_Object *l, *r;
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)expr;
      l = expr_implies_predicate(b->tbranch, info, delta, fuel-1);
      if (l) {
        r = expr_implies_predicate(b->fbranch, info, delta, fuel-1);
        if (SAME_OBJ(l, r))
          return l;
      }
    }
    break;
  case scheme_sequence_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)expr;

      return expr_implies_predicate(seq->array[seq->count-1], info, delta, fuel-1);
    }
  case scheme_compiled_let_void_type:
    {
      Scheme_Let_Header *lh = (Scheme_Let_Header *)expr;
      int i;
      delta += lh->count;
      expr = lh->body;
      for (i = 0; i < lh->num_clauses; i++) {
        expr = ((Scheme_Compiled_Let_Value *)expr)->body;
      }
      return expr_implies_predicate(expr, info, delta, fuel-1);
    }
    break;
  case scheme_pair_type:
    return scheme_pair_p_proc;
    break;
  case scheme_mutable_pair_type:
    return scheme_mpair_p_proc;
    break;
  case scheme_vector_type:
    return scheme_vector_p_proc;
    break;
  case scheme_box_type:
    return scheme_box_p_proc;
    break;
  default:
    if (SCHEME_FLOATP(expr))
      return scheme_flonum_p_proc;
    if (SCHEME_LONG_DBLP(expr))
      return scheme_extflonum_p_proc;
    if (SCHEME_INTP(expr)
        && IN_FIXNUM_RANGE_ON_ALL_PLATFORMS(SCHEME_INT_VAL(expr)))
      return scheme_fixnum_p_proc;
  }

  if (rator)
    return rator_implies_predicate(rator, argc);

  {
    /* These tests are slower, so put them at the end */  
    int flags, sub_context = 0;
    if (lookup_constant_proc(info, expr, delta)
        || optimize_for_inline(info, expr, 1, NULL, NULL, NULL, &flags, sub_context, 1, delta)){
      return scheme_procedure_p_proc;
    }
  }

  return NULL;
}

static Scheme_Object *finish_optimize_app(Scheme_Object *o, Optimize_Info *info, int context, int rator_flags)
{
  switch(SCHEME_TYPE(o)) {
  case scheme_application_type:
    return finish_optimize_application((Scheme_App_Rec *)o, info, context, rator_flags);
  case scheme_application2_type:
    return finish_optimize_application2((Scheme_App2_Rec *)o, info, context, rator_flags);
  case scheme_application3_type:
    return finish_optimize_application3((Scheme_App3_Rec *)o, info, context, rator_flags);
  default:
    return o; /* may be a constant due to constant-folding */
  }
}

static Scheme_Object *direct_apply(Scheme_Object *expr, Scheme_Object *rator, Scheme_Object *last_rand, Optimize_Info *info)
{
  if (SAME_OBJ(rator, scheme_apply_proc)) {
    switch(SCHEME_TYPE(last_rand)) {
    case scheme_application_type:
      rator = ((Scheme_App_Rec *)last_rand)->args[0];
      break;
    case scheme_application2_type:
      rator = ((Scheme_App2_Rec *)last_rand)->rator;
      break;
    case scheme_application3_type:
      rator = ((Scheme_App3_Rec *)last_rand)->rator;
      break;
    case scheme_pair_type:
      if (scheme_is_list(last_rand))
        rator = scheme_list_proc;
      else
        rator = NULL;
      break;
    case scheme_null_type:
      rator = scheme_list_proc;
      break;
    default:
      rator = NULL;
      break;
    }

    if (rator && SAME_OBJ(rator, scheme_list_proc)) {
      /* Convert (apply f arg1 ... (list arg2 ...))
         to (f arg1 ... arg2 ...) */
      Scheme_Object *l = scheme_null;
      int i;

      switch(SCHEME_TYPE(last_rand)) {
      case scheme_application_type:
        for (i = ((Scheme_App_Rec *)last_rand)->num_args; i--; ) {
          l = scheme_make_pair(((Scheme_App_Rec *)last_rand)->args[i+1], l);
        }
        break;
      case scheme_application2_type:
        l = scheme_make_pair(((Scheme_App2_Rec *)last_rand)->rand, l);
        break;
      case scheme_application3_type:
        l = scheme_make_pair(((Scheme_App3_Rec *)last_rand)->rand2, l);
        l = scheme_make_pair(((Scheme_App3_Rec *)last_rand)->rand1, l);
        break;
      case scheme_pair_type:
        l = last_rand;
        break;
      case scheme_null_type:
        l = scheme_null;
        break;
      }

      switch(SCHEME_TYPE(expr)) {
      case scheme_application_type:
        for (i = ((Scheme_App_Rec *)expr)->num_args - 1; i--; ) {
          l = scheme_make_pair(((Scheme_App_Rec *)expr)->args[i+1], l);
        }
        break;
      default:
      case scheme_application3_type:
        l = scheme_make_pair(((Scheme_App3_Rec *)expr)->rand1, l);
        break;
      }

      return scheme_make_application(l, info);
    }
  }

  return NULL;
}

static Scheme_Object *optimize_application(Scheme_Object *o, Optimize_Info *info, int context)
{
  Scheme_Object *le;
  Scheme_App_Rec *app;
  int i, n, rator_apply_escapes = 0, rator_flags = 0, sub_context = 0;
  Optimize_Info_Sequence info_seq;

  app = (Scheme_App_Rec *)o;

  /* Check for (apply ... (list ...)) early: */
  le = direct_apply((Scheme_Object *)app, app->args[0], app->args[app->num_args], info);
  if (le)
    return scheme_optimize_expr(le, info, context);

  le = check_app_let_rator(o, app->args[0], info, app->num_args, context);
  if (le)
    return le;

  n = app->num_args + 1;

  optimize_info_seq_init(info, &info_seq);

  for (i = 0; i < n; i++) {
    if (!i) {
      le = optimize_for_inline(info, app->args[i], n - 1, app, NULL, NULL, &rator_flags, context, 0, 0);
      if (le)
        return le;
    }

    sub_context = OPT_CONTEXT_SINGLED;
    if (i > 0) {
      int ty;
      ty = wants_local_type_arguments(app->args[0], i - 1);
      if (ty)
        sub_context |= (ty << OPT_CONTEXT_TYPE_SHIFT);
    }

    optimize_info_seq_step(info, &info_seq);
    le = scheme_optimize_expr(app->args[i], info, sub_context);
    app->args[i] = le;
    if (info->escapes) {
      int j;
      Scheme_Object *e, *l;
      optimize_info_seq_done(info, &info_seq);

      l = scheme_make_pair(app->args[i], scheme_null);

      for (j = i - 1; j >= 0; j--) {
        e = app->args[j];
        e = optimize_ignored(e, info, 0, 1, 1, 5);
        if (e) {
          if (!single_valued_noncm_expression(e, 5))
            e = ensure_single_value(e);
          l = scheme_make_pair(e, l);
        }
      }
      return scheme_make_sequence_compilation(l, 1);
    }

    if (!i) {
      /* Maybe found "((lambda" after optimizing; try again */
      le = optimize_for_inline(info, app->args[i], n - 1, app, NULL, NULL, &rator_flags, context, 1, 0);
      if (le)
        return le;
      rator_apply_escapes = info->escapes;
    }
  }

  optimize_info_seq_done(info, &info_seq);

  /* Check for (apply ... (list ...)) after some optimizations: */
  le = direct_apply((Scheme_Object *)app, app->args[0], app->args[app->num_args], info);
  if (le) return finish_optimize_app(le, info, context, rator_flags);

  /* Convert (hash-ref '#hash... key (lambda () literal))
     to (hash-ref '#hash... key literal) */
  if ((app->num_args == 3)
      && SAME_OBJ(scheme_hash_ref_proc, app->args[0])
      && SCHEME_HASHTRP(app->args[1])
      && SAME_TYPE(scheme_compiled_unclosed_procedure_type, SCHEME_TYPE(app->args[3]))
      && (SCHEME_TYPE(((Scheme_Closure_Data *)app->args[3])->code) > _scheme_compiled_values_types_)
      && !SCHEME_PROCP(((Scheme_Closure_Data *)app->args[3])->code)) {
    app->args[3] = ((Scheme_Closure_Data *)app->args[3])->code;
  }

  if (rator_apply_escapes) {
   info->escapes = 1;
   SCHEME_APPN_FLAGS(app) |= (APPN_FLAG_IMMED | APPN_FLAG_SFS_TAIL);
  }

  return finish_optimize_application(app, info, context, rator_flags);
}

static int appn_flags(Scheme_Object *rator, Optimize_Info *info)
{
  if (SAME_TYPE(SCHEME_TYPE(rator), scheme_compiled_toplevel_type)) {
    if (info->top_level_consts) {
      int pos;
      pos = SCHEME_TOPLEVEL_POS(rator);
      rator = scheme_hash_get(info->top_level_consts, scheme_make_integer(pos));
      rator = no_potential_size(rator);
      if (!rator) return 0;
      if (SAME_TYPE(SCHEME_TYPE(rator), scheme_proc_shape_type)) {
        return APPN_FLAG_SFS_TAIL;
      } else if (SAME_TYPE(SCHEME_TYPE(rator), scheme_struct_proc_shape_type)) {
        int ps = SCHEME_PROC_SHAPE_MODE(rator);
        if ((ps == STRUCT_PROC_SHAPE_PRED)
            || (ps == STRUCT_PROC_SHAPE_GETTER)
            || (ps == STRUCT_PROC_SHAPE_SETTER))
          return (APPN_FLAG_IMMED | APPN_FLAG_SFS_TAIL);
        return 0;
      }
    }
  }

  if (SCHEME_PRIMP(rator)) {
    int opt = (SCHEME_PRIM_PROC_FLAGS(rator) & SCHEME_PRIM_OPT_MASK);
    if (opt >= SCHEME_PRIM_OPT_IMMEDIATE)
      return (APPN_FLAG_IMMED | APPN_FLAG_SFS_TAIL);
    return 0;
  }

  if (SAME_TYPE(scheme_compiled_unclosed_procedure_type, SCHEME_TYPE(rator))
      || SAME_TYPE(scheme_case_lambda_sequence_type, SCHEME_TYPE(rator))
      || SAME_TYPE(scheme_noninline_proc_type, SCHEME_TYPE(rator)))
    return APPN_FLAG_SFS_TAIL;
  
  return 0;
}

static Scheme_Object *finish_optimize_any_application(Scheme_Object *app, Scheme_Object *rator, int argc,
                                                      Optimize_Info *info, int context)
{
  if (context & OPT_CONTEXT_BOOLEAN)
    if (rator_implies_predicate(rator, argc))
      return make_discarding_sequence(app, scheme_true, info, 0);

  if (SAME_OBJ(rator, scheme_void_proc))
    return make_discarding_sequence(app, scheme_void, info, 0);
  
  if (is_allways_escaping_primitive(rator)) {
    info->escapes = 1;
  }

  return app;
}

static Scheme_Object *finish_optimize_application(Scheme_App_Rec *app, Optimize_Info *info, int context, int rator_flags)
{
  Scheme_Object *le;
  int all_vals = 1, i, flags;

  for (i = app->num_args; i--; ) {
    if (SCHEME_TYPE(app->args[i+1]) < _scheme_compiled_values_types_)
      all_vals = 0;
  }

  info->size += 1;
  if (!is_nonmutating_primitive(app->args[0], app->num_args))
    info->vclock += 1;
  if (!is_noncapturing_primitive(app->args[0], app->num_args))
    info->kclock += 1;
  if (!is_nonsaving_primitive(app->args[0], app->num_args))
    info->sclock += 1;

  if (all_vals) {
    le = try_optimize_fold(app->args[0], NULL, (Scheme_Object *)app, info);
    if (le)
      return le;
  }

  info->preserves_marks = !!(rator_flags & CLOS_PRESERVES_MARKS);
  info->single_result = !!(rator_flags & CLOS_SINGLE_RESULT);
  if (rator_flags & CLOS_RESULT_TENTATIVE) {
    info->preserves_marks = -info->preserves_marks;
    info->single_result = -info->single_result;
  }

  if (!app->num_args && SAME_OBJ(app->args[0], scheme_list_proc))
    return scheme_null;

  register_local_argument_types(app, NULL, NULL, info);

  flags = appn_flags(app->args[0], info);
  SCHEME_APPN_FLAGS(app) |= flags;

  return finish_optimize_any_application((Scheme_Object *)app, app->args[0], app->num_args,
                                         info, context);
}

static Scheme_Object *lookup_constant_proc(Optimize_Info *info, Scheme_Object *rand, int delta)
{
  Scheme_Object *c = NULL;

  if (SAME_TYPE(scheme_compiled_unclosed_procedure_type, SCHEME_TYPE(rand)))
    c = rand;
  else if (SAME_TYPE(SCHEME_TYPE(rand), scheme_local_type)) {
    int offset, pos;
    Scheme_Object *expr;
    pos = SCHEME_LOCAL_POS(rand);
    if (pos >= delta) {
      pos -= delta;
      expr = optimize_reverse(info, pos, 0, 0);
      c = optimize_info_lookup(info, SCHEME_LOCAL_POS(expr), &offset, NULL, 0, 0, NULL, NULL);
    }
  } else if (SAME_TYPE(SCHEME_TYPE(rand), scheme_compiled_toplevel_type)) {
    if (info->top_level_consts) {
      int pos;

      while (1) {
        pos = SCHEME_TOPLEVEL_POS(rand);
        c = scheme_hash_get(info->top_level_consts, scheme_make_integer(pos));
        c = no_potential_size(c);
        if (c && SAME_TYPE(SCHEME_TYPE(c), scheme_compiled_toplevel_type))
          rand = c;
        else
          break;
      }
    }
  }

  if (c && SAME_TYPE(scheme_noninline_proc_type, SCHEME_TYPE(c))) {
    c = SCHEME_BOX_VAL(c);

    while (SAME_TYPE(SCHEME_TYPE(c), scheme_compiled_let_void_type)) {
      /* This must be (let ([x <omittable>]) <proc>); see scheme_is_statically_proc() */
      Scheme_Let_Header *lh = (Scheme_Let_Header *)c;
      Scheme_Compiled_Let_Value *lv = (Scheme_Compiled_Let_Value *)lh->body;
      c = lv->body;
    }
  }

  if (c && (SAME_TYPE(scheme_compiled_unclosed_procedure_type, SCHEME_TYPE(c))
            || SAME_TYPE(scheme_case_lambda_sequence_type, SCHEME_TYPE(c))))
    return c;

  return NULL;
}

static void check_known2(Optimize_Info *info, Scheme_App2_Rec *app,
                         Scheme_Object *rand, int id_offset,
                         const char *who, Scheme_Object *expect_pred, Scheme_Object *unsafe)
/* Replace the rator with an unsafe version if we know that it's ok. Alternatively,
   the rator implies a check, so add type information for subsequent expressions. 
   If the rand has alredy a different type, mark that this will generate an error. */
{
  if (IS_NAMED_PRIM(app->rator, who)) {
    Scheme_Object *pred;
      
    pred = expr_implies_predicate(rand, info, id_offset, 5); 
    if (pred) {
      if (SAME_OBJ(pred, expect_pred))
        app->rator = unsafe;
      else
        info->escapes = 1;
    } else {
      if (SAME_TYPE(SCHEME_TYPE(rand), scheme_local_type)) {
        int pos = SCHEME_LOCAL_POS(rand);
        if (pos >= id_offset) {
          pos -= id_offset;
          if (!optimize_is_mutated(info, pos))
            add_type(info, pos, expect_pred);
        }
      }
    }
  }
}

static Scheme_Object *try_reduce_predicate(Scheme_Object *rator, Scheme_Object *rand,
                                           Optimize_Info *info, int id_offset)
/* Change (pair? (list X complex-Y Z)) => (begin complex-Y #t), etc.
   It's especially nice to avoid the constructions. */
{
  int matches;
  Scheme_Object *pred;

  if (!relevant_predicate(rator)
      && (!SAME_OBJ(rator, scheme_list_p_proc)))
    return NULL;

  pred = expr_implies_predicate(rand, info, id_offset, 5);

  if (!pred)
    return NULL;

  matches = SAME_OBJ(rator, pred);

  if (SAME_OBJ(rator, scheme_list_p_proc)) {
    if (SAME_OBJ(pred, scheme_pair_p_proc)) {
      /* a pair may be a list or not */
      return NULL;
    } else {
      /* otherwise, only null is a list */
      matches = SAME_OBJ(scheme_null_p_proc, pred);
    }
  }

  return make_discarding_sequence(rand, (matches ? scheme_true : scheme_false), info, id_offset);
}

static Scheme_Object *make_optimize_prim_application2(Scheme_Object *prim, Scheme_Object *rand,
                                                      Optimize_Info *info, int context)
/* make (prim rand) and optimize it. rand must be already optimized */
{
  Scheme_Object *alt;
  alt = make_application_2(prim, rand, info);
  /* scheme_make_application may use constant folding, check that alt is not a constant */
  if (SAME_TYPE(SCHEME_TYPE(alt), scheme_application2_type)) {
    int rator_flags = 0;
    scheme_check_leaf_rator(prim, &rator_flags);
    return finish_optimize_application2((Scheme_App2_Rec *)alt, info, context, rator_flags);
  } else
    return alt;
}


static Scheme_Object *optimize_application2(Scheme_Object *o, Optimize_Info *info, int context)
{
  Scheme_App2_Rec *app;
  Scheme_Object *le;
  int rator_flags = 0, rator_apply_escapes, sub_context, ty;
  Optimize_Info_Sequence info_seq;

  app = (Scheme_App2_Rec *)o;

  le = check_app_let_rator(o, app->rator, info, 1, context);
  if (le)
    return le;

  le = optimize_for_inline(info, app->rator, 1, NULL, app, NULL, &rator_flags, context, 0, 0);
  if (le)
    return le;

  optimize_info_seq_init(info, &info_seq);

  sub_context = OPT_CONTEXT_SINGLED;

  le = scheme_optimize_expr(app->rator, info, sub_context);
  app->rator = le;
  if (info->escapes) {
    optimize_info_seq_done(info, &info_seq);
    return app->rator;
  }

  {
    /* Maybe found "((lambda" after optimizing; try again */
    le = optimize_for_inline(info, app->rator, 1, NULL, app, NULL, &rator_flags, context, 1, 0);
    if (le)
      return le;
    rator_apply_escapes = info->escapes;
  }

  if (SAME_PTR(scheme_not_prim, app->rator)){
    sub_context |= OPT_CONTEXT_BOOLEAN;
  } else {
    ty = wants_local_type_arguments(app->rator, 0);
    if (ty)
      sub_context |= (ty << OPT_CONTEXT_TYPE_SHIFT);
  }

  optimize_info_seq_step(info, &info_seq);

  le = scheme_optimize_expr(app->rand, info, sub_context);
  app->rand = le;
  optimize_info_seq_done(info, &info_seq);
  if (info->escapes) {
    info->size += 1;
    return make_discarding_first_sequence(app->rator, app->rand, info, 0);
  }

  if (rator_apply_escapes) {
   info->escapes = 1;
   SCHEME_APPN_FLAGS(app) |= (APPN_FLAG_IMMED | APPN_FLAG_SFS_TAIL);
  }

  return finish_optimize_application2(app, info, context, rator_flags);
}

static Scheme_Object *finish_optimize_application2(Scheme_App2_Rec *app, Optimize_Info *info, int context, int rator_flags)
{
  int flags;
  Scheme_Object *rand, *inside = NULL, *alt;
  int id_offset = 0;

  info->size += 1;

  /* Path for direct constant folding */
  if (SCHEME_TYPE(app->rand) > _scheme_compiled_values_types_) {
    Scheme_Object *le;
    le = try_optimize_fold(app->rator, NULL, (Scheme_Object *)app, info);
    if (le)
      return le;
  }

  rand = app->rand;

  /* We can go inside a `begin' and a `let', which is useful in case
     the argument was a function call that has been inlined. */
  extract_tail_inside(&rand, &inside, &id_offset);

  if (SCHEME_TYPE(rand) > _scheme_compiled_values_types_) {
    Scheme_Object *le;
    le = try_optimize_fold(app->rator, scheme_make_pair(rand, scheme_null), NULL, info);
    if (le)
      return replace_tail_inside(le, inside, app->rand);
  }

  if (!is_nonmutating_primitive(app->rator, 1))
    info->vclock += 1;
  if (!is_noncapturing_primitive(app->rator, 1))
    info->kclock += 1;
  if (!is_nonsaving_primitive(app->rator, 1))
    info->sclock += 1;

  info->preserves_marks = !!(rator_flags & CLOS_PRESERVES_MARKS);
  info->single_result = !!(rator_flags & CLOS_SINGLE_RESULT);
  if (rator_flags & CLOS_RESULT_TENTATIVE) {
    info->preserves_marks = -info->preserves_marks;
    info->single_result = -info->single_result;
  }

  if ((SAME_OBJ(scheme_values_func, app->rator)
        || SAME_OBJ(scheme_list_star_proc, app->rator))
      && ((context & OPT_CONTEXT_SINGLED)
          || scheme_omittable_expr(rand, 1, -1, 0, info, info, 0, id_offset, ID_OMIT)
          || single_valued_noncm_expression(rand, 5))) {
    info->preserves_marks = 1;
    info->single_result = 1;
    return replace_tail_inside(rand, inside, app->rand);
  }

  /* Check for things like (cXr (cons X Y)): */
  if (SCHEME_PRIMP(app->rator)
    && (SCHEME_PRIM_PROC_OPT_FLAGS(app->rator) & SCHEME_PRIM_IS_UNARY_INLINED)) {

    switch (SCHEME_TYPE(rand)) {
    case scheme_application2_type:
      {
        Scheme_App2_Rec *app2 = (Scheme_App2_Rec *)rand;
        if (SAME_OBJ(scheme_list_proc, app2->rator)) {
          if (IS_NAMED_PRIM(app->rator, "car")) {
            /* (car (list X)) */
            alt = make_discarding_sequence(scheme_void, app2->rand, info, id_offset);
            return replace_tail_inside(alt, inside, app->rand);
          } else if (IS_NAMED_PRIM(app->rator, "cdr")) {
            /* (cdr (list X)) */
            alt = make_discarding_sequence(app2->rand, scheme_null, info, id_offset);
            return replace_tail_inside(alt, inside, app->rand);
          }
        }
        break;
      }
    case scheme_application3_type:
      {
        Scheme_App3_Rec *app3 = (Scheme_App3_Rec *)rand;
        if (IS_NAMED_PRIM(app->rator, "car")) {
          if (SAME_OBJ(scheme_cons_proc, app3->rator)
              || SAME_OBJ(scheme_unsafe_cons_list_proc, app3->rator)
              || SAME_OBJ(scheme_list_proc, app3->rator)
              || SAME_OBJ(scheme_list_star_proc, app3->rator)) {
            /* (car ({cons|list|list*} X Y)) */
            alt = make_discarding_reverse_sequence(app3->rand2, app3->rand1, info, id_offset);
            return replace_tail_inside(alt, inside, app->rand);
          }
        } else if (IS_NAMED_PRIM(app->rator, "cdr")) {
          if (SAME_OBJ(scheme_cons_proc, app3->rator)
              || SAME_OBJ(scheme_unsafe_cons_list_proc, app3->rator)
              || SAME_OBJ(scheme_list_star_proc, app3->rator)) {
            /* (cdr ({cons|list*} X Y)) */
            alt = make_discarding_sequence(app3->rand1, app3->rand2, info, id_offset);
            return replace_tail_inside(alt, inside, app->rand);
          } else if (SAME_OBJ(scheme_list_proc, app3->rator)) {
            /* (cdr (list X Y)) */
            alt = make_application_2(scheme_list_proc, app3->rand2, info);
            SCHEME_APPN_FLAGS(((Scheme_App_Rec *)alt)) |= (APPN_FLAG_IMMED | APPN_FLAG_SFS_TAIL);
            alt = make_discarding_sequence(app3->rand1, alt, info, id_offset);
            return replace_tail_inside(alt, inside, app->rand);
          }
        } else if (IS_NAMED_PRIM(app->rator, "cadr")) {
          if (SAME_OBJ(scheme_list_proc, app3->rator)) {
            /* (cadr (list X Y)) */
            alt = make_discarding_sequence(app3->rand1, app3->rand2, info, id_offset);
            return replace_tail_inside(alt, inside, app->rand);
          }
        }
        break;
      }
    case scheme_application_type:
      {
        Scheme_App_Rec *appr = (Scheme_App_Rec *)rand;
        Scheme_Object *r = appr->args[0];
        if (IS_NAMED_PRIM(app->rator, "car")) {
          if ((appr->args > 0)
              && (SAME_OBJ(scheme_list_proc, r)
                  || SAME_OBJ(scheme_list_star_proc, r))) {
            /* (car ({list|list*} X Y ...)) */
            alt = make_discarding_app_sequence(appr, 0, NULL, info, id_offset);
            return replace_tail_inside(alt, inside, app->rand);
          }
        } else if (IS_NAMED_PRIM(app->rator, "cdr")) {
          /* (cdr ({list|list*} X Y ...)) */
          if ((appr->args > 0)
              && (SAME_OBJ(scheme_list_proc, r)
                  || SAME_OBJ(scheme_list_star_proc, r))) {
            Scheme_Object *al = scheme_null;
            int k;
            for (k = appr->num_args; k > 1; k--) {
              al = scheme_make_pair(appr->args[k], al);
            }
            al = scheme_make_pair(r, al);
            alt = scheme_make_application(al, info);
            SCHEME_APPN_FLAGS(((Scheme_App_Rec *)alt)) |= (APPN_FLAG_IMMED | APPN_FLAG_SFS_TAIL);
            alt = make_discarding_sequence(appr->args[1], alt, info, id_offset);
            return replace_tail_inside(alt, inside, app->rand);
          }
        }
        break;
      }
    }

    alt = try_reduce_predicate(app->rator, rand, info, id_offset);
    if (alt)
      return replace_tail_inside(alt, inside, app->rand);

    check_known2(info, app, rand, id_offset, "car", scheme_pair_p_proc, scheme_unsafe_car_proc);
    check_known2(info, app, rand, id_offset, "cdr", scheme_pair_p_proc, scheme_unsafe_cdr_proc);
    check_known2(info, app, rand, id_offset, "mcar", scheme_mpair_p_proc, scheme_unsafe_mcar_proc);
    check_known2(info, app, rand, id_offset, "mcdr", scheme_mpair_p_proc, scheme_unsafe_mcdr_proc);
    /* It's not clear that these are useful, since a chaperone check is needed anyway: */
    check_known2(info, app, rand, id_offset, "unbox", scheme_box_p_proc, scheme_unsafe_unbox_proc);
    check_known2(info, app, rand, id_offset, "vector-length", scheme_vector_p_proc, scheme_unsafe_vector_length_proc);

  } else {
    if (SAME_OBJ(scheme_struct_type_p_proc, app->rator)) {
      Scheme_Object *c;
      c = get_struct_proc_shape(rand, info);
      if (c && ((SCHEME_PROC_SHAPE_MODE(c) & STRUCT_PROC_SHAPE_MASK)
                == STRUCT_PROC_SHAPE_STRUCT)) {
        info->preserves_marks = 1;
        info->single_result = 1;
        return replace_tail_inside(scheme_true, inside, app->rand);
      }
    }

    if (SAME_OBJ(scheme_varref_const_p_proc, app->rator)
        && SAME_TYPE(SCHEME_TYPE(rand), scheme_varref_form_type)) {
      Scheme_Object *var = SCHEME_PTR1_VAL(rand);
      if (SAME_OBJ(var, scheme_true)) {
        info->preserves_marks = 1;
        info->single_result = 1;
        return replace_tail_inside(scheme_true, inside, app->rand);
      } else if (SAME_OBJ(var, scheme_false)) {
        info->preserves_marks = 1;
        info->single_result = 1;
        return replace_tail_inside(scheme_false, inside, app->rand);
      } else {
        if (var && scheme_compiled_propagate_ok(var, info)) {
          /* can propagate => is a constant */
          info->preserves_marks = 1;
          info->single_result = 1;
          return replace_tail_inside(scheme_true, inside, app->rand);
        }
      }
    }
  }

  register_local_argument_types(NULL, app, NULL, info);

  flags = appn_flags(app->rator, info);
  SCHEME_APPN_FLAGS(app) |= flags;

  return finish_optimize_any_application((Scheme_Object *)app, app->rator, 1,
                                         info, context);
}

int scheme_eq_testable_constant(Scheme_Object *v)
{
  if (SCHEME_SYMBOLP(v)
      || SCHEME_KEYWORDP(v)
      || SCHEME_FALSEP(v)
      || SAME_OBJ(v, scheme_true)
      || SCHEME_NULLP(v)
      || SCHEME_VOIDP(v)
      || SCHEME_EOFP(v))
    return 1;

  if (SCHEME_CHARP(v) && (SCHEME_CHAR_VAL(v) < 256))
    return 1;

  if (SCHEME_INTP(v) 
      && IN_FIXNUM_RANGE_ON_ALL_PLATFORMS(SCHEME_INT_VAL(v)))
    return 1;

  return 0;
}

static Scheme_Object *optimize_application3(Scheme_Object *o, Optimize_Info *info, int context)
{
  Scheme_App3_Rec *app;
  Scheme_Object *le;
  int rator_flags = 0, rator_apply_escapes, sub_context, ty, flags;
  Optimize_Info_Sequence info_seq;

  app = (Scheme_App3_Rec *)o;

  if (SAME_OBJ(app->rator, scheme_check_not_undefined_proc)
      && SCHEME_SYMBOLP(app->rand2)) {
    scheme_log(info->logger,
               SCHEME_LOG_DEBUG,
               0,
               "warning%s: use-before-definition check inserted on variable: %S",
               scheme_optimize_context_to_string(info->context),
               app->rand2);
  }

  /* Check for (apply ... (list ...)) early: */
  le = direct_apply((Scheme_Object *)app, app->rator, app->rand2, info);
  if (le)
    return scheme_optimize_expr(le, info, context);

  le = check_app_let_rator(o, app->rator, info, 2, context);
  if (le) 
    return le;

  le = optimize_for_inline(info, app->rator, 2, NULL, NULL, app, &rator_flags, context, 0, 0);
  if (le)
    return le;

  optimize_info_seq_init(info, &info_seq);

  sub_context = OPT_CONTEXT_SINGLED;

  le = scheme_optimize_expr(app->rator, info, sub_context);
  app->rator = le;
  if (info->escapes) {
    optimize_info_seq_done(info, &info_seq);
    return app->rator;
  }

  {
    /* Maybe found "((lambda" after optimizing; try again */
    le = optimize_for_inline(info, app->rator, 2, NULL, NULL, app, &rator_flags, context, 1, 0);
    if (le)
      return le;
    rator_apply_escapes = info->escapes;
  }

  /* 1st arg */

  ty = wants_local_type_arguments(app->rator, 0);
  if (ty)
    sub_context |= (ty << OPT_CONTEXT_TYPE_SHIFT);

  optimize_info_seq_step(info, &info_seq);

  le = scheme_optimize_expr(app->rand1, info, sub_context);
  app->rand1 = le;
  if (info->escapes) {
    info->size += 1;
    return make_discarding_first_sequence(app->rator, app->rand1, info, 0);
  }

  /* 2nd arg */

  ty = wants_local_type_arguments(app->rator, 1);
  if (ty)
    sub_context |= (ty << OPT_CONTEXT_TYPE_SHIFT);
  else
    sub_context &= ~OPT_CONTEXT_TYPE_MASK;

  optimize_info_seq_step(info, &info_seq);

  le = scheme_optimize_expr(app->rand2, info, sub_context);
  app->rand2 = le;
  optimize_info_seq_done(info, &info_seq);
  if (info->escapes) {
    info->size += 1;
    return make_discarding_first_sequence(app->rator,
                                          make_discarding_first_sequence(app->rand1, app->rand2,
                                                                         info, 0),
                                             info, 0);
  }

  /* Check for (apply ... (list ...)) after some optimizations: */
  le = direct_apply((Scheme_Object *)app, app->rator, app->rand2, info);
  if (le) return finish_optimize_app(le, info, context, rator_flags);

  flags = appn_flags(app->rator, info);
  SCHEME_APPN_FLAGS(app) |= flags;

  if (rator_apply_escapes) {
   info->escapes = 1;
   SCHEME_APPN_FLAGS(app) |= (APPN_FLAG_IMMED | APPN_FLAG_SFS_TAIL);
  }

  return finish_optimize_application3(app, info, context, rator_flags);
}

static Scheme_Object *finish_optimize_application3(Scheme_App3_Rec *app, Optimize_Info *info, int context, int rator_flags)
{
  int flags;
  Scheme_Object *le;
  int all_vals = 1;
  int id_offset = 0;

  info->size += 1;

  if (SCHEME_TYPE(app->rand1) < _scheme_compiled_values_types_)
    all_vals = 0;
  if (SCHEME_TYPE(app->rand2) < _scheme_compiled_values_types_)
    all_vals = 0;


  if (all_vals) {
    le = try_optimize_fold(app->rator, NULL, (Scheme_Object *)app, info);
    if (le)
      return le;
  }

  if (!is_nonmutating_primitive(app->rator, 2))
    info->vclock += 1;
  if (!is_noncapturing_primitive(app->rator, 2))
    info->kclock += 1;
  if (!is_nonsaving_primitive(app->rator, 2))
    info->sclock += 1;

  /* Check for (call-with-values (lambda () M) N): */
  if (SAME_OBJ(app->rator, scheme_call_with_values_proc)) {
    if (SAME_TYPE(SCHEME_TYPE(app->rand1), scheme_compiled_unclosed_procedure_type)) {
      Scheme_Closure_Data *data = (Scheme_Closure_Data *)app->rand1;

      if (!data->num_params) {
        /* Convert to apply-values form: */
        return scheme_optimize_apply_values(app->rand2, data->code, info,
                                            ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_SINGLE_RESULT)
                                             ? ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_RESULT_TENTATIVE)
                                                ? -1
                                                : 1)
                                             : 0),
                                            context);
      }
    }
  }

  if (SAME_OBJ(scheme_procedure_arity_includes_proc, app->rator)) {
    if (SCHEME_INTP(app->rand2)) {
      Scheme_Object *proc;
      Scheme_Case_Lambda *cl;
      int i, cnt;

      proc = lookup_constant_proc(info, app->rand1, 0);
      if (proc) {
        if (SAME_TYPE(SCHEME_TYPE(proc), scheme_compiled_unclosed_procedure_type)) {
          cnt = 1;
          cl = NULL;
        } else {
          cl = (Scheme_Case_Lambda *)proc;
          cnt = cl->count;
        }

        for (i = 0; i < cnt; i++) {
          if (cl) proc = cl->array[i];

          if (SAME_TYPE(SCHEME_TYPE(proc), scheme_compiled_unclosed_procedure_type)) {
            Scheme_Closure_Data *data = (Scheme_Closure_Data *)proc;
            int n = SCHEME_INT_VAL(app->rand2), ok;
            if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST) {
              ok = ((data->num_params - 1) <= n);
            } else {
              ok = (data->num_params == n);
            }
            if (ok) {
              info->preserves_marks = 1;
              info->single_result = 1;
              return scheme_true;
            }
          } else {
            break;
          }
        }

        if (i == cnt) {
          info->preserves_marks = 1;
          info->single_result = 1;
          return scheme_false;
        }
      }
    }
  }

  /* Optimize `equal?' or `eqv?' test on certain types
     to `eq?'. This is especially helpful for the JIT. */
  if ((SAME_OBJ(app->rator, scheme_equal_prim)
       || SAME_OBJ(app->rator, scheme_eqv_prim))
      && (scheme_eq_testable_constant(app->rand1)
         || scheme_eq_testable_constant(app->rand2))) {
    app->rator = scheme_eq_prim;
    SCHEME_APPN_FLAGS(app) |= (APPN_FLAG_IMMED | APPN_FLAG_SFS_TAIL);
    scheme_check_leaf_rator(scheme_eq_prim, &rator_flags);

    /* eq? is foldable */
    if (all_vals) {
      le = try_optimize_fold(app->rator, NULL, (Scheme_Object *)app, info);
      if (le)
        return le;
    }
  }

  info->preserves_marks = !!(rator_flags & CLOS_PRESERVES_MARKS);
  info->single_result = !!(rator_flags & CLOS_SINGLE_RESULT);
  if (rator_flags & CLOS_RESULT_TENTATIVE) {
    info->preserves_marks = -info->preserves_marks;
    info->single_result = -info->single_result;
  }

  /* Ad hoc optimization of (unsafe-fx+ <x> 0), etc. */
  if (SCHEME_PRIMP(app->rator)
      && (SCHEME_PRIM_PROC_OPT_FLAGS(app->rator) & SCHEME_PRIM_IS_UNSAFE_NONMUTATING)) {
    int z1, z2;

    z1 = SAME_OBJ(app->rand1, scheme_make_integer(0));
    z2 = SAME_OBJ(app->rand2, scheme_make_integer(0));
    if (IS_NAMED_PRIM(app->rator, "unsafe-fx+")) {
      if (z1)
        return app->rand2;
      else if (z2)
        return app->rand1;
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-fx-")) {
      if (z2)
        return app->rand1;
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-fx*")) {
      if (z1 || z2) {
        if (z1 && z2)
          return scheme_make_integer(0);
        else if (z2)
          return make_discarding_sequence(app->rand1, scheme_make_integer(0), info, id_offset);
        else
          return make_discarding_sequence(app->rand2, scheme_make_integer(0), info, id_offset);
      }
      if (SAME_OBJ(app->rand1, scheme_make_integer(1)))
        return app->rand2;
      if (SAME_OBJ(app->rand2, scheme_make_integer(1)))
        return app->rand1;
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-fxquotient")) {
      if (z1)
        return make_discarding_sequence(app->rand2, scheme_make_integer(0), info, id_offset);
      if (SAME_OBJ(app->rand2, scheme_make_integer(1)))
        return app->rand1;
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-fxremainder")
               || IS_NAMED_PRIM(app->rator, "unsafe-fxmodulo")) {
      if (z1)
        return make_discarding_sequence(app->rand2, scheme_make_integer(0), info, id_offset);
      if (SAME_OBJ(app->rand2, scheme_make_integer(1)))
        return make_discarding_sequence(app->rand1, scheme_make_integer(0), info, id_offset);
    }

    z1 = (SCHEME_FLOATP(app->rand1) && (SCHEME_FLOAT_VAL(app->rand1) == 0.0));
    z2 = (SCHEME_FLOATP(app->rand2) && (SCHEME_FLOAT_VAL(app->rand2) == 0.0));

    if (IS_NAMED_PRIM(app->rator, "unsafe-fl+")) {
      if (z1)
        return app->rand2;
      else if (z2)
        return app->rand1;
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-fl-")) {
      if (z2)
        return app->rand1;
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-fl*")) {
      if (SCHEME_FLOATP(app->rand1) && (SCHEME_FLOAT_VAL(app->rand1) == 1.0))
        return app->rand2;
      if (SCHEME_FLOATP(app->rand2) && (SCHEME_FLOAT_VAL(app->rand2) == 1.0))
        return app->rand1;
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-fl/")) {
      if (SCHEME_FLOATP(app->rand2) && (SCHEME_FLOAT_VAL(app->rand2) == 1.0))
        return app->rand1;
    }

    /* Possible improvement: detect 0 and 1 constants even when general
       extflonum operations are not supported. */
#ifdef MZ_LONG_DOUBLE
    z1 = (SCHEME_LONG_DBLP(app->rand1) && long_double_is_zero(SCHEME_LONG_DBL_VAL(app->rand1)));
    z2 = (SCHEME_LONG_DBLP(app->rand2) && long_double_is_zero(SCHEME_LONG_DBL_VAL(app->rand2)));

    if (IS_NAMED_PRIM(app->rator, "unsafe-extfl+")) {
      if (z1)
        return app->rand2;
      else if (z2)
        return app->rand1;
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-extfl-")) {
      if (z2)
        return app->rand1;
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-extfl*")) {
      if (SCHEME_LONG_DBLP(app->rand1) && long_double_is_1(SCHEME_LONG_DBL_VAL(app->rand1)))
        return app->rand2;
      if (SCHEME_LONG_DBLP(app->rand2) && long_double_is_1(SCHEME_LONG_DBL_VAL(app->rand2)))
        return app->rand1;
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-extfl/")) {
      if (SCHEME_LONG_DBLP(app->rand2) && long_double_is_1(SCHEME_LONG_DBL_VAL(app->rand2)))
        return app->rand1;
    }
#endif
  } else if (SCHEME_PRIMP(app->rator)
             && (SCHEME_PRIM_PROC_OPT_FLAGS(app->rator) & SCHEME_PRIM_IS_BINARY_INLINED)) {
    /* Recognize combinations of bitwise operations as generating fixnums */
    if (IS_NAMED_PRIM(app->rator, "bitwise-and")
        || IS_NAMED_PRIM(app->rator, "bitwise-ior")
        || IS_NAMED_PRIM(app->rator, "bitwise-xor")) {
      if ((is_local_type_expression(app->rand1, info) == SCHEME_LOCAL_TYPE_FIXNUM)
          && (is_local_type_expression(app->rand2, info) == SCHEME_LOCAL_TYPE_FIXNUM)) {
        if (IS_NAMED_PRIM(app->rator, "bitwise-and"))
          app->rator = scheme_unsafe_fxand_proc;
        else if (IS_NAMED_PRIM(app->rator, "bitwise-ior"))
          app->rator = scheme_unsafe_fxior_proc;
        else
          app->rator = scheme_unsafe_fxxor_proc;
      }
    } else if (IS_NAMED_PRIM(app->rator, "arithmetic-shift")) {
      if (SCHEME_INTP(app->rand2) && (SCHEME_INT_VAL(app->rand2) <= 0)
          && (is_local_type_expression(app->rand1, info) == SCHEME_LOCAL_TYPE_FIXNUM)) {
        app->rator = scheme_unsafe_fxrshift_proc;
        app->rand2 = scheme_make_integer(-(SCHEME_INT_VAL(app->rand2)));
      }
    } else if (SAME_OBJ(app->rator, scheme_eq_prim)) {
      /* Try optimize: (eq? x #f) => (not x) and (eq? x '()) => (null? x) */
      if (SCHEME_FALSEP(app->rand1)) {
        info->size -= 1;
        return make_optimize_prim_application2(scheme_not_prim, app->rand2, info, context);
      } else if (SCHEME_FALSEP(app->rand2)) {
        info->size -= 1;
        return make_optimize_prim_application2(scheme_not_prim, app->rand1, info, context);
      }
      if (SCHEME_NULLP(app->rand1)) {
        info->size -= 1;
        return make_optimize_prim_application2(scheme_null_p_proc, app->rand2, info, context);
      } else if (SCHEME_NULLP(app->rand2)) {
        info->size -= 1;
        return make_optimize_prim_application2(scheme_null_p_proc, app->rand1, info, context);
      }
    }
  }

  register_local_argument_types(NULL, NULL, app, info);

  flags = appn_flags(app->rator, info);
  SCHEME_APPN_FLAGS(app) |= flags;

  return finish_optimize_any_application((Scheme_Object *)app, app->rator, 2,
                                         info, context);
}

Scheme_Object *scheme_optimize_apply_values(Scheme_Object *f, Scheme_Object *e,
                                            Optimize_Info *info,
                                            int e_single_result,
                                            int context)
/* f and e are already optimized */
{
  Scheme_Object *f_is_proc = NULL;

  info->preserves_marks = 0;
  info->single_result = 0;

  {
    Scheme_Object *rev;
    if (SAME_TYPE(SCHEME_TYPE(f), scheme_local_type)) {
      rev = optimize_reverse(info, SCHEME_LOCAL_POS(f), 1, 0);
    } else
      rev = f;

    if (rev) {
      int rator2_flags;
      Scheme_Object *o_f;
      o_f = lookup_constant_proc(info, rev, 0);
      if (!o_f)
        o_f = optimize_for_inline(info, rev, 1, NULL, NULL, NULL, &rator2_flags, context, 0, 0);

      if (o_f) {
        f_is_proc = rev;

        if (SAME_TYPE(SCHEME_TYPE(o_f), scheme_compiled_unclosed_procedure_type)) {
          Scheme_Closure_Data *data2 = (Scheme_Closure_Data *)o_f;
          int flags = SCHEME_CLOSURE_DATA_FLAGS(data2);
          info->preserves_marks = !!(flags & CLOS_PRESERVES_MARKS);
          info->single_result = !!(flags & CLOS_SINGLE_RESULT);
          if (flags & CLOS_RESULT_TENTATIVE) {
            info->preserves_marks = -info->preserves_marks;
            info->single_result = -info->single_result;
          }
        }
      }
    }
  }

  if (f_is_proc && (e_single_result > 0)) {
    /* Just make it an application (N M): */
    Scheme_App2_Rec *app2;
    Scheme_Object *cloned, *f_cloned;

    app2 = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
    app2->iso.so.type = scheme_application2_type;

    /* We'd like to try to inline here. The problem is that
       e (the argument) has been optimized already,
       which means it's in the wrong coordinate system.
       If we can shift-clone it, then it will be back in the right
       coordinates. */

    cloned = optimize_clone(1, e, info, 0, 0);
    if (cloned) {
      if (SAME_TYPE(SCHEME_TYPE(f_is_proc), scheme_compiled_unclosed_procedure_type))
        f_cloned = optimize_clone(1, f_is_proc, info, 0, 0);
      else {
        /* Otherwise, no clone is needed; in the case of a lexical
           variable, we already reversed it. */
        f_cloned = f_is_proc;
      }

      if (f_cloned) {
        app2->rator = f_cloned;
        app2->rand = cloned;
        info->inline_fuel >>= 1; /* because we've already optimized the rand */
        return optimize_application2((Scheme_Object *)app2, info, context);
      }
    }

    app2->rator = f;
    app2->rand = e;
    return (Scheme_Object *)app2;
  }

  {
    Scheme_Object *av;
    av = scheme_alloc_object();
    av->type = scheme_apply_values_type;
    SCHEME_PTR1_VAL(av) = f;
    SCHEME_PTR2_VAL(av) = e;
    return av;
  }
}

static Scheme_Object *optimize_sequence(Scheme_Object *o, Optimize_Info *info, int context)
{
  Scheme_Sequence *s = (Scheme_Sequence *)o;
  Scheme_Object *le;
  int i, count, prev_size;
  int drop = 0, preserves_marks = 0, single_result = 0;
  Optimize_Info_Sequence info_seq;

  optimize_info_seq_init(info, &info_seq);
  
  count = s->count;
  for (i = 0; i < count; i++) {
    prev_size = info->size;

    optimize_info_seq_step(info, &info_seq);
    le = scheme_optimize_expr(s->array[i], info,
                              ((i + 1 == count)
                               ? scheme_optimize_tail_context(context)
                               : 0));

    if (i + 1 == count) {
      single_result = info->single_result;
      preserves_marks = info->preserves_marks;
      s->array[i] = le;
    } else {
      if (!info->escapes) {
        /* Inlining and constant propagation can expose omittable expressions. */
        le = optimize_ignored(le, info, 0, -1, 1, 5);
        if (!le) {
          drop++;
          info->size = prev_size;
          s->array[i] = NULL;
        } else {
          s->array[i] = le;
        }
      } else {
        int j;
        
        single_result = info->single_result;
        preserves_marks = info->preserves_marks;
        /* Move to last position in case the begin form is droped */
        s->array[count - 1] = le;
        for (j = i; j < count - 1; j++) {
          drop++;
          s->array[j] = NULL;
        }
        break;
      }
    }
  }

  optimize_info_seq_done(info, &info_seq);

  info->preserves_marks = preserves_marks;
  info->single_result = single_result;

  if (drop + 1 == s->count) {
    return s->array[drop];
  } else if (drop) {
    Scheme_Sequence *s2;
    int j = 0;

    s2 = scheme_malloc_sequence(s->count - drop);
    s2->so.type = s->so.type;
    s2->count = s->count - drop;

    for (i = 0; i < s->count; i++) {
      if (s->array[i]) {
	s2->array[j++] = s->array[i];
      }
    }

    s = s2;
  }

  return (Scheme_Object *)s;
}

XFORM_NONGCING static int small_inline_number(Scheme_Object *o)
{
  if (SCHEME_BIGNUMP(o))
    return SCHEME_BIGLEN(o) < 32;
  else if (SCHEME_COMPLEXP(o))
    return (small_inline_number(scheme_complex_real_part(o))
            && small_inline_number(scheme_complex_imaginary_part(o)));
  else if (SCHEME_RATIONALP(o))
    return (small_inline_number(scheme_rational_numerator(o))
            && small_inline_number(scheme_rational_denominator(o)));
  else
    return 1;
}

#define STR_INLINE_LIMIT 256

int scheme_compiled_duplicate_ok(Scheme_Object *fb, int cross_module)
{
  return (SCHEME_VOIDP(fb)
	  || SAME_OBJ(fb, scheme_true)
	  || SCHEME_FALSEP(fb)
	  || (SCHEME_SYMBOLP(fb) 
              && (!cross_module || (!SCHEME_SYM_WEIRDP(fb)
                                    && (SCHEME_SYM_LEN(fb) < STR_INLINE_LIMIT))))
	  || (SCHEME_KEYWORDP(fb)
              && (!cross_module || (SCHEME_KEYWORD_LEN(fb) < STR_INLINE_LIMIT)))
	  || SCHEME_EOFP(fb)
	  || SCHEME_INTP(fb)
	  || SCHEME_NULLP(fb)
	  || (!cross_module && SAME_TYPE(SCHEME_TYPE(fb), scheme_local_type))
          || SCHEME_PRIMP(fb)
          /* Values that are hashed by the printer and/or interned on 
             read to avoid duplication: */
	  || SCHEME_CHARP(fb)
          || (SCHEME_CHAR_STRINGP(fb) 
              && (!cross_module || (SCHEME_CHAR_STRLEN_VAL(fb) < STR_INLINE_LIMIT)))
          || (SCHEME_BYTE_STRINGP(fb)
              && (!cross_module || (SCHEME_BYTE_STRLEN_VAL(fb) < STR_INLINE_LIMIT)))
          || SAME_TYPE(SCHEME_TYPE(fb), scheme_regexp_type)
          || (SCHEME_NUMBERP(fb)
              && (!cross_module || small_inline_number(fb))));
}

static int equivalent_exprs(Scheme_Object *a, Scheme_Object *b)
{
  if (SAME_OBJ(a, b))
    return 1;
  if (SAME_TYPE(SCHEME_TYPE(a), scheme_local_type)
      && SAME_TYPE(SCHEME_TYPE(b), scheme_local_type)
      && (SCHEME_LOCAL_POS(a) == SCHEME_LOCAL_POS(b)))
    return 1;

  return 0;
}

static void add_type(Optimize_Info *info, int pos, Scheme_Object *pred)
{
  Scheme_Hash_Tree *new_types;
  new_types = info->types;
  if (!new_types)
    new_types = scheme_make_hash_tree(0);
  new_types = scheme_hash_tree_set(new_types,
                                   scheme_make_integer(pos),
                                   pred);
  info->types = new_types;
}

static void merge_types(Optimize_Info *src_info, Optimize_Info *info, int delta)
{
  Scheme_Hash_Tree *types = src_info->types;
  Scheme_Object *pos, *pred;
  intptr_t i;

  if (!types)
    return;
  
  i = scheme_hash_tree_next(types, -1);
  while (i != -1) {
    scheme_hash_tree_index(types, i, &pos, &pred);
    add_type(info, SCHEME_INT_VAL(pos)+delta, pred);
    i = scheme_hash_tree_next(types, i);
  }
}

static int relevant_predicate(Scheme_Object *pred)
{
  /* Relevant predicates need to be disjoint for try_reduce_predicate(),
     and they need to recognize non-#f values for optimize_branch().
     list? is recognized in try_reduce_predicate as a special case*/

  return (SAME_OBJ(pred, scheme_pair_p_proc)
          || SAME_OBJ(pred, scheme_null_p_proc)
          || SAME_OBJ(pred, scheme_mpair_p_proc)
          || SAME_OBJ(pred, scheme_box_p_proc)
          || SAME_OBJ(pred, scheme_vector_p_proc)
          || SAME_OBJ(pred, scheme_procedure_p_proc)
          || SAME_OBJ(pred, scheme_syntax_p_proc)
          || SAME_OBJ(pred, scheme_fixnum_p_proc)
          || SAME_OBJ(pred, scheme_flonum_p_proc)
          || SAME_OBJ(pred, scheme_extflonum_p_proc)
          );
}

static void add_types(Scheme_Object *t, Optimize_Info *info, int fuel)
{
  if (fuel < 0)
    return;

  if (SAME_TYPE(SCHEME_TYPE(t), scheme_application2_type)) {
    Scheme_App2_Rec *app = (Scheme_App2_Rec *)t;
    if (SCHEME_PRIMP(app->rator)
        && SAME_TYPE(SCHEME_TYPE(app->rand), scheme_local_type)
        && relevant_predicate(app->rator)) {
      /* Looks like a predicate on a local variable. Record that the
         predicate succeeded, which may allow conversion of safe
         operations to unsafe operations. */
      add_type(info, SCHEME_LOCAL_POS(app->rand), app->rator);
    }
  } else if (SAME_TYPE(SCHEME_TYPE(t), scheme_branch_type)) {
    Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)t;
    if (SCHEME_FALSEP(b->fbranch)) {
      add_types(b->test, info, fuel-1);
      add_types(b->tbranch, info, fuel-1);
    }
  }
}

static int or_tentative(int x, int y)
{
  if (x && y) {
    if ((x < 0) || (y < 0))
      return -1;
    else
      return 1;
  } else {
    return 0;
  }
}

static Scheme_Object *optimize_branch(Scheme_Object *o, Optimize_Info *info, int context)
{
  Scheme_Branch_Rec *b;
  Scheme_Object *t, *tb, *fb;
  Scheme_Hash_Tree *init_types, *then_types;
  int init_vclock, init_kclock, init_sclock;
  int then_escapes, then_preserves_marks, then_single_result;
  int then_vclock, then_kclock, then_sclock;
  Optimize_Info_Sequence info_seq;

  b = (Scheme_Branch_Rec *)o;

  t = b->test;
  tb = b->tbranch;
  fb = b->fbranch;

  /* Convert (if <id> expr <id>) to (if <id> expr #f) */
  if (SAME_TYPE(SCHEME_TYPE(t), scheme_local_type)
      && SAME_TYPE(SCHEME_TYPE(fb), scheme_local_type)
      && (SCHEME_LOCAL_POS(t) == SCHEME_LOCAL_POS(fb))) {
    fb = scheme_false;
  }
  
  if (context & OPT_CONTEXT_BOOLEAN) {
    /* For test position, convert (if <id> <id> expr) to (if <id> #t expr) */
    if (SAME_TYPE(SCHEME_TYPE(t), scheme_local_type)
        && SAME_TYPE(SCHEME_TYPE(tb), scheme_local_type)
        && (SCHEME_LOCAL_POS(t) == SCHEME_LOCAL_POS(tb))) {
      tb = scheme_true;
    }

    /* Convert (if <expr> #t #f) to <expr> */
    if (SAME_OBJ(tb, scheme_true) && SAME_OBJ(fb, scheme_false))
      return scheme_optimize_expr(t, info, context);
  }

  optimize_info_seq_init(info, &info_seq);

  t = scheme_optimize_expr(t, info, OPT_CONTEXT_BOOLEAN | OPT_CONTEXT_SINGLED);

  if (info->escapes) {
    optimize_info_seq_done(info, &info_seq);
    return t;
  }

  /* Try to lift out `let`s and `begin`s around a test: */
  {
    Scheme_Object *inside = NULL, *t2 = t;
    int id_offset = 0;

    while (1) {
      extract_tail_inside(&t2, &inside, &id_offset);

      /* Try optimize: (if (not x) y z) => (if x z y) */
      if (SAME_TYPE(SCHEME_TYPE(t2), scheme_application2_type)) {
        Scheme_App2_Rec *app = (Scheme_App2_Rec *)t2;
        
        if (SAME_PTR(scheme_not_prim, app->rator)) {
          t2 = tb;
          tb = fb;
          fb = t2;
          
          t2 = app->rand;
          t = replace_tail_inside(t2, inside, t);
        } else
          break;
      } else
        break;
    }

    if (expr_implies_predicate(t2, info, id_offset, 5)) {
      /* (if (let () (cons x y)) a b) => (if (begin (let () (begin x y #<void>)) #t) a b) */
      /* all predicates recognize non-#f things */
      t2 = optimize_ignored(t2, info, id_offset, 1, 0, 5);
      t = replace_tail_inside(t2, inside, t);
      
      t2 = scheme_true;
      id_offset = 0;
      if (scheme_omittable_expr(t, 1, 5, 0, info, NULL, 0, 0, ID_OMIT)) {
        t = scheme_true;
        inside = NULL;
      } else {       
        t = make_sequence_2(t, scheme_true);
        inside = t;
      }
    }
    
    if (SCHEME_TYPE(t2) > _scheme_compiled_values_types_) {
      /* Branch is statically known */
      Scheme_Object *xb;

      optimize_info_seq_done(info, &info_seq);
      info->size -= 1;

      if (SCHEME_FALSEP(t2))
        xb = scheme_optimize_expr(fb, info, scheme_optimize_tail_context(context));
      else
        xb = scheme_optimize_expr(tb, info, scheme_optimize_tail_context(context));
      
      if (id_offset){
        replace_tail_inside(scheme_void, inside, NULL);
        /* t and xb are not 'inside' the let's, so we use id_offset = 0 */
        if (scheme_omittable_expr(t, 1, 5, 0, info, NULL, 0, 0, ID_OMIT))
          return xb;
        else
          return make_sequence_2(t, xb);
      } else {
        return replace_tail_inside(xb, inside, t);
      }
    }
  }

  optimize_info_seq_step(info, &info_seq);

  info->vclock += 1; /* model branch as clock increment */

  init_vclock = info->vclock;
  init_kclock = info->kclock;
  init_sclock = info->sclock;
  init_types = info->types;

  add_types(t, info, 5);

  tb = scheme_optimize_expr(tb, info, scheme_optimize_tail_context(context));

  then_types = info->types;
  then_preserves_marks = info->preserves_marks;
  then_single_result = info->single_result;
  then_escapes = info->escapes;
  then_vclock = info->vclock;
  then_kclock = info->kclock;
  then_sclock = info->sclock;

  info->types = init_types;
  info->vclock = init_vclock;
  info->kclock = init_kclock;
  info->sclock = init_sclock;

  optimize_info_seq_step(info, &info_seq);

  fb = scheme_optimize_expr(fb, info, scheme_optimize_tail_context(context));

  if (info->escapes && then_escapes) {
    /* both branches escaped */
    info->preserves_marks = 1;
    info->single_result = 1;
    info->kclock = init_kclock;
    info->types = init_types; /* not sure if this is necesary */

  } else if (info->escapes) {
    info->preserves_marks = then_preserves_marks;
    info->single_result = then_single_result;
    info->kclock = then_kclock;
    info->types = then_types;
    info->escapes = 0;

  } else if (then_escapes) {
    info->escapes = 0;

  } else {
    then_preserves_marks = or_tentative(then_preserves_marks, info->preserves_marks);
    info->preserves_marks = then_preserves_marks;
    then_single_result = or_tentative(then_single_result, info->single_result);
    info->single_result = then_single_result;
    if (then_kclock > info->kclock)
      info->kclock = then_kclock;
    info->types = init_types; /* could try to take an intersection here ... */
  }

  if (then_sclock > info->sclock)
    info->sclock = then_sclock;

  if ((init_vclock == then_vclock) && (init_vclock == info->vclock)) {
    /* we can rewind the vclock to just after the test, because the
       `if` as a whole has no effect */
    info->vclock--;
  }

  optimize_info_seq_done(info, &info_seq);

  /* Try optimize: (if x x #f) => x */
  if (SAME_TYPE(SCHEME_TYPE(t), scheme_local_type)
      && SAME_TYPE(SCHEME_TYPE(tb), scheme_local_type)
      && (SCHEME_LOCAL_POS(t) == SCHEME_LOCAL_POS(tb))
      && SCHEME_FALSEP(fb)) {
    info->size -= 2;
    return t;
  }

  /* Try optimize: (if x #f #t) => (not x) */
  if (SCHEME_FALSEP(tb)
      && SAME_OBJ(fb, scheme_true)) {
    info->size -= 2;
    return make_optimize_prim_application2(scheme_not_prim, t, info, context);
  }

  /* Try optimize: (if <omitable-expr> v v) => v */
  if (equivalent_exprs(tb, fb)) {
    info->size -= 1; /* could be more precise */
    return make_discarding_first_sequence(t, tb, info, 0);
  }

  /* Convert: (if (if M N #f) M2 K) => (if M (if N M2 K) K)
     for simple constants K. This is useful to expose simple
     tests to the JIT. */
  if (SAME_TYPE(SCHEME_TYPE(t), scheme_branch_type)
      && scheme_compiled_duplicate_ok(fb, 0)) {
    Scheme_Branch_Rec *b2 = (Scheme_Branch_Rec *)t;
    if (SCHEME_FALSEP(b2->fbranch)) {
      Scheme_Branch_Rec *b3;
      b3 = MALLOC_ONE_TAGGED(Scheme_Branch_Rec);
      b3->so.type = scheme_branch_type;
      b3->test = b2->tbranch;
      b3->tbranch = tb;
      b3->fbranch = fb;
      t = b2->test;
      tb = (Scheme_Object *)b3;
    }
  }

  b->test = t;
  b->tbranch = tb;
  b->fbranch = fb;

  if (OPT_BRANCH_ADDS_NO_SIZE) {
    /* Seems to work better to not to increase the size
       specifically for `if' */
  } else {
    info->size += 1;
  }

  return o;
}

static int omittable_key(Scheme_Object *k, Optimize_Info *info)
{
  /* A key is not omittable if it might refer to a chaperoned/impersonated
     continuation mark key, so that's why we pass NO_ID_OMIT: */
  return scheme_omittable_expr(k, 1, 20, 0, info, info, 0, 0, NO_ID_OMIT);
}

static Scheme_Object *optimize_wcm(Scheme_Object *o, Optimize_Info *info, int context)
{
  Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)o;
  Scheme_Object *k, *v, *b;
  int init_vclock;
  Optimize_Info_Sequence info_seq;

  optimize_info_seq_init(info, &info_seq);

  k = scheme_optimize_expr(wcm->key, info, OPT_CONTEXT_SINGLED);

  if (info->escapes) {
    optimize_info_seq_done(info, &info_seq);
    return k;
  }

  optimize_info_seq_step(info, &info_seq);

  v = scheme_optimize_expr(wcm->val, info, OPT_CONTEXT_SINGLED);

  if (info->escapes) {
    optimize_info_seq_done(info, &info_seq);
    info->size += 1;
    return make_discarding_first_sequence(k, v, info, 0);
  }

  /* The presence of a key can be detected by other expressions,
     to increment vclock to prevent expressions incorrectly
     moving under the mark: */
  info->vclock++;
  init_vclock = info->vclock;

  optimize_info_seq_step(info, &info_seq);

  b = scheme_optimize_expr(wcm->body, info, scheme_optimize_tail_context(context));

  if (init_vclock == info->vclock) {
    /* body has no effect itself, so we can rewind the clock */
    info->vclock--;
  }

  optimize_info_seq_done(info, &info_seq);

  if (omittable_key(k, info)
      && scheme_omittable_expr(b, -1, 20, 0, info, info, 0, 0, ID_OMIT))
    return make_discarding_first_sequence(v, b, info, 0);

  /* info->single_result is already set */
  info->preserves_marks = 0;

  wcm->key = k;
  wcm->val = v;
  wcm->body = b;

  info->size += 1;

  return (Scheme_Object *)wcm;
}

/*========================================================================*/
/*                            other syntax                                */
/*========================================================================*/

static Scheme_Object *
define_values_optimize(Scheme_Object *data, Optimize_Info *info, int context)
{
  Scheme_Object *vars = SCHEME_VEC_ELS(data)[0];
  Scheme_Object *val = SCHEME_VEC_ELS(data)[1];

  optimize_info_used_top(info);
  val = scheme_optimize_expr(val, info, 0);

  SCHEME_VEC_ELS(data)[0] = vars;
  SCHEME_VEC_ELS(data)[1] = val;

  return data;
}

static Scheme_Object *
set_optimize(Scheme_Object *data, Optimize_Info *info, int context)
{
  Scheme_Set_Bang *sb = (Scheme_Set_Bang *)data;
  Scheme_Object *var, *val;

  var = sb->var;
  val = sb->val;

  val = scheme_optimize_expr(val, info, OPT_CONTEXT_SINGLED);

  if (info->escapes)
      return val;

  info->preserves_marks = 1;
  info->single_result = 1;

  if (SAME_TYPE(SCHEME_TYPE(var), scheme_local_type)) {
    int pos, delta;

    pos = SCHEME_LOCAL_POS(var);

    /* Register that we use this variable: */
    optimize_info_lookup(info, pos, NULL, NULL, 0, 0, NULL, NULL);

    /* Offset: */
    delta = optimize_info_get_shift(info, pos);
    if (delta)
      var = scheme_make_local(scheme_local_type, pos + delta, 0);
  } else {
    optimize_info_used_top(info);
  }

  info->vclock++;

  sb->var = var;
  sb->val = val;

  return (Scheme_Object *)sb;
}

static Scheme_Object *
set_clone(int dup_ok, Scheme_Object *data, Optimize_Info *info, int delta, int closure_depth)
{
  Scheme_Set_Bang *sb = (Scheme_Set_Bang *)data, *naya;
  Scheme_Object *var, *val;

  naya = MALLOC_ONE_TAGGED(Scheme_Set_Bang);
  memcpy(naya, sb, sizeof(Scheme_Set_Bang));

  var = naya->var;
  val = naya->val;

  val = optimize_clone(dup_ok, val, info, delta, closure_depth);
  if (!val) return NULL;
  if (SAME_TYPE(SCHEME_TYPE(var), scheme_local_type)) {
    var = optimize_clone(dup_ok, var, info, delta, closure_depth);
    if (!var) return NULL;
  }

  naya->var = var;
  naya->val = val;

  return (Scheme_Object *)naya;
}

static Scheme_Object *set_shift(Scheme_Object *data, int delta, int after_depth)
{
  Scheme_Set_Bang *sb = (Scheme_Set_Bang *)data;
  Scheme_Object *e;

  e = optimize_shift(sb->var, delta, after_depth);
  sb->var = e;

  e = optimize_shift(sb->val, delta, after_depth);
  sb->val = e;

  return (Scheme_Object *)sb;
}

static Scheme_Object *
ref_optimize(Scheme_Object *data, Optimize_Info *info, int context)
{
  Scheme_Object *v;

  optimize_info_used_top(info);

  v = SCHEME_PTR1_VAL(data);
  if (SAME_TYPE(SCHEME_TYPE(v), scheme_local_type)) {
    int is_mutated = 0;
    optimize_info_mutated_lookup(info, SCHEME_LOCAL_POS(v), &is_mutated);
    SCHEME_PTR1_VAL(data) = (is_mutated ? scheme_false : scheme_true);
  } else if (SAME_TYPE(SCHEME_TYPE(v), scheme_compiled_toplevel_type)) {
    /* Knowing whether a top-level variable is fixed lets up optimize
       uses of `variable-reference-constant?` */
    if (info->top_level_consts) {
      int pos = SCHEME_TOPLEVEL_POS(v);
      int fixed = 0;

      if (scheme_hash_get(info->top_level_consts, scheme_make_integer(pos)))
        fixed = 1;
      else {
        GC_CAN_IGNORE Scheme_Object *t;
        t = scheme_hash_get(info->top_level_consts, scheme_false);
        if (t) {
          if (scheme_hash_get((Scheme_Hash_Table *)t, scheme_make_integer(pos)))
            fixed = 1;
        }
      }

      if (fixed) {
        v = scheme_toplevel_to_flagged_toplevel(v, SCHEME_TOPLEVEL_FIXED);
        SCHEME_PTR1_VAL(data) = v;
      }
    }
  }

  info->preserves_marks = 1;
  info->single_result = 1;
  info->size++;

  return data;
}

static Scheme_Object *
ref_shift(Scheme_Object *data, int delta, int after_depth)
{
  Scheme_Object *v;

  v = optimize_shift(SCHEME_PTR1_VAL(data), delta, after_depth);
  SCHEME_PTR1_VAL(data) = v;

  v = optimize_shift(SCHEME_PTR2_VAL(data), delta, after_depth);
  SCHEME_PTR2_VAL(data) = v;

  return data;
}

static Scheme_Object *
ref_clone(int dup_ok, Scheme_Object *data, Optimize_Info *info, int delta, int closure_depth)
{
  Scheme_Object *naya;
  Scheme_Object *a, *b;

  a = SCHEME_PTR1_VAL(data);
  a = optimize_clone(dup_ok, a, info, delta, closure_depth);
  if (!a) return NULL;

  b = SCHEME_PTR2_VAL(data);
  b = optimize_clone(dup_ok, b, info, delta, closure_depth);
  if (!b) return NULL;

  naya = scheme_alloc_object();
  naya->type = scheme_varref_form_type;
  SCHEME_PTR1_VAL(naya) = a;
  SCHEME_PTR2_VAL(naya) = b;

  return naya;
}

static Scheme_Object *
apply_values_optimize(Scheme_Object *data, Optimize_Info *info, int context)
{
  Scheme_Object *f, *e;
  Optimize_Info_Sequence info_seq;

  f = SCHEME_PTR1_VAL(data);
  e = SCHEME_PTR2_VAL(data);

  optimize_info_seq_init(info, &info_seq);

  f = scheme_optimize_expr(f, info, OPT_CONTEXT_SINGLED);

  if (info->escapes) {
    optimize_info_seq_done(info, &info_seq);
    return f;
  }
  optimize_info_seq_step(info, &info_seq);

  e = scheme_optimize_expr(e, info, 0);

  optimize_info_seq_done(info, &info_seq);

  if (info->escapes) {
    info->size += 1;
    return make_discarding_first_sequence(f, e, info, 0);
  }

  info->size += 1;
  info->vclock += 1;
  info->kclock += 1;
  info->sclock += 1;

  return scheme_optimize_apply_values(f, e, info, info->single_result, context);
}

static Scheme_Object *
apply_values_shift(Scheme_Object *data, int delta, int after_depth)
{
  Scheme_Object *e;

  e = optimize_shift(SCHEME_PTR1_VAL(data), delta, after_depth);
  SCHEME_PTR1_VAL(data) = e;

  e = optimize_shift(SCHEME_PTR2_VAL(data), delta, after_depth);
  SCHEME_PTR2_VAL(data) = e;

  return data;
}

static Scheme_Object *
apply_values_clone(int dup_ok, Scheme_Object *data, Optimize_Info *info, int delta, int closure_depth)
{
  Scheme_Object *f, *e;

  f = SCHEME_PTR1_VAL(data);
  e = SCHEME_PTR2_VAL(data);

  f = optimize_clone(dup_ok, f, info, delta, closure_depth);
  if (!f) return NULL;
  e = optimize_clone(dup_ok, e, info, delta, closure_depth);
  if (!e) return NULL;

  data = scheme_alloc_object();
  data->type = scheme_apply_values_type;
  SCHEME_PTR1_VAL(data) = f;
  SCHEME_PTR2_VAL(data) = e;

  return data;
}

static Scheme_Object *
case_lambda_optimize(Scheme_Object *expr, Optimize_Info *info, int context)
{
  Scheme_Object *le;
  int i;
  Scheme_Case_Lambda *seq = (Scheme_Case_Lambda *)expr;
  mzshort **tus, *tu;
  int *tu_lens, tup, tu_count = 0;

  if (info->transitive_use_pos) {
    /* We'll need to merge transitive_use arrays */
    tup = info->transitive_use_pos - 1;
    tus = (mzshort **)MALLOC_N(mzshort*, seq->count);
    tu_lens = (int*)MALLOC_N_ATOMIC(int, seq->count);
  } else {
    tup = 0;
    tus = NULL;
    tu_lens = NULL;
  }

  for (i = 0; i < seq->count; i++) {
    le = seq->array[i];
    le = scheme_optimize_expr(le, info, 0);
    seq->array[i] = le;

    if (tus) {
      tus[i] = info->transitive_use[tup];
      tu_lens[i] = info->transitive_use_len[tup];
      if (tus[i]) {
        tu_count += tu_lens[i];
      }
      info->transitive_use[tup] = NULL;
      info->transitive_use_len[tup] = 0;
    }
  }

  info->preserves_marks = 1;
  info->single_result = 1;
  info->size += 1;

  if (tu_count) {
    tu = MALLOC_N_ATOMIC(mzshort, tu_count);
    tu_count = 0;
    for (i = 0; i < seq->count; i++) {
      if (tus[i]) {
        memcpy(tu + tu_count, tus[i], tu_lens[i] * sizeof(mzshort));
        tu_count += tu_lens[i];
      }
    }
    info->transitive_use[tup] = tu;
    info->transitive_use_len[tup] = tu_count;
  }

  return expr;
}

static Scheme_Object *
case_lambda_clone(int dup_ok, Scheme_Object *data, Optimize_Info *info, int delta, int closure_depth)
{
  Scheme_Object *le;
  int i, sz;
  Scheme_Case_Lambda *seq = (Scheme_Case_Lambda *)data;
  Scheme_Case_Lambda *seq2;

  sz = sizeof(Scheme_Case_Lambda) + ((seq->count - mzFLEX_DELTA) * sizeof(Scheme_Object*));
  seq2 = (Scheme_Case_Lambda *)scheme_malloc_tagged(sz);
  memcpy(seq2, seq, sz);

  for (i = 0; i < seq->count; i++) {
    le = seq->array[i];
    le = optimize_clone(dup_ok, le, info, delta, closure_depth);
    if (!le) return NULL;
    seq2->array[i] = le;
  }

  return (Scheme_Object *)seq2;
}

static Scheme_Object *
case_lambda_shift(Scheme_Object *data, int delta, int after_depth)
{
  Scheme_Object *le;
  int i;
  Scheme_Case_Lambda *seq = (Scheme_Case_Lambda *)data;

  for (i = 0; i < seq->count; i++) {
    le = seq->array[i];
    le = optimize_shift(le, delta, after_depth);
    seq->array[i] = le;
  }

  return data;
}

static Scheme_Object *
begin0_optimize(Scheme_Object *obj, Optimize_Info *info, int context)
{
  int i, count, drop = 0, prev_size, single_result = 0, preserves_marks = 0, kclock = 0, sclock = 0;
  Scheme_Sequence *s = (Scheme_Sequence *)obj;
  Scheme_Object *inside = NULL, *expr, *orig_first;
  int id_offset = 0;
  Scheme_Object *le;
  Optimize_Info_Sequence info_seq;

  count = s->count;
  optimize_info_seq_init(info, &info_seq);

  for (i = 0; i < count; i++) {
    prev_size = info->size;

    optimize_info_seq_step(info, &info_seq);

    le = scheme_optimize_expr(s->array[i], 
                              info,
                              (!i
                               ? scheme_optimize_result_context(context)
                               : 0));

    if (!i) {
      single_result = info->single_result;
      preserves_marks = info->preserves_marks;
      kclock = info->kclock;
      sclock = info->sclock;
      s->array[0] = le;
    } else {
      /* Inlining and constant propagation can expose omittable expressions: */
      le = optimize_ignored(le, info, 0, -1, 1, 5);
      if (!le) {
        drop++;
        info->size = prev_size;
        s->array[i] = NULL;
      } else {
        s->array[i] = le;
      }
    }

    if (info->escapes) {
      int j;
      single_result = info->single_result;
      preserves_marks = info->preserves_marks;
      for (j = i + 1; j < count; j++) {
        drop++;
        s->array[j] = NULL;
      }
      break;
    }
  }

  optimize_info_seq_done(info, &info_seq);

  if (info->escapes) {
    /* In case of an error, optimize (begin0 ... <error> ...) => (begin ... <error>) */
    Scheme_Sequence *s2;
    int j = 0;

    info->single_result = 1;
    info->preserves_marks = 1;

    if (i != 0) {
      /* We will ignore the first expresion too */
      le = optimize_ignored(s->array[0], info, 0, -1, 1, 5);
      if (!le) {
        drop++;
        info->size = prev_size;
        s->array[0] = NULL;
      } else {
        s->array[0] = le;
      }
    }

    if ((count - drop) == 1) {
      /* If it's only one expression we can drop the begin0 */
      return s->array[i];
    }

    s2 = scheme_malloc_sequence(count - drop);
    s2->so.type = scheme_sequence_type;
    s2->count = count - drop;

    for (i = 0; i < count; i++) {
      if (s->array[i]) {
        s2->array[j++] = s->array[i];
      }
    }
    return (Scheme_Object *)s2;
  }

  info->preserves_marks = 1;
  info->single_result = single_result;

  if ((s->count - drop) == 1 && (preserves_marks == 1)) {
    /* If the first expression preserves marks we can drop the begin0 */
    return s->array[0];
  }

  expr = s->array[0];
  orig_first = s->array[0];
  extract_tail_inside(&expr, &inside, &id_offset);

  if (id_offset) {
    /* don't change the first expression if it needs to be shifted */
    inside = NULL;
    expr = s->array[0];
    id_offset = 0;
  }

  /* Try optimize (begin0 <movable> ...) => (begin ... <movable>) */
  if (movable_expression(expr, info, 0, 0, kclock != info->kclock,
                         sclock != info->sclock, 0, 50)) {
    if ((s->count - drop) == 1) {
      /* drop the begin0 */
      info->size -= 1;
      /* expr = expr */
    } else {
      Scheme_Sequence *s2;
      int j = 0;

      s2 = scheme_malloc_sequence(s->count - drop);
      s2->so.type = scheme_sequence_type;
      s2->count = s->count - drop;

      for (i = 1; i < s->count; i++) {
        if (s->array[i]) {
          s2->array[j++] = s->array[i];
        }
      }
      if (!info->escapes)
        s2->array[j++] = expr;

      expr = (Scheme_Object *)s2;
    }
  } else {
    if (drop) {
      Scheme_Sequence *s2;
      int j = 0;

      s2 = scheme_malloc_sequence(s->count - drop);
      s2->so.type = s->so.type;
      s2->count = s->count - drop;

      s2->array[j++] = expr;
      for (i = 1; i < s->count; i++) {
        if (s->array[i]) {
          s2->array[j++] = s->array[i];
        }
      }

      expr = (Scheme_Object *)s2;
    } else {
      s->array[0] = expr;
      expr = (Scheme_Object *)s;
    }
  }

  info->size += 1;

  return replace_tail_inside(expr, inside, orig_first);
}

static Scheme_Object *do_define_syntaxes_optimize(Scheme_Object *data, Optimize_Info *info)
{
  Scheme_Object *val;
  Optimize_Info *einfo;

  val = SCHEME_VEC_ELS(data)[3];

  einfo = scheme_optimize_info_create(info->cp, 0);
  if (info->inline_fuel < 0)
    einfo->inline_fuel = -1;
  einfo->logger = info->logger;

  val = scheme_optimize_expr(val, einfo, 0);

  SCHEME_VEC_ELS(data)[3] = val;

  return data;
}

static Scheme_Object *define_syntaxes_optimize(Scheme_Object *data, Optimize_Info *info, int context)
{
  return do_define_syntaxes_optimize(data, info);
}

static Scheme_Object *begin_for_syntax_optimize(Scheme_Object *data, Optimize_Info *info, int context)
{
  Scheme_Object *l, *a;
  Optimize_Info *einfo;

  l = SCHEME_VEC_ELS(data)[2];

  while (!SCHEME_NULLP(l)) {
    einfo = scheme_optimize_info_create(info->cp, 0);
    if (info->inline_fuel < 0)
      einfo->inline_fuel = -1;
    einfo->logger = info->logger;
    
    a = SCHEME_CAR(l);
    a = scheme_optimize_expr(a, einfo, 0);
    SCHEME_CAR(l) = a;

    l = SCHEME_CDR(l);
  }

  return data;
}

/*========================================================================*/
/*                    let, let-values, letrec, etc.                       */
/*========================================================================*/

static int is_liftable_prim(Scheme_Object *v, int or_escape)
{
  if (SCHEME_PRIMP(v)) {
    int opt = (((Scheme_Primitive_Proc *)v)->pp.flags & SCHEME_PRIM_OPT_MASK);
    if (opt >= SCHEME_PRIM_OPT_IMMEDIATE)
      return 1;
    if (or_escape && (opt >= SCHEME_PRIM_OPT_NONCM)) {
      if (SCHEME_PRIM_PROC_OPT_FLAGS(v) & SCHEME_PRIM_ALWAYS_ESCAPES)
        return 1;
    }
  }

  return 0;
}

int scheme_is_liftable(Scheme_Object *o, int bind_count, int fuel, int as_rator, int or_escape)
{
  Scheme_Type t = SCHEME_TYPE(o);

  if (!fuel) return 0;

  switch (t) {
  case scheme_compiled_unclosed_procedure_type:
    return !as_rator;
  case scheme_case_lambda_sequence_type:
    return !as_rator;
  case scheme_compiled_toplevel_type:
    return 1;
  case scheme_local_type:
    if (SCHEME_LOCAL_POS(o) > bind_count)
      return 1;
    break;
  case scheme_branch_type:
    {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)o;
      if (scheme_is_liftable(b->test, bind_count, fuel - 1, 0, or_escape)
	  && scheme_is_liftable(b->tbranch, bind_count, fuel - 1, as_rator, or_escape)
	  && scheme_is_liftable(b->fbranch, bind_count, fuel - 1, as_rator, or_escape))
	return 1;
    }
    break;
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)o;
      int i;
      if (!is_liftable_prim(app->args[0], or_escape))
        return 0;
      if (0) /* not resolved, yet */
        if (bind_count >= 0)
          bind_count += app->num_args;
      for (i = app->num_args + 1; i--; ) {
	if (!scheme_is_liftable(app->args[i], bind_count, fuel - 1, 1, or_escape))
	  return 0;
      }
      return 1;
    }
    break;
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)o;
      if (!is_liftable_prim(app->rator, or_escape))
        return 0;
      if (0) /* not resolved, yet */
        if (bind_count >= 0)
          bind_count += 1;
      if (scheme_is_liftable(app->rator, bind_count, fuel - 1, 1, or_escape)
	  && scheme_is_liftable(app->rand, bind_count, fuel - 1, 1, or_escape))
	return 1;
    }
    break;
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)o;
      if (!is_liftable_prim(app->rator, or_escape))
        return 0;
      if (0) /* not resolved, yet */
        if (bind_count >= 0)
          bind_count += 2;
      if (scheme_is_liftable(app->rator, bind_count, fuel - 1, 1, or_escape)
	  && scheme_is_liftable(app->rand1, bind_count, fuel - 1, 1, or_escape)
	  && scheme_is_liftable(app->rand2, bind_count, fuel - 1, 1, or_escape))
	return 1;
    }
    break;
  case scheme_compiled_let_void_type:
    {
      Scheme_Let_Header *lh = (Scheme_Let_Header *)o;
      int i;
      int post_bind = !(SCHEME_LET_FLAGS(lh) & (SCHEME_LET_RECURSIVE | SCHEME_LET_STAR));

      if (post_bind) {
        o = lh->body;
        for (i = lh->num_clauses; i--; ) {
          if (!scheme_is_liftable(((Scheme_Compiled_Let_Value *)o)->value, bind_count, fuel - 1, as_rator, or_escape))
            return 0;
          o = ((Scheme_Compiled_Let_Value *)o)->body;
        }
        if (scheme_is_liftable(o, bind_count + lh->count, fuel - 1, as_rator, or_escape))
          return 1;
      }
      break;
    }
  default:
    if (t > _scheme_compiled_values_types_)
      return 1;
  }

  return 0;
}

int scheme_compiled_propagate_ok(Scheme_Object *value, Optimize_Info *info)
{
  if (scheme_compiled_duplicate_ok(value, 0))
    return 1;

  if (SAME_TYPE(SCHEME_TYPE(value), scheme_compiled_unclosed_procedure_type)) {
    int sz;
    sz = closure_body_size((Scheme_Closure_Data *)value, 1, info, NULL);
    if ((sz >= 0) && (sz <= MAX_PROC_INLINE_SIZE))
      return 1;
   else {
     Scheme_Closure_Data *data = (Scheme_Closure_Data *)value;
     if (sz < 0)
       scheme_log(info->logger,
		  SCHEME_LOG_DEBUG,
		  0,
		  /* contains non-copyable body elements that prevent inlining */
		  "non-copyable %s size: %d threshold: %d#<separator>%s",
		  scheme_write_to_string(data->name ? data->name : scheme_false, NULL),
		  sz,
		  0, /* no sensible threshold here */
		  scheme_optimize_context_to_string(info->context));
     else
       scheme_log(info->logger,
		  SCHEME_LOG_DEBUG,
		  0,
		  /* too large to be an inlining candidate */
		  "too-large %s size: %d threshold: %d#<separator>%s",
		  scheme_write_to_string(data->name ? data->name : scheme_false, NULL),
		  sz,
		  0, /* no sensible threshold here */
		  scheme_optimize_context_to_string(info->context));
     return 0;
   }

  }

  if (SAME_TYPE(scheme_case_lambda_sequence_type, SCHEME_TYPE(value))) {
    Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)value;
    int i;
    for (i = cl->count; i--; ) {
      if (!scheme_compiled_propagate_ok(cl->array[i], info))
        return 0;
    }
    return 1;
  }

  if (SAME_TYPE(SCHEME_TYPE(value), scheme_compiled_toplevel_type)) {
    if ((SCHEME_TOPLEVEL_FLAGS(value) & SCHEME_TOPLEVEL_FLAGS_MASK) >= SCHEME_TOPLEVEL_FIXED)
      return 1;
    if (info->top_level_consts) {
      int pos;
      pos = SCHEME_TOPLEVEL_POS(value);
      value = scheme_hash_get(info->top_level_consts, scheme_make_integer(pos));
      value = no_potential_size(value);
      if (SAME_OBJ(value, scheme_constant_key)
          || (value && SAME_TYPE(SCHEME_TYPE(value), scheme_struct_proc_shape_type)))
        return 0;
      if (value)
        return 1;
    }
  }

  return 0;
}

int scheme_is_statically_proc(Scheme_Object *value, Optimize_Info *info)
{
  while (1) {
    if (SAME_TYPE(SCHEME_TYPE(value), scheme_compiled_unclosed_procedure_type))
      return 1;
    else if (SAME_TYPE(SCHEME_TYPE(value), scheme_case_lambda_sequence_type)) {
      return 1;
    } else if (SAME_TYPE(SCHEME_TYPE(value), scheme_compiled_let_void_type)) {
      /* Look for (let ([x <omittable>]) <proc>), which is generated for optional arguments. */
      Scheme_Let_Header *lh = (Scheme_Let_Header *)value;
      if (lh->num_clauses == 1) {
        Scheme_Compiled_Let_Value *lv = (Scheme_Compiled_Let_Value *)lh->body;
        if (scheme_omittable_expr(lv->value, lv->count, 20, 0, info, NULL, 0, 0, ID_OMIT)) {
          value = lv->body;
          info = NULL;
        } else
          break;
      } else
        break;
    } else
      break;
  }

  return 0;
}

Scheme_Object *scheme_make_noninline_proc(Scheme_Object *e)
{
  Scheme_Object *ni;

  ni = scheme_alloc_small_object();
  ni->type = scheme_noninline_proc_type;
  SCHEME_PTR_VAL(ni) = e;

  return ni;
}

static int is_values_apply(Scheme_Object *e, int n, Optimize_Info *info, int depth, int fuel)
{
  if (SAME_TYPE(SCHEME_TYPE(e), scheme_application_type)) {
    Scheme_App_Rec *app = (Scheme_App_Rec *)e;
    if (n != app->num_args) return 0;
    return SAME_OBJ(scheme_values_func, app->args[0]);
  } else if ((n == 1) && SAME_TYPE(SCHEME_TYPE(e), scheme_application2_type)) {
    Scheme_App2_Rec *app = (Scheme_App2_Rec *)e;
    return SAME_OBJ(scheme_values_func, app->rator);
  } else if ((n == 2) && SAME_TYPE(SCHEME_TYPE(e), scheme_application3_type)) {
    Scheme_App3_Rec *app = (Scheme_App3_Rec *)e;
    return SAME_OBJ(scheme_values_func, app->rator);
  } else if (fuel && SAME_TYPE(SCHEME_TYPE(e), scheme_branch_type)) {
    Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)e;
    if (SAME_TYPE(SCHEME_TYPE(b->test), scheme_local_type)
        && scheme_omittable_expr(b->test, 1, -1, 0, info, info, depth, 0, NO_MUTABLE_ID_OMIT)) {
      return (is_values_apply(b->tbranch, n, info, depth, 0)
              && is_values_apply(b->fbranch, n, info, depth, 0));
    }
  }

  return 0;
}

static int can_reorder_values_arguments(Scheme_Object *e, Optimize_Info *info, int skip_depth)
{
  /* We can reorder the argument as long at at most one is non-omitable,
     treating mutable variables as non-omitable for this purpose */

  if (SAME_TYPE(SCHEME_TYPE(e), scheme_application_type)) {
    Scheme_App_Rec *app = (Scheme_App_Rec *)e;
    int i, count = 0;
    for (i = app->num_args; i--; ) {
      if (scheme_omittable_expr(app->args[i+1], 1, 5, 0, info, info, skip_depth, 0, NO_MUTABLE_ID_OMIT))
        count++;
    }
    return (count >= app->num_args - 1);
  } else if (SAME_TYPE(SCHEME_TYPE(e), scheme_application2_type)) {
    /* nothing to reorder */
    return 1;
  } else if (SAME_TYPE(SCHEME_TYPE(e), scheme_application3_type)) {
    Scheme_App3_Rec *app = (Scheme_App3_Rec *)e;
    return (scheme_omittable_expr(app->rand1, 1, 5, 0, info, info, skip_depth, 0, NO_MUTABLE_ID_OMIT)
            || scheme_omittable_expr(app->rand2, 1, 5, 0, info, info, skip_depth, 0, NO_MUTABLE_ID_OMIT));
  } else if (SAME_TYPE(SCHEME_TYPE(e), scheme_branch_type)) {
    Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)e;
    if (scheme_omittable_expr(b->tbranch, -1, 5, 0, info, info, skip_depth, 0, NO_MUTABLE_ID_OMIT)) {
      return can_reorder_values_arguments(b->fbranch, info, skip_depth);
    } else if (scheme_omittable_expr(b->fbranch, -1, 5, 0, info, info, skip_depth, 0, NO_MUTABLE_ID_OMIT)) {
      return can_reorder_values_arguments(b->tbranch, info, skip_depth);
    }
  }

  return 0;
}

static int no_mutable_bindings(Scheme_Compiled_Let_Value *pre_body)
{
  int i;

  for (i = pre_body->count; i--; ) {
    if (pre_body->flags[i]  & SCHEME_WAS_SET_BANGED)
      return 0;
  }

  return 1;
}

static void update_rhs_value(Scheme_Compiled_Let_Value *naya, Scheme_Object *e,
                             Optimize_Info *info, Scheme_Object *tst)
{
  if (tst) {
    if (!equivalent_exprs(naya->value, e)) {
      Scheme_Branch_Rec *b;

      /* In case `tst` was formerly a single-use variable, mark it as multi-use: */
      (void)optimize_reverse(info, SCHEME_LOCAL_POS(tst), 0, 1);

      b = MALLOC_ONE_TAGGED(Scheme_Branch_Rec);
      b->so.type = scheme_branch_type;
      b->test = tst;
      b->tbranch = naya->value;
      b->fbranch = e;

      naya->value = (Scheme_Object *)b;
    }
  } else
    naya->value = e;
}

static void unpack_values_application(Scheme_Object *e, Scheme_Compiled_Let_Value *naya,
                                      int rev_bind_order, Optimize_Info *info, Scheme_Object *branch_test)
{
  if (SAME_TYPE(SCHEME_TYPE(e), scheme_application_type)) {
    Scheme_App_Rec *app = (Scheme_App_Rec *)e;
    int i;
    for (i = 0; i < app->num_args; i++) {
      if (rev_bind_order)
        update_rhs_value(naya, app->args[app->num_args - i], info, branch_test);
      else
        update_rhs_value(naya, app->args[i + 1], info, branch_test);
      naya = (Scheme_Compiled_Let_Value *)naya->body;
    }
  } else if (SAME_TYPE(SCHEME_TYPE(e), scheme_application2_type)) {
    Scheme_App2_Rec *app = (Scheme_App2_Rec *)e;
    update_rhs_value(naya, app->rand, info, branch_test);
  } else if (SAME_TYPE(SCHEME_TYPE(e), scheme_application3_type)) {
    Scheme_App3_Rec *app = (Scheme_App3_Rec *)e;
    update_rhs_value(naya, rev_bind_order ? app->rand2 : app->rand1, info, branch_test);
    naya = (Scheme_Compiled_Let_Value *)naya->body;
    update_rhs_value(naya, rev_bind_order ? app->rand1 : app->rand2, info, branch_test);
  } else if (SAME_TYPE(SCHEME_TYPE(e), scheme_branch_type)) {
    Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)e;

    unpack_values_application(b->tbranch, naya, rev_bind_order, info, NULL);
    unpack_values_application(b->fbranch, naya, rev_bind_order, info, b->test);
  }
}

static Scheme_Object *make_clones(Scheme_Compiled_Let_Value *retry_start,
                                  Scheme_Compiled_Let_Value *pre_body,
                                  Optimize_Info *body_info)
{
  Scheme_Compiled_Let_Value *clv;
  Scheme_Object *value, *clone, *pr;
  Scheme_Object *last = NULL, *first = NULL;

  clv = retry_start;
  while (1) {
    value = clv->value;
    if (IS_COMPILED_PROC(value)) {
      clone = optimize_clone(1, value, body_info, 0, 0);
      if (clone) {
        pr = scheme_make_raw_pair(scheme_make_raw_pair(value, clone), NULL);
      } else
        pr = scheme_make_raw_pair(NULL, NULL);
      if (last)
        SCHEME_CDR(last) = pr;
      else
        first = pr;
      last = pr;
    }
    if (clv == pre_body)
      break;
    clv = (Scheme_Compiled_Let_Value *)clv->body;
  }

  return first;
}

static int set_one_code_flags(Scheme_Object *value, int flags,
                              Scheme_Object *first, Scheme_Object *second,
                              int set_flags, int mask_flags, int just_tentative,
                              int merge_local_typed)
{
  Scheme_Case_Lambda *cl, *cl2, *cl3;
  Scheme_Closure_Data *data, *data2, *data3;
  int i, count;

  if (SAME_TYPE(scheme_compiled_unclosed_procedure_type, SCHEME_TYPE(value))) {
    count = 1;
    cl = NULL;
    cl2 = NULL;
    cl3 = NULL;
  } else {
    cl = (Scheme_Case_Lambda *)value;
    cl2 = (Scheme_Case_Lambda *)first;
    cl3 = (Scheme_Case_Lambda *)second;
    count = cl->count;
  }

  for (i = 0; i < count; i++) {
    if (cl) {
      data = (Scheme_Closure_Data *)cl->array[i];
      data2 = (Scheme_Closure_Data *)cl2->array[i];
      data3 = (Scheme_Closure_Data *)cl3->array[i];
    } else {
      data = (Scheme_Closure_Data *)value;
      data2 = (Scheme_Closure_Data *)first;
      data3 = (Scheme_Closure_Data *)second;
    }

    if (merge_local_typed) {
      merge_closure_local_type_map(data, data2);
      merge_closure_local_type_map(data, data3);
      merge_closure_local_type_map(data, data2);
    }

    if (!just_tentative || (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_RESULT_TENTATIVE)) {
      flags = (flags & SCHEME_CLOSURE_DATA_FLAGS(data));
      SCHEME_CLOSURE_DATA_FLAGS(data2) = set_flags | (SCHEME_CLOSURE_DATA_FLAGS(data2) & mask_flags);
      SCHEME_CLOSURE_DATA_FLAGS(data3) = set_flags | (SCHEME_CLOSURE_DATA_FLAGS(data3) & mask_flags);
    }
  }

  return flags;
}

static int set_code_flags(Scheme_Compiled_Let_Value *retry_start,
                          Scheme_Compiled_Let_Value *pre_body,
                          Scheme_Object *clones,
                          int set_flags, int mask_flags, int just_tentative,
                          int merge_local_typed)
{
  Scheme_Compiled_Let_Value *clv;
  Scheme_Object *value, *first;
  int flags = CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS;

  /* The first in a clone pair is the one that is consulted for
     references. The second one is the clone, and it's the one whose
     flags are updated by optimization. So consult the clone, and set
     flags in both. */

  clv = retry_start;
  while (clones) {
    value = clv->value;
    if (IS_COMPILED_PROC(value)) {
      first = SCHEME_CAR(clones);

      if (first)
        flags = set_one_code_flags(value, flags,
                                   SCHEME_CAR(first), SCHEME_CDR(first),
                                   set_flags, mask_flags, just_tentative,
                                   merge_local_typed);

      clones = SCHEME_CDR(clones);
    }

    if (clv == pre_body)
      break;
    clv = (Scheme_Compiled_Let_Value *)clv->body;
  }

  return flags;
}

static int compiled_proc_body_size(Scheme_Object *o, int less_args)
{
  int bsz;

  if (SAME_TYPE(SCHEME_TYPE(o), scheme_compiled_unclosed_procedure_type)) {
    bsz = closure_body_size((Scheme_Closure_Data *)o, 0, NULL, NULL);
    if (less_args) bsz -= ((Scheme_Closure_Data *)o)->num_params;
    return bsz;
  } else if (SAME_TYPE(SCHEME_TYPE(o), scheme_case_lambda_sequence_type)) {
    Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)o;
    int i, sz = 0;
    for (i = cl->count; i--; ) {
      bsz = closure_body_size((Scheme_Closure_Data *)cl->array[i], 0, NULL, NULL);
      if (less_args) {
        bsz -= ((Scheme_Closure_Data *)cl->array[i])->num_params;
        if (bsz > sz) sz = bsz;
      } else
        sz += bsz;
    }
    return sz;
  } else
    return 0;
}

static int expr_size(Scheme_Object *o, Optimize_Info *info)
{
  return compiled_proc_body_size(o, 0) + 1;
}

int scheme_might_invoke_call_cc(Scheme_Object *value)
{
  return !scheme_is_liftable(value, -1, 10, 0, 1);
}

static int worth_lifting(Scheme_Object *v)
{
  Scheme_Type lhs;
  lhs = SCHEME_TYPE(v);
  if ((lhs == scheme_compiled_unclosed_procedure_type)
      || (lhs == scheme_case_lambda_sequence_type)
      || (lhs == scheme_local_type)
      || (lhs == scheme_compiled_toplevel_type)
      || (lhs == scheme_compiled_quote_syntax_type)
      || (lhs > _scheme_compiled_values_types_))
    return 1;
  return 0;
}

Scheme_Object *
scheme_optimize_lets(Scheme_Object *form, Optimize_Info *info, int for_inline, int context)
{
  Optimize_Info *sub_info, *body_info, *rhs_info;
  Optimize_Info_Sequence info_seq;
  Scheme_Let_Header *head = (Scheme_Let_Header *)form;
  Scheme_Compiled_Let_Value *clv, *pre_body, *retry_start, *prev_body;
  Scheme_Object *body, *value, *ready_pairs = NULL, *rp_last = NULL, *ready_pairs_start;
  Scheme_Once_Used *first_once_used = NULL, *last_once_used = NULL, *once_used;
  int i, j, pos, is_rec, not_simply_let_star = 0, undiscourage, split_shift, skip_opts = 0;
  int did_set_value, checked_once, skip_depth, unused_clauses, found_escapes;
  int remove_last_one = 0, inline_fuel, rev_bind_order;
  int post_bind = !(SCHEME_LET_FLAGS(head) & (SCHEME_LET_RECURSIVE | SCHEME_LET_STAR));

# define pos_EARLIER(a, b) (rev_bind_order ? ((a) > (b)) : ((a) < (b)))

  if (context & OPT_CONTEXT_BOOLEAN) {
    /* Special case: (let ([x M]) (if x x N)), where x is not in N,
       to (if M #t N), since we're in a test position. */
    if (!(SCHEME_LET_FLAGS(head) & SCHEME_LET_RECURSIVE) && (head->count == 1) && (head->num_clauses == 1)) {
      clv = (Scheme_Compiled_Let_Value *)head->body;
      if (SAME_TYPE(SCHEME_TYPE(clv->body), scheme_branch_type)
          && (((clv->flags[0] & SCHEME_USE_COUNT_MASK) >> SCHEME_USE_COUNT_SHIFT)
              == 2)) {
        Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)clv->body;
        if (SAME_TYPE(SCHEME_TYPE(b->test), scheme_local_type)
            && SAME_TYPE(SCHEME_TYPE(b->tbranch), scheme_local_type)
            && !SCHEME_LOCAL_POS(b->test)
            && !SCHEME_LOCAL_POS(b->tbranch)) {
          Scheme_Branch_Rec *b3;

          b3 = MALLOC_ONE_TAGGED(Scheme_Branch_Rec);
          b3->so.type = scheme_branch_type;
          b3->test = clv->value;
          b3->tbranch = scheme_true;
          if (post_bind) {
            /* still need a `let' around N: */
            b3->fbranch = (Scheme_Object *)head;
            clv->value = scheme_false;
            clv->flags[0] = 0; /* variable now unused */
            clv->body = b->fbranch;
          } else {
            b3->fbranch = b->fbranch;
          }

          if (post_bind)
            sub_info = info;
          else
            sub_info = optimize_info_add_frame(info, 1, 0, 0);

          form = scheme_optimize_expr((Scheme_Object *)b3, sub_info, context);

          if (!post_bind) {
            info->single_result = sub_info->single_result;
            info->preserves_marks = sub_info->preserves_marks;
            optimize_info_done(sub_info, NULL);
          }

          return form;
        }
      }
    }
  }

  /* Special case: (let ([x E]) x) where E is lambda, case-lambda, or
     a constant. (If we allowed arbitrary E here, it would affect the
     tailness of E.) */
  if (!(SCHEME_LET_FLAGS(head) & SCHEME_LET_RECURSIVE) && (head->count == 1) && (head->num_clauses == 1)) {
    clv = (Scheme_Compiled_Let_Value *)head->body;
    if (SAME_TYPE(SCHEME_TYPE(clv->body), scheme_local_type)
        && (((Scheme_Local *)clv->body)->position == 0)) {
      if (worth_lifting(clv->value)) {
        if (post_bind) {
          /* Just drop the let */
          return scheme_optimize_expr(clv->value, info, context);
	} else {
          info = optimize_info_add_frame(info, 1, 0, 0);
          body = scheme_optimize_expr(clv->value, info, context);
          info->next->single_result = info->single_result;
          info->next->preserves_marks = info->preserves_marks;
          optimize_info_done(info, NULL);
          return body;
	}
      }
    }
  }

  is_rec = (SCHEME_LET_FLAGS(head) & SCHEME_LET_RECURSIVE);

  if (!is_rec) {
    int try_again;
    do {
      try_again = 0;
      /* (let ([x (let~ ([y M]) N)]) P) => (let~ ([y M]) (let ([x N]) P))
         or (let ([x (begin M ... N)]) P) => (begin M ... (let ([x N]) P)) */
      if (post_bind) {
        if (head->num_clauses == 1) {
          clv = (Scheme_Compiled_Let_Value *)head->body; /* ([x ...]) */
          if (SAME_TYPE(SCHEME_TYPE(clv->value), scheme_compiled_let_void_type)) {
            Scheme_Let_Header *lh = (Scheme_Let_Header *)clv->value; /* (let~ ([y ...]) ...) */

            value = clv->body; /* = P */
            if (lh->count)
              value = optimize_shift(value, lh->count, head->count);
            if (value) {
              clv->body = value;

              if (!lh->num_clauses) {
                clv->value = lh->body;
                lh->body = (Scheme_Object *)head;
              } else {
                body = lh->body;
                for (i = lh->num_clauses - 1; i--; ) {
                  body = ((Scheme_Compiled_Let_Value *)body)->body;
                }
                clv->value = ((Scheme_Compiled_Let_Value *)body)->body; /* N */
                ((Scheme_Compiled_Let_Value *)body)->body = (Scheme_Object *)head;
              }

              head = lh;
              form = (Scheme_Object *)head;
              is_rec = (SCHEME_LET_FLAGS(head) & SCHEME_LET_RECURSIVE);
              post_bind = !(SCHEME_LET_FLAGS(head) & (SCHEME_LET_RECURSIVE | SCHEME_LET_STAR));
              try_again = 1;
            }
          } else if (SAME_TYPE(SCHEME_TYPE(clv->value), scheme_sequence_type)) {
            Scheme_Sequence *seq = (Scheme_Sequence *)clv->value; /* (begin M ... N) */

            clv->value = seq->array[seq->count - 1];
            seq->array[seq->count - 1] = (Scheme_Object *)head;

            return scheme_optimize_expr((Scheme_Object *)seq, info, context);
          }
        }
      }
    } while (try_again);
  }

  split_shift = 0;
  if (is_rec) {
    /* Check whether we should break a prefix out into its own
       letrec set. */
    body = head->body;
    j = 0;
    for (i = 0; i < head->num_clauses - 1; i++) {
      pre_body = (Scheme_Compiled_Let_Value *)body;
      if (SCHEME_CLV_FLAGS(pre_body) & SCHEME_CLV_NO_GROUP_LATER_USES) {
        /* yes --- break group here */
        Scheme_Let_Header *h2;

        j += pre_body->count;
        i++;

        h2 = MALLOC_ONE_TAGGED(Scheme_Let_Header);
        h2->iso.so.type = scheme_compiled_let_void_type;
        h2->count = head->count - j;
        h2->num_clauses = head->num_clauses - i;
        h2->body = pre_body->body;
        SCHEME_LET_FLAGS(h2) = SCHEME_LET_RECURSIVE;

        head->count = j;
        head->num_clauses = i;

        pre_body->body = (Scheme_Object *)h2;

        split_shift = h2->count;

        body = head->body;
        for (j = 0; j < i; j++) {
          pre_body = (Scheme_Compiled_Let_Value *)body;
          pre_body->position -= split_shift;
          body = pre_body->body;
        }

        break;
      } else {
        j += pre_body->count;
        body = pre_body->body;
      }
    }
  }

  body_info = optimize_info_add_frame(info, head->count, head->count,
                                      post_bind ? SCHEME_POST_BIND_FRAME : 0);
  if (post_bind)
    rhs_info = optimize_info_add_frame(info, 0, 0, 0);
  else if (split_shift)
    rhs_info = optimize_info_add_frame(body_info, split_shift, 0, 0);
  else
    rhs_info = body_info;

  body = head->body;
  for (i = head->num_clauses; i--; ) {
    pre_body = (Scheme_Compiled_Let_Value *)body;
    pos = pre_body->position;
    for (j = pre_body->count; j--; ) {
      if (pre_body->flags[j] & SCHEME_WAS_SET_BANGED) {
	optimize_mutated(body_info, pos + j);
      } else if (is_rec) {
        /* Indicate that it's not yet ready, so it cannot be inlined: */
        Scheme_Object *rp;
        rp = scheme_make_raw_pair(scheme_false, NULL);
        if (rp_last)
          SCHEME_CDR(rp_last) = rp;
        else
          ready_pairs = rp;
        rp_last = rp;
        optimize_propagate(body_info, pos+j, rp_last, 0);
      }
    }
    body = pre_body->body;
  }

  if (OPT_ESTIMATE_FUTURE_SIZES) {
    if (is_rec && !body_info->letrec_not_twice) {
      /* For each identifier bound to a procedure, register an initial
         size estimate, which is used to discourage early loop unrolling
         at the expense of later inlining. */
      body = head->body;
      pre_body = NULL;
      for (i = head->num_clauses; i--; ) {
        pre_body = (Scheme_Compiled_Let_Value *)body;
        pos = pre_body->position;

        if ((pre_body->count == 1)
            && IS_COMPILED_PROC(pre_body->value)
            && !(pre_body->flags[0] & SCHEME_WAS_SET_BANGED)) {
          optimize_propagate(body_info, pos, estimate_closure_size(pre_body->value), 0);
        }

        body = pre_body->body;
      }
      rhs_info->use_psize = 1;
    }
  }

  rev_bind_order = 0;
  if (is_rec)
    rev_bind_order = 1;
  else if (head->num_clauses > 1) {
    int pos;
    body = head->body;
    pre_body = (Scheme_Compiled_Let_Value *)body;
    pos = pre_body->position;
    body = pre_body->body;
    for (i = head->num_clauses - 1; i--; ) {
      pre_body = (Scheme_Compiled_Let_Value *)body;
      if (pre_body->position < pos) {
        rev_bind_order = 1;
        break;
      } else if (pre_body->position > pos) {
        break;
      }
      body = pre_body->body;
    }
  }

  optimize_info_seq_init(rhs_info, &info_seq);

  prev_body = NULL;
  body = head->body;
  pre_body = NULL;
  retry_start = NULL;
  ready_pairs_start = NULL;
  did_set_value = 0;
  found_escapes = 0;
  for (i = head->num_clauses; i--; ) {
    pre_body = (Scheme_Compiled_Let_Value *)body;
    pos = pre_body->position;

    if ((pre_body->count == 1)
        && IS_COMPILED_PROC(pre_body->value)
        && !optimize_is_used(body_info, pos)) {
      if (!body_info->transitive_use) {
        mzshort **tu;
        int *tu_len;
        tu = (mzshort **)scheme_malloc(sizeof(mzshort *) * head->count);
        tu_len = (int *)scheme_malloc_atomic(sizeof(int) * head->count);
        memset(tu_len, 0, sizeof(int) * head->count);
        body_info->transitive_use = tu;
        body_info->transitive_use_len = tu_len;
      }
      body_info->transitive_use_pos = pos + 1;
    }

    if (is_rec && OPT_DISCOURAGE_EARLY_INLINE && !rhs_info->letrec_not_twice
        && IS_COMPILED_PROC(pre_body->value)) {
      inline_fuel = rhs_info->inline_fuel;
      if (inline_fuel > 2)
        rhs_info->inline_fuel = 2;
      rhs_info->letrec_not_twice++;
      undiscourage = 1;
    } else {
      inline_fuel = 0;
      undiscourage = 0;
    }

    if (!skip_opts) {
      if (!found_escapes) {
        optimize_info_seq_step(rhs_info, &info_seq);
        value = scheme_optimize_expr(pre_body->value, rhs_info,
                                     ((pre_body->count == 1)
                                      ? OPT_CONTEXT_SINGLED
                                      : 0));
        pre_body->value = value;
        if (rhs_info->escapes)
          found_escapes = 1;
      } else {
        optimize_info_seq_step(rhs_info, &info_seq);
        value = scheme_false;
        pre_body->value = value;
        body_info->single_result = 1;
        body_info->preserves_marks = 1;
        body_info->escapes = 1;
        body_info->size++;
      }
    } else {
      value = pre_body->value;
      --skip_opts;
    }

    if (undiscourage) {
      rhs_info->inline_fuel = inline_fuel;
      --rhs_info->letrec_not_twice;
    }

    body_info->transitive_use_pos = 0;

    if (is_rec && !not_simply_let_star) {
      /* Keep track of whether we can simplify to let*: */
      if (scheme_might_invoke_call_cc(value)
          || optimize_any_uses(body_info, 0, pos+pre_body->count))
        not_simply_let_star = 1;
    }

    /* Change (let-values ([(id ...) (values e ...)]) body)
       to (let-values ([id e] ...) body) for simple e.
       The is_values_apply() and related functions also handle
       (if id (values e1 ...) (values e2 ...)) to effetcively convert to
       (values (if id e1 e2) ...) and then split the values call, since
       duplicating the id use and test is likely to pay off.
       Beware that the transformation reorders the e sequence if
       !rev_bind_order, so checks are needed to make sure that's ok. */
    skip_depth = (is_rec ? (pre_body->position + pre_body->count) : 0);
    if ((pre_body->count != 1)
        && (found_escapes
            || (is_values_apply(value, pre_body->count, rhs_info, skip_depth, 1)
        && ((!is_rec && no_mutable_bindings(pre_body)
             && (rev_bind_order
                 /* When !rev_bind_order, the transformation reorders the arguments
                    to `values`, so check that it's ok: */
                 || can_reorder_values_arguments(value, rhs_info, skip_depth)))
            /* If the right-hand side is omittable, then there are
               no side effects, so reordering is always ok. But if !rev_bind_order,
               we pass NO_MUTABLE_ID_OMIT in case some other thread is mutating
               an identifier in a way that could expose reordering: */
            || scheme_omittable_expr(value, pre_body->count, -1, 0, rhs_info, info,
                                     skip_depth, 0,
                                     rev_bind_order ? ID_OMIT : NO_MUTABLE_ID_OMIT))))) {
      if (!pre_body->count && !i) {
        /* We want to drop the clause entirely, but doing it
           here messes up the loop for letrec. So wait and
           remove it at the end. */
        remove_last_one = 1;
      } else {
        Scheme_Compiled_Let_Value *naya;
        Scheme_Object *rest = pre_body->body;
        int *new_flags;
        int cnt;

        /* This conversion may reorder the expressions. */
        if (pre_body->count) {
          if (rev_bind_order)
            cnt = 0;
          else
            cnt = pre_body->count - 1;

          while (1) {
            naya = MALLOC_ONE_TAGGED(Scheme_Compiled_Let_Value);
            naya->iso.so.type = scheme_compiled_let_value_type;
            naya->body = rest;
            naya->count = 1;
            naya->position = pre_body->position + cnt;
            new_flags = (int *)scheme_malloc_atomic(sizeof(int));
            new_flags[0] = pre_body->flags[cnt];
            naya->flags = new_flags;
            rest = (Scheme_Object *)naya;

            if (rev_bind_order) {
              cnt++;
              if (cnt >= pre_body->count)
                break;
            } else {
              if (!cnt)
                break;
              cnt--;
            }
          }
        }

        naya = (Scheme_Compiled_Let_Value *)rest;
        if (!found_escapes) {
          unpack_values_application(value, naya, rev_bind_order, rhs_info, NULL);
        } else {
          Scheme_Compiled_Let_Value *naya2 = naya;
          int i;
          for (i = 0; i < pre_body->count; i++) {
            if (!i)
              naya2->value = value;
            else
              naya2->value = scheme_false;
            naya2 = (Scheme_Compiled_Let_Value *)naya2->body;
          }
        }

        if (prev_body)
          prev_body->body = (Scheme_Object *)naya;
        else
          head->body = (Scheme_Object *)naya;
        head->num_clauses += (pre_body->count - 1);
        i += (pre_body->count - 1);
        if (pre_body->count) {
          /* We're backing up. Since the RHSs have been optimized
             already, don't re-optimize. */
          skip_opts = pre_body->count - 1;
          pre_body = naya;
          body = (Scheme_Object *)naya;
          value = pre_body->value;
          pos = pre_body->position;
        } else {
          /* We've dropped this clause entirely. */
          i++;
          if (i > 0) {
            body = (Scheme_Object *)naya;
            continue;
          } else
            break;
        }
      }
    }

    checked_once = 0;

    if ((pre_body->count == 1)
	&& !(pre_body->flags[0] & SCHEME_WAS_SET_BANGED)) {
      int indirect = 0, indirect_binding = 0;

      while (indirect < 10) {
        if (SAME_TYPE(SCHEME_TYPE(value), scheme_sequence_type)) {
          Scheme_Sequence *seq = (Scheme_Sequence *)value;
          value = seq->array[seq->count - 1];
          indirect++;
        } else if (SAME_TYPE(SCHEME_TYPE(value), scheme_compiled_let_void_type)) {
          Scheme_Let_Header *head2 = (Scheme_Let_Header *)value;
          int i;

          if (head2->num_clauses < 10) {
            value = head2->body;
            for (i = head2->num_clauses; i--; ) {
              value = ((Scheme_Compiled_Let_Value *)value)->body;
            }
          }
          indirect++;
          if (head2->count)
            indirect_binding = 1;
        } else
          break;
      }

      if (indirect_binding) {
        /* only allow constants */
        if (SCHEME_TYPE(value) < _scheme_compiled_values_types_)
          value = NULL;
      }

      if (value && SAME_TYPE(SCHEME_TYPE(value), scheme_local_type)) {
        /* Don't optimize reference to a local binding
           that's not available yet, or that's mutable. */
        int vpos;
        vpos = SCHEME_LOCAL_POS(value);
        if (!post_bind && (vpos < head->count) && !pos_EARLIER(vpos, pos))
          value = NULL;
        else {
          /* Convert value back to a pre-optimized local coordinates.
             Unless post_bind, this must be done with respect to
             body_info, not rhs_info, because we attach the value to
             body_info: */
          value = optimize_reverse(post_bind ? rhs_info : body_info, vpos, 1, 0);

          /* Double-check that the value is ready, because we might be
             nested in the RHS of a `letrec': */
          if (value)
            if (!optimize_info_is_ready(body_info, SCHEME_LOCAL_POS(value)))
              value = NULL;
        }
      }

      if (value && (scheme_compiled_propagate_ok(value, body_info))) {
        int cnt;

        if (is_rec)
          cnt = 2;
        else
          cnt = ((pre_body->flags[0] & SCHEME_USE_COUNT_MASK) >> SCHEME_USE_COUNT_SHIFT);

        optimize_propagate(body_info, pos, value, cnt == 1);
        did_set_value = 1;
        checked_once = 1;
      } else if (value && !is_rec) {
        int cnt, ct;
        Scheme_Object *pred;

        ct = scheme_expr_produces_local_type(value);
        if (ct)
          optimize_produces_local_type(body_info, pos, ct);

        if (SAME_TYPE(SCHEME_TYPE(value), scheme_local_type)) {
          /* shouldn't get here, since scheme_compiled_propagate_ok()
             should have returned true, but just in case...
             local is in unoptimized coordinates */
          pred = NULL;
        } else
          pred = expr_implies_predicate(value, rhs_info, 0, 5);
        if (pred)
          add_type(body_info, pos, pred);

        if (!indirect) {
          checked_once = 1;
          cnt = ((pre_body->flags[0] & SCHEME_USE_COUNT_MASK) >> SCHEME_USE_COUNT_SHIFT);
          if (cnt == 1) {
            /* used only once; we may be able to shift the expression to the use
               site, instead of binding to a temporary */
            once_used = make_once_used(value, pos,
                                       rhs_info->vclock, rhs_info->kclock, rhs_info->sclock,
                                       NULL);
            if (!last_once_used)
              first_once_used = once_used;
            else
              last_once_used->next = once_used;
            last_once_used = once_used;
            optimize_propagate(body_info, pos, (Scheme_Object *)once_used, 1);
          }
        }
      }
    }

    if (!checked_once) {
      /* Didn't handle once-used check in case of copy propagation, so check here. */
      int i, cnt;
      for (i = pre_body->count; i--; ) {
        if (!(pre_body->flags[i] & SCHEME_WAS_SET_BANGED)) {
          cnt = ((pre_body->flags[i] & SCHEME_USE_COUNT_MASK) >> SCHEME_USE_COUNT_SHIFT);
          if (cnt == 1) {
            /* Need to register as once-used, in case of copy propagation */
            once_used = make_once_used(NULL, pos+i,
                                       rhs_info->vclock, rhs_info->kclock, rhs_info->sclock,
                                       NULL);
            if (!last_once_used)
              first_once_used = once_used;
            else
              last_once_used->next = once_used;
            last_once_used = once_used;
            optimize_propagate(body_info, pos+i, (Scheme_Object *)once_used, 1);
          }
        }
      }
    }

    if (!retry_start) {
      retry_start = pre_body;
      ready_pairs_start = ready_pairs;
    }

    /* Re-optimize to inline letrec bindings? */
    if (is_rec
	&& !body_info->letrec_not_twice
	&& ((i < 1)
	    || (!scheme_is_compiled_procedure(((Scheme_Compiled_Let_Value *)pre_body->body)->value, 1, 1)
		&& !scheme_is_liftable(((Scheme_Compiled_Let_Value *)pre_body->body)->value, head->count, 5, 1, 0)))) {
      Scheme_Object *prop_later = NULL;

      if (did_set_value) {
        /* Next RHS ends a reorderable sequence.
           Re-optimize from retry_start to pre_body, inclusive.
           For procedures, assume CLOS_SINGLE_RESULT and CLOS_PRESERVES_MARKS for all,
           but then assume not for all if any turn out not (i.e., approximate fix point). */
        int flags;
        Scheme_Object *clones, *cl, *cl_first;
        /* Reset "ready" flags: */
        for (rp_last = ready_pairs_start; !SAME_OBJ(rp_last, ready_pairs); rp_last = SCHEME_CDR(rp_last)) {
          SCHEME_CAR(rp_last) = scheme_false;
        }
        /* Set-flags loop: */
        clones = make_clones(retry_start, pre_body, rhs_info);
        (void)set_code_flags(retry_start, pre_body, clones,
                             CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS | CLOS_RESULT_TENTATIVE,
                             0xFFFF,
                             0,
                             0);
        /* Re-optimize loop: */
        clv = retry_start;
        cl = clones;
        while (1) {
         value = clv->value;
          if (cl) {
            cl_first = SCHEME_CAR(cl);
            if (!cl_first)
              cl = SCHEME_CDR(cl);
          } else
            cl_first = NULL;
          if (cl_first && SAME_OBJ(value, SCHEME_CAR(cl_first))) {
            /* Try optimization. */
            Scheme_Object *self_value;
            int sz;
            char use_psize;

            if ((clv->count == 1)
                && rhs_info->transitive_use
                && !optimize_is_used(body_info, clv->position)) {
              body_info->transitive_use[clv->position] = NULL;
              body_info->transitive_use_pos = clv->position + 1;
            }

            cl = SCHEME_CDR(cl);
            self_value = SCHEME_CDR(cl_first);

            /* Drop old size, and remove old inline fuel: */
            sz = compiled_proc_body_size(value, 0);
            rhs_info->size -= (sz + 1);

            /* Setting letrec_not_twice prevents inlinining
               of letrec bindings in this RHS. There's a small
               chance that we miss some optimizations, but we
               avoid the possibility of N^2 behavior. */
            if (!OPT_DISCOURAGE_EARLY_INLINE)
              rhs_info->letrec_not_twice++;
            use_psize = rhs_info->use_psize;
            rhs_info->use_psize = info->use_psize;

            optimize_info_seq_step(rhs_info, &info_seq);
            value = scheme_optimize_expr(self_value, rhs_info,
                                         ((clv->count == 1)
                                          ? OPT_CONTEXT_SINGLED
                                          : 0));

            if (!OPT_DISCOURAGE_EARLY_INLINE)
              --rhs_info->letrec_not_twice;
            rhs_info->use_psize = use_psize;

            clv->value = value;

            if (!(clv->flags[0] & SCHEME_WAS_SET_BANGED)) {
              if (scheme_compiled_propagate_ok(value, rhs_info)) {
                /* Register re-optimized as the value for the binding, but
                   maybe only if it didn't grow too much: */
                int new_sz;
                if (OPT_DELAY_GROUP_PROPAGATE || OPT_LIMIT_FUNCTION_RESIZE)
                  new_sz = compiled_proc_body_size(value, 0);
                else
                  new_sz = 0;
                if (new_sz <= sz)
                  optimize_propagate(body_info, clv->position, value, 0);
                else if (!OPT_LIMIT_FUNCTION_RESIZE
                         || (new_sz < 4 * sz))
                  prop_later = scheme_make_raw_pair(scheme_make_pair(scheme_make_integer(clv->position),
                                                                     value),
                                                    prop_later);
              }
            }

            body_info->transitive_use_pos = 0;
	  }
	  if (clv == pre_body)
	    break;
          {
            /* Since letrec is really letrec*, the variables
               for this binding are now ready: */
            int i;
            for (i = clv->count; i--; ) {
              if (!(clv->flags[i] & SCHEME_WAS_SET_BANGED)) {
                SCHEME_CAR(ready_pairs_start) = scheme_true;
                ready_pairs_start = SCHEME_CDR(ready_pairs_start);
              }
            }
          }
	  clv = (Scheme_Compiled_Let_Value *)clv->body;
	}
        /* Check flags loop: */
        flags = set_code_flags(retry_start, pre_body, clones, 0, 0xFFFF, 0, 0);
        /* Reset-flags loop: */
        (void)set_code_flags(retry_start, pre_body, clones,
                             (flags & (CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS)),
                             ~(CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS | CLOS_RESULT_TENTATIVE),
                             1,
                             1);
      }
      retry_start = NULL;
      ready_pairs_start = NULL;
      did_set_value = 0;

      while (prop_later) {
        value = SCHEME_CAR(prop_later);
        optimize_propagate(body_info,
                           SCHEME_INT_VAL(SCHEME_CAR(value)),
                           SCHEME_CDR(value),
                           0);
        prop_later = SCHEME_CDR(prop_later);
      }
    }

    if (is_rec) {
      /* Since letrec is really letrec*, the variables
         for this binding are now ready: */
      int i;
      for (i = pre_body->count; i--; ) {
        if (!(pre_body->flags[i] & SCHEME_WAS_SET_BANGED)) {
          SCHEME_CAR(ready_pairs) = scheme_true;
          ready_pairs = SCHEME_CDR(ready_pairs);
        }
      }
    }

    if (remove_last_one) {
      head->num_clauses -= 1;
      body = (Scheme_Object *)pre_body->body;
      if (prev_body) {
        prev_body->body = body;
        pre_body = prev_body;
      } else {
        head->body = body;
        pre_body = NULL;
      }
      break;
    }

    prev_body = pre_body;
    body = pre_body->body;
  }

  optimize_info_seq_done(rhs_info, &info_seq);

  if (post_bind) {
    optimize_info_done(rhs_info, body_info);
    merge_types(rhs_info, body_info, head->count);
  } else if (split_shift)
    optimize_info_done(rhs_info, body_info);

  if (!found_escapes) {
    body = scheme_optimize_expr(body, body_info, scheme_optimize_tail_context(context));
  } else {
    body = scheme_false;
    body_info->single_result = 1;
    body_info->preserves_marks = 1;
    body_info->escapes = 1;
    body_info->size++;
  }
  if (head->num_clauses)
    pre_body->body = body;
  else
    head->body = body;

  info->single_result = body_info->single_result;
  info->preserves_marks = body_info->preserves_marks;
  info->vclock = body_info->vclock;
  info->kclock = body_info->kclock;
  info->sclock = body_info->sclock;

  /* Clear used flags where possible */
  body = head->body;
  unused_clauses = 0;
  for (i = head->num_clauses; i--; ) {
    int used = 0, j;

    pre_body = (Scheme_Compiled_Let_Value *)body;
    pos = pre_body->position;

    for (j = pre_body->count; j--; ) {
      if (optimize_is_used(body_info, pos+j)) {
        used = 1;
        break;
      }
    }

    if (!used
        && (scheme_omittable_expr(pre_body->value, pre_body->count, -1, 0, info, info, 0, 0, ID_OMIT)
            || ((pre_body->count == 1)
                && first_once_used
                && (first_once_used->pos == pos)
                && first_once_used->used))) {
      for (j = pre_body->count; j--; ) {
        if (pre_body->flags[j] & SCHEME_WAS_USED) {
          pre_body->flags[j] -= SCHEME_WAS_USED;
        }

        if (first_once_used && (first_once_used->pos == (pos + j)))
          first_once_used = first_once_used->next;
      }
      if (pre_body->count == 1) {
        /* Drop expr and deduct from size to aid further inlining. */
        int sz;
        sz = expr_size(pre_body->value, info);
        pre_body->value = scheme_false;
        info->size -= sz;
        unused_clauses++;
      }
    } else {
      for (j = pre_body->count; j--; ) {
        int ct;

        pre_body->flags[j] |= SCHEME_WAS_USED;
        ct = optimize_is_local_type_arg(body_info, pos+j, 0);
        if (ct) {
          if (ALWAYS_PREFER_UNBOX_TYPE(ct)
              || !optimize_escapes_after_k_tick(body_info, pos+j))
            pre_body->flags[j] |= (ct << SCHEME_WAS_TYPED_ARGUMENT_SHIFT);
        }
          

        if (first_once_used && (first_once_used->pos == (pos+j))) {
          if (first_once_used->vclock < 0) {
            /* single-use no longer true, due to copy propagation */
            pre_body->flags[j] |= SCHEME_USE_COUNT_MASK;
          }
          first_once_used = first_once_used->next;
        }
      }
      info->size += 1;
    }
    body = pre_body->body;
  }

  if (unused_clauses && (head->num_clauses == unused_clauses)) {
    /* It's worth removing the `let` wrapper and shifting the body to
       enable further optimizations outside this expression, but we risk
       quadratic work here, so use up shift fuel: */
    if (body_info->shift_fuel) {
      optimize_info_done(body_info, NULL);
      info->shift_fuel--;
      body = head->body;
      for (j = head->num_clauses; j--; ) {
        body = ((Scheme_Compiled_Let_Value *)body)->body;
      }
      return optimize_shift(body, -head->count, 0);
    }
  }

  /* Optimized away all clauses? */
  if (!head->num_clauses) {
    optimize_info_done(body_info, NULL);
    return head->body;
  }

  if (is_rec && !not_simply_let_star) {
    /* We can simplify letrec to let* */
    SCHEME_LET_FLAGS(head) -= SCHEME_LET_RECURSIVE;
    SCHEME_LET_FLAGS(head) |= SCHEME_LET_STAR;
  }

  {
    int extract_depth = 0;

    value = NULL;

    /* Check again for (let ([x <proc>]) x). */
    if (!is_rec && (head->count == 1) && (head->num_clauses == 1)) {
      clv = (Scheme_Compiled_Let_Value *)head->body;
      if (SAME_TYPE(SCHEME_TYPE(clv->body), scheme_local_type)
          && (((Scheme_Local *)clv->body)->position == 0)) {
        if (worth_lifting(clv->value)) {
          value = clv->value;
          extract_depth = 1;
        }
      }
    }

    /* Check for (let ([unused #f] ...) <proc>) */
    if (!value) {
      if (head->count == head->num_clauses) {
        body = head->body;
        for (i = head->num_clauses; i--; ) {
          pre_body = (Scheme_Compiled_Let_Value *)body;
          if ((pre_body->count != 1)
              || !SCHEME_FALSEP(pre_body->value)
              || (pre_body->flags[0] & SCHEME_WAS_USED))
            break;
          body = pre_body->body;
        }
        if (i < 0) {
          if (worth_lifting(body)) {
            value = body;
            extract_depth = head->count;
            rhs_info = body_info;
            post_bind = 0;
          }
        }
      }
    }

    if (value) {
      value = optimize_clone(1, value, rhs_info, 0, 0);

      if (value) {
        sub_info = optimize_info_add_frame(info, post_bind ? 0 : extract_depth, 0, 0);
        sub_info->inline_fuel = 0;
        value = scheme_optimize_expr(value, sub_info, context);
        info->single_result = sub_info->single_result;
        info->preserves_marks = sub_info->preserves_marks;
        optimize_info_done(sub_info, NULL);
        return value;
      }
    }
  }

  optimize_info_done(body_info, NULL);

  return form;
}

/*========================================================================*/
/*                             closures                                   */
/*========================================================================*/

static Scheme_Object *
optimize_closure_compilation(Scheme_Object *_data, Optimize_Info *info, int context)
{
  Scheme_Closure_Data *data;
  Scheme_Object *code, *ctx;
  Closure_Info *cl;
  mzshort dcs, *dcm;
  int i, cnt, init_vclock, init_kclock, init_sclock;
  Scheme_Once_Used *first_once_used = NULL, *last_once_used = NULL;

  data = (Scheme_Closure_Data *)_data;

  info->single_result = 1;
  info->preserves_marks = 1;

  info = optimize_info_add_frame(info, data->num_params, data->num_params,
                                 SCHEME_LAMBDA_FRAME);

  init_vclock = info->vclock;
  init_kclock = info->kclock;
  init_sclock = info->sclock;

  info->vclock += 1; /* model delayed evaluation as vclock increment */
  info->kclock += 1;
  info->sclock += 1;

  /* For reporting warnings: */
  if (info->context && SCHEME_PAIRP(info->context))
    ctx = scheme_make_pair((Scheme_Object *)data,
                           SCHEME_CDR(info->context));
  else if (info->context)
    ctx = scheme_make_pair((Scheme_Object *)data, info->context);
  else
    ctx = (Scheme_Object *)data;
  info->context = ctx;

  cl = (Closure_Info *)data->closure_map;
  for (i = 0; i < data->num_params; i++) {
    if (cl->local_flags[i] & SCHEME_WAS_SET_BANGED)
      optimize_mutated(info, i);

    cnt = ((cl->local_flags[i] & SCHEME_USE_COUNT_MASK) >> SCHEME_USE_COUNT_SHIFT);
    if (cnt == 1) {
      last_once_used = make_once_used(NULL, i,
                                      info->vclock, info->kclock, info->sclock,
                                      last_once_used);
      if (!first_once_used) first_once_used = last_once_used;
      optimize_propagate(info, i, (Scheme_Object *)last_once_used, 1);
    }
  }

  code = scheme_optimize_expr(data->code, info, 0);

  for (i = 0; i < data->num_params; i++) {
    int ct;
    ct = optimize_is_local_type_arg(info, i, 1);
    if (ct)
      cl->local_flags[i] |= (ct << SCHEME_WAS_TYPED_ARGUMENT_SHIFT);
  }

  while (first_once_used) {
    if (first_once_used->vclock < 0) {
      /* no longer used once, due to binding propagation */
      cl->local_flags[first_once_used->pos] |= SCHEME_USE_COUNT_MASK;
    }
    first_once_used = first_once_used->next;
  }

  if (info->single_result)
    SCHEME_CLOSURE_DATA_FLAGS(data) |= CLOS_SINGLE_RESULT;
  else if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_SINGLE_RESULT)
    SCHEME_CLOSURE_DATA_FLAGS(data) -= CLOS_SINGLE_RESULT;

  if (info->preserves_marks)
    SCHEME_CLOSURE_DATA_FLAGS(data) |= CLOS_PRESERVES_MARKS;
  else if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_PRESERVES_MARKS)
    SCHEME_CLOSURE_DATA_FLAGS(data) -= CLOS_PRESERVES_MARKS;

  if ((info->single_result > 0) && (info->preserves_marks > 0)
      && (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_RESULT_TENTATIVE))
    SCHEME_CLOSURE_DATA_FLAGS(data) -= CLOS_RESULT_TENTATIVE;

  data->code = code;

  /* Remembers positions of used vars (and unsets usage for this level) */
  env_make_closure_map(info, &dcs, &dcm);
  cl->base_closure_size = dcs;
  cl->base_closure_map = dcm;
  if (env_uses_toplevel(info))
    cl->has_tl = 1;
  else
    cl->has_tl = 0;
  cl->body_size = info->size;
  cl->body_psize = info->psize;
  cl->has_nonleaf = info->has_nonleaf;

  /* closure itself is not an effect */
  info->vclock = init_vclock;
  info->kclock = init_kclock;
  info->sclock = init_sclock;
  info->escapes = 0;

  info->size++;

  data->closure_size = (cl->base_closure_size
			+ (cl->has_tl ? 1 : 0));

  optimize_info_done(info, NULL);

  return (Scheme_Object *)data;
}

static char *get_closure_local_type_map(Scheme_Closure_Data *data, int arg_n, int *ok)
{
  Closure_Info *cl = (Closure_Info *)data->closure_map;

  if ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST)
      || (arg_n != data->num_params)) {
    *ok = 0;
    return NULL;
  }

  if (cl->has_tymap && !cl->local_type_map) {
    *ok = 0;
    return NULL;
  }

  *ok = 1;
  return cl->local_type_map;
}

static void set_closure_local_type_map(Scheme_Closure_Data *data, char *local_type_map)
{
  Closure_Info *cl = (Closure_Info *)data->closure_map;
  int i;

  if (!cl->local_type_map) {
    cl->has_tymap = 1;
    cl->local_type_map = local_type_map;
  }

  if (local_type_map) {
    for (i = data->num_params; i--; ) {
      if (local_type_map[i]) break;
    }

    if (i < 0) {
      cl->local_type_map = NULL;
    }
  }
}

static void merge_closure_local_type_map(Scheme_Closure_Data *data1, Scheme_Closure_Data *data2)
{
  Closure_Info *cl1 = (Closure_Info *)data1->closure_map;
  Closure_Info *cl2 = (Closure_Info *)data2->closure_map;

  if (cl1->has_tymap) {
    if (!cl1->local_type_map || !cl2->has_tymap) {
      cl2->has_tymap = 1;
      cl2->local_type_map = cl1->local_type_map;
    } else if (cl2->local_type_map) {
      int i, recheck = 0;
      for (i = data1->num_params; i--; ) {
        if (cl1->local_type_map[i] != cl2->local_type_map[i]) {
          cl1->local_type_map[i] = 0;
          cl2->local_type_map[i] = 0;
          recheck = 1;
        }
      }
      if (recheck) {
        for (i = data1->num_params; i--; ) {
          if (cl1->local_type_map[i]) break;
        }
        if (i < 0) {
          cl1->local_type_map = NULL;
          cl2->local_type_map = NULL;
        }
      }
    } else {
      cl1->local_type_map = NULL;
    }
  } else if (cl2->has_tymap) {
    cl1->has_tymap = 1;
    cl1->local_type_map = cl2->local_type_map;
  }
}

static Scheme_Object *clone_closure_compilation(int dup_ok, Scheme_Object *_data, Optimize_Info *info, int delta, int closure_depth)
{
  Scheme_Closure_Data *data, *data2;
  Scheme_Object *body;
  Closure_Info *cl;
  int *flags, sz;
  char *local_type_map;

  data = (Scheme_Closure_Data *)_data;

  body = optimize_clone(dup_ok, data->code, info, delta, closure_depth + data->num_params);
  if (!body) return NULL;

  data2 = MALLOC_ONE_TAGGED(Scheme_Closure_Data);
  memcpy(data2, data, sizeof(Scheme_Closure_Data));

  data2->code = body;

  cl = MALLOC_ONE_RT(Closure_Info);
  memcpy(cl, data->closure_map, sizeof(Closure_Info));
  data2->closure_map = (mzshort *)cl;

  /* We don't have to update base_closure_map, because
     it will get re-computed as the closure is re-optimized. */

  sz = sizeof(int) * data2->num_params;
  flags = (int *)scheme_malloc_atomic(sz);
  memcpy(flags, cl->local_flags, sz);
  cl->local_flags = flags;

  if (cl->local_type_map) {
    sz = data2->num_params;
    local_type_map = (char *)scheme_malloc_atomic(sz);
    memcpy(local_type_map, cl->local_type_map, sz);
    cl->local_type_map = local_type_map;
  }

  return (Scheme_Object *)data2;
}

static Scheme_Object *shift_closure_compilation(Scheme_Object *_data, int delta, int after_depth)
{
  Scheme_Object *expr;
  Scheme_Closure_Data *data = (Scheme_Closure_Data *)_data;
  Closure_Info *cl;
  int i, sz;
  mzshort *naya;

  expr = optimize_shift(data->code, delta, after_depth + data->num_params);
  data->code = expr;

  /* In case the result is not going to be re-optimized, we need
     to update base_closure_map. */

  cl = (Closure_Info *)data->closure_map;
  sz = cl->base_closure_size;
  naya = MALLOC_N_ATOMIC(mzshort, sz);

  for (i = 0; i < sz; i++) {
    naya[i] = cl->base_closure_map[i];
    if (naya[i] >= after_depth)
      naya[i] += delta;
  }

  cl->base_closure_map = naya;

  return _data;
}

static int closure_body_size(Scheme_Closure_Data *data, int check_assign,
                             Optimize_Info *info, int *is_leaf)
{
  int i;
  Closure_Info *cl;

  cl = (Closure_Info *)data->closure_map;

  if (check_assign) {
    /* Don't try to inline if any arguments are mutated: */
    for (i = data->num_params; i--; ) {
      if (cl->local_flags[i] & SCHEME_WAS_SET_BANGED)
	return -1;
    }
  }

  if (is_leaf)
    *is_leaf = !cl->has_nonleaf;

  return cl->body_size + ((info && info->use_psize) ? cl->body_psize : 0);
}

static int closure_has_top_level(Scheme_Closure_Data *data)
{
  Closure_Info *cl;

  cl = (Closure_Info *)data->closure_map;

  return cl->has_tl;
}

static int closure_argument_flags(Scheme_Closure_Data *data, int i)
{
  return ((Closure_Info *)data->closure_map)->local_flags[i];
}

/*========================================================================*/
/*                              modules                                   */
/*========================================================================*/

static int set_code_closure_flags(Scheme_Object *clones,
                                  int set_flags, int mask_flags,
                                  int just_tentative)
{
  Scheme_Object *clone, *orig, *first;
  int flags = CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS;

  /* The first in a clone pair is the one that is consulted for
     references. The second one is the original, and its the one whose
     flags are updated by optimization. So consult the original, and set
     flags in both. */

  while (clones) {
    first = SCHEME_CAR(clones);
    clone = SCHEME_CAR(first);
    orig = SCHEME_CDR(first);

    flags = set_one_code_flags(orig, flags,
                               orig, clone,
                               set_flags, mask_flags, just_tentative,
                               0);

    clones = SCHEME_CDR(clones);
  }

  return flags;
}

static Scheme_Object *is_cross_module_inline_candidiate(Scheme_Object *e, Optimize_Info *info,
                                                        int size_override)
{
  if (IS_COMPILED_PROC(e)) {
    if (size_override || (compiled_proc_body_size(e, 1) < CROSS_MODULE_INLINE_SIZE))
      return optimize_clone(0, e, info, 0, 0);
  }

  return NULL;
}

static int is_general_compiled_proc(Scheme_Object *e, Optimize_Info *info)
{
  /* recognize (begin <omitable>* <proc>) */
  if (SCHEME_TYPE(e) == scheme_sequence_type) {
    Scheme_Sequence *seq = (Scheme_Sequence *)e;
    if (seq->count > 0) {
      int i;
      for (i = seq->count - 1; i--; ) {
        if (!scheme_omittable_expr(seq->array[i], -1, 20, 0, info, NULL, 0, 0, ID_OMIT))
          return 0;
      }
    }
    e = seq->array[seq->count - 1];
  }

  /* recognize (let ([x <proc>]) x) */
  if (SCHEME_TYPE(e) == scheme_compiled_let_void_type) {
    Scheme_Let_Header *lh = (Scheme_Let_Header *)e;
    if (!(SCHEME_LET_FLAGS(lh) & SCHEME_LET_RECURSIVE)
        && (lh->count == 1) 
        && (lh->num_clauses == 1)
        && SAME_TYPE(SCHEME_TYPE(lh->body), scheme_compiled_let_value_type)) {
      Scheme_Compiled_Let_Value *lv = (Scheme_Compiled_Let_Value *)lh->body;
      if (IS_COMPILED_PROC(lv->value)) {
        if (SAME_TYPE(SCHEME_TYPE(lv->body), scheme_local_type))
          return (SCHEME_LOCAL_POS(lv->body) == 0);
      }
    }
  }

  if (IS_COMPILED_PROC(e))
    return 1;

  return 0;
}

void install_definition(Scheme_Object *vec, int pos, Scheme_Object *var, Scheme_Object *rhs)
{
  Scheme_Object *def;

  var = scheme_make_pair(var, scheme_null);
  def = scheme_make_vector(2, NULL);
  SCHEME_VEC_ELS(def)[0] = var;
  SCHEME_VEC_ELS(def)[1] = rhs;
  def->type = scheme_define_values_type;

  SCHEME_VEC_ELS(vec)[pos] = def;
}

int split_define_values(Scheme_Object *e, int n, Scheme_Object *vars, Scheme_Object *vec, int offset)
{
  if (SAME_TYPE(SCHEME_TYPE(e), scheme_compiled_let_void_type)) {
    /* This is a tedious case to recognize the pattern
         (let ([x rhs] ...) (values x ...))
       which might be the result of expansion that involved a local
       macro to define the `x's */
    Scheme_Let_Header *lh = (Scheme_Let_Header *)e;
    if ((lh->count == n) && (lh->num_clauses == n)
        && !(SCHEME_LET_FLAGS(lh) & (SCHEME_LET_RECURSIVE | SCHEME_LET_STAR))) {
      Scheme_Object *body = lh->body;
      int i;
      for (i = 0; i < n; i++) {
        if (SAME_TYPE(SCHEME_TYPE(body), scheme_compiled_let_value_type)) {
          Scheme_Compiled_Let_Value *lv = (Scheme_Compiled_Let_Value *)body;
          if (lv->count == 1) {
            if (!scheme_omittable_expr(lv->value, 1, 5, 0, NULL, NULL, n, 0, ID_OMIT))
              return 0;
            body = lv->body;
          } else
            return 0;
        } else
          return 0;
      }
      if ((n == 2) && SAME_TYPE(SCHEME_TYPE(body), scheme_application3_type)) {
        Scheme_App3_Rec *app = (Scheme_App3_Rec *)body;
        if (SAME_OBJ(app->rator, scheme_values_func)
            && SAME_TYPE(SCHEME_TYPE(app->rand1), scheme_local_type)
            && (SCHEME_LOCAL_POS(app->rand1) == 0)
            && SAME_TYPE(SCHEME_TYPE(app->rand2), scheme_local_type)
            && (SCHEME_LOCAL_POS(app->rand2) == 1)) {
          if (vars) {
            Scheme_Compiled_Let_Value *lv = (Scheme_Compiled_Let_Value *)lh->body;
            install_definition(vec, offset, SCHEME_CAR(vars), lv->value);
            vars = SCHEME_CDR(vars);
            lv = (Scheme_Compiled_Let_Value *)lv->body;
            install_definition(vec, offset+1, SCHEME_CAR(vars), lv->value);
          }
          return 1;
        }
      } else if (SAME_TYPE(SCHEME_TYPE(body), scheme_application_type)
                 && ((Scheme_App_Rec *)body)->num_args == n) {
        Scheme_App_Rec *app = (Scheme_App_Rec *)body;
        if (SAME_OBJ(app->args[0], scheme_values_func)) {
          for (i = 0; i < n; i++) {
            if (!SAME_TYPE(SCHEME_TYPE(app->args[i+1]), scheme_local_type)
                || SCHEME_LOCAL_POS(app->args[i+1]) != i)
              return 0;
          }
          if (vars) {
            body = lh->body;
            for (i = 0; i < n; i++) {
              Scheme_Compiled_Let_Value *lv = (Scheme_Compiled_Let_Value *)body;
              install_definition(vec, offset+i, SCHEME_CAR(vars), lv->value);
              vars = SCHEME_CDR(vars);
              body = lv->body;
            }
          }
          return 1;
        }
      }
    }    
  } else if ((n == 2) && SAME_TYPE(SCHEME_TYPE(e), scheme_application3_type)) {
    Scheme_App3_Rec *app = (Scheme_App3_Rec *)e;
    if (SAME_OBJ(app->rator, scheme_values_func)
        && scheme_omittable_expr(app->rand1, 1, 5, 0, NULL, NULL, 0, 0, ID_OMIT)
        && scheme_omittable_expr(app->rand2, 1, 5, 0, NULL, NULL, 0, 0, ID_OMIT)) {
      if (vars) {
        install_definition(vec, offset, SCHEME_CAR(vars), app->rand1);
        vars = SCHEME_CDR(vars);
        install_definition(vec, offset+1, SCHEME_CAR(vars), app->rand2);
      }
      return 1;
    }
  } else if (SAME_TYPE(SCHEME_TYPE(e), scheme_application_type)
             && ((Scheme_App_Rec *)e)->num_args == n) {
    Scheme_App_Rec *app = (Scheme_App_Rec *)e;
    if (SAME_OBJ(app->args[0], scheme_values_func)) {
      int i;
      for (i = 0; i < n; i++) {
        if (!scheme_omittable_expr(app->args[i+1], 1, 5, 0, NULL, NULL, 0, 0, ID_OMIT))
          return 0;
      }
      if (vars) {
        for (i = 0; i < n; i++) {
          install_definition(vec, offset+i, SCHEME_CAR(vars), app->args[i+1]);
          vars = SCHEME_CDR(vars);
        }
      }
      return 1;
    }
  }

  return 0;
}

static Scheme_Object *
module_optimize(Scheme_Object *data, Optimize_Info *info, int context)
{
  Scheme_Module *m = (Scheme_Module *)data;
  Scheme_Object *e, *vars, *old_context;
  int start_simltaneous = 0, i_m, cnt;
  Scheme_Object *cl_first = NULL, *cl_last = NULL;
  Scheme_Hash_Table *consts = NULL, *fixed_table = NULL, *re_consts = NULL;
  Scheme_Hash_Table *originals = NULL;
  int cont, next_pos_ready = -1, inline_fuel, is_proc_def;
  Comp_Prefix *prev_cp;
  Optimize_Info_Sequence info_seq;

  if (!m->comp_prefix) {
    /* already resolved */
    return (Scheme_Object *)m;
  }

  if (m->phaseless) {
    scheme_log(info->logger,
               SCHEME_LOG_DEBUG,
               0,
               "compilation of cross-phase persistent module: %D",
               m->modname);
  }

  old_context = info->context;
  info->context = (Scheme_Object *)m;

  optimize_info_seq_init(info, &info_seq);

  prev_cp = info->cp;
  info->cp = m->comp_prefix;

  cnt = SCHEME_VEC_SIZE(m->bodies[0]);

  /* First, flatten `(define-values (x ...) (values e ...))'
     to `(define (x) e) ...' when possible. */
  {
    int inc = 0;
    for (i_m = 0; i_m < cnt; i_m++) {
      e = SCHEME_VEC_ELS(m->bodies[0])[i_m];
      if (SAME_TYPE(SCHEME_TYPE(e), scheme_define_values_type))  {
        int n;
        vars = SCHEME_VEC_ELS(e)[0];
        n = scheme_list_length(vars);
        if (n > 1) {
          e = SCHEME_VEC_ELS(e)[1];
          if (split_define_values(e, n, NULL, NULL, 0))
            inc += (n - 1);
        }
      }
    }

    if (inc > 0) {
      Scheme_Object *new_vec;
      int j = 0;
      new_vec = scheme_make_vector(cnt+inc, NULL);
      for (i_m = 0; i_m < cnt; i_m++) {
        e = SCHEME_VEC_ELS(m->bodies[0])[i_m];
        if (SAME_TYPE(SCHEME_TYPE(e), scheme_define_values_type)) {
          int n;
          vars = SCHEME_VEC_ELS(e)[0];
          n = scheme_list_length(vars);
          if (n > 1) {
            if (split_define_values(SCHEME_VEC_ELS(e)[1], n, vars, new_vec, j)) {
              j += n;
            } else
              SCHEME_VEC_ELS(new_vec)[j++] = e;
          } else
            SCHEME_VEC_ELS(new_vec)[j++] = e;
        } else
          SCHEME_VEC_ELS(new_vec)[j++] = e;
      }
      cnt += inc;
      m->bodies[0] = new_vec;
    }
  }

  if (OPT_ESTIMATE_FUTURE_SIZES) {
    if (info->enforce_const) {
      /* For each identifier bound to a procedure, register an initial
         size estimate, which is used to discourage early loop unrolling
         at the expense of later inlining. */
      for (i_m = 0; i_m < cnt; i_m++) {
        e = SCHEME_VEC_ELS(m->bodies[0])[i_m];
        if (SAME_TYPE(SCHEME_TYPE(e), scheme_define_values_type))  {
          int n;

          vars = SCHEME_VEC_ELS(e)[0];
          e = SCHEME_VEC_ELS(e)[1];

          n = scheme_list_length(vars);
          if ((n == 1) && IS_COMPILED_PROC(e))  {
            Scheme_Toplevel *tl;

            tl = (Scheme_Toplevel *)SCHEME_CAR(vars);
            
            if (!(SCHEME_TOPLEVEL_FLAGS(tl) & SCHEME_TOPLEVEL_MUTATED)) {
              int pos;
              if (!consts)
                consts = scheme_make_hash_table(SCHEME_hash_ptr);
              pos = tl->position;
              scheme_hash_set(consts,
                              scheme_make_integer(pos),
                              estimate_closure_size(e));
            }
          }
        }
      }

      if (consts) {
        info->top_level_consts = consts;
        consts = NULL;
      }
    }
  }

  for (i_m = 0; i_m < cnt; i_m++) {
    /* Optimize this expression: */
    e = SCHEME_VEC_ELS(m->bodies[0])[i_m];

    is_proc_def = 0;
    if (OPT_DISCOURAGE_EARLY_INLINE && info->enforce_const) {
      if (SAME_TYPE(SCHEME_TYPE(e), scheme_define_values_type)) {
        Scheme_Object *e2;
        e2 = SCHEME_VEC_ELS(e)[1];
        if (is_general_compiled_proc(e2, info))
          is_proc_def = 1;
      }
    }

    if (is_proc_def && OPT_DISCOURAGE_EARLY_INLINE) {
      info->use_psize = 1;
      inline_fuel = info->inline_fuel;
      if (inline_fuel > 2)
        info->inline_fuel = 2;
    } else
      inline_fuel = 0;
    optimize_info_seq_step(info, &info_seq);
    e = scheme_optimize_expr(e, info, 0);
    if (is_proc_def && OPT_DISCOURAGE_EARLY_INLINE) {
      info->use_psize = 0;
      info->inline_fuel = inline_fuel;
    }
    SCHEME_VEC_ELS(m->bodies[0])[i_m] = e;

    if (info->enforce_const) {
      /* If this expression/definition can't have any side effect
	 (including raising an exception), then continue the group of
	 simultaneous definitions: */
      if (SAME_TYPE(SCHEME_TYPE(e), scheme_define_values_type))  {
	int n, cnst = 0, sproc = 0, sstruct = 0;
        Simple_Stuct_Type_Info stinfo;

	vars = SCHEME_VEC_ELS(e)[0];
	e = SCHEME_VEC_ELS(e)[1];

	n = scheme_list_length(vars);
	cont = scheme_omittable_expr(e, n, -1, 0, 
                                     /* no `info' here, because the decision
                                        of omittable should not depend on
                                        information that's only available at
                                        optimization time: */
                                     NULL, 
                                     info, 0, 0, ID_OMIT);

        if (n == 1) {
          if (scheme_compiled_propagate_ok(e, info))
            cnst = 1;
          else if (scheme_is_statically_proc(e, info)) {
            cnst = 1;
            sproc = 1;
          }
        } else if (scheme_is_simple_make_struct_type(e, n, 0, 1, NULL, 
                                                     &stinfo,
                                                     info->top_level_consts, 
                                                     NULL, NULL, 0, NULL, NULL,
                                                     5)) {
          sstruct = 1;
          cnst = 1;
        }

	if (cnst) {
	  Scheme_Toplevel *tl;
          int i;
          for (i = 0; i < n; i++) {
            tl = (Scheme_Toplevel *)SCHEME_CAR(vars);
            vars  = SCHEME_CDR(vars);

            if (!(SCHEME_TOPLEVEL_FLAGS(tl) & SCHEME_TOPLEVEL_MUTATED)) {
              Scheme_Object *e2;

              if (sstruct) {
                e2 = scheme_make_struct_proc_shape(scheme_get_struct_proc_shape(i, &stinfo));
              } else if (sproc) {
                e2 = scheme_make_noninline_proc(e);
              } else if (IS_COMPILED_PROC(e)) {
                e2 = optimize_clone(1, e, info, 0, 0);
                if (e2) {
                  Scheme_Object *pr;
                  pr = scheme_make_raw_pair(scheme_make_raw_pair(e2, e), NULL);
                  if (cl_last)
                    SCHEME_CDR(cl_last) = pr;
                  else
                    cl_first = pr;
                  cl_last = pr;
                } else
                  e2 = scheme_make_noninline_proc(e);
              } else {
                e2 = e;
              }

              if (e2) {
                int pos;
                pos = tl->position;

                consts = info->top_level_consts;
                if (!consts) {
                  consts = scheme_make_hash_table(SCHEME_hash_ptr);
                  info->top_level_consts = consts;
                }
                scheme_hash_set(consts, scheme_make_integer(pos), e2);

                if (sstruct || (SCHEME_TYPE(e2) > _scheme_compiled_values_types_)) {
                  /* No use re-optimizing */
                } else {
                  if (!re_consts)
                    re_consts = scheme_make_hash_table(SCHEME_hash_ptr);
                  scheme_hash_set(re_consts, scheme_make_integer(i_m),
                                  scheme_make_integer(pos));
                }
              } else {
                /* At least mark it as fixed */
                if (!fixed_table) {
                  fixed_table = scheme_make_hash_table(SCHEME_hash_ptr);
                  if (!info->top_level_consts) {
                    consts = scheme_make_hash_table(SCHEME_hash_ptr);
                    info->top_level_consts = consts;
                    consts = NULL;
                  }
                  scheme_hash_set(info->top_level_consts, scheme_false, (Scheme_Object *)fixed_table);
                }
                scheme_hash_set(fixed_table, scheme_make_integer(tl->position), scheme_true);
              }
            }
          }
	} else {
	  /* The binding is not inlinable/propagatable, but unless it's
	     set!ed, it is constant after evaluating the definition. We
	     map the top-level position to indicate constantness. */
	  Scheme_Object *l, *a;
	  int pos;

	  for (l = vars; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
	    a = SCHEME_CAR(l);

	    /* Test for set!: */
	    if (!(SCHEME_TOPLEVEL_FLAGS(a) & SCHEME_TOPLEVEL_MUTATED)) {
	      pos = SCHEME_TOPLEVEL_POS(a);

              next_pos_ready = pos;
	    }
	  }
	}
      } else {
	cont = scheme_omittable_expr(e, -1, -1, 0, NULL, NULL, 0, 0, ID_OMIT);
      }
      if (i_m + 1 == cnt)
	cont = 0;
    } else
      cont = 1;

    if (!cont) {
      Scheme_Object *prop_later = NULL;
      /* If we have new constants, re-optimize to inline: */
      if (consts) {
        int flags;

        /* Same as in letrec: assume CLOS_SINGLE_RESULT and
           CLOS_PRESERVES_MARKS for all, but then assume not for all
           if any turn out not (i.e., approximate fix point). */
        (void)set_code_closure_flags(cl_first,
                                     CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS | CLOS_RESULT_TENTATIVE,
                                     0xFFFF,
                                     0);

	while (1) {
	  /* Re-optimize this expression. We can optimize anything without
             shift-cloning, since there are no local variables in scope. */
          int old_sz, new_sz;

          e = SCHEME_VEC_ELS(m->bodies[0])[start_simltaneous];

          if (OPT_DELAY_GROUP_PROPAGATE || OPT_LIMIT_FUNCTION_RESIZE) {
            if (SAME_TYPE(SCHEME_TYPE(e), scheme_define_values_type)) {
              Scheme_Object *sub_e;
              sub_e = SCHEME_VEC_ELS(e)[1];
              old_sz = compiled_proc_body_size(sub_e, 0);
            } else
              old_sz = 0;
          } else
            old_sz = 0;

          optimize_info_seq_step(info, &info_seq);
          e = scheme_optimize_expr(e, info, 0);
	  SCHEME_VEC_ELS(m->bodies[0])[start_simltaneous] = e;

          if (re_consts) {
            /* Install optimized closures into constant table ---
               unless, maybe, they grow too much: */
            Scheme_Object *rpos;
            rpos = scheme_hash_get(re_consts, scheme_make_integer(start_simltaneous));
            if (rpos) {
              Scheme_Object *old_e;

              e = SCHEME_VEC_ELS(e)[1];

              old_e = scheme_hash_get(info->top_level_consts, rpos);
              if (old_e && IS_COMPILED_PROC(old_e)) {
                if (!originals)
                  originals = scheme_make_hash_table(SCHEME_hash_ptr);
                scheme_hash_set(originals, scheme_make_integer(start_simltaneous), old_e);
              }

              if (!scheme_compiled_propagate_ok(e, info)
                  && scheme_is_statically_proc(e, info)) {
                /* If we previously installed a procedure for inlining,
                   don't replace that with a worse approximation. */
                if (IS_COMPILED_PROC(old_e))
                  e = NULL;
                else
                  e = scheme_make_noninline_proc(e);
              }

              if (e) {
                if (OPT_DELAY_GROUP_PROPAGATE || OPT_LIMIT_FUNCTION_RESIZE)
                  new_sz = compiled_proc_body_size(e, 0);
                else
                  new_sz = 0;

                if (!old_sz
                    || (new_sz <= old_sz)
                    || (!OPT_DELAY_GROUP_PROPAGATE && !OPT_LIMIT_FUNCTION_RESIZE))
                  scheme_hash_set(info->top_level_consts, rpos, e);
                else if (!OPT_LIMIT_FUNCTION_RESIZE
                         || (new_sz < 4 * old_sz))
                  prop_later = scheme_make_raw_pair(scheme_make_pair(rpos, e), prop_later);
              }
            }
          }

	  if (start_simltaneous == i_m)
	    break;
          start_simltaneous++;
	}

        flags = set_code_closure_flags(cl_first, 0, 0xFFFF, 0);
        (void)set_code_closure_flags(cl_first,
                                     (flags & (CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS)),
                                     ~(CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS | CLOS_RESULT_TENTATIVE),
                                     1);
      }

      cl_last = cl_first = NULL;
      consts = NULL;
      re_consts = NULL;
      start_simltaneous = i_m + 1;

      while (prop_later) {
        e = SCHEME_CAR(prop_later);
        scheme_hash_set(info->top_level_consts, SCHEME_CAR(e), SCHEME_CDR(e));
        prop_later = SCHEME_CDR(prop_later);
      }
    }

    if (next_pos_ready > -1) {
      if (!fixed_table) {
        fixed_table = scheme_make_hash_table(SCHEME_hash_ptr);
        if (!info->top_level_consts) {
          consts = scheme_make_hash_table(SCHEME_hash_ptr);
          info->top_level_consts = consts;
          consts = NULL;
        }
        scheme_hash_set(info->top_level_consts, scheme_false, (Scheme_Object *)fixed_table);
      }
      scheme_hash_set(fixed_table, scheme_make_integer(next_pos_ready), scheme_true);
      next_pos_ready = -1;
    }
  }

  /* For functions that are potentially inlineable, perhaps 
     before optimization, insert inline_variant records: */
  if (info->enforce_const) {
    for (i_m = 0; i_m < cnt; i_m++) {
      /* Optimize this expression: */
      e = SCHEME_VEC_ELS(m->bodies[0])[i_m];
      if (SAME_TYPE(SCHEME_TYPE(e), scheme_define_values_type)) {
        int size_override;
        size_override = SCHEME_IMMUTABLEP(e);
        vars = SCHEME_VEC_ELS(e)[0];
        if (SCHEME_PAIRP(vars) && SCHEME_NULLP(SCHEME_CDR(vars))) {
          Scheme_Object *sub_e, *alt_e;
          sub_e = SCHEME_VEC_ELS(e)[1];
          alt_e = is_cross_module_inline_candidiate(sub_e, info, 0);
          if (!alt_e && originals) {
            alt_e = scheme_hash_get(originals, scheme_make_integer(i_m));
            if (SAME_OBJ(alt_e, sub_e) && !size_override)
              alt_e = NULL;
            else if (alt_e)
              alt_e = is_cross_module_inline_candidiate(alt_e, info, size_override);
          }
          if (alt_e) {
            Scheme_Object *iv;
            iv = scheme_make_vector(3, scheme_false);
            iv->type = scheme_inline_variant_type;
            SCHEME_VEC_ELS(iv)[0] = sub_e;
            SCHEME_VEC_ELS(iv)[1] = alt_e;
            SCHEME_VEC_ELS(e)[1] = iv;
          }
        }
      }
    }
  }

  /* Check one more time for expressions that we can omit: */
  {
    int can_omit = 0;
    for (i_m = 0; i_m < cnt; i_m++) {
      /* Optimize this expression: */
      e = SCHEME_VEC_ELS(m->bodies[0])[i_m];
      if (scheme_omittable_expr(e, -1, -1, 0, info, NULL, 0, 0, ID_OMIT)) {
        can_omit++;
      }
    }
    if (can_omit) {
      Scheme_Object *vec;
      int j = 0;
      vec = scheme_make_vector(cnt - can_omit, NULL);
      for (i_m = 0; i_m < cnt; i_m++) {
        /* Optimize this expression: */
        e = SCHEME_VEC_ELS(m->bodies[0])[i_m];
        if (!scheme_omittable_expr(e, -1, -1, 0, info, NULL, 0, 0, ID_OMIT)) {
          SCHEME_VEC_ELS(vec)[j++] = e;
        }
      }
      m->bodies[0] = vec;
    }
    cnt -= can_omit;
  }

  info->context = old_context;
  info->cp = prev_cp;

  /* Exp-time body was optimized during compilation */

  {
    /* optimize submodules */
    int k;
    Scheme_Object *p;
    for (k = 0; k < 2; k++) {
      p = (k ? m->post_submodules : m->pre_submodules);
      if (p) {
        while (!SCHEME_NULLP(p)) {
          optimize_info_seq_step(info, &info_seq);
          scheme_optimize_expr(SCHEME_CAR(p), info, 0);
          p = SCHEME_CDR(p);
        }
      }
    }
  }

  optimize_info_seq_done(info, &info_seq);

  info->escapes = 0;

  return data;
}

static Scheme_Object *
top_level_require_optimize(Scheme_Object *data, Optimize_Info *info, int context)
{
  return data;
}

/*========================================================================*/
/*                            expressions                                 */
/*========================================================================*/

static Scheme_Object *optimize_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *expr = (Scheme_Object *)p->ku.k.p1;
  Optimize_Info *info = (Optimize_Info *)p->ku.k.p2;
  int context = p->ku.k.i1;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return scheme_optimize_expr(expr, info, context);
}

Scheme_Object *scheme_optimize_expr(Scheme_Object *expr, Optimize_Info *info, int context)
{
  Scheme_Type type = SCHEME_TYPE(expr);

#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Thread *p = scheme_current_thread;

    p->ku.k.p1 = (void *)expr;
    p->ku.k.p2 = (void *)info;
    p->ku.k.i1 = context;

    return scheme_handle_stack_overflow(optimize_k);
  }
#endif

  info->preserves_marks = 1;
  info->single_result = 1;
  info->escapes = 0;

  switch (type) {
  case scheme_local_type:
    {
      Scheme_Object *val;
      int pos, delta, is_mutated = 0;

      info->size += 1;

      pos = SCHEME_LOCAL_POS(expr);

      val = optimize_info_lookup(info, pos, NULL, NULL,
                                 (context & OPT_CONTEXT_NO_SINGLE) ? 0 : 1,
                                 context, NULL, &is_mutated);

      if (val) {
        if (SAME_TYPE(SCHEME_TYPE(val), scheme_once_used_type)) {
          Scheme_Once_Used *o = (Scheme_Once_Used *)val;
          if (((o->vclock == info->vclock)
               && ((context & OPT_CONTEXT_SINGLED)
                   || single_valued_noncm_expression(o->expr, 5)))
              || movable_expression(o->expr, info, o->delta, o->cross_lambda,
                                    o->kclock != info->kclock,
                                    o->sclock != info->sclock,
                                    0, 5)) {
            val = optimize_clone(1, o->expr, info, o->delta, 0);
            if (val) {
              int save_fuel = info->inline_fuel, save_no_types = info->no_types;
              int save_vclock, save_kclock, save_sclock;
              info->size -= 1;
              o->used = 1;
              info->inline_fuel = 0; /* no more inlining; o->expr was already optimized */
              info->no_types = 1; /* cannot used inferred types, in case `val' inferred them */
              save_vclock = info->vclock; /* allowed to move => no change to clocks */
              save_kclock = info->kclock;
              save_sclock = info->sclock;

              val = scheme_optimize_expr(val, info, context);

              info->inline_fuel = save_fuel;
              info->no_types = save_no_types;
              info->vclock = save_vclock;
              info->kclock = save_kclock;
              info->sclock = save_sclock;
              return val;
            }
          }
          /* Can't move expression, so lookup again to mark as used
             and to perform any copy propagation that might apply. */
          val = optimize_info_lookup(info, pos, NULL, NULL, 0, context, NULL, NULL);
          if (val)
            return val;
        } else {
          if (SAME_TYPE(SCHEME_TYPE(val), scheme_compiled_toplevel_type)) {
            info->size -= 1;
            return scheme_optimize_expr(val, info, context);
          }
          return val;
        }
      } else if (is_mutated) {
        info->vclock += 1;
      }

      delta = optimize_info_get_shift(info, pos);

      if (context & OPT_CONTEXT_BOOLEAN) {
        Scheme_Object *pred;
        pred = optimize_get_predicate(pos + delta, info);
        if (pred) {
          /* all predicates recognize non-#f things */
          return scheme_true;
        }
      }

      if (delta)
	expr = scheme_make_local(scheme_local_type, pos + delta, 0);

      return expr;
    }
  case scheme_application_type:
    return optimize_application(expr, info, context);
  case scheme_application2_type:
    return optimize_application2(expr, info, context);
  case scheme_application3_type:
    return optimize_application3(expr, info, context);
  case scheme_sequence_type:
  case scheme_splice_sequence_type:
    return optimize_sequence(expr, info, context);
  case scheme_branch_type:
    return optimize_branch(expr, info, context);
  case scheme_with_cont_mark_type:
    return optimize_wcm(expr, info, context);
  case scheme_compiled_unclosed_procedure_type:
    if (context & OPT_CONTEXT_BOOLEAN)
      return scheme_true;
    else
      return optimize_closure_compilation(expr, info, context);
  case scheme_compiled_let_void_type:
    return scheme_optimize_lets(expr, info, 0, context);
  case scheme_compiled_toplevel_type:
    info->size += 1;
    if (info->top_level_consts) {
      int pos;
      Scheme_Object *c;

      while (1) {
        pos = SCHEME_TOPLEVEL_POS(expr);
        c = scheme_hash_get(info->top_level_consts, scheme_make_integer(pos));
        c = no_potential_size(c);
        if (c && SAME_TYPE(SCHEME_TYPE(c), scheme_compiled_toplevel_type))
          expr = c;
        else
          break;
      }

      if (c) {
        if (context & OPT_CONTEXT_BOOLEAN)
          return (SCHEME_FALSEP(c) ? scheme_false : scheme_true);

	if (scheme_compiled_duplicate_ok(c, 0))
	  return c;

	/* We can't inline, but mark the top level as a constant,
	   so we can direct-jump and avoid null checks in JITed code: */
	expr = scheme_toplevel_to_flagged_toplevel(expr, SCHEME_TOPLEVEL_CONST);
      } else {
	/* false is mapped to a table of non-constant ready values: */
	c = scheme_hash_get(info->top_level_consts, scheme_false);
	if (c) {
	  c = scheme_hash_get((Scheme_Hash_Table *)c, scheme_make_integer(pos));

	  if (c) {
	    /* We can't inline, but mark the top level as ready and fixed,
	       so we can avoid null checks in JITed code, etc: */
	    expr = scheme_toplevel_to_flagged_toplevel(expr, SCHEME_TOPLEVEL_FIXED);
	  }
	}
        if (!c)
          info->vclock += 1;
      }
    } else {
      info->vclock += 1;
    }
    optimize_info_used_top(info);
    return expr;
  case scheme_compiled_quote_syntax_type:
    if (context & OPT_CONTEXT_BOOLEAN)
      return scheme_true;
    else {
      info->size += 1;
      optimize_info_used_top(info);
    }
    return expr;
  case scheme_variable_type:
  case scheme_module_variable_type:
    scheme_signal_error("got top-level in wrong place");
    return 0;
  case scheme_define_values_type:
    return define_values_optimize(expr, info, context);
  case scheme_varref_form_type:
    return ref_optimize(expr, info, context);
  case scheme_set_bang_type:
    return set_optimize(expr, info, context);
  case scheme_define_syntaxes_type:
    return define_syntaxes_optimize(expr, info, context);
  case scheme_begin_for_syntax_type:
    return begin_for_syntax_optimize(expr, info, context);
  case scheme_case_lambda_sequence_type:
    if (context & OPT_CONTEXT_BOOLEAN)
      return scheme_true;
    else
      return case_lambda_optimize(expr, info, context);
  case scheme_begin0_sequence_type:
    return begin0_optimize(expr, info, context);
  case scheme_apply_values_type:
    return apply_values_optimize(expr, info, context);
  case scheme_require_form_type:
    return top_level_require_optimize(expr, info, context);
  case scheme_module_type:
    return module_optimize(expr, info, context);
  default:
    info->size += 1;
    if ((context & OPT_CONTEXT_BOOLEAN)
        && (SCHEME_TYPE(expr) > _scheme_compiled_values_types_)
        && SCHEME_TRUEP(expr))
      return scheme_true;
    else
      return expr;
  }
}

Scheme_Object *optimize_clone(int dup_ok, Scheme_Object *expr, Optimize_Info *info, int delta, int closure_depth)
/* Past closure_depth, need to reverse optimize to unoptimized with respect to info;
   delta is the amount to skip in info to get to the frame that bound the code.
   If dup_ok is 1, then the old copy will be dropped, so it's ok to "duplicate"
   any constant. */
{
  int t;

  t = SCHEME_TYPE(expr);

  switch(t) {
  case scheme_local_type:
    {
      int pos = SCHEME_LOCAL_POS(expr);
      if (pos >= closure_depth) {
	expr = optimize_reverse(info, pos + delta - closure_depth, 0, !dup_ok);
	if (closure_depth)
	  expr = scheme_make_local(scheme_local_type, SCHEME_LOCAL_POS(expr) + closure_depth, 0);
      }
      return expr;
    }
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)expr, *app2;

      app2 = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
      app2->iso.so.type = scheme_application2_type;

      expr = optimize_clone(dup_ok, app->rator, info, delta, closure_depth);
      if (!expr) return NULL;
      app2->rator = expr;

      expr = optimize_clone(dup_ok, app->rand, info, delta, closure_depth);
      if (!expr) return NULL;
      app2->rand = expr;

      SCHEME_APPN_FLAGS(app2) |= (SCHEME_APPN_FLAGS(app) & APPN_FLAG_MASK);

      return (Scheme_Object *)app2;
    }
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)expr, *app2;
      int i;

      app2 = scheme_malloc_application(app->num_args + 1);

      for (i = app->num_args + 1; i--; ) {
	expr = optimize_clone(dup_ok, app->args[i], info, delta, closure_depth);
	if (!expr) return NULL;
	app2->args[i] = expr;
      }

      SCHEME_APPN_FLAGS(app2) |= (SCHEME_APPN_FLAGS(app) & APPN_FLAG_MASK);

      return (Scheme_Object *)app2;
    }
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)expr, *app2;

      app2 = MALLOC_ONE_TAGGED(Scheme_App3_Rec);
      app2->iso.so.type = scheme_application3_type;

      expr = optimize_clone(dup_ok, app->rator, info, delta, closure_depth);
      if (!expr) return NULL;
      app2->rator = expr;

      expr = optimize_clone(dup_ok, app->rand1, info, delta, closure_depth);
      if (!expr) return NULL;
      app2->rand1 = expr;

      expr = optimize_clone(dup_ok, app->rand2, info, delta, closure_depth);
      if (!expr) return NULL;
      app2->rand2 = expr;

      SCHEME_APPN_FLAGS(app2) |= (SCHEME_APPN_FLAGS(app) & APPN_FLAG_MASK);

      return (Scheme_Object *)app2;
    }
  case scheme_compiled_let_void_type:
    {
      Scheme_Let_Header *head = (Scheme_Let_Header *)expr, *head2;
      Scheme_Object *body;
      Scheme_Compiled_Let_Value *lv, *lv2, *prev = NULL;
      int i, *flags, sz;
      int post_bind = !(SCHEME_LET_FLAGS(head) & (SCHEME_LET_RECURSIVE | SCHEME_LET_STAR));

      head2 = MALLOC_ONE_TAGGED(Scheme_Let_Header);
      head2->iso.so.type = scheme_compiled_let_void_type;
      head2->count = head->count;
      head2->num_clauses = head->num_clauses;
      SCHEME_LET_FLAGS(head2) = SCHEME_LET_FLAGS(head);

      /* Build let-value change: */
      body = head->body;
      for (i = head->num_clauses; i--; ) {
	lv = (Scheme_Compiled_Let_Value *)body;

	sz = sizeof(int) * lv->count;
	flags = (int *)scheme_malloc_atomic(sz);
	memcpy(flags, lv->flags, sz);

	lv2 = MALLOC_ONE_TAGGED(Scheme_Compiled_Let_Value);
        SCHEME_CLV_FLAGS(lv2) |= (SCHEME_CLV_FLAGS(lv) & 0x1);
	lv2->iso.so.type = scheme_compiled_let_value_type;
	lv2->count = lv->count;
	lv2->position = lv->position;
	lv2->flags = flags;

	expr = optimize_clone(dup_ok, lv->value, info, delta,
                                     closure_depth + (post_bind ? 0 : head->count));
	if (!expr) return NULL;
	lv2->value = expr;

	if (prev)
	  prev->body = (Scheme_Object *)lv2;
	else
	  head2->body = (Scheme_Object *)lv2;
	prev = lv2;

	body = lv->body;
      }
      if (prev)
	prev->body = body;
      else
	head2->body = body;

      expr = optimize_clone(dup_ok, body, info, delta, closure_depth + head->count);
      if (!expr) return NULL;

      if (prev)
	prev->body = expr;
      else
	head2->body = expr;

      return (Scheme_Object *)head2;
    }
  case scheme_sequence_type:
  case scheme_begin0_sequence_type:
  case scheme_splice_sequence_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)expr, *seq2;
      int i;

      seq2 = scheme_malloc_sequence(seq->count);
      seq2->so.type = seq->so.type;
      seq2->count = seq->count;

      for (i = seq->count; i--; ) {
	expr = optimize_clone(dup_ok, seq->array[i], info, delta, closure_depth);
	if (!expr) return NULL;
	seq2->array[i] = expr;
      }

      return (Scheme_Object *)seq2;
    }
  case scheme_branch_type:
    {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)expr, *b2;

      b2 = MALLOC_ONE_TAGGED(Scheme_Branch_Rec);
      b2->so.type = scheme_branch_type;

      expr = optimize_clone(dup_ok, b->test, info, delta, closure_depth);
      if (!expr) return NULL;
      b2->test = expr;

      expr = optimize_clone(dup_ok, b->tbranch, info, delta, closure_depth);
      if (!expr) return NULL;
      b2->tbranch = expr;

      expr = optimize_clone(dup_ok, b->fbranch, info, delta, closure_depth);
      if (!expr) return NULL;
      b2->fbranch = expr;

      return (Scheme_Object *)b2;
    }
  case scheme_with_cont_mark_type:
    {
      Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)expr, *wcm2;

      wcm2 = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
      wcm2->so.type = scheme_with_cont_mark_type;

      expr = optimize_clone(dup_ok, wcm->key, info, delta, closure_depth);
      if (!expr) return NULL;
      wcm2->key = expr;

      expr = optimize_clone(dup_ok, wcm->val, info, delta, closure_depth);
      if (!expr) return NULL;
      wcm2->val = expr;

      expr = optimize_clone(dup_ok, wcm->body, info, delta, closure_depth);
      if (!expr) return NULL;
      wcm2->body = expr;

      return (Scheme_Object *)wcm2;
    }
  case scheme_compiled_unclosed_procedure_type:
    return clone_closure_compilation(dup_ok, expr, info, delta, closure_depth);
  case scheme_compiled_toplevel_type:
  case scheme_compiled_quote_syntax_type:
    return expr;
  case scheme_define_values_type:
  case scheme_define_syntaxes_type:
  case scheme_begin_for_syntax_type:
  case scheme_boxenv_type:
    return NULL;
  case scheme_require_form_type:
    return NULL;
  case scheme_varref_form_type:
    return ref_clone(dup_ok, expr, info, delta, closure_depth);
  case scheme_set_bang_type:
    return set_clone(dup_ok, expr, info, delta, closure_depth);
  case scheme_apply_values_type:
    return apply_values_clone(dup_ok, expr, info, delta, closure_depth);
  case scheme_case_lambda_sequence_type:
    return case_lambda_clone(dup_ok, expr, info, delta, closure_depth);
  case scheme_module_type:
    return NULL;
  default:
    if (t > _scheme_compiled_values_types_) {
      if (dup_ok || scheme_compiled_duplicate_ok(expr, 0))
	return expr;
    }
  }

  return NULL;
}

Scheme_Object *optimize_shift(Scheme_Object *expr, int delta, int after_depth)
/* Shift lexical addresses deeper by delta if already deeper than after_depth;
   can mutate. */
{
  int t;

  /* FIXME: need stack check */

  t = SCHEME_TYPE(expr);

  switch(t) {
  case scheme_local_type:
  case scheme_local_unbox_type:
    {
      int pos = SCHEME_LOCAL_POS(expr);
      if (pos >= after_depth) {
        expr = scheme_make_local(t, SCHEME_LOCAL_POS(expr) + delta, 0);
      }
      return expr;
    }
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)expr;
      int i;

      for (i = app->num_args + 1; i--; ) {
	expr = optimize_shift(app->args[i], delta, after_depth);
	app->args[i] = expr;
      }

      return (Scheme_Object *)app;
    }
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)expr;

      expr = optimize_shift(app->rator, delta, after_depth);
      app->rator = expr;

      expr = optimize_shift(app->rand, delta, after_depth);
      app->rand = expr;

      return (Scheme_Object *)app;
    }
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)expr;

      expr = optimize_shift(app->rator, delta, after_depth);
      app->rator = expr;

      expr = optimize_shift(app->rand1, delta, after_depth);
      app->rand1 = expr;

      expr = optimize_shift(app->rand2, delta, after_depth);
      app->rand2 = expr;

      return (Scheme_Object *)app;
    }
  case scheme_compiled_let_void_type:
    {
      Scheme_Let_Header *head = (Scheme_Let_Header *)expr;
      Scheme_Object *body;
      Scheme_Compiled_Let_Value *lv = NULL;
      int i;
      int post_bind = !(SCHEME_LET_FLAGS(head) & (SCHEME_LET_RECURSIVE | SCHEME_LET_STAR));

      /* Build let-value change: */
      body = head->body;
      for (i = head->num_clauses; i--; ) {
	lv = (Scheme_Compiled_Let_Value *)body;

	expr = optimize_shift(lv->value, delta, after_depth + (post_bind ? 0 : head->count));
	lv->value = expr;

        body = lv->body;
      }
      expr = optimize_shift(body, delta, after_depth + head->count);

      if (head->num_clauses)
	lv->body = expr;
      else
	head->body = expr;

      return (Scheme_Object *)head;
    }
  case scheme_sequence_type:
  case scheme_splice_sequence_type:
  case scheme_begin0_sequence_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)expr;
      int i;

      for (i = seq->count; i--; ) {
	expr = optimize_shift(seq->array[i], delta, after_depth);
	seq->array[i] = expr;
      }

      return (Scheme_Object *)seq;
    }
  case scheme_branch_type:
    {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)expr;

      expr = optimize_shift(b->test, delta, after_depth);
      b->test = expr;

      expr = optimize_shift(b->tbranch, delta, after_depth);
      b->tbranch = expr;

      expr = optimize_shift(b->fbranch, delta, after_depth);
      b->fbranch = expr;

      return (Scheme_Object *)b;
    }
  case scheme_with_cont_mark_type:
    {
      Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)expr;

      expr = optimize_shift(wcm->key, delta, after_depth);
      wcm->key = expr;

      expr = optimize_shift(wcm->val, delta, after_depth);
      wcm->val = expr;

      expr = optimize_shift(wcm->body, delta, after_depth);
      wcm->body = expr;

      return (Scheme_Object *)wcm;
    }
  case scheme_compiled_unclosed_procedure_type:
    return shift_closure_compilation(expr, delta, after_depth);
  case scheme_compiled_toplevel_type:
  case scheme_compiled_quote_syntax_type:
    return expr;
  case scheme_set_bang_type:
    return set_shift(expr, delta, after_depth);
  case scheme_varref_form_type:
    return ref_shift(expr, delta, after_depth);
  case scheme_apply_values_type:
    return apply_values_shift(expr, delta, after_depth);
  case scheme_case_lambda_sequence_type:
    return case_lambda_shift(expr, delta, after_depth);
  case scheme_boxenv_type:
  case scheme_define_values_type:
  case scheme_define_syntaxes_type:
  case scheme_begin_for_syntax_type:
  case scheme_require_form_type:
  case scheme_module_type:
    scheme_signal_error("optimize_shift: no shift available for %d", SCHEME_TYPE(expr));
    return NULL;
  default:
    return expr;
  }

  return NULL;
}

/*========================================================================*/
/*                 compile-time env for optimization                      */
/*========================================================================*/

Optimize_Info *scheme_optimize_info_create(Comp_Prefix *cp, int get_logger)
{
  Optimize_Info *info;

  info = MALLOC_ONE_RT(Optimize_Info);
#ifdef MZTAG_REQUIRED
  info->type = scheme_rt_optimize_info;
#endif
  info->inline_fuel = 32;
  info->shift_fuel = 16;
  info->cp = cp;

  if (get_logger) {
    Scheme_Logger *logger;
    logger = (Scheme_Logger *)scheme_get_param(scheme_current_config(), MZCONFIG_LOGGER);
    logger = scheme_make_logger(logger, scheme_intern_symbol("optimizer"));
    info->logger = logger;
  }

  return info;
}

static void optimize_info_seq_init(Optimize_Info *info, Optimize_Info_Sequence *info_seq)
{
  info_seq->init_shift_fuel = info->shift_fuel;
  info_seq->min_shift_fuel = info->shift_fuel;
}

static void optimize_info_seq_step(Optimize_Info *info, Optimize_Info_Sequence *info_seq)
{
  if (info->shift_fuel < info_seq->min_shift_fuel)
    info_seq->min_shift_fuel = info->shift_fuel;
  info->shift_fuel = info_seq->init_shift_fuel;
}

static void optimize_info_seq_done(Optimize_Info *info, Optimize_Info_Sequence *info_seq)
{
  if (info->shift_fuel > info_seq->min_shift_fuel)
    info->shift_fuel = info_seq->min_shift_fuel;
}

void scheme_optimize_info_enforce_const(Optimize_Info *oi, int enforce_const)
{
  oi->enforce_const = enforce_const;
}

void scheme_optimize_info_set_context(Optimize_Info *oi, Scheme_Object *ctx)
{
  oi->context = ctx;
}

void scheme_optimize_info_never_inline(Optimize_Info *oi)
{
  oi->inline_fuel = -1;
}

static void register_transitive_use(Optimize_Info *info, int pos, int j);

static void register_stat_dist(Optimize_Info *info, int i, int j)
{
  if (!info->stat_dists) {
    int k, *ia;
    char **ca;
    ca = MALLOC_N(char*, info->new_frame);
    info->stat_dists = ca;
    ia = MALLOC_N_ATOMIC(int, info->new_frame);
    info->sd_depths = ia;
    for (k = info->new_frame; k--; ) {
      info->sd_depths[k] = 0;
    }
  }

  if (i >= info->new_frame)
    scheme_signal_error("internal error: bad stat-dist index");

  if (info->sd_depths[i] <= j) {
    char *naya, *a;
    int k;

    naya = MALLOC_N_ATOMIC(char, (j + 1));
    for (k = j + 1; k--; ) {
      naya[k] = 0;
    }
    a = info->stat_dists[i];
    for (k = info->sd_depths[i]; k--; ) {
      naya[k] = a[k];
    }

    info->stat_dists[i] = naya;
    info->sd_depths[i] = j + 1;
  }

  if (info->transitive_use && info->transitive_use[i]) {
    /* We're using a procedure that we weren't sure would be used.
       Transitively mark everything that the procedure uses --- unless
       a transitive accumulation is in effect, in which case we
       don't follow this one now, leaving it to be triggered when
       the one we're accumulating is triggered. */
    if (!info->transitive_use_pos) {
      mzshort *map = info->transitive_use[i];
      int len = info->transitive_use_len[i];
      int k;

      info->transitive_use[i] = NULL;

      for (k = 0; k < len; k++) {
        register_transitive_use(info, map[k], 0);
      }
    }
  }

  info->stat_dists[i][j] = 1;
}

static Scheme_Object *transitive_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Optimize_Info *info = (Optimize_Info *)p->ku.k.p1;

  p->ku.k.p1 = NULL;

  register_transitive_use(info, (int)p->ku.k.i1, (int)p->ku.k.i2);

  return scheme_false;
}

static void register_transitive_use(Optimize_Info *info, int pos, int j)
{
#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Thread *p = scheme_current_thread;

    p->ku.k.p1 = (void *)info;
    p->ku.k.i1 = pos;
    p->ku.k.i2 = j;

    scheme_handle_stack_overflow(transitive_k);

    return;
  }
#endif

  while (info) {
    if (info->flags & SCHEME_LAMBDA_FRAME)
      j++;
    if (pos < info->new_frame)
      break;
    pos -= info->new_frame;
    info = info->next;
  }

  if (info->sd_depths[pos] <= j) {
    scheme_signal_error("bad transitive position depth: %d vs. %d",
                        info->sd_depths[pos], j);
  }

  register_stat_dist(info, pos, j);
}

static void env_make_closure_map(Optimize_Info *info, mzshort *_size, mzshort **_map)
{
  /* A closure map lists the captured variables for a closure; the
     indices are resolved two new indices in the second phase of
     compilation. */
  Optimize_Info *frame;
  int i, j, pos = 0, lpos = 0, tu;
  mzshort *map, size;

  /* Count vars used by this closure (skip args): */
  j = 1;
  for (frame = info->next; frame; frame = frame->next) {
    if (frame->flags & SCHEME_LAMBDA_FRAME)
      j++;

    if (frame->stat_dists) {
      for (i = 0; i < frame->new_frame; i++) {
	if (frame->sd_depths[i] > j) {
	  if (frame->stat_dists[i][j]) {
	    pos++;
	  }
	}
      }
    }
  }

  size = pos;
  *_size = size;
  map = MALLOC_N_ATOMIC(mzshort, size);
  *_map = map;

  if (info->next && info->next->transitive_use_pos) {
    info->next->transitive_use[info->next->transitive_use_pos - 1] = map;
    info->next->transitive_use_len[info->next->transitive_use_pos - 1] = size;
    tu = 1;
  } else
    tu = 0;

  /* Build map, unmarking locals and marking deeper in parent frame */
  j = 1; pos = 0;
  for (frame = info->next; frame; frame = frame->next) {
    if (frame->flags & SCHEME_LAMBDA_FRAME)
      j++;

    if (frame->stat_dists) {
      for (i = 0; i < frame->new_frame; i++) {
	if (frame->sd_depths[i] > j) {
	  if (frame->stat_dists[i][j]) {
	    map[pos++] = lpos;
	    frame->stat_dists[i][j] = 0; /* This closure's done with these vars... */
            if (!tu)
              frame->stat_dists[i][j - 1] = 1; /* ... but ensure previous keeps */
	  }
	}
	lpos++;
      }
    } else
      lpos += frame->new_frame;
  }
}

static int env_uses_toplevel(Optimize_Info *frame)
{
  int used;

  used = frame->used_toplevel;

  if (used) {
    /* Propagate use to an enclosing lambda, if any: */
    frame = frame->next;
    while (frame) {
      if (frame->flags & SCHEME_LAMBDA_FRAME) {
	frame->used_toplevel = 1;
	break;
      }
      frame = frame->next;
    }
  }

  return used;
}

static void optimize_info_used_top(Optimize_Info *info)
{
  while (info) {
    if (info->flags & SCHEME_LAMBDA_FRAME) {
      info->used_toplevel = 1;
      break;
    }
    info = info->next;
  }
}

static void optimize_propagate(Optimize_Info *info, int pos, Scheme_Object *value, int single_use)
{
  /* A raw-pair `value' is an indicator for whether a letrec-bound
     variable is ready. */
  Scheme_Object *p;

  p = scheme_make_vector(4, NULL);
  SCHEME_VEC_ELS(p)[0] = info->consts;
  SCHEME_VEC_ELS(p)[1] = scheme_make_integer(pos);
  SCHEME_VEC_ELS(p)[2] = value;
  SCHEME_VEC_ELS(p)[3] = (single_use ? scheme_true : scheme_false);

  info->consts = p;
}

static Scheme_Once_Used *make_once_used(Scheme_Object *val, int pos,
                                        int vclock, int kclock, int sclock,
                                        Scheme_Once_Used *prev)
{
  Scheme_Once_Used *o;

  o = MALLOC_ONE_TAGGED(Scheme_Once_Used);
  o->so.type = scheme_once_used_type;

  o->expr = val;
  o->pos = pos;
  o->vclock = vclock;
  o->kclock = kclock;
  o->sclock = sclock;

  if (prev)
    prev->next = o;

  return o;
}

static void register_use(Optimize_Info *info, int pos, int flag)
/* pos must be in immediate frame */
{
  if (!info->use) {
    char *use;
    use = (char *)scheme_malloc_atomic(info->new_frame);
    memset(use, 0, info->new_frame);
    info->use = use;
  }
  info->use[pos] |= flag;
}

static void optimize_mutated(Optimize_Info *info, int pos)
/* pos must be in immediate frame */
{
  register_use(info, pos, OPT_IS_MUTATED);
}

static void optimize_produces_local_type(Optimize_Info *info, int pos, int ct)
/* pos must be in immediate frame */
{
  register_use(info, pos, ct << OPT_LOCAL_TYPE_VAL_SHIFT);
}

static Scheme_Object *optimize_reverse(Optimize_Info *info, int pos, int unless_mutated, int disrupt_single_use)
/* pos is in new-frame counts, and we want to produce an old-frame reference if
   it's not mutated */
{
  int delta = 0;

  while (1) {
    if (pos < info->new_frame)
      break;
    pos -= info->new_frame;
    delta += info->original_frame;
    info = info->next;
  }

  if (unless_mutated)
    if (info->use && (info->use[pos] & OPT_IS_MUTATED))
      return NULL;

  if (disrupt_single_use) {
    Scheme_Object *p, *n;
    p = info->consts;
    while (p) {
      n = SCHEME_VEC_ELS(p)[1];
      if (SCHEME_INT_VAL(n) == pos) {
        if (SCHEME_TRUEP(SCHEME_VEC_ELS(p)[3])) {
          SCHEME_VEC_ELS(p)[3] = scheme_false; /* disable "single use" mark */
        }
        n = SCHEME_VEC_ELS(p)[2];
        if (SAME_TYPE(SCHEME_TYPE(n), scheme_once_used_type)) {
          ((Scheme_Once_Used *)n)->expr = NULL;
          ((Scheme_Once_Used *)n)->vclock = -1;
        }
        break;
      }
      p = SCHEME_VEC_ELS(p)[0];
    }
  }

  return scheme_make_local(scheme_local_type, pos + delta, 0);
}

static int optimize_is_used(Optimize_Info *info, int pos)
/* pos must be in immediate frame */
{
  int i;

  if (info->stat_dists) {
    for (i = info->sd_depths[pos]; i--; ) {
      if (info->stat_dists[pos][i])
	return 1;
    }
  }

  return 0;
}

static int check_use(Optimize_Info *info, int pos, int mask, int shift)
/* pos is in new-frame counts */
{
  while (info) {
    if (pos < info->new_frame)
      break;
    pos -= info->new_frame;
    info = info->next;
  }

  if (info->use)
    return (info->use[pos] >> shift) & mask;

  return 0;
}

static int optimize_is_mutated(Optimize_Info *info, int pos)
/* pos is in new-frame counts */
{
  return check_use(info, pos, OPT_IS_MUTATED, 0);
}

static int optimize_escapes_after_k_tick(Optimize_Info *info, int pos)
/* pos is in new-frame counts */
{
  return check_use(info, pos, OPT_ESCAPES_AFTER_K_TICK, 0);
}

static int optimize_is_local_type_arg(Optimize_Info *info, int pos, int depth)
/* pos is in new-frame counts */
{
  return check_use(info, pos, SCHEME_MAX_LOCAL_TYPE_MASK, OPT_LOCAL_TYPE_ARG_SHIFT);
}

static int optimize_is_local_type_valued(Optimize_Info *info, int pos)
/* pos is in new-frame counts */
{
  return check_use(info, pos, SCHEME_MAX_LOCAL_TYPE_MASK, OPT_LOCAL_TYPE_VAL_SHIFT);
}

static int optimize_any_uses(Optimize_Info *info, int start_pos, int end_pos)
{
  int j, i;

  if (info->stat_dists) {
    for (i = start_pos; i < end_pos; i++) {
      for (j = info->sd_depths[i]; j--; ) {
        if (info->stat_dists[i][j])
          return 1;
      }
    }
  }

  if (info->transitive_use) {
    for (i = info->new_frame; i--; ) {
      if (info->transitive_use[i]) {
        for (j = info->transitive_use_len[i]; j--; ) {
          if ((info->transitive_use[i][j] >= start_pos)
              && (info->transitive_use[i][j] < end_pos))
            return 1;
        }
      }
    }
  }

  return 0;
}

static Scheme_Object *do_optimize_info_lookup(Optimize_Info *info, int pos, int j, int *closure_offset, int *single_use,
                                              int *not_ready, int once_used_ok, int context, int *potential_size,
                                              int disrupt_single_use, int *is_mutated, int just_test)
{
  Scheme_Object *p, *n;
  int delta = 0, orig_j = j, kclock = info->kclock;

  while (info) {
    if (info->flags & SCHEME_LAMBDA_FRAME)
      j++;
    if (pos < info->original_frame)
      break;
    pos -= info->original_frame;
    delta += info->new_frame;
    info = info->next;
  }

  if (OPT_CONTEXT_TYPE(context))
    register_use(info, pos, OPT_CONTEXT_TYPE(context) << OPT_LOCAL_TYPE_ARG_SHIFT);
  else if (!just_test && (kclock > info->init_kclock))
    register_use(info, pos, OPT_ESCAPES_AFTER_K_TICK);

  if (is_mutated)
    if (info->use && (info->use[pos] & OPT_IS_MUTATED))
      *is_mutated = 1;

  if (just_test) return NULL;

  p = info->consts;
  while (p) {
    n = SCHEME_VEC_ELS(p)[1];
    if (SCHEME_INT_VAL(n) == pos) {
      n = SCHEME_VEC_ELS(p)[2];
      if (info->flags & SCHEME_POST_BIND_FRAME)
        delta += info->new_frame;
      if (SCHEME_RPAIRP(n)) {
        /* This was a letrec-bound identifier that may or may not be ready,
           but which wasn't replaced with more information. */
        if (not_ready)
          *not_ready = SCHEME_TRUEP(SCHEME_CAR(n));
        break;
      }
      if (SCHEME_BOXP(n)) {
        /* A potential-size record: */
        if (potential_size)
          *potential_size = (int)SCHEME_INT_VAL(SCHEME_BOX_VAL(n));
        break;
      }
      if (single_use)
        *single_use = SCHEME_TRUEP(SCHEME_VEC_ELS(p)[3]);
      if (SAME_TYPE(SCHEME_TYPE(n), scheme_compiled_unclosed_procedure_type)) {
        if (context & OPT_CONTEXT_BOOLEAN) return scheme_true;
	if (!closure_offset)
	  break;
	else
          *closure_offset = delta;
      } else if (SAME_TYPE(SCHEME_TYPE(n), scheme_case_lambda_sequence_type)) {
        if (context & OPT_CONTEXT_BOOLEAN) return scheme_true;
        if (!closure_offset)
	  break;
	else
          *closure_offset = delta;
      } else if (SAME_TYPE(SCHEME_TYPE(n), scheme_compiled_toplevel_type)) {
        /* Ok */
      } else if (closure_offset) {
        /* Inlining can deal procedures and top-levels, but not other things. */
        return NULL;
      } else if (SAME_TYPE(SCHEME_TYPE(n), scheme_once_used_type)) {
        Scheme_Once_Used *o;

        if (disrupt_single_use) {
          ((Scheme_Once_Used *)n)->expr = NULL;
          ((Scheme_Once_Used *)n)->vclock = -1;
        }

        if (!once_used_ok)
          break;

        o = (Scheme_Once_Used *)n;
        if (!o->expr) break; /* disrupted or not available */

        o->delta = delta;
        o->info = info;
        o->cross_lambda = (j != orig_j);
        return (Scheme_Object *)o;
      } else if (SAME_TYPE(SCHEME_TYPE(n), scheme_local_type)) {
	int pos, cross_lambda = (j != orig_j);

	pos = SCHEME_LOCAL_POS(n);
	if (info->flags & SCHEME_LAMBDA_FRAME)
	  j--; /* because it will get re-added on recur */
        else if (info->flags & SCHEME_POST_BIND_FRAME)
          info = info->next; /* bindings are relative to next frame */

	/* Marks local as used; we don't expect to get back
	   a value, because chaining would normally happen on the
	   propagate-call side. Chaining there also means that we
	   avoid stack overflow here. */
        if (single_use) {
          if (!*single_use)
            single_use = NULL;
        }

        /* If the referenced variable is not single-use, then
           the variable it is replaced by is no longer single-use */
        disrupt_single_use = !SCHEME_TRUEP(SCHEME_VEC_ELS(p)[3]);

	n = do_optimize_info_lookup(info, pos, j, NULL, single_use, NULL,
                                    once_used_ok && !disrupt_single_use, context,
                                    potential_size, disrupt_single_use, NULL, 0);

	if (!n) {
	  /* Return shifted reference to other local: */
	  delta += optimize_info_get_shift(info, pos);
	  n = scheme_make_local(scheme_local_type, pos + delta, 0);
	} else if (SAME_TYPE(SCHEME_TYPE(n), scheme_once_used_type)) {
          /* Need to adjust delta: */
          delta += optimize_info_get_shift(info, pos);
          ((Scheme_Once_Used *)n)->delta += delta;
          if (cross_lambda) ((Scheme_Once_Used *)n)->cross_lambda = 1;
        }
      }
      return n;
    }
    p = SCHEME_VEC_ELS(p)[0];
  }

  if (!closure_offset)
    register_stat_dist(info, pos, j);

  return NULL;
}

static Scheme_Object *optimize_info_lookup(Optimize_Info *info, int pos, int *closure_offset, int *single_use,
                                           int once_used_ok, int context, int *potential_size, int *is_mutated)
{
  return do_optimize_info_lookup(info, pos, 0, closure_offset, single_use, NULL, once_used_ok, context,
                                 potential_size, 0, is_mutated, 0);
}

static int optimize_info_is_ready(Optimize_Info *info, int pos)
{
  int closure_offset, single_use, ready = 1;

  do_optimize_info_lookup(info, pos, 0, &closure_offset, &single_use, &ready, 0, 0, NULL, 0, NULL, 0);

  return ready;
}

static Scheme_Object *optimize_info_mutated_lookup(Optimize_Info *info, int pos, int *is_mutated)
{
  return do_optimize_info_lookup(info, pos, 0, NULL, NULL, NULL, 0, 0, NULL, 0, is_mutated, 1);
}

Scheme_Object *optimize_get_predicate(int pos, Optimize_Info *info)
/* pos is in new-frame counts */
{
  Scheme_Object *pred;

  if (info->no_types) return NULL;

  while (info) {
    if (info->types) {
      pred = scheme_hash_tree_get(info->types, scheme_make_integer(pos));
      if (pred)
        return pred;
    }
    pos -= info->new_frame;
    if (pos < 0)
      return NULL;
      info = info->next;
  }

  return NULL;
}

static Optimize_Info *optimize_info_add_frame(Optimize_Info *info, int orig, int current, int flags)
{
  Optimize_Info *naya;

  naya = scheme_optimize_info_create(info->cp, 0);
  naya->flags = (short)flags;
  naya->next = info;
  naya->original_frame = orig;
  naya->new_frame = current;
  naya->inline_fuel = info->inline_fuel;
  naya->shift_fuel = info->shift_fuel;
  naya->letrec_not_twice = info->letrec_not_twice;
  naya->enforce_const = info->enforce_const;
  naya->top_level_consts = info->top_level_consts;
  naya->context = info->context;
  naya->vclock = info->vclock;
  naya->kclock = info->kclock;
  naya->sclock = info->sclock;
  naya->escapes = info->escapes;
  naya->init_kclock = info->kclock;
  naya->use_psize = info->use_psize;
  naya->logger = info->logger;
  naya->no_types = info->no_types;

  return naya;
}

static int optimize_info_get_shift(Optimize_Info *info, int pos)
{
  int delta = 0;

  while (info) {
    if (pos < info->original_frame)
      break;
    pos -= info->original_frame;
    delta += (info->new_frame - info->original_frame);
    info = info->next;
  }

  if (!info)
    scheme_signal_error("error looking for local-variable offset");

  return delta;
}

static void optimize_info_done(Optimize_Info *info, Optimize_Info *parent)
{
  if (!parent) parent = info->next;

  parent->size += info->size;
  parent->vclock = info->vclock;
  parent->kclock = info->kclock;
  parent->sclock = info->sclock;
  parent->escapes = info->escapes;
  parent->psize += info->psize;
  parent->shift_fuel = info->shift_fuel;
  if (info->has_nonleaf)
    parent->has_nonleaf = 1;
}

/*========================================================================*/
/*                         precise GC traversers                          */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_optimize.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_once_used_type, mark_once_used);
  GC_REG_TRAV(scheme_rt_optimize_info, mark_optimize_info);
}

END_XFORM_SKIP;

#endif
