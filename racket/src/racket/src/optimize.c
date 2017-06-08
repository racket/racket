/*
  Racket
  Copyright (c) 2004-2017 PLT Design Inc.
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

/* This file implements bytecode optimization.

   See "eval.c" for an overview of compilation passes. */

#include "schpriv.h"
#include "schrunst.h"
#include "schmach.h"

/* Controls for inlining algorithm: */
#define OPT_ESTIMATE_FUTURE_SIZES   1
#define OPT_DISCOURAGE_EARLY_INLINE 1
#define OPT_LIMIT_FUNCTION_RESIZE   0
#define OPT_BRANCH_ADDS_NO_SIZE     1
#define OPT_DELAY_GROUP_PROPAGATE   0
#define OPT_PRE_OPTIMIZE_FOR_CROSS_MODULE(size_override) (size_override)

#define MAX_PROC_INLINE_SIZE     256
#define CROSS_MODULE_INLINE_SIZE 8

/* Various kinds of fuel ensure that
   the compiler doesn't go into a loop
   or take non-linear time */
#define INITIAL_INLINING_FUEL   32
#define INITIAL_FLATTENING_FUEL 16

/* Clasification for predicates. 
   Each one implies the smaller.  */
#define RLV_IS_RELEVANT   1 /* The predicate is remembered by the optimizer */
#define RLV_EQV_TESTEABLE 2 /* (equal? x <pred>) can be replaced by (eqv? x <pred>) */
#define RLV_EQ_TESTEABLE  3 /* (equal? x <pred>) can be replaced by (eq? x <pred>) */
#define RLV_SINGLETON     4 /* Recognizes a single value */

struct Optimize_Info
{
  MZTAG_IF_REQUIRED
  short flags;
  struct Optimize_Info *next;
  int original_frame, new_frame;
  Scheme_Object *consts;
  Comp_Prefix *cp;
  int init_kclock;

  /* Compilation context, used for unresolving for cross-module inlining: */
  Scheme_Env *env;
  Scheme_Object *insp;

  /* Propagated up and down the chain: */
  int size;
  int vclock; /* virtual clock that ticks for a side effect, a branch,
                 observation of a side effect (such as an unbox),
                 or a dependency on an earlier side effect (such as a
                 previous guard on an unsafe operation's argument);
                 the clock is only compared between binding sites and
                 uses, so we can rewind the clock at a join after an
                 increment that models a branch (if the branch is not
                 taken or doesn't increment the clock) */
  int aclock; /* virtual clock that ticks for allocation without side effects,
                 for constraining the reordering of operations that might
                 capture a continuation */
  int kclock; /* virtual clock that ticks for a potential continuation capture,
                 for constraining the movement of allocation operations */
  int sclock; /* virtual clock that ticks when space consumption is potentially observed */
  int psize;
  short inline_fuel, flatten_fuel;
  char letrec_not_twice, enforce_const, use_psize, has_nonleaf;
  Scheme_Hash_Table *top_level_consts;

  int maybe_values_argument; /* triggers an approximation for clock increments */

  /* Set by expression optimization: */
  int single_result, preserves_marks; /* negative means "tentative", due to fixpoint in progress */
  int escapes; /* flag to signal that the expression always escapes. When escapes is 1, it's assumed
                  that single_result and preserves_marks are also 1, and that it's not necessary to
                  use optimize_ignored before including the expression. */

  int lambda_depth; /* counts nesting depth under `lambda`s */
  int used_toplevel; /* tracks whether any non-local variables or syntax-object literals are used */
    
  Scheme_Hash_Table *uses; /* used variables, accumulated for closures */

  Scheme_IR_Local *transitive_use_var; /* set when optimizing a letrec-bound procedure
                                          to record variables that were added to `uses` */

  Scheme_Object *context; /* for logging */
  Scheme_Logger *logger;
  Scheme_Hash_Tree *types; /* maps position (from this frame) to predicate */
  int no_types; /* disables use of type info */
};

typedef struct Optimize_Info_Sequence {
  int init_flatten_fuel, min_flatten_fuel;
} Optimize_Info_Sequence;

static int get_rator_flags(Scheme_Object *rator, int num_args, Optimize_Info *info);
Scheme_Object *lookup_constant_proc(Optimize_Info *info, Scheme_Object *le, int argc);
static void merge_lambda_arg_types(Scheme_Lambda *lam1, Scheme_Lambda *lam2);
static void check_lambda_arg_types_registered(Scheme_Lambda *lam, int app_count);
static int lambda_body_size_plus_info(Scheme_Lambda *lam, int check_assign,
                                      Optimize_Info *info, int *is_leaf);
static int lambda_has_top_level(Scheme_Lambda *lam);

static Scheme_Object *make_sequence_2(Scheme_Object *a, Scheme_Object *b);

static int wants_local_type_arguments(Scheme_Object *rator, int argpos);

static void add_types_for_f_branch(Scheme_Object *t, Optimize_Info *info, int fuel);

static void register_use(Scheme_IR_Local *var, Optimize_Info *info);
static Scheme_Object *optimize_info_lookup(Scheme_Object *var);
static Scheme_Object *optimize_info_propagate_local(Scheme_Object *var);
static void optimize_info_used_top(Optimize_Info *info);
static Scheme_Object *optimize_get_predicate(Optimize_Info *info, Scheme_Object *var, int ignore_no_types);
static void add_type(Optimize_Info *info, Scheme_Object *var, Scheme_Object *pred);
static void merge_types(Optimize_Info *src_info, Optimize_Info *info, Scheme_Hash_Tree *skip_vars);

static Scheme_Object *expr_implies_predicate(Scheme_Object *expr, Optimize_Info *info);
static Scheme_Object *do_expr_implies_predicate(Scheme_Object *expr, Optimize_Info *info,
                                                int *_involves_k_cross, int fuel,
                                                Scheme_Hash_Tree *ignore_vars);
static int produces_local_type(Scheme_Object *rator, int argc);
static int optimize_any_uses(Optimize_Info *info, Scheme_IR_Let_Value *at_irlv, int n);
static void optimize_uses_of_mutable_imply_early_alloc(Scheme_IR_Let_Value *at_irlv, int n);
static void propagate_used_variables(Optimize_Info *info);
static int env_uses_toplevel(Optimize_Info *frame);
static Scheme_IR_Local *clone_variable(Scheme_IR_Local *var);
static void increment_use_count(Scheme_IR_Local *var, int as_rator);

static Optimize_Info *optimize_info_add_frame(Optimize_Info *info, int orig, int current, int flags);
static void optimize_info_done(Optimize_Info *info, Optimize_Info *parent);

static void register_transitive_uses(Scheme_IR_Local *var, Optimize_Info *info);

static void optimize_info_seq_init(Optimize_Info *info, Optimize_Info_Sequence *info_seq);
static void optimize_info_seq_step(Optimize_Info *info, Optimize_Info_Sequence *info_seq);
static void optimize_info_seq_done(Optimize_Info *info, Optimize_Info_Sequence *info_seq);

static int ir_propagate_ok(Scheme_Object *o, Optimize_Info *info, int used_once, Scheme_IR_Local *once_var);

static Scheme_Object *estimate_closure_size(Scheme_Object *e);
static Scheme_Object *no_potential_size(Scheme_Object *value);

static Scheme_Object *optimize_lets(Scheme_Object *form, Optimize_Info *info, int context);

static Scheme_Object *optimize_clone(int single_use, Scheme_Object *obj, Optimize_Info *info, Scheme_Hash_Tree *var_map, int as_rator);

XFORM_NONGCING static int relevant_predicate(Scheme_Object *pred);
XFORM_NONGCING static int predicate_implies(Scheme_Object *pred1, Scheme_Object *pred2);
XFORM_NONGCING static int predicate_implies_not(Scheme_Object *pred1, Scheme_Object *pred2);
static int single_valued_expression(Scheme_Object *expr, Optimize_Info *info, int fuel);
static int single_valued_noncm_expression(Scheme_Object *expr, Optimize_Info *info, int fuel);
static int noncm_expression(Scheme_Object *expr, Optimize_Info *info, int fuel);
static Scheme_Object *optimize_ignored(Scheme_Object *e, Optimize_Info *info,
                                       int expected_vals, int maybe_omittable,
                                       int fuel);
static Scheme_Object *equivalent_exprs(Scheme_Object *a, Scheme_Object *b,
                                       Optimize_Info *a_info, Optimize_Info *b_info, int context);
static int movable_expression(Scheme_Object *expr, Optimize_Info *info,
                              int cross_lambda, int cross_k, int cross_s,
                              int check_space, int fuel);
Scheme_Object *optimize_apply_values(Scheme_Object *f, Scheme_Object *e,
                                     Optimize_Info *info,
                                     int e_single_result,
                                     int context);

#define SCHEME_LAMBDAP(vals_expr) (SAME_TYPE(SCHEME_TYPE(vals_expr), scheme_ir_lambda_type) \
                                   || SAME_TYPE(SCHEME_TYPE(vals_expr), scheme_case_lambda_sequence_type))

#define SCHEME_WILL_BE_LAMBDAP(v)     SAME_TYPE(SCHEME_TYPE(v), scheme_will_be_lambda_type)
#define SCHEME_WILL_BE_LAMBDA_SIZE(v) SCHEME_PINT_VAL(v)
#define SCHEME_WILL_BE_LAMBDA(v)      SCHEME_IPTR_VAL(v)

static int lambda_body_size(Scheme_Object *o, int less_args);

typedef struct Scheme_Once_Used {
  Scheme_Object so;
  Scheme_Object *expr;
  Scheme_IR_Local *var;
  int vclock; /* record clocks at binding site */
  int aclock;
  int kclock;
  int sclock;
  int spans_k; /* potentially captures a continuation */
  int moved;
} Scheme_Once_Used;

static Scheme_Once_Used *make_once_used(Scheme_Object *val, Scheme_IR_Local *var,
                                        int vclock, int aclock, int kclock, int sclock, int spans_k);

static ROSYM Scheme_Hash_Tree *empty_eq_hash_tree;
 
#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

void scheme_init_optimize()
{
  REGISTER_SO(empty_eq_hash_tree);
  empty_eq_hash_tree = scheme_make_hash_tree(SCHEME_hashtr_eq);

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif
}

/*========================================================================*/
/*                                logging                                 */
/*========================================================================*/

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

char *scheme_optimize_context_to_string(Scheme_Object *context)
/* Convert a context to a string that is suitable for use in logging */
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

    if (SAME_TYPE(SCHEME_TYPE(func), scheme_ir_lambda_type)) {
      Scheme_Object *name;

      name = ((Scheme_Lambda *)func)->name;
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

/*========================================================================*/
/*                                  utils                                 */
/*========================================================================*/

static void set_optimize_mode(Scheme_IR_Local *var)
{
  MZ_ASSERT(SAME_TYPE(var->so.type, scheme_ir_local_type));
  memset(&var->optimize, 0, sizeof(var->optimize));
  var->mode = SCHEME_VAR_MODE_OPTIMIZE;
}

#define SCHEME_PRIM_IS_UNSAFE_NONMUTATING (SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL | SCHEME_PRIM_IS_UNSAFE_OMITABLE)

int scheme_is_functional_nonfailing_primitive(Scheme_Object *rator, int num_args, int expected_vals)
/* A call to a functional, non-failing primitive (i.e., it accepts any argument)
   can be discarded if its results are ignored.
   Return 2 => true, and results are a constant when arguments are constants. */
{
  if (SCHEME_PRIMP(rator)
      && (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & (SCHEME_PRIM_IS_OMITABLE_ANY | SCHEME_PRIM_IS_UNSAFE_NONMUTATING))
      && (num_args >= ((Scheme_Primitive_Proc *)rator)->mina)
      && (num_args <= ((Scheme_Primitive_Proc *)rator)->mu.maxa)
      && ((expected_vals < 0)
          || ((expected_vals == 1) && !(SCHEME_PRIM_PROC_FLAGS(rator) & SCHEME_PRIM_IS_MULTI_RESULT))
          || (SAME_OBJ(scheme_values_proc, rator)
              && (expected_vals == num_args)))) {
    if (SAME_OBJ(scheme_values_proc, rator))
      return 2;
    return 1;
  } else
    return 0;
}

static Scheme_Object *get_struct_proc_shape(Scheme_Object *rator, Optimize_Info *info, int prop_ok)
/* Determines whether `rator` is known to be a struct accessor, etc. */
{
  Scheme_Object *c;

  if (info
      && (info->top_level_consts || info->cp->inline_variants)
      && SAME_TYPE(SCHEME_TYPE(rator), scheme_ir_toplevel_type)) {
    int pos;
    pos = SCHEME_TOPLEVEL_POS(rator);
    c = NULL;
    if (info->top_level_consts)
      c = scheme_hash_get(info->top_level_consts, scheme_make_integer(pos));
    if (!c && info->cp->inline_variants)
      c = scheme_hash_get(info->cp->inline_variants, scheme_make_integer(pos));
    if (c && (SAME_TYPE(SCHEME_TYPE(c), scheme_struct_proc_shape_type)
              || (prop_ok && SAME_TYPE(SCHEME_TYPE(c), scheme_struct_prop_proc_shape_type)))) {
      return c;
    }
  }

  return NULL;
}

int scheme_is_struct_functional(Scheme_Object *rator, int num_args, Optimize_Info *info, int vals)
/* Determines whether `rator` is a functional, non-failing struct operation */
{
  Scheme_Object *c;

  if ((vals == 1) || (vals == -1)) {
    c = get_struct_proc_shape(rator, info, 1);
    if (c) {
      if (SAME_TYPE(SCHEME_TYPE(c), scheme_struct_proc_shape_type)) {
        int mode = (SCHEME_PROC_SHAPE_MODE(c) & STRUCT_PROC_SHAPE_MASK);
        int field_count = (SCHEME_PROC_SHAPE_MODE(c) >> STRUCT_PROC_SHAPE_SHIFT);
        if (((num_args == 1) && (mode == STRUCT_PROC_SHAPE_PRED))
            || ((num_args == field_count) && (mode == STRUCT_PROC_SHAPE_CONSTR))) {
          return 1;
        }
      } else if (SAME_TYPE(SCHEME_TYPE(c), scheme_struct_prop_proc_shape_type)) {
        int mode = (SCHEME_PROP_PROC_SHAPE_MODE(c) & STRUCT_PROC_SHAPE_MASK);
        if ((mode == STRUCT_PROP_PROC_SHAPE_PRED)
            && (num_args == 1))
          return 1;
      }
    }
  }

  return 0;
}

static Scheme_Object *extract_specialized_proc(Scheme_Object *le, Scheme_Object *default_val)
/* Look through `(procedure-specialize <e>)` to get `<e>` */
{
  if (SAME_TYPE(SCHEME_TYPE(le), scheme_application2_type)) {
    Scheme_App2_Rec *app = (Scheme_App2_Rec *)le;
    if (SAME_OBJ(app->rator, scheme_procedure_specialize_proc)) {
      if (SCHEME_PROCP(app->rand) || SCHEME_LAMBDAP(app->rand))
        return app->rand;
    }
  }

  return default_val;
}

int scheme_omittable_expr(Scheme_Object *o, int vals, int fuel, int flags,
                          Optimize_Info *opt_info, Optimize_Info *warn_info)
     /* Checks whether the bytecode `o` returns `vals` values with no
        side-effects and without pushing and using continuation marks.
        A -1 for `vals` means that any return count is ok.
        Also used with fully resolved expression by `module' to check
        for "functional" bodies, in which case `flags` includes
        `OMITTABLE_RESOLVED`.
        The `opt_info` argument is used only to access module-level
        information, not local bindings.
        If `warn_info` is supplied, complain when a mismatch is detected.
        We rely on the letrec-check pass to avoid omitting early references
        to letrec-bound variables, but `flags` can include `OMITTABLE_KEEP_VARS`
        to keep all variable references.
        If flags includes `OMITTABLE_KEEP_MUTABLE_VARS`, then references
        to mutable variables are kept, which allows this function to be
        a conservative approximation for "reorderable". */
{
  Scheme_Type vtype;

  /* FIXME: can overflow the stack */

 try_again:

  vtype = SCHEME_TYPE(o);

  if ((vtype > _scheme_ir_values_types_)
      || ((vtype == scheme_ir_local_type)
          && !(flags & OMITTABLE_KEEP_VARS)
          && (!(flags & OMITTABLE_KEEP_MUTABLE_VARS)
              || !SCHEME_VAR(o)->mutated))
      || ((vtype == scheme_local_type)
          && !(flags & OMITTABLE_KEEP_VARS)
          && !(SCHEME_GET_LOCAL_FLAGS(o) == SCHEME_LOCAL_CLEAR_ON_READ))
      || ((vtype == scheme_local_unbox_type)
          && !(flags & (OMITTABLE_KEEP_VARS | OMITTABLE_KEEP_MUTABLE_VARS))
          && !(SCHEME_GET_LOCAL_FLAGS(o) == SCHEME_LOCAL_CLEAR_ON_READ))
      || (vtype == scheme_lambda_type)
      || (vtype == scheme_ir_lambda_type)
      || (vtype == scheme_inline_variant_type)
      || (vtype == scheme_case_lambda_sequence_type)
      || (vtype == scheme_quote_syntax_type)
      || (vtype == scheme_varref_form_type)
      || (vtype == scheme_ir_quote_syntax_type)) {
    note_match(1, vals, warn_info);
    return ((vals == 1) || (vals < 0));
  }

  if (vtype == scheme_toplevel_type) {
    note_match(1, vals, warn_info);
    if (!(flags & OMITTABLE_KEEP_VARS) && (flags & OMITTABLE_RESOLVED) && ((vals == 1) || (vals < 0))) {
      if (SCHEME_TOPLEVEL_FLAGS(o) & SCHEME_TOPLEVEL_FLAGS_MASK)
        return 1;
      else
        return 0;
    }
  }

  if (vtype == scheme_ir_toplevel_type) {
    note_match(1, vals, warn_info);
    if ((vals == 1) || (vals < 0)) {
      if (!(flags & OMITTABLE_KEEP_VARS)
          && ((SCHEME_TOPLEVEL_FLAGS(o) & SCHEME_TOPLEVEL_FLAGS_MASK) >= SCHEME_TOPLEVEL_READY))
        return 1;
      else if ((SCHEME_TOPLEVEL_FLAGS(o) & SCHEME_TOPLEVEL_FLAGS_MASK) >= SCHEME_TOPLEVEL_FIXED)
        return 1;
      else
        return 0;
    }
  }

  if (vtype == scheme_branch_type) {
    Scheme_Branch_Rec *b;
    b = (Scheme_Branch_Rec *)o;
    return (scheme_omittable_expr(b->test, 1, fuel - 1, flags, opt_info, warn_info)
	    && scheme_omittable_expr(b->tbranch, vals, fuel - 1, flags, opt_info, warn_info)
	    && scheme_omittable_expr(b->fbranch, vals, fuel - 1, flags, opt_info, warn_info));
  }

  if (vtype == scheme_let_one_type) {
    Scheme_Let_One *lo = (Scheme_Let_One *)o;
    return (scheme_omittable_expr(lo->value, 1, fuel - 1, flags, opt_info, warn_info)
	    && scheme_omittable_expr(lo->body, vals, fuel - 1, flags, opt_info, warn_info));
  }

  if (vtype == scheme_let_void_type) {
    Scheme_Let_Void *lv = (Scheme_Let_Void *)o;
    /* recognize (letrec ([x <omittable>]) ...): */
    MZ_ASSERT(flags & OMITTABLE_RESOLVED);
    if (SAME_TYPE(SCHEME_TYPE(lv->body), scheme_let_value_type)) {
      Scheme_Let_Value *lv2 = (Scheme_Let_Value *)lv->body;
      if ((lv2->count == 1)
          && (lv2->position == 0)
          && scheme_omittable_expr(lv2->value, 1, fuel - 1, flags, opt_info, warn_info)) {
        o = lv2->body;
      } else
        o = lv->body;
    } else
      o = lv->body;
    goto try_again;
  }

  if (vtype == scheme_ir_let_header_type) {
    /* recognize another (let ([x <omittable>]) ...) pattern: */
    Scheme_IR_Let_Header *lh = (Scheme_IR_Let_Header *)o;
    int i;
    MZ_ASSERT(!(flags & OMITTABLE_RESOLVED));
    o = lh->body;
    for (i = 0; i < lh->num_clauses; i++) {
      Scheme_IR_Let_Value *lv = (Scheme_IR_Let_Value *)o;
      if (!scheme_omittable_expr(lv->value, lv->count, fuel - 1, flags, opt_info, warn_info))
        return 0;
      o = lv->body;
    }
    goto try_again;
  }

  if (vtype == scheme_letrec_type) {
    MZ_ASSERT(flags & OMITTABLE_RESOLVED);
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
        || scheme_is_struct_functional(app->args[0], app->num_args, opt_info, vals)
        || ((SCHEME_APPN_FLAGS(app) & APPN_FLAG_OMITTABLE) && !(flags & OMITTABLE_IGNORE_APPN_OMIT))) {
      int i;
      for (i = app->num_args; i--; ) {
        if (!scheme_omittable_expr(app->args[i + 1], 1, fuel - 1, flags, opt_info, warn_info))
          return 0;
      }
      return 1;
    } else if (SCHEME_PRIMP(app->args[0])) {
      if (!(SCHEME_PRIM_PROC_FLAGS(app->args[0]) & SCHEME_PRIM_IS_MULTI_RESULT)) {
        note_match(1, vals, warn_info);
      } else if (SAME_OBJ(scheme_values_proc, app->args[0])) {
        note_match(app->num_args, vals, warn_info);
      }
    }

    if (!SAME_OBJ(scheme_make_struct_type_proc, app->args[0]))
      return 0;
  }

  if (vtype == scheme_application2_type) {
    Scheme_App2_Rec *app = (Scheme_App2_Rec *)o;
    if (scheme_is_functional_nonfailing_primitive(app->rator, 1, vals)
        || scheme_is_struct_functional(app->rator, 1, opt_info, vals)
        || ((SCHEME_APPN_FLAGS(app) & APPN_FLAG_OMITTABLE) && !(flags & OMITTABLE_IGNORE_APPN_OMIT))) {
      if (scheme_omittable_expr(app->rand, 1, fuel - 1, flags, opt_info, warn_info))
        return 1;
    } else if (SAME_OBJ(app->rator, scheme_make_vector_proc)
               && (vals == 1 || vals == -1)
               && (SCHEME_INTP(app->rand) 
                   && (SCHEME_INT_VAL(app->rand) >= 0))
                   && IN_FIXNUM_RANGE_ON_ALL_PLATFORMS(SCHEME_INT_VAL(app->rand))) {
      return 1;
    } else if (SAME_OBJ(app->rator, scheme_procedure_specialize_proc)) {
      if ((vals == 1 || vals == -1) && extract_specialized_proc(o, NULL))
        return 1;
    } else if (SCHEME_PRIMP(app->rator)) {
      if (!(SCHEME_PRIM_PROC_FLAGS(app->rator) & SCHEME_PRIM_IS_MULTI_RESULT)
          || SAME_OBJ(scheme_values_proc, app->rator)) {
        note_match(1, vals, warn_info);
      }
    }

    if (!SAME_OBJ(scheme_make_struct_type_property_proc, app->rator))
      return 0;
  }

  if (vtype == scheme_application3_type) {
    Scheme_App3_Rec *app = (Scheme_App3_Rec *)o;
    if (scheme_is_functional_nonfailing_primitive(app->rator, 2, vals)
        || scheme_is_struct_functional(app->rator, 2, opt_info, vals)
        || ((SCHEME_APPN_FLAGS(app) & APPN_FLAG_OMITTABLE) && !(flags & OMITTABLE_IGNORE_APPN_OMIT))) {
      if (scheme_omittable_expr(app->rand1, 1, fuel - 1, flags, opt_info, warn_info)
          && scheme_omittable_expr(app->rand2, 1, fuel - 1, flags, opt_info, warn_info))
        return 1;
    } else if (SAME_OBJ(app->rator, scheme_make_vector_proc)
               && (vals == 1 || vals == -1)
               && (SCHEME_INTP(app->rand1)
                   && (SCHEME_INT_VAL(app->rand1) >= 0)
                   && IN_FIXNUM_RANGE_ON_ALL_PLATFORMS(SCHEME_INT_VAL(app->rand1)))
               && scheme_omittable_expr(app->rand2, 1, fuel - 1, flags, opt_info, warn_info)) {
      return 1;
    } else if (SCHEME_PRIMP(app->rator)) {
      if (!(SCHEME_PRIM_PROC_FLAGS(app->rator) & SCHEME_PRIM_IS_MULTI_RESULT)) {
        note_match(1, vals, warn_info);
      } else if (SAME_OBJ(scheme_values_proc, app->rator)) {
        note_match(2, vals, warn_info);
      }
    }

    if (!SAME_OBJ(scheme_make_struct_type_property_proc, app->rator))
      return 0;
  }

  /* check for (set! x x) */
  if (vtype == scheme_set_bang_type) {
    Scheme_Set_Bang *sb = (Scheme_Set_Bang *)o;
    if (SAME_TYPE(scheme_local_type, SCHEME_TYPE(sb->var))
        && SAME_TYPE(scheme_local_type, SCHEME_TYPE(sb->val))
        && (SCHEME_LOCAL_POS(sb->var) == SCHEME_LOCAL_POS(sb->val)))
      return 1;
    else if (SAME_TYPE(scheme_ir_local_type, SCHEME_TYPE(sb->var))
             && SAME_OBJ(sb->var, sb->val))
      return 1;
  }

  /* check for struct-type declaration: */
  if (!(flags & OMITTABLE_IGNORE_MAKE_STRUCT_TYPE)) {
    Scheme_Object *auto_e;
    int auto_e_depth;
    auto_e = scheme_is_simple_make_struct_type(o, vals,
                                               (((flags & OMITTABLE_RESOLVED) ? CHECK_STRUCT_TYPE_RESOLVED : 0)
                                                | CHECK_STRUCT_TYPE_ALWAYS_SUCCEED
                                                | CHECK_STRUCT_TYPE_DELAY_AUTO_CHECK),
                                               &auto_e_depth, 
                                               NULL, NULL,
                                               (opt_info ? opt_info->top_level_consts : NULL),
                                               ((opt_info && opt_info->cp) ? opt_info->cp->inline_variants : NULL),
                                               NULL, NULL, 0, NULL, NULL, NULL,
                                               5);
    if (auto_e) {
      if (scheme_omittable_expr(auto_e, 1, fuel - 1, flags, opt_info, warn_info))
        return 1;
    }
  }

  /* check for struct-type property declaration: */
  if (!(flags & OMITTABLE_IGNORE_MAKE_STRUCT_TYPE)) {
    if (scheme_is_simple_make_struct_type_property(o, vals,
                                                   (((flags & OMITTABLE_RESOLVED) ? CHECK_STRUCT_TYPE_RESOLVED : 0)
                                                    | CHECK_STRUCT_TYPE_ALWAYS_SUCCEED),
                                                   NULL,
                                                   (opt_info ? opt_info->top_level_consts : NULL),
                                                   ((opt_info && opt_info->cp) ? opt_info->cp->inline_variants : NULL),
                                                   NULL, NULL, 0, NULL, NULL,
                                                   5))
      return 1;
  }

  return 0;
}

static Scheme_Object *ensure_single_value(Scheme_Object *e, Optimize_Info *info)
/* Wrap `e` so that it either produces a single value or fails */
{
  Scheme_App2_Rec *app2;
  if (single_valued_expression(e, info, 5))
    return e;

  app2 = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
  app2->iso.so.type = scheme_application2_type;
  app2->rator = scheme_values_proc;
  app2->rand = e;
  SCHEME_APPN_FLAGS(app2) |= (APPN_FLAG_IMMED | APPN_FLAG_SFS_TAIL);
  
  return (Scheme_Object *)app2;
}

static Scheme_Object *ensure_single_value_noncm(Scheme_Object *e, Optimize_Info *info)
/* Wrap `e` so that it either produces a single value or fails.
   Also, wrap `e` in case it may have a `with-continuation-mark`
   in tail position. */
{
  Scheme_App2_Rec *app2;
  if (single_valued_noncm_expression(e, info, 5))
    return e;

  app2 = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
  app2->iso.so.type = scheme_application2_type;
  app2->rator = scheme_values_proc;
  app2->rand = e;
  SCHEME_APPN_FLAGS(app2) |= (APPN_FLAG_IMMED | APPN_FLAG_SFS_TAIL);
  
  return (Scheme_Object *)app2;
}

static Scheme_Object *ensure_noncm(Scheme_Object *e, Optimize_Info *info)
/* Wrap `e` in case it may have a `with-continuation-mark` form in tail
   position. This is useful when `e` escapes, and it is lifted and the
   surrounding is discarded, in which case the shift out of a nested
   position is observable. */
{
  Scheme_Sequence *seq;

  if (noncm_expression(e, info, 5))
    return e;

  seq = scheme_malloc_sequence(1);
  seq->so.type = scheme_begin0_sequence_type;
  seq->count = 1;
  seq->array[0] = e;
    
  return (Scheme_Object *)seq;
}

static Scheme_Object *do_make_discarding_sequence(Scheme_Object *e1, Scheme_Object *e2,
                                                  Optimize_Info *info,
                                                  int ignored, int rev)
/* Evaluate `e1` then `e2` (or opposite order if rev), and each must
   produce a single value. The result of `e1` is ignored and the
   result is `e2` --- except that `e2` is ignored, too, if
   `ignored`. */
{
  if (ignored)
    e2 = optimize_ignored(e2, info, 1, 0, 5);
    
  e2 = ensure_single_value_noncm(e2, info);
  
  if (scheme_omittable_expr(e1, 1, 5, 0, info, NULL))
    return e2;
    
  e1 = ensure_single_value(optimize_ignored(e1, info, 1, 0, 5), info);

  if (ignored && scheme_omittable_expr(e2, 1, 5, 0, info, NULL))
    return ensure_single_value_noncm(e1, info);

  /* use `begin` instead of `begin0` if we can swap the order: */
  if (rev && movable_expression(e2, info, 0, 1, 1, 0, 50))
    rev = 0;

  if (!rev && SAME_TYPE(SCHEME_TYPE(e1), scheme_sequence_type)) {
    Scheme_Sequence *seq = (Scheme_Sequence *)e1;

    if (SCHEME_TYPE(seq->array[seq->count - 1]) > _scheme_ir_values_types_) {
      seq->array[seq->count - 1] = e2;
      return e1;
    }
  }

  return scheme_make_sequence_compilation(scheme_make_pair((rev ? e2 : e1),
                                                           scheme_make_pair((rev ? e1 : e2), scheme_null)),
                                          rev ? -1 : 1,
                                          0);
}

static Scheme_Object *make_discarding_sequence(Scheme_Object *e1, Scheme_Object *e2,
                                               Optimize_Info *info)
{
  return do_make_discarding_sequence(e1, e2, info, 0, 0);
}

static Scheme_Object *make_discarding_reverse_sequence(Scheme_Object *e1, Scheme_Object *e2,
                                                       Optimize_Info *info)
{
  return do_make_discarding_sequence(e1, e2, info, 0, 1);
}

static Scheme_Object *make_discarding_sequence_3(Scheme_Object *e1, Scheme_Object *e2, Scheme_Object *e3,
                                                 Optimize_Info *info)
{
  return make_discarding_sequence(e1, make_discarding_sequence(e2, e3, info), info);
}

static Scheme_Object *make_discarding_app_sequence(Scheme_App_Rec *appr, int result_pos, Scheme_Object *result,
                                                   Optimize_Info *info)
/* Generalize do_make_discarding_sequence() to a sequence of argument
   expressions, where `result_pos` is the position of the returned
   argument. If `result_pos` is -1, then all argument results will be
   ignored. If `result`, then it is used as the result after all
   arguments are evaluated.*/
{
  int i;
  Scheme_Object *l = scheme_null;

  result_pos = result_pos + 1;
  if (result)
    l = scheme_make_pair(result, l);

  for (i = appr->num_args; i; i--) {
    Scheme_Object *e;
    e = appr->args[i];
    e = ensure_single_value(e, info);
    if (i == result_pos) {
      if (SCHEME_NULLP(l)) {
        e = ensure_single_value_noncm(e, info);
        l = scheme_make_pair(e, scheme_null);
      } else {
        l = scheme_make_sequence_compilation(scheme_make_pair(e, l), -1, 0);
        l = scheme_make_pair(l, scheme_null);
      }
    } else {
      e = optimize_ignored(e, info, 1, 1, 5);
      if (e)
        l = scheme_make_pair(e, l);
    }
  }

  if (SCHEME_NULLP(l))
    return scheme_void;

  if (SCHEME_NULLP(SCHEME_CDR(l)))
    return SCHEME_CAR(l);

  return scheme_make_sequence_compilation(l, 1, 0);
}

static Scheme_Object *optimize_ignored(Scheme_Object *e, Optimize_Info *info,
                                       int expected_vals, int maybe_omittable,
                                       int fuel)
/* Simplify an expression whose result will be ignored.  The
   `expected_vals` is 1 or -1. If `maybe_omittable`, the result can be
   NULL to indicate that it can be omitted. */
{
  if (scheme_omittable_expr(e, expected_vals, 5, 0, info, NULL))
    return maybe_omittable? NULL : scheme_false;

  if (fuel) {
    /* We could do a lot more here, but for now, we just avoid purely
       functional, always successful operations --- especially allocating ones. */
    switch (SCHEME_TYPE(e)) {
    case scheme_application2_type:
      {
        Scheme_App2_Rec *app = (Scheme_App2_Rec *)e;

        if (!SAME_OBJ(app->rator, scheme_values_proc)) /* `values` is probably here to ensure a single result */
          if (scheme_is_functional_nonfailing_primitive(app->rator, 1, expected_vals))
            return do_make_discarding_sequence(app->rand, scheme_void, info, 1, 0);
            
        /* (make-vector <num>) => <void> */
        if (SAME_OBJ(app->rator, scheme_make_vector_proc)
            && (SCHEME_INTP(app->rand) 
                && (SCHEME_INT_VAL(app->rand) >= 0))
                && IN_FIXNUM_RANGE_ON_ALL_PLATFORMS(SCHEME_INT_VAL(app->rand)))
          return (maybe_omittable ? NULL : scheme_void);
      }
      break;
    case scheme_application3_type:
      {
        Scheme_App3_Rec *app = (Scheme_App3_Rec *)e;

        if (scheme_is_functional_nonfailing_primitive(app->rator, 2, expected_vals))
          return do_make_discarding_sequence(app->rand1,
                                             do_make_discarding_sequence(app->rand2,
                                                                         scheme_void,
                                                                         info,
                                                                         1, 0),
                                             info,
                                             1, 0);
        
        /* (make-vector <num> <expr>) => <expr> */
        if (SAME_OBJ(app->rator, scheme_make_vector_proc)
            && (SCHEME_INTP(app->rand1) 
                && (SCHEME_INT_VAL(app->rand1) >= 0))
                && IN_FIXNUM_RANGE_ON_ALL_PLATFORMS(SCHEME_INT_VAL(app->rand1))) {
          Scheme_Object *val;
          val = ensure_single_value_noncm(app->rand2, info);
          return optimize_ignored(val, info, 1, maybe_omittable, 5);
        }
      }
      break;
    case scheme_application_type:
      {
        Scheme_App_Rec *app = (Scheme_App_Rec *)e;

        if (scheme_is_functional_nonfailing_primitive(app->args[0], app->num_args, expected_vals))
          return make_discarding_app_sequence(app, -1, NULL, info);
      }
      break;
    case scheme_branch_type:
      {
        Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)e;
        Scheme_Object *tb, *fb;
        
        tb = optimize_ignored(b->tbranch, info, expected_vals, 1, fuel - 1);
        fb = optimize_ignored(b->fbranch, info, expected_vals, 1, fuel - 1);
        
        if (tb || fb) {
          b->tbranch = tb ? tb : scheme_false;
          b->fbranch = fb ? fb : scheme_false;
          return (Scheme_Object*)b;
        } else {
          Scheme_Object *val;
          val = ensure_single_value_noncm(b->test, info);
          return optimize_ignored(val, info, 1, maybe_omittable, 5);
        }
      }
      break;
    case scheme_sequence_type:
      {
        Scheme_Sequence *seq = (Scheme_Sequence *)e;
        Scheme_Object *last;
        
        last = optimize_ignored(seq->array[seq->count - 1], info, expected_vals, 1, fuel - 1);
        
        if (last) {
          seq->array[seq->count - 1] = last;
          return (Scheme_Object*)seq;
        } else if (seq->count == 2
                   && (expected_vals == -1
                       || single_valued_noncm_expression(seq->array[0], info, 5))) {
          return seq->array[0];
        } else {
          seq->array[seq->count - 1] = scheme_false;
          return (Scheme_Object*)seq;
        }
      }
    case scheme_begin0_sequence_type:
      {
        Scheme_Sequence *seq = (Scheme_Sequence *)e;
        Scheme_Object *first;
        
        first = optimize_ignored(seq->array[0], info, expected_vals, 1, fuel - 1);
        
        if (first) {
          seq->array[0] = first;
          return (Scheme_Object*)seq;
        } else if (seq->count == 2
                   && (expected_vals == -1
                       || single_valued_noncm_expression(seq->array[1], info, 5))) {
          return seq->array[1];
        } else {
          seq->array[0] = scheme_false;
          return (Scheme_Object*)seq;
        }
      }
      break;
    case scheme_ir_let_header_type:
      {
        Scheme_IR_Let_Header *head = (Scheme_IR_Let_Header *)e;
        Scheme_IR_Let_Value *lv;
        Scheme_Object *body;
        int i;

        body = head->body;
        if (0 == head->num_clauses)
          lv = (Scheme_IR_Let_Value *)body;
        for (i = head->num_clauses; i--; ) {
          lv = (Scheme_IR_Let_Value *)body;
          body = lv->body;
        }
        body = optimize_ignored(body, info, expected_vals, 0, fuel - 1);
        lv->body = body;
        return (Scheme_Object*)head;
      }
      break;
    }
  }

  return e;
}

static Scheme_Object *make_sequence_2(Scheme_Object *a, Scheme_Object *b)
{
  return scheme_make_sequence_compilation(scheme_make_pair(a, scheme_make_pair(b, scheme_null)), 1, 0);
}

static Scheme_Object *make_discarding_first_sequence(Scheme_Object *e1, Scheme_Object *e2,
                                                     Optimize_Info *info)
/* Like make_discarding_sequence(), but second expression is not constrained to
   a single result. */
{
  e1 = optimize_ignored(e1, info, 1, 1, 5);
  if (!e1)
    return e2;
  e1 = ensure_single_value(e1, info);
  return make_sequence_2(e1, e2);
}

static Scheme_Object *make_application_2(Scheme_Object *a, Scheme_Object *b, Optimize_Info *info)
{
  return scheme_make_application(scheme_make_pair(a, scheme_make_pair(b, scheme_null)), info);
}

static Scheme_Object *make_application_3(Scheme_Object *a, Scheme_Object *b, Scheme_Object *c,
                                         Optimize_Info *info)
{
  return scheme_make_application(scheme_make_pair(a, scheme_make_pair(b, scheme_make_pair(c, scheme_null))),
                                 info);
}

static Scheme_Object *replace_tail_inside(Scheme_Object *alt, Scheme_Object *inside, Scheme_Object *orig)
/* Installs a new expression in the result position of various forms, such as `begin`;
   extract_tail_inside() needs to be consistent with this function */
{
  if (inside) {
    switch (SCHEME_TYPE(inside)) {
    case scheme_sequence_type:
      if (((Scheme_Sequence *)inside)->count)
        ((Scheme_Sequence *)inside)->array[((Scheme_Sequence *)inside)->count-1] = alt;
      else
        scheme_signal_error("internal error: strange inside replacement");
      break;
    case scheme_ir_let_header_type:
      ((Scheme_IR_Let_Header *)inside)->body = alt;
      break;
    case scheme_ir_let_value_type:
      ((Scheme_IR_Let_Value *)inside)->body = alt;
      break;
    default:
      scheme_signal_error("internal error: strange inside replacement");
    }
    return orig;
  }
  return alt;
}

static void extract_tail_inside(Scheme_Object **_t2, Scheme_Object **_inside)
/* Looks through various forms, like `begin` to extract a result expression;
   replace_tail_inside() needs to be consistent with this function */
{
  while (1) {
    if (SAME_TYPE(SCHEME_TYPE(*_t2), scheme_ir_let_header_type)) {
      Scheme_IR_Let_Header *head = (Scheme_IR_Let_Header *)*_t2;
      int i;
      *_inside = *_t2;
      *_t2 = head->body;
      for (i = head->num_clauses; i--; ) {
        *_inside = *_t2;
        *_t2 = ((Scheme_IR_Let_Value *)*_t2)->body;
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

Scheme_Object *scheme_optimize_extract_tail_inside(Scheme_Object *t2)
{
  Scheme_Object *inside;
  extract_tail_inside(&t2, &inside);
  return t2;
}

/*========================================================================*/
/*        detecting `make-struct-type` calls and struct shapes            */
/*========================================================================*/

static int is_inspector_call(Scheme_Object *a)
/* Does `a` produce an inspector? */
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

static int is_proc_spec_proc(Scheme_Object *p, int init_field_count)
/* Does `p` produce a good `prop:procedure` value? */
{
  Scheme_Type vtype;

  if (SCHEME_INTP(p)
      && (SCHEME_INT_VAL(p) >= 0)
      && (SCHEME_INT_VAL(p) < init_field_count))
    return 1;

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

  if ((vtype == scheme_lambda_type) || (vtype == scheme_ir_lambda_type)) {
    if (((Scheme_Lambda *)p)->num_params >= 1)
      return 1;
  }

  return 0;
}

static int is_local_ref(Scheme_Object *e, int p, int r, Scheme_IR_Local **vars)
/* Does `e` refer to...
    In resolved mode: variables at offet `p` though `p+r`?
    In optimizer IR mode: variables in `vars`? */
{
  if (!vars && SAME_TYPE(SCHEME_TYPE(e), scheme_local_type)) {
    if ((SCHEME_LOCAL_POS(e) >= p)
        && (SCHEME_LOCAL_POS(e) < (p + r)))
      return 1;
  } else if (vars && SAME_TYPE(SCHEME_TYPE(e), scheme_ir_local_type)) {
    int i;
    for (i = p; i < p + r; i++) {
      if (SAME_OBJ(e, (Scheme_Object *)vars[i]))
        return 1;
    }
  }

  return 0;
}

static int is_int_list(Scheme_Object *o, int up_to)
/* Is `o` a list of distinct integers that are less than `up_to`? */
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
                                int delta2, int field_count, Scheme_IR_Local **vars)
/* Does `rator` plus `rand1` and `rand2` create a struct accessor or mutator? */
{
  if ((SAME_OBJ(rator, scheme_make_struct_field_accessor_proc)
       && is_local_ref(rand1, delta2+3, 1, vars))
      || (SAME_OBJ(rator, scheme_make_struct_field_mutator_proc)
          && is_local_ref(rand1, delta2+4, 1, vars))) {
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
                                                 Simple_Struct_Type_Info *_stinfo,
                                                 Scheme_IR_Local **vars)
/* Does `e` produce values for a structure type, mutators, and accessors in the
   usual order? */
{
  if (SAME_TYPE(SCHEME_TYPE(e), scheme_application_type)) {
    Scheme_App_Rec *app = (Scheme_App_Rec *)e;
    int delta = (resolved ? app->num_args : 0);
    if (SAME_OBJ(app->args[0], scheme_values_proc)
        && (app->num_args == vals)
        && (app->num_args >= 3)
        && is_local_ref(app->args[1], delta, 1, vars)
        && is_local_ref(app->args[2], delta+1, 1, vars)
        && is_local_ref(app->args[3], delta+2, 1, vars)) {
      int i, num_gets = 0, num_sets = 0, normal_ops = 1;
      for (i = app->num_args; i > 3; i--) {
        if (is_local_ref(app->args[i], delta, 5, vars)) {
          normal_ops = 0;
        } else if (SAME_TYPE(SCHEME_TYPE(app->args[i]), scheme_application_type)
                   && _stinfo->normal_ops && !_stinfo->indexed_ops) {
          Scheme_App_Rec *app3 = (Scheme_App_Rec *)app->args[i];
          int delta2 = delta + (resolved ? app3->num_args : 0);
          if (app3->num_args == 3) {
            if (!ok_proc_creator_args(app3->args[0], app3->args[1], app3->args[2], app3->args[3],
                                      delta2, _stinfo->field_count, vars))
              break;
            if (SAME_OBJ(app3->args[0], scheme_make_struct_field_mutator_proc)) {
              if (num_gets) {
                /* Since we're alking backwards, it's not normal to hit a mutator
                   after (i.e., before in argument order) a selector */
                normal_ops = 0;
              }
              num_sets++;
            } else {
              if (SCHEME_INT_VAL(app3->args[2]) != (i - 4)) {
                /* selectors are not in the usual order */
                normal_ops = 0;
              }
              num_gets++;
            }
          } else
            break;
        } else if (SAME_TYPE(SCHEME_TYPE(app->args[i]), scheme_application3_type)
                   && _stinfo->normal_ops && !_stinfo->indexed_ops) {
          Scheme_App3_Rec *app3 = (Scheme_App3_Rec *)app->args[i];
          int delta2 = delta + (resolved ? 2 : 0);
          if (!ok_proc_creator_args(app3->rator, app3->rand1, app3->rand2, NULL,
                                    delta2, _stinfo->field_count, vars))
            break;
          if (SAME_OBJ(app3->rator, scheme_make_struct_field_mutator_proc)) {
            if (num_gets) normal_ops = 0;
            num_sets++;
          } else {
            if (SCHEME_INT_VAL(app3->rand2) != (i - 4)) normal_ops = 0;
            num_gets++;
          }
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
/* If `body` is a `begin` form that exists only to clear variables
   as installed by the SFS pass, then extract the result form. */
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

typedef int (*Ok_Value_Callback)(void *data, Scheme_Object *v, int mode);
#define OK_CONSTANT_SHAPE          1
#define OK_CONSTANT_ENCODED_SHAPE  2
#define OK_CONSTANT_VALIDATE_SHAPE 3
#define OK_CONSTANT_VARIANT        4
#define OK_CONSTANT_VALUE          5

static int is_ok_value(Ok_Value_Callback ok_value, void *data,
                       Scheme_Object *arg, 
                       Scheme_Hash_Table *top_level_consts, 
                       Scheme_Hash_Table *inline_variants, 
                       Scheme_Hash_Table *top_level_table,
                       Scheme_Object **runstack, int rs_delta,
                       Scheme_Object **symbols, Scheme_Hash_Table *symbol_table)
/* Does `arg` produce a value that satisfies `ok_value`? */
{
  int pos;
  Scheme_Object *v;

  if (SAME_TYPE(SCHEME_TYPE(arg), scheme_ir_toplevel_type)) {
    pos = SCHEME_TOPLEVEL_POS(arg);
    if (top_level_consts || inline_variants) {
      /* This is optimize mode */
      v = NULL;
      if (top_level_consts)
        v = scheme_hash_get(top_level_consts, scheme_make_integer(pos));
      if (!v && inline_variants)
        v = scheme_hash_get(inline_variants, scheme_make_integer(pos));
      if (v)
        return ok_value(data, v, OK_CONSTANT_SHAPE);
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
      if (b->val && (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_IS_CONSISTENT))
        return ok_value(data, b->val, OK_CONSTANT_VALUE);
    }
    if (symbols) {
      /* This is module-export mode; conceptually, this code belongs in 
         setup_accessible_table() */
      Scheme_Object *name;
      name = symbols[pos];
      if (SCHEME_SYMBOLP(name)) {
        v = scheme_hash_get(symbol_table, name);
        if (v)
          return ok_value(data, v, OK_CONSTANT_VARIANT);
      } else if (SAME_TYPE(SCHEME_TYPE(name), scheme_module_variable_type)) {
        if (((Module_Variable *)name)->shape)
          return ok_value(data, ((Module_Variable *)name)->shape, OK_CONSTANT_ENCODED_SHAPE);
      }
    }
    if (top_level_table) {
      /* This is validate mode; conceptually, this code belongs in 
         define_values_validate() */
      v = scheme_hash_get(top_level_table, scheme_make_integer(pos));
      if (v) {
        return ok_value(data, v, OK_CONSTANT_VALIDATE_SHAPE);
      }
    }
  }

  return 0;
}

static int ok_constant_super_value(void *data, Scheme_Object *v, int mode)
/* Is `v` a structure type (which can serve as a supertype)? */
{
  Scheme_Object **_parent_identity = (Scheme_Object **)data;

  if (mode == OK_CONSTANT_SHAPE) {
    if (SAME_TYPE(SCHEME_TYPE(v), scheme_struct_proc_shape_type)) {
      int mode = (SCHEME_PROC_SHAPE_MODE(v) & STRUCT_PROC_SHAPE_MASK);
      int field_count = (SCHEME_PROC_SHAPE_MODE(v) >> STRUCT_PROC_SHAPE_SHIFT);
      if (mode == STRUCT_PROC_SHAPE_STRUCT) {
        if (_parent_identity)
          *_parent_identity = SCHEME_PROC_SHAPE_IDENTITY(v);
        return field_count + 1;
      }
    }
  } else if (mode == OK_CONSTANT_ENCODED_SHAPE) {
    intptr_t k;
    if (scheme_decode_struct_shape(v, &k)) {
      if ((k & STRUCT_PROC_SHAPE_MASK) == STRUCT_PROC_SHAPE_STRUCT)
        return (k >> STRUCT_PROC_SHAPE_SHIFT) + 1;
    }
  } else if (mode == OK_CONSTANT_VALIDATE_SHAPE) {
    int k = SCHEME_INT_VAL(v);
    if ((k >= 0)
        && (k & STRUCT_PROC_SHAPE_MASK) == STRUCT_PROC_SHAPE_STRUCT)
      return (k >> STRUCT_PROC_SHAPE_SHIFT) + 1;
  } else if (mode == OK_CONSTANT_VARIANT) {
    if (SCHEME_VECTORP(v) && (SCHEME_VEC_SIZE(v) == 3)) {
      if (_parent_identity)
        *_parent_identity = SCHEME_VEC_ELS(v)[2];
      v = SCHEME_VEC_ELS(v)[1];
      if (v && SCHEME_INTP(v)) {
        int mode = (SCHEME_INT_VAL(v) & STRUCT_PROC_SHAPE_MASK);
        int field_count = (SCHEME_INT_VAL(v) >> STRUCT_PROC_SHAPE_SHIFT);
        if (mode == STRUCT_PROC_SHAPE_STRUCT)
          return field_count + 1;
      }
    }
  } else if (mode == OK_CONSTANT_VALUE) {
    if (SCHEME_STRUCT_TYPEP(v)) {
      Scheme_Struct_Type *st = (Scheme_Struct_Type *)v;
      if (st->num_slots == st->num_islots)
        return st->num_slots + 1;
    }
  }

  return 0;
}

static int is_constant_super(Scheme_Object *arg, 
                             Scheme_Hash_Table *top_level_consts, 
                             Scheme_Hash_Table *inline_variants, 
                             Scheme_Hash_Table *top_level_table,
                             Scheme_Object **runstack, int rs_delta,
                             Scheme_Object **symbols, Scheme_Hash_Table *symbol_table,
                             Scheme_Object **_parent_identity)
/* Does `arg` produce another structure type (which can serve as a supertype)? */
{
  return is_ok_value(ok_constant_super_value, _parent_identity,
                     arg,
                     top_level_consts,
                     inline_variants, top_level_table,
                     runstack, rs_delta,
                     symbols, symbol_table);
}

static int ok_constant_property_with_guard(void *data, Scheme_Object *v, int mode)
{
  intptr_t k = 0;

  if (mode == OK_CONSTANT_SHAPE) {
    if (SAME_TYPE(SCHEME_TYPE(v), scheme_struct_prop_proc_shape_type)) {
      k = SCHEME_PROP_PROC_SHAPE_MODE(v);
    }
  } else if (mode == OK_CONSTANT_ENCODED_SHAPE) {
    if (!scheme_decode_struct_prop_shape(v, &k))
      k = 0;
  } else if (mode == OK_CONSTANT_VALIDATE_SHAPE) {
    int k = SCHEME_INT_VAL(v);
    if (k < 0)
      k = -(k+1);
    else
      k = 0;
  } else if (mode == OK_CONSTANT_VARIANT) {
    if (SCHEME_VECTORP(v) && (SCHEME_VEC_SIZE(v) == 4)) {
      v = SCHEME_VEC_ELS(v)[1];
      if (v && SCHEME_INTP(v))
        k = SCHEME_INT_VAL(v);
    }
  } else if (mode == OK_CONSTANT_VALUE) {
    if (SAME_TYPE(SCHEME_TYPE(v), scheme_struct_property_type)) {
      if (!((Scheme_Struct_Property *)v)->guard)
        return 1;
    }
  }

  return (k == STRUCT_PROP_PROC_SHAPE_PROP);
}

static int is_struct_type_property_without_guard(Scheme_Object *arg, 
                                                 Scheme_Hash_Table *top_level_consts, 
                                                 Scheme_Hash_Table *inline_variants, 
                                                 Scheme_Hash_Table *top_level_table,
                                                 Scheme_Object **runstack, int rs_delta,
                                                 Scheme_Object **symbols, Scheme_Hash_Table *symbol_table)
/* Does `arg` produce a structure type property that has no guard (so that any value is ok)? */
{
  return is_ok_value(ok_constant_property_with_guard, NULL,
                     arg,
                     top_level_consts,
                     inline_variants, top_level_table,
                     runstack, rs_delta,
                     symbols, symbol_table);
}

static int is_simple_property_list(Scheme_Object *a, int resolved,
                                   Scheme_Hash_Table *top_level_consts, 
                                   Scheme_Hash_Table *inline_variants, 
                                   Scheme_Hash_Table *top_level_table,
                                   Scheme_Object **runstack, int rs_delta,
                                   Scheme_Object **symbols, Scheme_Hash_Table *symbol_table,
                                   int just_for_authentic, int *_authentic)
/* Does `a` produce a property list that always lets `make-struct-type` succeed? */
{
  Scheme_Object *arg;
  int i, count;
  
  if (SAME_TYPE(SCHEME_TYPE(a), scheme_application_type)) {
    if (!SAME_OBJ(((Scheme_App_Rec *)a)->args[0], scheme_list_proc))
      return 0;
    count = ((Scheme_App_Rec *)a)->num_args;
  } else if (SAME_TYPE(SCHEME_TYPE(a), scheme_application2_type)) {
    if (!SAME_OBJ(((Scheme_App2_Rec *)a)->rator, scheme_list_proc))
      return 0;
    count = 1;
  } else if (SAME_TYPE(SCHEME_TYPE(a), scheme_application3_type)) {
    if (!SAME_OBJ(((Scheme_App3_Rec *)a)->rator, scheme_list_proc))
      return 0;
    count = 2;
  } else
    return 0;

  for (i = 0; i < count; i++) {
    if (SAME_TYPE(SCHEME_TYPE(a), scheme_application_type))
      arg = ((Scheme_App_Rec *)a)->args[i+1];
    else if (SAME_TYPE(SCHEME_TYPE(a), scheme_application2_type))
      arg = ((Scheme_App2_Rec *)a)->rand;
    else {
      if (i == 0)
        arg = ((Scheme_App3_Rec *)a)->rand1;
      else
        arg = ((Scheme_App3_Rec *)a)->rand2;
    }

    if (SAME_TYPE(SCHEME_TYPE(arg), scheme_application3_type)) {
      Scheme_App3_Rec *a3 = (Scheme_App3_Rec *)arg;

      if (!SAME_OBJ(a3->rator, scheme_cons_proc)) {
        if (!just_for_authentic)
          return 0;
      } else {
        if (_authentic && SAME_OBJ(a3->rand1, scheme_authentic_property))
          *_authentic = 1;
        if (!just_for_authentic) {
          if (is_struct_type_property_without_guard(a3->rand1,
                                                    top_level_consts,
                                                    inline_variants, top_level_table,
                                                    runstack, rs_delta,
                                                    symbols, symbol_table)) {
            if (!scheme_omittable_expr(a3->rand2, 1, 3, (resolved ? OMITTABLE_RESOLVED : 0), NULL, NULL))
              return 0;
          } else
            return 0;
        }
      }
    } else {
      if (!just_for_authentic)
        return 0;
    }
  }
  
  return 1;
}

Scheme_Object *scheme_is_simple_make_struct_type(Scheme_Object *e, int vals,  int flags, 
                                                 GC_CAN_IGNORE int *_auto_e_depth, 
                                                 Simple_Struct_Type_Info *_stinfo,
                                                 Scheme_Object **_parent_identity,
                                                 Scheme_Hash_Table *top_level_consts, 
                                                 Scheme_Hash_Table *inline_variants, 
                                                 Scheme_Hash_Table *top_level_table,
                                                 Scheme_Object **runstack, int rs_delta,
                                                 Scheme_Object **symbols, Scheme_Hash_Table *symbol_table,
                                                 Scheme_Object **_name,
                                                 int fuel)
/* Checks whether it's a `make-struct-type' call --- that, if `flags` includes
   `CHECK_STRUCT_TYPE_ALWAYS_SUCCEED`, certainly succeeds (i.e., no exception) ---
   pending a check of the auto-value argument if `flags` includes `CHECK_STRUCT_TYPE_DELAY_AUTO_CHECK`.
   The expression itself must have no side-effects except for errors (but the possibility
   of errors means that the expression is not necessarily omittable).
   The resulting *constructor* must always succeed (i.e., no guards).
   The result is the auto-value argument or scheme_true if it's simple, NULL if not. 
   The first result of `e` will be a struct type, the second a constructor, and the third a predicate;
   the rest are selectors and mutators. */
{
  int resolved = (flags & CHECK_STRUCT_TYPE_RESOLVED);
  
  if (!fuel) return NULL;

  if (SAME_TYPE(SCHEME_TYPE(e), scheme_application_type)) {
    if ((vals == 5) || (vals < 0)) {
      Scheme_App_Rec *app = (Scheme_App_Rec *)e;

      if ((app->num_args >= 4) && (app->num_args <= 11)
          && SAME_OBJ(scheme_make_struct_type_proc, app->args[0])) {
        int super_count_plus_one;

        if (_parent_identity)
          *_parent_identity = scheme_null;
        if (!SCHEME_FALSEP(app->args[2]))
          super_count_plus_one = is_constant_super(app->args[2], 
                                                   top_level_consts, inline_variants, top_level_table, runstack,
                                                   rs_delta + app->num_args,
                                                   symbols, symbol_table, _parent_identity);
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
                || (flags & CHECK_STRUCT_TYPE_DELAY_AUTO_CHECK)
                || scheme_omittable_expr(app->args[5], 1, 3, (resolved ? OMITTABLE_RESOLVED : 0), NULL, NULL))
            && ((app->num_args < 6)
                /* no properties... */
                || SCHEME_NULLP(app->args[6])
                /* ... or properties that might make the `make-struct-type`
                   call itself fail, but otherwise don't affect the constructor
                   or selectors in a way that matters (although supplying the
                   `prop:chaperone-unsafe-undefined` property can affect the
                   constructor in an optimizer-irrelevant way) */
                || (!(flags & CHECK_STRUCT_TYPE_ALWAYS_SUCCEED)
                    && scheme_omittable_expr(app->args[6], 1, 4, (resolved ? OMITTABLE_RESOLVED : 0), NULL, NULL))
                || ((flags & CHECK_STRUCT_TYPE_ALWAYS_SUCCEED)
                    && is_simple_property_list(app->args[6], resolved,
                                               top_level_consts, inline_variants,
                                               top_level_table,
                                               runstack, rs_delta,
                                               symbols, symbol_table,
                                               0, NULL)))
            && ((app->num_args < 7)
                /* inspector: */
                || SCHEME_FALSEP(app->args[7])
                || (SCHEME_SYMBOLP(app->args[7])
                    && !strcmp("prefab", SCHEME_SYM_VAL(app->args[7]))
                    && !SCHEME_SYM_WEIRDP(app->args[7]))
                || is_inspector_call(app->args[7]))
            && ((app->num_args < 8)
                /* procedure property: */
                || SCHEME_FALSEP(app->args[8])
                || is_proc_spec_proc(app->args[8], SCHEME_INT_VAL(app->args[3])))
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
          if (_name)
            *_name = app->args[1];
          if (_stinfo) {
            int authentic = 0;
            int super_count = (super_count_plus_one 
                               ? (super_count_plus_one - 1)
                               : 0);
            _stinfo->init_field_count = SCHEME_INT_VAL(app->args[3]) + super_count;
            _stinfo->field_count = (SCHEME_INT_VAL(app->args[3]) 
                                    + SCHEME_INT_VAL(app->args[4])
                                    + super_count);
            _stinfo->uses_super = (super_count_plus_one ? 1 : 0);
            _stinfo->super_field_count = (super_count_plus_one ? (super_count_plus_one - 1) : 0);
            _stinfo->normal_ops = 1;
            _stinfo->indexed_ops = 0;
            _stinfo->authentic = 0;
            if ((app->num_args > 6)
                && is_simple_property_list(app->args[6], resolved,
                                           top_level_consts, inline_variants,
                                           top_level_table,
                                           runstack, rs_delta,
                                           symbols, symbol_table,
                                           1, &authentic))
              _stinfo->authentic = authentic;
            _stinfo->num_gets = 1;
            _stinfo->num_sets = 1;
          }
          return ((app->num_args < 5) ? scheme_true : app->args[5]);
        }
      }
    }
  }

  if (SAME_TYPE(SCHEME_TYPE(e), scheme_ir_let_header_type)) {
    /* check for (let-values ([(: mk ? ref- set-!) (make-struct-type ...)]) (values ...))
       as generated by the expansion of `struct' */
    Scheme_IR_Let_Header *lh = (Scheme_IR_Let_Header *)e;
    if ((lh->count == 5) && (lh->num_clauses == 1)) {
      if (SAME_TYPE(SCHEME_TYPE(lh->body), scheme_ir_let_value_type)) {
        Scheme_IR_Let_Value *lv = (Scheme_IR_Let_Value *)lh->body;
        if (SAME_TYPE(SCHEME_TYPE(lv->value), scheme_application_type)) {
          Scheme_Object *auto_e;
          Simple_Struct_Type_Info stinfo;
          if (!_stinfo) _stinfo = &stinfo;
          auto_e = scheme_is_simple_make_struct_type(lv->value, 5, flags, 
                                                     _auto_e_depth, _stinfo, _parent_identity,
                                                     top_level_consts, inline_variants, top_level_table, 
                                                     runstack, rs_delta,
                                                     symbols, symbol_table,
                                                     _name,
                                                     fuel-1);
          if (auto_e) {
            /* We have (let-values ([... (make-struct-type)]) ....), so make sure body
               just uses `make-struct-field-{accessor,mutator}'. */
            if (is_values_with_accessors_and_mutators(lv->body, vals, resolved, _stinfo, lv->vars)) {
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
            Simple_Struct_Type_Info stinfo;
            if (!_stinfo) _stinfo = &stinfo;
            auto_e = scheme_is_simple_make_struct_type(e2, 5, flags,
                                                       _auto_e_depth, _stinfo, _parent_identity,
                                                       top_level_consts, inline_variants, top_level_table,
                                                       runstack, rs_delta + lvd->count,
                                                       symbols, symbol_table,
                                                       _name,
                                                       fuel-1);
            if (auto_e) {
              /* We have (let-values ([... (make-struct-type)]) ....), so make sure body
                 just uses `make-struct-field-{accessor,mutator}'. */
              e2 = skip_clears(lv->body);
              if (is_values_with_accessors_and_mutators(e2, vals, resolved, _stinfo, NULL)) {
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

int scheme_is_simple_make_struct_type_property(Scheme_Object *e, int vals, int flags,
                                               int *_has_guard,
                                               Scheme_Hash_Table *top_level_consts, 
                                               Scheme_Hash_Table *inline_variants,
                                               Scheme_Hash_Table *top_level_table,
                                               Scheme_Object **runstack, int rs_delta,
                                               Scheme_Object **symbols, Scheme_Hash_Table *symbol_table,
                                               int fuel)
/* Reports whether `app` is a call to `make-struct-type-property` to
   produce a propert with no guard. */
{
  int resolved = (flags & CHECK_STRUCT_TYPE_RESOLVED);

  if ((vals != 3) && (vals >= 0)) return 0;

  if (SAME_TYPE(SCHEME_TYPE(e), scheme_application2_type)) {
    Scheme_App2_Rec *app = (Scheme_App2_Rec *)e;
    if (SAME_OBJ(app->rator, scheme_make_struct_type_property_proc)) {
      if (SCHEME_SYMBOLP(app->rand)) {
        if (_has_guard) *_has_guard = 0;
        return 1;
      }
    }
  }
  
  if (SAME_TYPE(SCHEME_TYPE(e), scheme_application3_type)) {
    Scheme_App3_Rec *app = (Scheme_App3_Rec *)e;
    if (SAME_OBJ(app->rator, scheme_make_struct_type_property_proc)) {
      if (SCHEME_SYMBOLP(app->rand1)
          && (!(flags & CHECK_STRUCT_TYPE_ALWAYS_SUCCEED)
              || SCHEME_LAMBDAP(app->rand2))
          && (scheme_omittable_expr(app->rator, 1, 4, (resolved ? OMITTABLE_RESOLVED : 0), NULL, NULL))) {
        if (_has_guard) *_has_guard = 1;
        return 1;
      }
    }
  }
  
  return 0;
}

/*========================================================================*/
/*                             more utils                                 */
/*========================================================================*/

intptr_t scheme_get_struct_proc_shape(int k, Simple_Struct_Type_Info *stinfo)
{
  switch (k) {
  case 0:
    if (stinfo->field_count == stinfo->init_field_count)
      return (STRUCT_PROC_SHAPE_STRUCT
              | (stinfo->authentic ? STRUCT_PROC_SHAPE_AUTHENTIC : 0)
              | (stinfo->field_count << STRUCT_PROC_SHAPE_SHIFT));
    else
      return STRUCT_PROC_SHAPE_OTHER;
    break;
  case 1:
    return STRUCT_PROC_SHAPE_CONSTR | (stinfo->init_field_count << STRUCT_PROC_SHAPE_SHIFT);
    break;
  case 2:
    return (STRUCT_PROC_SHAPE_PRED
            | (stinfo->authentic ? STRUCT_PROC_SHAPE_AUTHENTIC : 0));
    break;
  default:
    if (stinfo && stinfo->normal_ops && stinfo->indexed_ops) {
      if (k - 3 < stinfo->num_gets) {
        /* record index of field */
        return (STRUCT_PROC_SHAPE_GETTER
                | (stinfo->authentic ? STRUCT_PROC_SHAPE_AUTHENTIC : 0)
                | ((stinfo->super_field_count + (k - 3)) << STRUCT_PROC_SHAPE_SHIFT));
      } else
        return (STRUCT_PROC_SHAPE_SETTER
                | (stinfo->authentic ? STRUCT_PROC_SHAPE_AUTHENTIC : 0)
                | (stinfo->field_count << STRUCT_PROC_SHAPE_SHIFT));
    }
  }

  return STRUCT_PROC_SHAPE_OTHER;
}

Scheme_Object *scheme_make_struct_proc_shape(intptr_t k, Scheme_Object *identity)
{
  Scheme_Object *ps;

  ps = scheme_malloc_small_tagged(sizeof(Scheme_Simple_Object));
  ps->type = scheme_struct_proc_shape_type;
  SCHEME_PROC_SHAPE_MODE(ps) = k;
  SCHEME_PROC_SHAPE_IDENTITY(ps) = identity;

  return ps;
}

intptr_t scheme_get_struct_property_proc_shape(int k, int has_guard)
{
  switch (k) {
  case 0:
    if (has_guard)
      return STRUCT_PROP_PROC_SHAPE_GUARDED_PROP;
    else
      return STRUCT_PROP_PROC_SHAPE_PROP;
  case 1:
    return STRUCT_PROP_PROC_SHAPE_PRED;
  case 2:
  default:
    return STRUCT_PROP_PROC_SHAPE_GETTER;
  }
}

Scheme_Object *scheme_make_struct_property_proc_shape(intptr_t k)
{
  Scheme_Object *ps;

  ps = scheme_alloc_small_object();
  ps->type = scheme_struct_prop_proc_shape_type;
  SCHEME_PROP_PROC_SHAPE_MODE(ps) = k;

  return ps;
}

XFORM_NONGCING static int is_struct_identity_subtype(Scheme_Object *sub, Scheme_Object *sup)
{
  /* A structure identity is a list of symbols, but the symbols are
     just for debugging. Instead, the address of each pair forming the
     list represents an identiity. */
  while (SCHEME_PAIRP(sub)) {
    if (SAME_OBJ(sub, sup))
      return 1;
    sub = SCHEME_CDR(sub);
  }
  return 0;
}

static int single_valued_noncm_function(Scheme_Object *rator, int num_args,
                                        Optimize_Info *info, int s_v, int non_cm)
{
  int flags;
  
  if (!s_v && !non_cm)
    return 1;

  flags = get_rator_flags(rator, num_args, info);
  if (s_v && !(flags & LAMBDA_SINGLE_RESULT))
    return 0;
  if (non_cm && !(flags & LAMBDA_PRESERVES_MARKS))
    return 0;

  return 1;
}

static int do_single_valued_noncm_expression(Scheme_Object *expr, Optimize_Info *info, int fuel, int s_v, int non_cm)
/* Not necessarily omittable or copyable expression.
   If `s_v`, the expression must not be single-valued.
   If `non_cm`, the expression must be not sensitive to tail position. In particular,
   it has no with-continuation-mark in tail position, unless the body is omittable.
   The conservative answer is 0. */
{
  if (!s_v && !non_cm)
    return 1;

  while (fuel) {
    switch (SCHEME_TYPE(expr)) {
    case scheme_ir_local_type:
    case scheme_local_type:
    case scheme_local_unbox_type:
    case scheme_ir_toplevel_type:
      return 1;
      break;
    case scheme_application_type:
      {
        Scheme_App_Rec *app = (Scheme_App_Rec *)expr;
        return single_valued_noncm_function(app->args[0], app->num_args, info, s_v, non_cm);
      }
      break;
    case scheme_application2_type:
      {
        Scheme_App2_Rec *app = (Scheme_App2_Rec *)expr;
        return single_valued_noncm_function(app->rator, 1, info, s_v, non_cm);
      }
      break;
    case scheme_application3_type:
      {
        Scheme_App3_Rec *app = (Scheme_App3_Rec *)expr;
        return single_valued_noncm_function(app->rator, 2, info, s_v, non_cm);
      }
      break;
    case scheme_branch_type:
      {
        Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)expr;
        return (do_single_valued_noncm_expression(b->tbranch, info, fuel - 1, s_v, non_cm)
                && do_single_valued_noncm_expression(b->fbranch, info, fuel - 1, s_v, non_cm));
      }
      break;
    case scheme_ir_let_header_type:
      {
        Scheme_IR_Let_Header *hl = (Scheme_IR_Let_Header *)expr;
        expr = hl->body;
      }
      break;
    case scheme_ir_let_value_type:
      {
        Scheme_IR_Let_Value *lv = (Scheme_IR_Let_Value *)expr;
        expr = lv->body;
      }
      break;
    case scheme_sequence_type:
      {
        Scheme_Sequence *seq = (Scheme_Sequence *)expr;
        expr = seq->array[seq->count-1];
      }
      break;
    case scheme_begin0_sequence_type:
      {
         Scheme_Sequence *seq = (Scheme_Sequence *)expr;
      expr = seq->array[0];
      }
      break;
    case scheme_with_cont_mark_type:
      {
        Scheme_With_Continuation_Mark * wcm = (Scheme_With_Continuation_Mark *)expr;
        if (non_cm) {
          /* To avoid being sensitive to tail position, the body must not inspect
             the continuation at all. */
          return scheme_omittable_expr(wcm->body, s_v ? 1 : -1, 5, 0, NULL, NULL);
        } else {
          expr = wcm->body;
        }
      }
      break;
    case scheme_ir_lambda_type:
    case scheme_case_lambda_sequence_type:
    case scheme_set_bang_type:
      return 1;
      break;
    default:
      if (SCHEME_TYPE(expr) > _scheme_ir_values_types_)
        return 1;
      break;
    }
    fuel--;
  }

  return 0;
}

static int single_valued_noncm_expression(Scheme_Object *expr, Optimize_Info *info, int fuel)
{
  return do_single_valued_noncm_expression(expr, info, fuel, 1, 1);
}

static int single_valued_expression(Scheme_Object *expr, Optimize_Info *info, int fuel)
{
  return do_single_valued_noncm_expression(expr, info, fuel, 1, 0);
}

static int noncm_expression(Scheme_Object *expr, Optimize_Info *info, int fuel)
{
  return do_single_valued_noncm_expression(expr, info, fuel, 0, 1);
}

static int is_movable_prim(Scheme_Object *rator, int n, int cross_lambda, int cross_k, Optimize_Info *info)
/* Can we move a call to `rator` relative to other function calls?
   A -1 return means that the arguments must be movable without
   changing space complexity (which is the case for `cons`, for example). */
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

static int movable_expression(Scheme_Object *expr, Optimize_Info *info,
                              int cross_lambda, int cross_k, int cross_s,
                              int check_space, int fuel)
/* A movable expression can't necessarily be constant-folded,
   but can be delayed because it has no side-effects (or is unsafe),
   produces a single value,
   and is not sensitive to being in tail position */
{
  int can_move;

  if (fuel < 0) return 0;

  switch (SCHEME_TYPE(expr)) {
  case scheme_toplevel_type:
    return ((SCHEME_TOPLEVEL_FLAGS(expr) & SCHEME_TOPLEVEL_FLAGS_MASK) >= SCHEME_TOPLEVEL_FIXED);
  case scheme_ir_quote_syntax_type:
    return 1;
  case scheme_ir_local_type:
    {
      /* Ok if not mutable */
      if (!SCHEME_VAR(expr)->mutated) {
        if (check_space) {
          if (SCHEME_VAR(expr)->val_type)
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
    if (!cross_lambda
        && !cross_k
        && (SCHEME_APPN_FLAGS((Scheme_App_Rec *)expr) & APPN_FLAG_OMITTABLE))
      can_move = -1;
    else
      can_move = is_movable_prim(((Scheme_App_Rec *)expr)->args[0], ((Scheme_App_Rec *)expr)->num_args,
                                 cross_lambda, cross_k, info);
    if (can_move) {
      int i;
      for (i = ((Scheme_App_Rec *)expr)->num_args; i--; ) {
        if (!movable_expression(((Scheme_App_Rec *)expr)->args[i+1], info,
                                cross_lambda, cross_k, cross_s,
                                check_space || (cross_s && (can_move < 0)), fuel - 1))
          return 0;
      }
      return 1;
    }
    break;
  case scheme_application2_type:
    if (!cross_lambda
        && !cross_k
        && (SCHEME_APPN_FLAGS((Scheme_App2_Rec *)expr) & APPN_FLAG_OMITTABLE))
      can_move = -1;
    else
      can_move = is_movable_prim(((Scheme_App2_Rec *)expr)->rator, 1, cross_lambda, cross_k, info);
    if (can_move) {
      if (movable_expression(((Scheme_App2_Rec *)expr)->rand, info,
                             cross_lambda, cross_k, cross_s,
                             check_space || (cross_s && (can_move < 0)), fuel - 1))
        return 1;
    }
    break;
  case scheme_application3_type:
    if (!cross_lambda
        && !cross_k
        && (SCHEME_APPN_FLAGS((Scheme_App3_Rec *)expr) & APPN_FLAG_OMITTABLE))
      can_move = -1;
    else
      can_move = is_movable_prim(((Scheme_App3_Rec *)expr)->rator, 2, cross_lambda, cross_k, info);
    if (can_move) {
      if (movable_expression(((Scheme_App3_Rec *)expr)->rand1, info,
                             cross_lambda, cross_k, cross_s,
                             check_space || (cross_s && (can_move < 0)), fuel - 1)
          && movable_expression(((Scheme_App3_Rec *)expr)->rand2, info,
                                cross_lambda, cross_k, cross_s,
                                check_space || (cross_s && (can_move < 0)), fuel - 1))
        return 1;
    }
    break;
  case scheme_branch_type:
    {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)expr;
      if (movable_expression(b->test, info, cross_lambda, cross_k, cross_s, check_space, fuel-1)
          /* Check space for branches if cross_s, because evaluating `if` eliminates one of them */
          && movable_expression(b->tbranch, info, cross_lambda, cross_k, cross_s, check_space || cross_s, fuel-1)
          && movable_expression(b->fbranch, info, cross_lambda, cross_k, cross_s, check_space || cross_s, fuel-1))
        return 1;
    }
    break;
  case scheme_ir_lambda_type:
  case scheme_case_lambda_sequence_type:
    /* Can't move across lambda or continuation if not closed, since
       that changes allocation of a closure (i.e., might allocate the
       closure multiple times). */
    return !cross_lambda && !cross_k;
  default:
    if (SCHEME_TYPE(expr) > _scheme_ir_values_types_)
      return 1;
  }

  return 0;
}

int scheme_is_ir_lambda(Scheme_Object *o, int can_be_closed, int can_be_liftable)
{
  if (SAME_TYPE(SCHEME_TYPE(o), scheme_ir_lambda_type)) {
    if (!can_be_closed || !can_be_liftable) {
      Scheme_Lambda *lam;
      lam = (Scheme_Lambda *)o;
      /* Because == 0 is like a constant */
      if (!can_be_closed && !lam->closure_size)
        return 0;
      /* Because procs that reference only globals are lifted: */
      if (!can_be_liftable && (lam->closure_size == 1) && lambda_has_top_level(lam))
        return 0;
    }
    return 1;
  } else
    return 0;
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

int scheme_ir_duplicate_ok(Scheme_Object *fb, int cross_module)
/* Is the constant a value that we can "copy" in the code? */
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
          || (!cross_module && SAME_TYPE(SCHEME_TYPE(fb), scheme_ir_toplevel_type))
          || (!cross_module && SAME_TYPE(SCHEME_TYPE(fb), scheme_ir_local_type))
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
              && (!cross_module || small_inline_number(fb)))
          || SAME_TYPE(SCHEME_TYPE(fb), scheme_ctype_type));
}

/*========================================================================*/
/*                   applications, branches, sequences                    */
/*========================================================================*/

static Scheme_Object *finish_optimize_application(Scheme_App_Rec *app, Optimize_Info *info, int context);
static Scheme_Object *finish_optimize_application2(Scheme_App2_Rec *app, Optimize_Info *info, int context);
static Scheme_Object *finish_optimize_application3(Scheme_App3_Rec *app, Optimize_Info *info, int context);

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
  case scheme_ir_local_type:
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
  case scheme_ir_let_header_type:
    {
      Scheme_IR_Let_Header *head = (Scheme_IR_Let_Header *)expr;
      Scheme_Object *body;
      Scheme_IR_Let_Value *lv;
      int i;

      body = head->body;
      for (i = head->num_clauses; i--; ) {
	lv = (Scheme_IR_Let_Value *)body;
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
  case scheme_ir_lambda_type:
    {
      sz = estimate_expr_size(((Scheme_Lambda *)expr)->body, sz, fuel - 1);
      sz++;
      break;
    }
  case scheme_ir_toplevel_type:
  case scheme_ir_quote_syntax_type:
    /* FIXME: other syntax types not covered */
  default:
    sz += 1;
    break;
  }

  return sz;
}

static Scheme_Object *estimate_closure_size(Scheme_Object *e)
{
  Scheme_Object *wbl;
  int sz;
  sz = estimate_expr_size(e, 0, 32);

  wbl = scheme_alloc_object();
  wbl->type = scheme_will_be_lambda_type;
  SCHEME_WILL_BE_LAMBDA_SIZE(wbl) = sz;
  SCHEME_WILL_BE_LAMBDA(wbl) = e;
  
  return wbl;
}

static Scheme_Object *no_potential_size(Scheme_Object *v)
{
  if (v && SCHEME_WILL_BE_LAMBDAP(v))
    return NULL;
  else
    return v;
}

static Scheme_Object *apply_inlined(Scheme_Lambda *lam, Optimize_Info *info,
				    int argc, Scheme_App_Rec *app, Scheme_App2_Rec *app2, Scheme_App3_Rec *app3,
                                    int context, Scheme_Object *orig, Scheme_Object *le_prev,
                                    int single_use)
/* Optimize the body of `lam` given the known arguments in `app`, `app2`, or `app3` */
{
  Scheme_IR_Let_Header *lh;
  Scheme_IR_Let_Value *lv, *prev = NULL;
  Scheme_Object *val;
  int i, expected;
  Optimize_Info *sub_info;
  Scheme_IR_Local **vars;
  Scheme_Object *p = lam->body;

  expected = lam->num_params;

  if (!expected) {
    /* No arguments, so no need for a `let` wrapper: */
    sub_info = optimize_info_add_frame(info, 0, 0, 0);
    if (!single_use || lam->ir_info->is_dup)
      sub_info->inline_fuel >>= 1;
    p = scheme_optimize_expr(p, sub_info, context);
    info->single_result = sub_info->single_result;
    info->preserves_marks = sub_info->preserves_marks;
    optimize_info_done(sub_info, NULL);
    merge_types(sub_info, info, NULL);

    return replace_tail_inside(p, le_prev, orig);
  }

  lh = MALLOC_ONE_TAGGED(Scheme_IR_Let_Header);
  lh->iso.so.type = scheme_ir_let_header_type;
  lh->count = expected;
  lh->num_clauses = expected;

  for (i = 0; i < expected; i++) {
    lv = MALLOC_ONE_TAGGED(Scheme_IR_Let_Value);
    lv->iso.so.type = scheme_ir_let_value_type;
    lv->count = 1;

    vars = MALLOC_N(Scheme_IR_Local*, 1);
    vars[0] = lam->ir_info->vars[i];
    lv->vars = vars;

    if ((i == expected - 1)
        && (SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_HAS_REST)) {
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

        l = scheme_make_pair(val, l);
      }
      l = scheme_make_pair(scheme_list_proc, l);
      val = scheme_make_application(l, info);
    } else if (app)
      val = app->args[i + 1];
    else if (app3)
      val = (i ? app3->rand2 : app3->rand1);
    else
      val = app2->rand;

    lv->value = val;

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
  if (!single_use || lam->ir_info->is_dup)
    sub_info->inline_fuel >>= 1;

  p = optimize_lets((Scheme_Object *)lh, sub_info, context);

  info->single_result = sub_info->single_result;
  info->preserves_marks = sub_info->preserves_marks;
  optimize_info_done(sub_info, NULL);
  merge_types(sub_info, info, NULL);

  return replace_tail_inside(p, le_prev, orig);
}

int scheme_check_leaf_rator(Scheme_Object *le)
{
  if (le && SCHEME_PRIMP(le)) {
    int opt;
    opt = ((Scheme_Prim_Proc_Header *)le)->flags & SCHEME_PRIM_OPT_MASK;
    if (opt >= SCHEME_PRIM_OPT_IMMEDIATE)
      return 1;
  }
  return 0;
}

static int get_rator_flags(Scheme_Object *rator, int num_args, Optimize_Info *info)
{
  rator = lookup_constant_proc(info, rator, num_args);
  if (!rator) {
    return 0;
  } else if (SAME_OBJ(rator, scheme_true)) {
    /* wrong arity */
    return (LAMBDA_PRESERVES_MARKS | LAMBDA_SINGLE_RESULT);
  } else if (SAME_TYPE(SCHEME_TYPE(rator), scheme_struct_proc_shape_type)) {
    return (LAMBDA_PRESERVES_MARKS | LAMBDA_SINGLE_RESULT);
  } else if (SAME_TYPE(SCHEME_TYPE(rator), scheme_struct_prop_proc_shape_type)) {
    switch (SCHEME_PROP_PROC_SHAPE_MODE(rator)) {
    case STRUCT_PROP_PROC_SHAPE_PRED:
      return (LAMBDA_PRESERVES_MARKS | LAMBDA_SINGLE_RESULT);
    case STRUCT_PROP_PROC_SHAPE_GETTER:
      if (num_args == 1)
        return (LAMBDA_PRESERVES_MARKS | LAMBDA_SINGLE_RESULT);
    }
  } else if (SCHEME_PRIMP(rator)) {
    int opt;
    /* special cases for values */
    if (SAME_OBJ(rator, scheme_values_proc) && num_args == 1) {
      return (LAMBDA_PRESERVES_MARKS | LAMBDA_SINGLE_RESULT);
    }
    if (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & SCHEME_PRIM_ALWAYS_ESCAPES) {
      return (LAMBDA_PRESERVES_MARKS | LAMBDA_SINGLE_RESULT);
    }
    opt = ((Scheme_Prim_Proc_Header *)rator)->flags & SCHEME_PRIM_OPT_MASK;
    if (opt >= SCHEME_PRIM_OPT_NONCM) {
      return (LAMBDA_PRESERVES_MARKS | LAMBDA_SINGLE_RESULT);
    }
  } else if (SAME_TYPE(SCHEME_TYPE(rator), scheme_ir_lambda_type)) {
    Scheme_Lambda *lam = (Scheme_Lambda *)rator;
    return SCHEME_LAMBDA_FLAGS(lam);
  }
  return 0;
}

int check_single_use(Scheme_Object *var)
{
   Scheme_IR_Local *v = SCHEME_VAR(var);

  return ((v->use_count == 1)
          /* If we're outside the binding, then the binding
             itself will remain as a used: */
          && !v->optimize_outside_binding
          /* To help avoid infinite unrolling,
             don't count a self use as "single" use. */
          && !v->optimize_unready);
}

int check_potential_size(Scheme_Object *var)
{
  Scheme_Object* n;

  n = SCHEME_VAR(var)->optimize.known_val;
  if (n && SCHEME_WILL_BE_LAMBDAP(n)) {
    return SCHEME_WILL_BE_LAMBDA_SIZE(n);
  }

  return 0;
}

Scheme_Object *do_lookup_constant_proc(Optimize_Info *info, Scheme_Object *le,
                                       int argc, int for_inline, int *_single_use)
/* Return a known procedure, if any.
   When argc == -1 it may return a case-lambda. Else, it will check the arity
   and split a case-lambda to extact the relevant lambda. If the arity is
   wrong the result is scheme_true.
   If for_inline, it may return a potential size. Else, it will go inside
   potential sizes, noinline procedures, lets, begins and other construction,
   so the result can't be inlined and must be used only to get the properties
   of the actual procedure. It may also return a struct_(prop_)proc_shape.*/
    
{
  Scheme_Object *prev = NULL;
  
  *_single_use = 0;

  /* Move inside `let' bindings to get the inner procedure */
  if (!for_inline)
    extract_tail_inside(&le, &prev);

  le = extract_specialized_proc(le, le);

  if (SCHEME_LAMBDAP(le)) {
    /* Found a `((lambda' */
    *_single_use = 1;
  }

  if (SAME_TYPE(SCHEME_TYPE(le), scheme_ir_local_type)) {
    int tmp;
    tmp = check_single_use(le);
    *_single_use = tmp;
    if ((SCHEME_VAR(le)->mode != SCHEME_VAR_MODE_OPTIMIZE)) {
      /* We got a local that is bound in a let that is not yet optimized. */
      return NULL;
    }
    le = SCHEME_VAR(le)->optimize.known_val;
    if (!le)
      return NULL;
  }

  while (SAME_TYPE(SCHEME_TYPE(le), scheme_ir_toplevel_type)) {
    int pos;
    pos = SCHEME_TOPLEVEL_POS(le);
    *_single_use = 0;
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
        if (SAME_TYPE(SCHEME_TYPE(iv), scheme_vector_type)) { /* inline variant + shift info */
          int has_cases = 0;
          Scheme_Object *orig_iv = iv;
          MZ_ASSERT(SAME_TYPE(scheme_inline_variant_type, SCHEME_TYPE(SCHEME_VEC_ELS(iv)[0])));
          /* unresolving may add new top-levels to `info->cp`: */
          iv = scheme_unresolve(SCHEME_VEC_ELS(iv)[0], argc, &has_cases,
                                info->cp, info->env, info->insp, SCHEME_INT_VAL(SCHEME_VEC_ELS(iv)[3]),
                                SCHEME_VEC_ELS(iv)[1], SCHEME_VEC_ELS(iv)[2]);
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
    if (info->top_level_consts) {
      le = scheme_hash_get(info->top_level_consts, scheme_make_integer(pos));
      if (!le)
        return NULL;
    } else
      break;
  }

  if (SCHEME_WILL_BE_LAMBDAP(le)) {
    if (for_inline)
      return le;
    else
      le = SCHEME_WILL_BE_LAMBDA(le);
  }

  if (!for_inline && SAME_TYPE(scheme_noninline_proc_type, SCHEME_TYPE(le))) {
    le = SCHEME_BOX_VAL(le);
  }

  if (SAME_TYPE(SCHEME_TYPE(le), scheme_struct_proc_shape_type)) {
    int ok_arity;
    switch (SCHEME_PROC_SHAPE_MODE(le) & STRUCT_PROC_SHAPE_MASK) {
    case STRUCT_PROC_SHAPE_CONSTR:
      ok_arity = (argc == (SCHEME_PROC_SHAPE_MODE(le) >> STRUCT_PROC_SHAPE_SHIFT));
      break;
    case STRUCT_PROC_SHAPE_PRED:
      ok_arity = (argc == 1);
      break;
    case STRUCT_PROC_SHAPE_GETTER:
      ok_arity = (argc == 1);
      break;
    case STRUCT_PROC_SHAPE_SETTER:
      ok_arity = (argc == 2);
      break;
    default:
      return NULL;
    }
    if (ok_arity || (argc == -1)) {
      return for_inline ? NULL : le;
    } else {
      return scheme_true;
    }
  }

  if (SAME_TYPE(SCHEME_TYPE(le), scheme_struct_prop_proc_shape_type)) {
    int ok_arity;
    switch (SCHEME_PROP_PROC_SHAPE_MODE(le)) {
    case STRUCT_PROP_PROC_SHAPE_PRED:
      ok_arity = (argc == 1);
      break;
    case STRUCT_PROP_PROC_SHAPE_GETTER:
      ok_arity = (argc == 1) || (argc == 2);
      break;
    default:
      return NULL;
    }
    if (ok_arity || (argc == -1)) {
      return for_inline ? NULL : le;
    } else {
      return scheme_true;
    }
  }

  if (SAME_TYPE(SCHEME_TYPE(le), scheme_case_lambda_sequence_type)) {
    Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)le;
    Scheme_Object *cp;
    int i, count;

    if (argc == -1)
      return le;

    count = cl->count;
    for (i = 0; i < count; i++) {
      cp = cl->array[i];
      if (SAME_TYPE(SCHEME_TYPE(cp), scheme_ir_lambda_type)) {
        Scheme_Lambda *lam = (Scheme_Lambda *)cp;
        if ((lam->num_params == argc)
            || ((SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_HAS_REST)
                && (argc + 1 >= lam->num_params))) {
          return cp;
        }
      } else {
        scheme_signal_error("internal error: strange case-lambda");
      }
    }
    if (i >= count) {
      return scheme_true;
    }
  }

  if (SAME_TYPE(SCHEME_TYPE(le), scheme_ir_lambda_type)) {
    Scheme_Lambda *lam = (Scheme_Lambda *)le;

    if (argc == -1)
      return le;

    if ((lam->num_params == argc)
        || ((SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_HAS_REST)
            && (argc + 1 >= lam->num_params))) {
      return le;
    } else {
      return scheme_true;
    }
  }

  if (SCHEME_PROCP(le)) {
    Scheme_Object *a[1];

    if (argc == -1)
      return le;

    a[0] = le;
    if (scheme_check_proc_arity(NULL, argc, 0, 1, a))
      return le;
    else 
      return scheme_true;
  }

  return NULL;
}

Scheme_Object *lookup_constant_proc(Optimize_Info *info, Scheme_Object *le, int argc)
{
  int single_use = 0;
  return do_lookup_constant_proc(info, le, argc, 0, &single_use);
}

#if 0
# define LOG_INLINE(x) x
#else
# define LOG_INLINE(x) /*empty*/
#endif

Scheme_Object *optimize_for_inline(Optimize_Info *info, Scheme_Object *le, int argc,
                                   Scheme_App_Rec *app, Scheme_App2_Rec *app2, Scheme_App3_Rec *app3,
                                   int context, int optimized_rator)
/* One of app, app2 and app3 should be non-NULL.
   If app, we're inlining a general application. If app2, we're inlining an
   application with a single argument and if app3, we're inlining an
   application with two arguments. */
{
  int single_use = 0, psize = 0;
  Scheme_Object *prev = NULL, *orig_le = le, *le2;
  int already_opt = optimized_rator;

  if ((info->inline_fuel < 0) && info->has_nonleaf)
    return NULL;

  /* Move inside `let' bindings, so we can convert ((let (....) proc) arg ...)
     to (let (....) (proc arg ...)) */
  if (already_opt)
    extract_tail_inside(&le, &prev);

  le = extract_specialized_proc(le, le);
  
  if (!already_opt 
      && SCHEME_LAMBDAP(le)) {
    /* We have an immediate `lambda' that wasn't optimized, yet.
       Go optimize it, first. */
    return NULL;
  }

  le2 = le;
  le = do_lookup_constant_proc(info, le, argc, 1, &single_use);
  
  if (!le) {
    info->has_nonleaf = 1;
    return NULL;
  }

  if (SCHEME_WILL_BE_LAMBDAP(le)) {
    psize = SCHEME_WILL_BE_LAMBDA_SIZE(le);
    LOG_INLINE(fprintf(stderr, "Potential inline %d %d\n", psize, info->inline_fuel * (argc + 2)));
    /* If we inline, the enclosing function will get larger, so we increase
       its potential size. */
    if (psize <= (info->inline_fuel * (argc + 2)))
      info->psize += psize;
    info->has_nonleaf = 1;
    return NULL;
  }

  if (SAME_OBJ(le, scheme_true)) {
    /* wrong arity */
    int len;
    const char *pname = NULL, *context;
    info->escapes = 1;
    le2 = lookup_constant_proc(info, le2, -1);
    if (!SAME_TYPE(SCHEME_TYPE(le2), scheme_struct_proc_shape_type)
        && !SAME_TYPE(SCHEME_TYPE(le2), scheme_struct_prop_proc_shape_type)){
      pname = scheme_get_proc_name(le2, &len, 0);
    }
    context = scheme_optimize_context_to_string(info->context);
    scheme_log(info->logger,
               SCHEME_LOG_WARNING,
               0,
               "warning%s: optimizer detects procedure incorrectly applied to %d arguments%s%s",
               context,
               argc,
               pname ? ": " : "",
               pname ? pname : "");
    return NULL;
  }

  if (SAME_TYPE(SCHEME_TYPE(le), scheme_ir_lambda_type) && (info->inline_fuel >= 0)) {
    Scheme_Lambda *lam = (Scheme_Lambda *)le;
    int sz, threshold, is_leaf = 0;

    sz = lambda_body_size_plus_info(lam, 1, info, &is_leaf);
    if (is_leaf) {
      /* encourage inlining of leaves: */
      sz >>= 2;
    }
    threshold = info->inline_fuel * (2 + argc);

    /* Do we have enough fuel? */
    if ((sz >= 0) && (single_use || (sz <= threshold))) {
      Optimize_Info *sub_info;
      sub_info = info;

      /* If optimize_clone succeeds, inlining succeeds. */
      le = optimize_clone(single_use, (Scheme_Object *)lam, sub_info, empty_eq_hash_tree, 0);

      if (le) {
        LOG_INLINE(fprintf(stderr, "Inline %d[%d]<=%d@%d %d %s\n", sz, is_leaf, threshold, info->inline_fuel,
                           single_use, scheme_write_to_string(lam->name ? lam->name : scheme_false, NULL)));
        scheme_log(info->logger,
                   SCHEME_LOG_DEBUG,
                   0,
                   "inlining %s size: %d threshold: %d#<separator>%s",
                   scheme_write_to_string(lam->name ? lam->name : scheme_false, NULL),
                   sz,
                   threshold,
                   scheme_optimize_context_to_string(info->context));
        le = apply_inlined((Scheme_Lambda *)le, sub_info, argc, app, app2, app3, context,
                           orig_le, prev, single_use);
        return le;
      } else {
        LOG_INLINE(fprintf(stderr, "No inline %s\n", scheme_write_to_string(lam->name ? lam->name : scheme_false, NULL)));
        scheme_log(info->logger,
                   SCHEME_LOG_DEBUG,
                   0,
                   "no-inlining %s size: %d threshold: %d#<separator>%s",
                   scheme_write_to_string(lam->name ? lam->name : scheme_false, NULL),
                   sz,
                   threshold,
                   scheme_optimize_context_to_string(info->context));
      }
    } else {
      LOG_INLINE(fprintf(stderr, "No fuel %s %d[%d]>%d@%d %d\n", scheme_write_to_string(lam->name ? lam->name : scheme_false, NULL),
                         sz, is_leaf, threshold,
                         info->inline_fuel, info->use_psize));
      scheme_log(info->logger,
                 SCHEME_LOG_DEBUG,
                 0,
                 "out-of-fuel %s size: %d threshold: %d#<separator>%s",
                 scheme_write_to_string(lam->name ? lam->name : scheme_false, NULL),
                 sz,
                 threshold,
                 scheme_optimize_context_to_string(info->context));
    }
  }

  if (!scheme_check_leaf_rator(le))
    info->has_nonleaf = 1;

  return NULL;
}

static int is_local_type_expression(Scheme_Object *expr, Optimize_Info *info)
/* Get an unboxing type (e.g., flonum) for `expr` */
{
  return scheme_predicate_to_local_type(expr_implies_predicate(expr, info));
}

static void register_local_argument_types(Scheme_App_Rec *app, Scheme_App2_Rec *app2, Scheme_App3_Rec *app3,
                                          Optimize_Info *info)
/* If `rator` is a variable bound to a `lambda`, record the types of actual arguments
   provided in a function call. If all calls are consistent with unboxing, then the
   procedure will accept unboxed arguments at run time. */
{
  Scheme_Object *rator, *rand, *le;
  int n, i, nth_app;

  if (app) {
    rator = app->args[0];
    n = app->num_args;
    nth_app = SCHEME_APPN_FLAGS(app) & APPN_POSITION_MASK;
  } else if (app2) {
    rator = app2->rator;
    n = 1;
    nth_app = SCHEME_APPN_FLAGS(app2) & APPN_POSITION_MASK;
  } else {
    rator = app3->rator;
    n = 2;
    nth_app = SCHEME_APPN_FLAGS(app3) & APPN_POSITION_MASK;
  }

  if (SAME_TYPE(SCHEME_TYPE(rator), scheme_ir_local_type)) {
    le = optimize_info_lookup(rator);
    if (le && SCHEME_WILL_BE_LAMBDAP(le))
      le = SCHEME_WILL_BE_LAMBDA(le);

    if (le && SAME_TYPE(SCHEME_TYPE(le), scheme_ir_lambda_type)) {
      Scheme_Lambda *lam = (Scheme_Lambda *)le;
      if ((lam->num_params == n)
          && !(SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_HAS_REST)) {
        Scheme_Object *pred;

        if (!lam->ir_info->arg_types) {
          Scheme_Object **arg_types;
          short *contributors;
          arg_types = MALLOC_N(Scheme_Object*, n);
          lam->ir_info->arg_types = arg_types;
          contributors = MALLOC_N_ATOMIC(short, n);
          memset(contributors, 0, sizeof(short) * n);
          lam->ir_info->arg_type_contributors = contributors;
        }
        
        for (i = 0; i < n; i++) {
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

          if (lam->ir_info->arg_types[i]
              || !lam->ir_info->arg_type_contributors[i]) {
            int widen_to_top = 0;

            pred = expr_implies_predicate(rand, info);

            if (pred) {
              if (!lam->ir_info->arg_type_contributors[i]) {
                lam->ir_info->arg_types[i] = pred;
                if (nth_app)
                  lam->ir_info->arg_type_contributors[i] |= (1 << (nth_app-1));
              } else if (predicate_implies(pred, lam->ir_info->arg_types[i])) {
                /* ok */
                if (nth_app)
                  lam->ir_info->arg_type_contributors[i] |= (1 << (nth_app-1));
              } else if (predicate_implies(lam->ir_info->arg_types[i], pred)) {
                /* widen */
                lam->ir_info->arg_types[i] = pred;
                if (nth_app)
                  lam->ir_info->arg_type_contributors[i] |= (1 << (nth_app-1));
              } else
                widen_to_top = 1;
            } else
              widen_to_top = 1;

            if (widen_to_top) {
              if (nth_app) {
                /* Since we cant provide a nice type right now, just
                   don't check in, in case a future iteration provides
                   better information. If we never check in with a type,
                   it will count as widening in the end. */
              } else {
                /* since we don't have an identity, the lambda won't
                   be able to tell whether all apps have checked in,
                   so we have to registers a "top" as an anonymous
                   contributor. */
                lam->ir_info->arg_type_contributors[i] |= (1 << (SCHEME_USE_COUNT_INF-1));
                lam->ir_info->arg_types[i] = NULL;
              }
            }
          }
        }
      }
    }
  }
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

static void set_application_omittable(Scheme_Object *app, Scheme_Object *a)
{
  switch (SCHEME_TYPE(app)) {
  case scheme_application_type:
    SCHEME_APPN_FLAGS((Scheme_App_Rec *)app) |= APPN_FLAG_OMITTABLE;
    break;
  case scheme_application2_type:
    SCHEME_APPN_FLAGS((Scheme_App2_Rec *)app) |= APPN_FLAG_OMITTABLE;
    break;
  case scheme_application3_type:
    SCHEME_APPN_FLAGS((Scheme_App3_Rec *)app) |= APPN_FLAG_OMITTABLE;
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
    
  extract_tail_inside(&rator, &inside);

  if (!inside)
    return NULL;

  /* Moving a variable into application position: */
  if (SAME_TYPE(SCHEME_TYPE(rator), scheme_ir_local_type)) {
    Scheme_IR_Local *var = SCHEME_VAR(rator);
    if (var->non_app_count < SCHEME_USE_COUNT_INF)
      --var->non_app_count;
  }

  reset_rator(app, rator);
  orig_rator = replace_tail_inside(app, inside, orig_rator);

  return scheme_optimize_expr(orig_rator, info, context);
}

static int is_nonmutating_nondependant_primitive(Scheme_Object *rator, int n)
/* Does not include SCHEME_PRIM_IS_UNSAFE_OMITABLE, because those can
   depend on earlier tests (explicit or implicit) for whether the
   unsafe operation is defined */
{
  if (SCHEME_PRIMP(rator)
      && ((SCHEME_PRIM_PROC_OPT_FLAGS(rator) & (SCHEME_PRIM_IS_OMITABLE | SCHEME_PRIM_IS_OMITABLE_ALLOCATION))
          && !(SCHEME_PRIM_PROC_OPT_FLAGS(rator) & (SCHEME_PRIM_IS_UNSAFE_OMITABLE))
          && !((SAME_OBJ(scheme_values_proc, rator) && (n != 1))))
      && (n >= ((Scheme_Primitive_Proc *)rator)->mina)
      && (n <= ((Scheme_Primitive_Proc *)rator)->mu.maxa))
    return 1;

  return 0;
}

static int is_primitive_allocating(Scheme_Object *rator, int n)
{
  if (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & (SCHEME_PRIM_IS_OMITABLE_ALLOCATION))
    return 1;

  return 0;
}

static int is_noncapturing_primitive(Scheme_Object *rator, int n)
{
  if (SCHEME_PRIMP(rator)) {
    int opt, t;
    opt = ((Scheme_Prim_Proc_Header *)rator)->flags & SCHEME_PRIM_OPT_MASK;
    if (opt >= SCHEME_PRIM_OPT_IMMEDIATE)
      return 1;
    if (opt >= SCHEME_PRIM_OPT_NONCM) {
      if (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & SCHEME_PRIM_ALWAYS_ESCAPES) {
        /* even if a continuation is captured, it won't get back */
        return 1;
      }
    }
    t = (((Scheme_Primitive_Proc *)rator)->pp.flags & SCHEME_PRIM_OTHER_TYPE_MASK);
    if (!n && (t == SCHEME_PRIM_TYPE_PARAMETER))
      return 1;
    if (SAME_OBJ(rator, scheme_values_proc))
      return 1;
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
    if (SAME_OBJ(rator, scheme_values_proc))
      return 1;
  }

  return 0;
}

static int is_always_escaping_primitive(Scheme_Object *rator)
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

int scheme_predicate_to_local_type(Scheme_Object *pred)
{
  if (!pred)
    return 0;
  if (SAME_OBJ(scheme_flonum_p_proc, pred))
    return SCHEME_LOCAL_TYPE_FLONUM;
  if (SAME_OBJ(scheme_fixnum_p_proc, pred))
    return SCHEME_LOCAL_TYPE_FIXNUM;
  if (SAME_OBJ(scheme_extflonum_p_proc, pred))
    return SCHEME_LOCAL_TYPE_EXTFLONUM;
  return 0;
}

int scheme_expr_produces_local_type(Scheme_Object *expr, int *_involves_k_cross)
{
  if (_involves_k_cross) *_involves_k_cross = 0;
  return scheme_predicate_to_local_type(do_expr_implies_predicate(expr, NULL, _involves_k_cross,
                                                                  10, empty_eq_hash_tree));
}

static Scheme_Object *rator_implies_predicate(Scheme_Object *rator, Optimize_Info *info, int argc)
{
  if (SCHEME_PRIMP(rator)) {
    if (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & SCHEME_PRIM_PRODUCES_REAL)
      return scheme_real_p_proc;
    else if (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & SCHEME_PRIM_PRODUCES_NUMBER)
      return scheme_number_p_proc;
    else if (SAME_OBJ(rator, scheme_cons_proc))
      return scheme_pair_p_proc;
    else if (SAME_OBJ(rator, scheme_unsafe_cons_list_proc))
      return scheme_list_pair_p_proc;
    else if (SAME_OBJ(rator, scheme_mcons_proc))
      return scheme_mpair_p_proc;
    else if (SAME_OBJ(rator, scheme_list_proc)) {
      if (argc >= 1)
        return scheme_list_pair_p_proc;
      else
        return scheme_null_p_proc;
    } else if (SAME_OBJ(rator, scheme_list_star_proc)) {
      if (argc > 2)
        return scheme_pair_p_proc;
    } else if (IS_NAMED_PRIM(rator, "vector->list")
               || IS_NAMED_PRIM(rator, "map")) {
      return scheme_list_p_proc;
    } else if (IS_NAMED_PRIM(rator, "string-ref")) {
      return scheme_char_p_proc;
    } else if (IS_NAMED_PRIM(rator, "string-append")
               || IS_NAMED_PRIM(rator, "string->immutable-string")
               || IS_NAMED_PRIM(rator, "symbol->string")
               || IS_NAMED_PRIM(rator, "keyword->string")) {
        return scheme_string_p_proc;
    } else if (IS_NAMED_PRIM(rator, "bytes-append")
               || IS_NAMED_PRIM(rator, "bytes->immutable-bytes")) {
        return scheme_byte_string_p_proc;
    } else if (SAME_OBJ(rator, scheme_vector_proc)
               || SAME_OBJ(rator, scheme_vector_immutable_proc)
               || SAME_OBJ(rator, scheme_make_vector_proc)
               || SAME_OBJ(rator, scheme_list_to_vector_proc)
               || SAME_OBJ(rator, scheme_struct_to_vector_proc)
               || IS_NAMED_PRIM(rator, "vector->immutable-vector"))
      return scheme_vector_p_proc;
    else if (SAME_OBJ(rator, scheme_box_proc)
             || SAME_OBJ(rator, scheme_box_immutable_proc))
      return scheme_box_p_proc;
    else if (SAME_OBJ(rator, scheme_void_proc))
      return scheme_void_p_proc;
    else if (SAME_OBJ(rator, scheme_procedure_specialize_proc))
      return scheme_procedure_p_proc;
    else if (IS_NAMED_PRIM(rator, "vector-set!")
             || IS_NAMED_PRIM(rator, "string-set!")
             || IS_NAMED_PRIM(rator, "bytes-set!")
             || IS_NAMED_PRIM(rator, "set-box!"))
      return scheme_void_p_proc;
    else if (IS_NAMED_PRIM(rator, "vector-set!")
             || IS_NAMED_PRIM(rator, "string-set!")
             || IS_NAMED_PRIM(rator, "bytes-set!"))
      return scheme_void_p_proc;
    else if (IS_NAMED_PRIM(rator, "string->symbol")
             || IS_NAMED_PRIM(rator, "gensym"))
      return scheme_symbol_p_proc;
    else if (IS_NAMED_PRIM(rator, "string->keyword"))
      return scheme_keyword_p_proc;
    else if (IS_NAMED_PRIM(rator, "pair?")
             || IS_NAMED_PRIM(rator, "mpair?")
             || IS_NAMED_PRIM(rator, "list?")
             || IS_NAMED_PRIM(rator, "list-pair?")
             || IS_NAMED_PRIM(rator, "vector?")
             || IS_NAMED_PRIM(rator, "box?")
             || IS_NAMED_PRIM(rator, "number?")
             || IS_NAMED_PRIM(rator, "real?")
             || IS_NAMED_PRIM(rator, "complex?")
             || IS_NAMED_PRIM(rator, "rational?")
             || IS_NAMED_PRIM(rator, "integer?")
             || IS_NAMED_PRIM(rator, "exact-integer?")
             || IS_NAMED_PRIM(rator, "exact-nonnegative-integer?")
             || IS_NAMED_PRIM(rator, "exact-positive-integer?")
             || IS_NAMED_PRIM(rator, "inexact-real?")
             || IS_NAMED_PRIM(rator, "fixnum?")
             || IS_NAMED_PRIM(rator, "flonum?")
             || IS_NAMED_PRIM(rator, "single-flonum?")
             || IS_NAMED_PRIM(rator, "null?")
             || IS_NAMED_PRIM(rator, "void?")
             || IS_NAMED_PRIM(rator, "symbol?")
             || IS_NAMED_PRIM(rator, "keyword?")
             || IS_NAMED_PRIM(rator, "string?")
             || IS_NAMED_PRIM(rator, "bytes?")
             || IS_NAMED_PRIM(rator, "path?")
             || IS_NAMED_PRIM(rator, "char?")
             || IS_NAMED_PRIM(rator, "interned-char?")
             || IS_NAMED_PRIM(rator, "boolean?")
             || IS_NAMED_PRIM(rator, "chaperone?")
             || IS_NAMED_PRIM(rator, "impersonator?")
             || IS_NAMED_PRIM(rator, "procedure?")
             || IS_NAMED_PRIM(rator, "eof-object?")
             || IS_NAMED_PRIM(rator, "immutable?")
             || IS_NAMED_PRIM(rator, "not")
             || IS_NAMED_PRIM(rator, "true-object?")
             || IS_NAMED_PRIM(rator, "zero?")
             || IS_NAMED_PRIM(rator, "procedure-arity-includes?")
             || IS_NAMED_PRIM(rator, "variable-reference-constant?")
             || IS_NAMED_PRIM(rator, "eq?")
             || IS_NAMED_PRIM(rator, "eqv?")
             || IS_NAMED_PRIM(rator, "equal?")
             || IS_NAMED_PRIM(rator, "string=?")
             || IS_NAMED_PRIM(rator, "bytes=?")
             || IS_NAMED_PRIM(rator, "char=?")
             || IS_NAMED_PRIM(rator, "free-identifier=?")
             || IS_NAMED_PRIM(rator, "bound-identifier=?")
             || IS_NAMED_PRIM(rator, "procedure-closure-contents-eq?")) {
      return scheme_boolean_p_proc;
    }

    {
      Scheme_Object *p;
      p = local_type_to_predicate(produces_local_type(rator, argc));
      if (p)
        return p;
    }
  }

  {
    Scheme_Object *shape;
    shape = get_struct_proc_shape(rator, info, 1);
    if (shape) {
      if (SAME_TYPE(SCHEME_TYPE(shape), scheme_struct_proc_shape_type)) {
        if (((SCHEME_PROC_SHAPE_MODE(shape) & STRUCT_PROC_SHAPE_MASK) == STRUCT_PROC_SHAPE_PRED)) {
          return scheme_boolean_p_proc;
        }
      } else if (SAME_TYPE(SCHEME_TYPE(shape), scheme_struct_prop_proc_shape_type)) {
        if (SCHEME_PROP_PROC_SHAPE_MODE(shape) == STRUCT_PROP_PROC_SHAPE_PRED) {
          return scheme_boolean_p_proc;
        }
      }
    }
  }

  return NULL;
}

static Scheme_Object *do_expr_implies_predicate(Scheme_Object *expr, Optimize_Info *info,
                                                int *_involves_k_cross, int fuel,
                                                Scheme_Hash_Tree *ignore_vars)
/* can be called by the JIT with info = NULL;
   in that case, beware that the validator must be
   able to reconstruct the result in a shallow way, so don't 
   make the result of a function call depend on its arguments */
{
  if (fuel <= 0)
    return NULL;

  switch (SCHEME_TYPE(expr)) {
  case scheme_ir_local_type:
    {
      if (scheme_hash_tree_get(ignore_vars, expr))
        return NULL;
      
      if (!SCHEME_VAR(expr)->mutated) {
        Scheme_Object *p;

        if (info) {
          p = optimize_get_predicate(info, expr, 0);
          if (p)
            return p;
        }

        p = local_type_to_predicate(SCHEME_VAR(expr)->val_type);
        if (p) {
          if (_involves_k_cross
              && SCHEME_VAR(expr)->escapes_after_k_tick)
            *_involves_k_cross = 1;
          return p;
        }

        if ((SCHEME_VAR(expr)->mode == SCHEME_VAR_MODE_OPTIMIZE)
            && SCHEME_VAR(expr)->optimize.known_val)
          return do_expr_implies_predicate(SCHEME_VAR(expr)->optimize.known_val, info, _involves_k_cross,
                                           fuel-1, ignore_vars);
      }
    }
    break;
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)expr;

      if (SCHEME_PRIMP(app->rator)
          && SCHEME_PRIM_PROC_OPT_FLAGS(app->rator) & SCHEME_PRIM_CLOSED_ON_REALS) {
        Scheme_Object *p;
        p = do_expr_implies_predicate(app->rand, info, NULL, fuel-1, ignore_vars);
        if (p && predicate_implies(p, scheme_real_p_proc))
          return scheme_real_p_proc;
      }
    
      if (SAME_OBJ(app->rator, scheme_cdr_proc)
          || SAME_OBJ(app->rator, scheme_unsafe_cdr_proc)) {
        Scheme_Object *p;
        p = do_expr_implies_predicate(app->rand, info, NULL, fuel-1, ignore_vars);
        if (SAME_OBJ(p, scheme_list_pair_p_proc))
          return scheme_list_p_proc;
      }

      return rator_implies_predicate(app->rator, info, 1);
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
            combination produces a fixnum if either argument is a literal,
            nonnegative fixnum. */
         if ((SCHEME_INTP(app->rand1)
              && (SCHEME_INT_VAL(app->rand1) >= 0)
              && IN_FIXNUM_RANGE_ON_ALL_PLATFORMS(SCHEME_INT_VAL(app->rand1)))
             || (SCHEME_INTP(app->rand2)
                 && (SCHEME_INT_VAL(app->rand2) >= 0)
                 && IN_FIXNUM_RANGE_ON_ALL_PLATFORMS(SCHEME_INT_VAL(app->rand2)))) {
           return scheme_fixnum_p_proc;
         }
      }

      if (SCHEME_PRIMP(app->rator)
          && SCHEME_PRIM_PROC_OPT_FLAGS(app->rator) & SCHEME_PRIM_CLOSED_ON_REALS) {
        Scheme_Object *p;
        p = do_expr_implies_predicate(app->rand1, info, NULL, fuel-1, ignore_vars);
        if (p && predicate_implies(p, scheme_real_p_proc)) {
          p = do_expr_implies_predicate(app->rand2, info, NULL, fuel-1, ignore_vars);
          if (p && predicate_implies(p, scheme_real_p_proc)) {
            return scheme_real_p_proc;
          }
        }
      }

      if (SAME_OBJ(app->rator, scheme_cons_proc)) {
        Scheme_Object *p;
        p = do_expr_implies_predicate(app->rand2, info, NULL, fuel-1, ignore_vars);
        if (SAME_OBJ(p, scheme_list_pair_p_proc)
            || SAME_OBJ(p, scheme_list_p_proc)
            || SAME_OBJ(p, scheme_null_p_proc))
          return scheme_list_pair_p_proc;
      }

      if (SCHEME_PRIMP(app->rator)
          && IS_NAMED_PRIM(app->rator, "append")) {
        Scheme_Object *p;
        p = do_expr_implies_predicate(app->rand2, info, NULL, fuel-1, ignore_vars);
        if (SAME_OBJ(p, scheme_list_pair_p_proc))
          return scheme_list_pair_p_proc;
        if (SAME_OBJ(p, scheme_list_p_proc)
            || SAME_OBJ(p, scheme_null_p_proc))
          return scheme_list_p_proc;
      }

      return rator_implies_predicate(app->rator, info, 2);
    }
    break;
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)expr;

      if (SCHEME_PRIMP(app->args[0])
          && SCHEME_PRIM_PROC_OPT_FLAGS(app->args[0]) & SCHEME_PRIM_CLOSED_ON_REALS) {
        Scheme_Object *p;
        int i;
        for (i = 0; i < app->num_args; i++) {
          p = do_expr_implies_predicate(app->args[i+1], info, NULL, fuel-1, ignore_vars);
          if (!p || !predicate_implies(p, scheme_real_p_proc))
            break;
        }
        if (i >= app->num_args)
          return scheme_real_p_proc;
      }
      
      if (SCHEME_PRIMP(app->args[0])
          && IS_NAMED_PRIM(app->args[0], "append")) {
        Scheme_Object *p;
        p = do_expr_implies_predicate(app->args[app->num_args], info, NULL, fuel-1, ignore_vars);
        if (SAME_OBJ(p, scheme_list_pair_p_proc))
          return scheme_list_pair_p_proc;
        if (SAME_OBJ(p, scheme_list_p_proc)
            || SAME_OBJ(p, scheme_null_p_proc))
          return scheme_list_p_proc;
      }

      return rator_implies_predicate(app->args[0], info, app->num_args);
    }
    break;
  case scheme_ir_lambda_type:
    return scheme_procedure_p_proc;
    break;
  case scheme_case_lambda_sequence_type:
    return scheme_procedure_p_proc;
    break;
  case scheme_ir_quote_syntax_type:
    return scheme_syntax_p_proc;
    break;
  case scheme_branch_type:
    {
      Scheme_Object *l, *r;
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)expr;
      l = do_expr_implies_predicate(b->tbranch, info, _involves_k_cross, fuel-1, ignore_vars);
      if (l) {
        r = do_expr_implies_predicate(b->fbranch, info, _involves_k_cross, fuel-1, ignore_vars);
        if (predicate_implies(l, r))
          return r;
        else if (predicate_implies(r, l))
          return l;
        else
          return NULL;
      }
    }
    break;
  case scheme_sequence_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)expr;

      return do_expr_implies_predicate(seq->array[seq->count-1], info, _involves_k_cross, fuel-1, ignore_vars);
    }
  case scheme_with_cont_mark_type:
    {
      Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)expr;

      return do_expr_implies_predicate(wcm->body, info, _involves_k_cross, fuel-1, ignore_vars);
    }
  case scheme_ir_let_header_type:
    {
      Scheme_IR_Let_Header *lh = (Scheme_IR_Let_Header *)expr;
      Scheme_IR_Let_Value *irlv;
      int i, j;
      expr = lh->body;
      for (i = 0; i < lh->num_clauses; i++) {
        irlv = (Scheme_IR_Let_Value *)expr;
        for (j = 0; j < irlv->count; j++) {
          ignore_vars = scheme_hash_tree_set(ignore_vars, (Scheme_Object *)irlv->vars[j],
                                             scheme_true);
        }
        expr = irlv->body;
      }
      return do_expr_implies_predicate(expr, info, _involves_k_cross, fuel-1, ignore_vars);
    }
    break;
  case scheme_begin0_sequence_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)expr;

      return do_expr_implies_predicate(seq->array[0], info, _involves_k_cross, fuel-1, ignore_vars);
    }
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
    if (SCHEME_REALP(expr))
      return scheme_real_p_proc;
    if (SCHEME_NUMBERP(expr))
      return scheme_number_p_proc;

    if (SCHEME_NULLP(expr))
      return scheme_null_p_proc;
    if (scheme_is_list(expr))
      return scheme_list_pair_p_proc;
    if (SCHEME_PAIRP(expr))
      return scheme_pair_p_proc;
    if (SCHEME_MPAIRP(expr))
      return scheme_mpair_p_proc;
    if (SCHEME_CHAR_STRINGP(expr))
      return scheme_string_p_proc;
    if (SCHEME_BYTE_STRINGP(expr))
      return scheme_byte_string_p_proc;
    if (SCHEME_VOIDP(expr))
      return scheme_void_p_proc;
    if (SCHEME_EOFP(expr))
      return scheme_eof_object_p_proc;
    if (SCHEME_KEYWORDP(expr))
      return scheme_keyword_p_proc;
    if (SCHEME_SYMBOLP(expr))
      return scheme_symbol_p_proc;
    if (SCHEME_CHARP(expr) && SCHEME_CHAR_VAL(expr) < 256)
      return scheme_interned_char_p_proc;
    if (SCHEME_CHARP(expr))
      return scheme_char_p_proc;
    if (SAME_OBJ(expr, scheme_true))
      return scheme_true_object_p_proc;
    if (SCHEME_FALSEP(expr))
      return scheme_not_proc;
    if (SCHEME_PROCP(expr))
      return scheme_procedure_p_proc;
  }

  /* This test is slower, so put it at the end */  
  if (info
      && lookup_constant_proc(info, expr, -1)) {
    return scheme_procedure_p_proc;
  }

  return NULL;
}

static Scheme_Object *expr_implies_predicate(Scheme_Object *expr, Optimize_Info *info)
{
  return do_expr_implies_predicate(expr, info, NULL, 5, empty_eq_hash_tree);
}

static Scheme_Object *finish_optimize_app(Scheme_Object *o, Optimize_Info *info, int context)
{
  switch(SCHEME_TYPE(o)) {
  case scheme_application_type:
    return finish_optimize_application((Scheme_App_Rec *)o, info, context);
  case scheme_application2_type:
    return finish_optimize_application2((Scheme_App2_Rec *)o, info, context);
  case scheme_application3_type:
    return finish_optimize_application3((Scheme_App3_Rec *)o, info, context);
  default:
    return o; /* may be a constant due to constant-folding */
  }
}

static Scheme_Object *direct_apply(Scheme_Object *expr, Scheme_Object *rator, Scheme_Object *last_rand, Optimize_Info *info)
/* Convert `(apply f arg1 ... (list arg2 ...))` to `(f arg1 ... arg2 ...)` */
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

static Scheme_Object *call_with_immed_mark(Scheme_Object *rator,
                                           Scheme_Object *rand1,
                                           Scheme_Object *rand2,
                                           Scheme_Object *rand3,
                                           Optimize_Info *info)
/* Convert `(call-with-immediate-continuation-mark (lambda (arg) M))`
   to the with-immediate-mark bytecode form. */
{
  if (SAME_OBJ(rator, scheme_call_with_immed_mark_proc)
      && SAME_TYPE(SCHEME_TYPE(rand2), scheme_ir_lambda_type)
      && (((Scheme_Lambda *)rand2)->num_params == 1)
      && !(SCHEME_LAMBDA_FLAGS(((Scheme_Lambda *)rand2)) & LAMBDA_HAS_REST)) {
    Scheme_With_Continuation_Mark *wcm;
    Scheme_Object *e;
    
    wcm = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
    wcm->so.type = scheme_with_immed_mark_type;

    wcm->key = rand1;
    wcm->val = (rand3 ? rand3 : scheme_false);

    e = (Scheme_Object *)((Scheme_Lambda *)rand2)->ir_info->vars[0];
    e = scheme_make_mutable_pair(e, ((Scheme_Lambda *)rand2)->body);
    wcm->body = e;

    return (Scheme_Object *)wcm;
  }

  return NULL;
}

static Scheme_Object *optimize_application(Scheme_Object *o, Optimize_Info *info, int context)
{
  Scheme_Object *le;
  Scheme_App_Rec *app;
  int i, n, rator_apply_escapes = 0, sub_context = 0;
  Optimize_Info_Sequence info_seq;

  app = (Scheme_App_Rec *)o;

  /* Check for (apply ... (list ...)) early: */
  le = direct_apply((Scheme_Object *)app, app->args[0], app->args[app->num_args], info);
  if (le)
    return scheme_optimize_expr(le, info, context);

  if (app->num_args == 3) {
    le = call_with_immed_mark(app->args[0], app->args[1], app->args[2], app->args[3], info);
    if (le)
      return scheme_optimize_expr(le, info, context);
  }

  le = check_app_let_rator(o, app->args[0], info, app->num_args, context);
  if (le)
    return le;

  n = app->num_args + 1;

  optimize_info_seq_init(info, &info_seq);

  for (i = 0; i < n; i++) {
    if (!i) {
      le = optimize_for_inline(info, app->args[i], n - 1, app, NULL, NULL, context, 0);
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
        e = optimize_ignored(e, info, 1, 1, 5);
        if (e) {
          e = ensure_single_value(e, info);
          l = scheme_make_pair(e, l);
        }
      }
      return ensure_noncm(scheme_make_sequence_compilation(l, 1, 0), info);
    }

    if (!i) {
      /* Maybe found "((lambda" after optimizing; try again */
      le = optimize_for_inline(info, app->args[i], n - 1, app, NULL, NULL, context, 1);
      if (le)
        return le;
      if (SAME_OBJ(app->args[0], scheme_values_proc)
          || SAME_OBJ(app->args[0], scheme_apply_proc))
        info->maybe_values_argument = 1;
      rator_apply_escapes = info->escapes;
    }
  }

  optimize_info_seq_done(info, &info_seq);

  /* Check for (apply ... (list ...)) after some optimizations: */
  le = direct_apply((Scheme_Object *)app, app->args[0], app->args[app->num_args], info);
  if (le) return finish_optimize_app(le, info, context);

  /* Convert (hash-ref '#hash... key (lambda () literal))
     to (hash-ref '#hash... key literal) */
  if ((app->num_args == 3)
      && SAME_OBJ(scheme_hash_ref_proc, app->args[0])
      && SCHEME_HASHTRP(app->args[1])
      && SAME_TYPE(scheme_ir_lambda_type, SCHEME_TYPE(app->args[3]))
      && (((Scheme_Lambda *)(app->args[3]))->num_params == 0)
      && (SCHEME_TYPE(((Scheme_Lambda *)app->args[3])->body) > _scheme_ir_values_types_)
      && !SCHEME_PROCP(((Scheme_Lambda *)app->args[3])->body)) {
    app->args[3] = ((Scheme_Lambda *)app->args[3])->body;
  }

  if (rator_apply_escapes) {
   info->escapes = 1;
   SCHEME_APPN_FLAGS(app) |= (APPN_FLAG_IMMED | APPN_FLAG_SFS_TAIL);
  }

  return finish_optimize_application(app, info, context);
}

static int appn_flags(Scheme_Object *rator, Optimize_Info *info)
/* Record some properties of an application that are useful to the SFS pass. */
{
  if (SAME_TYPE(SCHEME_TYPE(rator), scheme_ir_toplevel_type)) {
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

  if (SCHEME_LAMBDAP(rator)
      || SAME_TYPE(scheme_noninline_proc_type, SCHEME_TYPE(rator)))
    return APPN_FLAG_SFS_TAIL;
  
  return 0;
}

static int check_known_variant(Optimize_Info *info, Scheme_Object *app,
                               Scheme_Object *rator, Scheme_Object *rand,
                               const char *who, Scheme_Object *expect_pred, Scheme_Object *unsafe,
                               Scheme_Object *implies_pred)
/* Replace the rator with an unsafe version if we know that it's ok:
   if the argument is consistent with `expect_pred`; if `unsafe` is
   #t, then just mark the application as omittable. Alternatively, the
   rator implies a check, so add type information for subsequent
   expressions: the argument is consistent with `implies_pred` (which
   must be itself implied by `expected_pred`, but might be weaker). If
   the rand has already an incompatible type, mark that this will
   generate an error. If unsafe is NULL then rator has no unsafe
   version, so only check the type. */
{
  if (SCHEME_PRIMP(rator) && (!who || IS_NAMED_PRIM(rator, who))) {
    Scheme_Object *pred;

    pred = expr_implies_predicate(rand, info);
    if (pred) {
      if (predicate_implies(pred, expect_pred)) {
        if (unsafe) {
          if (SAME_OBJ(unsafe, scheme_true))
            set_application_omittable(app, unsafe);
          else
            reset_rator(app, unsafe);
        }
        return 1;
      } else if (predicate_implies_not(pred, implies_pred)) {
        info->escapes = 1;
      }
    } else {
      if (SAME_TYPE(SCHEME_TYPE(rand), scheme_ir_local_type))
        add_type(info, rand, implies_pred);
    }
  }

  return 0;
}

static void check_known(Optimize_Info *info, Scheme_Object *app,
                        Scheme_Object *rator, Scheme_Object *rand,
                        const char *who, Scheme_Object *expect_pred, Scheme_Object *unsafe)
/* When the expected predicate for unsafe substitution is the same as the implied predicate. */
{
  (void)check_known_variant(info, app, rator, rand, who, expect_pred, unsafe, expect_pred);
}

static void check_known_rator(Optimize_Info *info, Scheme_Object *rator)
/* Check that rator is a procedure or add type information for subsequent expressions. */
{
  Scheme_Object *pred;

  pred = expr_implies_predicate(rator, info);
  if (pred) {
    if (predicate_implies_not(pred, scheme_procedure_p_proc))
      info->escapes = 1;
  } else {
    if (SAME_TYPE(SCHEME_TYPE(rator), scheme_ir_local_type))
      add_type(info, rator, scheme_procedure_p_proc);
  }
}

static void check_known_both_try(Optimize_Info *info, Scheme_Object *app,
                                 Scheme_Object *rator, Scheme_Object *rand1, Scheme_Object *rand2,
                                 const char *who, Scheme_Object *expect_pred, Scheme_Object *unsafe)
/* Replace the rator with an unsafe version if both rands have the right type.
   If not, don't save the type, nor mark this as an error */
{
  if (SCHEME_PRIMP(rator) && (!who || IS_NAMED_PRIM(rator, who))) {
    Scheme_Object *pred1, *pred2;
      
    pred1 = expr_implies_predicate(rand1, info); 
    if (pred1 && SAME_OBJ(pred1, expect_pred)) { 
      pred2 = expr_implies_predicate(rand2, info);
      if (pred2 && SAME_OBJ(pred2, expect_pred)) { 
          reset_rator(app, unsafe);
      }
    }
  }
}

static void check_known_both_variant(Optimize_Info *info, Scheme_Object *app,
                                     Scheme_Object *rator, Scheme_Object *rand1, Scheme_Object *rand2,
                                     const char *who, Scheme_Object *expect_pred, Scheme_Object *unsafe,
                                     Scheme_Object *implies_pred)
{
  if (SCHEME_PRIMP(rator) && (!who || IS_NAMED_PRIM(rator, who))) {
    int ok1;
    ok1 = check_known_variant(info, app, rator, rand1, who, expect_pred, NULL, implies_pred);
    check_known_variant(info, app, rator, rand2, who, expect_pred, (ok1 ? unsafe : NULL), implies_pred);
  }
}

static void check_known_both(Optimize_Info *info, Scheme_Object *app,
                             Scheme_Object *rator, Scheme_Object *rand1, Scheme_Object *rand2,
                             const char *who, Scheme_Object *expect_pred, Scheme_Object *unsafe)
{
  check_known_both_variant(info, app, rator, rand1, rand2, who, expect_pred, unsafe, expect_pred);
}


static void check_known_all(Optimize_Info *info, Scheme_Object *_app, int skip_head, int skip_tail,
                            const char *who, Scheme_Object *expect_pred, Scheme_Object *unsafe)
{
  Scheme_App_Rec *app = (Scheme_App_Rec *)_app;
  if (SCHEME_PRIMP(app->args[0]) && (!who || IS_NAMED_PRIM(app->args[0], who))) {
    int ok_so_far = 1, i;

    for (i = skip_head; i < app->num_args - skip_tail; i++) {
      if (!check_known_variant(info, _app, app->args[0], app->args[i+1], who, expect_pred,
                               NULL, expect_pred))
        ok_so_far = 0;
    }
    
    if (ok_so_far && unsafe) {
      if (SAME_OBJ(unsafe, scheme_true))
        set_application_omittable(_app, unsafe);
      else
        reset_rator(_app, unsafe);
    }
  }
}

static Scheme_Object *finish_optimize_any_application(Scheme_Object *app, Scheme_Object *rator, int argc,
                                                      Optimize_Info *info, int context)
{
  check_known_rator(info, rator);

  if ((context & OPT_CONTEXT_BOOLEAN) && !info->escapes) {
    Scheme_Object *pred;
    pred = rator_implies_predicate(rator, info, argc);
    if (pred && predicate_implies_not(pred, scheme_not_proc))
      return make_discarding_sequence(app, scheme_true, info);
    else if (pred && predicate_implies(pred, scheme_not_proc))
      return make_discarding_sequence(app, scheme_false, info);
  }

  if (SAME_OBJ(rator, scheme_void_proc))
    return make_discarding_sequence(app, scheme_void, info);
  
  if (is_always_escaping_primitive(rator)) {
    info->escapes = 1;
  }

  return app;
}

static void increment_clock_counts_for_application(GC_CAN_IGNORE int *_vclock,
                                                   GC_CAN_IGNORE int *_aclock,
                                                   GC_CAN_IGNORE int *_kclock,
                                                   GC_CAN_IGNORE int *_sclock,
                                                   Scheme_Object *rator,
                                                   int argc)
{
  if (!is_nonmutating_nondependant_primitive(rator, argc))
    *_vclock += 1;
  else if (is_primitive_allocating(rator, argc))
    *_aclock += 1;

  if (!is_noncapturing_primitive(rator, argc))
    *_kclock += 1;

  if (!is_nonsaving_primitive(rator, argc))
    *_sclock += 1;
}

static void increment_clocks_for_application(Optimize_Info *info,
                                             Scheme_Object *rator,
                                             int argc)
{
  int v, a, k, s;

  v = info->vclock;
  a = info->aclock;
  k = info->kclock;
  s = info->sclock;

  increment_clock_counts_for_application(&v, &a, &k, &s, rator, argc);

  info->vclock = v;
  info->aclock = a;
  info->kclock = k;
  info->sclock = s;
}

static Scheme_Object *finish_optimize_application(Scheme_App_Rec *app, Optimize_Info *info, int context)
{
  Scheme_Object *le;
  Scheme_Object *rator =  app->args[0];
  int all_vals = 1, i, flags, rator_flags;

  for (i = app->num_args; i--; ) {
    if (SCHEME_TYPE(app->args[i+1]) < _scheme_ir_values_types_)
      all_vals = 0;
  }

  info->size += 1;
  info->preserves_marks = 1;
  info->single_result = 1;
  
  if (all_vals) {
    le = try_optimize_fold(rator, NULL, (Scheme_Object *)app, info);
    if (le)
      return le;
  }

  if (!app->num_args  
      && (SAME_OBJ(rator, scheme_list_proc)
          || (SCHEME_PRIMP(rator) && IS_NAMED_PRIM(rator, "append")))) {
    return scheme_null;
  }
   
  if (SCHEME_PRIMP(rator)
      && (app->num_args >= ((Scheme_Primitive_Proc *)rator)->mina)
      && (app->num_args <= ((Scheme_Primitive_Proc *)rator)->mu.maxa)) {
    Scheme_Object *app_o = (Scheme_Object *)app;
    Scheme_Object *rand1 = NULL, *rand2 = NULL, *rand3 = NULL;

    if (app->num_args >= 1)
      rand1 = app->args[1];

    if (app->num_args >= 2)
      rand2 = app->args[2];

    if (app->num_args >= 3)
      rand3 = app->args[3];

    check_known(info, app_o, rator, rand1, "vector-set!", scheme_vector_p_proc, NULL);
    check_known(info, app_o, rator, rand2, "vector-set!", scheme_fixnum_p_proc, NULL);

    check_known(info, app_o, rator, rand1, "procedure-arity-includes?", scheme_procedure_p_proc, NULL);

    check_known(info, app_o, rator, rand1, "map", scheme_procedure_p_proc, NULL);
    check_known(info, app_o, rator, rand1, "for-each", scheme_procedure_p_proc, NULL);
    check_known(info, app_o, rator, rand1, "andmap", scheme_procedure_p_proc, NULL);
    check_known(info, app_o, rator, rand1, "ormap", scheme_procedure_p_proc, NULL);
    check_known_all(info, app_o, 1, 0, "map", scheme_list_p_proc, NULL);
    check_known_all(info, app_o, 1, 0, "for-each", scheme_list_p_proc, NULL);
    check_known_all(info, app_o, 1, 0, "andmap", scheme_list_p_proc, NULL);
    check_known_all(info, app_o, 1, 0, "ormap", scheme_list_p_proc, NULL);

    check_known(info, app_o, rator, rand1, "string-set!", scheme_string_p_proc, NULL);
    check_known(info, app_o, rator, rand2, "string-set!", scheme_fixnum_p_proc, NULL);
    check_known(info, app_o, rator, rand3, "string-set!", scheme_char_p_proc, NULL);
    check_known(info, app_o, rator, rand1, "bytes-set!", scheme_byte_string_p_proc, NULL);
    check_known(info, app_o, rator, rand2, "bytes-set!", scheme_fixnum_p_proc, NULL);
    check_known(info, app_o, rator, rand3, "bytes-set!", scheme_fixnum_p_proc, NULL);
    
    check_known_all(info, app_o, 0, 0, "string-append", scheme_string_p_proc, scheme_true);
    check_known_all(info, app_o, 0, 0, "bytes-append", scheme_byte_string_p_proc, scheme_true);

    check_known_all(info, app_o, 0, 1, "append", scheme_list_p_proc, scheme_true);

    if (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & SCHEME_PRIM_WANTS_REAL)
      check_known_all(info, app_o, 0, 0, NULL, scheme_real_p_proc,
                      (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & SCHEME_PRIM_OMITTABLE_ON_GOOD_ARGS) ? scheme_true : NULL);
    if (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & SCHEME_PRIM_WANTS_NUMBER)
      check_known_all(info, app_o, 0, 0, NULL, scheme_number_p_proc,
                      (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & SCHEME_PRIM_OMITTABLE_ON_GOOD_ARGS) ? scheme_true : NULL);

      /* Some of these may have changed app->rator. */
    rator = app->args[0];
  }

  increment_clocks_for_application(info, rator, app->num_args);

  rator_flags = get_rator_flags(rator, app->num_args, info);
  info->preserves_marks = !!(rator_flags & LAMBDA_PRESERVES_MARKS);
  info->single_result = !!(rator_flags & LAMBDA_SINGLE_RESULT);
  if (rator_flags & LAMBDA_RESULT_TENTATIVE) {
    info->preserves_marks = -info->preserves_marks;
    info->single_result = -info->single_result;
  }

  register_local_argument_types(app, NULL, NULL, info);

  flags = appn_flags(app->args[0], info);
  SCHEME_APPN_FLAGS(app) |= flags;

  return finish_optimize_any_application((Scheme_Object *)app, app->args[0], app->num_args,
                                         info, context);
}

static Scheme_Object *try_reduce_predicate(Scheme_Object *rator, Scheme_Object *rand,
                                           Optimize_Info *info)
/* Change (pair? (list X complex-Y Z)) => (begin complex-Y #t), etc.
   It's especially nice to avoid the constructions. */
{
  Scheme_Object *pred;

  if (!relevant_predicate(rator))
    return NULL;

  pred = expr_implies_predicate(rand, info);

  if (!pred)
    return NULL;

  if (predicate_implies(pred, rator))
    return make_discarding_sequence(rand, scheme_true, info);
  else if (predicate_implies_not(pred, rator))
    return make_discarding_sequence(rand, scheme_false, info);

  return NULL;
}

static Scheme_Object *check_ignored_call_cc(Scheme_Object *rator, Scheme_Object *rand,
                                            Optimize_Info *info, int context)
/* Convert (call/cc (lambda (ignored) body ...)) to (begin body ...) */
{
  if (SCHEME_PRIMP(rator)
      && (IS_NAMED_PRIM(rator, "call-with-current-continuation")
          || IS_NAMED_PRIM(rator, "call-with-composable-continuation")
          || IS_NAMED_PRIM(rator, "call-with-escape-continuation"))) {
      Scheme_Object *proc;

      proc = lookup_constant_proc(info, rand, 1);

      if (proc && SAME_TYPE(SCHEME_TYPE(proc), scheme_ir_lambda_type)) {
          Scheme_Lambda *lam = (Scheme_Lambda *)proc;
          if (lam->num_params == 1) {
              Scheme_IR_Lambda_Info *cl = lam->ir_info;
              if (!cl->vars[0]->use_count) {
                Scheme_Object *expr;
                info->vclock++;
                expr = make_application_2(rand, scheme_void, info);
                if (IS_NAMED_PRIM(rator, "call-with-escape-continuation")) {
                  Scheme_Sequence *seq;

                  seq = scheme_malloc_sequence(1);
                  seq->so.type = scheme_begin0_sequence_type;
                  seq->count = 1;
                  seq->array[0] = expr;
                  
                  expr = (Scheme_Object *)seq;
                }                
                return scheme_optimize_expr(expr, info, context);
              }
          }
      }
  }
  return NULL;
}

static Scheme_Object *make_optimize_prim_application2(Scheme_Object *prim, Scheme_Object *rand,
                                                      Optimize_Info *info, int context)
/* make (prim rand) and optimize it. rand must be already optimized */
{
  Scheme_Object *alt;
  alt = make_application_2(prim, rand, info);
  /* scheme_make_application may use constant folding, check that alt is not a constant */
  if (SAME_TYPE(SCHEME_TYPE(alt), scheme_application2_type)) {
    return finish_optimize_application2((Scheme_App2_Rec *)alt, info, context);
  } else
    return alt;
}


static Scheme_Object *optimize_application2(Scheme_Object *o, Optimize_Info *info, int context)
{
  Scheme_App2_Rec *app;
  Scheme_Object *le;
  int rator_apply_escapes, sub_context, ty;
  Optimize_Info_Sequence info_seq;

  app = (Scheme_App2_Rec *)o;

  le = check_app_let_rator(o, app->rator, info, 1, context);
  if (le)
    return le;

  le = check_ignored_call_cc(app->rator, app->rand, info, context);
  if (le)
    return le;

  le = optimize_for_inline(info, app->rator, 1, NULL, app, NULL, context, 0);
  if (le)
    return le;

  optimize_info_seq_init(info, &info_seq);

  sub_context = OPT_CONTEXT_SINGLED;

  le = scheme_optimize_expr(app->rator, info, sub_context);
  app->rator = le;
  if (info->escapes) {
    optimize_info_seq_done(info, &info_seq);
    return ensure_noncm(app->rator, info);
  }

  {
    /* Maybe found "((lambda" after optimizing; try again */
    le = optimize_for_inline(info, app->rator, 1, NULL, app, NULL, context, 1);
    if (le)
      return le;
    rator_apply_escapes = info->escapes;
  }

  if (SAME_PTR(scheme_not_proc, app->rator)){
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
    return ensure_noncm(make_discarding_first_sequence(app->rator, app->rand, info), info);
  }

  if (rator_apply_escapes) {
   info->escapes = 1;
   SCHEME_APPN_FLAGS(app) |= (APPN_FLAG_IMMED | APPN_FLAG_SFS_TAIL);
  }

  return finish_optimize_application2(app, info, context);
}

static Scheme_Object *finish_optimize_application2(Scheme_App2_Rec *app, Optimize_Info *info, int context)
{
  int flags, rator_flags;
  Scheme_Object *rator =  app->rator;
  Scheme_Object *rand, *inside = NULL, *alt;

  info->size += 1;
  info->preserves_marks = 1;
  info->single_result = 1;

  /* Path for direct constant folding */
  if (SCHEME_TYPE(app->rand) > _scheme_ir_values_types_) {
    Scheme_Object *le;
    le = try_optimize_fold(rator, NULL, (Scheme_Object *)app, info);
    if (le)
      return le;
  }

  rand = app->rand;

  /* We can go inside a `begin' and a `let', which is useful in case
     the argument was a function call that has been inlined. */
  extract_tail_inside(&rand, &inside);

  if (SCHEME_TYPE(rand) > _scheme_ir_values_types_) {
    Scheme_Object *le;
    le = try_optimize_fold(rator, scheme_make_pair(rand, scheme_null), NULL, info);
    if (le)
      return replace_tail_inside(le, inside, app->rand);
  }

  if (SAME_OBJ(scheme_values_proc, rator)
      || SAME_OBJ(scheme_list_star_proc, rator)
      || (SCHEME_PRIMP(rator) && IS_NAMED_PRIM(rator, "append"))) {
    SCHEME_APPN_FLAGS(app) |= (APPN_FLAG_IMMED | APPN_FLAG_SFS_TAIL);
    if ((context & OPT_CONTEXT_SINGLED)
        || scheme_omittable_expr(rand, 1, -1, 0, info, info)
        || single_valued_noncm_expression(rand, info, 5)) {
      return replace_tail_inside(rand, inside, app->rand);
    }
    app->rator = scheme_values_proc;
    rator = scheme_values_proc;
  }

  if (SCHEME_PRIMP(rator)) {
    /* Check for things like (cXr (cons X Y)): */
    switch (SCHEME_TYPE(rand)) {
    case scheme_application2_type:
      {
        Scheme_App2_Rec *app2 = (Scheme_App2_Rec *)rand;
        if (IS_NAMED_PRIM(rator, "car")
            || IS_NAMED_PRIM(rator, "unsafe-car")) {
          if (SAME_OBJ(scheme_list_proc, app2->rator)) {
            /* (car (list X)) */
            alt = ensure_single_value_noncm(app2->rand, info);
            return replace_tail_inside(alt, inside, app->rand);
          }
        } else if (IS_NAMED_PRIM(rator, "cdr")
                   || IS_NAMED_PRIM(rator, "unsafe-cdr")) {
          if (SAME_OBJ(scheme_list_proc, app2->rator)) {
            /* (cdr (list X)) */
            alt = make_discarding_sequence(app2->rand, scheme_null, info);
            return replace_tail_inside(alt, inside, app->rand);
          }
        } else if (IS_NAMED_PRIM(rator, "unbox")
                   || IS_NAMED_PRIM(rator, "unsafe-unbox")
                   || IS_NAMED_PRIM(rator, "unsafe-unbox*")) {
          if (SAME_OBJ(scheme_box_proc, app2->rator)) {
            /* (unbox (box X)) */
            alt = ensure_single_value_noncm(app2->rand, info);
            return replace_tail_inside(alt, inside, app->rand);
          }
        }
        break;
      }
    case scheme_application3_type:
      {
        Scheme_App3_Rec *app3 = (Scheme_App3_Rec *)rand;
        if (IS_NAMED_PRIM(rator, "car")
            || IS_NAMED_PRIM(rator, "unsafe-car")) {
          if (SAME_OBJ(scheme_cons_proc, app3->rator)
              || SAME_OBJ(scheme_unsafe_cons_list_proc, app3->rator)
              || SAME_OBJ(scheme_list_proc, app3->rator)
              || SAME_OBJ(scheme_list_star_proc, app3->rator)) {
            /* (car ({cons|list|list*} X Y)) */
            alt = make_discarding_reverse_sequence(app3->rand2, app3->rand1, info);
            return replace_tail_inside(alt, inside, app->rand);
          }
        } else if (IS_NAMED_PRIM(rator, "cdr")
                   || IS_NAMED_PRIM(rator, "unsafe-cdr")) {
          if (SAME_OBJ(scheme_cons_proc, app3->rator)
              || SAME_OBJ(scheme_unsafe_cons_list_proc, app3->rator)
              || SAME_OBJ(scheme_list_star_proc, app3->rator)) {
            /* (cdr ({cons|list*} X Y)) */
            alt = make_discarding_sequence(app3->rand1, app3->rand2, info);
            return replace_tail_inside(alt, inside, app->rand);
          } else if (SAME_OBJ(scheme_list_proc, app3->rator)) {
            /* (cdr (list X Y)) */
            alt = make_application_2(scheme_list_proc, app3->rand2, info);
            SCHEME_APPN_FLAGS(((Scheme_App_Rec *)alt)) |= (APPN_FLAG_IMMED | APPN_FLAG_SFS_TAIL);
            alt = make_discarding_sequence(app3->rand1, alt, info);
            return replace_tail_inside(alt, inside, app->rand);
          }
        } else if (IS_NAMED_PRIM(rator, "cadr")) {
          if (SAME_OBJ(scheme_list_proc, app3->rator)) {
            /* (cadr (list X Y)) */
            alt = make_discarding_sequence(app3->rand1, app3->rand2, info);
            return replace_tail_inside(alt, inside, app->rand);
          }
        }
        break;
      }
    case scheme_application_type:
      {
        Scheme_App_Rec *appr = (Scheme_App_Rec *)rand;
        Scheme_Object *r = appr->args[0];
        if (IS_NAMED_PRIM(rator, "car")
            || IS_NAMED_PRIM(rator, "unsafe-car")) {
          if ((appr->args > 0)
              && (SAME_OBJ(scheme_list_proc, r)
                  || SAME_OBJ(scheme_list_star_proc, r))) {
            /* (car ({list|list*} X Y ...)) */
            alt = make_discarding_app_sequence(appr, 0, NULL, info);
            return replace_tail_inside(alt, inside, app->rand);
          }
        } else if (IS_NAMED_PRIM(rator, "cdr")
                   || IS_NAMED_PRIM(rator, "unsafe-cdr")) {
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
            alt = make_discarding_sequence(appr->args[1], alt, info);
            return replace_tail_inside(alt, inside, app->rand);
          }
        }
        break;
      }
    }

    alt = try_reduce_predicate(rator, rand, info);
    if (alt)
      return replace_tail_inside(alt, inside, app->rand);

    if (SAME_OBJ(scheme_struct_type_p_proc, rator)) {
      Scheme_Object *c;
      c = get_struct_proc_shape(rand, info, 0);
      if (c && ((SCHEME_PROC_SHAPE_MODE(c) & STRUCT_PROC_SHAPE_MASK)
                == STRUCT_PROC_SHAPE_STRUCT)) {
        return replace_tail_inside(scheme_true, inside, app->rand);
      }
    }

    if (SAME_OBJ(scheme_varref_const_p_proc, rator)
        && SAME_TYPE(SCHEME_TYPE(rand), scheme_varref_form_type)) {
      Scheme_Object *var = SCHEME_PTR1_VAL(rand);
      if (SAME_OBJ(var, scheme_true)) {
        return replace_tail_inside(scheme_true, inside, app->rand);
      } else if (SAME_OBJ(var, scheme_false)) {
        return replace_tail_inside(scheme_false, inside, app->rand);
      } else {
        if (var && ir_propagate_ok(var, info, 1, NULL)) {
          /* can propagate => is a constant */
          return replace_tail_inside(scheme_true, inside, app->rand);
        }
      }
    }


    if (SCHEME_PRIMP(rator) && IS_NAMED_PRIM(rator, "zero?")) {
      Scheme_Object* pred;
      Scheme_App3_Rec *new;
   
      pred = expr_implies_predicate(rand, info); 
      if (pred && SAME_OBJ(pred, scheme_fixnum_p_proc)) {
        new = (Scheme_App3_Rec *)make_application_3(scheme_unsafe_fx_eq_proc, app->rand, scheme_make_integer(0), info);
        SCHEME_APPN_FLAGS(new) |= (APPN_FLAG_IMMED | APPN_FLAG_SFS_TAIL);
        return finish_optimize_application3(new, info, context);
      }
    }

    {
      /* Try to check the argument's type, and use the unsafe versions if possible. */ 
      Scheme_Object *app_o = (Scheme_Object *)app;

      check_known_variant(info, app_o, rator, rand, "bitwise-not", scheme_fixnum_p_proc, scheme_unsafe_fxnot_proc, scheme_real_p_proc);
      check_known_variant(info, app_o, rator, rand, "fxnot", scheme_fixnum_p_proc, scheme_unsafe_fxnot_proc, scheme_real_p_proc);

      check_known(info, app_o, rator, rand, "car", scheme_pair_p_proc, scheme_unsafe_car_proc);
      check_known(info, app_o, rator, rand, "unsafe-car", scheme_pair_p_proc, NULL);
      check_known(info, app_o, rator, rand, "cdr", scheme_pair_p_proc, scheme_unsafe_cdr_proc);
      check_known(info, app_o, rator, rand, "unsafe-cdr", scheme_pair_p_proc, NULL);
      check_known(info, app_o, rator, rand, "mcar", scheme_mpair_p_proc, scheme_unsafe_mcar_proc);
      check_known(info, app_o, rator, rand, "unsafe-mcar", scheme_mpair_p_proc, NULL);
      check_known(info, app_o, rator, rand, "mcdr", scheme_mpair_p_proc, scheme_unsafe_mcdr_proc);
      check_known(info, app_o, rator, rand, "unsafe-mcdr", scheme_mpair_p_proc, NULL);
      check_known(info, app_o, rator, rand, "string-length", scheme_string_p_proc, scheme_unsafe_string_length_proc);
      check_known(info, app_o, rator, rand, "bytes-length", scheme_byte_string_p_proc, scheme_unsafe_byte_string_length_proc);
      /* It's not clear that these are useful, since a chaperone check is needed anyway: */
      check_known(info, app_o, rator, rand, "unbox", scheme_box_p_proc, scheme_unsafe_unbox_proc);
      check_known(info, app_o, rator, rand, "unsafe-unbox", scheme_box_p_proc, NULL);
      check_known(info, app_o, rator, rand, "unsafe-unbox*", scheme_box_p_proc, NULL);
      check_known(info, app_o, rator, rand, "vector-length", scheme_vector_p_proc, scheme_unsafe_vector_length_proc);

      check_known(info, app_o, rator, rand, "length", scheme_list_p_proc, scheme_true);

      check_known(info, app_o, rator, rand, "string-append", scheme_string_p_proc, scheme_true);
      check_known(info, app_o, rator, rand, "bytes-append", scheme_byte_string_p_proc, scheme_true);
      check_known(info, app_o, rator, rand, "string->immutable-string", scheme_string_p_proc, scheme_true);
      check_known(info, app_o, rator, rand, "bytes->immutable-bytes", scheme_byte_string_p_proc, scheme_true);

      check_known(info, app_o, rator, rand, "string->symbol", scheme_string_p_proc, scheme_true);
      check_known(info, app_o, rator, rand, "symbol->string", scheme_symbol_p_proc, scheme_true);
      check_known(info, app_o, rator, rand, "string->keyword", scheme_string_p_proc, scheme_true);
      check_known(info, app_o, rator, rand, "keyword->string", scheme_keyword_p_proc, scheme_true);

      if (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & SCHEME_PRIM_WANTS_REAL)
        check_known(info, app_o, rator, rand, NULL, scheme_real_p_proc,
                    (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & SCHEME_PRIM_OMITTABLE_ON_GOOD_ARGS) ? scheme_true : NULL);
      if (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & SCHEME_PRIM_WANTS_NUMBER)
        check_known(info, app_o, rator, rand, NULL, scheme_number_p_proc,
                    (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & SCHEME_PRIM_OMITTABLE_ON_GOOD_ARGS) ? scheme_true : NULL);

      /* These operation don't have an unsafe replacement. Check to record types and detect errors: */
      check_known(info, app_o, rator, rand, "caar", scheme_pair_p_proc, NULL);
      check_known(info, app_o, rator, rand, "cadr", scheme_pair_p_proc, NULL);
      check_known(info, app_o, rator, rand, "cdar", scheme_pair_p_proc, NULL);
      check_known(info, app_o, rator, rand, "cddr", scheme_pair_p_proc, NULL);

      check_known(info, app_o, rator, rand, "caddr", scheme_pair_p_proc, NULL);
      check_known(info, app_o, rator, rand, "cdddr", scheme_pair_p_proc, NULL);
      check_known(info, app_o, rator, rand, "cadddr", scheme_pair_p_proc, NULL);
      check_known(info, app_o, rator, rand, "cddddr", scheme_pair_p_proc, NULL);

      check_known(info, app_o, rator, rand, "list->vector", scheme_list_p_proc, scheme_true);
      check_known(info, app_o, rator, rand, "vector->list", scheme_vector_p_proc, NULL);
      check_known(info, app_o, rator, rand, "vector->values", scheme_vector_p_proc, NULL);
      check_known(info, app_o, rator, rand, "vector->immutable-vector", scheme_vector_p_proc, NULL);
      check_known(info, app_o, rator, rand, "make-vector", scheme_fixnum_p_proc, NULL);
      
      /* Some of these may have changed app->rator. */
      rator = app->rator; 
    }
  }

  /* Using a struct getter or predicate? */
  alt = get_struct_proc_shape(rator, info, 0);
  if (alt) {
    int mode = (SCHEME_PROC_SHAPE_MODE(alt) & STRUCT_PROC_SHAPE_MASK);

    if ((mode == STRUCT_PROC_SHAPE_PRED)
        || (mode == STRUCT_PROC_SHAPE_GETTER)) {
      Scheme_Object *pred;
      pred = expr_implies_predicate(rand, info);

      if (pred
          && SAME_TYPE(SCHEME_TYPE(pred), scheme_struct_proc_shape_type)
          && is_struct_identity_subtype(SCHEME_PROC_SHAPE_IDENTITY(pred),
                                        SCHEME_PROC_SHAPE_IDENTITY(alt))) {
        if (mode == STRUCT_PROC_SHAPE_PRED) {
          /* We know that the predicate will succeed */
          return replace_tail_inside(make_discarding_sequence(rand, scheme_true, info),
                                     inside,
                                     app->rand);
        } else {
          /* Struct type matches, so use `unsafe-struct-ref` */
          Scheme_App3_Rec *new;
          new = (Scheme_App3_Rec *)make_application_3(((SCHEME_PROC_SHAPE_MODE(alt) & STRUCT_PROC_SHAPE_AUTHENTIC)
                                                       ? scheme_unsafe_struct_star_ref_proc
                                                       : scheme_unsafe_struct_ref_proc),
                                                      app->rand,
                                                      scheme_make_integer(SCHEME_PROC_SHAPE_MODE(alt) >> STRUCT_PROC_SHAPE_SHIFT),
                                                      info);
          SCHEME_APPN_FLAGS(new) |= (APPN_FLAG_IMMED | APPN_FLAG_SFS_TAIL);
          return finish_optimize_application3(new, info, context);
        }
      }

      /* Register type based on getter succeeding: */
      if ((mode == STRUCT_PROC_SHAPE_GETTER)
          && SCHEME_PAIRP(SCHEME_PROC_SHAPE_IDENTITY(alt))
          && SAME_TYPE(SCHEME_TYPE(rand), scheme_ir_local_type))
        add_type(info, rand, scheme_make_struct_proc_shape(STRUCT_PROC_SHAPE_PRED,
                                                           SCHEME_PROC_SHAPE_IDENTITY(alt)));
    }
  }

  increment_clocks_for_application(info, rator, 1);

  rator_flags = get_rator_flags(rator, 1, info);
  info->preserves_marks = !!(rator_flags & LAMBDA_PRESERVES_MARKS);
  info->single_result = !!(rator_flags & LAMBDA_SINGLE_RESULT);
  if (rator_flags & LAMBDA_RESULT_TENTATIVE) {
    info->preserves_marks = -info->preserves_marks;
    info->single_result = -info->single_result;
  }

  register_local_argument_types(NULL, app, NULL, info);

  flags = appn_flags(rator, info);
  SCHEME_APPN_FLAGS(app) |= flags;

  return finish_optimize_any_application((Scheme_Object *)app, rator, 1, info, context);
}

static Scheme_Object *optimize_application3(Scheme_Object *o, Optimize_Info *info, int context)
{
  Scheme_App3_Rec *app;
  Scheme_Object *le;
  int rator_apply_escapes, sub_context, ty, flags;
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

  le = call_with_immed_mark(app->rator, app->rand1, app->rand2, NULL, info);
  if (le)
    return scheme_optimize_expr(le, info, context);

  le = check_app_let_rator(o, app->rator, info, 2, context);
  if (le) 
    return le;

  le = optimize_for_inline(info, app->rator, 2, NULL, NULL, app, context, 0);
  if (le)
    return le;

  optimize_info_seq_init(info, &info_seq);

  sub_context = OPT_CONTEXT_SINGLED;

  le = scheme_optimize_expr(app->rator, info, sub_context);
  app->rator = le;
  if (info->escapes) {
    optimize_info_seq_done(info, &info_seq);
    return ensure_noncm(app->rator, info);
  }

  {
    /* Maybe found "((lambda" after optimizing; try again */
    le = optimize_for_inline(info, app->rator, 2, NULL, NULL, app, context, 1);
    if (le)
      return le;
    rator_apply_escapes = info->escapes;
  }

  if (SAME_OBJ(app->rator, scheme_values_proc)
      || SAME_OBJ(app->rator, scheme_apply_proc))
    info->maybe_values_argument = 1;

  /* 1st arg */

  ty = wants_local_type_arguments(app->rator, 0);
  if (ty)
    sub_context |= (ty << OPT_CONTEXT_TYPE_SHIFT);

  optimize_info_seq_step(info, &info_seq);

  le = scheme_optimize_expr(app->rand1, info, sub_context);
  app->rand1 = le;
  if (info->escapes) {
    info->size += 1;
    return ensure_noncm(make_discarding_first_sequence(app->rator, app->rand1, info), info);
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
    le = make_discarding_first_sequence(app->rator,
                                        make_discarding_first_sequence(app->rand1, app->rand2,
                                                                       info),
                                        info);
    return ensure_noncm(le, info);
  }

  /* Check for (apply ... (list ...)) after some optimizations: */
  le = direct_apply((Scheme_Object *)app, app->rator, app->rand2, info);
  if (le) return finish_optimize_app(le, info, context);

  flags = appn_flags(app->rator, info);
  SCHEME_APPN_FLAGS(app) |= flags;

  if (rator_apply_escapes) {
   info->escapes = 1;
   SCHEME_APPN_FLAGS(app) |= (APPN_FLAG_IMMED | APPN_FLAG_SFS_TAIL);
  }

  return finish_optimize_application3(app, info, context);
}

static Scheme_Object *finish_optimize_application3(Scheme_App3_Rec *app, Optimize_Info *info, int context)
{
  int flags, rator_flags;
  Scheme_Object *le;
  int all_vals = 1;

  info->size += 1;
  info->preserves_marks = 1;
  info->single_result = 1;

  if (SCHEME_TYPE(app->rand1) < _scheme_ir_values_types_)
    all_vals = 0;
  if (SCHEME_TYPE(app->rand2) < _scheme_ir_values_types_)
    all_vals = 0;


  if (all_vals) {
    le = try_optimize_fold(app->rator, NULL, (Scheme_Object *)app, info);
    if (le)
      return le;
  }

  /* Check for (call-with-values (lambda () M) N): */
  if (SAME_OBJ(app->rator, scheme_call_with_values_proc)) {
    if (SAME_TYPE(SCHEME_TYPE(app->rand1), scheme_ir_lambda_type)) {
      Scheme_Lambda *lam = (Scheme_Lambda *)app->rand1;

      if (!lam->num_params) {
        /* Convert to apply-values form: */
        return optimize_apply_values(app->rand2, lam->body, info,
                                     ((SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_SINGLE_RESULT)
                                       ? ((SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_RESULT_TENTATIVE)
                                          ? -1
                                          : 1)
                                       : 0),
                                     context);
      }
    }
  }

  if (SAME_OBJ(scheme_procedure_arity_includes_proc, app->rator)) {
    if (SCHEME_INTP(app->rand2) && SCHEME_INT_VAL(app->rand2) >= 0) {
      Scheme_Object *proc;

      proc = lookup_constant_proc(info, app->rand1, SCHEME_INT_VAL(app->rand2));
      if (proc) {
        info->preserves_marks = 1;
        info->single_result = 1;
        return make_discarding_sequence(app->rand1,
                                        SAME_OBJ(proc, scheme_true) ? scheme_false : scheme_true,
                                        info);
      }
    }
  }

  if (SAME_OBJ(app->rator, scheme_equal_proc)
       || SAME_OBJ(app->rator, scheme_eqv_proc)
       || SAME_OBJ(app->rator, scheme_eq_proc)) {
    if (equivalent_exprs(app->rand1, app->rand2, NULL, NULL, 0)) {
      return make_discarding_sequence_3(app->rand1, app->rand2, scheme_true, info);
    }
    {
      Scheme_Object *pred1, *pred2, *pred_new = NULL;
      int rel1=0, rel2=0, rel_max, eq_type=0;
       
      pred1 = expr_implies_predicate(app->rand1, info);
      pred2 = expr_implies_predicate(app->rand2, info);
      rel1 = relevant_predicate(pred1);
      rel2 = relevant_predicate(pred2);
      if ((pred1 && pred2)
          && (predicate_implies_not(pred1, pred2)
              || predicate_implies_not(pred2, pred1))) {
        info->preserves_marks = 1;
        info->single_result = 1;
        return make_discarding_sequence_3(app->rand1, app->rand2, scheme_false, info);
      }

      /* Try to transform it into a predicate */
      if (rel1 >= RLV_SINGLETON) {
        Scheme_Object *new_app;
        new_app = make_optimize_prim_application2(pred1, app->rand2, info, context);
        return make_discarding_sequence(app->rand1, new_app, info);
      }
      if (rel2 >= RLV_SINGLETON) {
        Scheme_Object *new_app;
        new_app = make_optimize_prim_application2(pred2, app->rand1, info, context);
        return make_discarding_reverse_sequence(app->rand2, new_app, info);
      }

      /* Optimize `equal?' or `eqv?' test on certain types
         to `eqv?` or `eq?'. This is especially helpful for the JIT. */
      if (SAME_OBJ(app->rator, scheme_eqv_proc))
        eq_type = RLV_EQV_TESTEABLE;
      if (SAME_OBJ(app->rator, scheme_eq_proc))
        eq_type = RLV_EQ_TESTEABLE;

      rel_max = (rel1 >= rel2) ? rel1 : rel2;
      if (rel_max >= RLV_EQ_TESTEABLE && eq_type < RLV_EQ_TESTEABLE)
        pred_new = scheme_eq_proc;
      else if (rel_max >= RLV_EQV_TESTEABLE && eq_type < RLV_EQV_TESTEABLE)
        pred_new = scheme_eqv_proc;
      
      if (pred_new) {
        app->rator = pred_new;
        SCHEME_APPN_FLAGS(app) |= (APPN_FLAG_IMMED | APPN_FLAG_SFS_TAIL);

        /* eq? and eqv? are foldable */
        if (all_vals) {
          le = try_optimize_fold(app->rator, NULL, (Scheme_Object *)app, info);
          if (le)
            return le;
        }
      }
    }
  }

  /* Ad hoc optimization of (unsafe-+ <x> 0), etc. */
  if (SCHEME_PRIMP(app->rator)
      && (SCHEME_PRIM_PROC_OPT_FLAGS(app->rator) & SCHEME_PRIM_IS_UNSAFE_NONMUTATING)) {
    int z1, z2;

    z1 = SAME_OBJ(app->rand1, scheme_make_integer(0));
    z2 = SAME_OBJ(app->rand2, scheme_make_integer(0));
    if (IS_NAMED_PRIM(app->rator, "unsafe-fx+")) {
      if (z1)
        return ensure_single_value_noncm(app->rand2, info);
      else if (z2)
        return ensure_single_value_noncm(app->rand1, info);
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-fx-")) {
      if (z2)
        return ensure_single_value_noncm(app->rand1, info);
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-fx*")) {
      if (z1 || z2) {
        if (z1 && z2)
          return scheme_make_integer(0);
        else if (z2)
          return make_discarding_sequence(app->rand1, scheme_make_integer(0), info);
        else
          return make_discarding_sequence(app->rand2, scheme_make_integer(0), info);
      }
      if (SAME_OBJ(app->rand1, scheme_make_integer(1)))
        return ensure_single_value_noncm(app->rand2, info);
      if (SAME_OBJ(app->rand2, scheme_make_integer(1)))
        return ensure_single_value_noncm(app->rand1, info);
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-fxquotient")) {
      if (z1)
        return make_discarding_sequence(app->rand2, scheme_make_integer(0), info);
      if (SAME_OBJ(app->rand2, scheme_make_integer(1)))
        return ensure_single_value_noncm(app->rand1, info);
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-fxremainder")
               || IS_NAMED_PRIM(app->rator, "unsafe-fxmodulo")) {
      if (z1)
        return make_discarding_sequence(app->rand2, scheme_make_integer(0), info);
      if (SAME_OBJ(app->rand2, scheme_make_integer(1)))
        return make_discarding_sequence(app->rand1, scheme_make_integer(0), info);
    }

    z1 = (SCHEME_FLOATP(app->rand1) && (SCHEME_FLOAT_VAL(app->rand1) == 0.0));
    z2 = (SCHEME_FLOATP(app->rand2) && (SCHEME_FLOAT_VAL(app->rand2) == 0.0));

    if (IS_NAMED_PRIM(app->rator, "unsafe-fl+")) {
      if (z1)
        return ensure_single_value_noncm(app->rand2, info);
      else if (z2)
        return ensure_single_value_noncm(app->rand1, info);
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-fl-")) {
      if (z2)
        return ensure_single_value_noncm(app->rand1, info);
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-fl*")) {
      if (SCHEME_FLOATP(app->rand1) && (SCHEME_FLOAT_VAL(app->rand1) == 1.0))
        return ensure_single_value_noncm(app->rand2, info);
      if (SCHEME_FLOATP(app->rand2) && (SCHEME_FLOAT_VAL(app->rand2) == 1.0))
        return ensure_single_value_noncm(app->rand1, info);
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-fl/")) {
      if (SCHEME_FLOATP(app->rand2) && (SCHEME_FLOAT_VAL(app->rand2) == 1.0))
        return ensure_single_value_noncm(app->rand1, info);
    }

    /* Possible improvement: detect 0 and 1 constants even when general
       extflonum operations are not supported. */
#ifdef MZ_LONG_DOUBLE
    z1 = (SCHEME_LONG_DBLP(app->rand1) && long_double_is_zero(SCHEME_LONG_DBL_VAL(app->rand1)));
    z2 = (SCHEME_LONG_DBLP(app->rand2) && long_double_is_zero(SCHEME_LONG_DBL_VAL(app->rand2)));

    if (IS_NAMED_PRIM(app->rator, "unsafe-extfl+")) {
      if (z1)
        return ensure_single_value_noncm(app->rand2, info);
      else if (z2)
        return ensure_single_value_noncm(app->rand1, info);
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-extfl-")) {
      if (z2)
        return ensure_single_value_noncm(app->rand1, info);
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-extfl*")) {
      if (SCHEME_LONG_DBLP(app->rand1) && long_double_is_1(SCHEME_LONG_DBL_VAL(app->rand1)))
        return ensure_single_value_noncm(app->rand2, info);
      if (SCHEME_LONG_DBLP(app->rand2) && long_double_is_1(SCHEME_LONG_DBL_VAL(app->rand2)))
        return ensure_single_value_noncm(app->rand1, info);
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-extfl/")) {
      if (SCHEME_LONG_DBLP(app->rand2) && long_double_is_1(SCHEME_LONG_DBL_VAL(app->rand2)))
        return ensure_single_value_noncm(app->rand1, info);
    }
#endif
  } else if (SCHEME_PRIMP(app->rator)
             && (SCHEME_PRIM_PROC_OPT_FLAGS(app->rator) & SCHEME_PRIM_IS_BINARY_INLINED)) {
    if (IS_NAMED_PRIM(app->rator, "arithmetic-shift")) {
      if (SCHEME_INTP(app->rand2) && (SCHEME_INT_VAL(app->rand2) <= 0)
          && (is_local_type_expression(app->rand1, info) == SCHEME_LOCAL_TYPE_FIXNUM)) {
        app->rator = scheme_unsafe_fxrshift_proc;
        app->rand2 = scheme_make_integer(-(SCHEME_INT_VAL(app->rand2)));
      }
    } else if (IS_NAMED_PRIM(app->rator, "string=?")) {
      if (SAME_TYPE(SCHEME_TYPE(app->rand1), scheme_char_string_type)
          && SAME_TYPE(SCHEME_TYPE(app->rand2), scheme_char_string_type)) {
        return scheme_string_eq_2(app->rand1, app->rand2);
      }
    } else if (IS_NAMED_PRIM(app->rator, "bytes=?")) {
      if (SAME_TYPE(SCHEME_TYPE(app->rand1), scheme_byte_string_type)
          && SAME_TYPE(SCHEME_TYPE(app->rand2), scheme_byte_string_type)) {
        return scheme_byte_string_eq_2(app->rand1, app->rand2);
      }
    } else if (IS_NAMED_PRIM(app->rator, "char=?")) {
      if (SAME_TYPE(SCHEME_TYPE(app->rand1), scheme_char_type)
          && SAME_TYPE(SCHEME_TYPE(app->rand2), scheme_char_type)) {
        return (SCHEME_CHAR_VAL(app->rand1) == SCHEME_CHAR_VAL(app->rand2)) ? scheme_true : scheme_false;
      }
    }
  }

  if (SCHEME_PRIMP(app->rator)) {
    Scheme_Object *app_o = (Scheme_Object *)app, *rator = app->rator, *rand1 = app->rand1, *rand2 = app->rand2;
    
    check_known_both_variant(info, app_o, rator, rand1, rand2, "bitwise-and", scheme_fixnum_p_proc, scheme_unsafe_fxand_proc, scheme_real_p_proc);
    check_known_both_variant(info, app_o, rator, rand1, rand2, "bitwise-ior", scheme_fixnum_p_proc, scheme_unsafe_fxior_proc, scheme_real_p_proc);
    check_known_both_variant(info, app_o, rator, rand1, rand2, "bitwise-xor", scheme_fixnum_p_proc, scheme_unsafe_fxxor_proc, scheme_real_p_proc);

    check_known_both_variant(info, app_o, rator, rand1, rand2, "fxand", scheme_fixnum_p_proc, scheme_unsafe_fxand_proc, scheme_real_p_proc);
    check_known_both_variant(info, app_o, rator, rand1, rand2, "fxior", scheme_fixnum_p_proc, scheme_unsafe_fxior_proc, scheme_real_p_proc);
    check_known_both_variant(info, app_o, rator, rand1, rand2, "fxxor", scheme_fixnum_p_proc, scheme_unsafe_fxxor_proc, scheme_real_p_proc);

    check_known_both_try(info, app_o, rator, rand1, rand2, "=", scheme_fixnum_p_proc, scheme_unsafe_fx_eq_proc);
    check_known_both_try(info, app_o, rator, rand1, rand2, "<", scheme_fixnum_p_proc, scheme_unsafe_fx_lt_proc);
    check_known_both_try(info, app_o, rator, rand1, rand2, ">", scheme_fixnum_p_proc, scheme_unsafe_fx_gt_proc);
    check_known_both_try(info, app_o, rator, rand1, rand2, "<=", scheme_fixnum_p_proc, scheme_unsafe_fx_lt_eq_proc);
    check_known_both_try(info, app_o, rator, rand1, rand2, ">=", scheme_fixnum_p_proc, scheme_unsafe_fx_gt_eq_proc);
    check_known_both_try(info, app_o, rator, rand1, rand2, "min", scheme_fixnum_p_proc, scheme_unsafe_fx_min_proc);
    check_known_both_try(info, app_o, rator, rand1, rand2, "max", scheme_fixnum_p_proc, scheme_unsafe_fx_max_proc);

    check_known_both_try(info, app_o, rator, rand1, rand2, "fx=", scheme_fixnum_p_proc, scheme_unsafe_fx_eq_proc);
    check_known_both_try(info, app_o, rator, rand1, rand2, "fx<", scheme_fixnum_p_proc, scheme_unsafe_fx_lt_proc);
    check_known_both_try(info, app_o, rator, rand1, rand2, "fx>", scheme_fixnum_p_proc, scheme_unsafe_fx_gt_proc);
    check_known_both_try(info, app_o, rator, rand1, rand2, "fx<=", scheme_fixnum_p_proc, scheme_unsafe_fx_lt_eq_proc);
    check_known_both_try(info, app_o, rator, rand1, rand2, "fx>=", scheme_fixnum_p_proc, scheme_unsafe_fx_gt_eq_proc);
    check_known_both_try(info, app_o, rator, rand1, rand2, "fxmin", scheme_fixnum_p_proc, scheme_unsafe_fx_min_proc);
    check_known_both_try(info, app_o, rator, rand1, rand2, "fxmax", scheme_fixnum_p_proc, scheme_unsafe_fx_max_proc);

    rator = app->rator; /* in case it was updated */

    check_known_both(info, app_o, rator, rand1, rand2, "string-append", scheme_string_p_proc, scheme_true);
    check_known_both(info, app_o, rator, rand1, rand2, "bytes-append", scheme_byte_string_p_proc, scheme_true);
    check_known(info, app_o, rator, rand1, "string-ref", scheme_string_p_proc, NULL);
    check_known(info, app_o, rator, rand2, "string-ref", scheme_fixnum_p_proc, NULL);
    check_known(info, app_o, rator, rand1, "bytes-ref", scheme_byte_string_p_proc, NULL);
    check_known(info, app_o, rator, rand2, "bytes-ref", scheme_fixnum_p_proc, NULL);

    check_known(info, app_o, rator, rand1, "append", scheme_list_p_proc, scheme_true);
    check_known(info, app_o, rator, rand1, "list-ref", scheme_pair_p_proc, NULL);
    check_known(info, app_o, rator, rand2, "list-ref", scheme_fixnum_p_proc, NULL);

    if (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & SCHEME_PRIM_WANTS_REAL)
      check_known_both(info, app_o, rator, rand1, rand2, NULL, scheme_real_p_proc,
                       (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & SCHEME_PRIM_OMITTABLE_ON_GOOD_ARGS) ? scheme_true : NULL);
    if (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & SCHEME_PRIM_WANTS_NUMBER)
      check_known_both(info, app_o, rator, rand1, rand2, NULL, scheme_number_p_proc,
                       (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & SCHEME_PRIM_OMITTABLE_ON_GOOD_ARGS) ? scheme_true : NULL);

    check_known(info, app_o, rator, rand1, "vector-ref", scheme_vector_p_proc, NULL);
    check_known(info, app_o, rator, rand2, "vector-ref", scheme_fixnum_p_proc, NULL);
    check_known(info, app_o, rator, rand1, "make-vector", scheme_fixnum_p_proc, NULL);

    check_known(info, app_o, rator, rand1, "set-box!", scheme_box_p_proc, NULL);
    check_known(info, app_o, rator, rand1, "unsafe-set-box!", scheme_box_p_proc, NULL);
    check_known(info, app_o, rator, rand1, "unsafe-set-box*!", scheme_box_p_proc, NULL);

    check_known(info, app_o, rator, rand1, "procedure-closure-contents-eq?", scheme_procedure_p_proc, NULL);
    check_known(info, app_o, rator, rand2, "procedure-closure-contents-eq?", scheme_procedure_p_proc, NULL);
    check_known(info, app_o, rator, rand1, "procedure-arity-includes?", scheme_procedure_p_proc, NULL);
    
    check_known(info, app_o, rator, rand1, "map", scheme_procedure_p_proc, NULL);
    check_known(info, app_o, rator, rand1, "for-each", scheme_procedure_p_proc, NULL);
    check_known(info, app_o, rator, rand1, "andmap", scheme_procedure_p_proc, NULL);
    check_known(info, app_o, rator, rand1, "ormap", scheme_procedure_p_proc, NULL);
    check_known(info, app_o, rator, rand2, "map", scheme_list_p_proc, NULL);
    check_known(info, app_o, rator, rand2, "for-each", scheme_list_p_proc, NULL);
    check_known(info, app_o, rator, rand2, "andmap", scheme_list_p_proc, NULL);
    check_known(info, app_o, rator, rand2, "ormap", scheme_list_p_proc, NULL);

    rator = app->rator; /* in case it was updated */
  }
  
  increment_clocks_for_application(info, app->rator, 2);

  rator_flags = get_rator_flags(app->rator, 2, info);
  info->preserves_marks = !!(rator_flags & LAMBDA_PRESERVES_MARKS);
  info->single_result = !!(rator_flags & LAMBDA_SINGLE_RESULT);
  if (rator_flags & LAMBDA_RESULT_TENTATIVE) {
    info->preserves_marks = -info->preserves_marks;
    info->single_result = -info->single_result;
  }

  register_local_argument_types(NULL, NULL, app, info);

  flags = appn_flags(app->rator, info);
  SCHEME_APPN_FLAGS(app) |= flags;

  return finish_optimize_any_application((Scheme_Object *)app, app->rator, 2,
                                         info, context);
}

/*========================================================================*/
/*                   the apply-values bytecode form                       */
/*========================================================================*/

Scheme_Object *optimize_apply_values(Scheme_Object *f, Scheme_Object *e,
                                     Optimize_Info *info,
                                     int e_single_result,
                                     int context)
/* f and e are already optimized */
{
  Scheme_Object *o_f;

  info->preserves_marks = 0;
  info->single_result = 0;

  o_f = lookup_constant_proc(info, f, (e_single_result > 0) ? 1 : -1);
  if (o_f) {
    if (SAME_TYPE(SCHEME_TYPE(o_f), scheme_ir_lambda_type)) {
      Scheme_Lambda *lam = (Scheme_Lambda *)o_f;
      int flags = SCHEME_LAMBDA_FLAGS(lam);
      info->preserves_marks = !!(flags & LAMBDA_PRESERVES_MARKS);
      info->single_result = !!(flags & LAMBDA_SINGLE_RESULT);
      if (flags & LAMBDA_RESULT_TENTATIVE) {
        info->preserves_marks = -info->preserves_marks;
        info->single_result = -info->single_result;
      }
    }
  }

  if (o_f && (e_single_result > 0)) {
    /* Just make it an application (N M): */
    Scheme_App2_Rec *app2;
    Scheme_Object *e_cloned, *f_cloned;

    app2 = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
    app2->iso.so.type = scheme_application2_type;

    /* Try to inline... */

    e_cloned = optimize_clone(1, e, info, empty_eq_hash_tree, 0);
    if (e_cloned) {
      if (SAME_TYPE(SCHEME_TYPE(f), scheme_ir_lambda_type))
        f_cloned = optimize_clone(1, f, info, empty_eq_hash_tree, 0);
      else {
        /* Otherwise, no clone is needed. */
        f_cloned = f;
      }

      if (f_cloned) {
        app2->rator = f_cloned;
        app2->rand = e_cloned;
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

/*========================================================================*/
/*                             begin and begin0                           */
/*========================================================================*/

static Scheme_Object *optimize_sequence(Scheme_Object *o, Optimize_Info *info, int context, int sub_opt);

static Scheme_Object *flatten_sequence(Scheme_Object *o, Optimize_Info *info, int context)
{
  Scheme_Sequence *s = (Scheme_Sequence *)o, *s2, *s3;
  Scheme_Object *o3;
  int i, j, k, count, extra = 0, split = 0, b0, new_count;

  if (SAME_TYPE(SCHEME_TYPE(o), scheme_splice_sequence_type))
    return o;
  
  if (!info->flatten_fuel)
    return o;

  b0 = SAME_TYPE(SCHEME_TYPE(o), scheme_begin0_sequence_type);
  count = s->count;
  
  /* exceptions: (begin ... (begin0 ...)) and (begin0 (begin ...) ...) */
  for (i = 0; i < count; i++) {
    o3 = s->array[i];
    if ((SAME_TYPE(SCHEME_TYPE(o3), scheme_sequence_type) && !(!i && b0))
        || (SAME_TYPE(SCHEME_TYPE(o3), scheme_begin0_sequence_type) && !(i == count - 1 && !b0))) {
      s3 = (Scheme_Sequence *)o3;
      extra += s3->count;
      split++;
    }
  }

  if (!split)
    return o;
  
  info->flatten_fuel--;
  info->size -= split;

  new_count = s->count + extra - split;
  if (new_count > 0) {
    s2 = scheme_malloc_sequence(new_count);
    s2->so.type = s->so.type;
    s2->count = new_count;
  } else
    s2 = NULL;
  k = 0;
  
  /* exceptions: (begin ... (begin0 ...)) and (begin0 (begin ...) ...) */
  for (i = 0; i < count; i++) {
    o3 = s->array[i];
    if ((SAME_TYPE(SCHEME_TYPE(o3), scheme_sequence_type) && !(!i && b0))
        || (SAME_TYPE(SCHEME_TYPE(o3), scheme_begin0_sequence_type) && !(i == count - 1 && !b0))) {
      s3 = (Scheme_Sequence *)o3;
      for (j = 0; j < s3->count; j++) {
        s2->array[k++] = s3->array[j];
      }
    } else {
      s2->array[k++] = o3;
    }
  }

  MZ_ASSERT(k == new_count);

  if (s2->count == 1)
    return s2->array[0];

  if (SAME_TYPE(SCHEME_TYPE(s2), scheme_sequence_type))
    return optimize_sequence((Scheme_Object *)s2, info, context, 0);
  else
    return (Scheme_Object *)s2;
}

static Scheme_Object *optimize_sequence(Scheme_Object *o, Optimize_Info *info, int context, int sub_opt)
{
  Scheme_Sequence *s = (Scheme_Sequence *)o;
  Scheme_Object *le;
  int i, count, prev_size;
  int drop = 0, preserves_marks = 0, single_result = 0;
  Optimize_Info_Sequence info_seq;

  /* If !sub_opt, then just inspect already-optimized results. Note
     that `info` doesn't change in this mode, so we shouldn't try to
     check whether an expression escapes, for example. */

  if (sub_opt)
    optimize_info_seq_init(info, &info_seq);
  else
    memset(&info_seq, 0, sizeof(info_seq));
  
  count = s->count;
  for (i = 0; i < count; i++) {
    prev_size = info->size;

    if (sub_opt) {
      optimize_info_seq_step(info, &info_seq);
      le = scheme_optimize_expr(s->array[i], info,
                                ((i + 1 == count)
                                 ? scheme_optimize_tail_context(context)
                                 : 0));
    } else
      le = s->array[i];

    if (i + 1 == count) {
      single_result = info->single_result;
      preserves_marks = info->preserves_marks;
      s->array[i] = le;
    } else {
      if (!sub_opt || !info->escapes) {
        /* Inlining and constant propagation can expose omittable expressions. */
        le = optimize_ignored(le, info, -1, 1, 5);
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
        /* Move to last position in case the begin form is dropped */
        s->array[count - 1] = le;
        for (j = i; j < count - 1; j++) {
          drop++;
          s->array[j] = NULL;
        }
        break;
      }
    }
  }

  if (sub_opt)
    optimize_info_seq_done(info, &info_seq);

  info->preserves_marks = preserves_marks;
  info->single_result = single_result;

  if (drop + 1 == s->count) {
    le = s->array[drop];
    if (info->escapes)
      le = ensure_noncm(le, info);
    return le;
  }

  if (drop) {
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

  return flatten_sequence((Scheme_Object *)s, info, context);
}

/*========================================================================*/
/*                      conditionals and types                            */
/*========================================================================*/

static Scheme_Object *collapse_local(Scheme_Object *var, Optimize_Info *info, int context)
/* Replace `var` in the given context with a constant, if possible based on its type  */
{
  if (!SCHEME_VAR(var)->mutated) {
    Scheme_Object *pred;

    pred = expr_implies_predicate(var, info);
    if (pred) {
      if (predicate_implies(pred, scheme_not_proc))
        return scheme_false;

      if (context & OPT_CONTEXT_BOOLEAN) {
        if (predicate_implies_not(pred, scheme_not_proc))
          return scheme_true;
      }

      if (SAME_OBJ(pred, scheme_true_object_p_proc))
        return scheme_true;
      if (SAME_OBJ(pred, scheme_null_p_proc))
        return scheme_null;
      if (SAME_OBJ(pred, scheme_void_p_proc))
        return scheme_void;
      if (SAME_OBJ(pred, scheme_eof_object_p_proc))
        return scheme_eof;
    }
  }
  return NULL;
}

/* This function is used to reduce: 
   (if <x> a b) => (begin <x> <result-a-or-b>)
   (if a b #f) => a , and similar
   (eq? a b) => (begin a b #t)
   The function considers only values and variable references, so <a> and <b> don't have side effects.
   But each reduction has a very different behavior for expressions with side effects. */
static Scheme_Object *equivalent_exprs(Scheme_Object *a, Scheme_Object *b,
                                       Optimize_Info *a_info, Optimize_Info *b_info, int context)
{
  if (SAME_OBJ(a, b))
    return a;

  if (SAME_TYPE(SCHEME_TYPE(a), scheme_ir_toplevel_type)
      && SAME_TYPE(SCHEME_TYPE(b), scheme_ir_toplevel_type)
      && (SCHEME_TOPLEVEL_POS(a) == SCHEME_TOPLEVEL_POS(b)))
    return a;

  if (b_info 
      && SAME_TYPE(SCHEME_TYPE(a), scheme_ir_local_type)
      && (SCHEME_TYPE(b) > _scheme_ir_values_types_)) {
    Scheme_Object *n;
    n = collapse_local(a, b_info, context);
    if (n && SAME_OBJ(n, b))
      return a;
  }

  if (a_info 
      && SAME_TYPE(SCHEME_TYPE(b), scheme_ir_local_type)
      && (SCHEME_TYPE(a) > _scheme_ir_values_types_)) {
    Scheme_Object *n;
    n = collapse_local(b, a_info, context);
    if (n && SAME_OBJ(n, a))
      return b;
  }

  return NULL;
}

static void add_type(Optimize_Info *info, Scheme_Object *var, Scheme_Object *pred)
/* This is conceptually an intersection, but `Any` is represented by a
   missing entry, so the implementation looks like an union. */
{
  Scheme_Hash_Tree *new_types = info->types;
  Scheme_Object *old_pred;
  
  if (SCHEME_VAR(var)->mutated)
    return;

  /* Don't add the type if something is already there, which may happen when no_types,
     as long as the existing predicate implies the new one. */
  if (SCHEME_VAR(var)->val_type) /* => more specific than other predicates */
    return;
  old_pred = optimize_get_predicate(info, var, 1);
  if (old_pred && predicate_implies(old_pred, pred))
    return;

  /* special case: list? and pair? => list-pair? */
  if (old_pred) {
    if ((SAME_OBJ(old_pred, scheme_list_p_proc)
         && (SAME_OBJ(pred, scheme_pair_p_proc)))
        || (SAME_OBJ(old_pred, scheme_pair_p_proc)
            && (SAME_OBJ(pred, scheme_list_p_proc)))) {
      pred = scheme_list_pair_p_proc;
    }
  }

  if (!new_types)
    new_types = scheme_make_hash_tree(SCHEME_hashtr_eq);
  new_types = scheme_hash_tree_set(new_types, var, pred);
  info->types = new_types;
}

static void add_type_no(Optimize_Info *info, Scheme_Object *var, Scheme_Object *pred)
/* Currently only check a few special cases for lists and booleans. */
{
  Scheme_Object *old_pred;
  
  if (SCHEME_VAR(var)->mutated)
    return;

  old_pred = optimize_get_predicate(info, var, 1);

  if (old_pred && SAME_OBJ(old_pred, scheme_list_p_proc)) {
    /* list? but not null? => list-pair? */
    if (SAME_OBJ(pred, scheme_null_p_proc))
      add_type(info, var, scheme_list_pair_p_proc);

    /* list? but not pair? => null? */
    /* list? but not list-pair? => null? */
    if (SAME_OBJ(pred, scheme_pair_p_proc)
        ||SAME_OBJ(pred, scheme_list_pair_p_proc))
      add_type(info, var, scheme_null_p_proc);
  }

  if (old_pred && SAME_OBJ(old_pred, scheme_boolean_p_proc)) {
    /* boolean? but not `not` => true-object? */
    if (SAME_OBJ(pred, scheme_not_proc))
      add_type(info, var, scheme_true_object_p_proc);

    /* boolean? but not true-object? => `not` */
    if (SAME_OBJ(pred, scheme_true_object_p_proc))
      add_type(info, var, scheme_not_proc);
  }
}



static void merge_types(Optimize_Info *src_info, Optimize_Info *info, Scheme_Hash_Tree *skip_vars)
{
  Scheme_Hash_Tree *types = src_info->types;
  Scheme_Object *var, *pred;
  intptr_t i;

  if (!types)
    return;

  if (skip_vars) {
    /* Remove variables from `types` that we're supposed to skip */
    i = scheme_hash_tree_next(skip_vars, -1);
    while (i != -1) {
      scheme_hash_tree_index(types, i, &var, NULL);
      scheme_hash_tree_set(types, var, NULL);
      i = scheme_hash_tree_next(skip_vars, i);
    }
  }

  if (!info->types || (types->count > info->types->count)) {
    /* It will be faster to merge the old table into the new one: */
    Scheme_Hash_Tree *old_types = info->types;
    info->types = types;
    if (!old_types)
      return;
    types = old_types;
  }
  
  i = scheme_hash_tree_next(types, -1);
  while (i != -1) {
    scheme_hash_tree_index(types, i, &var, &pred);
    add_type(info, var, pred);
    i = scheme_hash_tree_next(types, i);
  }
}

static void merge_branchs_types(Optimize_Info *t_info, Optimize_Info *f_info,
                                      Optimize_Info *base_info)
/* This is conceptually an union, but `Any` is represented by a
   missing entry, so the implementation looks like an intersection.
   This adds to base_info the "intersection" of the types of t_info and f_info */
{
  Scheme_Hash_Tree *t_types = t_info->types, *f_types = f_info->types;
  Scheme_Object *var, *t_pred, *f_pred;
  intptr_t i;

  if (!t_types || !f_types)
    return;

  if (f_types->count > t_types->count) {
    Scheme_Hash_Tree *swap = f_types;
    f_types = t_types;
    t_types = swap;
  }

  i = scheme_hash_tree_next(f_types, -1);
  while (i != -1) {
    scheme_hash_tree_index(f_types, i, &var, &f_pred);
    t_pred = scheme_hash_tree_get(t_types, var);
    if (t_pred) {
      if (predicate_implies(f_pred, t_pred))
        add_type(base_info, var, t_pred);
      else if (predicate_implies(t_pred, f_pred))
        add_type(base_info, var, f_pred);
      else {
        /* special case: null? or list-pair? => list? */
       if ((SAME_OBJ(t_pred, scheme_null_p_proc)
         && (SAME_OBJ(f_pred, scheme_list_pair_p_proc)))
        || (SAME_OBJ(t_pred, scheme_list_pair_p_proc)
            && (SAME_OBJ(f_pred, scheme_null_p_proc)))) {
        add_type(base_info, var, scheme_list_p_proc);
       }
        /* special case: true-object? or `not` => boolean? */
       if ((SAME_OBJ(t_pred, scheme_not_proc)
         && (SAME_OBJ(f_pred, scheme_true_object_p_proc)))
        || (SAME_OBJ(t_pred, scheme_true_object_p_proc)
            && (SAME_OBJ(f_pred, scheme_not_proc)))) {
        add_type(base_info, var, scheme_boolean_p_proc);
       }
      }
    }
    i = scheme_hash_tree_next(f_types, i);
  }
}

static int relevant_predicate(Scheme_Object *pred)
{
  /* Relevant predicates need to be disjoint for try_reduce_predicate(),
     finish_optimize_application3() and add_types_for_t_branch().
     The predicate_implies() and predicate_implies_not() functions must
     be kept in sync with this list. */

  if (SAME_OBJ(pred, scheme_pair_p_proc)
      || SAME_OBJ(pred, scheme_list_p_proc)
      || SAME_OBJ(pred, scheme_list_pair_p_proc)
      || SAME_OBJ(pred, scheme_mpair_p_proc)
      || SAME_OBJ(pred, scheme_box_p_proc)
      || SAME_OBJ(pred, scheme_string_p_proc)
      || SAME_OBJ(pred, scheme_byte_string_p_proc)
      || SAME_OBJ(pred, scheme_vector_p_proc)
      || SAME_OBJ(pred, scheme_procedure_p_proc)
      || SAME_OBJ(pred, scheme_syntax_p_proc)
      || SAME_OBJ(pred, scheme_extflonum_p_proc))
    return RLV_IS_RELEVANT;
  if (SAME_OBJ(pred, scheme_char_p_proc)
      || SAME_OBJ(pred, scheme_flonum_p_proc)
      || SAME_OBJ(pred, scheme_number_p_proc)
      || SAME_OBJ(pred, scheme_real_p_proc))
    return RLV_EQV_TESTEABLE;
  if (SAME_OBJ(pred, scheme_symbol_p_proc)
      || SAME_OBJ(pred, scheme_keyword_p_proc)
      || SAME_OBJ(pred, scheme_fixnum_p_proc)
      || SAME_OBJ(pred, scheme_interned_char_p_proc)
      || SAME_OBJ(pred, scheme_boolean_p_proc))
    return RLV_EQ_TESTEABLE;
  if (SAME_OBJ(pred, scheme_null_p_proc)
      || SAME_OBJ(pred, scheme_void_p_proc)
      || SAME_OBJ(pred, scheme_eof_object_p_proc)
      || SAME_OBJ(pred, scheme_true_object_p_proc)
      || SAME_OBJ(pred, scheme_not_proc))
    return RLV_SINGLETON;

  return 0;
}

static int predicate_implies(Scheme_Object *pred1, Scheme_Object *pred2)
{
  if (!pred1 || !pred2)
    return 0;
  
  /* P => P */
  if (SAME_OBJ(pred1, pred2))
    return 1;

  /* null? => list? */
  if (SAME_OBJ(pred2, scheme_list_p_proc)
      && SAME_OBJ(pred1, scheme_null_p_proc))
    return 1;

  /* list-pair? => list? */
  if (SAME_OBJ(pred2, scheme_list_p_proc)
      && SAME_OBJ(pred1, scheme_list_pair_p_proc))
    return 1;

  /* list-pair? => pair? */
  if (SAME_OBJ(pred2, scheme_pair_p_proc)
      && SAME_OBJ(pred1, scheme_list_pair_p_proc))
    return 1;

  /* interned-char? => char? */
  if (SAME_OBJ(pred2, scheme_char_p_proc)
      && SAME_OBJ(pred1, scheme_interned_char_p_proc))
    return 1;

  /* not, true-object? => boolean? */
  if (SAME_OBJ(pred2, scheme_boolean_p_proc)
      && (SAME_OBJ(pred1, scheme_not_proc)
          || SAME_OBJ(pred1, scheme_true_object_p_proc)))
    return 1;

  /* real?, fixnum?, or flonum? => number? */
  if (SAME_OBJ(pred2, scheme_number_p_proc)
      && (SAME_OBJ(pred1, scheme_real_p_proc)
          || SAME_OBJ(pred1, scheme_fixnum_p_proc)
          || SAME_OBJ(pred1, scheme_flonum_p_proc)))
    return 1;

  /* fixnum? or flonum? => real? */
  if (SAME_OBJ(pred2, scheme_real_p_proc)
      && (SAME_OBJ(pred1, scheme_fixnum_p_proc)
          || SAME_OBJ(pred1, scheme_flonum_p_proc)))
    return 1;

  /* structure subtype? */
  if (SAME_TYPE(SCHEME_TYPE(pred1), scheme_struct_proc_shape_type)
      && SAME_TYPE(SCHEME_TYPE(pred2), scheme_struct_proc_shape_type)
      && is_struct_identity_subtype(SCHEME_PROC_SHAPE_IDENTITY(pred1),
                                    SCHEME_PROC_SHAPE_IDENTITY(pred2)))
    return 1;

  return 0;
}

static int predicate_implies_not(Scheme_Object *pred1, Scheme_Object *pred2)
{
  if (SAME_OBJ(pred1, scheme_pair_p_proc) && SAME_OBJ(pred2, scheme_list_p_proc))
    return 0;
  if (SAME_OBJ(pred1, scheme_list_p_proc) && SAME_OBJ(pred2, scheme_pair_p_proc))
    return 0;

  /* we don't track structure-type identity precisely enough to know
     that structures don't rule out other structures --- or even other
     prdicates (such as `procedure?`) */
  if (SAME_TYPE(SCHEME_TYPE(pred1), scheme_struct_proc_shape_type)
      || SAME_TYPE(SCHEME_TYPE(pred2), scheme_struct_proc_shape_type))
    return 0;
  
  /* Otherwise, with our current set of predicates, overlapping matches happen
     only when one implies the other: */
  return (!predicate_implies(pred1, pred2) && !predicate_implies(pred2, pred1));
}

static void add_types_for_t_branch(Scheme_Object *t, Optimize_Info *info, int fuel)
{
  if (fuel < 0)
    return;

  if (SAME_TYPE(SCHEME_TYPE(t), scheme_ir_local_type)) {
    add_type_no(info, t, scheme_not_proc);
  } else if (SAME_TYPE(SCHEME_TYPE(t), scheme_application2_type)) {
    Scheme_App2_Rec *app = (Scheme_App2_Rec *)t;
    if (SCHEME_PRIMP(app->rator)
        && SAME_TYPE(SCHEME_TYPE(app->rand), scheme_ir_local_type)
        && relevant_predicate(app->rator)) {
      /* Looks like a predicate on a local variable. Record that the
         predicate succeeded, which may allow conversion of safe
         operations to unsafe operations. */
      add_type(info, app->rand, app->rator);
    }
    if (SAME_OBJ(app->rator, scheme_not_proc)) {
      add_types_for_f_branch(app->rand, info, fuel-1);
    }

    if (SAME_TYPE(SCHEME_TYPE(app->rand), scheme_ir_local_type)) {
      Scheme_Object *shape;
      shape = get_struct_proc_shape(app->rator, info, 0);
      if (shape
          && ((SCHEME_PROC_SHAPE_MODE(shape) & STRUCT_PROC_SHAPE_MASK) == STRUCT_PROC_SHAPE_PRED)
          && SCHEME_PAIRP(SCHEME_PROC_SHAPE_IDENTITY(shape))) {
        add_type(info, app->rand, shape);
      }
    }
  } else if (SAME_TYPE(SCHEME_TYPE(t), scheme_application3_type)) {
    Scheme_App3_Rec *app = (Scheme_App3_Rec *)t;
    Scheme_Object *pred1, *pred2;
    if (SAME_OBJ(app->rator, scheme_eq_proc)
        || SAME_OBJ(app->rator, scheme_eqv_proc)
        || SAME_OBJ(app->rator, scheme_equal_proc)) {
      if (SAME_TYPE(SCHEME_TYPE(app->rand1), scheme_ir_local_type)) {
        pred1 = expr_implies_predicate(app->rand1, info);
        if (!pred1) {
          pred2 = expr_implies_predicate(app->rand2, info);
          if (pred2)
            add_type(info, app->rand1, pred2);
        }
      }
      if (SAME_TYPE(SCHEME_TYPE(app->rand2), scheme_ir_local_type)) {
        pred2 = expr_implies_predicate(app->rand2, info);
        if (!pred2) {
          pred1 = expr_implies_predicate(app->rand1, info);
          if (pred1)
            add_type(info, app->rand2, pred1);
        }
      }
    }

  } else if (SAME_TYPE(SCHEME_TYPE(t), scheme_branch_type)) {
    Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)t;
    if (SCHEME_FALSEP(b->fbranch)) {
      add_types_for_t_branch(b->test, info, fuel-1);
      add_types_for_t_branch(b->tbranch, info, fuel-1);
    }
    if (SCHEME_FALSEP(b->tbranch)) {
      add_types_for_f_branch(b->test, info, fuel-1);
      add_types_for_t_branch(b->fbranch, info, fuel-1);
    }
  }
}

static void add_types_for_f_branch(Scheme_Object *t, Optimize_Info *info, int fuel)
{
  if (fuel < 0)
    return;

  if (SAME_TYPE(SCHEME_TYPE(t), scheme_ir_local_type)) {
    add_type(info, t, scheme_not_proc);
  
  } else if (SAME_TYPE(SCHEME_TYPE(t), scheme_application2_type)) {
    Scheme_App2_Rec *app = (Scheme_App2_Rec *)t;
    if (SCHEME_PRIMP(app->rator)
        && SAME_TYPE(SCHEME_TYPE(app->rand), scheme_ir_local_type)
        && relevant_predicate(app->rator)) {
      /* Looks like a predicate on a local variable. Record that the
         predicate failed, this is currently useful only for lists. */
      add_type_no(info, app->rand, app->rator);
    }

  } else if (SAME_TYPE(SCHEME_TYPE(t), scheme_branch_type)) {
    Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)t;
    if (SAME_OBJ(b->fbranch, scheme_true)) {
      add_types_for_t_branch(b->test, info, fuel-1);
      add_types_for_f_branch(b->tbranch, info, fuel-1);
    }
    if (SAME_OBJ(b->tbranch, scheme_true)) {
      add_types_for_f_branch(b->test, info, fuel-1);
      add_types_for_f_branch(b->fbranch, info, fuel-1);
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
  int init_vclock, init_aclock, init_kclock, init_sclock;
  Optimize_Info *then_info, *else_info;
  Optimize_Info *then_info_init, *else_info_init;
  Optimize_Info_Sequence info_seq;

  b = (Scheme_Branch_Rec *)o;

  t = b->test;
  tb = b->tbranch;
  fb = b->fbranch;

  /* Convert (if <id> expr <id>) to (if <id> expr #f) */
  if (equivalent_exprs(t, fb, NULL, NULL, 0)) {
    fb = scheme_false;
  }
  
  /* For test position, convert (if <id> <id> expr) to (if <id> #t expr) */
  if ((context & OPT_CONTEXT_BOOLEAN)
      && equivalent_exprs(t, tb, NULL, NULL, 0)) {
      tb = scheme_true;
  }

  optimize_info_seq_init(info, &info_seq);

  t = scheme_optimize_expr(t, info, OPT_CONTEXT_BOOLEAN | OPT_CONTEXT_SINGLED);

  if (info->escapes) {
    optimize_info_seq_done(info, &info_seq);
    return ensure_noncm(t, info);
  }

  /* Try to lift out `let`s and `begin`s around a test: */
  {
    Scheme_Object *inside = NULL, *t2 = t;

    while (1) {
      extract_tail_inside(&t2, &inside);

      /* Try optimize: (if (not x) y z) => (if x z y) */
      if (SAME_TYPE(SCHEME_TYPE(t2), scheme_application2_type)) {
        Scheme_App2_Rec *app = (Scheme_App2_Rec *)t2;
        
        if (SAME_PTR(scheme_not_proc, app->rator)) {
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

    if (!(SCHEME_TYPE(t2) > _scheme_ir_values_types_)) {
      /* (if (let (...) (cons x y)) a b) => (if (begin (let (...) (begin x y #<void>)) #t/#f) a b)
         but don't expand (if (let (...) (begin x K)) a b) */
      Scheme_Object *pred;

      pred = expr_implies_predicate(t2, info); 
      if (pred) {
        Scheme_Object *test_val = NULL;
        
        if (predicate_implies(pred, scheme_not_proc))
          test_val = scheme_false;
        else if (predicate_implies_not(pred, scheme_not_proc))
          test_val = scheme_true;

        if (test_val) {
          t2 = optimize_ignored(t2, info, 1, 0, 5);
          t = replace_tail_inside(t2, inside, t);

          t2 = test_val;
          if (scheme_omittable_expr(t, 1, 5, 0, info, NULL)) {
            t = test_val;
            inside = NULL;
          } else {
            t = make_sequence_2(t, test_val);
            inside = t;
          }
        }
      }
    }

    if (SCHEME_TYPE(t2) > _scheme_ir_values_types_) {
      /* Branch is statically known */
      Scheme_Object *xb;

      optimize_info_seq_done(info, &info_seq);
      info->size -= 1;

      if (SCHEME_FALSEP(t2))
        xb = scheme_optimize_expr(fb, info, scheme_optimize_tail_context(context));
      else
        xb = scheme_optimize_expr(tb, info, scheme_optimize_tail_context(context));
      
      optimize_info_seq_done(info, &info_seq);
      return replace_tail_inside(xb, inside, t);
    }
  }

  optimize_info_seq_step(info, &info_seq);

  info->vclock += 1; /* model branch as clock increment */

  init_vclock = info->vclock;
  init_aclock = info->aclock;
  init_kclock = info->kclock;
  init_sclock = info->sclock;

  then_info = optimize_info_add_frame(info, 0, 0, 0);
  add_types_for_t_branch(t, then_info, 5);
  then_info_init = optimize_info_add_frame(then_info, 0, 0, 0);
  tb = scheme_optimize_expr(tb, then_info, scheme_optimize_tail_context(context));
  optimize_info_done(then_info, NULL);

  info->escapes = 0;
  info->vclock = init_vclock;
  info->aclock = init_aclock;
  info->kclock = init_kclock;
  info->sclock = init_sclock;

  optimize_info_seq_step(info, &info_seq);

  else_info = optimize_info_add_frame(info, 0, 0, 0);
  add_types_for_f_branch(t, else_info, 5);
  else_info_init = optimize_info_add_frame(else_info, 0, 0, 0);
  fb = scheme_optimize_expr(fb, else_info, scheme_optimize_tail_context(context));
  optimize_info_done(else_info, NULL);

  if (then_info->escapes && else_info->escapes) {
    /* both branches escaped */
    info->preserves_marks = 1;
    info->single_result = 1;
    info->kclock = init_kclock;

  } else if (info->escapes) {
    info->preserves_marks = then_info->preserves_marks;
    info->single_result = then_info->single_result;
    info->kclock = then_info->kclock;
    merge_types(then_info, info, NULL);
    info->escapes = 0;

  } else if (then_info->escapes) {
      info->preserves_marks = else_info->preserves_marks;
      info->single_result = else_info->single_result;
      merge_types(else_info, info, NULL);
      info->escapes = 0;

  } else {
    int new_preserves_marks, new_single_result;

    new_preserves_marks = or_tentative(then_info->preserves_marks, else_info->preserves_marks);
    info->preserves_marks = new_preserves_marks;
    new_single_result = or_tentative(then_info->single_result, else_info->single_result);
    info->single_result = new_single_result;
    if (then_info->kclock > info->kclock)
      info->kclock = then_info->kclock;
    merge_branchs_types(then_info, else_info, info);
  }

  if (then_info->sclock > info->sclock)
    info->sclock = then_info->sclock;
  if (then_info->aclock > info->aclock)
    info->aclock = then_info->aclock;

  if ((init_vclock == then_info->vclock) && (init_vclock == info->vclock)) {
    /* we can rewind the vclock to just after the test, because the
       `if` as a whole has no effect */
    info->vclock--;
  }

  optimize_info_seq_done(info, &info_seq);

  /* Try optimize: (if x #f #t) => (not x) */
  if (SCHEME_FALSEP(tb)
      && SAME_OBJ(fb, scheme_true)) {
    info->size -= 2;
    return make_optimize_prim_application2(scheme_not_proc, t, info, context);
  }

  /* Convert (if <boolean> #t #f) to <boolean>
     and, for test position, convert (if <expr> #t #f) to <expr> */
  if (SAME_OBJ(tb, scheme_true) && SAME_OBJ(fb, scheme_false)) {
    Scheme_Object *pred;

    if (context & OPT_CONTEXT_BOOLEAN)
      /* In a boolean context, any expression can be extrated. */
      pred = scheme_boolean_p_proc;
    else
      pred = expr_implies_predicate(t, info);

    if (pred && predicate_implies(pred, scheme_boolean_p_proc)) {
      info->size -= 2;
      return ensure_single_value_noncm(t, info);
    }
  }

  /* Try optimize: (if <expr> v v) => (begin <expr> v) */
  {
    Scheme_Object *nb;

    nb = equivalent_exprs(tb, fb, then_info_init, else_info_init, context);
    if (nb) {
      info->size -= 1;
      return make_discarding_first_sequence(t, nb, info);
    }
  }

  /* Try optimize: (if x x #f) => x 
     This pattern is included in the previous reduction,
     but this is still useful if x is mutable or a top level*/
  if (SCHEME_FALSEP(fb)
      && equivalent_exprs(t, tb, NULL, NULL, 0)) {
      info->size -= 2;
      return ensure_single_value(t, info);
  }

  /* Convert: expressions like
     (if (if M N #f) P K) => (if M (if N P K) K)
     for simple constants K. This is useful to expose simple
     tests to the JIT. */
  if (SAME_TYPE(SCHEME_TYPE(t), scheme_branch_type)) {
    Scheme_Branch_Rec *b2 = (Scheme_Branch_Rec *)t;
    Scheme_Object *ntb, *nfb, *nt2 = NULL;
    if (SCHEME_FALSEP(b2->fbranch)
        && scheme_ir_duplicate_ok(fb, 0)) {
      /* (if (if M N #f) P K) => (if M (if N P K) K) */
      ntb = (Scheme_Object *)b2;
      nfb = optimize_clone(0, fb, info, empty_eq_hash_tree, 0);
      nt2 = b2->tbranch;
    } else if (SCHEME_FALSEP(b2->tbranch)
               && scheme_ir_duplicate_ok(fb, 0)) {
      /* (if (if M #f N) P K) => (if M K (if N P K)) */
      ntb = optimize_clone(0, fb, info, empty_eq_hash_tree, 0);
      nfb = (Scheme_Object *)b2;
      nt2 = b2->fbranch;
    } else if (SAME_OBJ(b2->fbranch, scheme_true)
               && scheme_ir_duplicate_ok(tb, 0)) {
      /* (if (if M N #t) K P) => (if M (if N K P) K) */
      ntb = (Scheme_Object *)b2;
      nfb = optimize_clone(0, tb, info, empty_eq_hash_tree, 0);
      nt2 = b2->tbranch;
    } else if (SAME_OBJ(b2->tbranch, scheme_true) 
               && scheme_ir_duplicate_ok(tb, 0)) {
      /* (if (if M #t N) K P) => (if M K (if N K P)) */
      ntb = optimize_clone(0, tb, info, empty_eq_hash_tree, 0);
      nfb = (Scheme_Object *)b2;
      nt2 = b2->fbranch;
    }
    if (nt2) {
      t = b2->test;
      b2->test = nt2;
      b2->tbranch = tb;
      b2->fbranch = fb;
      tb = ntb;
      fb = nfb;
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

/*========================================================================*/
/*                       with-continuation-marks                          */
/*========================================================================*/

static int omittable_key(Scheme_Object *k, Optimize_Info *info)
{
  /* A key is not omittable if it might refer to a chaperoned/impersonated
     continuation mark key, so that's why we pass OMITTABLE_KEEP_VARS: */
  return scheme_omittable_expr(k, 1, 20, OMITTABLE_KEEP_VARS, info, info);
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
    return ensure_noncm(k, info);
  }

  optimize_info_seq_step(info, &info_seq);

  v = scheme_optimize_expr(wcm->val, info, OPT_CONTEXT_SINGLED);

  if (info->escapes) {
    optimize_info_seq_done(info, &info_seq);
    info->size += 1;
    return ensure_noncm(make_discarding_first_sequence(k, v, info), info);
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

  /* If the body cannot inspect the continution, and if the key is not
     a chaperone, no need to add the mark: */
  if (omittable_key(k, info)
      && scheme_omittable_expr(b, -1, 20, 0, info, info))
    return make_discarding_first_sequence(v, b, info);

  /* info->single_result is already set */
  info->preserves_marks = 0;

  wcm->key = k;
  wcm->val = v;
  wcm->body = b;

  info->size += 1;

  /* Simplify (with-continuation-mark <same-key> <val1>
               (with-continuation-mark <same-key> <val2>
                 <body>))
     to (begin
         <val1>
         (with-continuation-mark <same-key> <val2>
         <body>))
     as long as <val2> doesn't inspect the continuation. */
  if (SAME_TYPE(SCHEME_TYPE(wcm->body), scheme_with_cont_mark_type)
      && equivalent_exprs(wcm->key, ((Scheme_With_Continuation_Mark *)wcm->body)->key, NULL, NULL, 0)
      && scheme_omittable_expr(((Scheme_With_Continuation_Mark *)wcm->body)->val, 1, 20, 0, info, info))
    return make_discarding_first_sequence(wcm->val, wcm->body, info);

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
    return ensure_noncm(val, info);

  info->preserves_marks = 1;
  info->single_result = 1;

  if (SAME_TYPE(SCHEME_TYPE(var), scheme_ir_local_type)) {
    register_use(SCHEME_VAR(var), info);
  } else {
    optimize_info_used_top(info);
  }

  info->vclock++;

  sb->var = var;
  sb->val = val;

  return (Scheme_Object *)sb;
}

static Scheme_Object *
set_clone(int single_use, Scheme_Object *data, Optimize_Info *info, Scheme_Hash_Tree *var_map)
{
  Scheme_Set_Bang *sb = (Scheme_Set_Bang *)data, *naya;
  Scheme_Object *var, *val;

  naya = MALLOC_ONE_TAGGED(Scheme_Set_Bang);
  memcpy(naya, sb, sizeof(Scheme_Set_Bang));

  var = naya->var;
  val = naya->val;

  val = optimize_clone(single_use, val, info, var_map, 0);
  if (!val) return NULL;
  if (SAME_TYPE(SCHEME_TYPE(var), scheme_ir_local_type)) {
    var = optimize_clone(single_use, var, info, var_map, 0);
    if (!var) return NULL;
  }

  naya->var = var;
  naya->val = val;

  return (Scheme_Object *)naya;
}

static Scheme_Object *
ref_optimize(Scheme_Object *data, Optimize_Info *info, int context)
{
  Scheme_Object *v;

  optimize_info_used_top(info);

  v = SCHEME_PTR1_VAL(data);
  if (SAME_TYPE(SCHEME_TYPE(v), scheme_ir_local_type)) {
    SCHEME_PTR1_VAL(data) = (SCHEME_VAR(v)->mutated ? scheme_false : scheme_true);
  } else if (SAME_TYPE(SCHEME_TYPE(v), scheme_ir_toplevel_type)) {
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
ref_clone(int single_use, Scheme_Object *data, Optimize_Info *info, Scheme_Hash_Tree *var_map)
{
  Scheme_Object *naya;
  Scheme_Object *a, *b;

  a = SCHEME_PTR1_VAL(data);
  a = optimize_clone(single_use, a, info, var_map, 0);
  if (!a) return NULL;

  b = SCHEME_PTR2_VAL(data);
  b = optimize_clone(single_use, b, info, var_map, 0);
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
    return ensure_noncm(f, info);
  }
  optimize_info_seq_step(info, &info_seq);

  e = scheme_optimize_expr(e, info, 0);

  optimize_info_seq_done(info, &info_seq);

  if (info->escapes) {
    info->size += 1;
    return ensure_noncm(make_discarding_first_sequence(f, e, info), info);
  }

  info->size += 1;
  info->vclock += 1;
  info->kclock += 1;
  info->sclock += 1;

  return optimize_apply_values(f, e, info, info->single_result, context);
}

static Scheme_Object *
apply_values_clone(int single_use, Scheme_Object *data, Optimize_Info *info, Scheme_Hash_Tree *var_map)
{
  Scheme_Object *f, *e;

  f = SCHEME_PTR1_VAL(data);
  e = SCHEME_PTR2_VAL(data);

  f = optimize_clone(single_use, f, info, var_map, 0);
  if (!f) return NULL;
  e = optimize_clone(single_use, e, info, var_map, 0);
  if (!e) return NULL;

  data = scheme_alloc_object();
  data->type = scheme_apply_values_type;
  SCHEME_PTR1_VAL(data) = f;
  SCHEME_PTR2_VAL(data) = e;

  return data;
}

static Scheme_Object *
with_immed_mark_optimize(Scheme_Object *data, Optimize_Info *info, int context)
{
  Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)data;
  Scheme_Object *key, *val, *body;
  Optimize_Info_Sequence info_seq;
  Optimize_Info *body_info;
  Scheme_IR_Local *var;

  optimize_info_seq_init(info, &info_seq);

  key = scheme_optimize_expr(wcm->key, info, OPT_CONTEXT_SINGLED);
  optimize_info_seq_step(info, &info_seq);
  if (info->escapes) {
    optimize_info_seq_done(info, &info_seq);
    return ensure_noncm(key, info);
  }

  val = scheme_optimize_expr(wcm->val, info, OPT_CONTEXT_SINGLED);
  optimize_info_seq_step(info, &info_seq);
  if (info->escapes) {
    optimize_info_seq_done(info, &info_seq);
    return ensure_noncm(make_discarding_first_sequence(key, val, info), info);
  }

  optimize_info_seq_done(info, &info_seq);
  
  body_info = optimize_info_add_frame(info, 1, 1, 0);
  var = SCHEME_VAR(SCHEME_CAR(wcm->body));
  set_optimize_mode(var);
  var->optimize.lambda_depth = body_info->lambda_depth;
  var->optimize_used = 0;
  var->optimize.init_kclock = info->kclock;
  
  body = scheme_optimize_expr(SCHEME_CDR(wcm->body), body_info, 0);
  
  optimize_info_done(body_info, NULL);

  wcm->key = key;
  wcm->val = val;
  SCHEME_CDR(wcm->body) = body;

  return data;
}

static Scheme_Object *
with_immed_mark_clone(int single_use, Scheme_Object *data, Optimize_Info *info, Scheme_Hash_Tree *var_map)
{
  Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)data;
  Scheme_With_Continuation_Mark *wcm2;
  Scheme_Object *e;
  Scheme_IR_Local *var;
  
  wcm2 = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
  wcm2->so.type = scheme_with_immed_mark_type;

  e = optimize_clone(single_use, wcm->key, info, var_map, 0);
  if (!e) return NULL;
  wcm2->key = e;

  e = optimize_clone(single_use, wcm->val, info, var_map, 0);
  if (!e) return NULL;
  wcm2->val = e;

  var = clone_variable(SCHEME_VAR(SCHEME_CAR(wcm->body)));
  var_map = scheme_hash_tree_set(var_map, SCHEME_CAR(wcm->body), (Scheme_Object *)var);
  
  e = optimize_clone(single_use, SCHEME_CDR(wcm->body), info, var_map, 0);
  if (!e) return NULL;
  e = scheme_make_mutable_pair((Scheme_Object *)var, e);
  wcm2->body = e;

  return (Scheme_Object *)wcm2;
}

static Scheme_Object *
case_lambda_optimize(Scheme_Object *expr, Optimize_Info *info, int context)
{
  Scheme_Object *le;
  int i;
  Scheme_Case_Lambda *seq = (Scheme_Case_Lambda *)expr;

  for (i = 0; i < seq->count; i++) {
    le = seq->array[i];
    le = scheme_optimize_expr(le, info, 0);
    seq->array[i] = le;
  }

  info->preserves_marks = 1;
  info->single_result = 1;
  info->size += 1;

  return expr;
}

static Scheme_Object *
case_lambda_clone(int single_use, Scheme_Object *data, Optimize_Info *info, Scheme_Hash_Tree *var_map)
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
    le = optimize_clone(single_use, le, info, var_map, 0);
    if (!le) return NULL;
    seq2->array[i] = le;
  }

  return (Scheme_Object *)seq2;
}

static Scheme_Object *begin0_optimize(Scheme_Object *obj, Optimize_Info *info, int context)
{
  int i, count, drop = 0, prev_size, single_result = 0, preserves_marks = 0, kclock = 0, sclock = 0;
  Scheme_Sequence *s = (Scheme_Sequence *)obj;
  Scheme_Object *inside = NULL, *expr, *orig_first;
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
      le = optimize_ignored(le, info, -1, 1, 5);
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
      /* We will ignore the first expression too */
      le = optimize_ignored(s->array[0], info, -1, 1, 5);
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
      return ensure_noncm(s->array[i], info);
    }

    s2 = scheme_malloc_sequence(count - drop);
    s2->so.type = scheme_sequence_type;
    s2->count = count - drop;

    for (i = 0; i < count; i++) {
      if (s->array[i]) {
        s2->array[j++] = s->array[i];
      }
    }
    return flatten_sequence((Scheme_Object *)s2, info, context);
  }

  info->preserves_marks = 1;
  info->single_result = single_result;

  if ((s->count - drop) == 1 && (preserves_marks == 1)) {
    /* If the first expression preserves marks we can drop the begin0 */
    return s->array[0];
  }

  expr = s->array[0];
  orig_first = s->array[0];
  extract_tail_inside(&expr, &inside);

  /* Try optimize (begin0 <movable> ...) => (begin ... <movable>) */
  if (movable_expression(expr, info, 0, kclock != info->kclock,
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
  expr = flatten_sequence(expr, info, context);
  return replace_tail_inside(expr, inside, orig_first);
}

static Scheme_Object *do_define_syntaxes_optimize(Scheme_Object *data, Optimize_Info *info)
{
  Scheme_Object *val;
  Optimize_Info *einfo;

  val = SCHEME_VEC_ELS(data)[3];

  einfo = scheme_optimize_info_create(info->cp, info->env, info->insp, 0);
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
    einfo = scheme_optimize_info_create(info->cp, info->env, info->insp, 0);
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
/* Can we lift a call to `v` out of a `letrec` to a wrapping `let`? */
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

  if (SAME_OBJ(v, scheme_values_proc))
    return 1;

  return 0;
}

int scheme_is_liftable(Scheme_Object *o, Scheme_Hash_Tree *exclude_vars, int fuel, int as_rator, int or_escape)
  /* Can we lift `o` out of a `letrec` to a wrapping `let`? Refences
     to `exclude_vars` are not allowed, since those are the LHS. */
{
  Scheme_Type t = SCHEME_TYPE(o);

  if (!fuel) return 0;

  switch (t) {
  case scheme_ir_lambda_type:
    return !as_rator;
  case scheme_case_lambda_sequence_type:
    return !as_rator;
  case scheme_ir_toplevel_type:
    return 1;
  case scheme_ir_local_type:
    if (!scheme_hash_tree_get(exclude_vars, o))
      return 1;
    break;
  case scheme_branch_type:
    {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)o;
      if (scheme_is_liftable(b->test, exclude_vars, fuel - 1, 0, or_escape)
	  && scheme_is_liftable(b->tbranch, exclude_vars, fuel - 1, as_rator, or_escape)
	  && scheme_is_liftable(b->fbranch, exclude_vars, fuel - 1, as_rator, or_escape))
	return 1;
    }
    break;
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)o;
      int i;
      if (!is_liftable_prim(app->args[0], or_escape))
        return 0;
      for (i = app->num_args + 1; i--; ) {
	if (!scheme_is_liftable(app->args[i], exclude_vars, fuel - 1, 1, or_escape))
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
      if (scheme_is_liftable(app->rator, exclude_vars, fuel - 1, 1, or_escape)
	  && scheme_is_liftable(app->rand, exclude_vars, fuel - 1, 1, or_escape))
	return 1;
    }
    break;
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)o;
      if (!is_liftable_prim(app->rator, or_escape))
        return 0;
      if (scheme_is_liftable(app->rator, exclude_vars, fuel - 1, 1, or_escape)
	  && scheme_is_liftable(app->rand1, exclude_vars, fuel - 1, 1, or_escape)
	  && scheme_is_liftable(app->rand2, exclude_vars, fuel - 1, 1, or_escape))
	return 1;
    }
    break;
  case scheme_ir_let_header_type:
    {
      Scheme_IR_Let_Header *lh = (Scheme_IR_Let_Header *)o;
      int i;

      o = lh->body;
      for (i = lh->num_clauses; i--; ) {
        if (!scheme_is_liftable(((Scheme_IR_Let_Value *)o)->value, exclude_vars, fuel - 1, as_rator, or_escape))
          return 0;
        o = ((Scheme_IR_Let_Value *)o)->body;
      }
      if (scheme_is_liftable(o, exclude_vars, fuel - 1, as_rator, or_escape))
        return 1;
      break;
    }
  default:
    if (t > _scheme_ir_values_types_)
      return 1;
  }

  return 0;
}

int ir_propagate_ok(Scheme_Object *value, Optimize_Info *info, int used_once, Scheme_IR_Local *once_var)
/* Can we constant-propagate the expression `value`?
   If `used_once` is true, the value is known to be used once,
   but if `once_var` is provided, record when the result
   relies on that once-usedness. */
{
  if (SAME_TYPE(SCHEME_TYPE(value), scheme_ir_lambda_type)) {
    int sz;
    sz = lambda_body_size_plus_info((Scheme_Lambda *)value, 1, info, NULL);
    if ((sz >= 0) && (sz <= MAX_PROC_INLINE_SIZE))
      return 1;
    else if (used_once) {
      if (once_var) {
        /* Mark the variable as having a known value only as long as it's used just
           once. In case the one reference is duplicated --- perhaps because it is
           used in a non-application position in a function that is itself inlined
           --- then the known value should be cleared. */
        once_var->optimize.clear_known_on_multi_use = 1;
      }
      return 1;
    } else {
      Scheme_Lambda *lam = (Scheme_Lambda *)value;
      if (sz < 0)
        scheme_log(info->logger,
                   SCHEME_LOG_DEBUG,
                   0,
                   /* contains non-copyable body elements that prevent inlining */
                   "non-copyable %s size: %d threshold: %d#<separator>%s",
                   scheme_write_to_string(lam->name ? lam->name : scheme_false, NULL),
                   sz,
                   0, /* no sensible threshold here */
                   scheme_optimize_context_to_string(info->context));
      else
        scheme_log(info->logger,
                   SCHEME_LOG_DEBUG,
                   0,
                   /* too large to be an inlining candidate */
                   "too-large %s size: %d threshold: %d#<separator>%s",
                   scheme_write_to_string(lam->name ? lam->name : scheme_false, NULL),
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
      if (!ir_propagate_ok(cl->array[i], info, used_once, once_var))
        return 0;
    }
    return 1;
  }

  if (SAME_TYPE(SCHEME_TYPE(value), scheme_ir_toplevel_type)) {
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
    return 0;
  }

  /* Test this after the specific cases, 
     because it recognizes locals and toplevels. */
  if (scheme_ir_duplicate_ok(value, 0))
    return 1;

  return 0;
}

int scheme_is_statically_proc(Scheme_Object *value, Optimize_Info *info, int flags)
/* Does `value` definitely produce a procedure of a specific shape?
   This function can be used on resolved (and SFS) forms, too, and it
   must be consistent with (i.e., as least as accepting as)
   optimization-time decisions. The `flags` argument is for
   scheme_omittable_expr(). */
{
  while (1) {
    if (SCHEME_LAMBDAP(value)
        || SCHEME_PROCP(value)
        || SAME_TYPE(SCHEME_TYPE(value), scheme_lambda_type)
        || SAME_TYPE(SCHEME_TYPE(value), scheme_case_lambda_sequence_type)
        || SAME_TYPE(SCHEME_TYPE(value), scheme_inline_variant_type))
      return 1;
    else if (SAME_TYPE(SCHEME_TYPE(value), scheme_ir_let_header_type)) {
      /* Look for (let ([x <omittable>]) <proc>), which is generated for optional arguments. */
      Scheme_IR_Let_Header *lh = (Scheme_IR_Let_Header *)value;
      if (lh->num_clauses == 1) {
        Scheme_IR_Let_Value *lv = (Scheme_IR_Let_Value *)lh->body;
        if (scheme_omittable_expr(lv->value, lv->count, 20, flags, info, NULL)) {
          value = lv->body;
        } else
          break;
      } else
        break;
    } else if (SAME_TYPE(SCHEME_TYPE(value), scheme_let_one_type)) {
      Scheme_Let_One *lo = (Scheme_Let_One *)value;
      if (scheme_omittable_expr(lo->value, 1, 20, flags, info, NULL)) {
        value = lo->body;
      } else
        break;
    } else if (SAME_TYPE(SCHEME_TYPE(value), scheme_boxenv_type)) {
      value = SCHEME_PTR2_VAL(value);
    } else if (SAME_TYPE(SCHEME_TYPE(value), scheme_sequence_type)
               /* Handle a sequence for resolved mode, because it might
                  be for safe-for-space clears around a procedure */
               && (flags & OMITTABLE_RESOLVED)) {
      Scheme_Sequence *seq = (Scheme_Sequence *)value;
      int i;
      for (i = 0; i < seq->count-1; i++) {
        if (!scheme_omittable_expr(seq->array[i], 1, 5, flags, info, NULL))
          break;
      }
      if (i == seq->count-1) {
        value = seq->array[i];
      } else
        break;
    } else
      break;
  }

  return 0;
}

Scheme_Object *scheme_make_noninline_proc(Scheme_Object *e)
/* Make a record that presents a procedure of a known shape, but
   that should not be inlined. */
{
  Scheme_Object *ni;

  while (SAME_TYPE(SCHEME_TYPE(e), scheme_ir_let_header_type)) {
    /* This must be (let ([x <omittable>]) <proc>); see scheme_is_statically_proc() */
    Scheme_IR_Let_Header *lh = (Scheme_IR_Let_Header *)e;
    Scheme_IR_Let_Value *lv = (Scheme_IR_Let_Value *)lh->body;
    MZ_ASSERT(lh->num_clauses == 1);
    e = lv->body;
  }

  ni = scheme_alloc_small_object();
  ni->type = scheme_noninline_proc_type;
  SCHEME_PTR_VAL(ni) = e;

  return ni;
}

static int is_values_apply(Scheme_Object *e, int n, Optimize_Info *info, Scheme_Hash_Tree *except_vars, int fuel)
/* Is `e` a `(values ...)` form --- or, in the case of `if`, can be be
   converted to one, so that we can split apart the results
   statically? */
{
  if (SAME_TYPE(SCHEME_TYPE(e), scheme_application_type)) {
    Scheme_App_Rec *app = (Scheme_App_Rec *)e;
    if (n != app->num_args) return 0;
    return SAME_OBJ(scheme_values_proc, app->args[0]);
  } else if ((n == 1) && SAME_TYPE(SCHEME_TYPE(e), scheme_application2_type)) {
    Scheme_App2_Rec *app = (Scheme_App2_Rec *)e;
    return SAME_OBJ(scheme_values_proc, app->rator);
  } else if ((n == 2) && SAME_TYPE(SCHEME_TYPE(e), scheme_application3_type)) {
    Scheme_App3_Rec *app = (Scheme_App3_Rec *)e;
    return SAME_OBJ(scheme_values_proc, app->rator);
  } else if (fuel && SAME_TYPE(SCHEME_TYPE(e), scheme_branch_type)) {
    Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)e;
    if (SAME_TYPE(SCHEME_TYPE(b->test), scheme_ir_local_type)
        && !scheme_hash_tree_get(except_vars, b->test)
        && !SCHEME_VAR(b->test)->mutated) {
      return (is_values_apply(b->tbranch, n, info, except_vars, 0)
              && is_values_apply(b->fbranch, n, info, except_vars, 0));
    }
  }

  return 0;
}

static int no_mutable_bindings(Scheme_IR_Let_Value *irlv)
/* Check whether a `let` clause has any mutable bindings */
{
  int i;

  for (i = irlv->count; i--; ) {
    if (irlv->vars[i]->mutated)
      return 0;
  }

  return 1;
}

static void update_rhs_value(Scheme_IR_Let_Value *naya, Scheme_Object *e,
                             Optimize_Info *info, Scheme_IR_Local *tst)
/* Install an expression from a split `(values ...)` */
{
  if (tst) {
    Scheme_Object *n;
    
    n = equivalent_exprs(naya->value, e, NULL, NULL, 0);
    if (!n) {
      Scheme_Branch_Rec *b;

      /* We're duplicating the test */
      increment_use_count(tst, 0);

      b = MALLOC_ONE_TAGGED(Scheme_Branch_Rec);
      b->so.type = scheme_branch_type;
      b->test = (Scheme_Object *)tst;
      b->tbranch = naya->value;
      b->fbranch = e;

      naya->value = (Scheme_Object *)b;
    } else
      naya->value = n;
  } else
    naya->value = e;
}

static void unpack_values_application(Scheme_Object *e, Scheme_IR_Let_Value *naya,
                                      Optimize_Info *info, Scheme_IR_Local *branch_test)
/* Install the expressions from a split `values` form into new `let` clauses */
{
  if (SAME_TYPE(SCHEME_TYPE(e), scheme_application_type)) {
    Scheme_App_Rec *app = (Scheme_App_Rec *)e;
    int i;
    for (i = 0; i < app->num_args; i++) {
      update_rhs_value(naya, app->args[i + 1], info, branch_test);
      naya = (Scheme_IR_Let_Value *)naya->body;
    }
  } else if (SAME_TYPE(SCHEME_TYPE(e), scheme_application2_type)) {
    Scheme_App2_Rec *app = (Scheme_App2_Rec *)e;
    update_rhs_value(naya, app->rand, info, branch_test);
  } else if (SAME_TYPE(SCHEME_TYPE(e), scheme_application3_type)) {
    Scheme_App3_Rec *app = (Scheme_App3_Rec *)e;
    update_rhs_value(naya, app->rand1, info, branch_test);
    naya = (Scheme_IR_Let_Value *)naya->body;
    update_rhs_value(naya, app->rand2, info, branch_test);
  } else if (SAME_TYPE(SCHEME_TYPE(e), scheme_branch_type)) {
    Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)e;

    MZ_ASSERT(SAME_TYPE(SCHEME_TYPE(b->test), scheme_ir_local_type));

    unpack_values_application(b->tbranch, naya, info, NULL);
    unpack_values_application(b->fbranch, naya, info, SCHEME_VAR(b->test));
  }
}

static Scheme_Object *make_clones(Scheme_IR_Let_Value *retry_start,
                                  Scheme_IR_Let_Value *pre_body,
                                  Optimize_Info *body_info)
/* Clone `lambda`s for re-optimization and for a fixpoint computation of
   procedure properties */
{
  Scheme_IR_Let_Value *irlv;
  Scheme_Object *value, *clone, *pr;
  Scheme_Object *last = NULL, *first = NULL;

  irlv = retry_start;
  while (1) {
    value = irlv->value;
    if (SCHEME_LAMBDAP(value)) {
      clone = optimize_clone(1, value, body_info, empty_eq_hash_tree, 0);
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
    if (irlv == pre_body)
      break;
    irlv = (Scheme_IR_Let_Value *)irlv->body;
  }

  return first;
}

static int set_one_code_flags(Scheme_Object *value, int flags,
                              Scheme_Object *first, Scheme_Object *second,
                              int set_flags, int mask_flags, int just_tentative,
                              int merge_local_typed)
/* Set, record, or merge procedure-property flags */
{
  Scheme_Case_Lambda *cl, *cl2, *cl3;
  Scheme_Lambda *lam, *lam2, *lam3;
  int i, count;

  if (SAME_TYPE(scheme_ir_lambda_type, SCHEME_TYPE(value))) {
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
      lam = (Scheme_Lambda *)cl->array[i];
      lam2 = (Scheme_Lambda *)cl2->array[i];
      lam3 = (Scheme_Lambda *)cl3->array[i];
    } else {
      lam = (Scheme_Lambda *)value;
      lam2 = (Scheme_Lambda *)first;
      lam3 = (Scheme_Lambda *)second;
    }

    if (merge_local_typed) {
      merge_lambda_arg_types(lam, lam2);
      merge_lambda_arg_types(lam, lam3);
      merge_lambda_arg_types(lam, lam2);
    }

    if (!just_tentative || (SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_RESULT_TENTATIVE)) {
      flags = (flags & SCHEME_LAMBDA_FLAGS(lam));
      SCHEME_LAMBDA_FLAGS(lam2) = set_flags | (SCHEME_LAMBDA_FLAGS(lam2) & mask_flags);
      SCHEME_LAMBDA_FLAGS(lam3) = set_flags | (SCHEME_LAMBDA_FLAGS(lam3) & mask_flags);
    }
  }

  return flags;
}

static int set_code_flags(Scheme_IR_Let_Value *retry_start,
                          Scheme_IR_Let_Value *pre_body,
                          Scheme_Object *clones,
                          int set_flags, int mask_flags, int just_tentative,
                          int merge_local_typed)
/* Set, record, or merge procedure-property flags */
{
  Scheme_IR_Let_Value *irlv;
  Scheme_Object *value, *first;
  int flags = LAMBDA_SINGLE_RESULT | LAMBDA_PRESERVES_MARKS;

  /* The first in a clone pair is the one that is consulted for
     references. The second one is the clone, and it's the one whose
     flags are updated by optimization. So consult the clone, and set
     flags in both. */

  irlv = retry_start;
  while (clones) {
    value = irlv->value;
    if (SCHEME_LAMBDAP(value)) {
      first = SCHEME_CAR(clones);

      if (first)
        flags = set_one_code_flags(value, flags,
                                   SCHEME_CAR(first), SCHEME_CDR(first),
                                   set_flags, mask_flags, just_tentative,
                                   merge_local_typed);

      clones = SCHEME_CDR(clones);
    }

    if (irlv == pre_body)
      break;
    irlv = (Scheme_IR_Let_Value *)irlv->body;
  }

  return flags;
}

static int lambda_body_size(Scheme_Object *o, int less_args)
{
  int bsz;

  if (SAME_TYPE(SCHEME_TYPE(o), scheme_ir_lambda_type)) {
    bsz = lambda_body_size_plus_info((Scheme_Lambda *)o, 0, NULL, NULL);
    if (less_args) bsz -= ((Scheme_Lambda *)o)->num_params;
    return bsz;
  } else if (SAME_TYPE(SCHEME_TYPE(o), scheme_case_lambda_sequence_type)) {
    Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)o;
    int i, sz = 0;
    for (i = cl->count; i--; ) {
      bsz = lambda_body_size_plus_info((Scheme_Lambda *)cl->array[i], 0, NULL, NULL);
      if (less_args) {
        bsz -= ((Scheme_Lambda *)cl->array[i])->num_params;
        if (bsz > sz) sz = bsz;
      } else
        sz += bsz;
    }
    return sz;
  } else
    return 0;
}

static int expr_size(Scheme_Object *o)
{
  return lambda_body_size(o, 0) + 1;
}

int scheme_might_invoke_call_cc(Scheme_Object *value)
{
  return !scheme_is_liftable(value, empty_eq_hash_tree, 10, 0, 1);
}

#define ADVANCE_CLOCKS_INIT_FUEL 3

void advance_clocks_for_optimized(Scheme_Object *o,
                                  GC_CAN_IGNORE int *_vclock,
                                  GC_CAN_IGNORE int *_aclock,
                                  GC_CAN_IGNORE int *_kclock,
                                  GC_CAN_IGNORE int *_sclock,
                                  Optimize_Info *info,
                                  int fuel)
/* It's ok for this function to advance clocks *less* than
   accurately, but not more than accurately */
{
  Scheme_Object *rator = NULL;
  int argc = 0;

  if (!fuel) return;

  switch (SCHEME_TYPE(o)) {
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)o;
      int i;
      for (i = 0; i < app->num_args; i++) {
        advance_clocks_for_optimized(app->args[i+1],
                                     _vclock, _aclock, _kclock, _sclock,
                                     info, fuel - 1);
      }
      rator = app->args[0];
      argc = app->num_args;
    }
    break;
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)o;
      advance_clocks_for_optimized(app->rand,
                                   _vclock, _aclock, _kclock, _sclock,
                                   info, fuel - 1);
      rator = app->rator;
      argc = 1;
      break;
    }
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)o;
      advance_clocks_for_optimized(app->rand1,
                                   _vclock, _aclock, _kclock, _sclock,
                                   info, fuel - 1);
      advance_clocks_for_optimized(app->rand2,
                                   _vclock, _aclock, _kclock, _sclock,
                                   info, fuel - 1);
      rator = app->rator;
      argc = 2;
    }
    break;
  default:
    break;
  }

  if (rator)
    increment_clock_counts_for_application(_vclock, _aclock, _kclock, _sclock, rator, argc);

  if ((*_vclock > info->vclock)
      || (*_aclock > info->aclock)
      || (*_kclock > info->kclock)
      || (*_sclock > info->sclock))
    scheme_signal_error("internal error: optimizer clock tracking has gone wrong");
}

static void set_application_types(Scheme_Object *o, Optimize_Info *info, int fuel)
/* Peek ahead in an expression to set readily apparent type information
   for function calls. This information is useful for type-invariant loop
   arguments, for example. */
{
  if (!fuel) return;

  switch (SCHEME_TYPE(o)) {
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)o;
      int i;
      register_local_argument_types(app, NULL, NULL, info);
      for (i = 0; i < app->num_args+1; i++) {
        set_application_types(app->args[i], info, fuel - 1);
      }
    }
    break;
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)o;
      register_local_argument_types(NULL, app, NULL, info);
      set_application_types(app->rator, info, fuel - 1);
      set_application_types(app->rand, info, fuel - 1);
      break;
    }
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)o;
      register_local_argument_types(NULL, NULL, app, info);
      set_application_types(app->rator, info, fuel - 1);
      set_application_types(app->rand1, info, fuel - 1);
      set_application_types(app->rand2, info, fuel - 1);
    }
    break;
  case scheme_sequence_type:
  case scheme_begin0_sequence_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)o;
      int i;

      for (i = 0; i < seq->count; i++) {
        set_application_types(seq->array[i], info, fuel - 1);
      }
    }
    break;
  case scheme_branch_type:
    {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)o;
      set_application_types(b->test, info, fuel - 1);
      set_application_types(b->tbranch, info, fuel - 1);
      set_application_types(b->fbranch, info, fuel - 1);
    }
    break;
  default:
    break;
  }
}

static void flip_transitive(Scheme_Hash_Table *ht, int on)
/* Adjust usage flags based on recorded tentative uses */
{
  Scheme_IR_Local *tvar;
  int j;
  Scheme_Object *to_remove = scheme_null;
      
  for (j = 0; j < ht->size; j++) {
    if (ht->vals[j]) {
      tvar = SCHEME_VAR(ht->keys[j]);
      if (on) {
        if (tvar->optimize_used) {
          /* use of `tvar` is no longer dependent on another variable */
          to_remove = scheme_make_pair((Scheme_Object *)tvar,
                                       to_remove);
        } else
          tvar->optimize_used = 1;
      } else {
        /* It's possible that `tvar->optimize_used` is already 0; a variable
           is sometimes tenatively marked as used, and then unmarked */
        tvar->optimize_used = 0;
      }
    }
  }

  while (!SCHEME_NULLP(to_remove)) {
    scheme_hash_set(ht, SCHEME_CAR(to_remove), NULL);
    to_remove = SCHEME_CDR(to_remove);
  }
}

static void start_transitive_use_record(Optimize_Info *to_info, Optimize_Info *info, Scheme_IR_Local *var)
/* Start recording uses as tentative. Uses in a `lambda` as the RHS of
   the binding of `var` will only be used in the end of `var` itself
   is used. */
{
  if (var->optimize_used)
    return;

  info->transitive_use_var = var;

  /* Restore use flags, if any, saved from before: */
  if (var->optimize.transitive_uses)
    flip_transitive(var->optimize.transitive_uses, 1);
}

static void end_transitive_use_record(Optimize_Info *info)
/* Stop recording uses as tentative. */
{
  Scheme_IR_Local *var = info->transitive_use_var;

  if (var != info->next->transitive_use_var) {
    info->transitive_use_var = info->next->transitive_use_var;

    if (var->optimize.transitive_uses)
      flip_transitive(var->optimize.transitive_uses, 0);
  }
}

static Scheme_Object *optimize_lets(Scheme_Object *form, Optimize_Info *info, int context)
/* This is the main entry point for optimizing a `let[rec]-values` form. */
{
  Optimize_Info *body_info, *rhs_info;
  Optimize_Info_Sequence info_seq;
  Scheme_IR_Let_Header *head = (Scheme_IR_Let_Header *)form;
  Scheme_IR_Let_Value *irlv, *pre_body, *retry_start, *prev_body;
  Scheme_Object *body, *value, *ready_pairs = NULL, *rp_last = NULL, *ready_pairs_start;
  Scheme_Object *escape_body = scheme_false;
  Scheme_Once_Used *once_used;
  Scheme_Hash_Tree *merge_skip_vars;
  int i, j, is_rec, not_simply_let_star = 0, undiscourage, skip_opts = 0;
  int did_set_value, found_escapes;
  int remove_last_one = 0, inline_fuel;
  int pre_vclock, pre_aclock, pre_kclock, pre_sclock, increments_kclock = 0;
  int once_vclock, once_aclock, once_kclock, once_sclock, once_increments_kclock = 0;

  /* Special case: (let ([x M]) (if x x N)), where x is not in N,
     to (if M #t N), when the expression is in a test position
     or the result of M is a boolean?. */
  if (!(SCHEME_LET_FLAGS(head) & SCHEME_LET_RECURSIVE)
      && (head->count == 1)
      && (head->num_clauses == 1)) {
    irlv = (Scheme_IR_Let_Value *)head->body;
    if (SAME_TYPE(SCHEME_TYPE(irlv->body), scheme_branch_type)
        && (irlv->vars[0]->use_count == 2)) {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)irlv->body;
      if (SAME_OBJ(b->test, (Scheme_Object *)irlv->vars[0])
          && SAME_OBJ(b->tbranch, (Scheme_Object *)irlv->vars[0])) {
        Scheme_Object *pred;
         
        if (context & OPT_CONTEXT_BOOLEAN)
          /* In a boolean context, any expression can be moved. */
          pred = scheme_boolean_p_proc;
        else
          pred = expr_implies_predicate(irlv->value, info);
        
        if (pred && predicate_implies(pred, scheme_boolean_p_proc)) { 
          Scheme_Branch_Rec *b3;

          b3 = MALLOC_ONE_TAGGED(Scheme_Branch_Rec);
          b3->so.type = scheme_branch_type;
          b3->test = irlv->value;
          b3->tbranch = scheme_true;
          b3->fbranch = b->fbranch;

          form = scheme_optimize_expr((Scheme_Object *)b3, info, context);

          return form;
        }
      }
    }
  }

  is_rec = (SCHEME_LET_FLAGS(head) & SCHEME_LET_RECURSIVE);

  /* Special case: (let ([x E]) x) => E or (values E) */
  if (!is_rec
      && (head->count == 1)
      && (head->num_clauses == 1)) {
    irlv = (Scheme_IR_Let_Value *)head->body;
    if (SAME_OBJ((Scheme_Object *)irlv->vars[0], irlv->body)) {
      body = irlv->value;
      body = ensure_single_value_noncm(body, info);
      return scheme_optimize_expr(body, info, context);
    }
  }

  if (!is_rec) {
    int try_again;
    do {
      try_again = 0;
      /* (let ([x (let ([y M]) N)]) P) => (let ([y M]) (let ([x N]) P))
         or (let ([x (begin M ... N)]) P) => (begin M ... (let ([x N]) P)) */
      if (head->num_clauses) {
        irlv = (Scheme_IR_Let_Value *)head->body; /* ([x ...]) */
        if (SAME_TYPE(SCHEME_TYPE(irlv->value), scheme_ir_let_header_type)) {
          Scheme_IR_Let_Header *lh = (Scheme_IR_Let_Header *)irlv->value; /* (let ([y ...]) ...) */

          if (!lh->num_clauses) {
            irlv->value = lh->body;
            lh->body = (Scheme_Object *)head;
          } else {
            body = lh->body;
            for (i = lh->num_clauses - 1; i--; ) {
              body = ((Scheme_IR_Let_Value *)body)->body;
            }
            irlv->value = ((Scheme_IR_Let_Value *)body)->body; /* N */
            ((Scheme_IR_Let_Value *)body)->body = (Scheme_Object *)head;
          }
          
          head = lh;
          form = (Scheme_Object *)head;
          is_rec = (SCHEME_LET_FLAGS(head) & SCHEME_LET_RECURSIVE);
          try_again = !is_rec;
        } else if (SAME_TYPE(SCHEME_TYPE(irlv->value), scheme_sequence_type)) {
          Scheme_Sequence *seq = (Scheme_Sequence *)irlv->value; /* (begin M ... N) */

          irlv->value = seq->array[seq->count - 1];
          seq->array[seq->count - 1] = (Scheme_Object *)head;

          return scheme_optimize_expr((Scheme_Object *)seq, info, context);
        }
      }
    } while (try_again);
  }

  body_info = optimize_info_add_frame(info, head->count, head->count, 0);
  rhs_info = body_info;

  merge_skip_vars = scheme_make_hash_tree(SCHEME_hashtr_eq);
  body = head->body;
  for (i = head->num_clauses; i--; ) {
    pre_body = (Scheme_IR_Let_Value *)body;
    for (j = pre_body->count; j--; ) {
      merge_skip_vars = scheme_hash_tree_set(merge_skip_vars, (Scheme_Object *)pre_body->vars[j], scheme_true);
      set_optimize_mode(pre_body->vars[j]);
      pre_body->vars[j]->optimize.lambda_depth = body_info->lambda_depth;
      pre_body->vars[j]->optimize_used = 0;
      pre_body->vars[j]->optimize_outside_binding = 0;
      if (!pre_body->vars[j]->mutated && is_rec) {
        /* Indicate that it's not yet ready, so it cannot be inlined: */
        Scheme_Object *rp;
        pre_body->vars[j]->optimize_unready = 1;
        rp = scheme_make_raw_pair((Scheme_Object *)pre_body->vars[j], NULL);
        if (rp_last)
          SCHEME_CDR(rp_last) = rp;
        else
          ready_pairs = rp;
        rp_last = rp;
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
        pre_body = (Scheme_IR_Let_Value *)body;

        if ((pre_body->count == 1)
            && SCHEME_LAMBDAP(pre_body->value)
            && !pre_body->vars[0]->mutated) {
          Scheme_Object *sz;
          sz = estimate_closure_size(pre_body->value);
          pre_body->vars[0]->optimize.known_val = sz;
        }

        body = pre_body->body;
      }
      rhs_info->use_psize = 1;
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
    pre_body = (Scheme_IR_Let_Value *)body;

    if ((pre_body->count == 1)
        && SCHEME_LAMBDAP(pre_body->value)
        && !pre_body->vars[0]->optimize_used)
      start_transitive_use_record(body_info, rhs_info, pre_body->vars[0]);

    if (is_rec && OPT_DISCOURAGE_EARLY_INLINE && !rhs_info->letrec_not_twice
        && SCHEME_LAMBDAP(pre_body->value)) {
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
      pre_vclock = rhs_info->vclock;
      pre_aclock = rhs_info->aclock;
      pre_kclock = rhs_info->kclock;
      pre_sclock = rhs_info->sclock;
      if (!found_escapes) {
        optimize_info_seq_step(rhs_info, &info_seq);
        value = scheme_optimize_expr(pre_body->value, rhs_info,
                                     (((pre_body->count == 1)
                                       ? OPT_CONTEXT_SINGLED
                                       : 0)
                                      | (((pre_body->count == 1)
                                          && !pre_body->vars[0]->non_app_count)
                                         ? (pre_body->vars[0]->use_count << OPT_CONTEXT_APP_COUNT_SHIFT)
                                         : 0)));
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
      once_vclock = rhs_info->vclock;
      once_aclock = rhs_info->aclock;
      once_kclock = rhs_info->kclock;
      once_sclock = rhs_info->sclock;
      increments_kclock = (once_kclock > pre_kclock);
      once_increments_kclock = increments_kclock;
    } else {
      value = pre_body->value;
      --skip_opts;
      if (skip_opts) {
        /* when a `values` group is split, we've lost track of the
           clock values for points between the `values` arguments;
           we can conservatively assume the clock before the whole group
           for the purpose of registering once-used variables,
           but we can also conservatively advance the clock: */
        if (!found_escapes)
          advance_clocks_for_optimized(value,
                                       &pre_vclock, &pre_aclock, &pre_kclock, &pre_sclock,
                                       rhs_info,
                                       ADVANCE_CLOCKS_INIT_FUEL);
        once_vclock = pre_vclock;
        once_aclock = pre_aclock;
        once_kclock = pre_kclock;
        once_sclock = pre_sclock;       
      } else {
        /* end of split group, so rhs_info clock is right */
        once_vclock = rhs_info->vclock;
        once_aclock = rhs_info->aclock;
        once_kclock = rhs_info->kclock;
        once_sclock = rhs_info->sclock;
      }
      if (increments_kclock) {
        /* note that we conservatively assume that a member of a split
           advance the kclock, unless we can easily show otherwise */
        once_increments_kclock = 1;
      } 
    }

    if (undiscourage) {
      rhs_info->inline_fuel = inline_fuel;
      --rhs_info->letrec_not_twice;
    }

    end_transitive_use_record(rhs_info);

    if (is_rec && !not_simply_let_star) {
      /* Keep track of whether we can simplify to let*: */
      if (scheme_might_invoke_call_cc(value)
          || optimize_any_uses(body_info, pre_body, i+1))
        not_simply_let_star = 1;
    }

    /* Change (let-values ([(id ...) (values e ...)]) body)
       to (let-values ([id e] ...) body) for simple e.
       The is_values_apply() and related functions also handle
       (if id (values e1 ...) (values e2 ...)) to effectively convert to
       (values (if id e1 e2) ...) and then split the values call, since
       duplicating the id use and test is likely to pay off. */
    if ((pre_body->count != 1)
        && ((!is_rec && found_escapes)
            || (is_values_apply(value, pre_body->count, rhs_info, merge_skip_vars, 1)
                && ((!is_rec && no_mutable_bindings(pre_body))
                    /* If the right-hand side is omittable, then there are
                       no side effects, so mutation and recursiveness are ok */
                    || scheme_omittable_expr(value, pre_body->count, -1, 0, rhs_info, info))))) {
      if (!pre_body->count && !i) {
        /* We want to drop the clause entirely, but doing it
           here messes up the loop for letrec. So wait and
           remove it at the end. */
        remove_last_one = 1;
        /* If `found_escapes`, either this expression is the
           one that escaped, or `value` should have been simplified
           to `#f`. So, if it's not `#f`, we'll need to keep
           the expression part */
        if (!found_escapes)
          value = scheme_false;
        pre_body->value = value;
      } else {
        Scheme_IR_Let_Value *naya;
        Scheme_Object *rest = pre_body->body;
        int j;

        for (j = pre_body->count; j--; ) {
          Scheme_IR_Local **new_vars;
          naya = MALLOC_ONE_TAGGED(Scheme_IR_Let_Value);
          naya->iso.so.type = scheme_ir_let_value_type;
          naya->body = rest;
          naya->count = 1;
          new_vars = MALLOC_N(Scheme_IR_Local *, 1);
          new_vars[0] = pre_body->vars[j];
          naya->vars = new_vars;
          rest = (Scheme_Object *)naya;
        }

        naya = (Scheme_IR_Let_Value *)rest;
        if (!found_escapes) {
          unpack_values_application(value, naya, rhs_info, NULL);
        } else {
          Scheme_IR_Let_Value *naya2 = naya;
          for (j = 0; j < pre_body->count; j++) {
            if (!j)
              naya2->value = value;
            else
              naya2->value = scheme_false;
            naya2 = (Scheme_IR_Let_Value *)naya2->body;
          }

          if (!pre_body->count && !SCHEME_FALSEP(value)) {
            /* Since `value` is not false, this clause must be the one
               that is escaping. We'll end up dropping the remaining
               clauses and the original body, but we need to keep the
               erroring expression. */
            escape_body = value;
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

          if (skip_opts) {
            /* Use "pre" clocks: */
            if (!found_escapes)
              advance_clocks_for_optimized(value,
                                           &pre_vclock, &pre_aclock, &pre_kclock, &pre_sclock,
                                           rhs_info,
                                           ADVANCE_CLOCKS_INIT_FUEL);
            once_vclock = pre_vclock;
            once_aclock = pre_aclock;
            once_kclock = pre_kclock;
            once_sclock = pre_sclock;
          }
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

    if ((pre_body->count == 1) && !pre_body->vars[0]->mutated) {
      int indirect = 0, indirect_binding = 0;

      while (indirect < 10) {
        if (SAME_TYPE(SCHEME_TYPE(value), scheme_sequence_type)) {
          Scheme_Sequence *seq = (Scheme_Sequence *)value;
          value = seq->array[seq->count - 1];
          indirect++;
        } else if (SAME_TYPE(SCHEME_TYPE(value), scheme_with_cont_mark_type)) {
          Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)value;
          value = wcm->body;
          indirect++;
        } else if (SAME_TYPE(SCHEME_TYPE(value), scheme_ir_let_header_type)) {
          Scheme_IR_Let_Header *head2 = (Scheme_IR_Let_Header *)value;
          int i;

          if (head2->num_clauses < 10) {
            value = head2->body;
            for (i = head2->num_clauses; i--; ) {
              value = ((Scheme_IR_Let_Value *)value)->body;
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
        if (SCHEME_TYPE(value) < _scheme_ir_values_types_)
          value = NULL;
      }

      if (value && SAME_TYPE(SCHEME_TYPE(value), scheme_ir_local_type)) {
        /* Don't optimize reference to a local that's mutable; also,
           double-check that the value is ready, because we might be
           nested in the RHS of a `letrec': */
        if (SCHEME_VAR(value)->mutated || SCHEME_VAR(value)->optimize_unready)
          value = NULL;
      }

      if (value)
        value = extract_specialized_proc(value, value);

      if (value && ir_propagate_ok(value,
                                   body_info,
                                   (!indirect && (pre_body->vars[0]->use_count == 1)),
                                   pre_body->vars[0])) {
        pre_body->vars[0]->optimize.known_val = value;
        did_set_value = 1;
      } else if (value && !is_rec) {
        int cnt, ct, involves_k_cross;
        Scheme_Object *pred;

        ct = scheme_expr_produces_local_type(value, &involves_k_cross);
        if (ct) {
          SCHEME_VAR(pre_body->vars[0])->val_type = ct;
          if (involves_k_cross) {
            /* Although this variable's uses do not necessarily cross
               a continuation capture, the inference of its type
               depends on that crossing, so we treat as having a crossing.
               This is an accommodation to the bytecode format and
               validator, which has no way to distinguish between
               a known type and unboxing capability for that type. */
            SCHEME_VAR(pre_body->vars[0])->escapes_after_k_tick = 1;
          }
        }

        pred = expr_implies_predicate(value, rhs_info);

        if (pred)
          add_type(body_info, (Scheme_Object *)pre_body->vars[0], pred);

        if (!indirect) {
          cnt = pre_body->vars[0]->use_count;
          if (cnt == 1) {
            /* used only once; we may be able to shift the expression to the use
               site, instead of binding to a temporary */
            once_used = make_once_used(value, pre_body->vars[0],
                                       once_vclock, once_aclock, once_kclock, once_sclock,
                                       once_increments_kclock);
            pre_body->vars[0]->optimize.known_val = (Scheme_Object *)once_used;
            pre_body->vars[0]->optimize.clear_known_on_multi_use = 1;
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
	    || (!scheme_is_ir_lambda(((Scheme_IR_Let_Value *)pre_body->body)->value, 1, 1)
		&& !scheme_is_liftable(((Scheme_IR_Let_Value *)pre_body->body)->value, merge_skip_vars, 5, 1, 0)))) {
      Scheme_Object *prop_later = NULL;

      if (did_set_value) {
        /* Next RHS ends a reorderable sequence.
           Re-optimize from retry_start to pre_body, inclusive.
           For procedures, assume LAMBDA_SINGLE_RESULT and LAMBDA_PRESERVES_MARKS for all,
           but then assume not for all if any turn out not (i.e., approximate fix point). */
        int flags;
        Scheme_Object *clones, *cl, *cl_first;

        /* If this is the last binding, peek ahead in the body to
           check for easy type info in function calls */
        if (!i)
          set_application_types(pre_body->body, body_info, 5);

        /* Reset "unready" flags: */
        for (rp_last = ready_pairs_start; !SAME_OBJ(rp_last, ready_pairs); rp_last = SCHEME_CDR(rp_last)) {
          SCHEME_VAR(SCHEME_CAR(rp_last))->optimize_unready = 1;
        }
        /* Set-flags loop: */
        clones = make_clones(retry_start, pre_body, rhs_info);
        (void)set_code_flags(retry_start, pre_body, clones,
                             LAMBDA_SINGLE_RESULT | LAMBDA_PRESERVES_MARKS | LAMBDA_RESULT_TENTATIVE,
                             0xFFFF,
                             0,
                             0);
        /* Re-optimize loop: */
        irlv = retry_start;
        cl = clones;
        while (1) {
         value = irlv->value;
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

            if ((irlv->count == 1)
                && !irlv->vars[0]->optimize_used)
              start_transitive_use_record(body_info, rhs_info, irlv->vars[0]);

            cl = SCHEME_CDR(cl);
            self_value = SCHEME_CDR(cl_first);

            /* Drop old size, and remove old inline fuel: */
            sz = lambda_body_size(value, 0);
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
                                         (((irlv->count == 1)
                                           ? OPT_CONTEXT_SINGLED
                                           : 0)
                                          | (((irlv->count == 1)
                                              && !irlv->vars[0]->non_app_count)
                                             ? (irlv->vars[0]->use_count << OPT_CONTEXT_APP_COUNT_SHIFT)
                                             : 0)));
            
            if (!OPT_DISCOURAGE_EARLY_INLINE)
              --rhs_info->letrec_not_twice;
            rhs_info->use_psize = use_psize;

            irlv->value = value;

            if (!irlv->vars[0]->mutated) {
              if (ir_propagate_ok(value, rhs_info, irlv->vars[0]->use_count == 1, irlv->vars[0])) {
                /* Register re-optimized as the value for the binding, but
                   maybe only if it didn't grow too much: */
                int new_sz;
                if (OPT_DELAY_GROUP_PROPAGATE || OPT_LIMIT_FUNCTION_RESIZE)
                  new_sz = lambda_body_size(value, 0);
                else
                  new_sz = 0;
                if (new_sz <= sz) {
                  irlv->vars[0]->optimize.known_val = value;
                }
                else if (!OPT_LIMIT_FUNCTION_RESIZE
                         || (new_sz < 4 * sz))
                  prop_later = scheme_make_raw_pair(scheme_make_pair((Scheme_Object *)irlv->vars[0],
                                                                     value),
                                                    prop_later);
              }
            }

            end_transitive_use_record(rhs_info);
	  }
	  if (irlv == pre_body)
	    break;
          {
            /* Since letrec is really letrec*, the variables
               for this binding are now ready: */
            int i;
            for (i = irlv->count; i--; ) {
              if (!irlv->vars[i]->mutated) {
                SCHEME_VAR(SCHEME_CAR(ready_pairs_start))->optimize_unready = 0;
                ready_pairs_start = SCHEME_CDR(ready_pairs_start);
              }
            }
          }
	  irlv = (Scheme_IR_Let_Value *)irlv->body;
	}
        /* Check flags loop: */
        flags = set_code_flags(retry_start, pre_body, clones, 0, 0xFFFF, 0, 0);
        /* Reset-flags loop: */
        (void)set_code_flags(retry_start, pre_body, clones,
                             (flags & (LAMBDA_SINGLE_RESULT | LAMBDA_PRESERVES_MARKS)),
                             ~(LAMBDA_SINGLE_RESULT | LAMBDA_PRESERVES_MARKS | LAMBDA_RESULT_TENTATIVE),
                             1,
                             1);
      }
      retry_start = NULL;
      ready_pairs_start = NULL;
      did_set_value = 0;

      while (prop_later) {
        value = SCHEME_CAR(prop_later);
        SCHEME_VAR(SCHEME_CAR(value))->optimize.known_val = SCHEME_CDR(value);
        prop_later = SCHEME_CDR(prop_later);
      }
    }

    if (is_rec) {
      /* Since letrec is really letrec*, the variables
         for this binding are now ready: */
      int i;
      for (i = pre_body->count; i--; ) {
        pre_body->vars[i]->optimize.init_kclock = rhs_info->kclock;
        if (!pre_body->vars[i]->mutated) {
          SCHEME_VAR(SCHEME_CAR(ready_pairs))->optimize_unready = 0;
          ready_pairs = SCHEME_CDR(ready_pairs);
        }
      }
    }

    if (remove_last_one) {
      head->num_clauses -= 1;
      body = (Scheme_Object *)pre_body->body;

      if (found_escapes && !SCHEME_FALSEP(pre_body->value)) {
        /* Since `pre_body->value` wasn't simplified to #f,
           keep this as the new body */
        escape_body = pre_body->value;
      }

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

  if (!is_rec) {
    /* All `let`-bound variables are now allocated: */
    body = head->body;
    for (i = head->num_clauses; i--; ) {
      pre_body = (Scheme_IR_Let_Value *)body;
      for (j = pre_body->count; j--; ) {
        pre_body->vars[j]->optimize.init_kclock = body_info->kclock;
      }
      body = pre_body->body;
    }
  }

  optimize_info_seq_done(body_info, &info_seq);

  if (!found_escapes) {
    body = scheme_optimize_expr(body, body_info, scheme_optimize_tail_context(context));
  } else {
    body = ensure_noncm(escape_body, body_info);
    body_info->single_result = 1;
    body_info->preserves_marks = 1;
    body_info->escapes = 1;
    body_info->size++;
  }
  if (head->num_clauses)
    pre_body->body = body;
  else
    head->body = body;

  /* Propagate any use from formerly tentative uses: */
  while (1) {
    int changed = 0;
    body = head->body;
    for (i = head->num_clauses; i--; ) {
      pre_body = (Scheme_IR_Let_Value *)body;
      for (j = pre_body->count; j--; ) {
        if (pre_body->vars[j]->optimize_used
            && pre_body->vars[j]->optimize.transitive_uses) {
          register_transitive_uses(pre_body->vars[j], body_info);
          changed = 1;
          pre_body->vars[j]->optimize.transitive_uses = NULL;
        }
      }
      body = pre_body->body;
    }
    if (!changed)
      break;
  }

  info->single_result = body_info->single_result;
  info->preserves_marks = body_info->preserves_marks;
  info->vclock = body_info->vclock;
  info->aclock = body_info->aclock;
  info->kclock = body_info->kclock;
  info->sclock = body_info->sclock;

  /* Clear used flags where possible, clear once-used references, etc. */
  body = head->body;
  prev_body = NULL;
  for (i = head->num_clauses; i--; ) {
    int used = 0, j;

    pre_body = (Scheme_IR_Let_Value *)body;

    if (pre_body->count == 1) {
      /* If the right-hand side is a function, make sure all use sites
         are accounted for toward type inference of arguments. */
      if (pre_body->vars[0]->optimize.known_val
          && SAME_TYPE(SCHEME_TYPE(pre_body->vars[0]->optimize.known_val), scheme_lambda_type)) {
        check_lambda_arg_types_registered((Scheme_Lambda *)pre_body->vars[0]->optimize.known_val,
                                          pre_body->vars[0]->use_count);
      }
    }

    for (j = pre_body->count; j--; ) {
      if (pre_body->vars[j]->optimize_used) {
        used = 1;
        break;
      }
    }

    /* once-used moved implies not optimize_used: */
    MZ_ASSERT(!(used
                && (pre_body->count == 1)
                && pre_body->vars[0]->optimize.known_val
                && SAME_TYPE(scheme_once_used_type, SCHEME_TYPE(pre_body->vars[0]->optimize.known_val))
                && ((Scheme_Once_Used *)pre_body->vars[0]->optimize.known_val)->moved));

    if (!used
        && (scheme_omittable_expr(pre_body->value, pre_body->count, -1, 0, info, info)
            || ((pre_body->count == 1)
                && pre_body->vars[0]->optimize.known_val
                && SAME_TYPE(scheme_once_used_type, SCHEME_TYPE(pre_body->vars[0]->optimize.known_val))
                && ((Scheme_Once_Used *)pre_body->vars[0]->optimize.known_val)->moved))) {
      /* Drop the binding(s) */
      for (j = pre_body->count; j--; ) {
        pre_body->vars[j]->mode = SCHEME_VAR_MODE_NONE;
      }
      head->num_clauses -= 1;
      head->count -= pre_body->count;
      if (prev_body)
        prev_body->body = pre_body->body;
      else
        head->body = pre_body->body;
      /* Deduct from size to aid further inlining. */
      {
        int sz;
        sz = expr_size(pre_body->value);
        body_info->size -= sz;
      }
    } else {
      if (!used && (pre_body->count == 1)) {
        /* The whole binding is not omittable, but maybe the tail is omittable: */
        Scheme_Object *v2 = pre_body->value, *inside;
        extract_tail_inside(&v2, &inside);
        if (scheme_omittable_expr(v2, pre_body->count, -1, 0, info, info)) {
          replace_tail_inside(scheme_false, inside, pre_body->value);
        }
      }

      for (j = pre_body->count; j--; ) {
        int ct;

        pre_body->vars[j]->optimize_outside_binding = 1;
        if (pre_body->vars[j]->optimize.known_val
            && SAME_TYPE(scheme_once_used_type, SCHEME_TYPE(pre_body->vars[j]->optimize.known_val))) {
          /* We're keeping this clause here, so don't allow movement of the once-used
             value when peeking under bindings via extract_tail_inside(): */
          pre_body->vars[j]->optimize.known_val = NULL;
        }

        ct = pre_body->vars[j]->arg_type;
        if (ct) {
          if (ALWAYS_PREFER_UNBOX_TYPE(ct)
              || !pre_body->vars[j]->escapes_after_k_tick)
            pre_body->vars[j]->arg_type = ct;
        }
      }
      info->size += 1;
      prev_body = pre_body;
    }
    body = pre_body->body;
  }

  optimize_info_done(body_info, NULL);
  merge_types(body_info, info, merge_skip_vars);

  if (is_rec && !not_simply_let_star) {
    /* We can simplify letrec to let* */
    SCHEME_LET_FLAGS(head) -= SCHEME_LET_RECURSIVE;
    is_rec = 0;
    optimize_uses_of_mutable_imply_early_alloc((Scheme_IR_Let_Value *)head->body, head->num_clauses);
  }

  /* Optimized away all clauses? */
  if (!head->num_clauses) {
    return body;
  }

  if (!is_rec
      && ((SCHEME_TYPE(body) > _scheme_ir_values_types_)
          || SAME_TYPE(SCHEME_TYPE(body), scheme_ir_toplevel_type)
          || SAME_TYPE(SCHEME_TYPE(body), scheme_ir_local_type))) {
    /* If the body is a constant, toplevel or another local, the last binding
       is unused, so reduce (let ([x <expr>]) K) => (begin <expr> K). 
       As a special case, include a second check for (let ([x E]) x) => E or (values E). */ 
    Scheme_Object *inside;

    inside = (Scheme_Object *)head;
    pre_body = (Scheme_IR_Let_Value *)head->body;
    for (i = head->num_clauses - 1; i--; ) {
      inside = (Scheme_Object *)pre_body;
      pre_body = (Scheme_IR_Let_Value *)pre_body->body;
    }

    if (pre_body->count == 1) {
      if (!SAME_OBJ((Scheme_Object *)pre_body->vars[0], body)
          && !found_escapes) {
        body = make_discarding_sequence(pre_body->value, body, info);
      } else {
        /* Special case for (let ([x E]) x) and (let ([x <error>]) #f) */
        body = pre_body->value;
        body = ensure_single_value_noncm(body, info);
        if (found_escapes) {
          found_escapes = 0; /* Perhaps the error is moved to the body. */
          body = ensure_noncm(body, info);
        }
      }

      if (head->num_clauses == 1)
        return body;

      (void)replace_tail_inside(body, inside, NULL);
      head->count--;
      head->num_clauses--;
    }
  }

  if (!is_rec) {
    /* One last pass to peel off unused bindings */
    Scheme_Object *prev = NULL, *rhs;

    body = head->body;
    for (i = head->num_clauses; i--; ) {
      pre_body = (Scheme_IR_Let_Value *)body;
      if ((pre_body->count == 1)
          && !pre_body->vars[0]->optimize_used) {
        Scheme_Sequence *seq;
        Scheme_Object *new_body;

        pre_body->vars[0]->mode = SCHEME_VAR_MODE_NONE;
    
        seq = scheme_malloc_sequence(2);
        seq->so.type = scheme_sequence_type;
        seq->count = 2;

        rhs = pre_body->value;
        rhs = ensure_single_value_noncm(rhs, info);
        seq->array[0] = rhs;

        head->count--;
        head->num_clauses--;
        head->body = pre_body->body;

        new_body = (Scheme_Object *)seq;

        if (head->num_clauses)
          seq->array[1] = (Scheme_Object *)head;
        else if (found_escapes && SCHEME_FALSEP(head->body)) {
          /* don't need the `#f` for the body, because some RHS escapes */
          new_body = ensure_noncm(rhs, info);
        } else
          seq->array[1] = head->body;
                
        if (prev)
          (void)replace_tail_inside(new_body, prev, NULL);
        else
          form = new_body;
        prev = new_body;

        body = pre_body->body;
      } else
        break;
    }

    if (prev && SAME_TYPE(SCHEME_TYPE(prev), scheme_sequence_type))
      form = optimize_sequence(form, info, context, 0);
  }

  return form;
}

/*========================================================================*/
/*                               lambda                                   */
/*========================================================================*/

static Scheme_Object *
optimize_lambda(Scheme_Object *_lam, Optimize_Info *info, int context)
{
  Scheme_Lambda *lam;
  Scheme_Object *code, *ctx;
  Scheme_IR_Lambda_Info *cl;
  int i, init_vclock, init_aclock, init_kclock, init_sclock;
  Scheme_Hash_Table *ht;
  int app_count = OPT_CONTEXT_APP_COUNT(context);

  lam = (Scheme_Lambda *)_lam;

  info->single_result = 1;
  info->preserves_marks = 1;

  info = optimize_info_add_frame(info, lam->num_params, lam->num_params,
                                 SCHEME_LAMBDA_FRAME);

  ht = scheme_make_hash_table(SCHEME_hash_ptr);
  info->uses = ht;

  init_vclock = info->vclock;
  init_aclock = info->aclock;
  init_kclock = info->kclock;
  init_sclock = info->sclock;

  info->vclock += 1; /* model delayed evaluation as vclock increment */
  info->kclock += 1;
  info->sclock += 1;

  /* For reporting warnings: */
  if (info->context && SCHEME_PAIRP(info->context))
    ctx = scheme_make_pair((Scheme_Object *)lam,
                           SCHEME_CDR(info->context));
  else if (info->context)
    ctx = scheme_make_pair((Scheme_Object *)lam, info->context);
  else
    ctx = (Scheme_Object *)lam;
  info->context = ctx;

  cl = lam->ir_info;
  for (i = 0; i < lam->num_params; i++) {
    set_optimize_mode(cl->vars[i]);
    cl->vars[i]->optimize.lambda_depth = info->lambda_depth;
    cl->vars[i]->optimize_used = 0;
    cl->vars[i]->optimize.init_kclock = info->kclock;
    if (app_count
        && (app_count < SCHEME_USE_COUNT_INF)
        && cl->arg_types
        && cl->arg_types[i]
        && (cl->arg_type_contributors[i] == ((1 << app_count) - 1))) {
      /* All uses accounted for, so we can rely on type info */
      add_type(info, (Scheme_Object *)cl->vars[i], cl->arg_types[i]);
    }
  }

  code = scheme_optimize_expr(lam->body, info, 0);

  propagate_used_variables(info);

  if (info->single_result)
    SCHEME_LAMBDA_FLAGS(lam) |= LAMBDA_SINGLE_RESULT;
  else if (SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_SINGLE_RESULT)
    SCHEME_LAMBDA_FLAGS(lam) -= LAMBDA_SINGLE_RESULT;

  if (info->preserves_marks)
    SCHEME_LAMBDA_FLAGS(lam) |= LAMBDA_PRESERVES_MARKS;
  else if (SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_PRESERVES_MARKS)
    SCHEME_LAMBDA_FLAGS(lam) -= LAMBDA_PRESERVES_MARKS;

  if ((info->single_result > 0) && (info->preserves_marks > 0)
      && (SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_RESULT_TENTATIVE))
    SCHEME_LAMBDA_FLAGS(lam) -= LAMBDA_RESULT_TENTATIVE;

  lam->body = code;

  /* Remembers positions of used vars (and unsets usage for this level) */
  cl->base_closure = info->uses;
  if (env_uses_toplevel(info))
    cl->has_tl = 1;
  else
    cl->has_tl = 0;
  cl->body_size = info->size;
  cl->body_psize = info->psize;
  cl->has_nonleaf = info->has_nonleaf;

  /* closure itself is not an effect */
  info->vclock = init_vclock;
  info->aclock = init_aclock;
  info->kclock = init_kclock;
  info->sclock = init_sclock;
  info->escapes = 0;

  info->size++;

  lam->closure_size = (cl->base_closure->count
                       + (cl->has_tl ? 1 : 0));

  optimize_info_done(info, NULL);

  return (Scheme_Object *)lam;
}

static void merge_lambda_arg_types(Scheme_Lambda *lam1, Scheme_Lambda *lam2)
{
  Scheme_IR_Lambda_Info *cl1 = lam1->ir_info;
  Scheme_IR_Lambda_Info *cl2 = lam2->ir_info;
  int i;

  if (!cl1->arg_types) {
    if (cl2->arg_types) {
      cl1->arg_types = cl2->arg_types;
      cl1->arg_type_contributors = cl2->arg_type_contributors;
    }
  } else {
    if (cl2->arg_types) {
      for (i = lam1->num_params; i--; ) {
        if (!cl1->arg_type_contributors[i]) {
          cl1->arg_types[i] = cl2->arg_types[i];
          cl1->arg_type_contributors[i] = cl2->arg_type_contributors[i];
        } else if (cl2->arg_type_contributors[i]) {
          if (!cl2->arg_types[i])
            cl1->arg_types[i] = NULL;
          else if (predicate_implies(cl1->arg_types[i], cl2->arg_types[i]))
            cl1->arg_types[i] = cl2->arg_types[i];
          else if (!predicate_implies(cl2->arg_types[i], cl1->arg_types[i])) {
            cl1->arg_types[i] = NULL;
            cl1->arg_type_contributors[i] |= (1 << (SCHEME_USE_COUNT_INF-1));
          }
          cl1->arg_type_contributors[i] |= cl2->arg_type_contributors[i];
        }
      }
    }

    cl2->arg_types = cl1->arg_types;
    cl2->arg_type_contributors = cl1->arg_type_contributors;
  }
}

static void check_lambda_arg_types_registered(Scheme_Lambda *lam, int app_count)
{
  if (lam->ir_info->arg_types) {
    int i;
    for (i = lam->num_params; i--; ) {
      if (lam->ir_info->arg_types[i]) {
        if ((lam->ir_info->arg_type_contributors[i] & (1 << (SCHEME_USE_COUNT_INF-1)))
            || (lam->ir_info->arg_type_contributors[i] < ((1 << app_count) - 1))) {
          /* someone caller didn't weigh in with a type,
             of an anonymous caller had no type to record */
          lam->ir_info->arg_types[i] = NULL;
        }
      }
    }
  }
}

static Scheme_IR_Local *clone_variable(Scheme_IR_Local *var)
{
  Scheme_IR_Local *var2;
  MZ_ASSERT(SAME_TYPE(var->so.type, scheme_ir_local_type));
  var2 = MALLOC_ONE_TAGGED(Scheme_IR_Local);
  memcpy(var2, var, sizeof(Scheme_IR_Local));
  return var2;
}

static Scheme_IR_Local **clone_variable_array(Scheme_IR_Local **vars,
                                              int sz,
                                              Scheme_Hash_Tree **_var_map)
{
  Scheme_IR_Local **new_vars, *var;
  Scheme_Hash_Tree *var_map = *_var_map;
  int j;
  
  new_vars = MALLOC_N(Scheme_IR_Local*, sz);
  for (j = sz; j--; ) {
    var = clone_variable(vars[j]);
    var->mode = SCHEME_VAR_MODE_NONE;
    new_vars[j] = var;
    var_map = scheme_hash_tree_set(var_map, (Scheme_Object *)vars[j], (Scheme_Object *)new_vars[j]);
  }

  *_var_map = var_map;
  return new_vars;
}

static Scheme_Object *clone_lambda(int single_use, Scheme_Object *_lam, Optimize_Info *info, Scheme_Hash_Tree *var_map)
{
  Scheme_Lambda *lam, *lam2;
  Scheme_Object *body, *var;
  Scheme_Hash_Table *ht;
  Scheme_IR_Lambda_Info *cl;
  Scheme_IR_Local **vars;
  int sz;
  Scheme_Object **arg_types;
  short *arg_type_contributors;

  lam = (Scheme_Lambda *)_lam;

  lam2 = MALLOC_ONE_TAGGED(Scheme_Lambda);
  memcpy(lam2, lam, sizeof(Scheme_Lambda));

  cl = MALLOC_ONE_RT(Scheme_IR_Lambda_Info);
  memcpy(cl, lam->ir_info, sizeof(Scheme_IR_Lambda_Info));
  lam2->ir_info = cl;

  vars = clone_variable_array(cl->vars, lam2->num_params, &var_map);
  cl->vars = vars;

  cl->is_dup |= !single_use;

  body = optimize_clone(single_use, lam->body, info, var_map, 0);
  if (!body) return NULL;

  lam2->body = body;

  if (cl->arg_types) {
    sz = lam2->num_params;
    arg_types = MALLOC_N(Scheme_Object*, sz);
    arg_type_contributors = MALLOC_N_ATOMIC(short, sz);
    memcpy(arg_types, cl->arg_types, sz * sizeof(Scheme_Object*));
    memcpy(arg_type_contributors, cl->arg_type_contributors, sz * sizeof(short));
    cl->arg_types = arg_types;
    cl->arg_type_contributors = arg_type_contributors;
  }

  if (cl->base_closure && var_map->count) {
    int i;
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    for (i = 0; i < cl->base_closure->size; i++) {
      if (cl->base_closure->vals[i]) {
        var = scheme_hash_tree_get(var_map, cl->base_closure->keys[i]);
        scheme_hash_set(ht,
                        (var
                         ? var
                         : cl->base_closure->keys[i]),
                        cl->base_closure->vals[i]);
      }
    }
    cl->base_closure = ht;
  }

  return (Scheme_Object *)lam2;
}

static int lambda_body_size_plus_info(Scheme_Lambda *lam, int check_assign,
                                      Optimize_Info *info, int *is_leaf)
{
  int i;
  Scheme_IR_Lambda_Info *cl;

  cl = lam->ir_info;

  if (check_assign) {
    /* Don't try to inline if any arguments are mutated: */
    for (i = lam->num_params; i--; ) {
      if (cl->vars[i]->mutated)
	return -1;
    }
  }

  if (is_leaf)
    *is_leaf = !cl->has_nonleaf;

  return cl->body_size + ((info && info->use_psize) ? cl->body_psize : 0);
}

static int lambda_has_top_level(Scheme_Lambda *lam)
{
  return lam->ir_info->has_tl;
}

/*========================================================================*/
/*                              modules                                   */
/*========================================================================*/

static int set_code_closure_flags(Scheme_Object *clones,
                                  int set_flags, int mask_flags,
                                  int just_tentative)
{
  Scheme_Object *clone, *orig, *first;
  int flags = LAMBDA_SINGLE_RESULT | LAMBDA_PRESERVES_MARKS;

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
  if (SCHEME_LAMBDAP(e)) {
    if (size_override || (lambda_body_size(e, 1) < CROSS_MODULE_INLINE_SIZE))
      return optimize_clone(0, e, info, empty_eq_hash_tree, 0);
  }

  return NULL;
}

static int is_general_lambda(Scheme_Object *e, Optimize_Info *info)
{
  /* recognize (begin <omitable>* <proc>) */
  if (SCHEME_TYPE(e) == scheme_sequence_type) {
    Scheme_Sequence *seq = (Scheme_Sequence *)e;
    if (seq->count > 0) {
      int i;
      for (i = seq->count - 1; i--; ) {
        if (!scheme_omittable_expr(seq->array[i], -1, 20, 0, info, NULL))
          return 0;
      }
    }
    e = seq->array[seq->count - 1];
  }

  /* recognize (let ([x <proc>]) x) */
  if (SCHEME_TYPE(e) == scheme_ir_let_header_type) {
    Scheme_IR_Let_Header *lh = (Scheme_IR_Let_Header *)e;
    if (!(SCHEME_LET_FLAGS(lh) & SCHEME_LET_RECURSIVE)
        && (lh->count == 1) 
        && (lh->num_clauses == 1)
        && SAME_TYPE(SCHEME_TYPE(lh->body), scheme_ir_let_value_type)) {
      Scheme_IR_Let_Value *lv = (Scheme_IR_Let_Value *)lh->body;
      if (SCHEME_LAMBDAP(lv->value))
        return SAME_OBJ(lv->body, (Scheme_Object *)lv->vars[0]);
    }
  }

  if (SCHEME_LAMBDAP(e))
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
  if (SAME_TYPE(SCHEME_TYPE(e), scheme_ir_let_header_type)) {
    /* This is a tedious case to recognize the pattern
         (let ([x rhs] ...) (values x ...))
       which might be the result of expansion that involved a local
       macro to define the `x's */
    Scheme_IR_Let_Header *lh = (Scheme_IR_Let_Header *)e;
    if ((lh->count == n) && (lh->num_clauses == n)
        && !(SCHEME_LET_FLAGS(lh) & SCHEME_LET_RECURSIVE)) {
      Scheme_Object *body = lh->body;
      int i;
      for (i = 0; i < n; i++) {
        if (SAME_TYPE(SCHEME_TYPE(body), scheme_ir_let_value_type)) {
          Scheme_IR_Let_Value *lv = (Scheme_IR_Let_Value *)body;
          if (lv->count == 1) {
            if (!scheme_omittable_expr(lv->value, 1, 5, 0, NULL, NULL))
              return 0;
            body = lv->body;
          } else
            return 0;
        } else
          return 0;
      }
      if ((n == 2) && SAME_TYPE(SCHEME_TYPE(body), scheme_application3_type)) {
        Scheme_App3_Rec *app = (Scheme_App3_Rec *)body;
        Scheme_IR_Let_Value *lv = (Scheme_IR_Let_Value *)lh->body;
        if (SAME_OBJ(app->rator, scheme_values_proc)
            && SAME_OBJ(app->rand1, (Scheme_Object *)lv->vars[0])
            && SAME_OBJ(app->rand2, (Scheme_Object *)((Scheme_IR_Let_Value *)lv->body)->vars[0])) {
          if (vars) {
            install_definition(vec, offset, SCHEME_CAR(vars), lv->value);
            vars = SCHEME_CDR(vars);
            lv = (Scheme_IR_Let_Value *)lv->body;
            install_definition(vec, offset+1, SCHEME_CAR(vars), lv->value);
          }
          return 1;
        }
      } else if (SAME_TYPE(SCHEME_TYPE(body), scheme_application_type)
                 && ((Scheme_App_Rec *)body)->num_args == n) {
        Scheme_App_Rec *app = (Scheme_App_Rec *)body;
        Scheme_IR_Let_Value *lv = (Scheme_IR_Let_Value *)lh->body;
        if (SAME_OBJ(app->args[0], scheme_values_proc)) {
          for (i = 0; i < n; i++) {
            if (!SAME_TYPE(SCHEME_TYPE(app->args[i+1]), scheme_ir_local_type)
                || !SAME_OBJ((Scheme_Object *)lv->vars[0], app->args[i+1]))
              return 0;
            lv = (Scheme_IR_Let_Value *)lv->body;
          }
          if (vars) {
            body = lh->body;
            for (i = 0; i < n; i++) {
              Scheme_IR_Let_Value *lv2 = (Scheme_IR_Let_Value *)body;
              install_definition(vec, offset+i, SCHEME_CAR(vars), lv2->value);
              vars = SCHEME_CDR(vars);
              body = lv2->body;
            }
          }
          return 1;
        }
      }
    }    
  } else if ((n == 2) && SAME_TYPE(SCHEME_TYPE(e), scheme_application3_type)) {
    Scheme_App3_Rec *app = (Scheme_App3_Rec *)e;
    if (SAME_OBJ(app->rator, scheme_values_proc)
        && scheme_omittable_expr(app->rand1, 1, 5, 0, NULL, NULL)
        && scheme_omittable_expr(app->rand2, 1, 5, 0, NULL, NULL)) {
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
    if (SAME_OBJ(app->args[0], scheme_values_proc)) {
      int i;
      for (i = 0; i < n; i++) {
        if (!scheme_omittable_expr(app->args[i+1], 1, 5, 0, NULL, NULL))
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

static Scheme_Hash_Table *set_as_fixed(Scheme_Hash_Table *fixed_table, Optimize_Info *info, int pos)
{
  if (!fixed_table) {
    fixed_table = scheme_make_hash_table(SCHEME_hash_ptr);
    if (!info->top_level_consts) {
      Scheme_Hash_Table *consts;
      consts = scheme_make_hash_table(SCHEME_hash_ptr);
      info->top_level_consts = consts;
    }
    scheme_hash_set(info->top_level_consts, scheme_false, (Scheme_Object *)fixed_table);
  }
  
  scheme_hash_set(fixed_table, scheme_make_integer(pos), scheme_true);

  return fixed_table;
}

static Scheme_Object *
module_optimize(Scheme_Object *data, Optimize_Info *info, int context)
{
  Scheme_Module *m = (Scheme_Module *)data;
  Scheme_Object *e, *vars, *old_context;
  int start_simultaneous = 0, i_m, cnt;
  Scheme_Object *cl_first = NULL, *cl_last = NULL;
  Scheme_Hash_Table *consts = NULL, *fixed_table = NULL, *re_consts = NULL;
  Scheme_Hash_Table *originals = NULL;
  int cont, next_pos_ready = -1, inline_fuel, is_proc_def;
  Comp_Prefix *prev_cp;
  Optimize_Info *limited_info;
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

  /* Use `limited_info` for optimization decisions that need to be
     rediscovered by the validator. The validator knows shape
     information for imported variables, and it knows about structure
     bindings for later forms. */
  limited_info = MALLOC_ONE_RT(Optimize_Info);
#ifdef MZTAG_REQUIRED
  limited_info->type = scheme_rt_optimize_info;
#endif
  limited_info->cp = info->cp;

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
          if ((n == 1) && SCHEME_LAMBDAP(e))  {
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
        if (is_general_lambda(e2, info))
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
      if (SAME_TYPE(SCHEME_TYPE(e), scheme_define_values_type)) {
	int n, cnst = 0, sproc = 0, sprop = 0, has_guard = 0;
        Scheme_Object *sstruct = NULL, *parent_identity = NULL;
        Simple_Struct_Type_Info stinfo;

	vars = SCHEME_VEC_ELS(e)[0];
	e = SCHEME_VEC_ELS(e)[1];

	n = scheme_list_length(vars);
	cont = scheme_omittable_expr(e, n, -1,
                                     /* ignore APPN_FLAG_OMITTABLE, because the
                                        validator won't be able to reconstruct it
                                        in general; also, don't recognize struct-type
                                        functions, since they weren't recognized
                                        as immediate calls */
                                     (OMITTABLE_IGNORE_APPN_OMIT
                                      | OMITTABLE_IGNORE_MAKE_STRUCT_TYPE),
                                     /* similarly, use `limited_info` instead of `info'
                                        here, because the decision
                                        of omittable should not depend on
                                        information that's only available at
                                        optimization time: */
                                     limited_info, 
                                     info);

        if (n == 1) {
          if (ir_propagate_ok(e, info, 0, NULL))
            cnst = 1;
          else if (scheme_is_statically_proc(e, info, OMITTABLE_IGNORE_APPN_OMIT)) {
            cnst = 1;
            sproc = 1;
          }
        } else if (scheme_is_simple_make_struct_type(e, n, 0, NULL, 
                                                     &stinfo, &parent_identity,
                                                     info->top_level_consts, 
                                                     info->cp->inline_variants, 
                                                     NULL, NULL, 0, NULL, NULL,
                                                     &sstruct,
                                                     5)) {
          sstruct = scheme_make_pair(sstruct, parent_identity);
          cnst = 1;
        } else if (scheme_is_simple_make_struct_type_property(e, n, 0,
                                                              &has_guard, 
                                                              info->top_level_consts, 
                                                              info->cp->inline_variants, 
                                                              NULL, NULL, 0, NULL, NULL,
                                                              5)) {
          sprop = 1;
          cnst = 1;
        } else
          sstruct = NULL;

        if ((sstruct || sprop) && !cont) {
          /* Since the `make-struct-type` or `make-struct-tye-property` form is immediate
             enough that the validator can see it, re-check whether we can continue
             a group of simultaneously defined variables. */
          cont = scheme_omittable_expr(e, n, 5, OMITTABLE_IGNORE_APPN_OMIT, limited_info, NULL);
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
                e2 = scheme_make_struct_proc_shape(scheme_get_struct_proc_shape(i, &stinfo),
                                                   sstruct);
              } else if (sprop) {
                e2 = scheme_make_struct_property_proc_shape(scheme_get_struct_property_proc_shape(i, has_guard));
              } else if (sproc) {
                e2 = scheme_make_noninline_proc(e);
              } else if (SCHEME_LAMBDAP(e)) {
                e2 = optimize_clone(1, e, info, empty_eq_hash_tree, 0);
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

                if (sstruct || sprop) {
                  /* include in `limited_info` */
                  Scheme_Hash_Table *limited_consts = limited_info->top_level_consts;
                  if (!limited_consts) {
                    limited_consts = scheme_make_hash_table(SCHEME_hash_ptr);
                    limited_info->top_level_consts = limited_consts;
                  }
                  scheme_hash_set(limited_consts, scheme_make_integer(pos), e2);
                }

                if (sstruct || (SCHEME_TYPE(e2) > _scheme_ir_values_types_)) {
                  /* No use re-optimizing */
                } else {
                  if (!re_consts)
                    re_consts = scheme_make_hash_table(SCHEME_hash_ptr);
                  scheme_hash_set(re_consts, scheme_make_integer(i_m),
                                  scheme_make_integer(pos));
                }
              } else {
                /* At least mark it as fixed */
                fixed_table = set_as_fixed(fixed_table, info, tl->position);
              }
            }
          }
	} else {
	  /* The binding is not inlinable/propagatable, but unless it's
	     set!ed, it is constant after evaluating the definition. We
	     map the top-level position to indicate constantness --- immediately
             if `cont`, and later if not. */
	  Scheme_Object *l, *a;
	  int pos;

	  for (l = vars; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
	    a = SCHEME_CAR(l);

	    /* Test for set!: */
	    if (!(SCHEME_TOPLEVEL_FLAGS(a) & SCHEME_TOPLEVEL_MUTATED)) {
	      pos = SCHEME_TOPLEVEL_POS(a);

              if (cont)
                fixed_table = set_as_fixed(fixed_table, info, pos);
              else
                next_pos_ready = pos;
	    }
	  }
	}
      } else {
	cont = scheme_omittable_expr(e, -1, -1, 0, NULL, NULL);
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

        /* Same as in letrec: assume LAMBDA_SINGLE_RESULT and
           LAMBDA_PRESERVES_MARKS for all, but then assume not for all
           if any turn out not (i.e., approximate fix point). */
        (void)set_code_closure_flags(cl_first,
                                     LAMBDA_SINGLE_RESULT | LAMBDA_PRESERVES_MARKS | LAMBDA_RESULT_TENTATIVE,
                                     0xFFFF,
                                     0);

	while (1) {
	  /* Re-optimize this expression. */
          int old_sz, new_sz;

          e = SCHEME_VEC_ELS(m->bodies[0])[start_simultaneous];

          if (OPT_DELAY_GROUP_PROPAGATE || OPT_LIMIT_FUNCTION_RESIZE) {
            if (SAME_TYPE(SCHEME_TYPE(e), scheme_define_values_type)) {
              Scheme_Object *sub_e;
              sub_e = SCHEME_VEC_ELS(e)[1];
              old_sz = lambda_body_size(sub_e, 0);
            } else
              old_sz = 0;
          } else
            old_sz = 0;

          optimize_info_seq_step(info, &info_seq);
          e = scheme_optimize_expr(e, info, 0);
	  SCHEME_VEC_ELS(m->bodies[0])[start_simultaneous] = e;

          if (re_consts) {
            /* Install optimized closures into constant table ---
               unless, maybe, they grow too much: */
            Scheme_Object *rpos;
            rpos = scheme_hash_get(re_consts, scheme_make_integer(start_simultaneous));
            if (rpos) {
              Scheme_Object *old_e;

              e = SCHEME_VEC_ELS(e)[1];

              old_e = scheme_hash_get(info->top_level_consts, rpos);
              if (old_e && SCHEME_LAMBDAP(old_e) && OPT_PRE_OPTIMIZE_FOR_CROSS_MODULE(1)) {
                if (!originals)
                  originals = scheme_make_hash_table(SCHEME_hash_ptr);
                scheme_hash_set(originals, scheme_make_integer(start_simultaneous), old_e);
              }

              if (!ir_propagate_ok(e, info, 0, NULL)
                  && scheme_is_statically_proc(e, info, 0)) {
                /* If we previously installed a procedure for inlining,
                   don't replace that with a worse approximation. */
                if (SCHEME_LAMBDAP(old_e))
                  e = NULL;
                else
                  e = scheme_make_noninline_proc(e);
              }

              if (e) {
                if (OPT_DELAY_GROUP_PROPAGATE || OPT_LIMIT_FUNCTION_RESIZE)
                  new_sz = lambda_body_size(e, 0);
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

	  if (start_simultaneous == i_m)
	    break;
          start_simultaneous++;
	}

        flags = set_code_closure_flags(cl_first, 0, 0xFFFF, 0);
        (void)set_code_closure_flags(cl_first,
                                     (flags & (LAMBDA_SINGLE_RESULT | LAMBDA_PRESERVES_MARKS)),
                                     ~(LAMBDA_SINGLE_RESULT | LAMBDA_PRESERVES_MARKS | LAMBDA_RESULT_TENTATIVE),
                                     1);
      }

      cl_last = cl_first = NULL;
      consts = NULL;
      re_consts = NULL;
      start_simultaneous = i_m + 1;

      while (prop_later) {
        e = SCHEME_CAR(prop_later);
        scheme_hash_set(info->top_level_consts, SCHEME_CAR(e), SCHEME_CDR(e));
        prop_later = SCHEME_CDR(prop_later);
      }
    }

    if (next_pos_ready > -1) {
      fixed_table = set_as_fixed(fixed_table, info, next_pos_ready);
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
          if (!alt_e && originals && OPT_PRE_OPTIMIZE_FOR_CROSS_MODULE(size_override)) {
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
      if (scheme_omittable_expr(e, -1, -1, 0, info, NULL)) {
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
        if (!scheme_omittable_expr(e, -1, -1, 0, info, NULL)) {
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
  case scheme_ir_local_type:
    {
      Scheme_Object *val;

      info->size += 1;

      if (SCHEME_VAR(expr)->mutated) {
        info->vclock += 1;
        register_use(SCHEME_VAR(expr), info);
        return expr;
      }

      val = optimize_info_propagate_local(expr);
      if (val) {
        info->size -= 1;
        return scheme_optimize_expr(val, info, context);
      }

      val = collapse_local(expr, info, context);
      if (val)
        return val;

      if (!(context & OPT_CONTEXT_NO_SINGLE)) {
        val = SCHEME_VAR(expr)->optimize.known_val;
      
        if (val && SAME_TYPE(SCHEME_TYPE(val), scheme_once_used_type)) {
          Scheme_Once_Used *o = (Scheme_Once_Used *)val;

          MZ_ASSERT(!o->moved);
          MZ_ASSERT(!SCHEME_VAR(expr)->optimize_outside_binding);

          /* In case this variable was tentatively used before: */
          SCHEME_VAR(expr)->optimize_used = 0;
        
          if (((o->vclock == info->vclock)
               && ((o->aclock == info->aclock)
                   || !o->spans_k)
               && ((context & OPT_CONTEXT_SINGLED)
                   || single_valued_noncm_expression(o->expr, info, 5)))
              || movable_expression(o->expr, info,
                                    o->var->optimize.lambda_depth != info->lambda_depth,
                                    o->kclock != info->kclock,
                                    o->sclock != info->sclock,
                                    0, 5)) {
            int save_fuel = info->inline_fuel, save_no_types = info->no_types;
            int save_vclock, save_aclock, save_kclock, save_sclock;
            info->size -= 1;
            info->inline_fuel = 0; /* no more inlining; o->expr was already optimized */
            info->no_types = 1; /* cannot used inferred types, in case `val' inferred them */
            save_vclock = info->vclock; /* allowed to move => no change to clocks */
            save_aclock = info->aclock;
            save_kclock = info->kclock;
            save_sclock = info->sclock;

            o->moved = 1;

            val = scheme_optimize_expr(o->expr, info, context);

            if (info->maybe_values_argument) {
              /* Although `val` could be counted as taking 0 time, we advance
                 the clock conservatively to be consistent with `values`
                 splitting. */
              advance_clocks_for_optimized(val,
                                           &save_vclock, &save_aclock, &save_kclock, &save_sclock,
                                           info,
                                           ADVANCE_CLOCKS_INIT_FUEL);
            }

            info->inline_fuel = save_fuel;
            info->no_types = save_no_types;
            info->vclock = save_vclock;
            info->aclock = save_aclock;
            info->kclock = save_kclock;
            info->sclock = save_sclock;
            return val;
          }
        }
      }

      /* If everything fails, mark it as used. */ 
      if (OPT_CONTEXT_TYPE(context))
        SCHEME_VAR(expr)->arg_type = OPT_CONTEXT_TYPE(context);
      if (info->kclock > SCHEME_VAR(expr)->optimize.init_kclock)
        SCHEME_VAR(expr)->escapes_after_k_tick = 1;
      register_use(SCHEME_VAR(expr), info);
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
    return optimize_sequence(expr, info, context, 1);
  case scheme_branch_type:
    return optimize_branch(expr, info, context);
  case scheme_with_cont_mark_type:
    return optimize_wcm(expr, info, context);
  case scheme_ir_lambda_type:
    if (context & OPT_CONTEXT_BOOLEAN)
      return scheme_true;
    else
      return optimize_lambda(expr, info, context);
  case scheme_ir_let_header_type:
    return optimize_lets(expr, info, context);
  case scheme_ir_toplevel_type:
    info->size += 1;
    if (info->top_level_consts) {
      int pos;
      Scheme_Object *c;

      while (1) {
        pos = SCHEME_TOPLEVEL_POS(expr);
        c = scheme_hash_get(info->top_level_consts, scheme_make_integer(pos));
        c = no_potential_size(c);
        if (c && SAME_TYPE(SCHEME_TYPE(c), scheme_ir_toplevel_type))
          expr = c;
        else
          break;
      }

      if (c) {
        if (context & OPT_CONTEXT_BOOLEAN)
          return (SCHEME_FALSEP(c) ? scheme_false : scheme_true);

	if (scheme_ir_duplicate_ok(c, 0))
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
  case scheme_ir_quote_syntax_type:
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
  case scheme_with_immed_mark_type:
    return with_immed_mark_optimize(expr, info, context);
  case scheme_require_form_type:
    return top_level_require_optimize(expr, info, context);
  case scheme_module_type:
    return module_optimize(expr, info, context);
  default:
    info->size += 1;
    if ((context & OPT_CONTEXT_BOOLEAN)
        && (SCHEME_TYPE(expr) > _scheme_ir_values_types_)
        && SCHEME_TRUEP(expr))
      return scheme_true;
    else
      return expr;
  }
}

static void increment_use_count(Scheme_IR_Local *var, int as_rator)
{
  if (var->use_count < SCHEME_USE_COUNT_INF)
    var->use_count++;
  if (!as_rator && (var->non_app_count < SCHEME_USE_COUNT_INF))
    var->non_app_count++;
  
  if (var->optimize.known_val
      && var->optimize.clear_known_on_multi_use)
    var->optimize.known_val = NULL;
}

Scheme_Object *optimize_clone(int single_use, Scheme_Object *expr, Optimize_Info *info, Scheme_Hash_Tree *var_map, int as_rator)
/* If single_use is 1, then the old copy will be dropped --- so it's ok to "duplicate"
   any constant, and local-variable use counts should not be incremented. */
{
  int t;

  t = SCHEME_TYPE(expr);

  switch(t) {
  case scheme_ir_local_type:
    {
      Scheme_Object *v;
      v = scheme_hash_tree_get(var_map, expr);
      if (v)
        return v;
      else if (!single_use)
        increment_use_count(SCHEME_VAR(expr), as_rator);
      return expr;
    }
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)expr, *app2;

      app2 = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
      app2->iso.so.type = scheme_application2_type;

      expr = optimize_clone(single_use, app->rator, info, var_map, 1);
      if (!expr) return NULL;
      app2->rator = expr;

      expr = optimize_clone(single_use, app->rand, info, var_map, 0);
      if (!expr) return NULL;
      app2->rand = expr;

      SCHEME_APPN_FLAGS(app2) |= (SCHEME_APPN_FLAGS(app) & APPN_FLAG_MASK);
      if (single_use)
        SCHEME_APPN_FLAGS(app2) |= (SCHEME_APPN_FLAGS(app) & APPN_POSITION_MASK);

      return (Scheme_Object *)app2;
    }
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)expr, *app2;
      int i;

      app2 = scheme_malloc_application(app->num_args + 1);

      for (i = app->num_args + 1; i--; ) {
	expr = optimize_clone(single_use, app->args[i], info, var_map, !i);
	if (!expr) return NULL;
	app2->args[i] = expr;
      }

      SCHEME_APPN_FLAGS(app2) |= (SCHEME_APPN_FLAGS(app) & APPN_FLAG_MASK);
      if (single_use)
        SCHEME_APPN_FLAGS(app2) |= (SCHEME_APPN_FLAGS(app) & APPN_POSITION_MASK);

      return (Scheme_Object *)app2;
    }
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)expr, *app2;

      app2 = MALLOC_ONE_TAGGED(Scheme_App3_Rec);
      app2->iso.so.type = scheme_application3_type;

      expr = optimize_clone(single_use, app->rator, info, var_map, 1);
      if (!expr) return NULL;
      app2->rator = expr;

      expr = optimize_clone(single_use, app->rand1, info, var_map, 0);
      if (!expr) return NULL;
      app2->rand1 = expr;

      expr = optimize_clone(single_use, app->rand2, info, var_map, 0);
      if (!expr) return NULL;
      app2->rand2 = expr;

      SCHEME_APPN_FLAGS(app2) |= (SCHEME_APPN_FLAGS(app) & APPN_FLAG_MASK);
      if (single_use)
        SCHEME_APPN_FLAGS(app2) |= (SCHEME_APPN_FLAGS(app) & APPN_POSITION_MASK);

      return (Scheme_Object *)app2;
    }
  case scheme_ir_let_header_type:
    {
      Scheme_IR_Let_Header *head = (Scheme_IR_Let_Header *)expr, *head2;
      Scheme_Object *body;
      Scheme_IR_Let_Value *lv, *lv2, *prev = NULL;
      Scheme_IR_Local **vars;
      int i;

      head2 = MALLOC_ONE_TAGGED(Scheme_IR_Let_Header);
      head2->iso.so.type = scheme_ir_let_header_type;
      head2->count = head->count;
      head2->num_clauses = head->num_clauses;
      SCHEME_LET_FLAGS(head2) = SCHEME_LET_FLAGS(head);

      /* Build let-value change: */
      body = head->body;
      for (i = head->num_clauses; i--; ) {
	lv = (Scheme_IR_Let_Value *)body;

        vars = clone_variable_array(lv->vars, lv->count, &var_map);

	lv2 = MALLOC_ONE_TAGGED(Scheme_IR_Let_Value);
        SCHEME_IRLV_FLAGS(lv2) |= (SCHEME_IRLV_FLAGS(lv) & 0x1);
	lv2->iso.so.type = scheme_ir_let_value_type;
	lv2->count = lv->count;
	lv2->vars = vars;
        lv2->value = lv->value;

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

      body = head2->body;
      for (i = head->num_clauses; i--; ) {
	lv2 = (Scheme_IR_Let_Value *)body;

        expr = optimize_clone(single_use, lv2->value, info, var_map, 0);
	if (!expr) return NULL;
	lv2->value = expr;

	body = lv2->body;
      }

      expr = optimize_clone(single_use, body, info, var_map, 0);
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
	expr = optimize_clone(single_use, seq->array[i], info, var_map, 0);
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

      expr = optimize_clone(single_use, b->test, info, var_map, 0);
      if (!expr) return NULL;
      b2->test = expr;

      expr = optimize_clone(single_use, b->tbranch, info, var_map, 0);
      if (!expr) return NULL;
      b2->tbranch = expr;

      expr = optimize_clone(single_use, b->fbranch, info, var_map, 0);
      if (!expr) return NULL;
      b2->fbranch = expr;

      return (Scheme_Object *)b2;
    }
  case scheme_with_cont_mark_type:
    {
      Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)expr, *wcm2;

      wcm2 = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
      wcm2->so.type = scheme_with_cont_mark_type;

      expr = optimize_clone(single_use, wcm->key, info, var_map, 0);
      if (!expr) return NULL;
      wcm2->key = expr;

      expr = optimize_clone(single_use, wcm->val, info, var_map, 0);
      if (!expr) return NULL;
      wcm2->val = expr;

      expr = optimize_clone(single_use, wcm->body, info, var_map, 0);
      if (!expr) return NULL;
      wcm2->body = expr;

      return (Scheme_Object *)wcm2;
    }
  case scheme_ir_lambda_type:
    return clone_lambda(single_use, expr, info, var_map);
  case scheme_ir_toplevel_type:
  case scheme_ir_quote_syntax_type:
    return expr;
  case scheme_define_values_type:
  case scheme_define_syntaxes_type:
  case scheme_begin_for_syntax_type:
  case scheme_boxenv_type:
    return NULL;
  case scheme_require_form_type:
    return NULL;
  case scheme_varref_form_type:
    return ref_clone(single_use, expr, info, var_map);
  case scheme_set_bang_type:
    return set_clone(single_use, expr, info, var_map);
  case scheme_apply_values_type:
    return apply_values_clone(single_use, expr, info, var_map);
  case scheme_with_immed_mark_type:
    return with_immed_mark_clone(single_use, expr, info, var_map);
  case scheme_case_lambda_sequence_type:
    return case_lambda_clone(single_use, expr, info, var_map);
  case scheme_module_type:
    return NULL;
  default:
    if (t > _scheme_ir_values_types_) {
      if (single_use || scheme_ir_duplicate_ok(expr, 0))
	return expr;
    }
  }

  return NULL;
}

/*========================================================================*/
/*                 compile-time env for optimization                      */
/*========================================================================*/

Optimize_Info *scheme_optimize_info_create(Comp_Prefix *cp, Scheme_Env *env, Scheme_Object *insp, int get_logger)
{
  Optimize_Info *info;

  info = MALLOC_ONE_RT(Optimize_Info);
#ifdef MZTAG_REQUIRED
  info->type = scheme_rt_optimize_info;
#endif
  info->inline_fuel = INITIAL_INLINING_FUEL;
  info->flatten_fuel = INITIAL_FLATTENING_FUEL;
  info->cp = cp;
  info->env = env;
  info->insp = insp;

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
  info_seq->init_flatten_fuel = info->flatten_fuel;
  info_seq->min_flatten_fuel = info->flatten_fuel;
}

static void optimize_info_seq_step(Optimize_Info *info, Optimize_Info_Sequence *info_seq)
{
  if (info->flatten_fuel < info_seq->min_flatten_fuel)
    info_seq->min_flatten_fuel = info->flatten_fuel;
  info->flatten_fuel = info_seq->init_flatten_fuel;
}

static void optimize_info_seq_done(Optimize_Info *info, Optimize_Info_Sequence *info_seq)
{
  if (info->flatten_fuel > info_seq->min_flatten_fuel)
    info->flatten_fuel = info_seq->min_flatten_fuel;
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

static void propagate_used_variables(Optimize_Info *info)
{
  Scheme_Hash_Table *ht;
  Scheme_IR_Local *tvar;
  int j;

  if (info->next->uses) {
    ht = info->uses;
    for (j = 0; j < ht->size; j++) {
      if (ht->vals[j]) {
        tvar = SCHEME_VAR(ht->keys[j]);
        if (tvar->optimize.lambda_depth < info->next->lambda_depth)
          scheme_hash_set(info->next->uses, (Scheme_Object *)tvar, scheme_true);
      }
    }
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

static Scheme_Once_Used *make_once_used(Scheme_Object *val, Scheme_IR_Local *var,
                                        int vclock, int aclock, int kclock, int sclock, int spans_k)
{
  Scheme_Once_Used *o;

  /* Procedures should be handled more specifically, because there are
     issues with transitive delayed-use registration to handle
     `letrec`, where a value that has already been moved can be
     marked later as used. */
  MZ_ASSERT(!SCHEME_LAMBDAP(val));

  o = MALLOC_ONE_TAGGED(Scheme_Once_Used);
  o->so.type = scheme_once_used_type;

  o->expr = val;
  o->var = var;
  o->vclock = vclock;
  o->aclock = aclock;
  o->kclock = kclock;
  o->sclock = sclock;
  o->spans_k = spans_k;

  return o;
}

static int optimize_any_uses(Optimize_Info *info, Scheme_IR_Let_Value *at_irlv, int n)
{
  int i, j;
  Scheme_IR_Let_Value *irlv = at_irlv;

  while (n--) {
    for (i = irlv->count; i--; ) {
      if (irlv->vars[i]->optimize_used)
        return 1;
      for (j = at_irlv->count; j--; ) {
        if (at_irlv->vars[j]->optimize.transitive_uses) {
          if (scheme_hash_get(at_irlv->vars[j]->optimize.transitive_uses,
                              (Scheme_Object *)irlv->vars[i]))
            return 1;
        }
      }
    }
    irlv = (Scheme_IR_Let_Value *)irlv->body;
  }

  return 0;
}

static void optimize_uses_of_mutable_imply_early_alloc(Scheme_IR_Let_Value *at_irlv, int n)
{
  int i, j;
  Scheme_IR_Let_Value *irlv = at_irlv;

  /* We we're reinterpreting a `letrec` as `let*`, and when it realy
     must be `let*` instead of `let`, and when a mutable variable is
     involved, then we need to tell the `resolve` pass that the
     mutable varaiable's value must be boxed immediately, instead of
     delaying to the body of the `let*`. */

  while (n--) {
    for (i = irlv->count; i--; ) {
      if (irlv->vars[i]->mutated) {
        int used = 0;
        if (irlv->vars[i]->optimize_used)
          used = 1;
        else {
          for (j = at_irlv->count; j--; ) {
            if (at_irlv->vars[j]->optimize.transitive_uses) {
              if (scheme_hash_get(at_irlv->vars[j]->optimize.transitive_uses,
                                  (Scheme_Object *)irlv->vars[i]))
                used = 1;
            }
          }
        }
        if (used)
          irlv->vars[i]->must_allocate_immediately = 1;
      }
    }
    irlv = (Scheme_IR_Let_Value *)irlv->body;
  }
}

static void register_use(Scheme_IR_Local *var, Optimize_Info *info)
{
  MZ_ASSERT(SCHEME_VAR(var)->mode == SCHEME_VAR_MODE_OPTIMIZE);
  MZ_ASSERT(SCHEME_VAR(var)->use_count);

  if (var->optimize.lambda_depth < info->lambda_depth)
    scheme_hash_set(info->uses, (Scheme_Object *)var, scheme_true);

  if (!var->optimize_used) {
    var->optimize_used = 1;

    if (info->transitive_use_var
        && (var->optimize.lambda_depth
            <= info->transitive_use_var->optimize.lambda_depth)) {
      Scheme_Hash_Table *ht = info->transitive_use_var->optimize.transitive_uses;

      if (!ht) {
        ht = scheme_make_hash_table(SCHEME_hash_ptr);
        info->transitive_use_var->optimize.transitive_uses = ht;
      }
      scheme_hash_set(ht, (Scheme_Object *)var, scheme_true);
    }
  }
}

static void register_transitive_uses(Scheme_IR_Local *var, Optimize_Info *info)
{
  Scheme_Hash_Table *ht;
  Scheme_IR_Local *tvar;
  int j;

  ht = var->optimize.transitive_uses;

  for (j = 0; j < ht->size; j++) {
    if (ht->vals[j]) {
      tvar = SCHEME_VAR(ht->keys[j]);

      if (tvar->optimize.known_val
          && SAME_TYPE(SCHEME_TYPE(tvar->optimize.known_val), scheme_once_used_type)
          && ((Scheme_Once_Used *)tvar->optimize.known_val)->moved) {
        /* variable no longer used, and any transitive uses were
           covered by re-optimizing in its use context */
        MZ_ASSERT(!tvar->optimize_used);
      } else
        register_use(tvar, info);
    }
  }
}

static Scheme_Object *optimize_info_lookup(Scheme_Object *var)
{
  MZ_ASSERT(SCHEME_VAR(var)->mode == SCHEME_VAR_MODE_OPTIMIZE);
  MZ_ASSERT(SCHEME_VAR(var)->use_count);

  return SCHEME_VAR(var)->optimize.known_val;
}

static Scheme_Object *optimize_info_propagate_local(Scheme_Object *var)
{
  Scheme_Object *last, *val = var;

  last = val; /* Avoid compiler warning */

  while (val && SAME_TYPE(SCHEME_TYPE(val), scheme_ir_local_type)) {
    MZ_ASSERT(SCHEME_VAR(val)->mode == SCHEME_VAR_MODE_OPTIMIZE);
    MZ_ASSERT(SCHEME_VAR(val)->use_count);
    last = val;
    val = SCHEME_VAR(val)->optimize.known_val;
  }
  
  if (!val
      || SCHEME_WILL_BE_LAMBDAP(val)
      || SCHEME_LAMBDAP(val)
      || SAME_TYPE(SCHEME_TYPE(val), scheme_once_used_type)) {
    if (SAME_OBJ(last, var))
      return NULL;

    if (SCHEME_VAR(var)->use_count != 1)
      increment_use_count(SCHEME_VAR(last), 0);
  
    return last;
  }

  return val;
}

Scheme_Object *optimize_get_predicate(Optimize_Info *info, Scheme_Object *var, int ignore_no_types)
{
  Scheme_Object *pred;

  if (info->no_types && !ignore_no_types) return NULL;

  while (info) {
    if (info->types) {
      pred = scheme_hash_tree_get(info->types, var);
      if (pred)
        return pred;
    }
    info = info->next;
  }

  return NULL;
}

static Optimize_Info *optimize_info_add_frame(Optimize_Info *info, int orig, int current, int flags)
{
  Optimize_Info *naya;

  naya = scheme_optimize_info_create(info->cp, info->env, info->insp, 0);
  naya->flags = (short)flags;
  naya->next = info;
  naya->original_frame = orig;
  naya->new_frame = current;
  naya->inline_fuel = info->inline_fuel;
  naya->flatten_fuel = info->flatten_fuel;
  naya->letrec_not_twice = info->letrec_not_twice;
  naya->enforce_const = info->enforce_const;
  naya->top_level_consts = info->top_level_consts;
  naya->context = info->context;
  naya->vclock = info->vclock;
  naya->aclock = info->aclock;
  naya->kclock = info->kclock;
  naya->sclock = info->sclock;
  naya->escapes = info->escapes;
  naya->init_kclock = info->kclock;
  naya->maybe_values_argument = info->maybe_values_argument;
  naya->use_psize = info->use_psize;
  naya->logger = info->logger;
  naya->no_types = info->no_types;
  naya->lambda_depth = info->lambda_depth + ((flags & SCHEME_LAMBDA_FRAME) ? 1 : 0);
  naya->uses = info->uses;
  naya->transitive_use_var = info->transitive_use_var;

  return naya;
}

static void optimize_info_done(Optimize_Info *info, Optimize_Info *parent)
{
  if (!parent) parent = info->next;

  parent->size += info->size;
  parent->vclock = info->vclock;
  parent->aclock = info->aclock;
  parent->kclock = info->kclock;
  parent->sclock = info->sclock;
  parent->escapes = info->escapes;
  parent->psize += info->psize;
  parent->flatten_fuel = info->flatten_fuel;
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
