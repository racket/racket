/*
  Racket
  Copyright (c) 2004-2016 PLT Design Inc.

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

/* This file implements the compiler's letrec-check pass.
 *
 * See "eval.c" for an overview of compilation passes.
 * 
 * PLAN:
 *
 * Imagine starting with a simple abstract interpretation: traverse
 * the program in evaluation order, treating `if` like `begin` and
 * `lambda` like `let`. Instantiate each value in the original
 * expression exacty once. (Grep "EXPL-5" for implementation.) The
 * astract value of each variable is just whether it has a value. A
 * variable that is not bound by `letrec` always has the "ready"
 * value. A letrec-bound variable is mutable, and its abstract value
 * becomes "ready" just after its right-hand side is evaluated. On
 * encountering a reference to a variable whose abstract value is not
 * "ready", wrap the reference in a check for #<unsafe-undefined>.
 *
 * That simple interpretation will add checks to too many variables,
 * because some uses of a `letrec`-bound variable are under `lambda`
 * and not reached until after the variable acquires a value.
 *
 * To improve the abstraction:
 *
 * - Change the abstract value to pair "not ready" or "ready" with
 *   a list of functions that have not yet been applied but are
 *   reachable from the variable (i.e., potentially reachable from
 *   the variable's concrete value). The list of functions are the
 *   variable's "deferred" expressions.
 *
 * - Keep track of whether an expression's result is on the right-hand
 *   side of a particular variable such that it is not applied before
 *   the variable gets its value. (Grep "EXPL-3" for representation
 *   details.) For example, an expression immediately on the
 *   right-hand side of a variable is in such a position, and so are
 *   the arguments to `list` in such a position. Call those "safe"
 *   positions.
 *
 * - When `lambda` is in such a position, record the `lambda`
 *   expression as "deferred" for the variable --- but only if the
 *   varible was not previously accessed in an unsafe position. (Grep
 *   "EXPL-4" for implementation.) Each deferred expression will be
 *   forced at most once.
 *
 * - When referencing a variable in a non-safe position, force all of
 *   its deferred expressions. Note that the forced expressions get
 *   #<unsafe-undefined> checks according to the current state of any
 *   referenced variable.
 *
 * - When referencing a variable in a safe position, attach its
 *   deferred expressions to the variable relative to which the access
 *   is safe. If that variable was previously accessed in an unsafe
 *   position, immediately force the added deferred expressions.
 *   (Grep "EXPL-2" for implementation.)
 *
 * Since variables progress only from not-"ready" to "ready", forcing
 * a deferred expression on first use is consistent with all later
 * uses.
 *
 *
 * After traversing a `letrec` form, go back and remove the
 * SCHEME_WAS_ONLY_APPLIED and SCHEME_WAS_APPLIED_EXCEPT_ONCE flags
 * from variables who had undefined checks added around them.  (The
 * LET_CHECKED flag for each variable keeps track of those checks.)
 *
 *
 * It's possible that we get to the end of checking with deferred
 * expressions that were never forced. In that case, the analysis has
 * detected dead code, and we just drop the unused body expression ---
 * in case a later optimization pass would somehow be confused by a
 * lack of checking for "undefined". (Grep "EXPL-6" for the
 * implementation.)
 */

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

void scheme_init_letrec_check()
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif
}

#define LET_RHS_EXPR   1
#define LET_BODY_EXPR  2

#define LET_APPLY_USE  0x1
#define LET_READY     (0x1 << 1)
#define LET_CHECKED   (0x1 << 2)

#define FRAME_TYPE_LETREC   1
#define FRAME_TYPE_LET      3
#define FRAME_TYPE_CLOSURE  4
#define FRAME_TYPE_TOP      5

typedef Scheme_Object Wrapped_Lhs;

/* struct where all mutable information is stored during this pass! */
typedef struct Letrec_Check_Frame {
  MZTAG_IF_REQUIRED

  /* whether this is a frame for a letrec, let*, let, or closure */
  int frame_type;

  /* which sub-expression of a letrec we are in, so that we know
     whether `count` binding count toward the current context (i.e.,
     they do for `letrec` and `let*`, but not `let`) */
  int subexpr;

  /* total number of bindings in this frame */
  int count;

  /* table of lists of deferred sub expressions for each bound
     variable */
  Scheme_Object **def;

  /* track a variable's state (ready or not), whether it has been used
     in an applied (or "unsafe") position, and whether it has ever
     been wrapped with an #<unsafe-undefined> check */
  int *ref;

  /* we keep a list of all deferred expressions, only so that we can
     drop the body for any that are not processed (which means that
     they won't be used) */
  struct Scheme_Deferred_Expr **deferred_chain;

  struct Letrec_Check_Frame *next;
} Letrec_Check_Frame;

/* a deferred expression, these are inserted and completely removed by
   the letrec_check pass */
typedef struct Scheme_Deferred_Expr {
  Scheme_Object so;

  /* the same deferral can be attached to multiple variables, but we
     only need to process it once */
  int done;

  /* the expression that has been deferred */
  Scheme_Lambda *expr;

  /* the frame that existed when the expr was deferred */
  Letrec_Check_Frame *frame;

  /* for the global chain of all deferrals, which is used only for
     dead-code elimination: */
  struct Scheme_Deferred_Expr *chain_next;
} Scheme_Deferred_Expr;

static void process_deferred_bindings(Letrec_Check_Frame *frame, int position);

/* initializes a Letrec_Check_Frame */
static Letrec_Check_Frame *init_letrec_check_frame(int frame_type, int subexpr,
                                                   mzshort count,
                                                   Letrec_Check_Frame *prev,
                                                   Letrec_Check_Frame *share_with,
                                                   Scheme_IR_Let_Header *head,
                                                   Scheme_Lambda *lam)
{
  Scheme_Deferred_Expr **chain;
  Letrec_Check_Frame *frame;
  Scheme_Object **def;
  int *ref, i, j, pos;

  frame = (Letrec_Check_Frame *)MALLOC_ONE_RT(Letrec_Check_Frame);
#ifdef MZTAG_REQUIRED
  frame->type = scheme_rt_letrec_check_frame;
#endif
    
  frame->frame_type = frame_type;
    
  frame->count = count;
  frame->next = prev;

  if (head) {
    Scheme_IR_Let_Value *irlv = (Scheme_IR_Let_Value *)head->body;
    pos = 0;
    for (i = head->num_clauses; i--; irlv = (Scheme_IR_Let_Value *)irlv->body) {
      for (j = 0; j < irlv->count; j++) {
        irlv->vars[j]->mode = SCHEME_VAR_MODE_LETREC_CHECK;
        irlv->vars[j]->letrec_check.frame = frame;
        irlv->vars[j]->letrec_check.frame_pos = pos++;
      }
    }
  } else if (lam) {
    for (j = lam->num_params; j--; ) {
      lam->ir_info->vars[j]->mode = SCHEME_VAR_MODE_LETREC_CHECK;
      lam->ir_info->vars[j]->letrec_check.frame = frame;
      lam->ir_info->vars[j]->letrec_check.frame_pos = j;
    }
  }

  if (share_with) {
    /* Moving from RHS phase to BODY phase for `let[rec]`,
       need to share arrays that represent the dynamic state
       of variables: */
    frame->def = share_with->def;
    frame->ref = share_with->ref;
  } else if ((frame_type == FRAME_TYPE_CLOSURE)
             || (frame_type == FRAME_TYPE_TOP)) {
    frame->def = NULL;
    frame->ref = NULL;
  } else {
    int init_ref;

    /* def will be a table of lists so every entry should be
       initialized to scheme_null */
    def = MALLOC_N(Scheme_Object *, count);
    for (i = 0; i < count; i++) { def[i] = scheme_null; }
    frame->def = def;
    
    /* ref is a table of flags, 0 for unreferenced, 1-3 for referenced
       in the body and/or the RHS */
    ref = MALLOC_N_ATOMIC(int, count);
    if (frame_type == FRAME_TYPE_LETREC)
      init_ref = 0;
    else
      init_ref = LET_READY;
    for (i = count; i--;) { ref[i] = init_ref; }
    frame->ref = ref;
  }

  /* the sub-expression of the let[rec] (if we're in a let[rec]),
     i.e. the RHS or the body. This is for tracking where LHS
     variables are referenced */
  frame->subexpr = subexpr;

  if (prev)
    chain = prev->deferred_chain;
  else
    chain = MALLOC_N(Scheme_Deferred_Expr*, 1);
  frame->deferred_chain = chain;

  return frame;
}

/* returns the frame that is the nearest enclosing let in the
   LET_RHS_EXPR */
static Letrec_Check_Frame *get_nearest_rhs(Letrec_Check_Frame *frame)
{
  for (; frame != NULL; frame = frame->next) {
    if (frame->subexpr == LET_RHS_EXPR)
      return frame;
  }

  scheme_signal_error("get_nearest_rhs: not in a let RHS");
  ESCAPED_BEFORE_HERE;
}

/* returns the frame that was created when pos was created, and
   changes pos to be relative to that frame */
static Letrec_Check_Frame *get_relative_frame(int *pos, Scheme_IR_Local *var)
{
  *pos = var->letrec_check.frame_pos;
  return var->letrec_check.frame;
}

/* adds expr to the deferred bindings of lhs */
static void update_frame(Letrec_Check_Frame *outer, Letrec_Check_Frame *inner,
                         int position, Scheme_Deferred_Expr *clos)
{
  Scheme_Object *prev_def;

  SCHEME_ASSERT(position < outer->count, "update_frame: position exceeds binding count");

  /* put the deferred expression in the right place */
  prev_def = outer->def[position];
  prev_def = scheme_make_pair((Scheme_Object *)clos, prev_def); 
  outer->def[position] = prev_def; 

  if (outer->ref[position] & LET_APPLY_USE)
    process_deferred_bindings(outer, position);
}

/* creates a deferred expression "closure" by closing over the frame */
static Scheme_Deferred_Expr *make_deferred_expr_closure(Scheme_Lambda *expr, Letrec_Check_Frame *frame)
{
  Scheme_Deferred_Expr *clos;

  clos = MALLOC_ONE_RT(Scheme_Deferred_Expr);
  clos->so.type = scheme_deferred_expr_type;
  clos->done = 0;
  clos->expr = expr;
  clos->frame = frame;

  clos->chain_next = *frame->deferred_chain;
  *frame->deferred_chain = clos;

  return clos;
}

static Scheme_Object *letrec_check_expr(Scheme_Object *, Letrec_Check_Frame *, Scheme_Object *);

static void letrec_check_lets_resume(Letrec_Check_Frame *frame, Scheme_IR_Let_Header *head)
{
  Scheme_IR_Let_Value *irlv;
  Scheme_Object *body;
  int i, j, k;
  int was_checked;

  body = head->body;
  if (frame->frame_type == FRAME_TYPE_LETREC) {
    /* loops through every right hand side again to update the flags
       that we have invalidated; i.e., adding check-undefineds around
       references means there is one (more) instance where the LHS
       variable is not used in application position */
    k = head->count;
    for (i = head->num_clauses; i--;) {
      irlv = (Scheme_IR_Let_Value *) body;
      k -= irlv->count;
      for (j = 0; j < irlv->count; j++) {
        was_checked = (frame->ref[k + j] & LET_CHECKED);
        if (was_checked)
          irlv->vars[j]->non_app_count = irlv->vars[j]->use_count;
      }
      body = irlv->body;
    }
  }
}

/* records that we have seen a reference to loc */
static Scheme_Object *record_checked(Scheme_IR_Local *loc, Letrec_Check_Frame *frame)
{
  int position;
  
  frame = get_relative_frame(&position, loc);
  frame->ref[position] |= LET_CHECKED;

  return loc->name;
}

static Scheme_Object *letrec_check_local(Scheme_Object *o, Letrec_Check_Frame *frame,
                                         Scheme_Object *pos)
{
  Letrec_Check_Frame *in_frame;
  Scheme_IR_Local *loc = (Scheme_IR_Local *)o;
  int in_position;

  in_frame = get_relative_frame(&in_position, loc);

  if (SCHEME_FALSEP(pos)) {
    /* mark as potentially applied (i.e., in an "unsafe" context)
       for deferred closures (gre "EXPL-4" for information): */
    if (in_frame->ref)
      in_frame->ref[in_position] |= LET_APPLY_USE;
  } else {
    /* propagate any deferred expressions (grep "EXPL-2" for information): */
    if (in_frame->def
        && !SCHEME_NULLP(in_frame->def[in_position])
        && !SCHEME_NULLP(pos)) {
      Letrec_Check_Frame *outer_frame;
      Scheme_Object *ls;
      outer_frame = get_nearest_rhs(frame);
      while (SCHEME_INTP(pos) || SCHEME_PAIRP(pos)) {
        int dpos;
        
        if (SCHEME_INTP(pos)) {
          dpos = SCHEME_INT_VAL(pos);
          pos = scheme_null;
        } else {
          dpos = SCHEME_INT_VAL(SCHEME_CAR(pos));
          pos = SCHEME_CDR(pos);
        }

        ls = scheme_append(in_frame->def[in_position],
                           outer_frame->def[dpos]);
        outer_frame->def[dpos] = ls;
      }
    }
  }

  /* If we've just set LET_APPLY_USE, or if we've just added deferred
     expressions and LET_APPLY_USE was set before, then we need to
     force any deferred expressions: */
  if (in_frame->ref
      && (in_frame->ref[in_position] & LET_APPLY_USE))
    process_deferred_bindings(in_frame, in_position);

  if (in_frame->ref
      && !(in_frame->ref[in_position] & LET_READY)) {
    /* our reference is not ready, so we need to insert an
       #<unsafe-undefined> check around it */
    Scheme_App3_Rec *app3;
    Scheme_Object *name;
    
    name = record_checked(loc, frame);
    
    app3 = MALLOC_ONE_TAGGED(Scheme_App3_Rec);
    app3->iso.so.type = scheme_application3_type;
    app3->rator = scheme_check_not_undefined_proc;
    app3->rand1 = o;
    app3->rand2 = name;
    
    return (Scheme_Object *) app3;
  }

  /* our reference is protected, so we're fine to access directly */
  return o;
}

static int is_effect_free_prim(Scheme_Object *rator)
{
  if (SCHEME_PRIMP(rator)
      && (SCHEME_PRIM_PROC_OPT_FLAGS(rator) & SCHEME_PRIM_IS_OMITABLE_ANY))
    return 1;

  return 0;
}

static Scheme_Object *letrec_check_application(Scheme_Object *o, Letrec_Check_Frame *frame, Scheme_Object *pos)
{
  int i,n;
  Scheme_App_Rec *app;
  Scheme_Object *val;

  app = (Scheme_App_Rec *)o;

  /* we'll have to check the rator and all the arguments */
  n = 1 + app->num_args;

  if (is_effect_free_prim(app->args[0])) {
    /* an immediate prim cannot call anything among its arguments */
  } else {
    /* argument might get applied */
    pos = scheme_false;
  }

  for (i = 0; i < n; i++) { 
    val = letrec_check_expr(app->args[i], frame, pos);
    app->args[i] = val;
  }

  return o;
}

static Scheme_Object *letrec_check_application2(Scheme_Object *o, Letrec_Check_Frame *frame, Scheme_Object *pos)
{
  Scheme_App2_Rec *app;
  Scheme_Object *val;

  app = (Scheme_App2_Rec *)o;
    
  if (is_effect_free_prim(app->rator)) {
    /* an immediate prim cannot call anything among its arguments */
  } else {
    /* argument might get applied */
    pos = scheme_false;
  }

  val = letrec_check_expr(app->rator, frame, pos);
  app->rator = val;
  val = letrec_check_expr(app->rand,  frame, pos);
  app->rand = val;

  return o;
}

static Scheme_Object *letrec_check_application3(Scheme_Object *o, Letrec_Check_Frame *frame, Scheme_Object *pos)
{
  Scheme_App3_Rec *app;
  Scheme_Object *val;

  app = (Scheme_App3_Rec *)o;

  if (is_effect_free_prim(app->rator)) {
    /* an immediate prim cannot call anything among its arguments */
  } else {
    /* argument might get applied */
    pos = scheme_false;
  }

  val = letrec_check_expr(app->rator, frame, pos);
  app->rator = val;
  val = letrec_check_expr(app->rand1, frame, pos);
  app->rand1 = val;
  val = letrec_check_expr(app->rand2, frame, pos);
  app->rand2 = val;

  return o;
}

static Scheme_Object *letrec_check_sequence(Scheme_Object *o, Letrec_Check_Frame *frame, Scheme_Object *pos)
{
  Scheme_Sequence *seq;
  Scheme_Object *val;
  int i,n;

  seq = (Scheme_Sequence *)o;

  n = seq->count;
  for (i = 0; i < n; i++) {
    val = letrec_check_expr(seq->array[i], frame, pos);
    seq->array[i] = val;
  }

  return o;
}

static Scheme_Object *letrec_check_branch(Scheme_Object *o, Letrec_Check_Frame *frame, Scheme_Object *pos)
{
  Scheme_Branch_Rec *br;
  Scheme_Object *val;

  br = (Scheme_Branch_Rec *)o;

  val = letrec_check_expr(br->test, frame, pos);
  br->test = val;
  val = letrec_check_expr(br->tbranch, frame, pos);
  br->tbranch = val;
  val = letrec_check_expr(br->fbranch, frame, pos);
  br->fbranch = val;
    
  return o;
}

static Scheme_Object *letrec_check_wcm(Scheme_Object *o, Letrec_Check_Frame *frame, Scheme_Object *pos)
{
  Scheme_With_Continuation_Mark *wcm;
  Scheme_Object *val, *val_pos;

  wcm = (Scheme_With_Continuation_Mark *)o;
    
  val = letrec_check_expr(wcm->key, frame, pos);
  wcm->key = val;

  /* Since a value can be accessed through `current-continuation-marks`... */
  val_pos = scheme_false;
  val = letrec_check_expr(wcm->val, frame, val_pos);
  wcm->val = val;

  val = letrec_check_expr(wcm->body, frame, pos);
  wcm->body = val;
    
  return o;
}

static Scheme_Object *letrec_check_lambda(Scheme_Object *o, Letrec_Check_Frame *frame, Scheme_Object *pos)
{
  Scheme_Lambda *lam;
  Letrec_Check_Frame *new_frame;
  Scheme_Object *val;
  int num_params;

  lam = (Scheme_Lambda *)o;

  /* if we have not entered a letrec, pos will be false */
  if (SCHEME_FALSEP(pos)) {
    num_params = lam->num_params;
    new_frame = init_letrec_check_frame(FRAME_TYPE_CLOSURE, LET_BODY_EXPR,
                                        num_params, frame, NULL,
                                        NULL, lam);

    SCHEME_ASSERT(num_params >= 0, "lambda has negative arguments what do");
        
    val = letrec_check_expr(lam->body, new_frame, pos);
    lam->body = val;
  } else {
    /* we can defer this lambda because it is not inside an
       application! hurray! */
    Scheme_Deferred_Expr *clos;
    Letrec_Check_Frame *outer_frame = NULL;

    if (!SCHEME_NULLP(pos)) {
      /* pos is either a single integer or a list of integers */

      /* create a deferred expression that closes over the frame it
         appeared in, and update the frame where the binding lives
         (which may be an enclosing frame) */
      outer_frame = get_nearest_rhs(frame);
      clos = make_deferred_expr_closure(lam, frame);

      while (SCHEME_INTP(pos) || SCHEME_PAIRP(pos)) {
        int position;

        if (SCHEME_INTP(pos)) {
          position = SCHEME_INT_VAL(pos);
          pos = scheme_null;
        } else {
          position = SCHEME_INT_VAL(SCHEME_CAR(pos));
          pos = SCHEME_CDR(pos);
        }

        /* attach the deferred_expr_closure to the right position 
           in the correct frame */
        update_frame(outer_frame, frame, position, clos);
      }
    }
  }

  return o;
}

static void letrec_check_deferred_expr(Scheme_Object *o)
{
  Scheme_Deferred_Expr *clos;
  Scheme_Lambda *lam;
  Letrec_Check_Frame *inner, *new_frame;
  Scheme_Object *val;
  int num_params;

  /* gets the closed over lam from clos, which will always be a
     deferred expression that contains a closure */
  clos = (Scheme_Deferred_Expr *)o;

  if (clos->done)
    return;
  clos->done = 1;

  SCHEME_ASSERT(SAME_TYPE(SCHEME_TYPE(clos), scheme_deferred_expr_type),
                "letrec_check_deferred_expr: clos is not a scheme_deferred_expr");

  lam = (Scheme_Lambda *)clos->expr;
  SCHEME_ASSERT(SAME_TYPE(SCHEME_TYPE(lam), scheme_ir_lambda_type),
                "deferred expression does not contain a lambda");

  inner = clos->frame;

  num_params = lam->num_params;

  new_frame = init_letrec_check_frame(FRAME_TYPE_CLOSURE, LET_BODY_EXPR,
                                      num_params, inner, NULL, 
                                      NULL, lam);

  val = letrec_check_expr(lam->body, new_frame, scheme_false);
  lam->body = val;
}

static void clean_dead_deferred_expr(Scheme_Deferred_Expr *clos)
{
  Scheme_Lambda *lam;

  /* We keep a global chain of all deferred expression. A deferred
     expression that is never forced is a function that is never
     called, so its body is dead code. (Grep "EXPL-6" for
     information.) */

  while (clos) {
    SCHEME_ASSERT(SAME_TYPE(SCHEME_TYPE(clos), scheme_deferred_expr_type),
                  "letrec_check_deferred_expr: clos is not a scheme_deferred_expr");

    if (!clos->done) {
      lam = (Scheme_Lambda *)clos->expr;
      SCHEME_ASSERT(SAME_TYPE(SCHEME_TYPE(lam), scheme_ir_lambda_type),
                    "deferred expression does not contain a lambda");
      
      /* Since this deferral was never done, it's dead code. */
      lam->body = scheme_void;

      clos->done = 1;
    }

    clos = clos->chain_next;
  }
}


static void process_deferred_bindings(Letrec_Check_Frame *frame, int position)
{
  Scheme_Object *ls;

  if (frame->def && !SCHEME_NULLP(frame->def[position])) {
    ls = frame->def[position];
    frame->def[position] = scheme_null;
    while (!SCHEME_NULLP(ls)) {
      letrec_check_deferred_expr(SCHEME_CAR(ls));
      ls = SCHEME_CDR(ls);
    }
  }
}

static Scheme_Object *letrec_check_lets(Scheme_Object *o, Letrec_Check_Frame *old_frame, Scheme_Object *pos)
{
  Letrec_Check_Frame *frame, *body_frame;
  Scheme_IR_Let_Value *irlv;
  Scheme_Object *body, *val;
  int i, j, k;

  /* gets the information out of our header about the number of
     total clauses, the number of total bindings, and whether or not
     this let is recursive */
  Scheme_IR_Let_Header *head = (Scheme_IR_Let_Header *)o;

  /* number of clauses in the let */
  int num_clauses = head->num_clauses;

  /* number of total bindings (not necessarily the same as the
     number of bindings thanks to let(rec)-values) */
  int count = head->count;

  /* information about this let */
  int header_flags = SCHEME_LET_FLAGS(head);

  /* what kind of let this is: letrec, let*, or let */
  int frame_type;

  if (header_flags & SCHEME_LET_RECURSIVE)
    frame_type = FRAME_TYPE_LETREC; 
  else
    frame_type = FRAME_TYPE_LET; 

  /* push the new bindings on to the frame, where `frame_type`
     determines how the variables are initialzed and counted when
     resolving local-variable offsets */
  frame = init_letrec_check_frame(frame_type, LET_RHS_EXPR,
                                  count, old_frame, NULL,
                                  head, NULL);

  body = head->body;

  k = 0;

  /* loops through every right hand side */
  irlv = NULL;
  for (i = num_clauses; i--;) {
    irlv = (Scheme_IR_Let_Value *)body;

    if (irlv->count == 0) {
      val = letrec_check_expr(irlv->value, frame,
                              /* deferred closures get attached to no variables,
                                 which is sensible because the closure will not
                                 be reachable: */
                              scheme_null);
    } else {
      Scheme_Object *new_pos;

      if (irlv->count == 1) {
        /* any deferred closure on the right-hand side gets attached to the
           variable on the left-hand side: */
        new_pos = scheme_make_integer(k);
      } else {
        /* attach any deferred closures on the right-hand side to all
           variables on the left-hand side; we could do better by
           recognizing an immediate `values` to avoid conflating all
           variables in that case */
        int sub;
        new_pos = scheme_null;
        for (sub = irlv->count; sub--; ) {
          new_pos = scheme_make_pair(scheme_make_integer(k+sub), new_pos);
        }
      }

      val = letrec_check_expr(irlv->value, frame, new_pos);
    }

    if (frame_type == FRAME_TYPE_LETREC) {
      for (j = 0; j < irlv->count; j++) {
        frame->ref[j + k] |= LET_READY;
      }
    }

    k += irlv->count;
        
    irlv->value = val;

    body = irlv->body;
  }

  /* the body variant of the frame shares the `ref`, etc., arrays with
     `frame`, so that there's a single array cell instantiated for
     each variable during the entire analysis (see "EXPL-5" for
     information) */
  body_frame = init_letrec_check_frame(frame_type, LET_BODY_EXPR,
                                       count, old_frame, frame,
                                       head, NULL);
  
  val = letrec_check_expr(body, body_frame, pos);

  /* put the new body in the right place: after the last RHS if the
     let had bindings, otherwise, the let header should point to the
     new body */
  if (num_clauses > 0)
    irlv->body = val;
  else
    head->body = val;

  letrec_check_lets_resume(frame, head);

  return o;
}

/* note to future self: the length of define_values is sometimes 1,
   and you definitely don't want to look inside if that's the case */
static Scheme_Object *letrec_check_define_values(Scheme_Object *lam, Letrec_Check_Frame *frame, Scheme_Object *pos)
{
  if (SCHEME_VEC_SIZE(lam) <= 1)
    return lam;
  else {
    Scheme_Object *vars = SCHEME_VEC_ELS(lam)[0];
    Scheme_Object *val = SCHEME_VEC_ELS(lam)[1];
    SCHEME_ASSERT(SCHEME_PAIRP(vars) || SCHEME_NULLP(vars),
                  "letrec_check_define_values: processing resolved code");

    val = letrec_check_expr(val, frame, pos);

    SCHEME_VEC_ELS(lam)[1] = val;
  }
    
  return lam;
}

static Scheme_Object *letrec_check_ref(Scheme_Object *lam, Letrec_Check_Frame *frame, Wrapped_Lhs *lhs)
{
  return lam;
}

static Scheme_Object *letrec_check_set(Scheme_Object *o, Letrec_Check_Frame *frame, Scheme_Object *pos)
{
  Scheme_Set_Bang *sb;
  Scheme_Object *val, *rhs_pos;

  sb = (Scheme_Set_Bang *)o;
  val = sb->val;

  /* Treat `set!` as allowing the right-hand side to escape.  (We
     could do better if `sb->var` is a variable that we know about.) */
  rhs_pos = scheme_false;

  val = letrec_check_expr(val, frame, rhs_pos);
  sb->val = val;

  if (SAME_TYPE(SCHEME_TYPE(sb->var), scheme_ir_local_type)) {
    /* We may need to insert a definedness check before the assignment */
    Letrec_Check_Frame *in_frame;
    int position;

    in_frame = get_relative_frame(&position, (Scheme_IR_Local *)sb->var);
    
    if (in_frame->ref
        && !(in_frame->ref[position] & LET_READY)) {
      /* Insert the check: */
      Scheme_App3_Rec *app3;
      Scheme_Object *name;
      Scheme_Sequence *seq;
      
      name = record_checked((Scheme_IR_Local *)sb->var, frame);
      
      app3 = MALLOC_ONE_TAGGED(Scheme_App3_Rec);
      app3->iso.so.type = scheme_application3_type;
      app3->rator = scheme_check_assign_not_undefined_proc;
      app3->rand1 = sb->var;
      app3->rand2 = name;
      
      seq = scheme_malloc_sequence(2);
      seq->so.type = scheme_sequence_type;
      seq->count = 2;
      seq->array[0] = (Scheme_Object *)app3;
      seq->array[1] = (Scheme_Object *)sb;

      return (Scheme_Object *)seq;
    }
  }

  return o;
}

static Scheme_Object *letrec_check_define_syntaxes(Scheme_Object *lam, Letrec_Check_Frame *frame, Scheme_Object *pos)
{
  Scheme_Object *val;
  val = SCHEME_VEC_ELS(lam)[3];

  val = letrec_check_expr(val, frame, pos);
  SCHEME_VEC_ELS(lam)[3] = val;

  return lam;
}

static Scheme_Object *letrec_check_begin_for_syntax(Scheme_Object *lam, Letrec_Check_Frame *frame, Scheme_Object *pos)
{
  Scheme_Object *l, *a, *val;
    
  l = SCHEME_VEC_ELS(lam)[2];
    
  while (!SCHEME_NULLP(l)) {
    a = SCHEME_CAR(l);
    val = letrec_check_expr(a, frame, pos);
    SCHEME_CAR(l) = val;
    l = SCHEME_CDR(l);
  }
    
  return lam;
}

static Scheme_Object *letrec_check_case_lambda(Scheme_Object *o, Letrec_Check_Frame *frame, Scheme_Object *pos)
{
  Scheme_Case_Lambda *cl;
  Scheme_Object *val;
  int i, n;

  cl = (Scheme_Case_Lambda *)o;

  n = cl->count;
  for (i = 0; i < n; i++) {
    val = letrec_check_expr(cl->array[i], frame, pos);
    cl->array[i] = val;
  }

  return o;
}

static Scheme_Object *letrec_check_begin0(Scheme_Object *o, Letrec_Check_Frame *frame, Scheme_Object *pos)
{
  int i, n;
  Scheme_Sequence *seq;
  Scheme_Object *val;
    
  seq = (Scheme_Sequence *)o;
    
  n = seq->count;
  for (i = 0; i < n; i++) {
    val = letrec_check_expr(seq->array[i], frame, pos);
    seq->array[i] = val;
  }

  return o;
}

static Scheme_Object *letrec_check_apply_values(Scheme_Object *lam, Letrec_Check_Frame *frame, Scheme_Object *pos)
{
  Scheme_Object *f, *e;
    
  f = SCHEME_PTR1_VAL(lam);
  e = SCHEME_PTR2_VAL(lam);
    
  f = letrec_check_expr(f, frame, pos);
  e = letrec_check_expr(e, frame, pos);

  SCHEME_PTR1_VAL(lam) = f;
  SCHEME_PTR2_VAL(lam) = e;
    
  return lam;
}

static Scheme_Object *letrec_check_module(Scheme_Object *o, Letrec_Check_Frame *frame, Scheme_Object *pos)
{
  int i, cnt;
  Scheme_Module *m;
  Scheme_Object *val;
  m = (Scheme_Module *)o;

  if (!m->comp_prefix) {
    /* already resolved */
    return (Scheme_Object *)m;
  }
    
  cnt = SCHEME_VEC_SIZE(m->bodies[0]);
  for(i = 0; i < cnt; i++) {
    val = SCHEME_VEC_ELS(m->bodies[0])[i];
    val = letrec_check_expr(val, frame, pos);
    SCHEME_VEC_ELS(m->bodies[0])[i] = val;
  }

  {
    /* check submodules */
    int k;
    Scheme_Object *p;
    for (k = 0; k < 2; k++) {
      p = (k ? m->post_submodules : m->pre_submodules);
      if (p) {
        while (!SCHEME_NULLP(p)) {
          letrec_check_expr(SCHEME_CAR(p), frame, pos);
          p = SCHEME_CDR(p);
        }
      }
    }
  }

  return o;
}

static Scheme_Object *letrec_check_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *expr = (Scheme_Object *)p->ku.k.p1;
  Letrec_Check_Frame *frame = (Letrec_Check_Frame *)p->ku.k.p3;
  Scheme_Object *pos = (Scheme_Object *)p->ku.k.p3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  return letrec_check_expr(expr, frame, pos);
}

static Scheme_Object *letrec_check_expr(Scheme_Object *expr, Letrec_Check_Frame *frame, Scheme_Object *pos)
{
  int type;

#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Thread *p = scheme_current_thread;

    p->ku.k.p1 = (void *)expr;
    p->ku.k.p2 = (void *)frame;
    p->ku.k.p3 = (void *)pos;

    return scheme_handle_stack_overflow(letrec_check_k);
  }
#endif

  type = SCHEME_TYPE(expr);

  SCHEME_USE_FUEL(1);

  switch (type) {
  case scheme_ir_local_type:
    return letrec_check_local(expr, frame, pos);
  case scheme_application_type:
    return letrec_check_application(expr, frame, pos);
  case scheme_application2_type:
    return letrec_check_application2(expr, frame, pos);
  case scheme_application3_type:
    return letrec_check_application3(expr, frame, pos);
  case scheme_sequence_type:
  case scheme_splice_sequence_type:
    return letrec_check_sequence(expr, frame, pos);
  case scheme_branch_type:
    return letrec_check_branch(expr, frame, pos);
  case scheme_with_cont_mark_type:
    return letrec_check_wcm(expr, frame, pos);
  case scheme_ir_lambda_type:
    return letrec_check_lambda(expr, frame, pos);
  case scheme_ir_let_header_type:
    return letrec_check_lets(expr, frame, pos);
  case scheme_ir_toplevel_type: /* var ref to a top level */
    return expr;
  case scheme_ir_quote_syntax_type:
    return expr;
  case scheme_variable_type:
  case scheme_module_variable_type:
    scheme_signal_error("got top-level in wrong place");
    return 0;
  case scheme_define_values_type:
    return letrec_check_define_values(expr, frame, pos);
  case scheme_varref_form_type:
    return letrec_check_ref(expr, frame, pos);
  case scheme_set_bang_type:
    return letrec_check_set(expr, frame, pos);
  case scheme_define_syntaxes_type:
    return letrec_check_define_syntaxes(expr, frame, pos);
  case scheme_begin_for_syntax_type:
    return letrec_check_begin_for_syntax(expr, frame, pos);
  case scheme_case_lambda_sequence_type:
    return letrec_check_case_lambda(expr, frame, pos);
  case scheme_begin0_sequence_type:
    return letrec_check_begin0(expr, frame, pos);
  case scheme_apply_values_type:
    return letrec_check_apply_values(expr, frame, pos);
  case scheme_with_immed_mark_type:
    scheme_signal_error("internal error: with-immediate-mark not expected before optimization");
    return NULL;
  case scheme_require_form_type:
    return expr;
  case scheme_module_type:
    return letrec_check_module(expr, frame, pos);
  default:
    return expr;
  }
}

Scheme_Object *scheme_letrec_check_expr(Scheme_Object *expr)
{
  Scheme_Object *val;
  Scheme_Object *init_pos = scheme_false;
  Letrec_Check_Frame *frame;

  frame = init_letrec_check_frame(FRAME_TYPE_TOP, LET_BODY_EXPR,
                                  0, NULL, NULL,
                                  NULL, NULL);

  /* (Grep "EXPL-3" for information): The `pos` argument, starting
     here as `init_pos`, represents whether we're in a non-application
     position for a particular variable's RHS. The value of `pos` is
     #f if we're in a (potential) application position; otherwise,
     it's a number or list of numbers corresponds to binding
     positions. We use a list of numbers for the RHS of a
     `let[rec]-values` form with multiple variables. */

  val = letrec_check_expr(expr, NULL, init_pos);

  clean_dead_deferred_expr(*frame->deferred_chain);

  return val;
}

/*========================================================================*/
/*                         precise GC traversers                          */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_letrec_check.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_letrec_check_frame, mark_letrec_check_frame);
  GC_REG_TRAV(scheme_deferred_expr_type, mark_scheme_deferred_expr);
}

END_XFORM_SKIP;

#endif
