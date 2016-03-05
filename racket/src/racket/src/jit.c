/*
  Racket
  Copyright (c) 2006-2016 PLT Design Inc.

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

#define DEFINE_LIGHTNING_FUNCS

#include "jit.h"

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

static void generate_case_lambda(Scheme_Case_Lambda *c, Scheme_Native_Lambda *nlam, int is_method);
static void *generate_lambda_simple_arity_check(int num_params, int has_rest, int is_method, int permanent);
int scheme_generate_non_tail_mark_pos_prefix(mz_jit_state *jitter);
void scheme_generate_non_tail_mark_pos_suffix(mz_jit_state *jitter);
static int lambda_has_been_jitted(Scheme_Native_Lambda *nlam);

void scheme_jit_fill_threadlocal_table();


typedef struct {
  Scheme_Native_Lambda nc;
  Scheme_Native_Lambda *case_lam;
} Scheme_Native_Lambda_Plus_Case;

static Scheme_Object *make_global_ref(Scheme_Object *var, Scheme_Object *dummy)
{
  GC_CAN_IGNORE Scheme_Object *o;

  o = scheme_alloc_object();
  o->type = scheme_global_ref_type;
  SCHEME_PTR1_VAL(o) = var;
  SCHEME_PTR2_VAL(o) = dummy;

  return o;
}

static Scheme_Object *make_global_const_ref(Scheme_Object *var, Scheme_Object *dummy)
{
  GC_CAN_IGNORE Scheme_Object *o;

  o = make_global_ref(var, dummy);
  SCHEME_VARREF_FLAGS(o) |= 0x1;

  return o;
}

/*========================================================================*/
/*                               run time                                 */
/*========================================================================*/

static MZ_INLINE Scheme_Object *do_make_native_closure(Scheme_Native_Lambda *code, int size)
{
  Scheme_Native_Closure *o;

  o = (Scheme_Native_Closure *)scheme_malloc_tagged(sizeof(Scheme_Native_Closure)
						    + ((size - mzFLEX_DELTA) * sizeof(Scheme_Object *)));
  o->so.type = scheme_native_closure_type;
  o->code = code;

  return (Scheme_Object *)o;
}

Scheme_Object *scheme_make_native_closure(Scheme_Native_Lambda *code)
{
  return do_make_native_closure(code, code->closure_size);
}

Scheme_Object *scheme_make_native_case_closure(Scheme_Native_Lambda *code)
{
  return do_make_native_closure(code, -(code->closure_size + 1));
}

static void call_set_global_bucket(Scheme_Bucket *b, Scheme_Object *val, int set_undef)
{
  scheme_set_global_bucket("set!", b, val, set_undef);
}

static void lexical_binding_wrong_return_arity(int expected, int got, Scheme_Object **argv)
{
  scheme_wrong_return_arity(NULL, expected, got, argv, "\n  in: local-binding form");
}

static void wrong_argument_count(Scheme_Object *proc, int argc, Scheme_Object **argv)
{
  scheme_wrong_count((char *)proc, -1, -1, argc, argv);
}

static Scheme_Object *clear_rs_arguments(Scheme_Object *v, int size, int delta) XFORM_SKIP_PROC
{
  int i;
  Scheme_Object **argv = MZ_RUNSTACK;
  for (i = size; i-- > delta; ) {
    argv[i] = NULL;
  }
  return v;
}

#define JIT_TS_PROCS
#define JIT_BOX_TS_PROCS
#include "jit_ts.c"

/*========================================================================*/
/*                      lightweight continuations                         */
/*========================================================================*/

THREAD_LOCAL_DECL(Scheme_Current_LWC *scheme_current_lwc);

static Scheme_Object *do_call_as_lwc(Scheme_Native_Proc *code,
                                     void *data,
                                     int argc,
                                     Scheme_Object **argv,
                                     MZ_MARK_STACK_TYPE cont_mark_stack_start)
{
#ifdef JIT_THREAD_LOCAL
# define THDLOC &BOTTOM_VARIABLE
#else
# define THDLOC NULL
#endif
  scheme_current_lwc->runstack_start = MZ_RUNSTACK;
  scheme_current_lwc->cont_mark_stack_start = cont_mark_stack_start;
  return sjc.native_starter_code(data, argc, argv, THDLOC, code, (void **)&scheme_current_lwc->stack_start);
#undef THDLOC
}

Scheme_Object *scheme_call_as_lightweight_continuation(Scheme_Native_Proc *code,
                                                       void *data,
                                                       int argc,
                                                       Scheme_Object **argv)
{
  return do_call_as_lwc(code, data, argc, argv, MZ_CONT_MARK_STACK);
}

#ifdef MZ_USE_FUTURES
Scheme_Object *scheme_force_value_same_mark_as_lightweight_continuation(Scheme_Object *v)
{
  /* Providing 0 as cont_mark_stack_start is the "same_mark" part:
     it preserves any continuation marks that are in place as part
     of the continuation. */
  return do_call_as_lwc(sjc.force_value_same_mark_code, NULL, 0, NULL, 0);
}
#endif

void scheme_fill_stack_lwc_end(void) XFORM_SKIP_PROC
{
#ifdef JIT_THREAD_LOCAL
  scheme_current_lwc->saved_save_fp = scheme_jit_save_fp;
# ifdef MZ_LONG_DOUBLE
  scheme_current_lwc->saved_save_extfp = scheme_jit_save_extfp;
# endif
#endif
}

typedef Scheme_Object *(*Continuation_Abort_Code)(void *result, void *stack_pos);

void *scheme_save_lightweight_continuation_stack(Scheme_Current_LWC *lwc)
  XFORM_SKIP_PROC
/* This function assumes that lwc won't move during an
   allocation. Also, if allocation fails, it can abort and return
   NULL, so it can work in a thread for running futures (where
   allocation and GC in general are disallowed). */

{
  /* Copies just the C-stack part for a lightweight continuation.
     Copying the runstack, copying the cont-mark stack and saving
     arguments is up to the caller. */
  void *p;
  intptr_t size;

  size = (intptr_t)lwc->stack_start - (intptr_t)lwc->stack_end;

  p = (void *)scheme_malloc_atomic(size);
  if (!p) return NULL;

  memcpy(p, lwc->stack_end, size);

  return p;
}

#ifdef MZ_USE_LWC
Scheme_Object *scheme_jit_continuation_apply_install(Apply_LWC_Args *args) XFORM_SKIP_PROC
{
  intptr_t delta, cm_delta;
  void **old_fp, **new_fp, **next_old_fp, **next_new_fp;
  Scheme_Current_LWC *lwc;
  void *new_stack_start;

  /* application of a lightweight continuation forms a lightweight continuation: */
  scheme_current_lwc->stack_start = args->dest_stack_pos;

  /* args->dest_stack_pos has been set, and room has been made on the stack */
  new_stack_start = (char *)args->dest_stack_pos - (intptr_t)args->full_size;
  memcpy(new_stack_start, args->copy_to_install, args->copy_size);

  lwc = args->lwc;

  args->new_runstack = MZ_RUNSTACK;
  args->new_runstack_base = MZ_RUNSTACK + (lwc->runstack_base_end - lwc->runstack_end);
#ifdef USE_THREAD_LOCAL
  args->new_threadlocal = &BOTTOM_VARIABLE;
  scheme_jit_save_fp = lwc->saved_save_fp;
# ifdef MZ_LONG_DOUBLE
  scheme_jit_save_extfp = lwc->saved_save_extfp;
# endif
#endif

  delta = (intptr_t)new_stack_start - (intptr_t)lwc->stack_end;
  cm_delta = (intptr_t)MZ_CONT_MARK_STACK - (intptr_t)lwc->cont_mark_stack_end;

  /* fix frame pointers, etc. */
  old_fp = lwc->frame_end;
  new_fp = NULL;
  while ((uintptr_t)old_fp < (uintptr_t)lwc->stack_start) {
    new_fp = (void **)((char *)old_fp + delta);
    /* we usually do not copy/update the very last frame pointer, so check: */
    if ((uintptr_t)old_fp < ((uintptr_t)lwc->stack_end + args->copy_size)) {
      /* we need to update */
      next_old_fp = *new_fp;
      next_new_fp = (void **)((char *)next_old_fp + delta);
      old_fp = next_old_fp;
    } else {
      /* no updates from here on; force old_lp to be past the saved area: */
      old_fp = lwc->stack_start;
      next_new_fp = NULL;
    }

    /* fixups of local variables in the fraame */
#if defined(JIT_X86_64) || defined(JIT_X86_PLAIN)
    new_fp[-4] = (void *)((intptr_t)new_fp[-4] + cm_delta); /* LOCAL1 */
#endif
#ifdef JIT_X86_PLAIN
# ifdef JIT_THREAD_LOCAL
#  ifdef THREAD_LOCAL_USES_JIT_V2
    /* LOCAL4 = RUNSTACK_BASE */
    new_fp[-JIT_LOCAL4_OFFSET] = (MZ_RUNSTACK + ((Scheme_Object **)new_fp[-JIT_LOCAL4_OFFSET] 
                                                 - lwc->runstack_end));
#  else
    new_fp[-JIT_LOCAL4_OFFSET] = &BOTTOM_VARIABLE; /* LOCAL4 = THREAD_LOCAL */
#  endif
# endif
#endif

    if ((uintptr_t)old_fp < (uintptr_t)lwc->stack_start) {
      new_fp[0] = next_new_fp;

      /* fixups for saved registers in a call from JIT-generated code */
#ifdef JIT_X86_64
      new_fp[-1] = MZ_RUNSTACK + ((Scheme_Object **)new_fp[-1] - lwc->runstack_end); /* JIT_RUNSTACK */
      new_fp[-3] = MZ_RUNSTACK + ((Scheme_Object **)new_fp[-3] - lwc->runstack_end); /* JIT_RUNSTACK_BASE */
# ifdef JIT_THREAD_LOCAL
      new_fp[-JIT_LOCAL4_OFFSET] = &BOTTOM_VARIABLE; /* LOCAL4 */
# endif
#endif
#ifdef JIT_X86_PLAIN
      new_fp[-1] = MZ_RUNSTACK + ((Scheme_Object **)new_fp[-1] - lwc->runstack_end); /* JIT_RUNSTACK */
# ifdef THREAD_LOCAL_USES_JIT_V2
      new_fp[-3] = &BOTTOM_VARIABLE;
# else
      new_fp[-3] = MZ_RUNSTACK + ((Scheme_Object **)new_fp[-3] - lwc->runstack_end); /* JIT_RUNSTACK_BASE */
# endif
#endif
    } else {
#ifdef JIT_X86_64
# ifdef JIT_THREAD_LOCAL
      /* topmost frame holds the original R14: */
      new_fp[-JIT_LOCAL4_OFFSET] = (void *)args->saved_r14; /* LOCAL4 */
# endif
#endif
    }
  }

  /* jump to the old code */
  new_fp = (void **)((char *)lwc->frame_end + delta);
  sjc.continuation_apply_finish_code(args, new_stack_start, new_fp);

  return NULL;
}
#endif

Scheme_Object *scheme_apply_lightweight_continuation_stack(Scheme_Current_LWC *lwc, void *stack,
                                                           Scheme_Object *result) XFORM_SKIP_PROC
{
  /* Restores just the C-stack part and uses the given (updated)
     arguments. Restring the runstack and cont-mark stack is up to the
     caller. */
  Apply_LWC_Args args;
  intptr_t size;

  size = (intptr_t)lwc->stack_start - (intptr_t)lwc->stack_end;
  args.full_size = size;

  /* Adjust size to skip stack part that we don't want to overwrite. */
#ifdef JIT_X86_64
  size -= 4 * sizeof(void*); /* frame pointer and 3 saved registers */
#endif
#ifdef JIT_X86_PLAIN
  size -= 4 * sizeof(void*); /* frame pointer and 3 saved registers */
#endif
  args.copy_size = size;

  args.lwc = lwc;
  args.copy_to_install = stack;
  args.result = result;

  return sjc.continuation_apply_indirect_code(&args, size);
}

/*========================================================================*/
/*                         bytecode properties                            */
/*========================================================================*/

#ifdef USE_FLONUM_UNBOXING
int scheme_jit_check_closure_flonum_bit(Scheme_Lambda *lam, int pos, int delta)
{
  int ct;
  pos += delta;
  ct = scheme_boxmap_get(lam->closure_map, pos, lam->closure_size);
  if (ct == (LAMBDA_TYPE_TYPE_OFFSET + SCHEME_LOCAL_TYPE_FLONUM))
    return 1;
  else
    return 0;
}
int scheme_jit_check_closure_extflonum_bit(Scheme_Lambda *lam, int pos, int delta)
{
#ifdef MZ_LONG_DOUBLE
  int ct;
  pos += delta;
  ct = scheme_boxmap_get(lam->closure_map, pos, lam->closure_size);
  if (ct == (LAMBDA_TYPE_TYPE_OFFSET + SCHEME_LOCAL_TYPE_EXTFLONUM))
    return 1;
  else
    return 0;
#else
  return 0;
#endif
}
#endif

#ifdef NEED_LONG_BRANCHES
static int is_short(Scheme_Object *obj, int fuel)
{
  Scheme_Type t;

  if (fuel <= 0)
    return fuel;

  t = SCHEME_TYPE(obj);

  switch (t) {
  case scheme_case_lambda_sequence_type:
    return fuel - 1;
    break;
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)obj;
      int i;

      fuel -= app->num_args;
      for (i = app->num_args + 1; i--; ) {
	fuel = is_short(app->args[i], fuel);
      }
      return fuel;
    }
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)obj;
      fuel -= 2;
      fuel = is_short(app->rator, fuel);
      return is_short(app->rand, fuel);
    }
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)obj;
      fuel -= 3;
      fuel = is_short(app->rator, fuel);
      fuel = is_short(app->rand1, fuel);
      return is_short(app->rand2, fuel);
    }
  case scheme_sequence_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)obj;
      int i;

      fuel -= seq->count;
      for (i = seq->count; i--; ) {
	fuel = is_short(seq->array[i], fuel);
      }
      return fuel;
    }
    break;
  case scheme_branch_type:
    {
      Scheme_Branch_Rec *branch = (Scheme_Branch_Rec *)obj;
      fuel -= 3;
      fuel = is_short(branch->test, fuel);
      fuel = is_short(branch->tbranch, fuel);
      return is_short(branch->fbranch, fuel);
    }
  case scheme_toplevel_type:
  case scheme_quote_syntax_type:
  case scheme_local_type:
  case scheme_local_unbox_type:
  case scheme_lambda_type:
    return fuel - 1;
  default:
    if (t > _scheme_values_types_)
      return fuel - 1;
    else
      return 0;
  }
}
#endif

Scheme_Object *scheme_extract_global(Scheme_Object *o, Scheme_Native_Closure *nc, int local_only)
{
  /* GLOBAL ASSUMPTION: we assume that globals are the last thing
     in the closure; grep for "GLOBAL ASSUMPTION" in fun.c. */
  Scheme_Prefix *globs;
  int pos;

  globs = (Scheme_Prefix *)nc->vals[nc->code->u2.orig_code->closure_size - 1];
  pos = SCHEME_TOPLEVEL_POS(o);

  if (local_only) {
    /* Look for local bindings when the JIT depends on information that is not
       validated across module boundaries. */
    scheme_signal_error("internal error: import map not available");
  }

  return globs->a[pos];
}

static Scheme_Object *extract_syntax(Scheme_Quote_Syntax *qs, Scheme_Native_Closure *nc)
{
  /* GLOBAL ASSUMPTION: we assume that globals are the last thing
     in the closure; grep for "GLOBAL ASSUMPTION" in fun.c. */
  Scheme_Prefix *globs;
  int i, pos;
  Scheme_Object *v;

  globs = (Scheme_Prefix *)nc->vals[nc->code->u2.orig_code->closure_size - 1];

  i = qs->position;
  pos = qs->midpoint;

  v = globs->a[i+pos+1];
  if (!v) {
    v = globs->a[pos];
    v = scheme_delayed_shift((Scheme_Object **)v, i);
    globs->a[i+pos+1] = v;
  }

  return v;
}

static Scheme_Object *extract_closure_local(int pos, mz_jit_state *jitter, int get_constant)
{
  if (PAST_LIMIT()) return NULL;

  if (pos >= jitter->self_pos - jitter->self_to_closure_delta) {
    pos -= (jitter->self_pos - jitter->self_to_closure_delta);
    if (pos < jitter->nc->code->u2.orig_code->closure_size) {
      /* in the closure */
      if (!get_constant
          || (SCHEME_NATIVE_LAMBDA_FLAGS(jitter->nc->code) & NATIVE_SPECIALIZED))
        return jitter->nc->vals[pos];
    } else if (!get_constant) {
      /* maybe an example argument... which is useful when
         the enclosing function has been lifted, converting
         a closure element into an argument */
      pos -= jitter->closure_to_args_delta;
      if (pos < jitter->example_argc)
        return jitter->example_argv[pos + jitter->example_argv_delta];
    }
  }

  return NULL;
}

Scheme_Object *scheme_extract_closure_local(Scheme_Object *obj, mz_jit_state *jitter,
                                            int extra_push, int get_constant)
{
  int pos;

  pos = SCHEME_LOCAL_POS(obj);
  pos -= extra_push;
  return extract_closure_local(pos, jitter, get_constant);
}


Scheme_Object *scheme_specialize_to_constant(Scheme_Object *obj, mz_jit_state *jitter, int extra_push)
{
  Scheme_Object *c;

  if (PAST_LIMIT()) return obj;

  if (SCHEME_NATIVE_LAMBDA_FLAGS(jitter->nc->code) & NATIVE_SPECIALIZED) {
    if (SAME_TYPE(SCHEME_TYPE(obj), scheme_local_type)) {
      c = scheme_extract_closure_local(obj, jitter, extra_push, 1);
      if (c) {
        MZ_ASSERT(SCHEME_TYPE(c) != scheme_prefix_type);
        return c;
      }
    }

    if (SAME_TYPE(SCHEME_TYPE(obj), scheme_toplevel_type)) {
      c = scheme_extract_global(obj, jitter->nc, 0);
      if (c) {
        c = ((Scheme_Bucket *)c)->val;
        if (c)
          return c;
      }
    }
  }

  return obj;
}

int scheme_native_closure_preserves_marks(Scheme_Object *p)
{
  Scheme_Native_Lambda *nlam = ((Scheme_Native_Closure *)p)->code;

  if (nlam->closure_size >= 0) { /* not case-lambda */
    if (lambda_has_been_jitted(nlam)) {
      if (SCHEME_NATIVE_LAMBDA_FLAGS(nlam) & NATIVE_PRESERVES_MARKS)
        return 1;
    } else {
      if (SCHEME_LAMBDA_FLAGS(nlam->u2.orig_code) & LAMBDA_PRESERVES_MARKS)
        return 1;
    }
  }

  return 0;
}

int scheme_is_noncm(Scheme_Object *a, mz_jit_state *jitter, int depth, int stack_start)
{
  a = scheme_specialize_to_constant(a, jitter, stack_start);

  if (SCHEME_PRIMP(a)) {
    int opts;
    opts = ((Scheme_Prim_Proc_Header *)a)->flags & SCHEME_PRIM_OPT_MASK;
    if (opts >= SCHEME_PRIM_OPT_NONCM) {
      /* Structure-type predicates are handled specially, so don't claim NONCM: */
      if ((((Scheme_Prim_Proc_Header *)a)->flags & SCHEME_PRIM_OTHER_TYPE_MASK)
          == SCHEME_PRIM_STRUCT_TYPE_PRED)
        return 0;
      return 1;
    }
  }

  if (depth 
      && jitter->nc 
      && SAME_TYPE(SCHEME_TYPE(a), scheme_toplevel_type)
      && ((SCHEME_TOPLEVEL_FLAGS(a) & SCHEME_TOPLEVEL_FLAGS_MASK) >= SCHEME_TOPLEVEL_CONST)) {
    Scheme_Object *p;
    p = scheme_extract_global(a, jitter->nc, 0);
    if (p) {
      p = ((Scheme_Bucket *)p)->val;
      if (p && SAME_TYPE(SCHEME_TYPE(p), scheme_native_closure_type)) {
        if (scheme_native_closure_preserves_marks(p))
          return 1;
      }
    }
  }

  if (SAME_TYPE(SCHEME_TYPE(a), scheme_local_type)) {
    int pos = SCHEME_LOCAL_POS(a) - stack_start;
    if (pos >= 0) {
      int flags;
      if (scheme_mz_is_closure(jitter, pos, -1, &flags)) {
        return (flags & NATIVE_PRESERVES_MARKS);
      }
    }
  }

  if (depth && SAME_TYPE(SCHEME_TYPE(a), scheme_closure_type)) {
    Scheme_Lambda *lam;

    lam = ((Scheme_Closure *)a)->code;
    if (SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_PRESERVES_MARKS)
      return 1;
  }

  return 0;
}

int scheme_is_simple(Scheme_Object *obj, int depth, int just_markless, mz_jit_state *jitter, int stack_start)
{
  /* Return 1 if evaluating `obj' doesn't change the runstack or cont-mark stack ---
     or, if just_markless is 1, doesn't use the cont-mark stack.
     If a form doesn't itself change/use the stack, then check all
     expressions in tail position, up to some depth. The conservative
     answer is always 0. */
  Scheme_Type type;

  type = SCHEME_TYPE(obj);

  switch (type) {
  case scheme_case_lambda_sequence_type:
    return 1;
    break;

  case scheme_sequence_type:
    if (depth) {
      Scheme_Sequence *seq = (Scheme_Sequence *)obj;

      return scheme_is_simple(seq->array[seq->count - 1], depth - 1, 
                              just_markless, jitter, stack_start);
    }
    break;

  case scheme_branch_type:
    if (depth) {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)obj;
      return (scheme_is_simple(b->tbranch, depth - 1, just_markless, jitter, stack_start)
	      && scheme_is_simple(b->fbranch, depth - 1, just_markless, jitter, stack_start));
    }
    break;
    
  case scheme_let_value_type:
    if (depth) {
      return scheme_is_simple(((Scheme_Let_Value *)obj)->body, depth - 1, just_markless, jitter, stack_start);
    }
    break;
  case scheme_let_one_type:
    if (just_markless && depth) {
      return scheme_is_simple(((Scheme_Let_One *)obj)->body, depth - 1, just_markless, jitter, stack_start + 1);
    }
    break;
  case scheme_let_void_type:
    if (just_markless && depth) {
      return scheme_is_simple(((Scheme_Let_Void *)obj)->body, depth - 1, just_markless, jitter,
                              stack_start + ((Scheme_Let_Void *)obj)->count);
    }
    break;
  case scheme_letrec_type:
    if (just_markless && depth) {
      return scheme_is_simple(((Scheme_Letrec *)obj)->body, depth - 1, just_markless, jitter,
                              stack_start + ((Scheme_Letrec *)obj)->count);
    }
    break;

  case scheme_application_type:
    {
      Scheme_Object *rator;
      rator = scheme_specialize_to_constant(((Scheme_App_Rec *)obj)->args[0], jitter,
                                            stack_start +  ((Scheme_App_Rec *)obj)->num_args);
      if (scheme_inlined_nary_prim(rator, obj, jitter)
          && !SAME_OBJ(rator, scheme_values_proc))
        return 1;
      if (just_markless) {
        return scheme_is_noncm(rator, jitter, depth, 
                               stack_start + ((Scheme_App_Rec *)obj)->num_args);
      }
    }
    break;
  case scheme_application2_type:
    {
      Scheme_Object *rator;
      rator = scheme_specialize_to_constant(((Scheme_App2_Rec *)obj)->rator, jitter, stack_start + 1);
      if (scheme_inlined_unary_prim(rator, obj, jitter))
        return 1;
      else if (just_markless) {
        return scheme_is_noncm(rator, jitter, depth, stack_start + 1);
      }
    }
    break;
  case scheme_application3_type:
    {
      Scheme_Object *rator;
      rator = scheme_specialize_to_constant(((Scheme_App3_Rec *)obj)->rator, jitter, stack_start + 2);
      if (scheme_inlined_binary_prim(rator, obj, jitter)
          && !SAME_OBJ(rator, scheme_values_proc)) 
        return 1;
      else if (just_markless) {
        return scheme_is_noncm(rator, jitter, depth, stack_start + 2);
      }
    }
    break;
    
  case scheme_toplevel_type:
  case scheme_quote_syntax_type:
  case scheme_local_type:
  case scheme_local_unbox_type:
  case scheme_lambda_type:
    return 1;
    break;
  }

  return (type > _scheme_values_types_);
}

int scheme_is_non_gc(Scheme_Object *obj, int depth)
{
  /* Return 1 if evaluating `obj' can't trigger a GC. */
  Scheme_Type type;

  type = SCHEME_TYPE(obj);

  switch (type) {
  case scheme_branch_type:
    if (depth) {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)obj;
      return (scheme_is_non_gc(b->test, depth - 1)
	      && scheme_is_non_gc(b->tbranch, depth - 1)
	      && scheme_is_non_gc(b->fbranch, depth - 1));
    }
    break;
    
  case scheme_let_value_type:
    if (depth) {
      Scheme_Let_Value *lv = (Scheme_Let_Value *)obj;
      if (SCHEME_LET_VALUE_AUTOBOX(lv))
        return 0;
      return scheme_is_non_gc(lv->body, depth - 1);
    }
    break;
  case scheme_let_one_type:
    if (depth) {
      return (scheme_is_non_gc(((Scheme_Let_One *)obj)->value, depth - 1)
              && scheme_is_non_gc(((Scheme_Let_One *)obj)->body, depth - 1));
    }
    break;
  case scheme_let_void_type:
    if (depth) {
      Scheme_Let_Void *lv = (Scheme_Let_Void *)obj;
      if (SCHEME_LET_VOID_AUTOBOX(lv))
        return 0;
      return scheme_is_non_gc(lv->body, depth - 1);
    }
    break;
  case scheme_letrec_type:
    break;

  case scheme_application_type:
    break;
  case scheme_application2_type:
    break;
  case scheme_application3_type:
    break;

  case scheme_toplevel_type:
    break;
  case scheme_lambda_type:
    break;

  case scheme_local_type:
    if (JIT_TYPE_NEEDS_BOXING(SCHEME_GET_LOCAL_TYPE(obj)))
      return 0;
    return 1;
    break;
    
  case scheme_quote_syntax_type:
  case scheme_local_unbox_type:
    return 1;
    break;
  }

  return (type > _scheme_values_types_);
}

static int is_a_procedure(Scheme_Object *v, mz_jit_state *jitter)
{
  Scheme_Type t;

  if (SCHEME_PROCP(v))
    return 1;

  t = SCHEME_TYPE(v);
  if (SAME_TYPE(t, scheme_closure_type)
      || SAME_TYPE(t, scheme_lambda_type))
    return 1;
  else if (SAME_TYPE(t, scheme_case_lambda_sequence_type)) {
    return 1;
  } else if (SAME_TYPE(t, scheme_local_type)) {
    int flags;
    return scheme_mz_is_closure(jitter, SCHEME_LOCAL_POS(v), -1, &flags);
  } else if (t == scheme_toplevel_type) {
    if ((SCHEME_TOPLEVEL_FLAGS(v) & SCHEME_TOPLEVEL_FLAGS_MASK) >= SCHEME_TOPLEVEL_CONST) {
      if (jitter->nc) {
	Scheme_Object *p;
        
	p = scheme_extract_global(v, jitter->nc, 0);
        if (p) {
          p = ((Scheme_Bucket *)p)->val;
          return SAME_TYPE(SCHEME_TYPE(p), scheme_native_closure_type);
        }
      }
    }
  }

  return 0;
}

int scheme_ok_to_move_local(Scheme_Object *obj)
{
  if (SAME_TYPE(SCHEME_TYPE(obj), scheme_local_type)) {
    int flags = SCHEME_GET_LOCAL_FLAGS(obj);
    if (!flags
        || ((flags > SCHEME_LOCAL_TYPE_OFFSET)
            && !JIT_TYPE_NEEDS_BOXING(flags - SCHEME_LOCAL_TYPE_OFFSET)))
      return 1;
  }
  
  return 0;
}

int scheme_ok_to_delay_local(Scheme_Object *obj)
{
  if (SAME_TYPE(SCHEME_TYPE(obj), scheme_local_type)
      /* We can delay if the clears flag is set and no type: */
      && (SCHEME_GET_LOCAL_FLAGS(obj) <= SCHEME_LOCAL_CLEAR_ON_READ)) {
    return 1;
  } else
    return 0;
}

int scheme_can_delay_and_avoids_r1_r2(Scheme_Object *obj)
{
  Scheme_Type t = SCHEME_TYPE(obj);

  if (SAME_TYPE(t, scheme_local_type) && scheme_ok_to_delay_local(obj)) {
    return 1;
  } else
    return (t >= _scheme_ir_values_types_);
}

int scheme_can_delay_and_avoids_r1(Scheme_Object *obj)
{
  Scheme_Type t = SCHEME_TYPE(obj);

  if (SAME_TYPE(t, scheme_toplevel_type)) {
    return (((SCHEME_TOPLEVEL_FLAGS(obj) & SCHEME_TOPLEVEL_FLAGS_MASK) >= SCHEME_TOPLEVEL_FIXED)
            ? 1
            : 0);
  } else
    return scheme_can_delay_and_avoids_r1_r2(obj);
}

int scheme_is_constant_and_avoids_r1(Scheme_Object *obj)
{
  Scheme_Type t = SCHEME_TYPE(obj);

  if (SAME_TYPE(t, scheme_toplevel_type)) {
    return (((SCHEME_TOPLEVEL_FLAGS(obj) & SCHEME_TOPLEVEL_FLAGS_MASK) >= SCHEME_TOPLEVEL_FIXED)
            ? 1
            : 0);
  } else if (SAME_TYPE(t, scheme_local_type) && scheme_ok_to_move_local(obj)) {
    return 1;
  } else
    return (t >= _scheme_ir_values_types_);
}

static int expression_avoids_clearing_local(Scheme_Object *wrt, int pos, int fuel)
{
  Scheme_Type t;
  t = SCHEME_TYPE(wrt);

  if (t > _scheme_values_types_)
    return 1;
  else if (SAME_TYPE(t, scheme_local_type))
    return ((SCHEME_LOCAL_POS(wrt) != pos)
            || !(SCHEME_GET_LOCAL_FLAGS(wrt) == SCHEME_LOCAL_CLEAR_ON_READ));
  else if (SAME_TYPE(t, scheme_toplevel_type))
    return 1;
  else if (t == scheme_application2_type) {
    Scheme_App2_Rec *app = (Scheme_App2_Rec *)wrt;
    if (fuel < 0) return 0;
    if (expression_avoids_clearing_local(app->rator, pos + 1, fuel - 1)
        && expression_avoids_clearing_local(app->rand, pos + 1, fuel - 1))
      return 1;
  } else if (t == scheme_application3_type) {
    Scheme_App3_Rec *app = (Scheme_App3_Rec *)wrt;
    if (fuel < 0) return 0;
    if (expression_avoids_clearing_local(app->rator, pos + 2, fuel - 1)
        && expression_avoids_clearing_local(app->rand1, pos + 2, fuel - 1)
        && expression_avoids_clearing_local(app->rand2, pos + 2, fuel - 1))
      return 1;
  }

  return 0;
}

int scheme_is_relatively_constant_and_avoids_r1_maybe_fp(Scheme_Object *obj, Scheme_Object *wrt,
                                                         int fp_ok, int extfl)
{
  Scheme_Type t;

  if (scheme_is_constant_and_avoids_r1(obj))
    return 1;

  t = SCHEME_TYPE(obj);
  if (SAME_TYPE(t, scheme_local_type)) {
    /* Must have clearing, other-clears, or type flag set,
       otherwise is_constant_and_avoids_r1() would have returned 1. */
    if (SCHEME_GET_LOCAL_TYPE(obj) == SCHEME_LOCAL_TYPE_FLONUM)
      return (fp_ok && !extfl);
#ifdef MZ_LONG_DOUBLE
    else if (SCHEME_GET_LOCAL_TYPE(obj) == SCHEME_LOCAL_TYPE_EXTFLONUM)
      return (fp_ok && extfl);
#endif
    else if (expression_avoids_clearing_local(wrt, SCHEME_LOCAL_POS(obj), 3))
      /* different local vars, sp order doesn't matter */
      return 1;
  }

  return 0;
}

int scheme_is_relatively_constant_and_avoids_r1(Scheme_Object *obj, Scheme_Object *wrt)
{
  return scheme_is_relatively_constant_and_avoids_r1_maybe_fp(obj, wrt, 0, 0);
}

int scheme_needs_only_target_register(Scheme_Object *obj, int and_can_reorder)
{
  Scheme_Type t;

  if (scheme_is_constant_and_avoids_r1(obj))
    return 1;

  t = SCHEME_TYPE(obj);
  if (SAME_TYPE(t, scheme_local_type)) {
    int flags = SCHEME_GET_LOCAL_FLAGS(obj);
    if (and_can_reorder && (flags && (flags <= SCHEME_LOCAL_OTHER_CLEARS)))
      return 0;
    if (JIT_TYPE_NEEDS_BOXING(flags - SCHEME_LOCAL_TYPE_OFFSET))
      return 0;
    return 1;
  } else 
    return (t >= _scheme_ir_values_types_);
}

int scheme_native_closure_is_single_result(Scheme_Object *rator)
{
  Scheme_Native_Closure *nc = (Scheme_Native_Closure *)rator;
  if (nc->code->start_code == scheme_on_demand_jit_code)
    return (SCHEME_LAMBDA_FLAGS(nc->code->u2.orig_code) & LAMBDA_SINGLE_RESULT);
  else
    return (SCHEME_NATIVE_LAMBDA_FLAGS(nc->code) & NATIVE_IS_SINGLE_RESULT);
}

static int produces_single_value(Scheme_Object *rator, int num_args, mz_jit_state *jitter)
{
  rator = scheme_specialize_to_constant(rator, jitter, num_args);

  if (SAME_TYPE(SCHEME_TYPE(rator), scheme_native_closure_type))
    return scheme_native_closure_is_single_result(rator);

  if (SCHEME_PRIMP(rator)) {
    int opt;
    opt = ((Scheme_Prim_Proc_Header *)rator)->flags & SCHEME_PRIM_OPT_MASK;
    if (opt >= SCHEME_PRIM_OPT_NONCM)
      return 1;

   /* special case: (values <expr>) */
   if (SAME_OBJ(rator, scheme_values_proc) && (num_args == 1))
     return 1;
  }

  return 0;
}

static int is_single_valued(Scheme_Object *obj, mz_jit_state *jitter)
{
  Scheme_Type t = SCHEME_TYPE(obj);

  switch(t) {
  case scheme_application_type:
    return produces_single_value(((Scheme_App_Rec *)obj)->args[0], ((Scheme_App_Rec *)obj)->num_args, jitter);
    break;
  case scheme_application2_type:
    return produces_single_value(((Scheme_App2_Rec *)obj)->rator, 1, jitter);
    break;
  case scheme_application3_type:
    return produces_single_value(((Scheme_App3_Rec *)obj)->rator, 2, jitter);
    break;
  default:
    if (t > _scheme_values_types_)
      return 1;
  }

  return 0;
}

/*========================================================================*/
/*                             branch info                                */
/*========================================================================*/

static void patch_branch_addr(mz_jit_state *jitter, Branch_Info_Addr *addr, int i)
{
  if (addr[i].kind == BRANCH_ADDR_BRANCH) {
    mz_patch_branch(addr[i].addr);
  } else if (addr[i].kind == BRANCH_ADDR_UCBRANCH) {
    mz_patch_ucbranch(addr[i].addr);
  } else {
    jit_patch_movi(addr[i].addr, jit_get_ip());
  }
}

static void add_branch(Branch_Info *for_branch, GC_CAN_IGNORE jit_insn *ref, int mode, int kind)
{
  if (ref) {
    if (for_branch->addrs_count == for_branch->addrs_size) {
      int size = 2 * for_branch->addrs_size;
      Branch_Info_Addr *addrs;
      addrs = MALLOC_N_ATOMIC(Branch_Info_Addr, size);
      memcpy(addrs, for_branch->addrs, sizeof(Branch_Info_Addr) * for_branch->addrs_size);
      for_branch->addrs = addrs;
      for_branch->addrs_size = size;
    }

    for_branch->addrs[for_branch->addrs_count].addr = ref;
    for_branch->addrs[for_branch->addrs_count].mode = mode;
    for_branch->addrs[for_branch->addrs_count].kind = kind;
    for_branch->addrs_count++;
  }
}

void scheme_add_or_patch_branch_true_uc(mz_jit_state *jitter, Branch_Info *for_branch, GC_CAN_IGNORE jit_insn *ref)
/* Short-jump mode for addr branch should be consistent with for_branch->banch_short */
{
  if (for_branch->true_needs_jump) {
    add_branch(for_branch, ref, BRANCH_ADDR_TRUE, BRANCH_ADDR_UCBRANCH);
  } else {
    mz_patch_ucbranch(ref);
  }
}

void scheme_add_or_patch_branch_true_movi(mz_jit_state *jitter, Branch_Info *for_branch, GC_CAN_IGNORE jit_insn *ref)
/* Short-jump mode for addr move should be consistent with for_branch->banch_short */
{
  if (for_branch->true_needs_jump) {
    add_branch(for_branch, ref, BRANCH_ADDR_TRUE, BRANCH_ADDR_MOVI);
  } else {
    jit_patch_movi(ref, jit_get_ip());
  }
}

void scheme_add_branch_false(Branch_Info *for_branch, GC_CAN_IGNORE jit_insn *ref)
/* Short-jump mode for addr branch should be consistent with for_branch->banch_short */
{
  add_branch(for_branch, ref, BRANCH_ADDR_FALSE, BRANCH_ADDR_BRANCH);
}

void scheme_add_branch_false_movi(Branch_Info *for_branch, GC_CAN_IGNORE jit_insn *ref)
/* Short-jump mode for addr move should be consistent with for_branch->branch_short */
{
  add_branch(for_branch, ref, BRANCH_ADDR_FALSE, BRANCH_ADDR_MOVI);
}

void scheme_prepare_branch_jump(mz_jit_state *jitter, Branch_Info *for_branch)
{
  if (for_branch->non_tail) {
    /* Assumes that the runstack isn't going to be used until after the branch. */
    scheme_mz_flostack_restore(jitter, for_branch->flostack, for_branch->flostack_pos, 1, 0);

    if (for_branch->restore_depth) {
      int amt;
      amt = scheme_mz_compute_runstack_restored(jitter, 0, for_branch->restore_depth - 1);
      if (amt) {
        mz_rs_inc(amt);
      }
    }
  }

  mz_rs_sync();
}

static int branch_restore_is_empty(mz_jit_state *jitter, Branch_Info *for_branch)
{
  if (for_branch->non_tail) {
    if (for_branch->flostack != jitter->flostack_space)
      return 0;
    
    if (for_branch->restore_depth) {
      int amt;
      amt = scheme_mz_compute_runstack_restored(jitter, 0, for_branch->restore_depth - 1);
      if (amt)
        return 0;
    }
  }

  return 1;
}

static int finish_branch_with_true(mz_jit_state *jitter, Branch_Info *for_branch)
{
  scheme_prepare_branch_jump(jitter, for_branch);
  CHECK_LIMIT();

  if (for_branch->true_needs_jump) {
    GC_CAN_IGNORE jit_insn *ref;

    __START_SHORT_JUMPS__(for_branch->branch_short);
    ref = jit_jmpi(jit_forward());
    add_branch(for_branch, ref, BRANCH_ADDR_TRUE, BRANCH_ADDR_UCBRANCH);
    __END_SHORT_JUMPS__(for_branch->branch_short);
  }

  return 1;
}

static int finish_branch_with_false(mz_jit_state *jitter, Branch_Info *for_branch)
{
  GC_CAN_IGNORE jit_insn *ref;

  scheme_prepare_branch_jump(jitter, for_branch);
  CHECK_LIMIT();
  
  __START_SHORT_JUMPS__(for_branch->branch_short);
  ref = jit_jmpi(jit_forward());
  add_branch(for_branch, ref, BRANCH_ADDR_FALSE, BRANCH_ADDR_UCBRANCH);
  __END_SHORT_JUMPS__(for_branch->branch_short);

  return 1;
}

void scheme_branch_for_true(mz_jit_state *jitter, Branch_Info *for_branch)
/* Short-jump mode for move should be consistent with for_branch->branch_short */
{
  if (for_branch->true_needs_jump) {
    GC_CAN_IGNORE jit_insn *ref;

    ref = jit_jmpi(jit_forward());
    add_branch(for_branch, ref, BRANCH_ADDR_TRUE, BRANCH_ADDR_UCBRANCH);
  }
}

static int finish_branch(mz_jit_state *jitter, int target, Branch_Info *for_branch)
{
  GC_CAN_IGNORE jit_insn *ref;

  scheme_prepare_branch_jump(jitter, for_branch);
  CHECK_LIMIT();

  __START_SHORT_JUMPS__(for_branch->branch_short);

  ref = jit_beqi_p(jit_forward(), target, scheme_false);
  add_branch(for_branch, ref, BRANCH_ADDR_FALSE, BRANCH_ADDR_BRANCH);
  
  scheme_branch_for_true(jitter, for_branch);

  __END_SHORT_JUMPS__(for_branch->branch_short);

  return 1;
}

/*========================================================================*/
/*                           flonum boxing                                */
/*========================================================================*/

#ifdef USE_FLONUM_UNBOXING

int scheme_generate_flonum_local_boxing(mz_jit_state *jitter, int pos, int offset, int target, int extfl)
{
  GC_CAN_IGNORE jit_insn *ref;
  __START_TINY_JUMPS__(1);
  ref = jit_bnei_p(jit_forward(), target, NULL);
  __END_TINY_JUMPS__(1);
  CHECK_LIMIT();
  jit_movi_l(JIT_R0, offset);
  LOG_IT((" {box flonum}\n"));
  MZ_FPUSEL_STMT(extfl,
                 (void)jit_calli(sjc.box_extflonum_from_stack_code),
                 (void)jit_calli(sjc.box_flonum_from_stack_code));
  mz_rs_stxi(pos, JIT_R0);
  __START_TINY_JUMPS__(1);
  mz_patch_branch(ref);
  __END_TINY_JUMPS__(1);

  return 1;
}

static int generate_flonum_local_boxing(mz_jit_state *jitter, int pos, int local_pos, int target, int extfl)
{
  int offset;

  offset = scheme_mz_flostack_pos(jitter, local_pos);
  offset = JIT_FRAME_FLOSTACK_OFFSET - offset;
  if (jitter->unbox) {
    int fpr0 USED_ONLY_SOMETIMES;
    fpr0 = JIT_FPUSEL_FPR_0(extfl, jitter->unbox_depth);
    jit_FPSEL_ldxi_xd_fppush(extfl, fpr0, JIT_FP, offset);
    jitter->unbox_depth++;
  } else {
    mz_rs_sync();
    scheme_generate_flonum_local_boxing(jitter, pos, offset, target, extfl);
  }

  return 1;
}

int scheme_generate_flonum_local_unboxing(mz_jit_state *jitter, int push, int no_store, int extfl)
/* Move FPR0 onto C stack */
{
  int sz;
  int fpr0 USED_ONLY_SOMETIMES;

  sz = MZ_FPUSEL(extfl, 2 * sizeof(double), sizeof(double));

  if ((jitter->flostack_offset + sz) > jitter->flostack_space) {
    int space = FLOSTACK_SPACE_CHUNK;
    jitter->flostack_space += space;
    jit_subi_l(JIT_SP, JIT_SP, space);
  }
  jitter->flostack_offset += sz;

  if (push) mz_runstack_flonum_pushed(jitter, jitter->flostack_offset);
  CHECK_LIMIT();

  if (!no_store) {
    fpr0 = MZ_FPUSEL(extfl, JIT_FPU_FPR0, JIT_FPR0);
    mz_st_fppop(jitter->flostack_offset, fpr0, extfl);
  }

  return 1;
}

#endif

/*========================================================================*/
/*                           lambda codegen                               */
/*========================================================================*/

#ifdef JIT_PRECISE_GC
static Scheme_Object example_so = { scheme_native_closure_type, 0 };
#endif

static Scheme_Native_Lambda *create_native_lambda(Scheme_Lambda *lam, int clear_code_after_jit,
                                                  Scheme_Native_Lambda *case_lam);

static void ensure_closure_native(Scheme_Lambda *lam, 
				  Scheme_Native_Lambda *case_lam)
{
  if (!lam->u.native_code || SCHEME_FALSEP((Scheme_Object *)lam->u.native_code)) {
    Scheme_Native_Lambda *code;
    code = create_native_lambda(lam, 0, case_lam);
    lam->u.native_code = code;
  }
}

static int generate_closure(Scheme_Lambda *lam, 
			    mz_jit_state *jitter,
                            int immediately_filled)
{
  Scheme_Native_Lambda *code;

  ensure_closure_native(lam, NULL);
  code = lam->u.native_code;

#ifdef JIT_PRECISE_GC
  if (lam->closure_size < 100) {
    int sz;
    intptr_t init_word;
    sz = (sizeof(Scheme_Native_Closure)
          + ((lam->closure_size - mzFLEX_DELTA) * sizeof(Scheme_Object *)));
# ifdef CAN_INLINE_ALLOC
    if (immediately_filled) {
      /* Inlined alloc */
      scheme_inline_alloc(jitter, sz, scheme_native_closure_type, 0, 0, 0, 0,0 );
      CHECK_LIMIT();
      jit_addi_p(JIT_R0, JIT_V1, OBJHEAD_SIZE);
    } else
# endif
      {
        /* Non-inlined alloc */
        GC_CAN_IGNORE jit_insn *refr USED_ONLY_FOR_FUTURES;

        JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
  
        jit_movi_l(JIT_R0, sz);
        mz_prepare(1);
        jit_pusharg_l(JIT_R0);
        if (immediately_filled) {
          (void)mz_finish_lwe(ts_GC_malloc_one_small_dirty_tagged, refr);
        } else {
          (void)mz_finish_lwe(ts_GC_malloc_one_small_tagged, refr);
        }
        jit_retval(JIT_R0);
        memcpy(&init_word, &example_so, sizeof(intptr_t));
        jit_movi_l(JIT_R1, init_word);
        jit_str_l(JIT_R0, JIT_R1); 
      }
    scheme_mz_load_retained(jitter, JIT_R1, code);
    jit_stxi_p((intptr_t)&((Scheme_Native_Closure *)0x0)->code, JIT_R0, JIT_R1);

    return 1;
  }
#endif

  JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();

  mz_prepare(1);
  scheme_mz_load_retained(jitter, JIT_R0, code);
  jit_pusharg_p(JIT_R0);
  {
    GC_CAN_IGNORE jit_insn *refr USED_ONLY_FOR_FUTURES;
    (void)mz_finish_lwe(ts_scheme_make_native_closure, refr);
  }
  jit_retval(JIT_R0);

  return 1;
}

static int generate_closure_fill(Scheme_Lambda *lam, 
				 mz_jit_state *jitter)
{
  /* Fill in closure */
  int j, size, pos;
  mzshort *map;
  Scheme_Object *v;
  size = lam->closure_size;
  map = lam->closure_map;
  jit_addi_p(JIT_R2, JIT_R0, &((Scheme_Native_Closure *)0x0)->vals);
  for (j = 0; j < size; j++) {
    CHECK_LIMIT();

    if (SCHEME_NATIVE_LAMBDA_FLAGS(jitter->nc->code) & NATIVE_SPECIALIZED)
      v = extract_closure_local(map[j], jitter, 1);
    else
      v = NULL;

    if (v) {
      /* capture value directly within specialized */
      scheme_mz_load_retained(jitter, JIT_R1, v);
    } else {
      pos = mz_remap(map[j]);
      jit_ldxi_p(JIT_R1, JIT_RUNSTACK, WORDS_TO_BYTES(pos));
    }
    jit_stxi_p(WORDS_TO_BYTES(j), JIT_R2, JIT_R1);
  }
  return 1;
}

static int generate_closure_prep(Scheme_Lambda *lam, mz_jit_state *jitter)
{
  int retval = 0;
#ifdef USE_FLONUM_UNBOXING
  /* Ensure that flonums are boxed */
  int j, size, pos;
  mzshort *map;

  if (SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_HAS_TYPED_ARGS) {
    size = lam->closure_size;
    map = lam->closure_map;
    for (j = 0; j < size; j++) {
      if (CLOSURE_CONTENT_IS_FLONUM(lam, j)
          || CLOSURE_CONTENT_IS_EXTFLONUM(lam, j)) {
        int extfl;
        extfl = CLOSURE_CONTENT_IS_EXTFLONUM(lam, j);
        pos = mz_remap(map[j]);
        jit_ldxi_p(JIT_R1, JIT_RUNSTACK, WORDS_TO_BYTES(pos));
        generate_flonum_local_boxing(jitter, pos, map[j], JIT_R1, extfl);
        CHECK_LIMIT();
        retval = 1;
      }
    }
  }
#endif

  return retval;
}

static Scheme_Native_Lambda *create_native_case_lambda(Scheme_Case_Lambda *c)
{
  Scheme_Lambda *lam;
  Scheme_Native_Lambda *nlam;
  Scheme_Object *name, *o;
  int max_let_depth = 0, i, count, is_method = 0;

  nlam = MALLOC_ONE_RT(Scheme_Native_Lambda);
#ifdef MZTAG_REQUIRED
  nlam->iso.so.type = scheme_rt_native_code;
#endif
  name = c->name;
  if (name && SCHEME_BOXP(name)) {
    name = SCHEME_BOX_VAL(name);
    is_method = 1;
  }
  nlam->u2.name = name;
  count = c->count;
  for (i = 0; i < count; i++) {
    o = c->array[i];
    if (SCHEME_PROCP(o))
      o = (Scheme_Object *)((Scheme_Closure *)o)->code;
    lam = MALLOC_ONE_TAGGED(Scheme_Lambda);
    memcpy(lam, o, sizeof(Scheme_Lambda));
    ensure_closure_native(lam, nlam);
    if (lam->u.native_code->max_let_depth > max_let_depth)
      max_let_depth = lam->u.native_code->max_let_depth;
    c->array[i] = (Scheme_Object *)lam;
  }
  nlam->max_let_depth = max_let_depth;
  nlam->closure_size = -(count + 1); /* Indicates case-lambda */

  if (count) {
    o = c->array[0];
    if (SCHEME_PROCP(o))
      o = (Scheme_Object *)((Scheme_Closure *)o)->code;
    lam = (Scheme_Lambda *)o;
    is_method = ((SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_IS_METHOD) ? 1 : 0);
  }

  generate_case_lambda(c, nlam, is_method);

  return nlam;
}

Scheme_Native_Lambda *scheme_generate_case_lambda(Scheme_Case_Lambda *c)
{
  Scheme_Native_Lambda *nlam;

  nlam = create_native_case_lambda(c);

  return nlam;
}

static void ensure_case_closure_native(Scheme_Case_Lambda *c)
{
  if (!c->native_code || SCHEME_FALSEP((Scheme_Object *)c->native_code)) {
    Scheme_Native_Lambda *nlam;
    nlam = create_native_case_lambda(c);
    c->native_code = nlam;
  }
}

static int generate_case_closure(Scheme_Object *obj, mz_jit_state *jitter, int target)
/* de-sync's */
{
  Scheme_Case_Lambda *c = (Scheme_Case_Lambda *)obj;
  Scheme_Native_Lambda *nlam;
  Scheme_Lambda *lam;
  Scheme_Object *o;
  int i, offset, count;

  ensure_case_closure_native(c);
  nlam = c->native_code;

  count = c->count;

  for (i = 0; i < count; i++) {
    o = c->array[i];
    if (SCHEME_PROCP(o))
      o = (Scheme_Object *)((Scheme_Closure *)o)->code;
    lam = (Scheme_Lambda *)o;
    mz_rs_sync();
    CHECK_LIMIT();
    generate_closure_prep(lam, jitter);
    CHECK_LIMIT();
  }

  mz_rs_sync();

  JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
  mz_prepare(1);
  scheme_mz_load_retained(jitter, JIT_R0, nlam);
  jit_pusharg_p(JIT_R0);
  {
    GC_CAN_IGNORE jit_insn *refr USED_ONLY_FOR_FUTURES;
    (void)mz_finish_lwe(ts_scheme_make_native_case_closure, refr);
  }
  jit_retval(JIT_R1);
  CHECK_LIMIT();

  for (i = 0; i < count; i++) {
    o = c->array[i];
    if (SCHEME_PROCP(o))
      o = (Scheme_Object *)((Scheme_Closure *)o)->code;
    lam = (Scheme_Lambda *)o;
    mz_pushr_p(JIT_R1);
    mz_rs_sync();
    generate_closure(lam, jitter, 1);
    CHECK_LIMIT();
    generate_closure_fill(lam, jitter);
    CHECK_LIMIT();
    mz_popr_p(JIT_R1);
    offset = WORDS_TO_BYTES(i) + (uintptr_t)&((Scheme_Native_Closure *)0x0)->vals;
    jit_stxi_p(offset, JIT_R1, JIT_R0);
    CHECK_LIMIT();
  }
  jit_movr_p(target, JIT_R1);
  
  return 1;
}

/*========================================================================*/
/*                          non-tail codegen                              */
/*========================================================================*/

int scheme_generate_non_tail_mark_pos_prefix(mz_jit_state *jitter)
{
  /* dsync'd ok.
     This part of a non-tail setup can be done once for a sequence
     of non-tail calls. In that case, pass 0 for the `mark_pos_ends'
     argument to generate_non_tail(), so that it can skip this prefix
     and suffix. In case this prefix needs to adjust the runstack,
     the result indicates the number of pushed values. */
  mz_tl_ldi_l(JIT_R2, tl_scheme_current_cont_mark_pos);
  jit_addi_l(JIT_R2, JIT_R2, 2);
  mz_tl_sti_l(tl_scheme_current_cont_mark_pos, JIT_R2, JIT_R0);
  return 0 /* = number of pushed items */;
}

void scheme_generate_non_tail_mark_pos_suffix(mz_jit_state *jitter)
/* dsync'd ok */
{
  mz_tl_ldi_l(JIT_R2, tl_scheme_current_cont_mark_pos);
  jit_subi_l(JIT_R2, JIT_R2, 2);
  mz_tl_sti_l(tl_scheme_current_cont_mark_pos, JIT_R2, JIT_R0);
}

static int generate_non_tail_with_branch_and_values(Scheme_Object *obj, mz_jit_state *jitter,
                                                    int multi_ok, int mark_pos_ends, int ignored,
                                                    Branch_Info *for_branch,
                                                    Expected_Values_Info *for_values)
/* de-sync's rs */
{
  int flostack, flostack_pos;

  if (scheme_is_simple(obj, INIT_SIMPLE_DEPTH, 0, jitter, 0)) {
    /* Simple; doesn't change the stack or set marks: */
    int v;
    FOR_LOG(jitter->log_depth++);
    flostack = scheme_mz_flostack_save(jitter, &flostack_pos);

    if (for_branch) {
      for_branch->non_tail = 1;
      for_branch->restore_depth = 0;
      for_branch->flostack = flostack;
      for_branch->flostack_pos = flostack_pos;
    }
    v = scheme_generate(obj, jitter, 0, 0, multi_ok, ignored ? -1 : JIT_R0, for_branch, for_values);
    CHECK_LIMIT();
    scheme_mz_flostack_restore(jitter, flostack, flostack_pos, !for_branch, 1);
    FOR_LOG(--jitter->log_depth);
    /* mz_SET_REG_STATUS_VALID(0); --- not needed, since stack doesn't change */
    return v;
  }

  {
    int amt, need_ends = 1, using_local1 = 0, save_pushed_marks;
    START_JIT_DATA();
    
    save_pushed_marks = jitter->pushed_marks;

    /* Might change the stack or marks: */
    if (scheme_is_simple(obj, INIT_SIMPLE_DEPTH, 1, jitter, 0)) {
      need_ends = 0;
    } else {
      LOG_IT(("non-tail\n"));
      if (mark_pos_ends)
	scheme_generate_non_tail_mark_pos_prefix(jitter);
      CHECK_LIMIT();
      if (!jitter->local1_busy) {
        mz_tl_ldi_p(JIT_R2, tl_scheme_current_cont_mark_stack);
        using_local1 = 1;
        jitter->local1_busy = save_pushed_marks + 1;
        mz_set_local_p(JIT_R2, JIT_LOCAL1);
      } else if ((save_pushed_marks + 1) == jitter->local1_busy) {
        /* value in LOCAL1 works here, too, because no marks
           have been pushed */
        using_local1 = 2;
      } else {
        mz_tl_ldi_p(JIT_R2, tl_scheme_current_cont_mark_stack);
        /* mark stack is an integer... turn it into a pointer */
        jit_fixnum_l(JIT_R2, JIT_R2);
        mz_pushr_p(JIT_R2); /* no sync */
# ifdef MZ_USE_LWC
        /* For lighweight continuations, we need to be able to recognize
           and adjust mark-stack depths: */
        jit_movi_p(JIT_R2, SCHEME_EVAL_WAITING);
        mz_pushr_p(JIT_R2); /* no sync */
# endif
      }
      CHECK_LIMIT();
    }
    scheme_mz_runstack_saved(jitter);
    flostack = scheme_mz_flostack_save(jitter, &flostack_pos);
    CHECK_LIMIT();

    if (for_branch) {
      if (need_ends) {
        for_branch->include_slow = 1;
        for_branch = NULL;
      } else {
        for_branch->non_tail = 1;
        for_branch->restore_depth = 1;
        for_branch->flostack = flostack;
        for_branch->flostack_pos = flostack_pos;
      }
    }
    
    PAUSE_JIT_DATA();
    FOR_LOG(jitter->log_depth++);

    scheme_generate(obj, jitter, 0, 0, multi_ok, ignored ? -1 : JIT_R0, for_branch, for_values); /* no sync */

    FOR_LOG(--jitter->log_depth);
    RESUME_JIT_DATA();
    CHECK_LIMIT();

    scheme_mz_flostack_restore(jitter, flostack, flostack_pos, !for_branch, 1);
    amt = scheme_mz_runstack_restored(jitter);
    if (amt && !for_branch) {
      mz_rs_inc(amt);
    }
    if (need_ends) {
      if (using_local1) {
        mz_get_local_p(JIT_R2, JIT_LOCAL1);
        if (using_local1 == 1)
          jitter->local1_busy = 0;
      } else {
# ifdef MZ_USE_LWC
        mz_popr_p(JIT_R2); /* no sync */
# endif
        mz_popr_p(JIT_R2); /* no sync */
        jit_rshi_l(JIT_R2, JIT_R2, 0x1); /* pointer back to integer */
      }
      mz_tl_sti_p(tl_scheme_current_cont_mark_stack, JIT_R2, JIT_R0);
      if (mark_pos_ends)
	scheme_generate_non_tail_mark_pos_suffix(jitter);
      CHECK_LIMIT();
    }

    jitter->pushed_marks = save_pushed_marks;
    mz_SET_REG_STATUS_VALID(0);

    END_JIT_DATA(21);
  }
    
  return 1;
}

int scheme_generate_non_tail(Scheme_Object *obj, mz_jit_state *jitter, 
                             int multi_ok, int mark_pos_ends, int ignored)
{
  return generate_non_tail_with_branch_and_values(obj, jitter, multi_ok, mark_pos_ends, ignored, NULL, NULL);
}

int scheme_generate_non_tail_for_values(Scheme_Object *obj, mz_jit_state *jitter,
                                        int multi_ok, int mark_pos_ends, int ignored,
                                        Expected_Values_Info *for_values)
{
  return generate_non_tail_with_branch_and_values(obj, jitter, multi_ok, mark_pos_ends, ignored, NULL, for_values);
}

int scheme_generate_unboxed(Scheme_Object *obj, mz_jit_state *jitter, int inlined_ok, int unbox_anyway)
/* de-sync's;
   inlined_ok == 2 => can generate directly; inlined_ok == 1 => non-tail unbox */
{
  mz_jit_unbox_state ubs;

  if (inlined_ok) {
    if (inlined_ok == 2)
      return scheme_generate(obj, jitter, 0, 0, 1, JIT_R0, NULL, NULL);
    else
      return scheme_generate_non_tail(obj, jitter, 0, 1, 0);
  } else if (unbox_anyway && SAME_TYPE(SCHEME_TYPE(obj), scheme_local_type)) {
    /* local unboxing can be handled in generate(), and 
       we want to handle it there to avoid unnecessary (and potentially
       harmful) clearing of the runstack location */
    return scheme_generate(obj, jitter, 0, 0, 1, JIT_R0, NULL, NULL);
  }

  if (!jitter->unbox || jitter->unbox_depth)
    scheme_signal_error("internal error: bad unboxing mode or depth");
  
  /* It probably would be useful to special-case a let-one
     sequence down to something that can be unboxed. */

  scheme_mz_unbox_save(jitter, &ubs);

  scheme_generate_non_tail(obj, jitter, 0, 1, 0);
  CHECK_LIMIT();

  scheme_mz_unbox_restore(jitter, &ubs);

  if (inlined_ok || unbox_anyway) {
    /* Move result into floating-point register: */
    scheme_generate_unboxing(jitter, JIT_R0);
  }

  return 1;
}

/*========================================================================*/
/*                          expression codegen                            */
/*========================================================================*/

static Scheme_Object *generate_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *obj = (Scheme_Object *)p->ku.k.p1;
  mz_jit_state *jitter = (mz_jit_state *)p->ku.k.p2;
  Branch_Info *for_branch = (Branch_Info *)p->ku.k.p3, for_branch_copy;
  Branch_Info_Addr *for_branch_addrs = (Branch_Info_Addr *)p->ku.k.p4;
  Expected_Values_Info *for_values = (Expected_Values_Info *)p->ku.k.p5;
  int v;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;
  p->ku.k.p5 = NULL;

  if (for_branch) {
    memcpy(&for_branch_copy, for_branch, sizeof(Branch_Info));
    for_branch_copy.addrs = for_branch_addrs;
  }

  v = scheme_generate(obj, jitter, p->ku.k.i1, p->ku.k.i4, p->ku.k.i2, p->ku.k.i3, 
                      (for_branch ? &for_branch_copy : NULL),
                      for_values);

  if (for_branch) {
    memcpy(for_branch, &for_branch_copy, sizeof(Branch_Info));
    return scheme_make_raw_pair(scheme_make_integer(v), (Scheme_Object *)for_branch->addrs);
  } else
    return scheme_make_integer(v);
}

#define NUM_QUICK_INFO_ADDRS 6

static int generate_branch(Scheme_Object *obj, mz_jit_state *jitter, int is_tail, int wcm_may_replace,
                           int multi_ok, int orig_target, int result_ignored, Branch_Info *for_branch)
{
  Scheme_Branch_Rec *branch = (Scheme_Branch_Rec *)obj;
  Branch_Info for_this_branch;
  GC_CAN_IGNORE Branch_Info_Addr addrs[NUM_QUICK_INFO_ADDRS];
  GC_CAN_IGNORE jit_insn *ref2;
  mz_jit_unbox_state ubs;
  int ubd, save_ubd;
  int pushed_marks;
  int nsrs, nsrs1, g1, g2, amt, flostack, flostack_pos;
  int else_is_empty = 0, i, can_chain_branch, chain_true, chain_false, old_self_pos;
#ifdef NEED_LONG_BRANCHES
  int then_short_ok, else_short_ok;
#else
  int then_short_ok = 1;
# ifdef NEED_LONG_JUMPS
  int else_short_ok = 1;
# endif
#endif
  START_JIT_DATA();

#ifdef NEED_LONG_BRANCHES
  /* It's possible that the code for a then
     or else branch will be so large that we might
     need a long jump. Conservatively analyze the
     `then' and `else' expressions. The `the' short
     measurement includes the test because we might
     jump out from a nested conditional. */
  then_short_ok = ((is_short(branch->tbranch, 32) > 0)
                   && (is_short(branch->test, 32) > 0));
  else_short_ok = (is_short(branch->fbranch, 32) > 0);
#endif

  old_self_pos = jitter->self_pos;

  for_this_branch.addrs = addrs;
  for_this_branch.addrs_size = NUM_QUICK_INFO_ADDRS;
  for_this_branch.addrs_count = 0;
  for_this_branch.true_needs_jump = 0;
  for_this_branch.non_tail = 0; /* generate_non_tail_with_branch() adjusts this */
  for_this_branch.include_slow = 0; /* and maybe this, too */

  if (for_branch && branch_restore_is_empty(jitter, for_branch))
    can_chain_branch = 1;
  else
    can_chain_branch = 0;
  chain_true = (SCHEME_TYPE(branch->tbranch) > _scheme_ir_values_types_);
  chain_false = (SCHEME_TYPE(branch->fbranch) > _scheme_ir_values_types_);

  if (can_chain_branch && chain_true)
    for_this_branch.true_needs_jump = 1;
#ifdef NEED_LONG_BRANCHES
  if (can_chain_branch && (chain_true || chain_false)
      && !for_branch->branch_short)
    then_short_ok = 0;
#endif
  for_this_branch.branch_short = then_short_ok;
  
  LOG_IT(("if...\n"));

  if (result_ignored 
      && (SCHEME_TYPE(branch->fbranch) > _scheme_ir_values_types_))
    else_is_empty = 1;
  else if (can_chain_branch && chain_false)
    else_is_empty = 1;

  mz_rs_sync();

  scheme_mz_unbox_save(jitter, &ubs);

  if (!scheme_generate_inlined_test(jitter, branch->test, then_short_ok, &for_this_branch)) {
    CHECK_LIMIT();
    generate_non_tail_with_branch_and_values(branch->test, jitter, 0, 1, 0, &for_this_branch, NULL);
    CHECK_LIMIT();
    if (for_this_branch.include_slow) {
      finish_branch(jitter, JIT_R0, &for_this_branch);
    }
  }
  CHECK_LIMIT();

  if (old_self_pos != jitter->self_pos)
    scheme_signal_error("internal error: self position moved across test");

  save_ubd = jitter->unbox_depth;
  scheme_mz_unbox_restore(jitter, &ubs);

  /* True branch */
  scheme_mz_runstack_saved(jitter);
  flostack = scheme_mz_flostack_save(jitter, &flostack_pos);
  nsrs = jitter->need_set_rs;
  pushed_marks = jitter->pushed_marks;
  PAUSE_JIT_DATA();
  LOG_IT(("...then...\n"));
  FOR_LOG(++jitter->log_depth);
  __START_SHORT_JUMPS__(then_short_ok);
  for (i = for_this_branch.addrs_count; i--; ) {
    if (for_this_branch.addrs[i].mode == BRANCH_ADDR_TRUE) {
      if (can_chain_branch && chain_true)
        add_branch(for_branch, 
                   for_this_branch.addrs[i].addr,
                   SCHEME_FALSEP(branch->tbranch) ? BRANCH_ADDR_FALSE : BRANCH_ADDR_TRUE,
                   for_this_branch.addrs[i].kind);
      else
        patch_branch_addr(jitter, for_this_branch.addrs, i);
      CHECK_LIMIT();
    }
  }
  __END_SHORT_JUMPS__(then_short_ok);
  if (!(can_chain_branch && chain_true)) {
    if (for_branch) {
      for_branch->true_needs_jump++;
      for_branch->restore_depth++;
    }
    g1 = scheme_generate(branch->tbranch, jitter, is_tail, wcm_may_replace, multi_ok, 
                         orig_target, for_branch, NULL);
    if (for_branch) {
      --for_branch->true_needs_jump;
      --for_branch->restore_depth;
    }
  } else
    g1 = 1;
  RESUME_JIT_DATA();
  CHECK_LIMIT();
  amt = scheme_mz_runstack_restored(jitter);
  scheme_mz_flostack_restore(jitter, flostack, flostack_pos, (g1 != 2) && !for_branch, 1);
  if ((g1 != 2) && !for_branch) {
    if (!is_tail) {
      if (amt)
        mz_rs_inc(amt);
      mz_rs_sync();
    }
    __START_SHORT_JUMPS__(else_short_ok);
    if (else_is_empty)
      ref2 = NULL;
    else
      ref2 = jit_jmpi(jit_forward());
    __END_SHORT_JUMPS__(else_short_ok);
    nsrs1 = jitter->need_set_rs;
  } else {
    ref2 = 0;
    if (g1 == 2) {
      nsrs1 = 0;
    } else {
      nsrs1 = jitter->need_set_rs;
    }
  }
  jitter->need_set_rs = nsrs;
  jitter->pushed_marks = pushed_marks;
  mz_rs_sync_0();

  if (old_self_pos != jitter->self_pos)
    scheme_signal_error("internal error: self position moved across branch");

  ubd = jitter->unbox_depth;
  jitter->unbox_depth = save_ubd;
  scheme_mz_unbox_restore(jitter, &ubs);

  /* False branch */
  mz_SET_REG_STATUS_VALID(0);
  scheme_mz_runstack_saved(jitter);
  flostack = scheme_mz_flostack_save(jitter, &flostack_pos);
  __START_SHORT_JUMPS__(then_short_ok);
  for (i = for_this_branch.addrs_count; i--; ) {
    if (for_this_branch.addrs[i].mode == BRANCH_ADDR_FALSE) {
      if (can_chain_branch && chain_false)
        add_branch(for_branch, 
                   for_this_branch.addrs[i].addr,
                   SCHEME_FALSEP(branch->fbranch) ? BRANCH_ADDR_FALSE : BRANCH_ADDR_TRUE,
                   for_this_branch.addrs[i].kind);
      else
        patch_branch_addr(jitter, for_this_branch.addrs, i);
      CHECK_LIMIT();
    }
  }
  __END_SHORT_JUMPS__(then_short_ok);
  PAUSE_JIT_DATA();
  FOR_LOG(jitter->log_depth--);
  LOG_IT(("...else\n"));
  FOR_LOG(++jitter->log_depth);
  if (!(can_chain_branch && chain_false)) {
    if (for_branch) {
      for_branch->restore_depth++;
    }
    g2 = scheme_generate(branch->fbranch, jitter, is_tail, wcm_may_replace, multi_ok, 
                         orig_target, for_branch, NULL);
    if (for_branch) {
      --for_branch->restore_depth;
    }
  } else
    g2 = 1;
  RESUME_JIT_DATA();
  CHECK_LIMIT();
  amt = scheme_mz_runstack_restored(jitter);
  scheme_mz_flostack_restore(jitter, flostack, flostack_pos, (g2 != 2) && !for_branch, 1);
  if ((g2 != 2) && !for_branch) {
    if (!is_tail) {
      if (amt)
        mz_rs_inc(amt);
      mz_rs_sync();
    }
  } else if (g2 == 2) {
    jitter->need_set_rs = 0;
  }
  if ((g1 != 2) && !for_branch) {
    __START_SHORT_JUMPS__(else_short_ok);
    if (!else_is_empty) {
      mz_patch_ucbranch(ref2);
    }
    __END_SHORT_JUMPS__(else_short_ok);
  }
  FOR_LOG(jitter->log_depth--);

  END_JIT_DATA(12);

  if (ubd != jitter->unbox_depth)
    scheme_signal_error("internal error: different unbox depth for branches");

  /* Return result */

  if ((g1 == 2) && (g2 == 2))
    return 2;

  if (nsrs1)
    jitter->need_set_rs = 1;

  mz_SET_REG_STATUS_VALID(0);

  return 1;
}

int scheme_generate(Scheme_Object *obj, mz_jit_state *jitter, int is_tail, int wcm_may_replace, int multi_ok, int target,
                    Branch_Info *for_branch, Expected_Values_Info *for_values)
/* de-sync's; result goes to target */
{
  Scheme_Type type;
  int result_ignored, orig_target;

#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Object *ok;
    Scheme_Thread *p = scheme_current_thread;
    mz_jit_state *jitter_copy;
    Branch_Info *for_branch_copy;
    Branch_Info_Addr *addrs;
    Expected_Values_Info *for_values_copy;
    int *copy_mappings;

    copy_mappings = (int *)scheme_malloc_atomic(jitter->mappings_size * sizeof(int));
    memcpy(copy_mappings, jitter->mappings, jitter->mappings_size * sizeof(int));
    jitter->mappings = copy_mappings;

    jitter_copy = scheme_clone_jitter(jitter);
    if (for_branch) {
      for_branch_copy = scheme_malloc_atomic(sizeof(Branch_Info));
      memcpy(for_branch_copy, for_branch, sizeof(Branch_Info));
      addrs = scheme_malloc_atomic(sizeof(Branch_Info_Addr) * for_branch->addrs_size);
      memcpy(addrs, for_branch->addrs, sizeof(Branch_Info_Addr) * for_branch->addrs_count);
    } else {
      for_branch_copy = NULL;
      addrs = NULL;
    }
    if (for_values) {
      for_values_copy = (Expected_Values_Info *)scheme_malloc_atomic(sizeof(Expected_Values_Info));
      memcpy(for_values_copy, for_values, sizeof(Expected_Values_Info));
    } else
      for_values_copy = NULL;

    p->ku.k.p1 = (void *)obj;
    p->ku.k.p2 = (void *)jitter_copy;
    p->ku.k.i1 = is_tail;
    p->ku.k.i4 = wcm_may_replace;
    p->ku.k.i2 = multi_ok;
    p->ku.k.i3 = target;
    p->ku.k.p3 = (void *)for_branch_copy;
    p->ku.k.p4 = (void *)addrs;
    p->ku.k.p5 = (void *)for_values_copy;

    ok = scheme_handle_stack_overflow(generate_k);

    scheme_unclone_jitter(jitter, jitter_copy);

    if (for_branch) {
      memcpy(for_branch, for_branch_copy, sizeof(Branch_Info));
      for_branch->addrs = (Branch_Info_Addr *)SCHEME_CDR(ok);
      ok = SCHEME_CAR(ok);
    }
    if (for_values) {
      memcpy(for_values, for_values_copy, sizeof(Expected_Values_Info));
    }

    return SCHEME_INT_VAL(ok);
  }
#endif

#ifdef TEST_ALTERNATE_TARGET_REGISTER
# define IS_SKIP_TYPE(t) 0
  if ((target == JIT_R0)
      && !is_tail
      && !for_branch
      && !for_values
      && !jitter->unbox
      && !IS_SKIP_TYPE(SCHEME_TYPE(obj))
      && !SAME_TYPE(SCHEME_TYPE(obj), scheme_toplevel_type)
      && !SAME_TYPE(SCHEME_TYPE(obj), scheme_local_type)
      && !SAME_TYPE(SCHEME_TYPE(obj), scheme_local_unbox_type)
      && (SCHEME_TYPE(obj) < _scheme_values_types_)) {
    scheme_generate(obj, jitter, is_tail, wcm_may_replace, multi_ok, JIT_R1, for_branch, NULL);
    CHECK_LIMIT();
    jit_movr_p(JIT_R0, JIT_R1);
    return 1;
  } else if ((target == JIT_R1)
             && IS_SKIP_TYPE(SCHEME_TYPE(obj))) {
    scheme_generate(obj, jitter, is_tail, wcm_may_replace, multi_ok, JIT_R0, for_branch, NULL);
    CHECK_LIMIT();
    jit_movr_p(JIT_R1, JIT_R0);
    return 1;
  }
#endif

  obj = scheme_specialize_to_constant(obj, jitter, 0);

  orig_target = target;
  result_ignored = (target < 0);
  if (target < 0) target = JIT_R0;

  if (for_branch) {
    mz_rs_sync();
    if (scheme_generate_inlined_test(jitter, obj, for_branch->branch_short, for_branch))
      return 1;
    CHECK_LIMIT();
  }

  type = SCHEME_TYPE(obj);
  switch (type) {
  case scheme_toplevel_type:
    {
      int can_fail;
      /* Other parts of the JIT rely on this code not modifying R1 */
      can_fail = ((SCHEME_TOPLEVEL_FLAGS(obj) & SCHEME_TOPLEVEL_FLAGS_MASK) < SCHEME_TOPLEVEL_READY);
      if (!can_fail && result_ignored) {
        /* skip */
      } else {
        int pos;
        START_JIT_DATA();
        LOG_IT(("top-level\n"));
        mz_rs_sync_fail_branch();
        if (SCHEME_NATIVE_LAMBDA_FLAGS(jitter->nc->code) & NATIVE_SPECIALIZED) {
          /* Must be a top-level that is not yet defined. */
          Scheme_Object *b;
          mz_rs_sync_fail_branch();
          b = scheme_extract_global(obj, jitter->nc, 0);
          scheme_mz_load_retained(jitter, JIT_R2, b);
        } else {
          /* Load global array: */
          pos = mz_remap(SCHEME_TOPLEVEL_DEPTH(obj));
          mz_rs_ldxi(JIT_R2, pos);
          /* Load bucket: */
          pos = SCHEME_TOPLEVEL_POS(obj);
          jit_ldxi_p(JIT_R2, JIT_R2, &(((Scheme_Prefix *)0x0)->a[pos]));
        }
        /* Extract bucket value */
        jit_ldxi_p(target, JIT_R2, &(SCHEME_VAR_BUCKET(0x0)->val));
        CHECK_LIMIT();
        if (can_fail) {
          /* Is it NULL? */
          scheme_generate_pop_unboxed(jitter);
          CHECK_LIMIT();
          (void)jit_beqi_p(sjc.unbound_global_code, target, 0);
        }
        if (jitter->unbox) scheme_generate_unboxing(jitter, target);
        END_JIT_DATA(0);
      }
      if (for_branch) finish_branch(jitter, target, for_branch);
      return 1;
    }
  case scheme_local_type:
    {
      /* Other parts of the JIT rely on this code modifying only the target register,
         unless the type is SCHEME_FLONUM_TYPE */
      int pos, flonum;
      int extfl USED_ONLY_IF_FLONUM_UNBOXING;
      START_JIT_DATA();
#ifdef USE_FLONUM_UNBOXING
      flonum = (SCHEME_GET_LOCAL_TYPE(obj) == SCHEME_LOCAL_TYPE_FLONUM);
      if (MZ_LONG_DOUBLE_AND(SCHEME_GET_LOCAL_TYPE(obj) == SCHEME_LOCAL_TYPE_EXTFLONUM))
        flonum = extfl = 1;
      else
        extfl = 0;
#else
      flonum = 0;
      extfl = 0;
#endif
      pos = mz_remap(SCHEME_LOCAL_POS(obj));
      LOG_IT(("local %d [%d]\n", pos, SCHEME_LOCAL_FLAGS(obj)));
      if (!result_ignored && (!flonum || !jitter->unbox)) {
        int valid, old_r0 = -1, old_r1 = -1;

        if (mz_CURRENT_REG_STATUS_VALID()) {
          valid = 1;
          old_r0 = jitter->r0_status;
          old_r1 = jitter->r1_status;
        } else
          valid = 0;

        if (pos == old_r0) {
          if (target != JIT_R0) {
            jit_movr_p(target, JIT_R0);
            if (target == JIT_R1)
              jitter->r1_status = pos;
            mz_SET_REG_STATUS_VALID(1);
          }
        } else if (pos == old_r1) {
          if (target != JIT_R1) {
            jit_movr_p(target, JIT_R1);
            if (target == JIT_R0)
              jitter->r0_status = pos;
            mz_SET_REG_STATUS_VALID(1);
          }
        } else {
          mz_rs_ldxi(target, pos);
          VALIDATE_RESULT(target);
          if (target == JIT_R0) {
            jitter->r0_status = pos;
            jitter->r1_status = old_r1;
            mz_SET_REG_STATUS_VALID(1);
          } else if (target == JIT_R1) {
            jitter->r1_status = pos;
            jitter->r0_status = old_r0;
            mz_SET_REG_STATUS_VALID(1);
          } else if (valid) {
            /* R0 and R1 are unchanged */
            mz_SET_REG_STATUS_VALID(1);
          }
        }
      }
      if (SCHEME_GET_LOCAL_FLAGS(obj) == SCHEME_LOCAL_CLEAR_ON_READ) {
        if (!flonum && !jitter->unbox)
          mz_rs_stxi(pos, JIT_RUNSTACK);
        else {
          /* Don't clear the box. It's not important for space safety, because
             it's just a flonum. More importantly, argument setup for a self-call
             with an unboxed argument wants to keep the box. */
        }
      }
      CHECK_LIMIT();
      if (flonum && !result_ignored) {
#ifdef USE_FLONUM_UNBOXING
        generate_flonum_local_boxing(jitter, pos, SCHEME_LOCAL_POS(obj), target, extfl);
        CHECK_LIMIT();
#endif
      } else {
        if (jitter->unbox) scheme_generate_unboxing(jitter, target);
      }
      if (for_branch) finish_branch(jitter, target, for_branch);
      END_JIT_DATA(2);
      return 1;
    }
  case scheme_local_unbox_type:
    {
      int pos;
      Scheme_Object *specialized = NULL;
      START_JIT_DATA();
      LOG_IT(("unbox local\n"));

      if (SCHEME_NATIVE_LAMBDA_FLAGS(jitter->nc->code) & NATIVE_SPECIALIZED)
        specialized = scheme_extract_closure_local(obj, jitter, 0, 1);

      pos = mz_remap(SCHEME_LOCAL_POS(obj));
      if (!result_ignored) {
        if (specialized)
          scheme_mz_load_retained(jitter, JIT_R0, specialized);
        else
          mz_rs_ldxi(JIT_R0, pos);
        jit_ldr_p(target, JIT_R0);
      }
      if ((SCHEME_GET_LOCAL_FLAGS(obj) == SCHEME_LOCAL_CLEAR_ON_READ)
          && !specialized) {
        LOG_IT(("clear-on-read\n"));
        mz_rs_stxi(pos, JIT_RUNSTACK);
      }
      VALIDATE_RESULT(target);
      CHECK_LIMIT();
      if (jitter->unbox) scheme_generate_unboxing(jitter, target);
      if (for_branch) finish_branch(jitter, target, for_branch);
      
      END_JIT_DATA(3);
      return 1;
    }
  case scheme_case_lambda_sequence_type:
    {
      START_JIT_DATA();
      LOG_IT(("case-lambda\n"));
      /* case-lambda */
      if (for_branch) 
        finish_branch_with_true(jitter, for_branch);
      else
        generate_case_closure(obj, jitter, target);
      END_JIT_DATA(5);
      return 1;
    } 
    break;
  case scheme_begin0_sequence_type:
    {
      Scheme_Sequence *seq;
      GC_CAN_IGNORE jit_insn *ref, *ref2;
      int i;
      START_JIT_DATA();

      LOG_IT(("begin0\n"));

      seq = (Scheme_Sequence *)obj;
	
      /* Evaluate first expression: */
      scheme_generate_non_tail(seq->array[0], jitter, multi_ok, 1, result_ignored);
      CHECK_LIMIT();

      /* Save value(s) */
      if (!result_ignored) {
        mz_pushr_p(JIT_R0);
        if (multi_ok) {
          mz_pushr_p(JIT_R0);
          mz_pushr_p(JIT_R0);
          mz_pushr_p(JIT_R0);
          mz_rs_sync();
          __START_SHORT_JUMPS__(1);
          ref = jit_bnei_p(jit_forward(), JIT_R0, SCHEME_MULTIPLE_VALUES);
          CHECK_LIMIT();
          /* Save away multiple values */
          mz_popr_p(JIT_V1); /* sync'd below... */
          mz_popr_p(JIT_V1);
          mz_popr_p(JIT_V1);
          mz_tl_ldi_p(JIT_R0, tl_scheme_current_thread);
          CHECK_LIMIT();
          jit_ldxi_l(JIT_V1, JIT_R0, &((Scheme_Thread *)0x0)->ku.multiple.count);
          jit_fixnum_l(JIT_V1, JIT_V1);
          mz_pushr_p(JIT_V1); /* sync'd below */
          jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Thread *)0x0)->ku.multiple.array);
          mz_pushr_p(JIT_V1); /* sync'd below */
          CHECK_LIMIT();
          (void)jit_movi_p(JIT_R1, 0x0);
          mz_pushr_p(JIT_R1); /* pushing 0 indicates that multi-array follows */
          /* If multi-value array is values buffer, zero out values buffer */
          jit_ldxi_p(JIT_R2, JIT_R0, &((Scheme_Thread *)0x0)->values_buffer);
          mz_rs_sync();
          ref2 = jit_bner_p(jit_forward(), JIT_V1, JIT_R2);
          jit_stxi_p(&((Scheme_Thread *)0x0)->values_buffer, JIT_R0, JIT_R1);
          CHECK_LIMIT();

          mz_patch_branch(ref);
          mz_patch_branch(ref2);
          __END_SHORT_JUMPS__(1);
        }
      }

      /* evaluate remaining expressions */
      for (i = 1; i < seq->count; i++) {
        scheme_generate_non_tail(seq->array[i], jitter, 1, 1, 1); /* sync's below */
        CHECK_LIMIT();
      }

      /* Restore values, if necessary */
      if (!result_ignored) {
        mz_popr_p(JIT_R0);
        if (multi_ok) {
          mz_popr_p(JIT_R1);
          mz_popr_p(JIT_R2);
          mz_rs_sync();
          CHECK_LIMIT();
          __START_TINY_JUMPS__(1);
          ref = jit_bnei_p(jit_forward(), JIT_R0, 0x0);
          CHECK_LIMIT();
          mz_tl_ldi_p(JIT_R0, tl_scheme_current_thread);
          jit_stxi_p(&((Scheme_Thread *)0x0)->ku.multiple.array, JIT_R0, JIT_R1);
          jit_rshi_ul(JIT_R2, JIT_R2, 0x1);
          jit_stxi_l(&((Scheme_Thread *)0x0)->ku.multiple.count, JIT_R0, JIT_R2);
          (void)jit_movi_p(JIT_R0, SCHEME_MULTIPLE_VALUES);

          mz_patch_branch(ref);
          __END_TINY_JUMPS__(1);
        }

        if (target != JIT_R0)
          jit_movr_p(target, JIT_R0);
      }

      if (for_branch) finish_branch(jitter, target, for_branch);

      END_JIT_DATA(6);

      return 1;
    }
    break;
  case scheme_set_bang_type:
    {
      Scheme_Set_Bang *sb = (Scheme_Set_Bang *)obj;
      Scheme_Object *p, *v;
      int pos, set_undef;
      GC_CAN_IGNORE jit_insn *ref1, *ref2, *ref3;

      START_JIT_DATA();
      
      LOG_IT(("set!\n"));
      
      p = sb->val;
      v = sb->var;
      set_undef = sb->set_undef;

      scheme_generate_non_tail(p, jitter, 0, 1, 0);
      CHECK_LIMIT();
      mz_rs_sync();
      
      /* Load prefix: */
      pos = mz_remap(SCHEME_TOPLEVEL_DEPTH(v));
      mz_rs_ldxi(JIT_R2, pos);
      /* Extract bucket from prefix: */
      pos = SCHEME_TOPLEVEL_POS(v);
      jit_ldxi_p(JIT_R2, JIT_R2, &(((Scheme_Prefix *)0x0)->a[pos]));
      CHECK_LIMIT();
	
      /* R0 has values, R2 has bucket */
      __START_SHORT_JUMPS__(1);
      jit_ldxi_p(JIT_R1, JIT_R2, &((Scheme_Bucket *)0x0)->val);
      ref1 = jit_beqi_p(jit_forward(), JIT_R1, NULL);
      jit_ldxi_s(JIT_R1, JIT_R2, &((Scheme_Bucket_With_Flags *)0x0)->flags);
      ref2 = jit_bmsi_i(jit_forward(), JIT_R1, GLOB_IS_IMMUTATED);
      
      /* Fast path: */
      jit_stxi_p(&((Scheme_Bucket *)0x0)->val, JIT_R2, JIT_R0);
      ref3 = jit_jmpi(jit_forward());
      
      /* Slow path: */
      mz_patch_branch(ref1);
      mz_patch_branch(ref2);
      __END_SHORT_JUMPS__(1);
      JIT_UPDATE_THREAD_RSPTR_FOR_BRANCH_IF_NEEDED();
      mz_prepare(3);
      (void)jit_movi_i(JIT_R1, set_undef);
      jit_pusharg_p(JIT_R1);
      jit_pusharg_p(JIT_R0);
      jit_pusharg_p(JIT_R2);
      CHECK_LIMIT();
      (void)mz_finish_lwe(ts_call_set_global_bucket, ref1);
      CHECK_LIMIT();
          
      __START_SHORT_JUMPS__(1);
      mz_patch_ucbranch(ref3);
      __END_SHORT_JUMPS__(1);

      if (for_branch) 
        finish_branch_with_true(jitter, for_branch);
      else {
        if (!result_ignored)
          (void)jit_movi_p(target, scheme_void);
      }
      END_JIT_DATA(7);

      return 1;
    }
    break;
  case scheme_apply_values_type:
    {
      Scheme_Object *p, *v;
      START_JIT_DATA();

      LOG_IT(("appvals\n"));

      v = SCHEME_PTR1_VAL(obj);
      p = SCHEME_PTR2_VAL(obj);

      v = scheme_specialize_to_constant(v, jitter, 0);
      p = scheme_specialize_to_constant(p, jitter, 0);

      if (is_single_valued(p, jitter)) {
        /* We might discover late that `v` produces a single value,
           possibly because we're in a specialized closure. In that
           case, use a plain application. */
        Scheme_Object *alt_rands[2];
        int r;

        alt_rands[0] = v;
        alt_rands[1] = p;

        r = scheme_generate_app(NULL, alt_rands, 1, 0, jitter, is_tail, multi_ok, result_ignored, 0);

        CHECK_LIMIT();
        if (target != JIT_R0)
          jit_movr_p(target, JIT_R0);

        if (for_branch) finish_branch(jitter, target, for_branch);

        return r;
      } else {
        GC_CAN_IGNORE jit_insn *ref, *ref2, *ref3, *ref5, *refloop;

        scheme_generate_non_tail(v, jitter, 0, 1, 0);
        CHECK_LIMIT();

        /* If v is not known to produce a procedure, then check result: */
        if (!is_a_procedure(v, jitter)) {
          mz_rs_sync();
          (void)jit_bmsi_l(sjc.bad_app_vals_target, JIT_R0, 0x1);
          jit_ldxi_s(JIT_R1, JIT_R0, &((Scheme_Object *)0x0)->type);
          (void)jit_blti_i(sjc.bad_app_vals_target, JIT_R1, scheme_prim_type);
          (void)jit_bgti_i(sjc.bad_app_vals_target, JIT_R1, scheme_proc_chaperone_type);
          CHECK_LIMIT();
        }

        mz_pushr_p(JIT_R0);
        scheme_generate_non_tail(p, jitter, 1, 1, 0);
        CHECK_LIMIT();

        mz_popr_p(JIT_V1);
        /* Function is in V1, argument(s) in R0 */

        mz_rs_sync();

        __START_SHORT_JUMPS__(1);
        ref = jit_beqi_p(jit_forward(), JIT_R0, SCHEME_MULTIPLE_VALUES);
        /* Single-value case: --------------- */
        /* We definitely have stack space for one argument, because we
           just used it for the rator. */
        if (is_tail) {
          mz_ld_runstack_base_alt(JIT_RUNSTACK);
          jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK_BASE_OR_ALT(JIT_RUNSTACK), WORDS_TO_BYTES(1));
        } else {
          jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
        }
        CHECK_RUNSTACK_OVERFLOW();
        jit_str_p(JIT_RUNSTACK, JIT_R0);
        jit_movi_l(JIT_R0, 1);
        ref2 = jit_jmpi(jit_forward());
        CHECK_LIMIT();

        /* Multiple-values case: ------------ */
        mz_patch_branch(ref);
        /* Get new argc: */
        (void)mz_tl_ldi_p(JIT_R1, tl_scheme_current_thread);
        jit_ldxi_l(JIT_R2, JIT_R1, &((Scheme_Thread *)0x0)->ku.multiple.count);
        /* Enough room on runstack? */
        mz_tl_ldi_p(JIT_R0, tl_MZ_RUNSTACK_START);
        if (is_tail) {
          mz_ld_runstack_base_alt(JIT_R0);
          jit_subr_ul(JIT_R0, JIT_RUNSTACK_BASE_OR_ALT(JIT_R0), JIT_R0);
        } else {
          jit_subr_ul(JIT_R0, JIT_RUNSTACK, JIT_R0); 
        }
        CHECK_LIMIT();
        /* R0 is space left (in bytes), R2 is argc */
        jit_lshi_l(JIT_R2, JIT_R2, JIT_LOG_WORD_SIZE);
        if (is_tail) {
          int fpos, fstack;
          fstack = scheme_mz_flostack_save(jitter, &fpos);
          __END_SHORT_JUMPS__(1);
          scheme_mz_flostack_restore(jitter, 0, 0, 1, 1);
          (void)jit_bltr_ul(sjc.app_values_tail_slow_code, JIT_R0, JIT_R2);
          __START_SHORT_JUMPS__(1);
          scheme_mz_flostack_restore(jitter, fstack, fpos, 0, 1);
          ref5 = 0;
        } else {
          GC_CAN_IGNORE jit_insn *refok;
          refok = jit_bger_ul(jit_forward(), JIT_R0, JIT_R2);
          __END_SHORT_JUMPS__(1);
          if (multi_ok) {
            (void)jit_calli(sjc.app_values_multi_slow_code);
          } else {
            (void)jit_calli(sjc.app_values_slow_code);
          }
          __START_SHORT_JUMPS__(1);
          ref5 = jit_jmpi(jit_forward());
          mz_patch_branch(refok);
        }
        CHECK_LIMIT();
        if (is_tail) {
          mz_ld_runstack_base_alt(JIT_RUNSTACK);
          jit_subr_ul(JIT_RUNSTACK, JIT_RUNSTACK_BASE_OR_ALT(JIT_RUNSTACK), JIT_R2);
        } else {
          jit_subr_ul(JIT_RUNSTACK, JIT_RUNSTACK, JIT_R2);
        }
        CHECK_RUNSTACK_OVERFLOW();
        /* Copy args: */
        jit_ldxi_l(JIT_R1, JIT_R1, &((Scheme_Thread *)0x0)->ku.multiple.array);
        refloop = jit_get_ip();
        ref3 = jit_blei_l(jit_forward(), JIT_R2, 0);
        jit_subi_l(JIT_R2, JIT_R2, JIT_WORD_SIZE);
        jit_ldxr_p(JIT_R0, JIT_R1, JIT_R2);
        jit_stxr_p(JIT_R2, JIT_RUNSTACK, JIT_R0);
        (void)jit_jmpi(refloop);
        CHECK_LIMIT();
        mz_patch_branch(ref3);
        /* clear array pointer and re-laod argc: */
        (void)mz_tl_ldi_p(JIT_R0, tl_scheme_current_thread);
        (void)jit_movi_p(JIT_R1, NULL);
        jit_stxi_l(&((Scheme_Thread *)0x0)->ku.multiple.array, JIT_R0, JIT_R1);
        jit_ldxi_l(JIT_R0, JIT_R0, &((Scheme_Thread *)0x0)->ku.multiple.count);
        CHECK_LIMIT();

        /* Perform call --------------------- */
        /* Function is in V1, argc in R0, args on RUNSTACK */
        mz_patch_ucbranch(ref2);
        __END_SHORT_JUMPS__(1);

        if (is_tail) {
          if (!sjc.shared_tail_argc_code) {
            sjc.shared_tail_argc_code = scheme_generate_shared_call(-1, jitter, 1, 0, 1, 0, 0, 0, 0);
          }
          mz_set_local_p(JIT_R0, JIT_LOCAL2);
          (void)jit_jmpi(sjc.shared_tail_argc_code);
        } else {
          int mo = (multi_ok 
                    ? (result_ignored ? SHARED_RESULT_IGNORED_CASE : SHARED_MULTI_OK_CASE) 
                    : SHARED_SINGLE_VALUE_CASE);
          void *code;
          if (!sjc.shared_non_tail_argc_code[mo]) {
            scheme_ensure_retry_available(jitter, multi_ok, result_ignored);
            code = scheme_generate_shared_call(-2, jitter, multi_ok, result_ignored, 0, 0, 0, 0, 0);
            sjc.shared_non_tail_argc_code[mo] = code;
          }
          code = sjc.shared_non_tail_argc_code[mo];
          (void)jit_calli(code);
          /* non-tail code pops args off runstack for us */
          jitter->need_set_rs = 1;
          mz_patch_ucbranch(ref5);
          if (target != JIT_R0)
            jit_movr_p(target, JIT_R0);
        }
        CHECK_LIMIT();

        if (for_branch) finish_branch(jitter, target, for_branch);

        END_JIT_DATA(81);

        if (is_tail)
          return 2;
        return 1;
      }
    }
    break;
  case scheme_with_immed_mark_type:
    {
      Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)obj;
      START_JIT_DATA();

      LOG_IT(("with-immediate-continuation-mark...\n"));

      scheme_generate_two_args(wcm->key, wcm->val, jitter, 1, 0);
      CHECK_LIMIT();
      
      /* key is in JIT_R0, default value is in JIT_R1 */
      mz_rs_sync();

      (void)jit_calli(sjc.with_immed_mark_code);

      mz_rs_dec(1);
      CHECK_RUNSTACK_OVERFLOW();
      mz_runstack_pushed(jitter, 1);
      mz_rs_str(JIT_R0);
      
      CHECK_LIMIT();

      END_JIT_DATA(22);

      LOG_IT(("...in\n"));

      if (for_values)
        for_values->position++;

      return scheme_generate(wcm->body, jitter, is_tail, wcm_may_replace, 
                             multi_ok, orig_target, for_branch, for_values);
    }
    break;
  case scheme_boxenv_type:
    {
      Scheme_Object *p, *v;
      int pos;
      START_JIT_DATA();

      LOG_IT(("boxenv\n"));

      mz_rs_sync();
      JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();

      v = SCHEME_PTR1_VAL(obj);
      pos = mz_remap(SCHEME_INT_VAL(v));
      p = SCHEME_PTR2_VAL(obj);

#ifdef CAN_INLINE_ALLOC
      scheme_inline_alloc(jitter, sizeof(Scheme_Object*), -1, 0, 0, 0, 0, 0);
      CHECK_LIMIT();
      jit_addi_p(JIT_R0, JIT_V1, OBJHEAD_SIZE);
      jit_ldxi_p(JIT_R2, JIT_RUNSTACK, WORDS_TO_BYTES(pos));
      jit_str_p(JIT_R0, JIT_R2);
#else
      jit_ldxi_p(JIT_R2, JIT_RUNSTACK, WORDS_TO_BYTES(pos));
      mz_prepare(1);
      jit_pusharg_p(JIT_R2);
      {
        GC_CAN_IGNORE jit_insn *refr USED_ONLY_FOR_FUTURES;
        (void)mz_finish_lwe(ts_scheme_make_envunbox, refr);
      }
      jit_retval(JIT_R0);
#endif
      jit_stxi_p(WORDS_TO_BYTES(pos), JIT_RUNSTACK, JIT_R0);
      CHECK_LIMIT();

      scheme_generate(p, jitter, is_tail, wcm_may_replace, multi_ok, orig_target, for_branch, for_values);

      END_JIT_DATA(8);

      return 1;
    }
    break;
  case scheme_varref_form_type:
    {
      if (for_branch)
        finish_branch_with_true(jitter, for_branch);
      else {
        Scheme_Object *dummy;
        int pos, is_const;

        mz_rs_sync();

        is_const = (SCHEME_PAIR_FLAGS(obj) & 0x1);

        dummy = SCHEME_PTR2_VAL(obj);
        obj = SCHEME_PTR1_VAL(obj);
      
        /* Load global array: */
        pos = mz_remap(SCHEME_TOPLEVEL_DEPTH(obj));
        jit_ldxi_p(JIT_R2, JIT_RUNSTACK, WORDS_TO_BYTES(pos));
        /* Load bucket: */
        pos = SCHEME_TOPLEVEL_POS(obj);
        jit_ldxi_p(JIT_R1, JIT_R2, &(((Scheme_Prefix *)0x0)->a[pos]));
        CHECK_LIMIT();

        /* Load dummy bucket: */
        if (SCHEME_FALSEP(dummy)) {
          (void)jit_movi_p(JIT_R2, scheme_false);
        } else {
          pos = SCHEME_TOPLEVEL_POS(dummy);
          jit_ldxi_p(JIT_R2, JIT_R2, &(((Scheme_Prefix *)0x0)->a[pos]));
          CHECK_LIMIT();
        }

        JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
        mz_prepare(2);
        jit_pusharg_p(JIT_R2);
        jit_pusharg_p(JIT_R1);
        {
          GC_CAN_IGNORE jit_insn *refr  USED_ONLY_FOR_FUTURES;
          if (is_const) {
            (void)mz_finish_lwe(ts_make_global_const_ref, refr);
          } else {
            (void)mz_finish_lwe(ts_make_global_ref, refr);
          }
        }
        CHECK_LIMIT();
        jit_retval(target);
        VALIDATE_RESULT(target);
      }

      return 1;
    }
    break;
  case scheme_splice_sequence_type:
  case scheme_define_values_type:
  case scheme_define_syntaxes_type:
  case scheme_begin_for_syntax_type:
  case scheme_require_form_type:
  case scheme_module_type:
  case scheme_inline_variant_type:
    {
      scheme_signal_error("internal error: cannot JIT a top-level form");
      return 0;
    }
    break;
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)obj;
      int r;

      LOG_IT(("app %d\n", app->num_args));

      if (for_values
          && SAME_OBJ(app->args[0], scheme_values_proc)
          && (app->num_args == for_values->count)) {
        int i, pos;

        mz_runstack_skipped(jitter, app->num_args);

        for (i = 0; i < app->num_args; i++) {
          scheme_generate_non_tail(app->args[i+1], jitter, 0, 1, 0);
          CHECK_LIMIT();
          pos = mz_remap(for_values->position + app->num_args + i);
          mz_rs_stxi(pos, JIT_R0);
        }

        for_values->delivered = 1;
        return 1;
      }

      r = scheme_generate_inlined_nary(jitter, app, is_tail, multi_ok, NULL, 1, result_ignored, target);
      CHECK_LIMIT();
      if (r) {
        if (for_branch) finish_branch(jitter, target, for_branch);
	return r;
      }

      r = scheme_generate_app(app, NULL, app->num_args, app->num_args, jitter, is_tail, multi_ok, result_ignored, 0);

      CHECK_LIMIT();
      if (target != JIT_R0)
        jit_movr_p(target, JIT_R0);

      if (for_branch) finish_branch(jitter, target, for_branch);

      return r;
    }
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)obj;
      Scheme_Object *args[2];
      int r;

      r = scheme_generate_inlined_unary(jitter, app, is_tail, multi_ok, NULL, 0, result_ignored, target);
      CHECK_LIMIT();
      if (r) {
        if (for_branch) finish_branch(jitter, target, for_branch);
	return r;
      }

      LOG_IT(("app 2\n"));

      args[0] = app->rator;
      args[1] = app->rand;
      
      r = scheme_generate_app(NULL, args, 1, 1, jitter, is_tail, multi_ok, result_ignored, 0);

      CHECK_LIMIT();
      if (target != JIT_R0)
        jit_movr_p(target, JIT_R0);

      if (for_branch) finish_branch(jitter, target, for_branch);

      return r;
    }
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)obj;
      Scheme_Object *args[3];
      int r;

      if (for_values
          && SAME_OBJ(app->rator, scheme_values_proc)
          && (for_values->count == 2)) {
        int pos;

        mz_runstack_skipped(jitter, 2);

        scheme_generate_non_tail(app->rand1, jitter, 0, 1, 0);
        CHECK_LIMIT();
        pos = mz_remap(for_values->position + 2);
        mz_rs_stxi(pos, JIT_R0);

        scheme_generate_non_tail(app->rand2, jitter, 0, 1, 0);
        CHECK_LIMIT();
        pos = mz_remap(for_values->position + 2 + 1);
        mz_rs_stxi(pos, JIT_R0);

        for_values->delivered = 1;

        return 1;
      }

      r = scheme_generate_inlined_binary(jitter, app, is_tail, multi_ok, NULL, 0, result_ignored, target);
      CHECK_LIMIT();
      if (r) {
        if (for_branch) finish_branch(jitter, target, for_branch);
	return r;
      }

      LOG_IT(("app 3\n"));

      args[0] = app->rator;
      args[1] = app->rand1;
      args[2] = app->rand2;

      r = scheme_generate_app(NULL, args, 2, 2, jitter, is_tail, multi_ok, result_ignored, 0);

      CHECK_LIMIT();
      if (target != JIT_R0)
        jit_movr_p(target, JIT_R0);

      if (for_branch) finish_branch(jitter, target, for_branch);

      return r;
    }
  case scheme_sequence_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)obj;
      int cnt = seq->count, i;
      mz_jit_unbox_state ubs;
      START_JIT_DATA();

      LOG_IT(("begin\n"));

      scheme_mz_unbox_save(jitter, &ubs);

      for (i = 0; i < cnt - 1; i++) {
	scheme_generate_non_tail(seq->array[i], jitter, 1, 1, 1);
	CHECK_LIMIT();
      }

      END_JIT_DATA(11);

      scheme_mz_unbox_restore(jitter, &ubs);

      return scheme_generate(seq->array[cnt - 1], jitter, is_tail, wcm_may_replace, 
                             multi_ok, orig_target, for_branch, for_values);
    }
  case scheme_branch_type:
    {
      return generate_branch(obj, jitter, is_tail, wcm_may_replace, multi_ok, orig_target, result_ignored, for_branch);
    }
  case scheme_lambda_type:
    {
      Scheme_Lambda *lam = (Scheme_Lambda *)obj;
      START_JIT_DATA();

      LOG_IT(("lambda\n"));
      
      if (for_branch)
        finish_branch_with_true(jitter, for_branch);
      else {
        mz_rs_sync();

        generate_closure_prep(lam, jitter);
        CHECK_LIMIT();
      
        /* Allocate closure */
        generate_closure(lam, jitter, 1);
        CHECK_LIMIT();

        generate_closure_fill(lam, jitter);

        CHECK_LIMIT();
        if (target != JIT_R0)
          jit_movr_p(target, JIT_R0);
      }

      END_JIT_DATA(13);

      return 0;
    }
  case scheme_let_value_type:
    {
      Scheme_Let_Value *lv = (Scheme_Let_Value *)obj;
      int ab = SCHEME_LET_VALUE_AUTOBOX(lv), i, pos;
      mz_jit_unbox_state ubs;
      START_JIT_DATA();

      LOG_IT(("let...\n"));

      scheme_mz_unbox_save(jitter, &ubs);

      if (lv->count == 1) {
	/* Expect one result: */
        Scheme_Object *specialized = NULL;
        if (SCHEME_NATIVE_LAMBDA_FLAGS(jitter->nc->code) & NATIVE_SPECIALIZED)
          specialized = extract_closure_local(lv->position, jitter, 1);
	scheme_generate_non_tail(lv->value, jitter, 0, 1, 0); /* no sync */
	CHECK_LIMIT();
	if (ab) {
	  pos = mz_remap(lv->position);
          if (specialized)
            scheme_mz_load_retained(jitter, JIT_R2, specialized);
          else
            mz_rs_ldxi(JIT_R2, pos);
	  jit_str_p(JIT_R2, JIT_R0);
	} else {
          MZ_ASSERT(!specialized);
	  pos = mz_remap(lv->position);
	  mz_rs_stxi(pos, JIT_R0);
	}
	CHECK_LIMIT();
      } else {
	/* Expect multiple results: */
	GC_CAN_IGNORE jit_insn *ref, *ref2, *ref3;
        Expected_Values_Info for_rhs_values;

        for_rhs_values.count = lv->count;
        for_rhs_values.position = lv->position;
        for_rhs_values.delivered = 0;

	scheme_generate_non_tail_for_values(lv->value, jitter, 1, 1, 0,
                                            ab ? NULL : &for_rhs_values); /* no sync */
	CHECK_LIMIT();

        if (for_rhs_values.delivered) {
          /* Generated code for `lv->value` delivers values to the runstack,
             so there's nothing more to do here */
        } else {
          mz_rs_sync();
    
          __START_SHORT_JUMPS__(1);

          /* Did we get multiple results? If not, go to error: */
          ref = jit_bnei_p(jit_forward(), JIT_R0, SCHEME_MULTIPLE_VALUES);
          /* Load count and result array: */
          mz_tl_ldi_p(JIT_V1, tl_scheme_current_thread);
          jit_ldxi_p(JIT_R2, JIT_V1, &((Scheme_Thread *)0x0)->ku.multiple.array);
          jit_ldxi_l(JIT_R1, JIT_V1, &((Scheme_Thread *)0x0)->ku.multiple.count);
          CHECK_LIMIT();
          /* If we got the expected count, jump to installing values: */
          ref2 = jit_beqi_i(jit_forward(), JIT_R1, lv->count);
          /* Otherwise, jump to error: */
          ref3 = jit_jmpi(jit_forward());
          CHECK_LIMIT();

          /* Jump here when we didn't get multiple values. Set count to 1
             and "array" to single value: */
          mz_patch_branch(ref);
          jit_movi_i(JIT_R1, 1);
          jit_movr_p(JIT_R2, JIT_R0);
          CHECK_LIMIT();
	  
          /* Error starts here: */
          mz_patch_ucbranch(ref3);
          JIT_UPDATE_THREAD_RSPTR_FOR_BRANCH_IF_NEEDED();
          mz_prepare(3);
          jit_pusharg_p(JIT_R2);
          jit_pusharg_i(JIT_R1);
          CHECK_LIMIT();
          jit_movi_i(JIT_V1, lv->count);
          jit_pusharg_i(JIT_V1);
          (void)mz_finish_lwe(ts_lexical_binding_wrong_return_arity, ref);
          CHECK_LIMIT();

          /* Continue with expected values; R2 has values and V1 has thread pointer: */
          mz_patch_branch(ref2);
          __END_SHORT_JUMPS__(1);
          (void)jit_movi_p(JIT_R0, NULL);
          jit_stxi_p(&((Scheme_Thread *)0x0)->ku.multiple.array, JIT_V1, JIT_R0);
          for (i = 0; i < lv->count; i++) {
            jit_ldxi_p(JIT_R1, JIT_R2, WORDS_TO_BYTES(i));
            if (ab) {
              pos = mz_remap(lv->position + i);
              jit_ldxi_p(JIT_R0, JIT_RUNSTACK, WORDS_TO_BYTES(pos));
              jit_str_p(JIT_R0, JIT_R1);
            } else {
              pos = mz_remap(lv->position + i);
              jit_stxi_p(WORDS_TO_BYTES(pos), JIT_RUNSTACK, JIT_R1);
            }
            CHECK_LIMIT();
          }
        }
      }

      END_JIT_DATA(14);

      LOG_IT(("...in\n"));

      scheme_mz_unbox_restore(jitter, &ubs);

      return scheme_generate(lv->body, jitter, is_tail, wcm_may_replace, 
                             multi_ok, orig_target, for_branch, for_values);
    }
  case scheme_let_void_type:
    {
      Scheme_Let_Void *lv = (Scheme_Let_Void *)obj;
      int c = lv->count;
      mz_jit_unbox_state ubs;
      START_JIT_DATA();

      LOG_IT(("letv...\n"));

      scheme_mz_unbox_save(jitter, &ubs);

      mz_rs_dec(c);
      CHECK_RUNSTACK_OVERFLOW();
      scheme_stack_safety(jitter, c, 0);
      mz_runstack_pushed(jitter, c);

      if (SCHEME_LET_VOID_AUTOBOX(lv)) {
	int i;
        mz_rs_sync();
	JIT_UPDATE_THREAD_RSPTR_IF_NEEDED();
	for (i = 0; i < c; i++) {
	  CHECK_LIMIT();
#ifdef CAN_INLINE_ALLOC
          scheme_inline_alloc(jitter, sizeof(Scheme_Object*), -1, 0, 0, 0, 0, 0);
          CHECK_LIMIT();
          jit_addi_p(JIT_R0, JIT_V1, OBJHEAD_SIZE);
          (void)jit_movi_p(JIT_R1, scheme_undefined);
          jit_str_p(JIT_R0, JIT_R1);
#else
	  (void)jit_movi_p(JIT_R0, scheme_undefined);
	  mz_prepare(1);
	  jit_pusharg_p(JIT_R0);
          {
            GC_CAN_IGNORE jit_insn *refr USED_ONLY_FOR_FUTURES;
            (void)mz_finish_lwe(ts_scheme_make_envunbox, refr);
          }
	  jit_retval(JIT_R0);
#endif
	  jit_stxi_p(WORDS_TO_BYTES(i), JIT_RUNSTACK, JIT_R0);
	}
      }
      CHECK_LIMIT();

      END_JIT_DATA(15);

      LOG_IT(("...in\n"));

      scheme_mz_unbox_restore(jitter, &ubs);

      if (for_values)
        for_values->position += c;

      return scheme_generate(lv->body, jitter, is_tail, wcm_may_replace, 
                             multi_ok, orig_target, for_branch, for_values);
    }
  case scheme_letrec_type:
    {
      Scheme_Letrec *l = (Scheme_Letrec *)obj;
      int i, nsrs, prepped = 0;
      mz_jit_unbox_state ubs;
      START_JIT_DATA();

      LOG_IT(("letrec...\n"));

      scheme_mz_unbox_save(jitter, &ubs);

      mz_rs_sync();

      /* Box any unboxed values that will go into a closure */
      for (i = 0; i < l->count; i++) {
	if (generate_closure_prep((Scheme_Lambda *)l->procs[i], jitter))
          prepped = 1;
        CHECK_LIMIT();
      }

      /* Create unfinished closures */
      for (i = 0; i < l->count; i++) {
	((Scheme_Lambda *)l->procs[i])->context = (Scheme_Object *)l;
	generate_closure((Scheme_Lambda *)l->procs[i], jitter, i + 1 == l->count);
	CHECK_LIMIT();
	jit_stxi_p(WORDS_TO_BYTES(i), JIT_RUNSTACK, JIT_R0);
      }
      /* We assume no allocation between last generated closure and
         filling all closures, since the last one may be allocated as
         "dirty". */

      /* Close them: */
      for (i = l->count; i--; ) {
        /* Last one we created may still be in JIT_R0: */
	if (prepped || (i != l->count - 1)) {
	  jit_ldxi_p(JIT_R0, JIT_RUNSTACK, WORDS_TO_BYTES(i));
	}
	generate_closure_fill((Scheme_Lambda *)l->procs[i], jitter);
	CHECK_LIMIT();
      }

      END_JIT_DATA(16);

      LOG_IT(("...in\n"));

      /* Assuming we can replace the last l->count, push closure info instead: */
      nsrs = jitter->need_set_rs;
      if (mz_try_runstack_pop(jitter, l->count)) {
        int i;
        for (i = l->count; i--; ) {
          Scheme_Lambda *lam2 = (Scheme_Lambda *)l->procs[i];
          mz_runstack_closure_pushed(jitter, 
                                     (lam2->num_params
                                      - ((SCHEME_LAMBDA_FLAGS(lam2) & LAMBDA_HAS_REST)
                                         ? 1
                                         : 0)),
                                     (((SCHEME_LAMBDA_FLAGS(lam2) & LAMBDA_PRESERVES_MARKS)
                                       ? NATIVE_PRESERVES_MARKS
                                       : 0)
                                      | ((SCHEME_LAMBDA_FLAGS(lam2) & LAMBDA_SINGLE_RESULT)
                                         ? NATIVE_IS_SINGLE_RESULT
                                         : 0)));
        }
        jitter->need_set_rs = nsrs;
      }

      scheme_mz_unbox_restore(jitter, &ubs);

      return scheme_generate(l->body, jitter, is_tail, wcm_may_replace, 
                             multi_ok, orig_target, for_branch, for_values);
    }
  case scheme_let_one_type:
    {
      Scheme_Let_One *lv = (Scheme_Let_One *)obj;
      int flonum, unused, extfl;
      mz_jit_unbox_state ubs;
      START_JIT_DATA();

      LOG_IT(("leto...\n"));

      scheme_mz_unbox_save(jitter, &ubs);

      mz_runstack_skipped(jitter, 1);

#ifdef USE_FLONUM_UNBOXING
      flonum = (SCHEME_LET_ONE_TYPE(lv) == SCHEME_LOCAL_TYPE_FLONUM);
      if (MZ_LONG_DOUBLE_AND(SCHEME_LET_ONE_TYPE(lv) == SCHEME_LOCAL_TYPE_EXTFLONUM))
        flonum = extfl = 1;
      else
        extfl = 0;
#else
      flonum = extfl = 0;
#endif
      unused = SCHEME_LET_EVAL_TYPE(lv) & LET_ONE_UNUSED;

      PAUSE_JIT_DATA();
      if (flonum) {
#ifdef USE_FLONUM_UNBOXING
        if (scheme_can_unbox_inline(lv->value, 5, JIT_FPUSEL_FPR_NUM(extfl)-1, 0, extfl)) {
          jitter->unbox++;
          MZ_FPUSEL_STMT_ONLY(extfl, jitter->unbox_extflonum++;);
          scheme_generate_unboxed(lv->value, jitter, 2, 0);
        } else if (scheme_can_unbox_directly(lv->value, extfl)) {
          jitter->unbox++;
          MZ_FPUSEL_STMT_ONLY(extfl, jitter->unbox_extflonum++;);
          scheme_generate_unboxed(lv->value, jitter, 1, 0);
        } else {
          /* validator should ensure that this is ok */
          jitter->unbox++;
          MZ_FPUSEL_STMT_ONLY(extfl, jitter->unbox_extflonum++;);
          scheme_generate_unboxed(lv->value, jitter, 0, 1);
        }
#endif
      } else if (unused && SCHEME_FALSEP(lv->value)) {
        /* unused constants are collapsed to #f by the bytecde compiler */
      } else
        scheme_generate_non_tail(lv->value, jitter, 0, 1, unused); /* no sync */

      RESUME_JIT_DATA();
      CHECK_LIMIT();
      
      if (!unused) {
        mz_runstack_unskipped(jitter, 1);
        
        mz_rs_dec(1);
        CHECK_RUNSTACK_OVERFLOW();
      }

      if (flonum) {
#ifdef USE_FLONUM_UNBOXING
        MZ_FPUSEL_STMT_ONLY(extfl, --jitter->unbox_extflonum);
        --jitter->unbox;
        --jitter->unbox_depth;
        if (jitter->unbox_depth)
          scheme_signal_error("internal error: flonum let RHS leaves unbox depth");
        scheme_generate_flonum_local_unboxing(jitter, 1, 0, extfl);
        CHECK_LIMIT();
        (void)jit_movi_p(JIT_R0, NULL);
#endif
      } else {
        if (!unused)
          mz_runstack_pushed(jitter, 1);
      }

      if (!unused) {
        mz_rs_str(JIT_R0);
        jitter->r0_status = 0;
        jitter->r1_status = -1;
        mz_SET_REG_STATUS_VALID(1);
      }
      
      END_JIT_DATA(17);

      LOG_IT(("...in\n"));

      scheme_mz_unbox_restore(jitter, &ubs);

      if (for_values)
        for_values->position++;

      return scheme_generate(lv->body, jitter, is_tail, wcm_may_replace, 
                             multi_ok, orig_target, for_branch, for_values);
    }
  case scheme_with_cont_mark_type:
    {
      Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)obj;
      mz_jit_unbox_state ubs;
      START_JIT_DATA();

      LOG_IT(("wcm...\n"));

      scheme_mz_unbox_save(jitter, &ubs);

      /* Key: */
      scheme_generate_non_tail(wcm->key, jitter, 0, 1, 0); /* sync'd below */
      mz_pushr_p(JIT_R0); /* sync'd below */
      CHECK_LIMIT();
      /* Value: */
      scheme_generate_non_tail(wcm->val, jitter, 0, 1, 0); /* sync'd below */
      CHECK_LIMIT();
      mz_pushr_p(JIT_R0); /* sync'd below */

      /* Key and value are on runstack */
      mz_rs_sync();

      if (SCHEME_TYPE(wcm->key) < _scheme_values_types_) {
        /* Check whether the key is chaperoned: */
        GC_CAN_IGNORE jit_insn *ref, *ref2;
        mz_rs_ldxi(JIT_R0, 1);
        __START_TINY_JUMPS__(1);
        ref = jit_bmsi_i(jit_forward(), JIT_R0, 0x1);
        ref2 = mz_bnei_t(jit_forward(), JIT_R0, scheme_chaperone_type, JIT_R1);
        __END_TINY_JUMPS__(1);
        (void)jit_calli(sjc.wcm_chaperone); /* adjusts values on the runstack */
        __START_TINY_JUMPS__(1);
        mz_patch_branch(ref);
        mz_patch_branch(ref2);
        __END_TINY_JUMPS__(1);
      }

      CHECK_LIMIT();

      /* Key and value are (still) on runstack */
      if (!wcm_may_replace) {
        (void)jit_calli(sjc.wcm_nontail_code);
        wcm_may_replace = 1;
      } else
        (void)jit_calli(sjc.wcm_code);
      
      mz_popr_x();
      mz_popr_x();

      END_JIT_DATA(18);

      LOG_IT(("...in\n"));

      jitter->pushed_marks++;

      scheme_mz_unbox_restore(jitter, &ubs);
	
      return scheme_generate(wcm->body, jitter, is_tail, wcm_may_replace, 
                             multi_ok, orig_target, for_branch, for_values);
    }
  case scheme_quote_syntax_type:
    {
      Scheme_Quote_Syntax *qs = (Scheme_Quote_Syntax *)obj;
      int i, c, p;
      START_JIT_DATA();
      
      LOG_IT(("quote-syntax\n"));

      if (for_branch)
        finish_branch_with_true(jitter, for_branch);
      else {
        i = qs->position;
        c = mz_remap(qs->depth);
        p = qs->midpoint;
      
        mz_rs_sync();

        if (SCHEME_NATIVE_LAMBDA_FLAGS(jitter->nc->code) & NATIVE_SPECIALIZED) {
          Scheme_Object *stx;
          stx = extract_syntax(qs, jitter->nc);
          scheme_mz_load_retained(jitter, target, stx);
          CHECK_LIMIT();
        } else {
          jit_movi_i(JIT_R0, WORDS_TO_BYTES(c));
          jit_movi_i(JIT_R1, (int)(intptr_t)&(((Scheme_Prefix *)0x0)->a[i + p + 1]));
          jit_movi_i(JIT_R2, (int)(intptr_t)&(((Scheme_Prefix *)0x0)->a[p]));
          (void)jit_calli(sjc.quote_syntax_code);
          CHECK_LIMIT();

          if (target != JIT_R0)
            jit_movr_p(target, JIT_R0);
        }
      }
      
      END_JIT_DATA(10);

      return 1;
    }
  default:
    /* Other parts of the JIT rely on this code modifying the target register, only */
    if (for_branch) {
      if (SCHEME_FALSEP(obj))
        finish_branch_with_false(jitter, for_branch);
      else
        finish_branch_with_true(jitter, for_branch);
      return 1;
    } else if (jitter->unbox) {
      GC_CAN_IGNORE const char *bad = NULL;

#ifdef MZ_LONG_DOUBLE
      if (jitter->unbox_extflonum) {
        long_double d;
        int fpr0 USED_ONLY_SOMETIMES;

        if (SCHEME_LONG_DBLP(obj))
          d = SCHEME_LONG_DBL_VAL(obj);
        else {
          bad = "ext";
          d = get_long_double_zero();
        }

        fpr0 = JIT_FPU_FPR_0(jitter->unbox_depth);
        mz_fpu_movi_ld_fppush(fpr0, d, target);
     } else
#endif
      {
        double d;
        int fpr0 USED_ONLY_SOMETIMES;
        
        if (SCHEME_FLOATP(obj))
          d = SCHEME_FLOAT_VAL(obj);
        else {
          bad = "";
          d = 0.0;
        }
        
        fpr0 = JIT_FPR_0(jitter->unbox_depth);
        mz_movi_d_fppush(fpr0, d, target);
      }
      
      if (bad)
        scheme_log(NULL,
                   SCHEME_LOG_WARNING,
                   0,
                   "warning: JIT detects %sflonum operation applied to non-%sflonum constant: %V",
                   bad, bad,
                   obj);

      jitter->unbox_depth++;
        
      return 1;
    } else if (!result_ignored) {
      Scheme_Type type = SCHEME_TYPE(obj);
      START_JIT_DATA();

      LOG_IT(("const\n"));

      /* Avoid compiling closures multiple times: */
      if (jitter->retain_start) {
	if (type == scheme_closure_type) {
	  /* Compile the code and get a native closure: */
	  Scheme_Closure *c = (Scheme_Closure *)obj;
	  if (ZERO_SIZED_CLOSUREP(c))
	    obj = scheme_jit_closure((Scheme_Object *)c->code, NULL);
          else {
            /* Strange case that can happen if a 3-D macro injects a closure
               into an expression: */
            int i;
            obj = scheme_jit_closure((Scheme_Object *)c->code, NULL);
            obj = scheme_make_native_closure(((Scheme_Lambda *)obj)->u.native_code);
            for (i = c->code->closure_size; i--; ) {
              ((Scheme_Native_Closure *)obj)->vals[i] = c->vals[i];
            }
          }
	} else if (type == scheme_case_closure_type) {
	  /* Empty case closure? Turn in into a JITted empty case closure. */
	  obj = scheme_unclose_case_lambda(obj, 1);
	}
      }

      scheme_mz_load_retained(jitter, target, obj);

      END_JIT_DATA(19);
      return 1;
    } else {
      return 1;
    }
  }
}

/*========================================================================*/
/*                          procedure codegen                             */
/*========================================================================*/

void scheme_generate_function_prolog(mz_jit_state *jitter)
{
  int in;
  START_JIT_DATA();

  jit_prolog(NATIVE_ARG_COUNT);
  
  mz_push_threadlocal_early();

  in = jit_arg_p();
  jit_getarg_p(JIT_R0, in); /* closure */
  in = jit_arg_i();
  jit_getarg_i(JIT_R1, in); /* argc */
  in = jit_arg_p();
  jit_getarg_p(JIT_R2, in); /* argv */
  
  mz_push_locals();
  mz_push_threadlocal(in);

  mz_tl_ldi_p(JIT_RUNSTACK, tl_MZ_RUNSTACK);

  END_JIT_DATA(1);
}

static int generate_function_getarg(mz_jit_state *jitter, int has_rest, int num_params)
{
  int i, cnt;
  GC_CAN_IGNORE jit_insn *ref, *ref2;

  /* Normalize the argument array by making sure that it's at the
     start of the runstack, and set runstack base to be at the end of
     the arguments.  Rest arguments to be collected into a list remain
     in their original location. Note that tail calls modify the
     runstack below runstack base, which is why all parts of the
     runteim system that call scheme_apply() with an arrayu of
     arguments on the runstack must be prepared for that array to
     change during the call. */

  if (!num_params && !has_rest) {
    /* No arguments, so simply set runstack base to runstack. If it
       turns out that arguments are provided, then we'll abort through
       an arity exception, anyway. */
#ifdef JIT_RUNSTACK_BASE
    jit_movr_p(JIT_RUNSTACK_BASE, JIT_RUNSTACK);
#else
    mz_set_local_p(JIT_RUNSTACK, JIT_RUNSTACK_BASE_LOCAL);
#endif
    return 1;
  }

  /* If rands == runstack, set runstack base to runstack + rands (and
     don't copy rands), otherwise set base to runstack and copy
     arguments at runstack. Implement the test by optimistically
     assuming rands == runstack, so that there's just one jump. */
  jit_lshi_l(JIT_RUNSTACK_BASE_OR_ALT(JIT_V1), JIT_R1, JIT_LOG_WORD_SIZE);
  jit_addr_p(JIT_RUNSTACK_BASE_OR_ALT(JIT_V1), JIT_R2, JIT_RUNSTACK_BASE_OR_ALT(JIT_V1));
#ifndef JIT_RUNSTACK_BASE
  mz_set_local_p(JIT_V1, JIT_RUNSTACK_BASE_LOCAL);
#endif
  __START_TINY_OR_SHORT_JUMPS__(num_params < 10, num_params < 100);
  ref = jit_beqr_p(jit_forward(), JIT_RUNSTACK, JIT_R2);
  __END_TINY_OR_SHORT_JUMPS__(num_params < 10, num_params < 100);

  /* Since we're going to copy arguments, make sure argument
     count is right; otherwise, the arity error can use the
     arguments in the original location. */
  __START_TINY_OR_SHORT_JUMPS__(num_params < 10, num_params < 100);
  if (!has_rest)
    ref2 = jit_bnei_i(jit_forward(), JIT_R1, num_params);
  else
    ref2 = jit_blti_i(jit_forward(), JIT_R1, (num_params - 1));
  __END_TINY_OR_SHORT_JUMPS__(num_params < 10, num_params < 100);

#ifdef JIT_RUNSTACK_BASE
  jit_movr_p(JIT_RUNSTACK_BASE, JIT_RUNSTACK);
#else
  mz_set_local_p(JIT_RUNSTACK, JIT_RUNSTACK_BASE_LOCAL);
#endif

  /* Make stack room for arguments: */
  cnt = num_params;
  if (cnt) {
    CHECK_LIMIT();
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(cnt));
    CHECK_RUNSTACK_OVERFLOW();
    if (has_rest) {
      --cnt;
      scheme_stack_safety(jitter, 1, cnt);
    }
  }
  
  /* Extract arguments to runstack: */
  for (i = cnt; i--; ) {
    jit_ldxi_p(JIT_V1, JIT_R2, WORDS_TO_BYTES(i));
    jit_stxi_p(WORDS_TO_BYTES(i), JIT_RUNSTACK, JIT_V1);
    CHECK_LIMIT();
  }

  __START_TINY_OR_SHORT_JUMPS__(num_params < 10, num_params < 100);
  mz_patch_branch(ref);
  mz_patch_branch(ref2);
  __END_TINY_OR_SHORT_JUMPS__(num_params < 10, num_params < 100);

  return cnt;
}

typedef struct {
  Scheme_Lambda *lam;
  void *arity_code, *start_code, *tail_code, *code_end, **patch_depth;
  int max_extra, max_depth, max_tail_depth;
  Scheme_Native_Closure *nc;
  int argc, argv_delta;
  Scheme_Object **argv;
#ifdef NEED_RETAIN_CODE_POINTERS
  void *retain_code;
#endif
} Generate_Lambda;

static int do_generate_closure(mz_jit_state *jitter, void *_data)
{
  Generate_Lambda *gdata = (Generate_Lambda *)_data;
  Scheme_Lambda *lam = gdata->lam;
  void *start_code, *tail_code, *code_end, *arity_code, *do_arity_code;
#ifdef NEED_RETAIN_CODE_POINTERS
  void *retain_code = NULL;
#endif
  int i, r, cnt, has_rest, is_method, num_params, to_args, argc, argv_delta;
  int specialized;
  Scheme_Object **argv;

  start_code = jit_get_ip();

  jitter->nc = gdata->nc;

  argc = gdata->argc;
  argv = gdata->argv;
  argv_delta = gdata->argv_delta;

  scheme_generate_function_prolog(jitter);
  CHECK_LIMIT();

  cnt = generate_function_getarg(jitter, 
				 (SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_HAS_REST),
				 lam->num_params);
  /* At this point, all non-rest arguments are now at the runstack */
  CHECK_LIMIT();

  /* A tail call with arity checking can start here.
     (This is a little reundant checking when `start_code' is the
     entry point, but that's the slow path anyway.) */
  
  has_rest = ((SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_HAS_REST) ? 1 : 0);
  is_method = ((SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_IS_METHOD) ? 1 : 0);
  num_params = lam->num_params;
  if (num_params && has_rest)
    --num_params;

  if (num_params < MAX_SHARED_ARITY_CHECK) {
    void *shared_arity_code;

    shared_arity_code = sjc.shared_arity_check[num_params][has_rest][is_method];
    if (!shared_arity_code) {
      shared_arity_code = generate_lambda_simple_arity_check(num_params, has_rest, is_method, 1);
      shared_arity_code = jit_adjust_ip(shared_arity_code);
      sjc.shared_arity_check[num_params][has_rest][is_method] = shared_arity_code;
    }
    CHECK_NESTED_GENERATE();

    arity_code = jit_get_ip();
  
    do_arity_code = shared_arity_code;
  } else {
    arity_code = generate_lambda_simple_arity_check(num_params, has_rest, is_method, 0);
#ifdef NEED_RETAIN_CODE_POINTERS
    retain_code = MALLOC_N(void*,2);
    ((void **)retain_code)[1] = arity_code;
#endif
    arity_code = jit_adjust_ip(arity_code);
    CHECK_NESTED_GENERATE();

    do_arity_code = arity_code;
  }

  if (!has_rest)
    (void)jit_bnei_i(do_arity_code, JIT_R1, num_params);
  else
    (void)jit_blti_i(do_arity_code, JIT_R1, num_params);

  /* A tail call starts here. Caller must ensure that the stack is big
     enough, right number of arguments (at start of runstack), closure
     is in R0. If the closure has a rest arg, also ensure argc in R1
     and argv in R2. */
  tail_code = jit_get_ip();

  /* 0 params and has_rest => (lambda args E) where args is not in E,
     so accept any number of arguments and just clear them (for space 
     safety). */

  if (has_rest && lam->num_params) {
    /* If runstack == argv and argc == cnt, then we didn't
       copy args down, and we need to make room for scheme_null. */
    GC_CAN_IGNORE jit_insn *ref, *ref2, *ref3;
	  
    CHECK_LIMIT();
    
    __START_SHORT_JUMPS__(cnt < 100);

    /* check whether argv == runstack: */
    ref = jit_bner_p(jit_forward(), JIT_RUNSTACK, JIT_R2);
    /* check whether we have at least one rest arg: */
    ref3 = jit_bgti_i(jit_forward(), JIT_R1, cnt);
    /* yes and no: make room for the scheme_null */
    jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(1));
    CHECK_RUNSTACK_OVERFLOW();
    for (i = 0; i < cnt; i++) {
      jit_ldxi_p(JIT_V1, JIT_RUNSTACK, WORDS_TO_BYTES(i+1));
      jit_stxi_p(WORDS_TO_BYTES(i), JIT_RUNSTACK, JIT_V1);
      CHECK_LIMIT();
    }
    (void)jit_movi_p(JIT_V1, scheme_null);
    jit_stxi_p(WORDS_TO_BYTES(cnt), JIT_RUNSTACK, JIT_V1);
    ref2 = jit_jmpi(jit_forward());
    CHECK_LIMIT();

    /* Build a list for extra arguments: */
    mz_patch_branch(ref);
    mz_patch_branch(ref3);
    CHECK_LIMIT();

    jit_movi_i(JIT_V1, cnt);
    mz_set_local_p(JIT_V1, JIT_LOCAL3);
    mz_rs_sync();
    if ((SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_NEED_REST_CLEAR))
      (void)jit_calli(sjc.make_rest_list_clear_code);
    else
      (void)jit_calli(sjc.make_rest_list_code);
    jit_stxi_p(WORDS_TO_BYTES(cnt), JIT_RUNSTACK, JIT_V1);

    mz_patch_ucbranch(ref2); /* jump here if we copied and produced null */

    __END_SHORT_JUMPS__(cnt < 100);

    has_rest = 1;
    if (argc < (lam->num_params - 1)) {
      argv = NULL;
      argc = 0;
    }
  } else {
    if (has_rest && (SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_NEED_REST_CLEAR)) {
      /* if we get here, the rest argument isn't used */
      GC_CAN_IGNORE jit_insn *ref;
      __START_TINY_JUMPS__(1);
      /* check whether argv == runstack: */
      ref = jit_bner_p(jit_forward(), JIT_RUNSTACK, JIT_R2);
      __END_TINY_JUMPS__(1);
      /* yes, so clear rest args (for space safety): */
      mz_rs_sync();
      JIT_UPDATE_THREAD_RSPTR();
      CHECK_LIMIT();
      mz_prepare(3);
      jit_movi_i(JIT_V1, cnt);
      jit_pusharg_i(JIT_V1);
      jit_pusharg_i(JIT_R1);
      jit_pusharg_p(JIT_R0);
      CHECK_LIMIT();
      (void)mz_finish(clear_rs_arguments);
      jit_retval(JIT_R0);
      __START_TINY_JUMPS__(1);
      mz_patch_branch(ref);
      __END_TINY_JUMPS__(1);
    }
    has_rest = 0;
    if (argc != lam->num_params) {
      argv = NULL;
      argc = 0;
    }
  }

#ifdef USE_FLONUM_UNBOXING
  /* Unpack flonum arguments */
  if (SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_HAS_TYPED_ARGS) {
    GC_CAN_IGNORE jit_insn *zref;
    int f_offset;

    /* In the case of a direct native call, the flonums can be
       already unpacked, in which case JIT_SP is set up. Check whether
       JIT_SP is already different than the 0-flonums case. */
    f_offset = JIT_FRAME_FLOSTACK_OFFSET - jitter->flostack_space;
    jit_subr_p(JIT_R1, JIT_SP, JIT_FP);
    zref = jit_bnei_l(jit_forward(), JIT_R1, f_offset);
        
    for (i = lam->num_params; i--; ) {
      if (CLOSURE_ARGUMENT_IS_FLONUM(lam, i)
          || CLOSURE_ARGUMENT_IS_EXTFLONUM(lam, i)) {
        int extfl;
        extfl = CLOSURE_ARGUMENT_IS_EXTFLONUM(lam, i);
        mz_rs_ldxi(JIT_R1, i);
        MZ_FPUSEL_STMT(extfl,
                       jit_fpu_ldxi_ld_fppush(JIT_FPU_FPR0, JIT_R1, &((Scheme_Long_Double *)0x0)->long_double_val),
                       jit_ldxi_d_fppush(JIT_FPR0, JIT_R1, &((Scheme_Double *)0x0)->double_val));
        scheme_generate_flonum_local_unboxing(jitter, 1, 0, extfl);
        CHECK_LIMIT();
      } else {
        mz_runstack_pushed(jitter, 1);
      }
    }
    jitter->self_pos = 0;
    jitter->depth = 0;

    mz_patch_branch(zref);
  }
#endif

#ifdef JIT_PRECISE_GC
  /* Keeping the native-closure code pointer on the runstack ensures
     that the code won't be GCed while we're running it. If the
     closure is empty, it's ok, faster, and useful to keep it,
     otherwise keep just the code pointer for space safety. */
  if (!lam->closure_size) {
    jitter->closure_self_on_runstack = 1;
    mz_pushr_p(JIT_R0);  /* no sync */
  } else {
    jit_ldxi_p(JIT_R1, JIT_R0, &((Scheme_Native_Closure *)0x0)->code);
    mz_pushr_p(JIT_R1);  /* no sync */
  }
  to_args = 0;
#else
  to_args = 0;
#endif

  specialized = SCHEME_NATIVE_LAMBDA_FLAGS(jitter->nc->code) & NATIVE_SPECIALIZED;

  /* Extract closure to runstack: */
  cnt = lam->closure_size;
  to_args += cnt;
  if (cnt) {
    if (specialized) {
      /* References to closure data will be replaced with values */
    } else {
      mz_rs_dec(cnt);
      CHECK_RUNSTACK_OVERFLOW();
    
      for (i = cnt; i--; ) {
        int pos;
        pos = WORDS_TO_BYTES(i) + (intptr_t)&((Scheme_Native_Closure *)0x0)->vals;
        jit_ldxi_p(JIT_R1, JIT_R0, pos);
        mz_rs_stxi(i, JIT_R1);
        CHECK_LIMIT();
      }
    }
  }

  mz_rs_sync();

  /* If we have a letrec context, record arities */
  if (lam->context && SAME_TYPE(SCHEME_TYPE(lam->context), scheme_letrec_type) && !specialized) {
    Scheme_Letrec *lr = (Scheme_Letrec *)lam->context;
    int pos, self_pos = -1;
    for (i = lam->closure_size; i--; ) {
      pos = lam->closure_map[i];
      if (pos < lr->count) {
	Scheme_Lambda *lam2 = (Scheme_Lambda *)lr->procs[pos];
	mz_runstack_closure_pushed(jitter, 
                                   (lam2->num_params
                                    - ((SCHEME_LAMBDA_FLAGS(lam2) & LAMBDA_HAS_REST)
                                       ? 1
                                       : 0)),
                                   (((SCHEME_LAMBDA_FLAGS(lam2) & LAMBDA_PRESERVES_MARKS)
                                     ? NATIVE_PRESERVES_MARKS
                                     : 0)
                                    | ((SCHEME_LAMBDA_FLAGS(lam2) & LAMBDA_SINGLE_RESULT)
                                     ? NATIVE_IS_SINGLE_RESULT
                                       : 0)));
	if (SAME_OBJ(lr->procs[pos], (Scheme_Object *)lam)) {
          self_pos = i;
	}
      } else {
#ifdef USE_FLONUM_UNBOXING
        if ((SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_HAS_TYPED_ARGS)
            && (CLOSURE_CONTENT_IS_FLONUM(lam, i)
                || CLOSURE_CONTENT_IS_EXTFLONUM(lam, i))) {
          int extfl;
          extfl = CLOSURE_CONTENT_IS_EXTFLONUM(lam, i);
          mz_rs_ldxi(JIT_R1, i);
          MZ_FPUSEL_STMT(extfl,
                         jit_fpu_ldxi_ld_fppush(JIT_FPU_FPR0, JIT_R1, &((Scheme_Long_Double *)0x0)->long_double_val),
                         jit_ldxi_d_fppush(JIT_FPR0, JIT_R1, &((Scheme_Double *)0x0)->double_val));
          scheme_generate_flonum_local_unboxing(jitter, 1, 0, extfl);
          CHECK_LIMIT();
        } else
#endif
          mz_runstack_pushed(jitter, 1);
      }
    }
    if ((self_pos >= 0) && !has_rest) {
      jitter->self_pos = self_pos;
      jitter->self_closure_size = lam->closure_size;
    }
  } else {
#ifdef USE_FLONUM_UNBOXING
    /* Unpack flonum closure data */
    if ((SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_HAS_TYPED_ARGS) && !specialized) {
      for (i = lam->closure_size; i--; ) {
        if (CLOSURE_CONTENT_IS_FLONUM(lam, i)
            || CLOSURE_CONTENT_IS_EXTFLONUM(lam, i)) {
          int extfl;
          extfl = CLOSURE_CONTENT_IS_EXTFLONUM(lam, i);
          mz_rs_ldxi(JIT_R1, i);
          MZ_FPUSEL_STMT(extfl,
                         jit_fpu_ldxi_ld_fppush(JIT_FPU_FPR0, JIT_R1, &((Scheme_Long_Double *)0x0)->long_double_val),
                         jit_ldxi_d_fppush(JIT_FPR0, JIT_R1, &((Scheme_Double *)0x0)->double_val));
          scheme_generate_flonum_local_unboxing(jitter, 1, 0, extfl);
          CHECK_LIMIT();
        } else {
          mz_runstack_pushed(jitter, 1);
        }
      }
    } else
#endif
      {
        if (specialized)
          mz_runstack_skipped(jitter, cnt);
        else
          mz_runstack_pushed(jitter, cnt);
      }
  
    /* A define-values context? */
    if (lam->context && SAME_TYPE(SCHEME_TYPE(lam->context), scheme_toplevel_type)) {
      jitter->self_toplevel_pos = SCHEME_TOPLEVEL_POS(lam->context);
      jitter->self_closure_size = lam->closure_size;
    }
  }

  FOR_LOG({
      char *log_name;
      Scheme_Object *a[1];
      a[0] = lam->name;
      log_name = (lam->name ? scheme_format_utf8("~s", 2, 1, a, NULL) : "???");
      LOG_IT(("PROC: %s, %d args, flags: %x\n", 
              log_name,
              lam->num_params,
              SCHEME_LAMBDA_FLAGS(lam)));
      jitter->log_depth++;
    });
  
  jitter->self_lam = lam;

  jitter->self_restart_code = jit_get_ip();
  jitter->self_restart_space = jitter->flostack_space;
  jitter->self_restart_offset = jitter->flostack_offset;
  if (!has_rest)
    jitter->self_nontail_code = tail_code;

  jitter->self_to_closure_delta = jitter->self_pos;
  jitter->closure_to_args_delta = to_args;
  jitter->example_argc = argc;
  jitter->example_argv = argv;
  jitter->example_argv_delta = argv_delta;

  /* Generate code for the body: */
  jitter->need_set_rs = 1;
  r = scheme_generate(lam->body, jitter, 1, 1, 1, JIT_R0, NULL, NULL); /* no need for sync */
  /* Result is in JIT_R0 */

  CHECK_LIMIT();

  /* r == 2 => tail call performed */
  if (r != 2) {
    scheme_mz_flostack_restore(jitter, 0, 0, 1, 1);
    jit_movr_p(JIT_RET, JIT_R0);
    mz_pop_threadlocal();
    mz_pop_locals();
    jit_ret();
  }

  code_end = jit_get_ip();

  if (jitter->retain_start) {
    gdata->arity_code = arity_code;
    gdata->start_code = start_code;
    gdata->tail_code = tail_code;
    gdata->max_extra = jitter->max_extra_pushed;
    gdata->max_depth = jitter->max_depth;
    gdata->max_tail_depth = jitter->max_tail_depth;
    gdata->code_end = code_end;
    gdata->patch_depth = jitter->patch_depth;
#ifdef NEED_RETAIN_CODE_POINTERS
    if (retain_code)
      ((void **)retain_code)[0] = jit_unadjust_ip(start_code);
    else
      retain_code = jit_unadjust_ip(start_code);
    gdata->retain_code = retain_code;
#endif
  }

  return 1;
}

static void on_demand_generate_lambda(Scheme_Native_Closure *nc, int argc, Scheme_Object **argv, int argv_delta)
{
  Scheme_Native_Lambda *nlam = nc->code;
  Scheme_Lambda *lam;
  Generate_Lambda gdata;
  void *start_code, *tail_code, *arity_code;
  int max_depth;

  lam = nlam->u2.orig_code;
  
  gdata.lam = lam;
  gdata.nc = nc;
  gdata.argc = argc;
  gdata.argv = argv;
  gdata.argv_delta = argv_delta;

  /* This action is not atomic: */
  scheme_delay_load_closure(lam);

  /* So, check again whether we still need to generate: */
  if (nlam->start_code != scheme_on_demand_jit_code)
    return;

  nlam->arity_code = sjc.in_progress_on_demand_jit_arity_code; /* => in progress */

  scheme_generate_one(NULL, do_generate_closure, &gdata, 1, lam->name, nlam);

  if (gdata.max_depth > lam->max_let_depth) {
    scheme_console_printf("Bad max depth! Given %d, counted %d.\n", lam->max_let_depth, gdata.max_depth);
    abort();
  }

  if (SCHEME_NATIVE_LAMBDA_FLAGS(nlam) & NATIVE_SPECIALIZED)
    SCHEME_NATIVE_LAMBDA_FLAGS(nlam) -= NATIVE_SPECIALIZED;

  if (SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_PRESERVES_MARKS)
    SCHEME_NATIVE_LAMBDA_FLAGS(nlam) |= NATIVE_PRESERVES_MARKS;
  if (SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_SINGLE_RESULT)
    SCHEME_NATIVE_LAMBDA_FLAGS(nlam) |= NATIVE_IS_SINGLE_RESULT;

  arity_code = gdata.arity_code;
  start_code = gdata.start_code;
  tail_code = gdata.tail_code;
  
  if (lam->name) {
    scheme_jit_add_symbol((uintptr_t)jit_unadjust_ip(start_code),
                          (uintptr_t)jit_unadjust_ip(gdata.code_end) - 1,
                          lam->name, 1);
  } else {
#ifdef MZ_USE_DWARF_LIBUNWIND
    scheme_jit_add_symbol((uintptr_t)jit_unadjust_ip(start_code),
                          (uintptr_t)jit_unadjust_ip(gdata.code_end) - 1,
                          scheme_null, 1);
#endif
  }
  
  /* Add a couple of extra slots to computed let-depth, as needed
     by various inlined operations. */
# define JIT_RUNSTACK_RESERVE 4
  max_depth = WORDS_TO_BYTES(lam->max_let_depth + gdata.max_extra + JIT_RUNSTACK_RESERVE);
  if (gdata.max_tail_depth > max_depth)
    max_depth = gdata.max_tail_depth;

  /* max_let_depth is used for flags by generate_lambda: */
  if (nlam->max_let_depth & 0x1) {
    lam->body = NULL;
  }
  lam->context = NULL;
  if (nlam->max_let_depth & 0x2) {
    Scheme_Native_Lambda *case_lam;
    case_lam = ((Scheme_Native_Lambda_Plus_Case *)nlam)->case_lam;
    if (case_lam->max_let_depth < max_depth)
      case_lam->max_let_depth = max_depth;
  }

  while (gdata.patch_depth) {
    void **pd;
    pd = (void **)gdata.patch_depth;
    gdata.patch_depth = pd[1];
    jit_patch_movi(((jit_insn *)(*pd)), (void *)(intptr_t)max_depth);
  }

  nlam->start_code = start_code;
  nlam->u.tail_code = tail_code;
  nlam->arity_code = arity_code;
  nlam->u2.name = lam->name;
  /* Let-depth is in bytes instead of words: */
  nlam->max_let_depth = max_depth;
#ifdef NEED_RETAIN_CODE_POINTERS
  nlam->retain_code = gdata.retain_code;
#endif
}

void scheme_on_demand_generate_lambda(Scheme_Native_Closure *nc, int argc, Scheme_Object **argv, int argv_delta)
{
  on_demand_generate_lambda(nc, argc, argv, argv_delta);
}

Scheme_Object **scheme_on_demand_with_args(Scheme_Object **in_argv, Scheme_Object **argv, int argv_delta)
{
  /* On runstack: closure (nearest), argc, probably argv (deepest) */
  Scheme_Object *c, *argc;

  c = in_argv[0];
  argc = in_argv[1];

  if (((Scheme_Native_Closure *)c)->code->start_code == scheme_on_demand_jit_code)
    scheme_on_demand_generate_lambda((Scheme_Native_Closure *)c, SCHEME_INT_VAL(argc), argv, argv_delta);

  return argv;
}

Scheme_Object **scheme_on_demand(Scheme_Object **rs)
{
  return scheme_on_demand_with_args(MZ_RUNSTACK, rs, 0);
}

static Scheme_Native_Lambda *create_native_lambda(Scheme_Lambda *lam, int clear_code_after_jit,
                                                  Scheme_Native_Lambda *case_lam)
{
  Scheme_Native_Lambda *nlam;

  if (!sjc.check_arity_code) {
    /* Create shared code used for stack-overflow handling, etc.: */
    scheme_jit_fill_threadlocal_table();
    scheme_generate_one(NULL, scheme_do_generate_common, NULL, 0, NULL, NULL);
    scheme_generate_one(NULL, scheme_do_generate_more_common, NULL, 0, NULL, NULL);
  }

  if (!case_lam) {
    nlam = MALLOC_ONE_RT(Scheme_Native_Lambda);
#ifdef MZTAG_REQUIRED
    nlam->iso.so.type = scheme_rt_native_code;
#endif
  } else {
    Scheme_Native_Lambda_Plus_Case *nlamp;
    nlamp = MALLOC_ONE_RT(Scheme_Native_Lambda_Plus_Case);
    nlamp->case_lam = case_lam;
    nlam = (Scheme_Native_Lambda *)nlamp;
#ifdef MZTAG_REQUIRED
    nlam->iso.so.type = scheme_rt_native_code_plus_case;
#endif
  }
  nlam->start_code = scheme_on_demand_jit_code;
  nlam->u.tail_code = sjc.on_demand_jit_arity_code;
  nlam->arity_code = sjc.on_demand_jit_arity_code;
  nlam->u2.orig_code = lam;
  nlam->closure_size = lam->closure_size;
  nlam->max_let_depth = (JIT_RUNSTACK_RESERVE * sizeof(void*)) | (case_lam ? 0x2 : 0) | (clear_code_after_jit ? 0x1 : 0);
  nlam->tl_map = lam->tl_map;

#if 0
  /* Compile immediately: */
  on_demand_generate_lambda(nlam);
#endif

  return nlam;
}

Scheme_Native_Lambda *scheme_generate_lambda(Scheme_Lambda *lam, int clear_code_after_jit,
                                             Scheme_Native_Lambda *case_lam)
{
  Scheme_Native_Lambda *nlam;

  nlam = create_native_lambda(lam, clear_code_after_jit, case_lam);

  return nlam;
}

static int generate_simple_arity_check(mz_jit_state *jitter, int num_params, int has_rest, int is_method)
{
  /* JIT_R0 is closure */
  /* JIT_R1 is argc */
  /* JIT_R2 is argv */
  /* If arity matches, JIT_RUNSTACK and JIT_RUNSTACK_BASE should be preserved */
  /* That leaves just JIT_V1 to use if arity is ok. */
  /* This code expects a return context with 3 arguments, so make sure that's
     true dynamically for all jumps to the code. Also, at JIT time, make sure
     that jitter is initialized with a size-3 prolog. */

  GC_CAN_IGNORE jit_insn *ref, *ref2;
  GC_CAN_IGNORE jit_insn *refrts USED_ONLY_FOR_FUTURES;

  __START_TINY_JUMPS__(1);

  if (!has_rest)
    ref = jit_bnei_i(jit_forward(), JIT_R1, num_params);
  else
    ref = jit_blti_i(jit_forward(), JIT_R1, num_params);

  jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure *)0x0)->code);
  jit_ldxi_p(JIT_V1, JIT_V1, &((Scheme_Native_Lambda *)0x0)->u.tail_code);
  jit_jmpr(JIT_V1);
  CHECK_LIMIT();

  /* Failed */
  mz_patch_branch(ref);
  
  /* If argc is negative, this was really a request for arity checking or reporting */
  ref = jit_blti_i(jit_forward(), JIT_R1, 0x0);

  /* Not negative, so report run-time arity mismatch */
  mz_prepare(3);
  jit_pusharg_p(JIT_R2);
  jit_pusharg_i(JIT_R1);
  jit_pusharg_p(JIT_R0);
  CHECK_LIMIT();
  (void)mz_nonrs_finish_lwe(ts_wrong_argument_count, refrts);
  CHECK_LIMIT();

  /* Arity check or reporting. If argv is NULL, it's a reporting request */
  mz_patch_branch(ref);
  ref = jit_beqi_i(jit_forward(), JIT_R2, 0x0);
  
  /* Arity check --- try again with argv cast to argc: */
  jit_subi_i(JIT_R2, JIT_R2, 1);
  if (!has_rest)
    ref2 = jit_bnei_i(jit_forward(), JIT_R2, num_params);
  else
    ref2 = jit_blti_i(jit_forward(), JIT_R2, num_params);
  CHECK_LIMIT();
  jit_movi_i(JIT_RET, 1);
  mz_pop_threadlocal();
  mz_pop_locals();
  jit_ret();
  mz_patch_branch(ref2);
  jit_movi_i(JIT_RET, 0);
  mz_pop_threadlocal();
  mz_pop_locals();
  jit_ret();
  CHECK_LIMIT();

  /* Finally, we know that it was an arity-report request */
  mz_patch_branch(ref);
  if (!has_rest) 
    (void)jit_movi_p(JIT_R0, scheme_make_integer(num_params));
  else
    (void)jit_movi_p(JIT_R0, scheme_make_integer(-(num_params+1)));
  CHECK_LIMIT();
  if (is_method) {
    mz_prepare(1);
    jit_pusharg_p(JIT_R0);
    (void)mz_nonrs_finish_lwe(ts_scheme_box, refrts);
    mz_pop_threadlocal();
    mz_pop_locals();
    jit_ret();
  } else {
    jit_movr_p(JIT_RET, JIT_R0);
    mz_pop_threadlocal();
    mz_pop_locals();
    jit_ret();
  }

  __END_TINY_JUMPS__(1);

  return 1;
}

typedef struct {
  int num_params;
  int has_rest;
  int is_method;
  int gcable;
} Generate_Arity_Check_Data;

static int do_generate_lambda_simple_arity_check(mz_jit_state *jitter, void *_data)
{
  Generate_Arity_Check_Data *data = (Generate_Arity_Check_Data *)_data;
  void *code;
  int r;
  
#ifdef MZ_USE_JIT_PPC
  jitter->js.jitl.nbArgs = 2; /* matches check_arity_code prolog */
#endif

  code = jit_get_ip();

  r = generate_simple_arity_check(jitter, data->num_params, data->has_rest, data->is_method);

  scheme_jit_register_helper_func(jitter, code, data->gcable);

  return r;
}

static void *generate_lambda_simple_arity_check(int num_params, int has_rest, int is_method, int permanent)
{
  Generate_Arity_Check_Data data;

  data.num_params = num_params;
  data.has_rest = has_rest;
  data.is_method = is_method;
  data.gcable = !permanent;

  return scheme_generate_one(NULL, do_generate_lambda_simple_arity_check, &data, !permanent, NULL, NULL);
}

static int generate_case_lambda_dispatch(mz_jit_state *jitter, Scheme_Case_Lambda *c, Scheme_Native_Lambda *nlam,
					 int do_getarg)
{
  /* See top of generate_simple_arity_check for register and other context info. */
  Scheme_Lambda *lam;
  Scheme_Object *o;
  int i, cnt, has_rest, offset, num_params;
  GC_CAN_IGNORE jit_insn *ref = NULL;
  GC_CAN_IGNORE jit_insn *refrts USED_ONLY_FOR_FUTURES;

  cnt = c->count;
  for (i = 0; i < cnt; i++) {
    /* Check arity for this case: */
    o = c->array[i];
    if (SCHEME_PROCP(o))
      o = (Scheme_Object *)((Scheme_Closure *)o)->code;
    lam = (Scheme_Lambda *)o;

    num_params = lam->num_params;
    has_rest = ((SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_HAS_REST) ? 1 : 0);
    if (has_rest && num_params)
      --num_params;

    /* Check for arity match. */
    if (!has_rest)
      ref = jit_bnei_i(jit_forward(), JIT_R1, num_params);
    else
      ref = jit_blti_i(jit_forward(), JIT_R1, num_params);

    /* Function-argument handling for this case: */
    if (do_getarg) {
      generate_function_getarg(jitter, has_rest, num_params + (has_rest ? 1 : 0));
      CHECK_LIMIT();
    }
    
    /* Jump to tail-code location of the selected branch: */
    offset = WORDS_TO_BYTES(i) + (uintptr_t)&((Scheme_Native_Closure *)0x0)->vals;
    jit_ldxi_p(JIT_R0, JIT_R0, offset);
    jit_ldxi_p(JIT_V1, JIT_R0, &((Scheme_Native_Closure *)0x0)->code);
    jit_ldxi_p(JIT_V1, JIT_V1, &((Scheme_Native_Lambda *)0x0)->u.tail_code);
    jit_jmpr(JIT_V1);
    CHECK_LIMIT();
    
    mz_patch_branch(ref);
    /* Try the next one... */
  }

  if (!do_getarg) {
    /* Report run-time arity mismatch */
    JIT_UPDATE_THREAD_RSPTR();
    mz_prepare(3);
    jit_pusharg_p(JIT_R2);
    jit_pusharg_i(JIT_R1);
    jit_pusharg_p(JIT_R0);
    CHECK_LIMIT();
    (void)mz_finish_lwe(ts_wrong_argument_count, refrts);
    CHECK_LIMIT();
  }

  return 1;
}

typedef struct {
  Scheme_Case_Lambda *c;
  Scheme_Native_Lambda *nlam;
  int is_method;
} Generate_Case_Dispatch_Data;

static int do_generate_case_lambda_dispatch(mz_jit_state *jitter, void *_data)
{
  Generate_Case_Dispatch_Data *data = (Generate_Case_Dispatch_Data *)_data;
  void *start_code, *arity_code;

  start_code = jit_get_ip();
  
  scheme_generate_function_prolog(jitter);
  CHECK_LIMIT();
  
  if (generate_case_lambda_dispatch(jitter, data->c, data->nlam, 1)) {
    arity_code = jit_get_ip();
    if (generate_case_lambda_dispatch(jitter, data->c, data->nlam, 0)) {
      data->nlam->start_code = start_code;
      data->nlam->arity_code = arity_code;
#ifdef NEED_RETAIN_CODE_POINTERS
      data->nlam->retain_code = jit_unadjust_ip(start_code);
#endif

      scheme_jit_register_helper_func(jitter, start_code, 1);

      return 1;
    }
  }

  return 0;
}

static void generate_case_lambda(Scheme_Case_Lambda *c, Scheme_Native_Lambda *nlam, int is_method)
{
  Generate_Case_Dispatch_Data gdata;
  Scheme_Lambda *lam;
  Scheme_Object *o;
  int i, cnt, num_params, has_rest, single_result = 1;
  mzshort *arities;

  gdata.c = c;
  gdata.nlam = nlam;
  gdata.is_method = is_method;

  scheme_generate_one(NULL, do_generate_case_lambda_dispatch, &gdata, 1, NULL, nlam);

  /* Generate arity table used by scheme_native_arity_check
     and scheme_get_native_arity: */
  
  cnt = c->count;
  arities = (mzshort *)scheme_malloc_atomic(sizeof(mzshort) * (cnt + 1));
  arities[cnt] = is_method;
  for (i = 0; i < cnt; i++) {
    o = c->array[i];
    if (SCHEME_PROCP(o))
      o = (Scheme_Object *)((Scheme_Closure *)o)->code;
    lam = (Scheme_Lambda *)o;
    num_params = lam->num_params;
    has_rest = ((SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_HAS_REST) ? 1 : 0);
    if (has_rest && num_params)
      --num_params;
    if (!(SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_SINGLE_RESULT))
      single_result = 0;
	  
    if (!has_rest) 
      arities[i] = num_params;
    else
      arities[i] = -(num_params+1);
  }
  nlam->u.arities = arities;

  if (single_result)
    SCHEME_NATIVE_LAMBDA_FLAGS(nlam) |= NATIVE_IS_SINGLE_RESULT;
}

/*========================================================================*/
/*                          native arity queries                          */
/*========================================================================*/

XFORM_NONGCING static int lambda_has_been_jitted(Scheme_Native_Lambda *nlam)
/* called by scheme_native_arity_check(), which is not XFORMed */
{
  return (nlam->start_code != scheme_on_demand_jit_code);
}

int scheme_native_arity_check(Scheme_Object *closure, int argc)
  XFORM_SKIP_PROC /* called in a future thread for `future' argument arity check */
{
  int cnt;

  cnt = ((Scheme_Native_Closure *)closure)->code->closure_size;
  if (cnt < 0) {
    /* Case-lambda */
    int i;
    mzshort *arities, v;

    arities = ((Scheme_Native_Closure *)closure)->code->u.arities;
    cnt = -(cnt + 1);
    for (i = 0; i < cnt; i++) {
      v = arities[i];
      if (v < 0) {
	v = -(v + 1);
	if (argc >= v)
	  return 1;
      } else if (argc == v)
	return 1;
    }
    return 0;
  }

  if (!lambda_has_been_jitted(((Scheme_Native_Closure *)closure)->code)) {
    Scheme_Lambda *lam = ((Scheme_Native_Closure *)closure)->code->u2.orig_code;
    int mina, maxa;
    mina = maxa = lam->num_params;
    if (SCHEME_LAMBDA_FLAGS(lam) & LAMBDA_HAS_REST) {
      if (mina)
	--mina;
      maxa = -1;
    }
    if (argc < mina)
      return 0;
    if ((maxa > -1) && (argc > maxa))
      return 0;
    return 1;
  }

  return sjc.check_arity_code(closure, argc + 1, 0 EXTRA_NATIVE_ARGUMENT);
}

Scheme_Object *scheme_get_native_arity(Scheme_Object *closure, int mode)
{
  int cnt;

  cnt = ((Scheme_Native_Closure *)closure)->code->closure_size;
  if (cnt < 0) {
    /* Case-lambda */
    Scheme_Object *l = scheme_null, *a;
    int i, has_rest, is_method;
    mzshort *arities, v;

    arities = ((Scheme_Native_Closure *)closure)->code->u.arities;
    cnt = -(cnt + 1);
    is_method = arities[cnt];
    for (i = cnt; i--; ) {
      v = arities[i];
      if (v < 0) {
	v = -(v + 1);
	has_rest = 1;
      } else 
	has_rest = 0;
      if (mode == -3) {
        if (has_rest) v = -(v+1);
        a = scheme_make_integer(v);
      } else
        a = scheme_make_arity(v, has_rest ? -1 : v);
      l = scheme_make_pair(a, l);
    }
    if (is_method)
      l = scheme_box(l);
    return l;
  }

  if (!lambda_has_been_jitted(((Scheme_Native_Closure *)closure)->code)) {
    Scheme_Closure c;
    Scheme_Object *a;
    c.so.type = scheme_closure_type;
    c.code = ((Scheme_Native_Closure *)closure)->code->u2.orig_code;
    a = scheme_get_or_check_arity((Scheme_Object *)&c, -1);
    if (SCHEME_LAMBDA_FLAGS(c.code) & LAMBDA_IS_METHOD)
      a = scheme_box(a);
    return a;
  }

  return sjc.get_arity_code(closure, 0, 0 EXTRA_NATIVE_ARGUMENT);
}

/**********************************************************************/
/*                       thread-local table                           */
/**********************************************************************/

void scheme_jit_fill_threadlocal_table() {
}

/**********************************************************************/
/*                           Precise GC                               */
/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_jit.inc"

void scheme_jit_register_traversers(void)
{
  GC_REG_TRAV(scheme_native_closure_type, native_closure);
  GC_REG_TRAV(scheme_rt_jitter_data, mark_jit_state);
  GC_REG_TRAV(scheme_rt_native_code, native_unclosed_proc);
  GC_REG_TRAV(scheme_rt_native_code_plus_case, native_unclosed_proc_plus_case);
}

END_XFORM_SKIP;

#endif /* MZ_PRECISE_GC */

#endif /* MZ_USE_JIT */
