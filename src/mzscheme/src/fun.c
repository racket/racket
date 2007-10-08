/*
  MzScheme
  Copyright (c) 2004-2007 PLT Scheme Inc.
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

/* This file is a hodge-podge of various aspects of application and
   continuations.  It includes primitives like `call/cc' and
   `procedure-arity', which have no better home, as well as parts of
   closure compilation and wrappers for evaluation to handle stack
   overflow and continuation-jump limits. */

#include "schpriv.h"
#include "schexpobs.h"

/* The implementations of the time primitives, such as
   `current-seconds', vary a lot from platform to platform. */
#ifdef TIME_SYNTAX
# ifdef USE_MACTIME
#  include <OSUtils.h>
#  include <Timer.h>
# else
#  ifndef USE_PALMTIME
#   if defined(OSKIT) && !defined(OSKIT_TEST)
    /* Get FreeBSD version, not oskit/time.h version */
#    include <freebsd/time.h>
#   endif
#   include <time.h>
#   ifdef USE_FTIME
#    include <sys/timeb.h>
#   else
#    include <sys/time.h>
#   endif /* USE_FTIME */
#   ifdef USE_GETRUSAGE
#    include <sys/types.h>
#    include <sys/time.h>
#    include <sys/resource.h>
#   endif /* USE_GETRUSAGE */
#   ifdef USE_SYSCALL_GETRUSAGE
#    include <sys/syscall.h>
#    define getrusage(a, b)  syscall(SYS_GETRUSAGE, a, b)
#    define USE_GETRUSAGE
#   endif /* USE_SYSCALL_GETRUSAGE */
#   ifdef WINDOWS_GET_PROCESS_TIMES
#    include <Windows.h>
#   endif
#  endif /* USE_PALMTIME */
# endif /* USE_MACTIME */
#endif /* TIME_SYNTAX */

static void ASSERT_SUSPEND_BREAK_ZERO() {
#if 0
  if (scheme_current_thread->suspend_break)
    abort();
#endif
}

/* globals */
int scheme_defining_primitives; /* set to 1 during start-up */

Scheme_Object scheme_void[1]; /* the void constant */
Scheme_Object *scheme_values_func; /* the function bound to `values' */
Scheme_Object *scheme_procedure_p_proc;
Scheme_Object *scheme_void_proc;
Scheme_Object *scheme_call_with_values_proc; /* the function bound to `call-with-values' */

Scheme_Object *scheme_tail_call_waiting;

Scheme_Object *scheme_inferred_name_symbol;

int scheme_cont_capture_count;
int scheme_prompt_capture_count;

Scheme_Object *scheme_default_prompt_tag;

/* locals */
static Scheme_Object *procedure_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *apply (int argc, Scheme_Object *argv[]);
static Scheme_Object *map (int argc, Scheme_Object *argv[]);
static Scheme_Object *for_each (int argc, Scheme_Object *argv[]);
static Scheme_Object *andmap (int argc, Scheme_Object *argv[]);
static Scheme_Object *ormap (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_cc (int argc, Scheme_Object *argv[]);
static Scheme_Object *internal_call_cc (int argc, Scheme_Object *argv[]);
static Scheme_Object *continuation_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_with_continuation_barrier (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_with_prompt (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_with_control (int argc, Scheme_Object *argv[]);
static Scheme_Object *make_prompt_tag (int argc, Scheme_Object *argv[]);
static Scheme_Object *abort_continuation (int argc, Scheme_Object *argv[]);
static Scheme_Object *continuation_prompt_available(int argc, Scheme_Object *argv[]);
static Scheme_Object *get_default_prompt_tag (int argc, Scheme_Object *argv[]);
static Scheme_Object *prompt_tag_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_with_sema (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_with_sema_enable_break (int argc, Scheme_Object *argv[]);
static Scheme_Object *cc_marks (int argc, Scheme_Object *argv[]);
static Scheme_Object *cont_marks (int argc, Scheme_Object *argv[]);
static Scheme_Object *cc_marks_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *extract_cc_marks (int argc, Scheme_Object *argv[]);
static Scheme_Object *extract_cc_markses (int argc, Scheme_Object *argv[]);
static Scheme_Object *extract_cc_proc_marks (int argc, Scheme_Object *argv[]);
static Scheme_Object *extract_one_cc_mark (int argc, Scheme_Object *argv[]);
static Scheme_Object *void_func (int argc, Scheme_Object *argv[]);
static Scheme_Object *void_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *dynamic_wind (int argc, Scheme_Object *argv[]);
#ifdef TIME_SYNTAX
static Scheme_Object *time_apply(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_milliseconds(int argc, Scheme_Object **argv);
static Scheme_Object *current_inexact_milliseconds(int argc, Scheme_Object **argv);
static Scheme_Object *current_process_milliseconds(int argc, Scheme_Object **argv);
static Scheme_Object *current_gc_milliseconds(int argc, Scheme_Object **argv);
static Scheme_Object *current_seconds(int argc, Scheme_Object **argv);
static Scheme_Object *seconds_to_date(int argc, Scheme_Object **argv);
#endif
static Scheme_Object *object_name(int argc, Scheme_Object *argv[]);
static Scheme_Object *procedure_arity(int argc, Scheme_Object *argv[]);
static Scheme_Object *procedure_arity_includes(int argc, Scheme_Object *argv[]);
static Scheme_Object *procedure_reduce_arity(int argc, Scheme_Object *argv[]);
static Scheme_Object *procedure_equal_closure_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *primitive_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *primitive_closure_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *primitive_result_arity (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_with_values(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_values(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_print(int argc, Scheme_Object **argv);
static Scheme_Object *current_prompt_read(int, Scheme_Object **);

static Scheme_Object *write_compiled_closure(Scheme_Object *obj);
static Scheme_Object *read_compiled_closure(Scheme_Object *obj);

/* Back-door arguments to scheme_top_level_do: */
static int top_next_registered;
static Scheme_Comp_Env *top_next_env;
static Scheme_Object *top_next_mark;
static Scheme_Object *top_next_name;
static Scheme_Object *top_next_certs;
static Scheme_Object *top_next_modidx;
static Scheme_Env *top_next_menv;
static int top_next_use_thread_cc_ok;

static Scheme_Prompt *original_default_prompt; /* for escapes, represents the implicit initial prompt */

static Scheme_Object *certify_mode_symbol, *transparent_symbol, *transparent_binding_symbol, *opaque_symbol;

static Scheme_Object *cont_key, *barrier_prompt_key;

static Scheme_Object *is_method_symbol;

static Scheme_Object *call_with_prompt_proc, *abort_continuation_proc;

static Scheme_Prompt *available_prompt, *available_cws_prompt, *available_regular_prompt;
static Scheme_Dynamic_Wind *available_prompt_dw;
static Scheme_Meta_Continuation *available_prompt_mc;

static Scheme_Object *reduced_procedure_struct;

typedef void (*DW_PrePost_Proc)(void *);

#define CONS(a,b) scheme_make_pair(a,b)

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

static Scheme_Object *internal_call_cc_prim;

/* See call_cc: */
typedef struct Scheme_Dynamic_Wind_List {
  MZTAG_IF_REQUIRED
  Scheme_Dynamic_Wind *dw;
  int meta_depth;
  struct Scheme_Dynamic_Wind_List *next;
} Scheme_Dynamic_Wind_List;

static Scheme_Object *cached_beg_stx, *cached_dv_stx, *cached_ds_stx;
int cached_stx_phase;

/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/

void
scheme_init_fun (Scheme_Env *env)
{
  Scheme_Object *o;

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

#ifdef MZ_APPLY_WAITING_CONSTANT
  scheme_tail_call_waiting = MZ_APPLY_WAITING_CONSTANT;
#else
  REGISTER_SO(scheme_tail_call_waiting);
  scheme_tail_call_waiting = scheme_alloc_eternal_object();
  scheme_tail_call_waiting->type = scheme_tail_call_waiting_type;
#endif

  REGISTER_SO(cached_beg_stx);
  REGISTER_SO(cached_dv_stx);
  REGISTER_SO(cached_ds_stx);
  REGISTER_SO(scheme_procedure_p_proc);

  o = scheme_make_folding_prim(procedure_p, "procedure?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(o) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("procedure?", o, env);

  scheme_procedure_p_proc = o;

  scheme_add_global_constant("apply",
			     scheme_make_prim_w_arity2(apply,
						       "apply",
						       2, -1,
						       0, -1),
			     env);
  scheme_add_global_constant("map",
			     scheme_make_prim_w_arity(map,
						      "map",
						      2, -1),
			     env);
  scheme_add_global_constant("for-each",
			     scheme_make_prim_w_arity(for_each,
						      "for-each",
						      2, -1),
			     env);
  scheme_add_global_constant("andmap",
			     scheme_make_prim_w_arity(andmap,
						      "andmap",
						      2, -1),
			     env);
  scheme_add_global_constant("ormap",
			     scheme_make_prim_w_arity(ormap,
						      "ormap",
						      2, -1),
			     env);

  REGISTER_SO(scheme_call_with_values_proc);
  scheme_call_with_values_proc = scheme_make_prim_w_arity2(call_with_values,
                                                           "call-with-values",
                                                           2, 2,
                                                           0, -1);
  scheme_add_global_constant("call-with-values",
			     scheme_call_with_values_proc,
			     env);

  REGISTER_SO(scheme_values_func);
  scheme_values_func = scheme_make_prim_w_arity2(scheme_values,
						 "values",
						 0, -1,
						 0, -1);
  scheme_add_global_constant("values",
			     scheme_values_func,
			     env);

  o = scheme_make_prim_w_arity2(scheme_call_ec,
				"call-with-escape-continuation",
				1, 1,
				0, -1);
  scheme_add_global_constant("call-with-escape-continuation", o, env);
  scheme_add_global_constant("call/ec", o, env);

  REGISTER_SO(internal_call_cc_prim);
  internal_call_cc_prim = scheme_make_prim_w_arity2(internal_call_cc,
						    "call-with-current-continuation",
						    1, 3,
						    0, -1);

  o = scheme_make_prim_w_arity2(call_cc,
				"call-with-current-continuation",
				1, 2,
				0, -1);

  scheme_add_global_constant("call-with-current-continuation", o, env);
  scheme_add_global_constant("call/cc", o, env);

  scheme_add_global_constant("continuation?",
                             scheme_make_folding_prim(continuation_p,
						      "continuation?",
						      1, 1, 1),
                             env);

  scheme_add_global_constant("call-with-continuation-barrier",
			     scheme_make_prim_w_arity2(call_with_continuation_barrier,
						       "call-with-continuation-barrier",
						       1, 1,
						       0, -1), 
			     env);

  REGISTER_SO(call_with_prompt_proc);
  call_with_prompt_proc = scheme_make_prim_w_arity2(call_with_prompt,
                                                    "call-with-continuation-prompt",
                                                    1, 3,
                                                    0, -1);
  scheme_add_global_constant("call-with-continuation-prompt",
			     call_with_prompt_proc, 
			     env);

  scheme_add_global_constant("call-with-composable-continuation",
			     scheme_make_prim_w_arity2(call_with_control,
                                                       "call-with-composable-continuation",
                                                       1, 2,
                                                       0, -1), 
			     env);

  REGISTER_SO(abort_continuation_proc);
  abort_continuation_proc = scheme_make_prim_w_arity(abort_continuation,
                                                     "abort-current-continuation",
                                                     1, -1);
  scheme_add_global_constant("abort-current-continuation",
			     abort_continuation_proc, 
			     env);

  scheme_add_global_constant("continuation-prompt-available?",
			     scheme_make_prim_w_arity(continuation_prompt_available,
                                                      "continuation-prompt-available?",
                                                      1, 2), 
			     env);

  scheme_add_global_constant("make-continuation-prompt-tag",
			     scheme_make_prim_w_arity(make_prompt_tag,
                                                      "make-continuation-prompt-tag",
                                                      0, 1), 
			     env);

  scheme_add_global_constant("default-continuation-prompt-tag",
                             scheme_make_prim_w_arity(get_default_prompt_tag,
                                                      "default-continuation-prompt-tag",
                                                      0, 0), 
			     env);
  scheme_add_global_constant("continuation-prompt-tag?",
                             scheme_make_folding_prim(prompt_tag_p,
						      "continuation-prompt-tag?",
						      1, 1, 1),
                             env);

  scheme_add_global_constant("call-with-semaphore",
			     scheme_make_prim_w_arity2(call_with_sema,
						       "call-with-semaphore",
						       2, -1,
						       0, -1), 
			     env);
  scheme_add_global_constant("call-with-semaphore/enable-break",
			     scheme_make_prim_w_arity2(call_with_sema_enable_break,
						       "call-with-semaphore/enable-break",
						       2, -1,
						       0, -1),
			     env);

  scheme_add_global_constant("current-continuation-marks",
			     scheme_make_prim_w_arity(cc_marks,
						      "current-continuation-marks",
						      0, 1),
			     env);
  scheme_add_global_constant("continuation-marks",
			     scheme_make_prim_w_arity(cont_marks,
						      "continuation-marks",
						      1, 2),
			     env);
  scheme_add_global_constant("continuation-mark-set->list",
			     scheme_make_prim_w_arity(extract_cc_marks,
						      "continuation-mark-set->list",
						      2, 3),
			     env);
  scheme_add_global_constant("continuation-mark-set->list*",
			     scheme_make_prim_w_arity(extract_cc_markses,
						      "continuation-mark-set->list*",
						      2, 4),
			     env);
  scheme_add_global_constant("continuation-mark-set-first",
			     scheme_make_prim_w_arity(extract_one_cc_mark,
						      "continuation-mark-set-first",
						      2, 4),
			     env);
  scheme_add_global_constant("continuation-mark-set?",
			     scheme_make_prim_w_arity(cc_marks_p,
						      "continuation-mark-set?",
						      1, 1),
			     env);
  scheme_add_global_constant("continuation-mark-set->context",
			     scheme_make_prim_w_arity(extract_cc_proc_marks,
						      "continuation-mark-set->context",
						      1, 1),
			     env);

  REGISTER_SO(scheme_void_proc);
  scheme_void_proc = scheme_make_folding_prim(void_func,
					      "void",
					      0, -1, 1);
  scheme_add_global_constant("void", scheme_void_proc, env);
  scheme_add_global_constant("void?",
			     scheme_make_folding_prim(void_p,
						      "void?",
						      1, 1, 1),
			     env);
#ifdef TIME_SYNTAX
  scheme_add_global_constant("time-apply",
			     scheme_make_prim_w_arity2(time_apply,
						       "time-apply",
						       2, 2,
						       4, 4),
			     env);
  scheme_add_global_constant("current-milliseconds",
			     scheme_make_prim_w_arity(current_milliseconds,
						      "current-milliseconds",
						      0, 0),
			     env);
  scheme_add_global_constant("current-inexact-milliseconds",
			     scheme_make_prim_w_arity(current_inexact_milliseconds,
						      "current-inexact-milliseconds",
						      0, 0),
			     env);
  scheme_add_global_constant("current-process-milliseconds",
			     scheme_make_prim_w_arity(current_process_milliseconds,
						      "current-process-milliseconds",
						      0, 0),
			     env);
  scheme_add_global_constant("current-gc-milliseconds",
			     scheme_make_prim_w_arity(current_gc_milliseconds,
						      "current-gc-milliseconds",
						      0, 0),
			     env);
  scheme_add_global_constant("current-seconds",
			     scheme_make_prim_w_arity(current_seconds,
						      "current-seconds",
						      0, 0),
			     env);
  scheme_add_global_constant("seconds->date",
			     scheme_make_prim_w_arity(seconds_to_date,
						      "seconds->date",
						      1, 1),
			     env);
#endif

  scheme_add_global_constant("dynamic-wind",
			     scheme_make_prim_w_arity(dynamic_wind,
						      "dynamic-wind",
						      3, 3),
			     env);

  scheme_add_global_constant("object-name",
			     scheme_make_folding_prim(object_name,
						      "object-name",
						      1, 1, 1),
			     env);

  scheme_add_global_constant("procedure-arity",
			     scheme_make_folding_prim(procedure_arity,
						      "procedure-arity",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("procedure-arity-includes?",
			     scheme_make_folding_prim(procedure_arity_includes,
						      "procedure-arity-includes?",
						      2, 2, 1),
			     env);
  scheme_add_global_constant("procedure-reduce-arity",
			     scheme_make_prim_w_arity(procedure_reduce_arity,
						      "procedure-reduce-arity",
						      2, 2),
			     env);
  scheme_add_global_constant("procedure-closure-contents-eq?",
			     scheme_make_folding_prim(procedure_equal_closure_p,
						      "procedure-closure-contents-eq?",
						      2, 2, 1),
			     env);

  scheme_add_global_constant("primitive?",
			     scheme_make_folding_prim(primitive_p,
						      "primitive?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("primitive-closure?",
			     scheme_make_folding_prim(primitive_closure_p,
						      "primitive-closure?",
						      1, 1, 1),
			     env);

  scheme_add_global_constant("primitive-result-arity",
			     scheme_make_folding_prim(primitive_result_arity,
						      "primitive-result-arity",
						      1, 1, 1),
			     env);

  scheme_add_global_constant("current-print",
			     scheme_register_parameter(current_print,
						       "current-print",
						       MZCONFIG_PRINT_HANDLER),
			     env);
  scheme_add_global_constant("current-prompt-read",
			     scheme_register_parameter(current_prompt_read,
						       "current-prompt-read",
						       MZCONFIG_PROMPT_READ_HANDLER),
			     env);

  scheme_install_type_writer(scheme_unclosed_procedure_type,
			     write_compiled_closure);
  scheme_install_type_reader(scheme_unclosed_procedure_type,
			     read_compiled_closure);

  REGISTER_SO(is_method_symbol);
  REGISTER_SO(scheme_inferred_name_symbol);
  REGISTER_SO(cont_key);
  is_method_symbol = scheme_intern_symbol("method-arity-error");
  scheme_inferred_name_symbol = scheme_intern_symbol("inferred-name");
  cont_key = scheme_make_symbol("k"); /* uninterned */
  
  REGISTER_SO(scheme_default_prompt_tag);
  {
    Scheme_Object *a[1];
    a[0] = scheme_intern_symbol("default");
    scheme_default_prompt_tag = make_prompt_tag(1, a);
  }

  REGISTER_SO(original_default_prompt);
  original_default_prompt = MALLOC_ONE_TAGGED(Scheme_Prompt);
  original_default_prompt->so.type = scheme_prompt_type;
  original_default_prompt->tag = scheme_default_prompt_tag;
}

Scheme_Object *
scheme_make_void (void)
{
  return scheme_void;
}

/*========================================================================*/
/*                          primitive procedures                          */
/*========================================================================*/

static Scheme_Object *
make_prim_closure(Scheme_Prim *fun, int eternal,
		  const char *name,
		  mzshort mina, mzshort maxa,
		  int flags,
		  mzshort minr, mzshort maxr,
		  int closed, int count, Scheme_Object **vals)
{
  Scheme_Primitive_Proc *prim;
  int hasr, size;

  hasr = ((minr != 1) || (maxr != 1));
  size = (hasr 
	  ? sizeof(Scheme_Prim_W_Result_Arity) 
	  : (closed
	     ? (sizeof(Scheme_Primitive_Closure)
		+ ((count - 1) * sizeof(Scheme_Object *)))
	     : sizeof(Scheme_Primitive_Proc)));

  if (eternal && scheme_starting_up && !closed)
    prim = (Scheme_Primitive_Proc *)scheme_malloc_eternal_tagged(size);
  else
    prim = (Scheme_Primitive_Proc *)scheme_malloc_tagged(size);
  prim->pp.so.type = scheme_prim_type;
  prim->prim_val = (Scheme_Primitive_Closure_Proc *)fun;
  prim->name = name;
  prim->mina = mina;
  if (maxa < 0)
    maxa = SCHEME_MAX_ARGS + 1;
  prim->mu.maxa = maxa;
  prim->pp.flags = (flags
		    | (scheme_defining_primitives ? SCHEME_PRIM_IS_PRIMITIVE : 0)
		    | (hasr ? SCHEME_PRIM_IS_MULTI_RESULT : 0)
		    | (closed ? SCHEME_PRIM_IS_CLOSURE : 0));

  if (hasr) {
    ((Scheme_Prim_W_Result_Arity *)prim)->minr = minr;
    ((Scheme_Prim_W_Result_Arity *)prim)->maxr = maxr;
  }
  if (closed) {
#ifdef MZ_PRECISE_GC
    ((Scheme_Primitive_Closure *)prim)->count = count;
#endif
    memcpy(((Scheme_Primitive_Closure *)prim)->val,
	   vals,
	   count * sizeof(Scheme_Object *));
  }

  return (Scheme_Object *)prim;
}

Scheme_Object *
scheme_make_prim_w_everything(Scheme_Prim *fun, int eternal,
			      const char *name,
			      mzshort mina, mzshort maxa,
			      int flags,
			      mzshort minr, mzshort maxr)
{
  return make_prim_closure(fun, eternal,
			   name,
			   mina, maxa,
			   flags,
			   minr, maxr,
			   0, 0, NULL);
}

Scheme_Object *scheme_make_prim(Scheme_Prim *fun)
{
  return make_prim_closure(fun, 1, NULL, 0, -1, 0, 1, 1,
			   0, 0, NULL);
}

Scheme_Object *
scheme_make_noneternal_prim (Scheme_Prim *fun)
{
  return make_prim_closure(fun, 0, NULL, 0, -1, 0, 1, 1,
			   0, 0, NULL);
}

Scheme_Object *
scheme_make_prim_w_arity(Scheme_Prim *fun, const char *name,
			 mzshort mina, mzshort maxa)
{
  return make_prim_closure(fun, 1, name, mina, maxa, 0, 1, 1,
			   0, 0, NULL);
}

Scheme_Object *
scheme_make_folding_prim(Scheme_Prim *fun, const char *name,
			 mzshort mina, mzshort maxa,
			 short folding)
{
  return make_prim_closure(fun, 1, name, mina, maxa,
			   (folding 
			    ? (SCHEME_PRIM_IS_FOLDING
			       | SCHEME_PRIM_IS_NONCM)
			    : 0),
			   1, 1,
			   0, 0, NULL);
}

Scheme_Object *
scheme_make_noncm_prim(Scheme_Prim *fun, const char *name,
		       mzshort mina, mzshort maxa)
{
  /* A non-cm primitive leaves the mark stack unchanged when it returns,
     it can't return multiple values or a tail call, and it cannot
     use its third argument (i.e., the closure pointer) */
  return make_prim_closure(fun, 1, name, mina, maxa,
			   SCHEME_PRIM_IS_NONCM,
			   1, 1,
			   0, 0, NULL);
}

Scheme_Object *
scheme_make_noneternal_prim_w_arity(Scheme_Prim *fun, const char *name,
				    mzshort mina, mzshort maxa)
{
  return make_prim_closure(fun, 0, name, mina, maxa, 0, 1, 1,
			   0, 0, NULL);
}

Scheme_Object *scheme_make_prim_closure_w_arity(Scheme_Primitive_Closure_Proc *prim,
						int size, Scheme_Object **vals,
						const char *name,
						mzshort mina, mzshort maxa)
{
  return make_prim_closure((Scheme_Prim *)prim, 1, name, mina, maxa, 0, 1, 1,
			   1, size, vals);

}

Scheme_Object *scheme_make_folding_prim_closure(Scheme_Primitive_Closure_Proc *prim,
						int size, Scheme_Object **vals,
						const char *name,
						mzshort mina, mzshort maxa,
						short functional)
{
  return make_prim_closure((Scheme_Prim *)prim, 1, name, mina, maxa,
			   (functional
			    ? SCHEME_PRIM_IS_FOLDING
			    : 0),
			   1, 1,
			   1, size, vals);
}

Scheme_Object *
scheme_make_closed_prim_w_everything(Scheme_Closed_Prim *fun,
				     void *data,
				     const char *name,
				     mzshort mina, mzshort maxa,
				     short folding,
				     mzshort minr, mzshort maxr)
{
  Scheme_Closed_Primitive_Proc *prim;
  int hasr, size;

  hasr = ((minr != 1) || (maxr != 1));
  size = hasr ? sizeof(Scheme_Closed_Prim_W_Result_Arity) : sizeof(Scheme_Closed_Primitive_Proc);

  prim = (Scheme_Closed_Primitive_Proc *)scheme_malloc_tagged(size);

  prim->pp.so.type = scheme_closed_prim_type;
  SCHEME_CLSD_PRIM(prim) = fun;
  SCHEME_CLSD_PRIM_DATA(prim) = data;
  prim->name = name;
  prim->mina = mina;
  prim->maxa = maxa;
  prim->pp.flags = ((folding ? SCHEME_PRIM_IS_FOLDING : 0)
		    | (scheme_defining_primitives ? SCHEME_PRIM_IS_PRIMITIVE : 0)
		    | (hasr ? SCHEME_PRIM_IS_MULTI_RESULT : 0));

  if (hasr) {
    ((Scheme_Closed_Prim_W_Result_Arity *)prim)->minr = minr;
    ((Scheme_Closed_Prim_W_Result_Arity *)prim)->maxr = maxr;
  }

  return (Scheme_Object *)prim;
}

Scheme_Object *
scheme_make_folding_closed_prim(Scheme_Closed_Prim *fun,
				void *data,
				const char *name,
				mzshort mina, mzshort maxa,
				short folding)
{
  return scheme_make_closed_prim_w_everything(fun, data, name, mina, maxa, folding, 1, 1);
}

Scheme_Object *
scheme_make_closed_prim_w_arity(Scheme_Closed_Prim *fun, void *data,
				const char *name, mzshort mina, mzshort maxa)
{
  return scheme_make_closed_prim_w_everything(fun, data, name, mina, maxa, 0, 1, 1);
}

Scheme_Object *
scheme_make_closed_prim(Scheme_Closed_Prim *fun, void *data)
{
  return scheme_make_closed_prim_w_everything(fun, data, NULL, 0, -1, 0, 1, 1);
}

void scheme_prim_is_method(Scheme_Object *o)
{
  if (SCHEME_CLSD_PRIMP(o))
    ((Scheme_Closed_Primitive_Proc *)o)->pp.flags |= SCHEME_PRIM_IS_METHOD;
  else
    ((Scheme_Primitive_Proc *)o)->pp.flags |= SCHEME_PRIM_IS_METHOD;
}

int scheme_has_method_property(Scheme_Object *code)
{
  return SCHEME_TRUEP(scheme_stx_property(code, is_method_symbol, NULL));
}

/*========================================================================*/
/*                  closures (run time and compilation)                   */
/*========================================================================*/

Scheme_Object *
scheme_make_closure(Scheme_Thread *p, Scheme_Object *code, int close)
     /* Creates a closure at run-time (or an empty closure at compile
        time; note that the byte-code marshaller in print.c can handle
        empty closures for that reason). */
{
  Scheme_Closure_Data *data;
  Scheme_Closure *closure;
  GC_CAN_IGNORE Scheme_Object **runstack;
  GC_CAN_IGNORE Scheme_Object **dest;
  GC_CAN_IGNORE mzshort *map;
  int i;

  data = (Scheme_Closure_Data *)code;
  
#ifdef MZ_USE_JIT
  if (data->u.native_code) {
    Scheme_Object *nc;

    nc = scheme_make_native_closure(data->u.native_code);

    if (close) {
      runstack = MZ_RUNSTACK;
      dest = ((Scheme_Native_Closure *)nc)->vals;
      map = data->closure_map;
      i = data->closure_size;
      
      /* Copy data into the closure: */
      while (i--) {
	dest[i] = runstack[map[i]];
      }
    }

    return nc;
  }
#endif

  i = data->closure_size;

  closure = (Scheme_Closure *)
    scheme_malloc_tagged(sizeof(Scheme_Closure)
			 + (i - 1) * sizeof(Scheme_Object *));

  closure->so.type = scheme_closure_type;
  SCHEME_COMPILED_CLOS_CODE(closure) = data;

  if (!close || !i)
    return (Scheme_Object *)closure;

  runstack = MZ_RUNSTACK;
  dest = closure->vals;
  map = data->closure_map;

  /* Copy data into the closure: */
  while (i--) {
    dest[i] = runstack[map[i]];
  }

  return (Scheme_Object *)closure;
}

Scheme_Closure *scheme_malloc_empty_closure()
{
  Scheme_Closure *cl;

  cl = (Scheme_Closure *)scheme_malloc_tagged(sizeof(Scheme_Closure) - sizeof(Scheme_Object *));
  cl->so.type = scheme_closure_type;

  return cl;
}

Scheme_Object *scheme_jit_closure(Scheme_Object *code, Scheme_Object *context)
  /* If lr is supplied as a letrec binding this closure, it may be used
     for JIT compilation. */
{
#ifdef MZ_USE_JIT
  Scheme_Closure_Data *data = (Scheme_Closure_Data *)code, *data2;

  /* We need to cache clones to support multiple references
     to a zero-sized closure in bytecode. We need either a clone
     or native code, and context determines which field is releveant,
     so we put the two possibilities in a union `u'. */

  if (!context)
    data2 = data->u.jit_clone;
  else
    data2 = NULL;

  if (!data2) {
    Scheme_Native_Closure_Data *ndata;
    
    data2 = MALLOC_ONE_TAGGED(Scheme_Closure_Data);
    memcpy(data2, code, sizeof(Scheme_Closure_Data));

    data2->context = context;

    ndata = scheme_generate_lambda(data2, 1, NULL);
    data2->u.native_code = ndata;

    if (!context)
      data->u.jit_clone = data2;
  }      
    
  /* If it's zero-sized, then create closure now */
  if (!data2->closure_size)
    return scheme_make_native_closure(data2->u.native_code);

  return (Scheme_Object *)data2;
#endif

  return code;
}

/* Closure_Info is used to store extra closure information
   before a closure mapping is resolved. */
typedef struct {
  MZTAG_IF_REQUIRED
  int *local_flags;
  mzshort base_closure_size; /* doesn't include top-level (if any) */
  mzshort *base_closure_map;
  short has_tl, body_size;
} Closure_Info;

Scheme_Object *
scheme_optimize_closure_compilation(Scheme_Object *_data, Optimize_Info *info)
{
  Scheme_Closure_Data *data;
  Scheme_Object *code;
  Closure_Info *cl;
  mzshort dcs, *dcm;
  int i;

  data = (Scheme_Closure_Data *)_data;

  info->single_result = 1;
  info->preserves_marks = 1;

  info = scheme_optimize_info_add_frame(info, data->num_params, data->num_params,
					SCHEME_LAMBDA_FRAME);

  cl = (Closure_Info *)data->closure_map;
  for (i = 0; i < data->num_params; i++) {
    if (cl->local_flags[i] & SCHEME_WAS_SET_BANGED)
      scheme_optimize_mutated(info, i);
  }

  code = scheme_optimize_expr(data->code, info);

  if (info->single_result)
    SCHEME_CLOSURE_DATA_FLAGS(data) |= CLOS_SINGLE_RESULT;
  else if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_SINGLE_RESULT)
    SCHEME_CLOSURE_DATA_FLAGS(data) -= CLOS_SINGLE_RESULT;

  if (info->preserves_marks)
    SCHEME_CLOSURE_DATA_FLAGS(data) |= CLOS_PRESERVES_MARKS;
  else if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_PRESERVES_MARKS)
    SCHEME_CLOSURE_DATA_FLAGS(data) -= CLOS_PRESERVES_MARKS;

  data->code = code;

  /* Remembers positions of used vars (and unsets usage for this level) */
  scheme_env_make_closure_map(info, &dcs, &dcm);
  cl->base_closure_size = dcs;
  cl->base_closure_map = dcm;
  if (scheme_env_uses_toplevel(info))
    cl->has_tl = 1;
  else
    cl->has_tl = 0;
  cl->body_size = info->size;

  info->size++;
  info->inline_fuel++;

  data->closure_size = (cl->base_closure_size
			+ (cl->has_tl ? 1 : 0));

  scheme_optimize_info_done(info);

  return (Scheme_Object *)data;
}

Scheme_Object *scheme_clone_closure_compilation(int dup_ok, Scheme_Object *_data, Optimize_Info *info, int delta, int closure_depth)
{
  Scheme_Closure_Data *data, *data2;
  Scheme_Object *body;
  Closure_Info *cl;
  int *flags, sz;

  data = (Scheme_Closure_Data *)_data;
  
  body = scheme_optimize_clone(dup_ok, data->code, info, delta, closure_depth + data->num_params);
  if (!body) return NULL;

  data2 = MALLOC_ONE_TAGGED(Scheme_Closure_Data);
  memcpy(data2, data, sizeof(Scheme_Closure_Data));

  data2->code = body;

  cl = MALLOC_ONE_RT(Closure_Info);
  memcpy(cl, data->closure_map, sizeof(Closure_Info));
  data2->closure_map = (mzshort *)cl;

  sz = sizeof(int) * data2->num_params;
  flags = (int *)scheme_malloc_atomic(sz);
  memcpy(flags, cl->local_flags, sz);
  cl->local_flags = flags;

  return (Scheme_Object *)data2;
}

Scheme_Object *scheme_shift_closure_compilation(Scheme_Object *_data, int delta, int after_depth)
{
  Scheme_Object *expr;
  Scheme_Closure_Data *data = (Scheme_Closure_Data *)_data;

  expr = scheme_optimize_shift(data->code, delta, after_depth + data->num_params);
  data->code = expr;

  return _data;
}

int scheme_closure_body_size(Scheme_Closure_Data *data, int check_assign)
{
  int i;
  Closure_Info *cl;

  cl = (Closure_Info *)data->closure_map;

  if (check_assign) {
    /* Don't try to inline if there's a rest arg: */
    if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST)
      return -1;
    
    /* Don't try to inline if any arguments are mutated: */
    for (i = data->num_params; i--; ) {
      if (cl->local_flags[i] & SCHEME_WAS_SET_BANGED)
	return -1;
    }
  }

  return cl->body_size;
}

int scheme_closure_has_top_level(Scheme_Closure_Data *data)
{
  Closure_Info *cl;

  cl = (Closure_Info *)data->closure_map;

  return cl->has_tl;
}

int scheme_closure_argument_flags(Scheme_Closure_Data *data, int i)
{
  return ((Closure_Info *)data->closure_map)->local_flags[i];
}

XFORM_NONGCING static int boxmap_size(int n)
{
  return (n + (BITS_PER_MZSHORT - 1)) / BITS_PER_MZSHORT;
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

XFORM_NONGCING static void boxmap_set(mzshort *boxmap, int j)
{
  boxmap[j / BITS_PER_MZSHORT] |= ((mzshort)1 << (j & (BITS_PER_MZSHORT - 1)));
}

XFORM_NONGCING static int boxmap_get(mzshort *boxmap, int j)
{
  if (boxmap[j / BITS_PER_MZSHORT] & ((mzshort)1 << (j & (BITS_PER_MZSHORT - 1))))
    return 1;
  else
    return 0;
}

Scheme_Object *
scheme_resolve_closure_compilation(Scheme_Object *_data, Resolve_Info *info, 
                                   int can_lift, int convert, int just_compute_lift,
                                   Scheme_Object *precomputed_lift)
{
  Scheme_Closure_Data *data;
  int i, closure_size, offset, np, num_params;
  int has_tl, convert_size, need_lift;
  mzshort *oldpos, *closure_map;
  Closure_Info *cl;
  Resolve_Info *new_info;
  Scheme_Object *lifted, *result, *lifteds = NULL;
  Scheme_Hash_Table *captured = NULL;
  mzshort *convert_map, *convert_boxes = NULL;

  data = (Scheme_Closure_Data *)_data;
  cl = (Closure_Info *)data->closure_map;
  if (!just_compute_lift)
    data->iso.so.type = scheme_unclosed_procedure_type;

  if (convert || can_lift) {
    if (!scheme_resolving_in_procedure(info)) {
      convert = 0;
      can_lift = 0;
    }
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
  closure_map = (mzshort *)scheme_malloc_atomic(sizeof(mzshort) * closure_size);

  has_tl = cl->has_tl;
  if (has_tl && !can_lift)
    convert = 0;

  /* Locals in closure are first: */
  oldpos = cl->base_closure_map;
  offset = 0;
  for (i = 0; i < cl->base_closure_size; i++) {
    int li, flags;
    li = scheme_resolve_info_lookup(info, oldpos[i], &flags, &lifted, 0);
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
      if (convert && (flags & SCHEME_INFO_BOXED)) {
        /* The only problem with a boxed variable is that
           it's more difficult to validate. We have to track
           which arguments are boxes. And the resulting procedure
           must be used only in application positions. */
        if (!convert_boxes)
          convert_boxes = allocate_boxmap(cl->base_closure_size);
        boxmap_set(convert_boxes, offset);
      }
      offset++;
    }
  }

  /* Add bindings introduced by closure conversion. The `captured'
     table maps old positions to new positions. */
  while (lifteds) {
    int j, cnt, boxed;
    Scheme_Object *vec, *loc;

    if (!captured) {
      captured = scheme_make_hash_table(SCHEME_hash_ptr);
      for (i = 0; i < offset; i++) {
        int cp;
        cp = i;
        if (convert_boxes && boxmap_get(convert_boxes, i))
          cp = -(cp + 1);
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
        boxed = 1;
      } else
        boxed = 0;
      i = SCHEME_LOCAL_POS(loc);
      if (!scheme_hash_get(captured, scheme_make_integer(i))) {
        /* Need to capture an extra binding: */
        int cp;
        cp = captured->count;
        if (boxed)
          cp = -(cp + 1);
        scheme_hash_set(captured, scheme_make_integer(i), scheme_make_integer(cp));
      }
    }

    lifteds = SCHEME_CDR(lifteds);
  }

  if (captured && (captured->count > offset)) {
    /* We need to extend the closure map.  All the info
       is in captured, so just build it from scratch. */
    int old_pos, j;
    closure_map = (mzshort *)scheme_malloc_atomic(sizeof(mzshort) * (captured->count + (has_tl ? 1 : 0)));
    offset = captured->count;
    convert_boxes = NULL;
    for (j = captured->size; j--; ) {
      if (captured->vals[j]) {
        int cp;
        cp = SCHEME_INT_VAL(captured->vals[j]);
        old_pos = SCHEME_INT_VAL(captured->keys[j]);
        if (cp < 0) {
          /* Boxed */
          cp = -(cp + 1);
          if (!convert_boxes)
            convert_boxes = allocate_boxmap(offset);
          boxmap_set(convert_boxes, cp);
        }
        closure_map[cp] = old_pos;
      }
    }
  }

  if (convert
      && (offset || !has_tl) /* either need args, or treat as convert becasue it's fully closed */
      ) {
    /* Take over closure_map to be the convert map, instead. */
    int new_boxes_size;

    convert_map = closure_map;
    convert_size = offset;

    if (convert_boxes)
      new_boxes_size = boxmap_size(convert_size + data->num_params);
    else
      new_boxes_size = 0;

    if (has_tl || convert_boxes) {
      int sz;
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
       for "GLOBAL ASSUMPTION" in jit.c */
    int li;
    li = scheme_resolve_toplevel_pos(info);
    closure_map[offset] = li;
    offset++;
  }

  /* Reset closure_size, in case a lifted variable was removed: */
  closure_size = offset;
  if (!just_compute_lift) {
    data->closure_size = closure_size;
    if (convert && convert_boxes)
      SCHEME_CLOSURE_DATA_FLAGS(data) |= CLOS_HAS_REF_ARGS;
  }

  /* Set up environment mapping, initialized for arguments: */

  np = num_params = data->num_params;
  if ((data->num_params == 1)
      && (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST)
      && !(cl->local_flags[0] & SCHEME_WAS_USED)
      && !convert) {
    /* (lambda args E) where args is not in E => drop the argument */
    new_info = scheme_resolve_info_extend(info, 0, 1, cl->base_closure_size);
    num_params = 0;
    if (!just_compute_lift)
      data->num_params = 0;
  } else {
    new_info = scheme_resolve_info_extend(info, data->num_params, data->num_params,
					  cl->base_closure_size + data->num_params);
    for (i = 0; i < data->num_params; i++) {
      scheme_resolve_info_add_mapping(new_info, i, i + closure_size + convert_size,
                                      ((cl->local_flags[i] & SCHEME_WAS_SET_BANGED)
                                       ? SCHEME_INFO_BOXED
                                       : 0),
                                      NULL);
    }
  }

  /* Extend mapping to go from old locations on the stack (as if bodies were
     evaluated immediately) to new locations (where closures
     effectively shift and compact values on the stack). 

     We don't have to include bindings added because an oiriginal
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

    flags = scheme_resolve_info_flags(info, oldpos[i], &lifted);

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
        loc = SCHEME_VEC_ELS(vec)[j+1];
        if (SCHEME_BOXP(loc)) {
          if (!boxmap)
            boxmap = allocate_boxmap(sz);
          boxmap_set(boxmap, j);
          loc = SCHEME_BOX_VAL(loc);
        }
        loc = scheme_hash_get(captured, scheme_make_integer(SCHEME_LOCAL_POS(loc)));
        cp = SCHEME_INT_VAL(loc);
        if (cp < 0)
          cp = -(cp + 1);
        cmap[j] = cp + (has_tl && convert ? 1 : 0);
      }

      ca = MALLOC_N(Scheme_Object *, 4);
      ca[0] = scheme_make_integer(sz);
      ca[1] = (Scheme_Object *)cmap;
      ca[2] = SCHEME_VEC_ELS(vec)[0];
      ca[3] = (Scheme_Object *)boxmap;
      
      lifted = scheme_make_raw_pair(SCHEME_CAR(lifted), (Scheme_Object *)ca);
    }

    scheme_resolve_info_add_mapping(new_info, p, lifted ? 0 : offset++, flags, lifted);
  }
  if (has_tl) {
    if (convert)
      offset = 0; /* other closure elements converted to arguments */
    else
      offset = closure_size - 1;
    scheme_resolve_info_set_toplevel_pos(new_info, offset);
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

    /* Add code to box set!ed argument variables: */
    for (i = 0; i < num_params; i++) {
      if (cl->local_flags[i] & SCHEME_WAS_SET_BANGED) {
        int j = i + closure_size + convert_size;
        Scheme_Object *bcode;
        
        bcode = scheme_make_syntax_resolved(BOXENV_EXPD,
                                            scheme_make_pair(scheme_make_integer(j),
                                                             data->code));
        data->code = bcode;
      }
    }
    
    if (SCHEME_TYPE(data->code) > _scheme_compiled_values_types_)
      SCHEME_CLOSURE_DATA_FLAGS(data) |= CLOS_FOLDABLE;
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
        result = scheme_resolve_invent_toplevel(info);
      else
        result = scheme_resolve_generate_stub_lift();
    } else {
      Scheme_Object *tl, *defn_tl;
      if (precomputed_lift) {
        tl = precomputed_lift;
        if (SCHEME_RPAIRP(tl))
          tl = SCHEME_CAR(tl);
      } else {
        tl = scheme_resolve_invent_toplevel(info);
      }
      defn_tl = scheme_resolve_invented_toplevel_to_defn(info, tl);
      scheme_resolve_lift_definition(info, defn_tl, result);
      if (has_tl)
        closure_map[0] = 0; /* globals for closure creation will be at 0 after lifting */
      result = tl;
    }
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

Scheme_Object *scheme_source_to_name(Scheme_Object *code)
     /* Makes up a procedure name when there's not a good one in the source: */
{
  Scheme_Stx *cstx = (Scheme_Stx *)code;
  if ((cstx->srcloc->col >= 0) || (cstx->srcloc->pos >= 0)) {
    char buf[50], src[20];
    Scheme_Object *name;

    if (cstx->srcloc->src && SCHEME_PATHP(cstx->srcloc->src)) {
      if (SCHEME_BYTE_STRLEN_VAL(cstx->srcloc->src) < 20)
	memcpy(src, SCHEME_BYTE_STR_VAL(cstx->srcloc->src), SCHEME_BYTE_STRLEN_VAL(cstx->srcloc->src) + 1);
      else {
	memcpy(src, SCHEME_BYTE_STR_VAL(cstx->srcloc->src) + SCHEME_BYTE_STRLEN_VAL(cstx->srcloc->src) - 19, 20);
	src[0] = '.';
	src[1] = '.';
	src[2] = '.';
      }
    } else {
      return NULL;
    }

    if (cstx->srcloc->line >= 0) {
      sprintf(buf, "%s%s%ld:%ld",
	      src, (src[0] ? ":" : ""), cstx->srcloc->line, cstx->srcloc->col - 1);
    } else {
      sprintf(buf, "%s%s%ld",
	      src, (src[0] ? "::" : ""), cstx->srcloc->pos);
    }

    name = scheme_intern_exact_symbol(buf, strlen(buf));
    return name;
  }

  return NULL;
}

Scheme_Object *combine_name_with_srcloc(Scheme_Object *name, Scheme_Object *code, int src_based_name)
{
  Scheme_Stx *cstx = (Scheme_Stx *)code;

  if (((cstx->srcloc->col >= 0) || (cstx->srcloc->pos >= 0))
      && cstx->srcloc->src) {
    Scheme_Object *vec;
    vec = scheme_make_vector(7, NULL);
    SCHEME_VEC_ELS(vec)[0] = name;
    SCHEME_VEC_ELS(vec)[1] = cstx->srcloc->src;
    if (cstx->srcloc->line >= 0) {
      SCHEME_VEC_ELS(vec)[2] = scheme_make_integer(cstx->srcloc->line);
      SCHEME_VEC_ELS(vec)[3] = scheme_make_integer(cstx->srcloc->col-1);
    } else {
      SCHEME_VEC_ELS(vec)[2] = scheme_false;
      SCHEME_VEC_ELS(vec)[3] = scheme_false;
    }
    if (cstx->srcloc->pos >= 0)
      SCHEME_VEC_ELS(vec)[4] = scheme_make_integer(cstx->srcloc->pos);
    else
      SCHEME_VEC_ELS(vec)[4] = scheme_false;
    if (cstx->srcloc->span >= 0)
      SCHEME_VEC_ELS(vec)[5] = scheme_make_integer(cstx->srcloc->span);
    else
      SCHEME_VEC_ELS(vec)[5] = scheme_false;
    SCHEME_VEC_ELS(vec)[6] = (src_based_name ? scheme_true : scheme_false);
    
    return vec;
  }

  return name;
}

Scheme_Object *scheme_build_closure_name(Scheme_Object *code, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *name;

  name = scheme_stx_property(code, scheme_inferred_name_symbol, NULL);
  if (name && SCHEME_SYMBOLP(name)) {
    name = combine_name_with_srcloc(name, code, 0);
  } else {
    name = rec[drec].value_name;
    if (!name || SCHEME_FALSEP(name)) {
      name = scheme_source_to_name(code);
      if (name)
	name = combine_name_with_srcloc(name, code, 1);
    } else {
      name = combine_name_with_srcloc(name, code, 0);
    }
  }
  return name;
}

Scheme_Object *
scheme_make_closure_compilation(Scheme_Comp_Env *env, Scheme_Object *code,
				Scheme_Compile_Info *rec, int drec)
     /* Compiles a `lambda' expression */
{
  Scheme_Object *allparams, *params, *forms, *param, *name;
  Scheme_Closure_Data *data;
  Scheme_Compile_Info lam;
  Scheme_Comp_Env *frame;
  int i;
  long num_params;
  Closure_Info *cl;

  data  = MALLOC_ONE_TAGGED(Scheme_Closure_Data);

  data->iso.so.type = scheme_compiled_unclosed_procedure_type;

  params = SCHEME_STX_CDR(code);
  params = SCHEME_STX_CAR(params);
  allparams = params;

  num_params = 0;
  for (; SCHEME_STX_PAIRP(params); params = SCHEME_STX_CDR(params)) {
    num_params++;
  }
  SCHEME_CLOSURE_DATA_FLAGS(data) = 0;
  if (!SCHEME_STX_NULLP(params)) {
    SCHEME_CLOSURE_DATA_FLAGS(data) |= CLOS_HAS_REST;
    num_params++;
  }
  data->num_params = num_params;
  if ((data->num_params > 0) && scheme_has_method_property(code))
    SCHEME_CLOSURE_DATA_FLAGS(data) |= CLOS_IS_METHOD;

  forms = SCHEME_STX_CDR(code);
  forms = SCHEME_STX_CDR(forms);

  frame = scheme_new_compilation_frame(data->num_params, SCHEME_LAMBDA_FRAME, env, rec[drec].certs);
  params = allparams;
  for (i = 0; i < data->num_params; i++) {
    if (!SCHEME_STX_PAIRP(params))
      param = params;
    else
      param = SCHEME_STX_CAR(params);
    scheme_add_compilation_binding(i, param, frame);
    if (SCHEME_STX_PAIRP(params))
      params = SCHEME_STX_CDR (params);
  }

  if (SCHEME_STX_NULLP(forms))
    scheme_wrong_syntax(NULL, NULL, code, "bad syntax (empty body)");

  forms = scheme_datum_to_syntax(forms, code, code, 0, 0);
  forms = scheme_add_env_renames(forms, frame, env);

  name = scheme_build_closure_name(code, rec, drec);
  data->name = name;

  scheme_compile_rec_done_local(rec, drec);

  scheme_init_lambda_rec(rec, drec, &lam, 0);

  {
    Scheme_Object *datacode;
    datacode = scheme_compile_sequence(forms,
				       scheme_no_defines(frame),
				       &lam, 0);
    data->code = datacode;
  }

  scheme_merge_lambda_rec(rec, drec, &lam, 0);

  cl = MALLOC_ONE_RT(Closure_Info);
#ifdef MZTAG_REQUIRED
  cl->type = scheme_rt_closure_info;
#endif
  {
    int *local_flags;
    local_flags = scheme_env_get_flags(frame, 0, data->num_params);
    cl->local_flags = local_flags;
  }
  data->closure_map = (mzshort *)cl;

  return (Scheme_Object *)data;
}


/*========================================================================*/
/*                            prompt helpers                              */
/*========================================================================*/

static void initialize_prompt(Scheme_Thread *p, Scheme_Prompt *prompt, void *stack_boundary)
{
  prompt->is_barrier = 0;
  prompt->stack_boundary = stack_boundary;
  prompt->runstack_boundary_start = MZ_RUNSTACK_START;
  prompt->runstack_boundary_offset = (MZ_RUNSTACK - MZ_RUNSTACK_START);
  prompt->mark_boundary = MZ_CONT_MARK_STACK;
  prompt->boundary_mark_pos = MZ_CONT_MARK_POS;
}

/*========================================================================*/
/*                         stack-overflow wrapper                         */
/*========================================================================*/

typedef Scheme_Object *(*Overflow_K_Proc)(void);

Scheme_Overflow_Jmp *scheme_overflow_jmp;
void *scheme_overflow_stack_start;

/* private, but declared public to avoid inlining: */
void scheme_really_create_overflow(void *stack_base)
{
  Scheme_Overflow_Jmp *jmp;

  /* Even if we already have scheme_overflow_jmp,
     it's possible that we're starting at a lower stack position
     than previously. It's important (for non-3m) to declare
     the new stack start: */
  scheme_ensure_stack_start(stack_base);

  if (scheme_overflow_jmp)
    return;

  scheme_overflow_stack_start = stack_base;

  jmp = MALLOC_ONE_RT(Scheme_Overflow_Jmp);
#ifdef MZTAG_REQUIRED
  jmp->type = scheme_rt_overflow_jmp;
#endif

  scheme_init_jmpup_buf(&jmp->cont);
  if (scheme_setjmpup(&jmp->cont, jmp, stack_base)) {
    /* A jump into here is a request to handle overflow.
       The way to continue is in scheme_overflow_k.
       When we get back, put the result into
       scheme_overflow_reply. The route to return is
       in the thread's `overflow' field. */
    Scheme_Thread * volatile p;
    Scheme_Overflow * volatile overflow;
    mz_jmp_buf nestedbuf;

    p = scheme_current_thread;
    overflow = p->overflow;

    overflow->jmp->savebuf = p->error_buf;
    p->error_buf = &nestedbuf;
    if (scheme_setjmp(nestedbuf)) {
      /* there was an escape from the overflow */
      p = scheme_current_thread;
      p->overflow_reply = NULL; /* means "continue the error" */
    } else {
      void *p1, *p2, *p3, *p4, *p5;
      long i1, i2, i3, i4;
      Overflow_K_Proc f = scheme_overflow_k;
      Scheme_Object *reply;

      p1 = p->ku.k.p1;
      p2 = p->ku.k.p2;
      p3 = p->ku.k.p3;
      p4 = p->ku.k.p4;
      p5 = p->ku.k.p5;
      i1 = p->ku.k.i1;
      i2 = p->ku.k.i2;
      i3 = p->ku.k.i3;
      i4 = p->ku.k.i4;

      /* stack overflow is a lot of work; force a sleep */
      scheme_thread_block(0);
      p->ran_some = 1;

      p->ku.k.p1 = p1;
      p->ku.k.p2 = p2;
      p->ku.k.p3 = p3;
      p->ku.k.p4 = p4;
      p->ku.k.p5 = p5;
      p->ku.k.i1 = i1;
      p->ku.k.i2 = i2;
      p->ku.k.i3 = i3;
      p->ku.k.i4 = i4;

      reply = f();
      scheme_overflow_reply = reply;
    }

    p = scheme_current_thread;
    overflow = p->overflow;
    p->stack_start = overflow->stack_start;

    /* Reset overflow buffer and continue */
    scheme_longjmpup(&overflow->jmp->cont);
  }

  if (scheme_overflow_jmp) {
    scheme_signal_error("shouldn't get here!");
  }

  scheme_overflow_jmp = jmp;
}

void scheme_create_overflow(void)
{
  void *dummy;
  scheme_really_create_overflow(PROMPT_STACK(dummy));
  dummy = NULL; /* to ensure that we get __gc_var_stack__ in 3m */
}

void scheme_init_overflow(void)
{
  REGISTER_SO(scheme_overflow_jmp);
}

void scheme_reset_overflow(void)
{
  scheme_overflow_jmp = NULL;
}

/*========================================================================*/
/*                       entry continuation barrier                       */
/*========================================================================*/

void scheme_on_next_top(Scheme_Comp_Env *env, Scheme_Object *mark, 
			Scheme_Object *name, 
			Scheme_Object *certs, 
			Scheme_Env *menv,
			Scheme_Object *modidx)
     /* Set back-door arguments for scheme_top_level_do */
{
  if (!top_next_registered) {
    top_next_registered = 1;
    REGISTER_SO(top_next_env);
    REGISTER_SO(top_next_mark);
    REGISTER_SO(top_next_name);
    REGISTER_SO(top_next_certs);
    REGISTER_SO(top_next_modidx);
    REGISTER_SO(top_next_menv);
  }

  top_next_env = env;
  top_next_mark = mark;
  top_next_name = name;
  top_next_certs = certs;
  top_next_modidx = modidx;
  top_next_menv = menv;
}

void *top_level_do(void *(*k)(void), int eb, void *sj_start)
     /* Wraps a function `k' with a handler for stack overflows and
	barriers to full-continuation jumps. No barrier if !eb. */
{
  void *v;
  Scheme_Prompt * volatile prompt;
  volatile long save_list_stack_pos;
  mz_jmp_buf *save, newbuf;
  Scheme_Stack_State envss;
  Scheme_Comp_Env * volatile save_current_local_env;
  Scheme_Object * volatile save_mark, *  volatile save_name, * volatile save_certs, * volatile save_modidx;
  Scheme_Env * volatile save_menv;
  Scheme_Simple_Object * volatile save_list_stack;
  Scheme_Thread * volatile p = scheme_current_thread;
  int thread_cc = top_next_use_thread_cc_ok;
  volatile int old_pcc = scheme_prompt_capture_count;
  Scheme_Cont_Frame_Data cframe;
#ifdef MZ_PRECISE_GC
  void *external_stack;
#endif

  top_next_use_thread_cc_ok = 0;

  if (scheme_active_but_sleeping)
    scheme_wake_up();

  if (eb) {
    if (available_prompt) {
      prompt = available_prompt;
      available_prompt = NULL;
    } else  {
      prompt = MALLOC_ONE_TAGGED(Scheme_Prompt);
      prompt->so.type = scheme_prompt_type;
    }

    initialize_prompt(p, prompt, PROMPT_STACK(prompt));
    if (!thread_cc) {
      prompt->is_barrier = 1;
    }

    if (!barrier_prompt_key) {
      REGISTER_SO(barrier_prompt_key);
      barrier_prompt_key = scheme_make_symbol("bar"); /* ininterned */
    }
  } else {
    prompt = NULL;
  }

#ifdef MZ_PRECISE_GC
  if (scheme_get_external_stack_val)
    external_stack = scheme_get_external_stack_val();
  else
    external_stack = NULL;
#endif

  scheme_save_env_stack_w_thread(envss, p);

  save_current_local_env = p->current_local_env;
  save_mark = p->current_local_mark;
  save_name = p->current_local_name;
  save_certs = p->current_local_certs;
  save_modidx = p->current_local_modidx;
  save_menv = p->current_local_menv;
  save_list_stack = p->list_stack;
  save_list_stack_pos = p->list_stack_pos;

  if (top_next_env) {
    p->current_local_env = top_next_env;
    p->current_local_mark = top_next_mark;
    p->current_local_name = top_next_name;
    p->current_local_certs = top_next_certs;
    p->current_local_modidx = top_next_modidx;
    p->current_local_menv = top_next_menv;
    top_next_env = NULL;
    top_next_mark = NULL;
    top_next_name = NULL;
    top_next_certs = NULL;
    top_next_modidx = NULL;
    top_next_menv = NULL;
  }

  scheme_create_overflow(); /* needed even if scheme_overflow_jmp is already set */

  if (prompt) {
    scheme_push_continuation_frame(&cframe);
    scheme_set_cont_mark(barrier_prompt_key, (Scheme_Object *)prompt);
  }

  save = p->error_buf;
  p->error_buf = &newbuf;

  if (scheme_setjmp(newbuf)) {
    if (!thread_cc) {
      p = scheme_current_thread;
      scheme_restore_env_stack_w_thread(envss, p);
#ifdef MZ_PRECISE_GC
      if (scheme_set_external_stack_val)
        scheme_set_external_stack_val(external_stack);
#endif
      if (prompt) {
        scheme_pop_continuation_frame(&cframe);
        if (old_pcc == scheme_prompt_capture_count) {
          /* It wasn't used */
          available_prompt = prompt;
        }
      }
      p->current_local_env = save_current_local_env;
      p->current_local_mark = save_mark;
      p->current_local_name = save_name;
      p->current_local_certs = save_certs;
      p->current_local_modidx = save_modidx;
      p->current_local_menv = save_menv;
      p->list_stack = save_list_stack;
      p->list_stack_pos = save_list_stack_pos;
    }
    scheme_longjmp(*save, 1);
  }

  if (thread_cc) {
    /* check for initial break before we do anything */
    scheme_check_break_now();
  }

  v = k();

  /* IMPORTANT: no GCs from here to return, since v
     may refer to multiple values, and we don't want the
     multiple-value array cleared. */

  if (!thread_cc) {
    p = scheme_current_thread;

    p->current_local_env = save_current_local_env;
    p->current_local_mark = save_mark;
    p->current_local_name = save_name;
    p->current_local_certs = save_certs;
    p->current_local_modidx = save_modidx;
    p->current_local_menv = save_menv;

    p->error_buf = save;
  }

  if (prompt) {
    scheme_pop_continuation_frame(&cframe);
    if (old_pcc == scheme_prompt_capture_count) {
      /* It wasn't used */
      available_prompt = prompt;
    }
  }

  if (scheme_active_but_sleeping)
    scheme_wake_up();

  return v;
}

void *scheme_top_level_do(void *(*k)(void), int eb)
{
  void *sj_start;

#ifdef MZ_PRECISE_GC
  sj_start = (void *)&__gc_var_stack__;
#else
  sj_start = &sj_start;
#endif

  sj_start = top_level_do(k, eb, sj_start);

#ifdef MZ_PRECISE_GC
  if (0) {
    /* ensure __gc_var_stack__ is here: */
    sj_start = scheme_malloc_atomic(0);
  }
#endif

  return sj_start;
}

void scheme_clear_prompt_cache()
{
  available_prompt = NULL;
  available_cws_prompt = NULL;
  available_regular_prompt = NULL;
  available_prompt_dw = NULL;
  available_prompt_mc = NULL;
}

static void ensure_overflow_id(Scheme_Overflow *overflow)
{
  void *id;
  if (!overflow->id) {
    if (overflow->jmp) {
      overflow->id = overflow->jmp;
    } else {
      id = scheme_malloc_atomic(4);
      overflow->id = id;
    }
  }
}

void scheme_ensure_dw_id(Scheme_Dynamic_Wind *dw)
{
  void *id;
  if (!dw->id) {
    id = scheme_malloc_atomic(4);
    dw->id = id;
  }
}

/*========================================================================*/
/*                  procedure application evaluation                      */
/*========================================================================*/

static Scheme_Object *
force_values(Scheme_Object *obj, int multi_ok)
  /* Called where _scheme_apply() or _scheme_value() might return a
     a tail-call-waiting trampoline token.  */
{
  if (SAME_OBJ(obj, SCHEME_TAIL_CALL_WAITING)) {
    Scheme_Thread *p = scheme_current_thread;
    GC_CAN_IGNORE Scheme_Object *rator;
    GC_CAN_IGNORE Scheme_Object **rands;
      
    /* Watch out for use of tail buffer: */
    if (p->ku.apply.tail_rands == p->tail_buffer) {
      GC_CAN_IGNORE Scheme_Object **tb;
      p->tail_buffer = NULL; /* so args aren't zeroed */
      tb = MALLOC_N(Scheme_Object *, p->tail_buffer_size);
      p->tail_buffer = tb;
    }

    rator = p->ku.apply.tail_rator;
    rands = p->ku.apply.tail_rands;
    p->ku.apply.tail_rator = NULL;
    p->ku.apply.tail_rands = NULL;
      
    if (multi_ok) {
      return _scheme_apply_multi(rator,
				 p->ku.apply.tail_num_rands,
				 rands);
    } else {
      return _scheme_apply(rator,
			   p->ku.apply.tail_num_rands,
			   rands);
    }
  } else if (SAME_OBJ(obj, SCHEME_EVAL_WAITING)) {
    Scheme_Thread *p = scheme_current_thread;
    if (multi_ok)
      return _scheme_eval_linked_expr_multi(p->ku.eval.wait_expr);
    else
      return _scheme_eval_linked_expr(p->ku.eval.wait_expr);
  } else if (obj)
    return obj;
  else
    return scheme_void;
}

Scheme_Object *
scheme_force_value(Scheme_Object *obj)
{
  return force_values(obj, 1);
}

Scheme_Object *
scheme_force_one_value(Scheme_Object *obj)
{
  return force_values(obj, 0);
}

Scheme_Object *
scheme_force_value_same_mark(Scheme_Object *obj)
{
  Scheme_Object *v;
  
  MZ_CONT_MARK_POS -= 2;
  v = force_values(obj, 1);
  MZ_CONT_MARK_POS += 2;

  return v;
}

Scheme_Object *
scheme_force_one_value_same_mark(Scheme_Object *obj)
{
  Scheme_Object *v;
  
  MZ_CONT_MARK_POS -= 2;
  v = force_values(obj, 0);
  MZ_CONT_MARK_POS += 2;

  return v;
}

static void *apply_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *rator;
  int num_rands;
  Scheme_Object **rands;

  rator = (Scheme_Object *)p->ku.k.p1;
  rands = (Scheme_Object **)p->ku.k.p2;
  num_rands = p->ku.k.i1;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  if (p->ku.k.i2)
    return (void *)_scheme_apply_multi_wp(rator, num_rands, rands, p);
  else
    return (void *)_scheme_apply_wp(rator, num_rands, rands, p);
}

static Scheme_Object *
_apply(Scheme_Object *rator, int num_rands, Scheme_Object **rands, int multi, int eb)
{
  Scheme_Thread *p = scheme_current_thread;

  p->ku.k.p1 = rator;
  p->ku.k.p2 = rands;
  p->ku.k.i1 = num_rands;
  p->ku.k.i2 = multi;

  return (Scheme_Object *)scheme_top_level_do(apply_k, eb);
}

Scheme_Object *
scheme_apply(Scheme_Object *rator, int num_rands, Scheme_Object **rands)
{
  return _apply(rator, num_rands, rands, 0, 1);
}

Scheme_Object *
scheme_apply_multi(Scheme_Object *rator, int num_rands, Scheme_Object **rands)
{
  return _apply(rator, num_rands, rands, 1, 1);
}

Scheme_Object *scheme_apply_thread_thunk(Scheme_Object *rator)
{
  top_next_use_thread_cc_ok = 1;
  return _apply(rator, 0, NULL, 1, 1);
}

Scheme_Object *
scheme_apply_no_eb(Scheme_Object *rator, int num_rands, Scheme_Object **rands)
{
  return _apply(rator, num_rands, rands, 0, 0);
}

Scheme_Object *
scheme_apply_multi_no_eb(Scheme_Object *rator, int num_rands, Scheme_Object **rands)
{
  return _apply(rator, num_rands, rands, 1, 0);
}

static Scheme_Object *
finish_apply_with_prompt(void *_data, int argc, Scheme_Object **argv)
{
  void **data = (void **)_data;
  Scheme_Object *rator, *is_multi;

  argv = (Scheme_Object **)_data;
  for (argc = 0; data[argc]; argc++) { }

  rator = (Scheme_Object *)data[argc+1];
  is_multi = (Scheme_Object *)data[argc+2];

  if (SCHEME_TRUEP(is_multi))
    return _scheme_apply_multi(rator, argc, argv);
  else
    return _scheme_apply(rator, argc, argv);
}

static Scheme_Object *
do_apply_with_prompt(Scheme_Object *rator, int num_rands, Scheme_Object **rands, int multi, int top_level)
{
  void **a;
  int i;

  a = MALLOC_N(void*, 3 + num_rands);

  for (i = 0; i < num_rands; i++) {
    a[i] = rands[i];
  }
  a[num_rands] = NULL;
  a[num_rands + 1] = rator;
  a[num_rands + 2] = (multi ? scheme_true : scheme_false);

  if (top_level) {
    if (multi)
      return scheme_call_with_prompt_multi(finish_apply_with_prompt, a);
    else
      return scheme_call_with_prompt(finish_apply_with_prompt, a);
  } else {
    if (multi)
      return _scheme_call_with_prompt_multi(finish_apply_with_prompt, a);
    else
      return _scheme_call_with_prompt(finish_apply_with_prompt, a);
  }
}

Scheme_Object *
scheme_apply_with_prompt(Scheme_Object *rator, int num_rands, Scheme_Object **rands)
{
  return do_apply_with_prompt(rator, num_rands, rands, 0, 1);
}

Scheme_Object *
scheme_apply_multi_with_prompt(Scheme_Object *rator, int num_rands, Scheme_Object **rands)
{
  return do_apply_with_prompt(rator, num_rands, rands, 1, 1);
}

Scheme_Object *
_scheme_apply_with_prompt(Scheme_Object *rator, int num_rands, Scheme_Object **rands)
{
  return do_apply_with_prompt(rator, num_rands, rands, 0, 0);
}

Scheme_Object *_scheme_apply_multi_with_prompt(Scheme_Object *rator, int num_rands, Scheme_Object **rands)
{
  return do_apply_with_prompt(rator, num_rands, rands, 1, 0);
}


Scheme_Object *
scheme_tail_apply (Scheme_Object *rator, int num_rands, Scheme_Object **rands)
{
  /* NOTE: apply_values_execute (in syntax.c) and
     tail_call_with_values_from_multiple_result (in jit.c)
     assume that this function won't allocate when 
     num_rands <= p->tail_buffer_size. */
  int i;
  Scheme_Thread *p = scheme_current_thread;

  p->ku.apply.tail_rator = rator;
  p->ku.apply.tail_num_rands = num_rands;

  if (num_rands) {
    Scheme_Object **a;
    if (num_rands > p->tail_buffer_size) {
      {
	Scheme_Object **tb;
	tb = MALLOC_N(Scheme_Object *, num_rands);
	p->tail_buffer = tb;
	p->tail_buffer_size = num_rands;
      }
    }
    a = p->tail_buffer;
    p->ku.apply.tail_rands = a;
    for (i = num_rands; i--; ) {
      a[i] = rands[i];
    }
  } else
    p->ku.apply.tail_rands = NULL;

  return SCHEME_TAIL_CALL_WAITING;
}

Scheme_Object *
scheme_tail_apply_no_copy (Scheme_Object *rator, int num_rands,
			   Scheme_Object **rands)
{
  Scheme_Thread *p = scheme_current_thread;

  p->ku.apply.tail_rator = rator;
  p->ku.apply.tail_num_rands = num_rands;
  p->ku.apply.tail_rands = rands;

  return SCHEME_TAIL_CALL_WAITING;
}

static
Scheme_Object *
X_scheme_apply_to_list(Scheme_Object *rator, Scheme_Object *rands, int force,
		       int top_level)
{
  int num_rands, i;
  Scheme_Object **rands_vec;

  num_rands = scheme_list_length(rands);
  rands_vec = MALLOC_N(Scheme_Object *, num_rands);

  for (i = 0; i < num_rands ; i++) {
    if (!SCHEME_PAIRP(rands)) {
      scheme_signal_error("bad application form");
    }
    rands_vec[i] = SCHEME_CAR(rands);
    rands = SCHEME_CDR(rands);
  }

  if (top_level)  {
    if (force)
      return scheme_apply(rator, num_rands, rands_vec);
    else
      return scheme_tail_apply(rator, num_rands, rands_vec);
  } else {
    if (force)
      return _scheme_apply(rator, num_rands, rands_vec);
    else
      return _scheme_tail_apply(rator, num_rands, rands_vec);
  }
}

Scheme_Object *
scheme_apply_to_list (Scheme_Object *rator, Scheme_Object *rands)
{
  return X_scheme_apply_to_list(rator, rands, 1, 1);
}

Scheme_Object *
scheme_tail_apply_to_list (Scheme_Object *rator, Scheme_Object *rands)
{
  return X_scheme_apply_to_list(rator, rands, 0, 1);
}

Scheme_Object *
_scheme_apply_to_list (Scheme_Object *rator, Scheme_Object *rands)
{
  return X_scheme_apply_to_list(rator, rands, 1, 0);
}

Scheme_Object *
_scheme_tail_apply_to_list (Scheme_Object *rator, Scheme_Object *rands)
{
  return X_scheme_apply_to_list(rator, rands, 0, 0);
}

static Scheme_Object *
cert_with_specials(Scheme_Object *code, Scheme_Object *mark, Scheme_Env *menv, 
		   Scheme_Object *orig_code, Scheme_Object *closest_code,
                   Scheme_Comp_Env *cenv, int phase, 
		   int deflt, int cadr_deflt)
{
  Scheme_Object *prop;
  int next_cadr_deflt = 0;

  if (!certify_mode_symbol) {
    REGISTER_SO(certify_mode_symbol);
    REGISTER_SO(transparent_symbol);
    REGISTER_SO(transparent_binding_symbol);
    REGISTER_SO(opaque_symbol);
    certify_mode_symbol = scheme_intern_symbol("certify-mode");
    transparent_symbol = scheme_intern_symbol("transparent");
    transparent_binding_symbol = scheme_intern_symbol("transparent-binding");
    opaque_symbol = scheme_intern_symbol("opaque");
  }

  if (SCHEME_STXP(code)) {
    prop = scheme_stx_property(code, certify_mode_symbol, NULL);
    if (SAME_OBJ(prop, opaque_symbol)) {
      return scheme_stx_cert(code, mark, menv, orig_code, NULL, 1);
    } else if (SAME_OBJ(prop, transparent_symbol)) {
      cadr_deflt = 0;
      /* fall through */
    } else if (SAME_OBJ(prop, transparent_binding_symbol)) {
      cadr_deflt = 0;
      next_cadr_deflt = 1;
      /* fall through */
    } else {
      /* Default transparency depends on module-identifier=? comparison
	 to `begin', `define-values', and `define-syntaxes'. */
      int trans = deflt;
      if (SCHEME_STX_PAIRP(code)) {
	Scheme_Object *name;
	name = SCHEME_STX_CAR(code);
	if (SCHEME_STX_SYMBOLP(name)) {
	  Scheme_Object *beg_stx, *dv_stx, *ds_stx;

	  if (!phase) {
	    beg_stx = scheme_begin_stx;
	    dv_stx = scheme_define_values_stx;
	    ds_stx = scheme_define_syntaxes_stx;
	  } else if (phase == cached_stx_phase) {
	    beg_stx = cached_beg_stx;
	    dv_stx = cached_dv_stx;
	    ds_stx = cached_ds_stx;
	  } else {
	    beg_stx = scheme_datum_to_syntax(SCHEME_STX_VAL(scheme_begin_stx), scheme_false, 
					     scheme_sys_wraps(cenv), 0, 0);
	    dv_stx = scheme_datum_to_syntax(SCHEME_STX_VAL(scheme_define_values_stx), scheme_false, 
					    scheme_sys_wraps(cenv), 0, 0);
	    ds_stx = scheme_datum_to_syntax(SCHEME_STX_VAL(scheme_define_syntaxes_stx), scheme_false, 
					    scheme_sys_wraps(cenv), 0, 0);
	    cached_beg_stx = beg_stx;
	    cached_dv_stx = dv_stx;
	    cached_ds_stx = ds_stx;
	    cached_stx_phase = phase;
	  }

	  if (scheme_stx_module_eq(beg_stx, name, phase)) {
	    trans = 1;
	    next_cadr_deflt = 0;
	  } else if (scheme_stx_module_eq(dv_stx, name, phase)
		     || scheme_stx_module_eq(ds_stx, name, phase)) {
	    trans = 1;
	    next_cadr_deflt = 1;
	  }
	}
      }
      
      if (!trans)
	return scheme_stx_cert(code, mark, menv, orig_code, NULL, 1);
    }
  }

  if (SCHEME_STX_PAIRP(code)) {
    Scheme_Object *a, *d, *v;
    
    if (SCHEME_STXP(code))
      closest_code = code;

    a = SCHEME_STX_CAR(code);
    a = scheme_stx_propagate_inactive_certs(a, closest_code);
    a = cert_with_specials(a, mark, menv, orig_code, closest_code, cenv, phase, cadr_deflt, 0);
    d = SCHEME_STX_CDR(code);
    if (SCHEME_STXP(d))
      d = scheme_stx_propagate_inactive_certs(d, closest_code);
    d = cert_with_specials(d, mark, menv, orig_code, closest_code, cenv, phase, 1, next_cadr_deflt);

    v = scheme_make_pair(a, d);

    if (SCHEME_PAIRP(code))
      return v;

    return scheme_datum_to_syntax(v, code, code, 0, 2);
  } else if (SCHEME_STX_NULLP(code))
    return code;

  return scheme_stx_cert(code, mark, menv, orig_code, NULL, 1);
}

Scheme_Object *
scheme_apply_macro(Scheme_Object *name, Scheme_Env *menv,
		   Scheme_Object *rator, Scheme_Object *code,
		   Scheme_Comp_Env *env, Scheme_Object *boundname,
                   Scheme_Compile_Expand_Info *rec, int drec,
		   int for_set)
{
  Scheme_Object *orig_code = code;
  Scheme_Object *certs;
  certs = rec[drec].certs;

 if (SAME_TYPE(SCHEME_TYPE(rator), scheme_id_macro_type)) {
   Scheme_Object *mark;
   
   rator = SCHEME_PTR1_VAL(rator);
   /* rator is now an identifier */

   /* and it's introduced by this expression: */
   mark = scheme_new_mark();
   rator = scheme_add_remove_mark(rator, mark);

   if (for_set) {
     Scheme_Object *tail, *setkw;

     tail = SCHEME_STX_CDR(code);
     setkw = SCHEME_STX_CAR(code);
     tail = SCHEME_STX_CDR(tail);
     code = scheme_make_immutable_pair(setkw, scheme_make_immutable_pair(rator, tail));
     code = scheme_datum_to_syntax(code, orig_code, orig_code, 0, 0);
   } else if (SCHEME_SYMBOLP(SCHEME_STX_VAL(code)))
     code = rator;
   else {
     code = SCHEME_STX_CDR(code);
     code = scheme_make_immutable_pair(rator, code);
     code = scheme_datum_to_syntax(code, orig_code, scheme_sys_wraps(env), 0, 0);
   }

   code = cert_with_specials(code, mark, menv, orig_code, orig_code, env, env->genv->phase, 0, 0);

   code = scheme_stx_track(code, orig_code, name);

   return code;
 } else {
   Scheme_Object *mark, *rands_vec[1];

   certs = scheme_stx_extract_certs(code, certs);
 
   if (SAME_TYPE(SCHEME_TYPE(rator), scheme_set_macro_type))
     rator = SCHEME_PTR_VAL(rator);

   mark = scheme_new_mark();
   code = scheme_add_remove_mark(code, mark);

   SCHEME_EXPAND_OBSERVE_MACRO_PRE_X(rec[drec].observer, code);

   scheme_on_next_top(env, mark, boundname, certs, 
		      menv, menv ? menv->link_midx : env->genv->link_midx);

   rands_vec[0] = code;
   code = scheme_apply(rator, 1, rands_vec);

   SCHEME_EXPAND_OBSERVE_MACRO_POST_X(rec[drec].observer, code);

   if (!SCHEME_STXP(code)) {
     scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		      "%S: return value from syntax expander was not syntax: %V",
		      SCHEME_STX_SYM(name),
		      code);
   }

   code = scheme_add_remove_mark(code, mark);

   code = cert_with_specials(code, mark, menv, orig_code, orig_code, env, env->genv->phase, 0, 0);

   code = scheme_stx_track(code, orig_code, name);

   return code;
 }
}

/*========================================================================*/
/*                                   arity                                */
/*========================================================================*/

Scheme_Object *scheme_make_arity(mzshort mina, mzshort maxa)
{
  if (mina == maxa)
    return scheme_make_integer(mina);
  else if (maxa == -1) {
    Scheme_Object *p[1];
    p[0] = scheme_make_integer(mina);
    return scheme_make_struct_instance(scheme_arity_at_least, 1, p);
  } else {
    int i;
    Scheme_Object *l = scheme_null;

    for (i = maxa; i >= mina; --i) {
      l = scheme_make_pair(scheme_make_integer(i), l);
    }

    return l;
  }
}

static Scheme_Object *clone_arity(Scheme_Object *a)
{
  if (SCHEME_PAIRP(a)) {
    Scheme_Object *m, *l;
    m = scheme_copy_list(a);
    for (l = m; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
      a = clone_arity(SCHEME_CAR(l));
      SCHEME_CAR(l) = a;
    }
    return m;
  } else if (SCHEME_STRUCTP(a)) {
    Scheme_Object *p[1];
    p[0] = ((Scheme_Structure *)a)->slots[0];
    return scheme_make_struct_instance(scheme_arity_at_least, 1, p);
  } else
    return a;
}

static Scheme_Object *get_or_check_arity(Scheme_Object *p, long a, Scheme_Object *bign)
/* a == -1 => get arity
   a == -2 => check for allowing bignum */
{
  Scheme_Type type;
  mzshort mina, maxa;
  int drop = 0, cases_count = 0;
  mzshort *cases = NULL;

 top:

  type = SCHEME_TYPE(p);
  if (type == scheme_prim_type) {
    mina = ((Scheme_Primitive_Proc *)p)->mina;
    maxa = ((Scheme_Primitive_Proc *)p)->mu.maxa;
    if (mina < 0) {
      cases = ((Scheme_Primitive_Proc *)p)->mu.cases;
      cases_count = -(mina + 1);
    } else {
      if (maxa > SCHEME_MAX_ARGS)
	maxa = -1;
    }
  } else if (type == scheme_closed_prim_type) {
    mina = ((Scheme_Closed_Primitive_Proc *)p)->mina;
    maxa = ((Scheme_Closed_Primitive_Proc *)p)->maxa;
    if (mina == -2) {
      cases_count = -maxa;
      cases = ((Scheme_Closed_Case_Primitive_Proc *)p)->cases;
    }
  } else if (type == scheme_cont_type || type == scheme_escaping_cont_type) {
    mina = 0;
    maxa = -1;
  } else if (type == scheme_case_closure_type) {
    Scheme_Case_Lambda *seq;
    Scheme_Closure_Data *data;
    int i;
    Scheme_Object *first, *last = NULL, *v;

    if (a == -1)
      first = scheme_null;
    else
      first = scheme_false;

    seq = (Scheme_Case_Lambda *)p;
    for (i = 0; i < seq->count; i++) {
      data = SCHEME_COMPILED_CLOS_CODE(seq->array[i]);
      mina = maxa = data->num_params;
      if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST) {
	if (mina)
	  --mina;
	maxa = -1;
      }

      if (a >= 0) {
	if ((a + drop) >= mina && (maxa < 0 || (a + drop) <= maxa))
	  return scheme_true;
      } else if (a == -2) {
	if (maxa < 0)
	  return scheme_true;
      } else {
	if (mina >= drop) {
	  mina -= drop;
	  if (maxa > 0)
	    maxa -= drop;

	  v = scheme_make_pair(scheme_make_arity(mina, maxa), scheme_null);
	  if (!last)
	    first = v;
	  else
	    SCHEME_CDR(last) = v;
	  last = v;
	}
      }
    }

    return first;
  } else if (type == scheme_proc_struct_type) {
    int is_method;
    if (reduced_procedure_struct
        && scheme_is_struct_instance(reduced_procedure_struct, p)) {
      if (a >= 0)
        bign = scheme_make_integer(a);
      if (a == -1)
        return clone_arity(((Scheme_Structure *)p)->slots[1]);
      else {
        /* Check arity (or for varargs) */
        Scheme_Object *v;
        v = ((Scheme_Structure *)p)->slots[1];
        if (SCHEME_STRUCTP(v)) {
          v = ((Scheme_Structure *)v)->slots[0];
          return (scheme_bin_lt_eq(v, bign)
                  ? scheme_true
                  : scheme_false);
        } else if (SCHEME_PAIRP(v)) {
          Scheme_Object *x;
          while (!SCHEME_NULLP(v)) {
            x = SCHEME_CAR(v);
            if (SCHEME_STRUCTP(x)) {
              x = ((Scheme_Structure *)x)->slots[0];  
              if (scheme_bin_lt_eq(x, bign))
                return scheme_true;
            } else {
              if (scheme_bin_eq(x, bign))
                return scheme_true;
            }
            v = SCHEME_CDR(v);
          }
          return scheme_false;
        } else {
          return (scheme_bin_eq(v, bign)
                  ? scheme_true
                  : scheme_false);
        }
      }
    } else {
      p = scheme_extract_struct_procedure(p, -1, NULL, &is_method);
      if (!SCHEME_PROCP(p)) {
        if (a == -1)
          return scheme_null;
        else
          return scheme_false;
      }
      if (is_method)
        drop++;
    }
    SCHEME_USE_FUEL(1);
    goto top;
#ifdef MZ_USE_JIT
  } else if (type == scheme_native_closure_type) {
    if (a < 0) {
      Scheme_Object *pa;

      pa = scheme_get_native_arity(p);

      if (SCHEME_BOXP(pa)) {
	/* Is a method; pa already corrects for it */
	pa = SCHEME_BOX_VAL(pa);
      }

      if (SCHEME_STRUCTP(pa)) {
	/* This happens when a non-case-lambda is not yet JITted.
	   It's an arity-at-least record. Convert it to the
	   negative-int encoding. */
	int v;
	pa = ((Scheme_Structure *)pa)->slots[0];
	v = -(SCHEME_INT_VAL(pa) + 1);
	pa = scheme_make_integer(v);
      }

      if (SCHEME_INTP(pa)) {
	mina = SCHEME_INT_VAL(pa);
	if (mina < 0) {
	  if (a == -2) {
	    /* Yes, varargs */
	    return scheme_true;
	  }
	  mina = (-mina) - 1;
	  maxa = -1;
	} else {
	  if (a == -2) {
	    /* No varargs */
	    return scheme_false;
	  }
	  maxa = mina;
	}
      } else {
	if (a == -2) {
	  /* Check for varargs */
	  Scheme_Object *a;
	  while (!SCHEME_NULLP(pa)) {
	    a = SCHEME_CAR(pa);
	    if (SCHEME_STRUCTP(a))
	      return scheme_true;
	    pa = SCHEME_CDR(pa);
	  }
	  return scheme_false;
	} else {
	  if (drop) {
	    /* Need to adjust elements (e.g., because this
	       procedure is a struct's apply handler) */
	    Scheme_Object *first = scheme_null, *last = NULL, *a;
	    int v;
	    while (SCHEME_PAIRP(pa)) {
	      a = SCHEME_CAR(pa);
	      if (SCHEME_INTP(a)) {
		v = SCHEME_INT_VAL(a);
		if (v < drop)
		  a = NULL;
		else {
		  v -= drop;
		  a = scheme_make_integer(v);
		}
	      } else {
		/* arity-at-least */
		a = ((Scheme_Structure *)a)->slots[0];
		v = SCHEME_INT_VAL(a);
		if (v >= drop) {
		  a = scheme_make_arity(v - drop, -1);
		} else {
		  a = scheme_make_arity(0, -1);
		}
	      }
	      if (a) {
		a = scheme_make_pair(a, scheme_null);
		if (last)
		  SCHEME_CDR(last) = a;
		else
		  first = a;
		last = a;
	      }
	      pa = SCHEME_CDR(pa);
	    }
	    return first;
	  }
	  return pa;
	}
      }
    } else {
      if (scheme_native_arity_check(p, a + drop))
	return scheme_true;
      else
	return scheme_false;
    }
#endif
  } else {
    Scheme_Closure_Data *data;

    data = SCHEME_COMPILED_CLOS_CODE(p);
    mina = maxa = data->num_params;
    if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST) {
      if (mina)
	--mina;
      maxa = -1;
    }
  }

  if (cases) {
    int count = cases_count, i;

    if (a == -1) {
      Scheme_Object *arity, *a, *last = NULL;

      arity = scheme_alloc_list(count);

      for (i = 0, a = arity; i < count; i++) {
	Scheme_Object *av;
	int mn, mx;
	mn = cases[2 * i];
	mx = cases[(2 * i) + 1];

	if (mn >= drop) {
	  mn -= drop;
	  if (mx > 0)
	    mx -= drop;

	  av = scheme_make_arity(mn, mx);

	  SCHEME_CAR(a) = av;
	  last = a;
	  a = SCHEME_CDR(a);
	}
      }

      /* If drop > 0, might have found no matches */
      if (!SCHEME_NULLP(a)) {
	if (last)
	  SCHEME_CDR(last) = scheme_null;
	else
	  arity = scheme_null;
      }

      return arity;
    }

    if (a == -2) {
      for (i = 0; i < count; i++) {
	if (cases[(2 * i) + 1] < 0)
	  return scheme_true;
      }

      return scheme_false;
    }

    a += drop;

    for (i = 0; i < count; i++) {
      int na, xa;
      na = cases[2 * i];
      xa = cases[(2 * i) + 1];
      if ((a >= na) && ((xa < 0) || (a <= xa)))
	return scheme_true;
    }

    return scheme_false;
  }

  if (a == -1) {
    if (mina < drop)
      return scheme_null;
    else
      mina -= drop;
    if (maxa > 0)
      maxa -= drop;

    return scheme_make_arity(mina, maxa);
  }

  if (a == -2)
    return (maxa < 0) ? scheme_true : scheme_false;

  a += drop;

  if (a < mina || (maxa >= 0 && a > maxa))
    return scheme_false;

  return scheme_true;
}

Scheme_Object *scheme_get_or_check_arity(Scheme_Object *p, long a)
{
  return get_or_check_arity(p, a, NULL);
}

int scheme_check_proc_arity2(const char *where, int a,
			     int which, int argc, Scheme_Object **argv,
			     int false_ok)
{
  Scheme_Object *p;

  if (which < 0)
    p = argv[0];
  else
    p = argv[which];

  if (false_ok && SCHEME_FALSEP(p))
    return 1;

  if (!SCHEME_PROCP(p) || SCHEME_FALSEP(get_or_check_arity(p, a, NULL))) {
    if (where) {
      char buffer[60];

      sprintf(buffer, "procedure (arity %d)%s", 
	      a,
	      false_ok ? " or #f" : "");

      scheme_wrong_type(where, buffer, which, argc, argv);
    } else
      return 0;
  }

  return 1;
}

int scheme_check_proc_arity(const char *where, int a,
			    int which, int argc, Scheme_Object **argv)
{
  return scheme_check_proc_arity2(where, a, which, argc, argv, 0);
}

/*========================================================================*/
/*                        basic function primitives                       */
/*========================================================================*/

static Scheme_Object *
void_func (int argc, Scheme_Object *argv[])
{
  return scheme_void;
}

static Scheme_Object *
void_p (int argc, Scheme_Object *argv[])
{
  return SAME_OBJ(argv[0], scheme_void) ? scheme_true : scheme_false;
}

static Scheme_Object *
procedure_p (int argc, Scheme_Object *argv[])
{
  return (SCHEME_PROCP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *primitive_p(int argc, Scheme_Object *argv[])
{
  int isprim;

  if (SCHEME_PRIMP(argv[0]))
    isprim = (((Scheme_Primitive_Proc *)argv[0])->pp.flags & SCHEME_PRIM_IS_PRIMITIVE);
  else if (SCHEME_CLSD_PRIMP(argv[0]))
    isprim = (((Scheme_Closed_Primitive_Proc *)argv[0])->pp.flags & SCHEME_PRIM_IS_PRIMITIVE);
  else
    isprim = 0;

  return isprim ? scheme_true : scheme_false;
}

static Scheme_Object *primitive_closure_p(int argc, Scheme_Object *argv[])
{
  int isprim;

  if (SCHEME_CLSD_PRIMP(argv[0]))
    isprim = (((Scheme_Closed_Primitive_Proc *)argv[0])->pp.flags & SCHEME_PRIM_IS_PRIMITIVE);
  else
    isprim = 0;

  return isprim ? scheme_true : scheme_false;
}

Scheme_Object *scheme_proc_struct_name_source(Scheme_Object *a)
{
  Scheme_Object *b;

  while (SCHEME_PROC_STRUCTP(a)) {
    /* Either use struct name, or extract proc, depending
       whether it's method-style */
    int is_method;
    b = scheme_extract_struct_procedure(a, -1, NULL, &is_method);
    if (!is_method && SCHEME_PROCP(b)) {
      a = b;
      SCHEME_USE_FUEL(1);
    } else
      break;
  }

  return a;
}

const char *scheme_get_proc_name(Scheme_Object *p, int *len, int for_error)
     /* for_error > 0 => get name for an error message;
	for_error < 0 => symbol result ok set *len to -1 */
{
  Scheme_Type type;
  int dummy;
  char *s;

  if (!len)
    len = &dummy;

 top:

  type = SCHEME_TYPE(p);
  if (type == scheme_prim_type) {
    if (((Scheme_Primitive_Proc *)p)->name)
      *len = strlen(((Scheme_Primitive_Proc *)p)->name);
    return ((Scheme_Primitive_Proc *)p)->name;
  } else if (type == scheme_closed_prim_type) {
    if (((Scheme_Closed_Primitive_Proc *)p)->name)
      *len = strlen(((Scheme_Closed_Primitive_Proc *)p)->name);
    return ((Scheme_Closed_Primitive_Proc *)p)->name;
  } else if (type == scheme_cont_type || type == scheme_escaping_cont_type) {
    return NULL;
  } else if (type == scheme_case_closure_type) {
    Scheme_Object *n;

    n = ((Scheme_Case_Lambda *)p)->name;
    if (n) {
      if (SCHEME_BOXP(n)) {
	/* See note in schpriv.h about the IS_METHOD hack */
	n = SCHEME_BOX_VAL(n);
	if (SCHEME_FALSEP(n))
	  return NULL;
      }

      if (SCHEME_VECTORP(n))
	n = SCHEME_VEC_ELS(n)[0];

      if (for_error < 0) {
	s = (char *)n;
	*len = -1;
      } else {
	*len = SCHEME_SYM_LEN(n);
	s = scheme_symbol_val(n);
      }
    } else
      return NULL;
  } else if (type == scheme_proc_struct_type) {
    /* Assert: the request is for an error. */
    Scheme_Object *other;
    other = scheme_proc_struct_name_source(p);
    if (SAME_OBJ(other, p)) {
      Scheme_Object *sym;
      sym = SCHEME_STRUCT_NAME_SYM(p);
      *len = SCHEME_SYM_LEN(sym);
      s = (char *)scheme_malloc_atomic((*len) + 8);
      memcpy(s, "struct ", 7);
      memcpy(s + 7, scheme_symbol_val(sym), *len);
      (*len) += 7;
      s[*len] = 0;
      return s;
    } else {
      p = other;
      goto top;
    }
  } else {
    Scheme_Object *name;

    if (type == scheme_closure_type) {
      name = SCHEME_COMPILED_CLOS_CODE(p)->name;
    } else {
      /* Native closure: */
      name = ((Scheme_Native_Closure *)p)->code->u2.name;
      if (name && SAME_TYPE(SCHEME_TYPE(name), scheme_unclosed_procedure_type)) {
	/* Not yet jitted. Use `name' as the other alternaive of 
	   the union: */
	name = ((Scheme_Closure_Data *)name)->name;
      }
    }

    if (name) {
      if (SCHEME_VECTORP(name))
	name = SCHEME_VEC_ELS(name)[0];
      if (for_error < 0) {
	s = (char *)name;
	*len = -1;
      } else {
	*len = SCHEME_SYM_LEN(name);
	s = scheme_symbol_val(name);
      }
    } else
      return NULL;
  }

  if (for_error > 0) {
    char *r;

    r = (char *)scheme_malloc_atomic((*len) + 11);
    memcpy(r, "procedure ", 10);
    memcpy(r + 10, s, *len + 1);
    *len += 10;

    return r;
  }

  return s;
}

static Scheme_Object *primitive_result_arity(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o;

  o = argv[0];

  if (SCHEME_PRIMP(o)
      && (((Scheme_Primitive_Proc *)o)->pp.flags & SCHEME_PRIM_IS_PRIMITIVE)) {
    if (((Scheme_Primitive_Proc *)o)->pp.flags & SCHEME_PRIM_IS_MULTI_RESULT) {
      Scheme_Prim_W_Result_Arity *p = (Scheme_Prim_W_Result_Arity *)o;
      return scheme_make_arity(p->minr, p->maxr);
    }
  } else if (SCHEME_CLSD_PRIMP(o)
	     && (((Scheme_Closed_Primitive_Proc *)o)->pp.flags & SCHEME_PRIM_IS_PRIMITIVE)) {
    if (((Scheme_Closed_Primitive_Proc *)o)->pp.flags & SCHEME_PRIM_IS_MULTI_RESULT) {
      Scheme_Closed_Prim_W_Result_Arity *p = (Scheme_Closed_Prim_W_Result_Arity *)o;
      return scheme_make_arity(p->minr, p->maxr);
    }
  } else {
    scheme_wrong_type("primitive-result_arity", "primitive", 0, argc, argv);
    return NULL;
  }

  return scheme_make_integer(1);
}

static Scheme_Object *object_name(int argc, Scheme_Object **argv)
{
  Scheme_Object *a = argv[0];

  if (SCHEME_PROC_STRUCTP(a))
    a = scheme_proc_struct_name_source(a);

  if (SCHEME_STRUCTP(a)) {
    return SCHEME_STRUCT_NAME_SYM(a);
  } else if (SCHEME_PROCP(a)) {
    const char *s;
    int len;

    s = scheme_get_proc_name(a, &len, -1);
    if (s) {
      if (len < 0)
	return (Scheme_Object *)s;
      else
	return scheme_intern_exact_symbol(s, len);
    }
  } else if (SCHEME_STRUCT_TYPEP(a)) {
    return ((Scheme_Struct_Type *)a)->name;
  } else if (SAME_TYPE(SCHEME_TYPE(a), scheme_struct_property_type)) {
    return ((Scheme_Struct_Property *)a)->name;
  } else if (SAME_TYPE(SCHEME_TYPE(a), scheme_regexp_type)) {
    Scheme_Object *s;
    s = scheme_regexp_source(a);
    if (s)
      return s;
  } else if (SCHEME_INPUT_PORTP(a)) {
    Scheme_Input_Port *ip;
    ip = scheme_input_port_record(a);
    return ip->name;
  } else if (SCHEME_OUTPUT_PORTP(a)) {
    Scheme_Output_Port *op;
    op = scheme_output_port_record(a);
    return op->name;
  } else if (SCHEME_THREADP(a)) {
    Scheme_Thread *t = (Scheme_Thread *)a;
    if (t->name) {
      return t->name;
    }
  }

  return scheme_false;
}

Scheme_Object *scheme_arity(Scheme_Object *p)
{
  return get_or_check_arity(p, -1, NULL);
}

static Scheme_Object *procedure_arity(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_PROCP(argv[0]))
    scheme_wrong_type("procedure-arity", "procedure", 0, argc, argv);

  return get_or_check_arity(argv[0], -1, NULL);
}

static Scheme_Object *procedure_arity_includes(int argc, Scheme_Object *argv[])
{
  long n;

  if (!SCHEME_PROCP(argv[0]))
    scheme_wrong_type("procedure-arity-includes?", "procedure", 0, argc, argv);

  n = scheme_extract_index("procedure-arity-includes?", 1, argc, argv, -2, 0);
  /* -2 means a bignum */

  return get_or_check_arity(argv[0], n, argv[1]);
}

static int is_arity(Scheme_Object *a, int at_least_ok, int list_ok)
{
  if (SCHEME_INTP(a)) {
    return (SCHEME_INT_VAL(a) >= 0);
  } else if (SCHEME_BIGNUMP(a)) {
    return SCHEME_BIGPOS(a);
  } else if (at_least_ok
             && SCHEME_STRUCTP(a)
             && scheme_is_struct_instance(scheme_arity_at_least, a)) {
    a = ((Scheme_Structure *)a)->slots[0];
    return is_arity(a, 0, 0);
  }

  if (!list_ok)
    return 0;

  while (SCHEME_PAIRP(a)) {
    if (!is_arity(SCHEME_CAR(a), 1, 0))
      return 0;
    a = SCHEME_CDR(a);
  }

  if (SCHEME_NULLP(a))
    return 1;
  return 0;
}

static Scheme_Object *procedure_reduce_arity(int argc, Scheme_Object *argv[])
{
  Scheme_Object *orig, *req, *oa, *ra, *ol, *lra, *ara, *prev, *pr, *tmp, *a[3];

  if (!SCHEME_PROCP(argv[0]))
    scheme_wrong_type("procedure-reduce-arity", "procedure", 0, argc, argv);

  if (!is_arity(argv[1], 1, 1)) {
    scheme_wrong_type("procedure-reduce-arity", "arity", 1, argc, argv);
  }

  if (!reduced_procedure_struct) {
    REGISTER_SO(reduced_procedure_struct);
    pr = scheme_get_param(scheme_current_config(), MZCONFIG_INSPECTOR);
    while (((Scheme_Inspector *)pr)->superior->superior) {
      pr = (Scheme_Object *)((Scheme_Inspector *)pr)->superior;
    }
    orig = scheme_builtin_value("prop:procedure");
    reduced_procedure_struct = scheme_make_proc_struct_type(NULL,
                                                            NULL,
                                                            pr,
                                                            2, 0,
                                                            scheme_false,
                                                            scheme_make_integer(0),
                                                            NULL);
  }

  /* Check whether current arity covers the requested arity.  This is
     a bit complicated, because both the source and target can be
     lists that include arity-at-least records. */

  orig = get_or_check_arity(argv[0], -1, NULL);
  req = argv[1];

  if (!SCHEME_PAIRP(orig) && !SCHEME_NULLP(orig))
    orig = scheme_make_pair(orig, scheme_null);
  if (!SCHEME_PAIRP(req) && !SCHEME_NULLP(req))
    req = scheme_make_pair(req, scheme_null);

  while (!SCHEME_NULLP(req)) {
    ra = SCHEME_CAR(req);
    if (SCHEME_STRUCTP(ra)
        && scheme_is_struct_instance(scheme_arity_at_least, ra)) {
      /* Convert to a sequence of range pairs, where the
         last one can be (min, #f); we'll iterate through the 
         original arity to knock out ranges until (if it matches)
         we end up with an empty list of ranges. */
      ra = scheme_make_pair(scheme_make_pair(((Scheme_Structure *)ra)->slots[0],
                                             scheme_false),
                            scheme_null);
    }

    for (ol = orig; !SCHEME_NULLP(ol); ol = SCHEME_CDR(ol)) {
      oa = SCHEME_CAR(ol);
      if (SCHEME_INTP(ra) || SCHEME_BIGNUMP(ra)) {
        if (SCHEME_INTP(oa) || SCHEME_BIGNUMP(oa)) {
          if (scheme_equal(ra, oa))
            break;
        } else {
          /* orig is arity-at-least */
          oa = ((Scheme_Structure *)oa)->slots[0];
          if (scheme_bin_lt_eq(oa, ra))
            break;
        }
      } else {
        /* requested is arity-at-least */
        int at_least;
        if (SCHEME_INTP(oa) || SCHEME_BIGNUMP(oa)) {
          at_least = 0;
        } else {
          /* orig is arity-at-least */
          at_least = 1;
          oa = ((Scheme_Structure *)oa)->slots[0];
        }

        lra = ra;
        prev = NULL;
        while (!SCHEME_NULLP(lra)) {
          /* check [lo, hi] vs oa: */
          ara = SCHEME_CAR(lra);
          if (SCHEME_FALSEP(SCHEME_CDR(ara))
              || scheme_bin_lt_eq(oa, SCHEME_CDR(ara))) {
            if (scheme_bin_gt_eq(oa, SCHEME_CAR(ara))) {
              /* oa is in the range [lo, hi]: */
              if (scheme_equal(oa, SCHEME_CAR(ara))) {
                /* the range is [oa, hi] */
                if (at_least) {
                  /* oa is arity-at least, so drop from here */
                  if (prev)
                    SCHEME_CDR(prev) = scheme_null;
                  else
                    ra = scheme_null;
                } else {
                  if (scheme_equal(oa, SCHEME_CDR(ara))) {
                    /* the range is [oa, oa], so drop it */
                    if (prev)
                      SCHEME_CDR(prev) = SCHEME_CDR(lra);
                    else
                      ra = SCHEME_CDR(lra);
                  } else {
                    /* change range to [ao+1, hi] */
                    tmp = scheme_bin_plus(oa, scheme_make_integer(1));
                    SCHEME_CAR(ara) = tmp;
                  }
                }
              } else if (scheme_equal(oa, SCHEME_CAR(ara))) {
                /* the range is [lo, oa], where lo < oa */
                tmp = scheme_bin_minus(oa, scheme_make_integer(1));
                SCHEME_CDR(ara) = tmp;
                if (at_least) 
                  SCHEME_CDR(lra) = scheme_null;
              } else {
                /* split the range */
                if (at_least) {
                  tmp = scheme_bin_minus(oa, scheme_make_integer(1));
                  SCHEME_CDR(ara) = tmp;
                  SCHEME_CDR(lra) = scheme_null;
                } else {
                  pr = scheme_make_pair(scheme_make_pair(scheme_bin_plus(oa, scheme_make_integer(1)),
                                                         SCHEME_CDR(ara)),
                                        SCHEME_CDR(lra));
                  tmp = scheme_bin_minus(oa, scheme_make_integer(1));
                  SCHEME_CDR(ara) = tmp;
                  SCHEME_CDR(lra) = pr;
                }
              }
              break;
            } else if (at_least) {
              /* oa is less than lo, so truncate */
              if (prev)
                SCHEME_CDR(prev) = scheme_null;
              else
                ra = scheme_null;
              break;
            }
          }
          prev = lra;
          lra = SCHEME_CDR(lra);
        }
        if (SCHEME_NULLP(ra))
          break;
      }
    }

    if (SCHEME_NULLP(ol)) {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT_CONTINUATION,
                       "procedure-reduce-arity: arity of procedre: %V"
                       " does not include requested arity: %V : %V",
                       argv[0],
                       argv[1],
                       ra);
      return NULL;
    }

    req = SCHEME_CDR(req);
  }

  /* Construct a procedure that has the given arity. */

  a[0] = argv[0];
  pr = clone_arity(argv[1]);
  a[1] = pr;

  return scheme_make_struct_instance(reduced_procedure_struct, 2, a);
}

static Scheme_Object *procedure_equal_closure_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v1 = argv[0], *v2 = argv[1];

  if (!SCHEME_PROCP(v1))
    scheme_wrong_type("procedure-closure-contents-eq?", "procedure", 0, argc, argv);
  if (!SCHEME_PROCP(v2))
    scheme_wrong_type("procedure-closure-contents-eq?", "procedure", 1, argc, argv);

  if (SAME_OBJ(v1, v2))
    return scheme_true;

  if (!SAME_TYPE(SCHEME_TYPE(v1), SCHEME_TYPE(v2)))
    return scheme_false;

  switch (SCHEME_TYPE(v1)) {
  case scheme_prim_type:
    {
      Scheme_Primitive_Proc *p1 = (Scheme_Primitive_Proc *)v1;
      Scheme_Primitive_Proc *p2 = (Scheme_Primitive_Proc *)v2;

      if (p1->prim_val == p2->prim_val) {
	if (p1->pp.flags & SCHEME_PRIM_IS_CLOSURE) {
	  if (!(p2->pp.flags & SCHEME_PRIM_IS_CLOSURE))
	    return scheme_false;

	  /* They both are closures, but we don't know how 
	     many fields in each, except in 3m mode. So
	     give up. */
	  return scheme_false;
	} else if (!(p2->pp.flags & SCHEME_PRIM_IS_CLOSURE))
	  return scheme_true;
      }
    }
    break;
  case scheme_closure_type:
    {
      Scheme_Closure *c1 = (Scheme_Closure *)v1;
      Scheme_Closure *c2 = (Scheme_Closure *)v2;

      if (SAME_OBJ(c1->code, c2->code)) {
	int i;
	for (i = c1->code->closure_size; i--; ) {
	  if (!SAME_OBJ(c1->vals[i], c2->vals[i]))
	    return scheme_false;
	}
	return scheme_true;
      }
    }
    break;
  case scheme_native_closure_type:
    {
      Scheme_Native_Closure *c1 = (Scheme_Native_Closure *)v1;
      Scheme_Native_Closure *c2 = (Scheme_Native_Closure *)v2;

      if (SAME_OBJ(c1->code, c2->code)) {
	int i;
	i = c1->code->closure_size;
	if (i < 0) {
	  /* A case closure */
	  Scheme_Native_Closure *sc1, *sc2;
	  int j;
	  i = -(i + 1);
	  while (i--) {
	    sc1 = (Scheme_Native_Closure *)c1->vals[i];
	    sc2 = (Scheme_Native_Closure *)c2->vals[i];
	    j = sc1->code->closure_size;
	    while (j--) {
	      if (!SAME_OBJ(sc1->vals[j], sc2->vals[j]))
		return scheme_false;
	    }
	  }
	} else {
	  /* Normal closure: */
	  while (i--) {
	    if (!SAME_OBJ(c1->vals[i], c2->vals[i]))
	      return scheme_false;
	  }
	}
	return scheme_true;
      }
    }
    break;
  case scheme_case_closure_type:
    {
      Scheme_Case_Lambda *c1 = (Scheme_Case_Lambda *)v1;
      Scheme_Case_Lambda *c2 = (Scheme_Case_Lambda *)v2;
      if (c1->count == c2->count) {
	Scheme_Closure *sc1, *sc2;
	int i, j;
	for (i = c1->count; i--; ) {
	  sc1 = (Scheme_Closure *)c1->array[i];
	  sc2 = (Scheme_Closure *)c2->array[i];
	  if (!SAME_OBJ(sc1->code, sc2->code))
	    return scheme_false;
	  for (j = sc1->code->closure_size; j--; ) {
	    if (!SAME_OBJ(sc1->vals[j], sc2->vals[j]))
	      return scheme_false;
	  }
	}
	return scheme_true;
      }
    }
    break;
  }

  return scheme_false;
}

static Scheme_Object *
apply(int argc, Scheme_Object *argv[])
{
  Scheme_Object *rands;
  Scheme_Object **rand_vec;
  int i, num_rands;
  Scheme_Thread *p = scheme_current_thread;

  if (!SCHEME_PROCP(argv[0])) {
    scheme_wrong_type("apply", "procedure", 0, argc, argv);
    return NULL;
  }

  rands = argv[argc-1];

  num_rands = scheme_proper_list_length(rands);
  if (num_rands < 0) {
    scheme_wrong_type("apply", "proper list", argc - 1, argc, argv);
    return NULL;
  }
  num_rands += (argc - 2);

  if (num_rands > p->tail_buffer_size) {
    rand_vec = MALLOC_N(Scheme_Object *, num_rands);
    /* num_rands might be very big, so don't install it as the tail buffer */
  } else
    rand_vec = p->tail_buffer;

  for (i = argc - 2; i--; ) {
    rand_vec[i] = argv[i + 1];
  }

  for (i = argc - 2; SCHEME_PAIRP(rands); i++, rands = SCHEME_CDR(rands)) {
    rand_vec[i] = SCHEME_CAR(rands);
  }

  p->ku.apply.tail_rator = argv[0];
  p->ku.apply.tail_rands = rand_vec;
  p->ku.apply.tail_num_rands = num_rands;

  return SCHEME_TAIL_CALL_WAITING;
}

#define DO_MAP map
#define MAP_NAME "map"
#define MAP_MODE
#include "schmap.inc"
#undef MAP_MODE
#undef MAP_NAME
#undef DO_MAP

#define DO_MAP for_each
#define MAP_NAME "for-each"
#define FOR_EACH_MODE
#include "schmap.inc"
#undef FOR_EACH_MODE
#undef MAP_NAME
#undef DO_MAP

#define DO_MAP andmap
#define MAP_NAME "andmap"
#define AND_MODE
#include "schmap.inc"
#undef AND_MODE
#undef MAP_NAME
#undef DO_MAP

#define DO_MAP ormap
#define MAP_NAME "ormap"
#define OR_MODE
#include "schmap.inc"
#undef OR_MODE
#undef MAP_NAME
#undef DO_MAP

static Scheme_Object *call_with_values(int argc, Scheme_Object *argv[])
{
  Scheme_Thread *p;
  Scheme_Object *v;

  scheme_check_proc_arity("call-with-values", 0, 0, argc, argv);
  if (!SCHEME_PROCP(argv[1]))
    scheme_wrong_type("call-with-values", "procedure", 1, argc, argv);

  v = _scheme_apply_multi(argv[0], 0, NULL);
  p = scheme_current_thread;
  if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES)) {
    int n;
    Scheme_Object **a;
    if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
      p->values_buffer = NULL;
    /* Beware: the fields overlap! */
    n = p->ku.multiple.count;
    a = p->ku.multiple.array;
    p->ku.apply.tail_num_rands = n;
    p->ku.apply.tail_rands = a;
  } else {
    p->ku.apply.tail_num_rands = 1;
    p->ku.apply.tail_rands = p->tail_buffer;
    p->ku.apply.tail_rands[0] = v;
  }

  p->ku.apply.tail_rator = argv[1];

  return SCHEME_TAIL_CALL_WAITING;
}

static MZ_INLINE Scheme_Object *values_slow(int argc, Scheme_Object *argv[])
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object **a;
  int i;

  a = MALLOC_N(Scheme_Object *, argc);
  p->values_buffer = a;
  p->values_buffer_size = argc;

  p->ku.multiple.array = a;

  for (i = 0; i < argc; i++) {
    a[i] = argv[i];
  }

  return SCHEME_MULTIPLE_VALUES;
}

Scheme_Object *scheme_values(int argc, Scheme_Object *argv[])
{
  Scheme_Thread *p;
  int i;
  Scheme_Object **a;

  if (argc == 1)
    return argv[0];

  p = scheme_current_thread;
  p->ku.multiple.count = argc;
  if (p->values_buffer && (p->values_buffer_size >= argc)) {
    a = p->values_buffer;
  } else {
    return values_slow(argc, argv);
  }

  p->ku.multiple.array = a;

  for (i = 0; i < argc; i++) {
    a[i] = argv[i];
  }

  return SCHEME_MULTIPLE_VALUES;
}

/*========================================================================*/
/*                             continuations                              */
/*========================================================================*/

static void reset_cjs(Scheme_Continuation_Jump_State *a)
{
  a->jumping_to_continuation = NULL;
  a->val = NULL;
  a->num_vals = 0;
  a->is_kill = 0;
  a->is_escape = 0;
}

void scheme_clear_escape(void)
{
  Scheme_Thread *p = scheme_current_thread;

  reset_cjs(&p->cjs);
  p->suspend_break = 0;
}

static void copy_cjs(Scheme_Continuation_Jump_State *a, Scheme_Continuation_Jump_State *b)
{
  a->jumping_to_continuation = b->jumping_to_continuation;
  a->val = b->val;
  a->num_vals = b->num_vals;
  a->is_kill = b->is_kill;
  a->is_escape = b->is_escape;
}

Scheme_Object *
scheme_call_ec (int argc, Scheme_Object *argv[])
{
  mz_jmp_buf newbuf;
  Scheme_Escaping_Cont * volatile cont;
  Scheme_Thread *p1 = scheme_current_thread;
  Scheme_Object * volatile v;
  Scheme_Object *a[1];
  Scheme_Cont_Frame_Data cframe;
  Scheme_Prompt *barrier_prompt;

  scheme_check_proc_arity("call-with-escape-continuation", 1,
			  0, argc, argv);

  cont = MALLOC_ONE_TAGGED(Scheme_Escaping_Cont);
  cont->so.type = scheme_escaping_cont_type;
  ASSERT_SUSPEND_BREAK_ZERO();

  cont->saveerr = p1->error_buf;
  p1->error_buf = &newbuf;

  scheme_save_env_stack_w_thread(cont->envss, p1);

  barrier_prompt = scheme_get_barrier_prompt(NULL, NULL);
  cont->barrier_prompt = barrier_prompt;

  scheme_prompt_capture_count++;

  scheme_push_continuation_frame(&cframe);
  scheme_set_cont_mark((Scheme_Object *)cont, scheme_true);

  if (scheme_setjmp(newbuf)) {
    Scheme_Thread *p2 = scheme_current_thread;
    if (p2->cjs.jumping_to_continuation
	&& SAME_OBJ(p2->cjs.jumping_to_continuation, (Scheme_Object *)cont)) {
      int n = p2->cjs.num_vals;
      v = p2->cjs.val;
      reset_cjs(&p2->cjs);
      scheme_restore_env_stack_w_thread(cont->envss, p2);
      p2->suspend_break = 0;
      if (n != 1)
        v = scheme_values(n, (Scheme_Object **)v);
    } else {
      scheme_longjmp(*cont->saveerr, 1);
    }
  } else {
    a[0] = (Scheme_Object *)cont;
    v = _scheme_apply_multi(argv[0], 1, a);
  }

  p1 = scheme_current_thread;

  p1->error_buf = cont->saveerr;
  scheme_pop_continuation_frame(&cframe);

  return v;
}

int scheme_escape_continuation_ok(Scheme_Object *ec)
{
  Scheme_Escaping_Cont *cont = (Scheme_Escaping_Cont *)ec;

  if (scheme_extract_one_cc_mark(NULL, (Scheme_Object *)cont))
    return 1;
  else
    return 0;
}

static Scheme_Object *
do_call_with_sema(const char *who, int enable_break, int argc, Scheme_Object *argv[])
{
  mz_jmp_buf newbuf, * volatile savebuf;
  Scheme_Prompt * volatile prompt;
  int i, just_try;
  int volatile extra;
  Scheme_Object * volatile sema;
  Scheme_Object *v, *quick_args[4], **extra_args;
  Scheme_Cont_Frame_Data cframe;
  int old_pcc = scheme_prompt_capture_count;

  if (!SCHEME_SEMAP(argv[0])) {
    scheme_wrong_type(who, "semaphore", 0, argc, argv);
    return NULL;
  }
  if (argc > 2)
    extra = argc - 3;
  else
    extra = 0;
  if (!scheme_check_proc_arity(NULL, extra, 1, argc, argv)) {
    scheme_wrong_type(who, "procedure (arity matching extra args)", 1, argc, argv);
    return NULL;
  }
  if ((argc > 2) && SCHEME_TRUEP(argv[2])) {
    if (!scheme_check_proc_arity(NULL, 0, 2, argc, argv)) {
      scheme_wrong_type(who, "procedure (arity 0) or #f", 1, argc, argv);
      return NULL;
    }
    just_try = 1;
  } else
    just_try = 0;

  sema = argv[0];

  if (just_try && enable_break && scheme_current_thread->external_break) {
    /* Check for a break before polling the semaphore */
    Scheme_Cont_Frame_Data bcframe;
    scheme_push_break_enable(&bcframe, 1, 1);
    scheme_check_break_now();
    scheme_pop_break_enable(&bcframe, 0);
  }

  if (!scheme_wait_sema(sema, just_try ? 1 : (enable_break ? -1 : 0))) {
    return _scheme_tail_apply(argv[2], 0, NULL);
  }

  savebuf = scheme_current_thread->error_buf;
  scheme_current_thread->error_buf = &newbuf;

  if (available_cws_prompt) {
    prompt = available_cws_prompt;
    available_cws_prompt = NULL;
  } else {
    prompt = MALLOC_ONE_TAGGED(Scheme_Prompt);
    prompt->so.type = scheme_prompt_type;
  }

  scheme_push_continuation_frame(&cframe);
  scheme_set_cont_mark(barrier_prompt_key, (Scheme_Object *)prompt);

  if (scheme_setjmp(newbuf)) {
    v = NULL;
  } else {
    if (extra > 4)
      extra_args = MALLOC_N(Scheme_Object *, extra);
    else
      extra_args = quick_args;
    for (i = 3; i < argc; i++) {
      extra_args[i - 3] = argv[i];
    }

    v = _scheme_apply_multi(argv[1], extra, extra_args);
  }

  scheme_pop_continuation_frame(&cframe);

  scheme_post_sema(sema); /* FIXME: what if we reach the max count? */

  if (old_pcc != scheme_prompt_capture_count)
    available_cws_prompt = prompt;

  if (!v)
    scheme_longjmp(*savebuf, 1);

  scheme_current_thread->error_buf = savebuf;

  return v;
}

static Scheme_Object *
call_with_sema(int argc, Scheme_Object *argv[])
{
  return do_call_with_sema("call-with-semaphore", 0, argc, argv);
}

static Scheme_Object *
call_with_sema_enable_break(int argc, Scheme_Object *argv[])
{
  return do_call_with_sema("call-with-semaphore/enable-break", 1, argc, argv);
}

static Scheme_Saved_Stack *copy_out_runstack(Scheme_Thread *p,
					     Scheme_Object **runstack,
					     Scheme_Object **runstack_start,
					     Scheme_Cont *share_from,
					     Scheme_Prompt *effective_prompt)
{
  Scheme_Saved_Stack *saved, *isaved, *csaved, *share_saved, *share_csaved, *ss;
  Scheme_Object **start;
  long size;
  int done;

  /* Copy out current runstack: */
  saved = MALLOC_ONE_RT(Scheme_Saved_Stack);
#ifdef MZTAG_REQUIRED
  saved->type = scheme_rt_saved_stack;
#endif
  if (share_from && (share_from->runstack_start == runstack_start)) {
    /* Copy just the difference between share_from's runstack and current runstack... */
    size = (share_from->ss.runstack_offset - (runstack XFORM_OK_MINUS runstack_start));
    /* But add one, because call/cc takes one argument. If there's not one
       move value on the stack, then call/cc must have received its argument
       from elsewhere. */
    if (share_from->ss.runstack_offset < p->runstack_size)
      size++;
  } else if (effective_prompt && (effective_prompt->runstack_boundary_start == runstack_start)) {
    /* Copy only up to the prompt */
    size = effective_prompt->runstack_boundary_offset - (runstack XFORM_OK_MINUS runstack_start);
  } else {
    size = p->runstack_size - (runstack XFORM_OK_MINUS runstack_start);
  }

  saved->runstack_size = size;
  start = MALLOC_N(Scheme_Object*, size);
  saved->runstack_start = start;
  memcpy(saved->runstack_start, runstack, size * sizeof(Scheme_Object *));

  if (!effective_prompt || (effective_prompt->runstack_boundary_start != runstack_start)) {

    /* Copy saved runstacks: */
    if (share_from) {
      /* We can share all saved runstacks */
      share_csaved = share_from->runstack_saved;
      share_saved = share_from->runstack_copied->prev;
    } else {
      share_saved = NULL;
      share_csaved = NULL;
    }
    isaved = saved;
    for (csaved = p->runstack_saved; csaved; csaved = csaved->prev) {
      if (share_csaved && (csaved->runstack_start == share_csaved->runstack_start)) {
	/* Share */
	isaved->prev = share_saved;
	break;
      }
    
      ss = MALLOC_ONE_RT(Scheme_Saved_Stack);
#ifdef MZTAG_REQUIRED
      ss->type = scheme_rt_saved_stack;
#endif
      isaved->prev = ss;
      isaved = ss;

      if (effective_prompt && (effective_prompt->runstack_boundary_start == csaved->runstack_start)) {
	size = effective_prompt->runstack_boundary_offset - csaved->runstack_offset;
	done = 1;
      } else {
	size = csaved->runstack_size - csaved->runstack_offset;
	done = 0;
      }

      isaved->runstack_size = size;
      
      start = MALLOC_N(Scheme_Object*, size);
      isaved->runstack_start = start;
      memcpy(isaved->runstack_start, 
	     csaved->runstack_start XFORM_OK_PLUS csaved->runstack_offset, 
	     size * sizeof(Scheme_Object *));
      isaved->runstack_offset = csaved->runstack_offset;

      if (done) break;
    }
  }

  return saved;
}

static Scheme_Cont_Mark *copy_out_mark_stack(Scheme_Thread *p, 
					     MZ_MARK_STACK_TYPE pos,
					     Scheme_Cont *sub_cont,
					     long *_offset,
					     Scheme_Prompt *effective_prompt,
                                             int clear_caches)
{
  long cmcount, offset = 0, sub_count = 0;
  Scheme_Cont_Mark *cont_mark_stack_copied;

  /* Copy cont mark stack: */
  cmcount = (long)pos;
  offset = 0;

  if (sub_cont) {
    /* Rely on copy of marks in a tail of this continuation. */
    sub_count = sub_cont->cont_mark_total - sub_cont->cont_mark_nonshare;
    if (sub_count < 0)
      sub_count = 0;
  } else if (effective_prompt) {
    /* Copy only marks since the prompt. */
    sub_count = effective_prompt->mark_boundary;
  }
  cmcount -= sub_count;
  offset += sub_count; 

  if (_offset) *_offset = offset;

  if (cmcount) {
    cont_mark_stack_copied = MALLOC_N(Scheme_Cont_Mark, cmcount);
    while (cmcount--) {
      int cms = cmcount + offset;
      Scheme_Cont_Mark *seg = p->cont_mark_stack_segments[cms >> SCHEME_LOG_MARK_SEGMENT_SIZE];
      long pos = cms & SCHEME_MARK_SEGMENT_MASK;
      Scheme_Cont_Mark *cm = seg + pos;
      
      memcpy(cont_mark_stack_copied + cmcount, cm, sizeof(Scheme_Cont_Mark));
      if (clear_caches)
        cont_mark_stack_copied[cmcount].cache = NULL;
    }
    
    return cont_mark_stack_copied;
  } else
    return NULL;
}

static void copy_in_runstack(Scheme_Thread *p, Scheme_Saved_Stack *isaved, int set_runstack)
{
  Scheme_Saved_Stack *csaved;
  long size;

  size = isaved->runstack_size;
  if (set_runstack) {
    MZ_RUNSTACK = MZ_RUNSTACK_START + (p->runstack_size - size);
  }
  memcpy(MZ_RUNSTACK, isaved->runstack_start, size * sizeof(Scheme_Object *));
  for (csaved = p->runstack_saved; csaved; csaved = csaved->prev) {
    isaved = isaved->prev;
    if (!isaved) {
      /* The saved stack can be shorter than the current stack if
         there's a barrier prompt, or if we're in shortcut mode. */
      break;
    }
    size = isaved->runstack_size;
    csaved->runstack_offset = isaved->runstack_offset;
    memcpy(csaved->runstack_start XFORM_OK_PLUS csaved->runstack_offset, 
	   isaved->runstack_start, 
	   size * sizeof(Scheme_Object *));
  }
}

static void copy_in_mark_stack(Scheme_Thread *p, Scheme_Cont_Mark *cont_mark_stack_copied,
			       MZ_MARK_STACK_TYPE cms, MZ_MARK_STACK_TYPE base_cms,
			       long copied_offset, Scheme_Object **_sub_conts,
                               int clear_caches)
     /* Copies in the mark stack up to depth cms, but assumes that the
	stack up to depth base_cms is already in place (probably in
	place for a dynamic-wind context in an continuation
	restoration.) */
{
  long cmcount, base_cmcount, cmoffset;
  Scheme_Cont_Mark *cm_src;
  Scheme_Cont *sub_cont = NULL;

  cmcount = (long)cms;
  base_cmcount = (long)base_cms;

  if (cmcount) {
    /* First, make sure we have enough segments */
    long needed = ((cmcount - 1) >> SCHEME_LOG_MARK_SEGMENT_SIZE) + 1;

    if (needed > p->cont_mark_seg_count) {
      Scheme_Cont_Mark **segs, **old_segs = p->cont_mark_stack_segments;
      int newcount = needed, oldcount = p->cont_mark_seg_count, npos;

      /* Note: we perform allocations before changing p to avoid GC trouble,
	 since MzScheme adjusts a thread's cont_mark_stack_segments on GC. */
      segs = MALLOC_N(Scheme_Cont_Mark *, needed);

      for (npos = needed; npos--; ) {
	if (npos < oldcount)
	  segs[npos] = old_segs[npos]; /* might be NULL due to GC! */
	else
	  segs[npos] = NULL;

	if (!segs[npos]) {
	  Scheme_Cont_Mark *cm;
	  cm = scheme_malloc_allow_interior(sizeof(Scheme_Cont_Mark) * SCHEME_MARK_SEGMENT_SIZE);
	  segs[npos] = cm;
	}
      }

      p->cont_mark_seg_count = newcount;
      p->cont_mark_stack_segments = segs;
    }
  }

  if (_sub_conts) {
    if (*_sub_conts) {
      sub_cont = (Scheme_Cont *)SCHEME_CAR(*_sub_conts);
    }
  }

  while (base_cmcount < cmcount) {
    Scheme_Cont_Mark *seg = p->cont_mark_stack_segments[base_cmcount >> SCHEME_LOG_MARK_SEGMENT_SIZE];
    long pos = base_cmcount & SCHEME_MARK_SEGMENT_MASK;
    GC_CAN_IGNORE Scheme_Cont_Mark *cm = seg + pos;
    
    cm_src = cont_mark_stack_copied;
    cmoffset = base_cmcount - copied_offset;

    if (sub_cont) {
      while (base_cmcount >= (sub_cont->cont_mark_total - sub_cont->cont_mark_nonshare)) {
	*_sub_conts = SCHEME_CDR(*_sub_conts);
	if (*_sub_conts) {
	  sub_cont = (Scheme_Cont *)SCHEME_CAR(*_sub_conts);
	} else {
	  sub_cont = NULL;
	  break;
	}
      }
      if (sub_cont) {
	cm_src = sub_cont->cont_mark_stack_copied;
	cmoffset = base_cmcount - sub_cont->cont_mark_offset;
      }
    }

    memcpy(cm, cm_src + cmoffset, sizeof(Scheme_Cont_Mark));
    if (clear_caches) {
      cm->cache = NULL;
    }

    base_cmcount++;
  }
}

static MZ_MARK_STACK_TYPE find_shareable_marks()
{
  Scheme_Thread *p = scheme_current_thread;
  long cmcount, delta = 0;

  cmcount = (long)MZ_CONT_MARK_STACK;

  while (cmcount--) {
    Scheme_Cont_Mark *seg = p->cont_mark_stack_segments[cmcount >> SCHEME_LOG_MARK_SEGMENT_SIZE];
    long pos = cmcount & SCHEME_MARK_SEGMENT_MASK;

    if (seg[pos].pos < MZ_CONT_MARK_POS)
      break;
    if (SAME_OBJ(seg[pos].key, cont_key))
      delta = 1;
    else
      delta = 0;
  }

  return cmcount + 1 + delta;
}

static Scheme_Overflow *clone_overflows(Scheme_Overflow *overflow, void *limit, Scheme_Overflow *tail)
{
  Scheme_Overflow *naya, *first = NULL, *prev = NULL;

  for (; overflow && (!limit || (overflow->id != limit)); overflow = overflow->prev) {
    naya = MALLOC_ONE_RT(Scheme_Overflow);
    memcpy(naya, overflow, sizeof(Scheme_Overflow));
    if (prev)
      prev->prev = naya;
    else
      first = naya;
    prev = naya;
  }

  if (first) {
    prev->prev = tail;
    return first;
  } else
    return tail;
}

static Scheme_Dynamic_Wind *clone_dyn_wind(Scheme_Dynamic_Wind *dw, 
                                           Scheme_Object *limit_prompt_tag, int limit_depth,
                                           Scheme_Dynamic_Wind *tail, 
                                           int keep_tail, int composable)
{
  Scheme_Dynamic_Wind *naya, *first = NULL, *prev = NULL;
  int cnt = 0;

  for (; dw; dw = dw->prev) {
    if (dw->depth == limit_depth)
      break;
    if (composable && limit_prompt_tag && (dw->prompt_tag == limit_prompt_tag))
      break;
    scheme_ensure_dw_id(dw);
    naya = MALLOC_ONE_RT(Scheme_Dynamic_Wind);
    memcpy(naya, dw, sizeof(Scheme_Dynamic_Wind));
    if (prev)
      prev->prev = naya;
    else
      first = naya;
    prev = naya;
    cnt++;
    if (limit_prompt_tag && (dw->prompt_tag == limit_prompt_tag)) {
      dw = dw->prev; /* in case keep_tail is true */
      break;
    }
  }
  if (keep_tail)
    tail = dw;
  if (first) {
    prev->prev = tail;
    if (tail)
      cnt += tail->depth + 1;
    for (dw = first; dw != tail; dw = dw->prev) {
      dw->depth = --cnt;
    }
    return first;
  } else
    return tail;
}

static void clear_cm_copy_caches(Scheme_Cont_Mark *cp, int cnt)
{
  int i;
  for (i = 0; i < cnt; i++) {
    cp[i].cache = NULL;
  }
}

static Scheme_Meta_Continuation *clone_meta_cont(Scheme_Meta_Continuation *mc,
                                                 Scheme_Object *limit_tag, int limit_depth,
                                                 Scheme_Meta_Continuation *prompt_cont,
                                                 Scheme_Prompt *prompt,
                                                 Scheme_Meta_Continuation *tail,
                                                 int for_composable)
{
  Scheme_Meta_Continuation *naya, *first = NULL, *prev = NULL;
  int cnt = 0, depth;

  for (; mc; mc = mc->next) {
    if (!limit_depth--)
      break;
    if (!mc->pseudo && SAME_OBJ(mc->prompt_tag, limit_tag))
      break;
    if (for_composable && mc->pseudo && mc->empty_to_next && mc->next  
        && SAME_OBJ(mc->next->prompt_tag, limit_tag)) {
      /* We don't need to keep the compose-introduced
         meta-continuation, because it represents an empty
         continuation relative to the prompt. */
      break;
    }
    naya = MALLOC_ONE_RT(Scheme_Meta_Continuation);
    cnt++;
    memcpy(naya, mc, sizeof(Scheme_Meta_Continuation));
    if (SAME_OBJ(mc, prompt_cont)) {
      /* Need only part of this meta-continuation's marks. */
      long delta;
      delta = prompt->mark_boundary - naya->cont_mark_offset;
      if (delta) {
        naya->cont_mark_total -= delta;
        naya->cont_mark_offset += delta;
        if (naya->cont_mark_total) {
          Scheme_Cont_Mark *cp;
          cp = MALLOC_N(Scheme_Cont_Mark, naya->cont_mark_total);
          memcpy(cp, mc->cont_mark_stack_copied + delta, naya->cont_mark_total * sizeof(Scheme_Cont_Mark));
          if (mc->cm_caches) {
            clear_cm_copy_caches(cp, naya->cont_mark_total);
          }
          naya->cont_mark_stack_copied = cp;
          naya->cm_caches = 0;
          naya->cm_shared = 0;
        } else
          naya->cont_mark_stack_copied = NULL;
      }
      naya->cont_mark_pos_bottom = prompt->boundary_mark_pos;
    } else {
      if (!mc->cm_caches) {
        mc->cm_shared = 1;
        naya->cm_shared = 1;
      } else {
        Scheme_Cont_Mark *cp;
        cp = MALLOC_N(Scheme_Cont_Mark, naya->cont_mark_total);
        memcpy(cp, mc->cont_mark_stack_copied, naya->cont_mark_total * sizeof(Scheme_Cont_Mark));
        clear_cm_copy_caches(cp, naya->cont_mark_total);
        naya->cont_mark_stack_copied = cp;
        naya->cm_caches = 0;
        naya->cm_shared = 0;
      }
    }
    if (prev)
      prev->next = naya;
    else
      first = naya;
    prev = naya;
  }

  if (first) {
    prev->next = tail;
  } else
    first = tail;

  /* Set depth for newly prefixed meta-conts: */
  if (tail)
    depth = tail->depth + 1;
  else
    depth = 0;
  for (naya = first; cnt--; naya = naya->next) {
    naya->depth = depth + cnt;
  }

  return first;
}

void prune_cont_marks(Scheme_Meta_Continuation *resume_mc, Scheme_Cont *cont, Scheme_Object *extra_marks)
{
  Scheme_Object *val;
  Scheme_Hash_Table *ht;
  long pos, num_overlap, num_coverlap, new_overlap, base, i;
  Scheme_Cont_Mark *cp;
  
  for (pos = resume_mc->cont_mark_total, num_overlap = 0;
       pos--;
       num_overlap++) {
    if (resume_mc->cont_mark_stack_copied[pos].pos != resume_mc->cont_mark_pos)
      break;
  }

  if (!num_overlap && (!extra_marks || !SCHEME_VEC_SIZE(extra_marks))) {
    /* No pruning (nothing to prune) or addition needed. */
    return;
  }

  for (pos = cont->cont_mark_total, num_coverlap = 0;
       pos--;
       num_coverlap++) {
    if (cont->cont_mark_stack_copied[pos].pos != (cont->cont_mark_pos_bottom + 2))
      break;
  }

  if (!num_coverlap && (!extra_marks || !SCHEME_VEC_SIZE(extra_marks))) {
    /* No pruning (nothing to compare against) or addition needed. */
    return;
  }

  /* Compute the new set to have in the meta-continuation. */
  ht = scheme_make_hash_table(SCHEME_hash_ptr);
  
  for (pos = resume_mc->cont_mark_total - 1, i = 0; i < num_overlap; i++, pos--) {
    val = resume_mc->cont_mark_stack_copied[pos].val;
    if (!val)
      val = cont_key;
    scheme_hash_set(ht, 
                    resume_mc->cont_mark_stack_copied[pos].key,
                    val);
  }
  if (extra_marks) {
    for (i = 0; i < SCHEME_VEC_SIZE(extra_marks); i += 2) {
      val = SCHEME_VEC_ELS(extra_marks)[i+1];
      if (!val)
        val = cont_key;
      scheme_hash_set(ht, SCHEME_VEC_ELS(extra_marks)[i], val);
    }
  }
  for (pos = cont->cont_mark_total - 1, i = 0; i < num_coverlap; i++, pos--) {
    scheme_hash_set(ht, 
                    cont->cont_mark_stack_copied[pos].key,
                    NULL);
  }

  new_overlap = ht->count;

  /* Install changes: */
  base = resume_mc->cont_mark_total - num_overlap;
  cp = MALLOC_N(Scheme_Cont_Mark, base + new_overlap);
  memcpy(cp, resume_mc->cont_mark_stack_copied, base * sizeof(Scheme_Cont_Mark));
  resume_mc->cont_mark_stack_copied = cp;
  resume_mc->cont_mark_total = base + new_overlap;
  resume_mc->cm_shared = 0;
  resume_mc->cont_mark_stack += (new_overlap - num_overlap);
  for (i = 0; i < ht->size; i++) {
    if (ht->vals[i]) {
      cp[base].key = ht->keys[i];
      val = ht->vals[i];
      if (SAME_OBJ(val, cont_key))
        val = NULL;
      cp[base].val = val;
      cp[base].pos = resume_mc->cont_mark_pos;
      cp[base].cache = NULL;
      base++;
    }
  }
}

Scheme_Saved_Stack *clone_runstack_saved(Scheme_Saved_Stack *saved, Scheme_Object **boundary_start,
                                         Scheme_Saved_Stack *last)
{
  Scheme_Saved_Stack *naya, *first = last, *prev = NULL;

  while (saved) {
    naya = MALLOC_ONE_RT(Scheme_Saved_Stack);
    memcpy(naya, saved, sizeof(Scheme_Saved_Stack));
    if (prev)
      prev->prev = naya;
    else
      first = naya;
    prev = naya;
    if (saved->runstack_start == boundary_start)
      break;
    saved = saved->prev;
  }
  if (prev)
    prev->prev = last;
  
  return first;
}

static MZ_MARK_STACK_TYPE exec_dyn_wind_pres(Scheme_Dynamic_Wind_List *dwl,
                                             int dwl_len,
                                             Scheme_Cont *cont,
                                             MZ_MARK_STACK_TYPE copied_cms,
                                             int clear_cm_caches,
                                             Scheme_Object **_sub_conts)
{
  Scheme_Thread *p = scheme_current_thread;
  int old_cac = scheme_continuation_application_count;

  for (; dwl; dwl = dwl->next) {
    if (dwl->dw->pre) {
      p->dw = dwl->dw->prev;
      p->next_meta = dwl->meta_depth + dwl->dw->next_meta;
      if (dwl->meta_depth > 0) {
        scheme_apply_dw_in_meta(dwl->dw, 0, dwl->meta_depth, cont);
      } else {
        /* Restore the needed part of the mark stack for this
           dynamic-wind context. Clear cached info on restore
           if there's a prompt. */
        DW_PrePost_Proc pre = dwl->dw->pre;
        MZ_CONT_MARK_POS = dwl->dw->envss.cont_mark_pos;
        MZ_CONT_MARK_STACK = dwl->dw->envss.cont_mark_stack;
        copy_in_mark_stack(p, cont->cont_mark_stack_copied, 
                           MZ_CONT_MARK_STACK, copied_cms,
                           cont->cont_mark_offset, _sub_conts,
                           clear_cm_caches);
        copied_cms = MZ_CONT_MARK_STACK;

        pre(dwl->dw->data);

        if (scheme_continuation_application_count != old_cac) {
          old_cac = scheme_continuation_application_count;
          scheme_recheck_prompt_and_barrier(cont);
        }
      }
      p = scheme_current_thread;
    }
  }
  return copied_cms;
}

static Scheme_Object *
call_cc (int argc, Scheme_Object *argv[])
{
  scheme_check_proc_arity("call-with-current-continuation", 1,
			  0, argc, argv);
  if (argc > 1) {
    if (!SAME_TYPE(scheme_prompt_tag_type, SCHEME_TYPE(argv[1]))) {
      scheme_wrong_type("call-with-current-continuation", "continuation-prompt-tag",
                        1, argc, argv);
    }
  }

  /* Trampoline to internal_call_cc. This trampoline ensures that
     the runstack is flushed before we try to grab the continuation. */
  return _scheme_tail_apply(internal_call_cc_prim, argc, argv);
}

static Scheme_Cont *grab_continuation(Scheme_Thread *p, int for_prompt, int composable,
                                      Scheme_Object *prompt_tag,
                                      Scheme_Cont *sub_cont, Scheme_Prompt *prompt,
                                      Scheme_Meta_Continuation *prompt_cont, MZ_MARK_POS_TYPE prompt_pos,
                                      Scheme_Prompt *barrier_prompt, Scheme_Prompt *effective_barrier_prompt,
                                      Scheme_Meta_Continuation *barrier_cont, MZ_MARK_POS_TYPE barrier_pos)
{
  Scheme_Cont *cont;
  
  cont = MALLOC_ONE_TAGGED(Scheme_Cont);
  cont->so.type = scheme_cont_type;

  if (!for_prompt && !composable) {
    /* Set cont_key mark before capturing marks: */
    scheme_set_cont_mark(cont_key, (Scheme_Object *)cont);
  }

  if (composable)
    cont->composable = 1;

  scheme_init_jmpup_buf(&cont->buf);
  cont->prompt_tag = prompt_tag;
  if (for_prompt)
    cont->dw = NULL;
  else if (prompt) {
    Scheme_Dynamic_Wind *dw;
    if (p->dw) {
      dw = clone_dyn_wind(p->dw, prompt_tag, -1, NULL, 0, composable);
      cont->dw = dw;
      cont->next_meta = p->next_meta;
    } else
      cont->dw = NULL;
  } else {
    cont->dw = p->dw;
    cont->next_meta = p->next_meta;
  }
  if (!for_prompt)
    ASSERT_SUSPEND_BREAK_ZERO();
  copy_cjs(&cont->cjs, &p->cjs);
  cont->save_overflow = p->overflow;
  scheme_save_env_stack_w_thread(cont->ss, p);
  cont->runstack_size = p->runstack_size;
  cont->runstack_start = MZ_RUNSTACK_START;
  cont->runstack_saved = p->runstack_saved;
  cont->meta_tail_pos = (prompt ? prompt->boundary_mark_pos + 2 : 0);
  cont->init_config = p->init_config;
  cont->init_break_cell = p->init_break_cell;
  if (for_prompt) {
    cont->meta_continuation = NULL;
  } else if (prompt) {
    Scheme_Meta_Continuation *mc;
    Scheme_Object *id;
    mc = clone_meta_cont(p->meta_continuation, prompt_tag, -1, prompt_cont, prompt, NULL, composable);
    cont->meta_continuation = mc;
    if (!prompt_cont) {
      /* Remember the prompt id, so we can maybe take a shortcut on 
         invocation. (The shortcut only works within a meta-continuation.) */
      if (!prompt->id) {
        id = scheme_make_pair(scheme_false, scheme_false);
        prompt->id = id;
      }
      cont->prompt_id = prompt->id;
    }
    cont->has_prompt_dw = 1;
  } else
    cont->meta_continuation = p->meta_continuation;

  if (effective_barrier_prompt) {
    cont->barrier_prompt = effective_barrier_prompt;
    scheme_prompt_capture_count++;
  }

  if (p->meta_prompt && prompt_cont) /* prompt_cont => meta-prompt is shallower than prompt */
    prompt = p->meta_prompt;

  {
    Scheme_Overflow *overflow;
    /* Mark overflows as captured: */
    for (overflow = p->overflow; overflow; overflow = overflow->prev) {
      overflow->jmp->captured = 1;
    }
    /* If prompt, then clone overflow records up to the prompt. */
    if (prompt) {
      overflow = clone_overflows(p->overflow, prompt->boundary_overflow_id, NULL);
      cont->save_overflow = overflow;
    }
  }
  scheme_cont_capture_count++;

  if (!effective_barrier_prompt || !effective_barrier_prompt->is_barrier) {
    /* This continuation can be used by other threads,
       so we need to track ownership of the runstack */
    if (!p->runstack_owner) {
      Scheme_Thread **owner;
      owner = MALLOC_N(Scheme_Thread *, 1);
      p->runstack_owner = owner;
      *owner = p;
    }
    if (p->cont_mark_stack && !p->cont_mark_stack_owner) {
      Scheme_Thread **owner;
      owner = MALLOC_N(Scheme_Thread *, 1);
      p->cont_mark_stack_owner = owner;
      *owner = p;
    }
  }

#ifdef MZ_USE_JIT
  {
    Scheme_Object *tr;
    tr = scheme_native_stack_trace();
    cont->native_trace = tr;
  }
#endif

  {
    Scheme_Saved_Stack *saved;
    saved = copy_out_runstack(p, MZ_RUNSTACK, MZ_RUNSTACK_START, sub_cont, 
                              (for_prompt ? p->meta_prompt : prompt));
    cont->runstack_copied = saved;
    if (!for_prompt && prompt) {
      /* Prune cont->runstack_saved to drop unneeded saves. */
      if (SAME_OBJ(prompt->runstack_boundary_start, MZ_RUNSTACK_START))
        saved = NULL;
      else
        saved = clone_runstack_saved(cont->runstack_saved, 
                                     prompt->runstack_boundary_start,
                                     NULL);
      cont->runstack_saved = saved;
    }
  }

  {
    Scheme_Prompt *effective_prompt;
    Scheme_Cont_Mark *msaved;
    long offset;
    effective_prompt = (for_prompt ? p->meta_prompt : prompt);
    msaved = copy_out_mark_stack(p, cont->ss.cont_mark_stack, sub_cont, &offset, 
                                 effective_prompt,
                                 /* If there's a prompt, then clear caches in the mark stack,
                                    since any cached values are wrong for the delimited
                                    continuation. Otherwise, leave the cache in place
                                    for operations directly on the continuation; the caches
                                    will be cleared on restore if the continuation is appended
                                    to another on invocation. */
                                 !!prompt);
    cont->cont_mark_stack_copied = msaved;
    cont->cont_mark_offset = offset;
    if (effective_prompt)
      cont->cont_mark_total = cont->ss.cont_mark_stack - effective_prompt->mark_boundary;
    else
      cont->cont_mark_total = cont->ss.cont_mark_stack;
    offset = find_shareable_marks();
    cont->cont_mark_nonshare = cont->ss.cont_mark_stack - offset;
    /* Need to remember the pos key for the bottom, 
       at least for composable continuations, so 
       we can splice the captured continuation marks
       with a meta continuation's marks. */
    cont->cont_mark_pos_bottom = (effective_prompt
                                  ? effective_prompt->boundary_mark_pos
                                  : 1);
  }

  cont->runstack_owner = p->runstack_owner;
  cont->cont_mark_stack_owner = p->cont_mark_stack_owner;

  cont->stack_start = p->stack_start;

  cont->savebuf = p->error_buf;

  if (prompt)
    cont->prompt_buf = prompt->prompt_buf;

  return cont;
}

static void restore_continuation(Scheme_Cont *cont, Scheme_Thread *p, int for_prompt,
                                 Scheme_Object *result, 
                                 Scheme_Overflow *resume, int empty_to_next_mc,
                                 Scheme_Object *prompt_tag, Scheme_Cont *sub_cont,
                                 Scheme_Dynamic_Wind *common_dw, int common_next_meta, 
                                 Scheme_Prompt *shortcut_prompt,
                                 int clear_cm_caches, int do_reset_cjs,
                                 Scheme_Cont *cm_cont, Scheme_Object *extra_marks)
{
  MZ_MARK_STACK_TYPE copied_cms = 0;
  Scheme_Object **mv, *sub_conts = NULL;
  int mc;

  if (SAME_OBJ(result, SCHEME_MULTIPLE_VALUES)) {
    /* Get values out before GC */
    mv = p->ku.multiple.array;
    mc = p->ku.multiple.count;
    if (SAME_OBJ(mv, p->values_buffer))
      p->values_buffer = NULL;
  } else {
    mv = NULL;
    mc = 0;
  }

  p->error_buf = cont->savebuf;

  p->init_config = cont->init_config;
  p->init_break_cell = cont->init_break_cell;

  if (do_reset_cjs)
    copy_cjs(&p->cjs, &cont->cjs);
  if (shortcut_prompt) {
    Scheme_Overflow *overflow;
    overflow = clone_overflows(cont->save_overflow, NULL, p->overflow);
    p->overflow = overflow;
  } else {
    p->overflow = cont->save_overflow;
  }
  if (!for_prompt) {
    Scheme_Meta_Continuation *mc, *resume_mc;
    if (resume) {
      resume_mc = MALLOC_ONE_RT(Scheme_Meta_Continuation);
#ifdef MZTAG_REQUIRED
      resume_mc->type = scheme_rt_meta_cont;
#endif
      resume_mc->overflow = resume;

      resume_mc->prompt_tag = prompt_tag;
      resume_mc->pseudo = cont->composable;
      resume_mc->empty_to_next = empty_to_next_mc;
      resume_mc->meta_tail_pos = cont->meta_tail_pos;

      if (!cm_cont) {
        /* resume must correspond to the implicit prompt at
           the thread's beginning. */
      } else {
        resume_mc->cont_mark_stack = cm_cont->ss.cont_mark_stack;
        resume_mc->cont_mark_pos = cm_cont->ss.cont_mark_pos;
        resume_mc->cont_mark_total = cm_cont->cont_mark_total;
        resume_mc->cont_mark_offset = cm_cont->cont_mark_offset;
        resume_mc->cont_mark_pos_bottom = cm_cont->cont_mark_pos_bottom;
        resume_mc->cont_mark_stack_copied = cm_cont->cont_mark_stack_copied;

        resume_mc->cm_caches = 1; /* conservative assumption */

        resume_mc->next = p->meta_continuation;
        if (p->meta_continuation)
          resume_mc->depth = p->meta_continuation->depth + 1;
      }
    } else
      resume_mc = NULL;
    if (resume_mc) {
      if (cont->composable) {
        /* Prune resume_mc continuation marks that have replacements
           in the deepest frame of cont, and add extra_marks */
        prune_cont_marks(resume_mc, cont, extra_marks);
      }
      
      mc = clone_meta_cont(cont->meta_continuation, NULL, -1, NULL, NULL, resume_mc, 0);
    } else if (shortcut_prompt) {
      mc = clone_meta_cont(cont->meta_continuation, NULL, -1, NULL, NULL, p->meta_continuation, 0);
    } else
      mc = cont->meta_continuation;
    p->meta_continuation = mc;
  }

  if (shortcut_prompt) {
    /* In shortcut mode, we need to preserve saved runstacks
       that were pruned when capturing the continuation. */
    Scheme_Saved_Stack *rs;
    if (shortcut_prompt->runstack_boundary_start == MZ_RUNSTACK_START)
      rs = p->runstack_saved;
    else {
      rs = p->runstack_saved;
      while (rs && (rs->runstack_start != shortcut_prompt->runstack_boundary_start)) {
        rs = rs->prev;
      }
      if (rs)
        rs = rs->prev;
    }
    if (rs)
      rs = clone_runstack_saved(cont->runstack_saved, NULL, rs);
    else
      rs = cont->runstack_saved;
    p->runstack_saved = rs;
  } else
    p->runstack_saved = cont->runstack_saved;

  MZ_RUNSTACK_START = cont->runstack_start;
  p->runstack_size = cont->runstack_size;

  scheme_restore_env_stack_w_thread(cont->ss, p);

  if (p->runstack_owner
      && (*p->runstack_owner == p)) {
    *p->runstack_owner = NULL;
  }

  if (resume)
    p->meta_prompt = NULL; /* in case there's a GC before we can set it */

  p->runstack_owner = cont->runstack_owner;
  if (p->runstack_owner && (*p->runstack_owner != p)) {
    Scheme_Thread *op;
    op = *p->runstack_owner;
    if (op) {
      Scheme_Saved_Stack *saved;
      saved = copy_out_runstack(op, op->runstack, op->runstack_start, NULL, NULL);
      op->runstack_swapped = saved;
    }
    *p->runstack_owner = p;
  }

  /* Copy stack back in: p->runstack and p->runstack_saved arrays
     are already restored, so the shape is certainly the same as
     when cont->runstack_copied was made. If we have a derived
     continuation, then we're sharing it's base runstack. */
  copy_in_runstack(p, cont->runstack_copied, 0);
  {
    long done = cont->runstack_copied->runstack_size, size;
    sub_cont = cont;
    while (sub_cont) {
      if (sub_cont->buf.cont
          && (sub_cont->runstack_start == sub_cont->buf.cont->runstack_start)) {
        /* Copy shared part in: */
        sub_cont = sub_cont->buf.cont;
        size = sub_cont->runstack_copied->runstack_size;
        if (size) {
          /* Skip the first item, since that's the call/cc argument,
             which we don't want from the outer continuation. */
          memcpy(MZ_RUNSTACK XFORM_OK_PLUS done, 
                 sub_cont->runstack_copied->runstack_start + 1, 
                 (size - 1) * sizeof(Scheme_Object *));
          done += (size - 1);
        }
      } else
        break;
    }
  }
    
  if (p->cont_mark_stack_owner
      && (*p->cont_mark_stack_owner == p))
    *p->cont_mark_stack_owner = NULL;

  p->cont_mark_stack_owner = cont->cont_mark_stack_owner;
  if (p->cont_mark_stack_owner
      && (*p->cont_mark_stack_owner != p)) {
    Scheme_Thread *op;
    op = *p->cont_mark_stack_owner;
    if (op) {
      Scheme_Cont_Mark *msaved;
      msaved = copy_out_mark_stack(op, op->cont_mark_stack, NULL, NULL, NULL, 0);
      op->cont_mark_stack_swapped = msaved;
    }
    *p->cont_mark_stack_owner = p;
    /* In case there's a GC before we copy in marks: */
    MZ_CONT_MARK_STACK = 0;
  }

  /* If there's a resume, then set up a meta prompt: */
  if (resume) {
    Scheme_Prompt *meta_prompt;

    meta_prompt = MALLOC_ONE_TAGGED(Scheme_Prompt);
    meta_prompt->so.type = scheme_prompt_type;
    meta_prompt->stack_boundary = cont->prompt_stack_start;
    meta_prompt->boundary_overflow_id = NULL;
    {
      Scheme_Cont *tc;
      for (tc = cont; tc->buf.cont; tc = tc->buf.cont) {
      }
      meta_prompt->mark_boundary = tc->cont_mark_offset;
    }
    meta_prompt->prompt_buf = cont->prompt_buf;
    {
      /* Reverse-engineer where the saved runstack ends: */
      Scheme_Cont *rs_cont = cont;
      Scheme_Saved_Stack *saved, *actual;
      int delta = 0;
      while (rs_cont->buf.cont) {
        delta += rs_cont->runstack_copied->runstack_size;
        rs_cont = rs_cont->buf.cont;
        if (rs_cont->runstack_copied->runstack_size) {
          delta -= 1; /* overlap for not-saved call/cc argument */
        }
      }
      actual = NULL;
      for (saved = rs_cont->runstack_copied; saved->prev; saved = saved->prev) {
        if (!actual)
          actual = p->runstack_saved;
        else
          actual = actual->prev;
      }
      if (actual) {
        meta_prompt->runstack_boundary_start = actual->runstack_start;
        meta_prompt->runstack_boundary_offset = actual->runstack_offset + saved->runstack_size;
      } else {
        meta_prompt->runstack_boundary_start = MZ_RUNSTACK_START;
        meta_prompt->runstack_boundary_offset = (MZ_RUNSTACK - MZ_RUNSTACK_START) + saved->runstack_size + delta;
      }
    }

    p->meta_prompt = meta_prompt;
  }

  /* For copying cont marks back in, we need a list of sub_conts,
     deepest to shallowest: */
  copied_cms = cont->cont_mark_offset;
  for (sub_cont = cont->buf.cont; sub_cont; sub_cont = sub_cont->buf.cont) {
    copied_cms = sub_cont->cont_mark_offset;
    sub_conts = scheme_make_raw_pair((Scheme_Object *)sub_cont, sub_conts);
  }

  if (!shortcut_prompt) {
    Scheme_Cont *tc;
    for (tc = cont; tc->buf.cont; tc = tc->buf.cont) {
    }
    p->cont_mark_stack_bottom = tc->cont_mark_offset;
    p->cont_mark_pos_bottom = tc->cont_mark_pos_bottom;
  }

  if (for_prompt) {
    /* leave p->dw alone */
  } else {
    /* For dynamic-winds after the "common" intersection
       (see eval.c), execute the pre thunks. Make a list
       of these first because they have to be done in the
       inverse order of `prev' linkage. */
    Scheme_Dynamic_Wind *dw, *all_dw;
    Scheme_Dynamic_Wind_List *dwl = NULL;
    int common_depth, dwl_len = 0;

    /* The thread's dw is set to the common dw. */

    if (resume) {
      /* Figure out which dynamic winds use meta-continuations
         after an added one. */
      if (cont->composable) {
        /* All of them! */
        p->next_meta++;
      } else {
        /* D-Ws after the tag are now one further way:
           after the newly inserted meta-continuation for this tag. */
        p->dw = common_dw;
        p->next_meta = common_next_meta;
        if (p->dw) { /* can be empty if there's only the implicit prompt */
          /* also, there may be no dw with prompt_tag if there's only the implicit prompt */
          all_dw = clone_dyn_wind(p->dw, cont->prompt_tag, -1, NULL, 1, 0);
          for (dw = all_dw; dw && !SAME_OBJ(dw->prompt_tag, cont->prompt_tag); dw = dw->prev) {
            p->dw = p->dw->prev;
          }
          if (dw)
            dw->next_meta += 1;
          p->dw = all_dw;
        }
      }
    } else {
      p->dw = common_dw;
      p->next_meta = common_next_meta;
    }

    if (cont->dw) {
      int meta_depth;

      common_depth = (p->dw ? p->dw->depth : -1);
      all_dw = clone_dyn_wind(cont->dw, NULL, cont->common_dw_depth, p->dw, 0, 0);

      if ((common_depth != -1) && (common_depth != all_dw->depth)) {
        /* Move p->next_meta to the last added dw's next_meta. */
        for (dw = all_dw; dw->prev->depth != common_depth; dw = dw->prev) {
        }
        dw->next_meta = p->next_meta;
      }
      
      meta_depth = cont->next_meta;
      for (dw = all_dw; dw && (dw->depth != common_depth); dw = dw->prev) {
        Scheme_Dynamic_Wind_List *cell;

        cell = MALLOC_ONE_RT(Scheme_Dynamic_Wind_List);
#ifdef MZTAG_REQUIRED
        cell->type = scheme_rt_dyn_wind_cell;
#endif
        cell->dw = dw;
        cell->meta_depth = meta_depth;
        cell->next = dwl;
        dwl = cell;
        dwl_len++;

        meta_depth += dw->next_meta;
      }
      copied_cms = exec_dyn_wind_pres(dwl, dwl_len, cont, copied_cms, clear_cm_caches, &sub_conts);
      p = scheme_current_thread;
      p->dw = all_dw;
      p->next_meta = cont->next_meta;      
    }
  }

  if (!for_prompt)
    p->suspend_break = 0;

  /* Finish copying cont mark stack back in. */
    
  MZ_CONT_MARK_POS = cont->ss.cont_mark_pos;
  MZ_CONT_MARK_STACK = cont->ss.cont_mark_stack;
  copy_in_mark_stack(p, cont->cont_mark_stack_copied, 
                     MZ_CONT_MARK_STACK, copied_cms,
                     cont->cont_mark_offset, &sub_conts,
                     clear_cm_caches);
        
  if (SAME_OBJ(result, SCHEME_MULTIPLE_VALUES)) {
    p->ku.multiple.array = mv;
    p->ku.multiple.count = mc;
  }
}

static Scheme_Object *
internal_call_cc (int argc, Scheme_Object *argv[])
{
  Scheme_Object *ret, * volatile prompt_tag;
  Scheme_Cont * volatile cont;
  Scheme_Cont *sub_cont;
  Scheme_Meta_Continuation *prompt_cont, *barrier_cont;
  MZ_MARK_POS_TYPE prompt_pos, barrier_pos;
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Prompt *prompt, *barrier_prompt, *effective_barrier_prompt;
  GC_CAN_IGNORE void *stack_start;
  int composable;

  if (argc > 1)
    prompt_tag = argv[1];
  else
    prompt_tag = scheme_default_prompt_tag;

  composable = (argc > 2);

  prompt = (Scheme_Prompt *)scheme_extract_one_cc_mark_with_meta(NULL, SCHEME_PTR_VAL(prompt_tag), 
                                                                 NULL, &prompt_cont, &prompt_pos);
  if (!prompt && !SAME_OBJ(scheme_default_prompt_tag, prompt_tag)) {
    scheme_arg_mismatch((composable
                         ? "call-with-composable-continuation"
                         : "call-with-current-continuation"), 
                        "continuation includes no prompt with the given tag: ",
                        prompt_tag);
    return NULL;
  }

  barrier_prompt = scheme_get_barrier_prompt(&barrier_cont, &barrier_pos);

  if (composable) {
    if (!prompt && !barrier_prompt->is_barrier) {
      /* Pseduo-prompt ok. */
    } else {
      if (!prompt
          || scheme_is_cm_deeper(prompt_cont, prompt_pos, barrier_cont, barrier_pos)) {
        scheme_raise_exn(MZEXN_FAIL_CONTRACT_CONTINUATION,
                         "call-with-composable-continuation: cannot capture past continuation barrier");
      }
    }
  }

  effective_barrier_prompt = barrier_prompt;
  if (effective_barrier_prompt && prompt) {
    if (scheme_is_cm_deeper(barrier_cont, barrier_pos,
                            prompt_cont, prompt_pos))
      effective_barrier_prompt = NULL;
  }

  if (composable)
    sub_cont = NULL;
  else
    sub_cont = (Scheme_Cont *)scheme_extract_one_cc_mark(NULL, cont_key);
  if (sub_cont && ((sub_cont->save_overflow != p->overflow)
		   || (sub_cont->prompt_tag != prompt_tag)
		   || (sub_cont->barrier_prompt != effective_barrier_prompt))) {
    sub_cont = NULL;
  }
  if (sub_cont && (sub_cont->ss.cont_mark_pos == MZ_CONT_MARK_POS)) {
    Scheme_Object *argv2[1];
#ifdef MZ_USE_JIT
    ret = scheme_native_stack_trace();
#endif    
    /* Old cont is the same as this one, except that it may
       have different marks (not counting cont_key). */
    if (!sub_cont->cont_mark_nonshare
	&& (find_shareable_marks() == MZ_CONT_MARK_STACK)
#ifdef MZ_USE_JIT
	&& (SAME_OBJ(ret, sub_cont->native_trace)
	    /* Maybe a single-function loop, where we re-allocated the
	       last pair in the trace, but it's the same name: */
	    || (ret 
                && sub_cont->native_trace
                && SCHEME_PAIRP(ret)
		&& SCHEME_PAIRP(sub_cont->native_trace)
		&& SAME_OBJ(SCHEME_CAR(ret), SCHEME_CAR(sub_cont->native_trace))
		&& SAME_OBJ(SCHEME_CDR(ret), SCHEME_CDR(sub_cont->native_trace))))
#endif
	) {
      /* Just use this one. */
      cont = sub_cont;
    } else {
      /* Only continuation marks can be different. Mostly just re-use sub_cont. */
      long offset;
      Scheme_Cont_Mark *msaved;

      cont = MALLOC_ONE_TAGGED(Scheme_Cont);
      cont->so.type = scheme_cont_type;
      cont->buf.cont = sub_cont;
      sub_cont = sub_cont->buf.cont;

      /* This mark stack won't be restored, but it may be
	 used by `continuation-marks'. */
      cont->ss.cont_mark_stack = MZ_CONT_MARK_STACK;
      msaved = copy_out_mark_stack(p, cont->ss.cont_mark_stack, sub_cont, &offset, NULL, 0);
      cont->cont_mark_stack_copied = msaved;
      cont->cont_mark_offset = offset;
      cont->cont_mark_total = cont->ss.cont_mark_stack;
      offset = find_shareable_marks();
      cont->cont_mark_nonshare = cont->ss.cont_mark_stack - offset;
#ifdef MZ_USE_JIT
      cont->native_trace = ret;
#endif
    }

    argv2[0] = (Scheme_Object *)cont;
    return _scheme_tail_apply(argv[0], 1, argv2);
  }

  cont = grab_continuation(p, 0, composable, prompt_tag, sub_cont, 
                           prompt, prompt_cont, prompt_pos,
                           barrier_prompt, effective_barrier_prompt, barrier_cont, barrier_pos);

  scheme_zero_unneeded_rands(p);

  scheme_flatten_config(scheme_current_config());

  {
    void *overflow_id;

    overflow_id = (p->overflow
                   ? (p->overflow->id
                      ? p->overflow->id
                      : p->overflow)
                   : NULL);

    if (prompt 
        && !prompt_cont
        && (prompt->boundary_overflow_id == overflow_id)) {
      /* Must be inside barrier_prompt, or it wouldn't be allowed.
         Must be inside meta_prompt, or prompt_cont would be non-NULL.
         Must be inside overflow, or the ids wouldn't match. */
      stack_start = prompt->stack_boundary;
    } else {
      Scheme_Prompt *meta_prompt;

      if (!barrier_prompt->is_barrier)
        barrier_prompt = NULL;
      else if (barrier_prompt->boundary_overflow_id != overflow_id)
        barrier_prompt = NULL;
      meta_prompt = p->meta_prompt;
      if (meta_prompt)
        if (meta_prompt->boundary_overflow_id != overflow_id)
          meta_prompt = NULL;

      if (barrier_prompt && meta_prompt) {
        barrier_prompt = NULL;
      }

      if (barrier_prompt)
        stack_start = barrier_prompt->stack_boundary;
      else if (meta_prompt)
        stack_start = meta_prompt->stack_boundary;
      else
        stack_start = ADJUST_STACK_START(p->stack_start);
    }
  }

  /* Use cont->stack_start when calling `cont' directly
     from the same meta-continuation. Use cont->prompt_stack_start 
     when calling `cont' composably (i.e., when supplying a resume). */
  cont->prompt_stack_start = stack_start;

  /* Zero out any local variable that shouldn't be saved by the
     continuation.  The meta-continuation for the prompt is an
     especially important one to zero out (otherwise we build up
     chains). */
  prompt_cont = NULL;
  barrier_cont = NULL;

  if (scheme_setjmpup_relative(&cont->buf, cont, stack_start, sub_cont)) {
    /* We arrive here when the continuation is applied */
    Scheme_Object *result, *extra_marks;
    Scheme_Overflow *resume;
    Scheme_Cont *use_next_cont;
    Scheme_Dynamic_Wind *common_dw;
    Scheme_Prompt *shortcut_prompt;
    int common_next_meta, empty_to_next_mc;

    p = scheme_current_thread; /* maybe different than before */

    result = cont->value;
    cont->value = NULL;
    
    resume = cont->resume_to;
    cont->resume_to = NULL;

    use_next_cont = cont->use_next_cont;
    cont->use_next_cont = NULL;
  
    extra_marks = cont->extra_marks;
    cont->extra_marks = NULL;

    common_dw = cont->common_dw;
    cont->common_dw = NULL;

    common_next_meta = cont->common_next_meta;
    cont->common_next_meta = 0;
  
    shortcut_prompt = cont->shortcut_prompt;
    cont->shortcut_prompt = NULL;

    empty_to_next_mc = cont->empty_to_next_mc;
    cont->empty_to_next_mc = 0;

    restore_continuation(cont, p, 0, result, resume, empty_to_next_mc, 
                         prompt_tag, sub_cont, 
                         common_dw, common_next_meta, shortcut_prompt,
                         !!resume, 1, 
                         use_next_cont, extra_marks);

    /* We may have just re-activated breaking: */
    scheme_check_break_now();
    
    return result;
  } else {
    Scheme_Object *argv2[1];

    argv2[0] = (Scheme_Object *)cont;
    ret = _scheme_tail_apply(argv[0], 1, argv2);
    return ret;
  }
}

static Scheme_Object *continuation_p (int argc, Scheme_Object *argv[])
{
  return ((SCHEME_CONTP(argv[0]) || SCHEME_ECONTP(argv[0]))
          ? scheme_true
          : scheme_false);
}

void scheme_takeover_stacks(Scheme_Thread *p)
     /* When a contination captured in on e thread is invoked in another,
	the two threads can start using the same runstack, and possibly
	also the same cont-mark stack. This function swaps out the
	current owner in favor of p */
	
{
  if (p->runstack_owner && ((*p->runstack_owner) != p)) {
    Scheme_Thread *op;
    Scheme_Saved_Stack *swapped;
    op = *p->runstack_owner;
    if (op) {
      swapped = copy_out_runstack(op, op->runstack, op->runstack_start, NULL, NULL);
      op->runstack_swapped = swapped;
    }
    *(p->runstack_owner) = p;
    copy_in_runstack(p, p->runstack_swapped, 1);
    p->runstack_swapped = NULL;
  }

  if (p->cont_mark_stack_owner && ((*p->cont_mark_stack_owner) != p)) {
    Scheme_Thread *op;
    Scheme_Cont_Mark *swapped;
    op = *p->cont_mark_stack_owner;
    if (op) {
      swapped = copy_out_mark_stack(op, op->cont_mark_stack, NULL, NULL, NULL, 0);
      op->cont_mark_stack_swapped = swapped;
    }
    *(p->cont_mark_stack_owner) = p;
    copy_in_mark_stack(p, p->cont_mark_stack_swapped, MZ_CONT_MARK_STACK, 0, 0, NULL, 0);
    p->cont_mark_stack_swapped = NULL;
  }
}

static Scheme_Object *
call_with_continuation_barrier (int argc, Scheme_Object *argv[])
{
  scheme_check_proc_arity("call-with-continuation-barrier", 0, 0, argc, argv);

  return scheme_apply(argv[0], 0, NULL);
}

Scheme_Prompt *scheme_get_barrier_prompt(Scheme_Meta_Continuation **_meta_cont,
                                         MZ_MARK_POS_TYPE *_pos)
{
  return (Scheme_Prompt *)scheme_extract_one_cc_mark_with_meta(NULL, 
                                                               barrier_prompt_key,
                                                               NULL,
                                                               _meta_cont,
                                                               _pos);
}


static Scheme_Object *make_prompt_tag (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o, *key;

  if (argc && !SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("make-continuation-prompt-tag", "symbol", 0, argc, argv);

  key = scheme_make_pair(scheme_false, scheme_false);

  o = scheme_alloc_object();
  o->type = scheme_prompt_tag_type;
  SCHEME_CAR(o) = key;
  SCHEME_CDR(o) = (argc ? argv[0] : NULL);

  return o;
}

static Scheme_Object *get_default_prompt_tag (int argc, Scheme_Object *argv[])
{
  return scheme_default_prompt_tag;
}

static Scheme_Object *prompt_tag_p (int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(scheme_prompt_tag_type, SCHEME_TYPE(argv[0]))
          ? scheme_true
          : scheme_false);
}

Scheme_Overflow *scheme_get_thread_end_overflow(void)
{
  Scheme_Overflow *overflow;
  overflow = MALLOC_ONE_RT(Scheme_Overflow);
#ifdef MZTAG_REQUIRED
  overflow->type = scheme_rt_overflow;
#endif
  overflow->eot = 1;
  return overflow;
}


void scheme_drop_prompt_meta_continuations(Scheme_Object *prompt_tag)
{
  Scheme_Meta_Continuation *mc;

  mc = scheme_current_thread->meta_continuation;
  while (!SAME_OBJ(mc->prompt_tag, prompt_tag)) {
    if (mc->overflow) {
      scheme_signal_error("meta-continuation to drop is not just a placeholder?!");
    }
    mc = mc->next;
  }

  scheme_current_thread->meta_continuation = mc;
}

/* private, but declared public to avoid inlining: */
Scheme_Object *scheme_finish_apply_for_prompt(Scheme_Prompt *prompt, Scheme_Object *_prompt_tag, 
                                              Scheme_Object *proc, int argc, Scheme_Object **argv)
{
  /* Put space on the stack to record a longjmp target,
     in case a following continuation is restored for a
     different prompt.
     By putting this information on the stack, it will
     get captured if there's a further capture. */
  Scheme_Thread *p;
  Scheme_Object * volatile prompt_tag = _prompt_tag;
  mz_jmp_buf newbuf, * volatile savebuf;
  Scheme_Object *val;
  int cc_count = scheme_cont_capture_count;
  
  prompt->prompt_buf = &newbuf;
  prompt = NULL; /* to avoid prompt chains */

  p = scheme_current_thread;

  savebuf = p->error_buf;
  p->error_buf = &newbuf;

  /* Initial meta-continuation says to fall through. This
     one can get replaced when the current continuation
     is captured and then restored. */
  {
    Scheme_Meta_Continuation *resume_mc;
    if (available_prompt_mc) {
      resume_mc = available_prompt_mc;
      available_prompt_mc = NULL;
    } else
      resume_mc = MALLOC_ONE_RT(Scheme_Meta_Continuation);
#ifdef MZTAG_REQUIRED
    resume_mc->type = scheme_rt_meta_cont;
#endif
    resume_mc->prompt_tag = prompt_tag;
    if (p->meta_continuation) {
      resume_mc->next = p->meta_continuation;
      resume_mc->depth = p->meta_continuation->depth + 1;
    }
    resume_mc->meta_tail_pos = MZ_CONT_MARK_POS + 2;
    p->meta_continuation = resume_mc;
  }

  if (scheme_setjmp(newbuf)) {
    /*
      We can get here in three ways:
        1. abort-current-continuation with this prompt's tag:
           In this case, p->cjs.jumping_to_continuation is the
           prompt, p->cjs.val is a value to deliver to the
           prompt handler, and p->cjs.is_escape is unset.
           [This is a jump in the normal error/abort chain.]
        2. applying a continuation that is delimited by the prompt tag
           (in which case the jump originates from scheme_do_eval):
           In this case, p->cjs.jumping_to_continuation is the
           prompt, p->cjs.val is a continuation, and
           p->cjs.is_escape is set.
           [This is a jump in the special continuation-application
            direct mode.]
        3. other exception-level escape:
           In this case, p->cjs.jumping_to_continuation is the
           target (maybe an escape continuation), p->cjs.val is
           information to propagate to the target, and p->cjs.is_escape 
           is unset.
           [This is a jump in the normal error/abort chain.]
    */
    val = NULL;
  } else {
    val = _scheme_apply_multi(proc, argc, argv);
  }

  p = scheme_current_thread;
  p->error_buf = savebuf;

  {
    Scheme_Meta_Continuation *resume_mc;
    Scheme_Overflow *resume;
    
    resume = p->meta_continuation->overflow;
    resume_mc = p->meta_continuation;
    p->meta_continuation = p->meta_continuation->next;

    /* The following test was once useful for finding bugs. However,
       dropping meta-continuations that represent empty continuations
       (see for_composable in clone_meta_cont) interferes with the test. */
    /*
      if (!SAME_OBJ(resume_mc->prompt_tag, prompt_tag)) {
        scheme_signal_error("meta-continuation prompt tag does not match current prompt tag");
      }
    */

    if (cc_count == scheme_cont_capture_count) {
      memset(resume_mc, 0, sizeof(Scheme_Meta_Continuation));
#ifdef MZTAG_REQUIRED
      resume_mc->type = scheme_rt_meta_cont;
#endif
      available_prompt_mc = resume_mc;
    }

    if (!resume) {
      /* We return NULL if there's an escape of some sort (see above), 
         otherwise we return the result value. */
      return val;
    } else if (resume->eot) {
      /* There's nothing left in the continuation, 
         so just end the thread. We havent restored
         the thread state from the prompt, so flush
         anything that might otherwise have a clean-up action: */
      MZ_RUNSTACK = NULL;
      MZ_RUNSTACK_START = NULL;
      MZ_CONT_MARK_STACK = 0;
      p->runstack_start = NULL;
      p->runstack = NULL;
      p->runstack_size = 0;
      p->runstack_saved = NULL;
      p->cont_mark_stack_segments = NULL;
      scheme_end_current_thread();
      return NULL;
    } else {
      /* Continue by jumping to a meta-continuation. If
         val, then p->cjs.jumping_to_continuation is unset,
         so it's ok to communicate val via p->cjs.val. The
         target for this jump is in compose_continuation(). */
      p->next_meta -= 1;
      if (val) {
        if (val == SCHEME_MULTIPLE_VALUES) {
          if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
            p->values_buffer = NULL;
        }
        p->cjs.val = val;
      }
      p->stack_start = resume->stack_start;
      scheme_longjmpup(&resume->jmp->cont);
      return NULL;
    }
  }
}

/* private, but declared public to avoid inlining: */
Scheme_Object *scheme_apply_for_prompt(Scheme_Prompt *prompt, Scheme_Object *prompt_tag, 
                                       Scheme_Object *proc, int argc, Scheme_Object **argv)
{
  /* Grab stack address, then continue on with final step: */
  prompt->stack_boundary = PROMPT_STACK(proc);

  /* Even if all threads start deeper, a continuation might be sent
     to the thread to start it at this prompt's stack level. */
  scheme_ensure_stack_start(prompt->stack_boundary);

  proc = scheme_finish_apply_for_prompt(prompt, prompt_tag, proc, argc, argv);

  return proc;
}

static Scheme_Object *compose_continuation(Scheme_Cont *cont, int exec_chain, 
                                           Scheme_Object *loop_prompt, int empty_to_next_mc)
/* continuation arguments should be in `cont' already */
{
  /* Apply continuation as composable. There may or may not
     be a prompt immediately wrapping this application, depending on
     whether the continuation was captured as composable. */
  Scheme_Overflow *overflow;
  Scheme_Overflow_Jmp *jmp;
  Scheme_Cont *saved;
  Scheme_Prompt *saved_meta_prompt;
  Scheme_Thread *p = scheme_current_thread;

  scheme_about_to_move_C_stack();

  reset_cjs(&p->cjs);
  
  saved_meta_prompt = p->meta_prompt;

  /* Grab a continuation so that we capture the current Scheme stack,
     etc.: */
  saved = grab_continuation(p, 1, 0, NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, 0);
  
  overflow = MALLOC_ONE_RT(Scheme_Overflow);
#ifdef MZTAG_REQUIRED
  overflow->type = scheme_rt_overflow;
#endif
  overflow->prev = p->overflow;
  overflow->stack_start = p->stack_start;

  jmp = MALLOC_ONE_RT(Scheme_Overflow_Jmp);
#ifdef MZTAG_REQUIRED
  jmp->type = scheme_rt_overflow_jmp;
#endif
  overflow->jmp = jmp;
        
  scheme_init_jmpup_buf(&overflow->jmp->cont);
  if (scheme_setjmpup(&overflow->jmp->cont, overflow->jmp, ADJUST_STACK_START(p->stack_start))) {
    /* Returning. (Jumped here from finish_apply_for_prompt,
       scheme_compose_continuation, or scheme_eval.)
       
       We can return for several reasons:
        1. We got a result value.
           In that case, p->cjs.val holds the value, and
           p->cjs.jumping_to_continuation is NULL.
        2. There's an escape, and p->cjs.jumping_to_continuation
           is set. It could be a prompt, in which case we're
           escaping to the prompt, or it could be an
           error escape. In the former case, we may or may not be 
           applying a continuation at the target; see
           scheme_finish_apply_for_prompt() for those possibilities.
    */
    Scheme_Object *v;
    Scheme_Meta_Continuation *mc;

    p = scheme_current_thread;

    if (!p->cjs.jumping_to_continuation) {
      /* Got a result: */
      v = p->cjs.val;
      p->cjs.val = NULL;
      if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES)) {
        if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
          p->values_buffer = NULL;
      }
    } else {
      /* Some sort of escape, to be handled by the caller,
         or to be handled below if it's an escape to loop_prompt.  */
      v = NULL;
    }
    mc = p->meta_continuation;
    p->meta_prompt = saved_meta_prompt; /* Set meta_prompt before restoring runstack,
                                           since GC erases meta-prompt-blocked portion
                                           on the runstack. */
    restore_continuation(saved, p, 1, v, NULL, 0,
                         NULL, NULL,
                         NULL, 0, NULL,
                         0, !p->cjs.jumping_to_continuation, 
                         NULL, NULL);

    p->meta_continuation = mc;

    /* There can be two kinds of loops:
         1. An escape to the current prompt to invoke another
            continuation.
         2. A trampoline to turn a composable-continuation
            application into a tail call; in this case,
            jumping_to_continuation = #t. */
    if (!v && ((loop_prompt
                && SAME_OBJ((Scheme_Object *)p->cjs.jumping_to_continuation,
                            loop_prompt)
                && p->cjs.is_escape)
               || (!loop_prompt
                   && p->cjs.jumping_to_continuation
                   && SCHEME_VECTORP((Scheme_Object *)p->cjs.jumping_to_continuation)))) {
      /* We'll handle this escape directly, to avoid re-computing
         saved and overflow. */
      cont = (Scheme_Cont *)p->cjs.val;
      if (SCHEME_VECTORP((Scheme_Object *)p->cjs.jumping_to_continuation)) {
        /* Instead of installing marks in `saved' now, ask `cont' to do it, 
           since `cont' may have some of its own replacements. */
        cont->extra_marks = (Scheme_Object *)p->cjs.jumping_to_continuation;
      }
      reset_cjs(&p->cjs);
      /* The current meta-continuation may have changed since capture: */
      saved->meta_continuation = p->meta_continuation;
      /* Fall though to continuation application below. */
    } else {
      return v;
    }
  }

  scheme_current_thread->suspend_break++;
  
  /* Here's where we jump to the target: */
  saved->resume_to = overflow; /* used by eval to jump to current meta-continuation */
  cont->use_next_cont = saved;
  cont->resume_to = overflow;
  cont->empty_to_next_mc = (char)empty_to_next_mc;
  scheme_current_thread->stack_start = cont->prompt_stack_start;
  scheme_longjmpup(&cont->buf);

  ESCAPED_BEFORE_HERE;
}

static void continue_prompt_escape()
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Prompt *targetc = (Scheme_Prompt *)p->cjs.jumping_to_continuation;

  scheme_drop_prompt_meta_continuations(targetc->tag);

  if ((!targetc->boundary_overflow_id && !p->overflow)
      || (targetc->boundary_overflow_id == p->overflow->id)) {
    /* Jump directly to the target. */
    scheme_longjmp(*targetc->prompt_buf, 1);
  } else {
    /* More hassle: need to unwind overflows to get to the prompt. */
    Scheme_Overflow *overflow = p->overflow;
    while (overflow->prev
           && (!overflow->prev->id
               || (overflow->prev->id != targetc->boundary_overflow_id))) {
      overflow = overflow->prev;
    }
    p->overflow = overflow;
    p->stack_start = overflow->stack_start;
    scheme_longjmpup(&overflow->jmp->cont);
  }
}

static void restore_from_prompt(Scheme_Prompt *prompt)
{
  Scheme_Thread *p = scheme_current_thread;

  while (MZ_RUNSTACK_START != prompt->runstack_boundary_start) {
    MZ_RUNSTACK_START = p->runstack_saved->runstack_start;
    p->runstack_saved = p->runstack_saved->prev;
  }

  MZ_RUNSTACK = MZ_RUNSTACK_START + prompt->runstack_boundary_offset;
  MZ_CONT_MARK_STACK = prompt->mark_boundary;
  MZ_CONT_MARK_POS = prompt->boundary_mark_pos;
  
  p->runstack_size = prompt->runstack_size;

  if (prompt->boundary_overflow_id) {
    while (p->overflow->id != prompt->boundary_overflow_id) {
      p->overflow = p->overflow->prev;
    }
  } else
    p->overflow = NULL;
}

static void prompt_unwind_dw(Scheme_Object *prompt_tag)
{
  int delta = 0;
  Scheme_Thread *p = scheme_current_thread;

  while (p->dw && !SAME_OBJ(p->dw->prompt_tag, prompt_tag)) {
    delta += p->dw->next_meta;
    p->dw = p->dw->prev;
  }
  if (!p->dw) {
    scheme_signal_error("Lost prompt dynamic-wind record!\n");
  } else {
    delta += p->dw->next_meta;
    p->dw = p->dw->prev;
    p->next_meta += delta;
  }
}

static void prompt_unwind_one_dw(Scheme_Object *prompt_tag)
{
  Scheme_Thread *p = scheme_current_thread;
  if (!p->dw || !SAME_OBJ(p->dw->prompt_tag, prompt_tag)) {
    scheme_signal_error("Dynamic-wind record doesn't match prompt!\n");
  } else
    prompt_unwind_dw(prompt_tag);
}

static Scheme_Object *call_with_prompt (int in_argc, Scheme_Object *in_argv[])
{
  Scheme_Object *v;
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *proc = in_argv[0], *prompt_tag;
  Scheme_Prompt *prompt;
  int argc, handler_argument_error = 0;
  Scheme_Object **argv, *a[1], *handler;
  Scheme_Cont_Frame_Data cframe;
  Scheme_Dynamic_Wind *prompt_dw;
  int cc_count = scheme_cont_capture_count;

  scheme_check_proc_arity("call-with-continuation-prompt", 0, 0, in_argc, in_argv);
  if (in_argc > 1) {
    if (!SAME_TYPE(scheme_prompt_tag_type, SCHEME_TYPE(in_argv[1]))) {
      scheme_wrong_type("call-with-continuation-prompt", "continuation-prompt-tag",
                        1, in_argc, in_argv);
    }
    prompt_tag = in_argv[1];
  } else
    prompt_tag = scheme_default_prompt_tag;

  if (in_argc > 2) {
    if (SCHEME_TRUEP(in_argv[2]) && !SCHEME_PROCP(in_argv[2]))
      scheme_wrong_type("call-with-continuation-prompt", "procedure or #f", 2, in_argc, in_argv);
    handler = in_argv[2];
  } else
    handler = scheme_false;

  argv = NULL;
  argc = 0;

  do {
    /* loop implements the default prompt handler */

    if (available_regular_prompt) {
      /* `call-with-continuation-prompt' is used by `with-handlers' which might
         easily occur in a loop. Try to avoid allocation, even if only for unnested
         prompts. */
      prompt = available_regular_prompt;
      available_regular_prompt = NULL;
    } else
      prompt = MALLOC_ONE_TAGGED(Scheme_Prompt);

    prompt->so.type = scheme_prompt_type;

    prompt->tag = prompt_tag;

    scheme_push_continuation_frame(&cframe);
    scheme_set_cont_mark(SCHEME_PTR_VAL(prompt_tag), (Scheme_Object *)prompt);

    /* Note: prompt save marks after the one corresponding to itself,
       so that restoring a continuation captured under the prompt
       doesn't re-install this prompt. (Instead, the prompt that applies
       is the one in the invocation context). */

    ASSERT_SUSPEND_BREAK_ZERO();

    initialize_prompt(p, prompt, NULL);

    if (p->overflow) {
      ensure_overflow_id(p->overflow);
      prompt->boundary_overflow_id = p->overflow->id;
    }

    prompt->runstack_size = p->runstack_size;

    if (available_prompt_dw) {
      prompt_dw = available_prompt_dw;
      available_prompt_dw = NULL;
    } else
      prompt_dw = MALLOC_ONE_RT(Scheme_Dynamic_Wind);
#ifdef MZTAG_REQUIRED
    prompt_dw->type = scheme_rt_dyn_wind;
#endif
    prompt_dw->prompt_tag = prompt_tag;
    if (p->dw) {
      prompt_dw->next_meta = p->next_meta;
      prompt_dw->prev = p->dw;
      prompt_dw->depth = p->dw->depth + 1;
    }

    p->next_meta = 0;
    p->dw = prompt_dw;

    v = scheme_apply_for_prompt(prompt, prompt_tag, proc, argc, argv);

    /* >> An escape can jump directly here, instead of going through the
       usual escape chain of setjmps. That means we need to reset everything,
       such as the runstack pointer. The information we need is in the
       prompt record. */

    p = scheme_current_thread;

    restore_from_prompt(prompt);

    p->suspend_break = 0;

    if (!v) {
      /* There was an escape. See scheme_finish_apply_for_prompt for the possibilities. */
      if (SAME_OBJ((Scheme_Object *)p->cjs.jumping_to_continuation,
                   (Scheme_Object *)prompt)) {
        /* Jumping to this prompt, maybe to apply a different
           continuation... */
        if (p->cjs.is_escape) {
          /* Yes, a different continuation. That is, apply a non-functional continuation 
             that is based on a (potentially) different prompt. The d-w record
             is already removed as necessary at the cont call site in "eval.c". 
             Loop, in case we have a kind of tail-call to another such contionuation: */
          Scheme_Cont *target;

          target = (Scheme_Cont *)p->cjs.val;
          reset_cjs(&p->cjs);

          v = compose_continuation(target, 1, (Scheme_Object *)prompt, 0);
        
          if (v) {
            /* Got a result: */
            prompt_unwind_one_dw(prompt_tag);
            handler = NULL;
          } else {
            /* Escaping, maybe to here... */
            p = scheme_current_thread;
            if (SAME_OBJ((Scheme_Object *)p->cjs.jumping_to_continuation,
                         (Scheme_Object *)prompt)) {
              /* Jump to here. If p->cjs.is_escape, then 
                 we want to apply a continuation --- again. */
              if (p->cjs.is_escape) {
                /* this should have been caught in compose_continuation */
                scheme_signal_error("escape-to-prompt escaped!");
                return NULL;
              } else {
                /* It's an abort to here, so fall though and
                   pick up the values. */
                prompt_unwind_one_dw(prompt_tag);
                v = NULL;
              }
            } else if (p->cjs.is_escape) {
              /* We're trying to get to a prompt in this meta-continuation.
                 Jump again. */
              continue_prompt_escape();
              return NULL;
            } else {
              /* Exception-level or call/ec escape. Continue jumping: */
              restore_from_prompt(prompt);
              prompt_unwind_one_dw(prompt_tag);
              scheme_longjmp(*p->error_buf, 1);
              return NULL;
            }
          }
        } else {
          /* It was an abort to here; fall through, which picks up
             p->cjs.val to deliver to the handler. First discard the
             dw record that we introduced. */
          prompt_unwind_one_dw(prompt_tag);
          v = NULL;
        }

        /* At this point, v can be non-NULL if a continuation
           delivered a value. */

        if (!v) {
          argc = p->cjs.num_vals;

          if (argc == 1) {
            a[0] = p->cjs.val;
            argv = a;
          } else
            argv = (Scheme_Object **)p->cjs.val;

          reset_cjs(&p->cjs);

          if (SAME_OBJ(handler, scheme_values_func)) {
            v = scheme_values(argc, argv);
            handler = NULL;
          } else if (SCHEME_FALSEP(handler)) {
            if (argc == 1) {
              if (!scheme_check_proc_arity(NULL, 0, 0, argc, argv)) {
                /* delay error until we clean up: */
                handler_argument_error = 1;
                handler = NULL;
              } else {
                proc = a[0];
                argc = 0;
                argv = NULL;
              }
            } else {
              /* wrong number of arguments returned to default handler */
              handler_argument_error = 1;
              handler = NULL;
            }
          }
        } else {
          argc = 0;
          argv = NULL;
        }
      } else {
        /* Other error-like escape: */
        if ((p->dw != prompt_dw)
            && (!p->dw || !prompt_dw->id || (p->dw->id != prompt_dw->id))) {
          /* A full continuation jump was interrupted by an
             escape continuation jump (in a dw pre or post thunk). */
        } else
          prompt_unwind_one_dw(prompt_tag);
        scheme_longjmp(*p->error_buf, 1);
        return NULL;
      }
    } else {
      prompt_unwind_one_dw(prompt_tag);
      handler = NULL;
      argc = 0;
      argv = NULL;
    }

    scheme_pop_continuation_frame(&cframe);

    if (cc_count == scheme_cont_capture_count) {
      if (!available_regular_prompt) {
        memset(prompt, 0, sizeof(Scheme_Prompt));
        prompt->so.type = scheme_prompt_type;
        available_regular_prompt = prompt;
      }
      if (!available_prompt_dw) {
        memset(prompt_dw, 0, sizeof(Scheme_Dynamic_Wind));
#ifdef MZTAG_REQUIRED
        prompt_dw->type = scheme_rt_dyn_wind;
#endif
        available_prompt_dw = prompt_dw;
      }
    }
  } while (handler && SCHEME_FALSEP(handler));

  if (handler_argument_error) {
    if (argc == 1) {
      scheme_check_proc_arity("default-continuation-prompt-handler", 0, 0, argc, argv);
    } else {
      scheme_wrong_return_arity("call-with-continuation-prompt", 1, argc, argv,
                                "application of default prompt handler");
    }
  }

  if (handler) {
    return _scheme_tail_apply(handler, argc, argv);
  } else
    return v;
}

static Scheme_Object *propagate_abort(int argc, Scheme_Object **argv)
{
  Scheme_Object **argv2;

  argv2 = MALLOC_N(Scheme_Object *, argc + 1);
  memcpy(argv2 XFORM_OK_PLUS 1, argv, sizeof(Scheme_Object *) * argc);
  argv2[0] = scheme_default_prompt_tag;

  return _scheme_apply(abort_continuation_proc, argc+1, argv2);
}

static Scheme_Object *do_call_with_prompt(Scheme_Closed_Prim f, void *data, 
                                          int multi, int top_level)
{
  Scheme_Object *prim, *a[3];

  prim = scheme_make_closed_prim(f, data);
  a[0] = prim;
  a[1] = scheme_default_prompt_tag;
  a[2] = scheme_make_prim(propagate_abort);

  if (multi) {
    if (top_level)
      return scheme_apply_multi(call_with_prompt_proc, 3, a);
    else
      return _scheme_apply_multi(call_with_prompt_proc, 3, a);
  } else {
    if (top_level)
      return scheme_apply(call_with_prompt_proc, 3, a);
    else
      return _scheme_apply(call_with_prompt_proc, 3, a);
  }
}

Scheme_Object *scheme_call_with_prompt(Scheme_Closed_Prim f, void *data)
{
  return do_call_with_prompt(f, data, 0, 1);
}

Scheme_Object *scheme_call_with_prompt_multi(Scheme_Closed_Prim f, void *data)
{
  return do_call_with_prompt(f, data, 1, 1);
}

Scheme_Object *_scheme_call_with_prompt(Scheme_Closed_Prim f, void *data)
{
  return do_call_with_prompt(f, data, 0, 0);
}

Scheme_Object *_scheme_call_with_prompt_multi(Scheme_Closed_Prim f, void *data)
{
  return do_call_with_prompt(f, data, 1, 0);
}

Scheme_Object *scheme_compose_continuation(Scheme_Cont *cont, int num_rands, Scheme_Object *value)
{
  Scheme_Meta_Continuation *mc;
  int empty_to_next_mc;

  if (num_rands != 1) {
    value = scheme_values(num_rands, (Scheme_Object **)value);
    {
      Scheme_Thread *p = scheme_current_thread;
      if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
        p->values_buffer = NULL;
    }
  }
  cont->value = value;
  cont->common_dw_depth = -1;

  mc = scheme_current_thread->meta_continuation;
  if (mc && mc->pseudo && mc->meta_tail_pos == MZ_CONT_MARK_POS) {
    /* The existing meta-continuation is the same as the
       current continuation. Trampoline through the meta-continuation
       to implement the call as a tail call.
       We also need to propagate continuation marks here, if any,
       back to the trampoline. They get merged with the trampoline's
       meta-continuation when `cont' is applied. */
    Scheme_Thread *p = scheme_current_thread;
    Scheme_Object *cm_info;
    long findpos, bottom, pos;
    int count, mcount, i;

    p->meta_continuation = mc->next;

    bottom = (long)p->cont_mark_stack_bottom;
    count = 0;
    for (findpos = (long)MZ_CONT_MARK_STACK - 1; findpos >= bottom; findpos--) {
      GC_CAN_IGNORE Scheme_Cont_Mark *seg;

      seg = p->cont_mark_stack_segments[findpos >> SCHEME_LOG_MARK_SEGMENT_SIZE];
      pos = findpos & SCHEME_MARK_SEGMENT_MASK;
      if (seg[pos].pos != MZ_CONT_MARK_POS)
        break;
      count++;
    }
    mcount = 0;
    for (findpos = (long)mc->cont_mark_total; findpos--; ) {
      if (mc->cont_mark_stack_copied[findpos].pos != mc->cont_mark_pos)
        break;
      mcount++;
    }

    cm_info = scheme_make_vector((count + mcount) * 2, NULL);
    for (findpos = (long)MZ_CONT_MARK_STACK - 1, i = 0; i < count; findpos--, i++) {
      GC_CAN_IGNORE Scheme_Cont_Mark *seg;

      seg = p->cont_mark_stack_segments[findpos >> SCHEME_LOG_MARK_SEGMENT_SIZE];
      pos = findpos & SCHEME_MARK_SEGMENT_MASK;
      SCHEME_VEC_ELS(cm_info)[2*i] = seg[pos].key;
      SCHEME_VEC_ELS(cm_info)[(2*i)+1] = seg[pos].val;
    }
    for (findpos = (long)mc->cont_mark_total - 1, i = 0; i < mcount; findpos--, i++) {
      SCHEME_VEC_ELS(cm_info)[2*(count + i)] = mc->cont_mark_stack_copied[findpos].key;
      SCHEME_VEC_ELS(cm_info)[(2*(count + i))+1] = mc->cont_mark_stack_copied[findpos].val;
    }

    p->cjs.jumping_to_continuation = cm_info; /* vector => trampoline */
    p->cjs.val = (Scheme_Object *)cont;
    p->cjs.num_vals = 1;
    p->cjs.is_escape = 1;

    p->stack_start = mc->overflow->stack_start;

    scheme_longjmpup(&mc->overflow->jmp->cont);
    return NULL;
  } else if (mc && mc->meta_tail_pos == MZ_CONT_MARK_POS) {
    empty_to_next_mc = 1;
  } else {
    empty_to_next_mc = 0;
  }

  value = compose_continuation(cont, 0, NULL, empty_to_next_mc);
  
  scheme_current_thread->next_meta -= 1;

  if (!value) {
    /* Continue escape --- maybe a direct jump to a prompt
       in this meta-continuation. */
    Scheme_Thread *p = scheme_current_thread;
    if (p->cjs.is_escape) {
      /* We're trying to get to a prompt in this meta-continuation.
         Jump again. */
      continue_prompt_escape();
    } else {
      scheme_longjmp(*scheme_current_thread->error_buf, 1);
    }
  }

  return value;
}

static Scheme_Object *abort_continuation (int argc, Scheme_Object *argv[])
{
  Scheme_Object *prompt_tag;
  Scheme_Prompt *prompt;
  Scheme_Thread *p = scheme_current_thread;

  prompt_tag = argv[0];
  if (!SAME_TYPE(scheme_prompt_tag_type, SCHEME_TYPE(prompt_tag))) {
    scheme_wrong_type("abort-current-continuation", "continuation-prompt-tag",
                      0, argc, argv);
  }

  prompt = (Scheme_Prompt *)scheme_extract_one_cc_mark(NULL, SCHEME_PTR_VAL(prompt_tag));
  if (!prompt && SAME_OBJ(scheme_default_prompt_tag, prompt_tag))
    prompt = original_default_prompt;

  if (!prompt) {
    scheme_arg_mismatch("abort-current-continuation", 
                        "continuation includes no prompt with the given tag: ",
                        prompt_tag);
    return NULL;
  }

  if (argc == 2) {
    p->cjs.num_vals = 1;
    p->cjs.val = argv[1];
  } else {
    Scheme_Object **vals;
    int i;
    vals = MALLOC_N(Scheme_Object *, argc - 1);
    for (i = argc; i-- > 1; ) {
      vals[i-1] = argv[i];
    }
    p->cjs.num_vals = argc - 1;
    p->cjs.val = (Scheme_Object *)vals;
  }
  p->cjs.jumping_to_continuation = (Scheme_Object *)prompt;

  scheme_longjmp(*p->error_buf, 1);

  return NULL;
}

static Scheme_Object *call_with_control (int argc, Scheme_Object *argv[])
{
  Scheme_Object *prompt_tag;
  Scheme_Object *a[3];

  scheme_check_proc_arity("call-with-composable-continuation", 1, 0, argc, argv);
  if (argc > 1) {
    if (!SAME_TYPE(scheme_prompt_tag_type, SCHEME_TYPE(argv[1]))) {
      scheme_wrong_type("call-with-composable-continuation", "continuation-prompt-tag",
                        1, argc, argv);
    }
    prompt_tag = argv[1];
  } else
    prompt_tag = scheme_default_prompt_tag;

  a[0] = argv[0];
  a[1] = prompt_tag;
  a[2] = scheme_true;

  /* Trampoline to internal_call_cc. This trampoline ensures that
     the runstack is flushed before we try to grab the continuation. */
  return _scheme_tail_apply(internal_call_cc_prim, 3, a);
}

static Scheme_Object *continuation_marks(Scheme_Thread *p,
					 Scheme_Object *_cont,
					 Scheme_Object *econt,
                                         Scheme_Meta_Continuation *mc,
                                         Scheme_Object *prompt_tag,
                                         char *who,
					 int just_chain)
     /* cont => p is not used */
{
  Scheme_Cont *cont = (Scheme_Cont *)_cont, *top_cont;
  Scheme_Cont_Mark_Chain *first = NULL, *last = NULL;
  Scheme_Cont_Mark_Set *set;
  Scheme_Object *cache, *nt;
  long findpos, bottom;
  long cmpos, cdelta = 0;
  int found_tag = 0;

  if (cont && SAME_OBJ(cont->prompt_tag, prompt_tag))
    found_tag = 1;
  if (!prompt_tag)
    found_tag = 1;

  do {
    if (econt) {
      findpos = (long)((Scheme_Escaping_Cont *)econt)->envss.cont_mark_stack;
      cmpos = (long)((Scheme_Escaping_Cont *)econt)->envss.cont_mark_pos;
      if (mc) {
        cdelta = mc->cont_mark_offset;
        bottom = 0;
      } else
        bottom = p->cont_mark_stack_bottom;
    } else if (cont) {
      findpos = (long)cont->ss.cont_mark_stack;
      cmpos = (long)cont->ss.cont_mark_pos;
      cdelta = cont->cont_mark_offset;
      bottom = 0;
    } else if (mc) {
      findpos = (long)mc->cont_mark_stack;
      cmpos = (long)mc->cont_mark_pos;
      cdelta = mc->cont_mark_offset;
      bottom = 0;
    } else {
      findpos = (long)MZ_CONT_MARK_STACK;
      cmpos = (long)MZ_CONT_MARK_POS;
      if (!p->cont_mark_stack_segments)
        findpos = 0;
      bottom = p->cont_mark_stack_bottom;
    }

    top_cont = cont;

    while (findpos-- > bottom) {
      Scheme_Cont_Mark *find;
      long pos;

      if (cont) {
        while (findpos < cdelta) {
          if (!cont->runstack_copied) {
            /* Current cont was just a mark-stack variation of
               next cont, so skip the next cont. */
            cont = cont->buf.cont;
          }
          cont = cont->buf.cont;
          if (cont)
            cdelta = cont->cont_mark_offset;
          else
            break;
        }
        if (!cont)
          break;
        find = cont->cont_mark_stack_copied;
        pos = findpos - cdelta;
      } else if (mc) {
        if (findpos < cdelta)
          break;
        find = mc->cont_mark_stack_copied;
        pos = findpos - cdelta;
      } else {
        GC_CAN_IGNORE Scheme_Cont_Mark *seg;

        seg = p->cont_mark_stack_segments[findpos >> SCHEME_LOG_MARK_SEGMENT_SIZE];
        pos = findpos & SCHEME_MARK_SEGMENT_MASK;
        find = seg;
      }

      /* A cache is one of:
          NULL (empty)
          #f (empty)
          hash-table: maps prompt tag to tag-cache
          chain : for default_scheme_prompt_tag
          (vector chain key val depth) : chain is for default_scheme_prompt_tag,
                                         key+val+depth is for !prompt_tag

          A tag-cache is one of:
          chain : the chain we're looking for
          (vector chain key val depth) : key = NULL implies that val is
                                         a table of mappings from keys to (cons val depth)s
      */

      if (prompt_tag && (find[pos].key == SCHEME_PTR_VAL(prompt_tag))) {
        found_tag = 1;
        /* Break out of outer loop, too: */
        mc = NULL;
        p = NULL;
        econt = NULL;
        cont = NULL;
        break;
      }

      cache = find[pos].cache;
      if (cache) {
        if (SCHEME_FALSEP(cache))
          cache = NULL;
        if (cache) {
          if (SCHEME_HASHTP(cache))
            cache = scheme_hash_get((Scheme_Hash_Table *)cache, prompt_tag ? prompt_tag : scheme_false);
          else if (prompt_tag != scheme_default_prompt_tag)
            cache = NULL;
        }
        if (cache && SCHEME_VECTORP(cache)) {
          cache = SCHEME_VEC_ELS(cache)[0];
        }
      }

      if (cache) {
        if (((Scheme_Cont_Mark_Chain *)cache)->key) {
          if (last)
            last->next = (Scheme_Cont_Mark_Chain *)cache;
          else
            first = (Scheme_Cont_Mark_Chain *)cache;
          
          found_tag = 1; /* cached => tag is there */
        } else {
          /* bogus: tag wasn't there when we cached this chain */
        }

        /* Break out of outer loop, too: */
        mc = NULL;
        p = NULL;
        econt = NULL;
        cont = NULL;

        break;
      } else {
        Scheme_Cont_Mark_Chain *pr;
        pr = MALLOC_ONE_RT(Scheme_Cont_Mark_Chain);
        pr->so.type = scheme_cont_mark_chain_type;
        pr->key = find[pos].key;
        pr->val = find[pos].val;
        pr->pos = find[pos].pos;
        pr->next = NULL;
        if (mc) {
          if (mc->cm_shared) {
            Scheme_Cont_Mark *cp;
            cp = MALLOC_N(Scheme_Cont_Mark, mc->cont_mark_total);
            memcpy(cp, mc->cont_mark_stack_copied, mc->cont_mark_total * sizeof(Scheme_Cont_Mark));
            mc->cont_mark_stack_copied = cp;
            find = cp;
            mc->cm_shared = 0;
          }
          mc->cm_caches = 1;
        }
        cache = find[pos].cache;
        if (cache && !SCHEME_FALSEP(cache)) {
          if (SCHEME_HASHTP(cache)) {
            Scheme_Hash_Table *ht = (Scheme_Hash_Table *)cache;
            cache = scheme_hash_get(ht, prompt_tag ? prompt_tag : scheme_false);
            if (!cache) {
              scheme_hash_set(ht, prompt_tag ? prompt_tag : scheme_false, (Scheme_Object *)pr);
            } else {
              /* cache must be a vector */
              SCHEME_VEC_ELS(cache)[0] = (Scheme_Object *)pr;
            }
          } else if (!SCHEME_VECTORP(cache)) {
            /* cache is a chain and the tag is not the default prompt tag */
            Scheme_Hash_Table *ht;
            ht = scheme_make_hash_table(SCHEME_hash_ptr);
            scheme_hash_set(ht, scheme_default_prompt_tag, cache);
            scheme_hash_set(ht, prompt_tag ? prompt_tag : scheme_false, (Scheme_Object *)pr);
            find[pos].cache = (Scheme_Object *)ht;
          } else {
            /* cache must be a vector */
            if (prompt_tag == scheme_default_prompt_tag)
              SCHEME_VEC_ELS(cache)[0] = (Scheme_Object *)pr;
            else {
              /* Need to split up the default and NULL tags. Don't
                 try to use cache for just the null tag, in case
                 it's use by other copies. */
              Scheme_Hash_Table *ht;
              Scheme_Object *vec;
              ht = scheme_make_hash_table(SCHEME_hash_ptr);
              vec = scheme_make_vector(4, NULL);
              SCHEME_VEC_ELS(vec)[1] = SCHEME_VEC_ELS(cache)[1];
              SCHEME_VEC_ELS(vec)[2] = SCHEME_VEC_ELS(cache)[2];
              SCHEME_VEC_ELS(vec)[3] = SCHEME_VEC_ELS(cache)[3];
              scheme_hash_set(ht, scheme_false, vec);
              if (!prompt_tag)
                SCHEME_VEC_ELS(vec)[0] = (Scheme_Object *)pr;
              else
                scheme_hash_set(ht, prompt_tag, (Scheme_Object *)pr);
              find[pos].cache = (Scheme_Object *)ht;
            }
          }
        } else if (prompt_tag == scheme_default_prompt_tag) {
          find[pos].cache = (Scheme_Object *)pr;
        } else {
          cache = (Scheme_Object *)scheme_make_hash_table(SCHEME_hash_ptr);
          scheme_hash_set((Scheme_Hash_Table *)cache, 
                          prompt_tag ? prompt_tag : scheme_false, 
                          (Scheme_Object *)pr);
          find[pos].cache = cache;
        }
        if (last)
          last->next = pr;
        else
          first = pr;

        last = pr;
      }
    }

    if (mc) {
      mc = mc->next;
    } else if (top_cont) {
      mc = top_cont->meta_continuation;
    } else if (econt) {
      mc = p->meta_continuation;
    } else if (p) {
      mc = p->meta_continuation;
    }
    cont = NULL;
    econt = NULL;
    p = NULL;
  } while (mc);

  if (!found_tag) {
    if (!SAME_OBJ(prompt_tag, scheme_default_prompt_tag)) {
      /* The chain is cached. Destroy it, so that future cache references
         will indicate that the tag is not present (as opposed to delivering
         the bogus chain). */
      while (first) {
        first->key = NULL;
        first = first->next;
      }
      if (!who)
        return NULL;
      scheme_arg_mismatch(who,
                          "no corresponding prompt in the continuation: ",
                          prompt_tag);
    }
  }

  if (just_chain)
    return (Scheme_Object *)first;

#ifdef MZ_USE_JIT
  if (_cont)
    nt = ((Scheme_Cont *)_cont)->native_trace;
  else if (econt)
    nt = ((Scheme_Escaping_Cont *)econt)->native_trace;
  else
    nt = scheme_native_stack_trace();
#else
  nt = NULL;
#endif

  set = MALLOC_ONE_TAGGED(Scheme_Cont_Mark_Set);
  set->so.type = scheme_cont_mark_set_type;
  set->chain = first;
  set->cmpos = cmpos;
  set->native_stack_trace = nt;

  return (Scheme_Object *)set;
}

Scheme_Object *scheme_current_continuation_marks(Scheme_Object *prompt_tag)
{
  return continuation_marks(scheme_current_thread, NULL, NULL, NULL, 
                            prompt_tag ? prompt_tag : scheme_default_prompt_tag,
                            "continuation-marks",
                            0);
}

Scheme_Object *scheme_all_current_continuation_marks()
{
  return continuation_marks(scheme_current_thread, NULL, NULL, NULL, 
                            NULL,
                            "continuation-marks",
                            0);
}

static Scheme_Object *
cc_marks(int argc, Scheme_Object *argv[])
{
  if (argc) {
    if (!SAME_TYPE(scheme_prompt_tag_type, SCHEME_TYPE(argv[0]))) {
      scheme_wrong_type("current-continuation-marks", "continuation-prompt-tag",
                        0, argc, argv);
    }

    if (!SAME_OBJ(scheme_default_prompt_tag, argv[0]))
      if (!scheme_extract_one_cc_mark(NULL, SCHEME_PTR_VAL(argv[0])))
        scheme_arg_mismatch("current-continuation-marks",
                            "no corresponding prompt in the continuation: ",
                            argv[0]);
  }

  return scheme_current_continuation_marks(argc ? argv[0] : NULL);
}

static Scheme_Object *
cont_marks(int argc, Scheme_Object *argv[])
{
  Scheme_Object *prompt_tag;

  if (!SCHEME_CONTP(argv[0]) && !SCHEME_ECONTP(argv[0]))
    scheme_wrong_type("continuation-marks", "continuation", 0, argc, argv);

  if (argc > 1) {
    if (!SAME_TYPE(scheme_prompt_tag_type, SCHEME_TYPE(argv[1]))) {
      scheme_wrong_type("continuation-marks", "continuation-prompt-tag",
                        1, argc, argv);
    }
    prompt_tag = argv[1];
  } else
    prompt_tag = scheme_default_prompt_tag;

  if (SCHEME_ECONTP(argv[0])) {
    if (!scheme_escape_continuation_ok(argv[0])) {
      scheme_arg_mismatch("continuation-marks",
			  "escape continuation not in the current thread's continuation: ",
			  argv[0]);
      return NULL;
    } else {
      Scheme_Meta_Continuation *mc;
      scheme_extract_one_cc_mark_with_meta(NULL, argv[0], NULL, &mc, NULL);

      return continuation_marks(scheme_current_thread, NULL, argv[0], mc, prompt_tag, 
                                "continuation-marks", 0);
    }
  } else {
    return continuation_marks(NULL, argv[0], NULL, NULL, prompt_tag, 
                              "continuation-marks", 0);
  }
}

static Scheme_Object *
cc_marks_p(int argc, Scheme_Object *argv[])
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_cont_mark_set_type))
    return scheme_false;
  else
    return scheme_true;
}

static Scheme_Object *
extract_cc_marks(int argc, Scheme_Object *argv[])
{
  Scheme_Cont_Mark_Chain *chain;
  Scheme_Object *first = scheme_null, *last = NULL, *key, *prompt_tag;
  Scheme_Object *pr;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_cont_mark_set_type)) {
    scheme_wrong_type("continuation-mark-set->list", "continuation-mark-set", 0, argc, argv);
    return NULL;
  }
  if (argc > 2) {
    if (!SAME_TYPE(scheme_prompt_tag_type, SCHEME_TYPE(argv[2]))) {
      scheme_wrong_type("continuation-mark-set->list", "continuation-prompt-tag",
                        2, argc, argv);
    }
    prompt_tag = argv[2];
  } else
    prompt_tag = scheme_default_prompt_tag;

  chain = ((Scheme_Cont_Mark_Set *)argv[0])->chain;
  key = argv[1];

  if ((key == scheme_parameterization_key)
      || (key == scheme_break_enabled_key)
      || (key == scheme_exn_handler_key)) {
    scheme_signal_error("continuation-mark-set->list: secret key leaked!");
    return NULL;
  }

  prompt_tag = SCHEME_PTR_VAL(prompt_tag);

  while (chain) {
    if (chain->key == key) {
      pr = scheme_make_pair(chain->val, scheme_null);
      if (last)
	SCHEME_CDR(last) = pr;
      else
	first = pr;
      last = pr;
    } else if (chain->key == prompt_tag)
      break;

    chain = chain->next;
  }

  return first;
}

static Scheme_Object *
extract_cc_markses(int argc, Scheme_Object *argv[])
{
  Scheme_Cont_Mark_Chain *chain;
  Scheme_Object *first = scheme_null, *last = NULL;
  Scheme_Object *pr, **keys, *vals, *none, *prompt_tag;
  int len, i;
  long last_pos;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_cont_mark_set_type)) {
    scheme_wrong_type("continuation-mark-set->list*", "continuation-mark-set", 0, argc, argv);
    return NULL;
  }
  len = scheme_proper_list_length(argv[1]);
  if (len < 0) {
    scheme_wrong_type("continuation-mark-set->list*", "list", 1, argc, argv);
    return NULL;
  }
  if (argc > 2)
    none = argv[2];
  else
    none = scheme_false;
  if (argc > 3) {
    if (!SAME_TYPE(scheme_prompt_tag_type, SCHEME_TYPE(argv[3]))) {
      scheme_wrong_type("continuation-mark-set->list*", "continuation-prompt-tag",
                        3, argc, argv);
    }
    prompt_tag = argv[3];
  } else
    prompt_tag = scheme_default_prompt_tag;

  keys = MALLOC_N(Scheme_Object *, len);
  for (pr = argv[1], i = 0; SCHEME_PAIRP(pr); pr = SCHEME_CDR(pr), i++) {
    keys[i] = SCHEME_CAR(pr);
    if ((keys[i] == scheme_parameterization_key)
	|| (keys[i] == scheme_break_enabled_key)
	|| (keys[i] == scheme_exn_handler_key)) {
      scheme_signal_error("continuation-mark-set->list: secret key leaked!");
      return NULL;
    }
  }

  prompt_tag = SCHEME_PTR_VAL(prompt_tag);

  chain = ((Scheme_Cont_Mark_Set *)argv[0])->chain;
  last_pos = ((Scheme_Cont_Mark_Set *)argv[0])->cmpos + 2;

  while (chain) {
    for (i = 0; i < len; i++) {
      if (SAME_OBJ(chain->key, keys[i])) {
	long pos;
	pos = (long)chain->pos;
	if (pos != last_pos) {
	  vals = scheme_make_vector(len, none);
	  last_pos = pos;
	  pr = scheme_make_pair(vals, scheme_null);
	  if (last)
	    SCHEME_CDR(last) = pr;
	  else
	    first = pr;
	  last = pr;
	} else
	  vals = SCHEME_CAR(last);
	SCHEME_VEC_ELS(vals)[i] = chain->val;
      }
    }

    if (SAME_OBJ(chain->key, prompt_tag))
      break;
    
    chain = chain->next;
  }

  return first;
}

Scheme_Object *
scheme_get_stack_trace(Scheme_Object *mark_set)
{
  Scheme_Object *l, *n, *m, *name, *loc;
  Scheme_Object *a[2];

  l = ((Scheme_Cont_Mark_Set *)mark_set)->native_stack_trace;

  if (!l) {
    a[0] = mark_set;
    a[1] = scheme_stack_dump_key;
    
    l = extract_cc_marks(2, a);
  } else {
    /* Copy l: */
    Scheme_Object *first = scheme_null, *last = NULL;
    while (SCHEME_PAIRP(l)) {
      n = scheme_make_pair(SCHEME_CAR(l), scheme_null);
      if (last)
	SCHEME_CDR(last) = n;
      else
	first = n;
      last = n;
      l = SCHEME_CDR(l);
    }
    l = first;
  }

  /* Filter out NULLs */
  while (SCHEME_PAIRP(l) && !SCHEME_CAR(l)) {
    l = SCHEME_CDR(l);
  }
  for (n = l; SCHEME_PAIRP(n); ) { 
    m = SCHEME_CDR(n);
    if (SCHEME_NULLP(m))
      break;
    if (SCHEME_CAR(m)) {
      n = m;
    } else {
      SCHEME_CDR(n) = SCHEME_CDR(m);
    }
  }

  /* Make srclocs */
  for (n = l; SCHEME_PAIRP(n); n = SCHEME_CDR(n)) { 
    name = SCHEME_CAR(n);
    if (SCHEME_VECTORP(name)) {
      loc = scheme_make_location(SCHEME_VEC_ELS(name)[1],
				 SCHEME_VEC_ELS(name)[2],
				 SCHEME_VEC_ELS(name)[3],
				 SCHEME_VEC_ELS(name)[4],
				 SCHEME_VEC_ELS(name)[5]);
      if (SCHEME_TRUEP(SCHEME_VEC_ELS(name)[6]))
	name = scheme_make_pair(scheme_false, loc);
      else
	name = scheme_make_pair(SCHEME_VEC_ELS(name)[0], loc);
    } else {
      name = scheme_make_pair(name, scheme_false);
    }
    SCHEME_CAR(n) = name;
  }

  return l;
}

static Scheme_Object *
extract_cc_proc_marks(int argc, Scheme_Object *argv[])
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_cont_mark_set_type)) {
    scheme_wrong_type("continuation-mark-set->context", "continuation-mark-set", 0, argc, argv);
    return NULL;
  }

  return scheme_get_stack_trace(argv[0]);
}

Scheme_Object *
scheme_extract_one_cc_mark_with_meta(Scheme_Object *mark_set, Scheme_Object *key, 
                                     Scheme_Object *prompt_tag, Scheme_Meta_Continuation **_meta,
                                     MZ_MARK_POS_TYPE *_vpos)
{
  if (mark_set) {
    Scheme_Cont_Mark_Chain *chain;
    chain = ((Scheme_Cont_Mark_Set *)mark_set)->chain;
    while (chain) {
      if (chain->key == key)
	return chain->val;
      else 
	chain = chain->next;
    }
  } else {
    long findpos, bottom, startpos;
    long pos;
    Scheme_Object *val = NULL;
    MZ_MARK_POS_TYPE vpos = 0;
    Scheme_Object *cache;
    Scheme_Meta_Continuation *mc = NULL;
    Scheme_Cont_Mark *seg;
    Scheme_Thread *p = scheme_current_thread;

    do {
      if (mc) {
        startpos = mc->cont_mark_total;
        bottom = 0;
      } else {
        startpos = (long)MZ_CONT_MARK_STACK;
        if (!p->cont_mark_stack_segments)
          findpos = 0;
        bottom = p->cont_mark_stack_bottom;
      }

      findpos = startpos;

      /* Search mark stack, checking caches along the way: */
      while (findpos-- > bottom) {
        if (mc) {
          seg = mc->cont_mark_stack_copied;
          pos = findpos;
        } else {
          seg = p->cont_mark_stack_segments[findpos >> SCHEME_LOG_MARK_SEGMENT_SIZE];
          pos = findpos & SCHEME_MARK_SEGMENT_MASK;
        }

        if (SAME_OBJ(seg[pos].key, key)) {
          val = seg[pos].val;
          vpos = seg[pos].pos;
          break;
        } else {
          cache = seg[pos].cache;
          if (cache && SCHEME_HASHTP(cache))
            cache = scheme_hash_get((Scheme_Hash_Table *)cache, 
                                    prompt_tag ? prompt_tag : scheme_false);
          else if (prompt_tag)
            cache = NULL;
          if (cache && SCHEME_VECTORP(cache)) {
            /* If slot 1 has a key, this cache has just one key--value
               pair. Otherwise, slot 2 is a hash table. */
            if (SCHEME_VEC_ELS(cache)[1]) {
              if (SAME_OBJ(SCHEME_VEC_ELS(cache)[1], key)) {
                val = SCHEME_VEC_ELS(cache)[2];
                vpos = (MZ_MARK_POS_TYPE)SCHEME_VEC_ELS(cache)[3];
                break;
              }
            } else {
              Scheme_Hash_Table *ht;
              ht = (Scheme_Hash_Table *)SCHEME_VEC_ELS(cache)[2];
              val = scheme_hash_get(ht, key);
              if (val) {
                vpos = (MZ_MARK_POS_TYPE)SCHEME_CDR(val);
                val = SCHEME_CAR(val);
                break;
              }
            }
          }
	}
      }

      pos = startpos - findpos;
      if (pos > 16) {
        pos >>= 1;
        findpos = findpos + pos;
        if (mc) {
          seg = mc->cont_mark_stack_copied;
          pos = findpos;
        } else {
          seg = p->cont_mark_stack_segments[findpos >> SCHEME_LOG_MARK_SEGMENT_SIZE];
          pos = findpos & SCHEME_MARK_SEGMENT_MASK;
        }

        /* See continuation_marks() for information on what
           cache can be: */
        cache = seg[pos].cache;
        {
          Scheme_Hash_Table *cht;
          if (cache && SCHEME_HASHTP(cache)) {
            cht = (Scheme_Hash_Table *)cache;
            cache = scheme_hash_get(cht, prompt_tag ? prompt_tag : scheme_false);
          } else if (prompt_tag) {
            cht = scheme_make_hash_table(SCHEME_hash_ptr);
            if (cache) {
              if (SCHEME_VECTORP(cache)) {
                Scheme_Object *vec;
                if (SCHEME_VEC_ELS(cache)[0])
                  scheme_hash_set(cht, scheme_default_prompt_tag, SCHEME_VEC_ELS(cache)[0]);
                /* Don't try to re-use cache just for the null key */
                vec = scheme_make_vector(4, NULL);
                SCHEME_VEC_ELS(vec)[1] = SCHEME_VEC_ELS(cache)[1];
                SCHEME_VEC_ELS(vec)[2] = SCHEME_VEC_ELS(cache)[2];
                SCHEME_VEC_ELS(vec)[3] = SCHEME_VEC_ELS(cache)[3];
                scheme_hash_set(cht, scheme_false, vec);
              } else {
                scheme_hash_set(cht, scheme_default_prompt_tag, cache);
              }
              cache = NULL;
            }
            seg[pos].cache = (Scheme_Object *)cht;
          } else
            cht = NULL;

          if (!cache || !SCHEME_VECTORP(cache)) {
            /* No cache so far, so map one key */
            cache = scheme_make_vector(4, NULL);
            SCHEME_VEC_ELS(cache)[1] = key;
            SCHEME_VEC_ELS(cache)[2] = val;
            SCHEME_VEC_ELS(cache)[3] = (Scheme_Object *)vpos;
            if (cht) {
              scheme_hash_set(cht, prompt_tag ? prompt_tag : scheme_false, cache);
            } else {
              if (seg[pos].cache && !SCHEME_FALSEP(seg[pos].cache))
                SCHEME_VEC_ELS(cache)[0] = seg[pos].cache;
              seg[pos].cache = cache;
            }
          } else {
            if (SCHEME_VEC_ELS(cache)[1]) {
              /* More than one cached key, now; create hash table */
              Scheme_Hash_Table *ht;
              ht = scheme_make_hash_table(SCHEME_hash_ptr);
              scheme_hash_set(ht, key, scheme_make_raw_pair(val, (Scheme_Object *)vpos));
              scheme_hash_set(ht, SCHEME_VEC_ELS(cache)[1], scheme_make_raw_pair(SCHEME_VEC_ELS(cache)[2],
                                                                                 SCHEME_VEC_ELS(cache)[3]));
              SCHEME_VEC_ELS(cache)[1] = NULL;
              SCHEME_VEC_ELS(cache)[2] = (Scheme_Object *)ht;
            } else {
              /* Already have a hash table */
              Scheme_Hash_Table *ht;
              ht = (Scheme_Hash_Table *)SCHEME_VEC_ELS(cache)[2];
              scheme_hash_set(ht, key, scheme_make_raw_pair(val, (Scheme_Object *)vpos));
            }
          }
        }
      }

      if (val) {
        if (_meta)
          *_meta = mc;
        if (_vpos)
          *_vpos = vpos;
        return val;
      }
      
      if (mc) {
        mc = mc->next;
      } else {
        mc = p->meta_continuation;
      }
    } while (mc);
  }
  
  if (key == scheme_parameterization_key) {
    return (Scheme_Object *)scheme_current_thread->init_config;
  }
  if (key == scheme_break_enabled_key) {
    return scheme_current_thread->init_break_cell;
  }
  
  return NULL;
}

Scheme_Object *
scheme_extract_one_cc_mark(Scheme_Object *mark_set, Scheme_Object *key)
{
  return scheme_extract_one_cc_mark_with_meta(mark_set, key, NULL, NULL, NULL);
}

Scheme_Object *
scheme_extract_one_cc_mark_to_tag(Scheme_Object *mark_set, Scheme_Object *key,
                                  Scheme_Object *prompt_tag)
{
  return scheme_extract_one_cc_mark_with_meta(mark_set, key, prompt_tag, NULL, NULL);
}

static Scheme_Object *
extract_one_cc_mark(int argc, Scheme_Object *argv[])
{
  Scheme_Object *r;
  Scheme_Object *prompt_tag;

  if (SCHEME_TRUEP(argv[0])
      && !SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_cont_mark_set_type))
    scheme_wrong_type("continuation-mark-set-first", "continuation-mark-set or #f", 0, argc, argv);
  
  if ((argv[1] == scheme_parameterization_key)
      || (argv[1] == scheme_break_enabled_key)) {
    /* Minor hack: these keys are used in "startup.ss" to access
       parameterizations, and we want that access to go through
       prompts. If they keys somehow leaked, it's ok, because that
       doesn't expose anything that isn't already exposed by functions
       like `current-parameterization'. */
    prompt_tag = NULL; 
  } else
    prompt_tag = scheme_default_prompt_tag;

  if (argc > 3) {
    if (!SAME_TYPE(scheme_prompt_tag_type, SCHEME_TYPE(argv[3]))) {
      scheme_wrong_type("continuation-mark-set-first", "continuation-prompt-tag",
                        3, argc, argv);
    }
    prompt_tag = argv[3];

    if (!SAME_OBJ(scheme_default_prompt_tag, prompt_tag)) {
      if (SCHEME_FALSEP(argv[0])) {
        if (!scheme_extract_one_cc_mark(NULL, SCHEME_PTR_VAL(prompt_tag)))
          scheme_arg_mismatch("continuation-mark-set-first",
                              "no corresponding prompt in the current continuation: ",
                              prompt_tag);
      }
    }
  } 

  r = scheme_extract_one_cc_mark_with_meta(SCHEME_TRUEP(argv[0]) ? argv[0] : NULL, argv[1], 
                                           prompt_tag, NULL, NULL);
  if (!r) {
    if (argc > 2)
      r = argv[2];
    else
      r = scheme_false;
  }

  return r;
}

int scheme_is_cm_deeper(Scheme_Meta_Continuation *m1, MZ_MARK_POS_TYPE p1,
                        Scheme_Meta_Continuation *m2, MZ_MARK_POS_TYPE p2)
{
  if (m1 != m2) {
    if (!m1)
      return 0;
    if (!m2)
      return 1;
    return (m1->depth < m2->depth);
  }
  return p1 < p2;
}

static Scheme_Object *continuation_prompt_available(int argc, Scheme_Object *argv[])
{
  Scheme_Object *prompt_tag;

  prompt_tag = argv[0];
  if (!SAME_TYPE(scheme_prompt_tag_type, SCHEME_TYPE(prompt_tag))) {
    scheme_wrong_type("continuation-prompt-available?", "continuation-prompt-tag",
                      0, argc, argv);
  }

  if (argc > 1) {
    if (SCHEME_ECONTP(argv[1])) {
      if (!scheme_escape_continuation_ok(argv[1])) {
        scheme_arg_mismatch("continuation-prompt-available?",
                            "escape continuation not in the current thread's continuation: ",
                            argv[1]);
        return NULL;
      } else {
        Scheme_Meta_Continuation *mc;

        if (SAME_OBJ(scheme_default_prompt_tag, prompt_tag))
          return scheme_true;

        scheme_extract_one_cc_mark_with_meta(NULL, argv[1], NULL, &mc, NULL);
        
        if (continuation_marks(scheme_current_thread, NULL, argv[1], mc, prompt_tag, 
                               NULL, 0))
          return scheme_true;
      }
    } else if (SCHEME_CONTP(argv[1])) {
      if (continuation_marks(NULL, argv[1], NULL, NULL, prompt_tag, NULL, 0))
        return scheme_true;
    } else {
      scheme_wrong_type("continuation-prompt-available?", "continuation",
                        1, argc, argv);
    }
  } else {
    if (SAME_OBJ(scheme_default_prompt_tag, prompt_tag))
      return scheme_true;

    if (scheme_extract_one_cc_mark(NULL, SCHEME_PTR_VAL(prompt_tag)))
      return scheme_true;
  }

  return scheme_false;
}

/*========================================================================*/
/*                             dynamic-wind                               */
/*========================================================================*/

typedef struct {
  MZTAG_IF_REQUIRED
  Scheme_Object *pre, *act, *post;
} Dyn_Wind;

static void pre_post_dyn_wind(Scheme_Object *prepost)
{
  Scheme_Cont_Frame_Data cframe;

  /* Cancel internal suspend in eval or dyn-wind, because we convert
     it to a parameterize. */
  --scheme_current_thread->suspend_break;
  ASSERT_SUSPEND_BREAK_ZERO();

  scheme_push_break_enable(&cframe, 0, 0);

  /* Here's the main call: */
  (void)_scheme_apply_multi(prepost, 0, NULL);

  scheme_pop_break_enable(&cframe, 0);

  /* Restore internal suspend: */
  scheme_current_thread->suspend_break++;
}

static Scheme_Object *do_dyn_wind(void *d)
{
  Dyn_Wind *dw;
  dw = (Dyn_Wind *)d;

  return _scheme_apply_multi(dw->act, 0, NULL);
}

static void pre_dyn_wind(void *d)
{
  pre_post_dyn_wind(((Dyn_Wind *)d)->pre);
}

static void post_dyn_wind(void *d)
{
  pre_post_dyn_wind(((Dyn_Wind *)d)->post);
}

static Scheme_Object *dynamic_wind(int c, Scheme_Object *argv[])
{
  Dyn_Wind *dw;
  Scheme_Object *v;

  scheme_check_proc_arity("dynamic-wind", 0, 0, c, argv);
  scheme_check_proc_arity("dynamic-wind", 0, 1, c, argv);
  scheme_check_proc_arity("dynamic-wind", 0, 2, c, argv);

  dw = MALLOC_ONE_RT(Dyn_Wind);
#ifdef MZTAG_REQUIRED
  dw->type = scheme_rt_dyn_wind_info;
#endif

  dw->pre = argv[0];
  dw->act = argv[1];
  dw->post = argv[2];

  v = scheme_dynamic_wind(pre_dyn_wind, do_dyn_wind, post_dyn_wind, NULL,
			  (void *)dw);

  /* We may have just re-activated breaking: */
  {
    Scheme_Thread *p = scheme_current_thread;
    if (p->external_break && scheme_can_break(p)) {
      Scheme_Object **save_values;
      int save_count;

      if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES)) {
	save_count = p->ku.multiple.count;
	save_values = p->ku.multiple.array;
	p->ku.multiple.array = NULL;
	if (SAME_OBJ(save_values, p->values_buffer))
	  p->values_buffer = NULL;
      } else {
	save_count = 0;
	save_values = NULL;
      }

      scheme_thread_block_w_thread(0.0, p);
      p->ran_some = 1;

      if (save_values) {
	p->ku.multiple.count = save_count;
	p->ku.multiple.array = save_values;
      }
    }
  }

  return v;
}

Scheme_Object *scheme_dynamic_wind(void (*pre)(void *),
				   Scheme_Object *(* volatile act)(void *),
				   void (* volatile post)(void *),
				   Scheme_Object *(*jmp_handler)(void *),
				   void * volatile data)
{
  mz_jmp_buf newbuf;
  Scheme_Object * volatile v, ** volatile save_values;
  volatile int err;
  Scheme_Dynamic_Wind * volatile dw;
  volatile int save_count, old_cac;
  Scheme_Thread *p;
  int delta;

  p = scheme_current_thread;

  dw = MALLOC_ONE_RT(Scheme_Dynamic_Wind);
#ifdef MZTAG_REQUIRED
  dw->type = scheme_rt_dyn_wind;
#endif

  dw->data = data;
  dw->pre = pre;
  dw->post = post;
  dw->prev = p->dw;
  if (dw->prev)
    dw->depth = dw->prev->depth + 1;
  else
    dw->depth = 0;
  dw->next_meta = p->next_meta;

  if (pre) {
    ASSERT_SUSPEND_BREAK_ZERO();
    p->suspend_break++;
    pre(data);
    p = scheme_current_thread;
    --p->suspend_break;
  }

  p->next_meta = 0;
  p->dw = dw;
  
  dw->saveerr = scheme_current_thread->error_buf;
  scheme_current_thread->error_buf = &newbuf;

   scheme_save_env_stack_w_thread(dw->envss, p);

  if (scheme_setjmp(newbuf)) {
    p = scheme_current_thread;
    scheme_restore_env_stack_w_thread(dw->envss, p);
    if ((p->dw != dw)
        && (!p->dw || !dw->id || (p->dw->id != dw->id))) {
      /* A full continuation jump was interrupted by an
	 escape continuation jump (in a dw pre or post thunk). Either
           1. this dw's post is already done for an interupted upward
              jump; or
           2. we never actually got this far for an interrupted
	      downward jump.
	 In either case, skip up until we get to the right level. */
      scheme_longjmp(*dw->saveerr, 1);
    } else {
      if (jmp_handler)
	v = jmp_handler(data);
      else
	v = NULL;
      err = !v;
    }
  } else {
    if (pre) {
      /* Need to check for a break, in case one was queued during
	 pre: */
      scheme_check_break_now();
    }

    v = act(data);

    err = 0;
  }

  p = scheme_current_thread;
  if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES)) {
    save_count = p->ku.multiple.count;
    save_values = p->ku.multiple.array;
    p->ku.multiple.array = NULL;
    if (SAME_OBJ(save_values, p->values_buffer))
      p->values_buffer = NULL;
  } else {
    save_count = 0;
    save_values = NULL;
  }

  delta = p->dw->next_meta;
  p->dw = p->dw->prev; /* note: use p->dw, not dw, in case
                          continuation was composed */
  p->next_meta += delta;

  /* Don't run Scheme-based dyn-winds when we're killing a nested thread. */
  if (err && p->cjs.is_kill && (post == post_dyn_wind))
    post = NULL;

  old_cac = scheme_continuation_application_count;

  if (post) {
    p->error_buf = &newbuf;
    if (scheme_setjmp(newbuf)) {
      p = scheme_current_thread;
      scheme_restore_env_stack_w_thread(dw->envss, p);
      err = 1;
    } else {
      Scheme_Continuation_Jump_State cjs;
      p = scheme_current_thread;
      ASSERT_SUSPEND_BREAK_ZERO();
      p->suspend_break++;
      copy_cjs(&cjs, &p->cjs);
      reset_cjs(&p->cjs);
      post(data);
      copy_cjs(&p->cjs, &cjs);
      p = scheme_current_thread;
      --p->suspend_break;
    }
  }

  if (err) {
    /* If we're escaping to a prompt or escape continuation,
       check that it's still there. */
    if ((old_cac != scheme_continuation_application_count)
        && p->cjs.jumping_to_continuation) {
      p->error_buf = dw->saveerr;
      if (SAME_TYPE(SCHEME_TYPE(p->cjs.jumping_to_continuation), scheme_prompt_type)) {
        Scheme_Object *tag;
        Scheme_Prompt *prompt;
        tag = ((Scheme_Prompt *)p->cjs.jumping_to_continuation)->tag;
        prompt = (Scheme_Prompt *)scheme_extract_one_cc_mark(NULL, SCHEME_PTR_VAL(tag));
        if (!prompt && SAME_OBJ(scheme_default_prompt_tag, tag)) {
          prompt = original_default_prompt;
        }
        if (!prompt) {
          scheme_arg_mismatch("abort-current-continuation", 
                              "abort in progress, but current continuation includes"
                              " no prompt with the given tag"
                              " after a `dynamic-wind' post-thunk return: ",
                              tag);
          return NULL;
        }
        p->cjs.jumping_to_continuation = (Scheme_Object *)prompt;
      } else if (SCHEME_ECONTP(p->cjs.jumping_to_continuation)) {
        if (!scheme_escape_continuation_ok(p->cjs.jumping_to_continuation)) {
          scheme_raise_exn(MZEXN_FAIL_CONTRACT_CONTINUATION,
                           "jump to escape continuation in progress,"
                           " but the target is not in the current continuation"
                           " after a `dynamic-wind' post-thunk return");
          return NULL;
        }
      }
    }

    scheme_longjmp(*dw->saveerr, 1);
  }

  p->error_buf = dw->saveerr;

  if (post) {
    /* Need to check for a break, in case one was queued during post: */
    scheme_check_break_now();
  }

  if (v == SCHEME_MULTIPLE_VALUES) {
    p->ku.multiple.count = save_count;
    p->ku.multiple.array = save_values;
  }

  return v;
}

void scheme_apply_dw_in_meta(Scheme_Dynamic_Wind *dw, int post_part, int meta_depth, Scheme_Cont *recheck)
{
  /* Run the given dw pre/post thunk, but let it see only the 
     continuation marks starting with the given meta-continuation.
     We don't want to actually prune the meta-continuation, since 
     that would be out of sync with the control state, so we instead
     replace the first meta_depth frames to prune the continuation marks.
     On return, we have to unprune those marks. (If there's an escape,
     then we don't have to unprune, because the escape jumps out of
     the pruned meta-continuations.) Unfortunately, pruning this way
     requires time proportional to the meta depth.
     
     The pre/post thunk might install it's own marks. In that case, it
     uses the current mark stack. We don't care about the current mark
     stack's state, since we're either on our way out, or we're on our
     way in and we haven't started restoring the marks. So start with
     a clean mark stack, but make sure it doesn't appear to be in tail
     position for a meta-continuation.

     The pre/post thunk might jump, or it might capture a continuation that
     is later restored. In that case, the meta-continuation can be extended
     or different by the time we get back. That's why we take a meta_depth,
     rather than a meta continuation (i.e., the loop that calls this
     function shouldn't remember meta-continuations). The meta-continuation
     can't become shorter than the current needed meta_depth. It may become
     shorter than it was originally, which is relevant to a post loop that
     calls this function; but the d-w list for posts will become shorter in 
     that case, too, so the post loop is fine as long as it consults
     scheme_current_thread->dw.
  */
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Meta_Continuation *mc, *old_mc, *rest;
  long delta;
  int i, actual_depth;
  int old_cac;

  MZ_CONT_MARK_STACK = p->cont_mark_stack_bottom;
  MZ_CONT_MARK_POS = p->meta_continuation->meta_tail_pos + 2;

  old_mc = p->meta_continuation;

  /* clone the first meta_depth meta continuations: */
  for (i = 0, actual_depth = 0, rest = old_mc; i < meta_depth; actual_depth++) {
    if (rest->overflow)
      i++;
    rest = rest->next;
  }
  mc = clone_meta_cont(p->meta_continuation, NULL, actual_depth, NULL, NULL, rest, 0);
  p->meta_continuation = mc;

  /* strip the marks of the first actual_depth-1 meta continuations */
  rest = mc;
  for (i = 0; i < actual_depth - 1; i++) {
    rest->cont_mark_total = 0;
    rest->cont_mark_offset = 0;
    rest->cont_mark_stack_copied = NULL;
    rest = rest->next;
  }

  /* prune the actual_depth's meta continuation's marks. */
  delta = rest->cont_mark_stack - dw->envss.cont_mark_stack;
  if (delta) {
    rest->cont_mark_total -= delta;
    rest->cont_mark_stack -= delta;
    if (rest->cont_mark_total) {
      Scheme_Cont_Mark *cp;
      cp = MALLOC_N(Scheme_Cont_Mark, rest->cont_mark_total);
      memcpy(cp, rest->cont_mark_stack_copied, rest->cont_mark_total * sizeof(Scheme_Cont_Mark));
      rest->cont_mark_stack_copied = cp;
    } else
      rest->cont_mark_stack_copied = NULL;
  }

  old_cac = scheme_continuation_application_count;

  /* Run the post or pre thunk: */
  if (post_part) {
    DW_PrePost_Proc post = dw->post;
    post(dw->data);
  } else {
    DW_PrePost_Proc pre = dw->pre;
    pre(dw->data);
  }

  p = scheme_current_thread;

  if (recheck) {
    if (scheme_continuation_application_count != old_cac) {
      scheme_recheck_prompt_and_barrier(recheck);
    }
  }

  /* restore the first meta_depth meta continuations (onto
     a tail that is possibly different than when we captured
     old_mc) */
  for (i = 0, rest = p->meta_continuation; i < actual_depth; i++) {
    rest = rest->next;
  }
  old_mc = clone_meta_cont(old_mc, NULL, actual_depth, NULL, NULL, rest, 0);
  p->meta_continuation = old_mc;
}

/*========================================================================*/
/*                                  time                                  */
/*========================================================================*/

#ifdef TIME_SYNTAX

#ifndef CLOCKS_PER_SEC
#define CLOCKS_PER_SEC 1000000
#endif

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif

long scheme_get_milliseconds(void)
{
#ifdef USE_MACTIME
  return scheme_get_process_milliseconds();
#else
# ifdef USE_FTIME
  struct MSC_IZE(timeb) now;
  MSC_IZE(ftime)(&now);
  return (long)(now.time * 1000 + now.millitm);
# else
#  ifdef PALMOS_STUFF
  /* FIXME */
  return 0;
#  else
  struct timeval now;
  gettimeofday(&now, NULL);
  return now.tv_sec * 1000 + now.tv_usec / 1000;
#  endif
# endif
#endif
}

double scheme_get_inexact_milliseconds(void)
{
#ifdef USE_MACTIME
  {
    UnsignedWide time;
    Microseconds(&time);
    return (((double)(time.lo >> 10)
	    + ((double)(time.hi >> 10) * 4294967296.0))
	    * 1.024);
  }
#else
# ifdef USE_FTIME
  struct MSC_IZE(timeb) now;
  MSC_IZE(ftime)(&now);
  return (double)now.time * 1000.0 + (double)now.millitm;
# else
#  ifdef PALMOS_STUFF
  /* FIXME */
  return 0;
#  else
  struct timeval now;
  gettimeofday(&now, NULL);
  return (double)now.tv_sec * 1000.0 + (double)now.tv_usec / 1000;
#  endif
# endif
#endif
}

long scheme_get_process_milliseconds(void)
{
#ifdef USER_TIME_IS_CLOCK
  return scheme_get_milliseconds();
#else
# ifdef USE_GETRUSAGE
  struct rusage use;
  long s, u;

  getrusage(RUSAGE_SELF, &use);

  s = use.ru_utime.tv_sec + use.ru_stime.tv_sec;
  u = use.ru_utime.tv_usec + use.ru_stime.tv_usec;

  return s * 1000 + u / 1000;
# else
#  ifdef USE_MACTIME
  {
    UnsignedWide time;
    Microseconds(&time);
    return ((unsigned long)time.lo) / 1000;
  }
#  else
#   ifdef WINDOWS_GET_PROCESS_TIMES
  {
    FILETIME cr, ex, kr, us;
    if (GetProcessTimes(GetCurrentProcess(), &cr, &ex, &kr, &us)) {
      unsigned long n;
      n = (kr.dwLowDateTime + us.dwLowDateTime) / 10000;
      return n;
    }
  }
#   endif
  return clock()  * 1000 / CLOCKS_PER_SEC;

#  endif
# endif
#endif
}

#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

long scheme_get_seconds(void)
{
#ifdef USE_MACTIME
  unsigned long secs;
  GetDateTime(&secs);
  return secs;
#else
# ifdef USE_PALMTIME
  return TimGetSeconds();
# else
#  ifdef USE_FTIME
  struct MSC_IZE(timeb) now;
  MSC_IZE(ftime)(&now);
  return (long)now.time;
#  else
#   ifdef USE_PLAIN_TIME
  time_t now;
  now = time(NULL);
  return now;
#   else
  struct timeval now;
  gettimeofday(&now, NULL);
  return now.tv_sec;
#   endif
#  endif
# endif
#endif
}

#if defined(USE_MACTIME) || defined(USE_PALMTIME)
static int month_offsets[12] = { 0, 31, 59, 90,
                                120, 151, 181, 212,
                                243, 273, 304, 334 };
#endif

static Scheme_Object *seconds_to_date(int argc, Scheme_Object **argv)
{
  UNBUNDLE_TIME_TYPE lnow;
  int hour, min, sec, month, day, year, wday, yday, dst;
  long tzoffset;
#ifdef USE_MACTIME
# define CHECK_TIME_T unsigned long
  DateTimeRec localTime;
#else
# ifdef USE_PALMTIME
# define CHECK_TIME_T UInt32
  DateTimeType localTime;
# else
#  define CHECK_TIME_T time_t
  struct tm *localTime;
# endif
#endif
  CHECK_TIME_T now;
  Scheme_Object *p[10], *secs;

  secs = argv[0];

  if (!SCHEME_INTP(secs) && !SCHEME_BIGNUMP(secs)) {
    scheme_wrong_type("seconds->date", "exact integer", 0, argc, argv);
    return NULL;
  }

  if (scheme_get_time_val(secs, &lnow)
      && ((UNBUNDLE_TIME_TYPE)(now = (CHECK_TIME_T)lnow)) == lnow) {
    int success;

#ifdef USE_MACTIME
    SecondsToDate(lnow, &localTime);
    success = 1;
#else
# ifdef USE_PALMTIME
    TimSecondsToDateTime(lnow, &localTime) ;
# else
    localTime = localtime(&now);
    success = !!localTime;
# endif
#endif

    if (success) {
#if defined(USE_MACTIME) || defined(USE_PALMTIME)
# ifdef USE_MACTIME
#  define mzDOW(localTime) localTime.dayOfWeek - 1
# else
#  define mzDOW(localTime) localTime.weekDay
#endif

      hour = localTime.hour;
      min = localTime.minute;
      sec = localTime.second;

      month = localTime.month;
      day = localTime.day;
      year = localTime.year;

      wday = mzDOW(localTime);

      yday = month_offsets[localTime.month - 1] + localTime.day - 1;
      /* If month > 2, is it a leap-year? */
      if (localTime.month > 2) {
#ifdef USE_MACTIME
        unsigned long ttime;
        DateTimeRec tester;

	tester.year = localTime.year;
        tester.hour = tester.minute = 0;
        tester.second = 1;
        tester.month = 1;
        tester.day = 60;
        DateToSeconds(&tester, &ttime);
        SecondsToDate(ttime, &tester);
        if (tester.month == 2)
          /* It is a leap-year */
          yday++;
#else
	/* PalmOS: */
	if (DaysInMonth(2, year) > 28)
	  yday++;
#endif
      }

# ifdef USE_MACTIME
      {
	MachineLocation loc;
	ReadLocation(&loc);

	dst = (loc.u.dlsDelta != 0);

	tzoffset = loc.u.gmtDelta; /* 3-byte value in a long!! */
	/* Copied from Inside mac: */
	tzoffset = tzoffset & 0xFFFFFF;
	if (tzoffset & (0x1 << 23))
	  tzoffset |= 0xFF000000;
      }
# else
      /* No timezone on PalmOS: */
      tzoffset = 0;
# endif

#else
      hour = localTime->tm_hour;
      min = localTime->tm_min;
      sec = localTime->tm_sec;

      month = localTime->tm_mon + 1;
      day = localTime->tm_mday;
      year = localTime->tm_year + 1900;

      wday = localTime->tm_wday;
      yday = localTime->tm_yday;

      dst = localTime->tm_isdst;

      tzoffset = 0;
# ifdef USE_TIMEZONE_VAR
      tzoffset = -MSC_IZE(timezone);
# endif
# ifdef USE_TOD_FOR_TIMEZONE
      {
	  struct timezone xtz;
	  struct timeval xtv;
	  gettimeofday(&xtv, &xtz);
	  tzoffset = -(xtz.tz_minuteswest * 60);
      }
# endif
# ifdef USE_TIMEZONE_VAR_W_DLS
      tzoffset = -(MSCBOR_IZE(timezone) - (dst ? 3600 : 0));
# endif
# ifdef USE_TIMEZONE_AND_ALTZONE_VAR
      if (dst)
	tzoffset = -altzone;
      else
	tzoffset = -timezone;
# endif
# ifdef USE_TM_GMTOFF_FIELD
      tzoffset = localTime->tm_gmtoff;
# endif

#endif

      p[0] = scheme_make_integer(sec);
      p[1] = scheme_make_integer(min);
      p[2] = scheme_make_integer(hour);
      p[3] = scheme_make_integer(day);
      p[4] = scheme_make_integer(month);
      p[5] = scheme_make_integer(year);
      p[6] = scheme_make_integer(wday);
      p[7] = scheme_make_integer(yday);
      p[8] = dst ? scheme_true : scheme_false;
      p[9] = scheme_make_integer(tzoffset);

      return scheme_make_struct_instance(scheme_date, 10, p);
    }
  }

  scheme_raise_exn(MZEXN_FAIL,
		   "seconds->date: integer %s is out-of-range",
		   scheme_make_provided_string(secs, 0, NULL));

  return NULL;
}

static Scheme_Object *time_apply(int argc, Scheme_Object *argv[])
{
  long start, end;
  long cpustart, cpuend;
  long gcstart, gcend;
  long dur, cpudur, gcdur;
  int i, num_rands;
  Scheme_Object *v, *p[4], **rand_vec, *rands, *r;

  if (!SCHEME_PROCP(argv[0]))
    scheme_wrong_type("time-apply", "procedure", 0, argc, argv);

  rands = argv[1];

  num_rands = 0;
  r = rands;
  while (!SCHEME_NULLP(r)) {
    if (!SCHEME_PAIRP(r))
      scheme_wrong_type("time-apply", "proper list", 1, argc, argv);
    r = SCHEME_CDR(r);
    num_rands++;
  }

  if (SCHEME_FALSEP(get_or_check_arity(argv[0], num_rands, NULL))) {
    char *s;
    long aelen;

    s = scheme_make_arity_expect_string(argv[0], num_rands, NULL, &aelen);

    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "time-apply: arity mismatch for %t",
		     s, aelen);
    return NULL;
  }

  rand_vec = MALLOC_N(Scheme_Object *, num_rands);
  for (i = 0; SCHEME_PAIRP(rands); i++, rands = SCHEME_CDR(rands)) {
    rand_vec[i] = SCHEME_CAR(rands);
  }

  gcstart = scheme_total_gc_time;
  start = scheme_get_milliseconds();
  cpustart = scheme_get_process_milliseconds();
  v = _scheme_apply_multi(argv[0], num_rands, rand_vec);
  cpuend = scheme_get_process_milliseconds();
  end = scheme_get_milliseconds();
  gcend = scheme_total_gc_time;

  dur = end - start;
  cpudur = cpuend - cpustart;
  gcdur = gcend - gcstart;

  if (v == SCHEME_MULTIPLE_VALUES) {
    Scheme_Thread *cp = scheme_current_thread;
    if (SAME_OBJ(cp->ku.multiple.array, cp->values_buffer))
      cp->values_buffer = NULL;
    v = scheme_build_list(cp->ku.multiple.count,
			  cp->ku.multiple.array);
  } else
    v = scheme_make_pair(v, scheme_null);

  p[0] = v;
  p[1] = scheme_make_integer(cpudur);
  p[2] = scheme_make_integer(dur);
  p[3] = scheme_make_integer(gcdur);

  return scheme_values(4, p);
}

static Scheme_Object *current_milliseconds(int argc, Scheme_Object **argv)
{
  return scheme_make_integer(scheme_get_milliseconds());
}

static Scheme_Object *current_inexact_milliseconds(int argc, Scheme_Object **argv)
{
  return scheme_make_double(scheme_get_inexact_milliseconds());
}

static Scheme_Object *current_process_milliseconds(int argc, Scheme_Object **argv)
{
  return scheme_make_integer(scheme_get_process_milliseconds());
}

static Scheme_Object *current_gc_milliseconds(int argc, Scheme_Object **argv)
{
  return scheme_make_integer(scheme_total_gc_time);
}

static Scheme_Object *current_seconds(int argc, Scheme_Object **argv)
{
  long secs;
  secs = scheme_get_seconds();
  return scheme_make_integer_value_from_time(secs);
}

#endif


/*========================================================================*/
/*                             read-eval-print                            */
/*========================================================================*/

static Scheme_Object *
current_print(int argc, Scheme_Object **argv)
{
  return scheme_param_config("current-print",
			     scheme_make_integer(MZCONFIG_PRINT_HANDLER),
			     argc, argv,
			     1, NULL, NULL, 0);
}

static Scheme_Object *
current_prompt_read(int argc, Scheme_Object **argv)
{
  return scheme_param_config("current-prompt-read",
			     scheme_make_integer(MZCONFIG_PROMPT_READ_HANDLER),
			     argc, argv,
			     0, NULL, NULL, 0);
}

Scheme_Object *
scheme_default_print_handler(int argc, Scheme_Object *argv[])
{
  Scheme_Object *obj = argv[0];

  if (!SAME_OBJ(obj, scheme_void)) {
    Scheme_Config *config;
    Scheme_Object *port;
    Scheme_Object *argv[2];

    config = scheme_current_config();
    port = scheme_get_param(config, MZCONFIG_OUTPUT_PORT);

    argv[0] = obj;
    argv[1] = port;
    _scheme_apply(scheme_print_proc, 2, argv);
    scheme_write_byte_string("\n", 1, port);
  }

  return scheme_void;
}

Scheme_Object *
scheme_default_prompt_read_handler(int argc, Scheme_Object *argv[])
{
  Scheme_Config *config;
  Scheme_Object *port;
  Scheme_Object *inport;
  Scheme_Object *name;
  Scheme_Object *stx;
  Scheme_Cont_Frame_Data cframe;

  config = scheme_current_config();
  port = scheme_get_param(config, MZCONFIG_OUTPUT_PORT);
  inport = scheme_get_param(config, MZCONFIG_INPUT_PORT);

  scheme_write_byte_string("> ", 2, port);
  scheme_flush_output(port);

  name = ((Scheme_Input_Port *)inport)->name;

  if (inport == scheme_orig_stdin_port)
    scheme_flush_orig_outputs();

  config = scheme_extend_config(config, MZCONFIG_CAN_READ_READER, scheme_true);

  scheme_push_continuation_frame(&cframe);
  scheme_install_config(config);

  stx = scheme_read_syntax(inport, name);

  scheme_pop_continuation_frame(&cframe);

  return stx;
}

/*========================================================================*/
/*                        [un]marshalling closure code                    */
/*========================================================================*/

#define BOOL(x) (x ? scheme_true : scheme_false)

static Scheme_Object *write_compiled_closure(Scheme_Object *obj)
{
  Scheme_Closure_Data *data;
  Scheme_Object *name, *l;
  int svec_size;

  data = (Scheme_Closure_Data *)obj;

  if (data->name) {
    name = data->name;
    if (SCHEME_VECTORP(name)) {
      /* We can only save marshalable src names, which includes
	 paths, symbols, and strings: */
      Scheme_Object *src;
      src = SCHEME_VEC_ELS(name)[1];
      if (!SCHEME_PATHP(src)
	  && !SCHEME_PATHP(src)
	  && !SCHEME_SYMBOLP(src)) {
	/* Just keep the name */
	name = SCHEME_VEC_ELS(name)[0];
      }
    }
  } else {
    name = scheme_null;
  }

  svec_size = data->closure_size;
  if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REF_ARGS) {
    svec_size += (data->num_params + BITS_PER_MZSHORT - 1) / BITS_PER_MZSHORT;
  }

  l = CONS(scheme_make_svector(svec_size,
                               data->closure_map),
           scheme_protect_quote(data->code));

  if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REF_ARGS)
    l = CONS(scheme_make_integer(data->closure_size),
             l);

  return CONS(scheme_make_integer(SCHEME_CLOSURE_DATA_FLAGS(data)),
	      CONS(scheme_make_integer(data->num_params),
		   CONS(scheme_make_integer(data->max_let_depth),
			CONS(name,
			     l))));
}

static Scheme_Object *read_compiled_closure(Scheme_Object *obj)
{
  Scheme_Closure_Data *data;
  Scheme_Object *v;

#define BAD_CC "bad compiled closure"
#define X_SCHEME_ASSERT(x, y)

  data  = (Scheme_Closure_Data *)scheme_malloc_tagged(sizeof(Scheme_Closure_Data));

  data->iso.so.type = scheme_unclosed_procedure_type;

  if (!SCHEME_PAIRP(obj)) return NULL;
  v = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  SCHEME_CLOSURE_DATA_FLAGS(data) = (short)(SCHEME_INT_VAL(v));

  if (!SCHEME_PAIRP(obj)) return NULL;
  v = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  data->num_params = SCHEME_INT_VAL(v);

  if (!SCHEME_PAIRP(obj)) return NULL;
  data->max_let_depth = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return NULL;
  data->name = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  if (SCHEME_NULLP(data->name))
    data->name = NULL;

  if (!SCHEME_PAIRP(obj)) return NULL;
  v = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);

  /* v is an svector or an integer... */
  if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REF_ARGS) {
    if (!SCHEME_INTP(v)) return NULL;
    data->closure_size = SCHEME_INT_VAL(v);
    
    if (!SCHEME_PAIRP(obj)) return NULL;
    v = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);
  }

  data->code = obj;

  if (!SAME_TYPE(scheme_svector_type, SCHEME_TYPE(v))) return NULL;

  if (!(SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REF_ARGS))
    data->closure_size = SCHEME_SVEC_LEN(v);
  data->closure_map = SCHEME_SVEC_VEC(v);

  if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_FOLDABLE)
    SCHEME_CLOSURE_DATA_FLAGS(data) -= CLOS_FOLDABLE;

  if (SCHEME_TYPE(data->code) > _scheme_values_types_)
    SCHEME_CLOSURE_DATA_FLAGS(data) |= CLOS_FOLDABLE;

  /* If the closure is empty, create the closure now */
  if (!data->closure_size)
    return scheme_make_closure(NULL, (Scheme_Object *)data, 0);
  else
    return (Scheme_Object *)data;
}

/*========================================================================*/
/*                                precise GC                              */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_FUN_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_closure_info, mark_closure_info);
  GC_REG_TRAV(scheme_rt_dyn_wind_cell, mark_dyn_wind_cell);
  GC_REG_TRAV(scheme_rt_dyn_wind_info, mark_dyn_wind_info);
  GC_REG_TRAV(scheme_cont_mark_chain_type, mark_cont_mark_chain);
}

END_XFORM_SKIP;

#endif
