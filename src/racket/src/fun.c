/*
  Racket
  Copyright (c) 2004-2013 PLT Design Inc.
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
#include "schmach.h"

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
#    include <errno.h>
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
SHARED_OK int scheme_defining_primitives; /* set to 1 during start-up */

SHARED_OK int scheme_prim_opt_flags[(1 << SCHEME_PRIM_OPT_INDEX_SIZE)];

READ_ONLY Scheme_Object scheme_void[1]; /* the void constant */
READ_ONLY Scheme_Object *scheme_values_func; /* the function bound to `values' */
READ_ONLY Scheme_Object *scheme_procedure_p_proc;
READ_ONLY Scheme_Object *scheme_procedure_arity_includes_proc;
READ_ONLY Scheme_Object *scheme_void_proc;
READ_ONLY Scheme_Object *scheme_apply_proc;
READ_ONLY Scheme_Object *scheme_call_with_values_proc; /* the function bound to `call-with-values' */
READ_ONLY Scheme_Object *scheme_reduced_procedure_struct;
READ_ONLY Scheme_Object *scheme_tail_call_waiting;
READ_ONLY Scheme_Object *scheme_inferred_name_symbol;
READ_ONLY Scheme_Object *scheme_default_prompt_tag;

/* READ ONLY SHARABLE GLOBALS */

ROSYM static Scheme_Object *certify_mode_symbol;
ROSYM static Scheme_Object *taint_mode_symbol;
ROSYM static Scheme_Object *transparent_symbol;
ROSYM static Scheme_Object *transparent_binding_symbol;
ROSYM static Scheme_Object *opaque_symbol;
ROSYM static Scheme_Object *none_symbol;
ROSYM static Scheme_Object *is_method_symbol;
ROSYM static Scheme_Object *cont_key; /* uninterned */
ROSYM static Scheme_Object *barrier_prompt_key; /* uninterned */
ROSYM static Scheme_Object *prompt_cc_guard_key; /* uninterned */
READ_ONLY static Scheme_Prompt *original_default_prompt; /* for escapes, represents the implicit initial prompt */
READ_ONLY static Scheme_Object *call_with_prompt_proc;
READ_ONLY static Scheme_Object *abort_continuation_proc;
READ_ONLY static Scheme_Object *internal_call_cc_prim;
READ_ONLY static Scheme_Object *finish_call_cc_prim;

/* Caches need to be thread-local: */
THREAD_LOCAL_DECL(static Scheme_Prompt *available_prompt);
THREAD_LOCAL_DECL(static Scheme_Prompt *available_cws_prompt);
THREAD_LOCAL_DECL(static Scheme_Prompt *available_regular_prompt);
THREAD_LOCAL_DECL(static Scheme_Dynamic_Wind *available_prompt_dw);
THREAD_LOCAL_DECL(static Scheme_Meta_Continuation *available_prompt_mc);
THREAD_LOCAL_DECL(static Scheme_Cont *offstack_cont);
THREAD_LOCAL_DECL(static Scheme_Overflow *offstack_overflow);

THREAD_LOCAL_DECL(int scheme_cont_capture_count);
THREAD_LOCAL_DECL(static int scheme_prompt_capture_count);

/* locals */
static Scheme_Object *procedure_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *apply (int argc, Scheme_Object *argv[]);
static Scheme_Object *map (int argc, Scheme_Object *argv[]);
static Scheme_Object *for_each (int argc, Scheme_Object *argv[]);
static Scheme_Object *andmap (int argc, Scheme_Object *argv[]);
static Scheme_Object *ormap (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_cc (int argc, Scheme_Object *argv[]);
static Scheme_Object *internal_call_cc (int argc, Scheme_Object *argv[]);
static Scheme_Object *finish_call_cc (int argc, Scheme_Object *argv[]);
static Scheme_Object *continuation_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_with_continuation_barrier (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_with_prompt (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_with_control (int argc, Scheme_Object *argv[]);
static Scheme_Object *make_prompt_tag (int argc, Scheme_Object *argv[]);
static Scheme_Object *abort_continuation (int argc, Scheme_Object *argv[]);
static Scheme_Object *continuation_prompt_available(int argc, Scheme_Object *argv[]);
static Scheme_Object *get_default_prompt_tag (int argc, Scheme_Object *argv[]);
static Scheme_Object *prompt_tag_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *impersonate_prompt_tag (int argc, Scheme_Object *argv[]);
static Scheme_Object *chaperone_prompt_tag (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_with_sema (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_with_sema_enable_break (int argc, Scheme_Object *argv[]);
static Scheme_Object *make_continuation_mark_key (int argc, Scheme_Object *argv[]);
static Scheme_Object *continuation_mark_key_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *impersonate_continuation_mark_key (int argc, Scheme_Object *argv[]);
static Scheme_Object *chaperone_continuation_mark_key (int argc, Scheme_Object *argv[]);
static Scheme_Object *cc_marks (int argc, Scheme_Object *argv[]);
static Scheme_Object *cont_marks (int argc, Scheme_Object *argv[]);
static Scheme_Object *cc_marks_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *extract_cc_marks (int argc, Scheme_Object *argv[]);
static Scheme_Object *extract_cc_markses (int argc, Scheme_Object *argv[]);
static Scheme_Object *extract_cc_proc_marks (int argc, Scheme_Object *argv[]);
static Scheme_Object *extract_one_cc_mark (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_with_immediate_cc_mark (int argc, Scheme_Object *argv[]);
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
static Scheme_Object *procedure_arity_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *procedure_reduce_arity(int argc, Scheme_Object *argv[]);
static Scheme_Object *procedure_rename(int argc, Scheme_Object *argv[]);
static Scheme_Object *procedure_to_method(int argc, Scheme_Object *argv[]);
static Scheme_Object *procedure_equal_closure_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *chaperone_procedure(int argc, Scheme_Object *argv[]);
static Scheme_Object *impersonate_procedure(int argc, Scheme_Object *argv[]);
static Scheme_Object *primitive_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *primitive_closure_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *primitive_result_arity (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_with_values(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_values(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_print(int argc, Scheme_Object **argv);
static Scheme_Object *current_prompt_read(int, Scheme_Object **);
static Scheme_Object *current_read(int, Scheme_Object **);
static Scheme_Object *current_get_read_input_port(int, Scheme_Object **);


static Scheme_Object *chaperone_wrap_cc_guard(Scheme_Object *obj, Scheme_Object *proc);
static Scheme_Object *do_cc_guard(Scheme_Object *v, Scheme_Object *cc_guard, Scheme_Object *chaperone);

static Scheme_Object *
scheme_extract_one_cc_mark_with_meta(Scheme_Object *mark_set, Scheme_Object *key, 
                                     Scheme_Object *prompt_tag, Scheme_Meta_Continuation **_meta,
                                     MZ_MARK_POS_TYPE *_vpos);
static Scheme_Object *get_set_cont_mark_by_pos(Scheme_Object *key,
                                               Scheme_Thread *p,
                                               Scheme_Meta_Continuation *mc,
                                               MZ_MARK_POS_TYPE mpos,
                                               Scheme_Object *val);

static Scheme_Object *jump_to_alt_continuation();
static void reset_cjs(Scheme_Continuation_Jump_State *a);

typedef void (*DW_PrePost_Proc)(void *);

#define CONS(a,b) scheme_make_pair(a,b)

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif


/* See call_cc: */
typedef struct Scheme_Dynamic_Wind_List {
  MZTAG_IF_REQUIRED
  Scheme_Dynamic_Wind *dw;
  int meta_depth;
  struct Scheme_Dynamic_Wind_List *next;
} Scheme_Dynamic_Wind_List;

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

  REGISTER_SO(scheme_procedure_p_proc);
  REGISTER_SO(scheme_procedure_arity_includes_proc);

  o = scheme_make_folding_prim(procedure_p, "procedure?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(o) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE);
  scheme_add_global_constant("procedure?", o, env);

  scheme_procedure_p_proc = o;

  REGISTER_SO(scheme_apply_proc);
  scheme_apply_proc = scheme_make_prim_w_arity2(apply,
                                                "apply",
                                                2, -1,
                                                0, -1);
  scheme_add_global_constant("apply", scheme_apply_proc, env);
  scheme_add_global_constant("map",
			     scheme_make_noncm_prim(map,
                                                    "map",
                                                    2, -1),
			     env);
  scheme_add_global_constant("for-each",
			     scheme_make_noncm_prim(for_each,
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
  SCHEME_PRIM_PROC_FLAGS(scheme_values_func) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                                             | SCHEME_PRIM_IS_BINARY_INLINED
                                                                             | SCHEME_PRIM_IS_NARY_INLINED
                                                                             | SCHEME_PRIM_IS_OMITABLE);
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
  REGISTER_SO(finish_call_cc_prim);
  finish_call_cc_prim = scheme_make_prim_w_arity2(finish_call_cc,
                                                  "finish-call-with-current-continuation",
                                                  2, 2,
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
                                                    1, -1,
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
  scheme_add_global_constant("impersonate-prompt-tag",
                             scheme_make_prim_w_arity(impersonate_prompt_tag,
						      "impersonate-prompt-tag",
						      3, -1),
                             env);
  scheme_add_global_constant("chaperone-prompt-tag",
                             scheme_make_prim_w_arity(chaperone_prompt_tag,
						      "chaperone-prompt-tag",
						      3, -1),
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

  scheme_add_global_constant("make-continuation-mark-key",
			     scheme_make_prim_w_arity(make_continuation_mark_key,
						      "make-continuation-mark-key",
						      0, 1),
			     env);
  scheme_add_global_constant("continuation-mark-key?",
			     scheme_make_prim_w_arity(continuation_mark_key_p,
						      "continuation-mark-key?",
						      1, 1),
			     env);
  scheme_add_global_constant("impersonate-continuation-mark-key",
                             scheme_make_prim_w_arity(impersonate_continuation_mark_key,
						      "impersonate-continuation-mark-key",
						      3, -1),
                             env);
  scheme_add_global_constant("chaperone-continuation-mark-key",
                             scheme_make_prim_w_arity(chaperone_continuation_mark_key,
						      "chaperone-continuation-mark-key",
						      3, -1),
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

  o = scheme_make_prim_w_arity(extract_one_cc_mark,
                               "continuation-mark-set-first",
                               2, 4);
  SCHEME_PRIM_PROC_FLAGS(o) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED);
  scheme_add_global_constant("continuation-mark-set-first", o, env);

  scheme_add_global_constant("call-with-immediate-continuation-mark",
			     scheme_make_prim_w_arity2(call_with_immediate_cc_mark,
                                                       "call-with-immediate-continuation-mark",
                                                       2, 3,
                                                       0, -1),
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
  SCHEME_PRIM_PROC_FLAGS(scheme_void_proc) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_OMITABLE);
  scheme_add_global_constant("void", scheme_void_proc, env);

  
  o = scheme_make_folding_prim(void_p, "void?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(o) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE);
  scheme_add_global_constant("void?", o, env);

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
						      0, 1),
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
						      1, 2),
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
  scheme_add_global_constant("procedure-arity?",
			     scheme_make_folding_prim(procedure_arity_p,
						      "procedure-arity?",
						      1, 1, 1),
			     env);

  o = scheme_make_folding_prim(scheme_procedure_arity_includes,
                               "procedure-arity-includes?",
                               2, 3, 1);
  SCHEME_PRIM_PROC_FLAGS(o) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED);
  scheme_procedure_arity_includes_proc = o;
  scheme_add_global_constant("procedure-arity-includes?", o, env);

  scheme_add_global_constant("procedure-reduce-arity",
			     scheme_make_prim_w_arity(procedure_reduce_arity,
						      "procedure-reduce-arity",
						      2, 2),
			     env);
  scheme_add_global_constant("procedure-rename",
			     scheme_make_prim_w_arity(procedure_rename,
						      "procedure-rename",
						      2, 2),
			     env);
  scheme_add_global_constant("procedure->method",
			     scheme_make_prim_w_arity(procedure_to_method,
						      "procedure->method",
						      1, 1),
			     env);
  scheme_add_global_constant("procedure-closure-contents-eq?",
			     scheme_make_folding_prim(procedure_equal_closure_p,
						      "procedure-closure-contents-eq?",
						      2, 2, 1),
			     env);
  scheme_add_global_constant("chaperone-procedure",
			     scheme_make_prim_w_arity(chaperone_procedure,
						      "chaperone-procedure",
						      2, -1),
			     env);
  scheme_add_global_constant("impersonate-procedure",
			     scheme_make_prim_w_arity(impersonate_procedure,
						      "impersonate-procedure",
						      2, -1),
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
  scheme_add_global_constant("current-read-interaction",
			     scheme_register_parameter(current_read,
						       "current-read-interaction",
						       MZCONFIG_READ_HANDLER),
			     env);
  scheme_add_global_constant("current-get-interaction-input-port",
			     scheme_register_parameter(current_get_read_input_port,
						       "current-get-interaction-input-port",
						       MZCONFIG_READ_INPUT_PORT_HANDLER),
			     env);

  REGISTER_SO(certify_mode_symbol);
  REGISTER_SO(taint_mode_symbol);
  REGISTER_SO(transparent_symbol);
  REGISTER_SO(transparent_binding_symbol);
  REGISTER_SO(opaque_symbol);
  REGISTER_SO(none_symbol);
  certify_mode_symbol        = scheme_intern_symbol("certify-mode");
  taint_mode_symbol          = scheme_intern_symbol("taint-mode");
  transparent_symbol         = scheme_intern_symbol("transparent");
  transparent_binding_symbol = scheme_intern_symbol("transparent-binding");
  opaque_symbol              = scheme_intern_symbol("opaque");
  none_symbol                = scheme_intern_symbol("none");

  REGISTER_SO(is_method_symbol);
  REGISTER_SO(scheme_inferred_name_symbol);
  REGISTER_SO(cont_key);
  REGISTER_SO(barrier_prompt_key);
  REGISTER_SO(prompt_cc_guard_key);
  is_method_symbol = scheme_intern_symbol("method-arity-error");
  scheme_inferred_name_symbol = scheme_intern_symbol("inferred-name");
  cont_key = scheme_make_symbol("k"); /* uninterned */
  barrier_prompt_key = scheme_make_symbol("bar"); /* uninterned */
  prompt_cc_guard_key = scheme_make_symbol("cc"); /* uninterned */

  REGISTER_SO(scheme_default_prompt_tag);
  {
    Scheme_Object *a[1];
    a[0] = scheme_intern_symbol("default");
    scheme_default_prompt_tag = make_prompt_tag(1, a);
    (void)scheme_hash_key(SCHEME_PTR_VAL(scheme_default_prompt_tag));
  }

  REGISTER_SO(original_default_prompt);
  original_default_prompt = MALLOC_ONE_TAGGED(Scheme_Prompt);
  original_default_prompt->so.type = scheme_prompt_type;
  original_default_prompt->tag = scheme_default_prompt_tag;
}

void
scheme_init_fun_places()
{
  REGISTER_SO(offstack_cont);
  REGISTER_SO(offstack_overflow);
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
		+ ((count - mzFLEX_DELTA) * sizeof(Scheme_Object *)))
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
			    ? SCHEME_PRIM_OPT_FOLDING
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
     use its third argument (i.e., the closure pointer). */
  return make_prim_closure(fun, 1, name, mina, maxa,
			   SCHEME_PRIM_OPT_NONCM,
			   1, 1,
			   0, 0, NULL);
}

Scheme_Object *
scheme_make_immed_prim(Scheme_Prim *fun, const char *name,
		       mzshort mina, mzshort maxa)
{
  /* An immediate primitive is a non-cm primitive, and it doesn't
     extend the continuation in a way that interacts with space safety, except
     maybe to raise an exception. */
  return make_prim_closure(fun, 1, name, mina, maxa,
			   SCHEME_PRIM_OPT_IMMEDIATE,
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
			    ? SCHEME_PRIM_OPT_FOLDING
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
  prim->pp.flags = ((folding ? SCHEME_PRIM_OPT_FOLDING : 0)
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

int scheme_intern_prim_opt_flags(int flags)
{
  int i;

  if (!flags) return 0;

  for (i = 1; i < (1 << SCHEME_PRIM_OPT_INDEX_SIZE); i++) {
    if (scheme_prim_opt_flags[i] == flags)
      return (i << SCHEME_PRIM_OPT_INDEX_SHIFT);
    else if (!scheme_prim_opt_flags[i]) {
      scheme_prim_opt_flags[i] = flags;
      return (i << SCHEME_PRIM_OPT_INDEX_SHIFT);
    }
  }

  scheme_signal_error("too many flag combinations");

  return 0;
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

THREAD_LOCAL_DECL(Scheme_Overflow_Jmp *scheme_overflow_jmp);
THREAD_LOCAL_DECL(void *scheme_overflow_stack_start);

MZ_DO_NOT_INLINE(void scheme_really_create_overflow(void *stack_base));

void scheme_really_create_overflow(void *stack_base)
{
  Scheme_Overflow_Jmp *jmp;

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
       The way to continue is in p->overflow_k.
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
      intptr_t i1, i2, i3, i4;
      Overflow_K_Proc f = p->overflow_k;
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

      /* At the time of writing, there appear to be no GCs on the 
         longjmp return from stack overflow. Just in case, though,
         it seems better to protect multiple-value and tail-call 
         results from any GC that might be introduced one day. */
      if (reply == SCHEME_MULTIPLE_VALUES) {
        p = scheme_current_thread;
        if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
          p->values_buffer = NULL;
      } else if (reply == SCHEME_TAIL_CALL_WAITING) {
        p = scheme_current_thread;
        if (p->ku.apply.tail_rands == p->tail_buffer) {
          GC_CAN_IGNORE Scheme_Object **tb;
          p->tail_buffer = NULL; /* so args aren't zeroed */
          tb = MALLOC_N(Scheme_Object *, p->tail_buffer_size);
          p->tail_buffer = tb;
        }
      }
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
  void *stack_marker;
  scheme_really_create_overflow(PROMPT_STACK(stack_marker));
  stack_marker = NULL; /* to ensure that we get __gc_var_stack__ in 3m */
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

static Scheme_Prompt *allocate_prompt(Scheme_Prompt **cached_prompt) {
  Scheme_Prompt *prompt;
  if (*cached_prompt) {
    prompt = *cached_prompt;
    *cached_prompt = NULL;
  } else  {
    prompt = MALLOC_ONE_TAGGED(Scheme_Prompt);
    prompt->so.type = scheme_prompt_type;
  }
  return prompt;
}

static void save_dynamic_state(Scheme_Thread *thread, Scheme_Dynamic_State *state) {
    state->current_local_env = thread->current_local_env;
    state->mark              = thread->current_local_mark;
    state->name              = thread->current_local_name;
    state->modidx            = thread->current_local_modidx;
    state->menv              = thread->current_local_menv;
}

static void restore_dynamic_state(Scheme_Dynamic_State *state, Scheme_Thread *thread) {
    thread->current_local_env     = state->current_local_env;
    thread->current_local_mark    = state->mark;
    thread->current_local_name    = state->name;
    thread->current_local_modidx  = state->modidx;
    thread->current_local_menv    = state->menv;
}

void scheme_set_dynamic_state(Scheme_Dynamic_State *state, Scheme_Comp_Env *env, Scheme_Object *mark, 
                              Scheme_Object *name, 
                              Scheme_Env *menv,
                              Scheme_Object *modidx)
{
  state->current_local_env = env;
  state->mark              = mark;
  state->name              = name;
  state->modidx            = modidx;
  state->menv              = menv;
}

static void *apply_again_k()
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *val = p->ku.k.p1;
  int num_vals = p->ku.k.i1;

  p->ku.k.p1 = NULL;

  if (num_vals != 1) {
    scheme_wrong_return_arity("call-with-continuation-prompt", 1, num_vals, (Scheme_Object **)val,
                              "application of default prompt handler");
    return NULL;
  } else {
    scheme_check_proc_arity("default-continuation-prompt-handler", 0, 0, 1, &val);
    return (void *)_scheme_apply(val, 0, NULL);
  }
}

void *scheme_top_level_do(void *(*k)(void), int eb) {
    return scheme_top_level_do_worker(k, eb, 0, NULL);
}

void *scheme_top_level_do_worker(void *(*k)(void), int eb, int new_thread, Scheme_Dynamic_State *dyn_state)
{
  /* Wraps a function `k' with a handler for stack overflows and
     barriers to full-continuation jumps. No barrier if !eb. */
  void * v;
  Scheme_Prompt * volatile prompt = NULL;
  mz_jmp_buf *save;
  mz_jmp_buf newbuf;
  Scheme_Stack_State envss;
  Scheme_Dynamic_State save_dyn_state;
  Scheme_Thread * volatile p = scheme_current_thread;
  volatile int old_pcc = scheme_prompt_capture_count;
  Scheme_Cont_Frame_Data cframe;
  volatile int need_final_abort = 0;
#ifdef MZ_PRECISE_GC
  void *external_stack;
#endif

  if (scheme_active_but_sleeping)
    scheme_wake_up();

  if (eb) {
    prompt = allocate_prompt(&available_prompt);
    initialize_prompt(p, prompt, PROMPT_STACK(prompt));
      
    if (!new_thread) {
      prompt->is_barrier = 1;
    }
  }

#ifdef MZ_PRECISE_GC
  if (scheme_get_external_stack_val)
    external_stack = scheme_get_external_stack_val();
  else
    external_stack = NULL;
#endif

  scheme_save_env_stack_w_thread(envss, p);
  save_dynamic_state(p, &save_dyn_state);

  if (dyn_state) {
    restore_dynamic_state(dyn_state, p);
    dyn_state = NULL;
  }

  scheme_create_overflow(); /* needed even if scheme_overflow_jmp is already set */

  if (prompt) {
    scheme_push_continuation_frame(&cframe);
    scheme_set_cont_mark(barrier_prompt_key, (Scheme_Object *)prompt);
  }

  save = p->error_buf;
  p->error_buf = &newbuf;

  while (1) {

    if (scheme_setjmp(newbuf)) {
      p = scheme_current_thread;
      if (SAME_OBJ(p->cjs.jumping_to_continuation, (Scheme_Object *)original_default_prompt)) {
        /* an abort to the thread start; act like the default prompt handler,
           but remember to jump again */
        p->ku.k.i1 = p->cjs.num_vals;
        p->ku.k.p1 = p->cjs.val;
        reset_cjs(&p->cjs);
        k = apply_again_k;
        need_final_abort = 1;
      } else {
        if (!new_thread) {
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
          restore_dynamic_state(&save_dyn_state, p);
        }
        scheme_longjmp(*save, 1);
      }
    } else {
      if (new_thread) {
        /* check for initial break before we do anything */
        scheme_check_break_now();
      }
      
      v = k();

      break;
    }
  }

  /* IMPORTANT: no GCs from here to return, since v
     may refer to multiple values, and we don't want the
     multiple-value array cleared. */

  if (!new_thread) {
    p = scheme_current_thread;

    restore_dynamic_state(&save_dyn_state, p);

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

  if (need_final_abort) {
    p = scheme_current_thread;
    scheme_longjmp(*p->error_buf, 1);
  }

  return (Scheme_Object *)v;
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

Scheme_Object *
scheme_apply_thread_thunk(Scheme_Object *rator)
{
  Scheme_Thread *p = scheme_current_thread;

  p->ku.k.p1 = rator;
  p->ku.k.p2 = NULL;
  p->ku.k.i1 = 0;
  p->ku.k.i2 = 1;

  return (Scheme_Object *)scheme_top_level_do_worker(apply_k, 1, 1, NULL);
}

Scheme_Object *
scheme_apply_with_dynamic_state(Scheme_Object *rator, int num_rands, Scheme_Object **rands, Scheme_Dynamic_State *dyn_state)
{
  Scheme_Thread *p = scheme_current_thread;

  p->ku.k.p1 = rator;
  p->ku.k.p2 = rands;
  p->ku.k.i1 = num_rands;
  p->ku.k.i2 = 0;

  return (Scheme_Object *)scheme_top_level_do_worker(apply_k, 1, 0, dyn_state);
}

Scheme_Object *
scheme_apply_multi_with_dynamic_state(Scheme_Object *rator, int num_rands, Scheme_Object **rands, Scheme_Dynamic_State *dyn_state)
{
  Scheme_Thread *p = scheme_current_thread;

  p->ku.k.p1 = rator;
  p->ku.k.p2 = rands;
  p->ku.k.i1 = num_rands;
  p->ku.k.i2 = 1;

  return (Scheme_Object *)scheme_top_level_do_worker(apply_k, 1, 0, dyn_state);
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

#ifdef INSTRUMENT_PRIMITIVES
extern int g_print_prims;
#endif

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
      Scheme_Object **tb;
      tb = MALLOC_N(Scheme_Object *, num_rands);
      p->tail_buffer = tb;
      p->tail_buffer_size = num_rands;
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

static Scheme_Object *cert_with_specials_k(void);

static Scheme_Object *
cert_with_specials(Scheme_Object *code, 
                   Scheme_Object *insp,
                   Scheme_Object *old_stx,
                   intptr_t phase, 
		   int deflt, int cadr_deflt)
/* Arms (insp) or re-arms (old_stx) taints. */
{
  Scheme_Object *prop;
  int next_cadr_deflt = 0, phase_delta = 0;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      Scheme_Object **args;
      args = MALLOC_N(Scheme_Object*, 3);
      args[0] = code;
      args[1] = insp;
      args[2] = old_stx;
      p->ku.k.p1 = (void *)args;
      p->ku.k.i1 = phase;
      p->ku.k.i2 = deflt;
      p->ku.k.i3 = cadr_deflt;
      return scheme_handle_stack_overflow(cert_with_specials_k);
    }
  }
#endif

  if (SCHEME_STXP(code)) {
    if (scheme_stx_is_tainted(code))
       /* nothing happens to already-tainted syntax objects */
      return code;

    prop = scheme_stx_property(code, taint_mode_symbol, NULL);
    if (SCHEME_FALSEP(prop))
      prop = scheme_stx_property(code, certify_mode_symbol, NULL);
    if (SAME_OBJ(prop, none_symbol))
      return code;
    else if (SAME_OBJ(prop, opaque_symbol)) {
      if (old_stx)
        return scheme_stx_taint_rearm(code, old_stx);
      else
        return scheme_stx_taint_arm(code, insp);
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
      if (SCHEME_TRUEP(prop))
        scheme_log(NULL,
                   SCHEME_LOG_WARNING,
                   0,
                   "warning: unrecognized 'taint-mode property value: %V",
                   prop);
      if (SCHEME_STX_PAIRP(code)) {
	Scheme_Object *name;
	/* name = SCHEME_STX_CAR(code); */
        name = scheme_stx_taint_disarm(code, NULL);
        name = SCHEME_STX_CAR(name);
	if (SCHEME_STX_SYMBOLP(name)) {
	  if (scheme_stx_module_eq_x(scheme_begin_stx, name, phase)
              || scheme_stx_module_eq_x(scheme_module_stx, name, phase)
              || scheme_stx_module_eq_x(scheme_modulestar_stx, name, phase)
              || scheme_stx_module_eq_x(scheme_module_begin_stx, name, phase)) {
	    trans = 1;
	    next_cadr_deflt = 0;
	  } else if (scheme_stx_module_eq_x(scheme_begin_for_syntax_stx, name, phase)) {
	    trans = 1;
	    next_cadr_deflt = 0;
            phase_delta = 1;
	  } else if (scheme_stx_module_eq_x(scheme_define_values_stx, name, phase)
		     || scheme_stx_module_eq_x(scheme_define_syntaxes_stx, name, phase)) {
	    trans = 1;
	    next_cadr_deflt = 1;
	  }
	}
      }

      if (!trans) {
        if (old_stx)
          return scheme_stx_taint_rearm(code, old_stx);
        else
          return scheme_stx_taint_arm(code, insp);
      }
    }
  }

  if (SCHEME_STX_PAIRP(code)) {
    Scheme_Object *a, *d, *v;
    
    a = SCHEME_STX_CAR(code);
    a = cert_with_specials(a, insp, old_stx, phase + phase_delta, cadr_deflt, 0);
    d = SCHEME_STX_CDR(code);
    d = cert_with_specials(d, insp, old_stx, phase + phase_delta, 1, next_cadr_deflt);

    v = scheme_make_pair(a, d);

    if (SCHEME_PAIRP(code))
      return v;

    return scheme_datum_to_syntax(v, code, code, 0, 1);
  } else if (SCHEME_STX_NULLP(code))
    return code;

  if (old_stx)
    return scheme_stx_taint_rearm(code, old_stx);
  else
    return scheme_stx_taint_arm(code, insp);
}

static Scheme_Object *cert_with_specials_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object **args = (Scheme_Object **)p->ku.k.p1;

  p->ku.k.p1 = NULL;

  return cert_with_specials(args[0], args[1], args[2],
                            p->ku.k.i1,
                            p->ku.k.i2, p->ku.k.i3);
}

Scheme_Object *
scheme_apply_macro(Scheme_Object *name, Scheme_Env *menv,
		   Scheme_Object *rator, Scheme_Object *code,
		   Scheme_Comp_Env *env, Scheme_Object *boundname,
                   Scheme_Compile_Expand_Info *rec, int drec,
		   int for_set)
{
  Scheme_Object *orig_code = code;

  if (scheme_is_rename_transformer(rator)) {
    Scheme_Object *mark;
   
    rator = scheme_rename_transformer_id(rator);
    /* rator is now an identifier */

    /* and it's introduced by this expression: */
    mark = scheme_new_mark();
    rator = scheme_add_remove_mark(rator, mark);

    if (for_set) {
      Scheme_Object *tail, *setkw;

      tail = SCHEME_STX_CDR(code);
      setkw = SCHEME_STX_CAR(code);
      tail = SCHEME_STX_CDR(tail);
      code = scheme_make_pair(setkw, scheme_make_pair(rator, tail));
      code = scheme_datum_to_syntax(code, orig_code, orig_code, 0, 0);
    } else if (SCHEME_SYMBOLP(SCHEME_STX_VAL(code)))
      code = rator;
    else {
      code = SCHEME_STX_CDR(code);
      code = scheme_make_pair(rator, code);
      code = scheme_datum_to_syntax(code, orig_code, scheme_sys_wraps(env), 0, 0);
    }

    code = scheme_stx_track(code, orig_code, name);

    /* Restore old dye packs: */
    code = cert_with_specials(code, NULL, orig_code, env->genv->phase, 0, 0);

    return code;
  } else {
    Scheme_Object *mark, *rands_vec[1], *track_code, *pre_code;

    if (scheme_is_set_transformer(rator))
      rator = scheme_set_transformer_proc(rator);

    {
      /* Ensure that source doesn't already have 'taint-mode or 'certify-mode, 
         in case argument properties are used for result properties. */
      Scheme_Object *prop;
      prop = scheme_stx_property(code, taint_mode_symbol, NULL);
      if (SCHEME_TRUEP(prop))
        code = scheme_stx_property(code, taint_mode_symbol, scheme_false);
      prop = scheme_stx_property(code, certify_mode_symbol, NULL);
      if (SCHEME_TRUEP(prop))
        code = scheme_stx_property(code, certify_mode_symbol, scheme_false);
    }
    track_code = code;  /* after mode properties are removed */

    mark = scheme_new_mark();
    code = scheme_add_remove_mark(code, mark);

    code = scheme_stx_taint_disarm(code, NULL);

    pre_code = code;
    SCHEME_EXPAND_OBSERVE_MACRO_PRE_X(rec[drec].observer, code);

    {
      Scheme_Dynamic_State dyn_state;
      Scheme_Cont_Frame_Data cframe;
      Scheme_Config *config;

      scheme_prepare_exp_env(env->genv);
      config = scheme_extend_config(scheme_current_config(),
                                    MZCONFIG_ENV,
                                    (Scheme_Object *)env->genv->exp_env);
      scheme_push_continuation_frame(&cframe);
      scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);

      scheme_set_dynamic_state(&dyn_state, env, mark, boundname, 
                               menv, menv ? menv->link_midx : env->genv->link_midx);

      rands_vec[0] = code;
      code = scheme_apply_with_dynamic_state(rator, 1, rands_vec, &dyn_state);

      scheme_pop_continuation_frame(&cframe);
    }

    SCHEME_EXPAND_OBSERVE_MACRO_POST_X(rec[drec].observer, code, pre_code);

    if (!SCHEME_STXP(code)) {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                       "%S: received value from syntax expander was not syntax\n"
                       "  received: %V",
                       SCHEME_STX_SYM(name),
                       code);
    }

    code = scheme_add_remove_mark(code, mark);

    code = scheme_stx_track(code, track_code, name);
    
    /* Restore old dye packs: */
    code = cert_with_specials(code, NULL, orig_code, env->genv->phase, 0, 0);

    return code;
  }
}

Scheme_Object *scheme_syntax_taint_arm(Scheme_Object *stx, Scheme_Object *insp, int use_mode)
{
  intptr_t phase;

  if (SCHEME_FALSEP(insp)) {
    insp = scheme_get_local_inspector();
  }

  if (use_mode) {
    Scheme_Thread *p = scheme_current_thread;
    phase = (p->current_local_env
             ? p->current_local_env->genv->phase
             : p->current_phase_shift);
    return cert_with_specials(stx, insp, NULL, phase, 0, 0);
  } else
    return scheme_stx_taint_arm(stx, insp);
}

Scheme_Object *scheme_syntax_taint_disarm(Scheme_Object *o, Scheme_Object *insp)
{
  if (SCHEME_FALSEP(insp)) {
    insp = scheme_get_local_inspector();
  }

  return scheme_stx_taint_disarm(o, insp);
}

Scheme_Object *scheme_syntax_taint_rearm(Scheme_Object *stx, Scheme_Object *from_stx)
{
  Scheme_Thread *p = scheme_current_thread;
  intptr_t phase;
  
  phase = (p->current_local_env
           ? p->current_local_env->genv->phase
           : p->current_phase_shift);
  
  return cert_with_specials(stx, NULL, from_stx, phase, 0, 0);
}

/*========================================================================*/
/*                                   arity                                */
/*========================================================================*/

static Scheme_Object *make_arity(mzshort mina, mzshort maxa, int mode)
{
  if (mina == maxa)
    return scheme_make_integer(mina);
  else if (maxa == -1) {
    if (mode == -3) {
      return scheme_make_integer(-(mina+1));
    } else {
      Scheme_Object *p[1];
      p[0] = scheme_make_integer(mina);
      return scheme_make_struct_instance(scheme_arity_at_least, 1, p);
    }
  } else {
    int i;
    Scheme_Object *l = scheme_null;

    for (i = maxa; i >= mina; --i) {
      l = scheme_make_pair(scheme_make_integer(i), l);
    }

    return l;
  }
}

Scheme_Object *scheme_make_arity(mzshort mina, mzshort maxa)
{
  return make_arity(mina, maxa, -1);
}

static Scheme_Object *clone_arity(Scheme_Object *a, int delta, int mode)
{
  if (SCHEME_PAIRP(a)) {
    Scheme_Object *m, *l;
    m = scheme_copy_list(a);
    for (l = m; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
      a = clone_arity(SCHEME_CAR(l), delta, mode);
      SCHEME_CAR(l) = a;
    }
    return m;
  } else if (SCHEME_CHAPERONE_STRUCTP(a)) {
    Scheme_Object *p[1];
    a = scheme_struct_ref(a, 0);
    if (delta)
      a = scheme_bin_minus(a, scheme_make_integer(delta));
    if (mode == -3) {
      return scheme_make_integer(-(SCHEME_INT_VAL(a)+1));
    } else {
      p[0] = a;
      return scheme_make_struct_instance(scheme_arity_at_least, 1, p);
    }
  } else if (SCHEME_NULLP(a))
    return a;
  else if (delta)
    return scheme_bin_minus(a, scheme_make_integer(delta));
  else
    return a;
}

static Scheme_Object *get_or_check_arity(Scheme_Object *p, intptr_t a, Scheme_Object *bign, int inc_ok)
/* a == -1 => get arity
   a == -2 => check for allowing bignum
   a == -3 => like -1, but alternate representation using negative numbers for arity-at-least  */
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
  } else if ((type == scheme_case_closure_type)
             || (type == scheme_case_lambda_sequence_type)) {
    Scheme_Case_Lambda *seq;
    Scheme_Closure_Data *data;
    int i;
    Scheme_Object *first, *last = NULL, *v;

    if ((a == -1) || (a == -3))
      first = scheme_null;
    else
      first = scheme_false;

    seq = (Scheme_Case_Lambda *)p;
    for (i = 0; i < seq->count; i++) {
      v = seq->array[i];
      if (SAME_TYPE(SCHEME_TYPE(v), scheme_unclosed_procedure_type))
        data = (Scheme_Closure_Data *)v;
      else
        data = SCHEME_COMPILED_CLOS_CODE(v);
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

	  v = scheme_make_pair(make_arity(mina, maxa, a), scheme_null);
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
    if (!inc_ok
        && scheme_no_arity_property
        && scheme_struct_type_property_ref(scheme_no_arity_property, p))
      return scheme_false;
    if (scheme_reduced_procedure_struct
        && scheme_is_struct_instance(scheme_reduced_procedure_struct, p)) {
      if (a >= 0) {
        bign = scheme_make_integer(a);
        if (drop)
          bign = scheme_bin_plus(bign, scheme_make_integer(drop));
      }
      if ((a == -1) || (a == -3))
        return clone_arity(((Scheme_Structure *)p)->slots[1], drop, a);
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
        } else if (SCHEME_NULLP(v)) {
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
        if ((a == -1) || (a == -3))
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

      pa = scheme_get_native_arity(p, a);

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
	    Scheme_Object *first = scheme_null, *last = NULL, *ae;
	    int v;
	    while (SCHEME_PAIRP(pa)) {
	      ae = SCHEME_CAR(pa);
	      if (SCHEME_INTP(ae)) {
		v = SCHEME_INT_VAL(ae);
		if (v < drop)
		  ae = NULL;
		else {
		  v -= drop;
		  ae = scheme_make_integer(v);
		}
	      } else {
		/* arity-at-least */
		ae = ((Scheme_Structure *)ae)->slots[0];
		v = SCHEME_INT_VAL(ae);
		if (v >= drop) {
		  ae = make_arity(v - drop, -1, a);
		} else {
		  ae = make_arity(0, -1, a);
		}
	      }
	      if (ae) {
		ae = scheme_make_pair(ae, scheme_null);
		if (last)
		  SCHEME_CDR(last) = ae;
		else
		  first = ae;
		last = ae;
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
  } else if (type == scheme_proc_chaperone_type) {
    p = SCHEME_CHAPERONE_VAL(p);
    SCHEME_USE_FUEL(1);
    goto top;
  } else {
    Scheme_Closure_Data *data;

    if (type == scheme_unclosed_procedure_type) 
      data = (Scheme_Closure_Data *)p;
    else
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

    if ((a == -1) || (a == -3)) {
      Scheme_Object *arity, *ae, *last = NULL;

      arity = scheme_alloc_list(count);

      for (i = 0, ae = arity; i < count; i++) {
	Scheme_Object *av;
	int mn, mx;
	mn = cases[2 * i];
	mx = cases[(2 * i) + 1];

	if (mn >= drop) {
	  mn -= drop;
	  if (mx > 0)
	    mx -= drop;

	  av = make_arity(mn, mx, a);

	  SCHEME_CAR(ae) = av;
	  last = ae;
	  ae = SCHEME_CDR(ae);
	}
      }

      /* If drop > 0, might have found no matches */
      if (!SCHEME_NULLP(ae)) {
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

  if ((a == -1) || (a == -3)) {
    if (mina < drop)
      return scheme_null;
    else
      mina -= drop;
    if (maxa > 0)
      maxa -= drop;

    return make_arity(mina, maxa, a);
  }

  if (a == -2)
    return (maxa < 0) ? scheme_true : scheme_false;

  a += drop;

  if (a < mina || (maxa >= 0 && a > maxa))
    return scheme_false;

  return scheme_true;
}

Scheme_Object *scheme_get_or_check_arity(Scheme_Object *p, intptr_t a)
{
  return get_or_check_arity(p, a, NULL, 1);
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

  if (!SCHEME_PROCP(p) || SCHEME_FALSEP(get_or_check_arity(p, a, NULL, 1))) {
    if (where) {
      char buffer[60];
      const char *pre, *post;

      if (false_ok) {
        pre = "(or/c ";
        post = " #f)";
      } else
        pre = post = "";

      switch (a) {
      case 0:
        sprintf(buffer, "%s(-> any)%s", pre, post);
        break;
      case 1:
        sprintf(buffer, "%s(any/c . -> . any)%s", pre, post);
        break;
      case 2:
        sprintf(buffer, "%s(any/c any/c . -> . any)%s", pre, post);
        break;
      case 3:
        sprintf(buffer, "%s(any/c any/c any/c . -> . any)%s", pre, post);
        break;
      default:
        sprintf(buffer, "%s(procedure-arity-includes/c %d)%s", 
                pre, a, post);
        break;
      }

      scheme_wrong_contract(where, buffer, which, argc, argv);
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

int scheme_closure_preserves_marks(Scheme_Object *p)
{
  Scheme_Type type = SCHEME_TYPE(p);
  Scheme_Closure_Data *data;

#ifdef MZ_USE_JIT
  if (type == scheme_native_closure_type)
    return scheme_native_closure_preserves_marks(p);
#endif

  if (type == scheme_closure_type) {
    data = SCHEME_COMPILED_CLOS_CODE(p);
  } else if (type == scheme_unclosed_procedure_type) {
    data = (Scheme_Closure_Data *)p;
  } else
    return 0;

  if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_PRESERVES_MARKS)
    return 1;

  return 0;
}

Scheme_Object *scheme_get_or_check_procedure_shape(Scheme_Object *e, Scheme_Object *expected)
/* result is interned --- a symbol or fixnum */
{
  Scheme_Object *p;

  if (expected 
      && SCHEME_SYMBOLP(expected) 
      && SCHEME_SYM_VAL(expected)[0] == 's') {
    return (scheme_check_structure_shape(e, expected)
            ? expected
            : NULL);
  }

  if (SAME_TYPE(SCHEME_TYPE(e), scheme_inline_variant_type))
    e = SCHEME_VEC_ELS(e)[1];

  p = scheme_get_or_check_arity(e, -3);

  if (SCHEME_PAIRP(p)) {
    /* encode as a symbol */
    int sz = 32, c = 0;
    char *b, *naya;
    b = (char *)scheme_malloc_atomic(sz);

    while (SCHEME_PAIRP(p)) {
      if (sz - c < 10) {
        sz *= 2;
        naya = (char *)scheme_malloc_atomic(sz);
        memcpy(naya, b, c);
        b = naya;
      }
      if (c)
        b[c++] = ':';
      c += sprintf(b XFORM_OK_PLUS c, "%" PRIdPTR, SCHEME_INT_VAL(SCHEME_CAR(p)));
      
      p = SCHEME_CDR(p);
    }
    b[c] = c;
    p = scheme_intern_exact_symbol(b, c);
  } else {
    /* Integer encoding, but shift to use low bit to indicate whether
       it preserves marks, which is useful information for the JIT. */
    intptr_t i = SCHEME_INT_VAL(p);
    i <<= 1;
    if (scheme_closure_preserves_marks(e)) {
      i |= 0x1;
    }
    p = scheme_make_integer(i);
  }

  if (expected && !SAME_OBJ(expected, p))
    return NULL;

  return p;
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

  while (SCHEME_CHAPERONE_PROC_STRUCTP(a)) {
    if (SCHEME_CHAPERONEP(a))
      a = SCHEME_CHAPERONE_VAL(a);
    if (scheme_reduced_procedure_struct
        && scheme_is_struct_instance(scheme_reduced_procedure_struct, a)
        && SCHEME_TRUEP(((Scheme_Structure *)a)->slots[2])) {
      return a;
    } else {
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
      if (scheme_reduced_procedure_struct
          && scheme_is_struct_instance(scheme_reduced_procedure_struct, p)) {
        /* It must have a name: */
        Scheme_Object *sym = ((Scheme_Structure *)p)->slots[2];
        if (for_error < 0) {
          s = (char *)sym;
          *len = -1;
        } else {
          *len = SCHEME_SYM_LEN(sym);
          s = scheme_symbol_val(sym);
        }
      } else {
        Scheme_Object *sym;
        intptr_t offset;
        sym = SCHEME_STRUCT_NAME_SYM(p);
        *len = SCHEME_SYM_LEN(sym);
        s = (char *)scheme_malloc_atomic((*len) + 8);
        if (0) {
          memcpy(s, "struct ", 7);
          offset = 7;
        } else
          offset = 0;
        memcpy(s + offset, scheme_symbol_val(sym), *len);
        (*len) += offset;
        s[*len] = 0;
        return s;
      }
    } else {
      p = other;
      goto top;
    }
  } else if (type == scheme_proc_chaperone_type) {
    p = SCHEME_CHAPERONE_VAL(p);
    SCHEME_USE_FUEL(1);
    goto top;
  } else {
    Scheme_Object *name;

    if (type == scheme_compiled_unclosed_procedure_type) {
      name = ((Scheme_Closure_Data *)p)->name;
    } else if (type == scheme_closure_type) {
      name = SCHEME_COMPILED_CLOS_CODE(p)->name;
    } else if (type == scheme_case_lambda_sequence_type) {
      Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)p;
      if (!cl->count)
        name = NULL;
      else
        name = ((Scheme_Closure_Data *)cl->array[0])->name;
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

  if (0) {
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
    scheme_wrong_contract("primitive-result_arity", "primitive?", 0, argc, argv);
    return NULL;
  }

  return scheme_make_integer(1);
}

Scheme_Object *scheme_object_name(Scheme_Object *a)
{
  if (SCHEME_CHAPERONEP(a))
    a = SCHEME_CHAPERONE_VAL(a);

  if (SCHEME_PROC_STRUCTP(a)) {
    a = scheme_proc_struct_name_source(a);
    if (SCHEME_CHAPERONEP(a))
      a = SCHEME_CHAPERONE_VAL(a);
    
    if (SCHEME_STRUCTP(a)
        && scheme_reduced_procedure_struct
        && scheme_is_struct_instance(scheme_reduced_procedure_struct, a)) {
      /* It must have a name: */
      return ((Scheme_Structure *)a)->slots[2];
    }
  }

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
  } else if (SAME_TYPE(SCHEME_TYPE(a), scheme_logger_type)) {
    Scheme_Logger *logger = (Scheme_Logger *)a;
    if (logger->name)
      return logger->name;
  }

  return scheme_false;
}

static Scheme_Object *object_name(int argc, Scheme_Object **argv)
{
  return scheme_object_name(argv[0]);
}

Scheme_Object *scheme_arity(Scheme_Object *p)
{
  return get_or_check_arity(p, -1, NULL, 1);
}

static Scheme_Object *procedure_arity(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_PROCP(argv[0]))
    scheme_wrong_contract("procedure-arity", "procedure?", 0, argc, argv);

  return get_or_check_arity(argv[0], -1, NULL, 1);
}

static Scheme_Object *procedure_arity_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *a = argv[0], *v;

  if (SCHEME_INTP(a)) {
    return ((SCHEME_INT_VAL(a) >= 0) ? scheme_true : scheme_false);
  } else if (SCHEME_BIGNUMP(a)) {
    return (SCHEME_BIGPOS(a) ? scheme_true : scheme_false);
  } else if (SCHEME_NULLP(a)) {
    return scheme_true;
  } else if (SCHEME_PAIRP(a)) {
    while (SCHEME_PAIRP(a)) {
      v = SCHEME_CAR(a);
      if (SCHEME_INTP(v)) {
        if (SCHEME_INT_VAL(v) < 0)
          return scheme_false;
      } else if (SCHEME_BIGNUMP(v)) {
        if (!SCHEME_BIGPOS(v))
          return scheme_false;
      } else if (!SCHEME_CHAPERONE_STRUCTP(v)
                 || !scheme_is_struct_instance(scheme_arity_at_least, v)) {
        return scheme_false;
      }
      a = SCHEME_CDR(a);
    }
    return SCHEME_NULLP(a) ? scheme_true : scheme_false;
  } else if (SCHEME_CHAPERONE_STRUCTP(a)
             && scheme_is_struct_instance(scheme_arity_at_least, a)) {
    return scheme_true;
  } else
    return scheme_false;
}

Scheme_Object *scheme_procedure_arity_includes(int argc, Scheme_Object *argv[])
{
  intptr_t n;
  int inc_ok;

  if (!SCHEME_PROCP(argv[0]))
    scheme_wrong_contract("procedure-arity-includes?", "procedure?", 0, argc, argv);

  n = scheme_extract_index("procedure-arity-includes?", 1, argc, argv, -2, 0);
  /* -2 means a bignum */

  inc_ok = ((argc > 2) && SCHEME_TRUEP(argv[2]));
  
  return get_or_check_arity(argv[0], n, argv[1], inc_ok);
}

static int is_arity(Scheme_Object *a, int at_least_ok, int list_ok)
{
  if (SCHEME_INTP(a)) {
    return (SCHEME_INT_VAL(a) >= 0);
  } else if (SCHEME_BIGNUMP(a)) {
    return SCHEME_BIGPOS(a);
  } else if (at_least_ok
             && SCHEME_CHAPERONE_STRUCTP(a)
             && scheme_is_struct_instance(scheme_arity_at_least, a)) {
    a = scheme_struct_ref(a, 0);
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

void scheme_init_reduced_proc_struct(Scheme_Env *env)
{
  if (!scheme_reduced_procedure_struct) {
    Scheme_Inspector *insp;

    REGISTER_SO(scheme_reduced_procedure_struct);
    insp = (Scheme_Inspector *) scheme_get_current_inspector();
    while (insp->superior->superior) {
      insp = insp->superior;
    }
    scheme_reduced_procedure_struct = scheme_make_struct_type2(NULL,
                                                               NULL,
                                                               (Scheme_Object *)insp,
                                                               4, 0,
                                                               scheme_false,
                                                               scheme_null,
                                                               scheme_make_integer(0),
                                                               NULL, NULL);
  }
}

static Scheme_Object *make_reduced_proc(Scheme_Object *proc, Scheme_Object *aty, Scheme_Object *name, Scheme_Object *is_meth)
{
  Scheme_Object *a[4];
  
  if (SCHEME_STRUCTP(proc)
      && scheme_is_struct_instance(scheme_reduced_procedure_struct, proc)) {
    /* Don't need the intermediate layer */
    if (!name)
      name = ((Scheme_Structure *)proc)->slots[2];
    if (!is_meth)
      is_meth = ((Scheme_Structure *)proc)->slots[3];
    proc = ((Scheme_Structure *)proc)->slots[0];
  }

  a[0] = proc;
  a[1] = aty;
  a[2] = (name ? name : scheme_false);
  a[3] = (is_meth ? is_meth : scheme_false);

  return scheme_make_struct_instance(scheme_reduced_procedure_struct, 4, a);
}

static int is_subarity(Scheme_Object *req, Scheme_Object *orig)
{
  Scheme_Object *oa, *ra, *ol, *lra, *ara, *prev, *pr, *tmp;

  if (!SCHEME_PAIRP(orig) && !SCHEME_NULLP(orig))
    orig = scheme_make_pair(orig, scheme_null);
  if (!SCHEME_PAIRP(req) && !SCHEME_NULLP(req))
    req = scheme_make_pair(req, scheme_null);

  while (!SCHEME_NULLP(req)) {
    ra = SCHEME_CAR(req);
    if (SCHEME_CHAPERONE_STRUCTP(ra)
        && scheme_is_struct_instance(scheme_arity_at_least, ra)) {
      /* Convert to a sequence of range pairs, where the
         last one can be (min, #f); we'll iterate through the 
         original arity to knock out ranges until (if it matches)
         we end up with an empty list of ranges. */
      ra = scheme_make_pair(scheme_make_pair(scheme_struct_ref(ra, 0),
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
      return 0;
    }

    req = SCHEME_CDR(req);
  }

  return 1;
}

static Scheme_Object *procedure_reduce_arity(int argc, Scheme_Object *argv[])
{
  Scheme_Object *orig, *aty;

  if (!SCHEME_PROCP(argv[0]))
    scheme_wrong_contract("procedure-reduce-arity", "procedure?", 0, argc, argv);

  if (!is_arity(argv[1], 1, 1)) {
    scheme_wrong_contract("procedure-reduce-arity", 
                          "(or/c exact-nonnegative-integer? arity-at-least? (listof (or/c exact-nonnegative-integer? arity-at-least?)))", 
                          1, argc, argv);
  }

  /* Check whether current arity covers the requested arity.  This is
     a bit complicated, because both the source and target can be
     lists that include arity-at-least records. */

  orig = get_or_check_arity(argv[0], -1, NULL, 1);
  aty = clone_arity(argv[1], 0, -1);

  if (!is_subarity(aty, orig)) {
    scheme_contract_error("procedure-reduce-arity",
                          "arity of procedure does not include requested arity",
                          "procedure", 1, argv[0],
                          "requested arity", 1, argv[1],
                          NULL);
    return NULL;
  }

  /* Construct a procedure that has the given arity. */
  return make_reduced_proc(argv[0], aty, NULL, NULL);
}

static Scheme_Object *procedure_rename(int argc, Scheme_Object *argv[])
{
  Scheme_Object *p, *aty;

  if (!SCHEME_PROCP(argv[0]))
    scheme_wrong_contract("procedure-rename", "procedure?", 0, argc, argv);
  if (!SCHEME_SYMBOLP(argv[1]))
    scheme_wrong_contract("procedure-rename", "symbol?", 1, argc, argv);

  p = scheme_rename_struct_proc(argv[0], argv[1]);
  if (p) return p;

  aty = get_or_check_arity(argv[0], -1, NULL, 1);  

  return make_reduced_proc(argv[0], aty, argv[1], NULL);
}

static Scheme_Object *procedure_to_method(int argc, Scheme_Object *argv[])
{
  Scheme_Object *aty;

  if (!SCHEME_PROCP(argv[0]))
    scheme_wrong_contract("procedure->method", "procedure?", 0, argc, argv);

  aty = get_or_check_arity(argv[0], -1, NULL, 1);  

  return make_reduced_proc(argv[0], aty, NULL, scheme_true);
}

static Scheme_Object *procedure_equal_closure_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v1 = argv[0], *v2 = argv[1];

  if (!SCHEME_PROCP(v1))
    scheme_wrong_contract("procedure-closure-contents-eq?", "procedure?", 0, argc, argv);
  if (!SCHEME_PROCP(v2))
    scheme_wrong_contract("procedure-closure-contents-eq?", "procedure?", 1, argc, argv);

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

static Scheme_Object *do_chaperone_procedure(const char *name, const char *whating,
                                             int is_impersonator, int argc, Scheme_Object *argv[])
{
  Scheme_Chaperone *px;
  Scheme_Object *val = argv[0], *orig, *naya, *r;
  Scheme_Hash_Tree *props;

  if (SCHEME_CHAPERONEP(val))
    val = SCHEME_CHAPERONE_VAL(val);

  if (!SCHEME_PROCP(val))
    scheme_wrong_contract(name, "procedure?", 0, argc, argv);
  if (!SCHEME_PROCP(argv[1]))
    scheme_wrong_contract(name, "procedure?", 1, argc, argv);

  orig = get_or_check_arity(val, -1, NULL, 1);
  naya = get_or_check_arity(argv[1], -1, NULL, 1);

  if (!is_subarity(orig, naya))
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                     "%s: arity of wrapper procedure does not cover arity of original procedure\n"
                     "  wrapper: %V\n"
                     "  original: %V",
                     name,
                     argv[1],
                     argv[0]);

  props = scheme_parse_chaperone_props(name, 2, argc, argv);

  px = MALLOC_ONE_TAGGED(Scheme_Chaperone);
  px->iso.so.type = scheme_proc_chaperone_type;
  px->val = val;
  px->prev = argv[0];
  px->props = props;
  /* put procedure with known-good arity (to speed checking) in a mutable pair: */
  r = scheme_make_mutable_pair(argv[1], scheme_make_integer(-1));
  px->redirects = r;

  if (is_impersonator)
    SCHEME_CHAPERONE_FLAGS(px) |= SCHEME_CHAPERONE_IS_IMPERSONATOR;

  return (Scheme_Object *)px;
}

static Scheme_Object *chaperone_procedure(int argc, Scheme_Object *argv[])
{
  return do_chaperone_procedure("chaperone-procedure", "chaperoning", 0, argc, argv);
}

static Scheme_Object *impersonate_procedure(int argc, Scheme_Object *argv[])
{
  return do_chaperone_procedure("impersonate-procedure", "impersonating", 1, argc, argv);
}

static Scheme_Object *apply_chaperone_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Object **argv = (Scheme_Object **)p->ku.k.p2;
  Scheme_Object *auto_val = (Scheme_Object *)p->ku.k.p3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  return scheme_apply_chaperone(o, p->ku.k.i1, argv, auto_val, p->ku.k.i2);
}

static Scheme_Object *do_apply_chaperone(Scheme_Object *o, int argc, Scheme_Object **argv, Scheme_Object *auto_val, int checks)
{
#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      Scheme_Object **argv2;
      argv2 = MALLOC_N(Scheme_Object*, argc);
      memcpy(argv2, argv, sizeof(Scheme_Object *) * argc);
      p->ku.k.p1 = (void *)o;
      p->ku.k.p2 = (void *)argv2;
      p->ku.k.p3 = (void *)auto_val;
      p->ku.k.i1 = argc;
      p->ku.k.i2 = checks;
      return scheme_handle_stack_overflow(apply_chaperone_k);
    }
  }
#endif

  return scheme_apply_chaperone(o, argc, argv, auto_val, checks);
}


static Scheme_Object *_apply_native(Scheme_Object *obj, int num_rands, Scheme_Object **rands)
{
  Scheme_Native_Closure_Data *data;
  GC_MAYBE_IGNORE_INTERIOR MZ_MARK_STACK_TYPE old_cont_mark_stack;
  GC_MAYBE_IGNORE_INTERIOR Scheme_Object **rs;

  data = ((Scheme_Native_Closure *)obj)->code;

  if ((uintptr_t)data->max_let_depth > ((uintptr_t)scheme_current_runstack - (uintptr_t)scheme_current_runstack_start)) {
    return _scheme_apply_multi(obj, num_rands, rands);
  }

  MZ_CONT_MARK_POS += 2;
  old_cont_mark_stack = MZ_CONT_MARK_STACK;
  rs = MZ_RUNSTACK;

  obj = data->start_code(obj, num_rands, rands EXTRA_NATIVE_ARGUMENT);

  if (obj == SCHEME_TAIL_CALL_WAITING)
    obj = force_values(obj, 1);

  MZ_CONT_MARK_STACK = old_cont_mark_stack;
  MZ_CONT_MARK_POS -= 2;
  MZ_RUNSTACK = rs;

  return obj;
}

Scheme_Object *_scheme_apply_native(Scheme_Object *obj, int num_rands, Scheme_Object **rands)
{
  return _apply_native(obj, num_rands, rands);
}

/* must be at least 3: */
#define MAX_QUICK_CHAP_ARGV 5

Scheme_Object *scheme_apply_chaperone(Scheme_Object *o, int argc, Scheme_Object **argv, Scheme_Object *auto_val, int checks)
/* checks & 0x2 => no tail; checks == 0x3 => no tail or multiple;  */
{
  const char *what;
  Scheme_Chaperone *px;
  Scheme_Object *v, *a[1], *a2[MAX_QUICK_CHAP_ARGV], **argv2, *post, *result_v, *orig_obj, *app_mark;
  int c, i, need_restore = 0;
  int need_pop_mark;
  Scheme_Cont_Frame_Data cframe;

  if (argv == MZ_RUNSTACK) {
    /* Pushing onto the runstack ensures that `(mcar px->redirects)' won't
       modify argv. */
    if (MZ_RUNSTACK > MZ_RUNSTACK_START) {
      --MZ_RUNSTACK;
      *MZ_RUNSTACK = NULL;
      need_restore = 1;
    } else {
      /* Can't push! Just allocate a copy. */
      argv2 = MALLOC_N(Scheme_Object *, argc);
      memcpy(argv2, argv, sizeof(Scheme_Object*) * argc);
      argv = argv2;
    }
  }

  if (SCHEME_RPAIRP(o)) {
    /* An applicable struct, where a layer of struct chaperones
       has been removed from the object to apply, but we will
       eventually need to extract the procedure from the original
       object. */
    orig_obj = SCHEME_CDR(o);
    o = SCHEME_CAR(o);
  } else {
    orig_obj = NULL;
  }
  px = (Scheme_Chaperone *)o;

  if (!(SCHEME_CHAPERONE_FLAGS(px) & SCHEME_CHAPERONE_IS_IMPERSONATOR))
    what = "chaperone";
  else
    what = "impersonator";

  /* Ensure that the original procedure accepts `argc' arguments: */
  if (argc != SCHEME_INT_VAL(SCHEME_CDR(px->redirects))) {
    a[0] = px->prev;
    if (!scheme_check_proc_arity(NULL, argc, 0, 0, a)) {
      /* Apply the original procedure, in case the chaperone would accept
         `argc' arguments (in addition to the original procedure's arity)
         in case the methodness of the original procedure is different
         from the chaperone, or in case the procedures have different names. */
      (void)_scheme_apply_multi(px->prev, argc, argv);
      scheme_signal_error("internal error: unexpected success applying chaperoned/proxied procedure");
      return NULL;
    }
    /* record that argc is ok, on the grounds that the function is likely
       to be applied to argc arguments again */
    SCHEME_CDR(px->redirects) = scheme_make_integer(argc);
  }

  if (px->props) {
    app_mark = scheme_hash_tree_get(px->props, scheme_app_mark_impersonator_property);
    /* app_mark should be (cons mark val) */
    if (app_mark && !SCHEME_PAIRP(app_mark))
      app_mark = NULL;
  } else
    app_mark = NULL;

  if (app_mark) {
    v = scheme_extract_one_cc_mark(NULL, SCHEME_CAR(app_mark));
    if (v) {
      scheme_push_continuation_frame(&cframe);
      scheme_set_cont_mark(SCHEME_CAR(app_mark), v);
      MZ_CONT_MARK_POS -= 2;
      need_pop_mark = 1;
    } else
      need_pop_mark = 0;
  } else
    need_pop_mark = 0;

  v = SCHEME_CAR(px->redirects);
  if (SAME_TYPE(SCHEME_TYPE(v), scheme_native_closure_type))
    v = _apply_native(v, argc, argv);
  else
    v = _scheme_apply_multi(v, argc, argv);
  if (v == SCHEME_MULTIPLE_VALUES) {
    GC_CAN_IGNORE Scheme_Thread *p = scheme_current_thread;
    c = p->ku.multiple.count;
    argv2 = p->ku.multiple.array;
    p->ku.multiple.array = NULL;
    if (SAME_OBJ(argv2, p->values_buffer)) {
      if (c <= MAX_QUICK_CHAP_ARGV) {
        for (i = 0; i < c; i++) {
          a2[i] = argv2[i];
        }
        argv2 = a2;
      } else {
        p->values_buffer = NULL;
      }
    }
  } else {
    c = 1;
    a2[0] = v;
    argv2 = a2;
  }

  if (need_pop_mark) {
    MZ_CONT_MARK_POS += 2;
    scheme_pop_continuation_frame(&cframe);
  }
  
  if ((c == argc) || (c == (argc + 1))) {
    if (c > argc) {
      post = argv2[0];
      memmove(argv2, argv2 + 1, sizeof(Scheme_Object*)*argc);
    } else
      post = NULL;
    if (!(SCHEME_CHAPERONE_FLAGS(px) & SCHEME_CHAPERONE_IS_IMPERSONATOR)) {
      for (i = 0; i < argc; i++) {
        if (!SAME_OBJ(argv2[i], argv[i])
            && !scheme_chaperone_of(argv2[i], argv[i])) {
          if (argc == 1)
            scheme_wrong_chaperoned("procedure chaperone", "argument", argv[i], argv2[i]);
          else {
            char nbuf[32];
            sprintf(nbuf, "%d%s argument", i, scheme_number_suffix(i));
            scheme_wrong_chaperoned("procedure chaperone", nbuf, argv[i], argv2[i]);
          }
        }
      }
    }
  } else {
    scheme_raise_exn(MZEXN_FAIL_CONTRACT_ARITY,
                     "procedure %s: arity mismatch;\n"
                     " expected number of results not received from wrapper on the orignal\n"
                     " procedure's arguments\n"
                     "  wrapper: %V\n"
                     "  expected: %d or %d\n"
                     "  received: %d",
                     what,
                     SCHEME_CAR(px->redirects),
                     argc, argc + 1,
                     c);
    return NULL;
  }

  if (need_restore) {
    /* As a step toward space safety, even clear out the arguments 
       form the runstack: */
    MZ_RUNSTACK++;
    for (i = 0; i < argc; i++) {
      argv[i] = NULL;
    }
  } else
    argv = NULL;

  if (c == argc) {
    /* No filter for the result, so tail call: */
    if (app_mark)
      scheme_set_cont_mark(SCHEME_CAR(app_mark), SCHEME_CDR(app_mark));
    if (auto_val) {
      if (SCHEME_CHAPERONEP(px->prev))
        return do_apply_chaperone(px->prev, c, argv2, auto_val, 0);
      else
        return argv2[0];
    } else {
      if (orig_obj)
        /* A raw pair tells apply to extract a procedure from orig_obj */
        orig_obj = scheme_make_raw_pair(px->prev, orig_obj);
      else
        orig_obj = px->prev;
      if (checks) {
        /* cannot return a tail call */
        MZ_CONT_MARK_POS -= 2;
        if (checks & 0x1) {
          v = _scheme_apply(orig_obj, c, argv2);
        } else if (SAME_TYPE(SCHEME_TYPE(orig_obj), scheme_native_closure_type)) {
          v = _apply_native(orig_obj, c, argv2);
        } else {
          v = _scheme_apply_multi(orig_obj, c, argv2);
        }
        MZ_CONT_MARK_POS += 2;
        return v;
      } else
        return scheme_tail_apply(orig_obj, c, argv2);
    }
  } else {
    /* First element is a filter for the result(s) */
    if (!SCHEME_PROCP(post))
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                       "procedure %s: wrapper's first result is not a procedure;\n"
                       " extra result compared to original argument count should be\n"
                       " a wrapper for the original procedure's result\n"
                       "  wrapper: %V\n"
                       "  received: %V",
                       what,
                       SCHEME_CAR(px->redirects),
                       post);

    if (app_mark) {
      scheme_push_continuation_frame(&cframe);
      scheme_set_cont_mark(SCHEME_CAR(app_mark), SCHEME_CDR(app_mark));
      MZ_CONT_MARK_POS -= 2;
      need_pop_mark = 1;
    }else
      need_pop_mark = 0;

    if (auto_val) {
      if (SCHEME_CHAPERONEP(px->prev))
        result_v = do_apply_chaperone(px->prev, argc, argv2, auto_val, 0);
      else
        result_v = argv2[0];
      v = auto_val;
    } else {
      if (orig_obj)
        /* A raw pair tells apply to extract a procedure from orig_obj */
        orig_obj = scheme_make_raw_pair(px->prev, orig_obj);
      else
        orig_obj = px->prev;
      if (SAME_TYPE(SCHEME_TYPE(orig_obj), scheme_native_closure_type))
        v = _apply_native(orig_obj, argc, argv2);
      else
        v = _scheme_apply_multi(orig_obj, argc, argv2);
      result_v = NULL;
    }

    if (v == SCHEME_MULTIPLE_VALUES) {
      GC_CAN_IGNORE Scheme_Thread *p = scheme_current_thread;
      if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
        p->values_buffer = NULL;
      c = p->ku.multiple.count;
      argv = p->ku.multiple.array;
      p->ku.multiple.array = NULL;
    } else {
      c = 1;
      a[0] = v;
      argv = a;
    }

    if (need_pop_mark) {
      MZ_CONT_MARK_POS += 2;
      scheme_pop_continuation_frame(&cframe);
    }
    
    if (!scheme_check_proc_arity(NULL, c, 0, -1, &post))
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                       "procedure-result chaperone: arity mismatch;\n"
                       " wrapper does not accept the number of values produced by\n"
                       " the original procedure\n"
                       "  wrapper: %V\n"
                       "  number of values: %d",
                       post,
                       c);
    
    if (SAME_TYPE(SCHEME_TYPE(post), scheme_native_closure_type))
      v = _apply_native(post, c, argv);
    else
      v = _scheme_apply_multi(post, c, argv);
    if (v == SCHEME_MULTIPLE_VALUES) {
      GC_CAN_IGNORE Scheme_Thread *p = scheme_current_thread;
      if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
        p->values_buffer = NULL;
      argc = p->ku.multiple.count;
      argv2 = p->ku.multiple.array;
      p->ku.multiple.array = NULL;
    } else {
      argc = 1;
      a2[0] = v;
      argv2 = a2;
    }

    if (c == argc) {
      if (!(SCHEME_CHAPERONE_FLAGS(px) & SCHEME_CHAPERONE_IS_IMPERSONATOR)) {
        for (i = 0; i < argc; i++) {
          if (!SAME_OBJ(argv2[i], argv[i])
              && !scheme_chaperone_of(argv2[i], argv[i])) {
            if (argc == 1)
              scheme_wrong_chaperoned("procedure-result chaperone", "result", argv[i], argv2[i]);
            else {
              char nbuf[32];
              sprintf(nbuf, "%d%s result", i, scheme_number_suffix(i));
              scheme_wrong_chaperoned("procedure-result chaperone", nbuf, argv[i], argv2[i]);
            }
          }
        }
      }
    } else {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT_ARITY,
                       "procedure-result %s: result arity mismatch;\n"
                       " expected number of values not received from wrapper on the original\n"
                       " procedure's result\n"
                       "  wrapper: %V\n"
                       "  expected: %d\n"
                       "  received: %d",
                       what,
                       post,
                       c, argc);
      return NULL;
    }

    if (result_v)
      return result_v;
    else if (c == 1)
      return argv2[0];
    else {
      if (checks & 0x1)
        scheme_wrong_return_arity(NULL, 1, c, argv2, NULL);
      return scheme_values(c, argv2);
    }
  }
}

static Scheme_Object *
apply(int argc, Scheme_Object *argv[])
{
  Scheme_Object *rands;
  Scheme_Object **rand_vec;
  int i, num_rands;
  Scheme_Thread *p = scheme_current_thread;

  if (!SCHEME_PROCP(argv[0])) {
    scheme_wrong_contract("apply", "procedure?", 0, argc, argv);
    return NULL;
  }

  rands = argv[argc-1];

  num_rands = scheme_proper_list_length(rands);
  if (num_rands < 0) {
    scheme_wrong_contract("apply", "list?", argc - 1, argc, argv);
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
    scheme_wrong_contract("call-with-values", "procedure?", 1, argc, argv);

  v = _scheme_apply_multi(argv[0], 0, NULL);
  p = scheme_current_thread;
  if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES)) {
    int n;
    Scheme_Object **a;
    if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
      p->values_buffer = NULL;
    n = p->ku.multiple.count;
    a = p->ku.multiple.array;
    p->ku.multiple.array = NULL;
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

void scheme_detach_multple_array(Scheme_Object **values)
{
  Scheme_Thread *t = scheme_current_thread;

  if (SAME_OBJ(values, t->values_buffer))
    t->values_buffer = NULL;
}

/*========================================================================*/
/*                             continuations                              */
/*========================================================================*/

static void reset_cjs(Scheme_Continuation_Jump_State *a)
{
  a->jumping_to_continuation = NULL;
  a->alt_full_continuation = NULL;
  a->val = NULL;
  a->num_vals = 0;
  a->is_kill = 0;
  a->is_escape = 0;
  a->skip_dws = 0;
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
  a->alt_full_continuation = b->alt_full_continuation;
  a->val = b->val;
  a->num_vals = b->num_vals;
  a->is_kill = b->is_kill;
  a->is_escape = b->is_escape;
  a->skip_dws = b->skip_dws;
}

Scheme_Object *
do_call_ec (int argc, Scheme_Object *argv[], Scheme_Object *_for_cc)
{
  mz_jmp_buf newbuf;
  Scheme_Escaping_Cont * volatile cont;
  Scheme_Thread *p1 = scheme_current_thread;
  Scheme_Object * volatile v;
  Scheme_Object *a[1];
  Scheme_Cont_Frame_Data cframe;
  Scheme_Prompt *barrier_prompt;
  Scheme_Object * volatile for_cc = _for_cc;

  cont = MALLOC_ONE_TAGGED(Scheme_Escaping_Cont);
  cont->so.type = scheme_escaping_cont_type;
  ASSERT_SUSPEND_BREAK_ZERO();

  cont->saveerr = p1->error_buf;
  p1->error_buf = &newbuf;
  cont->myerr = &newbuf;

  scheme_save_env_stack_w_thread(cont->envss, p1);

  barrier_prompt = scheme_get_barrier_prompt(NULL, NULL);
  cont->barrier_prompt = barrier_prompt;

  scheme_prompt_capture_count++;

  if (!for_cc)
    scheme_push_continuation_frame(&cframe);
  scheme_set_cont_mark((Scheme_Object *)cont, scheme_true);

  if (scheme_setjmp(newbuf)) {
    Scheme_Thread *p2 = scheme_current_thread;
    if (p2->cjs.jumping_to_continuation
	&& SAME_OBJ(p2->cjs.jumping_to_continuation, (Scheme_Object *)cont)) {
      Scheme_Object *alt_cont;
      int n;

      alt_cont = p2->cjs.alt_full_continuation;
      if (alt_cont && !((Scheme_Cont *)alt_cont)->orig_escape_cont) {
        /* The escape continuation does not exactly match the target
           continuation; the fll continuation was just re-using an
           existing escape continuation. Now that there's no barrier
           in the way, jump to the full continuation. */
        return jump_to_alt_continuation();
      }

      n = p2->cjs.num_vals;
      v = p2->cjs.val;
      reset_cjs(&p2->cjs);
      scheme_restore_env_stack_w_thread(cont->envss, p2);
      p2->suspend_break = 0;
      if (n != 1)
        v = scheme_values(n, (Scheme_Object **)v);
    } else {
      scheme_longjmp(*cont->saveerr, 1);
    }
  } else if (for_cc) {
    ((Scheme_Cont *)for_cc)->escape_cont = (Scheme_Object *)cont;
    ((Scheme_Cont *)for_cc)->orig_escape_cont = 1;
    a[0] = (Scheme_Object *)for_cc;
    MZ_CONT_MARK_POS -= 2;
    v = _scheme_apply_multi(argv[0], 1, a);
    MZ_CONT_MARK_POS += 2;
  } else {
    a[0] = (Scheme_Object *)cont;
    v = _scheme_apply_multi(argv[0], 1, a);
  }

  p1 = scheme_current_thread;

  p1->error_buf = cont->saveerr;
  if (!for_cc)
    scheme_pop_continuation_frame(&cframe);

  return v;
}

Scheme_Object *
scheme_call_ec (int argc, Scheme_Object *argv[])
{
  scheme_check_proc_arity("call-with-escape-continuation", 1,
			  0, argc, argv);

  return do_call_ec(argc, argv, NULL);
}

int scheme_escape_continuation_ok(Scheme_Object *ec)
{
  Scheme_Escaping_Cont *cont = (Scheme_Escaping_Cont *)ec;

  if (scheme_extract_one_cc_mark(NULL, (Scheme_Object *)cont))
    return 1;
  else
    return 0;
}

static Scheme_Object *make_continuation_mark_key (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o;

  if (argc && !SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_contract("make-continuation-mark-key", "symbol?", 0, argc, argv);

  o = scheme_alloc_small_object();
  o->type = scheme_continuation_mark_key_type;
  SCHEME_PTR_VAL(o) = (argc ? argv[0] : NULL);

  return o;
}

static Scheme_Object *continuation_mark_key_p (int argc, Scheme_Object *argv[])
{
  return (SCHEME_CHAPERONE_CONTINUATION_MARK_KEYP(argv[0])
          ? scheme_true
          : scheme_false);
}

Scheme_Object *scheme_chaperone_do_continuation_mark (const char *name, int is_get, Scheme_Object *key, Scheme_Object *val)
{
  Scheme_Chaperone *px;
  Scheme_Object *proc;
  Scheme_Object *a[1];

  while (1) {
    if (SCHEME_CONTINUATION_MARK_KEYP(key)) {
      return val;
    } else {
      px = (Scheme_Chaperone *)key;
      key = px->prev;

      if (is_get)
        proc = SCHEME_CAR(px->redirects);
      else
        proc = SCHEME_CDR(px->redirects);

      a[0] = val;
      val = _scheme_apply(proc, 1, a);

      if (!(SCHEME_CHAPERONE_FLAGS(px) & SCHEME_CHAPERONE_IS_IMPERSONATOR)) {
        if (!scheme_chaperone_of(val, a[0]))
          scheme_wrong_chaperoned(name, "value", a[0], val);
      }
    }
  }
}

Scheme_Object *do_chaperone_continuation_mark_key (const char *name, int is_impersonator, int argc, Scheme_Object **argv)
{
  Scheme_Chaperone *px;
  Scheme_Object *val = argv[0];
  Scheme_Object *redirects;
  Scheme_Hash_Tree *props;

  if (SCHEME_CHAPERONEP(val))
    val = SCHEME_CHAPERONE_VAL(val);

  if (!SCHEME_CONTINUATION_MARK_KEYP(val))
    scheme_wrong_contract(name, "continuation-mark-key?", 0, argc, argv);

  scheme_check_proc_arity(name, 1, 1, argc, argv);
  scheme_check_proc_arity(name, 1, 2, argc, argv);

  redirects = scheme_make_pair(argv[1], argv[2]);

  props = scheme_parse_chaperone_props(name, 3, argc, argv);

  px = MALLOC_ONE_TAGGED(Scheme_Chaperone);
  px->iso.so.type = scheme_chaperone_type;
  px->val = val;
  px->prev = argv[0];
  px->props = props;
  px->redirects = redirects;

  if (is_impersonator)
    SCHEME_CHAPERONE_FLAGS(px) |= SCHEME_CHAPERONE_IS_IMPERSONATOR;

  return (Scheme_Object *)px;
}

static Scheme_Object *chaperone_continuation_mark_key(int argc, Scheme_Object **argv)
{
  return do_chaperone_continuation_mark_key("chaperone-continuation-mark-key", 0, argc, argv);
}

static Scheme_Object *impersonate_continuation_mark_key(int argc, Scheme_Object **argv)
{
  return do_chaperone_continuation_mark_key("impersonate-continuation-mark-key", 1, argc, argv);
}


static Scheme_Object *call_with_immediate_cc_mark (int argc, Scheme_Object *argv[])
{
  Scheme_Thread *p = scheme_current_thread;
  intptr_t findpos, bottom;
  Scheme_Object *a[1], *key;

  scheme_check_proc_arity("call-with-immediate-continuation-mark", 1, 1, argc, argv);

  key = argv[0];
  if (SCHEME_NP_CHAPERONEP(key)
      && SCHEME_CONTINUATION_MARK_KEYP(SCHEME_CHAPERONE_VAL(key)))
    key = SCHEME_CHAPERONE_VAL(key);

  if (argc > 2)
    a[0] = argv[2];
  else
    a[0] = scheme_false;

  if (p->cont_mark_stack_segments) {
    findpos = (intptr_t)MZ_CONT_MARK_STACK;
    bottom = (intptr_t)p->cont_mark_stack_bottom;
    while (findpos-- > bottom) {
      Scheme_Cont_Mark *seg = p->cont_mark_stack_segments[findpos >> SCHEME_LOG_MARK_SEGMENT_SIZE];
      intptr_t pos = findpos & SCHEME_MARK_SEGMENT_MASK;
      Scheme_Cont_Mark *find = seg + pos;

      if ((intptr_t)find->pos < (intptr_t)MZ_CONT_MARK_POS) {
        break;
      } else {
        if (find->key == key) {
          /*
           * If not equal, it was a chaperone since we unwrapped the key
           */
          if (argv[0] != key) {
            Scheme_Object *val;
            val = scheme_chaperone_do_continuation_mark("call-with-immediate-continuation-mark",
                                                        1, argv[0], find->val);
            a[0] = val;
          } else
            a[0] = find->val;
          break;
        }
      }
    }
  }

  return scheme_tail_apply(argv[1], 1, a);
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
    scheme_wrong_contract(who, "semaphore?", 0, argc, argv);
    return NULL;
  }
  if (argc > 2)
    extra = argc - 3;
  else
    extra = 0;
  if (!scheme_check_proc_arity(NULL, extra, 1, argc, argv)) {
    if (!SCHEME_PROCP(argv[1]))
      scheme_wrong_contract(who, "procedure?", 1, argc, argv);
    else
      scheme_contract_error(who, "procedure arity does not match extra-argument count",
                            "procedure", 1, argv[1],
                            "extra-argument count", 1, scheme_make_integer(extra),
                            NULL);
    return NULL;
  }
  if ((argc > 2) && SCHEME_TRUEP(argv[2])) {
    if (!scheme_check_proc_arity(NULL, 0, 2, argc, argv)) {
      scheme_wrong_contract(who, "(or/c (-> any) #f)", 1, argc, argv);
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
  intptr_t size;
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
  saved->runstack_offset = (runstack XFORM_OK_MINUS runstack_start);

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
					     intptr_t *_offset,
					     Scheme_Prompt *effective_prompt,
                                             int clear_caches)
{
  intptr_t cmcount, offset = 0, sub_count = 0;
  Scheme_Cont_Mark *cont_mark_stack_copied;

  /* Copy cont mark stack: */
  cmcount = (intptr_t)pos;
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
      intptr_t pos = cms & SCHEME_MARK_SEGMENT_MASK;
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
  intptr_t size;

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
			       intptr_t copied_offset, Scheme_Object **_sub_conts,
                               int clear_caches)
     /* Copies in the mark stack up to depth cms, but assumes that the
	stack up to depth base_cms is already in place (probably in
	place for a dynamic-wind context in an continuation
	restoration.) */
{
  intptr_t cmcount, base_cmcount, cmoffset;
  Scheme_Cont_Mark *cm_src;
  Scheme_Cont *sub_cont = NULL;

  cmcount = (intptr_t)cms;
  base_cmcount = (intptr_t)base_cms;

  if (cmcount) {
    /* First, make sure we have enough segments */
    intptr_t needed = ((cmcount - 1) >> SCHEME_LOG_MARK_SEGMENT_SIZE) + 1;

    if (needed > p->cont_mark_seg_count) {
      Scheme_Cont_Mark **segs, **old_segs = p->cont_mark_stack_segments;
      int newcount = needed, oldcount = p->cont_mark_seg_count, npos;

      /* Note: we perform allocations before changing p to avoid GC trouble,
	 since Racket adjusts a thread's cont_mark_stack_segments on GC. */
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
    intptr_t pos = base_cmcount & SCHEME_MARK_SEGMENT_MASK;
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
  intptr_t cmcount, delta = 0;

  cmcount = (intptr_t)MZ_CONT_MARK_STACK;

  while (cmcount--) {
    Scheme_Cont_Mark *seg = p->cont_mark_stack_segments[cmcount >> SCHEME_LOG_MARK_SEGMENT_SIZE];
    intptr_t pos = cmcount & SCHEME_MARK_SEGMENT_MASK;

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
                                           Scheme_Object *limit_prompt_tag, int limit_depth, int limit_count,
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
    if (cnt == limit_count)
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

static Scheme_Saved_Stack *clone_runstack_saved(Scheme_Saved_Stack *saved, Scheme_Object **boundary_start,
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

static Scheme_Saved_Stack *clone_runstack_copied(Scheme_Saved_Stack *copied, 
                                                 Scheme_Object **copied_start,
                                                 Scheme_Saved_Stack *saved, 
                                                 Scheme_Object **boundary_start,
                                                 intptr_t boundary_offset)
{
  Scheme_Saved_Stack *naya, *first = NULL, *prev = NULL, *s;

  if (copied_start == boundary_start) {
    naya = copied;
  } else {
    for (naya = copied->prev, s = saved; 
         s->runstack_start != boundary_start; 
         naya = naya->prev, s = s->prev) {
    }
  }
  if ((naya->runstack_offset + naya->runstack_size == boundary_offset)
      && !naya->prev) {
    /* no need to prune anything */
    return copied;
  }

  s = NULL;
  while (copied) {
    naya = MALLOC_ONE_RT(Scheme_Saved_Stack);
    memcpy(naya, copied, sizeof(Scheme_Saved_Stack));
    naya->prev = NULL;
    if (prev)
      prev->prev = naya;
    else
      first = naya;
    prev = naya;
    if ((!s && copied_start == boundary_start)
        || (s && (s->runstack_start == boundary_start))) {
      intptr_t size;
      Scheme_Object **a;
      size = boundary_offset - naya->runstack_offset;
      if (size < 0)
        scheme_signal_error("negative stack-copy size while pruning");
      if (size > naya->runstack_size)
        scheme_signal_error("bigger stack-copy size while pruning: %d vs. %d", size, naya->runstack_size);
      a = MALLOC_N(Scheme_Object *, size);
      memcpy(a, naya->runstack_start, size * sizeof(Scheme_Object *));
      naya->runstack_start = a;
      naya->runstack_size = size;
      break;
    }

    copied = copied->prev;
    if (!s)
      s = saved;
    else
      s = s->prev;
  }
  
  return first;
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
      intptr_t delta;
      void *stack_boundary;

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

      if ((prompt->boundary_overflow_id && (prompt->boundary_overflow_id == naya->overflow->id))
          || (!prompt->boundary_overflow_id && !naya->overflow->prev)) {
        stack_boundary = prompt->stack_boundary;
      } else {
        stack_boundary = naya->overflow->stack_start;
      }

      if (naya->cont) {
        Scheme_Cont *cnaya;
        Scheme_Saved_Stack *saved;

        cnaya = MALLOC_ONE_TAGGED(Scheme_Cont);
        memcpy(cnaya, naya->cont, sizeof(Scheme_Cont));

        naya->cont = cnaya;

        cnaya->cont_mark_total = naya->cont_mark_total;
        cnaya->cont_mark_offset = naya->cont_mark_offset;
        cnaya->cont_mark_pos_bottom = naya->cont_mark_pos_bottom;
        cnaya->cont_mark_stack_copied = naya->cont_mark_stack_copied;

        cnaya->prompt_stack_start = stack_boundary;

        /* Prune unneeded runstack data */
        saved = clone_runstack_copied(cnaya->runstack_copied, 
                                      cnaya->runstack_start,
                                      cnaya->runstack_saved, 
                                      prompt->runstack_boundary_start,
                                      prompt->runstack_boundary_offset);
        cnaya->runstack_copied = saved;

        /* Prune unneeded buffers */
        if (prompt->runstack_boundary_start == cnaya->runstack_start)
          saved = NULL;
        else
          saved = clone_runstack_saved(cnaya->runstack_saved, 
                                       prompt->runstack_boundary_start,
                                       NULL);
        cnaya->runstack_saved = saved;

        cnaya->need_meta_prompt = 1;
      }
      if (naya->overflow && !naya->overflow->eot) {
        /* Prune unneeded C-stack data */
        Scheme_Overflow *onaya;
        Scheme_Overflow_Jmp *jmp;
        jmp = scheme_prune_jmpup(naya->overflow->jmp, stack_boundary);
        if (jmp) {
          onaya = MALLOC_ONE_RT(Scheme_Overflow);
          memcpy(onaya, naya->overflow, sizeof(Scheme_Overflow));
          naya->overflow = onaya;
          onaya->jmp = jmp;
          onaya->stack_start = stack_boundary;
        }
      }
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

static void sync_meta_cont(Scheme_Meta_Continuation *resume_mc)
{
  Scheme_Cont *cnaya;

  if (!resume_mc->cont)
    return;

  cnaya = MALLOC_ONE_TAGGED(Scheme_Cont);
  memcpy(cnaya, resume_mc->cont, sizeof(Scheme_Cont));
    
  resume_mc->cont = cnaya;
    
  cnaya->ss.cont_mark_stack += (resume_mc->cont_mark_total - cnaya->cont_mark_total);

  cnaya->cont_mark_total = resume_mc->cont_mark_total;
  cnaya->cont_mark_offset = resume_mc->cont_mark_offset;
  cnaya->cont_mark_pos_bottom = resume_mc->cont_mark_pos_bottom;
  cnaya->cont_mark_stack_copied = resume_mc->cont_mark_stack_copied;
}

void prune_cont_marks(Scheme_Meta_Continuation *resume_mc, Scheme_Cont *cont, Scheme_Object *extra_marks)
{
  Scheme_Object *val;
  Scheme_Hash_Table *ht;
  intptr_t pos, num_overlap, num_coverlap, new_overlap, base, i;
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

  sync_meta_cont(resume_mc);
}

static MZ_MARK_STACK_TYPE exec_dyn_wind_pres(Scheme_Dynamic_Wind_List *dwl,
                                             int dwl_len,
                                             Scheme_Cont *cont,
                                             MZ_MARK_STACK_TYPE copied_cms,
                                             int clear_cm_caches,
                                             Scheme_Object **_sub_conts,
                                             int skip_dws)
{
  Scheme_Thread *p = scheme_current_thread;
  int old_cac = scheme_continuation_application_count;
  int need_clone = 0;
  Scheme_Dynamic_Wind *dw;

  for (; dwl; dwl = dwl->next) {
    if (dwl->dw->pre) {
      p->next_meta = dwl->meta_depth + dwl->dw->next_meta;
      if (dwl->meta_depth > 0) {
        if (!skip_dws)
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

        if (!skip_dws)
          pre(dwl->dw->data);

        if (!cont->composable) {
          if (scheme_continuation_application_count != old_cac) {
            old_cac = scheme_continuation_application_count;
            scheme_recheck_prompt_and_barrier(cont);
          }
        }
      }
      p = scheme_current_thread;
    }

    if (p->dw != dwl->dw->prev) {
      /* something happened in the pre-thunk to change the
         continuation that we're building */
      need_clone = 1;
    }
    
    if (need_clone) {
      dw = clone_dyn_wind(dwl->dw, NULL, -1, 1, p->dw, 0, 0);
      dw->next_meta = p->next_meta;
    } else
      dw = dwl->dw;
    p->dw = dw;
  }
  return copied_cms;
}

static Scheme_Object *
call_cc (int argc, Scheme_Object *argv[])
{
  scheme_check_proc_arity("call-with-current-continuation", 1,
			  0, argc, argv);
  if (argc > 1) {
    if (!SAME_TYPE(scheme_prompt_tag_type, SCHEME_TYPE(argv[1]))
        && !((SCHEME_NP_CHAPERONEP(argv[1])
              && SCHEME_PROMPT_TAGP(SCHEME_CHAPERONE_VAL(argv[1])))))
      scheme_wrong_contract("call-with-current-continuation", "continuation-prompt-tag?",
                            1, argc, argv);
  }

  /* Trampoline to internal_call_cc. This trampoline ensures that
     the runstack is flushed before we try to grab the continuation. */
  return _scheme_tail_apply(internal_call_cc_prim, argc, argv);
}

static Scheme_Cont *grab_continuation(Scheme_Thread *p, int for_prompt, int composable,
                                      Scheme_Object *prompt_tag, Scheme_Object *pt,
                                      Scheme_Cont *sub_cont, Scheme_Prompt *prompt,
                                      Scheme_Meta_Continuation *prompt_cont, 
                                      Scheme_Prompt *effective_barrier_prompt
                                      )
{
  Scheme_Cont *cont;
  Scheme_Cont_Jmp *buf_ptr;
  
  cont = MALLOC_ONE_TAGGED(Scheme_Cont);
  cont->so.type = scheme_cont_type;

  if (!for_prompt && !composable) {
    /* Set cont_key mark before capturing marks: */
    scheme_set_cont_mark(cont_key, (Scheme_Object *)cont);
  }

  if (composable)
    cont->composable = 1;
  
  buf_ptr = MALLOC_ONE_RT(Scheme_Cont_Jmp);
  SET_REQUIRED_TAG(buf_ptr->type = scheme_rt_cont_jmp);
  cont->buf_ptr = buf_ptr;

  scheme_init_jmpup_buf(&cont->buf_ptr->buf);
  cont->prompt_tag = prompt_tag;
  if (for_prompt)
    cont->dw = NULL;
  else if (prompt) {
    Scheme_Dynamic_Wind *dw;
    if (p->dw) {
      dw = clone_dyn_wind(p->dw, pt, -1, -1, NULL, 0, composable);
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
    mc = clone_meta_cont(p->meta_continuation, pt, -1, prompt_cont, prompt, NULL, composable);
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
    if (cont->ss.cont_mark_stack && !p->cont_mark_stack_owner) {
      Scheme_Thread **owner;
      owner = MALLOC_N(Scheme_Thread *, 1);
      p->cont_mark_stack_owner = owner;
      *owner = p;
    }
  }

#ifdef MZ_USE_JIT
  /* This information can be expensive to compute, no one uses it
     currently, and it's approximate anyway. So skip it. */
  if (0) {
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
      /* Prune cont->runstack_saved to drop unneeded saves.
         (Note that this is different than runstack_copied; 
          runstack_saved keeps the shared runstack buffers, 
          not the content.) */
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
    intptr_t offset;
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
    p->ku.multiple.array = NULL;
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
  if (for_prompt) {
    if (p->meta_prompt)
      cont->need_meta_prompt = 1;
  } else {
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

        resume_mc->cont = cm_cont;

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
     continuation, then we're sharing its base runstack. */
  copy_in_runstack(p, cont->runstack_copied, 0);
  {
    intptr_t done = cont->runstack_copied->runstack_size, size;
    sub_cont = cont;
    while (sub_cont) {
      if (sub_cont->buf_ptr->buf.cont
          && (sub_cont->runstack_start == sub_cont->buf_ptr->buf.cont->runstack_start)) {
        /* Copy shared part in: */
        sub_cont = sub_cont->buf_ptr->buf.cont;
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

  /* If there's a resume, then set up a meta prompt.
     We also need a meta-prompt if we're returning from a composed
     continuation to a continuation captured under a meta-prompt,
     or truncated somewhere along the way. */
  if (resume || (for_prompt && cont->need_meta_prompt)) {
    Scheme_Prompt *meta_prompt;

    meta_prompt = MALLOC_ONE_TAGGED(Scheme_Prompt);
    meta_prompt->so.type = scheme_prompt_type;
    meta_prompt->stack_boundary = cont->prompt_stack_start;
    meta_prompt->boundary_overflow_id = NULL;
    {
      Scheme_Cont *tc;
      for (tc = cont; tc->buf_ptr->buf.cont; tc = tc->buf_ptr->buf.cont) {
      }
      meta_prompt->mark_boundary = tc->cont_mark_offset;
    }
    meta_prompt->prompt_buf = cont->prompt_buf;
    {
      /* Reverse-engineer where the saved runstack ends: */
      Scheme_Cont *rs_cont = cont;
      Scheme_Saved_Stack *saved, *actual;
      int delta = 0;
      while (rs_cont->buf_ptr->buf.cont) {
        delta += rs_cont->runstack_copied->runstack_size;
        rs_cont = rs_cont->buf_ptr->buf.cont;
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
  for (sub_cont = cont->buf_ptr->buf.cont; sub_cont; sub_cont = sub_cont->buf_ptr->buf.cont) {
    copied_cms = sub_cont->cont_mark_offset;
    sub_conts = scheme_make_raw_pair((Scheme_Object *)sub_cont, sub_conts);
  }

  if (!shortcut_prompt) {    
    Scheme_Cont *tc;
    for (tc = cont; tc->buf_ptr->buf.cont; tc = tc->buf_ptr->buf.cont) {
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
          all_dw = clone_dyn_wind(p->dw, cont->prompt_tag, -1, -1, NULL, 1, 0);
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

      /* The allow_dw chain that we build up here is actually
         premature, in that the tail to splice onto may change
         in pre-thunks. It doesn't usually happen, and we can 
         detect that case in exec_dyn_wind_pres() in re-clone. */
      common_depth = (p->dw ? p->dw->depth : -1);
      all_dw = clone_dyn_wind(cont->dw, NULL, cont->common_dw_depth, -1, p->dw, 0, 0);

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
      copied_cms = exec_dyn_wind_pres(dwl, dwl_len, cont, copied_cms, clear_cm_caches, &sub_conts,
                                      cont->skip_dws);
      p = scheme_current_thread;
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
  Scheme_Object *ret, * volatile pt, * prompt_tag;
  Scheme_Cont * volatile cont;
  Scheme_Cont *sub_cont;
  Scheme_Meta_Continuation *prompt_cont, *barrier_cont;
  MZ_MARK_POS_TYPE prompt_pos, barrier_pos;
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Prompt *prompt, *barrier_prompt, *effective_barrier_prompt;
  Scheme_Object *ec;
  GC_CAN_IGNORE void *stack_start;
  int composable;

  if (argc > 1)
    prompt_tag = argv[1];
  else
    prompt_tag = scheme_default_prompt_tag;

  if (SCHEME_NP_CHAPERONEP(prompt_tag))
    pt = SCHEME_CHAPERONE_VAL(prompt_tag);
  else
    pt = prompt_tag;

  composable = (argc > 2);

  prompt = scheme_get_prompt(SCHEME_PTR_VAL(pt), &prompt_cont, &prompt_pos);
  if (!prompt && !SAME_OBJ(scheme_default_prompt_tag, pt)) {
    scheme_contract_error((composable
                           ? "call-with-composable-continuation"
                           : "call-with-current-continuation"), 
                          "continuation includes no prompt with the given tag",
                          "tag", 1, prompt_tag,
                          NULL);
    return NULL;
  }

  barrier_prompt = scheme_get_barrier_prompt(&barrier_cont, &barrier_pos);

  if (composable && SCHEME_FALSEP(argv[2])) {
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
  if (sub_cont 
      && (sub_cont->save_overflow == p->overflow)
      && (sub_cont->prompt_tag == prompt_tag)
      && (sub_cont->barrier_prompt == effective_barrier_prompt)
      && (((Scheme_Escaping_Cont *)sub_cont->escape_cont)->myerr == p->error_buf)) {
    /* Whether sub_cont turns out to be the same continuation, we can use
       its escape continuation, because jumping to the escape continuation
       triggers the same C-level clean-up actions, same `dynamic-wind's, and
       crosses the same continuation barriers. */
    ec = sub_cont->escape_cont; 
  } else
    ec = NULL;
  if (sub_cont && ((sub_cont->save_overflow != p->overflow)
		   || (sub_cont->prompt_tag != prompt_tag)
		   || (sub_cont->barrier_prompt != effective_barrier_prompt)
		   || (sub_cont->meta_continuation != p->meta_continuation))) {
    sub_cont = NULL;
  }
  if (sub_cont && (sub_cont->ss.cont_mark_pos == MZ_CONT_MARK_POS)) {
    Scheme_Object *argv2[1];
#ifdef MZ_USE_JIT
    /* See note above on how the stack trace is expensive to compute
       and not all that useful. */
    if (0)
      ret = scheme_native_stack_trace();
    else
      ret = NULL;
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
      intptr_t offset;
      Scheme_Cont_Mark *msaved;
      Scheme_Cont_Jmp *buf_ptr;

      cont = MALLOC_ONE_TAGGED(Scheme_Cont);
      cont->so.type = scheme_cont_type;

      buf_ptr = MALLOC_ONE_RT(Scheme_Cont_Jmp);
      SET_REQUIRED_TAG(buf_ptr->type = scheme_rt_cont_jmp);
      cont->buf_ptr = buf_ptr;

      cont->buf_ptr->buf.cont = sub_cont;
      cont->escape_cont = sub_cont->escape_cont;

      sub_cont = sub_cont->buf_ptr->buf.cont;

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

  cont = grab_continuation(p, 0, composable, prompt_tag, pt, sub_cont, 
                           prompt, prompt_cont, effective_barrier_prompt);

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
      Scheme_Prompt *meta_prompt, *stack_barrier_prompt;

      stack_barrier_prompt = barrier_prompt;

      if (!stack_barrier_prompt->is_barrier)
        stack_barrier_prompt = NULL;
      else if (stack_barrier_prompt->boundary_overflow_id != overflow_id)
        stack_barrier_prompt = NULL;
      meta_prompt = p->meta_prompt;
      if (meta_prompt)
        if (meta_prompt->boundary_overflow_id != overflow_id)
          meta_prompt = NULL;

      if (stack_barrier_prompt && meta_prompt) {
        stack_barrier_prompt = NULL;
      }

      if (stack_barrier_prompt)
        stack_start = stack_barrier_prompt->stack_boundary;
      else if (meta_prompt)
        stack_start = meta_prompt->stack_boundary;
      else
        stack_start = p->stack_start;
    }
  }

  /* Use cont->stack_start when calling `cont' directly
     from the same meta-continuation. Use cont->prompt_stack_start 
     when calling `cont' composably (i.e., when supplying a resume). */
  cont->prompt_stack_start = stack_start;

  cont->escape_cont = ec;

  /* Zero out any local variable that shouldn't be saved by the
     continuation.  The meta-continuation for the prompt is an
     especially important one to zero out (otherwise we build up
     chains). */
  prompt_cont = NULL;
  barrier_cont = NULL;

  if (scheme_setjmpup_relative(&cont->buf_ptr->buf, cont->buf_ptr, stack_start, sub_cont)) {
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
                         pt, sub_cont, 
                         common_dw, common_next_meta, shortcut_prompt,
                         !!resume, 1, 
                         use_next_cont, extra_marks);

    /* We may have just re-activated breaking: */
    scheme_check_break_now();

    if (!scheme_get_barrier_prompt(NULL, NULL)) {
      /* The continuation was applied in a thread where the barrier prompt
         was supposed to be the pseduo-prompt for a thread, but we've lost
         that prompt. The barrier prompt from capturing the continuation
         has the right info, but we need to claim that it's not a barrier
         from the perspective of changing continuations. */
      Scheme_Prompt *acting_barrier_prompt;
      if (!barrier_prompt->is_barrier)
        acting_barrier_prompt = barrier_prompt;
      else {
        acting_barrier_prompt = MALLOC_ONE_TAGGED(Scheme_Prompt);
        memcpy(acting_barrier_prompt, barrier_prompt, sizeof(Scheme_Prompt));
        acting_barrier_prompt->is_barrier = 0;
      }
      p->acting_barrier_prompt = acting_barrier_prompt;
    }

    {
      Scheme_Meta_Continuation *mc;
      MZ_MARK_POS_TYPE pos;
      Scheme_Object *cc_guard;

      prompt = scheme_get_prompt(SCHEME_PTR_VAL(pt), &mc, &pos);
      if (prompt && (prompt->has_chaperone || SCHEME_NP_CHAPERONEP(cont->prompt_tag))) {
        cc_guard = get_set_cont_mark_by_pos(prompt_cc_guard_key, p, mc, pos, NULL);
        
        if (SCHEME_FALSEP(cc_guard))
          cc_guard = scheme_values_func;
        if (SCHEME_NP_CHAPERONEP(cont->prompt_tag))  
          cc_guard = chaperone_wrap_cc_guard(cont->prompt_tag, cc_guard);
        
        get_set_cont_mark_by_pos(prompt_cc_guard_key, p, mc, pos, cc_guard);
      }
    }
    
    return result;
  } else if (composable || cont->escape_cont) {
    Scheme_Object *argv2[1];

    if (SCHEME_TRUEP(argv[2]))
      cont->skip_dws = 1;

    argv2[0] = (Scheme_Object *)cont;
    ret = _scheme_tail_apply(argv[0], 1, argv2);
    return ret;
  } else {
    Scheme_Object *argv2[2];

    argv2[0] = argv[0];
    argv2[1] = (Scheme_Object *)cont;

    ret = _scheme_tail_apply(finish_call_cc_prim, 2, argv2);
    return ret;
  }
}

static Scheme_Object *
finish_call_cc (int argc, Scheme_Object *argv[])
{
  return do_call_ec(1, argv, argv[1]);
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

  /* scheme_apply_multi() is a top-level evaluation function and will
     thus install a continuation barrier */
  return scheme_apply_multi(argv[0], 0, NULL);
}

Scheme_Prompt *scheme_get_barrier_prompt(Scheme_Meta_Continuation **_meta_cont,
                                         MZ_MARK_POS_TYPE *_pos)
{
  Scheme_Prompt *p;
  
  p = (Scheme_Prompt *)scheme_extract_one_cc_mark_with_meta(NULL, barrier_prompt_key, NULL, _meta_cont, _pos);
  if (!p) {
    p = scheme_current_thread->acting_barrier_prompt;
    if (_meta_cont) {
      /* acting barrier prompt is deepest: */
      Scheme_Meta_Continuation *mc = scheme_current_thread->meta_continuation;
      while (mc && mc->next) {
        mc = mc->next;
      }
      *_meta_cont = mc;
      *_pos = -1;
    }
  }

  return p;
}

Scheme_Prompt *scheme_get_prompt(Scheme_Object *prompt_tag,
                                 Scheme_Meta_Continuation **_meta_cont,
                                 MZ_MARK_POS_TYPE *_pos)
{
  return (Scheme_Prompt *)scheme_extract_one_cc_mark_with_meta(NULL, prompt_tag, NULL, _meta_cont, _pos);
}

static Scheme_Meta_Continuation *scheme_get_meta_continuation(Scheme_Object *key)
{
  Scheme_Meta_Continuation *mc;
  scheme_extract_one_cc_mark_with_meta(NULL, key, NULL, &mc, NULL);
  return mc;
}


static Scheme_Object *make_prompt_tag (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o, *key;

  if (argc && !SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_contract("make-continuation-prompt-tag", "symbol?", 0, argc, argv);

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
  return (SCHEME_CHAPERONE_PROMPT_TAGP(argv[0])
          ? scheme_true
          : scheme_false);
}

Scheme_Object *do_chaperone_prompt_tag (const char *name, int is_impersonator, int argc, Scheme_Object **argv)
{
  Scheme_Chaperone *px;
  Scheme_Object *val = argv[0];
  Scheme_Object *redirects;
  Scheme_Hash_Tree *props;
  int ppos;

  if (SCHEME_CHAPERONEP(val))
    val = SCHEME_CHAPERONE_VAL(val);

  if (!SCHEME_PROMPT_TAGP(val))
    scheme_wrong_contract(name, "prompt-tag?", 0, argc, argv);

  if (!SCHEME_PROCP(argv[1]))
    scheme_wrong_contract(name, "procedure?", 1, argc, argv);
  if (!SCHEME_PROCP(argv[2]))
    scheme_wrong_contract(name, "procedure?", 2, argc, argv);

  if ((argc > 3) && !SCHEME_CHAPERONEP(argv[3])) {
    if (!SCHEME_PROCP(argv[3]))
      scheme_wrong_contract(name, "(or/c procedure? impersonator-property?)", 3, argc, argv);
    redirects = argv[3];
    if ((argc > 4) && !SCHEME_CHAPERONEP(argv[4])) {
      if (!scheme_check_proc_arity(NULL, 1, 4, argc, argv))
        scheme_wrong_contract(name, "(or/c (procedure-arity-includes/c 1) impersonator-property?)", 4, argc, argv);
      redirects = scheme_make_pair(redirects, argv[4]);
      ppos = 5;
    } else
      ppos = 4;
    redirects = scheme_make_pair(argv[2], redirects);
  } else {
    ppos = 3;
    redirects = argv[2];
  }
    
  redirects = scheme_make_pair(argv[1], redirects);

  props = scheme_parse_chaperone_props(name, ppos, argc, argv);

  px = MALLOC_ONE_TAGGED(Scheme_Chaperone);
  px->iso.so.type = scheme_chaperone_type;
  px->val = val;
  px->prev = argv[0];
  px->props = props;
  px->redirects = redirects;

  if (is_impersonator)
    SCHEME_CHAPERONE_FLAGS(px) |= SCHEME_CHAPERONE_IS_IMPERSONATOR;

  return (Scheme_Object *)px;
}

static Scheme_Object *chaperone_prompt_tag(int argc, Scheme_Object **argv)
{
  return do_chaperone_prompt_tag("chaperone-prompt-tag", 0, argc, argv);
}

static Scheme_Object *impersonate_prompt_tag(int argc, Scheme_Object **argv)
{
  return do_chaperone_prompt_tag("impersonate-prompt-tag", 1, argc, argv);
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

MZ_DO_NOT_INLINE(Scheme_Object *scheme_finish_apply_for_prompt(Scheme_Prompt *prompt, Scheme_Object *_prompt_tag, 
                                                               Scheme_Object *proc, int argc, Scheme_Object **argv));

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
      if (val) {
        if (val == SCHEME_MULTIPLE_VALUES) {
          if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
            p->values_buffer = NULL;
        }
        p->cjs.val = val;
      }
      p->stack_start = resume->stack_start;
      p->decompose_mc = resume_mc;
      scheme_longjmpup(&resume->jmp->cont);
      return NULL;
    }
  }
}

MZ_DO_NOT_INLINE(Scheme_Object *scheme_apply_for_prompt(Scheme_Prompt *prompt, Scheme_Object *prompt_tag, 
                                                        Scheme_Object *proc, int argc, Scheme_Object **argv));

Scheme_Object *scheme_apply_for_prompt(Scheme_Prompt *prompt, Scheme_Object *prompt_tag, 
                                       Scheme_Object *proc, int argc, Scheme_Object **argv)
{
  /* Grab stack address, then continue on with final step: */
  prompt->stack_boundary = PROMPT_STACK(proc);

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
  saved = grab_continuation(p, 1, 0, NULL, NULL, NULL, NULL, NULL, NULL);

  if (p->meta_prompt)
    saved->prompt_stack_start = p->meta_prompt->stack_boundary;

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

  saved->resume_to = overflow; /* used by eval to jump to current meta-continuation */
  offstack_cont = saved;
  saved = NULL;

  scheme_init_jmpup_buf(&overflow->jmp->cont);

  offstack_overflow = overflow;
  overflow = NULL; /* so it's not saved in the continuation */

  if (scheme_setjmpup(&offstack_overflow->jmp->cont, 
                      offstack_overflow->jmp, 
                      p->stack_start)) {
    /* Returning. (Jumped here from finish_apply_for_prompt,
       scheme_compose_continuation, scheme_eval, or start_child.)
       
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
    Scheme_Meta_Continuation *mc, *dmc;

    p = scheme_current_thread;

    dmc = p->decompose_mc;
    p->decompose_mc = NULL;
    saved = dmc->cont;
    overflow = dmc->overflow;

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
                         1, !p->cjs.jumping_to_continuation, 
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
  } else {
    saved = offstack_cont;
    overflow = offstack_overflow;
    offstack_cont = NULL;
    offstack_overflow = NULL;
  }

  scheme_current_thread->suspend_break++;
  
  /* Here's where we jump to the target: */
  cont->use_next_cont = saved;
  cont->resume_to = overflow;
  cont->empty_to_next_mc = (char)empty_to_next_mc;
  scheme_current_thread->stack_start = cont->prompt_stack_start;
  scheme_longjmpup(&cont->buf_ptr->buf);

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

static Scheme_Object **chaperone_do_control(const char *name, int mode, 
                                            Scheme_Object *init_guard, Scheme_Object *obj,
                                            int argc, Scheme_Object **argv)
{
  Scheme_Chaperone *px;
  Scheme_Object **vals = argv;
  Scheme_Object *v;
  Scheme_Object *proc;
  int i, num_args;

  while (1) {
    if (init_guard || !SCHEME_PROMPT_TAGP(obj)) {
      if (init_guard) {
        proc = init_guard;
        if (SAME_OBJ(NULL, scheme_values_func))
          proc = NULL;
        px = NULL;
      } else {
        px = (Scheme_Chaperone *)obj;
        obj = px->prev;

        if (!mode)
          proc = SCHEME_CAR(px->redirects);
        else {
          proc = SCHEME_CDR(px->redirects);
          if (mode == 1) {
            if (SCHEME_PAIRP(proc))
              proc = SCHEME_CAR(proc);
          } else {
            if (SCHEME_PAIRP(proc)) {
              proc = SCHEME_CDR(proc);
              if (mode == 2) {
                if (SCHEME_PAIRP(proc))
                  proc = SCHEME_CAR(proc);
              } else {
                if (SCHEME_PAIRP(proc))
                  proc = SCHEME_CDR(proc);
                else
                  proc = NULL;
              }
            } else
              proc = NULL;
          }
        }
      }

      if (proc) {
        if (mode == 3)
          v = scheme_apply(proc, argc, argv); /* with barrier */
        else
          v = _scheme_apply_multi(proc, argc, argv);

        if (v == SCHEME_MULTIPLE_VALUES) {
          GC_CAN_IGNORE Scheme_Thread *p = scheme_current_thread;
          if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
            p->values_buffer = NULL;
          num_args = p->ku.multiple.count;
          vals = p->ku.multiple.array;
          p->ku.multiple.array = NULL;
        } else {
          num_args = 1;
          vals = MALLOC_N(Scheme_Object *, 1);
          vals[0] = v;
        }

        /*
         * All kinds of proxies should return the same number of results
         * as the number of aborted values
         */
        if (num_args == 1 && num_args != argc)
          scheme_wrong_return_arity(name, argc, 1, (Scheme_Object **)(vals[0]), "use of redirecting procedure");
        else if (num_args != argc)
          scheme_wrong_return_arity(name, argc, num_args, vals, "use of redirecting procedure");

        if (mode == 3) {
          if (!scheme_check_proc_arity(NULL, 1, 0, argc, vals)) {
            scheme_wrong_type("call/cc guard-wrapping function", "(procedure-arity-includes/c 2)", 0, -1, vals);
          }
        }

        if (!init_guard) {
          if (!(SCHEME_CHAPERONE_FLAGS(px) & SCHEME_CHAPERONE_IS_IMPERSONATOR)) {
            for (i = 0; i < argc; i++) {
              if (!scheme_chaperone_of(vals[i], argv[i]))
                scheme_wrong_chaperoned(name, "value", argv[i], vals[i]);
            }
          }
        }

        argv = vals;
      }

      init_guard = NULL;
    } else {
      return vals;
    }
  }
}

static Scheme_Object **chaperone_do_prompt_handler(Scheme_Object *obj, int argc, Scheme_Object **argv)
{
  return chaperone_do_control("call-with-continuation-prompt", 0, NULL, obj, argc, argv);
}

static Scheme_Object **chaperone_do_abort(Scheme_Object *obj, int argc, Scheme_Object **argv)
{
  return chaperone_do_control("abort-current-continuation", 1, NULL, obj, argc, argv);
}

static Scheme_Object **chaperone_do_cc_guard(Scheme_Object *cc_guard, Scheme_Object *obj, int argc, Scheme_Object **argv)
{
  return chaperone_do_control("call-with-continuation-prompt", 2, cc_guard, obj, argc, argv);
}

static Scheme_Object *chaperone_wrap_cc_guard(Scheme_Object *obj, Scheme_Object *proc)
{
  Scheme_Object *a[1], **a2;

  a[0] = proc;
  a2 = chaperone_do_control("call-with-current-continuation", 3, NULL, obj, 1, a);

  return a2[0];
}

static Scheme_Object *do_cc_guard(Scheme_Object *v, Scheme_Object *cc_guard, Scheme_Object *chaperone)
{
  int argc;
  Scheme_Object **argv, *a[1];

  if (v == SCHEME_MULTIPLE_VALUES) {
    GC_CAN_IGNORE Scheme_Thread *p = scheme_current_thread;
    if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
      p->values_buffer = NULL;
    argc = p->ku.multiple.count;
    argv = p->ku.multiple.array;
    p->ku.multiple.array = NULL;
  } else {
    a[0] = v;
    argv = a;
    argc = 1;
  }
  
  if (!chaperone) chaperone = scheme_default_prompt_tag;

  argv = chaperone_do_cc_guard(cc_guard, chaperone, argc, argv);

  if (argc == 1)
    return argv[0];
  else
    return scheme_values(argc, argv);
}

static Scheme_Object *call_with_prompt (int in_argc, Scheme_Object *in_argv[])
{
  Scheme_Object *v;
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *proc = in_argv[0], *prompt_tag;
  Scheme_Prompt *prompt;
  int argc, handler_argument_error = 0;
# define QUICK_PROMPT_ARGS 3
  Scheme_Object **argv, *a[QUICK_PROMPT_ARGS], *handler;
  Scheme_Cont_Frame_Data cframe;
  Scheme_Dynamic_Wind *prompt_dw;
  int cc_count = scheme_cont_capture_count;
  Scheme_Object *chaperone = NULL, *cc_guard = scheme_false;

  argc = in_argc - 3;
  if (argc <= 0) {
    argc = 0;
    argv = NULL;
  } else {
    int i;
    if (argc <= QUICK_PROMPT_ARGS)
      argv = a;
    else
      argv = MALLOC_N(Scheme_Object *, argc);
    for (i = 0; i < argc; i++) {
      argv[i] = in_argv[i+3];
    }
  }

  scheme_check_proc_arity("call-with-continuation-prompt", argc, 0, in_argc, in_argv);
  if (in_argc > 1) {
    /* Check if the prompt tag is proxied */
    if (!SCHEME_PROMPT_TAGP(in_argv[1])) {
      if (SCHEME_NP_CHAPERONEP(in_argv[1])
          && SCHEME_PROMPT_TAGP(SCHEME_CHAPERONE_VAL(in_argv[1]))) {
        chaperone = in_argv[1];
        prompt_tag = SCHEME_CHAPERONE_VAL(in_argv[1]);
      } else {
        scheme_wrong_contract("call-with-continuation-prompt", "continuation-prompt-tag?",
                          1, in_argc, in_argv);
        return NULL;
      }
    } else
      prompt_tag = in_argv[1];
  } else
    prompt_tag = scheme_default_prompt_tag;

  if (in_argc > 2) {
    if (SCHEME_TRUEP(in_argv[2]) && !SCHEME_PROCP(in_argv[2]))
      scheme_wrong_contract("call-with-continuation-prompt", "(or/c procedure? #f)", 2, in_argc, in_argv);
    handler = in_argv[2];
  } else
    handler = scheme_false;

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

    /* An abuse of the continuation-mark stack: to keep track of
       chaperoning guards on a continuation result, we mutate a mark
       that is keyed on prompt_cc_mark_key and that sits next to the
       prompt mark. This is an abuse, because marks are not supposed
       to be mutable, but we do that to keep the mark setting attached
       to a continuation (given that continuation marks are copied out
       and in for a saved and restored continuation). We don't run
       afoul of caching, which depends on immuatbility of marks,
       because we access the mark only by get_set_cont_mark_by_pos(). */

    scheme_push_continuation_frame(&cframe);
    scheme_set_cont_mark(prompt_cc_guard_key, cc_guard); /* see "abuse" note above */
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
    if (chaperone)
      prompt->has_chaperone = 1;

    v = scheme_apply_for_prompt(prompt, prompt_tag, proc, argc, argv);

    /* >> An escape can jump directly here, instead of going through the
       usual escape chain of setjmps. That means we need to reset everything,
       such as the runstack pointer. The information we need is in the
       prompt record. */

    p = scheme_current_thread;

    if (v == SCHEME_MULTIPLE_VALUES) {
      if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
        p->values_buffer = NULL;
    }

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
            if (v == SCHEME_MULTIPLE_VALUES) {
              if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
                p->values_buffer = NULL;
            }
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
          /* cancel any pending cc_guard: */
          get_set_cont_mark_by_pos(prompt_cc_guard_key, p, NULL, MZ_CONT_MARK_POS, scheme_false);

          argc = p->cjs.num_vals;

          if (argc == 1) {
            a[0] = p->cjs.val;
            argv = a;
          } else
            argv = (Scheme_Object **)p->cjs.val;

          reset_cjs(&p->cjs);

          /*
           * If the prompt tag is proxied, run the intercession function
           * and call the handler on its results
           */
          if (chaperone) {
            argv = chaperone_do_prompt_handler(chaperone, argc, argv);
          }

          if (SAME_OBJ(handler, scheme_values_func)) {
            v = scheme_values(argc, argv);
            if (v == SCHEME_MULTIPLE_VALUES) {
              if (SAME_OBJ(p->ku.multiple.array, p->values_buffer))
                p->values_buffer = NULL;
            }
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

    cc_guard = get_set_cont_mark_by_pos(prompt_cc_guard_key, p, NULL, MZ_CONT_MARK_POS, NULL);
    if (SCHEME_FALSEP(cc_guard)) cc_guard = NULL;

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
  } else if (cc_guard) {
    if (SAME_OBJ(cc_guard, scheme_values_func))
      cc_guard = NULL;
    if (cc_guard || chaperone)
      return do_cc_guard(v, cc_guard, chaperone);
    else
      return v;
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
    intptr_t findpos, bottom, pos;
    int count, mcount, i;

    p->meta_continuation = mc->next;

    bottom = (intptr_t)p->cont_mark_stack_bottom;
    count = 0;
    for (findpos = (intptr_t)MZ_CONT_MARK_STACK - 1; findpos >= bottom; findpos--) {
      GC_CAN_IGNORE Scheme_Cont_Mark *seg;

      seg = p->cont_mark_stack_segments[findpos >> SCHEME_LOG_MARK_SEGMENT_SIZE];
      pos = findpos & SCHEME_MARK_SEGMENT_MASK;
      if (seg[pos].pos != MZ_CONT_MARK_POS)
        break;
      count++;
    }
    mcount = 0;
    for (findpos = (intptr_t)mc->cont_mark_total; findpos--; ) {
      if (mc->cont_mark_stack_copied[findpos].pos != mc->cont_mark_pos)
        break;
      mcount++;
    }

    cm_info = scheme_make_vector((count + mcount) * 2, NULL);
    for (findpos = (intptr_t)MZ_CONT_MARK_STACK - 1, i = 0; i < count; findpos--, i++) {
      GC_CAN_IGNORE Scheme_Cont_Mark *seg;

      seg = p->cont_mark_stack_segments[findpos >> SCHEME_LOG_MARK_SEGMENT_SIZE];
      pos = findpos & SCHEME_MARK_SEGMENT_MASK;
      SCHEME_VEC_ELS(cm_info)[2*i] = seg[pos].key;
      SCHEME_VEC_ELS(cm_info)[(2*i)+1] = seg[pos].val;
    }
    for (findpos = (intptr_t)mc->cont_mark_total - 1, i = 0; i < mcount; findpos--, i++) {
      SCHEME_VEC_ELS(cm_info)[2*(count + i)] = mc->cont_mark_stack_copied[findpos].key;
      SCHEME_VEC_ELS(cm_info)[(2*(count + i))+1] = mc->cont_mark_stack_copied[findpos].val;
    }

    p->cjs.jumping_to_continuation = cm_info; /* vector => trampoline */
    p->cjs.alt_full_continuation = NULL;
    p->cjs.val = (Scheme_Object *)cont;
    p->cjs.num_vals = 1;
    p->cjs.is_escape = 1;
    p->cjs.skip_dws = 0;

    p->stack_start = mc->overflow->stack_start;
    p->decompose_mc = mc;

    scheme_longjmpup(&mc->overflow->jmp->cont);
    return NULL;
  } else if (mc && mc->meta_tail_pos == MZ_CONT_MARK_POS) {
    empty_to_next_mc = 1;
  } else {
    empty_to_next_mc = 0;
  }

  /* Clear to avoid retaining a chain of meta-continuationss: */
  mc = NULL;

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

static Scheme_Object *do_abort_continuation (int argc, Scheme_Object *argv[], int skip_dws)
{
  Scheme_Object *prompt_tag;
  Scheme_Prompt *prompt;
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *a[1];
  Scheme_Object **vals;
  int is_chaperoned = 0;

  if (!SCHEME_PROMPT_TAGP(argv[0])) {
    /* Check if the prompt tag is proxied */
    if (SCHEME_NP_CHAPERONEP(argv[0])
        && SCHEME_PROMPT_TAGP(SCHEME_CHAPERONE_VAL(argv[0]))) {
      is_chaperoned = 1;
      prompt_tag = SCHEME_CHAPERONE_VAL(argv[0]);
    } else {
      scheme_wrong_contract("abort-current-continuation", "continuation-prompt-tag?",
                        0, argc, argv);
      return NULL;
    }
  } else
    prompt_tag = argv[0];

  prompt = (Scheme_Prompt *)scheme_extract_one_cc_mark(NULL, SCHEME_PTR_VAL(prompt_tag));
  if (!prompt && SAME_OBJ(scheme_default_prompt_tag, prompt_tag))
    prompt = original_default_prompt;

  if (!prompt) {
    scheme_contract_error("abort-current-continuation", 
                          "continuation includes no prompt with the given tag",
                          "tag", 1, prompt_tag,
                          NULL);
    return NULL;
  }

  if (argc == 2) {
    p->cjs.num_vals = 1;
    /*
     * If the prompt tag isn't proxied, continue with the aborted value.
     * Otherwise, run the intercession function and then continue with its
     * new results.
     */
    if (!is_chaperoned)
      p->cjs.val = argv[1];
    else {
      a[0] = argv[1];
      vals = chaperone_do_abort(argv[0], 1, a);
      p->cjs.val = (Scheme_Object *)vals[0];
    }
  } else {
    int i;
    vals = MALLOC_N(Scheme_Object *, argc - 1);
    for (i = argc; i-- > 1; ) {
      vals[i-1] = argv[i];
    }
    p->cjs.num_vals = argc - 1;
    if (!is_chaperoned)
      p->cjs.val = (Scheme_Object *)vals;
    else {
      vals = chaperone_do_abort(argv[0], argc - 1, vals);
      p->cjs.val = (Scheme_Object *)vals;
    }
  }
  p->cjs.jumping_to_continuation = (Scheme_Object *)prompt;
  p->cjs.alt_full_continuation = NULL;
  p->cjs.skip_dws = skip_dws;

  scheme_longjmp(*p->error_buf, 1);

  return NULL;
}

static Scheme_Object *abort_continuation (int argc, Scheme_Object *argv[])
{
  return do_abort_continuation(argc, argv, 0);
}

Scheme_Object *scheme_abort_continuation_no_dws (Scheme_Object *pt, Scheme_Object *v)
{
  /* This function is useful for GRacket-like extensions of Racket that need to
     implement something like subtreads through composable continuations. */
  Scheme_Object *a[2];

  a[0] = pt;
  a[1] = v;

  return do_abort_continuation(2, a, 1);
}

static Scheme_Object *do_call_with_control (int argc, Scheme_Object *argv[], int no_dws)
{
  Scheme_Object *prompt_tag;
  Scheme_Object *a[3];

  scheme_check_proc_arity("call-with-composable-continuation", 1, 0, argc, argv);
  if (argc > 1) {
    prompt_tag = argv[1];
    if (!SAME_TYPE(scheme_prompt_tag_type, SCHEME_TYPE(prompt_tag))) {
      if (SCHEME_NP_CHAPERONEP(prompt_tag)
          && SCHEME_PROMPT_TAGP(SCHEME_CHAPERONE_VAL(prompt_tag)))
        prompt_tag = SCHEME_CHAPERONE_VAL(prompt_tag);
      else {
        scheme_wrong_contract("call-with-composable-continuation", "continuation-prompt-tag?",
                          1, argc, argv);
        return NULL;
      }
    } 
  } else
    prompt_tag = scheme_default_prompt_tag;

  a[0] = argv[0];
  a[1] = prompt_tag;
  a[2] = (no_dws ? scheme_true : scheme_false);

  /* Trampoline to internal_call_cc. This trampoline ensures that
     the runstack is flushed before we try to grab the continuation. */
  return _scheme_tail_apply(internal_call_cc_prim, 3, a);
}

static Scheme_Object *call_with_control (int argc, Scheme_Object *argv[])
{
  return do_call_with_control(argc, argv, 0);
}

Scheme_Object *scheme_call_with_composable_no_dws (Scheme_Object *proc, Scheme_Object *pt)
{
  /* Works with scheme_abort_continuation_no_dws() above. */
  Scheme_Object *a[2];

  a[0] = proc;
  a[1] = pt;

  return do_call_with_control(2, a, 1);
}

static Scheme_Cont_Mark *copy_cm_shared_on_write(Scheme_Meta_Continuation *mc)
{
  Scheme_Cont_Mark *cp;

  cp = MALLOC_N(Scheme_Cont_Mark, mc->cont_mark_total);
  memcpy(cp, mc->cont_mark_stack_copied, mc->cont_mark_total * sizeof(Scheme_Cont_Mark));
  mc->cont_mark_stack_copied = cp;
  mc->cm_shared = 0;

  return cp;
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
  intptr_t findpos, bottom;
  intptr_t cmpos, first_cmpos = 0, cdelta = 0;
  int found_tag = 0, at_mc_boundary = 0;

  if (cont && SAME_OBJ(cont->prompt_tag, prompt_tag))
    found_tag = 1;
  if (!prompt_tag)
    found_tag = 1;

  do {
    if (econt) {
      findpos = (intptr_t)((Scheme_Escaping_Cont *)econt)->envss.cont_mark_stack;
      cmpos = (intptr_t)((Scheme_Escaping_Cont *)econt)->envss.cont_mark_pos;
      if (mc) {
        cdelta = mc->cont_mark_offset;
        bottom = 0;
      } else
        bottom = p->cont_mark_stack_bottom;
    } else if (cont) {
      findpos = (intptr_t)cont->ss.cont_mark_stack;
      cmpos = (intptr_t)cont->ss.cont_mark_pos;
      cdelta = cont->cont_mark_offset;
      bottom = 0;
    } else if (mc) {
      findpos = (intptr_t)mc->cont_mark_stack;
      cmpos = (intptr_t)mc->cont_mark_pos;
      cdelta = mc->cont_mark_offset;
      bottom = 0;
      at_mc_boundary = 1;
    } else {
      findpos = (intptr_t)MZ_CONT_MARK_STACK;
      cmpos = (intptr_t)MZ_CONT_MARK_POS;
      if (!p->cont_mark_stack_segments)
        findpos = 0;
      bottom = p->cont_mark_stack_bottom;
    }

    top_cont = cont;

    while (findpos-- > bottom) {
      Scheme_Cont_Mark *find;
      intptr_t pos;

      if (cont) {
        while (findpos < cdelta) {
          if (!cont->runstack_copied) {
            /* Current cont was just a mark-stack variation of
               next cont, so skip the next cont. */
            cont = cont->buf_ptr->buf.cont;
          }
          cont = cont->buf_ptr->buf.cont;
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
          if (last) {
            last->next = (Scheme_Cont_Mark_Chain *)cache;
            if (at_mc_boundary) {
              SCHEME_MARK_CHAIN_FLAG(last) |= 0x1;
              at_mc_boundary = 0;
            }
          } else {
            first = (Scheme_Cont_Mark_Chain *)cache;
            first_cmpos = cmpos;
          }
          
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
        pr->iso.so.type = scheme_cont_mark_chain_type;
        pr->key = find[pos].key;
        pr->val = find[pos].val;
        pr->pos = find[pos].pos;
        pr->next = NULL;
        if (mc) {
          if (mc->cm_shared)
            find = copy_cm_shared_on_write(mc);
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
        if (last) {
          last->next = pr;
          if (at_mc_boundary) {
            SCHEME_MARK_CHAIN_FLAG(last) |= 1;
            at_mc_boundary = 0;
          }
        } else {
          first = pr;
          first_cmpos = cmpos;
        }

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
      scheme_contract_error(who,
                            "no corresponding prompt in the continuation",
                            "tag", 1, prompt_tag,
                            NULL);
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

  if (first && (first_cmpos < first->pos)) {
    /* Don't use scheme_signal_error(), because that will try to get
       a continuation mark... */
    scheme_log_abort("internal error: bad mark-stack position");
    abort();
  }

  set = MALLOC_ONE_TAGGED(Scheme_Cont_Mark_Set);
  set->so.type = scheme_cont_mark_set_type;
  set->chain = first;
  set->cmpos = first_cmpos;
  set->native_stack_trace = nt;

  return (Scheme_Object *)set;
}

static Scheme_Object *make_empty_marks()
{
  /* empty marks */
  Scheme_Cont_Mark_Set *set;
  
  set = MALLOC_ONE_TAGGED(Scheme_Cont_Mark_Set);
  set->so.type = scheme_cont_mark_set_type;
  set->chain = NULL;
  set->cmpos = 1;
  set->native_stack_trace = NULL;
  
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
  Scheme_Object *prompt_tag;

  if (argc) {
    prompt_tag = argv[0];
    if (!SAME_TYPE(scheme_prompt_tag_type, SCHEME_TYPE(prompt_tag))) {
      if (SCHEME_NP_CHAPERONEP(prompt_tag)
          && SCHEME_PROMPT_TAGP(SCHEME_CHAPERONE_VAL(prompt_tag)))
        prompt_tag = SCHEME_CHAPERONE_VAL(prompt_tag);
      else
        scheme_wrong_contract("current-continuation-marks", "continuation-prompt-tag?",
                          0, argc, argv);
    }

    if (!SAME_OBJ(scheme_default_prompt_tag, prompt_tag))
      if (!scheme_extract_one_cc_mark(NULL, SCHEME_PTR_VAL(prompt_tag)))
        scheme_contract_error("current-continuation-marks",
                              "no corresponding prompt in the continuation",
                              "prompt tag", 1, prompt_tag,
                              NULL);
  }

  return scheme_current_continuation_marks(argc ? prompt_tag : NULL);
}

static Scheme_Object *
cont_marks(int argc, Scheme_Object *argv[])
{
  Scheme_Object *prompt_tag;

  if (SCHEME_TRUEP(argv[0])
      && !SCHEME_CONTP(argv[0]) && !SCHEME_ECONTP(argv[0]) && !SCHEME_THREADP(argv[0]))
    scheme_wrong_contract("continuation-marks", "(or/c continuation? thread? #f)", 0, argc, argv);

  if (argc > 1) {
    if (!SAME_TYPE(scheme_prompt_tag_type, SCHEME_TYPE(argv[1]))) {
      if (SCHEME_NP_CHAPERONEP(argv[1])
          && SCHEME_PROMPT_TAGP(SCHEME_CHAPERONE_VAL(argv[1])))
        prompt_tag = SCHEME_CHAPERONE_VAL(argv[1]);
      else {
        scheme_wrong_contract("continuation-marks", "continuation-prompt-tag?",
                              1, argc, argv);
        return NULL;
      }
    } else
      prompt_tag = argv[1];
  } else
    prompt_tag = scheme_default_prompt_tag;

  if (SCHEME_FALSEP(argv[0])) {
    return make_empty_marks();
  } else if (SCHEME_ECONTP(argv[0])) {
    if (!scheme_escape_continuation_ok(argv[0])) {
      scheme_contract_error("continuation-marks",
                            "escape continuation not in the current thread's continuation",
                            "escape continuation", 1, argv[0],
                            NULL);
      return NULL;
    } else {
      Scheme_Meta_Continuation *mc;
      mc = scheme_get_meta_continuation(argv[0]);

      return continuation_marks(scheme_current_thread, NULL, argv[0], mc, prompt_tag, 
                                "continuation-marks", 0);
    }
  } else if (SCHEME_THREADP(argv[0])) {
    Scheme_Thread *t = (Scheme_Thread *)argv[0];
    Scheme_Object *m;

    while (t->nestee) {
      t = t->nestee;
    }

    if (SAME_OBJ(t, scheme_current_thread))
      return scheme_current_continuation_marks(prompt_tag);

    while (t->return_marks_to) {
      scheme_thread_block(0.0);
    }

    if (!(t->running & MZTHREAD_RUNNING)) {
      return make_empty_marks();
    } else {
      scheme_start_atomic(); /* just in case */

      t->return_marks_to = scheme_current_thread;
      t->returned_marks = prompt_tag;
      scheme_swap_thread(t);
      
      m = t->returned_marks;
      t->returned_marks = NULL;
      
      scheme_end_atomic_no_swap();

      return m;
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
  Scheme_Object *v;
  Scheme_Object *pr;
  int is_chaperoned = 0;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_cont_mark_set_type)) {
    scheme_wrong_contract("continuation-mark-set->list", "continuation-mark-set?", 0, argc, argv);
    return NULL;
  }
  if (argc > 2) {
    if (!SAME_TYPE(scheme_prompt_tag_type, SCHEME_TYPE(argv[2]))) {
      if (SCHEME_NP_CHAPERONEP(argv[2])
          && SCHEME_PROMPT_TAGP(SCHEME_CHAPERONE_VAL(argv[2])))
        prompt_tag = SCHEME_CHAPERONE_VAL(argv[2]);
      else {
        scheme_wrong_contract("continuation-mark-set->list", "continuation-prompt-tag?",
                          2, argc, argv);
        return NULL;
      }
    } else
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

  if (SCHEME_NP_CHAPERONEP(key)
      && SCHEME_CONTINUATION_MARK_KEYP(SCHEME_CHAPERONE_VAL(key))) {
    is_chaperoned = 1;
    key = SCHEME_CHAPERONE_VAL(key);
  }

  prompt_tag = SCHEME_PTR_VAL(prompt_tag);

  while (chain) {
    if (chain->key == key) {
      if (is_chaperoned)
        v = scheme_chaperone_do_continuation_mark("continuation-mark-set->list",
                                                  1, argv[1], chain->val);
      else
        v = chain->val;
      pr = scheme_make_pair(v, scheme_null);
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
  intptr_t last_pos;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_cont_mark_set_type)) {
    scheme_wrong_contract("continuation-mark-set->list*", "continuation-mark-set?", 0, argc, argv);
    return NULL;
  }
  len = scheme_proper_list_length(argv[1]);
  if (len < 0) {
    scheme_wrong_contract("continuation-mark-set->list*", "list?", 1, argc, argv);
    return NULL;
  }
  if (argc > 2)
    none = argv[2];
  else
    none = scheme_false;
  if (argc > 3) {
    if (!SAME_TYPE(scheme_prompt_tag_type, SCHEME_TYPE(argv[3]))) {
      if (SCHEME_NP_CHAPERONEP(argv[3])
          && SCHEME_PROMPT_TAGP(SCHEME_CHAPERONE_VAL(argv[3])))
        prompt_tag = SCHEME_CHAPERONE_VAL(argv[3]);
      else {
        scheme_wrong_contract("continuation-mark-set->list*", "continuation-prompt-tag?",
                              3, argc, argv);
        return NULL;
      }
    } else
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
  last_pos = -1;

  while (chain) {
    for (i = 0; i < len; i++) {
      int is_chaperoned = 0;
      Scheme_Object *orig_key, *val;

      if (SCHEME_MARK_CHAIN_FLAG(chain) & 0x1)
        last_pos = -1;
      if (SCHEME_NP_CHAPERONEP(keys[i])
          && SCHEME_CONTINUATION_MARK_KEYP(SCHEME_CHAPERONE_VAL(keys[i]))) {
        is_chaperoned = 1;
        orig_key = keys[i];
        keys[i] = SCHEME_CHAPERONE_VAL(orig_key);
      } else
        orig_key = NULL;
      if (SAME_OBJ(chain->key, keys[i])) {
	intptr_t pos;
	pos = (intptr_t)chain->pos;
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
        if (is_chaperoned) {
          val = scheme_chaperone_do_continuation_mark("continuation-mark-set->list*",
                                                      1, orig_key, chain->val);
          SCHEME_VEC_ELS(vals)[i] = val;
        } else
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
    } else if (SCHEME_PAIRP(name) && SCHEME_RMPP(SCHEME_CAR(name))) {
      /* a resolved module path means that we're running a module body */
      const char *what;

      if (SCHEME_FALSEP(SCHEME_CDR(name)))
        what = "[traversing imports]";
      else if (SCHEME_VOIDP(SCHEME_CDR(name)))
        what = "[running expand-time body]";
      else
        what = "[running body]";

      name = SCHEME_CAR(name);
      name = SCHEME_PTR_VAL(name);
      if (SCHEME_PAIRP(name))
        name = scheme_make_pair(scheme_intern_symbol("submod"), name);
      loc = scheme_make_location(name, scheme_false, 
                                 scheme_false, scheme_false, scheme_false);

      name = scheme_intern_symbol(what);
      name = scheme_make_pair(name, loc);
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
    scheme_wrong_contract("continuation-mark-set->context", "continuation-mark-set?", 0, argc, argv);
    return NULL;
  }

  return scheme_get_stack_trace(argv[0]);
}

static Scheme_Object *
scheme_extract_one_cc_mark_with_meta(Scheme_Object *mark_set, Scheme_Object *key_arg, 
                                     Scheme_Object *prompt_tag, Scheme_Meta_Continuation **_meta,
                                     MZ_MARK_POS_TYPE *_vpos)
{
  Scheme_Object *key = key_arg;
  if (SCHEME_NP_CHAPERONEP(key)
      && SCHEME_CONTINUATION_MARK_KEYP(SCHEME_CHAPERONE_VAL(key))) {
    key = SCHEME_CHAPERONE_VAL(key);
  }

  if (mark_set) {
    Scheme_Cont_Mark_Chain *chain;
    chain = ((Scheme_Cont_Mark_Set *)mark_set)->chain;
    while (chain) {
      if (chain->key == key)
        if (key_arg != key)
          /*
           * TODO: is this the only name that this procedure is called as
           * publicly?
           */
          return scheme_chaperone_do_continuation_mark("continuation-mark-set-first",
                                                       1, key_arg, chain->val);
        else
	  return chain->val;
      else if (SAME_OBJ(chain->key, prompt_tag))
        break;
      else 
	chain = chain->next;
    }
  } else {
    intptr_t findpos, bottom, startpos;
    intptr_t pos;
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
        startpos = (intptr_t)MZ_CONT_MARK_STACK;
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
        } else if (SAME_OBJ(seg[pos].key, prompt_tag)) {
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

      if (key_arg != key && val != NULL)
        val = scheme_chaperone_do_continuation_mark("continuation-mark-set-first", 1, key_arg, val);

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

static Scheme_Object *get_set_cont_mark_by_pos(Scheme_Object *key,
                                               Scheme_Thread *p,
                                               Scheme_Meta_Continuation *mc,
                                               MZ_MARK_POS_TYPE mpos,
                                               Scheme_Object *val)
{
  intptr_t findpos, bottom, startpos;
  intptr_t pos;
  int down_delta = 0;
  Scheme_Cont_Mark *seg;

  if (mc) {
    startpos = mc->cont_mark_total;
    bottom = 0;
  } else {
    startpos = (intptr_t)MZ_CONT_MARK_STACK;
    if (!p->cont_mark_stack_segments)
      findpos = 0;
    bottom = p->cont_mark_stack_bottom;
  }
  
  findpos = startpos;

  /* binary search: */
  while (bottom < startpos) {
    findpos = ((bottom + startpos) / 2) - down_delta;

    if (mc) {
      seg = mc->cont_mark_stack_copied;
      pos = findpos;
    } else {
      seg = p->cont_mark_stack_segments[findpos >> SCHEME_LOG_MARK_SEGMENT_SIZE];
      pos = findpos & SCHEME_MARK_SEGMENT_MASK;
    }

    if (seg[pos].pos == mpos) {
      if (SAME_OBJ(seg[pos].key, key)) {
        if (!val)
          return seg[pos].val;
        
        if (mc && mc->cm_shared)
          seg = copy_cm_shared_on_write(mc);
        
        seg[pos].val = val;
        
        return scheme_void;
      } else if (findpos > bottom) {
        down_delta++;
      } else {
        bottom = (findpos + down_delta) + 1;
        down_delta = 0;
      }
    } else if (seg[pos].pos < mpos) {
      bottom = findpos + 1;
    } else {
      startpos = findpos;
    }
  }

  scheme_signal_error("get_set_cont_mark_by_pos: key not found");
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
    scheme_wrong_contract("continuation-mark-set-first", "(or/c continuation-mark-set? #f)", 0, argc, argv);
  
  if ((argv[1] == scheme_parameterization_key)
      || (argv[1] == scheme_break_enabled_key)) {
    /* Minor hack: these keys are used in "startup.rkt" to access
       parameterizations, and we want that access to go through
       prompts. If they keys somehow leaked, it's ok, because that
       doesn't expose anything that isn't already exposed by functions
       like `current-parameterization'. */
    prompt_tag = NULL; 
  } else
    prompt_tag = scheme_default_prompt_tag;

  if (argc > 3) {
    if (!SAME_TYPE(scheme_prompt_tag_type, SCHEME_TYPE(argv[3]))) {
      if (SCHEME_NP_CHAPERONEP(argv[3])
          && SCHEME_PROMPT_TAGP(SCHEME_CHAPERONE_VAL(argv[3])))
        prompt_tag = SCHEME_CHAPERONE_VAL(argv[3]);
      else
        scheme_wrong_contract("continuation-mark-set-first", "continuation-prompt-tag?",
                          3, argc, argv);
    } else
      prompt_tag = argv[3];

    if (!SAME_OBJ(scheme_default_prompt_tag, prompt_tag)) {
      if (SCHEME_FALSEP(argv[0])) {
        if (!scheme_extract_one_cc_mark(NULL, SCHEME_PTR_VAL(prompt_tag)))
          scheme_contract_error("continuation-mark-set-first",
                                "no corresponding prompt in the current continuation",
                                "tag", 1, prompt_tag,
                                NULL);
      }
    }
  } 

  r = scheme_extract_one_cc_mark_to_tag(SCHEME_TRUEP(argv[0]) ? argv[0] : NULL, argv[1], 
                                        prompt_tag ? SCHEME_PTR_VAL(prompt_tag) : NULL);
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
    if (SCHEME_NP_CHAPERONEP(prompt_tag)
        && SCHEME_PROMPT_TAGP(SCHEME_CHAPERONE_VAL(prompt_tag)))
      prompt_tag = SCHEME_CHAPERONE_VAL(prompt_tag);
    else
      scheme_wrong_contract("continuation-prompt-available?", "continuation-prompt-tag?",
                        0, argc, argv);
  }

  if (argc > 1) {
    if (SCHEME_ECONTP(argv[1])) {
      if (!scheme_escape_continuation_ok(argv[1])) {
        scheme_contract_error("continuation-prompt-available?",
                              "escape continuation not in the current thread's continuation",
                              "escape continuation", 1, argv[1],
                              NULL);
        return NULL;
      } else {
        Scheme_Meta_Continuation *mc;

        if (SAME_OBJ(scheme_default_prompt_tag, prompt_tag))
          return scheme_true;

        mc = scheme_get_meta_continuation(argv[1]);
        
        if (continuation_marks(scheme_current_thread, NULL, argv[1], mc, prompt_tag, 
                               NULL, 0))
          return scheme_true;
      }
    } else if (SCHEME_CONTP(argv[1])) {
      if (continuation_marks(NULL, argv[1], NULL, NULL, prompt_tag, NULL, 0))
        return scheme_true;
    } else {
      scheme_wrong_contract("continuation-prompt-available?", "continuation?",
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
/*                        lightweight continuations                       */
/*========================================================================*/

/* A lightweight continuation is one that contains only frames from
   JIT-generated code. The code here manages capture and restore for
   the runstack and mark stack, while the rest is in the JIT. */

#ifdef MZ_USE_JIT

struct Scheme_Lightweight_Continuation {
  MZTAG_IF_REQUIRED /* scheme_rt_lightweight_cont */
  Scheme_Current_LWC *saved_lwc;
  void *stack_slice;
  Scheme_Object **runstack_slice;
  Scheme_Cont_Mark *cont_mark_stack_slice;
  void *stored1, *stored2;
};

void scheme_init_thread_lwc(void) XFORM_SKIP_PROC
{
  scheme_current_lwc = (Scheme_Current_LWC *)malloc(sizeof(Scheme_Current_LWC));
}

void scheme_fill_lwc_start(void) XFORM_SKIP_PROC
{
  scheme_current_lwc->runstack_start = MZ_RUNSTACK;
  scheme_current_lwc->cont_mark_stack_start = MZ_CONT_MARK_STACK;
  scheme_current_lwc->cont_mark_pos_start = MZ_CONT_MARK_POS;
}

void scheme_fill_lwc_end(void) XFORM_SKIP_PROC
{
  scheme_current_lwc->runstack_end = MZ_RUNSTACK;
  scheme_current_lwc->cont_mark_stack_end = MZ_CONT_MARK_STACK;
  scheme_current_lwc->cont_mark_pos_end = MZ_CONT_MARK_POS;
  scheme_fill_stack_lwc_end();
}

void scheme_clear_lwc(void) XFORM_SKIP_PROC
{
}

Scheme_Lightweight_Continuation *scheme_capture_lightweight_continuation(Scheme_Thread *p,
                                                                         Scheme_Current_LWC *p_lwc,
                                                                         void **storage)
  XFORM_SKIP_PROC
/* This function explicitly cooperates with the GC by storing the
   pointers it needs to save across a collection in `storage'. Also,
   if allocation fails, it can abort and return NULL. The combination
   allows it to work in a thread for running futures (where allocation
   and GC in general ae disallowed). */
{
  intptr_t len, i, j, pos;
  Scheme_Object **runstack_slice;
  Scheme_Cont_Mark *cont_mark_stack_slice;
  Scheme_Current_LWC *lwc;
  Scheme_Cont_Mark *seg;
  Scheme_Lightweight_Continuation *lw;
  void *stack;

#ifndef MZ_PRECISE_GC
  return NULL;
#endif

  storage[1] = p;

  lw = MALLOC_ONE_RT(Scheme_Lightweight_Continuation);
  if (!lw) return NULL;
#ifdef MZTAG_REQUIRED
  lw->type = scheme_rt_lightweight_cont;
#endif

  storage[0] = lw;

  lwc = (Scheme_Current_LWC *)scheme_malloc_atomic(sizeof(Scheme_Current_LWC));
  if (!lwc) return NULL;

  memcpy(lwc, p_lwc, sizeof(Scheme_Current_LWC));

  lw = (Scheme_Lightweight_Continuation *)storage[0];
  lw->saved_lwc = lwc;

  stack = scheme_save_lightweight_continuation_stack(p_lwc);
  if (!stack) return NULL;

  lw = (Scheme_Lightweight_Continuation *)storage[0];
  lw->stack_slice = stack;
  lwc = lw->saved_lwc;

  len = lwc->runstack_start - lwc->runstack_end;
  runstack_slice = MALLOC_N(Scheme_Object*, len);
  if (!runstack_slice) return NULL;

  lw = (Scheme_Lightweight_Continuation *)storage[0];
  lwc = lw->saved_lwc;
  lw->runstack_slice = runstack_slice;
  memcpy(runstack_slice, lw->saved_lwc->runstack_end, len * sizeof(Scheme_Object *));

  /* The runstack may contain pointers to itself, but they are just 
     cleared slots where a register containing the runstack pointer
     was handy; zero out such slots to avoid retaining a runstack
     unnecessarily: */
  for (i = 0; i < len; i++) {
    if (((uintptr_t)runstack_slice[i] >= (uintptr_t)lwc->runstack_end)
        && ((uintptr_t)runstack_slice[i] <= (uintptr_t)lwc->runstack_start))
      runstack_slice[i] = NULL;
  }

  len = lwc->cont_mark_stack_end - lwc->cont_mark_stack_start;

  if (len) {
    cont_mark_stack_slice = MALLOC_N(Scheme_Cont_Mark, len);
    if (!cont_mark_stack_slice) return NULL;
    lw = (Scheme_Lightweight_Continuation *)storage[0];
  } else
    cont_mark_stack_slice = NULL;

  lw->cont_mark_stack_slice = cont_mark_stack_slice;

  lwc = lw->saved_lwc;
  p = (Scheme_Thread *)storage[1];

  for (j = 0; j < len; j++) {
    i = j + lwc->cont_mark_stack_start;

    seg = p->cont_mark_stack_segments[i >> SCHEME_LOG_MARK_SEGMENT_SIZE];
    pos = i & SCHEME_MARK_SEGMENT_MASK;
    
    memcpy(cont_mark_stack_slice + i, seg + pos, sizeof(Scheme_Cont_Mark));
  }

  return lw;
}

Scheme_Object **scheme_adjust_runstack_argument(Scheme_Lightweight_Continuation *lw,
                                                Scheme_Object **arg)
  XFORM_SKIP_PROC
{
  if (arg == lw->saved_lwc->runstack_end)
    return lw->runstack_slice;
  else
    return arg;
}

static void *apply_lwc_k()
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Lightweight_Continuation *lw = (Scheme_Lightweight_Continuation *)p->ku.k.p1;
  Scheme_Object *result = (Scheme_Object *)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return scheme_apply_lightweight_continuation(lw, result, p->ku.k.i1, p->ku.k.i2);
}

static Scheme_Object *can_apply_lwc_k(void);

static int can_apply_lightweight_continuation(Scheme_Lightweight_Continuation *lw, int did_overflow)
{
#ifdef DO_STACK_CHECK
  /* enough room on C stack? */
  uintptr_t size;
  size = (uintptr_t)lw->saved_lwc->stack_start - (uintptr_t)lw->saved_lwc->stack_end;
  
  {
# define SCHEME_PLUS_STACK_DELTA(x) ((x) - size)
# include "mzstkchk.h"
    {
      if (did_overflow)
        return 0;
      else {
        scheme_current_thread->ku.k.p1 = lw;
        if (SCHEME_TRUEP(scheme_handle_stack_overflow(can_apply_lwc_k)))
          return 2;
        else
          return 0;
      }
    }
  }

  return 1;
#else
  return 0;
#endif
}

int scheme_can_apply_lightweight_continuation(Scheme_Lightweight_Continuation *lw, int check_overflow)
/* result value 2 => need to handle stack overflow to have enough room */
{
  if (check_overflow)
    return can_apply_lightweight_continuation(lw, 0);
  else
    /* assume that we can apply the continuation, though 
       overflow handling may be needed (i.e., assume that the runtime
       thread's stack size is > than a future thread's stack) */
    return 1;
}

static Scheme_Object *can_apply_lwc_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Lightweight_Continuation *lwc = (Scheme_Lightweight_Continuation *)p->ku.k.p1;

  p->ku.k.p1 = NULL;

  if (can_apply_lightweight_continuation(lwc, 1))
    return scheme_true;
  else
    return scheme_false;
}

Scheme_Object *scheme_apply_lightweight_continuation(Scheme_Lightweight_Continuation *lw,
                                                     Scheme_Object *result,
                                                     int result_is_rs_argv,
                                                     intptr_t min_stacksize) 
  XFORM_SKIP_PROC
{
  intptr_t len, cm_delta, i, cm;
  Scheme_Object **rs;

  len = lw->saved_lwc->runstack_start - lw->saved_lwc->runstack_end;
 
  if (!scheme_check_runstack(len)
      /* besides making sure that the save slice fits, we need to
         make sure that any advance check on available from the old thread
         still applies in the new thread */
      || ((MZ_RUNSTACK - MZ_RUNSTACK_START) < min_stacksize)) {
    /* This will not happen when restoring a future-thread-captured
       continuation in a future thread. */
    scheme_current_thread->ku.k.p1 = lw;
    scheme_current_thread->ku.k.p2 = result;
    scheme_current_thread->ku.k.i1 = result_is_rs_argv;
    scheme_current_thread->ku.k.i2 = min_stacksize;
    if (len < min_stacksize)
      len = min_stacksize;
    return (Scheme_Object *)scheme_enlarge_runstack(len, apply_lwc_k);
  }

  /* application of a lightweight continuation forms a lightweight continuation: */
  scheme_current_lwc->runstack_start = MZ_RUNSTACK;
  scheme_current_lwc->cont_mark_stack_start = MZ_CONT_MARK_STACK;
  scheme_current_lwc->cont_mark_pos_start = MZ_CONT_MARK_POS + 2;

#ifdef MZ_USE_FUTURES
  jit_future_storage[3] = result;
#endif      
  lw = scheme_restore_lightweight_continuation_marks(lw); /* can trigger GC */
#ifdef MZ_USE_FUTURES
  result = (Scheme_Object *)jit_future_storage[3];
#endif      

  cm_delta = (intptr_t)MZ_CONT_MARK_STACK - (intptr_t)lw->saved_lwc->cont_mark_stack_end;

  rs = MZ_RUNSTACK - len;
  MZ_RUNSTACK = rs;

  memcpy(rs, lw->runstack_slice, len * sizeof(Scheme_Object*));
  
  /* If SCHEME_EVAL_WAITING appears in the runstack slice, it
     indicates that a cm position follows: */
  for (i = 0; i < len; i++) {
    if (rs[i] == SCHEME_EVAL_WAITING) {
      cm = SCHEME_INT_VAL(rs[i+1]);
      cm += cm_delta;
      rs[i+1] = scheme_make_integer(cm);
    }
  }

  if (result_is_rs_argv)
    result = (Scheme_Object *)(rs + 2);

  return scheme_apply_lightweight_continuation_stack(lw->saved_lwc, lw->stack_slice, result);
}

Scheme_Lightweight_Continuation *scheme_restore_lightweight_continuation_marks(Scheme_Lightweight_Continuation *lw)
  XFORM_SKIP_PROC
/* Called by any thread, but this function can trigger a GC in the runtime thread */
{
  intptr_t cm_len, i, cm_pos_delta;
  Scheme_Cont_Mark *seg;

  cm_len = lw->saved_lwc->cont_mark_stack_end - lw->saved_lwc->cont_mark_stack_start;
  cm_pos_delta = MZ_CONT_MARK_POS + 2 - lw->saved_lwc->cont_mark_pos_start;

  if (cm_len) {
    /* install captured continuation marks, adjusting the pos
       to match the new context: */
    seg = lw->cont_mark_stack_slice;
    for (i = 0; i < cm_len; i++) {
      MZ_CONT_MARK_POS = seg[i].pos + cm_pos_delta;
#ifdef MZ_USE_FUTURES
      jit_future_storage[2] = lw;
#endif      
      scheme_set_cont_mark(seg[i].key, seg[i].val); /* can trigger a GC */
#ifdef MZ_USE_FUTURES
      lw = (Scheme_Lightweight_Continuation *)jit_future_storage[2];
#endif      
    }
  }

  MZ_CONT_MARK_POS = lw->saved_lwc->cont_mark_pos_end + cm_pos_delta;

  return lw;
}

int scheme_push_marks_from_lightweight_continuation(Scheme_Lightweight_Continuation *lw, 
                                                    Scheme_Cont_Frame_Data *d)
{
  intptr_t pos, len, delta;
  Scheme_Cont_Mark *seg;

  len = (lw->saved_lwc->cont_mark_stack_end
         - lw->saved_lwc->cont_mark_stack_start);

  if (len) {
    scheme_push_continuation_frame(d);

    seg = lw->cont_mark_stack_slice;

    delta = MZ_CONT_MARK_POS + 2 - lw->saved_lwc->cont_mark_pos_start;
      
    for (pos = 0; pos < len; pos++) {
      MZ_CONT_MARK_POS = seg[pos].pos + delta;
      scheme_set_cont_mark(seg[pos].key, seg[pos].val);
    }

    MZ_CONT_MARK_POS = lw->saved_lwc->cont_mark_pos_end + delta;

    return 1;
  }

  return 0;
}

int scheme_push_marks_from_thread(Scheme_Thread *p2, Scheme_Cont_Frame_Data *d)
{
  intptr_t i, pos, delta;
  Scheme_Cont_Mark *seg;

  if (p2->cont_mark_stack) {
    scheme_push_continuation_frame(d);

    delta = MZ_CONT_MARK_POS - p2->cont_mark_pos;
    if (delta < 0) delta = 0;
      
    for (i = 0; i < p2->cont_mark_stack; i++) {
      seg = p2->cont_mark_stack_segments[i >> SCHEME_LOG_MARK_SEGMENT_SIZE];
      pos = i & SCHEME_MARK_SEGMENT_MASK;

      MZ_CONT_MARK_POS = seg[pos].pos + delta;
      scheme_set_cont_mark(seg[pos].key, seg[pos].val);
    }

    MZ_CONT_MARK_POS = p2->cont_mark_pos + delta;

    return 1;
  }

  return 0;
}

#else

void scheme_init_thread_lwc(void) XFORM_SKIP_PROC { }

#endif

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
  scheme_ignore_result(_scheme_apply_multi(prepost, 0, NULL));

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

  if (pre) {
    ASSERT_SUSPEND_BREAK_ZERO();
    p->suspend_break++;
    pre(data);
    p = scheme_current_thread;
    --p->suspend_break;
  }

  /* set up `dw' after pre(), in case a continuation
     is captured in pre() and composed later */

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

  /* Don't run Racket-based dyn-winds when we're killing a nested thread. */
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
      if (!p->cjs.skip_dws) {
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
          scheme_contract_error("abort-current-continuation", 
                                "abort in progress, but current continuation includes"
                                " no prompt with the given tag"
                                " after a `dynamic-wind' post-thunk return",
                                "tag", 1, tag,
                                NULL);
          return NULL;
        }
        p->cjs.jumping_to_continuation = (Scheme_Object *)prompt;
      } else if (SCHEME_ECONTP(p->cjs.jumping_to_continuation)) {
        if (!scheme_escape_continuation_ok(p->cjs.jumping_to_continuation)) {
          if (p->cjs.alt_full_continuation) {
            /* We were trying to execute a full-continuation jump through
               an escape-continuation jump. Go back to full-jump mode. */
            return jump_to_alt_continuation();
          }
          scheme_raise_exn(MZEXN_FAIL_CONTRACT_CONTINUATION,
                           "continuation application: lost target;\n"
                           " jump to escape continuation in progress, and the target is not in the\n"
                           " current continuation after a `dynamic-wind' post-thunk return");
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
  intptr_t delta;
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
    rest->cont_mark_offset = rest->cont_mark_stack;
    rest->cont_mark_stack_copied = NULL;
    sync_meta_cont(rest);
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
    sync_meta_cont(rest);
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

  if (recheck && !recheck->composable) {
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

static Scheme_Object *jump_to_alt_continuation()
{
  Scheme_Thread *p;
  Scheme_Object *a[1], **args, *fc;

  p = scheme_current_thread;

  a[0] = p->cjs.val;
  fc = p->cjs.alt_full_continuation;
  args = ((p->cjs.num_vals == 1) ? a : (Scheme_Object **)p->cjs.val);
  p->cjs.jumping_to_continuation = NULL;
  p->cjs.alt_full_continuation = NULL;
  p->cjs.val = NULL;
  p->cjs.skip_dws = 0;

  return scheme_jump_to_continuation(fc, p->cjs.num_vals, args, NULL, 0);
}

/*========================================================================*/
/*                                  time                                  */
/*========================================================================*/

#ifdef TIME_SYNTAX

#ifndef CLOCKS_PER_SEC
#define CLOCKS_PER_SEC 1000000
#endif

intptr_t scheme_get_milliseconds(void)
  XFORM_SKIP_PROC
/* this function can be called from any OS thread */
{
#ifdef USE_MACTIME
  return scheme_get_process_milliseconds();
#else
# ifdef USE_FTIME
  struct MSC_IZE(timeb) now;
  MSC_IZE(ftime)(&now);
  return (intptr_t)(now.time * 1000 + now.millitm);
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
  XFORM_SKIP_PROC
/* this function can be called from any OS thread */
{
#ifdef USE_MACTIME
  {
    /* This is wrong, since it's not since January 1, 1970 */
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

intptr_t scheme_get_process_milliseconds(void)
  XFORM_SKIP_PROC
{
#ifdef USER_TIME_IS_CLOCK
  return scheme_get_milliseconds();
#else
# ifdef USE_GETRUSAGE
  struct rusage use;
  intptr_t s, u;

  do {
    if (!getrusage(RUSAGE_SELF, &use))
      break;
  } while (errno == EINTR);

  s = use.ru_utime.tv_sec + use.ru_stime.tv_sec;
  u = use.ru_utime.tv_usec + use.ru_stime.tv_usec;

  return s * 1000 + u / 1000;
# else
#  ifdef USE_MACTIME
  {
    UnsignedWide time;
    Microseconds(&time);
    return ((uintptr_t)time.lo) / 1000;
  }
#  else
#   ifdef WINDOWS_GET_PROCESS_TIMES
  {
    FILETIME cr, ex, kr, us;
    if (GetProcessTimes(GetCurrentProcess(), &cr, &ex, &kr, &us)) {
      mzlonglong v;
      v = ((((mzlonglong)kr.dwHighDateTime << 32) + kr.dwLowDateTime)
	   + (((mzlonglong)us.dwHighDateTime << 32) + us.dwLowDateTime));
      return (uintptr_t)(v / 10000);
    }
  }
#   endif
  return clock()  * 1000 / CLOCKS_PER_SEC;

#  endif
# endif
#endif
}

intptr_t scheme_get_thread_milliseconds(Scheme_Object *thrd)
  XFORM_SKIP_PROC
{
  Scheme_Thread *t = thrd ? (Scheme_Thread *)thrd : scheme_current_thread;

  if (t == scheme_current_thread) {
    intptr_t cpm;
    cpm = scheme_get_process_milliseconds();
    return t->accum_process_msec + (cpm - t->current_start_process_msec);
  } else {
    return t->accum_process_msec;
  }
}

intptr_t scheme_get_seconds(void)
{
#ifdef USE_MACTIME
  /* This is wrong, since it's not since January 1, 1970 */
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
  return (intptr_t)now.time;
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

#if (defined(OS_X) || defined(XONX)) && defined(__x86_64__)
/* work around a bug in localtime() in 10.6.8 */
# include <sys/param.h>
# include <sys/sysctl.h>
static int VALID_TIME_RANGE(UNBUNDLE_TIME_TYPE lnow)
{
  /* Fits in 32 bits? */
  int ilnow = (int)lnow;
  if (lnow == (UNBUNDLE_TIME_TYPE)ilnow)
    return 1;

  /* 10.7 or later? */
  {
    int a[2];
    size_t len;
    char *vers;

    a[0] = CTL_KERN;
    a[1] = KERN_OSRELEASE;
    sysctl(a, 2, NULL, &len, NULL, 0);
    vers = (char *)scheme_malloc_atomic(len * sizeof(char));
    sysctl(a, 2, vers, &len, NULL, 0);

    if ((vers[0] == '1') && (vers[1] == '0') && (vers[2] == '.')) {
      /* localtime() in 10.7.x (= 10.x at the kernel layer) doesn't seem
         to work right with negative numbers that don't fit into 32 bits */
      return 0;
    }
  }

  return 1;
}
#else
# ifdef MIN_VALID_DATE_SECONDS
#  define VALID_TIME_RANGE(x) ((x) >= MIN_VALID_DATE_SECONDS)
# else
#  define VALID_TIME_RANGE(x) 1
# endif
#endif

static Scheme_Object *seconds_to_date(int argc, Scheme_Object **argv)
{
  UNBUNDLE_TIME_TYPE lnow;
  int get_gmt;
  int hour, min, sec, month, day, year, wday, yday, dst;
  long tzoffset;
#ifdef USE_MACTIME
# define CHECK_TIME_T unsigned long
  DateTimeRec localTime;
#else
# ifdef USE_PALMTIME
#  define CHECK_TIME_T UInt32
  DateTimeType localTime;
# else
#  define CHECK_TIME_T time_t
  struct tm *localTime;
# endif
#endif
  CHECK_TIME_T now;
  char *tzn;
  Scheme_Object *p[12], *secs, *nsecs, *zname;

  secs = argv[0];

  if (!SCHEME_REALP(secs)) {
    scheme_wrong_contract("seconds->date", "real?", 0, argc, argv);
    return NULL;
  }
  
  if (argc > 1)
    get_gmt = SCHEME_FALSEP(argv[1]);
  else
    get_gmt = 0;

  if (SCHEME_INTP(secs) || SCHEME_BIGNUMP(secs)) {
    nsecs = scheme_make_integer(0);
  } else {
    nsecs = secs;
    p[0] = secs;
    secs = scheme_floor(1, p);
    nsecs = scheme_bin_minus(nsecs, secs);
    nsecs = scheme_bin_mult(nsecs, scheme_make_integer(1000000000));
    p[0] = nsecs;
    nsecs = scheme_floor(1, p);
    p[0] = nsecs;
    nsecs = scheme_inexact_to_exact(1, p);
    p[0] = secs;
    secs = scheme_inexact_to_exact(1, p);
  }

  if (scheme_get_time_val(secs, &lnow)
      && (((UNBUNDLE_TIME_TYPE)(now = (CHECK_TIME_T)lnow)) == lnow)
      && VALID_TIME_RANGE(lnow)) {
    int success;

#ifdef USE_MACTIME
    SecondsToDate(lnow, &localTime);
    success = 1;
#else
# ifdef USE_PALMTIME
    TimSecondsToDateTime(lnow, &localTime) ;
# else
    if (get_gmt)
      localTime = gmtime(&now);
    else
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
# endif

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
# ifdef USE_MACTIME
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
# else
	/* PalmOS: */
	if (DaysInMonth(2, year) > 28)
	  yday++;
# endif
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

      if (get_gmt)
        dst = 0;
      else
        dst = localTime->tm_isdst;

      tzoffset = 0;
      if (!get_gmt) {
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
# ifdef USE_TZNAME_VAR
        tzn = MSC_IZE(tzname)[localTime->tm_isdst];
# elif defined(USE_TM_ZONE_FIELD)
        tzn = localTime->tm_zone;
# else
        tzn = NULL;
# endif
      } else
        tzn = "UTC";

#endif

      if (!tzn)
        tzn = "?";
      zname = scheme_make_utf8_string(tzn);
      SCHEME_SET_IMMUTABLE(zname);

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
      p[10] = nsecs;
      p[11] = zname;

      return scheme_make_struct_instance(scheme_date, 12, p);
    }
  }

  scheme_raise_exn(MZEXN_FAIL,
		   "seconds->date: integer is out-of-range\n"
                   "  integer: %V",
                   secs);

  return NULL;
}

static Scheme_Object *time_apply(int argc, Scheme_Object *argv[])
{
  intptr_t start, end;
  intptr_t cpustart, cpuend;
  intptr_t gcstart, gcend;
  intptr_t dur, cpudur, gcdur;
  int i, num_rands;
  Scheme_Object *v, *p[4], **rand_vec, *rands, *r;

  if (!SCHEME_PROCP(argv[0]))
    scheme_wrong_contract("time-apply", "procedure?", 0, argc, argv);

  rands = argv[1];

  num_rands = 0;
  r = rands;
  while (!SCHEME_NULLP(r)) {
    if (!SCHEME_PAIRP(r))
      scheme_wrong_contract("time-apply", "list?", 1, argc, argv);
    r = SCHEME_CDR(r);
    num_rands++;
  }

  if (SCHEME_FALSEP(get_or_check_arity(argv[0], num_rands, NULL, 1))) {
    scheme_contract_error("time-apply",
                          "arity mismatch between procedure and argument-list length\n",
                          "procedure", 1, argv[0],
                          "argument-list length", 1, scheme_make_integer(num_rands),
                          NULL);
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
    Scheme_Object **args;
    if (SAME_OBJ(cp->ku.multiple.array, cp->values_buffer))
      cp->values_buffer = NULL;
    args = cp->ku.multiple.array;
    cp->ku.multiple.array = NULL;
    v = scheme_build_list(cp->ku.multiple.count, args);
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
  if (!argc || SCHEME_FALSEP(argv[0]))
    return scheme_make_integer(scheme_get_process_milliseconds());
  else {
    if (SCHEME_THREADP(argv[0]))
      return scheme_make_integer(scheme_get_thread_milliseconds(argv[0]));
    scheme_wrong_contract("current-process-milliseconds", "thread?", 0, argc, argv);
    return NULL;
  }
}

static Scheme_Object *current_gc_milliseconds(int argc, Scheme_Object **argv)
{
  return scheme_make_integer(scheme_total_gc_time);
}

static Scheme_Object *current_seconds(int argc, Scheme_Object **argv)
{
  intptr_t secs;
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

static Scheme_Object *
current_read(int argc, Scheme_Object **argv)
{
  return scheme_param_config("current-read-interaction",
			     scheme_make_integer(MZCONFIG_READ_HANDLER),
			     argc, argv,
			     2, NULL, NULL, 0);
}

static Scheme_Object *
current_get_read_input_port(int argc, Scheme_Object **argv)
{
  return scheme_param_config("current-get-interaction-input-port",
			     scheme_make_integer(MZCONFIG_READ_INPUT_PORT_HANDLER),
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
scheme_default_read_input_port_handler(int argc, Scheme_Object *argv[])
{
  Scheme_Object *inport;
  
  inport = scheme_get_param(scheme_current_config(), MZCONFIG_INPUT_PORT);

  if (inport == scheme_orig_stdin_port)
    scheme_flush_orig_outputs();

  return inport;
}

Scheme_Object *
scheme_default_prompt_read_handler(int argc, Scheme_Object *argv[])
{
  Scheme_Config *config;
  Scheme_Object *port, *reader, *getter;
  Scheme_Object *inport, *name, *a[4], *v;

  config = scheme_current_config();
  port = scheme_get_param(config, MZCONFIG_OUTPUT_PORT);

  scheme_write_byte_string("> ", 2, port);
  scheme_flush_output(port);

  getter = scheme_get_param(config, MZCONFIG_READ_INPUT_PORT_HANDLER);
  inport = _scheme_apply(getter, 0, NULL);

  if (!SCHEME_INPORTP(inport))
    scheme_wrong_contract("default-prompt-read-hander", "input-port?", -1, -1, &inport);

  name = (Scheme_Object *)scheme_port_record(inport);
  name = ((Scheme_Input_Port *)name)->name;

  reader = scheme_get_param(config, MZCONFIG_READ_HANDLER);

  a[0] = name;
  a[1] = inport;
  v = _scheme_apply(reader, 2, a);

  a[0] = inport;
  if (SCHEME_TRUEP(scheme_terminal_port_p(1, a))) {
    a[0] = port;
    if (SCHEME_TRUEP(scheme_terminal_port_p(1, a))) {
      intptr_t line, col, pos;
      scheme_tell_all(port, &line, &col, &pos);
      if ((col > 0) && (line > 0)) {
        /* input and output are terminals (assume the same one), 
           and the output port counts lines: tell output port
           that it's on a new line: */
        a[0] = port;
        a[1] = scheme_make_integer(line + 1);
        a[2] = scheme_make_integer(0);
        if (pos > 0)
          a[3] = scheme_make_integer(pos + 2); /* incremet plus 0-adjust */
        else
          a[3] = scheme_false;
        scheme_set_port_location(4, a);
      }
    }
  }

  return v;
}
  
Scheme_Object *
scheme_default_read_handler(int argc, Scheme_Object *argv[])
{
  Scheme_Config *config;
  Scheme_Object *name = argv[0];
  Scheme_Object *inport = argv[1];
  Scheme_Object *stx;
  Scheme_Cont_Frame_Data cframe;

  if (!SCHEME_INPORTP(inport))
    scheme_wrong_contract("default-read-interaction-handler",
                          "input-port?",
                          1,
                          argc,
                          argv);

  config = scheme_current_config();
  config = scheme_extend_config(config, MZCONFIG_CAN_READ_READER, scheme_true);
  config = scheme_extend_config(config, MZCONFIG_CAN_READ_LANG, scheme_false);

  scheme_push_continuation_frame(&cframe);
  scheme_install_config(config);

  stx = scheme_read_syntax(inport, name);

  scheme_pop_continuation_frame(&cframe);

  return stx;
}

/*========================================================================*/
/*                                precise GC                              */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_fun.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_closure_info, mark_closure_info);
  GC_REG_TRAV(scheme_rt_dyn_wind_cell, mark_dyn_wind_cell);
  GC_REG_TRAV(scheme_rt_dyn_wind_info, mark_dyn_wind_info);
  GC_REG_TRAV(scheme_cont_mark_chain_type, mark_cont_mark_chain);
#ifdef MZ_USE_JIT
  GC_REG_TRAV(scheme_rt_lightweight_cont, mark_lightweight_cont);
#endif
}

END_XFORM_SKIP;

#endif
