/*
  MzScheme
  Copyright (c) 2004-2006 PLT Scheme Inc.
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
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

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

/* globals */
int scheme_defining_primitives; /* set to 1 during start-up */

Scheme_Object scheme_void[1]; /* the void constant */
Scheme_Object *scheme_values_func; /* the function bound to `values' */
Scheme_Object *scheme_void_proc;

Scheme_Object *scheme_tail_call_waiting;

Scheme_Object *scheme_inferred_name_symbol;

int scheme_cont_capture_count;

static Scheme_Object *certify_mode_symbol, *transparent_symbol, *transparent_binding_symbol, *opaque_symbol;

static Scheme_Object *null_val_key, *cont_key;

/* locals */
static Scheme_Object *procedure_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *apply (int argc, Scheme_Object *argv[]);
static Scheme_Object *map (int argc, Scheme_Object *argv[]);
static Scheme_Object *for_each (int argc, Scheme_Object *argv[]);
static Scheme_Object *andmap (int argc, Scheme_Object *argv[]);
static Scheme_Object *ormap (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_cc (int argc, Scheme_Object *argv[]);
static Scheme_Object *internal_call_cc (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_with_continuation_barrier (int argc, Scheme_Object *argv[]);
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

static Scheme_Object *is_method_symbol;

static long *thread_init_cc_ok;

static long *available_cc_ok;
static long *available_cws_cc_ok;

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

  o = scheme_make_folding_prim(procedure_p, "procedure?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(o) |= SCHEME_PRIM_IS_UNARY_INLINED;
  scheme_add_global_constant("procedure?", o, env);

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
  scheme_add_global_constant("call-with-values",
			     scheme_make_prim_w_arity2(call_with_values,
						       "call-with-values",
						       2, 2,
						       0, -1),
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
						    1, 1,
						    0, -1);

  o = scheme_make_prim_w_arity2(call_cc,
				"call-with-current-continuation",
				1, 1,
				0, -1);

  scheme_add_global_constant("call-with-current-continuation", o, env);
  scheme_add_global_constant("call/cc", o, env);

  scheme_add_global_constant("call-with-continuation-barrier",
			     scheme_make_prim_w_arity2(call_with_continuation_barrier,
						       "call-with-continuation-barrier",
						       1, 1,
						       0, -1), 
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
						      0, 0),
			     env);
  scheme_add_global_constant("continuation-marks",
			     scheme_make_prim_w_arity(cont_marks,
						      "continuation-marks",
						      1, 1),
			     env);
  scheme_add_global_constant("continuation-mark-set->list",
			     scheme_make_prim_w_arity(extract_cc_marks,
						      "continuation-mark-set->list",
						      2, 2),
			     env);
  scheme_add_global_constant("continuation-mark-set->list*",
			     scheme_make_prim_w_arity(extract_cc_markses,
						      "continuation-mark-set->list*",
						      2, 3),
			     env);
  scheme_add_global_constant("continuation-mark-set-first",
			     scheme_make_prim_w_arity(extract_one_cc_mark,
						      "continuation-mark-set-first",
						      2, 3),
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
  if (data->native_code) {
    Scheme_Object *nc;

    nc = scheme_make_native_closure(data->native_code);

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

Scheme_Object *scheme_jit_closure(Scheme_Object *code, Scheme_Object *context)
  /* If lr is supplied as a letrec binding this closure, it may be used
     for JIT compilation. */
{
#ifdef MZ_USE_JIT
  Scheme_Closure_Data *data = (Scheme_Closure_Data *)code;
  
  if (!data->native_code) {
    Scheme_Native_Closure_Data *ndata;
    
    data  = MALLOC_ONE_TAGGED(Scheme_Closure_Data);
    memcpy(data, code, sizeof(Scheme_Closure_Data));

    data->context = context;

    ndata = scheme_generate_lambda(data, 1, NULL);
    data->native_code = ndata;
    
    /* If it's zero-sized, then create closure now */
    if (!data->closure_size) {
      return scheme_make_native_closure(ndata);
    }

    return (Scheme_Object *)data;
  }
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

  info = scheme_optimize_info_add_frame(info, data->num_params, data->num_params,
					SCHEME_LAMBDA_FRAME);

  cl = (Closure_Info *)data->closure_map;
  for (i = 0; i < data->num_params; i++) {
    if (cl->local_flags[i] & SCHEME_WAS_SET_BANGED)
      scheme_optimize_mutated(info, i);
  }

  code = scheme_optimize_expr(data->code, info);

  data->code = code;

  /* Remembers positions of used vars (and unsets usage for this level) */
  scheme_env_make_closure_map(info, &dcs, &dcm);
  cl->base_closure_size = dcs;
  cl->base_closure_map = dcm;
  if (scheme_env_uses_toplevel(info))
    cl->has_tl = 1;
  cl->body_size = info->size;

  info->size++;
  info->inline_fuel++;

  data->closure_size = (cl->base_closure_size
			+ (cl->has_tl ? 1 : 0));

  info->max_let_depth += data->num_params + data->closure_size;
  data->max_let_depth = info->max_let_depth;

  info->max_let_depth = 0; /* So it doesn't propagate outward */

  scheme_optimize_info_done(info);

  return (Scheme_Object *)data;
}

Scheme_Object *scheme_clone_closure_compilation(Scheme_Object *_data, Optimize_Info *info, int delta, int closure_depth)
{
  Scheme_Closure_Data *data, *data2;
  Scheme_Object *body;
  Closure_Info *cl;
  int *flags, sz;

  data = (Scheme_Closure_Data *)_data;
  
  body = scheme_optimize_clone(data->code, info, delta, closure_depth + data->num_params);
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

int scheme_closure_argument_flags(Scheme_Closure_Data *data, int i)
{
  return ((Closure_Info *)data->closure_map)->local_flags[i];
}

Scheme_Object *
scheme_resolve_closure_compilation(Scheme_Object *_data, Resolve_Info *info)
{
  Scheme_Closure_Data *data;
  int i, closure_size, offset, np, orig_first_flag;
  mzshort *oldpos, *closure_map;
  Closure_Info *cl;
  Resolve_Info *new_info;

  data = (Scheme_Closure_Data *)_data;
  cl = (Closure_Info *)data->closure_map;
  data->iso.so.type = scheme_unclosed_procedure_type;

  /* Set local_flags: */
  orig_first_flag = (data->num_params ? cl->local_flags[0] : 0);
  for (i = 0; i < data->num_params; i++) {
    if (cl->local_flags[i] & SCHEME_WAS_SET_BANGED)
      cl->local_flags[i] = SCHEME_INFO_BOXED;
    else
      cl->local_flags[i] = 0;
  }

  closure_size = data->closure_size;
  closure_map = (mzshort *)scheme_malloc_atomic(sizeof(mzshort) * closure_size);

  /* Locals in closure are first: */
  oldpos = cl->base_closure_map;
  for (i = cl->base_closure_size; i--; ) {
    int li;
    li = scheme_resolve_info_lookup(info, oldpos[i], NULL);
    closure_map[i] = li;
  }

  /* Then the pointer to globals, if any: */
  offset = cl->base_closure_size;
  if (cl->has_tl) {
    /* GLOBAL ASSUMPTION: jit.c assumes that the array
       of globals is the last item in the closure; grep
       for "GLOBAL ASSUMPTION" in jit.c */
    int li;
    li = scheme_resolve_toplevel_pos(info);
    closure_map[offset] = li;
    offset++;
  }

  /* Set up mappng from old locations on the stack (as if bodies were
     evaluated immediately) to new locations (where closures
     effectively shift and compact values on the stack): */

  np = data->num_params;
  if ((data->num_params == 1)
      && (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST)
      && !(orig_first_flag & SCHEME_WAS_USED)) {
    /* (lambda args E) where args is not in E => drop the argument */
    new_info = scheme_resolve_info_extend(info, 0, 1, cl->base_closure_size);
    data->num_params = 0;
  } else {
    new_info = scheme_resolve_info_extend(info, data->num_params, data->num_params,
					  cl->base_closure_size + data->num_params);
    for (i = 0; i < data->num_params; i++) {
      scheme_resolve_info_add_mapping(new_info, i, i + closure_size,
				      cl->local_flags[i]);
    }
  }
  for (i = 0; i < cl->base_closure_size; i++) {
    int p = oldpos[i];

    if (p < 0)
      p -= np;
    else
      p += np;

    scheme_resolve_info_add_mapping(new_info, p, i,
				    scheme_resolve_info_flags(info, oldpos[i]));
  }
  if (cl->has_tl)
    scheme_resolve_info_set_toplevel_pos(new_info, cl->base_closure_size);

  data->closure_map = closure_map;

  {
    Scheme_Object *code;
    code = scheme_resolve_expr(data->code, new_info);
    data->code = code;
  }

  /* Add code to box set!ed argument variables: */
  for (i = 0; i < data->num_params; i++) {
    if (cl->local_flags[i] & SCHEME_INFO_BOXED) {
      int j = i + closure_size;
      Scheme_Object *code;

      code = scheme_make_syntax_resolved(BOXENV_EXPD,
					 scheme_make_pair(scheme_make_integer(j),
							  data->code));
      data->code = code;
    }
  }

  if (SCHEME_TYPE(data->code) > _scheme_compiled_values_types_)
    SCHEME_CLOSURE_DATA_FLAGS(data) |= CLOS_FOLDABLE;

  /* If the closure is empty, create the closure now */
  if (!data->closure_size)
    return scheme_make_closure(NULL, (Scheme_Object *)data, 1);
  else
    return (Scheme_Object *)data;
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
/*                         stack-overflow wrapper                         */
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

typedef Scheme_Object *(*Overflow_K_Proc)(void);

void *top_level_do(void *(*k)(void), int eb, void *sj_start)
     /* Wraps a function `k' with a handler for stack overflows and
	barriers to full-continuation jumps. No barrier if !eb. */
{
  void *v;
  long * volatile old_cc_ok;
  long volatile old_cc_ok_val = 0;
  long * volatile cc_ok;
  volatile long save_list_stack_pos;
  mz_jmp_buf *save, * volatile oversave = NULL, newbuf, overbuf;
  Scheme_Stack_State envss;
  Scheme_Comp_Env * volatile save_current_local_env;
  Scheme_Object * volatile save_mark, *  volatile save_name, * volatile save_certs, * volatile save_modidx;
  Scheme_Env * volatile save_menv;
  Scheme_Simple_Object * volatile save_list_stack;
  Scheme_Thread * volatile p = scheme_current_thread;
  volatile int save_suspend_break;
  int set_overflow;
#ifdef MZ_PRECISE_GC
  void *external_stack;
#endif

  if (scheme_active_but_sleeping)
    scheme_wake_up();

  if (eb) {
    old_cc_ok = p->cc_ok;

    if (top_next_use_thread_cc_ok) {
      top_next_use_thread_cc_ok = 0;
      if (!thread_init_cc_ok) {
	REGISTER_SO(thread_init_cc_ok);
	thread_init_cc_ok = (long *)scheme_malloc_atomic(sizeof(long));
      }
      cc_ok = thread_init_cc_ok;
    } else if (available_cc_ok) {
      cc_ok = available_cc_ok;
      available_cc_ok = NULL;
    } else
      cc_ok = (long *)scheme_malloc_atomic(sizeof(long));
    p->cc_ok = cc_ok;

    if (old_cc_ok) {
      old_cc_ok_val = *old_cc_ok;
      *old_cc_ok = 0;
    }
    *cc_ok = 1;
  } else {
    cc_ok = NULL;
    old_cc_ok = NULL;
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
  save_suspend_break = p->suspend_break;

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

  /* We set up an overflow handler at the lowest point possible
     in the stack for each thread. When we create a thread,
     we need to make sure that it starts out with a reasonable
     amount of space. */
  set_overflow = !p->overflow_set;
  if (set_overflow) {
    p->o_start = sj_start;
    p->overflow_set = 1;
    
    oversave = p->overflow_buf;
    p->overflow_buf = &overbuf;
    if (scheme_setjmp(overbuf)) {
      while (1) {
	/* We get `p' again because it might be a nestee: */
	Scheme_Thread * volatile pp;
	Scheme_Overflow * volatile overflow;
	mz_jmp_buf nestedbuf;

	pp = scheme_current_thread;
	overflow = pp->overflow;

	overflow->savebuf = pp->error_buf;
	pp->error_buf = &nestedbuf;
	if (scheme_setjmp(nestedbuf)) {
	  /* If we use scheme_overflow_reply here, it crashes on
	     Sparc. Sometimes. Can anyone tell me why? */
	  pp = scheme_current_thread;
	  pp->overflow_reply = NULL; /* means "continue the error" */
	} else {
	  void *p1, *p2, *p3, *p4, *p5;
	  long i1, i2, i3, i4;

	  p1 = pp->ku.k.p1;
	  p2 = pp->ku.k.p2;
	  p3 = pp->ku.k.p3;
	  p4 = pp->ku.k.p4;
	  p5 = pp->ku.k.p5;
	  i1 = pp->ku.k.i1;
	  i2 = pp->ku.k.i2;
	  i3 = pp->ku.k.i3;
	  i4 = pp->ku.k.i4;

	  /* stack overflow is a lot of work; force a sleep */
	  scheme_thread_block(0);
	  pp->ran_some = 1;

	  pp->ku.k.p1 = p1;
	  pp->ku.k.p2 = p2;
	  pp->ku.k.p3 = p3;
	  pp->ku.k.p4 = p4;
	  pp->ku.k.p5 = p5;
	  pp->ku.k.i1 = i1;
	  pp->ku.k.i2 = i2;
	  pp->ku.k.i3 = i3;
	  pp->ku.k.i4 = i4;

	  {
	    Overflow_K_Proc f = scheme_overflow_k;
	    Scheme_Object *reply;
	    reply = f();
	    scheme_overflow_reply = reply;
	  }
	}

	pp = scheme_current_thread;
	overflow = pp->overflow;
	pp->error_buf = overflow->savebuf;
	pp->overflow = overflow->prev;
	/* Reset overflow buffer and continue */
	if (scheme_setjmp(*pp->overflow_buf)) {
	  /* handle again */
	} else
	  scheme_longjmpup(&overflow->cont);
      }
    }
  }

  save = p->error_buf;
  p->error_buf = &newbuf;

  if (scheme_setjmp(newbuf)) {
    p = scheme_current_thread;
    scheme_restore_env_stack_w_thread(envss, p);
#ifdef MZ_PRECISE_GC
    if (scheme_set_external_stack_val)
      scheme_set_external_stack_val(external_stack);
#endif
    if (cc_ok) {
      if ((*cc_ok == 1) && (cc_ok != thread_init_cc_ok)) {
	/* It wasn't used */
	available_cc_ok = cc_ok;
      } else
	*cc_ok = 0;
      if (old_cc_ok)
	*old_cc_ok = old_cc_ok_val;
      p->cc_ok = old_cc_ok;
    }
    if (set_overflow) {
      p->overflow_buf = oversave;
      p->overflow_set = 0;
    }
    p->current_local_env = save_current_local_env;
    p->current_local_mark = save_mark;
    p->current_local_name = save_name;
    p->current_local_certs = save_certs;
    p->current_local_modidx = save_modidx;
    p->current_local_menv = save_menv;
    p->list_stack = save_list_stack;
    p->list_stack_pos = save_list_stack_pos;
    p->suspend_break = save_suspend_break;
    scheme_longjmp(*save, 1);
  }

  v = k();

  p = scheme_current_thread;

  p->current_local_env = save_current_local_env;
  p->current_local_mark = save_mark;
  p->current_local_name = save_name;
  p->current_local_certs = save_certs;
  p->current_local_modidx = save_modidx;
  p->current_local_menv = save_menv;

  p->error_buf = save;

  if (set_overflow) {
    p->overflow_buf = oversave;
    p->overflow_set = 0;
  }

  if (cc_ok) {
    if ((*cc_ok == 1) && (cc_ok != thread_init_cc_ok)) {
      /* It wasn't used */
      available_cc_ok = cc_ok;
    } else
      *cc_ok = 0;
    if (old_cc_ok)
      *old_cc_ok = old_cc_ok_val;
    p->cc_ok = old_cc_ok;
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

void scheme_clear_cc_ok()
{
  available_cc_ok = NULL;
  available_cws_cc_ok = NULL;
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

    /* Watch out for use of tail buffer: */
    if (p->ku.apply.tail_rands == p->tail_buffer) {
      GC_CAN_IGNORE Scheme_Object **tb;
      p->tail_buffer = NULL; /* so args aren't zeroed */
      tb = MALLOC_N(Scheme_Object *, p->tail_buffer_size);
      p->tail_buffer = tb;
    }

    if (multi_ok)
      return _scheme_apply_multi(p->ku.apply.tail_rator,
				 p->ku.apply.tail_num_rands,
				 p->ku.apply.tail_rands);
    else
      return _scheme_apply(p->ku.apply.tail_rator,
			   p->ku.apply.tail_num_rands,
			   p->ku.apply.tail_rands);
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

Scheme_Object *
scheme_tail_apply (Scheme_Object *rator, int num_rands, Scheme_Object **rands)
{
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
		   Scheme_Object *orig_code, Scheme_Comp_Env *cenv, int phase, 
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
    
    a = SCHEME_STX_CAR(code);
    a = cert_with_specials(a, mark, menv, orig_code, cenv, phase, cadr_deflt, 0);
    d = SCHEME_STX_CDR(code);
    d = cert_with_specials(d, mark, menv, orig_code, cenv, phase, 1, next_cadr_deflt);

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
		   Scheme_Object *certs, int for_set)
{
  Scheme_Object *orig_code = code;

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

   code = cert_with_specials(code, mark, menv, orig_code, env, env->genv->phase, 0, 0);

   code = scheme_stx_track(code, orig_code, name);

   return code;
 } else {
   Scheme_Object *mark, *rands_vec[1];

   certs = scheme_stx_extract_certs(code, certs);
 
   if (SAME_TYPE(SCHEME_TYPE(rator), scheme_set_macro_type))
     rator = SCHEME_PTR_VAL(rator);

   mark = scheme_new_mark();
   code = scheme_add_remove_mark(code, mark);

   scheme_on_next_top(env, mark, boundname, certs, 
		      menv, menv ? menv->link_midx : env->genv->link_midx);

   rands_vec[0] = code;
   code = scheme_apply(rator, 1, rands_vec);

   if (!SCHEME_STXP(code)) {
     scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		      "%S: return value from syntax expander was not syntax: %V",
		      SCHEME_STX_SYM(name),
		      code);
   }

   code = scheme_add_remove_mark(code, mark);

   code = cert_with_specials(code, mark, menv, orig_code, env, env->genv->phase, 0, 0);

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

Scheme_Object *scheme_get_or_check_arity(Scheme_Object *p, long a)
/* a == -1 => get arity
   a == -2 => check for allowing varargs */
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
    p = scheme_extract_struct_procedure(p, -1, NULL, &is_method);
    if (!SCHEME_PROCP(p))
      return scheme_null;
    if (is_method)
      drop++;
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

  if (!SCHEME_PROCP(p) || SCHEME_FALSEP(scheme_get_or_check_arity(p, a))) {
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
  } else if (SCHEME_INPORTP(a)) {
    Scheme_Input_Port *ip = (Scheme_Input_Port *)a;
    return ip->name;
  } else if (SCHEME_OUTPORTP(a)) {
    Scheme_Output_Port *op = (Scheme_Output_Port *)a;
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
  return scheme_get_or_check_arity(p, -1);
}

static Scheme_Object *procedure_arity(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_PROCP(argv[0]))
    scheme_wrong_type("procedure-arity", "procedure", 0, argc, argv);

  return scheme_get_or_check_arity(argv[0], -1);
}

static Scheme_Object *procedure_arity_includes(int argc, Scheme_Object *argv[])
{
  long n;

  if (!SCHEME_PROCP(argv[0]))
    scheme_wrong_type("procedure-arity-includes?", "procedure", 0, argc, argv);

  n = scheme_extract_index("procedure-arity-includes?", 1, argc, argv, -2, 0);

  return scheme_get_or_check_arity(argv[0], n);
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

  if (1 || num_rands > p->tail_buffer_size) {
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

static Scheme_Object *
do_map(int argc, Scheme_Object *argv[], char *name, int make_result,
       int and_mode, int or_mode)
     /* common code for `map', `for-each', `andmap' and `ormap' */
{
# define NUM_QUICK_ARGS 3
# define NUM_QUICK_RES  5
  int i, size = 0, l, pos;
  int can_multi;
  Scheme_Object *quick1[NUM_QUICK_ARGS], *quick2[NUM_QUICK_ARGS];
  Scheme_Object *quick3[NUM_QUICK_RES], **working, **args, **resarray;
  Scheme_Object *v, *retval;
  int cc;

  can_multi = (!make_result && !and_mode && !or_mode);

  if (!SCHEME_PROCP(argv[0]))
    scheme_wrong_type(name, "procedure", 0, argc, argv);

  for (i = 1; i < argc; i++) {
    if (!SCHEME_LISTP (argv[i]))
      scheme_wrong_type(name, "list", i, argc, argv);

    l = scheme_proper_list_length(argv[i]);

    if (l < 0)
      scheme_wrong_type(name, "proper list", i, argc, argv);

    if (i == 1)
      size = l;
    else if (size != l) {
      char *argstr;
      long alen;

      argstr = scheme_make_args_string("", -1, argc, argv, &alen);

      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "%s: all lists must have same size%t",
		       name, argstr, alen);
      return NULL;
    }
  }

  if (SCHEME_FALSEP(scheme_get_or_check_arity(argv[0], argc - 1))) {
    char *s;
    long aelen;

    s = scheme_make_arity_expect_string(argv[0], argc - 1, NULL, &aelen);

    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "%s: arity mismatch for %t", name,
		     s, aelen);
    return NULL;
  }

  if (argc <= (NUM_QUICK_ARGS + 1)) {
    args = quick1;
    working = quick2;
  } else {
    args = MALLOC_N(Scheme_Object *, argc - 1);
    working = MALLOC_N(Scheme_Object *, argc - 1);
  }

  if (size <= NUM_QUICK_RES) {
    resarray = quick3;
  } else {
    if (make_result)
      resarray = MALLOC_N(Scheme_Object *, size);
    else
      resarray = NULL;
  }

  /* Copy argc into working array */
  for (i = 1; i < argc; i++) {
    working[i-1] = argv[i];
  }

  --argc;

  if (and_mode)
    retval = scheme_true;
  else if (or_mode)
    retval = scheme_false;
  else
    retval = scheme_void;

  pos = 0;
  while (pos < size) {
    /* collect args to apply */
    for (i = 0; i < argc ; i++) {
      if (!SCHEME_PAIRP(working[i])) {
	/* There was a mutation! */
	scheme_raise_exn(MZEXN_FAIL_CONTRACT,
			 "%s: argument list mutated",
			 name);
	return NULL;
      }
      args[i] = SCHEME_CAR(working[i]);
      working[i] = SCHEME_CDR(working[i]);
    }

    cc = scheme_cont_capture_count;

    if (can_multi)
      v = _scheme_apply_multi(argv[0], argc, args);
    else
      v = _scheme_apply(argv[0], argc, args);

    if (cc != scheme_cont_capture_count) {
      /* Copy arrays to avoid messing with other continuations */
      if (make_result && (size > NUM_QUICK_RES)) {
	Scheme_Object **naya;
	naya = MALLOC_N(Scheme_Object *, size);
	memcpy(naya, resarray, pos * sizeof(Scheme_Object *));
	resarray = naya;
      }
      if (argc > NUM_QUICK_ARGS) {
	Scheme_Object **naya;
	args = MALLOC_N(Scheme_Object *, argc);
	naya = MALLOC_N(Scheme_Object *, argc);
	memcpy(naya, working, argc * sizeof(Scheme_Object *));
	working = naya;
      }
    }

    if (make_result) {
      resarray[pos] = v;
    } else if (and_mode) {
      if (SCHEME_FALSEP(v))
	return scheme_false;
      retval = v;
    } else if (or_mode) {
      if (SCHEME_TRUEP(v))
	return v;
    }
    pos++;
  }

  if (make_result)
    retval = scheme_build_list(size, resarray);

  return retval;
}

static Scheme_Object *
map (int argc, Scheme_Object *argv[])
{
  return do_map(argc, argv, "map", 1, 0, 0);
}

static Scheme_Object *
for_each (int argc, Scheme_Object *argv[])
{
  return do_map(argc, argv, "for-each", 0, 0, 0);
}

static Scheme_Object *
andmap(int argc, Scheme_Object *argv[])
{
  return do_map(argc, argv, "andmap", 0, 1, 0);
}

static Scheme_Object *
ormap(int argc, Scheme_Object *argv[])
{
  return do_map(argc, argv, "ormap", 0, 0, 1);
}

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

Scheme_Object *scheme_values(int argc, Scheme_Object *argv[])
{
  Scheme_Thread *p;
  int i;
  Scheme_Object **a;

  if (argc == 1)
    return argv[0];

  p = scheme_current_thread;
  p->ku.multiple.count = argc;
  if (p->values_buffer && (p->values_buffer_size >= argc))
    a = p->values_buffer;
  else {
    a = MALLOC_N(Scheme_Object *, argc);
    p->values_buffer = a;
    p->values_buffer_size = argc;
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

void scheme_clear_escape(void)
{
  Scheme_Thread *p = scheme_current_thread;

  p->cjs.jumping_to_continuation = NULL;
  p->cjs.u.val = NULL;
  p->cjs.num_vals = 0;
}

static void copy_cjs(Scheme_Continuation_Jump_State *a, Scheme_Continuation_Jump_State *b)
{
  a->jumping_to_continuation = b->jumping_to_continuation;
  a->u.val = b->u.val;
  a->num_vals = b->num_vals;
  a->is_kill = b->is_kill;
}

static Scheme_Object *get_ec_marks_prefix()
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *pr = scheme_null;
  long findpos;
  Scheme_Cont_Mark *find;

  findpos = (long)MZ_CONT_MARK_STACK;

  while (findpos--) {
    long pos;

    find = p->cont_mark_stack_segments[findpos >> SCHEME_LOG_MARK_SEGMENT_SIZE];
    pos = findpos & SCHEME_MARK_SEGMENT_MASK;

    if (find[pos].pos != MZ_CONT_MARK_POS)
      break;

    pr = scheme_make_pair(scheme_make_pair(find[pos].key,
					   find[pos].val),
			  pr);
  }

  return pr;
}

Scheme_Object *
scheme_call_ec (int argc, Scheme_Object *argv[])
{
  mz_jmp_buf newbuf;
  Scheme_Escaping_Cont * volatile cont;
  Scheme_Thread *p1 = scheme_current_thread;
  Scheme_Object * volatile v;
  Scheme_Object *mark_key, *a[1];

  scheme_check_proc_arity("call-with-escaping-continuation", 1,
			  0, argc, argv);

  /* In tail position with respect to an existing
     escape continuation? */
  mark_key = p1->current_escape_cont_key;
  if (mark_key && SAME_OBJ((Scheme_Object *)MZ_CONT_MARK_POS,
			   SCHEME_CAR(mark_key))) {
    /* Yes - reuse the old continuation */
    cont = (Scheme_Escaping_Cont *)SCHEME_CDR(mark_key);

    v = get_ec_marks_prefix();

    if (!scheme_equal(v, cont->marks_prefix)) {
      /* The continuation marks are different this time. 
	 We need to clone the continuation, then change mark prefix. */
      Scheme_Escaping_Cont *c2;
      c2 = MALLOC_ONE_TAGGED(Scheme_Escaping_Cont);
      memcpy(c2, cont, sizeof(Scheme_Escaping_Cont));
      c2->marks_prefix = v;
      cont = c2;
    }

    a[0] = (Scheme_Object *)cont;
    SCHEME_USE_FUEL(1);
    return scheme_tail_apply(argv[0], 1, a);
  }

  mark_key = scheme_make_pair((Scheme_Object *)MZ_CONT_MARK_POS, 
			      scheme_false);
  
  cont = MALLOC_ONE_TAGGED(Scheme_Escaping_Cont);
  cont->so.type = scheme_escaping_cont_type;
  cont->mark_key = mark_key;
  cont->suspend_break = p1->suspend_break;
  copy_cjs(&cont->cjs, &p1->cjs);

  SCHEME_CDR(mark_key) = (Scheme_Object *)cont;

  v = get_ec_marks_prefix();
  cont->marks_prefix = v;

  cont->saveerr = p1->error_buf;
  p1->error_buf = &newbuf;

  scheme_save_env_stack_w_thread(cont->envss, p1);

  /* Don't push a continuation frame; argument function
     is called as tail. */
  scheme_set_cont_mark(mark_key, scheme_true);
  p1->current_escape_cont_key = mark_key;

  if (scheme_setjmp(newbuf)) {
    Scheme_Thread *p2 = scheme_current_thread;
    if (p2->cjs.jumping_to_continuation
	&& SAME_OBJ(p2->cjs.jumping_to_continuation->mark_key,
		    cont->mark_key)) {
      int n = p2->cjs.num_vals;
      Scheme_Object **vs = p2->cjs.u.vals;
      v = p2->cjs.u.val;
      copy_cjs(&p2->cjs, &cont->cjs);
      scheme_restore_env_stack_w_thread(cont->envss, p2);
      p2->suspend_break = cont->suspend_break;
      if (n != 1)
	v = scheme_values(n, vs);
    } else {
      scheme_longjmp(*cont->saveerr, 1);
    }
  } else {
    /* Adjusting MZ_CONT_MARK_POS, we make the application appear to
       be in tail position. The actual non-tailness is limited to a
       single frame, since call_ec checks the current escape-cont key
       as a continuation mark before getting here. */
    MZ_CONT_MARK_POS -= 2;

    a[0] = (Scheme_Object *)cont;
    v = _scheme_apply_multi(argv[0], 1, a);

    MZ_CONT_MARK_POS += 2;
  }

  p1 = scheme_current_thread;

  p1->error_buf = cont->saveerr;
  p1->current_escape_cont_key = cont->envss.current_escape_cont_key;

  return v;
}

int scheme_escape_continuation_ok(Scheme_Object *ec)
{
  Scheme_Escaping_Cont *cont = (Scheme_Escaping_Cont *)ec;

  if (scheme_extract_one_cc_mark(NULL, cont->mark_key))
    return 1;
  else
    return 0;
}

static Scheme_Object *
do_call_with_sema(const char *who, int enable_break, int argc, Scheme_Object *argv[])
{
  mz_jmp_buf newbuf, * volatile savebuf;
  long * volatile cc_ok;
  long * volatile old_cc_ok;
  long volatile old_cc_ok_val;
  int i, just_try;
  int volatile extra;
  Scheme_Object * volatile sema;
  Scheme_Object *v, *quick_args[4], **extra_args;

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
    Scheme_Cont_Frame_Data cframe;
    scheme_push_break_enable(&cframe, 1, 1);
    scheme_check_break_now();
    scheme_pop_break_enable(&cframe, 0);
  }

  if (!scheme_wait_sema(sema, just_try ? 1 : (enable_break ? -1 : 0))) {
    return _scheme_tail_apply(argv[2], 0, NULL);
  }

  savebuf = scheme_current_thread->error_buf;
  scheme_current_thread->error_buf = &newbuf;

  if (available_cws_cc_ok) {
    cc_ok = available_cws_cc_ok;
    available_cws_cc_ok = NULL;
  } else
    cc_ok = (long *)scheme_malloc_atomic(sizeof(long));
  *cc_ok = 1;

  old_cc_ok = scheme_current_thread->cc_ok;
  scheme_current_thread->cc_ok = cc_ok;
  if (old_cc_ok) {
    old_cc_ok_val = *old_cc_ok;
    *old_cc_ok = 0;
  }

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

  scheme_post_sema(sema);

  if (*cc_ok != 2)
    available_cws_cc_ok = cc_ok;
  else
    *cc_ok = 0;
  if (old_cc_ok)
    *old_cc_ok = old_cc_ok_val;
  scheme_current_thread->cc_ok = old_cc_ok;

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
					     Scheme_Cont *share_from)
{
  Scheme_Saved_Stack *saved, *isaved, *csaved, *share_saved, *ss;
  Scheme_Object **start;
  long size;

  /* Copy out current runstack: */
  saved = MALLOC_ONE_RT(Scheme_Saved_Stack);
#ifdef MZTAG_REQUIRED
  saved->type = scheme_rt_saved_stack;
#endif
  if (share_from && (share_from->ss.runstack_start == runstack_start)) {
    /* Copy just the difference between share_from's runstack and current runstack... */
    size = (share_from->ss.runstack XFORM_OK_MINUS runstack);
    /* But add one, because call/cc takes one argument. If there's not one
       move value on the stack, then call/cc must have received its argument
       from elsewhere. */
    if ((share_from->ss.runstack XFORM_OK_MINUS runstack_start) < p->runstack_size)
      size++;
  } else {
    size = p->runstack_size - (runstack XFORM_OK_MINUS runstack_start);
  }

  saved->runstack_size = size;
  start = MALLOC_N(Scheme_Object*, size);
  saved->runstack_start = start;
  memcpy(saved->runstack_start, runstack, size * sizeof(Scheme_Object *));

  /* Copy saved runstacks: */
  isaved = saved;
  share_saved = NULL;
  if (share_from) {
    /* We can share all saved runstacks */
    share_saved = share_from->ss.runstack_saved;
  }
  for (csaved = p->runstack_saved; csaved; csaved = csaved->prev) {
    if (share_saved && (csaved->runstack_start == share_saved->runstack_start)) {
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

    size = csaved->runstack_size - (csaved->runstack XFORM_OK_MINUS csaved->runstack_start);
    isaved->runstack_size = size;

    start = MALLOC_N(Scheme_Object*, size);
    isaved->runstack_start = start;
    memcpy(isaved->runstack_start, csaved->runstack, size * sizeof(Scheme_Object *));
  }

  return saved;
}

static Scheme_Cont_Mark *copy_out_mark_stack(Scheme_Thread *p, 
					     MZ_MARK_STACK_TYPE pos,
					     Scheme_Cont *sub_cont,
					     long *_offset)
{
  long cmcount, offset = 0;
  Scheme_Cont_Mark *cont_mark_stack_copied;

  /* Copy cont mark stack: */
  cmcount = (long)pos;
  offset = 0;

  if (sub_cont) {
    /* Rely on copy of marks in a tail of this continuation. */
    long sub_count = sub_cont->cont_mark_shareable;
    cmcount -= sub_count;
    offset += sub_count;
  }

  if (_offset) *_offset = offset;

  if (cmcount) {
    cont_mark_stack_copied = MALLOC_N(Scheme_Cont_Mark, cmcount);
    while (cmcount--) {
      int cms = cmcount + offset;
      Scheme_Cont_Mark *seg = p->cont_mark_stack_segments[cms >> SCHEME_LOG_MARK_SEGMENT_SIZE];
      long pos = cms & SCHEME_MARK_SEGMENT_MASK;
      Scheme_Cont_Mark *cm = seg + pos;
      
      memcpy(cont_mark_stack_copied + cmcount, cm, sizeof(Scheme_Cont_Mark));
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
    size = isaved->runstack_size;
    csaved->runstack = csaved->runstack_start + (csaved->runstack_size - size);
    memcpy(csaved->runstack, isaved->runstack_start, size * sizeof(Scheme_Object *));
  }
}

static void copy_in_mark_stack(Scheme_Thread *p, Scheme_Cont_Mark *cont_mark_stack_copied,
			       MZ_MARK_STACK_TYPE cms, MZ_MARK_STACK_TYPE base_cms,
			       long copied_offset, Scheme_Object **_sub_conts)
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
      int newcount = needed, oldcount = p->cont_mark_seg_count;

      /* Note: we perform allocations before changing p to avoid GC trouble,
	 since MzScheme adjusts a thread's cont_mark_stack_segments on GC. */
      segs = MALLOC_N(Scheme_Cont_Mark *, needed);

      while (needed--) {
	if (needed < oldcount)
	  segs[needed] = old_segs[needed]; /* might be NULL due to GC! */
	else
	  segs[needed] = NULL;

	if (!segs[needed]) {
	  Scheme_Cont_Mark *cm;
	  cm = scheme_malloc_allow_interior(sizeof(Scheme_Cont_Mark) * SCHEME_MARK_SEGMENT_SIZE);
	  segs[needed] = cm;
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
    Scheme_Cont_Mark *cm = seg + pos;
    
    cm_src = cont_mark_stack_copied;
    cmoffset = base_cmcount - copied_offset;

    if (sub_cont) {
      while (base_cmcount >= sub_cont->cont_mark_shareable) {
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

static Scheme_Cont_Mark **copy_out_segment_array(Scheme_Cont *sub_cont)
{
  long cnt;
  Scheme_Cont_Mark **orig;

  if (!MZ_CONT_MARK_STACK)
    cnt = 0;
  else
    cnt = (((long)MZ_CONT_MARK_STACK - 1) >> SCHEME_LOG_MARK_SEGMENT_SIZE) + 1;

  if (sub_cont) {
    /* Already saved this set? */
    int scnt;
    if (!sub_cont->ss.cont_mark_stack)
      scnt = 0;
    else
      scnt = (((long)(sub_cont->ss.cont_mark_stack) - 1) >> SCHEME_LOG_MARK_SEGMENT_SIZE) + 1;
    if (scnt == cnt) {
      return sub_cont->orig_mark_segments;
    }
  }

  orig = (Scheme_Cont_Mark **)scheme_malloc(cnt * sizeof(Scheme_Cont_Mark*));
  memcpy(orig, scheme_current_thread->cont_mark_stack_segments, cnt * sizeof(Scheme_Cont_Mark*));
  return orig;
}

static Scheme_Object *
call_cc (int argc, Scheme_Object *argv[])
{
  scheme_check_proc_arity("call-with-current-continuation", 1,
			  0, argc, argv);

  /* Trampoline to internal_call_cc. This trampoline ensures that
     the runstack is flushed before we try to grab the continuation. */
  return _scheme_tail_apply(internal_call_cc_prim, argc, argv);
}

static Scheme_Object *
internal_call_cc (int argc, Scheme_Object *argv[])
{
  Scheme_Object *ret;
  Scheme_Cont * volatile cont;
  Scheme_Cont *sub_cont;
  Scheme_Dynamic_Wind *dw;
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Saved_Stack *saved;
  Scheme_Cont_Mark *msaved;

  sub_cont = (Scheme_Cont *)scheme_extract_one_cc_mark(NULL, cont_key);
  if (sub_cont && (sub_cont->save_overflow != p->overflow))
    sub_cont = NULL;
  if (sub_cont && (sub_cont->ss.cont_mark_pos == MZ_CONT_MARK_POS)) {
    Scheme_Object *argv2[1];
#ifdef MZ_USE_JIT
    ret = scheme_native_stack_trace();
#endif    
    /* Old cont is the same as this one, except that it may
       have different marks (not counting cont_key). */
    if ((sub_cont->cont_mark_shareable == (long)sub_cont->ss.cont_mark_stack)
	&& (find_shareable_marks() == MZ_CONT_MARK_STACK)
#ifdef MZ_USE_JIT
	&& (SAME_OBJ(ret, sub_cont->native_trace)
	    /* Maybe a single-function loop, where we re-allocated the
	       last pair in the trace, but it's the same name: */
	    || (SCHEME_PAIRP(ret)
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

      cont = MALLOC_ONE_TAGGED(Scheme_Cont);
      cont->so.type = scheme_cont_type;
      cont->buf.cont = sub_cont;
      sub_cont = sub_cont->buf.cont;

      /* This mark stack won't be restored, but it may be
	 used by `continuation-marks'. */
      cont->ss.cont_mark_stack = MZ_CONT_MARK_STACK;
      msaved = copy_out_mark_stack(p, cont->ss.cont_mark_stack, sub_cont, &offset);
      cont->cont_mark_stack_copied = msaved;
      cont->cont_mark_offset = offset;
      offset = find_shareable_marks();
      cont->cont_mark_shareable = offset;
#ifdef MZ_USE_JIT
      cont->native_trace = ret;
#endif
    }

    argv2[0] = (Scheme_Object *)cont;
    return _scheme_tail_apply(argv[0], 1, argv2);
  }

  cont = MALLOC_ONE_TAGGED(Scheme_Cont);
  cont->so.type = scheme_cont_type;

  /* Set cont_key mark before capturing marks: */
  scheme_set_cont_mark(cont_key, (Scheme_Object *)cont);

  scheme_init_jmpup_buf(&cont->buf);
  cont->ok = p->cc_ok;
  *(p->cc_ok) = 2; /* Marks it as used */
  cont->dw = p->dw;
  cont->suspend_break = p->suspend_break;
  copy_cjs(&cont->cjs, &p->cjs);
  cont->save_overflow = p->overflow;
  cont->save_overflow_buf = p->overflow_buf;
  scheme_save_env_stack_w_thread(cont->ss, p);
  cont->init_config = p->init_config;
  cont->init_break_cell = p->init_break_cell;

  {
    Scheme_Overflow *overflow;
    /* Mark overflows as captured: */
    for (overflow = p->overflow; overflow && !overflow->captured; overflow = overflow->prev) {
      overflow->captured = 1;
    }
  }
  scheme_cont_capture_count++;

  if (p->cc_ok == thread_init_cc_ok) {
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
  ret = scheme_native_stack_trace();
  cont->native_trace = ret;
#endif

  saved = copy_out_runstack(p, MZ_RUNSTACK, MZ_RUNSTACK_START, sub_cont);
  cont->runstack_copied = saved;
  {
    long offset;
    msaved = copy_out_mark_stack(p, cont->ss.cont_mark_stack, sub_cont, &offset);
    cont->cont_mark_stack_copied = msaved;
    cont->cont_mark_offset = offset;
    offset = find_shareable_marks();
    cont->cont_mark_shareable = offset;
  }

  /* Remember the original mark-stack segments. */
  {
    Scheme_Cont_Mark **orig;
    orig = copy_out_segment_array(sub_cont);
    cont->orig_mark_segments = orig;
  }

  cont->runstack_owner = p->runstack_owner;
  cont->cont_mark_stack_owner = p->cont_mark_stack_owner;

  cont->stack_start = p->stack_start;
  cont->o_start = p->o_start;

  cont->savebuf = p->error_buf;

  scheme_zero_unneeded_rands(p);

  scheme_flatten_config(scheme_current_config());

  if (scheme_setjmpup_relative(&cont->buf, cont, p->next ? p->stack_start : p->o_start, sub_cont)) {
    /* We arrive here when the continuation is applied */
    MZ_MARK_STACK_TYPE copied_cms = 0;
    Scheme_Object *result, **mv, *sub_conts = NULL;
    int mc;
    
    result = cont->value;
    cont->value = NULL;

    p = scheme_current_thread; /* maybe different than before */

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

    p->stack_start = cont->stack_start;
    p->o_start = cont->o_start;
    p->init_config = cont->init_config;
    p->init_break_cell = cont->init_break_cell;

    p->suspend_break = cont->suspend_break;
    
    copy_cjs(&p->cjs, &cont->cjs);
    p->overflow_buf = cont->save_overflow_buf;
    p->overflow = cont->save_overflow;
    scheme_restore_env_stack_w_thread(cont->ss, p);

    if (p->runstack_owner
	&& (*p->runstack_owner == p))
      *p->runstack_owner = NULL;

    p->runstack_owner = cont->runstack_owner;
    if (p->runstack_owner && (*p->runstack_owner != p)) {
      Scheme_Thread *op;
      op = *p->runstack_owner;
      if (op) {
	saved = copy_out_runstack(op, op->runstack, op->runstack_start, NULL);
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
	    && (sub_cont->ss.runstack_start == sub_cont->buf.cont->ss.runstack_start)) {
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
	msaved = copy_out_mark_stack(op, op->cont_mark_stack, NULL, NULL);
	op->cont_mark_stack_swapped = msaved;
      }
      *p->cont_mark_stack_owner = p;
      /* In case there's a GC before we copy in marks: */
      MZ_CONT_MARK_STACK = 0;
    }

    /* For copying cont marks back in, we need a list of sub_conts,
       deepest to shallowest: */
    for (sub_cont = cont->buf.cont; sub_cont; sub_cont = sub_cont->buf.cont) {
      sub_conts = scheme_make_raw_pair((Scheme_Object *)sub_cont, sub_conts);
    }
    
    /* For dynamic-winds after the "common" intersection
       (see eval.c), execute the pre thunks. Make a list
       of these first because they have to be done in the
       inverse order of `prev' linkage. */
    if (cont->dw) {
      Scheme_Dynamic_Wind_List *dwl = NULL;

      p->suspend_break++;
      for (dw = cont->dw; dw != cont->common; dw = dw->prev) {
	Scheme_Dynamic_Wind_List *cell;

	cell = MALLOC_ONE_RT(Scheme_Dynamic_Wind_List);
#ifdef MZTAG_REQUIRED
	cell->type = scheme_rt_dyn_wind_cell;
#endif
	cell->dw = dw;
	cell->next = dwl;
	dwl = cell;
      }
      for (; dwl; dwl = dwl->next) {
	if (dwl->dw->pre) {
	  DW_PrePost_Proc pre = dwl->dw->pre;

	  /* Restore the needed part of the mark stack for this
	     dynamic-wind context. */
	  MZ_CONT_MARK_POS = dwl->dw->envss.cont_mark_pos;
	  MZ_CONT_MARK_STACK = dwl->dw->envss.cont_mark_stack;
	  copy_in_mark_stack(p, cont->cont_mark_stack_copied, 
			     MZ_CONT_MARK_STACK, copied_cms,
			     cont->cont_mark_offset, &sub_conts);
	  copied_cms = MZ_CONT_MARK_STACK;

	  p->dw = dwl->dw->prev;
	  pre(dwl->dw->data);
	  p = scheme_current_thread;
	}
      }
      --p->suspend_break;
    }

    p->dw = cont->dw;

    /* Finish copying cont mark stack back in. */
    
    MZ_CONT_MARK_POS = cont->ss.cont_mark_pos;
    MZ_CONT_MARK_STACK = cont->ss.cont_mark_stack;
    copy_in_mark_stack(p, cont->cont_mark_stack_copied, 
		       MZ_CONT_MARK_STACK, copied_cms,
		       cont->cont_mark_offset, &sub_conts);

    /* If any mark-stack segment is different now than before, then
       set the cache field of the *original* mark segment. Setting the
       cache field ensures that any `pm' pointer in scheme_do_eval
       will get reset to point to the new segment. */
    {
      long cnt, i, j;
      Scheme_Cont_Mark *cm;
      if (!MZ_CONT_MARK_STACK)
	cnt = 0;
      else
	cnt = (((long)MZ_CONT_MARK_STACK - 1) >> SCHEME_LOG_MARK_SEGMENT_SIZE) + 1;
      for (i = 0; i < cnt; i++) {
	if (cont->orig_mark_segments[i] != p->cont_mark_stack_segments[i]) {
	  if (i + 1 == cnt) {
	    j = ((long)MZ_CONT_MARK_STACK) & SCHEME_MARK_SEGMENT_MASK;
	  } else {
	    j = SCHEME_MARK_SEGMENT_SIZE;
	  }
	  while (j--) {
	    cm = cont->orig_mark_segments[i] + j;
	    if (SAME_OBJ(cm->key, scheme_stack_dump_key)) {
	      cm->cache = scheme_false;
	    }
	  }
	}
      }
    }
        
    /* We may have just re-activated breaking: */
    scheme_check_break_now();

    if (SAME_OBJ(result, SCHEME_MULTIPLE_VALUES)) {
      p->ku.multiple.array = mv;
      p->ku.multiple.count = mc;
    }

    return result;
  } else {
    Scheme_Object *argv2[1];

    argv2[0] = (Scheme_Object *)cont;
    ret = _scheme_tail_apply(argv[0], 1, argv2);
    return ret;
  }
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
      swapped = copy_out_runstack(op, op->runstack, op->runstack_start, NULL);
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
      swapped = copy_out_mark_stack(op, op->cont_mark_stack, NULL, NULL);
      op->cont_mark_stack_swapped = swapped;
    }
    *(p->cont_mark_stack_owner) = p;
    copy_in_mark_stack(p, p->cont_mark_stack_swapped, MZ_CONT_MARK_STACK, 0, 0, NULL);
    p->cont_mark_stack_swapped = NULL;
  }
}

static Scheme_Object *
call_with_continuation_barrier (int argc, Scheme_Object *argv[])
{
  scheme_check_proc_arity("call-with-continuation-barrier", 0, 0, argc, argv);

  return scheme_apply(argv[0], 0, NULL);
}


static Scheme_Object *continuation_marks(Scheme_Thread *p,
					 Scheme_Object *_cont,
					 Scheme_Object *econt,
					 int just_chain)
     /* cont => p is not used */
{
  Scheme_Cont *cont = (Scheme_Cont *)_cont;
  Scheme_Cont_Mark_Chain *first = NULL, *last = NULL;
  Scheme_Cont_Mark_Set *set;
  Scheme_Object *cache, *nt;
  long findpos;
  long cmpos, cdelta = 0;

  if (cont) {
    findpos = (long)cont->ss.cont_mark_stack;
    cmpos = (long)cont->ss.cont_mark_pos;
    if (cont->buf.cont) {
      if (cont->runstack_copied)
	cdelta = cont->buf.cont->cont_mark_shareable;
      else {
	/* Current cont was just a mark-stack variation of
	     next cont, so skip the next cont. */
	if (cont->buf.cont->buf.cont)
	  cdelta = cont->buf.cont->buf.cont->cont_mark_shareable;
      }
    }
  } else if (econt) {
    findpos = (long)((Scheme_Escaping_Cont *)econt)->envss.cont_mark_stack;
    cmpos = (long)((Scheme_Escaping_Cont *)econt)->envss.cont_mark_pos;
  } else {
    findpos = (long)MZ_CONT_MARK_STACK;
    cmpos = (long)MZ_CONT_MARK_POS;
    if (!p->cont_mark_stack_segments)
      findpos = 0;
  }

  while (findpos--) {
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
	if (cont->buf.cont)
	  cdelta = cont->buf.cont->cont_mark_shareable;
	else
	  cdelta = 0;
      }
      find = cont->cont_mark_stack_copied;
      pos = findpos - cdelta;
    } else {
      GC_CAN_IGNORE Scheme_Cont_Mark *seg;

      seg = p->cont_mark_stack_segments[findpos >> SCHEME_LOG_MARK_SEGMENT_SIZE];
      pos = findpos & SCHEME_MARK_SEGMENT_MASK;
      find = seg;
    }

    /* For econt, skip positions that match cmpos; the econt
       record has a prefix to use, instead. */
    
    if (!econt || (find[pos].pos != cmpos)) {
      cache = find[pos].cache;
      if (cache) {
	if (SCHEME_FALSEP(cache))
	  cache = NULL;
	else if (SCHEME_VECTORP(cache)) {
	  cache = SCHEME_VEC_ELS(cache)[0];
	}
      }

      if (cache) {
	if (last)
	  last->next = (Scheme_Cont_Mark_Chain *)cache;
	else
	  first = (Scheme_Cont_Mark_Chain *)cache;

	break;
      } else {
	Scheme_Cont_Mark_Chain *pr;
	pr = MALLOC_ONE_RT(Scheme_Cont_Mark_Chain);
	pr->so.type = scheme_cont_mark_chain_type;
	pr->key = find[pos].key;
	pr->val = find[pos].val;
	pr->pos = find[pos].pos;
	pr->next = NULL;
	cache = find[pos].cache;
	if (cache && !SCHEME_FALSEP(cache)) {
	  SCHEME_VEC_ELS(cache)[0] = (Scheme_Object *)pr;
	} else {
	  find[pos].cache = (Scheme_Object *)pr;
	}
	if (last)
	  last->next = pr;
	else
	  first = pr;

	last = pr;
      }
    }
  }

  if (econt) {
    Scheme_Object *l, *a;
    for (l = ((Scheme_Escaping_Cont *)econt)->marks_prefix; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
      Scheme_Cont_Mark_Chain *pr;
      pr = MALLOC_ONE_RT(Scheme_Cont_Mark_Chain);
      pr->so.type = scheme_cont_mark_chain_type;
      a = SCHEME_CAR(l);
      pr->key = SCHEME_CAR(a);
      pr->val = SCHEME_CDR(a);
      pr->pos = cmpos;
      pr->next = first;
      first = pr;
    }
  }

  if (just_chain)
    return (Scheme_Object *)first;

#ifdef MZ_USE_JIT
  if (cont)
    nt = cont->native_trace;
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

Scheme_Object *scheme_current_continuation_marks(void)
{
  return continuation_marks(scheme_current_thread, NULL, NULL, 0);
}

static Scheme_Object *
cc_marks(int argc, Scheme_Object *argv[])
{
  return scheme_current_continuation_marks();
}

static Scheme_Object *
cont_marks(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_CONTP(argv[0]) && !SCHEME_ECONTP(argv[0]))
    scheme_wrong_type("continuation-marks", "continuation", 0, argc, argv);

  if (SCHEME_ECONTP(argv[0])) {
    if (!scheme_escape_continuation_ok(argv[0])) {
      scheme_arg_mismatch("continuation-marks",
			  "escape continuation not in the current thread's continuation: ",
			  argv[0]);
    }

    return continuation_marks(scheme_current_thread, NULL, argv[0], 0);
  } else
    return continuation_marks(NULL, argv[0], NULL, 0);
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
  Scheme_Object *first = scheme_null, *last = NULL, *key;
  Scheme_Object *pr;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_cont_mark_set_type)) {
    scheme_wrong_type("continuation-mark-set->list", "continuation-mark-set", 0, argc, argv);
    return NULL;
  }
  chain = ((Scheme_Cont_Mark_Set *)argv[0])->chain;
  key = argv[1];

  if ((key == scheme_parameterization_key)
      || (key == scheme_break_enabled_key)) {
    scheme_signal_error("continuation-mark-set->list: secret key leaked!");
    return NULL;
  }

  while (chain) {
    if (chain->key == key) {
      pr = scheme_make_pair(chain->val, scheme_null);
      if (last)
	SCHEME_CDR(last) = pr;
      else
	first = pr;
      last = pr;
    }

    chain = chain->next;
  }

  return first;
}

static Scheme_Object *
extract_cc_markses(int argc, Scheme_Object *argv[])
{
  Scheme_Cont_Mark_Chain *chain;
  Scheme_Object *first = scheme_null, *last = NULL;
  Scheme_Object *pr, **keys, *vals, *none;
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

  keys = MALLOC_N(Scheme_Object *, len);
  for (pr = argv[1], i = 0; SCHEME_PAIRP(pr); pr = SCHEME_CDR(pr), i++) {
    keys[i] = SCHEME_CAR(pr);
    if ((keys[i] == scheme_parameterization_key)
	|| (keys[i] == scheme_break_enabled_key)) {
      scheme_signal_error("continuation-mark-set->list: secret key leaked!");
      return NULL;
    }
  }

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
scheme_extract_one_cc_mark(Scheme_Object *mark_set, Scheme_Object *key)
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
    long findpos;
    long pos;
    Scheme_Object *val = NULL;
    Scheme_Object *cache;
    GC_CAN_IGNORE Scheme_Cont_Mark *seg;
    Scheme_Thread *p = scheme_current_thread;
    
    findpos = (long)MZ_CONT_MARK_STACK;
    if (!p->cont_mark_stack_segments)
      findpos = 0;

    /* Search mark stack, checking caches along the way: */
    while (findpos--) {
      seg = p->cont_mark_stack_segments[findpos >> SCHEME_LOG_MARK_SEGMENT_SIZE];
      pos = findpos & SCHEME_MARK_SEGMENT_MASK;

      if (SAME_OBJ(seg[pos].key, key)) {
	val = seg[pos].val;
	break;
      } else {
	cache = seg[pos].cache;
	if (cache && SCHEME_VECTORP(cache)) {
	  /* If slot 1 has a key, this cache has just one key--value
	     pair. Otherwise, slot 2 is a hash table. */
	  if (SCHEME_VEC_ELS(cache)[1]) {
	    if (SAME_OBJ(SCHEME_VEC_ELS(cache)[1], key)) {
	      val = SCHEME_VEC_ELS(cache)[2];
	      break;
	    }
	  } else {
	    Scheme_Hash_Table *ht;
	    ht = (Scheme_Hash_Table *)SCHEME_VEC_ELS(cache)[2];
	    val = scheme_hash_get(ht, key);
	    if (val) {
	      /* In the hash table, null_val_key is used to indicate
		 that there's no value for the key. */
	      if (SAME_OBJ(val, null_val_key))
		val = NULL;
	      break;
	    }
	  }
	}
      }
    }

    pos = (long)MZ_CONT_MARK_STACK - findpos;
    if (pos > 16) {
      pos >>= 1;
      findpos = findpos + pos;
      seg = p->cont_mark_stack_segments[findpos >> SCHEME_LOG_MARK_SEGMENT_SIZE];
      pos = findpos & SCHEME_MARK_SEGMENT_MASK;
      cache = seg[pos].cache;
      if (!cache || !SCHEME_VECTORP(cache)) {
	/* No cache so far, so map one key */
	cache = scheme_make_vector(3, NULL);
	if (seg[pos].cache && !SCHEME_FALSEP(seg[pos].cache))
	  SCHEME_VEC_ELS(cache)[0] = seg[pos].cache;
	SCHEME_VEC_ELS(cache)[1] = key;
	SCHEME_VEC_ELS(cache)[2] = val;
	seg[pos].cache = cache;
      } else {
	if (!null_val_key) {
	  REGISTER_SO(null_val_key);
	  null_val_key = scheme_make_symbol("nul");
	}

	if (SCHEME_VEC_ELS(cache)[1]) {
	  /* More than one cached key, now; create hash table */
	  Scheme_Hash_Table *ht;
	  Scheme_Object *v2;
	  ht = scheme_make_hash_table(SCHEME_hash_ptr);
	  scheme_hash_set(ht, key, val ? val : null_val_key);
	  v2 = SCHEME_VEC_ELS(cache)[2];
	  scheme_hash_set(ht, SCHEME_VEC_ELS(cache)[1], v2 ? v2 : null_val_key);
	  SCHEME_VEC_ELS(cache)[1] = NULL;
	  SCHEME_VEC_ELS(cache)[2] = (Scheme_Object *)ht;
	} else {
	  /* Already have a hash table */
	  Scheme_Hash_Table *ht;
	  ht = (Scheme_Hash_Table *)SCHEME_VEC_ELS(cache)[2];
	  scheme_hash_set(ht, key, val ? val : null_val_key);
	}
      }
    }

    if (val)
      return val;
  }
  
  if (key == scheme_parameterization_key) {
    return (Scheme_Object *)scheme_current_thread->init_config;
  }
  if (key == scheme_break_enabled_key) {
    return scheme_current_thread->init_break_cell;
  }
  
  return NULL;
}

static Scheme_Object *
extract_one_cc_mark(int argc, Scheme_Object *argv[])
{
  Scheme_Object *r;

  if (SCHEME_TRUEP(argv[0])
      && !SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_cont_mark_set_type))
    scheme_wrong_type("continuation-mark-set-first", "continuation-mark-set or #f", 0, argc, argv);
  
  r = scheme_extract_one_cc_mark(SCHEME_TRUEP(argv[0]) ? argv[0] : NULL, argv[1]);
  if (!r) {
    if (argc > 2)
      r = argv[2];
    else
      r = scheme_false;
  }

  return r;
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
  volatile int save_count;
  Scheme_Thread *p;

  p = scheme_current_thread;

  dw = MALLOC_ONE_RT(Scheme_Dynamic_Wind);
#ifdef MZTAG_REQUIRED
  dw->type = scheme_rt_dyn_wind;
#endif

  dw->data = data;
  dw->pre = pre;
  dw->post = post;
  dw->prev = p->dw;

  if (pre) {
    p->suspend_break++;
    pre(data);
    p = scheme_current_thread;
    --p->suspend_break;
  }

  p->dw = dw;
  
  dw->saveerr = scheme_current_thread->error_buf;
  scheme_current_thread->error_buf = &newbuf;

  scheme_save_env_stack_w_thread(dw->envss, p);

  if (scheme_setjmp(newbuf)) {
    p = scheme_current_thread;
    scheme_restore_env_stack_w_thread(dw->envss, p);
    if (p->dw != dw) {
      /* Apparently, a full continuation jump was interrupted by an
	 escape continuation jump (in a dw pre or post thunk). Either

	 1) this dw's post is already done for an interupted upward
	 jump; or

	 2) we never actually got this far for an interrupted
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

  p->dw = dw->prev;

  /* Don't run Scheme-based dyn-winds when we're killing a nested thread. */
  if (err && p->cjs.is_kill && (post == post_dyn_wind))
    post = NULL;

  if (post) {
    p->error_buf = &newbuf;
    if (scheme_setjmp(newbuf)) {
      p = scheme_current_thread;
      scheme_restore_env_stack_w_thread(dw->envss, p);
      err = 1;
    } else {
      p = scheme_current_thread;
      p->suspend_break++;
      post(data);
      p = scheme_current_thread;
      --p->suspend_break;
    }
  }

  if (err)
    scheme_longjmp(*dw->saveerr, 1);

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
  return now.time * 1000 + now.millitm;
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
  return now.time;
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

  if (SCHEME_FALSEP(scheme_get_or_check_arity(argv[0], num_rands))) {
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
  Scheme_Object *name;

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

  return CONS(scheme_make_integer(SCHEME_CLOSURE_DATA_FLAGS(data)),
	      CONS(scheme_make_integer(data->num_params),
		   CONS(scheme_make_integer(data->max_let_depth),
			CONS(name,
			     CONS(scheme_make_svector(data->closure_size,
						      data->closure_map),
				  scheme_protect_quote(data->code))))));
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
  /* v is an svector */

  data->code = obj;

  if (!SAME_TYPE(scheme_svector_type, SCHEME_TYPE(v))) return NULL;

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
