/*
  MzScheme
  Copyright (c) 2004-2005 PLT Scheme, Inc.
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

/* locals */
static Scheme_Object *procedure_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *apply (int argc, Scheme_Object *argv[]);
static Scheme_Object *map (int argc, Scheme_Object *argv[]);
static Scheme_Object *for_each (int argc, Scheme_Object *argv[]);
static Scheme_Object *andmap (int argc, Scheme_Object *argv[]);
static Scheme_Object *ormap (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_cc (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_with_continuation_barrier (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_with_sema (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_with_sema_enable_break (int argc, Scheme_Object *argv[]);
static Scheme_Object *cc_marks (int argc, Scheme_Object *argv[]);
static Scheme_Object *cont_marks (int argc, Scheme_Object *argv[]);
static Scheme_Object *cc_marks_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *extract_cc_marks (int argc, Scheme_Object *argv[]);
static Scheme_Object *extract_cc_markses (int argc, Scheme_Object *argv[]);
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
static Scheme_Object *primitive_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *primitive_closure_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *primitive_result_arity (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_with_values(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_values(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_print(int argc, Scheme_Object **argv);
static Scheme_Object *current_prompt_read(int, Scheme_Object **);

static Scheme_Object *get_or_check_arity(Scheme_Object *p, long a);

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

  scheme_add_global_constant("procedure?",
			     scheme_make_folding_prim(procedure_p,
						      "procedure?",
						      1, 1, 1),
			     env);
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
				0, -1),
  scheme_add_global_constant("call-with-escape-continuation", o, env);
  scheme_add_global_constant("call/ec", o, env);

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
  is_method_symbol = scheme_intern_symbol("method-arity-error");
  scheme_inferred_name_symbol = scheme_intern_symbol("inferred-name");
}

Scheme_Object *
scheme_make_void (void)
{
  return scheme_void;
}

/*========================================================================*/
/*                          primitive procedures                          */
/*========================================================================*/

Scheme_Object *
scheme_make_prim_w_everything(Scheme_Prim *fun, int eternal,
			      const char *name,
			      mzshort mina, mzshort maxa,
			      short folding,
			      mzshort minr, mzshort maxr)
{
  Scheme_Primitive_Proc *prim;
  int hasr, size;

  hasr = ((minr != 1) || (maxr != 1));
  size = hasr ? sizeof(Scheme_Prim_W_Result_Arity) : sizeof(Scheme_Primitive_Proc);

  if (eternal && scheme_starting_up)
    prim = (Scheme_Primitive_Proc *)scheme_malloc_eternal_tagged(size);
  else
    prim = (Scheme_Primitive_Proc *)scheme_malloc_tagged(size);
  prim->pp.so.type = scheme_prim_type;
  SCHEME_PRIM(prim) = fun;
  prim->name = name;
  prim->mina = mina;
  prim->maxa = maxa;
  prim->pp.flags = ((folding ? SCHEME_PRIM_IS_FOLDING : 0)
		    | (scheme_defining_primitives ? SCHEME_PRIM_IS_PRIMITIVE : 0)
		    | (hasr ? SCHEME_PRIM_IS_MULTI_RESULT : 0));

  if (hasr) {
    ((Scheme_Prim_W_Result_Arity *)prim)->minr = minr;
    ((Scheme_Prim_W_Result_Arity *)prim)->maxr = maxr;
  }

  return (Scheme_Object *)prim;
}

Scheme_Object *scheme_make_prim(Scheme_Prim *fun)
{
  return scheme_make_prim_w_everything(fun, 1, NULL, 0, -1, 0, 1, 1);
}

Scheme_Object *
scheme_make_noneternal_prim (Scheme_Prim *fun)
{
  return scheme_make_prim_w_everything(fun, 0, NULL, 0, -1, 0, 1, 1);
}

Scheme_Object *
scheme_make_prim_w_arity(Scheme_Prim *fun, const char *name,
			 mzshort mina, mzshort maxa)
{
  return scheme_make_prim_w_everything(fun, 1, name, mina, maxa, 0, 1, 1);
}

Scheme_Object *
scheme_make_folding_prim(Scheme_Prim *fun, const char *name,
			 mzshort mina, mzshort maxa,
			 short folding)
{
  return scheme_make_prim_w_everything(fun, 1, name, mina, maxa,
				       folding, 1, 1);
}

Scheme_Object *
scheme_make_noneternal_prim_w_arity(Scheme_Prim *fun, const char *name,
				    mzshort mina, mzshort maxa)
{
  return scheme_make_prim_w_everything(fun, 0, name, mina, maxa, 0, 1, 1);
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
  Scheme_Object **runstack, **dest;
  mzshort *map;
  int i;

  data = (Scheme_Closure_Data *)code;

  i = data->closure_size;

  closure = (Scheme_Closure *)
    scheme_malloc_tagged(sizeof(Scheme_Closure)
			 + (i - 1) * sizeof(Scheme_Object *));

  closure->so.type = scheme_closure_type;
  SCHEME_COMPILED_CLOS_CODE(closure) = data;

#ifdef MZ_PRECISE_GC
  closure->closure_size = i; /* needed for getting the object's size in bytes */
#else
  closure->zero_sized = !i; /* info used for reporting certain arity errors */
#endif

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

/* Closure_Info is used to store extra closure information
   before a closure mapping is resolved. */
typedef struct {
  MZTAG_IF_REQUIRED
  int *local_flags;
  mzshort base_closure_size; /* doesn't include top-level (if any) */
  mzshort *base_closure_map;
  short has_tl;
} Closure_Info;

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

  if (!data->closure_size)
    /* If the closure is empty, go ahead and finalize closure */
    return scheme_make_closure(NULL, (Scheme_Object *)data, 0);
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

    src[0] = 0;
    if (cstx->srcloc->src && SCHEME_PATHP(cstx->srcloc->src)) {
      if (SCHEME_BYTE_STRLEN_VAL(cstx->srcloc->src) < 20)
	memcpy(src, SCHEME_BYTE_STR_VAL(cstx->srcloc->src), SCHEME_BYTE_STRLEN_VAL(cstx->srcloc->src) + 1);
      else {
	memcpy(src, SCHEME_BYTE_STR_VAL(cstx->srcloc->src) + SCHEME_BYTE_STRLEN_VAL(cstx->srcloc->src) - 19, 20);
	src[0] = '.';
	src[1] = '.';
	src[2] = '.';
      }
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

Scheme_Object *
scheme_make_closure_compilation(Scheme_Comp_Env *env, Scheme_Object *code,
				Scheme_Compile_Info *rec, int drec)
     /* Compiles a `lambda' expression */
{
  Scheme_Object *allparams, *params, *forms, *param, *name;
  Scheme_Closure_Data *data;
  Scheme_Compile_Info lam;
  Scheme_Comp_Env *frame;
  Closure_Info *cl;
  int i;
  long num_params;
  mzshort dcs, *dcm;

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

  name = scheme_stx_property(code, scheme_inferred_name_symbol, NULL);
  if (name && SCHEME_SYMBOLP(name)) {
    data->name = name;
  } else {
    data->name = rec[drec].value_name;
    if (!data->name || SCHEME_FALSEP(data->name)) {
      name = scheme_source_to_name(code);
      data->name= name;
    }
  }

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

  /* Remembers positions of used vars (and unsets usage for this level) */
  scheme_env_make_closure_map(frame, &dcs, &dcm);
  cl->base_closure_size = dcs;
  cl->base_closure_map = dcm;
  if (scheme_env_uses_toplevel(frame))
    cl->has_tl = 1;

  data->closure_size = (cl->base_closure_size
			+ (cl->has_tl ? 1 : 0));
  data->closure_map = (mzshort *)cl;

  data->max_let_depth = lam.max_let_depth + data->num_params + data->closure_size;

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
	  void *p1, *p2, *p3, *p4;
	  int i1, i2, i3;

	  p1 = pp->ku.k.p1;
	  p2 = pp->ku.k.p2;
	  p3 = pp->ku.k.p3;
	  p4 = pp->ku.k.p4;
	  i1 = pp->ku.k.i1;
	  i2 = pp->ku.k.i2;
	  i3 = pp->ku.k.i3;

	  /* stack overflow is a lot of work; force a sleep */
	  scheme_thread_block(0);
	  pp->ran_some = 1;

	  pp->ku.k.p1 = p1;
	  pp->ku.k.p2 = p2;
	  pp->ku.k.p3 = p3;
	  pp->ku.k.p4 = p4;
	  pp->ku.k.i1 = i1;
	  pp->ku.k.i2 = i2;
	  pp->ku.k.i3 = i3;

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

Scheme_Object *
scheme_force_value(Scheme_Object *obj)
     /* Called where _scheme_apply() or _scheme_value() might return a
	a tail-call-waiting trampoline token.  */
{
  if (SAME_OBJ(obj, SCHEME_TAIL_CALL_WAITING)) {
    Scheme_Thread *p = scheme_current_thread;
    Scheme_Object *v;

    /* Watch out for use for use of tail buffer: */
    if (p->ku.apply.tail_rands == p->tail_buffer) {
      GC_CAN_IGNORE Scheme_Object **tb;
      p->tail_buffer = NULL; /* so args aren't zeroed */
      tb = MALLOC_N(Scheme_Object *, p->tail_buffer_size);
      p->tail_buffer = tb;
    }

    v = _scheme_apply_multi(p->ku.apply.tail_rator,
			    p->ku.apply.tail_num_rands,
			    p->ku.apply.tail_rands);
    return v;
  } else if (SAME_OBJ(obj, SCHEME_EVAL_WAITING)) {
    Scheme_Thread *p = scheme_current_thread;
    return _scheme_eval_linked_expr_multi(p->ku.eval.wait_expr);
  } else if (obj)
    return obj;
  else
    return scheme_void;
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

static Scheme_Object *get_or_check_arity(Scheme_Object *p, long a)
/* a == -1 => get arity
   a == -2 => check for allowing varargs */
{
  Scheme_Type type;
  mzshort mina, maxa;
  int drop = 0;

 top:

  type = SCHEME_TYPE(p);
  if (type == scheme_prim_type) {
    mina = ((Scheme_Primitive_Proc *)p)->mina;
    maxa = ((Scheme_Primitive_Proc *)p)->maxa;
  } else if (type == scheme_closed_prim_type) {
    mina = ((Scheme_Closed_Primitive_Proc *)p)->mina;
    maxa = ((Scheme_Closed_Primitive_Proc *)p)->maxa;

    if (mina == -2) {
      mzshort *cases = ((Scheme_Closed_Case_Primitive_Proc *)p)->cases;
      int count = -maxa, i;

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

  if (!SCHEME_PROCP(p) || SCHEME_FALSEP(get_or_check_arity(p, a))) {
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
    Scheme_Closure_Data *data;

    data = SCHEME_COMPILED_CLOS_CODE(p);
    if (data->name) {
      if (for_error < 0) {
	s = (char *)data->name;
	*len = -1;
      } else {
	*len = SCHEME_SYM_LEN(data->name);
	s = scheme_symbol_val(data->name);
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
  return get_or_check_arity(p, -1);
}

static Scheme_Object *procedure_arity(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_PROCP(argv[0]))
    scheme_wrong_type("procedure-arity", "procedure", 0, argc, argv);

  return get_or_check_arity(argv[0], -1);
}

static Scheme_Object *procedure_arity_includes(int argc, Scheme_Object *argv[])
{
  long n;

  if (!SCHEME_PROCP(argv[0]))
    scheme_wrong_type("procedure-arity-includes?", "procedure", 0, argc, argv);

  n = scheme_extract_index("procedure-arity-includes?", 1, argc, argv, -2, 0);

  return get_or_check_arity(argv[0], n);
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

  if (SCHEME_FALSEP(get_or_check_arity(argv[0], argc - 1))) {
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

Scheme_Object *
scheme_call_ec (int argc, Scheme_Object *argv[])
{
  mz_jmp_buf newbuf;
  Scheme_Escaping_Cont * volatile cont;
  Scheme_Thread *p1 = scheme_current_thread;
  Scheme_Object * volatile v;
  Scheme_Object *mark_key, *a[1];
  Scheme_Cont_Frame_Data volatile cframe;

  scheme_check_proc_arity("call-with-escaping-continuation", 1,
			  0, argc, argv);

  mark_key = scheme_make_pair(scheme_false, scheme_false);

  cont = MALLOC_ONE_TAGGED(Scheme_Escaping_Cont);
  cont->so.type = scheme_escaping_cont_type;
  cont->mark_key = mark_key;
  cont->suspend_break = p1->suspend_break;
  copy_cjs(&cont->cjs, &p1->cjs);

  cont->saveerr = p1->error_buf;
  p1->error_buf = &newbuf;

  scheme_save_env_stack_w_thread(cont->envss, p1);

  scheme_push_continuation_frame((Scheme_Cont_Frame_Data *)&cframe);
  scheme_set_cont_mark(mark_key, scheme_true);

  if (scheme_setjmp(newbuf)) {
    Scheme_Thread *p2 = scheme_current_thread;
    if ((void *)p2->cjs.jumping_to_continuation == cont) {
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
    a[0] = (Scheme_Object *)cont;
    v = _scheme_apply_multi(argv[0], 1, a);
  }

  p1 = scheme_current_thread;

  p1->error_buf = cont->saveerr;

  scheme_pop_continuation_frame((Scheme_Cont_Frame_Data *)&cframe);

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

#define TOTAL_STACK_SIZE (sizeof(Scheme_Object*) * SCHEME_STACK_SIZE)

static Scheme_Saved_Stack *copy_out_runstack(Scheme_Thread *p,
					     Scheme_Object **runstack,
					     Scheme_Object **runstack_start)
{
  Scheme_Saved_Stack *saved, *isaved, *csaved;
  long size;

  /* Copy out stack: */
  saved = MALLOC_ONE_RT(Scheme_Saved_Stack);
#ifdef MZTAG_REQUIRED
  saved->type = scheme_rt_saved_stack;
#endif
  size = p->runstack_size - (runstack XFORM_OK_MINUS runstack_start);
  saved->runstack_size = size;
  {
    Scheme_Object **start;
    start = MALLOC_N(Scheme_Object*, size);
    saved->runstack_start = start;
  }
  memcpy(saved->runstack_start, runstack, size * sizeof(Scheme_Object *));
  isaved = saved;
  for (csaved = p->runstack_saved; csaved; csaved = csaved->prev) {
    {
      Scheme_Saved_Stack *ss;
      ss = MALLOC_ONE_RT(Scheme_Saved_Stack);
#ifdef MZTAG_REQUIRED
      ss->type = scheme_rt_saved_stack;
#endif
      isaved->prev = ss;
    }
    isaved = isaved->prev;
    size = csaved->runstack_size - (csaved->runstack XFORM_OK_MINUS csaved->runstack_start);
    isaved->runstack_size = size;
    {
      Scheme_Object **start;
      start = MALLOC_N(Scheme_Object*, size);
      isaved->runstack_start = start;
    }
    memcpy(isaved->runstack_start, csaved->runstack, size * sizeof(Scheme_Object *));
  }
  isaved->prev = NULL;

  return saved;
}

static Scheme_Cont_Mark *copy_out_mark_stack(Scheme_Thread *p, 
					     MZ_MARK_POS_TYPE pos)
{
  long cmcount;
  Scheme_Cont_Mark *cont_mark_stack_copied;

  /* Copy cont mark stack: */
  cmcount = (long)pos;
  if (cmcount) {
    cont_mark_stack_copied = MALLOC_N(Scheme_Cont_Mark, cmcount);
    while (cmcount--) {
      Scheme_Cont_Mark *seg = p->cont_mark_stack_segments[cmcount >> SCHEME_LOG_MARK_SEGMENT_SIZE];
      long pos = cmcount & SCHEME_MARK_SEGMENT_MASK;
      Scheme_Cont_Mark *cm = seg + pos;
      
      memcpy(cont_mark_stack_copied + cmcount, cm, sizeof(Scheme_Cont_Mark));
    }
    
    return cont_mark_stack_copied;
  } else
    return NULL;
}

static void copy_in_runstack(Scheme_Thread *p, Scheme_Saved_Stack *isaved)
{
  Scheme_Saved_Stack *csaved;
  long size;

  size = isaved->runstack_size;
  MZ_RUNSTACK = MZ_RUNSTACK_START + (p->runstack_size - size);
  memcpy(MZ_RUNSTACK, isaved->runstack_start, size * sizeof(Scheme_Object *));
  for (csaved = p->runstack_saved; csaved; csaved = csaved->prev) {
    isaved = isaved->prev;
    size = isaved->runstack_size;
    csaved->runstack = csaved->runstack_start + (csaved->runstack_size - size);
    memcpy(csaved->runstack, isaved->runstack_start, size * sizeof(Scheme_Object *));
  }
}

static void copy_in_mark_stack(Scheme_Thread *p, Scheme_Cont_Mark *cont_mark_stack_copied,
			       MZ_MARK_STACK_TYPE cms, MZ_MARK_STACK_TYPE base_cms)
     /* Copies in the mark stack up to depth cms, but assumes that the
	stack up to depth base_cms is already in place (probably in
	place for a dynamic-wind context in an continuation
	restoration.) */
{
  long cmcount, base_cmcount;

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
  while (cmcount-- > base_cmcount) {
    Scheme_Cont_Mark *seg = p->cont_mark_stack_segments[cmcount >> SCHEME_LOG_MARK_SEGMENT_SIZE];
    long pos = cmcount & SCHEME_MARK_SEGMENT_MASK;
    Scheme_Cont_Mark *cm = seg + pos;
    
    memcpy(cm, cont_mark_stack_copied + cmcount, sizeof(Scheme_Cont_Mark));
  }
}

static Scheme_Object *
call_cc (int argc, Scheme_Object *argv[])
{
  Scheme_Object *ret;
  Scheme_Cont * volatile cont;
  Scheme_Dynamic_Wind *dw;
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Saved_Stack *saved;
  Scheme_Cont_Mark *msaved;

  scheme_check_proc_arity("call-with-current-continuation", 1,
			  0, argc, argv);

  cont = MALLOC_ONE_TAGGED(Scheme_Cont);
  cont->so.type = scheme_cont_type;
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

  /* Hide call/cc's arg off of stack */
  p->ku.k.p1 = argv[0];
  argv[0] = NULL;

  saved = copy_out_runstack(p, MZ_RUNSTACK, MZ_RUNSTACK_START);
  cont->runstack_copied = saved;
  msaved = copy_out_mark_stack(p, MZ_CONT_MARK_STACK);
  cont->cont_mark_stack_copied = msaved;

  cont->runstack_owner = p->runstack_owner;
  cont->cont_mark_stack_owner = p->cont_mark_stack_owner;

  cont->stack_start = p->stack_start;
  cont->o_start = p->o_start;

  cont->savebuf = p->error_buf;

  scheme_zero_unneeded_rands(p);

  scheme_flatten_config(scheme_current_config());

  if (scheme_setjmpup(&cont->buf, cont, p->next ? p->stack_start : p->o_start)) {
    /* We arrive here when the continuation is applied */
    MZ_MARK_STACK_TYPE copied_cms = 0;
    Scheme_Object *result, **mv;
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
	saved = copy_out_runstack(op, op->runstack, op->runstack_start);
	op->runstack_swapped = saved;
      }
      *p->runstack_owner = p;
    }

    /* Copy stack back in: (p->runstack and p->runstack_saved arrays
       are already restored, so the shape is certainly the same as
       when cont->runstack_copied was made) */
    copy_in_runstack(p, cont->runstack_copied);
    
    if (p->cont_mark_stack_owner
	&& (*p->cont_mark_stack_owner == p))
      *p->cont_mark_stack_owner = NULL;

    p->cont_mark_stack_owner = cont->cont_mark_stack_owner;
    if (p->cont_mark_stack_owner
	&& (*p->cont_mark_stack_owner != p)) {
      Scheme_Thread *op;
      op = *p->cont_mark_stack_owner;
      if (op) {
	msaved = copy_out_mark_stack(op, op->cont_mark_stack);
	op->cont_mark_stack_swapped = msaved;
      }
      *p->cont_mark_stack_owner = p;
      /* In case there's a GC before we copy in marks: */
      MZ_CONT_MARK_STACK = 0;
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
			     MZ_CONT_MARK_STACK, copied_cms);

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
		       MZ_CONT_MARK_STACK, copied_cms);
    
    /* We may have just re-activated breaking: */
    scheme_check_break_now();

    if (SAME_OBJ(result, SCHEME_MULTIPLE_VALUES)) {
      p->ku.multiple.array = mv;
      p->ku.multiple.count = mc;
    }

    return result;
  } else {
    Scheme_Object *argv2[1];

    /* Restore call/cc's arg to stack. */
    /* (We aren't actually allowed to modify argv! :) */
    argv[0] = p->ku.k.p1;
    p->ku.k.p1 = NULL;

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
      swapped = copy_out_runstack(op, op->runstack, op->runstack_start);
      op->runstack_swapped = swapped;
    }
    *(p->runstack_owner) = p;
    copy_in_runstack(p, p->runstack_swapped);
    p->runstack_swapped = NULL;
  }

  if (p->cont_mark_stack_owner && ((*p->cont_mark_stack_owner) != p)) {
    Scheme_Thread *op;
    Scheme_Cont_Mark *swapped;
    op = *p->cont_mark_stack_owner;
    if (op) {
      swapped = copy_out_mark_stack(op, op->cont_mark_stack);
      op->cont_mark_stack_swapped = swapped;
    }
    *(p->cont_mark_stack_owner) = p;
    copy_in_mark_stack(p, p->cont_mark_stack_swapped, MZ_CONT_MARK_STACK, 0);
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
  long findpos;
  long cmpos;

  if (cont) {
    findpos = (long)cont->ss.cont_mark_stack;
    cmpos = (long)cont->ss.cont_mark_pos;
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
      find = cont->cont_mark_stack_copied;
      pos = findpos;
    } else {
      GC_CAN_IGNORE Scheme_Cont_Mark *seg;

      seg = p->cont_mark_stack_segments[findpos >> SCHEME_LOG_MARK_SEGMENT_SIZE];
      pos = findpos & SCHEME_MARK_SEGMENT_MASK;
      find = seg;
    }

    if (find[pos].cached_chain) {
      if (last)
	last->next = find[pos].cached_chain;
      else
	first = find[pos].cached_chain;

      break;
    } else {
      Scheme_Cont_Mark_Chain *pr;
      pr = MALLOC_ONE_RT(Scheme_Cont_Mark_Chain);
#ifdef MZTAG_REQUIRED
      pr->type = scheme_rt_cont_mark_chain;
#endif
      pr->key = find[pos].key;
      pr->val = find[pos].val;
      pr->pos = find[pos].pos;
      pr->next = NULL;
      find[pos].cached_chain = pr;
      if (last)
	last->next = pr;
      else
	first = pr;

      last = pr;
    }
  }

  if (just_chain)
    return (Scheme_Object *)first;

  set = MALLOC_ONE_TAGGED(Scheme_Cont_Mark_Set);
  set->so.type = scheme_cont_mark_set_type;
  set->chain = first;
  set->cmpos = cmpos;

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
scheme_extract_one_cc_mark(Scheme_Object *mark_set, Scheme_Object *key)
{
  Scheme_Cont_Mark_Chain *chain;

  if (mark_set) {
    chain = ((Scheme_Cont_Mark_Set *)mark_set)->chain;
  } else {
    chain = (Scheme_Cont_Mark_Chain *)continuation_marks(scheme_current_thread, NULL, NULL, 1);
  }
  
  while (chain) {
    if (chain->key == key)
      return chain->val;
    else 
      chain = chain->next;
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

  if (SCHEME_FALSEP(get_or_check_arity(argv[0], num_rands))) {
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

  data = (Scheme_Closure_Data *)obj;

  return CONS(scheme_make_integer(SCHEME_CLOSURE_DATA_FLAGS(data)),
	      CONS(scheme_make_integer(data->num_params),
		   CONS(scheme_make_integer(data->max_let_depth),
			CONS(data->name ? data->name : scheme_null,
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

  if (!data->closure_size)
    /* If the closure is empty, go ahead and finalize */
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
  GC_REG_TRAV(scheme_rt_cont_mark_chain, mark_cont_mark_chain);
}

END_XFORM_SKIP;

#endif
