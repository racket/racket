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
*/

/* This file implements most of the built-in syntactic forms, except
   the module-related forms (which are in module.c) and certain
   aspects of the most primitive forms, such as application (handled
   in eval.c) and functions (in fun.c).

   A primitive syntactic form consists of an expander, called by
   `expand' and related functions, and a compiler, used by `compile'
   and `eval'. (Compilation does *not* expand primitive forms first,
   but instead peforms any necessary expansion directly.) */

#include "schpriv.h"
#include "schmach.h"

/* globals */
Scheme_Object *scheme_define_values_syntax, *scheme_define_syntaxes_syntax;
Scheme_Object *scheme_ref_syntax;
Scheme_Object *scheme_begin_syntax;
Scheme_Object *scheme_lambda_syntax;
Scheme_Object *scheme_compiled_void_code;
Scheme_Object scheme_undefined[1];

Scheme_Syntax_Optimizer scheme_syntax_optimizers[_COUNT_EXPD_];
Scheme_Syntax_Resolver scheme_syntax_resolvers[_COUNT_EXPD_];
Scheme_Syntax_Validater scheme_syntax_validaters[_COUNT_EXPD_];
Scheme_Syntax_Executer scheme_syntax_executers[_COUNT_EXPD_];
Scheme_Syntax_Jitter scheme_syntax_jitters[_COUNT_EXPD_];
Scheme_Syntax_Cloner scheme_syntax_cloners[_COUNT_EXPD_];
int scheme_syntax_protect_afters[_COUNT_EXPD_];

/* locals */
static Scheme_Object *lambda_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *lambda_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *define_values_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *define_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *ref_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *ref_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *quote_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *quote_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *if_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *if_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *set_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *set_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *case_lambda_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *case_lambda_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *let_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *let_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *let_star_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *let_star_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *letrec_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *letrec_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *let_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *let_values_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *let_star_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *let_star_values_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *letrec_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *letrec_values_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *begin_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *begin_expand (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *begin0_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *begin0_expand (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);

static Scheme_Object *unquote_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *unquote_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);

static Scheme_Object *with_cont_mark_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *with_cont_mark_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);

static Scheme_Object *quote_syntax_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *quote_syntax_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *define_syntaxes_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *define_syntaxes_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *define_for_syntaxes_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *define_for_syntaxes_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *letrec_syntaxes_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *letrec_syntaxes_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);

static Scheme_Object *define_values_execute(Scheme_Object *data);
static Scheme_Object *ref_execute(Scheme_Object *data);
static Scheme_Object *set_execute(Scheme_Object *data);
static Scheme_Object *define_syntaxes_execute(Scheme_Object *expr);
static Scheme_Object *define_for_syntaxes_execute(Scheme_Object *expr);
static Scheme_Object *case_lambda_execute(Scheme_Object *expr);
static Scheme_Object *begin0_execute(Scheme_Object *data);

static Scheme_Object *bangboxenv_execute(Scheme_Object *data);
static Scheme_Object *bangboxvalue_execute(Scheme_Object *data);

static Scheme_Object *define_values_optimize(Scheme_Object *data, Optimize_Info *info);
static Scheme_Object *ref_optimize(Scheme_Object *data, Optimize_Info *info);
static Scheme_Object *set_optimize(Scheme_Object *data, Optimize_Info *info);
static Scheme_Object *define_syntaxes_optimize(Scheme_Object *expr, Optimize_Info *info);
static Scheme_Object *define_for_syntaxes_optimize(Scheme_Object *expr, Optimize_Info *info);
static Scheme_Object *case_lambda_optimize(Scheme_Object *expr, Optimize_Info *info);
static Scheme_Object *begin0_optimize(Scheme_Object *data, Optimize_Info *info);

static Scheme_Object *begin0_clone(Scheme_Object *data, Optimize_Info *info, int delta, int closure_depth);
static Scheme_Object *set_clone(Scheme_Object *data, Optimize_Info *info, int delta, int closure_depth);

static Scheme_Object *define_values_resolve(Scheme_Object *data, Resolve_Info *info);
static Scheme_Object *ref_resolve(Scheme_Object *data, Resolve_Info *info);
static Scheme_Object *set_resolve(Scheme_Object *data, Resolve_Info *info);
static Scheme_Object *define_syntaxes_resolve(Scheme_Object *expr, Resolve_Info *info);
static Scheme_Object *define_for_syntaxes_resolve(Scheme_Object *expr, Resolve_Info *info);
static Scheme_Object *case_lambda_resolve(Scheme_Object *expr, Resolve_Info *info);
static Scheme_Object *begin0_resolve(Scheme_Object *data, Resolve_Info *info);

static void define_values_validate(Scheme_Object *data, Mz_CPort *port, char *stack, int depth, int letlimit, int delta, 
				   int num_toplevels, int num_stxes);
static void ref_validate(Scheme_Object *data, Mz_CPort *port, char *stack, int depth, int letlimit, int delta, 
			 int num_toplevels, int num_stxes);
static void set_validate(Scheme_Object *data, Mz_CPort *port, char *stack, int depth, int letlimit, int delta, 
			 int num_toplevels, int num_stxes);
static void define_syntaxes_validate(Scheme_Object *data, Mz_CPort *port, char *stack, int depth, int letlimit, int delta, 
				     int num_toplevels, int num_stxes);
static void define_for_syntaxes_validate(Scheme_Object *data, Mz_CPort *port, char *stack, int depth, int letlimit, int delta, 
					 int num_toplevels, int num_stxes);
static void case_lambda_validate(Scheme_Object *data, Mz_CPort *port, char *stack, int depth, int letlimit, int delta, 
				 int num_toplevels, int num_stxes);
static void begin0_validate(Scheme_Object *data, Mz_CPort *port, char *stack, int depth, int letlimit, int delta,
			    int num_toplevels, int num_stxes);
static void bangboxenv_validate(Scheme_Object *data, Mz_CPort *port, char *stack, int depth, int letlimit, int delta,
				int num_toplevels, int num_stxes);
static void bangboxvalue_validate(Scheme_Object *data, Mz_CPort *port, char *stack, int depth, int letlimit, int delta,
				  int num_toplevels, int num_stxes);

static Scheme_Object *define_values_jit(Scheme_Object *data);
static Scheme_Object *ref_jit(Scheme_Object *data);
static Scheme_Object *set_jit(Scheme_Object *data);
static Scheme_Object *define_syntaxes_jit(Scheme_Object *expr);
static Scheme_Object *define_for_syntaxes_jit(Scheme_Object *expr);
static Scheme_Object *case_lambda_jit(Scheme_Object *expr);
static Scheme_Object *begin0_jit(Scheme_Object *data);
static Scheme_Object *bangboxvalue_jit(Scheme_Object *data);

static Scheme_Object *named_let_syntax (Scheme_Object *form, Scheme_Comp_Env *env, 
					Scheme_Compile_Expand_Info *rec, int drec);

static Scheme_Object *expand_lam(int argc, Scheme_Object **argv);

static Scheme_Object *write_let_value(Scheme_Object *obj);
static Scheme_Object *read_let_value(Scheme_Object *obj);
static Scheme_Object *write_let_void(Scheme_Object *obj);
static Scheme_Object *read_let_void(Scheme_Object *obj);
static Scheme_Object *write_letrec(Scheme_Object *obj);
static Scheme_Object *read_letrec(Scheme_Object *obj);
static Scheme_Object *write_let_one(Scheme_Object *obj);
static Scheme_Object *read_let_one(Scheme_Object *obj);
static Scheme_Object *write_top(Scheme_Object *obj);
static Scheme_Object *read_top(Scheme_Object *obj);
static Scheme_Object *write_case_lambda(Scheme_Object *obj);
static Scheme_Object *read_case_lambda(Scheme_Object *obj);

/* symbols */
static Scheme_Object *lambda_symbol;
static Scheme_Object *letrec_symbol;
static Scheme_Object *let_star_symbol;
static Scheme_Object *let_symbol;
static Scheme_Object *letrec_values_symbol;
static Scheme_Object *let_star_values_symbol;
static Scheme_Object *let_values_symbol;
static Scheme_Object *begin_symbol;
static Scheme_Object *disappeared_binding_symbol;

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

#define cons(a,b) scheme_make_pair(a,b)
#define icons(a,b) scheme_make_immutable_pair(a,b)

#define max(a, b) (((a) > (b)) ? (a) : (b))

#define MAX_PROC_INLINE_SIZE 32

/**********************************************************************/
/*                          initialization                            */
/**********************************************************************/

void 
scheme_init_syntax (Scheme_Env *env)
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  REGISTER_SO(scheme_define_values_syntax);
  REGISTER_SO(scheme_define_syntaxes_syntax);
  REGISTER_SO(scheme_lambda_syntax);
  REGISTER_SO(scheme_begin_syntax);
  REGISTER_SO(scheme_compiled_void_code);

  REGISTER_SO(lambda_symbol);
  REGISTER_SO(letrec_symbol);
  REGISTER_SO(let_star_symbol);
  REGISTER_SO(let_symbol);
  REGISTER_SO(letrec_values_symbol);
  REGISTER_SO(let_star_values_symbol);
  REGISTER_SO(let_values_symbol);
  REGISTER_SO(begin_symbol);
  REGISTER_SO(disappeared_binding_symbol);

  scheme_undefined->type = scheme_undefined_type;
  
  lambda_symbol = scheme_intern_symbol("lambda");

  letrec_symbol = scheme_intern_symbol("letrec");
  let_star_symbol = scheme_intern_symbol("let*");
  let_symbol = scheme_intern_symbol("let");

  letrec_values_symbol = scheme_intern_symbol("letrec-values");
  let_star_values_symbol = scheme_intern_symbol("let*-values");
  let_values_symbol = scheme_intern_symbol("let-values");

  begin_symbol = scheme_intern_symbol("begin");

  disappeared_binding_symbol = scheme_intern_symbol("disappeared-binding");

  scheme_register_syntax(DEFINE_VALUES_EXPD, 
			 define_values_optimize, 
			 define_values_resolve, define_values_validate, 
			 define_values_execute, define_values_jit, 
			 NULL, 1);
  scheme_register_syntax(SET_EXPD,
			 set_optimize,
			 set_resolve, set_validate,
			 set_execute, set_jit, 
			 set_clone, 2);
  scheme_register_syntax(REF_EXPD, 
			 ref_optimize,
			 ref_resolve, ref_validate, 
			 ref_execute, ref_jit, 
			 NULL, 0);
  scheme_register_syntax(DEFINE_SYNTAX_EXPD, 
			 define_syntaxes_optimize,
			 define_syntaxes_resolve, define_syntaxes_validate,
			 define_syntaxes_execute, define_syntaxes_jit, 
			 NULL, 4);
  scheme_register_syntax(DEFINE_FOR_SYNTAX_EXPD, 
			 define_for_syntaxes_optimize, 
			 define_for_syntaxes_resolve, define_for_syntaxes_validate,
			 define_for_syntaxes_execute, define_for_syntaxes_jit, 
			 NULL, 4);
  scheme_register_syntax(CASE_LAMBDA_EXPD, 
			 case_lambda_optimize,
			 case_lambda_resolve, case_lambda_validate,
			 case_lambda_execute, case_lambda_jit, 
			 NULL, -1);
  scheme_register_syntax(BEGIN0_EXPD, 
			 begin0_optimize,
			 begin0_resolve, begin0_validate,
			 begin0_execute, begin0_jit, 
			 begin0_clone, -1);

  scheme_register_syntax(BOXENV_EXPD, 
			 NULL, NULL, bangboxenv_validate,
			 bangboxenv_execute, NULL, 
			 NULL, 1);
  scheme_register_syntax(BOXVAL_EXPD, 
			 NULL, NULL, bangboxvalue_validate,
			 bangboxvalue_execute, bangboxvalue_jit, 
			 NULL, 2);

  scheme_install_type_writer(scheme_let_value_type, write_let_value);
  scheme_install_type_reader(scheme_let_value_type, read_let_value);
  scheme_install_type_writer(scheme_let_void_type, write_let_void);
  scheme_install_type_reader(scheme_let_void_type, read_let_void);
  scheme_install_type_writer(scheme_letrec_type, write_letrec);
  scheme_install_type_reader(scheme_letrec_type, read_letrec);
  scheme_install_type_writer(scheme_let_one_type, write_let_one);
  scheme_install_type_reader(scheme_let_one_type, read_let_one);
  scheme_install_type_writer(scheme_case_lambda_sequence_type, write_case_lambda);
  scheme_install_type_reader(scheme_case_lambda_sequence_type, read_case_lambda);

  scheme_install_type_writer(scheme_compilation_top_type, write_top);
  scheme_install_type_reader(scheme_compilation_top_type, read_top);

  scheme_define_values_syntax = scheme_make_compiled_syntax(define_values_syntax, 
							    define_values_expand);
  scheme_define_syntaxes_syntax = scheme_make_compiled_syntax(define_syntaxes_syntax, 
							      define_syntaxes_expand);
  scheme_lambda_syntax = scheme_make_compiled_syntax(lambda_syntax,
						     lambda_expand);
  scheme_begin_syntax = scheme_make_compiled_syntax(begin_syntax, 
						    begin_expand);
  
  scheme_add_global_keyword("lambda", 
			    scheme_lambda_syntax,
			    env);
  {
    /* Graak lambda binding: */
    Scheme_Object *macro, *fn;

    fn = scheme_make_prim_w_arity(expand_lam, "\316\273", 1, 1);
    macro = scheme_alloc_small_object();
    macro->type = scheme_macro_type;
    SCHEME_PTR_VAL(macro) = fn;

    scheme_add_global_keyword("\316\273", macro, env);
  }
  scheme_add_global_keyword("define-values", scheme_define_values_syntax, env);
  scheme_add_global_keyword("quote", 
			    scheme_make_compiled_syntax(quote_syntax,
							quote_expand), 
			    env);
  scheme_add_global_keyword("if", 
			    scheme_make_compiled_syntax(if_syntax, 
							if_expand),
			    env);
  scheme_add_global_keyword("set!", 
			    scheme_make_compiled_syntax(set_syntax, 
							set_expand), 
			    env);
  scheme_add_global_keyword("#%variable-reference", 
			    scheme_make_compiled_syntax(ref_syntax,
							ref_expand), 
			    env);

  scheme_add_global_keyword("case-lambda", 
			    scheme_make_compiled_syntax(case_lambda_syntax, 
							case_lambda_expand), 
			    env);

  scheme_add_global_keyword("let", 
			    scheme_make_compiled_syntax(let_syntax, 
							let_expand), 
			    env);
  scheme_add_global_keyword("let*", 
			    scheme_make_compiled_syntax(let_star_syntax, 
							let_star_expand), 
			    env);
  scheme_add_global_keyword("letrec", 
			    scheme_make_compiled_syntax(letrec_syntax, 
						        letrec_expand), 
			    env);  
  
  scheme_add_global_keyword("let-values", 
			    scheme_make_compiled_syntax(let_values_syntax, 
							let_values_expand), 
			    env);
  scheme_add_global_keyword("let*-values", 
			    scheme_make_compiled_syntax(let_star_values_syntax, 
							let_star_values_expand), 
			    env);
  scheme_add_global_keyword("letrec-values", 
			    scheme_make_compiled_syntax(letrec_values_syntax, 
						        letrec_values_expand), 
			    env);  
  
  scheme_add_global_keyword("begin", 
			    scheme_begin_syntax, 
			    env);

  scheme_add_global_keyword("begin0", 
			    scheme_make_compiled_syntax(begin0_syntax, 
						        begin0_expand), 
			    env);

  scheme_add_global_keyword("unquote", 
			    scheme_make_compiled_syntax(unquote_syntax, 
							unquote_expand), 
			    env);
  scheme_add_global_keyword("unquote-splicing", 
			    scheme_make_compiled_syntax(unquote_syntax, 
							unquote_expand), 
			    env);

  scheme_add_global_keyword("with-continuation-mark", 
			    scheme_make_compiled_syntax(with_cont_mark_syntax, 
							with_cont_mark_expand), 
			    env);

  scheme_add_global_keyword("quote-syntax", 
			    scheme_make_compiled_syntax(quote_syntax_syntax, 
							quote_syntax_expand), 
			    env);
  scheme_add_global_keyword("define-syntaxes", scheme_define_syntaxes_syntax, env);
  scheme_add_global_keyword("define-values-for-syntax", 
			    scheme_make_compiled_syntax(define_for_syntaxes_syntax, 
							define_for_syntaxes_expand),
			    env);
  scheme_add_global_keyword("letrec-syntaxes+values", 
			    scheme_make_compiled_syntax(letrec_syntaxes_syntax, 
							letrec_syntaxes_expand), 
			    env);
}

Scheme_Object *
scheme_make_compiled_syntax(Scheme_Syntax *proc, 
			    Scheme_Syntax_Expander *eproc)
{
  Scheme_Object *syntax;

  syntax = scheme_alloc_eternal_object();
  syntax->type = scheme_syntax_compiler_type;
  SCHEME_SYNTAX(syntax) = (Scheme_Object *)proc;
  SCHEME_SYNTAX_EXP(syntax) = (Scheme_Object *)eproc;

  return syntax;
}

/**********************************************************************/
/*                            utilities                               */
/**********************************************************************/

static int check_form(Scheme_Object *form, Scheme_Object *base_form)
{
  int i;

  for (i = 0; SCHEME_STX_PAIRP(form); i++) {
    form = SCHEME_STX_CDR(form);
  }

  if (!SCHEME_STX_NULLP(form)) {
    scheme_wrong_syntax(NULL, form, base_form, "bad syntax (" IMPROPER_LIST_FORM ")");
  }

  return i;
}

static void bad_form(Scheme_Object *form, int l)
{ 
  scheme_wrong_syntax(NULL, NULL, form, 
		      "bad syntax (has %d part%s after keyword)", 
		      l - 1, (l != 2) ? "s" : "");
}

Scheme_Object *scheme_check_name_property(Scheme_Object *code, Scheme_Object *current_val)
{
  Scheme_Object *name;

  name = scheme_stx_property(code, scheme_inferred_name_symbol, NULL);
  if (name && SCHEME_SYMBOLP(name))
    return name;
  else
    return current_val;
}

/**********************************************************************/
/*                           lambda utils                             */
/**********************************************************************/

static void lambda_check(Scheme_Object *form)
{
  if (SCHEME_STX_PAIRP(form)
      && SCHEME_STX_PAIRP(SCHEME_STX_CDR(form))) {
    Scheme_Object *rest;
    rest = SCHEME_STX_CDR(form);
    if (SCHEME_STX_PAIRP(SCHEME_STX_CDR(rest)))
      return;
  }

  scheme_wrong_syntax(NULL, NULL, form, NULL);
}

static void lambda_check_args(Scheme_Object *args, Scheme_Object *form, Scheme_Comp_Env *env)
{
  Scheme_Object *v, *a;
  DupCheckRecord r;

  if (!SCHEME_STX_SYMBOLP(args)) {
    for (v = args; SCHEME_STX_PAIRP(v); v = SCHEME_STX_CDR(v)) {
      a = SCHEME_STX_CAR(v);
      scheme_check_identifier(NULL, a, NULL, env, form);
    }

    if (!SCHEME_STX_NULLP(v)) {
      if (!SCHEME_STX_SYMBOLP(v)) {
	scheme_check_identifier(NULL, v, NULL, env, form);
      }
    }

    /* Check for duplicate names: */
    scheme_begin_dup_symbol_check(&r, env);
    for (v = args; SCHEME_STX_PAIRP(v); v = SCHEME_STX_CDR(v)) {
      Scheme_Object *name;

      name = SCHEME_STX_CAR(v);
      scheme_dup_symbol_check(&r, NULL, name, "argument", form);
    }
    if (!SCHEME_STX_NULLP(v)) {
      scheme_dup_symbol_check(&r, NULL, v, "argument", form);
    }
  }
}

static Scheme_Object *
lambda_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *args;

  lambda_check(form);

  args = SCHEME_STX_CDR(form);
  args = SCHEME_STX_CAR(args);
  lambda_check_args(args, form, env);

  scheme_rec_add_certs(rec, drec, form);

  return scheme_make_closure_compilation(env, form, rec, drec);
}

static Scheme_Object *
lambda_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  Scheme_Object *args, *body, *fn;
  Scheme_Comp_Env *newenv;

  lambda_check(form);
  
  args = SCHEME_STX_CDR(form);
  args = SCHEME_STX_CAR(args);

  lambda_check_args(args, form, env);

  scheme_rec_add_certs(erec, drec, form);

  newenv = scheme_add_compilation_frame(args, env, 0, erec[drec].certs);

  body = SCHEME_STX_CDR(form);
  body = SCHEME_STX_CDR(body);
  body = scheme_datum_to_syntax(body, form, form, 0, 0);

  body = scheme_add_env_renames(body, newenv, env);

  args = scheme_add_env_renames(args, newenv, env); /* for re-expansion */

  fn = SCHEME_STX_CAR(form);

  return scheme_datum_to_syntax(icons(fn,
				      icons(args,
					    scheme_expand_block(body,
								newenv,
								erec, 
								drec))),
				form, form, 
				0, 2);
}

static Scheme_Object *expand_lam(int argc, Scheme_Object **argv)
{
  Scheme_Object *form = argv[0], *args, *fn;
  Scheme_Comp_Env *env;

  env = scheme_current_thread->current_local_env;

  lambda_check(form);
  
  args = SCHEME_STX_CDR(form);
  args = SCHEME_STX_CAR(args);

  lambda_check_args(args, form, env);

  fn = SCHEME_STX_CAR(form);
  fn = scheme_datum_to_syntax(lambda_symbol, fn, scheme_sys_wraps(env), 0, 0);
  
  args = SCHEME_STX_CDR(form);
  return scheme_datum_to_syntax(icons(fn, args), form, fn, 0, 0);
}

/**********************************************************************/
/*                           define utils                             */
/**********************************************************************/

void scheme_set_global_bucket(char *who, Scheme_Bucket *b, Scheme_Object *val,
			      int set_undef)
{
  if ((b->val || set_undef) 
      && ((b->so.type != scheme_variable_type)
	  || !(((Scheme_Bucket_With_Flags *)b)->flags & GLOB_IS_IMMUTATED)))
    b->val = val;
  else {
    if (((Scheme_Bucket_With_Home *)b)->home->module) {
      const char *msg;

      if (SCHEME_TRUEP(scheme_get_param(scheme_current_config(), MZCONFIG_ERROR_PRINT_SRCLOC)))
	msg = "%s: cannot %s: %S in module: %S";
      else
	msg = "%s: cannot %s: %S";

      scheme_raise_exn(MZEXN_FAIL_CONTRACT_VARIABLE, b->key,
		       msg,
		       who,
		       (b->val
			? "change identifier that is instantiated as a module constant"
			: "set identifier before its definition"),
		       (Scheme_Object *)b->key,
		       ((Scheme_Bucket_With_Home *)b)->home->module->modname);
    } else {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT_VARIABLE, b->key,
		       "%s: cannot %s identifier: %S",
		       who,
		       b->val ? "change constant" : "set undefined",
		       (Scheme_Object *)b->key);
    }
  }
}

void scheme_install_macro(Scheme_Bucket *b, Scheme_Object *v)
{
  Scheme_Object *macro;

  macro = scheme_alloc_small_object();
  macro->type = scheme_macro_type;
  SCHEME_PTR_VAL(macro) = v;

  b->val = macro;
}

static Scheme_Object *
define_execute(Scheme_Object *vars, Scheme_Object *vals, int defmacro,
	       Resolve_Prefix *rp, Scheme_Env *dm_env)
{
  Scheme_Object *l, *name, *macro;
  int i, g, show_any;
  Scheme_Bucket *b;
  Scheme_Object **save_runstack = NULL;

  if (dm_env) {
    scheme_prepare_exp_env(dm_env);

    save_runstack = scheme_push_prefix(dm_env->exp_env, rp, NULL, NULL, 1, 1);
    vals = scheme_eval_linked_expr_multi(vals);
    if (defmacro == 2)
      dm_env = NULL;
    else
      scheme_pop_prefix(save_runstack);
  } else {
    vals = _scheme_eval_linked_expr_multi(vals);
    dm_env = NULL;
  }

  if (SAME_OBJ(vals, SCHEME_MULTIPLE_VALUES)) {
    Scheme_Object *v, **values;

    for (v = vars, i = 0; SCHEME_PAIRP(v); i++, v = SCHEME_CDR(v)) {}
    
    g = scheme_current_thread->ku.multiple.count;
    if (i == g) {
      values = scheme_current_thread->ku.multiple.array;
      scheme_current_thread->ku.multiple.array = NULL;
      if (SAME_OBJ(values, scheme_current_thread->values_buffer))
	scheme_current_thread->values_buffer = NULL;
      for (i = 0; i < g; i++, vars = SCHEME_CDR(vars)) {
	if (dm_env) {
	  b = scheme_global_keyword_bucket(SCHEME_CAR(vars), dm_env);

	  macro = scheme_alloc_small_object();
	  macro->type = scheme_macro_type;
	  SCHEME_PTR_VAL(macro) = values[i];

	  scheme_set_global_bucket("define-syntaxes", b, macro, 1);
	  scheme_shadow(dm_env, (Scheme_Object *)b->key, 0);
	} else {
	  Scheme_Object **toplevels;
	  toplevels = (Scheme_Object **)MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(SCHEME_CAR(vars))];
	  b = (Scheme_Bucket *)toplevels[SCHEME_TOPLEVEL_POS(SCHEME_CAR(vars))];
	
	  scheme_set_global_bucket("define-values", b, values[i], 1);
	  scheme_shadow(((Scheme_Bucket_With_Home *)b)->home, (Scheme_Object *)b->key, 1);

	  if (SCHEME_TOPLEVEL_FLAGS(SCHEME_CAR(vars)) & SCHEME_TOPLEVEL_CONST) {
	    ((Scheme_Bucket_With_Flags *)b)->flags |= GLOB_IS_IMMUTATED;
	  }
	}
      }
      if (defmacro)
	scheme_pop_prefix(save_runstack);
	
      return scheme_void;
    }
  } else if (SCHEME_PAIRP(vars) && SCHEME_NULLP(SCHEME_CDR(vars))) {
    if (dm_env) {
      b = scheme_global_keyword_bucket(SCHEME_CAR(vars), dm_env);

      macro = scheme_alloc_small_object();
      macro->type = scheme_macro_type;
      SCHEME_PTR_VAL(macro) = vals;
      
      scheme_set_global_bucket("define-syntaxes", b, macro, 1);
      scheme_shadow(dm_env, (Scheme_Object *)b->key, 0);
    } else {
      Scheme_Object **toplevels;
      toplevels = (Scheme_Object **)MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(SCHEME_CAR(vars))];
      b = (Scheme_Bucket *)toplevels[SCHEME_TOPLEVEL_POS(SCHEME_CAR(vars))];

      scheme_set_global_bucket("define-values", b, vals, 1);
      scheme_shadow(((Scheme_Bucket_With_Home *)b)->home, (Scheme_Object *)b->key, 1);
      
      if (SCHEME_TOPLEVEL_FLAGS(SCHEME_CAR(vars)) & SCHEME_TOPLEVEL_CONST) {
	((Scheme_Bucket_With_Flags *)b)->flags |= GLOB_IS_IMMUTATED;
      }
      
      if (defmacro)
	scheme_pop_prefix(save_runstack);
    }

    return scheme_void;
  } else
    g = 1;

  /* Special handling of 0 values for define-syntaxes:
     do nothing. This makes (define-values (a b c) (values))
     a kind of declaration form, which is useful is
     a, b, or c is introduced by a macro. */
  if (dm_env && !g)
    return scheme_void;
  
  l = vars;
  for (i = 0; SCHEME_PAIRP(l); i++, l = SCHEME_CDR(l)) {}

  show_any = i;

  if (show_any) {
    if (dm_env) {
      b = scheme_global_keyword_bucket(SCHEME_CAR(vars), dm_env);
      name = (Scheme_Object *)b->key;
    } else {
      Scheme_Object **toplevels;
      toplevels = (Scheme_Object **)MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(SCHEME_CAR(vars))];
      b = (Scheme_Bucket *)toplevels[SCHEME_TOPLEVEL_POS(SCHEME_CAR(vars))];
      name = (Scheme_Object *)b->key;
    }
  } else
    name = NULL;
  
  if (defmacro > 1)
    scheme_pop_prefix(save_runstack);

  {
    const char *symname;

    symname = (show_any ? scheme_symbol_name(name) : "");

    scheme_wrong_return_arity((defmacro 
			       ? (dm_env ? "define-syntaxes" : "define-values-for-syntax")
			       : "define-values"),
			      i, g,
			      (g == 1) ? (Scheme_Object **)vals : scheme_current_thread->ku.multiple.array,
			      "%s%s%s",
			      show_any ? "defining \"" : "0 names",
			      symname,
			      show_any ? ((i == 1) ? "\"" : "\", ...") : "");
  }

  return NULL;
}

static Scheme_Object *
define_values_execute(Scheme_Object *data)
{
  return define_execute(SCHEME_CAR(data), SCHEME_CDR(data), 0, NULL, NULL);
}

static Scheme_Object *define_values_jit(Scheme_Object *data)
{
  Scheme_Object *orig = SCHEME_CDR(data), *naya, *vars;

  vars = SCHEME_CAR(data);

  if (SAME_TYPE(SCHEME_TYPE(orig), scheme_unclosed_procedure_type)
      && SCHEME_PAIRP(vars)
      && SCHEME_NULLP(SCHEME_CDR(vars)))
    naya = scheme_jit_closure(orig, SCHEME_CAR(vars));
  else
    naya = scheme_jit_expr(orig);

  if (SAME_OBJ(naya, orig))
    return data;
  else
    return scheme_make_pair(vars, naya);
}

static void define_values_validate(Scheme_Object *data, Mz_CPort *port, 
				   char *stack, int depth, int letlimit, int delta, int num_toplevels, int num_stxes)
{
  Scheme_Object *vars, *val;

  if (!SCHEME_PAIRP(data))
    scheme_ill_formed_code(port);

  vars = SCHEME_CAR(data);
  val = SCHEME_CDR(data);  
    
  for (; SCHEME_PAIRP(vars); vars = SCHEME_CDR(vars)) {
    scheme_validate_toplevel(SCHEME_CAR(vars), port, stack, depth, delta, num_toplevels, num_stxes);
  }
  
  if (!SCHEME_NULLP(vars))
    scheme_ill_formed_code(port);

  scheme_validate_expr(port, val, stack, depth, letlimit, delta, num_toplevels, num_stxes);
}

static Scheme_Object *
define_values_optimize(Scheme_Object *data, Optimize_Info *info)
{
  Scheme_Object *vars = SCHEME_CAR(data);
  Scheme_Object *val = SCHEME_CDR(data);

  scheme_optimize_info_used_top(info);
  val = scheme_optimize_expr(val, info);

  return scheme_make_syntax_compiled(DEFINE_VALUES_EXPD, cons(vars, val));
}

static Scheme_Object *
define_values_resolve(Scheme_Object *data, Resolve_Info *rslv)
{
  Scheme_Object *vars = SCHEME_CAR(data), *l, *a;
  Scheme_Object *val = SCHEME_CDR(data);

  /* If this is a module-level definition: for each variable, if the
     defined variable doesn't have SCHEME_TOPLEVEL_MUTATED, then
     resolve to a top-level reference with SCHEME_TOPLEVEL_CONST, so
     that we know to set GLOS_IS_IMMUTATED at run time. */
  for (l = vars; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    a = SCHEME_CAR(l);
    if (rslv->in_module
	&& rslv->enforce_const
	&& (!(SCHEME_TOPLEVEL_FLAGS(a) & SCHEME_TOPLEVEL_MUTATED))) {
      a = scheme_toplevel_to_flagged_toplevel(a, SCHEME_TOPLEVEL_CONST);
    }
    a = scheme_resolve_toplevel(rslv, a);
    SCHEME_CAR(l) = a;
  }

  val = scheme_resolve_expr(val, rslv);

  return scheme_make_syntax_resolved(DEFINE_VALUES_EXPD, cons(vars, val));
}

void scheme_define_parse(Scheme_Object *form, 
			 Scheme_Object **var, Scheme_Object **_stk_val,
			 int defmacro,
			 Scheme_Comp_Env *env)
{
  Scheme_Object *vars, *rest;
  int len;
  DupCheckRecord r;

  if (!scheme_is_toplevel(env))
    scheme_wrong_syntax(NULL, NULL, form, "illegal use (not at top-level)");

  len = check_form(form, form);
  if (len != 3)
    bad_form(form, len);
  
  rest = SCHEME_STX_CDR(form);
  vars = SCHEME_STX_CAR(rest);
  rest = SCHEME_STX_CDR(rest);
  *_stk_val = SCHEME_STX_CAR(rest);

  *var = vars;

  scheme_begin_dup_symbol_check(&r, env);

  while (SCHEME_STX_PAIRP(vars)) {
    Scheme_Object *name;

    name = SCHEME_STX_CAR(vars);
    scheme_check_identifier(NULL, name, NULL, env, form);

    vars = SCHEME_STX_CDR(vars);

    scheme_dup_symbol_check(&r, NULL, name, "binding", form);
  }  

  if (!SCHEME_STX_NULLP(vars))
    scheme_wrong_syntax(NULL, *var, form, "bad variable list");
}

static Scheme_Object *
defn_targets_syntax (Scheme_Object *var, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *first = scheme_null, *last = NULL;

  while (SCHEME_STX_PAIRP(var)) {
    Scheme_Object *name, *pr, *bucket;

    name = SCHEME_STX_CAR(var);
    name = scheme_tl_id_sym(env->genv, name, NULL, 2);

    if (rec[drec].resolve_module_ids || !env->genv->module) {
      bucket = (Scheme_Object *)scheme_global_bucket(name, env->genv);
    } else {
      /* Create a module variable reference, so that idx is preserved: */
      bucket = scheme_hash_module_variable(env->genv, env->genv->module->self_modidx, 
					   name, env->genv->module->insp, 
					   -1, env->genv->mod_phase);
    }
    /* Get indirection through the prefix: */
    bucket = scheme_register_toplevel_in_prefix(bucket, env, rec, drec);

    pr = cons(bucket, scheme_null);
    if (last)
      SCHEME_CDR(last) = pr;
    else
      first = pr;
    last = pr;

    var = SCHEME_STX_CDR(var);
  }

  return first;
}

static Scheme_Object *
define_values_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *var, *val, *targets, *variables;
  
  scheme_define_parse(form, &var, &val, 0, env);
  variables = var;
  
  targets = defn_targets_syntax(var, env, rec, drec);

  scheme_compile_rec_done_local(rec, drec);
  if (SCHEME_STX_PAIRP(targets) && SCHEME_STX_NULLP(SCHEME_STX_CDR(targets))) {
    var = SCHEME_STX_CAR(variables);
    rec[drec].value_name = SCHEME_STX_SYM(var);
  }

  env = scheme_no_defines(env);

  scheme_rec_add_certs(rec, drec, form);

  val = scheme_compile_expr(val, env, rec, drec);

  /* Note: module_optimize depends on the representation of
     DEFINE_VALUES_EXPD's value. */
  return scheme_make_syntax_compiled(DEFINE_VALUES_EXPD, cons(targets, val));
}

static Scheme_Object *
define_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  Scheme_Object *var, *val, *fn, *boundname;

  scheme_define_parse(form, &var, &val, 0, env);

  env = scheme_no_defines(env);

  if (SCHEME_STX_PAIRP(var) && SCHEME_STX_NULLP(SCHEME_STX_CDR(var)))
    boundname = SCHEME_STX_CAR(var);
  else
    boundname = scheme_false;
  erec[drec].value_name = boundname;

  scheme_rec_add_certs(erec, drec, form);

  fn = SCHEME_STX_CAR(form);
  return scheme_datum_to_syntax(icons(fn,
				      icons(var,
					    icons(scheme_expand_expr(val, env, erec, drec), 
						  scheme_null))),
				form,
				form,
				0, 2);
}

/**********************************************************************/
/*                               quote                                */
/**********************************************************************/

static Scheme_Object *
quote_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *v, *rest;

  rest = SCHEME_STX_CDR(form);
  if (!(SCHEME_STX_PAIRP(rest) && SCHEME_STX_NULLP(SCHEME_STX_CDR(rest))))
    scheme_wrong_syntax(NULL, NULL, form, "bad syntax (wrong number of parts)");

  scheme_compile_rec_done_local(rec, drec);
  scheme_default_compile_rec(rec, drec);
  
  v = SCHEME_STX_CAR(rest);

  if (SCHEME_STXP(v))
    return scheme_syntax_to_datum(v, 0, NULL);
  else
    return v;
}

static Scheme_Object *
quote_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  Scheme_Object *rest;

  rest = SCHEME_STX_CDR(form);

  if (!(SCHEME_STX_PAIRP(rest) && SCHEME_STX_NULLP(SCHEME_STX_CDR(rest))))
    scheme_wrong_syntax(NULL, NULL, form, "bad syntax (wrong number of parts)");

  return form;
}

/**********************************************************************/
/*                                if                                  */
/**********************************************************************/

static Scheme_Object *
if_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  int len, opt;
  Scheme_Object *test, *thenp, *elsep, *name, *rest;
  Scheme_Compile_Info recs[3];

  len = check_form(form, form);
  if (!(((len == 3) || (len == 4))))
    bad_form(form, len);

  name = rec[drec].value_name;
  scheme_compile_rec_done_local(rec, drec);

  name = scheme_check_name_property(form, name);

  rest = SCHEME_STX_CDR(form);
  test = SCHEME_STX_CAR(rest);
  rest = SCHEME_STX_CDR(rest);
  thenp = SCHEME_STX_CAR(rest);
  if (len == 4) {
    rest = SCHEME_STX_CDR(rest);
    elsep = SCHEME_STX_CAR(rest);
  } else
    elsep = scheme_compiled_void();

  scheme_rec_add_certs(rec, drec, form);

  scheme_init_compile_recs(rec, drec, recs, 3);
  recs[1].value_name = name;
  recs[2].value_name = name;

  env = scheme_no_defines(env);

  test = scheme_compile_expr(test, env, recs, 0);

  if (SCHEME_TYPE(test) > _scheme_compiled_values_types_) {
    opt = 1;
    
    if (SCHEME_FALSEP(test)) {
      /* compile other branch only to get syntax checking: */
      recs[2].dont_mark_local_use = 1;
      scheme_compile_expr(thenp, env, recs, 2);

      if (len == 4)
	test = scheme_compile_expr(elsep, env, recs, 1);
      else
	test = elsep;
    } else {
      if (len == 4) {
	/* compile other branch only to get syntax checking: */
	recs[2].dont_mark_local_use = 1;
	scheme_compile_expr(elsep, env, recs, 2);
      }
      
      test = scheme_compile_expr(thenp, env, recs, 1);
    }
  } else {
    opt = 0;
    thenp = scheme_compile_expr(thenp, env, recs, 1);
    if (len == 4)
      elsep = scheme_compile_expr(elsep, env, recs, 2);
  }

  scheme_merge_compile_recs(rec, drec, recs, (opt || (len == 3)) ? 2 : 3);
  
  if (opt)
    return test;
  else
    return scheme_make_branch(test, thenp, elsep);
}

static Scheme_Object *
if_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  Scheme_Object *test, *rest, *thenp, *elsep, *fn, *boundname;
  int len;
  Scheme_Expand_Info recs[3];

  len = check_form(form, form);

  if (!(((len == 3) || (len == 4))))
    bad_form(form, len);

  env = scheme_no_defines(env);

  boundname = scheme_check_name_property(form, erec[drec].value_name);

  scheme_rec_add_certs(erec, drec, form);  

  scheme_init_expand_recs(erec, drec, recs, 3);
  recs[0].value_name = scheme_false;
  recs[1].value_name = boundname;
  recs[2].value_name = boundname;

  rest = SCHEME_STX_CDR(form);
  test = SCHEME_STX_CAR(rest);
  test = scheme_expand_expr(test, env, recs, 0);

  rest = SCHEME_STX_CDR(rest);
  thenp = SCHEME_STX_CAR(rest);
  thenp = scheme_expand_expr(thenp, env, recs, 1);

  rest = SCHEME_STX_CDR(rest);
  if (!SCHEME_STX_NULLP(rest)) {
    elsep = SCHEME_STX_CAR(rest);
    elsep = scheme_expand_expr(elsep, env, recs, 2);
    rest = icons(elsep, scheme_null);
  } else {
    rest = scheme_null;
  }

  rest = icons(thenp, rest);

  fn = SCHEME_STX_CAR(form);
  return scheme_datum_to_syntax(icons(fn, icons(test, rest)),
				form, form, 
				0, 2);
}

/**********************************************************************/
/*                    with-continuation-mark                          */
/**********************************************************************/

static Scheme_Object *
with_cont_mark_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *key, *val, *expr, *name, *orig_form = form;
  Scheme_Compile_Info recs[3];
  Scheme_With_Continuation_Mark *wcm;
  int len;
  len = check_form(form, form);

  if (len != 4)
    bad_form(form, len);

  env = scheme_no_defines(env);

  form = SCHEME_STX_CDR(form);
  key = SCHEME_STX_CAR(form);
  form = SCHEME_STX_CDR(form);
  val = SCHEME_STX_CAR(form);
  form = SCHEME_STX_CDR(form);
  expr = SCHEME_STX_CAR(form);

  name = rec[drec].value_name;
  scheme_compile_rec_done_local(rec, drec);

  name = scheme_check_name_property(orig_form, name);

  scheme_rec_add_certs(rec, drec, orig_form);

  scheme_init_compile_recs(rec, drec, recs, 3);
  recs[2].value_name = name;

  key = scheme_compile_expr(key, env, recs, 0);
  val = scheme_compile_expr(val, env, recs, 1);
  expr = scheme_compile_expr(expr, env, recs, 2);

  scheme_merge_compile_recs(rec, drec, recs, 3);

  wcm = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
  wcm->so.type = scheme_with_cont_mark_type;
  wcm->key = key;
  wcm->val = val;
  wcm->body = expr;
  
  return (Scheme_Object *)wcm;
}

static Scheme_Object *
with_cont_mark_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  Scheme_Object *key, *val, *expr, *orig_form = form, *fn, *boundname;
  int len;
  Scheme_Expand_Info recs[3];

  len = check_form(form, form);
  if (len != 4)
    bad_form(form, len);

  env = scheme_no_defines(env);

  boundname = scheme_check_name_property(form, erec[drec].value_name);

  scheme_rec_add_certs(erec, drec, form);

  scheme_init_expand_recs(erec, drec, recs, 3);
  recs[0].value_name = scheme_false;
  recs[1].value_name = scheme_false;
  recs[2].value_name = boundname;

  form = SCHEME_STX_CDR(form);
  key = SCHEME_STX_CAR(form);
  form = SCHEME_STX_CDR(form);
  val = SCHEME_STX_CAR(form);
  form = SCHEME_STX_CDR(form);
  expr = SCHEME_STX_CAR(form);

  key = scheme_expand_expr(key, env, recs, 0);
  val = scheme_expand_expr(val, env, recs, 1);
  expr = scheme_expand_expr(expr, env, recs, 2);

  fn = SCHEME_STX_CAR(orig_form);
  return scheme_datum_to_syntax(icons(fn,
				      icons(key,
					    icons(val,
						  icons(expr, scheme_null)))),
				orig_form,
				orig_form, 
				0, 2);
}

/**********************************************************************/
/*                               set!                                 */
/**********************************************************************/

static Scheme_Object *
set_execute (Scheme_Object *data)
{
  Scheme_Object *val, *set_undef, *tl, **toplevels;
  Scheme_Bucket *var;

  set_undef = SCHEME_CAR(data);
  data = SCHEME_CDR(data);
  
  val = SCHEME_CDR(data);
  val = _scheme_eval_linked_expr(val);

  tl = SCHEME_CAR(data);
  toplevels = (Scheme_Object **)MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(tl)];
  var = (Scheme_Bucket *)toplevels[SCHEME_TOPLEVEL_POS(tl)];
  
  scheme_set_global_bucket("set!", var, val, SCHEME_TRUEP(set_undef));

  return scheme_void;
}

static Scheme_Object *set_jit(Scheme_Object *data)
{
  Scheme_Object *orig_val, *naya_val;

  orig_val = SCHEME_CDR(data);
  orig_val = SCHEME_CDR(orig_val);

  naya_val = scheme_jit_expr(orig_val);
  
  if (SAME_OBJ(naya_val, orig_val))
    return data;
  else
    return scheme_make_pair(SCHEME_CAR(data),
			    scheme_make_pair(SCHEME_CADR(data),
					     naya_val));
}

static void set_validate(Scheme_Object *data, Mz_CPort *port, 
			 char *stack, int depth, int letlimit, int delta, int num_toplevels, int num_stxes)
{
  Scheme_Object *val, *tl;

  if (!SCHEME_PAIRP(data)
      || !SCHEME_PAIRP(SCHEME_CDR(data)))
    scheme_ill_formed_code(port);
  
  data = SCHEME_CDR(data);
  tl = SCHEME_CAR(data);
  val = SCHEME_CDR(data);

  scheme_validate_expr(port, val, stack, depth, letlimit, delta, num_toplevels, num_stxes);
  scheme_validate_toplevel(tl,  port, stack, depth, delta, num_toplevels, num_stxes);
}

static Scheme_Object *
set_optimize(Scheme_Object *data, Optimize_Info *info)
{
  Scheme_Object *var, *val, *set_undef;

  set_undef = SCHEME_CAR(data);
  data = SCHEME_CDR(data);
  var = SCHEME_CAR(data);
  val = SCHEME_CDR(data);
  
  val = scheme_optimize_expr(val, info);

  if (SAME_TYPE(SCHEME_TYPE(var), scheme_local_type)) {
    int pos, delta;

    pos = SCHEME_LOCAL_POS(var);

    /* Register that we use this variable: */
    scheme_optimize_info_lookup(info, pos, NULL);

    /* Offset: */
    delta = scheme_optimize_info_get_shift(info, pos);
    if (delta)
      var = scheme_make_local(scheme_local_type, pos + delta);
  } else {
    scheme_optimize_info_used_top(info);
  }
  
  return scheme_make_syntax_compiled(SET_EXPD, cons(set_undef, cons(var, val)));
}

static Scheme_Object *
set_clone(Scheme_Object *data, Optimize_Info *info, int delta, int closure_depth)
{
  Scheme_Object *var, *val, *set_undef;

  set_undef = SCHEME_CAR(data);
  data = SCHEME_CDR(data);
  var = SCHEME_CAR(data);
  val = SCHEME_CDR(data);
  
  val = scheme_optimize_clone(val, info, delta, closure_depth);
  if (!val) return NULL;
  if (SAME_TYPE(SCHEME_TYPE(var), scheme_local_type)) {
    var = scheme_optimize_clone(var, info, delta, closure_depth);
    if (!var) return NULL;
  }
  
  return scheme_make_syntax_compiled(SET_EXPD, cons(set_undef, cons(var, val)));  
}

static Scheme_Object *
set_resolve(Scheme_Object *data, Resolve_Info *rslv)
{
  Scheme_Object *var, *val, *set_undef;

  set_undef = SCHEME_CAR(data);
  data = SCHEME_CDR(data);
  var = SCHEME_CAR(data);
  val = SCHEME_CDR(data);
  
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
    li = scheme_resolve_info_lookup(rslv, SCHEME_LOCAL_POS(var), &flags);
    lv->position = li;
    SCHEME_LET_AUTOBOX(lv) = (flags & SCHEME_INFO_BOXED);
    lv->value = val;

    if (!(flags & SCHEME_INFO_BOXED))
      scheme_signal_error("internal error: set!: set!ed local variable is not boxed");

    return (Scheme_Object *)lv;
  }

  var = scheme_resolve_expr(var, rslv);
  
  return scheme_make_syntax_resolved(SET_EXPD, cons(set_undef, cons(var, val)));
}

static Scheme_Object *
set_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Env *menv = NULL;
  Scheme_Object *var, *val, *name, *body, *rest, *find_name;
  int l, set_undef;

  l = check_form(form, form);
  if (l != 3)
    bad_form(form, l);

  rest = SCHEME_STX_CDR(form);
  name = SCHEME_STX_CAR(rest);
  rest = SCHEME_STX_CDR(rest);
  body = SCHEME_STX_CAR(rest);
  
  scheme_check_identifier("set!", name, NULL, env, form);

  find_name = name;

  scheme_rec_add_certs(rec, drec, form);

  while (1) {
    var = scheme_lookup_binding(find_name, env, 
				SCHEME_SETTING 
				+ SCHEME_GLOB_ALWAYS_REFERENCE
				+ (rec[drec].dont_mark_local_use 
				   ? SCHEME_DONT_MARK_USE 
				   : 0)
				+ (rec[drec].resolve_module_ids
				   ? SCHEME_RESOLVE_MODIDS
				   : 0),
				rec[drec].certs, env->in_modidx, 
				&menv, NULL);
    
    if (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)) {
      /* Redirect to a macro? */
      if (SAME_TYPE(SCHEME_TYPE(SCHEME_PTR_VAL(var)), scheme_set_macro_type)) {
	form = scheme_apply_macro(name, menv, SCHEME_PTR_VAL(var), form, env, scheme_false, rec[drec].certs, 1);
	
	return scheme_compile_expr(form, env, rec, drec);
      } else if (SAME_TYPE(SCHEME_TYPE(SCHEME_PTR_VAL(var)), scheme_id_macro_type)) {
	find_name = SCHEME_PTR_VAL(SCHEME_PTR_VAL(var));
	find_name = scheme_stx_cert(find_name, scheme_false, menv, find_name, NULL, 1);
	SCHEME_USE_FUEL(1);
	menv = NULL;
      } else
	break;
    } else
      break;
  }

  if (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)
      || SAME_TYPE(SCHEME_TYPE(var), scheme_syntax_compiler_type)) {
    scheme_wrong_syntax(NULL, name, form, "cannot mutate syntax identifier");
    return NULL;
  }

  if (SAME_TYPE(SCHEME_TYPE(var), scheme_variable_type)
      || SAME_TYPE(SCHEME_TYPE(var), scheme_module_variable_type)) {
    var = scheme_register_toplevel_in_prefix(var, env, rec, drec);
    if (env->genv->module)
      SCHEME_TOPLEVEL_FLAGS(var) |= SCHEME_TOPLEVEL_MUTATED;
  }

  scheme_compile_rec_done_local(rec, drec);
  rec[drec].value_name = SCHEME_STX_SYM(name);

  val = scheme_compile_expr(body, scheme_no_defines(env), rec, drec);

  /* check for (set! x x) */
  if (SAME_TYPE(SCHEME_TYPE(var), SCHEME_TYPE(val))) {
    if (SAME_TYPE(SCHEME_TYPE(var), scheme_local_type)
	|| SAME_TYPE(SCHEME_TYPE(var), scheme_local_unbox_type)) {
      /* local */
      if (SCHEME_LOCAL_POS(var) == SCHEME_LOCAL_POS(val))
	return scheme_compiled_void();
    } else {
      /* global; can't do anything b/c var might be undefined or constant */
    }
  }
  
  set_undef = SCHEME_TRUEP(scheme_get_param(scheme_current_config(),
					    MZCONFIG_ALLOW_SET_UNDEFINED));
  
  return scheme_make_syntax_compiled(SET_EXPD, 
				     cons(set_undef
					  ? scheme_true
					  : scheme_false,
					  cons(var, val)));
}

static Scheme_Object *
set_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  Scheme_Env *menv = NULL;
  Scheme_Object *name, *var, *fn, *rhs, *find_name;
  int l;
  l = check_form(form, form);
  if (l != 3)
    bad_form(form, l);

  env = scheme_no_defines(env);

  name = SCHEME_STX_CDR(form);
  name = SCHEME_STX_CAR(name);

  scheme_check_identifier("set!", name, NULL, env, form);

  find_name = name;

  scheme_rec_add_certs(erec, drec, form);

  while (1) {
    /* Make sure it's mutable, and check for redirects: */
    var = scheme_lookup_binding(find_name, env, SCHEME_SETTING, 
				erec[drec].certs, env->in_modidx, 
				&menv, NULL);
    
    if ((erec[drec].depth != 0) && SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)) {
      /* Redirect to a macro? */
      if (SAME_TYPE(SCHEME_TYPE(SCHEME_PTR_VAL(var)), scheme_set_macro_type)) {
	form = scheme_apply_macro(name, menv, SCHEME_PTR_VAL(var), form, env, scheme_false, erec[drec].certs, 1);
      
	if (erec[drec].depth > 0)
	  erec[drec].depth--;

	erec[drec].value_name = name;

	return scheme_expand_expr(form, env, erec, drec);
      } else if (SAME_TYPE(SCHEME_TYPE(SCHEME_PTR_VAL(var)), scheme_id_macro_type)) {
	Scheme_Object *new_name;
	new_name = SCHEME_PTR_VAL(SCHEME_PTR_VAL(var));
	new_name = scheme_stx_track(new_name, find_name, find_name);
	new_name = scheme_stx_cert(new_name, scheme_false, menv, find_name, NULL, 1);
	find_name = new_name;
	menv = NULL;
      } else
	break;
    } else
      break;
  }

  if (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)
      || SAME_TYPE(SCHEME_TYPE(var), scheme_syntax_compiler_type)) {
    scheme_wrong_syntax(NULL, name, form, "cannot mutate syntax identifier");
  }


  fn = SCHEME_STX_CAR(form);
  rhs = SCHEME_STX_CDR(form);
  rhs = SCHEME_STX_CDR(rhs);
  rhs = SCHEME_STX_CAR(rhs);

  erec[drec].value_name = name;

  return scheme_datum_to_syntax(icons(fn,
				      icons(find_name,
					    icons(scheme_expand_expr(rhs, env, erec, drec),
						  scheme_null))),
				form,
				form, 
				0, 2);
}

/**********************************************************************/
/*                              #%variable-reference                                 */
/**********************************************************************/

static Scheme_Object *
ref_execute (Scheme_Object *tl)
{
  Scheme_Object **toplevels, *o;
  Scheme_Bucket *var;

  toplevels = (Scheme_Object **)MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(tl)];
  var = (Scheme_Bucket *)toplevels[SCHEME_TOPLEVEL_POS(tl)];
  
  o = scheme_alloc_small_object();
  o->type = scheme_global_ref_type;
  SCHEME_PTR_VAL(o) = (Scheme_Object *)var;

  return o;
}

static Scheme_Object *ref_jit(Scheme_Object *data)
{
  return data;
}

static void ref_validate(Scheme_Object *tl, Mz_CPort *port, 
			 char *stack, int depth, int letlimit, int delta, int num_toplevels, int num_stxes)
{
  scheme_validate_toplevel(tl,  port, stack, depth, delta, num_toplevels, num_stxes);
}

static Scheme_Object *
ref_optimize(Scheme_Object *tl, Optimize_Info *rslv)
{
  return scheme_make_syntax_compiled(REF_EXPD, tl);
}

static Scheme_Object *
ref_resolve(Scheme_Object *tl, Resolve_Info *rslv)
{
  return scheme_make_syntax_resolved(REF_EXPD, scheme_resolve_expr(tl, rslv));
}

static Scheme_Object *
ref_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Env *menv = NULL;
  Scheme_Object *var, *name, *rest;
  int l, ok;

  l = check_form(form, form);
  if (l != 2)
    bad_form(form, l);

  rest = SCHEME_STX_CDR(form);
  name = SCHEME_STX_CAR(rest);

  if (SCHEME_STX_PAIRP(name)) {
    rest = SCHEME_STX_CAR(name);
    if (env->genv->phase == 0) {
      var = scheme_top_stx;
    } else {
      var = scheme_datum_to_syntax(SCHEME_STX_VAL(scheme_top_stx), scheme_false, scheme_sys_wraps(env), 0, 0);
    }
    ok = scheme_stx_module_eq(rest, var, env->genv->phase);
  } else 
    ok = SCHEME_STX_SYMBOLP(name);

  if (!ok) {
    scheme_wrong_syntax("#%variable-reference", name, 
			form, 
			"not an identifier or #%%top form");
    return NULL;
  }

  if (SCHEME_STX_PAIRP(name)) {
    if (rec[drec].comp)
      var = scheme_compile_expr(name, env, rec, drec);
    else
      var = scheme_expand_expr(name, env, rec, drec);
  } else {
    scheme_rec_add_certs(rec, drec, form);

    var = scheme_lookup_binding(name, env, 
				SCHEME_REFERENCING 
				+ SCHEME_GLOB_ALWAYS_REFERENCE
				+ (rec[drec].dont_mark_local_use 
				   ? SCHEME_DONT_MARK_USE 
				   : 0)
				+ (rec[drec].resolve_module_ids
				   ? SCHEME_RESOLVE_MODIDS
				   : 0),
				rec[drec].certs, env->in_modidx, 
				&menv, NULL);

    if (SAME_TYPE(SCHEME_TYPE(var), scheme_variable_type)
	|| SAME_TYPE(SCHEME_TYPE(var), scheme_module_variable_type)) {
      if (rec[drec].comp)
	var = scheme_register_toplevel_in_prefix(var, env, rec, drec);
    } else {
      scheme_wrong_syntax(NULL, name, form, "identifier does not refer to a top-level or module variable");
    }

    if (rec[drec].comp)
      scheme_compile_rec_done_local(rec, drec);
  }

  if (rec[drec].comp)
    return scheme_make_syntax_compiled(REF_EXPD, var);
  else
    return scheme_void;
}

static Scheme_Object *
ref_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  /* Error checking: */
  ref_syntax(form, env, erec, drec);

  /* No change: */
  return form;
}

/**********************************************************************/
/*                             case-lambda                            */
/**********************************************************************/

static Scheme_Object *
case_lambda_execute(Scheme_Object *expr)
{
  Scheme_Case_Lambda *seqin, *seqout;
  int i, cnt;
  Scheme_Thread *p = scheme_current_thread;

  seqin = (Scheme_Case_Lambda *)expr;

#ifdef MZ_USE_JIT
  if (seqin->native_code) {
    Scheme_Native_Closure_Data *ndata;
    Scheme_Native_Closure *nc, *na;
    Scheme_Closure_Data *data;
    Scheme_Object *val;
    GC_CAN_IGNORE Scheme_Object **runstack;
    GC_CAN_IGNORE mzshort *map;
    int j, jcnt;

    ndata = seqin->native_code;
    nc = (Scheme_Native_Closure *)scheme_make_native_case_closure(ndata);

    cnt = seqin->count;
    for (i = 0; i < cnt; i++) {
      val = seqin->array[i];
      if (!SCHEME_PROCP(val)) {
	data = (Scheme_Closure_Data *)val;
	na = (Scheme_Native_Closure *)scheme_make_native_closure(data->native_code);
	runstack = MZ_RUNSTACK;
	jcnt = data->closure_size;
	map = data->closure_map;
	for (j = 0; j < jcnt; j++) {
	  na->vals[j] = runstack[map[j]];
	}
	val = (Scheme_Object *)na;
      }
      nc->vals[i] = val;
    }

    return (Scheme_Object *)nc;
  }
#endif

  seqout = (Scheme_Case_Lambda *)
    scheme_malloc_tagged(sizeof(Scheme_Case_Lambda)
			 + (seqin->count - 1) * sizeof(Scheme_Object *));
  seqout->so.type = scheme_case_closure_type;
  seqout->count = seqin->count;
  seqout->name = seqin->name;

  cnt = seqin->count;
  for (i = 0; i < cnt; i++) {
    if (SAME_TYPE(SCHEME_TYPE(seqin->array[i]), scheme_closure_type)) {
      /* An empty closure, created at compile time */
      seqout->array[i] = seqin->array[i];
    } else {
      Scheme_Object *lc;
      lc = scheme_make_closure(p, seqin->array[i], 1);
      seqout->array[i] = lc;
    }
  }

  return (Scheme_Object *)seqout;
}

static Scheme_Object *case_lambda_jit(Scheme_Object *expr)
{
#ifdef MZ_USE_JIT
  Scheme_Case_Lambda *seqin = (Scheme_Case_Lambda *)expr;

  if (!seqin->native_code) {
    Scheme_Case_Lambda *seqout;
    Scheme_Native_Closure_Data *ndata;
    Scheme_Object *val, *name;
    int i, cnt, size, all_closed = 1;

    cnt = seqin->count;
    
    size = sizeof(Scheme_Case_Lambda) + ((cnt - 1) * sizeof(Scheme_Object *));

    seqout = (Scheme_Case_Lambda *)scheme_malloc_tagged(size);
    memcpy(seqout, seqin, size);

    name = seqin->name;
    if (name && SCHEME_BOXP(name))
      name = SCHEME_BOX_VAL(name);

    for (i = 0; i < cnt; i++) {
      val = seqout->array[i];
      if (SCHEME_PROCP(val)) {
	/* Undo creation of empty closure */
	val = (Scheme_Object *)((Scheme_Closure *)val)->code;
	seqout->array[i] = val;
      }
      ((Scheme_Closure_Data *)val)->name = name;
      if (((Scheme_Closure_Data *)val)->closure_size)
	all_closed = 0;
    }

    /* Generating the code may cause empty closures to be formed: */
    ndata = scheme_generate_case_lambda(seqout);
    seqout->native_code = ndata;

    if (all_closed) {
      /* Native closures do not refer back to the original bytecode,
	 so no need to worry about clearing the reference. */
      Scheme_Native_Closure *nc;
      nc = (Scheme_Native_Closure *)scheme_make_native_case_closure(ndata);
      for (i = 0; i < cnt; i++) {
	val = seqout->array[i];
	if (!SCHEME_PROCP(val)) {
	  val = scheme_make_native_closure(((Scheme_Closure_Data *)val)->native_code);
	}
	nc->vals[i] = val;
      }
      return (Scheme_Object *)nc;
    } else {
      /* The case-lambda data must point to the original closure-data
	 record, because that's where the closure maps are kept. But
	 we don't need the bytecode, anymore. So clone the
	 closure-data record and drop the bytecode in thte clone. */
      for (i = 0; i < cnt; i++) {
	val = seqout->array[i];
	if (!SCHEME_PROCP(val)) {
	  Scheme_Closure_Data *data;
	  data = MALLOC_ONE_TAGGED(Scheme_Closure_Data);
	  memcpy(data, val, sizeof(Scheme_Closure_Data));
	  data->code = NULL;
	  seqout->array[i] = (Scheme_Object *)data;
	}
      }
    }

    return (Scheme_Object *)seqout;
  }
#endif
 
  return expr;
}

static void case_lambda_validate(Scheme_Object *data, Mz_CPort *port, char *stack, 
				 int depth, int letlimit, int delta, int num_toplevels, int num_stxes)
{
  Scheme_Case_Lambda *seq = (Scheme_Case_Lambda *)data;
  int i;

  for (i = 0; i < seq->count; i++) { 
    scheme_validate_expr(port, seq->array[i], stack, depth, letlimit, delta, num_toplevels, num_stxes);
  }
}

static Scheme_Object *
case_lambda_resolve(Scheme_Object *expr, Resolve_Info *rslv)
{
  int i, all_closed = 1;
  Scheme_Case_Lambda *seq = (Scheme_Case_Lambda *)expr;

  for (i = 0; i < seq->count; i++) {
    Scheme_Object *le;
    le = seq->array[i];
    le = scheme_resolve_expr(le, rslv);
    seq->array[i] = le;
    if (!SCHEME_PROCP(le))
      all_closed = 0;
  }

  if (all_closed) {
    /* Produce closure directly */
    return case_lambda_execute(expr);
  }

  return scheme_make_syntax_resolved(CASE_LAMBDA_EXPD, expr);
}

static Scheme_Object *
case_lambda_optimize(Scheme_Object *expr, Optimize_Info *info)
{
  Scheme_Object *le;
  int i;
  Scheme_Case_Lambda *seq = (Scheme_Case_Lambda *)expr;

  for (i = 0; i < seq->count; i++) {
    le = seq->array[i];
    le = scheme_optimize_expr(le, info);
    seq->array[i] = le;
  }

  return scheme_make_syntax_compiled(CASE_LAMBDA_EXPD, expr);
}

Scheme_Object *scheme_unclose_case_lambda(Scheme_Object *expr, int jit)
{
  Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)expr;
  Scheme_Closure *c;
  int i;

  for (i = cl->count; i--; ) {
    c = (Scheme_Closure *)cl->array[i];
    if (!ZERO_SIZED_CLOSUREP(c)) {
      break;
    }
  }

  if (i < 0) {
    /* We can reconstruct a case-lambda syntactic form. */
    Scheme_Case_Lambda *cl2;

    cl2 = (Scheme_Case_Lambda *)scheme_malloc_tagged(sizeof(Scheme_Case_Lambda)
						     + ((cl->count - 1) * sizeof(Scheme_Object*)));
    
    cl2->so.type = scheme_case_lambda_sequence_type;
    cl2->count = cl->count;
    cl2->name = cl->name;

    for (i = cl->count; i--; ) {
      c = (Scheme_Closure *)cl->array[i];
      cl2->array[i] = (Scheme_Object *)c->code;
    }

    if (jit)
      return case_lambda_jit((Scheme_Object *)cl2);
    else
      return (Scheme_Object *)cl2;
  }
  
  return expr;
}

static void case_lambda_check_line(Scheme_Object *line, Scheme_Object *form, Scheme_Comp_Env *env)
{
  Scheme_Object *body, *args;

  if (!SCHEME_STX_PAIRP(line))
    scheme_wrong_syntax(NULL, line, form, NULL);
  
  body = SCHEME_STX_CDR(line);
  args = SCHEME_STX_CAR(line);
  
  lambda_check_args(args, form, env);
  
  if (!SCHEME_STX_PAIRP(body))
    scheme_wrong_syntax(NULL, line, form, "bad syntax (%s)",
			SCHEME_STX_NULLP(body) ? "empty body" : IMPROPER_LIST_FORM);
}

static Scheme_Object *
case_lambda_syntax (Scheme_Object *form, Scheme_Comp_Env *env, 
		    Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *list, *last, *c, *orig_form = form, *name;
  Scheme_Case_Lambda *cl;
  int i, count = 0;
  Scheme_Compile_Info *recs;
  
  form = SCHEME_STX_CDR(form);

  name = scheme_build_closure_name(orig_form, rec, drec);
  
  if (SCHEME_STX_NULLP(form)) {
    /* Case where there are no cases... */
    form = (Scheme_Object *)scheme_malloc_tagged(sizeof(Scheme_Case_Lambda)
						 - sizeof(Scheme_Object*));

    form->type = scheme_case_lambda_sequence_type;
    ((Scheme_Case_Lambda *)form)->count = 0;
    ((Scheme_Case_Lambda *)form)->name = name;

    scheme_compile_rec_done_local(rec, drec);
    scheme_default_compile_rec(rec, drec);

    if (scheme_has_method_property(orig_form)) {
      /* See note in schpriv.h about the IS_METHOD hack */
      if (!name)
	name = scheme_false;
      name = scheme_box(name);
      ((Scheme_Case_Lambda *)form)->name = name;
    }

    return scheme_make_syntax_compiled(CASE_LAMBDA_EXPD, form);
  }

  if (!SCHEME_STX_PAIRP(form))
    scheme_wrong_syntax(NULL, form, orig_form, NULL);
  if (SCHEME_STX_NULLP(SCHEME_STX_CDR(form))) {
    c = SCHEME_STX_CAR(form);

    case_lambda_check_line(c, orig_form, env);

    c = icons(scheme_datum_to_syntax(lambda_symbol, scheme_false, scheme_sys_wraps(env), 0, 0),
	      c);
    c = scheme_datum_to_syntax(c, orig_form, orig_form, 0, 2);
    
    return lambda_syntax(c, env, rec, drec);
  }

  scheme_compile_rec_done_local(rec, drec);

  scheme_rec_add_certs(rec, drec, orig_form);

  list = last = NULL;
  while (SCHEME_STX_PAIRP(form)) {
    Scheme_Object *clause;
    clause = SCHEME_STX_CAR(form);
    case_lambda_check_line(clause, orig_form, env);

    c = icons(lambda_symbol, clause);

    c = scheme_datum_to_syntax(c, clause, scheme_sys_wraps(env), 0, 0);

    c = cons(c, scheme_null);

    if (list)
      SCHEME_CDR(last) = c;
    else
      list = c;

    last = c;
    form = SCHEME_STX_CDR(form);

    count++;
  }

  if (!SCHEME_STX_NULLP(form))
    scheme_wrong_syntax(NULL, form, orig_form, NULL);

  cl = (Scheme_Case_Lambda *)
    scheme_malloc_tagged(sizeof(Scheme_Case_Lambda)
			 + (count - 1) * sizeof(Scheme_Object *));
  cl->so.type = scheme_case_lambda_sequence_type;
  cl->count = count;
  cl->name = SCHEME_TRUEP(name) ? name : NULL;

  scheme_compile_rec_done_local(rec, drec);
  recs = MALLOC_N_RT(Scheme_Compile_Info, count);
  scheme_init_compile_recs(rec, drec, recs, count);

  for (i = 0; i < count; i++) {
    Scheme_Object *ce;
    ce = SCHEME_CAR(list);
    ce = scheme_compile_expr(ce, env, recs, i);
    cl->array[i] = ce;
    list = SCHEME_CDR(list);
  }

  scheme_merge_compile_recs(rec, drec, recs, count);

  if (scheme_has_method_property(orig_form)) {
    Scheme_Closure_Data *data;
    /* Make sure no branch has 0 arguments: */
    for (i = 0; i < count; i++) {
      data = (Scheme_Closure_Data *)cl->array[i];
      if (!data->num_params)
	break;
    }
    if (i >= count) {
      data = (Scheme_Closure_Data *)cl->array[0];
      SCHEME_CLOSURE_DATA_FLAGS(data) |= CLOS_IS_METHOD;
    }
  }

  return scheme_make_syntax_compiled(CASE_LAMBDA_EXPD, (Scheme_Object *)cl);
}

static Scheme_Object *
case_lambda_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  Scheme_Object *first, *last, *args, *body, *c, *new_line, *orig_form = form;

  first = SCHEME_STX_CAR(form);
  first = icons(first, scheme_null);
  last = first;
  form = SCHEME_STX_CDR(form);

  scheme_rec_add_certs(erec, drec, orig_form);

  while (SCHEME_STX_PAIRP(form)) {
    Scheme_Object *line_form;
    Scheme_Comp_Env *newenv;

    line_form = SCHEME_STX_CAR(form);

    case_lambda_check_line(line_form, orig_form, env);
    
    body = SCHEME_STX_CDR(line_form);
    args = SCHEME_STX_CAR(line_form);

    body = scheme_datum_to_syntax(body, line_form, line_form, 0, 0);
    
    newenv = scheme_add_compilation_frame(args, env, 0, erec[drec].certs);
    
    body = scheme_add_env_renames(body, newenv, env);
    args = scheme_add_env_renames(args, newenv, env);

    {
      Scheme_Expand_Info erec1;
      scheme_init_expand_recs(erec, drec, &erec1, 1);
      erec1.value_name = scheme_false;
      new_line = icons(args, scheme_expand_block(body, newenv, &erec1, 0));
    }
    new_line = scheme_datum_to_syntax(new_line, line_form, line_form, 0, 1);

    c = icons(new_line, scheme_null);

    SCHEME_CDR(last) = c;
    last = c;

    form = SCHEME_STX_CDR(form);
  }

  if (!SCHEME_STX_NULLP(form))
    scheme_wrong_syntax(NULL, form, orig_form, NULL);
  
  return scheme_datum_to_syntax(first, orig_form, orig_form, 0, 2);
}

/**********************************************************************/
/*                          implicit set!s                            */
/**********************************************************************/

/* A bangboxenv step is inserted by the compilation of `lambda' forms
   where an argument is set!ed in the function body. */

Scheme_Object *bangboxenv_execute(Scheme_Object *data)
{
  int pos = SCHEME_INT_VAL(SCHEME_CAR(data));
  Scheme_Object *bb;

  data = SCHEME_CDR(data);
  
  bb = scheme_make_envunbox(MZ_RUNSTACK[pos]);
  MZ_RUNSTACK[pos] = bb;

  return _scheme_tail_eval(data);
}

static void bangboxenv_validate(Scheme_Object *data, Mz_CPort *port, 
				char *stack, int depth, int letlimit, int delta, int num_toplevels, int num_stxes)
{
  if (!SCHEME_PAIRP(data))
    scheme_ill_formed_code(port);
    
  scheme_validate_boxenv(SCHEME_INT_VAL(SCHEME_CAR(data)), port, stack, depth, delta);

  scheme_validate_expr(port, SCHEME_CDR(data), stack, depth, letlimit, delta, num_toplevels, num_stxes);
}



/* A bangboxval step is inserted by the compilation of `let' forms
   where the RHS is bound to a variable that will be set!ed. */

static Scheme_Object *
bangboxvalue_execute(Scheme_Object *data)
{
  int pos, cnt;
  Scheme_Object *val;

  pos = SCHEME_INT_VAL(SCHEME_CAR(data));
  data = SCHEME_CDR(data);
  cnt = SCHEME_INT_VAL(SCHEME_CAR(data));
  data = SCHEME_CDR(data);
  
  val = _scheme_eval_linked_expr_multi(data);

  if (SAME_OBJ(val, SCHEME_MULTIPLE_VALUES)) {
    Scheme_Thread *p = scheme_current_thread;
    if (cnt == p->ku.multiple.count) {
      Scheme_Object **naya, **a;
      int i;

      naya = MALLOC_N(Scheme_Object *, p->ku.multiple.count);
      a = p->ku.multiple.array;

      for (i = p->ku.multiple.count; i--; ) {
	naya[i] = a[i];
      }
      {
	Scheme_Object *eb;
	eb = scheme_make_envunbox(naya[pos]);
	naya[pos] = eb;
      }

      p->ku.multiple.array = naya;
    }
  } else if (cnt == 1)
    val = scheme_make_envunbox(val);

  return val;
}

static Scheme_Object *bangboxvalue_jit(Scheme_Object *data)
{
  Scheme_Object *orig, *naya;

  orig = SCHEME_CDR(data);
  orig = SCHEME_CDR(orig);
  naya = scheme_jit_expr(orig);
  if (SAME_OBJ(naya, orig))
    return data;
  else
    return cons(SCHEME_CAR(data),
		cons(SCHEME_CADR(data),
		     naya));
}

static void bangboxvalue_validate(Scheme_Object *data, Mz_CPort *port, 
				  char *stack, int depth, int letlimit, int delta, int num_toplevels, int num_stxes)
{
  if (!SCHEME_PAIRP(data)
      || !SCHEME_PAIRP(SCHEME_CDR(data))
      || (SCHEME_INT_VAL(SCHEME_CADR(data)) < 0)
      || (SCHEME_INT_VAL(SCHEME_CADR(data)) <= SCHEME_INT_VAL(SCHEME_CAR(data))))
    scheme_ill_formed_code(port);
  
  scheme_validate_expr(port, SCHEME_CDR(SCHEME_CDR(data)), stack, depth, letlimit, delta, num_toplevels, num_stxes);
}

/**********************************************************************/
/*                  let, let-values, letrec, etc.                     */
/**********************************************************************/

static int is_liftable(Scheme_Object *o, int bind_count, int fuel)
{
  Scheme_Type t = SCHEME_TYPE(o);

  switch (t) {
  case scheme_compiled_toplevel_type:
    return 1;
  case scheme_local_type:
    if (SCHEME_LOCAL_POS(o) > bind_count)
      return 1;
    break;
  case scheme_branch_type:
    if (fuel) {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)o;
      if (is_liftable(b->test, bind_count, fuel - 1)
	  && is_liftable(b->tbranch, bind_count, fuel - 1)
	  && is_liftable(b->fbranch, bind_count, fuel - 1))
	return 1;
    }
    break;
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)o;
      int i;
      for (i = app->num_args + 1; i--; ) {
	if (!is_liftable(app->args[i], bind_count + app->num_args, fuel - 1))
	  return 0;
      }
      return 1;
    }
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)o;
      if (is_liftable(app->rator, bind_count + 1, fuel - 1)
	  && is_liftable(app->rand, bind_count + 1, fuel - 1))
	return 1;
    }
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)o;
      if (is_liftable(app->rator, bind_count + 2, fuel - 1)
	  && is_liftable(app->rand1, bind_count + 2, fuel - 1)
	  && is_liftable(app->rand2, bind_count + 2, fuel - 1))
	return 1;
    }
  default:
    if (t > _scheme_compiled_values_types_)
      return 1;
  }

  return 0;
}

int scheme_compiled_propagate_ok(Scheme_Object *value)
{
  if (scheme_compiled_duplicate_ok(value))
    return 1;

  if (SAME_TYPE(SCHEME_TYPE(value), scheme_compiled_unclosed_procedure_type)) {
    int sz;
    sz = scheme_closure_body_size((Scheme_Closure_Data *)value, 1);
    if ((sz >= 0) && (sz <= MAX_PROC_INLINE_SIZE))
      return 1;
  }

  return 0;
}

Scheme_Object *
scheme_optimize_lets(Scheme_Object *form, Optimize_Info *info, int for_inline)
{
  Optimize_Info *body_info, *rhs_info;
  Scheme_Let_Header *head = (Scheme_Let_Header *)form;
  Scheme_Compiled_Let_Value *clv, *pre_body, *retry_start;
  Scheme_Object *body, *value;
  int i, j, pos, is_rec, max_let_depth = 0, all_simple = 1, skipped = 0;
  int size_before_opt, did_set_value;

  /* Special case: (let ([x E]) x) where E is lambda, case-lambda, or
     a constant. (If we allowed arbitrary E here, it would affect the
     tailness of E.) */
  if (!SCHEME_LET_RECURSIVE(head) && (head->count == 1) && (head->num_clauses == 1)) {
    clv = (Scheme_Compiled_Let_Value *)head->body;
    if (SAME_TYPE(SCHEME_TYPE(clv->body), scheme_local_type)
	&& (((Scheme_Local *)clv->body)->position == 0)) {
      Scheme_Type lhs;
      lhs = SCHEME_TYPE(clv->value);
      if ((lhs == scheme_compiled_unclosed_procedure_type)
	  || (lhs > _scheme_compiled_values_types_)) {
	if (for_inline) {
	  /* Just drop the inline-introduced let */
	  return scheme_optimize_expr(clv->value, info);
	} else {
	  info = scheme_optimize_info_add_frame(info, 1, 0, 0);
	  body = scheme_optimize_expr(clv->value, info);
	  scheme_optimize_info_done(info);
	  return body;
	}
      }
    }
  }

  body_info = scheme_optimize_info_add_frame(info, head->count, head->count, 0);
  if (for_inline) {
    rhs_info = scheme_optimize_info_add_frame(info, 0, head->count, 0);
    body_info->inline_fuel >>= 1;
  } else
    rhs_info = body_info;

  is_rec = SCHEME_LET_RECURSIVE(head);
  if (is_rec)
    all_simple = 0;

  body = head->body;
  pos = 0;
  for (i = head->num_clauses; i--; ) {
    pre_body = (Scheme_Compiled_Let_Value *)body;
    if (pre_body->count != 1)
      all_simple = 0;
    for (j = pre_body->count; j--; ) {
      if (pre_body->flags[j] & SCHEME_WAS_SET_BANGED) {
	scheme_optimize_mutated(body_info, pos + j);
	all_simple = 0;
      }
    }
    pos += pre_body->count;
    body = pre_body->body;
  }

  body = head->body;
  pre_body = NULL;
  retry_start = NULL;
  did_set_value = 0;
  pos = 0;
  for (i = head->num_clauses; i--; ) {
    pre_body = (Scheme_Compiled_Let_Value *)body;

    if (!retry_start)
      retry_start = pre_body;

    size_before_opt = body_info->size;
    
    value = scheme_optimize_expr(pre_body->value, rhs_info);

    pre_body->value = value;
    if ((pre_body->count == 1)
	&& !(pre_body->flags[0] & SCHEME_WAS_SET_BANGED)) {

      if (SAME_TYPE(SCHEME_TYPE(value), scheme_local_type)) {
	/* Don't optimize reference to a local binding
	   that's not available yet, or that mutable. */
	int vpos;
	vpos = SCHEME_LOCAL_POS(value);
	if ((vpos < head->count) && (vpos >= pos))
	  value = NULL;
	else {
	  /* Convert value back to a pre-optimized local coordinate.
	     This must be done with respect to body_info, not
	     rhs_info, because we attach the value to body_info: */
	  value = scheme_optimize_reverse(body_info, vpos, 1);
	}
      }

      if (value && (scheme_compiled_propagate_ok(value))) {
	scheme_optimize_propagate(body_info, pos, value);
	did_set_value = 1;
      }
    }

    /* Re-optimize to inline letrec bindings? */
    if (is_rec
	&& !body_info->letrec_not_twice
	&& ((i < 1) 
	    || (!scheme_is_compiled_procedure(((Scheme_Compiled_Let_Value *)pre_body->body)->value, 1)
		&& !is_liftable(((Scheme_Compiled_Let_Value *)pre_body->body)->value, head->count, 5)))) {
      if (did_set_value) {
	/* Next RHS ends a reorderable sequence. 
	   Re-optimize from retry_start to pre_body, inclusive. */
	while (1) {
	  value = retry_start->value;
	  if (SAME_TYPE(SCHEME_TYPE(value), scheme_compiled_unclosed_procedure_type)) {
	    Scheme_Object *self_value;
	    self_value = scheme_optimize_clone(value, body_info, 0, 0);
	    if (self_value) {
	      /* Try optimization. */
	      int sz;

	      /* Drop old size, and remove old inline fuel: */
	      sz = scheme_closure_body_size((Scheme_Closure_Data *)value, 0);
	      body_info->size -= (sz + 1);

	      /* Setting letrec_not_twice prevents inlinining
		 of letrec bindings in this RHS. There's a small
		 chance that we miss some optimizations, but we
		 avoid the possibility of N^2 behavior. */
	      body_info->letrec_not_twice = 1;

	      value = scheme_optimize_expr(self_value, body_info);

	      body_info->letrec_not_twice = 0;

	      retry_start->value = value;
	    }
	  }
	  if (retry_start == pre_body)
	    break;
	  retry_start = (Scheme_Compiled_Let_Value *)retry_start->body;
	}
      }
      retry_start = NULL;
      did_set_value = 0;
    }
    
    if (rhs_info->max_let_depth > max_let_depth)
      max_let_depth = rhs_info->max_let_depth;
    rhs_info->max_let_depth = 0;

    pos += pre_body->count;
    body = pre_body->body;
    info->size += 1;
  }

  if (for_inline) {
    body_info->size = rhs_info->size;
  }

  body = scheme_optimize_expr(body, body_info);
  pre_body->body = body;
  info->size += 1;

  /* Clear used flags where possible */
  if (all_simple) {    
    body = head->body;
    pos = 0;
    for (i = head->num_clauses; i--; ) {
      pre_body = (Scheme_Compiled_Let_Value *)body;
      if (!scheme_optimize_is_used(body_info, pos)
	  && scheme_omittable_expr(pre_body->value, 1)) {
	skipped++;
	if (pre_body->flags[0] & SCHEME_WAS_USED) {
	  pre_body->flags[0] -= SCHEME_WAS_USED;
	}
      } else {
	pre_body->flags[0] |= SCHEME_WAS_USED;
      }
      pos += pre_body->count;
      body = pre_body->body;
    }
  }

  if (body_info->max_let_depth > max_let_depth)
    max_let_depth = body_info->max_let_depth;

  body_info->max_let_depth = (max_let_depth + head->count - skipped);

  scheme_optimize_info_done(body_info);

  return form;
}

Scheme_Object *
scheme_optimize_lets_for_test(Scheme_Object *form, Optimize_Info *info)
/* Special case for when the `let' expression appears in an `if' test */
{
  Scheme_Let_Header *head = (Scheme_Let_Header *)form;

  /* Special case: (let ([x M]) (if x x N)), where x is not in N,
     to (if M #t #f), since we're in a test position. */
  if (!SCHEME_LET_RECURSIVE(head) && (head->count == 1) && (head->num_clauses == 1)) {
    Scheme_Compiled_Let_Value *clv;
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
	Optimize_Info *sub_info;

	b3 = MALLOC_ONE_TAGGED(Scheme_Branch_Rec);
	b3->so.type = scheme_branch_type;
	b3->test = clv->value;
	b3->tbranch = scheme_true;
	b3->fbranch = b->fbranch;

	sub_info = scheme_optimize_info_add_frame(info, 1, 0, 0);
	
	form = scheme_optimize_expr((Scheme_Object *)b3, sub_info);

	scheme_optimize_info_done(sub_info);

	return form;
      }
    }
  }


  return scheme_optimize_lets(form, info, 0);
}

Scheme_Object *
scheme_resolve_lets(Scheme_Object *form, Resolve_Info *info)
{
  Resolve_Info *linfo, *val_linfo;
  Scheme_Let_Header *head = (Scheme_Let_Header *)form;
  Scheme_Compiled_Let_Value *clv, *pre_body;
  Scheme_Let_Value *lv, *last = NULL;
  Scheme_Object *first = NULL, *body;
  Scheme_Letrec *letrec;
  mzshort *skips, skips_fast[5];
  int i, pos, opos, rpos, recbox, num_rec_procs = 0, extra_alloc;

  /* Find body: */
  body = head->body;
  pre_body = NULL;
  for (i = head->num_clauses; i--; ) {
    pre_body = (Scheme_Compiled_Let_Value *)body;
    body = pre_body->body;
  }

  recbox = 0;
  if (SCHEME_LET_RECURSIVE(head)) {
    /* Do we need to box vars in a letrec? */
    clv = (Scheme_Compiled_Let_Value *)head->body;
    for (i = head->num_clauses; i--; clv = (Scheme_Compiled_Let_Value *)clv->body) {
      int is_proc, is_lift;

      is_proc = scheme_is_compiled_procedure(clv->value, 1);
      if (is_proc)
	is_lift = 0;
      else
	is_lift = is_liftable(clv->value, head->count, 5);
      
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

	  if (scheme_is_compiled_procedure(clv->value, 0))
	    num_rec_procs++;
	}
      }
    }

    if (recbox)
      num_rec_procs = 0;
  } else {
    /* Sequence of single-value lets? */
    clv = (Scheme_Compiled_Let_Value *)head->body;
    for (i = head->num_clauses; i--; clv = (Scheme_Compiled_Let_Value *)clv->body) {
      if (clv->count != 1)
	break;
      if (clv->flags[0] & SCHEME_WAS_SET_BANGED)
	break;
    }
    if (i < 0) {
      /* Yes - build chain of Scheme_Let_Ones and we're done: */
      int skip_count = 0, frame_size;
      int j, k;

      j = head->num_clauses;
      if (j <= 5)
	skips = skips_fast; 
      else
	skips = MALLOC_N_ATOMIC(mzshort, j);

      clv = (Scheme_Compiled_Let_Value *)head->body;
      for (i = 0; i < j; i++, clv = (Scheme_Compiled_Let_Value *)clv->body) {
	if (!(clv->flags[0] & SCHEME_WAS_USED))
	  skips[i] = 1;
	else
	  skips[i] = 0;
      }

      clv = (Scheme_Compiled_Let_Value *)head->body;
      for (i = 0; i < head->num_clauses; i++, clv = (Scheme_Compiled_Let_Value *)clv->body) {
	Scheme_Object *le;

	if (!(clv->flags[0] & SCHEME_WAS_USED)) {
	  skip_count++;
	}

	/* First `i+1' bindings now exist "at runtime", except those skipped. */
	/* The mapping is complicated because we now push in the order of 
	   the variables, but it was compiled using the inverse order. */
	frame_size = i + 1 - skip_count;
	linfo = scheme_resolve_info_extend(info, frame_size, head->count, i + 1);
	for (j = i, k = 0; j >= 0; j--) {
	  if (skips[j])
	    scheme_resolve_info_add_mapping(linfo, j, 0, 0);
	  else
	    scheme_resolve_info_add_mapping(linfo, j, k++, 0);
	}

	le = scheme_resolve_expr(clv->value, linfo);

	if (!(clv->flags[0] & SCHEME_WAS_USED)) {
	  /* Unused binding, so drop it. */
	} else {
	  Scheme_Let_One *lo;
	  int et;

	  lo = MALLOC_ONE_TAGGED(Scheme_Let_One);
	  lo->iso.so.type = scheme_let_one_type;
	  lo->value = le;

	  et = scheme_get_eval_type(lo->value);
	  SCHEME_LET_EVAL_TYPE(lo) = et;

	  if (last)
	    ((Scheme_Let_One *)last)->body = (Scheme_Object *)lo;
	  else
	    first = (Scheme_Object *)lo;
	  last = (Scheme_Let_Value *)lo;
	}
      }

      frame_size = head->count - skip_count;
      linfo = scheme_resolve_info_extend(info, frame_size, head->count, head->count);
      for (k = 0, i = head->count; i--; ) {
	if (skips[i])
	  scheme_resolve_info_add_mapping(linfo, i, ((skips[i] < 0)
						     ? (k - skips[i] - 1)
						     : (skips[i] - 1 + frame_size)), 0);
	else
	  scheme_resolve_info_add_mapping(linfo, i, k++, 0);
      }
      
      body = scheme_resolve_expr(body, linfo);
      if (last)
	((Scheme_Let_One *)last)->body = body;
      else
	first = body;
      
      return first;
    }
  }

  linfo = scheme_resolve_info_extend(info, head->count, head->count, head->count);

  /* Build mapping of compile-time indices to run-time indices, shuffling
     letrecs to fall together: */
  clv = (Scheme_Compiled_Let_Value *)head->body;
  pos = num_rec_procs;
  rpos = 0; opos = 0;
  for (i = head->num_clauses; i--; clv = (Scheme_Compiled_Let_Value *)clv->body) {
    int j;

    for (j = 0; j < clv->count; j++) {
      int p;

      if (num_rec_procs && scheme_is_compiled_procedure(clv->value, 0))
	p = rpos++;
      else
	p = pos++;
      
      scheme_resolve_info_add_mapping(linfo, opos, p,
				      ((recbox 
					|| (clv->flags[j] & SCHEME_WAS_SET_BANGED))
				       ? SCHEME_INFO_BOXED
				       : 0));

      opos++;
    }
  }

  /* Resolve body: */
  body = scheme_resolve_expr(body, linfo);

  extra_alloc = 0;
  val_linfo = linfo;

  if (num_rec_procs) {
    Scheme_Object **sa;
    letrec = MALLOC_ONE_TAGGED(Scheme_Letrec);
    letrec->so.type = scheme_letrec_type;
    letrec->count = num_rec_procs;
    sa = MALLOC_N(Scheme_Object *, num_rec_procs);
    letrec->procs = sa;
  } else
    letrec = NULL;

  /* Resolve values: */
  clv = (Scheme_Compiled_Let_Value *)head->body;
  rpos = 0; opos = 0;
  for (i = head->num_clauses; i--; clv = (Scheme_Compiled_Let_Value *)clv->body) {
    int isproc;
    Scheme_Object *expr;
    isproc = scheme_is_compiled_procedure(clv->value, 0);
    expr = scheme_resolve_expr(clv->value, val_linfo);
    if (num_rec_procs && isproc) {
      letrec->procs[rpos++] = expr;
    } else {
      int j;

      lv = MALLOC_ONE_TAGGED(Scheme_Let_Value);
      if (last)
	last->body = (Scheme_Object *)lv;
      else
	first = (Scheme_Object *)lv;
      last = lv;
      
      lv->iso.so.type = scheme_let_value_type;
      lv->value = expr;
      if (clv->count) {
	int li;
	li = scheme_resolve_info_lookup(linfo, clv->position, NULL);
	lv->position = li + extra_alloc;
      } else
	lv->position = 0;
      lv->count = clv->count;
      SCHEME_LET_AUTOBOX(lv) = recbox;

      for (j = lv->count; j--; ) {
	if (!recbox
	    && (scheme_resolve_info_flags(linfo, opos + j) & SCHEME_INFO_BOXED)) {
	  Scheme_Object *sl;
	  /* See bangboxval... */
	  sl = scheme_make_syntax_resolved(BOXVAL_EXPD, 
					   cons(scheme_make_integer(j),
						cons(scheme_make_integer(lv->count),
						     lv->value)));
	  lv->value = sl;
	}
      }
    }
    opos += clv->count;
  }
  
  if (letrec) {
    letrec->body = body;
    if (last)
      last->body = (Scheme_Object *)letrec;
    else
      first = (Scheme_Object *)letrec;
  } else
    last->body = body;

  {
    Scheme_Let_Void *lvd;

    lvd = MALLOC_ONE_TAGGED(Scheme_Let_Void);
    lvd->iso.so.type = scheme_let_void_type;
    lvd->body = first;
    lvd->count = head->count + extra_alloc;
    SCHEME_LET_AUTOBOX(lvd) = recbox;

    first = (Scheme_Object *)lvd;
  }

  return first;
}

static Scheme_Object *
gen_let_syntax (Scheme_Object *form, Scheme_Comp_Env *origenv, char *formname,
		int star, int recursive, int multi, Scheme_Compile_Info *rec, int drec,
		Scheme_Comp_Env *frame_already)
{
  Scheme_Object *bindings, *l, *binding, *name, **names, *forms, *defname;
  int num_clauses, num_bindings, i, j, k, m, pre_k;
  Scheme_Comp_Env *frame, *env;
  Scheme_Compile_Info *recs;
  Scheme_Object *first = NULL;
  Scheme_Compiled_Let_Value *last = NULL, *lv;
  DupCheckRecord r;

  i = scheme_stx_proper_list_length(form);
  if (i < 3)
    scheme_wrong_syntax(NULL, NULL, form, (!i ? "bad syntax (empty body)" : NULL));

  bindings = SCHEME_STX_CDR(form);
  bindings = SCHEME_STX_CAR(bindings);
  num_clauses = scheme_stx_proper_list_length(bindings);

  if (num_clauses < 0)
    scheme_wrong_syntax(NULL, bindings, form, NULL);

  scheme_rec_add_certs(rec, drec, form);

  forms = SCHEME_STX_CDR(form);
  forms = SCHEME_STX_CDR(forms);
  forms = scheme_datum_to_syntax(forms, form, form, 0, 0);

  if (!num_clauses) {
    env = scheme_no_defines(origenv);

    name = scheme_check_name_property(form, rec[drec].value_name);
    rec[drec].value_name = name;

    return scheme_compile_sequence(forms, env, rec, drec);
  }
  
  if (multi) {
    num_bindings = 0;
    l = bindings;
    while (!SCHEME_STX_NULLP(l)) {
      Scheme_Object *clause, *names, *rest;
      int num_names;

      clause = SCHEME_STX_CAR(l);
      
      if (!SCHEME_STX_PAIRP(clause))
	rest = NULL;
      else {
	rest = SCHEME_STX_CDR(clause);
	if (!SCHEME_STX_PAIRP(rest))
	  rest = NULL;
	else {
	  rest = SCHEME_STX_CDR(rest);
	  if (!SCHEME_STX_NULLP(rest))
	    rest = NULL;
	}
      }
      if (!rest)
	scheme_wrong_syntax(NULL, clause, form, NULL);
      
      names = SCHEME_STX_CAR(clause);
      
      num_names = scheme_stx_proper_list_length(names);
      if (num_names < 0)
	scheme_wrong_syntax(NULL, names, form, NULL);
     
      num_bindings += num_names;
 
      l = SCHEME_STX_CDR(l);
    }
  } else
    num_bindings = num_clauses;


  names = MALLOC_N(Scheme_Object *, num_bindings);
  if (frame_already)
    frame = frame_already;
  else
    frame = scheme_new_compilation_frame(num_bindings, 0, origenv, rec[drec].certs);
  env = frame;

  recs = MALLOC_N_RT(Scheme_Compile_Info, (num_clauses + 1));

  defname = rec[drec].value_name;
  scheme_compile_rec_done_local(rec, drec);
  scheme_init_compile_recs(rec, drec, recs, num_clauses + 1);

  defname = scheme_check_name_property(form, defname);
  
  if (!star) {
    scheme_begin_dup_symbol_check(&r, env);
  }

  for (i = 0, k = 0; i < num_clauses; i++) {
    if (!SCHEME_STX_PAIRP(bindings))
      scheme_wrong_syntax(NULL, bindings, form, NULL);
    binding = SCHEME_STX_CAR(bindings);
    if (!SCHEME_STX_PAIRP(binding) || !SCHEME_STX_PAIRP(SCHEME_STX_CDR(binding)))
      scheme_wrong_syntax(NULL, binding, form, NULL);

    {
      Scheme_Object *rest;
      rest = SCHEME_STX_CDR(binding);
      if (!SCHEME_STX_NULLP(SCHEME_STX_CDR(rest)))
	scheme_wrong_syntax(NULL, binding, form, NULL);
    }
    
    pre_k = k;

    name = SCHEME_STX_CAR(binding);
    if (multi) {
      while (!SCHEME_STX_NULLP(name)) {
	Scheme_Object *n;
	n = SCHEME_STX_CAR(name);
	names[k] = n;
	scheme_check_identifier(NULL, names[k], NULL, env, form);
	k++;
	name = SCHEME_STX_CDR(name);
      }

      for (j = pre_k; j < k; j++) {
	for (m = j + 1; m < k; m++) {
	  if (scheme_stx_bound_eq(names[m], names[j], env->genv->phase))
	    scheme_wrong_syntax(NULL, NULL, form,
				"multiple bindings of `%S' in the same clause", 
				SCHEME_STX_SYM(names[m]));
	}
      }
    } else {
      scheme_check_identifier(NULL, name, NULL, env, form);
      names[k++] = name;
    }
    
    if (!star) {
      for (m = pre_k; m < k; m++) {
	scheme_dup_symbol_check(&r, NULL, names[m], "binding", form);
      }
    }

    lv = MALLOC_ONE_TAGGED(Scheme_Compiled_Let_Value);
    lv->so.type = scheme_compiled_let_value_type;
    if (!last)
      first = (Scheme_Object *)lv;
    else
      last->body = (Scheme_Object *)lv;
    last = lv;
    lv->count = (k - pre_k);
    lv->position = pre_k;

    if (lv->count == 1)
      recs[i].value_name = SCHEME_STX_SYM(names[pre_k]);

    if (!recursive) {
      Scheme_Object *ce, *rhs;
      rhs = SCHEME_STX_CDR(binding);
      rhs = SCHEME_STX_CAR(rhs);
      rhs = scheme_add_env_renames(rhs, env, origenv);
      ce = scheme_compile_expr(rhs, env, recs, i);
      lv->value = ce;
    } else {
      Scheme_Object *rhs;
      rhs = SCHEME_STX_CDR(binding);
      rhs = SCHEME_STX_CAR(rhs);
      lv->value = rhs;
    }
    
    if (star || recursive) {
      for (m = pre_k; m < k; m++) {
	scheme_add_compilation_binding(m, names[m], frame);
      }
    }
    
    bindings = SCHEME_STX_CDR(bindings);
  }
  
  if (!star && !recursive) {
    for (i = 0; i < num_bindings; i++) {
      scheme_add_compilation_binding(i, names[i], frame);
    }
  }

  if (recursive) {
    lv = (Scheme_Compiled_Let_Value *)first;
    for (i = 0; i < num_clauses; i++, lv = (Scheme_Compiled_Let_Value *)lv->body) {
      Scheme_Object *ce, *rhs;
      rhs = lv->value;
      rhs = scheme_add_env_renames(rhs, env, origenv);
      ce = scheme_compile_expr(rhs, env, recs, i);
      lv->value = ce;
    }
  }

  recs[num_clauses].value_name = defname ? SCHEME_STX_SYM(defname) : NULL;
  {
    Scheme_Object *cs;
    forms = scheme_add_env_renames(forms, env, origenv);
    cs = scheme_compile_sequence(forms, env, recs, num_clauses);
    last->body = cs;
  }

  /* Save flags: */
  lv = (Scheme_Compiled_Let_Value *)first;
  for (i = 0; i < num_clauses; i++, lv = (Scheme_Compiled_Let_Value *)lv->body) {
    int *flags;
    flags = scheme_env_get_flags(env, lv->position, lv->count);
    lv->flags = flags;
  }

  {
    Scheme_Let_Header *head;
    
    head = MALLOC_ONE_TAGGED(Scheme_Let_Header);
    head->iso.so.type = scheme_compiled_let_void_type;
    head->body = first;
    head->count = num_bindings;
    head->num_clauses = num_clauses;
    SCHEME_LET_RECURSIVE(head) = recursive;

    first = (Scheme_Object *)head;
  }
  
  scheme_merge_compile_recs(rec, drec, recs, num_clauses + 1);

  return first;
}

static Scheme_Object *
do_let_expand(Scheme_Object *form, Scheme_Comp_Env *origenv, Scheme_Expand_Info *erec, int drec,
	      const char *formname, int letrec, int multi, int letstar,
	      Scheme_Comp_Env *env_already)
{
  int named, partial;
  Scheme_Object *vars, *body, *first, *last, *name, *v, *vs, *vlist, *boundname;
  Scheme_Comp_Env *use_env, *env;
  Scheme_Expand_Info erec1;
  DupCheckRecord r;

  vars = SCHEME_STX_CDR(form);

  named = (!multi
	   && !letrec
	   && !letstar
	   && SCHEME_STX_PAIRP(vars) 
	   && SCHEME_STX_SYMBOLP(SCHEME_STX_CAR(vars)));
  
  if (named)
    return named_let_syntax(form, origenv, erec, drec);

  if (!SCHEME_STX_PAIRP(vars))
    scheme_wrong_syntax(NULL, NULL, form, NULL);

  body = SCHEME_STX_CDR(vars);
  vars = SCHEME_STX_CAR(vars);

  if (!SCHEME_STX_PAIRP(body))
    scheme_wrong_syntax(NULL, NULL, form, (SCHEME_STX_NULLP(body) 
					   ? "bad syntax (empty body)" 
					   : NULL));

  boundname = scheme_check_name_property(form, erec[drec].value_name);
  erec[drec].value_name = boundname;

  scheme_rec_add_certs(erec, drec, form);
  
  if (letstar) {
    if (!SCHEME_STX_NULLP(vars)) {
      Scheme_Object *a, *vr;

      if (!SCHEME_STX_PAIRP(vars))
	scheme_wrong_syntax(NULL, vars, form, NULL);

      a = SCHEME_STX_CAR(vars);
      vr = SCHEME_STX_CDR(vars);
      
      first = multi ? let_values_symbol : let_symbol;
      first = scheme_datum_to_syntax(first, form, scheme_sys_wraps(origenv), 0, 0);

      if (SCHEME_STX_NULLP(vr)) {
	/* Don't create redundant empty let form */
      } else {
	last = multi ? let_star_values_symbol : let_star_symbol;
	last = scheme_datum_to_syntax(last, form, scheme_sys_wraps(origenv), 0, 0);
	body = icons(icons(last, icons(vr, body)),
		     scheme_null);
      }
      
      body = icons(first,
		   icons(icons(a, scheme_null),
			 body));
    } else {
      first = scheme_datum_to_syntax(let_values_symbol, form, scheme_sys_wraps(origenv), 0, 0);
      body = icons(first, icons(scheme_null, body));
    }

    body = scheme_datum_to_syntax(body, form, form, 0, 2);

    first = SCHEME_STX_CAR(form);
    body = scheme_stx_track(body, form, first);

    if (erec[drec].depth > 0)
      --erec[drec].depth;

    if (!erec[drec].depth)
      return body;
    else {
      env = scheme_no_defines(origenv);
      return scheme_expand_expr(body, env, erec, drec);
    }
  }

  /* Note: no more letstar handling needed after this point */

  /* Check whether this is a partial expansion terminating in the
     `-values' form. If so, don't recursively expand here and don't
     introduce syntactic renamings (i.e., act like a non-primitive
     macro). */
  if (!multi) {
    v = (letrec 
	 ? letrec_values_symbol 
	 : let_values_symbol) ;
    v = scheme_datum_to_syntax(v, scheme_false, scheme_sys_wraps(origenv), 0, 0);
    v = scheme_lookup_binding(v, origenv,
			      SCHEME_NULL_FOR_UNBOUND
			      + SCHEME_APP_POS + SCHEME_ENV_CONSTANTS_OK
			      + SCHEME_DONT_MARK_USE,
			      erec[drec].certs, origenv->in_modidx, 
			      NULL, NULL);
    first = scheme_get_stop_expander();
    partial = SAME_OBJ(first, v);
  } else
    partial = 0;

  scheme_begin_dup_symbol_check(&r, origenv);

  vlist = scheme_null;
  vs = vars;
  while (SCHEME_STX_PAIRP(vs)) {
    Scheme_Object *v2;
    v = SCHEME_STX_CAR(vs);
    if (SCHEME_STX_PAIRP(v))
      v2 = SCHEME_STX_CDR(v);
    else
      v2 = scheme_false;
    if (!SCHEME_STX_PAIRP(v2) || !SCHEME_STX_NULLP(SCHEME_STX_CDR(v2)))
      scheme_wrong_syntax(NULL, v, form, NULL);

    name = SCHEME_STX_CAR(v);
  
    if (multi) {
      DupCheckRecord r2;
      Scheme_Object *names = name;
      scheme_begin_dup_symbol_check(&r2, origenv);
      while (SCHEME_STX_PAIRP(names)) {
	name = SCHEME_STX_CAR(names);

	scheme_check_identifier(NULL, name, NULL, origenv, form);
	vlist = cons(name, vlist);

	scheme_dup_symbol_check(&r2, NULL, name, "clause binding", form);
	scheme_dup_symbol_check(&r, NULL, name, "binding", form);
	
	names = SCHEME_STX_CDR(names);
      }
      if (!SCHEME_STX_NULLP(names))
	scheme_wrong_syntax(NULL, names, form, NULL);
    } else {
      scheme_check_identifier(NULL, name, NULL, origenv, form);
      vlist = cons(name, vlist);
      scheme_dup_symbol_check(&r, NULL, name, "binding", form);
    }

    vs = SCHEME_STX_CDR(vs);
  }

  if (!SCHEME_STX_NULLP(vs))
    scheme_wrong_syntax(NULL, vs, form, NULL);

  use_env = origenv;
  if (env_already)
    env = env_already;
  else if (partial)
    env = origenv;
  else
    env = scheme_add_compilation_frame(vlist, origenv,0,erec[drec].certs);

  if (letrec)
    use_env = env;

  first = last = NULL;
  vs = vars;
  while (SCHEME_STX_PAIRP(vars)) {
    Scheme_Object *rhs, *rhs_name;

    v = SCHEME_STX_CAR(vars);

    /* Make sure names gets their own renames: */
    name = SCHEME_STX_CAR(v);
    if (!multi) {
      if (!partial)
	name = scheme_add_env_renames(name, env, origenv);	
      name = icons(name, scheme_null);
    } else {
      if (!partial)
	name = scheme_add_env_renames(name, env, origenv);
    }

    rhs = SCHEME_STX_CDR(v);
    rhs = SCHEME_STX_CAR(rhs);
    if (!partial)
      rhs = scheme_add_env_renames(rhs, use_env, origenv);
    
    if (SCHEME_STX_PAIRP(name) && SCHEME_STX_NULLP(SCHEME_STX_CDR(name))) {
      rhs_name = SCHEME_STX_CAR(name);
    } else {
      rhs_name = scheme_false;
    }

    if (!partial) {
      scheme_init_expand_recs(erec, drec, &erec1, 1);
      erec1.value_name = rhs_name;
      rhs = scheme_expand_expr(rhs, use_env, &erec1, 0);
    }

    v = scheme_datum_to_syntax(icons(name, icons(rhs, scheme_null)), v, v, 0, 1);
    v = icons(v, scheme_null);

    if (!first)
      first = v;
    else
      SCHEME_CDR(last) = v;

    last = v;

    vars = SCHEME_STX_CDR(vars);
  }

  if (!SCHEME_STX_NULLP(vars))
    scheme_wrong_syntax(NULL, vars, form, NULL);
  
  if (!first)
    first = scheme_null;

  first = scheme_datum_to_syntax(first, vs, vs, 0, 1);

  body = scheme_datum_to_syntax(body, form, form, 0, 0);
  if (!partial) {
    body = scheme_add_env_renames(body, env, origenv);
    scheme_init_expand_recs(erec, drec, &erec1, 1);
    erec1.value_name = erec[drec].value_name;
    body = scheme_expand_block(body, env, &erec1, 0);
  }

  if (multi)
    v = SCHEME_STX_CAR(form);
  else
    v = scheme_datum_to_syntax((letrec 
				? letrec_values_symbol 
				: let_values_symbol),
			       form, scheme_sys_wraps(origenv), 
			       0, 0);

  v = icons(v, icons(first, body));

  v = scheme_datum_to_syntax(v, form, form, 0, multi ? 2 : -1);
  if (!multi) {
    name = SCHEME_STX_CAR(form);
    v = scheme_stx_track(v, form, name);
  }

  return v;
}

static Scheme_Object *
let_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  return do_let_expand(form, env, erec, drec, "let", 0, 0, 0, NULL);
}

static Scheme_Object *
let_star_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  return do_let_expand(form, env, erec, drec, "let*", 0, 0, 1, NULL);
}

static Scheme_Object *
letrec_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  return do_let_expand(form, env, erec, drec, "letrec", 1, 0, 0, NULL);
}

static Scheme_Object *
let_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  return do_let_expand(form, env, erec, drec, "let", 0, 1, 0, NULL);
}

static Scheme_Object *
let_star_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  return do_let_expand(form, env, erec, drec, "let*", 0, 1, 1, NULL);
}

static Scheme_Object *
letrec_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  return do_let_expand(form, env, erec, drec, "letrec", 1, 1, 0, NULL);
}

static Scheme_Object *
let_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *rest;

  rest = SCHEME_STX_CDR(form);
  if (!SCHEME_STX_PAIRP(rest))
    scheme_wrong_syntax(NULL, NULL, form, (SCHEME_STX_NULLP(rest)
					   ? NULL
					   : "bad syntax (" IMPROPER_LIST_FORM ")"));

  if (SCHEME_STX_SYMBOLP(SCHEME_STX_CAR(rest)))
    return named_let_syntax (form, env, rec, drec);
  
  return gen_let_syntax(form, env, "let", 0, 0, 0, rec, drec, NULL);
}

static Scheme_Object *
let_star_syntax (Scheme_Object *form, Scheme_Comp_Env *env, 
		 Scheme_Compile_Info *rec, int drec)
{
  return gen_let_syntax(form, env, "let*", 1, 0, 0, rec, drec, NULL);
}

static Scheme_Object *
letrec_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return gen_let_syntax(form, env, "letrec", 0, 1, 0, rec, drec, NULL);
}

static Scheme_Object *
let_values_syntax (Scheme_Object *form, Scheme_Comp_Env *env, 
		   Scheme_Compile_Info *rec, int drec)
{
  return gen_let_syntax(form, env, "let-values", 0, 0, 1, rec, drec, NULL);
}

static Scheme_Object *
let_star_values_syntax (Scheme_Object *form, Scheme_Comp_Env *env, 
		 Scheme_Compile_Info *rec, int drec)
{
  return gen_let_syntax(form, env, "let*-values", 1, 0, 1, rec, drec, NULL);
}

static Scheme_Object *
letrec_values_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return gen_let_syntax(form, env, "letrec-values", 0, 1, 1, rec, drec, NULL);
}

static Scheme_Object *nl_car(Scheme_Object *l, Scheme_Object *form)
{
  Scheme_Object *s;
  
  if (!SCHEME_STX_PAIRP(l))
    scheme_wrong_syntax("named let", l, form, 
			"bad syntax (not an identifier-value pair)");
  s = SCHEME_STX_CAR(l);
  if (!SCHEME_STX_SYMBOLP(s))
    scheme_wrong_syntax("named let", s, form, 
			"bad syntax (name not an identifier)");

  return s;
}

static Scheme_Object *nl_cadr(Scheme_Object *l, Scheme_Object *form)
{
  Scheme_Object *rest;
  
  if (!SCHEME_STX_PAIRP(l) || !SCHEME_STX_PAIRP(SCHEME_STX_CDR(l)))
    scheme_wrong_syntax("named let", l, form, 
			"bad syntax (not an identifier-value pair)");
  
  rest = SCHEME_STX_CDR(l);
  if (!SCHEME_STX_NULLP(SCHEME_STX_CDR(rest)))
    scheme_wrong_syntax("named let", l, form, 
			"bad syntax (extra form in indentifier-value pair)");
  
  return SCHEME_STX_CAR(rest);
}

static Scheme_Object *
named_let_syntax (Scheme_Object *form, Scheme_Comp_Env *env, 
		  Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *name, *bindings, *vars, *vals, *forms, *rest, *_vars, *_vals, *v;
  Scheme_Object *proc, *app, *letrec;

  rest = SCHEME_STX_CDR(form);
  if (!SCHEME_STX_PAIRP(rest))
    rest = NULL;
  else {
    rest = SCHEME_STX_CDR(rest);
    if (!SCHEME_STX_PAIRP(rest))
      rest = NULL;
    else {
      rest = SCHEME_STX_CDR(rest);
      if (!SCHEME_STX_PAIRP(rest))
	rest = NULL;
    }
  }

  if (!rest)
    scheme_wrong_syntax("named let", NULL, form, NULL);

  rest = SCHEME_STX_CDR(form);
  name = SCHEME_STX_CAR(rest);
  rest = SCHEME_STX_CDR(rest);
  bindings = SCHEME_STX_CAR(rest);
  if (!SCHEME_STX_PAIRP(bindings) && !SCHEME_STX_NULLP(bindings))
    scheme_wrong_syntax("named let", bindings, form, NULL);

  vars = scheme_named_map_1("named let", nl_car, bindings, form);
  vals = scheme_named_map_1("named let", nl_cadr, bindings, form);

  /* Add inferred-name attribute to arguments: */
  for (_vars = vars, _vals = vals; SCHEME_PAIRP(_vars); _vars = SCHEME_CDR(_vars), _vals = SCHEME_CDR(_vals)) {
    v = scheme_stx_property(SCHEME_CAR(_vals), scheme_inferred_name_symbol, NULL);
    if (SCHEME_FALSEP(v)) {
      v = scheme_stx_property(SCHEME_CAR(_vals), scheme_inferred_name_symbol, SCHEME_STX_VAL(SCHEME_CAR(_vars)));
      SCHEME_CAR(_vals) = v;
    }
  }

  forms = SCHEME_STX_CDR(form);
  forms = SCHEME_STX_CDR(forms);
  forms = SCHEME_STX_CDR(forms);

  proc = icons(lambda_symbol, icons(vars, forms));
  
  letrec = icons(letrec_symbol,
		 icons(icons(icons(name, icons(proc, scheme_null)), scheme_null),
		       icons(name,
			     scheme_null)));
  app = icons(letrec, vals);

  app = scheme_datum_to_syntax(app, form, scheme_sys_wraps(env), 0, 2);

  if (rec[drec].comp)
    return scheme_compile_expr(app, env, rec, drec);
  else {
    name = SCHEME_STX_CAR(form);
    app = scheme_stx_track(app, form, name);

    if (rec[drec].depth > 0)
      --rec[drec].depth;
    if (!rec[drec].depth)
      return app;
    else
      return scheme_expand_expr(app, env, rec, drec);
  }
}

/**********************************************************************/
/*                   begin, begin0, implicit begins                   */
/**********************************************************************/

Scheme_Object *scheme_compile_sequence(Scheme_Object *forms,
				       Scheme_Comp_Env *env, 
				       Scheme_Compile_Info *rec, int drec)
{
  if (SCHEME_STX_PAIRP(forms) && SCHEME_STX_NULLP(SCHEME_STX_CDR(forms))) {
    /* If it's a begin, we have to check some more... */
    Scheme_Object *first, *val;

    first = SCHEME_STX_CAR(forms);
    first = scheme_check_immediate_macro(first, env, rec, drec, 0, &val, NULL, NULL);

    if (SAME_OBJ(val, scheme_begin_syntax) && SCHEME_STX_PAIRP(first)) {      
      /* Flatten begin: */
      Scheme_Object *rest;
      rest = SCHEME_STX_CDR(first);
      if (scheme_stx_proper_list_length(rest) > 0) {
	first = scheme_datum_to_syntax(rest, first, first, 0, 2);
	return scheme_compile_sequence(first, env, rec, drec);
      }
    }

    return scheme_compile_expr(first, env, rec, drec);
  } else {
    if (scheme_stx_proper_list_length(forms) < 0) {
      scheme_wrong_syntax(scheme_begin_stx_string, NULL, 
			  scheme_datum_to_syntax(icons(begin_symbol, forms), forms, forms, 0, 0),
			  "bad syntax (" IMPROPER_LIST_FORM ")");
      return NULL;
    } else {
      Scheme_Object *body;
      body = scheme_compile_block(forms, env, rec, drec);
      return scheme_make_sequence_compilation(body, 1);
    }
  }
}

Scheme_Object *scheme_compiled_void()
{
  return scheme_void;
}

static Scheme_Object *
begin0_execute(Scheme_Object *obj)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *v, **mv;
  int i, mc, apos;
  
  i = ((Scheme_Sequence *)obj)->count;

  v = _scheme_eval_linked_expr_multi_wp(((Scheme_Sequence *)obj)->array[0], p);
  i--;
  if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES)) {
    mv = p->ku.multiple.array;
    mc = p->ku.multiple.count;
    if (SAME_OBJ(mv, p->values_buffer))
      p->values_buffer = NULL;
  } else {
    mv = NULL;
    mc = 0; /* makes compilers happy */
  }

  apos = 1;
  while (i--) {
    (void)_scheme_eval_linked_expr_multi_wp(((Scheme_Sequence *)obj)->array[apos++], p);
  }

  if (mv) {
    p->ku.multiple.array = mv;
    p->ku.multiple.count = mc;
  }

  return v;
}

static Scheme_Object *begin0_jit(Scheme_Object *data)
{
  Scheme_Sequence *seq = (Scheme_Sequence *)data, *seq2;
  Scheme_Object *old, *naya = NULL;
  int i, j, count;

  count = seq->count;
  for (i = 0; i < count; i++) {
    old = seq->array[i];
    naya = scheme_jit_expr(old);
    if (!SAME_OBJ(old, naya))
      break;
  }

  if (i >= count)
    return data;

  seq2 = (Scheme_Sequence *)scheme_malloc_tagged(sizeof(Scheme_Sequence)
						 + (count - 1) 
						 * sizeof(Scheme_Object *));
  seq2->so.type = scheme_begin0_sequence_type;
  seq2->count = count;
  for (j = 0; j < i; j++) {
    seq2->array[j] = seq->array[j];
  }
  seq2->array[i] = naya;
  for (i++; i < count; i++) {
    old = seq->array[i];
    naya = scheme_jit_expr(old);
    seq2->array[i] = naya;
  }
  
  return (Scheme_Object *)seq2;
}

static void begin0_validate(Scheme_Object *data, Mz_CPort *port, char *stack, 
			    int depth, int letlimit, int delta, int num_toplevels, int num_stxes)
{
  Scheme_Sequence *seq = (Scheme_Sequence *)data;
  int i;

  for (i = 0; i < seq->count; i++) { 
    scheme_validate_expr(port, seq->array[i], stack, depth, letlimit, delta, num_toplevels, num_stxes);
  }
}

static Scheme_Object *
begin0_optimize(Scheme_Object *obj, Optimize_Info *info)
{
  int i;
  
  i = ((Scheme_Sequence *)obj)->count;

  while (i--) {
    Scheme_Object *le;
    le = scheme_optimize_expr(((Scheme_Sequence *)obj)->array[i], info);
    ((Scheme_Sequence *)obj)->array[i] = le;
  }

  return scheme_make_syntax_compiled(BEGIN0_EXPD, obj);
}

static Scheme_Object *
begin0_clone(Scheme_Object *obj, Optimize_Info *info, int delta, int closure_depth)
{
  obj = scheme_optimize_clone(obj, info, delta, closure_depth);
  if (!obj) return NULL;
  return scheme_make_syntax_compiled(BEGIN0_EXPD, obj);
}

static Scheme_Object *
begin0_resolve(Scheme_Object *obj, Resolve_Info *info)
{
  int i;
  
  i = ((Scheme_Sequence *)obj)->count;

  while (i--) {
    Scheme_Object *le;
    le = scheme_resolve_expr(((Scheme_Sequence *)obj)->array[i], info);
    ((Scheme_Sequence *)obj)->array[i] = le;
  }

  return scheme_make_syntax_resolved(BEGIN0_EXPD, obj);
}

static Scheme_Object *
do_begin_syntax(char *name,
		Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec, 
		int zero)
{
  Scheme_Object *forms, *body;

  forms = SCHEME_STX_CDR(form);
  
  if (SCHEME_STX_NULLP(forms)) {
    if (!zero && scheme_is_toplevel(env))
      return scheme_compiled_void();
    scheme_wrong_syntax(NULL, NULL, form, "bad syntax (empty form)");
    return NULL;
  }

  check_form(form, form);

  if (zero)
    env = scheme_no_defines(env);

  if (SCHEME_STX_NULLP(SCHEME_STX_CDR(forms))) {
    scheme_rec_add_certs(rec, drec, form);
    forms = SCHEME_STX_CAR(forms);
    return scheme_compile_expr(forms, env, rec, drec);
  }

  if (!scheme_is_toplevel(env)) {
    /* Not at top-level */
    if (zero) {
      /* First expression is not part of the block: */
      Scheme_Compile_Info recs[2];
      Scheme_Object *first, *rest, *vname;

      vname = rec[drec].value_name;
      scheme_compile_rec_done_local(rec, drec);

      vname = scheme_check_name_property(form, vname);

      scheme_rec_add_certs(rec, drec, form);

      scheme_init_compile_recs(rec, drec, recs, 2);
      recs[0].value_name = vname;

      first = SCHEME_STX_CAR(forms);
      first = scheme_compile_expr(first, env, recs, 0);
      rest = SCHEME_STX_CDR(forms);
      rest = scheme_compile_list(rest, env, recs, 1);
      
      scheme_merge_compile_recs(rec, drec, recs, 2);

      body = icons(first, rest);
    } else {
      Scheme_Object *v;
      v = scheme_check_name_property(form, rec[drec].value_name);
      rec[drec].value_name = v;
      scheme_rec_add_certs(rec, drec, form);

      body = scheme_compile_list(forms, env, rec, drec);
    }
  } else {
    /* Top level */
    scheme_rec_add_certs(rec, drec, form);
    body = scheme_compile_list(forms, env, rec, drec);
  }

  forms = scheme_make_sequence_compilation(body, zero ? -1 : 1);

  if (!zero || (NOT_SAME_TYPE(SCHEME_TYPE(forms), scheme_begin0_sequence_type)))
    return forms;

  return scheme_make_syntax_compiled(BEGIN0_EXPD, forms);
}

static Scheme_Object *
begin_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_begin_syntax("begin", form, env, rec, drec, 0);
}

static Scheme_Object *
begin0_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_begin_syntax("begin0", form, env, rec, drec, 1);
}

static Scheme_Object *
do_begin_expand(char *name,
		Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec,
		int zero)
{
  Scheme_Object *form_name;
  Scheme_Object *rest;
  Scheme_Object *orig_form = form;

  check_form(form, form);

  form_name = SCHEME_STX_CAR(form);

  rest = SCHEME_STX_CDR(form);

  if (SCHEME_STX_NULLP(rest)) {
    if (!zero && scheme_is_toplevel(env))
      return form;
    scheme_wrong_syntax(NULL, NULL, form, "bad syntax (empty form)");
    return NULL;
  }

  if (zero)
    env = scheme_no_defines(env);

  if (!scheme_is_toplevel(env)) {
    /* Not at top-level: */
    if (zero) {
      Scheme_Object *fst, *boundname;
      Scheme_Expand_Info erec1;
      scheme_rec_add_certs(erec, drec, form);
      scheme_init_expand_recs(erec, drec, &erec1, 1);
      boundname = scheme_check_name_property(form, erec[drec].value_name);
      erec1.value_name = boundname;
      erec[drec].value_name = scheme_false;
      fst = SCHEME_STX_CAR(rest);
      rest = SCHEME_STX_CDR(rest);
      form = icons(scheme_expand_expr(fst, env, &erec1, 0),
		   scheme_expand_list(scheme_datum_to_syntax(rest, 
							     form, 
							     form, 0, 0),
				      env, erec, drec));
    } else {
      Scheme_Object *boundname;
      boundname = scheme_check_name_property(form, erec[drec].value_name);
      erec[drec].value_name = boundname;
      scheme_rec_add_certs(erec, drec, form);
      
      form = scheme_expand_list(scheme_datum_to_syntax(rest, form, form, 0, 0),
				env, erec, drec);
#if 0
      if (SCHEME_STX_NULLP(SCHEME_STX_CDR(form)))
	return SCHEME_STX_CAR(form);
#endif
    }
  } else {
    /* Top level */
    scheme_rec_add_certs(erec, drec, form);
    form =  scheme_expand_list(scheme_datum_to_syntax(rest, form, form, 0, 0),
			       env, erec, drec);
  }

  return scheme_datum_to_syntax(icons(form_name, form), 
				orig_form, orig_form, 
				0, 2);
}

static Scheme_Object *
begin_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  return do_begin_expand("begin", form, env, erec, drec, 0);
}

static Scheme_Object *
begin0_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  return do_begin_expand("begin0", form, env, erec, drec, 1);
}

/**********************************************************************/
/*                      unquote, unquote-splicing                     */
/**********************************************************************/

static Scheme_Object *
unquote_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  int len;

  if (rec[drec].comp)
    scheme_compile_rec_done_local(rec, drec);

  len = check_form(form, form);
  if (len != 2)
    bad_form(form, len);

  scheme_wrong_syntax(NULL, NULL, form, "not in quasiquote");
  return NULL;
}

static Scheme_Object *
unquote_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  return unquote_syntax(form, env, erec, drec);
}

/**********************************************************************/
/*                            quote-syntax                            */
/**********************************************************************/

static Scheme_Object *
quote_syntax_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  int len;
  Scheme_Object *stx;

  if (rec[drec].comp)
    scheme_compile_rec_done_local(rec, drec);

  len = check_form(form, form);
  if (len != 2)
    bad_form(form, len);

  scheme_rec_add_certs(rec, drec, form);

  stx = SCHEME_STX_CDR(form);
  stx = SCHEME_STX_CAR(stx);

  /* Push all certificates in the environment down to the syntax object. */
  stx = scheme_stx_add_inactive_certs(stx, rec[drec].certs);
  
  if (rec[drec].comp) {
    return scheme_register_stx_in_prefix(stx, env, rec, drec);
  } else {
    Scheme_Object *fn;
    fn = SCHEME_STX_CAR(form);
    return scheme_datum_to_syntax(icons(fn, icons(stx, scheme_null)),
				  form,
				  form, 
				  0, 2);
  }
}

static Scheme_Object *
quote_syntax_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  return quote_syntax_syntax(form, env, erec, drec);
}


/**********************************************************************/
/*                          define-syntaxes                           */
/**********************************************************************/

static Scheme_Object *do_define_syntaxes_execute(Scheme_Object *expr, Scheme_Env *dm_env, int for_stx);

static void *define_syntaxes_execute_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *form = p->ku.k.p1;
  Scheme_Env *dm_env = (Scheme_Env *)p->ku.k.p2;
  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  return do_define_syntaxes_execute(form, dm_env, p->ku.k.i1);
}

static Scheme_Object *
do_define_syntaxes_execute(Scheme_Object *form, Scheme_Env *dm_env, int for_stx)
{
  Scheme_Thread *p = scheme_current_thread;
  Resolve_Prefix *rp;
  Scheme_Object *base_stack_depth, *dummy;
  int depth;
  Scheme_Comp_Env *rhs_env;

  rp = (Resolve_Prefix *)SCHEME_CAR(form);
  base_stack_depth = SCHEME_CADR(form);

  depth = SCHEME_INT_VAL(base_stack_depth) + rp->num_stxes + 1;
  if (!scheme_check_runstack(depth)) {
    p->ku.k.p1 = form;

    if (!dm_env) {
      /* Need to get env before we enlarge the runstack: */
      form = SCHEME_CDDR(form);
      dummy = SCHEME_CAR(form);
      dm_env = scheme_environment_from_dummy(dummy);
    }
    p->ku.k.p2 = (Scheme_Object *)dm_env;
    p->ku.k.i1 = for_stx;

    return (Scheme_Object *)scheme_enlarge_runstack(depth, define_syntaxes_execute_k);
  }

  form = SCHEME_CDDR(form);
  dummy = SCHEME_CAR(form);
  form = SCHEME_CDR(form);

  rhs_env = scheme_new_comp_env(scheme_get_env(NULL), NULL, SCHEME_TOPLEVEL_FRAME);

  if (!dm_env)
    dm_env = scheme_environment_from_dummy(dummy);

  scheme_on_next_top(rhs_env, NULL, scheme_false, NULL, dm_env, dm_env->link_midx);
  return define_execute(SCHEME_CAR(form), SCHEME_CDR(form), for_stx ? 2 : 1, rp, dm_env);
}

static Scheme_Object *
define_syntaxes_execute(Scheme_Object *form)
{
  return do_define_syntaxes_execute(form, NULL, 0);
}

static Scheme_Object *
define_for_syntaxes_execute(Scheme_Object *form)
{
  return do_define_syntaxes_execute(form, NULL, 1);
}

static Scheme_Object *do_define_syntaxes_jit(Scheme_Object *expr)
{
  Scheme_Object *orig, *naya, *data = expr;
  Scheme_Object *a, *ad, *add;
  
  a = SCHEME_CAR(data);
  data = SCHEME_CDR(data);
  ad = SCHEME_CAR(data);
  data = SCHEME_CDR(data);
  add = SCHEME_CAR(data);
  data = SCHEME_CDR(data);

  orig = SCHEME_CDR(data);
  
  naya = scheme_jit_expr(orig);
  
  if (SAME_OBJ(naya, orig))
    return expr;
  else {
    return cons(a, 
		cons(ad, 
		     cons(add, 
			  cons(SCHEME_CAR(data),
			       naya))));
  }
}

static Scheme_Object *define_syntaxes_jit(Scheme_Object *expr)
{
  return do_define_syntaxes_jit(expr);
}

static Scheme_Object *define_for_syntaxes_jit(Scheme_Object *expr)
{
  return do_define_syntaxes_jit(expr);
}

static void do_define_syntaxes_validate(Scheme_Object *data, Mz_CPort *port, 
					char *stack, int depth, int letlimit, int delta, 
					int num_toplevels, int num_stxes,
					int for_stx)
{
  Resolve_Prefix *rp;
  Scheme_Object *names, *val, *base_stack_depth, *dummy;
  int sdepth;

  if (!SCHEME_PAIRP(data)
      || !SCHEME_PAIRP(SCHEME_CDR(data)))
    scheme_ill_formed_code(port);

  rp = (Resolve_Prefix *)SCHEME_CAR(data);
  base_stack_depth = SCHEME_CADR(data);
  sdepth = SCHEME_INT_VAL(base_stack_depth);

  data = SCHEME_CDDR(data);
  if (!SCHEME_PAIRP(data)
      || !SCHEME_PAIRP(SCHEME_CDR(data))
      || !SAME_TYPE(rp->so.type, scheme_resolve_prefix_type)
      || (sdepth < 0))
    scheme_ill_formed_code(port);

  dummy = SCHEME_CAR(data);
  data = SCHEME_CDR(data);
  names = SCHEME_CAR(data);
  val = SCHEME_CDR(data);

  if (!for_stx) {
    for (; SCHEME_PAIRP(names); names = SCHEME_CDR(names)) {
      if (!SCHEME_SYMBOLP(SCHEME_CAR(names)))
	scheme_ill_formed_code(port);
    }
    if (!SCHEME_NULLP(names))
      scheme_ill_formed_code(port);
  }

  scheme_validate_toplevel(dummy,  port, stack, depth, delta, num_toplevels, num_stxes);
  
  if (!for_stx) {
    scheme_validate_code(port, val, sdepth, rp->num_toplevels, rp->num_stxes);
  } else {
    /* Make a fake `define-values' to check with respect to the exp-time stack */
    val = scheme_make_syntax_resolved(DEFINE_VALUES_EXPD, cons(names, val));
    scheme_validate_code(port, val, sdepth, rp->num_toplevels, rp->num_stxes);
  }
}

static void define_syntaxes_validate(Scheme_Object *data, Mz_CPort *port, 
				     char *stack, int depth, int letlimit, int delta, 
				     int num_toplevels, int num_stxes)
{
  do_define_syntaxes_validate(data, port, stack, depth, letlimit, delta, num_toplevels, num_stxes, 0);
}

static void define_for_syntaxes_validate(Scheme_Object *data, Mz_CPort *port, 
					 char *stack, int depth, int letlimit, int delta, 
					 int num_toplevels, int num_stxes)
{
  do_define_syntaxes_validate(data, port, stack, depth, letlimit, delta, num_toplevels, num_stxes, 1);
}

static Scheme_Object *do_define_syntaxes_optimize(Scheme_Object *data, Optimize_Info *info, int for_stx)
{
  Scheme_Object *cp, *names, *val, *base_stack_depth, *dummy;
  Optimize_Info *einfo;

  cp = SCHEME_CAR(data);
  data = SCHEME_CDDR(data);
  dummy = SCHEME_CAR(data);
  data = SCHEME_CDR(data);

  names = SCHEME_CAR(data);
  val = SCHEME_CDR(data);

  einfo = scheme_optimize_info_create();

  val = scheme_optimize_expr(val, einfo);

  base_stack_depth = scheme_make_integer(einfo->max_let_depth);

  return scheme_make_syntax_compiled((for_stx ? DEFINE_FOR_SYNTAX_EXPD : DEFINE_SYNTAX_EXPD), 
				     cons(cp,
					  cons(base_stack_depth,
					       cons(dummy,
						    cons(names, val)))));
}

static Scheme_Object *define_syntaxes_optimize(Scheme_Object *data, Optimize_Info *info)
{
  return do_define_syntaxes_optimize(data, info, 0);
}

static Scheme_Object *define_for_syntaxes_optimize(Scheme_Object *data, Optimize_Info *info)
{
  return do_define_syntaxes_optimize(data, info, 1);
}

static Scheme_Object *do_define_syntaxes_resolve(Scheme_Object *data, Resolve_Info *info, int for_stx)
{
  Comp_Prefix *cp;
  Resolve_Prefix *rp;
  Scheme_Object *names, *val, *base_stack_depth, *dummy;
  Resolve_Info *einfo;

  cp = (Comp_Prefix *)SCHEME_CAR(data);
  base_stack_depth = SCHEME_CADR(data);
  data = SCHEME_CDDR(data);
  dummy = SCHEME_CAR(data);
  data = SCHEME_CDR(data);

  names = SCHEME_CAR(data);
  val = SCHEME_CDR(data);

  rp = scheme_resolve_prefix(1, cp, 1);

  dummy = scheme_resolve_expr(dummy, info);

  einfo = scheme_resolve_info_create(rp);

  if (for_stx)
    names = scheme_resolve_list(names, einfo);
  val = scheme_resolve_expr(val, einfo);

  return scheme_make_syntax_resolved((for_stx ? DEFINE_FOR_SYNTAX_EXPD : DEFINE_SYNTAX_EXPD), 
				     cons((Scheme_Object *)rp,
					  cons(base_stack_depth,
					       cons(dummy,
						    cons(names, val)))));
}

static Scheme_Object *define_syntaxes_resolve(Scheme_Object *data, Resolve_Info *info)
{
  return do_define_syntaxes_resolve(data, info, 0);
}

static Scheme_Object *define_for_syntaxes_resolve(Scheme_Object *data, Resolve_Info *info)
{
  return do_define_syntaxes_resolve(data, info, 1);
}

static Scheme_Object *stx_val(Scheme_Object *name, Scheme_Object *_env)
{
  Scheme_Env *env = (Scheme_Env *)_env;

  return scheme_tl_id_sym(env, name, NULL, 2);
}

static Scheme_Object *
do_define_syntaxes_syntax(Scheme_Object *form, Scheme_Comp_Env *env, 
			  Scheme_Compile_Info *rec, int drec, int for_stx)
{
  Scheme_Object *names, *code, *dummy;
  Scheme_Object *val;
  Scheme_Comp_Env *exp_env;
  Scheme_Compile_Info rec1;

  scheme_compile_rec_done_local(rec, drec);
  scheme_default_compile_rec(rec, drec);
  scheme_rec_add_certs(rec, drec, form);
      
  scheme_define_parse(form, &names, &code, 1, env);

  scheme_prepare_exp_env(env->genv);

  if (!for_stx)
    names = scheme_named_map_1(NULL, stx_val, names, (Scheme_Object *)env->genv);

  exp_env = scheme_new_comp_env(env->genv->exp_env, env->insp, 0);

  dummy = scheme_make_environment_dummy(env);
  
  rec1.comp = 1;
  rec1.dont_mark_local_use = 0;
  rec1.resolve_module_ids = 0;
  rec1.value_name = NULL;
  rec1.certs = rec[drec].certs;

  if (for_stx) {
    names = defn_targets_syntax(names, exp_env, &rec1, 0);
    scheme_compile_rec_done_local(&rec1, 0);
  }

  val = scheme_compile_expr_lift_to_let(code, exp_env, &rec1, 0);

  return scheme_make_syntax_compiled((for_stx ? DEFINE_FOR_SYNTAX_EXPD : DEFINE_SYNTAX_EXPD), 
				     cons((Scheme_Object *)exp_env->prefix, 
					  cons(scheme_make_integer(0),
					       cons(dummy,
						    cons(names, val)))));
}

static Scheme_Object *
define_syntaxes_syntax(Scheme_Object *form, Scheme_Comp_Env *env, 
		       Scheme_Compile_Info *rec, int drec)
{
  return do_define_syntaxes_syntax(form, env, rec, drec, 0);
}

static Scheme_Object *
define_for_syntaxes_syntax(Scheme_Object *form, Scheme_Comp_Env *env, 
			   Scheme_Compile_Info *rec, int drec)
{
  return do_define_syntaxes_syntax(form, env, rec, drec, 1);
}

static Scheme_Object *
define_syntaxes_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  Scheme_Object *names, *code, *fpart, *fn;

  scheme_prepare_exp_env(env->genv);

  scheme_define_parse(form, &names, &code, 1, env);
  
  env = scheme_new_expand_env(env->genv->exp_env, env->insp, 0);

  scheme_rec_add_certs(erec, drec, form);
  erec[drec].value_name = names;
  fpart = scheme_expand_expr_lift_to_let(code, env, erec, drec);
  
  code = icons(fpart, scheme_null);
  code = icons(names, code);

  fn = SCHEME_STX_CAR(form);
  return scheme_datum_to_syntax(icons(fn, code), 
				form, form, 
				0, 2);
}

static Scheme_Object *
define_for_syntaxes_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  return define_syntaxes_expand(form, env, erec, drec);
}

Scheme_Object *scheme_make_environment_dummy(Scheme_Comp_Env *env)
{ 
  Scheme_Object *dummy;
 
  /* Get prefixed-based accessors for a dummy top-level buckets. It's
     used to "link" to the right enviornment. begin_symbol is arbitrary */
  dummy = (Scheme_Object *)scheme_global_bucket(begin_symbol, env->genv);
  dummy = scheme_register_toplevel_in_prefix(dummy, env, NULL, 0);

  return dummy;
}

Scheme_Env *scheme_environment_from_dummy(Scheme_Object *dummy)
{
  Scheme_Object **toplevels;
  Scheme_Bucket_With_Home *b;

  toplevels = (Scheme_Object **)MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(dummy)];
  b = (Scheme_Bucket_With_Home *)toplevels[SCHEME_TOPLEVEL_POS(dummy)];
  return b->home;
}

/**********************************************************************/
/*                           letrec-syntaxes                          */
/**********************************************************************/

static void *eval_letmacro_rhs_k(void);

static Scheme_Object *eval_letmacro_rhs(Scheme_Object *a, Scheme_Comp_Env *rhs_env, 
					int max_let_depth, Resolve_Prefix *rp,
					int phase, Scheme_Object *certs)
{
  Scheme_Object **save_runstack;
  int depth;

  depth = max_let_depth + scheme_prefix_depth(rp);
  if (!scheme_check_runstack(depth)) {
    Scheme_Thread *p = scheme_current_thread;
    p->ku.k.p1 = a;
    p->ku.k.p2 = rhs_env;
    p->ku.k.p3 = rp;
    p->ku.k.p4 = certs;
    p->ku.k.i1 = max_let_depth;
    p->ku.k.i2 = phase;
    return (Scheme_Object *)scheme_enlarge_runstack(depth, eval_letmacro_rhs_k);
  }

  save_runstack = scheme_push_prefix(NULL, rp, NULL, NULL, phase, phase);

  if (scheme_omittable_expr(a, 1)) {
    /* short cut */
    a = _scheme_eval_linked_expr_multi(a);
  } else {
    scheme_on_next_top(rhs_env, NULL, scheme_false, certs, rhs_env->genv, rhs_env->genv->link_midx);
    a = scheme_eval_linked_expr_multi(a);
  }

  scheme_pop_prefix(save_runstack);

  return a;
}

static void *eval_letmacro_rhs_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *a, *certs; 
  Scheme_Comp_Env *rhs_env;
  int max_let_depth, phase;
  Resolve_Prefix *rp;

  a = (Scheme_Object *)p->ku.k.p1;
  rhs_env = (Scheme_Comp_Env *)p->ku.k.p2;
  rp = (Resolve_Prefix *)p->ku.k.p3;
  certs = (Scheme_Object *)p->ku.k.p4;
  max_let_depth = p->ku.k.i1;
  phase = p->ku.k.i2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;

  return (void *)eval_letmacro_rhs(a, rhs_env, max_let_depth, rp, phase, certs);
}


Scheme_Object *scheme_bind_syntaxes(const char *where, Scheme_Object *names, Scheme_Object *a, 
				    Scheme_Env *exp_env, Scheme_Object *insp, Scheme_Object *certs,
				    Scheme_Comp_Env *stx_env, Scheme_Comp_Env *rhs_env,
				    int *_pos, Scheme_Object *names_to_disappear)
{
  Scheme_Object **results, *l;
  Scheme_Comp_Env *eenv;
  Resolve_Prefix *rp;
  Resolve_Info *ri;
  Optimize_Info *oi;
  int vc, nc, j, i;
  Scheme_Compile_Info mrec;

  mrec.comp = 1;
  mrec.dont_mark_local_use = 0;
  mrec.resolve_module_ids = 1;
  mrec.value_name = NULL;
  mrec.certs = certs;

  eenv = scheme_new_comp_env(exp_env, insp, 0);

  a = scheme_compile_expr_lift_to_let(a, eenv, &mrec, 0);
    
  /* For internal defn, don't simplify as resolving, because the
       expression may have syntax objects with a lexical rename that
       is still being extended. 
     For letrec-syntaxes+values, don't simplify because it's too expensive. */
  rp = scheme_resolve_prefix(eenv->genv->phase, eenv->prefix, 0);

  oi = scheme_optimize_info_create();
  a = scheme_optimize_expr(a, oi);

  ri = scheme_resolve_info_create(rp);
  a = scheme_resolve_expr(a, ri);

  /* To JIT:
       if (ri->use_jit) a = scheme_jit_expr(a);
     but it's not likely that a let-syntax-bound macro is going
     to run lots of times, so JITting is probably not worth it. */

  a = eval_letmacro_rhs(a, rhs_env, oi->max_let_depth, rp, eenv->genv->phase, certs);

  if (SAME_OBJ(a, SCHEME_MULTIPLE_VALUES))
    vc = scheme_current_thread->ku.multiple.count;
  else
    vc = 1;

  for (nc = 0, l = names; SCHEME_STX_PAIRP(l); l = SCHEME_STX_CDR(l)) {
    nc++;
  }
  
  if (vc != nc) {
    Scheme_Object *name;
    const char *symname;
    
    if (nc >= 1) {
      name = SCHEME_STX_CAR(names);
      name = SCHEME_STX_VAL(name);
    } else
      name = NULL;
    symname = (name ? scheme_symbol_name(name) : "");
    
    scheme_wrong_return_arity(where,
			      nc, vc,
			      (vc == 1) ? (Scheme_Object **)a : scheme_current_thread->ku.multiple.array,
			      "%s%s%s",
			      name ? "defining \"" : "0 names",
			      symname,
			      name ? ((nc == 1) ? "\"" : "\", ...") : "");
  }

  results = scheme_current_thread->ku.multiple.array;
  scheme_current_thread->ku.multiple.array = NULL;
  if (SAME_OBJ(results, scheme_current_thread->values_buffer))
    scheme_current_thread->values_buffer = NULL;

  i = *_pos;
  for (j = 0, l = names; SCHEME_STX_PAIRP(l); l = SCHEME_STX_CDR(l), j++) {
    Scheme_Object *name, *macro;
    name = SCHEME_STX_CAR(l);
    
    macro = scheme_alloc_small_object();
    macro->type = scheme_macro_type;
    if (vc == 1)
      SCHEME_PTR_VAL(macro) = a;
    else
      SCHEME_PTR_VAL(macro) = results[j];
    
    scheme_set_local_syntax(i++, name, macro, stx_env);
    if (names_to_disappear)
      names_to_disappear = icons(name, names_to_disappear);
  }
  *_pos = i;

  return names_to_disappear;
}

static Scheme_Object *
do_letrec_syntaxes(const char *where,
		   Scheme_Object *forms, Scheme_Comp_Env *origenv, 
		   Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *form, *bindings, *var_bindings, *body, *v;
  Scheme_Object *names_to_disappear;
  Scheme_Comp_Env *stx_env, *var_env, *rhs_env;
  int cnt, stx_cnt, var_cnt, i, j, depth, saw_var;
  DupCheckRecord r;

  form = SCHEME_STX_CDR(forms);
  if (!SCHEME_STX_PAIRP(form))
    scheme_wrong_syntax(NULL, NULL, forms, NULL);
  bindings = SCHEME_STX_CAR(form);
  form = SCHEME_STX_CDR(form);
  if (!SCHEME_STX_PAIRP(form))
    scheme_wrong_syntax(NULL, NULL, forms, NULL);
  var_bindings = SCHEME_STX_CAR(form);
  form = SCHEME_STX_CDR(form);
  if (!SCHEME_STX_PAIRP(form))
    scheme_wrong_syntax(NULL, NULL, forms, NULL);
  body = scheme_datum_to_syntax(form, forms, forms, 0, 0);

  scheme_rec_add_certs(rec, drec, forms);

  stx_env = scheme_new_compilation_frame(0, 0, origenv, rec[drec].certs);

  rhs_env = stx_env;

  if (!SCHEME_STX_NULLP(bindings) && !SCHEME_STX_PAIRP(bindings)) {
    scheme_wrong_syntax(NULL, bindings, forms, "bad syntax (not a binding sequence)");
  } else
    check_form(bindings, forms);
  if (!SCHEME_STX_NULLP(var_bindings) && !SCHEME_STX_PAIRP(var_bindings)) {
    scheme_wrong_syntax(NULL, var_bindings, forms, "bad syntax (not a binding sequence)");
  } else
    check_form(var_bindings, forms);

  cnt = stx_cnt = var_cnt = 0;
  saw_var = 0;

  depth = rec[drec].depth;

  if (!rec[drec].comp && (depth <= 0) && (depth > -2))
    names_to_disappear = scheme_null;
  else
    names_to_disappear = NULL;


  scheme_begin_dup_symbol_check(&r, stx_env);

  for (i = 0; i < 2 ; i++) {
    for (v = (i ? var_bindings : bindings); SCHEME_STX_PAIRP(v); v = SCHEME_STX_CDR(v)) {
      Scheme_Object *a, *l;

      a = SCHEME_STX_CAR(v);
      if (!SCHEME_STX_PAIRP(a)
	  || !SCHEME_STX_PAIRP(SCHEME_STX_CDR(a)))
	v = NULL;
      else {
	for (l = SCHEME_STX_CAR(a); SCHEME_STX_PAIRP(l); l = SCHEME_STX_CDR(l)) {
	  if (!SCHEME_STX_SYMBOLP(SCHEME_STX_CAR(l)))
	    break;
	}
	if (!SCHEME_STX_NULLP(l))
	  v = NULL;
      }

      if (v) {
	Scheme_Object *rest;
	rest = SCHEME_STX_CDR(a);
	if (!SCHEME_STX_NULLP(SCHEME_STX_CDR(rest)))
	  v = NULL;
      }

      if (!v)
	scheme_wrong_syntax(NULL, a, forms, 
			    "bad syntax (binding clause not an identifier sequence and expression)");

      for (l = SCHEME_STX_CAR(a); SCHEME_STX_PAIRP(l); l = SCHEME_STX_CDR(l)) {
	a = SCHEME_STX_CAR(l);
	scheme_check_identifier(where, a, NULL, stx_env, forms);
	scheme_dup_symbol_check(&r, where, a, "binding", forms);
	cnt++;
      }
      if (i)
	saw_var = 1;
    }

    if (!i)
      stx_cnt = cnt;
    else
      var_cnt = cnt - stx_cnt;
  }

  scheme_add_local_syntax(stx_cnt, stx_env);
  if (saw_var)
    var_env = scheme_new_compilation_frame(var_cnt, 0, stx_env, rec[drec].certs);
  else
    var_env = NULL;

  for (i = 0; i < (var_env ? 2 : 1) ; i++) {
    cnt = (i ? var_cnt : stx_cnt);
    if (cnt > 0) {
      /* Add new syntax names to the environment: */
      j = 0;
      for (v = (i ? var_bindings : bindings); SCHEME_STX_PAIRP(v); v = SCHEME_STX_CDR(v)) {
	Scheme_Object *a, *l;
	
	a = SCHEME_STX_CAR(v);
	for (l = SCHEME_STX_CAR(a); SCHEME_STX_PAIRP(l); l = SCHEME_STX_CDR(l)) {
	  a = SCHEME_STX_CAR(l);
	  if (i) {
	    /* In compile mode, this will get re-written by the letrec compiler.
	       But that's ok. We need it now for env_renames. */
	    scheme_add_compilation_binding(j++, a, var_env);
	  } else
	    scheme_set_local_syntax(j++, a, NULL, stx_env);
	}
      }
    }
  }

  scheme_prepare_exp_env(stx_env->genv);

  i = 0;

  for (v = bindings; SCHEME_STX_PAIRP(v); v = SCHEME_STX_CDR(v)) {
    Scheme_Object *a, *names;

    a = SCHEME_STX_CAR(v);
    names = SCHEME_STX_CAR(a);
    a = SCHEME_STX_CDR(a);
    a = SCHEME_STX_CAR(a);

    a = scheme_add_env_renames(a, stx_env, origenv);
    if (var_env)
      a = scheme_add_env_renames(a, var_env, stx_env);

    names_to_disappear = scheme_bind_syntaxes(where, names, a,
					      stx_env->genv->exp_env, stx_env->insp, rec[drec].certs,
					      stx_env, rhs_env, 
					      &i, names_to_disappear);
  }

  body = scheme_add_env_renames(body, stx_env, origenv);
  if (names_to_disappear) {
    /* Need to add renaming for disappeared bindings --- unless
       they originated for internal definitions, in which case
       adding the renaming is unnecessary and intereferes with the
       comparsion (due to limitations of the syntax-object
       representation for internal definitions). */
    if (!(origenv->flags & SCHEME_FOR_INTDEF)) {
      Scheme_Object *l, *a;
      for (l = names_to_disappear; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
	a = SCHEME_CAR(l);
	a = scheme_add_env_renames(a, stx_env, origenv);
	SCHEME_CAR(l) = a;
      }
    }
  }
  if (var_env)
    var_bindings = scheme_add_env_renames(var_bindings, stx_env, origenv);

  if (!var_env) {
    var_env = scheme_require_renames(stx_env);
    if (rec[drec].comp) {
      v = scheme_check_name_property(forms, rec[drec].value_name);
      rec[drec].value_name = v;
      v = scheme_compile_block(body, var_env, rec, drec);
      v = scheme_make_sequence_compilation(v, 1);
    } else {
      v = scheme_expand_block(body, var_env, rec, drec);
      if ((depth >= 0) || (depth == -2)) {
	Scheme_Object *formname;
	formname = SCHEME_STX_CAR(forms);
	v = icons(formname, icons(bindings, icons(var_bindings, v)));
      } else
	/* Should this be `let' instead? */
	v = icons(begin_symbol, v);

      if (SCHEME_PAIRP(v))
	v = scheme_datum_to_syntax(v, forms, scheme_sys_wraps(origenv), 
				   0, 2);
      
    }
  } else {
    /* Construct letrec-values expression: */
    v = icons(letrec_values_symbol, icons(var_bindings, body));
    v = scheme_datum_to_syntax(v, forms, scheme_sys_wraps(origenv), 0, 2);
    
    if (rec[drec].comp) {
      v = gen_let_syntax(v, stx_env, "letrec-values", 0, 1, 1, rec, drec, var_env);
    } else {
      v = do_let_expand(v, stx_env, rec, drec, "letrec-values", 1, 1, 0, var_env);
      
      if ((depth >= 0) || (depth == -2)) {
	/* Pull back out the pieces we want: */
	Scheme_Object *formname;
	formname = SCHEME_STX_CAR(forms);
	v = SCHEME_STX_CDR(v);
	v = icons(formname, icons(bindings, v));
	v = scheme_datum_to_syntax(v, forms, scheme_sys_wraps(origenv), 0, 2);
      }
    }
  }

  /* Add the 'disappeared-binding property */
  if (names_to_disappear)
    v = scheme_stx_property(v, disappeared_binding_symbol, names_to_disappear);

  return v;
}

static Scheme_Object *
letrec_syntaxes_syntax(Scheme_Object *form, Scheme_Comp_Env *env, 
		       Scheme_Compile_Info *rec, int drec)
{
  return do_letrec_syntaxes("letrec-syntaxes+values", form, env, rec, drec);
}

static Scheme_Object *
letrec_syntaxes_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  return do_letrec_syntaxes("letrec-syntaxes+values", form, env, erec, drec);
}

/**********************************************************************/
/*                        marshal/unmarshal                           */
/**********************************************************************/

static Scheme_Object *write_let_value(Scheme_Object *obj)
{
  Scheme_Let_Value *lv;
 
  lv = (Scheme_Let_Value *)obj;

  return cons(scheme_make_integer(lv->count),
	      cons(scheme_make_integer(lv->position),
		   cons(SCHEME_LET_AUTOBOX(lv) ? scheme_true : scheme_false,
			cons(scheme_protect_quote(lv->value), 
			     scheme_protect_quote(lv->body)))));
}

static Scheme_Object *read_let_value(Scheme_Object *obj)
{
  Scheme_Let_Value *lv;
 
  lv = (Scheme_Let_Value *)scheme_malloc_tagged(sizeof(Scheme_Let_Value));
  lv->iso.so.type = scheme_let_value_type;

  if (!SCHEME_PAIRP(obj)) return NULL;
  lv->count = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return NULL;
  lv->position = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return NULL;
  SCHEME_LET_AUTOBOX(lv) = SCHEME_TRUEP(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return NULL;
  lv->value = SCHEME_CAR(obj);
  lv->body = SCHEME_CDR(obj);

  return (Scheme_Object *)lv;
}

static Scheme_Object *write_let_void(Scheme_Object *obj)
{
  Scheme_Let_Void *lv;
 
  lv = (Scheme_Let_Void *)obj;

  return cons(scheme_make_integer(lv->count), 
	      cons(SCHEME_LET_AUTOBOX(lv) ? scheme_true : scheme_false,
		   scheme_protect_quote(lv->body)));
}

static Scheme_Object *read_let_void(Scheme_Object *obj)
{
  Scheme_Let_Void *lv;
 
  lv = (Scheme_Let_Void *)scheme_malloc_tagged(sizeof(Scheme_Let_Void));
  lv->iso.so.type = scheme_let_void_type;
  
  if (!SCHEME_PAIRP(obj)) return NULL;
  lv->count = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return NULL;
  SCHEME_LET_AUTOBOX(lv) = SCHEME_TRUEP(SCHEME_CAR(obj));
  lv->body = SCHEME_CDR(obj);

  return (Scheme_Object *)lv;
}

static Scheme_Object *write_let_one(Scheme_Object *obj)
{
  scheme_signal_error("let-one writer shouldn't be used");
  return NULL;
}

static Scheme_Object *read_let_one(Scheme_Object *obj)
{
  return NULL;
}

static Scheme_Object *write_letrec(Scheme_Object *obj)
{
  Scheme_Letrec *lr = (Scheme_Letrec *)obj;
  Scheme_Object *l = scheme_null;
  int i = lr->count;
  
  while (i--) {
    l = cons(scheme_protect_quote(lr->procs[i]), l);
  }

  return cons(scheme_make_integer(lr->count), 
	      cons(scheme_protect_quote(lr->body), l));
}

static Scheme_Object *read_letrec(Scheme_Object *obj)
{
  Scheme_Letrec *lr;
  int i, c;
  Scheme_Object **sa;

  lr = MALLOC_ONE_TAGGED(Scheme_Letrec);

  lr->so.type = scheme_letrec_type;

  if (!SCHEME_PAIRP(obj)) return NULL;
  c = lr->count = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return NULL;
  lr->body = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);

  sa = MALLOC_N(Scheme_Object*, c);
  lr->procs = sa;
  for (i = 0; i < c; i++) {
    if (!SCHEME_PAIRP(obj)) return NULL;
    lr->procs[i] = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);
  }

  return (Scheme_Object *)lr;
}

static Scheme_Object *write_top(Scheme_Object *obj)
{
  Scheme_Compilation_Top *top = (Scheme_Compilation_Top *)obj;

  return cons(scheme_make_integer(top->max_let_depth),
	      cons((Scheme_Object *)top->prefix,
		   scheme_protect_quote(top->code)));
}

static Scheme_Object *read_top(Scheme_Object *obj)
{
  Scheme_Compilation_Top *top;

  top = MALLOC_ONE_TAGGED(Scheme_Compilation_Top);
  top->so.type = scheme_compilation_top_type;
  if (!SCHEME_PAIRP(obj)) return NULL;
  top->max_let_depth = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return NULL;
  top->prefix = (Resolve_Prefix *)SCHEME_CAR(obj);
  top->code = SCHEME_CDR(obj);

  return (Scheme_Object *)top;
}

static Scheme_Object *write_case_lambda(Scheme_Object *obj)
{
  Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)obj;
  int i;
  Scheme_Object *l;

  i = cl->count;

  l = scheme_null;
  for (; i--; ) {
    l = cons(cl->array[i], l);
  }
  
  return cons((cl->name ? cl->name : scheme_null),
	      l);
}

static Scheme_Object *read_case_lambda(Scheme_Object *obj)
{
  Scheme_Object *s, *a;
  int count, i, all_closed = 1;
  Scheme_Case_Lambda *cl;

  if (!SCHEME_PAIRP(obj)) return NULL;
  s = SCHEME_CDR(obj);
  for (count = 0; SCHEME_PAIRP(s); s = SCHEME_CDR(s)) {
    count++;
  }

  cl = (Scheme_Case_Lambda *)
    scheme_malloc_tagged(sizeof(Scheme_Case_Lambda)
			 + (count - 1) * sizeof(Scheme_Object *));

  cl->so.type = scheme_case_lambda_sequence_type;
  cl->count = count;
  cl->name = SCHEME_CAR(obj);
  if (SCHEME_NULLP(cl->name))
    cl->name = NULL;

  s = SCHEME_CDR(obj);
  for (i = 0; i < count; i++, s = SCHEME_CDR(s)) {
    a = SCHEME_CAR(s);
    cl->array[i] = a;
    if (!SCHEME_PROCP(a))
      all_closed = 0;
  }

  if (all_closed) {
    /* Empty closure: produce procedure value directly.
       (We assume that this was generated by a direct write of
        a case-lambda data record in print.c, and that it's not
	in a CASE_LAMBDA_EXPD syntax record.) */
    return case_lambda_execute((Scheme_Object *)cl);
  }

  return (Scheme_Object *)cl;
}

/**********************************************************************/
/*                            precise GC                              */
/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_SYNTAX_C
#include "mzmark.c"

static void register_traversers(void)
{
}

END_XFORM_SKIP;

#endif
