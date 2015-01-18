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
*/

/* This file implements macro expansion and compilation. Instead of
   always fully expanding code and then compiling it, the compiler
   expands as it goes, which enables some shortcuts compared to fully
   expanding first.

   See "eval.c" for an overview of compilation passes.

   The main compile/expand loop is scheme_compile_expand_expr(). */

#include "schpriv.h"
#include "schmach.h"
#include "schexpobs.h"

/* globals */
READ_ONLY Scheme_Object *scheme_define_values_syntax;
READ_ONLY Scheme_Object *scheme_define_syntaxes_syntax;
READ_ONLY Scheme_Object *scheme_ref_syntax;
READ_ONLY Scheme_Object *scheme_begin_syntax;
READ_ONLY Scheme_Object *scheme_lambda_syntax;
READ_ONLY Scheme_Object *scheme_compiled_void_code;
READ_ONLY Scheme_Object scheme_undefined[1];

/* read-only globals */
READ_ONLY static Scheme_Object *app_expander;
READ_ONLY static Scheme_Object *datum_expander;
READ_ONLY static Scheme_Object *top_expander;
READ_ONLY static Scheme_Object *stop_expander;

/* symbols */
ROSYM static Scheme_Object *lambda_symbol;
ROSYM static Scheme_Object *letrec_values_symbol;
ROSYM static Scheme_Object *let_star_values_symbol;
ROSYM static Scheme_Object *let_values_symbol;
ROSYM static Scheme_Object *begin_symbol;
ROSYM static Scheme_Object *disappeared_binding_symbol;
ROSYM static Scheme_Object *compiler_inline_hint_symbol;
ROSYM static Scheme_Object *app_symbol;
ROSYM static Scheme_Object *datum_symbol;
ROSYM static Scheme_Object *top_symbol;
ROSYM static Scheme_Object *protected_symbol;
ROSYM static Scheme_Object *quote_symbol;
ROSYM static Scheme_Object *letrec_syntaxes_symbol;
ROSYM static Scheme_Object *values_symbol;
ROSYM static Scheme_Object *call_with_values_symbol;
ROSYM static Scheme_Object *inferred_name_symbol;
ROSYM static Scheme_Object *undefined_error_name_symbol;

THREAD_LOCAL_DECL(static Scheme_Object *quick_stx);

THREAD_LOCAL_DECL(struct Scheme_Object *cwv_stx);
THREAD_LOCAL_DECL(int cwv_stx_phase);

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
static Scheme_Object *stratified_body_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *stratified_body_expand (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *expression_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *expression_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);

static Scheme_Object *unquote_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *unquote_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);

static Scheme_Object *with_cont_mark_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *with_cont_mark_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);

static Scheme_Object *quote_syntax_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *quote_syntax_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *define_syntaxes_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *define_syntaxes_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *begin_for_syntax_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *begin_for_syntax_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *letrec_syntaxes_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *letrec_syntaxes_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);

static Scheme_Object *app_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *app_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *datum_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *datum_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *top_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *top_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *stop_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *stop_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);

static Scheme_Object *expand_lam(int argc, Scheme_Object **argv);

static Scheme_Object *compile_block(Scheme_Object *forms, Scheme_Comp_Env *env,
                                    Scheme_Compile_Info *rec, int drec);
static Scheme_Object *compile_stratified_block(Scheme_Object *forms, Scheme_Comp_Env *env,
                                               Scheme_Compile_Info *rec, int drec);
static Scheme_Object *expand_block(Scheme_Object *form, Scheme_Comp_Env *env,
				   Scheme_Expand_Info *erec, int drec);
static Scheme_Object *expand_stratified_block(Scheme_Object *form, Scheme_Comp_Env *env,
                                              Scheme_Expand_Info *erec, int drec);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

#define cons(a,b) scheme_make_pair(a,b)
#define icons(a,b) scheme_make_pair(a,b)

/**********************************************************************/
/*                          initialization                            */
/**********************************************************************/

void scheme_init_compile (Scheme_Env *env)
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
  REGISTER_SO(letrec_values_symbol);
  REGISTER_SO(let_star_values_symbol);
  REGISTER_SO(let_values_symbol);
  REGISTER_SO(begin_symbol);
  REGISTER_SO(disappeared_binding_symbol);
  REGISTER_SO(compiler_inline_hint_symbol);

  REGISTER_SO(inferred_name_symbol);
  REGISTER_SO(undefined_error_name_symbol);

  scheme_undefined->type = scheme_undefined_type;
  
  lambda_symbol = scheme_intern_symbol("lambda");

  letrec_values_symbol = scheme_intern_symbol("letrec-values");
  let_star_values_symbol = scheme_intern_symbol("let*-values");
  let_values_symbol = scheme_intern_symbol("let-values");

  begin_symbol = scheme_intern_symbol("begin");

  disappeared_binding_symbol = scheme_intern_symbol("disappeared-binding");
  compiler_inline_hint_symbol = scheme_intern_symbol("compiler-hint:cross-module-inline");

  inferred_name_symbol = scheme_intern_symbol("inferred-name");
  undefined_error_name_symbol = scheme_intern_symbol("undefined-error-name");

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

  scheme_add_global_keyword("#%expression", 
			    scheme_make_compiled_syntax(expression_syntax,
							expression_expand), 
			    env);

  scheme_add_global_keyword("case-lambda", 
			    scheme_make_compiled_syntax(case_lambda_syntax, 
							case_lambda_expand), 
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
  scheme_add_global_keyword("#%stratified-body", 
                            scheme_make_compiled_syntax(stratified_body_syntax, 
                                                        stratified_body_expand), 
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
  scheme_add_global_keyword("begin-for-syntax", 
			    scheme_make_compiled_syntax(begin_for_syntax_syntax, 
							begin_for_syntax_expand),
			    env);
  scheme_add_global_keyword("letrec-syntaxes+values", 
			    scheme_make_compiled_syntax(letrec_syntaxes_syntax, 
							letrec_syntaxes_expand), 
			    env);
  
  REGISTER_SO(app_symbol);
  REGISTER_SO(datum_symbol);
  REGISTER_SO(top_symbol);
  REGISTER_SO(protected_symbol);
  REGISTER_SO(quote_symbol);
  REGISTER_SO(letrec_syntaxes_symbol);
  REGISTER_SO(values_symbol);
  REGISTER_SO(call_with_values_symbol);

  app_symbol    = scheme_intern_symbol("#%app");
  datum_symbol  = scheme_intern_symbol("#%datum");
  top_symbol    = scheme_intern_symbol("#%top");
  protected_symbol = scheme_intern_symbol("protected");
  quote_symbol  = scheme_intern_symbol("quote");
  letrec_syntaxes_symbol = scheme_intern_symbol("letrec-syntaxes+values");
  values_symbol = scheme_intern_symbol("values");
  call_with_values_symbol = scheme_intern_symbol("call-with-values");

  REGISTER_SO(app_expander);
  REGISTER_SO(datum_expander);
  REGISTER_SO(top_expander);
  REGISTER_SO(stop_expander);

  app_expander    = scheme_make_compiled_syntax(app_syntax,   app_expand);
  datum_expander  = scheme_make_compiled_syntax(datum_syntax, datum_expand);
  top_expander    = scheme_make_compiled_syntax(top_syntax,   top_expand);
  stop_expander   = scheme_make_compiled_syntax(stop_syntax,  stop_expand);
  scheme_add_global_keyword("#%app",    app_expander,   env);
  scheme_add_global_keyword("#%datum",  datum_expander, env);
  scheme_add_global_keyword("#%top",    top_expander,   env);

  scheme_init_marshal(env);
}

void scheme_init_compile_places()
{
  REGISTER_SO(quick_stx);
  REGISTER_SO(cwv_stx);
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
    scheme_wrong_syntax(NULL, form, base_form, IMPROPER_LIST_FORM);
  }

  return i;
}

static void bad_form(Scheme_Object *form, int l)
{ 
  scheme_wrong_syntax(NULL, NULL, form, 
		      "bad syntax;\n has %d part%s after keyword", 
		      l - 1, (l != 2) ? "s" : "");
}

Scheme_Object *scheme_check_name_property(Scheme_Object *code, Scheme_Object *current_val)
{
  Scheme_Object *name;

  name = scheme_stx_property(code, inferred_name_symbol, NULL);
  if (name && SCHEME_SYMBOLP(name))
    return name;
  else
    return current_val;
}

static Scheme_Object *get_local_name(Scheme_Object *id)
{
  Scheme_Object *name;

  name = scheme_stx_property(id, undefined_error_name_symbol, NULL);
  if (name && SCHEME_SYMBOLP(name))
    return name;
  else
    return SCHEME_STX_VAL(id);
}

/**********************************************************************/
/*                           lambda utils                             */
/**********************************************************************/

static Scheme_Object *lambda_check(Scheme_Object *form)
{
  form = scheme_stx_taint_disarm(form, NULL);

  if (SCHEME_STX_PAIRP(form)
      && SCHEME_STX_PAIRP(SCHEME_STX_CDR(form))) {
    Scheme_Object *rest;
    rest = SCHEME_STX_CDR(form);
    if (SCHEME_STX_PAIRP(SCHEME_STX_CDR(rest)))
      return form;
  }

  scheme_wrong_syntax(NULL, NULL, form, NULL);
  return NULL;
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
      sprintf(buf, "%s%s%" PRIdPTR ":%" PRIdPTR,
	      src, (src[0] ? ":" : ""), cstx->srcloc->line, cstx->srcloc->col - 1);
    } else {
      sprintf(buf, "%s%s%" PRIdPTR,
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

  name = scheme_stx_property(code, inferred_name_symbol, NULL);
  if (name && SCHEME_SYMBOLP(name)) {
    name = combine_name_with_srcloc(name, code, 0);
  } else if (name && SCHEME_VOIDP(name)) {
    name = scheme_source_to_name(code);
    if (name)
      name = combine_name_with_srcloc(name, code, 1);
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

static Scheme_Object *
make_closure_compilation(Scheme_Comp_Env *env, Scheme_Object *code,
                         Scheme_Compile_Info *rec, int drec)
/* Compiles a `lambda' expression */
{
  Scheme_Object *allparams, *params, *forms, *param, *name;
  Scheme_Closure_Data *data;
  Scheme_Compile_Info lam;
  Scheme_Comp_Env *frame;
  int i;
  intptr_t num_params;
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

  frame = scheme_new_compilation_frame(data->num_params, SCHEME_LAMBDA_FRAME, env);
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
    scheme_wrong_syntax(NULL, NULL, code, "empty body not allowed");

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
  SET_REQUIRED_TAG(cl->type = scheme_rt_closure_info);
  {
    int *local_flags;
    local_flags = scheme_env_get_flags(frame, 0, data->num_params);
    cl->local_flags = local_flags;
  }
  data->closure_map = (mzshort *)cl;

  return (Scheme_Object *)data;
}

static Scheme_Object *
lambda_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *args;

  form = lambda_check(form);

  args = SCHEME_STX_CDR(form);
  args = SCHEME_STX_CAR(args);
  lambda_check_args(args, form, env);

  return make_closure_compilation(env, form, rec, drec);
}

static Scheme_Object *
lambda_expand(Scheme_Object *orig_form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  Scheme_Object *args, *body, *fn, *form;
  Scheme_Comp_Env *newenv;
  Scheme_Expand_Info erec1;

  SCHEME_EXPAND_OBSERVE_PRIM_LAMBDA(erec[drec].observer);

  form = lambda_check(orig_form);
  
  args = SCHEME_STX_CDR(form);
  args = SCHEME_STX_CAR(args);

  lambda_check_args(args, form, env);

  newenv = scheme_add_compilation_frame(args, env, 0);

  body = SCHEME_STX_CDR(form);
  body = SCHEME_STX_CDR(body);
  body = scheme_datum_to_syntax(body, form, form, 0, 0);

  body = scheme_add_env_renames(body, newenv, env);

  args = scheme_add_env_renames(args, newenv, env); /* for re-expansion */
  SCHEME_EXPAND_OBSERVE_LAMBDA_RENAMES(erec[drec].observer, args, body);

  fn = SCHEME_STX_CAR(form);

  scheme_init_expand_recs(erec, drec, &erec1, 1);
  erec1.value_name = scheme_false;

  return scheme_datum_to_syntax(cons(fn,
				      cons(args,
                                           expand_block(body,
                                                        newenv,
                                                        &erec1, 
                                                        0))),
				orig_form, orig_form, 
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
  return scheme_datum_to_syntax(cons(fn, args), form, form, 0, 2);
}

Scheme_Object *scheme_clone_vector(Scheme_Object *data, int skip, int set_type)
{
  Scheme_Object *naya;
  int i, size;

  size = SCHEME_VEC_SIZE(data);
  naya = scheme_make_vector(size - skip, NULL);
  for (i = skip; i < size; i++) {
    SCHEME_VEC_ELS(naya)[i - skip] = SCHEME_VEC_ELS(data)[i];
  }

  if (set_type)
    naya->type = data->type;

  return naya;
}

void scheme_define_parse(Scheme_Object *form, 
                         Scheme_Object **var, Scheme_Object **_stk_val,
                         int defmacro,
                         Scheme_Comp_Env *env,
                         int no_toplevel_check)
{
  Scheme_Object *vars, *rest;
  int len;
  DupCheckRecord r;

  if (!no_toplevel_check && !scheme_is_toplevel(env))
    scheme_wrong_syntax(NULL, NULL, form, "not in a definition context");

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
    name = scheme_tl_id_sym(env->genv, name, NULL, 2, NULL, NULL);

    if (rec[drec].resolve_module_ids || !env->genv->module) {
      bucket = (Scheme_Object *)scheme_global_bucket(name, env->genv);
    } else {
      /* Create a module variable reference, so that idx is preserved: */
      bucket = scheme_hash_module_variable(env->genv, env->genv->module->self_modidx, 
					   name, env->genv->module->insp, 
					   -1, env->genv->mod_phase, 0,
                                           NULL);
    }
    /* Get indirection through the prefix: */
    bucket = scheme_register_toplevel_in_prefix(bucket, env, rec, drec, 0, NULL);

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
  Scheme_Object *var, *val, *targets, *variables, *vec;
  
  scheme_define_parse(form, &var, &val, 0, env, 0);
  variables = var;
  
  targets = defn_targets_syntax(var, env, rec, drec);

  scheme_compile_rec_done_local(rec, drec);
  if (SCHEME_STX_PAIRP(targets) && SCHEME_STX_NULLP(SCHEME_STX_CDR(targets))) {
    var = SCHEME_STX_CAR(variables);
    rec[drec].value_name = SCHEME_STX_SYM(var);
  }

  env = scheme_no_defines(env);

  val = scheme_compile_expr(val, env, rec, drec);

  vec = scheme_make_vector(2, NULL);
  SCHEME_VEC_ELS(vec)[0] = targets;
  SCHEME_VEC_ELS(vec)[1] = val;
  vec->type = scheme_define_values_type;

  if (SCHEME_TRUEP(scheme_stx_property(form, compiler_inline_hint_symbol, NULL))) {
    /* use "immutable" bit to mark compiler-inline hint: */
    SCHEME_SET_IMMUTABLE(vec);
  }

  return vec;
}

static Scheme_Object *
define_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  Scheme_Object *var, *val, *fn, *boundname;

  SCHEME_EXPAND_OBSERVE_PRIM_DEFINE_VALUES(erec[drec].observer);

  scheme_define_parse(form, &var, &val, 0, env, 0);

  env = scheme_no_defines(env);

  if (SCHEME_STX_PAIRP(var) && SCHEME_STX_NULLP(SCHEME_STX_CDR(var)))
    boundname = SCHEME_STX_CAR(var);
  else
    boundname = scheme_false;
  erec[drec].value_name = boundname;

  fn = SCHEME_STX_CAR(form);
  return scheme_datum_to_syntax(cons(fn,
				      cons(var,
					    cons(scheme_expand_expr(val, env, erec, drec), 
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
    scheme_wrong_syntax(NULL, NULL, form, "wrong number of parts");

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

  SCHEME_EXPAND_OBSERVE_PRIM_QUOTE(erec[drec].observer);

  rest = SCHEME_STX_CDR(form);

  if (!(SCHEME_STX_PAIRP(rest) && SCHEME_STX_NULLP(SCHEME_STX_CDR(rest))))
    scheme_wrong_syntax(NULL, NULL, form, "wrong number of parts");

  return form;
}

/**********************************************************************/
/*                                if                                  */
/**********************************************************************/

static void check_if_len(Scheme_Object *form, int len)
{
  if (len != 4) {
    if (len == 3) {
      scheme_wrong_syntax(NULL, NULL, form, 
                          "missing an \"else\" expression");
    } else {
      bad_form(form, len);
    }
  }
}

Scheme_Object *
scheme_make_branch(Scheme_Object *test, Scheme_Object *thenp,
		   Scheme_Object *elsep)
{
  Scheme_Branch_Rec *b;

  if (SCHEME_TYPE(test) > _scheme_compiled_values_types_) {
    if (SCHEME_FALSEP(test))
      return elsep;
    else
      return thenp;
  }

  b = MALLOC_ONE_TAGGED(Scheme_Branch_Rec);
  b->so.type = scheme_branch_type;

  b->test = test;
  b->tbranch = thenp;
  b->fbranch = elsep;

  return (Scheme_Object *)b;
}

static Scheme_Object *
if_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  int len, opt;
  Scheme_Object *test, *thenp, *elsep, *name, *rest;
  Scheme_Compile_Info recs[3];

  form = scheme_stx_taint_disarm(form, NULL);

  len = check_form(form, form);
  check_if_len(form, len);

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
if_expand(Scheme_Object *orig_form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  Scheme_Object *form, *test, *rest, *thenp, *elsep, *fn, *boundname;
  int len;
  Scheme_Expand_Info recs[3];

  SCHEME_EXPAND_OBSERVE_PRIM_IF(erec[drec].observer);

  form = scheme_stx_taint_disarm(orig_form, NULL);

  len = check_form(form, form);

  check_if_len(form, len);

  if (len == 3) {
    SCHEME_EXPAND_OBSERVE_NEXT_GROUP(erec[drec].observer);
  }

  env = scheme_no_defines(env);

  boundname = scheme_check_name_property(form, erec[drec].value_name);

  scheme_init_expand_recs(erec, drec, recs, 3);
  recs[0].value_name = scheme_false;
  recs[1].value_name = boundname;
  recs[2].value_name = boundname;

  rest = SCHEME_STX_CDR(form);
  test = SCHEME_STX_CAR(rest);
  test = scheme_expand_expr(test, env, recs, 0);

  SCHEME_EXPAND_OBSERVE_NEXT(erec[drec].observer);
  rest = SCHEME_STX_CDR(rest);
  thenp = SCHEME_STX_CAR(rest);
  thenp = scheme_expand_expr(thenp, env, recs, 1);

  rest = SCHEME_STX_CDR(rest);
  if (!SCHEME_STX_NULLP(rest)) {
    SCHEME_EXPAND_OBSERVE_NEXT(erec[drec].observer);
    elsep = SCHEME_STX_CAR(rest);
    elsep = scheme_expand_expr(elsep, env, recs, 2);
    rest = cons(elsep, scheme_null);
  } else {
    rest = scheme_null;
  }

  rest = cons(thenp, rest);

  fn = SCHEME_STX_CAR(form);
  return scheme_datum_to_syntax(cons(fn, cons(test, rest)),
				orig_form, orig_form, 
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

  form = scheme_stx_taint_disarm(form, NULL);

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
with_cont_mark_expand(Scheme_Object *orig_form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  Scheme_Object *key, *val, *expr, *form, *fn, *boundname;
  int len;
  Scheme_Expand_Info recs[3];

  SCHEME_EXPAND_OBSERVE_PRIM_WCM(erec[drec].observer);

  form = scheme_stx_taint_disarm(orig_form, NULL);

  len = check_form(form, form);
  if (len != 4)
    bad_form(form, len);

  fn = SCHEME_STX_CAR(form);

  env = scheme_no_defines(env);

  boundname = scheme_check_name_property(form, erec[drec].value_name);

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
  SCHEME_EXPAND_OBSERVE_NEXT(erec[drec].observer);
  val = scheme_expand_expr(val, env, recs, 1);
  SCHEME_EXPAND_OBSERVE_NEXT(erec[drec].observer);
  expr = scheme_expand_expr(expr, env, recs, 2);

  return scheme_datum_to_syntax(cons(fn,
				      cons(key,
					    cons(val,
						  cons(expr, scheme_null)))),
				orig_form,
				orig_form, 
				0, 2);
}

/**********************************************************************/
/*                               set!                                 */
/**********************************************************************/

static Scheme_Object *
set_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Set_Bang *sb;
  Scheme_Env *menv = NULL;
  Scheme_Object *var, *val, *name, *body, *rest, *find_name;
  int l, set_undef;

  form = scheme_stx_taint_disarm(form, NULL);

  l = check_form(form, form);
  if (l != 3)
    bad_form(form, l);

  rest = SCHEME_STX_CDR(form);
  name = SCHEME_STX_CAR(rest);
  rest = SCHEME_STX_CDR(rest);
  body = SCHEME_STX_CAR(rest);
  
  scheme_check_identifier("set!", name, NULL, env, form);

  find_name = name;

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
				env->in_modidx, 
				&menv, NULL, NULL, NULL);
    
    if (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)) {
      /* Redirect to a macro? */
      if (scheme_is_set_transformer(SCHEME_PTR_VAL(var))) {
	form = scheme_apply_macro(name, menv, SCHEME_PTR_VAL(var), form, env, scheme_false, rec, drec, 1);
	
	return scheme_compile_expr(form, env, rec, drec);
      } else if (scheme_is_rename_transformer(SCHEME_PTR_VAL(var))) {
	find_name = scheme_rename_transformer_id(SCHEME_PTR_VAL(var));
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
    var = scheme_register_toplevel_in_prefix(var, env, rec, drec, 0, NULL);
    if (env->genv->module)
      SCHEME_TOPLEVEL_FLAGS(var) |= SCHEME_TOPLEVEL_MUTATED;
    env->prefix->non_phaseless = 1;
  }

  scheme_compile_rec_done_local(rec, drec);
  rec[drec].value_name = SCHEME_STX_SYM(name);

  val = scheme_compile_expr(body, scheme_no_defines(env), rec, drec);
  
  set_undef = (rec[drec].comp_flags & COMP_ALLOW_SET_UNDEFINED);
 
  sb = MALLOC_ONE_TAGGED(Scheme_Set_Bang);  
  sb->so.type = scheme_set_bang_type;
  sb->var = var;
  sb->val = val;
  sb->set_undef = set_undef;

  return (Scheme_Object *)sb;
}

static Scheme_Object *
set_expand(Scheme_Object *orig_form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  Scheme_Env *menv = NULL;
  Scheme_Object *name, *var, *fn, *rhs, *find_name, *lexical_binding_id, *form;
  int l;

  SCHEME_EXPAND_OBSERVE_PRIM_SET(erec[drec].observer);

  form = scheme_stx_taint_disarm(orig_form, NULL);

  l = check_form(form, form);
  if (l != 3)
    bad_form(form, l);

  env = scheme_no_defines(env);

  name = SCHEME_STX_CDR(form);
  name = SCHEME_STX_CAR(name);

  scheme_check_identifier("set!", name, NULL, env, form);

  find_name = name;

  while (1) {
    /* Make sure it's mutable, and check for redirects: */
    lexical_binding_id = NULL;
    var = scheme_lookup_binding(find_name, env, SCHEME_SETTING, 
				env->in_modidx, 
				&menv, NULL, &lexical_binding_id, NULL);

    SCHEME_EXPAND_OBSERVE_RESOLVE(erec[drec].observer, find_name);

    if ((erec[drec].depth != 0) && SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)) {
      /* Redirect to a macro? */
      if (scheme_is_set_transformer(SCHEME_PTR_VAL(var))) {

	SCHEME_EXPAND_OBSERVE_ENTER_MACRO(erec[drec].observer, form);

	form = scheme_apply_macro(name, menv, SCHEME_PTR_VAL(var), form, env, scheme_false, erec, drec, 1);

	SCHEME_EXPAND_OBSERVE_EXIT_MACRO(erec[drec].observer, form);

	if (erec[drec].depth > 0)
	  erec[drec].depth--;

	erec[drec].value_name = name;

	return scheme_expand_expr(form, env, erec, drec);
      } else if (scheme_is_rename_transformer(SCHEME_PTR_VAL(var))) {
	Scheme_Object *new_name;
	new_name = scheme_rename_transformer_id(SCHEME_PTR_VAL(var));
	new_name = scheme_stx_track(new_name, find_name, find_name);
	find_name = new_name;
	menv = NULL;
      } else
        break;
    } else {
      if (lexical_binding_id) {
        find_name = lexical_binding_id;
      }
      break;
    }
  }

  if (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)
      || SAME_TYPE(SCHEME_TYPE(var), scheme_syntax_compiler_type)) {
    scheme_wrong_syntax(NULL, name, form, "cannot mutate syntax identifier");
  }

  SCHEME_EXPAND_OBSERVE_NEXT(erec[drec].observer);


  fn = SCHEME_STX_CAR(form);
  rhs = SCHEME_STX_CDR(form);
  rhs = SCHEME_STX_CDR(rhs);
  rhs = SCHEME_STX_CAR(rhs);

  erec[drec].value_name = name;

  rhs = scheme_expand_expr(rhs, env, erec, drec);

  return scheme_datum_to_syntax(cons(fn,
				      cons(find_name,
					    cons(rhs, scheme_null))),
				orig_form,
				orig_form, 
				0, 2);
}

/**********************************************************************/
/*                     #%variable-reference                           */
/**********************************************************************/

static Scheme_Object *
ref_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Env *menv = NULL;
  Scheme_Object *var, *name, *rest, *dummy, *lex_id = NULL;
  int l, ok;

  if (rec[drec].comp)
    env->prefix->non_phaseless = 1;

  form = scheme_stx_taint_disarm(form, NULL);

  l = check_form(form, form);

  /* retaining `dummy' ensures that the environment stays
     linked from the actual variable */
  if (rec[drec].comp && ((l == 1) || !rec[drec].testing_constantness))
    dummy = scheme_make_environment_dummy(env);
  else
    dummy = NULL;

  if (l == 1) {
    if (rec[drec].comp)
      var = dummy;
    else
      var = scheme_void;
  } else {
    if (l != 2)
      bad_form(form, l);

    rest = SCHEME_STX_CDR(form);
    name = SCHEME_STX_CAR(rest);
    name = scheme_stx_taint_disarm(name, NULL);

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
      /* FIXME: when using #%top, need to set mutated flag */
      if (rec[drec].comp)
        var = scheme_compile_expr(name, env, rec, drec);
      else
        var = scheme_expand_expr(name, env, rec, drec);
    } else {
      lex_id = NULL;
      var = scheme_lookup_binding(name, env, 
                                  SCHEME_REFERENCING 
                                  + SCHEME_GLOB_ALWAYS_REFERENCE
                                  + (rec[drec].dont_mark_local_use 
                                     ? SCHEME_DONT_MARK_USE 
                                     : 0)
                                  + (rec[drec].resolve_module_ids
                                     ? SCHEME_RESOLVE_MODIDS
                                     : 0),
                                  env->in_modidx, 
                                  &menv, NULL, &lex_id, NULL);

      if (SAME_TYPE(SCHEME_TYPE(var), scheme_variable_type)
          || SAME_TYPE(SCHEME_TYPE(var), scheme_module_variable_type)) {
        int imported = 0;
        imported = scheme_is_imported(var, env);

        if (rec[drec].comp) {
          var = scheme_register_toplevel_in_prefix(var, env, rec, drec, imported, NULL);
          if (!imported && env->genv->module && !rec[drec].testing_constantness)
            SCHEME_TOPLEVEL_FLAGS(var) |= SCHEME_TOPLEVEL_MUTATED;
        }
      } else if (SAME_TYPE(SCHEME_TYPE(var), scheme_local_type)) {
        /* ok */
      } else {
        scheme_wrong_syntax(NULL, name, form, "identifier does not refer to a variable");
      }

      if (rec[drec].comp)
        scheme_compile_rec_done_local(rec, drec);
    }
  }

  if (rec[drec].comp) {
    Scheme_Object *o;
    o = scheme_alloc_object();
    o->type = scheme_varref_form_type;
    SCHEME_PTR1_VAL(o) = (Scheme_Object *)var;
    if (!dummy) dummy = scheme_false;
    SCHEME_PTR2_VAL(o) = (Scheme_Object *)dummy;
    return o;
  } else {
    if (lex_id) {
      form = SCHEME_STX_CAR(form);
      return scheme_make_pair(form, scheme_make_pair(lex_id, scheme_null));
    }
    return NULL;
  }
}

static Scheme_Object *
ref_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  Scheme_Object *naya;

  SCHEME_EXPAND_OBSERVE_PRIM_VARREF(erec[drec].observer);

  /* Error checking, and lexical variable update: */
  naya = ref_syntax(form, env, erec, drec);

  if (!naya)
    /* No change: */
    return form;

  return scheme_datum_to_syntax(naya, form, form, 0, 2);
}

/**********************************************************************/
/*                             case-lambda                            */
/**********************************************************************/

Scheme_Object *scheme_unclose_case_lambda(Scheme_Object *expr, int mode)
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
						     + ((cl->count - mzFLEX_DELTA) * sizeof(Scheme_Object*)));
    
    cl2->so.type = scheme_case_lambda_sequence_type;
    cl2->count = cl->count;
    cl2->name = cl->name;

    for (i = cl->count; i--; ) {
      c = (Scheme_Closure *)cl->array[i];
      cl2->array[i] = (Scheme_Object *)c->code;
    }

    if (mode == 2) {
      /* sfs */
      return (Scheme_Object *)cl2;
#ifdef MZ_USE_JIT
    } else if (mode == 1) {
      /* JIT */
      return scheme_case_lambda_jit((Scheme_Object *)cl2);
#endif
    } else
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
    scheme_wrong_syntax(NULL, line, form, "%s",
			SCHEME_STX_NULLP(body) ? "empty body not allowed" : IMPROPER_LIST_FORM);
}

static Scheme_Object *
case_lambda_syntax (Scheme_Object *form, Scheme_Comp_Env *env, 
		    Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *list, *last, *c, *orig_form = form, *name;
  Scheme_Case_Lambda *cl;
  int i, count = 0;
  Scheme_Compile_Info *recs;

  form = scheme_stx_taint_disarm(form, NULL);
  
  form = SCHEME_STX_CDR(form);

  name = scheme_build_closure_name(orig_form, rec, drec);
  
  if (SCHEME_STX_NULLP(form)) {
    /* Case where there are no cases... */
    form = (Scheme_Object *)scheme_malloc_tagged(sizeof(Scheme_Case_Lambda)
						 - (mzFLEX_DELTA * sizeof(Scheme_Object*)));

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

    return form;
  }

  if (!SCHEME_STX_PAIRP(form))
    scheme_wrong_syntax(NULL, form, orig_form, NULL);
  if (SCHEME_STX_NULLP(SCHEME_STX_CDR(form))) {
    c = SCHEME_STX_CAR(form);

    case_lambda_check_line(c, orig_form, env);

    c = cons(scheme_datum_to_syntax(lambda_symbol, scheme_false, scheme_sys_wraps(env), 0, 0),
	      c);
    c = scheme_datum_to_syntax(c, orig_form, orig_form, 0, 2);
    
    return lambda_syntax(c, env, rec, drec);
  }

  scheme_compile_rec_done_local(rec, drec);

  list = last = NULL;
  while (SCHEME_STX_PAIRP(form)) {
    Scheme_Object *clause;
    clause = SCHEME_STX_CAR(form);
    case_lambda_check_line(clause, orig_form, env);

    c = cons(lambda_symbol, clause);

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
			 + (count - mzFLEX_DELTA) * sizeof(Scheme_Object *));
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

  return (Scheme_Object *)cl;
}

static Scheme_Object *
case_lambda_expand(Scheme_Object *orig_form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  Scheme_Object *first, *last, *args, *body, *c, *new_line, *form;

  SCHEME_EXPAND_OBSERVE_PRIM_CASE_LAMBDA(erec[drec].observer);

  form = scheme_stx_taint_disarm(orig_form, NULL);

  first = SCHEME_STX_CAR(form);
  first = cons(first, scheme_null);
  last = first;
  form = SCHEME_STX_CDR(form);

  while (SCHEME_STX_PAIRP(form)) {
    Scheme_Object *line_form;
    Scheme_Comp_Env *newenv;
    
    SCHEME_EXPAND_OBSERVE_NEXT(erec[drec].observer);

    line_form = SCHEME_STX_CAR(form);

    case_lambda_check_line(line_form, orig_form, env);
    
    body = SCHEME_STX_CDR(line_form);
    args = SCHEME_STX_CAR(line_form);

    body = scheme_datum_to_syntax(body, line_form, line_form, 0, 0);
    
    newenv = scheme_add_compilation_frame(args, env, 0);
    
    body = scheme_add_env_renames(body, newenv, env);
    args = scheme_add_env_renames(args, newenv, env);
    SCHEME_EXPAND_OBSERVE_CASE_LAMBDA_RENAMES(erec[drec].observer, args, body);

    {
      Scheme_Expand_Info erec1;
      scheme_init_expand_recs(erec, drec, &erec1, 1);
      erec1.value_name = scheme_false;
      new_line = cons(args, expand_block(body, newenv, &erec1, 0));
    }
    new_line = scheme_datum_to_syntax(new_line, line_form, line_form, 0, 1);

    c = cons(new_line, scheme_null);

    SCHEME_CDR(last) = c;
    last = c;

    form = SCHEME_STX_CDR(form);
  }

  if (!SCHEME_STX_NULLP(form))
    scheme_wrong_syntax(NULL, form, orig_form, NULL);
  
  return scheme_datum_to_syntax(first, orig_form, orig_form, 0, 2);
}

/**********************************************************************/
/*                  let, let-values, letrec, etc.                     */
/**********************************************************************/

static Scheme_Let_Header *make_header(Scheme_Object *first, int num_bindings, int num_clauses, 
                                      int flags)
{
  Scheme_Let_Header *head;

  head = MALLOC_ONE_TAGGED(Scheme_Let_Header);
  head->iso.so.type = scheme_compiled_let_void_type;
  head->body = first;
  head->count = num_bindings;
  head->num_clauses = num_clauses;
  SCHEME_LET_FLAGS(head) = flags;

  return head;
}

static Scheme_Object *shift_compiled_expression(Scheme_Object *v, int delta, int skip);

static Scheme_Object *shift_compiled_expression_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *v = (Scheme_Object *)p->ku.k.p1;

  p->ku.k.p1 = NULL;

  return (void *)shift_compiled_expression(v, p->ku.k.i1, p->ku.k.i2);
}

static Scheme_Object *shift_compiled_expression(Scheme_Object *v, int delta, int skip)
{
  if (!delta || (SCHEME_TYPE(v) > _scheme_compiled_values_types_))
    return v;

  if (delta < 0) scheme_signal_error("internal error: bad shift delta");

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;

      p->ku.k.p1 = (void *)v;
      p->ku.k.i1 = delta;
      p->ku.k.i2 = skip;

      return scheme_handle_stack_overflow(shift_compiled_expression_k);
    }
  }
#endif

  /* Perform simple shifts directly. We want to avoid adding
     extra `let' ayers if possible, since it might interefere
     with optimizations. */

  switch (SCHEME_TYPE(v)) {
  case scheme_compiled_toplevel_type:
  case scheme_compiled_quote_syntax_type:
    return v;
  case scheme_local_type:
    {
      int pos = SCHEME_LOCAL_POS(v);
      if (pos < skip)
        return v;
      else
        return scheme_make_local(scheme_local_type, pos - delta, 0);
    }
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)v;
      int i;

      for (i = app->num_args + 1; i--; ) {
        v = shift_compiled_expression(app->args[i], delta, skip);
        app->args[i] = v;
      }

      return (Scheme_Object *)app;
    }
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)v;

      v = shift_compiled_expression(app->rator, delta, skip);
      app->rator = v;
      v = shift_compiled_expression(app->rand, delta, skip);
      app->rand = v;

      return (Scheme_Object *)app;
    }
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)v;

      v = shift_compiled_expression(app->rator, delta, skip);
      app->rator = v;
      v = shift_compiled_expression(app->rand1, delta, skip);
      app->rand1 = v;
      v = shift_compiled_expression(app->rand2, delta, skip);
      app->rand2 = v;

      return (Scheme_Object *)app;
    }
  case scheme_branch_type:
    {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)v;
      
      v = shift_compiled_expression(b->test, delta, skip);
      b->test = v;
      v = shift_compiled_expression(b->tbranch, delta, skip);
      b->tbranch = v;
      v = shift_compiled_expression(b->fbranch, delta, skip);
      b->fbranch = v;

      return (Scheme_Object *)b;
    }        
  case scheme_with_cont_mark_type:
    {
      Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)v;
      
      v = shift_compiled_expression(wcm->key, delta, skip);
      wcm->key = v;
      v = shift_compiled_expression(wcm->val, delta, skip);
      wcm->val = v;
      v = shift_compiled_expression(wcm->body, delta, skip);
      wcm->body = v;

      return (Scheme_Object *)wcm;
    }
  case scheme_sequence_type:
  case scheme_begin0_sequence_type:
    {
      Scheme_Sequence *s = (Scheme_Sequence *)v;
      int i;

      for (i = s->count; i--; ) {
        v = shift_compiled_expression(s->array[i], delta, skip);
        s->array[i] = v;
      }
  
      return (Scheme_Object *)s;
    }
  case scheme_apply_values_type:
    {
      Scheme_Object *v2;

      v2 = shift_compiled_expression(SCHEME_PTR1_VAL(v), delta, skip);
      SCHEME_PTR1_VAL(v) = v2;
      v2 = shift_compiled_expression(SCHEME_PTR2_VAL(v), delta, skip);
      SCHEME_PTR2_VAL(v) = v2;

      return v;
    }
  case scheme_set_bang_type:
    {
      Scheme_Set_Bang *sb = (Scheme_Set_Bang *)v;

      v = shift_compiled_expression(sb->var, delta, skip);
      sb->var = v;
      v = shift_compiled_expression(sb->val, delta, skip);
      sb->val = v;

      return (Scheme_Object *)sb;
    }
  case scheme_compiled_unclosed_procedure_type:
    {
      Scheme_Closure_Data *data = (Scheme_Closure_Data *)v;

      v = shift_compiled_expression(data->code, delta, skip + data->num_params);
      data->code = v;

      return (Scheme_Object *)data;
    }
  case scheme_case_lambda_sequence_type:
    {
      Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)v;
      int i;

      for (i = cl->count; i--; ) {
        v = shift_compiled_expression(cl->array[i], delta, skip);
        cl->array[i] = v;
      }

      return (Scheme_Object *)cl;
    }
  case scheme_compiled_let_void_type:
    {
      Scheme_Let_Header *lh = (Scheme_Let_Header *)v;
      Scheme_Compiled_Let_Value *clv;
      int post_bind = !(SCHEME_LET_FLAGS(lh) & (SCHEME_LET_RECURSIVE | SCHEME_LET_STAR));
      int i;

      if (!post_bind) skip += lh->count;

      clv = (Scheme_Compiled_Let_Value *)lh->body;
      i = lh->num_clauses;
      while (1) {
        v = shift_compiled_expression(clv->value, delta, skip);
        clv->value = v;
        if (--i)
          clv = (Scheme_Compiled_Let_Value *)clv->body;
        else
          break;
      }

      if (post_bind) skip += lh->count;

      if (!lh->num_clauses) {
        v = shift_compiled_expression(lh->body, delta, skip);
        lh->body = v;
      } else {
        v = shift_compiled_expression(clv->body, delta, skip);
        clv->body = v;
      }

      return (Scheme_Object *)lh;
    }
  case scheme_varref_form_type:
    {
      Scheme_Object *sv;

      sv = shift_compiled_expression(SCHEME_PTR1_VAL(v), delta, skip);
      SCHEME_PTR1_VAL(v) = sv;
      
      sv = shift_compiled_expression(SCHEME_PTR2_VAL(v), delta, skip);
      SCHEME_PTR2_VAL(v) = sv;
      
      return v;
    }
  default:
    scheme_signal_error("internal error: compile-time shift failed: %d", SCHEME_TYPE(v));
    return NULL;
  }
}

static Scheme_Object *force_traditional_letrec(Scheme_Object *result, Scheme_Comp_Env *env)
{
  /* Force `letrec'-style binding by adding a forward
     reference to the last binding as a first binding:
     (letrec-values+syntaxes ([() (if #f <last-id> (#%app values))] ....) ....).
     To avoid affecting performance, this hack is reverted in
     the `letrec' compiler and expander. */
  Scheme_Object *sbh, *vbh, *vb, *v, *last_name = NULL, *values, *app;

  sbh = SCHEME_STX_CDR(result);
  vbh = SCHEME_STX_CDR(sbh);
  vb = SCHEME_STX_CAR(vbh);

  while (!SCHEME_STX_NULLP(vb)) {
    v = SCHEME_STX_CAR(vb);
    v = SCHEME_STX_CAR(v);
    if (!SCHEME_STX_NULLP(v)) {
      last_name = SCHEME_STX_CAR(v);
    }
    vb = SCHEME_STX_CDR(vb);
  }

  if (last_name) {
    vb = SCHEME_STX_CAR(vbh);
    v = scheme_datum_to_syntax(scheme_intern_symbol("if"), scheme_false, 
                               scheme_sys_wraps(env), 0, 0);
    app = scheme_datum_to_syntax(app_symbol, scheme_false, 
                                 scheme_sys_wraps(env), 0, 0);
    values = scheme_datum_to_syntax(values_symbol, scheme_false, 
                                    scheme_sys_wraps(env), 0, 0);
    vb = icons(icons(scheme_null,
                     icons(icons(v,
                                 icons(scheme_false,
                                       icons(last_name, 
                                             icons(icons(app, icons(values, scheme_null)),
                                                   scheme_null)))),
                           scheme_null)),
               vb);
    vbh = SCHEME_STX_CDR(vbh);
    sbh = SCHEME_STX_CAR(sbh);
    v = SCHEME_STX_CAR(result);
    v = icons(v, icons(sbh, icons(vb, vbh)));
    result = scheme_datum_to_syntax(v, result, result, 0, 2);
  }

  return result;
}

static Scheme_Object *detect_traditional_letrec(Scheme_Object *form, Scheme_Comp_Env *env)
/* See force_traditional_letrec() */
{
  Scheme_Object *v, *v2, *v3, *id;

  v = SCHEME_STX_CDR(form);
  v = SCHEME_STX_CAR(v);
  if (SCHEME_STX_NULLP(v)) return form;

  v = SCHEME_STX_CAR(v);
  /* is v `[() ...]' ? */
  v2 = SCHEME_STX_CAR(v);
  if (!SCHEME_STX_NULLP(v2)) return form;

  v2 = SCHEME_STX_CDR(v);
  v2 = SCHEME_STX_CAR(v2);
  
  /* is v2 `(if #f ... (values))' ? */
  if (!SCHEME_STX_PAIRP(v2)) return form;
  v = SCHEME_STX_CDR(v2);
  if (!SCHEME_STX_PAIRP(v)) return form;
  v = SCHEME_STX_CAR(v);
  v = SCHEME_STX_VAL(v);

  if (!SCHEME_FALSEP(v)) {
    /* try '#f: */
    if (!SCHEME_PAIRP(v)) return form;
    v3 = SCHEME_CDR(v);
    if (!SCHEME_STX_PAIRP(v3)) return form;
    v3 = SCHEME_STX_CAR(v3);
    v3 = SCHEME_STX_VAL(v3);
    if (!SCHEME_FALSEP(v3)) return form;

    v3 = SCHEME_CDR(v);
    v3 = SCHEME_STX_CDR(v3);
    if (!SCHEME_STX_NULLP(v3)) return form;
  }

  /* found #f; look for `if' and `(#%app values)': */
  v = SCHEME_STX_CAR(v2);
  if (!SCHEME_STX_SYMBOLP(v)) return form;
  
  id = scheme_datum_to_syntax(scheme_intern_symbol("if"), scheme_false, 
                              scheme_sys_wraps(env), 0, 0);
  if (!scheme_stx_module_eq(v, id, env->genv->phase)) return form;

  /* found `if'; look for `(#%app values)' */
  v = SCHEME_STX_CDR(v2);
  v = SCHEME_STX_CDR(v);
  if (!SCHEME_STX_PAIRP(v)) return form;
  v = SCHEME_STX_CDR(v);
  if (!SCHEME_STX_PAIRP(v)) return form;
  v2 = SCHEME_STX_CDR(v);
  if (!SCHEME_STX_NULLP(v2)) return form;

  v = SCHEME_STX_CAR(v);
  if (!SCHEME_STX_PAIRP(v)) return form;
  v2 = SCHEME_STX_CAR(v);
  if (!SCHEME_STX_SYMBOLP(v2)) return form;
  id = scheme_datum_to_syntax(app_symbol, scheme_false, 
                              scheme_sys_wraps(env), 0, 0);
  if (!scheme_stx_module_eq(v2, id, env->genv->phase)) return form;

  v = SCHEME_STX_CDR(v);
  if (!SCHEME_STX_PAIRP(v)) return form;
  v2 = SCHEME_STX_CDR(v);
  if (!SCHEME_STX_NULLP(v2)) return form;

  v = SCHEME_STX_CAR(v);
  if (!SCHEME_STX_SYMBOLP(v)) return form;
  id = scheme_datum_to_syntax(values_symbol, scheme_false, 
                              scheme_sys_wraps(env), 0, 0);
  if (!scheme_stx_module_eq(v, id, env->genv->phase)) return form;
  
  /* pattern matched; drop the first clause */
  v = SCHEME_STX_CDR(form);
  v2 = SCHEME_STX_CAR(v);
  v2 = SCHEME_STX_CDR(v2);

  v = SCHEME_STX_CDR(v);
  v = scheme_datum_to_syntax(v, scheme_false, scheme_false, 0, 0);
  v2 = icons(v2, v);
  
  v = SCHEME_STX_CAR(form);
  v2 = icons(v, v2);

  return scheme_datum_to_syntax(v2, form, form, 0, 2);
}

static Scheme_Object *
gen_let_syntax (Scheme_Object *form, Scheme_Comp_Env *origenv, char *formname,
		int star, int recursive, int multi, Scheme_Compile_Info *rec, int drec,
		Scheme_Comp_Env *frame_already)
{
  Scheme_Object *bindings, *l, *binding, *name, **names, **clv_names, *forms, *defname;
  int num_clauses, num_bindings, i, j, k, m, pre_k;
  Scheme_Comp_Env *frame, *env, *rhs_env;
  Scheme_Compile_Info *recs;
  Scheme_Object *first = NULL;
  Scheme_Compiled_Let_Value *last = NULL, *lv;
  DupCheckRecord r;
  int rec_env_already = rec[drec].env_already;
  int rev_bind_order,  post_bind;
  Scheme_Let_Header *head;
    
  form = scheme_stx_taint_disarm(form, NULL);

  if (rec_env_already == 2) {
    l = detect_traditional_letrec(form, origenv);
    if (!SAME_OBJ(l, form)) {
      rec_env_already = 1;
      form = l;
    }
  }

  i = scheme_stx_proper_list_length(form);
  if (i < 3)
    scheme_wrong_syntax(NULL, NULL, form, (!i ? "empty body not allowed" : NULL));

  bindings = SCHEME_STX_CDR(form);
  bindings = SCHEME_STX_CAR(bindings);
  num_clauses = scheme_stx_proper_list_length(bindings);

  if (num_clauses < 0)
    scheme_wrong_syntax(NULL, bindings, form, NULL);

  if (num_clauses < 2) star = 0;

  post_bind = !recursive && !star;
  rev_bind_order = recursive;

  /* forms ends up being the let body */
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
  else {
    frame = scheme_new_compilation_frame(num_bindings, 
                                         (rec_env_already ? SCHEME_INTDEF_SHADOW : 0),
                                         origenv);
    if (rec_env_already)
      frame_already = frame;
  }
  env = frame;
  if (post_bind)
    rhs_env = scheme_no_defines(origenv);
  else
    rhs_env = env;

  recs = MALLOC_N_RT(Scheme_Compile_Info, (num_clauses + 1));

  defname = rec[drec].value_name;
  scheme_compile_rec_done_local(rec, drec);
  scheme_init_compile_recs(rec, drec, recs, num_clauses + 1);

  defname = scheme_check_name_property(form, defname);
  
  if (!star && !frame_already) {
    scheme_begin_dup_symbol_check(&r, env);
  }

  /* For `letrec', we bind the first set of identifiers at the deepest
     position. That order makes it easier to peel off a prefix into a
     separate `letrec'. For `let' and `let*', the first set of
     identifiers is at the shallowest position. */

  if (rev_bind_order)
    k = num_bindings;
  else
    k = 0;

  for (i = 0; i < num_clauses; i++) {
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
    
    if (rev_bind_order) {
      if (multi) {
        name = SCHEME_STX_CAR(binding);
        while (!SCHEME_STX_NULLP(name)) {
          name = SCHEME_STX_CDR(name);
          k--;
        }
      } else
        k--;
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
	  if (scheme_stx_bound_eq(names[m], names[j], scheme_make_integer(env->genv->phase)))
	    scheme_wrong_syntax(NULL, NULL, form,
				"multiple bindings of `%S' in the same clause", 
				SCHEME_STX_SYM(names[m]));
	}
      }
    } else {
      scheme_check_identifier(NULL, name, NULL, env, form);
      names[k++] = name;
    }
    
    if (!star && !frame_already) {
      for (m = pre_k; m < k; m++) {
	scheme_dup_symbol_check(&r, NULL, names[m], "binding", form);
      }
    }

    lv = MALLOC_ONE_TAGGED(Scheme_Compiled_Let_Value);
    lv->iso.so.type = scheme_compiled_let_value_type;
    if (!last)
      first = (Scheme_Object *)lv;
    else
      last->body = (Scheme_Object *)lv;
    last = lv;
    lv->count = (k - pre_k);
    lv->position = pre_k;

    if (recursive) {
      /* The names are only used for recursive bindings (in letrec_check),
         currently. It would be ok if we record extra names, though. */
      clv_names = MALLOC_N(Scheme_Object*, lv->count);
      for (m = pre_k; m < k; m++) {
        Scheme_Object *ln;
        ln = get_local_name(names[m]);
        clv_names[m - pre_k] = ln;
      }
      lv->names = clv_names;
    }

    if (lv->count == 1)
      recs[i].value_name = SCHEME_STX_SYM(names[pre_k]);

    if (!recursive) {
      Scheme_Object *ce, *rhs;
      rhs = SCHEME_STX_CDR(binding);
      rhs = SCHEME_STX_CAR(rhs);
      rhs = scheme_add_env_renames(rhs, env, origenv);
      ce = scheme_compile_expr(rhs, rhs_env, recs, i);
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

    if (rev_bind_order)
      k = pre_k;
  }
  
  if (!star && !recursive) {
    for (i = 0; i < num_bindings; i++) {
      scheme_add_compilation_binding(i, names[i], frame);
    }
  }

  head = make_header(first, num_bindings, num_clauses,
                     ((recursive ? SCHEME_LET_RECURSIVE : 0)
                      | (star ? SCHEME_LET_STAR : 0)));

  if (recursive) {
    Scheme_Let_Header *current_head = head;
    int prev_might_invoke = 0;
    int group_clauses = 0, group_count = 0;

    lv = (Scheme_Compiled_Let_Value *)first;
    for (i = 0; i < num_clauses; i++, lv = (Scheme_Compiled_Let_Value *)lv->body) {
      Scheme_Object *ce, *rhs;
      rhs = lv->value;
      rhs = scheme_add_env_renames(rhs, env, origenv);
      ce = scheme_compile_expr(rhs, env, recs, i);
      lv->value = ce;
      
      /* Record the fact that this binding doesn't use any or later
         bindings in the same set. In internal-definition mode,
         break bindings into smaller sets based on this
         information; otherwise, the `let' optimizer and resolver
         may do so, but we have to be more conservative as reflected
         by scheme_might_invoke_call_cc(). */
      if ((rec_env_already == 2) /* int def: semantics is `let' */
          || (!prev_might_invoke
              && !scheme_might_invoke_call_cc(ce))) {
        if (!scheme_env_check_reset_any_use(env))
          SCHEME_CLV_FLAGS(lv) |= SCHEME_CLV_NO_GROUP_USES;
        if ((rec_env_already == 2)
            && !group_clauses
            && !scheme_env_min_use_below(env, lv->position + lv->count)) {
          /* A clause that should be in its own `let' */
          Scheme_Let_Header *next_head;
          next_head = make_header(lv->body, 
                                  current_head->count - lv->count,
                                  current_head->num_clauses - 1,
                                  SCHEME_LET_RECURSIVE);
          current_head->num_clauses = 1;
          current_head->count = lv->count;
          current_head->body = (Scheme_Object *)next_head;
          SCHEME_LET_FLAGS(current_head) -= SCHEME_LET_RECURSIVE;
          current_head = next_head;
        } else if (!scheme_env_min_use_below(env, lv->position)) {
          /* End a recursive `letrec' group */
          SCHEME_CLV_FLAGS(lv) |= SCHEME_CLV_NO_GROUP_LATER_USES;
          
          if (rec_env_already == 2) {
            Scheme_Let_Header *next_head;
            group_clauses++;
            group_count += lv->count;
            next_head = make_header(lv->body, 
                                    current_head->count - group_count,
                                    current_head->num_clauses - group_clauses,
                                    SCHEME_LET_RECURSIVE);
            current_head->num_clauses = group_clauses;
            current_head->count = group_count;
            current_head->body = (Scheme_Object *)next_head;
            current_head = next_head;
          }
          group_clauses = 0;
          group_count = 0;
        } else {
          group_clauses++;
          group_count += lv->count;
        }
      } else
        prev_might_invoke = 1;
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

  if (rec_env_already == 2) {
    /* `head' is a chain of group headers; splice them into the lv
       chain, and adjust coordinates in each lv->value due to
       grouping */
    Scheme_Let_Header *current_head = head, *next_head = (Scheme_Let_Header *)head->body;
    Scheme_Object *rhs, *next = NULL;
    int num_group_clauses = 0;

    head->body = first;
    lv = (Scheme_Compiled_Let_Value *)first;
    for (i = 0; i < num_clauses; i++, lv = (Scheme_Compiled_Let_Value *)next) {
      rhs = shift_compiled_expression(lv->value,
                                      ((SCHEME_LET_FLAGS(current_head) & SCHEME_LET_RECURSIVE)
                                       ? num_bindings - current_head->count
                                       : num_bindings),
                                      0);
      lv->value = rhs;
      lv->position -= (num_bindings - current_head->count);
      next = lv->body;

      num_group_clauses++;
      if (current_head->num_clauses == num_group_clauses) {
        num_bindings -= current_head->count;
        current_head = next_head;
        next_head = (Scheme_Let_Header *)current_head->body;
        if ((i + 1) < num_clauses) {
          current_head->body = lv->body;
          lv->body = (Scheme_Object *)current_head;
        }
        num_group_clauses = 0;
      }
    }
  }

  scheme_merge_compile_recs(rec, drec, recs, num_clauses + 1);

  return (Scheme_Object *)head;
}

static Scheme_Object *
do_let_expand(Scheme_Object *orig_form, Scheme_Comp_Env *origenv, Scheme_Expand_Info *erec, int drec,
	      const char *formname, int letrec, int multi, int letstar,
	      Scheme_Comp_Env *env_already)
{
  Scheme_Object *vars, *body, *first, *last, *name, *v, *vs, *vlist, *boundname, *form, *pre_set;
  Scheme_Comp_Env *use_env, *env;
  Scheme_Expand_Info erec1;
  DupCheckRecord r;
  int rec_env_already = erec[drec].env_already, forward_ref_boundary;
  /* If env_already == 2, then it's not a true `letrec':
     it's from `letrec-values+syntax' and should be
     expanded into `let' plus `letrec'. */

  form = scheme_stx_taint_disarm(orig_form, NULL);

  if (rec_env_already == 2) {
    v = detect_traditional_letrec(form, origenv);
    if (!SAME_OBJ(v, form)) {
      rec_env_already = 1;
      form = v;
    }
  }

  vars = SCHEME_STX_CDR(form);

  if (!SCHEME_STX_PAIRP(vars))
    scheme_wrong_syntax(NULL, NULL, form, NULL);

  body = SCHEME_STX_CDR(vars);
  vars = SCHEME_STX_CAR(vars);

  if (!SCHEME_STX_PAIRP(body))
    scheme_wrong_syntax(NULL, NULL, form, (SCHEME_STX_NULLP(body) 
					   ? "empty body not allowed"
					   : NULL));

  boundname = scheme_check_name_property(form, erec[drec].value_name);
  erec[drec].value_name = boundname;

  if (letstar) {
    if (!SCHEME_STX_NULLP(vars)) {
      Scheme_Object *a, *vr;

      if (!SCHEME_STX_PAIRP(vars))
	scheme_wrong_syntax(NULL, vars, form, NULL);

      a = SCHEME_STX_CAR(vars);
      vr = SCHEME_STX_CDR(vars);
      
      first = let_values_symbol;
      first = scheme_datum_to_syntax(first, form, scheme_sys_wraps(origenv), 0, 0);
      
      if (SCHEME_STX_NULLP(vr)) {
	/* Don't create redundant empty let form */
      } else {
	last = let_star_values_symbol;
	last = scheme_datum_to_syntax(last, form, scheme_sys_wraps(origenv), 0, 0);
	body = cons(cons(last, cons(vr, body)),
		     scheme_null);
      }
      
      body = cons(first,
		   cons(cons(a, scheme_null),
			 body));
    } else {
      first = scheme_datum_to_syntax(let_values_symbol, form, scheme_sys_wraps(origenv), 0, 0);
      body = cons(first, cons(scheme_null, body));
    }
    
    body = scheme_datum_to_syntax(body, form, form, 0, -1);

    first = SCHEME_STX_CAR(form);
    body = scheme_stx_track(body, form, first);
    
    if (erec[drec].depth > 0)
      --erec[drec].depth;
    
    body = scheme_stx_taint_rearm(body, orig_form);

    if (!erec[drec].depth)
      return body;
    else {
      env = scheme_no_defines(origenv);
      return scheme_expand_expr(body, env, erec, drec);
    }
  }
  
  /* Note: no more letstar handling needed after this point */
  if (!env_already && !rec_env_already)
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
  
    {
      DupCheckRecord r2;
      Scheme_Object *names = name;
      if (!env_already && !rec_env_already)
        scheme_begin_dup_symbol_check(&r2, origenv);
      while (SCHEME_STX_PAIRP(names)) {
	name = SCHEME_STX_CAR(names);

	scheme_check_identifier(NULL, name, NULL, origenv, form);
	vlist = cons(name, vlist);

        if (!env_already && !rec_env_already) {
          scheme_dup_symbol_check(&r2, NULL, name, "clause binding", form);
          scheme_dup_symbol_check(&r, NULL, name, "binding", form);
        }
	
	names = SCHEME_STX_CDR(names);
      }
      if (!SCHEME_STX_NULLP(names))
	scheme_wrong_syntax(NULL, names, form, NULL);
    }

    vs = SCHEME_STX_CDR(vs);
  }

  if (!SCHEME_STX_NULLP(vs))
    scheme_wrong_syntax(NULL, vs, form, NULL);

  if (env_already)
    env = env_already;
  else
    env = scheme_add_compilation_frame(vlist, 
                                       origenv, 
                                       (rec_env_already ? SCHEME_INTDEF_SHADOW : 0));

  if (letrec)
    use_env = env;
  else
    use_env = scheme_no_defines(origenv);

  /* Pass 1: Rename */

  first = last = NULL;
  vs = vars;
  forward_ref_boundary = 0;
  while (SCHEME_STX_PAIRP(vars)) {
    Scheme_Object *rhs;

    v = SCHEME_STX_CAR(vars);

    /* Make sure names gets their own renames: */
    name = SCHEME_STX_CAR(v);
    name = scheme_add_env_renames(name, env, origenv);

    if (rec_env_already == 2)
      forward_ref_boundary += scheme_stx_proper_list_length(name);

    rhs = SCHEME_STX_CDR(v);
    rhs = SCHEME_STX_CAR(rhs);
    rhs = scheme_add_env_renames(rhs, use_env, origenv);
    
    v = scheme_datum_to_syntax(cons(name, cons(rhs, scheme_null)), v, v, 0, 1);
    v = cons(v, scheme_null);

    if (!first)
      first = v;
    else
      SCHEME_CDR(last) = v;

    last = v;
    vars = SCHEME_STX_CDR(vars);
  }
  if (!first) {
    first = scheme_null;
  }
  vars = first;

  body = scheme_datum_to_syntax(body, form, form, 0, 0);
  body = scheme_add_env_renames(body, env, origenv);
  SCHEME_EXPAND_OBSERVE_LET_RENAMES(erec[drec].observer, vars, body);

  /* Pass 2: Expand */

  first = last = NULL;
  pre_set = scheme_null;
  while (SCHEME_STX_PAIRP(vars)) {
    Scheme_Object *rhs, *rhs_name;

    SCHEME_EXPAND_OBSERVE_NEXT(erec[drec].observer);

    v = SCHEME_STX_CAR(vars);

    name = SCHEME_STX_CAR(v);
    rhs = SCHEME_STX_CDR(v);
    rhs = SCHEME_STX_CAR(rhs);

    if (SCHEME_STX_PAIRP(name) && SCHEME_STX_NULLP(SCHEME_STX_CDR(name))) {
      rhs_name = SCHEME_STX_CAR(name);
    } else {
      rhs_name = scheme_false;
    }

    scheme_init_expand_recs(erec, drec, &erec1, 1);
    erec1.value_name = rhs_name;
    rhs = scheme_expand_expr(rhs, use_env, &erec1, 0);

    v = scheme_datum_to_syntax(cons(name, cons(rhs, scheme_null)), v, v, 0, 1);
    v = cons(v, scheme_null);

    if (!first)
      first = v;
    else
      SCHEME_CDR(last) = v;

    last = v;

    if (rec_env_already == 2) {
      /* Expansion for internal definitions: break into `let' and
         `letrec' groups based on references among definitions: */
      int cnt;
      cnt = scheme_stx_proper_list_length(name);
      if (SCHEME_NULLP(SCHEME_CDR(first))
          && !scheme_env_min_use_below(use_env, forward_ref_boundary)) {
        /* no self or forward references */
        first = scheme_datum_to_syntax(first, vs, vs, 0, 1);
        pre_set = cons(cons(let_values_symbol, first), pre_set);
        first = NULL;
      } else if (!scheme_env_min_use_below(use_env, forward_ref_boundary - cnt)) {
        /* no (further) forward references */
        first = scheme_datum_to_syntax(first, vs, vs, 0, 1);
        pre_set = cons(cons(letrec_values_symbol, first), pre_set);
        first = NULL;
      }
      forward_ref_boundary -= cnt;
    }

    vars = SCHEME_STX_CDR(vars);
  }

  /* End Pass 2 */

  if (!SCHEME_STX_NULLP(vars))
    scheme_wrong_syntax(NULL, vars, form, NULL);
  
  if (SCHEME_NULLP(pre_set) || first) {
    if (!first)
      first = scheme_null;
    
    first = scheme_datum_to_syntax(first, vs, vs, 0, 1);
  }

  SCHEME_EXPAND_OBSERVE_NEXT_GROUP(erec[drec].observer);
  scheme_init_expand_recs(erec, drec, &erec1, 1);
  erec1.value_name = erec[drec].value_name;
  body = expand_block(body, env, &erec1, 0);
  
  if (SCHEME_PAIRP(pre_set)) {
    if (first)
      pre_set = cons(cons(letrec_values_symbol, first), pre_set);

    while (!SCHEME_NULLP(pre_set)) {
      v = scheme_datum_to_syntax(SCHEME_CAR(SCHEME_CAR(pre_set)), orig_form, scheme_sys_wraps(origenv), 0, 0);
      body = cons(v, cons(SCHEME_CDR(SCHEME_CAR(pre_set)), body));
      body = scheme_datum_to_syntax(body, orig_form, orig_form, 0, 2);
      body = cons(body, scheme_null);
      pre_set = SCHEME_CDR(pre_set);
    }
    
    return SCHEME_CAR(body);
  } else {
    v = SCHEME_STX_CAR(form);
    v = cons(v, cons(first, body));
    v = scheme_datum_to_syntax(v, orig_form, orig_form, 0, 2);
  }

  return v;
}

static Scheme_Object *
let_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_LET_VALUES(erec[drec].observer);
  return do_let_expand(form, env, erec, drec, "let-values", 0, 1, 0, NULL);
}

static Scheme_Object *
let_star_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_LETSTAR_VALUES(erec[drec].observer);
  return do_let_expand(form, env, erec, drec, "let*-values", 0, 1, 1, NULL);
}

static Scheme_Object *
letrec_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_LETREC_VALUES(erec[drec].observer);
  return do_let_expand(form, env, erec, drec, "letrec-values", 1, 1, 0, NULL);
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

/**********************************************************************/
/*                   begin, begin0, implicit begins                   */
/**********************************************************************/

Scheme_Object *scheme_compile_sequence(Scheme_Object *forms,
				       Scheme_Comp_Env *env, 
				       Scheme_Compile_Info *rec, int drec)
{
#if 0
  /* This attempt at a shortcut is wrong, because the sole expression might expand
     to a `begin' that needs to be spliced into an internal-definition context. */
 try_again:

  if (SCHEME_STX_PAIRP(forms) && SCHEME_STX_NULLP(SCHEME_STX_CDR(forms))) {
    /* If it's a begin, we have to check some more... */
    Scheme_Object *first, *val;

    first = SCHEME_STX_CAR(forms);
    first = scheme_check_immediate_macro(first, env, rec, drec, 1, &val, NULL, NULL, 0);

    if (SAME_OBJ(val, scheme_begin_syntax) && SCHEME_STX_PAIRP(first)) {      
      /* Flatten begin: */
      if (scheme_stx_proper_list_length(first) > 1) {
        Scheme_Object *rest;
        rest = scheme_flatten_begin(first, scheme_null);
        first = scheme_datum_to_syntax(rest, first, first, 0, 2);
        forms = first;
        goto try_again;
      }
    }

    return scheme_compile_expr(first, env, rec, drec);
  }
#endif

  if (scheme_stx_proper_list_length(forms) < 0) {
    scheme_wrong_syntax(scheme_begin_stx_string, NULL, 
                        scheme_datum_to_syntax(cons(begin_symbol, forms), forms, forms, 0, 0),
                        IMPROPER_LIST_FORM);
    return NULL;
  } else {
    Scheme_Object *body;
    body = compile_block(forms, env, rec, drec);
    return scheme_make_sequence_compilation(body, 1);
  }
}

Scheme_Object *scheme_compiled_void()
{
  return scheme_void;
}

static Scheme_Object *
do_begin_syntax(char *name,
		Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec, 
		int zero)
{
  Scheme_Object *forms, *body;

  form = scheme_stx_taint_disarm(form, NULL);

  forms = SCHEME_STX_CDR(form);
  
  if (SCHEME_STX_NULLP(forms)) {
    if (!zero && scheme_is_toplevel(env))
      return scheme_compiled_void();
    scheme_wrong_syntax(NULL, NULL, form, "empty form not allowed");
    return NULL;
  }

  check_form(form, form);

  if (zero)
    env = scheme_no_defines(env);

  /* if the begin has only one expression inside, drop the begin 
     TODO: is this right */
  if (SCHEME_STX_NULLP(SCHEME_STX_CDR(forms))) {
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

      scheme_init_compile_recs(rec, drec, recs, 2);
      recs[0].value_name = vname;

      first = SCHEME_STX_CAR(forms);
      first = scheme_compile_expr(first, env, recs, 0);
      rest = SCHEME_STX_CDR(forms);
      rest = scheme_compile_list(rest, env, recs, 1);
      
      scheme_merge_compile_recs(rec, drec, recs, 2);

      body = cons(first, rest);
    } else {
      Scheme_Object *v;
      v = scheme_check_name_property(form, rec[drec].value_name);
      rec[drec].value_name = v;

      body = scheme_compile_list(forms, env, rec, drec);
    }
  } else {
    /* Top level */
    body = scheme_compile_list(forms, env, rec, drec);
  }

  forms = scheme_make_sequence_compilation(body, zero ? -1 : 1);

  if (!zero
      && SAME_TYPE(SCHEME_TYPE(forms), scheme_sequence_type)
      && scheme_is_toplevel(env)) {
    forms->type = scheme_splice_sequence_type;
    return forms;
  }

  return forms;
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

Scheme_Sequence *scheme_malloc_sequence(int count)
{
  return (Scheme_Sequence *)scheme_malloc_tagged(sizeof(Scheme_Sequence)
						 + (count - mzFLEX_DELTA) 
						 * sizeof(Scheme_Object *));
}

Scheme_Object *scheme_make_sequence_compilation(Scheme_Object *seq, int opt)
{
  /* We have to be defensive in processing `seq'; it might be bad due
     to a bad .zo */
  Scheme_Object *list, *v, *good;
  Scheme_Sequence *o;
  int count, i, k, total, last, first, setgood;
  Scheme_Type type;

  type = scheme_sequence_type;

  list = seq;
  count = i = 0;
  good = NULL;
  total = 0;
  first = 1;
  setgood = 1;
  while (SCHEME_PAIRP(list)) {
    v = SCHEME_CAR(list);
    list = SCHEME_CDR(list);
    last = SCHEME_NULLP(list);

    if (((opt > 0) || !first) && SAME_TYPE(SCHEME_TYPE(v), type)) {
      /* "Inline" nested begins */
      count += ((Scheme_Sequence *)v)->count;
      total++;
    } else if (opt
               && (((opt > 0) && !last) || ((opt < 0) && !first))
               && scheme_omittable_expr(v, -1, -1, 0, NULL, NULL, 0, 0, 1)) {
      /* A value that is not the result. We'll drop it. */
      total++;
    } else {
      if (setgood)
	good = v;
      count++;
      total++;
    }
    i++;
    if (first) {
      if (opt < 0)
	setgood = 0;
      first = 0;
    }
  }

  if (!SCHEME_NULLP(list))
    return NULL; /* bad .zo */

  if (!count)
    return scheme_compiled_void();
  
  if (count == 1) {
    if (opt < -1) {
      /* can't optimize away a begin0 reading a .zo time */
    } else if ((opt < 0) && !scheme_omittable_expr(SCHEME_CAR(seq), 1, -1, 0, NULL, NULL, 0, 0, 1)) {
      /* We can't optimize (begin0 expr cont) to expr because
	 exp is not in tail position in the original (so we'd mess
	 up continuation marks). */
    } else
      return good;
  }

  o = scheme_malloc_sequence(count);

  o->so.type = ((opt < 0) ? scheme_begin0_sequence_type : scheme_sequence_type);
  o->count = count;
  
  --total;
  for (i = k = 0; i < count; k++) {
    v = SCHEME_CAR(seq);
    seq = SCHEME_CDR(seq);

    if (((opt > 0) || k) && SAME_TYPE(SCHEME_TYPE(v), type)) {
      int c, j;
      Scheme_Object **a;

      c = ((Scheme_Sequence *)v)->count;
      a = ((Scheme_Sequence *)v)->array; /* <-- mismaligned for precise GC */
      for (j = 0; j < c; j++) {
	o->array[i++] = a[j];
      }
    } else if (opt 
	       && (((opt > 0) && (k < total))
		   || ((opt < 0) && k))
	       && scheme_omittable_expr(v, -1, -1, 0, NULL, NULL, 0, 0, 1)) {
      /* Value not the result. Do nothing. */
    } else
      o->array[i++] = v;
  }

  return (Scheme_Object *)o;
}

static Scheme_Object *
stratified_body_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *body;

  check_form(form, form);

  body = SCHEME_STX_CDR(form);
  body = scheme_datum_to_syntax(body, form, form, 0, 0);

  body = compile_stratified_block(body, env, rec, drec);

  if (SCHEME_NULLP(SCHEME_CDR(body)))
    return SCHEME_CAR(body);
  else
    return scheme_make_sequence_compilation(body, 1);
}

static Scheme_Object *
do_begin_expand(char *name,
		Scheme_Object *orig_form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec,
		int zero)
{
  Scheme_Object *form_name;
  Scheme_Object *rest;
  Scheme_Object *form;

  form = scheme_stx_taint_disarm(orig_form, NULL);

  check_form(form, form);

  form_name = SCHEME_STX_CAR(form);

  rest = SCHEME_STX_CDR(form);

  if (SCHEME_STX_NULLP(rest)) {
    if (!zero && scheme_is_toplevel(env)) {
      SCHEME_EXPAND_OBSERVE_ENTER_LIST(erec[drec].observer, form);
      SCHEME_EXPAND_OBSERVE_EXIT_LIST(erec[drec].observer, form);
      return orig_form;
    }
    scheme_wrong_syntax(NULL, NULL, form, "empty form not allowed");
    return NULL;
  }

  if (zero)
    env = scheme_no_defines(env);

  if (!scheme_is_toplevel(env)) {
    /* Not at top-level: */
    if (zero) {
      Scheme_Object *fst, *boundname;
      Scheme_Expand_Info erec1;
      scheme_init_expand_recs(erec, drec, &erec1, 1);
      boundname = scheme_check_name_property(form, erec[drec].value_name);
      erec1.value_name = boundname;
      erec[drec].value_name = scheme_false;
      fst = SCHEME_STX_CAR(rest);
      rest = SCHEME_STX_CDR(rest);

      SCHEME_EXPAND_OBSERVE_NEXT(erec[drec].observer);
      fst = scheme_expand_expr(fst, env, &erec1, 0);
      rest = scheme_datum_to_syntax(rest, form, form, 0, 0);
      SCHEME_EXPAND_OBSERVE_NEXT(erec[drec].observer);
      rest = scheme_expand_list(rest, env, erec, drec);

      form = cons(fst, rest);
    } else {
      Scheme_Object *boundname;
      boundname = scheme_check_name_property(form, erec[drec].value_name);
      erec[drec].value_name = boundname;
      
      form = scheme_expand_list(scheme_datum_to_syntax(rest, form, form, 0, 0),
				env, erec, drec);
#if 0
      if (SCHEME_STX_NULLP(SCHEME_STX_CDR(form)))
	return scheme_stx_taint_rearm(SCHEME_STX_CAR(form), orig_form);
#endif
    }
  } else {
    /* Top level */
    form =  scheme_expand_list(scheme_datum_to_syntax(rest, form, form, 0, 0),
			       env, erec, drec);
  }

  return scheme_datum_to_syntax(cons(form_name, form), 
				orig_form, orig_form, 
				0, 2);
}

static Scheme_Object *
begin_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_BEGIN(erec[drec].observer);
  return do_begin_expand("begin", form, env, erec, drec, 0);
}

static Scheme_Object *
begin0_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_BEGIN0(erec[drec].observer);
  return do_begin_expand("begin0", form, env, erec, drec, 1);
}

static Scheme_Object *
stratified_body_expand(Scheme_Object *orig_form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  Scheme_Object *body, *form;

  SCHEME_EXPAND_OBSERVE_PRIM_STRATIFIED(erec[drec].observer);

  form = scheme_stx_taint_disarm(orig_form, NULL);

  check_form(form, form);

  body = SCHEME_STX_CDR(form);
  body = scheme_datum_to_syntax(body, form, form, 0, 0);
  
  body = expand_stratified_block(body, env, erec, drec);
  
  if (SCHEME_STX_NULLP(SCHEME_STX_CDR(body))) {
    body = SCHEME_STX_CAR(body);
    return scheme_stx_taint_rearm(body, orig_form);
  } else {
    body = cons(scheme_datum_to_syntax(begin_symbol, scheme_false, scheme_sys_wraps(env), 0, 0),
                body);
    return scheme_datum_to_syntax(body, orig_form, orig_form, 0, 0);
  }
}

/**********************************************************************/
/*                    #%non-module and #%expression                   */
/**********************************************************************/

static Scheme_Object *check_single(Scheme_Object *form, Scheme_Comp_Env *top_only)
{
  Scheme_Object *rest;

  form = scheme_stx_taint_disarm(form, NULL);

  check_form(form, form);

  rest = SCHEME_STX_CDR(form);
  if (!(SCHEME_STX_PAIRP(rest) && SCHEME_STX_NULLP(SCHEME_STX_CDR(rest))))
    scheme_wrong_syntax(NULL, NULL, form, "wrong number of parts");

  if (top_only && !scheme_is_toplevel(top_only))
    scheme_wrong_syntax(NULL, NULL, form, "illegal use (not at top-level)");

  return SCHEME_STX_CAR(rest);
}

static Scheme_Object *
single_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec, int top_only)
{
  return scheme_compile_expr(check_single(form, top_only ? env: NULL), env, rec, drec);
}

static Scheme_Object *
single_expand(Scheme_Object *orig_form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec, 
              int top_only, int simplify)
{
  Scheme_Object *expr, *form_name, *form;

  form = scheme_stx_taint_disarm(orig_form, NULL);

  expr = check_single(form, top_only ? env : NULL);
  expr = scheme_expand_expr(expr, env, erec, drec);

  form_name = SCHEME_STX_CAR(form);

  if (simplify && (erec[drec].depth == -1)) {
    expr = scheme_stx_track(expr, form, form_name);
    SCHEME_EXPAND_OBSERVE_TAG(erec[drec].observer,expr);
    return expr;
  }

  return scheme_datum_to_syntax(cons(form_name, cons(expr, scheme_null)), 
				orig_form, orig_form,
				0, 2);
}

static Scheme_Object *expression_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return single_syntax(form, scheme_no_defines(env), rec, drec, 0);
}

static Scheme_Object *expression_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_EXPRESSION(erec[drec].observer);
  return single_expand(form, scheme_no_defines(env), erec, drec, 0,
                       !(env->flags & SCHEME_TOPLEVEL_FRAME));
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
quote_syntax_syntax(Scheme_Object *orig_form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  int len;
  Scheme_Object *stx, *form;

  if (rec[drec].comp)
    env->prefix->non_phaseless = 1;

  form = scheme_stx_taint_disarm(orig_form, NULL);

  if (rec[drec].comp)
    scheme_compile_rec_done_local(rec, drec);

  len = check_form(form, form);
  if (len != 2)
    bad_form(form, len);

  if (rec[drec].comp) {
    stx = SCHEME_STX_CDR(form);
    stx = SCHEME_STX_CAR(stx);
    return scheme_register_stx_in_prefix(stx, env, rec, drec);
  } else
    return orig_form;
}

static Scheme_Object *
quote_syntax_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_QUOTE_SYNTAX(erec[drec].observer);
  return quote_syntax_syntax(form, env, erec, drec);
}


/**********************************************************************/
/*                          define-syntaxes                           */
/**********************************************************************/

static void prep_exp_env_compile_rec(Scheme_Compile_Info *rec, int drec)
{
  rec[0].comp = 1;
  rec[0].dont_mark_local_use = 0;
  rec[0].resolve_module_ids = 0;
  rec[0].value_name = NULL;
  rec[0].observer = NULL;
  rec[0].pre_unwrapped = 0;
  rec[0].testing_constantness = 0;
  rec[0].env_already = 0;
  rec[0].comp_flags = rec[drec].comp_flags;
}

static Scheme_Object *stx_val(Scheme_Object *name, Scheme_Object *_env)
{
  Scheme_Env *env = (Scheme_Env *)_env;

  return scheme_tl_id_sym(env, name, NULL, 2, NULL, NULL);
}

static Scheme_Object *
do_define_syntaxes_syntax(Scheme_Object *form, Scheme_Comp_Env *env, 
			  Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *names, *code, *dummy;
  Scheme_Object *val, *vec;
  Scheme_Comp_Env *exp_env;
  Scheme_Compile_Info rec1;

  scheme_compile_rec_done_local(rec, drec);
  scheme_default_compile_rec(rec, drec);
      
  scheme_define_parse(form, &names, &code, 1, env, 0);

  scheme_prepare_exp_env(env->genv);
  scheme_prepare_compile_env(env->genv->exp_env);

  names = scheme_named_map_1(NULL, stx_val, names, (Scheme_Object *)env->genv);

  exp_env = scheme_new_comp_env(env->genv->exp_env, env->insp, 0);

  dummy = scheme_make_environment_dummy(env);

  prep_exp_env_compile_rec(&rec1, 0);

  val = scheme_compile_expr_lift_to_let(code, exp_env, &rec1, 0);

  vec = scheme_make_vector(4, NULL);
  SCHEME_VEC_ELS(vec)[0] = (Scheme_Object *)exp_env->prefix;
  SCHEME_VEC_ELS(vec)[1] = dummy;
  SCHEME_VEC_ELS(vec)[2] = names;
  SCHEME_VEC_ELS(vec)[3] = val;

  vec->type = scheme_define_syntaxes_type;

  scheme_merge_undefineds(exp_env, env);

  return vec;
}

static Scheme_Object *
define_syntaxes_syntax(Scheme_Object *form, Scheme_Comp_Env *env, 
		       Scheme_Compile_Info *rec, int drec)
{
  return do_define_syntaxes_syntax(form, env, rec, drec);
}

static Scheme_Object *
define_syntaxes_expand(Scheme_Object *orig_form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  Scheme_Object *names, *code, *fpart, *fn, *form;

  SCHEME_EXPAND_OBSERVE_PRIM_DEFINE_SYNTAXES(erec[drec].observer);

  form = orig_form;

  scheme_define_parse(form, &names, &code, 1, env, 0);

  SCHEME_EXPAND_OBSERVE_PREPARE_ENV(erec[drec].observer);

  scheme_prepare_exp_env(env->genv);
  scheme_prepare_compile_env(env->genv->exp_env);
  
  env = scheme_new_expand_env(env->genv->exp_env, env->insp, 0);

  erec[drec].value_name = names;
  fpart = scheme_expand_expr_lift_to_let(code, env, erec, drec);
  
  code = cons(fpart, scheme_null);
  code = cons(names, code);

  fn = SCHEME_STX_CAR(form);
  return scheme_datum_to_syntax(cons(fn, code), 
				orig_form, orig_form,
				0, 2);
}

static Scheme_Object *
begin_for_syntax_expand(Scheme_Object *orig_form, Scheme_Comp_Env *in_env, Scheme_Expand_Info *rec, int drec)
{
  Scheme_Expand_Info recs[1];
  Scheme_Object *form, *l, *fn, *vec, *dummy;
  Scheme_Comp_Env *env;

  SCHEME_EXPAND_OBSERVE_PRIM_BEGIN_FOR_SYNTAX(rec[drec].observer);

  form = orig_form;

  if (!scheme_is_toplevel(in_env))
    scheme_wrong_syntax(NULL, NULL, form, "not in a definition context");

  (void)check_form(form, form);

  SCHEME_EXPAND_OBSERVE_PREPARE_ENV(rec[drec].observer);

  scheme_prepare_exp_env(in_env->genv);
  scheme_prepare_compile_env(in_env->genv->exp_env);

  if (rec[drec].comp)
    env = scheme_new_comp_env(in_env->genv->exp_env, in_env->insp, 0);
  else
    env = scheme_new_expand_env(in_env->genv->exp_env, in_env->insp, 0);

  if (rec[drec].comp)
    dummy = scheme_make_environment_dummy(in_env);
  else
    dummy = NULL;

  l = SCHEME_STX_CDR(form);
  form = scheme_null;

  while (1) {
    scheme_frame_captures_lifts(env, scheme_make_lifted_defn, scheme_sys_wraps(env),
                                scheme_false, scheme_top_level_lifts_key(env), scheme_null, scheme_false);

    if (rec[drec].comp) {
      scheme_init_compile_recs(rec, drec, recs, 1);
      prep_exp_env_compile_rec(recs, 0);
      l = scheme_compile_list(l, env, recs, 0);
    } else {
      scheme_init_expand_recs(rec, drec, recs, 1);
      l = scheme_expand_list(l, env, recs, 0);
    }

    if (SCHEME_NULLP(form))
      form = l;
    else
      form = scheme_append(l, form);
    
    l = scheme_frame_get_lifts(env);
    if (SCHEME_NULLP(l)) {
      /* No lifts */
      if (rec[drec].comp)
        scheme_merge_compile_recs(rec, drec, NULL, 1); /* fix this if merge changes to do something */
      break;
    } else {
      /* We have lifts: */
      SCHEME_EXPAND_OBSERVE_MODULE_LIFT_LOOP(rec[drec].observer, l);
    }
  }

  if (rec[drec].comp) {
    vec = scheme_make_vector(4, NULL);
    SCHEME_VEC_ELS(vec)[0] = (Scheme_Object *)env->prefix;
    SCHEME_VEC_ELS(vec)[1] = dummy;
    SCHEME_VEC_ELS(vec)[2] = form;
    vec->type = scheme_begin_for_syntax_type;

    return vec;
  } else {
    fn = SCHEME_STX_CAR(orig_form);
    return scheme_datum_to_syntax(cons(fn, form), 
                                  orig_form, orig_form,
                                  0, 2);
  }
}

static Scheme_Object *
begin_for_syntax_syntax(Scheme_Object *form, Scheme_Comp_Env *env, 
                        Scheme_Compile_Info *rec, int drec)
{
  return begin_for_syntax_expand(form, env, rec, drec);
}

Scheme_Object *scheme_make_environment_dummy(Scheme_Comp_Env *env)
{ 
  /* Get a prefixed-based accessor for a dummy top-level bucket. It's
     used to "link" to the right environment at run time. The #f as
     a toplevel is handled in the prefix linker specially. */
  return scheme_register_toplevel_in_prefix(scheme_false, env, NULL, 0, 0, NULL);
}

Scheme_Env *scheme_environment_from_dummy(Scheme_Object *dummy)
{
  Scheme_Prefix *toplevels;
  Scheme_Bucket *b;

  toplevels = (Scheme_Prefix *)MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(dummy)];
  b = (Scheme_Bucket *)toplevels->a[SCHEME_TOPLEVEL_POS(dummy)];
  return scheme_get_bucket_home(b);
}

/**********************************************************************/
/*                           letrec-syntaxes                          */
/**********************************************************************/

static void *eval_letmacro_rhs_k(void);

static Scheme_Object *eval_letmacro_rhs(Scheme_Object *a, Scheme_Comp_Env *rhs_env, 
					int max_let_depth, Resolve_Prefix *rp,
					int phase)
{
  Scheme_Object **save_runstack;
  int depth;

  depth = max_let_depth + scheme_prefix_depth(rp);
  if (!scheme_check_runstack(depth)) {
    Scheme_Thread *p = scheme_current_thread;
    p->ku.k.p1 = a;
    p->ku.k.p2 = rhs_env;
    p->ku.k.p3 = rp;
    p->ku.k.i1 = max_let_depth;
    p->ku.k.i2 = phase;
    return (Scheme_Object *)scheme_enlarge_runstack(depth, eval_letmacro_rhs_k);
  }

  save_runstack = scheme_push_prefix(NULL, rp, NULL, NULL, phase, phase, rhs_env->genv, NULL);

  if (scheme_omittable_expr(a, 1, -1, 0, NULL, NULL, 0, 0, 0)) {
    /* short cut */
    a = _scheme_eval_linked_expr_multi(a);
  } else {
    Scheme_Cont_Frame_Data cframe;
    Scheme_Config *config;
    Scheme_Dynamic_State dyn_state;

    scheme_prepare_exp_env(rhs_env->genv);
    scheme_prepare_compile_env(rhs_env->genv->exp_env);

    config = scheme_extend_config(scheme_current_config(),
                                  MZCONFIG_ENV,
                                  (Scheme_Object *)rhs_env->genv->exp_env);
    scheme_push_continuation_frame(&cframe);
    scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);
  
    scheme_set_dynamic_state(&dyn_state, rhs_env, NULL, scheme_false, rhs_env->genv, rhs_env->genv->link_midx);
    a = scheme_eval_linked_expr_multi_with_dynamic_state(a, &dyn_state);
    
    scheme_pop_continuation_frame(&cframe);
  }

  scheme_pop_prefix(save_runstack);

  return a;
}

static void *eval_letmacro_rhs_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *a;
  Scheme_Comp_Env *rhs_env;
  int max_let_depth, phase;
  Resolve_Prefix *rp;

  a = (Scheme_Object *)p->ku.k.p1;
  rhs_env = (Scheme_Comp_Env *)p->ku.k.p2;
  rp = (Resolve_Prefix *)p->ku.k.p3;
  max_let_depth = p->ku.k.i1;
  phase = p->ku.k.i2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  return (void *)eval_letmacro_rhs(a, rhs_env, max_let_depth, rp, phase);
}

void scheme_bind_syntaxes(const char *where, Scheme_Object *names, Scheme_Object *a, 
                          Scheme_Env *exp_env, Scheme_Object *insp, 
                          Scheme_Compile_Expand_Info *rec, int drec,
                          Scheme_Comp_Env *stx_env, Scheme_Comp_Env *rhs_env,
                          int *_pos, Scheme_Object *rename_rib)
{
  Scheme_Object **results, *l, *a_expr;
  Scheme_Comp_Env *eenv;
  Resolve_Prefix *rp;
  Resolve_Info *ri;
  Optimize_Info *oi;
  int vc, nc, j, i;
  Scheme_Compile_Expand_Info mrec;

  eenv = scheme_new_comp_env(exp_env, insp, 0);

  /* First expand for expansion-observation */
  if (!rec[drec].comp) {
    scheme_init_expand_recs(rec, drec, &mrec, 1);
    SCHEME_EXPAND_OBSERVE_ENTER_BIND(rec[drec].observer);
    a = scheme_expand_expr_lift_to_let(a, eenv, &mrec, 0);
  }

  /* Then compile */
  mrec.comp = 1;
  mrec.dont_mark_local_use = 0;
  mrec.resolve_module_ids = 1;
  mrec.value_name = NULL;
  mrec.observer = NULL;
  mrec.pre_unwrapped = 0;
  mrec.testing_constantness = 0;
  mrec.env_already = 0;
  mrec.comp_flags = rec[drec].comp_flags;

  a = scheme_compile_expr_lift_to_let(a, eenv, &mrec, 0);

  a = scheme_letrec_check_expr(a);

  oi = scheme_optimize_info_create(eenv->prefix, 1);
  if (!(rec[drec].comp_flags & COMP_CAN_INLINE))
    scheme_optimize_info_never_inline(oi);
  a = scheme_optimize_expr(a, oi, 0);

  /* For internal defn, don't simplify as resolving, because the
       expression may have syntax objects with a lexical rename that
       is still being extended. 
     For letrec-syntaxes+values, don't simplify because it's too expensive. */
  rp = scheme_resolve_prefix(eenv->genv->phase, eenv->prefix, 0);

  ri = scheme_resolve_info_create(rp);
  a = scheme_resolve_expr(a, ri);

  rp = scheme_remap_prefix(rp, ri);

  /* To JIT:
       if (ri->use_jit) a = scheme_jit_expr(a);
     but it's not likely that a let-syntax-bound macro is going
     to run lots of times, so JITting is probably not worth it. */

  SCHEME_EXPAND_OBSERVE_NEXT(rec[drec].observer);

  a_expr = a;
  a = eval_letmacro_rhs(a_expr, rhs_env, 
                        scheme_resolve_info_max_let_depth(ri), 
                        rp, eenv->genv->phase);

  if (SAME_OBJ(a, SCHEME_MULTIPLE_VALUES)) {
    vc = scheme_current_thread->ku.multiple.count;
    results = scheme_current_thread->ku.multiple.array;
    scheme_current_thread->ku.multiple.array = NULL;
    if (SAME_OBJ(results, scheme_current_thread->values_buffer))
      scheme_current_thread->values_buffer = NULL;
  } else {
    vc = 1;
    results = NULL;
  }

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
			      (vc == 1) ? (Scheme_Object **)a : results, 
			      "%s%s%s",
			      name ? "defining \"" : "0 names",
			      symname,
			      name ? ((nc == 1) ? "\"" : "\", ...") : "");
  }

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

    if (scheme_is_binding_rename_transformer(SCHEME_PTR_VAL(macro))) {
      /* Install a free-id=? rename */
      scheme_install_free_id_rename(name, scheme_rename_transformer_id(SCHEME_PTR_VAL(macro)), rename_rib,
                                    scheme_make_integer(rhs_env->genv->phase));
    }
  }
  *_pos = i;

  scheme_merge_undefineds(eenv, rhs_env);

  SCHEME_EXPAND_OBSERVE_EXIT_BIND(rec[drec].observer);
}

static Scheme_Object *
do_letrec_syntaxes(const char *where,
		   Scheme_Object *orig_forms, Scheme_Comp_Env *origenv, 
		   Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *forms, *form, *bindings, *var_bindings, *body, *v;
  Scheme_Object *names_to_disappear;
  Scheme_Comp_Env *stx_env, *var_env, *rhs_env;
  int cnt, stx_cnt, var_cnt, i, j, depth, saw_var, env_already;
  DupCheckRecord r;

  forms = scheme_stx_taint_disarm(orig_forms, NULL);

  env_already = rec[drec].env_already;

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

  if (env_already)
    stx_env = origenv;
  else
    stx_env = scheme_new_compilation_frame(0, 0, origenv);

  rhs_env = stx_env;

  if (!SCHEME_STX_NULLP(bindings) && !SCHEME_STX_PAIRP(bindings)) {
    scheme_wrong_syntax(NULL, bindings, forms, "not a binding sequence");
  } else
    check_form(bindings, forms);
  if (!SCHEME_STX_NULLP(var_bindings) && !SCHEME_STX_PAIRP(var_bindings)) {
    scheme_wrong_syntax(NULL, var_bindings, forms, "not a binding sequence");
  } else
    check_form(var_bindings, forms);

  cnt = stx_cnt = var_cnt = 0;
  saw_var = 0;

  depth = rec[drec].depth;

  if (!rec[drec].comp && (depth <= 0) && (depth > -2))
    names_to_disappear = scheme_null;
  else
    names_to_disappear = NULL;

  if (!env_already)
    scheme_begin_dup_symbol_check(&r, stx_env);

  /* Pass 1: Check and Rename */

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
			    "binding clause not an identifier sequence and expression");

      for (l = SCHEME_STX_CAR(a); SCHEME_STX_PAIRP(l); l = SCHEME_STX_CDR(l)) {
	a = SCHEME_STX_CAR(l);
        if (!env_already) {
          scheme_check_identifier(where, a, NULL, stx_env, forms);
          scheme_dup_symbol_check(&r, where, a, "binding", forms);
        }
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

  if (!env_already)
    scheme_add_local_syntax(stx_cnt, stx_env);
  
  if (saw_var) {
    var_env = scheme_new_compilation_frame(var_cnt, 
                                           (env_already ? SCHEME_INTDEF_SHADOW : 0), 
                                           stx_env);
  } else
    var_env = NULL;

  for (i = (env_already ? 1 : 0); i < (var_env ? 2 : 1) ; i++) {
    cnt = (i ? var_cnt : stx_cnt);
    if (cnt > 0) {
      /* Add new syntax/variable names to the environment: */
      if (i) {
        /* values in reverse order across clauses, in order within a clause */
        j = var_cnt;
      } else
        j = 0;
      for (v = (i ? var_bindings : bindings); SCHEME_STX_PAIRP(v); v = SCHEME_STX_CDR(v)) {
        Scheme_Object *a, *l;
        int pre_j;

        if (i) {
          a = SCHEME_STX_CAR(v);
          for (l = SCHEME_STX_CAR(a); SCHEME_STX_PAIRP(l); l = SCHEME_STX_CDR(l)) {
            j--;
          }
          pre_j = j;
        } else
          pre_j = 0;
	
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

        if (i) j = pre_j;
      }
    }
  }

  if (names_to_disappear) {
    for (v = bindings; SCHEME_STX_PAIRP(v); v = SCHEME_STX_CDR(v)) {
      Scheme_Object *a, *names;

      a = SCHEME_STX_CAR(v);
      names = SCHEME_STX_CAR(a);
      while (!SCHEME_STX_NULLP(names)) {
        a = SCHEME_STX_CAR(names);
        if (names_to_disappear)
          names_to_disappear = cons(a, names_to_disappear);
        names = SCHEME_STX_CDR(names);
      }
    }
  }
  
  bindings = scheme_add_env_renames(bindings, stx_env, origenv);
  if (var_env)
    bindings = scheme_add_env_renames(bindings, var_env, origenv);
  if (var_env)
    var_bindings = scheme_add_env_renames(var_bindings, stx_env, origenv);

  body = scheme_add_env_renames(body, stx_env, origenv);
  SCHEME_EXPAND_OBSERVE_LETREC_SYNTAXES_RENAMES(rec[drec].observer, bindings, var_bindings, body);

  SCHEME_EXPAND_OBSERVE_PREPARE_ENV(rec[drec].observer);
  scheme_prepare_exp_env(stx_env->genv);
  scheme_prepare_compile_env(stx_env->genv->exp_env);

  if (!env_already) {
    i = 0;

    for (v = bindings; SCHEME_STX_PAIRP(v); v = SCHEME_STX_CDR(v)) {
      Scheme_Object *a, *names;

      SCHEME_EXPAND_OBSERVE_NEXT(rec[drec].observer);

      a = SCHEME_STX_CAR(v);
      names = SCHEME_STX_CAR(a);
      a = SCHEME_STX_CDR(a);
      a = SCHEME_STX_CAR(a);

      scheme_bind_syntaxes(where, names, a,
                           stx_env->genv->exp_env,
                           stx_env->insp,
                           rec, drec,
                           stx_env, rhs_env, 
                           &i, NULL);
    }
  }

  SCHEME_EXPAND_OBSERVE_NEXT_GROUP(rec[drec].observer);

  if (!env_already && names_to_disappear) {
    /* Need to add renaming for disappeared bindings. If they
       originated for internal definitions, then we need both
       pre-renamed and renamed, since some might have been
       expanded to determine definitions. */
    Scheme_Object *l, *a, *pf = NULL, *pl = NULL;

    if (origenv->flags & SCHEME_FOR_INTDEF) {
      for (l = names_to_disappear; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
        a = SCHEME_CAR(l);
        a = cons(a, scheme_null);
        if (pl)
          SCHEME_CDR(pl) = a;
        else
          pf = a;
        pl = a;
      }
    }

    for (l = names_to_disappear; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
      a = SCHEME_CAR(l);
      a = scheme_add_env_renames(a, stx_env, origenv);
      SCHEME_CAR(l) = a;
    }

    if (pf) {
      SCHEME_CDR(pl) = names_to_disappear;
      names_to_disappear = pf;
    }
  }

  if (!var_env) {
    var_env = scheme_require_renames(stx_env);
    if (rec[drec].comp) {
      v = scheme_check_name_property(forms, rec[drec].value_name);
      rec[drec].value_name = v;
      v = compile_block(body, var_env, rec, drec);
      v = scheme_make_sequence_compilation(v, 1);
    } else {
      v = expand_block(body, var_env, rec, drec);
      if ((depth >= 0) || (depth == -2)) {
	Scheme_Object *formname;
	formname = SCHEME_STX_CAR(forms);
	v = cons(formname, cons(bindings, cons(var_bindings, v)));
      } else {
	v = cons(let_values_symbol, cons(scheme_null, v));
      }

      if (SCHEME_PAIRP(v))
	v = scheme_datum_to_syntax(v, orig_forms, scheme_sys_wraps(origenv), 
				   0, 2);
      else
        v = scheme_stx_taint_rearm(v, orig_forms);

      if (!((depth >= 0) || (depth == -2))) {
        SCHEME_EXPAND_OBSERVE_TAG(rec[drec].observer,v);
      }
    }
  } else {
    /* Construct letrec-values expression: */
    v = cons(letrec_values_symbol, cons(var_bindings, body));
    v = scheme_datum_to_syntax(v, orig_forms, scheme_sys_wraps(origenv), 0, 2);
    
    if (!env_already) { /* i.e., not internal defn */
      /* We want non-`letrec' semantics for value bindings (i.e., sort
         out the bindings into `letrec' and `let'): */
      rec[drec].env_already = 2;
    }
    
    if (rec[drec].comp) {
      v = gen_let_syntax(v, stx_env, "letrec-values", 0, 1, 1, rec, drec, var_env);
    } else {
      int restore = ((depth >= 0) || (depth == -2));

      if (restore && (rec[drec].env_already == 2)) {
        /* don't sort out after all, because we're keeping `letrec-values+syntaxes' */
        rec[drec].env_already = 1;
      }

      SCHEME_EXPAND_OBSERVE_PRIM_LETREC_VALUES(rec[drec].observer);
      v = do_let_expand(v, stx_env, rec, drec, "letrec-values", 1, 1, 0, var_env);
      
      if (restore) {
	/* Add back out the pieces we want: */
	Scheme_Object *formname;
	formname = SCHEME_STX_CAR(forms);
        v = scheme_stx_taint_disarm(v, NULL);
	v = SCHEME_STX_CDR(v);
	v = cons(formname, cons(bindings, v));
	v = scheme_datum_to_syntax(v, orig_forms, scheme_sys_wraps(origenv), 0, 2);
      } else {
        SCHEME_EXPAND_OBSERVE_TAG(rec[drec].observer,v);
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
  SCHEME_EXPAND_OBSERVE_PRIM_LETREC_SYNTAXES_VALUES(erec[drec].observer);

  return do_letrec_syntaxes("letrec-syntaxes+values", form, env, erec, drec);
}

/*========================================================================*/
/*                            applications                                */
/*========================================================================*/

int scheme_get_eval_type(Scheme_Object *obj)
     /* Categories for short-cutting recursive calls to the evaluator */
{
  Scheme_Type type;

  type = SCHEME_TYPE(obj);

  if (type > _scheme_values_types_)
    return SCHEME_EVAL_CONSTANT;
  else if (SAME_TYPE(type, scheme_local_type))
    return SCHEME_EVAL_LOCAL;
  else if (SAME_TYPE(type, scheme_local_unbox_type))
    return SCHEME_EVAL_LOCAL_UNBOX;
  else if (SAME_TYPE(type, scheme_toplevel_type))
    return SCHEME_EVAL_GLOBAL;
  else
    return SCHEME_EVAL_GENERAL;
}    

Scheme_Object *scheme_try_apply(Scheme_Object *f, Scheme_Object *args, Optimize_Info *info)
     /* Apply `f' to `args' and ignore failues --- used for constant
        folding attempts */
{
  Scheme_Object * volatile result;
  Scheme_Object * volatile exn = NULL;
  mz_jmp_buf *savebuf, newbuf;

  scheme_current_thread->reading_delayed = NULL;
  scheme_current_thread->constant_folding = (info ? info : (Optimize_Info *)scheme_false);
  savebuf = scheme_current_thread->error_buf;
  scheme_current_thread->error_buf = &newbuf;

  if (scheme_setjmp(newbuf)) {
    result = NULL;
    exn = scheme_current_thread->reading_delayed;
  } else
    result = _scheme_apply_to_list(f, args);
  
  scheme_current_thread->error_buf = savebuf;
  scheme_current_thread->constant_folding = NULL;
  scheme_current_thread->reading_delayed = NULL;

  if (scheme_current_thread->cjs.is_kill) {
    scheme_longjmp(*scheme_current_thread->error_buf, 1);
  }

  if (exn)
    scheme_raise(exn);

  return result;
}

static int foldable_body(Scheme_Object *f)
{
  Scheme_Closure_Data *d;
  
  d = SCHEME_COMPILED_CLOS_CODE(f);

  scheme_delay_load_closure(d);

  return (SCHEME_TYPE(d->code) > _scheme_values_types_);
}

int scheme_is_foldable_prim(Scheme_Object *f)
{
  if (SCHEME_PRIMP(f)
      && ((((Scheme_Primitive_Proc *)f)->pp.flags & SCHEME_PRIM_OPT_MASK)
          == SCHEME_PRIM_OPT_FOLDING))
    return 1;

  if (SCHEME_CLSD_PRIMP(f)
      && ((((Scheme_Closed_Primitive_Proc *)f)->pp.flags & SCHEME_PRIM_OPT_MASK)
          == SCHEME_PRIM_OPT_FOLDING))
    return 1;

  return 0;
}

Scheme_Object *scheme_make_application(Scheme_Object *v, Optimize_Info *info)
{
  Scheme_Object *o;
  int i, nv;
  volatile int n;

  o = v;
  n = 0;
  nv = 0;
  while (!SCHEME_NULLP(o)) {
    Scheme_Type type;
    
    n++;
    type = SCHEME_TYPE(SCHEME_CAR(o));
    if (type < _scheme_compiled_values_types_)
      nv = 1;
    o = SCHEME_CDR(o);
  }

  if (!nv) {
    /* They're all values. Applying folding prim or closure? */
    Scheme_Object *f;

    f = SCHEME_CAR(v);

    if (scheme_is_foldable_prim(f)
	|| (SAME_TYPE(SCHEME_TYPE(f), scheme_closure_type)
	    && (foldable_body(f)))) {
      f = scheme_try_apply(f, SCHEME_CDR(v), info);
      
      if (f)
	return f;
    }
  }

  if (n == 2) {
    Scheme_App2_Rec *app;

    app = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
    app->iso.so.type = scheme_application2_type;

    app->rator = SCHEME_CAR(v);
    v = SCHEME_CDR(v);
    app->rand = SCHEME_CAR(v);
    
    return (Scheme_Object *)app;
  } else if (n == 3) {
    Scheme_App3_Rec *app;

    app = MALLOC_ONE_TAGGED(Scheme_App3_Rec);
    app->iso.so.type = scheme_application3_type;

    app->rator = SCHEME_CAR(v);
    v = SCHEME_CDR(v);
    app->rand1 = SCHEME_CAR(v);
    v = SCHEME_CDR(v);
    app->rand2 = SCHEME_CAR(v);

    return (Scheme_Object *)app;
  } else {
    Scheme_App_Rec *app;

    app = scheme_malloc_application(n);
    
    for (i = 0; i < n; i++, v = SCHEME_CDR(v)) {
      app->args[i] = SCHEME_CAR(v);
    }

    return (Scheme_Object *)app;
  }
}

Scheme_App_Rec *scheme_malloc_application(int n)
{
  Scheme_App_Rec *app;
  intptr_t size;

  if (n < 0) {
    scheme_signal_error("bad application count");
    app = NULL;
  } else if (n > 4096) {
    size = scheme_check_overflow(n, 
                                 sizeof(char),
                                 (sizeof(Scheme_App_Rec) 
                                  + ((n - mzFLEX_DELTA) * sizeof(Scheme_Object *))));
    app = (Scheme_App_Rec *)scheme_malloc_fail_ok(scheme_malloc_tagged, size);
    if (!app) scheme_signal_error("out of memory allocating application bytecode");
  } else {
    size = (sizeof(Scheme_App_Rec) 
            + ((n - mzFLEX_DELTA) * sizeof(Scheme_Object *))
            + n * sizeof(char));
    app = (Scheme_App_Rec *)scheme_malloc_tagged(size);
  }

  app->iso.so.type = scheme_application_type;

  app->num_args = n - 1;

  return app;
}

void scheme_finish_application(Scheme_App_Rec *app)
{
  int i, devals, n;

  n = app->num_args + 1;

  devals = sizeof(Scheme_App_Rec) + ((app->num_args + 1 - mzFLEX_DELTA) * sizeof(Scheme_Object *));

  for (i = 0; i < n; i++) {
    char etype;
    etype = scheme_get_eval_type(app->args[i]);
    ((char *)app XFORM_OK_PLUS devals)[i] = etype;
  }
}

/*========================================================================*/
/*                         compilation dispatcher                         */
/*========================================================================*/

static Scheme_Object *
scheme_inner_compile_list(Scheme_Object *form, Scheme_Comp_Env *env, 
			  Scheme_Compile_Info *rec, int drec, int start_app_position)
{
  int len;

  len = scheme_stx_proper_list_length(form);

  if (!len) {
    scheme_compile_rec_done_local(rec, drec);
    scheme_default_compile_rec(rec, drec);
    return scheme_null;
  } else if (len > 0) {
    Scheme_Compile_Info *recs, quick[5];
    int i;
    Scheme_Object *c, *p, *comp_first, *comp_last, *name, *first, *rest;

    name = rec[drec].value_name;
    scheme_compile_rec_done_local(rec, drec);

    if (len <= 5)
      recs = quick;
    else
      recs = MALLOC_N_RT(Scheme_Compile_Info, len);
    scheme_init_compile_recs(rec, drec, recs, len);
    recs[len - 1].value_name = name;

    comp_first = comp_last = NULL;

    for (i = 0, rest = form; i < len; i++) {
      first = SCHEME_STX_CAR(rest);
      rest = SCHEME_STX_CDR(rest);

      c = scheme_compile_expand_expr(first, env, recs, i,
				     !i && start_app_position);

      p = scheme_make_pair(c, scheme_null);
      if (comp_last)
	SCHEME_CDR(comp_last) = p;
      else
	comp_first = p;
      comp_last = p;

      if (!i && start_app_position && (len == 2)
          && SAME_OBJ(c, scheme_varref_const_p_proc)) {
        recs[1].testing_constantness = 1;
      }
    }

    scheme_merge_compile_recs(rec, drec, recs, len);

    return comp_first;
  } else {
    scheme_signal_error("internal error: compile-list on non-list");
    return NULL;
  }
}

static Scheme_Object *compile_application(Scheme_Object *form, Scheme_Comp_Env *env,
					  Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *result;
  int len;

  form = scheme_stx_taint_disarm(form, NULL);

  len = scheme_stx_proper_list_length(form);

  if (len < 0)
    scheme_wrong_syntax(scheme_application_stx_string, NULL, form, NULL);
  
  scheme_compile_rec_done_local(rec, drec);
  form = scheme_inner_compile_list(form, scheme_no_defines(env), rec, drec, 1);

  result = scheme_make_application(form, NULL);
  
  return result;
}

Scheme_Object *
scheme_compile_list(Scheme_Object *form, Scheme_Comp_Env *env, 
		    Scheme_Compile_Info *rec, int drec)
{
  return scheme_inner_compile_list(form, env, rec, drec, 0);
}

Scheme_Object *scheme_check_immediate_macro(Scheme_Object *first, 
					    Scheme_Comp_Env *env, 
					    Scheme_Compile_Expand_Info *rec, int drec,
					    int internel_def_pos,
					    Scheme_Object **current_val,
					    Scheme_Comp_Env **_xenv,
					    Scheme_Object *ctx,
                                            int keep_name)
{
  Scheme_Object *name, *val;
  Scheme_Comp_Env *xenv = (_xenv ? *_xenv : NULL);
  Scheme_Expand_Info erec1;
  Scheme_Env *menv = NULL;

  SCHEME_EXPAND_OBSERVE_ENTER_CHECK(rec[drec].observer, first);

  while (1) {
    *current_val = NULL;

    if (SCHEME_STX_PAIRP(first)) {
      name = scheme_stx_taint_disarm(first, NULL);
      name = SCHEME_STX_CAR(name);
    } else {
      name = first;
    }

    if (!SCHEME_STX_SYMBOLP(name)) {
      SCHEME_EXPAND_OBSERVE_EXIT_CHECK(rec[drec].observer, first);
      return first;
    }

    while (1) {
      val = scheme_lookup_binding(name, env, 
                                  SCHEME_NULL_FOR_UNBOUND
                                  + SCHEME_APP_POS + SCHEME_ENV_CONSTANTS_OK
                                  + SCHEME_DONT_MARK_USE
                                  + ((!rec[drec].comp && (rec[drec].depth == -2))
                                     ? SCHEME_OUT_OF_CONTEXT_OK
                                     : 0)
                                  + ((rec[drec].comp && rec[drec].resolve_module_ids)
                                     ? SCHEME_RESOLVE_MODIDS
                                     : 0),
                                  env->in_modidx,
                                  &menv, NULL, NULL, NULL);
    
      if (SCHEME_STX_PAIRP(first))
        *current_val = val;

      if (!val) {
        SCHEME_EXPAND_OBSERVE_EXIT_CHECK(rec[drec].observer, first);
        return first;
      } else if (SAME_TYPE(SCHEME_TYPE(val), scheme_macro_type)) {
        if (scheme_is_rename_transformer(SCHEME_PTR_VAL(val))) {
          /* It's a rename. Look up the target name and try again. */
          name = scheme_transfer_srcloc(scheme_rename_transformer_id(SCHEME_PTR_VAL(val)),
                                        name);
          menv = NULL;
          SCHEME_USE_FUEL(1);
        } else {
          /* It's a normal macro; expand once. Also, extend env to indicate
             an internal-define position, if necessary. */
          if (!xenv) {
            if (internel_def_pos) {
              xenv = scheme_new_compilation_frame(0, SCHEME_INTDEF_FRAME, env);
              if (ctx)
                xenv->intdef_name = ctx;
              if (_xenv)
                *_xenv = xenv;
            } else
              xenv = env;
          }
          {
            scheme_init_expand_recs(rec, drec, &erec1, 1);
            erec1.depth = 1;
            erec1.value_name = (keep_name ? rec[drec].value_name : scheme_false);
            first = scheme_expand_expr(first, xenv, &erec1, 0);
          }
          break; /* break to outer loop */
        }
      } else {
        SCHEME_EXPAND_OBSERVE_EXIT_CHECK(rec[drec].observer, first);
        return first;
      }
    }
  }
}

static Scheme_Object *
compile_expand_macro_app(Scheme_Object *name, Scheme_Env *menv, Scheme_Object *macro,
			 Scheme_Object *form, Scheme_Comp_Env *env,
			 Scheme_Compile_Expand_Info *rec, int drec)
{
  Scheme_Object *xformer, *boundname;

  xformer = (Scheme_Object *)SCHEME_PTR_VAL(macro);

  if (scheme_is_set_transformer(xformer)) {
    /* scheme_apply_macro unwraps it */
  } else {
    if (!scheme_check_proc_arity(NULL, 1, 0, -1, &xformer)) {
      scheme_wrong_syntax(NULL, NULL, form, "illegal use of syntax");
      return NULL;
    }
  }

  boundname = rec[drec].value_name;
  if (!boundname)
    boundname = scheme_false;

  return scheme_apply_macro(name, menv, xformer, form, env, boundname, rec, drec, 0);

  /* caller expects rec[drec] to be used to compile the result... */
}

static int same_effective_env(Scheme_Comp_Env *orig, Scheme_Comp_Env *e)
{
  while (1) {
    if (orig == e)
      return 1;
    if (e && e->flags & SCHEME_FOR_STOPS)
      e = e->next;
    else
      return 0;
  }
}

static Scheme_Object *compile_expand_expr_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *form = (Scheme_Object *)p->ku.k.p1;
  Scheme_Comp_Env *env = (Scheme_Comp_Env *)p->ku.k.p2;
  Scheme_Compile_Info *rec = (Scheme_Compile_Info *)p->ku.k.p3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  return scheme_compile_expand_expr(form, 
				    env,
				    rec,
				    p->ku.k.i3,
				    p->ku.k.i2);
}

Scheme_Object *
scheme_compile_expand_expr(Scheme_Object *form, Scheme_Comp_Env *env, 
			   Scheme_Compile_Expand_Info *rec, int drec, 
			   int app_position)
{
  Scheme_Object *name, *var, *stx, *normal, *can_recycle_stx = NULL, *orig_unbound_name = NULL;
  Scheme_Env *menv = NULL;
  GC_CAN_IGNORE char *not_allowed;
  int looking_for_top, has_orig_unbound = 0;

 top:

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      Scheme_Compile_Expand_Info *recx;

      recx = MALLOC_ONE_RT(Scheme_Compile_Expand_Info);
      memcpy(recx, rec + drec, sizeof(Scheme_Compile_Expand_Info));
#ifdef MZTAG_REQUIRED
      recx->type = scheme_rt_compile_info;
#endif

      p->ku.k.p1 = (void *)form;
      p->ku.k.p2 = (void *)env;
      p->ku.k.p3 = (void *)recx;
      p->ku.k.i3 = 0;
      p->ku.k.i2 = app_position;

      var = scheme_handle_stack_overflow(compile_expand_expr_k);

      memcpy(rec + drec, recx, sizeof(Scheme_Compile_Expand_Info));
      return var;
    }
  }
#endif

  DO_CHECK_FOR_BREAK(scheme_current_thread, ;);

#if 1
  if (!SCHEME_STXP(form))
    scheme_signal_error("internal error: not syntax");
#endif

  if (rec[drec].comp) {
    scheme_default_compile_rec(rec, drec);
  } else {
    SCHEME_EXPAND_OBSERVE_VISIT(rec[drec].observer,form);
  }

  if (SAME_TYPE(SCHEME_TYPE(SCHEME_STX_VAL(form)), scheme_expanded_syntax_type)) {
    var = SCHEME_STX_VAL(form);
    if (scheme_stx_has_empty_wraps(form)
        && same_effective_env(SCHEME_PTR2_VAL(var), env)) {
      /* FIXME [Ryan?]: this needs EXPAND_OBSERVE callbacks. */
      form = scheme_stx_track(SCHEME_PTR1_VAL(var), form, form);
      if (!rec[drec].comp && (rec[drec].depth != -1)) {
        /* Already fully expanded. */
        return form;
      }
    } else {
      scheme_wrong_syntax(NULL, NULL, SCHEME_PTR1_VAL(var), 
                          "expanded syntax not in its original lexical context"
                          " (extra bindings or marks in the current context)");
    }
  }

  looking_for_top = 0;

  if (SCHEME_STX_NULLP(form)) {
    stx = app_symbol;
    not_allowed = "function application";
    normal = app_expander;
  } else if (!SCHEME_STX_PAIRP(form)) {
    if (SCHEME_STX_SYMBOLP(form)) {
      Scheme_Object *find_name = form, *lexical_binding_id, *inline_variant;
      int protected = 0;

      while (1) {
        lexical_binding_id = NULL;
        inline_variant = NULL;
	var = scheme_lookup_binding(find_name, env, 
				    SCHEME_NULL_FOR_UNBOUND
				    + SCHEME_ENV_CONSTANTS_OK
				    + (rec[drec].comp
				       ? SCHEME_ELIM_CONST 
				       : 0)
				    + (app_position 
				       ? SCHEME_APP_POS 
				       : 0)
				    + ((rec[drec].comp && rec[drec].dont_mark_local_use) ? 
				       SCHEME_DONT_MARK_USE 
				       : 0)
				    + ((rec[drec].comp && rec[drec].resolve_module_ids)
				       ? SCHEME_RESOLVE_MODIDS
				       : 0)
                                    + ((!rec[drec].comp && (rec[drec].depth == -2))
                                       ? (SCHEME_OUT_OF_CONTEXT_OK | SCHEME_OUT_OF_CONTEXT_LOCAL)
                                       : 0),
				    env->in_modidx, 
				    &menv, &protected, &lexical_binding_id, &inline_variant);

        SCHEME_EXPAND_OBSERVE_RESOLVE(rec[drec].observer,find_name);

	if (var && SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)
	    && scheme_is_rename_transformer(SCHEME_PTR_VAL(var))) {
	  /* It's a rename. Look up the target name and try again. */
	  Scheme_Object *new_name;
	  new_name = scheme_rename_transformer_id(SCHEME_PTR_VAL(var));
	  if (!rec[drec].comp) {
	    new_name = scheme_stx_track(new_name, find_name, find_name);
	  }
	  find_name = scheme_transfer_srcloc(new_name, find_name);
	  SCHEME_USE_FUEL(1);
	  menv = NULL;
	  protected = 0;
	} else
	  break;
      }
      
      if (!var) {
	/* Top variable */
	stx = top_symbol;
        if (env->genv->module)
          not_allowed = "reference to an unbound identifier";
        else
          not_allowed = "reference to a top-level identifier";
	normal = top_expander;
        has_orig_unbound = 1;
	form = find_name; /* in case it was re-mapped */
	looking_for_top = 1;
      } else {
	if (SAME_TYPE(SCHEME_TYPE(var), scheme_syntax_compiler_type)) {
	  if (var == stop_expander) {
            SCHEME_EXPAND_OBSERVE_ENTER_PRIM(rec[drec].observer,form);
            SCHEME_EXPAND_OBSERVE_PRIM_STOP(rec[drec].observer);
            SCHEME_EXPAND_OBSERVE_EXIT_PRIM(rec[drec].observer,form);
            SCHEME_EXPAND_OBSERVE_RETURN(rec[drec].observer,form);
	    return form;
          } else {
	    scheme_wrong_syntax(NULL, NULL, form, "bad syntax");
	    return NULL;
	  }
	} else if (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)) {
	  name = form;
	  goto macro;
	}
	
	if (rec[drec].comp) {
	  scheme_compile_rec_done_local(rec, drec);
          if (SAME_TYPE(SCHEME_TYPE(var), scheme_variable_type)) {
            if (scheme_extract_unsafe(var)) {
              return scheme_extract_unsafe(var);
            } else if (scheme_extract_flfxnum(var)) {
              return scheme_extract_flfxnum(var);
            } else if (scheme_extract_extfl(var)) {
              return scheme_extract_extfl(var);
            } else if (scheme_extract_futures(var)) {
              return scheme_extract_futures(var);
            } else if (scheme_extract_foreign(var)) {
              return scheme_extract_foreign(var);
            }
          }
          if (SAME_TYPE(SCHEME_TYPE(var), scheme_variable_type)
              || SAME_TYPE(SCHEME_TYPE(var), scheme_module_variable_type))
	    return scheme_register_toplevel_in_prefix(var, env, rec, drec, 
                                                      scheme_is_imported(var, env),
                                                      inline_variant);
	  else
	    return var;
	} else {
          SCHEME_EXPAND_OBSERVE_VARIABLE(rec[drec].observer, form, find_name);
          if (lexical_binding_id) {
            find_name = lexical_binding_id;
          }
	  if (protected) {
	    /* Add a property to indicate that the name is protected */
	    find_name = scheme_stx_property(find_name, protected_symbol, scheme_true);
	  }
          SCHEME_EXPAND_OBSERVE_RETURN(rec[drec].observer, find_name);
	  return find_name; /* which is usually == form */
	}
      }
    } else {
      /* A hack for handling lifted expressions. See compile_expand_lift_to_let. */
      if (SAME_TYPE(SCHEME_TYPE(SCHEME_STX_VAL(form)), scheme_already_comp_type)) {
	form = SCHEME_STX_VAL(form);
	return SCHEME_IPTR_VAL(form);
      }

      stx = datum_symbol;
      not_allowed = "literal data";
      normal = datum_expander;
    }
  } else {
    name = scheme_stx_taint_disarm(form, NULL);
    name = SCHEME_STX_CAR(name);
    if (SCHEME_STX_SYMBOLP(name)) {
      /* Check for macros: */
      Scheme_Object *find_name = name;
      Scheme_Expand_Info erec1;

      /* While resolving name, we used to need taints from `form' */
      scheme_init_expand_recs(rec, drec, &erec1, 1);

      while (1) {
	var = scheme_lookup_binding(find_name, env, 
				    SCHEME_APP_POS
				    + SCHEME_NULL_FOR_UNBOUND
				    + SCHEME_ENV_CONSTANTS_OK
				    + (rec[drec].comp
				       ? SCHEME_ELIM_CONST
				       : 0)
				    + SCHEME_DONT_MARK_USE
				    + ((rec[drec].comp && rec[drec].resolve_module_ids)
				       ? SCHEME_RESOLVE_MODIDS
				       : 0)
                                    + ((!rec[drec].comp && (rec[drec].depth == -2))
                                       ? (SCHEME_OUT_OF_CONTEXT_OK | SCHEME_OUT_OF_CONTEXT_LOCAL)
                                       : 0),
				    env->in_modidx, 
				    &menv, NULL, NULL, NULL);

        SCHEME_EXPAND_OBSERVE_RESOLVE(rec[drec].observer, find_name);
	if (var && SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)
	    && scheme_is_rename_transformer(SCHEME_PTR_VAL(var))) {
	  /* It's a rename. Look up the target name and try again. */
	  Scheme_Object *new_name;
	  new_name = scheme_rename_transformer_id(SCHEME_PTR_VAL(var));
	  if (!rec[drec].comp) {
	    new_name = scheme_stx_track(new_name, find_name, find_name);
	  }
          find_name = scheme_transfer_srcloc(new_name, find_name);
	  SCHEME_USE_FUEL(1);
	  menv = NULL;
	} else
	  break;
      }
      
      if (!var) {
	/* apply to global variable: compile it normally */
        orig_unbound_name = find_name;
        has_orig_unbound = 1;
      } else if (SAME_TYPE(SCHEME_TYPE(var), scheme_local_type)
		 || SAME_TYPE(SCHEME_TYPE(var), scheme_local_unbox_type)) {
	/* apply to local variable: compile it normally */
      } else {
	if (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)) {
	  goto macro;
	} else if (SAME_TYPE(SCHEME_TYPE(var), scheme_syntax_compiler_type)) {
	  if (rec[drec].comp) {
	    Scheme_Syntax *f;
	    f = (Scheme_Syntax *)SCHEME_SYNTAX(var);
	    return f(form, env, rec, drec);
	  } else {
	    Scheme_Syntax_Expander *f;
	    f = (Scheme_Syntax_Expander *)SCHEME_SYNTAX_EXP(var);
	    SCHEME_EXPAND_OBSERVE_ENTER_PRIM(rec[drec].observer, form);
	    form = f(form, env, rec, drec);
	    SCHEME_EXPAND_OBSERVE_EXIT_PRIM(rec[drec].observer, form);
	    SCHEME_EXPAND_OBSERVE_RETURN(rec[drec].observer, form);
	    return form;
	  }
	}
	
	/* Else: unknown global - must be a function: compile as application */
      }

      if (!SAME_OBJ(name, find_name)) {
	/* the rator position was mapped */
	Scheme_Object *code;
        code = scheme_stx_taint_disarm(form, NULL);
        code = SCHEME_STX_CDR(code);
	code = scheme_make_pair(find_name, code);
	form = scheme_datum_to_syntax(code, form, form, 0, 2);
      }
    }

    stx = app_symbol;
    not_allowed = "function application";
    normal = app_expander;
  }

  /* Compile/expand as application, datum, or top: */
  if (scheme_stx_is_tainted(form)) {
    stx = scheme_datum_to_syntax(stx, form, form, 0, 1);
    stx = scheme_stx_taint_rearm(stx, form);
  } else if (quick_stx && rec[drec].comp) {
    ((Scheme_Stx *)quick_stx)->val = stx;
    ((Scheme_Stx *)quick_stx)->wraps = ((Scheme_Stx *)form)->wraps;
    ((Scheme_Stx *)quick_stx)->u.modinfo_cache = NULL;
    ((Scheme_Stx *)quick_stx)->taints = NULL;
    stx = quick_stx;
    quick_stx = NULL;
  } else
    stx = scheme_datum_to_syntax(stx, scheme_false, form, 0, 0);

  if (rec[drec].comp)
    can_recycle_stx = stx;

  {
    Scheme_Object *find_name = stx;

    while (1) {
      var = scheme_lookup_binding(find_name, env,
				  SCHEME_NULL_FOR_UNBOUND
				  + SCHEME_APP_POS + SCHEME_ENV_CONSTANTS_OK
				  + SCHEME_DONT_MARK_USE
                                  + ((!rec[drec].comp && (rec[drec].depth == -2))
                                     ? (SCHEME_OUT_OF_CONTEXT_OK | SCHEME_OUT_OF_CONTEXT_LOCAL)
                                     : 0),
				  env->in_modidx, 
				  &menv, NULL, NULL, NULL);

      SCHEME_EXPAND_OBSERVE_RESOLVE(rec[drec].observer, find_name);

      if (var && SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)
	  && scheme_is_rename_transformer(SCHEME_PTR_VAL(var))) {
	/* It's a rename. Look up the target name and try again. */
	Scheme_Object *new_name;
	new_name = scheme_rename_transformer_id(SCHEME_PTR_VAL(var));
	if (!rec[drec].comp) {
	  new_name = scheme_stx_track(new_name, find_name, find_name);
	}
        find_name = scheme_transfer_srcloc(new_name, find_name);
	SCHEME_USE_FUEL(1);
	menv = NULL;
      } else
	break;
    }
  }

  if (!SAME_OBJ(var, normal)) {
    /* Someone might keep the stx: */
    can_recycle_stx = NULL;
  }

  if (!var && looking_for_top) {
    /* If form is a marked name, then force #%top binding.
       This is so temporaries can be used as defined ids. */
    Scheme_Object *nm;
    nm = scheme_tl_id_sym(env->genv, form, NULL, 0, NULL, NULL);
    if (!SAME_OBJ(nm, SCHEME_STX_VAL(form))) {
      stx = scheme_datum_to_syntax(top_symbol, scheme_false, scheme_sys_wraps(env), 0, 0);

      /* Should be either top_expander or stop_expander: */
      var = scheme_lookup_binding(stx, env,
				  SCHEME_NULL_FOR_UNBOUND
				  + SCHEME_APP_POS + SCHEME_ENV_CONSTANTS_OK
				  + SCHEME_DONT_MARK_USE
                                  + ((!rec[drec].comp && (rec[drec].depth == -2))
                                     ? (SCHEME_OUT_OF_CONTEXT_OK | SCHEME_OUT_OF_CONTEXT_LOCAL)
                                     : 0),
				  env->in_modidx, 
				  &menv, NULL, NULL, NULL);
    }
  }

  if (var && (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)
	      || SAME_TYPE(SCHEME_TYPE(var), scheme_syntax_compiler_type))) {
    if (SAME_OBJ(var, stop_expander)) {
      /* Return original: */
      SCHEME_EXPAND_OBSERVE_ENTER_PRIM(rec[drec].observer, form);
      SCHEME_EXPAND_OBSERVE_PRIM_STOP(rec[drec].observer);
      SCHEME_EXPAND_OBSERVE_EXIT_PRIM(rec[drec].observer, form);
      SCHEME_EXPAND_OBSERVE_RETURN(rec[drec].observer, form);
      return form;
    } else if (rec[drec].comp && SAME_OBJ(var, normal) && !rec[drec].observer) {
      /* Skip creation of intermediate form */
      Scheme_Syntax *f;
      rec[drec].pre_unwrapped = 1;
      f = (Scheme_Syntax *)SCHEME_SYNTAX(var);
      if (can_recycle_stx && !quick_stx) {
        quick_stx = can_recycle_stx;
        ((Scheme_Stx *)quick_stx)->val = NULL;
        ((Scheme_Stx *)quick_stx)->wraps = NULL;
        ((Scheme_Stx *)quick_stx)->u.modinfo_cache = NULL;
        ((Scheme_Stx *)quick_stx)->taints = NULL;
      }
      return f(form, env, rec, drec);
    } else {
      if (!rec[drec].comp
          && (rec[drec].depth == -2) /* local-expand */
          && SAME_OBJ(var, normal)
          && SAME_OBJ(SCHEME_STX_VAL(stx), top_symbol)) {
        rec[drec].pre_unwrapped = 1;
      } else {
        name = scheme_stx_taint_disarm(form, NULL);
        form = scheme_datum_to_syntax(scheme_make_pair(stx, name), form, form, 0, 2);
        SCHEME_EXPAND_OBSERVE_TAG(rec[drec].observer, form);
      }

      if (SAME_TYPE(SCHEME_TYPE(var), scheme_syntax_compiler_type)) {
	if (rec[drec].comp) {
	  Scheme_Syntax *f;
	  f = (Scheme_Syntax *)SCHEME_SYNTAX(var);
	  return f(form, env, rec, drec);
	} else {
	  Scheme_Syntax_Expander *f;
	  f = (Scheme_Syntax_Expander *)SCHEME_SYNTAX_EXP(var);
	  SCHEME_EXPAND_OBSERVE_ENTER_PRIM(rec[drec].observer, form);
	  form = f(form, env, rec, drec);
	  SCHEME_EXPAND_OBSERVE_EXIT_PRIM(rec[drec].observer, form);
	  SCHEME_EXPAND_OBSERVE_RETURN(rec[drec].observer, form);
	  return form;
	}
      } else {
	name = stx;
	goto macro;
      }
    }
  } else {
    /* Not allowed this context! */
    char *phase, buf[30];
    if (env->genv->phase == 0)
      phase = "";
    else if (env->genv->phase == 1)
      phase = " in the transformer environment";
    else {
      phase = buf;
      sprintf(buf, " at phase %" PRIdPTR, env->genv->phase);
    }
    if (has_orig_unbound) {
      scheme_wrong_syntax(scheme_compile_stx_string, 
                          orig_unbound_name, form, 
                          "unbound identifier%s;\n"
                          " also, no %S syntax transformer is bound",
                          phase,
                          SCHEME_STX_VAL(stx));
    } else {
      scheme_wrong_syntax(scheme_compile_stx_string, NULL, form, 
                          "%s is not allowed;\n"
                          " no %S syntax transformer is bound%s",
                          not_allowed,
                          SCHEME_STX_VAL(stx),
                          phase);
    }
    return NULL;
  }

 macro:
  if (!rec[drec].comp && !rec[drec].depth) {
    SCHEME_EXPAND_OBSERVE_RETURN(rec[drec].observer, form);
    return form; /* We've gone as deep as requested */
  }

  SCHEME_EXPAND_OBSERVE_ENTER_MACRO(rec[drec].observer, form);
  form = compile_expand_macro_app(name, menv, var, form, env, rec, drec);
  SCHEME_EXPAND_OBSERVE_EXIT_MACRO(rec[drec].observer, form);

  if (rec[drec].comp)
    goto top;
  else {
    if (rec[drec].depth > 0)
      --rec[drec].depth;
    if (rec[drec].depth)
      goto top;
    else {
      SCHEME_EXPAND_OBSERVE_RETURN(rec[drec].observer, form);
      return form;
    }
  }
}

static int arg_count(Scheme_Object *lam, Scheme_Comp_Env *env)
{
  Scheme_Object *l, *id, *form = lam;
  int cnt = 0;
  DupCheckRecord r;

  lam = scheme_stx_taint_disarm(lam, NULL);
  
  lam = SCHEME_STX_CDR(lam);
  if (!SCHEME_STX_PAIRP(lam)) return -1;

  l = SCHEME_STX_CAR(lam);

  lam = SCHEME_STX_CDR(lam);
  if (!SCHEME_STX_PAIRP(lam)) return -1;

  while (SCHEME_STX_PAIRP(lam)) { lam = SCHEME_STX_CDR(lam); }
  if (!SCHEME_STX_NULLP(lam)) return -1;
  

  scheme_begin_dup_symbol_check(&r, env);

  while (SCHEME_STX_PAIRP(l)) {
    id = SCHEME_STX_CAR(l);
    scheme_check_identifier("lambda", id, NULL, env, form);
    scheme_dup_symbol_check(&r, NULL, id, "argument", form);
    l = SCHEME_STX_CDR(l);
    cnt++;
  }
  if (!SCHEME_STX_NULLP(l)) return -1;

  return cnt;
}

static Scheme_Object *
compile_expand_app(Scheme_Object *orig_form, Scheme_Comp_Env *env, 
		   Scheme_Compile_Expand_Info *rec, int drec)
{
  Scheme_Object *form, *naya, *forms;
  int tsc;

  forms = scheme_stx_taint_disarm(orig_form, NULL);

  tsc = rec[drec].pre_unwrapped;
  rec[drec].pre_unwrapped = 0;

  if (tsc) {
    form = forms;
  } else {
    form = SCHEME_STX_CDR(forms);
    form = scheme_datum_to_syntax(form, forms, forms, 0, 0);
  }
  
  if (SCHEME_STX_NULLP(form)) {
    /* Compile/expand empty application to null list: */
    if (rec[drec].comp)
      return scheme_null;
    else
      return scheme_datum_to_syntax(icons(quote_symbol,
                                          icons(form, scheme_null)),
				    orig_form,
				    scheme_sys_wraps(env), 
				    0, 2);
  } else if (!SCHEME_STX_PAIRP(form)) {
     /* will end in error */
    if (rec[drec].comp)
      return compile_application(form, env, rec, drec);
    else {
      rec[drec].value_name = scheme_false;
      naya = scheme_expand_list(form, scheme_no_defines(env), rec, drec);
      /* naya will be prefixed and returned... */
    }
  } else if (rec[drec].comp) {
    Scheme_Object *name, *origname, *gval, *orig_rest_form, *rest_form;
    name = SCHEME_STX_CAR(form);
    origname = name;
    
    name = scheme_check_immediate_macro(name, env, rec, drec, 0, &gval, NULL, NULL, 0);

    /* look for ((lambda (x ...) ....) ....) or ((lambda x ....) ....) */
    if (SAME_OBJ(gval, scheme_lambda_syntax)) {
      Scheme_Object *argsnbody, *d_name;

      d_name = scheme_stx_taint_disarm(name, NULL);
      argsnbody = SCHEME_STX_CDR(d_name);
      if (SCHEME_STX_PAIRP(argsnbody)) {
        Scheme_Object *args, *body;

        args = SCHEME_STX_CAR(argsnbody);
        body = SCHEME_STX_CDR(argsnbody);
	  
        if (SCHEME_STX_PAIRP(body)) {
          int pl;
          pl = scheme_stx_proper_list_length(args);
          if ((pl >= 0) || SCHEME_STX_SYMBOLP(args)) {
            Scheme_Object *bindings = scheme_null, *last = NULL;
            Scheme_Object *rest;
            int al;
            
            rest = SCHEME_STX_CDR(form);
            al = scheme_stx_proper_list_length(rest);

            if ((pl < 0) || (al == pl)) {
              DupCheckRecord r;

              scheme_begin_dup_symbol_check(&r, env);
	      
              while (!SCHEME_STX_NULLP(args)) {
                Scheme_Object *v, *n;
		  
                if (pl < 0)
                  n = args;
                else
                  n = SCHEME_STX_CAR(args);
                scheme_check_identifier("lambda", n, NULL, env, name);

                /* If we don't check here, the error is in terms of `let': */
                scheme_dup_symbol_check(&r, NULL, n, "argument", name);
  
                if (pl < 0) {
                  v = scheme_intern_symbol("list");
                  v = scheme_datum_to_syntax(v, scheme_false, scheme_sys_wraps(env), 0, 0);
                  v = cons(v, rest);
                } else
                  v = SCHEME_STX_CAR(rest);
                v = cons(cons(cons(n, scheme_null), cons(v, scheme_null)), scheme_null);
                if (last)
                  SCHEME_CDR(last) = v;
                else
                  bindings = v;
		  
                last = v;
                if (pl < 0) {
                  /* rator is (lambda rest-x ....) */
                  break;
                } else {
                  args = SCHEME_STX_CDR(args);
                  rest = SCHEME_STX_CDR(rest);
                }
              }

              body = scheme_datum_to_syntax(icons(begin_symbol, body), form, 
                                            scheme_sys_wraps(env), 
                                            0, 2);
              
              body = scheme_datum_to_syntax(cons(let_values_symbol,
                                                 cons(bindings,
                                                      cons(body, scheme_null))),
                                            form, 
                                            scheme_sys_wraps(env), 
                                            0, 2);

              body = scheme_syntax_taint_rearm(body, orig_form);

              return scheme_compile_expand_expr(body, env, rec, drec, 0);
            } else {
#if 0
              scheme_wrong_syntax(scheme_application_stx_string, NULL, form, 
                                  "procedure application: bad ((lambda (...) ...) ...) syntax");
              return NULL;
#endif
            }
          }
        }
      }
    }

    orig_rest_form = SCHEME_STX_CDR(form);

    /* Look for (call-with-values (lambda () M) (lambda (id ...) N)) */ 
    if (SCHEME_STX_SYMBOLP(name)) {
      Scheme_Object *at_first, *at_second, *the_end;
      at_first = SCHEME_STX_CDR(form);
      if (SCHEME_STX_PAIRP(at_first)) {
        at_second = SCHEME_STX_CDR(at_first);
        if (SCHEME_STX_PAIRP(at_second)) {
          the_end = SCHEME_STX_CDR(at_second);
          if (SCHEME_STX_NULLP(the_end)) {
            Scheme_Object *orig_at_second = at_second;

            if (!cwv_stx || (env->genv->phase != cwv_stx_phase)) {
              cwv_stx_phase = env->genv->phase;
              cwv_stx = scheme_datum_to_syntax(call_with_values_symbol, 
                                               scheme_false, scheme_sys_wraps(env), 0, 0);
            }

            if (scheme_stx_module_eq(name, cwv_stx, 0)) {
              Scheme_Object *first, *orig_first;
              orig_first = SCHEME_STX_CAR(at_first);
              first = scheme_check_immediate_macro(orig_first, env, rec, drec, 0, &gval, NULL, NULL, 0);
              if (SAME_OBJ(gval, scheme_lambda_syntax) 
                  && SCHEME_STX_PAIRP(first)
                  && (arg_count(first, env) == 0)) {
                Scheme_Object *second, *orig_second;
                orig_second = SCHEME_STX_CAR(at_second);
                second = scheme_check_immediate_macro(orig_second, env, rec, drec, 0, &gval, NULL, NULL, 0);
                if (SAME_OBJ(gval, scheme_lambda_syntax) 
                    && SCHEME_STX_PAIRP(second)
                    && (arg_count(second, env) >= 0)) {
                  Scheme_Object *lhs, *orig_post_first, *orig_post_second;
                  orig_post_first = first;
                  orig_post_second = second;
                  first = scheme_stx_taint_disarm(first, NULL);
                  second = scheme_stx_taint_disarm(second, NULL);
                  second = SCHEME_STX_CDR(second);
                  lhs = SCHEME_STX_CAR(second);
                  second = SCHEME_STX_CDR(second);
                  first = SCHEME_STX_CDR(first);
                  first = SCHEME_STX_CDR(first);
                  first = icons(begin_symbol, first);
                  first = scheme_datum_to_syntax(first, orig_post_first, scheme_sys_wraps(env), 0, 1);
                  second = icons(begin_symbol, second);
                  second = scheme_datum_to_syntax(second, orig_post_second, scheme_sys_wraps(env), 0, 1);
                  /* Convert to let-values: */
                  name = icons(let_values_symbol,
                               icons(icons(icons(lhs, icons(first, scheme_null)), 
                                           scheme_null),
                                     icons(second, scheme_null)));
                  form = scheme_datum_to_syntax(name, forms, scheme_sys_wraps(env), 0, 2);
                  return scheme_compile_expand_expr(form, env, rec, drec, 0);
                }
                if (!SAME_OBJ(second, orig_second)) {
                  at_second = scheme_datum_to_syntax(icons(second, the_end), at_second, at_second, 0, 2);
                } 
              }
              if (!SAME_OBJ(first, orig_first)
                  || !SAME_OBJ(at_second, orig_at_second)) {
                at_first = scheme_datum_to_syntax(icons(first, at_second), at_first, at_first, 0, 2);
              }
            }
          }
        }
      }
      rest_form = at_first;
    } else {
      rest_form = orig_rest_form;
    }

    if (NOT_SAME_OBJ(name, origname)
        || NOT_SAME_OBJ(rest_form, orig_rest_form)) {
      form = scheme_datum_to_syntax(scheme_make_pair(name, rest_form), forms, forms, 0, 2);
    }
    
    return compile_application(form, env, rec, drec);
  } else {
    rec[drec].value_name = scheme_false;
    naya = scheme_expand_list(form, scheme_no_defines(env), rec, drec);
    /* naya will be prefixed returned... */
  }

  if (SAME_OBJ(form, naya))
    return orig_form;

  /* Add #%app prefix back: */
  {
    Scheme_Object *first;

    first = SCHEME_STX_CAR(forms);
    return scheme_datum_to_syntax(scheme_make_pair(first, naya), orig_form, orig_form, 0, 2);
  }
}

static Scheme_Object *
app_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return compile_expand_app(form, env, rec, drec);
}

static Scheme_Object *
app_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_APP(erec[drec].observer);
  return compile_expand_app(form, env, erec, drec);
}

static Scheme_Object *
datum_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *c, *v;

  if (rec[drec].pre_unwrapped) {
    c = form;
    rec[drec].pre_unwrapped = 0;
  } else {
    c = SCHEME_STX_CDR(form);
    /* Need datum->syntax, in case c is a list: */
    c = scheme_datum_to_syntax(c, form, form, 0, 2);
  }

  v = SCHEME_STX_VAL(c);
  if (SCHEME_KEYWORDP(v)) {
    scheme_wrong_syntax("#%datum", NULL, c, "keyword used as an expression");
    return NULL;
  }

  return scheme_syntax_to_datum(c, 0, NULL);
}

static Scheme_Object *
datum_expand(Scheme_Object *orig_form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  Scheme_Object *rest, *v, *form;

  SCHEME_EXPAND_OBSERVE_PRIM_DATUM(erec[drec].observer);

  form = scheme_stx_taint_disarm(orig_form, NULL);

  rest = SCHEME_STX_CDR(form);

  v = SCHEME_STX_VAL(rest);
  if (SCHEME_KEYWORDP(v)) {
    scheme_wrong_syntax("#%datum", NULL, rest, "keyword used as an expression");
    return NULL;
  }

  return scheme_datum_to_syntax(icons(quote_symbol,
                                      icons(rest, scheme_null)),
                                orig_form,
                                scheme_sys_wraps(env), 
                                0, 2);
}

int scheme_check_top_identifier_bound(Scheme_Object *c, Scheme_Env *genv, int disallow_unbound)
{
  Scheme_Object *symbol = c;
  Scheme_Object *modidx, *tl_id;
  int bad;
  
  tl_id = scheme_tl_id_sym(genv, symbol, NULL, 0, NULL, NULL);
  if (NOT_SAME_OBJ(tl_id, SCHEME_STX_SYM(symbol))) {
    /* Since the module has a rename for this id, count it as
       defined. This covers the unusual case that a marked identifier
       is bound in a module, but the identifier doesn't have the
       module's post_ex_rename_set in its lexical information. */
    bad = 0;
  } else {
    modidx = scheme_stx_module_name(NULL, &symbol, scheme_make_integer(genv->phase), NULL, NULL, NULL, 
                                    NULL, NULL, NULL, NULL, NULL, NULL);
    if (modidx) {
      /* If it's an access path, resolve it: */
      if (genv->module
          && SAME_OBJ(scheme_module_resolve(modidx, 1), genv->module->modname))
        bad = 0;
      else
        bad = 1;
    } else
      bad = 1;

    if (disallow_unbound) {
      if (bad || !scheme_lookup_in_table(genv->toplevel, (const char *)SCHEME_STX_SYM(c))) {
        GC_CAN_IGNORE const char *reason;
        if (genv->phase == 1) {
          reason = "unbound identifier in module (in phase 1, transformer environment)";
          /* Check in the run-time environment */
          if (scheme_lookup_in_table(genv->template_env->toplevel, (const char *)SCHEME_STX_SYM(c))) {
            reason = ("unbound identifier in module (in the transformer environment, which does"
                      " not include the run-time definition)");
          } else if (genv->template_env->syntax
                     && scheme_lookup_in_table(genv->template_env->syntax, (const char *)SCHEME_STX_SYM(c))) {
            reason = ("unbound identifier in module (in the transformer environment, which does"
                      " not include the macro definition that is visible to run-time expressions)");
          }
        } else if (genv->phase == 0)
          reason = "unbound identifier in module";
        else
          reason = "unbound identifier in module (in phase %d)";
        scheme_unbound_syntax(scheme_expand_stx_string, NULL, c, reason, genv->phase);
      }
    }
  }

  return !bad;
}

static Scheme_Object *check_top(Scheme_Object *orig_form, 
                                Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec,
                                int *_need_bound_check)
{
  Scheme_Object *c, *form;

  form = scheme_stx_taint_disarm(orig_form, NULL);

  if (rec[drec].pre_unwrapped) {
    c = form;
    rec[drec].pre_unwrapped = 0;
  } else
    c = SCHEME_STX_CDR(form);

  if (!SCHEME_STX_SYMBOLP(c))
    scheme_wrong_syntax(NULL, NULL, form, NULL);

  if (env->genv->module) {
    int bad;
    bad = !scheme_check_top_identifier_bound(c, env->genv, env->genv->disallow_unbound > 0);
    if (_need_bound_check)
      *_need_bound_check = bad;
  }

  return c;
}

static Scheme_Object *
top_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *c;
  int need_bound_check = 0;

  c = check_top(form, env, rec, drec, &need_bound_check);

  if (need_bound_check)
    scheme_register_unbound_toplevel(env, c);

  c = scheme_tl_id_sym(env->genv, c, NULL, 0, NULL, NULL);

  if (env->genv->module && !rec[drec].resolve_module_ids) {
    /* Self-reference in a module; need to remember the modidx.  Don't
       need a pos, because the symbol's gensym-ness (if any) will be
       preserved within the module. */
    c = scheme_hash_module_variable(env->genv, env->genv->module->self_modidx, 
				    c, env->genv->module->insp,
				    -1, env->genv->mod_phase, 0,
                                    NULL);
  } else {
    c = (Scheme_Object *)scheme_global_bucket(c, env->genv);
  }

  return scheme_register_toplevel_in_prefix(c, env, rec, drec, 0, NULL);
}

static Scheme_Object *
top_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  Scheme_Object *c;
  int need_bound_check = 0;

  SCHEME_EXPAND_OBSERVE_PRIM_TOP(erec[drec].observer);
  c = check_top(form, env, erec, drec, &need_bound_check);

  if (env->genv->module)
    return c; /* strip `#%top' prefix */

  return form;
}

Scheme_Object *scheme_compile_expr(Scheme_Object *form, Scheme_Comp_Env *env, 
				   Scheme_Compile_Info *rec, int drec)
{
  return scheme_compile_expand_expr(form, env, rec, drec, 0);
}

Scheme_Object *scheme_expand_expr(Scheme_Object *form, Scheme_Comp_Env *env, 
				  Scheme_Expand_Info *erec, int drec)
{
  return scheme_compile_expand_expr(form, env, erec, drec, 0);
}

Scheme_Object *scheme_pair_lifted(Scheme_Object *_ip, Scheme_Object **_ids, Scheme_Object *expr, Scheme_Comp_Env *env)
{
  Scheme_Comp_Env **ip = (Scheme_Comp_Env **)_ip, *naya;
  Scheme_Object *ids, *id;
  int pos;

  pos = scheme_list_length(*_ids);
  naya = scheme_new_compilation_frame(pos, SCHEME_CAPTURE_LIFTED, (*ip)->next);
  (*ip)->next = naya;
  *ip = naya;

  for (ids = *_ids; !SCHEME_NULLP(ids); ids = SCHEME_CDR(ids)) {
    id = SCHEME_CAR(ids);
    scheme_add_compilation_binding(--pos, id, naya);
  }

  return icons(*_ids, icons(expr, scheme_null));
}

Scheme_Object *scheme_add_lifts_as_let(Scheme_Object *obj, Scheme_Object *l, Scheme_Comp_Env *env,
                                       Scheme_Object *orig_form, int comp_rev)
{
  Scheme_Object *revl, *a;

  if (SCHEME_NULLP(l)) return obj;

  revl = scheme_reverse(l);

  if (comp_rev) {
    /* We've already compiled the body of this let
       with the bindings in reverse order. So insert a series of `lets'
       to match that order: */
    if (!SCHEME_NULLP(SCHEME_CDR(l))) {
      for (; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
        a = scheme_reverse(SCHEME_CAR(SCHEME_CAR(l)));
        for (; !SCHEME_NULLP(a); a = SCHEME_CDR(a)) {
          obj = icons(scheme_datum_to_syntax(let_values_symbol, scheme_false, scheme_sys_wraps(env), 0, 0),
                      icons(icons(icons(icons(SCHEME_CAR(a), scheme_null), icons(SCHEME_CAR(a), scheme_null)),
                                  scheme_null),
                            icons(obj, scheme_null)));
        }
      }
    }
  }

  for (; SCHEME_PAIRP(revl); revl = SCHEME_CDR(revl)) {
    a = SCHEME_CAR(revl);
    obj = icons(scheme_datum_to_syntax(let_values_symbol, scheme_false, scheme_sys_wraps(env), 0, 0),
                icons(icons(a, scheme_null),
                      icons(obj, scheme_null)));
  }

  obj = scheme_datum_to_syntax(obj, orig_form, scheme_false, 0, 0);
  
  return obj;
}
 
static Scheme_Object *compile_expand_expr_lift_to_let_k(void);

static Scheme_Object *
compile_expand_expr_lift_to_let(Scheme_Object *form, Scheme_Comp_Env *env,
				Scheme_Expand_Info *rec, int drec)
{
  Scheme_Expand_Info recs[2];
  Scheme_Object *l, *orig_form = form, *context_key;
  Scheme_Comp_Env *inserted, **ip;

  /* This function only works when `env' has no lexical bindings,
     because we might insert new ones at the beginning.  In
     particular, we might insert frames between `inserted' and
     `env'.

     This function also relies on the way that compilation of `let'
     works. A let-bound variable is compiled to a count of the frames
     to skip and the index within the frame, so we can insert new
     frames without affecting lookups computed so far. Inserting each
     new frame before any previous one turns out to be consistent with
     the nested `let's that we generate at the end. 

     Some optimizations can happen later, for example constant
     propagate.  But these optimizations take place on the result of
     this function, so we don't have to worry about them.  

     Don't generate a `let*' expression instead of nested `let's,
     because the compiler actually takes shortcuts (that are
     inconsistent with our frame nesting) instead of expanding `let*'
     to `let'. */

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      Scheme_Compile_Expand_Info *recx;

      recx = MALLOC_ONE_RT(Scheme_Compile_Expand_Info);
      memcpy(recx, rec + drec, sizeof(Scheme_Compile_Expand_Info));
#ifdef MZTAG_REQUIRED
      recx->type = scheme_rt_compile_info;
#endif

      p->ku.k.p1 = (void *)form;
      p->ku.k.p2 = (void *)env;
      p->ku.k.p3 = (void *)recx;

      form = scheme_handle_stack_overflow(compile_expand_expr_lift_to_let_k);

      memcpy(rec + drec, recx, sizeof(Scheme_Compile_Expand_Info));
      return form;
    }
  }
#endif

  inserted = scheme_new_compilation_frame(0, 0, env);

  ip = MALLOC_N(Scheme_Comp_Env *, 1);
  *ip = inserted;

  context_key = scheme_generate_lifts_key();
  
  scheme_frame_captures_lifts(inserted, scheme_pair_lifted, (Scheme_Object *)ip, scheme_false, 
                              context_key, NULL, scheme_false);

  if (rec[drec].comp) {
    scheme_init_compile_recs(rec, drec, recs, 2);
    form = scheme_compile_expr(form, inserted, recs, 0);
  } else {
    scheme_init_expand_recs(rec, drec, recs, 2);
    form = scheme_expand_expr(form, inserted, recs, 0);
  }

  l = scheme_frame_get_lifts(inserted);
  if (SCHEME_NULLP(l)) {
    /* No lifts */
    if (rec[drec].comp)
      scheme_merge_compile_recs(rec, drec, recs, 1);
    return form;
  } else {
    /* We have lifts, so add let* wrapper and go again */
    Scheme_Object *o;
    if (rec[drec].comp) {
      /* Wrap compiled part so the compiler recognizes it later: */
      o = scheme_alloc_object();
      o->type = scheme_already_comp_type;
      SCHEME_IPTR_VAL(o) = form;
    } else
      o = form;
    form = scheme_add_lifts_as_let(o, l, env, orig_form, rec[drec].comp);
    SCHEME_EXPAND_OBSERVE_LETLIFT_LOOP(rec[drec].observer, form);
    form = compile_expand_expr_lift_to_let(form, env, recs, 1);
    if (rec[drec].comp)
      scheme_merge_compile_recs(rec, drec, recs, 2);
    return form;
  }
}

static Scheme_Object *compile_expand_expr_lift_to_let_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *form = (Scheme_Object *)p->ku.k.p1;
  Scheme_Comp_Env *env = (Scheme_Comp_Env *)p->ku.k.p2;
  Scheme_Compile_Info *rec = (Scheme_Compile_Info *)p->ku.k.p3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  return compile_expand_expr_lift_to_let(form, env, rec, 0);
}

Scheme_Object *
scheme_compile_expr_lift_to_let(Scheme_Object *form, Scheme_Comp_Env *env,
				Scheme_Compile_Info *rec, int drec)
{
  return compile_expand_expr_lift_to_let(form, env, rec, drec);
}

Scheme_Object *
scheme_expand_expr_lift_to_let(Scheme_Object *form, Scheme_Comp_Env *env,
			       Scheme_Expand_Info *erec, int drec)
{
  return compile_expand_expr_lift_to_let(form, env, erec, drec);
}

static Scheme_Object *beginify(Scheme_Comp_Env *env, Scheme_Object *lst)
{
  return scheme_datum_to_syntax(scheme_make_pair(begin_symbol, lst),
                                lst, 
                                scheme_sys_wraps(env), 
                                0, 0);
}

static Scheme_Object *
compile_expand_block(Scheme_Object *forms, Scheme_Comp_Env *env, 
                     Scheme_Compile_Expand_Info *rec, int drec,
                     int mixed)
/* This ugly code parses a block of code, transforming embedded
   define-values and define-syntax into letrec and letrec-syntax.
   It is espcailly ugly because we have to expand macros
   before deciding what we have. */
{
  Scheme_Object *first, *rib, *ctx, *ectx, *orig = forms, *pre_exprs = scheme_null;
  void **d;
  Scheme_Comp_Env *xenv = NULL;
  Scheme_Compile_Info recs[2];
  DupCheckRecord r;

  if (rec[drec].comp) {
    scheme_default_compile_rec(rec, drec);
  } else {
    SCHEME_EXPAND_OBSERVE_ENTER_BLOCK(rec[drec].observer, forms);
  }

  if (SCHEME_STX_NULLP(forms)) {
    if (rec[drec].comp) {
      scheme_compile_rec_done_local(rec, drec);
      return scheme_null;
    } else {
      SCHEME_EXPAND_OBSERVE_BLOCK_TO_LIST(rec[drec].observer, forms);
      SCHEME_EXPAND_OBSERVE_ENTER_LIST(rec[drec].observer, forms);
      SCHEME_EXPAND_OBSERVE_EXIT_LIST(rec[drec].observer, forms);
      return forms;
    }
  }

  rib = scheme_make_rename_rib();
  ctx = scheme_alloc_object();
  ctx->type = scheme_intdef_context_type;
  d = MALLOC_N(void*, 3);
  d[0] = env;
  SCHEME_PTR1_VAL(ctx) = d;
  SCHEME_PTR2_VAL(ctx) = rib;
  ectx = scheme_make_pair(scheme_make_struct_instance(scheme_liberal_def_ctx_type, 0, NULL), 
                          scheme_null);
  scheme_begin_dup_symbol_check(&r, env);

 try_again:

  SCHEME_EXPAND_OBSERVE_NEXT(rec[drec].observer);

  if (!SCHEME_STX_PAIRP(forms)) {
    scheme_wrong_syntax(scheme_begin_stx_string, NULL, beginify(env, forms), "bad syntax");
    return NULL;
  }

  first = SCHEME_STX_CAR(forms);

  {
    /* Need to send both parts (before & after) of block rename */
    Scheme_Object *old_first;

    old_first = first;
    first = scheme_add_rename_rib(first, rib);
    
    SCHEME_EXPAND_OBSERVE_BLOCK_RENAMES(rec[drec].observer,old_first,first);
  }

  {
    Scheme_Object *gval, *result;
    int more = 1, is_last;

    is_last = SCHEME_STX_NULLP(SCHEME_STX_CDR(forms));

    result = forms;

    /* Check for macro expansion, which could mask the real
       define-values, define-syntax, etc.: */
    first = scheme_check_immediate_macro(first, env, rec, drec, 1, &gval, &xenv, ectx, is_last);
    
    if (SAME_OBJ(gval, scheme_begin_syntax)) {
      /* Inline content */
      Scheme_Object *orig_forms = forms;

      SCHEME_EXPAND_OBSERVE_PRIM_BEGIN(rec[drec].observer);

      /* FIXME: Redundant with check done by scheme_flatten_begin below? */
      if (scheme_stx_proper_list_length(first) < 0)
	scheme_wrong_syntax(scheme_begin_stx_string, NULL, first, 
			    IMPROPER_LIST_FORM);

      forms = SCHEME_STX_CDR(forms);

      if (SCHEME_STX_NULLP(forms)) {
	/* A `begin' that ends the block.  An `inferred-name' property
	   attached to this begin should apply to the ultimate last
	   thing in the block. */
	Scheme_Object *v;
	v = scheme_check_name_property(first, rec[drec].value_name);
	rec[drec].value_name = v;
      }

      forms = scheme_flatten_begin(first, forms);

      SCHEME_EXPAND_OBSERVE_SPLICE(rec[drec].observer, forms);

      if (SCHEME_STX_NULLP(forms)) {
        if (!SCHEME_PAIRP(pre_exprs)) {
          scheme_wrong_syntax(scheme_begin_stx_string, NULL, first, 
                              "empty form is not allowed");
          return NULL;
        } else {
          /* fall through to handle expressions without definitions */
        }
      } else {
        forms = scheme_datum_to_syntax(forms, orig_forms, orig_forms, 0, 0);
        
        goto try_again;
      }

      forms = scheme_datum_to_syntax(forms, orig_forms, orig_forms, 0, 0);
    } else if (SAME_OBJ(gval, scheme_define_values_syntax)
	       || SAME_OBJ(gval, scheme_define_syntaxes_syntax)) {
      /* Turn defines into a letrec: */
      Scheme_Object *var, *vars, *v, *link;
      Scheme_Object *l = scheme_null, *start = NULL;
      Scheme_Object *stx_l = scheme_null, *stx_start = NULL;
      int is_val;

      while (1) {
	int cnt;

        if (!SCHEME_NULLP(pre_exprs)) {
          Scheme_Object *begin_stx, *values_app_stx, *exp_mark;

          pre_exprs = scheme_reverse(pre_exprs);

          exp_mark = scheme_new_mark();

          begin_stx = scheme_datum_to_syntax(begin_symbol, 
                                             scheme_false, 
                                             scheme_sys_wraps(env), 
                                             0, 0);
          begin_stx = scheme_add_remove_mark(begin_stx, exp_mark);
          values_app_stx = scheme_datum_to_syntax(scheme_make_pair(values_symbol, scheme_null),
                                                  scheme_false, 
                                                  scheme_sys_wraps(env), 
                                                  0, 0);
          values_app_stx = scheme_add_remove_mark(values_app_stx, exp_mark);

          while (SCHEME_PAIRP(pre_exprs)) {
            v = scheme_make_pair(scheme_null,
                                 scheme_make_pair(scheme_make_pair(begin_stx,
                                                                   scheme_make_pair(SCHEME_CAR(pre_exprs),
                                                                                    scheme_make_pair(values_app_stx,
                                                                                                     scheme_null))),
                                                  scheme_null));
            v = scheme_datum_to_syntax(v, SCHEME_CAR(pre_exprs), SCHEME_CAR(pre_exprs), 0, 0);
          
            link = scheme_make_pair(v, scheme_null);
            if (!start)
              start = link;
            else
              SCHEME_CDR(l) = link;
            l = link;

            pre_exprs = SCHEME_CDR(pre_exprs);
          }
        }

	is_val = SAME_OBJ(gval, scheme_define_values_syntax);
	
	v = SCHEME_STX_CDR(first);

        if (is_val) {
          SCHEME_EXPAND_OBSERVE_PRIM_DEFINE_VALUES(rec[drec].observer);
        } else {
          SCHEME_EXPAND_OBSERVE_PRIM_DEFINE_SYNTAXES(rec[drec].observer);
        }
	
	if (!SCHEME_STX_PAIRP(v))
	  scheme_wrong_syntax(NULL, NULL, first, 
			      IMPROPER_LIST_FORM);

	var = NULL;
	vars = SCHEME_STX_CAR(v);
	cnt = 0;
	while (SCHEME_STX_PAIRP(vars)) {
	  var = SCHEME_STX_CAR(vars);
	  if (!SCHEME_STX_SYMBOLP(var))
	    scheme_wrong_syntax(NULL, var, first, 
				"name must be an identifier");
	  /* scheme_dup_symbol_check(&r, "internal definition", var, "binding", first); */
	  vars = SCHEME_STX_CDR(vars);
	  cnt++;
	}
	if (!SCHEME_STX_NULLP(vars)) {
	  vars = SCHEME_STX_CAR(v);
	  scheme_wrong_syntax(NULL, vars, first, 
			      "not a sequence of identifiers");
	}

	/* Preserve properties and track at the clause level: */
	v = scheme_datum_to_syntax(v, first, first, 0, 0);
	var = SCHEME_STX_CAR(first);
	v = scheme_stx_track(v, first, var);

        SCHEME_EXPAND_OBSERVE_RENAME_ONE(rec[drec].observer,v);

	link = scheme_make_pair(v, scheme_null);
	if (is_val) {
	  if (!start)
	    start = link;
	  else
	    SCHEME_CDR(l) = link;
	  l = link;
	} else {
	  if (!stx_start)
	    stx_start = link;
	  else
	    SCHEME_CDR(stx_l) = link;
	  stx_l = link;
	}

	result = SCHEME_STX_CDR(result);
	if (!SCHEME_STX_NULLP(result) && !SCHEME_STX_PAIRP(result))
	  scheme_wrong_syntax(NULL, NULL, first, NULL);

	{
	  /* Execute internal macro definition and register non-macros */
	  Scheme_Comp_Env *new_env;
	  Scheme_Object *names, *expr, *l, *a;
	  int pos;

	  new_env = scheme_new_compilation_frame(0, SCHEME_FOR_INTDEF, env);

	  names = SCHEME_STX_CAR(v);
	  expr = SCHEME_STX_CDR(v);
	  if (!SCHEME_STX_PAIRP(expr)) {
	    if (SCHEME_STX_NULLP(expr))
	      scheme_wrong_syntax(NULL, NULL, first, 
				  "missing expression");
	    else
	      scheme_wrong_syntax(NULL, NULL, first, 
				  IMPROPER_LIST_FORM);
	  }
	  link = SCHEME_STX_CDR(expr);
	  if (!SCHEME_STX_NULLP(link)) {
	    scheme_wrong_syntax(NULL, NULL, first, 
				"extra data after expression");
	  }
	  expr = SCHEME_STX_CAR(expr);
	  
	  scheme_add_local_syntax(cnt, new_env);

	  /* Initialize environment slots to #f, which means "not syntax". */
	  cnt = 0;
	  for (l = names; SCHEME_STX_PAIRP(l); l = SCHEME_STX_CDR(l)) {
	    a = SCHEME_STX_CAR(l);
	    scheme_set_local_syntax(cnt++, a, scheme_false, new_env);
	  }

	  /* Extend shared rib with renamings */
	  scheme_add_env_renames(rib, new_env, env);

          /* Check for duplicates after extending the rib with renamings,
             since the renamings properly track marks. */
          for (l = names; SCHEME_STX_PAIRP(l); l = SCHEME_STX_CDR(l)) {
	    a = SCHEME_STX_CAR(l);
            scheme_dup_symbol_check(&r, "internal definition", a, "binding", first);
          }

	  if (!is_val) {
	    /* Evaluate and bind syntaxes */
            SCHEME_EXPAND_OBSERVE_PREPARE_ENV(rec[drec].observer);
	    scheme_prepare_exp_env(new_env->genv);
            scheme_prepare_compile_env(new_env->genv->exp_env);
	    pos = 0;
	    expr = scheme_add_rename_rib(expr, rib);
	    scheme_bind_syntaxes("local syntax definition", 
				 names, expr,
				 new_env->genv->exp_env, new_env->insp, rec, drec,
				 new_env, new_env,
				 &pos, rib);
	  }

	  /* Remember extended environment */
	  ((void **)SCHEME_PTR1_VAL(ctx))[0] = new_env;
	  env = new_env;
	  xenv = NULL;
	}

      define_try_again:
	if (!SCHEME_STX_NULLP(result)) {
	  first = SCHEME_STX_CAR(result);
	  first = scheme_datum_to_syntax(first, forms, forms, 0, 0);
          {
            Scheme_Object *old_first;
            old_first = first;
            first = scheme_add_rename_rib(first, rib);
            SCHEME_EXPAND_OBSERVE_NEXT(rec[drec].observer);
            SCHEME_EXPAND_OBSERVE_BLOCK_RENAMES(rec[drec].observer,old_first,first);
          }
          is_last = SCHEME_STX_NULLP(SCHEME_STX_CDR(result));
	  first = scheme_check_immediate_macro(first, env, rec, drec, 1, &gval, &xenv, ectx, is_last);
	  more = 1;
	  if (NOT_SAME_OBJ(gval, scheme_define_values_syntax)
	      && NOT_SAME_OBJ(gval, scheme_define_syntaxes_syntax)) {
	    if (SAME_OBJ(gval, scheme_begin_syntax)) {
	      /* Inline content */
	      result = SCHEME_STX_CDR(result);
              SCHEME_EXPAND_OBSERVE_PRIM_BEGIN(rec[drec].observer);
	      result = scheme_flatten_begin(first, result);
	      SCHEME_EXPAND_OBSERVE_SPLICE(rec[drec].observer,result);
              goto define_try_again;
	    } else if (mixed) {
              /* accumulate expr for either sequence after definitions
                 or made-up empty bindings before the next definition */
              pre_exprs = scheme_make_pair(first, pre_exprs);
              result = SCHEME_STX_CDR(result);
              goto define_try_again;
            } else {
	      /* Keep partially expanded `first': */
	      result = SCHEME_STX_CDR(result);
	      result = scheme_make_pair(first, result);
	      break;
	    }
	  }
	} else
	  break;
      }

      if (SCHEME_STX_PAIRP(result) || SCHEME_PAIRP(pre_exprs)) {
	if (!start)
	  start = scheme_null;

        if (SCHEME_PAIRP(pre_exprs))
          result = scheme_reverse(pre_exprs); /* from mixed mode */

        if (!mixed) {
          result = scheme_make_pair(scheme_make_pair(scheme_intern_symbol("#%stratified-body"),
                                                     result),
                                    scheme_null);
        }
        
	if (stx_start || (mixed && !rec[drec].comp && (rec[drec].depth != -1))) {
	  result = scheme_make_pair(letrec_syntaxes_symbol,
                                    scheme_make_pair((stx_start ? stx_start : scheme_null),
                                                     scheme_make_pair(start, result)));
	} else {
	  result = scheme_make_pair(letrec_values_symbol, scheme_make_pair(start, result));
	}
	result = scheme_datum_to_syntax(result, forms, scheme_sys_wraps(env), 0, 2);
	result = scheme_add_rename_rib(result, rib);

	more = 0;
      } else {
	/* Empty body: illegal. */
	scheme_wrong_syntax(scheme_begin_stx_string, NULL, beginify(env, orig), 
			    "no expression after a sequence of internal definitions");
      }
    } else if (mixed) {
      /* accumulate expr for either an expr-only sequence or made-up
         empty bindings before a definition that appears later */
      pre_exprs = scheme_make_pair(first, pre_exprs);
      first = SCHEME_STX_CDR(forms);
      forms = scheme_datum_to_syntax(first, forms, forms, 0, 0);
      if (SCHEME_STX_NULLP(forms)) {
        /* fall through to handle expressions without definitions */
      } else {
        goto try_again;
      }
    } else {
      /* fall through to handle just expressions in non-mixed mode */
    }

    if (!more) {
      /* We've converted to a letrec or letrec-values+syntaxes */
      scheme_stx_seal_rib(rib);
      rec[drec].env_already = (mixed ? 2 : 1);

      if (rec[drec].comp) {
	result = scheme_compile_expr(result, env, rec, drec);
        return scheme_make_pair(result, scheme_null);
      } else {
        if (!mixed && ((rec[drec].depth == -2) || (rec[drec].depth > 0))) {
          if (SAME_OBJ(letrec_syntaxes_symbol, SCHEME_STX_VAL(SCHEME_CAR(SCHEME_STX_VAL(result)))))
            result = force_traditional_letrec(result, env);
        }
        if (rec[drec].depth > 0)
          --rec[drec].depth;
	if (rec[drec].depth) {
          SCHEME_EXPAND_OBSERVE_BLOCK_TO_LETREC(rec[drec].observer,
                                                scheme_make_pair(result, scheme_null));
          result = scheme_expand_expr(result, env, rec, drec);
        }
        result = scheme_make_pair(result, scheme_null);
        return scheme_datum_to_syntax(result, forms, forms, 0, 0);
      }
    }
  }

  scheme_stx_seal_rib(rib);

  if (SCHEME_PAIRP(pre_exprs))
    pre_exprs = scheme_reverse(pre_exprs);

  if (rec[drec].comp) {
    Scheme_Object *vname, *rest;

    vname = rec[drec].value_name;
    scheme_compile_rec_done_local(rec, drec);
    scheme_init_compile_recs(rec, drec, recs, 2);

    if (SCHEME_NULLP(pre_exprs))
      rest = SCHEME_STX_CDR(forms);
    else {
      first = SCHEME_CAR(pre_exprs);
      rest = SCHEME_CDR(pre_exprs);
    }

    if (SCHEME_STX_NULLP(rest))
      recs[0].value_name = vname;
    else
      recs[1].value_name = vname;

    rest = scheme_datum_to_syntax(rest, orig, orig, 0, 0);

    first = scheme_compile_expr(first, env, recs, 0);

    forms = scheme_compile_list(rest, env, recs, 1);
    
    scheme_merge_compile_recs(rec, drec, recs, 2);
    return scheme_make_pair(first, forms);
  } else {
    Scheme_Object *newforms, *vname;

    vname = rec[drec].value_name;
    rec[drec].value_name = scheme_false;
    scheme_init_expand_recs(rec, drec, recs, 2);

    recs[0].value_name = vname;

    if (SCHEME_PAIRP(pre_exprs))
      newforms = pre_exprs;
    else {
      newforms = SCHEME_STX_CDR(forms);
      newforms = scheme_make_pair(first, newforms);
    }

    forms = scheme_datum_to_syntax(newforms, orig, orig, 0, -1);
    
    if (scheme_stx_proper_list_length(forms) < 0)
      scheme_wrong_syntax(scheme_begin_stx_string, NULL, beginify(env, forms), "bad syntax");
    
    SCHEME_EXPAND_OBSERVE_BLOCK_TO_LIST(rec[drec].observer, forms);
    forms = scheme_expand_list(forms, env, recs, 0);
    return forms;
  }
}

static Scheme_Object *
compile_block(Scheme_Object *forms, Scheme_Comp_Env *env, 
              Scheme_Compile_Info *rec, int drec)
{
  return compile_expand_block(forms, env, rec, drec, 1);
}

static Scheme_Object *
expand_block(Scheme_Object *forms, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  return compile_expand_block(forms, env, erec, drec, 1);
}

static Scheme_Object *
compile_stratified_block(Scheme_Object *forms, Scheme_Comp_Env *env, 
                         Scheme_Compile_Info *rec, int drec)
{
  return compile_expand_block(forms, env, rec, drec, 0);
}

static Scheme_Object *
expand_stratified_block(Scheme_Object *forms, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  return compile_expand_block(forms, env, erec, drec, 0);
}

Scheme_Object *
scheme_expand_list(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  Scheme_Object *first = NULL, *last = NULL, *fm;

  SCHEME_EXPAND_OBSERVE_ENTER_LIST(erec[drec].observer, form);

  if (SCHEME_STX_NULLP(form)) {
    SCHEME_EXPAND_OBSERVE_EXIT_LIST(erec[drec].observer, form);
    return scheme_null;
  }

  if (scheme_stx_proper_list_length(form) < 0) {
    /* This is already checked for anything but application */
    scheme_wrong_syntax(scheme_application_stx_string, NULL, form, 
			IMPROPER_LIST_FORM);
  }

  fm = form;
  while (SCHEME_STX_PAIRP(fm)) {
    Scheme_Object *r, *p;
    Scheme_Expand_Info erec1;

    SCHEME_EXPAND_OBSERVE_NEXT(erec[drec].observer);

    p = SCHEME_STX_CDR(fm);
    
    scheme_init_expand_recs(erec, drec, &erec1, 1);
    erec1.value_name = (SCHEME_STX_NULLP(p) ? erec[drec].value_name : scheme_false);

    r = SCHEME_STX_CAR(fm);
    r = scheme_expand_expr(r, env, &erec1, 0);
    p = scheme_make_pair(r, scheme_null);
    if (last)
      SCHEME_CDR(last) = p;
    else
      first = p;
    last = p;

    fm = SCHEME_STX_CDR(fm);
  }

  form = scheme_datum_to_syntax(first, form, form, 0, 0);
  SCHEME_EXPAND_OBSERVE_EXIT_LIST(erec[drec].observer, form);
  return form;
}


Scheme_Object *
scheme_flatten_begin(Scheme_Object *expr, Scheme_Object *append_onto)
{
  Scheme_Object *l, *ll, *a, *name, *body;
  
  if (scheme_stx_proper_list_length(expr) < 0)
    scheme_wrong_syntax(NULL, NULL, expr, IMPROPER_LIST_FORM);

  name = SCHEME_STX_CAR(expr);
  body = SCHEME_STX_CDR(expr);

  /* Extract body of `begin' and add tracking information */
  l = scheme_copy_list(scheme_flatten_syntax_list(body, NULL));
  for (ll = l; !SCHEME_NULLP(ll); ll = SCHEME_CDR(ll)) {
    a = SCHEME_CAR(ll);
    a = scheme_stx_track(a, expr, name);
    SCHEME_CAR(ll) = a;
  }
  
  return scheme_append(l, append_onto);
}

/**********************************************************************/
/*                          stop expander                             */
/**********************************************************************/

static Scheme_Object *stop_syntax(Scheme_Object *form, Scheme_Comp_Env *env, 
				  Scheme_Compile_Info *rec, int drec)
{
  scheme_signal_error("internal error: shouldn't get to stop syntax");
  return NULL;
}

static Scheme_Object *stop_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_STOP(erec[drec].observer);
  return form;
}

Scheme_Object *scheme_get_stop_expander(void)
{
  return stop_expander;
}

void scheme_add_core_stop_form(int pos, Scheme_Object *sym, Scheme_Comp_Env *env)
{
  Scheme_Object *stx;
  stx = scheme_datum_to_syntax(sym, scheme_false, scheme_sys_wraps(env), 0, 0);
  scheme_set_local_syntax(pos, stx, stop_expander, env);
}

/**********************************************************************/
/*                            precise GC                              */
/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_compile.inc"

static void register_traversers(void)
{
}

END_XFORM_SKIP;

#endif
