/*
  MzScheme
  Copyright (c) 2004-2007 PLT Scheme Inc.
  Copyright (c) 2000-2001 Matthew Flatt
 
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

/* This file implements the first-order, top-level module system. An
   initiantiated module is implemented essentially as a namespace. The
   bindings at the top level of a module are namespace top-level
   bindings. */

#include "schpriv.h"
#include "schmach.h"
#include "schexpobs.h"

/* globals */
Scheme_Object *scheme_sys_wraps0;
Scheme_Object *scheme_sys_wraps1;
Scheme_Object *(*scheme_module_demand_hook)(int, Scheme_Object **);

/* locals */
static Scheme_Object *current_module_name_resolver(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_module_name_prefix(int argc, Scheme_Object *argv[]);
static Scheme_Object *dynamic_require_for_syntax(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_require(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_trans_require(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_require_copy(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_require_etonly(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_attach_module(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_unprotect_module(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_name(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_imports(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_exports(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_to_namespace(int argc, Scheme_Object *argv[]);

static Scheme_Object *module_path_index_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_path_index_resolve(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_path_index_split(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_path_index_join(int argc, Scheme_Object *argv[]);

static Scheme_Object *module_export_protected_p(int argc, Scheme_Object **argv);

static Scheme_Object *module_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *module_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *module_begin_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *module_begin_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);

static Scheme_Object *require_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *require_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *require_for_syntax_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *require_for_syntax_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *require_for_template_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *require_for_template_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *require_for_label_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *require_for_label_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *provide_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *provide_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);

static Scheme_Object *module_execute(Scheme_Object *data);
static Scheme_Object *top_level_require_execute(Scheme_Object *data);

static Scheme_Object *module_jit(Scheme_Object *data);
static Scheme_Object *top_level_require_jit(Scheme_Object *data);

static Scheme_Object *module_optimize(Scheme_Object *data, Optimize_Info *info);
static Scheme_Object *module_resolve(Scheme_Object *data, Resolve_Info *info);
static Scheme_Object *top_level_require_optimize(Scheme_Object *data, Optimize_Info *info);
static Scheme_Object *top_level_require_resolve(Scheme_Object *data, Resolve_Info *info);

static void module_validate(Scheme_Object *data, Mz_CPort *port, 
                            char *stack, Scheme_Hash_Table *ht, Scheme_Object **tls,
                            int depth, int letlimit, int delta, 
			    int num_toplevels, int num_stxes, int num_lifts);
static void top_level_require_validate(Scheme_Object *data, Mz_CPort *port, 
                                       char *stack, Scheme_Hash_Table *ht, Scheme_Object **tls,
                                       int depth, int letlimit, int delta, 
				       int num_toplevels, int num_stxes, int num_lifts);

static Scheme_Object *write_module(Scheme_Object *obj);
static Scheme_Object *read_module(Scheme_Object *obj);

static Scheme_Module *module_load(Scheme_Object *modname, Scheme_Env *env, const char *who);

static void eval_defmacro(Scheme_Object *names, int count,
			  Scheme_Object *expr, 
			  Scheme_Env *genv, Scheme_Comp_Env *env,
			  Resolve_Prefix *rp, int let_depth, int shift,
			  Scheme_Bucket_Table *syntax, int for_stx,
			  Scheme_Object *certs);

static Scheme_Module_Exports *make_module_exports();

#define cons scheme_make_pair

static Scheme_Object *modbeg_syntax;

static Scheme_Object *kernel_symbol;
static Scheme_Module *kernel;

static Scheme_Object *module_symbol;
static Scheme_Object *module_begin_symbol;

static Scheme_Object *prefix_symbol;
static Scheme_Object *only_symbol;
static Scheme_Object *rename_symbol;
static Scheme_Object *all_except_symbol;
static Scheme_Object *prefix_all_except_symbol;
static Scheme_Object *all_from_symbol;
static Scheme_Object *all_from_except_symbol;
static Scheme_Object *all_defined_symbol;
static Scheme_Object *all_defined_except_symbol;
static Scheme_Object *prefix_all_defined_symbol;
static Scheme_Object *prefix_all_defined_except_symbol;
static Scheme_Object *struct_symbol;
static Scheme_Object *protect_symbol;

static Scheme_Object *module_name_symbol;

Scheme_Object *scheme_module_stx;
Scheme_Object *scheme_begin_stx;
Scheme_Object *scheme_define_values_stx;
Scheme_Object *scheme_define_syntaxes_stx;
static Scheme_Object *define_for_syntaxes_stx;
static Scheme_Object *require_stx;
static Scheme_Object *require_for_syntax_stx;
static Scheme_Object *require_for_template_stx;
static Scheme_Object *require_for_label_stx;
static Scheme_Object *provide_stx;
static Scheme_Object *provide_for_syntax_stx;
static Scheme_Object *provide_for_label_stx;
static Scheme_Object *set_stx;
static Scheme_Object *app_stx;
Scheme_Object *scheme_top_stx;
static Scheme_Object *lambda_stx;
static Scheme_Object *case_lambda_stx;
static Scheme_Object *let_values_stx;
static Scheme_Object *letrec_values_stx;
static Scheme_Object *if_stx;
static Scheme_Object *begin0_stx;
static Scheme_Object *set_stx;
static Scheme_Object *with_continuation_mark_stx;
static Scheme_Object *letrec_syntaxes_stx;
static Scheme_Object *var_ref_stx;
static Scheme_Object *expression_stx;

static Scheme_Env *initial_modules_env;
static int num_initial_modules;
static Scheme_Object **initial_modules;
static Scheme_Object *initial_renames;
static Scheme_Bucket_Table *initial_toplevel;

static Scheme_Object *empty_self_modidx;
static Scheme_Object *empty_self_symbol;

static Scheme_Modidx *modidx_caching_chain;
static Scheme_Object *global_shift_cache;
#define GLOBAL_SHIFT_CACHE_SIZE 40
#ifdef USE_SENORA_GC
# define SHIFT_CACHE_NULL scheme_false
# define SHIFT_CACHE_NULLP(x) SCHEME_FALSEP(x)
#else
# define SHIFT_CACHE_NULL NULL
# define SHIFT_CACHE_NULLP(x) !(x)
#endif

typedef void (*Check_Func)(Scheme_Object *prnt_name, Scheme_Object *name, Scheme_Object *nominal_modname, 
			   Scheme_Object *modname, Scheme_Object *srcname, 
			   int isval, void *data, Scheme_Object *e, Scheme_Object *form, Scheme_Object *err_src);
static Scheme_Object *parse_requires(Scheme_Object *form,
				     Scheme_Object *base_modidx,
				     Scheme_Env *env,
				     Scheme_Object *rn, Scheme_Object *post_ex_rn,
				     Scheme_Object *et_rn, Scheme_Object *et_post_ex_rn,
				     Scheme_Object *dt_rn, Scheme_Object *dt_post_ex_rn,
				     Check_Func ck, void *data, void *et_data, void *dt_data,
				     int start, int expstart, Scheme_Object *redef_modname,
				     int unpack_kern, int copy_vars, int can_save_marshal,
				     int *all_simple);
static Scheme_Object *parse_provides(Scheme_Object *form, Scheme_Object *fst, Scheme_Object *e,
                                     Scheme_Hash_Table *provided, Scheme_Object *reprovided,
                                     Scheme_Object *self_modidx,
                                     Scheme_Object **_all_defs_out);
static int compute_reprovides(Scheme_Hash_Table *provided, Scheme_Object *reprovided, 
                              Scheme_Object *requires, Scheme_Hash_Table *required, 
                              Scheme_Env *genv, Scheme_Object *all_defs, Scheme_Object *all_defs_out, 
                              Scheme_Object **_exclude_hint,
                              const char *matching_form);
static char *compute_provide_arrays(Scheme_Hash_Table *provided, Scheme_Hash_Table *required,
                                    Scheme_Module_Phase_Exports *pt,
                                    Scheme_Env *genv, int def_phase,
                                    int reprovide_kernel,
                                    Scheme_Object *form,
                                    const char *def_way);
static void start_module(Scheme_Module *m, Scheme_Env *env, int restart, Scheme_Object *syntax_idx, int delay_exptime, int with_tt, Scheme_Object *cycle_list);
static void expstart_module(Scheme_Module *m, Scheme_Env *env, int restart, Scheme_Object *syntax_idx, int delay_exptime, int with_tt, Scheme_Object *cycle_list);
static void finish_expstart_module(Scheme_Env *menv, Scheme_Env *env, int with_tt, Scheme_Object *cycle_list);
static void finish_expstart_module_in_namespace(Scheme_Env *menv, Scheme_Env *env);
static void eval_module_body(Scheme_Env *menv);

static Scheme_Object *do_namespace_require(Scheme_Env *env, int argc, Scheme_Object *argv[], 
                                           int for_exp, int copy, int etonly);

static Scheme_Object *default_module_resolver(int argc, Scheme_Object **argv);

static void qsort_provides(Scheme_Object **exs, Scheme_Object **exsns, Scheme_Object **exss, char *exps, char *exets,
			   int start, int count, int do_uninterned);

#define MODCHAIN_TABLE(p) ((Scheme_Hash_Table *)(SCHEME_VEC_ELS(p)[0]))

/**********************************************************************/
/*                           initialization                           */
/**********************************************************************/

void scheme_init_module(Scheme_Env *env)
{
  Scheme_Object *o;

  scheme_register_syntax(MODULE_EXPD, 
			 module_optimize,
			 module_resolve, module_validate, 
			 module_execute, module_jit, 
			 NULL, NULL, -1);
  scheme_register_syntax(REQUIRE_EXPD, 
			 top_level_require_optimize,
			 top_level_require_resolve, top_level_require_validate, 
			 top_level_require_execute, top_level_require_jit, 
			 NULL, NULL, 2);

  scheme_add_global_keyword("module", 
			    scheme_make_compiled_syntax(module_syntax, 
							module_expand),
			    env);

  REGISTER_SO(modbeg_syntax);
  modbeg_syntax = scheme_make_compiled_syntax(module_begin_syntax, 
					      module_begin_expand);

  scheme_add_global_keyword("#%module-begin", 
			    modbeg_syntax,
			    env);

  scheme_add_global_keyword("require", 
			    scheme_make_compiled_syntax(require_syntax, 
							require_expand), 
			    env);
  scheme_add_global_keyword("require-for-syntax", 
			    scheme_make_compiled_syntax(require_for_syntax_syntax, 
							require_for_syntax_expand), 
			    env);
  scheme_add_global_keyword("require-for-template", 
			    scheme_make_compiled_syntax(require_for_template_syntax, 
							require_for_template_expand), 
			    env);
  scheme_add_global_keyword("require-for-label", 
			    scheme_make_compiled_syntax(require_for_label_syntax, 
							require_for_label_expand), 
			    env);
  scheme_add_global_keyword("provide", 
			    scheme_make_compiled_syntax(provide_syntax, 
							provide_expand), 
			    env);
  scheme_add_global_keyword("provide-for-syntax", 
			    scheme_make_compiled_syntax(provide_syntax, 
							provide_expand), 
			    env);
  scheme_add_global_keyword("provide-for-label", 
			    scheme_make_compiled_syntax(provide_syntax, 
							provide_expand), 
			    env);

  REGISTER_SO(kernel_symbol);
  kernel_symbol = scheme_intern_symbol("#%kernel");

  REGISTER_SO(module_symbol);
  REGISTER_SO(module_begin_symbol);
  module_symbol = scheme_intern_symbol("module");
  module_begin_symbol = scheme_intern_symbol("#%module-begin");

  scheme_install_type_writer(scheme_module_type, write_module);
  scheme_install_type_reader(scheme_module_type, read_module);

  o = scheme_make_prim_w_arity(default_module_resolver,
			       "default-module-name-resolver",
			       1, 4);
  scheme_set_param(scheme_current_config(), MZCONFIG_CURRENT_MODULE_RESOLVER, o);

  scheme_set_param(scheme_current_config(), MZCONFIG_CURRENT_MODULE_PREFIX, scheme_false);

  scheme_add_global_constant("current-module-name-resolver", 
			     scheme_register_parameter(current_module_name_resolver, 
						       "current-module-name-resolver",
						       MZCONFIG_CURRENT_MODULE_RESOLVER), 
			     env);
  scheme_add_global_constant("current-module-name-prefix", 
			     scheme_register_parameter(current_module_name_prefix, 
						       "current-module-name-prefix",
						       MZCONFIG_CURRENT_MODULE_PREFIX), 
			     env);

  scheme_add_global_constant("dynamic-require", 
			     scheme_make_prim_w_arity(scheme_dynamic_require,
						      "dynamic-require",
						      2, 2),
			     env);
  scheme_add_global_constant("dynamic-require-for-syntax", 
			     scheme_make_prim_w_arity(dynamic_require_for_syntax,
						      "dynamic-require-for-syntax",
						      2, 2),
			     env);
  scheme_add_global_constant("namespace-require",
			     scheme_make_prim_w_arity(namespace_require,
						      "namespace-require",
						      1, 1),
			     env);
  scheme_add_global_constant("namespace-transformer-require",
			     scheme_make_prim_w_arity(namespace_trans_require,
						      "namespace-transformer-require",
						      1, 1),
			     env);
  scheme_add_global_constant("namespace-attach-module",
			     scheme_make_prim_w_arity(namespace_attach_module,
						      "namespace-attach-module",
						      2, 3),
			     env);
  scheme_add_global_constant("namespace-unprotect-module",
			     scheme_make_prim_w_arity(namespace_unprotect_module,
						      "namespace-unprotect-module",
						      2, 3),
			     env);
  scheme_add_global_constant("namespace-require/copy",
			     scheme_make_prim_w_arity(namespace_require_copy,
						      "namespace-require/copy",
						      1, 1),
			     env);  
  scheme_add_global_constant("namespace-require/expansion-time",
			     scheme_make_prim_w_arity(namespace_require_etonly,
						      "namespace-require/expansion-time",
						      1, 1),
			     env);
  

  scheme_add_global_constant("compiled-module-expression?",
			     scheme_make_prim_w_arity(module_compiled_p,
						      "compiled-module-expression?",
						      1, 1),
			     env);
  scheme_add_global_constant("module-compiled-name",
			     scheme_make_prim_w_arity(module_compiled_name,
						      "module-compiled-name",
						       1, 1),
			     env);
  scheme_add_global_constant("module-compiled-imports",
			     scheme_make_prim_w_arity2(module_compiled_imports,
						       "module-compiled-imports",
						       1, 1,
						       3, 3),
			     env);
  scheme_add_global_constant("module-compiled-exports",
			     scheme_make_prim_w_arity2(module_compiled_exports,
						       "module-compiled-exports",
						       1, 1,
						       2, 2),
			     env);

  scheme_add_global_constant("module-path-index?",
			     scheme_make_folding_prim(module_path_index_p,
						      "module-path-index?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("module-path-index-resolve",
			     scheme_make_prim_w_arity(module_path_index_resolve,
                                                      "module-path-index-resolve",
                                                      1, 1),
			     env);
  scheme_add_global_constant("module-path-index-split",
			     scheme_make_prim_w_arity2(module_path_index_split,
						       "module-path-index-split",
						       1, 1,
						       2, 2),
			     env);
  scheme_add_global_constant("module-path-index-join",
			     scheme_make_prim_w_arity(module_path_index_join,
						      "module-path-index-join",
						      2, 2),
			     env);

  scheme_add_global_constant("module-provide-protected?", 
			     scheme_make_prim_w_arity(module_export_protected_p,
						      "module-provide-protected?",
						      2, 2),
			     env);

    scheme_add_global_constant("module->namespace",
			     scheme_make_prim_w_arity(module_to_namespace,
						      "module->namespace",
						      1, 1),
			     env);
}

void scheme_finish_kernel(Scheme_Env *env)
{
  /* When this function is called, the initial namespace has all the
     primitive bindings for syntax and procedures. This function fills
     in the module wrapper for #%kernel. */
  Scheme_Bucket_Table *ht;
  int i, j, count, syntax_start = 0;
  Scheme_Bucket **bs;
  Scheme_Object **exs, *w, *rn;
  Scheme_Object *insp;

  REGISTER_SO(kernel);

  kernel = MALLOC_ONE_TAGGED(Scheme_Module);
  kernel->so.type = scheme_module_type;

  insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);
  
  scheme_initial_env->module = kernel;
  scheme_initial_env->insp = insp;

  kernel->modname = kernel_symbol;
  kernel->requires = scheme_null;
  kernel->et_requires = scheme_null;
  kernel->tt_requires = scheme_null;
  kernel->dt_requires = scheme_null;

  kernel->insp = insp;
  
  /* Provide all syntax and variables: */
  count = 0;
  for (j = 0; j < 2; j++) {
    if (!j)
      ht = scheme_initial_env->toplevel;
    else {
      ht = scheme_initial_env->syntax;
      syntax_start = count;
    }

    bs = ht->buckets;
    for (i = ht->size; i--; ) {
      Scheme_Bucket *b = bs[i];
      if (b && b->val)
	count++;
    }
  }

  exs = MALLOC_N(Scheme_Object *, count);
  count = 0;
  for (j = 0; j < 2; j++) {
    if (!j)
      ht = scheme_initial_env->toplevel;
    else
      ht = scheme_initial_env->syntax;

    bs = ht->buckets;
    for (i = ht->size; i--; ) {
      Scheme_Bucket *b = bs[i];
      if (b && b->val)
	exs[count++] = (Scheme_Object *)b->key;
    }
  }
 
  kernel->functional = 1;
  kernel->et_functional = 1;
  kernel->tt_functional = 1;
  kernel->no_cert = 1;

  {
    Scheme_Module_Exports *me;
    me = make_module_exports();
    kernel->me = me;
  }

  kernel->me->rt->provides = exs;
  kernel->me->rt->provide_srcs = NULL;
  kernel->me->rt->provide_src_names = exs;
  kernel->me->rt->num_provides = count;
  kernel->me->rt->num_var_provides = syntax_start;

  scheme_initial_env->running = 1;
  scheme_initial_env->et_running = 1;
  scheme_initial_env->attached = 1;

  rn = scheme_make_module_rename(0, mzMOD_RENAME_NORMAL, NULL);
  for (i = kernel->me->rt->num_provides; i--; ) {
    scheme_extend_module_rename(rn, kernel_symbol, exs[i], exs[i], kernel_symbol, exs[i], 0, 0);
  }

  scheme_sys_wraps(NULL);

  REGISTER_SO(scheme_module_stx);
  REGISTER_SO(scheme_begin_stx);
  REGISTER_SO(scheme_define_values_stx);
  REGISTER_SO(scheme_define_syntaxes_stx);
  REGISTER_SO(define_for_syntaxes_stx);
  REGISTER_SO(require_stx);
  REGISTER_SO(require_for_syntax_stx);
  REGISTER_SO(require_for_template_stx);
  REGISTER_SO(require_for_label_stx);
  REGISTER_SO(provide_stx);
  REGISTER_SO(provide_for_syntax_stx);
  REGISTER_SO(provide_for_label_stx);
  REGISTER_SO(set_stx);
  REGISTER_SO(app_stx);
  REGISTER_SO(scheme_top_stx);
  REGISTER_SO(lambda_stx);
  REGISTER_SO(case_lambda_stx);
  REGISTER_SO(let_values_stx);
  REGISTER_SO(letrec_values_stx);
  REGISTER_SO(if_stx);
  REGISTER_SO(begin0_stx);
  REGISTER_SO(set_stx);
  REGISTER_SO(with_continuation_mark_stx);
  REGISTER_SO(letrec_syntaxes_stx);
  REGISTER_SO(var_ref_stx);
  REGISTER_SO(expression_stx);

  w = scheme_sys_wraps0;
  scheme_module_stx = scheme_datum_to_syntax(scheme_intern_symbol("module"), scheme_false, w, 0, 0);
  scheme_begin_stx = scheme_datum_to_syntax(scheme_intern_symbol("begin"), scheme_false, w, 0, 0);
  scheme_define_values_stx = scheme_datum_to_syntax(scheme_intern_symbol("define-values"), scheme_false, w, 0, 0);
  scheme_define_syntaxes_stx = scheme_datum_to_syntax(scheme_intern_symbol("define-syntaxes"), scheme_false, w, 0, 0);
  define_for_syntaxes_stx = scheme_datum_to_syntax(scheme_intern_symbol("define-values-for-syntax"), scheme_false, w, 0, 0);
  require_stx = scheme_datum_to_syntax(scheme_intern_symbol("require"), scheme_false, w, 0, 0);
  require_for_syntax_stx = scheme_datum_to_syntax(scheme_intern_symbol("require-for-syntax"), scheme_false, w, 0, 0);
  require_for_template_stx = scheme_datum_to_syntax(scheme_intern_symbol("require-for-template"), scheme_false, w, 0, 0);
  require_for_label_stx = scheme_datum_to_syntax(scheme_intern_symbol("require-for-label"), scheme_false, w, 0, 0);
  provide_stx = scheme_datum_to_syntax(scheme_intern_symbol("provide"), scheme_false, w, 0, 0);
  provide_for_syntax_stx = scheme_datum_to_syntax(scheme_intern_symbol("provide-for-syntax"), scheme_false, w, 0, 0);
  provide_for_label_stx = scheme_datum_to_syntax(scheme_intern_symbol("provide-for-label"), scheme_false, w, 0, 0);
  set_stx = scheme_datum_to_syntax(scheme_intern_symbol("set!"), scheme_false, w, 0, 0);
  app_stx = scheme_datum_to_syntax(scheme_intern_symbol("#%app"), scheme_false, w, 0, 0);
  scheme_top_stx = scheme_datum_to_syntax(scheme_intern_symbol("#%top"), scheme_false, w, 0, 0);
  lambda_stx = scheme_datum_to_syntax(scheme_intern_symbol("lambda"), scheme_false, w, 0, 0);
  case_lambda_stx = scheme_datum_to_syntax(scheme_intern_symbol("case-lambda"), scheme_false, w, 0, 0);
  let_values_stx = scheme_datum_to_syntax(scheme_intern_symbol("let-values"), scheme_false, w, 0, 0);
  letrec_values_stx = scheme_datum_to_syntax(scheme_intern_symbol("letrec-values"), scheme_false, w, 0, 0);
  if_stx = scheme_datum_to_syntax(scheme_intern_symbol("if"), scheme_false, w, 0, 0);
  begin0_stx = scheme_datum_to_syntax(scheme_intern_symbol("begin0"), scheme_false, w, 0, 0);
  set_stx = scheme_datum_to_syntax(scheme_intern_symbol("set!"), scheme_false, w, 0, 0);
  with_continuation_mark_stx = scheme_datum_to_syntax(scheme_intern_symbol("with-continuation-mark"), scheme_false, w, 0, 0);
  letrec_syntaxes_stx = scheme_datum_to_syntax(scheme_intern_symbol("letrec-syntaxes+values"), scheme_false, w, 0, 0);
  var_ref_stx = scheme_datum_to_syntax(scheme_intern_symbol("#%variable-reference"), scheme_false, w, 0, 0);
  expression_stx = scheme_datum_to_syntax(scheme_intern_symbol("#%expression"), scheme_false, w, 0, 0);

  REGISTER_SO(prefix_symbol);
  REGISTER_SO(only_symbol);
  REGISTER_SO(rename_symbol);
  REGISTER_SO(all_except_symbol);
  REGISTER_SO(prefix_all_except_symbol);
  REGISTER_SO(all_from_symbol);
  REGISTER_SO(all_from_except_symbol);
  REGISTER_SO(all_defined_symbol);
  REGISTER_SO(all_defined_except_symbol);
  REGISTER_SO(prefix_all_defined_symbol);
  REGISTER_SO(prefix_all_defined_except_symbol);
  REGISTER_SO(struct_symbol);
  REGISTER_SO(protect_symbol);
  prefix_symbol = scheme_intern_symbol("prefix");
  only_symbol = scheme_intern_symbol("only");
  rename_symbol = scheme_intern_symbol("rename");
  all_except_symbol = scheme_intern_symbol("all-except");
  prefix_all_except_symbol = scheme_intern_symbol("prefix-all-except");
  all_from_symbol = scheme_intern_symbol("all-from");
  all_from_except_symbol = scheme_intern_symbol("all-from-except");
  all_defined_symbol = scheme_intern_symbol("all-defined");
  all_defined_except_symbol = scheme_intern_symbol("all-defined-except");
  prefix_all_defined_symbol = scheme_intern_symbol("prefix-all-defined");
  prefix_all_defined_except_symbol = scheme_intern_symbol("prefix-all-defined-except");
  struct_symbol = scheme_intern_symbol("struct");
  protect_symbol = scheme_intern_symbol("protect");

  REGISTER_SO(module_name_symbol);
  module_name_symbol = scheme_intern_symbol("enclosing-module-name");
}

void scheme_require_from_original_env(Scheme_Env *env, int syntax_only)
{
  Scheme_Object *rn, **exs;
  int i, c;

  rn = env->rename;
  if (!rn) {
    rn = scheme_make_module_rename(env->phase, mzMOD_RENAME_TOPLEVEL, NULL);
    env->rename = rn;
  }

  exs = kernel->me->rt->provides;
  c = kernel->me->rt->num_provides;
  i = (syntax_only ? kernel->me->rt->num_var_provides : 0);
  for (; i < c; i++) {
    scheme_extend_module_rename(rn, kernel_symbol, exs[i], exs[i], kernel_symbol, exs[i], 0, 0);
  }
}

Scheme_Object *scheme_sys_wraps(Scheme_Comp_Env *env)
{
  Scheme_Object *rn, *w;
  long phase;

  if (!env)
    phase = 0;
  else if (SCHEME_INTP((Scheme_Object *)env))
    phase = SCHEME_INT_VAL((Scheme_Object *)env);
  else
    phase = env->genv->phase;

  if ((phase == 0) && scheme_sys_wraps0)
    return scheme_sys_wraps0;
  if ((phase == 1) && scheme_sys_wraps1)
    return scheme_sys_wraps1;

  rn = scheme_make_module_rename(phase, mzMOD_RENAME_NORMAL, NULL);

  /* Add a module mapping for all kernel provides: */
  scheme_extend_module_rename_with_kernel(rn, kernel_symbol);
  
  w = scheme_datum_to_syntax(kernel_symbol, scheme_false, scheme_false, 0, 0);
  w = scheme_add_rename(w, rn);
  if (phase == 0) {
    REGISTER_SO(scheme_sys_wraps0);
    scheme_sys_wraps0 = w;
  }
  if (phase == 1) {
    REGISTER_SO(scheme_sys_wraps1);
    scheme_sys_wraps1 = w;
  }

  return w;
}

void scheme_save_initial_module_set(Scheme_Env *env)
/* Can be called multiple times! */
{
  int i, c, count;
  Scheme_Hash_Table *ht;
	
  if (!initial_modules_env) {
    REGISTER_SO(initial_modules_env);
  }
  initial_modules_env = env;
  
  ht = env->module_registry;
  c = ht->size;

  count = 0;
  for (i = 0; i < c; i++) {
    if (ht->vals[i])
      count++;
  }

  num_initial_modules = count;
  
  if (!initial_modules) {
    REGISTER_SO(initial_modules);
  }
  initial_modules = MALLOC_N(Scheme_Object *, count);

  count = 0;
  for (i = 0; i < c; i++) {
    if (ht->vals[i]) {
      initial_modules[count++] = ht->keys[i];
    }
  }

  /* Clone renames: */
  if (!initial_renames) {
    REGISTER_SO(initial_renames);
  }
  initial_renames = scheme_make_module_rename(0, mzMOD_RENAME_NORMAL, NULL);
  scheme_append_module_rename(env->rename, initial_renames);

  /* Clone variable bindings: */
  if (!initial_toplevel) {
     REGISTER_SO(initial_toplevel);
  }
  initial_toplevel = scheme_clone_toplevel(env->toplevel, NULL);
}

void scheme_install_initial_module_set(Scheme_Env *env)
{
  int i;
  Scheme_Object *a[3];
  Scheme_Module *m;

  /* Copy over module declarations and instances: */
  for (i = 0; i < num_initial_modules; i++) {
    a[0] = (Scheme_Object *)initial_modules_env;
    a[1] = initial_modules[i];
    a[2] = (Scheme_Object *)env;

    /* Make sure module is running: */
    m = (Scheme_Module *)scheme_hash_get(initial_modules_env->module_registry, a[1]);
    start_module(m, initial_modules_env, 0, a[1], 0, 0, scheme_null);

    namespace_attach_module(3, a);
  }

  /* Copy renamings: */
  if (!env->rename) {
    Scheme_Object *rn;
    rn = scheme_make_module_rename(0, mzMOD_RENAME_TOPLEVEL, NULL);
    env->rename = rn;
  }
  scheme_append_module_rename(initial_renames, env->rename);

  /* Copy toplevel: */
  {
    Scheme_Bucket_Table *tl;
    tl = scheme_clone_toplevel(initial_toplevel, env);
    env->toplevel = tl;
  }
}

static void annote_marked_names_nonempty(Scheme_Hash_Table *mn_ht)
{
  /* Prevents a module-renaming record for macro-introduced bindings
     from being dropped in syntax objects until the module is fully
     compiled/expanded. */
  scheme_hash_set(mn_ht, scheme_false, scheme_null);
}

static void clear_marked_names_nonempty(Scheme_Hash_Table *mn_ht)
{
  /* Clears the annotation, since the module is fully
     compiled/expanded. */
  scheme_hash_set(mn_ht, scheme_false, NULL);
}

/**********************************************************************/
/*                             parameters                             */
/**********************************************************************/

static Scheme_Object *default_module_resolver(int argc, Scheme_Object **argv)
{
  scheme_arg_mismatch("default-module-name-resolver", 
		      "the kernel's resolver always fails; given: ", 
		      argv[0]);
  return NULL;
}

static Scheme_Object *check_resolver(int argc, Scheme_Object **argv)
{
  if (scheme_check_proc_arity(NULL, 1, 0, argc, argv)
      && scheme_check_proc_arity(NULL, 3, 0, argc, argv)
      && scheme_check_proc_arity(NULL, 4, 0, argc, argv))
    return argv[0];

  scheme_wrong_type("current-module-name-resolver", "procedure of arity 1, 3, and 4", 0, argc, argv);

  return NULL;
}

static Scheme_Object *
current_module_name_resolver(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-module-name-resolver",
			     scheme_make_integer(MZCONFIG_CURRENT_MODULE_RESOLVER),
			     argc, argv,
			     -1, check_resolver, "procedure of arity 1, 3, and 4", 1);
}

static Scheme_Object *prefix_p(int argc, Scheme_Object **argv)
{
  Scheme_Object *o = argv[0];
  
  if (SCHEME_FALSEP(o) || SCHEME_SYMBOLP(o))
    return o;

  return NULL;
}

static Scheme_Object *
current_module_name_prefix(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-module-name-prefix",
			     scheme_make_integer(MZCONFIG_CURRENT_MODULE_PREFIX),
			     argc, argv,
			     -1, prefix_p, "symbol or #f", 1);
}

/**********************************************************************/
/*                            procedures                              */
/**********************************************************************/

int scheme_module_protected_wrt(Scheme_Object *home_insp, Scheme_Object *insp)
{
  if (!insp)
    return 1;
  if (SAME_OBJ(insp, scheme_true))
    return 0;
  return !scheme_is_subinspector(home_insp, insp);
}

static Scheme_Object *_dynamic_require(int argc, Scheme_Object *argv[],
				       Scheme_Env *env,
				       int get_bucket, 
				       int phase, int mod_phase, int indirect_ok,
				       int fail_with_error,
				       int position)
{
  Scheme_Object *modname, *modidx;
  Scheme_Object *name, *srcname, *srcmname;
  Scheme_Module *m, *srcm;
  Scheme_Env *menv, *lookup_env = NULL;
  int i, count, protected = 0;
  const char *errname;

  modname = argv[0];
  name = argv[1];

  errname = (phase 
	     ? ((phase < 0)
		? "dynamic-require-for-template" 
		: "dynamic-require-for-syntax" )
	     : "dynamic-require");

  if (SCHEME_TRUEP(name) && !SCHEME_SYMBOLP(name) && !SCHEME_VOIDP(name)) {
    scheme_wrong_type(errname, "symbol, #f, or void", 1, argc, argv);
    return NULL;
  }

  if (SAME_TYPE(SCHEME_TYPE(modname), scheme_module_index_type))
    modidx = modname;
  else
    modidx = scheme_make_modidx(modname, scheme_false, scheme_false);

  modname = scheme_module_resolve(modidx, 1);

  if (phase == 1) {
    scheme_prepare_exp_env(env);
    if (mod_phase)
      lookup_env = env->exp_env;
    else
      env = env->exp_env;
  } else if (phase == -1) {
    scheme_prepare_template_env(env);
    env = env->template_env;
  }

  m = module_load(modname, env, errname);
  srcm = m;

  srcmname = NULL;
  srcname = NULL;

  if (SCHEME_SYMBOLP(name)) {
    if (mod_phase) {
      srcname = name;
      srcmname = modname;
    } else {
    try_again:
    
      /* Before starting, check whether the name is provided */
      count = srcm->me->rt->num_provides;
      if (position >= 0) {
	if (position < srcm->me->rt->num_var_provides) {
	  i = position;
	  if ((SCHEME_SYM_LEN(name) == SCHEME_SYM_LEN(srcm->me->rt->provide_src_names[i]))
	      && !memcmp(SCHEME_SYM_VAL(name), SCHEME_SYM_VAL(srcm->me->rt->provide_src_names[i]), SCHEME_SYM_LEN(name))) {
	    name = srcm->me->rt->provides[i];
	  } else {
	    i = count; /* not found */
	    indirect_ok = 0; /* don't look further */
	  }
	} else {
	  position -= srcm->me->rt->num_var_provides;
	  i = count;
	}
      } else {
	for (i = 0; i < count; i++) {
	  if (SAME_OBJ(name, srcm->me->rt->provides[i])) {
	    if (i < srcm->me->rt->num_var_provides) {
	      break;
	    } else {
	      if (fail_with_error) {
                if (!phase) {
                  /* Evaluate id in a fresh namespace */
                  Scheme_Object *a[3], *ns;
                  start_module(m, env, 0, modidx, 1, 0, scheme_null);
                  a[0] = scheme_intern_symbol("empty");
                  ns = scheme_make_namespace(1, a);
                  a[0] = (Scheme_Object *)env;
                  a[1] = srcm->modname;
                  a[2] = ns;
                  namespace_attach_module(3, a);
                  a[0] = scheme_make_pair(scheme_intern_symbol("only"),
                                          scheme_make_pair(srcm->modname,
                                                           scheme_make_pair(name,
                                                                            scheme_null)));
                  do_namespace_require((Scheme_Env *)ns, 1, a, 0, 0, 0);
                  return scheme_eval(name, (Scheme_Env *)ns);
                } else {
                  scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                                   "%s: name is provided as syntax: %V by module: %V",
                                   errname,
                                   name, srcm->modname);
                }
              }
	      return NULL;
	    }
	  }
	}
      }

      if (i < count) {
	if (srcm->provide_protects)
	  protected = srcm->provide_protects[i];
	srcmname = (srcm->me->rt->provide_srcs ? srcm->me->rt->provide_srcs[i] : scheme_false);
	if (SCHEME_FALSEP(srcmname))
	  srcmname = srcm->modname;
	else {
	  srcmname = scheme_modidx_shift(srcmname, srcm->me->src_modidx, srcm->self_modidx);
	  srcmname = scheme_module_resolve(srcmname, 1);
	}
	srcname = srcm->me->rt->provide_src_names[i];
      }

      if ((position < 0) && (i == count) && srcm->me->rt->reprovide_kernel) {
	/* Check kernel. */
	srcm = kernel;
	goto try_again;
      }

      if (i == count) {
	if (indirect_ok) {
	  /* Try indirect provides: */
	  srcm = m;
	  count = srcm->num_indirect_provides;
	  if (position >= 0) {
	    i = position;
	    if ((i < srcm->num_indirect_provides)
		&& (SCHEME_SYM_LEN(name) == SCHEME_SYM_LEN(srcm->indirect_provides[i]))
		&& !memcmp(SCHEME_SYM_VAL(name), SCHEME_SYM_VAL(srcm->indirect_provides[i]), SCHEME_SYM_LEN(name))) {
	      name = srcm->indirect_provides[i];
	      srcname = name;
	      srcmname = srcm->modname;
	      if (srcm->provide_protects)
		protected = srcm->provide_protects[i];
	    } else
	      i = count; /* not found */
	  } else {
	    for (i = 0; i < count; i++) {
	      if (SAME_OBJ(name, srcm->indirect_provides[i])) {
		srcname = name;
		srcmname = srcm->modname;
		if (srcm->provide_protects)
		  protected = srcm->provide_protects[i];
		break;
	      }
	    }
	  }
	}

	if (i == count) {
	  if (fail_with_error)
	    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
			     "%s: name is not provided: %V by module: %V",
			     errname,
			     name, srcm->modname);
	  return NULL;
	}
      }
    }
  }

  if (SCHEME_VOIDP(name))
    expstart_module(m, env, 0, modidx, 0, 1, scheme_null);
  else
    start_module(m, env, 0, modidx, 1, 0, scheme_null);

  if (SCHEME_SYMBOLP(name)) {
    Scheme_Bucket *b;

    menv = scheme_module_access(srcmname, lookup_env ? lookup_env : env, mod_phase);

    if (protected) {
      Scheme_Object *insp;
      insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);
      if (scheme_module_protected_wrt(menv->insp, insp))
	scheme_raise_exn(MZEXN_FAIL_CONTRACT,
			 "%s: name is protected: %V from module: %V",
			 errname,
			 name, srcm->modname);
    }
    
    b = scheme_bucket_from_table(menv->toplevel, (const char *)srcname);

    if (get_bucket)
      return (Scheme_Object *)b;
    else {
      if (!b->val && fail_with_error)
	scheme_unbound_global(b);
      return b->val;
    }
  } else
    return scheme_void;
}

Scheme_Object *scheme_dynamic_require(int argc, Scheme_Object *argv[])
{
  if (scheme_module_demand_hook) {
    Scheme_Object *r;
    r = scheme_module_demand_hook(argc, argv);
    if (r) return r;
  }

  return _dynamic_require(argc, argv, scheme_get_env(NULL), 0, 0, 0, 0, 1, -1);
}

static Scheme_Object *dynamic_require_for_syntax(int argc, Scheme_Object *argv[])
{
  return _dynamic_require(argc, argv, scheme_get_env(NULL), 0, 1, 0, 0, 1, -1);
}

static Scheme_Object *do_namespace_require(Scheme_Env *env, int argc, Scheme_Object *argv[], 
                                           int for_exp, int copy, int etonly)
{
  Scheme_Object *form, *rn, *brn, *et_rn, *dt_rn;

  if (!env)
    env = scheme_get_env(NULL);
  if (for_exp) {
    scheme_prepare_exp_env(env);
    env = env->exp_env;
  }

  form = scheme_datum_to_syntax(scheme_make_pair(require_stx,
						 scheme_make_pair(argv[0], scheme_null)),
				scheme_false, scheme_false, 1, 0);
  
  rn = scheme_make_module_rename(for_exp, mzMOD_RENAME_TOPLEVEL, NULL);


  if (!for_exp && !etonly && !copy) {
    scheme_prepare_exp_env(env);

    et_rn = env->exp_env->rename;
    if (!et_rn) {
      et_rn = scheme_make_module_rename(1, mzMOD_RENAME_TOPLEVEL, NULL);
      env->exp_env->rename = et_rn;
    }

    dt_rn = env->dt_rename;
    if (!dt_rn) {
      dt_rn = scheme_make_module_rename(MZ_LABEL_PHASE, mzMOD_RENAME_TOPLEVEL, NULL);
      env->dt_rename = dt_rn;
    }
  } else {
    et_rn = NULL;
    dt_rn = NULL;
  }

  (void)parse_requires(form, scheme_false, env, 
                       rn, rn, 
                       et_rn, et_rn,
                       dt_rn, dt_rn,
                       NULL, NULL,
		       NULL, NULL, !etonly, etonly, NULL, 1, copy, 0, NULL);

  brn = env->rename;
  if (!brn) {
    brn = scheme_make_module_rename(for_exp, mzMOD_RENAME_TOPLEVEL, NULL);
    env->rename = brn;
  }

  scheme_append_module_rename(rn, brn);

  return scheme_void;
}

static Scheme_Object *namespace_require(int argc, Scheme_Object *argv[])
{
  return do_namespace_require(NULL, argc, argv, 0, 0, 0);
}

static Scheme_Object *namespace_trans_require(int argc, Scheme_Object *argv[])
{
  return do_namespace_require(NULL, argc, argv, 1, 0, 0);
}

static Scheme_Object *namespace_require_copy(int argc, Scheme_Object *argv[])
{
  return do_namespace_require(NULL, argc, argv, 0, 1, 0);
}

static Scheme_Object *namespace_require_etonly(int argc, Scheme_Object *argv[])
{
  return do_namespace_require(NULL, argc, argv, 0, 0, 1);
}

/* I think we don't need to copy phaseless modules for an
   attach. (If we do try to copy, there's a problem in that the 
   transitive requirements of a for-label import are not 
   necessarily loaded, yet.) */
#define NEED_COPY_NOPHASE 0

static Scheme_Object *namespace_attach_module(int argc, Scheme_Object *argv[])
{
  Scheme_Env *from_env, *to_env, *menv, *menv2;
  Scheme_Object *todo, *next_phase_todo, *prev_phase_todo;
  Scheme_Object *name, *notifies = scheme_null, *a[1], *resolver;
  Scheme_Object *to_modchain, *from_modchain, *l;
  Scheme_Hash_Table *checked, *next_checked, *prev_checked;
  Scheme_Object *past_checkeds, *future_checkeds, *future_todos, *past_to_modchains;
  Scheme_Module *m2;
# if NEED_COPY_NOPHASE
  Scheme_Object *nophase_todo;
  Scheme_Hash_Table *nophase_checked;
# endif
  int same_namespace, set_env_for_notify = 0, phase;

  if (!SCHEME_NAMESPACEP(argv[0]))
    scheme_wrong_type("namespace-attach-module", "namespace", 0, argc, argv);

  from_env = (Scheme_Env *)argv[0];
  if (argc > 2) {
    if (!SCHEME_NAMESPACEP(argv[2]))
      scheme_wrong_type("namespace-attach-module", "namespace", 2, argc, argv);
    to_env = (Scheme_Env *)argv[2];
    set_env_for_notify = 1;
  } else
    to_env = scheme_get_env(NULL);

  same_namespace = SAME_OBJ(from_env, to_env);

  if (SCHEME_SYMBOLP(argv[1]))
    name = argv[1];
  else
    name = scheme_module_resolve(scheme_make_modidx(argv[1], scheme_false, scheme_false), 0);

  todo = scheme_make_pair(name, scheme_null);
  next_phase_todo = scheme_null;
  prev_phase_todo = scheme_null;
# if NEED_COPY_NOPHASE
  nophase_todo = scheme_null;
# endif
  from_modchain = from_env->modchain;
  to_modchain = to_env->modchain;
  phase = 0;

  checked = NULL;
  next_checked = NULL;
  prev_checked = NULL;

  past_checkeds = scheme_null;
  future_checkeds = scheme_null;
  future_todos = scheme_null;
  past_to_modchains = scheme_null;

# if NEED_COPY_NOPHASE
  nophase_checked = scheme_make_hash_table(SCHEME_hash_ptr);
# endif

  /* Check whether todo, or anything it needs, is already declared
     incompatibly. Successive iterations of the outer loop explore
     successive phases (i.e, for-syntax levels). */
  while (!SCHEME_NULLP(todo)) {
    if (!checked)
      checked = scheme_make_hash_table(SCHEME_hash_ptr);
    /* This is just a shortcut: */
    if (!next_checked)
      next_checked = scheme_make_hash_table(SCHEME_hash_ptr);

    /* This loop iterates through require chains in the same phase */
    while (!SCHEME_NULLP(todo)) {
      name = SCHEME_CAR(todo);

      todo = SCHEME_CDR(todo);

      scheme_hash_set(checked, name, scheme_true);

      if (!SAME_OBJ(name, kernel_symbol)) {
	menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(from_modchain), name);

	/* printf("Check %d %s\n", phase, SCHEME_SYM_VAL(name)); */
	
	if (!menv) {
	  /* Assert: name == argv[1] */
	  /* Module at least declared? */
	  if (scheme_hash_get(from_env->module_registry, name))
	    scheme_arg_mismatch("namespace-attach-module",
				"module not instantiated (in the source namespace): ",
				name);
	  else
	    scheme_arg_mismatch("namespace-attach-module",
				"unknown module (in the source namespace): ",
				name);
	}

	/* If to_modchain goes to #f, then our source check has gone
	   deeper in phases (for-syntax levels) than the target
	   namespace has ever gone, so there's definitely no conflict
	   at this level in that case. */
	if (SCHEME_TRUEP(to_modchain)) {
	  menv2 = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(to_modchain), name);
	  if (menv2) {
	    if (!SAME_OBJ(menv->toplevel, menv2->toplevel))
	      m2 = menv2->module;
	    else
	      m2 = NULL;
	  } else {
	    m2 = (Scheme_Module *)scheme_hash_get(to_env->module_registry, name);
	    if (m2 && SAME_OBJ(m2, menv->module))
	      m2 = NULL;
	  }
	  
	  if (m2) {
	    char *phase, buf[32];

	    if (!menv->phase)
	      phase = "";
	    else if (menv->phase == 1)
	      phase = " for syntax";
	    else {
	      sprintf(buf, " at phase %ld", menv->phase);
	      phase = buf;
	    }

	    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
			     "namespace-attach-module: "
			     "a different module with the same name is already "
			     "in the destination namespace%s, for name: %S",
			     phase, name);
	    return NULL;
	  }
	} else
	  menv2 = NULL;

	if (!menv2 || same_namespace) {
	  /* Push requires onto the check list: */
	  l = menv->require_names;
	  while (!SCHEME_NULLP(l)) {
	    name = scheme_module_resolve(SCHEME_CAR(l), 0);
	    if (!scheme_hash_get(checked, name)) {
              /* printf("Add %d %s (%p)\n", phase, SCHEME_SYM_VAL(name), checked); */
	      todo = scheme_make_pair(name, todo);
	      scheme_hash_set(checked, name, scheme_true);
	    }
	    l = SCHEME_CDR(l);
	  }

	  /* Have to force laziness in source to ensure sharing: */
	  if (menv->lazy_syntax)
	    finish_expstart_module_in_namespace(menv, from_env);

	  l = menv->et_require_names;
	  while (!SCHEME_NULLP(l)) {
	    name = scheme_module_resolve(SCHEME_CAR(l), 0);
	    if (!scheme_hash_get(next_checked, name)) {
	      /* printf("Add +%d %s (%p)\n", phase+1, SCHEME_SYM_VAL(name), next_checked); */
	      next_phase_todo = scheme_make_pair(name, next_phase_todo);
	      scheme_hash_set(next_checked, name, scheme_true);
	    }
	    l = SCHEME_CDR(l);
	  }

          l = menv->tt_require_names;
          if (l) {
            while (!SCHEME_NULLP(l)) {
              name = scheme_module_resolve(SCHEME_CAR(l), 0);
              if (phase > 0) {
                if (!prev_checked)
                  prev_checked = scheme_make_hash_table(SCHEME_hash_ptr);
                if (!scheme_hash_get(prev_checked, name)) {
                  /* printf("Add -%d %s (%p)\n", phase-1, SCHEME_SYM_VAL(name), prev_checked); */
                  prev_phase_todo = scheme_make_pair(name, prev_phase_todo);
                  scheme_hash_set(prev_checked, name, scheme_true);
                }
              } else {
#               if NEED_COPY_NOPHASE
                /* Need (phaseless) declaration, only */
                if (!same_namespace) {
                  if (!scheme_hash_get(nophase_checked, name)) {
                    /* printf("Add -* %s\n", SCHEME_SYM_VAL(name)); */
                    nophase_todo = scheme_make_pair(name, nophase_todo);
                    scheme_hash_set(nophase_checked, name, scheme_true);
                  }
                }
#               endif
              }
              l = SCHEME_CDR(l);
            }
	  }

#         if NEED_COPY_NOPHASE
          if (!same_namespace) {
            l = menv->dt_require_names;
            if (l) {
              /* Need (phaseless) declaration, only */
              while (!SCHEME_NULLP(l)) {
                name = scheme_module_resolve(SCHEME_CAR(l), 0);
              
                if (!scheme_hash_get(nophase_checked, name)) {
                  /* printf("Add * %s\n", SCHEME_SYM_VAL(name)); */
                  nophase_todo = scheme_make_pair(name, nophase_todo);
                  scheme_hash_set(nophase_checked, name, scheme_true);
                }
                l = SCHEME_CDR(l);
              }
            }
          }
#         endif
        }
      }
    }

#   if NEED_COPY_NOPHASE
    while (!SCHEME_NULLP(nophase_todo)) {
      name = SCHEME_CAR(nophase_todo);
      nophase_todo = SCHEME_CDR(nophase_todo);

      if (!SAME_OBJ(name, kernel_symbol)) {
        if (!scheme_hash_get(to_env->module_registry, name)) {
          Scheme_Object *m1;
          int step;

          m1 = (Scheme_Module *)scheme_hash_get(from_env->module_registry, name);

          if (!m1) {
            /* This shouldn't happen! */
            scheme_arg_mismatch("namespace-attach-module",
				"unknown module (in the source namespace, for no-phase): ",
				name);
          }

          m2 = (Scheme_Module *)scheme_hash_get(from_env->module_registry, name);

          if (m2 && !SAME_OBJ(m1, m2)) {
            scheme_raise_exn(MZEXN_FAIL_CONTRACT,
			     "namespace-attach-module: "
			     "a different module with the same name is already "
			     "in the destination namespace, for name: %S",
			     name);
          }

          /* Need transitive declarations for all phases 
             (but not neecssarily instantiations): */
          for (step = 0; step < 4; step++) {
            switch (step) {
            case 0:
              l = m2->requires;
              break;
            case 1:
              l = m2->et_requires;
              break;
            case 2:
              l = m2->tt_requires;
              break;
            case 3:
            default:
              l = m2->dt_requires;
              break;
            }

            while (!SCHEME_NULLP(l)) {
              name = scheme_module_resolve(SCHEME_CAR(l), 0);
              if (!scheme_hash_get(nophase_checked, name)) {
                nophase_todo = scheme_make_pair(name, nophase_todo);
                scheme_hash_set(nophase_checked, name, scheme_true);
              }
              l = SCHEME_CDR(l);
            }
	  }
        }
      }
    }
#   endif
    
    do {
      if (SCHEME_PAIRP(prev_phase_todo)) {
	future_todos = cons(next_phase_todo, future_todos);
	future_checkeds = cons((Scheme_Object *)next_checked, future_checkeds);
	next_checked = checked;
	
	next_phase_todo = scheme_null;
	todo = prev_phase_todo;
	prev_phase_todo = scheme_null;
	
	checked = prev_checked;
	prev_checked = (Scheme_Hash_Table *)SCHEME_CAR(past_checkeds);
	past_checkeds = SCHEME_CDR(past_checkeds);

	from_modchain = SCHEME_VEC_ELS(from_modchain)[2];
	to_modchain = SCHEME_CAR(past_to_modchains);
	past_to_modchains = SCHEME_CDR(past_to_modchains);
	phase--;
      } else {
	past_checkeds = scheme_make_raw_pair((Scheme_Object *)prev_checked, past_checkeds);
	prev_checked = checked;

	todo = next_phase_todo;
	checked = next_checked;
	
	if (SCHEME_NULLP(future_todos)) {
	  next_phase_todo = scheme_null;
	  next_checked = NULL;
	} else {
	  next_phase_todo = SCHEME_CAR(future_todos);
	  future_todos = SCHEME_CDR(future_todos);
	  next_checked = (Scheme_Hash_Table *)SCHEME_CAR(future_checkeds);
	  future_checkeds = SCHEME_CDR(future_checkeds);
	}
	
	from_modchain = SCHEME_VEC_ELS(from_modchain)[1];
	past_to_modchains = cons(to_modchain, past_to_modchains);
	if (SCHEME_TRUEP(to_modchain))
	  to_modchain = SCHEME_VEC_ELS(to_modchain)[1];
	phase++;
      }
    } while (SCHEME_NULLP(todo) && (SCHEME_PAIRP(next_phase_todo)
				    || SCHEME_PAIRP(future_todos)));
  }

  /* printf("Done phase: %d\n", phase); */
  
  phase += 2; /* represents phase at the start of in future_checkeds */

  /* All of the modules that we saw are in the ***_checked hash tables */
  if (phase > 1) {
    if (next_checked)
      future_checkeds = cons((Scheme_Object *)next_checked, future_checkeds);
    /* else future_checkeds must be scheme_null */
    --phase;
  }
  if (phase > 0) {
    if (checked)
      future_checkeds = cons((Scheme_Object *)checked, future_checkeds);
    /* else future_checkeds must be scheme_null */
    --phase;
  }
  if (phase > 0) {
    future_checkeds = cons((Scheme_Object *)prev_checked, future_checkeds);
    --phase;
  }
  while (phase > 0) {
    prev_checked = (Scheme_Hash_Table *)SCHEME_CAR(past_checkeds);
    future_checkeds = scheme_make_raw_pair((Scheme_Object *)prev_checked, future_checkeds);
    
    past_checkeds = SCHEME_CDR(past_checkeds);
    --phase;
  }
  /* Now all the modules to check are in the future_checkeds
     list of hash tables. */

# if NEED_COPY_NOPHASE
  /* Before we transfer instances, we can transfer modules for which we
     just need declarations. */
  {
    int i;
    for (i = nophase_checked->size; i--; ) {
      if (nophase_checked->vals[i]) {
        name = nophase_checked->keys[i];
        
        if (!SAME_OBJ(name, kernel_symbol)) {
          
          m2 = (Scheme_Module *)scheme_hash_get(from_env->module_registry, name);
          scheme_hash_set(to_env->module_registry, name, (Scheme_Object *)m2);
        }
      }
    }
  }
# endif

  /* Go through that list, this time tranferring module instances */
  from_modchain = from_env->modchain;
  to_modchain = to_env->modchain;

  /* Again, outer loop iterates through phases. */
  while (!SCHEME_NULLP(future_checkeds)) {
    /* Inner loop iterates through requires within a phase. */
    int i;

    checked = (Scheme_Hash_Table *)SCHEME_CAR(future_checkeds);

    /* printf("Copying %d (%p)\n", phase, checked); */

    for (i = checked->size; i--; ) {
      if (checked->vals[i]) {
	name = checked->keys[i];

	if (!SAME_OBJ(name, kernel_symbol)) {
	  menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(from_modchain), name);
	  
	  /* printf("Copy %d %s\n", phase, SCHEME_SYM_VAL(name)); */

	  menv2 = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(to_modchain), name);
	  if (!menv2) {
	    /* Clone menv for the new namespace: */
	    menv2 = scheme_clone_module_env(menv, to_env, to_modchain);
	    if (menv->attached)
	      menv2->attached = 1;
	    
	    scheme_hash_set(MODCHAIN_TABLE(to_modchain), name, (Scheme_Object *)menv2);
	    scheme_hash_set(to_env->module_registry, name, (Scheme_Object *)menv2->module);
	    scheme_hash_set(to_env->export_registry, name, (Scheme_Object *)menv2->module->me);
	    
	    /* Push name onto notify list: */
	    if (!same_namespace)
	      notifies = scheme_make_pair(name, notifies);
	  }
	}
      }
    }
    
    future_checkeds = SCHEME_CDR(future_checkeds);
    from_modchain = SCHEME_VEC_ELS(from_modchain)[1];
    to_modchain = SCHEME_VEC_ELS(to_modchain)[1];
    phase++;
    /* Preceding scheme_clone_module_env ensures that we don't get a
       #f for to_modchain if there's more to do. */
  }

  /* Notify module name resolver of attached modules: */
  {
    Scheme_Cont_Frame_Data cframe;
    Scheme_Config *config;

    config = scheme_current_config();
    
    if (set_env_for_notify) {
      config = scheme_extend_config(scheme_current_config(),
				    MZCONFIG_ENV,
				    (Scheme_Object *)to_env);
  
      scheme_push_continuation_frame(&cframe);
      scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);
    }

    resolver = scheme_get_param(config, MZCONFIG_CURRENT_MODULE_RESOLVER);
    while (!SCHEME_NULLP(notifies)) {
      a[0] = SCHEME_CAR(notifies);
      
      scheme_apply(resolver, 1, a);
      
      notifies = SCHEME_CDR(notifies);
    }

    if (set_env_for_notify) {
      scheme_pop_continuation_frame(&cframe);
    }
  }

  return scheme_void;
}

static Scheme_Object *namespace_unprotect_module(int argc, Scheme_Object *argv[])
{
  Scheme_Env *to_env, *menv2;
  Scheme_Object *name, *to_modchain, *insp, *code_insp;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_inspector_type))
    scheme_wrong_type("namespace-unprotect-module", "inspector", 0, argc, argv);

  insp = argv[0];
  if (argc > 2)
    to_env = (Scheme_Env *)argv[2];
  else
    to_env = scheme_get_env(NULL);

  if (SCHEME_SYMBOLP(argv[1]))
    name = argv[1];
  else
    name = scheme_module_resolve(scheme_make_modidx(argv[1], scheme_false, scheme_false), 0);

  to_modchain = to_env->modchain;

  code_insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);

  if (!SAME_OBJ(name, kernel_symbol)) {
    menv2 = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(to_modchain), name);

    if (!menv2) {
      scheme_arg_mismatch("namespace-unprotect-module",
			  "module not instantiated (in the target namespace): ",
			  name);
    }

    if (!scheme_module_protected_wrt(menv2->insp, insp) && !menv2->attached) {
      code_insp = scheme_make_inspector(code_insp);
      menv2->insp = code_insp;
    }
  }

  return scheme_void;
}

static int do_add_require_renames(Scheme_Object *rn, 
                                  Scheme_Hash_Table *required, Scheme_Object *orig_src,
                                  Scheme_Module *im, Scheme_Module_Phase_Exports *pt,
                                  Scheme_Object *idx,
                                  int marshal_k)
{
  int i, saw_mb, numvals;
  Scheme_Object **exs, **exss, **exsns, *midx, *info, *vec, *nml;
  char *exets;

  saw_mb = 0;

  exs = pt->provides;
  exsns = pt->provide_src_names;
  exss = pt->provide_srcs;
  exets = pt->provide_src_phases;
  numvals = pt->num_var_provides;
  for (i = pt->num_provides; i--; ) {
    if (exss && !SCHEME_FALSEP(exss[i]))
      midx = scheme_modidx_shift(exss[i], im->me->src_modidx, idx);
    else
      midx = idx;
    scheme_extend_module_rename(rn, midx, exs[i], exsns[i], idx, exs[i], 
                                exets ? exets[i] : 0, 1);
    if (SAME_OBJ(exs[i], module_begin_symbol))
      saw_mb = 1;

    if (required) {
      vec = scheme_make_vector(6, NULL);
      nml = scheme_make_pair(idx, scheme_null);
      SCHEME_VEC_ELS(vec)[0] = nml;
      SCHEME_VEC_ELS(vec)[1] = midx;
      SCHEME_VEC_ELS(vec)[2] = exsns[i];
      SCHEME_VEC_ELS(vec)[3] = ((i < numvals) ? scheme_true : scheme_false);
      SCHEME_VEC_ELS(vec)[4] = exs[i];
      SCHEME_VEC_ELS(vec)[5] = orig_src;
      scheme_hash_set(required, exs[i], vec);
    }
  }
  
  if (pt->reprovide_kernel) {
    scheme_extend_module_rename_with_kernel(rn, idx);
    saw_mb = 1;

    if (required) {
      exs = kernel->me->rt->provides;
      numvals = kernel->me->rt->num_var_provides;
      for (i = kernel->me->rt->num_provides; i--; ) {
        if (!SAME_OBJ(pt->kernel_exclusion, exs[i])) {
          vec = scheme_make_vector(6, NULL);
          nml = scheme_make_pair(idx, scheme_null);
          SCHEME_VEC_ELS(vec)[0] = nml;
          SCHEME_VEC_ELS(vec)[1] = kernel_symbol;
          SCHEME_VEC_ELS(vec)[2] = exs[i];
          SCHEME_VEC_ELS(vec)[3] = ((i < numvals) ? scheme_true : scheme_false);
          SCHEME_VEC_ELS(vec)[4] = exs[i];
          SCHEME_VEC_ELS(vec)[5] = orig_src;
          scheme_hash_set(required, exs[i], vec);
        }
      }
    }
  }

  info = cons(idx, cons(scheme_make_integer(marshal_k), 
                        cons(scheme_null, scheme_false)));
  scheme_save_module_rename_unmarshal(rn, info);

  return saw_mb;
}

static int add_initial_require_renames(Scheme_Object *orig_src,
                                       Scheme_Object *rn, Scheme_Hash_Table *rn_required,
                                       Scheme_Object *et_rn, Scheme_Hash_Table *et_required,
                                       Scheme_Object *dt_rn, Scheme_Hash_Table *dt_required,
                                       Scheme_Module *im, Scheme_Object *idx)
{
  int saw_mb;

  if (rn)
    saw_mb = do_add_require_renames(rn, rn_required, orig_src, im, im->me->rt, idx, 0);
  else
    saw_mb = 0;
  if (et_rn && im->me->et)
    do_add_require_renames(et_rn, et_required, orig_src, im, im->me->et, idx, 1);
  if (dt_rn && im->me->dt)
    do_add_require_renames(dt_rn, dt_required, orig_src, im, im->me->dt, idx, 2);

  return saw_mb;
}

static int add_require_renames(Scheme_Object *rn, Scheme_Object *et_rn, Scheme_Object *dt_rn, 
                               Scheme_Module *im, Scheme_Object *idx)
{
  return add_initial_require_renames(NULL, rn, NULL, et_rn, NULL, dt_rn, NULL, im, idx);
}

static Scheme_Object *module_to_namespace(int argc, Scheme_Object *argv[])
{
  Scheme_Env *menv, *env;
  Scheme_Object *modchain, *name;

  env = scheme_get_env(NULL);

  name = scheme_module_resolve(scheme_make_modidx(argv[0], scheme_false, scheme_false), 1);

  modchain = env->modchain;
  menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(modchain), name);
  if (!menv) {
    if (scheme_hash_get(env->module_registry, name))
      scheme_arg_mismatch("module->namespace",
			  "module not instantiated in the current namespace: ",
			  name);
    else
      scheme_arg_mismatch("module->namespace",
			  "unknown module in the current namespace: ",
			  name);
  }

  {
    Scheme_Object *insp;
    insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);
    if (scheme_module_protected_wrt(menv->insp, insp) || menv->attached) {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "module->namespace: current code inspector cannot access namespace of module: %S",
		       name);
    }
  }

  if (!menv->rename) {
    if (menv->module->rn_stx) {
      Scheme_Object *v, *rn;
      Scheme_Module *m = menv->module;

      if (SAME_OBJ(scheme_true, m->rn_stx)) {
	/* Reconstruct renames based on defns and requires */
	int i;
	Scheme_Module *im;
	Scheme_Object *l, *idx;
	Scheme_Hash_Table *mn_ht;

	if (menv->marked_names)
	  mn_ht = menv->marked_names;
	else {
	  mn_ht = scheme_make_hash_table(SCHEME_hash_ptr);
	  menv->marked_names = mn_ht;
	}

	rn = scheme_make_module_rename(0, mzMOD_RENAME_NORMAL, mn_ht);

	/* Local, provided: */
	for (i = 0; i < m->me->rt->num_provides; i++) {
	  if (SCHEME_FALSEP(m->me->rt->provide_srcs[i])) {
	    name = m->me->rt->provides[i];
	    scheme_extend_module_rename(rn, m->self_modidx, name, name, m->self_modidx, name, 0, 0);
	  }
	}
	/* Local, not provided: */
	for (i = 0; i < m->num_indirect_provides; i++) {
	  name = m->indirect_provides[i];
	  scheme_extend_module_rename(rn, m->self_modidx, name, name, m->self_modidx, name, 0, 0);
	}

	/* Required: */
	for (l = menv->require_names; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
	  idx = SCHEME_CAR(l);
	  name = scheme_module_resolve(idx, 0);

	  if (SAME_OBJ(name, kernel_symbol))
	    im = kernel;
	  else
	    im = (Scheme_Module *)scheme_hash_get(menv->module_registry, name);

	  add_require_renames(rn, NULL, NULL, im, idx);
	}
	
	rn = scheme_rename_to_stx(rn);
	m->rn_stx = rn;
      } else if (SCHEME_PAIRP(m->rn_stx)) {
	/* Delayed shift: */
	Scheme_Object *rn_stx, *rn, *midx;
	rn_stx = SCHEME_CAR(m->rn_stx);
	midx = SCHEME_CDR(m->rn_stx);
	rn = scheme_stx_to_rename(rn_stx);
	rn = scheme_stx_shift_rename(rn, midx, m->self_modidx);
	rn_stx = scheme_rename_to_stx(rn);
	m->rn_stx = rn_stx;
      }

      v = scheme_stx_to_rename(m->rn_stx);
      rn = scheme_make_module_rename(0, mzMOD_RENAME_NORMAL, NULL);
      scheme_append_module_rename(v, rn);
      menv->rename = rn;
      if (!menv->marked_names) {
	Scheme_Hash_Table *mn;
	mn = scheme_module_rename_marked_names(rn);
	menv->marked_names = mn;
      }
    }
  }

  if (menv->lazy_syntax)
    finish_expstart_module(menv, env, 0, scheme_null);
  scheme_prepare_exp_env(menv);

  if (!menv->exp_env->rename) {
    Scheme_Module *m = menv->module;

    if (m->et_rn_stx) {
      Scheme_Object *v, *rn;

      if (SAME_OBJ(scheme_true, m->et_rn_stx)) {
	/* Reconstruct renames based on defns and requires */
	Scheme_Module *im;
	Scheme_Object *l, *idx;
	Scheme_Hash_Table *mn_ht;

	if (menv->exp_env->marked_names)
	  mn_ht = menv->exp_env->marked_names;
	else {
	  mn_ht = scheme_make_hash_table(SCHEME_hash_ptr);
	  menv->exp_env->marked_names = mn_ht;
	}

	rn = scheme_make_module_rename(0, mzMOD_RENAME_NORMAL, mn_ht);

	/* Required for syntax: */
	for (l = menv->et_require_names; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
	  idx = SCHEME_CAR(l);
	  name = scheme_module_resolve(idx, 0);
          if (SAME_OBJ(name, kernel_symbol))
	    im = kernel;
	  else
            im = (Scheme_Module *)scheme_hash_get(menv->module_registry, name);
	 
	  add_require_renames(rn, NULL, NULL, im, idx);
	}
	/* Required, maybe has for-syntax exports: */
	for (l = menv->require_names; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
	  idx = SCHEME_CAR(l);
	  name = scheme_module_resolve(idx, 0);

	  if (SAME_OBJ(name, kernel_symbol))
	    im = kernel;
	  else
	    im = (Scheme_Module *)scheme_hash_get(menv->module_registry, name);

	  add_require_renames(NULL, rn, NULL, im, idx);
	}

	rn = scheme_rename_to_stx(rn);
	m->et_rn_stx = rn;
      } else if (SCHEME_PAIRP(m->et_rn_stx)) {
	/* Delayed shift: */
	Scheme_Object *et_rn_stx, *rn, *midx;
	et_rn_stx = SCHEME_CAR(m->et_rn_stx);
	midx = SCHEME_CDR(m->et_rn_stx);
	rn = scheme_stx_to_rename(et_rn_stx);
	rn = scheme_stx_shift_rename(rn, midx, m->self_modidx);
	et_rn_stx = scheme_rename_to_stx(rn);
	m->et_rn_stx = et_rn_stx;
      }

      v = scheme_stx_to_rename(m->et_rn_stx);
      rn = scheme_make_module_rename(1, mzMOD_RENAME_NORMAL, NULL);
      scheme_append_module_rename(v, rn);
      menv->exp_env->rename = rn;
      if (!menv->exp_env->marked_names) {
	Scheme_Hash_Table *mn;
	mn = scheme_module_rename_marked_names(rn);
	menv->exp_env->marked_names = mn;
      }
    }
  }

  if (!menv->dt_rename) {
    Scheme_Module *m = menv->module;

    if (m->dt_rn_stx) {
      Scheme_Object *v, *rn;

      if (SAME_OBJ(scheme_true, m->dt_rn_stx)) {
	/* Reconstruct renames based on requires */
	Scheme_Module *im;
	Scheme_Object *l, *idx;
	Scheme_Hash_Table *mn_ht;

	if (menv->marked_names)
	  mn_ht = menv->marked_names;
	else {
	  mn_ht = scheme_make_hash_table(SCHEME_hash_ptr);
	  menv->marked_names = mn_ht;
	}

	rn = scheme_make_module_rename(MZ_LABEL_PHASE, mzMOD_RENAME_NORMAL, mn_ht);

	/* Required for label: */
	for (l = menv->dt_require_names; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
	  idx = SCHEME_CAR(l);
	  name = scheme_module_resolve(idx, 0);
          if (SAME_OBJ(name, kernel_symbol))
	    im = kernel;
	  else
            im = (Scheme_Module *)scheme_hash_get(menv->module_registry, name);
	 
	  add_require_renames(rn, NULL, NULL, im, idx);
	}
	/* Required, maybe has for-label exports: */
	for (l = menv->require_names; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
	  idx = SCHEME_CAR(l);
	  name = scheme_module_resolve(idx, 0);

	  if (SAME_OBJ(name, kernel_symbol))
	    im = kernel;
	  else
	    im = (Scheme_Module *)scheme_hash_get(menv->module_registry, name);

	  add_require_renames(NULL, NULL, rn, im, idx);
	}

	rn = scheme_rename_to_stx(rn);
	m->dt_rn_stx = rn;
      } else if (SCHEME_PAIRP(m->dt_rn_stx)) {
	/* Delayed shift: */
	Scheme_Object *dt_rn_stx, *rn, *midx;
	dt_rn_stx = SCHEME_CAR(m->dt_rn_stx);
	midx = SCHEME_CDR(m->dt_rn_stx);
	rn = scheme_stx_to_rename(dt_rn_stx);
	rn = scheme_stx_shift_rename(rn, midx, m->self_modidx);
	dt_rn_stx = scheme_rename_to_stx(rn);
	m->dt_rn_stx = dt_rn_stx;
      }

      v = scheme_stx_to_rename(m->dt_rn_stx);
      rn = scheme_make_module_rename(MZ_LABEL_PHASE, mzMOD_RENAME_NORMAL, NULL);
      scheme_append_module_rename(v, rn);
      menv->dt_rename = rn;
      if (!menv->marked_names) {
	Scheme_Hash_Table *mn;
	mn = scheme_module_rename_marked_names(rn);
	menv->marked_names = mn;
      }
    }
  }

  return (Scheme_Object *)menv;
}

static Scheme_Object *module_compiled_p(int argc, Scheme_Object *argv[])
{
  Scheme_Module *m;

  m = scheme_extract_compiled_module(argv[0]);
      
  return (m ? scheme_true : scheme_false);
}

static Scheme_Object *module_compiled_name(int argc, Scheme_Object *argv[])
{
  Scheme_Module *m;

  m = scheme_extract_compiled_module(argv[0]);
      
  if (m) {
    return m->modname;
  }

  scheme_wrong_type("module-compiled-name", "compiled module declaration", 0, argc, argv);
  return NULL;
}

static Scheme_Object *module_compiled_imports(int argc, Scheme_Object *argv[])
{
  Scheme_Module *m;
  Scheme_Object *a[4];

  m = scheme_extract_compiled_module(argv[0]);

  if (m) {
    /* Ensure that the lists are immutable: */
    scheme_make_list_immutable(m->requires);
    scheme_make_list_immutable(m->et_requires);
    scheme_make_list_immutable(m->tt_requires);
    scheme_make_list_immutable(m->dt_requires);
    
    a[0] = m->requires;
    a[1] = m->et_requires;
    a[2] = m->tt_requires;
    a[3] = m->dt_requires;
    
    return scheme_values(4, a);
  }

  scheme_wrong_type("module-compiled-imports", "compiled module declaration", 0, argc, argv);
  return NULL;
}

static Scheme_Object *module_compiled_exports(int argc, Scheme_Object *argv[])
{
  Scheme_Module *m;
  Scheme_Object *a[6];
  Scheme_Object *ml, *vl;
  Scheme_Module_Phase_Exports *pt;
  int i, n, k;

  m = scheme_extract_compiled_module(argv[0]);

  if (m) {
    for (k = 0; k < 3; k++) {
      switch(k) {
      case 0:
        pt = m->me->rt;
        break;
      case 1:
        pt = m->me->et;
        break;
      case 2:
      default:
        pt = m->me->dt;
        break;
      }

      ml = scheme_null;
      vl = scheme_null;
      n = pt->num_var_provides;
      for (i = pt->num_provides - 1; i >= n; --i) {
        ml = scheme_make_immutable_pair(pt->provides[i], ml);
      }
      for (; i >= 0; --i) {
        vl = scheme_make_immutable_pair(pt->provides[i], vl);
      }
      
      a[2 * k] = vl;
      a[(2 * k) + 1] = ml;
    }
    
    return scheme_values(6, a);
  }

  scheme_wrong_type("module-compiled-exports", "compiled module declaration", 0, argc, argv);
  return NULL;
}

static Scheme_Object *module_path_index_p(int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_module_index_type)
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *module_path_index_resolve(int argc, Scheme_Object *argv[])
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_module_index_type))
    scheme_wrong_type("module-path-index-resolve", "module-path-index", 0, argc, argv);

  return scheme_module_resolve(argv[0], 0);
}

static Scheme_Object *module_path_index_split(int argc, Scheme_Object *argv[])
{
  Scheme_Modidx *modidx;
  Scheme_Object *a[2];

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_module_index_type))
    scheme_wrong_type("module-path-index-split", "module-path-index", 0, argc, argv);

  modidx = (Scheme_Modidx *)argv[0];
  a[0] = modidx->path;
  a[1] = modidx->base;

  return scheme_values(2, a);
}

static Scheme_Object *module_path_index_join(int argc, Scheme_Object *argv[])
{
  if (SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("module-path-index-join", "non-symbol", 0, argc, argv);

  if (argv[1]) { /* mzc will generate NULL sometimes; see scheme_declare_module(), below */
    if (SCHEME_TRUEP(argv[1])
	&& !SCHEME_SYMBOLP(argv[1])
	&& !SAME_TYPE(SCHEME_TYPE(argv[1]), scheme_module_index_type))
      scheme_wrong_type("module-path-index-join", "module-path-index, symbol, or #f", 1, argc, argv);
  }

  return scheme_make_modidx(argv[0], argv[1], scheme_false);
}

static Scheme_Object *module_export_protected_p(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;
  Scheme_Object *modname, *mv, *name;
  Scheme_Module *m;
  int i, count;

  if (!SCHEME_SYMBOLP(argv[0])
      && !SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_module_index_type))
    scheme_wrong_type("module-provide-protected?", "symbol or module-path-index", 0, argc, argv);
  if (!SCHEME_SYMBOLP(argv[1]))
    scheme_wrong_type("module-provide-protected?", "symbol", 1, argc, argv);

  modname = scheme_module_resolve(argv[0], 1);
  name = argv[1];

  env = scheme_get_env(NULL);
  if (SAME_OBJ(modname, kernel_symbol))
    mv = (Scheme_Object *)kernel;
  else
    mv = scheme_hash_get(env->module_registry, modname);
  if (!mv) {
    scheme_arg_mismatch("module-provide-protected?",
			"unknown module (in the source namespace): ",
			modname);
    return NULL;
  }

  m = (Scheme_Module *)mv;

  count = m->me->rt->num_provides;
  for (i = 0; i < count; i++) {
    if (SAME_OBJ(name, m->me->rt->provides[i])) {
      if (m->provide_protects && m->provide_protects[i])
	return scheme_true;
      else
	return scheme_false;
    }
  }

  return scheme_true;
}

/**********************************************************************/
/*                       basic module operations                      */
/**********************************************************************/

Scheme_Object *scheme_make_modidx(Scheme_Object *path, 
				  Scheme_Object *base_modidx,
				  Scheme_Object *resolved)
{
  Scheme_Modidx *modidx;

  if (SCHEME_SYMBOLP(path))
    return path;

  modidx = MALLOC_ONE_TAGGED(Scheme_Modidx);
  modidx->so.type = scheme_module_index_type;
  modidx->path = path;
  modidx->base = base_modidx;
  modidx->resolved = resolved;
  
  return (Scheme_Object *)modidx;
}

int same_modidx(Scheme_Object *a, Scheme_Object *b)
{
  if (SAME_TYPE(SCHEME_TYPE(a), scheme_module_index_type))
    a = ((Scheme_Modidx *)a)->path;
  if (SAME_TYPE(SCHEME_TYPE(b), scheme_module_index_type))
    b = ((Scheme_Modidx *)b)->path;

  return scheme_equal(a, b);
}

int same_resolved_modidx(Scheme_Object *a, Scheme_Object *b)
{
  if (SAME_TYPE(SCHEME_TYPE(a), scheme_module_index_type))
    a = scheme_module_resolve(a, 1);
  if (SAME_TYPE(SCHEME_TYPE(b), scheme_module_index_type))
    b = scheme_module_resolve(b, 1);

  return scheme_equal(a, b);
}

static Scheme_Object *_module_resolve_k(void);

static Scheme_Object *_module_resolve(Scheme_Object *modidx, Scheme_Object *stx, int load_it)
{
  if (SCHEME_SYMBOLP(modidx) || SCHEME_FALSEP(modidx))
    return modidx;

  if (SAME_OBJ(modidx, empty_self_modidx))
    return empty_self_symbol;

  if (SCHEME_FALSEP(((Scheme_Modidx *)modidx)->resolved)) {
    /* Need to resolve access path to a module name: */
    Scheme_Object *a[4];
    Scheme_Object *name, *base;
    
    base = ((Scheme_Modidx *)modidx)->base;
    if (!SCHEME_FALSEP(base)) {
# include "mzstkchk.h"
      {
	Scheme_Thread *p = scheme_current_thread;
	p->ku.k.p1 = (void *)base;
	p->ku.k.i1 = load_it;
	base = scheme_handle_stack_overflow(_module_resolve_k);
      } else {
	base = _module_resolve(base, NULL, load_it);
      }
    }

    a[0] = ((Scheme_Modidx *)modidx)->path;
    a[1] = base;
    a[2] = (stx ? stx : scheme_false);
    a[3] = (load_it ? scheme_true : scheme_false);
    
    if (SCHEME_FALSEP(a[0])) {
      scheme_wrong_syntax("require", NULL, NULL, 
			  "broken compiled/expanded code: unresolved module index without path");
    }

    name = scheme_apply(scheme_get_param(scheme_current_config(), MZCONFIG_CURRENT_MODULE_RESOLVER), 4, a);
    
    if (!SCHEME_SYMBOLP(name)) {
      a[0] = name;
      scheme_wrong_type("module name resolver", "symbol", -1, -1, a);
    }

    ((Scheme_Modidx *)modidx)->resolved = name;
  }

  return ((Scheme_Modidx *)modidx)->resolved;
}

static Scheme_Object *_module_resolve_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *base = (Scheme_Object *)p->ku.k.p1;

  p->ku.k.p1 = NULL;

  return _module_resolve(base, NULL, p->ku.k.i1);
}

Scheme_Object *scheme_module_resolve(Scheme_Object *modidx, int load_it)
{
  return _module_resolve(modidx, NULL, load_it);
}

Scheme_Object *scheme_modidx_shift(Scheme_Object *modidx, 
				   Scheme_Object *shift_from_modidx,
				   Scheme_Object *shift_to_modidx)
{
  Scheme_Object *base;

  if (!shift_to_modidx)
    return modidx;

  if (SAME_OBJ(modidx, shift_from_modidx))
    return shift_to_modidx;

  if (!SAME_TYPE(SCHEME_TYPE(modidx), scheme_module_index_type))
    return modidx;
  
  /* Need to shift relative part? */
  base = ((Scheme_Modidx *)modidx)->base;
  if (!SCHEME_FALSEP(base)) {
    /* FIXME: depth */
    Scheme_Object *sbase;
    sbase = scheme_modidx_shift(base, shift_from_modidx, shift_to_modidx);

    if (!SAME_OBJ(base, sbase)) {
      /* There was a shift in the relative part. */
      Scheme_Modidx *sbm;
      int i, c;
      Scheme_Object *smodidx, *cvec;

      /* Shift cached? sbase as a symbol is rare, but we need at least a little
         caching to make other things (e.g., .zo output) compact, so we use
         a small global cache in that case. */

      if (SCHEME_SYMBOLP(sbase)) {
	sbm = NULL;
	cvec = global_shift_cache;
      } else {
	sbm = (Scheme_Modidx *)sbase;
	cvec = sbm->shift_cache;
      }

      c = (cvec ? SCHEME_VEC_SIZE(cvec) : 0);
      
      for (i = 0; i < c; i += 2) {
	if (SHIFT_CACHE_NULLP(SCHEME_VEC_ELS(cvec)[i]))
	  break;
	if (SAME_OBJ(modidx, SCHEME_VEC_ELS(cvec)[i]))
	  return SCHEME_VEC_ELS(cvec)[i + 1];
      }
      
      smodidx = scheme_make_modidx(((Scheme_Modidx *)modidx)->path,
				   sbase,
				   scheme_false);

      if (!sbm) {
	if (!global_shift_cache)
	  global_shift_cache = scheme_make_vector(GLOBAL_SHIFT_CACHE_SIZE, SHIFT_CACHE_NULL);
	for (i = 0; i < (GLOBAL_SHIFT_CACHE_SIZE - 2); i++) {
	  SCHEME_VEC_ELS(global_shift_cache)[i+2] = SCHEME_VEC_ELS(global_shift_cache)[i];
	}
	SCHEME_VEC_ELS(global_shift_cache)[0] = modidx;
	SCHEME_VEC_ELS(global_shift_cache)[1] = smodidx;
      } else {
	/* May have GCed: */
	if (cvec && !sbm->shift_cache)
	  sbm->shift_cache = cvec;

	if (i >= c) {
	  /* Grow cache vector */
	  Scheme_Object *naya;
	  int j;
	    
	  naya = scheme_make_vector(c + 10, SHIFT_CACHE_NULL);
	  for (j = 0; j < c; j++) {
	    SCHEME_VEC_ELS(naya)[j] = SCHEME_VEC_ELS(cvec)[j];
	  }
	  if (!sbm->shift_cache) {
	    sbm->cache_next = modidx_caching_chain;
	    modidx_caching_chain = sbm;
	  }

	  sbm->shift_cache = naya;
	}
	  
	SCHEME_VEC_ELS(sbm->shift_cache)[i] = modidx;
	SCHEME_VEC_ELS(sbm->shift_cache)[i+1] = smodidx;
      }

      return smodidx;
    }
  }

  return modidx;
}

void scheme_clear_modidx_cache(void)
{
  Scheme_Modidx *sbm, *next;

  global_shift_cache = NULL;
  
  for (sbm = modidx_caching_chain; sbm; sbm = next) {
    sbm->shift_cache = NULL;
    next = sbm->cache_next;
    sbm->cache_next = NULL;
  }
  modidx_caching_chain = NULL;
}

static Scheme_Module *module_load(Scheme_Object *name, Scheme_Env *env, const char *who)
{
  if (name == kernel_symbol)
    return kernel;
  else {
    Scheme_Module *m;

    m = (Scheme_Module *)scheme_hash_get(env->module_registry, name);

    if (!m) {
      char *mred_note;

      if (!strcmp(SCHEME_SYM_VAL(name), "#%mred-kernel")
	  && !(scheme_strncmp(scheme_banner(), "Welcome to MzScheme", 19)))
	mred_note = "; need to run in MrEd instead of MzScheme";
      else
	mred_note = "";

      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "%s: unknown module: %S%s",
		       who ? who : "require", 
		       name, mred_note);
      return NULL;
    }

    return m;
  }
}

static void setup_accessible_table(Scheme_Module *m)
{
  if (!m->accessible) {
    Scheme_Hash_Table *ht;
    int i, count, nvp;

    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    nvp = m->me->rt->num_var_provides;
    for (i = 0; i < nvp; i++) {
      if (SCHEME_FALSEP(m->me->rt->provide_srcs[i])) {
	scheme_hash_set(ht, m->me->rt->provide_src_names[i], scheme_make_integer(i));
      }
    }

    count = m->num_indirect_provides;
    for (i = 0; i < count; i++) {
      scheme_hash_set(ht, m->indirect_provides[i], scheme_make_integer(i + nvp));
    }
    m->accessible = ht;
    
    /* Add syntax as negative ids: */
    count = m->me->rt->num_provides;
    for (i = nvp; i < count; i++) {
      if (SCHEME_FALSEP(m->me->rt->provide_srcs[i])) {
	scheme_hash_set(ht, m->me->rt->provide_src_names[i], scheme_make_integer(-(i+1)));
      }
    }
  }
}

Scheme_Env *scheme_module_access(Scheme_Object *name, Scheme_Env *env, int rev_mod_phase)
{
  if ((name == kernel_symbol) && !rev_mod_phase)
    return scheme_initial_env;
  else {
    Scheme_Object *chain;
    Scheme_Env *menv;

    chain = env->modchain;
    if (rev_mod_phase && chain) {
      chain = (SCHEME_VEC_ELS(chain))[2];
      if (SCHEME_FALSEP(chain))
	return NULL;
    }

    if (!chain) {
      scheme_signal_error("internal error: missing chain for module instances");
      return NULL;
    }

    menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(chain), name);
    
    if (rev_mod_phase && menv)
      menv = menv->exp_env;

    return menv;
  }
}

static void check_certified(Scheme_Object *stx, Scheme_Object *certs, 
			    Scheme_Object *insp, Scheme_Object *in_modidx,
			    Scheme_Env *env, Scheme_Object *symbol,
			    int var, int prot)
{
  int need_cert = 1;
  Scheme_Object *midx;

  midx = (env->link_midx ? env->link_midx : env->module->me->src_modidx);
    
  if (stx)
    need_cert = !scheme_stx_certified(stx, certs, prot ? NULL : midx, env->insp);

  if (need_cert && insp)
    need_cert = scheme_module_protected_wrt(env->insp, insp);

  if (need_cert && in_modidx) {
    /* If we're currently executing a macro expander in this module,
       then allow the access under any cirsumstances. This is mostly
       useful for syntax-local-value and local-expand. */
    in_modidx = scheme_module_resolve(in_modidx, 0);
    midx = scheme_module_resolve(midx, 0);
    if (SAME_OBJ(in_modidx, midx))
      need_cert = 0;
  }

  if (need_cert) {
    /* For error, if stx is no more specific than symbol, drop symbol. */
    if (stx && SAME_OBJ(SCHEME_STX_SYM(stx), symbol)) {
      symbol = stx;
      stx = NULL;
    }
    scheme_wrong_syntax("compile", stx, symbol, 
			"access from an uncertified context to %s %s from module: %S",
			prot ? "protected" : "unexported",
			var ? "variable" : "syntax",
			env->module->modname);
  }
}

Scheme_Object *scheme_check_accessible_in_module(Scheme_Env *env, Scheme_Object *prot_insp, Scheme_Object *in_modidx,
						 Scheme_Object *symbol, Scheme_Object *stx,
						 Scheme_Object *certs, Scheme_Object *unexp_insp,
						 int position, int want_pos, int *_protected)
     /* Returns the actual name when !want_pos, needed in case of
	uninterned names.  Otherwise, returns a position value on success.
	If position < -1, then merely checks for protected syntax.

	Access for protected and unexported names depends on
	certifictions in stx+certs, access implied by
	{prot_,unexp_}insp, or access implied by in_modidx. For
	unexported access, either stx+certs or unexp_insp must be
	supplied (not both). For unprotected access, both prot_insp
	and stx+certs should be supplied. */
{
  symbol = scheme_tl_id_sym(env, symbol, NULL, 0);

  if ((env == scheme_initial_env)
      || ((env->module->primitive
           && !env->module->provide_protects))
      /* For now[?], we're pretending that all definitions exists for
	 non-0 local phase. */
      || env->mod_phase) {
    if (want_pos)
      return scheme_make_integer(-1);
    else
      return symbol;
  }

  if (position >= 0) {
    /* Check whether the symbol at `pos' matches the string part of
       the expected symbol.  */
    Scheme_Object *isym;
    int need_cert = 0;

    if (position < env->module->me->rt->num_var_provides) {
      if (!env->module->me->rt->provide_srcs
          || SCHEME_FALSEP(env->module->me->rt->provide_srcs[position]))
	isym = env->module->me->rt->provide_src_names[position];
      else
	isym = NULL;
    } else {
      int ipos = position - env->module->me->rt->num_var_provides;
      if (ipos < env->module->num_indirect_provides) {
	isym = env->module->indirect_provides[ipos];
	need_cert = 1;
	if (_protected)
	  *_protected = 1;
      } else
	isym = NULL;
    }

    if (isym) {
      if (SAME_OBJ(isym, symbol)
	  || (SCHEME_SYM_LEN(isym) == SCHEME_SYM_LEN(symbol)
	      && !memcmp(SCHEME_SYM_VAL(isym), SCHEME_SYM_VAL(symbol), SCHEME_SYM_LEN(isym)))) {
	
	if ((position < env->module->me->rt->num_var_provides)
	    && scheme_module_protected_wrt(env->insp, prot_insp)
	    && env->module->provide_protects
	    && env->module->provide_protects[position]) {
	  if (_protected)
	    *_protected = 1;
	  check_certified(stx, certs, prot_insp, in_modidx, env, symbol, 1, 1);
	}

	if (need_cert)
	  check_certified(stx, certs, unexp_insp, in_modidx, env, symbol, 1, 0);
	
	if (want_pos)
	  return scheme_make_integer(position);
	else
	  return isym;
      } 
    }
    /* failure */
  } else {
    Scheme_Object *pos;

    pos = scheme_hash_get(env->module->accessible, symbol);

    if (pos) {
      if (position < -1) {
	if (SCHEME_INT_VAL(pos) < 0)
	  pos = scheme_make_integer(-SCHEME_INT_VAL(pos) - 1);
	else
	  pos = NULL;
      } else {
	if (SCHEME_INT_VAL(pos) < 0)
	  pos = NULL;
      }
    }

    if (pos) {
      if (env->module->provide_protects
	  && (SCHEME_INT_VAL(pos) < env->module->me->rt->num_provides)
	  && env->module->provide_protects[SCHEME_INT_VAL(pos)]) {
	if (_protected)
	  *_protected = 1;
	check_certified(stx, certs, prot_insp, in_modidx, env, symbol, 1, 1);
      }

      if ((position >= -1) 
	  && (SCHEME_INT_VAL(pos) >= env->module->me->rt->num_var_provides)) {
	/* unexported var -- need cert */
	if (_protected)
	  *_protected = 1;
	check_certified(stx, certs, unexp_insp, in_modidx, env, symbol, 1, 0);
      }

      if (want_pos)
        return pos;
      else
	return symbol;
    }

    if (position < -1) {
      /* unexported syntax -- need cert */
      check_certified(stx, certs, unexp_insp, in_modidx, env, symbol, 0, 0);
      return NULL;
    }
  }

  /* For error, if stx is no more specific than symbol, drop symbol. */
  if (stx && SAME_OBJ(SCHEME_STX_SYM(stx), symbol)) {
    symbol = stx;
    stx = NULL;
  }
  scheme_wrong_syntax("compile", stx, symbol, 
		      "variable not provided (directly or indirectly%s) from module: %S",
		      (position >= 0) ? " and at the expected position" : "",
		      env->module->modname);
  return NULL;
}

int scheme_module_export_position(Scheme_Object *modname, Scheme_Env *env, Scheme_Object *varname)
{
  Scheme_Module *m;
  Scheme_Object *pos;

  if (modname == kernel_symbol)
    return -1;

  m = module_load(modname, env, NULL);
  if (!m || m->primitive)
    return -1;

  setup_accessible_table(m);

  pos = scheme_hash_get(m->accessible, varname);
  
  if (pos && (SCHEME_INT_VAL(pos) >= 0))
    return SCHEME_INT_VAL(pos);
  else
    return -1;
}

Scheme_Object *scheme_module_syntax(Scheme_Object *modname, Scheme_Env *env, Scheme_Object *name)
{
  if (modname == kernel_symbol) {
    name = SCHEME_STX_SYM(name);
    return scheme_lookup_in_table(scheme_initial_env->syntax, (char *)name);
  } else {
    Scheme_Env *menv;
    Scheme_Object *val;

    menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(env->modchain), modname);
    
    if (!menv)
      return NULL;

    if (menv->lazy_syntax) {
      finish_expstart_module_in_namespace(menv, env);
    }

    name = scheme_tl_id_sym(menv, name, NULL, 0);

    val = scheme_lookup_in_table(menv->syntax, (char *)name);

    return val;
  }
}

void scheme_module_force_lazy(Scheme_Env *env, int previous)
{
  Scheme_Object *modchain;
  Scheme_Hash_Table *mht;
  int mi;

  modchain = env->modchain;

  if (previous)
    modchain = SCHEME_VEC_ELS(modchain)[2];
    
  mht = MODCHAIN_TABLE(modchain);
  
  for (mi = mht->size; mi--; ) {
    if (mht->vals[mi]) {
      /* Check this module for lazy syntax. */
      Scheme_Env *menv = (Scheme_Env *)mht->vals[mi];

      if (menv->lazy_syntax)
	finish_expstart_module(menv, env, 0, scheme_null);
    }
  }
}

static void templstart_module(Scheme_Env *menv, Scheme_Env *env, 
			      int with_tt, Scheme_Object *cycle_list)
{
  Scheme_Object *np, *new_cycle_list, *midx, *l;
  Scheme_Module *im;

  new_cycle_list = scheme_make_pair(menv->module->modname, cycle_list);

  np = scheme_null;
  for (l = menv->module->tt_requires; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    midx = scheme_modidx_shift(SCHEME_CAR(l), menv->module->me->src_modidx, menv->link_midx);

    scheme_prepare_template_env(env);
    
    im = module_load(scheme_module_resolve(midx, 1), env, NULL);

    if (with_tt > 1)
      start_module(im, 
		   env->template_env, 0, 
		   midx,
		   0, with_tt - 1,
		   new_cycle_list);
    else
      expstart_module(im, 
		      env->template_env, 0, 
		      midx,
		      0, with_tt - 1,
		      new_cycle_list);
    
    np = cons(midx, np);
  }

  menv->tt_require_names = np;
  if (with_tt)
    menv->tt_running = 1;
  else
    menv->tt_running = -1;
}

static void expstart_module(Scheme_Module *m, Scheme_Env *env, int restart, 
			    Scheme_Object *syntax_idx, int delay_exptime, 
			    int with_tt,
			    Scheme_Object *cycle_list)
{
  Scheme_Env *menv;
  Scheme_Object *l, *midx, *np, *new_cycle_list;
  Scheme_Module *im;

  if (!delay_exptime)
    delay_exptime = m->et_functional;

  for (l = cycle_list; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    if (SAME_OBJ(m->modname, SCHEME_CAR(l))) {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "module: import cycle detected at: %S",
		       m->modname);
    }
  }

  if (SAME_OBJ(m, kernel))
    return;

  if (!restart) {
    menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(env->modchain), m->modname);
    if (menv && menv->et_running) {
      if (!delay_exptime && menv->lazy_syntax)
	finish_expstart_module(menv, env, with_tt, cycle_list);
      else if (((with_tt > 1) && (menv->tt_running <= 0))
	       || ((with_tt > 0) && (menv->tt_running == 0)))
	templstart_module(menv, env, with_tt, cycle_list);
      return;
    }
  }

  if (m->primitive) {
    menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(env->modchain), m->modname);
    if (!menv) {
      menv = m->primitive;
      scheme_hash_set(MODCHAIN_TABLE(env->modchain), m->modname, (Scheme_Object *)menv);
    }
    menv->require_names = scheme_null;
    menv->et_require_names = scheme_null;
    menv->tt_require_names = scheme_null;
    menv->dt_require_names = scheme_null;
    return;
  }

  menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(env->modchain), m->modname);
  if (!menv || restart) {
    if (!menv) {
      Scheme_Object *insp;

      /* printf("new %ld %s\n", env->phase, SCHEME_SYM_VAL(m->modname)); */
      menv = scheme_new_module_env(env, m, 0);
      scheme_hash_set(MODCHAIN_TABLE(env->modchain), m->modname, (Scheme_Object *)menv);
      
      /* These three should be set by various "finish"es, but
         we initialize them in case there's an error runing a "finish". */
      menv->require_names = scheme_null;
      menv->et_require_names = scheme_null;
      menv->tt_require_names = scheme_null;
      menv->dt_require_names = scheme_null;

      menv->phase = env->phase;
      menv->link_midx = syntax_idx;
      insp = scheme_make_inspector(m->insp);
      menv->insp = insp;
    } else {
      menv->module = m;
      menv->running = 0;
      menv->et_running = 0;
    }

    setup_accessible_table(m);

    /* Create provided global variables: */
    {
      Scheme_Object **exss, **exsns;
      int i, count;

      exsns = m->me->rt->provide_src_names;
      exss = m->me->rt->provide_srcs;
      count = m->me->rt->num_var_provides;

      for (i = 0; i < count; i++) {
	if (SCHEME_FALSEP(exss[i]))
	  scheme_add_to_table(menv->toplevel, (const char *)exsns[i], NULL, 0);
      }

      count = m->num_indirect_provides;
      exsns = m->indirect_provides;
      for (i = 0; i < count; i++) {
	scheme_add_to_table(menv->toplevel, (const char *)exsns[i], NULL, 0);
      }
    }
  }
  
  new_cycle_list = scheme_make_pair(m->modname, cycle_list);

  np = scheme_null;

  for (l = m->requires; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    if (syntax_idx)
      midx = scheme_modidx_shift(SCHEME_CAR(l), m->me->src_modidx, syntax_idx);
    else
      midx = scheme_modidx_shift(SCHEME_CAR(l), m->me->src_modidx, m->self_modidx);

    np = cons(midx, np);

    im = module_load(scheme_module_resolve(midx, 1), env, NULL);

    expstart_module(im, 
		    env, 0, 
		    midx,
		    delay_exptime,
		    with_tt,
		    new_cycle_list);
  }

  menv->require_names = np;
  menv->et_running = 1;
  if (scheme_starting_up)
    menv->attached = 1; /* protect initial modules from redefinition, etc. */

  np = scheme_null;
  for (l = m->dt_requires; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    if (syntax_idx)
      midx = scheme_modidx_shift(SCHEME_CAR(l), m->me->src_modidx, syntax_idx);
    else
      midx = scheme_modidx_shift(SCHEME_CAR(l), m->me->src_modidx, m->self_modidx);

    np = cons(midx, np);

    im = module_load(scheme_module_resolve(midx, 1), env, NULL);
  }
  menv->dt_require_names = np;

  if (m->prim_et_body || !SCHEME_NULLP(m->et_body) || !SCHEME_NULLP(m->et_requires)) {
    if (delay_exptime) {
      /* Set lazy-syntax flag. */
      menv->lazy_syntax = 1;
    } else
      finish_expstart_module(menv, env, with_tt, cycle_list);
  } else
    menv->et_require_names = scheme_null;
}

static void finish_expstart_module(Scheme_Env *menv, Scheme_Env *env, 
				   int with_tt, Scheme_Object *cycle_list)
{
  Scheme_Object *l, *body, *e, *names, *midx, *np, *new_cycle_list;
  Scheme_Env *exp_env;
  Scheme_Bucket_Table *syntax, *for_stx_globals;
  Scheme_Module *im;
  int let_depth, for_stx;

  /* Continue a delayed expstart: */
  menv->lazy_syntax = 0;

  new_cycle_list = scheme_make_pair(menv->module->modname, cycle_list);

  /* make sure exptimes of imports have been forced: */
  for (l = menv->require_names; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    midx = SCHEME_CAR(l);
    expstart_module(module_load(scheme_module_resolve(midx, 1), env, NULL), 
		    env, 0,
		    midx,
		    0,
		    with_tt,
		    new_cycle_list);
  }

  /* If a for-syntax require fails, start all over: */
  menv->et_running = 0;

  syntax = menv->syntax;

  scheme_prepare_exp_env(menv);
  exp_env = menv->exp_env;

  /* This line was here to help minimize garbage, I think, but
     with the advent of `begin-for-syntax', we need to keep
     a module's exp_env. */
  /* menv->exp_env = NULL; */

  for_stx_globals = exp_env->toplevel;

  exp_env->link_midx = menv->link_midx;

  np = scheme_null;

  for (l = menv->module->et_requires; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    midx = scheme_modidx_shift(SCHEME_CAR(l), menv->module->me->src_modidx, exp_env->link_midx);

    np = cons(midx, np);

    im = module_load(scheme_module_resolve(midx, 1), env, NULL);

    start_module(im, 
		 exp_env, 0,
		 midx,
		 0, with_tt + 1,
		 new_cycle_list);
  }
  
  menv->et_require_names = np;

  if (SCHEME_NULLP(menv->module->tt_requires))
    menv->tt_running = 1;

  if (((with_tt > 1) && (menv->tt_running <= 0))
      || ((with_tt > 0) && (menv->tt_running == 0))) {
    templstart_module(menv, env, with_tt, cycle_list);
  }

  menv->et_running = 1;

  if (menv->module->prim_et_body) {
    Scheme_Invoke_Proc ivk = menv->module->prim_et_body;
    Scheme_Env *cenv;

    /* To simplify mzc's job, we make up an environment where the
       syntax table is the same as menv, the toplevel table is
       exp_env's, and exp_env itself is exp_env */
    cenv = MALLOC_ONE_TAGGED(Scheme_Env);
    cenv->so.type = scheme_namespace_type;
    cenv->module_registry = menv->module_registry;
    cenv->export_registry = menv->export_registry;
    cenv->module = menv->module;
    cenv->insp = menv->insp;
    cenv->syntax = menv->syntax;
    cenv->toplevel = exp_env->toplevel;
    cenv->exp_env = exp_env;
    cenv->modchain = menv->modchain;

    ivk(cenv, menv->phase, menv->link_midx, menv->module->body);
  } else {
    Resolve_Prefix *rp;
    Scheme_Comp_Env *rhs_env;

    rhs_env = scheme_new_comp_env(menv, menv->module->insp, SCHEME_TOPLEVEL_FRAME);

    for (body = menv->module->et_body; !SCHEME_NULLP(body); body = SCHEME_CDR(body)) {
      e = SCHEME_CAR(body);
      
      names = SCHEME_VEC_ELS(e)[0];
      let_depth = SCHEME_INT_VAL(SCHEME_VEC_ELS(e)[2]);
      rp = (Resolve_Prefix *)SCHEME_VEC_ELS(e)[3];
      for_stx = SCHEME_TRUEP(SCHEME_VEC_ELS(e)[4]);
      e = SCHEME_VEC_ELS(e)[1];
      
      eval_defmacro(names, scheme_proper_list_length(names), e, exp_env, rhs_env,
		    rp, let_depth, 1, (for_stx ? for_stx_globals : syntax), for_stx,
		    NULL);
    }
  }
}

static void finish_expstart_module_in_namespace(Scheme_Env *menv, Scheme_Env *from_env)
{
  Scheme_Cont_Frame_Data cframe;
  Scheme_Config *config;
  
  config = scheme_extend_config(scheme_current_config(),
				MZCONFIG_ENV,
				(Scheme_Object *)from_env);
  
  scheme_push_continuation_frame(&cframe);
  scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);
  
  finish_expstart_module(menv, from_env, 0, scheme_null);
  
  scheme_pop_continuation_frame(&cframe);
}

static void start_module(Scheme_Module *m, Scheme_Env *env, int restart, 
			 Scheme_Object *syntax_idx, int delay_expstart, int with_tt,
			 Scheme_Object *cycle_list)
{
  Scheme_Env *menv;
  Scheme_Object *l, *midx, *new_cycle_list;

  if (SAME_OBJ(m, kernel))
    return;

  for (l = cycle_list; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    if (SAME_OBJ(m->modname, SCHEME_CAR(l))) {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "module: import cycle detected at: %S",
		       m->modname);
    }
  }
  
  expstart_module(m, env, restart, syntax_idx, delay_expstart, with_tt, cycle_list);

  if (m->primitive)
    return;

  menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(env->modchain), m->modname);

  if (restart)
    menv->running = 0;

  if (menv->running > 0)
    return;
  
  new_cycle_list = scheme_make_pair(m->modname, cycle_list);

  for (l = menv->require_names; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    midx = SCHEME_CAR(l);
    start_module(module_load(scheme_module_resolve(midx, 1), env, NULL), 
		 env, 0, 
		 midx,
		 delay_expstart, with_tt,
		 new_cycle_list);
  }

  menv->running = 1;

  if (menv->module->prim_body) {
    Scheme_Invoke_Proc ivk = menv->module->prim_body;
    ivk(menv, menv->phase, menv->link_midx, m->body);
  } else {
    eval_module_body(menv);
  }
}

static void *eval_module_body_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Env *menv;

  menv = (Scheme_Env *)p->ku.k.p1;
  p->ku.k.p1 = NULL;

  eval_module_body(menv);
  
  return NULL;
}

static void eval_module_body(Scheme_Env *menv)
{
  Scheme_Module *m = menv->module;
  Scheme_Object *body, **save_runstack;
  int depth;

  depth = m->max_let_depth + scheme_prefix_depth(m->prefix);
  if (!scheme_check_runstack(depth)) {
    Scheme_Thread *p = scheme_current_thread;
    p->ku.k.p1 = menv;
    (void)scheme_enlarge_runstack(depth, eval_module_body_k);
    return;
  }

  save_runstack = scheme_push_prefix(menv, m->prefix,
				     m->me->src_modidx, menv->link_midx,
				     0, menv->phase);

  body = m->body;
  for (; !SCHEME_NULLP(body); body = SCHEME_CDR(body)) {
    _scheme_eval_linked_expr_multi(SCHEME_CAR(body));
  }

  if (scheme_module_demand_hook) {
    Scheme_Object *a[1], *val, *sym;
    a[0] = menv->module->modname;
    sym = scheme_module_demand_hook(1, a);
    if (sym) {
      val = scheme_lookup_global(sym, menv);
      if (val) {
	a[0] = val;
	val = scheme_module_demand_hook(3, a);
	if (val) {
	  scheme_add_global_symbol(sym, val, menv);
	}
      }
    }
  }

  scheme_pop_prefix(save_runstack);
}

Scheme_Env *scheme_primitive_module(Scheme_Object *name, Scheme_Env *for_env)
{
  Scheme_Module *m;
  Scheme_Env *env;
  Scheme_Object *prefix, *insp;
  Scheme_Config *config;

  m = MALLOC_ONE_TAGGED(Scheme_Module);
  m->so.type = scheme_module_type;
  
  env = scheme_new_module_env(for_env, m, 0);

  config = scheme_current_config();

  prefix = scheme_get_param(config, MZCONFIG_CURRENT_MODULE_PREFIX);
  if (SCHEME_SYMBOLP(prefix))
    name = scheme_symbol_append(prefix, name);
  insp = scheme_get_param(config, MZCONFIG_CODE_INSPECTOR);

  m->modname = name;
  m->requires = scheme_null;
  m->et_requires = scheme_null;
  m->tt_requires = scheme_null;
  m->dt_requires = scheme_null;
  m->primitive = env;
  m->insp = insp;

  {
    Scheme_Module_Exports *me;
    me = make_module_exports();
    m->me = me;
  }

  scheme_hash_set(for_env->export_registry, m->modname, (Scheme_Object *)m->me);

  insp = scheme_make_inspector(insp);
  env->insp = insp;

  scheme_hash_set(for_env->module_registry, m->modname, (Scheme_Object *)m);

  return env;
}

void scheme_finish_primitive_module(Scheme_Env *env)
{
  Scheme_Module *m = env->module;
  Scheme_Bucket_Table *ht;
  Scheme_Bucket **bs;
  Scheme_Object **exs;
  int i, count;

  /* Provide all variables: */
  count = 0;
  ht = env->toplevel;

  bs = ht->buckets;
  for (i = ht->size; i--; ) {
    Scheme_Bucket *b = bs[i];
    if (b && b->val)
      count++;
  }

  exs = MALLOC_N(Scheme_Object *, count);
  count = 0;
  for (i = ht->size; i--; ) {
    Scheme_Bucket *b = bs[i];
    if (b && b->val)
      exs[count++] = (Scheme_Object *)b->key;
  }
 
  m->functional = 1;
  m->et_functional = 1;
  m->tt_functional = 1;

  m->me->rt->provides = exs;
  m->me->rt->provide_srcs = NULL;
  m->me->rt->provide_src_names = exs;
  m->me->rt->num_provides = count;
  m->me->rt->num_var_provides = count;

  qsort_provides(exs, NULL, NULL, NULL, NULL, 0, count, 1);

  env->running = 1;
}

void scheme_protect_primitive_provide(Scheme_Env *env, Scheme_Object *name)
{
  Scheme_Module *m = env->module;
  int i;

  if (!m->provide_protects) {
    Scheme_Hash_Table *ht;
    char *exps;
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    exps = MALLOC_N_ATOMIC(char, m->me->rt->num_provides);
    for (i = m->me->rt->num_provides; i--; ) {
      exps[i] = 0;
      scheme_hash_set(ht, m->me->rt->provides[i], scheme_make_integer(i));
    }
    m->provide_protects = exps;
    m->accessible = ht;
  }

  if (name) {
    for (i = m->me->rt->num_provides; i--; ) {
      if (SAME_OBJ(name, m->me->rt->provides[i])) {
	m->provide_protects[i] = 1;
	break;
      }
    }
  } else {
    /* Protect all */
    for (i = m->me->rt->num_provides; i--; ) {
      m->provide_protects[i] = 1;
    }
  }
}

Scheme_Bucket *scheme_module_bucket(Scheme_Object *modname, Scheme_Object *var, int pos, Scheme_Env *env)
{
  Scheme_Object *a[2];

  a[0] = modname;
  a[1] = var;

  return (Scheme_Bucket *)_dynamic_require(2, a, env, 1, 0, 0, 1, 1, pos);
}

Scheme_Object *scheme_builtin_value(const char *name)
{
  Scheme_Object *a[2], *v;

  a[1] = scheme_intern_symbol(name);

  /* Try kernel first: */
  a[0] = kernel_symbol;
  v = _dynamic_require(2, a, scheme_get_env(NULL), 0, 0, 0, 0, 0, -1);

  if (v)
    return v;

  /* Maybe in MzScheme? */
  a[0] = scheme_intern_symbol("mzscheme");
  return _dynamic_require(2, a, initial_modules_env, 0, 0, 0, 0, 0, -1);
}

Scheme_Module *scheme_extract_compiled_module(Scheme_Object *o)
{
  if (SAME_TYPE(SCHEME_TYPE(o), scheme_compilation_top_type)) {
    Scheme_Compilation_Top *c = (Scheme_Compilation_Top *)o;
    
    if (SAME_TYPE(SCHEME_TYPE(c->code), scheme_syntax_type)
	&& (SCHEME_PINT_VAL(c->code) == MODULE_EXPD)) {
      return (Scheme_Module *)SCHEME_IPTR_VAL(c->code);
    }
  }

  return NULL;
}

static Scheme_Module_Exports *make_module_exports()
{
  Scheme_Module_Exports *me;
  Scheme_Module_Phase_Exports *pt;

  me = MALLOC_ONE_RT(Scheme_Module_Exports);
  SET_REQUIRED_TAG(me->type = scheme_rt_module_exports);

  pt = MALLOC_ONE_RT(Scheme_Module_Phase_Exports);
  SET_REQUIRED_TAG(pt->type = scheme_rt_module_phase_exports);
  me->rt = pt;

  pt = MALLOC_ONE_RT(Scheme_Module_Phase_Exports);
  SET_REQUIRED_TAG(pt->type = scheme_rt_module_phase_exports);
  me->et = pt;

  pt = MALLOC_ONE_RT(Scheme_Module_Phase_Exports);
  SET_REQUIRED_TAG(pt->type = scheme_rt_module_phase_exports);
  me->dt = pt;

  return me;
}

/**********************************************************************/
/*                          define-syntaxes                           */
/**********************************************************************/

static void *eval_defmacro_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *names;
  int count, for_stx;
  Scheme_Object *expr, *certs;
  Scheme_Env *genv;
  Scheme_Comp_Env *comp_env;
  Resolve_Prefix *rp;
  int let_depth, shift;
  Scheme_Bucket_Table *syntax;

  names = (Scheme_Object *)p->ku.k.p1;
  expr = (Scheme_Object *)p->ku.k.p2;
  genv = (Scheme_Env *)SCHEME_CAR((Scheme_Object *)p->ku.k.p3);
  comp_env = (Scheme_Comp_Env *)SCHEME_CDR((Scheme_Object *)p->ku.k.p3);
  rp = (Resolve_Prefix *)SCHEME_CAR((Scheme_Object *)p->ku.k.p4);
  syntax = (Scheme_Bucket_Table *)SCHEME_CDR((Scheme_Object *)p->ku.k.p4);
  count = p->ku.k.i1;
  let_depth = p->ku.k.i2;
  shift = p->ku.k.i3;
  for_stx = p->ku.k.i4;
  certs = (Scheme_Object *)p->ku.k.p5;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;
  p->ku.k.p5 = NULL;

  eval_defmacro(names, count, expr, genv, comp_env, rp, let_depth, shift, syntax, for_stx, certs);

  return NULL;
}

static void eval_defmacro(Scheme_Object *names, int count,
			  Scheme_Object *expr, 
			  Scheme_Env *genv, Scheme_Comp_Env *comp_env,
			  Resolve_Prefix *rp,
			  int let_depth, int shift, Scheme_Bucket_Table *syntax,
			  int for_stx, Scheme_Object *certs)
{
  Scheme_Object *macro, *vals, *name, **save_runstack;
  int i, g, depth;

  depth = let_depth + scheme_prefix_depth(rp);
  if (!scheme_check_runstack(depth)) {
    Scheme_Thread *p = scheme_current_thread;
    p->ku.k.p1 = names;
    p->ku.k.p2 = expr;
    vals = scheme_make_pair((Scheme_Object *)genv, (Scheme_Object *)comp_env);
    p->ku.k.p3 = vals;
    vals = scheme_make_pair((Scheme_Object *)rp, (Scheme_Object *)syntax);
    p->ku.k.p4 = vals;
    p->ku.k.i1 = count;
    p->ku.k.i2 = let_depth;
    p->ku.k.i3 = shift;
    p->ku.k.i4 = for_stx;
    p->ku.k.p5 = certs;
    (void)scheme_enlarge_runstack(depth, eval_defmacro_k);
    return;
  }

  save_runstack = scheme_push_prefix(genv, rp,
				     (shift ? genv->module->me->src_modidx : NULL), 
				     (shift ? genv->link_midx : NULL), 
				     1, genv->phase);
	
  scheme_on_next_top(comp_env, NULL, scheme_false, certs, 
		     genv, (genv->link_midx ? genv->link_midx : genv->module->me->src_modidx));
  vals = scheme_eval_linked_expr_multi(expr);

  scheme_pop_prefix(save_runstack);

  
  if (SAME_OBJ(vals, SCHEME_MULTIPLE_VALUES)) {
    g = scheme_current_thread->ku.multiple.count;
    if (count == g) {
      Scheme_Object **values;

      values = scheme_current_thread->ku.multiple.array;
      scheme_current_thread->ku.multiple.array = NULL;
      if (SAME_OBJ(values, scheme_current_thread->values_buffer))
	scheme_current_thread->values_buffer = NULL;
      for (i = 0; i < g; i++, names = SCHEME_CDR(names)) {
	name = SCHEME_CAR(names);

	if (!for_stx) {
	  macro = scheme_alloc_small_object();
	  macro->type = scheme_macro_type;
	  SCHEME_PTR_VAL(macro) = values[i];
	} else
	  macro = values[i];
	
	scheme_add_to_table(syntax, (const char *)name, macro, 0);
      }
	
      return;
    }
  } else if (SCHEME_PAIRP(names) && SCHEME_NULLP(SCHEME_CDR(names))) {
    name = SCHEME_CAR(names);

    if (!for_stx) {
      macro = scheme_alloc_small_object();
      macro->type = scheme_macro_type;
      SCHEME_PTR_VAL(macro) = vals;
    } else
      macro = vals;

    scheme_add_to_table(syntax, (const char *)name, macro, 0);
      
    return;
  } else
    g = 1;
  
  if (count)
    name = SCHEME_CAR(names);
  else
    name = NULL;
  
  {
    const char *symname;

    symname = (name ? scheme_symbol_name(name) : "");

    scheme_wrong_return_arity((for_stx ? "define-values-for-syntax" : "define-syntaxes"),
			      count, g,
			      (g == 1) ? (Scheme_Object **)vals : scheme_current_thread->ku.multiple.array,
			      "%s%s%s",
			      name ? "defining \"" : "0 names",
			      symname,
			      name ? ((count == 1) ? "\"" : "\", ...") : "");
  }  
}

/**********************************************************************/
/*                               module                               */
/**********************************************************************/

static Scheme_Object *
module_execute(Scheme_Object *data)
{
  Scheme_Module *m;
  Scheme_Env *env;
  Scheme_Env *old_menv;
  Scheme_Object *prefix, *insp;
  
  m = MALLOC_ONE_TAGGED(Scheme_Module);
  memcpy(m, data, sizeof(Scheme_Module));

  prefix = scheme_get_param(scheme_current_config(), MZCONFIG_CURRENT_MODULE_PREFIX);
  if (SCHEME_SYMBOLP(prefix)) {
    prefix = scheme_symbol_append(prefix, m->modname);
    m->modname = prefix;
    
    if (m->self_modidx) {
      if (!SCHEME_SYMBOLP(m->self_modidx)) {
	Scheme_Modidx *midx = (Scheme_Modidx *)m->self_modidx;
	Scheme_Object *nmidx;
	
	nmidx = scheme_make_modidx(midx->path, midx->base, m->modname);
	m->self_modidx = nmidx;

	if (m->rn_stx && !SAME_OBJ(scheme_true, m->rn_stx)) {
	  /* Delay the shift: */
	  Scheme_Object *v;
	  v = scheme_make_pair(m->rn_stx, (Scheme_Object *)midx);
	  m->rn_stx = v;
	}
	if (m->et_rn_stx && !SAME_OBJ(scheme_true, m->et_rn_stx)) {
	  /* Delay the shift: */
	  Scheme_Object *v;
	  v = scheme_make_pair(m->et_rn_stx, (Scheme_Object *)midx);
	  m->et_rn_stx = v;
	}
	if (m->dt_rn_stx && !SAME_OBJ(scheme_true, m->dt_rn_stx)) {
	  /* Delay the shift: */
	  Scheme_Object *v;
	  v = scheme_make_pair(m->dt_rn_stx, (Scheme_Object *)midx);
	  m->dt_rn_stx = v;
	}
      }
    }
  }

  env = scheme_environment_from_dummy(m->dummy);

  if (SAME_OBJ(m->modname, kernel_symbol))
    old_menv = scheme_initial_env;
  else
    old_menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(env->modchain), m->modname);

  insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);
  
  if (old_menv) {
    if (scheme_module_protected_wrt(old_menv->insp, insp) || old_menv->attached) {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "module->namespace: current code inspector cannot re-declare module: %S",
		       m->modname);
      return NULL;
    }
  }

  m->insp = insp;
  scheme_hash_set(env->module_registry, m->modname, (Scheme_Object *)m);
  scheme_hash_set(env->export_registry, m->modname, (Scheme_Object *)m->me);

  /* We might compute whether the module is obviously functional (as
     opposed to imperative). But it doesn't seem to matter much except 
     for starting up. */

  if (scheme_starting_up) {
    m->functional = 1;
    m->et_functional = 1;
    m->tt_functional = 1;
  }

  /* Replacing an already-running or already-syntaxing module? */
  if (old_menv) {
    if (old_menv->running > 0)
      start_module(m, env, 1, NULL, 1, 0, scheme_null);
    else
      expstart_module(m, env, 1, NULL, 1, 1, scheme_null);
  }

  return scheme_void;
}

static Scheme_Object *rebuild_et_vec(Scheme_Object *naya, Scheme_Object *vec)
{
  Scheme_Object *vec2;
  int i;
  
  i = SCHEME_VEC_SIZE(vec);
  vec2 = scheme_make_vector(i, NULL);
  while (i--) {
    SCHEME_VEC_ELS(vec2)[i] = SCHEME_VEC_ELS(vec)[i];
  }
  SCHEME_VEC_ELS(vec2)[1] = naya;

  return vec2;
}

static Scheme_Object *jit_list(Scheme_Object *orig_l, int in_vec)
{
  Scheme_Object *l, *orig, *naya = NULL;
  int saw;

  for (l = orig_l, saw = 0; SCHEME_PAIRP(l); l = SCHEME_CDR(l), saw++) {
    orig = SCHEME_CAR(l);
    if (in_vec)
      orig = SCHEME_VEC_ELS(orig)[1];

    naya = scheme_jit_expr(orig);
    if (!SAME_OBJ(orig, naya))
      break;
  }

  if (SCHEME_PAIRP(l)) {
    Scheme_Object *first = scheme_null, *last = NULL, *pr;
    for (l = orig_l; saw--; l = SCHEME_CDR(l)) {
      orig = SCHEME_CAR(l);
      pr = scheme_make_pair(orig, scheme_null);
      if (last)
	SCHEME_CDR(last) = pr;
      else
	first = pr;
      last = pr;
    }
    if (in_vec)
      naya = rebuild_et_vec(naya, SCHEME_CAR(l));
    pr = scheme_make_pair(naya, scheme_null);
    if (last)
      SCHEME_CDR(last) = pr;
    else
      first = pr;
    last = pr;
    l = SCHEME_CDR(l);
    for (; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
      orig = SCHEME_CAR(l);
      if (in_vec)
	orig = SCHEME_VEC_ELS(orig)[1];
      naya = scheme_jit_expr(orig);
      if (in_vec) {
	if (!SAME_OBJ(orig, naya))
	  naya = rebuild_et_vec(naya, SCHEME_CAR(l));
	else
	  naya = SCHEME_CAR(l);
      }
      pr = scheme_make_pair(naya, scheme_null);
      SCHEME_CDR(last) = pr;
      last = pr;
    }
    return first;
  } else
    return orig_l;
}

static Scheme_Object *module_jit(Scheme_Object *data)
{
  Scheme_Module *m = (Scheme_Module *)data;
  Scheme_Object *l1, *l2;

  l1 = jit_list(m->body, 0);
  l2 = jit_list(m->et_body, 1);

  if (SAME_OBJ(l1, m->body) && SAME_OBJ(l2, m->body))
    return data;
  
  m = MALLOC_ONE_TAGGED(Scheme_Module);
  memcpy(m, data, sizeof(Scheme_Module));
  m->body = l1;
  m->et_body = l2;

  return (Scheme_Object *)m;
}

static void module_validate(Scheme_Object *data, Mz_CPort *port, 
                            char *stack, Scheme_Hash_Table *ht, Scheme_Object **tls,
			    int depth, int letlimit, int delta, 
			    int num_toplevels, int num_stxes, int num_lifts)
{
  Scheme_Module *m;
  Scheme_Object *l;

  if (!SAME_TYPE(SCHEME_TYPE(data), scheme_module_type))
    scheme_ill_formed_code(port);

  m = (Scheme_Module *)data;

  if (!SCHEME_SYMBOLP(m->modname))
    scheme_ill_formed_code(port);

  for (l = m->body; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    scheme_validate_code(port, SCHEME_CAR(l), ht, m->max_let_depth,
			 m->prefix->num_toplevels, m->prefix->num_stxes, m->prefix->num_lifts);
  }
  if (!SCHEME_NULLP(l))
    scheme_ill_formed_code(port);

  /* FIXME: validate exp-time code */
}

static int set_code_closure_flags(Scheme_Object *clones,
                                  int set_flags, int mask_flags)
{
  Scheme_Object *clone, *orig, *first;
  Scheme_Closure_Data *data;
  int flags = CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS;

  /* The first in a clone pair is the one that is consulted for
     references. The second one is the original, and its the one whose
     flags are updated by optimization. So consult the original, and set
     flags in both. */

  while (clones) {
    first = SCHEME_CAR(clones);
    clone = SCHEME_CAR(first);
    orig = SCHEME_CDR(first);

    data = (Scheme_Closure_Data *)orig;
    flags = (flags & SCHEME_CLOSURE_DATA_FLAGS(data));
    SCHEME_CLOSURE_DATA_FLAGS(data) = set_flags | (SCHEME_CLOSURE_DATA_FLAGS(data) & mask_flags);
    data = (Scheme_Closure_Data *)clone;
    SCHEME_CLOSURE_DATA_FLAGS(data) = set_flags | (SCHEME_CLOSURE_DATA_FLAGS(data) & mask_flags);

    clones = SCHEME_CDR(clones);
  }

  return flags;
}

static Scheme_Object *
module_optimize(Scheme_Object *data, Optimize_Info *info)
{
  Scheme_Module *m = (Scheme_Module *)data;
  Scheme_Object *e, *b, *vars, *start_simltaneous_b;
  Scheme_Object *cl_first = NULL, *cl_last = NULL;
  Scheme_Hash_Table *consts = NULL, *ready_table = NULL;
  int cont;

  start_simltaneous_b = m->body;
  for (b = m->body; !SCHEME_NULLP(b); b = SCHEME_CDR(b)) {
    /* Optimize this expression: */
    e = scheme_optimize_expr(SCHEME_CAR(b), info);
    SCHEME_CAR(b) = e;

    if (info->enforce_const) {
      /* If this expression/definition can't have any side effect
	 (including raising an exception), then continue the group of
	 simultaneous definitions: */
      if (SAME_TYPE(SCHEME_TYPE(e), scheme_compiled_syntax_type)
	  && (SCHEME_PINT_VAL(e) == DEFINE_VALUES_EXPD)) {
	int n;

	e = (Scheme_Object *)SCHEME_IPTR_VAL(e);

	vars = SCHEME_CAR(e);
	e = SCHEME_CDR(e);

	n = scheme_list_length(vars);
	cont = scheme_omittable_expr(e, n);
      
	if ((n == 1) && scheme_compiled_propagate_ok(e, info)) {
	  Scheme_Toplevel *tl;

	  tl = (Scheme_Toplevel *)SCHEME_CAR(vars);

	  if (!(SCHEME_TOPLEVEL_FLAGS(tl) & SCHEME_TOPLEVEL_MUTATED)) {
	    Scheme_Object *e2;

	    if (SAME_TYPE(SCHEME_TYPE(e), scheme_compiled_unclosed_procedure_type)) {
	      e2 = scheme_optimize_clone(1, e, info, 0, 0);
              if (e2) {
                Scheme_Object *pr;
                pr = scheme_make_raw_pair(scheme_make_raw_pair(e2, e), NULL);
                if (cl_last)
                  SCHEME_CDR(cl_last) = pr;
                else
                  cl_first = pr;
                cl_last = pr;
              }
	    } else {
	      e2 = e;
	    }

	    if (e2) {
	      int pos;
	      if (!consts)
		consts = scheme_make_hash_table(SCHEME_hash_ptr);
	      pos = tl->position;
	      scheme_hash_set(consts, scheme_make_integer(pos), e2);
	    } else {
	      /* At least mark it as ready */
	      if (!ready_table) {
		ready_table = scheme_make_hash_table(SCHEME_hash_ptr);
		if (!consts)
		  consts = scheme_make_hash_table(SCHEME_hash_ptr);
		scheme_hash_set(consts, scheme_false, (Scheme_Object *)ready_table);
	      }
	      scheme_hash_set(ready_table, scheme_make_integer(tl->position), scheme_true);
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

	    /* Test for ISCONST to indicate no set!: */
	    if (!(SCHEME_TOPLEVEL_FLAGS(a) & SCHEME_TOPLEVEL_MUTATED)) {
	      pos = SCHEME_TOPLEVEL_POS(a);
	
	      if (!ready_table) {
		ready_table = scheme_make_hash_table(SCHEME_hash_ptr);
		if (!consts)
		  consts = scheme_make_hash_table(SCHEME_hash_ptr);
		scheme_hash_set(consts, scheme_false, (Scheme_Object *)ready_table);
	      }
	      scheme_hash_set(ready_table, scheme_make_integer(pos), scheme_true);
	    }
	  }
	}
      } else {
	cont = scheme_omittable_expr(e, 1);
      }
      if (SCHEME_NULLP(SCHEME_CDR(b)))
	cont = 0;
    } else
      cont = 1;

    if (!cont) {
      /* If we have new constants, re-optimize to inline: */
      if (consts) {
        int flags;

	if (!info->top_level_consts) {
	  info->top_level_consts = consts;
	} else {
	  int i;
	  for (i = 0; i < consts->size; i++) {
	    if (consts->vals[i]) {
	      scheme_hash_set(info->top_level_consts,
			      consts->keys[i],
			      consts->vals[i]);
	    }
	  }
	}

        /* Same as in letrec: assume CLOS_SINGLE_RESULT and
           CLOS_PRESERVES_MARKS for all, but then assume not for all
           if any turn out not (i.e., approximate fix point). */
        (void)set_code_closure_flags(cl_first, 
                                     CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS | CLOS_RESULT_TENTATIVE, 
                                     0xFFFF);

	while (1) {
	  /* Re-optimize this expression. We can optimize anything without
             shift-cloning, since there are no local variables in scope. */
	  e = scheme_optimize_expr(SCHEME_CAR(start_simltaneous_b), info);
	  SCHEME_CAR(start_simltaneous_b) = e;
	  
	  if (SAME_OBJ(start_simltaneous_b, b))
	    break;
	  start_simltaneous_b = SCHEME_CDR(start_simltaneous_b);
	}

        flags = set_code_closure_flags(cl_first, 0, 0xFFFF);
        (void)set_code_closure_flags(cl_first,
                                     (flags & (CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS)), 
                                     ~(CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS | CLOS_RESULT_TENTATIVE));
      }
      
      cl_last = cl_first = NULL;
      consts = NULL;
      start_simltaneous_b = SCHEME_CDR(b);
    }
  }

  /* Exp-time body was optimized during compilation */

  return scheme_make_syntax_compiled(MODULE_EXPD, data);
}

static Scheme_Object *
module_resolve(Scheme_Object *data, Resolve_Info *old_rslv)
{
  Scheme_Module *m = (Scheme_Module *)data;
  Scheme_Object *b, *lift_vec;
  Resolve_Prefix *rp;
  Resolve_Info *rslv;

  rp = scheme_resolve_prefix(0, m->comp_prefix, 1);
  m->comp_prefix = NULL;

  b = scheme_resolve_expr(m->dummy, old_rslv);
  m->dummy = b;

  rslv = scheme_resolve_info_create(rp);
  rslv->enforce_const = old_rslv->enforce_const;
  rslv->in_module = 1;
  scheme_enable_expression_resolve_lifts(rslv);

  for (b = m->body; !SCHEME_NULLP(b); b = SCHEME_CDR(b)) {
    Scheme_Object *e;
    e = scheme_resolve_expr(SCHEME_CAR(b), rslv);
    SCHEME_CAR(b) = e;
  }

  m->max_let_depth = rslv->max_let_depth;

  lift_vec = rslv->lifts;
  b = scheme_append(SCHEME_VEC_ELS(lift_vec)[0], m->body);
  m->body = b;
  rp->num_lifts = SCHEME_INT_VAL(SCHEME_VEC_ELS(lift_vec)[1]);

  rp = scheme_remap_prefix(rp, rslv);

  m->prefix = rp;

  /* Exp-time body was resolved during compilation */

  return scheme_make_syntax_resolved(MODULE_EXPD, data);
}

static Scheme_Object *do_module(Scheme_Object *form, Scheme_Comp_Env *env, 
				Scheme_Compile_Expand_Info *rec, int drec)
{
  Scheme_Object *fm, *nm, *ii, *rn, *et_rn, *tt_rn, *dt_rn, *iidx, *self_modidx;
  Scheme_Module *iim;
  Scheme_Env *menv;
  Scheme_Comp_Env *benv;
  Scheme_Module *m;
  Scheme_Object *mbval;
  Scheme_Hash_Table *mn_ht, *et_mn_ht, *tt_mn_ht, *dt_mn_ht;
  int saw_mb, check_mb = 0;
  int restore_confusing_name = 0;

  if (!scheme_is_toplevel(env))
    scheme_wrong_syntax(NULL, NULL, form, "illegal use (not at top-level)");

  fm = SCHEME_STX_CDR(form);
  if (!SCHEME_STX_PAIRP(fm))
    scheme_wrong_syntax(NULL, NULL, form, NULL);
  nm = SCHEME_STX_CAR(fm);
  if (!SCHEME_STX_SYMBOLP(nm))
    scheme_wrong_syntax(NULL, nm, form, "module name is not an identifier");
  fm = SCHEME_STX_CDR(fm);
  if (!SCHEME_STX_PAIRP(fm))
    scheme_wrong_syntax(NULL, NULL, form, NULL);
  ii = SCHEME_STX_CAR(fm);
  fm = SCHEME_STX_CDR(fm);

  m = MALLOC_ONE_TAGGED(Scheme_Module);
  m->so.type = scheme_module_type;
  
  m->modname = SCHEME_STX_VAL(nm); /* must set before calling new_module_env */
  if (SAME_OBJ(m->modname, kernel_symbol)) {
    /* Too confusing. Give it a different name while compiling. */
    Scheme_Object *k2;
    k2 = scheme_make_symbol("#%kernel");
    m->modname = k2;
    restore_confusing_name = 1;
  }

  {
    Scheme_Module_Exports *me;
    me = make_module_exports();
    m->me = me;
  }

  menv = scheme_new_module_env(env->genv, m, 1);
  
  self_modidx = scheme_make_modidx(scheme_false, scheme_false, m->modname);
  m->self_modidx = self_modidx;
  m->me->src_modidx = self_modidx;

  m->insp = env->insp;

  m->ii_src = ii;

  iidx = scheme_make_modidx(scheme_syntax_to_datum(ii, 0, NULL), 
			    self_modidx,
			    scheme_false);

  /* load the module for the initial require */
  iim = module_load(_module_resolve(iidx, ii, 1), menv, NULL); 
  expstart_module(iim, menv, 0, iidx, 0, 0, scheme_null);

  {
    Scheme_Object *ins;
    ins = cons(iidx, scheme_null);
    m->requires = ins;
    m->et_requires = scheme_null;
    m->tt_requires = scheme_null;
    m->dt_requires = scheme_null;
  }

  mn_ht = scheme_make_hash_table(SCHEME_hash_ptr);
  et_mn_ht = scheme_make_hash_table(SCHEME_hash_ptr);
  tt_mn_ht = scheme_make_hash_table(SCHEME_hash_ptr);
  dt_mn_ht = mn_ht;

  annote_marked_names_nonempty(mn_ht);
  annote_marked_names_nonempty(et_mn_ht);
  annote_marked_names_nonempty(tt_mn_ht);

  rn = scheme_make_module_rename(0, mzMOD_RENAME_NORMAL, mn_ht);
  et_rn = scheme_make_module_rename(1, mzMOD_RENAME_NORMAL, et_mn_ht);
  tt_rn = scheme_make_module_rename(-1, mzMOD_RENAME_NORMAL, tt_mn_ht);
  dt_rn = scheme_make_module_rename(MZ_LABEL_PHASE, mzMOD_RENAME_NORMAL, dt_mn_ht);

  menv->rename = rn;
  menv->et_rename = et_rn;
  menv->tt_rename = tt_rn;
  menv->dt_rename = dt_rn;

  {
    Scheme_Object *insp;
    insp = scheme_make_inspector(env->insp);
    menv->insp = insp;
  }

  menv->marked_names = mn_ht;
  scheme_prepare_exp_env(menv);
  menv->exp_env->marked_names = et_mn_ht;
  scheme_prepare_template_env(menv);
  menv->template_env->marked_names = tt_mn_ht;

  /* For each provide in iim, add a module rename to fm */
  if (SAME_OBJ(iim, kernel)) {
    scheme_extend_module_rename_with_kernel(rn, kernel_symbol);
    saw_mb = 1;
  } else {
    saw_mb = add_require_renames(rn, et_rn, dt_rn, iim, iidx);
  }

  if (rec[drec].comp)
    benv = scheme_new_comp_env(menv, env->insp, SCHEME_MODULE_FRAME);
  else
    benv = scheme_new_expand_env(menv, env->insp, SCHEME_MODULE_FRAME);

  /* If fm isn't a single expression, it certainly needs a
     `#%module-begin': */
  if (SCHEME_STX_PAIRP(fm) && SCHEME_STX_NULLP(SCHEME_STX_CDR(fm))) {
    /* Perhaps expandable... */
    fm = SCHEME_STX_CAR(fm);
  } else {
    fm = scheme_make_pair(scheme_datum_to_syntax(module_begin_symbol, form, scheme_false, 0, 2), 
			  fm);
    check_mb = 1;
  }

  fm = scheme_datum_to_syntax(fm, form, form, 0, 2);
  fm = scheme_stx_property(fm, module_name_symbol, m->modname);

  if (!empty_self_modidx) {
    REGISTER_SO(empty_self_modidx);
    REGISTER_SO(empty_self_symbol);
    empty_self_modidx = scheme_make_modidx(scheme_false, scheme_false, scheme_false);
    empty_self_symbol = scheme_make_symbol("expanded module"); /* uninterned */
  }
  
  /* phase shift to replace self_modidx of previous expansion (if any): */
  fm = scheme_stx_phase_shift(fm, 0, empty_self_modidx, self_modidx, NULL);

  fm = scheme_add_rename(fm, rn);
  fm = scheme_add_rename(fm, et_rn);
  fm = scheme_add_rename(fm, tt_rn);
  fm = scheme_add_rename(fm, dt_rn);

  if (!check_mb) {
    
    SCHEME_EXPAND_OBSERVE_NEXT(rec[drec].observer);

    fm = scheme_check_immediate_macro(fm, benv, rec, drec, 0, &mbval, NULL, NULL);

    SCHEME_EXPAND_OBSERVE_NEXT(rec[drec].observer);

    /* If expansion is not the primitive `#%module-begin', add local one: */
    if (!SAME_OBJ(mbval, modbeg_syntax)) {
      Scheme_Object *mb;
      mb = scheme_datum_to_syntax(module_begin_symbol, form, scheme_false, 0, 0);
      fm = scheme_make_pair(mb, scheme_make_pair(fm, scheme_null));
      fm = scheme_datum_to_syntax(fm, form, form, 0, 2);
      fm = scheme_stx_property(fm, module_name_symbol, m->modname);
      /* Since fm is a newly-created syntax object, we need to re-add renamings: */
      fm = scheme_add_rename(fm, rn);
      fm = scheme_add_rename(fm, et_rn);
      fm = scheme_add_rename(fm, tt_rn);
      fm = scheme_add_rename(fm, dt_rn);
      check_mb = 1;
    }
  }

  if (check_mb && !saw_mb) {
    scheme_wrong_syntax(NULL, NULL, form, 
			"no #%%module-begin binding in the module's language");
  }

  if (rec[drec].comp) {
    Scheme_Object *dummy;

    dummy = scheme_make_environment_dummy(env);
    m->dummy = dummy;
    
    scheme_compile_rec_done_local(rec, drec);
    fm = scheme_compile_expr(fm, benv, rec, drec);

    /* result should be a module body value: */
    if (!SAME_OBJ(fm, (Scheme_Object *)m)) {
      scheme_wrong_syntax(NULL, NULL, form, "compiled body was not built with #%%module-begin");
    }

    if (restore_confusing_name)
      m->modname = kernel_symbol;

    m->ii_src = NULL;

    fm = scheme_make_syntax_compiled(MODULE_EXPD, (Scheme_Object *)m);
  } else {
    Scheme_Object *hints, *formname;

    fm = scheme_expand_expr(fm, benv, rec, drec);

    m->ii_src = NULL;

    hints = m->hints;
    m->hints = NULL;

    formname = SCHEME_STX_CAR(form);
    fm = cons(formname,
	      cons(nm,
		   cons(ii, cons(fm, scheme_null))));

    fm = scheme_datum_to_syntax(fm, form, form, 0, 2);
    
    if (hints) {
      scheme_make_list_immutable(m->requires);
      scheme_make_list_immutable(m->et_requires);
      scheme_make_list_immutable(m->tt_requires);

      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-direct-requires"),
			       m->requires);
      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-direct-for-syntax-requires"),
			       m->et_requires);
      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-direct-for-template-requires"),
			       m->tt_requires);
      
      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-variable-provides"),
			       SCHEME_CAR(hints));
      hints = SCHEME_CDR(hints);
      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-syntax-provides"),
			       SCHEME_CAR(hints));
      hints = SCHEME_CDR(hints);
      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-indirect-provides"),
			       SCHEME_CAR(hints));
      hints = SCHEME_CDR(hints);
      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-kernel-reprovide-hint"),
			       SCHEME_CAR(hints));
      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-self-path-index"),
			       empty_self_modidx);
    }

    /* for future expansion, shift away from self_modidx: */
    fm = scheme_stx_phase_shift(fm, 0, self_modidx, empty_self_modidx, NULL);

    /* make self_modidx like the empty modidx */
    ((Scheme_Modidx *)self_modidx)->resolved = empty_self_symbol;
  }

  clear_marked_names_nonempty(mn_ht);
  clear_marked_names_nonempty(et_mn_ht);
  clear_marked_names_nonempty(tt_mn_ht);
  
  return fm;
}

static Scheme_Object *
module_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_module(form, env, rec, drec);
}

static Scheme_Object *
module_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_MODULE(erec[drec].observer);
  if (erec[drec].depth > 0)
    erec[drec].depth++;

  return do_module(form, env, erec, drec);
}

/* For mzc: */
Scheme_Object *scheme_apply_for_syntax_in_env(Scheme_Object *proc, Scheme_Env *env)
{
  Scheme_Comp_Env *rhs_env;

  rhs_env = scheme_new_comp_env(env, NULL, SCHEME_TOPLEVEL_FRAME);

  scheme_on_next_top(rhs_env, NULL, scheme_false, NULL, 
		     env, (env->link_midx 
			   ? env->link_midx 
			   : (env->module
			      ? env->module->me->src_modidx
			      : NULL)));
  return scheme_apply_multi(proc, 0, NULL);
}

/**********************************************************************/
/*                          #%module-begin                            */
/**********************************************************************/

static void check_require_name(Scheme_Object *prnt_name, Scheme_Object *name, Scheme_Object *nominal_modidx,
			       Scheme_Object *modidx, Scheme_Object *exname,
			       int isval, void *tables, Scheme_Object *e, Scheme_Object *form, Scheme_Object *err_src)
{
  Scheme_Bucket_Table *toplevel, *syntax;
  Scheme_Hash_Table *required;
  Scheme_Object *vec, *nml;

  toplevel = ((Scheme_Bucket_Table **)tables)[0];
  required = ((Scheme_Hash_Table **)tables)[1];
  syntax = ((Scheme_Bucket_Table **)tables)[2];

  /* Check that it's not yet defined: */
  if (toplevel) {
    if (scheme_lookup_in_table(toplevel, (const char *)name)) {
      scheme_wrong_syntax("module", prnt_name, form, "imported identifier already defined");
    }
  }
	    
  /* Not required, or required from same module: */
  vec = scheme_hash_get(required, name);
  if (vec) {
    Scheme_Object *srcs;
    char *fromsrc = NULL, *fromsrc_colon = "";
    long fromsrclen = 0;
    
    if (same_resolved_modidx(SCHEME_VEC_ELS(vec)[1], modidx)
	&& SAME_OBJ(SCHEME_VEC_ELS(vec)[2], exname)) {
      /* already required, same source; add redundant nominal (for re-provides) */
      nml = scheme_make_pair(nominal_modidx, SCHEME_VEC_ELS(vec)[0]);
      SCHEME_VEC_ELS(vec)[0] = nml;
      return; 
    }

    srcs = scheme_null;
    if (SCHEME_TRUEP(SCHEME_VEC_ELS(vec)[5])) {
      srcs = scheme_make_immutable_pair(SCHEME_VEC_ELS(vec)[5], srcs);
      /* don't use error_write_to_string_w_max since this is code */
      if (SCHEME_TRUEP(scheme_get_param(scheme_current_config(), MZCONFIG_ERROR_PRINT_SRCLOC))) {
        fromsrc = scheme_write_to_string_w_max(scheme_syntax_to_datum(SCHEME_VEC_ELS(vec)[5], 0, NULL), 
                                               &fromsrclen, 32);
        fromsrc_colon = ":";
      }
    }
      
    if (!fromsrc) {
      fromsrc = "a different source";
      fromsrclen = strlen(fromsrc);
    }

    if (err_src)
      srcs = scheme_make_immutable_pair(err_src, srcs);

    scheme_wrong_syntax_with_more_sources("module", prnt_name, err_src, srcs,
                                          "identifier already imported from%s %t",
                                          fromsrc_colon, fromsrc, fromsrclen);
  }
	    
  /* Not syntax: */
  if (syntax) {
    if (scheme_lookup_in_table(syntax, (const char *)name)) {
      scheme_wrong_syntax("module", prnt_name, form, "imported identifier already defined");
    }
  }

  /* Remember require: */
  vec = scheme_make_vector(6, NULL);
  nml = scheme_make_pair(nominal_modidx, scheme_null);
  SCHEME_VEC_ELS(vec)[0] = nml;
  SCHEME_VEC_ELS(vec)[1] = modidx;
  SCHEME_VEC_ELS(vec)[2] = exname;
  SCHEME_VEC_ELS(vec)[3] = (isval ? scheme_true : scheme_false);
  SCHEME_VEC_ELS(vec)[4] = prnt_name;
  SCHEME_VEC_ELS(vec)[5] = (err_src ? err_src : scheme_false);
  scheme_hash_set(required, name, vec);
}

static Scheme_Object *stx_sym(Scheme_Object *name, Scheme_Object *_genv)
{
  return scheme_tl_id_sym((Scheme_Env *)_genv, name, NULL, 2);
}

static Scheme_Object *add_a_rename(Scheme_Object *fm, Scheme_Object *post_ex_rn)
{
  return scheme_add_rename(fm, post_ex_rn);
}

static Scheme_Object *add_req(Scheme_Object *imods, Scheme_Object *requires)
{
  for (; !SCHEME_NULLP(imods); imods = SCHEME_CDR(imods)) {
    Scheme_Object *il, *ilast = NULL;
    Scheme_Object *idx = SCHEME_CAR(imods);
    
    for (il = requires; SCHEME_PAIRP(il); il = SCHEME_CDR(il)) {
      if (same_modidx(idx, SCHEME_CAR(il)))
	break;
      ilast = il;
    }
    
    if (SCHEME_NULLP(il)) {
      il = scheme_make_pair(idx, scheme_null);
      if (ilast)
	SCHEME_CDR(ilast) = il;
      else
	requires = il;
    }
  }

  return requires;
}

static Scheme_Object *add_lifted_defn(Scheme_Object *data, Scheme_Object **_id, Scheme_Object *expr, Scheme_Comp_Env *_env)
{
  Scheme_Comp_Env *env;
  Scheme_Object *self_modidx, *rn, *name, *id;

  env = (Scheme_Comp_Env *)SCHEME_VEC_ELS(data)[0];
  self_modidx = SCHEME_VEC_ELS(data)[1];
  rn = SCHEME_VEC_ELS(data)[2];
  
  name = scheme_tl_id_sym(env->genv, *_id, scheme_false, 2);

  /* Create the bucket, indicating that the name will be defined: */
  scheme_add_global_symbol(name, scheme_undefined, env->genv);
  
  /* Add a renaming: */
  scheme_extend_module_rename(rn, self_modidx, name, name, self_modidx, name, 0, 0);

  id = scheme_add_rename(*_id, rn);
  *_id = id;

  return scheme_make_lifted_defn(scheme_sys_wraps(env), _id, expr, _env);
}

static void flush_definitions(Scheme_Env *genv)
{
  if (genv->syntax) {
    Scheme_Bucket_Table *t;
    t = scheme_make_bucket_table(7, SCHEME_hash_ptr);
    genv->syntax = t;
  }
  if (genv->toplevel) {
    Scheme_Bucket_Table *t;
    t = scheme_make_bucket_table(7, SCHEME_hash_ptr);
    t->with_home = 1;
    genv->toplevel = t;
  }
}

static Scheme_Object *do_module_begin(Scheme_Object *form, Scheme_Comp_Env *env, 
				      Scheme_Compile_Expand_Info *rec, int drec)
{
  Scheme_Object *fm, *first, *last, *p, *rn, *exp_body, *et_rn, *tt_rn, *dt_rn, *self_modidx, *prev_p;
  Scheme_Comp_Env *xenv, *cenv, *rhs_env;
  Scheme_Hash_Table *et_required; /* just to avoid duplicates */
  Scheme_Hash_Table *tt_required; /* just to avoid duplicates */
  Scheme_Hash_Table *dt_required; /* just to avoid duplicates */
  Scheme_Hash_Table *required;    /* name -> (vector nominal-modidx-list modidx srcname var? prntname) */
  /**/                            /*   first nominal-modidx goes with modidx, rest are for re-provides */
  Scheme_Hash_Table *provided;    /* exname -> (cons locname-stx-or-sym protected?) */
  Scheme_Object *reprovided;      /* list of (list modidx syntax except-name ...) */
  Scheme_Object *all_defs_out;    /* list of (cons protected? (stx-list except-name ...)) */
  Scheme_Object *all_et_defs_out;
  Scheme_Hash_Table *et_provided, *dt_provided;
  Scheme_Object *et_reprovided, *dt_reprovided;
  Scheme_Object *all_defs;        /* list of stxid; this is almost redundant to the syntax and toplevel
				     tables, but it preserves the original name for exporting */
  Scheme_Object *all_et_defs;
  Scheme_Object *post_ex_rn, *post_ex_et_rn, *post_ex_tt_rn, *post_ex_dt_rn; /* renames for ids introduced by expansion */
  void *tables[3], *et_tables[3], *tt_tables[3], *dt_tables[3];
  Scheme_Object *exclude_hint = scheme_false, *lift_data;
  Scheme_Hash_Table *et_mn;
  Scheme_Object **exis;
  Scheme_Object *lift_ctx;
  int exicount;
  char *exps;
  int all_simple_renames = 1, et_all_simple_renames = 1, tt_all_simple_renames = 1, dt_all_simple_renames = 1;
  int maybe_has_lifts = 0;
  int reprovide_kernel;
  Scheme_Object *redef_modname;
  Scheme_Object *observer;

  if (!(env->flags & SCHEME_MODULE_FRAME))
    scheme_wrong_syntax(NULL, NULL, form, "illegal use (not a module body)");

  if (scheme_stx_proper_list_length(form) < 0)
    scheme_wrong_syntax(NULL, NULL, form, "bad syntax (" IMPROPER_LIST_FORM ")");

  if (!env->genv->module)
    scheme_wrong_syntax(NULL, NULL, form, "not currently transforming a module");

  /* Redefining a module? */
  redef_modname = env->genv->module->modname;
  if (!scheme_hash_get(env->genv->module_registry, redef_modname))
    redef_modname = NULL;

  /* Expand each expression in form up to `begin', `define-values', `define-syntax', 
     `require', `provide', `#%app', etc. */
  xenv = scheme_new_compilation_frame(0, (SCHEME_CAPTURE_WITHOUT_RENAME 
					  | SCHEME_MODULE_BEGIN_FRAME
					  | SCHEME_FOR_STOPS), 
				      env, NULL);
  {
    Scheme_Object *stop;
    stop = scheme_get_stop_expander();
    scheme_add_local_syntax(25, xenv);
    scheme_set_local_syntax(0, scheme_begin_stx, stop, xenv);
    scheme_set_local_syntax(1, scheme_define_values_stx, stop, xenv);
    scheme_set_local_syntax(2, scheme_define_syntaxes_stx, stop, xenv);
    scheme_set_local_syntax(3, define_for_syntaxes_stx, stop, xenv);
    scheme_set_local_syntax(4, require_stx, stop, xenv);
    scheme_set_local_syntax(5, require_for_syntax_stx, stop, xenv);
    scheme_set_local_syntax(6, require_for_template_stx, stop, xenv);
    scheme_set_local_syntax(7, require_for_label_stx, stop, xenv);
    scheme_set_local_syntax(8, provide_stx, stop, xenv);
    scheme_set_local_syntax(9, provide_for_syntax_stx, stop, xenv);
    scheme_set_local_syntax(10, provide_for_label_stx, stop, xenv);
    scheme_set_local_syntax(11, set_stx, stop, xenv);
    scheme_set_local_syntax(12, app_stx, stop, xenv);
    scheme_set_local_syntax(13, scheme_top_stx, stop, xenv);
    scheme_set_local_syntax(14, lambda_stx, stop, xenv);
    scheme_set_local_syntax(15, case_lambda_stx, stop, xenv);
    scheme_set_local_syntax(16, let_values_stx, stop, xenv);
    scheme_set_local_syntax(17, letrec_values_stx, stop, xenv);
    scheme_set_local_syntax(18, if_stx, stop, xenv);
    scheme_set_local_syntax(19, begin0_stx, stop, xenv);
    scheme_set_local_syntax(20, set_stx, stop, xenv);
    scheme_set_local_syntax(21, with_continuation_mark_stx, stop, xenv);
    scheme_set_local_syntax(22, letrec_syntaxes_stx, stop, xenv);
    scheme_set_local_syntax(23, var_ref_stx, stop, xenv);
    scheme_set_local_syntax(24, expression_stx, stop, xenv);
  }

  first = scheme_null;
  last = NULL;

  rn = env->genv->rename;
  et_rn = env->genv->et_rename;
  tt_rn = env->genv->tt_rename;
  dt_rn = env->genv->dt_rename;

  required = scheme_make_hash_table(SCHEME_hash_ptr);
  et_required = scheme_make_hash_table(SCHEME_hash_ptr);
  dt_required = scheme_make_hash_table(SCHEME_hash_ptr);

  /* Put initial requires into the table: */
  {
    Scheme_Module *iim;
    Scheme_Object *nmidx, *orig_src;

    /* stx src of original import: */
    orig_src = env->genv->module->ii_src;
    if (!orig_src)
      orig_src = scheme_false;
    else if (!SCHEME_STXP(orig_src))
      orig_src = scheme_false;
    
    nmidx = SCHEME_CAR(env->genv->module->requires);
    iim = module_load(scheme_module_resolve(nmidx, 1), env->genv, NULL);

    add_initial_require_renames(orig_src, 
                                rn, required, 
                                et_rn, et_required, 
                                dt_rn, dt_required,
                                iim, nmidx);
  }

  if (rec[drec].comp || (rec[drec].depth != -2)) {
    /* rename tables no longer needed; NULL them out */
    env->genv->rename = NULL;
    env->genv->et_rename = NULL;
    env->genv->tt_rename = NULL;
  }

  {
    Scheme_Object *v;
    v = scheme_rename_to_stx(rn);
    env->genv->module->rn_stx = v;
    v = scheme_rename_to_stx(et_rn);
    env->genv->module->et_rn_stx = v;
    v = scheme_rename_to_stx(tt_rn);
    env->genv->module->tt_rn_stx = v;
    v = scheme_rename_to_stx(dt_rn);
    env->genv->module->dt_rn_stx = v;
  }

  tables[0] = env->genv->toplevel;
  tables[1] = required;
  tables[2] = env->genv->syntax;

  et_tables[0] = NULL;
  et_tables[1] = et_required;
  et_tables[2] = NULL;

  tt_required = scheme_make_hash_table(SCHEME_hash_ptr);
  tt_tables[0] = NULL;
  tt_tables[1] = tt_required;
  tt_tables[2] = NULL;

  dt_tables[0] = NULL;
  dt_tables[1] = dt_required;
  dt_tables[2] = NULL;

  provided = scheme_make_hash_table(SCHEME_hash_ptr);
  reprovided = scheme_null;
  et_provided = scheme_make_hash_table(SCHEME_hash_ptr);
  et_reprovided = scheme_null;
  dt_provided = scheme_make_hash_table(SCHEME_hash_ptr);
  dt_reprovided = scheme_null;

  all_defs_out = scheme_null;
  all_et_defs_out = scheme_null;

  all_defs = scheme_null;
  all_et_defs = scheme_null;

  exp_body = scheme_null;

  self_modidx = env->genv->module->self_modidx;

  post_ex_rn = scheme_make_module_rename(0, mzMOD_RENAME_MARKED, env->genv->marked_names);
  post_ex_et_rn = scheme_make_module_rename(1, mzMOD_RENAME_MARKED, env->genv->exp_env->marked_names);
  post_ex_tt_rn = scheme_make_module_rename(-1, mzMOD_RENAME_MARKED, env->genv->template_env->marked_names);
  post_ex_dt_rn = scheme_make_module_rename(MZ_LABEL_PHASE, mzMOD_RENAME_MARKED, env->genv->template_env->marked_names);

  /* For syntax-local-context, etc., in a d-s RHS: */
  rhs_env = scheme_new_comp_env(env->genv, env->insp, SCHEME_TOPLEVEL_FRAME);

  scheme_rec_add_certs(rec, drec, form);

  /* It's possible that #%module-begin expansion introduces
     marked identifiers for definitions. */
  form = scheme_add_rename(form, post_ex_rn);
  form = scheme_add_rename(form, post_ex_et_rn);
  form = scheme_add_rename(form, post_ex_tt_rn);
  form = scheme_add_rename(form, post_ex_dt_rn);

  maybe_has_lifts = 0;
  lift_ctx = scheme_generate_lifts_key();
  
  /* Pass 1 */

  observer = rec[drec].observer;

  /* Partially expand all expressions, and process definitions, requires,
     and provides. Also, flatten top-level `begin' expressions: */
  for (fm = SCHEME_STX_CDR(form); !SCHEME_STX_NULLP(fm); ) {
    Scheme_Object *e;
    int kind;

    while (1) {
      Scheme_Object *fst;

      SCHEME_EXPAND_OBSERVE_NEXT(observer);

      e = SCHEME_STX_CAR(fm);

      p = (maybe_has_lifts 
           ? scheme_frame_get_end_statement_lifts(xenv) 
           : scheme_null);
      scheme_frame_captures_lifts(xenv, scheme_make_lifted_defn, scheme_sys_wraps(xenv), p, lift_ctx);
      maybe_has_lifts = 1;

      {
	Scheme_Expand_Info erec1;
	erec1.comp = 0;
	erec1.depth = -1;
	erec1.value_name = scheme_false;
	erec1.certs = rec[drec].certs;
        erec1.observer = rec[drec].observer;
	e = scheme_expand_expr(e, xenv, &erec1, 0);	
      }

      fst = scheme_frame_get_lifts(xenv);
      if (!SCHEME_NULLP(fst)) {
	/* Expansion lifted expressions, so add them to
	   the front and try again. */
	fm = SCHEME_STX_CDR(fm);
        e = scheme_add_rename(e, post_ex_rn);
        e = scheme_add_rename(e, post_ex_et_rn);
        e = scheme_add_rename(e, post_ex_tt_rn);
        e = scheme_add_rename(e, post_ex_dt_rn);
        fm = scheme_named_map_1(NULL, add_a_rename, fm, post_ex_rn);
        fm = scheme_named_map_1(NULL, add_a_rename, fm, post_ex_et_rn);
        fm = scheme_named_map_1(NULL, add_a_rename, fm, post_ex_tt_rn);
        fm = scheme_named_map_1(NULL, add_a_rename, fm, post_ex_dt_rn);
	fm = scheme_append(fst, scheme_make_pair(e, fm));
        SCHEME_EXPAND_OBSERVE_MODULE_LIFT_LOOP(observer, fst);
      } else {
	/* No definition lifts added... */
	if (SCHEME_STX_PAIRP(e))
	  fst = SCHEME_STX_CAR(e);
	else
	  fst = NULL;
	
	if (fst && SCHEME_STX_SYMBOLP(fst) && scheme_stx_module_eq(scheme_begin_stx, fst, 0)) {
	  fm = SCHEME_STX_CDR(fm);
	  e = scheme_add_rename(e, post_ex_rn);
	  e = scheme_add_rename(e, post_ex_et_rn);
	  e = scheme_add_rename(e, post_ex_tt_rn);
	  e = scheme_add_rename(e, post_ex_dt_rn);
	  fm = scheme_flatten_begin(e, fm);
	  SCHEME_EXPAND_OBSERVE_SPLICE(observer, fm);
	  if (SCHEME_STX_NULLP(fm)) {
            fm = scheme_frame_get_end_statement_lifts(xenv);
            fm = scheme_reverse(fm);
            SCHEME_EXPAND_OBSERVE_MODULE_LIFT_END_LOOP(observer, fm);
            maybe_has_lifts = 0;
            if (SCHEME_NULLP(fm)) {
              e = NULL;
              break;
            }
	  }
	} else
          break;
      }
    }
    if (!e) break; /* (begin) expansion at end */

    e = scheme_add_rename(e, post_ex_rn);
    e = scheme_add_rename(e, post_ex_et_rn);
    e = scheme_add_rename(e, post_ex_tt_rn);
    e = scheme_add_rename(e, post_ex_dt_rn);
    
    if (SCHEME_STX_PAIRP(e)) {
      Scheme_Object *fst;

      fst = SCHEME_STX_CAR(e);

      if (SCHEME_STX_SYMBOLP(fst)) {

	Scheme_Object *n;
	n = SCHEME_STX_CAR(e);
	if (scheme_stx_module_eq(scheme_define_values_stx, fst, 0)) {
	  /************ define-values *************/
	  Scheme_Object *vars, *val;

          SCHEME_EXPAND_OBSERVE_ENTER_PRIM(observer, e);
          SCHEME_EXPAND_OBSERVE_PRIM_DEFINE_VALUES(observer);

	  /* Create top-level vars */
	  scheme_define_parse(e, &vars, &val, 0, env, 1);

	  while (SCHEME_STX_PAIRP(vars)) {
	    Scheme_Object *name, *orig_name;

	    name = SCHEME_STX_CAR(vars);

	    orig_name = name;

	    /* Remember the original: */
	    all_defs = scheme_make_pair(name, all_defs);
	    
	    name = scheme_tl_id_sym(env->genv, name, NULL, 2);

	    /* Check that it's not yet defined: */
	    if (scheme_lookup_in_table(env->genv->toplevel, (const char *)name)) {
	      scheme_wrong_syntax("module", orig_name, e, "duplicate definition for identifier");
	      return NULL;
	    }

	    /* Not required: */
	    if (scheme_hash_get(required, name)) {
	      scheme_wrong_syntax("module", orig_name, e, "identifier is already imported");
	      return NULL;
	    }

	    /* Not syntax: */
	    if (scheme_lookup_in_table(env->genv->syntax, (const char *)name)) {
	      scheme_wrong_syntax("module", orig_name, e, "duplicate definition for identifier");
	      return NULL;
	    }

	    /* Create the bucket, indicating that the name will be defined: */
	    scheme_add_global_symbol(name, scheme_undefined, env->genv);

	    /* Add a renaming: */
	    if (!SAME_OBJ(SCHEME_STX_VAL(orig_name), name))
	      scheme_extend_module_rename(post_ex_rn, self_modidx, name, name, self_modidx, name, 0, 0);
	    else
	      scheme_extend_module_rename(rn, self_modidx, name, name, self_modidx, name, 0, 0);
	    
	    vars = SCHEME_STX_CDR(vars);
	  }
          
          SCHEME_EXPAND_OBSERVE_EXIT_PRIM(observer, e);
	  kind = 2;
	} else if (scheme_stx_module_eq(scheme_define_syntaxes_stx, fst, 0)
		   || scheme_stx_module_eq(define_for_syntaxes_stx, fst, 0)) {
	  /************ define-syntaxes & define-values-for-syntax *************/
	  /* Define the macro: */
	  Scheme_Compile_Info mrec;
	  Scheme_Object *names, *l, *code, *m, *vec, *boundname;
	  Resolve_Prefix *rp;
	  Resolve_Info *ri;
	  Scheme_Comp_Env *oenv, *eenv;
	  Optimize_Info *oi;
	  int count = 0;
	  int for_stx;

	  for_stx = scheme_stx_module_eq(define_for_syntaxes_stx, fst, 0);

          SCHEME_EXPAND_OBSERVE_ENTER_PRIM(observer, e);
          SCHEME_EXPAND_OBSERVE_PRIM_DEFINE_SYNTAXES(observer);

	  scheme_define_parse(e, &names, &code, 1, env, 1);

	  if (SCHEME_STX_PAIRP(names) && SCHEME_STX_NULLP(SCHEME_STX_CDR(names)))
	    boundname = SCHEME_STX_CAR(names);
	  else
	    boundname = scheme_false;
	  
	  scheme_prepare_exp_env(env->genv);
	  eenv = scheme_new_comp_env(env->genv->exp_env, env->insp, 0);

	  oenv = (for_stx ? eenv : env);
	  
	  for (l = names; SCHEME_STX_PAIRP(l); l = SCHEME_STX_CDR(l)) {
	    Scheme_Object *name, *orig_name;
	    name = SCHEME_STX_CAR(l);

	    orig_name = name;

            /* Remember the original: */
	    if (!for_stx)
	      all_defs = scheme_make_pair(name, all_defs);
            else
              all_et_defs = scheme_make_pair(name, all_et_defs);
	    
	    name = scheme_tl_id_sym(oenv->genv, name, NULL, 2);
	    
	    if (scheme_lookup_in_table(oenv->genv->syntax, (const char *)name)) {
	      scheme_wrong_syntax("module", orig_name, e, 
				  (for_stx
				   ? "duplicate for-syntax definition for identifier"
				   : "duplicate definition for identifier"));
	      return NULL;
	    }
	    
	    /* Check that it's not yet defined: */
	    if (scheme_lookup_in_table(oenv->genv->toplevel, (const char *)name)) {
	      scheme_wrong_syntax("module", orig_name, e, 
				  (for_stx
				   ? "duplicate for-syntax definition for identifier"
				   : "duplicate definition for identifier"));
	      return NULL;
	    }

	    /* Not required: */
	    if (scheme_hash_get(for_stx ? et_required : required, name)) {
	      scheme_wrong_syntax("module", orig_name, e, 
				  (for_stx
				   ? "identifier is already imported for syntax"
				   : "identifier is already imported"));
	      return NULL;
	    }

	    if (!SAME_OBJ(SCHEME_STX_VAL(orig_name), name))
	      scheme_extend_module_rename(for_stx ? post_ex_et_rn : post_ex_rn, self_modidx, name, name, self_modidx, name,
					  for_stx ? 1 : 0, 0);
	    else
	      scheme_extend_module_rename(for_stx ? et_rn : rn, self_modidx, name, name, self_modidx, name,
					  for_stx ? 1 : 0, 0);

	    count++;
	  }

	  names = scheme_named_map_1(NULL, stx_sym, names, (Scheme_Object *)oenv->genv);
	  
	  mrec.comp = 1;
	  mrec.dont_mark_local_use = 0;
	  mrec.resolve_module_ids = 0;
	  mrec.value_name = NULL;
	  mrec.certs = rec[drec].certs;
          mrec.observer = NULL;

	  if (!rec[drec].comp) {
	    Scheme_Expand_Info erec1;
	    erec1.comp = 0;
	    erec1.depth = -1;
	    erec1.value_name = boundname;
	    erec1.certs = rec[drec].certs;
            erec1.observer = rec[drec].observer;
	    SCHEME_EXPAND_OBSERVE_PHASE_UP(observer);
	    code = scheme_expand_expr_lift_to_let(code, eenv, &erec1, 0);
	  }
	  m = scheme_compile_expr_lift_to_let(code, eenv, &mrec, 0);

	  oi = scheme_optimize_info_create();
	  m = scheme_optimize_expr(m, oi);
	  
	  /* Simplify only in compile mode; it is too slow in expand mode. */
	  rp = scheme_resolve_prefix(1, eenv->prefix, rec[drec].comp);
	  ri = scheme_resolve_info_create(rp);
          scheme_enable_expression_resolve_lifts(ri);
	  m = scheme_resolve_expr(m, ri);
          m = scheme_merge_expression_resolve_lifts(m, rp, ri);
          rp = scheme_remap_prefix(rp, ri);

	  /* Add code with names and lexical depth to exp-time body: */
	  vec = scheme_make_vector(5, NULL);
	  SCHEME_VEC_ELS(vec)[0] = names;
	  SCHEME_VEC_ELS(vec)[1] = m;
	  SCHEME_VEC_ELS(vec)[2] = scheme_make_integer(ri->max_let_depth);
	  SCHEME_VEC_ELS(vec)[3] = (Scheme_Object *)rp;
	  SCHEME_VEC_ELS(vec)[4] = (for_stx ? scheme_true : scheme_false);
	  exp_body = scheme_make_pair(vec, exp_body);

	  if (ri->use_jit)
	    m = scheme_jit_expr(m);
	
	  eval_defmacro(names, count, m, eenv->genv, rhs_env, rp, ri->max_let_depth, 0, 
			(for_stx ? env->genv->exp_env->toplevel : env->genv->syntax), for_stx,
			rec[drec].certs);

	  if (rec[drec].comp)
	    e = NULL;
	  else {
	    m = SCHEME_STX_CDR(e);
	    m = SCHEME_STX_CAR(m);
	    m = scheme_make_pair(SCHEME_CAR(fst),
				 scheme_make_pair(m, scheme_make_pair(code, scheme_null)));
	    e = scheme_datum_to_syntax(m, e, e, 0, 2);
	  }
          
          SCHEME_EXPAND_OBSERVE_EXIT_PRIM(observer, e);
	  kind = 0;
	} else if (scheme_stx_module_eq(require_stx, fst, 0)) {	
	  /************ require *************/
	  Scheme_Object *imods;
          
          SCHEME_EXPAND_OBSERVE_ENTER_PRIM(observer, e);
          SCHEME_EXPAND_OBSERVE_PRIM_REQUIRE(observer);

	  /* Add requires to renaming: */
	  imods = parse_requires(e, self_modidx, env->genv, 
				 rn, post_ex_rn, 
                                 et_rn, post_ex_et_rn,
                                 dt_rn, post_ex_dt_rn,
                                 check_require_name, tables, et_tables, dt_tables, 0, 1,
				 redef_modname, 0, 0, 1,
				 &all_simple_renames);
	
	  /* Add required modules to requires list: */
	  add_req(imods, env->genv->module->requires);

	  if (rec[drec].comp)
	    e = NULL;

          SCHEME_EXPAND_OBSERVE_EXIT_PRIM(observer, e);
	  kind = 0;
	} else if (scheme_stx_module_eq(require_for_syntax_stx, fst, 0)) {	
	  /************ require-for-syntax *************/
	  Scheme_Object *imods;

          SCHEME_EXPAND_OBSERVE_ENTER_PRIM(observer, e);
          SCHEME_EXPAND_OBSERVE_PRIM_REQUIRE_FOR_SYNTAX(observer);

	  scheme_prepare_exp_env(env->genv);

	  /* Add requires to renaming: */
	  imods = parse_requires(e, self_modidx, env->genv->exp_env,
				 et_rn, post_ex_et_rn, 
                                 NULL, NULL,
                                 NULL, NULL,
                                 check_require_name, et_tables, NULL, NULL, 1, 0,
				 redef_modname, 0, 0, 1,
				 &et_all_simple_renames);

	  /* Add required modules to et_requires list: */
	  {
	    Scheme_Object *reqs;
	    reqs = add_req(imods, env->genv->module->et_requires);
	    env->genv->module->et_requires = reqs;
	  }

	  if (rec[drec].comp)
	    e = NULL;

          SCHEME_EXPAND_OBSERVE_EXIT_PRIM(observer, e);
	  kind = 0;
	} else if (scheme_stx_module_eq(require_for_template_stx, fst, 0)) {	
	  /************ require-for-template *************/
	  Scheme_Object *imods;

          SCHEME_EXPAND_OBSERVE_ENTER_PRIM(observer, e);
          SCHEME_EXPAND_OBSERVE_PRIM_REQUIRE_FOR_TEMPLATE(observer);

	  scheme_prepare_template_env(env->genv);

	  /* Add requires to renaming: */
	  imods = parse_requires(e, self_modidx, env->genv->template_env,
				 tt_rn, post_ex_tt_rn,
                                 NULL, NULL,
                                 NULL, NULL,
                                 check_require_name, tt_tables, NULL, NULL, 0, 0,
				 redef_modname, 0, 0, 1,
				 &tt_all_simple_renames);

	  /* Add required modules to tt_requires list: */
	  {
	    Scheme_Object *reqs;
	    reqs = add_req(imods, env->genv->module->tt_requires);
	    env->genv->module->tt_requires = reqs;
	  }

	  if (rec[drec].comp)
	    e = NULL;

          SCHEME_EXPAND_OBSERVE_EXIT_PRIM(observer, e);
	  kind = 0;
	} else if (scheme_stx_module_eq(require_for_label_stx, fst, 0)) {	
	  /************ require-for-label *************/
	  Scheme_Object *imods;

          SCHEME_EXPAND_OBSERVE_ENTER_PRIM(observer, e);
          SCHEME_EXPAND_OBSERVE_PRIM_REQUIRE_FOR_TEMPLATE(observer); /* FIXME! */

	  scheme_prepare_template_env(env->genv);

	  /* Add requires to renaming: */
	  imods = parse_requires(e, self_modidx, env->genv->template_env,
				 dt_rn, post_ex_dt_rn, 
                                 NULL, NULL,
                                 NULL, NULL,
                                 check_require_name, dt_tables, NULL, NULL, 0, 0,
				 redef_modname, 0, 0, 1,
				 &dt_all_simple_renames);

	  /* Add required modules to dt_requires list: */
	  {
	    Scheme_Object *reqs;
	    reqs = add_req(imods, env->genv->module->dt_requires);
	    env->genv->module->dt_requires = reqs;
	  }

	  if (rec[drec].comp)
	    e = NULL;

          SCHEME_EXPAND_OBSERVE_EXIT_PRIM(observer, e);
	  kind = 0;
	} else if (scheme_stx_module_eq(provide_stx, fst, 0)) {
	  /************ provide *************/
	  /* Add provides to table: */

          SCHEME_EXPAND_OBSERVE_ENTER_PRIM(observer, e);
          SCHEME_EXPAND_OBSERVE_PRIM_PROVIDE(observer);

          reprovided = parse_provides(form, fst, e, 
                                      provided, reprovided,
                                      self_modidx,
                                      &all_defs_out);

	  if (rec[drec].comp)
	    e = NULL;

          SCHEME_EXPAND_OBSERVE_EXIT_PRIM(observer, e);
	  kind = 0;
	} else if (scheme_stx_module_eq(provide_for_syntax_stx, fst, 0)) {
	  /************ provide-for-syntax *************/
	  /* Add provides to table: */

          SCHEME_EXPAND_OBSERVE_ENTER_PRIM(observer, e);
          SCHEME_EXPAND_OBSERVE_PRIM_PROVIDE(observer); /* FIXME! */

          et_reprovided = parse_provides(form, fst, e, 
                                         et_provided, et_reprovided,
                                         self_modidx,
                                         &all_et_defs_out);

	  if (rec[drec].comp)
	    e = NULL;

          SCHEME_EXPAND_OBSERVE_EXIT_PRIM(observer, e);
	  kind = 0;
	} else if (scheme_stx_module_eq(provide_for_label_stx, fst, 0)) {
	  /************ provide-for-label *************/
	  /* Add provides to table: */

          SCHEME_EXPAND_OBSERVE_ENTER_PRIM(observer, e);
          SCHEME_EXPAND_OBSERVE_PRIM_PROVIDE(observer); /* FIXME! */

          dt_reprovided = parse_provides(form, fst, e, 
                                         dt_provided, dt_reprovided,
                                         self_modidx,
                                         NULL);

	  if (rec[drec].comp)
	    e = NULL;

          SCHEME_EXPAND_OBSERVE_EXIT_PRIM(observer, e);
	  kind = 0;
	} else {
	  kind = 1;
        }
      } else
	kind = 1;
    } else
      kind = 1;

    if (e) {
      p = scheme_make_pair(scheme_make_pair(e, scheme_make_integer(kind)), scheme_null);
      if (last)
	SCHEME_CDR(last) = p;
      else
	first = p;
      last = p;
    }

    fm = SCHEME_STX_CDR(fm);

    /* If we're out of declarations, check for lifted-to-end: */
    if (SCHEME_STX_NULLP(fm) && maybe_has_lifts) {
      fm = scheme_frame_get_end_statement_lifts(xenv);
      fm = scheme_reverse(fm);
      SCHEME_EXPAND_OBSERVE_MODULE_LIFT_END_LOOP(observer, fm);
      maybe_has_lifts = 0;
    }
  }
  /* first =  a list of (cons semi-expanded-expression kind) */

  /* Pass 2 */
  SCHEME_EXPAND_OBSERVE_NEXT_GROUP(observer);
  
  if (rec[drec].comp) {
    /* Module manages its own prefix. That's how we get
       multiple instantiation of a module with "dynamic linking". */
    cenv = scheme_new_comp_env(env->genv, env->insp, SCHEME_TOPLEVEL_FRAME);
  } else
    cenv = scheme_extend_as_toplevel(env);

  lift_data = scheme_make_vector(3, NULL);
  SCHEME_VEC_ELS(lift_data)[0] = (Scheme_Object *)cenv;
  SCHEME_VEC_ELS(lift_data)[1] = self_modidx;
  SCHEME_VEC_ELS(lift_data)[2] = rn;

  maybe_has_lifts = 0;

  prev_p = NULL;
  for (p = first; !SCHEME_NULLP(p); ) {
    Scheme_Object *e, *l, *ll;
    int kind;

    e = SCHEME_CAR(p);
    kind = SCHEME_INT_VAL(SCHEME_CDR(e));
    e = SCHEME_CAR(e);
    
    SCHEME_EXPAND_OBSERVE_NEXT(observer);

    if (kind) {
      Scheme_Comp_Env *nenv;

      l = (maybe_has_lifts 
           ? scheme_frame_get_end_statement_lifts(cenv) 
           : scheme_null);
      scheme_frame_captures_lifts(cenv, add_lifted_defn, lift_data, l, lift_ctx);
      maybe_has_lifts = 1;

      if (kind == 2)
        nenv = cenv;
      else
        nenv = scheme_new_compilation_frame(0, 0, cenv, NULL);

      if (rec[drec].comp) {
	Scheme_Compile_Info crec1;
	scheme_init_compile_recs(rec, drec, &crec1, 1);
	crec1.resolve_module_ids = 0;
	e = scheme_compile_expr(e, nenv, &crec1, 0);
      } else {
	Scheme_Expand_Info erec1;
	scheme_init_expand_recs(rec, drec, &erec1, 1);
	erec1.value_name = scheme_false;
	e = scheme_expand_expr(e, nenv, &erec1, 0);
      }
      
      l = scheme_frame_get_lifts(cenv);
      if (SCHEME_NULLP(l)) {
	/* No lifts - continue normally */
	SCHEME_CAR(p) = e;
	prev_p = p;
	p = SCHEME_CDR(p);
      } else {
	/* Lifts - insert them and try again */
        SCHEME_EXPAND_OBSERVE_MODULE_LIFT_LOOP(observer, scheme_copy_list(l));
	e = scheme_make_pair(e, scheme_make_integer(0)); /* don't re-compile/-expand */
	SCHEME_CAR(p) = e;
	for (ll = l; SCHEME_PAIRP(ll); ll = SCHEME_CDR(ll)) {
	  e = scheme_make_pair(SCHEME_CAR(ll), scheme_make_integer(2));
	  SCHEME_CAR(ll) = e;
	}
	p = scheme_append(l, p);
	if (prev_p) {
	  SCHEME_CDR(prev_p) = p;
	} else {
	  first = p;
	}
      }
    } else {
      SCHEME_CAR(p) = e;
      prev_p = p;
      p = SCHEME_CDR(p);
    }

    /* If we're out of declarations, check for lifted-to-end: */
    if (SCHEME_NULLP(p) && maybe_has_lifts) {
      p = scheme_frame_get_end_statement_lifts(cenv);
      SCHEME_EXPAND_OBSERVE_MODULE_LIFT_END_LOOP(observer, scheme_reverse(p));
      p = scheme_reverse(p);
      for (ll = p; SCHEME_PAIRP(ll); ll = SCHEME_CDR(ll)) {
        e = scheme_make_pair(SCHEME_CAR(ll), scheme_make_integer(1));
        SCHEME_CAR(ll) = e;
      }
      maybe_has_lifts = 0;
      if (prev_p) {
        SCHEME_CDR(prev_p) = p;
      } else {
        first = p;
      }
    }
  }
  /* first = a list of expanded/compiled expressions */

  et_mn = env->genv->exp_env->marked_names;

  /* If compiling, drop expressions that are constants: */
  if (rec[drec].comp) {
    Scheme_Object *prev = NULL, *next;
    for (p = first; !SCHEME_NULLP(p); p = next) {
      next = SCHEME_CDR(p);
      if (scheme_omittable_expr(SCHEME_CAR(p), 1)) {
	if (prev)
	  SCHEME_CDR(p) = next;
	else
	  first = next;
      } else
	prev = p;
    }
  }

  /* Compute provides for re-provides and all-defs-out: */
  reprovide_kernel = compute_reprovides(provided, reprovided, 
                                        env->genv->module->requires, required,
                                        env->genv, all_defs, all_defs_out, 
                                        &exclude_hint,
                                        "require");
  (void)compute_reprovides(et_provided, et_reprovided, 
                           env->genv->module->et_requires, et_required,
                           env->genv->exp_env, all_et_defs, all_et_defs_out, 
                           NULL,
                           "require-for-syntax");
  (void)compute_reprovides(dt_provided, dt_reprovided, 
                           env->genv->module->dt_requires, dt_required,
                           NULL, NULL, NULL,
                           NULL,
                           "require-for-label");

  /* Ad hoc optimization: mzscheme is everything from kernel except
     #%module_begin */
  if ((reprovide_kernel == (kernel->me->rt->num_provides - 1))
      && SCHEME_FALSEP(exclude_hint)) {
    exclude_hint = scheme_make_pair(module_begin_symbol, scheme_null);
    exclude_hint = scheme_datum_to_syntax(exclude_hint, scheme_false, scheme_top_stx, 0, 0);
  }

  /* Re-providing all of the kernel without prefixing? */
  if (reprovide_kernel) {
    if ((reprovide_kernel == (kernel->me->rt->num_provides - 1))
	&& SCHEME_TRUEP(exclude_hint)) {
      if (SCHEME_STX_PAIRP(exclude_hint) && SCHEME_NULLP(SCHEME_STX_CDR(exclude_hint))) {
	Scheme_Object *n;

	exclude_hint = SCHEME_STX_CAR(exclude_hint);
	exclude_hint = SCHEME_STX_VAL(exclude_hint);
	n = scheme_hash_get(provided, exclude_hint);
	if (n) {
	  /* may be a single shadowed exclusion, now bound to exclude_hint... */
	  n = SCHEME_CAR(n);
	  if (SCHEME_STXP(n))
	    n = scheme_tl_id_sym(env->genv, n, NULL, 0);
	  n = scheme_hash_get(required, n);
	  if (n && !SAME_OBJ(SCHEME_VEC_ELS(n)[1], kernel_symbol)) {
	    /* there is a single shadowed exclusion. */
	  } else
	    reprovide_kernel = 0;
	} else
	  reprovide_kernel = 0;
      } else
	reprovide_kernel = 0;
    } else if (reprovide_kernel != kernel->me->rt->num_provides)
      reprovide_kernel = 0;
    else
      exclude_hint = scheme_false;
  }
  /* If reprovide_kernel is non-zero, we re-provide all of it */

  /* Compute provide arrays */
  exps = compute_provide_arrays(provided, required,
                                env->genv->module->me->rt,
                                env->genv, 0,
                                reprovide_kernel,
                                form,
                                "provided identifier not defined or imported");
  (void)compute_provide_arrays(et_provided, et_required,
                               env->genv->module->me->et,
                               env->genv->exp_env, 1,
                               0,
                               form,
                               "for-syntax provided identifier not defined for syntax or imported for syntax");
  (void)compute_provide_arrays(dt_provided, dt_required,
                               env->genv->module->me->dt,
                               NULL, 0,
                               0,
                               form,
                               "for-label provided identifier not imported for label");
  
  if (rec[drec].comp || (rec[drec].depth != -2)) {
    scheme_clean_dead_env(env->genv);
  }

  /* Compute indirect provides (which is everything at the top-level): */
  {
    int i, count, j;
    Scheme_Bucket **bs, *b;
    Scheme_Object **exsns = env->genv->module->me->rt->provide_src_names;
    int exvcount = env->genv->module->me->rt->num_var_provides;
    
    bs = env->genv->toplevel->buckets;
    for (count = 0, i = env->genv->toplevel->size; i--; ) {
      b = bs[i];
      if (b && b->val)
	count++;
    }

    exis = MALLOC_N(Scheme_Object *, count);

    for (count = 0, i = env->genv->toplevel->size; i--; ) {
      b = bs[i];
      if (b && b->val) {
	Scheme_Object *name;
	  
	name = (Scheme_Object *)b->key;
	
	/* If the name is directly provided, no need for indirect... */
	for (j = 0; j < exvcount; j++) {
	  if (SAME_OBJ(name, exsns[j]))
	    break;
	}
	
	if (j == exvcount)
	  exis[count++] = name;
      }
    }

    exicount = count;

    qsort_provides(exis, NULL, NULL, NULL, NULL, 0, exicount, 1);
  }

  if (!rec[drec].comp) {
    Scheme_Module_Phase_Exports *rt = env->genv->module->me->rt;
    int excount = rt->num_provides;
    int exvcount = rt->num_var_provides;
    Scheme_Object **exsns = rt->provide_src_names;
    Scheme_Object **exs = rt->provides;
    Scheme_Object **exss = rt->provide_srcs;

    /* Produce annotations (in the form of properties)
       for module information:
         'module-variable-provides = '(item ...)
         'module-syntax-provides = '(item ...)
	 'module-indirect-provides = '(id ...)
         'module-kernel-reprovide-hint = 'kernel-reexport

      item = name
           | (ext-id . def-id)
           | (modidx ext-id . def-id)
     kernel-reexport = #f
                     | #t
                     | exclusion-id
    */
    int j;
    Scheme_Object *e, *a, *result;

    result = scheme_null;

    /* kernel re-export info: */
    if (reprovide_kernel) {
      if (exclude_hint)
	result = scheme_make_immutable_pair(exclude_hint, result);
      else
	result = scheme_make_immutable_pair(scheme_true, result);
    } else
      result = scheme_make_immutable_pair(scheme_false, result);

    /* Indirect provides */ 
    a = scheme_null;
    for (j = 0; j < exicount; j++) {
      a = scheme_make_immutable_pair(exis[j], a);
    }
    result = scheme_make_immutable_pair(a, result);
    
    /* add syntax and value exports: */
    for (j = 0; j < 2; j++) {
      int top, i;

      e = scheme_null;

      if (reprovide_kernel) {
	if (!j) {
	  i = kernel->me->rt->num_var_provides;
	  top = kernel->me->rt->num_provides;
	} else {
	  i = 0;
	  top = kernel->me->rt->num_var_provides;
	}

	for (; i < top; i++) {
	  if (!SAME_OBJ(kernel->me->rt->provides[i], exclude_hint)) {
	    a = scheme_make_immutable_pair(kernel->me->rt->provides[i], kernel->me->rt->provides[i]);
	    a = scheme_make_immutable_pair(kernel_symbol, a);
	    e = scheme_make_immutable_pair(a, e);
	  }
	}
      }

      if (!j) {
	i = exvcount;
	top = excount;
      } else {
	i = 0;
	top = exvcount;
      }
      
      for (; i < top; i++) {
	if (SCHEME_FALSEP(exss[i])
	    && SAME_OBJ(exs[i], exsns[i]))
	  a = exs[i];
	else {
	  a = scheme_make_immutable_pair(exs[i], exsns[i]);
	  if (!SCHEME_FALSEP(exss[i])) {
	    a = scheme_make_immutable_pair(exss[i], a);
	  }
	}
	e = scheme_make_immutable_pair(a, e);
      }
      result = scheme_make_immutable_pair(e, result);
    }

    env->genv->module->hints = result;
  }

  if (rec[drec].comp) {
    Scheme_Object *exp_body_r = scheme_null;
    
    /* Reverse exp_body */
    while (!SCHEME_NULLP(exp_body)) {
      exp_body_r = scheme_make_pair(SCHEME_CAR(exp_body),
				    exp_body_r);
      exp_body = SCHEME_CDR(exp_body);
    }

    env->genv->module->body = first;
    env->genv->module->et_body = exp_body_r;

    env->genv->module->provide_protects = exps;

    env->genv->module->me->rt->reprovide_kernel = reprovide_kernel;
    env->genv->module->me->rt->kernel_exclusion = exclude_hint;

    env->genv->module->indirect_provides = exis;
    env->genv->module->num_indirect_provides = exicount;

    env->genv->module->comp_prefix = cenv->prefix;

    if (all_simple_renames 
	&& (env->genv->marked_names->count == 1 /* just the false mapping */)) {
      env->genv->module->rn_stx = scheme_true;
    }
    if (et_all_simple_renames 
        && all_simple_renames
	&& (et_mn->count == 1 /* just the false mapping */)) {
      env->genv->module->et_rn_stx = scheme_true;
    }
    if (tt_all_simple_renames) {
      env->genv->module->tt_rn_stx = scheme_true;
    }
    if (dt_all_simple_renames
        && all_simple_renames) {
      env->genv->module->dt_rn_stx = scheme_true;
    }

    return (Scheme_Object *)env->genv->module;
  } else {
    if (rec[drec].depth == -2) {
      /* This was a local expand. Flush definitions, because the body expand may start over. */
      flush_definitions(env->genv);
      if (env->genv->exp_env)
        flush_definitions(env->genv->exp_env);
    }

    p = SCHEME_STX_CAR(form);
    return scheme_datum_to_syntax(cons(p, first), form, form, 0, 2);
  }
}

static Scheme_Object *
module_begin_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_module_begin(form, env, rec, drec);
}

static Scheme_Object *
module_begin_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_MODULE_BEGIN(erec[drec].observer);
  return do_module_begin(form, env, erec, drec);
}

int compute_reprovides(Scheme_Hash_Table *provided, Scheme_Object *reprovided, 
                       Scheme_Object *requires, Scheme_Hash_Table *required, 
                       Scheme_Env *genv, Scheme_Object *all_defs, Scheme_Object *all_defs_out, 
                       Scheme_Object **_exclude_hint,
                       const char *matching_form)
{
  int i;
  Scheme_Object *rx;
  int reprovide_kernel = 0;

  /* First, check the sanity of the re-provide specifications: */
  for (rx = reprovided; !SCHEME_NULLP(rx); rx = SCHEME_CDR(rx)) {
    Scheme_Object *midx = SCHEME_CAR(SCHEME_CAR(rx)), *l, *exns;
	
    for (l = requires; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
      if (same_modidx(midx, SCHEME_CAR(l)))
        break;
    }
    if (SCHEME_NULLP(l)) {
      /* Didn't require the named module */
      Scheme_Object *name;
      name = SCHEME_CAR(rx);
      name = SCHEME_STX_CDR(name);
      name = SCHEME_STX_CAR(name);
      scheme_wrong_syntax("module", 
                          SCHEME_SYMBOLP(midx) ? midx : ((Scheme_Modidx *)midx)->path, 
                          name,
                          "cannot provide from a module without a matching `%s'",
                          matching_form);
    }

    exns = SCHEME_CDR(SCHEME_CDR(SCHEME_CAR(rx)));
    for (l = exns; !SCHEME_STX_NULLP(l); l = SCHEME_STX_CDR(l)) {
      /* Make sure excluded name was required: */
      Scheme_Object *a, *vec;
      a = SCHEME_STX_VAL(SCHEME_STX_CAR(l));
      vec = scheme_hash_get(required, a);
      if (vec) {
        /* Check for nominal modidx in list */
        Scheme_Object *nml;
        nml = SCHEME_VEC_ELS(vec)[0];
        for (; SCHEME_PAIRP(nml); nml = SCHEME_CDR(nml)) {
          if (same_modidx(SCHEME_CAR(SCHEME_CAR(rx)), SCHEME_CAR(nml)))
            break;
        }
        if (!SCHEME_PAIRP(nml))
          vec = NULL; /* So it was provided, but not from the indicated module */
      }
      if (!vec) {
        a = SCHEME_STX_CAR(l);
        scheme_wrong_syntax("module", a, SCHEME_CADR(SCHEME_CAR(rx)),
                            "excluded name was not required from the module");
      }
    }
  }

  /* Walk through requires, check for re-providing: */
  for (i = required->size; i--; ) {
    if (required->vals[i]) {
      Scheme_Object *nominal_modidx, *name, *modidx, *srcname, *outname, *nml, *orig_nml;
      int break_outer = 0;
	
      name = required->keys[i];
      orig_nml = SCHEME_VEC_ELS(required->vals[i])[0];
      modidx = SCHEME_VEC_ELS(required->vals[i])[1];
      srcname = SCHEME_VEC_ELS(required->vals[i])[2];
      outname = SCHEME_VEC_ELS(required->vals[i])[4];

      for (rx = reprovided; !SCHEME_NULLP(rx); rx = SCHEME_CDR(rx)) {
        for (nml = orig_nml; SCHEME_PAIRP(nml); nml = SCHEME_CDR(nml)) {
          nominal_modidx = SCHEME_CAR(nml);
          if (same_modidx(SCHEME_CAR(SCHEME_CAR(rx)), nominal_modidx)) {
            Scheme_Object *exns, *ree;
	      
            break_outer = 1;
	      
            ree = SCHEME_CDR(SCHEME_CAR(rx));

            exns = SCHEME_CDR(ree);
            if (SAME_OBJ(modidx, kernel_symbol))
              if (!SCHEME_STX_NULLP(exns)) {
                if (_exclude_hint)
                  *_exclude_hint = exns;
              }
	    
            for (; !SCHEME_STX_NULLP(exns); exns = SCHEME_STX_CDR(exns)) {
              /* Was this name exluded? */
              Scheme_Object *a;
              a = SCHEME_STX_VAL(SCHEME_STX_CAR(exns));
              if (SAME_OBJ(a, name))
                break;
            }

            if (SCHEME_STX_NULLP(exns)) {
              /* Not excluded, so provide it. */
              if (scheme_hash_get(provided, outname))
                scheme_wrong_syntax("module", outname, SCHEME_CAR(ree), "identifier already provided");
	      
              scheme_hash_set(provided, outname, scheme_make_pair(name, scheme_false));

              if (SAME_OBJ(modidx, kernel_symbol) && SAME_OBJ(outname, srcname))
                reprovide_kernel++;
            }
          }
        }
        if (break_outer) break;
      }
    }
  }

  /* Do all-defined provides */
  if (all_defs_out) {
    for (; !SCHEME_NULLP(all_defs_out); all_defs_out = SCHEME_CDR(all_defs_out)) {
      Scheme_Object *exns, *ree, *ree_kw, *exl, *name, *a, *adl, *exname, *pfx;
      int protected;
	    
      ree = SCHEME_CAR(all_defs_out);
      protected = SCHEME_TRUEP(SCHEME_CDR(ree));
      ree = SCHEME_CAR(ree);
      ree_kw = SCHEME_CAR(ree);
      ree = SCHEME_CDR(ree);
      exl = SCHEME_CAR(ree);
      pfx = SCHEME_CDR(ree);

      /* Make sure each excluded name was defined: */
      for (exns = exl; !SCHEME_STX_NULLP(exns); exns = SCHEME_STX_CDR(exns)) {
        a = SCHEME_STX_CAR(exns);
        name = scheme_tl_id_sym(genv, a, NULL, 0);
        if (!scheme_lookup_in_table(genv->toplevel, (const char *)name)
            && !scheme_lookup_in_table(genv->syntax, (const char *)name)) {
          scheme_wrong_syntax("module", a, ree_kw, "excluded identifier was not defined");
        }
      }

      for (adl = all_defs; SCHEME_PAIRP(adl); adl = SCHEME_CDR(adl)) {
        name = SCHEME_CAR(adl);
        exname = SCHEME_STX_SYM(name);
        name = scheme_tl_id_sym(genv, name, NULL, 0);
	
        /* Was this one excluded? */
        for (exns = exl; !SCHEME_STX_NULLP(exns); exns = SCHEME_STX_CDR(exns)) {
          a = SCHEME_STX_CAR(exns);
          a = scheme_tl_id_sym(genv, a, NULL, 0);
          if (SAME_OBJ(a, name))
            break;
        }

        if (SCHEME_STX_NULLP(exns)) {
          /* not excluded */
	  
          /* But don't export uninterned: */
          if (!SCHEME_SYM_UNINTERNEDP(name)) {
            /* Also, check that ree_kw and the identifier have the same
               introduction (in case one or the other was introduced by
               a macro). We perform this check by getting exname's tl_id
               as if it had ree_kw's context, then comparing that result
               to the actual tl_id. */
            a = scheme_datum_to_syntax(exname, scheme_false, ree_kw, 0, 0);
            a = scheme_tl_id_sym(genv, a, NULL, 0);
	    
            if (SAME_OBJ(a, name)) {
              /* Add prefix, if any */
              if (SCHEME_TRUEP(pfx)) {
                exname = scheme_symbol_append(pfx, exname);
              }
              if (scheme_hash_get(provided, exname))
                scheme_wrong_syntax("module", exname, ree_kw, "identifier already provided");
	      
              scheme_hash_set(provided, exname, 
                              scheme_make_pair(name, protected ? scheme_true : scheme_false));
            }
          }
        }
      }
    }
  }

  return reprovide_kernel;
}

char *compute_provide_arrays(Scheme_Hash_Table *provided, Scheme_Hash_Table *required,
                             Scheme_Module_Phase_Exports *pt,
                             Scheme_Env *genv, int def_phase,
                             int reprovide_kernel,
                             Scheme_Object *form,
                             const char *def_way)
{
  int i, count;
  Scheme_Object **exs, **exsns, **exss;
  char *exps, *exets;
  int excount, exvcount;
    
  for (count = 0, i = provided->size; i--; ) {
    if (provided->vals[i])
      count++;
  }
    
  count -= reprovide_kernel;

  exs = MALLOC_N(Scheme_Object *, count);
  exsns = MALLOC_N(Scheme_Object *, count);
  exss = MALLOC_N(Scheme_Object *, count);
  exps = MALLOC_N_ATOMIC(char, count);
  if (def_phase) {
    exets = MALLOC_N_ATOMIC(char, count);
    memset(exets, 0, count);
  } else
    exets = NULL;

  /* Do non-syntax first. */
  for (count = 0, i = provided->size; i--; ) {
    if (provided->vals[i]) {
      Scheme_Object *name, *prnt_name, *v;
      int protected;
	
      v = provided->vals[i];
      name = SCHEME_CAR(v);
      protected = SCHEME_TRUEP(SCHEME_CDR(v));

      prnt_name = name;
      if (SCHEME_STXP(name)) {
        if (genv)
          name = scheme_tl_id_sym(genv, name, NULL, 0);
        else
          name = SCHEME_STX_VAL(name); /* shouldn't get here; no `define-for-label' */
      }

      if (genv && scheme_lookup_in_table(genv->toplevel, (const char *)name)) {
        /* Defined locally */
        exs[count] = provided->keys[i];
        exsns[count] = name;
        exss[count] = scheme_false; /* means "self" */
        exps[count] = protected;
        if (exets)
          exets[count] = def_phase;
        count++;
      } else if (genv && scheme_lookup_in_table(genv->syntax, (const char *)name)) {
        /* Skip for now. */
      } else if ((v = scheme_hash_get(required, name))) {
        /* Required */
        if (protected) {
          name = SCHEME_CAR(provided->vals[i]);
          scheme_wrong_syntax("module", NULL, name, "cannot protect imported identifier with re-provide"); 
        }
        if (SCHEME_TRUEP(SCHEME_VEC_ELS(v)[3])) {
          /* If this is a kernel re-provide, don't provide after all. */
          if (reprovide_kernel
              && SAME_OBJ(SCHEME_VEC_ELS(v)[1], kernel_symbol)
              && SAME_OBJ(provided->keys[i], SCHEME_VEC_ELS(v)[2])) {
            /* skip */
          } else {
            exs[count] = provided->keys[i];
            exsns[count] = SCHEME_VEC_ELS(v)[2];
            exss[count] = SCHEME_VEC_ELS(v)[1];
            exps[count] = protected;
            count++;
          }
        }
      } else {
        /* Not defined! */
        scheme_wrong_syntax("module", prnt_name, form, def_way);
      }
    }
  }

  exvcount = count;

  for (i = provided->size; i--; ) {
    if (provided->vals[i]) {
      Scheme_Object *name, *v;
      int protected;
	  
      v = provided->vals[i];
      name = SCHEME_CAR(v);
      protected = SCHEME_TRUEP(SCHEME_CDR(v));

      if (SCHEME_STXP(name)) {
        if (genv)
          name = scheme_tl_id_sym(genv, name, NULL, 0);
        else {
          name = SCHEME_STX_VAL(name); /* shouldn't get here; no `define-for-label' */
        }
      }

      if (genv && scheme_lookup_in_table(genv->syntax, (const char *)name)) {
        /* Defined locally */
        exs[count] = provided->keys[i];
        exsns[count] = name;
        exss[count] = scheme_false; /* means "self" */
        exps[count] = protected;
        if (exets)
          exets[count] = def_phase;
        count++;
      } else if ((v = scheme_hash_get(required, name))) {
        /* Required */
        if (SCHEME_FALSEP(SCHEME_VEC_ELS(v)[3])) {
          /* If this is a kernel re-provide, don't provide after all. */
          if (reprovide_kernel
              && SAME_OBJ(SCHEME_VEC_ELS(v)[1], kernel_symbol)
              && SAME_OBJ(provided->keys[i], SCHEME_VEC_ELS(v)[2])) {
            /* skip */
          } else {
            exs[count] = provided->keys[i];
            exsns[count] = SCHEME_VEC_ELS(v)[2];
            exss[count] = SCHEME_VEC_ELS(v)[1];
            exps[count] = protected;
            count++;
          }
        }
      }
    }
  }

  excount = count;

  /* Sort provide array for variables: interned followed by
     uninterned, alphabetical within each. This is important for
     having a consistent provide arrays. */
  qsort_provides(exs, exsns, exss, exps, exets, 0, exvcount, 1);

  pt->num_provides = excount;
  pt->num_var_provides = exvcount;
  pt->provides = exs;
  pt->provide_src_names = exsns;
  pt->provide_srcs = exss;
  if (exets) {
    for (i = 0; i < excount; i++) {
      if (exets[i])
        break;
    }
    if (i >= excount)
      exets = NULL;
  }
  pt->provide_src_phases = exets;

  return exps;
}

/* Helper: */
static void qsort_provides(Scheme_Object **exs, Scheme_Object **exsns, Scheme_Object **exss, char *exps, char *exets,
			   int start, int count, int do_uninterned)
{
  int i, j;
  Scheme_Object *tmp_ex, *tmp_exsn, *tmp_exs, *pivot;
  char tmp_exp, tmp_exet;

  if (do_uninterned) {
    /* Look for uninterned and move to end: */

    for (j = count; j--; ) {
      if (!SCHEME_SYM_WEIRDP(exs[j]))
	break;
    }

    for (i = start; i < j; i++) {
      if (SCHEME_SYM_WEIRDP(exs[i])) {
	tmp_ex = exs[i];
	exs[i] = exs[j];
	exs[j] = tmp_ex;

	if (exsns) {
	  tmp_exsn = exsns[i];
	  tmp_exs = exss[i];
	  tmp_exp = exps[i];

	  exsns[i] = exsns[j];
	  exss[i] = exss[j];
	  exps[i] = exps[j];

	  exsns[j] = tmp_exsn;
	  exss[j] = tmp_exs;
	  exps[j] = tmp_exp;
	}
        if (exets) {
          tmp_exet = exets[i];
          exets[i] = exets[j];
          exets[j] = tmp_exet;
        }

	j--;
	/* Skip over uninterns already at the end: */
	while (j) {
	  if (!SCHEME_SYM_WEIRDP(exs[j]))
	    break;
	  else
	    j--;
	}
      }
    }

    /* Sort interned and uninterned separately: */
    qsort_provides(exs, exsns, exss, exps, exets, 0, j + 1, 0);
    qsort_provides(exs, exsns, exss, exps, exets, j + 1, count - j - 1, 0);
  } else {
    j = start;
    while (count > 1) {
      j = start;
      pivot = exs[j];
      
      for (i = 1; i < count; i++) {
	int k = i + start;
	if (strcmp(SCHEME_SYM_VAL(exs[k]), SCHEME_SYM_VAL(pivot)) < 0) {
	  tmp_ex = exs[k];
	  exs[k] = exs[j];
	  exs[j] = tmp_ex;
	  
	  if (exsns) {
	    tmp_exsn = exsns[k];
	    tmp_exs = exss[k];
	    tmp_exp = exps[k];
	    
	    exsns[k] = exsns[j];
	    exss[k] = exss[j];
	    exps[k] = exps[j];
	    
	    exsns[j] = tmp_exsn;
	    exss[j] = tmp_exs;
	    exps[j] = tmp_exp;
	  }
          if (exets) {
            tmp_exet = exets[k];
            exets[k] = exets[j];
            exets[j] = tmp_exet;
          }
	  
	  j++;
	}
      }

      if (j == start) {
	start++;
	--count;
      } else
	break;
    }

    if (count > 1) {
      qsort_provides(exs, exsns, exss, exps, exets, start, j - start, 0);
      qsort_provides(exs, exsns, exss, exps, exets, j, count - (j - start), 0);
    }
  }
}

Scheme_Object *parse_provides(Scheme_Object *form, Scheme_Object *fst, Scheme_Object *e, 
                              Scheme_Hash_Table *provided, Scheme_Object *reprovided,
                              Scheme_Object *self_modidx, Scheme_Object **_all_defs_out)
{
  Scheme_Object *l;
  int protect_cnt = 0;
  Scheme_Object *all_defs_out  = (_all_defs_out ? *_all_defs_out : NULL);

  if (scheme_stx_proper_list_length(e) < 0)
    scheme_wrong_syntax(NULL, e, form, "bad syntax (" IMPROPER_LIST_FORM ")");
  
  for (l = SCHEME_STX_CDR(e); !SCHEME_STX_NULLP(l); l = SCHEME_STX_CDR(l)) {
    Scheme_Object *a, *midx, *name;

    a = SCHEME_STX_CAR(l);

    if (SCHEME_STX_PAIRP(a) && (scheme_stx_proper_list_length(a) > 0)) {
      fst = SCHEME_STX_CAR(a);
      if (SCHEME_STX_SYMBOLP(fst)
          && (SAME_OBJ(protect_symbol, SCHEME_STX_VAL(fst)))) {
        if (protect_cnt)
          scheme_wrong_syntax(NULL, a, e, "bad syntax (nested protect)");
        a = SCHEME_STX_CDR(a);
        a = scheme_flatten_syntax_list(a, NULL);
        l = SCHEME_STX_CDR(l);
        l = scheme_append(a, l);
        protect_cnt = scheme_list_length(a);

        /* In case a provide ends with an empty protect: */
        if (SCHEME_STX_NULLP(l))
          break;

        a = SCHEME_STX_CAR(l);
      }
    }

    if (SCHEME_STX_SYMBOLP(a)) {
      /* <id> */
      name = SCHEME_STX_VAL(a);
      if (scheme_hash_get(provided, name))
        scheme_wrong_syntax("module", a, form, "identifier already provided");
      /* Provide a: */
      scheme_hash_set(provided, name, scheme_make_pair(a, protect_cnt ? scheme_true : scheme_false));
    } else if (SCHEME_STX_PAIRP(a)) {
      Scheme_Object *rest;

      fst = SCHEME_STX_CAR(a);
      rest = SCHEME_STX_CDR(a);

      if (SAME_OBJ(rename_symbol, SCHEME_STX_VAL(fst))) {
        /* (rename <id> <id>) */
        Scheme_Object *inm, *enm;

        if (!SCHEME_STX_PAIRP(rest)
            || !SCHEME_STX_PAIRP(SCHEME_STX_CDR(rest)))
          scheme_wrong_syntax(NULL, a, e, "bad syntax");
        inm = SCHEME_STX_CAR(rest);
        rest = SCHEME_STX_CDR(rest);
        enm = SCHEME_STX_CAR(rest);
        if (!SCHEME_STX_SYMBOLP(inm))
          scheme_wrong_syntax(NULL, a, e, "bad syntax (internal name is not an identifier)");
        if (!SCHEME_STX_SYMBOLP(enm))
          scheme_wrong_syntax(NULL, a, e, "bad syntax (external name is not an identifier)");
        rest = SCHEME_STX_CDR(rest);
        if (!SCHEME_STX_NULLP(rest))
          scheme_wrong_syntax(NULL, a, e, "bad syntax (data following external name)");
		
        enm = SCHEME_STX_VAL(enm);
		
        if (scheme_hash_get(provided, enm))
          scheme_wrong_syntax("module", enm, a, "identifier already provided");
        /* Provide enm: */
        scheme_hash_set(provided, enm, scheme_make_pair(inm, protect_cnt ? scheme_true : scheme_false));
      } else if (SAME_OBJ(all_from_symbol, SCHEME_STX_VAL(fst))) {
        /* (all-from <modname>) */
        if (protect_cnt)
          scheme_wrong_syntax(NULL, a, e, "bad syntax (not allowed as protected)");
        if (!SCHEME_STX_PAIRP(rest))
          scheme_wrong_syntax(NULL, a, e, "bad syntax");
        if (!SCHEME_STX_NULLP(SCHEME_STX_CDR(rest)))
          scheme_wrong_syntax(NULL, a, e, "bad syntax (data following `all-from')");
		
        midx = SCHEME_STX_CAR(rest);
        midx = scheme_make_modidx(scheme_syntax_to_datum(midx, 0, NULL),
                                  self_modidx,
                                  scheme_false);
		
        reprovided = scheme_make_pair(scheme_make_pair(midx, scheme_make_pair(e, scheme_null)), 
                                      reprovided);
      } else if (SAME_OBJ(all_from_except_symbol, SCHEME_STX_VAL(fst))) {
        /* (all-from-except <modname> <id> ...) */
        Scheme_Object *exns, *el, *p;
        int len;
		
        if (protect_cnt)
          scheme_wrong_syntax(NULL, a, e, "bad syntax (not allowed as protected)");
		
        len = scheme_stx_proper_list_length(a);

        if (len < 0)
          scheme_wrong_syntax(NULL, a, e, "bad syntax (" IMPROPER_LIST_FORM ")");
        else if (len == 1)
          scheme_wrong_syntax(NULL, a, e, "bad syntax (missing module name)");
		
        midx = SCHEME_STX_CAR(rest);
        midx = scheme_make_modidx(scheme_syntax_to_datum(midx, 0, NULL),
                                  self_modidx,
                                  scheme_false);
        exns = SCHEME_STX_CDR(rest);
		
        /* Check all exclusions are identifiers: */
        for (el = exns; SCHEME_STX_PAIRP(el); el = SCHEME_STX_CDR(el)) {
          p = SCHEME_STX_CAR(el);
          if (!SCHEME_STX_SYMBOLP(p)) {
            scheme_wrong_syntax(NULL, p, e,
                                "bad syntax (excluded name is not an identifier)");
          }
        }
		
        reprovided = scheme_make_pair(scheme_make_pair(midx, scheme_make_pair(e, exns)), 
                                      reprovided);
      } else if (SAME_OBJ(struct_symbol, SCHEME_STX_VAL(fst))) {
        /* (struct <id> (<id> ...)) */
        int len, i;
        Scheme_Object *prnt_base, *base, *fields, *el, **names, *p;
		
        len = scheme_stx_proper_list_length(rest);
        if (len != 2) {
          if (len < 0)
            scheme_wrong_syntax(NULL, a, e, "bad syntax (" IMPROPER_LIST_FORM ")");
          else
            scheme_wrong_syntax(NULL, a, e, "bad syntax "
                                "(not a struct identifier followed by "
                                "a sequence of field identifiers)");
        }

        base = SCHEME_STX_CAR(rest);
        fields = SCHEME_STX_CDR(rest);
        fields = SCHEME_STX_CAR(fields);
		
        if (!SCHEME_STX_SYMBOLP(base))
          scheme_wrong_syntax(NULL, base, e,
                              "bad syntax (struct name is not an identifier)");

        /* Check all field names are identifiers: */
        for (el = fields; SCHEME_STX_PAIRP(el); el = SCHEME_STX_CDR(el)) {
          p = SCHEME_STX_CAR(el);
          if (!SCHEME_STX_SYMBOLP(p)) {
            scheme_wrong_syntax(NULL, p, e,
                                "bad syntax (field name is not an identifier)");
          }
        }
        if (!SCHEME_STX_NULLP(el))
          scheme_wrong_syntax(NULL, fields, e,
                              "bad syntax (" IMPROPER_LIST_FORM ")");
		
        prnt_base = base;
        base = SCHEME_STX_VAL(base);
        fields = scheme_syntax_to_datum(fields, 0, NULL);

        names = scheme_make_struct_names(base, fields, SCHEME_STRUCT_EXPTIME, &len);

        for (i = 0; i < len; i++) {
          if (scheme_hash_get(provided, names[i]))
            scheme_wrong_syntax("module", names[i], e, "identifier already provided");
          /* Wrap local name with prnt_base in case there are marks that 
             trigger "gensym"ing */
          scheme_hash_set(provided, names[i], 
                          scheme_make_pair(scheme_datum_to_syntax(names[i], scheme_false, prnt_base, 0, 0),
                                           protect_cnt ? scheme_true : scheme_false));
        }
      } else if (SAME_OBJ(all_defined_symbol, SCHEME_STX_VAL(fst))) {
        /* (all-defined) */
        if (!SCHEME_STX_NULLP(rest))
          scheme_wrong_syntax(NULL, a, e, "bad syntax");
		
        all_defs_out = scheme_make_pair(scheme_make_pair(scheme_make_pair(e, 
                                                                          scheme_make_pair(scheme_null, 
                                                                                           scheme_false)),
                                                         protect_cnt ? scheme_true : scheme_false),
                                        all_defs_out);
      } else if (SAME_OBJ(prefix_all_defined_symbol, SCHEME_STX_VAL(fst))) {
        /* (prefix-all-defined <prefix>) */
        Scheme_Object *prefix;

        if (!SCHEME_STX_PAIRP(rest))
          scheme_wrong_syntax(NULL, a, e, "bad syntax");
        prefix = SCHEME_STX_CAR(rest);
        rest = SCHEME_STX_CDR(rest);
        if (!SCHEME_STX_NULLP(rest))
          scheme_wrong_syntax(NULL, a, e, "bad syntax");
		
        if (!SCHEME_STX_SYMBOLP(prefix)) {
          scheme_wrong_syntax(NULL, a, e,
                              "bad syntax (prefix is not an identifier)");
        }
        prefix = SCHEME_STX_VAL(prefix);

        all_defs_out = scheme_make_pair(scheme_make_pair(scheme_make_pair(e, 
                                                                          scheme_make_pair(scheme_null, 
                                                                                           prefix)),
                                                         protect_cnt ? scheme_true : scheme_false),
                                        all_defs_out);
      } else if (SAME_OBJ(all_defined_except_symbol, SCHEME_STX_VAL(fst))
                 || SAME_OBJ(prefix_all_defined_except_symbol, SCHEME_STX_VAL(fst))) {
        /* ([prefix-]all-defined-except <id> ...) */
        Scheme_Object *exns, *el, *prefix = scheme_false, *p;
        int len, is_prefix;
		
        is_prefix = SAME_OBJ(prefix_all_defined_except_symbol, SCHEME_STX_VAL(fst));

        len = scheme_stx_proper_list_length(a);

        if (len < 0)
          scheme_wrong_syntax(NULL, a, e, "bad syntax (" IMPROPER_LIST_FORM ")");
		
        if (is_prefix && (len < 2))
          scheme_wrong_syntax(NULL, a, e, "bad syntax (missing prefix)");

        if (is_prefix) {
          prefix = SCHEME_STX_CAR(rest);
          if (!SCHEME_STX_SYMBOLP(prefix))
            scheme_wrong_syntax(NULL, a, e, "bad syntax (prefix is not an identifier)");
          prefix = SCHEME_STX_VAL(prefix);
          rest = SCHEME_STX_CDR(rest);
        }

        exns = rest;
		
        /* Check all exclusions are identifiers: */
        for (el = exns; SCHEME_STX_PAIRP(el); el = SCHEME_STX_CDR(el)) {
          p = SCHEME_STX_CAR(el);
          if (!SCHEME_STX_SYMBOLP(p)) {
            scheme_wrong_syntax(NULL, p, e,
                                "bad syntax (excluded name is not an identifier)");
          }
        }
		
        all_defs_out = scheme_make_pair(scheme_make_pair(scheme_make_pair(e, 
                                                                          scheme_make_pair(exns,
                                                                                           prefix)),
                                                         protect_cnt ? scheme_true : scheme_false),
                                        all_defs_out);
      } else {
        scheme_wrong_syntax(NULL, a, e, NULL);
      }
    } else {
      scheme_wrong_syntax(NULL, a, e, NULL);
    }

    if (protect_cnt)
      --protect_cnt;
  }

  if (_all_defs_out)
    *_all_defs_out = all_defs_out;

  return reprovided;
}

/**********************************************************************/
/*                         top-level require                          */
/**********************************************************************/

void add_single_require(Scheme_Module_Exports *me, /* from module */
                        int base_k, /* [0, 1, 2] => start with [rt, et, dt] */
			Scheme_Object *idx, /* from module's idx; may be saved for unmarshalling */
			Scheme_Env *orig_env, /* env for mark_src or copy_vars */
			Scheme_Object *rt_rn, /* add requires to this rename when no mark_src */
			Scheme_Object *post_ex_rt_rn, /* add requires to this rename when mark_src */
			Scheme_Object *et_rn, Scheme_Object *post_ex_et_rn,
			Scheme_Object *dt_rn, Scheme_Object *post_ex_dt_rn,
			Scheme_Object *exns, /* NULL or [syntax] list of [syntax] symbols not to import */
			Scheme_Hash_Table *onlys, /* NULL or hash table of names to import; the hash table is mutated */
			Scheme_Object *prefix, /* NULL or prefix symbol */
			Scheme_Object *iname, /* NULL or symbol for a single import */
			Scheme_Object *orig_ename, /* NULL or symbol for a single import */
			Scheme_Object *mark_src, /* default mark_src; if onlys, each is also mark_src */
			int unpack_kern, int copy_vars, int for_unmarshal,
			int can_save_marshal,
			int *all_simple,
			Check_Func ck, /* NULL or called for each addition */
			void *rt_data, void *et_data, void *dt_data, 
                        Scheme_Object *form, Scheme_Object *err_src, Scheme_Object *cki /* ck args */
			)
{
  int j, var_count;
  Scheme_Object *orig_idx = idx;
  Scheme_Object **exs, **exsns, **exss;
  char *exets;
  void *data;
  int is_kern, has_context, save_marshal_info = 0;
  Scheme_Object *nominal_modidx, *one_exn, *prnt_iname, *name, *rn, *post_ex_rn, *ename = orig_ename;
  Scheme_Hash_Table *orig_onlys;
  Scheme_Env *env;
  int k;
  
  if (mark_src) {
    /* Check whether there's context for this import (which
       leads to generated local names). */
    Scheme_Object *l;
    l = scheme_stx_extract_marks(mark_src);
    has_context = !SCHEME_NULLP(l);
    if (has_context) {
      if (all_simple)
	*all_simple = 0;
    }
  } else
    has_context = 0; /* computed later */

  if (iname || ename || onlys || for_unmarshal || unpack_kern || has_context)
    can_save_marshal = 0;
  
  if (onlys)
    orig_onlys = scheme_clone_hash_table(onlys);
  else
    orig_onlys = NULL;
    
  for (k = 0; k < (et_rn ? 3 : 1); k++) {
    Scheme_Module_Phase_Exports *pt;

    switch(k) {
    case 0:
      switch(base_k) {
      case 0:
        pt = me->rt;
        break;
      case 1:
        pt = me->et;
        break;
      case 2:
      default:
        pt = me->dt;
        break;
      }
      env = orig_env;
      rn = rt_rn;
      post_ex_rn = post_ex_rt_rn;
      data = rt_data;
      break;
    case 1:
      pt = me->et;
      env = orig_env->exp_env;
      rn = et_rn;
      post_ex_rn = post_ex_et_rn;
      data = et_data;
      break;
    case 2:
    default:
      pt = me->dt;
      env = orig_env; /* using run-time rename is consistent with dt_mn_ht = mn_ht */
      rn = dt_rn;
      post_ex_rn = post_ex_dt_rn;
      data = dt_data;
      break;
    }

    is_kern = (SAME_OBJ(idx, kernel_symbol)
               && !exns
               && !onlys
               && !prefix
               && !iname
               && !unpack_kern
               && !has_context);
    
    one_exn = NULL;
    
    nominal_modidx = idx;

    while (1) { /* loop to handle kernel re-provides... */
      int break_if_iname_null = !!iname;

      exs = pt->provides;
      exsns = pt->provide_src_names;
      exss = pt->provide_srcs;
      exets = pt->provide_src_phases;
      var_count = pt->num_var_provides;

      for (j = pt->num_provides; j--; ) {
        Scheme_Object *modidx;
	
        if (orig_ename) {
          if (!SAME_OBJ(SCHEME_STX_VAL(orig_ename), exs[j]))
            continue;  /* we don't want this one. */
        } else if (onlys) {
          name = scheme_hash_get(orig_onlys, exs[j]);
          if (!name)
            continue;  /* we don't want this one. */
          mark_src = name;
          {
            Scheme_Object *l;
            l = scheme_stx_extract_marks(mark_src);
            has_context = !SCHEME_NULLP(l);
          }
          /* Remove to indicate that it's been imported: */
          scheme_hash_set(onlys, exs[j], NULL);
        } else {
          if (exns) {
            Scheme_Object *l, *a;
            for (l = exns; SCHEME_STX_PAIRP(l); l = SCHEME_STX_CDR(l)) {
              a = SCHEME_STX_CAR(l);
              if (SCHEME_STXP(a)) 
                a = SCHEME_STX_VAL(a);
              if (SAME_OBJ(a, exs[j]))
                break;
            }
            if (!SCHEME_STX_NULLP(l))
              continue; /* we don't want this one. */
          }

          if (one_exn) {
            if (SAME_OBJ(one_exn, exs[j]))
              continue; /* we don't want this one. */
          }
        }
	
        modidx = ((exss && !SCHEME_FALSEP(exss[j])) 
                  ? scheme_modidx_shift(exss[j], me->src_modidx, idx)
                  : idx);
      
        if (!iname)
          iname = exs[j];

        if (SCHEME_SYM_WEIRDP(iname)) {
          /* This shouldn't happen. In case it does, don't import a
             gensym or parallel symbol. The former is useless. The
             latter is supposed to be module-specific, and it could
             collide with local module-specific ids. */
          iname = NULL;
          continue;
        }

        if (prefix)
          iname = scheme_symbol_append(prefix, iname);

        prnt_iname = iname;
        if (has_context) {
          /* The `require' expression has a set of marks in its
             context, which means that we need to generate a name. */
          iname = scheme_datum_to_syntax(iname, scheme_false, mark_src, 0, 0);
          iname = scheme_tl_id_sym(env, iname, scheme_false, 2);
        }

        if (ck)
          ck(prnt_iname, iname, nominal_modidx, modidx, exsns[j], (j < var_count), data, cki, form, err_src);
	
        if (!is_kern) {
          if (copy_vars && (j < var_count) && !env->module && !env->phase && !k) {
            Scheme_Env *menv;
            Scheme_Object *val;
            modidx = scheme_module_resolve(modidx, 1);
            menv = scheme_module_access(modidx, env, 0);
            val = scheme_lookup_in_table(menv->toplevel, (char *)exsns[j]);
            scheme_add_global_symbol(iname, val, env);
            scheme_shadow(env, iname, 1);
          } else if (!for_unmarshal || !has_context) {
            if (!save_marshal_info && !has_context && can_save_marshal)
              save_marshal_info = 1;
            scheme_extend_module_rename((has_context ? post_ex_rn : rn), 
                                        modidx, iname, exsns[j], nominal_modidx, exs[j], 
                                        exets ? exets[j] : 0,
                                        for_unmarshal || (!has_context && can_save_marshal));
          }
        }

        iname = NULL;
	
        if (ename) {
          ename = NULL;
          break;
        }
      }

      if (is_kern)
        scheme_extend_module_rename_with_kernel(rn, nominal_modidx);

      if (break_if_iname_null && !iname)
        break;

      if (pt->reprovide_kernel) {
        idx = kernel_symbol;
        one_exn = pt->kernel_exclusion;
        me = kernel->me;
        pt = kernel->me->rt;
        is_kern = !prefix && !unpack_kern && !ename && !has_context && !onlys;
      } else
        break;
    }

    if (save_marshal_info) {
      Scheme_Object *info, *a;

      if (exns) {
        /* Convert to a list of symbols: */
        info = scheme_null;
        for (; SCHEME_STX_PAIRP(exns); exns = SCHEME_STX_CDR(exns)) {
          a = SCHEME_STX_CAR(exns);
          if (SCHEME_STXP(a))
            a = SCHEME_STX_VAL(a);
          info = cons(a, info);
        }
        exns = info;
      } else
        exns = scheme_null;

      /* The format of this data is checked in stxobj for unmarshaling
         a Module_Renames. Also the idx must be first, to support shifting. */
      info = cons(orig_idx, cons(scheme_make_integer(k+base_k),
                                 cons(exns, prefix ? prefix : scheme_false)));

      scheme_save_module_rename_unmarshal(rn, info);

      save_marshal_info = 0;
    }
  }

  if (ename) {
    scheme_wrong_syntax(NULL, ename, form, "no such provided variable");
    return;
  }
}

void scheme_do_module_rename_unmarshal(Scheme_Object *rn, Scheme_Object *info,
				       Scheme_Object *modidx_shift_from, Scheme_Object *modidx_shift_to,
				       Scheme_Hash_Table *export_registry)
{
  Scheme_Object *orig_idx, *exns, *prefix, *idx, *name, *kv;
  Scheme_Module_Exports *me;
  Scheme_Env *env;

  idx = SCHEME_CAR(info);
  orig_idx = idx;
  info = SCHEME_CDR(info);
  kv = SCHEME_CAR(info);
  info = SCHEME_CDR(info);
  exns = SCHEME_CAR(info);
  prefix = SCHEME_CDR(info);

  if (SCHEME_FALSEP(prefix))
    prefix = NULL;
  if (SCHEME_NULLP(exns))
    exns = NULL;

  if (modidx_shift_from)
    idx = scheme_modidx_shift(idx,
			      modidx_shift_from,
			      modidx_shift_to);

  name = scheme_module_resolve(idx, 0);

  if (SAME_OBJ(kernel_symbol, name)) {
    me = kernel->me;
  } else {
    if (!export_registry) {
      env = scheme_get_env(scheme_current_config());
      export_registry = env->export_registry;
    }

    me = (Scheme_Module_Exports *)scheme_hash_get(export_registry, name);
    if (!me) {
      scheme_signal_error("compiled/expanded code out of context;"
			  " cannot find exports to restore imported renamings"
			  " for module: %s",
			  scheme_symbol_name(name));
      return;
    }
  }

  add_single_require(me, SCHEME_INT_VAL(kv), orig_idx, NULL,
		     rn, NULL, 
                     NULL, NULL,
                     NULL, NULL,
		     exns, NULL, prefix, NULL, NULL,
		     NULL,
		     0, 0, 1, 0,
		     NULL,
		     NULL, 
                     NULL, NULL, NULL, 
                     NULL, NULL, NULL);
}

Scheme_Object *parse_requires(Scheme_Object *form, 
			      Scheme_Object *base_modidx,
			      Scheme_Env *env,
			      Scheme_Object *rn, Scheme_Object *post_ex_rn,
			      Scheme_Object *et_rn, Scheme_Object *post_ex_et_rn,
			      Scheme_Object *dt_rn, Scheme_Object *post_ex_dt_rn,
			      Check_Func ck, void *data, void *et_data, void *dt_data,
			      int start, int expstart, Scheme_Object *redef_modname,
			      int unpack_kern, int copy_vars, int can_save_marshal,
			      int *all_simple) 
{
  Scheme_Object *ll = form;
  Scheme_Module *m;
  Scheme_Object *idxstx, *idx, *name, *i, *exns, *prefix, *iname, *ename, *aa;
  Scheme_Object *imods, *mark_src, *err_src;
  Scheme_Hash_Table *onlys;

  imods = scheme_null;

  if (scheme_stx_proper_list_length(form) < 0)
    scheme_wrong_syntax(NULL, NULL, form, "bad syntax (" IMPROPER_LIST_FORM ")");
  
  
  for (ll = SCHEME_STX_CDR(ll); !SCHEME_STX_NULLP(ll); ll = SCHEME_STX_CDR(ll)) {
    i = SCHEME_STX_CAR(ll);
    iname = ename = NULL;
    onlys = NULL;
    if (SCHEME_STX_PAIRP(i))
      aa = SCHEME_STX_CAR(i);
    else
      aa = NULL;

    err_src = i;
    mark_src = i;

    if (aa && SAME_OBJ(prefix_symbol, SCHEME_STX_VAL(aa))) {
      /* prefix */
      int len;

      if (all_simple)
	*all_simple = 0;

      len = scheme_stx_proper_list_length(i);
      if (len != 3) {
	GC_CAN_IGNORE const char *reason;
	
	if (len < 0)
	  reason = "bad syntax (" IMPROPER_LIST_FORM ")";
	else if (len < 2)
	  reason = "bad syntax (prefix missing)";
	else if (len < 3)
	  reason = "bad syntax (module name missing)";
	else
	  reason = "bad syntax (extra data after module name)";
	scheme_wrong_syntax(NULL, i, form, reason);
	return NULL;
      }

      i = SCHEME_STX_CDR(i);
      prefix = SCHEME_STX_CAR(i);
      i = SCHEME_STX_CDR(i);
      idxstx = SCHEME_STX_CAR(i);
      exns = NULL;

      if (!SCHEME_SYMBOLP(SCHEME_STX_VAL(prefix))) {
	scheme_wrong_syntax(NULL, prefix, form, "bad prefix (not an identifier)");
	return NULL;
      }

      prefix = SCHEME_STX_VAL(prefix);

    } else if (aa && (SAME_OBJ(all_except_symbol, SCHEME_STX_VAL(aa))
		      || SAME_OBJ(prefix_all_except_symbol, SCHEME_STX_VAL(aa)))) {
      /* all-except and prefix-all-except */
      Scheme_Object *l;
      int len;
      int has_prefix;

      if (all_simple)
	*all_simple = 0;

      has_prefix = SAME_OBJ(prefix_all_except_symbol, SCHEME_STX_VAL(aa));

      len = scheme_stx_proper_list_length(i);
      if (len < 0)
	scheme_wrong_syntax(NULL, i, form, "bad syntax (" IMPROPER_LIST_FORM ")");
      else if (has_prefix && (len < 2))
	scheme_wrong_syntax(NULL, i, form, "bad syntax (prefix missing)");
      else if (len < (has_prefix ? 3 : 2))
	scheme_wrong_syntax(NULL, i, form, "bad syntax (module name missing)");

      idxstx = SCHEME_STX_CDR(i);
      if (has_prefix) {
	prefix = SCHEME_STX_CAR(idxstx);
	idxstx = SCHEME_STX_CDR(idxstx);

	if (!SCHEME_SYMBOLP(SCHEME_STX_VAL(prefix))) {
	  scheme_wrong_syntax(NULL, prefix, form, "bad prefix (not an identifier)");
	  return NULL;
	}
	prefix = SCHEME_STX_VAL(prefix);
      } else
	prefix = NULL;
      exns = SCHEME_STX_CDR(idxstx);
      idxstx = SCHEME_STX_CAR(idxstx);

      for (l = exns; SCHEME_STX_PAIRP(l); l = SCHEME_STX_CDR(l)) {
	if (!SCHEME_STX_SYMBOLP(SCHEME_STX_CAR(l))) {
	  l = SCHEME_STX_CAR(l);
	  scheme_wrong_syntax(NULL, l, form,
			      "bad syntax (excluded name is not an identifier)");
	}
      }
    } else if (aa && SAME_OBJ(only_symbol, SCHEME_STX_VAL(aa))) {
      /* only */
      int len;
      Scheme_Object *rest, *nm;

      if (all_simple)
	*all_simple = 0;

      len = scheme_stx_proper_list_length(i);
      if (len < 2) {
	GC_CAN_IGNORE const char *reason;
	
	if (len < 0)
	  reason = "bad syntax (" IMPROPER_LIST_FORM ")";
	else
	  reason = "bad syntax (module name missing)";
	scheme_wrong_syntax(NULL, i, form, reason);
	return NULL;
      }

      onlys = scheme_make_hash_table(SCHEME_hash_ptr);

      rest = SCHEME_STX_CDR(i);
      idxstx = SCHEME_STX_CAR(rest);
      rest = SCHEME_STX_CDR(rest);
      while (SCHEME_STX_PAIRP(rest)) {
	nm = SCHEME_STX_CAR(rest);
	if (!SCHEME_STX_SYMBOLP(nm)) {
	  scheme_wrong_syntax(NULL, nm, form, "bad syntax (name for `only' is not an identifier)");
	}
	scheme_hash_set(onlys, SCHEME_STX_VAL(nm), nm);
	rest = SCHEME_STX_CDR(rest);
      }

      mark_src = NULL;
      exns = NULL;
      prefix = NULL;
    } else if (aa && SAME_OBJ(rename_symbol, SCHEME_STX_VAL(aa))) {
      /* rename */
      int len;
      Scheme_Object *rest;

      if (all_simple)
	*all_simple = 0;

      len = scheme_stx_proper_list_length(i);
      if (len != 4) {
	GC_CAN_IGNORE const char *reason;
	
	if (len < 0)
	  reason = "bad syntax (" IMPROPER_LIST_FORM ")";
	else if (len < 2)
	  reason = "bad syntax (module name missing)";
	else if (len < 3)
	  reason = "bad syntax (internal name missing)";
	else if (len < 4)
	  reason = "bad syntax (external name missing)";
	else
	  reason = "bad syntax (extra data after external name)";
	scheme_wrong_syntax(NULL, i, form, reason);
	return NULL;
      }

      rest = SCHEME_STX_CDR(i);
      idxstx = SCHEME_STX_CAR(rest);
      rest = SCHEME_STX_CDR(rest);
      iname = SCHEME_STX_CAR(rest);
      rest = SCHEME_STX_CDR(rest);
      ename = SCHEME_STX_CAR(rest);

      if (!SCHEME_STX_SYMBOLP(iname))
	scheme_wrong_syntax(NULL, i, form, "bad syntax (internal name is not an identifier)");
      if (!SCHEME_STX_SYMBOLP(ename))
	scheme_wrong_syntax(NULL, i, form, "bad syntax (external name is not an identifier)");

      mark_src = iname;

      iname = SCHEME_STX_VAL(iname);
      
      prefix = NULL;
      exns = NULL;
    } else {
      idxstx = i;
      exns = NULL;
      prefix = NULL;
    }

    idx = scheme_make_modidx(scheme_syntax_to_datum(idxstx, 0, NULL), 
			     base_modidx,
			     scheme_false);

    name = _module_resolve(idx, idxstx, 1);

    m = module_load(name, env, NULL);

    if (start)
      start_module(m, env, 0, idx, 0, 1, scheme_null);
    else if (expstart)
      expstart_module(m, env, 0, idx, 0, 0, scheme_null);

    /* Add name to require list, if it's not there: */
    {
      Scheme_Object *l, *last = NULL, *p;
      for (l = imods; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
	if (same_modidx(SCHEME_CAR(l), idx))
	  break;
	last = l;
      }
      if (SCHEME_NULLP(l)) {
	p = scheme_make_pair(idx, scheme_null);
	if (last)
	  SCHEME_CDR(last) = p;
	else
	  imods = p;
      }
    }
    
    add_single_require(m->me, 0, idx, env, 
                       rn, post_ex_rn,
                       et_rn, post_ex_et_rn,
                       dt_rn, post_ex_dt_rn,
		       exns, onlys, prefix, iname, ename,
		       mark_src, 
		       unpack_kern, copy_vars && start, 0, can_save_marshal,
		       all_simple,
		       ck, 
                       data, et_data, dt_data, 
                       form, err_src, i);

    if (onlys && onlys->count) {
      /* Something required in `only' wasn't provided by the module */
      int k;
      for (k = 0; k < onlys->size; k++) {
	if (onlys->vals[k])
	  scheme_wrong_syntax(NULL, onlys->vals[k], form, "no such provided variable");
      }
    }
  }

  return imods;
}

static void check_dup_require(Scheme_Object *prnt_name, Scheme_Object *name, Scheme_Object *nominal_modidx, 
			      Scheme_Object *modidx, Scheme_Object *srcname, 
			      int isval, void *ht, Scheme_Object *e, Scheme_Object *form, Scheme_Object *err_src)
{
  Scheme_Object *i;

  if (ht) {
    i = scheme_hash_get((Scheme_Hash_Table *)ht, name);

    if (i) {
      if (same_resolved_modidx(modidx, SCHEME_CAR(i)) && SAME_OBJ(srcname, SCHEME_CDR(i)))
	return; /* same source */
      scheme_wrong_syntax(NULL, prnt_name, form, "duplicate import identifier");
    } else
      scheme_hash_set((Scheme_Hash_Table *)ht, name, scheme_make_pair(modidx, srcname));
  }
}

static Scheme_Object *
top_level_require_execute(Scheme_Object *data)
{
  Scheme_Hash_Table *ht, *et_ht, *dt_ht;
  Scheme_Object *rn, *et_rn, *dt_rn, *modidx;
  Scheme_Object *form = SCHEME_CDDR(data), *rest, *brn;
  int for_phase = SCHEME_INT_VAL(SCHEME_CADR(data));
  Scheme_Env *env;

  env = scheme_environment_from_dummy(SCHEME_CAR(data));

  if (env->module)
    modidx = env->module->self_modidx;
  else
    modidx = scheme_false;

  if (for_phase == 1) {
    scheme_prepare_exp_env(env);
    env = env->exp_env;
  } else if (for_phase == -1) {
    scheme_prepare_template_env(env);
    env = env->template_env;
  }

  /* Don't check for dups if we import from less that two sources: */
  rest = SCHEME_STX_CDR(form);
  if (SCHEME_STX_NULLP(rest)) {
    rest = NULL;
  } else if (SCHEME_STX_PAIRP(rest)) {
    rest = SCHEME_STX_CDR(rest);
    if (SCHEME_STX_NULLP(rest)) {
      rest = NULL;
    }
  }

  if (rest)
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
  else
    ht = NULL;

  rn = scheme_make_module_rename(for_phase, mzMOD_RENAME_TOPLEVEL, NULL);
  if (!for_phase) {
    scheme_prepare_exp_env(env);
    et_rn = scheme_make_module_rename(1, mzMOD_RENAME_TOPLEVEL, NULL);
    dt_rn = scheme_make_module_rename(MZ_LABEL_PHASE, mzMOD_RENAME_TOPLEVEL, NULL);
    if (rest) {
      et_ht = scheme_make_hash_table(SCHEME_hash_ptr);
      dt_ht = scheme_make_hash_table(SCHEME_hash_ptr);
    } else {
      et_ht = NULL;
      dt_ht = NULL;
    }
  } else {
    et_rn = NULL;
    dt_rn = NULL;
    et_ht = NULL;
    dt_ht = NULL;
  }

  (void)parse_requires(form, modidx, env, 
                       rn, rn,
                       et_rn, et_rn,
                       dt_rn, dt_rn,
		       check_dup_require, ht, et_ht, dt_ht, (for_phase > -1), (for_phase == 0), NULL,
		       !env->module, 0, 0, NULL);

  brn = ((for_phase == MZ_LABEL_PHASE) ? env->dt_rename : env->rename);
  if (!brn) {
    brn = scheme_make_module_rename(for_phase, mzMOD_RENAME_TOPLEVEL, NULL);
    if (for_phase == MZ_LABEL_PHASE)
      env->dt_rename = brn;
    else
      env->rename = brn;
  }

  scheme_append_module_rename(rn, brn);
  
  if (!for_phase) {
    brn = env->exp_env->rename;
    if (!brn) {
      brn = scheme_make_module_rename(1, mzMOD_RENAME_TOPLEVEL, NULL);
      env->exp_env->rename = brn;
    }
    scheme_append_module_rename(et_rn, brn);

    brn = env->dt_rename;
    if (!brn) {
      brn = scheme_make_module_rename(MZ_LABEL_PHASE, mzMOD_RENAME_TOPLEVEL, NULL);
      env->dt_rename = brn;
    }
    scheme_append_module_rename(dt_rn, brn);
  }

  return scheme_void;
}

static Scheme_Object *
top_level_require_jit(Scheme_Object *data)
{
  return data;
}

static void top_level_require_validate(Scheme_Object *data, Mz_CPort *port, 
                                       char *stack, Scheme_Hash_Table *ht, Scheme_Object **tls,
				       int depth, int letlimit, int delta, 
				       int num_toplevels, int num_stxes, int num_lifts)
{
}

static Scheme_Object *
top_level_require_optimize(Scheme_Object *data, Optimize_Info *info)
{
  return scheme_make_syntax_compiled(REQUIRE_EXPD, data);
}

static Scheme_Object *
top_level_require_resolve(Scheme_Object *data, Resolve_Info *rslv)
{
  Scheme_Object *dummy = SCHEME_CAR(data);

  dummy = scheme_resolve_expr(dummy, rslv);

  return scheme_make_syntax_resolved(REQUIRE_EXPD, cons(dummy, SCHEME_CDR(data)));
}

static Scheme_Object *do_require(Scheme_Object *form, Scheme_Comp_Env *env, 
				 Scheme_Compile_Expand_Info *rec, int drec,
				 int for_phase)
{
  Scheme_Hash_Table *ht, *et_ht, *dt_ht;
  Scheme_Object *rn, *et_rn, *dt_rn, *dummy, *modidx;
  Scheme_Env *genv;

  if (!scheme_is_toplevel(env))
    scheme_wrong_syntax(NULL, NULL, form, "not at top-level or in module body");

  /* If we get here, it must be a top-level require. */

  /* Hash table is for checking duplicate names in require list: */
  ht = scheme_make_hash_table(SCHEME_hash_ptr);

  rn = scheme_make_module_rename(for_phase, mzMOD_RENAME_TOPLEVEL, NULL);
  if (!for_phase) {
    et_rn = scheme_make_module_rename(1, mzMOD_RENAME_TOPLEVEL, NULL);
    dt_rn = scheme_make_module_rename(MZ_LABEL_PHASE, mzMOD_RENAME_TOPLEVEL, NULL);
    et_ht = scheme_make_hash_table(SCHEME_hash_ptr);
    dt_ht = scheme_make_hash_table(SCHEME_hash_ptr);
  } else {
    et_rn = NULL;
    dt_rn = NULL;
    et_ht = NULL;
    dt_ht = NULL;
  }

  genv = env->genv;

  if (genv->module)
    modidx = genv->module->self_modidx;
  else
    modidx = scheme_false;

  if (for_phase == 1) {
    scheme_prepare_exp_env(genv);
    genv = genv->exp_env;
  } else if (for_phase == -1) {
    scheme_prepare_template_env(genv);
    genv = genv->template_env;
  }

  (void)parse_requires(form, modidx, genv, 
                       rn, rn,
                       et_rn, et_rn,
                       dt_rn, dt_rn,
		       check_dup_require, ht, et_ht, dt_ht, 0, 0, 
		       NULL, 0, 0, 0, NULL);

  if (rec[drec].comp) {
    /* Dummy lets us access a top-level environment: */
    dummy = scheme_make_environment_dummy(env);

    scheme_compile_rec_done_local(rec, drec);
    scheme_default_compile_rec(rec, drec);
    return scheme_make_syntax_compiled(REQUIRE_EXPD, 
				       cons(dummy,
					    cons(scheme_make_integer(for_phase),
						 form)));
  } else
    return form;
}

static Scheme_Object *
require_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_require(form, env, rec, drec, 0);
}

static Scheme_Object *
require_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_REQUIRE(erec[drec].observer);
  return do_require(form, env, erec, drec, 0);
}

static Scheme_Object *
require_for_syntax_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_require(form, env, rec, drec, 1);
}

static Scheme_Object *
require_for_syntax_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_REQUIRE_FOR_SYNTAX(erec[drec].observer);
  return do_require(form, env, erec, drec, 1);
}

static Scheme_Object *
require_for_template_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_require(form, env, rec, drec, -1);
}

static Scheme_Object *
require_for_template_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_REQUIRE_FOR_TEMPLATE(erec[drec].observer);
  return do_require(form, env, erec, drec, -1);
}

static Scheme_Object *
require_for_label_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_require(form, env, rec, drec, MZ_LABEL_PHASE);
}

static Scheme_Object *
require_for_label_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_REQUIRE_FOR_TEMPLATE(erec[drec].observer);
  return do_require(form, env, erec, drec, MZ_LABEL_PHASE);
}

/**********************************************************************/
/*                            dummy forms                             */
/**********************************************************************/

static Scheme_Object *
provide_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  scheme_wrong_syntax(NULL, NULL, form, "not in module body");
  return NULL;
}

static Scheme_Object *
provide_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_PROVIDE(erec[drec].observer);
  scheme_wrong_syntax(NULL, NULL, form, "not in module body");
  return NULL;
}

/**********************************************************************/
/*                        marshal/unmarshal                           */
/**********************************************************************/

XFORM_NONGCING static Scheme_Object *wrap_mod_stx(Scheme_Object *stx)
{
  return (stx ? stx : scheme_false);
}

static Scheme_Object *write_module(Scheme_Object *obj)
{
  Scheme_Module *m = (Scheme_Module *)obj;
  Scheme_Module_Phase_Exports *pt;
  Scheme_Object *l, *v;
  int i, k, count;

  l = m->dt_requires;
  l = cons(m->tt_requires, l);
  l = cons(m->et_requires, l);
  l = cons(m->requires, l);

  l = cons(m->body, l);
  l = cons(m->et_body, l);

  for (k = 0; k < 3; k++) {
    switch (k) {
    case 2:
      pt = m->me->dt;
      break;
    case 1:
      pt = m->me->et;
      break;
    case 0:
    default:
      pt = m->me->rt;
      break;
    }
    
    l = cons(scheme_make_integer(pt->num_provides), l);
    l = cons(scheme_make_integer(pt->num_var_provides), l);

    count = pt->num_provides;

    v = scheme_make_vector(count, NULL);
    for (i = 0; i < count; i++) {
      SCHEME_VEC_ELS(v)[i] = pt->provides[i];
    }
    l = cons(v, l);
  
    v = scheme_make_vector(count, NULL);
    for (i = 0; i < count; i++) {
      SCHEME_VEC_ELS(v)[i] = pt->provide_srcs[i];
    }
    l = cons(v, l);
    
    v = scheme_make_vector(count, NULL);
    for (i = 0; i < count; i++) {
      SCHEME_VEC_ELS(v)[i] = pt->provide_src_names[i];
    }
    l = cons(v, l);

    if (pt->provide_src_phases) {
      v = scheme_make_vector(count, NULL);
      for (i = 0; i < count; i++) {
        SCHEME_VEC_ELS(v)[i] = (pt->provide_src_phases[i] ? scheme_true : scheme_false);
      } 
    } else
      v = scheme_false;
    l = cons(v, l);
  }

  count = m->me->rt->num_provides;

  if (m->provide_protects) {
    for (i = 0; i < count; i++) {
      if (m->provide_protects[i])
	break;
    }
    if (i < count) {
      v = scheme_make_vector(count, NULL);
      for (i = 0; i < count; i++) {
	SCHEME_VEC_ELS(v)[i] = (m->provide_protects[i] ? scheme_true : scheme_false);
      }
    } else
      v = scheme_false;
    l = cons(v, l);
  } else
    l = cons(scheme_false, l);
  
  l = cons(scheme_make_integer(m->num_indirect_provides), l);

  count = m->num_indirect_provides;

  v = scheme_make_vector(count, NULL);
  for (i = 0; i < count; i++) {
    SCHEME_VEC_ELS(v)[i] = m->indirect_provides[i];
  }
  l = cons(v, l);

  l = cons(m->me->rt->reprovide_kernel ? scheme_true : scheme_false, l);
  l = cons(m->me->rt->kernel_exclusion, l);

  l = cons((Scheme_Object *)m->prefix, l);
  l = cons(m->dummy, l);

  l = cons(scheme_make_integer(m->max_let_depth), l);

  l = cons(wrap_mod_stx(m->dt_rn_stx), l);
  l = cons(wrap_mod_stx(m->tt_rn_stx), l);
  l = cons(wrap_mod_stx(m->et_rn_stx), l);
  l = cons(wrap_mod_stx(m->rn_stx), l);

  l = cons(m->me->src_modidx, l);
  l = cons(m->modname, l);

  return l;
}

static int check_requires_ok(Scheme_Object *l)
{
  Scheme_Object *x;
  while (!SCHEME_NULLP(l)) {
    x = SCHEME_CAR(l);
    if (!SCHEME_SYMBOLP(x) && !SAME_TYPE(SCHEME_TYPE(x), scheme_module_index_type))
      return 0;
    l = SCHEME_CDR(l);
  }
  return 1;
}

#define return_NULL() return (printf("%d\n", __LINE__), NULL)

static Scheme_Object *read_module(Scheme_Object *obj)
{
  Scheme_Module *m;
  Scheme_Object *ie, *nie;
  Scheme_Object *esp, *esn, *esph, *es, *e, *nve, *ne, **v;
  Scheme_Module_Exports *me;
  Scheme_Module_Phase_Exports *pt;
  char *ps, *sps;
  int i, k, count;

  m = MALLOC_ONE_TAGGED(Scheme_Module);
  m->so.type = scheme_module_type;

  me = make_module_exports();
  m->me = me;

  if (!SCHEME_PAIRP(obj)) return_NULL();
  m->modname = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  me->src_modidx = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  ((Scheme_Modidx *)m->me->src_modidx)->resolved = m->modname;
  m->self_modidx = m->me->src_modidx;

  if (!SCHEME_PAIRP(obj)) return_NULL();
  m->rn_stx = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  if (SCHEME_FALSEP(m->rn_stx))
    m->rn_stx = NULL;

  if (!SCHEME_PAIRP(obj)) return_NULL();
  m->et_rn_stx = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  if (SCHEME_FALSEP(m->et_rn_stx))
    m->et_rn_stx = NULL;

  if (!SCHEME_PAIRP(obj)) return_NULL();
  m->tt_rn_stx = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  if (SCHEME_FALSEP(m->tt_rn_stx))
    m->tt_rn_stx = NULL;

  if (!SCHEME_PAIRP(obj)) return_NULL();
  m->dt_rn_stx = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  if (SCHEME_FALSEP(m->dt_rn_stx))
    m->dt_rn_stx = NULL;

  if (!SCHEME_PAIRP(obj)) return_NULL();
  m->max_let_depth = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  m->dummy = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  m->prefix = (Resolve_Prefix *)SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  me->rt->kernel_exclusion = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return_NULL();
  me->rt->reprovide_kernel = SCHEME_TRUEP(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  ie = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  nie = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  
  count = SCHEME_INT_VAL(nie);

  if (!SCHEME_VECTORP(ie) || (SCHEME_VEC_SIZE(ie) != count)) return_NULL();
  v = MALLOC_N(Scheme_Object *, count);
  for (i = 0; i < count; i++) {
    v[i] = SCHEME_VEC_ELS(ie)[i];
  }
  m->indirect_provides = v;
  m->num_indirect_provides = count;

  if (!SCHEME_PAIRP(obj)) return_NULL();
  esp = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);

  for (k = 0; k < 3; k++) {
    switch (k) {
    case 0:
      pt = me->dt;
      break;
    case 1:
      pt = me->et;
      break;
    case 2:
    default:
      pt = me->rt;
      break;
    }

    if (!SCHEME_PAIRP(obj)) return_NULL();
    esph = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);

    if (!SCHEME_PAIRP(obj)) return_NULL();
    esn = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);

    if (!SCHEME_PAIRP(obj)) return_NULL();
    es = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);

    if (!SCHEME_PAIRP(obj)) return_NULL();
    e = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);
  
    if (!SCHEME_PAIRP(obj)) return_NULL();
    nve = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);

    if (!SCHEME_PAIRP(obj)) return_NULL();
    ne = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);

    count = SCHEME_INT_VAL(ne);
    pt->num_provides = count;
    pt->num_var_provides = SCHEME_INT_VAL(nve);

    if (!SCHEME_VECTORP(e) || (SCHEME_VEC_SIZE(e) != count)) return_NULL();
    v = MALLOC_N(Scheme_Object *, count);
    for (i = 0; i < count; i++) {
      v[i] = SCHEME_VEC_ELS(e)[i];
    }
    pt->provides = v;

    if (!SCHEME_VECTORP(es) || (SCHEME_VEC_SIZE(es) != count)) return_NULL();
    v = MALLOC_N(Scheme_Object *, count);
    for (i = 0; i < count; i++) {
      v[i] = SCHEME_VEC_ELS(es)[i];
    }
    pt->provide_srcs = v;

    if (!SCHEME_VECTORP(esn) || (SCHEME_VEC_SIZE(esn) != count)) return_NULL();
    v = MALLOC_N(Scheme_Object *, count);
    for (i = 0; i < count; i++) {
      v[i] = SCHEME_VEC_ELS(esn)[i];
    }
    pt->provide_src_names = v;

    if (SCHEME_FALSEP(esph))
      sps = NULL;
    else {
      if (!SCHEME_VECTORP(esph) || (SCHEME_VEC_SIZE(esph) != count)) return_NULL();
      sps = MALLOC_N_ATOMIC(char, count);
      for (i = 0; i < count; i++) {
        sps[i] = SCHEME_TRUEP(SCHEME_VEC_ELS(esph)[i]);
      }
    }
    pt->provide_src_phases = sps;
  }

  if (SCHEME_FALSEP(esp)) {
    m->provide_protects = NULL;
  } else {
    if (!SCHEME_VECTORP(esp) || (SCHEME_VEC_SIZE(esp) != count)) return_NULL();
    ps = MALLOC_N_ATOMIC(char, count);
    for (i = 0; i < count; i++) {
      ps[i] = SCHEME_TRUEP(SCHEME_VEC_ELS(esp)[i]);
    }
    m->provide_protects = ps;
  }

  if (!SCHEME_PAIRP(obj)) return_NULL();
  if (scheme_proper_list_length(SCHEME_CAR(obj)) < 0) return_NULL();
  e = scheme_copy_list(SCHEME_CAR(obj));
  m->et_body = e;
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  if (scheme_proper_list_length(SCHEME_CAR(obj)) < 0) return_NULL();
  e = scheme_copy_list(SCHEME_CAR(obj));
  m->body = e;
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  if (scheme_proper_list_length(SCHEME_CAR(obj)) < 0) return_NULL();
  e = scheme_copy_list(SCHEME_CAR(obj));
  m->requires = e;
  if (!check_requires_ok(e)) return_NULL();
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  if (scheme_proper_list_length(SCHEME_CAR(obj)) < 0) return_NULL();
  e = scheme_copy_list(SCHEME_CAR(obj));
  m->et_requires = e;
  if (!check_requires_ok(e)) return_NULL();
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  if (scheme_proper_list_length(SCHEME_CAR(obj)) < 0) return_NULL();
  e = scheme_copy_list(SCHEME_CAR(obj));
  m->tt_requires = e;
  if (!check_requires_ok(e)) return_NULL();
  obj = SCHEME_CDR(obj);

  if (scheme_proper_list_length(obj) < 0) return_NULL();
  e = scheme_copy_list(obj);
  m->dt_requires = e;
  if (!check_requires_ok(e)) return_NULL();
  
  return (Scheme_Object *)m;
}
