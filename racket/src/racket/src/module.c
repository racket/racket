/*
  Racket
  Copyright (c) 2004-2013 PLT Design Inc.
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

#define mz_MIN(l,o) ((l) < (o) ? (l) : (o))

/* globals */
SHARED_OK Scheme_Object *(*scheme_module_demand_hook)(int, Scheme_Object **);
THREAD_LOCAL_DECL(Scheme_Bucket_Table *scheme_module_code_cache);

SHARED_OK static Scheme_Bucket_Table *modpath_table;
#ifdef MZ_USE_PLACES
SHARED_OK static mzrt_mutex *modpath_table_mutex;
#else
# define mzrt_mutex_lock(l) /* empty */
# define mzrt_mutex_unlock(l) /* empty */
#endif

/* locals */
static Scheme_Object *current_module_name_resolver(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_module_name_prefix(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_module_name_source(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_module_load_path(int argc, Scheme_Object *argv[]);
static Scheme_Object *dynamic_require_for_syntax(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_require(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_require_copy(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_require_constant(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_require_etonly(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_attach_module(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_attach_module_decl(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_unprotect_module(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_name(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_imports(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_exports(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_lang_info(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_submodules(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_phaseless_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_to_namespace(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_to_lang_info(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_to_imports(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_to_exports(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_is_declared(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_is_predefined(int argc, Scheme_Object *argv[]);

static Scheme_Object *module_path_index_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_path_index_resolve(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_path_index_split(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_path_index_join(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_path_index_submodule(int argc, Scheme_Object *argv[]);

static Scheme_Object *is_module_path(int argc, Scheme_Object **argv);

static Scheme_Object *resolved_module_path_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_resolved_module_path(int argc, Scheme_Object *argv[]);
static Scheme_Object *resolved_module_path_name(int argc, Scheme_Object *argv[]);

static Scheme_Object *module_export_protected_p(int argc, Scheme_Object **argv);

/* syntax */
static Scheme_Object *module_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *module_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *modulestar_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *modulestar_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *module_begin_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *module_begin_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *declare_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *declare_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *require_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *require_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *provide_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *provide_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);

static Scheme_Module *module_load(Scheme_Object *modname, Scheme_Env *env, const char *who);

static void run_module(Scheme_Env *menv, int set_ns);
static void run_module_exptime(Scheme_Env *menv, int phase);

static void eval_exptime(Scheme_Object *names, int count,
                         Scheme_Object *expr, 
                         Scheme_Env *genv, Scheme_Comp_Env *env,
                         Resolve_Prefix *rp, int let_depth, int shift,
                         Scheme_Bucket_Table *syntax, int at_phase,
                         Scheme_Object *free_id_rename_rn,
                         Scheme_Object *insp);

typedef struct Module_Begin_Expand_State {
  /* All pointers, because it's allocated with scheme_malloc(): */
  Scheme_Object *post_ex_rn_set;
  Scheme_Hash_Table *tables; /* phase -> (vector toplevels requires syntaxes) */
  Scheme_Hash_Table *all_provided; /* phase -> table like `provided' */
  Scheme_Hash_Table *all_reprovided; /* phase -> list of (list modidx syntax except-name ...) */
  Scheme_Hash_Tree *all_defs; /* phase -> list of sxtid */
  Scheme_Hash_Table *all_defs_out; /* phase -> list of (cons protected? (stx-list except-name ...)) */
  int *all_simple_renames;
  int *_num_phases;
  Scheme_Object *saved_provides; /* list of (cons form phase) */
  Scheme_Object *saved_submodules; /* list of (cons form phase) */
  Scheme_Hash_Table *submodule_names; /* symbol -> #t (pre-module) or #<void> (post-module) */
  Scheme_Hash_Table *modidx_cache;
  Scheme_Object *redef_modname;
  Scheme_Object *end_statementss; /* list of lists */
  Scheme_Object *rn_stx;
} Module_Begin_Expand_State;

static Scheme_Object *do_module_begin_at_phase(Scheme_Object *form, Scheme_Comp_Env *env, 
                                               Scheme_Compile_Expand_Info *rec, int drec,
                                               Scheme_Compile_Expand_Info *erec, int derec,
                                               int phase, Scheme_Object *body_lists,
                                               Module_Begin_Expand_State *bxs);

static Scheme_Object *expand_all_provides(Scheme_Object *form,
                                          Scheme_Comp_Env *cenv, 
                                          Scheme_Compile_Expand_Info *rec, int drec,
                                          Scheme_Object *self_modidx,
                                          Module_Begin_Expand_State *bxs,
                                          int keep_expanded);

static Scheme_Object *expand_submodules(Scheme_Compile_Expand_Info *rec, int drec,
                                        Scheme_Comp_Env *env,
                                        Scheme_Object *l, int post,
                                        Module_Begin_Expand_State *bxs,
                                        int keep_expanded);

static Scheme_Object *fixup_expanded(Scheme_Object *expanded_l,
                                     Scheme_Object *expanded_provides,
                                     int phase,
                                     int kind);

static void check_formerly_unbound(Scheme_Object *unbounds, Scheme_Comp_Env *env);
static void install_stops(Scheme_Comp_Env *xenv, int phase, Scheme_Object **_begin_for_syntax_stx);
static int is_modulestar_stop(Scheme_Comp_Env *env);

typedef int (*Convert_Submodule_Proc)(Scheme_Object *mp, Scheme_Object *data);
static Scheme_Object *convert_submodule_path(Scheme_Object *name, 
                                             Convert_Submodule_Proc check,
                                             Scheme_Object *check_data);
static int check_is_submodule(Scheme_Object *modname, Scheme_Object *_genv);

static Scheme_Object *scheme_sys_wraps_phase_worker(intptr_t p);

static int phaseless_rhs(Scheme_Object *val, int var_count, int phase);

#define cons scheme_make_pair

/* We'd prefer a field like modsrc, but with submodule info like modname: */
#define MODSRCNAME modname

/* global read-only kernel stuff */
READ_ONLY static Scheme_Object *kernel_modname;
READ_ONLY static Scheme_Object *kernel_symbol;
READ_ONLY static Scheme_Object *kernel_modidx;
READ_ONLY static Scheme_Module *kernel;
READ_ONLY static Scheme_Object *flfxnum_modname;
READ_ONLY static Scheme_Object *extfl_modname;
READ_ONLY static Scheme_Object *futures_modname;
READ_ONLY static Scheme_Object *unsafe_modname;

/* global read-only phase wraps */
READ_ONLY static Scheme_Object *scheme_sys_wraps0;
READ_ONLY static Scheme_Object *scheme_sys_wraps1;

/* global read-only symbols */
ROSYM static Scheme_Object *module_begin_symbol;
ROSYM static Scheme_Object *prefix_symbol;
ROSYM static Scheme_Object *only_symbol;
ROSYM static Scheme_Object *rename_symbol;
ROSYM static Scheme_Object *all_except_symbol;
ROSYM static Scheme_Object *prefix_all_except_symbol;
ROSYM static Scheme_Object *all_from_symbol;
ROSYM static Scheme_Object *all_from_except_symbol;
ROSYM static Scheme_Object *all_defined_symbol;
ROSYM static Scheme_Object *all_defined_except_symbol;
ROSYM static Scheme_Object *prefix_all_defined_symbol;
ROSYM static Scheme_Object *prefix_all_defined_except_symbol;
ROSYM static Scheme_Object *struct_symbol;
ROSYM static Scheme_Object *protect_symbol;
ROSYM static Scheme_Object *expand_symbol;
ROSYM static Scheme_Object *for_syntax_symbol;
ROSYM static Scheme_Object *for_template_symbol;
ROSYM static Scheme_Object *for_label_symbol;
ROSYM static Scheme_Object *for_meta_symbol;
ROSYM static Scheme_Object *just_meta_symbol;
ROSYM static Scheme_Object *quote_symbol;
ROSYM static Scheme_Object *lib_symbol;
ROSYM static Scheme_Object *planet_symbol;
ROSYM static Scheme_Object *file_symbol;
ROSYM static Scheme_Object *submod_symbol;
ROSYM static Scheme_Object *module_name_symbol;
ROSYM static Scheme_Object *nominal_id_symbol;
ROSYM static Scheme_Object *phaseless_keyword;

/* global read-only syntax */
READ_ONLY Scheme_Object *scheme_module_stx;
READ_ONLY Scheme_Object *scheme_modulestar_stx;
READ_ONLY Scheme_Object *scheme_module_begin_stx;
READ_ONLY Scheme_Object *scheme_begin_stx;
READ_ONLY Scheme_Object *scheme_define_values_stx;
READ_ONLY Scheme_Object *scheme_define_syntaxes_stx;
READ_ONLY Scheme_Object *scheme_top_stx;
READ_ONLY Scheme_Object *scheme_begin_for_syntax_stx;
READ_ONLY static Scheme_Object *modbeg_syntax;
READ_ONLY static Scheme_Object *require_stx;
READ_ONLY static Scheme_Object *provide_stx;
READ_ONLY static Scheme_Object *declare_stx;
READ_ONLY static Scheme_Object *set_stx;
READ_ONLY static Scheme_Object *app_stx;
READ_ONLY static Scheme_Object *lambda_stx;
READ_ONLY static Scheme_Object *case_lambda_stx;
READ_ONLY static Scheme_Object *let_values_stx;
READ_ONLY static Scheme_Object *letrec_values_stx;
READ_ONLY static Scheme_Object *if_stx;
READ_ONLY static Scheme_Object *begin0_stx;
READ_ONLY static Scheme_Object *set_stx;
READ_ONLY static Scheme_Object *with_continuation_mark_stx;
READ_ONLY static Scheme_Object *letrec_syntaxes_stx;
READ_ONLY static Scheme_Object *var_ref_stx;
READ_ONLY static Scheme_Object *expression_stx;
READ_ONLY static Scheme_Object *quote_stx;
READ_ONLY static Scheme_Object *datum_stx;

READ_ONLY static Scheme_Object *make_struct_type_stx;
READ_ONLY static Scheme_Object *make_struct_type_property_stx;
READ_ONLY static Scheme_Object *list_stx;
READ_ONLY static Scheme_Object *cons_stx;
READ_ONLY static Scheme_Object *gensym_stx;
READ_ONLY static Scheme_Object *string_to_uninterned_symbol_stx;

READ_ONLY static Scheme_Object *empty_self_modidx;
READ_ONLY static Scheme_Object *empty_self_modname;

THREAD_LOCAL_DECL(static Scheme_Object *empty_self_shift_cache);
THREAD_LOCAL_DECL(static Scheme_Bucket_Table *starts_table);
THREAD_LOCAL_DECL(static Scheme_Bucket_Table *submodule_empty_modidx_table);
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
# define PLACE_LOCAL_MODPATH_TABLE 1
THREAD_LOCAL_DECL(static Scheme_Bucket_Table *place_local_modpath_table);
#else
# define PLACE_LOCAL_MODPATH_TABLE 0
#endif

THREAD_LOCAL_DECL(static Scheme_Env *initial_modules_env);
THREAD_LOCAL_DECL(static int num_initial_modules);
THREAD_LOCAL_DECL(static Scheme_Object **initial_modules);
THREAD_LOCAL_DECL(static Scheme_Object *initial_renames);
THREAD_LOCAL_DECL(static Scheme_Bucket_Table *initial_toplevel);

/* caches */
THREAD_LOCAL_DECL(static Scheme_Modidx *modidx_caching_chain);
THREAD_LOCAL_DECL(static Scheme_Object *global_shift_cache);
#define GLOBAL_SHIFT_CACHE_SIZE 40
#ifdef USE_SENORA_GC
# define SHIFT_CACHE_NULL scheme_false
# define SHIFT_CACHE_NULLP(x) SCHEME_FALSEP(x)
#else
# define SHIFT_CACHE_NULL NULL
# define SHIFT_CACHE_NULLP(x) !(x)
#endif

#define SCHEME_RMP_VAL(obj)  SCHEME_PTR_VAL(obj)

#define DONE_MODFORM_KIND 0
#define EXPR_MODFORM_KIND 1
#define DEFN_MODFORM_KIND 2
#define PROVIDE_MODFORM_KIND 3
#define MODULE_MODFORM_KIND 4
#define SAVED_MODFORM_KIND 5

/* combined bitwise: */
#define NON_PHASELESS_IMPORT 0x1
#define NON_PHASELESS_FORM   0x2

typedef void (*Check_Func)(Scheme_Object *prnt_name, Scheme_Object *name, 
                           Scheme_Object *nominal_modname, Scheme_Object *nominal_export,
			   Scheme_Object *modname, Scheme_Object *srcname, int exet,
			   int isval, void *data, Scheme_Object *e, Scheme_Object *form, 
                           Scheme_Object *err_src, Scheme_Object *mark_src,
                           Scheme_Object *to_phase, Scheme_Object *src_phase_index,
                           Scheme_Object *nominal_export_phase);
static void parse_requires(Scheme_Object *form, int at_phase,
                           Scheme_Object *base_modidx,
                           Scheme_Env *env,
                           Scheme_Module *for_m,
                           Scheme_Object *rns, Scheme_Object *post_ex_rns,
                           Check_Func ck, void *data,
                           Scheme_Object *redef_modname,
                           int unpack_kern, int copy_vars, int can_save_marshal, 
                           int eval_exp, int eval_run,
                           int *all_simple,
                           Scheme_Hash_Table *modix_cache,
                           Scheme_Hash_Table *submodule_names,
                           int *non_phaseless);
static void parse_provides(Scheme_Object *form, Scheme_Object *fst, Scheme_Object *e,
                           int at_phase,
                           Scheme_Hash_Table *all_provided,
                           Scheme_Hash_Table *all_reprovided,
                           Scheme_Object *self_modidx,
                           Scheme_Hash_Table *all_defs_out, 
                           Scheme_Hash_Table *tables,
                           Scheme_Hash_Tree *all_defs,
                           Scheme_Comp_Env *cenv, Scheme_Compile_Info *rec, int drec,
                           Scheme_Object **_expanded);
static int compute_reprovides(Scheme_Hash_Table *all_provided,
                              Scheme_Hash_Table *all_reprovided, 
                              Scheme_Module *mod_for_requires,
                              Scheme_Hash_Table *tables,
                              Scheme_Env *genv, 
                              int num_phases,
                              Scheme_Hash_Tree *all_defs, Scheme_Hash_Table *all_defs_out,
                              const char *matching_form,
                              Scheme_Object *all_mods, Scheme_Object *all_phases);
static void compute_provide_arrays(Scheme_Hash_Table *all_provided, Scheme_Hash_Table *tables,
                                   Scheme_Module_Exports *me,
                                   Scheme_Env *genv,
                                   Scheme_Object *form,
                                   int num_phases, Scheme_Module_Export_Info **exp_infos);
static Scheme_Object **compute_indirects(Scheme_Env *genv, 
                                         Scheme_Module_Phase_Exports *pt,
                                         int *_count,
                                         int vars);
static void start_module(Scheme_Module *m, Scheme_Env *env, int restart, Scheme_Object *syntax_idx, 
                         int eval_exp, int eval_run, intptr_t base_phase, Scheme_Object *cycle_list,
                         int not_new);
static void eval_module_body(Scheme_Env *menv, Scheme_Env *env);

static Scheme_Object *do_namespace_require(Scheme_Env *env, int argc, Scheme_Object *argv[], 
                                           int copy, int etonly);

static Scheme_Object *default_module_resolver(int argc, Scheme_Object **argv);

static void qsort_provides(Scheme_Object **exs, Scheme_Object **exsns, Scheme_Object **exss, char *exps, int *exets,
                           Scheme_Object **exsnoms,
			   int start, int count, int do_uninterned);

#define MODCHAIN_TABLE(p) ((Scheme_Hash_Table *)(SCHEME_VEC_ELS(p)[0]))
#define MODCHAIN_AVAIL(p, n) (SCHEME_VEC_ELS(p)[3+n])

/**********************************************************************/
/*                           initialization                           */
/**********************************************************************/

void scheme_init_module(Scheme_Env *env)
{
  scheme_add_global_keyword("module", 
			    scheme_make_compiled_syntax(module_syntax, 
							module_expand),
			    env);
  scheme_add_global_keyword("module*", 
			    scheme_make_compiled_syntax(modulestar_syntax, 
							modulestar_expand),
			    env);

  REGISTER_SO(modbeg_syntax);
  modbeg_syntax = scheme_make_compiled_syntax(module_begin_syntax, 
					      module_begin_expand);

  scheme_add_global_keyword("#%module-begin", 
			    modbeg_syntax,
			    env);

  scheme_add_global_keyword("#%declare",
                            scheme_make_compiled_syntax(declare_syntax,
                                                        declare_expand),
			    env);

  scheme_add_global_keyword("#%require", 
			    scheme_make_compiled_syntax(require_syntax, 
							require_expand), 
			    env);
  scheme_add_global_keyword("#%provide", 
			    scheme_make_compiled_syntax(provide_syntax, 
							provide_expand), 
			    env);

#ifdef MZ_USE_PLACES
  mzrt_mutex_create(&modpath_table_mutex);
#endif

  if (!empty_self_modidx) {
    REGISTER_SO(empty_self_modidx);
    REGISTER_SO(empty_self_modname);
    empty_self_modidx = scheme_make_modidx(scheme_false, scheme_false, scheme_false);
    (void)scheme_hash_key(empty_self_modidx);
    empty_self_modname = scheme_make_symbol("expanded module"); /* uninterned */
    empty_self_modname = scheme_intern_resolved_module_path(empty_self_modname);
  }

  REGISTER_SO(quote_symbol);
  REGISTER_SO(file_symbol);
  REGISTER_SO(lib_symbol);
  REGISTER_SO(planet_symbol);
  REGISTER_SO(submod_symbol);
  quote_symbol = scheme_intern_symbol("quote");
  file_symbol = scheme_intern_symbol("file");
  lib_symbol = scheme_intern_symbol("lib");
  planet_symbol = scheme_intern_symbol("planet");
  submod_symbol = scheme_intern_symbol("submod");

  REGISTER_SO(kernel_symbol);
  REGISTER_SO(kernel_modname);
  REGISTER_SO(kernel_modidx);
  REGISTER_SO(unsafe_modname);
  REGISTER_SO(flfxnum_modname);
  REGISTER_SO(extfl_modname);
  REGISTER_SO(futures_modname);
  kernel_symbol = scheme_intern_symbol("#%kernel");
  kernel_modname = scheme_intern_resolved_module_path(kernel_symbol);
  kernel_modidx = scheme_make_modidx(scheme_make_pair(quote_symbol,
                                                      scheme_make_pair(kernel_symbol, 
                                                                       scheme_null)),
                                     scheme_false, kernel_modname);
  (void)scheme_hash_key(kernel_modidx);
  unsafe_modname = scheme_intern_resolved_module_path(scheme_intern_symbol("#%unsafe"));
  flfxnum_modname = scheme_intern_resolved_module_path(scheme_intern_symbol("#%flfxnum"));
  extfl_modname = scheme_intern_resolved_module_path(scheme_intern_symbol("#%extfl"));
  futures_modname = scheme_intern_resolved_module_path(scheme_intern_symbol("#%futures"));
  
  REGISTER_SO(module_begin_symbol);
  module_begin_symbol = scheme_intern_symbol("#%module-begin");

  GLOBAL_PARAMETER("current-module-name-resolver",  current_module_name_resolver, MZCONFIG_CURRENT_MODULE_RESOLVER, env);
  GLOBAL_PARAMETER("current-module-declare-name",   current_module_name_prefix,   MZCONFIG_CURRENT_MODULE_NAME,     env);
  GLOBAL_PARAMETER("current-module-declare-source", current_module_name_source,   MZCONFIG_CURRENT_MODULE_SRC,      env);
  GLOBAL_PARAMETER("current-module-path-for-load",  current_module_load_path,     MZCONFIG_CURRENT_MODULE_LOAD_PATH, env);

  GLOBAL_PRIM_W_ARITY("dynamic-require",                  scheme_dynamic_require,     2, 3, env);
  GLOBAL_PRIM_W_ARITY("dynamic-require-for-syntax",       dynamic_require_for_syntax, 2, 3, env);
  GLOBAL_PRIM_W_ARITY("namespace-require",                namespace_require,          1, 1, env);
  GLOBAL_PRIM_W_ARITY("namespace-attach-module",          namespace_attach_module,    2, 3, env);
  GLOBAL_PRIM_W_ARITY("namespace-attach-module-declaration", namespace_attach_module_decl, 2, 3, env);
  GLOBAL_PRIM_W_ARITY("namespace-unprotect-module",       namespace_unprotect_module, 2, 3, env);
  GLOBAL_PRIM_W_ARITY("namespace-require/copy",           namespace_require_copy,     1, 1, env);
  GLOBAL_PRIM_W_ARITY("namespace-require/constant",       namespace_require_constant, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("namespace-require/expansion-time", namespace_require_etonly,   1, 1, env);
  GLOBAL_PRIM_W_ARITY("compiled-module-expression?",      module_compiled_p,          1, 1, env);
  GLOBAL_PRIM_W_ARITY("module-compiled-name",             module_compiled_name,       1, 2, env);
  GLOBAL_PRIM_W_ARITY("module-compiled-imports",          module_compiled_imports,    1, 1, env);
  GLOBAL_PRIM_W_ARITY2("module-compiled-exports",         module_compiled_exports,    1, 1, 2, 2, env);
  GLOBAL_PRIM_W_ARITY("module-compiled-language-info",    module_compiled_lang_info,  1, 1, env);
  GLOBAL_PRIM_W_ARITY("module-compiled-submodules",       module_compiled_submodules, 2, 3, env);
  GLOBAL_PRIM_W_ARITY("module-compiled-cross-phase-persistent?", module_compiled_phaseless_p, 1, 1, env);
  GLOBAL_FOLDING_PRIM("module-path-index?",               module_path_index_p,        1, 1, 1, env); 
  GLOBAL_PRIM_W_ARITY("module-path-index-resolve",        module_path_index_resolve,  1, 1, env); 
  GLOBAL_PRIM_W_ARITY2("module-path-index-split",         module_path_index_split,    1, 1, 2, 2, env); 
  GLOBAL_PRIM_W_ARITY("module-path-index-submodule",      module_path_index_submodule,1, 1, env); 
  GLOBAL_PRIM_W_ARITY("module-path-index-join",           module_path_index_join,     2, 3, env);
  GLOBAL_FOLDING_PRIM("resolved-module-path?",            resolved_module_path_p,     1, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("make-resolved-module-path",        make_resolved_module_path,  1, 1, env);
  GLOBAL_PRIM_W_ARITY("resolved-module-path-name",        resolved_module_path_name,  1, 1, env);
  GLOBAL_PRIM_W_ARITY("module-provide-protected?",        module_export_protected_p,  2, 2, env);
  GLOBAL_PRIM_W_ARITY("module->namespace",                module_to_namespace,        1, 1, env);
  GLOBAL_PRIM_W_ARITY("module->language-info",            module_to_lang_info,        1, 2, env);
  GLOBAL_PRIM_W_ARITY("module->imports",                  module_to_imports,          1, 1, env);
  GLOBAL_PRIM_W_ARITY2("module->exports",                 module_to_exports,          1, 1, 2, 2, env);
  GLOBAL_PRIM_W_ARITY("module-declared?",                 module_is_declared,         1, 2, env);
  GLOBAL_PRIM_W_ARITY("module-predefined?",               module_is_predefined,       1, 1, env);
  GLOBAL_PRIM_W_ARITY("module-path?",                     is_module_path,             1, 1, env);
}

void scheme_init_module_resolver(void)
{
  Scheme_Object *o;
  Scheme_Config *config;

  /* this function is called multiple times when scheme_basic_env() is called multiple times */

  if (!starts_table) {
    REGISTER_SO(starts_table);
    starts_table = scheme_make_weak_equal_table();
#if PLACE_LOCAL_MODPATH_TABLE
    REGISTER_SO(place_local_modpath_table);
    place_local_modpath_table = scheme_make_weak_equal_table();
#endif
  }

  config = scheme_current_config();

  o = scheme_make_prim_w_arity(default_module_resolver,
			       "default-module-name-resolver",
			       2, 4);
 
  scheme_set_param(config, MZCONFIG_CURRENT_MODULE_RESOLVER, o);

  scheme_set_param(config, MZCONFIG_CURRENT_MODULE_NAME, scheme_false);
}

static void add_exp_infos(Scheme_Module *m)
{
  Scheme_Module_Export_Info **exp_infos, *exp_info;
  
  exp_infos = MALLOC_N(Scheme_Module_Export_Info *, 1);
  exp_info = MALLOC_ONE_RT(Scheme_Module_Export_Info);
  SET_REQUIRED_TAG(exp_info->type = scheme_rt_export_info);
  exp_infos[0] = exp_info;
  m->exp_infos = exp_infos;
  m->num_phases = 1;
}

void scheme_finish_kernel(Scheme_Env *env)
{
  /* When this function is called, the initial namespace has all the
     primitive bindings for syntax and procedures. This function fills
     in the module wrapper for #%kernel. */
  Scheme_Object *w;
  char *running;

  REGISTER_SO(kernel);

  kernel = MALLOC_ONE_TAGGED(Scheme_Module);
  kernel->so.type = scheme_module_type;
  kernel->predefined = 1;
  kernel->phaseless = scheme_true;
  env->module = kernel;

  {
    Scheme_Object *insp;
    insp = scheme_get_current_inspector();

    env->guard_insp = insp; /* nothing is protected, anyway */
    env->access_insp = insp;
    kernel->insp = insp;
  }

  kernel->modname = kernel_modname;
  kernel->modsrc = kernel_modname;
  kernel->requires = scheme_null;
  kernel->et_requires = scheme_null;
  kernel->tt_requires = scheme_null;
  kernel->dt_requires = scheme_null;
  kernel->other_requires = NULL;
  add_exp_infos(kernel);
  
  {
    Scheme_Bucket_Table *ht;
    int i, j, count, syntax_start = 0;
    Scheme_Bucket **bs;
    Scheme_Object **exs;
    Scheme_Object *rn;
    /* Provide all syntax and variables: */
    count = 0;
    for (j = 0; j < 2; j++) {
      if (!j)
        ht = env->toplevel;
      else {
        ht = env->syntax;
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
        ht = env->toplevel;
      else
        ht = env->syntax;

      bs = ht->buckets;
      for (i = ht->size; i--; ) {
        Scheme_Bucket *b = bs[i];
        if (b && b->val)
          exs[count++] = (Scheme_Object *)b->key;
      }
    }

    {
      Scheme_Module_Exports *me;
      me = scheme_make_module_exports();
      kernel->me = me;
      kernel->me->modsrc = kernel_modname;
    }

    kernel->me->rt->provides = exs;
    kernel->me->rt->provide_srcs = NULL;
    kernel->me->rt->provide_src_names = exs;
    kernel->me->rt->num_provides = count;
    kernel->me->rt->num_var_provides = syntax_start;
    scheme_populate_pt_ht(kernel->me->rt);

    running = (char *)scheme_malloc_atomic(2);
    running[0] = 1;
    running[1] = 1;
    env->running = running;
    env->attached = 1;
    
    /* Since this is the first module rename, it's registered as
       the kernel module rename: */
    rn = scheme_make_module_rename(scheme_make_integer(0), mzMOD_RENAME_NORMAL, NULL, NULL, NULL);
    for (i = kernel->me->rt->num_provides; i--; ) {
      scheme_extend_module_rename(rn, kernel_modidx, exs[i], exs[i], kernel_modidx, exs[i], 
                                  0, scheme_make_integer(0), NULL, 0);
    }
    scheme_seal_module_rename(rn, STX_SEAL_ALL);
  }

  REGISTER_SO(scheme_sys_wraps0);
  REGISTER_SO(scheme_sys_wraps1);

  scheme_sys_wraps0 = scheme_sys_wraps_phase_worker(0);
  scheme_sys_wraps1 = scheme_sys_wraps_phase_worker(1);

  scheme_sys_wraps(NULL);

  REGISTER_SO(scheme_module_stx);
  REGISTER_SO(scheme_modulestar_stx);
  REGISTER_SO(scheme_module_begin_stx);
  REGISTER_SO(scheme_begin_stx);
  REGISTER_SO(scheme_define_values_stx);
  REGISTER_SO(scheme_define_syntaxes_stx);
  REGISTER_SO(scheme_begin_for_syntax_stx);
  REGISTER_SO(require_stx);
  REGISTER_SO(provide_stx);
  REGISTER_SO(declare_stx);
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
  REGISTER_SO(quote_stx);
  REGISTER_SO(datum_stx);

  w = scheme_sys_wraps0;
  scheme_module_stx = scheme_datum_to_syntax(scheme_intern_symbol("module"), scheme_false, w, 0, 0);
  scheme_modulestar_stx = scheme_datum_to_syntax(scheme_intern_symbol("module*"), scheme_false, w, 0, 0);
  scheme_module_begin_stx = scheme_datum_to_syntax(module_begin_symbol, scheme_false, w, 0, 0);
  scheme_begin_stx = scheme_datum_to_syntax(scheme_intern_symbol("begin"), scheme_false, w, 0, 0);
  scheme_define_values_stx = scheme_datum_to_syntax(scheme_intern_symbol("define-values"), scheme_false, w, 0, 0);
  scheme_define_syntaxes_stx = scheme_datum_to_syntax(scheme_intern_symbol("define-syntaxes"), scheme_false, w, 0, 0);
  scheme_begin_for_syntax_stx = scheme_datum_to_syntax(scheme_intern_symbol("begin-for-syntax"), scheme_false, w, 0, 0);
  require_stx = scheme_datum_to_syntax(scheme_intern_symbol("#%require"), scheme_false, w, 0, 0);
  provide_stx = scheme_datum_to_syntax(scheme_intern_symbol("#%provide"), scheme_false, w, 0, 0);
  declare_stx = scheme_datum_to_syntax(scheme_intern_symbol("#%declare"), scheme_false, w, 0, 0);
  set_stx = scheme_datum_to_syntax(scheme_intern_symbol("set!"), scheme_false, w, 0, 0);
  app_stx = scheme_datum_to_syntax(scheme_intern_symbol("#%app"), scheme_false, w, 0, 0);
  scheme_top_stx = scheme_datum_to_syntax(scheme_intern_symbol("#%top"), scheme_false, w, 0, 0);
  lambda_stx = scheme_datum_to_syntax(scheme_intern_symbol("lambda"), scheme_false, w, 0, 0);
  case_lambda_stx = scheme_datum_to_syntax(scheme_intern_symbol("case-lambda"), scheme_false, w, 0, 0);
  let_values_stx = scheme_datum_to_syntax(scheme_intern_symbol("let-values"), scheme_false, w, 0, 0);
  letrec_values_stx = scheme_datum_to_syntax(scheme_intern_symbol("letrec-values"), scheme_false, w, 0, 0);
  if_stx = scheme_datum_to_syntax(scheme_intern_symbol("if"), scheme_false, w, 0, 0);
  begin0_stx = scheme_datum_to_syntax(scheme_intern_symbol("begin0"), scheme_false, w, 0, 0);
  with_continuation_mark_stx = scheme_datum_to_syntax(scheme_intern_symbol("with-continuation-mark"), scheme_false, w, 0, 0);
  letrec_syntaxes_stx = scheme_datum_to_syntax(scheme_intern_symbol("letrec-syntaxes+values"), scheme_false, w, 0, 0);
  var_ref_stx = scheme_datum_to_syntax(scheme_intern_symbol("#%variable-reference"), scheme_false, w, 0, 0);
  expression_stx = scheme_datum_to_syntax(scheme_intern_symbol("#%expression"), scheme_false, w, 0, 0);
  quote_stx = scheme_datum_to_syntax(scheme_intern_symbol("quote"), scheme_false, w, 0, 0);
  datum_stx = scheme_datum_to_syntax(scheme_intern_symbol("#%datum"), scheme_false, w, 0, 0);

  REGISTER_SO(make_struct_type_stx);
  REGISTER_SO(make_struct_type_property_stx);
  REGISTER_SO(cons_stx);
  REGISTER_SO(list_stx);
  REGISTER_SO(gensym_stx);
  REGISTER_SO(string_to_uninterned_symbol_stx);

  make_struct_type_stx = scheme_datum_to_syntax(scheme_intern_symbol("make-struct-type"), scheme_false, w, 0, 0);
  make_struct_type_property_stx = scheme_datum_to_syntax(scheme_intern_symbol("make-struct-type-property"), scheme_false, w, 0, 0);
  cons_stx = scheme_datum_to_syntax(scheme_intern_symbol("cons"), scheme_false, w, 0, 0);
  list_stx = scheme_datum_to_syntax(scheme_intern_symbol("list"), scheme_false, w, 0, 0);
  gensym_stx = scheme_datum_to_syntax(scheme_intern_symbol("gensym"), scheme_false, w, 0, 0);
  string_to_uninterned_symbol_stx = scheme_datum_to_syntax(scheme_intern_symbol("string->uninterned-symbol"), 
                                                           scheme_false, w, 0, 0);

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
  REGISTER_SO(expand_symbol);
  REGISTER_SO(for_syntax_symbol);
  REGISTER_SO(for_template_symbol);
  REGISTER_SO(for_label_symbol);
  REGISTER_SO(for_meta_symbol);
  REGISTER_SO(just_meta_symbol);
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
  expand_symbol = scheme_intern_symbol("expand");
  for_syntax_symbol = scheme_intern_symbol("for-syntax");
  for_template_symbol = scheme_intern_symbol("for-template");
  for_label_symbol = scheme_intern_symbol("for-label");
  for_meta_symbol = scheme_intern_symbol("for-meta");
  just_meta_symbol = scheme_intern_symbol("just-meta");

  REGISTER_SO(module_name_symbol);
  module_name_symbol = scheme_intern_symbol("enclosing-module-name");

  REGISTER_SO(nominal_id_symbol);
  nominal_id_symbol = scheme_intern_symbol("nominal-id");

  REGISTER_SO(phaseless_keyword);
  {
    const char *s = "cross-phase-persistent";
    phaseless_keyword = scheme_intern_exact_keyword(s, strlen(s));
  }
}

int scheme_is_kernel_modname(Scheme_Object *modname)
{
  return SAME_OBJ(modname, kernel_modname);
}

int scheme_is_unsafe_modname(Scheme_Object *modname)
{
  return SAME_OBJ(modname, unsafe_modname);
}

int scheme_is_flfxnum_modname(Scheme_Object *modname)
{
  return SAME_OBJ(modname, flfxnum_modname);
}

int scheme_is_extfl_modname(Scheme_Object *modname)
{
  return SAME_OBJ(modname, extfl_modname);
}

int scheme_is_futures_modname(Scheme_Object *modname)
{
  return SAME_OBJ(modname, futures_modname);
}

Scheme_Module *get_special_module(Scheme_Object *name)
{
  if (SAME_OBJ(name, kernel_modname))
    return kernel;
  else if (SAME_OBJ(name, unsafe_modname))
    return scheme_get_unsafe_env()->module;
  else if (SAME_OBJ(name, flfxnum_modname))
    return scheme_get_flfxnum_env()->module;
  else if (SAME_OBJ(name, extfl_modname))
    return scheme_get_extfl_env()->module;
  else if (SAME_OBJ(name, futures_modname))
    return scheme_get_futures_env()->module;
  else
    return NULL;
}

Scheme_Env *get_special_modenv(Scheme_Object *name)
{
  if (SAME_OBJ(name, kernel_modname))
    return scheme_get_kernel_env();
  else if (SAME_OBJ(name, flfxnum_modname))
    return scheme_get_flfxnum_env();
  else if (SAME_OBJ(name, extfl_modname))
    return scheme_get_extfl_env();
  else if (SAME_OBJ(name, futures_modname))
    return scheme_get_futures_env();
  else if (SAME_OBJ(name, unsafe_modname))
    return scheme_get_unsafe_env();
  else
    return NULL;
}

static int is_builtin_modname(Scheme_Object *modname) 
{
  return (SAME_OBJ(modname, kernel_modname)
          || SAME_OBJ(modname, unsafe_modname)
          || SAME_OBJ(modname, flfxnum_modname)
          || SAME_OBJ(modname, extfl_modname)
          || SAME_OBJ(modname, futures_modname));
}

Scheme_Object *scheme_sys_wraps(Scheme_Comp_Env *env)
{
  intptr_t phase;

  if (!env)
    phase = 0;
  else if (SCHEME_INTP((Scheme_Object *)env))
    phase = SCHEME_INT_VAL((Scheme_Object *)env);
  else
    phase = env->genv->phase;

  return scheme_sys_wraps_phase(scheme_make_integer(phase));
}

static Scheme_Object *scheme_sys_wraps_phase_worker(intptr_t p)
{
  Scheme_Object *rn, *w;

  rn = scheme_make_module_rename(scheme_make_integer(p), mzMOD_RENAME_NORMAL, NULL, NULL, NULL);

  /* Add a module mapping for all kernel provides: */
  scheme_extend_module_rename_with_shared(rn, kernel_modidx, 
                                          kernel->me->rt,
                                          scheme_make_integer(p),
                                          scheme_make_integer(0),
                                          scheme_null,
                                          NULL,
                                          1);

  scheme_seal_module_rename(rn, STX_SEAL_ALL);

  w = scheme_datum_to_syntax(kernel_symbol, scheme_false, scheme_false, 0, 0);
  w = scheme_add_rename(w, rn);

  return w;
}

Scheme_Object *scheme_sys_wraps_phase(Scheme_Object *phase)
{
  intptr_t p;

  if (SCHEME_INTP(phase))
    p = SCHEME_INT_VAL(phase);
  else
    p = -1;

  if (p == 0) return scheme_sys_wraps0;
  if (p == 1) return scheme_sys_wraps1;

  return scheme_sys_wraps_phase_worker(p);
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
  
  ht = env->module_registry->loaded;
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
  initial_renames = scheme_make_module_rename(scheme_make_integer(0), 
                                              mzMOD_RENAME_NORMAL, 
                                              NULL,
                                              NULL,
                                              NULL);
  scheme_prepare_env_renames(env, mzMOD_RENAME_TOPLEVEL);
  scheme_append_module_rename(scheme_get_module_rename_from_set(env->rename_set, 
                                                                scheme_make_integer(0),
                                                                1),
                              initial_renames, 
                              1);
  
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
    m = (Scheme_Module *)scheme_hash_get(initial_modules_env->module_registry->loaded, a[1]);
    start_module(m, initial_modules_env, 0, a[1], 0, 1, 0, scheme_null, 0);

    namespace_attach_module(3, a);
  }

  /* Copy renamings: */
  scheme_prepare_env_renames(env, mzMOD_RENAME_TOPLEVEL);
  scheme_append_module_rename(initial_renames, 
                              scheme_get_module_rename_from_set(env->rename_set, 
                                                                scheme_make_integer(0),
                                                                1),
                              1);

  /* Copy toplevel: */
  {
    Scheme_Bucket_Table *tl;
    tl = scheme_clone_toplevel(initial_toplevel, env);
    env->toplevel = tl;
  }
}

static Scheme_Module *registry_get_loaded(Scheme_Env *env, Scheme_Object *name)
{
  Scheme_Object *o;

  if (env->module_pre_registry && env->module_pre_registry->loaded) {
    o = scheme_hash_get(env->module_pre_registry->loaded, name);
    if (o) 
      return (Scheme_Module *)o;
  }

  return (Scheme_Module *)scheme_hash_get(env->module_registry->loaded, name);
}

/**********************************************************************/
/*                             parameters                             */
/**********************************************************************/

static Scheme_Object *default_module_resolver(int argc, Scheme_Object **argv)
{
  Scheme_Object *p = argv[0];

  if (argc == 2)
    return scheme_void; /* ignore notify */

  /* if (quote SYMBOL) */
  if (SCHEME_PAIRP(p)
      && SAME_OBJ(SCHEME_CAR(p), quote_symbol)
      && SCHEME_PAIRP(SCHEME_CDR(p))
      && SCHEME_SYMBOLP(SCHEME_CAR(SCHEME_CDR(p)))
      && SCHEME_NULLP(SCHEME_CDR(SCHEME_CDR(p))))
    return scheme_intern_resolved_module_path(SCHEME_CAR(SCHEME_CDR(p)));

  scheme_contract_error("default-module-name-resolver", 
                        "the kernel's resolver works only on `quote' forms", 
                        "given", 1, p,
                        NULL);
  return NULL;
}

static Scheme_Object *check_resolver(int argc, Scheme_Object **argv)
{
  if (scheme_check_proc_arity(NULL, 2, 0, argc, argv)
      && scheme_check_proc_arity(NULL, 4, 0, argc, argv))
    return argv[0];

  scheme_wrong_contract("current-module-name-resolver", 
                        "(case-> (any/c any/c . -> . any) (any/c any/c any/c any/c . -> . any))", 
                        0, argc, argv);

  return NULL;
}

static Scheme_Object *
current_module_name_resolver(int argc, Scheme_Object *argv[])
{
  return scheme_param_config2("current-module-name-resolver",
                              scheme_make_integer(MZCONFIG_CURRENT_MODULE_RESOLVER),
                              argc, argv,
                              -1, check_resolver, 
                              "(and/c (procedure-arity-includes/c 1)"
                              /* */ " (procedure-arity-includes/c 4))", 
                              1);
}

static Scheme_Object *prefix_p(int argc, Scheme_Object **argv)
{
  Scheme_Object *o = argv[0];
  
  if (SCHEME_FALSEP(o) || (SCHEME_MODNAMEP(o)))
    return o;

  return NULL;
}

static Scheme_Object *
current_module_name_prefix(int argc, Scheme_Object *argv[])
{
  return scheme_param_config2("current-module-declared-name",
                              scheme_make_integer(MZCONFIG_CURRENT_MODULE_NAME),
                              argc, argv,
                              -1, prefix_p, "(or/c resolved-module-path? #f)", 1);
}

static Scheme_Object *source_p(int argc, Scheme_Object **argv)
{
  Scheme_Object *o = argv[0];
  
  if (!SCHEME_FALSEP(o)
      && !SCHEME_SYMBOLP(o)
      && (!SCHEME_PATHP(o)
          || !scheme_is_complete_path(SCHEME_PATH_VAL(o),
                                      SCHEME_PATH_LEN(o),
                                      SCHEME_PLATFORM_PATH_KIND)))
    return NULL;

  return o;
}

static Scheme_Object *
current_module_name_source(int argc, Scheme_Object *argv[])
{
  return scheme_param_config2("current-module-declared-name",
                              scheme_make_integer(MZCONFIG_CURRENT_MODULE_SRC),
                              argc, argv,
                              -1, source_p, 
                              "(or/c symbol? (and/c path-string? complete-path?) #f)", 
                              1);
}

static Scheme_Object *load_path_p(int argc, Scheme_Object **argv)
{
  Scheme_Object *o = argv[0];
  
  if (!SCHEME_FALSEP(o)
      && !scheme_is_module_path(o)
      && (!SCHEME_STXP(o)
          || !scheme_is_module_path(scheme_syntax_to_datum(o, 0, NULL))))
    return NULL;

  return o;
}

static Scheme_Object *
current_module_load_path(int argc, Scheme_Object *argv[])
{
  return scheme_param_config2("current-module-path-for-load",
                              scheme_make_integer(MZCONFIG_CURRENT_MODULE_LOAD_PATH),
                              argc, argv,
                              -1, load_path_p, 
                              "(or/c module-path?"
                              /**/ " (and/c syntax? (lambda (stx) (module-path? (syntax->datum stx))))"
                              /**/ " #f)",
                              1);
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
  Scheme_Object *name, *srcname, *srcmname, *fail_thunk;
  Scheme_Module *m, *srcm;
  Scheme_Env *menv, *lookup_env = NULL;
  int i, count, protected = 0;
  const char *errname;
  intptr_t base_phase;

  modname = argv[0];
  name = argv[1];
  if (argc > 2)
    fail_thunk = argv[2];
  else
    fail_thunk = NULL;

  errname = (phase 
	     ? ((phase < 0)
		? "dynamic-require-for-template" 
		: "dynamic-require-for-syntax" )
	     : "dynamic-require");

  if (SCHEME_TRUEP(name) 
      && !SCHEME_SYMBOLP(name) 
      && !SAME_OBJ(name, scheme_make_integer(0))
      && !SCHEME_VOIDP(name)) {
    scheme_wrong_contract(errname, "(or/c symbol? #f 0 void?)", 1, argc, argv);
    return NULL;
  }

  if (fail_thunk)
    scheme_check_proc_arity(errname, 0, 2, argc, argv);

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
  }

  base_phase = env->phase;

  scheme_prepare_compile_env(env);

  m = module_load(modname, env, errname);
  srcm = m;

  srcmname = NULL;
  srcname = NULL;

  if (SCHEME_SYMBOLP(name)) {
    if (mod_phase) {
      srcname = name;
      srcmname = modname;
    } else {
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
                  Scheme_Config *config;
                  Scheme_Cont_Frame_Data cframe;

                  start_module(m, env, 0, modidx, 0, 1, base_phase, scheme_null, 0);
                  ns = scheme_make_namespace(0, NULL);
                  a[0] = (Scheme_Object *)env;
                  a[1] = srcm->modname;
                  a[2] = ns;
                  namespace_attach_module(3, a);
                  a[0] = scheme_make_pair(scheme_intern_symbol("only"),
                                          scheme_make_pair(srcm->modname,
                                                           scheme_make_pair(name,
                                                                            scheme_null)));
                  do_namespace_require((Scheme_Env *)ns, 1, a, 0, 0);

                  scheme_push_continuation_frame(&cframe);
                  config = scheme_extend_config(scheme_current_config(),
                                                MZCONFIG_ENV,
                                                ns);
                  scheme_set_cont_mark(scheme_parameterization_key, 
                                       (Scheme_Object *)config);

                  ns = scheme_eval(name, (Scheme_Env *)ns);

                  scheme_pop_continuation_frame(&cframe);

                  return ns;
                } else {
                  scheme_contract_error(errname,
                                        "name is provided as syntax",
                                        "name", 1, name, 
                                        "module", 1, srcm->MODSRCNAME,
                                        NULL);
                }
              }
	      return NULL;
	    }
	  }
	}
      }

      if (i < count) {
	if (srcm->exp_infos[0]->provide_protects)
	  protected = srcm->exp_infos[0]->provide_protects[i];
	srcmname = (srcm->me->rt->provide_srcs ? srcm->me->rt->provide_srcs[i] : scheme_false);
	if (SCHEME_FALSEP(srcmname))
	  srcmname = srcm->modname;
	else {
	  srcmname = scheme_modidx_shift(srcmname, srcm->me->src_modidx, srcm->self_modidx);
	  srcmname = scheme_module_resolve(srcmname, 1);
	}
	srcname = srcm->me->rt->provide_src_names[i];
      }

      if (i == count) {
	if (indirect_ok) {
	  /* Try indirect provides: */
          Scheme_Module_Export_Info *exp_info = m->exp_infos[0];
	  srcm = m;
	  count = exp_info->num_indirect_provides;
	  if (position >= 0) {
	    i = position;
	    if ((i < exp_info->num_indirect_provides)
		&& (SCHEME_SYM_LEN(name) == SCHEME_SYM_LEN(exp_info->indirect_provides[i]))
		&& !memcmp(SCHEME_SYM_VAL(name), SCHEME_SYM_VAL(exp_info->indirect_provides[i]), SCHEME_SYM_LEN(name))) {
	      name = exp_info->indirect_provides[i];
	      srcname = name;
	      srcmname = srcm->modname;
	      if (exp_info->provide_protects)
		protected = exp_info->provide_protects[i];
	    } else
	      i = count; /* not found */
	  } else {
	    for (i = 0; i < count; i++) {
	      if (SAME_OBJ(name, exp_info->indirect_provides[i])) {
		srcname = name;
		srcmname = srcm->modname;
		if (exp_info->provide_protects)
		  protected = exp_info->provide_protects[i];
		break;
	      }
	    }
	  }
	}

	if (i == count) {
	  if (fail_with_error) {
            if (fail_thunk)
              return scheme_tail_apply(fail_thunk, 0, NULL);
	    scheme_contract_error(errname,
                                  "name is not provided",
                                  "name", 1, name, 
                                  "module", 1, srcm->MODSRCNAME,
                                  NULL);
          }
	  return NULL;
	}
      }
    }
  }

  start_module(m, env, 0, modidx, 
               (SCHEME_VOIDP(name)
                ? 1
                : (SAME_OBJ(name, scheme_make_integer(0)) 
                   ? -1
                   : 0)), 
               (SCHEME_VOIDP(name)
                ? 0
                : 1),
               base_phase, 
               scheme_null,
               0);

  if (SCHEME_SYMBOLP(name)) {
    Scheme_Bucket *b;

    menv = scheme_module_access(srcmname, lookup_env ? lookup_env : env, mod_phase);

    if (protected) {
      Scheme_Object *insp;
      insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);
      if (scheme_module_protected_wrt(menv->guard_insp, insp))
	scheme_contract_error(errname,
                              "name is protected",
                              "name", 1, name, 
                              "module", 1, srcm->MODSRCNAME,
                              NULL);
    }

    if (!menv || !menv->toplevel) {
      scheme_contract_error(errname,
                            "module inialization failed",
                            "module", 1, srcm->MODSRCNAME,
                            NULL);
    }
    
    b = scheme_bucket_from_table(menv->toplevel, (const char *)srcname);
    scheme_set_bucket_home(b, menv);

    if (get_bucket)
      return (Scheme_Object *)b;
    else {
      if (!b->val) {
        if (!menv->ran)
          run_module(menv, 1);
      }
      if (!b->val && fail_with_error) {
        if (fail_thunk)
          return scheme_tail_apply(fail_thunk, 0, NULL);
	scheme_unbound_global(b);
      }
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
                                           int copy, int etonly)
{
  Scheme_Object *form, *rns, *insp;

  if (!env)
    env = scheme_get_env(NULL);
  scheme_prepare_exp_env(env);

  if (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_module_index_type))
    form = argv[0];
  else
    form = scheme_datum_to_syntax(scheme_make_pair(require_stx,
                                                   scheme_make_pair(argv[0], scheme_null)),
                                  scheme_false, scheme_false, 1, 0);

  insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);

  rns = scheme_make_module_rename_set(mzMOD_RENAME_TOPLEVEL, NULL, insp);

  parse_requires(form, env->phase, scheme_false, env, NULL,
                 rns, NULL,
                 NULL /* ck */, NULL /* data */,
                 NULL, 
                 1, copy, 0, 
                 (etonly ? 1 : -1), !etonly,
                 NULL, NULL, NULL,
                 NULL);

  scheme_append_rename_set_to_env(rns, env);

  return scheme_void;
}

static Scheme_Object *namespace_require(int argc, Scheme_Object *argv[])
{
  return do_namespace_require(NULL, argc, argv, 0, 0);
}

Scheme_Object *scheme_namespace_require(Scheme_Object *r)
{
  Scheme_Object *a[1];
  a[0] = r;
  return namespace_require(1, a);
}

static Scheme_Object *namespace_require_copy(int argc, Scheme_Object *argv[])
{
  return do_namespace_require(NULL, argc, argv, 1, 0);
}

static Scheme_Object *namespace_require_constant(int argc, Scheme_Object *argv[])
{
  return do_namespace_require(NULL, argc, argv, 2, 0);
}

static Scheme_Object *namespace_require_etonly(int argc, Scheme_Object *argv[])
{
  return do_namespace_require(NULL, argc, argv, 0, 1);
}

static Scheme_Object *extend_list_depth(Scheme_Object *l, Scheme_Object *n, int with_ht)
{
  Scheme_Object *p, *orig;
  int k;

  if (!SCHEME_INTP(n))
    scheme_raise_out_of_memory(NULL, NULL);

  k = SCHEME_INT_VAL(n);

  if (SCHEME_NULLP(l)) {
    if (with_ht)
      p = (Scheme_Object *)scheme_make_hash_table(SCHEME_hash_ptr);
    else
      p = scheme_null;
    l = scheme_make_pair(p, scheme_null);
  }
   
  orig = l;
  
  while (k--) {
    if (SCHEME_NULLP(SCHEME_CDR(l))) {
      if (with_ht)
        p = (Scheme_Object *)scheme_make_hash_table(SCHEME_hash_ptr);
      else
        p = scheme_null;
      p = scheme_make_pair(p, scheme_null);
      SCHEME_CDR(l) = p;
    }
    l = SCHEME_CDR(l);
  }

  return orig;
}

static Scheme_Object *extract_at_depth(Scheme_Object *l, Scheme_Object *n)
{
  int k = SCHEME_INT_VAL(n);

  while (k--) {
    l = SCHEME_CDR(l);
  }

  return SCHEME_CAR(l);
}

static void set_at_depth(Scheme_Object *l, Scheme_Object *n, Scheme_Object *v)
{
  int k = SCHEME_INT_VAL(n);

  while (k--) {
    l = SCHEME_CDR(l);
  }
  
  SCHEME_CAR(l) = v;
}

#if 0
static void check_phase(Scheme_Env *menv, Scheme_Env *env, int phase)
{
  if (env && (env->exp_env == env)) {
    /* label phase */
    return;
  }

  if (!menv->module->primitive 
      && ((env && (menv->phase != env->phase))
          || (!env && (menv->phase != phase)))) {
    fprintf(stderr, "phase mismatch\n");
  }
}

static void check_modchain_consistency(Scheme_Hash_Table *ht, int phase)
{
  int i;

  for (i = ht->size; i--; ) {
    if (ht->vals[i]) {
      check_phase((Scheme_Env *)ht->vals[i], NULL, phase);
    }
  }
}
#else
static void check_phase(Scheme_Env *menv, Scheme_Env *env, int phase) { }
static void check_modchain_consistency(Scheme_Hash_Table *ht, int phase) { }
#endif

void ensure_instantiate_for_label(const char *who, Scheme_Env *from_env, Scheme_Object *name, Scheme_Object *modidx)
{
  Scheme_Module *m2;

  m2 = registry_get_loaded(from_env, name);
  if (!m2)
    scheme_contract_error(who,
                          "module not declared (in the source namespace)",
                          "name", 1, name,
                          NULL);
  else {
    /* instantiate for-label: */
    Scheme_Cont_Frame_Data cframe;
    Scheme_Config *config;

    /* make sure `from_env' is the current namespace, because
       start_module() may need to resolve module paths: */
    config = scheme_extend_config(scheme_current_config(),
                                  MZCONFIG_ENV,
                                  (Scheme_Object *)from_env);
    scheme_push_continuation_frame(&cframe);
    scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);

    start_module(m2, 
                 from_env->label_env, 0, 
                 modidx, 
                 0, 0, -1,
                 scheme_null,
                 0);

    scheme_pop_continuation_frame(&cframe);
  } 
}

static Scheme_Object *make_sub_modidx_pair(Scheme_Env *menv, Scheme_Object *name, int i)
{
  Scheme_Object *modidx;

  if (i) {
    name = scheme_resolved_module_path_value(name);
    while (SCHEME_PAIRP(SCHEME_CDR(name))) {
      name = SCHEME_CDR(name);
    }
    name = SCHEME_CAR(name);
  } else {
    name = scheme_make_utf8_string("..");
  }
  
  modidx = scheme_make_modidx(scheme_make_pair(submod_symbol,
                                               scheme_make_pair(scheme_make_utf8_string("."),
                                                                scheme_make_pair(name,
                                                                                 scheme_null))),
                              menv->link_midx,
                              scheme_false);
  name = scheme_module_resolve(modidx, 0);
  
  return scheme_make_pair(name, modidx);
}

#if 0
# define LOG_ATTACH(x) (x, fflush(stdout))
#else
# define LOG_ATTACH(x) /* nothing */
#endif

static Scheme_Object *do_namespace_attach_module(const char *who, int argc, Scheme_Object *argv[], 
                                                 int only_declare)
{
  Scheme_Env *from_env, *to_env, *menv, *menv2;
  Scheme_Object *todo, *next_phase_todo, *prev_phase_todo;
  Scheme_Object *name, *notifies = scheme_null, *a[2], *resolver;
  Scheme_Object *to_modchain, *from_modchain, *l, *main_modidx;
  Scheme_Hash_Table *checked, *next_checked, *prev_checked;
  Scheme_Object *past_checkeds, *future_checkeds, *future_todos, *past_to_modchains, *past_todos;
  Scheme_Module *m2;
  int same_namespace, set_env_for_notify = 0, phase, orig_phase, max_phase;
  int just_declare;
  Scheme_Object *nophase_todo;
  Scheme_Hash_Table *nophase_checked;

  if (!SCHEME_NAMESPACEP(argv[0]))
    scheme_wrong_contract(who, "namespace?", 0, argc, argv);
  from_env = (Scheme_Env *)argv[0];

  if (argc > 2) {
    if (!SCHEME_NAMESPACEP(argv[2]))
      scheme_wrong_contract(who, "namespace?", 2, argc, argv);
    to_env = (Scheme_Env *)argv[2];
    set_env_for_notify = 1;
  } else
    to_env = scheme_get_env(NULL);

  same_namespace = SAME_OBJ(from_env, to_env);

  if (from_env->phase != to_env->phase) {
    scheme_contract_error("namespace-attach-module",
                          "source and destination namespace phases do not match",
                          "source phase", 1, scheme_make_integer(from_env->phase),
                          "destination phase", 1, scheme_make_integer(to_env->phase),
                          NULL);
  }

  main_modidx = scheme_make_modidx(argv[1], scheme_false, scheme_false);
  name = scheme_module_resolve(main_modidx, 0);

  if (!only_declare) {
    todo = scheme_make_pair(name, scheme_null);
    nophase_todo = scheme_null;
  } else {
    todo = scheme_null;
    nophase_todo = scheme_make_pair(name, scheme_null);
  }

  next_phase_todo = scheme_null;
  prev_phase_todo = scheme_null;
  from_modchain = from_env->modchain;
  to_modchain = to_env->modchain;
  phase = from_env->phase;
  orig_phase = phase;

  checked = NULL;
  next_checked = NULL;
  prev_checked = NULL;

  past_checkeds = scheme_null;
  past_todos = scheme_null;
  future_checkeds = scheme_null;
  future_todos = scheme_null;
  past_to_modchains = scheme_null;

  nophase_checked = scheme_make_hash_table(SCHEME_hash_ptr);
  if (only_declare) {
    scheme_hash_set(nophase_checked, name, scheme_false);
  }

  max_phase = phase;

  checked = scheme_make_hash_table(SCHEME_hash_ptr);
  scheme_hash_set(checked, name, scheme_true);
  
  just_declare = 0;

  /* Check whether todo, or anything it needs, is already declared
     incompatibly. Successive iterations of the outer loop explore
     successive phases (i.e, for-syntax levels). */
  while (!SCHEME_NULLP(todo)) {
    if (phase > max_phase)
      max_phase = phase;
    if (phase < orig_phase) {
      /* As soon as we start traversing negative phases, stop transferring
         instances (i.e., transfer declarations only). This transfer-only
         mode should stick even even if we go back into positive phases. */
      just_declare = 1;
    }

    if (!checked)
      checked = scheme_make_hash_table(SCHEME_hash_ptr);
    /* This is just a shortcut: */
    if (!next_checked)
      next_checked = scheme_make_hash_table(SCHEME_hash_ptr);

    /* This loop iterates through require chains in the same phase */
    while (!SCHEME_NULLP(todo)) {
      name = SCHEME_CAR(todo);

      todo = SCHEME_CDR(todo);

      if (!scheme_hash_get(checked, name)) {
        scheme_signal_error("internal error: module not in `checked' table");
      }

      if (!is_builtin_modname(name)) {
	LOG_ATTACH(printf("Check %d %s\n", phase, scheme_write_to_string(name, 0)));

	menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(from_modchain), name);
	
	if (!menv) {
	  /* Assert: name == argv[1] */
	  /* Module at least declared? */
          if (registry_get_loaded(from_env, name))
            scheme_contract_error(who,
                                  "module not instantiated (in the source namespace)",
                                  "name", 1, name,
                                  NULL);
          else
            scheme_contract_error(who,
                                  "unknown module (in the source namespace)",
                                  "name", 1, name,
                                  NULL);
	}

        /* If to_modchain goes to #f, then our source check has gone
	   deeper in phases (for-syntax levels) than the target
	   namespace has ever gone, so there's definitely no conflict
	   at this level in that case. */
	if ((phase >= orig_phase) && SCHEME_TRUEP(to_modchain)) {
	  menv2 = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(to_modchain), name);
	  if (menv2) {
	    if (!SAME_OBJ(menv->toplevel, menv2->toplevel))
	      m2 = menv2->module;
	    else
	      m2 = NULL;
	  } else {
	    m2 = registry_get_loaded(to_env, name);
	    if (m2 && SAME_OBJ(m2, menv->module))
	      m2 = NULL;
	  }

          if (m2 && (phase > orig_phase) && SAME_OBJ(menv->module, m2)) {
            /* different instance of same module is ok at higher phases */
            m2 = NULL;
          }
	  
	  if (m2) {
	    char *phase, buf[32], *kind;

	    if (!menv->phase)
	      phase = "";
	    else if (menv->phase == 1)
	      phase = "  phase: for syntax\n";
	    else {
	      sprintf(buf, "  phase: %" PRIdPTR "\n", menv->phase);
	      phase = buf;
	    }

            if (SAME_OBJ(menv->module, m2))
              kind = "instance of the same module";
            else
              kind = "module with the same name";

	    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
			     "namespace-attach-module: "
			     "a different %s is already "
			     "in the destination namespace\n"
                             "%s"
                             "  module name: %D",
			     kind, phase, name);
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
              LOG_ATTACH(printf("Add %d %s (%p)\n", phase, scheme_write_to_string(name, 0), checked));
	      todo = scheme_make_pair(name, todo);
	      scheme_hash_set(checked, name, just_declare ? scheme_false : scheme_true);
	    }
	    l = SCHEME_CDR(l);
	  }

          /* was here */

	  l = menv->et_require_names;
	  while (!SCHEME_NULLP(l)) {
	    name = scheme_module_resolve(SCHEME_CAR(l), 0);
	    if (!scheme_hash_get(next_checked, name)) {
	      LOG_ATTACH(printf("Add +%d %s (%p)\n", phase+1, scheme_write_to_string(name, 0), next_checked));
	      next_phase_todo = scheme_make_pair(name, next_phase_todo);
	      scheme_hash_set(next_checked, name, just_declare ? scheme_false : scheme_true);
	    }
	    l = SCHEME_CDR(l);
	  }

          l = menv->tt_require_names;
          if (l) {
            while (!SCHEME_NULLP(l)) {
              name = scheme_module_resolve(SCHEME_CAR(l), 0);
              if (!prev_checked)
                prev_checked = scheme_make_hash_table(SCHEME_hash_ptr);
              if (!scheme_hash_get(prev_checked, name)) {
                LOG_ATTACH(printf("Add -%d %s (%p)\n", phase-1, scheme_write_to_string(name, 0), prev_checked));
                prev_phase_todo = scheme_make_pair(name, prev_phase_todo);
                scheme_hash_set(prev_checked, name, just_declare ? scheme_false : scheme_true);
              }
              l = SCHEME_CDR(l);
            }
	  }

          if (!same_namespace) {
            l = menv->dt_require_names;
            if (l) {
              while (!SCHEME_NULLP(l)) {
                name = scheme_module_resolve(SCHEME_CAR(l), 0);
                
                if (!scheme_hash_get(nophase_checked, name)) {
                  LOG_ATTACH(printf("Add * %s\n", scheme_write_to_string(name, NULL)));
                  nophase_todo = scheme_make_pair(name, nophase_todo);
                  scheme_hash_set(nophase_checked, name, just_declare ? scheme_false : scheme_true);
                }
                l = SCHEME_CDR(l);
              }
            }
          }

          if (menv->other_require_names) {
            Scheme_Hash_Table *oht;
            int i;
            oht = menv->other_require_names;
            for (i = 0; i < oht->size; i++) {
              if (oht->vals[i]) {
                Scheme_Object *lphase = oht->keys[i];
                Scheme_Object *l = oht->vals[i], *todos, *checkeds;

                if (scheme_is_negative(lphase)) {
                  lphase = scheme_bin_minus(scheme_make_integer(0), lphase);
                  lphase = scheme_bin_minus(lphase, scheme_make_integer(2));
                  past_todos = extend_list_depth(past_todos, lphase, 0);
                  past_checkeds = extend_list_depth(past_checkeds, lphase, 1);
                  todos = past_todos;
                  checkeds = past_checkeds;
                } else {
                  lphase = scheme_bin_minus(lphase, scheme_make_integer(2));
                  future_todos = extend_list_depth(future_todos, lphase, 0);
                  future_checkeds = extend_list_depth(future_checkeds, lphase, 1);
                  todos = future_todos;
                  checkeds = future_checkeds;
                }
                if (todos) {
                  Scheme_Object *a_todo;
                  Scheme_Hash_Table *a_checked;
                  
                  a_todo = extract_at_depth(todos, lphase);
                  a_checked = (Scheme_Hash_Table *)extract_at_depth(checkeds, lphase);
                  
                  while (!SCHEME_NULLP(l)) {
                    name = scheme_module_resolve(SCHEME_CAR(l), 0);
                    if (!scheme_hash_get(a_checked, name)) {
                      LOG_ATTACH(printf("Add +%ld %s (%p)\n", 
                                        SCHEME_INT_VAL(oht->keys[i]), 
                                        scheme_write_to_string(name, 0), a_checked));
                      a_todo = scheme_make_pair(name, a_todo);
                      scheme_hash_set(a_checked, name, just_declare ? scheme_false : scheme_true);
                    }
                    l = SCHEME_CDR(l);
                  }
                  
                  set_at_depth(todos, lphase, a_todo);
                }
              }
            }
          }

          if (!same_namespace) {
            /* attached submodules: like for-label imports: */
            int i;
            for (i = 0; i < 3; i++) {
              switch (i) {
              case 0:
                if (menv->module->supermodule)
                  l = scheme_make_pair(menv->module->supermodule, scheme_null);
                else
                  l = scheme_null;
                break;
              case 1:
                l = menv->module->post_submodules;
                break;
              case 2:
              default:
                l = menv->module->pre_submodules;
                break;
              }
              if (l) {
                while (!SCHEME_NULLP(l)) {
                  name = ((Scheme_Module *)SCHEME_CAR(l))->modname;
                  
                  if (!scheme_hash_get(nophase_checked, name)) {
                    name = make_sub_modidx_pair(menv, name, i);
                    LOG_ATTACH(printf("Add s %s\n", scheme_write_to_string(SCHEME_CAR(name), NULL)));
                    nophase_todo = scheme_make_pair(name, nophase_todo);
                    scheme_hash_set(nophase_checked, SCHEME_CAR(name), just_declare ? scheme_false : scheme_true);
                  }
                  l = SCHEME_CDR(l);
                }
              }
            }
          }
        }
      }
    }

    do {
      if (!SCHEME_PAIRP(next_phase_todo)) {
        /* Work on earlier phase */
        LOG_ATTACH(printf("prev\n"));
	future_todos = cons(next_phase_todo, future_todos);
        next_phase_todo = todo;
	future_checkeds = cons((Scheme_Object *)next_checked, future_checkeds);
	next_checked = checked;
	
	todo = prev_phase_todo;
        checked = prev_checked;

        if (SCHEME_NULLP(past_todos)) {
          prev_phase_todo = scheme_null;
          prev_checked = NULL;
        } else {
          prev_phase_todo = SCHEME_CAR(past_todos);
	  past_todos = SCHEME_CDR(past_todos);
          prev_checked = (Scheme_Hash_Table *)SCHEME_CAR(past_checkeds);
	  past_checkeds = SCHEME_CDR(past_checkeds);
        }
	
	from_modchain = SCHEME_VEC_ELS(from_modchain)[2];
        if (phase > orig_phase) {
          to_modchain = SCHEME_CAR(past_to_modchains);
          past_to_modchains = SCHEME_CDR(past_to_modchains);
        }
	phase--;
      } else {
        /* Work on later phase */
        LOG_ATTACH(printf("later\n"));
        past_todos = cons(prev_phase_todo, past_todos);
        prev_phase_todo = todo;
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
        if (phase >= orig_phase) {
          past_to_modchains = cons(to_modchain, past_to_modchains);
          if (SCHEME_TRUEP(to_modchain))
            to_modchain = SCHEME_VEC_ELS(to_modchain)[1];
        }
	phase++;
      }
    } while (SCHEME_NULLP(todo) && (SCHEME_PAIRP(prev_phase_todo)
				    || SCHEME_PAIRP(past_todos)));
  }

  LOG_ATTACH(printf("Done phase: %d\n", phase));

  if (SCHEME_PAIRP(nophase_todo) && !from_env->label_env)
    scheme_signal_error("internal error: missing label environment");

  /* Recursively process phase-#f modules: */
  while (!SCHEME_NULLP(nophase_todo)) {
    int is_submod;

    name = SCHEME_CAR(nophase_todo);
    if (SCHEME_PAIRP(name)) {
      is_submod = 1;
      main_modidx = SCHEME_CDR(name);
      name = SCHEME_CAR(name);
    } else
      is_submod = 0;
    nophase_todo = SCHEME_CDR(nophase_todo);      

    if (!is_builtin_modname(name)) {
      int i;

      menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(from_env->label_env->modchain), name);
    
      LOG_ATTACH(printf("Check #f %s\n", scheme_write_to_string(name, 0)));
    
      if (!menv) {
        if ((only_declare || is_submod) && main_modidx) {
          ensure_instantiate_for_label(who, from_env, name, main_modidx);
          /* try again: */
          menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(from_env->label_env->modchain), name);
        }
        
        if (!menv)
          scheme_arg_mismatch(who,
                              "internal error; unknown module (for label): ",
                              name);
      }

      main_modidx = NULL;

      menv2 = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(to_modchain), name);
      m2 = registry_get_loaded(to_env, name);
      if (m2 && !SAME_OBJ(m2, menv->module)) {
        const char * kind = "module with the same name";
        const char * phase = "";
        scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                         "namespace-attach-module: "
                         "a different %s is already "
                         "in the destination namespace\n"
                         "%s"
                         "  module name: %D",
                         kind, phase, name);
      }
    
      for (i = -4; 
           i < (menv->other_require_names ? menv->other_require_names->size : 0); 
           i++) {
        switch (i) {
        case -4:
          l = menv->require_names;
          break;
        case -3:
          l = menv->et_require_names;
          break;
        case -2:
          l = menv->tt_require_names;
          break;
        case -1:
          l = menv->dt_require_names;
          break;
        default:
          l = menv->other_require_names->vals[i];
          break;
        }
      
        if (l) {
          while (!SCHEME_NULLP(l)) {
            name = scheme_module_resolve(SCHEME_CAR(l), 0);
            if (!scheme_hash_get(nophase_checked, name)) {
              LOG_ATTACH(printf("Add .* %s\n", scheme_write_to_string(name, 0)));
              nophase_todo = scheme_make_pair(name, nophase_todo);
              scheme_hash_set(nophase_checked, name, scheme_true);
            }
            l = SCHEME_CDR(l);
          }
        }
      }

      for (i = 0; i < 3; i++) {
        switch (i) {
        case 0:
          if (menv->module->supermodule)
            l = scheme_make_pair(menv->module->supermodule, scheme_null);
          else
            l = scheme_null;
          break;
        case 1:
          l = menv->module->post_submodules;
          break;
        case 2:
        default:
          l = menv->module->pre_submodules;
          break;
        }
        
        if (l) {
          while (!SCHEME_NULLP(l)) {
            name = ((Scheme_Module *)SCHEME_CAR(l))->modname;
                  
            if (!scheme_hash_get(nophase_checked, name)) {
              name = make_sub_modidx_pair(menv, name, i);
              LOG_ATTACH(printf("Add s %s\n", scheme_write_to_string(SCHEME_CAR(name), NULL)));
              nophase_todo = scheme_make_pair(name, nophase_todo);
              scheme_hash_set(nophase_checked, SCHEME_CAR(name), just_declare ? scheme_false : scheme_true);
            }
            l = SCHEME_CDR(l);
          }
        }
      }
    }
  }
  
  /* All of the modules that we saw are in the ***_checked hash tables */
  if (prev_checked) {
    past_checkeds = cons((Scheme_Object *)prev_checked, past_checkeds);
  }
  if (!only_declare){
    if (!checked)
      checked = scheme_make_hash_table(SCHEME_hash_ptr);
    past_checkeds = cons((Scheme_Object *)checked, past_checkeds);
  }

  if (phase < max_phase) {
    past_checkeds = cons((Scheme_Object *)next_checked, past_checkeds);
    phase++;
  }
  while (phase < max_phase) {
    next_checked = (Scheme_Hash_Table *)SCHEME_CAR(future_checkeds);
    past_checkeds = scheme_make_raw_pair((Scheme_Object *)next_checked, past_checkeds);
    
    future_checkeds = SCHEME_CDR(future_checkeds);
    phase++;
  }
  /* Now all the modules to check are in the past_checkeds
     list of hash tables. */

  /* Transfers phase-#f modules first. */
  {
    int i;
    Scheme_Hash_Table *ht;
    
    scheme_prepare_label_env(to_env);

    ht = nophase_checked;
    for (i = ht->size; i--; ) {
      if (ht->vals[i]) {
        name = ht->keys[i];
        
        if (!is_builtin_modname(name)) {

          LOG_ATTACH(printf("Copying no-phase %s\n", scheme_write_to_string(name, NULL)));
          
          m2 = registry_get_loaded(from_env, name);
          scheme_hash_set(to_env->module_registry->loaded, name, (Scheme_Object *)m2);

          menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(from_env->label_env->modchain), name);
          menv2 = scheme_copy_module_env(menv, to_env->label_env, to_env->label_env->modchain, menv->phase + 1);
          check_phase(menv2, to_env->label_env, 0);
          scheme_hash_set(MODCHAIN_TABLE(to_env->label_env->modchain), name, (Scheme_Object *)menv2);

          if (menv->attached)
            menv2->attached = 1;

          /* Push name onto notify list: */
          if (!same_namespace)
            notifies = scheme_make_pair(name, notifies);
        }
      }
    }
  }

  /* Get modchain at `phase': */
  {
    int i;
    Scheme_Env *te = to_env;
    from_modchain = from_env->modchain;
    to_modchain = to_env->modchain;
    for (i = from_env->phase; i < phase; i++) {
      from_modchain = SCHEME_VEC_ELS(from_modchain)[1];

      scheme_prepare_exp_env(te);
      te = te->exp_env;
      to_modchain = SCHEME_VEC_ELS(to_modchain)[1];
    }
  }

  /* Go through that list, this time tranferring module instances. */
  /* Again, outer loop iterates through phases. */
  while (!SCHEME_NULLP(past_checkeds)) {
    /* Inner loop iterates through requires within a phase. */
    int i;

    checked = (Scheme_Hash_Table *)SCHEME_CAR(past_checkeds);

    LOG_ATTACH(printf("Copying %d (%p)\n", phase, checked));

    if (phase >= orig_phase)
      check_modchain_consistency(MODCHAIN_TABLE(to_modchain), phase);

    for (i = checked->size; i--; ) {
      if (checked->vals[i]) {
	name = checked->keys[i];
        just_declare = SCHEME_FALSEP(checked->vals[i]);

	if (!is_builtin_modname(name)) {
	  menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(from_modchain), name);
	  
	  LOG_ATTACH(printf("Copy %d %s\n", phase, scheme_write_to_string(name, 0)));

	  menv2 = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(to_modchain), name);
	  if (!menv2) {
	    /* Clone/copy menv for the new namespace: */
            if ((phase >= orig_phase) && !just_declare) {
              menv2 = scheme_copy_module_env(menv, to_env, to_modchain, orig_phase);
              if (menv->attached)
                menv2->attached = 1;
              
              check_phase(menv2, NULL, phase);
              scheme_hash_set(MODCHAIN_TABLE(to_modchain), name, (Scheme_Object *)menv2);
            }
	    scheme_hash_set(to_env->module_registry->loaded, name, (Scheme_Object *)menv->module);
	    scheme_hash_set(to_env->module_registry->exports, name, (Scheme_Object *)menv->module->me);
	    
	    /* Push name onto notify list: */
	    if (!same_namespace)
	      notifies = scheme_make_pair(name, notifies);
	  }
	}
      }
    }
    
    past_checkeds = SCHEME_CDR(past_checkeds);
    from_modchain = SCHEME_VEC_ELS(from_modchain)[2];
    if (phase > orig_phase)
      to_modchain = SCHEME_VEC_ELS(to_modchain)[2];   
    --phase;
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
      a[1] = (Scheme_Object *)from_env;

      scheme_apply(resolver, 2, a);
      
      notifies = SCHEME_CDR(notifies);
    }

    if (set_env_for_notify) {
      scheme_pop_continuation_frame(&cframe);
    }
  }

  return scheme_void;
}

static Scheme_Object *namespace_attach_module(int argc, Scheme_Object *argv[])
{
  return do_namespace_attach_module("namespace-attach-module", argc, argv, 0);
}

static Scheme_Object *namespace_attach_module_decl(int argc, Scheme_Object *argv[])
{
  return do_namespace_attach_module("namespace-attach-module-declaration", argc, argv, 1);
}

static Scheme_Object *namespace_unprotect_module(int argc, Scheme_Object *argv[])
{
  Scheme_Env *to_env, *menv2;
  Scheme_Object *name, *to_modchain, *insp, *code_insp;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_inspector_type))
    scheme_wrong_contract("namespace-unprotect-module", "inspector?", 0, argc, argv);

  insp = argv[0];
  if (argc > 2)
    to_env = (Scheme_Env *)argv[2];
  else
    to_env = scheme_get_env(NULL);

  name = scheme_module_resolve(scheme_make_modidx(argv[1], scheme_false, scheme_false), 0);

  to_modchain = to_env->modchain;

  code_insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);

  if (!SAME_OBJ(name, kernel_modname)
      && !SAME_OBJ(name, flfxnum_modname)
      && !SAME_OBJ(name, extfl_modname)
      && !SAME_OBJ(name, futures_modname)) {
    if (SAME_OBJ(name, unsafe_modname))
      menv2 = scheme_get_unsafe_env();
    else
      menv2 = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(to_modchain), name);

    if (!menv2) {
      scheme_contract_error("namespace-unprotect-module",
                            "module not instantiated (in the target namespace)",
                            "name", 1, name,
                            NULL);
    }

    if (!scheme_module_protected_wrt(menv2->guard_insp, insp) && !menv2->attached) {
      code_insp = scheme_make_inspector(code_insp);
      menv2->guard_insp = code_insp;
    }
  }

  return scheme_void;
}

static int plain_char(int c)
{
  return (((c >= 'a') && (c <= 'z'))
          || ((c >= 'A') && (c <= 'Z'))
          || ((c >= '0') && (c <= '9'))
          || (c == '-')
          || (c == '_')
          || (c == '+'));
}

static int ok_hex(int c)
{
  return (((c >= 'a') && (c <= 'f'))
          || ((c >= '0') && (c <= '9')));
}

static int ok_escape(int c1, int c2)
{
  c1 = (((c1 >= 'a') && (c1 <= 'f'))
        ? (c1 - 'a' + 10)
        : (c1 - '0'));
  c2 = (((c2 >= 'a') && (c2 <= 'f'))
        ? (c2 - 'a' + 10)
        : (c2 - '0'));

  c1 = (c1 << 4) + c2;
  
  if (plain_char(c1))
    return 0;
  else
    return 1;
}

static int ok_path_string(Scheme_Object *obj, int dir_ok, int just_file_ok, int file_end_ok, int for_planet)
{
  mzchar *s = SCHEME_CHAR_STR_VAL(obj);
  int i = SCHEME_CHAR_STRLEN_VAL(obj), c, start_package_pos = 0, end_package_pos = 0;
  int prev_was_slash = 0, saw_slash = !file_end_ok, saw_dot = 0;

  if (!i)
    return 0;
  if (s[0] == '/')
    return 0;
  if (s[i - 1] == '/')
    return 0;

  if (for_planet) {
    /* Must have at least two slashes, and a version spec is allowed between them */
    int j, counter = 0, colon1_pos = 0, colon2_pos = 0;
    for (j = 0; j < i; j++) {
      c = s[j];
      if (c == '/') {
        counter++;
        if (counter == 1)
          start_package_pos = j + 1;
        else if (counter == 2)
          end_package_pos = j;
      } else if (c == ':') {
        if (counter == 1) {
          if (colon2_pos)
            return 0;
          else if (colon1_pos)
            colon2_pos = j;
          else
            colon1_pos = j;
        }
      }
    }

    if (counter == 1)
      end_package_pos = i;

    if (end_package_pos <= start_package_pos)
      return 0;

    if (colon1_pos) {
      /* Check that the version spec is well-formed, leaving the rest to the loop below */
      int colon1_end = (colon2_pos ? colon2_pos : end_package_pos);
      
      if (colon1_end == (colon1_pos + 1))
        return 0;
      for (j = colon1_pos + 1; j < colon1_end; j++) {
        c = s[j];
        if (!((c >= '0') && (c <= '9')))
          return 0;
      }

      if (colon2_pos) {
        colon2_pos++;
        c = s[colon2_pos];
        if ((c == '<') || (c == '>')) {
          if (s[colon2_pos+1] == '=')
            colon2_pos += 2;
          else
            return 0;
        } else if (c == '=') {
          colon2_pos += 1;
        } else {
          if ((c >= '0') && (c <= '9')) {
            /* check for range: */
            for (j = colon2_pos; j < end_package_pos; j++) {
              if (s[j] == '-') {
                colon2_pos = j + 1;
                break;
              } else if (!((c >= '0') && (c <= '9')))
                return 0;
            }
          }
        }
        if (end_package_pos == colon2_pos)
          return 0;
        
        for (j = colon2_pos; j < end_package_pos; j++) {
          c = s[j];
          if (!((c >= '0') && (c <= '9')))
            return 0;
        }
      }

      /* tell loop below to ignore the version part: */
      start_package_pos = colon1_pos;
    } else {
      /* package must have normal directory syntax */
      start_package_pos = end_package_pos = 0;
    }
  }

  while (i--) {
    c = s[i];
    if (c == '/') {
      saw_slash = 1;
      if (prev_was_slash)
        return 0;
      prev_was_slash = 1;
    } else if (c == '.') {
      if (s[i+1] && (s[i+1] != '/') && (s[i+1] != '.')) {
        if (saw_slash) {
          /* can't have suffix on a directory */
          return 0;
        }
        saw_dot = 1;
      }
      prev_was_slash = 0;
    } else {
      if (plain_char(c)
          || ((c == '%')
              && ok_hex(s[i+1])
              && ok_hex(s[i+2])
              && ok_escape(s[i+1], s[i+2]))) {
        prev_was_slash = 0;
      } else if ((i < start_package_pos) || (i >= end_package_pos))
        return 0;
      else {
        prev_was_slash = 0;
      }
    }
  }

  if (!just_file_ok) {
    if (saw_dot && !saw_slash) {
      /* can't have a file name with no directory */
      return 0;
    }
  }

  if (!dir_ok) {
    for (i = 0; s[i]; i++) {
      if (s[i] == '.') {
        if (!s[i+1] || (s[i+1] == '/'))
          return 0;
        if (s[i+1] == '.')
          if (!s[i+2] || (s[i+2] == '/'))
            return 0;
        while (s[i] == '.') {
          i++;
        }
      }
    }
  }

  return 1;
}

static int ok_planet_number(Scheme_Object *a)
{
  if (SCHEME_INTP(a)) {
    if (SCHEME_INT_VAL(a) >= 0)
      return 1;
  } else if (SCHEME_BIGNUMP(a)) {
    if (SCHEME_BIGPOS(a))
      return 1;
  }
  return 0;
}


static int ok_planet_string(Scheme_Object *obj)
{
  mzchar *s;
  int i, c;

  if (!SCHEME_CHAR_STRINGP(obj))
    return 0;

  s  = SCHEME_CHAR_STR_VAL(obj);
  i = SCHEME_CHAR_STRLEN_VAL(obj);

  if (!i)
    return 0;

  while (i--) {
    c = s[i];
    if ((c == '%')
        && ok_hex(s[i+1])
        && ok_hex(s[i+2])
        && ok_escape(s[i+1], s[i+2])) {
      /* ok */
    } else if (plain_char(c) || (c == '.')) {
      /* ok */
    } else
      return 0;
  }

  return 1;
}

int scheme_is_module_path(Scheme_Object *obj)
{
  if (SCHEME_PAIRP(obj)
      && (SAME_OBJ(SCHEME_CAR(obj), submod_symbol))) {
    Scheme_Object *p, *a;
    int len = 0;
    p = SCHEME_CDR(obj);
    if (SCHEME_PAIRP(p)) {
      p = SCHEME_CDR(p);
      while (SCHEME_PAIRP(p)) {
        len++;
        a = SCHEME_CAR(p);
        if (!SCHEME_SYMBOLP(a)
            && (!SCHEME_CHAR_STRINGP(a)
                || (SCHEME_CHAR_STRLEN_VAL(a) != 2)
                || (SCHEME_CHAR_STR_VAL(a)[0] != '.')
                || (SCHEME_CHAR_STR_VAL(a)[1] != '.')))
          break;
        p = SCHEME_CDR(p);
      }
    } else
      p = scheme_false;
    if (SCHEME_NULLP(p)) {
      obj = SCHEME_CDR(obj);
      obj = SCHEME_CAR(obj);
      if (SCHEME_CHAR_STRINGP(obj) 
          && (((SCHEME_CHAR_STRLEN_VAL(obj) == 1)
               && (SCHEME_CHAR_STR_VAL(obj)[0] == '.'))
              || ((SCHEME_CHAR_STRLEN_VAL(obj) == 2)
                  && (SCHEME_CHAR_STR_VAL(obj)[0] == '.')
                  && (SCHEME_CHAR_STR_VAL(obj)[1] == '.'))))
        return 1;
    }
  }

  if (SCHEME_PATHP(obj))
    return 1;

  if (SCHEME_CHAR_STRINGP(obj)) {
    return ok_path_string(obj, 1, 1, 1, 0);
  }

  if (SCHEME_SYMBOLP(obj)) {
    obj = scheme_make_sized_offset_utf8_string((char *)(obj),
                                               SCHEME_SYMSTR_OFFSET(obj),
                                               SCHEME_SYM_LEN(obj));
    return ok_path_string(obj, 0, 0, 0, 0);
  }

  if (SCHEME_PAIRP(obj)) {
    if (SAME_OBJ(SCHEME_CAR(obj), quote_symbol)) {
      obj = SCHEME_CDR(obj);
      if (SCHEME_PAIRP(obj)) {
        if (SCHEME_NULLP(SCHEME_CDR(obj))) {
          obj = SCHEME_CAR(obj);
          return SCHEME_SYMBOLP(obj);
      } else
          return 0;
      } else
        return 0;
    } else if (SAME_OBJ(SCHEME_CAR(obj), lib_symbol)) {
      obj = SCHEME_CDR(obj);
      if (SCHEME_PAIRP(obj)) {
        Scheme_Object *a;
        int is_first = 1;
        while (SCHEME_PAIRP(obj)) {
          a = SCHEME_CAR(obj);
          if (SCHEME_CHAR_STRINGP(a)) {
            if (!ok_path_string(a, 0, is_first, is_first, 0))
              return 0;
          } else
            return 0;
          obj = SCHEME_CDR(obj);
          is_first = 0;
        }
        if (SCHEME_NULLP(obj))
          return 1;
        else
          return 0;
      } else
        return 0;
    } else if (SAME_OBJ(SCHEME_CAR(obj), file_symbol)) {
      obj = SCHEME_CDR(obj);
      if (SCHEME_PAIRP(obj) && SCHEME_NULLP(SCHEME_CDR(obj))) {
        int i;
        mzchar *s;
        obj = SCHEME_CAR(obj);
        if (!SCHEME_CHAR_STRINGP(obj))
          return 0;
        s = SCHEME_CHAR_STR_VAL(obj);
        i = SCHEME_CHAR_STRLEN_VAL(obj);
        if (!i)
          return 0;
        while (i--) {
          if (!s[i])
            return 0;
        }
        return 1;
      }
    } else if (SAME_OBJ(SCHEME_CAR(obj), planet_symbol)) {
      Scheme_Object *a, *subs;
      int len, counter;

      len = scheme_proper_list_length(obj);

      if (len == 2) {
        /* Symbolic or string shorthand? */
        obj = SCHEME_CDR(obj);
        a = SCHEME_CAR(obj);
        if (SCHEME_SYMBOLP(a)) {
          obj = scheme_make_sized_offset_utf8_string((char *)(a),
                                                     SCHEME_SYMSTR_OFFSET(a),
                                                     SCHEME_SYM_LEN(a));
          return ok_path_string(obj, 0, 0, 0, 1);
        } else if (SCHEME_CHAR_STRINGP(a)) {
          return ok_path_string(a, 0, 0, 1, 1);
        }
      }

      if (len < 3)
        return 0;
      obj = SCHEME_CDR(obj);
      a = SCHEME_CAR(obj);
      if (!SCHEME_CHAR_STRINGP(a))
        return 0;
      if (!ok_path_string(a, 0, 1, 1, 0))
        return 0;
      obj = SCHEME_CDR(obj);
      subs = SCHEME_CDR(obj);
      obj = SCHEME_CAR(obj);
      len = scheme_proper_list_length(obj);
      if (len < 2)
        return 0;

      a = SCHEME_CAR(obj);
      if (!ok_planet_string(a))
        return 0;

      obj = SCHEME_CDR(obj);
      a = SCHEME_CAR(obj);
      if (!ok_planet_string(a))
        return 0;

      /* planet allows a major and minor version number: */
      counter = 0;
      for (obj = SCHEME_CDR(obj); !SCHEME_NULLP(obj); obj = SCHEME_CDR(obj)) {
        if (counter == 2)
          return 0;
        a = SCHEME_CAR(obj);
        if (ok_planet_number(a)) {
          /* ok */
        } else if ((counter == 1) && SCHEME_PAIRP(a)) {
          if (scheme_proper_list_length(a) != 2)
            return 0;
          if (ok_planet_number(SCHEME_CAR(a))) {
            if (ok_planet_number(SCHEME_CADR(a))) {
              if (scheme_bin_lt_eq(SCHEME_CAR(a), SCHEME_CADR(a))) {
                /* ok */
              } else
                return 0;
            } else
              return 0;
          } else if (SCHEME_SYMBOLP(SCHEME_CAR(a))) {
            if (SCHEME_SYM_LEN(SCHEME_CAR(a))) {
              int c;
              c = SCHEME_SYM_VAL(SCHEME_CAR(a))[0];
              if ((c == '=') || (c == '+') || (c == '-')) {
                if (!ok_planet_number(SCHEME_CADR(a)))
                  return 0;
                /* else ok */
              } else
                return 0;
            } else
              return 0;
          } else
            return 0;
        } else
          return 0;
        counter++;
      }

      for (; !SCHEME_NULLP(subs); subs = SCHEME_CDR(subs)) {
        a = SCHEME_CAR(subs);
        if (!SCHEME_CHAR_STRINGP(a))
          return 0;
        if (!ok_path_string(a, 0, 0, 0, 0))
          return 0;
      }

      return 1;
    }
  }

  return 0;
}

static Scheme_Object *is_module_path(int argc, Scheme_Object **argv)
{
  return (scheme_is_module_path(argv[0])
          ? scheme_true
          : scheme_false);
}

static int do_add_simple_require_renames(Scheme_Object *rn, 
                                         Scheme_Hash_Table *required, Scheme_Object *orig_src,
                                         Scheme_Module *im, Scheme_Module_Phase_Exports *pt,
                                         Scheme_Object *idx,
                                         Scheme_Object *marshal_phase_index,
                                         Scheme_Object *src_phase_index,
                                         int can_override)
{
  int i, saw_mb, numvals;
  Scheme_Object **exs, **exss, **exsns, *midx, *info, *vec, *nml, *mark_src;
  int *exets;
  int with_shared = 1;

  saw_mb = 0;

  if (!pt->num_provides)
    return 0;

  if (with_shared) {
    if (!pt->src_modidx && im->me->src_modidx)
      pt->src_modidx = im->me->src_modidx;
    scheme_extend_module_rename_with_shared(rn, idx, pt, 
                                            marshal_phase_index, 
                                            scheme_make_integer(0), 
                                            scheme_null,
                                            NULL,
                                            1);
  }

  mark_src = scheme_rename_to_stx(rn);

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
    if (!with_shared) {
      scheme_extend_module_rename(rn, midx, exs[i], exsns[i], idx, exs[i], 
                                  exets ? exets[i] : 0, src_phase_index, pt->phase_index, 
                                  1);
    }
    if (SAME_OBJ(exs[i], module_begin_symbol))
      saw_mb = 1;

    if (required) {
      /*
        A `required' vector has the following slots:
          0 : list of nominal source (i.e., the modules written with `require')
          1 : the initial midx for the import
          2 : a symbolic name in the original exporting module
          3 : variable => #t; syntax => #f
          4 : the exported name
          5 : a syntax object for error reporting
          6 : a syntax object for marks
          7 : whether the import can be shadowed
          8 : source phase
      */
      vec = scheme_make_vector(9, NULL);
      nml = scheme_make_pair(idx, scheme_null);
      SCHEME_VEC_ELS(vec)[0] = nml;
      SCHEME_VEC_ELS(vec)[1] = midx;
      SCHEME_VEC_ELS(vec)[2] = exsns[i];
      SCHEME_VEC_ELS(vec)[3] = ((i < numvals) ? scheme_true : scheme_false);
      SCHEME_VEC_ELS(vec)[4] = exs[i];
      SCHEME_VEC_ELS(vec)[5] = orig_src;
      SCHEME_VEC_ELS(vec)[6] = mark_src;
      SCHEME_VEC_ELS(vec)[7] = (can_override ? scheme_true : scheme_false);
      SCHEME_VEC_ELS(vec)[8] = exets ? scheme_make_integer(exets[i]) : scheme_make_integer(0);
      scheme_hash_set(required, exs[i], vec);
    }
  }

  if (!with_shared) {
    info = cons(idx, cons(marshal_phase_index, 
                          cons(scheme_make_integer(0),
                               cons(scheme_null, scheme_false))));
    scheme_save_module_rename_unmarshal(rn, info);
  }

  return saw_mb;
}

static Scheme_Object *get_table(Scheme_Hash_Table *tables, Scheme_Object *phase)
{
  Scheme_Object *vec;
  Scheme_Hash_Table *required;

  vec = scheme_hash_get(tables, phase);
  if (!vec) {
    required = scheme_make_hash_table(SCHEME_hash_ptr);
    vec = scheme_make_vector(3, NULL);
    SCHEME_VEC_ELS(vec)[1] = (Scheme_Object *)required;
    scheme_hash_set(tables, phase, vec);
  }

  return vec;
}

static Scheme_Hash_Table *get_required_from_tables(Scheme_Hash_Table *tables, Scheme_Object *phase)
{
  Scheme_Object *vec;

  if (!tables)
    return NULL;

  vec = get_table(tables, phase);

  return (Scheme_Hash_Table *)SCHEME_VEC_ELS(vec)[1];
}

static int add_simple_require_renames(Scheme_Object *orig_src,
                                      Scheme_Object *rn_set, 
                                      Scheme_Hash_Table *tables,
                                      Scheme_Module *im, Scheme_Object *idx,
                                      Scheme_Object *import_shift /* = src_phase_index */,
                                      Scheme_Object *only_export_phase,
                                      int can_override)
{
  int saw_mb;
  Scheme_Object *phase;

  if (im->me->rt
      && (!only_export_phase || SAME_OBJ(only_export_phase, scheme_make_integer(0))))
    saw_mb = do_add_simple_require_renames(scheme_get_module_rename_from_set(rn_set, import_shift, 1), 
                                           get_required_from_tables(tables, import_shift),
                                           orig_src, im, im->me->rt, idx,
                                           scheme_make_integer(0),
                                           import_shift,
                                           can_override);
  else
    saw_mb = 0;
  
  if (im->me->et
      && (!only_export_phase || SAME_OBJ(only_export_phase, scheme_make_integer(1)))) {
    if (SCHEME_FALSEP(import_shift))
      phase = scheme_false;
    else
      phase = scheme_bin_plus(scheme_make_integer(1), import_shift);
    do_add_simple_require_renames(scheme_get_module_rename_from_set(rn_set, phase, 1), 
                                  get_required_from_tables(tables, phase),
                                  orig_src, im, im->me->et, idx,
                                  scheme_make_integer(1),
                                  import_shift,
                                  can_override);
  }

  if (im->me->dt
      && (!only_export_phase || SAME_OBJ(only_export_phase, scheme_false))) {
    do_add_simple_require_renames(scheme_get_module_rename_from_set(rn_set, scheme_false, 1), 
                                  get_required_from_tables(tables, scheme_false),
                                  orig_src, im, im->me->dt, idx,
                                  scheme_false,
                                  import_shift,
                                  can_override);
  }

  if (im->me->other_phases) {
    Scheme_Object *val, *key;
    int i;
    for (i = 0; i < im->me->other_phases->size; i++) {
      val = im->me->other_phases->vals[i];
      if (val) {
        key = im->me->other_phases->keys[i];
        if (!only_export_phase || scheme_eqv(only_export_phase, key)) {
          if (SCHEME_FALSEP(import_shift))
            phase = scheme_false;
          else
            phase = scheme_bin_plus(key, import_shift);
          do_add_simple_require_renames(scheme_get_module_rename_from_set(rn_set, phase, 1), 
                                        get_required_from_tables(tables, phase),
                                        orig_src, im, (Scheme_Module_Phase_Exports *)val, idx,
                                        key,
                                        import_shift,
                                        can_override);
        }
      }
    }
  }

  return saw_mb;
}

void scheme_prep_namespace_rename(Scheme_Env *menv)
{
  while (menv->mod_phase > 0) {
    scheme_prepare_template_env(menv);
    menv = menv->template_env;
  }

  scheme_prepare_exp_env(menv);
  start_module(menv->module, menv, 0, NULL, -1, 1, menv->phase, scheme_null, 1);

  if (!menv->rename_set_ready) {
    if (menv->module->rn_stx) {
      Scheme_Object *rns;
      Scheme_Module *m = menv->module;

      scheme_prepare_env_renames(menv, mzMOD_RENAME_NORMAL);

      if (SAME_OBJ(scheme_true, m->rn_stx)) {
	/* Reconstruct renames based on defns and requires. This case is
           used only when it's easy to reconstruct: no renames, no for-syntax
           definitions, etc. */
	int i, j;
	Scheme_Module *im;
	Scheme_Object *l, *idx, *one_rn, *shift, *name;

	rns = menv->rename_set;
        one_rn = scheme_get_module_rename_from_set(rns, scheme_make_integer(0), 1);

	/* Local, provided: */
	for (i = 0; i < m->me->rt->num_provides; i++) {
	  if (SCHEME_FALSEP(m->me->rt->provide_srcs[i])) {
	    name = m->me->rt->provide_src_names[i];
	    scheme_extend_module_rename(one_rn, m->self_modidx, name, name, m->self_modidx, name, 0, 
                                        scheme_make_integer(0), NULL, 0);
	  }
	}
        for (j = 0; j < m->num_phases; j++) {
          Scheme_Module_Export_Info *exp_info = m->exp_infos[j];
          one_rn = scheme_get_module_rename_from_set(rns, scheme_make_integer(j), 1);
          for (i = 0; i < exp_info->num_indirect_provides; i++) {
            name = exp_info->indirect_provides[i];
            scheme_extend_module_rename(one_rn, m->self_modidx, name, name, m->self_modidx, name, j, 
                                        scheme_make_integer(j), NULL, 0);
          }
          for (i = 0; i < exp_info->num_indirect_syntax_provides; i++) {
            name = exp_info->indirect_syntax_provides[i];
            scheme_extend_module_rename(one_rn, m->self_modidx, name, name, m->self_modidx, name, j, 
                                        scheme_make_integer(j), NULL, 0);
          }
        }

	/* Required: */
        for (i = -4; i < (menv->other_require_names ? menv->other_require_names->size : 0); i++) {
          switch (i) {
          case -4:
            l = menv->require_names;
            shift = scheme_make_integer(0);
            break;
          case -3:
            l = menv->et_require_names;
            shift = scheme_make_integer(1);
            break;
          case -2:
            l = menv->tt_require_names;
            shift = scheme_make_integer(-1);
            break;
          case -1:
            l = menv->dt_require_names;
            shift = scheme_false;
            break;
          default:
            l = menv->other_require_names->vals[i];
            shift = menv->other_require_names->keys[i];
            break;
          }

          if (l) {
            /* Do initial import first to get shadowing right: */
            for (; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
              idx = SCHEME_CAR(l);
              name = scheme_module_resolve(idx, 0);
              
              im = get_special_module(name);
              if (!im)
                im = registry_get_loaded(menv, name);
              
              add_simple_require_renames(NULL, rns, NULL, im, idx, shift, NULL, 0);
            }
          }
        }
	
	rns = scheme_rename_to_stx(rns);
	m->rn_stx = rns;
      } else if (SCHEME_PAIRP(m->rn_stx)) {
	/* Delayed shift: */
	Scheme_Object *vec, *vec2, *rn_stx, *midx;
        int i;

	vec = SCHEME_CAR(m->rn_stx);
	midx = SCHEME_CDR(m->rn_stx);
        
        if (!SCHEME_VECTORP(vec))
          vec = scheme_make_vector(1, vec);
        vec2 = scheme_make_vector(SCHEME_VEC_SIZE(vec), NULL);

        for (i = SCHEME_VEC_SIZE(vec); i--; ) {
          rn_stx = SCHEME_VEC_ELS(vec)[i];
          rns = scheme_stx_to_rename(rn_stx);
          rns = scheme_stx_shift_rename_set(rns, midx, m->self_modidx, menv->access_insp);
          rn_stx = scheme_rename_to_stx(rns);
          SCHEME_VEC_ELS(vec2)[i] = rn_stx;
        }

	m->rn_stx = vec2;
      }

      /* add rename(s) to the environment's rename: */
      {
        int i;
        Scheme_Object *vec = m->rn_stx, *prior = NULL;

        if (!SCHEME_VECTORP(vec)) {
          vec = scheme_make_vector(1, vec);
          m->rn_stx = vec;
        }

        for (i = SCHEME_VEC_SIZE(vec); i--; ) {
          rns = scheme_stx_to_rename(SCHEME_VEC_ELS(vec)[i]);
          scheme_append_rename_set_to_env(rns, menv);
          prior = scheme_accum_prior_contexts(rns, prior);
        }
        scheme_install_prior_contexts_to_env(prior, menv);
      }

      menv->rename_set_ready = 1;
    }
  }
}

Scheme_Object *scheme_module_to_namespace(Scheme_Object *name, Scheme_Env *env)
{
  Scheme_Env *menv;
  Scheme_Object *modchain;

  name = scheme_module_resolve(scheme_make_modidx(name, scheme_false, scheme_false), 1);

  menv = get_special_modenv(name);
  if (!menv) {
    modchain = env->modchain;
    menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(modchain), name);
    if (!menv) {
      if (registry_get_loaded(env, name))
        scheme_contract_error("module->namespace",
                              "module not instantiated in the current namespace",
                              "name", 1, name,
                              NULL);
      else
        scheme_contract_error("module->namespace",
                              "unknown module in the current namespace",
                              "name", 1, name,
                              NULL);
    }
  }

  {
    Scheme_Object *insp;
    insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);
    if (scheme_module_protected_wrt(menv->guard_insp, insp) || menv->attached) {
      scheme_contract_error("module->namespace",
                            "current code inspector cannot access namespace of module",
                            "module name", 1, name,
                            NULL);
    }
  }

  scheme_prep_namespace_rename(menv);

  return (Scheme_Object *)menv;
}

static Scheme_Object *module_to_namespace(int argc, Scheme_Object *argv[])
{
  Scheme_Env *env;

  env = scheme_get_env(NULL);

  if (!scheme_is_module_path(argv[0]))
    scheme_wrong_contract("module->namespace", "module-path?", 0, argc, argv);

  return scheme_module_to_namespace(argv[0], env);
}

static Scheme_Module *module_to_(const char *who, int argc, Scheme_Object *argv[], int unknown_ok)
{
  Scheme_Env *env;
  Scheme_Object *name;
  Scheme_Module *m;

  env = scheme_get_env(NULL);

  name = argv[0];

  if (!SCHEME_PATHP(name)
      && !SCHEME_MODNAMEP(name)
      && !SAME_TYPE(SCHEME_TYPE(name), scheme_module_index_type)
      && !scheme_is_module_path(name))
    scheme_wrong_contract(who, "(or/c module-path? module-path-index? resolved-module-path?)", 0, argc, argv);

  if (!SCHEME_MODNAMEP(name)) {
    if (!SAME_TYPE(SCHEME_TYPE(name), scheme_module_index_type))
      name = scheme_make_modidx(name, scheme_false, scheme_false);
    name = scheme_module_resolve(name, (argc > 1) ? SCHEME_TRUEP(argv[1]) : 0);
  }

  m = get_special_module(name);
  if (!m) {
    env = scheme_get_env(NULL);
    m = registry_get_loaded(env, name);
  }

  if (!m && !unknown_ok)
    scheme_contract_error(who,
                          "unknown module in the current namespace",
                          "name", 1, name,
                          NULL);

  return m;
}

static Scheme_Object *module_to_lang_info(int argc, Scheme_Object *argv[])
{
  Scheme_Module *m;

  m = module_to_("module->language-info", argc, argv, 0);

  return (m->lang_info ? m->lang_info : scheme_false);
}

static Scheme_Object *module_is_declared(int argc, Scheme_Object *argv[])
{
  Scheme_Module *m;

  m = module_to_("module-declared?", argc, argv, 1);

  return (m ? scheme_true : scheme_false);
}

int scheme_module_is_declared(Scheme_Object *name, int try_load)
{
  Scheme_Object *a[2];
  Scheme_Module *m;

  a[0] = name;
  a[1] = (try_load ? scheme_true : scheme_false);
  m = module_to_("module-declared?", 2, a, 1);

  return (m ? 1 : 0);
}

static Scheme_Object *module_is_predefined(int argc, Scheme_Object *argv[])
{
  Scheme_Module *m;

  m = module_to_("module-predefined?", argc, argv, 1);

  return ((m && m->predefined) ? scheme_true : scheme_false);
}

int scheme_is_predefined_module_p(Scheme_Object *name)
{
  Scheme_Object *a[1];
  Scheme_Module *m;

  a[0] = name;
  m = module_to_("module-predefined?", 1, a, 1);
  
  return m && m->predefined;
}

static Scheme_Object *extract_compiled_imports(Scheme_Module *m)
{
  Scheme_Object *l;
  int i;

  l = scheme_null;
  if (!SCHEME_NULLP(m->requires))
    l = scheme_make_pair(scheme_make_pair(scheme_make_integer(0),
                                          m->requires),
                         l);
  if (!SCHEME_NULLP(m->et_requires))
    l = scheme_make_pair(scheme_make_pair(scheme_make_integer(1),
                                          m->et_requires),
                         l);
  if (!SCHEME_NULLP(m->tt_requires))
    l = scheme_make_pair(scheme_make_pair(scheme_make_integer(-1),
                                          m->tt_requires),
                         l);
  if (!SCHEME_NULLP(m->dt_requires))
    l = scheme_make_pair(scheme_make_pair(scheme_false,
                                          m->dt_requires),
                         l);

  if (m->other_requires) {
    for (i = 0; i < m->other_requires->size; i++) {
      if (m->other_requires->vals[i]) {
        l = scheme_make_pair(scheme_make_pair(m->other_requires->keys[i],
                                              m->other_requires->vals[i]),
                             l);
      }
    }
  }
    
  return l;
}

static Scheme_Object *make_provide_desc(Scheme_Module_Phase_Exports *pt, int i)
{
  return scheme_make_pair(pt->provides[i],
                          scheme_make_pair((pt->provide_nominal_srcs
                                            ? pt->provide_nominal_srcs[i]
                                            : scheme_null),
                                           scheme_null));
}

static Scheme_Object *extract_compiled_exports(Scheme_Module *m)
{
  Scheme_Object *a[2];
  Scheme_Object *ml, *vl, *val_l, *mac_l;
  Scheme_Module_Phase_Exports *pt;
  int i, n, k;

  val_l = scheme_null;
  mac_l = scheme_null;

  for (k = -3; k < (m->me->other_phases ? m->me->other_phases->size : 0); k++) {
    switch(k) {
    case -3:
      pt = m->me->rt;
      break;
    case -2:
      pt = m->me->et;
      break;
    case -1:
      pt = m->me->dt;
      break;
    default:
      pt = (Scheme_Module_Phase_Exports *)m->me->other_phases->vals[k];
      break;
    }

    if (pt) {
      ml = scheme_null;
      vl = scheme_null;
      n = pt->num_var_provides;
      for (i = pt->num_provides - 1; i >= n; --i) {
        ml = scheme_make_pair(make_provide_desc(pt, i), ml);
      }
      for (; i >= 0; --i) {
        vl = scheme_make_pair(make_provide_desc(pt, i), vl);
      }

      if (!SCHEME_NULLP(vl))
        val_l = scheme_make_pair(scheme_make_pair(pt->phase_index, vl), 
                                 val_l);

      if (!SCHEME_NULLP(ml))
        mac_l = scheme_make_pair(scheme_make_pair(pt->phase_index, ml),
                                 mac_l);
    }
  }
    
  a[0] = val_l;
  a[1] = mac_l;
  return scheme_values(2, a);
}

static Scheme_Object *module_to_imports(int argc, Scheme_Object *argv[])
{
  Scheme_Module *m;

  m = module_to_("module->imports", argc, argv, 0);

  return extract_compiled_imports(m);
}

static Scheme_Object *module_to_exports(int argc, Scheme_Object *argv[])
{
  Scheme_Module *m;

  m = module_to_("module->exports", argc, argv, 0);

  return extract_compiled_exports(m);
}

static Scheme_Object *module_compiled_p(int argc, Scheme_Object *argv[])
{
  Scheme_Module *m;

  m = scheme_extract_compiled_module(argv[0]);
      
  return (m ? scheme_true : scheme_false);
}

static Scheme_Object *wrap_module_in_top(Scheme_Object *m, Scheme_Object *t)
{
  Scheme_Compilation_Top *top;

  top = MALLOC_ONE_TAGGED(Scheme_Compilation_Top);
  memcpy(top, t, sizeof(Scheme_Compilation_Top));
  top->code = m;

  return (Scheme_Object *)top;
}

static void reset_submodule_paths(Scheme_Module *m)
{
  Scheme_Module *m2;
  Scheme_Object *stack, *l, *l2, *v, *name, *submodule_path;
  int k;

  stack = scheme_make_pair((Scheme_Object *)m, scheme_null);
  while (!SCHEME_NULLP(stack)) {
    m = (Scheme_Module *)SCHEME_CAR(stack);
    stack = SCHEME_CDR(stack);

    if (!m->submodule_path || SCHEME_NULLP(m->submodule_path))
      submodule_path = scheme_make_pair(scheme_resolved_module_path_value(m->modname), scheme_null);
    else {
      submodule_path = m->submodule_path;
      submodule_path = scheme_reverse(submodule_path);
    }

    for (k = 0; k < 2; k++) {
      l = (k ? m->post_submodules : m->pre_submodules);
      if (l) {
        l2 = scheme_null;
        for (; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
          m2 = MALLOC_ONE_TAGGED(Scheme_Module);
          memcpy(m2, SCHEME_CAR(l), sizeof(Scheme_Module));
          
          name = scheme_resolved_module_path_value(m2->modname);
          if (SCHEME_PAIRP(name)) {
            while (SCHEME_PAIRP(name) && SCHEME_PAIRP(SCHEME_CDR(name))) {
              name = SCHEME_CDR(name);
            }
            name = SCHEME_CAR(name);
          }
          v = scheme_reverse(scheme_make_pair(name, submodule_path));
          m2->submodule_path = v;
          v = scheme_intern_resolved_module_path(v);
          m2->modname = v;
          
          l2 = scheme_make_pair((Scheme_Object *)m2, l2);
          stack = scheme_make_pair((Scheme_Object *)m2, stack);
        }
        l2 = scheme_reverse(l2);
        if (k)
          m->post_submodules = l2;
        else
          m->pre_submodules = l2;
      }
    }
  }
}

static Scheme_Object *module_compiled_name(int argc, Scheme_Object *argv[])
{
  Scheme_Module *m, *m2;
  Scheme_Object *v, *p;

  m = scheme_extract_compiled_module(argv[0]);
      
  if (m) {
    if (argc > 1) {
      v = argv[1];
      if (!SCHEME_SYMBOLP(v)) {
        if (SCHEME_PAIRP(v)) {
          while (SCHEME_PAIRP(v)) {
            if (!SCHEME_SYMBOLP(SCHEME_CAR(v)))
              break;
            v = SCHEME_CDR(v);
          }
          if (!SCHEME_NULLP(v))
            v = NULL;
        } else
          v = NULL;
      }
      if (!v)
        scheme_wrong_contract("module-compiled-name", "(or/c symbol? (listof symbol?))", 1, argc, argv);
      if (SCHEME_PAIRP(v)) {
        p = SCHEME_CDR(v);
        if (SCHEME_NULLP(p))
          v = SCHEME_CAR(v);
      } else
        p = scheme_null;
      v = scheme_intern_resolved_module_path(v);
      m2 = MALLOC_ONE_TAGGED(Scheme_Module);
      memcpy(m2, m, sizeof(Scheme_Module));
      m2->modname = v;
      m2->submodule_path = p;
      reset_submodule_paths(m2);
      return wrap_module_in_top((Scheme_Object *)m2, argv[0]);
    } else
      return scheme_resolved_module_path_value(m->modname);
  }

  scheme_wrong_contract("module-compiled-name", "compiled-module-expression?", 0, argc, argv);
  return NULL;
}

static Scheme_Object *module_compiled_imports(int argc, Scheme_Object *argv[])
{
  Scheme_Module *m;

  m = scheme_extract_compiled_module(argv[0]);

  if (m)
    return extract_compiled_imports(m);

  scheme_wrong_contract("module-compiled-imports", "compiled-module-expression?", 0, argc, argv);
  return NULL;
}

static Scheme_Object *module_compiled_exports(int argc, Scheme_Object *argv[])
{
  Scheme_Module *m;
  m = scheme_extract_compiled_module(argv[0]);

  if (m)
    return extract_compiled_exports(m);

  scheme_wrong_contract("module-compiled-exports", "compiled-module-expression?", 0, argc, argv);
  return NULL;
}

static Scheme_Object *module_compiled_lang_info(int argc, Scheme_Object *argv[])
{
  Scheme_Module *m;

  m = scheme_extract_compiled_module(argv[0]);

  if (m) {
    return (m->lang_info ? m->lang_info : scheme_false);
  }

  scheme_wrong_contract("module-compiled-language-info", "compiled-module-expression?", 0, argc, argv);
  return NULL;
}

static Scheme_Object *module_compiled_submodules(int argc, Scheme_Object *argv[])
{
  Scheme_Module *m, *m2;
  Scheme_Object *l, *l2;
  int pre;

  m = scheme_extract_compiled_module(argv[0]);
  pre = SCHEME_TRUEP(argv[1]);

  if (m) {
    if (argc > 2) {
      l2 = scheme_null;
      for (l = argv[2]; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
        m2 = scheme_extract_compiled_module(SCHEME_CAR(l));
        if (!m2) break;
        l2 = scheme_make_pair((Scheme_Object *)m2, l2);
      }
      if (SCHEME_NULLP(l)) {
        m2 = MALLOC_ONE_TAGGED(Scheme_Module);
        memcpy(m2, m, sizeof(Scheme_Module));
        l2 = scheme_reverse(l2);
        if (pre)
          m2->pre_submodules = l2;
        else
          m2->post_submodules = l2;
        reset_submodule_paths(m2);
        return wrap_module_in_top((Scheme_Object *)m2, argv[0]);
      } else {
        scheme_wrong_contract("module-compiled-submodules", "(listof compiled-module-expression?)", 2, argc, argv);
      }
    } else {
      l2 = scheme_null;
      l = (pre ? m->pre_submodules : m->post_submodules);
      l = l ? l : scheme_null;
      for (; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
        l2 = scheme_make_pair(wrap_module_in_top(SCHEME_CAR(l), argv[0]), l2);
      }
    }
    
    return scheme_reverse(l2);
  }
  
  scheme_wrong_contract("module-compiled-submodules", "compiled-module-expression?", 0, argc, argv);
  return NULL;
}

static Scheme_Object *module_compiled_phaseless_p(int argc, Scheme_Object *argv[])
{
  Scheme_Module *m;

  m = scheme_extract_compiled_module(argv[0]);
  if (m) {
    if (m->phaseless)
      return scheme_true;
  } else
    scheme_wrong_contract("module-compiled-cross-phase-persistent?", 
                          "compiled-module-expression?", 0, argc, argv);
  
  return scheme_false;
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
    scheme_wrong_contract("module-path-index-resolve", "module-path-index?", 0, argc, argv);

  return scheme_module_resolve(argv[0], 0);
}

static Scheme_Object *module_path_index_split(int argc, Scheme_Object *argv[])
{
  Scheme_Modidx *modidx;
  Scheme_Object *a[2];

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_module_index_type))
    scheme_wrong_contract("module-path-index-split", "module-path-index?", 0, argc, argv);

  modidx = (Scheme_Modidx *)argv[0];
  a[0] = modidx->path;
  a[1] = modidx->base;

  return scheme_values(2, a);
}

static Scheme_Object *module_path_index_join(int argc, Scheme_Object *argv[])
{
  if (!scheme_is_module_path(argv[0])
      && !SCHEME_FALSEP(argv[0]))
    scheme_wrong_contract("module-path-index-join", "(or/c module-path? #f)", 0, argc, argv);

  if (argv[1]) { /* mzc will generate NULL sometimes; see scheme_declare_module(), below */
    if (SCHEME_TRUEP(argv[1])
	&& !SCHEME_MODNAMEP(argv[1])
	&& !SAME_TYPE(SCHEME_TYPE(argv[1]), scheme_module_index_type))
      scheme_wrong_contract("module-path-index-join", "(or/c module-path-index? resolved-module-path? #f)", 1, argc, argv);

    if (SCHEME_FALSEP(argv[0]) && !SCHEME_FALSEP(argv[1]))
      scheme_contract_error("module-path-index-join", 
                            "first argument cannot be #f when second argument is not #f",
                            "second argument", 1, argv[1],
                            NULL);
  }

  if (argc > 2) {
    Scheme_Object *l = argv[2];
    if (SCHEME_TRUEP(l)) {
      if (SCHEME_PAIRP(l)) {
        while (SCHEME_PAIRP(l)) {
          if (!SCHEME_SYMBOLP(SCHEME_CAR(l)))
            break;
          l = SCHEME_CDR(l);
        }
      } else
        l = scheme_false;
      if (!SCHEME_NULLP(l))
        scheme_wrong_contract("module-path-index-join", "(non-empty-listof symbol?)", 2, argc, argv);
      if (SCHEME_TRUEP(argv[0]) || SCHEME_TRUEP(argv[1]))
        scheme_contract_error("module-path-index-join", 
                              "third argument must be #f when first or second argument is non-#f",
                              "first argument", 1, argv[0],
                              "second argument", 1, argv[1],
                              "third argument", 1, argv[2],
                              NULL);
      return scheme_get_submodule_empty_self_modidx(argv[2]);
    }
  }

  return scheme_make_modidx(argv[0], argv[1], scheme_false);
}

static Scheme_Object *module_path_index_submodule(int argc, Scheme_Object *argv[])
{

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_module_index_type))
    scheme_wrong_contract("module-path-index-submodule", "module-path-index?", 0, argc, argv);
  
  return scheme_modidx_submodule(argv[0]);
}

Scheme_Object *scheme_modidx_submodule(Scheme_Object *_modidx)
{
  Scheme_Modidx *modidx;
  Scheme_Object *a;

  modidx = (Scheme_Modidx *)_modidx;
  a = modidx->resolved;
  if (SCHEME_TRUEP(modidx->path)
      || SCHEME_TRUEP(modidx->base)
      || SCHEME_FALSEP(a))
    return scheme_false;

  a = scheme_resolved_module_path_value(a);
  if (!SCHEME_PAIRP(a))
    return scheme_false;

  return SCHEME_CDR(a);
}

void scheme_init_module_path_table()
{
  REGISTER_SO(modpath_table);
#if PLACE_LOCAL_MODPATH_TABLE
  modpath_table = scheme_make_nonlock_equal_bucket_table();
#else
  modpath_table = scheme_make_weak_equal_table();
#endif
}

static Scheme_Object *make_resolved_module_path_obj(Scheme_Object *o)
{
  Scheme_Object *rmp;
  
  rmp = scheme_alloc_small_object();
  rmp->type = scheme_resolved_module_path_type;
  SCHEME_PTR_VAL(rmp) = o;

  return rmp;
}

Scheme_Object *scheme_resolved_module_path_value(Scheme_Object *rmp)
{
  return SCHEME_RMP_VAL(rmp);
}

int scheme_resolved_module_path_value_matches(Scheme_Object *rmp, Scheme_Object *o) {
  Scheme_Object *rmp_val = SCHEME_RMP_VAL(rmp);
  if (SAME_OBJ(rmp_val, o)) 
    return 1;
  else if (SCHEME_BYTE_STRINGP(rmp_val) && SCHEME_SYMBOLP(o)) {
    return !strncmp(SCHEME_BYTE_STR_VAL(rmp_val), 
                    SCHEME_SYM_VAL(o), 
                    mz_MIN(SCHEME_BYTE_STRLEN_VAL(rmp_val), SCHEME_SYM_LEN(o)));
  }  else {
    scheme_arg_mismatch("scheme_resolved_module_path_value_matches", 
                        "internal error: unknown type of resolved_module_path_value",
                        rmp_val);
    return 0;
  }
}

Scheme_Object *scheme_intern_resolved_module_path(Scheme_Object *o)
{
  Scheme_Bucket_Table *create_table;
  Scheme_Object *rmp;
  Scheme_Bucket *b;

  rmp = make_resolved_module_path_obj(o);
#if PLACE_LOCAL_MODPATH_TABLE
  if (place_local_modpath_table) {
    scheme_start_atomic();
    b = scheme_bucket_or_null_from_table(place_local_modpath_table, (const char *)rmp, 0);
    scheme_end_atomic_no_swap();
    if (b) {
      return (Scheme_Object *)HT_EXTRACT_WEAK(b->key);
    }
  }
#endif

  scheme_start_atomic();
  b = scheme_bucket_or_null_from_table(modpath_table, (const char *)rmp, 0);
  scheme_end_atomic_no_swap();

  if (b) {
#if PLACE_LOCAL_MODPATH_TABLE
    return (Scheme_Object *)b->key;
#else
    return (Scheme_Object *)HT_EXTRACT_WEAK(b->key);
#endif
  }

#if PLACE_LOCAL_MODPATH_TABLE
  create_table = place_local_modpath_table ? place_local_modpath_table : modpath_table;
#else
  create_table = modpath_table;
#endif

  scheme_start_atomic();
  b = scheme_bucket_from_table(create_table, (const char *)rmp);
  scheme_end_atomic_no_swap();

  if (!b->val)
    b->val = scheme_true;

#if PLACE_LOCAL_MODPATH_TABLE
  if (!place_local_modpath_table)
    return (Scheme_Object *)b->key;
#endif
  return(Scheme_Object *)HT_EXTRACT_WEAK(b->key);
}

static Scheme_Object *resolved_module_path_p(int argc, Scheme_Object *argv[])
{
  return (SCHEME_MODNAMEP(argv[0])
          ? scheme_true
          : scheme_false);
}

static Scheme_Object *make_resolved_module_path(int argc, Scheme_Object *argv[])
{
  Scheme_Object *p;

  p = argv[0];
  if (SCHEME_PAIRP(p)) {
    if (scheme_is_list(p)) {
      p = SCHEME_CDR(p);
      if (SCHEME_PAIRP(p)) {
        while (SCHEME_PAIRP(p)) {
          if (!SCHEME_SYMBOLP(SCHEME_CAR(p)))
            break;
          p = SCHEME_CDR(p);
        }
      } else
        p = scheme_false;
      if (SCHEME_NULLP(p))
        p = SCHEME_CAR(argv[0]);
      else
        p = scheme_false;
    } else
      p = scheme_false;
  }

  if (!SCHEME_SYMBOLP(p)
      && (!SCHEME_PATHP(p)
          || !scheme_is_complete_path(SCHEME_PATH_VAL(p),
                                      SCHEME_PATH_LEN(p),
                                      SCHEME_PLATFORM_PATH_KIND)))
    scheme_wrong_contract("make-resolved-module-path", 
                          "(or/c symbol?"
                          " (and/c path? complete-path?)"
                          " (cons/c (or/c symbol? (and/c path? complete-path?)) (non-empty-listof symbol?))"
                          ")",
                          0, argc, argv);

  return scheme_intern_resolved_module_path(argv[0]);
}

static Scheme_Object *resolved_module_path_name(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_MODNAMEP(argv[0]))
    scheme_wrong_contract("resolved-module-path-name", "resolved-module-path?", 0, argc, argv);

  return scheme_resolved_module_path_value(argv[0]);
}


static Scheme_Object *module_export_protected_p(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;
  Scheme_Object *modname, *name;
  Scheme_Module *m;
  int i, count;

  if (!SCHEME_MODNAMEP(argv[0])
      && !SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_module_index_type))
    scheme_wrong_contract("module-provide-protected?", "(or/c resolved-module-path? module-path-index?)", 0, argc, argv);
  if (!SCHEME_SYMBOLP(argv[1]))
    scheme_wrong_contract("module-provide-protected?", "symbol?", 1, argc, argv);

  modname = scheme_module_resolve(argv[0], 1);
  name = argv[1];

  env = scheme_get_env(NULL);
  m = get_special_module(modname);
  if (!m)
    m = registry_get_loaded(env, modname);
  if (!m) {
    scheme_contract_error("module-provide-protected?",
                          "unknown module (in the source namespace)",
                          "name", 1, modname,
                          NULL);
    return NULL;
  }

  count = m->me->rt->num_provides;
  for (i = 0; i < count; i++) {
    if (SAME_OBJ(name, m->me->rt->provides[i])) {
      if (m->exp_infos[0]->provide_protects && m->exp_infos[0]->provide_protects[i])
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

  if (SCHEME_MODNAMEP(path))
    return path;
  
  if (SCHEME_PAIRP(path)
      && SAME_OBJ(SCHEME_CAR(path), quote_symbol)
      && SCHEME_PAIRP(SCHEME_CDR(path))
      && SAME_OBJ(SCHEME_CADR(path), kernel_symbol)
      && SCHEME_NULLP(SCHEME_CDR(SCHEME_CDR(path)))
      && kernel_modidx)
    return kernel_modidx;

  modidx = MALLOC_ONE_TAGGED(Scheme_Modidx);
  modidx->so.type = scheme_module_index_type;
  modidx->path = path;

  /* base is needed only for relative-path strings,
     `file' forms, and `(submod "." ...)' forms: */
  if (SCHEME_CHAR_STRINGP(path)
      || (SCHEME_PAIRP(path)
          && SAME_OBJ(file_symbol, SCHEME_CAR(path)))
      || (SCHEME_PAIRP(path)
          && SAME_OBJ(submod_symbol, SCHEME_CAR(path))
          && SCHEME_CHAR_STRINGP(SCHEME_CAR(SCHEME_CDR(path)))))
    modidx->base = base_modidx;
  else
    modidx->base = scheme_false;

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

Scheme_Object *scheme_get_submodule_empty_self_modidx(Scheme_Object *submodule_path)
{
  Scheme_Bucket *b;

  if (SCHEME_NULLP(submodule_path))
    return empty_self_modidx;

  if (!submodule_empty_modidx_table) {
    REGISTER_SO(submodule_empty_modidx_table);
    submodule_empty_modidx_table = scheme_make_weak_equal_table();
  }

  scheme_start_atomic();
  b = scheme_bucket_from_table(submodule_empty_modidx_table, (const char *)submodule_path);
  if (!b->val) {
    submodule_path = make_resolved_module_path_obj(scheme_make_pair(scheme_resolved_module_path_value(empty_self_modname),
                                                                    submodule_path));
    submodule_path = scheme_make_modidx(scheme_false, 
                                        scheme_false,
                                        submodule_path);
    b->val = submodule_path;
  }
  scheme_end_atomic_no_swap();

  return b->val;
}

static Scheme_Object *_module_resolve_k(void);

static Scheme_Object *_module_resolve(Scheme_Object *modidx, Scheme_Object *stx, Scheme_Env *env, int load_it)
{
  if (SCHEME_MODNAMEP(modidx) || SCHEME_FALSEP(modidx))
    return modidx;

  if (SAME_OBJ(modidx, empty_self_modidx))
    return empty_self_modname;

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
	p->ku.k.p2 = (void *)env;
	p->ku.k.i1 = load_it;
	base = scheme_handle_stack_overflow(_module_resolve_k);
      } else {
	base = _module_resolve(base, NULL, env, load_it);
      }
    }

    if (SCHEME_SYMBOLP(base))
      base = scheme_false;

    if (stx && !SCHEME_FALSEP(stx) && !SCHEME_STXP(stx))
      stx = NULL;

    a[0] = ((Scheme_Modidx *)modidx)->path;
    a[1] = base;
    a[2] = (stx ? stx : scheme_false);
    a[3] = (load_it ? scheme_true : scheme_false);
    
    if (SCHEME_FALSEP(a[0])) {
      scheme_contract_error("module-path-index-resolve",
                            "\"self\" index has no resolution",
                            "module path index", 1, modidx,
                            NULL);
    }


    {
      Scheme_Cont_Frame_Data cframe;

      if (env) {
        Scheme_Config *config;
        
        config = scheme_extend_config(scheme_current_config(),
                                      MZCONFIG_ENV,
                                      (Scheme_Object *)env);
        scheme_push_continuation_frame(&cframe);
        scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);
      }

      name = scheme_apply(scheme_get_param(scheme_current_config(), MZCONFIG_CURRENT_MODULE_RESOLVER), 4, a);

      if (env) {
        scheme_pop_continuation_frame(&cframe);
      }
    }
    
    if (!SCHEME_MODNAMEP(name)) {
      a[0] = name;
      scheme_wrong_contract("module name resolver", "resolved-module-path?", -1, -1, a);
    }

    ((Scheme_Modidx *)modidx)->resolved = name;
  }

  return ((Scheme_Modidx *)modidx)->resolved;
}

static Scheme_Object *_module_resolve_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *base = (Scheme_Object *)p->ku.k.p1;
  Scheme_Env *env = (Scheme_Env *)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return _module_resolve(base, NULL, env, p->ku.k.i1);
}

Scheme_Object *scheme_module_resolve(Scheme_Object *modidx, int load_it)
{
  return _module_resolve(modidx, NULL, NULL, load_it);
}

Scheme_Object *module_resolve_in_namespace(Scheme_Object *modidx, Scheme_Env *env, int load_it)
{
  return _module_resolve(modidx, NULL, env, load_it);
}

static Scheme_Object *clone_modidx(Scheme_Object *modidx, Scheme_Object *src_modidx)
{
  Scheme_Object *base;

  if (SAME_OBJ(modidx, src_modidx))
    return modidx;

  if (!SAME_TYPE(SCHEME_TYPE(modidx), scheme_module_index_type))
    return modidx;
  
  /* Need to shift relative part? */
  base = ((Scheme_Modidx *)modidx)->base;
  if (!SCHEME_FALSEP(base)) {
    /* FIXME: depth */
    base = clone_modidx(base, src_modidx);
  }

  return scheme_make_modidx(((Scheme_Modidx *)modidx)->path,
                            base,
                            scheme_false);
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

      /* Shift cached? sbase as a modname is rare, but we need at least a little
         caching to make other things (e.g., .zo output) compact, so we use
         a small global cache in that case. */

      if (SCHEME_MODNAMEP(sbase)) {
        sbm = NULL;
        cvec = global_shift_cache;
      } else if (SAME_OBJ(sbase, empty_self_modidx)) {
        sbm = (Scheme_Modidx *)sbase;
        cvec = empty_self_shift_cache;
      } else {
        sbm = (Scheme_Modidx *)sbase;
        cvec = sbm->shift_cache;
      }

      /* attempt lookup in cache */
      /* ASSERT(SCHEME_VECTORP(cvec)); */
      c = (cvec ? SCHEME_VEC_SIZE(cvec) : 0);
      for (i = 0; i < c; i += 2) {
        if (SHIFT_CACHE_NULLP(SCHEME_VEC_ELS(cvec)[i]))
          break;
        if (SAME_OBJ(modidx, SCHEME_VEC_ELS(cvec)[i]))
          return SCHEME_VEC_ELS(cvec)[i + 1];
      }
     
      /* lookup failed, add entry to cache */
      smodidx = scheme_make_modidx(((Scheme_Modidx *)modidx)->path,
                                   sbase,
                                   scheme_false);
      
      /* make room in cache */
      if (!sbm) {
        if (!global_shift_cache)
          global_shift_cache = scheme_make_vector(GLOBAL_SHIFT_CACHE_SIZE, SHIFT_CACHE_NULL);
        else {
          for (i = (GLOBAL_SHIFT_CACHE_SIZE - 2); i--; ) {
            SCHEME_VEC_ELS(global_shift_cache)[i+2] = SCHEME_VEC_ELS(global_shift_cache)[i];
          }
        }
        cvec = global_shift_cache;
        i = 0;
      } else {
        /* May have GCed: */
        if (cvec && !sbm->shift_cache
            && !SAME_OBJ((Scheme_Object *)sbm, empty_self_modidx))
          sbm->shift_cache = cvec;

        if (i >= c) {
          /* Grow cache vector */
          Scheme_Object *naya;
          int j;

          naya = scheme_make_vector(c + 10, SHIFT_CACHE_NULL);
          for (j = 0; j < c; j++) {
            SCHEME_VEC_ELS(naya)[j] = SCHEME_VEC_ELS(cvec)[j];
          }
          if (!SAME_OBJ((Scheme_Object *)sbm, empty_self_modidx) && !sbm->shift_cache) {
            sbm->cache_next = modidx_caching_chain;
            modidx_caching_chain = sbm;
          }
          cvec = naya;
          if (!SAME_OBJ((Scheme_Object *)sbm, empty_self_modidx)) {
            sbm->shift_cache = cvec;
          } else {
            empty_self_shift_cache = cvec;
          }
        }
      }

      /* set entry in cache */
      SCHEME_VEC_ELS(cvec)[i]   = modidx;
      SCHEME_VEC_ELS(cvec)[i+1] = smodidx;

      return smodidx;
    }
  }

  return modidx;
}

void scheme_clear_modidx_cache(void)
{
  Scheme_Modidx *sbm, *next;

  global_shift_cache = NULL;
  empty_self_shift_cache = NULL;
  
  for (sbm = modidx_caching_chain; sbm; sbm = next) {
    sbm->shift_cache = NULL;
    next = sbm->cache_next;
    sbm->cache_next = NULL;
  }
  modidx_caching_chain = NULL;
}

static Scheme_Module *module_load(Scheme_Object *name, Scheme_Env *env, const char *who)
{
  Scheme_Module *m;

  m = get_special_module(name);
  if (!m) {
    m = registry_get_loaded(env, name);

    if (!m) {
      scheme_contract_error((who ? who : "require"),
                            "unknown module",
                            "module name", 1, name,
                            NULL);
      return NULL;
    }
  }
  
  return m;
}

static int is_procedure_expression(Scheme_Object *e)
{
  Scheme_Type t;

  if (SCHEME_PROCP(e))
    return 1;

  t = SCHEME_TYPE(e);

  return ((t == scheme_unclosed_procedure_type)
          || (t == scheme_case_lambda_sequence_type));
}

static void get_procedure_shape(Scheme_Object *e, Scheme_Object **_c)
{
  Scheme_Object *p, *v;

  p = scheme_get_or_check_procedure_shape(e, NULL);

  v = scheme_alloc_small_object();
  v->type = scheme_proc_shape_type;
  SCHEME_PTR_VAL(v) = p;

  *_c = v;
}

static void setup_accessible_table(Scheme_Module *m)
{
  if (!m->exp_infos[0]->accessible) {
    Scheme_Module_Phase_Exports *pt;
    int j;

    for (j = 0; j < m->num_phases; j++) {
      if (!j)
        pt = m->me->rt;
      else if (j == 1)
        pt = m->me->et;
      else {
        if (m->me->other_phases)
          pt = (Scheme_Module_Phase_Exports *)scheme_hash_get(m->me->other_phases,
                                                              scheme_make_integer(j));
        else
          pt = NULL;
      }
      
      if (pt) {
        Scheme_Hash_Table *ht;
        int i, count, nvp;
        
        ht = scheme_make_hash_table(SCHEME_hash_ptr);
        nvp = pt->num_var_provides;
        for (i = 0; i < nvp; i++) {
          if (SCHEME_FALSEP(pt->provide_srcs[i])) {
            scheme_hash_set(ht, pt->provide_src_names[i], scheme_make_integer(i));
          }
        }
        
        count = m->exp_infos[j]->num_indirect_provides;
        for (i = 0; i < count; i++) {
          scheme_hash_set(ht, m->exp_infos[j]->indirect_provides[i], scheme_make_integer(i + nvp));
        }
        
        /* Add syntax as negative ids: */
        count = pt->num_provides;
        for (i = nvp; i < count; i++) {
          if (SCHEME_FALSEP(pt->provide_srcs[i])) {
            scheme_hash_set(ht, pt->provide_src_names[i], scheme_make_integer(-(i+1)));
          }
        }

        if (!j) {
          /* find constants: */
          int i, cnt = SCHEME_VEC_SIZE(m->bodies[0]), k;
          Scheme_Object *form, *tl;

          for (i = 0; i < cnt; i++) {
            form = SCHEME_VEC_ELS(m->bodies[0])[i];
            if (SAME_TYPE(SCHEME_TYPE(form), scheme_define_values_type)) {
              int checked_st = 0, is_st = 0;
              Simple_Stuct_Type_Info stinfo;
              for (k = SCHEME_VEC_SIZE(form); k-- > 1; ) {
                tl = SCHEME_VEC_ELS(form)[k];
                if (SCHEME_TOPLEVEL_FLAGS(tl) & SCHEME_TOPLEVEL_SEAL) {
                  int pos = SCHEME_TOPLEVEL_POS(tl);
                  if (pos < m->prefix->num_toplevels) {
                    tl = m->prefix->toplevels[pos];
                    if (SCHEME_SYMBOLP(tl)) {
                      Scheme_Object *v;
                      v = scheme_hash_get(ht, tl);
                      if (!v) { 
                        /* The defined name is inaccessible. The bytecode compiler
                           won't generate such modules, but synthesized module bytecode
                           might leave bindings out of the `toplevels' table. */
                      } else {
                        if (SCHEME_VEC_SIZE(form) == 2) {
                          if (scheme_compiled_duplicate_ok(SCHEME_VEC_ELS(form)[0], 1)) {
                            /* record simple constant from cross-module propagation: */
                            v = scheme_make_pair(v, SCHEME_VEC_ELS(form)[0]);
                          } else if (SAME_TYPE(SCHEME_TYPE(SCHEME_VEC_ELS(form)[0]), scheme_inline_variant_type)) {
                            /* record a potentially inlineable function */
                            if (SCHEME_VEC_ELS(SCHEME_VEC_ELS(form)[0])[2] != (Scheme_Object *)m->prefix)
                              SCHEME_VEC_ELS(SCHEME_VEC_ELS(form)[0])[2] = (Scheme_Object *)m->prefix;
                            v = scheme_make_pair(v, SCHEME_VEC_ELS(form)[0]);
                          } else if (is_procedure_expression(SCHEME_VEC_ELS(form)[0])) {
                            /* that it's a procedure: */
                            v = scheme_make_vector(2, v);
                            SCHEME_VEC_ELS(v)[1] = SCHEME_VEC_ELS(form)[0];
                          } else {
                            /* record that it's fixed for any given instantiation: */
                            v = scheme_make_pair(v, scheme_fixed_key);
                          }
                        } else {
                          if (!checked_st) {
                            is_st = !!scheme_is_simple_make_struct_type(SCHEME_VEC_ELS(form)[0],
                                                                        SCHEME_VEC_SIZE(form)-1,
                                                                        1, 1, NULL, &stinfo,
                                                                        NULL, NULL, NULL, 0,
                                                                        m->prefix->toplevels, ht,
                                                                        5);
                            checked_st = 1;
                          }
                          if (is_st) {
                            intptr_t shape;
                            shape = scheme_get_struct_proc_shape(k-1, &stinfo);
                            v = scheme_make_vector(3, v);
                            SCHEME_VEC_ELS(v)[1] = scheme_make_integer(shape);
                          }
                        }
                        scheme_hash_set(ht, tl, v);
                      }
                    } else
                      scheme_signal_error("internal error: strange defn target %d", SCHEME_TYPE(tl));
                  }
                }
              }
            }
          }
        }

        m->exp_infos[j]->accessible = ht;
      }
    }
  }
}

Scheme_Env *scheme_module_access(Scheme_Object *name, Scheme_Env *env, intptr_t rev_mod_phase)
{
  Scheme_Env *menv;

  menv = get_special_modenv(name);

  if (!menv) {
    Scheme_Object *chain;
    int ph;

    chain = env->modchain;
    ph = rev_mod_phase;
    while (ph && chain) {
      chain = (SCHEME_VEC_ELS(chain))[2];
      if (SCHEME_FALSEP(chain))
	return NULL;
      ph--;
    }

    if (!chain) {
      scheme_signal_error("internal error: missing chain for module instances");
      return NULL;
    }

    menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(chain), name);
    
    while ((ph < rev_mod_phase) && menv) {
      menv = menv->exp_env;
      ph++;
    }
  }

  return menv;
}

static void check_certified(Scheme_Object *stx,
			    Scheme_Object *prot_insp, Scheme_Object *insp, 
                            Scheme_Object *rename_insp, Scheme_Object *in_modidx,
			    Scheme_Env *env, Scheme_Object *symbol,
			    int var, int prot, int *_would_complain)
{
  int need_cert = 1;
    
  if (need_cert && insp)
    need_cert = scheme_module_protected_wrt(env->guard_insp, insp);
  if (need_cert && rename_insp)
    need_cert = scheme_module_protected_wrt(env->guard_insp, rename_insp);

  if (need_cert) {
    if (_would_complain) {
      *_would_complain = 1;
    } else {
      /* For error, if stx is no more specific than symbol, drop symbol. */
      if (stx && SAME_OBJ(SCHEME_STX_SYM(stx), symbol)) {
        symbol = stx;
        stx = NULL;
      }
      scheme_wrong_syntax(scheme_compile_stx_string, stx, symbol, 
                          "access disallowed by code inspector to %s %s from module: %D",
                          prot ? "protected" : "unexported",
                          var ? "variable" : "syntax",
                          env->module->MODSRCNAME);
    }
  }
}

Scheme_Object *scheme_check_accessible_in_module(Scheme_Env *env, Scheme_Object *prot_insp, Scheme_Object *in_modidx,
						 Scheme_Object *symbol, Scheme_Object *stx,
						 Scheme_Object *certs, Scheme_Object *unexp_insp, 
                                                 Scheme_Object *rename_insp,
						 int position, int want_pos, 
                                                 int *_protected, int *_unexported,
                                                 Scheme_Env *from_env, int *_would_complain,
                                                 Scheme_Object **_is_constant)
     /* Returns the actual name when !want_pos, needed in case of
	uninterned names.  Otherwise, returns a position value on success.
	If position < -1, then merely checks for protected syntax.

	Access for protected and unexported names depends on
	certifictions in stx+certs, access implied by
	{prot_,unexp_}insp, or access implied by in_modidx. For
	unexported access, either stx+certs or unexp_insp must be
	supplied (not both), and prot_insp should be supplied 
        (for protected re-exports of unexported).
        For unprotected access, both prot_insp and stx+certs 
        should be supplied. In either case, rename_insp
        is optionally allowed. */
{
  Scheme_Module_Phase_Exports *pt;

  if (!SCHEME_SYMBOLP(symbol))
    symbol = scheme_tl_id_sym(env, symbol, NULL, 0, NULL, NULL);

  if (scheme_is_kernel_env(env)
      || ((env->module->primitive && !env->module->exp_infos[0]->provide_protects))) {
    if (want_pos)
      return scheme_make_integer(-1);
    else
      return symbol;
  }

  switch (env->mod_phase) {
  case 0:
    pt = env->module->me->rt;
    break;
  case 1:
    pt = env->module->me->et;
    break;
  default:
    pt = (Scheme_Module_Phase_Exports *)scheme_hash_get(env->module->me->other_phases,
                                                        scheme_make_integer(env->mod_phase));
    break;
  }

  if (pt) {
    if (position >= 0) {
      /* Check whether the symbol at `pos' matches the string part of
         the expected symbol.  */
      Scheme_Object *isym;
      int need_cert = 0;

      if (position < pt->num_var_provides) {
        if (!pt->provide_srcs
            || SCHEME_FALSEP(pt->provide_srcs[position]))
          isym = pt->provide_src_names[position];
        else
          isym = NULL;
      } else {
        int ipos = position - pt->num_var_provides;
        int num_indirect_provides;
        Scheme_Object **indirect_provides;

        if ((env->mod_phase >= 0) && (env->mod_phase < env->module->num_phases)) {
          num_indirect_provides = env->module->exp_infos[env->mod_phase]->num_indirect_provides;
          indirect_provides = env->module->exp_infos[env->mod_phase]->indirect_provides;
        } else {
          num_indirect_provides = 0;
          indirect_provides = NULL;
        }

        if (ipos < num_indirect_provides) {
          isym = indirect_provides[ipos];
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
	
          if ((position < pt->num_var_provides)
              && scheme_module_protected_wrt(env->guard_insp, prot_insp)) {
            char *provide_protects;

            if ((env->mod_phase >= 0) && (env->mod_phase < env->module->num_phases))
              provide_protects = env->module->exp_infos[env->mod_phase]->provide_protects;
            else
              provide_protects = NULL;
            
            if (provide_protects
                && provide_protects[position]) {
              if (_protected)
                *_protected = 1;
              check_certified(stx, prot_insp, prot_insp, rename_insp, in_modidx, env, symbol, 1, 1, _would_complain);
            }
          }

          if (need_cert)
            check_certified(stx, prot_insp, unexp_insp, rename_insp, in_modidx, env, symbol, 1, 0, _would_complain);
	
          if (want_pos)
            return scheme_make_integer(position);
          else
            return isym;
        } 
      }
      /* failure */
    } else {
      Scheme_Object *pos;

      if (env->mod_phase < env->module->num_phases)
        pos = scheme_hash_get(env->module->exp_infos[env->mod_phase]->accessible, symbol);
      else
        pos = NULL;
      
      if (pos) {
        if (SCHEME_PAIRP(pos)) {
          if (_is_constant) *_is_constant = SCHEME_CDR(pos);
          pos = SCHEME_CAR(pos);
        } else if (SCHEME_VECTORP(pos)) {
          if (SCHEME_VEC_SIZE(pos) == 2) {
            if (_is_constant)
              get_procedure_shape(SCHEME_VEC_ELS(pos)[1], _is_constant);
          } else {
            if (_is_constant) {
              Scheme_Object *ps;
              
              ps = scheme_make_struct_proc_shape(SCHEME_INT_VAL(SCHEME_VEC_ELS(pos)[1]));

              *_is_constant = ps;
            }
          }
          pos = SCHEME_VEC_ELS(pos)[0];
        }
      }

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
        char *provide_protects;

        if ((env->mod_phase >= 0) && (env->mod_phase < env->module->num_phases))
          provide_protects = env->module->exp_infos[env->mod_phase]->provide_protects;
        else
          provide_protects = NULL;

        if (provide_protects
            && (SCHEME_INT_VAL(pos) < pt->num_provides)
            && provide_protects[SCHEME_INT_VAL(pos)]) {
          if (_protected)
            *_protected = 1;
          check_certified(stx, prot_insp, prot_insp, rename_insp, in_modidx, env, symbol, 1, 1, _would_complain);
        }

        if ((position >= -1) 
            && (SCHEME_INT_VAL(pos) >= pt->num_var_provides)) {
          /* unexported var -- need cert */
          if (_protected)
            *_protected = 1;
          if (_unexported)
            *_unexported = 1;
          check_certified(stx, prot_insp, unexp_insp, rename_insp, in_modidx, env, symbol, 1, 0, _would_complain);
        }

        if (want_pos)
          return pos;
        else
          return symbol;
      }

      if (position < -1) {
        /* unexported syntax -- need cert */
        if (_unexported)
          *_unexported = 1;
        check_certified(stx, prot_insp, unexp_insp, rename_insp, in_modidx, env, symbol, 0, 0, _would_complain);
        return NULL;
      }
    }
  }

  if (_would_complain) {
    *_would_complain = 1;
    return NULL;
  }

  /* For error, if stx is no more specific than symbol, drop symbol. */
  if (stx && SAME_OBJ(SCHEME_STX_SYM(stx), symbol)) {
    symbol = stx;
    stx = NULL;
  }

  {
    const char *srcstr;
    intptr_t srclen;
    
    if (from_env->module)
      srcstr = scheme_display_to_string(from_env->module->MODSRCNAME, &srclen);
    else {
      srcstr = "";
      srclen = 0;
    }

    scheme_wrong_syntax("link", stx, symbol, 
                        "module mismatch;\n"
                        " possibly, bytecode file needs re-compile because dependencies changed\n"
                        "%s%t%s"
                        "  exporting module: %D\n"
                        "  exporting phase level: %d\n"
                        "  internal explanation: variable not provided (directly or indirectly%s)",
                        srclen ? "  importing module: " : "",
                        srcstr, srclen,
                        srclen ? "\n" : "",
                        env->module->MODSRCNAME,
                        env->mod_phase,
                        (position >= 0) ? " and at the expected position" : "");
  }

  return NULL;
}

void scheme_check_unsafe_accessible(Scheme_Object *insp, Scheme_Env *from_env)
{
  Scheme_Env *unsafe_env;

  unsafe_env = scheme_get_unsafe_env();

  if (insp && SCHEME_HASHTRP(insp)) {
    Scheme_Hash_Tree *t = (Scheme_Hash_Tree *)insp;
    int i;
    Scheme_Object *k, *v;

    for (i = scheme_hash_tree_next(t, -1); i != -1; i = scheme_hash_tree_next(t, i)) {
      scheme_hash_tree_index(t, i, &k, &v);
      insp = k;
      if (scheme_module_protected_wrt(unsafe_env->guard_insp, insp)) {
        break;
      }
    }

    if (i < 0)
      return;
  }

  if (!insp || scheme_module_protected_wrt(unsafe_env->guard_insp, insp)) {
    scheme_wrong_syntax("link", 
                        NULL, NULL, 
                        "attempt to access unsafe bindings from an untrusted context");
  }
}

int scheme_module_export_position(Scheme_Object *modname, Scheme_Env *env, Scheme_Object *varname)
{
  Scheme_Module *m;
  Scheme_Object *pos;

  if (SAME_OBJ(modname, kernel_modname)
      || SAME_OBJ(modname, unsafe_modname)
      || SAME_OBJ(modname, flfxnum_modname)
      || SAME_OBJ(modname, extfl_modname)
      || SAME_OBJ(modname, futures_modname))
    return -1;

  m = module_load(modname, env, NULL);
  if (!m || m->primitive)
    return -1;

  setup_accessible_table(m);

  pos = scheme_hash_get(m->exp_infos[0]->accessible, varname);

  if (SCHEME_PAIRP(pos))
    pos = SCHEME_CAR(pos);
  else if (SCHEME_VECTORP(pos))
    pos = SCHEME_VEC_ELS(pos)[0];
  
  if (pos && (SCHEME_INT_VAL(pos) >= 0))
    return SCHEME_INT_VAL(pos);
  else
    return -1;
}

Scheme_Object *scheme_module_syntax(Scheme_Object *modname, Scheme_Env *env, 
                                    Scheme_Object *name, int mod_phase)
{
  if (SAME_OBJ(modname, kernel_modname)) {
    Scheme_Env *kenv;
    kenv = scheme_get_kernel_env();
    if (SCHEME_STXP(name))
      name = SCHEME_STX_SYM(name);
    return scheme_lookup_in_table(kenv->syntax, (char *)name);
  } else if (SAME_OBJ(modname, unsafe_modname)
             || SAME_OBJ(modname, flfxnum_modname)
             || SAME_OBJ(modname, extfl_modname)
             || SAME_OBJ(modname, futures_modname)) {
    /* no unsafe, flfxnum, extfl, or futures syntax */
    return NULL;
  } else {
    Scheme_Env *menv;
    Scheme_Object *val;
    int i;

    for (i = 0; i < mod_phase; i++) {
      scheme_prepare_template_env(env);
      env = env->template_env;
      if (!env) return NULL;
    }

    menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(env->modchain), modname);

    if (!menv)
      return NULL;

    if (menv->module 
        && menv->running
        && ((mod_phase+1) < menv->module->num_phases)
        && !menv->running[mod_phase+1]) {
      scheme_wrong_syntax(scheme_compile_stx_string, NULL, name, 
                          "module mismatch;\n"
                          " attempted to use a module that is not available\n"
                          "  possible cause:\n"
                          "   using (dynamic-require .... #f)\n"
                          "   but need (dynamic-require .... 0)\n"
                          "  module: %D\n"
                          "  phase: %d",
                          menv->module->MODSRCNAME,
                          mod_phase);
      return NULL;
    }

    for (i = 0; i < mod_phase; i++) {
      scheme_prepare_exp_env(menv);
      menv = menv->exp_env;
      if (!menv) return NULL;
    }

    if (SCHEME_STXP(name))
      name = scheme_tl_id_sym(menv, name, NULL, 0, NULL, NULL);

    val = scheme_lookup_in_table(menv->syntax, (char *)name);

    return val;
  }
}

static int wait_registry(Scheme_Env *env)
{
  Scheme_Object *lock, *a[2];

  while (1) {
    lock = scheme_hash_get(env->module_registry->loaded, scheme_false);
    if (!lock)
      return 1;

    if (SAME_OBJ(SCHEME_CDR(lock), (Scheme_Object *)scheme_current_thread))
      return 0;

    a[0] = SCHEME_CAR(lock);
    a[1] = SCHEME_CDR(lock);
    (void)scheme_sync(2, a);
  }
}

static void lock_registry(Scheme_Env *env)
{
  Scheme_Object *lock;
  lock = scheme_make_pair(scheme_make_sema(0),
                          (Scheme_Object *) scheme_current_thread);
  scheme_hash_set(env->module_registry->loaded, scheme_false, lock);
}

static void unlock_registry(Scheme_Env *env)
{
  Scheme_Object *lock;
  lock = scheme_hash_get(env->module_registry->loaded, scheme_false);
  scheme_post_sema(SCHEME_CAR(lock));
  scheme_hash_set(env->module_registry->loaded, scheme_false, NULL);
}

XFORM_NONGCING static intptr_t make_key(int base_phase, int eval_exp, int eval_run)
{
  return ((base_phase << 3) 
          | (eval_exp ? ((eval_exp > 0) ? 2 : 4) : 0) 
          | (eval_run ? 1 : 0));
}

static int did_start(Scheme_Object *v, int base_phase, int eval_exp, int eval_run)
{
  intptr_t key;

  key = make_key(base_phase, eval_exp, eval_run);

  if (!v)
    return 0;

  if (scheme_hash_tree_get((Scheme_Hash_Tree *)v, scheme_make_integer(key)))
    return 1;

  return 0;
}

static Scheme_Object *add_start(Scheme_Object *v, int base_phase, int eval_exp, int eval_run)
{
  intptr_t key;
  Scheme_Hash_Tree *ht = (Scheme_Hash_Tree *)v;
  Scheme_Bucket *b;

  if (!ht)
    ht = scheme_make_hash_tree(0);

  key = make_key(base_phase, eval_exp, eval_run);

  ht = scheme_hash_tree_set(ht, scheme_make_integer(key), scheme_true);
  
  b = scheme_bucket_from_table(starts_table, (const char *)ht);
  if (!b->val)
    b->val = scheme_true;
  return (Scheme_Object *)HT_EXTRACT_WEAK(b->key);
}

#if 0
static int indent = 0;
# define show_indent(d) (indent += d)
static void show(const char *what, Scheme_Env *menv, int v1, int v2, int ph, int base_phase)
{
  if (menv->phase > 3) return;
  if (1 || SCHEME_SYMBOLP(SCHEME_PTR_VAL(menv->module->modname)))
    if (1 || SCHEME_SYM_VAL(SCHEME_PTR_VAL(menv->module->modname))[0] != '#') {
      int i;
      for (i = 0; i < indent; i++) {
        fprintf(stderr, " ");
      }
      fprintf(stderr, "%s \t%s @%ld+%d/%d [%d/%d] %p\n", 
              what, scheme_write_to_string(menv->module->modname, NULL), 
              menv->phase, ph, base_phase, v1, v2, menv->modchain);
    }
}
static void show_done(const char *what, Scheme_Env *menv, int v1, int v2, int i, int base_phase){
  show(what, menv, v1, v2, i, base_phase);
}
#else
# define show_indent(d) /* nothing */
# define show(w, m, v1, v2, i, bp) /* nothing */
# define show_done(w, m, v1, v2, i, bp) /* nothing */
#endif

static void clone_require_names(Scheme_Module *m, Scheme_Object *phase)
{
  Scheme_Object *np, *np_first, *np_last, *l, *reqs;

  if (SAME_OBJ(phase, scheme_make_integer(0))) {
    reqs = m->requires;
  } else if (SAME_OBJ(phase, scheme_make_integer(1))) {
    reqs = m->et_requires;
  } else if (SAME_OBJ(phase, scheme_make_integer(-1))) {
    reqs = m->tt_requires;
  } else if (SAME_OBJ(phase, scheme_false)) {
    reqs = m->dt_requires;
  } else {
    if (m->other_requires) {
      reqs = scheme_hash_get(m->other_requires, phase);
      if (!reqs)
        reqs = scheme_null;
    } else
      reqs = scheme_null;
  }

  if (SCHEME_NULLP(reqs)) return;

  np_first = scheme_null;
  np_last = NULL;

  for (l = reqs; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    np = cons(clone_modidx(SCHEME_CAR(l), m->me->src_modidx), scheme_null);
    if (np_last)
      SCHEME_CDR(np_last) = np;
    else
      np_first = np;
    np_last = np;
  }

  np = np_first;

  if (SAME_OBJ(phase, scheme_make_integer(0))) {
    m->requires = np;
  } else if (SAME_OBJ(phase, scheme_make_integer(1))) {
    m->et_requires = np;
  } else if (SAME_OBJ(phase, scheme_make_integer(-1))) {
    m->tt_requires = np;
  } else if (SAME_OBJ(phase, scheme_false)) {
    m->dt_requires = np;
  } else {
    scheme_hash_set(m->other_requires, phase, np);
  }
}

static void clone_all_require_names(Scheme_Module *m)
{
  clone_require_names(m, scheme_make_integer(0));
  clone_require_names(m, scheme_make_integer(1));
  clone_require_names(m, scheme_make_integer(-1));
  clone_require_names(m, scheme_false);

  if (m->other_requires) {
    Scheme_Hash_Table *ht;
    intptr_t i;
    ht = scheme_clone_hash_table(m->other_requires);
    m->other_requires = ht;
    for (i = 0; i < ht->size; i++) {
      if (ht->vals[i]) {
        clone_require_names(m, ht->keys[i]);
      }
    }
  }
}

static void compute_require_names(Scheme_Env *menv, Scheme_Object *phase, 
                                  Scheme_Env *load_env, Scheme_Object *syntax_idx)
{
  Scheme_Object *np, *np_first, *np_last, *midx, *l, *reqs, *req_names;

  if (SAME_OBJ(phase, scheme_make_integer(0))) {
    req_names = menv->require_names;
    reqs = menv->module->requires;
  } else if (SAME_OBJ(phase, scheme_make_integer(1))) {
    req_names = menv->et_require_names;
    reqs = menv->module->et_requires;
  } else if (SAME_OBJ(phase, scheme_make_integer(-1))) {
    req_names = menv->tt_require_names;
    reqs = menv->module->tt_requires;
  } else if (SAME_OBJ(phase, scheme_false)) {
    req_names = menv->dt_require_names;
    reqs = menv->module->dt_requires;
  } else {
    if (menv->module->other_requires) {
      reqs = scheme_hash_get(menv->module->other_requires, phase);
      if (!reqs)
        reqs = scheme_null;
    } else
      reqs = scheme_null;
    if (!SCHEME_NULLP(reqs) && !menv->other_require_names) {
      Scheme_Hash_Table *ht;
      ht = scheme_make_hash_table_equal();
      menv->other_require_names = ht;
    }
    if (menv->other_require_names)
      req_names = scheme_hash_get(menv->other_require_names, phase);
    else
      req_names = NULL;
  }

  if (req_names && !SCHEME_NULLP(req_names))
    return;

  np_first = scheme_null;
  np_last = NULL;

  for (l = reqs; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    midx = scheme_modidx_shift(SCHEME_CAR(l), 
                               menv->module->me->src_modidx, 
                               (syntax_idx ? syntax_idx : menv->link_midx));

    if (load_env)
      module_load(scheme_module_resolve(midx, 1), load_env, NULL);
    
    np = cons(midx, scheme_null);
    if (np_last)
      SCHEME_CDR(np_last) = np;
    else
      np_first = np;
    np_last = np;
  }

  np = np_first;

  if (!SAME_OBJ(np, req_names)) {
    if (SAME_OBJ(phase, scheme_make_integer(0))) {
      menv->require_names = np;
    } else if (SAME_OBJ(phase, scheme_make_integer(1))) {
      menv->et_require_names = np;
    } else if (SAME_OBJ(phase, scheme_make_integer(-1))) {
      menv->tt_require_names = np;
    } else if (SAME_OBJ(phase, scheme_false)) {
      menv->dt_require_names = np;
    } else {
      if (menv->other_require_names)
        scheme_hash_set(menv->other_require_names, phase, np);
    }
  }
}

static void chain_start_module(Scheme_Env *menv, Scheme_Env *env, int eval_exp, int eval_run, 
                               intptr_t base_phase, Scheme_Object *cycle_list, Scheme_Object *syntax_idx);

static Scheme_Object *chain_start_module_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Env *menv = (Scheme_Env *)p->ku.k.p1;
  Scheme_Env *env = (Scheme_Env *)p->ku.k.p2;
  Scheme_Object *cycle_list = (Scheme_Object *)p->ku.k.p3;
  Scheme_Object *syntax_idx = (Scheme_Object *)p->ku.k.p4;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;

  chain_start_module(menv, env,
                     p->ku.k.i1, p->ku.k.i2,
                     p->ku.k.i3, cycle_list, syntax_idx);

  return scheme_true;
}

static void chain_start_module(Scheme_Env *menv, Scheme_Env *env, int eval_exp, int eval_run, 
                               intptr_t base_phase, Scheme_Object *cycle_list, Scheme_Object *syntax_idx)
{
  Scheme_Object *new_cycle_list, *midx, *l;
  Scheme_Module *im;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      p->ku.k.p1 = (void *)menv;
      p->ku.k.p2 = (void *)env;
      p->ku.k.i1 = eval_exp;
      p->ku.k.i2 = eval_run;
      p->ku.k.i3 = base_phase;
      p->ku.k.p3 = (void *)cycle_list;
      p->ku.k.p4 = (void *)syntax_idx;
      (void)scheme_handle_stack_overflow(chain_start_module_k);
      return;
    }
  }
#endif

  new_cycle_list = scheme_make_pair(menv->module->modname, cycle_list);
  
  if (!SCHEME_NULLP(menv->module->dt_requires)) {
    compute_require_names(menv, scheme_false, env, syntax_idx);

    scheme_prepare_label_env(menv);

    for (l = menv->dt_require_names; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
      midx = SCHEME_CAR(l);
    
      im = module_load(scheme_module_resolve(midx, 1), env, NULL);

      start_module(im, 
                   menv->label_env, 0, 
                   midx,
                   0, 0, base_phase,
                   new_cycle_list,
                   0);
    }
  }
  
  if (!SCHEME_NULLP(menv->module->tt_requires)) {

    compute_require_names(menv, scheme_make_integer(-1), env, syntax_idx);

    scheme_prepare_template_env(menv);

    for (l = menv->tt_require_names; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
      midx = SCHEME_CAR(l);
    
      im = module_load(scheme_module_resolve(midx, 1), env, NULL);

      start_module(im, 
                   menv->template_env, 0, 
                   midx,
                   eval_exp, eval_run, base_phase,
                   new_cycle_list,
                   0);
    }
  }

  compute_require_names(menv, scheme_make_integer(0), env, syntax_idx);

  for (l = menv->require_names; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    midx = SCHEME_CAR(l);

    im = module_load(scheme_module_resolve(midx, 1), env, NULL);

    start_module(im, env, 0, midx, eval_exp, eval_run, base_phase, 
                 new_cycle_list, 0);
  }

  scheme_prepare_exp_env(menv);
  menv->exp_env->link_midx = menv->link_midx;
  
  if (!SCHEME_NULLP(menv->module->et_requires)) {
    compute_require_names(menv, scheme_make_integer(1), env, syntax_idx);
    
    for (l = menv->et_require_names; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
      midx = SCHEME_CAR(l);
      
      im = module_load(scheme_module_resolve(midx, 1), env, NULL);
      
      start_module(im, menv->exp_env, 0, midx, eval_exp, eval_run, base_phase, 
                   new_cycle_list, 0);
    }
  }

  if (menv->module->other_requires) {
    int i;
    Scheme_Object *phase, *n;
    Scheme_Env *menv2;
    for (i = 0; i < menv->module->other_requires->size; i++) {
      if (menv->module->other_requires->vals[i]) {
        phase = menv->module->other_requires->keys[i];

        if (scheme_is_negative(phase)) {
          compute_require_names(menv, phase, env, syntax_idx);

          n = phase;
          menv2 = menv;
          while (scheme_is_negative(n)) {
            scheme_prepare_template_env(menv2);
            menv2 = menv2->template_env;
            n = scheme_bin_plus(n, scheme_make_integer(1));
          }

          l = scheme_hash_get(menv->other_require_names, phase);

          for (; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
            midx = SCHEME_CAR(l);

            im = module_load(scheme_module_resolve(midx, 1), env, NULL);

            start_module(im, 
                         menv2, 0, 
                         midx,
                         eval_exp, eval_run, base_phase,
                         new_cycle_list,
                         0);
          }
        } else {
          compute_require_names(menv, phase, env, syntax_idx);

          n = phase;
          menv2 = menv;
          while (scheme_is_positive(n)) {
            scheme_prepare_exp_env(menv2);
            menv2->exp_env->link_midx = menv2->link_midx;
            menv2 = menv2->exp_env;
            n = scheme_bin_minus(n, scheme_make_integer(1));
          }

          l = scheme_hash_get(menv->other_require_names, phase);

          for (; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
            midx = SCHEME_CAR(l);
            
            im = module_load(scheme_module_resolve(midx, 1), env, NULL);
            
            start_module(im, menv2, 0, midx, eval_exp, eval_run, base_phase, 
                         new_cycle_list, 0);
          }
        }
      }
    }
  }
}

typedef struct Start_Module_Args {
  Scheme_Env *menv;
  Scheme_Env *env;
  int eval_exp;
  int eval_run;
  intptr_t base_phase;
  Scheme_Object *cycle_list;
  Scheme_Object *syntax_idx;
} Start_Module_Args;

static void chain_start_module_w_push(Scheme_Env *menv, Scheme_Env *env, int eval_exp, int eval_run, 
                                      intptr_t base_phase, Scheme_Object *cycle_list, Scheme_Object *syntax_idx)
{
  Start_Module_Args a;
  
  a.menv = menv;
  a.env = env;
  a.eval_exp = eval_exp;
  a.eval_run = eval_run;
  a.base_phase = base_phase;
  a.cycle_list = cycle_list;
  a.syntax_idx = syntax_idx;

#ifdef MZ_USE_JIT
  (void)scheme_module_start_start(&a, scheme_make_pair(menv->module->modname, scheme_false));
#else
  (void)scheme_module_start_finish(&a);
#endif
}

void *scheme_module_start_finish(struct Start_Module_Args *a)
{
  chain_start_module(a->menv, a->env,
                     a->eval_exp, a->eval_run, a->base_phase,
                     a->cycle_list, a->syntax_idx);
  return NULL;
}

static Scheme_Env *instantiate_module(Scheme_Module *m, Scheme_Env *env, int restart, 
                                      Scheme_Object *syntax_idx, int not_new)
{
  Scheme_Env *menv;

  if (!restart) {
    menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(env->modchain), m->modname);
    if (menv) {
      check_phase(menv, env, 0);
      return menv;
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
    return menv;
  }

  menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(env->modchain), m->modname);
  if (!menv || restart) {
    Scheme_Object *insp;

    if (!menv) {
      char *running;

      if (not_new)
        scheme_signal_error("internal error: shouldn't instantiate module %s now",
                            scheme_write_to_string(m->modname, NULL));

      /* printf("new %ld %s\n", env->phase, SCHEME_SYM_VAL(m->modname)); */
      menv = scheme_new_module_env(env, m, 0);
      scheme_hash_set(MODCHAIN_TABLE(env->modchain), m->modname, (Scheme_Object *)menv);

      running = (char *)scheme_malloc_atomic(menv->module->num_phases);
      menv->running = running;
      memset(menv->running, 0, menv->module->num_phases);

      menv->phase = env->phase;
      menv->link_midx = syntax_idx;
    } else {
      Scheme_Env *env2;

      menv->module = m;
      memset(menv->running, 0, menv->module->num_phases);
      menv->ran = 0;
      menv->did_starts = NULL;

      for (env2 = menv->exp_env; env2; env2 = env2->exp_env) {
        env2->module = m;
      }
      for (env2 = menv->template_env; env2; env2 = env2->template_env) {
        env2->module = m;
      }
      env2 = menv->label_env;
      if (env2)
        env2->module = m;
    }

    menv->access_insp = m->insp;
    insp = scheme_make_inspector(m->insp);
    menv->guard_insp = insp;

    /* These three should be set by various "finish"es, but
       we initialize them in case there's an error running a "finish". */
    menv->require_names = scheme_null;
    menv->et_require_names = scheme_null;
    menv->tt_require_names = scheme_null;
    menv->dt_require_names = scheme_null;

    if (env->label_env != env) {
      setup_accessible_table(m);

      /* Create provided global variables: */
      if ((menv->phase <= 0)
          && ((menv->phase + m->num_phases) > 0)) {
        Scheme_Module_Phase_Exports *pt;
        Scheme_Object **exss, **exsns;
        int i, count;
        Scheme_Env *menv2 = menv;
        int pl;

        pl = -menv->phase;

        for (i = 0; i < pl; i++) {
          scheme_prepare_exp_env(menv2);
          menv2 = menv2->exp_env;
        }

        switch(pl) {
        case 0:
          pt = m->me->rt;
          break;
        case 1:
          pt = m->me->et;
          break;
        default:
          if (m->me->other_phases)
            pt = (Scheme_Module_Phase_Exports *)scheme_hash_get(m->me->other_phases, scheme_make_integer(pl));
          else
            pt = NULL;
          break;
        }

        if (pt) {
          exsns = pt->provide_src_names;
          exss = pt->provide_srcs;
          count = pt->num_var_provides;
          
          for (i = 0; i < count; i++) {
            if (SCHEME_FALSEP(exss[i]))
              scheme_add_to_table(menv2->toplevel, (const char *)exsns[i], NULL, 0);
          }
        }

        if (m->exp_infos[pl]) {
          count = m->exp_infos[pl]->num_indirect_provides;
          exsns = m->exp_infos[pl]->indirect_provides;
          for (i = 0; i < count; i++) {
            scheme_add_to_table(menv2->toplevel, (const char *)exsns[i], NULL, 0);
          }
        }
      }
    }
  }

  return menv;
}

static void expstart_module(Scheme_Env *menv, Scheme_Env *env, int phase, int restart)
{
  if (!restart) {
    if (menv && menv->running[phase])
      return;
  }

  if (menv->module->primitive)
    return;

  menv->running[phase] = 1;
  if (scheme_starting_up)
    menv->attached = 1; /* protect initial modules from redefinition, etc. */

  run_module_exptime(menv, phase);

  return;
}

static void run_module_exptime(Scheme_Env *menv, int phase)
{
#ifdef MZ_USE_JIT
  (void)scheme_module_exprun_start(menv, phase, scheme_make_pair(menv->module->modname, scheme_void));
#else
  (void)scheme_module_exprun_finish(menv, phase);
#endif
}

void *scheme_module_exprun_finish(Scheme_Env *menv, int at_phase)
{
  int let_depth, for_stx;
  Scheme_Object *names, *e;
  Resolve_Prefix *rp;
  Scheme_Comp_Env *rhs_env;
  int i, cnt, len;
  Scheme_Env *exp_env;
  Scheme_Bucket_Table *syntax;

  if (menv->module->primitive)
    return NULL;

  if ((menv->module->num_phases <= at_phase) || (!SCHEME_VEC_SIZE(menv->module->bodies[at_phase])))
    return NULL;

  for (i = 1; i < at_phase; i++) {
    scheme_prepare_exp_env(menv);
    if (!menv->exp_env->link_midx)
      menv->exp_env->link_midx = menv->link_midx;
    menv = menv->exp_env;
  }
  scheme_prepare_exp_env(menv);
  exp_env = menv->exp_env;
  if (!exp_env->link_midx)
      exp_env->link_midx = menv->link_midx;

  if (!exp_env)
    return NULL;

  syntax = menv->syntax;

  rhs_env = scheme_new_comp_env(menv, menv->access_insp, SCHEME_TOPLEVEL_FRAME);

  cnt = SCHEME_VEC_SIZE(menv->module->bodies[at_phase]);
  for (i = 0; i < cnt; i++) {
    e = SCHEME_VEC_ELS(menv->module->bodies[at_phase])[i];
      
    names = SCHEME_VEC_ELS(e)[0];
    let_depth = SCHEME_INT_VAL(SCHEME_VEC_ELS(e)[2]);
    rp = (Resolve_Prefix *)SCHEME_VEC_ELS(e)[3];
    for_stx = SCHEME_TRUEP(SCHEME_VEC_ELS(e)[4]);
    e = SCHEME_VEC_ELS(e)[1];
    
    if (for_stx) {
      names = NULL;
      len = 0;
    } else {
      if (SCHEME_SYMBOLP(names))
        names = scheme_make_pair(names, scheme_null);
      len = scheme_list_length(names);
    }

    eval_exptime(names, len, e, exp_env, rhs_env,
                 rp, let_depth, 1, (for_stx ? NULL : syntax), at_phase,
                 scheme_false, menv->access_insp);
  }

  return NULL;
}

static void do_start_module(Scheme_Module *m, Scheme_Env *menv, Scheme_Env *env, int restart)
{
  if (m->primitive) {
    menv->running[0] = 1;
    menv->ran = 1;
    return;
  }

  if (menv->running[0] > 0) {
    return;
  }
  
  menv->running[0] = 1;

  if (menv->module->prim_body) {
    Scheme_Invoke_Proc ivk = menv->module->prim_body;
    menv->ran = 1;
    ivk(menv, menv->phase, menv->link_midx, m->bodies[0]);
  } else {
    eval_module_body(menv, env);
  }
}

static void should_run_for_compile(Scheme_Env *menv, int phase)
{
  if (menv->running[phase]) return;

  if (!phase) {
    scheme_prepare_template_env(menv);
    menv = menv->template_env;
  } else {
    while (phase > 1) {
      scheme_prepare_exp_env(menv);
      menv = menv->exp_env;
      phase--;
    }
  }

#if 0
  if (!scheme_hash_get(MODCHAIN_TABLE(menv->instance_env->modchain), menv->module->modname))
    scheme_signal_error("internal error: inconsistent instance_env");
#endif

  if (!menv->available_next[0]) {
    menv->available_next[0] = MODCHAIN_AVAIL(menv->modchain, 0);
    MODCHAIN_AVAIL(menv->modchain, 0) = (Scheme_Object *)menv;
  }
}

static void start_module(Scheme_Module *m, Scheme_Env *env, int restart, 
                         Scheme_Object *syntax_idx, int eval_exp, int eval_run, intptr_t base_phase,
                         Scheme_Object *cycle_list, int not_new)
/* Make an instance of module `m' in `env', which means that phase level 0 of module `m'
   will be shifted to phase `env->phase'.
   Let P=`base_phase'-`env->phase'. 
    - If `eval_run', then instantiate phase-level P of `m' (which is at `base_phase' in `env').
    - If `eval_exp' is -1, then (also) make its P+1 phase-level ready.
    - If `eval_exp' is 1, then visit at phase P => run phase P+1. */
{
  Scheme_Env *menv;
  Scheme_Object *l;
  int prep_namespace = 0, i;

  if (is_builtin_modname(m->modname))
    return;

  for (l = cycle_list; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    if (SAME_OBJ(m->modname, SCHEME_CAR(l))) {
      scheme_contract_error("module",
                            "import cycle detected",
                            "module in cycle", 1, m->MODSRCNAME,
                            NULL);
    }
  }

  menv = instantiate_module(m, env, restart, syntax_idx, not_new);

  check_phase(menv, env, 0);

  show("chck", menv, eval_exp, eval_run, 0, base_phase);

  if (did_start(menv->did_starts, base_phase, eval_exp, eval_run))
    return;
  
  show("strt", menv, eval_exp, eval_run, 0, base_phase);
  show_indent(+1);

  {
    Scheme_Object *v;
    v = add_start(menv->did_starts, base_phase, eval_exp, eval_run);
    menv->did_starts = v;
  }

  chain_start_module_w_push(menv, env, eval_exp, eval_run, base_phase, cycle_list, syntax_idx);

  if (restart) {
    if (menv->rename_set_ready) {
      menv->rename_set_ready = 0;
      prep_namespace = 1;
    }
  }

  if (eval_run || eval_exp) {
    for (i = menv->module->num_phases; i-- ; ) {
      if (env->phase + i == base_phase) {
        if (eval_exp) {
          if (i + 1 < menv->module->num_phases) {
            if (eval_exp > 0) {
              show("exp=", menv, eval_exp, eval_run, i, base_phase);
              expstart_module(menv, env, i+1, restart);
            } else {
              should_run_for_compile(menv, i);
            }
          }
        }
        if (eval_run) {
          show("run=", menv, eval_exp, eval_run, i, base_phase);
          if (i == 0)
            do_start_module(m, menv, env, restart);
          else
            expstart_module(menv, env, i, restart);
        }
      } else if (env->phase + i > base_phase) {
        if (eval_exp) {
          should_run_for_compile(menv, i);
          if (eval_exp > 0) {
            if (env->phase + i == base_phase + 1) {
              show("run+", menv, eval_exp, eval_run, i, base_phase);
              if (i == 0)
                do_start_module(m, menv, env, restart);
              else
                expstart_module(menv, env, i, restart);
            }
          }
        }
      } else {
        /* env->phase + i < base_phase */
      }
    }

  }

  show_indent(-1);
  show_done("done", menv, eval_exp, eval_run, 0, base_phase);

  if (prep_namespace)
    scheme_prep_namespace_rename(menv);
}

static void do_prepare_compile_env(Scheme_Env *env, int base_phase, int pos)
{
  Scheme_Object *v, *prev;
  Scheme_Env *menv;
  int need_lock;

  need_lock = wait_registry(env);

  v = MODCHAIN_AVAIL(env->modchain, pos);
  if (!SCHEME_FALSEP(v)) {
    MODCHAIN_AVAIL(env->modchain, pos) = scheme_false;

    /* Reverse order of the list; if X requires Y, Y
       has been pushed onto the front of the list 
       before X. */
    prev = scheme_false;
    while (SCHEME_NAMESPACEP(v)) {
      menv = (Scheme_Env *)v;
      v = menv->available_next[pos];
      menv->available_next[pos] = prev;
      prev = (Scheme_Object *)menv;
    }
    v = prev;

    if (need_lock)
      lock_registry(env);

    while (SCHEME_NAMESPACEP(v)) {
      menv = (Scheme_Env *)v;
      v = menv->available_next[pos];
      menv->available_next[pos] = NULL;
      start_module(menv->module, menv->instance_env, 0,
                   NULL, 1, 0, base_phase,
                   scheme_null, 1);
    }

    if (need_lock)
      unlock_registry(env);
  }
}

void scheme_prepare_compile_env(Scheme_Env *env)
/* We're going to compile expressions at env->phase, so make sure
   that env->phase is visited. */
{
  do_prepare_compile_env(env, env->phase, 0);
}

static void *eval_module_body_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Env *menv, *env;

  menv = (Scheme_Env *)p->ku.k.p1;
  env = (Scheme_Env *)p->ku.k.p2;
  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  eval_module_body(menv, env);
  
  return NULL;
}

#if 0
# define LOG_RUN_DECLS intptr_t start_time
# define LOG_START_RUN(mod) (start_time = scheme_get_process_milliseconds())
# define LOG_END_RUN(mod) (printf("Ran %s [%d msec]\n", \
                                  scheme_write_to_string(mod->modname, NULL), \
                                  scheme_get_process_milliseconds() - start_time))
#else
# define LOG_RUN_DECLS /* empty */
# define LOG_START_RUN(mod) /* empty */
# define LOG_END_RUN(mod) /* empty */
#endif

static void eval_module_body(Scheme_Env *menv, Scheme_Env *env)
{
  if (menv->module->phaseless) {
    /* Phaseless modules are implemented by last-minute sharing of the
       `toplevels' table. In principle, much more repeated work up to
       this point could be skipped, but this is the simplest point to
       implement the sharing. */
    if (SAME_OBJ(scheme_true, menv->module->phaseless)) {
      menv->module->phaseless = (Scheme_Object *)menv->toplevel;
    } else {
      menv->toplevel = (Scheme_Bucket_Table *)menv->module->phaseless;
      return;
    }
  }

#ifdef MZ_USE_JIT
  (void)scheme_module_run_start(menv, env, scheme_make_pair(menv->module->MODSRCNAME, scheme_true));
#else
  (void)scheme_module_run_finish(menv, env);
#endif
}

static Scheme_Object *body_one_expr(void *prefix_plus_expr, int argc, Scheme_Object **argv)
{
  Scheme_Object *v, **saved_runstack;

  saved_runstack = scheme_resume_prefix(SCHEME_CAR((Scheme_Object *)prefix_plus_expr));
  v = _scheme_eval_linked_expr_multi(SCHEME_CDR((Scheme_Object *)prefix_plus_expr));
  scheme_suspend_prefix(saved_runstack);

  scheme_ignore_result(v);

  return scheme_void;
}

static int needs_prompt(Scheme_Object *e)
{
  Scheme_Type t;
  
  while (1) {
    t = SCHEME_TYPE(e);
    if (t > _scheme_values_types_)
      return 0;
  
    switch (t) {
    case scheme_unclosed_procedure_type:
    case scheme_toplevel_type:
    case scheme_local_type:
    case scheme_local_unbox_type:
      return 0;
    case scheme_case_lambda_sequence_type:
      return 0;
    case scheme_define_values_type:
      e = SCHEME_VEC_ELS(e)[0];
      break;
    case scheme_inline_variant_type:
      e = SCHEME_VEC_ELS(e)[0];
      break;
    default:
      return 1;
    }
  }
}

void *scheme_module_run_finish(Scheme_Env *menv, Scheme_Env *env)
{
  Scheme_Thread *p;
  Scheme_Module *m = menv->module;
  Scheme_Object *body, **save_runstack, *save_prefix;
  int depth;
  int i, cnt;
  Scheme_Cont_Frame_Data cframe;
  Scheme_Config *config;
  int volatile save_phase_shift;
  mz_jmp_buf newbuf, * volatile savebuf;
  LOG_RUN_DECLS;

  menv->running[0] = 1;
  menv->ran = 1;

  depth = m->max_let_depth + scheme_prefix_depth(m->prefix);
  if (!scheme_check_runstack(depth)) {
    p = scheme_current_thread;
    p->ku.k.p1 = menv;
    p->ku.k.p2 = env;
    (void)scheme_enlarge_runstack(depth, eval_module_body_k);
    return NULL;
  }

  LOG_START_RUN(menv->module);

  save_runstack = scheme_push_prefix(menv, m->prefix,
				     m->me->src_modidx, menv->link_midx,
				     0, menv->phase, NULL,
                                     menv->access_insp);

  p = scheme_current_thread;
  save_phase_shift = p->current_phase_shift;
  p->current_phase_shift = menv->phase;
  savebuf = p->error_buf;
  p->error_buf = &newbuf;

  if (scheme_setjmp(newbuf)) {
    Scheme_Thread *p2;
    p2 = scheme_current_thread;
    p2->error_buf = savebuf;
    p2->current_phase_shift = save_phase_shift;
    scheme_longjmp(*savebuf, 1);
  } else {
    if (env && menv->phase) {
      config = scheme_extend_config(scheme_current_config(),
                                    MZCONFIG_ENV,
                                    (Scheme_Object *)env);
      scheme_push_continuation_frame(&cframe);
      scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);
    }

    cnt = SCHEME_VEC_SIZE(m->bodies[0]);
    for (i = 0; i < cnt; i++) {
      body = SCHEME_VEC_ELS(m->bodies[0])[i];
      if (needs_prompt(body)) {
        /* We need to push the prefix after the prompt is set, so
           restore the runstack and then add the prefix back. */
        save_prefix = scheme_suspend_prefix(save_runstack);
        (void)_scheme_call_with_prompt_multi(body_one_expr, 
                                             scheme_make_raw_pair(save_prefix, body));
        scheme_resume_prefix(save_prefix);
      } else
        scheme_ignore_result(_scheme_eval_linked_expr_multi(body));
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

    if (env && menv->phase) {
      scheme_pop_continuation_frame(&cframe);
    }

    p = scheme_current_thread;
    p->error_buf = savebuf;
    p->current_phase_shift = save_phase_shift;

    scheme_pop_prefix(save_runstack);
  }

  LOG_END_RUN(menv->module);

  return NULL;
}

static void run_module(Scheme_Env *menv, int set_ns)
{
  Scheme_Cont_Frame_Data cframe;
  Scheme_Config *config;

  if (set_ns) {
    config = scheme_extend_config(scheme_current_config(),
                                  MZCONFIG_ENV,
                                  (Scheme_Object *)menv);
    
    scheme_push_continuation_frame(&cframe);
    scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);
  }
  
  eval_module_body(menv, NULL);

  if (set_ns) {
    scheme_pop_continuation_frame(&cframe);
  }
  
}

Scheme_Env *scheme_primitive_module(Scheme_Object *name, Scheme_Env *for_env)
{
  Scheme_Module *m;
  Scheme_Env *env;
  Scheme_Object *prefix, *insp, *src, *midx;
  Scheme_Config *config;
  char *running;

  m = MALLOC_ONE_TAGGED(Scheme_Module);
  m->so.type = scheme_module_type;
  m->predefined = scheme_starting_up;
  m->phaseless = scheme_true;
  
  env = scheme_new_module_env(for_env, m, 0);

  if (!scheme_defining_primitives) {
    config = scheme_current_config();
    prefix = scheme_get_param(config, MZCONFIG_CURRENT_MODULE_NAME);
    if (SCHEME_MODNAMEP(prefix))
      name = prefix;
    else
      name = scheme_intern_resolved_module_path(name);
    src = scheme_get_param(config, MZCONFIG_CURRENT_MODULE_SRC);
    if (SCHEME_FALSEP(src))
      src = prefix;
    else
      src = scheme_intern_resolved_module_path(src);
    if (SCHEME_FALSEP(src))
      src = name;
    insp = scheme_get_param(config, MZCONFIG_CODE_INSPECTOR);
  }
  else {
    name = scheme_intern_resolved_module_path(name);
    src = name;
    insp = scheme_get_current_inspector();
  }

  m->modname = name;
  m->modsrc = src;
  m->requires = scheme_null;
  m->et_requires = scheme_null;
  m->tt_requires = scheme_null;
  m->dt_requires = scheme_null;
  m->primitive = env;
  m->insp = insp;
  
  midx = scheme_make_modidx(scheme_false, scheme_false, name);
  m->self_modidx = midx;

  {
    Scheme_Module_Exports *me;
    me = scheme_make_module_exports();
    m->me = me;
    me->modsrc = src;
  }

  scheme_hash_set(for_env->module_registry->exports, m->modname, (Scheme_Object *)m->me);

  env->access_insp = insp;
  insp = scheme_make_inspector(insp);
  env->guard_insp = insp;

  scheme_hash_set(for_env->module_registry->loaded, m->modname, (Scheme_Object *)m);

  running = scheme_malloc_atomic(2);
  running[0] = 1;
  running[1] = 1;
  env->running = running;

  return env;
}

void scheme_set_primitive_module_phaseless(Scheme_Env *env, int phaseless)
{
  env->module->phaseless = (phaseless ? scheme_true : NULL); 
}

void scheme_finish_primitive_module(Scheme_Env *env)
{
  Scheme_Module *m = env->module;
  Scheme_Bucket_Table *ht;
  Scheme_Bucket **bs;
  Scheme_Object **exs;
  int i, count;

  if (!m->exp_infos)
    add_exp_infos(m);

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
 
  m->me->rt->provides = exs;
  m->me->rt->provide_srcs = NULL;
  m->me->rt->provide_src_names = exs;
  m->me->rt->num_provides = count;
  m->me->rt->num_var_provides = count;

  qsort_provides(exs, NULL, NULL, NULL, NULL, NULL, 0, count, 1);

  env->running[0] = 1;
}

void scheme_protect_primitive_provide(Scheme_Env *env, Scheme_Object *name)
{
  Scheme_Module *m = env->module;
  int i;

  if (!m->exp_infos)
    add_exp_infos(m);

  if (!m->exp_infos[0]->provide_protects) {
    Scheme_Hash_Table *ht;
    char *exps;
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    exps = MALLOC_N_ATOMIC(char, m->me->rt->num_provides);
    for (i = m->me->rt->num_provides; i--; ) {
      exps[i] = 0;
      scheme_hash_set(ht, m->me->rt->provides[i], scheme_make_integer(i));
    }
    add_exp_infos(m);
    m->exp_infos[0]->provide_protects = exps;
    m->exp_infos[0]->accessible = ht;
  }

  if (name) {
    for (i = m->me->rt->num_provides; i--; ) {
      if (SAME_OBJ(name, m->me->rt->provides[i])) {
	m->exp_infos[0]->provide_protects[i] = 1;
	break;
      }
    }
  } else {
    /* Protect all */
    for (i = m->me->rt->num_provides; i--; ) {
      m->exp_infos[0]->provide_protects[i] = 1;
    }
  }
}

Scheme_Bucket *scheme_module_bucket(Scheme_Object *modname, Scheme_Object *var, int pos, Scheme_Env *env)
{
  Scheme_Object *a[2];

  if (SAME_OBJ(modname, kernel_symbol))
    a[0] = ((Scheme_Modidx *)kernel_modidx)->path;
  else
    a[0] = modname;
  a[1] = var;

  return (Scheme_Bucket *)_dynamic_require(2, a, env, 1, 0, 0, 1, 1, pos);
}

Scheme_Object *scheme_builtin_value(const char *name)
{
  Scheme_Object *a[2], *v;

  a[1] = scheme_intern_symbol(name);

  /* Try kernel first: */
  a[0] = kernel_modname;
  v = _dynamic_require(2, a, scheme_get_env(NULL), 0, 0, 0, 0, 0, -1);
  if (v)
    return v;

  /* Try flfxnum next: */
  a[0] = flfxnum_modname;
  v = _dynamic_require(2, a, scheme_get_env(NULL), 0, 0, 0, 0, 0, -1);
  if (v)
    return v;

  /* Try extfl next: */
  a[0] = extfl_modname;
  v = _dynamic_require(2, a, scheme_get_env(NULL), 0, 0, 0, 0, 0, -1);
  if (v)
    return v;

  /* Try unsafe next: */
  a[0] = unsafe_modname;
  v = _dynamic_require(2, a, scheme_get_env(NULL), 0, 0, 0, 0, 0, -1);
  if (v)
    return v;

  /* Also try #%utils... */
  a[0] = scheme_make_pair(quote_symbol,
                          scheme_make_pair(scheme_intern_symbol("#%utils"),
                                           scheme_null));
  v = _dynamic_require(2, a, initial_modules_env, 0, 0, 0, 0, 0, -1);
  if (v)
    return v;

  return NULL;
}

Scheme_Module *scheme_extract_compiled_module(Scheme_Object *o)
{
  if (SAME_TYPE(SCHEME_TYPE(o), scheme_compilation_top_type)) {
    Scheme_Compilation_Top *c = (Scheme_Compilation_Top *)o;

    if (!c->prefix) /* => compiled module is in `code' field */
      return (Scheme_Module *)c->code;
    
    if (SAME_TYPE(SCHEME_TYPE(c->code), scheme_module_type)) {
      return (Scheme_Module *)c->code;
    }
  }

  return NULL;
}

Scheme_Module_Exports *scheme_make_module_exports()
{
  Scheme_Module_Exports *me;
  Scheme_Module_Phase_Exports *pt;

  me = MALLOC_ONE_RT(Scheme_Module_Exports);
  SET_REQUIRED_TAG(me->type = scheme_rt_module_exports);

  pt = MALLOC_ONE_RT(Scheme_Module_Phase_Exports);
  pt->so.type = scheme_module_phase_exports_type;
  pt->phase_index = scheme_make_integer(0);
  me->rt = pt;

  pt = MALLOC_ONE_RT(Scheme_Module_Phase_Exports);
  pt->so.type = scheme_module_phase_exports_type;
  pt->phase_index = scheme_make_integer(1);
  me->et = pt;

  pt = MALLOC_ONE_RT(Scheme_Module_Phase_Exports);
  pt->so.type = scheme_module_phase_exports_type;
  pt->phase_index = scheme_false;
  me->dt = pt;

  return me;
}

/**********************************************************************/
/*                          define-syntaxes                           */
/**********************************************************************/

static void *eval_exptime_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *names;
  int count, at_phase;
  Scheme_Object *expr;
  Scheme_Env *genv;
  Scheme_Comp_Env *comp_env;
  Resolve_Prefix *rp;
  int let_depth, shift;
  Scheme_Bucket_Table *syntax;
  Scheme_Object *free_id_rename_rn, *insp;

  names = (Scheme_Object *)p->ku.k.p1;
  expr = (Scheme_Object *)p->ku.k.p2;
  genv = (Scheme_Env *)SCHEME_VEC_ELS((Scheme_Object *)p->ku.k.p4)[0];
  comp_env = (Scheme_Comp_Env *)SCHEME_VEC_ELS((Scheme_Object *)p->ku.k.p4)[1];
  free_id_rename_rn = SCHEME_VEC_ELS((Scheme_Object *)p->ku.k.p4)[2];
  rp = (Resolve_Prefix *)SCHEME_VEC_ELS((Scheme_Object *)p->ku.k.p4)[3];
  syntax = (Scheme_Bucket_Table *)SCHEME_VEC_ELS((Scheme_Object *)p->ku.k.p4)[4];
  insp = SCHEME_VEC_ELS((Scheme_Object *)p->ku.k.p4)[5];
  count = p->ku.k.i1;
  let_depth = p->ku.k.i2;
  shift = p->ku.k.i3;
  at_phase = p->ku.k.i4;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;
  p->ku.k.p5 = NULL;

  eval_exptime(names, count, expr, genv, comp_env, rp, let_depth, shift, syntax, at_phase, 
               free_id_rename_rn, insp);

  return NULL;
}

static int is_simple_expr(Scheme_Object *v)
{
  Scheme_Type t;

  t = SCHEME_TYPE(v);
  if (SAME_TYPE(t, scheme_unclosed_procedure_type))
    return 1;

  return 0;
}

static void eval_exptime(Scheme_Object *names, int count,
                         Scheme_Object *expr, 
                         Scheme_Env *genv, Scheme_Comp_Env *comp_env,
                         Resolve_Prefix *rp,
                         int let_depth, int shift, Scheme_Bucket_Table *syntax,
                         int at_phase,
                         Scheme_Object *free_id_rename_rn,
                         Scheme_Object *insp)
{
  Scheme_Object *macro, *vals, *name, **save_runstack;
  int i, g, depth;

  depth = let_depth + scheme_prefix_depth(rp);
  if (!scheme_check_runstack(depth)) {
    Scheme_Thread *p = scheme_current_thread;
    p->ku.k.p1 = names;
    p->ku.k.p2 = expr;
    vals = scheme_make_vector(6, NULL);
    SCHEME_VEC_ELS(vals)[0] = (Scheme_Object *)genv;
    SCHEME_VEC_ELS(vals)[1] = (Scheme_Object *)comp_env;
    SCHEME_VEC_ELS(vals)[2] = free_id_rename_rn;
    SCHEME_VEC_ELS(vals)[3] = (Scheme_Object *)rp;
    SCHEME_VEC_ELS(vals)[4] = (Scheme_Object *)syntax;
    SCHEME_VEC_ELS(vals)[5] = insp;
    p->ku.k.p4 = vals;
    p->ku.k.i1 = count;
    p->ku.k.i2 = let_depth;
    p->ku.k.i3 = shift;
    p->ku.k.i4 = at_phase;
    (void)scheme_enlarge_runstack(depth, eval_exptime_k);
    return;
  }

  if (SCHEME_TYPE(expr) > _scheme_values_types_) {
    vals = expr;
  } else {
    save_runstack = scheme_push_prefix(genv, rp,
                                       (shift ? genv->module->me->src_modidx : NULL), 
                                       (shift ? genv->link_midx : NULL), 
                                       at_phase, genv->phase,
                                       NULL, insp);

    if (is_simple_expr(expr)) {
      vals = _scheme_eval_linked_expr_multi_wp(expr, scheme_current_thread);
    } else {
      Scheme_Cont_Frame_Data cframe;
      Scheme_Config *config;
      Scheme_Dynamic_State dyn_state;

      config = scheme_extend_config(scheme_current_config(),
                                    MZCONFIG_ENV,
                                    (Scheme_Object *)genv);
      scheme_push_continuation_frame(&cframe);
      scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);
    
      scheme_set_dynamic_state(&dyn_state, comp_env, NULL, scheme_false,
                               genv, (genv->link_midx ? genv->link_midx : genv->module->me->src_modidx));
      vals = scheme_eval_linked_expr_multi_with_dynamic_state(expr, &dyn_state);

      scheme_pop_continuation_frame(&cframe);
    }

    scheme_pop_prefix(save_runstack);
  }
  
  if (names) {
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

          macro = scheme_alloc_small_object();
          macro->type = scheme_macro_type;
          SCHEME_PTR_VAL(macro) = values[i];
          
          if (SCHEME_TRUEP(free_id_rename_rn)
              && scheme_is_binding_rename_transformer(values[i]))
            scheme_install_free_id_rename(name, scheme_rename_transformer_id(values[i]), free_id_rename_rn, 
                                          scheme_make_integer(0));
	
          scheme_add_to_table(syntax, (const char *)name, macro, 0);
        }
	
        return;
      }
    } else if (SCHEME_PAIRP(names) && SCHEME_NULLP(SCHEME_CDR(names))) {
      name = SCHEME_CAR(names);

      macro = scheme_alloc_small_object();
      macro->type = scheme_macro_type;
      SCHEME_PTR_VAL(macro) = vals;
      
      if (SCHEME_TRUEP(free_id_rename_rn)
          && scheme_is_binding_rename_transformer(vals))
        scheme_install_free_id_rename(name, scheme_rename_transformer_id(vals), free_id_rename_rn, 
                                      scheme_make_integer(0));
      
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

      scheme_wrong_return_arity("define-syntaxes",
                                count, g,
                                (g == 1) ? (Scheme_Object **)vals : scheme_current_thread->ku.multiple.array,
                                "%s%s%s",
                                name ? "defining \"" : "0 names",
                                symname,
                                name ? ((count == 1) ? "\"" : "\", ...") : "");
    }  
  }
}

/**********************************************************************/
/*                               module                               */
/**********************************************************************/

static Scheme_Object *do_module_execute(Scheme_Object *data, Scheme_Env *genv, 
                                        int set_cache, int set_in_pre, 
                                        Scheme_Object *prefix,
                                        Scheme_Object *supermodule);

static Scheme_Object *do_module_execute_k()
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *data = (Scheme_Object *)p->ku.k.p1;
  Scheme_Env *genv = (Scheme_Env *)p->ku.k.p2;
  Scheme_Object *prefix = (Scheme_Object *)p->ku.k.p3;
  Scheme_Object *supermodule = (Scheme_Object *)p->ku.k.p4;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;

  return do_module_execute(data, genv, p->ku.k.i1, p->ku.k.i2, prefix, supermodule);
}

static Scheme_Object *do_module_execute_recur(Scheme_Object *data, Scheme_Env *genv, 
                                              int set_cache, int set_in_pre, 
                                              Scheme_Object *prefix,
                                              Scheme_Object *supermodule)
{
# include "mzstkchk.h"
  {
    Scheme_Thread *p = scheme_current_thread;
    p->ku.k.p1 = (void *)data;
    p->ku.k.p2 = (void *)genv;
    p->ku.k.i1 = set_cache;
    p->ku.k.i2 = set_in_pre;
    p->ku.k.p3 = (void *)prefix;
    p->ku.k.p4 = (void *)supermodule;
    return scheme_handle_stack_overflow(do_module_execute_k);
  } else {
    return do_module_execute(data, genv, set_cache, set_in_pre, prefix, supermodule);
  }
}

static void execute_submodules(Scheme_Module *m, int pre, Scheme_Env *genv, 
                               int set_cache, int set_in_pre, 
                               Scheme_Object *prefix)
{
  Scheme_Object *p;

  p = (pre ? m->pre_submodules : m->post_submodules);

  if (p) {
    if (SCHEME_PAIRP(scheme_resolved_module_path_value(prefix))) {
      prefix = scheme_resolved_module_path_value(prefix);
      prefix = scheme_intern_resolved_module_path(SCHEME_CAR(prefix));
    }
    
    while (!SCHEME_NULLP(p)) {
      do_module_execute_recur(SCHEME_CAR(p), genv, set_cache, set_in_pre, prefix, 
                              (Scheme_Object *)m);
      p = SCHEME_CDR(p);
    }
  }
}

static Scheme_Object *do_module_execute(Scheme_Object *data, Scheme_Env *genv, 
                                        int set_cache, int set_in_pre, 
                                        Scheme_Object *prefix,
                                        Scheme_Object *supermodule)
{
  Scheme_Module *m, *old_m;
  Scheme_Env *env;
  Scheme_Env *old_menv;
  Scheme_Config *config;
  Scheme_Object *src, *insp;

  m = MALLOC_ONE_TAGGED(Scheme_Module);
  memcpy(m, data, sizeof(Scheme_Module));

  if (set_cache && m->code_key) {
    if (!scheme_module_code_cache) {
      REGISTER_SO(scheme_module_code_cache);
      scheme_module_code_cache = scheme_make_weak_equal_table();
    }
    scheme_add_to_table(scheme_module_code_cache,
                        (const char *)m->code_key,
                        scheme_make_ephemeron(m->code_key, data),
                        0);
  }

  if (m->code_key) {
    /* clone `requires', etc., so that different uses of the cached
       module don't share resolution of modiule paths in modidxs */
    clone_all_require_names(m);
  }

  config = scheme_current_config();

  if (!prefix)
    prefix = scheme_get_param(config, MZCONFIG_CURRENT_MODULE_NAME);
  
  if (SCHEME_MODNAMEP(prefix)) {
    if (m->submodule_path && !SCHEME_NULLP(m->submodule_path)) {
      prefix = scheme_make_pair(scheme_resolved_module_path_value(prefix),
                                m->submodule_path);
      prefix = scheme_intern_resolved_module_path(prefix);
    }

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
          v = m->rn_stx;
          if (SCHEME_PAIRP(v))
            v = scheme_list_to_vector(v);
	  v = scheme_make_pair(v, (Scheme_Object *)midx);
	  m->rn_stx = v;
	}
      }
    }
  } else
    prefix = m->modname; /* used for submodules */

  src = scheme_get_param(config, MZCONFIG_CURRENT_MODULE_SRC);
  if (!SCHEME_FALSEP(src)) {
    src = scheme_intern_resolved_module_path(src);
    m->modsrc = src;
  } else {
    src = m->modname;
    if (m->submodule_path && !SCHEME_NULLP(m->submodule_path)) {
      src = scheme_resolved_module_path_value(src);
      if (SCHEME_PAIRP(src))
        src = SCHEME_CAR(src);
      src = scheme_intern_resolved_module_path(src);
    }
    m->modsrc = src;
  }

  if (supermodule)
    m->supermodule = supermodule;

  if (genv)
    env = genv;
  else
    env = scheme_environment_from_dummy(m->dummy);

  old_menv = get_special_modenv(m->modname);
  if (!old_menv)
    old_menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(env->modchain), m->modname);

  insp = scheme_get_param(config, MZCONFIG_CODE_INSPECTOR);
  
  if (old_menv) {
    if (scheme_module_protected_wrt(old_menv->guard_insp, insp) || old_menv->attached) {
      scheme_contract_error("module->namespace",
                            "current code inspector cannot redeclare module",
                            "module name", 1, m->modname,
                            NULL);
      return NULL;
    }
  }

  if (old_menv)
    old_m = old_menv->module;
  else
    old_m = (Scheme_Module *)scheme_hash_get(env->module_registry->loaded, m->modname);
  
  if (old_m && old_m->phaseless) {
    scheme_contract_error("module->namespace",
                          "cannot redeclare cross-phase persistent module",
                          "module name", 1, m->modname,
                          NULL);
    return NULL;
  }

  if (!set_in_pre) {
    /* execute pre-submodules: */
    execute_submodules(m, 1, genv, set_cache, set_in_pre, prefix);
  }

  if (!SAME_OBJ(m->me->modsrc, m->modsrc)) {
    /* have to clone m->me, etc. */
    Scheme_Module_Exports *naya_me;

    naya_me = MALLOC_ONE_TAGGED(Scheme_Module_Exports);
    memcpy(naya_me, m->me, sizeof(Scheme_Module_Exports));
    m->me = naya_me;
    m->me->modsrc = m->modsrc;
  }

  m->insp = insp;
  if (set_in_pre) {
    if (!env->module_pre_registry->loaded) {
      Scheme_Hash_Table *ht;
      ht = scheme_make_hash_table(SCHEME_hash_ptr);
      env->module_pre_registry->loaded = ht;
      ht = scheme_make_hash_table(SCHEME_hash_ptr);
      env->module_pre_registry->exports = ht;
    }
    scheme_hash_set(env->module_pre_registry->loaded, m->modname, (Scheme_Object *)m);
    scheme_hash_set(env->module_pre_registry->exports, m->modname, (Scheme_Object *)m->me);
  } else {
    scheme_hash_set(env->module_registry->loaded, m->modname, (Scheme_Object *)m);
    scheme_hash_set(env->module_registry->exports, m->modname, (Scheme_Object *)m->me);
  }

  if (!set_in_pre) {
    Scheme_Object *resolver, *a[2];
    resolver = scheme_get_param(config, MZCONFIG_CURRENT_MODULE_RESOLVER);
    a[0] = m->modname;
    a[1] = scheme_false;
    scheme_apply(resolver, 2, a);
  }

  /* Replacing an already-running or already-syntaxing module? */
  if (old_menv) {
    start_module(m, env, 1, NULL, 
                 ((m->num_phases > 1) ? old_menv->running[1] : 0), 
                 old_menv->running[0], 
                 env->phase, scheme_null, 1);
  }

  /* execute post-submodules: */
  execute_submodules(m, 0, genv, set_cache, set_in_pre, prefix);

  return scheme_void;
}

Scheme_Object *scheme_module_execute(Scheme_Object *data, Scheme_Env *genv)
{
  return do_module_execute(data, genv, 1, 0, NULL, NULL);
}

static Scheme_Object *rebuild_et_vec(Scheme_Object *naya, Scheme_Object *vec, Resolve_Prefix *rp)
{
  Scheme_Object *vec2;
  int i;
  
  i = SCHEME_VEC_SIZE(vec);
  vec2 = scheme_make_vector(i, NULL);
  while (i--) {
    SCHEME_VEC_ELS(vec2)[i] = SCHEME_VEC_ELS(vec)[i];
  }
  SCHEME_VEC_ELS(vec2)[1] = naya;
  SCHEME_VEC_ELS(vec2)[3] = (Scheme_Object *)rp;

  return vec2;
}

static Scheme_Object *jit_vector(Scheme_Object *orig_l, int in_vec, int jit)
{
  Scheme_Object *orig, *naya = NULL;
  Resolve_Prefix *orig_rp, *rp;
  int i, cnt;

  cnt = SCHEME_VEC_SIZE(orig_l);
  for (i = 0; i < cnt; i++) {
    orig = SCHEME_VEC_ELS(orig_l)[i];
    if (in_vec) {
      orig_rp = (Resolve_Prefix *)SCHEME_VEC_ELS(orig)[3];
      rp = scheme_prefix_eval_clone(orig_rp);
      orig = SCHEME_VEC_ELS(orig)[1];
    } else {
      orig_rp = rp = NULL;
    }

    if (jit)
      naya = scheme_jit_expr(orig);
    else
      naya = orig;

    if (!SAME_OBJ(orig, naya)
        || !SAME_OBJ(orig_rp, rp))
      break;
  }

  if (i < cnt) {
    Scheme_Object *new_l;
    int j;
    new_l = scheme_make_vector(cnt, NULL);
    for (j = 0; j < i; j++) {
      SCHEME_VEC_ELS(new_l)[j] = SCHEME_VEC_ELS(orig_l)[j];
    }
    if (in_vec)
      naya = rebuild_et_vec(naya, SCHEME_VEC_ELS(orig_l)[i], rp);
    SCHEME_VEC_ELS(new_l)[i] = naya;
    for (i++; i < cnt; i++) {
      orig = SCHEME_VEC_ELS(orig_l)[i];
      if (in_vec) {
        orig_rp = (Resolve_Prefix *)SCHEME_VEC_ELS(orig)[3];
        rp = scheme_prefix_eval_clone(orig_rp);
        orig = SCHEME_VEC_ELS(orig)[1];        
      } else {
        orig_rp = rp = NULL;
      }

      if (jit)
        naya = scheme_jit_expr(orig);
      else
        naya = orig;

      if (in_vec) {
	if (!SAME_OBJ(orig, naya)
            || !SAME_OBJ(rp, orig_rp))
	  naya = rebuild_et_vec(naya, SCHEME_VEC_ELS(orig_l)[i], rp);
	else
	  naya = SCHEME_VEC_ELS(orig_l)[i];
      }
      SCHEME_VEC_ELS(new_l)[i] = naya;
    }
    return new_l;
  } else
    return orig_l;
}

static Scheme_Object *do_module_clone(Scheme_Object *data, int jit)
{
  Scheme_Module *m = (Scheme_Module *)data;
  Scheme_Object *l1, *l2, *pre_submods, *post_submods, *sm, **naya = NULL;
  int j, i, submod_changed;
  Resolve_Prefix *rp;
  
  rp = scheme_prefix_eval_clone(m->prefix);

  for (j = m->num_phases; j--; ) {
    if (!jit && !j) {
      if (naya) 
        naya[0] = m->bodies[0];
      break;
    }
    l1 = jit_vector(m->bodies[j], j > 0, jit);
    if (naya)
      naya[j] = l1;
    else if (!SAME_OBJ(l1, m->bodies[j])) {
      naya = MALLOC_N(Scheme_Object*, m->num_phases);
      for (i = m->num_phases; i-- > j; ) {
        naya[i] = m->bodies[i];
      }
      naya[j] = l1;
    }
  }

  pre_submods = m->pre_submodules;
  post_submods = m->post_submodules;
  submod_changed = 0;

  for (j = 0; j < 2; j++) {
    l1 = (j ? post_submods : pre_submods);
    if (l1 && !SCHEME_NULLP(l1)) {
      l2 = scheme_null;
      while (!SCHEME_NULLP(l1)) {
        sm = do_module_clone(SCHEME_CAR(l1), jit);
        if (!SAME_OBJ(sm, SCHEME_CAR(l1)))
          submod_changed = 1;
        l2 = scheme_make_pair(sm, l2);
        l1 = SCHEME_CDR(l1);
      }
      if (submod_changed) {
        l2 = scheme_reverse(l2);
        if (j)
          post_submods = l2;
        else
          pre_submods = l2;
      }
    }
  }

  if (!naya) {
    if (SAME_OBJ(rp, m->prefix) && !submod_changed)
      return data;
    naya = m->bodies;
  }
  
  m = MALLOC_ONE_TAGGED(Scheme_Module);
  memcpy(m, data, sizeof(Scheme_Module));
  m->bodies = naya;
  m->prefix = rp;

  m->pre_submodules = pre_submods;
  m->post_submodules = post_submods;

  return (Scheme_Object *)m;
}

Scheme_Object *scheme_module_jit(Scheme_Object *data)
{
  return do_module_clone(data, 1);
}

Scheme_Object *scheme_module_eval_clone(Scheme_Object *data)
{
  return do_module_clone(data, 0);
}

static Scheme_Object *strip_lexical_context(Scheme_Object *stx);

static Scheme_Object *strip_lexical_context_k()
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *v = (Scheme_Object *)p->ku.k.p1;

  p->ku.k.p1 = NULL;

  return strip_lexical_context(v);
}

static Scheme_Object *strip_lexical_context(Scheme_Object *stx)
{
  Scheme_Object *v = NULL;

#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Thread *p = scheme_current_thread;

    p->ku.k.p1 = (void *)v;
    
    return scheme_handle_stack_overflow(strip_lexical_context_k);
  }
#endif
  
  if (SCHEME_STXP(stx)) {
    stx = scheme_stx_taint_disarm(stx, NULL);
    v = SCHEME_STX_VAL(stx);
  } else
    v = stx;

  if (SCHEME_PAIRP(v)) {
    v = scheme_make_pair(strip_lexical_context(SCHEME_CAR(v)),
                         strip_lexical_context(SCHEME_CDR(v)));
  } else if (SCHEME_VECTORP(v)) {
    Scheme_Object *v2, *a;
    int i = SCHEME_VEC_SIZE(v);
    v2 = scheme_make_vector(i, NULL);
    for (; i--; ) {
      a = strip_lexical_context(SCHEME_VEC_ELS(v)[i]);
      SCHEME_VEC_ELS(v2)[i] = a;
    }
  } else if (SCHEME_BOXP(v)) {
    v = strip_lexical_context(SCHEME_BOX_VAL(v));
    v = scheme_box(v);
  }
  /* FIXME: handle prefabs & hashes */

  if (SCHEME_STXP(stx))
    v = scheme_datum_to_syntax(v, stx, scheme_false, 0, 1);

  return v;
}

static void check_not_tainted(Scheme_Object *orig)
{
  if (scheme_stx_is_tainted(orig))
    scheme_wrong_syntax(NULL, orig, NULL,
			"cannot expand module body tainted by macro expansion");
}

static Scheme_Object *do_annotate_submodules_k(void);

Scheme_Object *do_annotate_submodules(Scheme_Object *fm, int phase, int incl_star)
{
  Scheme_Object *a, *d, *v, *fm2;
  int changed = 0;

#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Thread *p = scheme_current_thread;
    p->ku.k.p1 = (void *)fm;
    p->ku.k.i1 = phase;
    p->ku.k.i2 = incl_star;
    return scheme_handle_stack_overflow(do_annotate_submodules_k);
  }
#endif

  if (SCHEME_STXP(fm))
    check_not_tainted(fm);

  if (!SCHEME_STX_PAIRP(fm))
    return fm;

  if (SCHEME_STXP(fm))
    fm2 = scheme_stx_taint_disarm(fm, NULL);
  else
    fm2 = fm;

  a = SCHEME_STX_CAR(fm2);
  if (SCHEME_STX_PAIRP(a)) {
    a = scheme_stx_taint_disarm(a, NULL);
    v = SCHEME_STX_CAR(a);
    if (SCHEME_STX_SYMBOLP(v)) {
      if (scheme_stx_module_eq3(scheme_module_stx, v, 
                                scheme_make_integer(0), scheme_make_integer(phase), 
                                NULL)
          || (incl_star
              && scheme_stx_module_eq3(scheme_modulestar_stx, v, 
                                       scheme_make_integer(0), scheme_make_integer(phase), 
                                       NULL))) {
        /* found a submodule */
        v = scheme_stx_property(a, scheme_intern_symbol("submodule"), NULL);
        if (SCHEME_FALSEP(v)) {
          a = scheme_stx_property(a, scheme_intern_symbol("submodule"), a);
          changed = 1;
        }
      } else if (scheme_stx_module_eq3(scheme_begin_for_syntax_stx, v, 
                                       scheme_make_integer(0), scheme_make_integer(phase), 
                                       NULL)) {
        /* found `begin-for-syntax' */
        v = do_annotate_submodules(a, phase+1, incl_star);
        if (!SAME_OBJ(v, a)) {
          changed = 1;
          a = v;
        }
      } else if (scheme_stx_module_eq3(scheme_begin_stx, v, 
                                       scheme_make_integer(0), scheme_make_integer(phase), 
                                       NULL)) {
        /* found `begin' */
        v = do_annotate_submodules(a, phase, incl_star);
        if (!SAME_OBJ(v, a)) {
          changed = 1;
          a = v;
        }
      }
    }
  }

  v = SCHEME_STX_CDR(fm2);
  d = do_annotate_submodules(v, phase, incl_star);

  if (!changed && SAME_OBJ(v, d))
    return fm;
   
  v = scheme_make_pair(a, d);
  if (SCHEME_STXP(fm))
    v = scheme_datum_to_syntax(v, fm, fm, 0, 2);

  return v;
}

static Scheme_Object *do_annotate_submodules_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *fm = (Scheme_Object *)p->ku.k.p1;

  p->ku.k.p1 = NULL;

  return do_annotate_submodules(fm, p->ku.k.i1, p->ku.k.i2);
}

Scheme_Object *scheme_annotate_existing_submodules(Scheme_Object *orig_fm, int incl_star)
{
  Scheme_Object *fm = orig_fm;

  if (!SCHEME_STX_PAIRP(fm))
    return orig_fm;
  fm = SCHEME_STX_CAR(fm);
  if (!SCHEME_STX_SYMBOLP(fm))
    return orig_fm;

  if (scheme_stx_module_eq(scheme_module_begin_stx, fm, 0)) {
    /* It's a `#%plain-module-begin' form */
    return do_annotate_submodules(orig_fm, 0, incl_star);
  }

  return orig_fm;
}

static Scheme_Object *phase_shift_tail(Scheme_Object *v, Scheme_Object *ps)
{
  if (!SCHEME_STXP(v))
    v = scheme_datum_to_syntax(v, scheme_false, scheme_false, 0, 0);

  return scheme_add_rename(v, ps);
}

static Scheme_Object *rebuild_with_phase_shift(Scheme_Object *orig, Scheme_Object *a, Scheme_Object *d, 
                                               Scheme_Object *ps)
{
  if (!a) {
    a = orig;
    if (SCHEME_STXP(a))
      a = scheme_stx_taint_disarm(a, NULL);
    a = SCHEME_STX_CAR(a);
    a = scheme_add_rename(a, ps);
  }
  if (!d) {
    d = orig;
    if (SCHEME_STXP(d))
      d = scheme_stx_taint_disarm(d, NULL);
    d = SCHEME_STX_CDR(d);
    d = phase_shift_tail(d, ps);
  }

  a = scheme_make_pair(a, d);

  if (SCHEME_PAIRP(orig))
    return a;

  check_not_tainted(orig);

  orig = scheme_add_rename(orig, ps);
  return scheme_datum_to_syntax(a, orig, orig, 0, 2);
}

static Scheme_Object *phase_shift_skip_submodules_k(void);

static Scheme_Object *phase_shift_skip_submodules(Scheme_Object *fm, Scheme_Object *ps, int phase)
{
  Scheme_Object *v0, *v1, *v2, *v3, *v4, *naya;

#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Thread *p = scheme_current_thread;
    p->ku.k.p1 = (void *)fm;
    p->ku.k.p2 = (void *)ps;
    p->ku.k.i1 = phase;
    return scheme_handle_stack_overflow(phase_shift_skip_submodules_k);
  }
#endif

  if (phase == -1) {
    /* at top, so this is a `module[*]' form: */
    v0 = fm;
    if (SCHEME_STXP(v0))
      v0 = scheme_stx_taint_disarm(v0, NULL);
    v0 = SCHEME_STX_CDR(v0);
    v1 = SCHEME_STX_CDR(v0);
    v2 = SCHEME_STX_CDR(v1);
    v3 = SCHEME_STX_CAR(v2);
    v4 = scheme_stx_taint_disarm(v3, NULL);
    v4 = SCHEME_STX_CDR(v4);

    naya = phase_shift_skip_submodules(v4, ps, 0);
    if (SAME_OBJ(naya, v4)) {
      return scheme_add_rename(fm, ps);
    } else {
      v3 = rebuild_with_phase_shift(v3, NULL, naya, ps);
      v2 = rebuild_with_phase_shift(v2, v3, NULL, ps);
      v1 = rebuild_with_phase_shift(v1, NULL, v2, ps);
      v0 = rebuild_with_phase_shift(v0, NULL, v1, ps);
      return rebuild_with_phase_shift(fm, NULL, v0, ps);
    }
  } else if (SCHEME_STX_NULLP(fm)) {
    return fm;
  } else {
    v0 = fm;
    if (SCHEME_STXP(v0))
      v0 = scheme_stx_taint_disarm(v0, NULL);
    v1 = SCHEME_STX_CAR(v0);
    
    if (SCHEME_STX_PAIRP(v1)) {
      if (SCHEME_STXP(v1))
        v1 = scheme_stx_taint_disarm(v1, NULL);  
      v2 = SCHEME_STX_CAR(v1);
      if (SCHEME_STX_SYMBOLP(v2)) {
        if (scheme_stx_module_eq_x(scheme_module_stx, v2, phase)
            || scheme_stx_module_eq_x(scheme_modulestar_stx, v2, phase)) {
          /* found a submodule */
          v2 = SCHEME_STX_CDR(fm);
          naya = phase_shift_skip_submodules(v2, ps, phase);
          if (SAME_OBJ(naya, v2))
            naya = phase_shift_tail(naya, ps);
          return rebuild_with_phase_shift(fm, v1, naya, ps);
        } else if (scheme_stx_module_eq_x(scheme_begin_for_syntax_stx, v2, phase)) {
          /* found `begin-for-syntax': */
          naya = phase_shift_skip_submodules(v1, ps, phase+1);
          v2 = SCHEME_STX_CDR(fm);
          v3 = phase_shift_skip_submodules(v2, ps, phase);
          if (SAME_OBJ(naya, v1) && SAME_OBJ(v2, v3))
            return fm;
          else {
            if (SAME_OBJ(naya, v1))
              naya = phase_shift_tail(naya, ps);
            if (SAME_OBJ(v2, v3))
              v3 = phase_shift_tail(v3, ps);
            return rebuild_with_phase_shift(fm, naya, v3, ps);
          }
        }
      }
    }

    v3 = SCHEME_STX_CDR(fm);
    v4 = phase_shift_skip_submodules(v3, ps, phase);
    if (SAME_OBJ(v3, v4))
      return fm;
    else {
      v1 = scheme_add_rename(v1, ps);
      return rebuild_with_phase_shift(fm, v1, v4, ps);
    }
  }
}

static Scheme_Object *phase_shift_skip_submodules_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *fm = (Scheme_Object *)p->ku.k.p1;
  Scheme_Object *ps = (Scheme_Object *)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return phase_shift_skip_submodules(fm, ps, p->ku.k.i1);
}

static Scheme_Env *find_env(Scheme_Env *env, intptr_t ph)
{
  intptr_t j;

  if (ph > 0) {
    for (j = 0; j < ph; j++) {
      scheme_prepare_exp_env(env);
      env = env->exp_env;
    }
  } else if (ph < 0) {
    for (j = 0; j > ph; j--) {
      scheme_prepare_template_env(env);
      env = env->template_env;
    }
  }

  return env;
}

static Scheme_Object *extract_root_module_name(Scheme_Module *m)
{
  Scheme_Object *root_module_name;

  root_module_name = m->submodule_ancestry;
  if (SCHEME_NULLP(root_module_name)) {
    root_module_name = m->modname;
  } else {
    while (SCHEME_PAIRP(SCHEME_CDR(root_module_name))) {
      root_module_name = SCHEME_CDR(root_module_name);
    }
    root_module_name = ((Scheme_Env *)SCHEME_CAR(root_module_name))->module->modname;
  }

  return root_module_name;
}

#if 0
# define LOG_EXPAND_DECLS intptr_t start_time
# define LOG_START_EXPAND(mod) (start_time = scheme_get_process_milliseconds())
# define LOG_END_EXPAND(mod) (printf("Expanded/compiled %s [%d msec]\n", \
                                     scheme_write_to_string(mod->modname, NULL), \
                                     scheme_get_process_milliseconds() - start_time))
#else
# define LOG_EXPAND_DECLS /* empty */
# define LOG_START_EXPAND(mod) /* empty */
# define LOG_END_EXPAND(mod) /* empty */
#endif

static Scheme_Object *do_module(Scheme_Object *form, Scheme_Comp_Env *env, 
				Scheme_Compile_Expand_Info *rec, int drec,
                                Scheme_Object *submodule_ancestry, Scheme_Object *submodule_path, int post,
                                Module_Begin_Expand_State *super_bxs,
                                Scheme_Object *super_phase_shift)
{
  Scheme_Object *fm, *nm, *ii, *iidx, *self_modidx, *rmp, *rn_set, *mb_ctx;
  Scheme_Module *iim;
  Scheme_Env *menv, *top_env;
  Scheme_Comp_Env *benv;
  Scheme_Module *m;
  Scheme_Object *mbval, *orig_ii;
  Scheme_Object *this_empty_self_modidx;
  int saw_mb, check_mb = 0, skip_strip = 0;
  Scheme_Object *restore_confusing_name = NULL;
  LOG_EXPAND_DECLS;

  if (!rec[drec].comp) {
    SCHEME_EXPAND_OBSERVE_PRIM_MODULE(rec[drec].observer);
    if (rec[drec].depth > 0)
      rec[drec].depth++;
  }

  if (scheme_is_nested_module(env)) {
    fm = scheme_stx_property(form, scheme_intern_symbol("submodule"), NULL);
    if (SCHEME_STXP(fm)) {
      form = fm;
      skip_strip = 1;
    } else
      skip_strip = 0;
  }

  if (!scheme_is_toplevel(env))
    scheme_wrong_syntax(NULL, NULL, form, "not in a module-definition context");

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
  orig_ii = ii;

  if (scheme_is_nested_module(env)) {
    if (post && SCHEME_FALSEP(SCHEME_STX_VAL(ii))) {
      ii = NULL;
    } else {
      super_phase_shift = scheme_make_integer(0);
      if (!skip_strip) {
        ii = strip_lexical_context(ii);
        fm = strip_lexical_context(fm);
      }
    }
  }

  m = MALLOC_ONE_TAGGED(Scheme_Module);
  m->so.type = scheme_module_type;
  m->predefined = scheme_starting_up;
  m->phaseless = (scheme_starting_up ? scheme_true : NULL);

  /* must set before calling new_module_env: */
  rmp = SCHEME_STX_VAL(nm);
  rmp = scheme_intern_resolved_module_path(rmp);
  m->modname = rmp;
  m->modsrc = rmp;

  if (!SCHEME_NULLP(submodule_ancestry))
    submodule_path = scheme_append(submodule_path, scheme_make_pair(SCHEME_STX_VAL(nm), scheme_null));
  m->submodule_ancestry = submodule_ancestry;
  m->submodule_path = submodule_path;
  
  if (!SCHEME_NULLP(submodule_path)) {
    Scheme_Object *self_name;
    self_name = scheme_resolved_module_path_value(extract_root_module_name(m));
    self_name = scheme_intern_resolved_module_path(scheme_make_pair(self_name, submodule_path));
    m->modname = self_name;
  }

  LOG_START_EXPAND(m);

  if (SAME_OBJ(m->modname, kernel_modname)
      || SAME_OBJ(m->modname, unsafe_modname)
      || SAME_OBJ(m->modname, flfxnum_modname)
      || SAME_OBJ(m->modname, extfl_modname)
      || SAME_OBJ(m->modname, futures_modname)) {
    /* Too confusing. Give it a different name while compiling. */
    Scheme_Object *k2;
    const char *kname;
    if (SAME_OBJ(m->modname, kernel_modname))
      kname = "#%kernel";
    else if (SAME_OBJ(m->modname, flfxnum_modname))
      kname = "#%flfxnum";
    else if (SAME_OBJ(m->modname, extfl_modname))
      kname = "#%extfl";
    else if (SAME_OBJ(m->modname, futures_modname))
      kname = "#%futures";
    else
      kname = "#%unsafe";
    k2 = scheme_intern_resolved_module_path(scheme_make_symbol(kname)); /* uninterned! */
    restore_confusing_name = m->modname;
    m->modname = k2;
  }

  {
    Scheme_Module_Exports *me;
    me = scheme_make_module_exports();
    m->me = me;
    me->modsrc = m->modsrc;
  }

  top_env = env->genv;
  /* Create module env from phase-0 env. This doesn't create bad
     sharing, because compile-time module instances for compiling this
     module are all fresh instances. */
  while (top_env->phase) {
    scheme_prepare_template_env(top_env);
    top_env = top_env->template_env;
  }

  /* Create module environment. This environment gets a fresh table
     for phase-1 instances: */
  menv = scheme_new_module_env(top_env, m, 1);

  menv->disallow_unbound = 1;
  
  self_modidx = scheme_make_modidx(scheme_false, scheme_false, m->modname);
  m->self_modidx = self_modidx;
  m->me->src_modidx = self_modidx;

  m->insp = env->insp;

  if (ii) {
    m->ii_src = ii;

    ii = scheme_syntax_to_datum(ii, 0, NULL);

    if (!scheme_is_module_path(ii)) {
      scheme_wrong_syntax(NULL, m->ii_src, form, "initial import is not a well-formed module path");
    }

    iidx = scheme_make_modidx(ii, 
                              self_modidx,
                              scheme_false);
  } else {
    void **super_bxs_info;
    Scheme_Object *rn, *rnss, *rnss2, *rn2;

    iidx = scheme_make_modidx(scheme_make_pair(submod_symbol,
                                               scheme_make_pair(scheme_make_utf8_string(".."),
                                                                scheme_null)),
                              self_modidx,
                              scheme_false);

    super_phase_shift = scheme_bin_minus(scheme_make_integer(0), super_phase_shift);

    rn = scheme_stx_phase_shift_as_rename(super_phase_shift, 
                                          top_env->module->self_modidx, iidx, 
                                          menv->module_registry->exports,
                                          env->insp, NULL);

    rnss2 = scheme_null;
    for (rnss = super_bxs->rn_stx; SCHEME_PAIRP(rnss); rnss = SCHEME_CDR(rnss)) {
      rn2 = scheme_stx_to_rename(SCHEME_CAR(rnss));
      rn2 = scheme_stx_shift_rename_set(rn2,
                                        top_env->module->self_modidx, iidx, 
                                        env->insp);
      rnss2 = scheme_make_pair(scheme_rename_to_stx(rn2), rnss2);
    }
    rnss2 = scheme_reverse(rnss2);
    
    super_bxs_info = MALLOC_N(void*, 7);
    super_bxs_info[0] = super_bxs;
    super_bxs_info[1] = rn;
    super_bxs_info[2] = top_env->module->self_modidx;
    super_bxs_info[3] = iidx;
    super_bxs_info[4] = top_env;
    super_bxs_info[5] = super_phase_shift;
    super_bxs_info[6] = rnss2;
    m->super_bxs_info = super_bxs_info;
  }

  SCHEME_EXPAND_OBSERVE_PREPARE_ENV(rec[drec].observer);

  /* load the module for the initial require */
  if (iidx) {
    iim = module_load(_module_resolve(iidx, m->ii_src, NULL, 1), menv, NULL); 
    start_module(iim, find_env(menv, SCHEME_INT_VAL(super_phase_shift)), 0, iidx, 1, 0, menv->phase, scheme_null, 0);
  } else
    iim = NULL;

  m->requires = scheme_null;
  m->et_requires = scheme_null;
  m->tt_requires = scheme_null;
  m->dt_requires = scheme_null;

  if (iim && iim->phaseless && rec[drec].comp)
    m->phaseless = scheme_true;

  if (iidx) {
    Scheme_Object *ins;
    ins = cons(iidx, scheme_null);
    if (SAME_OBJ(super_phase_shift, scheme_make_integer(0))) {
      m->requires = ins;
    } else if (SAME_OBJ(super_phase_shift, scheme_make_integer(-1))) {
      m->tt_requires = ins;
    } else {
      Scheme_Hash_Table *oht;
      oht = m->other_requires;
      if (!oht) {
        oht = scheme_make_hash_table_equal();
        m->other_requires = oht;
      }
      scheme_hash_set(oht, super_phase_shift, ins);
    }
  }

  scheme_prepare_env_renames(menv, mzMOD_RENAME_NORMAL);

  rn_set = menv->rename_set;

  {
    Scheme_Object *insp;
    menv->access_insp = env->insp;
    insp = scheme_make_inspector(env->insp);
    menv->guard_insp = insp;
  }

  scheme_prepare_exp_env(menv);

  /* Allow phase-1 references to unbound identifiers; we check
     at the end of body expansion to make sure that all referenced
     identifiers were eventually bound. Meanwhile, 
     reference-before-definition errors are possible. */
  menv->exp_env->disallow_unbound = -1;

  mb_ctx = scheme_false;

  /* For each provide in iim, add a module rename to fm */
  if (ii)
    saw_mb = add_simple_require_renames(NULL, rn_set, NULL, iim, iidx, scheme_make_integer(0), NULL, 1);
  else {
    if (!skip_strip) {
      Scheme_Object *rn;
      rn = (Scheme_Object *)m->super_bxs_info[1];
      if (!SCHEME_STXP(fm))
        fm = scheme_datum_to_syntax(fm, scheme_false, scheme_false, 0, 0);
      fm = scheme_add_rename(fm, rn);
      mb_ctx = scheme_add_rename(form, rn);
    } else {
      if (!SCHEME_STXP(fm))
        fm = scheme_datum_to_syntax(fm, scheme_false, scheme_false, 0, 0);
    }
    /* there must be a `#%module-begin' in the enclosing module, right? */
    saw_mb = 1;
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

    /* If the body is `#%plain-module-begin' and if any form is a
       `module' form (i.e., already with the `module' binding, then
       attach the original form as a property to the `module' form, so
       that re-expansion can use it instead of dropping all lexical
       context: */
    fm = scheme_annotate_existing_submodules(fm, 1);
  } else {
    fm = scheme_make_pair(scheme_datum_to_syntax(module_begin_symbol, form, mb_ctx, 0, 2), 
			  fm);
    check_mb = 1;
  }

  fm = scheme_datum_to_syntax(fm, form, mb_ctx, 0, 2);

  if (check_mb) {
    SCHEME_EXPAND_OBSERVE_TAG(rec[drec].observer, fm);
  }

  fm = scheme_stx_property(fm, module_name_symbol, scheme_resolved_module_path_value(rmp));

  this_empty_self_modidx = scheme_get_submodule_empty_self_modidx(submodule_path);

  if (ii) {
    /* phase shift to replace self_modidx of previous expansion (if any): */
    fm = scheme_stx_phase_shift(fm, NULL, this_empty_self_modidx, self_modidx, NULL, m->insp, NULL);

    fm = scheme_add_rename(fm, rn_set);
  } else {
    if (skip_strip) {
      /* phase shift to replace self_modidx of previous expansion: */
      fm = scheme_stx_phase_shift(fm, NULL, this_empty_self_modidx, self_modidx, NULL, m->insp, NULL);
    }
  }

  SCHEME_EXPAND_OBSERVE_RENAME_ONE(rec[drec].observer, fm);

  if (!check_mb) {

    fm = scheme_check_immediate_macro(fm, benv, rec, drec, 0, &mbval, NULL, NULL);

    /* If expansion is not the primitive `#%module-begin', add local one: */
    if (!SAME_OBJ(mbval, modbeg_syntax)) {
      Scheme_Object *mb;
      mb = scheme_datum_to_syntax(module_begin_symbol, form, mb_ctx, 0, 0);
      fm = scheme_make_pair(mb, scheme_make_pair(fm, scheme_null));
      fm = scheme_datum_to_syntax(fm, form, form, 0, 2);
      fm = scheme_stx_property(fm, module_name_symbol, scheme_resolved_module_path_value(rmp));
      if (ii) {
        /* Since fm is a newly-created syntax object, we need to re-add renamings: */
        fm = scheme_add_rename(fm, rn_set);
      }
      
      SCHEME_EXPAND_OBSERVE_TAG(rec[drec].observer, fm);

      check_mb = 1;
    }
  }

  if (check_mb && !saw_mb) {
    scheme_wrong_syntax(NULL, NULL, form, 
			"no #%%module-begin binding in the module's language");
  }

  if (rec[drec].comp) {
    Scheme_Object *dummy, *pv;

    dummy = scheme_make_environment_dummy(env);
    m->dummy = dummy;
    
    scheme_compile_rec_done_local(rec, drec);
    fm = scheme_compile_expr(fm, benv, rec, drec);

    /* result should be a module body value: */
    if (!SAME_OBJ(fm, (Scheme_Object *)m)) {
      scheme_wrong_syntax(NULL, NULL, form, "compiled body was not built with #%%module-begin");
    }

    if (restore_confusing_name)
      m->modname = restore_confusing_name;

    m->ii_src = NULL;
    m->super_bxs_info = NULL;

    pv = scheme_stx_property(form, scheme_intern_symbol("module-language"), NULL);
    if (pv && SCHEME_TRUEP(pv)) {
      if (SCHEME_VECTORP(pv)
          && (3 == SCHEME_VEC_SIZE(pv))
          && scheme_is_module_path(SCHEME_VEC_ELS(pv)[0])
          && SCHEME_SYMBOLP(SCHEME_VEC_ELS(pv)[1]))
        m->lang_info = pv;
    }

    fm = (Scheme_Object *)m;
  } else {
    Scheme_Object *hints, *formname, *ps;

    fm = scheme_expand_expr(fm, benv, rec, drec);

    m->ii_src = NULL;
    m->super_bxs_info = NULL;

    hints = m->hints;
    m->hints = NULL;

    formname = SCHEME_STX_CAR(form);
    fm = cons(formname,
	      cons(nm,
		   cons(orig_ii, cons(fm, scheme_null))));

    fm = scheme_datum_to_syntax(fm, form, form, 0, 2);
    
    if (hints) {
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
			       this_empty_self_modidx);
    }

    /* for future expansion, shift away from self_modidx: */
    ps = scheme_stx_phase_shift_as_rename(NULL, self_modidx, this_empty_self_modidx,
                                          NULL, NULL, scheme_rename_set_identity(rn_set));
    if (m->pre_submodules) /* non-NULL => some submodules, even if it's '() */
      fm = phase_shift_skip_submodules(fm, ps, -1);
    else
      fm = scheme_add_rename(fm, ps);

    /* make self_modidx like the empty modidx */
    if (SAME_OBJ(this_empty_self_modidx, empty_self_modidx))
      ((Scheme_Modidx *)self_modidx)->resolved = empty_self_modname;
    else
      ((Scheme_Modidx *)self_modidx)->resolved = ((Scheme_Modidx *)this_empty_self_modidx)->resolved;
  }

  if (rec[drec].comp || (rec[drec].depth != -2)) {
    /* rename tables no longer needed; NULL them out */
    menv->rename_set = NULL;
    menv->post_ex_rename_set = NULL;
  }

  LOG_END_EXPAND(m);

  SCHEME_EXPAND_OBSERVE_RENAME_ONE(rec[drec].observer, fm);
  return fm;
}

static Scheme_Object *
module_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_module(form, env, rec, drec, scheme_null, scheme_null, 0,
                   NULL, scheme_make_integer(0));
}

static Scheme_Object *
module_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  return do_module(form, env, erec, drec, scheme_null, scheme_null, 0,
                   NULL, scheme_make_integer(0));
}

static Scheme_Object *
modulestar_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  scheme_wrong_syntax(NULL, NULL, form, "illegal use (not in a module top-level)");
  return NULL;
}

static Scheme_Object *
modulestar_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  return modulestar_syntax(form, env, erec, drec);
}

/* For mzc: */
Scheme_Object *scheme_apply_for_syntax_in_env(Scheme_Object *proc, Scheme_Env *env)
{
  Scheme_Comp_Env *rhs_env;
  Scheme_Dynamic_State dyn_state;

  rhs_env = scheme_new_comp_env(env, NULL, SCHEME_TOPLEVEL_FRAME);

  scheme_set_dynamic_state(&dyn_state, rhs_env, NULL, scheme_false, 
                           env, (env->link_midx 
                                 ? env->link_midx 
                                 : (env->module
                                    ? env->module->me->src_modidx
                                    : NULL)));

  return scheme_apply_multi_with_dynamic_state(proc, 0, NULL, &dyn_state);
}

/**********************************************************************/
/*                          #%module-begin                            */
/**********************************************************************/

static void check_require_name(Scheme_Object *prnt_name, Scheme_Object *name, 
                               Scheme_Object *nominal_modidx, Scheme_Object *nominal_name,
			       Scheme_Object *modidx, Scheme_Object *exname, int exet,
			       int isval, void *tables, Scheme_Object *e, Scheme_Object *form, 
                               Scheme_Object *err_src, Scheme_Object *mark_src,
                               Scheme_Object *phase, Scheme_Object *src_phase_index,
                               Scheme_Object *nominal_export_phase)
{
  Scheme_Bucket_Table *toplevel, *syntax;
  Scheme_Hash_Table *required;
  Scheme_Object *vec, *nml, *tvec;

  tvec = scheme_hash_get((Scheme_Hash_Table *)tables, phase);
  if (!tvec) {
    required = get_required_from_tables(tables, phase);
    toplevel = NULL;
    syntax = NULL;
  } else {
    toplevel = (Scheme_Bucket_Table *)(SCHEME_VEC_ELS(tvec)[0]);
    required = (Scheme_Hash_Table *)(SCHEME_VEC_ELS(tvec)[1]);
    syntax = (Scheme_Bucket_Table *)(SCHEME_VEC_ELS(tvec)[2]);
  }

  /* Check that it's not yet defined: */
  if (toplevel) {
    if (scheme_lookup_in_table(toplevel, (const char *)name)) {
      scheme_wrong_syntax("module", prnt_name, form, "imported identifier already defined");
    }
  }

  if (!SAME_OBJ(src_phase_index, scheme_make_integer(0))
      || !SAME_OBJ(nominal_export_phase, scheme_make_integer(0))
      || !SAME_OBJ(nominal_name, prnt_name)) {
    nominal_modidx = scheme_make_pair(nominal_modidx,
                                      scheme_make_pair(src_phase_index,
                                                       scheme_make_pair(nominal_name,
                                                                        scheme_make_pair(nominal_export_phase,
                                                                                         scheme_null))));
  }
	    
  /* Check not required, or required from same module: */
  vec = scheme_hash_get(required, name);
  if (vec) {
    Scheme_Object *srcs;
    char *fromsrc = NULL, *fromsrc_colon = "", *phase_expl;
    intptr_t fromsrclen = 0;
    
    if (same_resolved_modidx(SCHEME_VEC_ELS(vec)[1], modidx)
	&& SAME_OBJ(SCHEME_VEC_ELS(vec)[2], exname)) {
      /* already required, same source; add redundant nominal (for re-provides),
         and also add source phase for re-provides. */
      nml = scheme_make_pair(nominal_modidx, SCHEME_VEC_ELS(vec)[0]);
      SCHEME_VEC_ELS(vec)[0] = nml;
      SCHEME_VEC_ELS(vec)[7] = scheme_false;
      return; 
    }

    if (SCHEME_TRUEP(SCHEME_VEC_ELS(vec)[7])) {
      /* can override */
    } else {
      /* error: already imported */
      srcs = scheme_null;
      if (SCHEME_TRUEP(SCHEME_VEC_ELS(vec)[5])) {
        srcs = scheme_make_pair(SCHEME_VEC_ELS(vec)[5], srcs);
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
        srcs = scheme_make_pair(err_src, srcs);

      if (SCHEME_FALSEP(phase))
        phase_expl = " for label";
      else if (!SCHEME_INT_VAL(phase))
        phase_expl = "";
      else if (SCHEME_INT_VAL(phase) == 1)
        phase_expl = " for syntax";
      else {
        char buf[32];
        sprintf(buf, " for phase %" PRIdPTR, SCHEME_INT_VAL(phase));
        phase_expl = scheme_strdup(buf);
      }

      scheme_wrong_syntax_with_more_sources("module", prnt_name, err_src, srcs,
                                            "identifier already imported%s from%s %t",
                                            phase_expl,
                                            fromsrc_colon, fromsrc, fromsrclen);
    }
  }
	    
  /* Check not syntax: */
  if (syntax) {
    if (scheme_lookup_in_table(syntax, (const char *)name)) {
      scheme_wrong_syntax("module", prnt_name, form, "imported identifier already defined");
    }
  }

  /* Remember require: */
  vec = scheme_make_vector(9, NULL);
  nml = scheme_make_pair(nominal_modidx, scheme_null);
  SCHEME_VEC_ELS(vec)[0] = nml;
  SCHEME_VEC_ELS(vec)[1] = modidx;
  SCHEME_VEC_ELS(vec)[2] = exname;
  SCHEME_VEC_ELS(vec)[3] = (isval ? scheme_true : scheme_false);
  SCHEME_VEC_ELS(vec)[4] = prnt_name;
  SCHEME_VEC_ELS(vec)[5] = (err_src ? err_src : scheme_false);
  SCHEME_VEC_ELS(vec)[6] = (mark_src ? mark_src : scheme_false);
  SCHEME_VEC_ELS(vec)[7] = scheme_false;
  SCHEME_VEC_ELS(vec)[8] = scheme_make_integer(exet);
  scheme_hash_set(required, name, vec);
}

static int check_already_required(Scheme_Hash_Table *required, Scheme_Object *name)
{
  Scheme_Object *vec;

  vec = scheme_hash_get(required, name);
  if (vec) {
    if (SCHEME_TRUEP(SCHEME_VEC_ELS(vec)[7])) {
      scheme_hash_set(required, name, NULL);
      return 0;
    }
    return 1;
  }

  return 0;
}

static void propagate_imports(Module_Begin_Expand_State *bxs,
                              Module_Begin_Expand_State *super_bxs,
                              Scheme_Object *rn,
                              Scheme_Object *from_idx,
                              Scheme_Object *to_idx,
                              Scheme_Env *super_genv,
                              Scheme_Env *genv,
                              Scheme_Object *phase_shift)
{
  Scheme_Hash_Table *ht, *required, *super_required;
  Scheme_Object *phase, *super_name, *name, *super_vec, *vec;
  Scheme_Object *l, *v, *super_defs, *key, *val;
  int i, j;
  Scheme_Env *super_def_genv;

  ht = super_bxs->tables;
  for (i = ht->size; i--; ) {
    if (ht->vals[i]) {
      phase = ht->keys[i];
      super_required = (Scheme_Hash_Table *)SCHEME_VEC_ELS(ht->vals[i])[1];

      if (SCHEME_TRUEP(phase))
        phase = scheme_bin_plus(phase, phase_shift);

      required = (Scheme_Hash_Table *)get_required_from_tables(bxs->tables, phase);

      for (j = super_required->size; j--; ) {
        if (super_required->vals[j]) {
          super_name = super_required->keys[j];
          super_vec = super_required->vals[j];

          name = super_name;

          vec = scheme_make_vector(9, NULL);

          l = SCHEME_VEC_ELS(super_vec)[0];
          v = scheme_null;
          while (!SCHEME_NULLP(l)) {
            v = scheme_make_pair(scheme_modidx_shift(SCHEME_CAR(l), from_idx, to_idx),
                                 v);
            l = SCHEME_CDR(l);
          }
          v = scheme_reverse(v);
          SCHEME_VEC_ELS(vec)[0] = v;
          
          v = scheme_modidx_shift(SCHEME_VEC_ELS(super_vec)[1], from_idx, to_idx);
          SCHEME_VEC_ELS(vec)[1] = v;
          
          SCHEME_VEC_ELS(vec)[2] = SCHEME_VEC_ELS(super_vec)[2];
          SCHEME_VEC_ELS(vec)[3] = SCHEME_VEC_ELS(super_vec)[3];
          SCHEME_VEC_ELS(vec)[4] = SCHEME_VEC_ELS(super_vec)[4];
          SCHEME_VEC_ELS(vec)[5] = SCHEME_VEC_ELS(super_vec)[5];

          v = SCHEME_VEC_ELS(super_vec)[6];
          if (SCHEME_TRUEP(v))
            v = scheme_add_rename(v, rn);
          SCHEME_VEC_ELS(vec)[6] = v;

          SCHEME_VEC_ELS(vec)[7] = scheme_true; /* can be shadowed */
          SCHEME_VEC_ELS(vec)[8] = SCHEME_VEC_ELS(super_vec)[8];

          scheme_hash_set(required, name, vec);
        }
      }
    }
  }

  i = -1;
  while (1) {
    i = scheme_hash_tree_next(super_bxs->all_defs, i); 
    if (i == -1) break;
    if (scheme_hash_tree_index(super_bxs->all_defs, i, &key, &val)) {
      phase = key;
      super_defs = val;

      super_def_genv = find_env(super_genv, SCHEME_INT_VAL(phase));
      
      required = (Scheme_Hash_Table *)get_required_from_tables(bxs->tables, 
                                                               scheme_bin_plus(phase, phase_shift));

      while (!SCHEME_NULLP(super_defs)) {
        name = SCHEME_CAR(super_defs);
        super_defs = SCHEME_CDR(super_defs);
       
        vec = scheme_make_vector(9, NULL);

        v = scheme_make_pair(to_idx, scheme_null);
        SCHEME_VEC_ELS(vec)[0] = v;
        SCHEME_VEC_ELS(vec)[1] = to_idx;
        v = scheme_tl_id_sym(super_def_genv, name, NULL, 2, NULL, NULL);
        SCHEME_VEC_ELS(vec)[2] = v;
        if (scheme_lookup_in_table(super_def_genv->toplevel, (char *)name))
          SCHEME_VEC_ELS(vec)[3] = scheme_true;
        else
          SCHEME_VEC_ELS(vec)[3] = scheme_false;
        SCHEME_VEC_ELS(vec)[4] = SCHEME_STX_VAL(name);
        SCHEME_VEC_ELS(vec)[5] = name;
        name = scheme_add_rename(name, rn);
        SCHEME_VEC_ELS(vec)[6] = name;
        SCHEME_VEC_ELS(vec)[7] = scheme_true; /* can be shadowed */
        SCHEME_VEC_ELS(vec)[8] = phase;
        
        name = SCHEME_STX_VAL(name); /* is this right? */
        scheme_hash_set(required, name, vec);
      }
    }
  }
}

Scheme_Object *reverse_and_add_rename(Scheme_Object *fm, Scheme_Object *post_ex_rn)
{
  Scheme_Object *l2 = scheme_null;

  while (!SCHEME_NULLP(fm)) {
    l2 = scheme_make_pair(scheme_add_rename(SCHEME_CAR(fm), post_ex_rn),
                          l2);
    fm = SCHEME_CDR(fm);
  }
  return l2;
}

static Scheme_Object *stx_sym(Scheme_Object *name, Scheme_Object *_genv)
{
  return scheme_tl_id_sym((Scheme_Env *)_genv, name, NULL, 2, NULL, NULL);
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

static Scheme_Object *add_lifted_defn(Scheme_Object *data, Scheme_Object **_ids, Scheme_Object *expr, Scheme_Comp_Env *_env)
{
  Scheme_Comp_Env *env;
  Scheme_Object *self_modidx, *rn, *name, *ids, *id, *new_ids = scheme_null;

  env = (Scheme_Comp_Env *)SCHEME_VEC_ELS(data)[0];
  self_modidx = SCHEME_VEC_ELS(data)[1];
  rn = SCHEME_VEC_ELS(data)[2];

  for (ids = *_ids; !SCHEME_NULLP(ids); ids = SCHEME_CDR(ids)) {
    id = SCHEME_CAR(ids);
  
    name = scheme_tl_id_sym(env->genv, id, scheme_false, 2, NULL, NULL);

    /* Create the bucket, indicating that the name will be defined: */
    scheme_add_global_symbol(name, scheme_undefined, env->genv);
  
    /* Add a renaming: */
    scheme_extend_module_rename(rn, self_modidx, name, name, self_modidx, name, 0, NULL, NULL, 0);

    id = scheme_add_rename(id, rn);
    new_ids = cons(id, new_ids);
  }

  new_ids = scheme_reverse(new_ids);
  *_ids = new_ids;

  return scheme_make_lifted_defn(scheme_sys_wraps(env), _ids, expr, _env);
}

static Scheme_Object *make_require_form(Scheme_Object *module_path, intptr_t phase, Scheme_Object *mark)
{
  Scheme_Object *e = module_path;

  if (phase != 0) {
    e = scheme_make_pair(for_meta_symbol,
                         scheme_make_pair(scheme_make_integer(phase),
                                          scheme_make_pair(e,
                                                           scheme_null)));
  }
  e = scheme_make_pair(require_stx, scheme_make_pair(e, scheme_null));
  e = scheme_datum_to_syntax(e, scheme_false, scheme_false, 0, 0);

  e = scheme_add_remove_mark(e, mark);

  return e;
}

Scheme_Object *scheme_parse_lifted_require(Scheme_Object *module_path,
                                           intptr_t phase,
                                           Scheme_Object *mark,
                                           void *data)
{
  Scheme_Object *e;
  Scheme_Object *base_modidx = (Scheme_Object *)((void **)data)[1];
  Scheme_Env *env = (Scheme_Env *)((void **)data)[2];
  Scheme_Module *for_m = (Scheme_Module *)((void **)data)[3];
  Scheme_Object *rns = (Scheme_Object *)((void **)data)[4];
  Scheme_Object *post_ex_rns = (Scheme_Object *)((void **)data)[5];
  void *tables = ((void **)data)[6];
  Scheme_Object *redef_modname = (Scheme_Object *)((void **)data)[7];
  int *all_simple = (int *)((void **)data)[8];
  Scheme_Hash_Table *submodule_names = (Scheme_Hash_Table *)((void **)data)[9];

  e = make_require_form(module_path, phase, mark);

  parse_requires(e, env->phase, base_modidx, env, for_m,
                 rns, post_ex_rns,
                 check_require_name, tables,
                 redef_modname, 
                 0, 0, 1, 
                 1, 0,
                 all_simple, 
                 NULL,
                 submodule_names,
                 NULL);

  return e;
}

static Scheme_Object *package_require_data(Scheme_Object *base_modidx,
                                           Scheme_Env *env,
                                           Scheme_Module *for_m,
                                           Scheme_Object *rns, Scheme_Object *post_ex_rns,
                                           void *data,
                                           Scheme_Object *redef_modname,
                                           int *all_simple,
                                           Scheme_Hash_Table *submodule_names)
{
  void **vals;

  vals = MALLOC_N(void*, 10);
  vals[0] = NULL; /* this slot is available */
  vals[1] = base_modidx;
  vals[2] = env;
  vals[3] = for_m;
  vals[4] = rns;
  vals[5] = post_ex_rns;
  vals[6] = data;
  vals[7] = redef_modname;
  vals[8] = all_simple;
  vals[9] = submodule_names;

  return scheme_make_raw_pair((Scheme_Object *)vals, NULL);
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

static Scheme_Object *do_module_begin(Scheme_Object *orig_form, Scheme_Comp_Env *env, 
				      Scheme_Compile_Expand_Info *rec, int drec)
{
  int num_phases, *_num_phases, i, exicount, *all_simple_renames, can_clean, has_submodules;
  Scheme_Hash_Tree *all_defs;
  Scheme_Hash_Table *tables, *all_defs_out, *all_provided, *all_reprovided, *modidx_cache;
  Scheme_Module_Export_Info **exp_infos, *exp_info;
  Scheme_Module_Phase_Exports *pt;
  Scheme_Object *post_ex_rn_set; /* phase -> post_ex_rn-like rename */
  Scheme_Object *form, *redef_modname, *rn_set, *observer, **exis, *body_lists, *expanded_l, *rn_stx;
  Scheme_Env *genv;
  Module_Begin_Expand_State *bxs;
  Scheme_Expand_Info crec;

  form = scheme_stx_taint_disarm(orig_form, NULL);

  if (!(env->flags & SCHEME_MODULE_FRAME))
    scheme_wrong_syntax(NULL, NULL, form, "illegal use (not a module body)");

  if (scheme_stx_proper_list_length(form) < 0)
    scheme_wrong_syntax(NULL, NULL, form, IMPROPER_LIST_FORM);

  if (!env->genv->module)
    scheme_wrong_syntax(NULL, NULL, form, "not currently transforming a module");

  /* Redefining a module? */
  redef_modname = env->genv->module->modname;
  if (!scheme_hash_get(env->genv->module_registry->loaded, redef_modname))
    redef_modname = NULL;

  tables = scheme_make_hash_table_equal();

  modidx_cache = scheme_make_hash_table_equal();

  all_provided = scheme_make_hash_table_equal();
  all_reprovided = scheme_make_hash_table_equal();
  all_defs = scheme_make_hash_tree(1);
  all_defs_out = scheme_make_hash_table_equal();

  rn_set = env->genv->rename_set;
  post_ex_rn_set = scheme_make_module_rename_set(mzMOD_RENAME_MARKED, rn_set, env->genv->access_insp);

  /* It's possible that #%module-begin expansion introduces
     marked identifiers for definitions. */
  form = scheme_add_rename(form, post_ex_rn_set);

  observer = rec[drec].observer;
  SCHEME_EXPAND_OBSERVE_RENAME_ONE(observer, form);

  _num_phases = MALLOC_ONE_ATOMIC(int);
  *_num_phases = 0;

  all_simple_renames = (int *)scheme_malloc_atomic(sizeof(int));
  *all_simple_renames = 1;

  if (env->genv->module->super_bxs_info) {
    rn_stx = scheme_rename_to_stx(post_ex_rn_set);
    *all_simple_renames = 0;
  } else
    rn_stx = scheme_rename_to_stx(rn_set);
  if (env->genv->module->super_bxs_info && env->genv->module->super_bxs_info[6])
    rn_stx = scheme_make_pair(rn_stx, env->genv->module->super_bxs_info[6]);
  {
    Scheme_Object *v;
    if (SCHEME_PAIRP(rn_stx))
      v = scheme_list_to_vector(rn_stx); 
   else
      v = rn_stx;
    env->genv->module->rn_stx = v;
  }

  bxs = scheme_malloc(sizeof(Module_Begin_Expand_State));
  bxs->post_ex_rn_set = post_ex_rn_set;
  bxs->tables = tables;
  bxs->all_provided = all_provided;
  bxs->all_reprovided = all_reprovided;
  bxs->all_defs = all_defs;
  bxs->all_defs_out = all_defs_out;
  bxs->all_simple_renames = all_simple_renames;
  bxs->_num_phases = _num_phases;
  bxs->saved_provides = scheme_null;
  bxs->saved_submodules = scheme_null;
  bxs->submodule_names = NULL;
  bxs->modidx_cache = modidx_cache;
  bxs->redef_modname = redef_modname;
  bxs->end_statementss = scheme_null;

  if (env->genv->module->super_bxs_info) {
    /* initialize imports that are available for export from the enclosing module's
       `all_defs' and `imports' (within `tables'): */
    void **super_bxs_info = env->genv->module->super_bxs_info;
    propagate_imports(bxs,
                      (Module_Begin_Expand_State *)super_bxs_info[0],
                      (Scheme_Object *)super_bxs_info[1],
                      (Scheme_Object *)super_bxs_info[2],
                      (Scheme_Object *)super_bxs_info[3],
                      (Scheme_Env *)super_bxs_info[4],
                      env->genv,
                      (Scheme_Object *)super_bxs_info[5]);
  }

  if (!rec[drec].comp) {
    /* In expand mode, we need to compile anyway in case of nested modules. */
    crec.comp = 1;
    crec.dont_mark_local_use = 0;
    crec.resolve_module_ids = 0;
    crec.value_name = scheme_false;
    crec.observer = NULL;
    crec.pre_unwrapped = 0;
    crec.env_already = 0;
    crec.comp_flags = rec[drec].comp_flags;

    if (!env->prefix) {
      Comp_Prefix *cp;
      cp = MALLOC_ONE_RT(Comp_Prefix);
#ifdef MZTAG_REQUIRED
      cp->type = scheme_rt_comp_prefix;
#endif
      env->prefix = cp;
    }
  }

  body_lists = do_module_begin_at_phase(form, env, 
                                        rec[drec].comp ? rec : &crec, 
                                        rec[drec].comp ? drec : 0,
                                        rec[drec].comp ? NULL : rec, drec, 
                                        0, 
                                        scheme_null,
                                        bxs);
  num_phases = *_num_phases;

  if (!rec[drec].comp) {
    expanded_l = SCHEME_CAR(body_lists);
    body_lists = SCHEME_CDR(body_lists);
  } else
    expanded_l = body_lists;

  /* Compute provides for re-provides and all-defs-out: */
  (void)compute_reprovides(all_provided,
                           all_reprovided, 
                           env->genv->module, 
                           tables,
                           env->genv, 
                           num_phases,
                           bxs->all_defs, all_defs_out, 
                           "require", NULL, NULL);

  exp_infos = MALLOC_N(Scheme_Module_Export_Info*, num_phases);
  for (i = 0; i < num_phases; i++) {
    exp_info = MALLOC_ONE_RT(Scheme_Module_Export_Info);
    SET_REQUIRED_TAG(exp_info->type = scheme_rt_export_info);
    exp_infos[i] = exp_info;
  }

  /* Compute provide arrays */
  compute_provide_arrays(all_provided, tables,
                         env->genv->module->me,
                         env->genv,
                         form, 
                         num_phases, exp_infos);
  
  /* Compute indirect provides (which is everything at the top-level): */
  genv = env->genv;
  for (i = 0; i < num_phases; i++) {
    switch (i) {
    case 0:
      pt = env->genv->module->me->rt;
      break;
    case 1:
      pt = env->genv->module->me->et;
      break;
    default:
      if (env->genv->module->me->other_phases)
        pt = (Scheme_Module_Phase_Exports *)scheme_hash_get(env->genv->module->me->other_phases,
                                                            scheme_make_integer(i));
      else 
        pt = NULL;
      break;
    }
    if (pt) {
      exis = compute_indirects(genv, pt, &exicount, 1);
      exp_infos[i]->indirect_provides = exis;
      exp_infos[i]->num_indirect_provides = exicount;
      exis = compute_indirects(genv, pt, &exicount, 0);
      exp_infos[i]->indirect_syntax_provides = exis;
      exp_infos[i]->num_indirect_syntax_provides = exicount;
    }
    genv = genv->exp_env;
  }

  if (rec[drec].comp || (rec[drec].depth != -2))
    can_clean = 1;
  else
    can_clean = 0;

  has_submodules = (!SCHEME_NULLP(bxs->saved_submodules)
                    || (env->genv->module->submodule_path
                        && !SCHEME_NULLP(env->genv->module->submodule_path)));

  if (can_clean && !has_submodules)
    scheme_clean_dead_env(env->genv);

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

    /* kernel re-export info (always #f): */
    result = scheme_make_pair(scheme_false, result);

    /* Indirect provides */ 
    a = scheme_null;
    for (j = 0; j < exp_infos[0]->num_indirect_provides; j++) {
      a = scheme_make_pair(exp_infos[0]->indirect_provides[j], a);
    }
    result = scheme_make_pair(a, result);
    
    /* add syntax and value exports: */
    for (j = 0; j < 2; j++) {
      int top, i;

      e = scheme_null;

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
	  a = scheme_make_pair(exs[i], exsns[i]);
	  if (!SCHEME_FALSEP(exss[i])) {
	    a = scheme_make_pair(exss[i], a);
	  }
	}
	e = scheme_make_pair(a, e);
      }
      result = scheme_make_pair(e, result);
    }

    env->genv->module->hints = result;
  }

  if (rec[drec].comp || has_submodules) {
    Scheme_Object *a, **bodies;

    bodies = MALLOC_N(Scheme_Object*, num_phases);
    for (i = 0; i < num_phases; i++) {
      a = SCHEME_CAR(body_lists);
      if (i > 0) a = scheme_reverse(a);
      a = scheme_list_to_vector(a);
      bodies[i] = a;
      body_lists = SCHEME_CDR(body_lists);
    }
    env->genv->module->bodies = bodies;
    env->genv->module->num_phases = num_phases;

    env->genv->module->exp_infos = exp_infos;

    if (!*all_simple_renames) {
      /* No need to keep indirect syntax provides */
      for (i = 0; i < num_phases; i++) {
        exp_infos[i]->indirect_syntax_provides = NULL;
        exp_infos[i]->num_indirect_syntax_provides = 0;
      }
    }

    if (*all_simple_renames) {
      env->genv->module->rn_stx = scheme_true;
    }
  }

  if (rec[drec].comp || has_submodules) {
    Scheme_Object *dummy;
    dummy = scheme_make_environment_dummy(env);
    env->genv->module->dummy = dummy;
  }

  SCHEME_EXPAND_OBSERVE_NEXT(observer);

  /* Submodules */
  if (has_submodules) {
    Scheme_Object *expanded_modules, *root_module_name;

    root_module_name = extract_root_module_name(env->genv->module);

    /* Need to declare the just-finished module, so it can be
       referenced by nested modules: */
    {
      Optimize_Info *oi;
      Resolve_Prefix *rp;
      Resolve_Info *ri;
      Scheme_Object *o;
      int max_let_depth;
      int use_jit;

      /* Since we optimize & resolve the module here, it won't need to
         be optimized and resolved later. The resolve pass
         sets m->comp_prefix to NULL, which is how optimize & resolve
         know to avoid re-optimizing and re-resolving. */

      /* Note: don't use MZCONFIG_USE_JIT for module bodies */
      use_jit = scheme_startup_use_jit;

      oi = scheme_optimize_info_create(env->prefix, 1);
      scheme_optimize_info_enforce_const(oi, rec[drec].comp_flags & COMP_ENFORCE_CONSTS);
      if (!(rec[drec].comp_flags & COMP_CAN_INLINE))
        scheme_optimize_info_never_inline(oi);
      o = scheme_optimize_expr((Scheme_Object *)env->genv->module, oi, 0);

      rp = scheme_resolve_prefix(0, env->prefix, 1);
      ri = scheme_resolve_info_create(rp);
      scheme_resolve_info_enforce_const(ri, rec[drec].comp_flags & COMP_ENFORCE_CONSTS);

      o = scheme_resolve_expr(o, ri);
      max_let_depth = scheme_resolve_info_max_let_depth(ri);
      o = scheme_sfs(o, NULL, max_let_depth);

      if (use_jit)
        o = scheme_jit_expr(o);
      else
        o = scheme_eval_clone(o);
      
      (void)do_module_execute(o, env->genv, 0, 1, root_module_name, NULL);
    }

    if (!SCHEME_PAIRP(rn_stx))
      rn_stx = scheme_make_pair(rn_stx, scheme_null);
    bxs->rn_stx = rn_stx;

    if (!rec[drec].comp && (is_modulestar_stop(env))) {
      Scheme_Object *l = bxs->saved_submodules;
      expanded_modules =  NULL;
      while (!SCHEME_NULLP(l)) {
        expanded_modules = scheme_make_pair(SCHEME_CAR(SCHEME_CAR(l)),
                                            expanded_modules);
        l = SCHEME_CDR(l);
      }
      bxs->saved_submodules = scheme_null;
    } else
      expanded_modules = expand_submodules(rec, drec, env, bxs->saved_submodules, 1, bxs, !rec[drec].comp);

    if (!rec[drec].comp) {
      (void)fixup_expanded(expanded_l, expanded_modules, 0, MODULE_MODFORM_KIND);
    }

    if (can_clean)
      scheme_clean_dead_env(env->genv);
  }

  /* Return module or expanded code: */
  if (rec[drec].comp) {
    return (Scheme_Object *)env->genv->module;
  } else {
    Scheme_Object *p;

    if (rec[drec].depth == -2) {
      /* This was a local expand. Flush definitions, because the body expand may start over. */
      Scheme_Env *f_genv = env->genv;
      while (f_genv) {
        flush_definitions(f_genv);
        f_genv = f_genv->exp_env;
      }
    }

    p = SCHEME_STX_CAR(form);

    return scheme_datum_to_syntax(cons(p, expanded_l), orig_form, orig_form, 0, 2);
  }
}

static Scheme_Object *get_higher_phase_lifts(Module_Begin_Expand_State *bxs,
                                             Scheme_Object *begin_for_syntax_stx)
{
  Scheme_Object *p, *e, *fm = scheme_null;

  if (SCHEME_PAIRP(bxs->end_statementss)) {
    /* No other ends, so start shitfing higher-phase ends into `b-f-s': */
    int depth = 1;
    for (p = bxs->end_statementss; SCHEME_PAIRP(p); p = SCHEME_CDR(p), depth++) {
      if (SCHEME_PAIRP(SCHEME_CAR(p)))
        break;
    }
    if (SCHEME_PAIRP(p)) {
      /* wrap `depth' `begin-for-syntaxes' around SCHEME_CAR(p): */
      int di;
      e = scheme_reverse(SCHEME_CAR(p));
      e = scheme_make_pair(begin_for_syntax_stx, e);
      for (di = 1; di < depth; di++) {
        e = scheme_make_pair(begin_for_syntax_stx, scheme_make_pair(e, scheme_null));
      }
      fm = scheme_make_pair(scheme_datum_to_syntax(e, scheme_false, scheme_false, 0, 0),
                            scheme_null);
      /* first `depth' end-statement lists are now empty: */
      p = SCHEME_CDR(p);
      for (di = 0; di < depth; di++) {
        p = scheme_make_pair(scheme_null, p);
      }
      bxs->end_statementss = p;
    } else
      bxs->end_statementss = scheme_null;
  }

  return fm;
}

static Scheme_Object *do_module_begin_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  void **args = p->ku.k.p1;
  Scheme_Object *form = (Scheme_Object *)args[0];
  Scheme_Comp_Env *env = (Scheme_Comp_Env *)args[1];
  Scheme_Compile_Expand_Info *rec = (Scheme_Compile_Expand_Info *)args[2];
  Scheme_Compile_Expand_Info *erec = (Scheme_Compile_Expand_Info *)args[3];
  int phase = SCHEME_INT_VAL((Scheme_Object *)args[4]);
  Scheme_Object *body_lists = (Scheme_Object *)args[5];
  Module_Begin_Expand_State *bxs = (Module_Begin_Expand_State *)args[6];

  p->ku.k.p1 = NULL;
  
  return do_module_begin_at_phase(form, env, rec, 0, erec, 0,
                                  phase, body_lists, bxs);
}


static Scheme_Object *do_module_begin_at_phase(Scheme_Object *form, Scheme_Comp_Env *env, 
                                               Scheme_Compile_Expand_Info *rec, int drec,
                                               Scheme_Compile_Expand_Info *erec, int derec,
                                               int phase, 
                                               Scheme_Object *body_lists, /* starts from phase + 1; null in expand mode */
                                               Module_Begin_Expand_State *bxs)
/* Result in expand mode is expressions in order.
   Result in compile mode is a body_lists starting with `phase',
   where a body_lists has each phase in order, with each list after the first in reverse order.
   If both rec[drec].comp && erec, cons results.
   If !rec[drec].comp, then erec is non-NULL. */
{
  Scheme_Object *fm, *first, *last, *p, *rn_set, *rn, *exp_body, *self_modidx, *prev_p;
  Scheme_Object *expanded_l;
  Scheme_Comp_Env *xenv, *cenv, *rhs_env;
  Scheme_Hash_Table *required;    /* name -> (vector nominal-modidx-list modidx srcname var? prntname)
                                     first nominal-modidx goes with modidx, rest are for re-provides */
  Scheme_Hash_Table *provided;    /* exname -> (cons locname-stx-or-sym protected?) */
  Scheme_Object *all_rt_defs;        /* list of stxid; this is almost redundant to the syntax and toplevel
                                        tables, but it preserves the original name for exporting */
  Scheme_Hash_Tree *adt;
  Scheme_Object *post_ex_rn;     /* renames for ids introduced by expansion */
  Scheme_Object *lift_data;
  Scheme_Object *lift_ctx;
  Scheme_Object *lifted_reqs = scheme_null, *req_data, *unbounds = scheme_null;
  int maybe_has_lifts = 0, expand_ends = (phase == 0), non_phaseless, requested_phaseless;
  Scheme_Object *observer, *vec, *end_statements;
  Scheme_Object *begin_for_syntax_stx, *non_phaseless_form = NULL;
  const char *who = "module";

#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Thread *pt = scheme_current_thread;
    Scheme_Compile_Expand_Info *recx, *erecx;
    void **args;

    if (rec) {
      recx = MALLOC_ONE_RT(Scheme_Compile_Expand_Info);
      memcpy(recx, rec + drec, sizeof(Scheme_Compile_Expand_Info));
#ifdef MZTAG_REQUIRED
      recx->type = scheme_rt_compile_info;
#endif
    } else
      recx = NULL;
    
    if (erec) {
      erecx = MALLOC_ONE_RT(Scheme_Compile_Expand_Info);
      memcpy(erecx, erec + derec, sizeof(Scheme_Compile_Expand_Info));
#ifdef MZTAG_REQUIRED
      erecx->type = scheme_rt_compile_info;
#endif
    } else
      erecx = NULL;

    args = MALLOC_N(void*, 7);

    args[0] = form;
    args[1] = env;
    args[2] = recx;
    args[3] = erecx;
    args[4] = scheme_make_integer(phase);
    args[5] = body_lists;
    args[6] = bxs;

    pt->ku.k.p1 = (void *)args;
    
    fm = scheme_handle_stack_overflow(do_module_begin_k);

    if (recx)
      memcpy(rec + drec, recx, sizeof(Scheme_Compile_Expand_Info));
    if (erecx)
      memcpy(erec + derec, erecx, sizeof(Scheme_Compile_Expand_Info));

    return fm;
  }
#endif
  
  if (*bxs->_num_phases < phase + 1)
    *bxs->_num_phases = phase + 1;

  non_phaseless = (env->genv->module->phaseless ? 0 : NON_PHASELESS_IMPORT);
  requested_phaseless = 0;
  env->genv->module->phaseless = NULL;

  /* Expand each expression in form up to `begin', `define-values', `define-syntax', 
     `require', `provide', `#%app', etc. */
  xenv = scheme_new_compilation_frame(0, (SCHEME_CAPTURE_WITHOUT_RENAME 
					  | SCHEME_MODULE_BEGIN_FRAME
					  | SCHEME_FOR_STOPS), 
				      env);

  install_stops(xenv, phase, &begin_for_syntax_stx);

  first = scheme_null;
  last = NULL;

  rn_set = env->genv->rename_set;
  rn = scheme_get_module_rename_from_set(rn_set, scheme_make_integer(phase), 1);

  vec = get_table(bxs->tables, scheme_make_integer(phase));
  if (!SCHEME_VEC_ELS(vec)[0])
    SCHEME_VEC_ELS(vec)[0] = (Scheme_Object *)env->genv->toplevel;
  if (!SCHEME_VEC_ELS(vec)[2])
    SCHEME_VEC_ELS(vec)[2] = (Scheme_Object *)env->genv->syntax;
  required = (Scheme_Hash_Table *)SCHEME_VEC_ELS(vec)[1];

  if (phase == 0) {
    /* Put initial requires into the table:
       (This is redundant for the rename set, but we need to fill
       the `all_requires' table, etc.) */
    if (env->genv->module->ii_src) {
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

      add_simple_require_renames(orig_src, rn_set, bxs->tables, 
                                 iim, nmidx,
                                 scheme_make_integer(0),
                                 NULL, 1);

      scheme_hash_set(bxs->modidx_cache, ((Scheme_Modidx *)nmidx)->path, nmidx);
    }
  }

  provided = (Scheme_Hash_Table *)scheme_hash_get(bxs->all_provided, scheme_make_integer(phase));
  if (!provided) {
    provided = scheme_make_hash_table(SCHEME_hash_ptr);
    scheme_hash_set(bxs->all_provided, scheme_make_integer(phase), (Scheme_Object *)provided);
  }

  all_rt_defs = scheme_hash_tree_get(bxs->all_defs, scheme_make_integer(phase));
  if (!all_rt_defs) all_rt_defs = scheme_null;

  if (SCHEME_NULLP(body_lists))
    exp_body = scheme_null;
  else {
    exp_body = SCHEME_CAR(body_lists);
    body_lists = SCHEME_CDR(body_lists);
  }

  self_modidx = env->genv->module->self_modidx;

  post_ex_rn = scheme_get_module_rename_from_set(bxs->post_ex_rn_set, scheme_make_integer(phase), 1);
  env->genv->post_ex_rename_set = bxs->post_ex_rn_set;

  /* For syntax-local-context, etc., in a d-s RHS: */
  rhs_env = scheme_new_comp_env(env->genv, env->insp, SCHEME_TOPLEVEL_FRAME);

  if (erec) {
    observer = erec[derec].observer;
  } else {
    observer = NULL;
  }

  maybe_has_lifts = 0;
  lift_ctx = scheme_generate_lifts_key();

  req_data = package_require_data(self_modidx, env->genv, env->genv->module,
                                  rn_set, bxs->post_ex_rn_set,
                                  bxs->tables,
                                  bxs->redef_modname, 
                                  bxs->all_simple_renames,
                                  bxs->submodule_names);

  if (SCHEME_PAIRP(bxs->end_statementss)) {
    end_statements = SCHEME_CAR(bxs->end_statementss);
    bxs->end_statementss = SCHEME_CDR(bxs->end_statementss);
  } else
    end_statements = scheme_null;

  /* Pass 1 */

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
           : end_statements);
      prev_p = (maybe_has_lifts 
                ? scheme_frame_get_provide_lifts(xenv) 
                : scheme_null);
      scheme_frame_captures_lifts(xenv, scheme_make_lifted_defn, scheme_sys_wraps(xenv), 
                                  p, lift_ctx, req_data, prev_p);
      maybe_has_lifts = 1;

      {
	Scheme_Expand_Info erec1;
	erec1.comp = 0;
	erec1.depth = -1;
	erec1.value_name = scheme_false;
        erec1.observer = observer;
        erec1.pre_unwrapped = 0;
        erec1.env_already = 0;
        erec1.comp_flags = rec[drec].comp_flags;
	e = scheme_expand_expr(e, xenv, &erec1, 0);	
      }

      lifted_reqs = scheme_append(scheme_frame_get_require_lifts(xenv), lifted_reqs);

      fst = scheme_frame_get_lifts(xenv);
      if (!SCHEME_NULLP(fst)) {
	/* Expansion lifted expressions, so add them to
	   the front and try again. */
        *bxs->all_simple_renames = 0;
	fm = SCHEME_STX_CDR(fm);
        e = scheme_add_rename(e, bxs->post_ex_rn_set);
        fm = scheme_named_map_1(NULL, add_a_rename, fm, bxs->post_ex_rn_set);
        fm = scheme_make_pair(e, fm);
        SCHEME_EXPAND_OBSERVE_RENAME_LIST(observer, fm);
	fm = scheme_append(fst, fm);
        SCHEME_EXPAND_OBSERVE_MODULE_LIFT_LOOP(observer, fst);
      } else {
	/* No definition lifts added... */
	if (SCHEME_STX_PAIRP(e))
	  fst = SCHEME_STX_CAR(e);
	else
	  fst = NULL;
	
	if (fst && SCHEME_STX_SYMBOLP(fst) && scheme_stx_module_eq_x(scheme_begin_stx, fst, phase)) {
	  fm = SCHEME_STX_CDR(fm);
	  e = scheme_add_rename(e, bxs->post_ex_rn_set);
          SCHEME_EXPAND_OBSERVE_RENAME_ONE(observer, e);
	  fm = scheme_flatten_begin(e, fm);
	  SCHEME_EXPAND_OBSERVE_SPLICE(observer, fm);
	  if (SCHEME_STX_NULLP(fm)) {
            e = scheme_frame_get_provide_lifts(xenv);
            e = scheme_reverse(e);
            if (expand_ends) {
              fm = scheme_frame_get_end_statement_lifts(xenv);
              fm = reverse_and_add_rename(fm, post_ex_rn);
              if (!SCHEME_NULLP(e))
                fm = scheme_append(fm, e);
              maybe_has_lifts = 0;
            } else
              fm = e;
            if (SCHEME_NULLP(fm) && expand_ends)
              fm = get_higher_phase_lifts(bxs, begin_for_syntax_stx);
            SCHEME_EXPAND_OBSERVE_MODULE_LIFT_END_LOOP(observer, fm);
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

    e = scheme_add_rename(e, bxs->post_ex_rn_set);

    SCHEME_EXPAND_OBSERVE_RENAME_ONE(observer, e);
    
    if (SCHEME_STX_PAIRP(e)) {
      Scheme_Object *fst;

      fst = SCHEME_STX_CAR(e);

      if (SCHEME_STX_SYMBOLP(fst)) {
	if (scheme_stx_module_eq_x(scheme_define_values_stx, fst, phase)) {
	  /************ define-values *************/
	  Scheme_Object *vars, *val;
          int var_count = 0;

          SCHEME_EXPAND_OBSERVE_ENTER_PRIM(observer, e);
          SCHEME_EXPAND_OBSERVE_PRIM_DEFINE_VALUES(observer);

	  /* Create top-level vars */
	  scheme_define_parse(e, &vars, &val, 0, env, 1);

	  while (SCHEME_STX_PAIRP(vars)) {
	    Scheme_Object *name, *orig_name;

	    name = SCHEME_STX_CAR(vars);

	    orig_name = name;

	    /* Remember the original: */
	    all_rt_defs = scheme_make_pair(name, all_rt_defs);
	    
	    name = scheme_tl_id_sym(env->genv, name, NULL, 2, NULL, NULL);

	    /* Check that it's not yet defined: */
	    if (scheme_lookup_in_table(env->genv->toplevel, (const char *)name)) {
	      scheme_wrong_syntax(who, orig_name, e, "duplicate definition for identifier");
	      return NULL;
	    }

	    /* Not required: */
	    if (check_already_required(required, name)) {
	      scheme_wrong_syntax(who, orig_name, e, "identifier is already imported");
	      return NULL;
	    }

	    /* Not syntax: */
	    if (scheme_lookup_in_table(env->genv->syntax, (const char *)name)) {
	      scheme_wrong_syntax(who, orig_name, e, "duplicate definition for identifier");
	      return NULL;
	    }

	    /* Create the bucket, indicating that the name will be defined: */
	    scheme_add_global_symbol(name, scheme_undefined, env->genv);

	    /* Add a renaming: */
	    if (!SAME_OBJ(SCHEME_STX_VAL(orig_name), name)) {
	      scheme_extend_module_rename(post_ex_rn, self_modidx, name, name, self_modidx, name, phase, NULL, NULL, 0);
              *bxs->all_simple_renames = 0;
	    } else
	      scheme_extend_module_rename(rn, self_modidx, name, name, self_modidx, name, phase, NULL, NULL, 0);

	    vars = SCHEME_STX_CDR(vars);
            var_count++;
	  }

          if (!(non_phaseless & NON_PHASELESS_FORM) && !phaseless_rhs(val, var_count, phase)) {
            non_phaseless |= NON_PHASELESS_FORM;
            non_phaseless_form = val;
          }
          
          SCHEME_EXPAND_OBSERVE_EXIT_PRIM(observer, e);
	  kind = DEFN_MODFORM_KIND;
        } else if (scheme_stx_module_eq_x(scheme_define_syntaxes_stx, fst, phase)
                   || scheme_stx_module_eq_x(scheme_begin_for_syntax_stx, fst, phase)) {
	  /************ define-syntaxes & begin-for-syntax *************/
	  /* Define the macro: */
	  Scheme_Compile_Info mrec, erec1;
	  Scheme_Object *names, *l, *code, *m, *vec, *boundname;
	  Resolve_Prefix *rp;
	  Resolve_Info *ri;
	  Scheme_Comp_Env *oenv, *eenv;
	  Optimize_Info *oi;
	  int count = 0;
	  int for_stx;
          int use_post_ex = 0;
          int max_let_depth;

	  for_stx = scheme_stx_module_eq_x(scheme_begin_for_syntax_stx, fst, phase);
          
          SCHEME_EXPAND_OBSERVE_ENTER_PRIM(observer, e);

          if (for_stx) {
            SCHEME_EXPAND_OBSERVE_PRIM_BEGIN_FOR_SYNTAX(observer);
            if (scheme_stx_proper_list_length(e) < 0)
              scheme_wrong_syntax(NULL, NULL, e, NULL);
            code = e;
          } else {
            SCHEME_EXPAND_OBSERVE_PRIM_DEFINE_SYNTAXES(observer);
            scheme_define_parse(e, &names, &code, 1, env, 1);
          }

          if (!for_stx && SCHEME_STX_PAIRP(names) && SCHEME_STX_NULLP(SCHEME_STX_CDR(names)))
            boundname = SCHEME_STX_CAR(names);
          else
            boundname = scheme_false;

          SCHEME_EXPAND_OBSERVE_PREPARE_ENV(observer);
          
	  scheme_prepare_exp_env(env->genv);
	  scheme_prepare_compile_env(env->genv->exp_env);
	  eenv = scheme_new_comp_env(env->genv->exp_env, env->insp, 0);
          if (!for_stx)
            scheme_frame_captures_lifts(eenv, NULL, NULL, scheme_false, scheme_false, 
                                        req_data, scheme_false);

	  oenv = env;
	  
          if (!for_stx) {
            for (l = names; SCHEME_STX_PAIRP(l); l = SCHEME_STX_CDR(l)) {
              Scheme_Object *name, *orig_name;
              name = SCHEME_STX_CAR(l);

              orig_name = name;

              /* Remember the original: */
              all_rt_defs = scheme_make_pair(name, all_rt_defs);
	    
              name = scheme_tl_id_sym(oenv->genv, name, NULL, 2, NULL, NULL);
	    
              if (scheme_lookup_in_table(oenv->genv->syntax, (const char *)name)) {
                scheme_wrong_syntax(who, orig_name, e, 
                                    "duplicate definition for identifier");
                return NULL;
              }
	    
              /* Check that it's not yet defined: */
              if (scheme_lookup_in_table(oenv->genv->toplevel, (const char *)name)) {
                scheme_wrong_syntax(who, orig_name, e, 
                                    "duplicate definition for identifier");
                return NULL;
              }

              /* Not required: */
              if (check_already_required(required, name)) {
                scheme_wrong_syntax(who, orig_name, e, "identifier is already imported");
                return NULL;
              }

              if (!SAME_OBJ(SCHEME_STX_VAL(orig_name), name)) {
                scheme_extend_module_rename(post_ex_rn, self_modidx, name, name, self_modidx, name,
                                            phase, NULL, NULL, 0);
                *bxs->all_simple_renames = 0;
                use_post_ex = 1;
              } else
                scheme_extend_module_rename(rn, self_modidx, name, name, self_modidx, name,
                                            phase, NULL, NULL, 0);

              count++;
            }
          }

          if (for_stx)
            names = NULL;
          else
            names = scheme_named_map_1(NULL, stx_sym, names, (Scheme_Object *)oenv->genv);
	  
	  mrec.comp = 1;
	  mrec.dont_mark_local_use = 0;
	  mrec.resolve_module_ids = 0;
	  mrec.value_name = NULL;
          mrec.observer = NULL;
          mrec.pre_unwrapped = 0;
          mrec.env_already = 0;
          mrec.comp_flags = rec[drec].comp_flags;

          if (erec) {
            erec1.comp = 0;
            erec1.depth = -1;
            erec1.value_name = boundname;
            erec1.observer = observer;
            erec1.pre_unwrapped = 0;
            erec1.env_already = 0;
            erec1.comp_flags = rec[drec].comp_flags;
          }

	  if (for_stx) {
            adt = scheme_hash_tree_set(bxs->all_defs, scheme_make_integer(phase), all_rt_defs);
            bxs->all_defs = adt;
            if (erec) {
              SCHEME_EXPAND_OBSERVE_PHASE_UP(observer);
              /* We expand & compile the for-syntax code in one pass. */
            }
            m = do_module_begin_at_phase(code, eenv, 
                                         &mrec, 0, 
                                         (erec ? &erec1 : NULL), 0,
                                         phase + 1, body_lists,
                                         bxs);
            if (erec) {
              code = SCHEME_STX_CAR(code);
              code = scheme_make_pair(code, SCHEME_CAR(m));
              m = SCHEME_CDR(m);
            }
            if (rec[drec].comp)
              body_lists = SCHEME_CDR(m);
            m = SCHEME_CAR(m);
            /* turn list of compiled expressions into a splice: */
            m = scheme_make_sequence_compilation(m, 0);
            if (m->type == scheme_sequence_type)
              m->type = scheme_splice_sequence_type;
          } else {
            if (erec) {
              SCHEME_EXPAND_OBSERVE_PHASE_UP(observer);
              code = scheme_expand_expr_lift_to_let(code, eenv, &erec1, 0);
            }
            m = scheme_compile_expr_lift_to_let(code, eenv, &mrec, 0);
          }

          if (!for_stx)
            lifted_reqs = scheme_append(scheme_frame_get_require_lifts(eenv), lifted_reqs);

	  oi = scheme_optimize_info_create(eenv->prefix, 1);
          scheme_optimize_info_set_context(oi, (Scheme_Object *)env->genv->module);
          if (!(rec[drec].comp_flags & COMP_CAN_INLINE))
            scheme_optimize_info_never_inline(oi);
	  m = scheme_optimize_expr(m, oi, 0);
	  
	  /* Simplify only in compile mode; it is too slow in expand mode. */
	  rp = scheme_resolve_prefix(1, eenv->prefix, !erec);
	  ri = scheme_resolve_info_create(rp);
          scheme_enable_expression_resolve_lifts(ri);
	  m = scheme_resolve_expr(m, ri);
          m = scheme_merge_expression_resolve_lifts(m, rp, ri);
          rp = scheme_remap_prefix(rp, ri);

          max_let_depth = scheme_resolve_info_max_let_depth(ri);

	  /* Add code with names and lexical depth to exp-time body: */
	  vec = scheme_make_vector(5, NULL);
	  SCHEME_VEC_ELS(vec)[0] = (for_stx
                                    ? scheme_false
                                    : ((SCHEME_PAIRP(names) && SCHEME_NULLP(SCHEME_CDR(names)))
                                       ? SCHEME_CAR(names)
                                       : names));
	  SCHEME_VEC_ELS(vec)[1] = m;
          SCHEME_VEC_ELS(vec)[2] = scheme_make_integer(max_let_depth);
	  SCHEME_VEC_ELS(vec)[3] = (Scheme_Object *)rp;
	  SCHEME_VEC_ELS(vec)[4] = (for_stx ? scheme_true : scheme_false);
	  exp_body = scheme_make_pair(vec, exp_body);

          if (eenv->prefix->unbound)
            unbounds = scheme_make_pair(eenv->prefix->unbound, unbounds);
	
          m = scheme_sfs(m, NULL, max_let_depth);
	  if (scheme_startup_use_jit /* Note: not scheme_resolve_info_use_jit(ri) */)
	    m = scheme_jit_expr(m);
          rp = scheme_prefix_eval_clone(rp);
          
	  eval_exptime(names, count, m, eenv->genv, rhs_env, rp, max_let_depth, 0, 
                       (for_stx ? env->genv->exp_env->toplevel : env->genv->syntax), 
                       phase + 1,
                       for_stx ? scheme_false : (use_post_ex ? post_ex_rn : rn),
                       NULL);
          
	  if (erec) {
            if (for_stx) {
              m = code;
            } else {
              m = SCHEME_STX_CDR(e);
              m = SCHEME_STX_CAR(m);
              m = scheme_make_pair(fst,
                                   scheme_make_pair(m, scheme_make_pair(code, scheme_null)));
            }
	    e = scheme_datum_to_syntax(m, e, e, 0, 2);
	  } else
            e = NULL;
          
          SCHEME_EXPAND_OBSERVE_EXIT_PRIM(observer, e);

          kind = DONE_MODFORM_KIND;

          non_phaseless |= NON_PHASELESS_FORM;
          if (!non_phaseless_form)
            non_phaseless_form = e;
	} else if (scheme_stx_module_eq_x(require_stx, fst, phase)) {
	  /************ require *************/
          SCHEME_EXPAND_OBSERVE_ENTER_PRIM(observer, e);
          SCHEME_EXPAND_OBSERVE_PRIM_REQUIRE(observer);

	  /* Adds requires to renamings and required modules to requires lists: */
	  parse_requires(e, phase, self_modidx, env->genv, env->genv->module,
                         rn_set, bxs->post_ex_rn_set,
                         check_require_name, bxs->tables,
                         bxs->redef_modname, 
                         0, 0, 1, 
                         1, phase ? 1 : 0,
                         bxs->all_simple_renames, bxs->modidx_cache,
                         bxs->submodule_names,
                         &non_phaseless);

	  if (!erec)
	    e = NULL;

          SCHEME_EXPAND_OBSERVE_EXIT_PRIM(observer, e);
	  kind = DONE_MODFORM_KIND;
	} else if (scheme_stx_module_eq_x(provide_stx, fst, phase)) {
	  /************ provide *************/
          /* remember it for pass 3 */
          p = scheme_make_pair(scheme_make_pair(e, scheme_make_integer(phase)),
                               bxs->saved_provides);
          bxs->saved_provides = p;
          kind = PROVIDE_MODFORM_KIND;
	} else if (scheme_stx_module_eq_x(declare_stx, fst, phase)) {
	  /************ declare *************/
          Scheme_Object *kws, *kw;
          
          kws = SCHEME_STX_CDR(e);
          while (SCHEME_STX_PAIRP(kws)) {
            kw = SCHEME_STX_CAR(kws);
            if (SCHEME_KEYWORDP(SCHEME_STX_VAL(kw))) {
              if (SAME_OBJ(SCHEME_STX_VAL(kw), phaseless_keyword)) {
                if (requested_phaseless)
                  scheme_wrong_syntax(who, kw, e, "duplicate declaration");
                requested_phaseless = 1;
              } else {
                scheme_wrong_syntax(who, kw, e, "unrecognized keyword");
              }
            }
            kws = SCHEME_STX_CDR(kws);
          }
          if (!SCHEME_STX_NULLP(kws))
            scheme_wrong_syntax(who, NULL, e, IMPROPER_LIST_FORM);
          
          kind = SAVED_MODFORM_KIND;
	} else if (scheme_stx_module_eq_x(scheme_module_stx, fst, phase)
                   || scheme_stx_module_eq_x(scheme_modulestar_stx, fst, phase)) {
	  /************ module[*] *************/
          /* check outer syntax & name, then expand pre-module or remember for post-module pass */
          Scheme_Object *name = NULL;
          int is_star;

          is_star = scheme_stx_module_eq_x(scheme_modulestar_stx, fst, phase);

          SCHEME_EXPAND_OBSERVE_ENTER_PRIM(observer, e);
          if (is_star) {
            SCHEME_EXPAND_OBSERVE_PRIM_SUBMODULE_STAR(observer);
          } else {
            SCHEME_EXPAND_OBSERVE_PRIM_SUBMODULE(observer);
          }

          if (SCHEME_STX_PAIRP(e)) {
            p = SCHEME_STX_CDR(e);
            if (SCHEME_STX_PAIRP(p)) {
              name = SCHEME_STX_CAR(p);
              p = SCHEME_STX_CDR(p);
              if (!SCHEME_STX_SYMBOLP(name)
                  || !SCHEME_STX_PAIRP(p)) {
                name = NULL;
              }
            }
          }
          if (!name) {
            scheme_wrong_syntax(who, NULL, e, NULL);
          }

          if (!bxs->submodule_names) {
            Scheme_Hash_Table *smn;
            smn = scheme_make_hash_table(SCHEME_hash_ptr);
            bxs->submodule_names = smn;
          }
          if (scheme_hash_get(bxs->submodule_names, SCHEME_STX_VAL(name))) {
            scheme_wrong_syntax(who, name, fst, "duplicate submodule definition");
          }
          scheme_hash_set(bxs->submodule_names, 
                          SCHEME_STX_VAL(name), 
                          is_star ? scheme_void : scheme_true);

          if (!is_star) {
            p = expand_submodules(erec ? erec : rec, erec ? derec :drec, env, 
                                  scheme_make_pair(scheme_make_pair(e, scheme_make_integer(phase)), scheme_null), 0, 
                                  bxs, !!erec);
            if (erec)
              e = SCHEME_CAR(p);
            else
              e = NULL;
            kind = DONE_MODFORM_KIND;
          } else {
            p = scheme_make_pair(scheme_make_pair(e, scheme_make_integer(phase)), 
                                 bxs->saved_submodules);
            bxs->saved_submodules = p;
            kind = MODULE_MODFORM_KIND;
          }
          SCHEME_EXPAND_OBSERVE_EXIT_PRIM(observer,e);
	} else {
	  kind = EXPR_MODFORM_KIND;
          non_phaseless |= NON_PHASELESS_FORM;
          if (!non_phaseless_form)
            non_phaseless_form = e;
        }
      } else {
	kind = EXPR_MODFORM_KIND;
        non_phaseless |= NON_PHASELESS_FORM;
        if (!non_phaseless_form)
            non_phaseless_form = e;
      }
    } else {
      kind = EXPR_MODFORM_KIND;
      non_phaseless |= NON_PHASELESS_FORM;
      if (!non_phaseless_form)
        non_phaseless_form = e;
    }

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
      e = scheme_frame_get_provide_lifts(xenv);
      e = scheme_reverse(e);
      if (expand_ends) {
        fm = scheme_frame_get_end_statement_lifts(xenv);
        fm = reverse_and_add_rename(fm, post_ex_rn);
        if (!SCHEME_NULLP(e))
          fm = scheme_append(fm, e);
        maybe_has_lifts = 0;
        if (SCHEME_NULLP(fm))
          fm = get_higher_phase_lifts(bxs, begin_for_syntax_stx);
      } else
        fm = e;
      SCHEME_EXPAND_OBSERVE_MODULE_LIFT_END_LOOP(observer, fm);
    }
  }
  /* first =  a list of (cons semi-expanded-expression kind) */

  if (!expand_ends) {
    if (maybe_has_lifts)
      end_statements = scheme_frame_get_end_statement_lifts(xenv);
  }

  if (!phase) {
    /* Bound names will not be re-bound at this point: */
    if (!erec || (erec[derec].depth != -2)) {
      scheme_seal_module_rename_set(rn_set, STX_SEAL_BOUND);
    }
    scheme_seal_module_rename_set(bxs->post_ex_rn_set, STX_SEAL_BOUND);

    /* Check that all bindings used in phase-N expressions (for N >= 1) 
       were defined by now: */
    check_formerly_unbound(unbounds, env);
  }

  /* Pass 2 */
  SCHEME_EXPAND_OBSERVE_NEXT_GROUP(observer);
  
  if (rec[drec].comp) {
    /* Module and each `begin-for-syntax' group manages its own prefix: */
    cenv = scheme_new_comp_env(env->genv, env->insp, SCHEME_TOPLEVEL_FRAME);
  } else
    cenv = scheme_extend_as_toplevel(env);

  lift_data = scheme_make_vector(3, NULL);
  SCHEME_VEC_ELS(lift_data)[0] = (Scheme_Object *)cenv;
  SCHEME_VEC_ELS(lift_data)[1] = self_modidx;
  SCHEME_VEC_ELS(lift_data)[2] = rn;

  maybe_has_lifts = 0;

  prev_p = NULL;
  expanded_l = scheme_null;
  for (p = first; !SCHEME_NULLP(p); ) {
    Scheme_Object *e, *l, *ll;
    int kind;

    e = SCHEME_CAR(p);
    kind = SCHEME_INT_VAL(SCHEME_CDR(e));
    e = SCHEME_CAR(e);
    
    SCHEME_EXPAND_OBSERVE_NEXT(observer);

    if (kind == SAVED_MODFORM_KIND) {
      expanded_l = scheme_make_pair(SCHEME_CDR(e), expanded_l);
      SCHEME_CAR(p) = SCHEME_CAR(e);
      prev_p = p;
      p = SCHEME_CDR(p);      
    } else if ((kind == PROVIDE_MODFORM_KIND)
               || (kind == MODULE_MODFORM_KIND)) {
      /* handle `provide's and `module's in later passes */
      if (erec)
        expanded_l = scheme_make_pair(e, expanded_l);
      if (rec[drec].comp) {
        if (!prev_p)
          first = SCHEME_CDR(p);
        else
          SCHEME_CDR(prev_p) = SCHEME_CDR(p);
      }
      p = SCHEME_CDR(p);
    } else if ((kind == EXPR_MODFORM_KIND)
               || (kind == DEFN_MODFORM_KIND)) {
      Scheme_Comp_Env *nenv;

      l = (maybe_has_lifts 
           ? scheme_frame_get_end_statement_lifts(cenv) 
           : end_statements);
      ll = (maybe_has_lifts 
            ? scheme_frame_get_provide_lifts(cenv) 
            : scheme_null);
      scheme_frame_captures_lifts(cenv, add_lifted_defn, lift_data, l, lift_ctx, req_data, ll);
      maybe_has_lifts = 1;

      if (kind == DEFN_MODFORM_KIND)
        nenv = cenv;
      else
        nenv = scheme_new_compilation_frame(0, 0, cenv);

      if (erec) {
	Scheme_Expand_Info erec1;
	scheme_init_expand_recs(erec, derec, &erec1, 1);
	erec1.value_name = scheme_false;
	e = scheme_expand_expr(e, nenv, &erec1, 0);
        expanded_l = scheme_make_pair(e, expanded_l);        
      }

      if (rec[drec].comp) {
	Scheme_Compile_Info crec1;
	scheme_init_compile_recs(rec, drec, &crec1, 1);
	crec1.resolve_module_ids = 0;
	e = scheme_compile_expr(e, nenv, &crec1, 0);
      }

      lifted_reqs = scheme_append(scheme_frame_get_require_lifts(cenv), lifted_reqs);
      
      l = scheme_frame_get_lifts(cenv);
      if (SCHEME_NULLP(l)) {
	/* No lifts - continue normally */
	SCHEME_CAR(p) = e;
	prev_p = p;
	p = SCHEME_CDR(p);
      } else {
	/* Lifts - insert them and try again */
        *bxs->all_simple_renames = 0;
        SCHEME_EXPAND_OBSERVE_MODULE_LIFT_LOOP(observer, scheme_copy_list(l));
        if (erec) {
          e = scheme_make_pair(scheme_make_pair(e, SCHEME_CAR(expanded_l)),
                               scheme_make_integer(SAVED_MODFORM_KIND)); /* kept both expanded & maybe compiled */
          /* add back expanded at correct position later: */
          expanded_l = SCHEME_CDR(expanded_l);
        } else
          e = scheme_make_pair(e, scheme_make_integer(DONE_MODFORM_KIND)); /* don't re-compile/-expand */
	SCHEME_CAR(p) = e;
	for (ll = l; SCHEME_PAIRP(ll); ll = SCHEME_CDR(ll)) {
	  e = scheme_make_pair(SCHEME_CAR(ll), scheme_make_integer(DEFN_MODFORM_KIND));
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
      if (erec)
        expanded_l = scheme_make_pair(e, expanded_l);
      SCHEME_CAR(p) = e;
      prev_p = p;
      p = SCHEME_CDR(p);
    }

    /* If we're out of declarations, check for lifted-to-end: */
    if (SCHEME_NULLP(p) && maybe_has_lifts) {
      int expr_cnt;
      Scheme_Object *sp;
      e = scheme_frame_get_provide_lifts(cenv);
      e = scheme_reverse(e);
      if (expand_ends) {
        p = scheme_frame_get_end_statement_lifts(cenv);
        p = scheme_reverse(p);
        expr_cnt = scheme_list_length(p);
        if (!SCHEME_NULLP(e))
          p = scheme_append(p, e);
      } else {
        p = e;
        expr_cnt = 0;
      }
      SCHEME_EXPAND_OBSERVE_MODULE_LIFT_END_LOOP(observer, p);
      for (ll = p; SCHEME_PAIRP(ll); ll = SCHEME_CDR(ll)) {
        e = SCHEME_CAR(ll);
        if (expr_cnt <= 0) {
          sp = scheme_make_pair(scheme_make_pair(e, scheme_make_integer(phase)),
                                bxs->saved_provides);
          bxs->saved_provides = sp;
        }
        e = scheme_make_pair(e, ((expr_cnt > 0) 
                                 ? scheme_make_integer(EXPR_MODFORM_KIND) 
                                 : scheme_make_integer(PROVIDE_MODFORM_KIND)));
        SCHEME_CAR(ll) = e;
        expr_cnt--;
      }
      maybe_has_lifts = 0;
      if (prev_p) {
        SCHEME_CDR(prev_p) = p;
      } else {
        first = p;
      }
    }
  }
  if (erec) expanded_l = scheme_reverse(expanded_l);

  /* If not phase 0, save end statements */
  if (!expand_ends) {
    if (maybe_has_lifts)
      end_statements = scheme_frame_get_end_statement_lifts(cenv);
    if (!SCHEME_NULLP(end_statements) || !SCHEME_NULLP(bxs->end_statementss)) {
      p = scheme_make_pair(end_statements, bxs->end_statementss);
      bxs->end_statementss = p;
    }
  }

  adt = scheme_hash_tree_set(bxs->all_defs, scheme_make_integer(phase), all_rt_defs);
  bxs->all_defs = adt;

  /* Pass 3 */
  /* if at phase 0, expand provides for all phases */
  SCHEME_EXPAND_OBSERVE_NEXT_GROUP(observer);
  
  if (phase == 0) {
    Scheme_Object *expanded_provides;

    expanded_provides = expand_all_provides(form, cenv, 
                                            (erec ? erec : rec), (erec ? derec : drec),
                                            self_modidx,
                                            bxs, !!erec);
  
    if (erec) {
      expanded_provides = scheme_reverse(expanded_provides);
      (void)fixup_expanded(expanded_l, expanded_provides, 0, PROVIDE_MODFORM_KIND);
    }
  }

  /* first = a list of compiled expressions */
  /* expanded_l = list of expanded expressions */

  /* If compiling, drop expressions that are constants: */
  if (rec[drec].comp) {
    Scheme_Object *prev = NULL, *next;
    for (p = first; !SCHEME_NULLP(p); p = next) {
      next = SCHEME_CDR(p);
      if (scheme_omittable_expr(SCHEME_CAR(p), -1, -1, 0, NULL, NULL, -1, 0)) {
	if (prev)
	  SCHEME_CDR(prev) = next;
	else
	  first = next;
      } else
	prev = p;
    }
  }

  if (phase == 0) {
    if (!erec || (erec[derec].depth != -2)) {
      scheme_seal_module_rename_set(rn_set, STX_SEAL_ALL);
    }
    scheme_seal_module_rename_set(bxs->post_ex_rn_set, STX_SEAL_ALL);
  }

  adt = scheme_hash_tree_set(bxs->all_defs, scheme_make_integer(phase), all_rt_defs);
  bxs->all_defs = adt;

  if (cenv->prefix->non_phaseless)
    non_phaseless |= NON_PHASELESS_IMPORT;

  if (!phase)
    env->genv->module->comp_prefix = cenv->prefix;
  else
    env->prefix = cenv->prefix;

  if (!SCHEME_NULLP(exp_body)) {
    if (*bxs->_num_phases < phase + 2)
      *bxs->_num_phases = phase + 2;
  }

  if (erec) {
    /* Add lifted requires */
    if (!SCHEME_NULLP(lifted_reqs)) {
      lifted_reqs = scheme_reverse(lifted_reqs);
      expanded_l = scheme_append(lifted_reqs, expanded_l);
    }
  }

  if (requested_phaseless) {
    if (!non_phaseless)
      env->genv->module->phaseless = scheme_true;
    else {
      if (non_phaseless & NON_PHASELESS_IMPORT)
        scheme_wrong_syntax(who, NULL, form, "cannot be cross-phase persistent due to required modules");
      else
        scheme_wrong_syntax(who, non_phaseless_form, form, "does not satisfy cross-phase persistent grammar");
    }
  }

  if (rec[drec].comp) {
    body_lists = scheme_make_pair(first, scheme_make_pair(exp_body, body_lists));
    if (erec)
      return scheme_make_pair(expanded_l, body_lists);
    else
      return body_lists;
  } else
    return expanded_l;
}

static Scheme_Object *expand_all_provides(Scheme_Object *form,
                                          Scheme_Comp_Env *cenv, 
                                          Scheme_Compile_Expand_Info *rec, int drec,
                                          Scheme_Object *self_modidx,
                                          Module_Begin_Expand_State *bxs,
                                          int keep_expanded)
/* expands `#%provide's for all phases in a module that is otherwise
   fully expanded; returns a list of expanded forms in reverse order,
   if requested by `keep_expanded'. */
{
  Scheme_Object *saved_provides;
  Scheme_Object *observer, *expanded_provides = scheme_null;
  int provide_phase;
  Scheme_Object *e, *ex, *fst;
  Scheme_Comp_Env *pcenv;

  observer = rec[drec].observer;
  
  saved_provides = scheme_reverse(bxs->saved_provides);
  while (!SCHEME_NULLP(saved_provides)) {
    e = SCHEME_CAR(saved_provides);
    provide_phase = SCHEME_INT_VAL(SCHEME_CDR(e));
    e = SCHEME_CAR(e);
    
    fst = SCHEME_STX_CAR(e);
    
    /* Expand and add provides to table: */
    
    SCHEME_EXPAND_OBSERVE_ENTER_PRIM(observer, e);
    SCHEME_EXPAND_OBSERVE_PRIM_PROVIDE(observer);
      
    ex = e;

    if (provide_phase != 0) {
      Scheme_Env *penv = cenv->genv;
      int k;
      for (k = 0; k < provide_phase; k++) {
        penv = penv->exp_env;
      }
      if (rec[drec].comp)
        pcenv = scheme_new_comp_env(penv, penv->access_insp, SCHEME_TOPLEVEL_FRAME);
      else
        pcenv = scheme_new_expand_env(penv, penv->access_insp, SCHEME_TOPLEVEL_FRAME);
    } else {
      pcenv = cenv;
    }

    parse_provides(form, fst, e, provide_phase,
                   bxs->all_provided, bxs->all_reprovided,
                   self_modidx,
                   bxs->all_defs_out,
                   bxs->tables,
                   bxs->all_defs, 
                   pcenv, rec, drec,
                   &ex);
    
    if (keep_expanded)
      expanded_provides = scheme_make_pair(ex, expanded_provides);
    
    SCHEME_EXPAND_OBSERVE_EXIT_PRIM(observer, e);
    
    saved_provides = SCHEME_CDR(saved_provides);
  }

  return expanded_provides;
}

static Scheme_Object *expand_submodules(Scheme_Compile_Expand_Info *rec, int drec,
                                        Scheme_Comp_Env *env,
                                        Scheme_Object *l, int post,
                                        Module_Begin_Expand_State *bxs,
                                        int keep_expanded)
{
  Scheme_Object *mods = scheme_null, *mod, *ancestry;
  
  ancestry = scheme_make_pair((Scheme_Object *)env->genv, env->genv->module->submodule_ancestry);
  /* do_module() will extend submodule_path */

  env = scheme_new_compilation_frame(0, 
                                     (SCHEME_TOPLEVEL_FRAME | SCHEME_NESTED_MODULE_FRAME),
                                     env);

  l = scheme_reverse(l);

  while (!SCHEME_NULLP(l)) {
    mod = SCHEME_CAR(l);
    SCHEME_EXPAND_OBSERVE_ENTER_PRIM(rec[drec].observer,SCHEME_CAR(mod));
    mod = do_module(SCHEME_CAR(mod), env, rec, drec, ancestry, env->genv->module->submodule_path, post,
                    bxs, SCHEME_CDR(mod));
    SCHEME_EXPAND_OBSERVE_EXIT_PRIM(rec[drec].observer,mod);

    mods = scheme_make_pair(mod, mods);

    l = SCHEME_CDR(l);
  }

  if (keep_expanded)
    mods = scheme_reverse(mods);

  if (rec[drec].comp) {
    if (post) {
      env->genv->module->post_submodules = mods;
      /* also reverse pres, now: */
      l = env->genv->module->pre_submodules;
      if (l) {
        l = scheme_reverse(l);
        env->genv->module->pre_submodules = l;
      }
    } else {
      l = env->genv->module->pre_submodules;
      if (!l) l = scheme_null;
      l = scheme_make_pair(SCHEME_CAR(mods), l);
      env->genv->module->pre_submodules = l;
    }
  } else if (!SCHEME_NULLP(mods)) {
    /* setting pre_submodules to '() indicates that there were submodules during expansion */
    env->genv->module->pre_submodules = scheme_null;
    if (!post) {
      l = env->genv->module->pre_submodule_names;
      if (!l) l = scheme_null;
      /* extract just the name: */
      mod = SCHEME_CAR(mods);
      mod = SCHEME_STX_CDR(mod);
      mod = SCHEME_STX_CAR(mod);
      mod = SCHEME_STX_VAL(mod);
      l = scheme_make_pair(mod, l);
      env->genv->module->pre_submodule_names = l;
    }
  }

  return mods;
}

static Scheme_Object *fixup_expanded(Scheme_Object *expanded_l,
                                     Scheme_Object *expanded_provides,
                                     int phase, int kind)
/* mutates `expanded_l' to find `#%provide's or `module's (possibly nested in
   `begin-for-syntax') and replace them with the ones in
   `expanded_provides'. The provides in `expanded_l' and
   `expanded_provides' are matched up by order. */
{
  Scheme_Object *p, *e, *fst, *prov_stx, *l;

  if (kind == PROVIDE_MODFORM_KIND)
    prov_stx = provide_stx;
  else
    prov_stx = scheme_modulestar_stx;

  for (p = expanded_l; !SCHEME_NULLP(p); p = SCHEME_CDR(p)) {
    e = SCHEME_CAR(p);
    if (SCHEME_STX_PAIRP(e)) {
      fst = SCHEME_STX_CAR(e);
      if (scheme_stx_module_eq_x(prov_stx, fst, phase)) {
        SCHEME_CAR(p) = SCHEME_CAR(expanded_provides);
        expanded_provides = SCHEME_CDR(expanded_provides);
      } else if (scheme_stx_module_eq_x(scheme_begin_for_syntax_stx, fst, phase)) {
        l = scheme_flatten_syntax_list(e, NULL);
        l = scheme_copy_list(l);
        expanded_provides = fixup_expanded(SCHEME_CDR(l), expanded_provides, phase + 1, kind);
        e = scheme_datum_to_syntax(l, e, e, 0, 2);
        SCHEME_CAR(p) = e;
      }
    }
  }

  return expanded_provides;
}

static void check_formerly_unbound(Scheme_Object *unbounds,
                                   Scheme_Comp_Env *env)
{
  Scheme_Object *stack = scheme_null, *lst, *p;
  Scheme_Env *uenv = env->genv->exp_env;

  while (!SCHEME_NULLP(unbounds)) {
    stack = scheme_null;
    uenv = env->genv->exp_env;

    lst = SCHEME_CAR(unbounds);
    while(1) {
      while (!SCHEME_NULLP(lst)) {
        p = SCHEME_CAR(lst);
        if (SCHEME_PAIRP(p)) {
          if (!uenv->exp_env)
            scheme_signal_error("internal error: no such environment to check unbounds");
          else {
            /* switch to nested list, push current list onto stack: */
            stack = scheme_make_pair(scheme_make_pair(SCHEME_CDR(lst), (Scheme_Object *)uenv),
                                     stack);
            uenv = uenv->exp_env;
            lst = SCHEME_CAR(lst);
          }
        } else {
          (void)scheme_check_top_identifier_bound(p, uenv, 1);
          lst = SCHEME_CDR(lst);
        }
      }
      if (!SCHEME_NULLP(stack)) {
        lst = SCHEME_CAR(stack);
        stack = SCHEME_CDR(stack);
        uenv = (Scheme_Env *)SCHEME_CDR(lst);
        lst = SCHEME_CAR(lst);
      } else
        break;
    }
    unbounds = SCHEME_CDR(unbounds);
  }

  /* Disallow unbound variables from now on: */
  uenv = env->genv->exp_env;
  while (uenv) {
    uenv->disallow_unbound = 1;
    uenv = uenv->exp_env;
  }
}

static int is_modulestar_stop(Scheme_Comp_Env *env)
{
  Scheme_Object *p;
  p = scheme_datum_to_syntax(scheme_intern_symbol("module*"), scheme_false, scheme_sys_wraps(env), 0, 0);
  p = scheme_lookup_binding(p, env, 
                            (SCHEME_NULL_FOR_UNBOUND
                             + SCHEME_DONT_MARK_USE 
                             + SCHEME_ENV_CONSTANTS_OK
                             + (SCHEME_OUT_OF_CONTEXT_OK | SCHEME_OUT_OF_CONTEXT_LOCAL)),
                            env->in_modidx, 
                            NULL, NULL, NULL, NULL);
  return (scheme_get_stop_expander() == p);
}

static void install_stops(Scheme_Comp_Env *xenv, int phase, Scheme_Object **_begin_for_syntax_stx)
{
  Scheme_Object *stop, *w, *s;

  stop = scheme_get_stop_expander();

  scheme_add_local_syntax(22, xenv);

  if (phase == 0) {
    scheme_set_local_syntax(0, scheme_begin_stx, stop, xenv);
    scheme_set_local_syntax(1, scheme_define_values_stx, stop, xenv);
    scheme_set_local_syntax(2, scheme_define_syntaxes_stx, stop, xenv);
    scheme_set_local_syntax(3, scheme_begin_for_syntax_stx, stop, xenv);
    *_begin_for_syntax_stx = scheme_begin_for_syntax_stx;
    scheme_set_local_syntax(4, require_stx, stop, xenv);
    scheme_set_local_syntax(5, provide_stx, stop, xenv);
    scheme_set_local_syntax(6, set_stx, stop, xenv);
    scheme_set_local_syntax(7, app_stx, stop, xenv);
    scheme_set_local_syntax(8, scheme_top_stx, stop, xenv);
    scheme_set_local_syntax(9, lambda_stx, stop, xenv);
    scheme_set_local_syntax(10, case_lambda_stx, stop, xenv);
    scheme_set_local_syntax(11, let_values_stx, stop, xenv);
    scheme_set_local_syntax(12, letrec_values_stx, stop, xenv);
    scheme_set_local_syntax(13, if_stx, stop, xenv);
    scheme_set_local_syntax(14, begin0_stx, stop, xenv);
    scheme_set_local_syntax(15, with_continuation_mark_stx, stop, xenv);
    scheme_set_local_syntax(16, letrec_syntaxes_stx, stop, xenv);
    scheme_set_local_syntax(17, var_ref_stx, stop, xenv);
    scheme_set_local_syntax(18, expression_stx, stop, xenv);
    scheme_set_local_syntax(19, scheme_modulestar_stx, stop, xenv);
    scheme_set_local_syntax(20, scheme_module_stx, stop, xenv);
    scheme_set_local_syntax(21, declare_stx, stop, xenv);
  } else {
    w = scheme_sys_wraps_phase_worker(phase);
    s = scheme_datum_to_syntax(scheme_intern_symbol("begin"), scheme_false, w, 0, 0);
    scheme_set_local_syntax(0, s, stop, xenv);
    s = scheme_datum_to_syntax(scheme_intern_symbol("define-values"), scheme_false, w, 0, 0);
    scheme_set_local_syntax(1, s, stop, xenv);
    s = scheme_datum_to_syntax(scheme_intern_symbol("define-syntaxes"), scheme_false, w, 0, 0);
    scheme_set_local_syntax(2, s, stop, xenv);
    s = scheme_datum_to_syntax(scheme_intern_symbol("begin-for-syntax"), scheme_false, w, 0, 0);
    scheme_set_local_syntax(3, s, stop, xenv);
    *_begin_for_syntax_stx = s;
    s = scheme_datum_to_syntax(scheme_intern_symbol("#%require"), scheme_false, w, 0, 0);
    scheme_set_local_syntax(4, s, stop, xenv);
    s = scheme_datum_to_syntax(scheme_intern_symbol("#%provide"), scheme_false, w, 0, 0);
    scheme_set_local_syntax(5, s, stop, xenv);
    scheme_set_local_syntax(6, scheme_datum_to_syntax(scheme_intern_symbol("set!"), scheme_false, w, 0, 0), stop, xenv);
    scheme_set_local_syntax(7, scheme_datum_to_syntax(scheme_intern_symbol("#%app"), scheme_false, w, 0, 0), stop, xenv);
    scheme_set_local_syntax(8, scheme_datum_to_syntax(scheme_intern_symbol("#%top"), scheme_false, w, 0, 0), stop, xenv);
    scheme_set_local_syntax(9, scheme_datum_to_syntax(scheme_intern_symbol("lambda"), scheme_false, w, 0, 0), stop, xenv);
    scheme_set_local_syntax(10, scheme_datum_to_syntax(scheme_intern_symbol("case-lambda"), scheme_false, w, 0, 0), stop, xenv);
    scheme_set_local_syntax(11, scheme_datum_to_syntax(scheme_intern_symbol("let-values"), scheme_false, w, 0, 0), stop, xenv);
    scheme_set_local_syntax(12, scheme_datum_to_syntax(scheme_intern_symbol("letrec-values"), scheme_false, w, 0, 0), stop, xenv);
    scheme_set_local_syntax(13, scheme_datum_to_syntax(scheme_intern_symbol("if"), scheme_false, w, 0, 0), stop, xenv);
    scheme_set_local_syntax(14, scheme_datum_to_syntax(scheme_intern_symbol("begin0"), scheme_false, w, 0, 0), stop, xenv);
    scheme_set_local_syntax(15, scheme_datum_to_syntax(scheme_intern_symbol("with-continuation-mark"), scheme_false, w, 0, 0), stop, xenv);
    scheme_set_local_syntax(16, scheme_datum_to_syntax(scheme_intern_symbol("letrec-syntaxes+values"), scheme_false, w, 0, 0), stop, xenv);
    scheme_set_local_syntax(17, scheme_datum_to_syntax(scheme_intern_symbol("#%variable-reference"), scheme_false, w, 0, 0), stop, xenv);
    scheme_set_local_syntax(18, scheme_datum_to_syntax(scheme_intern_symbol("#%expression"), scheme_false, w, 0, 0), stop, xenv);
    s = scheme_datum_to_syntax(scheme_intern_symbol("module*"), scheme_false, w, 0, 0);
    scheme_set_local_syntax(19, s, stop, xenv);
    s = scheme_datum_to_syntax(scheme_intern_symbol("module"), scheme_false, w, 0, 0);
    scheme_set_local_syntax(20, s, stop, xenv);
    s = scheme_datum_to_syntax(scheme_intern_symbol("#%declare"), scheme_false, w, 0, 0);
    scheme_set_local_syntax(21, s, stop, xenv);
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

static void check_already_provided(Scheme_Hash_Table *provided, Scheme_Object *outname, Scheme_Object *name, 
                                   int protected, Scheme_Object *form, Scheme_Object *phase)
{
  Scheme_Object *v;

  v = scheme_hash_get(provided, outname);
  if (v) {
    if (!scheme_stx_module_eq2(SCHEME_CAR(v), name, phase, NULL))
      scheme_wrong_syntax("module", outname, form, "identifier already provided (as a different binding)");
    
    if (protected && SCHEME_FALSEP(SCHEME_CDR(v)))
      scheme_wrong_syntax("module", outname, form, "identifier already provided as unprotected");
    if (!protected && SCHEME_TRUEP(SCHEME_CDR(v)))
      scheme_wrong_syntax("module", outname, form, "identifier already provided as protected");
  }
}

int compute_reprovides(Scheme_Hash_Table *all_provided,
                       Scheme_Hash_Table *all_reprovided, 
                       Scheme_Module *mod_for_requires,
                       Scheme_Hash_Table *tables,
                       Scheme_Env *_genv, 
                       int num_phases,
                       Scheme_Hash_Tree *all_defs, Scheme_Hash_Table *all_defs_out, 
                       const char *matching_form,
                       Scheme_Object *all_mods, /* a phase list to use for all mods */
                       Scheme_Object *all_phases) /* a module-path list for all phases */
{
  Scheme_Hash_Table *provided, *required;
  Scheme_Object *reprovided, *tvec;
  int i, k, z;
  Scheme_Object *rx, *provided_list, *phase, *req_phase;
  Scheme_Object *all_x_defs, *all_x_defs_out;
  Scheme_Env *genv;

  if (all_phases) {
    /* synthesize all_reprovided for the loop below: */
    if (all_mods)
      reprovided = scheme_make_pair(scheme_false, scheme_null);
    else
      reprovided = all_phases;
    all_reprovided = scheme_make_hash_table_equal();
    if (mod_for_requires->requires
        && !SCHEME_NULLP(mod_for_requires->requires))
      scheme_hash_set(all_reprovided, scheme_make_integer(0), reprovided);
    if (mod_for_requires->et_requires
        && !SCHEME_NULLP(mod_for_requires->et_requires))
      scheme_hash_set(all_reprovided, scheme_make_integer(1), reprovided);
    if (mod_for_requires->tt_requires
        && !SCHEME_NULLP(mod_for_requires->tt_requires))
      scheme_hash_set(all_reprovided, scheme_make_integer(-1), reprovided);
    if (mod_for_requires->dt_requires
        && !SCHEME_NULLP(mod_for_requires->dt_requires))
      scheme_hash_set(all_reprovided, scheme_false, reprovided);
    if (mod_for_requires->other_requires) {
      for (z = 0; z < mod_for_requires->other_requires->size; z++) {
        if (mod_for_requires->other_requires->vals[z])
          scheme_hash_set(all_reprovided, 
                          mod_for_requires->other_requires->keys[z],
                          reprovided);
      }
    }
  } else if (all_mods) {
    reprovided = scheme_make_pair(scheme_false, scheme_null);
    all_reprovided = scheme_make_hash_table_equal();
    while (SCHEME_PAIRP(all_mods)) {
      scheme_hash_set(all_reprovided, SCHEME_CAR(all_mods), reprovided);
      all_mods = SCHEME_CDR(all_mods);
    }
  }

  /* First, check the sanity of the re-provide specifications (unless
     we synthesized them): */
  if (!all_mods) {
    for (z = 0; z < all_reprovided->size; z++) {
      if (all_reprovided->vals[z]) {
        Scheme_Object *requires;

        reprovided = all_reprovided->vals[z];
        phase = all_reprovided->keys[z];

        if (SAME_OBJ(phase, scheme_make_integer(0))) {
          requires = mod_for_requires->requires;
        } else if (SAME_OBJ(phase, scheme_make_integer(1))) {
          requires = mod_for_requires->et_requires;
        } else if (SAME_OBJ(phase, scheme_make_integer(-1))) {
          requires = mod_for_requires->tt_requires;
        } else if (SAME_OBJ(phase, scheme_false)) {
          requires = mod_for_requires->dt_requires;
        } else {
          if (mod_for_requires->other_requires)
            requires = scheme_hash_get(mod_for_requires->other_requires, phase);
          else
            requires = NULL;
        }
        if (!requires)
          requires = scheme_null;
        
        for (rx = reprovided; !SCHEME_NULLP(rx); rx = SCHEME_CDR(rx)) {
          Scheme_Object *midx = SCHEME_CAR(SCHEME_CAR(rx)), *l, *exns;
	
          for (l = requires; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
            if (same_modidx(midx, SCHEME_CAR(l)))
              break;
          }
          if (SCHEME_NULLP(l)) {
            /* Didn't require the named module */
            if (matching_form) {
              Scheme_Object *name;
              name = SCHEME_CAR(rx);
              name = SCHEME_STX_CDR(name);
              name = SCHEME_STX_CAR(name);
              scheme_wrong_syntax("module", 
                                  SCHEME_MODNAMEP(midx) ? midx : ((Scheme_Modidx *)midx)->path, 
                                  name,
                                  "cannot provide from a module without a matching `%s'",
                                  matching_form);
            } else {
              return 0;
            }
          }

          exns = SCHEME_CDR(SCHEME_CDR(SCHEME_CAR(rx)));
          for (l = exns; !SCHEME_STX_NULLP(l); l = SCHEME_STX_CDR(l)) {
            /* Make sure excluded name was required: */
            Scheme_Object *a, *vec = NULL;
            a = SCHEME_STX_VAL(SCHEME_STX_CAR(l));

            for (k = 0; k < tables->size; k++) {
              if (tables->vals[k]) {
                tvec = tables->vals[k];
                required = (Scheme_Hash_Table *)SCHEME_VEC_ELS(tvec)[1];
                
                if (required)
                  vec = scheme_hash_get(required, a);
                else
                  vec = NULL;
      
                if (vec) {
                  /* Check for nominal modidx in list */
                  Scheme_Object *nml, *nml_modidx;
                  nml = SCHEME_VEC_ELS(vec)[0];
                  for (; SCHEME_PAIRP(nml); nml = SCHEME_CDR(nml)) {
                    nml_modidx = SCHEME_CAR(nml);
                    if (SCHEME_PAIRP(nml_modidx))
                      nml_modidx = SCHEME_CAR(nml_modidx);
                    if (same_modidx(SCHEME_CAR(SCHEME_CAR(rx)), nml_modidx))
                      break;
                  }
                  if (!SCHEME_PAIRP(nml))
                    vec = NULL; /* So it was provided, but not from the indicated module */
                }

                if (vec)
                  break;
              }
            }
            if (!vec) {
              a = SCHEME_STX_CAR(l);
              scheme_wrong_syntax("module", a, SCHEME_CADR(SCHEME_CAR(rx)),
                                  "excluded name was not required from the module");
            }
          }
        }
      }
    }
  }

  /* For each reprovided, walk through requires, check for re-provided bindings: */
  for (z = 0; z < all_reprovided->size; z++) {
    reprovided = all_reprovided->vals[z];
    if (reprovided && !SCHEME_NULLP(reprovided)) {
      phase = all_reprovided->keys[z];

      for (k = 0; k < tables->size; k++) {
        tvec = tables->vals[k];
        if (tvec) {
          required = (Scheme_Hash_Table *)SCHEME_VEC_ELS(tvec)[1];
          req_phase = tables->keys[k];

          for (i = required->size; i--; ) {
            if (required->vals[i]) {
              Scheme_Object *nominal_modidx, *name, *outname, *nml, *orig_nml, *mark_src;
              int break_outer = 0;
	
              name = required->keys[i]; /* internal symbolic name */
              orig_nml = SCHEME_VEC_ELS(required->vals[i])[0];
              outname = SCHEME_VEC_ELS(required->vals[i])[4];
              mark_src = SCHEME_VEC_ELS(required->vals[i])[6];

              for (rx = reprovided; !SCHEME_NULLP(rx); rx = SCHEME_CDR(rx)) {
                for (nml = orig_nml; SCHEME_PAIRP(nml); nml = SCHEME_CDR(nml)) {
                  nominal_modidx = SCHEME_CAR(nml);
                  if (SCHEME_PAIRP(nominal_modidx))
                    nominal_modidx = SCHEME_CAR(nominal_modidx);
                  if (all_mods || same_modidx(SCHEME_CAR(SCHEME_CAR(rx)), nominal_modidx)) {
                    Scheme_Object *nml_pi;

                    if (SCHEME_PAIRP(SCHEME_CAR(nml)))
                      nml_pi = SCHEME_CADR(SCHEME_CAR(nml));
                    else
                      nml_pi = scheme_make_integer(0);

                    if (SAME_OBJ(phase, nml_pi)) {
                      Scheme_Object *exns, *ree;

                      if (!all_mods) {
                        break_outer = 1;
                  
                        ree = SCHEME_CDR(SCHEME_CAR(rx));

                        exns = SCHEME_CDR(ree);
                      } else {
                        ree = NULL;
                        exns = scheme_null;
                      }
	    
                      for (; !SCHEME_STX_NULLP(exns); exns = SCHEME_STX_CDR(exns)) {
                        /* Was this name excluded? */
                        Scheme_Object *a;
                        a = SCHEME_STX_VAL(SCHEME_STX_CAR(exns));
                        if (SAME_OBJ(a, name))
                          break;
                      }

                      if (SCHEME_STX_NULLP(exns)) {
                        /* Not excluded, so provide it. */
                        if (matching_form) {
                          /* Assert: !all_mods */
                          provided = (Scheme_Hash_Table *)scheme_hash_get(all_provided, req_phase);
                          if (!provided) {
                            provided = scheme_make_hash_table(SCHEME_hash_ptr);
                            scheme_hash_set(all_provided, req_phase, (Scheme_Object *)provided);
                          }
                          check_already_provided(provided, outname, name, 0, SCHEME_CAR(ree), req_phase);
                          scheme_hash_set(provided, outname, scheme_make_pair(name, scheme_false));
                        } else {
                          if (SCHEME_TRUEP(mark_src)) {
                            if (SCHEME_SYM_PARALLELP(name)) {
                              /* reverse scheme_tl_id_sym */
                              char *s;
                              int len;
                              len = SCHEME_SYM_LEN(name);
                              s = scheme_malloc_atomic(len + 1);
                              memcpy(s, SCHEME_SYM_VAL(name), len+1);
                              while (len && (s[len] != '.')) {
                                --len;
                              }
                              s[len] = 0;
                              name = scheme_intern_exact_symbol(s, len);
                            }
                            name = scheme_datum_to_syntax(name, scheme_false, mark_src, 0, 0);
                          } else {
                            scheme_signal_error("found an import with no lexical context");
                          }

                          provided_list = scheme_hash_get(all_provided, req_phase);
                          if (!provided_list)
                            provided_list = scheme_null;
                          provided_list = scheme_make_pair(name, provided_list);
                          scheme_hash_set(all_provided, req_phase, provided_list);
                        }
                      }
                    }
                  }
                  if (break_outer) break;
                }
              }
            }
          }
        }
      }
    }
  }

  /* Do all-defined provides */
  genv = _genv;
  for (z = 0; z < num_phases; z++) {
    all_x_defs = scheme_hash_tree_get(all_defs, scheme_make_integer(z));
    if (!all_x_defs) all_x_defs = scheme_null;
    all_x_defs_out = scheme_hash_get(all_defs_out, scheme_make_integer(z));
    provided = (Scheme_Hash_Table *)scheme_hash_get(all_provided, scheme_make_integer(z));
    phase = scheme_make_integer(z);
    
    if (all_x_defs_out) {
      for (; !SCHEME_NULLP(all_x_defs_out); all_x_defs_out = SCHEME_CDR(all_x_defs_out)) {
        Scheme_Object *exns, *ree, *ree_kw, *exl, *name, *a, *adl, *exname, *pfx;
        int protected;
	    
        ree = SCHEME_CAR(all_x_defs_out);
        protected = SCHEME_TRUEP(SCHEME_CDR(ree));
        ree = SCHEME_CAR(ree);
        ree_kw = SCHEME_CAR(ree);
        ree = SCHEME_CDR(ree);
        exl = SCHEME_CAR(ree);
        pfx = SCHEME_CDR(ree);

        /* Make sure each excluded name was defined: */
        for (exns = exl; !SCHEME_STX_NULLP(exns); exns = SCHEME_STX_CDR(exns)) {
          a = SCHEME_STX_CAR(exns);
          name = scheme_tl_id_sym(genv, a, NULL, 0, NULL, NULL);
          if (!scheme_lookup_in_table(genv->toplevel, (const char *)name)
              && !scheme_lookup_in_table(genv->syntax, (const char *)name)) {
            scheme_wrong_syntax("module", a, ree_kw, "excluded identifier was not defined");
          }
        }

        for (adl = all_x_defs; SCHEME_PAIRP(adl); adl = SCHEME_CDR(adl)) {
          name = SCHEME_CAR(adl);
          exname = SCHEME_STX_SYM(name);
          name = scheme_tl_id_sym(genv, name, NULL, 0, NULL, NULL);
	
          /* Was this one excluded? */
          for (exns = exl; !SCHEME_STX_NULLP(exns); exns = SCHEME_STX_CDR(exns)) {
            a = SCHEME_STX_CAR(exns);
            a = scheme_tl_id_sym(genv, a, NULL, 0, NULL, NULL);
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
              a = scheme_tl_id_sym(genv, a, NULL, 0, NULL, NULL);
	    
              if (SAME_OBJ(a, name)) {
                /* Add prefix, if any */
                if (SCHEME_TRUEP(pfx)) {
                  exname = scheme_symbol_append(pfx, exname);
                }
                check_already_provided(provided, exname, name, protected, ree_kw, phase);
	      
                scheme_hash_set(provided, exname, 
                                scheme_make_pair(name, protected ? scheme_true : scheme_false));
              }
            }
          }
        }
      }
    }

    genv = _genv->exp_env;
  }

  return 1;
}

static Scheme_Object **compute_indirects(Scheme_Env *genv, 
                                         Scheme_Module_Phase_Exports *pt,
                                         int *_count,
                                         int vars)
{
  int i, count, j, start, end;
  Scheme_Bucket **bs, *b;
  Scheme_Object **exsns = pt->provide_src_names, **exis;
  int exicount;
  Scheme_Bucket_Table *t;

  if (vars) {
    start = 0;
    end = pt->num_var_provides;
  } else {
    start = pt->num_var_provides;
    end = pt->num_provides;
  }

  if (vars)
    t = genv->toplevel;
  else
    t = genv->syntax;
    

  if (!t)
    count = 0;
  else {
    bs = t->buckets;
    for (count = 0, i = t->size; i--; ) {
      b = bs[i];
      if (b && b->val)
        count++;
    }
  }

  if (!count) {
    *_count = 0;
    return NULL;
  }
  
  exis = MALLOC_N(Scheme_Object *, count);

  for (count = 0, i = t->size; i--; ) {
    b = bs[i];
    if (b && b->val) {
      Scheme_Object *name;
      
      name = (Scheme_Object *)b->key;
      
      /* If the name is directly provided, no need for indirect... */
      for (j = start; j < end; j++) {
        if (SAME_OBJ(name, exsns[j]))
          break;
      }
	
      if (j == end)
        exis[count++] = name;
    }
  }

  if (!count) {
    *_count = 0;
    return NULL;
  }
  
  exicount = count;

  qsort_provides(exis, NULL, NULL, NULL, NULL, NULL, 0, exicount, 1);

  *_count = exicount;
  return exis;
}

Scheme_Object *scheme_module_imported_list(Scheme_Env *genv, Scheme_Object *bindings, Scheme_Object *modpath,
                                           Scheme_Object *mode)
{
  Scheme_Object *l, *all_mods, *all_phases;
  Scheme_Hash_Table *tables, *all_reprovided, *all_provided;
  int v, i;

  tables = (Scheme_Hash_Table *)SCHEME_CAR(bindings);
  all_reprovided = scheme_make_hash_table_equal();

  if (SCHEME_FALSEP(modpath)) {
    if (SAME_OBJ(mode, scheme_true)) {
      all_mods = scheme_null; 
      all_phases = scheme_null; 
    } else {
      all_mods = scheme_make_pair(mode, scheme_null);
      all_phases = NULL;
    }
  } else {
    Scheme_Object *reprovided;

    modpath = convert_submodule_path(modpath, check_is_submodule, 
                                     (Scheme_Object *)genv);

    reprovided = scheme_make_pair(scheme_make_pair(modpath,
                                                   scheme_make_pair(scheme_false,
                                                                    scheme_null)),
                                  scheme_null);
    all_mods = NULL;
    if (SAME_OBJ(mode, scheme_true)) {
      all_phases = reprovided;
    } else {
      scheme_hash_set(all_reprovided, mode, reprovided);
      all_phases = NULL;
    }
  }

  /* Receives result: */
  all_provided = scheme_make_hash_table_equal();
  
  v = compute_reprovides(all_provided,
                         all_reprovided,
                         genv->module, 
                         tables,
                         genv, 
                         0,
                         NULL, NULL,
                         NULL,
                         all_mods, all_phases);
    
  if (!v) {
    return scheme_false;
  } else {
    l = scheme_null;
    for (i = 0; i < all_provided->size; i++) {
      if (all_provided->vals[i]) {
        l = scheme_make_pair(scheme_make_pair(all_provided->keys[i],
                                              all_provided->vals[i]),
                             l);
      }
    }

    return l;
  }
}

static Scheme_Object *adjust_for_rename(Scheme_Object *out_name, Scheme_Object *in_name, Scheme_Object *noms)
{
  Scheme_Object *first = scheme_null, *last = NULL, *p, *a;

  if (SCHEME_STXP(in_name))
    in_name = SCHEME_STX_VAL(in_name);

  if (SAME_OBJ(in_name, out_name))
    return noms;

  while (SCHEME_PAIRP(noms)) {
    a = SCHEME_CAR(noms);
    if (SCHEME_PAIRP(a)) {
      /* no change */
    } else {
      a = scheme_make_pair(a,
                           scheme_make_pair(scheme_make_integer(0),
                                            scheme_make_pair(in_name,
                                                             scheme_make_pair(scheme_make_integer(0),
                                                                              scheme_null))));
    }

    p = scheme_make_pair(a, scheme_null);
    if (last)
      SCHEME_CDR(last) = p;
    else
      first = p;
    last = p;

    noms = SCHEME_CDR(noms);
  }

  return first;
}

static Scheme_Object *extract_free_id_name(Scheme_Object *name,
                                           Scheme_Object *phase,
                                           Scheme_Env *genv,
                                           int always,
                                           int *_implicit,
                                           Scheme_Object **_implicit_src,
                                           Scheme_Object **_implicit_src_name,
                                           Scheme_Object **_implicit_mod_phase,
                                           Scheme_Object **_implicit_nominal_name,
                                           Scheme_Object **_implicit_nominal_mod)
{
  *_implicit = 0;

  if (genv) {
    if (SCHEME_FALSEP(phase)) {
      /* genv is used for tl_id_sym */
    } else {
      int i;
      i = SCHEME_INT_VAL(phase);
      if (i > 0) {
        for (; i--; ) {
          genv = genv->exp_env;
          if (!genv) break;
        }
      } else if (i < 0) {
        for (; i++; ) {
          genv = genv->template_env;
          if (!genv) break;
        }
      }
    }
  }

  while (1) { /* loop for free-id=? renaming */
    if (SCHEME_STXP(name)) {
      if (genv
          && (always || SCHEME_INTP(phase))) {
        name = scheme_tl_id_sym(genv, name, NULL, -1, phase, NULL);
      } else
        name = SCHEME_STX_VAL(name); /* shouldn't get here; no `define-for-label' */
    }
    
    /* Check for free-id=? renaming: */
    if (SAME_OBJ(phase, scheme_make_integer(0))) {
      Scheme_Object *v2;
      v2 = scheme_lookup_in_table(genv->syntax, (const char *)name);
      if (v2 && scheme_is_binding_rename_transformer(SCHEME_PTR_VAL(v2))) {
        Scheme_Object *name2;
        Scheme_Object *mod, *id, *rename_insp = NULL;
        Scheme_Object *mod_phase = NULL;

        name2 = scheme_rename_transformer_id(SCHEME_PTR_VAL(v2));
        id = name2;

        if (_implicit_mod_phase) mod_phase = *_implicit_mod_phase;
        mod = scheme_stx_module_name(NULL, &id, phase, 
                                     _implicit_nominal_mod, _implicit_nominal_name,
                                     &mod_phase, 
                                     NULL, NULL, NULL, NULL, &rename_insp, NULL);
        if (_implicit_mod_phase) *_implicit_mod_phase = mod_phase;
          
        if (mod && SAME_TYPE(SCHEME_TYPE(mod), scheme_module_index_type)) {
          if (SCHEME_FALSEP(((Scheme_Modidx *)mod)->path)) {
            /* keep looking locally */
            name = name2;
            SCHEME_USE_FUEL(1);
          } else {
            /* free-id=? equivalence to a name that is not necessarily imported explicitly. */
            /* Note that we're dropping `rename_insp'. It's possible that `rename_insp' provides
               more access than a context where the export htat we're recording is eventually
               imported; in that case, a non-free=id? rename transformer might still be able
               to acces the binding, since it doesn't lose track of `rename_insp'. But re-exporting
               a protected binding with less protection is a bad idea, and tracking
               `rename_insp' is coplicated --- too much work to support a bad idea. */
            if (_implicit_src) {
              *_implicit_src = mod;
              *_implicit_src_name = id;
              name2 = scheme_stx_property(name2, nominal_id_symbol, NULL);
              if (SCHEME_SYMBOLP(name2))
                *_implicit_nominal_name = name2;
              *_implicit = 1;
            }
            break;
          }
        } else
          break;
      } else
        break;
    } else
      break;
  }

  return name;
}

static int lookup(Scheme_Env *genv, Scheme_Object *phase, int as_syntax, const char *name)
{
  int p;

  if (SCHEME_FALSEP(phase))
    return 0;

  p = SCHEME_INT_VAL(phase);
  while (p--) {
    genv = genv->exp_env;
    if (!genv) return 0;
  }
  
  return !!scheme_lookup_in_table((as_syntax ? genv->syntax : genv->toplevel), (const char *)name);
}

void compute_provide_arrays(Scheme_Hash_Table *all_provided, Scheme_Hash_Table *tables,
                            Scheme_Module_Exports *me,
                            Scheme_Env *genv,
                            Scheme_Object *form,
                            int num_phases, Scheme_Module_Export_Info **exp_infos)
{
  int i, count, z, implicit;
  Scheme_Object **exs, **exsns, **exss, **exsnoms, *phase;
  Scheme_Hash_Table *provided, *required;
  char *exps;
  int *exets;
  int excount, exvcount;
  Scheme_Module_Phase_Exports *pt;
  Scheme_Object *implicit_src, *implicit_src_name, *implicit_mod_phase;
  Scheme_Object *implicit_nominal_name, *implicit_nominal_mod;

  for (z = 0; z < all_provided->size; z++) {
    provided = (Scheme_Hash_Table *)all_provided->vals[z];

    if (provided) {
      phase = all_provided->keys[z];
      required = get_required_from_tables(tables, phase);
      if (!required)
        required = scheme_make_hash_table(SCHEME_hash_ptr);
      
      if (SAME_OBJ(phase, scheme_make_integer(0)))
        pt = me->rt;
      else if (SAME_OBJ(phase, scheme_make_integer(1)))
        pt = me->et;
      else if (SAME_OBJ(phase, scheme_false))
        pt = me->dt;
      else {
        pt = MALLOC_ONE_RT(Scheme_Module_Phase_Exports);
        pt->so.type = scheme_module_phase_exports_type;
        pt->phase_index = phase;
        if (!me->other_phases) {
          Scheme_Hash_Table *ht;
          ht = scheme_make_hash_table_equal();
          me->other_phases = ht;
        }
        scheme_hash_set(me->other_phases, phase, (Scheme_Object *)pt);
      }

      for (count = 0, i = provided->size; i--; ) {
        if (provided->vals[i])
          count++;
      }
    
      exs = MALLOC_N(Scheme_Object *, count);
      exsns = MALLOC_N(Scheme_Object *, count);
      exss = MALLOC_N(Scheme_Object *, count);
      exsnoms = MALLOC_N(Scheme_Object *, count);
      exps = MALLOC_N_ATOMIC(char, count);
      exets = MALLOC_N_ATOMIC(int, count);
      memset(exets, 0, count * sizeof(int));

      /* Do non-syntax first. */
      for (count = 0, i = provided->size; i--; ) {
        if (provided->vals[i]) {
          Scheme_Object *name, *prnt_name, *v;
          int protected;
	
          v = provided->vals[i]; /* external name */
          name = SCHEME_CAR(v);  /* internal name (maybe already a symbol) */
          protected = SCHEME_TRUEP(SCHEME_CDR(v));
          prnt_name = name;

          name = extract_free_id_name(name, phase, genv, 1, &implicit, 
                                      NULL, NULL, NULL, 
                                      NULL, NULL);

          if (!implicit
              && genv 
              && lookup(genv, phase, 0, (const char *)name)) {
            /* Defined locally */
            exs[count] = provided->keys[i];
            exsns[count] = name;
            exss[count] = scheme_false; /* means "self" */
            exsnoms[count] = scheme_null; /* since "self" */
            exps[count] = protected;
            exets[count] = SCHEME_INT_VAL(phase);
            count++;
          } else if (!implicit
                     && genv 
                     && lookup(genv, phase, 1, (const char *)name)) {
            /* Skip syntax for now. */
          } else if (implicit) {
            /* Rename-transformer redirect; skip for now. */
          } else if ((v = scheme_hash_get(required, name))) {
            /* Required */
            if (protected) {
              name = SCHEME_CAR(provided->vals[i]);
              scheme_wrong_syntax("module", NULL, name, "cannot protect imported identifier with re-provide"); 
            }
            if (SCHEME_TRUEP(SCHEME_VEC_ELS(v)[3])) {
              Scheme_Object *noms;
              exs[count] = provided->keys[i];
              exsns[count] = SCHEME_VEC_ELS(v)[2];
              exss[count] = SCHEME_VEC_ELS(v)[1];
              noms = adjust_for_rename(exs[count], SCHEME_VEC_ELS(v)[4], SCHEME_VEC_ELS(v)[0]);
              exsnoms[count] = noms;
              exps[count] = protected;
              exets[count] = SCHEME_INT_VAL(SCHEME_VEC_ELS(v)[8]);

              count++;
            }
          } else {
            /* Not defined! */
            char buf[32], *phase_expl;
            if (phase) {
              if (SCHEME_FALSEP(phase)) {
                phase_expl = " for-label";
              } else {
                sprintf(buf, " for phase %" PRIdPTR, SCHEME_INT_VAL(phase));
                phase_expl = scheme_strdup(buf);
              }
            } else
              phase_expl = "";
            scheme_wrong_syntax("module", prnt_name, form, 
                                "provided identifier not defined or imported%s",
                                phase_expl);
          }
        }
      }

      exvcount = count;

      for (i = provided->size; i--; ) {
        if (provided->vals[i]) {
          Scheme_Object *name, *v;
          int protected;
	  
          v = provided->vals[i];
          name = SCHEME_CAR(v); /* internal name (maybe already a symbol) */
          protected = SCHEME_TRUEP(SCHEME_CDR(v));

          name = extract_free_id_name(name, phase, genv, 0, &implicit,
                                      &implicit_src, &implicit_src_name, 
                                      &implicit_mod_phase,
                                      &implicit_nominal_name, &implicit_nominal_mod);

          if (!implicit
              && genv 
              && lookup(genv, phase, 1, (const char *)name)) {
            /* Defined locally */
            exs[count] = provided->keys[i];
            exsns[count] = name;
            exss[count] = scheme_false; /* means "self" */
            exsnoms[count] = scheme_null; /* since "self" */
            exps[count] = protected;
            exets[count] = SCHEME_INT_VAL(phase);
            count++;
          } else if (implicit) {
            /* We record all free-id=?-based exports as syntax, even though they may be values. */
            Scheme_Object *noms;
            exs[count] = provided->keys[i];
            exsns[count] = implicit_src_name;
            exss[count] = implicit_src;
            noms = adjust_for_rename(exs[count], implicit_nominal_name, cons(implicit_nominal_mod, scheme_null));
            exsnoms[count] = noms;
            exps[count] = protected;
            count++;
          } else if ((v = scheme_hash_get(required, name))) {
            /* Required */
            if (SCHEME_FALSEP(SCHEME_VEC_ELS(v)[3])) {
              Scheme_Object *noms;
              exs[count] = provided->keys[i];
              exsns[count] = SCHEME_VEC_ELS(v)[2];
              exss[count] = SCHEME_VEC_ELS(v)[1];
              noms = adjust_for_rename(exs[count], SCHEME_VEC_ELS(v)[4], SCHEME_VEC_ELS(v)[0]);
              exsnoms[count] = noms;
              exps[count] = protected;
              exets[count] = SCHEME_INT_VAL(SCHEME_VEC_ELS(v)[8]);
              count++;
            }
          }
        }
      }

      excount = count;

      /* Discard exsnom[n]s if there are no re-exports */
      for (i = 0; i < excount; i++) {
        if (!SCHEME_NULLP(exsnoms[i]))
          break;
      }
      if (i >= excount) {
        exsnoms = NULL;
      }

      /* Discard exets if all 0 */
      if (exets) {
        for (i = 0; i < excount; i++) {
          if (exets[i])
            break;
        }
        if (i >= excount)
          exets = NULL;
      }

      /* Sort provide array for variables: interned followed by
         uninterned, alphabetical within each. This is important for
         having a consistent provide arrays. */
      qsort_provides(exs, exsns, exss, exps, exets, exsnoms, 0, exvcount, 1);

      pt->num_provides = excount;
      pt->num_var_provides = exvcount;
      pt->provides = exs;
      pt->provide_src_names = exsns;
      pt->provide_srcs = exss;
      pt->provide_nominal_srcs = exsnoms;
      pt->provide_src_phases = exets;

      /* Discard exps if all 0 */
      if (exps) {
        for (i = 0; i < excount; i++) {
          if (exps[i])
            break;
        }
        if (i >= excount)
          exps = NULL;
      }

      if (exps) {
        if (SCHEME_TRUEP(phase)) {
          if ((SCHEME_INT_VAL(phase) < 0)
              || (SCHEME_INT_VAL(phase) >= num_phases))
            scheme_signal_error("internal error: bad phase for exports");
          exp_infos[SCHEME_INT_VAL(phase)]->provide_protects = exps;
        }
      }
    }
  }
}

/* Helper: */
static void qsort_provides(Scheme_Object **exs, Scheme_Object **exsns, Scheme_Object **exss, 
                           char *exps, int *exets,
			   Scheme_Object **exsnoms,
                           int start, int count, int do_uninterned)
{
  int i, j;
  Scheme_Object *tmp_ex, *tmp_exsn, *tmp_exs, *tmp_exsnom, *pivot;
  char tmp_exp;
  int tmp_exet;

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
        if (exsnoms) {
	  tmp_exsnom = exsnoms[i];

	  exsnoms[i] = exsnoms[j];

	  exsnoms[j] = tmp_exsnom;    
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
    qsort_provides(exs, exsns, exss, exps, exets, exsnoms, 0, j + 1, 0);
    qsort_provides(exs, exsns, exss, exps, exets, exsnoms, j + 1, count - j - 1, 0);
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
          if (exsnoms) {
            tmp_exsnom = exsnoms[k];
            
            exsnoms[k] = exsnoms[j];
            
            exsnoms[j] = tmp_exsnom;    
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
      qsort_provides(exs, exsns, exss, exps, exets, exsnoms, start, j - start, 0);
      qsort_provides(exs, exsns, exss, exps, exets, exsnoms, j, count - (j - start), 0);
    }
  }
}

static Scheme_Object *expand_provide(Scheme_Object *e, int at_phase,
                                     Scheme_Hash_Table *tables,
                                     Scheme_Hash_Tree *all_defs,
                                     Scheme_Comp_Env *cenv, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Expand_Info erec1;
  Scheme_Thread *p;
  Scheme_Object *b, *stop;
  Scheme_Comp_Env *xenv;
  mz_jmp_buf newbuf, * volatile savebuf;
  
  xenv = scheme_new_compilation_frame(0, (SCHEME_CAPTURE_WITHOUT_RENAME 
					  | SCHEME_FOR_STOPS), 
				      cenv);
  stop = scheme_get_stop_expander();
  scheme_add_local_syntax(1, xenv);
  if (!at_phase)
    scheme_set_local_syntax(0, scheme_begin_stx, stop, xenv);
  else
    scheme_set_local_syntax(0, scheme_datum_to_syntax(scheme_intern_symbol("begin"), 
                                                      scheme_false, 
                                                      scheme_sys_wraps_phase_worker(at_phase), 
                                                      0, 0), 
                            stop, xenv);

  scheme_init_expand_recs(rec, drec, &erec1, 1);
  erec1.value_name = scheme_false;
  erec1.depth = -1;

  p = scheme_current_thread;

  b = scheme_make_pair((Scheme_Object *)tables, (Scheme_Object *)all_defs);
  p->current_local_bindings = b;
  
  savebuf = p->error_buf;
  p->error_buf = &newbuf;

  if (scheme_setjmp(newbuf)) {
    Scheme_Thread *p2;
    p2 = scheme_current_thread;
    p2->current_local_bindings = NULL;
    p2->error_buf = savebuf;
    scheme_longjmp(*savebuf, 1);    
    return NULL;
  } else {  
    e = scheme_expand_expr(e, xenv, &erec1, 0);
  
    p = scheme_current_thread;
    p->current_local_bindings = NULL;
    p->error_buf = savebuf;
    
    return e;
  }
}

void parse_provides(Scheme_Object *form, Scheme_Object *fst, Scheme_Object *e, 
                    int at_phase,
                    Scheme_Hash_Table *all_provided,
                    Scheme_Hash_Table *all_reprovided,
                    Scheme_Object *self_modidx, 
                    Scheme_Hash_Table *all_defs_out,
                    Scheme_Hash_Table *tables,
                    Scheme_Hash_Tree *all_defs,
                    Scheme_Comp_Env *cenv, Scheme_Compile_Info *rec, int drec,
                    Scheme_Object **_expanded)
{
  Scheme_Object *l, *rebuilt = scheme_null, *protect_stx = NULL;
  int protect_cnt = 0, mode_cnt = 0, expanded = 0;
  Scheme_Object *mode = scheme_make_integer(0), *mode_stx = NULL;
  Scheme_Object *all_x_defs_out, *all_x_defs;
  Scheme_Hash_Table *provided;
  Scheme_Object *phase;

  if (scheme_stx_proper_list_length(e) < 0)
    scheme_wrong_syntax(NULL, e, form, IMPROPER_LIST_FORM);
  
  for (l = SCHEME_STX_CDR(e); !SCHEME_STX_NULLP(l); l = SCHEME_STX_CDR(l)) {
    Scheme_Object *a, *midx, *name, *av;

    a = SCHEME_STX_CAR(l);

    while (1) {
      if (SCHEME_STX_PAIRP(a) && (scheme_stx_proper_list_length(a) > 0)) {
        fst = SCHEME_STX_CAR(a);
        if (SCHEME_STX_SYMBOLP(fst))
          av = SCHEME_STX_VAL(fst);
        else
          av = NULL;
        if (SAME_OBJ(protect_symbol, av)) {
          if (protect_cnt)
            scheme_wrong_syntax(NULL, a, e, "nested `protect' not allowed");
          protect_stx = a;
          a = SCHEME_STX_CDR(a);
          a = scheme_flatten_syntax_list(a, NULL);
          l = SCHEME_STX_CDR(l);
          l = scheme_append(a, l);
          protect_cnt = scheme_list_length(a);

          if (protect_cnt != 1)
            expanded = 1;
        
          /* In case a provide ends with an empty protect: */
          if (SCHEME_STX_NULLP(l))
            break;

          a = SCHEME_STX_CAR(l);
        } else if (SAME_OBJ(av, for_syntax_symbol)
                   || SAME_OBJ(av, for_label_symbol)
                   || SAME_OBJ(av, for_meta_symbol)) {
          if (mode_cnt)
            scheme_wrong_syntax(NULL, a, e, 
                                (SAME_OBJ(av, for_syntax_symbol)
                                 ? "nested `for-syntax' not allowed"
                                 : (SAME_OBJ(av, for_label_symbol)
                                    ? "nested `for-label' not allowed"
                                    : "nested `for-meta' not allowed")));
          
          mode_stx = a;
          a = SCHEME_STX_CDR(a);
          a = scheme_flatten_syntax_list(a, NULL);
          if (SAME_OBJ(av, for_meta_symbol)) {
            if (SCHEME_NULLP(a)) {
              scheme_wrong_syntax(NULL, mode_stx, e, "missing `for-meta' phase");
            }
            mode = SCHEME_CAR(a);
            mode = SCHEME_STX_VAL(mode);
            if (!SCHEME_FALSEP(mode)
                && !SCHEME_INTP(mode)
                && !SCHEME_BIGNUMP(mode)) {
              scheme_wrong_syntax(NULL, mode_stx, e, "bad `for-meta' phase");
            }
            a = SCHEME_CDR(a);
          } else if (SAME_OBJ(av, for_syntax_symbol))
            mode = scheme_make_integer(1);
          else if (SAME_OBJ(av, for_label_symbol))
            mode = scheme_false;
          l = SCHEME_STX_CDR(l);
          l = scheme_append(a, l);
          mode_cnt = scheme_list_length(a);
          if (protect_cnt)
            protect_cnt += (mode_cnt - 1);;
          a = SCHEME_STX_CAR(l);
        } else
          break;
      } else
        break;
    }

    if (SCHEME_FALSEP(mode))
      phase = mode;
    else
      phase = scheme_bin_plus(mode, scheme_make_integer(at_phase));

    all_x_defs_out = scheme_hash_get(all_defs_out, phase);
    if (!all_x_defs_out) all_x_defs_out = scheme_null;

    provided = (Scheme_Hash_Table *)scheme_hash_get(all_provided, phase);
    if (!provided) {
      provided = scheme_make_hash_table(SCHEME_hash_ptr);
      scheme_hash_set(all_provided, phase, (Scheme_Object *)provided);
    }

    if (SCHEME_STX_SYMBOLP(a)) {
      /* <id> */
      name = SCHEME_STX_VAL(a);
      check_already_provided(provided, name, a, protect_cnt, form, phase);
      /* Provide a: */
      scheme_hash_set(provided, name, scheme_make_pair(a, protect_cnt ? scheme_true : scheme_false));
    } else if (SCHEME_STX_PAIRP(a)) {
      Scheme_Object *rest;

      fst = SCHEME_STX_CAR(a);
      rest = SCHEME_STX_CDR(a);

      if (SAME_OBJ(expand_symbol, SCHEME_STX_VAL(fst))) {
        Scheme_Object *p;
        int islist;

        if (SCHEME_STX_PAIRP(rest)) {
          p = SCHEME_STX_CAR(rest);
          rest = SCHEME_STX_CDR(rest);
          if (!SCHEME_STX_NULLP(rest))
            scheme_wrong_syntax(NULL, a, e, "extra forms after one to expand");
        } else {
          scheme_wrong_syntax(NULL, a, e, "missing form to expand");
          return;
        }

        all_x_defs = scheme_hash_tree_get(all_defs, mode);
        if (!all_x_defs) all_x_defs = scheme_null;
        p = expand_provide(p, at_phase, tables, all_defs, cenv, rec, drec);

        /* Check for '(begin datum ...) result: */
        p = scheme_flatten_syntax_list(p, &islist);
        if (!islist)
          p = NULL;
        else if (SCHEME_NULLP(p))
          p = NULL;
        else {
          rest = SCHEME_CAR(p);
          if (!SCHEME_STX_SYMBOLP(rest)
              || !scheme_stx_module_eq_x(scheme_begin_stx, rest, at_phase)) {
            p = NULL;
          }
        }
        
        if (!p) {
          scheme_wrong_syntax(NULL, a, e, "expansion was not a `begin' sequence");
          return;
        }

        p = SCHEME_CDR(p);
        l = SCHEME_STX_CDR(l);
        l = scheme_make_pair(scheme_false, scheme_append(p, l));
        
        if (protect_cnt) {
          protect_cnt += scheme_stx_proper_list_length(p);
        }
        if (mode_cnt) {
          mode_cnt += scheme_stx_proper_list_length(p);
        }

        expanded = 1;
      } else if (SAME_OBJ(rename_symbol, SCHEME_STX_VAL(fst))) {
        /* (rename <id> <id>) */
        Scheme_Object *inm, *enm;

        if (!SCHEME_STX_PAIRP(rest)
            || !SCHEME_STX_PAIRP(SCHEME_STX_CDR(rest)))
          scheme_wrong_syntax(NULL, a, e, "bad syntax");
        inm = SCHEME_STX_CAR(rest);
        rest = SCHEME_STX_CDR(rest);
        enm = SCHEME_STX_CAR(rest);
        if (!SCHEME_STX_SYMBOLP(inm))
          scheme_wrong_syntax(NULL, a, e, "internal name is not an identifier");
        if (!SCHEME_STX_SYMBOLP(enm))
          scheme_wrong_syntax(NULL, a, e, "external name is not an identifier");
        rest = SCHEME_STX_CDR(rest);
        if (!SCHEME_STX_NULLP(rest))
          scheme_wrong_syntax(NULL, a, e, "data following external name");
		
        enm = SCHEME_STX_VAL(enm);
		
        check_already_provided(provided, enm, inm, protect_cnt, a, phase);
        /* Provide enm: */
        scheme_hash_set(provided, enm, scheme_make_pair(inm, protect_cnt ? scheme_true : scheme_false));
      } else if (SAME_OBJ(all_from_symbol, SCHEME_STX_VAL(fst))) {
        /* (all-from <modname>) */
        Scheme_Object *reprovided;
        
        if (protect_cnt)
          scheme_wrong_syntax(NULL, a, e, "not allowed as protected");
        if (!SCHEME_STX_PAIRP(rest))
          scheme_wrong_syntax(NULL, a, e, "bad syntax");
        if (!SCHEME_STX_NULLP(SCHEME_STX_CDR(rest)))
          scheme_wrong_syntax(NULL, a, e, "data following `all-from'");
		
        midx = SCHEME_STX_CAR(rest);
        midx = scheme_make_modidx(scheme_syntax_to_datum(midx, 0, NULL),
                                  self_modidx,
                                  scheme_false);
	
        reprovided = scheme_hash_get(all_reprovided, mode);
        if (!reprovided)
          reprovided = scheme_null;
            
        reprovided = scheme_make_pair(scheme_make_pair(midx, scheme_make_pair(e, scheme_null)), 
                                      reprovided);

        scheme_hash_set(all_reprovided, mode, reprovided);
      } else if (SAME_OBJ(all_from_except_symbol, SCHEME_STX_VAL(fst))) {
        /* (all-from-except <modname> <id> ...) */
        Scheme_Object *reprovided;
        Scheme_Object *exns, *el, *p;
        int len;
		
        if (protect_cnt)
          scheme_wrong_syntax(NULL, a, e, "not allowed as protected");
		
        len = scheme_stx_proper_list_length(a);

        if (len < 0)
          scheme_wrong_syntax(NULL, a, e, IMPROPER_LIST_FORM);
        else if (len == 1)
          scheme_wrong_syntax(NULL, a, e, "missing module name");
		
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
                                "excluded name is not an identifier");
          }
        }
		
        reprovided = scheme_hash_get(all_reprovided, mode);
        if (!reprovided)
          reprovided = scheme_null;
        
        reprovided = scheme_make_pair(scheme_make_pair(midx, scheme_make_pair(e, exns)), 
                                      reprovided);

        scheme_hash_set(all_reprovided, mode, reprovided);
      } else if (SAME_OBJ(struct_symbol, SCHEME_STX_VAL(fst))) {
        /* (struct <id> (<id> ...)) */
        int len, i;
        Scheme_Object *prnt_base, *base, *fields, *el, **names, *p;
		
        len = scheme_stx_proper_list_length(rest);
        if (len != 2) {
          if (len < 0)
            scheme_wrong_syntax(NULL, a, e, IMPROPER_LIST_FORM);
          else
            scheme_wrong_syntax(NULL, a, e,
                                "not a struct identifier followed by "
                                "a sequence of field identifiers");
        }

        base = SCHEME_STX_CAR(rest);
        fields = SCHEME_STX_CDR(rest);
        fields = SCHEME_STX_CAR(fields);
		
        if (!SCHEME_STX_SYMBOLP(base))
          scheme_wrong_syntax(NULL, base, e,
                              "struct name is not an identifier");

        /* Check all field names are identifiers: */
        for (el = fields; SCHEME_STX_PAIRP(el); el = SCHEME_STX_CDR(el)) {
          p = SCHEME_STX_CAR(el);
          if (!SCHEME_STX_SYMBOLP(p)) {
            scheme_wrong_syntax(NULL, p, e,
                                "field name is not an identifier");
          }
        }
        if (!SCHEME_STX_NULLP(el))
          scheme_wrong_syntax(NULL, fields, e, IMPROPER_LIST_FORM);
		
        prnt_base = base;
        base = SCHEME_STX_VAL(base);
        fields = scheme_syntax_to_datum(fields, 0, NULL);

        names = scheme_make_struct_names(base, fields, SCHEME_STRUCT_EXPTIME, &len);

        for (i = 0; i < len; i++) {
          /* Wrap local name with prnt_base in case there are marks that 
             trigger "gensym"ing */
          p = scheme_datum_to_syntax(names[i], scheme_false, prnt_base, 0, 0);
          check_already_provided(provided, names[i], p, protect_cnt, e, phase);
          scheme_hash_set(provided, names[i], 
                          scheme_make_pair(p, protect_cnt ? scheme_true : scheme_false));
        }
      } else if (SAME_OBJ(all_defined_symbol, SCHEME_STX_VAL(fst))) {
        /* (all-defined) */
        if (!SCHEME_STX_NULLP(rest))
          scheme_wrong_syntax(NULL, a, e, "bad syntax");
        
        if (!all_x_defs_out) {
          scheme_wrong_syntax(NULL, a, e, "no definitions at phase level %V",
                              mode);
        }

        all_x_defs_out = scheme_make_pair(scheme_make_pair(scheme_make_pair(e, 
                                                                            scheme_make_pair(scheme_null, 
                                                                                             scheme_false)),
                                                           protect_cnt ? scheme_true : scheme_false),
                                          all_x_defs_out);
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
                              "prefix is not an identifier");
        }
        prefix = SCHEME_STX_VAL(prefix);

        if (!all_x_defs_out) {
          scheme_wrong_syntax(NULL, a, e, "no definitions at phase level %V",
                              mode);
        }
        
        all_x_defs_out = scheme_make_pair(scheme_make_pair(scheme_make_pair(e, 
                                                                            scheme_make_pair(scheme_null, 
                                                                                             prefix)),
                                                           protect_cnt ? scheme_true : scheme_false),
                                          all_x_defs_out);
      } else if (SAME_OBJ(all_defined_except_symbol, SCHEME_STX_VAL(fst))
                 || SAME_OBJ(prefix_all_defined_except_symbol, SCHEME_STX_VAL(fst))) {
        /* ([prefix-]all-defined-except <id> ...) */
        Scheme_Object *exns, *el, *prefix = scheme_false, *p;
        int len, is_prefix;
		
        is_prefix = SAME_OBJ(prefix_all_defined_except_symbol, SCHEME_STX_VAL(fst));

        len = scheme_stx_proper_list_length(a);

        if (len < 0)
          scheme_wrong_syntax(NULL, a, e, IMPROPER_LIST_FORM);
		
        if (is_prefix && (len < 2))
          scheme_wrong_syntax(NULL, a, e, "missing prefix");

        if (is_prefix) {
          prefix = SCHEME_STX_CAR(rest);
          if (!SCHEME_STX_SYMBOLP(prefix))
            scheme_wrong_syntax(NULL, a, e, "prefix is not an identifier");
          prefix = SCHEME_STX_VAL(prefix);
          rest = SCHEME_STX_CDR(rest);
        }

        exns = rest;
		
        /* Check all exclusions are identifiers: */
        for (el = exns; SCHEME_STX_PAIRP(el); el = SCHEME_STX_CDR(el)) {
          p = SCHEME_STX_CAR(el);
          if (!SCHEME_STX_SYMBOLP(p)) {
            scheme_wrong_syntax(NULL, p, e,
                                "excluded name is not an identifier");
          }
        }
		
        if (!all_x_defs_out) {
          scheme_wrong_syntax(NULL, a, e, "no definitions at phase level %V",
                              mode);
        }
        
        all_x_defs_out = scheme_make_pair(scheme_make_pair(scheme_make_pair(e, 
                                                                            scheme_make_pair(exns,
                                                                                             prefix)),
                                                           protect_cnt ? scheme_true : scheme_false),
                                          all_x_defs_out);
      } else {
        scheme_wrong_syntax(NULL, a, e, NULL);
      }
    } else {
      scheme_wrong_syntax(NULL, a, e, NULL);
    }
    
    a = SCHEME_STX_CAR(l);
    if (SCHEME_TRUEP(a)) {
      if (protect_cnt) {
        Scheme_Object *f;
        f = SCHEME_STX_CAR(protect_stx);
        a = scheme_make_pair(f, scheme_make_pair(a, scheme_null));
        a = scheme_datum_to_syntax(a, protect_stx, protect_stx, 0, 0);
      }
      if (!SAME_OBJ(mode, scheme_make_integer(0))) {
        Scheme_Object *f;
        f = SCHEME_STX_CDR(mode_stx);
        f = SCHEME_STX_CAR(f);
        a = scheme_make_pair(for_meta_symbol, 
                             scheme_make_pair(f, 
                                              scheme_make_pair(a, scheme_null)));
        a = scheme_datum_to_syntax(a, mode_stx, mode_stx, 0, 0);
      }
      rebuilt = scheme_make_pair(a, rebuilt);
    }

    if (protect_cnt)
      --protect_cnt;

    if (all_x_defs_out)
      scheme_hash_set(all_defs_out, mode, all_x_defs_out);

    if (mode_cnt) {
      --mode_cnt;
      if (!mode_cnt)
        mode = scheme_make_integer(0);
    }
  }

  if (_expanded) {
    if (expanded) {
      Scheme_Object *a;
      a = SCHEME_STX_CAR(e);
      rebuilt = scheme_make_pair(a, scheme_reverse(rebuilt));
      rebuilt = scheme_datum_to_syntax(rebuilt, e, e, 0, 2);
      *_expanded = rebuilt;
    } else {
      *_expanded = e;
    }
  }
}

static int check_in_hash(Scheme_Object *mp, Scheme_Object *data)
{
  Scheme_Object *v;
  v = scheme_hash_get((Scheme_Hash_Table *)data, mp);
  return v && SAME_OBJ(v, scheme_true);
}

static int check_is_submodule(Scheme_Object *modname, Scheme_Object *_genv)
{
  Scheme_Env *genv = (Scheme_Env *)_genv;
  Scheme_Object *l, *n;

  if (genv->module) {
    l = genv->module->pre_submodule_names;
    if (!l)
      l = genv->module->pre_submodules;
    if (l) {
      while (!SCHEME_NULLP(l)) {
        n = SCHEME_CAR(l);
        if (SCHEME_SYMBOLP(n)) {
          if (SAME_OBJ(n, modname))
            return 1;
        } else {
          n = scheme_resolved_module_path_value(((Scheme_Module *)n)->modname);
          while (SCHEME_PAIRP(SCHEME_CDR(n))) {
            n = SCHEME_CDR(n);
          }
          n = SCHEME_CAR(n);
          if (SAME_OBJ(n, modname))
            return 1;
        }
        l = SCHEME_CDR(l);
      }
    }
  }
  
  return 0;
}

static Scheme_Object *convert_submodule_path(Scheme_Object *name, 
                                             Convert_Submodule_Proc check,
                                             Scheme_Object *check_data) 
{
  Scheme_Object *mp, *v;
          
  if (SAME_OBJ(SCHEME_CAR(name), submod_symbol)
      && SCHEME_PAIRP(SCHEME_CDR(name))
      && SCHEME_PAIRP(SCHEME_CDR(SCHEME_CDR(name)))
      && scheme_is_list(name))
    mp = SCHEME_CADR(name);
  else
    mp = name;

  if (SCHEME_PAIRP(mp)
      && SAME_OBJ(SCHEME_CAR(mp), quote_symbol)
      && SCHEME_PAIRP(SCHEME_CDR(mp))
      && SCHEME_NULLP(SCHEME_CDR(SCHEME_CDR(mp)))) {
    mp = SCHEME_CADR(mp);
    if (check(mp, check_data)) {
      /* convert to `submod' format */
      if (SAME_OBJ(SCHEME_CAR(name), submod_symbol))
        v = SCHEME_CDR(SCHEME_CDR(name));
      else
        v = scheme_null;
      name = scheme_make_pair(submod_symbol,
                              scheme_make_pair(scheme_make_utf8_string("."),
                                               scheme_make_pair(mp, v)));
    }
  }

  return name;
}

Scheme_Object *scheme_module_exported_list(Scheme_Object *modpath, Scheme_Env *genv)
{
  Scheme_Object *modname, *l, *modidx, *stx, *phase, *result;
  Scheme_Module *m;
  int i, j;
  Scheme_Module_Phase_Exports *pt;

  if (SCHEME_STXP(modpath)) {
    stx = modpath;
    modpath = scheme_syntax_to_datum(stx, 0, NULL);
  } else
    stx = NULL;

  modpath = convert_submodule_path(modpath, check_is_submodule, 
                                   (Scheme_Object *)genv);
  
  modidx = scheme_make_modidx(modpath, 
                              (genv->module ? genv->module->self_modidx : scheme_false), 
                              scheme_false);

  modname = _module_resolve(modidx, stx, NULL, 1);

  m = module_load(modname, genv, "syntax-local-module-exports");

  if (!m) {
    /* Can we get here? */
    return scheme_null;
  } else {
    result = scheme_null;

    for (i = -3; i < (m->me->other_phases ? m->me->other_phases->size : 0); i++) {
      l = scheme_null;
      switch (i) {
      case -3:
        pt = m->me->rt;
        phase = scheme_make_integer(0);
        break;
      case -2:
        pt = m->me->et;
        phase = scheme_make_integer(1);
        break;
      case -1:
        pt = m->me->dt;
        phase = scheme_false;
        break;
      default:
        pt = (Scheme_Module_Phase_Exports *)m->me->other_phases->vals[i];
        phase = m->me->other_phases->keys[i];
        break;
      }
      if (pt) {
        for (j = 0; j < pt->num_provides; j++) {
          l = scheme_make_pair(pt->provides[j], l);
        }

        result = scheme_make_pair(scheme_make_pair(phase, l),
                                  result);
      }
    }

    return result;
  }
}

static int expression_starts(Scheme_Object *expr, Scheme_Object *id, int phase)
{
  if (SCHEME_STX_PAIRP(expr)) {
    expr = SCHEME_STX_CAR(expr);
    if (SCHEME_STX_SYMBOLP(expr)) {
      if (scheme_stx_module_eq_x(id, expr, phase))
        return 1;
    }
  }
  
  return 0;
}

static int expression_starts_app(Scheme_Object *expr, Scheme_Object *id, int phase)
{
  if (expression_starts(expr, app_stx, phase)) {
    expr = SCHEME_STX_CDR(expr);
    return expression_starts(expr, id, phase);
  } else if (expression_starts(expr, id, phase)) {
    /* would explicit `#%app' be the core one? */
    id = scheme_datum_to_syntax(SCHEME_STX_VAL(app_stx), expr, expr, 0, 0);
    id = scheme_stx_taint_rearm(id, expr);
    if (scheme_stx_module_eq_x(app_stx, id, phase))
      return 1;
  }

  return 0;
}

static Scheme_Object *expression_app_args(Scheme_Object *expr, int phase)
{
  if (expression_starts(expr, app_stx, phase)) {
    expr = SCHEME_STX_CDR(expr);
    return SCHEME_STX_CDR(expr);
  } else
    return SCHEME_STX_CDR(expr);
}

static int phaseless_literal(Scheme_Object *val)
{
  val = SCHEME_STX_VAL(val);

  if (SCHEME_BOOLP(val)
      || SCHEME_SYMBOLP(val)
      || SCHEME_KEYWORDP(val)
      || SCHEME_NULLP(val)
      || SCHEME_NUMBERP(val)
      || (SCHEME_CHAR_STRINGP(val) && SCHEME_IMMUTABLEP(val))
      || (SCHEME_BYTE_STRINGP(val) && SCHEME_IMMUTABLEP(val)))
    return 1;

  return 0;
}

static int phaseless_constant_expression(Scheme_Object *val, int phase);

static int phaseless_constant_expressions(Scheme_Object *expr,  int phase) 
{
  Scheme_Object *a;

  while (SCHEME_STX_PAIRP(expr)) {
    a = SCHEME_STX_CAR(expr);
    if (!phaseless_constant_expression(a, phase))
      return 0;
    expr = SCHEME_STX_CDR(expr);
  }

  return SCHEME_STX_NULLP(expr);
}

static Scheme_Object *phaseless_constant_expression_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *val = (Scheme_Object *)p->ku.k.p1;

  p->ku.k.p1 = NULL;

  if (phaseless_constant_expression(val, p->ku.k.i1))
    return scheme_true;
  else
    return scheme_false;
}

static int phaseless_constant_expression(Scheme_Object *val, int phase)
{  
# include "mzstkchk.h"
  {
    Scheme_Thread *p = scheme_current_thread;
    p->ku.k.p1 = (void *)val;
    p->ku.k.i1 = phase;
    val = scheme_handle_stack_overflow(phaseless_constant_expression_k);
    return SCHEME_TRUEP(val);
  }

  /* identifier? */
  if (SCHEME_SYMBOLP(SCHEME_STX_VAL(val)))
    return 1;

  if (expression_starts(val, lambda_stx, phase))
    return 1;

  if (expression_starts(val, case_lambda_stx, phase))
    return 1;

  if (expression_starts(val, quote_stx, phase)) {
    val = SCHEME_STX_CDR(val);
    if (SCHEME_STX_PAIRP(val)) {
      val = SCHEME_STX_CAR(val);
      if (phaseless_literal(val))
        return 1;
    }
    return 0;
  } else if (expression_starts(val, datum_stx, phase)) {
    val = SCHEME_STX_CDR(val);
    if (phaseless_literal(val))
      return 1;
    return 0;
  } else if (phaseless_literal(val)) {
    /* would explicit `#%datum' be the core one? */
    Scheme_Object *a;
    a = SCHEME_STX_VAL(datum_stx);
    val = scheme_stx_taint_rearm(scheme_datum_to_syntax(a, val, val, 0, 0), 
                                 val);
    if (scheme_stx_module_eq_x(datum_stx, val, phase))
      return 1;
    return 0;
  }
  
  if (expression_starts_app(val, cons_stx, phase)
      || expression_starts_app(val, list_stx, phase)) {
    val = expression_app_args(val, phase);
    return phaseless_constant_expressions(val, phase);
  }

  return 0;
}

static int expression_string_argument(Scheme_Object *val, int phase)
{
  Scheme_Object *a, *av;

  if (SCHEME_STX_PAIRP(val)) {
    a = SCHEME_STX_CAR(val);
    val = SCHEME_STX_CDR(val);
    if (SCHEME_STX_NULLP(val)) {
      av = SCHEME_STX_VAL(a);
      if (SCHEME_CHAR_STRINGP(av)
          && phaseless_constant_expression(a, phase))
        return 1;
      else if (expression_starts(a, quote_stx, phase)) {
        val = SCHEME_STX_CDR(a);
        if (SCHEME_STX_PAIRP(val)) {
          val = SCHEME_STX_CAR(val);
          a = SCHEME_STX_VAL(val);
          if (SCHEME_CHAR_STRINGP(a))
            return 1;
        }
      }
    }
  }

  return 0;
}
  
static int phaseless_rhs(Scheme_Object *val, int var_count, int phase)
{
  if (var_count == 1) {
    if (phaseless_constant_expression(val, phase))
      return 1;
    else if (expression_starts_app(val, gensym_stx, phase)) {
      val = expression_app_args(val, phase);
      if (SCHEME_STX_NULLP(val))
        return 1;
      else if (expression_string_argument(val, phase))
        return 1;
    } else if (expression_starts_app(val, string_to_uninterned_symbol_stx, phase)) {
      val = expression_app_args(val, phase);
      if (expression_string_argument(val, phase))
        return 1;
    }
  } else if (var_count == 5) {
    if (expression_starts_app(val, make_struct_type_stx, phase)
        && phaseless_constant_expressions(val, phase)) {
      return 1;
    }
  } else if (var_count == 3) {
    if (expression_starts_app(val, make_struct_type_property_stx, phase)
        && phaseless_constant_expressions(val, phase)) {
      return 1;
    }
  }

  return 0;
}

/**********************************************************************/
/*                         top-level require                          */
/**********************************************************************/

void add_single_require(Scheme_Module_Exports *me, /* from module */
                        Scheme_Object *only_phase,
                        Scheme_Object *src_phase_index, /* import from pahse 0 to src_phase_index */
			Scheme_Object *idx, /* from module's idx; may be saved for unmarshalling */
			Scheme_Env *orig_env, /* env for mark_src or copy_vars */
			Scheme_Object *rn_set, /* add requires to renames in this set when no mark_src */
			Scheme_Object *post_ex_rn_set, /* add requires to this rename when mark_src */
                        Scheme_Object *single_rn, /* instead of rn_set */
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
			void *data,
                        Scheme_Object *form, Scheme_Object *err_src, Scheme_Object *cki /* ck args */
			)
{
  int j, var_count;
  Scheme_Object *orig_idx = idx, *to_phase;
  Scheme_Object **exs, **exsns, **exss, *context_marks = scheme_null;
  int *exets;
  int has_context, save_marshal_info = 0;
  Scheme_Object *nominal_modidx, *one_exn, *prnt_iname, *name, *rn, *ename = orig_ename, *bdg;
  Scheme_Hash_Table *orig_onlys;
  int k, skip_rename, do_copy_vars;
  Scheme_Env *name_env;

  if (mark_src) {
    /* Check whether there's context for this import (which
       leads to generated local names). */
    context_marks = scheme_stx_extract_marks(mark_src);
    bdg = scheme_stx_moduleless_env(mark_src);
    has_context = !SCHEME_NULLP(context_marks) || !SCHEME_FALSEP(bdg);
    if (has_context) {
      if (all_simple)
	*all_simple = 0;
    }
  } else {
    has_context = 0; /* computed later */
    bdg = NULL;
  }

  if (iname || ename || onlys || for_unmarshal || unpack_kern)
    can_save_marshal = 0;

  if (onlys)
    orig_onlys = scheme_clone_hash_table(onlys);
  else
    orig_onlys = NULL;
    
  for (k = -3; k < (me->other_phases ? me->other_phases->size : 0); k++) {
    Scheme_Module_Phase_Exports *pt;

    switch(k) {
    case -3:
      pt = me->rt;
      break;
    case -2:
      pt = me->et;
      break;
    case -1:
      pt = me->dt;
      break;
    default:
      pt = (Scheme_Module_Phase_Exports *)me->other_phases->vals[k];
      break;
    }

    if (pt && only_phase) {
      if (!scheme_eqv(pt->phase_index, only_phase))
        pt = NULL;
    }

    name_env = orig_env;
    if (pt) {
      if (SCHEME_FALSEP(pt->phase_index))
        to_phase = scheme_false;
      else if (SCHEME_FALSEP(src_phase_index))
        to_phase = scheme_false;
      else {
        if (orig_env) {
          to_phase = pt->phase_index;
          while (SCHEME_INT_VAL(to_phase) > 0) {
            scheme_prepare_exp_env(name_env);
            name_env = name_env->exp_env;
            to_phase = scheme_bin_minus(to_phase, scheme_make_integer(1));
          }
          while (SCHEME_INT_VAL(to_phase) < 0) {
            scheme_prepare_template_env(name_env);
            name_env = name_env->template_env;
            to_phase = scheme_bin_plus(to_phase, scheme_make_integer(1));
          }
        }
        to_phase = scheme_bin_plus(pt->phase_index, src_phase_index);
      }
    } else
      to_phase = NULL;

    if (pt) {
      one_exn = NULL;
    
      nominal_modidx = idx;

      if (single_rn)
        rn = single_rn;
      else
        rn = scheme_get_module_rename_from_set(((has_context && post_ex_rn_set) ? post_ex_rn_set : rn_set),
                                               to_phase,
                                               1);

      if (copy_vars)
        do_copy_vars = !orig_env->module && !orig_env->phase && SAME_OBJ(src_phase_index, scheme_make_integer(0)) && (k == -3);
      else
        do_copy_vars = 0;

      if (can_save_marshal
          && !exns
          && !prefix
          && !orig_ename
          && pt->num_provides
          && !do_copy_vars) {
        /* Simple "import everything" whose mappings can be shared via the exporting module: */
        if (!pt->src_modidx && me->src_modidx)
          pt->src_modidx = me->src_modidx;
        scheme_extend_module_rename_with_shared(rn, idx, pt, pt->phase_index, src_phase_index, context_marks, bdg, 1);
        skip_rename = 1;
      } else
        skip_rename = 0;

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
            bdg = scheme_stx_moduleless_env(mark_src);
            has_context = !SCHEME_NULLP(l) || !SCHEME_FALSEP(bdg);
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
      
        if (SCHEME_SYM_WEIRDP(exs[j])) {
          /* This shouldn't happen. In case it does, don't import a
             gensym or parallel symbol. The former is useless. The
             latter is supposed to be module-specific, and it could
             collide with local module-specific ids. */
          iname = NULL;
          continue;
        }

        if (!iname)
          iname = exs[j];

        if (prefix)
          iname = scheme_symbol_append(prefix, iname);

        prnt_iname = iname;
        if (has_context) {
          /* The `require' expression has a set of marks in its
             context, which means that we need to generate a name. */
          iname = scheme_datum_to_syntax(iname, scheme_false, mark_src, 0, 0);
          iname = scheme_tl_id_sym(name_env, iname, bdg, skip_rename ? 3 : 2, to_phase, NULL);
          if (all_simple)
            *all_simple = 0;
        }

        if (ck)
          ck(prnt_iname, iname, nominal_modidx, exs[j], modidx, exsns[j], exets ? exets[j] : 0,
             (j < var_count), 
             data, cki, form, err_src, mark_src, to_phase, src_phase_index, pt->phase_index);

        {
          int done;

          if (do_copy_vars && (j < var_count)) {
            Scheme_Env *menv;
            Scheme_Object *val, *modname;
            Scheme_Bucket *b;
            modname = scheme_module_resolve(modidx, 1);
            menv = scheme_module_access(modname, orig_env, 0);
            val = scheme_lookup_in_table(menv->toplevel, (char *)exsns[j]);
            b = scheme_global_bucket(iname, orig_env);
            scheme_set_global_bucket(((copy_vars == 2)
                                      ? "namespace-require/constant"
                                      : "namespace-require/copy"),
                                     b, val, 1);
            if (copy_vars == 2) {
              ((Scheme_Bucket_With_Flags *)b)->flags |= GLOB_IS_IMMUTATED;
              done = 0;
            } else {
              scheme_shadow(orig_env, iname, 1);
              done = 1;
            }
          } else
            done = 0;

          if (done) {
          } else if (!for_unmarshal || !has_context) {
            if (!skip_rename) {
              if (!save_marshal_info && !has_context && can_save_marshal)
                save_marshal_info = 1;

              scheme_extend_module_rename(rn, 
                                          modidx, iname, exsns[j], nominal_modidx, exs[j], 
                                          exets ? exets[j] : 0,
                                          src_phase_index,
                                          pt->phase_index,
                                          (for_unmarshal || (!has_context && can_save_marshal)) ? 1 : 0);
            }
          }
        }

        iname = NULL;
	
        if (ename) {
          ename = NULL;
          break;
        }
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

        /* The format of this data is checked in "syntax.c" for unmarshaling
           a Module_Renames. Also the idx must be first, to support shifting. */
        info = cons(orig_idx, cons(pt->phase_index,
                                   cons(src_phase_index,
                                        cons(exns, prefix ? prefix : scheme_false))));

        scheme_save_module_rename_unmarshal(rn, info);

        save_marshal_info = 0;
      }
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
  Scheme_Object *orig_idx, *exns, *prefix, *idx, *name, *pt_phase, *src_phase_index, *marks, *bdg;
  Scheme_Module_Exports *me;
  Scheme_Env *env;
  int share_all;

  idx = SCHEME_CAR(info);
  orig_idx = idx;
  info = SCHEME_CDR(info);
  pt_phase = SCHEME_CAR(info);
  info = SCHEME_CDR(info);

  if (SCHEME_PAIRP(info) && (SCHEME_PAIRP(SCHEME_CAR(info))
                             || SCHEME_VECTORP(SCHEME_CAR(info)))) {
    marks = SCHEME_CAR(info);
    info = SCHEME_CDR(info);
  } else
    marks = scheme_null;

  if (SCHEME_VECTORP(marks)) {
    bdg = SCHEME_VEC_ELS(marks)[1];
    marks = SCHEME_VEC_ELS(marks)[0];
  } else
    bdg = scheme_false;

  if (SCHEME_INTP(info)
      || SCHEME_FALSEP(info)) {
    share_all = 1;
    src_phase_index = info;
    
    exns = NULL;
    prefix = NULL;
  } else {
    share_all = 0;
    src_phase_index = SCHEME_CAR(info);
    info = SCHEME_CDR(info);
    exns = SCHEME_CAR(info);
    prefix = SCHEME_CDR(info);

    if (SCHEME_FALSEP(prefix))
      prefix = NULL;
    if (SCHEME_NULLP(exns))
      exns = NULL;
  }
    
  if (modidx_shift_from)
    idx = scheme_modidx_shift(idx,
			      modidx_shift_from,
			      modidx_shift_to);

  name = scheme_module_resolve(idx, 0);

  {
    Scheme_Module *mod;
    mod = get_special_module(name);
    if (mod) 
      me = mod->me;
    else
      me = NULL;
  }

  if (!me) {
    if (!export_registry) {
      env = scheme_get_env(scheme_current_config());
      export_registry = env->module_registry->exports;
    }

    me = (Scheme_Module_Exports *)scheme_hash_get(export_registry, name);
    if (!me) {
      scheme_signal_error("compiled/expanded code out of context;"
			  " cannot find exports to restore imported renamings"
			  " for module: %D",
			  name);
      return;
    }
  }

  if (share_all) {
    Scheme_Module_Phase_Exports *pt;

    if (SAME_OBJ(pt_phase, scheme_make_integer(0)))
      pt = me->rt;
    else if (SAME_OBJ(pt_phase, scheme_make_integer(1)))
      pt = me->et;
    else if (SAME_OBJ(pt_phase, scheme_false))
      pt = me->dt;
    else
      pt = (Scheme_Module_Phase_Exports *)scheme_hash_get(me->other_phases, pt_phase);
    
    if (pt) {
      if (!pt->src_modidx && me->src_modidx)
        pt->src_modidx = me->src_modidx;
      scheme_extend_module_rename_with_shared(rn, orig_idx, pt, pt->phase_index, src_phase_index, marks, bdg, 0);
    }
  } else {
    if (!SCHEME_NULLP(marks) || SCHEME_TRUEP(bdg))
      scheme_signal_error("internal error: unexpected marks/bdg");

    add_single_require(me, pt_phase, src_phase_index, orig_idx, NULL,
                       NULL, NULL, rn,
                       exns, NULL, prefix, NULL, NULL,
                       NULL,
                       0, 0, 1, 0,
                       NULL/* _all_simple */,
                       NULL /* ck */, NULL /* data */, 
                       NULL, NULL, NULL);
  }
}

Scheme_Object *scheme_get_kernel_modidx(void)
{
  return kernel_modidx;
}

void parse_requires(Scheme_Object *form, int at_phase,
                    Scheme_Object *base_modidx,
                    Scheme_Env *main_env,
                    Scheme_Module *for_m,
                    Scheme_Object *rn_set, Scheme_Object *post_ex_rn_set,
                    Check_Func ck, void *data,
                    Scheme_Object *redef_modname,
                    int unpack_kern, int copy_vars, int can_save_marshal, 
                    int eval_exp, int eval_run,
                    int *all_simple,
                    Scheme_Hash_Table *modidx_cache,
                    Scheme_Hash_Table *submodule_names,
                    int *non_phaseless)
/* form can be a module-path index or a quoted require spec */
{
  Scheme_Object *ll = form, *mode = scheme_make_integer(0), *just_mode = NULL, *x_mode, *x_just_mode;
  Scheme_Module *m;
  Scheme_Object *idxstx, *idx, *name, *i, *exns, *prefix, *iname, *ename, *aa, *aav;
  Scheme_Object *mark_src, *err_src;
  Scheme_Hash_Table *onlys;
  Scheme_Env *env;
  int skip_one, mode_cnt = 0, just_mode_cnt = 0, is_mpi;

  if (SAME_TYPE(SCHEME_TYPE(form), scheme_module_index_type)) {
    ll = scheme_make_pair(scheme_false, scheme_make_pair(form, scheme_null));
    is_mpi = 1;
  } else {
    if (scheme_stx_proper_list_length(form) < 0)
      scheme_wrong_syntax(NULL, NULL, form, IMPROPER_LIST_FORM);
    is_mpi = 0;
  }
  
  for (ll = SCHEME_STX_CDR(ll); !SCHEME_STX_NULLP(ll); ll = SCHEME_STX_CDR(ll)) {
    i = SCHEME_STX_CAR(ll);
    iname = ename = NULL;
    onlys = NULL;
    if (SCHEME_STX_PAIRP(i)) {
      aa = SCHEME_STX_CAR(i);
      aav = SCHEME_STX_VAL(aa);
    } else {
      aa = NULL;
      aav = NULL;
    }

    err_src = i;
    mark_src = i;
    skip_one = 0;

    if (is_mpi) {
      idxstx = i;
      exns = NULL;
      prefix = NULL;
      mark_src = NULL;
    } else if (SAME_OBJ(for_syntax_symbol, aav)
        || SAME_OBJ(for_template_symbol, aav)
        || SAME_OBJ(for_label_symbol, aav)
        || SAME_OBJ(for_meta_symbol, aav)
        || SAME_OBJ(just_meta_symbol, aav)) {
      if (!SAME_OBJ(just_meta_symbol, aav)) {
        if (mode_cnt)
          scheme_wrong_syntax(NULL, i, form, 
                              (SAME_OBJ(for_syntax_symbol, aav)
                               ? "nested `for-syntax' not allowed"
                               : (SAME_OBJ(for_template_symbol, aav)
                                  ? "nested `for-template' not allowed"
                                  : (SAME_OBJ(for_label_symbol, aav)
                                     ? "nested `for-label' not allowed"
                                     : "nested `for-meta' not allowed"))));
      } else {
        if (just_mode_cnt)
          scheme_wrong_syntax(NULL, i, form, "nested `just-meta' not allowed");
      }
      
      aa = scheme_flatten_syntax_list(i, NULL);
      ll = SCHEME_STX_CDR(ll);
      if (SAME_OBJ(for_meta_symbol, aav)
          || SAME_OBJ(just_meta_symbol, aav)) {
        Scheme_Object *a_mode;
        aa = SCHEME_STX_CDR(aa);
        if (SCHEME_STX_NULLP(aa))
          scheme_wrong_syntax(NULL, i, form, "missing `%s-meta' level specification",
                              (SAME_OBJ(for_meta_symbol, aav) ? "for" : "just"));
        a_mode = SCHEME_STX_CAR(aa);
        a_mode = SCHEME_STX_VAL(a_mode);
        if (!SCHEME_FALSEP(a_mode)
            && !SCHEME_INTP(a_mode)
            && !SCHEME_BIGNUMP(a_mode))
          scheme_wrong_syntax(NULL, i, form, "bad `%s-meta' level specification",
                              (SAME_OBJ(for_meta_symbol, aav) ? "for" : "just"));
        if (SAME_OBJ(for_meta_symbol, aav))  {
          if (SCHEME_FALSEP(a_mode))
            mode = a_mode;
          else
            mode = scheme_bin_plus(a_mode, scheme_make_integer(0));
        } else
          just_mode = a_mode;
      } else {
        if (SAME_OBJ(for_syntax_symbol, aav))
          mode = scheme_make_integer(1);
        else if (SAME_OBJ(for_template_symbol, aav))
          mode = scheme_make_integer(-1);
        else
          mode = scheme_false;
      }
      ll = scheme_append(aa, ll);
      
      if (!SAME_OBJ(just_meta_symbol, aav)) {
        mode_cnt = scheme_list_length(aa);
        if (just_mode_cnt)
          just_mode_cnt += (mode_cnt - 1);
      } else {
        just_mode_cnt = scheme_list_length(aa);
        if (mode_cnt)
          mode_cnt += (just_mode_cnt - 1);
      }

      skip_one = 1;
    } else if (aa && SAME_OBJ(prefix_symbol, SCHEME_STX_VAL(aa))) {
      /* prefix */
      int len;

      if (all_simple)
	*all_simple = 0;

      len = scheme_stx_proper_list_length(i);
      if (len != 3) {
	GC_CAN_IGNORE const char *reason;
	
	if (len < 0)
	  reason = IMPROPER_LIST_FORM;
	else if (len < 2)
	  reason = "prefix missing";
	else if (len < 3)
	  reason = "module name missing";
	else
	  reason = "extra data after module name";
	scheme_wrong_syntax(NULL, i, form, reason);
	return;
      }

      i = SCHEME_STX_CDR(i);
      prefix = SCHEME_STX_CAR(i);
      i = SCHEME_STX_CDR(i);
      idxstx = SCHEME_STX_CAR(i);
      exns = NULL;

      if (!SCHEME_SYMBOLP(SCHEME_STX_VAL(prefix))) {
	scheme_wrong_syntax(NULL, prefix, form, "bad prefix (not an identifier)");
	return;
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
	scheme_wrong_syntax(NULL, i, form, IMPROPER_LIST_FORM);
      else if (has_prefix && (len < 2))
	scheme_wrong_syntax(NULL, i, form, "prefix missing");
      else if (len < (has_prefix ? 3 : 2))
	scheme_wrong_syntax(NULL, i, form, "module name missing");

      idxstx = SCHEME_STX_CDR(i);
      if (has_prefix) {
	prefix = SCHEME_STX_CAR(idxstx);
	idxstx = SCHEME_STX_CDR(idxstx);

	if (!SCHEME_SYMBOLP(SCHEME_STX_VAL(prefix))) {
	  scheme_wrong_syntax(NULL, prefix, form, "prefix is not an identifier");
	  return;
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
			      "excluded name is not an identifier");
	}
      }
      if (SCHEME_STX_NULLP(exns))
        exns = NULL;
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
	  reason = IMPROPER_LIST_FORM;
	else
	  reason = "module name missing";
	scheme_wrong_syntax(NULL, i, form, reason);
	return;
      }

      onlys = scheme_make_hash_table(SCHEME_hash_ptr);

      rest = SCHEME_STX_CDR(i);
      idxstx = SCHEME_STX_CAR(rest);
      rest = SCHEME_STX_CDR(rest);
      while (SCHEME_STX_PAIRP(rest)) {
	nm = SCHEME_STX_CAR(rest);
	if (!SCHEME_STX_SYMBOLP(nm)) {
	  scheme_wrong_syntax(NULL, nm, form, "name for `only' is not an identifier");
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
	  reason = IMPROPER_LIST_FORM;
	else if (len < 2)
	  reason = "module name missing";
	else if (len < 3)
	  reason = "internal name missing";
	else if (len < 4)
	  reason = "external name missing";
	else
	  reason = "extra data after external name";
	scheme_wrong_syntax(NULL, i, form, reason);
	return;
      }

      rest = SCHEME_STX_CDR(i);
      idxstx = SCHEME_STX_CAR(rest);
      rest = SCHEME_STX_CDR(rest);
      iname = SCHEME_STX_CAR(rest);
      rest = SCHEME_STX_CDR(rest);
      ename = SCHEME_STX_CAR(rest);

      if (!SCHEME_STX_SYMBOLP(iname))
	scheme_wrong_syntax(NULL, i, form, "internal name is not an identifier");
      if (!SCHEME_STX_SYMBOLP(ename))
	scheme_wrong_syntax(NULL, i, form, "external name is not an identifier");

      mark_src = iname;

      iname = SCHEME_STX_VAL(iname);
      
      prefix = NULL;
      exns = NULL;
    } else {
      idxstx = i;
      exns = NULL;
      prefix = NULL;
    }

    if (!skip_one) {
      int start = 1;
      Scheme_Env *rename_env;

      if (SCHEME_FALSEP(mode)) {
        start = 0;
        scheme_prepare_label_env(main_env);
        env = main_env->label_env;
        rename_env = main_env;
      } else if (scheme_is_positive(mode)) {
        Scheme_Object *n = mode;
        env = main_env;
        do {
          scheme_prepare_exp_env(env);
          env = env->exp_env;
          n = scheme_bin_minus(n, scheme_make_integer(1));
        } while (scheme_is_positive(n));
        rename_env = env;
      } else if (scheme_is_negative(mode)) {
        Scheme_Object *n = mode;
        env = main_env;
        do {
          scheme_prepare_template_env(env);
          env = env->template_env;
          n = scheme_bin_plus(n, scheme_make_integer(1));
        } while (scheme_is_negative(n));
        rename_env = env;
      } else {
        env = main_env;
        rename_env = env;
      }

      if (is_mpi) {
        idx = form;
      } else {
        name = scheme_syntax_to_datum(idxstx, 0, NULL);

        if (submodule_names && SCHEME_PAIRP(name)) {
          /* check for 'x where x is a submodule name */
          name = convert_submodule_path(name, check_in_hash, 
                                        (Scheme_Object *)submodule_names);
        }

        if (modidx_cache)
          idx = scheme_hash_get(modidx_cache, name);
        else
          idx = NULL;
        if (!idx) {
          if (SCHEME_PAIRP(name) 
              && SAME_OBJ(SCHEME_CAR(name), submod_symbol)
              && SCHEME_PAIRP(SCHEME_CDR(name))
              && SCHEME_PATHP(SCHEME_CADR(name))) {
            idx = scheme_make_modidx(SCHEME_CADR(name), base_modidx, scheme_false);
            idx = scheme_make_modidx(scheme_make_pair(submod_symbol,
                                                      scheme_make_pair(scheme_make_utf8_string("."),
                                                                       SCHEME_CDDR(name))),
                                     idx, 
                                     scheme_false);
          } else
            idx = scheme_make_modidx(name, base_modidx, scheme_false);
          if (modidx_cache)
            scheme_hash_set(modidx_cache, name, idx);
        }
      }

      name = _module_resolve(idx, idxstx, NULL, 1);

      m = module_load(name, env, NULL);

      start_module(m, env, 0, idx, 
                   start ? eval_exp : 0, start ? eval_run : 0, 
                   main_env->phase, scheme_null, 0);

      if (non_phaseless && !m->phaseless)
        *non_phaseless |= NON_PHASELESS_IMPORT;

      x_just_mode = just_mode;
      x_mode = mode;
      if (at_phase) {
        if (x_mode && SCHEME_TRUEP(x_mode)) {
          x_mode = scheme_bin_plus(x_mode, scheme_make_integer(at_phase));
        }
        /* x_just_mode refers to the mode at export, which doesn't shift 
           by phase context at import */
      }

      /* Add name to require list, if it's not there: */
      if (main_env->module) {
        Scheme_Object *reqs;
        if (SAME_OBJ(x_mode, scheme_make_integer(0))) {
          reqs = add_req(scheme_make_pair(idx, scheme_null), main_env->module->requires);
          main_env->module->requires = reqs;
        } else if (SAME_OBJ(x_mode, scheme_make_integer(1))) {
          reqs = add_req(scheme_make_pair(idx, scheme_null), main_env->module->et_requires);
          main_env->module->et_requires = reqs;
        } else if (SAME_OBJ(x_mode, scheme_make_integer(-1))) {
          reqs = add_req(scheme_make_pair(idx, scheme_null), main_env->module->tt_requires);
          main_env->module->tt_requires = reqs;
        } else if (SAME_OBJ(x_mode, scheme_false)) {
          reqs = add_req(scheme_make_pair(idx, scheme_null), main_env->module->dt_requires);
          main_env->module->dt_requires = reqs;
        } else {
          Scheme_Hash_Table *oht;
          oht = main_env->module->other_requires;
          if (!oht) {
            oht = scheme_make_hash_table_equal();
            main_env->module->other_requires = oht;
          }
          reqs = scheme_hash_get(oht, x_mode);
          if (!reqs)
            reqs = scheme_null;
          reqs = add_req(scheme_make_pair(idx, scheme_null), reqs);
          scheme_hash_set(oht, x_mode, reqs);
        }
      }

      add_single_require(m->me, x_just_mode, x_mode, idx, rename_env,
                         rn_set, post_ex_rn_set, NULL,
                         exns, onlys, prefix, iname, ename,
                         mark_src, 
                         unpack_kern, copy_vars, 0, can_save_marshal,
                         all_simple,
                         ck, data,
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

    if (mode_cnt) {
      --mode_cnt;
      if (!mode_cnt)
        mode = scheme_make_integer(0);
    }
    if (just_mode_cnt) {
      --just_mode_cnt;
      if (!just_mode_cnt)
        just_mode = NULL;
    }
  }
}

static void check_dup_require(Scheme_Object *prnt_name, Scheme_Object *name, 
                              Scheme_Object *nominal_modidx, Scheme_Object *nominal_name, 
			      Scheme_Object *modidx, Scheme_Object *srcname, int exet,
			      int isval, void *ht, Scheme_Object *e, Scheme_Object *form, 
                              Scheme_Object *err_src, Scheme_Object *mark_src, 
                              Scheme_Object *to_phase, Scheme_Object *src_phase_index,
                              Scheme_Object *nominal_export_phase)
{
  Scheme_Object *i;

  if (ht) {
    Scheme_Hash_Table *pht;

    pht = (Scheme_Hash_Table *)scheme_hash_get((Scheme_Hash_Table *)ht, to_phase);
    if (!pht) {
      pht = scheme_make_hash_table(SCHEME_hash_ptr);
      scheme_hash_set((Scheme_Hash_Table *)ht, name, (Scheme_Object *)pht);
    }

    i = scheme_hash_get(pht, name);

    if (i) {
      if (same_resolved_modidx(modidx, SCHEME_CAR(i)) && SAME_OBJ(srcname, SCHEME_CDR(i)))
	return; /* same source */
      scheme_wrong_syntax(NULL, prnt_name, form, "duplicate import identifier");
    } else
      scheme_hash_set((Scheme_Hash_Table *)ht, name, scheme_make_pair(modidx, srcname));
  }
}

static Scheme_Object *
do_require_execute(Scheme_Env *env, Scheme_Object *form)
{
  Scheme_Hash_Table *ht;
  Scheme_Object *rn_set, *modidx;
  Scheme_Object *rest, *insp;

  if (env->module)
    modidx = env->module->self_modidx;
  else
    modidx = scheme_false;

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

  scheme_prepare_exp_env(env);
  scheme_prepare_template_env(env);

  insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);

  rn_set = scheme_make_module_rename_set(mzMOD_RENAME_TOPLEVEL, NULL, insp);

  if (rest) {
    ht = scheme_make_hash_table_equal();
  } else {
    ht = NULL;
  }

  parse_requires(form, env->phase, modidx, env, NULL,
                 rn_set, rn_set,
                 check_dup_require, ht,
                 NULL,
                 !env->module, 0, 0, 
                 -1, 1,
                 NULL, NULL, NULL,
                 NULL);

  scheme_append_rename_set_to_env(rn_set, env);

  return scheme_void;
}

Scheme_Object *
scheme_top_level_require_execute(Scheme_Object *data)
{
  do_require_execute(scheme_environment_from_dummy(SCHEME_PTR1_VAL(data)),
                     SCHEME_PTR2_VAL(data));
  return scheme_void;
}

Scheme_Object *
scheme_top_level_require_jit(Scheme_Object *data)
{
  return data;
}

static Scheme_Object *do_require(Scheme_Object *form, Scheme_Comp_Env *env, 
				 Scheme_Compile_Expand_Info *rec, int drec)
{
  Scheme_Hash_Table *ht;
  Scheme_Object *rn_set, *dummy, *modidx, *data, *insp;
  Scheme_Env *genv;

  if (!scheme_is_toplevel(env))
    scheme_wrong_syntax(NULL, NULL, form, "not at top-level or in module body");

  /* If we get here, it must be a top-level require. */

  /* Hash table is for checking duplicate names in require list: */
  ht = scheme_make_hash_table_equal();

  insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);

  rn_set = scheme_make_module_rename_set(mzMOD_RENAME_TOPLEVEL, NULL, insp);

  genv = env->genv;
  scheme_prepare_exp_env(genv);
  scheme_prepare_template_env(genv);

  if (genv->module)
    modidx = genv->module->self_modidx;
  else
    modidx = scheme_false;

  parse_requires(form, genv->phase, modidx, genv, NULL,
                 rn_set, rn_set,
                 check_dup_require, ht,
                 NULL, 
                 0, 0, 0, 
                 1, 0,
                 NULL, NULL, NULL,
                 NULL);

  if (rec && rec[drec].comp) {
    /* Dummy lets us access a top-level environment: */
    dummy = scheme_make_environment_dummy(env);
    
    scheme_compile_rec_done_local(rec, drec);
    scheme_default_compile_rec(rec, drec);

    data = scheme_alloc_object();
    data->type = scheme_require_form_type;
    SCHEME_PTR1_VAL(data) = dummy;
    SCHEME_PTR2_VAL(data) = form;

    return data;
  } else
    return form;
}

static Scheme_Object *
require_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_require(form, env, rec, drec);
}

static Scheme_Object *
require_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_REQUIRE(erec[drec].observer);
  return do_require(form, env, erec, drec);
}

Scheme_Object *scheme_toplevel_require_for_expand(Scheme_Object *module_path, 
                                                  intptr_t phase,
                                                  Scheme_Comp_Env *cenv,
                                                  Scheme_Object *mark)
{
  Scheme_Object *form;

  form = make_require_form(module_path, phase, mark);

  do_require_execute(cenv->genv, form);

  return form;
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

static Scheme_Object *
declare_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  scheme_wrong_syntax(NULL, NULL, form, "not in module body");
  return NULL;
}

static Scheme_Object *
declare_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec)
{
  SCHEME_EXPAND_OBSERVE_PRIM_PROVIDE(erec[drec].observer);
  scheme_wrong_syntax(NULL, NULL, form, "not in module body");
  return NULL;
}
