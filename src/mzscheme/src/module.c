/*
  MzScheme
  Copyright (c) 2004-2010 PLT Scheme Inc.
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
#include "mzrt.h"
#include "schmach.h"
#include "schexpobs.h"

/* globals */
SHARED_OK Scheme_Object *(*scheme_module_demand_hook)(int, Scheme_Object **);

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
static Scheme_Object *dynamic_require_for_syntax(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_require(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_require_copy(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_require_constant(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_require_etonly(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_attach_module(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_unprotect_module(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_name(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_imports(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_exports(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_lang_info(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_to_namespace(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_to_lang_info(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_to_imports(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_to_exports(int argc, Scheme_Object *argv[]);

static Scheme_Object *module_path_index_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_path_index_resolve(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_path_index_split(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_path_index_join(int argc, Scheme_Object *argv[]);

static Scheme_Object *is_module_path(int argc, Scheme_Object **argv);

static Scheme_Object *resolved_module_path_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_resolved_module_path(int argc, Scheme_Object *argv[]);
static Scheme_Object *resolved_module_path_name(int argc, Scheme_Object *argv[]);

static Scheme_Object *module_export_protected_p(int argc, Scheme_Object **argv);

/* syntax */
static Scheme_Object *module_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *module_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *module_begin_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *module_begin_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *require_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *require_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *provide_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *provide_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);

static Scheme_Object *module_execute(Scheme_Object *data);
static Scheme_Object *top_level_require_execute(Scheme_Object *data);

static Scheme_Object *module_jit(Scheme_Object *data);
static Scheme_Object *top_level_require_jit(Scheme_Object *data);

static Scheme_Object *module_optimize(Scheme_Object *data, Optimize_Info *info, int context);
static Scheme_Object *module_resolve(Scheme_Object *data, Resolve_Info *info);
static Scheme_Object *module_sfs(Scheme_Object *data, SFS_Info *info);
static Scheme_Object *top_level_require_optimize(Scheme_Object *data, Optimize_Info *info, int context);
static Scheme_Object *top_level_require_resolve(Scheme_Object *data, Resolve_Info *info);
static Scheme_Object *top_level_require_sfs(Scheme_Object *data, SFS_Info *info);

static void module_validate(Scheme_Object *data, Mz_CPort *port, 
                            char *stack, Validate_TLS tls,
                            int depth, int letlimit, int delta, 
			    int num_toplevels, int num_stxes, int num_lifts,
                            struct Validate_Clearing *vc, int tailpos);
static void top_level_require_validate(Scheme_Object *data, Mz_CPort *port, 
                                       char *stack, Validate_TLS tls,
                                       int depth, int letlimit, int delta, 
				       int num_toplevels, int num_stxes, int num_lifts,
                                       struct Validate_Clearing *vc, int tailpos);

static Scheme_Object *write_module(Scheme_Object *obj);
static Scheme_Object *read_module(Scheme_Object *obj);

static Scheme_Module *module_load(Scheme_Object *modname, Scheme_Env *env, const char *who);

static void run_module(Scheme_Env *menv, int set_ns);
static void run_module_exptime(Scheme_Env *menv, int set_ns);

static void eval_exptime(Scheme_Object *names, int count,
                         Scheme_Object *expr, 
                         Scheme_Env *genv, Scheme_Comp_Env *env,
                         Resolve_Prefix *rp, int let_depth, int shift,
                         Scheme_Bucket_Table *syntax, int for_stx,
                         Scheme_Object *certs,
                         Scheme_Object *free_id_rename_rn);

static Scheme_Module_Exports *make_module_exports();

static Scheme_Object *scheme_sys_wraps_phase_worker(long p);

#define cons scheme_make_pair

/* global read-only kernel stuff */
READ_ONLY static Scheme_Object *kernel_modname;
READ_ONLY static Scheme_Object *kernel_symbol;
READ_ONLY static Scheme_Object *kernel_modidx;
READ_ONLY static Scheme_Module *kernel;
READ_ONLY static Scheme_Object *flfxnum_modname;
READ_ONLY static Scheme_Object *unsafe_modname;

/* global read-only phase wraps */
READ_ONLY static Scheme_Object *scheme_sys_wraps0;
READ_ONLY static Scheme_Object *scheme_sys_wraps1;

/* global read-only symbols */
ROSYM static Scheme_Object *module_symbol;
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
ROSYM static Scheme_Object *module_name_symbol;
ROSYM static Scheme_Object *nominal_id_symbol;

/* global read-only syntax */
READ_ONLY Scheme_Object *scheme_module_stx;
READ_ONLY Scheme_Object *scheme_module_begin_stx;
READ_ONLY Scheme_Object *scheme_begin_stx;
READ_ONLY Scheme_Object *scheme_define_values_stx;
READ_ONLY Scheme_Object *scheme_define_syntaxes_stx;
READ_ONLY Scheme_Object *scheme_top_stx;
READ_ONLY static Scheme_Object *modbeg_syntax;
READ_ONLY static Scheme_Object *define_for_syntaxes_stx;
READ_ONLY static Scheme_Object *require_stx;
READ_ONLY static Scheme_Object *provide_stx;
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

READ_ONLY static Scheme_Object *empty_self_modidx;
READ_ONLY static Scheme_Object *empty_self_modname;

THREAD_LOCAL_DECL(static Scheme_Bucket_Table *starts_table);
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
THREAD_LOCAL_DECL(static Scheme_Bucket_Table *place_local_modpath_table);
#endif

/* FIXME eventually theses initial objects should be shared, but work required */
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

#define SCHEME_MODNAMEP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_resolved_module_path_type)

typedef void (*Check_Func)(Scheme_Object *prnt_name, Scheme_Object *name, 
                           Scheme_Object *nominal_modname, Scheme_Object *nominal_export,
			   Scheme_Object *modname, Scheme_Object *srcname, int exet,
			   int isval, void *data, Scheme_Object *e, Scheme_Object *form, 
                           Scheme_Object *err_src, Scheme_Object *mark_src,
                           Scheme_Object *to_phase, Scheme_Object *src_phase_index,
                           Scheme_Object *nominal_export_phase, Scheme_Object *in_insp);
static void parse_requires(Scheme_Object *form,
                           Scheme_Object *base_modidx,
                           Scheme_Env *env,
                           Scheme_Module *for_m,
                           Scheme_Object *rns, Scheme_Object *post_ex_rns,
                           Check_Func ck, void *data,
                           Scheme_Object *redef_modname,
                           int unpack_kern, int copy_vars, int can_save_marshal, 
                           int eval_exp, int eval_run,
                           int *all_simple);
static void parse_provides(Scheme_Object *form, Scheme_Object *fst, Scheme_Object *e,
                           Scheme_Hash_Table *all_provided,
                           Scheme_Hash_Table *all_reprovided,
                           Scheme_Object *self_modidx,
                           Scheme_Object **_all_defs_out, 
                           Scheme_Object **_et_all_defs_out, 
                           Scheme_Hash_Table *tables,
                           Scheme_Object *all_defs, Scheme_Object *all_et_defs,
                           Scheme_Comp_Env *cenv, Scheme_Compile_Info *rec, int drec,
                           Scheme_Object **_expanded);
static int compute_reprovides(Scheme_Hash_Table *all_provided,
                              Scheme_Hash_Table *all_reprovided, 
                              Scheme_Module *mod_for_requires,
                              Scheme_Hash_Table *tables,
                              Scheme_Env *genv, 
                              Scheme_Object *all_rt_defs, Scheme_Object *all_rt_defs_out, 
                              Scheme_Object *all_et_defs, Scheme_Object *all_et_defs_out, 
                              const char *matching_form,
                              Scheme_Object *all_mods, Scheme_Object *all_phases);
static char *compute_provide_arrays(Scheme_Hash_Table *all_provided, Scheme_Hash_Table *tables,
                                    Scheme_Module_Exports *me,
                                    Scheme_Env *genv,
                                    Scheme_Object *form,
                                    char **_phase1_protects);
static Scheme_Object **compute_indirects(Scheme_Env *genv, 
                                         Scheme_Module_Phase_Exports *pt,
                                         int *_count,
                                         int vars);
static void start_module(Scheme_Module *m, Scheme_Env *env, int restart, Scheme_Object *syntax_idx, 
                         int eval_exp, int eval_run, long base_phase, Scheme_Object *cycle_list);
static void eval_module_body(Scheme_Env *menv, Scheme_Env *env);

static Scheme_Object *do_namespace_require(Scheme_Env *env, int argc, Scheme_Object *argv[], 
                                           int copy, int etonly);

static Scheme_Object *default_module_resolver(int argc, Scheme_Object **argv);

static void qsort_provides(Scheme_Object **exs, Scheme_Object **exsns, Scheme_Object **exss, char *exps, char *exets,
                           Scheme_Object **exsnoms, Scheme_Object **exinsps,
			   int start, int count, int do_uninterned);

#define MODCHAIN_TABLE(p) ((Scheme_Hash_Table *)(SCHEME_VEC_ELS(p)[0]))
#define MODCHAIN_AVAIL(p, n) (SCHEME_VEC_ELS(p)[3+n])

/**********************************************************************/
/*                           initialization                           */
/**********************************************************************/

void scheme_init_module(Scheme_Env *env)
{
  scheme_register_syntax(MODULE_EXPD, 
			 module_optimize,
			 module_resolve, module_sfs, module_validate, 
			 module_execute, module_jit, 
			 NULL, NULL, -1);
  scheme_register_syntax(REQUIRE_EXPD, 
			 top_level_require_optimize,
			 top_level_require_resolve, top_level_require_sfs, top_level_require_validate, 
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
#ifdef MZ_USE_PLACES
    empty_self_modname = scheme_intern_symbol("expanded module"); /* FIXME: needs to be uninterned */
#else
    empty_self_modname = scheme_make_symbol("expanded module"); /* uninterned */
#endif
    empty_self_modname = scheme_intern_resolved_module_path(empty_self_modname);
  }

  REGISTER_SO(quote_symbol);
  REGISTER_SO(file_symbol);
  REGISTER_SO(lib_symbol);
  REGISTER_SO(planet_symbol);
  quote_symbol = scheme_intern_symbol("quote");
  file_symbol = scheme_intern_symbol("file");
  lib_symbol = scheme_intern_symbol("lib");
  planet_symbol = scheme_intern_symbol("planet");

  REGISTER_SO(kernel_symbol);
  REGISTER_SO(kernel_modname);
  REGISTER_SO(kernel_modidx);
  REGISTER_SO(unsafe_modname);
  REGISTER_SO(flfxnum_modname);
  kernel_symbol = scheme_intern_symbol("#%kernel");
  kernel_modname = scheme_intern_resolved_module_path(kernel_symbol);
  kernel_modidx = scheme_make_modidx(scheme_make_pair(quote_symbol,
                                                      scheme_make_pair(kernel_symbol, 
                                                                       scheme_null)),
                                     scheme_false, kernel_modname);
  (void)scheme_hash_key(kernel_modidx);
  unsafe_modname = scheme_intern_resolved_module_path(scheme_intern_symbol("#%unsafe"));
  flfxnum_modname = scheme_intern_resolved_module_path(scheme_intern_symbol("#%flfxnum"));

  REGISTER_SO(module_symbol);
  REGISTER_SO(module_begin_symbol);
  module_symbol = scheme_intern_symbol("module");
  module_begin_symbol = scheme_intern_symbol("#%module-begin");

  scheme_install_type_writer(scheme_module_type, write_module);
  scheme_install_type_reader(scheme_module_type, read_module);

  GLOBAL_PARAMETER("current-module-name-resolver",  current_module_name_resolver, MZCONFIG_CURRENT_MODULE_RESOLVER, env);
  GLOBAL_PARAMETER("current-module-declare-name",   current_module_name_prefix,   MZCONFIG_CURRENT_MODULE_NAME,     env);
  GLOBAL_PARAMETER("current-module-declare-source", current_module_name_source,   MZCONFIG_CURRENT_MODULE_SRC,      env);

  GLOBAL_PRIM_W_ARITY("dynamic-require",                  scheme_dynamic_require,     2, 3, env);
  GLOBAL_PRIM_W_ARITY("dynamic-require-for-syntax",       dynamic_require_for_syntax, 2, 3, env);
  GLOBAL_PRIM_W_ARITY("namespace-require",                namespace_require,          1, 1, env);
  GLOBAL_PRIM_W_ARITY("namespace-attach-module",          namespace_attach_module,    2, 3, env);
  GLOBAL_PRIM_W_ARITY("namespace-unprotect-module",       namespace_unprotect_module, 2, 3, env);
  GLOBAL_PRIM_W_ARITY("namespace-require/copy",           namespace_require_copy,     1, 1, env);
  GLOBAL_PRIM_W_ARITY("namespace-require/constant",       namespace_require_constant, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("namespace-require/expansion-time", namespace_require_etonly,   1, 1, env);
  GLOBAL_PRIM_W_ARITY("compiled-module-expression?",      module_compiled_p,          1, 1, env);
  GLOBAL_PRIM_W_ARITY("module-compiled-name",             module_compiled_name,       1, 1, env);
  GLOBAL_PRIM_W_ARITY("module-compiled-imports",          module_compiled_imports,    1, 1, env);
  GLOBAL_PRIM_W_ARITY2("module-compiled-exports",         module_compiled_exports,    1, 1, 2, 2, env);
  GLOBAL_PRIM_W_ARITY("module-compiled-language-info",    module_compiled_lang_info,  1, 1, env);
  GLOBAL_FOLDING_PRIM("module-path-index?",               module_path_index_p,        1, 1, 1, env); 
  GLOBAL_PRIM_W_ARITY("module-path-index-resolve",        module_path_index_resolve,  1, 1, env); 
  GLOBAL_PRIM_W_ARITY2("module-path-index-split",         module_path_index_split,    1, 1, 2, 2, env); 
  GLOBAL_PRIM_W_ARITY("module-path-index-join",           module_path_index_join,     2, 2, env);
  GLOBAL_FOLDING_PRIM("resolved-module-path?",            resolved_module_path_p,     1, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("make-resolved-module-path",        make_resolved_module_path,  1, 1, env);
  GLOBAL_PRIM_W_ARITY("resolved-module-path-name",        resolved_module_path_name,  1, 1, env);
  GLOBAL_PRIM_W_ARITY("module-provide-protected?",        module_export_protected_p,  2, 2, env);
  GLOBAL_PRIM_W_ARITY("module->namespace",                module_to_namespace,        1, 1, env);
  GLOBAL_PRIM_W_ARITY("module->language-info",            module_to_lang_info,        1, 2, env);
  GLOBAL_PRIM_W_ARITY("module->imports",                  module_to_imports,          1, 1, env);
  GLOBAL_PRIM_W_ARITY2("module->exports",                 module_to_exports,          1, 1, 2, 2, env);
  GLOBAL_PRIM_W_ARITY("module-path?",                     is_module_path,             1, 1, env);
}

void scheme_init_module_resolver(void)
{
  Scheme_Object *o;
  Scheme_Config *config;

  REGISTER_SO(starts_table);
  starts_table = scheme_make_weak_equal_table();
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  REGISTER_SO(place_local_modpath_table);
  place_local_modpath_table = scheme_make_weak_equal_table();
#endif

  config = scheme_current_config();

  o = scheme_make_prim_w_arity(default_module_resolver,
			       "default-module-name-resolver",
			       1, 4);
 
  scheme_set_param(config, MZCONFIG_CURRENT_MODULE_RESOLVER, o);

  scheme_set_param(config, MZCONFIG_CURRENT_MODULE_NAME, scheme_false);
}

void scheme_finish_kernel(Scheme_Env *env)
{
  /* When this function is called, the initial namespace has all the
     primitive bindings for syntax and procedures. This function fills
     in the module wrapper for #%kernel. */
  Scheme_Object *w;

  REGISTER_SO(kernel);

  kernel = MALLOC_ONE_TAGGED(Scheme_Module);
  kernel->so.type = scheme_module_type;
  env->module = kernel;

  {
    Scheme_Object *insp;
    insp = scheme_get_current_inspector();

    env->insp = insp;
    kernel->insp = insp;
  }

  kernel->modname = kernel_modname;
  kernel->modsrc = kernel_modname;
  kernel->requires = scheme_null;
  kernel->et_requires = scheme_null;
  kernel->tt_requires = scheme_null;
  kernel->dt_requires = scheme_null;
  kernel->other_requires = NULL;

  
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

    kernel->no_cert = 1;

    {
      Scheme_Module_Exports *me;
      me = make_module_exports();
      kernel->me = me;
      kernel->me->modsrc = kernel_modname;
    }

    kernel->me->rt->provides = exs;
    kernel->me->rt->provide_srcs = NULL;
    kernel->me->rt->provide_src_names = exs;
    kernel->me->rt->num_provides = count;
    kernel->me->rt->num_var_provides = syntax_start;
    scheme_populate_pt_ht(kernel->me->rt);

    env->running = 1;
    env->et_running = 1;
    env->attached = 1;

    /* Since this is the first module rename, it's registered as
       the kernel module rename: */
    rn = scheme_make_module_rename(scheme_make_integer(0), mzMOD_RENAME_NORMAL, NULL);
    for (i = kernel->me->rt->num_provides; i--; ) {
      scheme_extend_module_rename(rn, kernel_modidx, exs[i], exs[i], kernel_modidx, exs[i], 
          0, scheme_make_integer(0), NULL, NULL, 0);
    }
    scheme_seal_module_rename(rn, STX_SEAL_ALL);
  }

  REGISTER_SO(scheme_sys_wraps0);
  REGISTER_SO(scheme_sys_wraps1);

  scheme_sys_wraps0 = scheme_sys_wraps_phase_worker(0);
  scheme_sys_wraps1 = scheme_sys_wraps_phase_worker(1);

  scheme_sys_wraps(NULL);

  REGISTER_SO(scheme_module_stx);
  REGISTER_SO(scheme_module_begin_stx);
  REGISTER_SO(scheme_begin_stx);
  REGISTER_SO(scheme_define_values_stx);
  REGISTER_SO(scheme_define_syntaxes_stx);
  REGISTER_SO(define_for_syntaxes_stx);
  REGISTER_SO(require_stx);
  REGISTER_SO(provide_stx);
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
  scheme_module_begin_stx = scheme_datum_to_syntax(module_begin_symbol, scheme_false, w, 0, 0);
  scheme_begin_stx = scheme_datum_to_syntax(scheme_intern_symbol("begin"), scheme_false, w, 0, 0);
  scheme_define_values_stx = scheme_datum_to_syntax(scheme_intern_symbol("define-values"), scheme_false, w, 0, 0);
  scheme_define_syntaxes_stx = scheme_datum_to_syntax(scheme_intern_symbol("define-syntaxes"), scheme_false, w, 0, 0);
  define_for_syntaxes_stx = scheme_datum_to_syntax(scheme_intern_symbol("define-values-for-syntax"), scheme_false, w, 0, 0);
  require_stx = scheme_datum_to_syntax(scheme_intern_symbol("#%require"), scheme_false, w, 0, 0);
  provide_stx = scheme_datum_to_syntax(scheme_intern_symbol("#%provide"), scheme_false, w, 0, 0);
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

static int is_builtin_modname(Scheme_Object *modname) 
{
  return (SAME_OBJ(modname, kernel_modname)
          || SAME_OBJ(modname, unsafe_modname)
          || SAME_OBJ(modname, flfxnum_modname));
}

Scheme_Object *scheme_sys_wraps(Scheme_Comp_Env *env)
{
  long phase;

  if (!env)
    phase = 0;
  else if (SCHEME_INTP((Scheme_Object *)env))
    phase = SCHEME_INT_VAL((Scheme_Object *)env);
  else
    phase = env->genv->phase;

  return scheme_sys_wraps_phase(scheme_make_integer(phase));
}

static Scheme_Object *scheme_sys_wraps_phase_worker(long p)
{
  Scheme_Object *rn, *w;

  rn = scheme_make_module_rename(scheme_make_integer(p), mzMOD_RENAME_NORMAL, NULL);

  /* Add a module mapping for all kernel provides: */
  scheme_extend_module_rename_with_shared(rn, kernel_modidx, 
                                          kernel->me->rt,
                                          scheme_make_integer(p),
                                          scheme_make_integer(0),
                                          scheme_null,
                                          1);

  scheme_seal_module_rename(rn, STX_SEAL_ALL);

  w = scheme_datum_to_syntax(kernel_symbol, scheme_false, scheme_false, 0, 0);
  w = scheme_add_rename(w, rn);

  return w;
}

Scheme_Object *scheme_sys_wraps_phase(Scheme_Object *phase)
{
  long p;

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
  initial_renames = scheme_make_module_rename(scheme_make_integer(0), 
                                              mzMOD_RENAME_NORMAL, 
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
    m = (Scheme_Module *)scheme_hash_get(initial_modules_env->module_registry, a[1]);
    start_module(m, initial_modules_env, 0, a[1], 0, 1, 0, scheme_null);

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

/**********************************************************************/
/*                             parameters                             */
/**********************************************************************/

static Scheme_Object *default_module_resolver(int argc, Scheme_Object **argv)
{
  Scheme_Object *p = argv[0];

  if (argc == 1)
    return scheme_void; /* ignore notify */

  if (SCHEME_PAIRP(p)
      && SAME_OBJ(SCHEME_CAR(p), quote_symbol)
      && SCHEME_PAIRP(SCHEME_CDR(p))
      && SCHEME_SYMBOLP(SCHEME_CAR(SCHEME_CDR(p)))
      && SCHEME_NULLP(SCHEME_CDR(SCHEME_CDR(p))))
    return scheme_intern_resolved_module_path(SCHEME_CAR(SCHEME_CDR(p)));

  scheme_arg_mismatch("default-module-name-resolver", 
		      "the kernel's resolver works only on `quote' forms; given: ", 
		      p);
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
  
  if (SCHEME_FALSEP(o) || (SCHEME_MODNAMEP(o)))
    return o;

  return NULL;
}

static Scheme_Object *
current_module_name_prefix(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-module-declared-name",
			     scheme_make_integer(MZCONFIG_CURRENT_MODULE_NAME),
			     argc, argv,
			     -1, prefix_p, "resolved-module-path or #f", 1);
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
  return scheme_param_config("current-module-declared-name",
			     scheme_make_integer(MZCONFIG_CURRENT_MODULE_SRC),
			     argc, argv,
			     -1, source_p, "symbol, complete path, or #f", 1);
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
  long base_phase;

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
    scheme_wrong_type(errname, "symbol, #f, 0, or void", 1, argc, argv);
    return NULL;
  }

  if (fail_thunk)
    scheme_check_proc_arity(errname, 0, 2, argc, argv);

  if (SAME_TYPE(SCHEME_TYPE(modname), scheme_module_index_type))
    modidx = modname;
  else
    modidx = scheme_make_modidx(modname, scheme_false, scheme_false);

  modname = scheme_module_resolve(modidx, 1);
  base_phase = env->phase;

  if (phase == 1) {
    scheme_prepare_exp_env(env);
    if (mod_phase)
      lookup_env = env->exp_env;
    else
      env = env->exp_env;
  }

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

                  start_module(m, env, 0, modidx, 0, 1, base_phase, scheme_null);
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
                  scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                                   "%s: name is provided as syntax: %V by module: %V",
                                   errname,
                                   name, srcm->modsrc);
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
	  if (fail_with_error) {
            if (fail_thunk)
              return scheme_tail_apply(fail_thunk, 0, NULL);
	    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
			     "%s: name is not provided: %V by module: %V",
			     errname,
			     name, srcm->modsrc);
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
               scheme_null);

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
			 name, srcm->modsrc);
    }

    if (!menv || !menv->toplevel) {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                       "%s: module initialization failed: %V",
                       errname,
                       srcm->modsrc);
    }
    
    b = scheme_bucket_from_table(menv->toplevel, (const char *)srcname);
    if (!((Scheme_Bucket_With_Home *)b)->home)
      ((Scheme_Bucket_With_Home *)b)->home = menv;

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
  Scheme_Object *form, *rns;

  if (!env)
    env = scheme_get_env(NULL);
  scheme_prepare_exp_env(env);

  form = scheme_datum_to_syntax(scheme_make_pair(require_stx,
						 scheme_make_pair(argv[0], scheme_null)),
				scheme_false, scheme_false, 1, 0);

  rns = scheme_make_module_rename_set(mzMOD_RENAME_TOPLEVEL, NULL);

  parse_requires(form, scheme_false, env, NULL,
                 rns, NULL,
                 NULL /* ck */, NULL /* data */,
                 NULL, 
                 1, copy, 0, 
                 etonly ? 1 : -1, !etonly,
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

#if 0
# define LOG_ATTACH(x) (x, fflush(stdout))
#else
# define LOG_ATTACH(x) /* nothing */
#endif

static Scheme_Object *namespace_attach_module(int argc, Scheme_Object *argv[])
{
  Scheme_Env *from_env, *to_env, *menv, *menv2;
  Scheme_Object *todo, *next_phase_todo, *prev_phase_todo;
  Scheme_Object *name, *notifies = scheme_null, *a[1], *resolver;
  Scheme_Object *to_modchain, *from_modchain, *l;
  Scheme_Hash_Table *checked, *next_checked, *prev_checked;
  Scheme_Object *past_checkeds, *future_checkeds, *future_todos, *past_to_modchains, *past_todos;
  Scheme_Module *m2;
  int same_namespace, set_env_for_notify = 0, phase, orig_phase, max_phase, first_iteration;
  int just_declare;
  Scheme_Object *nophase_todo;
  Scheme_Hash_Table *nophase_checked;

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

  if (from_env->phase != to_env->phase) {
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                     "namespace-attach-module: "
                     "source namespace phase: %ld does not match destination namespace phase: %ld",
                     (long)from_env->phase, (long)to_env->phase);
  }

  name = scheme_module_resolve(scheme_make_modidx(argv[1], scheme_false, scheme_false), 0);

  todo = scheme_make_pair(name, scheme_null);
  next_phase_todo = scheme_null;
  prev_phase_todo = scheme_null;
  nophase_todo = scheme_null;
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

  first_iteration = 1;
  max_phase = phase;
  just_declare = 0;

  checked = scheme_make_hash_table(SCHEME_hash_ptr);
  scheme_hash_set(checked, name, scheme_true);

  /* Check whether todo, or anything it needs, is already declared
     incompatibly. Successive iterations of the outer loop explore
     successive phases (i.e, for-syntax levels). */
  while (!SCHEME_NULLP(todo)) {
    if (phase > max_phase)
      max_phase = phase;
    if (phase < 0) {
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
	if ((phase >= 0) && SCHEME_TRUEP(to_modchain)) {
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

          if (m2 && (phase > orig_phase) && SAME_OBJ(menv->module, m2)) {
            /* different instance of same module is ok at higher phases */
            m2 = NULL;
          }
	  
	  if (m2) {
	    char *phase, buf[32], *kind;

	    if (!menv->phase)
	      phase = "";
	    else if (menv->phase == 1)
	      phase = " for syntax";
	    else {
	      sprintf(buf, " at phase %ld", menv->phase);
	      phase = buf;
	    }

            if (SAME_OBJ(menv->module, m2))
              kind = "instance of the same module";
            else
              kind = "module with the same name";

	    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
			     "namespace-attach-module: "
			     "a different %s is already "
			     "in the destination namespace%s, for name: %D",
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
        if (phase > 0) {
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
        if (phase >= 0) {
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
    name = SCHEME_CAR(nophase_todo);
    nophase_todo = SCHEME_CDR(nophase_todo);      

    if (!is_builtin_modname(name)) {
      int i;

      menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(from_env->label_env->modchain), name);
    
      LOG_ATTACH(printf("Check #f %s\n", scheme_write_to_string(name, 0)));
    
      if (!menv) {
        scheme_arg_mismatch("namespace-attach-module",
                            "internal error; unknown module (for label): ",
                            name);
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
    }
  }
  
  /* All of the modules that we saw are in the ***_checked hash tables */
  if (prev_checked) {
    past_checkeds = cons((Scheme_Object *)prev_checked, past_checkeds);
  }
  if (!checked)
    checked = scheme_make_hash_table(SCHEME_hash_ptr);
  past_checkeds = cons((Scheme_Object *)checked, past_checkeds);

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
          
          m2 = (Scheme_Module *)scheme_hash_get(from_env->module_registry, name);
          scheme_hash_set(to_env->module_registry, name, (Scheme_Object *)m2);

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

    if (phase >= 0)
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
            if ((phase >= 0) && !just_declare) {
              menv2 = scheme_copy_module_env(menv, to_env, to_modchain, orig_phase);
              if (menv->attached)
                menv2->attached = 1;
              
              check_phase(menv2, NULL, phase);
              scheme_hash_set(MODCHAIN_TABLE(to_modchain), name, (Scheme_Object *)menv2);
            }
	    scheme_hash_set(to_env->module_registry, name, (Scheme_Object *)menv->module);
	    scheme_hash_set(to_env->export_registry, name, (Scheme_Object *)menv->module->me);
	    
	    /* Push name onto notify list: */
	    if (!same_namespace)
	      notifies = scheme_make_pair(name, notifies);
	  }
	}
      }
    }
    
    past_checkeds = SCHEME_CDR(past_checkeds);
    from_modchain = SCHEME_VEC_ELS(from_modchain)[2];
    if (phase > 0)
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

  name = scheme_module_resolve(scheme_make_modidx(argv[1], scheme_false, scheme_false), 0);

  to_modchain = to_env->modchain;

  code_insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);

  if (!SAME_OBJ(name, kernel_modname)
      && !SAME_OBJ(name, flfxnum_modname)) {
    if (SAME_OBJ(name, unsafe_modname))
      menv2 = scheme_get_unsafe_env();
    else
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
  Scheme_Object **exs, **exss, **exsns, *midx, *info, *vec, *nml, *mark_src, **exinsps;
  char *exets;
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
                                            1);
  }

  mark_src = scheme_rename_to_stx(rn);

  exs = pt->provides;
  exsns = pt->provide_src_names;
  exss = pt->provide_srcs;
  exets = pt->provide_src_phases;
  exinsps = pt->provide_insps;
  numvals = pt->num_var_provides;
  for (i = pt->num_provides; i--; ) {
    if (exss && !SCHEME_FALSEP(exss[i]))
      midx = scheme_modidx_shift(exss[i], im->me->src_modidx, idx);
    else
      midx = idx;
    if (!with_shared) {
      scheme_extend_module_rename(rn, midx, exs[i], exsns[i], idx, exs[i], 
                                  exets ? exets[i] : 0, src_phase_index, pt->phase_index, 
                                  exinsps ? exinsps[i] : NULL, 1);
    }
    if (SAME_OBJ(exs[i], module_begin_symbol))
      saw_mb = 1;

    if (required) {
      vec = scheme_make_vector(10, NULL);
      nml = scheme_make_pair(idx, scheme_null);
      SCHEME_VEC_ELS(vec)[0] = nml;
      SCHEME_VEC_ELS(vec)[1] = midx;
      SCHEME_VEC_ELS(vec)[2] = exsns[i];
      SCHEME_VEC_ELS(vec)[3] = ((i < numvals) ? scheme_true : scheme_false);
      SCHEME_VEC_ELS(vec)[4] = exs[i];
      SCHEME_VEC_ELS(vec)[5] = orig_src;
      SCHEME_VEC_ELS(vec)[6] = mark_src;
      SCHEME_VEC_ELS(vec)[7] = (can_override ? scheme_true : scheme_false);
      SCHEME_VEC_ELS(vec)[8] = exets ? scheme_make_integer(exets[i]) : scheme_false;
      SCHEME_VEC_ELS(vec)[9] = exinsps ? exinsps[i] : scheme_false;
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

static Scheme_Hash_Table *get_required_from_tables(Scheme_Hash_Table *tables, Scheme_Object *phase)
{
  Scheme_Object *vec;

  if (!tables)
    return NULL;
  
  vec = scheme_hash_get(tables, phase);
  if (!vec) {
    Scheme_Hash_Table *res;
    vec = scheme_make_vector(3, NULL);
    res = scheme_make_hash_table(SCHEME_hash_ptr);
    SCHEME_VEC_ELS(vec)[1] = (Scheme_Object *)res;
    scheme_hash_set(tables, phase, vec);
  }

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
  scheme_prepare_exp_env(menv);
  start_module(menv->module, menv, 0, NULL, -1, 1, menv->phase, scheme_null);

  if (!menv->rename_set_ready) {
    if (menv->module->rn_stx) {
      Scheme_Object *rns;
      Scheme_Module *m = menv->module;

      scheme_prepare_env_renames(menv, mzMOD_RENAME_NORMAL);

      if (SAME_OBJ(scheme_true, m->rn_stx)) {
	/* Reconstruct renames based on defns and requires. This case is
           used only when it's easy to reconstruct: no renames, no for-syntax
           definitions, etc. */
	int i;
	Scheme_Module *im;
	Scheme_Object *l, *idx, *one_rn, *shift, *name;

	rns = menv->rename_set;
        one_rn = scheme_get_module_rename_from_set(rns, scheme_make_integer(0), 1);

	/* Local, provided: */
	for (i = 0; i < m->me->rt->num_provides; i++) {
	  if (SCHEME_FALSEP(m->me->rt->provide_srcs[i])) {
	    name = m->me->rt->provide_src_names[i];
	    scheme_extend_module_rename(one_rn, m->self_modidx, name, name, m->self_modidx, name, 0, 
                                        scheme_make_integer(0), NULL, NULL, 0);
	  }
	}
	/* Local, not provided: */
	for (i = 0; i < m->num_indirect_provides; i++) {
	  name = m->indirect_provides[i];
	  scheme_extend_module_rename(one_rn, m->self_modidx, name, name, m->self_modidx, name, 0, 
                                      scheme_make_integer(0), NULL, NULL, 0);
	}
        for (i = 0; i < m->num_indirect_syntax_provides; i++) {
	  name = m->indirect_syntax_provides[i];
	  scheme_extend_module_rename(one_rn, m->self_modidx, name, name, m->self_modidx, name, 0, 
                                      scheme_make_integer(0), NULL, NULL, 0);
	}

        one_rn = scheme_get_module_rename_from_set(rns, scheme_make_integer(1), 1);

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
            l = scheme_reverse(l);
            for (; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
              idx = SCHEME_CAR(l);
              name = scheme_module_resolve(idx, 0);
              
              if (SAME_OBJ(name, kernel_modname))
                im = kernel;
              else if (SAME_OBJ(name, unsafe_modname))
                im = scheme_get_unsafe_env()->module;
              else if (SAME_OBJ(name, flfxnum_modname))
                im = scheme_get_flfxnum_env()->module;
              else
                im = (Scheme_Module *)scheme_hash_get(menv->module_registry, name);
              
              add_simple_require_renames(NULL, rns, NULL, im, idx, shift, NULL, 0);
            }
          }
        }
	
	rns = scheme_rename_to_stx(rns);
	m->rn_stx = rns;
      } else if (SCHEME_PAIRP(m->rn_stx)) {
	/* Delayed shift: */
	Scheme_Object *rn_stx, *midx;
	rn_stx = SCHEME_CAR(m->rn_stx);
	midx = SCHEME_CDR(m->rn_stx);
	rns = scheme_stx_to_rename(rn_stx);
	rns = scheme_stx_shift_rename_set(rns, midx, m->self_modidx);
	rn_stx = scheme_rename_to_stx(rns);
	m->rn_stx = rn_stx;
      }

      rns = scheme_stx_to_rename(m->rn_stx);
      scheme_append_rename_set_to_env(rns, menv);
      menv->rename_set_ready = 1;
    }
  }
}

Scheme_Object *scheme_module_to_namespace(Scheme_Object *name, Scheme_Env *env)
{
  Scheme_Env *menv;
  Scheme_Object *modchain;

  name = scheme_module_resolve(scheme_make_modidx(name, scheme_false, scheme_false), 1);

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
		       "module->namespace: current code inspector cannot access namespace of module: %D",
		       name);
    }
  }

  scheme_prep_namespace_rename(menv);

  return (Scheme_Object *)menv;
}

static Scheme_Object *module_to_namespace(int argc, Scheme_Object *argv[])
{
  Scheme_Env *env;

  env = scheme_get_env(NULL);

  if (!SCHEME_PATHP(argv[0])
      && !scheme_is_module_path(argv[0]))
    scheme_wrong_type("module->namespace", "path or module-path", 0, argc, argv);

  return scheme_module_to_namespace(argv[0], env);
}

static Scheme_Module *module_to_(const char *who, int argc, Scheme_Object *argv[])
{
  Scheme_Env *env;
  Scheme_Object *name;
  Scheme_Module *m;

  env = scheme_get_env(NULL);

  if (!SCHEME_PATHP(argv[0])
      && !SCHEME_MODNAMEP(argv[0])
      && !scheme_is_module_path(argv[0]))
    scheme_wrong_type(who, "path, module-path, or resolved-module-path", 0, argc, argv);

  if (SCHEME_MODNAMEP(argv[0]))
    name = argv[0];
  else
    name = scheme_module_resolve(scheme_make_modidx(argv[0], scheme_false, scheme_false),
                                 (argc > 1) ? SCHEME_TRUEP(argv[1]) : 0);

  if (SAME_OBJ(name, kernel_modname))
    m = kernel;
  else if (SAME_OBJ(name, unsafe_modname))
    m = scheme_get_unsafe_env()->module;
  else if (SAME_OBJ(name, flfxnum_modname))
    m = scheme_get_flfxnum_env()->module;
  else {
    env = scheme_get_env(NULL);
    m = (Scheme_Module *)scheme_hash_get(env->module_registry, name);
  }

  if (!m)
    scheme_arg_mismatch(who,
                        "unknown module in the current namespace: ",
                        name);

  return m;
}

static Scheme_Object *module_to_lang_info(int argc, Scheme_Object *argv[])
{
  Scheme_Module *m;

  m = module_to_("module->language-info", argc, argv);

  return (m->lang_info ? m->lang_info : scheme_false);
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

  m = module_to_("module->imports", argc, argv);

  return extract_compiled_imports(m);
}

static Scheme_Object *module_to_exports(int argc, Scheme_Object *argv[])
{
  Scheme_Module *m;

  m = module_to_("module->exports", argc, argv);

  return extract_compiled_exports(m);
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
    return SCHEME_PTR_VAL(m->modname);
  }

  scheme_wrong_type("module-compiled-name", "compiled module declaration", 0, argc, argv);
  return NULL;
}

static Scheme_Object *module_compiled_imports(int argc, Scheme_Object *argv[])
{
  Scheme_Module *m;

  m = scheme_extract_compiled_module(argv[0]);

  if (m)
    return extract_compiled_imports(m);

  scheme_wrong_type("module-compiled-imports", "compiled module declaration", 0, argc, argv);
  return NULL;
}

static Scheme_Object *module_compiled_exports(int argc, Scheme_Object *argv[])
{
  Scheme_Module *m;
  m = scheme_extract_compiled_module(argv[0]);

  if (m)
    return extract_compiled_exports(m);

  scheme_wrong_type("module-compiled-exports", "compiled module declaration", 0, argc, argv);
  return NULL;
}

static Scheme_Object *module_compiled_lang_info(int argc, Scheme_Object *argv[])
{
  Scheme_Module *m;

  m = scheme_extract_compiled_module(argv[0]);

  if (m) {
    return (m->lang_info ? m->lang_info : scheme_false);
  }

  scheme_wrong_type("module-compiled-language-info", "compiled module declaration", 0, argc, argv);
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
  if (!SCHEME_PATHP(argv[0])
      && !scheme_is_module_path(argv[0])
      && !SCHEME_FALSEP(argv[0]))
    scheme_wrong_type("module-path-index-join", "module path, path, or #f", 0, argc, argv);

  if (argv[1]) { /* mzc will generate NULL sometimes; see scheme_declare_module(), below */
    if (SCHEME_TRUEP(argv[1])
	&& !SCHEME_MODNAMEP(argv[1])
	&& !SAME_TYPE(SCHEME_TYPE(argv[1]), scheme_module_index_type))
      scheme_wrong_type("module-path-index-join", "module-path-index, resolved-module-path, or #f", 1, argc, argv);

    if (SCHEME_FALSEP(argv[0]) && !SCHEME_FALSEP(argv[1]))
      scheme_arg_mismatch("module-path-index-join", 
                          "first argument cannot be #f when second argument is not #f: ",
                          argv[1]);
  }

  return scheme_make_modidx(argv[0], argv[1], scheme_false);
}

void scheme_init_module_path_table()
{
  REGISTER_SO(modpath_table);
  modpath_table = scheme_make_weak_equal_table();
}

Scheme_Object *scheme_intern_resolved_module_path_worker(Scheme_Object *o)
{
  Scheme_Object *rmp;
  Scheme_Bucket *b;
  Scheme_Object *return_value;

  mzrt_mutex_lock(modpath_table_mutex);

  rmp = scheme_alloc_small_object();
  rmp->type = scheme_resolved_module_path_type;
  SCHEME_PTR_VAL(rmp) = o;

  scheme_start_atomic();
  b = scheme_bucket_from_table(modpath_table, (const char *)rmp);
  scheme_end_atomic_no_swap();
  if (!b->val)
    b->val = scheme_true;

  return_value = (Scheme_Object *)HT_EXTRACT_WEAK(b->key);

  mzrt_mutex_unlock(modpath_table_mutex);

  return return_value;
}

#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
static Scheme_Object *scheme_intern_local_resolved_module_path_worker(Scheme_Object *o)
{
  Scheme_Object *rmp;
  Scheme_Bucket *b;
  Scheme_Object *return_value;

  rmp = scheme_alloc_small_object();
  rmp->type = scheme_resolved_module_path_type;
  SCHEME_PTR_VAL(rmp) = o;

  scheme_start_atomic();
  b = scheme_bucket_from_table(place_local_modpath_table, (const char *)rmp);
  scheme_end_atomic_no_swap();
  if (!b->val)
    b->val = scheme_true;

  return_value = (Scheme_Object *)HT_EXTRACT_WEAK(b->key);

  return return_value;
}
#endif

Scheme_Object *scheme_intern_resolved_module_path(Scheme_Object *o)
{
#if defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC)
  void *return_payload;
  if (SCHEME_SYMBOLP(o) && SCHEME_SYM_UNINTERNEDP(o)) {
    return scheme_intern_local_resolved_module_path_worker(o);
  }
  return_payload = scheme_master_fast_path(1, o);
  return (Scheme_Object*) return_payload;
#endif
  return scheme_intern_resolved_module_path_worker(o);
}

static Scheme_Object *resolved_module_path_p(int argc, Scheme_Object *argv[])
{
  return (SCHEME_MODNAMEP(argv[0])
          ? scheme_true
          : scheme_false);
}

static Scheme_Object *make_resolved_module_path(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_SYMBOLP(argv[0])
      && (!SCHEME_PATHP(argv[0])
          || !scheme_is_complete_path(SCHEME_PATH_VAL(argv[0]),
                                      SCHEME_PATH_LEN(argv[0]),
                                      SCHEME_PLATFORM_PATH_KIND)))
    scheme_wrong_type("make-resolved-module-path", "symbol or complete path", 0, argc, argv);

  return scheme_intern_resolved_module_path(argv[0]);
}

static Scheme_Object *resolved_module_path_name(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_MODNAMEP(argv[0]))
    scheme_wrong_type("resolved-module-path-name", "resolved-module-path", 0, argc, argv);

  return SCHEME_PTR_VAL(argv[0]);
}


static Scheme_Object *module_export_protected_p(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;
  Scheme_Object *modname, *mv, *name;
  Scheme_Module *m;
  int i, count;

  if (!SCHEME_MODNAMEP(argv[0])
      && !SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_module_index_type))
    scheme_wrong_type("module-provide-protected?", "resolved-module-path or module-path-index", 0, argc, argv);
  if (!SCHEME_SYMBOLP(argv[1]))
    scheme_wrong_type("module-provide-protected?", "symbol", 1, argc, argv);

  modname = scheme_module_resolve(argv[0], 1);
  name = argv[1];

  env = scheme_get_env(NULL);
  if (SAME_OBJ(modname, kernel_modname))
    mv = (Scheme_Object *)kernel;
  else if (SAME_OBJ(modname, unsafe_modname))
    mv = (Scheme_Object *)scheme_get_unsafe_env()->module;
  else if (SAME_OBJ(modname, flfxnum_modname))
    mv = (Scheme_Object *)scheme_get_flfxnum_env()->module;
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

  /* base is needed only for relative-path strings
     and `file' forms: */
  if (SCHEME_CHAR_STRINGP(path)
      || (SCHEME_PAIRP(path)
          && SAME_OBJ(file_symbol, SCHEME_CAR(path))))
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

    a[0] = ((Scheme_Modidx *)modidx)->path;
    a[1] = base;
    a[2] = (stx ? stx : scheme_false);
    a[3] = (load_it ? scheme_true : scheme_false);
    
    if (SCHEME_FALSEP(a[0])) {
      scheme_arg_mismatch("module-path-index-resolve",
                          "\"self\" index has no resolution: ",
                          modidx);
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
      scheme_wrong_type("module name resolver", "resolved-module-path", -1, -1, a);
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
	  if (0 && !sbm->shift_cache) {
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
  if (name == kernel_modname)
    return kernel;
  else if (name == unsafe_modname)
    return scheme_get_unsafe_env()->module;
  else if (name == flfxnum_modname)
    return scheme_get_flfxnum_env()->module;
  else {
    Scheme_Module *m;

    m = (Scheme_Module *)scheme_hash_get(env->module_registry, name);

    if (!m) {
      char *mred_note;

      if (!strcmp(SCHEME_SYM_VAL(SCHEME_PTR_VAL(name)), "#%mred-kernel")
	  && !(scheme_strncmp(scheme_banner(), "Welcome to MzScheme", 19)))
	mred_note = "; need to run in mred instead of mzscheme";
      else
	mred_note = "";

      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "%s: unknown module: %D%s",
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
    Scheme_Module_Phase_Exports *pt;
    int j;

    for (j = 0; j < 2; j++) {
      if (!j)
        pt = m->me->rt;
      else
        pt = m->me->et;
      
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
        
        if (j == 0) {
          count = m->num_indirect_provides;
          for (i = 0; i < count; i++) {
            scheme_hash_set(ht, m->indirect_provides[i], scheme_make_integer(i + nvp));
          }
        } else {
          count = m->num_indirect_et_provides;
          for (i = 0; i < count; i++) {
            scheme_hash_set(ht, m->et_indirect_provides[i], scheme_make_integer(i + nvp));
          }
        }
        
        /* Add syntax as negative ids: */
        count = pt->num_provides;
        for (i = nvp; i < count; i++) {
          if (SCHEME_FALSEP(pt->provide_srcs[i])) {
            scheme_hash_set(ht, pt->provide_src_names[i], scheme_make_integer(-(i+1)));
          }
        }

        if (!j)
          m->accessible = ht;
        else
          m->et_accessible = ht;
      }
    }
  }
}

Scheme_Env *scheme_module_access(Scheme_Object *name, Scheme_Env *env, int rev_mod_phase)
{
  if ((name == kernel_modname) && !rev_mod_phase)
    return scheme_get_kernel_env();
  else if ((name == unsafe_modname) && !rev_mod_phase)
    return scheme_get_unsafe_env();
  else if ((name == flfxnum_modname) && !rev_mod_phase)
    return scheme_get_flfxnum_env();
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
			    Scheme_Object *prot_insp, Scheme_Object *insp, 
                            Scheme_Object *rename_insp, Scheme_Object *in_modidx,
			    Scheme_Env *env, Scheme_Object *symbol,
			    int var, int prot, int *_would_complain)
{
  int need_cert = 1;
  Scheme_Object *midx;

  midx = (env->link_midx ? env->link_midx : env->module->me->src_modidx);
    
  if (stx)
    need_cert = !scheme_stx_certified(stx, certs, prot ? NULL : midx, env->insp);

  if (need_cert && insp)
    need_cert = scheme_module_protected_wrt(env->insp, insp);
  if (need_cert && rename_insp) {
    if (SCHEME_PAIRP(rename_insp)) {
      /* First inspector of pair protects second */
      if (!prot_insp
          || scheme_module_protected_wrt(SCHEME_CAR(rename_insp), prot_insp)) {
        rename_insp = NULL;
      } else
        rename_insp = SCHEME_CDR(rename_insp);
    }
    if (rename_insp)
      need_cert = scheme_module_protected_wrt(env->insp, rename_insp);
  }

  if (need_cert && in_modidx && midx) {
    /* If we're currently executing a macro expander in this module,
       then allow the access under any cirsumstances. This is mostly
       useful for syntax-local-value and local-expand. */
    in_modidx = scheme_module_resolve(in_modidx, 0);
    midx = scheme_module_resolve(midx, 0);
    if (SAME_OBJ(in_modidx, midx))
      need_cert = 0;
  }

  if (need_cert) {
    if (_would_complain) {
      *_would_complain = 1;
    } else {
      /* For error, if stx is no more specific than symbol, drop symbol. */
      if (stx && SAME_OBJ(SCHEME_STX_SYM(stx), symbol)) {
        symbol = stx;
        stx = NULL;
      }
      scheme_wrong_syntax("compile", stx, symbol, 
                          "access from an uncertified context to %s %s from module: %D",
                          prot ? "protected" : "unexported",
                          var ? "variable" : "syntax",
                          env->module->modsrc);
    }
  }
}

Scheme_Object *scheme_check_accessible_in_module(Scheme_Env *env, Scheme_Object *prot_insp, Scheme_Object *in_modidx,
						 Scheme_Object *symbol, Scheme_Object *stx,
						 Scheme_Object *certs, Scheme_Object *unexp_insp, 
                                                 Scheme_Object *rename_insp,
						 int position, int want_pos, 
                                                 int *_protected, int *_unexported,
                                                 Scheme_Env *from_env, int *_would_complain)
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
      || ((env->module->primitive && !env->module->provide_protects))) {
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

        if (env->mod_phase == 0) {
          num_indirect_provides = env->module->num_indirect_provides;
          indirect_provides = env->module->indirect_provides;
        } else if (env->mod_phase == 1) {
          num_indirect_provides = env->module->num_indirect_et_provides;
          indirect_provides = env->module->et_indirect_provides;
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
              && scheme_module_protected_wrt(env->insp, prot_insp)) {
            char *provide_protects;
            
            if (env->mod_phase == 0)
              provide_protects = env->module->provide_protects;
            else if (env->mod_phase == 0)
              provide_protects = env->module->et_provide_protects;
            else
              provide_protects = NULL;
            
            if (provide_protects
                && provide_protects[position]) {
              if (_protected)
                *_protected = 1;
              check_certified(stx, certs, prot_insp, prot_insp, rename_insp, in_modidx, env, symbol, 1, 1, _would_complain);
            }
          }

          if (need_cert)
            check_certified(stx, certs, prot_insp, unexp_insp, rename_insp, in_modidx, env, symbol, 1, 0, _would_complain);
	
          if (want_pos)
            return scheme_make_integer(position);
          else
            return isym;
        } 
      }
      /* failure */
    } else {
      Scheme_Object *pos;

      if (!env->mod_phase)
        pos = scheme_hash_get(env->module->accessible, symbol);
      else if (env->mod_phase == 1)
        pos = scheme_hash_get(env->module->et_accessible, symbol);
      else
        pos = NULL;

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

        if (env->mod_phase == 0)
          provide_protects = env->module->provide_protects;
        else if (env->mod_phase == 1)
          provide_protects = env->module->et_provide_protects;
        else
          provide_protects = NULL;

        if (provide_protects
            && (SCHEME_INT_VAL(pos) < pt->num_provides)
            && provide_protects[SCHEME_INT_VAL(pos)]) {
          if (_protected)
            *_protected = 1;
          check_certified(stx, certs, prot_insp, prot_insp, rename_insp, in_modidx, env, symbol, 1, 1, _would_complain);
        }

        if ((position >= -1) 
            && (SCHEME_INT_VAL(pos) >= pt->num_var_provides)) {
          /* unexported var -- need cert */
          if (_protected)
            *_protected = 1;
          if (_unexported)
            *_unexported = 1;
          check_certified(stx, certs, prot_insp, unexp_insp, rename_insp, in_modidx, env, symbol, 1, 0, _would_complain);
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
        check_certified(stx, certs, prot_insp, unexp_insp, rename_insp, in_modidx, env, symbol, 0, 0, _would_complain);
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
    long srclen;
    
    if (from_env->module)
      srcstr = scheme_display_to_string(from_env->module->modsrc, &srclen);
    else {
      srcstr = "";
      srclen = 0;
    }

    scheme_wrong_syntax("link", stx, symbol, 
                        "module mismatch, probably from old bytecode whose dependencies have changed: "
                        "variable not provided (directly or indirectly%s) from module: %D%s%t at source phase level: %d",
                        (position >= 0) ? " and at the expected position" : "",
                        env->module->modsrc,
                        srclen ? " accessed from module: " : "",
                        srcstr, srclen,
                        env->mod_phase);
  }

  return NULL;
}

void scheme_check_unsafe_accessible(Scheme_Object *insp, Scheme_Env *from_env)
{
  Scheme_Env *unsafe_env;

  unsafe_env = scheme_get_unsafe_env();

  if (SCHEME_HASHTRP(insp)) {
    Scheme_Hash_Tree *t = (Scheme_Hash_Tree *)insp;
    int i;
    Scheme_Object *k, *v;

    for (i = t->count; i--; ) {
      scheme_hash_tree_index(t, i, &k, &v);
      insp = k;
      if (scheme_module_protected_wrt(unsafe_env->insp, insp)) {
        break;
      }
    }

    if (i < 0)
      return;
  }

  if (scheme_module_protected_wrt(unsafe_env->insp, insp)) {
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
      || SAME_OBJ(modname, flfxnum_modname))
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
  if (SAME_OBJ(modname, kernel_modname)) {
    Scheme_Env *kenv;
    kenv = scheme_get_kernel_env();
    name = SCHEME_STX_SYM(name);
    return scheme_lookup_in_table(kenv->syntax, (char *)name);
  } else if (SAME_OBJ(modname, unsafe_modname)
             || SAME_OBJ(modname, flfxnum_modname)) {
    /* no unsafe or flfxnum syntax */
    return NULL;
  } else {
    Scheme_Env *menv;
    Scheme_Object *val;

    menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(env->modchain), modname);
    
    if (!menv)
      return NULL;

    name = scheme_tl_id_sym(menv, name, NULL, 0, NULL, NULL);

    val = scheme_lookup_in_table(menv->syntax, (char *)name);

    return val;
  }
}

void scheme_module_force_lazy(Scheme_Env *env, int previous)
{
  /* not anymore */
}

static int wait_registry(Scheme_Env *env)
{
  Scheme_Object *lock, *a[1];

  while (1) {
    lock = scheme_hash_get(env->module_registry, scheme_false);
    if (!lock)
      return 1;

    if (SAME_OBJ(SCHEME_CDR(lock), (Scheme_Object *)scheme_current_thread))
      return 0;

    a[0] = SCHEME_CAR(lock);
    a[1] = SCHEME_CDR(lock);
    (void)scheme_sync(1, a);
  }
}

static void lock_registry(Scheme_Env *env)
{
  Scheme_Object *lock;
  lock = scheme_make_pair(scheme_make_sema(0),
                          (Scheme_Object *) scheme_current_thread);
  scheme_hash_set(env->module_registry, scheme_false, lock);
}

static void unlock_registry(Scheme_Env *env)
{
  Scheme_Object *lock;
  lock = scheme_hash_get(env->module_registry, scheme_false);
  scheme_post_sema(SCHEME_CAR(lock));
  scheme_hash_set(env->module_registry, scheme_false, NULL);
}

XFORM_NONGCING static long make_key(int base_phase, int eval_exp, int eval_run)
{
  return ((base_phase << 3) 
          | (eval_exp ? ((eval_exp > 0) ? 2 : 4) : 0) 
          | (eval_run ? 1 : 0));
}

static int did_start(Scheme_Object *v, int base_phase, int eval_exp, int eval_run)
{
  long key;

  key = make_key(base_phase, eval_exp, eval_run);

  if (!v)
    return 0;

  if (scheme_hash_tree_get((Scheme_Hash_Tree *)v, scheme_make_integer(key)))
    return 1;

  return 0;
}

static Scheme_Object *add_start(Scheme_Object *v, int base_phase, int eval_exp, int eval_run)
{
  long key;
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
static void show(const char *what, Scheme_Env *menv, int v1, int v2, int base_phase)
{
  if (menv->phase > 3) return;
  if (1 || SCHEME_SYMBOLP(SCHEME_PTR_VAL(menv->module->modname)))
    if (1 || SCHEME_SYM_VAL(SCHEME_PTR_VAL(menv->module->modname))[0] != '#') {
      int i;
      for (i = 0; i < indent; i++) {
        fprintf(stderr, " ");
      }
      fprintf(stderr, "%s \t%s @%ld/%d [%d/%d] %p\n", 
              what, scheme_write_to_string(menv->module->modname, NULL), 
              menv->phase, base_phase, v1, v2, menv->modchain);
    }
}
static void show_done(const char *what, Scheme_Env *menv, int v1, int v2, int base_phase){
  show(what, menv, v1, v2, base_phase);
}
#else
# define show_indent(d) /* nothing */
# define show(w, m, v1, v2, bp) /* nothing */
# define show_done(w, m, v1, v2, bp) /* nothing */
#endif

static void compute_require_names(Scheme_Env *menv, Scheme_Object *phase, 
                                  Scheme_Env *load_env, Scheme_Object *syntax_idx)
{
  Scheme_Object *np, *midx, *l, *reqs, *req_names;

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

  np = scheme_null;

  for (l = reqs; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    midx = scheme_modidx_shift(SCHEME_CAR(l), 
                               menv->module->me->src_modidx, 
                               (syntax_idx ? syntax_idx : menv->link_midx));

    if (load_env)
      module_load(scheme_module_resolve(midx, 1), load_env, NULL);
    
    np = cons(midx, np);
  }

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
                               long base_phase, Scheme_Object *cycle_list, Scheme_Object *syntax_idx)
{
  Scheme_Object *new_cycle_list, *midx, *l;
  Scheme_Module *im;

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
                   new_cycle_list);
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
                   new_cycle_list);
    }
  }

  compute_require_names(menv, scheme_make_integer(0), env, syntax_idx);

  for (l = menv->require_names; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    midx = SCHEME_CAR(l);

    im = module_load(scheme_module_resolve(midx, 1), env, NULL);

    start_module(im, env, 0, midx, eval_exp, eval_run, base_phase, new_cycle_list);
  }

  scheme_prepare_exp_env(menv);
  menv->exp_env->link_midx = menv->link_midx;
  
  if (!SCHEME_NULLP(menv->module->et_requires)) {
    compute_require_names(menv, scheme_make_integer(1), env, syntax_idx);
    
    for (l = menv->et_require_names; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
      midx = SCHEME_CAR(l);
      
      im = module_load(scheme_module_resolve(midx, 1), env, NULL);
      
      start_module(im, menv->exp_env, 0, midx, eval_exp, eval_run, base_phase, new_cycle_list);
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
                         new_cycle_list);
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
            
            start_module(im, menv2, 0, midx, eval_exp, eval_run, base_phase, new_cycle_list);
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
  long base_phase;
  Scheme_Object *cycle_list;
  Scheme_Object *syntax_idx;
} Start_Module_Args;

static void chain_start_module_w_push(Scheme_Env *menv, Scheme_Env *env, int eval_exp, int eval_run, 
                                      long base_phase, Scheme_Object *cycle_list, Scheme_Object *syntax_idx)
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

static Scheme_Env *instantiate_module(Scheme_Module *m, Scheme_Env *env, int restart, Scheme_Object *syntax_idx)
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
      /* printf("new %ld %s\n", env->phase, SCHEME_SYM_VAL(m->modname)); */
      menv = scheme_new_module_env(env, m, 0);
      scheme_hash_set(MODCHAIN_TABLE(env->modchain), m->modname, (Scheme_Object *)menv);
      
      menv->phase = env->phase;
      menv->link_midx = syntax_idx;
    } else {
      Scheme_Env *env2;

      menv->module = m;
      menv->running = 0;
      menv->et_running = 0;
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

    insp = scheme_make_inspector(m->insp);
    menv->insp = insp;

    /* These three should be set by various "finish"es, but
       we initialize them in case there's an error runing a "finish". */
    menv->require_names = scheme_null;
    menv->et_require_names = scheme_null;
    menv->tt_require_names = scheme_null;
    menv->dt_require_names = scheme_null;

    if (env->label_env != env) {
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
  }

  return menv;
}

static void expstart_module(Scheme_Env *menv, Scheme_Env *env, int restart)
{
  if (!restart) {
    if (menv && menv->et_running)
      return;
  }

  if (menv->module->primitive)
    return;

  menv->et_running = 1;
  if (scheme_starting_up)
    menv->attached = 1; /* protect initial modules from redefinition, etc. */

  run_module_exptime(menv, 0);

  return;
}

static void run_module_exptime(Scheme_Env *menv, int set_ns)
{
#ifdef MZ_USE_JIT
  (void)scheme_module_exprun_start(menv, set_ns, scheme_make_pair(menv->module->modname, scheme_void));
#else
  (void)scheme_module_exprun_finish(menv, set_ns);
#endif
}

void *scheme_module_exprun_finish(Scheme_Env *menv, int set_ns)
{
  int let_depth, for_stx;
  Scheme_Object *names, *e;
  Resolve_Prefix *rp;
  Scheme_Comp_Env *rhs_env;
  int i, cnt;
  Scheme_Env *exp_env;
  Scheme_Bucket_Table *syntax, *for_stx_globals;
  Scheme_Cont_Frame_Data cframe;
  Scheme_Config *config;
  
  if (menv->module->primitive)
    return NULL;

  if (!SCHEME_VEC_SIZE(menv->module->et_body))
    return NULL;

  syntax = menv->syntax;

  exp_env = menv->exp_env;

  if (!exp_env)
    return NULL;

  for_stx_globals = exp_env->toplevel;

  if (set_ns) {
    config = scheme_extend_config(scheme_current_config(),
                                  MZCONFIG_ENV,
                                  (Scheme_Object *)menv);
    
    scheme_push_continuation_frame(&cframe);
    scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);
  }

  rhs_env = scheme_new_comp_env(menv, menv->module->insp, SCHEME_TOPLEVEL_FRAME);

  cnt = SCHEME_VEC_SIZE(menv->module->et_body);
  for (i = 0; i < cnt; i++) {
    e = SCHEME_VEC_ELS(menv->module->et_body)[i];
      
    names = SCHEME_VEC_ELS(e)[0];
    let_depth = SCHEME_INT_VAL(SCHEME_VEC_ELS(e)[2]);
    rp = (Resolve_Prefix *)SCHEME_VEC_ELS(e)[3];
    for_stx = SCHEME_TRUEP(SCHEME_VEC_ELS(e)[4]);
    e = SCHEME_VEC_ELS(e)[1];
      
    if (SCHEME_SYMBOLP(names))
      names = scheme_make_pair(names, scheme_null);

    eval_exptime(names, scheme_list_length(names), e, exp_env, rhs_env,
                 rp, let_depth, 1, (for_stx ? for_stx_globals : syntax), for_stx,
                 NULL, scheme_false);
  }

  if (set_ns) {
    scheme_pop_continuation_frame(&cframe);
  }

  return NULL;
}

static void do_start_module(Scheme_Module *m, Scheme_Env *menv, Scheme_Env *env, int restart)
{
  if (m->primitive) {
    menv->running = 1;
    menv->ran = 1;
    return;
  }

  if (menv->running > 0) {
    return;
  }
  
  menv->running = 1;

  if (menv->module->prim_body) {
    Scheme_Invoke_Proc ivk = menv->module->prim_body;
    menv->ran = 1;
    ivk(menv, menv->phase, menv->link_midx, m->body);
  } else {
    eval_module_body(menv, env);
  }
}

static void should_run_for_compile(Scheme_Env *menv)
{
  if (!menv->available_next[0]) {
    menv->available_next[0] = MODCHAIN_AVAIL(menv->modchain, 0);
    MODCHAIN_AVAIL(menv->modchain, 0) = (Scheme_Object *)menv;
  }
  if (!menv->available_next[1]) {
    menv->available_next[1] = MODCHAIN_AVAIL(menv->modchain, 1);
    MODCHAIN_AVAIL(menv->modchain, 1) = (Scheme_Object *)menv;
  }
}

static void start_module(Scheme_Module *m, Scheme_Env *env, int restart, 
                         Scheme_Object *syntax_idx, int eval_exp, int eval_run, long base_phase,
                         Scheme_Object *cycle_list)
/* eval_exp == -1 => make it ready, eval_exp == 1 => run exp-time, eval_exp = 0 => don't even make ready */
{
  Scheme_Env *menv;
  Scheme_Object *l, *new_cycle_list;
  int prep_namespace = 0;

  if (is_builtin_modname(m->modname))
    return;

  for (l = cycle_list; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    if (SAME_OBJ(m->modname, SCHEME_CAR(l))) {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "module: import cycle detected at: %D",
		       m->modsrc);
    }
  }

  new_cycle_list = scheme_make_pair(m->modname, cycle_list);

  menv = instantiate_module(m, env, restart, syntax_idx);

  check_phase(menv, env, 0);

  show("chck", menv, eval_exp, eval_run, base_phase);

  if (did_start(menv->did_starts, base_phase, eval_exp, eval_run))
    return;
  
  show("strt", menv, eval_exp, eval_run, base_phase);
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

  if (env->phase == base_phase) {
    if (eval_exp) {
      if (eval_exp > 0) {
        show("exp=", menv, eval_exp, eval_run, base_phase);
        expstart_module(menv, env, restart);
      } else {
        should_run_for_compile(menv);
      }
    }
    if (eval_run) {
      show("run=", menv, eval_exp, eval_run, base_phase);
      do_start_module(m, menv, env, restart);
    }
  } else if (env->phase < base_phase) {
    if (env->phase == base_phase - 1) {
      if (eval_run) {
        show("run-", menv, eval_exp, eval_run, base_phase);
        expstart_module(menv, env, restart);
      }
    }
  } else {
    /* env->phase > base_phase */
    if (eval_exp) {
      should_run_for_compile(menv);
    }
    if (eval_exp > 0) {
      if (env->phase == base_phase + 1) {
        show("run+", menv, eval_exp, eval_run, base_phase);
        do_start_module(m, menv, env, restart);
      }
    }
  }

  show_indent(-1);
  show_done("done", menv, eval_exp, eval_run, base_phase);

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
      start_module(menv->module, env, 0,
                   NULL, 1, 0, base_phase,
                   scheme_null);
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

  /* A top-level `require' can introduce in any phase with a
     `for-syntax' import whose visit triggers an instantiation.
     So, also check for instances at the next phase. */
  if (env->exp_env) {
    do_prepare_compile_env(env->exp_env, env->phase, 1);
  }
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
# define LOG_RUN_DECLS long start_time
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
#ifdef MZ_USE_JIT
  (void)scheme_module_run_start(menv, env, scheme_make_pair(menv->module->modsrc, scheme_true));
#else
  (void)scheme_module_run_finish(menv, env);
#endif
}

static Scheme_Object *body_one_expr(void *expr, int argc, Scheme_Object **argv)
{
  return _scheme_eval_linked_expr_multi((Scheme_Object *)expr);
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
    case scheme_syntax_type:
      switch (SCHEME_PINT_VAL(e)) {
      case CASE_LAMBDA_EXPD:
        return 0;
      case DEFINE_VALUES_EXPD:
        e = (Scheme_Object *)SCHEME_IPTR_VAL(e);
        e = SCHEME_VEC_ELS(e)[0];
        break;
      default:
        return 1;
      }
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
  Scheme_Object *body, **save_runstack;
  int depth;
  int i, cnt;
  Scheme_Cont_Frame_Data cframe;
  Scheme_Config *config;
  int volatile save_phase_shift;
  mz_jmp_buf newbuf, * volatile savebuf;
  LOG_RUN_DECLS;

  menv->running = 1;
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
				     0, menv->phase, NULL);

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

    cnt = SCHEME_VEC_SIZE(m->body);
    for (i = 0; i < cnt; i++) {
      body = SCHEME_VEC_ELS(m->body)[i];
      if (needs_prompt(body))
        (void)_scheme_call_with_prompt_multi(body_one_expr, body);
      else
        (void)_scheme_eval_linked_expr_multi(body);
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
  Scheme_Object *prefix, *insp, *src;
  Scheme_Config *config;

  m = MALLOC_ONE_TAGGED(Scheme_Module);
  m->so.type = scheme_module_type;
  
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

  {
    Scheme_Module_Exports *me;
    me = make_module_exports();
    m->me = me;
    me->modsrc = src;
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
 
  m->me->rt->provides = exs;
  m->me->rt->provide_srcs = NULL;
  m->me->rt->provide_src_names = exs;
  m->me->rt->num_provides = count;
  m->me->rt->num_var_provides = count;

  qsort_provides(exs, NULL, NULL, NULL, NULL, NULL, NULL, 0, count, 1);

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
  int count, for_stx;
  Scheme_Object *expr, *certs;
  Scheme_Env *genv;
  Scheme_Comp_Env *comp_env;
  Resolve_Prefix *rp;
  int let_depth, shift;
  Scheme_Bucket_Table *syntax;
  Scheme_Object *free_id_rename_rn;

  names = (Scheme_Object *)p->ku.k.p1;
  expr = (Scheme_Object *)p->ku.k.p2;
  genv = (Scheme_Env *)SCHEME_CAR((Scheme_Object *)p->ku.k.p3);
  comp_env = (Scheme_Comp_Env *)SCHEME_CDR((Scheme_Object *)p->ku.k.p3);
  free_id_rename_rn = SCHEME_CAR((Scheme_Object *)p->ku.k.p4);
  rp = (Resolve_Prefix *)SCHEME_CAR(SCHEME_CDR((Scheme_Object *)p->ku.k.p4));
  syntax = (Scheme_Bucket_Table *)SCHEME_CDR(SCHEME_CDR((Scheme_Object *)p->ku.k.p4));
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

  eval_exptime(names, count, expr, genv, comp_env, rp, let_depth, shift, syntax, for_stx, 
               certs, free_id_rename_rn);

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
                         int for_stx, Scheme_Object *certs,
                         Scheme_Object *free_id_rename_rn)
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
    vals = scheme_make_pair(free_id_rename_rn, vals);
    p->ku.k.p4 = vals;
    p->ku.k.i1 = count;
    p->ku.k.i2 = let_depth;
    p->ku.k.i3 = shift;
    p->ku.k.i4 = for_stx;
    p->ku.k.p5 = certs;
    (void)scheme_enlarge_runstack(depth, eval_exptime_k);
    return;
  }

  if (SCHEME_TYPE(expr) > _scheme_values_types_) {
    vals = expr;
  } else {
    save_runstack = scheme_push_prefix(genv, rp,
                                       (shift ? genv->module->me->src_modidx : NULL), 
                                       (shift ? genv->link_midx : NULL), 
                                       1, genv->phase,
                                       NULL);

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
    
      scheme_set_dynamic_state(&dyn_state, comp_env, NULL, scheme_false, certs, 
                         genv, (genv->link_midx ? genv->link_midx : genv->module->me->src_modidx));
      vals = scheme_eval_linked_expr_multi_with_dynamic_state(expr, &dyn_state);

      scheme_pop_continuation_frame(&cframe);
    }

    scheme_pop_prefix(save_runstack);
  }
  
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

          if (SCHEME_TRUEP(free_id_rename_rn)
              && scheme_is_binding_rename_transformer(values[i]))
            scheme_install_free_id_rename(name, scheme_rename_transformer_id(values[i]), free_id_rename_rn, 
                                          scheme_make_integer(0));
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

      if (SCHEME_TRUEP(free_id_rename_rn)
          && scheme_is_binding_rename_transformer(vals))
        scheme_install_free_id_rename(name, scheme_rename_transformer_id(vals), free_id_rename_rn, 
                                      scheme_make_integer(0));
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

static Scheme_Object **declare_insps(int n, Scheme_Object **insps, Scheme_Object *insp)
{
  int i;
  Scheme_Object **naya, *v;

  for (i = 0; i < n; i++) {
    if (insps[i] && SCHEME_PAIRP(insps[i]))
      break;
  }
  if (i >= n)
    return insps;
  
  insp = scheme_make_inspector(insp);

  naya = MALLOC_N(Scheme_Object*, n);
  for (i = 0; i < n; i++) {
    v = insps[i];
    if (v && SCHEME_PAIRP(v)) {
      v = cons(insp, SCHEME_CDR(v));
    }
    naya[i] = v;
  }

  return naya;
}

static Scheme_Object *
module_execute(Scheme_Object *data)
{
  Scheme_Module *m;
  Scheme_Env *env;
  Scheme_Env *old_menv;
  Scheme_Config *config;
  Scheme_Object *prefix, *src, *insp, **rt_insps, **et_insps;

  m = MALLOC_ONE_TAGGED(Scheme_Module);
  memcpy(m, data, sizeof(Scheme_Module));

  config = scheme_current_config();

  prefix = scheme_get_param(config, MZCONFIG_CURRENT_MODULE_NAME);
  if (SCHEME_MODNAMEP(prefix)) {
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
      }
    }
  }

  src = scheme_get_param(config, MZCONFIG_CURRENT_MODULE_SRC);
  if (!SCHEME_FALSEP(src)) {
    src = scheme_intern_resolved_module_path(src);
    m->modsrc = src;
  } else
    m->modsrc = m->modname;

  env = scheme_environment_from_dummy(m->dummy);

  if (SAME_OBJ(m->modname, kernel_modname))
    old_menv = scheme_get_kernel_env();
  else if (SAME_OBJ(m->modname, flfxnum_modname))
    old_menv = scheme_get_flfxnum_env();
  else if (SAME_OBJ(m->modname, unsafe_modname))
    old_menv = scheme_get_unsafe_env();
  else
    old_menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(env->modchain), m->modname);

  insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);
  
  if (old_menv) {
    if (scheme_module_protected_wrt(old_menv->insp, insp) || old_menv->attached) {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "module->namespace: current code inspector cannot re-declare module: %D",
		       m->modname);
      return NULL;
    }
  }

  if (m->me->rt->provide_insps)
    rt_insps = declare_insps(m->me->rt->num_provides, m->me->rt->provide_insps, insp);
  else
    rt_insps = NULL;
  if (m->me->et->provide_insps)
    et_insps = declare_insps(m->me->et->num_provides, m->me->et->provide_insps, insp);
  else
    et_insps = NULL;

  if (!SAME_OBJ(rt_insps, m->me->rt->provide_insps)
      || !SAME_OBJ(et_insps, m->me->et->provide_insps)
      || !SAME_OBJ(m->me->modsrc, m->modsrc)) {
    /* have to clone m->me, etc. */
    Scheme_Module_Exports *naya_me;
    Scheme_Module_Phase_Exports *pt;

    naya_me = MALLOC_ONE_TAGGED(Scheme_Module_Exports);
    memcpy(naya_me, m->me, sizeof(Scheme_Module_Exports));
    m->me = naya_me;
    m->me->modsrc = m->modsrc;

    if (!SAME_OBJ(rt_insps, m->me->rt->provide_insps)) {
      pt = MALLOC_ONE_TAGGED(Scheme_Module_Phase_Exports);
      memcpy(pt, m->me->rt, sizeof(Scheme_Module_Phase_Exports));
      m->me->rt = pt;
      pt->provide_insps = rt_insps;
    }

    if (!SAME_OBJ(rt_insps, m->me->et->provide_insps)) {
      pt = MALLOC_ONE_TAGGED(Scheme_Module_Phase_Exports);
      memcpy(pt, m->me->et, sizeof(Scheme_Module_Phase_Exports));
      m->me->et = pt;
      pt->provide_insps = et_insps;
    }
  }

  m->insp = insp;
  scheme_hash_set(env->module_registry, m->modname, (Scheme_Object *)m);
  scheme_hash_set(env->export_registry, m->modname, (Scheme_Object *)m->me);

  /* Replacing an already-running or already-syntaxing module? */
  if (old_menv) {
    start_module(m, env, 1, NULL, old_menv->et_running, old_menv->running, env->phase, scheme_null);
  }

  return scheme_void;
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
  Scheme_Object *l1, *l2;
  Resolve_Prefix *rp;
  
  rp = scheme_prefix_eval_clone(m->prefix);

  if (jit)
    l1 = jit_vector(m->body, 0, jit);
  else
    l1 = m->body;
  l2 = jit_vector(m->et_body, 1, jit);

  if (SAME_OBJ(l1, m->body) 
      && SAME_OBJ(l2, m->body)
      && SAME_OBJ(rp, m->prefix))
    return data;
  
  m = MALLOC_ONE_TAGGED(Scheme_Module);
  memcpy(m, data, sizeof(Scheme_Module));
  m->body = l1;
  m->et_body = l2;
  m->prefix = rp;

  return (Scheme_Object *)m;
}

static Scheme_Object *module_jit(Scheme_Object *data)
{
  return do_module_clone(data, 1);
}

Scheme_Object *scheme_module_eval_clone(Scheme_Object *data)
{
  return do_module_clone(data, 0);
}

static void module_validate(Scheme_Object *data, Mz_CPort *port, 
                            char *stack, Validate_TLS tls,
			    int depth, int letlimit, int delta, 
			    int num_toplevels, int num_stxes, int num_lifts,
                            struct Validate_Clearing *vc, int tailpos)
{
  Scheme_Module *m;
  int i, cnt, let_depth;
  Resolve_Prefix *rp;
  Scheme_Object *e;

  if (!SAME_TYPE(SCHEME_TYPE(data), scheme_module_type))
    scheme_ill_formed_code(port);

  m = (Scheme_Module *)data;

  if (!SCHEME_MODNAMEP(m->modname))
    scheme_ill_formed_code(port);

  scheme_validate_code(port, m->body, m->max_let_depth,
                       m->prefix->num_toplevels, m->prefix->num_stxes, m->prefix->num_lifts,
                       1);
  
  /* validate exp-time code */
  cnt = SCHEME_VEC_SIZE(m->et_body);
  for (i = 0; i < cnt; i++) {
    e = SCHEME_VEC_ELS(m->et_body)[i];
      
    let_depth = SCHEME_INT_VAL(SCHEME_VEC_ELS(e)[2]);
    rp = (Resolve_Prefix *)SCHEME_VEC_ELS(e)[3];
    e = SCHEME_VEC_ELS(e)[1];
      
    scheme_validate_code(port, e, let_depth,
                         rp->num_toplevels, rp->num_stxes, rp->num_lifts,
                         0);
  }
}

static int set_code_closure_flags(Scheme_Object *clones,
                                  int set_flags, int mask_flags,
                                  int just_tentative)
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
    if (!just_tentative || (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_RESULT_TENTATIVE)) {
      flags = (flags & SCHEME_CLOSURE_DATA_FLAGS(data));
      SCHEME_CLOSURE_DATA_FLAGS(data) = set_flags | (SCHEME_CLOSURE_DATA_FLAGS(data) & mask_flags);
      data = (Scheme_Closure_Data *)clone;
      SCHEME_CLOSURE_DATA_FLAGS(data) = set_flags | (SCHEME_CLOSURE_DATA_FLAGS(data) & mask_flags);
    }

    clones = SCHEME_CDR(clones);
  }

  return flags;
}

static Scheme_Object *
module_optimize(Scheme_Object *data, Optimize_Info *info, int context)
{
  Scheme_Module *m = (Scheme_Module *)data;
  Scheme_Object *e, *vars, *old_context;
  int start_simltaneous = 0, i_m, cnt;
  Scheme_Object *cl_first = NULL, *cl_last = NULL;
  Scheme_Hash_Table *consts = NULL, *ready_table = NULL, *re_consts = NULL;
  int cont, next_pos_ready = -1, inline_fuel, is_proc_def;

  old_context = info->context;
  info->context = (Scheme_Object *)m;

  cnt = SCHEME_VEC_SIZE(m->body);

  if (OPT_ESTIMATE_FUTURE_SIZES) {
    if (info->enforce_const) {
      /* For each identifier bound to a procedure, register an initial
         size estimate, which is used to discourage early loop unrolling 
         at the expense of later inlining. */
      for (i_m = 0; i_m < cnt; i_m++) {
        e = SCHEME_VEC_ELS(m->body)[i_m];
        if (SAME_TYPE(SCHEME_TYPE(e), scheme_compiled_syntax_type)
            && (SCHEME_PINT_VAL(e) == DEFINE_VALUES_EXPD)) {
          int n;

          e = (Scheme_Object *)SCHEME_IPTR_VAL(e);
          vars = SCHEME_CAR(e);
          e = SCHEME_CDR(e);

          n = scheme_list_length(vars);
          if (n == 1) {
            if (SAME_TYPE(SCHEME_TYPE(e), scheme_compiled_unclosed_procedure_type)) {
              Scheme_Toplevel *tl;
            
              tl = (Scheme_Toplevel *)SCHEME_CAR(vars);
            
              if (!(SCHEME_TOPLEVEL_FLAGS(tl) & SCHEME_TOPLEVEL_MUTATED)) {
                int pos;
                if (!consts)
                  consts = scheme_make_hash_table(SCHEME_hash_ptr);
                pos = tl->position;
                scheme_hash_set(consts, 
                                scheme_make_integer(pos),
                                scheme_estimate_closure_size(e));
              }
            }
          }
        }
      }

      if (consts) {
        info->top_level_consts = consts;
        consts = NULL;
      }
    }
  }

  for (i_m = 0; i_m < cnt; i_m++) {
    /* Optimize this expression: */
    e = SCHEME_VEC_ELS(m->body)[i_m];

    is_proc_def = 0;
    if (OPT_DISCOURAGE_EARLY_INLINE && info->enforce_const) {
      if (SAME_TYPE(SCHEME_TYPE(e), scheme_compiled_syntax_type)
	  && (SCHEME_PINT_VAL(e) == DEFINE_VALUES_EXPD)) {
        Scheme_Object *e2;
        e2 = (Scheme_Object *)SCHEME_IPTR_VAL(e);
        e2 = SCHEME_CDR(e2);
        if (SAME_TYPE(SCHEME_TYPE(e2), scheme_compiled_unclosed_procedure_type))
          is_proc_def = 1;
      }
    }

    if (is_proc_def && OPT_DISCOURAGE_EARLY_INLINE) {
      info->use_psize = 1;
      inline_fuel = info->inline_fuel;
      if (inline_fuel > 2)
        info->inline_fuel = 2;
    } else
      inline_fuel = 0;
    e = scheme_optimize_expr(e, info, 0);
    if (is_proc_def && OPT_DISCOURAGE_EARLY_INLINE) {
      info->use_psize = 0;
      info->inline_fuel = inline_fuel;
    }
    SCHEME_VEC_ELS(m->body)[i_m] = e;

    if (info->enforce_const) {
      /* If this expression/definition can't have any side effect
	 (including raising an exception), then continue the group of
	 simultaneous definitions: */
      if (SAME_TYPE(SCHEME_TYPE(e), scheme_compiled_syntax_type)
	  && (SCHEME_PINT_VAL(e) == DEFINE_VALUES_EXPD)) {
	int n, cnst = 0, sproc = 0;

	e = (Scheme_Object *)SCHEME_IPTR_VAL(e);

	vars = SCHEME_CAR(e);
	e = SCHEME_CDR(e);

	n = scheme_list_length(vars);
	cont = scheme_omittable_expr(e, n, -1, 0, info);
      
        if (n == 1) {
          if (scheme_compiled_propagate_ok(e, info))
            cnst = 1;
          else if (scheme_is_statically_proc(e, info)) {
            cnst = 1;
            sproc = 1;
          }
        }

	if (cnst) {
	  Scheme_Toplevel *tl;

	  tl = (Scheme_Toplevel *)SCHEME_CAR(vars);

	  if (!(SCHEME_TOPLEVEL_FLAGS(tl) & SCHEME_TOPLEVEL_MUTATED)) {
	    Scheme_Object *e2;

            if (sproc) {
              e2 = scheme_make_noninline_proc(e);
            } else if (SAME_TYPE(SCHEME_TYPE(e), scheme_compiled_unclosed_procedure_type)) {
	      e2 = scheme_optimize_clone(1, e, info, 0, 0);
              if (e2) {
                Scheme_Object *pr;
                pr = scheme_make_raw_pair(scheme_make_raw_pair(e2, e), NULL);
                if (cl_last)
                  SCHEME_CDR(cl_last) = pr;
                else
                  cl_first = pr;
                cl_last = pr;
              } else
                e2 = scheme_make_noninline_proc(e);
	    } else {
	      e2 = e;
	    }

	    if (e2) {
	      int pos;
	      if (!consts)
		consts = scheme_make_hash_table(SCHEME_hash_ptr);
	      pos = tl->position;
	      scheme_hash_set(consts, scheme_make_integer(pos), e2);
              if (!re_consts)
                re_consts = scheme_make_hash_table(SCHEME_hash_ptr);
              scheme_hash_set(re_consts, scheme_make_integer(i_m), 
                              scheme_make_integer(pos));
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

              next_pos_ready = pos;
	    }
	  }
	}
      } else {
	cont = scheme_omittable_expr(e, -1, -1, 0, NULL);
      }
      if (i_m + 1 == cnt)
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
                                     0xFFFF,
                                     0);

	while (1) {
	  /* Re-optimize this expression. We can optimize anything without
             shift-cloning, since there are no local variables in scope. */
          int old_sz, new_sz;

          e = SCHEME_VEC_ELS(m->body)[start_simltaneous];

          if (OPT_LIMIT_FUNCTION_RESIZE) {
            if (SAME_TYPE(SCHEME_TYPE(e), scheme_compiled_syntax_type)
                && (SCHEME_PINT_VAL(e) == DEFINE_VALUES_EXPD)) {
              Scheme_Object *sub_e;
              sub_e = (Scheme_Object *)SCHEME_IPTR_VAL(e);
              sub_e = SCHEME_CDR(sub_e);
              if (SAME_TYPE(SCHEME_TYPE(sub_e), scheme_compiled_unclosed_procedure_type))
                old_sz = scheme_closure_body_size((Scheme_Closure_Data *)sub_e, 0, NULL, NULL);
              else
                old_sz = 0;
            } else
              old_sz = 0;
          } else
            old_sz = 0;

          e = scheme_optimize_expr(e, info, 0);
	  SCHEME_VEC_ELS(m->body)[start_simltaneous] = e;

          if (re_consts) {
            /* Install optimized closures into constant table ---
               unless, maybe, they grow too much: */
            Scheme_Object *rpos;
            rpos = scheme_hash_get(re_consts, scheme_make_integer(start_simltaneous));
            if (rpos) {
              e = (Scheme_Object *)SCHEME_IPTR_VAL(e);
              e = SCHEME_CDR(e);
              if (!scheme_compiled_propagate_ok(e, info)
                  && scheme_is_statically_proc(e, info)) {
                /* If we previously installed a procedure for inlining,
                   don't replace that with a worse approximation. */
                Scheme_Object *old_e;
                old_e = scheme_hash_get(info->top_level_consts, rpos);
                if (SAME_TYPE(SCHEME_TYPE(old_e), scheme_compiled_unclosed_procedure_type))
                  e = NULL;
                else
                  e = scheme_make_noninline_proc(e);
              }

              if (e) {
                if (OPT_LIMIT_FUNCTION_RESIZE) {
                  if (SAME_TYPE(SCHEME_TYPE(e), scheme_compiled_unclosed_procedure_type))
                    new_sz = scheme_closure_body_size((Scheme_Closure_Data *)e, 0, NULL, NULL);
                  else
                    new_sz = 0;
                } else
                  new_sz = 0;
                
                if (!new_sz || !old_sz || (new_sz < 4 * old_sz))
                  scheme_hash_set(info->top_level_consts, rpos, e);
              }
            }
          }

	  if (start_simltaneous == i_m)
	    break;
          start_simltaneous++;
	}

        flags = set_code_closure_flags(cl_first, 0, 0xFFFF, 0);
        (void)set_code_closure_flags(cl_first,
                                     (flags & (CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS)), 
                                     ~(CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS | CLOS_RESULT_TENTATIVE),
                                     1);
      }
      
      cl_last = cl_first = NULL;
      consts = NULL;
      re_consts = NULL;
      start_simltaneous = i_m + 1;
    }

    if (next_pos_ready > -1) {
      if (!ready_table) {
        ready_table = scheme_make_hash_table(SCHEME_hash_ptr);
        if (!consts)
          consts = scheme_make_hash_table(SCHEME_hash_ptr);
        scheme_hash_set(consts, scheme_false, (Scheme_Object *)ready_table);
      }
      scheme_hash_set(ready_table, scheme_make_integer(next_pos_ready), scheme_true);
      next_pos_ready = -1;
    }
  }

  /* Check one more time for expressions that we can omit: */
  {
    int can_omit = 0;
    for (i_m = 0; i_m < cnt; i_m++) {
      /* Optimize this expression: */
      e = SCHEME_VEC_ELS(m->body)[i_m];
      if (scheme_omittable_expr(e, -1, -1, 0, NULL)) {
        can_omit++;
      }
    }
    if (can_omit) {
      Scheme_Object *vec;
      int j = 0;
      vec = scheme_make_vector(cnt - can_omit, NULL);
      for (i_m = 0; i_m < cnt; i_m++) {
        /* Optimize this expression: */
        e = SCHEME_VEC_ELS(m->body)[i_m];
        if (!scheme_omittable_expr(e, -1, -1, 0, NULL)) {
          SCHEME_VEC_ELS(vec)[j++] = e;
        }
      }
      m->body = vec;
    }
  }

  info->context = old_context;

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
  int i, cnt;

  rp = scheme_resolve_prefix(0, m->comp_prefix, 1);
  m->comp_prefix = NULL;

  b = scheme_resolve_expr(m->dummy, old_rslv);
  m->dummy = b;

  rslv = scheme_resolve_info_create(rp);
  rslv->enforce_const = old_rslv->enforce_const;
  rslv->in_module = 1;
  scheme_enable_expression_resolve_lifts(rslv);

  cnt = SCHEME_VEC_SIZE(m->body);
  for (i = 0; i < cnt; i++) {
    Scheme_Object *e;
    e = scheme_resolve_expr(SCHEME_VEC_ELS(m->body)[i], rslv);
    SCHEME_VEC_ELS(m->body)[i] = e;
  }

  m->max_let_depth = rslv->max_let_depth;

  lift_vec = rslv->lifts;
  if (!SCHEME_NULLP(SCHEME_VEC_ELS(lift_vec)[0])) {
    b = scheme_append(SCHEME_VEC_ELS(lift_vec)[0], scheme_vector_to_list(m->body));
    b = scheme_list_to_vector(b);
    m->body = b;
  }
  rp->num_lifts = SCHEME_INT_VAL(SCHEME_VEC_ELS(lift_vec)[1]);

  rp = scheme_remap_prefix(rp, rslv);

  m->prefix = rp;

  /* Exp-time body was resolved during compilation */

  return scheme_make_syntax_resolved(MODULE_EXPD, data);
}

static Scheme_Object *
module_sfs(Scheme_Object *data, SFS_Info *old_info)
{
  Scheme_Module *m = (Scheme_Module *)data;
  Scheme_Object *e, *ex;
  SFS_Info *info;
  int i, cnt, let_depth;

  if (!old_info->for_mod) {
    if (old_info->pass)
      return data;

    info = scheme_new_sfs_info(m->max_let_depth);
    info->for_mod = 1;
    scheme_sfs(scheme_make_syntax_resolved(MODULE_EXPD, data), 
               info, 
               m->max_let_depth);
    return data;
  }

  info = old_info;

  cnt = SCHEME_VEC_SIZE(m->body);
  scheme_sfs_start_sequence(info, cnt, 0);

  for (i = 0; i < cnt; i++) {
    e = scheme_sfs_expr(SCHEME_VEC_ELS(m->body)[i], info, -1);
    SCHEME_VEC_ELS(m->body)[i] = e;
  }

  if (!info->pass) {
    cnt = SCHEME_VEC_SIZE(m->et_body);
    for (i = 0; i < cnt; i++) {
      e = SCHEME_VEC_ELS(m->et_body)[i];
      
      let_depth = SCHEME_INT_VAL(SCHEME_VEC_ELS(e)[2]);
      ex = SCHEME_VEC_ELS(e)[1];
      
      info = scheme_new_sfs_info(let_depth);
      ex = scheme_sfs(ex, info, let_depth);
      SCHEME_VEC_ELS(e)[1] = ex;
    }
  }

  return data;
}

#if 0
# define LOG_EXPAND_DECLS long start_time
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
				Scheme_Compile_Expand_Info *rec, int drec)
{
  Scheme_Object *fm, *nm, *ii, *rn, *et_rn, *iidx, *self_modidx, *rmp, *rn_set;
  Scheme_Module *iim;
  Scheme_Env *menv, *top_env;
  Scheme_Comp_Env *benv;
  Scheme_Module *m;
  Scheme_Object *mbval, *orig_ii;
  int saw_mb, check_mb = 0;
  Scheme_Object *restore_confusing_name = NULL;
  LOG_EXPAND_DECLS;

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

  /* must set before calling new_module_env: */
  rmp = SCHEME_STX_VAL(nm);
  rmp = scheme_intern_resolved_module_path(rmp);
  m->modname = rmp;
  m->modsrc = rmp;

  LOG_START_EXPAND(m);

  if (SAME_OBJ(m->modname, kernel_modname)
      || SAME_OBJ(m->modname, unsafe_modname)
      || SAME_OBJ(m->modname, flfxnum_modname)) {
    /* Too confusing. Give it a different name while compiling. */
    Scheme_Object *k2;
    const char *kname;
    if (SAME_OBJ(m->modname, kernel_modname))
      kname = "#%kernel";
    else if (SAME_OBJ(m->modname, flfxnum_modname))
      kname = "#%flfxnum";
    else
      kname = "#%unsafe";
    k2 = scheme_intern_resolved_module_path(scheme_make_symbol(kname)); /* uninterned! */
    restore_confusing_name = m->modname;
    m->modname = k2;
  }

  {
    Scheme_Module_Exports *me;
    me = make_module_exports();
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

  menv = scheme_new_module_env(top_env, m, 1);

  menv->disallow_unbound = 1;
  
  self_modidx = scheme_make_modidx(scheme_false, scheme_false, m->modname);
  m->self_modidx = self_modidx;
  m->me->src_modidx = self_modidx;

  m->insp = env->insp;

  m->ii_src = ii;

  orig_ii = ii;
  ii = scheme_syntax_to_datum(ii, 0, NULL);

  if (!scheme_is_module_path(ii)) {
    scheme_wrong_syntax(NULL, m->ii_src, form, "initial import is not a well-formed module path");
  }

  iidx = scheme_make_modidx(ii, 
			    self_modidx,
			    scheme_false);

  SCHEME_EXPAND_OBSERVE_NEXT(rec[drec].observer);

  /* load the module for the initial require */
  iim = module_load(_module_resolve(iidx, m->ii_src, NULL, 1), menv, NULL); 
  start_module(iim, menv, 0, iidx, 1, 0, menv->phase, scheme_null);

  {
    Scheme_Object *ins;
    ins = cons(iidx, scheme_null);
    m->requires = ins;
    m->et_requires = scheme_null;
    m->tt_requires = scheme_null;
    m->dt_requires = scheme_null;
  }

  scheme_prepare_env_renames(menv, mzMOD_RENAME_NORMAL);

  rn_set = menv->rename_set;
  rn = scheme_get_module_rename_from_set(rn_set, scheme_make_integer(0), 1);
  et_rn = scheme_get_module_rename_from_set(rn_set, scheme_make_integer(1), 1);

  {
    Scheme_Object *insp;
    insp = scheme_make_inspector(env->insp);
    menv->insp = insp;
  }

  scheme_prepare_exp_env(menv);
  
  /* For each provide in iim, add a module rename to fm */
  saw_mb = add_simple_require_renames(NULL, rn_set, NULL, iim, iidx, scheme_make_integer(0), NULL, 1);

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

  if (check_mb) {
    SCHEME_EXPAND_OBSERVE_TAG(rec[drec].observer, fm);
  }

  fm = scheme_stx_property(fm, module_name_symbol, SCHEME_PTR_VAL(m->modname));

  /* phase shift to replace self_modidx of previous expansion (if any): */
  fm = scheme_stx_phase_shift(fm, 0, empty_self_modidx, self_modidx, NULL);

  fm = scheme_add_rename(fm, rn_set);

  SCHEME_EXPAND_OBSERVE_RENAME_ONE(rec[drec].observer, fm);

  if (!check_mb) {

    fm = scheme_check_immediate_macro(fm, benv, rec, drec, 0, &mbval, NULL, NULL);

    /* If expansion is not the primitive `#%module-begin', add local one: */
    if (!SAME_OBJ(mbval, modbeg_syntax)) {
      Scheme_Object *mb;
      mb = scheme_datum_to_syntax(module_begin_symbol, form, scheme_false, 0, 0);
      fm = scheme_make_pair(mb, scheme_make_pair(fm, scheme_null));
      fm = scheme_datum_to_syntax(fm, form, form, 0, 2);
      fm = scheme_stx_property(fm, module_name_symbol, SCHEME_PTR_VAL(m->modname));
      /* Since fm is a newly-created syntax object, we need to re-add renamings: */
      fm = scheme_add_rename(fm, rn_set);
      
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

    pv = scheme_stx_property(form, scheme_intern_symbol("module-language"), NULL);
    if (pv && SCHEME_TRUEP(pv)) {
      if (SCHEME_VECTORP(pv)
          && (3 == SCHEME_VEC_SIZE(pv))
          && scheme_is_module_path(SCHEME_VEC_ELS(pv)[0])
          && SCHEME_SYMBOLP(SCHEME_VEC_ELS(pv)[1]))
        m->lang_info = pv;
    }

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
			       empty_self_modidx);
    }

    /* for future expansion, shift away from self_modidx: */
    fm = scheme_stx_phase_shift(fm, 0, self_modidx, empty_self_modidx, NULL);

    /* make self_modidx like the empty modidx */
    ((Scheme_Modidx *)self_modidx)->resolved = empty_self_modname;
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
  Scheme_Dynamic_State dyn_state;

  rhs_env = scheme_new_comp_env(env, NULL, SCHEME_TOPLEVEL_FRAME);

  scheme_set_dynamic_state(&dyn_state, rhs_env, NULL, scheme_false, NULL, 
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
                               Scheme_Object *nominal_export_phase, Scheme_Object *in_insp)
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
    char *fromsrc = NULL, *fromsrc_colon = "";
    long fromsrclen = 0;
    
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

      scheme_wrong_syntax_with_more_sources("module", prnt_name, err_src, srcs,
                                            "identifier already imported from%s %t",
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
  vec = scheme_make_vector(10, NULL);
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
  SCHEME_VEC_ELS(vec)[9] = in_insp;
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
    scheme_extend_module_rename(rn, self_modidx, name, name, self_modidx, name, 0, NULL, NULL, NULL, 0);

    id = scheme_add_rename(id, rn);
    new_ids = cons(id, new_ids);
  }

  new_ids = scheme_reverse(new_ids);
  *_ids = new_ids;

  return scheme_make_lifted_defn(scheme_sys_wraps(env), _ids, expr, _env);
}

static Scheme_Object *make_require_form(Scheme_Object *module_path, long phase, Scheme_Object *mark)
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
                                           long phase,
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

  e = make_require_form(module_path, phase, mark);

  parse_requires(e, base_modidx, env, for_m,
                 rns, post_ex_rns,
                 check_require_name, tables,
                 redef_modname, 
                 0, 0, 1, 
                 1, 0,
                 all_simple);

  return e;
}

static Scheme_Object *package_require_data(Scheme_Object *base_modidx,
                                           Scheme_Env *env,
                                           Scheme_Module *for_m,
                                           Scheme_Object *rns, Scheme_Object *post_ex_rns,
                                           void *data,
                                           Scheme_Object *redef_modname,
                                           int *all_simple)
{
  void **vals;

  vals = MALLOC_N(void*, 9);
  vals[0] = NULL; /* this slot is available */
  vals[1] = base_modidx;
  vals[2] = env;
  vals[3] = for_m;
  vals[4] = rns;
  vals[5] = post_ex_rns;
  vals[6] = data;
  vals[7] = redef_modname;
  vals[8] = all_simple;

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

static Scheme_Object *do_module_begin(Scheme_Object *form, Scheme_Comp_Env *env, 
				      Scheme_Compile_Expand_Info *rec, int drec)
{
  Scheme_Object *fm, *first, *last, *p, *rn_set, *rn, *exp_body, *et_rn, *self_modidx, *prev_p;
  Scheme_Comp_Env *xenv, *cenv, *rhs_env;
  Scheme_Hash_Table *et_required; /* just to avoid duplicates */
  Scheme_Hash_Table *required;    /* name -> (vector nominal-modidx-list modidx srcname var? prntname) */
  /**/                            /*   first nominal-modidx goes with modidx, rest are for re-provides */
  Scheme_Hash_Table *provided;    /* exname -> (cons locname-stx-or-sym protected?) */
  Scheme_Hash_Table *all_reprovided; /* phase -> list of (list modidx syntax except-name ...) */
  Scheme_Object *all_defs_out;    /* list of (cons protected? (stx-list except-name ...)) */
  Scheme_Object *all_et_defs_out;
  Scheme_Hash_Table *all_provided; /* phase -> table like `provided' */
  Scheme_Object *all_defs;        /* list of stxid; this is almost redundant to the syntax and toplevel
				     tables, but it preserves the original name for exporting */
  Scheme_Object *all_et_defs;
  Scheme_Object *post_ex_rn, *post_ex_et_rn; /* renames for ids introduced by expansion */
  Scheme_Object *post_ex_rn_set; /* phase -> post_ex_rn-like rename */
  Scheme_Hash_Table *tables; /* phase -> (vector toplevels requires syntaxes) */
  Scheme_Object *lift_data;
  Scheme_Object **exis, **et_exis, **exsis;
  Scheme_Object *lift_ctx;
  Scheme_Object *lifted_reqs = scheme_null, *req_data;
  int exicount, et_exicount, exsicount;
  char *exps, *et_exps;
  int *all_simple_renames;
  int maybe_has_lifts = 0;
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
    scheme_add_local_syntax(19, xenv);
    scheme_set_local_syntax(0, scheme_begin_stx, stop, xenv);
    scheme_set_local_syntax(1, scheme_define_values_stx, stop, xenv);
    scheme_set_local_syntax(2, scheme_define_syntaxes_stx, stop, xenv);
    scheme_set_local_syntax(3, define_for_syntaxes_stx, stop, xenv);
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
  }

  first = scheme_null;
  last = NULL;

  rn_set = env->genv->rename_set;
  rn = scheme_get_module_rename_from_set(rn_set, scheme_make_integer(0), 1);
  et_rn = scheme_get_module_rename_from_set(rn_set, scheme_make_integer(1), 1);

  required = scheme_make_hash_table(SCHEME_hash_ptr);
  et_required = scheme_make_hash_table(SCHEME_hash_ptr);

  tables = scheme_make_hash_table_equal();
  {
    Scheme_Object *vec;

    vec = scheme_make_vector(3, NULL);
    SCHEME_VEC_ELS(vec)[0] = (Scheme_Object *)env->genv->toplevel;
    SCHEME_VEC_ELS(vec)[1] = (Scheme_Object *)required;
    SCHEME_VEC_ELS(vec)[2] = (Scheme_Object *)env->genv->syntax;
    scheme_hash_set(tables, scheme_make_integer(0), vec);

    vec = scheme_make_vector(3, NULL);
    SCHEME_VEC_ELS(vec)[0] = (Scheme_Object *)env->genv->exp_env->toplevel;
    SCHEME_VEC_ELS(vec)[1] = (Scheme_Object *)et_required;
    SCHEME_VEC_ELS(vec)[2] = NULL;
    scheme_hash_set(tables, scheme_make_integer(1), vec);
  }

  /* Put initial requires into the table:
     (This is redundant for the rename set, but we need to fill
     the `all_requires' table, etc.) */
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

    add_simple_require_renames(orig_src, rn_set, tables, 
                               iim, nmidx,
                               scheme_make_integer(0),
                               NULL, 1);
  }

  {
    Scheme_Object *v;
    v = scheme_rename_to_stx(rn_set);
    env->genv->module->rn_stx = v;
  }

  provided = scheme_make_hash_table(SCHEME_hash_ptr);
  all_provided = scheme_make_hash_table_equal();
  scheme_hash_set(all_provided, scheme_make_integer(0), (Scheme_Object *)provided);

  all_reprovided = scheme_make_hash_table_equal();

  all_defs_out = scheme_null;
  all_et_defs_out = scheme_null;

  all_defs = scheme_null;
  all_et_defs = scheme_null;

  exp_body = scheme_null;

  self_modidx = env->genv->module->self_modidx;

  post_ex_rn_set = scheme_make_module_rename_set(mzMOD_RENAME_MARKED, rn_set);
  post_ex_rn = scheme_get_module_rename_from_set(post_ex_rn_set, scheme_make_integer(0), 1);
  post_ex_et_rn = scheme_get_module_rename_from_set(post_ex_rn_set, scheme_make_integer(1), 1);
  env->genv->post_ex_rename_set = post_ex_rn_set;

  /* For syntax-local-context, etc., in a d-s RHS: */
  rhs_env = scheme_new_comp_env(env->genv, env->insp, SCHEME_TOPLEVEL_FRAME);

  scheme_rec_add_certs(rec, drec, form);

  observer = rec[drec].observer;

  /* It's possible that #%module-begin expansion introduces
     marked identifiers for definitions. */
  form = scheme_add_rename(form, post_ex_rn_set);
  SCHEME_EXPAND_OBSERVE_RENAME_ONE(observer, form);

  maybe_has_lifts = 0;
  lift_ctx = scheme_generate_lifts_key();

  all_simple_renames = (int *)scheme_malloc_atomic(sizeof(int));
  *all_simple_renames = 1;

  req_data = package_require_data(self_modidx, env->genv, env->genv->module,
                                  rn_set, post_ex_rn_set,
                                  tables,
                                  redef_modname, 
                                  all_simple_renames);

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
           : scheme_null);
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
	erec1.certs = rec[drec].certs;
        erec1.observer = rec[drec].observer;
        erec1.pre_unwrapped = 0;
        erec1.no_module_cert = 0;
        erec1.env_already = 0;
        erec1.comp_flags = rec[drec].comp_flags;
	e = scheme_expand_expr(e, xenv, &erec1, 0);	
      }

      lifted_reqs = scheme_append(scheme_frame_get_require_lifts(xenv), lifted_reqs);

      fst = scheme_frame_get_lifts(xenv);
      if (!SCHEME_NULLP(fst)) {
	/* Expansion lifted expressions, so add them to
	   the front and try again. */
        *all_simple_renames = 0;
	fm = SCHEME_STX_CDR(fm);
        e = scheme_add_rename(e, post_ex_rn_set);
        fm = scheme_named_map_1(NULL, add_a_rename, fm, post_ex_rn_set);
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
	
	if (fst && SCHEME_STX_SYMBOLP(fst) && scheme_stx_module_eq(scheme_begin_stx, fst, 0)) {
	  fm = SCHEME_STX_CDR(fm);
	  e = scheme_add_rename(e, post_ex_rn_set);
          SCHEME_EXPAND_OBSERVE_RENAME_ONE(observer, e);
	  fm = scheme_flatten_begin(e, fm);
	  SCHEME_EXPAND_OBSERVE_SPLICE(observer, fm);
	  if (SCHEME_STX_NULLP(fm)) {
            e = scheme_frame_get_provide_lifts(xenv);
            e = scheme_reverse(e);
            fm = scheme_frame_get_end_statement_lifts(xenv);
            fm = scheme_reverse(fm);
            if (!SCHEME_NULLP(e))
              fm = scheme_append(fm, e);
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

    e = scheme_add_rename(e, post_ex_rn_set);

    SCHEME_EXPAND_OBSERVE_RENAME_ONE(observer, e);
    
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
	    
	    name = scheme_tl_id_sym(env->genv, name, NULL, 2, NULL, NULL);

	    /* Check that it's not yet defined: */
	    if (scheme_lookup_in_table(env->genv->toplevel, (const char *)name)) {
	      scheme_wrong_syntax("module", orig_name, e, "duplicate definition for identifier");
	      return NULL;
	    }

	    /* Not required: */
	    if (check_already_required(required, name)) {
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
	    if (!SAME_OBJ(SCHEME_STX_VAL(orig_name), name)) {
	      scheme_extend_module_rename(post_ex_rn, self_modidx, name, name, self_modidx, name, 0, NULL, NULL, NULL, 0);
              *all_simple_renames = 0;
	    } else
	      scheme_extend_module_rename(rn, self_modidx, name, name, self_modidx, name, 0, NULL, NULL, NULL, 0);

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
          int use_post_ex = 0;

	  for_stx = scheme_stx_module_eq(define_for_syntaxes_stx, fst, 0);

          SCHEME_EXPAND_OBSERVE_ENTER_PRIM(observer, e);
          SCHEME_EXPAND_OBSERVE_PRIM_DEFINE_SYNTAXES(observer);

	  scheme_define_parse(e, &names, &code, 1, env, 1);

	  if (SCHEME_STX_PAIRP(names) && SCHEME_STX_NULLP(SCHEME_STX_CDR(names)))
	    boundname = SCHEME_STX_CAR(names);
	  else
	    boundname = scheme_false;
	  
	  scheme_prepare_exp_env(env->genv);
	  scheme_prepare_compile_env(env->genv->exp_env);
	  eenv = scheme_new_comp_env(env->genv->exp_env, env->insp, 0);
          scheme_frame_captures_lifts(eenv, NULL, NULL, scheme_false, scheme_false, 
                                      req_data, scheme_false);

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
	    
	    name = scheme_tl_id_sym(oenv->genv, name, NULL, 2, NULL, NULL);
	    
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
	    if (check_already_required(for_stx ? et_required : required, name)) {
	      scheme_wrong_syntax("module", orig_name, e, 
				  (for_stx
				   ? "identifier is already imported for syntax"
				   : "identifier is already imported"));
	      return NULL;
	    }

	    if (!SAME_OBJ(SCHEME_STX_VAL(orig_name), name)) {
	      scheme_extend_module_rename(for_stx ? post_ex_et_rn : post_ex_rn, self_modidx, name, name, self_modidx, name,
					  for_stx ? 1 : 0, NULL, NULL, NULL, 0);
              *all_simple_renames = 0;
              use_post_ex = 1;
	    } else
	      scheme_extend_module_rename(for_stx ? et_rn : rn, self_modidx, name, name, self_modidx, name,
					  for_stx ? 1 : 0, NULL, NULL, NULL, 0);

	    count++;
	  }

	  names = scheme_named_map_1(NULL, stx_sym, names, (Scheme_Object *)oenv->genv);
	  
	  mrec.comp = 1;
	  mrec.dont_mark_local_use = 0;
	  mrec.resolve_module_ids = 0;
          mrec.no_module_cert = 0;
	  mrec.value_name = NULL;
	  mrec.certs = rec[drec].certs;
          mrec.observer = NULL;
          mrec.pre_unwrapped = 0;
          mrec.env_already = 0;
          mrec.comp_flags = rec[drec].comp_flags;
          scheme_rec_add_certs(&mrec, 0, e);

	  if (!rec[drec].comp) {
	    Scheme_Expand_Info erec1;
	    erec1.comp = 0;
	    erec1.depth = -1;
	    erec1.value_name = boundname;
	    erec1.certs = mrec.certs;
            erec1.observer = rec[drec].observer;
            erec1.pre_unwrapped = 0;
            erec1.no_module_cert = 0;
            erec1.env_already = 0;
            erec1.comp_flags = rec[drec].comp_flags;
	    SCHEME_EXPAND_OBSERVE_PHASE_UP(observer);
	    code = scheme_expand_expr_lift_to_let(code, eenv, &erec1, 0);
	  }
	  m = scheme_compile_expr_lift_to_let(code, eenv, &mrec, 0);

          lifted_reqs = scheme_append(scheme_frame_get_require_lifts(eenv), lifted_reqs);

	  oi = scheme_optimize_info_create();
          oi->context = (Scheme_Object *)env->genv->module;
          if (!(rec[drec].comp_flags & COMP_CAN_INLINE))
            oi->inline_fuel = -1;
	  m = scheme_optimize_expr(m, oi, 0);
	  
	  /* Simplify only in compile mode; it is too slow in expand mode. */
	  rp = scheme_resolve_prefix(1, eenv->prefix, rec[drec].comp);
	  ri = scheme_resolve_info_create(rp);
          scheme_enable_expression_resolve_lifts(ri);
	  m = scheme_resolve_expr(m, ri);
          m = scheme_merge_expression_resolve_lifts(m, rp, ri);
          rp = scheme_remap_prefix(rp, ri);

	  /* Add code with names and lexical depth to exp-time body: */
	  vec = scheme_make_vector(5, NULL);
	  SCHEME_VEC_ELS(vec)[0] = ((SCHEME_PAIRP(names) && SCHEME_NULLP(SCHEME_CDR(names)))
                                    ? SCHEME_CAR(names)
                                    : names);
	  SCHEME_VEC_ELS(vec)[1] = m;
	  SCHEME_VEC_ELS(vec)[2] = scheme_make_integer(ri->max_let_depth);
	  SCHEME_VEC_ELS(vec)[3] = (Scheme_Object *)rp;
	  SCHEME_VEC_ELS(vec)[4] = (for_stx ? scheme_true : scheme_false);
	  exp_body = scheme_make_pair(vec, exp_body);

          m = scheme_sfs(m, NULL, ri->max_let_depth);
	  if (ri->use_jit)
	    m = scheme_jit_expr(m);
          rp = scheme_prefix_eval_clone(rp);
	
	  eval_exptime(names, count, m, eenv->genv, rhs_env, rp, ri->max_let_depth, 0, 
                       (for_stx ? env->genv->exp_env->toplevel : env->genv->syntax), for_stx,
                       rec[drec].certs, 
                       for_stx ? scheme_false : (use_post_ex ? post_ex_rn : rn));
          
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
          SCHEME_EXPAND_OBSERVE_ENTER_PRIM(observer, e);
          SCHEME_EXPAND_OBSERVE_PRIM_REQUIRE(observer);

	  /* Adds requires to renamings and required modules to requires lists: */
	  parse_requires(e, self_modidx, env->genv, env->genv->module,
                         rn_set, post_ex_rn_set,
                         check_require_name, tables,
                         redef_modname, 
                         0, 0, 1, 
                         1, 0,
                         all_simple_renames);

	  if (rec[drec].comp)
	    e = NULL;

          SCHEME_EXPAND_OBSERVE_EXIT_PRIM(observer, e);
	  kind = 0;
	} else if (scheme_stx_module_eq(provide_stx, fst, 0)) {
	  /************ provide *************/
          /* remember it for the second pass */
          kind = 3;
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
      e = scheme_frame_get_provide_lifts(xenv);
      e = scheme_reverse(e);
      fm = scheme_frame_get_end_statement_lifts(xenv);
      fm = scheme_reverse(fm);
      if (!SCHEME_NULLP(e))
        fm = scheme_append(fm, e);
      SCHEME_EXPAND_OBSERVE_MODULE_LIFT_END_LOOP(observer, fm);
      maybe_has_lifts = 0;
    }
  }
  /* first =  a list of (cons semi-expanded-expression kind) */

  /* Bound names will not be re-bound at this point: */
  if (rec[drec].comp || (rec[drec].depth != -2)) {
    scheme_seal_module_rename_set(rn_set, STX_SEAL_BOUND);
  }
  scheme_seal_module_rename_set(post_ex_rn_set, STX_SEAL_BOUND);

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

    if (kind == 3) {
      Scheme_Object *fst;

      fst = SCHEME_STX_CAR(e);

      if (scheme_stx_module_eq(provide_stx, fst, 0)) {
        /************ provide *************/
        /* Add provides to table: */
        Scheme_Object *ex;

        SCHEME_EXPAND_OBSERVE_ENTER_PRIM(observer, e);
        SCHEME_EXPAND_OBSERVE_PRIM_PROVIDE(observer);
      
        ex = e;
  
        parse_provides(form, fst, e, 
                       all_provided, all_reprovided,
                       self_modidx,
                       &all_defs_out, &all_et_defs_out,
                       tables,
                       all_defs, all_et_defs, cenv, rec, drec,
                       &ex);
        
        e = ex;

        SCHEME_EXPAND_OBSERVE_EXIT_PRIM(observer, e);
      }
      if (!rec[drec].comp) {
        SCHEME_CAR(p) = e;
        prev_p = p;
        p = SCHEME_CDR(p);
      } else {
        p = SCHEME_CDR(p);
        if (!prev_p)
          first = p;
        else
          SCHEME_CDR(prev_p) = p;
      }
    } else if (kind) {
      Scheme_Comp_Env *nenv;

      l = (maybe_has_lifts 
           ? scheme_frame_get_end_statement_lifts(cenv) 
           : scheme_null);
      ll = (maybe_has_lifts 
            ? scheme_frame_get_provide_lifts(cenv) 
            : scheme_null);
      scheme_frame_captures_lifts(cenv, add_lifted_defn, lift_data, l, lift_ctx, req_data, ll);
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

      lifted_reqs = scheme_append(scheme_frame_get_require_lifts(cenv), lifted_reqs);
      
      l = scheme_frame_get_lifts(cenv);
      if (SCHEME_NULLP(l)) {
	/* No lifts - continue normally */
	SCHEME_CAR(p) = e;
	prev_p = p;
	p = SCHEME_CDR(p);
      } else {
	/* Lifts - insert them and try again */
        *all_simple_renames = 0;
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
      int expr_cnt;
      e = scheme_frame_get_provide_lifts(cenv);
      e = scheme_reverse(e);
      p = scheme_frame_get_end_statement_lifts(cenv);
      p = scheme_reverse(p);
      expr_cnt = scheme_list_length(p);
      if (!SCHEME_NULLP(e))
        p = scheme_append(p, e);
      SCHEME_EXPAND_OBSERVE_MODULE_LIFT_END_LOOP(observer, p);
      for (ll = p; SCHEME_PAIRP(ll); ll = SCHEME_CDR(ll)) {
        e = scheme_make_pair(SCHEME_CAR(ll), (expr_cnt > 0) ? scheme_make_integer(1) : scheme_make_integer(3));
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
  /* first = a list of expanded/compiled expressions */

  /* If compiling, drop expressions that are constants: */
  if (rec[drec].comp) {
    Scheme_Object *prev = NULL, *next;
    for (p = first; !SCHEME_NULLP(p); p = next) {
      next = SCHEME_CDR(p);
      if (scheme_omittable_expr(SCHEME_CAR(p), -1, -1, 0, NULL)) {
	if (prev)
	  SCHEME_CDR(prev) = next;
	else
	  first = next;
      } else
	prev = p;
    }
  }

  if (rec[drec].comp || (rec[drec].depth != -2)) {
    scheme_seal_module_rename_set(rn_set, STX_SEAL_ALL);
  }
  scheme_seal_module_rename_set(post_ex_rn_set, STX_SEAL_ALL);

  /* Compute provides for re-provides and all-defs-out: */
  (void)compute_reprovides(all_provided,
                           all_reprovided, 
                           env->genv->module, 
                           tables,
                           env->genv, 
                           all_defs, all_defs_out, 
                           all_et_defs, all_et_defs_out, 
                           "require", NULL, NULL);

  /* Compute provide arrays */
  exps = compute_provide_arrays(all_provided, tables,
                                env->genv->module->me,
                                env->genv,
                                form, &et_exps);
  
  /* Compute indirect provides (which is everything at the top-level): */
  exis = compute_indirects(env->genv, env->genv->module->me->rt, &exicount, 1);
  exsis = compute_indirects(env->genv, env->genv->module->me->rt, &exsicount, 0);
  et_exis = compute_indirects(env->genv->exp_env, env->genv->module->me->et, &et_exicount, 1);

  if (rec[drec].comp || (rec[drec].depth != -2)) {
    scheme_clean_dead_env(env->genv);
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

    /* kernel re-export info (always #f): */
    result = scheme_make_pair(scheme_false, result);

    /* Indirect provides */ 
    a = scheme_null;
    for (j = 0; j < exicount; j++) {
      a = scheme_make_pair(exis[j], a);
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

  if (rec[drec].comp) {
    Scheme_Object *exp_body_r = scheme_null;
    
    /* Reverse exp_body */
    while (!SCHEME_NULLP(exp_body)) {
      exp_body_r = scheme_make_pair(SCHEME_CAR(exp_body),
				    exp_body_r);
      exp_body = SCHEME_CDR(exp_body);
    }

    first = scheme_list_to_vector(first);
    env->genv->module->body = first;
    exp_body_r = scheme_list_to_vector(exp_body_r);
    env->genv->module->et_body = exp_body_r;

    env->genv->module->provide_protects = exps;
    env->genv->module->et_provide_protects = et_exps;

    env->genv->module->indirect_provides = exis;
    env->genv->module->num_indirect_provides = exicount;

    if (*all_simple_renames) {
      env->genv->module->indirect_syntax_provides = exsis;
      env->genv->module->num_indirect_syntax_provides = exsicount;
    } else {
      env->genv->module->indirect_syntax_provides = NULL;
      env->genv->module->num_indirect_syntax_provides = 0;
    }

    env->genv->module->et_indirect_provides = et_exis;
    env->genv->module->num_indirect_et_provides = et_exicount;

    env->genv->module->comp_prefix = cenv->prefix;

    if (*all_simple_renames) {
      env->genv->module->rn_stx = scheme_true;
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

    /* Add lifted requires */
    if (!SCHEME_NULLP(lifted_reqs)) {
      lifted_reqs = scheme_reverse(lifted_reqs);
      first = scheme_append(lifted_reqs, first);
    }

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
                       Scheme_Object *all_rt_defs, Scheme_Object *all_rt_defs_out, 
                       Scheme_Object *all_et_defs, Scheme_Object *all_et_defs_out, 
                       const char *matching_form,
                       Scheme_Object *all_mods, /* a phase list to use for all mods */
                       Scheme_Object *all_phases) /* a module-path list for all phases */
{
  Scheme_Hash_Table *provided, *required;
  Scheme_Object *reprovided, *tvec;
  int i, k, z;
  Scheme_Object *rx, *provided_list, *phase, *req_phase;
  Scheme_Object *all_defs, *all_defs_out;
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
              Scheme_Object *nominal_modidx, *name, *modidx, *srcname, *outname, *nml, *orig_nml, *mark_src;
              int break_outer = 0;
	
              name = required->keys[i]; /* internal symbolic name */
              orig_nml = SCHEME_VEC_ELS(required->vals[i])[0];
              modidx = SCHEME_VEC_ELS(required->vals[i])[1];
              srcname = SCHEME_VEC_ELS(required->vals[i])[2];
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
  for (z = 0; z < 2; z++) {
    if (!z) {
      all_defs = all_rt_defs;
      all_defs_out = all_rt_defs_out;
      provided = (Scheme_Hash_Table *)scheme_hash_get(all_provided, scheme_make_integer(0));
      phase = scheme_make_integer(0);
      genv = _genv;
    } else {
      all_defs = all_et_defs;
      all_defs_out = all_et_defs_out;
      provided = (Scheme_Hash_Table *)scheme_hash_get(all_provided, scheme_make_integer(1));
      phase = scheme_make_integer(1);
      genv = _genv->exp_env;
    }

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
          name = scheme_tl_id_sym(genv, a, NULL, 0, NULL, NULL);
          if (!scheme_lookup_in_table(genv->toplevel, (const char *)name)
              && !scheme_lookup_in_table(genv->syntax, (const char *)name)) {
            scheme_wrong_syntax("module", a, ree_kw, "excluded identifier was not defined");
          }
        }

        for (adl = all_defs; SCHEME_PAIRP(adl); adl = SCHEME_CDR(adl)) {
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

  qsort_provides(exis, NULL, NULL, NULL, NULL, NULL, NULL, 0, exicount, 1);

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
                         NULL, NULL, NULL, NULL, 
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
                                           Scheme_Object **_implicit_nominal_mod,
                                           Scheme_Object **_implicit_insp)
{
  *_implicit = 0;

  while (1) { /* loop for free-id=? renaming */
    if (SCHEME_STXP(name)) {
      if (genv
          && (always
              || SAME_OBJ(phase, scheme_make_integer(0))
              || SAME_OBJ(phase, scheme_make_integer(1))))
        name = scheme_tl_id_sym(genv, name, NULL, -1, phase, NULL);
      else
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
                                     NULL, NULL, NULL, NULL, &rename_insp);
        if (_implicit_mod_phase) *_implicit_mod_phase = mod_phase;
          
        if (mod && SAME_TYPE(SCHEME_TYPE(mod), scheme_module_index_type)) {
          if (SCHEME_FALSEP(((Scheme_Modidx *)mod)->path)) {
            /* keep looking locally */
            name = name2;
            SCHEME_USE_FUEL(1);
          } else {
            /* free-id=? equivalence to a name that is not necessarily imported explicitly. */
            int would_complain = 0, is_prot = 0, is_unexp = 0;

            if (!SCHEME_FALSEP(phase)) {
              /* Check whether reference is certified, and ignore it if not: */
              Scheme_Env *menv;
              Scheme_Object *modname;
              
              modname = scheme_module_resolve(mod, 1);
              menv = scheme_module_access(modname, genv, SCHEME_INT_VAL(mod_phase));
              if (!menv)
                would_complain = 1;
              else {
                scheme_check_accessible_in_module(menv, menv->module->insp, mod, 
                                                  SCHEME_STX_VAL(name2), name2, 
                                                  NULL, NULL, rename_insp,
                                                  -1, 0, 
                                                  &is_prot, &is_unexp, genv, &would_complain);
                if (would_complain && (!is_prot && !is_unexp)) {
                  /* Must be unexported syntax */
                  is_prot = is_unexp = would_complain = 0;
                  scheme_check_accessible_in_module(menv, menv->module->insp, mod, 
                                                    SCHEME_STX_VAL(name2), name2, 
                                                    NULL, NULL, rename_insp,
                                                    -2, 0, 
                                                    &is_prot, &is_unexp, genv, &would_complain);
                }
              }
            }


            if (!would_complain) {
              if (_implicit_src) {
                *_implicit_src = mod;
                *_implicit_src_name = id;
                if (is_prot || is_unexp) {
                  if (rename_insp)
                    *_implicit_insp = rename_insp;
                  else
                    *_implicit_insp = genv->module->insp;
                }
                name2 = scheme_stx_property(name2, nominal_id_symbol, NULL);
                if (SCHEME_SYMBOLP(name2))
                  *_implicit_nominal_name = name2;
              }
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

char *compute_provide_arrays(Scheme_Hash_Table *all_provided, Scheme_Hash_Table *tables,
                             Scheme_Module_Exports *me,
                             Scheme_Env *genv,
                             Scheme_Object *form,
                             char **_phase1_protects)
{
  int i, count, z, implicit;
  Scheme_Object **exs, **exsns, **exss, **exsnoms, **exinsps, *phase;
  Scheme_Hash_Table *provided, *required;
  char *exps, *exets, *phase0_exps = NULL, *phase1_exps = NULL;
  int excount, exvcount;
  Scheme_Module_Phase_Exports *pt;
  Scheme_Object *implicit_src, *implicit_src_name, *implicit_mod_phase;
  Scheme_Object *implicit_nominal_name, *implicit_nominal_mod;
  Scheme_Object *implicit_insp;

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
      exinsps = MALLOC_N(Scheme_Object *, count);
      exps = MALLOC_N_ATOMIC(char, count);
      exets = MALLOC_N_ATOMIC(char, count);
      memset(exets, 0, count);

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
                                      NULL, NULL, NULL);

          if (!implicit
              && genv 
              && (SAME_OBJ(phase, scheme_make_integer(0))
                  || SAME_OBJ(phase, scheme_make_integer(1)))
              && scheme_lookup_in_table(SAME_OBJ(phase, scheme_make_integer(0))
                                        ? genv->toplevel
                                        : genv->exp_env->toplevel,
                                        (const char *)name)) {
            /* Defined locally */
            exs[count] = provided->keys[i];
            exsns[count] = name;
            exss[count] = scheme_false; /* means "self" */
            exsnoms[count] = scheme_null; /* since "self" */
            exps[count] = protected;
            if (SAME_OBJ(phase, scheme_make_integer(1)))
              exets[count] = 1;
            count++;
          } else if (!implicit
                     && genv 
                     && SAME_OBJ(phase, scheme_make_integer(0))
                     && scheme_lookup_in_table(genv->syntax, (const char *)name)) {
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
              if (SAME_OBJ(SCHEME_VEC_ELS(v)[8], scheme_make_integer(1)))
                exets[count] = 1;              
              if (SCHEME_TRUEP(SCHEME_VEC_ELS(v)[9]))
                exinsps[count] = SCHEME_VEC_ELS(v)[9];

              count++;
            }
          } else {
            /* Not defined! */
            scheme_wrong_syntax("module", prnt_name, form, "provided identifier not defined or imported");
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
                                      &implicit_nominal_name, &implicit_nominal_mod,
                                      &implicit_insp);

          if (!implicit
              && genv 
              && SAME_OBJ(phase, scheme_make_integer(0))
              && scheme_lookup_in_table(genv->syntax, (const char *)name)) {
            /* Defined locally */
            exs[count] = provided->keys[i];
            exsns[count] = name;
            exss[count] = scheme_false; /* means "self" */
            exsnoms[count] = scheme_null; /* since "self" */
            exps[count] = protected;
            count++;
          } else if (implicit) {
            /* We record all free-id=?-based exprts as syntax, even though they may be values. */
            Scheme_Object *noms;
            exs[count] = provided->keys[i];
            exsns[count] = implicit_src_name;
            exss[count] = implicit_src;
            noms = adjust_for_rename(exs[count], implicit_nominal_name, cons(implicit_nominal_mod, scheme_null));
            exsnoms[count] = noms;
            exps[count] = protected;
            if (implicit_insp) {
              if (protected) {
                implicit_insp = cons(genv->insp, implicit_insp);
              }
              exinsps[count] = implicit_insp;
            }
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
              if (SCHEME_TRUEP(SCHEME_VEC_ELS(v)[9]))
                exinsps[count] = SCHEME_VEC_ELS(v)[9];
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

      /* Discard exinsps if there are no inspectors */
      for (i = 0; i < excount; i++) {
        if (exinsps[i])
          break;
      }
      if (i >= excount) {
        exinsps = NULL;
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
      qsort_provides(exs, exsns, exss, exps, exets, exsnoms, exinsps, 0, exvcount, 1);

      pt->num_provides = excount;
      pt->num_var_provides = exvcount;
      pt->provides = exs;
      pt->provide_src_names = exsns;
      pt->provide_srcs = exss;
      pt->provide_nominal_srcs = exsnoms;
      pt->provide_insps = exinsps;
      pt->provide_src_phases = exets;

      if (SAME_OBJ(phase, scheme_make_integer(0)))
        phase0_exps = exps;
      else if (SAME_OBJ(phase, scheme_make_integer(1)))
        phase1_exps = exps;
    }
  }

  *_phase1_protects = phase1_exps;
    
  return phase0_exps;
}

/* Helper: */
static void qsort_provides(Scheme_Object **exs, Scheme_Object **exsns, Scheme_Object **exss, 
                           char *exps, char *exets,
			   Scheme_Object **exsnoms, Scheme_Object **exinsps,
                           int start, int count, int do_uninterned)
{
  int i, j;
  Scheme_Object *tmp_ex, *tmp_exsn, *tmp_exs, *tmp_exsnom, *tmp_exinsp, *pivot;
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
        if (exsnoms) {
	  tmp_exsnom = exsnoms[i];

	  exsnoms[i] = exsnoms[j];

	  exsnoms[j] = tmp_exsnom;    
        }
        if (exinsps) {
          tmp_exinsp = exinsps[i];

	  exinsps[i] = exinsps[j];

	  exinsps[j] = tmp_exinsp;
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
    qsort_provides(exs, exsns, exss, exps, exets, exsnoms, exinsps, 0, j + 1, 0);
    qsort_provides(exs, exsns, exss, exps, exets, exsnoms, exinsps, j + 1, count - j - 1, 0);
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
          if (exinsps) {
            tmp_exinsp = exinsps[k];
            
            exinsps[k] = exinsps[j];
            
            exinsps[j] = tmp_exinsp;    
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
      qsort_provides(exs, exsns, exss, exps, exets, exsnoms, exinsps, start, j - start, 0);
      qsort_provides(exs, exsns, exss, exps, exets, exsnoms, exinsps, j, count - (j - start), 0);
    }
  }
}

static Scheme_Object *expand_provide(Scheme_Object *e, 
                                     Scheme_Hash_Table *tables,
                                     Scheme_Object *all_defs, Scheme_Object *all_et_defs,
                                     Scheme_Comp_Env *cenv, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Expand_Info erec1;
  Scheme_Object *b, *stop;
  Scheme_Comp_Env *xenv;
  
  xenv = scheme_new_compilation_frame(0, (SCHEME_CAPTURE_WITHOUT_RENAME 
					  | SCHEME_FOR_STOPS), 
				      cenv, NULL);
  stop = scheme_get_stop_expander();
  scheme_add_local_syntax(1, xenv);
  scheme_set_local_syntax(0, scheme_begin_stx, stop, xenv);

  b = scheme_make_pair((Scheme_Object *)tables,
                       scheme_make_pair(all_defs, all_et_defs));
  scheme_current_thread->current_local_bindings = b;
  
  scheme_init_expand_recs(rec, drec, &erec1, 1);
  erec1.value_name = scheme_false;
  erec1.depth = -1;

  e = scheme_expand_expr(e, xenv, &erec1, 0);
  
  scheme_current_thread->current_local_bindings = NULL;

  return e;
}

void parse_provides(Scheme_Object *form, Scheme_Object *fst, Scheme_Object *e, 
                    Scheme_Hash_Table *all_provided,
                    Scheme_Hash_Table *all_reprovided,
                    Scheme_Object *self_modidx, 
                    Scheme_Object **_all_defs_out,
                    Scheme_Object **_et_all_defs_out,
                    Scheme_Hash_Table *tables,
                    Scheme_Object *all_defs, Scheme_Object *all_et_defs,
                    Scheme_Comp_Env *cenv, Scheme_Compile_Info *rec, int drec,
                    Scheme_Object **_expanded)
{
  Scheme_Object *l, *rebuilt = scheme_null, *protect_stx = NULL;
  int protect_cnt = 0, mode_cnt = 0, expanded = 0;
  Scheme_Object *mode = scheme_make_integer(0), *mode_stx = NULL;
  Scheme_Object *all_defs_out;
  Scheme_Hash_Table *provided;
  Scheme_Object *phase;

  if (scheme_stx_proper_list_length(e) < 0)
    scheme_wrong_syntax(NULL, e, form, "bad syntax (" IMPROPER_LIST_FORM ")");
  
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
            scheme_wrong_syntax(NULL, a, e, "bad syntax (nested protect)");
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
                                 ? "bad syntax (nested `for-syntax')"
                                 : (SAME_OBJ(av, for_label_symbol)
                                    ? "bad syntax (nested `for-label')"
                                    : "bad syntax (nested `for-meta')")));
          
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
            protect_cnt += mode_cnt;
          a = SCHEME_STX_CAR(l);
        } else
          break;
      } else
        break;
    }

    if (SAME_OBJ(mode, scheme_make_integer(0)))
      all_defs_out = *_all_defs_out;
    else if (SAME_OBJ(mode, scheme_make_integer(1)))
      all_defs_out = *_et_all_defs_out;
    else
      all_defs_out = NULL;

    provided = (Scheme_Hash_Table *)scheme_hash_get(all_provided, mode);
    if (!provided) {
      provided = scheme_make_hash_table(SCHEME_hash_ptr);
      scheme_hash_set(all_provided, mode, (Scheme_Object *)provided);
    }
    phase = mode;

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
            scheme_wrong_syntax(NULL, a, e, "bad syntax (extra forms after one to expand)");
        } else {
          scheme_wrong_syntax(NULL, a, e, "bad syntax (missing form to expand)");
          return;
        }

        p = expand_provide(p, tables, all_defs, all_et_defs, cenv, rec, drec);

        /* Check for '(begin datum ...) result: */
        p = scheme_flatten_syntax_list(p, &islist);
        if (!islist)
          p = NULL;
        else if (SCHEME_NULLP(p))
          p = NULL;
        else {
          rest = SCHEME_CAR(p);
          if (!SCHEME_STX_SYMBOLP(rest)
              || !scheme_stx_module_eq(scheme_begin_stx, rest, 0)) {
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
          scheme_wrong_syntax(NULL, a, e, "bad syntax (internal name is not an identifier)");
        if (!SCHEME_STX_SYMBOLP(enm))
          scheme_wrong_syntax(NULL, a, e, "bad syntax (external name is not an identifier)");
        rest = SCHEME_STX_CDR(rest);
        if (!SCHEME_STX_NULLP(rest))
          scheme_wrong_syntax(NULL, a, e, "bad syntax (data following external name)");
		
        enm = SCHEME_STX_VAL(enm);
		
        check_already_provided(provided, enm, inm, protect_cnt, a, phase);
        /* Provide enm: */
        scheme_hash_set(provided, enm, scheme_make_pair(inm, protect_cnt ? scheme_true : scheme_false));
      } else if (SAME_OBJ(all_from_symbol, SCHEME_STX_VAL(fst))) {
        /* (all-from <modname>) */
        Scheme_Object *reprovided;
        
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
        
        if (!all_defs_out) {
          scheme_wrong_syntax(NULL, a, e, "no definitions at phase level %V",
                              mode);
        }

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

        if (!all_defs_out) {
          scheme_wrong_syntax(NULL, a, e, "no definitions at phase level %V",
                              mode);
        }
        
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
		
        if (!all_defs_out) {
          scheme_wrong_syntax(NULL, a, e, "no definitions at phase level %V",
                              mode);
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
        f = SCHEME_STX_CAR(mode_stx);
        a = scheme_make_pair(for_meta_symbol, 
                             scheme_make_pair(mode, 
                                              scheme_make_pair(a, scheme_null)));
        a = scheme_datum_to_syntax(a, mode_stx, mode_stx, 0, 0);
      }
      rebuilt = scheme_make_pair(a, rebuilt);
    }

    if (protect_cnt)
      --protect_cnt;

    if (SAME_OBJ(mode, scheme_make_integer(0)))
      *_all_defs_out = all_defs_out;
    else if (SAME_OBJ(mode, scheme_make_integer(1)))
      *_et_all_defs_out = all_defs_out;

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

/**********************************************************************/
/*                         top-level require                          */
/**********************************************************************/

void add_single_require(Scheme_Module_Exports *me, /* from module */
                        Scheme_Object *only_phase,
                        Scheme_Object *src_phase_index,
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
  Scheme_Object **exs, **exsns, **exss, *context_marks = scheme_null, **exinsps;
  char *exets;
  int has_context, save_marshal_info = 0;
  Scheme_Object *nominal_modidx, *one_exn, *prnt_iname, *name, *rn, *ename = orig_ename;
  Scheme_Hash_Table *orig_onlys;
  int k, skip_rename, do_copy_vars;
  
  if (mark_src) {
    /* Check whether there's context for this import (which
       leads to generated local names). */
    context_marks = scheme_stx_extract_marks(mark_src);
    has_context = !SCHEME_NULLP(context_marks);
    if (has_context) {
      if (all_simple)
	*all_simple = 0;
    }
  } else
    has_context = 0; /* computed later */

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

    if (pt) {
      if (SCHEME_FALSEP(pt->phase_index))
        to_phase = scheme_false;
      else if (SCHEME_FALSEP(src_phase_index))
        to_phase = scheme_false;
      else
        to_phase = scheme_bin_plus(pt->phase_index, src_phase_index);
    } else
      to_phase = NULL;

    if (pt) {
      one_exn = NULL;
    
      nominal_modidx = idx;

      if (single_rn)
        rn = single_rn;
      else
        rn = scheme_get_module_rename_from_set((has_context ? post_ex_rn_set : rn_set),
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
        scheme_extend_module_rename_with_shared(rn, idx, pt, pt->phase_index, src_phase_index, context_marks, 1);
        skip_rename = 1;
      } else
        skip_rename = 0;

      exs = pt->provides;
      exsns = pt->provide_src_names;
      exss = pt->provide_srcs;
      exets = pt->provide_src_phases;
      exinsps = pt->provide_insps;
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
          iname = scheme_tl_id_sym(orig_env, iname, scheme_false, skip_rename ? 3 : 2, to_phase, NULL);
          if (all_simple)
            *all_simple = 0;
        }

        if (ck)
          ck(prnt_iname, iname, nominal_modidx, exs[j], modidx, exsns[j], exets ? exets[j] : 0,
             (j < var_count), 
             data, cki, form, err_src, mark_src, to_phase, src_phase_index, pt->phase_index,
             exinsps ? exinsps[j] : scheme_false);

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
                                          exinsps ? exinsps[j] : NULL,
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

        /* The format of this data is checked in stxobj for unmarshaling
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
  Scheme_Object *orig_idx, *exns, *prefix, *idx, *name, *pt_phase, *src_phase_index, *marks;
  Scheme_Module_Exports *me;
  Scheme_Env *env;
  int share_all;

  idx = SCHEME_CAR(info);
  orig_idx = idx;
  info = SCHEME_CDR(info);
  pt_phase = SCHEME_CAR(info);
  info = SCHEME_CDR(info);

  if (SCHEME_PAIRP(info) && SCHEME_PAIRP(SCHEME_CAR(info))) {
    marks = SCHEME_CAR(info);
    info = SCHEME_CDR(info);
  } else
    marks = scheme_null;

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

  if (SAME_OBJ(kernel_modname, name)) {
    me = kernel->me;
  } else if (SAME_OBJ(unsafe_modname, name)) {
    me = scheme_get_unsafe_env()->module->me;
  } else if (SAME_OBJ(flfxnum_modname, name)) {
    me = scheme_get_flfxnum_env()->module->me;
  } else {
    if (!export_registry) {
      env = scheme_get_env(scheme_current_config());
      export_registry = env->export_registry;
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
      scheme_extend_module_rename_with_shared(rn, orig_idx, pt, pt->phase_index, src_phase_index, marks, 0);
    }
  } else {
    if (!SCHEME_NULLP(marks))
      scheme_signal_error("internal error: unexpected marks");

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

void parse_requires(Scheme_Object *form, 
                    Scheme_Object *base_modidx,
                    Scheme_Env *main_env,
                    Scheme_Module *for_m,
                    Scheme_Object *rn_set, Scheme_Object *post_ex_rn_set,
                    Check_Func ck, void *data,
                    Scheme_Object *redef_modname,
                    int unpack_kern, int copy_vars, int can_save_marshal, 
                    int eval_exp, int eval_run,
                    int *all_simple) 
{
  Scheme_Object *ll = form, *mode = scheme_make_integer(0), *just_mode = NULL, *x_mode, *x_just_mode;
  Scheme_Module *m;
  Scheme_Object *idxstx, *idx, *name, *i, *exns, *prefix, *iname, *ename, *aa, *aav;
  Scheme_Object *mark_src, *err_src;
  Scheme_Hash_Table *onlys;
  Scheme_Env *env;
  int skip_one, mode_cnt = 0, just_mode_cnt = 0;

  if (scheme_stx_proper_list_length(form) < 0)
    scheme_wrong_syntax(NULL, NULL, form, "bad syntax (" IMPROPER_LIST_FORM ")");
  
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

    if (SAME_OBJ(for_syntax_symbol, aav)
        || SAME_OBJ(for_template_symbol, aav)
        || SAME_OBJ(for_label_symbol, aav)
        || SAME_OBJ(for_meta_symbol, aav)
        || SAME_OBJ(just_meta_symbol, aav)) {
      if (!SAME_OBJ(just_meta_symbol, aav)) {
        if (mode_cnt)
          scheme_wrong_syntax(NULL, i, form, 
                              (SAME_OBJ(for_syntax_symbol, aav)
                               ? "bad syntax (nested `for-syntax')"
                               : (SAME_OBJ(for_template_symbol, aav)
                                  ? "bad syntax (nested `for-template')"
                                  : (SAME_OBJ(for_label_symbol, aav)
                                     ? "bad syntax (nested `for-label')"
                                     : "bad syntax (nested `for-meta')"))));
      } else {
        if (just_mode_cnt)
          scheme_wrong_syntax(NULL, i, form, "bad syntax (nested `just-meta')");
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
        if (SAME_OBJ(for_meta_symbol, aav)) 
          mode = a_mode;
        else
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
	  reason = "bad syntax (" IMPROPER_LIST_FORM ")";
	else if (len < 2)
	  reason = "bad syntax (prefix missing)";
	else if (len < 3)
	  reason = "bad syntax (module name missing)";
	else
	  reason = "bad syntax (extra data after module name)";
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
			      "bad syntax (excluded name is not an identifier)");
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
	  reason = "bad syntax (" IMPROPER_LIST_FORM ")";
	else
	  reason = "bad syntax (module name missing)";
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
	return;
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
      
      idx = scheme_make_modidx(scheme_syntax_to_datum(idxstx, 0, NULL), 
                               base_modidx,
                               scheme_false);

      name = _module_resolve(idx, idxstx, NULL, 1);

      m = module_load(name, env, NULL);

      start_module(m, env, 0, idx, 
                   start ? eval_exp : 0, start ? eval_run : 0, 
                   main_env->phase, scheme_null);

      /* Add name to require list, if it's not there: */
      if (main_env->module) {
        Scheme_Object *reqs;
        if (SAME_OBJ(mode, scheme_make_integer(0))) {
          reqs = add_req(scheme_make_pair(idx, scheme_null), main_env->module->requires);
          main_env->module->requires = reqs;
        } else if (SAME_OBJ(mode, scheme_make_integer(1))) {
          reqs = add_req(scheme_make_pair(idx, scheme_null), main_env->module->et_requires);
          main_env->module->et_requires = reqs;
        } else if (SAME_OBJ(mode, scheme_make_integer(-1))) {
          reqs = add_req(scheme_make_pair(idx, scheme_null), main_env->module->tt_requires);
          main_env->module->tt_requires = reqs;
        } else if (SAME_OBJ(mode, scheme_false)) {
          reqs = add_req(scheme_make_pair(idx, scheme_null), main_env->module->dt_requires);
          main_env->module->dt_requires = reqs;
        } else {
          Scheme_Hash_Table *oht;
          oht = main_env->module->other_requires;
          if (!oht) {
            oht = scheme_make_hash_table_equal();
            main_env->module->other_requires = oht;
          }
          reqs = scheme_hash_get(oht, mode);
          if (!reqs)
            reqs = scheme_null;
          reqs = add_req(scheme_make_pair(idx, scheme_null), reqs);
          scheme_hash_set(oht, mode, reqs);
        }
      }

      x_just_mode = just_mode;
      x_mode = mode;
      if (main_env->phase) {
        /* We get here only via `eval' or `namespace-require'. */
        if (x_just_mode && SCHEME_TRUEP(x_just_mode)) {
          x_just_mode = scheme_bin_plus(x_just_mode, scheme_make_integer(main_env->phase));
        }
        if (x_mode && SCHEME_TRUEP(x_mode)) {
          x_mode = scheme_bin_plus(x_mode, scheme_make_integer(main_env->phase));
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
                              Scheme_Object *nominal_export_phase, Scheme_Object *in_insp)
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
  Scheme_Object *rest;

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

  rn_set = scheme_make_module_rename_set(mzMOD_RENAME_TOPLEVEL, NULL);

  if (rest) {
    ht = scheme_make_hash_table_equal();
  } else {
    ht = NULL;
  }

  parse_requires(form, modidx, env, NULL,
                 rn_set, rn_set,
                 check_dup_require, ht,
                 NULL,
                 !env->module, 0, 0, 
                 -1, 1,
                 NULL);

  scheme_append_rename_set_to_env(rn_set, env);

  return scheme_void;
}

static Scheme_Object *
top_level_require_execute(Scheme_Object *data)
{
  do_require_execute(scheme_environment_from_dummy(SCHEME_CAR(data)),
                     SCHEME_CDR(data));
  return scheme_void;
}

static Scheme_Object *
top_level_require_jit(Scheme_Object *data)
{
  return data;
}

static void top_level_require_validate(Scheme_Object *data, Mz_CPort *port, 
                                       char *stack, Validate_TLS tls,
				       int depth, int letlimit, int delta, 
				       int num_toplevels, int num_stxes, int num_lifts,
                                       struct Validate_Clearing *vc, int tailpos)
{
}

static Scheme_Object *
top_level_require_optimize(Scheme_Object *data, Optimize_Info *info, int context)
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

static Scheme_Object *
top_level_require_sfs(Scheme_Object *data, SFS_Info *rslv)
{
  return data;
}

static Scheme_Object *do_require(Scheme_Object *form, Scheme_Comp_Env *env, 
				 Scheme_Compile_Expand_Info *rec, int drec)
{
  Scheme_Hash_Table *ht;
  Scheme_Object *rn_set, *dummy, *modidx;
  Scheme_Env *genv;

  if (!scheme_is_toplevel(env))
    scheme_wrong_syntax(NULL, NULL, form, "not at top-level or in module body");

  /* If we get here, it must be a top-level require. */

  /* Hash table is for checking duplicate names in require list: */
  ht = scheme_make_hash_table_equal();

  rn_set = scheme_make_module_rename_set(mzMOD_RENAME_TOPLEVEL, NULL);

  genv = env->genv;
  scheme_prepare_exp_env(genv);
  scheme_prepare_template_env(genv);

  if (genv->module)
    modidx = genv->module->self_modidx;
  else
    modidx = scheme_false;

  parse_requires(form, modidx, genv, NULL,
                 rn_set, rn_set,
                 check_dup_require, ht,
                 NULL, 
                 0, 0, 0, 
                 1, 0,
                 NULL);

  if (rec && rec[drec].comp) {
    /* Dummy lets us access a top-level environment: */
    dummy = scheme_make_environment_dummy(env);
    
    scheme_compile_rec_done_local(rec, drec);
    scheme_default_compile_rec(rec, drec);
    return scheme_make_syntax_compiled(REQUIRE_EXPD, 
				       cons(dummy,
					    form));
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
                                                  long phase,
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
  int i, k, count, cnt;

  l = scheme_null;
  cnt = 0;
  if (m->other_requires) {
    for (i = 0; i < m->other_requires->size; i++) {
      if (m->other_requires->vals[i]) {
        cnt++;
        l = scheme_make_pair(m->other_requires->keys[i],
                             scheme_make_pair(m->other_requires->vals[i],
                                              l));
      }
    }
  }
  l = cons(scheme_make_integer(cnt), l);

  l = cons(m->dt_requires, l);
  l = cons(m->tt_requires, l);
  l = cons(m->et_requires, l);
  l = cons(m->requires, l);

  l = cons(m->body, l);
  l = cons(m->et_body, l);

  cnt = 0;
  for (k = -3; k < (m->me->other_phases ? m->me->other_phases->size : 0); k++) {
    switch (k) {
    case -3:
      pt = m->me->dt;
      break;
    case -2:
      pt = m->me->et;
      break;
    case -1:
      pt = m->me->rt;
      break;
    default:
      pt = (Scheme_Module_Phase_Exports *)m->me->other_phases->vals[k];
    }
    
    if (pt) {
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

      if (pt->provide_nominal_srcs) {
        v = scheme_make_vector(count, NULL);
        for (i = 0; i < count; i++) {
          SCHEME_VEC_ELS(v)[i] = pt->provide_nominal_srcs[i];
        }
        l = cons(v, l);
      } else {
        l = cons(scheme_false, l);
      }

      if (pt->provide_src_phases) {
        v = scheme_make_vector(count, NULL);
        for (i = 0; i < count; i++) {
          SCHEME_VEC_ELS(v)[i] = (pt->provide_src_phases[i] ? scheme_true : scheme_false);
        } 
      } else
        v = scheme_false;
      l = cons(v, l);

      if (pt->provide_insps) {
        v = scheme_make_vector(count, scheme_false);
        for (i = 0; i < count; i++) {
          if (pt->provide_insps[i]) {
            if (SCHEME_PAIRP(pt->provide_insps[i]))
              SCHEME_VEC_ELS(v)[i] = scheme_void;
            else
              SCHEME_VEC_ELS(v)[i] = scheme_true;
          }
        }
      } else
        v = scheme_false;
      l = cons(v, l);

      l = cons(pt->phase_index, l);
      cnt++;
    }
  }

  l = cons(scheme_make_integer(cnt), l);

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
  
  count = m->me->et->num_provides;
  if (m->et_provide_protects) {
    for (i = 0; i < count; i++) {
      if (m->et_provide_protects[i])
	break;
    }
    if (i < count) {
      v = scheme_make_vector(count, NULL);
      for (i = 0; i < count; i++) {
	SCHEME_VEC_ELS(v)[i] = (m->et_provide_protects[i] ? scheme_true : scheme_false);
      }
    } else
      v = scheme_false;
    l = cons(v, l);
  } else
    l = cons(scheme_false, l);
  
  count = m->num_indirect_provides;
  l = cons(scheme_make_integer(count), l);
  v = scheme_make_vector(count, NULL);
  for (i = 0; i < count; i++) {
    SCHEME_VEC_ELS(v)[i] = m->indirect_provides[i];
  }
  l = cons(v, l);

  count = m->num_indirect_syntax_provides;
  l = cons(scheme_make_integer(count), l);
  v = scheme_make_vector(count, NULL);
  for (i = 0; i < count; i++) {
    SCHEME_VEC_ELS(v)[i] = m->indirect_syntax_provides[i];
  }
  l = cons(v, l);

  count = m->num_indirect_et_provides;
  l = cons(scheme_make_integer(count), l);
  v = scheme_make_vector(count, NULL);
  for (i = 0; i < count; i++) {
    SCHEME_VEC_ELS(v)[i] = m->et_indirect_provides[i];
  }
  l = cons(v, l);

  l = cons((Scheme_Object *)m->prefix, l);
  l = cons(m->dummy, l);

  l = cons(scheme_make_integer(m->max_let_depth), l);

  l = cons(wrap_mod_stx(m->rn_stx), l);

  /* previously recorded "functional?" info: */
  l = cons(scheme_false, l);
  l = cons(scheme_false, l);

  if (m->lang_info)
    l = cons(m->lang_info, l);
  else
    l = cons(scheme_false, l);

  l = cons(m->me->src_modidx, l);
  l = cons(SCHEME_PTR_VAL(m->modsrc), l);
  l = cons(SCHEME_PTR_VAL(m->modname), l);

  return l;
}

static int check_requires_ok(Scheme_Object *l)
{
  Scheme_Object *x;
  while (!SCHEME_NULLP(l)) {
    x = SCHEME_CAR(l);
    if (!SAME_TYPE(SCHEME_TYPE(x), scheme_module_index_type))
      return 0;
    l = SCHEME_CDR(l);
  }
  return 1;
}

#if 0
# define return_NULL() return (printf("%d\n", __LINE__), NULL)
#else
# define return_NULL() return NULL
#endif

static Scheme_Object *read_module(Scheme_Object *obj)
{
  Scheme_Module *m;
  Scheme_Object *ie, *nie, *insp;
  Scheme_Object *eesp, *esp, *esn, *esph, *es, *esnom, *einsp, *e, *nve, *ne, **v;
  Scheme_Module_Exports *me;
  Scheme_Module_Phase_Exports *pt;
  char *ps, *sps;
  int i, count, cnt;

  m = MALLOC_ONE_TAGGED(Scheme_Module);
  m->so.type = scheme_module_type;

  me = make_module_exports();
  m->me = me;

  if (!SCHEME_PAIRP(obj)) return_NULL();
  e = scheme_intern_resolved_module_path(SCHEME_CAR(obj));
  m->modname = e;
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  e = scheme_intern_resolved_module_path(SCHEME_CAR(obj));
  m->modsrc = e;
  m->me->modsrc = e;
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  me->src_modidx = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  ((Scheme_Modidx *)m->me->src_modidx)->resolved = m->modname;
  m->self_modidx = m->me->src_modidx;

  if (!SCHEME_PAIRP(obj)) return_NULL();
  e = SCHEME_CAR(obj);
  if (SCHEME_FALSEP(e))
    e = NULL;
  else if (!(SCHEME_VECTORP(e)
             && (3 == SCHEME_VEC_SIZE(e))
             && scheme_is_module_path(SCHEME_VEC_ELS(e)[0])
             && SCHEME_SYMBOLP(SCHEME_VEC_ELS(e)[1])))
    return_NULL();
  m->lang_info = e;
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  /* "functional?" info ignored */
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  /* "functional?" info ignored */
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  m->rn_stx = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  if (SCHEME_FALSEP(m->rn_stx))
    m->rn_stx = NULL;

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
  m->et_indirect_provides = v;
  m->num_indirect_et_provides = count;

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
  m->indirect_syntax_provides = v;
  m->num_indirect_syntax_provides = count;

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
  eesp = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  esp = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  cnt = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  
  while (cnt--) {
    Scheme_Object *phase;

    if (!SCHEME_PAIRP(obj)) return_NULL();
    phase = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);

    if (!SCHEME_FALSEP(phase)
        && !SCHEME_INTP(phase)
        && !SCHEME_BIGNUMP(phase))
      return_NULL();

    if (SAME_OBJ(phase, scheme_make_integer(0))) {
      pt = me->rt;
    } else if (SAME_OBJ(phase, scheme_make_integer(1))) {
      pt = me->et;
    } else if (SAME_OBJ(phase, scheme_false)) {
      pt = me->dt;
    } else {
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

    if (!SCHEME_PAIRP(obj)) return_NULL();
    einsp = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);

    if (!SCHEME_PAIRP(obj)) return_NULL();
    esph = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);

    if (!SCHEME_PAIRP(obj)) return_NULL();
    esnom = SCHEME_CAR(obj);
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

    if (SCHEME_FALSEP(esnom)) {
      pt->provide_nominal_srcs = NULL;
    } else {
      if (!SCHEME_VECTORP(esnom) || (SCHEME_VEC_SIZE(esnom) != count)) return_NULL();
      v = MALLOC_N(Scheme_Object *, count);
      for (i = 0; i < count; i++) {
        v[i] = SCHEME_VEC_ELS(esnom)[i];
      }
      pt->provide_nominal_srcs = v;
    }

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

    if (SCHEME_FALSEP(einsp)) {
      pt->provide_insps = NULL;
    } else {
      if (!SCHEME_VECTORP(einsp) || (SCHEME_VEC_SIZE(einsp) != count)) return_NULL();
      insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);
      v = MALLOC_N(Scheme_Object *, count);
      for (i = 0; i < count; i++) {
        if (SCHEME_TRUEP(SCHEME_VEC_ELS(einsp)[i])) {
          if (SCHEME_VOIDP(SCHEME_VEC_ELS(einsp)[i])) {
            e = cons(scheme_false, insp);
            v[i] = e;
          } else
            v[i] = insp;
        }
      }
      pt->provide_insps = v;
    }
  }

  count = me->rt->num_provides;

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

  if (SCHEME_FALSEP(eesp)) {
    m->et_provide_protects = NULL;
  } else {
    if (!SCHEME_VECTORP(eesp) || (SCHEME_VEC_SIZE(eesp) != count)) return_NULL();
    ps = MALLOC_N_ATOMIC(char, count);
    for (i = 0; i < count; i++) {
      ps[i] = SCHEME_TRUEP(SCHEME_VEC_ELS(eesp)[i]);
    }
    m->et_provide_protects = ps;
  }

  if (!SCHEME_PAIRP(obj)) return_NULL();
  e = SCHEME_CAR(obj);
  if (!SCHEME_VECTORP(e)) return_NULL();
  m->et_body = e;
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  e = SCHEME_CAR(obj);
  if (!SCHEME_VECTORP(e)) return_NULL();
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

  if (!SCHEME_PAIRP(obj)) return_NULL();
  if (scheme_proper_list_length(SCHEME_CAR(obj)) < 0) return_NULL();
  e = scheme_copy_list(SCHEME_CAR(obj));
  m->dt_requires = e;
  if (!check_requires_ok(e)) return_NULL();
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  cnt = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  while (cnt--) {
    Scheme_Object *phase;

    if (!SCHEME_PAIRP(obj)) return_NULL();
    phase = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);

    if (!SCHEME_INTP(phase)
        && !SCHEME_BIGNUMP(phase))
      return_NULL();

    if (SAME_OBJ(phase, scheme_make_integer(0))
        || SAME_OBJ(phase, scheme_make_integer(1))
        || SAME_OBJ(phase, scheme_make_integer(-1)))
      return_NULL();

    if (!SCHEME_PAIRP(obj)) return_NULL();
    e = scheme_copy_list(SCHEME_CAR(obj));
    if (!check_requires_ok(e)) return_NULL();

    if (!m->other_requires) {
      Scheme_Hash_Table *ht;
      ht = scheme_make_hash_table_equal();
      m->other_requires = ht;
    }
    scheme_hash_set(m->other_requires, phase, e);
    
    obj = SCHEME_CDR(obj);
  }
  
  return (Scheme_Object *)m;
}
