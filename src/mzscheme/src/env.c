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

/* This file implements environments (both compile-time and top-level
   envionments, a.k.a. namespaces), and also implements much of the
   initialization sequence (filling the initial namespace). */

#include "schpriv.h"
#include "schminc.h"
#include "schmach.h"
#include "schexpobs.h"

#if defined(UNIX_LIMIT_STACK) || defined(UNIX_LIMIT_FDSET_SIZE)
# include <signal.h>
# include <sys/time.h>
# include <sys/resource.h>
#endif

#ifdef MZ_USE_IRIX_SPROCS
# include "../gc/gc.h"
#endif

#define GLOBAL_TABLE_SIZE 500

/* #define TIME_STARTUP_PROCESS */

/* globals */
int scheme_allow_set_undefined;

void scheme_set_allow_set_undefined(int v) { scheme_allow_set_undefined =  v; }
int scheme_get_allow_set_undefined() { return scheme_allow_set_undefined; }

int scheme_starting_up;

Scheme_Object *scheme_local[MAX_CONST_LOCAL_POS][2];

#define MAX_CONST_TOPLEVEL_DEPTH 16
#define MAX_CONST_TOPLEVEL_POS 16
Scheme_Object *toplevels[MAX_CONST_TOPLEVEL_DEPTH][MAX_CONST_TOPLEVEL_POS][SCHEME_TOPLEVEL_FLAGS_MASK + 1];

#define TABLE_CACHE_MAX_SIZE 2048
Scheme_Hash_Table *toplevels_ht;
Scheme_Hash_Table *locals_ht[2];

Scheme_Env *scheme_initial_env;

/* locals */
static Scheme_Env *make_env(Scheme_Env *base, int semi, int toplevel_size);
static void make_init_env(void);

static Scheme_Object *namespace_identifier(int, Scheme_Object *[]);
static Scheme_Object *namespace_variable_value(int, Scheme_Object *[]);
static Scheme_Object *namespace_set_variable_value(int, Scheme_Object *[]);
static Scheme_Object *namespace_undefine_variable(int, Scheme_Object *[]);
static Scheme_Object *namespace_mapped_symbols(int, Scheme_Object *[]);
static Scheme_Object *namespace_module_registry(int, Scheme_Object *[]);
static Scheme_Object *now_transforming(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_exp_time_value(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_exp_time_name(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_context(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_make_intdef_context(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_introduce(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_module_introduce(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_get_shadower(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_certify(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_lift_expr(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_lift_context(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_lift_end_statement(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_introducer(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_set_transformer(int argc, Scheme_Object *argv[]);
static Scheme_Object *set_transformer_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *set_transformer_proc(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_rename_transformer(int argc, Scheme_Object *argv[]);
static Scheme_Object *rename_transformer_target(int argc, Scheme_Object *argv[]);
static Scheme_Object *rename_transformer_p(int argc, Scheme_Object *argv[]);

static Scheme_Object *write_toplevel(Scheme_Object *obj);
static Scheme_Object *read_toplevel(Scheme_Object *obj);
static Scheme_Object *write_variable(Scheme_Object *obj);
static Scheme_Object *read_variable(Scheme_Object *obj);
static Scheme_Object *write_local(Scheme_Object *obj);
static Scheme_Object *read_local(Scheme_Object *obj);
static Scheme_Object *read_local_unbox(Scheme_Object *obj);
static Scheme_Object *write_resolve_prefix(Scheme_Object *obj);
static Scheme_Object *read_resolve_prefix(Scheme_Object *obj);

static void skip_certain_things(Scheme_Object *o, Scheme_Close_Custodian_Client *f, void *data);

int scheme_is_module_begin_env(Scheme_Comp_Env *env);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

typedef Scheme_Object *(*Lazy_Macro_Fun)(Scheme_Object *, int);

static Scheme_Object *kernel_symbol;

static int intdef_counter = 0;

static int builtin_ref_counter = 0;

static int env_uid_counter;

#define ARBITRARY_USE     0x1
#define CONSTRAINED_USE   0x2
#define WAS_SET_BANGED    0x4
#define ONE_ARBITRARY_USE 0x8
/* See also SCHEME_USE_COUNT_MASK */

typedef struct Compile_Data {
  int num_const;
  Scheme_Object **const_names;
  Scheme_Object **const_vals;
  Scheme_Object **const_uids;
  int *use;
  Scheme_Object *lifts;
} Compile_Data;

typedef struct Scheme_Full_Comp_Env {
  Scheme_Comp_Env base;
  Compile_Data data;
} Scheme_Full_Comp_Env;
static void init_compile_data(Scheme_Comp_Env *env);

/* Precise GC WARNING: this macro produces unaligned pointers: */
#define COMPILE_DATA(e) (&((Scheme_Full_Comp_Env *)e)->data)

#define SCHEME_NON_SIMPLE_FRAME (SCHEME_NO_RENAME | SCHEME_CAPTURE_WITHOUT_RENAME \
                                 | SCHEME_FOR_STOPS | SCHEME_FOR_INTDEF | SCHEME_CAPTURE_LIFTED)

#define ASSERT_IS_VARIABLE_BUCKET(b) /* if (((Scheme_Object *)b)->type != scheme_variable_type) abort() */

/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/


#ifdef DONT_USE_FOREIGN
static void init_dummy_foreign(Scheme *env)
{
  /* Works just well enough that the `mzscheme' module can 
     import it (so that attaching `mzscheme' to a namespace 
     also attaches `#%foreign'). */
  Scheme_Env *menv;
  menv = scheme_primitive_module(scheme_intern_symbol("#%foreign"), env);
  scheme_finish_primitive_module(menv);
  scheme_protect_primitive_provide(menv, NULL);
}
#endif

Scheme_Env *scheme_basic_env()
{
  Scheme_Env *env;

  if (scheme_main_thread) {
    /* Reset everything: */
    scheme_do_close_managed(NULL, skip_certain_things);
    scheme_main_thread = NULL;

    scheme_reset_finalizations();
    scheme_init_stack_check();
#ifndef MZ_PRECISE_GC
    scheme_init_setjumpup();
#endif
    scheme_reset_overflow();

    scheme_make_thread();
    scheme_init_error_escape_proc(NULL);

    env = scheme_make_empty_env();
    scheme_install_initial_module_set(env);
    scheme_set_param(scheme_current_config(), MZCONFIG_ENV, 
		     (Scheme_Object *)env); 

    scheme_init_port_config();
    scheme_init_port_fun_config();
    scheme_init_error_config();
#ifndef NO_SCHEME_EXNS
    scheme_init_exn_config();
#endif

    return env;
  }

#ifdef UNIX_LIMIT_STACK
  {
    struct rlimit rl;
    
    getrlimit(RLIMIT_STACK, &rl);
    if (rl.rlim_cur > UNIX_LIMIT_STACK) {
      rl.rlim_cur = UNIX_LIMIT_STACK;
      setrlimit(RLIMIT_STACK, &rl);
    }
  }
#endif
#ifdef UNIX_LIMIT_FDSET_SIZE
  {
    struct rlimit rl;
    
    getrlimit(RLIMIT_NOFILE, &rl);
    if (rl.rlim_cur > FD_SETSIZE) {
      rl.rlim_cur = FD_SETSIZE;
      setrlimit(RLIMIT_NOFILE, &rl);
    }
  }
#endif

#ifdef MZ_USE_IRIX_SPROCS
  GC_INIT();
#endif

  scheme_starting_up = 1;

#ifndef MZ_PRECISE_GC
  scheme_init_setjumpup();
  scheme_init_ephemerons();
#endif

#ifdef TIME_STARTUP_PROCESS
  printf("#if 0\nbasic @ %ld\n", scheme_get_process_milliseconds());
#endif

  scheme_init_stack_check();
  scheme_init_overflow();
  scheme_init_portable_case();


  {
    int i, k;

#ifndef USE_TAGGED_ALLOCATION
    GC_CAN_IGNORE Scheme_Local *all;

    all = (Scheme_Local *)scheme_malloc_eternal(sizeof(Scheme_Local) * 2 * MAX_CONST_LOCAL_POS);
# ifdef MEMORY_COUNTING_ON
    scheme_misc_count += sizeof(Scheme_Local) * 2 * MAX_CONST_LOCAL_POS;
# endif    
#endif

    for (i = 0; i < MAX_CONST_LOCAL_POS; i++) {
      for (k = 0; k < 2; k++) {
	Scheme_Object *v;
	
#ifndef USE_TAGGED_ALLOCATION
	v = (Scheme_Object *)(all++);
#else
	v = (Scheme_Object *)scheme_malloc_eternal_tagged(sizeof(Scheme_Local));
#endif
	v->type = k + scheme_local_type;
	SCHEME_LOCAL_POS(v) = i;
	
	scheme_local[i][k] = v;
      }
    }
  }

  {
    int i, k, cnst;

#ifndef USE_TAGGED_ALLOCATION
    GC_CAN_IGNORE Scheme_Toplevel *all;

    all = (Scheme_Toplevel *)scheme_malloc_eternal(sizeof(Scheme_Toplevel) 
						   * MAX_CONST_TOPLEVEL_DEPTH 
						   * MAX_CONST_TOPLEVEL_POS
						   * (SCHEME_TOPLEVEL_FLAGS_MASK + 1));
# ifdef MEMORY_COUNTING_ON
    scheme_misc_count += (sizeof(Scheme_Toplevel) 
			  * MAX_CONST_TOPLEVEL_DEPTH 
			  * MAX_CONST_TOPLEVEL_POS
			  * (SCHEME_TOPLEVEL_FLAGS_MASK + 1));
# endif
#endif

    for (i = 0; i < MAX_CONST_TOPLEVEL_DEPTH; i++) {
      for (k = 0; k < MAX_CONST_TOPLEVEL_POS; k++) {
	for (cnst = 0; cnst <= SCHEME_TOPLEVEL_FLAGS_MASK; cnst++) {
	  Scheme_Toplevel *v;
	
#ifndef USE_TAGGED_ALLOCATION
	  v = (all++);
#else
	  v = (Scheme_Toplevel *)scheme_malloc_eternal_tagged(sizeof(Scheme_Toplevel));
#endif
	  v->iso.so.type = scheme_toplevel_type;
	  v->depth = i;
	  v->position = k;
	  SCHEME_TOPLEVEL_FLAGS(v) = cnst;
	
	  toplevels[i][k][cnst] = (Scheme_Object *)v;
	}
      }
    }
  }

#ifdef MZ_PRECISE_GC
  scheme_register_traversers();
  register_traversers();
  scheme_init_hash_key_procs();
#endif

  scheme_init_true_false();

  REGISTER_SO(toplevels_ht);
  REGISTER_SO(locals_ht[0]);
  REGISTER_SO(locals_ht[1]);

  {
    Scheme_Hash_Table *ht;
    toplevels_ht = scheme_make_hash_table_equal();
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    locals_ht[0] = ht;
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    locals_ht[1] = ht;
  }

#ifdef TIME_STARTUP_PROCESS
  printf("pre-process @ %ld\n", scheme_get_process_milliseconds());
#endif

#ifdef WINDOWS_PROCESSES
  /* Must be called before first scheme_make_thread() */
  scheme_init_thread_memory();
#endif
    
  scheme_init_getenv(); /* checks PLTNOJIT */

  scheme_make_thread();

#ifdef TIME_STARTUP_PROCESS
  printf("process @ %ld\n", scheme_get_process_milliseconds());
#endif

  make_init_env();

  env = scheme_make_empty_env();
  scheme_require_from_original_env(env, 1); /* Need kernel syntax... */

  scheme_set_param(scheme_current_config(), MZCONFIG_ENV, 
		   (Scheme_Object *)env); 
  scheme_init_memtrace(env);
  scheme_init_parameterization(env);
  scheme_init_expand_observe(env);

#ifndef DONT_USE_FOREIGN
  scheme_init_foreign(env);
#else
  init_dummy_foreign(env);
#endif

  scheme_add_embedded_builtins(env);

  scheme_save_initial_module_set(env);

  scheme_init_error_escape_proc(NULL);

  scheme_starting_up = 0;

#ifdef TIME_STARTUP_PROCESS
  printf("done @ %ld\n#endif\n", scheme_get_process_milliseconds());
#endif

  return env;
}

static void make_init_env(void)
{
  Scheme_Env *env;
#ifdef TIME_STARTUP_PROCESS
  long startt;
#endif

  env = make_env(NULL, 0, GLOBAL_TABLE_SIZE);

  scheme_set_param(scheme_current_config(), MZCONFIG_ENV, 
		   (Scheme_Object *)env);

  REGISTER_SO(scheme_initial_env);
  scheme_initial_env = env;

  scheme_defining_primitives = 1;
  builtin_ref_counter = 0;

#ifdef TIME_STARTUP_PROCESS
   printf("init @ %ld\n", scheme_get_process_milliseconds());
# define MZTIMEIT(n, f) (MARK_START_TIME(), f, DONE_TIME(n))
# define MARK_START_TIME() startt = scheme_get_process_milliseconds()
# define DONE_TIME(n) (printf(#n ": %ld\n", (long)(scheme_get_process_milliseconds() - startt)))
#else
# define MZTIMEIT(n, f) f
# define MARK_START_TIME() /**/
# define DONE_TIME(n) /**/
#endif

  /* The ordering of the first few init calls is important, so add to
     the end of the list, not the beginning. */
  MZTIMEIT(symbol-table, scheme_init_symbol_table());
  MZTIMEIT(type, scheme_init_type(env));
  MZTIMEIT(symbol-type, scheme_init_symbol_type(env));
  MZTIMEIT(fun, scheme_init_fun(env));
  MZTIMEIT(symbol, scheme_init_symbol(env));
  MZTIMEIT(list, scheme_init_list(env));
  MZTIMEIT(number, scheme_init_number(env));
  MZTIMEIT(numarith, scheme_init_numarith(env));
  MZTIMEIT(numcomp, scheme_init_numcomp(env));
  MZTIMEIT(numstr, scheme_init_numstr(env));
  MZTIMEIT(stx, scheme_init_stx(env));
  MZTIMEIT(module, scheme_init_module(env));
  MZTIMEIT(port, scheme_init_port(env));
  MZTIMEIT(portfun, scheme_init_port_fun(env));
#ifndef NO_TCP_SUPPORT
  MZTIMEIT(network, scheme_init_network(env));
#endif
  MZTIMEIT(string, scheme_init_string(env));
  MZTIMEIT(vector, scheme_init_vector(env));
  MZTIMEIT(char, scheme_init_char(env));
  MZTIMEIT(bool, scheme_init_bool(env));
  MZTIMEIT(syntax, scheme_init_syntax(env));
  MZTIMEIT(eval, scheme_init_eval(env));
  MZTIMEIT(error, scheme_init_error(env));
  MZTIMEIT(struct, scheme_init_struct(env));
#ifndef NO_SCHEME_EXNS
  MZTIMEIT(exn, scheme_init_exn(env));
#endif
  MZTIMEIT(process, scheme_init_thread(env));
#ifndef NO_SCHEME_THREADS
  MZTIMEIT(sema, scheme_init_sema(env));
#endif
  MZTIMEIT(read, scheme_init_read(env));
  MZTIMEIT(print, scheme_init_print(env));
  MZTIMEIT(file, scheme_init_file(env));
  MZTIMEIT(dynamic-extension, scheme_init_dynamic_extension(env));
#ifndef NO_REGEXP_UTILS
  MZTIMEIT(regexp, scheme_regexp_initialize(env));
#endif

  MARK_START_TIME();

  scheme_add_global_constant("namespace-symbol->identifier",
			     scheme_make_prim_w_arity(namespace_identifier,
						      "namespace-symbol->identifier",
						      1, 2),
			     env);

  scheme_add_global_constant("namespace-variable-value",
			     scheme_make_prim_w_arity(namespace_variable_value,
						      "namespace-variable-value",
						      1, 4),
			     env);

  scheme_add_global_constant("namespace-set-variable-value!",
			     scheme_make_prim_w_arity(namespace_set_variable_value,
						      "namespace-set-variable-value!",
						      2, 4),
			     env);

  scheme_add_global_constant("namespace-undefine-variable!",
			     scheme_make_prim_w_arity(namespace_undefine_variable,
						      "namespace-undefine-variable!",
						      1, 2),
			     env);

  scheme_add_global_constant("namespace-mapped-symbols",
			     scheme_make_prim_w_arity(namespace_mapped_symbols,
						      "namespace-mapped-symbols",
						      0, 1),
			     env);

  scheme_add_global_constant("namespace-module-registry",
			     scheme_make_prim_w_arity(namespace_module_registry,
						      "namespace-module-registry",
						      1, 1),
			     env);


  scheme_add_global_constant("syntax-transforming?", 
			     scheme_make_prim_w_arity(now_transforming,
						      "syntax-transforming?",
						      0, 0),
			     env);
  scheme_add_global_constant("syntax-local-value", 
			     scheme_make_prim_w_arity(local_exp_time_value,
						      "syntax-local-value",
						      1, 3),
			     env);
  scheme_add_global_constant("syntax-local-name", 
			     scheme_make_prim_w_arity(local_exp_time_name,
						      "syntax-local-name",
						      0, 0),
			     env);
  scheme_add_global_constant("syntax-local-context", 
			     scheme_make_prim_w_arity(local_context,
						      "syntax-local-context",
						      0, 0),
			     env);
  scheme_add_global_constant("syntax-local-make-definition-context", 
			     scheme_make_prim_w_arity(local_make_intdef_context,
						      "syntax-local-make-definition-context",
						      0, 0),
			     env);
  scheme_add_global_constant("syntax-local-get-shadower", 
			     scheme_make_prim_w_arity(local_get_shadower,
						      "syntax-local-get-shadower",
						      1, 1),
			     env);
  scheme_add_global_constant("syntax-local-introduce", 
			     scheme_make_prim_w_arity(local_introduce,
						      "syntax-local-introduce",
						      1, 1),
			     env);
  scheme_add_global_constant("make-syntax-introducer", 
			     scheme_make_prim_w_arity(make_introducer,
						      "make-syntax-introducer",
						      0, 1),
			     env);
  scheme_add_global_constant("syntax-local-certifier", 
			     scheme_make_prim_w_arity(local_certify,
						      "syntax-local-certifier",
						      0, 1),
			     env);

  scheme_add_global_constant("make-set!-transformer", 
			     scheme_make_prim_w_arity(make_set_transformer,
						      "make-set!-transformer",
						      1, 1),
			     env);

  scheme_add_global_constant("set!-transformer?", 
			     scheme_make_prim_w_arity(set_transformer_p,
						      "set!-transformer?",
						      1, 1),
			     env);

  scheme_add_global_constant("set!-transformer-procedure", 
			     scheme_make_prim_w_arity(set_transformer_proc,
						      "set!-transformer-procedure",
						      1, 1),
			     env);

  scheme_add_global_constant("make-rename-transformer", 
			     scheme_make_prim_w_arity(make_rename_transformer,
						      "make-rename-transformer",
						      1, 1),
			     env);

  scheme_add_global_constant("rename-transformer?", 
			     scheme_make_prim_w_arity(rename_transformer_p,
						      "rename-transformer?",
						      1, 1),
			     env);

  scheme_add_global_constant("rename-transformer-target", 
			     scheme_make_prim_w_arity(rename_transformer_target,
						      "rename-transformer-target",
						      1, 1),
			     env);

  scheme_add_global_constant("syntax-local-lift-expression", 
			     scheme_make_prim_w_arity(local_lift_expr, 
						      "syntax-local-lift-expression",
						      1, 1), 
			     env);
  scheme_add_global_constant("syntax-local-lift-context", 
			     scheme_make_prim_w_arity(local_lift_context, 
						      "syntax-local-lift-context",
						      0, 0), 
			     env);

  scheme_add_global_constant("syntax-local-lift-module-end-declaration", 
			     scheme_make_prim_w_arity(local_lift_end_statement, 
						      "syntax-local-lift-module-end-declaration",
						      1, 1), 
			     env);

  {
    Scheme_Object *sym;
    sym = scheme_intern_symbol("mzscheme");
    scheme_current_thread->name = sym;
  }

  DONE_TIME(env);

  scheme_install_type_writer(scheme_toplevel_type, write_toplevel);
  scheme_install_type_reader(scheme_toplevel_type, read_toplevel);
  scheme_install_type_writer(scheme_variable_type, write_variable);
  scheme_install_type_reader(scheme_variable_type, read_variable);
  scheme_install_type_writer(scheme_module_variable_type, write_variable);
  scheme_install_type_reader(scheme_module_variable_type, read_variable);
  scheme_install_type_writer(scheme_local_type, write_local);
  scheme_install_type_reader(scheme_local_type, read_local);
  scheme_install_type_writer(scheme_local_unbox_type, write_local);
  scheme_install_type_reader(scheme_local_unbox_type, read_local_unbox);
  scheme_install_type_writer(scheme_resolve_prefix_type, write_resolve_prefix);
  scheme_install_type_reader(scheme_resolve_prefix_type, read_resolve_prefix);

  REGISTER_SO(kernel_symbol);
  kernel_symbol = scheme_intern_symbol("#%kernel");

  MARK_START_TIME();

  scheme_finish_kernel(env);

#if USE_COMPILED_STARTUP
  if (builtin_ref_counter != EXPECTED_PRIM_COUNT) {
    printf("Primitive count %d doesn't match expected count %d\n"
	   "Turn off USE_COMPILED_STARTUP in src/schminc.h\n",
	   builtin_ref_counter, EXPECTED_PRIM_COUNT);
    exit(1);
  }
#endif
   
  scheme_defining_primitives = 0;
}

/* Shutdown procedure for resetting a namespace: */
static void skip_certain_things(Scheme_Object *o, Scheme_Close_Custodian_Client *f, void *data)
{
  if ((o == scheme_orig_stdin_port)
      || (o == scheme_orig_stdout_port)
      || (o == scheme_orig_stderr_port))
    return;

  /* f is NULL for threads */
  if (f)
    f(o, data);
}

/*========================================================================*/
/*                        namespace constructors                          */
/*========================================================================*/

static void create_env_marked_names(Scheme_Env *e)
{
  Scheme_Hash_Table *mn;
  Scheme_Object *rn;

  /* Set up a rename table, in case an identifier with a let-binding
     renaming ends up in a definition position: */

  mn = scheme_make_hash_table(SCHEME_hash_ptr);
  scheme_hash_set(mn, scheme_false, scheme_null);
  e->marked_names = mn;

  rn = scheme_make_module_rename(e->phase, mzMOD_RENAME_TOPLEVEL, mn);
  e->rename = rn;
}

Scheme_Env *scheme_make_empty_env(void)
{
  Scheme_Env *e; 

  e = make_env(NULL, 0, 7);
  create_env_marked_names(e);

  return e;
}

static Scheme_Env *make_env(Scheme_Env *base, int semi, int toplevel_size)
{
  Scheme_Bucket_Table *toplevel, *syntax;
  Scheme_Hash_Table *module_registry, *export_registry;
  Scheme_Object *modchain;
  Scheme_Env *env;

  toplevel = scheme_make_bucket_table(toplevel_size, SCHEME_hash_ptr);
  toplevel->with_home = 1;

  if (semi > 0) {
    syntax = NULL;
    modchain = NULL;
    module_registry = NULL;
    export_registry = NULL;
  } else {
    syntax = scheme_make_bucket_table(7, SCHEME_hash_ptr);
    if (base) {
      modchain = base->modchain;
      module_registry = base->module_registry;
      export_registry = base->export_registry;
    } else {
      if (semi < 0) {
	module_registry = NULL;
	export_registry = NULL;
	modchain = NULL;
      } else {
	Scheme_Hash_Table *modules;

	modules = scheme_make_hash_table(SCHEME_hash_ptr);
	modchain = scheme_make_vector(3, scheme_false);
	SCHEME_VEC_ELS(modchain)[0] = (Scheme_Object *)modules;

	module_registry = scheme_make_hash_table(SCHEME_hash_ptr);
	module_registry->iso.so.type = scheme_module_registry_type;
	
	export_registry = scheme_make_hash_table(SCHEME_hash_ptr);
      }
    }
  }

  env = MALLOC_ONE_TAGGED(Scheme_Env);
  env->so.type = scheme_namespace_type;

  env->toplevel = toplevel;

  if (semi < 1) {
    env->syntax = syntax;
    env->modchain = modchain;
    env->module_registry = module_registry;
    env->export_registry = export_registry;
  }

  return env;
}

Scheme_Env *
scheme_new_module_env(Scheme_Env *env, Scheme_Module *m, int new_exp_module_tree)
{
  Scheme_Env *menv;

  menv = make_env(env, 0, 7);

  menv->module = m;

  if (new_exp_module_tree) {
    Scheme_Object *p;
    Scheme_Hash_Table *modules;

    modules = scheme_make_hash_table(SCHEME_hash_ptr);
    p = scheme_make_vector(3, scheme_false);
    SCHEME_VEC_ELS(p)[0] = (Scheme_Object *)modules;
    menv->modchain = p;
  }

  return menv;
}

void scheme_prepare_exp_env(Scheme_Env *env)
{
  if (!env->exp_env) {
    Scheme_Env *eenv;
    Scheme_Object *modchain;

    eenv = make_env(NULL, -1, 7);
    eenv->phase = env->phase + 1;
    eenv->mod_phase = env->mod_phase + 1;

    eenv->module = env->module;
    eenv->module_registry = env->module_registry;
    eenv->export_registry = env->export_registry;
    eenv->insp = env->insp;

    modchain = SCHEME_VEC_ELS(env->modchain)[1];
    if (SCHEME_FALSEP(modchain)) {
      Scheme_Hash_Table *next_modules;

      next_modules = scheme_make_hash_table(SCHEME_hash_ptr);
      modchain = scheme_make_vector(3, scheme_false);
      SCHEME_VEC_ELS(modchain)[0] = (Scheme_Object *)next_modules;
      SCHEME_VEC_ELS(env->modchain)[1] = modchain;
      SCHEME_VEC_ELS(modchain)[2] = env->modchain;
    }
    eenv->modchain = modchain;

    env->exp_env = eenv;
    eenv->template_env = env;

    if (!env->module && !env->phase)
      create_env_marked_names(eenv);
  }
}

void scheme_prepare_template_env(Scheme_Env *env)
{
  if (!env->template_env) {
    Scheme_Env *eenv;
    Scheme_Object *modchain;

    eenv = make_env(NULL, -1, 7);
    eenv->phase = env->phase - 1;
    eenv->mod_phase = env->mod_phase - 1;

    eenv->module = env->module;
    eenv->module_registry = env->module_registry;
    eenv->export_registry = env->export_registry;
    eenv->insp = env->insp;

    modchain = SCHEME_VEC_ELS(env->modchain)[2];
    if (SCHEME_FALSEP(modchain)) {
      Scheme_Hash_Table *prev_modules;

      prev_modules = scheme_make_hash_table(SCHEME_hash_ptr);
      modchain = scheme_make_vector(3, scheme_false);
      SCHEME_VEC_ELS(modchain)[0] = (Scheme_Object *)prev_modules;
      SCHEME_VEC_ELS(env->modchain)[2] = modchain;
      SCHEME_VEC_ELS(modchain)[1] = env->modchain;
    }
    eenv->modchain = modchain;

    env->template_env = eenv;
    eenv->exp_env = env;
  }
}

Scheme_Env *scheme_clone_module_env(Scheme_Env *menv, Scheme_Env *ns, Scheme_Object *modchain)
{
  /* New env should have the same syntax and globals table, but it lives in
     a different namespaces. */
  Scheme_Env *menv2;

  menv2 = MALLOC_ONE_TAGGED(Scheme_Env);
  menv2->so.type = scheme_namespace_type;

  menv2->module = menv->module;
  menv2->module_registry = ns->module_registry;
  menv2->export_registry = ns->export_registry;
  menv2->insp = menv->insp;

  menv2->syntax = menv->syntax;

  menv2->phase = menv->phase;
  menv2->mod_phase = menv->mod_phase;
  menv2->link_midx = menv->link_midx;
  menv2->running = menv->running;
  menv2->et_running = menv->et_running;

  menv2->require_names = menv->require_names;
  menv2->et_require_names = menv->et_require_names;

  menv2->toplevel = menv->toplevel;
  
  menv2->modchain = modchain;

  if (!SCHEME_NULLP(menv2->module->et_requires)) {
    /* We'll need the next link in the modchain: */
    modchain = SCHEME_VEC_ELS(modchain)[1];
    if (SCHEME_FALSEP(modchain)) {
      Scheme_Hash_Table *next_modules;
      
      next_modules = scheme_make_hash_table(SCHEME_hash_ptr);
      modchain = scheme_make_vector(3, scheme_false);
      SCHEME_VEC_ELS(modchain)[0] = (Scheme_Object *)next_modules;
      SCHEME_VEC_ELS(menv2->modchain)[1] = modchain;
      SCHEME_VEC_ELS(modchain)[2] = menv2->modchain;
    }
  }

  if (menv->exp_env) {
    /* Share for-syntax bindings, too: */
    scheme_prepare_exp_env(menv2);
    menv2->exp_env->toplevel = menv->exp_env->toplevel;
  }

  return menv2;
}

Scheme_Bucket_Table *scheme_clone_toplevel(Scheme_Bucket_Table *ht, Scheme_Env *home)
{
  Scheme_Bucket_Table *r;
  Scheme_Bucket **bs;
  int i;

  r = scheme_make_bucket_table(ht->size, SCHEME_hash_ptr);
  if (home)
    r->with_home = 1;

  bs = ht->buckets;

  for (i = ht->size; i--; ) {
    Scheme_Bucket *b = bs[i];
    if (b && b->val) {
      Scheme_Object *name = (Scheme_Object *)b->key;
      Scheme_Object *val = (Scheme_Object *)b->val;

      b = scheme_bucket_from_table(r, (const char *)name);
      b->val = val;
      if (home) {
        ASSERT_IS_VARIABLE_BUCKET(b);
	((Scheme_Bucket_With_Home *)b)->home = home;
      }
    }
  }

  return r;
}

void scheme_clean_dead_env(Scheme_Env *env)
{
  Scheme_Object *modchain, *next;

  if (env->exp_env) {
    env->exp_env->template_env = NULL;
    scheme_clean_dead_env(env->exp_env);
    env->exp_env = NULL;
  }
  if (env->template_env) {
    env->template_env->exp_env = NULL;
    scheme_clean_dead_env(env->template_env);
    env->template_env = NULL;
  }

  env->modvars = NULL;
  
  modchain = env->modchain;
  env->modchain = NULL;
  while (modchain && !SCHEME_VECTORP(modchain)) {
    next = SCHEME_VEC_ELS(modchain)[1];
    SCHEME_VEC_ELS(modchain)[1] = scheme_void;
    modchain = next;
  }
}

/*========================================================================*/
/*                           namespace bindings                           */
/*========================================================================*/

/********** Lookup **********/

Scheme_Object *
scheme_lookup_global(Scheme_Object *symbol, Scheme_Env *env)
{
  Scheme_Bucket *b;
    
  b = scheme_bucket_or_null_from_table(env->toplevel, (char *)symbol, 0);
  if (b) {
    ASSERT_IS_VARIABLE_BUCKET(b);
    if (!((Scheme_Bucket_With_Home *)b)->home)
      ((Scheme_Bucket_With_Home *)b)->home = env;
    return (Scheme_Object *)b->val;
  }

  return NULL;
}

Scheme_Bucket *
scheme_global_bucket(Scheme_Object *symbol, Scheme_Env *env)
{
  Scheme_Bucket *b;
    
  b = scheme_bucket_from_table(env->toplevel, (char *)symbol);
  ASSERT_IS_VARIABLE_BUCKET(b);
  if (!((Scheme_Bucket_With_Home *)b)->home)
    ((Scheme_Bucket_With_Home *)b)->home = env;
    
  return b;
}

Scheme_Bucket *
scheme_global_keyword_bucket(Scheme_Object *symbol, Scheme_Env *env)
{
  Scheme_Bucket *b;
    
  b = scheme_bucket_from_table(env->syntax, (char *)symbol);
    
  return b;
}

/********** Set **********/

void
scheme_do_add_global_symbol(Scheme_Env *env, Scheme_Object *sym, 
			    Scheme_Object *obj, 
			    int valvar, int constant)
{
  if (valvar) {
    Scheme_Bucket *b;
    b = scheme_bucket_from_table(env->toplevel, (const char *)sym);
    b->val = obj;
    ASSERT_IS_VARIABLE_BUCKET(b);
    ((Scheme_Bucket_With_Home *)b)->home = env;
    if (constant && scheme_defining_primitives) {
      ((Scheme_Bucket_With_Flags *)b)->id = builtin_ref_counter++;
      ((Scheme_Bucket_With_Flags *)b)->flags |= (GLOB_HAS_REF_ID | GLOB_IS_CONST);
    }
  } else
    scheme_add_to_table(env->syntax, (const char *)sym, obj, constant);
}

void
scheme_add_global(const char *name, Scheme_Object *obj, Scheme_Env *env)
{
  scheme_do_add_global_symbol(env, scheme_intern_symbol(name), obj, 1, 0);
}

void
scheme_add_global_symbol(Scheme_Object *sym, Scheme_Object *obj, Scheme_Env *env)
{
  scheme_do_add_global_symbol(env, sym, obj, 1, 0);
}

void
scheme_add_global_constant(const char *name, Scheme_Object *obj, 
			   Scheme_Env *env)
{
  scheme_do_add_global_symbol(env, scheme_intern_symbol(name), obj, 1, 1);
}

void
scheme_add_global_constant_symbol(Scheme_Object *name, Scheme_Object *obj, 
				  Scheme_Env *env)
{
  scheme_do_add_global_symbol(env, name, obj, 1, 1);
}

void
scheme_add_global_keyword(const char *name, Scheme_Object *obj, 
			  Scheme_Env *env)
{
  scheme_do_add_global_symbol(env, scheme_intern_symbol(name), obj, 0, 0);
}

void
scheme_add_global_keyword_symbol(Scheme_Object *name, Scheme_Object *obj, 
				 Scheme_Env *env)
{
  scheme_do_add_global_symbol(env, name, obj, 0, 0);
}

void scheme_shadow(Scheme_Env *env, Scheme_Object *n, int stxtoo)
{
  if (env->rename) {
    scheme_remove_module_rename(env->rename, n);
    if (env->module) {
      scheme_extend_module_rename(env->rename,
				  env->module->self_modidx,
				  n, n,
				  env->module->self_modidx,
				  n,
				  env->mod_phase,
				  0);
    }
  }

  if (stxtoo) {
    if (!env->module || env->rename) {
      if (!env->shadowed_syntax) {
	Scheme_Hash_Table *ht;
	ht = scheme_make_hash_table(SCHEME_hash_ptr);
	env->shadowed_syntax = ht;
      }
      
      scheme_hash_set(env->shadowed_syntax, n, scheme_true);
    }
  } else {
    if (env->shadowed_syntax)
      scheme_hash_set(env->shadowed_syntax, n, NULL);
  }
}

/********** Auxilliary tables **********/

Scheme_Object **scheme_make_builtin_references_table(void)
{
  Scheme_Bucket_Table *ht;
  Scheme_Object **t;
  Scheme_Bucket **bs;
  long i;

  t = MALLOC_N(Scheme_Object *, (builtin_ref_counter + 1));
#ifdef MEMORY_COUNTING_ON
  scheme_misc_count += sizeof(Scheme_Object *) * (builtin_ref_counter + 1);
#endif

  ht = scheme_initial_env->toplevel;

  bs = ht->buckets;

  for (i = ht->size; i--; ) {
    Scheme_Bucket *b = bs[i];
    if (b && (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_HAS_REF_ID))
      t[((Scheme_Bucket_With_Ref_Id *)b)->id] = (Scheme_Object *)b->val;
  }

  return t;
}

Scheme_Hash_Table *scheme_map_constants_to_globals(void)
{
  Scheme_Bucket_Table *ht;
  Scheme_Hash_Table*result;
  Scheme_Bucket **bs;
  long i;

  ht = scheme_initial_env->toplevel;
  bs = ht->buckets;

  result = scheme_make_hash_table(SCHEME_hash_ptr);

  for (i = ht->size; i--; ) {
    Scheme_Bucket *b = bs[i];
    if (b && (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_IS_CONST))
      scheme_hash_set(result, b->val, (Scheme_Object *)b);
  }

  return result;
}

/*========================================================================*/
/*        compile-time env, constructors and simple queries               */
/*========================================================================*/

static void init_compile_data(Scheme_Comp_Env *env)
{
  Compile_Data *data;
  int i, c, *use;

  c = env->num_bindings;
  if (c)
    use = MALLOC_N_ATOMIC(int, c);
  else
    use = NULL;

  data = COMPILE_DATA(env);

  data->use = use;
  for (i = 0; i < c; i++) {
    use[i] = 0;
  }
}

Scheme_Comp_Env *scheme_new_compilation_frame(int num_bindings, int flags,
					      Scheme_Comp_Env *base, Scheme_Object *certs)
{
  Scheme_Comp_Env *frame;
  int count;
  
  count = num_bindings;

  frame = (Scheme_Comp_Env *)MALLOC_ONE_RT(Scheme_Full_Comp_Env);
#ifdef MZTAG_REQUIRED
  frame->type = scheme_rt_comp_env;
#endif

  {
    Scheme_Object **vals;
    vals = MALLOC_N(Scheme_Object *, count);
    frame->values = vals;
  }

  frame->certs = certs;
  frame->num_bindings = num_bindings;
  frame->flags = flags | (base->flags & SCHEME_NO_RENAME);
  frame->next = base;
  frame->genv = base->genv;
  frame->insp = base->insp;
  frame->prefix = base->prefix;
  frame->in_modidx = base->in_modidx;

  if (flags & SCHEME_NON_SIMPLE_FRAME)
    frame->skip_depth = 0;
  else if (base->next)
    frame->skip_depth = base->skip_depth + 1;
  else
    frame->skip_depth = 0;

  init_compile_data(frame);

  return frame;
}

Scheme_Comp_Env *scheme_new_comp_env(Scheme_Env *genv, Scheme_Object *insp, int flags)
{
  Scheme_Comp_Env *e;
  Comp_Prefix *cp;

  if (!insp)
    insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);

  e = (Scheme_Comp_Env *)MALLOC_ONE_RT(Scheme_Full_Comp_Env);
#ifdef MZTAG_REQUIRED
  e->type = scheme_rt_comp_env;
#endif
  e->num_bindings = 0;
  e->next = NULL;
  e->genv = genv;
  e->insp = insp;
  e->flags = flags;
  init_compile_data(e);

  cp = MALLOC_ONE_RT(Comp_Prefix);
#ifdef MZTAG_REQUIRED
  cp->type = scheme_rt_comp_prefix;
#endif

  e->prefix = cp;

  return e;
}

Scheme_Comp_Env *scheme_new_expand_env(Scheme_Env *genv, Scheme_Object *insp, int flags)
{
  Scheme_Comp_Env *e;

  e = scheme_new_comp_env(genv, insp, flags);
  e->prefix = NULL;

  return e;
}

int scheme_is_sub_env(Scheme_Comp_Env *stx_env, Scheme_Comp_Env *env)
{
  Scheme_Comp_Env *se;

  for (se = stx_env; NOT_SAME_OBJ(se, env); se = se->next) {
    if (!(se->flags & SCHEME_FOR_INTDEF))
      break;
  }
  return SAME_OBJ(se, env);
}

int scheme_used_ever(Scheme_Comp_Env *env, int which)
{
  Compile_Data *data = COMPILE_DATA(env);

  return !!data->use[which];
}

int scheme_is_env_variable_boxed(Scheme_Comp_Env *env, int which)
{
  Compile_Data *data = COMPILE_DATA(env);

  return !!(data->use[which] & WAS_SET_BANGED);
}

void
scheme_add_compilation_binding(int index, Scheme_Object *val, Scheme_Comp_Env *frame)
{
  if ((index >= frame->num_bindings) || (index < 0))
    scheme_signal_error("internal error: scheme_add_binding: "
			"index out of range: %d", index);
  
  frame->values[index] = val;
  frame->skip_table = NULL;
}

void scheme_frame_captures_lifts(Scheme_Comp_Env *env, Scheme_Lift_Capture_Proc cp, Scheme_Object *data, 
                                 Scheme_Object *end_stmts, Scheme_Object *context_key)
{
  Scheme_Lift_Capture_Proc *pp;
  Scheme_Object *vec;
  
  pp = (Scheme_Lift_Capture_Proc *)scheme_malloc_atomic(sizeof(Scheme_Lift_Capture_Proc));
  *pp = cp;

  vec = scheme_make_vector(5, NULL);
  SCHEME_VEC_ELS(vec)[0] = scheme_null;
  SCHEME_VEC_ELS(vec)[1] = (Scheme_Object *)pp;
  SCHEME_VEC_ELS(vec)[2] = data;
  SCHEME_VEC_ELS(vec)[3] = end_stmts;
  SCHEME_VEC_ELS(vec)[4] = context_key;

  COMPILE_DATA(env)->lifts = vec;
}

Scheme_Object *scheme_frame_get_lifts(Scheme_Comp_Env *env)
{
  return SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[0];
}

Scheme_Object *scheme_frame_get_end_statement_lifts(Scheme_Comp_Env *env)
{
  return SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[3];
}

void scheme_add_local_syntax(int cnt, Scheme_Comp_Env *env)
{
  Scheme_Object **ns, **vs;
  
  if (cnt) {
    ns = MALLOC_N(Scheme_Object *, cnt);
    vs = MALLOC_N(Scheme_Object *, cnt);

    COMPILE_DATA(env)->num_const = cnt;
    COMPILE_DATA(env)->const_names = ns;
    COMPILE_DATA(env)->const_vals = vs;

  }
}

void scheme_set_local_syntax(int pos,
			     Scheme_Object *name, Scheme_Object *val,
			     Scheme_Comp_Env *env)
{
  COMPILE_DATA(env)->const_names[pos] = name;
  COMPILE_DATA(env)->const_vals[pos] = val;
  env->skip_table = NULL;
}

Scheme_Comp_Env *
scheme_add_compilation_frame(Scheme_Object *vals, Scheme_Comp_Env *env, int flags, Scheme_Object *certs)
{
  Scheme_Comp_Env *frame;
  int len, i, count;
  
  len = scheme_stx_list_length(vals);
  count = len;

  frame = scheme_new_compilation_frame(count, flags, env, certs);

  for (i = 0; i < len ; i++) {
    if (SCHEME_STX_SYMBOLP(vals))
      frame->values[i] = vals;
    else {
      Scheme_Object *a;
      a = SCHEME_STX_CAR(vals);
      frame->values[i] = a;
      vals = SCHEME_STX_CDR(vals);
    }
  }
  
  init_compile_data(frame);

  return frame;
}

Scheme_Comp_Env *scheme_no_defines(Scheme_Comp_Env *env)
{
  if (scheme_is_toplevel(env)
      || scheme_is_module_env(env)
      || scheme_is_module_begin_env(env)
      || (env->flags & SCHEME_INTDEF_FRAME))
    return scheme_new_compilation_frame(0, 0, env, NULL);
  else
    return env;
}

Scheme_Comp_Env *scheme_require_renames(Scheme_Comp_Env *env)
{
  if (env->flags & SCHEME_NO_RENAME) {
    env = scheme_new_compilation_frame(0, 0, env, NULL);
    env->flags -= SCHEME_NO_RENAME;
  }

  return env;
}

int scheme_is_toplevel(Scheme_Comp_Env *env)
{
  return !env->next || (env->flags & SCHEME_TOPLEVEL_FRAME);
}

int scheme_is_module_env(Scheme_Comp_Env *env)
{
  return !!(env->flags & SCHEME_MODULE_BEGIN_FRAME); /* name is backwards compared to symbol! */
}

int scheme_is_module_begin_env(Scheme_Comp_Env *env)
{
  return !!(env->flags & SCHEME_MODULE_FRAME); /* name is backwards compared to symbol! */
}

Scheme_Comp_Env *scheme_extend_as_toplevel(Scheme_Comp_Env *env)
{
  if (scheme_is_toplevel(env))
    return env;
  else
    return scheme_new_compilation_frame(0, SCHEME_TOPLEVEL_FRAME, env, NULL);
}

static Scheme_Object *make_toplevel(mzshort depth, int position, int resolved, int flags)
{
  Scheme_Toplevel *tl;
  Scheme_Object *v, *pr;

  /* Important: non-resolved can't be cached, because the ISCONST
     field is modified to track mutated module-level variables. But
     the value for a specific toplevel is cached in the environment
     layer. */

  if (resolved) {
    if ((depth < MAX_CONST_TOPLEVEL_DEPTH)
	&& (position < MAX_CONST_TOPLEVEL_POS))
      return toplevels[depth][position][flags];

    pr = (flags
	  ? scheme_make_pair(scheme_make_integer(position),
			     scheme_make_integer(flags))
	  : scheme_make_integer(position));
    pr = scheme_make_pair(scheme_make_integer(depth), pr);
    v = scheme_hash_get_atomic(toplevels_ht, pr);
    if (v)
      return v;
  } else
    pr = NULL;

  tl = (Scheme_Toplevel *)scheme_malloc_atomic_tagged(sizeof(Scheme_Toplevel));
  tl->iso.so.type = (resolved ? scheme_toplevel_type : scheme_compiled_toplevel_type);
  tl->depth = depth;
  tl->position = position;
  SCHEME_TOPLEVEL_FLAGS(tl) = flags;

  if (resolved) {
    if (toplevels_ht->count > TABLE_CACHE_MAX_SIZE) {
      toplevels_ht = scheme_make_hash_table_equal();
    }
    scheme_hash_set_atomic(toplevels_ht, pr, (Scheme_Object *)tl);
  }

  return (Scheme_Object *)tl;
}

Scheme_Object *scheme_register_toplevel_in_prefix(Scheme_Object *var, Scheme_Comp_Env *env,
						  Scheme_Compile_Info *rec, int drec)
{
  Comp_Prefix *cp = env->prefix;
  Scheme_Hash_Table *ht;
  Scheme_Object *o;

  if (rec && rec[drec].dont_mark_local_use) {
    /* Make up anything; it's going to be ignored. */
    return make_toplevel(0, 0, 0, 0);
  }

  ht = cp->toplevels;
  if (!ht) {
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    cp->toplevels = ht;
  }

  o = scheme_hash_get(ht, var);
  if (o)
    return o;

  o = make_toplevel(0, cp->num_toplevels, 0, 0);

  cp->num_toplevels++;
  scheme_hash_set(ht, var, o);

  return o;
}

Scheme_Object *scheme_toplevel_to_flagged_toplevel(Scheme_Object *_tl, int flags)
{
  Scheme_Toplevel *tl = (Scheme_Toplevel *)_tl;
  return make_toplevel(tl->depth, tl->position, 0, flags);
}

Scheme_Object *scheme_register_stx_in_prefix(Scheme_Object *var, Scheme_Comp_Env *env, 
					     Scheme_Compile_Info *rec, int drec)
{
  Comp_Prefix *cp = env->prefix;
  Scheme_Local *l;
  Scheme_Object *o;
  int pos;

  if (rec && rec[drec].dont_mark_local_use) {
    /* Make up anything; it's going to be ignored. */
    l = (Scheme_Local *)scheme_malloc_atomic_tagged(sizeof(Scheme_Local));
    l->so.type = scheme_compiled_quote_syntax_type;
    l->position = 0;

    return (Scheme_Object *)l;
  }

  if (!cp->stxes) {
    Scheme_Hash_Table *ht;
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    cp->stxes = ht;
  }

  pos = cp->num_stxes;

  l = (Scheme_Local *)scheme_malloc_atomic_tagged(sizeof(Scheme_Local));
  l->so.type = scheme_compiled_quote_syntax_type;
  l->position = pos;

  cp->num_stxes++;
  o = (Scheme_Object *)l;
  
  scheme_hash_set(cp->stxes, var, o);

  return o;
}

/*========================================================================*/
/*                     compile-time env, lookup bindings                  */
/*========================================================================*/

static Scheme_Object *alloc_local(short type, int pos)
{
  Scheme_Object *v;

  v = (Scheme_Object *)scheme_malloc_atomic_tagged(sizeof(Scheme_Local));
  v->type = type;
  SCHEME_LOCAL_POS(v) = pos;

  return (Scheme_Object *)v;
}

Scheme_Object *scheme_make_local(Scheme_Type type, int pos)
{
  int k;
  Scheme_Object *v;

  k = type - scheme_local_type;

  if (pos < MAX_CONST_LOCAL_POS) {
    if (pos >= 0)
      return scheme_local[pos][k];
  }

  v = scheme_hash_get(locals_ht[k], scheme_make_integer(pos));
  if (v)
    return v;

  v = alloc_local(type, pos);

  if (locals_ht[k]->count > TABLE_CACHE_MAX_SIZE) {
    Scheme_Hash_Table *ht;
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    locals_ht[k] = ht;
  }

  scheme_hash_set(locals_ht[k], scheme_make_integer(pos), v);

  return v;
}

static Scheme_Object *force_lazy_macro(Scheme_Object *val, long phase)
{
  Lazy_Macro_Fun f = (Lazy_Macro_Fun)SCHEME_PTR1_VAL(val);
  Scheme_Object *data = SCHEME_PTR2_VAL(val);
  return f(data, phase);
}

static Scheme_Local *get_frame_loc(Scheme_Comp_Env *frame,
				   int i, int j, int p, int flags)
/* Generates a Scheme_Local record for a static distance coodinate, and also
   marks the variable as used for closures. */
{
  int cnt, u;

  u = COMPILE_DATA(frame)->use[i];
  
  u |= (((flags & (SCHEME_APP_POS | SCHEME_SETTING | SCHEME_REFERENCING))
	 ? CONSTRAINED_USE
	 : ((u & (ARBITRARY_USE | ONE_ARBITRARY_USE)) ? ARBITRARY_USE : ONE_ARBITRARY_USE))
	| ((flags & (SCHEME_SETTING | SCHEME_REFERENCING | SCHEME_LINKING_REF))
	   ? WAS_SET_BANGED
	   : 0));

  cnt = ((u & SCHEME_USE_COUNT_MASK) >> SCHEME_USE_COUNT_SHIFT);
  if (cnt < SCHEME_USE_COUNT_INF)
    cnt++;
  u -= (u & SCHEME_USE_COUNT_MASK);
  u |= (cnt << SCHEME_USE_COUNT_SHIFT);
  
  COMPILE_DATA(frame)->use[i] = u;

  return (Scheme_Local *)scheme_make_local(scheme_local_type, p + i);
}

Scheme_Object *scheme_hash_module_variable(Scheme_Env *env, Scheme_Object *modidx, 
					   Scheme_Object *stxsym, Scheme_Object *insp,
					   int pos, int mod_phase)
{
  Scheme_Object *val;
  Scheme_Hash_Table *ht;

  if (!env->modvars) {
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    env->modvars = ht;
  }

  stxsym = SCHEME_STX_SYM(stxsym);

  ht = (Scheme_Hash_Table *)scheme_hash_get(env->modvars, modidx);

  if (!ht) {
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    scheme_hash_set(env->modvars, modidx, (Scheme_Object *)ht);
  }

  /* Loop for inspector-specific hash table, maybe: */
  while (1) {
    
    val = scheme_hash_get(ht, stxsym);
    
    if (!val) {
      Module_Variable *mv;
      
      mv = MALLOC_ONE_TAGGED(Module_Variable);
      mv->so.type = scheme_module_variable_type;
      
      mv->modidx = modidx;
      mv->sym = stxsym;
      mv->insp = insp;
      mv->pos = pos;
      mv->mod_phase = mod_phase;
      
      val = (Scheme_Object *)mv;
      
      scheme_hash_set(ht, stxsym, val);
      
      break;
    } else {
      /* Check that inspector is the same. */
      Module_Variable *mv = (Module_Variable *)val;
      
      if (!SAME_OBJ(mv->insp, insp)) {
	/* Need binding for a different inspector. Try again. */
	val = scheme_hash_get(ht, insp);
	if (!val) {
	  Scheme_Hash_Table *ht2;
	  /* Make a table for this specific inspector */
	  ht2 = scheme_make_hash_table(SCHEME_hash_ptr);
	  scheme_hash_set(ht, insp, (Scheme_Object *)ht2);
	  ht = ht2;
	  /* loop... */
	} else
	  ht = (Scheme_Hash_Table *)val;
      } else
	break;
    }
  }

  return val;
}

Scheme_Object *scheme_tl_id_sym(Scheme_Env *env, Scheme_Object *id, Scheme_Object *bdg, int is_def)
/* The `env' argument can actually be a hash table. */
{
  Scheme_Object *marks = NULL, *sym, *map, *l, *a, *amarks, *m, *best_match, *cm, *abdg;
  int best_match_skipped, ms, one_mark;
  Scheme_Hash_Table *marked_names;

  sym = SCHEME_STX_SYM(id);

  if (SCHEME_HASHTP((Scheme_Object *)env))
    marked_names = (Scheme_Hash_Table *)env;
  else {
    /* If there's no table and we're not defining, bail out fast */
    if (!is_def && !env->marked_names)
      return sym;
    marked_names = env->marked_names;
  }

  if (is_def) {
    /* If we're defining, see if we need to create a table.  Getting
       marks is relatively expensive, but we only do this once per
       definition. */
    if (!bdg)
      bdg = scheme_stx_moduleless_env(id, 0 /* renames currently don't depend on phase */);
    marks = scheme_stx_extract_marks(id);
    if (SCHEME_NULLP(marks) && SCHEME_FALSEP(bdg))
      return sym;
  }

  if (!marked_names) {
    marked_names = scheme_make_hash_table(SCHEME_hash_ptr);
    env->marked_names = marked_names;
  }
  
  map = scheme_hash_get(marked_names, sym);

  if (!map) {
    /* If we're not defining, we can bail out before extracting marks. */
    if (!is_def)
      return sym;
    else
      map = scheme_null;
  }

  if (!bdg) {
    /* We need lexical binding, if any, too: */
    bdg = scheme_stx_moduleless_env(id, 0 /* renames currently don't depend on phase */);
  }

  if (!marks) {
    /* We really do need the marks. Get them. */
    marks = scheme_stx_extract_marks(id);
    if (SCHEME_NULLP(marks) && SCHEME_FALSEP(bdg))
      return sym;
  }

  best_match = NULL;
  best_match_skipped = scheme_list_length(marks);
  if (best_match_skipped == 1) {
    /* A mark list of length 1 is the common case.
       Since the list is otherwise marshaled into .zo, etc.,
       simplify by extracting just the mark: */
    marks = SCHEME_CAR(marks);
    one_mark = 1;
  } else
    one_mark = 0;

  if (!SCHEME_TRUEP(bdg))
    bdg = NULL;

  /* Find a mapping that matches the longest tail of marks */
  for (l = map; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    a = SCHEME_CAR(l);
    amarks = SCHEME_CAR(a);

    if (SCHEME_VECTORP(amarks)) {
      abdg = SCHEME_VEC_ELS(amarks)[1];
      amarks = SCHEME_VEC_ELS(amarks)[0];
    } else
      abdg = NULL;

    if (SAME_OBJ(abdg, bdg)) {
      if (is_def) {
	if (scheme_equal(amarks, marks)) {
	  best_match = SCHEME_CDR(a);
	  break;
	}
      } else {
	if (!SCHEME_PAIRP(marks)) {
	  /* To be better than nothing, could only match exactly: */
	  if (scheme_equal(amarks, marks)) {
	    best_match = SCHEME_CDR(a);
	    best_match_skipped = 0;
	  }
	} else {
	  /* amarks can match a tail of marks: */
	  for (m = marks, ms = 0; 
	       SCHEME_PAIRP(m) && (ms < best_match_skipped);
	       m = SCHEME_CDR(m), ms++) {

	    cm = m;
	    if (!SCHEME_PAIRP(amarks)) {
	      /* If we're down to the last element
		 of marks, then extract it to try to
		 match the symbol amarks. */
	      if (SCHEME_NULLP(SCHEME_CDR(m)))
		cm = SCHEME_CAR(m);
	    }
  
	    if (scheme_equal(amarks, cm)) {
	      best_match = SCHEME_CDR(a);
	      best_match_skipped = ms;
	      break;
	    }
	  }
	}
      }
    }
  }

  if (!best_match) {
    if (!is_def) {
      return sym;
    }

    /* Last chance before making up a new name. If we're processing a
       module body generated by `expand', then we picked a name last
       time around. We can't pick a new name now, otherwise
       "redundant" module renamings wouldn't be redundant. (See
       simpify in stxobj.c.) So check for a context-determined
       existing rename. */
    if (!SCHEME_HASHTP((Scheme_Object *)env) && env->module && (is_def != 2)) {
      Scheme_Object *mod, *nm = id;
      mod = scheme_stx_module_name(&nm, env->phase, NULL, NULL, NULL);
      if (mod /* must refer to env->module, otherwise there would
		 have been an error before getting here */
	  && NOT_SAME_OBJ(nm, sym))
	/* It has a rename already! */
	best_match = nm;
    }

    /* Adding a definition. We "gensym" here in a sense; actually, we
       use a symbol table that's in parallel to the normal table, so
       that we get the same parallel-symbol when unmarshalling
       code. We use a counter attached to the environment. Normally,
       this counter just increments, but if a module is re-expanded,
       then the counter starts at 0 for the re-expand, and we may
       re-pick an existing name. To avoid re-picking the same name,
       double-check for a mapping in the environment by inspecting the
       renames attached to id. In the top-level environment, it's
       still possible to get a collision, because separately compiled
       code might be loaded into the same environment (which is just
       too bad). */
    if (!best_match) {
      char onstack[50], *buf;
      int len;

      while (1) {
	env->id_counter++;
	len = SCHEME_SYM_LEN(sym);
	if (len <= 35)
	  buf = onstack;
	else
	  buf = scheme_malloc_atomic(len + 15);
	memcpy(buf, SCHEME_SYM_VAL(sym), len);
	
	/* The dot here is significant; it might gets stripped away when
	   printing the symbol */
	sprintf(buf + len, ".%d", env->id_counter);
	
	best_match = scheme_intern_exact_parallel_symbol(buf, strlen(buf));

	if (!scheme_stx_parallel_is_used(best_match, id)) {
	  /* Also check environment's rename tables. This
	     last check turns out to matter for compiling in
	     `module->namespace' contexts, because no renaming
	     is added after expansion to record the rename table. */
	  if (!scheme_tl_id_is_sym_used(marked_names, best_match)) {
	    /* Ok, no matches, so this name is fine. */
	    break;
	  }
	}

	/* Otherwise, increment counter and try again... */
      }
    }
    if (bdg) {
      a = scheme_make_vector(2, NULL);
      SCHEME_VEC_ELS(a)[0] = marks;
      SCHEME_VEC_ELS(a)[1] = bdg;
      marks = a;
    }
    a = scheme_make_pair(marks, best_match);
    map = scheme_make_pair(a, map);
    
    scheme_hash_set(marked_names, sym, map);
  }

  return best_match;
}

int scheme_tl_id_is_sym_used(Scheme_Hash_Table *marked_names, Scheme_Object *sym)
{
  int i;
  Scheme_Object *l, *a;

  if (!marked_names)
    return 0;

  for (i = marked_names->size; i--; ) {
    l = marked_names->vals[i];
    if (l) {
      for (; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
	a = SCHEME_CAR(l);
	if (SAME_OBJ(sym, SCHEME_CDR(a)))
	  return 1;
      }
    }
  }

  return 0;
}

static Scheme_Object *make_uid()
{
  char name[20];

  sprintf(name, "env%d", env_uid_counter++);
  return scheme_make_symbol(name); /* uninterned! */
}

Scheme_Object *scheme_env_frame_uid(Scheme_Comp_Env *env)
{
  if (env->flags & (SCHEME_NO_RENAME | SCHEME_CAPTURE_WITHOUT_RENAME | SCHEME_CAPTURE_LIFTED))
    return NULL;

  if (!env->uid) {
    Scheme_Object *sym;
    sym = make_uid();
    env->uid = sym;
  }
  return env->uid;
}

static void make_env_renames(Scheme_Comp_Env *env, int rcount, int rstart, int rstart_sec, int force_multi,
			     Scheme_Object *stx)
{
  Scheme_Object *rnm;
  Scheme_Object *uid = NULL;
  int i, pos;

  if (env->flags & (SCHEME_NO_RENAME | SCHEME_CAPTURE_WITHOUT_RENAME | SCHEME_CAPTURE_LIFTED))
    return;

  scheme_env_frame_uid(env);

  if (force_multi) {
    if (env->num_bindings && !env->uids) {
      Scheme_Object **uids;
      uids = MALLOC_N(Scheme_Object *, env->num_bindings);
      env->uids = uids;
    }
    if (COMPILE_DATA(env)->num_const && !COMPILE_DATA(env)->const_uids) {
      Scheme_Object **cuids;
      cuids = MALLOC_N(Scheme_Object *, COMPILE_DATA(env)->num_const);
      COMPILE_DATA(env)->const_uids = cuids;
    }
    if (env->uid && !SCHEME_FALSEP(env->uid)) {
      uid = env->uid;
      env->uid = scheme_false;
    }
  }

  if (!uid) {
    if (env->uid && SCHEME_TRUEP(env->uid)) {
      /* single-uid mode (at least for now) */
      uid = env->uid;
    } else {
      /* multi-uid mode */
      if (!rstart_sec)
	uid = COMPILE_DATA(env)->const_uids[rstart];
      else
	uid = env->uids[rstart];
      if (!uid)
	uid = make_uid();
    }
  }
  
  rnm = scheme_make_rename(uid, rcount);
  pos = 0;

  if (!rstart_sec) {
    for (i = rstart; (i < COMPILE_DATA(env)->num_const) && (pos < rcount); i++, pos++) {
      if (COMPILE_DATA(env)->const_uids)
	COMPILE_DATA(env)->const_uids[i] = uid;
      scheme_set_rename(rnm, pos, COMPILE_DATA(env)->const_names[i]);
    }
    rstart = 0;
  }
  for (i = rstart; pos < rcount; i++, pos++) {
    if (env->uids)
      env->uids[i] = uid;
    scheme_set_rename(rnm, pos, env->values[i]);
  }

  if (SCHEME_RIBP(stx))
    scheme_add_rib_rename(stx, rnm);
  
  if (env->renames) {
    if (SCHEME_PAIRP(env->renames) || SCHEME_NULLP(env->renames))
      rnm = scheme_make_pair(rnm, env->renames);
    else
      rnm = scheme_make_pair(rnm, scheme_make_pair(env->renames, scheme_null));
  }
  env->renames = rnm;
}

Scheme_Object *scheme_add_env_renames(Scheme_Object *stx, Scheme_Comp_Env *env, 
				      Scheme_Comp_Env *upto)
{
  if (!SCHEME_STXP(stx) && !SCHEME_RIBP(stx)) {
    scheme_signal_error("internal error: not syntax or rib");
    return NULL;
  }

  while (env != upto) {
    if (!(env->flags & (SCHEME_NO_RENAME | SCHEME_CAPTURE_WITHOUT_RENAME | SCHEME_CAPTURE_LIFTED))) {
      int i, count;
      
      /* How many slots filled in the frame so far?  This can change
	 due to the style of let* compilation, which generates a
	 rename record after each binding set. The "const" bindings
	 are always all in place before we generate any renames in
	 that case. However, the "const" bindings can grow by
	 themselves before non-const bindings are installed. */
      count = COMPILE_DATA(env)->num_const;
      for (i = env->num_bindings; i--; ) {
	if (env->values[i])
	  count++;
      }
      
      if (count) {
	Scheme_Object *l;

	if (!env->renames || (env->rename_var_count != count)) {
	  /* Need to create lexical renaming record(s). We create
	     multiple records as necessary to avoid uids that contain
	     more than one variable with the same symbol name.

	     This is complicated, because we don't want to allocate a
	     hash table in the common case of a binding set with a few
	     names. It's also complicated by incremental rename
	     building: if env->rename_var_count is not zero, we've
	     done this before for a subset of `values' (and there are
	     no consts in that case). In the incremental case, we have
	     a dup_check hash table left from the previous round. */
	  Scheme_Hash_Table *ht;
	  Scheme_Object *name;
	  int rcount = 0, rstart, rstart_sec = 0, vstart;
	  
	  /* rstart is where the to-be-created rename table starts
	     (saved from last time around, or initially zero).
	     vstart is where we start looking for new dups.
	     rstart_sec is TRUE when the new frame starts in the
	     non-constant area. */
	  rstart = env->rename_rstart;
	  if (env->renames) {
	    /* Incremental mode. Drop the most recent (first) rename
               table, because we'll recreate it: */
	    if (SCHEME_PAIRP(env->renames))
	      env->renames = SCHEME_CDR(env->renames);
	    else
	      env->renames = NULL;
	    if (SCHEME_RIBP(stx))
	      scheme_drop_first_rib_rename(stx);
	    vstart = env->rename_var_count;
	    rstart_sec = 1;
	    /* We already know that the first rcount
	       are distinct (from the last iteration) */
	    rcount = vstart - rstart;
	  } else
	    vstart = 0;

	  /* Create or find the hash table: */
	  if (env->dup_check)
	    ht = env->dup_check;
	  else if (env->num_bindings + COMPILE_DATA(env)->num_const > 10)
	    ht = scheme_make_hash_table(SCHEME_hash_ptr);
	  else
	    ht = NULL;

	  if (rcount > 16) {
	    /* Instead of n^2 growth for the rename, just close the current
	       one off and start fresh. */
	    make_env_renames(env, rcount, rstart, rstart_sec, 1, stx);
	    rcount = 0;
	    rstart = vstart;
	    rstart_sec = 1;
	    if (ht) {
	      /* Flush the table for a new set: */
	      ht = scheme_make_hash_table(SCHEME_hash_ptr);
	    }
	  }
	  
	  /* Check for dups among the statics, and build a rename for
             each dup-free set. */

	  /* First: constants. */
	  if (!rstart_sec) {
	    if (COMPILE_DATA(env)->num_const) {
	      /* Start at the beginning, always. */
	      for (i = 0; i < COMPILE_DATA(env)->num_const; i++) {
		int found = 0;
		name = SCHEME_STX_VAL(COMPILE_DATA(env)->const_names[i]);
		if (ht) {
		  if (scheme_hash_get(ht, name))
		    found = 1;
		  else
		    scheme_hash_set(ht, name, scheme_true);
		} else {
		  int j;
		  for (j = rstart; j < i; j++) {
		    if (SAME_OBJ(name, SCHEME_STX_VAL(COMPILE_DATA(env)->const_names[j]))) {
		      found = 1;
		      break;
		    }
		  }
		}

		if (found) {
		  make_env_renames(env, rcount, rstart, rstart_sec, 1, stx);
		  rcount = 1;
		  rstart = i;
		  if (ht) {
		    /* Flush the table for a new set: */
		    ht = scheme_make_hash_table(SCHEME_hash_ptr);
		    scheme_hash_set(ht, name, scheme_true);
		  }
		} else
		  rcount++;
	      }
	    } else 
	      rstart_sec = 1;
	  }

	  for (i = vstart; (i < env->num_bindings) && env->values[i]; i++) {
	    int found = 0;
	    name = SCHEME_STX_VAL(env->values[i]);

	    if (ht) {
	      if (scheme_hash_get(ht, name))
		found = 1;
	      else
		scheme_hash_set(ht, name, scheme_true);
	    } else {
	      int j;
	      if (!rstart_sec) {
		/* Look in consts, first: */
		for (j = rstart; j < COMPILE_DATA(env)->num_const; j++) {
		  if (SAME_OBJ(name, SCHEME_STX_VAL(COMPILE_DATA(env)->const_names[j]))) {
		    found = 1;
		    break;
		  }
		}

		j = 0;
	      } else
		j = rstart;

	      if (!found) {
		for (; j < i; j++) {
		  if (SAME_OBJ(name, SCHEME_STX_VAL(env->values[j]))) {
		    found = 1;
		    break;
		  }
		}
	      }
	    }

	    if (found) {
	      make_env_renames(env, rcount, rstart, rstart_sec, 1, stx);
	      rcount = 1;
	      rstart = i;
	      rstart_sec = 1;
	      if (ht) {
		/* Flush the table for a new set: */
		ht = scheme_make_hash_table(SCHEME_hash_ptr);
		scheme_hash_set(ht, name, scheme_true);
	      }
	    } else
	      rcount++;
	  }
	  
	  make_env_renames(env, rcount, rstart, rstart_sec, 0, stx);

	  env->rename_var_count = count;
	  env->rename_rstart = rstart;
	  if (count < env->num_bindings) {
	    /* save for next time around: */
	    env->dup_check = ht;
	  } else { 
	    /* drop a saved table if there; we're done with all increments */
	    env->dup_check = NULL;
	  }
	}

	if (SCHEME_STXP(stx)) {
	  for (l = env->renames; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
	    stx = scheme_add_rename(stx, SCHEME_CAR(l));
	  }
	  if (!SCHEME_NULLP(l))
	    stx = scheme_add_rename(stx, l);
	}
      }
    }

    env = env->next;
  }

  return stx;
}

void scheme_seal_env_renames(Scheme_Comp_Env *env)
{
  env->dup_check = NULL;
}

/*********************************************************************/

void create_skip_table(Scheme_Comp_Env *start_frame)
{
  Scheme_Comp_Env *end_frame, *frame;
  int depth, dj = 0, dp = 0, i;
  Scheme_Hash_Table *table;
  int stride = 0;

  depth = start_frame->skip_depth;

  /* Find frames to be covered by the skip table.
     The theory here is the same as the `mapped' table
     in Scheme_Cert (see stxobj.c) */
  for (end_frame = start_frame->next;
       end_frame && ((depth & end_frame->skip_depth) != end_frame->skip_depth);
       end_frame = end_frame->next) {
    stride++;
  }

  table = scheme_make_hash_table(SCHEME_hash_ptr);
  
  for (frame = start_frame; frame != end_frame; frame = frame->next) {
    if (frame->flags & SCHEME_LAMBDA_FRAME)
      dj++;
    dp += frame->num_bindings;
    for (i = frame->num_bindings; i--; ) {
      if (frame->values[i]) {
	scheme_hash_set(table, SCHEME_STX_VAL(frame->values[i]), scheme_true);
      }
    }
    for (i = COMPILE_DATA(frame)->num_const; i--; ) {
      scheme_hash_set(table, SCHEME_STX_VAL(COMPILE_DATA(frame)->const_names[i]), scheme_true);
    }
  }

  scheme_hash_set(table, scheme_make_integer(0), (Scheme_Object *)end_frame);
  scheme_hash_set(table, scheme_make_integer(1), scheme_make_integer(dj));
  scheme_hash_set(table, scheme_make_integer(2), scheme_make_integer(dp));

  start_frame->skip_table = table;
}

/*********************************************************************/
/* 

   scheme_lookup_binding() is the main resolver of lexical, module,
   and top-level bindings. Depending on the value of `flags', it can
   return a value whose type tag is:

     scheme_macro_type (id was bound to syntax),

     scheme_macro_set_type (id was bound to a set!-transformer),

     scheme_macro_id_type (id was bound to a rename-transformer),

     scheme_local_type (id was lexical),

     scheme_variable_type (id is a global or module-bound variable),
     or

     scheme_module_variable_type (id is a module-bound variable).

*/

Scheme_Object *
scheme_lookup_binding(Scheme_Object *find_id, Scheme_Comp_Env *env, int flags,
		      Scheme_Object *certs, Scheme_Object *in_modidx,
		      Scheme_Env **_menv, int *_protected,
                      Scheme_Object **_lexical_binding_id)
{
  Scheme_Comp_Env *frame;
  int j = 0, p = 0, modpos, skip_stops = 0, mod_defn_phase, module_self_reference = 0;
  Scheme_Bucket *b;
  Scheme_Object *val, *modidx, *modname, *src_find_id, *find_global_id;
  Scheme_Env *genv;
  long phase;

  /* Need to know the phase being compiled */
  phase = env->genv->phase;

  /* Walk through the compilation frames */
  for (frame = env; frame->next != NULL; frame = frame->next) {
    int i;
    Scheme_Object *uid;

    while (1) {
      if (frame->skip_table) {
	if (!scheme_hash_get(frame->skip_table, SCHEME_STX_VAL(find_id))) {
	  /* Skip ahead. 0 maps to frame, 1 maps to j delta, and 2 maps to p delta */
	  val = scheme_hash_get(frame->skip_table, scheme_make_integer(1));
	  j += SCHEME_INT_VAL(val);
	  val = scheme_hash_get(frame->skip_table, scheme_make_integer(2));
	  p += SCHEME_INT_VAL(val);
	  frame = (Scheme_Comp_Env *)scheme_hash_get(frame->skip_table, scheme_make_integer(0));
	} else
	  break;
      } else if (frame->skip_depth && !(frame->skip_depth & 0x1F)) {
	/* We're some multiple of 32 frames deep. Build a skip table and try again. */
	create_skip_table(frame);
      } else
	break;
    }
    
    if (frame->flags & SCHEME_LAMBDA_FRAME)
      j++;

    if (!skip_stops || !(frame->flags & SCHEME_FOR_STOPS)) {
      if (frame->flags & SCHEME_FOR_STOPS)
	skip_stops = 1;

      uid = scheme_env_frame_uid(frame);

      for (i = frame->num_bindings; i--; ) {
	if (frame->values[i]) {
	  if (frame->uids) 
	    uid = frame->uids[i];
	  if (SAME_OBJ(SCHEME_STX_VAL(find_id), SCHEME_STX_VAL(frame->values[i]))
	      && (scheme_stx_env_bound_eq(find_id, frame->values[i], uid, phase)
		  || ((frame->flags & SCHEME_CAPTURE_WITHOUT_RENAME)
		      && scheme_stx_module_eq(find_id, frame->values[i], phase))
		  || ((frame->flags & SCHEME_CAPTURE_LIFTED)
		      && scheme_stx_bound_eq(find_id, frame->values[i], phase)))) {
	    /* Found a lambda-, let-, etc. bound variable: */
	    /* First, check certs (don't bind with fewer certs): */
	    if (!(flags & SCHEME_NO_CERT_CHECKS) 
		&& !(frame->flags & (SCHEME_CAPTURE_WITHOUT_RENAME | SCHEME_CAPTURE_LIFTED))) {
	      if (scheme_stx_has_more_certs(find_id, certs, frame->values[i], frame->certs)) {
		scheme_wrong_syntax(scheme_compile_stx_string, NULL, find_id,
				    "reference is more certified than binding");
		return NULL;
	      }
	    }
	    /* Looks ok; return a lexical reference */
            if (_lexical_binding_id) {
              if (!(frame->flags & SCHEME_CAPTURE_WITHOUT_RENAME))
                val = scheme_stx_remove_extra_marks(find_id, frame->values[i],
                                                    ((frame->flags & SCHEME_CAPTURE_LIFTED)
                                                     ? NULL
                                                     : uid));
              else
                val = find_id;
              *_lexical_binding_id = val;
            }
	    if (flags & SCHEME_DONT_MARK_USE)
	      return scheme_make_local(scheme_local_type, 0);
	    else
	      return (Scheme_Object *)get_frame_loc(frame, i, j, p, flags);
	  }
	}
      }

      for (i = COMPILE_DATA(frame)->num_const; i--; ) {
	int issame;
	if (frame->flags & SCHEME_CAPTURE_WITHOUT_RENAME)
	  issame = scheme_stx_module_eq(find_id, COMPILE_DATA(frame)->const_names[i], phase);
	else {
	  if (COMPILE_DATA(frame)->const_uids) uid = COMPILE_DATA(frame)->const_uids[i];
	  issame = (SAME_OBJ(SCHEME_STX_VAL(find_id), 
			     SCHEME_STX_VAL(COMPILE_DATA(frame)->const_names[i]))
		    && scheme_stx_env_bound_eq(find_id, COMPILE_DATA(frame)->const_names[i], uid, phase));
	}
      
	if (issame) {
	  if (!(flags & SCHEME_NO_CERT_CHECKS) 
	      && !(frame->flags & SCHEME_CAPTURE_WITHOUT_RENAME)) {
	    if (scheme_stx_has_more_certs(find_id, certs, COMPILE_DATA(frame)->const_names[i], frame->certs)) {
	      scheme_wrong_syntax(scheme_compile_stx_string, NULL, find_id,
				  "reference is more certified than binding");
	      return NULL;
	    }
	  }

	  val = COMPILE_DATA(frame)->const_vals[i];
	
	  if (!val) {
	    scheme_wrong_syntax(scheme_compile_stx_string, NULL, find_id,
				"variable used out of context");
	    return NULL;
	  }

	  if (SCHEME_FALSEP(val)) {
	    /* Corresponds to a run-time binding (but will be replaced later
	       through a renaming to a different binding) */
	    return NULL;
	  }

	  if (!(flags & SCHEME_ENV_CONSTANTS_OK)) {
	    if (SAME_TYPE(SCHEME_TYPE(val), scheme_macro_type))
	      return val;
	    else if (SAME_TYPE(SCHEME_TYPE(val), scheme_lazy_macro_type))
	      return force_lazy_macro(val, phase);
	    else
	      scheme_wrong_syntax(scheme_set_stx_string, NULL, find_id,
				  "local syntax identifier cannot be mutated");
	    return NULL;
	  }

	  return val;
	}
      }
    }

    p += frame->num_bindings;
  }

  src_find_id = find_id;
  modidx = scheme_stx_module_name(&find_id, phase, NULL, NULL, &mod_defn_phase);

  /* Used out of context? */
  if (SAME_OBJ(modidx, scheme_undefined)) {
    if (!(flags & SCHEME_OUT_OF_CONTEXT_OK))
      scheme_wrong_syntax(scheme_compile_stx_string, NULL, find_id,
			  "identifier used out of context");
    return NULL;
  }

  if (modidx) {
    /* If it's an access path, resolve it: */
    modname = scheme_module_resolve(modidx, 1);

    if (env->genv->module && SAME_OBJ(modname, env->genv->module->modname)) {
      modidx = NULL;
      modname = NULL;
      genv = env->genv;
      /* So we can distinguish between unbound variables in a module
	 and references to top-level definitions: */
      module_self_reference = 1;
    } else {
      genv = scheme_module_access(modname, env->genv, mod_defn_phase);

      if (!genv) {
	if (env->genv->phase) {
	  /* The failure might be due a laziness in required-syntax
	     execution. Force all laziness at the prior level 
	     and try again. */
	  scheme_module_force_lazy(env->genv, 1);
	  genv = scheme_module_access(modname, env->genv, mod_defn_phase);
	}

	if (!genv) {
	  scheme_wrong_syntax("require", NULL, src_find_id, 
			      "broken compiled code (phase %d, defn-phase %d): cannot find module %S",
			      env->genv->phase, mod_defn_phase, modname);
	  return NULL;
	}
      }
    }
  } else {
    genv = env->genv;
    modname = NULL;

    if (genv->module && !genv->rename) {
      /* Free variable. Maybe don't continue. */
      if (flags & (SCHEME_SETTING | SCHEME_REFERENCING)) {
	scheme_wrong_syntax(((flags & SCHEME_SETTING) 
			     ? scheme_set_stx_string
			     : scheme_var_ref_string),
			    NULL, src_find_id, "unbound variable in module");
	return NULL;
      }
      if (flags & SCHEME_NULL_FOR_UNBOUND)
	return NULL;
    }
  }

  if (_menv && genv->module)
    *_menv = genv;
  
  if (!modname && SCHEME_STXP(find_id))
    find_global_id = scheme_tl_id_sym(env->genv, find_id, NULL, 0);
  else
    find_global_id = find_id;

  /* Try syntax table: */
  if (modname) {
    val = scheme_module_syntax(modname, env->genv, find_id);
    if (val && !(flags & SCHEME_NO_CERT_CHECKS))
      scheme_check_accessible_in_module(genv, env->insp, in_modidx, 
					find_id, src_find_id, certs, NULL, -2, 0, 
					NULL);
  } else {
    /* Only try syntax table if there's not an explicit (later)
       variable mapping: */
    if (genv->shadowed_syntax 
	&& scheme_hash_get(genv->shadowed_syntax, find_global_id))
      val = NULL;
    else
      val = scheme_lookup_in_table(genv->syntax, (const char *)find_global_id);
  }
  
  if (val) {
    if (SAME_TYPE(SCHEME_TYPE(val), scheme_lazy_macro_type))
      return force_lazy_macro(val, phase);
    return val;
  }

  if (modname) {
    Scheme_Object *pos;
    if (flags & SCHEME_NO_CERT_CHECKS) 
      pos = 0;
    else
      pos = scheme_check_accessible_in_module(genv, env->insp, in_modidx, 
					      find_id, src_find_id, certs, NULL, -1, 1,
					      _protected);
    modpos = SCHEME_INT_VAL(pos);
  } else
    modpos = -1;

  if (modname && (flags & SCHEME_SETTING)) {
    if (SAME_OBJ(src_find_id, find_id) || SAME_OBJ(SCHEME_STX_SYM(src_find_id), find_id))
      find_id = NULL;
    scheme_wrong_syntax(scheme_set_stx_string, find_id, src_find_id, "cannot mutate module-required variable");
    return NULL;
  }

  if (!modname && (flags & (SCHEME_SETTING | SCHEME_REFERENCING)) && (genv->module && !genv->rename)) {
    /* Check for set! of unbound variable: */    
    if (!scheme_lookup_in_table(genv->toplevel, (const char *)find_global_id)) {
      scheme_wrong_syntax(((flags & SCHEME_SETTING) 
			     ? scheme_set_stx_string
			     : scheme_var_ref_string), 
			  NULL, src_find_id, "unbound variable in module");
      return NULL;
    }
  }

  if (!modname && (flags & SCHEME_NULL_FOR_UNBOUND)) {
    if (module_self_reference) {
      /* Since the module has a rename for this id, it's certainly defined. */
      if (!(flags & SCHEME_RESOLVE_MODIDS)) {
	/* This is the same thing as #%top handling in compile mode. But
	   for expand mode, it prevents wrapping the identifier with #%top. */
	/* Don't need a pos, because the symbol's gensym-ness (if any) will be
	   preserved within the module. */
	return scheme_hash_module_variable(genv, genv->module->self_modidx, find_id, 
					   genv->module->insp,
					   -1, genv->mod_phase);
      }
    } else
      return NULL;
  }

  /* Used to have `&& !SAME_OBJ(modidx, modname)' below, but that was a bad
     idea, because it causes module instances to be preserved. */
  if (modname && !(flags & SCHEME_RESOLVE_MODIDS) 
      && (!SAME_OBJ(modidx, kernel_symbol) || (flags & SCHEME_REFERENCING))) {
    /* Create a module variable reference, so that idx is preserved: */
    return scheme_hash_module_variable(env->genv, modidx, find_id, 
				       genv->module->insp,
				       modpos, mod_defn_phase);
  }

  if (!modname && (flags & (SCHEME_SETTING | SCHEME_REFERENCING)) && genv->module) {
    /* Need to return a variable reference in this case, too. */
    return scheme_hash_module_variable(env->genv, genv->module->self_modidx, find_global_id, 
				       genv->module->insp,
				       modpos, genv->mod_phase);
  }

  b = scheme_bucket_from_table(genv->toplevel, (char *)find_global_id);

  if ((flags & SCHEME_ELIM_CONST) && b && b->val 
      && (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_IS_CONST)
      && !(flags & SCHEME_GLOB_ALWAYS_REFERENCE))
    return (Scheme_Object *)b->val;

  ASSERT_IS_VARIABLE_BUCKET(b);
  if (!((Scheme_Bucket_With_Home *)b)->home)
    ((Scheme_Bucket_With_Home *)b)->home = genv;
  
  return (Scheme_Object *)b;
}

int *scheme_env_get_flags(Scheme_Comp_Env *frame, int start, int count)
{
  int *v, i;
  
  v = MALLOC_N_ATOMIC(int, count);
  memcpy(v, COMPILE_DATA(frame)->use + start, sizeof(int) * count);

  for (i = count; i--; ) {
    int old;
    old = v[i];
    v[i] = 0;
    if (old & (ARBITRARY_USE | ONE_ARBITRARY_USE | CONSTRAINED_USE)) {
      v[i] |= SCHEME_WAS_USED;
      if (!(old & (ARBITRARY_USE | WAS_SET_BANGED))) {
        if (old & ONE_ARBITRARY_USE)
          v[i] |= SCHEME_WAS_APPLIED_EXCEPT_ONCE;
        else
          v[i] |= SCHEME_WAS_ONLY_APPLIED;
      }
    }
    if (old & WAS_SET_BANGED)
      v[i] |= SCHEME_WAS_SET_BANGED;
    v[i] |= (old & SCHEME_USE_COUNT_MASK);
  }

  return v;
}

/*========================================================================*/
/*                          syntax-checking utils                         */
/*========================================================================*/

void scheme_check_identifier(const char *formname, Scheme_Object *id, 
			     const char *where, Scheme_Comp_Env *env,
			     Scheme_Object *form)
{
  if (!where)
    where = "";

  if (!SCHEME_STX_SYMBOLP(id))
    scheme_wrong_syntax(formname, form ? id : NULL, 
			form ? form : id, 
			"not an identifier%s", where);
}

void scheme_begin_dup_symbol_check(DupCheckRecord *r, Scheme_Comp_Env *env)
{
  r->phase = env->genv->phase;
  r->count = 0;
}

void scheme_dup_symbol_check(DupCheckRecord *r, const char *where,
			     Scheme_Object *symbol, char *what, 
			     Scheme_Object *form)
{
  int i;

  if (r->count <= 5) {
    for (i = 0; i < r->count; i++) {
      if (scheme_stx_bound_eq(symbol, r->syms[i], r->phase))
	scheme_wrong_syntax(where, symbol, form,
			    "duplicate %s name", what);
    }

    if (r->count < 5) {
      r->syms[r->count++] = symbol;
      return;
    } else {
      Scheme_Hash_Table *ht;
      ht = scheme_make_hash_table(SCHEME_hash_bound_id);
      r->ht = ht;
      for (i = 0; i < r->count; i++) {
	scheme_hash_set(ht, r->syms[i], scheme_true);
      }
      r->count++;
    }
  }

  if (scheme_hash_get(r->ht, symbol)) {
    scheme_wrong_syntax(where, symbol, form,
			"duplicate %s name", what);
  }

  scheme_hash_set(r->ht, symbol, scheme_true);
}

int scheme_check_context(Scheme_Env *env, Scheme_Object *name, Scheme_Object *ok_modidx)
{
  Scheme_Object *mod, *id = name;

  mod = scheme_stx_source_module(id, 0);

  if (mod && SCHEME_TRUEP(mod) && NOT_SAME_OBJ(ok_modidx, mod)) {
    return 1;
  } else {
    mod = scheme_stx_module_name(&id, env->phase, NULL, NULL, NULL);
    if (SAME_OBJ(mod, scheme_undefined))
      return 1;
  }
  
  return 0;
}

/*========================================================================*/
/*                 compile-time env for optimization                      */
/*========================================================================*/

Optimize_Info *scheme_optimize_info_create()
{
  Optimize_Info *info;

  info = MALLOC_ONE_RT(Optimize_Info);
#ifdef MZTAG_REQUIRED
  info->type = scheme_rt_optimize_info;
#endif
  info->inline_fuel = 16;
  
  return info;
}

static void register_transitive_use(Optimize_Info *info, int pos, int j);

static void register_stat_dist(Optimize_Info *info, int i, int j)
{
  if (!info->stat_dists) {
    int k, *ia;
    char **ca;
    ca = MALLOC_N(char*, info->new_frame);
    info->stat_dists = ca;
    ia = MALLOC_N_ATOMIC(int, info->new_frame);
    info->sd_depths = ia;
    for (k = info->new_frame; k--; ) {
      info->sd_depths[k] = 0;
    }
  }
  
  if (info->sd_depths[i] <= j) {
    char *naya, *a;
    int k;
    
    naya = MALLOC_N_ATOMIC(char, (j + 1));
    for (k = j + 1; k--; ) {
      naya[k] = 0;
    }
    a = info->stat_dists[i];
    for (k = info->sd_depths[i]; k--; ) {
      naya[k] = a[k];
    }
    
    info->stat_dists[i] = naya;
    info->sd_depths[i] = j + 1;
  }

  if (info->transitive_use && info->transitive_use[i]) {
    /* We're using a procedure that we weren't sure would be used.
       Transitively mark everything that the procedure uses --- unless
       a transitive accumulation is in effect, in which case we
       don't for this one now, leaving it to be triggered when
       the one we're accumulating is triggered. */
    if (!info->transitive_use_pos) {
      mzshort *map = info->transitive_use[i];
      int len = info->transitive_use_len[i];
      int k;

      info->transitive_use[i] = NULL;

      for (k = 0; k < len; k++) {
        register_transitive_use(info, map[k], 0);
      }
    }
  }

  info->stat_dists[i][j] = 1;
}

static Scheme_Object *transitive_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Optimize_Info *info = (Optimize_Info *)p->ku.k.p1;

  p->ku.k.p1 = NULL;

  register_transitive_use(info, p->ku.k.i1, p->ku.k.i2);

  return scheme_false;
}

static void register_transitive_use(Optimize_Info *info, int pos, int j)
{
#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Thread *p = scheme_current_thread;

    p->ku.k.p1 = (void *)info;
    p->ku.k.i1 = pos;
    p->ku.k.i2 = j;

    scheme_handle_stack_overflow(transitive_k);

    return;
  }
#endif

  while (info) {
    if (info->flags & SCHEME_LAMBDA_FRAME)
      j++;
    if (pos < info->new_frame)
      break;
    pos -= info->new_frame;
    info = info->next;
  }

  if (info->sd_depths[pos] <= j) {
    scheme_signal_error("bad transitive position depth: %d vs. %d",
                        info->sd_depths[pos], j);
  }

  register_stat_dist(info, pos, j);
}

void scheme_env_make_closure_map(Optimize_Info *info, mzshort *_size, mzshort **_map)
{
  /* A closure map lists the captured variables for a closure; the
     indices are resolved two new indicies in the second phase of
     compilation. */
  Optimize_Info *frame;
  int i, j, pos = 0, lpos = 0, tu;
  mzshort *map, size;

  /* Count vars used by this closure (skip args): */
  j = 1;
  for (frame = info->next; frame; frame = frame->next) {
    if (frame->flags & SCHEME_LAMBDA_FRAME)
      j++;

    if (frame->stat_dists) {
      for (i = 0; i < frame->new_frame; i++) {
	if (frame->sd_depths[i] > j) {
	  if (frame->stat_dists[i][j]) {
	    pos++;
	  }
	}
      }
    }
  }

  size = pos;
  *_size = size;
  map = MALLOC_N_ATOMIC(mzshort, size);
  *_map = map;

  if (info->next && info->next->transitive_use_pos) {
    info->next->transitive_use[info->next->transitive_use_pos - 1] = map;
    info->next->transitive_use_len[info->next->transitive_use_pos - 1] = size;
    tu = 1;
  } else
    tu = 0;

  /* Build map, unmarking locals and marking deeper in parent frame */
  j = 1; pos = 0;
  for (frame = info->next; frame; frame = frame->next) {
    if (frame->flags & SCHEME_LAMBDA_FRAME)
      j++;

    if (frame->stat_dists) {
      for (i = 0; i < frame->new_frame; i++) {
	if (frame->sd_depths[i] > j) {
	  if (frame->stat_dists[i][j]) {
	    map[pos++] = lpos;
	    frame->stat_dists[i][j] = 0; /* This closure's done with these vars... */
            if (!tu)
              frame->stat_dists[i][j - 1] = 1; /* ... but ensure previous keeps */
	  }
	}
	lpos++;
      }
    } else
      lpos += frame->new_frame;
  }
}

int scheme_env_uses_toplevel(Optimize_Info *frame)
{
  int used;

  used = frame->used_toplevel;
  
  if (used) {
    /* Propagate use to an enclosing lambda, if any: */
    frame = frame->next;
    while (frame) {
      if (frame->flags & SCHEME_LAMBDA_FRAME) {
	frame->used_toplevel = 1;
	break;
      }
      frame = frame->next;
    }
  }

  return used;
}

void scheme_optimize_info_used_top(Optimize_Info *info)
{
  while (info) {
    if (info->flags & SCHEME_LAMBDA_FRAME) {
      info->used_toplevel = 1;
      break;
    }
    info = info->next;
  }
}

void scheme_optimize_propagate(Optimize_Info *info, int pos, Scheme_Object *value)
{
  Scheme_Object *p;

  p = scheme_make_vector(3, NULL);
  SCHEME_VEC_ELS(p)[0] = info->consts;
  SCHEME_VEC_ELS(p)[1] = scheme_make_integer(pos);
  SCHEME_VEC_ELS(p)[2] = value;

  info->consts = p;
}

void scheme_optimize_mutated(Optimize_Info *info, int pos)
/* pos must be in immediate frame */
{
  if (!info->use) {
    char *use;
    use = (char *)scheme_malloc_atomic(info->new_frame);
    memset(use, 0, info->new_frame);
    info->use = use;
  }
  info->use[pos] = 1;
}

Scheme_Object *scheme_optimize_reverse(Optimize_Info *info, int pos, int unless_mutated)
/* pos is in new-frame counts, and we want to produce an old-frame reference if
   it's not mutated */
{
  int delta = 0;

  while (1) {
    if (pos < info->new_frame)
      break;
    pos -= info->new_frame;
    delta += info->original_frame;
    info = info->next;
  }

  if (unless_mutated)
    if (info->use && info->use[pos])
      return NULL;

  return scheme_make_local(scheme_local_type, pos + delta);
}

int scheme_optimize_is_used(Optimize_Info *info, int pos)
/* pos must be in immediate frame */
{
  int i;

  if (info->stat_dists) {
    for (i = info->sd_depths[pos]; i--; ) {
      if (info->stat_dists[pos][i])
	return 1;
    }
  }

  return 0;
}

int scheme_optimize_any_uses(Optimize_Info *info, int start_pos, int end_pos)
{
  int j, i;

  if (info->stat_dists) {
    for (i = start_pos; i < end_pos; i++) {
      for (j = info->sd_depths[i]; j--; ) {
        if (info->stat_dists[i][j])
          return 1;
      }
    }
  }

  if (info->transitive_use) {
    for (i = info->new_frame; i--; ) {
      if (info->transitive_use[i]) {
        for (j = info->transitive_use_len[i]; j--; ) {
          if ((info->transitive_use[i][j] >= start_pos)
              && (info->transitive_use[i][j] < end_pos))
            return 1;
        }
      }
    }
  }

  return 0;
}

static Scheme_Object *do_optimize_info_lookup(Optimize_Info *info, int pos, int j, int *closure_offset)
{
  Scheme_Object *p, *n;
  int delta = 0;

  while (info) {
    if (info->flags & SCHEME_LAMBDA_FRAME)
      j++;
    if (pos < info->original_frame)
      break;
    pos -= info->original_frame;
    delta += info->new_frame;
    info = info->next;
  }

  p = info->consts;
  while (p) {
    n = SCHEME_VEC_ELS(p)[1];
    if (SCHEME_INT_VAL(n) == pos) {
      n = SCHEME_VEC_ELS(p)[2];
      if (SAME_TYPE(SCHEME_TYPE(n), scheme_compiled_unclosed_procedure_type)) {
	if (!closure_offset)
	  break;
	else {
	  *closure_offset = delta;
	}
      } else if (SAME_TYPE(SCHEME_TYPE(n), scheme_compiled_toplevel_type)) {
        /* Ok */
      } else if (closure_offset) {
        /* Inlining can deal procdures and top-levels, but not other things. */
        return NULL;
      } else if (SAME_TYPE(SCHEME_TYPE(n), scheme_local_type)) {
	int pos;

	pos = SCHEME_LOCAL_POS(n);
	if (info->flags & SCHEME_LAMBDA_FRAME)
	  j--; /* because it will get re-added on recur */

	/* Marks local as used; we don't expect to get back
	   a value, because chaining would normally happen on the 
	   propagate-call side. Chaining there also means that we 
	   avoid stack overflow here. */
	n = do_optimize_info_lookup(info, pos, j, NULL);

	if (!n) {
	  /* Return shifted reference to other local: */
	  delta += scheme_optimize_info_get_shift(info, pos);
	  n = scheme_make_local(scheme_local_type, pos + delta);
	}
      }
      return n;
    }
    p = SCHEME_VEC_ELS(p)[0];
  }

  if (!closure_offset)
    register_stat_dist(info, pos, j);
  
  return NULL;
}

Scheme_Object *scheme_optimize_info_lookup(Optimize_Info *info, int pos, int *closure_offset)
{
  return do_optimize_info_lookup(info, pos, 0, closure_offset);
}

Optimize_Info *scheme_optimize_info_add_frame(Optimize_Info *info, int orig, int current, int flags)
{
  Optimize_Info *naya;

  naya = scheme_optimize_info_create();
  naya->flags = (short)flags;
  naya->next = info;
  naya->original_frame = orig;
  naya->new_frame = current;
  naya->inline_fuel = info->inline_fuel;
  naya->letrec_not_twice = info->letrec_not_twice;
  naya->enforce_const = info->enforce_const;
  naya->top_level_consts = info->top_level_consts;

  return naya;
}

int scheme_optimize_info_get_shift(Optimize_Info *info, int pos)
{
  int delta = 0;

  while (info) {
    if (pos < info->original_frame)
      break;
    pos -= info->original_frame;
    delta += (info->new_frame - info->original_frame);
    info = info->next;
  }

  if (!info)
    *(long *)0x0 = 1;

  return delta;
}

void scheme_optimize_info_done(Optimize_Info *info)
{
  info->next->size += info->size;
}


  

/*========================================================================*/
/*                    compile-time env for resolve                        */
/*========================================================================*/

/* See eval.c for information about the compilation phases. */

Resolve_Prefix *scheme_resolve_prefix(int phase, Comp_Prefix *cp, int simplify)
{
  Resolve_Prefix *rp;
  Scheme_Object **tls, **stxes, *simplify_cache;
  Scheme_Hash_Table *ht;
  int i;

  rp = MALLOC_ONE_TAGGED(Resolve_Prefix);
  rp->so.type = scheme_resolve_prefix_type;
  rp->num_toplevels = cp->num_toplevels;
  rp->num_stxes = cp->num_stxes;
  
  if (rp->num_toplevels)
    tls = MALLOC_N(Scheme_Object*, rp->num_toplevels);
  else
    tls = NULL;
  if (rp->num_stxes)
    stxes = MALLOC_N(Scheme_Object*, rp->num_stxes);
  else
    stxes = NULL;

  rp->toplevels = tls;
  rp->stxes = stxes;

  ht = cp->toplevels;
  if (ht) {
    for (i = 0; i < ht->size; i++) {
      if (ht->vals[i]) {
	tls[SCHEME_TOPLEVEL_POS(ht->vals[i])] = ht->keys[i];
      }
    }
  }

  if (simplify)
    simplify_cache = scheme_new_stx_simplify_cache();
  else
    simplify_cache = NULL;  

  ht = cp->stxes;
  if (ht) {
    for (i = 0; i < ht->size; i++) {
      if (ht->vals[i]) {
	scheme_simplify_stx(ht->keys[i], simplify_cache);
	stxes[SCHEME_LOCAL_POS(ht->vals[i])] = ht->keys[i];
      }
    }
  }

  return rp;
}

Resolve_Prefix *scheme_remap_prefix(Resolve_Prefix *rp, Resolve_Info *ri)
{
  /* Rewrite stxes list based on actual uses at resolve pass.
     If we have no lifts, we can just srop unused stxes.
     Otherwise, if any stxes go unused, we just have to replace them
     with NULL. */
  int i, cnt;
  Scheme_Object **new_stxes, *v;

  if (!rp->num_stxes)
    return rp;

  if (rp->num_lifts)
    cnt = rp->num_stxes;
  else
    cnt = ri->stx_map->count;

  new_stxes = MALLOC_N(Scheme_Object *, cnt);

  for (i = 0; i < rp->num_stxes; i++) {
    if (ri->stx_map)
      v = scheme_hash_get(ri->stx_map, scheme_make_integer(i));
    else
      v = NULL;
    if (v) {
      new_stxes[SCHEME_INT_VAL(v)]  = rp->stxes[i];
    }
  }

  rp->stxes = new_stxes;
  rp->num_stxes = cnt;

  return rp;
}

Resolve_Info *scheme_resolve_info_create(Resolve_Prefix *rp)
{
  Resolve_Info *naya;
  Scheme_Object *b;
  Scheme_Hash_Table *ht;

  naya = MALLOC_ONE_RT(Resolve_Info);
#ifdef MZTAG_REQUIRED
  naya->type = scheme_rt_resolve_info;
#endif
  naya->prefix = rp;
  naya->count = 0;
  naya->next = NULL;
  naya->toplevel_pos = -1;

  ht = scheme_make_hash_table(SCHEME_hash_ptr);
  naya->stx_map = ht;

  b = scheme_get_param(scheme_current_config(), MZCONFIG_USE_JIT);
  naya->use_jit = SCHEME_TRUEP(b);

  return naya;
}

Resolve_Info *scheme_resolve_info_extend(Resolve_Info *info, int size, int oldsize, int mapc)
     /* size = number of appended items in run-time frame */
     /* oldisze = number of appended items in original compile-time frame */
     /* mapc = mappings that will be installed */
{
  Resolve_Info *naya;

  naya = MALLOC_ONE_RT(Resolve_Info);
#ifdef MZTAG_REQUIRED
  naya->type = scheme_rt_resolve_info;
#endif
  naya->prefix = info->prefix;
  naya->stx_map = info->stx_map;
  naya->next = info;
  naya->use_jit = info->use_jit;
  naya->enforce_const = info->enforce_const;
  naya->size = size;
  naya->oldsize = oldsize;
  naya->count = mapc;
  naya->pos = 0;
  naya->toplevel_pos = -1;
  naya->lifts = info->lifts;

  if (mapc) {
    int i, *ia;
    mzshort *sa;

    sa = MALLOC_N_ATOMIC(mzshort, mapc);
    naya->old_pos = sa;
    sa = MALLOC_N_ATOMIC(mzshort, mapc);
    naya->new_pos = sa;
    ia = MALLOC_N_ATOMIC(int, mapc);
    naya->flags = ia;

    /* necessary? added when changed allocation to atomic */
    for (i = mapc; i--; ) {
      naya->old_pos[i] = 0;
      naya->new_pos[i] = 0;
      naya->flags[i] = 0;
    }
  }

  return naya;
}

void scheme_resolve_info_add_mapping(Resolve_Info *info, int oldp, int newp, int flags, Scheme_Object *lifted)
{
  if (info->pos == info->count) {
    scheme_signal_error("internal error: add_mapping: "
			"too many: %d", info->pos);
  }

  info->old_pos[info->pos] = oldp;
  info->new_pos[info->pos] = newp;
  info->flags[info->pos] = flags;
  if (lifted) {
    if (!info->lifted) {
      Scheme_Object **lifteds;
      lifteds = MALLOC_N(Scheme_Object*, info->count);
      info->lifted = lifteds;
    }
    info->lifted[info->pos] = lifted;
  }
  
  info->pos++;
}

void scheme_resolve_info_adjust_mapping(Resolve_Info *info, int oldp, int newp, int flags, Scheme_Object *lifted)
{
  int i;

  for (i = info->pos; i--; ) {
    if (info->old_pos[i] == oldp) {
      info->new_pos[i] = newp;
      info->flags[i] = flags;
      if (lifted) {
        info->lifted[i] = lifted;
      }
      return;
    }
  }
      
  scheme_signal_error("internal error: adjust_mapping: "
                      "couldn't find: %d", oldp);
}

void scheme_resolve_info_set_toplevel_pos(Resolve_Info *info, int pos)
{
  info->toplevel_pos = pos;
}

static int resolve_info_lookup(Resolve_Info *info, int pos, int *flags, Scheme_Object **_lifted, int convert_shift)
{
  Resolve_Info *orig_info = info;
  int i, offset = 0, orig = pos;

  if (_lifted)
    *_lifted = NULL;

  while (info) {
    for (i = info->pos; i--; ) {
      int oldp = info->old_pos[i];
      if (pos == oldp) {
	if (flags)
	  *flags = info->flags[i];
        if (info->lifted && (info->lifted[i])) {
          int skip, shifted;
          Scheme_Object *lifted, *tl, **ca;

          if (!_lifted)
            scheme_signal_error("unexpected lifted binding");

          lifted = info->lifted[i];

          if (SCHEME_RPAIRP(lifted)) {
            tl = SCHEME_CAR(lifted);
            ca = (Scheme_Object **)SCHEME_CDR(lifted);
            if (convert_shift)
              shifted = SCHEME_INT_VAL(ca[0]) + convert_shift - 1;
            else
              shifted = 0;
          } else {
            tl = lifted;
            shifted = 0;
            ca = NULL;
          }

          if (SAME_TYPE(SCHEME_TYPE(tl), scheme_toplevel_type)) {
            skip = scheme_resolve_toplevel_pos(orig_info);
            tl = make_toplevel(skip + shifted, 
                               SCHEME_TOPLEVEL_POS(tl),
                               1,
                               SCHEME_TOPLEVEL_CONST);
          }

          if (SCHEME_RPAIRP(lifted)) {
            int sz, i;
            mzshort *posmap, *boxmap;
            Scheme_Object *vec, *loc;
            sz = SCHEME_INT_VAL(ca[0]);
            posmap = (mzshort *)ca[1];
            boxmap = (mzshort *)ca[3];
            vec = scheme_make_vector(sz + 1, NULL);
            for (i = 0; i < sz; i++) {
              loc = scheme_make_local(scheme_local_type,
                                      posmap[i] + offset + shifted);
              if (boxmap) {
                if (boxmap[i / BITS_PER_MZSHORT] & ((mzshort)1 << (i & (BITS_PER_MZSHORT - 1))))
                  loc = scheme_box(loc);
              }
              SCHEME_VEC_ELS(vec)[i+1] = loc;
            }
            SCHEME_VEC_ELS(vec)[0] = ca[2];
            lifted = scheme_make_raw_pair(tl, vec);
          } else
            lifted = tl;
          
          *_lifted = lifted;
           
           return 0;
        } else
          return info->new_pos[i] + offset;
      }
    }

    if (info->in_proc) {
      scheme_signal_error("internal error: scheme_resolve_info_lookup: "
                          "searching past procedure");
    }

    pos -= info->oldsize;
    offset += info->size;
    info = info->next;
  }

  scheme_signal_error("internal error: scheme_resolve_info_lookup: "
		      "variable %d not found", orig);

  return 0;
}

Scheme_Object *scheme_resolve_generate_stub_lift()
{
  return make_toplevel(0, 0, 1, SCHEME_TOPLEVEL_CONST);
}

int scheme_resolve_info_flags(Resolve_Info *info, int pos, Scheme_Object **lifted)
{
  int flags;

  resolve_info_lookup(info, pos, &flags, lifted, 0);

  return flags;
}

int scheme_resolve_info_lookup(Resolve_Info *info, int pos, int *flags, Scheme_Object **lifted, int convert_shift)
{
  return resolve_info_lookup(info, pos, flags, lifted, convert_shift);
}

int scheme_resolve_toplevel_pos(Resolve_Info *info)
{
  int pos = 0;

  while (info && (info->toplevel_pos < 0)) {
    if (info->in_proc) {
      scheme_signal_error("internal error: scheme_resolve_toplevel_pos: "
                          "searching past procedure");
    }
    pos += info->size;
    info = info->next;
  }

  if (!info)
    return pos;
  else
    return info->toplevel_pos + pos;
}

int scheme_resolve_is_toplevel_available(Resolve_Info *info)
{
  while (info) {
    if (info->toplevel_pos >= 0)
      return 1;
    if (info->in_proc)
      return 0;
    info = info->next;
  }

  return 0;
}

int scheme_resolve_quote_syntax_offset(int i, Resolve_Info *info)
{
  Scheme_Hash_Table *ht;
  Scheme_Object *v;

  ht = info->stx_map;

  v = scheme_hash_get(ht, scheme_make_integer(i));
  if (!v) {
    v = scheme_make_integer(ht->count);
    scheme_hash_set(ht, scheme_make_integer(i), v);
  }

  return SCHEME_INT_VAL(v);
}

int scheme_resolve_quote_syntax_pos(Resolve_Info *info)
{
  return info->prefix->num_toplevels;
}

Scheme_Object *scheme_resolve_toplevel(Resolve_Info *info, Scheme_Object *expr)
{
  int skip;

  skip = scheme_resolve_toplevel_pos(info);

  return make_toplevel(skip + SCHEME_TOPLEVEL_DEPTH(expr), /* depth is 0 (normal) or 1 (exp-time) */
		       SCHEME_TOPLEVEL_POS(expr),
		       1,
		       SCHEME_TOPLEVEL_FLAGS(expr) & SCHEME_TOPLEVEL_CONST);
}

Scheme_Object *scheme_shift_toplevel(Scheme_Object *expr, int delta)
{
  return make_toplevel(SCHEME_TOPLEVEL_DEPTH(expr) + delta,
		       SCHEME_TOPLEVEL_POS(expr),
		       1,
		       SCHEME_TOPLEVEL_FLAGS(expr) & SCHEME_TOPLEVEL_FLAGS_MASK);
}

Scheme_Object *scheme_resolve_invent_toplevel(Resolve_Info *info)
{
  int skip, pos;
  Scheme_Object *count;

  skip = scheme_resolve_toplevel_pos(info);

  count = SCHEME_VEC_ELS(info->lifts)[1];
  pos = (SCHEME_INT_VAL(count)
         + info->prefix->num_toplevels 
         + info->prefix->num_stxes
         + (info->prefix->num_stxes ? 1 : 0));
  count = scheme_make_integer(SCHEME_INT_VAL(count) + 1);
  SCHEME_VEC_ELS(info->lifts)[1] = count;

  return make_toplevel(skip,
		       pos,
		       1,
                       SCHEME_TOPLEVEL_CONST);
}

Scheme_Object *scheme_resolve_invented_toplevel_to_defn(Resolve_Info *info, Scheme_Object *tl)
{
  return make_toplevel(0,
                       SCHEME_TOPLEVEL_POS(tl),
                       1,
                       SCHEME_TOPLEVEL_CONST);
}

int scheme_resolving_in_procedure(Resolve_Info *info)
{
  while (info) {
    if (info->in_proc)
      return 1;
    info = info->next;
  }
  return 0;
}



/*========================================================================*/
/*                             run-time "stack"                           */
/*========================================================================*/

Scheme_Object *scheme_make_envunbox(Scheme_Object *value)
{
  Scheme_Object *obj;

  obj = (Scheme_Object *)scheme_malloc_envunbox(sizeof(Scheme_Object*));
  SCHEME_ENVBOX_VAL(obj) = value;

  return obj;
}

/*========================================================================*/
/*             run-time and expansion-time Scheme interface               */
/*========================================================================*/

static Scheme_Object *
namespace_identifier(int argc, Scheme_Object *argv[])
{
  Scheme_Object *obj;
  Scheme_Env *genv;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("namespace-symbol->identifier", "symbol", 0, argc, argv);
  if ((argc > 1) && !SCHEME_NAMESPACEP(argv[1]))
    scheme_wrong_type("namespace-symbol->identifier", "namespace", 1, argc, argv);

  if (argc > 1)
    genv = (Scheme_Env *)argv[1];
  else
    genv = scheme_get_env(NULL);

  obj = argv[0];
  obj = scheme_datum_to_syntax(obj, scheme_false, scheme_false, 1, 0);

  /* Renamings: */
  if (genv->rename)
    obj = scheme_add_rename(obj, genv->rename);
  if (genv->exp_env && genv->exp_env->rename)
    obj = scheme_add_rename(obj, genv->exp_env->rename);

  return obj;
}

static Scheme_Object *
namespace_variable_value(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v, *id = NULL;
  Scheme_Env *genv;
  int use_map;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("namespace-variable-value", "symbol", 0, argc, argv);
  use_map = ((argc > 1) ? SCHEME_TRUEP(argv[1]) : 1);
  if ((argc > 2) && SCHEME_TRUEP(argv[2])
      && !scheme_check_proc_arity(NULL, 0, 2, argc, argv))
    scheme_wrong_type("namespace-variable-value", "procedure (arity 0) or #f", 1, argc, argv);
  if ((argc > 3) && !SCHEME_NAMESPACEP(argv[3]))
    scheme_wrong_type("namespace-variable-value", "namespace", 3, argc, argv);

  if (argc > 3)
    genv = (Scheme_Env *)argv[3];
  else
    genv = scheme_get_env(NULL);

  if (!use_map)
    v = scheme_lookup_global(argv[0], genv);
  else {
    Scheme_Full_Comp_Env inlined_e;

    id = scheme_make_renamed_stx(argv[0], genv->rename);

    inlined_e.base.num_bindings = 0;
    inlined_e.base.next = NULL;
    inlined_e.base.genv = genv;
    inlined_e.base.flags = SCHEME_TOPLEVEL_FRAME;
    init_compile_data((Scheme_Comp_Env *)&inlined_e);
    inlined_e.base.prefix = NULL;

    v = scheme_lookup_binding(id, (Scheme_Comp_Env *)&inlined_e, SCHEME_RESOLVE_MODIDS, NULL, NULL, NULL, NULL, NULL);
    if (v) {
      if (!SAME_TYPE(SCHEME_TYPE(v), scheme_variable_type)) {
	use_map = -1;
	v = NULL;
      } else
	v = (Scheme_Object *)(SCHEME_VAR_BUCKET(v))->val;
    }
  }
  
  if (!v) {
    if ((argc > 2) && SCHEME_TRUEP(argv[2]))
      return _scheme_tail_apply(argv[2], 0, NULL);
    else if (use_map == -1) {
      scheme_wrong_syntax("namespace-variable-value", NULL, id, "bound to syntax");
      return NULL;
    } else {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT_VARIABLE, argv[0],
		       "namespace-variable-value: %S is not defined",
		       argv[0]);
      return NULL;
    }
  }

  return v;
}

static Scheme_Object *
namespace_set_variable_value(int argc, Scheme_Object *argv[])
{
  Scheme_Env *env;
  Scheme_Bucket *bucket;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("namespace-set-variable-value!", "symbol", 0, argc, argv);
  if ((argc > 3) && !SCHEME_NAMESPACEP(argv[3]))
    scheme_wrong_type("namespace-set-variable-value!", "namespace", 3, argc, argv);

  if (argc > 3)
    env = (Scheme_Env *)argv[3];
  else
    env = scheme_get_env(NULL);

  bucket = scheme_global_bucket(argv[0], env);
  
  scheme_set_global_bucket("namespace-set-variable-value!", bucket, argv[1], 1);
  
  if ((argc > 2) && SCHEME_TRUEP(argv[2])) {
    scheme_shadow(env, argv[0], 1);
  }

  return scheme_void;
}

static Scheme_Object *
namespace_undefine_variable(int argc, Scheme_Object *argv[])
{
  Scheme_Env *env;
  Scheme_Bucket *bucket;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("namespace-undefine-variable!", "symbol", 0, argc, argv);
  if ((argc > 1) && !SCHEME_NAMESPACEP(argv[1]))
    scheme_wrong_type("namespace-undefine-variable!", "namespace", 1, argc, argv);

  if (argc > 1)
    env = (Scheme_Env *)argv[1];
  else
    env = scheme_get_env(NULL);

  if (scheme_lookup_global(argv[0], env)) {
    bucket = scheme_global_bucket(argv[0], env);
    bucket->val = NULL;
  } else {
    scheme_raise_exn(MZEXN_FAIL_CONTRACT_VARIABLE, argv[0],
		     "namespace-undefine-variable!: %S is not defined",
		     argv[0]);
  }

  return scheme_void;
}

static Scheme_Object *
namespace_mapped_symbols(int argc, Scheme_Object *argv[])
{
  Scheme_Object *l;
  Scheme_Env *env;
  Scheme_Hash_Table *mapped;
  Scheme_Bucket_Table *ht;
  Scheme_Bucket **bs;
  int i, j;

  if ((argc > 0) && !SCHEME_NAMESPACEP(argv[0]))
    scheme_wrong_type("namespace-mapped-symbols", "namespace", 0, argc, argv);

  if (argc)
    env = (Scheme_Env *)argv[0];
  else
    env = scheme_get_env(NULL);
  
  mapped = scheme_make_hash_table(SCHEME_hash_ptr);

  for (j = 0; j < 2; j++) {
    if (j)
      ht = env->syntax;
    else
      ht = env->toplevel;

    bs = ht->buckets;
    for (i = ht->size; i--; ) {
      Scheme_Bucket *b = bs[i];
      if (b && b->val) {
	scheme_hash_set(mapped, (Scheme_Object *)b->key, scheme_true);
      }
    }
  }

  if (env->rename)
    scheme_list_module_rename(env->rename, mapped);

  l = scheme_null;
  for (i = mapped->size; i--; ) {
    if (mapped->vals[i])
      l = scheme_make_pair(mapped->keys[i], l);
  }

  return l;
}

static Scheme_Object *namespace_module_registry(int argc, Scheme_Object **argv)
{
  if (!SCHEME_NAMESPACEP(argv[0]))
    scheme_wrong_type("namespace-module-registry", "namespace", 0, argc, argv);

  return (Scheme_Object *)((Scheme_Env *)argv[0])->module_registry;
}

static Scheme_Object *
now_transforming(int argc, Scheme_Object *argv[])
{
  return (scheme_current_thread->current_local_env
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *
local_exp_time_value(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v, *sym;
  Scheme_Env *menv;
  Scheme_Comp_Env *env;
  int renamed = 0;

  env = scheme_current_thread->current_local_env;
  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "syntax-local-value: not currently transforming");

  sym = argv[0];

  if (!(SCHEME_STXP(sym) && SCHEME_SYMBOLP(SCHEME_STX_VAL(sym))))
    scheme_wrong_type("syntax-local-value", "syntax identifier", 0, argc, argv);

  if (argc > 1) {
    scheme_check_proc_arity2("syntax-local-value", 0, 1, argc, argv, 1);
    if ((argc > 2)
        && SCHEME_TRUEP(argv[2])) { 
      Scheme_Comp_Env *stx_env;
      if (!SAME_TYPE(scheme_intdef_context_type, SCHEME_TYPE(argv[2])))
	scheme_wrong_type("syntax-local-value", "internal-definition context or #f", 2, argc, argv);
      stx_env = (Scheme_Comp_Env *)SCHEME_PTR1_VAL(argv[2]);
      if (!scheme_is_sub_env(stx_env, env)) {
	scheme_raise_exn(MZEXN_FAIL_CONTRACT, "syntax-local-value: transforming context does "
			 "not match given internal-definition context");
      }
      env = stx_env;
    }
  }

  if (scheme_current_thread->current_local_mark)
    sym = scheme_add_remove_mark(sym, scheme_current_thread->current_local_mark);

  menv = NULL;

  sym = scheme_stx_activate_certs(sym);

  while (1) {
    v = scheme_lookup_binding(sym, env,
			      (SCHEME_NULL_FOR_UNBOUND
			       + SCHEME_RESOLVE_MODIDS
			       + SCHEME_APP_POS + SCHEME_ENV_CONSTANTS_OK
			       + SCHEME_OUT_OF_CONTEXT_OK + SCHEME_ELIM_CONST),
			      scheme_current_thread->current_local_certs, 
			      scheme_current_thread->current_local_modidx, 
			      &menv, NULL, NULL);
    
    /* Deref globals */
    if (v && SAME_TYPE(SCHEME_TYPE(v), scheme_variable_type))
      v = (Scheme_Object *)(SCHEME_VAR_BUCKET(v))->val;
    
    if (!v || NOT_SAME_TYPE(SCHEME_TYPE(v), scheme_macro_type)) {
      if ((argc > 1) && SCHEME_TRUEP(argv[1]))
	return _scheme_tail_apply(argv[1], 0, NULL);
      else
	scheme_arg_mismatch("syntax-local-value",
			    (renamed 
			     ? "not defined as syntax (after renaming): "
			     : "not defined as syntax: "),
			    argv[0]);
    }
    
    v = SCHEME_PTR_VAL(v);
    if (SAME_TYPE(SCHEME_TYPE(v), scheme_id_macro_type)) {
      sym = SCHEME_PTR1_VAL(v);
      sym = scheme_stx_cert(sym, scheme_false, menv, sym, NULL, 1);
      renamed = 1;
      menv = NULL;
      SCHEME_USE_FUEL(1);
    } else
      return v;
  }
}

static Scheme_Object *
local_exp_time_name(int argc, Scheme_Object *argv[])
{
  Scheme_Object *sym;

  sym = scheme_current_thread->current_local_name;
  if (!sym)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "syntax-local-name: not currently transforming");

  return sym;
}

static Scheme_Object *
local_context(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env;

  env = scheme_current_thread->current_local_env;
  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "syntax-local-context: not currently transforming");

  if (env->flags & SCHEME_INTDEF_FRAME) {
    if (!env->intdef_name) {
      Scheme_Object *sym, *pr, *prev = NULL;
      Scheme_Comp_Env *lenv = env;
      char buf[22];
      while (1) {
	if (env->flags & SCHEME_FOR_INTDEF) 
	  lenv = lenv->next;
	else {
	  sprintf(buf, "internal-define%d", intdef_counter++);
	  sym = scheme_make_symbol(buf); /* uninterned! */
	  pr = scheme_make_immutable_pair(sym, scheme_null);
	  lenv->intdef_name = pr;
	  if (prev)
	    SCHEME_CDR(prev) = pr;
	  if (lenv->next->flags & SCHEME_INTDEF_FRAME) {
	    if (lenv->next->intdef_name) {
	      SCHEME_CDR(pr) = lenv->next->intdef_name;
	      break;
	    } else {
	      prev = pr;
	      lenv = lenv->next;
	      /* Go again to continue building the list */
	    }
	  } else
	    break;
	}
      }
    }
    return env->intdef_name;
  } else if (scheme_is_module_env(env))
    return scheme_intern_symbol("module");
  else if (scheme_is_module_begin_env(env))
    return scheme_intern_symbol("module-begin");
  else if (scheme_is_toplevel(env))
    return scheme_intern_symbol("top-level");
  else
    return scheme_intern_symbol("expression");
}

static Scheme_Object *
local_make_intdef_context(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env;
  Scheme_Object *c, *rib;

  env = scheme_current_thread->current_local_env;
  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, "syntax-local-make-definition-context: not currently transforming");
  
  rib = scheme_make_rename_rib();

  c = scheme_alloc_object();
  c->type = scheme_intdef_context_type;
  SCHEME_PTR1_VAL(c) = env;
  SCHEME_PTR2_VAL(c) = rib;

  return c;
}

static Scheme_Object *
local_introduce(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env;
  Scheme_Object *s;

  env = scheme_current_thread->current_local_env;
  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "syntax-local-introduce: not currently transforming");

  s = argv[0];
  if (!SCHEME_STXP(s))
    scheme_wrong_type("syntax-local-introduce", "syntax", 0, argc, argv);

  if (scheme_current_thread->current_local_mark)
    s = scheme_add_remove_mark(s, scheme_current_thread->current_local_mark);

  return s;
}

static Scheme_Object *
local_module_introduce(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env;
  Scheme_Object *s, *v;

  env = scheme_current_thread->current_local_env;
  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "syntax-local-module-introduce: not currently transforming");

  s = argv[0];
  if (!SCHEME_STXP(s))
    scheme_wrong_type("syntax-local-module-introduce", "syntax", 0, argc, argv);

  v = scheme_stx_source_module(s, 0);
  if (SCHEME_FALSEP(v)) {
    if (env->genv->module) {
      if (env->genv->module->rn_stx && !SAME_OBJ(scheme_true, env->genv->module->rn_stx)) {
	v = scheme_stx_to_rename(env->genv->module->rn_stx);
	s = scheme_add_rename(s, v);
      }
      if (env->genv->module->et_rn_stx && !SAME_OBJ(scheme_true, env->genv->module->et_rn_stx)) {
	v = scheme_stx_to_rename(env->genv->module->et_rn_stx);
	s = scheme_add_rename(s, v);
      }
      if (env->genv->module->dt_rn_stx && !SAME_OBJ(scheme_true, env->genv->module->dt_rn_stx)) {
	v = scheme_stx_to_rename(env->genv->module->dt_rn_stx);
	s = scheme_add_rename(s, v);
      }
    } else {
      if (env->genv->rename)
	s = scheme_add_rename(s, env->genv->rename);
      if (env->genv->et_rename)
	s = scheme_add_rename(s, env->genv->et_rename);
      if (env->genv->dt_rename) {
	s = scheme_add_rename(s, env->genv->dt_rename);
      }
    }
  }

  return s;
}

static Scheme_Object *
local_get_shadower(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env, *frame;
  Scheme_Object *sym, *esym, *sym_marks = NULL, *orig_sym, *uid = NULL, *env_marks;

  env = scheme_current_thread->current_local_env;
  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "syntax-local-get-shadower: not currently transforming");

  sym = argv[0];
  orig_sym = sym;

  if (!(SCHEME_STXP(sym) && SCHEME_SYMBOLP(SCHEME_STX_VAL(sym))))
    scheme_wrong_type("syntax-local-get-shadower", "syntax identifier", 0, argc, argv);

  sym_marks = scheme_stx_extract_marks(sym);

  /* Walk backward through the frames, looking for a renaming binding
     with the same marks as the given identifier, sym. When we find
     it, rename the given identifier so that it matches frame */
  for (frame = env; frame->next != NULL; frame = frame->next) {
    int i;

    for (i = frame->num_bindings; i--; ) {
      if (frame->values[i]) {
	if (SAME_OBJ(SCHEME_STX_VAL(sym), SCHEME_STX_VAL(frame->values[i])))  {
	  esym = frame->values[i];
	  env_marks = scheme_stx_extract_marks(esym);
	  if (scheme_equal(env_marks, sym_marks)) {
	    sym = esym;
	    if (frame->uids)
	      uid = frame->uids[i];
	    else
	      uid = frame->uid;
	    break;
	  }
	}
      }
    }
    if (uid)
      break;

    for (i = COMPILE_DATA(frame)->num_const; i--; ) {
      if (!(frame->flags & SCHEME_CAPTURE_WITHOUT_RENAME)) {
	if (SAME_OBJ(SCHEME_STX_VAL(sym), 
		     SCHEME_STX_VAL(COMPILE_DATA(frame)->const_names[i]))) {
	  esym = COMPILE_DATA(frame)->const_names[i];
	  env_marks = scheme_stx_extract_marks(esym);
	  if (scheme_equal(env_marks, sym_marks)) {
	    sym = esym;
	    if (COMPILE_DATA(frame)->const_uids) {
	      uid = COMPILE_DATA(frame)->const_uids[i];
	    } else
	      uid = frame->uid;
	    break;
	  }
	}
      }
    }
    if (uid)
      break;
  }

  if (!uid) {
    /* No lexical shadower, but strip module context and mark barriers, if any. */
    sym = scheme_stx_strip_module_context(sym);
    /* Add current module context, if any. */
    sym = local_module_introduce(1, &sym);
    return sym;
  }

  {
    Scheme_Object *rn, *result;

    result = scheme_datum_to_syntax(SCHEME_STX_VAL(sym), orig_sym, sym, 0, 0);
    ((Scheme_Stx *)result)->props = ((Scheme_Stx *)orig_sym)->props;
    
    rn = scheme_make_rename(uid, 1);
    scheme_set_rename(rn, 0, result);

    return scheme_add_rename(result, rn);
  }
}

static Scheme_Object *
introducer_proc(void *mark, int argc, Scheme_Object *argv[])
{
  Scheme_Object *s;

  s = argv[0];
  if (!SCHEME_STXP(s))
    scheme_wrong_type("syntax-introducer", "syntax", 0, argc, argv);

  return scheme_add_remove_mark(s, (Scheme_Object *)mark);
}

static Scheme_Object *
make_introducer(int argc, Scheme_Object *argv[])
{
  Scheme_Object *mark;

  mark = scheme_new_mark();

  return scheme_make_closed_prim_w_arity(introducer_proc, mark,
					 "syntax-introducer", 1, 1);
}

static Scheme_Object *
certifier(void *_data, int argc, Scheme_Object **argv)
{
  Scheme_Object *s, **cert_data = (Scheme_Object **)_data;
  Scheme_Object *mark = scheme_false;

  s = argv[0];
  if (!SCHEME_STXP(s))
    scheme_wrong_type("certifier", "syntax", 0, argc, argv);

  if (argc > 2) {
    if (SCHEME_TRUEP(argv[2])) {
      if (SCHEME_CLSD_PRIMP(argv[2])
	  && (((Scheme_Closed_Primitive_Proc *)argv[2])->prim_val == introducer_proc))
	mark = (Scheme_Object *)((Scheme_Closed_Primitive_Proc *)argv[2])->data;
      else {
	scheme_wrong_type("certifier", 
			  "procedure from make-syntax-introducer or #f", 
			  2, argc, argv);
	return NULL;
      }
    }
  }

  if (cert_data[0] || cert_data[1] || cert_data[2]) {
    int as_active = SCHEME_TRUEP(cert_data[3]);
    s = scheme_stx_cert(s, mark, 
			(Scheme_Env *)(cert_data[1] ? cert_data[1] : cert_data[2]),
			cert_data[0],
			((argc > 1) && SCHEME_TRUEP(argv[1])) ? argv[1] : NULL,
			as_active);
    if (cert_data[1] && cert_data[2] && !SAME_OBJ(cert_data[1], cert_data[2])) {
      /* Have module we're expanding, in addition to module that bound
	 the expander. */
      s = scheme_stx_cert(s, mark, (Scheme_Env *)cert_data[2],
			  NULL,
			  ((argc > 1) && SCHEME_TRUEP(argv[1])) ? argv[1] : NULL,
			  as_active);
    }
  }

  return s;
}

static Scheme_Object *
local_certify(int argc, Scheme_Object *argv[])
{
  Scheme_Object **cert_data;
  Scheme_Env *menv;
  int active = 0;

  if (!scheme_current_thread->current_local_env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "syntax-local-certifier: not currently transforming");
  menv = scheme_current_thread->current_local_menv;

  if (argc)
    active = SCHEME_TRUEP(argv[0]);

  cert_data = MALLOC_N(Scheme_Object*, 4);
  cert_data[0] = scheme_current_thread->current_local_certs;
  /* Module that bound the macro we're now running: */
  cert_data[1] = (Scheme_Object *)((menv && menv->module) ? menv : NULL);
  /* Module that we're currently expanding: */
  menv = scheme_current_thread->current_local_env->genv;
  cert_data[2] = (Scheme_Object *)((menv && menv->module) ? menv : NULL);
  cert_data[3] = (active ? scheme_true : scheme_false);

  return scheme_make_closed_prim_w_arity(certifier,
					 cert_data,
					 "certifier",
					 1, 3);
}

static Scheme_Object *
local_lift_expr(int argc, Scheme_Object *argv[])
{
  Scheme_Env *menv;
  Scheme_Comp_Env *env, *orig_env;
  Scheme_Object *id, *local_mark, *expr, *data, *vec, *id_sym;
  Scheme_Lift_Capture_Proc cp;  
  Scheme_Object *orig_expr;

  expr = argv[0];
  if (!SCHEME_STXP(expr))
    scheme_wrong_type("syntax-local-lift-expression", "syntax", 0, argc, argv);

  env = orig_env = scheme_current_thread->current_local_env;
  local_mark = scheme_current_thread->current_local_mark;

  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "syntax-local-lift-expression: not currently transforming");

  while (env && !COMPILE_DATA(env)->lifts) {
    env = env->next;
  }

  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "syntax-local-lift-expression: no lift target");
  
  expr = scheme_add_remove_mark(expr, local_mark);

  id_sym = scheme_intern_exact_parallel_symbol("lifted", 6);
  id = scheme_datum_to_syntax(id_sym, scheme_false, scheme_false, 0, 0);
  id = scheme_add_remove_mark(id, scheme_new_mark());

  vec = COMPILE_DATA(env)->lifts;
  cp = *(Scheme_Lift_Capture_Proc *)SCHEME_VEC_ELS(vec)[1];
  data = SCHEME_VEC_ELS(vec)[2];

  menv = scheme_current_thread->current_local_menv;

  expr = scheme_stx_cert(expr, scheme_false, 
			 (menv && menv->module) ? menv : NULL,
			 scheme_current_thread->current_local_certs, 
			 NULL, 1);

  expr = scheme_stx_activate_certs(expr);
  orig_expr = expr;

  expr = cp(data, &id, expr, orig_env);

  expr = scheme_make_pair(expr, SCHEME_VEC_ELS(vec)[0]);
  SCHEME_VEC_ELS(vec)[0] = expr;

  SCHEME_EXPAND_OBSERVE_LOCAL_LIFT(scheme_get_expand_observe(), id, orig_expr);

  id = scheme_add_remove_mark(id, local_mark);
  return id;
}

static Scheme_Object *
local_lift_context(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env;

  env = scheme_current_thread->current_local_env;

  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "syntax-local-lift-context: not currently transforming");

  while (env && !COMPILE_DATA(env)->lifts) {
    env = env->next;
  }

  if (!env)
    return scheme_false;
  
  return SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[4];
}

static Scheme_Object *
local_lift_end_statement(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env;
  Scheme_Object *local_mark, *expr, *pr;
  Scheme_Object *orig_expr;

  expr = argv[0];
  if (!SCHEME_STXP(expr))
    scheme_wrong_type("syntax-local-lift-module-end-declaration", "syntax", 0, argc, argv);

  env = scheme_current_thread->current_local_env;
  local_mark = scheme_current_thread->current_local_mark;

  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "syntax-local-lift-module-end-declaration: not currently transforming");

  while (env) {
    if ((COMPILE_DATA(env)->lifts)
        && SCHEME_TRUEP(SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[3]))
      break;
    env = env->next;
  }

  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "syntax-local-lift-module-end-declaration: not currently transforming"
                     " a run-time expression in a module declaration");
  
  expr = scheme_add_remove_mark(expr, local_mark);
  orig_expr = expr;

  pr = scheme_make_pair(expr, SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[3]);
  SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[3] = pr;

  SCHEME_EXPAND_OBSERVE_LIFT_STATEMENT(scheme_get_expand_observe(), orig_expr);
  
  return scheme_void;
}

static Scheme_Object *
make_set_transformer(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;

  scheme_check_proc_arity("make-set!-transformer", 1, 0, argc, argv);

  v = scheme_alloc_small_object();
  v->type = scheme_set_macro_type;
  SCHEME_PTR_VAL(v) = argv[0];

  return v;
}

static Scheme_Object *
set_transformer_p(int argc, Scheme_Object *argv[])
{
  return ((SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_set_macro_type))
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *
set_transformer_proc(int argc, Scheme_Object *argv[])
{
  if (!(SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_set_macro_type)))
    scheme_wrong_type("set!-transformer-procedure", "set!-transformer", 1, argc, argv);

  return SCHEME_PTR_VAL(argv[0]);
}

static Scheme_Object *
make_rename_transformer(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;

  if (!SCHEME_STXP(argv[0]) || !SCHEME_SYMBOLP(SCHEME_STX_VAL(argv[0])))
    scheme_wrong_type("make-rename-transformer", "syntax identifier", 0, argc, argv);

  v = scheme_alloc_small_object();
  v->type = scheme_id_macro_type;
  SCHEME_PTR_VAL(v) = argv[0];

  return v;
}

static Scheme_Object *
rename_transformer_target(int argc, Scheme_Object *argv[])
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_id_macro_type))
    scheme_wrong_type("rename-transformer-target", "rename transformer", 0, argc, argv);

  return SCHEME_PTR_VAL(argv[0]);
}

static Scheme_Object *
rename_transformer_p(int argc, Scheme_Object *argv[])
{
  return ((SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_id_macro_type))
	  ? scheme_true
	  : scheme_false);
}


/*========================================================================*/
/*                    [un]marshalling variable reference                  */
/*========================================================================*/

static Scheme_Object *write_toplevel(Scheme_Object *obj)
{
  int pos, flags;
  Scheme_Object *pr;

  pos = SCHEME_TOPLEVEL_POS(obj);
  flags = (SCHEME_TOPLEVEL_FLAGS(obj) & SCHEME_TOPLEVEL_FLAGS_MASK);

  pr = (flags
	? scheme_make_pair(scheme_make_integer(pos),
			   scheme_make_integer(flags))
	: scheme_make_integer(pos));

  return scheme_make_pair(scheme_make_integer(SCHEME_TOPLEVEL_DEPTH(obj)),
			  pr);
}

static Scheme_Object *read_toplevel(Scheme_Object *obj)
{
  int pos, depth, flags;

  if (!SCHEME_PAIRP(obj)) return NULL;

  depth = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  if (SCHEME_PAIRP(obj)) {
    pos = SCHEME_INT_VAL(SCHEME_CAR(obj));
    flags = SCHEME_INT_VAL(SCHEME_CDR(obj)) & SCHEME_TOPLEVEL_FLAGS_MASK;
  } else {
    pos = SCHEME_INT_VAL(obj);
    flags = 0;
  }

  return make_toplevel(depth, pos, 1, flags);
}

static Scheme_Object *write_variable(Scheme_Object *obj)
  /* WARNING: phase-0 module variables and #%kernel references
     are handled in print.c, instead */
{
  if (SAME_TYPE(scheme_variable_type, SCHEME_TYPE(obj))) {
    Scheme_Object *sym;
    Scheme_Env *home;
    Scheme_Module *m;
    
    sym = (Scheme_Object *)(SCHEME_VAR_BUCKET(obj))->key;
    
    home = ((Scheme_Bucket_With_Home *)obj)->home;
    m = home->module;
    
    /* If we get a writeable variable (instead of a module variable),
       it must be a reference to a module referenced directly by its
       a symbolic name (i.e., no path). */
    
    if (m) {
      sym = scheme_make_pair(m->modname, sym);
      if (home->mod_phase)
	sym = scheme_make_pair(scheme_make_integer(home->mod_phase), sym);
    }

    return sym;
  } else {
    Module_Variable *mv = (Module_Variable *)obj;

    return scheme_make_pair(scheme_make_integer(mv->mod_phase),
			    scheme_make_pair(mv->modidx,
					     mv->sym));
  }
}

static Scheme_Object *read_variable(Scheme_Object *obj)
  /* WARNING: phase-0 module variables and #%kernel references
     are handled in read.c, instead */
{
  Scheme_Env *env;

  env = scheme_get_env(NULL);

  if (!SCHEME_SYMBOLP(obj)) {
    /* Find variable from module. */
    Scheme_Object *modname, *varname;
    int mod_phase = 0;

    if (!SCHEME_PAIRP(obj)) return NULL;

    modname = SCHEME_CAR(obj);
    
    if (SCHEME_INTP(modname)) {
      mod_phase = SCHEME_INT_VAL(modname);
      if (mod_phase != 1) return NULL;

      obj = SCHEME_CDR(obj);

      if (!SCHEME_PAIRP(obj)) return NULL;
      modname = SCHEME_CAR(obj);
    }

    varname = SCHEME_CDR(obj);

    if (SAME_OBJ(modname, kernel_symbol) && !mod_phase) {
      return (Scheme_Object *)scheme_global_bucket(varname, scheme_initial_env);
    } else {
      Module_Variable *mv;
      Scheme_Object *insp;

      insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);
      
      mv = MALLOC_ONE_TAGGED(Module_Variable);
      mv->so.type = scheme_module_variable_type;
      
      mv->modidx = modname;
      mv->sym = varname;
      mv->insp = insp;
      mv->pos = -1;
      mv->mod_phase = mod_phase;

      return (Scheme_Object *)mv;
    }
  }

  return (Scheme_Object *)scheme_global_bucket(obj, env);
}

static Scheme_Object *write_local(Scheme_Object *obj)
{
  return scheme_make_integer(SCHEME_LOCAL_POS(obj));
}

static Scheme_Object *read_local(Scheme_Object *obj)
{
  return scheme_make_local(scheme_local_type,
			   SCHEME_INT_VAL(obj));
}

static Scheme_Object *read_local_unbox(Scheme_Object *obj)
{
  return scheme_make_local(scheme_local_unbox_type,
			   SCHEME_INT_VAL(obj));
}

static Scheme_Object *write_resolve_prefix(Scheme_Object *obj)
{
  Resolve_Prefix *rp = (Resolve_Prefix *)obj;
  Scheme_Object *tv, *sv, *ds;
  int i;

  i = rp->num_toplevels;
  tv = scheme_make_vector(i, NULL);
  while (i--) {
    SCHEME_VEC_ELS(tv)[i] = rp->toplevels[i];
  }

  i = rp->num_stxes;
  sv = scheme_make_vector(i, NULL);
  while (i--) {
    if (rp->stxes[i]) {
      ds = scheme_alloc_small_object();
      ds->type = scheme_delay_syntax_type;
      SCHEME_PTR_VAL(ds) = rp->stxes[i];
    } else
      ds = scheme_false;
    SCHEME_VEC_ELS(sv)[i] = ds;
  }

  return scheme_make_pair(scheme_make_integer(rp->num_lifts), scheme_make_pair(tv, sv));
}

static Scheme_Object *read_resolve_prefix(Scheme_Object *obj)
{
  Resolve_Prefix *rp;
  Scheme_Object *tv, *sv, **a, *stx;
  int i;

  if (!SCHEME_PAIRP(obj)) return NULL;

  i = SCHEME_INT_VAL(SCHEME_CAR(obj));
  if (i < 0) return NULL;

  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return NULL;

  tv = SCHEME_CAR(obj);
  sv = SCHEME_CDR(obj);

  rp = MALLOC_ONE_TAGGED(Resolve_Prefix);
  rp->so.type = scheme_resolve_prefix_type;
  rp->num_toplevels = SCHEME_VEC_SIZE(tv);
  rp->num_stxes = SCHEME_VEC_SIZE(sv);
  rp->num_lifts = i;

  i = rp->num_toplevels;
  a = MALLOC_N(Scheme_Object *, i);
  while (i--) {
    a[i] = SCHEME_VEC_ELS(tv)[i];
  }
  rp->toplevels = a;
  
  i = rp->num_stxes;
  a = MALLOC_N(Scheme_Object *, i);
  while (i--) {
    stx = SCHEME_VEC_ELS(sv)[i];
    if (SCHEME_FALSEP(stx)) {
      stx = NULL;
    } else if (SCHEME_RPAIRP(stx)) {
      rp->delay_info = (struct Scheme_Load_Delay *)SCHEME_CDR(stx);
      rp->delay_refcount++;
      stx = SCHEME_CAR(stx);
    } else {
      if (!SCHEME_STXP(stx)) return NULL;
    }
    a[i] = stx;
  }
  rp->stxes = a;

  return (Scheme_Object *)rp;
}

/*========================================================================*/
/*                         precise GC traversers                          */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_ENV_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_comp_env, mark_comp_env);
  GC_REG_TRAV(scheme_rt_resolve_info, mark_resolve_info);
  GC_REG_TRAV(scheme_rt_optimize_info, mark_optimize_info);
}

END_XFORM_SKIP;

#endif
