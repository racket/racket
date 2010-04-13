/*
  MzScheme
  Copyright (c) 2004-2010 PLT Scheme Inc.
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
#include "mzrt.h"
#include "schminc.h"
#include "schmach.h"
#include "schexpobs.h"
#ifdef MZ_USE_FUTURES
# include "future.h"
#endif

#define GLOBAL_TABLE_SIZE 500
#define TABLE_CACHE_MAX_SIZE 2048

/* #define TIME_STARTUP_PROCESS */

/* global flags */
SHARED_OK int scheme_allow_set_undefined;
void scheme_set_allow_set_undefined(int v) { scheme_allow_set_undefined =  v; }
int scheme_get_allow_set_undefined() { return scheme_allow_set_undefined; }
SHARED_OK int scheme_starting_up;

/* globals READ-ONLY SHARED */
READ_ONLY static Scheme_Object *kernel_symbol;
READ_ONLY static Scheme_Object *unshadowable_symbol;
READ_ONLY static Scheme_Env    *kernel_env;
READ_ONLY static Scheme_Env    *unsafe_env;
READ_ONLY static Scheme_Env    *flfxnum_env;

#define MAX_CONST_LOCAL_POS 64
#define MAX_CONST_LOCAL_TYPES 2
#define MAX_CONST_LOCAL_FLAG_VAL 3
#define SCHEME_LOCAL_FLAGS_MASK 0x3
READ_ONLY static Scheme_Object *scheme_local[MAX_CONST_LOCAL_POS][MAX_CONST_LOCAL_TYPES][MAX_CONST_LOCAL_FLAG_VAL + 1];
#define MAX_CONST_TOPLEVEL_DEPTH 16
#define MAX_CONST_TOPLEVEL_POS 16
#define SCHEME_TOPLEVEL_FLAGS_MASK 0x3
READ_ONLY static Scheme_Object *toplevels[MAX_CONST_TOPLEVEL_DEPTH][MAX_CONST_TOPLEVEL_POS][SCHEME_TOPLEVEL_FLAGS_MASK + 1];

/* If locked, these are probably sharable: */
THREAD_LOCAL_DECL(static Scheme_Hash_Table *toplevels_ht);
THREAD_LOCAL_DECL(static Scheme_Hash_Table *locals_ht[2]);
THREAD_LOCAL_DECL(static int intdef_counter);
THREAD_LOCAL_DECL(static int builtin_ref_counter);
THREAD_LOCAL_DECL(static int env_uid_counter);

/* local functions */
static void make_kernel_env(void);
static void init_scheme_local();
static void init_toplevels();

static Scheme_Env *make_env(Scheme_Env *base, int toplevel_size);
static Scheme_Env *make_empty_inited_env(int toplevel_size);
static Scheme_Env *make_empty_not_inited_env(int toplevel_size);

static Scheme_Object *namespace_identifier(int, Scheme_Object *[]);
static Scheme_Object *namespace_module_identifier(int, Scheme_Object *[]);
static Scheme_Object *namespace_base_phase(int, Scheme_Object *[]);
static Scheme_Object *namespace_variable_value(int, Scheme_Object *[]);
static Scheme_Object *namespace_set_variable_value(int, Scheme_Object *[]);
static Scheme_Object *namespace_undefine_variable(int, Scheme_Object *[]);
static Scheme_Object *namespace_mapped_symbols(int, Scheme_Object *[]);
static Scheme_Object *namespace_module_registry(int, Scheme_Object *[]);
static Scheme_Object *variable_p(int, Scheme_Object *[]);
static Scheme_Object *variable_module_path(int, Scheme_Object *[]);
static Scheme_Object *variable_module_source(int, Scheme_Object *[]);
static Scheme_Object *variable_namespace(int, Scheme_Object *[]);
static Scheme_Object *variable_top_level_namespace(int, Scheme_Object *[]);
static Scheme_Object *variable_phase(int, Scheme_Object *[]);
static Scheme_Object *now_transforming(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_exp_time_value(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_exp_time_value_one(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_exp_time_name(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_context(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_phase_level(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_make_intdef_context(int argc, Scheme_Object *argv[]);
static Scheme_Object *intdef_context_seal(int argc, Scheme_Object *argv[]);
static Scheme_Object *intdef_context_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *id_intdef_remove(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_introduce(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_module_introduce(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_get_shadower(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_certify(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_module_exports(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_module_definitions(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_module_imports(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_module_expanding_provides(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_lift_expr(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_lift_exprs(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_lift_context(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_lift_end_statement(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_lift_require(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_lift_provide(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_introducer(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_make_delta_introduce(int argc, Scheme_Object *argv[]);
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
static Scheme_Object *write_module_variable(Scheme_Object *obj);
static Scheme_Object *read_module_variable(Scheme_Object *obj);
static Scheme_Object *write_local(Scheme_Object *obj);
static Scheme_Object *read_local(Scheme_Object *obj);
static Scheme_Object *read_local_unbox(Scheme_Object *obj);
static Scheme_Object *write_resolve_prefix(Scheme_Object *obj);
static Scheme_Object *read_resolve_prefix(Scheme_Object *obj, Scheme_Object *insp);

static void skip_certain_things(Scheme_Object *o, Scheme_Close_Custodian_Client *f, void *data);
int scheme_is_module_begin_env(Scheme_Comp_Env *env);

Scheme_Env *scheme_engine_instance_init();
Scheme_Env *scheme_place_instance_init();
static Scheme_Env *place_instance_init(void *stack_base, int initial_main_os_thread);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

typedef Scheme_Object *(*Lazy_Macro_Fun)(Scheme_Object *, int);

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
  int *sealed; /* NULL => already sealed */
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
                                 | SCHEME_FOR_STOPS | SCHEME_CAPTURE_LIFTED)

#define ASSERT_IS_VARIABLE_BUCKET(b) /* if (((Scheme_Object *)b)->type != scheme_variable_type) abort() */


/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/


#ifdef DONT_USE_FOREIGN
static void init_dummy_foreign(Scheme_Env *env)
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

static void boot_module_resolver()
{
  Scheme_Object *boot, *a[2];
  a[0] = scheme_make_pair(scheme_intern_symbol("quote"),
                          scheme_make_pair(scheme_intern_symbol("#%boot"),
                                           scheme_null));
  a[1] = scheme_intern_symbol("boot");
  boot = scheme_dynamic_require(2, a);
  scheme_apply(boot, 0, NULL);
}

void scheme_seal_parameters()
{
  Scheme_Object *seal, *a[2];
  a[0] = scheme_make_pair(scheme_intern_symbol("quote"),
                          scheme_make_pair(scheme_intern_symbol("#%boot"),
                                           scheme_null));
  a[1] = scheme_intern_symbol("seal");
  seal = scheme_dynamic_require(2, a);
  scheme_apply(seal, 0, NULL);
}

void os_platform_init() {
#ifdef UNIX_LIMIT_STACK
  struct rlimit rl;

  getrlimit(RLIMIT_STACK, &rl);
  if (rl.rlim_cur > UNIX_LIMIT_STACK) {
    rl.rlim_cur = UNIX_LIMIT_STACK;
    setrlimit(RLIMIT_STACK, &rl);
  }
#endif
#ifdef UNIX_LIMIT_FDSET_SIZE
  struct rlimit rl;

  getrlimit(RLIMIT_NOFILE, &rl);
  if (rl.rlim_cur > FD_SETSIZE) {
    rl.rlim_cur = FD_SETSIZE;
    setrlimit(RLIMIT_NOFILE, &rl);
  }
#endif
}

Scheme_Env *scheme_restart_instance() {
  Scheme_Env *env;
  void *stack_base;
  stack_base = (void *) scheme_get_current_os_thread_stack_base();

  /* Reset everything: */
  scheme_do_close_managed(NULL, skip_certain_things);
  scheme_main_thread = NULL;

  scheme_reset_finalizations();
  scheme_init_stack_check();
#ifndef MZ_PRECISE_GC
  scheme_init_setjumpup();
#endif
  scheme_reset_overflow();

  scheme_make_thread(stack_base);
  scheme_init_error_escape_proc(NULL);
  scheme_init_module_resolver();

  env = scheme_make_empty_env();
  scheme_install_initial_module_set(env);
  scheme_set_param(scheme_current_config(), MZCONFIG_ENV, (Scheme_Object *)env); 

  scheme_init_port_config();
  scheme_init_port_fun_config();
  scheme_init_error_config();
  scheme_init_logger_config();
#ifndef NO_SCHEME_EXNS
  scheme_init_exn_config();
#endif

  boot_module_resolver();

  return env;
}

Scheme_Env *scheme_basic_env()
{
  Scheme_Env *env;

  if (scheme_main_thread) {
    return scheme_restart_instance();
  }
  
  env = scheme_engine_instance_init();
  
  return env;
}

static void init_toplevel_local_offsets_hashtable_caches()
{
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
}

/* READ-ONLY GLOBAL structures ONE-TIME initialization */
Scheme_Env *scheme_engine_instance_init() {
  Scheme_Env *env;
  void *stack_base;
  stack_base = (void *) scheme_get_current_os_thread_stack_base();

  os_platform_init();

#ifdef TIME_STARTUP_PROCESS
  printf("#if 0\nengine_instance_init @ %ld\n", scheme_get_process_milliseconds());
#endif

  scheme_starting_up = 1;
 
  scheme_init_portable_case();
  init_scheme_local();
  init_toplevels();

  scheme_init_true_false();

#ifdef MZ_PRECISE_GC
  scheme_register_traversers();
  register_traversers();
  scheme_init_hash_key_procs();
#endif

  scheme_init_getenv(); /* checks PLTNOJIT */

#ifdef WINDOWS_PROCESSES
  /* Must be called before first scheme_make_thread() */
  scheme_init_thread_memory();
#endif

#ifndef MZ_PRECISE_GC
  scheme_init_ephemerons();
#endif

/* These calls must be made here so that they allocate out of the master GC */
  scheme_init_symbol_table();
  scheme_init_module_path_table();
  scheme_init_type();
  scheme_init_custodian_extractors();
#ifndef DONT_USE_FOREIGN
  scheme_init_foreign_globals();
#endif
  scheme_init_salloc();
#ifdef MZ_USE_JIT
  scheme_init_jit();
#endif
  make_kernel_env();

#if defined(MZ_PRECISE_GC) && defined(MZ_USE_PLACES)
  scheme_places_block_child_signal();

  GC_switch_out_master_gc();
  scheme_spawn_master_place();
#endif
  
  env = place_instance_init(stack_base, 1);

#if defined(MZ_PRECISE_GC) && defined(MZ_USE_PLACES)
{
  void *signal_fd;
  signal_fd = scheme_get_signal_handle();
  GC_set_put_external_event_fd(signal_fd);
}
#endif

  return env;
}

static void init_unsafe(Scheme_Env *env)
{
  Scheme_Module_Phase_Exports *pt;
  REGISTER_SO(unsafe_env);

  unsafe_env = scheme_primitive_module(scheme_intern_symbol("#%unsafe"), env);

  scheme_init_unsafe_number(unsafe_env);
  scheme_init_unsafe_numarith(unsafe_env);
  scheme_init_unsafe_numcomp(unsafe_env);
  scheme_init_unsafe_list(unsafe_env);
  scheme_init_unsafe_vector(unsafe_env);

  scheme_finish_primitive_module(unsafe_env);
  pt = unsafe_env->module->me->rt;
  scheme_populate_pt_ht(pt);
  scheme_protect_primitive_provide(unsafe_env, NULL);

#if USE_COMPILED_STARTUP
  if (builtin_ref_counter != (EXPECTED_PRIM_COUNT + EXPECTED_UNSAFE_COUNT)) {
    printf("Unsafe count %d doesn't match expected count %d\n",
	   builtin_ref_counter - EXPECTED_PRIM_COUNT, EXPECTED_UNSAFE_COUNT);
    abort();
  }
#endif
}

static void init_flfxnum(Scheme_Env *env)
{
  Scheme_Module_Phase_Exports *pt;
  REGISTER_SO(flfxnum_env);

  flfxnum_env = scheme_primitive_module(scheme_intern_symbol("#%flfxnum"), env);

  scheme_init_flfxnum_number(flfxnum_env);
  scheme_init_flfxnum_numarith(flfxnum_env);
  scheme_init_flfxnum_numcomp(flfxnum_env);

  scheme_finish_primitive_module(flfxnum_env);
  pt = flfxnum_env->module->me->rt;
  scheme_populate_pt_ht(pt);
  scheme_protect_primitive_provide(flfxnum_env, NULL);

#if USE_COMPILED_STARTUP
  if (builtin_ref_counter != (EXPECTED_PRIM_COUNT + EXPECTED_UNSAFE_COUNT + EXPECTED_FLFXNUM_COUNT)) {
    printf("Flfxnum count %d doesn't match expected count %d\n",
	   builtin_ref_counter - EXPECTED_PRIM_COUNT - EXPECTED_UNSAFE_COUNT, 
           EXPECTED_FLFXNUM_COUNT);
    abort();
  }
#endif
}

Scheme_Env *scheme_get_unsafe_env() {
  return unsafe_env;
}

Scheme_Env *scheme_get_flfxnum_env() {
  return flfxnum_env;
}


static Scheme_Env *place_instance_init(void *stack_base, int initial_main_os_thread) {
  Scheme_Env *env;

#ifdef TIME_STARTUP_PROCESS
  printf("place_init @ %ld\n", scheme_get_process_milliseconds());
#endif
  scheme_set_current_os_thread_stack_base(stack_base);

#ifndef MZ_PRECISE_GC
  scheme_init_setjumpup();
#endif

  scheme_init_stack_check();
  scheme_init_overflow();

  init_toplevel_local_offsets_hashtable_caches();


#ifdef TIME_STARTUP_PROCESS
  printf("pre-process @ %ld\n", scheme_get_process_milliseconds());
#endif

  scheme_make_thread(stack_base);

  {
    Scheme_Object *sym;
    sym = scheme_intern_symbol("mzscheme");
    scheme_current_thread->name = sym;
  }

  scheme_init_module_resolver();

#ifdef TIME_STARTUP_PROCESS
  printf("process @ %ld\n", scheme_get_process_milliseconds());
#endif

  /* error handling and buffers */
  /* this check prevents initializing orig ports twice for the first initial
   * place.  The kernel initializes orig_ports early. */
  scheme_init_fun_places();
  scheme_init_port_places();
  scheme_init_error_escape_proc(NULL);
  scheme_init_print_buffers_places();
  scheme_init_thread_places();
  scheme_init_string_places();
  scheme_init_logger();
  scheme_init_eval_places();
  scheme_init_regexp_places();
  scheme_init_stx_places(initial_main_os_thread);
  scheme_init_sema_places();
  scheme_init_gmp_places();
  scheme_alloc_global_fdset();
  scheme_init_file_places();

  env = scheme_make_empty_env();
  scheme_set_param(scheme_current_config(), MZCONFIG_ENV, (Scheme_Object *)env); 
 
  /*initialize config */
  scheme_init_port_config();
  scheme_init_port_fun_config();
  scheme_init_error_config();
  scheme_init_logger_config();
#ifndef NO_SCHEME_EXNS
  scheme_init_exn_config();
#endif
  scheme_init_error_config();

/* BEGIN PRIMITIVE MODULES */
  scheme_init_memtrace(env);
#ifndef NO_TCP_SUPPORT
  scheme_init_network(env);
#endif
  scheme_init_paramz(env);
  scheme_init_expand_observe(env);
  scheme_init_place(env);
/* END PRIMITIVE MODULES */
#if defined(MZ_USE_PLACES)
  scheme_jit_fill_threadlocal_table();
#endif
  scheme_init_futures(env);

#ifndef DONT_USE_FOREIGN
  scheme_init_foreign(env);
#else
  init_dummy_foreign(env);
#endif

  scheme_add_embedded_builtins(env);

  boot_module_resolver();

  scheme_save_initial_module_set(env);


  scheme_starting_up = 0;

  --scheme_current_thread->suspend_break; /* created with breaks suspended */

#ifdef TIME_STARTUP_PROCESS
  printf("done @ %ld\n#endif\n", scheme_get_process_milliseconds());
#endif

  return env;
}

Scheme_Env *scheme_place_instance_init(void *stack_base) {
  Scheme_Env *env;
#if defined(MZ_PRECISE_GC) && defined(MZ_USE_PLACES)
  int *signal_fd;
  GC_construct_child_gc();
#endif
  env = place_instance_init(stack_base, 0);
#if defined(MZ_PRECISE_GC) && defined(MZ_USE_PLACES)
  signal_fd = scheme_get_signal_handle();
  GC_set_put_external_event_fd(signal_fd);
#endif
  scheme_set_can_break(1);
  return env; 
}

void scheme_place_instance_destroy() {
#if defined(MZ_USE_PLACES)
  scheme_kill_green_thread_timer();
#endif
}

static void make_kernel_env(void)
{
  Scheme_Env *env;
#ifdef TIME_STARTUP_PROCESS
  long startt;
#endif

  env = make_empty_inited_env(GLOBAL_TABLE_SIZE);


  REGISTER_SO(kernel_env);
  kernel_env = env;

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
  MZTIMEIT(symbol-type, scheme_init_symbol_type(env));
  MZTIMEIT(fun, scheme_init_fun(env));
  MZTIMEIT(symbol, scheme_init_symbol(env));
  MZTIMEIT(list, scheme_init_list(env));
  MZTIMEIT(number, scheme_init_number(env));
  MZTIMEIT(numarith, scheme_init_numarith(env));
  MZTIMEIT(numcomp, scheme_init_numcomp(env));
  MZTIMEIT(numstr, scheme_init_numstr(env));
  MZTIMEIT(bignum, scheme_init_bignum());
  MZTIMEIT(stx, scheme_init_stx(env));
  MZTIMEIT(module, scheme_init_module(env));
  MZTIMEIT(port, scheme_init_port(env));
  MZTIMEIT(portfun, scheme_init_port_fun(env));
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
  scheme_init_inspector();
  MZTIMEIT(reduced, scheme_init_reduced_proc_struct(env));
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
  MZTIMEIT(params, scheme_init_parameterization());
  MZTIMEIT(places, scheme_init_places_once());

  MARK_START_TIME();

  GLOBAL_PRIM_W_ARITY("namespace-symbol->identifier", namespace_identifier, 1, 2, env);
  GLOBAL_PRIM_W_ARITY("namespace-module-identifier", namespace_module_identifier, 0, 1, env);
  GLOBAL_PRIM_W_ARITY("namespace-base-phase", namespace_base_phase, 0, 1, env);
  GLOBAL_PRIM_W_ARITY("namespace-variable-value", namespace_variable_value, 1, 4, env);
  GLOBAL_PRIM_W_ARITY("namespace-set-variable-value!", namespace_set_variable_value, 2, 4, env);
  GLOBAL_PRIM_W_ARITY("namespace-undefine-variable!", namespace_undefine_variable, 1, 2, env);
  GLOBAL_PRIM_W_ARITY("namespace-mapped-symbols", namespace_mapped_symbols, 0, 1, env);
  GLOBAL_PRIM_W_ARITY("namespace-module-registry", namespace_module_registry, 1, 1, env);

  GLOBAL_PRIM_W_ARITY("variable-reference?", variable_p, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("variable-reference->resolved-module-path", variable_module_path, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("variable-reference->module-source", variable_module_source, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("variable-reference->empty-namespace", variable_namespace, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("variable-reference->namespace", variable_top_level_namespace, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("variable-reference->phase", variable_phase, 1, 1, env);

  GLOBAL_PRIM_W_ARITY("syntax-transforming?", now_transforming, 0, 0, env);
  GLOBAL_PRIM_W_ARITY("syntax-local-value", local_exp_time_value, 1, 3, env);
  GLOBAL_PRIM_W_ARITY("syntax-local-value/immediate", local_exp_time_value_one, 1, 3, env);
  GLOBAL_PRIM_W_ARITY("syntax-local-name", local_exp_time_name, 0, 0, env);
  GLOBAL_PRIM_W_ARITY("syntax-local-context", local_context, 0, 0, env);
  GLOBAL_PRIM_W_ARITY("syntax-local-phase-level", local_phase_level, 0, 0, env);
  GLOBAL_PRIM_W_ARITY("syntax-local-make-definition-context", local_make_intdef_context, 0, 1, env);
  GLOBAL_PRIM_W_ARITY("internal-definition-context-seal", intdef_context_seal, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("internal-definition-context?", intdef_context_p, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("identifier-remove-from-definition-context", id_intdef_remove, 2, 2, env);
  GLOBAL_PRIM_W_ARITY("syntax-local-get-shadower", local_get_shadower, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("syntax-local-introduce", local_introduce, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("make-syntax-introducer", make_introducer, 0, 1, env);
  GLOBAL_PRIM_W_ARITY("syntax-local-make-delta-introducer", local_make_delta_introduce, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("syntax-local-certifier", local_certify, 0, 1, env);

  GLOBAL_PRIM_W_ARITY("syntax-local-module-exports", local_module_exports, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("syntax-local-module-defined-identifiers", local_module_definitions, 0, 0, env);
  GLOBAL_PRIM_W_ARITY("syntax-local-module-required-identifiers", local_module_imports, 2, 2, env);
  GLOBAL_PRIM_W_ARITY("syntax-local-transforming-module-provides?", local_module_expanding_provides, 0, 0, env);

  GLOBAL_PRIM_W_ARITY("make-set!-transformer", make_set_transformer, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("set!-transformer?", set_transformer_p, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("set!-transformer-procedure", set_transformer_proc, 1, 1, env);

  GLOBAL_PRIM_W_ARITY("make-rename-transformer", make_rename_transformer, 1, 2, env);
  GLOBAL_PRIM_W_ARITY("rename-transformer?", rename_transformer_p, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("rename-transformer-target", rename_transformer_target, 1, 1, env);

  GLOBAL_PRIM_W_ARITY("syntax-local-lift-expression", local_lift_expr, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("syntax-local-lift-values-expression", local_lift_exprs, 2, 2, env);
  GLOBAL_PRIM_W_ARITY("syntax-local-lift-context", local_lift_context, 0, 0, env);
  GLOBAL_PRIM_W_ARITY("syntax-local-lift-module-end-declaration", local_lift_end_statement, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("syntax-local-lift-require", local_lift_require, 2, 2, env);
  GLOBAL_PRIM_W_ARITY("syntax-local-lift-provide", local_lift_provide, 1, 1, env);

  
  REGISTER_SO(unshadowable_symbol);
  unshadowable_symbol = scheme_intern_symbol("unshadowable");

  DONE_TIME(env);

  scheme_install_type_writer(scheme_toplevel_type, write_toplevel);
  scheme_install_type_reader(scheme_toplevel_type, read_toplevel);
  scheme_install_type_writer(scheme_variable_type, write_variable);
  scheme_install_type_reader(scheme_variable_type, read_variable);
  scheme_install_type_writer(scheme_module_variable_type, write_module_variable);
  scheme_install_type_reader(scheme_module_variable_type, read_module_variable);
  scheme_install_type_writer(scheme_local_type, write_local);
  scheme_install_type_reader(scheme_local_type, read_local);
  scheme_install_type_writer(scheme_local_unbox_type, write_local);
  scheme_install_type_reader(scheme_local_unbox_type, read_local_unbox);
  scheme_install_type_writer(scheme_resolve_prefix_type, write_resolve_prefix);
  scheme_install_type_reader2(scheme_resolve_prefix_type, read_resolve_prefix);

  register_network_evts();

  REGISTER_SO(kernel_symbol);
  kernel_symbol = scheme_intern_symbol("#%kernel");

  MARK_START_TIME();

  scheme_finish_kernel(env);

#if USE_COMPILED_STARTUP
  if (builtin_ref_counter != EXPECTED_PRIM_COUNT) {
    printf("Primitive count %d doesn't match expected count %d\n"
	   "Turn off USE_COMPILED_STARTUP in src/schminc.h\n",
	   builtin_ref_counter, EXPECTED_PRIM_COUNT);
    abort();
  }
#endif

  init_unsafe(env);
  init_flfxnum(env);
  
  scheme_init_print_global_constants();
  scheme_init_variable_references_constants();

  scheme_defining_primitives = 0;
}

int scheme_is_kernel_env(Scheme_Env *env) {
  return (env == kernel_env);
}

Scheme_Env *scheme_get_kernel_env() {
  return kernel_env;
}

static void init_scheme_local() 
{
  int i, k, cor;

#ifndef USE_TAGGED_ALLOCATION
  GC_CAN_IGNORE Scheme_Local *all;

  all = (Scheme_Local *)scheme_malloc_eternal(sizeof(Scheme_Local) 
                                              * (MAX_CONST_LOCAL_FLAG_VAL + 1)
                                              * MAX_CONST_LOCAL_TYPES
                                              * MAX_CONST_LOCAL_POS);
# ifdef MEMORY_COUNTING_ON
  scheme_misc_count += (sizeof(Scheme_Local) 
                        * (MAX_CONST_LOCAL_FLAG_VAL + 1)
                        * MAX_CONST_LOCAL_TYPES
                        * MAX_CONST_LOCAL_POS);
# endif    
#endif

  for (i = 0; i < MAX_CONST_LOCAL_POS; i++) {
    for (k = 0; k < MAX_CONST_LOCAL_TYPES; k++) {
      for (cor = 0; cor < (MAX_CONST_LOCAL_FLAG_VAL + 1); cor++) {
        Scheme_Object *v;

#ifndef USE_TAGGED_ALLOCATION
        v = (Scheme_Object *)(all++);
#else
        v = (Scheme_Object *)scheme_malloc_eternal_tagged(sizeof(Scheme_Local));
#endif
        v->type = k + scheme_local_type;
        SCHEME_LOCAL_POS(v) = i;
        SCHEME_LOCAL_FLAGS(v) = cor;

        scheme_local[i][k][cor] = v;
      }
    }
  }
}

static void init_toplevels()
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

void scheme_prepare_env_renames(Scheme_Env *env, int kind)
{
  if (!env->rename_set) {
    Scheme_Object *rns;

    rns = scheme_make_module_rename_set(kind, NULL);
    env->rename_set = rns;
  }
}

Scheme_Env *scheme_make_empty_env(void)
{
  Scheme_Env *e;

  e = make_empty_inited_env(7);

  return e;
}

Scheme_Env *make_empty_inited_env(int toplevel_size)
{
  Scheme_Env *env;
  Scheme_Object *vector;
  Scheme_Hash_Table* hash_table;

  env = make_env(NULL, toplevel_size);

  vector = scheme_make_vector(5, scheme_false);
  hash_table = scheme_make_hash_table(SCHEME_hash_ptr);
  SCHEME_VEC_ELS(vector)[0] = (Scheme_Object *)hash_table;
  env->modchain = vector;

  hash_table = scheme_make_hash_table(SCHEME_hash_ptr);
  env->module_registry = hash_table;
  env->module_registry->iso.so.type = scheme_module_registry_type;

  hash_table = scheme_make_hash_table(SCHEME_hash_ptr);
  env->export_registry = hash_table;
  env->label_env = NULL;

  return env;
}

Scheme_Env *make_empty_not_inited_env(int toplevel_size)
{
  Scheme_Env *e;

  e = make_env(NULL, toplevel_size);

  return e;
}

static Scheme_Env *make_env(Scheme_Env *base, int toplevel_size)
{
  Scheme_Env *env;
  Scheme_Bucket_Table *bucket_table;

  env = MALLOC_ONE_TAGGED(Scheme_Env);
  env->so.type = scheme_namespace_type;

  bucket_table = scheme_make_bucket_table(toplevel_size, SCHEME_hash_ptr);
  env->toplevel = bucket_table;
  env->toplevel->with_home = 1;

  bucket_table = scheme_make_bucket_table(7, SCHEME_hash_ptr);
  env->syntax = bucket_table;

  if (base) {
    env->modchain = base->modchain;
    env->module_registry = base->module_registry;
    env->export_registry = base->export_registry;
    env->label_env = base->label_env;
  } else {
    env->modchain = NULL;
    env->module_registry = NULL;
    env->export_registry = NULL;
    env->label_env = NULL;
  }

  return env;
}

Scheme_Env *
scheme_new_module_env(Scheme_Env *env, Scheme_Module *m, int new_exp_module_tree)
{
  Scheme_Env *menv;

  menv = make_env(env, 7);

  menv->module = m;

  scheme_prepare_label_env(env);
  menv->label_env = env->label_env;

  if (new_exp_module_tree) {
    Scheme_Object *p;
    Scheme_Hash_Table *modules;

    modules = scheme_make_hash_table(SCHEME_hash_ptr);
    p = scheme_make_vector(5, scheme_false);
    SCHEME_VEC_ELS(p)[0] = (Scheme_Object *)modules;
    menv->modchain = p;
  }

  if (SAME_OBJ(env, env->exp_env)) {
    /* label phase */
    menv->exp_env = menv;
    menv->template_env = menv;
  }

  return menv;
}

void scheme_prepare_exp_env(Scheme_Env *env)
{
  if (!env->exp_env) {
    Scheme_Env *eenv;
    Scheme_Object *modchain;

    scheme_prepare_label_env(env);

    eenv = make_empty_not_inited_env(7);
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
      modchain = scheme_make_vector(5, scheme_false);
      SCHEME_VEC_ELS(modchain)[0] = (Scheme_Object *)next_modules;
      SCHEME_VEC_ELS(env->modchain)[1] = modchain;
      SCHEME_VEC_ELS(modchain)[2] = env->modchain;
    }
    eenv->modchain = modchain;

    env->exp_env = eenv;
    eenv->template_env = env;
    eenv->label_env = env->label_env;

    scheme_prepare_env_renames(env, mzMOD_RENAME_TOPLEVEL);
    eenv->rename_set = env->rename_set;

    if (env->disallow_unbound)
      eenv->disallow_unbound = 1;
  }
}

void scheme_prepare_template_env(Scheme_Env *env)
{
  if (!env->template_env) {
    Scheme_Env *eenv;
    Scheme_Object *modchain;

    scheme_prepare_label_env(env);

    eenv = make_empty_not_inited_env(7);
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
      modchain = scheme_make_vector(5, scheme_false);
      SCHEME_VEC_ELS(modchain)[0] = (Scheme_Object *)prev_modules;
      SCHEME_VEC_ELS(env->modchain)[2] = modchain;
      SCHEME_VEC_ELS(modchain)[1] = env->modchain;
    }
    eenv->modchain = modchain;

    scheme_prepare_env_renames(env, mzMOD_RENAME_TOPLEVEL);
    eenv->rename_set = env->rename_set;

    env->template_env = eenv;
    eenv->exp_env = env;       
    eenv->label_env = env->label_env;

    if (env->disallow_unbound)
      eenv->disallow_unbound = 1;
  }
}

void scheme_prepare_label_env(Scheme_Env *env)
{
  if (!env->label_env) {
    Scheme_Env *lenv;
    Scheme_Object *modchain;
    Scheme_Hash_Table *prev_modules;

    lenv = make_empty_not_inited_env(7);
    lenv->phase = 0;
    lenv->mod_phase = 0;

    lenv->module = env->module;
    lenv->module_registry = env->module_registry;
    lenv->export_registry = env->export_registry;
    lenv->insp = env->insp;

    modchain = scheme_make_vector(5, scheme_false);    
    prev_modules = scheme_make_hash_table(SCHEME_hash_ptr);
    SCHEME_VEC_ELS(modchain)[0] = (Scheme_Object *)prev_modules;
    SCHEME_VEC_ELS(modchain)[2] = modchain;
    SCHEME_VEC_ELS(modchain)[1] = modchain;
    lenv->modchain = modchain;

    env->label_env = lenv;

    lenv->exp_env = lenv;
    lenv->label_env = lenv;
    lenv->template_env = lenv;
  }
}

Scheme_Env *scheme_copy_module_env(Scheme_Env *menv, Scheme_Env *ns, Scheme_Object *modchain, int clone_phase)
{
  /* New env should have the same syntax and globals table, but it lives in
     a different namespace. */
  Scheme_Env *menv2;
  Scheme_Bucket_Table *bucket_table;

  scheme_prepare_label_env(ns);

  menv2 = MALLOC_ONE_TAGGED(Scheme_Env);
  menv2->so.type = scheme_namespace_type;

  menv2->module = menv->module;
  menv2->module_registry = ns->module_registry;
  menv2->export_registry = ns->export_registry;
  menv2->insp = menv->insp;

  if (menv->phase < clone_phase)
    menv2->syntax = menv->syntax;
  else {
    bucket_table = scheme_make_bucket_table(7, SCHEME_hash_ptr);
    menv2->syntax = bucket_table;
  }

  menv2->phase = menv->phase;
  menv2->mod_phase = menv->mod_phase;
  menv2->link_midx = menv->link_midx;
  if (menv->phase <= clone_phase) {
    menv2->running = menv->running;
    menv2->ran = menv->ran;
  }
  if (menv->phase < clone_phase)
    menv2->et_running = menv->et_running;

  menv2->require_names = menv->require_names;
  menv2->et_require_names = menv->et_require_names;

  if (menv->phase <= clone_phase) {
    menv2->toplevel = menv->toplevel;
  } else {
    bucket_table = scheme_make_bucket_table(7, SCHEME_hash_ptr);
    menv2->toplevel = bucket_table;
    menv2->toplevel->with_home = 1;
  }
  
  menv2->modchain = modchain;

  if (SAME_OBJ(menv->exp_env, menv)) {
    /* label phase */
    menv2->exp_env = menv2;
    menv2->template_env = menv2;
  } else if (menv->phase < clone_phase) {
    if (!SCHEME_NULLP(menv2->module->et_requires)) {
      /* We'll need the next link in the modchain: */
      modchain = SCHEME_VEC_ELS(modchain)[1];
      if (SCHEME_FALSEP(modchain)) {
        Scheme_Hash_Table *next_modules;
      
        next_modules = scheme_make_hash_table(SCHEME_hash_ptr);
        modchain = scheme_make_vector(5, scheme_false);
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
  }
   
  scheme_prepare_label_env(ns);
  menv2->label_env = ns->label_env;

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
  Scheme_Object *rn;

  if (env->rename_set) {
    rn = scheme_get_module_rename_from_set(env->rename_set,
                                           scheme_make_integer(env->phase),
                                           0);
    if (rn) {
      scheme_remove_module_rename(rn, n);
      if (env->module) {
        scheme_extend_module_rename(rn,
                                    env->module->self_modidx,
                                    n, n,
                                    env->module->self_modidx,
                                    n,
                                    env->mod_phase,
                                    NULL,
                                    NULL,
                                    NULL,
                                    0);
      }
    }
  } else
    rn = NULL;

  if (stxtoo) {
    if (!env->module || rn) {
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

    if (rn) {
      /* If the syntax binding is a rename transformer, need to install 
         a mapping. */
      Scheme_Object *v;
      v = scheme_lookup_in_table(env->syntax, (const char *)n);
      if (v) {
        v = SCHEME_PTR_VAL(v);
        if (scheme_is_binding_rename_transformer(v)) {
          scheme_install_free_id_rename(n, 
                                        scheme_rename_transformer_id(v), 
                                        rn, 
                                        scheme_make_integer(env->phase));
        }
      }
    }
  }
}

/********** Auxilliary tables **********/

Scheme_Object **scheme_make_builtin_references_table(void)
{
  Scheme_Bucket_Table *ht;
  Scheme_Object **t;
  Scheme_Bucket **bs;
  Scheme_Env *kenv;
  long i;
  int j;

  t = MALLOC_N(Scheme_Object *, (builtin_ref_counter + 1));
#ifdef MEMORY_COUNTING_ON
  scheme_misc_count += sizeof(Scheme_Object *) * (builtin_ref_counter + 1);
#endif

  for (j = 0; j < 3; j++) {
    if (!j)
      kenv = kernel_env;
    else if (j == 1)
      kenv = unsafe_env;
    else
      kenv = flfxnum_env;
    
    ht = kenv->toplevel;
    
    bs = ht->buckets;
    
    for (i = ht->size; i--; ) {
      Scheme_Bucket *b = bs[i];
      if (b && (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_HAS_REF_ID))
        t[((Scheme_Bucket_With_Ref_Id *)b)->id] = (Scheme_Object *)b->val;
    }
  }

  return t;
}

Scheme_Hash_Table *scheme_map_constants_to_globals(void)
{
  Scheme_Bucket_Table *ht;
  Scheme_Hash_Table*result;
  Scheme_Bucket **bs;
  Scheme_Env *kenv;
  long i;
  int j;

  result = scheme_make_hash_table(SCHEME_hash_ptr);
      
  for (j = 0; j < 3; j++) {
    if (!j)
      kenv = kernel_env;
    else if (j == 1)
      kenv = unsafe_env;
    else
      kenv = flfxnum_env;
    
    ht = kenv->toplevel;
    bs = ht->buckets;
    
    for (i = ht->size; i--; ) {
      Scheme_Bucket *b = bs[i];
      if (b && (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_IS_CONST)) {
        scheme_hash_set(result, b->val, (Scheme_Object *)b);
      }
    }
  }

  return result;
}

const char *scheme_look_for_primitive(void *code)
{
  Scheme_Bucket_Table *ht;
  Scheme_Bucket **bs;
  Scheme_Env *kenv;
  long i;
  int j;

  for (j = 0; j < 3; j++) {
    if (!j)
      kenv = kernel_env;
    else if (j == 1)
      kenv = unsafe_env;
    else
      kenv = flfxnum_env;
    
    ht = kenv->toplevel;
    bs = ht->buckets;
    
    for (i = ht->size; i--; ) {
      Scheme_Bucket *b = bs[i];
      if (b && b->val) {
        if (SCHEME_PRIMP(b->val)) {
          if (SCHEME_PRIM(b->val) == code)
            return ((Scheme_Primitive_Proc *)b->val)->name;
        }
      }
    }
  }

  return NULL;
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
                                 Scheme_Object *end_stmts, Scheme_Object *context_key, 
                                 Scheme_Object *requires, Scheme_Object *provides)
{
  Scheme_Lift_Capture_Proc *pp;
  Scheme_Object *vec;
  
  pp = (Scheme_Lift_Capture_Proc *)scheme_malloc_atomic(sizeof(Scheme_Lift_Capture_Proc));
  *pp = cp;

  vec = scheme_make_vector(8, NULL);
  SCHEME_VEC_ELS(vec)[0] = scheme_null;
  SCHEME_VEC_ELS(vec)[1] = (Scheme_Object *)pp;
  SCHEME_VEC_ELS(vec)[2] = data;
  SCHEME_VEC_ELS(vec)[3] = end_stmts;
  SCHEME_VEC_ELS(vec)[4] = context_key;
  SCHEME_VEC_ELS(vec)[5] = (requires ? requires : scheme_false);
  SCHEME_VEC_ELS(vec)[6] = scheme_null; /* accumulated requires */
  SCHEME_VEC_ELS(vec)[7] = provides;

  COMPILE_DATA(env)->lifts = vec;
}

void scheme_propagate_require_lift_capture(Scheme_Comp_Env *orig_env, Scheme_Comp_Env *env)
{
  while (orig_env) {
    if ((COMPILE_DATA(orig_env)->lifts)
        && SCHEME_TRUEP(SCHEME_VEC_ELS(COMPILE_DATA(orig_env)->lifts)[5]))
      break;
    orig_env = orig_env->next;
  }
  
  if (orig_env) {
    Scheme_Object *vec, *p;

    p = scheme_make_raw_pair(NULL, (Scheme_Object *)orig_env);

    vec = scheme_make_vector(8, NULL);
    SCHEME_VEC_ELS(vec)[0] = scheme_false;
    SCHEME_VEC_ELS(vec)[1] = scheme_void;
    SCHEME_VEC_ELS(vec)[2] = scheme_void;
    SCHEME_VEC_ELS(vec)[3] = scheme_false;
    SCHEME_VEC_ELS(vec)[4] = scheme_false;
    SCHEME_VEC_ELS(vec)[5] = p; /* (rcons NULL env) => continue with env */
    SCHEME_VEC_ELS(vec)[6] = scheme_null;
    SCHEME_VEC_ELS(vec)[7] = scheme_false;

    COMPILE_DATA(env)->lifts = vec;
  }
}

Scheme_Object *scheme_frame_get_lifts(Scheme_Comp_Env *env)
{
  return scheme_reverse(SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[0]);
}

Scheme_Object *scheme_frame_get_end_statement_lifts(Scheme_Comp_Env *env)
{
  return SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[3];
}

Scheme_Object *scheme_frame_get_require_lifts(Scheme_Comp_Env *env)
{
  return SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[6];
}

Scheme_Object *scheme_frame_get_provide_lifts(Scheme_Comp_Env *env)
{
  return SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[7];
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
						  Scheme_Compile_Info *rec, int drec,
                                                  int imported)
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

  o = make_toplevel(0, cp->num_toplevels, 0, imported ? SCHEME_TOPLEVEL_READY : 0);

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
    l->iso.so.type = scheme_compiled_quote_syntax_type;
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
  l->iso.so.type = scheme_compiled_quote_syntax_type;
  l->position = pos;

  cp->num_stxes++;
  o = (Scheme_Object *)l;
  
  scheme_hash_set(cp->stxes, var, o);

  return o;
}

void scheme_register_unsafe_in_prefix(Scheme_Comp_Env *env, 
                                      Scheme_Compile_Info *rec, int drec,
                                      Scheme_Env *menv)
{
  Scheme_Object *v, *insp;

  if (rec && rec[drec].dont_mark_local_use) {
    return;
  }

  insp = menv->module->insp;

  v = env->prefix->uses_unsafe;
  if (!v)
    v = insp;
  else if (!SAME_OBJ(v, insp)) {
    Scheme_Hash_Tree *ht;

    if (SCHEME_HASHTRP(v)) {
      ht = (Scheme_Hash_Tree *)v;
    } else {
      ht = scheme_make_hash_tree(0);
      ht = scheme_hash_tree_set(ht, v, scheme_true);
    }
    
    if (!scheme_hash_tree_get(ht, insp)) {
      ht = scheme_hash_tree_set(ht, insp, scheme_true);
      env->prefix->uses_unsafe = (Scheme_Object *)ht;
    }
  }
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

Scheme_Object *scheme_make_local(Scheme_Type type, int pos, int flags)
{
  int k;
  Scheme_Object *v, *key;

  k = type - scheme_local_type;
  
  /* Helper for reading bytecode: make sure flags is a valid value */
  switch (flags) {
  case 0:
  case SCHEME_LOCAL_CLEAR_ON_READ:
  case SCHEME_LOCAL_OTHER_CLEARS:
  case SCHEME_LOCAL_FLONUM:
    break;
  default:
    flags  = SCHEME_LOCAL_OTHER_CLEARS;
    break;
  }

  if (pos < MAX_CONST_LOCAL_POS) {
    return scheme_local[pos][k][flags];
  }

  key = scheme_make_integer(pos);
  if (flags) {
    key = scheme_make_pair(scheme_make_integer(flags), key);
  }

  v = scheme_hash_get(locals_ht[k], key);
  if (v)
    return v;

  v = alloc_local(type, pos);
  SCHEME_LOCAL_FLAGS(v) = flags;

  if (locals_ht[k]->count > TABLE_CACHE_MAX_SIZE) {
    Scheme_Hash_Table *ht;
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    locals_ht[k] = ht;
  }

  scheme_hash_set(locals_ht[k], key, v);

  return v;
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

  return (Scheme_Local *)scheme_make_local(scheme_local_type, p + i, 0);
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

Scheme_Object *scheme_tl_id_sym(Scheme_Env *env, Scheme_Object *id, Scheme_Object *bdg, 
                                int mode, /* -1, 0 => lookup; 2, 3 => define
                                             -1 and 3 => use temp table
                                             1 would mean define if no match; not currently used */
                                Scheme_Object *phase, int *_skipped)
/* The `env' argument can actually be a hash table. */
{
  Scheme_Object *marks = NULL, *sym, *map, *l, *a, *amarks, *m, *best_match, *cm, *abdg;
  int best_match_skipped, ms, one_mark;
  Scheme_Hash_Table *marked_names, *temp_marked_names, *dest_marked_names;

  sym = SCHEME_STX_SYM(id);

  if (_skipped)
    *_skipped = -1;

  if (SCHEME_HASHTP((Scheme_Object *)env)) {
    marked_names = (Scheme_Hash_Table *)env;
    temp_marked_names = NULL;
  } else {
    /* If there's no table and we're not defining, bail out fast */
    if ((mode <= 0) && !env->rename_set)
      return sym;
    marked_names = scheme_get_module_rename_marked_names(env->rename_set,
                                                         phase ? phase : scheme_make_integer(env->phase),
                                                         0);
    temp_marked_names = env->temp_marked_names;
  }

  if (mode > 0) {
    /* If we're defining, see if we need to create a table.  Getting
       marks is relatively expensive, but we only do this once per
       definition. */
    if (!bdg)
      bdg = scheme_stx_moduleless_env(id);
    marks = scheme_stx_extract_marks(id);
    if (SCHEME_NULLP(marks) && SCHEME_FALSEP(bdg))
      return sym;
  }

  if (!marked_names) {
    scheme_prepare_env_renames(env, mzMOD_RENAME_TOPLEVEL);
    marked_names = scheme_get_module_rename_marked_names(env->rename_set,
                                                         phase ? phase : scheme_make_integer(env->phase),
                                                         1);
  }
  if (!temp_marked_names && (mode > 2)) {
    /* The "temp" marked name table is used to correlate marked module
       requires with similarly marked provides. We don't go through
       the normal rename table because (for efficiency) the marks in
       this case are handled more directly in the shared_pes module
       renamings. */
    temp_marked_names = scheme_make_hash_table(SCHEME_hash_ptr);
    env->temp_marked_names = temp_marked_names;
  }
  
  map = scheme_hash_get(marked_names, sym);
  if (!map && ((mode < 0) || (mode > 2)) && temp_marked_names)
    map = scheme_hash_get(temp_marked_names, sym);

  if (!map) {
    /* If we're not defining, we can bail out before extracting marks. */
    if (mode <= 0)
      return sym;
    else
      map = scheme_null;
  }

  if (!bdg) {
    /* We need lexical binding, if any, too: */
    bdg = scheme_stx_moduleless_env(id);
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
      if (mode > 0) {
	if (scheme_equal(amarks, marks)) {
	  best_match = SCHEME_CDR(a);
	  break;
	}
      } else {
        if (!SCHEME_PAIRP(marks)) {
	  /* To be better than nothing, could only match exactly: */
	  if (scheme_equal(amarks, marks)
              || SCHEME_NULLP(amarks)) {
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
    if (mode <= 0) {
      return sym;
    }

    /* Last chance before making up a new name. If we're processing a
       module body generated by `expand', then we picked a name last
       time around. We can't pick a new name now, otherwise
       "redundant" module renamings wouldn't be redundant. (See
       simpify in stxobj.c.) So check for a context-determined
       existing rename. */
    if (!SCHEME_HASHTP((Scheme_Object *)env) && env->module && (mode < 2)) {
      Scheme_Object *mod, *nm = id;
      mod = scheme_stx_module_name(NULL, &nm, scheme_make_integer(env->phase), NULL, NULL, NULL, 
                                   NULL, NULL, NULL, NULL, NULL);
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
	sprintf(buf XFORM_OK_PLUS len, ".%d", env->id_counter);
	
	best_match = scheme_intern_exact_parallel_symbol(buf, strlen(buf));

	if (!scheme_stx_parallel_is_used(best_match, id)) {
	  /* Also check environment's rename tables. This last check
	     includes the temp table. It also turns out to matter for
	     compiling in `module->namespace' contexts, because no
	     renaming is added after expansion to record the rename
	     table. */
	  if (!scheme_tl_id_is_sym_used(marked_names, best_match)
              && (!temp_marked_names
                  || !scheme_tl_id_is_sym_used(temp_marked_names, best_match))) {
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
    
    dest_marked_names = ((mode < 0) || (mode > 2)) ? temp_marked_names : marked_names;
    scheme_hash_set(dest_marked_names, sym, map);
    {
      Scheme_Hash_Table *rev_ht;
      rev_ht = (Scheme_Hash_Table *)scheme_hash_get(dest_marked_names, scheme_false);
      if (rev_ht) {
        scheme_hash_set(rev_ht, best_match, scheme_true);
      }
    }
  } else {
    if (_skipped)
      *_skipped = best_match_skipped;
  }

  return best_match;
}

int scheme_tl_id_is_sym_used(Scheme_Hash_Table *marked_names, Scheme_Object *sym)
{
  int i;
  Scheme_Object *l, *a;
  Scheme_Hash_Table *rev_ht;

  if (!marked_names)
    return 0;

  if (!marked_names->count)
    return 0;

  rev_ht = (Scheme_Hash_Table *)scheme_hash_get(marked_names, scheme_false);

  if (!rev_ht) {
    rev_ht = scheme_make_hash_table(SCHEME_hash_ptr);

    for (i = marked_names->size; i--; ) {
      l = marked_names->vals[i];
      if (l) {
        for (; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
          a = SCHEME_CAR(l);
          scheme_hash_set(rev_ht, SCHEME_CDR(a), scheme_true);
        }
      }
      scheme_hash_set(marked_names, scheme_false, (Scheme_Object *)rev_ht);
    }
  }

  if (scheme_hash_get(rev_ht, sym))
    return 1;

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

  if (SCHEME_RIBP(stx)) {
    GC_CAN_IGNORE int *s;
    s = scheme_stx_get_rib_sealed(stx);
    COMPILE_DATA(env)->sealed = s;
  }

  while (env != upto) {
    if (!(env->flags & (SCHEME_NO_RENAME | SCHEME_CAPTURE_WITHOUT_RENAME 
                        | SCHEME_CAPTURE_LIFTED | SCHEME_INTDEF_SHADOW))) {
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
    } else if (env->flags & SCHEME_INTDEF_SHADOW) {
      /* Just extract existing uids from identifiers, and don't need to
         add renames to syntax objects. */
      if (!env->uids) {
        Scheme_Object **uids, *uid;
        int i;
        
        uids = MALLOC_N(Scheme_Object *, env->num_bindings);
        env->uids = uids;
        
        for (i = env->num_bindings; i--; ) {
          uid = scheme_stx_moduleless_env(env->values[i]);
          if (SCHEME_FALSEP(uid))
            scheme_signal_error("intdef shadow binding is #f for %d/%s",
                                SCHEME_TYPE(env->values[i]),
                                scheme_write_to_string(SCHEME_STX_VAL(env->values[i]), 
                                                       NULL));
          env->uids[i] = uid;
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
  int j = 0, p = 0, modpos, skip_stops = 0, module_self_reference = 0;
  Scheme_Bucket *b;
  Scheme_Object *val, *modidx, *modname, *src_find_id, *find_global_id, *mod_defn_phase;
  Scheme_Object *find_id_sym = NULL, *rename_insp = NULL;
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

      if (!find_id_sym 
          && (frame->flags & SCHEME_CAPTURE_WITHOUT_RENAME))
        find_id_sym = scheme_stx_get_module_eq_sym(find_id, scheme_make_integer(phase));

      for (i = frame->num_bindings; i--; ) {
	if (frame->values[i]) {
	  if (frame->uids) 
	    uid = frame->uids[i];
          if (SAME_OBJ(SCHEME_STX_VAL(find_id), SCHEME_STX_VAL(frame->values[i]))
	      && (scheme_stx_env_bound_eq(find_id, frame->values[i], uid, scheme_make_integer(phase))
		  || ((frame->flags & SCHEME_CAPTURE_WITHOUT_RENAME)
		      && scheme_stx_module_eq2(find_id, frame->values[i], scheme_make_integer(phase), find_id_sym))
		  || ((frame->flags & SCHEME_CAPTURE_LIFTED)
		      && scheme_stx_bound_eq(find_id, frame->values[i], scheme_make_integer(phase))))) {
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
	      return scheme_make_local(scheme_local_type, 0, 0);
	    else
	      return (Scheme_Object *)get_frame_loc(frame, i, j, p, flags);
	  }
	}
      }

      for (i = COMPILE_DATA(frame)->num_const; i--; ) {
	int issame;
	if (frame->flags & SCHEME_CAPTURE_WITHOUT_RENAME)
	  issame = scheme_stx_module_eq2(find_id, COMPILE_DATA(frame)->const_names[i], 
                                         scheme_make_integer(phase), find_id_sym);
        else {
	  if (COMPILE_DATA(frame)->const_uids) uid = COMPILE_DATA(frame)->const_uids[i];
	  issame = (SAME_OBJ(SCHEME_STX_VAL(find_id), 
			     SCHEME_STX_VAL(COMPILE_DATA(frame)->const_names[i]))
		    && scheme_stx_env_bound_eq(find_id, COMPILE_DATA(frame)->const_names[i], uid, 
                                               scheme_make_integer(phase)));
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

          if (_lexical_binding_id) {
            if (!(frame->flags & SCHEME_CAPTURE_WITHOUT_RENAME))
              val = scheme_stx_remove_extra_marks(find_id, COMPILE_DATA(frame)->const_names[i],
                                                  ((frame->flags & SCHEME_CAPTURE_LIFTED)
                                                   ? NULL
                                                   : uid));
            else
              val = find_id;
            *_lexical_binding_id = val;
          }

	  val = COMPILE_DATA(frame)->const_vals[i];
	
	  if (!val) {
            scheme_wrong_syntax(scheme_compile_stx_string, NULL, find_id,
                                "identifier used out of context");
	    return NULL;
	  }

	  if (SCHEME_FALSEP(val)) {
	    /* Corresponds to a run-time binding (but will be replaced later
	       through a renaming to a different binding) */
            if (flags & SCHEME_OUT_OF_CONTEXT_LOCAL)
              return scheme_make_local(scheme_local_type, 0, 0);
            return NULL;
	  }

	  if (!(flags & SCHEME_ENV_CONSTANTS_OK)) {
	    if (SAME_TYPE(SCHEME_TYPE(val), scheme_macro_type))
	      return val;
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
  modidx = scheme_stx_module_name(NULL, &find_id, scheme_make_integer(phase), NULL, NULL, &mod_defn_phase, 
                                  NULL, NULL, NULL, NULL, &rename_insp);

  /* Used out of context? */
  if (SAME_OBJ(modidx, scheme_undefined)) {
    if (SCHEME_STXP(find_id)) {
      /* Looks like lexically bound, but double-check that it's not bound via a tl_id: */
      find_global_id = scheme_tl_id_sym(env->genv, find_id, NULL, 0, NULL, NULL);
      if (!SAME_OBJ(find_global_id, SCHEME_STX_VAL(find_id)))
        modidx = NULL; /* yes, it is bound */
    }
    
    if (modidx) {
      if (!(flags & SCHEME_OUT_OF_CONTEXT_OK)) {
        scheme_wrong_syntax(scheme_compile_stx_string, NULL, find_id,
                            "identifier used out of context");
      }
      if (flags & SCHEME_OUT_OF_CONTEXT_LOCAL)
        return scheme_make_local(scheme_local_type, 0, 0);
      return NULL;
    }
  }

  if (modidx) {
    /* If it's an access path, resolve it: */
    modname = scheme_module_resolve(modidx, 1);

    if (env->genv->module && SAME_OBJ(modname, env->genv->module->modname)) {
      modidx = NULL;
      modname = NULL;
      genv = env->genv;
      /* So we can distinguish between unbound identifiers in a module
	 and references to top-level definitions: */
      module_self_reference = 1;
    } else {
      genv = scheme_module_access(modname, env->genv, SCHEME_INT_VAL(mod_defn_phase));

      if (!genv) {
	if (env->genv->phase) {
	  /* The failure might be due a laziness in required-syntax
	     execution. Force all laziness at the prior level 
	     and try again. */
	  scheme_module_force_lazy(env->genv, 1);
	  genv = scheme_module_access(modname, env->genv, SCHEME_INT_VAL(mod_defn_phase));
	}

	if (!genv) {
	  scheme_wrong_syntax("require", NULL, src_find_id, 
                              "namespace mismatch; reference (phase %d) to a module"
                              " %D that is not available (phase level %d)", 
			      env->genv->phase, modname, SCHEME_INT_VAL(mod_defn_phase));
	  return NULL;
	}
      }
    }
  } else {
    genv = env->genv;
    modname = NULL;

    if (genv->module && genv->disallow_unbound) {
      /* Free identifier. Maybe don't continue. */
      if (flags & (SCHEME_SETTING | SCHEME_REFERENCING)) {
        scheme_wrong_syntax(((flags & SCHEME_SETTING) 
			     ? scheme_set_stx_string
			     : scheme_var_ref_string),
			    NULL, src_find_id, "unbound identifier in module");
	return NULL;
      }
      if (flags & SCHEME_NULL_FOR_UNBOUND)
	return NULL;
    }
  }

  if (_menv && genv->module)
    *_menv = genv;
  
  if (!modname && SCHEME_STXP(find_id))
    find_global_id = scheme_tl_id_sym(env->genv, find_id, NULL, 0, NULL, NULL);
  else
    find_global_id = find_id;

  /* Try syntax table: */
  if (modname) {
    val = scheme_module_syntax(modname, env->genv, find_id);
    if (val && !(flags & SCHEME_NO_CERT_CHECKS))
      scheme_check_accessible_in_module(genv, env->insp, in_modidx, 
					find_id, src_find_id, certs, NULL, rename_insp,
                                        -2, 0, 
					NULL, NULL,
                                        env->genv, NULL);
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
    return val;
  }

  if (modname) {
    Scheme_Object *pos;
    if (flags & SCHEME_NO_CERT_CHECKS) 
      pos = 0;
    else
      pos = scheme_check_accessible_in_module(genv, env->insp, in_modidx, 
					      find_id, src_find_id, certs, NULL, rename_insp, -1, 1,
					      _protected, NULL, env->genv, NULL);
    modpos = SCHEME_INT_VAL(pos);
  } else
    modpos = -1;

  if (modname && (flags & SCHEME_SETTING)) {
    if (SAME_OBJ(src_find_id, find_id) || SAME_OBJ(SCHEME_STX_SYM(src_find_id), find_id))
      find_id = NULL;
    scheme_wrong_syntax(scheme_set_stx_string, find_id, src_find_id, "cannot mutate module-required identifier");
    return NULL;
  }

  if (!modname && (flags & (SCHEME_SETTING | SCHEME_REFERENCING)) 
      && (genv->module && genv->disallow_unbound)) {
    /* Check for set! of unbound identifier: */    
    if (!scheme_lookup_in_table(genv->toplevel, (const char *)find_global_id)) {
      scheme_wrong_syntax(((flags & SCHEME_SETTING) 
			     ? scheme_set_stx_string
			     : scheme_var_ref_string), 
			  NULL, src_find_id, "unbound identifier in module");
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
      && (!(scheme_is_kernel_modname(modname) 
            || scheme_is_unsafe_modname(modname)
            || scheme_is_flfxnum_modname(modname))
          || (flags & SCHEME_REFERENCING))) {
    /* Create a module variable reference, so that idx is preserved: */
    return scheme_hash_module_variable(env->genv, modidx, find_id, 
				       genv->module->insp,
				       modpos, SCHEME_INT_VAL(mod_defn_phase));
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
      && !(flags & SCHEME_GLOB_ALWAYS_REFERENCE)
      && (!modname || scheme_is_kernel_modname(modname)))
    return (Scheme_Object *)b->val;

  ASSERT_IS_VARIABLE_BUCKET(b);
  if (!((Scheme_Bucket_With_Home *)b)->home)
    ((Scheme_Bucket_With_Home *)b)->home = genv;
  
  return (Scheme_Object *)b;
}

int scheme_is_imported(Scheme_Object *var, Scheme_Comp_Env *env)
{
  if (env->genv->module) {
    if (SAME_TYPE(SCHEME_TYPE(var), scheme_module_variable_type)) {
      if (!SAME_OBJ(((Module_Variable *)var)->modidx, env->genv->module->self_modidx))
        return 1;
    } else
      return 1;
  } else {
    if (SAME_TYPE(SCHEME_TYPE(var), scheme_variable_type)) {
      if (!SAME_OBJ(((Scheme_Bucket_With_Home *)var)->home, env->genv))
        return 1;
    } else
      return 1;
  }
  return 0;
}

Scheme_Object *scheme_extract_unsafe(Scheme_Object *o)
{
  Scheme_Env *home = ((Scheme_Bucket_With_Home *)o)->home;
  if (home && home->module && scheme_is_unsafe_modname(home->module->modname))
    return (Scheme_Object *)((Scheme_Bucket *)o)->val;
  else
    return NULL;
}

Scheme_Object *scheme_extract_flfxnum(Scheme_Object *o)
{
  Scheme_Env *home = ((Scheme_Bucket_With_Home *)o)->home;
  if (home && home->module && scheme_is_flfxnum_modname(home->module->modname))
    return (Scheme_Object *)((Scheme_Bucket *)o)->val;
  else
    return NULL;
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
      if (scheme_stx_bound_eq(symbol, r->syms[i], scheme_make_integer(r->phase)))
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
    mod = scheme_stx_module_name(NULL, &id, scheme_make_integer(env->phase), NULL, NULL, NULL, 
                                 NULL, NULL, NULL, NULL, NULL);
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
  info->inline_fuel = 32;
  
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
     indices are resolved two new indices in the second phase of
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

void scheme_optimize_propagate(Optimize_Info *info, int pos, Scheme_Object *value, int single_use)
{
  /* A raw-pair `value' is an indicator for whether a letrec-bound
     variable is ready. */
  Scheme_Object *p;

  p = scheme_make_vector(4, NULL);
  SCHEME_VEC_ELS(p)[0] = info->consts;
  SCHEME_VEC_ELS(p)[1] = scheme_make_integer(pos);
  SCHEME_VEC_ELS(p)[2] = value;
  SCHEME_VEC_ELS(p)[3] = (single_use ? scheme_true : scheme_false);

  info->consts = p;
}

Scheme_Once_Used *scheme_make_once_used(Scheme_Object *val, int pos, int vclock, Scheme_Once_Used *prev)
{
  Scheme_Once_Used *o;

  o = MALLOC_ONE_TAGGED(Scheme_Once_Used);
  o->so.type = scheme_once_used_type;

  o->expr = val;
  o->pos = pos;
  o->vclock = vclock;

  if (prev)
    prev->next = o;
  
  return o;
}

static void register_use(Optimize_Info *info, int pos, int flag)
/* pos must be in immediate frame */
{
  if (!info->use) {
    char *use;
    use = (char *)scheme_malloc_atomic(info->new_frame);
    memset(use, 0, info->new_frame);
    info->use = use;
  }
  info->use[pos] |= flag;
}

void scheme_optimize_mutated(Optimize_Info *info, int pos)
/* pos must be in immediate frame */
{
  register_use(info, pos, 0x1);
}

void scheme_optimize_produces_flonum(Optimize_Info *info, int pos)
/* pos must be in immediate frame */
{
  register_use(info, pos, 0x4);
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
    if (info->use && (info->use[pos] & 0x1))
      return NULL;

  return scheme_make_local(scheme_local_type, pos + delta, 0);
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

static int check_use(Optimize_Info *info, int pos, int flag)
/* pos is in new-frame counts */
{
  while (1) {
    if (pos < info->new_frame)
      break;
    pos -= info->new_frame;
    info = info->next;
  }

  if (info->use && (info->use[pos] & flag))
    return 1;

  return 0;
}

int scheme_optimize_is_mutated(Optimize_Info *info, int pos)
/* pos is in new-frame counts */
{
  return check_use(info, pos, 0x1);
}

int scheme_optimize_is_flonum_arg(Optimize_Info *info, int pos, int depth)
/* pos is in new-frame counts */
{
  return check_use(info, pos, 0x2);
}

int scheme_optimize_is_flonum_valued(Optimize_Info *info, int pos)
/* pos is in new-frame counts */
{
  return check_use(info, pos, 0x4);
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

static Scheme_Object *do_optimize_info_lookup(Optimize_Info *info, int pos, int j, int *closure_offset, int *single_use, 
                                              int *not_ready, int once_used_ok, int context, int *potential_size)
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

  if (context & OPT_CONTEXT_FLONUM_ARG)
    register_use(info, pos, 0x2);

  p = info->consts;
  while (p) {
    n = SCHEME_VEC_ELS(p)[1];
    if (SCHEME_INT_VAL(n) == pos) {
      n = SCHEME_VEC_ELS(p)[2];
      if (SCHEME_RPAIRP(n)) {
        /* This was a letrec-bound identifier that may or may not be ready,
           but which wasn't replaced with more information. */
        if (not_ready)
          *not_ready = SCHEME_TRUEP(SCHEME_CAR(n));
        break;
      }
      if (SCHEME_BOXP(n)) {
        /* A potential-size record: */
        if (potential_size)
          *potential_size = SCHEME_INT_VAL(SCHEME_BOX_VAL(n));
        break;
      }
      if (single_use)
        *single_use = SCHEME_TRUEP(SCHEME_VEC_ELS(p)[3]);
      if (SAME_TYPE(SCHEME_TYPE(n), scheme_compiled_unclosed_procedure_type)) {
	if (!closure_offset)
	  break;
	else {
	  *closure_offset = delta;
	}
      } else if (SAME_TYPE(SCHEME_TYPE(n), scheme_compiled_toplevel_type)) {
        /* Ok */
      } else if (closure_offset) {
        /* Inlining can deal procedures and top-levels, but not other things. */
        return NULL;
      } else if (SAME_TYPE(SCHEME_TYPE(n), scheme_once_used_type)) {
        Scheme_Once_Used *o;

        if (!once_used_ok)
          break;

        o = (Scheme_Once_Used *)n;
        o->delta = delta;
        o->info = info;
        return (Scheme_Object *)o;
      } else if (SAME_TYPE(SCHEME_TYPE(n), scheme_local_type)) {
	int pos;

	pos = SCHEME_LOCAL_POS(n);
	if (info->flags & SCHEME_LAMBDA_FRAME)
	  j--; /* because it will get re-added on recur */

	/* Marks local as used; we don't expect to get back
	   a value, because chaining would normally happen on the 
	   propagate-call side. Chaining there also means that we 
	   avoid stack overflow here. */
        if (single_use) {
          if (!*single_use)
            single_use = NULL;
        }
	n = do_optimize_info_lookup(info, pos, j, NULL, single_use, NULL, 0, context, potential_size);

	if (!n) {
	  /* Return shifted reference to other local: */
	  delta += scheme_optimize_info_get_shift(info, pos);
	  n = scheme_make_local(scheme_local_type, pos + delta, 0);
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

Scheme_Object *scheme_optimize_info_lookup(Optimize_Info *info, int pos, int *closure_offset, int *single_use, 
                                           int once_used_ok, int context, int *potential_size)
{
  return do_optimize_info_lookup(info, pos, 0, closure_offset, single_use, NULL, once_used_ok, context, potential_size);
}

int scheme_optimize_info_is_ready(Optimize_Info *info, int pos)
{
  int closure_offset, single_use, ready = 1;
  
  do_optimize_info_lookup(info, pos, 0, &closure_offset, &single_use, &ready, 0, 0, NULL);

  return ready;
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
  naya->context = info->context;
  naya->vclock = info->vclock;
  naya->use_psize = info->use_psize;

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
    scheme_signal_error("error looking for local-variable offset");

  return delta;
}

void scheme_optimize_info_done(Optimize_Info *info)
{
  info->next->size += info->size;
  info->next->psize += info->psize;
  info->next->vclock = info->vclock;
  if (info->has_nonleaf)
    info->next->has_nonleaf = 1;
}

/*========================================================================*/
/*                    compile-time env for resolve                        */
/*========================================================================*/

/* See eval.c for information about the compilation phases. */

Resolve_Prefix *scheme_resolve_prefix(int phase, Comp_Prefix *cp, int simplify)
{
  Resolve_Prefix *rp;
  Scheme_Object **tls, **stxes, *simplify_cache, *m;
  Scheme_Hash_Table *ht;
  int i;

  rp = MALLOC_ONE_TAGGED(Resolve_Prefix);
  rp->so.type = scheme_resolve_prefix_type;
  rp->num_toplevels = cp->num_toplevels;
  rp->num_stxes = cp->num_stxes;
  rp->uses_unsafe = cp->uses_unsafe;
  
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
        m = ht->keys[i];
        if (SAME_TYPE(SCHEME_TYPE(m), scheme_module_variable_type)) {
          if (SCHEME_FALSEP(((Scheme_Modidx *)((Module_Variable *)m)->modidx)->base)
              && SCHEME_FALSEP(((Scheme_Modidx *)((Module_Variable *)m)->modidx)->path)) {
            /* Reduce self-referece to just a symbol: */
            m = ((Module_Variable *)m)->sym;
          }
        }
	tls[SCHEME_TOPLEVEL_POS(ht->vals[i])] = m;
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
              int boxed = 0, flonumed = 0, flags = 0;

              if (boxmap) {
                int byte = boxmap[(2 * i) / BITS_PER_MZSHORT];
                if (byte & ((mzshort)1 << ((2 * i) & (BITS_PER_MZSHORT - 1))))
                  boxed = 1;
                if (byte & ((mzshort)2 << ((2 * i) & (BITS_PER_MZSHORT - 1)))) {
                  flonumed = 1;
                  flags = SCHEME_LOCAL_FLONUM;
                }
              }
              
              loc = scheme_make_local(scheme_local_type,
                                      posmap[i] + offset + shifted,
                                      flags);
              
              if (boxed)
                loc = scheme_box(loc);
              else if (flonumed)
                loc = scheme_make_vector(1, loc);
              
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

Scheme_Object *scheme_resolve_toplevel(Resolve_Info *info, Scheme_Object *expr, int keep_ready)
{
  int skip;

  skip = scheme_resolve_toplevel_pos(info);

  return make_toplevel(skip + SCHEME_TOPLEVEL_DEPTH(expr), /* depth is 0 (normal) or 1 (exp-time) */
		       SCHEME_TOPLEVEL_POS(expr),
		       1,
		       SCHEME_TOPLEVEL_FLAGS(expr) & (SCHEME_TOPLEVEL_CONST
                                                      | (keep_ready 
                                                         ? SCHEME_TOPLEVEL_READY
                                                         : 0)));
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
  if (genv->rename_set)
    obj = scheme_add_rename(obj, genv->rename_set);

  return obj;
}

static Scheme_Object *
namespace_module_identifier(int argc, Scheme_Object *argv[])
{
  Scheme_Env *genv;
  Scheme_Object *phase;

  if (argc > 0) {
    if (SCHEME_NAMESPACEP(argv[0])) {
      genv = (Scheme_Env *)argv[0];
      phase = scheme_make_integer(genv->phase);
    } else if (SCHEME_FALSEP(argv[0])) {
      phase = scheme_false;
    } else if (SCHEME_INTP(argv[0]) || SCHEME_BIGNUMP(argv[0])) {
      phase = argv[0];
    } else {
      scheme_wrong_type("namespace-module-identifier", "namespace, #f, or exact integer", 0, argc, argv);
      return NULL;
    }
  } else {
    genv = scheme_get_env(NULL);
    phase = scheme_make_integer(genv->phase);
  }

  return scheme_datum_to_syntax(scheme_intern_symbol("module"), scheme_false, 
                                scheme_sys_wraps_phase(phase), 0, 0);
}

static Scheme_Object *
namespace_base_phase(int argc, Scheme_Object *argv[])
{
  Scheme_Env *genv;

  if ((argc > 0) && !SCHEME_NAMESPACEP(argv[0]))
    scheme_wrong_type("namespace-base-phase", "namespace", 0, argc, argv);

  if (argc)
    genv = (Scheme_Env *)argv[0];
  else
    genv = scheme_get_env(NULL);

  return scheme_make_integer(genv->phase);
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

    scheme_prepare_env_renames(genv, mzMOD_RENAME_TOPLEVEL);
    scheme_prepare_compile_env(genv);

    id = scheme_make_renamed_stx(argv[0], genv->rename_set);

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
    scheme_set_global_bucket("namespace-undefine-variable!", 
                             bucket,
                             NULL,
                             0);
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

  if (env->rename_set)
    scheme_list_module_rename(env->rename_set, mapped, env->export_registry);

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

static Scheme_Object *do_variable_namespace(const char *who, int tl, int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;
  Scheme_Env *env;
  int ph;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_global_ref_type))
    env = NULL;
  else {
    v = SCHEME_PTR_VAL(argv[0]);
    env = ((Scheme_Bucket_With_Home *)v)->home;
  }

  if (!env)
    scheme_wrong_type(who, 
                      "variable-reference", 
                      0, argc, argv);

  ph = env->phase;
  if (tl == 2) {
    return scheme_make_integer(ph);
  } else if (tl) {
    /* return env directly; need to set up  */
    if (!env->phase)
      scheme_prep_namespace_rename(env);
  } else {
    /* new namespace: */
    Scheme_Env *new_env;
    new_env = make_env(env, 0);
    new_env->phase = env->phase;
    env = new_env;
  }

  return (Scheme_Object *)env;
}

static Scheme_Object *variable_namespace(int argc, Scheme_Object *argv[])
{
  return do_variable_namespace("variable-reference->empty-namespace", 0, argc, argv);
}

static Scheme_Object *variable_top_level_namespace(int argc, Scheme_Object *argv[])
{
  return do_variable_namespace("variable-reference->namespace", 1, argc, argv);
}

static Scheme_Object *variable_phase(int argc, Scheme_Object *argv[])
{
  return do_variable_namespace("variable-reference->phase", 2, argc, argv);
}

static Scheme_Object *variable_p(int argc, Scheme_Object *argv[])
{
  Scheme_Env *env;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_global_ref_type))
    env = NULL;
  else
    env = ((Scheme_Bucket_With_Home *)SCHEME_PTR_VAL(argv[0]))->home;

  return env ? scheme_true : scheme_false;
}

static Scheme_Object *variable_module_path(int argc, Scheme_Object *argv[])
{
  Scheme_Env *env;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_global_ref_type))
    env = NULL;
  else
    env = ((Scheme_Bucket_With_Home *)SCHEME_PTR_VAL(argv[0]))->home;

  if (!env)
    scheme_wrong_type("variable-reference->resolved-module-path", "variable-reference", 0, argc, argv);

  if (env->module)
    return env->module->modname;
  else
    return scheme_false;
}

static Scheme_Object *variable_module_source(int argc, Scheme_Object *argv[])
{
  Scheme_Env *env;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_global_ref_type))
    env = NULL;
  else
    env = ((Scheme_Bucket_With_Home *)SCHEME_PTR_VAL(argv[0]))->home;

  if (!env)
    scheme_wrong_type("variable-reference->module-source", "variable-reference", 0, argc, argv);

  if (env->module)
    return env->module->modsrc;
  else
    return scheme_false;
}

static Scheme_Object *
now_transforming(int argc, Scheme_Object *argv[])
{
  return (scheme_current_thread->current_local_env
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *
do_local_exp_time_value(const char *name, int argc, Scheme_Object *argv[], int recur)
{
  Scheme_Object *v, *sym, *a[2];
  Scheme_Env *menv;
  Scheme_Comp_Env *env;
  int renamed = 0;

  env = scheme_current_thread->current_local_env;
  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "%s: not currently transforming",
                     name);

  sym = argv[0];

  if (!(SCHEME_STXP(sym) && SCHEME_SYMBOLP(SCHEME_STX_VAL(sym))))
    scheme_wrong_type(name, "syntax identifier", 0, argc, argv);

  if (argc > 1) {
    scheme_check_proc_arity2(name, 0, 1, argc, argv, 1);
    if ((argc > 2)
        && SCHEME_TRUEP(argv[2])) { 
      Scheme_Comp_Env *stx_env;
      if (!SAME_TYPE(scheme_intdef_context_type, SCHEME_TYPE(argv[2])))
	scheme_wrong_type(name, "internal-definition context or #f", 2, argc, argv);
      stx_env = (Scheme_Comp_Env *)((void **)SCHEME_PTR1_VAL(argv[2]))[0];
      if (!scheme_is_sub_env(stx_env, env)) {
	scheme_raise_exn(MZEXN_FAIL_CONTRACT, "%s: transforming context does "
			 "not match given internal-definition context",
                         name);
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
	scheme_arg_mismatch(name,
			    (renamed 
			     ? "not defined as syntax (after renaming): "
			     : "not defined as syntax: "),
			    argv[0]);
    }
    
    v = SCHEME_PTR_VAL(v);
    if (scheme_is_rename_transformer(v)) {
      sym = scheme_rename_transformer_id(v);
      sym = scheme_stx_cert(sym, scheme_false, menv, sym, NULL, 1);
      renamed = 1;
      menv = NULL;
      SCHEME_USE_FUEL(1);
      if (!recur) {
        a[0] = v;
        a[1] = sym;
        return scheme_values(2, a);
      }
    } else if (!recur) {
      a[0] = v;
      a[1] = scheme_false;
      return scheme_values(2, a);
    } else
      return v;
  }
}

static Scheme_Object *
local_exp_time_value(int argc, Scheme_Object *argv[])
{
  return do_local_exp_time_value("syntax-local-value", argc, argv, 1);
}

static Scheme_Object *
local_exp_time_value_one(int argc, Scheme_Object *argv[])
{
  return do_local_exp_time_value("syntax-local-value/immediate", argc, argv, 0);
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
	  pr = scheme_make_pair(sym, scheme_null);
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
local_phase_level(int argc, Scheme_Object *argv[])
{
  Scheme_Thread *p = scheme_current_thread;
  int phase;

  phase = (p->current_local_env
           ? p->current_local_env->genv->phase
           : 0);

  return scheme_make_integer(phase);
}

static Scheme_Object *
local_make_intdef_context(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env, *senv;
  Scheme_Object *c, *rib;
  void **d;

  d = MALLOC_N(void*, 3);

  env = scheme_current_thread->current_local_env;
  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, "syntax-local-make-definition-context: not currently transforming");
  
  if (argc && SCHEME_TRUEP(argv[0])) {
    if (!SAME_TYPE(scheme_intdef_context_type, SCHEME_TYPE(argv[0])))
      scheme_wrong_type("syntax-local-bind-syntaxes", "internal-definition context or #f", 0, argc, argv);
    senv = (Scheme_Comp_Env *)((void **)SCHEME_PTR1_VAL(argv[0]))[0];
    if (!scheme_is_sub_env(senv, env)) {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT, "syntax-local-make-definition-context: transforming context does "
                       "not match given internal-definition context");
    }
    env = senv;
    d[1] = argv[0];
  }
  d[0] = env;

  rib = scheme_make_rename_rib();

  c = scheme_alloc_object();
  c->type = scheme_intdef_context_type;
  SCHEME_PTR1_VAL(c) = d;
  SCHEME_PTR2_VAL(c) = rib;

  return c;
}

static Scheme_Object *
intdef_context_p(int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_intdef_context_type)
          ? scheme_true
          : scheme_false);
}

static Scheme_Object *intdef_context_seal(int argc, Scheme_Object *argv[])
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_intdef_context_type))
    scheme_wrong_type("internal-definition-context-seal", 
                      "internal-definition context", 0, argc, argv);
  
  scheme_stx_seal_rib(SCHEME_PTR2_VAL(argv[0]));
  return scheme_void;
}

static Scheme_Object *
id_intdef_remove(int argc, Scheme_Object *argv[])
{
  Scheme_Object *l, *res, *skips;

  if (!SCHEME_STXP(argv[0]) || !SCHEME_SYMBOLP(SCHEME_STX_VAL(argv[0])))
    scheme_wrong_type("identifier-remove-from-definition-context", 
                      "syntax identifier", 0, argc, argv);
  
  l = argv[1];
  if (!SAME_TYPE(SCHEME_TYPE(l), scheme_intdef_context_type)) {
    while (SCHEME_PAIRP(l)) {
      if (!SAME_TYPE(SCHEME_TYPE(SCHEME_CAR(l)), scheme_intdef_context_type))
        break;
      l = SCHEME_CDR(l);
    }
    if (!SCHEME_NULLP(l))
      scheme_wrong_type("identifier-remove-from-definition-context", 
                        "internal-definition context or list of internal-definition contexts", 
                        1, argc, argv);
  }

  l = argv[1];
  if (SAME_TYPE(SCHEME_TYPE(l), scheme_intdef_context_type))
    l = scheme_make_pair(l, scheme_null);

  res = argv[0];
  skips = scheme_null;

  while (SCHEME_PAIRP(l)) {
    res = scheme_stx_id_remove_rib(res, SCHEME_PTR2_VAL(SCHEME_CAR(l)));
    skips = scheme_make_pair(SCHEME_PTR2_VAL(SCHEME_CAR(l)), skips);
    l = SCHEME_CDR(l);
  }

  if (scheme_stx_ribs_matter(res, skips)) {
    /* Removing ribs leaves the binding for this identifier in limbo, because
       the rib that binds it depends on the removed ribs. Invent in inaccessible
       identifier. */
    res = scheme_add_remove_mark(res, scheme_new_mark());
  }
  
  return res;
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
    if (env->genv->rename_set)
      s = scheme_add_rename(s, env->genv->rename_set);
    if (env->genv->post_ex_rename_set)
      s = scheme_add_rename(s, env->genv->post_ex_rename_set);
  }

  return s;
}

static Scheme_Object *
local_get_shadower(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env, *frame;
  Scheme_Object *sym, *esym, *sym_marks = NULL, *orig_sym, *uid = NULL, *env_marks, *prop;

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
     with the same marks as the given identifier, sym. Skip over
     unsealed ribs, though. When we find a match, rename the given
     identifier so that it matches frame. */
  for (frame = env; frame->next != NULL; frame = frame->next) {
    int i;

    for (i = frame->num_bindings; i--; ) {
      if (frame->values[i]) {
	if (SAME_OBJ(SCHEME_STX_VAL(sym), SCHEME_STX_VAL(frame->values[i])))  {
          prop = scheme_stx_property(frame->values[i], unshadowable_symbol, NULL);
          if (SCHEME_FALSEP(prop)) {
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
    }
    if (uid)
      break;

    if (!COMPILE_DATA(frame)->sealed || *COMPILE_DATA(frame)->sealed) {
      for (i = COMPILE_DATA(frame)->num_const; i--; ) {
        if (!(frame->flags & SCHEME_CAPTURE_WITHOUT_RENAME)) {
          if (SAME_OBJ(SCHEME_STX_VAL(sym), 
                       SCHEME_STX_VAL(COMPILE_DATA(frame)->const_names[i]))) {
            esym = COMPILE_DATA(frame)->const_names[i];
            prop = scheme_stx_property(esym, unshadowable_symbol, NULL);
            if (SCHEME_FALSEP(prop)) {
              env_marks = scheme_stx_extract_marks(esym);
              if (scheme_equal(env_marks, sym_marks)) { /* This used to have 1 || --- why? */
                sym = esym;
                if (COMPILE_DATA(frame)->const_uids)
                  uid = COMPILE_DATA(frame)->const_uids[i];
                else
                  uid = frame->uid;
                break;
              }
            }
	  }
	}
      }
    }
    if (uid)
      break;
  }

  if (!uid) {
    /* No lexical shadower, but strip module context, if any */
    sym = scheme_stx_strip_module_context(sym);
    /* Add current module context, if any */
    sym = local_module_introduce(1, &sym);
    return sym;
  }

  {
    Scheme_Object *rn, *result;

    result = scheme_datum_to_syntax(SCHEME_STX_VAL(sym), orig_sym, sym, 0, 0);
    ((Scheme_Stx *)result)->props = ((Scheme_Stx *)orig_sym)->props;
    
    rn = scheme_make_rename(uid, 1);
    scheme_set_rename(rn, 0, result);

    result = scheme_add_rename(result, rn);

    return result;
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
delta_introducer_proc(void *_i_plus_m, int argc, Scheme_Object *argv[])
{
  Scheme_Object *p = (Scheme_Object *)_i_plus_m, *l, *v, *a[1];
  const char *who = "delta introducer attached to a rename transformer";

  v = argv[0];
  if (!SCHEME_STXP(v) || !SCHEME_SYMBOLP(SCHEME_STX_VAL(v))) {
    scheme_wrong_type(who, "identifier", 0, argc, argv);
  }
  
  /* Apply mapping functions: */
  l = SCHEME_CDR(p);
  while (SCHEME_PAIRP(l)) {
    a[0] = v;
    v = _scheme_apply(SCHEME_CAR(l), 1, a);
    l = SCHEME_CDR(l);
  }

  /* Apply delta-introducing functions: */
  l = SCHEME_CAR(p);
  while (SCHEME_PAIRP(l)) {
    a[0] = v;
    v = _scheme_apply(SCHEME_CAR(l), 1, a);
    if (!SCHEME_STXP(v) || !SCHEME_SYMBOLP(SCHEME_STX_VAL(v))) {
      a[0] = v;
      scheme_wrong_type(who, "identifier", -1, -1, a);
    }
    l = SCHEME_CDR(l);
  }

  return v;
}

static Scheme_Object *
local_make_delta_introduce(int argc, Scheme_Object *argv[])
{
  Scheme_Object *sym, *binder, *introducer, *a[2], *v;
  Scheme_Object *introducers = scheme_null, *mappers = scheme_null;
  int renamed = 0;
  Scheme_Comp_Env *env;
  Scheme_Object *certs;

  env = scheme_current_thread->current_local_env;
  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "syntax-local-make-delta-introducer: not currently transforming");

  if (!SCHEME_STXP(argv[0]) || !SCHEME_SYMBOLP(SCHEME_STX_VAL(argv[0])))
    scheme_wrong_type("syntax-local-make-delta-introducer", "syntax identifier", 0, argc, argv);

  sym = argv[0];

  sym = scheme_stx_activate_certs(sym);

  certs = scheme_current_thread->current_local_certs;

  while (1) {
    binder = NULL;

    v = scheme_lookup_binding(sym, env,
			      (SCHEME_NULL_FOR_UNBOUND
			       + SCHEME_RESOLVE_MODIDS
			       + SCHEME_APP_POS + SCHEME_ENV_CONSTANTS_OK
			       + SCHEME_OUT_OF_CONTEXT_OK + SCHEME_ELIM_CONST),
			      certs, 
			      scheme_current_thread->current_local_modidx, 
			      NULL, NULL, &binder);
    
    /* Deref globals */
    if (v && SAME_TYPE(SCHEME_TYPE(v), scheme_variable_type))
      v = (Scheme_Object *)(SCHEME_VAR_BUCKET(v))->val;
    
    if (!v || NOT_SAME_TYPE(SCHEME_TYPE(v), scheme_macro_type)) {
      scheme_arg_mismatch("syntax-local-make-delta-introducer",
                          (renamed 
                           ? "not defined as syntax (after renaming): "
                           : "not defined as syntax: "),
                          argv[0]);
    }

    if (!binder) {
      /* Not a lexical biding. Tell make-syntax-delta-introducer to
         use module-binding information. */
      binder = scheme_false;
    }

    a[0] = sym;
    a[1] = binder;
    introducer = scheme_syntax_make_transfer_intro(2, a);
    introducers = scheme_make_pair(introducer, introducers);
    
    v = SCHEME_PTR_VAL(v);
    if (scheme_is_rename_transformer(v)) {
      certs = scheme_stx_extract_certs(sym, certs);

      sym = scheme_rename_transformer_id(v);
      sym = scheme_stx_activate_certs(sym);

      v = SCHEME_PTR2_VAL(v);
      if (!SCHEME_FALSEP(v))
        mappers = scheme_make_pair(v, mappers);

      renamed = 1;
      SCHEME_USE_FUEL(1);
    } else {
      /* that's the end of the chain */
      mappers = scheme_reverse(mappers);
      return scheme_make_closed_prim_w_arity(delta_introducer_proc, 
                                             scheme_make_pair(introducers, mappers),
                                             "syntax-delta-introducer", 1, 1);
    }
  }
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
local_module_exports(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env;

  env = scheme_current_thread->current_local_env;
  
  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "syntax-local-module-exports: not currently transforming");

  return scheme_module_exported_list(argv[0], env->genv);
}

static Scheme_Object *
local_module_definitions(int argc, Scheme_Object *argv[])
{
  Scheme_Object *a[2];

  if (!scheme_current_thread->current_local_env
      || !scheme_current_thread->current_local_bindings)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "syntax-local-module-defined-identifiers: not currently transforming module provides");
  
  a[0] = SCHEME_CDR(scheme_current_thread->current_local_bindings);
  a[1] = SCHEME_CDR(a[0]);
  a[0] = SCHEME_CAR(a[0]);

  return scheme_values(2, a);
}

static Scheme_Object *
local_module_imports(int argc, Scheme_Object *argv[])
{
  if (!scheme_current_thread->current_local_env
      || !scheme_current_thread->current_local_bindings)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "syntax-local-module-required-identifiers: not currently transforming module provides");

  if (SCHEME_TRUEP(argv[0]) && !scheme_is_module_path(argv[0]))
    scheme_wrong_type("syntax-local-module-required-identifiers", "module-path or #f", 0, argc, argv);
  
  if (!SCHEME_FALSEP(argv[1]) 
      && !SAME_OBJ(scheme_true, argv[1])
      && !SCHEME_INTP(argv[1])
      && !SCHEME_BIGNUMP(argv[1]))
    scheme_wrong_type("syntax-local-module-required-identifiers", "exact integer, #f, or #t", 1, argc, argv);
  
  return scheme_module_imported_list(scheme_current_thread->current_local_env->genv,
                                     scheme_current_thread->current_local_bindings,
                                     argv[0],
                                     argv[1]);
}

static Scheme_Object *
local_module_expanding_provides(int argc, Scheme_Object *argv[])
{
  if (scheme_current_thread->current_local_env
      && scheme_current_thread->current_local_bindings)
    return scheme_true;
  else
    return scheme_false;
}

static Scheme_Object *
do_local_lift_expr(const char *who, int stx_pos, int argc, Scheme_Object *argv[])
{
  Scheme_Env *menv;
  Scheme_Comp_Env *env, *orig_env;
  Scheme_Object *id, *ids, *rev_ids, *local_mark, *expr, *data, *vec, *id_sym;
  Scheme_Lift_Capture_Proc cp;  
  Scheme_Object *orig_expr;
  int count;
  char buf[24];

  if (stx_pos) {
    if (SCHEME_INTP(argv[0])) {
      count = SCHEME_INT_VAL(argv[0]);
    } else if (SCHEME_BIGNUMP(argv[0])) {
      if (SCHEME_BIGPOS(argv[0]))
        scheme_raise_out_of_memory(NULL, NULL);
      count = -1;
    } else
      count = -1;

    if (count < 0)
      scheme_wrong_type(who, "exact nonnegative integer", 0, argc, argv);
  } else
    count = 1;

  expr = argv[stx_pos];
  if (!SCHEME_STXP(expr))
    scheme_wrong_type(who, "syntax", stx_pos, argc, argv);

  env = orig_env = scheme_current_thread->current_local_env;
  local_mark = scheme_current_thread->current_local_mark;

  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "%s: not currently transforming",
                     who);

  while (env && !COMPILE_DATA(env)->lifts) {
    env = env->next;
  }

  if (env)
    if (SCHEME_FALSEP(SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[0]))
      env = NULL;

  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "syntax-local-lift-expression: no lift target");
  
  expr = scheme_add_remove_mark(expr, local_mark);

  /* We don't really need a new symbol each time, since the mark
     will generate new bindings. But lots of things work better or faster
     when different bindings have different symbols. Use env->genv->id_counter
     to help keep name generation deterministic within a module. */
  rev_ids = scheme_null;
  while (count--) {
    sprintf(buf, "lifted.%d", env->genv->id_counter++);
    id_sym = scheme_intern_exact_parallel_symbol(buf, strlen(buf));

    id = scheme_datum_to_syntax(id_sym, scheme_false, scheme_false, 0, 0);
    id = scheme_add_remove_mark(id, scheme_new_mark());

    rev_ids = scheme_make_pair(id, rev_ids);
  }
  ids = scheme_reverse(rev_ids);

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

  expr = cp(data, &ids, expr, orig_env);

  expr = scheme_make_pair(expr, SCHEME_VEC_ELS(vec)[0]);
  SCHEME_VEC_ELS(vec)[0] = expr;

  SCHEME_EXPAND_OBSERVE_LOCAL_LIFT(scheme_get_expand_observe(), ids, orig_expr);

  rev_ids = scheme_null;
  for (; !SCHEME_NULLP(ids); ids = SCHEME_CDR(ids)) {
    id = SCHEME_CAR(ids);
    id = scheme_add_remove_mark(id, local_mark);
    rev_ids = scheme_make_pair(id, rev_ids);
  }
  ids = scheme_reverse(rev_ids);

  return ids;
}

static Scheme_Object *
local_lift_expr(int argc, Scheme_Object *argv[])
{
  Scheme_Object *ids;
  ids = do_local_lift_expr("syntax-local-lift-expression", 0, argc, argv);
  return SCHEME_CAR(ids);
}

static Scheme_Object *
local_lift_exprs(int argc, Scheme_Object *argv[])
{
  return do_local_lift_expr("syntax-local-lift-values-expression", 1, argc, argv);
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

static Scheme_Object *local_lift_require(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env;
  Scheme_Object *local_mark, *mark, *data, *pr, *form;
  Scheme_Object *orig_form, *req_form;
  long phase;

  if (!SCHEME_STXP(argv[1]))
    scheme_wrong_type("syntax-local-lift-require", "syntax", 1, argc, argv);

  env = scheme_current_thread->current_local_env;
  local_mark = scheme_current_thread->current_local_mark;
  phase = env->genv->phase;

  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "syntax-local-lift-require: not currently transforming");

  data = NULL;

  while (env) {
    if (COMPILE_DATA(env)->lifts
        && SCHEME_TRUEP(SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[5])) {
      data = SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[5];
      if (SCHEME_RPAIRP(data)
          && !SCHEME_CAR(data)) {
        env = (Scheme_Comp_Env *)SCHEME_CDR(data);
      } else
        break;
    } else
      env = env->next;
  }

  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "syntax-local-lift-requires: could not find target context");

  
  mark = scheme_new_mark();

  if (SCHEME_RPAIRP(data))
    form = scheme_parse_lifted_require(argv[0], phase, mark, SCHEME_CAR(data));
  else
    form = scheme_toplevel_require_for_expand(argv[0], phase, env, mark);
  
  pr = scheme_make_pair(form, SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[6]);
  SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[6] = pr;

  req_form = form;
  orig_form = argv[1];

  form = orig_form;
  form = scheme_add_remove_mark(form, local_mark);
  form = scheme_add_remove_mark(form, mark);
  form = scheme_add_remove_mark(form, local_mark);

  SCHEME_EXPAND_OBSERVE_LIFT_REQUIRE(scheme_get_expand_observe(), req_form, orig_form, form);

  return form;
}

static Scheme_Object *local_lift_provide(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env;
  Scheme_Object *pr, *form, *local_mark;

  form = argv[0];
  if (!SCHEME_STXP(form))
    scheme_wrong_type("syntax-local-lift-provide", "syntax", 1, argc, argv);

  env = scheme_current_thread->current_local_env;
  local_mark = scheme_current_thread->current_local_mark;

  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "syntax-local-lift-provide: not currently transforming");

  while (env) {
    if (COMPILE_DATA(env)->lifts
        && SCHEME_TRUEP(SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[7])) {
      break;
    } else
      env = env->next;
  }

  if (!env)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT, 
		     "syntax-local-lift-provide: not expanding in a module run-time body");
  
  form = scheme_add_remove_mark(form, local_mark);
  form = scheme_datum_to_syntax(scheme_make_pair(scheme_datum_to_syntax(scheme_intern_symbol("#%provide"), 
                                                                        scheme_false, scheme_sys_wraps(env), 
                                                                        0, 0),
                                                 scheme_make_pair(form, scheme_null)),
                                form, scheme_false, 0, 0);

  SCHEME_EXPAND_OBSERVE_LIFT_PROVIDE(scheme_get_expand_observe(), form);

  pr = scheme_make_pair(form, SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[7]);
  SCHEME_VEC_ELS(COMPILE_DATA(env)->lifts)[7] = pr;

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
  return (scheme_is_set_transformer(argv[0])
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *
set_transformer_proc(int argc, Scheme_Object *argv[])
{
  if (!scheme_is_set_transformer(argv[0]))
    scheme_wrong_type("set!-transformer-procedure", "set!-transformer", 1, argc, argv);

  return scheme_set_transformer_proc(argv[0]);
}

static Scheme_Object *
make_rename_transformer(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;

  if (!SCHEME_STXP(argv[0]) || !SCHEME_SYMBOLP(SCHEME_STX_VAL(argv[0])))
    scheme_wrong_type("make-rename-transformer", "syntax identifier", 0, argc, argv);

  if (argc > 1)
    scheme_check_proc_arity("make-rename-transformer", 1, 1, argc, argv);
  
  v = scheme_alloc_object();
  v->type = scheme_id_macro_type;
  SCHEME_PTR1_VAL(v) = argv[0];
  SCHEME_PTR2_VAL(v) = ((argc > 1) ? argv[1] : scheme_false);

  return v;
}

static Scheme_Object *
rename_transformer_target(int argc, Scheme_Object *argv[])
{
  if (!scheme_is_rename_transformer(argv[0]))
    scheme_wrong_type("rename-transformer-target", "rename transformer", 0, argc, argv);

  return scheme_rename_transformer_id(argv[0]);
}

static Scheme_Object *
rename_transformer_p(int argc, Scheme_Object *argv[])
{
  return (scheme_is_rename_transformer(argv[0])
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
  /* #%kernel references are handled in print.c, instead */
{
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
}

static Scheme_Object *read_variable(Scheme_Object *obj)
  /* #%kernel references are handled in read.c, instead */
{
  Scheme_Env *env;

  env = scheme_get_env(NULL);

  if (!SCHEME_SYMBOLP(obj)) return NULL;

  return (Scheme_Object *)scheme_global_bucket(obj, env);
}

static Scheme_Object *write_module_variable(Scheme_Object *obj)
{
  scheme_signal_error("module variables should have been handled in print.c");
  return NULL;
}

static Scheme_Object *read_module_variable(Scheme_Object *obj)
{
  scheme_signal_error("module variables should have been handled in read.c");
  return NULL;
}

static Scheme_Object *write_local(Scheme_Object *obj)
{
  return scheme_make_integer(SCHEME_LOCAL_POS(obj));
}

static Scheme_Object *do_read_local(Scheme_Type t, Scheme_Object *obj)
{
  int n, flags;

  if (SCHEME_PAIRP(obj)) {
    flags = SCHEME_INT_VAL(SCHEME_CAR(obj));
    obj = SCHEME_CDR(obj);
  } else
    flags = 0;

  n = SCHEME_INT_VAL(obj);

  return scheme_make_local(t, n, flags);
}

static Scheme_Object *read_local(Scheme_Object *obj)
{
  return do_read_local(scheme_local_type, obj);
}

static Scheme_Object *read_local_unbox(Scheme_Object *obj)
{
  return do_read_local(scheme_local_unbox_type, obj);
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
      if (SCHEME_INTP(rp->stxes[i])) {
        /* Need to foce this object, so we can write it.
           This should only happen if we're writing back 
           code loaded from bytecode. */
        scheme_load_delayed_syntax(rp, i);
      }

      ds = scheme_alloc_small_object();
      ds->type = scheme_delay_syntax_type;
      SCHEME_PTR_VAL(ds) = rp->stxes[i];
    } else
      ds = scheme_false;
    SCHEME_VEC_ELS(sv)[i] = ds;
  }

  tv = scheme_make_pair(scheme_make_integer(rp->num_lifts), 
                        scheme_make_pair(tv, sv));
  
  if (rp->uses_unsafe)
    tv = scheme_make_pair(scheme_true, tv);

  return tv;
}

static Scheme_Object *read_resolve_prefix(Scheme_Object *obj, Scheme_Object *insp)
{
  Resolve_Prefix *rp;
  Scheme_Object *tv, *sv, **a, *stx;
  int i, uses_unsafe = 0;

  if (!SCHEME_PAIRP(obj)) return NULL;

  if (!SCHEME_INTP(SCHEME_CAR(obj))) {
    uses_unsafe = 1;
    obj = SCHEME_CDR(obj);
  }

  if (!SCHEME_PAIRP(obj)) return NULL;

  i = SCHEME_INT_VAL(SCHEME_CAR(obj));
  if (i < 0) return NULL;

  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return NULL;

  tv = SCHEME_CAR(obj);
  sv = SCHEME_CDR(obj);

  if (!SCHEME_VECTORP(tv)) return NULL;
  if (!SCHEME_VECTORP(sv)) return NULL;

  rp = MALLOC_ONE_TAGGED(Resolve_Prefix);
  rp->so.type = scheme_resolve_prefix_type;
  rp->num_toplevels = SCHEME_VEC_SIZE(tv);
  rp->num_stxes = SCHEME_VEC_SIZE(sv);
  rp->num_lifts = i;
  if (uses_unsafe)
    rp->uses_unsafe = insp;

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
      struct Scheme_Load_Delay *d;
      Scheme_Object *pr;
      d = (struct Scheme_Load_Delay *)SCHEME_CDR(stx);
      stx = SCHEME_CAR(stx);
      pr = rp->delay_info_rpair;
      if (!pr) {
        pr = scheme_make_raw_pair(scheme_make_integer(0), (Scheme_Object *)d);
        rp->delay_info_rpair = pr;
      }
      SCHEME_CAR(pr) = scheme_make_integer(SCHEME_INT_VAL(SCHEME_CAR(pr)) + 1);
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
  GC_REG_TRAV(scheme_rt_sfs_info, mark_sfs_info);
  GC_REG_TRAV(scheme_once_used_type, mark_once_used);
}

END_XFORM_SKIP;

#endif
