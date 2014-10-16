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
#ifdef MZ_USE_FUTURES
# include "future.h"
#endif

#define GLOBAL_TABLE_SIZE 500

/* #define TIME_STARTUP_PROCESS */

/* global flags */
SHARED_OK int scheme_allow_set_undefined;
void scheme_set_allow_set_undefined(int v) { scheme_allow_set_undefined =  v; }
int scheme_get_allow_set_undefined() { return scheme_allow_set_undefined; }
THREAD_LOCAL_DECL(int scheme_starting_up);

/* globals READ-ONLY SHARED */
Scheme_Object *scheme_varref_const_p_proc;
READ_ONLY static Scheme_Object *kernel_symbol;
READ_ONLY static Scheme_Env    *kernel_env;
READ_ONLY static Scheme_Env    *unsafe_env;
READ_ONLY static Scheme_Env    *flfxnum_env;
READ_ONLY static Scheme_Env    *extfl_env;
READ_ONLY static Scheme_Env    *futures_env;

THREAD_LOCAL_DECL(static int intdef_counter);

static int builtin_ref_counter;
static int builtin_unsafe_start;

THREAD_LOCAL_DECL(static Scheme_Bucket_Table *literal_string_table);
THREAD_LOCAL_DECL(static Scheme_Bucket_Table *literal_number_table);

/* local functions */
static void make_kernel_env(void);

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
static Scheme_Object *variable_modidx(int, Scheme_Object *[]);
static Scheme_Object *variable_module_path(int, Scheme_Object *[]);
static Scheme_Object *variable_module_source(int, Scheme_Object *[]);
static Scheme_Object *variable_namespace(int, Scheme_Object *[]);
static Scheme_Object *variable_top_level_namespace(int, Scheme_Object *[]);
static Scheme_Object *variable_phase(int, Scheme_Object *[]);
static Scheme_Object *variable_base_phase(int, Scheme_Object *[]);
static Scheme_Object *variable_inspector(int, Scheme_Object *[]);
static Scheme_Object *variable_const_p(int, Scheme_Object *[]);
static Scheme_Object *now_transforming(int argc, Scheme_Object *argv[]);
static Scheme_Object *now_transforming_module(int argc, Scheme_Object *argv[]);
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
static Scheme_Object *local_module_exports(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_module_definitions(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_submodules(int argc, Scheme_Object *argv[]);
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

static void skip_certain_things(Scheme_Object *o, Scheme_Close_Custodian_Client *f, void *data);

Scheme_Env *scheme_engine_instance_init();
static Scheme_Env *place_instance_init(void *stack_base, int initial_main_os_thread);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

typedef Scheme_Object *(*Lazy_Macro_Fun)(Scheme_Object *, int);


/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/

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
  scheme_init_exn_config();

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

Scheme_Env *scheme_engine_instance_init() 
/* READ-ONLY GLOBAL structures, ONE-TIME initialization */
{
  Scheme_Env *env;
  void *stack_base;
  stack_base = (void *) scheme_get_current_os_thread_stack_base();

  os_platform_init();

#ifdef TIME_STARTUP_PROCESS
  printf("#if 0\nengine_instance_init @ %" PRIdPTR "\n", scheme_get_process_milliseconds());
#endif

#if defined(MZ_LONG_DOUBLE_API_IS_EXTERNAL) || defined(LONG_DOUBLE_STRING_OP_API_IS_EXTERNAL)
  scheme_load_long_double_dll();
#endif

  scheme_starting_up = 1;

  scheme_init_finalization();

  scheme_init_portable_case();
  scheme_init_compenv();
  scheme_init_letrec_check();
  scheme_init_optimize();
  scheme_init_resolve();
  scheme_init_sfs();
  scheme_init_validate();

  scheme_init_process_globals();

  scheme_init_true_false();

#ifdef MZ_PRECISE_GC
  /* scheme_register_traversers(); --- already done in scheme_set_stack_base() */
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
  make_kernel_env();

  scheme_init_logging_once();

  scheme_init_compenv_symbol();

#if defined(MZ_PLACES_WAITPID)
  scheme_places_start_child_signal_handler();
#endif

#if defined(MZ_PRECISE_GC) && defined(MZ_USE_PLACES)
  GC_switch_out_master_gc();

  scheme_spawn_master_place();
#endif

  env = place_instance_init(stack_base, 1);

#if defined(MZ_PRECISE_GC) && defined(MZ_USE_PLACES)
  {
    void *signal_handle;
    REGISTER_SO(place_object);
    place_object = (Scheme_Place_Object*) scheme_make_place_object();
    signal_handle = scheme_get_signal_handle();
    GC_set_put_external_event_fd(signal_handle);
    place_object->signal_handle = signal_handle;
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
  scheme_init_unsafe_fun(unsafe_env);

  scheme_init_extfl_unsafe_number(unsafe_env);
  scheme_init_extfl_unsafe_numarith(unsafe_env);
  scheme_init_extfl_unsafe_numcomp(unsafe_env);

  scheme_finish_primitive_module(unsafe_env);
  pt = unsafe_env->module->me->rt;
  scheme_populate_pt_ht(pt);
  scheme_protect_primitive_provide(unsafe_env, NULL);
  unsafe_env->attached = 1;

#if USE_COMPILED_STARTUP
  if (builtin_ref_counter != (EXPECTED_PRIM_COUNT + EXPECTED_FLFXNUM_COUNT
                              + EXPECTED_EXTFL_COUNT + EXPECTED_FUTURES_COUNT
                              + EXPECTED_UNSAFE_COUNT)) {
    printf("Unsafe count %d doesn't match expected count %d\n",
	   builtin_ref_counter - EXPECTED_PRIM_COUNT
           - EXPECTED_FLFXNUM_COUNT - EXPECTED_EXTFL_COUNT 
           - EXPECTED_FUTURES_COUNT, EXPECTED_UNSAFE_COUNT);
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
  flfxnum_env->attached = 1;

#if USE_COMPILED_STARTUP
  if (builtin_ref_counter != (EXPECTED_PRIM_COUNT + EXPECTED_FLFXNUM_COUNT)) {
    printf("Flfxnum count %d doesn't match expected count %d\n",
	   builtin_ref_counter - EXPECTED_PRIM_COUNT, 
           EXPECTED_FLFXNUM_COUNT);
    abort();
  }
#endif
}

static void init_extfl(Scheme_Env *env)
{
  Scheme_Module_Phase_Exports *pt;
  REGISTER_SO(extfl_env);

  extfl_env = scheme_primitive_module(scheme_intern_symbol("#%extfl"), env);

  scheme_init_extfl_number(extfl_env);
  scheme_init_extfl_numarith(extfl_env);
  scheme_init_extfl_numcomp(extfl_env);
  scheme_init_extfl_numstr(extfl_env);

  scheme_finish_primitive_module(extfl_env);
  pt = extfl_env->module->me->rt;
  scheme_populate_pt_ht(pt);
  scheme_protect_primitive_provide(extfl_env, NULL);
  extfl_env->attached = 1;

#if USE_COMPILED_STARTUP
  if (builtin_ref_counter != (EXPECTED_PRIM_COUNT + EXPECTED_FLFXNUM_COUNT
                              + EXPECTED_EXTFL_COUNT)) {
    printf("extfl count %d doesn't match expected count %d\n",
	   builtin_ref_counter - EXPECTED_PRIM_COUNT - EXPECTED_FLFXNUM_COUNT,
           EXPECTED_EXTFL_COUNT);
    abort();
  }
#endif
}

static void init_futures(Scheme_Env *env)
{
  Scheme_Module_Phase_Exports *pt;
  REGISTER_SO(futures_env);

  futures_env = scheme_primitive_module(scheme_intern_symbol("#%futures"), env);

  scheme_init_futures(futures_env);

  scheme_finish_primitive_module(futures_env);
  pt = futures_env->module->me->rt;
  scheme_populate_pt_ht(pt);
  scheme_protect_primitive_provide(futures_env, NULL);
  futures_env->attached = 1;

#if USE_COMPILED_STARTUP
  if (builtin_ref_counter != (EXPECTED_PRIM_COUNT + EXPECTED_FLFXNUM_COUNT
                              + EXPECTED_EXTFL_COUNT + EXPECTED_FUTURES_COUNT)) {
    printf("Futures count %d doesn't match expected count %d\n",
	   builtin_ref_counter - EXPECTED_PRIM_COUNT - EXPECTED_FLFXNUM_COUNT
           - EXPECTED_EXTFL_COUNT, 
           EXPECTED_FUTURES_COUNT);
    abort();
  }
#endif
}

static void init_foreign(Scheme_Env *env)
{
  Scheme_Env *ffi_env;

  scheme_init_foreign(env);

  ffi_env = scheme_get_foreign_env();
  scheme_populate_pt_ht(ffi_env->module->me->rt);
  ffi_env->attached = 1;

#if USE_COMPILED_STARTUP
  if (builtin_ref_counter != (EXPECTED_PRIM_COUNT + EXPECTED_FLFXNUM_COUNT
                              + EXPECTED_EXTFL_COUNT + EXPECTED_FUTURES_COUNT
                              + EXPECTED_UNSAFE_COUNT + EXPECTED_FOREIGN_COUNT)) {
    printf("Foreign count %d doesn't match expected count %d\n",
	   builtin_ref_counter - EXPECTED_PRIM_COUNT - EXPECTED_FLFXNUM_COUNT
           - EXPECTED_EXTFL_COUNT - EXPECTED_FUTURES_COUNT
           - EXPECTED_UNSAFE_COUNT,
           EXPECTED_FOREIGN_COUNT);
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

Scheme_Env *scheme_get_extfl_env() {
  return extfl_env;
}

Scheme_Env *scheme_get_futures_env() {
  return futures_env;
}


static Scheme_Env *place_instance_init(void *stack_base, int initial_main_os_thread) {
  Scheme_Env *env;

#ifdef TIME_STARTUP_PROCESS
  printf("place_init @ %" PRIdPTR "\n", scheme_get_process_milliseconds());
#endif
  scheme_set_current_os_thread_stack_base(stack_base);

#ifndef MZ_PRECISE_GC
  scheme_init_setjumpup();
#endif

  scheme_init_stack_check();
  scheme_init_overflow();

  scheme_init_thread_lwc();

  scheme_init_compenv_places();

#ifdef TIME_STARTUP_PROCESS
  printf("pre-process @ %" PRIdPTR "\n", scheme_get_process_milliseconds());
#endif

  scheme_init_file_places();

  scheme_make_thread(stack_base);

#if defined(MZ_PRECISE_GC) && defined(MZ_USE_PLACES)
  /* each place now has a local symbol table */
  scheme_init_place_local_symbol_table();
#endif

  {
    Scheme_Object *sym;
    sym = scheme_intern_symbol("mzscheme");
    scheme_current_thread->name = sym;
  }

  scheme_init_module_resolver();

#ifdef TIME_STARTUP_PROCESS
  printf("process @ %" PRIdPTR "\n", scheme_get_process_milliseconds());
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
  scheme_init_compile_places();
  scheme_init_regexp_places();
  scheme_init_stx_places(initial_main_os_thread);
  scheme_init_sema_places();
  scheme_init_gmp_places();
  scheme_init_kqueue();
  scheme_alloc_global_fdset();
#ifndef DONT_USE_FOREIGN
  scheme_init_foreign_places();
#endif

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
#if defined(MZ_USE_PLACES) && defined(MZ_USE_JIT)
  scheme_jit_fill_threadlocal_table();
#endif
  scheme_init_futures_per_place();

  REGISTER_SO(literal_string_table);
  REGISTER_SO(literal_number_table);
  literal_string_table = scheme_make_weak_equal_table();
  literal_number_table = scheme_make_weak_eqv_table();

  scheme_starting_up = 1; /* in case it's not set already */

#ifdef TIME_STARTUP_PROCESS
  printf("pre-embedded @ %" PRIdPTR "\n", scheme_get_process_milliseconds());
#endif

  scheme_add_embedded_builtins(env);

  boot_module_resolver();

  scheme_save_initial_module_set(env);

  scheme_starting_up = 0;

  --scheme_current_thread->suspend_break; /* created with breaks suspended */

#ifdef TIME_STARTUP_PROCESS
  printf("done @ %" PRIdPTR "\n#endif\n", scheme_get_process_milliseconds());
#endif

#if defined(MZ_USE_PLACES)
  REGISTER_SO(place_channel_links);
#endif

  return env;
}

#ifdef MZ_USE_PLACES
Scheme_Env *scheme_place_instance_init(void *stack_base, struct NewGC *parent_gc, intptr_t memory_limit) {
  Scheme_Env *env;
# if defined(MZ_PRECISE_GC)
  int *signal_fd;
  GC_construct_child_gc(parent_gc, memory_limit);
# endif
  env = place_instance_init(stack_base, 0);
# if defined(MZ_PRECISE_GC)
  signal_fd = scheme_get_signal_handle();
  GC_set_put_external_event_fd(signal_fd);
# endif
  scheme_set_can_break(1);
  return env; 
}
#endif

static void force_more_closed(Scheme_Object *o, Scheme_Close_Custodian_Client *f, void *data)
{
  /* no need to shut down threads: */
  if (!f || SCHEME_THREADP(o))
    return;

  f(o, data);
}

static void force_more_closed_after(Scheme_Object *o, Scheme_Close_Custodian_Client *f, void *data)
{
  scheme_run_atexit_closers(o, f, data);
  force_more_closed(o, f, data);
}

void scheme_place_instance_destroy(int force)
{
  /* run atexit handlers to flush file ports, and also
     force file-stream ports closed */
  if (force)
    scheme_run_atexit_closers_on_all(force_more_closed);
  else
    scheme_run_atexit_closers_on_all(force_more_closed_after);

  scheme_release_file_descriptor();

  scheme_end_futures_per_place();
#if defined(MZ_USE_PLACES)
  scheme_kill_green_thread_timer();
  scheme_free_place_bi_channels();
#endif
#if defined(MZ_PRECISE_GC) && defined(MZ_USE_PLACES)
  GC_destruct_child_gc();
#endif
  scheme_free_all_code();
  scheme_free_ghbn_data();
  scheme_release_kqueue();
  scheme_release_inotify();
}

static void make_kernel_env(void)
{
  Scheme_Env *env;
#ifdef TIME_STARTUP_PROCESS
  intptr_t startt;
#endif

  env = make_empty_inited_env(GLOBAL_TABLE_SIZE);

  REGISTER_SO(kernel_env);
  kernel_env = env;

  scheme_defining_primitives = 1;
  builtin_ref_counter = 0;

#ifdef TIME_STARTUP_PROCESS
   printf("init @ %" PRIdPTR "\n", scheme_get_process_milliseconds());
# define MZTIMEIT(n, f) (MARK_START_TIME(), f, DONE_TIME(n))
# define MARK_START_TIME() startt = scheme_get_process_milliseconds()
# define DONE_TIME(n) (printf(#n ": %" PRIdPTR "\n", (intptr_t)(scheme_get_process_milliseconds() - startt)))
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
  MZTIMEIT(syntax, scheme_init_compile(env));
  MZTIMEIT(eval, scheme_init_eval(env));
  MZTIMEIT(struct, scheme_init_struct(env));
  MZTIMEIT(error, scheme_init_error(env));
#ifndef NO_SCHEME_EXNS
  MZTIMEIT(exn, scheme_init_exn(env));
#endif
  MZTIMEIT(process, scheme_init_thread(env));
  scheme_init_port_wait();
  scheme_init_inspector();
  scheme_init_logger_wait();
  scheme_init_struct_wait();
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
  MZTIMEIT(futures, scheme_init_futures_once());
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
  GLOBAL_PRIM_W_ARITY("variable-reference->module-path-index", variable_modidx, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("variable-reference->resolved-module-path", variable_module_path, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("variable-reference->module-source", variable_module_source, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("variable-reference->empty-namespace", variable_namespace, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("variable-reference->namespace", variable_top_level_namespace, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("variable-reference->phase", variable_phase, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("variable-reference->module-base-phase", variable_base_phase, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("variable-reference->module-declaration-inspector", variable_inspector, 1, 1, env);

  REGISTER_SO(scheme_varref_const_p_proc);
  scheme_varref_const_p_proc = scheme_make_prim_w_arity(variable_const_p, 
                                                        "variable-reference-constant?", 
                                                        1, 1);
  scheme_add_global_constant("variable-reference-constant?", scheme_varref_const_p_proc, env);

  GLOBAL_PRIM_W_ARITY("syntax-transforming?", now_transforming, 0, 0, env);
  GLOBAL_PRIM_W_ARITY("syntax-transforming-module-expression?", now_transforming_module, 0, 0, env);
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

  GLOBAL_PRIM_W_ARITY("syntax-local-module-exports", local_module_exports, 1, 1, env);
  GLOBAL_PRIM_W_ARITY("syntax-local-module-defined-identifiers", local_module_definitions, 0, 0, env);
  GLOBAL_PRIM_W_ARITY("syntax-local-submodules", local_submodules, 0, 0, env);
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

  DONE_TIME(env);

  scheme_register_network_evts();

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

  init_flfxnum(env);
  init_extfl(env);
  init_futures(env);

  builtin_unsafe_start = builtin_ref_counter;
  init_unsafe(env);
  init_foreign(env);
  
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
    Scheme_Object *rns, *insp;

    insp = env->access_insp;
    if (!insp)
      insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);

    rns = scheme_make_module_rename_set(kind, NULL, insp);
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
  Scheme_Module_Registry *reg;

  env = make_env(NULL, toplevel_size);

  vector = scheme_make_vector(5, scheme_false);
  hash_table = scheme_make_hash_table(SCHEME_hash_ptr);
  SCHEME_VEC_ELS(vector)[0] = (Scheme_Object *)hash_table;
  env->modchain = vector;

  reg = MALLOC_ONE_TAGGED(Scheme_Module_Registry);
  reg->so.type = scheme_module_registry_type;
  env->module_registry = reg;

  hash_table = scheme_make_hash_table(SCHEME_hash_ptr);
  reg->loaded = hash_table;
  hash_table = scheme_make_hash_table(SCHEME_hash_ptr);
  reg->exports = hash_table;

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
    env->module_pre_registry = base->module_pre_registry;
    env->label_env = base->label_env;
  } else {
    env->modchain = NULL;
    env->module_registry = NULL;
    env->module_pre_registry = NULL;
    env->label_env = NULL;
  }

  return env;
}

Scheme_Env *
scheme_new_module_env(Scheme_Env *env, Scheme_Module *m,
                      int new_exp_module_tree, int new_pre_registry)
{
  Scheme_Env *menv;
  Scheme_Module_Registry *reg;

  menv = make_env(env, 7);

  if (new_pre_registry) {
    /* pre_registry is for declarations to be used by submodules */
    reg = MALLOC_ONE_TAGGED(Scheme_Module_Registry);
    reg->so.type = scheme_module_registry_type;
    menv->module_pre_registry = reg;
  }

  menv->module = m;
  menv->instance_env = env;

  if (new_exp_module_tree) {
    /* It would be nice to share the label env with `env`, but we need
       to set `module_pre_registry` in `menv->label_env` and not shared
       it with `env->label_env`: */
    menv->label_env = NULL;
    scheme_prepare_label_env(menv);
    menv->instance_env = menv;
  } else {
    scheme_prepare_label_env(env);
    menv->label_env = env->label_env;
  }

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
    eenv->module_pre_registry = env->module_pre_registry;
    eenv->access_insp = env->access_insp;
    eenv->guard_insp = env->guard_insp;

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
    eenv->instance_env = env->instance_env;

    scheme_prepare_env_renames(env, mzMOD_RENAME_TOPLEVEL);
    eenv->rename_set = env->rename_set;

    if (env->disallow_unbound)
      eenv->disallow_unbound = env->disallow_unbound;
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
    eenv->module_pre_registry = env->module_pre_registry;
    eenv->guard_insp = env->guard_insp;
    eenv->access_insp = env->access_insp;

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
    eenv->instance_env = env->instance_env;

    if (env->disallow_unbound)
      eenv->disallow_unbound = env->disallow_unbound;
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
    lenv->module_pre_registry = env->module_pre_registry;
    lenv->guard_insp = env->guard_insp;
    lenv->access_insp = env->access_insp;

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
    lenv->instance_env = env->instance_env;
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
  menv2->module_pre_registry = ns->module_pre_registry;
  menv2->guard_insp = menv->guard_insp;
  menv2->access_insp = menv->access_insp;

  menv2->instance_env = menv2;

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
    menv2->ran = menv->ran;
  }
  if (menv->mod_phase == 0) {
    char *running;
    int amt;
    running = (char *)scheme_malloc_atomic(menv->module->num_phases);
    menv2->running = running;
    memset(running, 0, menv->module->num_phases);
    amt = (clone_phase - menv->phase) + 1;
    if (amt > 0) {
      if (amt > menv->module->num_phases)
        amt = menv->module->num_phases;
      memcpy(running, menv->running, amt);
    }
  }

  menv2->require_names = menv->require_names;
  menv2->et_require_names = menv->et_require_names;
  menv2->tt_require_names = menv->tt_require_names;
  menv2->dt_require_names = menv->dt_require_names;
  menv2->other_require_names = menv->other_require_names;

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
  intptr_t i;

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
        scheme_set_bucket_home(b, home);
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

Scheme_Object *scheme_get_home_weak_link(Scheme_Env *e)
{
  if (!e->weak_self_link) {
    Scheme_Object *wb;
    if (scheme_starting_up)
      wb = scheme_box((Scheme_Object *)e);
    else
      wb = scheme_make_weak_box((Scheme_Object *)e);
    e->weak_self_link = wb;
  }

  return e->weak_self_link;
}

Scheme_Env *scheme_get_bucket_home(Scheme_Bucket *b)
{
  Scheme_Object *l;

  l = ((Scheme_Bucket_With_Home *)b)->home_link;
  if (l) {
    if (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_STRONG_HOME_LINK)
      return (Scheme_Env *)l;
    else
      return (Scheme_Env *)SCHEME_WEAK_BOX_VAL(l);
  } else
    return NULL;
}

void scheme_set_bucket_home(Scheme_Bucket *b, Scheme_Env *e)
{
  if (!((Scheme_Bucket_With_Home *)b)->home_link) {
    if (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_STRONG_HOME_LINK)
      ((Scheme_Bucket_With_Home *)b)->home_link = (Scheme_Object *)e;
    else {
      Scheme_Object *link;
      link = scheme_get_home_weak_link(e);
      ((Scheme_Bucket_With_Home *)b)->home_link = link;
    }
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
    scheme_set_bucket_home(b, env);
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
  scheme_set_bucket_home(b, env);
    
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
    if (constant && scheme_defining_primitives) {
      ((Scheme_Bucket_With_Flags *)b)->id = builtin_ref_counter++;
      ((Scheme_Bucket_With_Flags *)b)->flags |= (GLOB_HAS_REF_ID | GLOB_IS_CONST | GLOB_STRONG_HOME_LINK);
    }
    scheme_set_bucket_home(b, env);
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

  if (!env) return;

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

Scheme_Object **scheme_make_builtin_references_table(int *_unsafe_start)
{
  Scheme_Bucket_Table *ht;
  Scheme_Object **t;
  Scheme_Bucket **bs;
  Scheme_Env *kenv;
  intptr_t i;
  int j;

  t = MALLOC_N(Scheme_Object *, (builtin_ref_counter + 1));
#ifdef MEMORY_COUNTING_ON
  scheme_misc_count += sizeof(Scheme_Object *) * (builtin_ref_counter + 1);
#endif

  for (j = builtin_ref_counter + 1; j--; ) {
    t[j] = scheme_false;
  }

  for (j = 0; j < 6; j++) {
    if (!j)
      kenv = kernel_env;
    else if (j == 1)
      kenv = unsafe_env;
    else if (j == 2)
      kenv = flfxnum_env;
    else if (j == 3)
      kenv = extfl_env;
    else if (j == 4)
      kenv = futures_env;
    else
      kenv = scheme_get_foreign_env();
    
    ht = kenv->toplevel;
    
    bs = ht->buckets;
    
    for (i = ht->size; i--; ) {
      Scheme_Bucket *b = bs[i];
      if (b && (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_HAS_REF_ID))
        t[((Scheme_Bucket_With_Ref_Id *)b)->id] = (Scheme_Object *)b->val;
    }
  }

  *_unsafe_start = builtin_unsafe_start;

  return t;
}

Scheme_Hash_Table *scheme_map_constants_to_globals(void)
{
  Scheme_Bucket_Table *ht;
  Scheme_Hash_Table*result;
  Scheme_Bucket **bs;
  Scheme_Env *kenv;
  intptr_t i;
  int j;

  result = scheme_make_hash_table(SCHEME_hash_ptr);
      
  for (j = 0; j < 6; j++) {
    if (!j)
      kenv = kernel_env;
    else if (j == 1)
      kenv = unsafe_env;
    else if (j == 2)
      kenv = flfxnum_env;
    else if (j == 3)
      kenv = extfl_env;
    else if (j == 4)
      kenv = futures_env;
    else
      kenv = scheme_get_foreign_env();
    
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
  intptr_t i;
  int j;

  for (j = 0; j < 6; j++) {
    if (!j)
      kenv = kernel_env;
    else if (j == 1)
      kenv = unsafe_env;
    else if (j == 2)
      kenv = flfxnum_env;
    else if (j == 3)
      kenv = extfl_env;
    else if (j == 4)
      kenv = futures_env;
    else
      kenv = scheme_get_foreign_env();
    
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
/*                  intern literal strings and numbers                    */
/*========================================================================*/

Scheme_Object *scheme_intern_literal_string(Scheme_Object *str)
{
  Scheme_Bucket *b;

  scheme_start_atomic();
  b = scheme_bucket_from_table(literal_string_table, (const char *)str);
  scheme_end_atomic_no_swap();
  if (!b->val)
    b->val = scheme_true;

  return(Scheme_Object *)HT_EXTRACT_WEAK(b->key);
}

Scheme_Object *scheme_intern_literal_number(Scheme_Object *num)
{
  Scheme_Bucket *b;

  scheme_start_atomic();
  b = scheme_bucket_from_table(literal_number_table, (const char *)num);
  scheme_end_atomic_no_swap();
  if (!b->val)
    b->val = scheme_true;

  return(Scheme_Object *)HT_EXTRACT_WEAK(b->key);
}

/*========================================================================*/
/*             run-time and expansion-time Racket interface               */
/*========================================================================*/

static Scheme_Object *
namespace_identifier(int argc, Scheme_Object *argv[])
{
  Scheme_Object *obj;
  Scheme_Env *genv;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_contract("namespace-symbol->identifier", "symbol?", 0, argc, argv);
  if ((argc > 1) && !SCHEME_NAMESPACEP(argv[1]))
    scheme_wrong_contract("namespace-symbol->identifier", "namespace?", 1, argc, argv);

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
      scheme_wrong_contract("namespace-module-identifier", "(or/c namespace? #f exact-integer?)", 0, argc, argv);
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
    scheme_wrong_contract("namespace-base-phase", "namespace?", 0, argc, argv);

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
    scheme_wrong_contract("namespace-variable-value", "symbol?", 0, argc, argv);
  use_map = ((argc > 1) ? SCHEME_TRUEP(argv[1]) : 1);
  if ((argc > 2) && SCHEME_TRUEP(argv[2])
      && !scheme_check_proc_arity(NULL, 0, 2, argc, argv))
    scheme_wrong_contract("namespace-variable-value", "(or/c (-> any) #f)", 2, argc, argv);
  if ((argc > 3) && !SCHEME_NAMESPACEP(argv[3]))
    scheme_wrong_contract("namespace-variable-value", "namespace?", 3, argc, argv);

  if (argc > 3)
    genv = (Scheme_Env *)argv[3];
  else
    genv = scheme_get_env(NULL);

  if (!use_map)
    v = scheme_lookup_global(argv[0], genv);
  else
    v = scheme_namespace_lookup_value(argv[0], genv, &id, &use_map);
  
  if (!v) {
    if ((argc > 2) && SCHEME_TRUEP(argv[2]))
      return _scheme_tail_apply(argv[2], 0, NULL);
    else if (use_map == -1) {
      scheme_wrong_syntax("namespace-variable-value", NULL, id, "bound to syntax");
      return NULL;
    } else {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT_VARIABLE, argv[0],
		       "namespace-variable-value: given name is not defined\n"
                       "  name: %S",
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
    scheme_wrong_contract("namespace-set-variable-value!", "symbol?", 0, argc, argv);
  if ((argc > 3) && !SCHEME_NAMESPACEP(argv[3]))
    scheme_wrong_contract("namespace-set-variable-value!", "namespace?", 3, argc, argv);

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
    scheme_wrong_contract("namespace-undefine-variable!", "symbol?", 0, argc, argv);
  if ((argc > 1) && !SCHEME_NAMESPACEP(argv[1]))
    scheme_wrong_contract("namespace-undefine-variable!", "namespace?", 1, argc, argv);

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
		     "namespace-undefine-variable!: given name is not defined\n"
                     "  name: %S",
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
  intptr_t i, j;

  if ((argc > 0) && !SCHEME_NAMESPACEP(argv[0]))
    scheme_wrong_contract("namespace-mapped-symbols", "namespace?", 0, argc, argv);

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
    scheme_list_module_rename(env->rename_set, mapped, env->module_registry->exports);

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
    scheme_wrong_contract("namespace-module-registry", "namespace?", 0, argc, argv);

  return (Scheme_Object *)((Scheme_Env *)argv[0])->module_registry;
}

static Scheme_Object *do_variable_namespace(const char *who, int tl, int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;
  Scheme_Env *env;
  intptr_t ph;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_global_ref_type)) {
    v = NULL;
    env = NULL;
  }
  else {
    v = SCHEME_PTR1_VAL(argv[0]);
    env = scheme_get_bucket_home((Scheme_Bucket *)v);
  }

  if (!env)
    scheme_wrong_contract(who, "variable-reference?", 0, argc, argv);

  ph = env->phase;
  if (tl == 2) {
    return scheme_make_integer(ph);
  } else if (tl == 3) {
    return scheme_make_integer(ph - env->mod_phase);
  } else if (tl == 4) {
    if (((Scheme_Object *)((Scheme_Bucket *)v)->key != scheme_stack_dump_key)
        || !env->module) {
      scheme_contract_error(who, 
                            "variable reference does not refer to an anonymous module variable",
                            "variable reference", 1, v,
                            NULL);
    }
    return env->access_insp;
  } else if (tl) {
    /* return env directly; need to set up  */
    if (!env->phase && env->module)
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

static Scheme_Object *variable_base_phase(int argc, Scheme_Object *argv[])
{
  return do_variable_namespace("variable-reference->phase", 3, argc, argv);
}

static Scheme_Object *variable_inspector(int argc, Scheme_Object *argv[])
{
  return do_variable_namespace("variable-reference->module-declaration-inspector", 4, argc, argv);
}

static Scheme_Object *variable_const_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;

  v = argv[0];

  if (!SAME_TYPE(SCHEME_TYPE(v), scheme_global_ref_type))
    scheme_wrong_contract("variable-reference-constant?", "variable-reference?", 0, argc, argv);

  if (SCHEME_VARREF_FLAGS(v) & 0x1)
    return scheme_true;

  v = SCHEME_PTR1_VAL(v);
  if (((Scheme_Bucket_With_Flags *)v)->flags & GLOB_IS_IMMUTATED)
    return scheme_true;

  return scheme_false;
}

static Scheme_Object *variable_p(int argc, Scheme_Object *argv[])
{
  Scheme_Env *env;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_global_ref_type))
    env = NULL;
  else
    env = scheme_get_bucket_home((Scheme_Bucket *)SCHEME_PTR1_VAL(argv[0]));

  return env ? scheme_true : scheme_false;
}

static Scheme_Object *variable_module_path(int argc, Scheme_Object *argv[])
{
  Scheme_Env *env;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_global_ref_type))
    env = NULL;
  else
    env = scheme_get_bucket_home((Scheme_Bucket *)SCHEME_PTR1_VAL(argv[0]));

  if (!env)
    scheme_wrong_contract("variable-reference->resolved-module-path", "variable-reference?", 0, argc, argv);

  if (env->module)
    return env->module->modname;
  else
    return scheme_false;
}

static Scheme_Object *variable_modidx(int argc, Scheme_Object *argv[])
{
  Scheme_Env *env;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_global_ref_type))
    env = NULL;
  else
    env = scheme_get_bucket_home((Scheme_Bucket *)SCHEME_PTR1_VAL(argv[0]));

  if (!env)
    scheme_wrong_contract("variable-reference->module-path-index", "variable-reference?", 0, argc, argv);

  if (env->module) {
    if (!env->link_midx) 
      return env->module->self_modidx;
    else
      return env->link_midx;
  } else
    return scheme_false;
}

static Scheme_Object *variable_module_source(int argc, Scheme_Object *argv[])
{
  Scheme_Env *env;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_global_ref_type))
    env = NULL;
  else
    env = scheme_get_bucket_home((Scheme_Bucket *)SCHEME_PTR1_VAL(argv[0]));

  if (!env)
    scheme_wrong_contract("variable-reference->module-source", "variable-reference?", 0, argc, argv);

  if (env->module)
    return scheme_resolved_module_path_value(env->module->modsrc);
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
now_transforming_module(int argc, Scheme_Object *argv[])
{
  if (scheme_get_module_lift_env(scheme_current_thread->current_local_env))
    return scheme_true;
  return scheme_false;
}

static void not_currently_transforming(const char *name)
{
  scheme_contract_error(name,
                        "not currently transforming",
                        NULL);
}

static Scheme_Object *
do_local_exp_time_value(const char *name, int argc, Scheme_Object *argv[], int recur)
{
  Scheme_Object *v, *sym, *a[2], *observer;
  Scheme_Env *menv;
  Scheme_Comp_Env *env;
  int renamed = 0;

  env = scheme_current_thread->current_local_env;
  if (!env)
    not_currently_transforming(name);

  sym = argv[0];

  observer = scheme_get_expand_observe();
  SCHEME_EXPAND_OBSERVE_LOCAL_VALUE(observer, sym);

  if (!(SCHEME_STXP(sym) && SCHEME_SYMBOLP(SCHEME_STX_VAL(sym))))
    scheme_wrong_contract(name, "identifier?", 0, argc, argv);

  if (argc > 1) {
    scheme_check_proc_arity2(name, 0, 1, argc, argv, 1);
    if ((argc > 2)
        && SCHEME_TRUEP(argv[2])) { 
      Scheme_Comp_Env *stx_env;
      if (!SAME_TYPE(scheme_intdef_context_type, SCHEME_TYPE(argv[2])))
	scheme_wrong_contract(name, "(or/c internal-definition-context? #f)", 2, argc, argv);
      stx_env = (Scheme_Comp_Env *)((void **)SCHEME_PTR1_VAL(argv[2]))[0];
      if (!scheme_is_sub_env(stx_env, env)) {
	scheme_contract_error(name, 
                              "transforming context does not match given internal-definition context",
                              NULL);
      }
      env = stx_env;
    }
  }

  if (scheme_current_thread->current_local_mark)
    sym = scheme_add_remove_mark(sym, scheme_current_thread->current_local_mark);

  menv = NULL;

  while (1) {
    v = scheme_lookup_binding(sym, env,
			      (SCHEME_NULL_FOR_UNBOUND
			       + SCHEME_RESOLVE_MODIDS
			       + SCHEME_APP_POS + SCHEME_ENV_CONSTANTS_OK
			       + SCHEME_OUT_OF_CONTEXT_OK + SCHEME_ELIM_CONST),
			      scheme_current_thread->current_local_modidx, 
			      &menv, NULL, NULL, NULL);

    SCHEME_EXPAND_OBSERVE_RESOLVE(observer, sym);

    /* Deref globals */
    if (v && SAME_TYPE(SCHEME_TYPE(v), scheme_variable_type))
      v = (Scheme_Object *)(SCHEME_VAR_BUCKET(v))->val;
    
    if (!v || NOT_SAME_TYPE(SCHEME_TYPE(v), scheme_macro_type)) {
      SCHEME_EXPAND_OBSERVE_LOCAL_VALUE_RESULT(observer, scheme_false);
      if ((argc > 1) && SCHEME_TRUEP(argv[1]))
	return _scheme_tail_apply(argv[1], 0, NULL);
      else
	scheme_contract_error(name,
                              (renamed 
                               ? "not defined as syntax (after renaming)"
                               : "not defined as syntax"),
                              "identifier", 1, argv[0],
                              NULL);
    }
    
    v = SCHEME_PTR_VAL(v);
    if (scheme_is_rename_transformer(v)) {
      sym = scheme_transfer_srcloc(scheme_rename_transformer_id(v), sym);
      renamed = 1;
      menv = NULL;
      SCHEME_USE_FUEL(1);
      if (!recur) {
        SCHEME_EXPAND_OBSERVE_LOCAL_VALUE_RESULT(observer, scheme_true);
        a[0] = v;
        a[1] = sym;
        return scheme_values(2, a);
      }
    } else if (!recur) {
      SCHEME_EXPAND_OBSERVE_LOCAL_VALUE_RESULT(observer, scheme_true);
      a[0] = v;
      a[1] = scheme_false;
      return scheme_values(2, a);
    } else {
      SCHEME_EXPAND_OBSERVE_LOCAL_VALUE_RESULT(observer, scheme_true);
      return v;
    }
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
    not_currently_transforming("syntax-local-name");

  return sym;
}

static Scheme_Object *
local_context(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env;

  env = scheme_current_thread->current_local_env;
  if (!env)
    not_currently_transforming("syntax-local-context");

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
  intptr_t phase;

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
    not_currently_transforming("syntax-local-make-definition-context");
  
  if (argc && SCHEME_TRUEP(argv[0])) {
    if (!SAME_TYPE(scheme_intdef_context_type, SCHEME_TYPE(argv[0])))
      scheme_wrong_contract("syntax-local-make-definition-context", "(or/c internal-definition-context? #f)", 0, argc, argv);
    senv = (Scheme_Comp_Env *)((void **)SCHEME_PTR1_VAL(argv[0]))[0];
    if (!scheme_is_sub_env(senv, env)) {
      scheme_contract_error("syntax-local-make-definition-context",
                            "transforming context does "
                            "not match given internal-definition context",
                            NULL);
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
    scheme_wrong_contract("internal-definition-context-seal", 
                          "internal-definition-context?", 0, argc, argv);
  
  scheme_stx_seal_rib(SCHEME_PTR2_VAL(argv[0]));
  return scheme_void;
}

static Scheme_Object *
id_intdef_remove(int argc, Scheme_Object *argv[])
{
  Scheme_Object *l, *res, *skips;

  if (!SCHEME_STXP(argv[0]) || !SCHEME_SYMBOLP(SCHEME_STX_VAL(argv[0])))
    scheme_wrong_contract("identifier-remove-from-definition-context", 
                          "identifier?", 0, argc, argv);
  
  l = argv[1];
  if (!SAME_TYPE(SCHEME_TYPE(l), scheme_intdef_context_type)) {
    while (SCHEME_PAIRP(l)) {
      if (!SAME_TYPE(SCHEME_TYPE(SCHEME_CAR(l)), scheme_intdef_context_type))
        break;
      l = SCHEME_CDR(l);
    }
    if (!SCHEME_NULLP(l))
      scheme_wrong_contract("identifier-remove-from-definition-context", 
                            "(or/c internal-definition-context? (listof internal-definition-context?))", 
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
    not_currently_transforming("syntax-local-introduce");

  s = argv[0];
  if (!SCHEME_STXP(s))
    scheme_wrong_contract("syntax-local-introduce", "syntax?", 0, argc, argv);

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
    not_currently_transforming("syntax-local-module-introduce");

  s = argv[0];
  if (!SCHEME_STXP(s))
    scheme_wrong_contract("syntax-local-module-introduce", "syntax?", 0, argc, argv);

  v = scheme_stx_source_module(s, 0, 0);
  if (SCHEME_FALSEP(v)) {
    if (env->genv->module
        && env->genv->module->rn_stx
        && SCHEME_VECTORP(env->genv->module->rn_stx)) {
      /* This is a submodule, and `rn_stx' has renames for the enclosing modules */
      int i;
      for (i = SCHEME_VEC_SIZE(env->genv->module->rn_stx); i-- > 1; ) {
        v = SCHEME_VEC_ELS(env->genv->module->rn_stx)[i];
        s = scheme_add_rename(s, scheme_stx_to_rename(v));
      }
    }
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
  Scheme_Comp_Env *env;
  Scheme_Object *sym, *sym_marks = NULL, *orig_sym, *uid = NULL, *free_id = NULL;

  env = scheme_current_thread->current_local_env;
  if (!env)
    not_currently_transforming("syntax-local-get-shadower");

  sym = argv[0];
  orig_sym = sym;

  if (!(SCHEME_STXP(sym) && SCHEME_SYMBOLP(SCHEME_STX_VAL(sym))))
    scheme_wrong_contract("syntax-local-get-shadower", "identifier?", 0, argc, argv);

  sym_marks = scheme_stx_extract_marks(sym);

  uid = scheme_find_local_shadower(sym, sym_marks, env, &free_id);

  if (!uid) {
    uid = scheme_tl_id_sym(env->genv, sym, NULL, 0, 
                           scheme_make_integer(env->genv->phase), NULL);
    if (!SAME_OBJ(uid, SCHEME_STX_VAL(sym))) {
      /* has a toplevel biding via marks or context; keep it */
    } else {
      /* No lexical shadower, but strip module context, if any */
      sym = scheme_stx_strip_module_context(sym);
      /* Add current module context, if any */
      sym = local_module_introduce(1, &sym);

      if (!scheme_stx_is_clean(orig_sym))
        sym = scheme_stx_taint(sym);
    }

    return sym;
  }

  {
    Scheme_Object *rn, *result;

    result = scheme_datum_to_syntax(SCHEME_STX_VAL(sym), orig_sym, sym, 0, 0);
    ((Scheme_Stx *)result)->props = ((Scheme_Stx *)orig_sym)->props;

    rn = scheme_make_rename(uid, 1);
    scheme_set_rename(rn, 0, result);

    result = scheme_add_rename(result, rn);

    if (free_id)
      scheme_install_free_id_rename(result, free_id, NULL, scheme_make_integer(0));

    if (!scheme_stx_is_clean(orig_sym))
      result = scheme_stx_taint(result);

    return result;
  }
}

static Scheme_Object *
introducer_proc(void *mark, int argc, Scheme_Object *argv[])
{
  Scheme_Object *s;

  s = argv[0];
  if (!SCHEME_STXP(s))
    scheme_wrong_contract("syntax-introducer", "syntax?", 0, argc, argv);

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
    scheme_wrong_contract(who, "identifier?", 0, argc, argv);
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
      scheme_wrong_contract(who, "identifier?", -1, -1, a);
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

  env = scheme_current_thread->current_local_env;
  if (!env)
    not_currently_transforming("syntax-local-make-delta-introducer");

  if (!SCHEME_STXP(argv[0]) || !SCHEME_SYMBOLP(SCHEME_STX_VAL(argv[0])))
    scheme_wrong_contract("syntax-local-make-delta-introducer", "identifier?", 0, argc, argv);

  sym = argv[0];

  while (1) {
    binder = NULL;

    v = scheme_lookup_binding(sym, env,
			      (SCHEME_NULL_FOR_UNBOUND
			       + SCHEME_RESOLVE_MODIDS
			       + SCHEME_APP_POS + SCHEME_ENV_CONSTANTS_OK
			       + SCHEME_OUT_OF_CONTEXT_OK + SCHEME_ELIM_CONST),
			      scheme_current_thread->current_local_modidx, 
			      NULL, NULL, &binder, NULL);
    
    /* Deref globals */
    if (v && SAME_TYPE(SCHEME_TYPE(v), scheme_variable_type))
      v = (Scheme_Object *)(SCHEME_VAR_BUCKET(v))->val;
    
    if (!v || NOT_SAME_TYPE(SCHEME_TYPE(v), scheme_macro_type)) {
      scheme_contract_error("syntax-local-make-delta-introducer",
                            (renamed 
                             ? "not defined as syntax (after renaming)"
                             : "not defined as syntax"),
                            "identifier", 1, argv[0],
                            NULL);
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
      sym = scheme_rename_transformer_id(v);

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

Scheme_Object *scheme_get_local_inspector()
{
  Scheme_Thread *p = scheme_current_thread;

  if (p->current_local_menv)
    return p->current_local_menv->access_insp;
  else
    return scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);
}

static Scheme_Object *
local_module_exports(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env;

  env = scheme_current_thread->current_local_env;
  
  if (!env)
    not_currently_transforming("syntax-local-module-exports");

  return scheme_module_exported_list(argv[0], env->genv);
}

static Scheme_Object *local_submodules(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env;
  Scheme_Object *l, *r = scheme_null, *n;

  env = scheme_current_thread->current_local_env;
  
  if (!env)
    not_currently_transforming("syntax-local-submodules");

  if (env->genv->module) {
    l = env->genv->module->pre_submodule_names;
    if (!l)
      l = env->genv->module->pre_submodules;
    if (l) {
      while (!SCHEME_NULLP(l)) {
        n = SCHEME_CAR(l);
        if (!SCHEME_SYMBOLP(n)) {
          n = scheme_resolved_module_path_value(((Scheme_Module *)n)->modname);
          while (SCHEME_PAIRP(SCHEME_CDR(n))) {
            n = SCHEME_CDR(n);
          }
          n = SCHEME_CAR(n);
        }
        r = scheme_make_pair(n, r);
        l = SCHEME_CDR(l);
      }
    }
  }
   
  return r;
}

static Scheme_Object *
local_module_definitions(int argc, Scheme_Object *argv[])
{
  if (!scheme_current_thread->current_local_env
      || !scheme_current_thread->current_local_bindings)
    scheme_contract_error("syntax-local-module-defined-identifiers",
                          "not currently transforming module provides",
                          NULL);
  
  return SCHEME_CDR(scheme_current_thread->current_local_bindings);
}

static Scheme_Object *
local_module_imports(int argc, Scheme_Object *argv[])
{
  if (!scheme_current_thread->current_local_env
      || !scheme_current_thread->current_local_bindings)
    scheme_contract_error("syntax-local-module-required-identifiers",
                          "not currently transforming module provides",
                          NULL);
  
  if (SCHEME_TRUEP(argv[0]) && !scheme_is_module_path(argv[0]))
    scheme_wrong_contract("syntax-local-module-required-identifiers", "(or/c module-path? #f)", 0, argc, argv);
  
  if (!SCHEME_FALSEP(argv[1]) 
      && !SAME_OBJ(scheme_true, argv[1])
      && !SCHEME_INTP(argv[1])
      && !SCHEME_BIGNUMP(argv[1]))
    scheme_wrong_contract("syntax-local-module-required-identifiers", "(or/c exact-integer? #f #t)", 1, argc, argv);
  
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
local_lift_expr(int argc, Scheme_Object *argv[])
{
  Scheme_Object *ids;
  ids = scheme_do_local_lift_expr("syntax-local-lift-expression", 0, argc, argv);
  return SCHEME_CAR(ids);
}

static Scheme_Object *
local_lift_exprs(int argc, Scheme_Object *argv[])
{
  return scheme_do_local_lift_expr("syntax-local-lift-values-expression", 1, argc, argv);
}

static Scheme_Object *
local_lift_context(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env;

  env = scheme_current_thread->current_local_env;

  if (!env)
    not_currently_transforming("syntax-local-lift-context");

  return scheme_local_lift_context(env);
}

static Scheme_Object *
local_lift_end_statement(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env;
  Scheme_Object *local_mark, *expr;

  expr = argv[0];
  if (!SCHEME_STXP(expr))
    scheme_wrong_contract("syntax-local-lift-module-end-declaration", "syntax?", 0, argc, argv);

  env = scheme_current_thread->current_local_env;
  local_mark = scheme_current_thread->current_local_mark;

  if (!env)
    not_currently_transforming("syntax-local-lift-module-end-declaration");

  return scheme_local_lift_end_statement(expr, local_mark, env);
}

static Scheme_Object *local_lift_require(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env;
  Scheme_Object *local_mark;
  intptr_t phase;

  if (!SCHEME_STXP(argv[1]))
    scheme_wrong_contract("syntax-local-lift-require", "syntax?", 1, argc, argv);

  env = scheme_current_thread->current_local_env;
  local_mark = scheme_current_thread->current_local_mark;

  if (!env)
    not_currently_transforming("syntax-local-lift-require");

  phase = env->genv->phase;

  return scheme_local_lift_require(argv[0], argv[1], phase, local_mark, env);
}

static Scheme_Object *local_lift_provide(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env;
  Scheme_Object *form, *local_mark;

  form = argv[0];
  if (!SCHEME_STXP(form))
    scheme_wrong_contract("syntax-local-lift-provide", "syntax?", 1, argc, argv);

  env = scheme_current_thread->current_local_env;
  local_mark = scheme_current_thread->current_local_mark;

  if (!env)
    not_currently_transforming("syntax-local-lift-provide");

  return scheme_local_lift_provide(form, local_mark, env);
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
    scheme_wrong_contract("set!-transformer-procedure", "set!-transformer?", 0, argc, argv);

  return scheme_set_transformer_proc(argv[0]);
}

static Scheme_Object *
make_rename_transformer(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;

  if (!SCHEME_STXP(argv[0]) || !SCHEME_SYMBOLP(SCHEME_STX_VAL(argv[0])))
    scheme_wrong_contract("make-rename-transformer", "identifier?", 0, argc, argv);

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
    scheme_wrong_contract("rename-transformer-target", "rename-transformer?", 0, argc, argv);

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
/*                         precise GC traversers                          */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_env.inc"

static void register_traversers(void)
{

}

END_XFORM_SKIP;

#endif
