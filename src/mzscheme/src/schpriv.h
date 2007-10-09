/*
  MzScheme
  Copyright (c) 2004-2007 PLT Scheme Inc.
  Copyright (c) 1995-2001 Matthew Flatt
  All rights reserved.

  Please see the full copyright in the documentation.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

/*
   MzScheme prototypes and declarations for internal consumption.
*/

#ifndef __mzscheme_private__
#define __mzscheme_private__

#include "scheme.h"

/*========================================================================*/
/*                         allocation and GC                              */
/*========================================================================*/

#define MAKE_CLOSED_PRIM(f,v,n,mi,ma) \
  scheme_make_closed_prim_w_arity((Scheme_Closed_Prim *)f, (void *)v, n, mi, ma)

#define _MALLOC_N(x, n, malloc) ((x*)malloc(sizeof(x)*(n)))
#define MALLOC_ONE(x) _MALLOC_N(x, 1, scheme_malloc)
#define MALLOC_ONE_TAGGED(x) _MALLOC_N(x, 1, scheme_malloc_small_tagged)
#define MALLOC_N_TAGGED(x, n) _MALLOC_N(x, n, scheme_malloc_array_tagged)
#ifdef MZTAG_REQUIRED
# define scheme_malloc_rt(x) scheme_malloc_tagged(x)
# define MALLOC_ONE_RT(x) MALLOC_ONE_TAGGED(x)
# define MALLOC_N_RT(x,c) MALLOC_N_TAGGED(x,c)
# define MALLOC_ONE_WEAK(x) _MALLOC_N(x, 1, scheme_malloc)
# define MALLOC_N_WEAK(x,c) _MALLOC_N(x, c, scheme_malloc)
# define MALLOC_ONE_TAGGED_WEAK(x) _MALLOC_N(x, 1, scheme_malloc_tagged)
# define MALLOC_ONE_WEAK_RT(x) MALLOC_ONE_TAGGED_WEAK(x)
#else
# define scheme_malloc_rt(x) scheme_malloc(x)
# define MALLOC_ONE_RT(x) MALLOC_ONE(x)
# define MALLOC_N_RT(x,c) MALLOC_N(x,c)
# define MALLOC_ONE_WEAK(x) MALLOC_ONE_ATOMIC(x)
# define MALLOC_N_WEAK(x,c) MALLOC_N_ATOMIC(x,c)
# define MALLOC_ONE_WEAK_RT(x) MALLOC_ONE_WEAK(x)
# define MALLOC_ONE_TAGGED_WEAK(x) MALLOC_ONE_WEAK(x)
#endif
#define MALLOC_N(x, n) _MALLOC_N(x, n, scheme_malloc)
#define MALLOC_ONE_ATOMIC(x) _MALLOC_N(x, 1, scheme_malloc_atomic)
#define MALLOC_N_ATOMIC(x, n) _MALLOC_N(x, n, scheme_malloc_atomic)
#define MALLOC_SO_BOX() _MALLOC_ONE(Scheme_Object*, scheme_malloc)
#define MALLOC_N_STUBBORN(x, n) _MALLOC_N(x, n, scheme_malloc_stubborn)

#ifdef MZ_PRECISE_GC
# define WEAKIFY(x) scheme_make_weak_box(x)
# define WEAKIFIED(x) SCHEME_WEAK_BOX_VAL(x)
# define HT_EXTRACT_WEAK(x) SCHEME_WEAK_BOX_VAL(x)
#else
# define WEAKIFY(x) x
# define WEAKIFIED(x) x
# define HT_EXTRACT_WEAK(x) (*(char **)(x))
#endif

#ifndef MZ_XFORM
# define START_XFORM_SKIP /**/
# define END_XFORM_SKIP /**/
# define GC_CAN_IGNORE /**/
# define GC_MAYBE_IGNORE_INTERIOR /**/
# define XFORM_OK_PLUS +
# define XFORM_OK_MINUS -
#else
# ifdef GC_INTERIORABLES_NEVER_MOVE
#  define GC_MAYBE_IGNORE_INTERIOR GC_CAN_IGNORE
# else
#  define GC_MAYBE_IGNORE_INTERIOR /**/
# endif
#endif

#ifdef MZ_PRECISE_GC
long scheme_hash_key(Scheme_Object *o);
#else
# define scheme_hash_key(o) ((long)(o))
#endif
typedef int (*Compare_Proc)(void *v1, void *v2);

Scheme_Object *scheme_dump_gc_stats(int c, Scheme_Object *p[]);

#define REGISTER_SO(x) MZ_REGISTER_STATIC(x)

extern long scheme_total_gc_time;
extern int scheme_cont_capture_count;
extern int scheme_continuation_application_count;

int scheme_num_types(void);

#ifdef MZTAG_REQUIRED
# define MZTAG_IF_REQUIRED  Scheme_Type type;
# define SET_REQUIRED_TAG(e) e
#else
# define MZTAG_IF_REQUIRED /* empty */
# define SET_REQUIRED_TAG(e) /* empty */
#endif

void scheme_reset_finalizations(void);

extern unsigned long scheme_get_stack_base(void);

int scheme_propagate_ephemeron_marks(void);
void scheme_clear_ephemerons(void);

#ifndef MZ_XFORM
# define HIDE_FROM_XFORM(x) x
#endif

#define mzALIAS (void *)

#define BITS_PER_MZSHORT (8 * sizeof(mzshort))

#ifndef NO_INLINE_KEYWORD
# define MZ_INLINE MSC_IZE(inline)
#else
# define MZ_INLINE /* empty */
#endif

#ifdef MZ_PRECISE_GC
# define CLEAR_KEY_FIELD(o) ((o)->keyex = 0)
#else
# define CLEAR_KEY_FIELD(o) /* empty */
#endif

/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/

extern int scheme_starting_up;

void scheme_init_portable_case(void);
void scheme_init_stack_check(void);
void scheme_init_overflow(void);
#ifdef MZ_PRECISE_GC
void scheme_register_traversers(void);
void scheme_init_hash_key_procs(void);
#endif
Scheme_Thread *scheme_make_thread(void);
void scheme_init_true_false(void);
void scheme_init_symbol_table(void);
void scheme_init_symbol_type(Scheme_Env *env);
void scheme_init_type(Scheme_Env *env);
void scheme_init_list(Scheme_Env *env);
void scheme_init_stx(Scheme_Env *env);
void scheme_init_module(Scheme_Env *env);
void scheme_init_port(Scheme_Env *env);
void scheme_init_port_fun(Scheme_Env *env);
void scheme_init_network(Scheme_Env *env);
void scheme_init_file(Scheme_Env *env);
void scheme_init_proc(Scheme_Env *env);
void scheme_init_vector(Scheme_Env *env);
void scheme_init_string(Scheme_Env *env);
void scheme_init_number(Scheme_Env *env);
void scheme_init_numarith(Scheme_Env *env);
void scheme_init_numcomp(Scheme_Env *env);
void scheme_init_numstr(Scheme_Env *env);
void scheme_init_eval(Scheme_Env *env);
void scheme_init_promise(Scheme_Env *env);
void scheme_init_struct(Scheme_Env *env);
void scheme_init_fun(Scheme_Env *env);
void scheme_init_symbol(Scheme_Env *env);
void scheme_init_char(Scheme_Env *env);
void scheme_init_bool(Scheme_Env *env);
void scheme_init_syntax(Scheme_Env *env);
void scheme_init_error(Scheme_Env *env);
#ifndef NO_SCHEME_EXNS
void scheme_init_exn(Scheme_Env *env);
#endif
void scheme_init_debug(Scheme_Env *env);
void scheme_init_thread(Scheme_Env *env);
void scheme_init_read(Scheme_Env *env);
void scheme_init_print(Scheme_Env *env);
#ifndef NO_SCHEME_THREADS
void scheme_init_sema(Scheme_Env *env);
#endif
void scheme_init_dynamic_extension(Scheme_Env *env);
#ifndef NO_REGEXP_UTILS
extern void scheme_regexp_initialize(Scheme_Env *env);
#endif
void scheme_init_memtrace(Scheme_Env *env);
void scheme_init_parameterization(Scheme_Env *env);
void scheme_init_getenv(void);

#ifndef DONT_USE_FOREIGN
void scheme_init_foreign(Scheme_Env *env);
#endif

/* Type readers & writers for compiled code data */
typedef Scheme_Object *(*Scheme_Type_Reader)(Scheme_Object *list);
typedef Scheme_Object *(*Scheme_Type_Writer)(Scheme_Object *obj);

extern Scheme_Type_Reader *scheme_type_readers;
extern Scheme_Type_Writer *scheme_type_writers;

extern Scheme_Equal_Proc *scheme_type_equals;
extern Scheme_Primary_Hash_Proc *scheme_type_hash1s;
extern Scheme_Secondary_Hash_Proc *scheme_type_hash2s;

void scheme_init_port_config(void);
void scheme_init_port_fun_config(void);
Scheme_Config *scheme_init_error_escape_proc(Scheme_Config *c);
void scheme_init_error_config(void);
#ifndef NO_SCHEME_EXNS
void scheme_init_exn_config(void);
#endif
#ifdef WINDOWS_PROCESSES
void scheme_init_thread_memory(void);
#endif

void scheme_finish_kernel(Scheme_Env *env);

Scheme_Object *scheme_make_initial_inspectors(void);

extern int scheme_builtin_ref_counter;

Scheme_Object **scheme_make_builtin_references_table(void);
Scheme_Object *scheme_make_local(Scheme_Type type, int pos);

void scheme_add_embedded_builtins(Scheme_Env *env);
void scheme_do_add_global_symbol(Scheme_Env *env, Scheme_Object *sym,
				 Scheme_Object *obj, int constant,
				 int primitive);

/*========================================================================*/
/*                                constants                               */
/*========================================================================*/

extern Scheme_Object *scheme_values_func;
extern Scheme_Object *scheme_procedure_p_proc;
extern Scheme_Object *scheme_void_proc;
extern Scheme_Object *scheme_call_with_values_proc;
extern Scheme_Object *scheme_make_struct_type_proc;

extern Scheme_Object *scheme_define_values_syntax, *scheme_define_syntaxes_syntax;
extern Scheme_Object *scheme_lambda_syntax;
extern Scheme_Object *scheme_begin_syntax;

extern Scheme_Object *scheme_not_prim;
extern Scheme_Object *scheme_eq_prim;
extern Scheme_Object *scheme_eqv_prim;
extern Scheme_Object *scheme_equal_prim;

extern Scheme_Object *scheme_def_exit_proc;

extern Scheme_Object *scheme_orig_stdout_port;
extern Scheme_Object *scheme_orig_stdin_port;
extern Scheme_Object *scheme_orig_stderr_port;

extern Scheme_Object *scheme_arity_at_least, *scheme_make_arity_at_least;

extern Scheme_Object *scheme_write_proc, *scheme_display_proc, *scheme_print_proc;

extern Scheme_Object *scheme_raise_arity_error_proc;

#ifdef TIME_SYNTAX
extern Scheme_Object *scheme_date;
#endif

extern Scheme_Object *scheme_module_stx;
extern Scheme_Object *scheme_begin_stx;
extern Scheme_Object *scheme_define_values_stx;
extern Scheme_Object *scheme_define_syntaxes_stx;
extern Scheme_Object *scheme_top_stx;

extern Scheme_Object *scheme_recur_symbol, *scheme_display_symbol, *scheme_write_special_symbol;

extern Scheme_Object *scheme_none_symbol, *scheme_line_symbol, *scheme_block_symbol;

extern Scheme_Object *scheme_stack_dump_key;

extern Scheme_Object *scheme_default_prompt_tag;

extern Scheme_Object *scheme_system_idle_channel;

extern Scheme_Object *scheme_input_port_property, *scheme_output_port_property;

/*========================================================================*/
/*                    thread state and maintenance                        */
/*========================================================================*/

#define RUNSTACK_IS_GLOBAL

#ifdef RUNSTACK_IS_GLOBAL
extern Scheme_Object **scheme_current_runstack;
extern Scheme_Object **scheme_current_runstack_start;
extern MZ_MARK_STACK_TYPE scheme_current_cont_mark_stack;
extern MZ_MARK_POS_TYPE scheme_current_cont_mark_pos;
# define MZ_RUNSTACK scheme_current_runstack
# define MZ_RUNSTACK_START scheme_current_runstack_start
# define MZ_CONT_MARK_STACK scheme_current_cont_mark_stack
# define MZ_CONT_MARK_POS scheme_current_cont_mark_pos
#else
# define MZ_RUNSTACK (scheme_current_thread->runstack)
# define MZ_RUNSTACK_START (scheme_current_thread->runstack_start)
# define MZ_CONT_MARK_STACK (scheme_current_thread->cont_mark_stack)
# define MZ_CONT_MARK_POS (scheme_current_thread->cont_mark_pos)
#endif

extern volatile int scheme_fuel_counter;

extern Scheme_Thread *scheme_main_thread;

#define SCHEME_TAIL_COPY_THRESHOLD 5

/* Flags for Scheme_Thread's `running' field: */
#define MZTHREAD_RUNNING 0x1
#define MZTHREAD_SUSPENDED 0x2
#define MZTHREAD_KILLED 0x4
#define MZTHREAD_NEED_KILL_CLEANUP 0x8
#define MZTHREAD_USER_SUSPENDED 0x10
#define MZTHREAD_NEED_SUSPEND_CLEANUP 0x20
#define MZTHREAD_STILL_RUNNING(running) ((running) && !((running) & MZTHREAD_KILLED))

#ifdef WINDOWS_PROCESSES
MZ_EXTERN struct Scheme_Thread_Memory *scheme_remember_thread(void *, int);
void scheme_remember_subthread(struct Scheme_Thread_Memory *, void *);
MZ_EXTERN void scheme_forget_thread(struct Scheme_Thread_Memory *);
void scheme_forget_subthread(struct Scheme_Thread_Memory *);
void scheme_suspend_remembered_threads(void);
void scheme_resume_remembered_threads(void);
#endif
#if defined(USE_WIN32_THREAD_TIMER) || defined(USE_PTHREAD_THREAD_TIMER)
void scheme_start_itimer_thread(long usec);
#endif

#ifdef UNIX_PROCESSES
void scheme_block_child_signals(int block);
#endif

Scheme_Object **scheme_alloc_runstack(long len);
void scheme_set_runstack_limits(Scheme_Object **rs, long len, long start, long end);

void scheme_alloc_list_stack(Scheme_Thread *p);
void scheme_clean_list_stack(Scheme_Thread *p);

#ifdef WIN32_THREADS
void *scheme_win32_get_break_semaphore(void *th);
#endif

Scheme_Object *scheme_get_thread_dead(Scheme_Thread *p);
Scheme_Object *scheme_get_thread_suspend(Scheme_Thread *p);

void scheme_zero_unneeded_rands(Scheme_Thread *p);

int scheme_can_break(Scheme_Thread *p);

extern int scheme_overflow_count;

#define MZTHREADELEM(p, x) scheme_ ## x

struct Scheme_Custodian {
  Scheme_Object so;
  char shut_down, has_limit;
  int count, alloc;
  Scheme_Object ***boxes;
  Scheme_Custodian_Reference **mrefs;
  Scheme_Close_Custodian_Client **closers;
  void **data;

  /* weak indirections: */
  Scheme_Custodian_Reference *parent;
  Scheme_Custodian_Reference *sibling;
  Scheme_Custodian_Reference *children;

  Scheme_Custodian_Reference *global_next;
  Scheme_Custodian_Reference *global_prev;

#ifdef MZ_PRECISE_GC
  int gc_owner_set;
  Scheme_Object *cust_boxes;
#endif
};

typedef struct Scheme_Custodian_Box {
  Scheme_Object so;
  Scheme_Custodian *cust;
  Scheme_Object *v;
} Scheme_Custodian_Box;

Scheme_Thread *scheme_do_close_managed(Scheme_Custodian *m, Scheme_Exit_Closer_Func f);

typedef struct Scheme_Security_Guard {
  Scheme_Object so;
  struct Scheme_Security_Guard *parent;
  Scheme_Object *file_proc;    /* who-symbol path mode-symbol -> void */
  Scheme_Object *network_proc; /* who-symbol host-string-or-'listen port-k -> void */
  Scheme_Object *link_proc;    /* who-symbol path path -> void */
} Scheme_Security_Guard;

/* Always allocated on the stack: */
typedef struct {
  Scheme_Thread *false_positive_ok;  /* non-zero => return 1 to swap in thread rather than running Scheme code */
  int potentially_false_positive; /* => returning 1 to swap thread in, but truth may be 0 */
  Scheme_Object *current_syncing;
  double sleep_end;
  int w_i;
  short spin;
  short is_poll;
} Scheme_Schedule_Info;

void scheme_set_sync_target(Scheme_Schedule_Info *sinfo, Scheme_Object *target,
			    Scheme_Object *wrap, Scheme_Object *nack,
			    int repost, int retry);

typedef int (*Scheme_Ready_Fun_FPC)(Scheme_Object *o, Scheme_Schedule_Info *sinfo);

void scheme_check_break_now(void);

extern int scheme_main_was_once_suspended;

/* A "flattened" config. Maps parameters to thread cells. */
typedef struct {
  MZTAG_IF_REQUIRED
  Scheme_Bucket_Table *extensions;
  Scheme_Object *prims[1];
} Scheme_Parameterization;

struct Scheme_Config {
  Scheme_Object so;
  Scheme_Object *key;
  Scheme_Object *cell;
  int depth;
  struct Scheme_Config *next;
};

extern Scheme_Object *scheme_parameterization_key;
extern Scheme_Object *scheme_exn_handler_key;
extern Scheme_Object *scheme_break_enabled_key;

extern void scheme_flatten_config(Scheme_Config *c);

extern Scheme_Object *scheme_apply_thread_thunk(Scheme_Object *rator);

/*========================================================================*/
/*                       hash tables and globals                          */
/*========================================================================*/

#define GLOB_IS_CONST 1
#define GLOB_IS_PRIMITIVE 4
#define GLOB_IS_PERMANENT 8
#define GLOB_HAS_REF_ID 16
#define GLOB_HAS_HOME_PTR 32
#define GLOB_IS_IMMUTATED 64

typedef struct {
  Scheme_Bucket bucket;
  short flags, id;
} Scheme_Bucket_With_Flags;

typedef Scheme_Bucket_With_Flags Scheme_Bucket_With_Ref_Id;

typedef struct {
  Scheme_Bucket_With_Ref_Id bucket;
  Scheme_Env *home;
} Scheme_Bucket_With_Home;

Scheme_Object *
scheme_get_primitive_global(Scheme_Object *var, Scheme_Env *env,
			    int bucket_ok, int can_opt, int signal);

void scheme_add_bucket_to_table(Scheme_Bucket_Table *table, Scheme_Bucket *b);
Scheme_Bucket *scheme_bucket_or_null_from_table(Scheme_Bucket_Table *table, const char *key, int add);

void scheme_require_from_original_env(Scheme_Env *env, int syntax_only);

/*========================================================================*/
/*                              structs                                   */
/*========================================================================*/

typedef struct Scheme_Inspector {
  Scheme_Object so;
  int depth;
  struct Scheme_Inspector *superior;
} Scheme_Inspector;

typedef struct Scheme_Struct_Property {
  Scheme_Object so;
  Scheme_Object *name; /* a symbol */
  Scheme_Object *guard; /* NULL or a procedure */
} Scheme_Struct_Property;

int scheme_inspector_sees_part(Scheme_Object *s, Scheme_Object *insp, int pos);

typedef struct Scheme_Struct_Type {
  Scheme_Object so; /* scheme_structure_type or scheme_proc_struct_type */
  mzshort num_slots, num_islots;
  mzshort name_pos;

  Scheme_Object *name;

  Scheme_Object *inspector;
  Scheme_Object *accessor, *mutator;

  Scheme_Object *uninit_val;

  Scheme_Object **props; /* normally an array of pair of (property, value) pairs */
  int num_props; /* < 0 => props is really a hash table */

  Scheme_Object *proc_attr; /* int (position) or proc, only for proc_struct */
  char *immutables;

  Scheme_Object *guard;

  struct Scheme_Struct_Type *parent_types[1];
} Scheme_Struct_Type;

typedef struct Scheme_Structure
{
  Scheme_Object so;
  Scheme_Struct_Type *stype;
  Scheme_Object *slots[1];
} Scheme_Structure;

typedef struct Struct_Proc_Info {
  MZTAG_IF_REQUIRED
  Scheme_Struct_Type *struct_type;
  char *func_name;
  mzshort field;
} Struct_Proc_Info;

#define SCHEME_STRUCT_TYPE(o) (((Scheme_Structure *)o)->stype)

#define SCHEME_STRUCT_NUM_SLOTS(o) (SCHEME_STRUCT_TYPE(o)->num_slots)
#define SCHEME_STRUCT_NAME_SYM(o) (SCHEME_STRUCT_TYPE(o)->name)

Scheme_Object **scheme_make_struct_names_from_array(const char *base,
						    int fcount,
						    const char **field_names,
						    int flags, int *count_out);
Scheme_Object *scheme_make_struct_type_from_string(const char *base,
						   Scheme_Object *parent,
						   int num_fields,
						   Scheme_Object *props,
						   Scheme_Object *guard,
						   int immutable);
Scheme_Object *scheme_make_proc_struct_type(Scheme_Object *base,
                                            Scheme_Object *parent,
                                            Scheme_Object *inspector,
                                            int num_fields, int num_uninit,
                                            Scheme_Object *uninit_val,
                                            Scheme_Object *proc_attr,
                                            Scheme_Object *guard);

Scheme_Object *scheme_struct_to_vector(Scheme_Object *_s, Scheme_Object *unknown_val, Scheme_Object *insp);

Scheme_Object *scheme_extract_struct_procedure(Scheme_Object *obj, int num_rands, Scheme_Object **rands, int *is_method);

Scheme_Object *scheme_proc_struct_name_source(Scheme_Object *a);

Scheme_Object *scheme_is_writable_struct(Scheme_Object *s);

#define SCHEME_STRUCT_INSPECTOR(obj) (((Scheme_Structure *)obj)->stype->inspector)

extern Scheme_Object *scheme_source_property;

/*========================================================================*/
/*                         syntax objects                                 */
/*========================================================================*/

#define MZ_LABEL_PHASE 30000

typedef struct Scheme_Stx_Srcloc {
  MZTAG_IF_REQUIRED
  long line, col, pos, span;
  Scheme_Object *src;
} Scheme_Stx_Srcloc;

#define STX_GRAPH_FLAG 0x1
#define STX_SUBSTX_FLAG 0x2

typedef struct Scheme_Stx {
  Scheme_Inclhash_Object iso; /* 0x1 and 0x2 of keyex used */
  Scheme_Object *val;
  Scheme_Stx_Srcloc *srcloc;
  Scheme_Object *wraps;
  union {
    long lazy_prefix; /* # of initial items in wraps to propagate */
    Scheme_Object *modinfo_cache;
  } u;
  Scheme_Object *certs; /* cert chain or pair of cert chains */
  Scheme_Object *props;
} Scheme_Stx;

typedef struct Scheme_Stx_Offset {
  Scheme_Object so;
  long line, col, pos;
  Scheme_Object *src;
} Scheme_Stx_Offset;

struct Scheme_Marshal_Tables;
struct Scheme_Unmarshal_Tables;

Scheme_Object *scheme_make_stx(Scheme_Object *val,
			       Scheme_Stx_Srcloc *srcloc,
			       Scheme_Object *props);
Scheme_Object *scheme_make_stx_w_offset(Scheme_Object *val,
					long line, long col, long pos, long span,
					Scheme_Object *src,
					Scheme_Object *props);
Scheme_Object *scheme_make_graph_stx(Scheme_Object *stx,
				     long line, long col, long pos);
Scheme_Object *scheme_make_renamed_stx(Scheme_Object *sym,
				       Scheme_Object *rn);

Scheme_Object *scheme_new_stx_simplify_cache(void);
void scheme_simplify_stx(Scheme_Object *stx, Scheme_Object *simplify_cache);

Scheme_Object *scheme_datum_to_syntax(Scheme_Object *o, Scheme_Object *stx_src,
				      Scheme_Object *stx_wraps,
				      int cangraph, int copyprops);
Scheme_Object *scheme_syntax_to_datum(Scheme_Object *stx, int with_marks,
				      struct Scheme_Marshal_Tables *mt);
Scheme_Object *scheme_unmarshal_datum_to_syntax(Scheme_Object *o,
                                                struct Scheme_Unmarshal_Tables *ut,
                                                int can_graph);

int scheme_syntax_is_graph(Scheme_Object *stx);

Scheme_Object *scheme_stx_track(Scheme_Object *naya,
				Scheme_Object *old,
				Scheme_Object *origin);

int scheme_stx_has_empty_wraps(Scheme_Object *);

Scheme_Object *scheme_new_mark(void);
Scheme_Object *scheme_add_remove_mark(Scheme_Object *o, Scheme_Object *m);

Scheme_Object *scheme_make_rename(Scheme_Object *newname, int c);
void scheme_set_rename(Scheme_Object *rnm, int pos, Scheme_Object *oldname);

#define SCHEME_RIBP(v) SAME_TYPE(scheme_lexical_rib_type, SCHEME_TYPE(v))
Scheme_Object *scheme_make_rename_rib(void);
void scheme_add_rib_rename(Scheme_Object *ro, Scheme_Object *rename);
void scheme_drop_first_rib_rename(Scheme_Object *ro);

Scheme_Object *scheme_add_rename(Scheme_Object *o, Scheme_Object *rename);
Scheme_Object *scheme_add_rename_rib(Scheme_Object *o, Scheme_Object *rib);

Scheme_Object *scheme_stx_remove_extra_marks(Scheme_Object *o, Scheme_Object *relative_to,
                                             Scheme_Object *uid);

#define mzMOD_RENAME_TOPLEVEL 0
#define mzMOD_RENAME_NORMAL   1
#define mzMOD_RENAME_MARKED   2

Scheme_Object *scheme_make_module_rename(long phase, int kind, Scheme_Hash_Table *mns);
void scheme_extend_module_rename(Scheme_Object *rn, Scheme_Object *modname,
				 Scheme_Object *locname, Scheme_Object *exname,
				 Scheme_Object *nominal_src, Scheme_Object *nominal_ex,
				 int mod_phase, int drop_for_marshal);
void scheme_extend_module_rename_with_kernel(Scheme_Object *rn, Scheme_Object *nominal_src);
void scheme_save_module_rename_unmarshal(Scheme_Object *rn, Scheme_Object *info);
void scheme_do_module_rename_unmarshal(Scheme_Object *rn, Scheme_Object *info,
				       Scheme_Object *modidx_shift_from, Scheme_Object *modidx_shift_to,
				       Scheme_Hash_Table *export_registry);
void scheme_remove_module_rename(Scheme_Object *mrn,
				 Scheme_Object *localname);
void scheme_append_module_rename(Scheme_Object *src, Scheme_Object *dest);
void scheme_list_module_rename(Scheme_Object *src, Scheme_Hash_Table *ht);

Scheme_Object *scheme_rename_to_stx(Scheme_Object *rn);
Scheme_Object *scheme_stx_to_rename(Scheme_Object *stx);
Scheme_Object *scheme_stx_shift_rename(Scheme_Object *mrn, Scheme_Object *old_midx, Scheme_Object *new_midx);
Scheme_Hash_Table *scheme_module_rename_marked_names(Scheme_Object *rn);

Scheme_Object *scheme_stx_content(Scheme_Object *o);
Scheme_Object *scheme_flatten_syntax_list(Scheme_Object *lst, int *islist);

int scheme_stx_free_eq(Scheme_Object *a, Scheme_Object *b, long phase);
int scheme_stx_module_eq(Scheme_Object *a, Scheme_Object *b, long phase);
Scheme_Object *scheme_stx_module_name(Scheme_Object **name, long phase,
				      Scheme_Object **nominal_modidx,
				      Scheme_Object **nominal_name,
				      int *mod_phase);
Scheme_Object *scheme_stx_moduleless_env(Scheme_Object *a, long phase);
int scheme_stx_parallel_is_used(Scheme_Object *sym, Scheme_Object *stx);

int scheme_stx_bound_eq(Scheme_Object *a, Scheme_Object *b, long phase);
int scheme_stx_env_bound_eq(Scheme_Object *a, Scheme_Object *b, Scheme_Object *uid, long phase);

Scheme_Object *scheme_stx_source_module(Scheme_Object *stx, int resolve);

Scheme_Object *scheme_stx_property(Scheme_Object *_stx,
				   Scheme_Object *key,
				   Scheme_Object *val);

Scheme_Object *scheme_stx_phase_shift(Scheme_Object *stx, long shift,
				      Scheme_Object *old_midx, Scheme_Object *new_midx,
				      Scheme_Hash_Table *export_registry);
Scheme_Object *scheme_stx_phase_shift_as_rename(long shift,
						Scheme_Object *old_midx, Scheme_Object *new_midx,
						Scheme_Hash_Table *export_registry);

int scheme_stx_list_length(Scheme_Object *list);
int scheme_stx_proper_list_length(Scheme_Object *list);

Scheme_Object *scheme_stx_extract_marks(Scheme_Object *stx);

Scheme_Object *scheme_resolve_placeholders(Scheme_Object *obj, int mkstx, Scheme_Type ph_type);
Scheme_Hash_Table *scheme_setup_datum_graph(Scheme_Object *o, void *for_print);

Scheme_Object *scheme_stx_strip_module_context(Scheme_Object *stx);

#define SCHEME_STX_VAL(s) ((Scheme_Stx *)s)->val

#define SCHEME_STX_PAIRP(o) (SCHEME_PAIRP(o) || (SCHEME_STXP(o) && SCHEME_PAIRP(SCHEME_STX_VAL(o))))
#define SCHEME_STX_SYMBOLP(o) (SCHEME_SYMBOLP(o) || (SCHEME_STXP(o) && SCHEME_SYMBOLP(SCHEME_STX_VAL(o))))
#define SCHEME_STX_NULLP(o) (SCHEME_NULLP(o) || (SCHEME_STXP(o) && SCHEME_NULLP(SCHEME_STX_VAL(o))))

#define SCHEME_STX_CAR(o) (SCHEME_PAIRP(o) ? SCHEME_CAR(o) : SCHEME_CAR(scheme_stx_content(o)))
#define SCHEME_STX_CDR(o) (SCHEME_PAIRP(o) ? SCHEME_CDR(o) : SCHEME_CDR(scheme_stx_content(o)))
#define SCHEME_STX_SYM(o) (SCHEME_STXP(o) ? SCHEME_STX_VAL(o) : o)

Scheme_Object *scheme_source_to_name(Scheme_Object *code);

#define STX_SRCTAG scheme_false

Scheme_Object *scheme_stx_cert(Scheme_Object *o, Scheme_Object *mark, Scheme_Env *menv, Scheme_Object *plus_stx, 
			       Scheme_Object *mkey, int active);
int scheme_stx_certified(Scheme_Object *stx, Scheme_Object *extra_certs, Scheme_Object *modidx, Scheme_Object *home_insp);
int scheme_module_protected_wrt(Scheme_Object *home_insp, Scheme_Object *insp);
Scheme_Object *scheme_stx_activate_certs(Scheme_Object *stx);

Scheme_Object *scheme_stx_extract_certs(Scheme_Object *o, Scheme_Object *base_certs);
Scheme_Object *scheme_stx_add_inactive_certs(Scheme_Object *o, Scheme_Object *certs);
Scheme_Object *scheme_stx_propagate_inactive_certs(Scheme_Object *o, Scheme_Object *orig);

int scheme_stx_has_more_certs(Scheme_Object *id, Scheme_Object *certs,
			      Scheme_Object *than_id, Scheme_Object *than_certs);

Scheme_Object *scheme_delayed_rename(Scheme_Object **o, long i);

/*========================================================================*/
/*                   syntax run-time structures                           */
/*========================================================================*/

typedef struct {
  Scheme_Object so;
  mzshort num_args; /* doesn't include rator, so arguments are at args[1]...args[num_args] */
  Scheme_Object *args[1];
  /* After array of f & args, array of chars for eval type */
} Scheme_App_Rec;

typedef struct {
  Scheme_Inclhash_Object iso; /* keyex used for flags */
  Scheme_Object *rator;
  Scheme_Object *rand;
} Scheme_App2_Rec;

#define SCHEME_APPN_FLAGS(app) MZ_OPT_HASH_KEY(&app->iso)

typedef struct {
  Scheme_Inclhash_Object iso; /* keyex used for flags */
  Scheme_Object *rator;
  Scheme_Object *rand1;
  Scheme_Object *rand2;
} Scheme_App3_Rec;

typedef struct {
  Scheme_Object so;
  Scheme_Object *test;
  Scheme_Object *tbranch;
  Scheme_Object *fbranch;
} Scheme_Branch_Rec;

typedef struct {
  Scheme_Object so;
  mzshort max_let_depth;
  Scheme_Object *code;
  struct Resolve_Prefix *prefix;
} Scheme_Compilation_Top;

typedef struct Scheme_Compiled_Let_Value {
  Scheme_Object so;
  mzshort count;
  mzshort position;
  int *flags;
  Scheme_Object *value;
  Scheme_Object *body;
} Scheme_Compiled_Let_Value;

typedef struct Scheme_Let_Header {
  Scheme_Inclhash_Object iso; /* keyex used for recursive */
  mzshort count;
  mzshort num_clauses;
  Scheme_Object *body;
} Scheme_Let_Header;

#define SCHEME_LET_FLAGS(lh) MZ_OPT_HASH_KEY(&lh->iso)
#define SCHEME_LET_RECURSIVE 0x1
#define SCHEME_LET_STAR 0x2

typedef struct {
  Scheme_Object so;
  Scheme_Object *key;
  Scheme_Object *val;
  Scheme_Object *body;
} Scheme_With_Continuation_Mark;

typedef struct Scheme_Local {
  Scheme_Object so;
  mzshort position;
#ifdef MZ_PRECISE_GC
# ifdef MZSHORT_IS_SHORT
  /* Everything has to be at least 2 words in size. */
  int x;
# endif
#endif
} Scheme_Local;

#define SCHEME_LOCAL_POS(obj)    (((Scheme_Local *)(obj))->position)

typedef struct Scheme_Toplevel {
  Scheme_Inclhash_Object iso; /* keyex used for const flag */
  mzshort depth;
  int position;
} Scheme_Toplevel;

#define SCHEME_TOPLEVEL_DEPTH(obj)    (((Scheme_Toplevel *)(obj))->depth)
#define SCHEME_TOPLEVEL_POS(obj)    (((Scheme_Toplevel *)(obj))->position)
#define SCHEME_TOPLEVEL_FLAGS(obj)  MZ_OPT_HASH_KEY(&((Scheme_Toplevel *)(obj))->iso)

#define SCHEME_TOPLEVEL_CONST   0x1
#define SCHEME_TOPLEVEL_MUTATED 0x2
#define SCHEME_TOPLEVEL_READY   0x2
/* MUTATED and READY flags are used in different contexts */
#define SCHEME_TOPLEVEL_FLAGS_MASK 0x3

typedef struct Scheme_Quote_Syntax {
  Scheme_Object so; /* scheme_quote_syntax_type */
  mzshort depth;
  mzshort position;
  mzshort midpoint;
} Scheme_Quote_Syntax;

typedef struct Scheme_Let_Value {
  Scheme_Inclhash_Object iso; /* keyex used for autobox */
  mzshort count;
  mzshort position;
  Scheme_Object *value;
  Scheme_Object *body;
} Scheme_Let_Value;

#define SCHEME_LET_AUTOBOX(lh) MZ_OPT_HASH_KEY(&lh->iso)

typedef struct Scheme_Let_One {
  Scheme_Inclhash_Object iso; /* keyex used for eval_type */
  Scheme_Object *value;
  Scheme_Object *body;
} Scheme_Let_One;

#define SCHEME_LET_EVAL_TYPE(lh) MZ_OPT_HASH_KEY(&lh->iso)

typedef struct Scheme_Let_Void {
  Scheme_Inclhash_Object iso; /* keyex used for autobox */
  mzshort count;
  Scheme_Object *body;
} Scheme_Let_Void;

typedef struct Scheme_Letrec {
  Scheme_Object so;
  mzshort count;
  Scheme_Object **procs;
  Scheme_Object *body;
} Scheme_Letrec;

typedef struct {
  Scheme_Object so;
  mzshort count;
  Scheme_Object *array[1];
} Scheme_Sequence;

typedef struct {
  Scheme_Object so;
  mzshort count;
  Scheme_Object *name; /* see note below */
#ifdef MZ_USE_JIT
  struct Scheme_Native_Closure_Data *native_code; /* generated by lightning */
#endif
  Scheme_Object *array[1];
} Scheme_Case_Lambda;
/* If count is not 0, then check array[0] for CLOS_IS_METHOD.
   Otherwise, name is a boxed symbol (or #f) to indicate a method. */

#define scheme_make_prim_w_arity2(f, n, mina, maxa, minr, maxr) \
  scheme_make_prim_w_everything(f, 0, n, mina, maxa, 0, minr, maxr)

Scheme_Object *scheme_unclose_case_lambda(Scheme_Object *expr, int jit);

Scheme_Object *scheme_native_stack_trace(void);
void scheme_clean_native_symtab(void);
void scheme_clean_cust_box_list(void);

/*========================================================================*/
/*                              control flow                              */
/*========================================================================*/

Scheme_Object *scheme_handle_stack_overflow(Scheme_Object *(*k)(void));

extern struct Scheme_Overflow_Jmp *scheme_overflow_jmp;
extern void *scheme_overflow_stack_start;

void scheme_ensure_stack_start(void *d);
extern void *scheme_deepest_stack_start;

#ifdef MZ_PRECISE_GC
# define PROMPT_STACK(id) &__gc_var_stack__
# define ADJUST_STACK_START(start) (start)
#else
# define PROMPT_STACK(id) ((void *)(&id))
# define ADJUST_STACK_START(start) (start ? start : scheme_deepest_stack_start)
#endif

void scheme_jmpup_free(Scheme_Jumpup_Buf *);
void *scheme_enlarge_runstack(long size, void *(*k)());
int scheme_check_runstack(long size);

#ifndef MZ_PRECISE_GC
void scheme_init_setjumpup(void);
void scheme_init_ephemerons(void);
#endif

#ifdef MZ_PRECISE_GC
void scheme_flush_stack_copy_cache(void);
#endif

void *scheme_top_level_do(void *(*k)(void), int eb);
#define scheme_top_level_do_w_thread(k, eb, p) scheme_top_level_do(k, eb)

void scheme_on_next_top(struct Scheme_Comp_Env *env, Scheme_Object *mark, 
			Scheme_Object *name, Scheme_Object *certs, 
			Scheme_Env *menv, Scheme_Object *in_modidx);

Scheme_Object *scheme_call_ec(int argc, Scheme_Object *argv[]);

unsigned long scheme_get_deeper_address(void);

#ifdef DO_STACK_CHECK
void scheme_init_stack_limit (void);
#endif


typedef struct Scheme_Saved_Stack {
  MZTAG_IF_REQUIRED
  Scheme_Object **runstack_start;
  long runstack_offset;
  long runstack_size;
  struct Scheme_Saved_Stack *prev;
} Scheme_Saved_Stack;

typedef struct Scheme_Cont_Mark {
  /* Precise GC: We leave out the tag and make sure everything
     is a pointer, then allocate with GC_malloc_allow_interior */
  Scheme_Object *key;
  Scheme_Object *val;
  Scheme_Object *cache; /* chain and/or shortcut */
  MZ_MARK_POS_TYPE pos; /* Odd numbers - so they look like non-pointers */
} Scheme_Cont_Mark;

typedef struct Scheme_Cont_Mark_Chain {
  Scheme_Object so;
  Scheme_Object *key;
  Scheme_Object *val;
  MZ_MARK_POS_TYPE pos;
  struct Scheme_Cont_Mark_Chain *next;
} Scheme_Cont_Mark_Chain;

typedef struct Scheme_Cont_Mark_Set {
  Scheme_Object so;
  struct Scheme_Cont_Mark_Chain *chain;
  long cmpos;
  Scheme_Object *native_stack_trace;
} Scheme_Cont_Mark_Set;

#define SCHEME_LOG_MARK_SEGMENT_SIZE 8
#define SCHEME_MARK_SEGMENT_SIZE (1 << SCHEME_LOG_MARK_SEGMENT_SIZE)
#define SCHEME_MARK_SEGMENT_MASK (SCHEME_MARK_SEGMENT_SIZE - 1)

typedef struct Scheme_Stack_State {
  long runstack_offset;
  MZ_MARK_POS_TYPE cont_mark_pos;
  MZ_MARK_STACK_TYPE cont_mark_stack;
} Scheme_Stack_State;

typedef struct Scheme_Dynamic_Wind {
  MZTAG_IF_REQUIRED
  int depth;
  void *id; /* generated as needed */
  void *data;
  Scheme_Object *prompt_tag; /* If not NULL, indicates a fake D-W record for prompt boundary */
  void (*pre)(void *);
  void (*post)(void *);
  mz_jmp_buf *saveerr;
  int next_meta; /* amount to move forward in the meta-continuation chain, starting with next */
  struct Scheme_Stack_State envss;
  struct Scheme_Dynamic_Wind *prev;
} Scheme_Dynamic_Wind;

typedef struct Scheme_Cont {
  Scheme_Object so;
  char composable, has_prompt_dw;
  struct Scheme_Meta_Continuation *meta_continuation;
  Scheme_Jumpup_Buf buf;
  Scheme_Dynamic_Wind *dw;
  int next_meta;
  Scheme_Continuation_Jump_State cjs;
  Scheme_Stack_State ss;
  struct Scheme_Prompt *barrier_prompt; /* NULL if no barrier between cont and prompt */
  Scheme_Object **runstack_start;
  long runstack_size;
  Scheme_Saved_Stack *runstack_saved;
  Scheme_Object *prompt_tag;
  mz_jmp_buf *prompt_buf; /* needed for meta-prompt */
  MZ_MARK_POS_TYPE meta_tail_pos; /* to recognize opportunity for meta-tail calls */
  MZ_MARK_POS_TYPE cont_mark_pos_bottom; /* to splice cont mark values with meta-cont */
  void *prompt_stack_start;
  Scheme_Saved_Stack *runstack_copied;
  Scheme_Thread **runstack_owner;
  Scheme_Cont_Mark *cont_mark_stack_copied;
  Scheme_Thread **cont_mark_stack_owner;
  long cont_mark_total; /* size of the copied array plus cont_mark_offset */
  long cont_mark_offset; /* after the array, the original mark stack had this much */
  long cont_mark_nonshare; /* amount to skip for sub-cont sharing */
  void *stack_start;
  Scheme_Object *prompt_id; /* allows direct-jump optimization */
  Scheme_Config *init_config;
  Scheme_Object *init_break_cell;
#ifdef MZ_USE_JIT
  Scheme_Object *native_trace;
#endif
  struct Scheme_Overflow *save_overflow;
  mz_jmp_buf *savebuf; /* save old error buffer here */

  /* Arguments passed to a continuation invocation to the continuation restorer: */
  Scheme_Object *value; /* argument(s) to continuation */
  struct Scheme_Overflow *resume_to; /* meta-continuation return */
  char empty_to_next_mc;
  struct Scheme_Cont *use_next_cont; /* more meta-continuation return */
  int common_dw_depth; /* id for common dw record */
  Scheme_Dynamic_Wind *common_dw; /* shared part with source cont */
  int common_next_meta; /* for common_dw */
  Scheme_Object *extra_marks; /* vector of extra keys and marks to add to meta-cont */
  struct Scheme_Prompt *shortcut_prompt; /* prompt common to save and restore enabling shortcut */
} Scheme_Cont;

typedef struct Scheme_Escaping_Cont {
  Scheme_Object so;
  struct Scheme_Stack_State envss;
  struct Scheme_Prompt *barrier_prompt;
#ifdef MZ_USE_JIT
  Scheme_Object *native_trace;
#endif
  mz_jmp_buf *saveerr;
} Scheme_Escaping_Cont;

#define SCHEME_CONT_F(obj) (((Scheme_Escaping_Cont *)(obj))->f)

int scheme_escape_continuation_ok(Scheme_Object *);

#define scheme_save_env_stack_w_thread(ss, p) \
    (ss.runstack_offset = MZ_RUNSTACK - MZ_RUNSTACK_START, \
     ss.cont_mark_stack = MZ_CONT_MARK_STACK, ss.cont_mark_pos = MZ_CONT_MARK_POS)
#define scheme_restore_env_stack_w_thread(ss, p) \
    (MZ_RUNSTACK = MZ_RUNSTACK_START + ss.runstack_offset, \
     MZ_CONT_MARK_STACK = ss.cont_mark_stack, MZ_CONT_MARK_POS = ss.cont_mark_pos)
#define scheme_save_env_stack(ss) \
    scheme_save_env_stack_w_thread(ss, scheme_current_thread)
#define scheme_restore_env_stack(ss) \
    scheme_restore_env_stack_w_thread(ss, scheme_current_thread)

void scheme_takeover_stacks(Scheme_Thread *p);

typedef struct Scheme_Overflow_Jmp {
  MZTAG_IF_REQUIRED
  char captured; /* set to 1 if possibly captured in a continuation */
  Scheme_Jumpup_Buf cont; /* continuation after value obtained in overflowed */
  mz_jmp_buf *savebuf; /* save old error buffer pointer here */
} Scheme_Overflow_Jmp;

typedef struct Scheme_Overflow {
  MZTAG_IF_REQUIRED
  char eot;      /* set to 1 => pseudo-overflow: continuation is to exit the thread */
  Scheme_Overflow_Jmp *jmp; /* overflow data, so it can be shared when an overflow chain is cloned; */
  void *id;                 /* identity of overflow record; generated as needed, and often == jmp */
  void *stack_start;
  struct Scheme_Overflow *prev; /* old overflow info */
} Scheme_Overflow;

#if defined(UNIX_FIND_STACK_BOUNDS) || defined(WINDOWS_FIND_STACK_BOUNDS) \
    || defined(MACOS_FIND_STACK_BOUNDS) || defined(ASSUME_FIXED_STACK_SIZE) \
    || defined(BEOS_FIND_STACK_BOUNDS) || defined(OSKIT_FIXED_STACK_BOUNDS) \
    || defined(PALM_FIND_STACK_BOUNDS)
# define USE_STACK_BOUNDARY_VAR
extern unsigned long scheme_stack_boundary;
#endif

typedef struct Scheme_Meta_Continuation {
  MZTAG_IF_REQUIRED
  char pseudo; /* if set, don't treat it as a prompt */
  char empty_to_next; /* when pseudo, if the continuation is empty to the next one */
  char cm_caches; /* cached info in copied cm */
  char cm_shared; /* cm is shared, so copy before setting cache entries */
  int copy_after_captured; /* for mutating a meta-continuation in set_cont_stack_mark */
  int depth;
  Scheme_Object *prompt_tag;
  /* The C stack: */
  Scheme_Overflow *overflow;
  MZ_MARK_POS_TYPE meta_tail_pos; /* to recognize opportunity for meta-tail calls */  
  MZ_MARK_POS_TYPE cont_mark_pos_bottom; /* to splice cont mark values with meta-cont */
  /* Cont mark info: */
  MZ_MARK_STACK_TYPE cont_mark_stack;
  MZ_MARK_POS_TYPE cont_mark_pos;
  long cont_mark_total, cont_mark_offset;
  Scheme_Cont_Mark *cont_mark_stack_copied;
  /* Next: */
  struct Scheme_Meta_Continuation *next;
} Scheme_Meta_Continuation;

typedef struct Scheme_Prompt {
  Scheme_Object so;
  char is_barrier;
  Scheme_Object *tag;
  Scheme_Object *id;                  /* created as needed; allows direct-jump optimization for cont app */
  void *stack_boundary;               /* where to stop copying the C stack */
  void *boundary_overflow_id;         /* indicates the C stack segment */
  MZ_MARK_STACK_TYPE mark_boundary;   /* where to stop copying cont marks */
  MZ_MARK_POS_TYPE boundary_mark_pos; /* mark position of prompt */
  Scheme_Object **runstack_boundary_start; /* which stack has runstack_boundary */
  long runstack_boundary_offset;      /* where to stop copying the Scheme stack */
  mz_jmp_buf *prompt_buf;             /* to jump directly to the prompt */
  long runstack_size;                 /* needed for restore */
} Scheme_Prompt;

/* Compiler helper: */
#define ESCAPED_BEFORE_HERE  return NULL

Scheme_Object *scheme_extract_one_cc_mark_with_meta(Scheme_Object *mark_set, 
                                                    Scheme_Object *key,
                                                    Scheme_Object *prompt_tag,
                                                    Scheme_Meta_Continuation **_meta_cont,
                                                    MZ_MARK_POS_TYPE *_pos);
Scheme_Object *scheme_compose_continuation(Scheme_Cont *c, int num_rands, Scheme_Object *value);
Scheme_Overflow *scheme_get_thread_end_overflow(void);
void scheme_end_current_thread(void);
void scheme_ensure_dw_id(Scheme_Dynamic_Wind *dw);
void scheme_apply_dw_in_meta(Scheme_Dynamic_Wind *dw, int post, int mc_depth, struct Scheme_Cont *recheck);

void scheme_drop_prompt_meta_continuations(Scheme_Object *prompt_tag);

struct Scheme_Prompt *scheme_get_barrier_prompt(struct Scheme_Meta_Continuation **_meta_cont,
                                                MZ_MARK_POS_TYPE *_pos);
int scheme_is_cm_deeper(struct Scheme_Meta_Continuation *m1, MZ_MARK_POS_TYPE p1,
                        struct Scheme_Meta_Continuation *m2, MZ_MARK_POS_TYPE p2);
void scheme_recheck_prompt_and_barrier(struct Scheme_Cont *c);

Scheme_Object *scheme_all_current_continuation_marks(void);

void scheme_about_to_move_C_stack(void);

/*========================================================================*/
/*                         semaphores and locks                           */
/*========================================================================*/

typedef struct Scheme_Channel_Syncer {
  Scheme_Object so;
  Scheme_Thread *p;
  char in_line, picked;
  struct Scheme_Channel_Syncer *prev, *next;
  struct Syncing *syncing;
  Scheme_Object *obj;
  int syncing_i;
} Scheme_Channel_Syncer;

typedef struct Scheme_Sema {
  Scheme_Object so;
  Scheme_Channel_Syncer *first, *last;
  long value;
} Scheme_Sema;

typedef struct Scheme_Channel {
  Scheme_Object so;
  Scheme_Channel_Syncer *put_first, *put_last;
  Scheme_Channel_Syncer *get_first, *get_last;
} Scheme_Channel;

typedef struct Scheme_Channel_Put {
  Scheme_Object so;
  Scheme_Channel *ch;
  Scheme_Object *val;
} Scheme_Channel_Put;

#define GENERIC_BLOCKED -1
#define NOT_BLOCKED 0
#define SLEEP_BLOCKED 1

typedef struct Evt_Set {
  Scheme_Object so;

  int argc;
  Scheme_Object **argv; /* no evt sets; nested sets get flattened */
  struct Evt **ws;
} Evt_Set;

#define SCHEME_EVTSETP(o) SAME_TYPE(SCHEME_TYPE(o), scheme_evt_set_type)

typedef struct Syncing {
  MZTAG_IF_REQUIRED
  Evt_Set *set;
  int result, start_pos;
  double sleep_end;
  float timeout;

  Scheme_Object **wrapss;
  Scheme_Object **nackss;
  char *reposts;

  Scheme_Thread *disable_break; /* when result is set */
} Syncing;

int scheme_wait_semas_chs(int n, Scheme_Object **o, int just_try, Syncing *syncing);
Scheme_Object *scheme_make_sema_repost(Scheme_Object *sema);

Scheme_Object *scheme_wrap_evt(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_poll_evt(int argc, Scheme_Object *argv[]);

extern Scheme_Object *scheme_always_ready_evt;

void scheme_get_outof_line(Scheme_Channel_Syncer *ch_w);
void scheme_post_syncing_nacks(Syncing *syncing);

int scheme_try_channel_get(Scheme_Object *ch);

/*========================================================================*/
/*                                 numbers                                */
/*========================================================================*/

#ifdef MPW_C
/* Optimizer bug! */
# define scheme_exact_zero ((Scheme_Object *)0x1)
# define scheme_exact_one ((Scheme_Object *)0x3)
#else
# define scheme_exact_zero scheme_make_integer(0)
# define scheme_exact_one scheme_make_integer(1)
#endif

/****** Bignums *******/

#ifdef USE_LONG_LONG_FOR_BIGDIG
typedef unsigned long long bigdig;
#else
typedef unsigned long bigdig;
#endif

typedef struct {
  Scheme_Inclhash_Object iso;
  int len;
  bigdig *digits;
} Scheme_Bignum;

#if MZ_PRECISE_GC
# define SCHEME_BIGPOS(b) (MZ_OPT_HASH_KEY(&((Scheme_Bignum *)b)->iso) & 0x1)
# define SCHEME_SET_BIGPOS(b, v) MZ_OPT_HASH_KEY(&((Scheme_Bignum *)b)->iso) = ((v) | SCHEME_BIGINLINE(b))
# define SCHEME_BIGINLINE(b) (MZ_OPT_HASH_KEY(&((Scheme_Bignum *)b)->iso) & 0x2)
# define SCHEME_SET_BIGINLINE(b) MZ_OPT_HASH_KEY(&((Scheme_Bignum *)b)->iso) |= (0x2 | SCHEME_BIGPOS(b))
#else
# define SCHEME_BIGPOS(b) MZ_OPT_HASH_KEY(&((Scheme_Bignum *)b)->iso)
# define SCHEME_SET_BIGPOS(b, v) SCHEME_BIGPOS(b) = v
#endif

#define SCHEME_BIGLEN(b) (((Scheme_Bignum *)b)->len)
#define SCHEME_BIGDIG(b) (((Scheme_Bignum *)b)->digits)

typedef struct {
  Scheme_Bignum o;
  bigdig v[1];
} Small_Bignum;

XFORM_NONGCING Scheme_Object *scheme_make_small_bignum(long v, Small_Bignum *s);
char *scheme_number_to_string(int radix, Scheme_Object *obj);

XFORM_NONGCING int scheme_bignum_get_int_val(const Scheme_Object *o, long *v);
XFORM_NONGCING int scheme_bignum_get_unsigned_int_val(const Scheme_Object *o, unsigned long *v);
XFORM_NONGCING int scheme_bignum_get_long_long_val(const Scheme_Object *o, mzlonglong *v);
XFORM_NONGCING int scheme_bignum_get_unsigned_long_long_val(const Scheme_Object *o, umzlonglong *v);

XFORM_NONGCING int scheme_bignum_eq(const Scheme_Object *a, const Scheme_Object *b);
XFORM_NONGCING int scheme_bignum_lt(const Scheme_Object *a, const Scheme_Object *b);
XFORM_NONGCING int scheme_bignum_gt(const Scheme_Object *a, const Scheme_Object *b);
XFORM_NONGCING int scheme_bignum_le(const Scheme_Object *a, const Scheme_Object *b);
XFORM_NONGCING int scheme_bignum_ge(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_negate(const Scheme_Object *n);
Scheme_Object *scheme_bignum_add(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_subtract(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_add1(const Scheme_Object *n);
Scheme_Object *scheme_bignum_sub1(const Scheme_Object *n);
Scheme_Object *scheme_bignum_multiply(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_max(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_min(const Scheme_Object *a, const Scheme_Object *b);
void scheme_bignum_divide(const Scheme_Object *n, const Scheme_Object *d,
			  Scheme_Object **qp, Scheme_Object **rp, int norm);
Scheme_Object *scheme_generic_integer_power(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_gcd(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_integer_sqrt(const Scheme_Object *n);
Scheme_Object *scheme_integer_sqrt_rem(const Scheme_Object *n, Scheme_Object **r);
Scheme_Object *scheme_bignum_and(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_or(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_xor(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_not(const Scheme_Object *a);
Scheme_Object *scheme_bignum_shift(const Scheme_Object *a, long shift);

XFORM_NONGCING double scheme_bignum_to_double_inf_info(const Scheme_Object *n, int just_use, int *only_need);
#ifdef MZ_USE_SINGLE_FLOATS
XFORM_NONGCING float scheme_bignum_to_float_inf_info(const Scheme_Object *n, int just_use, int *only_need);
#else
# define scheme_bignum_to_float_inf_info scheme_bignum_to_double_inf_info
#endif

void scheme_clear_bignum_cache(void);

/****** Rational numbers *******/

typedef struct {
  Scheme_Object so;
  Scheme_Object *num;
  Scheme_Object *denom;
} Scheme_Rational;

typedef Scheme_Rational Small_Rational;

XFORM_NONGCING Scheme_Object *scheme_make_small_rational(long n, Small_Rational *space);
XFORM_NONGCING Scheme_Object *scheme_make_small_bn_rational(Scheme_Object *n, Small_Rational *space);
Scheme_Object *scheme_integer_to_rational(const Scheme_Object *n);
Scheme_Object *scheme_make_fixnum_rational(long n, long d);
XFORM_NONGCING int scheme_rational_eq(const Scheme_Object *a, const Scheme_Object *b);
int scheme_rational_lt(const Scheme_Object *a, const Scheme_Object *b);
int scheme_rational_gt(const Scheme_Object *a, const Scheme_Object *b);
int scheme_rational_le(const Scheme_Object *a, const Scheme_Object *b);
int scheme_rational_ge(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_rational_negate(const Scheme_Object *n);
Scheme_Object *scheme_rational_add(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_rational_subtract(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_rational_add1(const Scheme_Object *n);
Scheme_Object *scheme_rational_sub1(const Scheme_Object *n);
Scheme_Object *scheme_rational_multiply(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_rational_max(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_rational_min(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_rational_divide(const Scheme_Object *n, const Scheme_Object *d);
Scheme_Object *scheme_rational_power(const Scheme_Object *a, const Scheme_Object *b);
XFORM_NONGCING int scheme_is_rational_positive(const Scheme_Object *o);
Scheme_Object *scheme_rational_floor(const Scheme_Object *a);
Scheme_Object *scheme_rational_truncate(const Scheme_Object *a);
Scheme_Object *scheme_rational_ceiling(const Scheme_Object *a);
Scheme_Object *scheme_rational_round(const Scheme_Object *a);
Scheme_Object *scheme_rational_sqrt(const Scheme_Object *n);

/****** Complex numbers *******/

typedef struct {
  Scheme_Object so;
  Scheme_Object *r;
  Scheme_Object *i;
} Scheme_Complex;

typedef Scheme_Complex Small_Complex;

#define _scheme_complex_real_part(n) (((Scheme_Complex *)(n))->r)
#define _scheme_complex_imaginary_part(n) (((Scheme_Complex *)(n))->i)

Scheme_Object *scheme_make_small_complex(const Scheme_Object *n, Small_Complex *space);
Scheme_Object *scheme_real_to_complex(const Scheme_Object *n);
int scheme_complex_eq(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_complex_negate(const Scheme_Object *n);
Scheme_Object *scheme_complex_add(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_complex_subtract(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_complex_add1(const Scheme_Object *n);
Scheme_Object *scheme_complex_sub1(const Scheme_Object *n);
Scheme_Object *scheme_complex_multiply(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_complex_divide(const Scheme_Object *n, const Scheme_Object *d);
Scheme_Object *scheme_complex_power(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_complex_sqrt(const Scheme_Object *a);
XFORM_NONGCING int scheme_is_complex_exact(const Scheme_Object *o);

/****** Inexacts ******/

#define REAL_NUMBER_STR "real number"

int scheme_check_double(const char *where, double v, const char *dest);
#ifdef MZ_USE_SINGLE_FLOATS
int scheme_check_float(const char *where, float v, const char *dest);
#else
# define scheme_check_float scheme_check_double
#endif

double scheme_get_val_as_double(const Scheme_Object *n);
XFORM_NONGCING int scheme_minus_zero_p(double d);

#ifdef MZ_USE_SINGLE_FLOATS
float scheme_get_val_as_float(const Scheme_Object *n);
#endif

#if !defined(USE_IEEE_FP_PREDS) && !defined(USE_SCO_IEEE_PREDS) \
    && !defined(USE_OSF_FP_PREDS) && !defined(USE_PALM_INF_TESTS) \
    && !defined(USE_MSVC_FP_PREDS)
# define MZ_IS_POS_INFINITY(d) ((d) == scheme_infinity_val)
# define MZ_IS_NEG_INFINITY(d) ((d) == scheme_minus_infinity_val)
# ifdef NAN_EQUALS_ANYTHING
#  define MZ_IS_NAN(d) (((d) == 1.0) && ((d) == 2.0))
# else
#  ifdef DEFEAT_FP_COMP_OPTIMIZATION
extern int scheme_both_nan(double a, double b);
#   define MZ_IS_NAN(d) (scheme_both_nan(d, d))
#  else
#   define MZ_IS_NAN(d) (!((d) == (d)))
#  endif
# endif
#else
# ifdef USE_SCO_IEEE_PREDS
#  include <ieeefp.h>
#  define MZ_IS_POS_INFINITY(d) (fpclass(d) == FP_PINF)
#  define MZ_IS_NEG_INFINITY(d) (fpclass(d) == FP_NINF)
#  define MZ_IS_NAN(d) isnan(d)
# else
#  ifdef USE_PALM_INF_TESTS
#   define MZ_IS_POS_INFINITY(d) scheme_is_pos_inf(d)
#   define MZ_IS_NEG_INFINITY(d) scheme_is_neg_inf(d)
#   define MZ_IS_NAN(d) scheme_is_nan(d)
extern int scheme_is_pos_inf(double);
extern int scheme_is_neg_inf(double);
extern int scheme_is_nan(double);
#  else
#   ifdef USE_OSF_FP_PREDS
#    include <math.h>
#    include <fp_class.h>
#    define MZ_IS_POS_INFINITY(d) (fp_class(d) == FP_POS_INF)
#    define MZ_IS_NEG_INFINITY(d) (fp_class(d) == FP_NEG_INF)
#    define MZ_IS_NAN(d) isnan(d)
#   else
#    ifdef USE_CARBON_FP_PREDS
#     define MZ_IS_POS_INFINITY(d) (!__isfinited(d) && (d > 0))
#     define MZ_IS_NEG_INFINITY(d) (!__isfinited(d) && (d < 0))
#     define MZ_IS_NAN(d) __isnand(d)
#    else
#     ifdef USE_MSVC_FP_PREDS
#      include <float.h>
#      define MZ_IS_POS_INFINITY(d) (_fpclass(d) == _FPCLASS_PINF)
#      define MZ_IS_NEG_INFINITY(d) (_fpclass(d) == _FPCLASS_NINF)
#      define MZ_IS_NAN(d) _isnan(d)
#     else
       /* USE_IEEE_FP_PREDS */
#      define MZ_IS_POS_INFINITY(d) (isinf(d) && (d > 0))
#      define MZ_IS_NEG_INFINITY(d) (isinf(d) && (d < 0))
#      define MZ_IS_NAN(d) isnan(d)
#     endif
#    endif
#   endif
#  endif
# endif
#endif

#define IZI_REAL_PART(n) (((Scheme_Complex *)(n))->r)

extern double scheme_infinity_val, scheme_minus_infinity_val;
extern double scheme_floating_point_zero;
extern double scheme_floating_point_nzero;
extern Scheme_Object *scheme_zerod, *scheme_nzerod, *scheme_pi, *scheme_half_pi, *scheme_plus_i, *scheme_minus_i;
extern Scheme_Object *scheme_inf_object, *scheme_minus_inf_object, *scheme_nan_object;
#ifdef MZ_USE_SINGLE_FLOATS
extern Scheme_Object *scheme_zerof, *scheme_nzerof, *scheme_single_scheme_pi;
extern Scheme_Object *scheme_single_inf_object, *scheme_single_minus_inf_object, *scheme_single_nan_object;
#endif

/****** General numeric ******/

Scheme_Object *scheme_read_number(const mzchar *str, long len,
				  int is_float,
				  int is_not_float,
				  int decimal_means_float,
				  int radix, int radix_set,
				  Scheme_Object *port,
				  int *div_by_zero,
				  int test_only,
				  Scheme_Object *stxsrc, long line, long col, long pos, long span,
				  Scheme_Object *indentation);

Scheme_Object *scheme_bin_gcd(const Scheme_Object *n1, const Scheme_Object *n2);
Scheme_Object *scheme_bin_quotient(const Scheme_Object *n1, const Scheme_Object *n2);
Scheme_Object *scheme_bin_mult(const Scheme_Object *n1, const Scheme_Object *n2);
Scheme_Object *scheme_bin_div(const Scheme_Object *n1, const Scheme_Object *n2);
Scheme_Object *scheme_bin_plus(const Scheme_Object *n1, const Scheme_Object *n2);
Scheme_Object *scheme_bin_minus(const Scheme_Object *n1, const Scheme_Object *n2);
int scheme_bin_eq(const Scheme_Object *n1, const Scheme_Object *n2);
int scheme_bin_lt(const Scheme_Object *n1, const Scheme_Object *n2);
int scheme_bin_gt(const Scheme_Object *n1, const Scheme_Object *n2);
int scheme_bin_gt_eq(const Scheme_Object *n1, const Scheme_Object *n2);
int scheme_bin_lt_eq(const Scheme_Object *n1, const Scheme_Object *n2);

Scheme_Object *scheme_sub1(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_add1(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_odd_p(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_expt(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_modulo(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_sqrt(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_abs(int argc, Scheme_Object *argv[]);

Scheme_Object *scheme_inexact_to_exact(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_exact_to_inexact(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_inexact_p(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_TO_DOUBLE(const Scheme_Object *n);
Scheme_Object *scheme_to_bignum(const Scheme_Object *o);
XFORM_NONGCING int scheme_is_integer(const Scheme_Object *o);
XFORM_NONGCING int scheme_is_zero(const Scheme_Object *o);
XFORM_NONGCING int scheme_is_negative(const Scheme_Object *o);
XFORM_NONGCING int scheme_is_positive(const Scheme_Object *o);
Scheme_Object *scheme_make_polar(int argc, Scheme_Object *argv[]);

Scheme_Object *scheme_bitwise_shift(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_bitwise_and(int argc, Scheme_Object *argv[]);

int scheme_nonneg_exact_p(Scheme_Object *n);

#ifdef TIME_TYPE_IS_UNSIGNED
# define scheme_make_integer_value_from_time(t) scheme_make_integer_value_from_unsigned((unsigned long)t)
# define scheme_get_time_val(o, v) scheme_get_unsigned_int_val(o, v)
# define UNBUNDLE_TIME_TYPE unsigned long
#else
# define scheme_make_integer_value_from_time(t) scheme_make_integer_value((long)t)
# define scheme_get_time_val(o, v) scheme_get_int_val(o, v)
# define UNBUNDLE_TIME_TYPE long
#endif

/***** Random number generator *****/

#ifdef MZ_BSD_RANDOM_GENERATOR
# define MZ_RANDOM_STATE_DEG 31
typedef struct {
  Scheme_Object so;
  short fpos, rpos;
  long state[MZ_RANDOM_STATE_DEG];
} Scheme_Random_State;
#else
typedef struct {
  Scheme_Object so;
  double x10, x11, x12, x20, x21, x22; 
} Scheme_Random_State;
#endif

Scheme_Object *scheme_make_random_state(long seed);
long scheme_rand(Scheme_Random_State *rs);

/*========================================================================*/
/*                     read, eval, print                                  */
/*========================================================================*/

#define _scheme_do_eval(obj, env, v) \
  ((SCHEME_INTP(obj) || !SCHEME_STRTAG_VAL(_SCHEME_TYPE(obj))) \
   ? obj : scheme_do_eval(obj, -1, env, v))
#define q_scheme_eval_linked(obj) _scheme_do_eval(obj, 1)
#define q_scheme_tail_eval(obj) scheme_tail_eval(obj)

Scheme_Object *scheme_eval_linked_expr(Scheme_Object *expr);
Scheme_Object *scheme_eval_linked_expr_multi(Scheme_Object *expr);

Scheme_Object *_scheme_apply_to_list (Scheme_Object *rator, Scheme_Object *rands);
Scheme_Object *_scheme_tail_apply_to_list (Scheme_Object *rator, Scheme_Object *rands);

Scheme_Object *scheme_internal_read(Scheme_Object *port, Scheme_Object *stxsrc, int crc, int cantfail, 
				    int honu_mode, int recur, int expose_comment, int pre_char, Scheme_Object *readtable,
				    Scheme_Object *magic_sym, Scheme_Object *magic_val,
                                    Scheme_Object *delay_load_info);
void scheme_internal_display(Scheme_Object *obj, Scheme_Object *port);
void scheme_internal_write(Scheme_Object *obj, Scheme_Object *port);
void scheme_internal_print(Scheme_Object *obj, Scheme_Object *port);

#define _scheme_eval_linked_expr(obj) scheme_do_eval(obj,-1,NULL,1)
#define _scheme_eval_linked_expr_multi(obj) scheme_do_eval(obj,-1,NULL,-1)
#define _scheme_eval_linked_expr_wp(obj, p) scheme_do_eval_w_thread(obj,-1,NULL,1,p)
#define _scheme_eval_linked_expr_multi_wp(obj, p) scheme_do_eval_w_thread(obj,-1,NULL,-1,p)

Scheme_Object *scheme_named_map_1(char *,
				  Scheme_Object *(*fun)(Scheme_Object*, Scheme_Object *form),
				  Scheme_Object *lst, Scheme_Object *form);

XFORM_NONGCING int scheme_strncmp(const char *a, const char *b, int len);

#define _scheme_make_char(ch) scheme_make_character(ch)

Scheme_Object *scheme_default_eval_handler(int, Scheme_Object *[]);
Scheme_Object *scheme_default_compile_handler(int, Scheme_Object *[]);
Scheme_Object *scheme_default_print_handler(int, Scheme_Object *[]);
Scheme_Object *scheme_default_prompt_read_handler(int, Scheme_Object *[]);

extern Scheme_Object *scheme_default_global_print_handler;

/* Type readers & writers for compiled code data */
void scheme_install_type_reader(Scheme_Type type, Scheme_Type_Reader f);
void scheme_install_type_writer(Scheme_Type type, Scheme_Type_Writer f);

Scheme_Object *scheme_make_default_readtable(void);

Scheme_Object *_scheme_apply_from_native(Scheme_Object *rator,
					 int argc,
					 Scheme_Object **argv);
Scheme_Object *_scheme_apply_multi_from_native(Scheme_Object *rator,
					       int argc,
					       Scheme_Object **argv);
Scheme_Object *_scheme_tail_apply_from_native(Scheme_Object *rator,
					      int argc,
					      Scheme_Object **argv);

Scheme_Object *scheme_force_value_same_mark(Scheme_Object *);
Scheme_Object *scheme_force_one_value_same_mark(Scheme_Object *);

void scheme_flush_stack_cache(void);

struct Scheme_Load_Delay;
Scheme_Object *scheme_load_delayed_code(int pos, struct Scheme_Load_Delay *ld);

/*========================================================================*/
/*                          compile and link                              */
/*========================================================================*/

typedef struct Comp_Prefix
{
  MZTAG_IF_REQUIRED
  int num_toplevels, num_stxes;
  Scheme_Hash_Table *toplevels; /* buckets for toplevel/module variables */
  Scheme_Hash_Table *stxes;     /* syntax objects */
} Comp_Prefix;

typedef struct Scheme_Comp_Env
{
  MZTAG_IF_REQUIRED
  short flags;          /* used for expanding/compiling */
  mzshort num_bindings; /* number of `values' slots */
  Scheme_Env *genv;     /* top-level environment */
  Scheme_Object *insp;  /* code inspector for checking protected */
  Comp_Prefix *prefix;  /* stack base info: globals and stxes */

  struct Scheme_Object **values; /* names bound in this frame */
  Scheme_Object *certs; /* extra certs from binding context */

  Scheme_Object *uid;            /* renaming symbol for syntax, if all the same */
  struct Scheme_Object **uids;   /* renaming symbol for syntax when multiple are needed */

  struct Scheme_Object *renames; /* an stx lexical rename or a list of them */

  mzshort rename_var_count;      /* number of non-NULL `values' when `renames' was computed */
  mzshort rename_rstart;         /* leftover rstart from previous round; see env.c */
  Scheme_Hash_Table *dup_check;  /* table for finding colliding symbols in `values' */

  Scheme_Object *intdef_name;    /* syntax-local-context name for INTDEF frames */

  Scheme_Object *in_modidx;      /* an implicit certificate for syntax-local lookup/expand in macro */

  Scheme_Hash_Table *skip_table; /* for jumping ahead in the chain */
  int skip_depth;                /* depth in simple frames, used to trigger skip_table creation */

  struct Scheme_Comp_Env *next;
} Scheme_Comp_Env;

#define CLOS_HAS_REST 1
#define CLOS_HAS_REF_ARGS 2
#define CLOS_PRESERVES_MARKS 4
#define CLOS_FOLDABLE 8
#define CLOS_IS_METHOD 16
#define CLOS_SINGLE_RESULT 32
#define CLOS_RESULT_TENTATIVE 64

typedef struct Scheme_Compile_Expand_Info
{
  MZTAG_IF_REQUIRED
  int comp;
  Scheme_Object *value_name;
  Scheme_Object *certs;
  Scheme_Object *observer;
  char dont_mark_local_use;
  char resolve_module_ids;
  int depth;
} Scheme_Compile_Expand_Info;

typedef Scheme_Compile_Expand_Info Scheme_Compile_Info;
typedef Scheme_Compile_Expand_Info Scheme_Expand_Info;

typedef struct Resolve_Prefix
{
  Scheme_Object so;
  int num_toplevels, num_stxes, num_lifts;
  Scheme_Object **toplevels;
  Scheme_Object **stxes; /* simplified */
  int delay_refcount;
  struct Scheme_Load_Delay *delay_info;
} Resolve_Prefix;

typedef struct Resolve_Info
{
  MZTAG_IF_REQUIRED
  char use_jit, in_module, in_proc, enforce_const;
  int size, oldsize, count, pos;
  int max_let_depth; /* filled in by sub-expressions */
  Resolve_Prefix *prefix;
  Scheme_Hash_Table *stx_map; /* compile offset => resolve offset; prunes prefix-recored stxes */
  mzshort toplevel_pos; /* -1 mean consult next */
  mzshort *old_pos;
  mzshort *new_pos;
  int stx_count;
  mzshort *old_stx_pos; /* NULL => consult next; new pos is index in array */
  int *flags;
  Scheme_Object **lifted; /* maps bindings to lifts */
  Scheme_Object *lifts; /* accumulates lift info */
  struct Resolve_Info *next;
} Resolve_Info;

typedef struct Scheme_Object *
(Scheme_Syntax)(struct Scheme_Object *form, struct Scheme_Comp_Env *env,
		Scheme_Compile_Info *rec, int drec);

typedef struct Scheme_Object *
(Scheme_Syntax_Expander)(struct Scheme_Object *form, struct Scheme_Comp_Env *env,
			 Scheme_Expand_Info *rec, int drec);

typedef struct Scheme_Object *(*Scheme_Syntax_Resolver)(Scheme_Object *data, Resolve_Info *info);

typedef struct Optimize_Info
{
  MZTAG_IF_REQUIRED
  short flags;
  struct Optimize_Info *next;
  int original_frame, new_frame;
  Scheme_Object *consts;

  /* Propagated up and down the chain: */
  int size;
  short inline_fuel;
  char letrec_not_twice, enforce_const;
  Scheme_Hash_Table *top_level_consts;

  /* Set by expression optimization: */
  int single_result, preserves_marks; /* negative means "tentative", due to fixpoint in progress */

  char **stat_dists; /* (pos, depth) => used? */
  int *sd_depths;
  int used_toplevel;
  char *use;

  int transitive_use_pos; /* set to pos + 1 when optimizing a letrec-bound procedure */
  mzshort **transitive_use;
  int *transitive_use_len;
} Optimize_Info;

typedef struct Scheme_Object *(*Scheme_Syntax_Optimizer)(Scheme_Object *data, Optimize_Info *info);
typedef struct Scheme_Object *(*Scheme_Syntax_Cloner)(int dup_ok, Scheme_Object *data, Optimize_Info *info, int delta, int closure_depth);
typedef struct Scheme_Object *(*Scheme_Syntax_Shifter)(Scheme_Object *data, int delta, int after_depth);

typedef struct CPort Mz_CPort;

typedef void (*Scheme_Syntax_Validater)(Scheme_Object *data, Mz_CPort *port, 
                                        char *stack, Scheme_Hash_Table *ht, Scheme_Object **tls,
					int depth, int letlimit, int delta,
					int num_toplevels, int num_stxes, int num_lifts);

typedef struct Scheme_Object *(*Scheme_Syntax_Executer)(struct Scheme_Object *data);

typedef struct Scheme_Object *(*Scheme_Syntax_Jitter)(struct Scheme_Object *data);

typedef struct Scheme_Closure_Data
{
  Scheme_Inclhash_Object iso; /* keyex used for flags */
  mzshort num_params; /* includes collecting arg if has_rest */
  mzshort max_let_depth;
  mzshort closure_size;
  mzshort *closure_map; /* actually a Closure_Info* until resolved; if CLOS_HAS_REF_ARGS, followed by bit array */
  Scheme_Object *code;
  Scheme_Object *name;
#ifdef MZ_USE_JIT
  union {
    struct Scheme_Closure_Data *jit_clone;
    struct Scheme_Native_Closure_Data *native_code; /* generated by lightning */
  } u;
  Scheme_Object *context; /* e.g., a letrec that binds the closure */
#endif
} Scheme_Closure_Data;

#define SCHEME_CLOSURE_DATA_FLAGS(obj) MZ_OPT_HASH_KEY(&(obj)->iso)

int scheme_has_method_property(Scheme_Object *code);

typedef struct {
  Scheme_Object so;
  Scheme_Closure_Data *code;
  Scheme_Object *vals[1];
} Scheme_Closure;

#define SCHEME_COMPILED_CLOS_CODE(c) ((Scheme_Closure *)c)->code
#define SCHEME_COMPILED_CLOS_ENV(c) ((Scheme_Closure *)c)->vals

#define ZERO_SIZED_CLOSUREP(closure) !(closure->code->closure_size)

typedef struct Scheme_Native_Closure_Data {
  Scheme_Inclhash_Object iso; /* type tag only set when needed, but flags always needed */
  Scheme_Closed_Prim *code;
  union {
    void *tail_code;                       /* For non-case-lambda */
    mzshort *arities;                      /* For case-lambda */
  } u;
  void *arity_code;
  mzshort max_let_depth; /* In bytes instead of words */
  mzshort closure_size;
  union {
    struct Scheme_Closure_Data *orig_code; /* For not-yet-JITted non-case-lambda */
    Scheme_Object *name;
  } u2;
#ifdef MZ_PRECISE_GC
  void **retained; /* inside code */
  mzshort retain_count;
#endif
} Scheme_Native_Closure_Data;

#define SCHEME_NATIVE_CLOSURE_DATA_FLAGS(obj) MZ_OPT_HASH_KEY(&(obj)->iso)

typedef struct {
  Scheme_Object so;
  Scheme_Native_Closure_Data *code;
  Scheme_Object *vals[1];
} Scheme_Native_Closure;

Scheme_Native_Closure_Data *scheme_generate_lambda(Scheme_Closure_Data *obj, int drop_code, 
						   Scheme_Native_Closure_Data *case_lam);

#define MAX_CONST_LOCAL_POS 64
extern Scheme_Object *scheme_local[MAX_CONST_LOCAL_POS][2];

#define scheme_new_frame(n) scheme_new_special_frame(n, 0)
#define scheme_extend_env(f, e) (f->basic.next = e, f)
#define scheme_next_frame(e) ((e)->basic.next)
#define scheme_settable_frame(f, s) ((f)->basic.has_set_bang = (s))
#define scheme_get_frame_settable(f) ((f)->basic.has_set_bang)
#define scheme_get_binding(f, n) ((f)->values[n])

Scheme_Comp_Env *scheme_new_comp_env(Scheme_Env *genv, Scheme_Object *insp, int flags);
Scheme_Comp_Env *scheme_new_expand_env(Scheme_Env *genv, Scheme_Object *insp, int flags);

void scheme_check_identifier(const char *formname, Scheme_Object *id,
			     const char *where,
			     Scheme_Comp_Env *env,
			     Scheme_Object *form);
int scheme_check_context(Scheme_Env *env, Scheme_Object *id, Scheme_Object *ok_modix);

Scheme_Object *scheme_check_immediate_macro(Scheme_Object *first,
					    Scheme_Comp_Env *env,
					    Scheme_Compile_Expand_Info *erec, int drec,
					    int int_def_pos,
					    Scheme_Object **current_val,
					    Scheme_Comp_Env **_xenv,
					    Scheme_Object *ctx);

Scheme_Object *scheme_apply_macro(Scheme_Object *name, Scheme_Env *menv,
				  Scheme_Object *f, Scheme_Object *code,
				  Scheme_Comp_Env *env, Scheme_Object *boundname,
                                  Scheme_Compile_Expand_Info *rec, int drec,
                                  int for_set);

Scheme_Comp_Env *scheme_new_compilation_frame(int num_bindings, int flags, 
					      Scheme_Comp_Env *env, Scheme_Object *certs);
void scheme_add_compilation_binding(int index, Scheme_Object *val,
				    Scheme_Comp_Env *frame);
Scheme_Comp_Env *scheme_add_compilation_frame(Scheme_Object *vals,
					      Scheme_Comp_Env *env, int flags,
					      Scheme_Object *certs);
Scheme_Comp_Env *scheme_require_renames(Scheme_Comp_Env *env);

Scheme_Object *scheme_lookup_binding(Scheme_Object *symbol, Scheme_Comp_Env *env, int flags, 
				     Scheme_Object *certs, Scheme_Object *in_modidx, 
				     Scheme_Env **_menv, int *_protected,
                                     Scheme_Object **_lexical_binding_id);

Scheme_Object *scheme_add_env_renames(Scheme_Object *stx, Scheme_Comp_Env *env,
				      Scheme_Comp_Env *upto);

Scheme_Object *scheme_env_frame_uid(Scheme_Comp_Env *env);

typedef Scheme_Object *(*Scheme_Lift_Capture_Proc)(Scheme_Object *, Scheme_Object **, Scheme_Object *, Scheme_Comp_Env *);
void scheme_frame_captures_lifts(Scheme_Comp_Env *env, Scheme_Lift_Capture_Proc cp, Scheme_Object *data, 
                                 Scheme_Object *end_stmts, Scheme_Object *context_key);
Scheme_Object *scheme_frame_get_lifts(Scheme_Comp_Env *env);
Scheme_Object *scheme_frame_get_end_statement_lifts(Scheme_Comp_Env *env);
Scheme_Object *scheme_generate_lifts_key(void);

void scheme_add_local_syntax(int cnt, Scheme_Comp_Env *env);
void scheme_set_local_syntax(int pos, Scheme_Object *name, Scheme_Object *val,
			     Scheme_Comp_Env *env);

Scheme_Object *scheme_make_closure(Scheme_Thread *p,
				   Scheme_Object *compiled_code,
				   int close);
Scheme_Closure *scheme_malloc_empty_closure(void);

Scheme_Object *scheme_make_native_closure(Scheme_Native_Closure_Data *code);
Scheme_Object *scheme_make_native_case_closure(Scheme_Native_Closure_Data *code);

Scheme_Native_Closure_Data *scheme_generate_case_lambda(Scheme_Case_Lambda *cl);

#define scheme_add_good_binding(i,v,f) (f->values[i] = v)

Scheme_Object *scheme_compiled_void(void);

Scheme_Object *scheme_register_toplevel_in_prefix(Scheme_Object *var, Scheme_Comp_Env *env,
						  Scheme_Compile_Info *rec, int drec);
Scheme_Object *scheme_register_stx_in_prefix(Scheme_Object *var, Scheme_Comp_Env *env,
					     Scheme_Compile_Info *rec, int drec);

void scheme_bind_syntaxes(const char *where, Scheme_Object *names, Scheme_Object *a, 
                          Scheme_Env *exp_env, Scheme_Object *insp, 
                          Scheme_Compile_Expand_Info *rec, int drec,
                          Scheme_Comp_Env *stx_env, Scheme_Comp_Env *rhs_env,
                          int *_pos);
int scheme_is_sub_env(Scheme_Comp_Env *stx_env, Scheme_Comp_Env *env);

/* Resolving & linking */
#define DEFINE_VALUES_EXPD 0
#define DEFINE_SYNTAX_EXPD 1
#define SET_EXPD           2
#define CASE_LAMBDA_EXPD   3
#define BEGIN0_EXPD        4
#define BOXENV_EXPD        5
#define MODULE_EXPD        6
#define REQUIRE_EXPD       7
#define DEFINE_FOR_SYNTAX_EXPD 8
#define REF_EXPD           9
#define APPVALS_EXPD       10
#define SPLICE_EXPD        11
#define _COUNT_EXPD_       12

#define scheme_register_syntax(i, fo, fr, fv, fe, fj, cl, sh, pa)        \
     (scheme_syntax_optimizers[i] = fo, \
      scheme_syntax_resolvers[i] = fr, \
      scheme_syntax_executers[i] = fe, \
      scheme_syntax_validaters[i] = fv, \
      scheme_syntax_jitters[i] = fj, \
      scheme_syntax_cloners[i] = cl, \
      scheme_syntax_shifters[i] = sh, \
      scheme_syntax_protect_afters[i] = pa)
extern Scheme_Syntax_Optimizer scheme_syntax_optimizers[_COUNT_EXPD_];
extern Scheme_Syntax_Resolver scheme_syntax_resolvers[_COUNT_EXPD_];
extern Scheme_Syntax_Validater scheme_syntax_validaters[_COUNT_EXPD_];
extern Scheme_Syntax_Executer scheme_syntax_executers[_COUNT_EXPD_];
extern Scheme_Syntax_Jitter scheme_syntax_jitters[_COUNT_EXPD_];
extern Scheme_Syntax_Cloner scheme_syntax_cloners[_COUNT_EXPD_];
extern Scheme_Syntax_Shifter scheme_syntax_shifters[_COUNT_EXPD_];
extern int scheme_syntax_protect_afters[_COUNT_EXPD_];

Scheme_Object *scheme_protect_quote(Scheme_Object *expr);

Scheme_Object *scheme_make_syntax_resolved(int idx, Scheme_Object *data);
Scheme_Object *scheme_make_syntax_compiled(int idx, Scheme_Object *data);

Scheme_Object *scheme_optimize_expr(Scheme_Object *, Optimize_Info *);
Scheme_Object *scheme_optimize_lets(Scheme_Object *form, Optimize_Info *info, int for_inline);
Scheme_Object *scheme_optimize_lets_for_test(Scheme_Object *form, Optimize_Info *info);

Scheme_Object *scheme_optimize_apply_values(Scheme_Object *f, Scheme_Object *e, 
                                            Optimize_Info *info,
                                            int e_single_result);

int scheme_compiled_duplicate_ok(Scheme_Object *o);
int scheme_compiled_propagate_ok(Scheme_Object *o, Optimize_Info *info);

Scheme_Object *scheme_resolve_expr(Scheme_Object *, Resolve_Info *);
Scheme_Object *scheme_resolve_list(Scheme_Object *, Resolve_Info *);

int scheme_is_compiled_procedure(Scheme_Object *o, int can_be_closed, int can_be_liftable);

Scheme_Object *scheme_resolve_lets(Scheme_Object *form, Resolve_Info *info);

Resolve_Prefix *scheme_resolve_prefix(int phase, Comp_Prefix *cp, int simplify);
Resolve_Prefix *scheme_remap_prefix(Resolve_Prefix *rp, Resolve_Info *ri);

Resolve_Info *scheme_resolve_info_create(Resolve_Prefix *rp);
Resolve_Info *scheme_resolve_info_extend(Resolve_Info *info, int size, int oldsize, int mapcount);
void scheme_resolve_info_add_mapping(Resolve_Info *info, int oldp, int newp, int flags, Scheme_Object *lifted);
void scheme_resolve_info_adjust_mapping(Resolve_Info *info, int oldp, int newp, int flags, Scheme_Object *lifted);
int scheme_resolve_info_flags(Resolve_Info *info, int pos, Scheme_Object **lifted);
int scheme_resolve_info_lookup(Resolve_Info *resolve, int pos, int *flags, Scheme_Object **lifted, int convert_shift);
void scheme_resolve_info_set_toplevel_pos(Resolve_Info *info, int pos);

void scheme_enable_expression_resolve_lifts(Resolve_Info *ri);
Scheme_Object *scheme_merge_expression_resolve_lifts(Scheme_Object *expr, Resolve_Prefix *rp, Resolve_Info *ri);

Optimize_Info *scheme_optimize_info_create(void);

void scheme_optimize_propagate(Optimize_Info *info, int pos, Scheme_Object *value);
Scheme_Object *scheme_optimize_info_lookup(Optimize_Info *info, int pos, int *closure_offset);
void scheme_optimize_info_used_top(Optimize_Info *info);

void scheme_optimize_mutated(Optimize_Info *info, int pos);
Scheme_Object *scheme_optimize_reverse(Optimize_Info *info, int pos, int unless_mutated);
int scheme_optimize_is_used(Optimize_Info *info, int pos);
int scheme_optimize_any_uses(Optimize_Info *info, int start_pos, int end_pos);

Scheme_Object *scheme_optimize_clone(int dup_ok, Scheme_Object *obj, Optimize_Info *info, int delta, int closure_depth);
Scheme_Object *scheme_optimize_shift(Scheme_Object *obj, int delta, int after_depth);
Scheme_Object *scheme_clone_closure_compilation(int dup_ok, Scheme_Object *obj, Optimize_Info *info, int delta, int closure_depth);
Scheme_Object *scheme_shift_closure_compilation(Scheme_Object *obj, int delta, int after_depth);

int scheme_closure_body_size(Scheme_Closure_Data *closure_data, int check_assign);
int scheme_closure_argument_flags(Scheme_Closure_Data *closure_data, int i);
int scheme_closure_has_top_level(Scheme_Closure_Data *data);

Optimize_Info *scheme_optimize_info_add_frame(Optimize_Info *info, int orig, int current, int flags);
int scheme_optimize_info_get_shift(Optimize_Info *info, int pos);
void scheme_optimize_info_done(Optimize_Info *info);

Scheme_Object *scheme_toplevel_to_flagged_toplevel(Scheme_Object *tl, int flags);

void scheme_env_make_closure_map(Optimize_Info *frame, mzshort *size, mzshort **map);
int scheme_env_uses_toplevel(Optimize_Info *frame);

int scheme_resolve_toplevel_pos(Resolve_Info *info);
int scheme_resolve_is_toplevel_available(Resolve_Info *info);
int scheme_resolve_quote_syntax_offset(int i, Resolve_Info *info);
int scheme_resolve_quote_syntax_pos(Resolve_Info *info);
Scheme_Object *scheme_resolve_toplevel(Resolve_Info *info, Scheme_Object *expr);
Scheme_Object *scheme_resolve_invent_toplevel(Resolve_Info *info);
Scheme_Object *scheme_resolve_invented_toplevel_to_defn(Resolve_Info *info, Scheme_Object *tl);
Scheme_Object *scheme_shift_toplevel(Scheme_Object *expr, int delta);
Scheme_Object *scheme_resolve_generate_stub_lift(void);
int scheme_resolve_quote_syntax(Resolve_Info *info, int oldpos);
int scheme_resolving_in_procedure(Resolve_Info *info);

void scheme_resolve_lift_definition(Resolve_Info *info, Scheme_Object *var, Scheme_Object *rhs);

Scheme_Object *scheme_make_compiled_syntax(Scheme_Syntax *syntax,
					   Scheme_Syntax_Expander *exp);

Scheme_Object *scheme_compile_expr(Scheme_Object *form, Scheme_Comp_Env *env,
				   Scheme_Compile_Info *rec, int drec);
Scheme_Object *scheme_compile_sequence(Scheme_Object *forms, Scheme_Comp_Env *env,
			      Scheme_Compile_Info *rec, int drec);
Scheme_Object *scheme_compile_block(Scheme_Object *forms, Scheme_Comp_Env *env,
			      Scheme_Compile_Info *rec, int drec);
Scheme_Object *scheme_compile_list(Scheme_Object *form, Scheme_Comp_Env *env,
			      Scheme_Compile_Info *rec, int drec);

Scheme_Object *scheme_compile_expr_lift_to_let(Scheme_Object *form, Scheme_Comp_Env *env,
					       Scheme_Compile_Info *rec, int drec);

void scheme_default_compile_rec(Scheme_Compile_Info *src, int drec);
void scheme_compile_rec_done_local(Scheme_Compile_Info *src, int drec);
void scheme_init_compile_recs(Scheme_Compile_Info *src, int drec,
			      Scheme_Compile_Info *dest, int n);
void scheme_merge_compile_recs(Scheme_Compile_Info *src, int drec,
			       Scheme_Compile_Info *dest, int n);
void scheme_init_lambda_rec(Scheme_Compile_Info *src, int drec,
			    Scheme_Compile_Info *lam, int dlrec);
void scheme_merge_lambda_rec(Scheme_Compile_Info *src, int drec,
			    Scheme_Compile_Info *lam, int dlrec);


void scheme_init_expand_recs(Scheme_Expand_Info *src, int drec,
			     Scheme_Expand_Info *dest, int n);

void scheme_rec_add_certs(Scheme_Compile_Expand_Info *src, int drec, Scheme_Object *stx);

Scheme_Object *scheme_make_closure_compilation(Scheme_Comp_Env *env,
					       Scheme_Object *uncompiled_code,
					       Scheme_Compile_Info *rec, int drec);
Scheme_Object *scheme_make_sequence_compilation(Scheme_Object *compiled_list,
						int strip_values);

Scheme_Object *scheme_optimize_closure_compilation(Scheme_Object *_data, Optimize_Info *info);
Scheme_Object *scheme_resolve_closure_compilation(Scheme_Object *_data, Resolve_Info *info, 
                                                  int can_lift, int convert, int just_compute_lift,
                                                  Scheme_Object *precomputed_lift);

Scheme_App_Rec *scheme_malloc_application(int n);
void scheme_finish_application(Scheme_App_Rec *app);

Scheme_Object *scheme_jit_expr(Scheme_Object *);
Scheme_Object *scheme_jit_closure(Scheme_Object *, Scheme_Object *context);

Scheme_Object *scheme_build_closure_name(Scheme_Object *code, Scheme_Compile_Info *rec, int drec);

#define SCHEME_SYNTAX(obj)     SCHEME_PTR1_VAL(obj)
#define SCHEME_SYNTAX_EXP(obj) SCHEME_PTR2_VAL(obj)

int *scheme_env_get_flags(Scheme_Comp_Env *frame, int start, int count);

/* flags reported by scheme_env_get_flags */
#define SCHEME_WAS_USED                0x1
#define SCHEME_WAS_SET_BANGED          0x2
#define SCHEME_WAS_ONLY_APPLIED        0x4
#define SCHEME_WAS_APPLIED_EXCEPT_ONCE 0x8

#define SCHEME_USE_COUNT_MASK   0x70
#define SCHEME_USE_COUNT_SHIFT  4
#define SCHEME_USE_COUNT_INF    (SCHEME_USE_COUNT_MASK >> SCHEME_USE_COUNT_SHIFT)

/* flags reported by scheme_resolve_info_flags */
#define SCHEME_INFO_BOXED 1

/* flags used with scheme_new_frame */
#define SCHEME_TOPLEVEL_FRAME 1
#define SCHEME_MODULE_FRAME 2
#define SCHEME_MODULE_BEGIN_FRAME 4
#define SCHEME_LAMBDA_FRAME 8
#define SCHEME_INTDEF_FRAME 16
#define SCHEME_NO_RENAME 32
#define SCHEME_CAPTURE_WITHOUT_RENAME 64
#define SCHEME_FOR_STOPS 128
#define SCHEME_FOR_INTDEF 256
#define SCHEME_CAPTURE_LIFTED 512

/* Flags used with scheme_static_distance */
#define SCHEME_ELIM_CONST 1
#define SCHEME_APP_POS 2
#define SCHEME_SETTING 4
#define SCHEME_ENV_CONSTANTS_OK 8
#define SCHEME_GLOB_ALWAYS_REFERENCE 16
#define SCHEME_MUST_INDRECT 32
#define SCHEME_LINKING_REF 64
#define SCHEME_DONT_MARK_USE 128
#define SCHEME_OUT_OF_CONTEXT_OK 256
#define SCHEME_NULL_FOR_UNBOUND 512
#define SCHEME_RESOLVE_MODIDS 1024
#define SCHEME_NO_CERT_CHECKS 2048
#define SCHEME_REFERENCING 4096

Scheme_Hash_Table *scheme_map_constants_to_globals(void);

Scheme_Object *scheme_expand_expr(Scheme_Object *form, Scheme_Comp_Env *env,
				  Scheme_Expand_Info *erec, int drec);
Scheme_Object *scheme_expand_list(Scheme_Object *form, Scheme_Comp_Env *env,
				  Scheme_Expand_Info *erec, int drec);
Scheme_Object *scheme_expand_block(Scheme_Object *form, Scheme_Comp_Env *env,
				   Scheme_Expand_Info *erec, int drec);
Scheme_Object *scheme_expand_expr_lift_to_let(Scheme_Object *form, Scheme_Comp_Env *env,
					      Scheme_Expand_Info *erec, int drec);

Scheme_Object *scheme_flatten_begin(Scheme_Object *expr, Scheme_Object *append_onto);

Scheme_Object *scheme_make_svector(mzshort v, mzshort *a);

#define SCHEME_SVEC_LEN(obj) (((Scheme_Simple_Object *)(obj))->u.svector_val.len)
#define SCHEME_SVEC_VEC(obj) (((Scheme_Simple_Object *)(obj))->u.svector_val.vec)

Scheme_Object *scheme_hash_percent_name(const char *name, int len);

Scheme_Object *scheme_make_branch(Scheme_Object *test,
				  Scheme_Object *tbranch,
				  Scheme_Object *fbranch);

int scheme_is_toplevel(Scheme_Comp_Env *env);
Scheme_Comp_Env *scheme_extend_as_toplevel(Scheme_Comp_Env *env);

Scheme_Comp_Env *scheme_no_defines(Scheme_Comp_Env *env);

Scheme_Env *scheme_make_empty_env(void);
void scheme_prepare_exp_env(Scheme_Env *env);
void scheme_prepare_template_env(Scheme_Env *env);

int scheme_used_app_only(Scheme_Comp_Env *env, int which);
int scheme_used_ever(Scheme_Comp_Env *env, int which);

int scheme_omittable_expr(Scheme_Object *o, int vals);

int scheme_is_env_variable_boxed(Scheme_Comp_Env *env, int which);

int scheme_get_eval_type(Scheme_Object *obj);

Scheme_Object *scheme_get_stop_expander(void);

void scheme_define_parse(Scheme_Object *form,
			 Scheme_Object **vars, Scheme_Object **val,
			 int defmacro,
			 Scheme_Comp_Env *env,
                         int no_toplevel_check);

void scheme_shadow(Scheme_Env *env, Scheme_Object *n, int stxtoo);

int scheme_prefix_depth(Resolve_Prefix *rp);
Scheme_Object **scheme_push_prefix(Scheme_Env *genv, Resolve_Prefix *rp,
				   Scheme_Object *src_modix, Scheme_Object *now_modix,
				   int src_phase, int now_phase);
void scheme_pop_prefix(Scheme_Object **rs);

Scheme_Object *scheme_make_environment_dummy(Scheme_Comp_Env *env);
Scheme_Env *scheme_environment_from_dummy(Scheme_Object *dummy);

void scheme_validate_code(Mz_CPort *port, Scheme_Object *code, Scheme_Hash_Table *ht,
                          int depth,
			  int num_toplevels, int num_stxes, int num_lifts);
void scheme_validate_expr(Mz_CPort *port, Scheme_Object *expr, 
			  char *stack, Scheme_Hash_Table *ht, Scheme_Object **tls,
                          int depth, int letlimit, int delta,
			  int num_toplevels, int num_stxes, int num_lifts,
                          Scheme_Object *app_rator, int proc_with_refs_ok);
void scheme_validate_toplevel(Scheme_Object *expr, Mz_CPort *port,
			      char *stack, Scheme_Hash_Table *ht, Scheme_Object **tls,
                              int depth, int delta,
			      int num_toplevels, int num_stxes, int num_lifts,
                              int skip_refs_check);
void scheme_validate_boxenv(int pos, Mz_CPort *port,
			    char *stack, int depth, int delta);

int scheme_validate_rator_wants_box(Scheme_Object *app_rator, int pos,
                                    int hope,
                                    Scheme_Object **tls,
                                    int num_toplevels, int num_stxes, int num_lifts);

#define TRACK_ILL_FORMED_CATCH_LINES 1
#if TRACK_ILL_FORMED_CATCH_LINES
void scheme_ill_formed(Mz_CPort *port, const char *file, int line);
# define scheme_ill_formed_code(port) scheme_ill_formed(port, __FILE__, __LINE__)
#else
void scheme_ill_formed(Mz_CPort *port);
# define scheme_ill_formed_code(port) scheme_ill_formed(port)
#endif

extern Scheme_Object *scheme_inferred_name_symbol;
Scheme_Object *scheme_check_name_property(Scheme_Object *stx, Scheme_Object *current_name);

Scheme_Object *scheme_make_lifted_defn(Scheme_Object *sys_wraps, Scheme_Object **_id, Scheme_Object *expr, Scheme_Comp_Env *env);

typedef struct Scheme_Marshal_Tables {
  MZTAG_IF_REQUIRED  
  int pass, print_now;
  Scheme_Hash_Table *symtab;
  Scheme_Hash_Table *rns;
  Scheme_Hash_Table *rn_refs;
  Scheme_Hash_Table *st_refs;
  Scheme_Object *st_ref_stack;
  Scheme_Hash_Table *reverse_map; /* used on first pass */
  Scheme_Hash_Table *same_map;    /* set on first pass, used on later passes */
  Scheme_Hash_Table *top_map;     /* used on every pass */
  Scheme_Hash_Table *key_map;     /* set after first pass, used on later passes */
  Scheme_Hash_Table *delay_map;   /* set during first pass, used on later passes */
  Scheme_Hash_Table *rn_saved;    /* maps each original object to generated marshaling */
  long *shared_offsets;           /* set in second pass */
  long sorted_keys_count;
  Scheme_Object **sorted_keys;
} Scheme_Marshal_Tables;

void scheme_marshal_using_key(Scheme_Marshal_Tables *mt, Scheme_Object *key);
Scheme_Object *scheme_marshal_lookup(Scheme_Marshal_Tables *mt, Scheme_Object *a);
Scheme_Object *scheme_marshal_wrap_set(Scheme_Marshal_Tables *mt, Scheme_Object *a, Scheme_Object *v);
void scheme_marshal_push_refs(Scheme_Marshal_Tables *mt);
void scheme_marshal_pop_refs(Scheme_Marshal_Tables *mt, int keep);

typedef struct Scheme_Unmarshal_Tables {
  MZTAG_IF_REQUIRED  
  Scheme_Hash_Table *rns;
  struct CPort *rp;
  char *decoded;
} Scheme_Unmarshal_Tables;

Scheme_Object *scheme_unmarshal_wrap_get(Scheme_Unmarshal_Tables *ut, 
                                         Scheme_Object *wraps_key, 
                                         int *_decoded);
void scheme_unmarshal_wrap_set(Scheme_Unmarshal_Tables *ut, 
                               Scheme_Object *wraps_key, 
                               Scheme_Object *v);

/*========================================================================*/
/*                         namespaces and modules                         */
/*========================================================================*/

struct Scheme_Env {
  Scheme_Object so; /* scheme_namespace_type */

  struct Scheme_Module *module; /* NULL => top-level */

  Scheme_Hash_Table *module_registry; /* symbol -> module ; loaded modules,
					 shared with modules in same space */
  Scheme_Hash_Table *export_registry; /* symbol -> module-exports */
  Scheme_Object *insp; /* instantiation-time inspector, for granting
			  protected access and certificates */

  /* For compilation, per-declaration: */
  /* First two are passed from module to module-begin: */
  Scheme_Object *rename;    /* module rename record */
  Scheme_Object *et_rename; /* exp-time rename record */
  Scheme_Object *tt_rename; /* template-time rename record */
  Scheme_Object *dt_rename; /* template-time rename record */

  Scheme_Bucket_Table *syntax;
  struct Scheme_Env *exp_env;
  struct Scheme_Env *template_env;

  Scheme_Hash_Table *shadowed_syntax; /* top level only */

  /* Per-instance: */
  long phase, mod_phase;
  Scheme_Object *link_midx;
  Scheme_Object *require_names, *et_require_names, *tt_require_names, *dt_require_names; /* resolved */
  char running, et_running, tt_running, lazy_syntax, attached;

  Scheme_Bucket_Table *toplevel;
  Scheme_Object *modchain; /* Vector of:
			       1. symbol -> env ; running modules,
			           shared with instances in same phase
			       2. modchain for next phase (or #f)
                               3. modchain for previous phase (or #f) */

  Scheme_Hash_Table *modvars; /* for scheme_module_variable_type hashing */

  Scheme_Hash_Table *marked_names; /* for mapping marked ids to uninterned symbols */

  int id_counter;
};

/* A module access path (or "idx") is a pair: sexp * symbol-or-#f
   The symbol is the resolved module name, or #f if it's not
   yet resolved. */

/* A Scheme_Module corresponds to a module declaration. A module
   instantiation is reprsented by a Scheme_Env */

typedef struct Scheme_Module
{
  Scheme_Object so; /* scheme_module_type */

  Scheme_Object *modname;

  Scheme_Object *et_requires;  /* list of symbol-or-module-path-index */
  Scheme_Object *requires;     /* list of symbol-or-module-path-index */
  Scheme_Object *tt_requires;  /* list of symbol-or-module-path-index */
  Scheme_Object *dt_requires;  /* list of symbol-or-module-path-index */

  Scheme_Invoke_Proc prim_body;
  Scheme_Invoke_Proc prim_et_body;

  Scheme_Object *body;        /* or data, if prim_body */
  Scheme_Object *et_body;     /* list of (vector list-of-names expr depth-int resolve-prefix) */

  char functional, et_functional, tt_functional, no_cert;
  
  struct Scheme_Module_Exports *me;

  char *provide_protects;            /* 1 => protected, 0 => not */
  Scheme_Object **indirect_provides; /* symbols (internal names) */
  int num_indirect_provides;

  char *et_provide_protects;            /* 1 => protected, 0 => not */
  Scheme_Object **et_indirect_provides; /* symbols (internal names) */
  int num_indirect_et_provides;

  Scheme_Object *self_modidx;

  Scheme_Hash_Table *accessible;
  Scheme_Hash_Table *et_accessible;
  Scheme_Object *insp; /* declaration-time inspector, for creating certificates
			  and for module instantiation */

  Scheme_Object *hints; /* set by expansion; moved to properties */
  Scheme_Object *ii_src; /* set by compile, temporary */
  Comp_Prefix *comp_prefix; /* set by body compile, temporary */

  int max_let_depth;
  Resolve_Prefix *prefix;

  Scheme_Object *dummy; /* for accessing the environment */

  Scheme_Env *primitive;

  Scheme_Object *rn_stx, *et_rn_stx, *tt_rn_stx, *dt_rn_stx;
} Scheme_Module;

typedef struct Scheme_Module_Phase_Exports
{
  MZTAG_IF_REQUIRED

  Scheme_Object **provides;          /* symbols (extenal names) */
  Scheme_Object **provide_srcs;      /* module access paths, #f for self */
  Scheme_Object **provide_src_names; /* symbols (original internal names) */
  char *provide_src_phases;          /* NULL, or src phase for for-syntax import */
  int num_provides;
  int num_var_provides;              /* non-syntax listed first in provides */

  int reprovide_kernel;              /* if true, extend provides with kernel's */
  Scheme_Object *kernel_exclusion;  /* we allow one exn, but it must be shadowed */
} Scheme_Module_Phase_Exports;

typedef struct Scheme_Module_Exports
{
  /* Scheme_Module_Exports is separate from Scheme_Module
     so that we can create a global table mapping export
     keys to exports. This mapping is used to lazily 
     unmarshal syntax-object context. */
  MZTAG_IF_REQUIRED

  Scheme_Module_Phase_Exports *rt, *et, *dt;

  Scheme_Object *src_modidx;  /* the one used in marshalled syntax */
} Scheme_Module_Exports;

typedef struct Scheme_Modidx {
  Scheme_Object so; /* scheme_module_index_type */

  Scheme_Object *path;
  Scheme_Object *base;
  Scheme_Object *resolved;
  Scheme_Object *shift_cache; /* vector */
  struct Scheme_Modidx *cache_next;
} Scheme_Modidx;

typedef struct Module_Variable {
  Scheme_Object so; /* scheme_module_variable_type */
  Scheme_Object *modidx;
  Scheme_Object *sym;
  Scheme_Object *insp; /* for checking protected/unexported access */
  int pos, mod_phase;
} Module_Variable;

void scheme_add_global_keyword(const char *name, Scheme_Object *v, Scheme_Env *env);
void scheme_add_global_keyword_symbol(Scheme_Object *name, Scheme_Object *v, Scheme_Env *env);
void scheme_add_global_constant(const char *name, Scheme_Object *v, Scheme_Env *env);
void scheme_add_global_constant_symbol(Scheme_Object *name, Scheme_Object *v, Scheme_Env *env);

Scheme_Object *scheme_tl_id_sym(Scheme_Env *env, Scheme_Object *id, Scheme_Object *bdg, int is_def);
int scheme_tl_id_is_sym_used(Scheme_Hash_Table *marked_names, Scheme_Object *sym);

Scheme_Object *scheme_sys_wraps(Scheme_Comp_Env *env);

Scheme_Env *scheme_new_module_env(Scheme_Env *env, Scheme_Module *m, int new_exp_module_tree);
int scheme_is_module_env(Scheme_Comp_Env *env);

Scheme_Object *scheme_module_resolve(Scheme_Object *modidx, int load_it);
Scheme_Env *scheme_module_access(Scheme_Object *modname, Scheme_Env *env, int rev_mod_phase);
void scheme_module_force_lazy(Scheme_Env *env, int previous);

int scheme_module_export_position(Scheme_Object *modname, Scheme_Env *env, Scheme_Object *varname);

Scheme_Object *scheme_check_accessible_in_module(Scheme_Env *env, Scheme_Object *prot_insp, Scheme_Object *in_modidx,
						 Scheme_Object *symbol, Scheme_Object *stx, 
						 Scheme_Object *certs, Scheme_Object *unexp_insp,
						 int position, int want_pos,
						 int *_protected);
Scheme_Object *scheme_module_syntax(Scheme_Object *modname, Scheme_Env *env, Scheme_Object *name);

Scheme_Object *scheme_modidx_shift(Scheme_Object *modidx,
				   Scheme_Object *shift_from_modidx,
				   Scheme_Object *shift_to_modidx);

Scheme_Object *scheme_hash_module_variable(Scheme_Env *env, Scheme_Object *modidx, 
					   Scheme_Object *stxsym, Scheme_Object *insp,
					   int pos, int mod_phase);

extern Scheme_Env *scheme_initial_env;

void scheme_install_initial_module_set(Scheme_Env *env);
Scheme_Bucket_Table *scheme_clone_toplevel(Scheme_Bucket_Table *ht, Scheme_Env *home);

Scheme_Env *scheme_clone_module_env(Scheme_Env *menv, Scheme_Env *ns, Scheme_Object *modchain);

void scheme_clean_dead_env(Scheme_Env *env);

Scheme_Module *scheme_extract_compiled_module(Scheme_Object *o);

void scheme_clear_modidx_cache(void);
void scheme_clear_shift_cache(void);
void scheme_clear_prompt_cache(void);

/*========================================================================*/
/*                         errors and exceptions                          */
/*========================================================================*/

void scheme_read_err(Scheme_Object *port,
		     Scheme_Object *stxsrc,
		     long line, long column, long pos, long span,
		     int is_eof, Scheme_Object *indentation,
		     const char *detail, ...);
char *scheme_extract_indentation_suggestions(Scheme_Object *indentation);

void scheme_wrong_syntax(const char *where,
			 Scheme_Object *local_form,
			 Scheme_Object *form,
			 const char *detail, ...);
void scheme_wrong_syntax_with_more_sources(const char *where,
                                           Scheme_Object *detail_form,
                                           Scheme_Object *form,
                                           Scheme_Object *extra_sources,
                                           const char *detail, ...);
extern const char *scheme_compile_stx_string;
extern const char *scheme_expand_stx_string;
extern const char *scheme_application_stx_string;
extern const char *scheme_set_stx_string;
extern const char *scheme_var_ref_string;
extern const char *scheme_begin_stx_string;

void scheme_wrong_rator(Scheme_Object *rator, int argc, Scheme_Object **argv);

void scheme_raise_out_of_memory(const char *where, const char *msg, ...);

extern unsigned long scheme_max_found_symbol_name;

char *scheme_make_arity_expect_string(Scheme_Object *proc,
				      int argc, Scheme_Object **argv,
				      long *len);

long scheme_extract_index(const char *name, int pos, int argc,
			  Scheme_Object **argv, long top, int false_ok);

void scheme_get_substring_indices(const char *name, Scheme_Object *str,
				  int argc, Scheme_Object **argv,
				  int spos, int fpos, long *_start, long *_finish);

void scheme_out_of_string_range(const char *name, const char *which,
				Scheme_Object *i, Scheme_Object *s,
				long start, long len);

const char *scheme_number_suffix(int);

void scheme_reset_prepared_error_buffer(void);

const char *scheme_hostname_error(int err);

char *scheme_make_args_string(char *s, int which, int argc, Scheme_Object **argv, long *olen);

#define IMPROPER_LIST_FORM "illegal use of `.'"

int scheme_byte_string_has_null(Scheme_Object *o);
int scheme_any_string_has_null(Scheme_Object *o);
#define CHAR_STRING_W_NO_NULLS "string (with no nul characters)"
#define STRING_OR_BYTE_STRING_W_NO_NULLS "string or byte string (with no nul characters)"

Scheme_Object *scheme_do_exit(int argc, Scheme_Object *argv[]);

Scheme_Object *scheme_make_arity(mzshort minc, mzshort maxc);
Scheme_Object *scheme_arity(Scheme_Object *p);

typedef struct {
  MZTAG_IF_REQUIRED
  Scheme_Object *syms[5];
  int count;
  long phase;
  Scheme_Hash_Table *ht;
} DupCheckRecord;

void scheme_begin_dup_symbol_check(DupCheckRecord *r, Scheme_Comp_Env *e);
void scheme_dup_symbol_check(DupCheckRecord *r, const char *where,
			     Scheme_Object *symbol, char *what,
			     Scheme_Object *form);

extern int scheme_exiting_result;

Scheme_Object *scheme_special_comment_value(Scheme_Object *o);

Scheme_Object *scheme_get_stack_trace(Scheme_Object *mark_set);

Scheme_Object *scheme_get_or_check_arity(Scheme_Object *p, long a);
int scheme_native_arity_check(Scheme_Object *closure, int argc);
Scheme_Object *scheme_get_native_arity(Scheme_Object *closure);

/*========================================================================*/
/*                         filesystem utilities                           */
/*========================================================================*/

#ifdef USE_TRANSITIONAL_64_FILE_OPS
# define BIG_OFF_T_IZE(n) n ## 64
# define mz_off_t off64_t
#else
# define BIG_OFF_T_IZE(n) n
# if defined(DOS_FILE_SYSTEM)
#  define mz_off_t mzlonglong
# else
#  define mz_off_t off_t
# endif
#endif

int scheme_is_relative_path(const char *s, long len, int kind);
int scheme_is_complete_path(const char *s, long len, int kind);

Scheme_Object *scheme_get_file_directory(const char *filename);

char *scheme_normal_path_seps(char *s, int *_len, int delta);

int scheme_is_regular_file(char *filename);

#ifdef MAC_FILE_SYSTEM
void scheme_file_create_hook(char *filename);
#endif

void scheme_do_format(const char *procname, Scheme_Object *port,
		      const mzchar *format, int flen,
		      int fpos, int offset, int argc, Scheme_Object **argv);

Scheme_Object *scheme_load_with_clrd(int argc, Scheme_Object *argv[], char *who, int handler_param);

#ifdef MAC_CLASSIC_PROCESS_CONTROL
int scheme_mac_start_app(char *name, int find_path, Scheme_Object *s);
#endif
#ifdef MACINTOSH_EVENTS
int scheme_mac_send_event(char *name, int argc, Scheme_Object **argv, Scheme_Object **result, int *err, char **stage);
#endif

Scheme_Object *scheme_default_load_extension(int argc, Scheme_Object **argv);

Scheme_Object *scheme_remove_current_directory_prefix(Scheme_Object *fn);

#ifdef DOS_FILE_SYSTEM
int scheme_is_special_filename(const char *_f, int not_nul);
# define NUM_SPECIAL_FILE_KINDS 30
#endif

char *scheme_get_exec_path(void);

Scheme_Object *scheme_get_fd_identity(Scheme_Object *port, long fd);

Scheme_Object *scheme_extract_relative_to(Scheme_Object *obj, Scheme_Object *dir);

#ifdef DOS_FILE_SYSTEM
# define WIDE_PATH(s) scheme_convert_to_wchar(s, 0)
# define WIDE_PATH_COPY(s) scheme_convert_to_wchar(s, 1)
# define NARROW_PATH(s) scheme_convert_from_wchar(s)
extern wchar_t *scheme_convert_to_wchar(const char *s, int do_copy);
extern char *scheme_convert_from_wchar(const wchar_t *ws);
#else
# define WIDE_PATH(s) s
# define WIDE_PATH_COPY(s) s
# define NARROW_PATH(s) s
#endif

#if defined(DOS_FILE_SYSTEM) && !defined(__CYGWIN32__)
# define MSC_W_IZE(n) _w ## n
# define MSC_WIDE_PATH(s) WIDE_PATH(s)
# define MSC_WIDE_PATH_COPY(s) WIDE_PATH_COPY(s)
#else
# define MSC_W_IZE(n) MSC_IZE(n)
# define MSC_WIDE_PATH(s) s
# define MSC_WIDE_PATH_COPY(s) s
#endif

/*========================================================================*/
/*                               ports                                    */
/*========================================================================*/

#ifdef NO_TCP_SUPPORT
# undef USE_UNIX_SOCKETS_TCP
# undef USE_WINSOCK_TCP
# undef USE_MAC_TCP
#endif
#if defined(USE_UNIX_SOCKETS_TCP) || defined(USE_WINSOCK_TCP) || defined(USE_MAC_TCP)
# define USE_TCP
#endif

#if defined(USE_UNIX_SOCKETS_TCP) || defined(USE_WINSOCK_TCP)
# define USE_SOCKETS_TCP
#endif

extern int scheme_active_but_sleeping;
extern int scheme_file_open_count;

typedef struct Scheme_Indexed_String {
  MZTAG_IF_REQUIRED
  char *string;
  int size;
  int index;
  union {
    int hot; /* output port */
    int pos; /* input port */
  } u;
} Scheme_Indexed_String;

typedef struct Scheme_Pipe {
  MZTAG_IF_REQUIRED
  unsigned char *buf;
  long buflen, bufmax;
  long bufmaxextra; /* due to peeks, bufmax can effectively grow */
  long bufstart, bufend;
  int eof;
  Scheme_Object *wakeup_on_read;
  Scheme_Object *wakeup_on_write;
} Scheme_Pipe;

extern Scheme_Object *scheme_string_input_port_type;
extern Scheme_Object *scheme_string_output_port_type;
extern Scheme_Object *scheme_user_input_port_type;
extern Scheme_Object *scheme_user_output_port_type;
extern Scheme_Object *scheme_pipe_read_port_type;
extern Scheme_Object *scheme_pipe_write_port_type;
extern Scheme_Object *scheme_null_output_port_type;
#ifdef USE_TCP
extern Scheme_Object *scheme_tcp_input_port_type;
extern Scheme_Object *scheme_tcp_output_port_type;
#endif

extern int scheme_force_port_closed;

void scheme_flush_orig_outputs(void);
Scheme_Object *scheme_file_stream_port_p(int, Scheme_Object *[]);
Scheme_Object *scheme_terminal_port_p(int, Scheme_Object *[]);
Scheme_Object *scheme_do_open_input_file(char *name, int offset, int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_do_open_output_file(char *name, int offset, int argc, Scheme_Object *argv[], int and_read);
Scheme_Object *scheme_file_position(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_file_buffer(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_file_identity(int argc, Scheme_Object *argv[]);

long scheme_get_byte_string_or_ch_put(const char *who,
				      Scheme_Object *port,
				      char *buffer, long offset, long size,
				      int only_avail,
				      int peek, Scheme_Object *peek_skip,
				      Scheme_Object *unless_evt,
				      Scheme_Object *target_ch);

Scheme_Object *scheme_get_special(Scheme_Object *inport, Scheme_Object *stxsrc, long line, long col, long pos, int peek, 
				  Scheme_Hash_Table **for_read);
Scheme_Object *scheme_get_ready_read_special(Scheme_Object *port, Scheme_Object *stxsrc, Scheme_Hash_Table **ht);
void scheme_set_in_read_mark(Scheme_Object *stxsrc, Scheme_Hash_Table **ht);
Scheme_Object *scheme_get_special_proc(Scheme_Object *inport);
void scheme_bad_time_for_special(const char *name, Scheme_Object *port);
extern int scheme_special_ok;

int scheme_user_port_byte_probably_ready(Scheme_Input_Port *ip, Scheme_Schedule_Info *sinfo);
int scheme_user_port_write_probably_ready(Scheme_Output_Port *op, Scheme_Schedule_Info *sinfo);
int scheme_is_user_port(Scheme_Object *port);

int scheme_byte_ready_or_user_port_ready(Scheme_Object *p, Scheme_Schedule_Info *sinfo);

#define CURRENT_INPUT_PORT(config) scheme_get_param(config, MZCONFIG_INPUT_PORT)
#define CURRENT_OUTPUT_PORT(config) scheme_get_param(config, MZCONFIG_OUTPUT_PORT)
#define CHECK_PORT_CLOSED(who, kind, port, closed) if (closed) scheme_raise_exn(MZEXN_FAIL, "%s: " kind " port is closed", who);

#ifdef USE_FCNTL_O_NONBLOCK
# define MZ_NONBLOCKING O_NONBLOCK
#else
# define MZ_NONBLOCKING FNDELAY
#endif

#define MAX_UTF8_CHAR_BYTES 6

/*========================================================================*/
/*                         memory debugging                               */
/*========================================================================*/

#ifdef MEMORY_COUNTING_ON
extern Scheme_Hash_Table *scheme_symbol_table;
extern long scheme_type_table_count;
extern long scheme_misc_count;

Scheme_Object *scheme_dump_memory_count(int c, Scheme_Object *a[]);

long scheme_count_closure(Scheme_Object **o, mzshort len, Scheme_Hash_Table *ht);

long scheme_count_envbox(Scheme_Object *root, Scheme_Hash_Table *ht);
long scheme_count_memory(Scheme_Object *root, Scheme_Hash_Table *ht);
void scheme_count_input_port(Scheme_Object *port, long *s, long *e, Scheme_Hash_Table *ht);
void scheme_count_output_port(Scheme_Object *port, long *s, long *e, Scheme_Hash_Table *ht);

void scheme_count_struct_info(Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht);

#ifndef NO_OBJECT_SYSTEM
void scheme_count_object(Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht);
void scheme_count_class(Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht);
void scheme_count_class_data(Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht);
void scheme_count_generic(Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht);
#endif
#endif

/*========================================================================*/
/*                           miscellaneous                                */
/*========================================================================*/

Scheme_Object *scheme_checked_car(int argc, Scheme_Object **argv);
Scheme_Object *scheme_checked_cdr(int argc, Scheme_Object **argv);
Scheme_Object *scheme_checked_caar(int argc, Scheme_Object **argv);
Scheme_Object *scheme_checked_cadr(int argc, Scheme_Object **argv);
Scheme_Object *scheme_checked_cdar(int argc, Scheme_Object **argv);
Scheme_Object *scheme_checked_cddr(int argc, Scheme_Object **argv);
Scheme_Object *scheme_checked_set_car (int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_checked_set_cdr (int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_checked_vector_ref(int argc, Scheme_Object **argv);
Scheme_Object *scheme_checked_vector_set(int argc, Scheme_Object **argv);
Scheme_Object *scheme_checked_string_ref(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_checked_string_set(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_checked_byte_string_ref(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_checked_byte_string_set(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_checked_syntax_e(int argc, Scheme_Object **argv);

void scheme_set_root_param(int p, Scheme_Object *v);

Scheme_Object *scheme_intern_exact_parallel_symbol(const char *name, unsigned int len);
Scheme_Object *scheme_symbol_append(Scheme_Object *s1, Scheme_Object *s2);
Scheme_Object *scheme_copy_list(Scheme_Object *l);

void scheme_reset_hash_table(Scheme_Hash_Table *ht, int *history);

Scheme_Object *scheme_regexp_source(Scheme_Object *re);
int scheme_regexp_is_byte(Scheme_Object *re);
Scheme_Object *scheme_make_regexp(Scheme_Object *str, int byte, int pcre, int * volatile result_is_err_string);
int scheme_is_pregexp(Scheme_Object *o);
void scheme_clear_rx_buffers(void);
unsigned short * scheme_ucs4_to_utf16(const mzchar *text, int start, int end,
				      unsigned short *buf, int bufsize,
				      long *ulen, int term_size);

#ifdef SCHEME_BIG_ENDIAN
# define MZ_UCS4_NAME "UCS-4BE"
#else
# define MZ_UCS4_NAME "UCS-4LE"
#endif

#define SCHEME_SYM_UNINTERNEDP(o) (MZ_OPT_HASH_KEY(&((Scheme_Symbol *)(o))->iso) & 0x1)
#define SCHEME_SYM_PARALLELP(o) (MZ_OPT_HASH_KEY(&((Scheme_Symbol *)(o))->iso) & 0x2)
#define SCHEME_SYM_WEIRDP(o) (MZ_OPT_HASH_KEY(&((Scheme_Symbol *)(o))->iso) & 0x3)

#endif /* __mzscheme_private__ */
